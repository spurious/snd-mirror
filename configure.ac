# Configuration script for Snd

# TODO: change names to use MUS_ prefix throughout (remember libxm/sndlib/clm/clm-config.windoze)

AC_INIT(snd, 8.8, bil@ccrma.stanford.edu, ftp://ccrma-ftp.stanford.edu/pub/Lisp/snd-8.tar.gz)

AC_CONFIG_SRCDIR(snd.c)
AC_CANONICAL_HOST
AC_CONFIG_HEADERS(mus-config.h sndlib.h)
AC_CONFIG_FILES(makefile po/Makefile.in )
AC_PROG_CC
AC_HEADER_STDC # this needs to precede AC_CHECK_HEADER! (is this the case in 2.60?)
AC_PROG_INSTALL

MAKE_TARGET="snd"

AUDIO_SYSTEM="None"
RANDOM_FEATURES=""
OPTIONAL_LIBRARIES=""
LOCAL_LANGUAGE="None"
GRAPHICS_TOOLKIT="None"

PACKAGE=snd
VERSION=8.8
AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE")
AC_DEFINE_UNQUOTED(VERSION, "$VERSION")
AC_SUBST(PACKAGE)
AC_SUBST(VERSION)

AC_DEFINE_UNQUOTED(SND_HOST, "$host")

#--------------------------------------------------------------------------------
# configuration options
#   --with-motif          use Motif (the default)
#   --with-static-motif   use Motif statically loaded (for RPM generation)
#   --with-gtk            use Gtk+	
#   --with-alsa           use ALSA if possible
#   --with-oss            use OSS (the default)
#   --with-jack           use Jack
#   --with-static-alsa    use ALSA statically loaded (for RPM generation)
#   --with-snd-as-widget  make Snd a loadable widget, not a standalone program
#   --with-snd-as-pd-external  make Snd a loadable pd external, not a standalone program
#   --with-doubles        use doubles throughout (default is floats)
#   --with-float-samples  represent samples internally as floats or doubles (default=yes)
#   --enable-snd-debug    include Snd internal debugging stuff
#   --disable-deprecated  do not include any deprecated stuff (in gtk, motif, guile, sndlib, clm, etc)
#   --with-ladspa         include LADSPA plugin support (Linux)
#   --with-sample-width=N use N bits of samples (default = 24)
#   --with-esd            use Enlightened Sound Daemon
#   --with-no-gui         make Snd without any graphics support
#   --with-motif-prefix   set location of Motif
#   --with-static-gsl     try to statically load GSL
#   --with-guile          try to use Guile as the extension language (default=yes)
#   --with-forth          try to use Forth as the extension language
#   --with-ruby           try to use Ruby as the extension language
#   --with-ruby-prefix    set location of Ruby
#   --with-gauche         try to use Gauche as the extension language
#   --with-hobbit         include hobbit-style function arity checking
#   --with-static-xm      include xm module
#   --with-temp-dir       directory to use for temp files
#   --with-save-dir       directory to use for saved-state files
#   --with-doc-dir        directory to search for documentation
#   --with-gl             include OpenGL support (default=no)
#   --with-just-gl        include OpenGL support but omit the Guile bindings (gl.o) (default=no)
#   --with-gl2ps          include gl2ps
#   --with-modules        accommodate sndlib that was built with modules
#   --with-editres 	  include EditRes in xm
#   --with-shared-sndlib  load sndlib.so if possible
#   --with-builtin-gtkrc  include built-in gtkrc fallbacks (default=yes)
#   --without-gsl         omit GSL even if it exists
#   --without-fftw        omit FFTW even if it exists
#   --without-xpm         omit XPM even if it exists (for testing)
#   --without-xp          omit libXp even if it exists (for testing)
#   --without-fam         libfam can be obsolete, but it's not clear how to test this
#   --with-midi           include sndlib midi module
#   --enable-threads      include pthread stuff
#   GUILE_CONFIG_path     where to look for guile-config
#   GUILE_CONFIG_name     Fink (OSX) includes guile-config under funny names
#   GUILE_name            same problem
#   SNDLIB_CONFIG_path    where to look for sndlib-config

#--------------------------------------------------------------------------------

AC_ARG_WITH(esd,         [  --with-esd		  use ESD])
AC_ARG_WITH(alsa,        [  --with-alsa		  use ALSA])
AC_ARG_WITH(oss,         [  --with-oss		  use OSS])
AC_ARG_WITH(jack,        [  --with-jack		  use JACK])
AC_ARG_WITH(static-alsa, [  --with-static-alsa	  use ALSA statically loaded])
AC_ARG_WITH(gtk,         [  --with-gtk		  use Gtk+ to build Snd])
AC_ARG_WITH(no-gui,      [  --with-no-gui  	  make Snd without any graphics support])
AC_ARG_WITH(static-gsl,  [  --with-static-gsl	  use libgsl.a to build Snd (for obsolete RPM version)])
AC_ARG_WITH(static-xm,   [  --with-static-xm	  include the xm module])
AC_ARG_WITH(static-xg,   [  --with-static-xg	  include the xg module])
AC_ARG_WITH(guile,       [  --with-guile	  	  use Guile])
AC_ARG_WITH(gauche,      [  --with-gauche           use Gauche])
AC_ARG_WITH(gl,          [  --with-gl		  include OpenGL support])
AC_ARG_WITH(just-gl,     [  --with-just-gl	  include OpenGL support, but omit the GL/Guile bindings])
AC_ARG_WITH(gl2ps,       [  --with-gl2ps		  include gl2ps])
AC_ARG_WITH(motif,	 [  --with-motif	  	  use libXm to build Snd])
AC_ARG_WITH(static-motif,[  --with-static-motif	  use libXm.a to build Snd])
AC_ARG_WITH(modules,     [  --with-modules	  use if sndlib uses modules])
AC_ARG_WITH(editres,     [  --with-editres	  include editres in xm])
AC_ARG_WITH(ladspa,	 [  --with-ladspa  	  include support for LADSPA plugins, Linux only])
AC_ARG_WITH(hobbit,      [  --with-hobbit 	  include hobbit-style function arity checking, default=no])
AC_ARG_WITH(builtin-gtkrc,[  --with-builtin-gtkrc	  include built-in gtkrc fallbacks, default=yes])
AC_ARG_WITH(midi,        [  --with-midi		  include sndlib midi module])
AC_ARG_WITH(doubles,     [  --with-doubles	  use doubles throughout])
AC_ARG_WITH(float-samples, [  --with-float-samples	  use floats (or doubles) as the internal sample respresentation, default=yes])

# these are primarily for testing
AC_ARG_WITH(gsl,         [  --with-gsl		  use GSL, default=yes])
AC_ARG_WITH(fftw,        [  --with-fftw		  use fftw, default=yes])
AC_ARG_WITH(xpm,         [  --with-xpm		  use xpm, default=yes])
AC_ARG_WITH(xp,          [  --with-xp		  use xp, default=no])
AC_ARG_WITH(fam,         [  --with-fam		  use libfam (Gamin), default=yes])


# -------- internal sample data type --------

# sample-width only applies to the int case (ignored if float)
LOCAL_SNDLIB_BITS="24"
AC_ARG_WITH(sample-width,
	[  --with-sample-width=N   use N bits of samples],
	[AC_MSG_RESULT(Using $with_sample_width bit samples)
	 AC_DEFINE_UNQUOTED(MUS_SAMPLE_BITS, $with_sample_width)
         LOCAL_SNDLIB_BITS=$with_sample_width
	],
	[AC_DEFINE(MUS_SAMPLE_BITS, 24)]
	)

if test "$with_float_samples" != no ; then
  AC_DEFINE(SNDLIB_USE_FLOATS, 1)
  if test "$with_doubles" = yes ; then
    LOCAL_SNDLIB_BITS="8"
  else
    LOCAL_SNDLIB_BITS="4"
  fi
else
  AC_DEFINE(SNDLIB_USE_FLOATS, 0)
fi

if test "$with_doubles" = yes ; then
  AC_DEFINE(Float,double)
  AC_DEFINE(WITH_DOUBLES)
else
  AC_DEFINE(Float,float)
fi

if test "$with_hobbit" = yes ; then
  AC_DEFINE(WITH_HOBBIT)
  RANDOM_FEATURES="$RANDOM_FEATURES hobbit"
fi

if test "$with_builtin_gtkrc" != no; then
  AC_DEFINE(WITH_BUILTIN_GTKRC)
fi

if test "$with_static_xg" = yes ; then
  with_static_xm=yes
fi

AC_ARG_WITH(motif-prefix,[  --with-motif-prefix=PFX where Motif is installed],
            motif_prefix="$withval", motif_prefix="")

if test "$with_static_motif" = yes ; then
  with_motif=yes
fi

AC_ARG_WITH(temp-dir,
	[  --with-temp-dir	  directory to use for temp files],
	    AC_DEFINE_UNQUOTED(MUS_DEFAULT_TEMP_DIR, "${withval}")
	)

AC_ARG_WITH(save-dir,
	[  --with-save-dir	  directory to use for saved-state files],
	    AC_DEFINE_UNQUOTED(MUS_DEFAULT_SAVE_DIR, "${withval}")
	)

AC_ARG_WITH(doc-dir,
	[  --with-doc-dir	  directory to search for documentation],
	    AC_DEFINE_UNQUOTED(MUS_DEFAULT_DOC_DIR, "${withval}")
	)

AC_ARG_WITH(snd-as-widget,
	[  --with-snd-as-widget	  make Snd a loadable widget, not a standalone program],
  	if test "$with_snd_as_widget" = yes ; then
	   AC_DEFINE(SND_AS_WIDGET)
	   MAKE_TARGET=widget
	fi)

AC_ARG_WITH(snd-as-pd-external,
	[  --with-snd-as-pd-external	  make Snd a loadable pd external, not a standalone program],
  	[if test "$with_snd_as_pd_external" = yes ; then
	   if test "$with_motif" = yes || test "$with_gtk" = yes ; then
	      AC_MSG_ERROR(When making snd as a pd external, motif or gtk can not be used.)
	   fi
	   if test "$with_ruby" = yes ; then
	      AC_MSG_ERROR(When making snd as a pd external, ruby can not be used as extension language.)
	   fi
	   CFLAGS="$CFLAGS -DSND_PD_PATH=\\\"`pwd`\\\""
	   [with_jack=yes]
	   AC_DEFINE(SND_AS_PD_EXTERNAL)
	   MAKE_TARGET=external

    	   case "$host" in
             *-apple-*)
 	       CFLAGS="$CFLAGS -DPD -I. -bundle -undefined suppress -flat_namespace"
 	     ;;
           esac

	fi])


#--------------------------------------------------------------------------------
# standard libraries, header files, functions, OSS special cases
#--------------------------------------------------------------------------------

# AC_CHECK_LIB(m,main)
# AC_CHECK_LIB(c,main)
# AC_CHECK_LIB(dl,main)
LIBS=""

AC_CHECK_HEADERS(fcntl.h limits.h unistd.h string.h sys/soundcard.h machine/soundcard.h sys/mixer.h stdbool.h sys/time.h)
AC_CHECK_HEADERS(libc.h sys/statvfs.h setjmp.h dlfcn.h sys/param.h byteswap.h stdint.h pthread.h lrdf.h fam.h dirent.h)

AC_CHECK_HEADER([sys/mount.h], [], [], [#if HAVE_SYS_PARAM_H
				        #include <sys/param.h>
				        #endif
				       ])
# this still screws up in netBSD!

AC_CHECK_HEADER(/usr/local/lib/oss/include/sys/soundcard.h,[AC_DEFINE(MUS_HAVE_USR_LOCAL_LIB_OSS)])
AC_CHECK_HEADER(/usr/lib/oss/include/sys/soundcard.h,[AC_DEFINE(MUS_HAVE_USR_LIB_OSS)])
AC_CHECK_HEADER(/opt/oss/include/sys/soundcard.h,[AC_DEFINE(MUS_HAVE_OPT_OSS)])
AC_CHECK_HEADER(/var/lib/oss/include/sys/soundcard.h,[AC_DEFINE(MUS_HAVE_VAR_LIB_OSS)])
AC_CHECK_HEADER(sys/sam9407.h,[AC_DEFINE(HAVE_SAM_9407)])
AC_CHECK_HEADER(gnu/libc-version.h,[AC_DEFINE(HAVE_GNU_LIBC_VERSION_H)])
AC_CHECK_HEADER(alsa/asoundlib.h,[AC_DEFINE(HAVE_ALSA_ASOUNDLIB_H)])

AC_CHECK_DECLS(hypot,,,[#include <math.h>]) 
AC_CHECK_DECLS(isnan,,,[#include <math.h>])
AC_CHECK_DECLS(isinf,,,[#include <math.h>])

AC_TYPE_MODE_T
AC_TYPE_SIZE_T
AC_TYPE_PID_T

AC_C_BIGENDIAN( [AC_DEFINE(MUS_LITTLE_ENDIAN, 0)
		 AC_DEFINE(WORDS_BIGENDIAN)
    		 case "$host" in
      		  *-apple-*)
        		AC_DEFINE(MUS_AUDIO_COMPATIBLE_FORMAT, MUS_BFLOAT)
        		;;
      		  *)
			AC_DEFINE(MUS_AUDIO_COMPATIBLE_FORMAT, MUS_BSHORT)
			;;
    		 esac
		 if test "$with_float_samples" != no ; then
		    if test "$with_doubles" = yes ; then
                      AC_DEFINE(MUS_OUT_FORMAT, MUS_BDOUBLE)
                    else
                      AC_DEFINE(MUS_OUT_FORMAT, MUS_BFLOAT)
                    fi
                 else
                   AC_DEFINE(MUS_OUT_FORMAT, MUS_BINT)
 		 fi
 		],
		[AC_DEFINE(MUS_LITTLE_ENDIAN, 1)
    		 case "$host" in
      		  *-apple-*)
        		AC_DEFINE(MUS_AUDIO_COMPATIBLE_FORMAT, MUS_LFLOAT)
        		;;
      		  *)
			AC_DEFINE(MUS_AUDIO_COMPATIBLE_FORMAT, MUS_LSHORT)
			;;
    		 esac
		 if test "$with_float_samples" != no ; then
		    if test "$with_doubles" = yes ; then
                      AC_DEFINE(MUS_OUT_FORMAT, MUS_LDOUBLE)
                    else
                      AC_DEFINE(MUS_OUT_FORMAT, MUS_LFLOAT)
                    fi
                 else
                   AC_DEFINE(MUS_OUT_FORMAT, MUS_LINT)
 		 fi
		])

AC_SYS_LARGEFILE
AC_TYPE_OFF_T
AM_LANGINFO_DECIMAL_POINT
AC_CHECK_SIZEOF(off_t)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(long long)
AC_CHECK_SIZEOF(void *)
AC_CHECK_SIZEOF(int)
AC_TYPE_SIGNAL

AC_CHECK_FUNCS(getcwd getwd strerror readlink setlocale access opendir sleep signal statvfs statfs clock getline)
AC_CHECK_FUNCS(vsnprintf snprintf strftime memmove strdup lstat fileno strcasecmp pathconf)

AC_LIB_PROG_LD
AC_PROG_FGREP
AC_DEFINE_UNQUOTED(FGREP_PROG, "$FGREP")

AC_MSG_CHECKING(for __func__)
AC_LINK_IFELSE(
  [AC_LANG_PROGRAM(,
    [ char *s; 
      s = (char *)__func__
    ])],
  [AC_DEFINE(HAVE___FUNC__)
   AC_MSG_RESULT(yes)
  ],
  [AC_MSG_RESULT(no)])

AC_PATH_PROG(PKG_CONFIG, pkg-config, no)

# check for pthreads 
AC_ARG_ENABLE(threads,
  [  --enable-threads      include pthread support],
  [
  if test "$enable_threads" = yes ; then
    if test "$ac_cv_header_pthread_h" = yes ; then
      AC_DEFINE(MUS_WITH_THREADS)
      CFLAGS="$CFLAGS -D_REENTRANT"
      LIBS="$LIBS -lpthread"
      RANDOM_FEATURES="$RANDOM_FEATURES threads"
    else
      AC_MSG_WARN([can't enable threads -- can't find pthread.h])
    fi]
  fi)

# look for special functions in libm
AC_CHECK_LIB(m, jn, 
  [AC_CHECK_LIB(m, yn, 
    [AC_CHECK_LIB(m, lgamma,
      [AC_CHECK_LIB(m, erf, 
        [AC_DEFINE(HAVE_SPECIAL_FUNCTIONS)])])])])


#--------------------------------------------------------------------------------
# fftw
#--------------------------------------------------------------------------------

FFTW_LIBS=""
FFTW_CFLAGS=""
if test "$with_fftw" != no; then
  AC_MSG_CHECKING(for fftw3)
  if test x$PKG_CONFIG != xno ; then
    if $PKG_CONFIG fftw3 --exists ; then
      FFTW_LIBS="`$PKG_CONFIG fftw3 --libs`"
      FFTW_CFLAGS="`$PKG_CONFIG fftw3 --cflags`"
      AC_DEFINE(HAVE_FFTW3)
      OPTIONAL_LIBRARIES="$OPTIONAL_LIBRARIES fftw"
      AC_MSG_RESULT(yes)
    else
      AC_MSG_RESULT(no)
      AC_MSG_CHECKING(for fftw2)
      if $PKG_CONFIG fftw --exists ; then
        FFTW_LIBS="`$PKG_CONFIG fftw --libs`"
        FFTW_CFLAGS="`$PKG_CONFIG fftw --cflags`"
        AC_DEFINE(HAVE_FFTW)
        OPTIONAL_LIBRARIES="$OPTIONAL_LIBRARIES fftw"
        AC_MSG_RESULT(yes)
      else
        AC_MSG_RESULT(no)
      fi
    fi
  else
    SAVELIBS=$LIBS
    LIBS="$LIBS -lfftw3 -lm"
    AC_MSG_CHECKING([for fftw-3])
    AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <fftw3.h>],
		                [fftw_plan plan; fftw_execute(plan)])],
		[AC_MSG_RESULT(yes)
		 AC_DEFINE(HAVE_FFTW3)
                 OPTIONAL_LIBRARIES="$OPTIONAL_LIBRARIES fftw"
		 FFTW_LIBS="-lfftw3"],
		[AC_MSG_RESULT(no)
                 LIBS=$SAVELIBS
                 LIBS="$LIBS -lrfftw -lfftw -lm"
                 AC_MSG_CHECKING([for fftw-2])
                 AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <rfftw.h>],
                                [rfftw_one(NULL, NULL, NULL)])],
		  		[AC_MSG_RESULT(yes)
		   		 AC_DEFINE(HAVE_FFTW)
                                 OPTIONAL_LIBRARIES="$OPTIONAL_LIBRARIES fftw"
		   		 FFTW_LIBS="-lrfftw -lfftw"],
		  		[AC_MSG_RESULT(no)])
		])
    LIBS=$SAVELIBS
  fi
fi
AC_SUBST(FFTW_LIBS)
AC_SUBST(FFTW_CFLAGS)


#--------------------------------------------------------------------------------
# complex trig
#--------------------------------------------------------------------------------

# having <complex.h> + a cacos declaration is not enough: C++ dies with a complaint about a "deprecated header"
#
#  I'm also using cexp and in xen creal, cimag, and _Complex_I
#  I'm currently using the data type "complex double" though the header seems to prefer "double complex" and
#  the gcc documentation mentions _Complex double.
#

LIBS="$LIBS -lm"
AC_MSG_CHECKING(for complex trig)
AC_LINK_IFELSE(
  	[AC_LANG_PROGRAM(
        	[#include <complex.h>],
		[ _Complex double val; 
                  double rl, im;
                  val = 1.0 + 0.5 * _Complex_I;
                  rl = creal(val);
                  im = cimag(val);
                  val = ccosh(cacosh(1.5) / 100.0)
                ])],
        [
 	 AC_DEFINE(HAVE_COMPLEX_TRIG)
	 AC_MSG_RESULT(yes)
    	],
	[AC_MSG_RESULT(no)])


#--------------------------------------------------------------------------------
# GSL
#--------------------------------------------------------------------------------

GSL_LIBS=""
GSL_CFLAGS=""
if test "$with_gsl" != no; then
AC_PATH_PROG(GSL_CONFIG, gsl-config, no)
AC_MSG_CHECKING(for GSL)
if test "$GSL_CONFIG" = "no" ; then
  AC_MSG_RESULT(no)
else
  GSL_CFLAGS=`$GSL_CONFIG --cflags`
  GSL_PREFIX=`$GSL_CONFIG --prefix`

  if test "$with_static_gsl" = yes ; then
    AC_CHECK_FILE($GSL_PREFIX/lib/libgsl.a,
	[GSL_LIBS="$GSL_PREFIX/lib/libgsl.a"],
	[GSL_LIBS=`$GSL_CONFIG --libs`
	 with_static_gsl=no
	 AC_MSG_WARN([can't find libgsl.a])])
  else
    GSL_LIBS=`$GSL_CONFIG --libs`
  fi

  gsl_version="`$GSL_CONFIG --version`"
  AC_MSG_RESULT($gsl_version)
  OPTIONAL_LIBRARIES="$OPTIONAL_LIBRARIES gsl"

  if test "`$GSL_CONFIG --version`" = "0.6" || test "`$GSL_CONFIG --version`" = "0.7" || test "`$GSL_CONFIG --version`" = "0.5"; then
    AC_MSG_WARN([Snd needs GSL 0.8 or later])
  else
    if test "`$GSL_CONFIG --version`" = "0.8" && test "$with_static_gsl" = yes ; then
      GSL_LIBS="$GSL_LIBS $GSL_PREFIX/lib/libgslcblas.a"
    fi
    AC_DEFINE(HAVE_GSL)
    AC_DEFINE_UNQUOTED(MUS_GSL_VERSION,"${gsl_version}")
  fi

fi
fi
AC_SUBST(GSL_LIBS)
AC_SUBST(GSL_CFLAGS)


#--------------------------------------------------------------------------------
# Ladspa
#--------------------------------------------------------------------------------

if test "$with_ladspa" != no ; then
  # we also need dlfcn.h and dirent.h here, but presumably this is on Linux
  AC_CHECK_HEADER(ladspa.h, 
		  [AC_DEFINE(HAVE_LADSPA)
                   RANDOM_FEATURES="$RANDOM_FEATURES ladspa"
                  ],
		  if test "$with_ladspa" = yes ; then
		   AC_MSG_WARN([can't find ladspa.h!])
		  fi)
fi


#--------------------------------------------------------------------------------
# X/Motif
#--------------------------------------------------------------------------------

ac_snd_have_gui=no

AC_PATH_XTRA

if test "$with_motif" = no && test "$with_gtk" = no ; then
  with_no_gui=yes
fi

if test "$with_motif" = yes && test "$with_gtk" = yes ; then
  with_gtk=no
  AC_MSG_WARN([You asked for both Motif and Gtk -- Motif will be used])
fi

if test "$no_x" = yes || test "$with_no_gui" = yes || test "$with_snd_as_pd_external"; then

  AC_DEFINE(USE_NO_GUI)
  XLIBS=""
  XFLAGS=""
  AC_SUBST(XLIBS)
  AC_SUBST(XFLAGS)
  GX_FILES="NO_GUI_O_FILES"
  GX_HEADERS="NO_GUI_HEADERS"

else

  AC_DEFINE(HAVE_X)
  XFLAGS="$X_CFLAGS"
  X_POST_LIBS="-lX11 $X_EXTRA_LIBS"

  case "$host" in
    *-apple-*)
      X_POST_LIBS="$X_POST_LIBS -lSM -lICE"
      ;;
  esac

# this needs to precede the Xm check
  TEST_PRE_LIBS="$X_LIBS $X_PRE_LIBS"
  TEST_POST_LIBS="$X_POST_LIBS"
  TEST_LIBS="$TEST_PRE_LIBS $TEST_POST_LIBS"

  AC_CHECK_LIB(Xext, XShapeQueryExtension, 
    [
     X_POST_LIBS="$X_POST_LIBS -lXext"
     TEST_POST_LIBS="$TEST_POST_LIBS -lXext"
     TEST_LIBS="$TEST_PRE_LIBS $TEST_POST_LIBS"
     AC_DEFINE(HAVE_XSHAPEQUERYEXTENSION)
    ], , $TEST_LIBS)

  if test "$with_static_motif" = yes ; then
    if test x$motif_prefix != x ; then
    AC_CHECK_FILE($motif_prefix/lib/libXm.a,[
      X_PRE_LIBS="$motif_prefix/lib/libXm.a $X_LIBS $X_PRE_LIBS -lXt"
      XFLAGS="-I$motif_prefix/include $XFLAGS"
	],	
	[
      AC_MSG_WARN(can't find $motif_prefix/lib/libXm.a!)
      X_PRE_LIBS="$X_LIBS $X_PRE_LIBS -lXm -lXt"
	])
    else
    AC_CHECK_FILE($x_libraries/libXm.a,[
      X_PRE_LIBS="$x_libraries/libXm.a $X_LIBS $X_PRE_LIBS -lXt"
	],
	[
      AC_MSG_WARN(can't find $x_libraries/libXm.a!)
      X_PRE_LIBS="$X_LIBS $X_PRE_LIBS -lXm -lXt"
	])
    fi
  else
    if test x$motif_prefix != x ; then
      X_PRE_LIBS="$X_LIBS $X_PRE_LIBS -L$motif_prefix/lib -lXm -lXt"
      XFLAGS="-I$motif_prefix/include $XFLAGS"
    else
      X_PRE_LIBS="$X_LIBS $X_PRE_LIBS -lXm -lXt"
    fi
  fi

  TEST_PRE_LIBS="$X_LIBS $X_PRE_LIBS"
  TEST_POST_LIBS="$X_POST_LIBS"
  TEST_LIBS="$TEST_PRE_LIBS $TEST_POST_LIBS"

  GX_FILES="X_O_FILES"
  GX_HEADERS="SND_X_HEADERS"

  if test "$with_gtk" != yes && test "$with_motif" != no ; then

    SAVELIBS=$LIBS
    SAVEFLAGS=$CFLAGS

    # search for libXm will fail with Xp complaints in Linux, so we need to search for -lXp first
    AC_CHECK_LIB(Xp, XpGetDocumentData, [
	         X_PRE_LIBS="$X_PRE_LIBS -lXp"
	         TEST_PRE_LIBS="$TEST_PRE_LIBS -lXp"
                 TEST_LIBS="$TEST_PRE_LIBS $TEST_POST_LIBS"
	         ], , $TEST_LIBS)

    LIBS="$LIBS $X_PRE_LIBS $X_POST_LIBS"
    CFLAGS="$CFLAGS $XFLAGS"

    AC_MSG_CHECKING([whether libXm requires libpng etc])
    AC_TRY_LINK_FUNC(XmCreateForm,
	AC_MSG_RESULT(no),
	[
	  AC_MSG_RESULT(yes)
	  X_PRE_LIBS="$X_PRE_LIBS -lXmu -lXft -L/usr/local/lib -liconv -lpng -ljpeg"
	  TEST_PRE_LIBS="$TEST_PRE_LIBS -lXmu -lXft -L/usr/local/lib -liconv -lpng -ljpeg"
	],
	$TEST_LIBS)

    if test "$with_editres" = yes ; then
	AC_CHECK_LIB(Xmu, _XEditResCheckMessages, 
			[AC_DEFINE(MUS_WITH_EDITRES)
	                 OPTIONAL_LIBRARIES="$OPTIONAL_LIBRARIES editres"
			 X_PRE_LIBS="$X_PRE_LIBS -lXmu"
   		         TEST_PRE_LIBS="$TEST_PRE_LIBS -lXmu"
			])
    else
        # need to check for -lXmu required by (buggy) libXm.a (openmotif 2.2.2)
	AC_MSG_CHECKING([whether libXm requires libXmu])
	AC_TRY_LINK_FUNC(XmCreateForm,
		AC_MSG_RESULT(no),
		[
		  AC_MSG_RESULT(yes)
		  X_PRE_LIBS="$X_PRE_LIBS -lXmu"
		  TEST_PRE_LIBS="$TEST_PRE_LIBS -lXmu"
		],
		$TEST_LIBS)
    fi

    XLIBS="$X_PRE_LIBS $X_POST_LIBS"
    TEST_LIBS="$TEST_PRE_LIBS $TEST_POST_LIBS"

    AC_CHECK_LIB(m, XmCreateForm, 
          # was Xm here but that introduces a bogus -lXm into the load list
	[
	  with_motif=yes
	  ac_snd_have_gui=yes

	  if test "$with_xpm" != no; then
  	    AC_CHECK_LIB(Xpm, XpmReadFileToPixmap,
              [
  	       XLIBS="$XLIBS -lXpm"
	       TEST_LIBS="$TEST_LIBS -lXpm"
   	       AC_DEFINE(HAVE_XPM)
   	      ], , $TEST_LIBS)
   	    AC_CHECK_LIB(Xpm, XpmGetErrorString,           [AC_DEFINE(HAVE_XPM_GET_ERROR_STRING)], ,             $TEST_LIBS)
   	    AC_CHECK_LIB(Xpm, XpmCreateXpmImageFromPixmap, [AC_DEFINE(HAVE_XPM_CREATE_XPM_IMAGE_FROM_PIXMAP)], , $TEST_LIBS)
	  fi

	  if test "$with_static_xm" = yes ; then
  	    GX_FILES="XM_O_FILES"
	    AC_DEFINE(HAVE_STATIC_XM)
          fi

	  AC_SUBST(XLIBS)
	  AC_SUBST(XFLAGS)

          AC_MSG_CHECKING(for XmDataField)
          AC_LINK_IFELSE(
	    [AC_LANG_PROGRAM(
        	[#include <Xm/XmAll.h>
                 #include <Xm/DataF.h>],
		[ Widget w;
                  w = XmCreateDataField(NULL, "data-field", NULL, 0)
                ])],
            [
 	     AC_DEFINE(HAVE_XmCreateDataField)
	     AC_MSG_RESULT(yes)
    	    ],
	    [AC_MSG_RESULT(no)])

          AC_MSG_CHECKING(for XmButtonBox)
          AC_LINK_IFELSE(
	    [AC_LANG_PROGRAM(
        	[#include <Xm/XmAll.h>
                 #include <Xm/ButtonBox.h>],
		[ Widget w; int i; i = XmIconTop;
                  w = XmCreateButtonBox(NULL, "button-box", NULL, 0)
                ])],
            [
 	     AC_DEFINE(HAVE_XmCreateButtonBox)
	     AC_MSG_RESULT(yes)
    	    ],
	    [AC_MSG_RESULT(no)])

          AC_MSG_CHECKING(for XmTabStack)
          AC_LINK_IFELSE(
	    [AC_LANG_PROGRAM(
        	[#include <Xm/XmAll.h>
                 #include <Xm/TabStack.h>],
		[ Widget w, w1, w2; 
                  w = XmCreateTabStack(NULL, "tab-stack", NULL, 0);
                  /* w1 = XmTabStackXYToWidget(w, 0, 0); */
		  w2 = XmTabStackIndexToWidget(w, 0)
                ])],
            [
 	     AC_DEFINE(HAVE_XmCreateTabStack)
	     AC_MSG_RESULT(yes)
    	    ],
	    [AC_MSG_RESULT(no)])

          AC_MSG_CHECKING(for XmTabStackXYToWidget)
          AC_LINK_IFELSE(
	    [AC_LANG_PROGRAM(
        	[#include <Xm/XmAll.h>
                 #include <Xm/TabStack.h>],
		[ Widget w, w1, w2; 
                  w = XmCreateTabStack(NULL, "tab-stack", NULL, 0);
                  w1 = XmTabStackXYToWidget(w, 0, 0)
                ])],
            [
 	     AC_DEFINE(HAVE_XmTabStackXYToWidget)
	     AC_MSG_RESULT(yes)
    	    ],
	    [AC_MSG_RESULT(no)])

          AC_MSG_CHECKING(for XmColumn)
          AC_LINK_IFELSE(
	    [AC_LANG_PROGRAM(
        	[#include <Xm/XmAll.h>
                 #include <Xm/Column.h>],
		[ Widget w;
                  w = XmCreateColumn(NULL, "column", NULL, 0)
                  /* w = XmColumnGetChildLabel(w) */
                ])],
            [
 	     AC_DEFINE(HAVE_XmCreateColumn)
	     AC_MSG_RESULT(yes)
    	    ],
	    [AC_MSG_RESULT(no)])

          AC_MSG_CHECKING(for XmColumnGetChildLabel)
          AC_LINK_IFELSE(
	    [AC_LANG_PROGRAM(
        	[#include <Xm/XmAll.h>
                 #include <Xm/Column.h>],
		[ Widget w;
                  w = XmCreateColumn(NULL, "column", NULL, 0);
                  w = XmColumnGetChildLabel(w)
                ])],
            [
 	     AC_DEFINE(HAVE_XmColumnGetChildLabel)
	     AC_MSG_RESULT(yes)
    	    ],
	    [AC_MSG_RESULT(no)])

          AC_MSG_CHECKING(for XmDropDown)
          AC_LINK_IFELSE(
	    [AC_LANG_PROGRAM(
        	[#include <Xm/XmAll.h>
                 #include <Xm/DropDown.h>],
		[ Widget w;
                  w = XmCreateDropDown(NULL, "drop-down", NULL, 0)
                ])],
            [
 	     AC_DEFINE(HAVE_XmCreateDropDown)
	     AC_MSG_RESULT(yes)
    	    ],
	    [AC_MSG_RESULT(no)])

          AC_MSG_CHECKING(for XmCreateFontSelector)
          AC_LINK_IFELSE(
	    [AC_LANG_PROGRAM(
        	[#include <Xm/XmAll.h>
                 #include <Xm/FontS.h>],
		[ Widget w;
                  w = XmCreateFontSelector(NULL, "font-selector", NULL, 0)
                ])],
            [
 	     AC_DEFINE(HAVE_XmCreateFontSelector)
	     AC_MSG_RESULT(yes)
    	    ],
	    [AC_MSG_RESULT(no)])

          AC_MSG_CHECKING(for XmCreateColorSelector)
          AC_LINK_IFELSE(
	    [AC_LANG_PROGRAM(
        	[#include <Xm/XmAll.h>
                 #include <Xm/ColorS.h>],
		[ Widget w;
                  w = XmCreateColorSelector(NULL, "color-selector", NULL, 0)
                ])],
            [
 	     AC_DEFINE(HAVE_XmCreateColorSelector)
	     AC_MSG_RESULT(yes)
    	    ],
	    [AC_MSG_RESULT(no)])

          AC_MSG_CHECKING(for XmToolTipGetLabel)
          AC_LINK_IFELSE(
	    [AC_LANG_PROGRAM(
        	[#include <Xm/XmAll.h>],
		[ Widget w = NULL;
                  w = XmToolTipGetLabel(w)
                ])],
            [
 	     AC_DEFINE(HAVE_XmToolTipGetLabel)
	     AC_MSG_RESULT(yes)
    	    ],
	    [AC_MSG_RESULT(no)])

	  AC_MSG_CHECKING([whether sashes support relative panes])
	  AC_COMPILE_IFELSE(
            [AC_LANG_PROGRAM([#include <Xm/SashP.h>],
			     [[ Widget w; int happy = 0;
                                SashCallData call_data;
                                if ((XtIsSubclass(w, xmSashWidgetClass)) && 
                                    (strcmp(call_data->params[0], "Start") == 0)) {happy = 1;}
			     ]])],
	    [
	     AC_DEFINE(WITH_RELATIVE_PANES)
	     AC_MSG_RESULT(yes)
	    ],
	    [AC_MSG_RESULT(no)

            # perhaps the user hasn't installed the Motif headers? 
             AC_MSG_CHECKING([for Motif headers])
	     AC_COMPILE_IFELSE(
               [AC_LANG_PROGRAM([#include <Xm/XmAll.h>],
			        [[ Widget w; 
				   w = XmCreateForm(NULL, "form", NULL, 0)
			        ]]
                               )],
	      [AC_MSG_RESULT(yes)],
	      [AC_MSG_RESULT(no)
	       AC_MSG_WARN([can't find the Motif headers! These are in openmotif-devel or some such package, or perhaps you need to include the --with-motif-prefix switch: --with-motif-prefix=/usr/pkg for example -- I will look for Gtk])
               with_gtk=yes
	       with_motif=no
              ])
	    ])

	  if test "$with_xp" = yes; then
	    AC_MSG_CHECKING([for XmPrint support])
	    AC_COMPILE_IFELSE(
	      [AC_LANG_PROGRAM([#include <Xm/Print.h>],
			       [ Widget w; 
                                 XmPrintShellCallbackStruct xmp;
			         XtIsSubclass(w, xmPrintShellWidgetClass)
			        ])],
	      [
	       AC_DEFINE(HAVE_XM_XP)
               OPTIONAL_LIBRARIES="$OPTIONAL_LIBRARIES Xp"
	       AC_MSG_RESULT(yes)
 	      ],
	      [AC_MSG_RESULT(no)])
           fi
          ],
	[
         AC_MSG_RESULT(no)
	 AC_MSG_WARN([can't find the Motif library! -- will look for Gtk])
         with_gtk=yes
	 with_motif=no
        ],
	$TEST_LIBS)
    LIBS=$SAVELIBS
    CFLAGS=$SAVEFLAGS

  else
    if test "$with_gtk" != no ; then
      with_gtk=yes
    fi
  fi

  if test "$with_motif" = yes ; then
    AC_DEFINE(USE_MOTIF)  # for Snd
    AC_DEFINE(HAVE_MOTIF) # for xm
    GRAPHICS_TOOLKIT="Motif"
  fi


#--------------------------------------------------------------------------------
# Gtk
#--------------------------------------------------------------------------------

  if test "$with_gtk" = yes ; then

    AM_PATH_GTK_2_0(2.0.0,
      [
	with_gtk=yes
      ],
      [
    	AC_MSG_WARN([trouble with gtk -- will try to make Snd without any GUI])
      	with_gtk=no
      ])
    if test "$with_gtk" = yes ; then
     	GX_FILES="G_O_FILES"
      	GX_HEADERS="SND_G_HEADERS"

	case "$host" in
	  *-*-solaris*) 
	    GTK_LIBS="$GTK_LIBS -lX11"
	    ;; 
	esac

      	AC_SUBST(GTK_LIBS)
      	AC_SUBST(GTK_CFLAGS)
      	AC_DEFINE(USE_GTK)
        GRAPHICS_TOOLKIT="Gtk"
        ac_snd_have_gui=yes

	GTK_LD_LIBS="$GTK_LIBS"
	if test x$PKG_CONFIG != xno ; then
          GTK_LD_LIBS="`$PKG_CONFIG gtk+-2.0 --libs-only-L` `$PKG_CONFIG gtk+-2.0 --libs-only-l`"
          pango_version="`$PKG_CONFIG pango --modversion`"
          AC_DEFINE_UNQUOTED(MUS_PANGO_VERSION,"${pango_version}")
        fi

      	AC_SUBST(GTK_LD_LIBS)

	AC_CHECK_LIB(m, gdk_draw_pixbuf, [AC_DEFINE(HAVE_GDK_DRAW_PIXBUF)], ,$GTK_LIBS)
	# gtk 2.1
	AC_CHECK_LIB(m, gtk_tree_view_column_cell_get_position, [AC_DEFINE(HAVE_GTK_TREE_VIEW_COLUMN_CELL_GET_POSITION)], ,$GTK_LIBS)
        AC_CHECK_LIB(m, gtk_widget_get_clipboard, [AC_DEFINE(HAVE_GTK_WIDGET_GET_CLIPBOARD)], ,$GTK_LIBS)
	# gtk 2.2
	AC_CHECK_LIB(m, gtk_file_chooser_dialog_new, [AC_DEFINE(HAVE_GTK_FILE_CHOOSER_DIALOG_NEW)], ,$GTK_LIBS)
	# gtk 2.3
        AC_CHECK_LIB(m, gtk_text_buffer_select_range, [AC_DEFINE(HAVE_GTK_TEXT_BUFFER_SELECT_RANGE)], ,$GTK_LIBS)
	AC_CHECK_LIB(m, gtk_file_chooser_set_filename,
	[
    	  OLD_CFLAGS="$CFLAGS"
          CFLAGS="$GTK_CFLAGS $CFLAGS"
	  AC_MSG_CHECKING([for gboolean gtk_file_chooser_set_filename])
	  AC_COMPILE_IFELSE(
	    [AC_LANG_PROGRAM([#include <gtk/gtk.h>],
			     [ GtkWidget *w; 
			       gboolean result;
			       result = gtk_file_chooser_set_filename(GTK_FILE_CHOOSER(w), "hiho")
			     ])],
	    [
             AC_DEFINE(HAVE_GBOOLEAN_GTK_FILE_CHOOSER_SET_FILENAME)
	     AC_MSG_RESULT(yes)
 	    ],
            [AC_MSG_RESULT(no)])
	  CFLAGS="$OLD_CFLAGS"
	], ,$GTK_LIBS)
	# gtk 2.3.6
	AC_CHECK_LIB(m, gtk_button_set_alignment, [AC_DEFINE(HAVE_GTK_BUTTON_SET_ALIGNMENT)], ,$GTK_LIBS)
	# gtk 2.4.0|1|2|3|4: no API changes
	AC_CHECK_LIB(m, gtk_about_dialog_new, [AC_DEFINE(HAVE_GTK_ABOUT_DIALOG_NEW)], ,$GTK_LIBS)
	# gtk 2.5.0
	AC_CHECK_LIB(m, gdk_pango_renderer_new, [AC_DEFINE(HAVE_GDK_PANGO_RENDERER_NEW)],  ,$GTK_LIBS)
	# gtk 2.5.6
	AC_CHECK_LIB(m, gtk_text_layout_get_iter_at_position, [AC_DEFINE(HAVE_GTK_TEXT_LAYOUT_GET_ITER_AT_POSITION)],  ,$GTK_LIBS)
	# gtk 2.6.0|1|2|3|4|5|6|7|8: no API changes
	AC_CHECK_LIB(m, pango_matrix_rotate, [AC_DEFINE(HAVE_PANGO_MATRIX_ROTATE)],  ,$GTK_LIBS)
        AC_CHECK_LIB(m, gtk_menu_bar_get_child_pack_direction, 
	    [
	      AC_DEFINE(HAVE_GTK_MENU_BAR_GET_CHILD_PACK_DIRECTION)
	      if test x$PKG_CONFIG != xno ; then
                  CAIRO_CFLAGS="`$PKG_CONFIG cairo --cflags-only-I`"
      	          AC_SUBST(CAIRO_CFLAGS)
	          cairo_version="`$PKG_CONFIG cairo --modversion`"
                  AC_DEFINE_UNQUOTED(MUS_CAIRO_VERSION,"${cairo_version}")
              fi
	    ],  ,$GTK_LIBS)
	# gtk 2.7.0
	# cairo needs xfixes xextensions fixesext libXfixes fontconfig libXrender libpixman libXft2 glitz 
	#   and innumerable *.pc files scattered all over the world-wide-web
	AC_CHECK_LIB(m, gtk_tree_view_get_visible_range, [AC_DEFINE(HAVE_GTK_TREE_VIEW_GET_VISIBLE_RANGE)],  ,$GTK_LIBS)
	# gtk 2.7.3
	AC_CHECK_LIB(m, gtk_dialog_get_response_for_widget, [AC_DEFINE(HAVE_GTK_DIALOG_GET_RESPONSE_FOR_WIDGET)],  ,$GTK_LIBS)
	# for snd-gfile (2.7.0)
	AC_CHECK_LIB(m, gtk_entry_set_alignment, [AC_DEFINE(HAVE_GTK_ENTRY_SET_ALIGNMENT)], ,$GTK_LIBS)
	# for snd-prefs/gfile (2.3.6)
	AC_CHECK_LIB(m, gtk_combo_box_entry_new_text, [AC_DEFINE(HAVE_GTK_COMBO_BOX_ENTRY_NEW_TEXT)], ,$GTK_LIBS)
	# for snd-prefs/gfile (2.3.6)
	AC_CHECK_LIB(m, gtk_link_button_new, [AC_DEFINE(HAVE_GTK_LINK_BUTTON_NEW)], ,$GTK_LIBS)
	# for 2.9.0
	AC_CHECK_LIB(m, gtk_label_get_line_wrap_mode, [AC_DEFINE(HAVE_GTK_LABEL_GET_LINE_WRAP_MODE)], ,$GTK_LIBS)
	# for 2.10.0

	XLIBS=""
      	XFLAGS=""
      	AC_SUBST(XLIBS)
      	AC_SUBST(XFLAGS)
      	if test "$with_static_xm" = yes ; then
          GX_FILES="XG_O_FILES"
          AC_DEFINE(HAVE_STATIC_XM)
      	fi
     fi
  fi

  if test "$with_gtk" != yes && test "$with_motif" != yes ; then

  AC_DEFINE(USE_NO_GUI)
  XLIBS=""
  XFLAGS=""
  AC_SUBST(XLIBS)
  AC_SUBST(XFLAGS)
  GX_FILES="NO_GUI_O_FILES"
  GX_HEADERS="NO_GUI_HEADERS"
  fi

fi

AC_SUBST(GX_FILES)
AC_SUBST(GX_HEADERS)


#--------------------------------------------------------------------------------
# OpenGL
#--------------------------------------------------------------------------------

GL_LIBS=""
GL_FILES=""
GL_FLAGS=""
have_gl=no
if test "$with_gl" = yes || test "$with_just_gl" = yes ; then
  if test "$with_motif" = yes || test "$with_gtk" = yes ; then
    AC_CHECK_HEADER(GL/gl.h,
      [have_gl=yes],
      [
       OLD_CFLAGS="$CFLAGS"
       CFLAGS="-I/usr/X11R6/include $CFLAGS"
       # can't use AC_CHECK_HEADER here (GL/gl.h includes GL/glext.h, so the -I business has to be set up first)

       AC_MSG_CHECKING([for /usr/X11R6/include/GL/gl.h])
       AC_COMPILE_IFELSE(
         [AC_LANG_PROGRAM([#include <GL/gl.h>],
		          [int i; i = GL_TRUE])],
           [have_gl=yes
            GL_FLAGS="-I/usr/X11R6/include"
            CFLAGS="$OLD_CFLAGS"
            AC_MSG_RESULT(yes)
           ],
         [AC_MSG_WARN(can't find GL headers)])])

    # in FC5 glu.h and libGLU are missing by default -- do they come from Mesa?
    if test "$have_gl" = yes ; then
      AC_CHECK_HEADER(GL/glu.h, 
	  [AC_DEFINE(HAVE_GLU)
           GL_LIBS="$GL_LIBS -lGLU"
          ])
      GL_LIBS="$GL_LIBS -lGL"
      if test "$with_gtk" = yes ; then
        # motif uses built-in support, but gtk uses gtkglext, so we need to check for the latter
        if test x$PKG_CONFIG != xno && $PKG_CONFIG gtkglext-1.0 --exists ; then
          GL_LIBS="$GL_LIBS `$PKG_CONFIG gtkglext-1.0 --libs`"
          GL_FLAGS="$GL_FLAGS `$PKG_CONFIG gtkglext-1.0 --cflags`"
          AC_CHECK_LIB(m, gdk_gl_context_copy, 
            [
	      AC_DEFINE(HAVE_GDK_GL_CONTEXT_COPY)
              have_gl=yes
            ],
            [
	      AC_MSG_WARN([gtkglext version is too old])
              have_gl=no
            ], 
            $GL_LIBS)
        else
          have_gl=no
          AC_MSG_WARN([can't find gtkglext])
        fi
      fi
    fi
  else
    AC_MSG_WARN([GL only works with Motif or Gtk2])
  fi
fi

if test "$have_gl" = yes ; then
  AC_DEFINE(HAVE_GL)
  OPTIONAL_LIBRARIES="$OPTIONAL_LIBRARIES openGL"
  if test "$with_gl2ps" = yes ; then
    AC_DEFINE(MUS_WITH_GL2PS)
    RANDOM_FEATURES="$RANDOM_FEATURES gl2ps"
    GL_FILES="gl2ps.o"
  fi
  if test "$with_just_gl" = yes ; then
    AC_DEFINE(JUST_GL)
  else
    AC_COMPILE_IFELSE(
      [AC_LANG_PROGRAM([#include <GL/gl.h>], 
                       [int i; i = GL_TEXTURE_BINDING_3D])],
      [GL_FILES="$GL_FILES gl.o"],
      [AC_DEFINE(JUST_GL)])
  fi   
  case "$host" in
    *-apple-*)
      GL_LIBS="-framework OpenGL"
      # is this in place of or in addition to -lGL etc?
      ;;
  esac
else
  GL_LIBS=""
  GL_FILES=""
  GL_FLAGS=""
fi
AC_SUBST(GL_LIBS)
AC_SUBST(GL_FILES)
AC_SUBST(GL_FLAGS)


#--------------------------------------------------------------------------------
# fam/gamin (needs GUI)
#--------------------------------------------------------------------------------

FAM_LIB=""
if test "$ac_snd_have_gui" != no ; then
  if test "$with_fam" != no ; then
    # look for the File Alteration Monitor (gamin or SGI's fam -- both use fam.h and libfam apparently)
    AC_MSG_CHECKING(for Gamin)
    if test x$PKG_CONFIG != xno && $PKG_CONFIG gamin --exists ; then
       gamin_version="`$PKG_CONFIG gamin --modversion`"
       AC_MSG_RESULT($gamin_version)
       # before version 0.0.18, gamin.pc messed up the --libs result, so we'll just insist on a later version
       if $PKG_CONFIG --atleast-version 0.1.0 gamin; then
         AC_DEFINE_UNQUOTED(MUS_GAMIN_VERSION,"${gamin_version}")
         FAM_LIB="`$PKG_CONFIG gamin --libs`"
         AC_DEFINE(HAVE_FAM)
         AC_DEFINE(HAVE_FAM_NO_EXISTS)
         OPTIONAL_LIBRARIES="$OPTIONAL_LIBRARIES gamin"
       else
	AC_MSG_WARN([need gamin version 0.1.0 or later])
       fi
    else
      AC_MSG_RESULT(no)
      AC_MSG_CHECKING(for Fam)
      AC_CHECK_LIB(fam, FAMOpen, 
       [
         AC_DEFINE(HAVE_FAM)
         FAM_LIB="-lfam"
         OPTIONAL_LIBRARIES="$OPTIONAL_LIBRARIES fam"
       ])
    fi
  fi
fi
AC_SUBST(FAM_LIB)



ac_snd_have_extension_language=no

#--------------------------------------------------------------------------------
# Gauche
#--------------------------------------------------------------------------------

if test "$with_gauche" = yes ; then
  AC_PATH_PROG(GAUCHE_CONFIG, gauche-config, no)
  AC_MSG_CHECKING([for Gauche])
  if test "$GAUCHE_CONFIG" = "no" ; then
    AC_MSG_WARN([can't find Gauche!])
    AC_DEFINE(HAVE_EXTENSION_LANGUAGE,0)
  else
    # check for version >= 0.8.7
    gauche_version="`$GAUCHE_CONFIG -V`"
    AC_MSG_RESULT($gauche_version)

    OLD_CFLAGS="CFLAGS"
    # gauche-config inserts single-quotes in the -I and -L results thereby rendering them useless,
    #   so the following sed command tries to remove them.  You apparently can't quote a single-quote
    #   in this context, so I use \x27 = ASCII for single quote.
#   GUILE_CFLAGS=`$GAUCHE_CONFIG -I | sed 's/\x27//g'`
    GUILE_CFLAGS=`$GAUCHE_CONFIG -I | tr -d "'"` 
# apparently the sed line depends on the version of sed -- mine works (4.1.5) but others say it doesn't
#   thanks to Kjetil for the replacement using "tr"
    CFLAGS="$CFLAGS $GUILE_CFLAGS"

    AC_COMPILE_IFELSE(
     [AC_LANG_PROGRAM([#include <gauche.h>],
                      [#if (GAUCHE_MAJOR_VERSION == 0)
                         #if (GAUCHE_MINOR_VERSION < 8)
			    #error too old
			 #else
			   #if (GAUCHE_MINOR_VERSION == 8) && (GAUCHE_MICRO_VERSION < 7)
			     #error too old
                           #endif
			 #endif
		       #endif                                  
                  ])], 
                [
                 AC_DEFINE(HAVE_GAUCHE)
                 AC_DEFINE(HAVE_SCHEME)
                 ac_snd_have_extension_language=yes
                 AC_DEFINE(HAVE_EXTENSION_LANGUAGE)
                 GUILE_LIBS=`$GAUCHE_CONFIG -L  | sed 's/\x27//g'`
                 GUILE_LIBS="$GUILE_LIBS -lgauche"
                 LOCAL_LANGUAGE="Gauche"
        	 AC_DEFINE(WITH_RUN)

	         # 0.8.8
                 AC_CHECK_LIB(gauche, Scm_EvalRec, [AC_DEFINE(HAVE_SCM_EVALREC)])
                ],
		[
             	 AC_MSG_WARN([We need Gauche 0.8.7 or later])
                 with_gauche=no
                 CFLAGS="$OLD_CFLAGS"
                 GUILE_CFLAGS=""
		])
  fi
fi


#--------------------------------------------------------------------------------
# Ruby
#--------------------------------------------------------------------------------

RUBY="ruby"
AC_ARG_WITH(ruby-prefix,[  --with-ruby-prefix=PFX  where Ruby is installed],
            	ruby_prefix="$withval"
		RUBY="$ruby_prefix/bin/ruby",
		ruby_prefix="")

if test "$with_ruby" = yes && test "$ac_snd_have_extension_language" = yes ; then
  with_ruby=no
  AC_MSG_WARN([You asked for both Ruby and $LOCAL_LANGUAGE -- $LOCAL_LANGUAGE will be used])
fi

AC_ARG_WITH(ruby,
	[  --with-ruby	  	  try to use Ruby as the extension language],
  	if test "$with_ruby" = yes ; then
            AC_MSG_CHECKING([for Ruby])
	    RUBY_VERSION=`$RUBY -e 'puts RUBY_VERSION'`
	    RUBY_RELEASE_DATE=`$RUBY -e 'puts RUBY_RELEASE_DATE'`
            if test "$RUBY_VERSION" > "0" ; then
	      AC_DEFINE(HAVE_RUBY)
	      AC_DEFINE(HAVE_EXTENSION_LANGUAGE)
              AC_MSG_RESULT($RUBY_VERSION)
              AC_DEFINE_UNQUOTED(MUS_RUBY_VERSION,"${RUBY_VERSION}")
              AC_DEFINE_UNQUOTED(RUBY_RELEASE_DATE,"${RUBY_RELEASE_DATE}")

	      GUILE_CFLAGS=`$RUBY -e [['\$:.each {|path| print "-I", path, " "}']]`
	      GUILE_LIBS=`$RUBY -e [['\$:.each {|path| print "-L", path, " "}']]`
	      RUBY_SEARCH_PATH=`$RUBY -e [['\$:.each {|path| print path, ":"}']]`
	      RUBY_LIBS=`$RUBY -e [['require "rbconfig"; include Config; print CONFIG["LIBS"]']]`

	      AC_DEFINE_UNQUOTED(RUBY_SEARCH_PATH,"${RUBY_SEARCH_PATH}")
              GUILE_LIBS="$GUILE_LIBS -lruby $RUBY_LIBS"
	      AC_CHECK_LIB(readline, readline, 
			   [AC_DEFINE(HAVE_READLINE)
			    GUILE_LIBS="$GUILE_LIBS -lreadline -lncurses"], ,"-lncurses")
              AC_SUBST(GUILE_LIBS)
              AC_SUBST(GUILE_CFLAGS)

              OLD_LIBS="$LIBS"
    	      LIBS="$GUILE_LIBS $LIBS"
              OLD_CFLAGS="$CFLAGS"
              CFLAGS="$GUILE_CFLAGS -lm $CFLAGS"
              AC_CHECK_LIB(ruby, rb_gc_disable, [AC_DEFINE(HAVE_RB_GC_DISABLE)])
	      AC_CHECK_LIB(ruby, rb_ary_dup, [AC_DEFINE(HAVE_RB_ARY_DUP)])
	      AC_CHECK_LIB(ruby, rb_errinfo, [AC_DEFINE(HAVE_RB_ERRINFO)])

    	      AC_MSG_CHECKING([for working rb_gc_mark])
              AC_COMPILE_IFELSE(
                [AC_LANG_PROGRAM([#include <ruby.h>],
                                 [VALUE val; rb_gc_mark(val)])], 
                [AC_DEFINE(HAVE_REASONABLE_RB_GC_MARK)
                 AC_MSG_RESULT(yes)
                ],
                 AC_MSG_RESULT(no)
                )

	      CFLAGS="$CFLAGS -Werror"
    	      AC_MSG_CHECKING([that rb_find_file deals in char*])
              AC_COMPILE_IFELSE(
                [AC_LANG_PROGRAM([#include <ruby.h>],
                                 [char *file; file = rb_find_file("hiho")])], 
                [AC_MSG_RESULT(yes)
                ],
		[AC_DEFINE(RB_FIND_FILE_TAKES_VALUE)
                 AC_MSG_RESULT(no)
		])

              LIBS="$OLD_LIBS"
              CFLAGS="$OLD_CFLAGS"
              LOCAL_LANGUAGE="Ruby"
	      ac_snd_have_extension_language=yes

            else 
              AC_MSG_WARN([can't find Ruby!])
  	      AC_DEFINE(HAVE_RUBY,0)
              AC_DEFINE(HAVE_EXTENSION_LANGUAGE,0)
            fi
	fi)


#--------------------------------------------------------------------------------
# Forth
#--------------------------------------------------------------------------------

if test "$with_forth" = yes && test "$ac_snd_have_extension_language" = yes ; then
  with_forth=no
  AC_MSG_WARN([You asked for both Forth and $LOCAL_LANGUAGE -- $LOCAL_LANGUAGE will be used])
fi

AC_ARG_WITH(forth,
	[  --with-forth	  	  try to use Forth as the extension language],
  	if test "$with_forth" = yes ; then
	   FTH_CHECK_LIB([AC_DEFINE(HAVE_FORTH)
 	                  AC_DEFINE(HAVE_EXTENSION_LANGUAGE)
			  if test "$FTH_HAVE_COMPLEX" = yes ; then
     	      		     AC_DEFINE(HAVE_COMPLEX_TRIG)
     	      		     AC_DEFINE(HAVE_SCM_MAKE_COMPLEX)
     	      		     AC_DEFINE(HAVE_SCM_C_MAKE_RECTANGULAR)
			  fi
			  if test "$FTH_HAVE_RATIO" = yes ; then
     	      		     AC_DEFINE(HAVE_SCM_MAKE_RATIO)
			  fi
               		  AC_SUBST(GUILE_CFLAGS, $FTH_CFLAGS)
               		  AC_SUBST(GUILE_LIBS,   $FTH_LIBS)
 	      		  LOCAL_LANGUAGE="Forth"
 	      		  ac_snd_have_extension_language=yes])
	fi)


#--------------------------------------------------------------------------------
# Guile
#--------------------------------------------------------------------------------

if test "$with_guile" = yes && test "$ac_snd_have_extension_language" = yes ; then
  with_guile=no
  AC_MSG_WARN([You asked for both Guile and $LOCAL_LANGUAGE -- $LOCAL_LANGUAGE will be used])
fi

if test "$with_guile" != no && test "$with_ruby" != yes && test "$with_forth" != yes && test "$with_gauche" != yes ; then

GUILE_LIBS=""
GUILE_CFLAGS=""
if test "$GUILE_CONFIG_name" = "" ; then
  GUILE_CONFIG_name="guile-config"
fi
if test "$GUILE_name" = "" ; then
  GUILE_name="guile"
fi

# GUILE_CONFIG_path=""
# allow user to specify this in invocation line
GUILE_LIB_path=""
XM_LIBS=""

if test "$with_guile" = no && test "$with_snd_as_pd_external" = no; then
  AC_DEFINE(HAVE_GUILE,0)
  AC_DEFINE(HAVE_EXTENSION_LANGUAGE,0)
else

  AC_CHECK_FILE(/usr/lib/snd/bin/${GUILE_CONFIG_name},[
    GUILE_CONFIG_path=/usr/lib/snd/bin/
    GUILE_LIB_path=/usr/lib/snd/lib
	])

  AC_MSG_CHECKING(for Guile)
  if test "$GUILE_CONFIG_path" != "" ; then
    if ! test -x "${GUILE_CONFIG_path}${GUILE_CONFIG_name}" ; then
      # try adding the "/" to the path    
      GUILE_CONFIG_path="${GUILE_CONFIG_path}/"
      if ! test -x "${GUILE_CONFIG_path}${GUILE_CONFIG_name}" ; then
        AC_MSG_WARN([Guile config path is $GUILE_CONFIG_path, but I can't find guile-config!])
      fi
    fi
  fi

  if (${GUILE_CONFIG_path}${GUILE_CONFIG_name} link > /dev/null) 2>&1; then
    GUILE_CONFIG_works=yes
    guile_version="`${GUILE_CONFIG_path}${GUILE_name} -c '(display (version))'`"
    AC_MSG_RESULT($guile_version)
  else
    GUILE_CONFIG_works=no
    AC_MSG_RESULT(no)
  fi

if test $GUILE_CONFIG_works = yes; then

  GUILE_CFLAGS="`${GUILE_CONFIG_path}${GUILE_CONFIG_name} compile`"

  if test "$GUILE_LIB_path" != "" ; then
    # MacOSX linker doesn't know the -rpath stuff, but user may have placed Guile in /usr/lib/snd/bin
    case "$host" in
      *-apple-*)
        GUILE_LIBS="`${GUILE_CONFIG_path}${GUILE_CONFIG_name} link`"
        ;;
      *)
        GUILE_LIBS="-Xlinker -rpath -Xlinker $GUILE_LIB_path `${GUILE_CONFIG_path}${GUILE_CONFIG_name} link`"
	XM_LIBS="`${GUILE_CONFIG_path}${GUILE_CONFIG_name} link`"
	;;
    esac
  else
    # GUILE_LIB_path is empty
    GUILE_LIBS="`${GUILE_CONFIG_path}${GUILE_CONFIG_name} link`"
  fi

  if test "`${GUILE_CONFIG_path}${GUILE_name} -c '(display (string>=? (version) "1.3.4"))'`" != "#t"; then
    AC_MSG_WARN(Snd needs Guile 1.3.4 or later)
    AC_DEFINE(HAVE_GUILE,0)
    AC_DEFINE(HAVE_EXTENSION_LANGUAGE,0)
  else
    AC_SUBST(GUILE_CFLAGS)
    if test "$XM_LIBS" = ""; then
      XM_LIBS="$GUILE_LIBS"
      if test "`${GUILE_CONFIG_path}${GUILE_name} -c '(display (string>=? (version) "1.6.8"))'`" = "#t"; then
        # the -pthread business confuses ld
        new_libs=`echo " $XM_LIBS " | sed -e 's/-pthread //'`
        LD_XM_LIBS="$new_libs"
      fi
      if test "$with_snd_as_pd_external" && test "`${GUILE_CONFIG_path}${GUILE_name} -c '(display (string>=? (version) "1.7.2"))'`" != "#t"; then
	 AC_MSG_ERROR(Snd needs guile V1.7.2 or newer to work as a pd external)
      fi
    fi

    AC_SUBST(XM_LIBS)
    AC_SUBST(LD_XM_LIBS)

    AC_DEFINE(HAVE_GUILE)
    AC_DEFINE(HAVE_SCHEME)
    AC_DEFINE(HAVE_EXTENSION_LANGUAGE)
    LOCAL_LANGUAGE="Guile"
    ac_snd_have_extension_language=yes

    OLD_LIBS="$LIBS"
    LIBS="$GUILE_LIBS $LIBS"
    OLD_CFLAGS="$CFLAGS"
    CFLAGS="$GUILE_CFLAGS $CFLAGS"
    GNAME="guile"

# special Mac OSX stuff -- need to protect against multiple -lguile's, add -all_load, and add -lguile-ltdl
    case "$host" in
      *-apple-*)
 	CFLAGS="$CFLAGS -all_load"
	GNAME="m"
	;;
      *-*-solaris*) 
# this is a bug in the Sun guile-config I think
        if test "`${GUILE_CONFIG_path}${GUILE_name} -c '(display (string>=? (version) "1.6"))'`" = "#t"; then
	  GUILE_LIBS="$GUILE_LIBS -lltdl"
	fi
	;;
    esac

    AC_CHECK_LIB($GNAME, scm_set_smob_apply,     [AC_DEFINE(HAVE_APPLICABLE_SMOB)])
    AC_CHECK_LIB($GNAME, scm_remember_upto_here, [AC_DEFINE(HAVE_SCM_REMEMBER_UPTO_HERE)])
    AC_CHECK_LIB($GNAME, scm_make_real,          [AC_DEFINE(HAVE_SCM_MAKE_REAL)])
    AC_CHECK_LIB($GNAME, scm_object_to_string,   [AC_DEFINE(HAVE_SCM_OBJECT_TO_STRING)])
    AC_CHECK_LIB($GNAME, scm_num2long_long,      [AC_DEFINE(HAVE_SCM_NUM2LONG_LONG)])
    AC_CHECK_LIB($GNAME, scm_num2int,            [AC_DEFINE(HAVE_SCM_NUM2INT)])
    AC_CHECK_LIB($GNAME, scm_c_make_vector,      [AC_DEFINE(HAVE_SCM_C_MAKE_VECTOR)])
    AC_CHECK_LIB($GNAME, scm_c_define,           [AC_DEFINE(HAVE_SCM_C_DEFINE)])
    AC_CHECK_LIB($GNAME, scm_c_define_gsubr,     [AC_DEFINE(HAVE_SCM_C_DEFINE_GSUBR)])
    AC_CHECK_LIB($GNAME, scm_c_eval_string,      [AC_DEFINE(HAVE_SCM_C_EVAL_STRING)])
    AC_CHECK_LIB($GNAME, scm_list_n,             [AC_DEFINE(HAVE_SCM_LIST_N)])
    AC_CHECK_LIB($GNAME, scm_str2symbol,         [AC_DEFINE(HAVE_SCM_STR2SYMBOL)])
    AC_CHECK_LIB($GNAME, scm_defined_p,          [AC_DEFINE(HAVE_SCM_DEFINED_P)])
    AC_CHECK_LIB($GNAME, scm_mem2string,         [AC_DEFINE(HAVE_SCM_MEM2STRING)])
    AC_CHECK_LIB($GNAME, scm_continuation_p,     [AC_DEFINE(HAVE_SCM_CONTINUATION_P)])
    AC_CHECK_LIB($GNAME, scm_make_ratio,         [AC_DEFINE(HAVE_SCM_MAKE_RATIO)])
    AC_CHECK_LIB($GNAME, scm_make_complex,       [AC_DEFINE(HAVE_SCM_MAKE_COMPLEX)])
    AC_CHECK_LIB($GNAME, scm_to_signed_integer,  [AC_DEFINE(HAVE_SCM_TO_SIGNED_INTEGER)])
    AC_CHECK_LIB($GNAME, scm_c_make_rectangular, [AC_DEFINE(HAVE_SCM_C_MAKE_RECTANGULAR)])
    AC_CHECK_LIB($GNAME, scm_car,                [AC_DEFINE(HAVE_SCM_CAR)])
    AC_CHECK_LIB($GNAME, scm_from_locale_keyword,[AC_DEFINE(HAVE_SCM_FROM_LOCALE_KEYWORD)])
    AC_CHECK_LIB($GNAME, scm_is_vector,          [AC_DEFINE(HAVE_SCM_IS_VECTOR)])
    AC_CHECK_LIB($GNAME, scm_is_simple_vector,   [AC_DEFINE(HAVE_SCM_IS_SIMPLE_VECTOR)])
    AC_CHECK_LIB($GNAME, scm_c_primitive_load,   [AC_DEFINE(HAVE_SCM_C_PRIMITIVE_LOAD)])
    AC_CHECK_LIB($GNAME, scm_c_define_module,    [if test "$with_modules" = yes; then
						    AC_DEFINE(WITH_MODULES)
						  fi])
    AC_CHECK_TYPE(scm_t_catch_body,              [AC_DEFINE(HAVE_SCM_T_CATCH_BODY)], , [#include <libguile.h>]) # not gh.h here -> g++ dies
    AC_CHECK_TYPE(scm_t_guard,                   [AC_DEFINE(HAVE_GUILE_DYNAMIC_WIND)], , [#include <libguile.h>])

    AC_MSG_CHECKING([for call/cc])
    if test "`${GUILE_CONFIG_path}${GUILE_name} -c '(display (defined? (quote call/cc)))'`" = "#t"; then
      AC_DEFINE(HAVE_GUILE_CALL_CC)
      AC_MSG_RESULT(yes)
    else
      AC_MSG_RESULT(no)
    fi

    LIBS="$OLD_LIBS"
    CFLAGS="$OLD_CFLAGS"

    if test "`${GUILE_CONFIG_path}${GUILE_name} -c '(display (string<=? (version) "1.3.4"))'`" = "#t"; then
      echo found old out-of-date Guile library
    fi
    if test "`${GUILE_CONFIG_path}${GUILE_name} -c '(display (string>=? (version) "1.5"))'`" = "#t"; then
      AC_DEFINE(WITH_RUN)
    fi
    if test "$with_snd_as_pd_external" = yes ; then
      GUILE_LIBS=["`${GUILE_CONFIG_path}${GUILE_name} -c '(display (string-append (cdr (assoc (quote libdir) %guile-build-info)) "/libguile.a " (cdr (assoc (quote LIBS) %guile-build-info))))'`"]
    fi

    AC_SUBST(GUILE_LIBS)
  fi
else
  AC_DEFINE(HAVE_GUILE,0)
  AC_DEFINE(HAVE_EXTENSION_LANGUAGE,0)
fi
fi
fi


#--------------------------------------------------------------------------------
# Audio library
#--------------------------------------------------------------------------------

AUDIO_LIB=""
LDSO_FLAGS=""
SO_FLAGS=""
SO_LD="ld"

JACK_LIBS=""
JACK_FLAGS=""

# we need the sndlib.h equivalents to try to find the native sound support (see config.guess)
# this only matters for those cases where we've implemented the audio code in audio.c
# test for ALSA courtesy of Paul Davis
# test for ESD courtesy of Nick Bailey
# test for BSD courtesy of Steven Schultz
# test for Jack courtesy of Kjetil S. Matheussen

if test "$with_esd" = yes ; then
  AC_CHECK_LIB(esd,main,[
    AC_CHECK_HEADER(esd.h,
      [
	AC_DEFINE(MUS_ESD)
	esd_version="`esd-config --version`"
        AC_DEFINE_UNQUOTED(MUS_ESD_VERSION,"${esd_version}")
        audiofile_version="`audiofile-config --version`"
        AC_DEFINE_UNQUOTED(MUS_AUDIOFILE_VERSION,"${audiofile_version}")
        AUDIO_LIB="`esd-config --libs`"
#       ESD_CFLAGS="`esd-config --cflags`"
	AC_SUBST(AUDIO_LIB)
        if test "$with_alsa" = yes || test "$with_static_alsa" = yes ; then
	  AC_MSG_WARN([esd takes precedence over alsa -- the two are not compatible])
        fi
        AUDIO_SYSTEM=ESD
      ],
      [AC_MSG_WARN(can't find the ESD header files)])],
    [AC_MSG_WARN(can't find the ESD library)])
fi

if test "$AUDIO_SYSTEM" != None ; then
  AC_MSG_RESULT(Using the $AUDIO_SYSTEM audio system)
else

if test "$with_alsa" = yes && test "$with_oss" = yes ; then
  with_alsa=no
  AC_MSG_WARN([You asked for both ALSA and OSS -- OSS will be used])
fi

if test "$with_jack" = yes ; then 
            AC_CHECK_LIB(jack,main,[ 
	      AC_CHECK_HEADER(jack/jack.h,
                  ,
   	          [with_jack=no
                   AC_MSG_WARN(can't find the JACK header files)])],
              [with_jack=no
	       AC_MSG_WARN(can't find the JACK library)])
            AC_CHECK_LIB(samplerate,main,[ 
	      AC_CHECK_HEADER(samplerate.h,  ,
   	          [with_jack=no
                   AC_MSG_WARN(can't find the samplerate header files. JACK will not be used.)])],
              [with_jack=no
	       AC_MSG_WARN(can't find the samplerate library. JACK will not be used.)])
            if test $with_jack = yes ; then
               AUDIO_SYSTEM=JACK
	       AC_DEFINE(MUS_JACK)
	       if test x$PKG_CONFIG != xno ; then
                   jack_version="`$PKG_CONFIG jack --modversion`"
                   AC_DEFINE_UNQUOTED(MUS_JACK_VERSION,"${jack_version}")
          	   JACK_LIBS="`$PKG_CONFIG jack --libs`"
          	   JACK_FLAGS="`$PKG_CONFIG jack --cflags`"
               fi
            fi
fi


case "$host" in
    *-*-linux*) 
	AC_DEFINE(MUS_LINUX)
        LDSO_FLAGS="-shared"
        LIBS="$LIBS -lm"
	if test "$GCC" = yes ; then
	  SO_FLAGS="-fPIC $SO_FLAGS"
        fi

        AUDIO_SYSTEM=OSS

	if test "$with_alsa" = yes || test "$with_static_alsa" = yes ; then
          if test "$ac_cv_header_alsa_asoundlib_h" = yes ; then
            AC_CHECK_LIB(asound, main,
               AUDIO_SYSTEM=ALSA,
               [AC_MSG_WARN(can't find the ALSA library)])
          else
            AC_MSG_WARN(can't find the ALSA header files)
          fi
	fi

	if test "$with_jack" = yes ; then 
	       if test "$with_alsa" != yes ; then
                 AUDIO_SYSTEM=JACK
               fi
	fi

	case $AUDIO_SYSTEM in
	    ALSA)
                # if alsa version < 0.9 complain and use OSS
		alsa_ok=yes
		AC_MSG_CHECKING([for ALSA 0.9 or later])
		AC_COMPILE_IFELSE(
                  [AC_LANG_PROGRAM([#include <alsa/asoundlib.h>],
			           [
			  	    #if ((SND_LIB_MAJOR == 0) && (SND_LIB_MINOR < 9))
				      #error too old
				    #endif
				   ])],
		  alsa_ok=yes,
	          alsa_ok=no)
		AC_MSG_RESULT($alsa_ok)
		if test "$alsa_ok" = yes ; then
	    	    AC_DEFINE(HAVE_ALSA)
		    if test "$with_static_alsa" = yes ; then
                      AUDIO_LIB="/usr/lib/libasound.a"
		    else
	    	      AUDIO_LIB="-lasound"
		    fi
	    	    AC_SUBST(AUDIO_LIB)

                    # snd_config_get_id argnum changed in Nov-01
                    snd_config_get_id_args=1
                    AC_MSG_CHECKING([alsa snd_config_get_id args])
                    AC_COMPILE_IFELSE(
		      [AC_LANG_PROGRAM([#include <alsa/asoundlib.h>],
	             		       [snd_config_t *c; 
					const char *id; 
		      			snd_config_get_id(c, &id)])],
       		      snd_config_get_id_args=2)
                    AC_MSG_RESULT($snd_config_get_id_args)
                    AC_DEFINE_UNQUOTED(SND_CONFIG_GET_ID_ARGS,$snd_config_get_id_args)

                    AC_MSG_CHECKING([for newer alsa calling sequences])
                    AC_COMPILE_IFELSE(
		      [AC_LANG_PROGRAM([#include <alsa/asoundlib.h>],
	             		       [snd_pcm_hw_params_t *hw_params;
			  	        unsigned int tmp;
					int dir;
				        snd_pcm_hw_params_get_rate_max(hw_params, &tmp, &dir)
				       ])],
		       [AC_DEFINE(HAVE_NEW_ALSA)
                        AC_MSG_RESULT(yes)],
		       [AC_MSG_RESULT(no)])

		    if test "$with_jack" = yes ; then 
		      if test "$with_static_alsa" = yes ; then
			 AUDIO_LIB="/usr/lib/libasound.a -lsamplerate"
		      else
			 AUDIO_LIB="-lasound -lsamplerate"
	              fi
		      AC_DEFINE(HAVE_JACK)
		    fi
 		else
         	    AUDIO_SYSTEM=OSS
 		    AC_DEFINE(HAVE_OSS)
 		fi
 		;;
	    JACK)
		AC_DEFINE(HAVE_JACK)
		AC_DEFINE(HAVE_OSS)
		AUDIO_LIB="-lsamplerate"
		;;
	    OSS)
		AC_DEFINE(HAVE_OSS)
		;;
	esac
	;;
    *-*-sunos4*) 
        AC_DEFINE(MUS_SUN)
	LIBS="$LIBS -lm"
	AUDIO_SYSTEM=Sun
        ;;
    *-*-solaris*) 
	AC_DEFINE(MUS_SUN)
	LIBS="$LIBS -lm -ldl"
# odd... this causes an error in the sndlib configure (can't find libdl) but is needed here?
	AUDIO_SYSTEM=Sun
	LDSO_FLAGS="-G"
# if __SUNPRO_C we could add -xO3
        ;;
    *-*-hpux*) 
        AC_DEFINE(MUS_HPUX)
	AUDIO_SYSTEM=Hpux
	LDSO_FLAGS="+z -Ae +DA1.1"
	if test "$GCC" = yes ; then
	  SO_FLAGS="-fPIC $SO_FLAGS"
        fi
        ;;
    *-sgi*) 
        AC_DEFINE(MUS_SGI)
	AUDIO_SYSTEM=SGI
	AUDIO_LIB="-laudio"
	AC_SUBST(AUDIO_LIB)
        if test $GUILE_CONFIG_works = no; then
 	  LIBS="$LIBS -lc -lm -lmd"
        else
	  LIBS="$LIBS -lmd"
        fi
        LDSO_FLAGS="-shared -all"
	SO_LD=":"
	if test $LOCAL_LANGUAGE = "None"; then
	  LIBS="$LIBS -lm"
	fi
        ;;
    alpha*) 
        AC_DEFINE(MUS_ALPHA)
        ;;
    *-*-bsdi*) 
	AC_DEFINE(HAVE_OSS)
	LIBS="$LIBS -lm"
	AUDIO_SYSTEM=OSS
	if test "$GCC" = yes ; then
	  SO_FLAGS="-fPIC $SO_FLAGS"
        fi
        ;;
    *-*-freebsd*)
	AC_DEFINE(HAVE_OSS)
	LIBS="$LIBS -lm"
	AUDIO_SYSTEM=OSS
	if test "$GCC" = yes ; then
	  SO_LD="gcc"
	  SO_FLAGS="-fPIC $SO_FLAGS"
          LDSO_FLAGS="-shared"
        fi
	;;
    *-*-openbsd*) 
        AC_DEFINE(MUS_OPENBSD)
	AUDIO_SYSTEM=OpenBSD
	if test "$GCC" = yes ; then
	  SO_FLAGS="-fPIC $SO_FLAGS"
        fi
        ;;
    *-*-netbsd*) 
        AC_DEFINE(MUS_NETBSD)
	AUDIO_SYSTEM=NetBSD
	if test "$GCC" = yes ; then
	  SO_LD="gcc"
	  SO_FLAGS="-fPIC $SO_FLAGS"
          LDSO_FLAGS="-shared"
        fi
        ;;
    *-*-cygwin*)
	if test "$with_jack" != yes ; then 
	    AC_DEFINE(MUS_WINDOZE)
	    AUDIO_SYSTEM=Windoze
	fi
	;;
    *-apple-*)
	if test "$with_jack" != yes ; then 
	    AC_DEFINE(MUS_MAC_OSX)
	    AUDIO_SYSTEM=MacOSX
	    AUDIO_LIB="-framework CoreAudio -framework CoreFoundation -framework CoreMIDI"
	    AC_SUBST(AUDIO_LIB)
	    AC_MSG_CHECKING([for kAudioDevicePropertyDeviceManufacturer])
	    AC_COMPILE_IFELSE(
              [AC_LANG_PROGRAM([#include <CoreServices/CoreServices.h>
                                #include <CoreAudio/CoreAudio.h>],
			       [AudioDeviceID deviceID; 
			        UInt32 trans_size = 0, trans;
      			        trans_size = sizeof(UInt32);
     			        AudioDeviceGetProperty(deviceID, 0, true, kAudioDevicePropertyTransportType, &trans_size, &trans)
			       ])],
	      [AC_DEFINE(HAVE_KAUDIODEVICEPROPERTYTRANSPORTTYPE)
	       AC_MSG_RESULT(yes)],
	      [AC_MSG_RESULT(no)])
	    AC_MSG_CHECKING([for kLinearPCMFormatFlagIsNonInterleaved])
	    AC_COMPILE_IFELSE(
              [AC_LANG_PROGRAM([#include <CoreServices/CoreServices.h>
                                #include <CoreAudio/CoreAudio.h>],
			       [int i; i = kLinearPCMFormatFlagIsNonInterleaved])],
	      [AC_DEFINE(HAVE_KLINEARPCMFORMATFLAGISNONINTERLEAVED)
	       AC_MSG_RESULT(yes)],
	      [AC_MSG_RESULT(no)])
	fi
	;;
esac
AC_MSG_CHECKING([for audio system])
AC_MSG_RESULT($AUDIO_SYSTEM)
fi

AC_SUBST(JACK_LIBS)
AC_SUBST(JACK_FLAGS)

AC_SUBST(LDSO_FLAGS)
AC_SUBST(SO_FLAGS)
AC_SUBST(SO_LD)


#--------------------------------------------------------------------------------
# OGG, Flac, Speex, Mpeg, Midi, Shorten, TTA, Wavpack
# --------------------------------------------------------------------------------

AC_PATH_PROG(PATH_OGGDEC, oggdec, no) # OGG read
AC_PATH_PROG(PATH_OGGENC, oggenc, no) # OGG write

if test "$PATH_OGGDEC" != "no" ; then
  if test "$PATH_OGGENC" != "no" ; then
    AC_DEFINE(HAVE_OGG)
    AC_DEFINE_UNQUOTED(PATH_OGGDEC, "${PATH_OGGDEC}")
    AC_DEFINE_UNQUOTED(PATH_OGGENC, "${PATH_OGGENC}")
  fi
fi


AC_PATH_PROG(PATH_MPG123, mpg123, no) # MPEG read/write?

if test "$PATH_MPG123" != "no" ; then
  AC_DEFINE(HAVE_MPEG)
  AC_DEFINE_UNQUOTED(PATH_MPG123, "${PATH_MPG123}")
fi


AC_PATH_PROG(PATH_MPG321, mpg321, no) # MPEG read/write?

if test "$PATH_MPG321" != "no" ; then
  AC_DEFINE(HAVE_MPEG)
  AC_DEFINE_UNQUOTED(PATH_MPG321, "${PATH_MPG321}")
fi


AC_PATH_PROG(PATH_SPEEXDEC, speexdec, no) # Speex read
AC_PATH_PROG(PATH_SPEEXENC, speexenc, no) # Speex write

if test "$PATH_SPEEXDEC" != "no" ; then
  if test "$PATH_SPEEXENC" != "no" ; then
    AC_DEFINE(HAVE_SPEEX)
    AC_DEFINE_UNQUOTED(PATH_SPEEXDEC, "${PATH_SPEEXDEC}")
    AC_DEFINE_UNQUOTED(PATH_SPEEXENC, "${PATH_SPEEXENC}")
  fi
fi


AC_PATH_PROG(PATH_FLAC, flac, no) # Flac read/write

if test "$PATH_FLAC" != "no" ; then
  AC_DEFINE(HAVE_FLAC)
  AC_DEFINE_UNQUOTED(PATH_FLAC, "${PATH_FLAC}")
fi


AC_PATH_PROG(PATH_TIMIDITY, timidity, no) # Timidity for .mid -> .wav

if test "$PATH_TIMIDITY" != "no" ; then
  AC_DEFINE(HAVE_TIMIDITY)
  AC_DEFINE_UNQUOTED(PATH_TIMIDITY, "${PATH_TIMIDITY}")
fi


AC_PATH_PROG(PATH_SHORTEN, shorten, no)

if test "$PATH_SHORTEN" != "no" ; then
  AC_DEFINE(HAVE_SHORTEN)
  AC_DEFINE_UNQUOTED(PATH_SHORTEN, "${PATH_SHORTEN}")
fi


AC_PATH_PROG(PATH_TTA, ttaenc, no)

if test "$PATH_TTA" != "no" ; then
  AC_DEFINE(HAVE_TTA)
  AC_DEFINE_UNQUOTED(PATH_TTA, "${PATH_TTA}")
fi


AC_PATH_PROG(PATH_WAVPACK, wavpack, no)
AC_PATH_PROG(PATH_WVUNPACK, wvunpack, no)

if test "$PATH_WAVPACK" != "no" ; then
  if test "$PATH_WVUNPACK" != "no" ; then
    AC_DEFINE(HAVE_WAVPACK)
    AC_DEFINE_UNQUOTED(PATH_WAVPACK, "${PATH_WAVPACK}")
    AC_DEFINE_UNQUOTED(PATH_WVUNPACK, "${PATH_WVUNPACK}")
  fi
fi
  
# other decoders that I've looked at aren't acceptable in this context because they put various
#   special restrictions on use, or hide source code, or are just nuts.




#--------------------------------------------------------------------------------
# statvfs
#--------------------------------------------------------------------------------

statvfs_ok=0
if test "$ac_cv_func_statvfs" = yes ; then
  if test "$ac_cv_header_sys_statvfs_h" = yes ; then
    AC_MSG_CHECKING([for statvfs])
    AC_COMPILE_IFELSE(
      [AC_LANG_PROGRAM([#include <sys/statvfs.h>],
                       [struct statvfs buf; statvfs("test.snd", &buf)])], 
      AC_DEFINE(USE_STATVFS)
      statvfs_ok=1)
    if test $statvfs_ok = 1 ; then
      AC_MSG_RESULT(yes)
    else
      AC_MSG_RESULT(no)
    fi
  fi
fi

statfs_ok=0
if test $statvfs_ok = 0 ; then
  if test "$ac_cv_func_statfs" = yes ; then
    if test "$ac_cv_header_sys_param_h" = yes ; then
      if test "$ac_cv_header_sys_mount_h" = yes ; then
        AC_MSG_CHECKING([for statfs])
        AC_COMPILE_IFELSE(
         [AC_LANG_PROGRAM([#include <sys/param.h>
                           #include <sys/mount.h>
                          ],
                          [struct statfs buf;
                           int size = 0;
  			   statfs("test.snd", &buf);
                           if (buf.f_bsize = 1024)
			     size = (int)(buf.f_bavail)
			  ])],
          AC_DEFINE(USE_STATFS)
          statfs_ok=1)
        if test $statfs_ok = 1 ; then
          AC_MSG_RESULT(yes)
        else
          AC_MSG_RESULT(no)
        fi
      fi
    fi
  fi
fi

AC_ARG_ENABLE(snd-debug,
  [  --enable-snd-debug      include internal Snd debugging functions],
  if test "$enable_snd_debug" = yes ; then
    AC_DEFINE(MUS_DEBUGGING)
    RANDOM_FEATURES="$RANDOM_FEATURES debugging"
    VL_PROG_CC_WARNINGS()
  fi)


#--------------------------------------------------------------------------------
# sigsetjmp special case
#--------------------------------------------------------------------------------

# look for sigsetjmp for segfault trap
if test "$enable_snd_debug" != yes ; then
  trap_segfault=no
  AC_MSG_CHECKING([for sigsetjmp])
  AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <setjmp.h>],
                                  [sigjmp_buf hi; sigsetjmp(hi,1)])],
                 trap_segfault=yes)
  if test $trap_segfault = yes; then
    AC_DEFINE(MUS_TRAP_SEGFAULT,1)
  fi
  AC_MSG_RESULT($trap_segfault)
fi


#--------------------------------------------------------------------------------
# sndlib
#--------------------------------------------------------------------------------
#
# look for sndlib-config, check mus_sample_bits against current choice [with-float-sample, with-sample-width]
#   also language (Guile, Ruby) needs to match (what about versions!) and audio choice
# check against needed version (17.2 for now -- need c_vct_copy in vct.c)
# set SNDLIB_FILES and SNDLIB_LIB

MIDI_FILES=""
if test "$with_midi" = yes; then
  MIDI_FILES="MIDI_O_FILES"
  AC_DEFINE(WITH_MIDI)
fi
AC_SUBST(MIDI_FILES)

SNDLIB_FILES="SNDLIB_O_FILES"
if test "$enable_snd_debug" != yes ; then
  SNDLIB_LIB=""
fi
SNDLIB_PREFIX=""
AC_ARG_WITH(shared-sndlib,[  --with-shared-sndlib	  try to load libsndlib.so])
if test "$with_shared_sndlib" = yes; then
  if test "$SNDLIB_CONFIG_path" != "" ; then
    if ! test -x "${SNDLIB_CONFIG_path}sndlib-config" ; then
      # try adding the "/" to the path    
      SNDLIB_CONFIG_path="${SNDLIB_CONFIG_path}/"
    fi
  fi
  if ! test -x "${SNDLIB_CONFIG_path}sndlib-config" ; then
    AC_PATH_PROG(SNDLIB_CONFIG, sndlib-config, no)
  else
    SNDLIB_CONFIG="${SNDLIB_CONFIG_path}sndlib-config"
  fi
  if test "$SNDLIB_CONFIG" = "no" ; then
    AC_MSG_RESULT(no)
  else
    SNDLIB_PREFIX=`$SNDLIB_CONFIG --prefix`
    OLD_CFLAGS="$CFLAGS"
    CFLAGS="$CFLAGS -L$SNDLIB_PREFIX/lib"
    AC_CHECK_LIB(sndlib, c_vct_copy,
      [
	SNDLIB_AUDIO_CHOICE=`$SNDLIB_CONFIG --audio`
        if test "$SNDLIB_AUDIO_CHOICE" != "$AUDIO_SYSTEM" ; then
	  AC_MSG_WARN([libsndlib.so audio choice is $SNDLIB_AUDIO_CHOICE, but the current choice is $AUDIO_SYSTEM])
        fi
	SNDLIB_BITS=`$SNDLIB_CONFIG --bits`
	if test "$SNDLIB_BITS" = "$LOCAL_SNDLIB_BITS" ; then
          SNDLIB_LANGUAGE=`$SNDLIB_CONFIG --language`
          if test "$SNDLIB_LANGUAGE" = $LOCAL_LANGUAGE ; then
	    SNDLIB_FILES="NO_FILES"
            SNDLIB_LIB="-L$SNDLIB_PREFIX/lib -lsndlib"
            OPTIONAL_LIBRARIES="$OPTIONAL_LIBRARIES sndlib"
            AC_DEFINE(WITH_SHARED_SNDLIB)
	    SNDLIB_WITH_MODULES=`$SNDLIB_CONFIG --modules`
            if test "$SNDLIB_WITH_MODULES" = "yes" ; then
	      if test "$with_modules" != yes; then
		AC_DEFINE(WITH_MODULES)
              fi
            else
	      if test "$with_modules" = yes; then
		AC_DEFINE(WITH_MODULES, 0)
              fi
            fi
          else
	    AC_MSG_WARN([libsndlib.so was built with $SNDLIB_LANGUAGE, but current choice is $LOCAL_LANGUAGE])
          fi
        else
	  AC_MSG_WARN([libsndlib.so is not compatible with current Snd mus_sample_t choice])
   	fi
       ])
    CFLAGS="$OLD_CFLAGS"
    fi
fi
AC_SUBST(SNDLIB_FILES)
AC_SUBST(SNDLIB_LIB)


#--------------------------------------------------------------------------------
# gettext
#--------------------------------------------------------------------------------
#
# this goddamn macro is much more trouble than it is worth!

if test "$enable_nls" != no ; then
  AM_GNU_GETTEXT([external])
  OPTIONAL_LIBRARIES="$OPTIONAL_LIBRARIES gettext"

  AC_CHECK_HEADER(nl_types.h,
    [AC_MSG_CHECKING([whether nl_types.h defines MALLOC])
     AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM([#include <langinfo.h>],
		        [char i; i = MALLOC; return(0)])],
       [AC_DEFINE(NL_TYPES_H_DEFINES_MALLOC)
        AC_MSG_RESULT(yes)
       ],
       [AC_MSG_RESULT(no)])])
else
# desperation!
cp makefile.in makefile.gettext
cp makefile.no-gettext makefile.in
fi


#--------------------------------------------------------------------------------
# debugging stuff
#--------------------------------------------------------------------------------

AC_ARG_ENABLE(deprecated,
  [  --disable-deprecated	  do not include any deprecated stuff from gtk, guile, motif, clm, or sndlib],
  if test "$enable_deprecated" = no ; then
    CFLAGS="-DGTK_DISABLE_DEPRECATED -DG_DISABLE_DEPRECATED -DGDK_DISABLE_DEPRECATED -DPANGO_DISABLE_DEPRECATED $CFLAGS"
    AC_DEFINE(XM_DISABLE_DEPRECATED)
    AC_DEFINE(CLM_DISABLE_DEPRECATED)
    AC_DEFINE(SNDLIB_DISABLE_DEPRECATED)
    AC_DEFINE(XEN_DISABLE_DEPRECATED)
  fi)

if test "$enable_snd_debug" = yes ; then
  CFLAGS="-I. $CFLAGS"
  if test "$GCC" = yes ; then
    if test "$LOCAL_LANGUAGE" = "Ruby" ; then
      if test "$CC" = "g++" ; then
        CFLAGS="$CFLAGS -O -g3 -Wcast-align -Wpointer-arith -Wimplicit -Wreturn-type -Wunused-label -Wunused-variable -Wunused-value -Wcomment -Wformat -Wunused-function -Wuninitialized -Wparentheses -Wall"
      else
        CFLAGS="$CFLAGS -O -g3 -Wcast-align -Wpointer-arith -Wimplicit -Wreturn-type -Wunused-label -Wunused-variable -Wunused-value -Wcomment -Wformat -Wunused-function -Wuninitialized -Wparentheses -Wall"
      fi
    else
      if test "$CC" = "g++" ; then
        CFLAGS="$CFLAGS -O -g3 -Wredundant-decls -Wcast-align -Wpointer-arith -Wimplicit -Wreturn-type -Wunused-label -Wunused-variable -Wunused-value -Wcomment -Wformat -Wunused-function -Wuninitialized -Wparentheses -Wall"
      else
        CFLAGS="$CFLAGS -O -g3 -Wredundant-decls -Wcast-align -Wmissing-prototypes -Wpointer-arith -Wimplicit -Wreturn-type -Wunused-label -Wunused-variable -Wunused-value -Wcomment -Wformat -Wunused-function -Wuninitialized -Wparentheses -Wall"
      fi
    fi
  fi
else
  CFLAGS="-O2 -I. $CFLAGS"
fi

# -ffast-math appears to make exp about twice as fast, but slows down filtering by the same amount -- kinda strange
#   timing tests using -O3 -ffast-math indicate an overall speedup of maybe 4-5%



#--------------------------------------------------------------------------------
# export-dynamic
#--------------------------------------------------------------------------------

ORIGINAL_LDFLAGS="$LDFLAGS"

if test "$with_snd_as_widget" != yes ; then
  if test "$ac_cv_header_dlfcn_h" = yes ; then
    if test "$with_gnu_ld" = yes ; then
      LDFLAGS="$LDFLAGS -Wl,-export-dynamic"
      # I think this really should be CFLAGS since it's assuming it's passed to gcc, not ld, but
      #   that ends up generating endless dumb warnings in gcc
    fi
  fi
fi

if test "$ac_snd_have_extension_language" = no ; then
  if test "$ac_snd_have_gui" = no ; then
    if test "$ac_cv_header_dlfcn_h" = yes ; then
      LDFLAGS="$LDFLAGS -ldl"
    fi
    AC_MSG_WARN([Snd needs either an extension language (Guile or Ruby), or a graphics toolkit (Gtk or Motif), or preferably both.  All four packages are available free, normally prepackaged and ready-to-use.  As currently configured, this version of Snd is useless.])
    if test "$with_snd_as_pd_external" = yes ; then	
       AC_MSG_ERROR(Guile is needed when making snd as a pd external.)
    fi
  fi
fi

AC_SUBST(CFLAGS)
AC_SUBST(ORIGINAL_LDFLAGS)
AC_SUBST(LDFLAGS)
AC_SUBST(MAKE_TARGET)
AC_OUTPUT

# borrowed from gnucash

AC_MSG_RESULT([
  Options selected
  -------------------------
  Snd version .......... : $VERSION
  CFLAGS ............... : $CFLAGS
  LDFLAGS .............. :$LDFLAGS
  prefix.................: ${prefix}
  extension language.....: $LOCAL_LANGUAGE
  audio system...........: $AUDIO_SYSTEM
  graphics toolkit.......: $GRAPHICS_TOOLKIT
  optional libraries.....:$OPTIONAL_LIBRARIES
  random features........:$RANDOM_FEATURES
  environs...............: $host $CC
])
