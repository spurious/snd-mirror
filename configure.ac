dnl Configuration script for Snd
dnl Process this file with autoconf 2.57 to produce a configure script.

AC_INIT(snd, 6.13, bil@ccrma.stanford.edu, ftp://ccrma-ftp.stanford.edu/pub/Lisp/snd-6.tar.gz)
AC_CONFIG_SRCDIR(snd.c)
AC_CANONICAL_HOST
AC_CONFIG_HEADER(config.h)
AC_CONFIG_FILES(makefile po/Makefile.in )
AC_PROG_CC
AC_HEADER_STDC # this needs to precede AC_CHECK_HEADER! (autoconf docs don't mention this problem)
AC_PROG_INSTALL

audio_system="unknown"
MAKE_TARGET="snd"

PACKAGE=snd
VERSION=6.13
AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE")
AC_DEFINE_UNQUOTED(VERSION, "$VERSION")
AC_SUBST(PACKAGE)
AC_SUBST(VERSION)

#--------------------------------------------------------------------------------
# configuration options
#   --with-motif          use Motif (the default)
#   --with-static-motif   use Motif statically loaded (for RPM generation)
#   --with-gtk            use Gtk+	
#   --with-alsa           use ALSA if possible
#   --with-static-alsa    use ALSA statically loaded (for RPM generation)
#   --with-snd-as-widget  make Snd a loadable widget, not a standalone program
#   --with-doubles        use doubles throughout (default is floats)
#   --with-float-samples  represent samples internally as floats 
#   --enable-snd-debug    include Snd internal debugging stuff
#   --with-ladspa         include LADSPA plugin support (Linux)
#   --with-sample-width=N use N bits of samples (default = 24)
#   --with-esd            use Enlightened Sound Daemon
#   --with-no-gui         make Snd without any graphics support
#   --with-motif-prefix   set location of Motif (untested as yet)
#   --with-static-gsl     try to statically load GSL
#   --with-guile          try to use Guile as the extension language (default=yes)
#   --with-ruby           try to use Ruby as the extension language
#   --with-ruby-prefix    set location of Ruby
#   --with-static-xm      include xm module
#   --with-temp-dir       directory to use for temp files
#   --with-save-dir       directory to use for saved-state files
#   --with-gl             include OpenGL support (default=no)
#   --with-just-gl        include OpenGL support but omit the Guile bindings (gl.o) (default=no)
#   --with-modules        accommodate sndlib that was built with modules
#   --with-editres 	  include EditRes in xm
#   --with-shared-sndlib  load sndlib.so if possible
#   --with-x11            if gtk, this also includes X funcs from xm.c
#   --without-gsl         omit GSL even if it exists
#   --without-fftw        omit FFTW even if it exists
#   --without-xpm         omit XPM even if it exists (for testing)
#   --without-xp          omit libXp even if it exists (for testing)
#   GUILE_CONFIG_path     where to look for guile-config
#   SNDLIB_CONFIG_path    where to look for sndlib-config
#
# SOMEDAY: --with-static-libraries --with-static-guile

#--------------------------------------------------------------------------------

AC_ARG_WITH(esd,        [  --with-esd		  use ESD])
AC_ARG_WITH(alsa,       [  --with-alsa		  use ALSA])
AC_ARG_WITH(static-alsa,[  --with-static-alsa	  use ALSA statically loaded])
AC_ARG_WITH(gtk,        [  --with-gtk		  use Gtk+ to build Snd])
AC_ARG_WITH(no-gui,     [  --with-no-gui  	  make Snd without any graphics support])
AC_ARG_WITH(doubles,    [  --with-doubles	  use doubles throughout])
AC_ARG_WITH(static-gsl, [  --with-static-gsl	  use libgsl.a to build Snd])
AC_ARG_WITH(static-xm,  [  --with-static-xm	  include the xm module])
AC_ARG_WITH(guile,      [  --with-guile		  use Guile])
AC_ARG_WITH(gl,         [  --with-gl		  include OpenGL support])
AC_ARG_WITH(just-gl,    [  --with-just-gl	  include OpenGL support, but omit the GL/Guile bindings])
AC_ARG_WITH(motif,	[  --with-motif		  use libXm to build Snd])
AC_ARG_WITH(static-motif,[  --with-static-motif	  use libXm.a to build Snd])
AC_ARG_WITH(modules,    [  --with-modules	  use if sndlib uses modules])
AC_ARG_WITH(editres,    [  --with-editres	  include editres in xm])
AC_ARG_WITH(ladspa,	[  --with-ladspa  	  include support for LADSPA plugins])
AC_ARG_WITH(x11,	[  --with-x11		  include X funcs from xm.c with xg.c])

# these are primarily for testing
AC_ARG_WITH(gsl,        [  --with-gsl		  use GSL, default=yes])
AC_ARG_WITH(fftw,       [  --with-fftw		  use fftw, default=yes])
AC_ARG_WITH(xpm,        [  --with-xpm		  use xpm, default=yes])
AC_ARG_WITH(xp,         [  --with-xp		  use xp, default=yes])

if test "$with_doubles" = yes; then
  AC_DEFINE(Float,double)
else
  AC_DEFINE(Float,float)
fi

LOCAL_SNDLIB_BITS="24"
AC_ARG_WITH(float-samples,
	[  --with-float-samples	  use floats as the internal sample respresentation],
  	if test "$with_float_samples" = yes ; then
	    AC_DEFINE(SNDLIB_USE_FLOATS)
            LOCAL_SNDLIB_BITS="0"
	fi)

AC_ARG_WITH(sample-width,
	[  --with-sample-width=N   use N bits of samples],
	AC_MSG_RESULT(Using $with_sample_width bit samples)
	AC_DEFINE_UNQUOTED(MUS_SAMPLE_BITS,$with_sample_width)
        LOCAL_SNDLIB_BITS=$with_sample_width
	)

if test "$with_motif" = yes ; then
  AC_DEFINE(USE_MOTIF)
fi

AC_ARG_WITH(motif-prefix,[  --with-motif-prefix=PFX where Motif is installed],
            motif_prefix="$withval", motif_prefix="")

if test "$with_static_motif" = yes ; then
  AC_DEFINE(USE_MOTIF)
  with_motif=yes
fi

AC_ARG_WITH(temp-dir,
	[  --with-temp-dir	  directory to use for temp files],
	    AC_DEFINE_UNQUOTED(DEFAULT_TEMP_DIR, "${withval}")
	)

AC_ARG_WITH(save-dir,
	[  --with-save-dir	  directory to use for saved-state files],
	    AC_DEFINE_UNQUOTED(DEFAULT_SAVE_DIR, "${withval}")
	)

RUBY="ruby"
AC_ARG_WITH(ruby-prefix,[  --with-ruby-prefix=PFX  where Ruby is installed],
            	ruby_prefix="$withval"
		RUBY="$ruby_prefix/bin/ruby",
		ruby_prefix="")

LOCAL_LANGUAGE="None"
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
              AC_DEFINE_UNQUOTED(RUBY_VERSION,"${RUBY_VERSION}")
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
    	      LIBS="$GUILE_LIBS"
              OLD_CFLAGS="$CFLAGS"
              CFLAGS="$GUILE_CFLAGS -lm"
              AC_CHECK_LIB(ruby, rb_num2ll, [AC_DEFINE(HAVE_RB_NUM2LL)])
              AC_CHECK_LIB(ruby, rb_gc_disable, [AC_DEFINE(HAVE_RB_GC_DISABLE)])
              LIBS="$OLD_LIBS"
              CFLAGS="$OLD_CFLAGS"
              LOCAL_LANGUAGE="Ruby"

            else 
              AC_MSG_WARN([can't find Ruby!])
  	      AC_DEFINE(HAVE_RUBY,0)
              AC_DEFINE(HAVE_EXTENSION_LANGUAGE,0)
            fi
	fi)


AC_ARG_WITH(snd-as-widget,
	[  --with-snd-as-widget	  make Snd a loadable widget, not a standalone program],
  	if test "$with_snd_as_widget" = yes ; then
	    AC_DEFINE(SND_AS_WIDGET)
	    MAKE_TARGET=widget
	fi)

AC_ARG_ENABLE(snd-debug,
  [  --enable-snd-debug      include internal Snd debugging functions],
  if test "$enable_snd_debug" = yes ; then
    AC_DEFINE(DEBUGGING)
    AC_DEFINE(DEBUG_MEMORY)
    CFLAGS="$CFLAGS -g3 -Wall"
  fi)



#--------------------------------------------------------------------------------
# standard libraries, header files, functions, OSS special cases
#--------------------------------------------------------------------------------

AC_CHECK_LIB(m,main)
AC_CHECK_LIB(c,main)
AC_CHECK_LIB(dl,main)
LIBS=""

AC_HEADER_DIRENT
AC_CHECK_HEADERS(fcntl.h limits.h unistd.h string.h sys/soundcard.h soundcard.h machine/soundcard.h gsl/gsl_version.h sys/mixer.h stdbool.h)
AC_CHECK_HEADERS(libc.h sys/vfs.h sys/statfs.h sys/statvfs.h sys/mount.h setjmp.h pwd.h locale.h sys/fpu.h dlfcn.h sys/param.h byteswap.h)
AC_CHECK_HEADER(/usr/local/lib/oss/include/sys/soundcard.h,[AC_DEFINE(USR_LOCAL_LIB_OSS)])
AC_CHECK_HEADER(/usr/lib/oss/include/sys/soundcard.h,[AC_DEFINE(USR_LIB_OSS)])
AC_CHECK_HEADER(/opt/oss/include/sys/soundcard.h,[AC_DEFINE(OPT_OSS)])
AC_CHECK_HEADER(/var/lib/oss/include/sys/soundcard.h,[AC_DEFINE(VAR_LIB_OSS)])
AC_CHECK_HEADER(sys/sam9407.h,[AC_DEFINE(HAVE_SAM_9407)])
AC_CHECK_HEADER(gnu/libc-version.h,[AC_DEFINE(HAVE_GNU_LIBC_VERSION_H)])
AC_CHECK_HEADER(alsa/asoundlib.h,[AC_DEFINE(HAVE_ALSA_ASOUNDLIB_H)])
AC_HEADER_TIME

AC_CHECK_DECLS(hypot,,,[#include <math.h>]) 
AC_CHECK_DECLS(isnan,,,[#include <math.h>])

AC_C_CONST
AC_TYPE_MODE_T
AC_TYPE_SIZE_T
AC_TYPE_PID_T
AC_STRUCT_TM
AC_C_BIGENDIAN
AC_SYS_LARGEFILE
AC_TYPE_OFF_T
AC_C_STRINGIZE
AM_LANGINFO_DECIMAL_POINT

AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(char)
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(int *)
AC_CHECK_SIZEOF(float)
AC_CHECK_SIZEOF(void *)
AC_CHECK_SIZEOF(off_t)

SAVELIBS=$LIBS
FFTW_LIBS=""
if test "$with_fftw" != no; then
LIBS="$LIBS -lfftw3 -lm"
AC_MSG_CHECKING([for fftw-3])
AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <fftw3.h>],
		[fftw_plan plan; fftw_execute(plan)])],
		[AC_MSG_RESULT(yes)
		 AC_DEFINE(HAVE_FFTW3)
		 FFTW_LIBS="-lfftw3"],
		[AC_MSG_RESULT(no)
                 LIBS=$SAVELIBS
                 LIBS="$LIBS -lrfftw -lfftw -lm"
                 AC_MSG_CHECKING([for fftw-2])
                 AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <rfftw.h>],
                                [rfftw_one(NULL, NULL, NULL)])],
		  		[AC_MSG_RESULT(yes)
		   		 AC_DEFINE(HAVE_FFTW)
		   		 FFTW_LIBS="-lrfftw -lfftw"],
		  		[AC_MSG_RESULT(no)])
		])
fi
AC_SUBST(FFTW_LIBS)
LIBS=$SAVELIBS

AC_TYPE_SIGNAL
AC_FUNC_STRFTIME
AC_CHECK_FUNCS(getcwd strerror readlink setlocale access opendir sleep signal statfs statvfs clock vsnprintf snprintf memmove strdup lstat fileno strcasecmp)
AC_FUNC_CLOSEDIR_VOID
AC_FUNC_VPRINTF

# having <complex.h> + a cacos declaration is not enough: C++ dies with a complaint about a "deprecated header"
if test "$with_gsl" != yes; then
  AC_MSG_CHECKING(for complex trig)
  AC_COMPILE_IFELSE(
	[AC_LANG_PROGRAM(
        	[#include <complex.h>],
		[ _Complex double val; 
                  val = ccosh(cacosh(1.5) / 100.0)
                ])],
        [
 	 AC_DEFINE(HAVE_COMPLEX_TRIG)
	 AC_MSG_RESULT(yes)
         with_gsl=no
    	],
	[AC_MSG_RESULT(no)])
fi

# -------- GSL --------
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
  if test "`$GSL_CONFIG --version`" = "0.6" || test "`$GSL_CONFIG --version`" = "0.7" || test "`$GSL_CONFIG --version`" = "0.5"; then
    AC_MSG_WARN([Snd needs GSL 0.8 or later])
  else
    if test "`$GSL_CONFIG --version`" = "0.8" && test "$with_static_gsl" = yes ; then
      GSL_LIBS="$GSL_LIBS $GSL_PREFIX/lib/libgslcblas.a"
    fi
    AC_DEFINE(HAVE_GSL)
    AC_DEFINE_UNQUOTED(GSL_VERSION,"${gsl_version}")
  fi
fi
fi
AC_SUBST(GSL_LIBS)
AC_SUBST(GSL_CFLAGS)



# AC_CHECK_STRUCT_FOR([#include <sys/statfs.h>],statfs,f_bsize)

if test "$with_ladspa" != no ; then
  AC_CHECK_HEADER(ladspa.h, [AC_DEFINE(HAVE_LADSPA)])
fi


#--------------------------------------------------------------------------------
# X/Motif/Gtk+
#--------------------------------------------------------------------------------

AC_PATH_XTRA

if test "$with_motif" = no && test "$with_gtk" = no ; then
  with_no_gui=yes
fi

if test "$no_x" = yes || test "$with_no_gui" = yes ; then

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

  if test "$with_static_motif" = yes ; then
    if test x$motif_prefix != x ; then
    AC_CHECK_FILE($motif_prefix/lib/libXm.a,[
      XLIBS="$motif_prefix/lib/libXm.a $X_LIBS $X_PRE_LIBS -lXt -lX11 $X_EXTRA_LIBS"
	],	
	[
      AC_MSG_WARN(can't find $motif_prefix/lib/libXm.a!)
      XLIBS="$X_LIBS $X_PRE_LIBS -lXm -lXt -lX11 $X_EXTRA_LIBS"
	])
    else
    AC_CHECK_FILE($x_libraries/libXm.a,[
      XLIBS="$x_libraries/libXm.a $X_LIBS $X_PRE_LIBS -lXt -lX11 $X_EXTRA_LIBS"
	],
	[
      AC_MSG_WARN(can't find $x_libraries/libXm.a!)
      XLIBS="$X_LIBS $X_PRE_LIBS -lXm -lXt -lX11 $X_EXTRA_LIBS"
	])
    fi
  else
    if test x$motif_prefix != x ; then
      XLIBS="$X_LIBS $X_PRE_LIBS -L$motif_prefix/lib -lXm -lXt -lX11 $X_EXTRA_LIBS"
      XFLAGS="$XFLAGS -I$motif_prefix/include"
    else
      XLIBS="$X_LIBS $X_PRE_LIBS -lXm -lXt -lX11 $X_EXTRA_LIBS"
    fi
  fi

  TEST_LIBS="$X_LIBS $X_PRE_LIBS -lXt -lX11 $X_EXTRA_LIBS"

  AC_CHECK_LIB(Xext, XShapeCombineMask, 
    [
     XLIBS="$XLIBS -lXext"
     TEST_LIBS="$TEST_LIBS -lXext"
    ], , $TEST_LIBS)

  GX_FILES="X_O_FILES"
  GX_HEADERS="SND_X_HEADERS"

  if test "$with_gtk" != yes && test "$with_motif" != no ; then

    SAVELIBS=$LIBS
    SAVEFLAGS=$CFLAGS

    # search for libXm will fail with Xp complaints in Linux, so we need to search for -lXp first
    AC_CHECK_LIB(Xp, XpGetDocumentData, [
	         XLIBS="$XLIBS -lXp"
	         TEST_LIBS="$TEST_LIBS -lXp"
	         ], , $TEST_LIBS)

    LIBS=$XLIBS
    CFLAGS=$XFLAGS

    if test "$with_editres" = yes ; then
	AC_CHECK_LIB(Xmu, _XEditResCheckMessages, 
			[AC_DEFINE(WITH_EDITRES)
			 XLIBS="$XLIBS -lXmu"])
    else
# need to check for -lXmu required by (buggy) libXm.a (openmotif 2.2.2)
	AC_MSG_CHECKING([whether libXm requires libXmu])
	AC_TRY_LINK_FUNC(XmCreateForm,
		AC_MSG_RESULT(no),
		[
		  AC_MSG_RESULT(yes)
		  XLIBS="$XLIBS -lXmu"
		],
		$TEST_LIBS)
    fi

    AC_CHECK_LIB(Xm, XmCreateForm, 
	[
	  with_motif=yes
 	  AC_DEFINE(USE_MOTIF)  # for Snd
	  AC_DEFINE(HAVE_MOTIF) # for xm
	  AC_SUBST(XLIBS)
	  AC_SUBST(XFLAGS)
	  if test "$with_xpm" != no; then
  	    AC_CHECK_LIB(Xpm, XpmReadFileToPixmap,
              [
  	       XLIBS="$XLIBS -lXpm"
   	       AC_DEFINE(HAVE_XPM)
   	      ], , $TEST_LIBS)
   	    AC_CHECK_LIB(Xpm, XpmGetErrorString,           [AC_DEFINE(HAVE_XPM_GET_ERROR_STRING)], ,             $TEST_LIBS)
   	    AC_CHECK_LIB(Xpm, XpmCreateXpmImageFromPixmap, [AC_DEFINE(HAVE_XPM_CREATE_XPM_IMAGE_FROM_PIXMAP)], , $TEST_LIBS)
	  fi
	  if test "$with_static_xm" = yes ; then
  	    GX_FILES="XM_O_FILES"
	    AC_DEFINE(HAVE_STATIC_XM)
          fi
   	  AC_CHECK_LIB(Xm, XmToolTipGetLabel, [AC_DEFINE(HAVE_XmToolTipGetLabel)], , $TEST_LIBS)
	  AC_MSG_CHECKING([whether sashes support relative panes])
	  AC_COMPILE_IFELSE(
            [AC_LANG_PROGRAM([#include <Xm/SashP.h>],
			     [[ Widget w; 
                                SashCallData call_data;
                                if ((XtIsSubclass(w, xmSashWidgetClass)) && 
                                    (strcmp(call_data->params[0], "Start") == 0)) return
			     ]])],
	    [
	     AC_DEFINE(WITH_RELATIVE_PANES)
	     AC_MSG_RESULT(yes)
	    ],
	    [AC_MSG_RESULT(no)])
	  if test "$with_xp" != no; then
	    AC_MSG_CHECKING([for XmPrint support])
	    AC_COMPILE_IFELSE(
	      [AC_LANG_PROGRAM([#include <Xm/Print.h>],
			       [ Widget w; 
                                 XmPrintShellCallbackStruct xmp;
			         XtIsSubclass(w, xmPrintShellWidgetClass)
			        ])],
	      [
	       AC_DEFINE(HAVE_XM_XP)
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
        ])
    LIBS=$SAVELIBS
    CFLAGS=$SAVEFLAGS

  else
    if test "$with_gtk" != no ; then
      with_gtk=yes
    fi
  fi

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
      	AC_SUBST(GTK_LIBS)
      	AC_SUBST(GTK_CFLAGS)
      	AC_DEFINE(USE_GTK)

	AC_CHECK_LIB(gdk, gdk_draw_pixbuf, [AC_DEFINE(HAVE_GDK_DRAW_PIXBUF)], ,$GTK_LIBS)
	# gtk 2.1

        if test "$with_x11" = yes ; then
	  AC_DEFINE(WITH_GTK_AND_X11)
        fi
	XLIBS=""
      	XFLAGS=""
      	AC_SUBST(XLIBS)
      	AC_SUBST(XFLAGS)
      	if test "$with_static_xm" = yes ; then
          if test "$with_x11" = yes ; then
            GX_FILES="XMG_O_FILES"
          else
            GX_FILES="XG_O_FILES"
          fi
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


# -------- OpenGL --------
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
    if test "$have_gl" = yes ; then
      AC_DEFINE(HAVE_GL)
      if test "$with_just_gl" = yes ; then
        AC_DEFINE(JUST_GL)
      else
        AC_COMPILE_IFELSE(
          [AC_LANG_PROGRAM([#include <GL/gl.h>], 
	 	           [int i; i = GL_TEXTURE_BINDING_3D])],
          [GL_FILES="gl.o"],
          [AC_DEFINE(JUST_GL)])
      fi   
      GL_LIBS="-lGLU -lGL"
      if test "$with_gtk" = yes; then
# there's no gtkglext config program, as far as I can tell, so...
        OLD_CFLAGS="$CFLAGS"
	OLD_CPPFLAGS="$CPPFLAGS"
        CFLAGS="-I/usr/local/include/gtkglext-1.0 -I/usr/local/lib/gtkglext-1.0/include $GTK_CFLAGS $CFLAGS"
        CPPFLAGS="-I/usr/local/include/gtkglext-1.0 -I/usr/local/lib/gtkglext-1.0/include $GTK_CFLAGS $CFLAGS"
	AC_CHECK_HEADER(gtk/gtkgl.h,
	  [
            GL_LIBS="$GL_LIBS -lgdkglext-x11-1.0 -lgtkglext-x11-1.0"
            GL_FLAGS="$GL_FLAGS -I/usr/local/include/gtkglext-1.0 -I/usr/local/lib/gtkglext-1.0/include"
	  ],
	  [
            GL_LIBS="$GL_LIBS -lgdkglext-x11-0.x -lgtkglext-x11-0.x"
            GL_FLAGS="$GL_FLAGS -I/usr/local/include/gtkglext-0.x"
	  ])
	CFLAGS="$OLD_CFLAGS"
	CPPFLAGS="$OLD_CPPFLAGS"
        AC_CHECK_LIB(m, gdk_window_get_gl_config, [AC_DEFINE(HAVE_GTK_GL_EXT_0_1)], , $GL_LIBS)
	AC_CHECK_LIB(m, gdk_gl_font_use_gdk_font, [AC_DEFINE(HAVE_GDK_GL_FONT_USE_GDK_FONT)], , $GL_LIBS)
	AC_CHECK_LIB(m, gtk_widget_gl_make_current, [AC_DEFINE(HAVE_GTK_WIDGET_GL_MAKE_CURRENT)], , $GL_LIBS)
	AC_CHECK_LIB(m, gdk_gl_context_copy, [AC_DEFINE(HAVE_GDK_GL_CONTEXT_COPY)], , $GL_LIBS)
      fi
    fi
  else
    AC_MSG_WARN([GL only works with Motif or Gtk2])
  fi
fi
AC_SUBST(GL_LIBS)
AC_SUBST(GL_FILES)
AC_SUBST(GL_FLAGS)


#--------------------------------------------------------------------------------
# Guile
#--------------------------------------------------------------------------------

if test "$with_ruby" != yes ; then

GUILE_LIBS=""
GUILE_CFLAGS=""
# GUILE_CONFIG_path=""
# allow user to specify this in invocation line
GUILE_LIB_path=""
XM_LIBS=""

if test "$with_guile" = no ; then
  AC_DEFINE(HAVE_GUILE,0)
  AC_DEFINE(HAVE_EXTENSION_LANGUAGE,0)
else

  AC_CHECK_FILE(/usr/lib/snd/bin/guile-config,[
    GUILE_CONFIG_path=/usr/lib/snd/bin/
    GUILE_LIB_path=/usr/lib/snd/lib
	])

  AC_MSG_CHECKING(for Guile)
  if test "$GUILE_CONFIG_path" != "" ; then
    if ! test -x "${GUILE_CONFIG_path}guile-config" ; then
      # try adding the "/" to the path    
      GUILE_CONFIG_path="${GUILE_CONFIG_path}/"
    fi
  fi
  if (${GUILE_CONFIG_path}guile-config link > /dev/null) 2>&1; then
    GUILE_CONFIG_works=yes
    guile_version="`${GUILE_CONFIG_path}guile -c '(display (version))'`"
    AC_MSG_RESULT($guile_version)
  else
    GUILE_CONFIG_works=no
    AC_MSG_RESULT(no)
  fi

if test $GUILE_CONFIG_works = yes; then
  GUILE_CFLAGS="`${GUILE_CONFIG_path}guile-config compile`"
  if test "$GUILE_LIB_path" != "" ; then
    # MacOSX linker doesn't know the -rpath stuff, but user may have placed Guile in /usr/lib/snd/bin
    case "$host" in
      *-apple-*)
        GUILE_LIBS="`${GUILE_CONFIG_path}guile-config link`"
        ;;
      *)
        GUILE_LIBS="-Xlinker -rpath -Xlinker $GUILE_LIB_path `${GUILE_CONFIG_path}guile-config link`"
	XM_LIBS="`${GUILE_CONFIG_path}guile-config link`"
	;;
    esac
  else
    # GUILE_LIB_path is empty
    GUILE_LIBS="`${GUILE_CONFIG_path}guile-config link`"
  fi

  if test "`${GUILE_CONFIG_path}guile -c '(display (string>=? (version) "1.3.4"))'`" != "#t"; then
    AC_MSG_WARN(Snd needs Guile 1.3.4 or later)
    AC_DEFINE(HAVE_GUILE,0)
    AC_DEFINE(HAVE_EXTENSION_LANGUAGE,0)
  else
    AC_SUBST(GUILE_CFLAGS)
    if test "$XM_LIBS" = ""; then
      XM_LIBS="$GUILE_LIBS"
    fi
    AC_SUBST(XM_LIBS)
    AC_SUBST(GUILE_LIBS)
    AC_DEFINE(HAVE_GUILE)
    AC_DEFINE(HAVE_EXTENSION_LANGUAGE)
    LOCAL_LANGUAGE="Guile"

    OLD_LIBS="$LIBS"
    LIBS="$GUILE_LIBS"
    OLD_CFLAGS="$CFLAGS"
    CFLAGS="$GUILE_CFLAGS"

# special Mac OSX stuff -- need to protect against multiple -lguile's, add -all_load, and add -lguile-ltdl
    case "$host" in
      *-apple-*)
 	CFLAGS="$CFLAGS -all_load"
        if test "`${GUILE_CONFIG_path}guile -c '(display (string>=? (version) "1.7"))'`" = "#t"; then
          LIBS="$OLD_LIBS -lguile-ltdl -ldl -lm"
        fi
    esac

    AC_CHECK_LIB(guile, scm_set_smob_apply,     [AC_DEFINE(HAVE_APPLICABLE_SMOB)])
    AC_CHECK_LIB(guile, scm_remember_upto_here, [AC_DEFINE(HAVE_SCM_REMEMBER_UPTO_HERE)])
    AC_CHECK_LIB(guile, scm_make_real,          [AC_DEFINE(HAVE_SCM_MAKE_REAL)])
    AC_CHECK_LIB(guile, scm_object_to_string,   [AC_DEFINE(HAVE_SCM_OBJECT_TO_STRING)])
    AC_CHECK_LIB(guile, scm_num2long_long,      [AC_DEFINE(HAVE_SCM_NUM2LONG_LONG)])
    AC_CHECK_LIB(guile, scm_num2int,            [AC_DEFINE(HAVE_SCM_NUM2INT)])
    AC_CHECK_LIB(guile, scm_c_make_vector,      [AC_DEFINE(HAVE_SCM_C_MAKE_VECTOR)])
    AC_CHECK_LIB(guile, scm_c_define,           [AC_DEFINE(HAVE_SCM_C_DEFINE)])
    AC_CHECK_LIB(guile, scm_c_define_gsubr,     [AC_DEFINE(HAVE_SCM_C_DEFINE_GSUBR)])
    AC_CHECK_LIB(guile, scm_c_eval_string,      [AC_DEFINE(HAVE_SCM_C_EVAL_STRING)])
    AC_CHECK_LIB(guile, scm_list_n,             [AC_DEFINE(HAVE_SCM_LIST_N)])
    AC_CHECK_LIB(guile, scm_str2symbol,         [AC_DEFINE(HAVE_SCM_STR2SYMBOL)])
    AC_CHECK_LIB(guile, scm_defined_p,          [AC_DEFINE(HAVE_SCM_DEFINED_P)])
    AC_CHECK_LIB(guile, scm_port_p,             [AC_DEFINE(HAVE_SCM_PORT_P)])
    AC_CHECK_LIB(guile, scm_mem2string,         [AC_DEFINE(HAVE_SCM_MEM2STRING)])
    AC_CHECK_LIB(guile, scm_continuation_p,     [AC_DEFINE(HAVE_SCM_CONTINUATION_P)])
    AC_CHECK_LIB(guile, scm_c_define_module,    [AC_DEFINE(HAVE_SCM_C_DEFINE_MODULE)
                                                 if test "$with_modules" = yes; then
						   AC_DEFINE(WITH_MODULES)
						 fi])
    AC_CHECK_TYPE(scm_t_catch_body,             [AC_DEFINE(HAVE_SCM_T_CATCH_BODY)], , [#include <guile/gh.h>])
    AC_CHECK_TYPE(scm_t_guard,                  [AC_DEFINE(HAVE_DYNAMIC_WIND)], , [#include <guile/gh.h>])
    LIBS="$OLD_LIBS"
    CFLAGS="$OLD_CFLAGS"

    if test "`${GUILE_CONFIG_path}guile -c '(display (string<=? (version) "1.3.4"))'`" = "#t"; then
      echo found old out-of-date Guile library
    fi
    if test "`${GUILE_CONFIG_path}guile -c '(display (string>=? (version) "1.5"))'`" = "#t"; then
      AC_DEFINE(WITH_RUN)
    fi
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
SO_LD="ld"
SO_INSTALL="install"
A_LD="ld"
A_LD_FLAGS=""
dnl Python configure.in has a lot of code to get these switches right


dnl we need the sndlib.h equivalents to try to find the native sound support (see config.guess)
dnl this only matters for those cases where we've implemented the audio code in audio.c
dnl test for ALSA courtesy of Paul Barton-Davis
dnl test for ESD courtesy of Nick Bailey
dnl test for BSD courtesy of Steven Schultz

if test "$with_esd" = yes ; then
  AC_CHECK_LIB(esd,main,[
    AC_CHECK_HEADER(esd.h,
      [
	AC_DEFINE(ESD)
	esd_version="`esd-config --version`"
        AC_DEFINE_UNQUOTED(ESD_VERSION,"${esd_version}")
        audiofile_version="`audiofile-config --version`"
        AC_DEFINE_UNQUOTED(AUDIOFILE_VERSION,"${audiofile_version}")
        AUDIO_LIB="`esd-config --libs`"
#       ESD_CFLAGS="`esd-config --cflags`"
	AC_SUBST(AUDIO_LIB)
        audio_system=ESD
      ],
      [AC_MSG_WARN(can't find the ESD header files)])],
    [AC_MSG_WARN(can't find the ESD library)])
fi

if test "$audio_system" != unknown ; then
  AC_MSG_RESULT(Using the $audio_system audio system)
else

case "$host" in
    *-*-linux*) 
	AC_DEFINE(LINUX)
        LDSO_FLAGS="-shared"
        LIBS="-lm"
dnl     This ^ used to be "" but in redhat 7 the -lm needs to be explicit for make sndsine etc
dnl     it was "" to cancel -lXpm without the needed -L<dir> for the same case

        audio_system=OSS
	if test "$with_alsa" = yes || test "$with_static_alsa" = yes ; then 
          AC_CHECK_LIB(asound,main,[ 
	    AC_CHECK_HEADER(sys/asoundlib.h,
                [audio_system=ALSA],
		[AC_CHECK_HEADER(alsa/asoundlib.h,
                    [audio_system=ALSA],
   	            [AC_MSG_WARN(can't find the ALSA header files)])])],
          [AC_MSG_WARN(can't find the ALSA library)])
        fi

	case $audio_system in
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
                    AC_MSG_CHECKING([snd_config_get_id args])
                    AC_COMPILE_IFELSE(
		      [AC_LANG_PROGRAM([#include <alsa/asoundlib.h>],
	             		       [snd_config_t *c; 
					const char *id; 
		      			snd_config_get_id(c, &id)])],
       		      snd_config_get_id_args=2)
                    AC_MSG_RESULT($snd_config_get_id_args)
                    AC_DEFINE_UNQUOTED(SND_CONFIG_GET_ID_ARGS,$snd_config_get_id_args)
		else
        	    audio_system=OSS
		    AC_DEFINE(HAVE_OSS)
		fi
		;;
	    OSS)
		AC_DEFINE(HAVE_OSS)
		;;
	esac
	;;
    *-*-sunos4*) 
        AC_DEFINE(SUN)
	LIBS="-lm"
	audio_system=Sun
        ;;
    *-*-solaris*) 
	AC_DEFINE(SUN)
	LIBS="-lm -ldl"
# odd... this causes an error in the sndlib configure (can't find libdl) but is needed here?
	audio_system=Sun
	LDSO_FLAGS="-G"
# if __SUNPRO_C we could add -xO3
        ;;
    *-*-hpux*) 
        AC_DEFINE(HPUX)
	audio_system=Hpux
	LDSO_FLAGS="+z -Ae +DA1.1"
        ;;
    *-next*) 
        AC_DEFINE(NEXT)
	audio_system=NeXT
        LIBS=""
	SO_INSTALL=":"
	SO_LD=":"
        ;;
    *-sgi*) 
        AC_DEFINE(SGI)
	audio_system=SGI
	AUDIO_LIB="-laudio"
	AC_SUBST(AUDIO_LIB)
        if test $GUILE_CONFIG_works = no; then
 	  LIBS="-lc -lm -lmd"
        else
	  LIBS="$LIBS -lmd"
        fi
        LDSO_FLAGS="-shared -all"
	SO_INSTALL=":"
	SO_LD=":"
	A_LD_FLAGS="-non_shared"
dnl should we include -lSgm if motif?
	# OPT:Olimit check borrowed from Python's configure.in
	AC_MSG_CHECKING(whether $CC accepts -OPT:Olimit=0)
	AC_CACHE_VAL(ac_cv_opt_olimit_ok,
	[ac_save_cc="$CC"
	CC="$CC -OPT:Olimit=0"
	AC_TRY_RUN([int main() { return 0; }],
 	  ac_cv_opt_olimit_ok=yes,
  	  ac_cv_opt_olimit_ok=no,
  	  ac_cv_opt_olimit_ok=no)
	CC="$ac_save_cc"])
	AC_MSG_RESULT($ac_cv_opt_olimit_ok)
	if test $ac_cv_opt_olimit_ok = yes; then
	  CFLAGS="$CFLAGS -OPT:Olimit=0"
	  if test $LOCAL_LANGUAGE = "None"; then
	    LIBS="$LIBS -lm"
          fi
	fi
dnl how to incorporate the TARG:platform arguments? (hinv...)
dnl what is -IPA?
dnl do we need explicit -n32?	
        ;;
    alpha*) 
        AC_DEFINE(ALPHA)
        ;;
    *-*-bsdi*) 
        AC_DEFINE(BSDI)
	AC_DEFINE(HAVE_OSS)
	LIBS="-lm"
	audio_system=OSS
        ;;
    *-*-freebsd*)
	AC_DEFINE(HAVE_OSS)
	LIBS="-lm"
	audio_system=OSS
	;;
    *-*-openbsd*) 
        AC_DEFINE(OPENBSD)
	audio_system=Sun
        ;;
    *-*-sco*) 
        AC_DEFINE(SCO5)
	audio_system=OSS
	LDSO_FLAGS="-belf -dy -Bdynamic -G"
        ;;
    *-*-cygwin*)
	AC_DEFINE(WINDOZE)
	audio_system=Windoze
	;;
    *-apple-*)
	AC_DEFINE(MAC_OSX)
	audio_system=MacOSX
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
	;;
esac
AC_MSG_CHECKING([for audio system])
AC_MSG_RESULT($audio_system)
fi

AC_SUBST(LDSO_FLAGS)
AC_SUBST(SO_INSTALL)
AC_SUBST(SO_LD)
AC_SUBST(A_LD)
AC_SUBST(A_LD_FLAGS)

#--------------------------------------------------------------------------------
# statfs special case
#   it is also possible for statfs to take 3 args
#   the libit configure.in and fsusage.c might be useful here.
#--------------------------------------------------------------------------------

dnl statfs is a mess
statfs_args=0
AC_MSG_CHECKING([stafs args])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <sys/statfs.h>],[struct statfs buf; statfs("test.snd",&buf,sizeof(buf),0)])],statfs_args=4)
if test $statfs_args = 0; then
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <sys/vfs.h>],[struct statfs buf; statfs("test.snd",&buf,sizeof(buf),0)])],statfs_args=4)
fi
if test $statfs_args = 0; then
  AC_COMPILE_IFELSE(
    [AC_LANG_PROGRAM([#include <dirent.h>
                      #include <sys/vfs.h>
                      #include <sys/statfs.h>
                     ],
                     [struct statfs buf; statfs("test.snd",&buf,sizeof(buf),0)])],
    statfs_args=4)
fi
if test $statfs_args = 0; then
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <sys/statfs.h>],[struct statfs buf; statfs("test.snd",&buf)])],statfs_args=2)
fi
if test $statfs_args = 0; then
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <sys/vfs.h>],[struct statfs buf; statfs("test.snd",&buf)])],statfs_args=2)
fi
if test $statfs_args = 0; then
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <sys/mount.h>],[struct statfs buf; statfs("test.snd",&buf)])],statfs_args=2)
fi
if test $statfs_args = 0; then
  AC_COMPILE_IFELSE(
    [AC_LANG_PROGRAM([#include <dirent.h>
                      #include <sys/vfs.h>
                      #include <sys/statfs.h>
                     ],
                     [struct statfs buf; statfs("test.snd",&buf)])],
    statfs_args=2)
fi
if test $statfs_args = 0; then
  statfs_args=2
fi
AC_MSG_RESULT($statfs_args)
AC_DEFINE_UNQUOTED(STATFS_ARGS,$statfs_args)
dnl on NeXT, this is apparently in libc.h?


#--------------------------------------------------------------------------------
# sigsetjmp special case
#--------------------------------------------------------------------------------

dnl look for sigsetjmp for segfault trap
if test "$enable_snd_debug" != yes ; then
  trap_segfault=no
  AC_MSG_CHECKING([for sigsetjmp])
  AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <setjmp.h>],
                                  [sigjmp_buf hi; sigsetjmp(hi,1)])],
                 trap_segfault=yes)
  if test $trap_segfault = yes; then
    AC_DEFINE(TRAP_SEGFAULT,1)
  fi
  AC_MSG_RESULT($trap_segfault)
fi


# -------- sndlib --------
#
# look for sndlib-config, check mus_sample_bits against current choice [with-float-sample, with-sample-width]
# check against needed version (17.2 for now -- need c_vct_copy in vct.c)
# set SNDLIB_FILES and SNDLIB_LIB

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
	SNDLIB_BITS=`$SNDLIB_CONFIG --bits`
	if test "$SNDLIB_BITS" = "$LOCAL_SNDLIB_BITS" ; then
          SNDLIB_LANGUAGE=`$SNDLIB_CONFIG --language`
          if test "$SNDLIB_LANGUAGE" = $LOCAL_LANGUAGE ; then
	    SNDLIB_FILES="NO_FILES"
            SNDLIB_LIB="-L$SNDLIB_PREFIX/lib -lsndlib"
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


AM_GNU_GETTEXT([external])

AC_CHECK_HEADER(nl_types.h,
  [AC_MSG_CHECKING([whether nl_types.h defines MALLOC])
   AC_COMPILE_IFELSE(
     [AC_LANG_PROGRAM([#include <langinfo.h>],
		      [char i; i = MALLOC; return(0)])],
     [AC_DEFINE(NL_TYPES_H_DEFINES_MALLOC)
      AC_MSG_RESULT(yes)
     ],
     [AC_MSG_RESULT(no)])])

CFLAGS="-I. $CFLAGS"
# needed since we're looking for <config.h>, I think (where is config.h written?)

AC_SUBST(MAKE_TARGET)
AC_OUTPUT

