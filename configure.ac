# Configuration script for Snd

AC_INIT(snd, 13.9, bil@ccrma.stanford.edu, ftp://ccrma-ftp.stanford.edu/pub/Lisp/snd-13.tar.gz)

AC_CONFIG_SRCDIR(snd.c)
AC_CANONICAL_HOST
AC_CONFIG_HEADERS(mus-config.h)
AC_CONFIG_FILES(makefile)
AC_PROG_CC
AC_HEADER_STDC # this needs to precede AC_CHECK_HEADER! (is this the case in 2.60?)
AC_PROG_INSTALL

MAKE_TARGET="snd"

AUDIO_SYSTEM="None"
RANDOM_FEATURES=""
OPTIONAL_LIBRARIES=""
LOCAL_LANGUAGE="None"
GRAPHICS_TOOLKIT="None"

PACKAGE=Snd
VERSION=13.9

#--------------------------------------------------------------------------------
# configuration options
#   --with-motif          use Motif (the default)
#   --with-gtk            use Gtk+	
#   --with-alsa           use ALSA if possible (the default)
#   --with-oss            use OSS 
#   --with-jack           use Jack
#   --without-audio       stub out all audio
#   --with-snd-as-widget  make Snd a loadable widget, not a standalone program
#   --with-gmp            include multiprecision arithmetic via gmp, mpfr, and mpc
#   --disable-deprecated  do not include any deprecated stuff (in gtk, motif, s7, sndlib, clm, etc)
#   --with-ladspa         include LADSPA plugin support (Linux)
#   --with-gui            make Snd with(out) any graphics support
#   --with-motif-prefix   set location of Motif
#   --with-forth          use Forth as the extension language
#   --with-ruby           use Ruby as the extension language
#   --with-ruby-prefix    set location of Ruby
#   --with-s7             use S7 as the extension language (default = yes)
#   --with-extension-language use some extension language (default=yes)
#   --with-temp-dir       directory to use for temp files
#   --with-save-dir       directory to use for saved-state files
#   --with-doc-dir        directory to search for documentation
#   --with-gl             include OpenGL support (default=no, Motif only)
#   --with-just-gl        include OpenGL support but omit the extension language bindings (default=no, Motif only)
#   --with-gl2ps          include gl2ps (Motif only)
#   --with-editres 	  include EditRes in xm
#   --without-gsl         omit GSL even if it exists
#   --without-fftw        omit FFTW even if it exists
#   --with-pulseaudio     use PulseAudio
#   --with-portaudio      use portaudio
#   SNDLIB_CONFIG_path    where to look for sndlib-config

#--------------------------------------------------------------------------------

AC_ARG_WITH(alsa,        [  --with-alsa		  use ALSA])
AC_ARG_WITH(oss,         [  --with-oss		  use OSS])
AC_ARG_WITH(jack,        [  --with-jack		  use JACK])
AC_ARG_WITH(gtk,         [  --with-gtk		  use Gtk+ to build Snd])
AC_ARG_WITH(gui,         [  --with-gui  	  make Snd with graphics support])
AC_ARG_WITH(gl,          [  --with-gl		  include OpenGL support, Motif only])
AC_ARG_WITH(just-gl,     [  --with-just-gl	  include OpenGL support, but omit the extension language bindings, Motif only])
AC_ARG_WITH(gl2ps,       [  --with-gl2ps		  include gl2ps, Motif only])
AC_ARG_WITH(motif,	 [  --with-motif	  	  use libXm to build Snd])
AC_ARG_WITH(editres,     [  --with-editres	  include editres in xm])
AC_ARG_WITH(ladspa,	 [  --with-ladspa  	  include support for LADSPA plugins, Linux only])
AC_ARG_WITH(float-samples, [  --with-float-samples	  use floats (or doubles) as the internal sample respresentation, default=yes])
AC_ARG_WITH(pulseaudio,  [  --with-pulseaudio 		  use PulseAudio, default=no])
AC_ARG_WITH(portaudio,   [  --with-portaudio 		  use portaudio, default=no])

# these are primarily for testing
AC_ARG_WITH(gsl,         [  --with-gsl		  use GSL, default=yes])
AC_ARG_WITH(fftw,        [  --with-fftw		  use fftw, default=yes])
AC_ARG_WITH(gmp,         [  --with-gmp		  include multiprecision arithmetic via gmp, mpfr, and mpc, default=no])

AC_ARG_WITH(s7,          [  --with-s7  	          use S7, default=yes])
AC_ARG_WITH(extension-language, [  --with-extension-language	use some extension language, default=yes])

# an experiment
AC_ARG_WITH(directfb,    [  --with-directfb	  use directfb config scripts, rather than gtk, default=no])
AC_ARG_WITH(audio,       [  --without-audio       don't include any audio functionality])


AC_ARG_WITH(motif-prefix,[  --with-motif-prefix=PFX where Motif is installed],
            motif_prefix="$withval", motif_prefix="")

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

# if mingw add -mwindows to the load flags (I think this means we're trying to build a GUI app, not just a console app??)
case "$host" in
  *-*-mingw*)
    LDFLAGS="$LDFLAGS -mwindows"
    ;;
esac



#--------------------------------------------------------------------------------
# standard libraries, header files, functions, OSS special cases
#--------------------------------------------------------------------------------

LIBS=""
AC_CHECK_LIB(m,main)
AC_CHECK_LIB(c,main)

AC_CHECK_HEADERS(sys/soundcard.h machine/soundcard.h sys/mixer.h stdbool.h sys/time.h)
AC_CHECK_HEADERS(sys/statvfs.h setjmp.h dlfcn.h sys/param.h byteswap.h dirent.h)

AC_CHECK_HEADER([sys/mount.h], [], [], [#if HAVE_SYS_PARAM_H
				        #include <sys/param.h>
				        #endif
				       ])

AC_CHECK_HEADER(alsa/asoundlib.h,[AC_DEFINE(HAVE_ALSA_ASOUNDLIB_H)])

AC_SEARCH_LIBS([dlopen], [c, dl], [AC_DEFINE([HAVE_DLOPEN], [1])]) 
# If this macro finds dlopen, HAVE_DLOPEN is defined (for use eg. in 
# snd-xen.c), if a lib is required, the macro adds it silently to LIBS

AC_OLD_BIGENDIAN
AC_CHECK_SIZEOF(void *)

AC_CHECK_FUNCS(readlink setlocale access opendir sleep signal statvfs statfs getline vasprintf lstat)

AC_PATH_PROG(PKG_CONFIG, pkg-config, no)


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
      OPTIONAL_LIBRARIES="$OPTIONAL_LIBRARIES fftw3"
      AC_MSG_RESULT(yes)
    else
      AC_MSG_RESULT(no)
    fi
  else
    SAVELIBS=$LIBS
    LIBS="$LIBS -lfftw3 -lm"
    AC_MSG_CHECKING([for fftw-3])
    AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <fftw3.h>],
		                [fftw_plan plan; fftw_execute(plan)])],
		[AC_MSG_RESULT(yes)
		 AC_DEFINE(HAVE_FFTW3)
                 OPTIONAL_LIBRARIES="$OPTIONAL_LIBRARIES fftw3"
		 FFTW_LIBS="-lfftw3"],
		[AC_MSG_RESULT(no)
                 LIBS=$SAVELIBS
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
# in C++, we want to set these to WITH_COMPLEX 1 and HAVE_COMPLEX_TRIG 0 no matter what
#
LIBS="$LIBS -lm"

if test "$CC" = "g++" ; then
  AC_DEFINE(WITH_COMPLEX)
  ac_snd_have_complex_trig=no
else

  AC_MSG_CHECKING(for complex numbers)
  AC_LINK_IFELSE(
  	[AC_LANG_PROGRAM(
        	[#include <complex.h>],
		[ double complex val; 
                  double rl, im;
                  val = 1.0 + 0.5 * _Complex_I;
                  rl = creal(val);
                  im = cimag(val);
                ])],
        [
 	 AC_DEFINE(WITH_COMPLEX)
	 AC_MSG_RESULT(yes)
    	],
	[AC_MSG_RESULT(no)])


  ac_snd_have_complex_trig=no
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
	 ac_snd_have_complex_trig=yes
	 AC_MSG_RESULT(yes)
    	],
	[AC_MSG_RESULT(no)])
fi


#--------------------------------------------------------------------------------
# GMP, MPFR, MPC
#--------------------------------------------------------------------------------

GMP_LIBS=""
GMP_CFLAGS=""
LOCAL_GMP_LIBS=""
ac_snd_have_gmp=no

if test "$with_gmp" = yes ; then
  # look for gmp, mpfr, and mpc 

  SAVELIBS=$LIBS
  LIBS="$LIBS -lgmp -lm"
  AC_CHECK_LIB(gmp, __gmpz_init,
             [
	       LOCAL_GMP_LIBS="-lgmp"
  	       ac_snd_have_gmp=yes
	     ],
             [AC_MSG_ERROR(libgmp not found)])

  if test "$ac_snd_have_gmp" != no ; then
    LIBS="$LIBS -lmpfr"
    AC_MSG_CHECKING(for libmpfr)
    LOCAL_GMP_LIBS="$LOCAL_GMP_LIBS -lmpfr"
    AC_LINK_IFELSE(
        [AC_LANG_PROGRAM(
                [#include <mpfr.h>],
                [mpfr_t x;  mpfr_init(x) ; mpfr_clear(x);]
        )],
        [AC_MSG_RESULT(yes)],
        [
	  AC_MSG_RESULT(no)
	  ac_snd_have_gmp=no
	])
  fi

  if test "$ac_snd_have_gmp" != no ; then
    LIBS="$LIBS -lmpc"
    AC_MSG_CHECKING(for libmpc)
    LOCAL_GMP_LIBS="$LOCAL_GMP_LIBS -lmpc"
    AC_LINK_IFELSE(
        [AC_LANG_PROGRAM(
                [#include <mpc.h>],
                [mpc_t x;  mpc_init2(x, 53); mpc_clear(x);  mpc_asin(x, x, MPC_RNDNN);] 
        )],
        [AC_MSG_RESULT(yes)
         AC_CHECK_LIB(mpc, mpc_arg, 
                [
		 AC_DEFINE(WITH_GMP)
		 GMP_LIBS="$LOCAL_GMP_LIBS"
                 OPTIONAL_LIBRARIES="$OPTIONAL_LIBRARIES gmp mpfr mpc"
		], 
                [AC_MSG_ERROR(libmpc too old)], 
                $GMP_LIBS)
	],
        [AC_MSG_RESULT(no)])
  fi

  LIBS=$SAVELIBS
fi

AC_SUBST(GMP_LIBS)



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
    GSL_LIBS=`$GSL_CONFIG --libs`
    gsl_version="`$GSL_CONFIG --version`"
    AC_MSG_RESULT($gsl_version)
    OPTIONAL_LIBRARIES="$OPTIONAL_LIBRARIES gsl"
    AC_DEFINE(HAVE_GSL)
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
  with_gui=no
fi

if test "$with_motif" = yes && test "$with_gtk" = yes ; then
  with_gtk=no
  AC_MSG_WARN([You asked for both Motif and Gtk -- Motif will be used])
fi

if test "$with_gui" = no ; then

  AC_DEFINE(USE_NO_GUI)
  XLIBS=""
  XFLAGS=""
  AC_SUBST(XLIBS)
  AC_SUBST(XFLAGS)
  GX_FILES="NO_GUI_O_FILES"
  GX_HEADERS="NO_GUI_HEADERS"

else

  # can we have gtk without X in mingw? -- depend on fixup around line 1030, I guess

  if test "$no_x" != yes; then

    AC_DEFINE(HAVE_X)
    XFLAGS="$X_CFLAGS"
    X_POST_LIBS="-lX11 $X_EXTRA_LIBS -lXext"

    case "$host" in
      *-apple-*)
        X_POST_LIBS="$X_POST_LIBS -lSM -lICE"
        ;;
    esac

    # this needs to precede the Xm check
    TEST_PRE_LIBS="$X_LIBS $X_PRE_LIBS"
    TEST_POST_LIBS="$X_POST_LIBS"
    TEST_LIBS="$TEST_PRE_LIBS $TEST_POST_LIBS"

    # search for libXp and libXm may need libXft so...
    AC_CHECK_LIB(Xft, XftFontOpen, [
	         X_PRE_LIBS="$X_PRE_LIBS -lXft"
	         TEST_PRE_LIBS="$TEST_PRE_LIBS -lXft"
                 TEST_LIBS="$TEST_PRE_LIBS $TEST_POST_LIBS"
		 ], , $TEST_LIBS)

    if test x$motif_prefix != x ; then
      X_PRE_LIBS="$X_LIBS $X_PRE_LIBS -L$motif_prefix/lib -lXm -lXt"
      XFLAGS="-I$motif_prefix/include $XFLAGS"
    else
      X_PRE_LIBS="$X_LIBS $X_PRE_LIBS -lXm -lXt"
    fi

    TEST_PRE_LIBS="$X_LIBS $X_PRE_LIBS"
    TEST_POST_LIBS="$X_POST_LIBS"
    TEST_LIBS="$TEST_PRE_LIBS $TEST_POST_LIBS"

    GX_FILES="MOTIF_O_FILES"
    GX_HEADERS="SND_X_HEADERS"

  fi

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
	  X_PRE_LIBS="$X_PRE_LIBS -lXmu -L/usr/local/lib -liconv -lpng -ljpeg"
	  TEST_PRE_LIBS="$TEST_PRE_LIBS -lXmu -L/usr/local/lib -liconv -lpng -ljpeg"
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

  	  XLIBS="$XLIBS -lXpm"
	  TEST_LIBS="$TEST_LIBS -lXpm"
	  AC_SUBST(XLIBS)
	  AC_SUBST(XFLAGS)

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
          ],
	[
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
    AC_DEFINE(USE_MOTIF)
    GRAPHICS_TOOLKIT="Motif"
  fi


#--------------------------------------------------------------------------------
# Gtk
#--------------------------------------------------------------------------------

  if test "$with_gtk" = yes ; then

    if test x$PKG_CONFIG != xno ; then
      if $PKG_CONFIG gtk+-3.0 --exists ; then

        AM_PATH_GTK_3_0(2.90.0,
          [
	    with_gtk=yes
          ],
          [
    	    AC_MSG_WARN([trouble with gtk -- will try to make Snd without any GUI])
      	    with_gtk=no
          ])
      else

        AM_PATH_GTK_2_0(2.12.0,
          [
	    with_gtk=yes
          ],
          [
    	    AC_MSG_WARN([trouble with gtk (we need version 2.12.0 or later) -- will try to make Snd without any GUI])
      	    with_gtk=no
          ])
      fi

    else

        AM_PATH_GTK_2_0(2.12.0,
          [
	    with_gtk=yes
          ],
          [
    	    AC_MSG_WARN([trouble with gtk (we need version 2.12.0 or later) -- will try to make Snd without any GUI])
      	    with_gtk=no
          ])
    fi

    if test "$with_gtk" = yes ; then
     	GX_FILES="GTK_O_FILES"
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
          if $PKG_CONFIG gtk+-3.0 --exists ; then
            if test "$with_directfb" = yes ; then
              GTK_LD_LIBS="`$PKG_CONFIG gtk+-directfb-3.0 --libs-only-L` `$PKG_CONFIG gtk+-directfb-3.0 --libs-only-l`"
            else
              GTK_LD_LIBS="`$PKG_CONFIG gtk+-3.0 --libs-only-L` `$PKG_CONFIG gtk+-3.0 --libs-only-l`"
            fi
          else
            if test "$with_directfb" = yes ; then
              GTK_LD_LIBS="`$PKG_CONFIG gtk+-directfb-2.0 --libs-only-L` `$PKG_CONFIG gtk+-directfb-2.0 --libs-only-l`"
            else
              GTK_LD_LIBS="`$PKG_CONFIG gtk+-2.0 --libs-only-L` `$PKG_CONFIG gtk+-2.0 --libs-only-l`"
            fi
          fi
        fi

      	AC_SUBST(GTK_LD_LIBS)

	if test x$PKG_CONFIG != xno ; then
          CAIRO_CFLAGS="`$PKG_CONFIG cairo --cflags-only-I`"
      	  AC_SUBST(CAIRO_CFLAGS)
          fi

	XLIBS=""
      	XFLAGS=""
      	AC_SUBST(XLIBS)
      	AC_SUBST(XFLAGS)
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
  if test "$with_motif" = yes ; then
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
    fi
  else
    AC_MSG_WARN([GL only works with Motif])
  fi
fi

if test "$have_gl" = yes ; then
  AC_DEFINE(HAVE_GL)
  OPTIONAL_LIBRARIES="$OPTIONAL_LIBRARIES openGL"
  if test "$with_gl2ps" = yes ; then
    AC_DEFINE(WITH_GL2PS)
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

#      GL_LIBS="-framework OpenGL"
# is this in place of or in addition to -lGL etc?
#   on OSX 10.5 we need the following:

       GL_LIBS="-L/usr/X11/lib -lGLU -lGL -Wl,-dylib_file,/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib:/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib -framework OpenGL"
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



ac_snd_have_extension_language=no


#--------------------------------------------------------------------------------
# Ruby
#--------------------------------------------------------------------------------

dnl AC_CHECK_RUBY([MINIMUM-VERSION [, ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for Ruby, defines
dnl   RUBY_VERSION
dnl   RUBY_RELEASE_DATE
dnl   RUBY_SEARCH_PATH
dnl   RUBY_CFLAGS
dnl   RUBY_LIBS

AC_DEFUN([AC_CHECK_RUBY],
[
  [AC_PATH_PROGS([RUBY], [$RUBY ruby], [no])]
  RUBY_VERSION=""
  RUBY_RELEASE_DATE=""
  RUBY_SEARCH_PATH=""
  RUBY_CFLAGS=""
  RUBY_LIBS=""
  AC_MSG_CHECKING([for Ruby])
  if test "$RUBY" != no ; then
    minimum_version=ifelse([$1], [], [1.8.0], [$1])
    RUBY_VERSION=`$RUBY -e 'puts RUBY_VERSION'`
    if `$RUBY -e "exit(RUBY_VERSION >= '$minimum_version' ? 0 : 1)"` ; then
      AC_MSG_RESULT([$RUBY_VERSION])
      RUBY_RELEASE_DATE=`$RUBY -e 'puts RUBY_RELEASE_DATE'`
      RUBY_SEARCH_PATH=`$RUBY -e 'puts $:.join(":")'`
      ruby_ldflags=""
      ruby_libs=""
      if `$RUBY -e "exit(RUBY_VERSION < '1.9.0' ? 0 : 1)"` ; then
        ruby_hdrdir=`$RUBY -rrbconfig -e [["puts Config::CONFIG['archdir']"]]`
        AC_MSG_CHECKING([for ruby.h])
        if test -e "$ruby_hdrdir/ruby.h" ; then
          RUBY_CFLAGS="-I$ruby_hdrdir"
          AC_MSG_RESULT([$ruby_hdrdir/ruby.h])
        else
          RUBY_CFLAGS=`$RUBY -e 'puts "-I" + $:.join(" -I")'`
          AC_MSG_RESULT([use \$LOAD_PATH])
        fi
        ruby_ldflags=`$RUBY -rrbconfig -e [["puts Config::CONFIG['LIBRUBYARG']"]]`
        ruby_libs=`$RUBY -rrbconfig -e [["puts Config::CONFIG['LIBS']"]]`
      else
        ruby_hdrdir=`$RUBY -rrbconfig -e [["puts RbConfig::CONFIG['rubyhdrdir']"]]`
        ruby_arch=`$RUBY -rrbconfig -e [["puts RbConfig::CONFIG['arch']"]]`
        AC_MSG_CHECKING([for ruby.h])
        if test -e "$ruby_hdrdir/ruby.h" ; then
          RUBY_CFLAGS="-I$ruby_hdrdir -I$ruby_hdrdir/$ruby_arch"
          AC_MSG_RESULT([$ruby_hdrdir/ruby.h])
        else
          ruby_hdrdir=`$RUBY -rrbconfig -e [["puts RbConfig::CONFIG['archdir']"]]`
          if test -e "$ruby_hdrdir/ruby.h" ; then
            RUBY_CFLAGS="-I$ruby_hdrdir"
            AC_MSG_RESULT([$ruby_hdrdir/ruby.h])
	  else
            RUBY_CFLAGS=`$RUBY -e 'puts "-I" + $:.join(" -I")'`
            AC_MSG_RESULT([use \$LOAD_PATH])
	  fi
        fi
        ruby_ldflags=`$RUBY -rrbconfig -e [["puts RbConfig::CONFIG['LIBRUBYARG']"]]`
        ruby_libs=`$RUBY -rrbconfig -e [["puts RbConfig::CONFIG['LIBS']"]]`
      fi
      RUBY_LIBS="$ruby_ldflags $ruby_libs"
      [$2]
    else
      AC_MSG_RESULT([Ruby version $RUBY_VERSION < $minimum_version])
      [$3]
    fi
  else
    AC_MSG_RESULT([no])
    [$3]
  fi

  AC_SUBST([RUBY_SEARCH_PATH])
  AC_SUBST([RUBY_CFLAGS])
  AC_SUBST([RUBY_LIBS])
])# AC_CHECK_RUBY

# readline (for Ruby)
AC_ARG_ENABLE(readline,
  [  --enable-readline      include readline (the default)], ,)

AC_ARG_WITH(ruby-prefix,[  --with-ruby-prefix=PFX  where Ruby is installed],
                ruby_prefix="$withval"
                RUBY="$ruby_prefix/bin/ruby",
                ruby_prefix="/usr/local")

if test "$with_ruby" = yes && test "$ac_snd_have_extension_language" = yes ; then
  with_ruby=no
  AC_MSG_WARN([You asked for both Ruby and $LOCAL_LANGUAGE -- $LOCAL_LANGUAGE will be used])
fi

AC_ARG_WITH(ruby,
        [  --with-ruby            use Ruby as the extension language],
        if test "$with_ruby" = yes ; then
           AC_CHECK_RUBY([1.8.0],
                         [AC_DEFINE(HAVE_RUBY)
                          AC_DEFINE_UNQUOTED(RUBY_SEARCH_PATH,  "$RUBY_SEARCH_PATH")
                          XEN_CFLAGS="$XEN_CFLAGS $RUBY_CFLAGS"
                          XEN_LIBS="$XEN_LIBS $RUBY_LIBS" 
                          if test "$enable_readline" != no ; then
                            AC_CHECK_LIB(readline, readline,
                                         [AC_DEFINE(HAVE_READLINE)
                                          XEN_LIBS="$XEN_LIBS -lreadline -lncurses"], ,"-lncurses")
                          fi
                          AC_SUBST(XEN_LIBS)
                          AC_SUBST(XEN_CFLAGS)
                          LOCAL_LANGUAGE="Ruby"
                          ac_snd_have_extension_language=yes])
        fi)



#--------------------------------------------------------------------------------
# Forth
#--------------------------------------------------------------------------------

if test "$with_forth" = yes && test "$ac_snd_have_extension_language" = yes ; then
  with_forth=no
  AC_MSG_WARN([You asked for both Forth and $LOCAL_LANGUAGE -- $LOCAL_LANGUAGE will be used])
fi

AC_ARG_WITH(forth,
	[  --with-forth	  	  use Forth as the extension language],
  	if test "$with_forth" = yes ; then
	   FTH_CHECK_LIB([AC_DEFINE(HAVE_FORTH)
			  if test "$FTH_HAVE_COMPLEX" = yes ; then
     	      		     AC_DEFINE(HAVE_COMPLEX_TRIG)
     	      		     AC_DEFINE(HAVE_MAKE_COMPLEX)
     	      		     AC_DEFINE(HAVE_MAKE_RECTANGULAR)
			  fi
			  if test "$FTH_HAVE_RATIO" = yes ; then
     	      		     AC_DEFINE(HAVE_MAKE_RATIO)
			  fi
               		  AC_SUBST(XEN_CFLAGS, $FTH_CFLAGS)
               		  AC_SUBST(XEN_LIBS,   $FTH_LIBS)
 	      		  LOCAL_LANGUAGE="Forth"
 	      		  ac_snd_have_extension_language=yes])
	fi)



#--------------------------------------------------------------------------------
# S7 (the default)
#--------------------------------------------------------------------------------

S7_LIB=""

if test "$with_s7" = yes && test "$ac_snd_have_extension_language" = yes ; then
  with_s7=no
  AC_MSG_WARN([You asked for both s7 and $LOCAL_LANGUAGE -- $LOCAL_LANGUAGE will be used])
fi

if test "$with_s7" != no && test "$with_extension_language" != no && test "$ac_snd_have_extension_language" != yes ; then
    AC_DEFINE(HAVE_SCHEME)	
    ac_snd_have_extension_language=yes
    AC_DEFINE(HAVE_MAKE_RATIO)
    AC_DEFINE(WITH_SYSTEM_EXTRAS)
    AC_CHECK_HEADER(complex.h, 
        [
         AC_DEFINE(HAVE_MAKE_RECTANGULAR)
	])

    LOCAL_LANGUAGE="s7"
    S7_LIB="s7.o"
fi

AC_SUBST(S7_LIB)



#--------------------------------------------------------------------------------
# Audio library
#--------------------------------------------------------------------------------

AUDIO_LIB=""
LDSO_FLAGS=""
SO_FLAGS=""
SO_LD="ld"

JACK_LIBS=""
JACK_FLAGS=""


if test "$with_audio" != no ; then

# we need the sndlib.h equivalents to try to find the native sound support (see config.guess)
# this only matters for those cases where we've implemented the audio code in audio.c
# test for ALSA courtesy of Paul Davis
# test for BSD courtesy of Steven Schultz
# test for Jack courtesy of Kjetil S. Matheussen

if test "$with_pulseaudio" = yes ; then
  AC_CHECK_LIB(pulse-simple, main, [
    AC_CHECK_HEADER(pulse/simple.h,
       [AC_DEFINE(MUS_PULSEAUDIO)
	if test x$PKG_CONFIG != xno ; then
          AUDIO_LIB="`$PKG_CONFIG libpulse-simple --libs`"
        else 
          AUDIO_LIB="-lpulse-simple"
        fi
        AUDIO_SYSTEM=pulseaudio
       ],
      [AC_MSG_WARN(can't find the pulseaudio header files)])],
    [AC_MSG_WARN(can't find the pulseaudio library)])
fi


if test "$with_portaudio" = yes ; then
  AC_CHECK_LIB(portaudio, main, [
    AC_CHECK_HEADER(portaudio.h, [
          AUDIO_SYSTEM=portaudio
	  AC_DEFINE(MUS_PORTAUDIO)
	if test x$PKG_CONFIG != xno ; then
          AUDIO_LIB="`$PKG_CONFIG portaudio-2.0 --libs`"
        else 
	  AUDIO_LIB="-lportaudio"
        fi
	],
      [AC_MSG_WARN(can't find the portaudio header file)])],
    [AC_MSG_WARN(can't find the portaudio library)])
fi


if test "$AUDIO_SYSTEM" != None ; then
  AC_MSG_RESULT(Using the $AUDIO_SYSTEM audio system)
else

if test "$with_alsa" = yes && test "$with_oss" = yes ; then
  with_oss=no
  AC_MSG_WARN([You asked for both ALSA and OSS -- ALSA will be used])
fi

if test "$with_jack" = yes ; then 
            AC_CHECK_LIB(jack, main, [ 
	      AC_CHECK_HEADER(jack/jack.h, ,
   	          [with_jack=no
                   AC_MSG_WARN(can't find the JACK header files)])],
              [with_jack=no
	       AC_MSG_WARN(can't find the JACK library)])
            AC_CHECK_LIB(samplerate, main,[ 
	      AC_CHECK_HEADER(samplerate.h, ,
   	          [with_jack=no
                   AC_MSG_WARN(can't find the samplerate header files. JACK will not be used.)])],
              [with_jack=no
	       AC_MSG_WARN(can't find the samplerate library. JACK will not be used.)])
            if test $with_jack = yes ; then
               JACK_LIBS="-ljack -lsamplerate"
               AUDIO_SYSTEM=JACK
	       AC_DEFINE(MUS_JACK)
	       if test x$PKG_CONFIG != xno ; then
          	   JACK_LIBS="`$PKG_CONFIG jack --libs`"
          	   JACK_FLAGS="`$PKG_CONFIG jack --cflags`"
		   JACK_LIBS="$JACK_LIBS `$PKG_CONFIG samplerate --libs`"
		   JACK_FLAGS="$JACK_FLAGS `$PKG_CONFIG samplerate --cflags`"
               fi
            fi
fi


case "$host" in
    *-*-linux*) 
        LDSO_FLAGS="-shared"
        LIBS="$LIBS -lm"
#
# -ldl is now handled by LIBS
#	if test "$ac_cv_header_dlfcn_h" = yes ; then 
#	  LDFLAGS="$LDFLAGS -ldl" 
#	fi 
#
	if test "$GCC" = yes ; then
	  SO_FLAGS="-fPIC $SO_FLAGS"
        fi

        AUDIO_SYSTEM=ALSA
	if test "$with_oss" = yes ; then 
          AUDIO_SYSTEM=OSS
	else

          if test "$ac_cv_header_alsa_asoundlib_h" = yes ; then
            AC_CHECK_LIB(asound, main,
               AUDIO_SYSTEM=ALSA,
               [AC_MSG_WARN(can't find the ALSA library)])
          else
            AC_MSG_WARN(can't find the ALSA header files)
	    AUDIO_SYSTEM=OSS
          fi
	fi

	if test "$with_jack" = yes ; then 
	       if test "$with_alsa" != yes ; then
                 AUDIO_SYSTEM=JACK
               fi
	fi

	case $AUDIO_SYSTEM in
	    ALSA)
                # if alsa version < 1.0 complain and use OSS
		alsa_ok=yes
		AC_MSG_CHECKING([for ALSA 1.0 or later])
		AC_COMPILE_IFELSE(
                  [AC_LANG_PROGRAM([#include <alsa/asoundlib.h>],
			           [
			  	    #if (SND_LIB_MAJOR < 1)
				      #error too old
				    #endif
				   ])],
		  alsa_ok=yes,
	          alsa_ok=no)
		AC_MSG_RESULT($alsa_ok)
		if test "$alsa_ok" = yes ; then
	    	    AC_DEFINE(HAVE_ALSA)
	    	    AUDIO_LIB="-lasound"
		    if test "$with_jack" = yes ; then 
		      AUDIO_LIB="-lasound -lsamplerate"
		    fi
 		else
         	    AUDIO_SYSTEM=OSS
 		    AC_DEFINE(HAVE_OSS)
 		fi
 		;;
	    JACK)
		AC_DEFINE(HAVE_OSS)
		JACK_LIBS="$JACK_LIBS -lpthread"
# added -lpthread 21-May-10 for FC13 (Bill S)
		AUDIO_LIB="-lsamplerate"
		;;
	    OSS)
		AC_DEFINE(HAVE_OSS)
		AUDIO_SYSTEM=OSS
		;;
	esac
	;;
    *-*-sunos4*) 
	LIBS="$LIBS -lm"
	AUDIO_SYSTEM=Sun
        ;;
    *-*-solaris*) 
#	LIBS="$LIBS -lm -ldl"
# odd... this causes an error in the sndlib configure (can't find libdl) but is needed here?
	AUDIO_SYSTEM=Sun
	LDSO_FLAGS="-G"
# if __SUNPRO_C we could add -xO3
        ;;
    *-*-hpux*) 
	AUDIO_SYSTEM=Hpux
	LDSO_FLAGS="+z -Ae +DA1.1"
	if test "$GCC" = yes ; then
	  SO_FLAGS="-fPIC $SO_FLAGS"
        fi
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
	  SO_LD="$CC"
	  SO_FLAGS="-fPIC $SO_FLAGS"
          LDSO_FLAGS="-shared"
        fi
	;;
    *-*-openbsd*) 
	AUDIO_SYSTEM=OpenBSD
	if test "$GCC" = yes ; then
	  SO_FLAGS="-fPIC $SO_FLAGS"
          CFLAGS="-ftrampolines $CFLAGS"
        fi
        ;;
    *-*-netbsd*) 
	AUDIO_SYSTEM=NetBSD
	if test "$GCC" = yes ; then
	  SO_LD="$CC"
	  SO_FLAGS="-fPIC $SO_FLAGS"
          LDSO_FLAGS="-shared"
        fi
        ;;
    *-*-cygwin*)
	if test "$with_jack" != yes ; then 
	    AUDIO_SYSTEM=Windows
	fi
	;;
    *-*-mingw*)
	audio_system=Windows
	LIBS="$LIBS -lwinmm -lwsock32"
	;;
    *-apple-*)
	if test "$with_jack" != yes ; then 

	    AUDIO_SYSTEM=MacOSX
	    AUDIO_LIB="-framework CoreAudio -framework CoreFoundation -framework CoreMIDI"

	    # OSX 10.5, deprecating earlier AudioDeviceRemoveIOProc
	    AC_MSG_CHECKING([for AudioDeviceDestroyIOProcID])
	    AC_COMPILE_IFELSE(
              [AC_LANG_PROGRAM([#include <CoreServices/CoreServices.h>
                                #include <CoreAudio/CoreAudio.h>],
			       [AudioDeviceIOProcID procId;
			        AudioDeviceID device = kAudioDeviceUnknown;
        			AudioDeviceDestroyIOProcID(device, procId)
			       ])],
	      [AC_DEFINE(HAVE_AUDIODEVICEDESTROYIOPROCID)
	       AC_MSG_RESULT(yes)],
	      [AC_MSG_RESULT(no)])
        else
            AUDIO_SYSTEM=JACK
	    JACK_LIBS="-framework CoreAudio -framework CoreServices -framework AudioUnit -L/usr/local/lib -ljack -lsamplerate"
            JACK_FLAGS="-I/usr/local/include"
	fi
	;;
esac
AC_MSG_CHECKING([for audio system])
AC_MSG_RESULT($AUDIO_SYSTEM)
fi
fi

if test "$AUDIO_SYSTEM" != None ; then
  AC_DEFINE(WITH_AUDIO)
fi

AC_SUBST(AUDIO_LIB)
AC_SUBST(JACK_LIBS)
AC_SUBST(JACK_FLAGS)


AC_SUBST(LDSO_FLAGS)
AC_SUBST(SO_FLAGS)
AC_SUBST(SO_LD)


#--------------------------------------------------------------------------------
# OGG, Flac, Speex, Mpeg, Timidity, TTA, Wavpack
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
  AC_DEFINE(HAVE_MPG321)
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



#--------------------------------------------------------------------------------
# debugging stuff
#--------------------------------------------------------------------------------

AC_ARG_ENABLE(deprecated,
  [  --disable-deprecated	  do not include any deprecated stuff from gtk, s7, motif, clm, snd, or sndlib],
  if test "$enable_deprecated" = no ; then
    CFLAGS="-DGTK_DISABLE_DEPRECATED -DG_DISABLE_DEPRECATED -DGDK_DISABLE_DEPRECATED -DPANGO_DISABLE_DEPRECATED -DCAIRO_DISABLE_DEPRECATED -DGSEAL_ENABLE -DGTK_DISABLE_SINGLE_INCLUDES -Wall $CFLAGS"
    AC_DEFINE(DISABLE_DEPRECATED)
  fi)

if test "$enable_snd_debug" = yes ; then
  CFLAGS="-I. $CFLAGS"
  if test "$GCC" = yes ; then
    if test "$LOCAL_LANGUAGE" = "Ruby" ; then
      if test "$CC" = "g++" ; then
        CFLAGS="$CFLAGS -O -g3 -Wpointer-arith -Wimplicit -Wreturn-type -Wunused-label -Wunused-variable -Wunused-value -Wcomment -Wformat -Wunused-function -Wuninitialized -Wparentheses -Wall -Winit-self -Wsequence-point -Wmissing-field-initializers -Wmissing-prototypes"
      else
        CFLAGS="$CFLAGS -O -g3 -Wpointer-arith -Wimplicit -Wreturn-type -Wunused-label -Wunused-variable -Wunused-value -Wcomment -Wformat -Wunused-function -Wuninitialized -Wparentheses -Wall -Winit-self -Wsequence-point -Wmissing-field-initializers -Wmissing-prototypes"
      fi
    else
      if test "$CC" = "g++" ; then
        CFLAGS="$CFLAGS -O -g3 -Wredundant-decls -Wpointer-arith -Wimplicit -Wreturn-type -Wunused-label -Wunused-variable -Wunused-value -Wcomment -Wformat -Wunused-function -Wuninitialized -Wparentheses -Wall -Winit-self -Wsequence-point -Wmissing-field-initializers -Wmissing-prototypes"
      else
        CFLAGS="$CFLAGS -O -g3 -Wredundant-decls -Wmissing-prototypes -Wpointer-arith -Wimplicit -Wreturn-type -Wunused-label -Wunused-variable -Wunused-value -Wcomment -Wformat -Wunused-function -Wuninitialized -Wparentheses -Wall -Winit-self -Wsequence-point -Wmissing-field-initializers -Wmissing-prototypes"
      fi
    fi
  fi
else
  CFLAGS="-O2 -I. $CFLAGS"
fi

# -ffast-math appears to make exp about twice as fast, but slows down filtering by the same amount -- kinda strange
#   timing tests using -O3 -ffast-math indicate an overall speedup of maybe 4-5% in Snd, 1951 -> 1936 in s7test
#
# I also tried -ftree-vectorize but didn't see any big improvement


#--------------------------------------------------------------------------------
# export-dynamic
#--------------------------------------------------------------------------------

ORIGINAL_LDFLAGS="$LDFLAGS"

case "$host" in
  *-*-linux* | *-*-bsdi* | *-*-freebsd* | *-*-openbsd* | *-*-netbsd*) 
    if test "$with_snd_as_widget" != yes ; then
      if test "$ac_cv_header_dlfcn_h" = yes ; then
        if test "$CC" = "gcc" || test "$CC" = "g++" ; then
          LDFLAGS="$LDFLAGS -Wl,-export-dynamic"
          # I think this really should be CFLAGS since it's assuming it's passed to gcc, not ld, but
          #   that ends up generating endless dumb warnings in gcc
        fi
      fi
    fi
esac



#--------------------------------------------------------------------------------
# error checks, output
#--------------------------------------------------------------------------------

if test "$ac_snd_have_extension_language" = yes ; then
  if test "$ac_snd_have_gui" = yes ; then
    if test "$ac_cv_header_pthread_h" = yes ; then
      LDFLAGS="$LDFLAGS -lpthread"
    fi
  fi
fi

if test "$ac_snd_have_extension_language" = no ; then
  if test "$ac_snd_have_gui" = no ; then
#    if test "$ac_cv_header_dlfcn_h" = yes ; then
#      LDFLAGS="$LDFLAGS -ldl"
#    fi
    AC_MSG_WARN([Snd needs either an extension language (s7, Fth, or Ruby), or a graphics toolkit (Gtk or Motif), or preferably both.  As currently configured, this version of Snd is useless.])
  fi
fi

# if test "$ac_snd_have_extension_language" = yes && test "$with_audio" = no && test "$ac_cv_header_dlfcn_h" = yes ; then
#   LDFLAGS="$LDFLAGS -ldl"
# fi


AC_SUBST(CFLAGS)
AC_SUBST(ORIGINAL_LDFLAGS)
AC_SUBST(LDFLAGS)
AC_SUBST(MAKE_TARGET)
AC_OUTPUT

# borrowed from gnucash

AC_MSG_RESULT([
  Options selected
  -------------------------
  Snd version ...........: $VERSION
  CFLAGS ................: $CFLAGS
  LDFLAGS ...............:$LDFLAGS
  LIBS...................: $LIBS
  prefix.................: ${prefix}
  extension language.....: $LOCAL_LANGUAGE
  audio system...........: $AUDIO_SYSTEM
  graphics toolkit.......: $GRAPHICS_TOOLKIT
  optional libraries.....:$OPTIONAL_LIBRARIES
  random features........:$RANDOM_FEATURES
  environs...............: $host $CC
])

