dnl Configuration script for Snd
dnl Process this file with autoconf 2.50 to produce a configure script.

#--------------------------------------------------------------------------------
#
# this configure/make process tested so far in 
#       linux (rh 5.2, 6.0, 6.1, 6.2, 7.0, 7.1, 7.2), Mandrake 7.0, Suse Linux 6.4, Debian 2.2
#       sgi (irix 5.3, 6.2, 6.3, 6.5), 
#       mklinux (dr3), linuxppc (r4, r5, 2000), Debian ppc linux 2.2
#       sun (solaris 2.7, 2.8, linux)
#       w95/98 with cygwin
#       beos release 5 (intel)
#       next m68k and next i386
#       Mac OS-X (Darwin 1.1, 1.3)
#       Alpha Linux
#       FreeBSD 4.2
#
# config.guess:
#   m68k-next-nextstep3 mips-sgi-irix6.2 i386-next-nextstep3 i586-pc-beos
#   i686-pc-linux-gnu powerpc-unknown-linux-gnu sparc-sun-solaris2.7
#   powerpc-apple-darwin1.1 i386-pc-cygwin32 sparc64-unknown-linux-gnu
#   alphaev67-unknown-linux-gnu i386-unknown-freebsd4.2 powerpc-apple-darwin1.3.7
#
#--------------------------------------------------------------------------------
#
# still TODO: sndlib configuration
#             Mac OSX Guile/Gtk searches (Ruby?)
#             Sgi compiler optimations automated somehow
#             Gtk 2.0 choice
#             Lesstif detection and warning
#
#--------------------------------------------------------------------------------

AC_INIT(snd, 5.7, bil@ccrma.stanford.edu)
AC_CONFIG_SRCDIR(snd.c)
AC_CANONICAL_HOST
AC_CONFIG_HEADER(config.h)
AC_CONFIG_FILES(makefile)
AC_PROG_CC

audio_system="unknown"
MAKE_TARGET="snd"

#--------------------------------------------------------------------------------
# configuration options
#   --with-motif          use Motif (the default)
#   --with-static-motif   use Motif statically loaded (for RPM generation)
#   --with-gtk            use Gtk+	
#   --with-gtk-extra      use Gtk+extra file selection widget
#   --with-alsa           use ALSA if possible
#   --with-static-alsa    use ALSA statically loaded (for RPM generation)
#   --with-html           include the XmHTML or mozilla browser library
#   --with-big-colormap   use big colormap (default = yes)
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
#--------------------------------------------------------------------------------

AC_ARG_WITH(esd,        [  --with-esd		  use ESD])
AC_ARG_WITH(alsa,       [  --with-alsa		  use ALSA])
AC_ARG_WITH(static-alsa,[  --with-static-alsa	  use ALSA statically loaded])
AC_ARG_WITH(gtk,        [  --with-gtk		  use Gtk+ to build Snd])
AC_ARG_WITH(no-gui,     [  --with-no-gui  	  make Snd without any graphics support])
AC_ARG_WITH(doubles,    [  --with-doubles	  use doubles throughout])
AC_ARG_WITH(static-gsl, [  --with-static-gsl	  use libgsl.a to build Snd])
AC_ARG_WITH(xm,         [  --with-static-xm	  include the xm module])
AC_ARG_WITH(guile,      [  --with-guile		  use Guile])

if test "$with_doubles" = yes; then
  AC_DEFINE(Float,double)
else
  AC_DEFINE(Float,float)
fi

XMHTML_LIB=""
GTKHTML_LIB=""
GTKHTML_FLAGS=""
AC_ARG_WITH(html,
	[  --with-html		  include the XmHTML or mozilla browser library],
  	if test "$with_html" = yes ; then
	    AC_DEFINE(HAVE_HTML)
            XMHTML_LIB="-lXmHTML -ljpeg -lpng"
            GTKHTML_LIB="-Xlinker -rpath -Xlinker /usr/lib/mozilla -L/usr/lib/mozilla -lgtkembedmoz"
            GTKHTML_FLAGS="-I/usr/include/mozilla"
dnl which we actually use is decided later
	fi)

AC_ARG_WITH(big-colormap, [  --with-big-colormap	  assume at least 512 colors are available (default=yes)])
if test "$with_big_colormap" = no ; then
  AC_DEFINE_UNQUOTED(WITH_BIG_COLORMAP,0)
else
  AC_DEFINE(WITH_BIG_COLORMAP)
fi

AC_ARG_WITH(motif,
	[  --with-motif		  use libXm to build Snd],
  	if test "$with_motif" = yes ; then
	    AC_DEFINE(USE_MOTIF)
	fi)

AC_ARG_WITH(motif-prefix,[  --with-motif-prefix=PFX where Motif is installed],
            motif_prefix="$withval", motif_prefix="")

AC_ARG_WITH(static-motif,
	[  --with-static-motif	  use libXm.a to build Snd],
  	if test "$with_static_motif" = yes ; then
	    AC_DEFINE(USE_MOTIF)
            with_motif=yes
	fi)

AC_ARG_WITH(temp-dir,
	[  --with-temp-dir	  directory to use for temp files],
	    AC_DEFINE_UNQUOTED(DEFAULT_TEMP_DIR, "${withval}")
	)

AC_ARG_WITH(save-dir,
	[  --with-save-dir	  directory to use for saved-state files],
	    AC_DEFINE_UNQUOTED(DEFAULT_SAVE_DIR, "${withval}")
	)

AC_ARG_WITH(ruby-prefix,[  --with-ruby-prefix=PFX  where Ruby is installed],
            ruby_prefix="$withval", ruby_prefix="")

AC_ARG_WITH(ruby,
	[  --with-ruby	  	  try to use Ruby as the extension language],
  	if test "$with_ruby" = yes ; then
            AC_MSG_CHECKING([for Ruby])
	    RUBY_VERSION=`ruby -e 'puts RUBY_VERSION'`
	    RUBY_RELEASE_DATE=`ruby -e 'puts RUBY_RELEASE_DATE'`
            if test "$RUBY_VERSION" > "0" ; then
	      AC_DEFINE(HAVE_RUBY)
	      AC_DEFINE(HAVE_EXTENSION_LANGUAGE)
              AC_MSG_RESULT($RUBY_VERSION)
              AC_DEFINE_UNQUOTED(RUBY_VERSION,"${RUBY_VERSION}")
              AC_DEFINE_UNQUOTED(RUBY_RELEASE_DATE,"${RUBY_RELEASE_DATE}")
              if test x$ruby_prefix != x ; then
      		GUILE_CFLAGS=" -I$ruby_prefix"
                GUILE_LIBS="-L$ruby_prefix"
              else
                RUBY_LOC=`ruby -e [['puts \$:[0]']]`  # good grief... 
                GUILE_CFLAGS="-I$RUBY_LOC"
	        GUILE_LIBS="-L$RUBY_LOC"
                RUBY_LOC=`ruby -e [['puts \$:[1]']]`
	        if test "$RUBY_LOC" != nil && test "$RUBY_LOC" != "."; then
                  GUILE_CFLAGS="$GUILE_CFLAGS -I$RUBY_LOC"
	          GUILE_LIBS="$GUILE_LIBS -L$RUBY_LOC"
                  RUBY_LOC=`ruby -e [['puts \$:[2]']]`
	          if test "$RUBY_LOC" != nil && test "$RUBY_LOC" != "."; then
                    GUILE_CFLAGS="$GUILE_CFLAGS -I$RUBY_LOC"
	            GUILE_LIBS="$GUILE_LIBS -L$RUBY_LOC"
                    RUBY_LOC=`ruby -e [['puts \$:[3]']]`
	            if test "$RUBY_LOC" != nil && test "$RUBY_LOC" != "."; then
                      GUILE_CFLAGS="$GUILE_CFLAGS -I$RUBY_LOC"
	              GUILE_LIBS="$GUILE_LIBS -L$RUBY_LOC"
                      RUBY_LOC=`ruby -e [['puts \$:[4]']]`
	              if test "$RUBY_LOC" != nil && test "$RUBY_LOC" != "."; then
                        GUILE_CFLAGS="$GUILE_CFLAGS -I$RUBY_LOC"
	                GUILE_LIBS="$GUILE_LIBS -L$RUBY_LOC"
                        RUBY_LOC=`ruby -e [['puts \$:[5]']]`
	                if test "$RUBY_LOC" != nil && test "$RUBY_LOC" != "."; then
                          GUILE_CFLAGS="$GUILE_CFLAGS -I$RUBY_LOC"
	                  GUILE_LIBS="$GUILE_LIBS -L$RUBY_LOC"
                          RUBY_LOC=`ruby -e [['puts \$:[6]']]`
	                  if test "$RUBY_LOC" != nil && test "$RUBY_LOC" != "."; then
                            GUILE_CFLAGS="$GUILE_CFLAGS -I$RUBY_LOC"
	                    GUILE_LIBS="$GUILE_LIBS -L$RUBY_LOC"
  		          fi
	 	        fi
	              fi
	  	    fi
		  fi
	        fi
	      fi
              GUILE_LIBS="$GUILE_LIBS -lruby -ldl -lcrypt"
	      AC_CHECK_LIB(readline, readline, 
			   [AC_DEFINE(HAVE_READLINE)
			    GUILE_LIBS="$GUILE_LIBS -lreadline -lncurses"], ,"-lncurses")
              AC_SUBST(GUILE_LIBS)
              AC_SUBST(GUILE_CFLAGS)
            else 
              AC_MSG_WARN([can't find Ruby!])
  	      AC_DEFINE(HAVE_RUBY,0)
              AC_DEFINE(HAVE_EXTENSION_LANGUAGE,0)
            fi
	fi)


# -------- GSL --------
GSL_LIBS=""
GSL_CFLAGS=""
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
AC_SUBST(GSL_LIBS)
AC_SUBST(GSL_CFLAGS)


# -------- SNDLIB (not ready yet...) --------
# SNDLIB_LIB=""
# SNDLIB_FILES="headers.o audio.o io.o sound.o clm.o vct.o sndlib2xen.o clm2xen.o"
# AC_ARG_WITH(sndlib,
# 	[  --with-sndlib		  use libsndlib.so if possible],
#   	if test "$with_sndlib" = yes ; then
# find sndlib.so, check for scm files
#   if not found, make it (or have makefile do it)
# # sndlib really ought to provide sndlib-config and so on
#             SNDLIB_LIB="-lsndlib"
# 	    SNDLIB_FILES=""
# 	fi)
# AC_SUBST(SNDLIB_LIB)
# AC_SUBST(SNDLIB_FILES)


# -------- GTKEXTRA --------
GTKEXTRA_LIBS=""
GTKEXTRA_FLAGS=""

AC_ARG_WITH(gtkextra-prefix,[  --with-gtkextra-prefix=PFX   Prefix where GTKEXTRA is installed (optional)],
            gtkextra_config_prefix="$withval", gtkextra_config_prefix="")

AC_ARG_WITH(gtk-extra,
	[  --with-gtk-extra	  use Gtk+extra's file selection box (requires libgtkextra)],
	[if test "$with_gtk_extra" = yes ; then
          GTKEXTRA_CONFIG=gtkextra-config
          if test x$gtkextra_config_prefix != x ; then
            GTKEXTRA_CONFIG=$gtkextra_config_prefix/bin/gtkextra-config
          fi
	  AC_PATH_PROG(GTKEXTRA_CONFIG,gtkextra-config,no)
          if test "$GTKEXTRA_CONFIG" != "no" ; then
	    AC_DEFINE(HAVE_GTKEXTRA)
            GTKEXTRA_LIBS="`$GTKEXTRA_CONFIG --libs`"
            GTKEXTRA_FLAGS="`$GTKEXTRA_CONFIG --cflags`"
            gtkextra_version="`$GTKEXTRA_CONFIG --version`"
            AC_DEFINE_UNQUOTED(GTKEXTRA_VERSION,"${gtkextra_version}")
            AC_CHECK_LIB(gtkextra, gtk_icon_file_selection_show_tree, [AC_DEFINE(HAVE_GTK_ICON_FILE_SELECTION_SHOW_TREE)], , $GTKEXTRA_LIBS)
          else
	    AC_MSG_WARN([can't find libgtkextra!])
	  fi
	fi])
AC_SUBST(GTKEXTRA_LIBS)
AC_SUBST(GTKEXTRA_FLAGS)


AC_ARG_WITH(snd-as-widget,
	[  --with-snd-as-widget	  make Snd a loadable widget, not a standalone program],
  	if test "$with_snd_as_widget" = yes ; then
	    AC_DEFINE(SND_AS_WIDGET)
	    MAKE_TARGET=widget
	fi)

AC_ARG_WITH(float-samples,
	[  --with-float-samples	  use floats as the internal sample respresentation],
  	if test "$with_float_samples" = yes ; then
	    AC_DEFINE(SNDLIB_USE_FLOATS)
	fi)

AC_ARG_WITH(sample-width,
	[  --with-sample-width=N   use N bits of samples],
	AC_MSG_RESULT(Using $with_sample_width bit samples)
	AC_DEFINE_UNQUOTED(MUS_SAMPLE_BITS,$with_sample_width)
	)

AC_ARG_ENABLE(snd-debug,
  [  --enable-snd-debug      include internal Snd debugging functions],
  if test "$enable_snd_debug" = yes ; then
    AC_DEFINE(DEBUGGING)
    AC_DEFINE(DEBUG_MEMORY)
  fi)


AC_ARG_WITH(ladspa,
	[  --with-ladspa	  	  include support for LADSPA plugins],
  	if test "$with_ladspa" = yes ; then
	    AC_DEFINE(HAVE_LADSPA)
	fi)


#--------------------------------------------------------------------------------
# standard libraries, header files, functions, OSS special cases
#--------------------------------------------------------------------------------

AC_PROG_INSTALL

AC_CHECK_LIB(m,main)
AC_CHECK_LIB(c,main)
AC_CHECK_LIB(dl,main)
LIBS=""

AC_HEADER_DIRENT
AC_HEADER_STDC
AC_CHECK_HEADERS(fcntl.h limits.h unistd.h string.h sys/soundcard.h soundcard.h machine/soundcard.h gsl/gsl_version.h)
AC_CHECK_HEADERS(libc.h sys/vfs.h sys/statfs.h sys/mount.h setjmp.h sched.h pwd.h ladspa.h locale.h sys/fpu.h dlfcn.h sys/param.h)
AC_CHECK_HEADER(/usr/local/lib/oss/include/sys/soundcard.h,[AC_DEFINE(USR_LOCAL_LIB_OSS)])
AC_CHECK_HEADER(/usr/lib/oss/include/sys/soundcard.h,[AC_DEFINE(USR_LIB_OSS)])
AC_CHECK_HEADER(/opt/oss/include/sys/soundcard.h,[AC_DEFINE(OPT_OSS)])
AC_CHECK_HEADER(/var/lib/oss/include/sys/soundcard.h,[AC_DEFINE(VAR_LIB_OSS)])
AC_CHECK_HEADER(sys/sam9407.h,[AC_DEFINE(HAVE_SAM_9407)])
AC_CHECK_HEADER(gnu/libc-version.h,[AC_DEFINE(HAVE_GNU_LIBC_VERSION_H)])
AC_CHECK_HEADER(alsa/asoundlib.h,[AC_DEFINE(HAVE_ALSA_ASOUNDLIB_H)])
AC_HEADER_TIME

AC_CHECK_HEADER(execinfo.h, [AC_DEFINE(HAVE_EXECINFO_H)
                             LDFLAGS="$LDFLAGS -rdynamic"])

AC_C_CONST
AC_TYPE_MODE_T
AC_TYPE_SIZE_T
AC_TYPE_PID_T
AC_STRUCT_TM
AC_C_BIGENDIAN

AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(char)
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(int *)
AC_CHECK_SIZEOF(float)
AC_CHECK_SIZEOF(void *)

dnl this taken from Guile's configure.in
AC_CACHE_CHECK([for long longs], snd_cv_long_longs,
	       AC_TRY_COMPILE(,
			      [long long a],
			      snd_cv_long_longs=yes,
			      snd_cv_long_longs=no))
if test "$snd_cv_long_longs" = yes; then
  AC_DEFINE(HAVE_LLONGS)
fi

dnl from gsl's configure.in
AC_CHECK_FUNCS(isnan, ,[
    AC_MSG_CHECKING([for isnan with <math.h>])
    AC_TRY_LINK([#include <math.h>], [float f = 0.0; isnan(f)],[
        AC_MSG_RESULT(yes)
        AC_DEFINE(HAVE_ISNAN)],
        AC_MSG_RESULT(no))])

AC_TYPE_SIGNAL
AC_FUNC_STRFTIME
AC_CHECK_FUNCS(getcwd strerror readlink setlocale access opendir sleep signal strdup statfs clock vsnprintf snprintf)
AC_FUNC_CLOSEDIR_VOID
AC_FUNC_VPRINTF

# AC_CHECK_STRUCT_FOR([#include <sys/statfs.h>],statfs,f_bsize)


#--------------------------------------------------------------------------------
# X/Motif/Gtk+
#--------------------------------------------------------------------------------

AC_PATH_XTRA

if test "$no_x" = yes || test "$with_no_gui" = yes ; then

  AC_DEFINE(USE_NO_GUI)
  XLIBS=""
  XFLAGS=""
  GTKHTML_LIB=""
  GTKHTML_FLAGS=""
  XMHTML_LIB=""
  AC_SUBST(XLIBS)
  AC_SUBST(XFLAGS)
  AC_SUBST(GTKHTML_LIB)
  AC_SUBST(GTKHTML_FLAGS)
  AC_SUBST(XMHTML_LIB)
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
	         AC_DEFINE(HAVE_XP)
	         XLIBS="$XLIBS -lXp"
	         TEST_LIBS="$TEST_LIBS -lXp"
	         ], , $TEST_LIBS)

    LIBS=$XLIBS
    CFLAGS=$XFLAGS

    AC_CHECK_LIB(Xm, XmCreateForm, 
	[
	  with_motif=yes
 	  AC_DEFINE(USE_MOTIF)  # for Snd
	  AC_DEFINE(HAVE_MOTIF) # for xm
	  AC_SUBST(XLIBS)
	  AC_SUBST(XFLAGS)
 	  AC_SUBST(XMHTML_LIB)
 	  GTKHTML_LIB=""
 	  GTKHTML_FLAGS=""
	
 	  AC_CHECK_LIB(Xpm, XpmReadFileToPixmap,
            [
  	     XLIBS="$XLIBS -lXpm"
   	     AC_DEFINE(HAVE_XPM)
   	    ], , $TEST_LIBS)

   	  AC_CHECK_LIB(Xpm, XpmGetErrorString, , [AC_DEFINE(NEED_XPM_GET_ERROR_STRING)], , $TEST_LIBS)
   	  AC_CHECK_LIB(Xpm, XpmCreateXpmImageFromPixmap, [AC_DEFINE(HAVE_XPM_CREATE_XPM_IMAGE_FROM_PIXMAP)], , $TEST_LIBS)
	  if test "$with_static_xm" = yes ; then
  	    GX_FILES="XM_O_FILES"
	    AC_DEFINE(HAVE_STATIC_XM)
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
    with_gtk=yes
  fi

  if test "$with_gtk" = yes ; then

# first look for gtk2
    AM_PATH_GTK_2_0(1.3.13,
      [
	AC_DEFINE(HAVE_GTK2)
	with_gtk=yes
	AC_CHECK_LIB(m, gtk_tree_model_get_iter_first, 
			  [AC_DEFINE(HAVE_GTK_TREE_MODEL_GET_ITER_FIRST)
			   AC_DEFINE(HAVE_GTK_TREE_PATH_NEW_FIRST)
      			   AC_DEFINE(HAVE_GTK_FILE_SELECTION_GET_SELECT_MULTIPLE)
      			   AC_DEFINE(HAVE_GTK_FILE_SELECTION_SET_SELECT_MULTIPLE)
      			   AC_DEFINE(HAVE_GTK_FILE_SELECTION_GET_SELECTIONS)
			   ], , $GTK_LIBS)
# using -lm above since -lgtk is not safe (might find older version than the pkg-config version)
#   our version of AM_PATH_GTK_2_0 in aclocal.m4 sets GTK_LIBS before executing this branch
#   These five functions were all new in 1.3.15, so one can stand for all
      ],
      [
	AM_PATH_GTK(1.2.8,
      	[
	  AC_DEFINE(HAVE_GTK_1)
	  with_gtk=yes
	],
        [
      	  AC_MSG_WARN([trouble with gtk -- will try to make Snd without any GUI])
      	  with_gtk=no
      	])
      ])
    if test "$with_gtk" = yes ; then
     	GX_FILES="G_O_FILES"
      	GX_HEADERS="SND_G_HEADERS"
      	AC_SUBST(GTK_LIBS)
      	AC_SUBST(GTK_CFLAGS)
      	AC_DEFINE(USE_GTK)
      	AC_SUBST(GTKHTML_LIB)
      	AC_SUBST(GTKHTML_FLAGS)
	XLIBS=""
      	XFLAGS=""
      	XMHTML_LIB=""
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
  GTKHTML_LIB=""
  GTKHTML_FLAGS=""
  XMHTML_LIB=""
  AC_SUBST(XLIBS)
  AC_SUBST(XFLAGS)
  AC_SUBST(GTKHTML_LIB)
  AC_SUBST(GTKHTML_FLAGS)
  AC_SUBST(XMHTML_LIB)
  GX_FILES="NO_GUI_O_FILES"
  GX_HEADERS="NO_GUI_HEADERS"
  fi

fi

AC_SUBST(GX_FILES)
AC_SUBST(GX_HEADERS)


#--------------------------------------------------------------------------------
# Guile
#--------------------------------------------------------------------------------

if test "$with_ruby" != yes ; then

GUILE_LIBS=""
GUILE_CFLAGS=""
GUILE_CONFIG_path=""
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
  if (${GUILE_CONFIG_path}guile-config link > /dev/null) 2>&1; then
    GUILE_CONFIG_works=yes
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
    GUILE_LIBS="`${GUILE_CONFIG_path}guile-config link`"
  fi

  guile_version="`${GUILE_CONFIG_path}guile -c '(display (version))'`"
  AC_MSG_RESULT($guile_version)

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

    OLD_LIBS="$LIBS"
    LIBS="$GUILE_LIBS"
    OLD_CFLAGS="$CFLAGS"
    CFLAGS="$GUILE_CFLAGS"
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
    AC_CHECK_LIB(guile, scm_gc_malloc,          [AC_DEFINE(HAVE_SCM_GC_MALLOC)])
# scm_gc_malloc currently unused
    AC_CHECK_TYPE(scm_t_catch_body,             [AC_DEFINE(HAVE_SCM_T_CATCH_BODY)], , [#include <guile/gh.h>])
    LIBS="$OLD_LIBS"
    CFLAGS="$OLD_CFLAGS"

    if test "`${GUILE_CONFIG_path}guile -c '(display (string<=? (version) "1.3.4"))'`" = "#t"; then
      echo found old out-of-date Guile library
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

        if test "$with_esd" = yes ; then
	  AC_CHECK_LIB(esd,main,[
	    AC_CHECK_HEADER(esd.h,
		 [audio_system=ESD],
		 [AC_MSG_WARN(can't find the ESD header files)])],
            [AC_MSG_WARN(can't find the ESD library)])
        fi

	case $audio_system in
	    ALSA)
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
                AC_TRY_COMPILE(
                 [#include <alsa/asoundlib.h>],
	         [snd_config_t *c;
  	          const char *id;
	          snd_config_get_id(c, &id)
		 ],
	        snd_config_get_id_args=2)
                AC_MSG_RESULT($snd_config_get_id_args)
                AC_DEFINE_UNQUOTED(SND_CONFIG_GET_ID_ARGS,$snd_config_get_id_args)
		;;
	    ESD)
		AC_DEFINE(ESD)
	        esd_version="`esd-config --version`"
                AC_DEFINE_UNQUOTED(ESD_VERSION,"${esd_version}")
                audiofile_version="`audiofile-config --version`"
                AC_DEFINE_UNQUOTED(AUDIOFILE_VERSION,"${audiofile_version}")
                AUDIO_LIB="`esd-config --libs`"
#               ESD_CFLAGS="`esd-config --cflags`"
		AC_SUBST(AUDIO_LIB)
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
	audio_system=Sun
	LDSO_FLAGS="-G"
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
    *-aix*) 
        AC_DEFINE(AIX)
	audio_system=aix
        ;;
    *-apple-*)
	AC_DEFINE(MAC_OSX)
	audio_system=MacOSX
	AUDIO_LIB="-framework CoreAudio -framework CoreFoundation -framework CoreMIDI"
	AC_SUBST(AUDIO_LIB)
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
AC_TRY_COMPILE(
  [#include <sys/statfs.h>],
  [struct statfs buf; 
   int err,fd; 
   err=statfs(fd,&buf,sizeof(buf),0)
  ],
  statfs_args=4)

if test $statfs_args = 0; then
  AC_TRY_COMPILE(
    [#include <sys/vfs.h>],
    [struct statfs buf; 
     int err,fd; 
     err=statfs(fd,&buf,sizeof(buf),0)
    ],
    statfs_args=4)
fi
if test $statfs_args = 0; then
  AC_TRY_COMPILE(
    [#include <dirent.h>
     #include <sys/vfs.h>
     #include <sys/statfs.h>
    ],
    [struct statfs buf;
     int err,fd; 
     err=statfs(fd,&buf,sizeof(buf),0)
    ],
    statfs_args=4)
fi
if test $statfs_args = 0; then
  AC_TRY_COMPILE(
    [#include <sys/statfs.h>],
    [struct statfs buf; 
     int err,fd; 
     err=statfs(fd,&buf)
    ],
    statfs_args=2)
fi
if test $statfs_args = 0; then
  AC_TRY_COMPILE(
    [#include <sys/vfs.h>],
    [struct statfs buf; 
     int err,fd; 
     err=statfs(fd,&buf)
    ],
    statfs_args=2)
fi
if test $statfs_args = 0; then
  AC_TRY_COMPILE(
    [#include <sys/mount.h>],
    [struct statfs buf; 
     int err,fd;
     err=statfs(fd,&buf)
    ],
    statfs_args=2)
fi
if test $statfs_args = 0; then
  AC_TRY_COMPILE(
    [#include <dirent.h>
     #include <sys/vfs.h>
     #include <sys/statfs.h>
    ],
    [struct statfs buf; 
     int err,fd; 
     err=statfs(fd,&buf)
    ],
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
  AC_TRY_LINK([#include <setjmp.h>],
              [sigjmp_buf hi; sigsetjmp(hi,1)],
              trap_segfault=yes)
  if test $trap_segfault = yes; then
    AC_DEFINE(TRAP_SEGFAULT,1)
  fi
  AC_MSG_RESULT($trap_segfault)
fi

AC_SUBST(MAKE_TARGET)
AC_OUTPUT

dnl perhaps make with-ladspa the default in Linux
