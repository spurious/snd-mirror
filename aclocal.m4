# Configure paths for GTK+
# Owen Taylor     1997-2001
#
#   changed by bil to fit Snd configure better

dnl AM_PATH_GTK_2_0([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND [, MODULES]]]])
dnl Test for GTK+, and define GTK_CFLAGS and GTK_LIBS, if gthread is specified in MODULES, 
dnl pass to pkg-config
dnl
AC_DEFUN(AM_PATH_GTK_2_0,
[dnl 
dnl Get the cflags and libraries from pkg-config
dnl
  
  pkg_config_args=gtk+-2.0
  for module in . $4
  do
      case "$module" in
         gthread) 
             pkg_config_args="$pkg_config_args gthread-2.0"
         ;;
      esac
  done

  no_gtk=""

  AC_PATH_PROG(PKG_CONFIG, pkg-config, no)

  if test x$PKG_CONFIG != xno ; then
    if pkg-config --atleast-pkgconfig-version 0.7 ; then
      :
    else
      echo *** pkg-config too old; version 0.7 or better required.
      no_gtk=yes
      PKG_CONFIG=no
    fi
  else
    no_gtk=yes
  fi

  min_gtk_version=ifelse([$1], ,1.3.3,$1)
  AC_MSG_CHECKING(for GTK+ - version >= $min_gtk_version)

  if test x$PKG_CONFIG != xno ; then
    ## don't try to run the test against uninstalled libtool libs
    if $PKG_CONFIG --uninstalled $pkg_config_args; then
	  echo "Will use uninstalled version of GTK+ found in PKG_CONFIG_PATH"
    fi

    if $PKG_CONFIG --atleast-version $min_gtk_version $pkg_config_args; then
	  :
    else
	  no_gtk=yes
    fi
  fi

  if test x"$no_gtk" = x ; then
    AC_MSG_RESULT(yes)
    GTK_CFLAGS=`$PKG_CONFIG $pkg_config_args --cflags`
    GTK_LIBS=`$PKG_CONFIG $pkg_config_args --libs`
    gtk_config_major_version=`$PKG_CONFIG --modversion gtk+-2.0 | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
    gtk_config_minor_version=`$PKG_CONFIG --modversion gtk+-2.0 | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
    gtk_config_micro_version=`$PKG_CONFIG --modversion gtk+-2.0 | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
    ifelse([$2], , :, [$2])     
    AC_SUBST(GTK_CFLAGS)
    AC_SUBST(GTK_LIBS)
  else
    AC_MSG_RESULT(no)
    ifelse([$3], , :, [$3])
  fi
])


dnl AM_PATH_GTK_3_0([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND [, MODULES]]]])
dnl Test for GTK+, and define GTK_CFLAGS and GTK_LIBS, if gthread is specified in MODULES, 
dnl pass to pkg-config
dnl
AC_DEFUN(AM_PATH_GTK_3_0,
[dnl 
dnl Get the cflags and libraries from pkg-config
dnl
  pkg_config_args=gtk+-3.0
  for module in . $4
  do
      case "$module" in
         gthread) 
             pkg_config_args="$pkg_config_args gthread-3.0"
         ;;
      esac
  done

  no_gtk=""

  AC_PATH_PROG(PKG_CONFIG, pkg-config, no)

  if test x$PKG_CONFIG != xno ; then
    if pkg-config --atleast-pkgconfig-version 0.7 ; then
      :
    else
      echo *** pkg-config too old; version 0.7 or better required.
      no_gtk=yes
      PKG_CONFIG=no
    fi
  else
    no_gtk=yes
  fi

  min_gtk_version=ifelse([$1], ,1.3.3,$1)
  AC_MSG_CHECKING(for GTK+ - version >= $min_gtk_version)

  if test x$PKG_CONFIG != xno ; then
    ## don't try to run the test against uninstalled libtool libs
    if $PKG_CONFIG --uninstalled $pkg_config_args; then
	  echo "Will use uninstalled version of GTK+ found in PKG_CONFIG_PATH"
    fi

    if $PKG_CONFIG --atleast-version $min_gtk_version $pkg_config_args; then
	  :
    else
	  no_gtk=yes
    fi
  fi

  if test x"$no_gtk" = x ; then
    AC_MSG_RESULT(yes)
    GTK_CFLAGS=`$PKG_CONFIG $pkg_config_args --cflags`
    GTK_LIBS=`$PKG_CONFIG $pkg_config_args --libs`
    gtk_config_major_version=`$PKG_CONFIG --modversion gtk+-3.0 | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
    gtk_config_minor_version=`$PKG_CONFIG --modversion gtk+-3.0 | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
    gtk_config_micro_version=`$PKG_CONFIG --modversion gtk+-3.0 | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
    ifelse([$2], , :, [$2])     
    AC_SUBST(GTK_CFLAGS)
    AC_SUBST(GTK_LIBS)
  else
    AC_MSG_RESULT(no)
    ifelse([$3], , :, [$3])
  fi
])


## fth.m4 -- Autoconf macros for configuring FTH   -*- Autoconf -*-

## Copyright (C) 2006 Michael Scholz

## Author: Michael Scholz <scholz-micha@gmx.de>
## Created: Mon Mar 13 17:14:46 CET 2006
## Changed: Thu Mar 23 13:46:43 CET 2006
## Ident: $Id: fth.m4,v 1.1.1.1 2006/03/25 21:29:50 mi-scholz Exp $

## This file is part of FTH.

## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

## Commentary:

# FTH_CHECK_LIB(action-if-found, [action-if-not-found])
# 
# Usage: FTH_CHECK_LIB([AC_DEFINE([HAVE_FORTH])])
#
# Don't quote this macro: [FTH_CHECK_LIB(...)] isn't correct.
# Instead call it FTH_CHECK_LIB(...).
# 
# Six variables will be substituted:
#
# FTH               fth program path         or no
# FTH_VERSION       version string           or ""
# FTH_CFLAGS        -I${prefix}/include/fth  or ""
# FTH_LIBS          -L${prefix}/lib -lfth    or ""
# FTH_HAVE_COMPLEX  yes or no
# FTH_HAVE_RATIO    yes or no

## Code:

# AC_CHECK_LIB was written by David MacKenzie.
# This version is slightly changed to fit to FTH_CHECK_LIB.

AC_DEFUN([fth_AC_CHECK_LIB],
[
  m4_ifval([$3], , [AH_CHECK_LIB([$1])])dnl
  AS_LITERAL_IF([$1],
	        [AS_VAR_PUSHDEF([ac_Lib], [ac_cv_lib_$1_$2])],
	      	[AS_VAR_PUSHDEF([ac_Lib], [ac_cv_lib_$1''_$2])])dnl
  AC_CACHE_CHECK([m4_default([$4], [for $2 in -l$1])], ac_Lib,
  		 [fth_check_lib_save_LIBS=$LIBS
		  LIBS="-l$1 $5 $LIBS"
		  AC_LINK_IFELSE([AC_LANG_CALL([], [$2])],
	       	                 [AS_VAR_SET(ac_Lib, yes)],
				 [AS_VAR_SET(ac_Lib, no)])
		  LIBS=$fth_check_lib_save_LIBS])
  AS_IF([test AS_VAR_GET(ac_Lib) = yes],
        [m4_default([$3], [AC_DEFINE_UNQUOTED(AS_TR_CPP(HAVE_LIB$1)) LIBS="-l$1 $LIBS"])])dnl
  AS_VAR_POPDEF([ac_Lib])dnl
])# fth_AC_CHECK_LIB

AC_DEFUN([FTH_CHECK_LIB],
[
  [AC_PATH_PROG([FTH], [fth], [no])]
  FTH_VERSION=""
  FTH_CFLAGS=""
  FTH_LIBS=""
  FTH_HAVE_COMPLEX=no
  FTH_HAVE_RATIO=no
  AC_MSG_CHECKING([for Forth])
  if test "${FTH}" != no ; then
    FTH_VERSION=`${FTH} --no-init-file --eval .version`
    FTH_CFLAGS=`${FTH} --no-init-file --eval .cflags`
    FTH_LIBS=`${FTH} --no-init-file --eval .libs`
    AC_MSG_RESULT([FTH version ${FTH_VERSION}])
    fth_AC_CHECK_LIB([fth], [fth_make_complex], [FTH_HAVE_COMPLEX=yes],
    			    [whether FTH supports complex numbers], [${FTH_LIBS}])
    fth_AC_CHECK_LIB([fth], [fth_ratio_floor], [FTH_HAVE_RATIO=yes],
    			    [whether FTH supports rational numbers], [${FTH_LIBS}])
    [$1]
  else
    AC_MSG_RESULT([no])
    [$2]
  fi
  AC_SUBST([FTH_VERSION])
  AC_SUBST([FTH_CFLAGS])
  AC_SUBST([FTH_LIBS])
  AC_SUBST([FTH_HAVE_COMPLEX])
  AC_SUBST([FTH_HAVE_RATIO])
])# FTH_CHECK_LIB
	
## fth.m4 ends here



# autoconf 2.62 makes AC_C_BIGENDIAN completely useless, so 
#   here is the 2.61 version (!$%%@#$%) -- 2.63 returned to this I think

# AC_OLD_BIGENDIAN ([ACTION-IF-TRUE], [ACTION-IF-FALSE], [ACTION-IF-UNKNOWN])
# -------------------------------------------------------------------------
AC_DEFUN([AC_OLD_BIGENDIAN],
[AC_CACHE_CHECK(whether byte ordering is bigendian, ac_cv_c_bigendian,
[# See if sys/param.h defines the BYTE_ORDER macro.
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <sys/types.h>
#include <sys/param.h>
],
[#if  ! (defined BYTE_ORDER && defined BIG_ENDIAN && defined LITTLE_ENDIAN \
	&& BYTE_ORDER && BIG_ENDIAN && LITTLE_ENDIAN)
 bogus endian macros
#endif
])],
[# It does; now see whether it defined to BIG_ENDIAN or not.
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <sys/types.h>
#include <sys/param.h>
], [#if BYTE_ORDER != BIG_ENDIAN
 not big endian
#endif
])], [ac_cv_c_bigendian=yes], [ac_cv_c_bigendian=no])],
[# It does not; compile a test program.
AC_RUN_IFELSE(
[AC_LANG_PROGRAM([AC_INCLUDES_DEFAULT], [[
  /* Are we little or big endian?  From Harbison&Steele.  */
  union
  {
    long int l;
    char c[sizeof (long int)];
  } u;
  u.l = 1;
  return u.c[sizeof (long int) - 1] == 1;
]])],
	      [ac_cv_c_bigendian=no],
	      [ac_cv_c_bigendian=yes],
[# try to guess the endianness by grepping values into an object file
  ac_cv_c_bigendian=unknown
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
[[short int ascii_mm[] = { 0x4249, 0x4765, 0x6E44, 0x6961, 0x6E53, 0x7953, 0 };
short int ascii_ii[] = { 0x694C, 0x5454, 0x656C, 0x6E45, 0x6944, 0x6E61, 0 };
void _ascii () { char *s = (char *) ascii_mm; s = (char *) ascii_ii; }
short int ebcdic_ii[] = { 0x89D3, 0xE3E3, 0x8593, 0x95C5, 0x89C4, 0x9581, 0 };
short int ebcdic_mm[] = { 0xC2C9, 0xC785, 0x95C4, 0x8981, 0x95E2, 0xA8E2, 0 };
void _ebcdic () { char *s = (char *) ebcdic_mm; s = (char *) ebcdic_ii; }]],
[[ _ascii (); _ebcdic (); ]])],
[if grep BIGenDianSyS conftest.$ac_objext >/dev/null ; then
  ac_cv_c_bigendian=yes
fi
if grep LiTTleEnDian conftest.$ac_objext >/dev/null ; then
  if test "$ac_cv_c_bigendian" = unknown; then
    ac_cv_c_bigendian=no
  else
    # finding both strings is unlikely to happen, but who knows?
    ac_cv_c_bigendian=unknown
  fi
fi])])])])
case $ac_cv_c_bigendian in
  yes)
    m4_default([$1],
      [AC_DEFINE([WORDS_BIGENDIAN], 1,
	[Define to 1 if your processor stores words with the most significant
	 byte first (like Motorola and SPARC, unlike Intel and VAX).])]) ;;
  no)
    $2 ;;
  *)
    m4_default([$3],
      [AC_MSG_ERROR([unknown endianness
presetting ac_cv_c_bigendian=no (or yes) will help])]) ;;
esac
])# AC_OLD_BIGENDIAN

