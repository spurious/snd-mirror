# -*- Makefile -*-

# Copyright (C) 2003--2005 Michael Scholz

# Author: Michael Scholz <scholz-micha@gmx.de>
# Created: Tue Sep 23 05:59:33 CEST 2003
# Last: Thu Jan 13 23:37:34 CET 2005
# Ident: $Id: Makefile,v 1.20 2005/01/13 22:39:47 mike Exp $

# This file is part of GFM Gforth Music.

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of
# the License, or (at your option) any later version.

# This program is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied
# warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
# PURPOSE.  See the GNU General Public License for more details.

# You should have received a copy of the GNU General Public
# License along with this program; if not, write to the Free
# Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA 02111-1307 USA

prefix	= /usr/local
forthdir= ${prefix}/share/gforth/site-forth
gfmdir  = ${forthdir}/gfm

SHELL	= /bin/sh
INSTALL	= install -v -c
CHMOD	= chmod u+w
CP	= cp -p
RM	= rm -fr
TAR	= tar cvfz
INSTDIRS= mkinstalldirs
TARGET	= gfm.tar.gz
DISTDIR	= gfm
SPECIAL = gfm.fs utils.fs gfmpath.fs
FLIBS	= gfm-defs.fs csndlib.fs fsndlib.fs clm-ins.fs spectr.fs dlocsig.fs
SAMPS	= sndtest.gfm bird.gfm cm-examp.gfm fmviolin.gfm
SOURCES	= Makefile README COPYING ${SPECIAL} ${SAMPS} ${FLIBS} mkinstalldirs

all:
	@echo \`make install\' to install all \*.fs files in ${gfmdir}
	@echo \`make install prefix=/usr/gnu\' to install \*.fs in /usr/gnu/share/gforth/site-forth/gfm

install:
	${INSTDIRS} ${forthdir}
	${INSTDIRS} ${gfmdir}
	@echo "fpath path+ ${gfmdir}" > gfmpath.fs
	@for f in ${SPECIAL}; do ${INSTALL} $$f ${forthdir}; done
	@for f in ${FLIBS}; do ${INSTALL} $$f ${gfmdir}; done

uninstall:
	@for f in ${SPECIAL}; do ${RM} ${forthdir}/$$f; done
	${RM} ${gfmdir}

dist:
	${RM} ${DISTDIR}
	${INSTDIRS} ${DISTDIR}
	${CP} ${SOURCES} ${DISTDIR}/
	${CHMOD} ${DISTDIR}/*
	${RM} ${DISTDIR}/gfmpath.fs
	touch ${DISTDIR}/gfmpath.fs
	${TAR} ${TARGET} ${DISTDIR}
	${RM} ${DISTDIR}

# Makefile ends here
