# -*- Makefile -*-

# Copyright (C) 2003--2004 Michael Scholz

# Author: Michael Scholz <scholz-micha@gmx.de>
# Created: Tue Sep 23 05:59:33 CEST 2003
# Last: Tue Oct 05 16:14:51 CEST 2004
# Ident: $Id: Makefile,v 1.9 2004/10/05 14:15:36 mike Exp $

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
datadir	= ${prefix}/share
forthdir = ${datadir}/gforth/site-forth

SHELL	= /bin/sh
INSTALL	= install -v -c
CHMOD	= chmod u+w
CP	= cp -p
MKDIR	= mkdir
RM	= rm -frv
TAR	= tar cvfz
TARGET	= gfm.tar.gz
DISTDIR	= gfm
FLIBS	= gfm.fs utils.fs gfm-gens.fs gfm-defs.fs csndlib.fs fsndlib.fs notelist.fs clm-ins.fs
SAMPS	= sndtest.gfm bird.gfm pachelbel.gfm ricercari12.gfm cm-examp.gfm
SOURCES	= Makefile README COPYING ${SAMPS} ${FLIBS}

all:
	@echo \`make install\' to install all \*.fs files in ${forthdir}
	@echo \`make install prefix=/usr/gnu\' to install \*.fs in /usr/gnu/share/gforth/site-forth

install:
	@for f in ${FLIBS}; do ${INSTALL} $$f ${forthdir}; done

uninstall:
	@for f in ${FLIBS}; do ${RM} ${forthdir}/$$f; done

dist:
	${RM} ${DISTDIR}
	${MKDIR} ${DISTDIR}
	${CP} ${SOURCES} ${DISTDIR}/
	${CHMOD} ${DISTDIR}/*
	${TAR} ${TARGET} ${DISTDIR}
	${RM} ${DISTDIR}

# Makefile ends here
