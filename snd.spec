# RPM spec file for Snd

%define prefix  /usr
%define ver     4.12
%define rel     1
%define alsaapi 0.5

# these constants define which binary rpm's are going to be generated,
# you can enable type of gui [motif|gtk] and type of sound driver support
# [oss|alsa] and whether the command line utilities are built as well. 

%define build_motif_gui 1
%define build_static_motif_gui 1
%define build_gtk_gui 1
%define build_utils 1

%define build_oss_api 1
%define build_alsa_api 1

# special cases for requirements: if the distribution you're compiling for
# is named here set that definition to 1 and all others to 0

# SuSE 7.0
#   I installed the DEMO cd and then the following additional packages:
#     motif/motifdev, guile, guilegtk
#     gtk/gtkdev 1.2.8 55
#     glib/glibdev 1.2.8 57
#     alsa/alsadev 0.5.9 20

%define linux_SuSE_7_0 0

# RedHat 7.0
#   Additional packages:
#     openmotif 2.1.30-6 (from the PowerTools cdrom)
#     installed alsa-0.5.10 (driver, lib, utils)
#   We require installation of
#     snd-guile
#     snd-guile-gtk
#     [while out-of-the-box redhat7 comes with 
#      guile 1.3.4 it does not have guilegtk
#      so we would have to install that anyway]

%define linux_RedHat_7_0 1

# set this to 0 if you want to build the spec file with required libraries
# and packages automatically discovered by rpm

%define build_with_AutoReqProvOff 1

# We don't want to use the automatic requirement list built by rpm, that
# creates rpms that are unnecessarily restricted in the version of the 
# different libraries (and even package names) that they can install with.
#
# The following incantation will list all shared libraries a given snd 
# executable is linking with and the rpm packages (if any) they belong to:
# 
# ldd snd | awk '{printf $3 " "; system("rpm -q -f " $3);}' | awk '{print $2 " " $1}' | sort
#
# If you're compiling this spec file for a new distribution, first do a
# compilation with build_with_autoreqs set to 1 , then do an install of
# the packages and finally run the above command line on all installed
# binaries. You can find the name of the binaries with:
#
# rpm -q -a | grep ^snd- | grep -v guile | xargs rpm -q -l | grep /usr/bin/
#
# use the information to build a special case for the requirements, see
# the example for SuSE 7.0 that I created. Let me know so I can add it
# to this spec file <nando@ccrma.stanford.edu>. 
#
# here's a list of what we currently require, the defaults and why:

# alsa: the alsa driver libraries. Hmmm, I guess this should be statically
#       linked so that snd-*-alsa can run in an oss only environment. At
#       this point the alsa version autoswitches into oss if no alsa driver
#       is found (or if the alsa driver reports 0 installed cards). We
#       require alsa 0.5.*

  %define req_alsa libasound.so.1

# guile:    the guile interpreter
# guilegtk: the gtk glue library for guile

  %define req_guile libguile.so.9
  %define req_guilegtk libguilegtk-1.2.so.0

# motif: the motif toolkit, we use openmotif for generating the rpms but
#        any package that provides libXm.so.2 should do. 

  %define req_motif libXm.so.2

# gtk+: the gtk graphics toolkit. There are some known bugs in < 1.2.8
#       but apparently most things should work with > 1.2.6

  %define req_gtk gtk+ >= 1.2.6 glib >= 1.2.6

# glibc: the gnu c library, this will work for gnu libc based on 2.1.

  %define req_glibc glibc < 2.2

# X: the windowing system, I assume any version will do.  
#    These are the individual libs under rh7.0:
#       for gtk:   libX11.so.6 libXext.so.6 libXi.so.6
#       for motif: libICE.so.6 libSM.so.6 libX11.so.6 libXext.so.6 
#                  libXp.so.6 libXpm.so.6 libXt.so.6
# These are the libs under rh6.2:
# libSM.so.6 libICE.so.6 libXt.so.6 libX11.so.6 libXext.so.6 libXp.so.6 xpm

  %define req_x XFree86-libs xpm

##########
# SuSE 7.0
##########

%if %{linux_SuSE_7_0}
  # guile 1.3.4
  %define req_guile libguile.so.6
  # openmotif with broken libXm.so.2 provides
  %define req_motif motif
  # note package name change, gtk instead of gtk+
  %define req_gtk gtk >= 1.2.6 glib >= 1.2.6
  # glibc libraries are here
  %define req_glibc shlibs < 2.2
  # X libraries are here
  %define req_x xshared
%endif

############
# RedHat 7.0
############

%if %{linux_RedHat_7_0}
  # rh7 is based on 2.2
  %define req_glibc glibc = 2.2
  # comes with 1.2.8
  %define req_gtk gtk+ >= 1.2.8 glib >= 1.2.8
  # just require XFree86 libraries, xpm is part of them
  %define req_x XFree86-libs
%endif

# global requirements, to make life easier latter

%define req_X %{req_glibc} %{req_x} 
%define req_X_motif %{req_glibc} %{req_x} %{req_motif}
%define req_X_gtk %{req_glibc} %{req_x} %{req_gtk} %{req_guilegtk}


Summary:	A sound editor
Name:		snd
Version:	%{ver}
Release:	%{rel}
Copyright:	LGPL
Group:		Applications/Sound
Source:		ftp://ccrma-ftp.stanford.edu/pub/Lisp/snd-4.tar.gz
URL:		http://ccrma-www/CCRMA/Software/snd/snd.html
Vendor:		CCRMA/Music Stanford University
Packager:	Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
Buildroot:      %{_tmppath}/%{name}-root
Prefix:         %{prefix}

%description
Snd is a sound editor modelled loosely after Emacs and an old, sorely-missed
PDP-10 sound editor named Dpysnd. It can accomodate any number of sounds
each with any number of channels, and can be customized and extended
using guile and guile-gtk. Snd is free (LGPL); the code is available via
anonymous ftp at ccrma-ftp.stanford.edu as pub/Lisp/snd-4.tar.gz.

%package oss
Summary:        Motif/OSS version of snd
Group:		Applications/Sound
%if %{build_with_AutoReqProvOff}
AutoReqProv:    off
Requires:       snd == %{ver} %{req_X} %{req_guile}
%endif

%description oss
A version of the Snd editor compiled with the Motif gui (statically
linked), and support for the OSS api sound driver. 

%package alsa-%{alsaapi}
Summary:        Motif/ALSA version of snd
Group:		Applications/Sound
%if %{build_with_AutoReqProvOff}
AutoReqProv:    off
Requires:       snd == %{ver} %{req_X} %{req_guile} %{req_alsa}
%endif

%description alsa-%{alsaapi}
A version of the Snd editor compiled with the Motif gui (statically
linked), and support for the ALSA 0.5.x api sound driver. 

%package motif-oss
Summary:        Motif/OSS version of snd
Group:		Applications/Sound
%if %{build_with_AutoReqProvOff}
AutoReqProv:    off
Requires:       snd == %{ver} %{req_X_motif} %{req_guile}
%endif

%description motif-oss
A version of the Snd editor compiled with the Motif gui and support for
the OSS api sound driver. You will need to have an implementation of
Motif already installed. 

%package motif-alsa-%{alsaapi}
Summary:        Motif/ALSA version of snd
Group:		Applications/Sound
%if %{build_with_AutoReqProvOff}
AutoReqProv:    off
Requires:       snd == %{ver} %{req_X_motif} %{req_guile} %{req_alsa}
%endif

%description motif-alsa-%{alsaapi}
A version of the Snd editor compiled with the Motif gui and support for
the ALSA 0.5.x api sound driver. You will need to have an implementation 
of Motif already installed. 

%package gtk-oss
Summary:        Gtk/OSS version of snd
Group:		Applications/Sound
%if %{build_with_AutoReqProvOff}
AutoReqProv:    off
Requires:       snd == %{ver} %{req_X_gtk} %{req_guile}
%endif

%description gtk-oss
A version of the Snd editor compiled with the gtk gui and support for
the OSS api sound driver. 

%package gtk-alsa-%{alsaapi}
Summary:        Gtk/ALSA version of snd
Group:		Applications/Sound
%if %{build_with_AutoReqProvOff}
AutoReqProv:    off
Requires:       snd == %{ver} %{req_X_gtk} %{req_guile} %{req_alsa}
%endif

%description gtk-alsa-%{alsaapi}
A version of the Snd editor compiled with the gtk gui and support for
the ALSA api sound driver. 

%package utils-oss
Summary:        OSS version of sndplay and friends
Group:		Applications/Sound
%if %{build_with_AutoReqProvOff}
AutoReqProv:    off
Requires:       snd == %{ver} %{req_glibc}
%endif

%description utils-oss
Command line utilities included with the snd sound editor compiled with
support for the OSS api sound driver. 

%package utils-alsa-%{alsaapi}
Summary:        ALSA versions of sndplay and friends
Group:		Applications/Sound
%if %{build_with_AutoReqProvOff}
AutoReqProv:    off
Requires:       snd == %{ver} %{req_glibc} %{req_alsa}
%endif

%description utils-alsa-%{alsaapi}
Command line utilities included with the snd sound editor compiled with
support for the ALSA api sound driver. 

%prep
%setup -n snd-4

%build snd
%ifos Linux
%ifarch i386 i586 i686

# always prepend /usr/lib/snd/bin to the search path
%define guilepath PATH=/usr/lib/snd/bin:${PATH}

#--- build OSS api utilities
%if %{build_utils} && %{build_oss_api}
    %{guilepath} ./configure --prefix=%{prefix} 
    make sndplay sndrecord sndinfo sndsine audinfo
    mv sndplay sndplay-oss
    mv sndrecord sndrecord-oss
    mv sndinfo sndinfo-oss
    mv sndsine sndsine-oss
    mv audinfo audinfo-oss
    make clean
    rm -f config.cache
%endif

#--- build Motif/OSS version
%if %{build_motif_gui} && %{build_oss_api}
    %{guilepath} ./configure --prefix=%{prefix} 
    make
    mv snd snd-motif-oss
    make clean
    rm -f config.cache
%endif

#--- build static Motif/OSS version
%if %{build_static_motif_gui} && %{build_oss_api}
    %{guilepath} ./configure --prefix=%{prefix} --with-static-motif=yes
    make
    mv snd snd-oss
    make clean
    rm -f config.cache
%endif

#--- build ALSA api utilities
%if %{build_utils} && %{build_alsa_api}
    %{guilepath} ./configure --prefix=%{prefix} --with-alsa=yes
    make sndplay sndrecord sndinfo sndsine audinfo
    mv sndplay sndplay-alsa
    mv sndrecord sndrecord-alsa
    mv sndinfo sndinfo-alsa
    mv sndsine sndsine-alsa
    mv audinfo audinfo-alsa
    make clean
    rm -f config.cache
%endif

#--- build Motif/ALSA version
%if %{build_motif_gui} && %{build_alsa_api}
    %{guilepath} ./configure --prefix=%{prefix} --with-alsa=yes
    make
    mv snd snd-motif-alsa
    make clean
    rm -f config.cache
%endif

#--- build static Motif/ALSA version
%if %{build_static_motif_gui} && %{build_alsa_api}
    %{guilepath} ./configure --prefix=%{prefix} --with-alsa=yes --with-static-motif=yes
    make
    mv snd snd-alsa
    make clean
    rm -f config.cache
%endif

#--- build Gtk/OSS version
%if %{build_gtk_gui} && %{build_oss_api} 
    %{guilepath} ./configure --prefix=%{prefix} --with-gtk=yes
    make
    mv snd snd-gtk-oss
    make clean
    rm -f config.cache
%endif

#--- build Gtk/ALSA version
%if %{build_gtk_gui} && %{build_alsa_api}
    %{guilepath} ./configure --prefix=%{prefix} --with-gtk=yes --with-alsa=yes
    make
    mv snd snd-gtk-alsa
    make clean
    rm -f config.cache
%endif

%else
make -f makefile.ppcrpm
%endif
%endif

%install snd
rm -rf ${RPM_BUILD_ROOT}
install -m 755 -d ${RPM_BUILD_ROOT}%{prefix}/bin/
%if %{build_motif_gui} && %{build_oss_api}
    install -m 755 snd-motif-oss ${RPM_BUILD_ROOT}%{prefix}/bin/
%endif
%if %{build_static_motif_gui} && %{build_oss_api}
    install -m 755 snd-oss ${RPM_BUILD_ROOT}%{prefix}/bin/
%endif
%if %{build_motif_gui} && %{build_alsa_api}
    install -m 755 snd-motif-alsa ${RPM_BUILD_ROOT}%{prefix}/bin/
%endif
%if %{build_static_motif_gui} && %{build_alsa_api}
    install -m 755 snd-alsa ${RPM_BUILD_ROOT}%{prefix}/bin/
%endif
%if %{build_gtk_gui} && %{build_oss_api}
    install -m 755 snd-gtk-oss ${RPM_BUILD_ROOT}%{prefix}/bin/
%endif
%if %{build_gtk_gui} && %{build_alsa_api}
    install -m 755 snd-gtk-alsa ${RPM_BUILD_ROOT}%{prefix}/bin/
%endif
%if %{build_utils} && %{build_oss_api}
    install -m 755 sndplay-oss sndrecord-oss sndinfo-oss sndsine-oss audinfo-oss ${RPM_BUILD_ROOT}%{prefix}/bin/
%endif
%if %{build_utils} && %{build_alsa_api}
    install -m 755 sndplay-alsa sndrecord-alsa sndinfo-alsa sndsine-alsa audinfo-alsa ${RPM_BUILD_ROOT}%{prefix}/bin/
%endif

%clean
rm -rf ${RPM_BUILD_ROOT}

#--- snd package, just documentation
%files
%defattr(-, root, root)
%doc README.Snd HISTORY.Snd Snd.ad *.html
%doc *.png
%doc *.scm

#--- snd-oss (motif statically linked)
%if %{build_static_motif_gui} && %{build_oss_api} 
%post oss
if [ -L %{prefix}/bin/snd ] ; then
    rm -f %{prefix}/bin/snd
fi
if [ ! -e %{prefix}/bin/snd ] ; then
    ln -s %{prefix}/bin/snd-oss %{prefix}/bin/snd
fi
%postun oss
if [ -L %{prefix}/bin/snd ] ; then
    link=`ls -l %{prefix}/bin/snd | awk '{print $11}'`
    if [ `basename ${link}` = "snd-oss" ] ; then
	rm -f %{prefix}/bin/snd
    fi
fi
%files oss
%defattr(-, root, root)
%{prefix}/bin/snd-oss
%endif

#--- snd-motif-oss (motif dynamically linked)
%if %{build_motif_gui} && %{build_oss_api} 
%post motif-oss
if [ -L %{prefix}/bin/snd ] ; then
    rm -f %{prefix}/bin/snd
fi
if [ ! -e %{prefix}/bin/snd ] ; then
    ln -s %{prefix}/bin/snd-motif-oss %{prefix}/bin/snd
fi
%postun motif-oss
if [ -L %{prefix}/bin/snd ] ; then
    link=`ls -l %{prefix}/bin/snd | awk '{print $11}'`
    if [ `basename ${link}` = "snd-motif-oss" ] ; then
	rm -f %{prefix}/bin/snd
    fi
fi
%files motif-oss
%defattr(-, root, root)
%{prefix}/bin/snd-motif-oss
%endif

#--- snd-alsa (motif statically linked)
%if %{build_static_motif_gui} && %{build_alsa_api}
%post alsa-%{alsaapi}
if [ -L %{prefix}/bin/snd ] ; then
    rm -f %{prefix}/bin/snd
fi
if [ ! -e %{prefix}/bin/snd ] ; then
    ln -s %{prefix}/bin/snd-alsa %{prefix}/bin/snd
fi
%postun alsa-%{alsaapi}
if [ -L %{prefix}/bin/snd ] ; then
    link=`ls -l %{prefix}/bin/snd | awk '{print $11}'`
    if [ `basename ${link}` = "snd-alsa" ] ; then
	rm -f %{prefix}/bin/snd
    fi
fi
%files alsa-%{alsaapi}
%defattr(-, root, root)
%{prefix}/bin/snd-alsa
%endif

#--- snd-motif-alsa (motif dynamically linked)
%if %{build_motif_gui} && %{build_alsa_api}
%post motif-alsa-%{alsaapi}
if [ -L %{prefix}/bin/snd ] ; then
    rm -f %{prefix}/bin/snd
fi
if [ ! -e %{prefix}/bin/snd ] ; then
    ln -s %{prefix}/bin/snd-motif-alsa %{prefix}/bin/snd
fi
%postun motif-alsa-%{alsaapi}
if [ -L %{prefix}/bin/snd ] ; then
    link=`ls -l %{prefix}/bin/snd | awk '{print $11}'`
    if [ `basename ${link}` = "snd-motif-alsa" ] ; then
	rm -f %{prefix}/bin/snd
    fi
fi
%files motif-alsa-%{alsaapi}
%defattr(-, root, root)
%{prefix}/bin/snd-motif-alsa
%endif

#--- snd-gtk-oss
%if %{build_gtk_gui} && %{build_oss_api} 
%post gtk-oss
if [ -L %{prefix}/bin/snd ] ; then
    rm -f %{prefix}/bin/snd
fi
if [ ! -e %{prefix}/bin/snd ] ; then
    ln -s %{prefix}/bin/snd-gtk-oss %{prefix}/bin/snd
fi
%postun gtk-oss
if [ -L %{prefix}/bin/snd ] ; then
    link=`ls -l %{prefix}/bin/snd | awk '{print $11}'`
    if [ `basename ${link}` = "snd-gtk-oss" ] ; then
	rm -f %{prefix}/bin/snd
    fi
fi
%files gtk-oss
%defattr(-, root, root)
%{prefix}/bin/snd-gtk-oss
%endif

#--- snd-gtk-alsa
%if %{build_gtk_gui} && %{build_alsa_api} 
%post gtk-alsa-%{alsaapi}
if [ -L %{prefix}/bin/snd ] ; then
    rm -f %{prefix}/bin/snd
fi
if [ ! -e %{prefix}/bin/snd ] ; then
    ln -s %{prefix}/bin/snd-gtk-alsa %{prefix}/bin/snd
fi
%postun gtk-alsa-%{alsaapi}
if [ -L %{prefix}/bin/snd ] ; then
    link=`ls -l %{prefix}/bin/snd | awk '{print $11}'`
    if [ `basename ${link}` = "snd-gtk-alsa" ] ; then
	rm -f %{prefix}/bin/snd
    fi
fi
%files gtk-alsa-%{alsaapi}
%defattr(-, root, root)
%{prefix}/bin/snd-gtk-alsa
%endif

#--- snd-utils-oss
%if %{build_utils} && %{build_oss_api}
%post utils-oss
for util in sndplay sndrecord sndinfo sndsine audinfo
do 
    if [ -L %{prefix}/bin/${util} ] ; then
	rm -f %{prefix}/bin/${util}
    fi
    if [ ! -e %{prefix}/bin/${util} ] ; then
	ln -s %{prefix}/bin/${util}-oss %{prefix}/bin/${util}
    fi
done
%postun utils-oss
for util in sndplay sndrecord sndinfo sndsine audinfo
do 
    if [ -L %{prefix}/bin/${util} ] ; then
	link=`ls -l %{prefix}/bin/${util} | awk '{print $11}'`
	if [ `basename ${link}` = "${util}-oss" ] ; then
	    rm -f %{prefix}/bin/${util}
	fi
    fi
done
%files utils-oss
%defattr(-, root, root)
%{prefix}/bin/sndplay-oss
%{prefix}/bin/sndrecord-oss
%{prefix}/bin/sndinfo-oss
%{prefix}/bin/sndsine-oss
%{prefix}/bin/audinfo-oss
%endif

#--- snd-utils-alsa
%if %{build_utils} && %{build_alsa_api}
%post utils-alsa-%{alsaapi}
for util in sndplay sndrecord sndinfo sndsine audinfo
do 
    if [ -L %{prefix}/bin/${util} ] ; then
	rm -f %{prefix}/bin/${util}
    fi
    if [ ! -e %{prefix}/bin/${util} ] ; then
	ln -s %{prefix}/bin/${util}-alsa %{prefix}/bin/${util}
    fi
done
%postun utils-alsa-%{alsaapi}
for util in sndplay sndrecord sndinfo sndsine audinfo
do 
    if [ -L %{prefix}/bin/${util} ] ; then
	link=`ls -l %{prefix}/bin/${util} | awk '{print $11}'`
	if [ `basename ${link}` = "${util}-alsa" ] ; then
	    rm -f %{prefix}/bin/${util}
	fi
    fi
done
%files utils-alsa-%{alsaapi}
%defattr(-, root, root)
%{prefix}/bin/sndplay-alsa
%{prefix}/bin/sndrecord-alsa
%{prefix}/bin/sndinfo-alsa
%{prefix}/bin/sndsine-alsa
%{prefix}/bin/audinfo-alsa
%endif

%changelog
* Wed Apr 02 2001 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
- compiled for rh7, snd-4.tar.gz, version 4.12, dated 4/2/2001
* Wed Mar 07 2001 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
- compiled for rh7, snd-4.tar.gz, version 4.11, dated 3/7/2001
* Tue Jan 23 2001 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
- compiled for rh7, suse7.0 and snd-4.tar.gz, version 4.10, dated 1/23/2001
* Wed Jan 17 2001 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
- added support for redhat 7 and finished up support for SuSE 7
* Mon Jan 15 2001 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
- redid the dependencies stuff yet again
- updated to new snd-4.tar.gz of Jan 15 2001
* Sat Jan 13 2001 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
- compiled for snd-4.10 of Jan 13 2000, release set to 1
- patched snd-1.h, added ps_fd field to chan_info structure
- redid the dependencies for all packages. I'm now setting the 
  dependencies manually and trying to be as general as possible 
  so that the packages will run in as many distributions as possible. 
  See the detailed explanation and %defines at the beginning of the 
  spec file. 
- deleted the %{usesndguile} option. I now always prefix the path to 
  search for guile with /usr/lib/snd/bin/. The library will be found
  there at compile time but as the requirements are location independent
  it will be also looked for elsewhere at run time. Thus a distribution
  that comes with the latest guile will meet the requirement and the 
  rpm will install. If snd-guile is not installed or the version of
  guile in the distribution is less than 1.4 the install will fail. 
- added %{build_utils} to enable or not compilation of the command
  line utilities
* Mon Dec 11 2000 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
- compiled for snd-4.9 of Dec 11 2000, release set to 1
- added %{prefix}, removed ${RPM_BUILD_ROOT} prior to install, remove
  -o and -g from the install lines, thanks to Volker Kuhlmann for the tips
- use wildcards in doc file list, added Snd.ad
- added %{usesndguile} option for enabling the search for a private guile
  library
* Mon Nov 20 2000 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
- compiled for snd-4.8 of Nov 20 2000, release set to 1
- fixed dependencies, utils packages do not depend on guile-gtk, 
  only gtk-* do (thanks to Volker Kuhlmann <kuhlmav@elec.canterbury.ac.nz>)
* Mon Oct 16 2000 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
- compiled for snd-4.7, release set to 1
* Thu Sep 28 2000 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
- compiled for snd-4.6 of Sep 28 2000, release set to 1
* Fri Aug 25 2000 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
- added statically linked versions of the motif gui packages
- compiled using snd-4.tar.gz Aug 25 2000
* Mon Aug 21 2000 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
- changed postun to only erase link to binaries if the package being
  installed is the one that set up the link in the first place
- added alsa version number to naming of packages, that will enable
  us to have packages for alsa 0.5.x and 0.6.x in the same directory
* Wed Aug 16 2000 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
- no need to hack configure.in, a simple PATH added to the ./configure
  stage makes sure that the proper guile binary is found...
* Tue Aug 15 2000 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
- created subpackages snd-motif-oss snd-motif-alsa snd-gtk-oss
  snd-gtk-alsa snd-utils-oss snd-utils-alsa
- changed configure.in in snd-4.5 to search for guile first in 
  /usr/lib/snd/, added -rpath to the linker invocation so that 
  it is not necessary to add /usr/lib/snd/lib to /etc/ld.so.conf
- TODO: create a snd-devel package with the include files and sndlib?
* Mon Aug 14 2000 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
- added a root install directory so that compiling the package
  does not clobber the real filesystem
- changed prefix to represent everything up to bin|lib|whatever
  (so that prefix=/usr installs in /usr/bin, follow redhat convention)
- changed version numbering, 4.5 refers to the version of the program,
  the release number is the rpm package release number, the .tar.gz 
  file should be named snd-4.5.tar.gz...

