# RPM spec file for Snd

%define prefix  /usr
%define ver     4.10
%define rel     2
%define alsaapi 0.5

# these constants define which binary rpm's are going to be generated,
# you can enable type of gui [motif|gtk] and type of sound driver support
# [oss|alsa]. 

%define build_motif_gui 1
%define build_static_motif_gui 1
%define build_gtk_gui 1

%define build_oss_api 1
%define build_alsa_api 1

Summary:	A sound editor
Name:		snd
Version:	%{ver}
Release:	%{rel}
Copyright:	LGPL
Group:		Applications/Sound
Source:		ftp://ccrma-ftp.stanford.edu/pub/Lisp/snd-4.tar.gz
URL:		http://ccrma-www/CCRMA/Software/snd/snd.html
Vendor:		CCRMA/Music Stanford University
Packager:	Bill Schottstaedt <bil@ccrma.stanford.edu>
Docdir:         %{prefix}/doc
Buildroot:      %{_tmppath}/%{name}-root

%description
Snd is a sound editor modelled loosely after Emacs and an old, sorely-missed
PDP-10 sound editor named Dpysnd. It can accommodate any number of sounds
each with any number of channels, and can be customized and extended
using guile and guile-gtk. Snd is free (LGPL); the code is available via
anonymous ftp at ccrma-ftp.stanford.edu as pub/Lisp/snd-4.tar.gz.


%package oss
Summary:        Motif/OSS version of snd
Group:		Applications/Sound
Requires:       snd == %{ver} snd-guile

%description oss
A version of the Snd editor compiled with the Motif gui (statically
linked), and support for the OSS api sound driver. 

%package alsa-%{alsaapi}
Summary:        Motif/ALSA version of snd
Group:		Applications/Sound
Requires:       snd == %{ver} snd-guile

%description alsa-%{alsaapi}
A version of the Snd editor compiled with the Motif gui (statically
linked), and support for the ALSA 0.5.x api sound driver. 

%package motif-oss
Summary:        Motif/OSS version of snd
Group:		Applications/Sound
Requires:       snd == %{ver} snd-guile

%description motif-oss
A version of the Snd editor compiled with the Motif gui and support for
the OSS api sound driver. You will need to have an implementation of
Motif already installed. 

%package motif-alsa-%{alsaapi}
Summary:        Motif/ALSA version of snd
Group:		Applications/Sound
Requires:       snd == %{ver} snd-guile

%description motif-alsa-%{alsaapi}
A version of the Snd editor compiled with the Motif gui and support for
the ALSA 0.5.x api sound driver. You will need to have an implementation 
of Motif already installed. 

%package gtk-oss
Summary:        Gtk/OSS version of snd
Group:		Applications/Sound
Requires:       snd == %{ver} snd-guile

%description gtk-oss
A version of the Snd editor compiled with the gtk gui and support for
the OSS api sound driver. 

%package gtk-alsa-%{alsaapi}
Summary:        Gtk/ALSA version of snd
Group:		Applications/Sound
Requires:       snd == %{ver} snd-guile snd-guile-gtk

%description gtk-alsa-%{alsaapi}
A version of the Snd editor compiled with the gtk gui and support for
the ALSA api sound driver. 

%package utils-oss
Summary:        OSS version of sndplay and friends
Group:		Applications/Sound
Requires:       snd == %{ver} snd-guile snd-guile-gtk

%description utils-oss
Command line utilities included with the snd sound editor compiled with
support for the OSS api sound driver. 

%package utils-alsa-%{alsaapi}
Summary:        ALSA versions of sndplay and friends
Group:		Applications/Sound
Requires:       snd == %{ver} snd-guile

%description utils-alsa-%{alsaapi}
Command line utilities included with the snd sound editor compiled with
support for the ALSA api sound driver. 

%prep
%setup -n snd-4

%build snd
%ifos Linux
%ifarch i386

%define guilepath PATH=/usr/lib/snd/bin:${PATH}

#--- build OSS api utilities
%if %{build_oss_api}
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
%if %{build_alsa_api}
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
install -m 755 -o 0 -g 0 -d ${RPM_BUILD_ROOT}%{prefix}/bin/
%if %{build_motif_gui} && %{build_oss_api}
    install -m 755 -o 0 -g 0 snd-motif-oss ${RPM_BUILD_ROOT}%{prefix}/bin/
%endif
%if %{build_static_motif_gui} && %{build_oss_api}
    install -m 755 -o 0 -g 0 snd-oss ${RPM_BUILD_ROOT}%{prefix}/bin/
%endif
%if %{build_motif_gui} && %{build_alsa_api}
    install -m 755 -o 0 -g 0 snd-motif-alsa ${RPM_BUILD_ROOT}%{prefix}/bin/
%endif
%if %{build_static_motif_gui} && %{build_alsa_api}
    install -m 755 -o 0 -g 0 snd-alsa ${RPM_BUILD_ROOT}%{prefix}/bin/
%endif
%if %{build_gtk_gui} && %{build_oss_api}
    install -m 755 -o 0 -g 0 snd-gtk-oss ${RPM_BUILD_ROOT}%{prefix}/bin/
%endif
%if %{build_gtk_gui} && %{build_alsa_api}
    install -m 755 -o 0 -g 0 snd-gtk-alsa ${RPM_BUILD_ROOT}%{prefix}/bin/
%endif
%if %{build_oss_api}
    install -m 755 -o 0 -g 0 sndplay-oss sndrecord-oss sndinfo-oss sndsine-oss audinfo-oss ${RPM_BUILD_ROOT}%{prefix}/bin/
%endif
%if %{build_alsa_api}
    install -m 755 -o 0 -g 0 sndplay-alsa sndrecord-alsa sndinfo-alsa sndsine-alsa audinfo-alsa ${RPM_BUILD_ROOT}%{prefix}/bin/
%endif

#--- snd package, just documentation
%files
%defattr(-, root, root)
%doc README.Snd HISTORY.Snd snd.html snd.txt extsnd.html extsnd.txt sndlib.html sndlib.txt grfsnd.html grfsnd.txt clm.html
%doc title.png controls.png auto.png files.png regions.png mixer.png rec.png reclin.png hfft.png energy.png s.png n.png d.png env.png bgd.png note.png
%doc bell.scm glfft.scm loop.scm old-sndlib2scm.scm zip.scm env.scm gm.scm marks.scm pqwvox.scm snd-gtk.scm prc95.scm enved.scm
%doc examp.scm goopsnd.scm mix.scm pvoc.scm snd-test.scm fmv.scm index.scm moog.scm rgb.scm v.scm effects.scm bird.scm ws.scm rubber.scm


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
%if %{build_oss_api}
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
%if %{build_alsa_api}
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


# to make the rpm file (since I keep forgetting):
#   make up-to-date snd-4.tar.gz
#   increment release number above (and in snd.h, README.Snd)
#   su root
#   cd /usr/src/redhat/SOURCES
#   cp -f /<wherever>/snd-4.tar.gz .
#   cd ../SPECS
#   cp -f /<wherever>/snd.spec .
#   rpm -ba snd.spec
#   /usr/src/redhat/BUILD/snd-4 is where the build occurs
#   end result in /usr/src/redhat/RPMS/i386 (binary) and /usr/src/redhat/SRPMS (source)
#   update version references in README.Snd, ftp:LISP.README, web:snd.html, snd.h, sourceforge index.html
#   linux-snd:  makefile.guile-static
#   xlinux-snd: makefile.xlinux
#   sun-snd:    makefile.solaris
