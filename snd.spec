%define ver      7.11
%define rel      1
%define tarname  snd-7
%define snd_date "03/18/2005"

%define	desktop_vendor planetccrma
%define desktop_utils  %(test -x /usr/bin/desktop-file-install && echo "yes")

# gui to build
%define build_motif 1
%define build_gtk   1

# path to shared data
%define sndlib %{_libdir}/snd

# at ccrma snd lives in the shared server space, not in each
# individual machine, but I still want to install the icon,
# menu entry and docs in every machine. Set to 1 when building
# a version with a custom /etc/snd.conf and no builtin scheme
# code
%define build_for_ccrma 0

# standard configuration options
%define config_options --prefix=%{_prefix} --with-guile=yes --with-run --with-alsa --with-jack --with-midi --with-ladspa --with-gl

Summary:	A sound editor (%{ver}, %{snd_date})
Name:		snd
Version:	%{ver}
Release:        %{rel}
Copyright:	LGPL
Group:		Applications/Multimedia
Source:		ftp://ccrma-ftp.stanford.edu/pub/Lisp/%{tarname}.tar.gz
Source1:        snd.png
Source100:      snd-specs.tar.gz
Patch0:         snd-7.11-midi.patch
URL:		http://www-ccrma.stanford.edu/software/snd/
Vendor:		CCRMA/Music Stanford University
Buildroot:      %{_tmppath}/%{name}-%{version}-root
Packager:	Fernando Lopez-Lezcano
Distribution:   Planet CCRMA

BuildRequires:  XFree86-devel, gettext, guile-devel, gsl-devel, fftw3-devel
BuildRequires:  alsa-lib-devel, libsamplerate-devel, ladspa-devel
BuildRequires:  jack-audio-connection-kit-devel
%if "%{build_motif}" == "1"
BuildRequires:  openmotif-devel
%endif
%if "%{build_gtk}" == "1"
BuildRequires:  gcc-c++ gtk2-devel gtkglext-devel
%endif

%description
Snd is a sound editor modelled loosely after Emacs and an old, sorely-missed
PDP-10 sound editor named Dpysnd. It can accomodate any number of sounds
each with any number of channels, and can be customized and extended
using guile and guile-gtk. Snd is free (LGPL); the code is available via
anonymous ftp at ccrma-ftp.stanford.edu as pub/Lisp/%{tarname}.tar.gz.

This package contains snd version %{ver}, dated %{snd_date}.

%package motif
Summary:        Motif version of snd (%{ver}, %{snd_date})
Group:		Applications/Multimedia
Requires:       snd == %{version}-%{release}
Requires:       openmotif
%{?rh73:Obsoletes: snd-guile}
%{?rh80:Obsoletes: snd-guile}

%description motif
A version of the Snd editor (%{ver} of %{snd_date}) compiled with the
Motif gui.

%package gtk
Summary:        Gtk version of snd (%{ver}, %{snd_date})
Group:		Applications/Multimedia
Requires:       snd == %{version}-%{release}
%{?rh73:Obsoletes: snd-guile}
%{?rh80:Obsoletes: snd-guile}

%description gtk
A version of the Snd editor (%{ver} of %{snd_date}) compiled with the gtk
gui.

%package utils
Summary:        Sndplay and friends (Snd %{ver}, %{snd_date})
Group:		Applications/Multimedia
# always conflict with the (old) sndplay packages from cm/clm/cmn
Conflicts:      sndplay-oss sndplay

%description utils
Command line utilities included with the snd sound editor (%{ver} of
%{snd_date}).

%package -n sndlib
Summary:        Soundfile and audio handler collection
Group:		Applications/Multimedia

%description -n sndlib
The sound library is a collection of sound file and audio hardware
handlers written in C and running currently on SGI (either audio
library), Sun, OSS or ALSA (Linux and others), Mac, Mac OSX, HPUX,
LinuxPPC, and Windoze systems. It provides relatively straightforward
access to many sound file headers and data types, and most of the
features of the audio hardware.

%prep
%setup -q -n %{tarname}
# %patch0 -p1

%build
# alsa build is failing, hack it...
export CFLAGS="-DSND_CONFIG_GET_ID_ARGS=2 $CFLAGS"

# build Motif version
%if %{build_motif}
    ./configure %{config_options} --with-static-xm=yes
    # hack to fix detection of alsa
#    perl -p -i -e "s|/\* \#undef HAVE_ALSA \*/|\#define HAVE_ALSA 1|g" config.h
    # hack to add alsa library to link stage
#    perl -p -i -e "s|-ljack|-lasound -ljack|g" makefile
    %{__make} %{?_smp_mflags}
    %{__mv} snd snd-motif
    %{__make} clean
    %{__rm} -f config.cache
%endif

# build Gtk version
%if %{build_gtk}
    ./configure %{config_options} --with-gtk=yes
    # hack to fix detection of alsa
#    perl -p -i -e "s|/\* \#undef HAVE_ALSA \*/|\#define HAVE_ALSA 1|g" config.h
    # hack to add alsa library to link stage
#    perl -p -i -e "s|-ljack|-lasound -ljack|g" makefile
    %{__make} %{?_smp_mflags}
    %{__mv} snd snd-gtk
    %{__make} clean
    %{__rm} -f config.cache
%endif

# build snd utilities
./configure %{config_options}
# hack to fix detection of alsa
#perl -p -i -e "s|/\* \#undef HAVE_ALSA \*/|\#define HAVE_ALSA 1|g" config.h
# hack to add alsa library to link stage
#perl -p -i -e "s|-ljack|-lasound -ljack|g" makefile
# removed sndsine for now
%{__make}  %{?_smp_mflags} sndplay sndrecord sndinfo audinfo

%install
%{__rm} -rf %{buildroot}
%{__mkdir} -p %{buildroot}%{_bindir}/
%{__mkdir} -p %{buildroot}%{_mandir}/man1
%{__install} -m 644 snd.1 %{buildroot}%{_mandir}/man1
%if %{build_motif}
    %{__install} -m 755 snd-motif %{buildroot}%{_bindir}/snd
%endif
%if %{build_gtk}
    %{__install} -m 755 snd-gtk %{buildroot}%{_bindir}/snd-gtk
%endif
# removed sndsine for now
%{__install} -m 755 sndplay sndrecord sndinfo audinfo %{buildroot}%{_bindir}

# scheme source code
%{__mkdir} -p %{buildroot}%{sndlib}/scheme
(%{__tar} cf - *.scm dlp)|(cd %{buildroot}/%{sndlib}/scheme; %{__tar} xpf -)

# change interpreter for finder.scm, otherwise /usr/local/bin/guile is required
%{__perl} -p -i -e "s,/usr/local/bin,%{sndlib}/bin,g" %{buildroot}/%{sndlib}/scheme/finder.scm

# the snd icon
%{__mkdir} -p %{buildroot}/%{_datadir}/pixmaps/
%{__install} -m 644 %{SOURCE1} %{buildroot}%{_datadir}/pixmaps/

# desktop entry for redhat
%{__cat} << EOF > %{desktop_vendor}-%{name}.desktop
[Desktop Entry]
Name=Snd
Comment=the Snd sound editor
Icon=%{name}.png
Exec=%{_bindir}/%{name}
Terminal=false
Type=Application
EOF
%if "%{desktop_utils}" == "yes"
  %{__mkdir} -p %{buildroot}%{_datadir}/applications
  desktop-file-install --vendor %{desktop_vendor} \
    --dir %{buildroot}%{_datadir}/applications    \
    --add-category X-Red-Hat-Base                 \
    --add-category Application                    \
    --add-category AudioVideo                     \
    %{desktop_vendor}-%{name}.desktop
%else
  %{__install} -d %{buildroot}%{_sysconfdir}/X11/applnk/Multimedia
  %{__install} %{desktop_vendor}-%{name}.desktop %{buildroot}%{_sysconfdir}/X11/applnk/Multimedia/
%endif

#--- a configuration file with the default scheme paths ready to go
%{__mkdir} -p %{buildroot}/etc
%if "%{build_for_ccrma}" == "1"
%{__cat} << EOF > %{buildroot}/etc/snd.conf
;; Default ccrma snd configuration file [nando, mar 2002]
;;
;; add paths to begin of default load path (last in the list is the 
;; first in the search order)
(set! %load-path (cons "/usr/ccrma/lisp/src/snd/dlp" %load-path))
(set! %load-path (cons "/usr/ccrma/lisp/src/snd" %load-path))
EOF
%else
%{__cat} << EOF > %{buildroot}/etc/snd.conf
;; Default snd configuration file [nando, dec 2001]
;;
;; add paths to begin of default load path (last in the list is the 
;; first in the search order)
(set! %load-path (cons "%{sndlib}/scheme/dlp" %load-path))
(set! %load-path (cons "%{sndlib}/scheme" %load-path))
EOF
%endif

%clean
%{__rm} -rf %{buildroot}

#--- snd package, just documentation, icon and desktop menu entry
%files
%defattr(-, root, root)
%doc COPYING README.Snd HISTORY.Snd Snd.ad TODO.Snd *.html
%doc *.png
%{_datadir}/pixmaps/snd.png
%if "%{desktop_utils}" == "yes"
%{_datadir}/applications/*%{name}.desktop
%else
%{_sysconfdir}/X11/applnk/Multimedia/%{desktop_vendor}-%{name}.desktop
%endif
%{_mandir}/man1/snd.1*
#--- scheme code
%if "%{build_for_ccrma}" == "1"
# do not include the scheme code when building for ccrma
%else
%dir %{sndlib}/scheme
%{sndlib}/scheme/*
%endif
#--- default snd configuration file
%config(noreplace) /etc/snd.conf

# arguments to scripts:
# (see http://www-106.ibm.com/developerworks/library/l-rpm3/)
# installing: post = 1
# upgrading:  post = 2 -> postun = 1
# erasing:    postun = 0

#--- snd-motif
%if %{build_motif}
%files motif
%defattr(-, root, root)
%{_bindir}/snd
%endif

#--- snd-gtk
%if %{build_gtk}
%files gtk
%defattr(-, root, root)
%{_bindir}/snd-gtk
%endif

#--- snd-utils
%files utils
%defattr(-, root, root)
%{_bindir}/sndplay
%{_bindir}/sndrecord
%{_bindir}/sndinfo
# %{_bindir}/sndsine
%{_bindir}/audinfo

%changelog
* Sat Apr  2 2005 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 7.11-1
- updated to 7.11
- detection of alsa not working, hacked config.h, added -lasound to 
  link stage and patch to midi.c to be able to build oss and alsa
  support at the same time
* Sat Jan  7 2005 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 7.9-1
- updated to 7.9 of 2005.01.07
- start of spec file cleanup, remove almost all build switches, 
  nobody uses the customization anyway
- removed sndsine, will not build on fc3
* Fri Aug 26 2004 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 7.6-1
- updated to version 7.6
- added the with-midi build option
* Tue Jul  6 2004 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 7.5-1
- updated to version 7.5
* Wed May 19 2004 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 7.4-1
- updated to version 7.4
* Fri Feb 20 2004 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 7.2-1
- updated to version 7.2
* Mon Jan 12 2004 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 7.1-1
- updated to version 7.1
* Fri Dec 12 2003 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 7-1
- updated to version 7
* Sun Nov  9 2003 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 6.12-1
- spec file tweaks
- added defines to use old alsa api
* Mon Sep 29 2003 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 6.12-1
- updated to 6.12 of 09/29/2003
* Sun Aug 17 2003 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 6.11-1
- updated to 6.11 of 08/15/2003
- no longer use snd-guile packages (switched to guile 6.4 on all
  supported redhat releases, created a new build option to enable it, 
  added Obsoletes tag for 7.3 and 8.0 so that snd-guile is erased by
  the apt dist-upgrade process. 
- added release tags
* Thu May 29 2003 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 6.9-1
- updated to 6.9 of 05/29/2003
* Tue May  6 2003 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 6.8-1
- updated to 6.8 of 05/06/2003
* Tue Apr  8 2003 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 6.7-2
- rebuilt for newer version of fftw
* Sat Mar 22 2003 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 6.7-1
- updated to 6.7
- changed prefix to use default macros
- do not use links to differentiate between motif and gtk version, 
  motif version is now named "snd" and gtk version "snd-gtk"
- do not allow oss and alsa versions of the programs to coexist
- contrib directory for scheme and ruby code is now gone
* Tue Feb 18 2003 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 6.6-1
- updated to 6.6
* Mon Jan 13 2003 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 6.5-1
- updated to 6.5
* Sat Dec 14 2002 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 6.4-1
- updated to 6.4 of 12/14/2002
- changed to proper application group
* Wed Nov 13 2002 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 6.3-1
- updated to 6.3 of 11/13/2002
- added proper redhat 8.0 menu entry
* Wed Oct 16 2002 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 6.2-1
- updated to 6.2 of 10/16/2002
* Tue Sep 17 2002 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 6.1-1
- updated to 6.1 of 09/17/2002
* Mon Aug 26 2002 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 6.0-2
- made dependency on openmotif explicit, otherwise snd appears to be 
  happy with lesstif on redhat 7.2 but fails on startup because of an
  undefined symbol
- updated to snd 6.0 of 08/26/2002
* Wed Aug 14 2002 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 6.0-1
- updated to snd 6.0 of 08/14/2002
* Sun Jul 14 2002 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 5.12-3
- updated to snd of 07/14/2002
* Sat Jul  6 2002 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 5.12-2
- another update, fixed postun installs as they were not working with
  rpm updates (they need to check the argument to the script)
* Tue Jul  2 2002 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 5.12-1
- updated to latest snd
* Sun Jun 16 2002 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 5.11-1
- naming of packages: snd -> base package; snd-motif, snd-gtk main 
  executable, depending on driver either -oss or nothing added; 
  -utils, depending on driver either -oss or nothing added
* Fri Jun 14 2002 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
- simplified spec file, get dependencies automatically
- no longer supports building alsa 0.5 rpms
- no longer supports building with static gsl libraries
- if building for motif always build with static xm library
- simplify snd-detect-flavor, we don't care about releases anymore
- statically linked motif packages are gone, we have openmotif now
* Mon Mar 25 2002 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 5.8-1
- updated to 5.8 03/25/2002
- undo the guile change, after some thought I concluded it is not worth it,
  much better to depend on a separate snd-guile package, as before (save
  the current spec file as a keepsake :-)
- define a build_for_ccrma flag to create an rpm stub for ccrma internal
  use (does not contain any scheme code and has a configuration file that
  to the shared version of the snd source).
* Fri Jan 18 2002 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 5.6-1
- updated to 5.6 01/18/2002
* Mon Dec  3 2001 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 5.5-1
- updated to 5.5 12/03/2001
- incorporated guile 1.5.0 in the rpm (what used to be separate snd-guile 
  package). Currently an ugly hack because configuring and compiling snd
  requires a functional guile, but guile is not installed yet if it is 
  going to be part of snd snd rpms. So for now I do a temporary install
  of guile in the final location (by default /usr/lib/snd) while compiling
  snd. The install is removed at the end of the snd compilation process. 
- install all scheme files in /usr/lib/snd/scheme, including the contrib
  dir
- create a default /etc/snd.conf that adds all scheme source directories
  to the default search path so that "load-from-path" can use just the
  file names. 
* Wed Nov 28 2001 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 5.4-2
- added menu entry for Mandrake 8.x [thanks to Kevin Cosgrove]
- update to snd-5.4 11/28/2001 (with support for new snd_config_get_id 
  in alsa)
* Mon Nov 05 2001 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 5.4-1
- snd 5.4 10/01/2001
- added contrib/ to documentation
- fixed wrong guile_gtk dependency
* Mon Oct  1 2001 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 5.3-1
- snd 5.3 10/01/2001
- generate oss, alsa0.5 and alsa0.9 separate packages, simplifies spec file
- added environment control variables for build configuration through scripts
- added conflicts with sndplay-* packages (from the cm/clm/cmn rpms)
* Thu Sep 27 2001 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 5.2-2
- snd-5.2 09/27/2001
- added man page to files
- removed snd requirement for the snd-utils-* packages
- added package date to description of packages
- added ladspa switch
* Wed Sep  5 2001 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu> 5.2-1
- snd-5.2
- changed over to guile 1.5, will require snd-guile from now on
- start keeping old spec files for reference
- snd requires gsl >= 0.8, drop references to gsl 0.7
- added xm dependencies
- added menu entry for redhat rpms
- added snd icon
* Thu Jul 26 2001 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
- snd-5.1, alsa 0.9
- added autodetect of installed alsa version (%alsa_ver)
- added autodetect of linux flavor
- autobuild oss only or oss/alsa binary packages
- changed naming of packages and executables to reflect the fact that
  the statically linked alsa package also supports the oss api
* Thu Jul 12 2001 Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
- first try at snd-5, snd-guile-1.6, alsa 0.9
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

