# RPM spec file for Snd by Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
#
# the old scheme of trying to get meaningful names for libraries and other
# dependencies is gone. Things like apt for rpm make that unnecessary and 
# the dream of rpms that can be used on any distribution was just that, 
# a dream

%define prefix   /usr
%define sndlib   %{_libdir}/snd
%define ver      6.3
%define rel      1
%define tarname  snd-6
%define snd_date "11/13/2002"

%define	desktop_vendor	planetccrma
%define desktop_utils   %(if which desktop-file-install 2>1 >/dev/null ; then echo "yes" ; fi)

#### BEGIN BUILD CONFIGURATION VARIABLES ####

# these constants define which binary rpm's are going to be generated,
# you can enable the type of gui [motif|gtk] and whether the command
# line utilities are built as well. 

%define build_motif_gui 1
%define build_gtk_gui 0
%define build_utils 1

# define the sound api to use, set to "oss" or "alsa"
# (don't add the double quotes!). 

%define sound_api alsa

# set this to 1 if you want to build snd with ladspa support

%define with_ladspa 1

# set this to 1 if you want to build snd with opengl support

%define with_opengl 1

# set this to 1 if you want to link statically against alsa
#
#   if the binary is statically linked then the requirement
#   for alsa to be preinstalled can be dropped and the resulting
#   binary can run in either an alsa or oss only environment, 
#   snd will autoswitch to the existing driver giving alsa
#   priority. But it will make the executable bigger so if you
#   have alsa installed you might want to set this to 0

%define with_static_alsa 0

# at ccrma snd lives in the shared server space, not in each
# individual machine, but I still want to install the icon,
# menu entry and docs in every machine. Set to 1 when building
# a version with a custom /etc/snd.conf and no builtin scheme
# code

%define build_for_ccrma 0

# set this to 1 if you want to depend on the guile-gtk library
# (0 for now till it can be compiled with guile 1.5.x)

%define with_guile_gtk 0

# define any extra configure options here:
# currently prefix, guile, run optimizer.

%define snd_config_options --prefix=%{prefix} --with-guile=yes --with-run=yes


#### END BUILD CONFIGURATION VARIABLES ####

# autodetection of distribution we are building in, used for menus:

%define linux_flavor %(%{_sourcedir}/snd-detect-flavor)

# environment overrides for the build process: this enables
# an external script to control the build variables so that
# one script can recompile all versions of the binary rpm,

%define ENV_sound_api              %(echo $SND_SOUND_API)
%define ENV_build_motif_gui        %(echo $SND_BUILD_MOTIF_GUI)
%define ENV_build_gtk_gui          %(echo $SND_BUILD_GTK_GUI)
%define ENV_build_utils            %(echo $SND_BUILD_UTILS)

%if "%{ENV_sound_api}" != ""
%define sound_api %{ENV_sound_api}
%endif
%if "%{ENV_build_motif_gui}" != ""
%define build_motif_gui %{ENV_build_motif_gui}
%endif
%if "%{ENV_build_gtk_gui}" != ""
%define build_gtk_gui %{ENV_build_gtk_gui}
%endif
%if "%{ENV_build_utils}" != ""
%define build_utils %{ENV_build_utils}
%endif

# command line configuration options

%if "%{with_ladspa}" == "1"
    %define ladspa --with-ladspa
%else
    %define ladspa ""
%endif
%if "%{sound_api}" == "alsa" && "%{with_static_alsa}" == "0"
    %define sound_driver --with-alsa=yes
%endif
%if "%{sound_api}" == "alsa" && "%{with_static_alsa}" == "1"
    %define sound_driver --with-static-alsa=yes
%endif
%if "%{sound_api}" != "alsa"
    %define sound_driver --with-alsa=no
%endif
%if "%{with_opengl}" == "1"
    %define opengl --with-gl
%else
    %define opengl ""
%endif
%if "%{sound_api}" == "alsa"
    %define motif_name motif
    %define gtk_name gtk
    %define utils_name utils
%else
    %define motif_name motif-oss
    %define gtk_name gtk-oss
    %define utils_name utils-oss
%endif


Summary:	A sound editor (%{ver}, %{snd_date})
Name:		snd
Version:	%{ver}
Release:	%{rel}
Copyright:	LGPL
Group:		Applications/Sound
Source:		ftp://ccrma-ftp.stanford.edu/pub/Lisp/%{tarname}.tar.gz
Source2:        snd-detect-flavor
Source3:        snd.png
# old spec files (for reference)
Source100:      snd-5.1-1.spec
Source101:      snd-5.2-1.spec
Source102:      snd-5.2-2.spec
Source105:      snd-5.8-1.spec
URL:		http://www-ccrma.stanford.edu/software/snd/
Vendor:		CCRMA/Music Stanford University
Packager:	Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
Distribution:   Planet CCRMA
Buildroot:      %{_tmppath}/%{name}-root
Prefix:         %{prefix}
%if "%{with_ladspa}" == "1"
BuildRequires:  ladspa-sdk
%endif

%description
Snd is a sound editor modelled loosely after Emacs and an old, sorely-missed
PDP-10 sound editor named Dpysnd. It can accomodate any number of sounds
each with any number of channels, and can be customized and extended
using guile and guile-gtk. Snd is free (LGPL); the code is available via
anonymous ftp at ccrma-ftp.stanford.edu as pub/Lisp/%{tarname}.tar.gz.

This package contains snd version %{ver}, dated %{snd_date}.

%package %{motif_name}
Summary:        Motif/%{sound_api} version of snd (%{ver}, %{snd_date})
Group:		Applications/Sound
Requires:       snd == %{ver}
Requires:       openmotif

%description %{motif_name}
A version of the Snd editor (%{ver} of %{snd_date}) compiled with the
Motif gui and support for the %{sound_api} sound drivers api.

%if "%{sound_api}" == "alsa" && "%{with_static_alsa}" == "1"
This package will use the oss sound driver api if alsa is not installed. 
%endif

%package %{gtk_name}
Summary:        Gtk/%{sound_api} version of snd (%{ver}, %{snd_date})
Group:		Applications/Sound
Requires:       snd == %{ver}

%description %{gtk_name}
A version of the Snd editor (%{ver} of %{snd_date}) compiled with the gtk
gui and support for the %{sound_api} api sound drivers. 

%if "%{sound_api}" == "alsa" && "%{with_static_alsa}" == "1"
This package will use the oss sound driver api if alsa is not installed. 
%endif

%package %{utils_name}
Summary:        %{sound_api} versions of sndplay and friends (%{ver}, %{snd_date})
Group:		Applications/Sound
# always conflict with the sndplay packages from cm/clm/cmn
Conflicts:      sndplay-oss sndplay

%description %{utils_name}
Command line utilities included with the snd sound editor (%{ver} of %{snd_date})
compiled with support for the %{sound_api} api sound drivers. 

%if "%{sound_api}" == "alsa" && "%{with_static_alsa}" == "1"
This package will use the oss sound driver api if alsa is not installed. 
%endif

%prep
%setup -n %{tarname}

%build
echo "Building on %{linux_flavor}"
echo "Building for %{sound_api} sound api"

%ifos Linux
%ifarch i386 i586 i686

# always prepend the local guile bin path to the search path
%define guilepath PATH=%{sndlib}/bin:${PATH}

#--- build snd: utilities
%if %{build_utils}
    %{guilepath} ./configure %{snd_config_options} %{sound_driver} %{ladspa} %{opengl}
    make sndplay sndrecord sndinfo sndsine audinfo
    mv sndplay sndplay-%{sound_api}
    mv sndrecord sndrecord-%{sound_api}
    mv sndinfo sndinfo-%{sound_api}
    mv sndsine sndsine-%{sound_api}
    mv audinfo audinfo-%{sound_api}
    make clean
    rm -f config.cache
%endif

#--- build snd: Motif version
%if %{build_motif_gui}
    %{guilepath} ./configure %{snd_config_options} --with-static-xm=yes %{sound_driver} %{ladspa} %{opengl}
    make
    mv snd snd-%{motif_name}
    make clean
    rm -f config.cache
%endif

#--- build snd: Gtk version
%if %{build_gtk_gui}
    %{guilepath} ./configure %{snd_config_options} --with-gtk=yes %{sound_driver} %{ladspa} %{opengl}
    make
    mv snd snd-%{gtk_name}
    make clean
    rm -f config.cache
%endif

%else
make -f makefile.ppcrpm
%endif
%endif

%install
[ "$RPM_BUILD_ROOT" != "/" ] && rm -rf ${RPM_BUILD_ROOT}

#--- install snd
install -m 755 -d ${RPM_BUILD_ROOT}%{prefix}/bin/
mkdir -p ${RPM_BUILD_ROOT}%{_mandir}/man1
install -m 644 ${RPM_BUILD_DIR}/%{tarname}/snd.1 ${RPM_BUILD_ROOT}%{_mandir}/man1
%if %{build_motif_gui}
    install -m 755 snd-%{motif_name} ${RPM_BUILD_ROOT}%{prefix}/bin/
%endif
%if %{build_gtk_gui}
    install -m 755 snd-%{gtk_name} ${RPM_BUILD_ROOT}%{prefix}/bin/
%endif
%if %{build_utils}
    install -m 755 sndplay-%{sound_api} sndrecord-%{sound_api} sndinfo-%{sound_api} sndsine-%{sound_api} audinfo-%{sound_api} ${RPM_BUILD_ROOT}%{prefix}/bin/
%endif

#--- scheme source code
/bin/mkdir -p $RPM_BUILD_ROOT/%{sndlib}/scheme
(cd $RPM_BUILD_DIR/%{tarname}/; tar cf - *.scm contrib)|(cd $RPM_BUILD_ROOT/%{sndlib}/scheme; tar xpf -)

# change interpreter for finder.scm, otherwise /usr/local/bin/guile is required
perl -p -i -e "s,/usr/local/bin,%{sndlib}/bin,g" $RPM_BUILD_ROOT/%{sndlib}/scheme/finder.scm

#--- the snd icon
/bin/mkdir -p $RPM_BUILD_ROOT/%{prefix}/share/pixmaps/
install -m 644 %{SOURCE3} $RPM_BUILD_ROOT/%{prefix}/share/pixmaps/

%if "%{linux_flavor}" == "RedHat"
  #--- desktop entry for redhat
  cat << EOF > %{desktop_vendor}-%{name}.desktop
[Desktop Entry]
Name=Snd
Comment=the Snd sound editor
Icon=%{name}.png
Exec=%{_bindir}/%{name}
Terminal=false
Type=Application
EOF
%if "%{desktop_utils}" == "yes"
  mkdir -p %{buildroot}%{_datadir}/applications
  desktop-file-install --vendor %{desktop_vendor} \
    --dir %{buildroot}%{_datadir}/applications    \
    --add-category X-Red-Hat-Base                 \
    --add-category Application                    \
    --add-category AudioVideo                     \
    %{desktop_vendor}-%{name}.desktop
%else
  install -d $RPM_BUILD_ROOT%{_sysconfdir}/X11/applnk/Multimedia
  install %{desktop_vendor}-%{name}.desktop $RPM_BUILD_ROOT%{_sysconfdir}/X11/applnk/Multimedia/
%endif
%endif

%if "%{linux_flavor}" == "Mandrake"
  #--- desktop entry for Mandrake
  install -d %{buildroot}%{_menudir}
  cat << EOF > %{buildroot}%{_menudir}/%{name}
?package(%{name}): \
needs="X11" \
icon="%{name}.png" \
section="Multimedia/Sound" \
title="Snd" \
longtitle="A sound editor" \
command="%{prefix}/bin/%{name}"
EOF
%endif

#--- a configuration file with the default scheme paths ready to go
mkdir -p %{buildroot}/etc
%if "%{build_for_ccrma}" == "1"
cat << EOF > %{buildroot}/etc/snd.conf
;; Default ccrma snd configuration file [nando, mar 2002]
;;
;; add paths to begin of default load path (last in the list is the 
;; first in the search order)
(set! %load-path (cons "/usr/ccrma/lisp/src/snd/contrib/dlp" %load-path))
(set! %load-path (cons "/usr/ccrma/lisp/src/snd/contrib" %load-path))
(set! %load-path (cons "/usr/ccrma/lisp/src/snd" %load-path))
EOF
%else
cat << EOF > %{buildroot}/etc/snd.conf
;; Default snd configuration file [nando, dec 2001]
;;
;; add paths to begin of default load path (last in the list is the 
;; first in the search order)
(set! %load-path (cons "%{sndlib}/scheme/contrib/dlp" %load-path))
(set! %load-path (cons "%{sndlib}/scheme/contrib" %load-path))
(set! %load-path (cons "%{sndlib}/scheme" %load-path))
EOF
%endif

%clean
[ "$RPM_BUILD_ROOT" != "/" ] && rm -rf ${RPM_BUILD_ROOT}

#--- snd package, just documentation, icon and desktop menu entry
%files
%defattr(-, root, root)
%doc COPYING README.Snd HISTORY.Snd Snd.ad TODO.Snd *.html
%doc *.png
%{prefix}/share/pixmaps/snd.png
%if "%{linux_flavor}" == "RedHat"
%if "%{desktop_utils}" == "yes"
%{_datadir}/applications/*%{name}.desktop
%else
%{_sysconfdir}/X11/applnk/Multimedia/%{desktop_vendor}-%{name}.desktop
%endif
%endif
%if "%{linux_flavor}" == "Mandrake"
%{prefix}/lib/menu/%{name}
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
%if %{build_motif_gui}
%post %{motif_name}
%if "%{linux_flavor}" == "Mandrake"
%{update_menus}
%endif
# update the snd link to point to the last snd binary installed
if [ -L %{prefix}/bin/snd ] ; then
    rm -f %{prefix}/bin/snd
fi
if [ ! -e %{prefix}/bin/snd ] ; then
    ln -s %{prefix}/bin/snd-%{motif_name} %{prefix}/bin/snd
fi
%postun %{motif_name}
%if "%{linux_flavor}" == "Mandrake"
%{clean_menus}
%endif
# erase the link if it points to our binary, only when erasing the package
if [ "$1" = "0" ] ; then
    if [ -L %{prefix}/bin/snd ] ; then
        link=`ls -l %{prefix}/bin/snd | awk '{print $11}'`
        if [ `basename ${link}` = "snd-%{motif_name}" ] ; then
             rm -f %{prefix}/bin/snd
        fi
    fi
fi
%files %{motif_name}
%defattr(-, root, root)
%{prefix}/bin/snd-%{motif_name}
%endif

#--- snd-gtk
%if %{build_gtk_gui}
%post %{gtk_name}
%if "%{linux_flavor}" == "Mandrake"
%{update_menus}
%endif
# update the snd link to point to the last snd binary installed
if [ -L %{prefix}/bin/snd ] ; then
    rm -f %{prefix}/bin/snd
fi
if [ ! -e %{prefix}/bin/snd ] ; then
    ln -s %{prefix}/bin/snd-%{gtk_name} %{prefix}/bin/snd
fi
%postun %{gtk_name}
%if "%{linux_flavor}" == "Mandrake"
%{clean_menus}
%endif
# erase the link if it points to our binary, only when erasing the package
if [ "$1" = "0" ] ; then
    if [ -L %{prefix}/bin/snd ] ; then
        link=`ls -l %{prefix}/bin/snd | awk '{print $11}'`
        if [ `basename ${link}` = "snd-%{gtk_name}" ] ; then
	    rm -f %{prefix}/bin/snd
        fi
    fi
fi
%files %{gtk_name}
%defattr(-, root, root)
%{prefix}/bin/snd-%{gtk_name}
%endif

#--- snd-utils
%if %{build_utils}
%post %{utils_name}
for util in sndplay sndrecord sndinfo sndsine audinfo
do 
    if [ -L %{prefix}/bin/${util} ] ; then
	rm -f %{prefix}/bin/${util}
    fi
    if [ ! -e %{prefix}/bin/${util} ] ; then
	ln -s %{prefix}/bin/${util}-%{sound_api} %{prefix}/bin/${util}
    fi
done
%postun %{utils_name}
# erase the links if they point to our binary, only when erasing the package
if [ "$1" = "0" ] ; then
    for util in sndplay sndrecord sndinfo sndsine audinfo
    do 
        if [ -L %{prefix}/bin/${util} ] ; then
	    link=`ls -l %{prefix}/bin/${util} | awk '{print $11}'`
	    if [ `basename ${link}` = "${util}-%{sound_api}" ] ; then
	        rm -f %{prefix}/bin/${util}
	    fi
        fi
    done
fi
%files %{utils_name}
%defattr(-, root, root)
%{prefix}/bin/sndplay-%{sound_api}
%{prefix}/bin/sndrecord-%{sound_api}
%{prefix}/bin/sndinfo-%{sound_api}
%{prefix}/bin/sndsine-%{sound_api}
%{prefix}/bin/audinfo-%{sound_api}
%endif


%changelog
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

