# RPM spec file for Snd by Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
#
# yes, this is crazy and patches to make this simpler gladly accepted, 
# provided the whole thing can still be used in all currently
# supported distros. 

%define prefix   /usr
%define ver      5.3
%define rel      1
%define tarname  snd-5
%define snd_date "10/01/2001"

# autodetection of installed alsa library version:

%define alsa_ver %(if [ -e /usr/lib/libasound.so.2 ] ; then echo "0.9" ; elif [ -e /usr/lib/libasound.so.1 ] ; then echo "0.5" ; else echo "" ; fi)

# autodetection of distribution we are building in:

%define linux_flavor %(%{_sourcedir}/snd-detect-flavor)

# [BEGIN BUILD CONFIGURATION VARIABLES]

# these constants define which binary rpm's are going to be generated,
# you can enable the type of gui [motif|gtk] and whether the command
# line utilities are built as well. 

%define build_motif_gui 1
%define build_static_motif_gui 1
%define build_gtk_gui 1
%define build_utils 1

# define the sound api to use, set to "oss" or "alsa".
# in the case of alsa the installed version will be
# autodetected and appended to the package names

%define build_sound_api "alsa"

# set this to 1 if you want to link statically against gsl
# (gsl == GNU Scientific Library)

%define with_static_gsl 1

# set this to 1 if you want to link statically against xm
# (xm == Guile Motif interface module)

%define with_static_xm 1

# set this to 1 if you want to depend on the guile-gtk library
# (0 for now till it can be compiled with guile 1.5.x)

%define with_guile_gtk 0

# set this to 1 if you want to build snd with ladspa support

%define with_ladspa 1

# set this to 1 if you want to link statically against alsa
#
#   if the binary is statically linked then the requirement
#   for alsa to be preinstalled can be dropped and the resulting
#   binary can run in either an alsa or oss only environment, 
#   snd will autoswitch to the existing driver giving alsa
#   priority. But it will make the executable bigger so if you
#   have alsa installed you might want to set this to 0

%if "%{alsa_ver}" == "0.5"
    %define with_static_alsa 1
%endif
# for now the default for 0.9 is to link dynamically as the libraries
# and api are still changing a bit [as of 7/27/2001]
%if "%{alsa_ver}" == "0.9"
    %define with_static_alsa 0
%endif

# [END BUILD CONFIGURATION VARIABLES]

# environment overrides for the build process: this enables
# an external script to control the build variables so that
# one script can recompile all versions of the binary rpm,

%define ENV_build_sound_api        %(echo $SND_BUILD_SOUND_API)
%define ENV_build_motif_gui        %(echo $SND_BUILD_MOTIF_GUI)
%define ENV_build_static_motif_gui %(echo $SND_BUILD_STATIC_MOTIF_GUI)
%define ENV_build_gtk_gui          %(echo $SND_BUILD_GTK_GUI)
%define ENV_build_utils            %(echo $SND_BUILD_UTILS)

%if "%{ENV_build_sound_api}" != ""
%define build_sound_api %{ENV_build_sound_api}
%endif
%if "%{ENV_build_motif_gui}" != ""
%define build_motif_gui %{ENV_build_motif_gui}
%endif
%if "%{ENV_build_static_motif_gui}" != ""
%define build_static_motif_gui %{ENV_build_static_motif_gui}
%endif
%if "%{ENV_build_gtk_gui}" != ""
%define build_gtk_gui %{ENV_build_gtk_gui}
%endif
%if "%{ENV_build_utils}" != ""
%define build_utils %{ENV_build_utils}
%endif

# this is going to be used to name the packages
# according to the sound api and version (in the
# case of alsa)
%if "%{build_sound_api}" == "alsa"
%define sound_api %{build_sound_api}%{alsa_ver}
%else
%define sound_api %{build_sound_api}
%endif

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
# ldd snd | awk '{printf $3 " "; system("rpm -q -f " $3);}' | awk '{print $2 " -> " $1}' | sort
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
# here's a list of what we currently require under RedHat 6.2, the defaults
# used and a rationale for them:

# alsa: the alsa driver libraries. The requirement is only defined if
#       the library is dynamically linked. At this point the program 
#       autoswitches into oss if no alsa driver is found (or if the 
#       alsa library reports no installed cards). We support both alsa 
#       0.5.* or 0.9.*

%if "%{sound_api}" == "alsa0.5"
  %define req_alsa libasound.so.1
%endif
%if "%{sound_api}" == "alsa0.9"
  %define req_alsa libasound.so.2
%endif

# gsl: the GNU Scientific Library, the requirement is only defined
#      if we are dynamically linking against gsl 

  %define req_gsl libgsl.so.0 libgslcblas.so.0

# xm: the guile motif interface glue

  %define req_xm libxm.so

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

############
# RedHat 7.x
############

# RedHat 7.0-7.1
#   Additional packages:
#     openmotif 2.1.30-6 (from the PowerTools cdrom)
#     installed alsa (driver, lib, utils, either 0.5 or 0.9)
#   We require these packages to be installed:
#     snd-guile
#     snd-guile-gtk [not for guile 1.5, it is uncompilable]
#     [while out-of-the-box redhat7 comes with 
#      guile 1.3.4 it does not have guilegtk
#      so we would have to install that anyway]

%if "%{linux_flavor}" == "RedHat-7.0" || "%{linux_flavor}" == "RedHat-7.1" 
  # guile 1.5 (no guile-gtk for 1.5)
  %define req_guile libguile.so.10
  # rh7.x is based on 2.2
  %define req_glibc glibc >= 2.2
  # comes with 1.2.8
  %define req_gtk gtk+ >= 1.2.8 glib >= 1.2.8
  # just require XFree86 libraries, xpm is part of them
  %define req_x XFree86-libs
%endif

##########
# SuSE 7.0
##########

# SuSE 7.0
#   I installed the DEMO cd and then the following additional packages:
#     motif/motifdev, guile, guilegtk
#     gtk/gtkdev 1.2.8 55
#     glib/glibdev 1.2.8 57
#     alsa/alsadev 0.5.9 20

%if "%{linux_flavor}" == "SuSE-7.0"
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

##############
# Mandrake 7.2
##############

# Mandrake 7.2
#   Additional packages installed:
#     openmotif 2.1.30-5mdk (from the Applications 1 cdrom)
#     used the existing alsa-0.5.9 (driver, lib, utils)

%if "%{linux_flavor}" == "Mandrake-7.2"
  # mdk is based on 2.1
  %define req_glibc glibc >= 2.1
  # comes with 1.2.8
  %define req_gtk gtk+ >= 1.2.8 glib >= 1.2.8
  # require XFree86 libraries and xpm
  %define req_x XFree86-libs xpm
%endif

# global requirements, to make life easier latter

%define req_X %{req_glibc} %{req_x} 
%define req_X_motif %{req_glibc} %{req_x} %{req_motif}
%define req_X_gtk %{req_glibc} %{req_x} %{req_gtk}

# command line options for static linking

%if "%{with_static_gsl}" == "1"
    %define static_gsl "--with-static-gsl=yes"
%else
    %define static_gsl ""
%endif
%if "%{with_static_alsa}" == "1"
    %define static_alsa "--with-static-alsa=yes"
%else
    %define static_alsa ""
%endif
%if "%{with_static_xm}" == "1"
    %define static_xm "--with-static-xm=yes"
%else
    %define static_xm ""
%endif
%if "%{with_ladspa}" == "1"
    %define ladspa "--with-ladspa"
%else
    %define ladspa ""
%endif


Summary:	A sound editor (%{ver}, %{snd_date})
Name:		snd
Version:	%{ver}
Release:	%{rel}
Copyright:	LGPL
Group:		Applications/Sound
Source:		ftp://ccrma-ftp.stanford.edu/pub/Lisp/%{tarname}.tar.gz
Source1:        snd-detect-flavor
Source2:        snd.png
# old spec files (for reference)
Source100:      snd-5.1-1.spec
Source101:      snd-5.2-1.spec
Source102:      snd-5.2-2.spec
URL:		http://www-ccrma.stanford.edu/software/snd/
Vendor:		CCRMA/Music Stanford University
Packager:	Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
Buildroot:      %{_tmppath}/%{name}-root
Prefix:         %{prefix}
%if %{build_with_AutoReqProvOff}
AutoReqProv:    off
%endif
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

%package %{sound_api}
Summary:        Motif/%{sound_api} version of snd (%{ver}, %{snd_date})
Group:		Applications/Sound
%if %{build_with_AutoReqProvOff}
AutoReqProv:    off
Requires:       snd == %{ver} %{req_X} %{req_guile}
%if "%{build_sound_api}" == "alsa" && "%{with_static_alsa}" == "0"
Requires:       %{req_alsa}
%endif
%if "%{with_static_gsl}" == "0"
Requires:       %{req_gsl}
%endif
%if "%{with_static_xm}" == "0"
Requires:       %{req_xm}
%endif
%endif

%description %{sound_api}
A version of the Snd editor (%{ver} of %{snd_date}) compiled with the
Motif gui (statically linked), and support for the %{sound_api} sound
driver api.

%if "%{build_sound_api}" == "alsa" && "%{with_static_alsa}" == "1"
This package will use the oss sound driver api if alsa is not installed. 
%endif

 OSS and the
ALSA %{alsa_ver} api sound drivers. 

%package motif-%{sound_api}
Summary:        Motif/%{sound_api} version of snd (%{ver}, %{snd_date})
Group:		Applications/Sound
%if %{build_with_AutoReqProvOff}
AutoReqProv:    off
Requires:       snd == %{ver} %{req_X_motif} %{req_guile}
%if "%{build_sound_api}" == "alsa" && "%{with_static_alsa}" == "0"
Requires:       %{req_alsa}
%endif
%if "%{with_static_gsl}" == "0"
Requires:       %{req_gsl}
%endif
%if "%{with_static_xm}" == "0"
Requires:       %{req_xm}
%endif
%endif

%description motif-%{sound_api}
A version of the Snd editor (%{ver} of %{snd_date}) compiled with the
Motif gui and support for the %{sound_api} sound drivers api. You will
need to have Motif already installed to install this package. 

%if "%{build_sound_api}" == "alsa" && "%{with_static_alsa}" == "1"
This package will use the oss sound driver api if alsa is not installed. 
%endif

%package gtk-%{sound_api}
Summary:        Gtk/%{sound_api} version of snd (%{ver}, %{snd_date})
Group:		Applications/Sound
%if %{build_with_AutoReqProvOff}
AutoReqProv:    off
Requires:       snd == %{ver} %{req_X_gtk} %{req_guile}
%if "%{with_guile_gtk}" == "0"
Requires:       %{req_guilegtk}
%endif
%if "%{build_sound_api}" == "alsa" && "%{with_static_alsa}" == "0"
Requires:       %{req_alsa}
%endif
%if "%{with_static_gsl}" == "0"
Requires:       %{req_gsl}
%endif
%endif

%description gtk-%{sound_api}
A version of the Snd editor (%{ver} of %{snd_date}) compiled with the gtk
gui and support for the %{sound_api} api sound drivers. 

%if "%{build_sound_api}" == "alsa" && "%{with_static_alsa}" == "1"
This package will use the oss sound driver api if alsa is not installed. 
%endif

%package utils-%{sound_api}
Summary:        %{sound_api} versions of sndplay and friends (%{ver}, %{snd_date})
Group:		Applications/Sound
%if %{build_with_AutoReqProvOff}
AutoReqProv:    off
Requires:       %{req_glibc}
%if "%{build_sound_api}" == "alsa" && "%{with_static_alsa}" == "0"
Requires:       %{req_alsa}
%endif
%if "%{with_static_gsl}" == "0"
Requires:       %{req_gsl}
%endif
# always conflict with the sndplay packages from cm/clm/cmn
Conflicts:      sndplay-oss sndplay-alsa0.5 sndplay-alsa0.9
%endif

%description utils-%{sound_api}
Command line utilities included with the snd sound editor (%{ver} of %{snd_date})
compiled with support for the %{sound_api} api sound drivers. 

%if "%{build_sound_api}" == "alsa" && "%{with_static_alsa}" == "1"
This package will use the oss sound driver api if alsa is not installed. 
%endif

%prep
%setup -n %{tarname}

%build snd
echo "Building on %{linux_flavor}"
echo "Building for %{sound_api} sound api"
%if "%{alsa_ver}" != ""
echo "Building on a machine with ALSA %{alsa_ver}"
%else
echo "Building on an OSS only machine"
%endif

%ifos Linux
%ifarch i386 i586 i686

# always prepend /usr/lib/snd/bin to the search path
%define guilepath PATH=/usr/lib/snd/bin:${PATH}

#--- build utilities
%if %{build_utils}
%if "%{build_sound_api}" == "alsa"
    %{guilepath} ./configure --prefix=%{prefix} --with-alsa=yes %{static_alsa} %{static_gsl} %{ladspa}
%else
    %{guilepath} ./configure --prefix=%{prefix} --with-alsa=no %{static_gsl} %{ladspa}
%endif
    make sndplay sndrecord sndinfo sndsine audinfo
    mv sndplay sndplay-%{build_sound_api}
    mv sndrecord sndrecord-%{build_sound_api}
    mv sndinfo sndinfo-%{build_sound_api}
    mv sndsine sndsine-%{build_sound_api}
    mv audinfo audinfo-%{build_sound_api}
    make clean
    rm -f config.cache
%endif

#--- build dynamically linked Motif version
%if %{build_motif_gui}
%if "%{build_sound_api}" == "alsa"
    %{guilepath} ./configure --prefix=%{prefix} --with-alsa=yes %{static_alsa} %{static_gsl} %{static_xm} %{ladspa}
%else
    %{guilepath} ./configure --prefix=%{prefix} --with-alsa=no %{static_gsl} %{static_xm} %{ladspa}
%endif
    make
    mv snd snd-motif-%{build_sound_api}
    make clean
    rm -f config.cache
%endif

#--- build statically linked Motif version
%if %{build_static_motif_gui}
%if "%{build_sound_api}" == "alsa"
    %{guilepath} ./configure --prefix=%{prefix} --with-alsa=yes %{static_alsa}  --with-static-motif=yes %{static_gsl} %{static_xm} %{ladspa}
%else
    %{guilepath} ./configure --prefix=%{prefix} --with-alsa=no --with-static-motif=yes %{static_gsl} %{static_xm} %{ladspa}
%endif
    make
    mv snd snd-%{build_sound_api}
    make clean
    rm -f config.cache
%endif

#--- build Gtk version
%if %{build_gtk_gui}
%if "%{build_sound_api}" == "alsa"
    %{guilepath} ./configure --prefix=%{prefix} --with-gtk=yes --with-alsa=yes %{static_alsa} %{static_gsl} %{ladspa}
%else
    %{guilepath} ./configure --prefix=%{prefix} --with-gtk=yes --with-alsa=no %{static_gsl} %{ladspa}
%endif
    make
    mv snd snd-gtk-%{build_sound_api}
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
mkdir -p ${RPM_BUILD_ROOT}%{_mandir}/man1
install -m 644 ${RPM_BUILD_DIR}/snd-5/snd.1 ${RPM_BUILD_ROOT}%{_mandir}/man1
%if %{build_motif_gui}
    install -m 755 snd-motif-%{build_sound_api} ${RPM_BUILD_ROOT}%{prefix}/bin/
%endif
%if %{build_static_motif_gui}
    install -m 755 snd-%{build_sound_api} ${RPM_BUILD_ROOT}%{prefix}/bin/
%endif
%if %{build_gtk_gui}
    install -m 755 snd-gtk-%{build_sound_api} ${RPM_BUILD_ROOT}%{prefix}/bin/
%endif
%if %{build_utils}
    install -m 755 sndplay-%{build_sound_api} sndrecord-%{build_sound_api} sndinfo-%{build_sound_api} sndsine-%{build_sound_api} audinfo-%{build_sound_api} ${RPM_BUILD_ROOT}%{prefix}/bin/
%endif

# the snd icon
/bin/mkdir -p $RPM_BUILD_ROOT/%{prefix}/share/pixmaps/
install -m 644 %{SOURCE2} $RPM_BUILD_ROOT/%{prefix}/share/pixmaps/

%if "%{linux_flavor}" == "RedHat-7.0" || "%{linux_flavor}" == "RedHat-7.1" 
  # desktop entry for redhat
  /bin/mkdir -p $RPM_BUILD_ROOT/etc/X11/applnk/Multimedia
  cat <<EOF >$RPM_BUILD_ROOT/etc/X11/applnk/Multimedia/snd.desktop
[Desktop Entry]
Name=Snd
Comment=Snd
Exec=/usr/bin/snd
Icon=snd.png
Terminal=0
Type=Application
EOF
%endif

%clean
rm -rf ${RPM_BUILD_ROOT}

#--- snd package, just documentation, icon and desktop menu entry
%files
%defattr(-, root, root)
%doc COPYING README.Snd HISTORY.Snd Snd.ad TODO.Snd *.html
%doc *.png
%doc *.scm
%{prefix}/share/pixmaps/snd.png
%if "%{linux_flavor}" == "RedHat-7.0" || "%{linux_flavor}" == "RedHat-7.1" 
/etc/X11/applnk/Multimedia/snd.desktop
%endif
%{_mandir}/man1/snd.1*

#--- snd (motif statically linked)
%if %{build_static_motif_gui}
%post %{sound_api}
if [ -L %{prefix}/bin/snd ] ; then
    rm -f %{prefix}/bin/snd
fi
if [ ! -e %{prefix}/bin/snd ] ; then
    ln -s %{prefix}/bin/snd-%{build_sound_api} %{prefix}/bin/snd
fi
%postun %{sound_api}
if [ -L %{prefix}/bin/snd ] ; then
    link=`ls -l %{prefix}/bin/snd | awk '{print $11}'`
    if [ `basename ${link}` = "snd-%{build_sound_api}" ] ; then
	rm -f %{prefix}/bin/snd
    fi
fi
%files %{sound_api}
%defattr(-, root, root)
%{prefix}/bin/snd-%{build_sound_api}
%endif

#--- snd-motif (motif dynamically linked)
%if %{build_motif_gui}
%post motif-%{sound_api}
if [ -L %{prefix}/bin/snd ] ; then
    rm -f %{prefix}/bin/snd
fi
if [ ! -e %{prefix}/bin/snd ] ; then
    ln -s %{prefix}/bin/snd-motif-%{build_sound_api} %{prefix}/bin/snd
fi
%postun motif-%{sound_api}
if [ -L %{prefix}/bin/snd ] ; then
    link=`ls -l %{prefix}/bin/snd | awk '{print $11}'`
    if [ `basename ${link}` = "snd-motif-%{build_sound_api}" ] ; then
	rm -f %{prefix}/bin/snd
    fi
fi
%files motif-%{sound_api}
%defattr(-, root, root)
%{prefix}/bin/snd-motif-%{build_sound_api}
%endif

#--- snd-gtk-oss
%if %{build_gtk_gui}
%post gtk-%{sound_api}
if [ -L %{prefix}/bin/snd ] ; then
    rm -f %{prefix}/bin/snd
fi
if [ ! -e %{prefix}/bin/snd ] ; then
    ln -s %{prefix}/bin/snd-gtk-%{build_sound_api} %{prefix}/bin/snd
fi
%postun gtk-%{sound_api}
if [ -L %{prefix}/bin/snd ] ; then
    link=`ls -l %{prefix}/bin/snd | awk '{print $11}'`
    if [ `basename ${link}` = "snd-gtk-%{build_sound_api}" ] ; then
	rm -f %{prefix}/bin/snd
    fi
fi
%files gtk-%{sound_api}
%defattr(-, root, root)
%{prefix}/bin/snd-gtk-%{build_sound_api}
%endif

#--- snd-utils
%if %{build_utils}
%post utils-%{sound_api}
for util in sndplay sndrecord sndinfo sndsine audinfo
do 
    if [ -L %{prefix}/bin/${util} ] ; then
	rm -f %{prefix}/bin/${util}
    fi
    if [ ! -e %{prefix}/bin/${util} ] ; then
	ln -s %{prefix}/bin/${util}-%{build_sound_api} %{prefix}/bin/${util}
    fi
done
%postun utils-%{sound_api}
for util in sndplay sndrecord sndinfo sndsine audinfo
do 
    if [ -L %{prefix}/bin/${util} ] ; then
	link=`ls -l %{prefix}/bin/${util} | awk '{print $11}'`
	if [ `basename ${link}` = "${util}-%{build_sound_api}" ] ; then
	    rm -f %{prefix}/bin/${util}
	fi
    fi
done
%files utils-%{sound_api}
%defattr(-, root, root)
%{prefix}/bin/sndplay-%{build_sound_api}
%{prefix}/bin/sndrecord-%{build_sound_api}
%{prefix}/bin/sndinfo-%{build_sound_api}
%{prefix}/bin/sndsine-%{build_sound_api}
%{prefix}/bin/audinfo-%{build_sound_api}
%endif


%changelog
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

