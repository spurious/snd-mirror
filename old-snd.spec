# RPM spec file for Snd

%define prefix   /usr

Summary:	A sound editor
Name:		snd
Version:	6
Release:	5
Copyright:	LGPL
Group:		Applications/Sound
Source:		ftp://ccrma-ftp.stanford.edu/pub/Lisp/snd-6.tar.gz
URL:		http://www-ccrma.stanford.edu/software/snd/
Vendor:		CCRMA/Music Stanford University
Packager:	Bill Schottstaedt <bil@ccrma.stanford.edu>
Docdir:         %{prefix}/share/doc
Buildroot:      %{_tmppath}/%{name}-root

%description
Snd is a sound editor modelled loosely after Emacs and an old, sorely-missed
PDP-10 sound editor named Dpysnd. It can accommodate any number of sounds
each with any number of channels, and can be customized and extended
using Guile or Ruby. Snd is free (LGPL); the code is available via
anonymous ftp at ccrma-ftp.stanford.edu as pub/Lisp/snd-6.tar.gz.

%prep
%setup

%build
%ifos Linux
%ifarch i386
./configure --prefix=%{prefix} --with-static-gsl=yes --with-static-xm=yes --with-just-gl=yes
make
%else
./configure --prefix=%{prefix} --with-static-gsl=yes
make
%endif
%endif

%install
rm -rf ${RPM_BUILD_ROOT}
install -m 755 -d ${RPM_BUILD_ROOT}%{prefix}/bin/
install -m 755 snd ${RPM_BUILD_ROOT}%{prefix}/bin/

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(-, root, root)
%doc *.Snd *.html *.png *.scm *.rb
%{prefix}/bin/snd

# to make the rpm file (since I keep forgetting):
#   make sure none of the .scm scripts is executable
#   md5sum * > md5vals?
#   make up-to-date snd-6.tar.gz (and clear /usr/lib/snd if any)
#   increment release number above (and in snd.h, README.Snd)
#   su root
#   cd /usr/src/redhat/SOURCES
#   cp -f /<wherever>/snd-6.tar.gz .
#   cd ../SPECS
#   cp -f /<wherever>/snd.spec .
#   rpm -ba snd.spec or is it rpmbuild now?
#   /usr/src/redhat/BUILD/snd-6 is where the build occurs
#   end result in /usr/src/redhat/RPMS/i386 (binary) and /usr/src/redhat/SRPMS (source)
#   update version references in README.Snd, ftp:LISP.README, web:snd.html, snd.h, sourceforge index.html, configure.ac
