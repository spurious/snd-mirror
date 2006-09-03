# RPM spec file for Snd

%define prefix   /usr

Summary:	A sound editor
Name:		snd
Version:	8
Release:	3
License: 	GPL
Group:		Applications/Sound
Source:		ftp://ccrma-ftp.stanford.edu/pub/Lisp/snd-8.tar.gz
URL:		http://ccrma.stanford.edu/software/snd/
Vendor:		CCRMA/Music Stanford University
Packager:	Bill Schottstaedt <bil@ccrma.stanford.edu>
Docdir:         %{prefix}/share/doc
Buildroot:      %{_tmppath}/%{name}-root

%description
Snd is a sound editor modelled loosely after Emacs. It can accommodate any 
number of sounds each with any number of channels, and can be customized and extended
using Guile or Ruby. Snd is free (LGPL); the code is available via
anonymous ftp: ccrma-ftp.stanford.edu/pub/Lisp/snd-8.tar.gz,
or as part of PlanetCCRMA: http://ccrma.stanford.edu/planetccrma/software/,
or via CVS at SourceForge: http://sourceforge.net/projects/snd. 

%prep
%setup

%build
%ifos Linux
%ifarch i386
./configure --prefix=%{prefix} --with-static-xm --with-just-gl
make
%else
./configure --prefix=%{prefix}
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
%doc *.Snd *.html *.png *.scm *.rb *.tex
%{prefix}/bin/snd

# to make the rpm file (since I keep forgetting):
#   make up-to-date snd-8.tar.gz
#   increment release number above
#   su root
#   cd /usr/src/redhat/SOURCES
#   cp -f /<wherever>/snd-8.tar.gz .
#   cd ../SPECS
#   cp -f /<wherever>/snd.spec .
#   rpmbuild -ba snd.spec
#   /usr/src/redhat/BUILD/snd-8 is where the build occurs
#   end result in /usr/src/redhat/RPMS/i386 (binary) and /usr/src/redhat/SRPMS (source)
#   update version references in web:snd.html, snd.h, sourceforge index.html, configure.ac, NEWS
