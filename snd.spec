# RPM spec file for Snd

%define prefix   /usr

Summary:	A sound editor
Name:		snd
Version:	4
Release:	3
Copyright:	LGPL
Group:		Applications/Sound
Source:		ftp://ccrma-ftp.stanford.edu/pub/Lisp/snd-4.tar.gz
URL:		http://ccrma-www/CCRMA/Software/snd/snd.html
Vendor:		CCRMA/Music Stanford University
Packager:	Bill Schottstaedt <bil@ccrma.stanford.edu>
Docdir:         %{prefix}/doc


%description
Snd is a sound editor modelled loosely after Emacs and an old, sorely-missed
PDP-10 sound editor named Dpysnd. It can accomodate any number of sounds
each with any number of channels, and can be customized and extended
using guile and guile-gtk. Snd is free (LGPL); the code is available via
anonymous ftp at ccrma-ftp.stanford.edu as pub/Lisp/snd-4.tar.gz.

%prep
%setup

%build
%ifos Linux
%ifarch i386
make -f makefile.rpm prefix=%{prefix}
%else
make -f makefile.ppcrpm
%endif
%endif

%install
install -m 755 -o 0 -g 0 -d %{prefix}/local/bin/
install -m 755 -o 0 -g 0 snd %{prefix}/local/bin

%files
%defattr(-, root, root)
%doc README.Snd HISTORY.Snd snd.html snd.txt extsnd.html extsnd.txt sndlib.html sndlib.txt grfsnd.html grfsnd.txt clm.html
%doc title.png controls.png auto.png files.png regions.png mixer.png rec.png reclin.png hfft.png energy.png s.png n.png d.png env.png
%doc examp.scm mix.scm env.scm snd-test.scm snd-gtk.scm index.scm gm.scm old-sndlib2scm.scm rgb.scm moog.scm v.scm bell.scm pqwvox.scm 
%doc marks.scm setf.scm
%{prefix}/local/bin/snd

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
#   update version references in README.Snd, ftp:LISP.README, web:snd.html, snd.h
#   linux-snd:  makefile.guile-static
#   xlinux-snd: makefile.xlinux
#   sun-snd:    makefile.solaris
