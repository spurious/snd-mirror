#!/usr/bin/env ruby -w
# bess -- Translation of Bill Schottstaedt's bess.scm to Ruby.

# Copyright (C) 2002 Michael Scholz

# Author: Michael Scholz <scholz-micha@gmx.de>
# Created: Sun Sep 15 19:11:12 CEST 2002
# Last: Sun Oct 13 05:33:50 CEST 2002
# Version: $Revision: 1.3 $
# Keywords: sound, snd, clm

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

# Code:

DEBUG = true;
VER = "$Revision: 1.3 $";
FILE = File::basename($0);
BANNER = "This is #{FILE.upcase} v#{VER.scan(/[\d.]+/)}, (c) 2002 by Michael Scholz";

# options
$vers = false;
$help = false;
$play = 0.0;
$freq = 220.0;
$index = 1.0;
$ratio = 1;
$amp = 0.5;
$sliderback = "lightsteelblue";
$background = "lightsteelblue1";

# needed by usage()
$low_freq = 40.0;
$high_freq = 2000.0;
$high_index = 3.0;
$high_ratio = 10;

def usage
  print <<USAGE;
#{BANNER}

#{FILE.capitalize} is a Ruby script working with sndlib.so and
libxm.so which must be in the Ruby library path, for example in
/usr/local/lib/ruby/site_ruby, or the environment variable $RUBYLIB
must be set correctly. It opens the DAC, creates some scale widgets,
and starts two CLM oscillators doing frequency modulation in
semi-real-time. This is a translation of bess.scm of Bill
Schottstaedt\'s Snd sound editor.

Default values shown in brackets.

Usage: #{FILE} [ options ] [ -- X options ]

   -p, --play               play immediately (#{$play != 0.0 ? "yes" : "no"})

   -f, --frequency NUMBER   frequency between #{$low_freq} and #{$high_freq} (#{$freq})
   -i, --index NUMBER       index between 0 and #{$high_index} (#{$index})
   -r, --ratio NUMBER       ratio between 0 and #{$high_ratio} (#{$ratio})
   -a, --amplitude NUMBER   amplitude between 0 and 1 (#{$amp})

   -b  --background COLOR   background color (#{$background})
   -s, --sliderback COLOR   slider background color (#{$sliderback})

   -V, --version            display version information and exit
   -h, -?, --help           display this help message and exit

Example: #{FILE} -pf1000 -r3 -b ivory1 -s ivory3
USAGE
  exit(1);
end

def version
  print <<VERSION;
#{BANNER}
#{FILE.capitalize} comes with ABSOLUTELY NO WARRANTY.
You may redistribute copies of #{FILE.capitalize}
under the terms of the GNU General Public License.
For more information about these matters, see the file named COPYING.
VERSION
  exit(1);
end

def warn(str = "Warning")
  STDERR.printf("%s%s\n", str, $! ? ": #{$!}" : "");
  STDERR.print("[#{$@}]\n") if $@ and DEBUG;
end

def die(str = "Error", n = 1)
  warn(str);
  exit(n);
end

def get_color(color)
  col = RXColor();
  dpy = RXtDisplay($shell_app[0]);
  cmap = RDefaultColormap(dpy, RDefaultScreen(dpy));
  warn("Can't allocate \"#{color}\"!") if RXAllocNamedColor(dpy, cmap, color, col, col) == 0;
  Rpixel(col);
end

def set_label(w, str)
  RXtVaSetValues(w, [RXmNlabelString, RXmStringCreate(str, RXmFONTLIST_DEFAULT_TAG)]);
end

def make_label(name, alignwidget, form)
  RXtCreateManagedWidget(name, RxmLabelWidgetClass, form,
			 [RXmNleftAttachment, RXmATTACH_FORM,
			   RXmNbottomAttachment, RXmATTACH_NONE,
			   RXmNtopAttachment, RXmATTACH_WIDGET,
			   RXmNtopWidget, alignwidget,
			   RXmNrightAttachment, RXmATTACH_NONE,
			   RXmNalignment, RXmALIGNMENT_END,
			   RXmNwidth, 114,
			   RXmNrecomputeSize, false,
			   RXmNbackground, get_color($background)]);
end

def make_scale_label(alignwidget, form)
  RXtCreateManagedWidget("label", RxmLabelWidgetClass, form,
			 [RXmNleftAttachment, RXmATTACH_WIDGET,
			   RXmNleftWidget, alignwidget,
			   RXmNbottomAttachment, RXmATTACH_NONE,
			   RXmNtopAttachment, RXmATTACH_OPPOSITE_WIDGET,
			   RXmNtopWidget, alignwidget,
			   RXmNrightAttachment, RXmATTACH_NONE,
			   RXmNbackground, get_color($background)]);
end

def make_scale(alignwidget, form)
  RXtCreateManagedWidget("scale", RxmScaleWidgetClass, form,
			 [RXmNleftAttachment, RXmATTACH_WIDGET,
			   RXmNleftWidget, alignwidget,
			   RXmNbottomAttachment, RXmATTACH_NONE,
			   RXmNtopAttachment, RXmATTACH_OPPOSITE_WIDGET,
			   RXmNtopWidget, alignwidget,
			   RXmNrightAttachment, RXmATTACH_FORM,
			   RXmNshowValue, false,
			   RXmNorientation, RXmHORIZONTAL,
			   RXmNbackground, get_color($sliderback)]);
end

def freq_callback(w, c, i)
  $freq = $low_freq + Rvalue(i) * (($high_freq - $low_freq) / 100.0);
  set_label(c, format("%8.3f", $freq));
end

def amp_callback(w, c, i)
  $amp = Rvalue(i) / 100.0;
  set_label(c, format("%8.3f", $amp));
end

def fm_callback(w, c, i)
  $index = Rvalue(i) * ($high_index / 100.0);
  set_label(c, format("%8.3f", $index));
end

def ratio_callback(w, c, i)
  $ratio = (Rvalue(i) * ($high_ratio / 100.0)).to_i;
  set_label(c, format("%8d", $ratio));
end

for i in 1 .. 15		# SIGHUP .. SIGTERM
  if(trap(i, "SIG_IGN") != 0)	# 0 for SIG_IGN
    trap(i) { |sig| 
      die("\nSignal #{sig} received.  Process canceled.", 2);
    }
  end
end

begin
  require "libxm";

  args = ([$0] << $*).flatten;
  $shell_app = RXtVaOpenApplication("FM", args.length, args,
				    RapplicationShellWidgetClass,
				    [RXmNallowShellResize, true,
				      RXmNtitle, "FM forever!"]);

  begin
    require "getoptlong";

    GetoptLong.new(["--play", "-p", GetoptLong::NO_ARGUMENT],
		   ["--frequency", "-f", GetoptLong::REQUIRED_ARGUMENT],
		   ["--index", "-i", GetoptLong::REQUIRED_ARGUMENT],
		   ["--ratio", "-r", GetoptLong::REQUIRED_ARGUMENT],
		   ["--amplitude", "-a", GetoptLong::REQUIRED_ARGUMENT],
		   ["--background", "-b", GetoptLong::REQUIRED_ARGUMENT],
		   ["--sliderback", "-s", GetoptLong::REQUIRED_ARGUMENT],
		   ["--version", "-V", GetoptLong::NO_ARGUMENT],
		   ["--help", "-h", "-?", GetoptLong::NO_ARGUMENT]).each { |name, arg|

      case name
      when "--play"
	$play = 1.0;
      when "--frequency"
	freq = arg.to_f.abs;
	$freq = freq < $low_freq ? $low_freq : 
	  freq > $high_freq ? $high_freq : freq;
      when "--index"
	ind = arg.to_f.abs;
	$index = ind > $high_index ? $high_index : ind;
      when "--ratio"
	rat = arg.to_i.abs;
	$ratio = rat > $high_ratio ? $high_ratio : rat;
      when "--amplitude"
	amp = arg.to_f.abs;
	$amp = amp > 1 ? 1 : amp;
      when "--sliderback"
	$sliderback = arg;
      when "--background"
	$background = arg;
      when "--version"
	$vers = true;
      when "--help"
	$help = true;
      end
    }
  rescue
    usage();
  end

  usage() if $help;
  version() if $vers;

  require "sndlib";

  app = $shell_app[1];
  shell = $shell_app[0];
  carosc = make_oscil(0.0);
  modosc = make_oscil(0.0);

  RXtAddEventHandler(shell, 0, true, lambda { |w, c, i, f| R_XEditResCheckMessages(w, c, i, f) });
  form = RXtCreateManagedWidget("form", RxmFormWidgetClass, shell,
				[RXmNresizePolicy, RXmRESIZE_GROW,
				  RXmNbackground, get_color($background)]);
  
  play_button = RXtCreateManagedWidget("Play", RxmToggleButtonWidgetClass, form,
				       [RXmNtopAttachment, RXmATTACH_FORM,
					 RXmNleftAttachment, RXmATTACH_FORM,
					 RXmNrightAttachment, RXmATTACH_NONE,
					 RXmNbottomAttachment, RXmATTACH_NONE,
					 RXmNbackground, get_color($background)]);

  RXmToggleButtonSetState(play_button, $play != 0.0 ? true : false, false);

  quit_button = RXtCreateManagedWidget(" Quit ", RxmPushButtonWidgetClass, form,
				       [RXmNtopAttachment, RXmATTACH_FORM,
					 RXmNleftAttachment, RXmATTACH_NONE,
					 RXmNrightAttachment, RXmATTACH_FORM,
					 RXmNbottomAttachment, RXmATTACH_NONE,
					 RXmNbackground, get_color($background)]);

  freq_label = make_scale_label(make_label("Carrier Frequency:", play_button, form), form);
  freq_scale = make_scale(freq_label, form);

  amp_label = make_scale_label(make_label("Amplitude:", freq_label, form), form);
  amp_scale = make_scale(amp_label, form);

  fm_label = make_scale_label(make_label("FM Index:", amp_label, form), form);
  fm_scale = make_scale(fm_label, form);

  cm_label = make_scale_label(make_label("C/M Ratio:", fm_label, form), form);
  cm_scale = make_scale(cm_label, form);

  RXtAddCallback(freq_scale, RXmNdragCallback, 
		 lambda { |w, c, i| freq_callback(w, c, i); }, freq_label);
  RXtAddCallback(freq_scale, RXmNvalueChangedCallback, 
		 lambda { |w, c, i| freq_callback(w, c, i); }, freq_label);

  RXtAddCallback(amp_scale, RXmNdragCallback, 
		 lambda { |w, c, i| amp_callback(w, c, i); }, amp_label);
  RXtAddCallback(amp_scale, RXmNvalueChangedCallback, 
		 lambda { |w, c, i| amp_callback(w, c, i); }, amp_label);

  RXtAddCallback(fm_scale, RXmNdragCallback, 
		 lambda { |w, c, i| fm_callback(w, c, i); }, fm_label);
  RXtAddCallback(fm_scale, RXmNvalueChangedCallback, 
		 lambda { |w, c, i| fm_callback(w, c, i); }, fm_label);

  RXtAddCallback(cm_scale, RXmNdragCallback, 
		 lambda { |w, c, i| ratio_callback(w, c, i); }, cm_label);
  RXtAddCallback(cm_scale, RXmNvalueChangedCallback, 
		 lambda { |w, c, i| ratio_callback(w, c, i); }, cm_label);

  RXtAddCallback(play_button, RXmNvalueChangedCallback, 
		 lambda { |w, c, i| $play = Rset(i) ? 1.0 : 0.0; });
  RXtAddCallback(quit_button, RXmNactivateCallback, 
		 lambda { |w, c, i| exit(0); });

  set_label(freq_label, format("%8.3f", $freq));
  set_label(amp_label, format("%8.3f", $amp));
  set_label(fm_label, format("%8.3f", $index));
  set_label(cm_label, format("%8d", $ratio));

  RXmScaleSetValue(freq_scale, (100 * ($freq - $low_freq) / ($high_freq - $low_freq)).to_i);
  RXmScaleSetValue(amp_scale, (100 * $amp).to_i);
  RXmScaleSetValue(fm_scale, (100 * $index / $high_index).to_i);
  RXmScaleSetValue(cm_scale, ($ratio * 100 / $high_ratio).to_i);

  bufsize = 256;
  srate = 22050;
  chans = 1;
  data = make_sound_data(chans, bufsize);
  port = mus_audio_open_output(Mus_audio_default, srate, chans, Mus_lshort, bufsize * 2);

  die("Can't open DAC!") if port < 0;

  RXtAppAddWorkProc(app, lambda { |dummy|
		      (0...bufsize).each { |i|
			sound_data_set!(data, 0, i, $amp * $play *
					oscil(carosc, hz2radians($freq) + $index * 
					      oscil(modosc, hz2radians($ratio * $freq))));
		      }
		      mus_audio_write(port, data, bufsize);
		      false;
		    });

  RXtRealizeWidget(shell);
  RXtAppMainLoop(app);
rescue
  die("RXtAppMainLoop()");
end

# bess.rb ends here
