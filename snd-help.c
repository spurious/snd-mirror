/* TODO: snd-help should call netscape if help-with-html flag is #t
*/

#include "snd.h"
#include "sndlib-strings.h"
#include "vct.h"


/* ---------------- help 'news' menu item ---------------- */

static char *snd_itoa(int n)
{
  char *str;
  str = (char *)CALLOC(8,sizeof(char));
  sprintf(str,"%d",n);
  return(str);
}

#if HAVE_GSL
static char *snd_ftoa(float n)
{
  char *str;
  str = (char *)CALLOC(8,sizeof(char));
  sprintf(str,"%.1f",n);
  return(str);
}
#endif

#ifdef USE_MOTIF
  #include <X11/IntrinsicP.h>
  #if HAVE_XPM
    #include <X11/xpm.h>
  #endif
  #if HAVE_HTML
    #include <XmHTML/XmHTML.h>
  #endif
#endif

#if HAVE_GUILE
static char *guile_version(void) 
{ 
  return(gh_scm2newstr(scm_version(),NULL));
}
#endif

static char *sndlib_consistency_check(void)
{
#if SNDLIB_USE_FLOATS
  if (mus_sample_bits() > 0) 
    return(" Snd built expecting float samples, but sndlib uses int!"); 
#else
  char *buf;
  if (mus_sample_bits() == 0)
    return(" Snd built expecting int samples, but sndlib uses float!"); 
  else
    if (mus_sample_bits() != MUS_SAMPLE_BITS)
      {
	buf = (char *)CALLOC(32,sizeof(char)); /* memory leak here is the least of our worries... */
	sprintf(buf," Snd expects %d bit int samples, but sndlib uses %d bits!",
		MUS_SAMPLE_BITS,
		mus_sample_bits());
	return(buf);
      }
#endif  
  return("");
}

#if HAVE_GDBM
  #include <gdbm.h>
#endif

#if HAVE_GNU_LIBC_VERSION_H
  #include <gnu/libc-version.h>
#endif

static char* vstrcat(char *buf,...)
{
  va_list ap;
  char *str;
  va_start(ap,buf);
  while ((str = va_arg(ap,char *)))
    {
      strcat(buf,str);
    }
  va_end(ap);
  return(buf);
}

char *version_info(void)
{
  #define NUM_ITOAS 19
  char *buf;
  char **itoa;
  int i;
  buf = (char *)CALLOC(1024,sizeof(char));
  itoa = (char **)CALLOC(NUM_ITOAS,sizeof(char *));
  vstrcat(buf,
	  "This is Snd version ",
	  SND_RPM_VERSION,
	  " of ",
	  SND_VERSION,
	  ": ",
#ifdef SND_AS_WIDGET
	  "\n    (compiled as a widget)",
#endif
#if HAVE_GUILE
	  "\n    Guile ",guile_version(),
#ifdef LIBGUILE_VERSION
	  " libguile.so.",itoa[18]=snd_itoa(LIBGUILE_VERSION),
#endif
#endif
	  "\n    CLM ",itoa[0]=snd_itoa(MUS_VERSION),".",itoa[1]=snd_itoa(MUS_REVISION)," (",MUS_DATE,")",
#if ((HAVE_XPM) && (defined(USE_MOTIF)))
	  "\n    Xpm ",itoa[2]=snd_itoa(XpmFormat),".",itoa[3]=snd_itoa(XpmVersion),".",itoa[4]=snd_itoa(XpmRevision),
#endif
#ifdef SND_CONF
	  "\n    conf: ",SND_CONF,
#endif
#if HAVE_HTML
	  "\n    XmHTML ",itoa[5]=snd_itoa(XmHTMLVERSION),".",itoa[6]=snd_itoa(XmHTMLREVISION),".",itoa[7]=snd_itoa(XmHTMLUPDATE_LEVEL),
#endif
#ifdef USE_MOTIF
#ifdef LESSTIF_VERSION
	  "\n    Lesstif ",itoa[8]=snd_itoa(LESSTIF_VERSION),".",itoa[9]=snd_itoa(LESSTIF_REVISION)," ",
#endif
	  "\n    Motif ",itoa[10]=snd_itoa(XmVERSION),".",itoa[11]=snd_itoa(XmREVISION),".",itoa[12]=snd_itoa(XmUPDATE_LEVEL)," X",itoa[13]=snd_itoa(X_PROTOCOL),"R",itoa[14]=snd_itoa(XT_REVISION),
#endif
#ifdef USE_GTK
	  "\n    Gtk+ ",itoa[9]=snd_itoa(GTK_MAJOR_VERSION),".",itoa[10]=snd_itoa(GTK_MINOR_VERSION),".",itoa[11]=snd_itoa(GTK_MICRO_VERSION),", Glib ",itoa[12]=snd_itoa(GLIB_MAJOR_VERSION),".",itoa[13]=snd_itoa(GLIB_MINOR_VERSION),".",itoa[14]=snd_itoa(GLIB_MICRO_VERSION),
#endif
#if HAVE_GUILE_GTK
	  ", with guile-gtk",
#endif
#if (!(defined(USE_MOTIF))) && (!(defined(USE_GTK)))
	  "\n    without any graphics system",
#endif
#ifdef WITH_BIG_COLORMAP
	  ", (with big colormaps)",
#endif
#ifdef CCRMA
	  "\n    (uses ccrma-specific /zap dirs)",
#endif
	  "\n    ",mus_audio_moniker(),
	  "\n    Sndlib ",itoa[15]=snd_itoa(SNDLIB_VERSION),".",itoa[16]=snd_itoa(SNDLIB_REVISION)," (",SNDLIB_DATE,
#if SNDLIB_USE_FLOATS
	  ", float samples",
#else
	  ", int",itoa[17]=snd_itoa(MUS_SAMPLE_BITS)," samples",
#endif
	  ")",sndlib_consistency_check(),
#if HAVE_GDBM
	  "\n    gdbm: ",gdbm_version,
#endif
#if HAVE_GSL
	  "\n    gsl",
  #ifdef GSL_VERSION
          ": ",snd_ftoa(GSL_VERSION),
  #endif
#endif
#if HAVE_LADSPA
	  "\n    with LADSPA",
#endif
#ifdef __DATE__
	  "\n    Compiled ",__DATE__," ",__TIME__,
#endif
#ifdef __VERSION__
  #ifndef __cplusplus
	  "\n    C: ",
  #else
	  "\n    C++: ",
  #endif
	  __VERSION__,
#endif
#if HAVE_GNU_LIBC_VERSION_H
	  "\n    Libc: ",gnu_get_libc_version(),".",gnu_get_libc_release(),
#endif
	  "\n",NULL);
  for (i=0;i<NUM_ITOAS;i++) if (itoa[i]) FREE(itoa[i]);
  FREE(itoa);
  return(buf);
}

void news_help(snd_state *ss)
{
  char *info;
  info = version_info();
  ssnd_help(ss,STR_News,
	    info,
	    "\n",
	    "Recent changes include:\n\
\n\
25-Jul:  Snd now uses clm for all control-panel functions (play/apply).\n\
         added name-click-hook.\n\
24-Jul:  snd-tempnam, sound-interp gen in examp.scm and env-sound-interp.\n\
20-Jul:  added sum-of-sines, phase-vocoder.\n\
18-Jul:  play-selection and added end arg to play etc.\n\
         C-u 0 C-x p -> play region 0, C-x p -> play selection.\n\
17-Jul:  optargs keyword bug was caused by string-length which existed for guile 1.3.0 (fixed).\n\
14-Jul:  Snd appears to work in FreeBSD (thanks to Heiko Recktenwald).\n\
         pvoc.scm, loop.scm, fmv.scm.\n\
         -l no longer necessary -- if file has .scm extension, it is assumed to be Scheme.\n\
10-Jul:  added sounds (list of currently active sounds).\n\
         region-id and id-region, regions now returns a list of active region ids.\n\
         mixes now returns a list similar to marks.\n\
         removed active-sounds.\n\
6-Jul:   snd 4.4.\n\
         user-specified menu callbacks must be functions (not strings).\n\
         removed dsp-devices.\n\
5-Jul:   experimental erase-zeros flag.\n\
         swap-channels\n\
3-Jul:   save-hook and auto-save (examp.scm) improvements.\n\
         ok? renamed sound?, mix-ok? -> mix?, added mark?, region?, selection?\n\
         setf.scm for (setf (<var>) <val>) in place of (set-<var> <val>).\n\
         control-panel reverb can handle any number of chans (and now gets input from all).\n\
         show-axes variable changed: now has 3 values, show-all-axes (default), show-no-axes, show-x-axis.\n\
           removed View:Show Axes menu item; also these no longer affect the control-panel filter graph.\n\
",
NULL);
  FREE(info);
}


/* ---------------- help menu strings ---------------- */

static char file_menu_help_string[] =
"  Open: open a file.\n\
  Close: close the currently selected file.\n\
  Save: save any edits on the current file.\n\
  Save as: save edits on the current file,\n\
      in some new file.\n\
  Revert: undo any edits on the current file.\n\
  Mix: mix in a file.\n\
  Update: reload current sound from disk.\n\
  New: create an empty sound edit window.\n\
  Record: fire up the recorder\n\
  View: open a file in read-only mode.\n\
  Print: produce graph as Postscript file.\n\
  Exit: leave Snd, without saving any edits.\n\
";

static char edit_menu_help_string[] =
"  Undo: undo the last edit in the current file.\n\
  Redo: redo the undone edit.\n\
  Find: a global search -- operates across all\n\
     currently sync'd sounds.\n\
  Cut: store, then delete the selection.\n\
  Paste: insert the selection at the current location\n\
  Mix selection: add the selection at the current location\n\
  Play selection: play the current selection.\n\
  Save selection: save the current selection in a file.\n\
  Select all: select entire file (following sync).\n\
  Edit Envelope: start the envelope editor dialog.\n\
  Edit Header: view or edit file's header.\n\
  Show Edit History: show edits as text.\n\
";

static char view_menu_help_string[] =
"  Show Controls: display the control pane.\n\
  Normalize: During editing with multiple\n\
     files and channels, some data may be\n\
     obscured or compressed by changed window\n\
     sizes.  Normalize returns Snd to a state\n\
     where everything is equally compressed.\n\
  Channel style: combine all channels into\n\
     one graph.\n\
  Graph style: display data as dots, lines or,\n\
     filled polygons.\n\
  Show Marks: display mark locations using a\n\
     long vertical line.\n\
  Show Y=0: display the y=0 line.\n\
  Verbose cursor: show cursor loc and sample value.\n\
  Regions: fire up the region browser.\n\
  Files: fire up the file browser.\n\
  Color: color browser for sonogram.\n\
  Orientation: sonogram orientation.\n\
  Hide consoles: currently unimplemented.\n\
  X axis units: x axis labels in seconds, samples, ect.\n\
";

static char options_menu_help_string[] = 
"   Transform Options: various fft-related settings\n\
   Speed style: control panel speed scrollbar interpretation.\n\
   Zoom focus: where to focus during zooms.\n\
   Save Options: save the current Options and View menu settings.\n\
   Save state: save current state of Snd.\n\
\n\
";

static char help_menu_help_string[] =   
"  Click for Help: if you choose this, the\n\
      mouse cursor becomes a '?'; position\n\
      the cursor over the portion of the Snd\n\
      window that interests you, and click\n\
      the button.\n\
  About Snd: this text.\n\
  FFT: a discussion of Snd's FFT options.\n\
  Find: how to perform searches.\n\
  Undo/Redo: how to back up while editing.\n\
  Sync: how to perform multichannel operations.\n\
  Speed: how to change sampling rate.\n\
  Expand: how to change tempo.\n\
  Reverb: how to reverberate.\n\
  Contrast: how to add contrast enhancement.\n\
  Envelope: how to apply an envelope.\n\
  Marks: about marks in Snd.\n\
  Mixing: about mixing in Snd.\n\
  Formats: Snd-supported formats and headers.\n\
  Customization: how to customize Snd.\n\
  User functions: how to add your own functions to Snd.\n\
  Recording: how to use the recorder.\n\
";

static char about_snd_help_string[] = 
"Snd is a sound editor. See snd.html or snd.txt for\n\
full details.  Please send bug reports or suggestions to\n\
bil@ccrma.stanford.edu.\n\
\n\
To get started, go to the File menu, and\n\
open a sound file.  To hear the sound, click\n\
the 'play' button. To see an fft, click the\n\
'f' button on the left.  The left mouse button\n\
is used for most pointing operations; the\n\
middle button pastes in the current selection;\n\
the right button brings up the Snd popup menu.\n\
\n\
";

#ifndef _MSC_VER
static char graph_help_string[] =
"  [Down] zoom out, amount depends on state keys\n\
  [Up] zoom in\n\
  [Left] move window left, amount depends on state keys\n\
  [Right] move window right\n\
  <:   move cursor to sample 0\n\
  >:   move cursor to last sample\n\
  c-<: move cursor to sample 0\n\
  c->: move cursor to last sample\n\
  c-a: move cursor to window start\n\
  c-b: move cursor back one sample\n\
  c-d: delete sample at cursor\n\
  c-e: move cursor to window end\n\
  c-f: move cursor ahead one sample\n\
  c-g: abort current command\n\
  c-h: delete previous sample\n\
  c-i: display cursor info\n\
  c-j: goto mark\n\
  c-k: delete one line's worth of samples\n\
  c-l: position window so cursor is in the middle\n\
  c-m: place (or remove) mark at cursor location\n\
  c-n: move cursor ahead one 'line'\n\
  c-o: insert one zero sample at cursor\n\
  c-p: move cursor back one 'line'\n\
  c-q: play current channel starting at cursor\n\
  c-r: repeat last search backwards\n\
  c-s: search according to an expression\n\
       'y' is the current sample value,\n\
       'y(n) is the value of the sample n from the current\n\
       the find expression syntax is C-like and\n\
       supports much of C and its math library.\n\
       c-s y>.1 finds the next sample greater than .1\n\
  c-t: stop playing\n\
  c-u: start count definition.  If followed by a\n\
       float, the actual count is that number multiplied\n\
       by the current sampling rate.  If the optional\n\
       number is followed by c-m, the count returned\n\
       is the distance from the cursor to the n-th\n\
       successive mark.  That is, c-u c-m c-f is the\n\
       same as c-j.\n\
  c-v: move cursor to mid-window\n\
  c-w: delete current region\n\
  c-x: start extended command (see below)\n\
  c-y: paste in last deleted region. Use c-u\n\
       to paste in earlier regions.\n\
  c-z: set sample at cursor to 0.0\n\
  c-_: undo\n\
  c-[Space]: start region definition\n\
       - c-[Space] to remove region\n\
\n\
The extended commands (preceded by c-x) are:\n\
  a: apply envelope to selection\n\
  b: position window so cursor is on left margin\n\
  c: define selection from cursor to nth mark\n\
  d: set temp dir name\n\
  e: execute last keyboard macro\n\
  f: position window so cursor is on right margin\n\
  i: insert region\n\
  j: goto named mark\n\
  k: close file\n\
  l: position selection in mid-view\n\
  n: re-evaluate expression over selection\n\
  o: move to next or previous graph\n\
  p: play selection or region n\n\
  q: mix in region\n\
  r: redo last undone edit\n\
  u: undo last edit\n\
  v: position window over current selection\n\
  w: save selection as file\n\
  x: evaluate expression over selection\n\
  z: smooth selection\n\
  /: place named mark\n\
  (: begin keyboard macro definition\n\
  ): end keyboard macro definition\n\
\n\
  c-a: apply envelope.  If a count is specified,\n\
     the envelope is applied from the cursor for\n\
     that number of samples.  Otherwise, the\n\
     envelope is applied to the entire file, and\n\
     if syncing is on, all sync'd channels.\n\
  c-b: set x window bounds (preceded by 1 arg)\n\
  c-c: hide controls\n\
  c-d: print\n\
  c-e: give last keyboard macro a name\n\
  c-f: open file\n\
  c-g: abort command\n\
  c-i: insert file\n\
  c-m: add named mark\n\
  c-n: re-evaluate expression\n\
  c-o: show controls\n\
  c-p: set window size (preceded by 1 arg)\n\
  c-q: mix in file\n\
  c-r: redo last undone edit\n\
  c-s: save file\n\
  c-u: undo last edit\n\
       Snd supports 'unlimited undo/redo'\n\
  c-v: set window size as percentage of total\n\
  c-w: save current channel in file\n\
  c-x: evaluate expression\n\
  c-z: smooth using cosine\n\
\n\
Unless otherwise noted, case is not significant;\n\
c-a is the same as c-A.\n\
\n\
Most commands can be prefaced by an integer or\n\
a float; the integer causes the command to be repeated\n\
that many times; the float is multiplied by the\n\
sound's sampling rate, then applied that many times.\n\
So, for example, c-u 1.0 c-f causes the cursor to move\n\
ahead one second in the sound.\n\
\n\
The y-axis limit (default = 1.0) can be set\n\
in the minibuffer via the variable 'ymax'.\n\
\n\
To change a key binding, use " S_bind_key ".\n\
\n\
The Tab key in a text field invokes a\n\
context-sensitive completion function that tries\n\
to figure out what the rest of the text probably\n\
should be.  If it finds no matches, the text\n\
flashes red; if it finds multiple matches and\n\
can't extend the current text, it flashes green,\n\
and pops up the help window with a list of possible\n\
completions.  If there is no completion routine active,\n\
Tab is a no-op.\n\
";
#else
static char graph_help_string[] = "";
#endif

static char fft_keypad_help_string[] = 
"The keypad keys are mapped to various variables as follows:\n\
\n\
    variable         increase           decrease\n\
  " S_spectro_cutoff "   PageUp (9)          PageDown (3)\n\
  " S_spectro_hop "      Add (+)             Subtract (-)\n\
  " S_spectro_z_angle "  RightArrow (6)      LeftArrow (4)\n\
  " S_spectro_x_angle "  Ctrl-UpArrow (8)    Ctrl-DownArrow (2)\n\
  " S_spectro_y_angle "  Ctrl-RightArrow (6) Ctrl-LeftArrow (4)\n\
  " S_spectro_z_scale "  UpArrow (8)         DownArrow (2)\n\
  " S_fft_size "         Multiply (*)        Divide (/)\n\
  " S_dot_size "         Delete (.)          Insert (0)\n\
\n\
You can rotate the spectrogram around the various axes\n\
by holding down the keypad and control keys.  You can get\n\
arbitrarily small or large ffts with the Multiply and\n\
Divide keys.  The x and y axis scalers are named\n\
" S_spectro_x_scale " and " S_spectro_y_scale "; they can be set via\n\
M-X and setf.  The keypad Enter key resets all the\n\
spectrogram variables to their default values.\n\
(In Linux, use the corresponding numbered keys --\n\
add shift to the key sequences given above).\n\
See also the Color and Orientation menu options\n\
in the View menu.\n\
\n\
";

static char fft_help_string[] = 
"The FFT performs a projection of the\n\
time domain into the frequency domain.\n\
Good discussions of the Fourier Transform\n\
and the trick used in the FFT itself\n\
can be found in many DSP books; those\n\
I know of include 'A Digital Signal Processing\n\
Primer', Ken Steiglitz, Addison-Wesley,\n\
1996; or 'Numerical Recipes in C'.\n\
\n\
The FFT size can be any power of 2. The\n\
larger, the longer it takes to compute,\n\
and the larger the amount of the time domain\n\
that gets consumed.  Interpretation of the\n\
FFT results is not straightforward!\n\
\n\
The window choices are taken primarily\n\
from Harris' article.\n\
\n\
  Fredric J. Harris, 'On the Use of Windows\n\
     for Harmonic Analysis with the Discrete\n\
     Fourier Transform', Proceedings of the\n\
     IEEE, Vol. 66, No. 1, January 1978.\n\
\n\
with updates from:\n\
\n\
  Albert H. Nuttall, 'Some Windows with Very\n\
     Good Sidelobe Behaviour', IEEE Transactions\n\
     of Acoustics, Speech, and Signal Processing,\n\
     Vol. ASSP-29, 1, February 1981.\n\
\n\
\n\
Nearly all the transform-related choices are set\n\
by the transform dialog launched from the Options\n\
Menu Transform item. Most of this dialog should be\n\
self-explanatory.  Some of the windows take an\n\
additional parameter sometimes known as alpha or\n\
beta.  This is set in Snd by the scroller in the\n\
transform dialog.\n\
\n\
The FFT display is activated by setting the 'f'\n\
button on the channel's window.  It then updates\n\
itself each time the time domain waveform moves or\n\
changes.  The update function runs in the\n\
background, so in some cases, notably very large\n\
FFTs, you will notice that the FFT updates less\n\
often than the time domain.\n\
\n\
The spectrum data is usually normalized to fit\n\
between 0.0 to 1.0; if you'd rather have un-normalized\n\
data (the y-axis in this case changes to reflect the\n\
data values, to some extent), set the variable\n\
" S_normalize_fft " to 0.\n\
\n\
The harmonic analysis function is normally the\n\
Fourier Transform, but others are available,\n\
including about 20 wavelet choices, the Hankel and\n\
Chebyshev transforms, and perhaps others.\n\
\n\
";

static char find_help_string[] = 
"Searches in Snd refer to the sound data, and are\n\
in general patterned after Emacs.  When you type\n\
c-s or c-r, the minibuffer below the graph is\n\
activated and you are asked for the search expression.\n\
The expression is a Scheme function of one argument,\n\
the current sample value.  It should return #t when the\n\
search is satisified.  For example, (lambda (n) (> n .1)\n\
looks for the next sample that is greater than .1.\n\
Successive c-s or c-r repeat the search.  c-x c-s can\n\
redefine the search pattern, which is also cleared in other\n\
events, much like Emacs.\n\
\n\
Normally, the search applies only to the current channel.\n\
To search all current files at once, use the Edit menu's\n\
find option.\n\
\n\
";

static char undo_help_string[] = 
"Snd supports unlimited undo in the\n\
sense that you can backup through all\n\
the edits since the last save, and at\n\
any point redo those edits.  Certain\n\
operations require that temporary files\n\
be written, so disk space may eventually\n\
become a problem.  Revert is the same\n\
as backing up to the last save.\n\
\n\
In addition, eight or so of the previous\n\
selections are saved on a stack accessible\n\
via c-y.\n\
\n\
";

static char sync_help_string[] = 
"The sync button causes certain operations to\n\
apply to all channels simultaneously.  In mono\n\
sounds, the sync button has a similar effect,\n\
but applied across multiple sounds.\n\
\n\
To get multi-channel selections, set the sync\n\
button, then define the selection (by dragging\n\
the mouse) in one channel, and the parallel\n\
portions of the other channels will also be\n\
selected.\n\
\n\
";

static char speed_help_string[] = 
"'Speed' refers to the rate at which the\n\
sound data is consumed during playback.\n\
Another term might be 'srate'.  Snd uses\n\
linear interpolation to perform the speed\n\
change.  The arrow button on the right determines\n\
the direction we move through the data.\n\
The scroll bar position is normally interpreted\n\
as a float between .05 and 20.  The Options\n\
Speed Style menu (or the " S_speed_style " variable)\n\
can change this to use semitones (actually microtones)\n\
or just-intonation ratios.  The number of equal\n\
divisions to the octave in the semitone case is\n\
set by the variable " S_speed_tones " (normally 12).\n\
\n\
";

static char expand_help_string[] = 
"'Expand' refers to a kind of granular\n\
synthesis used to change the tempo of events\n\
in the sound without changing pitch.  Successive\n\
short slices of the file are overlapped with\n\
the difference in size between the input and\n\
output hops (between successive slices) giving\n\
the change in tempo.  This doesn't work in all\n\
files -- it sometimes sounds like execrable reverb\n\
or is too buzzy -- but it certainly is more\n\
robust than the phase vocoder approach to the\n\
same problem. \n\
\n\
There are a variety of variables that control\n\
hop sizes, segment lengths, and overall segment\n\
envelopes.  These can be set in the minibuffer\n\
via m-x and setf, or in your Snd init file.\n\
The variables are:\n\
\n\
  " S_expand_ramp ": the length of the ramp up (.4, 0 to .5)\n\
  " S_expand_length ": the length of each slice (.15)\n\
  " S_expand_hop ": the hop size (.05)\n\
\n\
The expander is on only if the expand\n\
button is set.\n\
\n\
";

static char reverb_help_string[] = 
"The Snd reverberator is a version of Michael\n\
McNabb's Nrev.  In addition to the controls\n\
in the control pane, you can set the reverb\n\
feedback gains and the coefficient of the low\n\
pass filter in the allpass bank. The variables\n\
are '" S_reverb_feedback "' and '" S_reverb_lowpass "'.\n\
The reverb is on only if the reverb button is set.\n\
";

static char contrast_help_string[] = 
"'Contrast enhancement' is my name for this\n\
somewhat weird waveshaper or compander.  It\n\
phase-modulates a sound, which can in some\n\
cases make it sound sharper or brighter.\n\
For softer sounds, it causes only an amplitude\n\
change.  Contrast is on only if the contrast\n\
button is set.\n\
";

static char env_help_string[] = 
"An envelope in Snd is a list of x y\n\
break-point pairs. The x axis range is\n\
arbitrary. For example, to define a triangle\n\
curve: '(0 0 1 1 2 0). There is no (obvious) limit\n\
on the number of breakpoints.\n\
\n\
To apply an envelope to a sound, use the extended\n\
command C-x C-a. If this command gets a numeric\n\
argument, the envelope is applied from the cursor\n\
for that many samples.\n\
\n\
  C-x a     apply amplitude envelope to selection\n\
  C-x C-a   apply amplitude envelope to channel\n\
\n\
You can also specify a envelope name to the C-x C-a\n\
prompt.\n\
\n\
To scale a file or selection by or to some\n\
amplitude, use the M-x commands:\n\
\n\
  " S_scale_by " args\n\
  " S_scale_to " args\n\
  " S_scale_selection_by " args\n\
  " S_scale_selection_to " args\n\
\n\
" S_scale_by " scales the current sync'd channels by its\n\
arguments, and " S_scale_to " scales them to its\n\
arguments (a normalization). The arguments in\n\
each case are either a list of floats\n\
corresponding to each successsive member of the\n\
current set of sync'd channels, or just one\n\
argument. In the latter case, " S_scale_by " uses that\n\
scaler for all its channels, and " S_scale_to "\n\
normalizes all the channels together so that the\n\
loudest reaches that amplitude (that is, " S_scale_to "\n\
.5) when applied to a stereo file means that both\n\
channels are scaled by the same amount so that the\n\
loudest point in the file becomes .5).\n\
";

static char sound_files_help_string[] = 
"Snd can read and write any of the sound\n\
file data and header formats that CLM can\n\
handle:\n\
\n\
read/write (many data formats):\n\
     NeXT/Sun/DEC/AFsp\n\
     AIFF/AIFC\n\
     RIFF (Microsoft wave)\n\
     IRCAM (old style)\n\
     NIST-sphere\n\
     no header\n\
     ----\n\
read-only (in selected data formats):\n\
     8SVX (IFF), EBICSF, INRS, ESPS,\n\
     SPPACK, ADC (OGI), AVR, VOC,\n\
     Sound Tools, Turtle Beach SMP, SoundFont 2.0,\n\
     Sound Designer I and II, PSION, MAUD, Kurzweil 2000,\n\
     Tandy DeskMate, Gravis Ultrasound, ASF, Ensoniq Paris,\n\
     Comdisco SPW, Goldwave sample, omf, quicktime\n\
     Sonic Foundry, SBStudio II, Delusion digital,\n\
     Digiplayer ST3, Farandole Composer WaveSample,\n\
     Ultratracker WaveSample, Sample Dump exchange,\n\
     Yamaha SY85, SY99, and TX16, Covox v8, SPL, AVI,\n\
     ----\n\
automatically translated to Sun 16-bit, then read/write:\n\
     IEEE text, Mus10 SAM 16-bit (modes 1 and 4), IBM CVSD, AVI\n\
     NIST shortpack, HCOM, Intel and Oki (Dialogic) ADPCM, MIDI sample dump\n\
     G721, G723_24, G723_40, IFF Fibonacci and Exponential\n\
\n\
'Linear' here means 2's complement integer.\n\
The files can have any number of channels.\n\
Data can be either big or little endian.\n\
For mpeg translation, see the code in examp.scm.\n\
\n\
When edits are saved, files in the first\n\
group are changed in place; those in the second\n\
group are changed to use one of the first\n\
group's headers (normally Sun); those in\n\
the third group are translated when opened\n\
and an new (perhaps redundant) '.snd' extension\n\
is added to distinguish the original from the\n\
translated form; the latter is then treated\n\
as the original by the editor.\n\
\n\
";

static char mark_help_string[] = 
"A mark in Snd is attached to a particular\n\
sample in the sound data.  It moves with that\n\
sample as you edit the data, and if the sample\n\
is deleted, so is its mark.  Marks also follow\n\
the undo/redo edit history -- I'm not sure this\n\
is a good idea, but it seemed more intuitive\n\
than other alternatives.  This means that marks\n\
are 'undone' and 'redone' alongside the edits\n\
that they accompany.\n\
\n\
The mark symbol itself has three or four\n\
parts.  The name, if any, is at the top.\n\
Then a 'tab'.  You can click the name or\n\
tab portion and drag the mark to redefine it.\n\
Then a line to the bottom of the graph, showing\n\
where the mark is. And, below the x axis, an\n\
arrow.  You can click and drag the arrow to\n\
play the data following the mouse -- sort of\n\
like listening to a tape as you rock it back\n\
and forth by hand on the spindles. Or just\n\
click the arrow to play the data starting\n\
at the mark.\n\
\n\
";

static char init_file_help_string[] =
"Nearly everything in Snd can be set in an initialization file, loaded at any\n\
time from a saved-state (Guile) file, specified via inter-process communciation from any\n\
other program, invoked via M-x in the minibuffer, imbedded in a keyboard\n\
macro, or dealt with from the lisp listener panel. The syntax used is lisp;\n\
if the Guile library is loaded, the underlying language is actually Scheme,\n\
these entities are fully incorporated into lisp, and all of them can be used\n\
in arbitrarily complicated functions. I've tried to bring out to lisp nearly\n\
every portion of Snd, both the signal-processing functions, and much of the\n\
user interface. You can, for example, add your own menu choices, editing\n\
operations, or graphing alternatives. These extensions can be loaded at any\n\
time.\n\
\n\
";

static char constants_help_string[] = 
"Sndlib (selected header and data format types):\n\
  " S_mus_next "    " S_mus_aifc "     " S_mus_riff "\n\
  " S_mus_nist "    " S_mus_raw "      " S_mus_ircam "\n\
  " S_mus_bshort "  " S_mus_mulaw "    " S_mus_byte "    " S_mus_lshort "\n\
  " S_mus_bint "    " S_mus_alaw "     " S_mus_ubyte "   " S_mus_lfloat "\n\
  " S_mus_bdouble " " S_mus_b24int "   " S_mus_bfloat "  " S_mus_lint "\n\
\n\
FFT style (the Transform Options Display choice):\n\
  " S_normal_fft "         " S_sonogram "            " S_spectrogram "\n\
\n\
Transform type:\n\
  " S_fourier_transform "  " S_wavelet_transform "   " S_hankel_transform "  " S_chebyshev_transform "   " S_cepstrum "\n\
  " S_autocorrelation "    " S_walsh_transform "\n\
\n\
FFT Window type:\n\
  rectangular-window  hanning-window      welch-window       parzen-window\n\
  bartlett-window     hamming-window      blackman2-window   blackman3-window\n\
  blackman4-window    exponential-window  riemann-window     kaiser-window\n\
  cauchy-window       poisson-window      gaussian-window    tukey-window\n\
\n\
Zoom Focus style:\n\
  " S_focus_left "         " S_focus_right "        " S_focus_active "      " S_focus_middle "\n\
\n\
X-axis Label:\n\
  " S_x_in_seconds "       " S_x_in_samples "       " S_x_to_one "\n\
\n\
Speed Control style:\n\
  " S_speed_as_float "     " S_speed_as_ratio "     " S_speed_as_semitone "\n\
\n\
Channel Combination style;\n\
  " S_channels_separate "  " S_channels_combined "  " S_channels_superimposed "\n\
\n\
Envelope Editor target:\n\
  " S_amplitude_env "      " S_spectrum_env "       " S_srate_env "\n\
\n\
Graph Line style:\n\
  " S_graph_lines "        " S_graph_dots "         " S_graph_filled "    " S_graph_lollipops "\n\
  " S_graph_dots_and_lines "\n\
\n\
Keyboard action choices:\n\
  " S_cursor_in_view "     " S_cursor_on_left "     " S_cursor_on_right "   " S_cursor_in_middle "\n\
  " S_cursor_update_display " " S_cursor_no_action " " S_cursor_claim_selection " " S_keyboard_no_action "\n\
\n\
Cursor style:\n\
  " S_cursor_cross "    " S_cursor_line "\n\
\n\
Axis choice:\n\
  " S_show_no_axes "    " S_show_all_axes "         " S_show_x_axis "\n\
";

static char variables_help_string[] =
"These variables are accessed as though each were a function\n\
of no arguments, and set using a function with \"set-\" prepended\n\
to the variable name.  For example, " S_auto_resize "'s current\n\
value can be accessed via (" S_auto_resize "), and set to a\n\
new value via (" S_set_auto_resize " #t). \n\
\n\
  " S_ask_before_overwrite "  #f\n\
  " S_audio_output_device "   " S_mus_audio_default "\n\
  " S_audio_state_file "      \"" AUDIO_STATE_FILE "\"\n\
  " S_auto_resize "           #t\n\
  " S_auto_update "           #f\n\
  " S_axis_label_font "       varies\n\
  " S_axis_numbers_font "     varies\n\
  " S_basic_color "           ivory2\n\
  " S_bold_button_font "      varies\n\
  " S_button_font "           varies\n\
  " S_channel_style "         " S_channels_separate "\n\
  " S_color_cutoff "          0.003\n\
  " S_color_inverted "        #t\n\
  " S_color_scale "           0.5\n\
  " S_colormap "             -1\n\
  " S_cursor_color "          red\n\
  " S_dac_folding "           #t\n\
  " S_dac_size "              256\n\
  " S_data_color "            black\n\
  " S_dot_size "              1\n\
  " S_enved_base "            1.0\n\
  " S_enved_clipping "        #f\n\
  " S_enved_dBing "           #f\n\
  " S_enved_exping "          #f\n\
  " S_enved_power "           3.0\n\
  " S_enved_target "          " S_amplitude_env "\n\
  " S_enved_waveform_color "  blue\n\
  " S_enved_waving "          #f\n\
  " S_eps_file "              \"snd.eps\"\n\
  " S_fft_beta "              0.0\n\
  " S_fft_log_frequency "     #f\n\
  " S_fft_log_magnitude "     #f\n\
  " S_fft_size "              256\n\
  " S_fft_style "             " S_normal_fft "\n\
  " S_fft_window "            blackman2-window\n\
  " S_filter_env_order "      40\n\
  " S_filter_waveform_color " blue\n\
  " S_fit_data_on_open "      #f\n\
  " S_graph_color "           white\n\
  " S_graph_cursor "          XC_crosshair (34)\n\
  " S_graph_style "           " S_graph_lines "\n\
  " S_graphs_horizontal "     #t\n\
  " S_help_text_font "        varies\n\
  " S_highlight_color "       ivory1\n\
  " S_initial_x0 "            0.0\n\
  " S_initial_x1 "            0.1\n\
  " S_initial_y0 "           -1.0\n\
  " S_initial_y1 "            1.0\n\
  " S_line_size "             128\n\
  " S_listener_color "        aliceblue\n\
  " S_listener_font "         varies\n\
  " S_listener_prompt "       \">\"\n\
  " S_mark_color "            red\n\
  " S_min_dB "               -60.0\n\
  " S_mix_color "             lightgreen\n\
  " S_mix_console_amp_scaler " 1.0\n\
  " S_mix_console_speed_scaler " 1.0\n\
  " S_mix_focus_color "       green2\n\
  " S_mix_waveform_color "    darkgray\n\
  " S_mix_waveform_height "   20\n\
  " S_movies "                #t\n\
  " S_normalize_fft "         #t\n\
  " S_normalize_on_open "     #t\n\
  " S_position_color "        ivory3\n\
  " S_prefix_arg "            nil\n\
  " S_print_length "          12\n\
  " S_pushed_button_color "   lightsteelblue1\n\
  " S_raw_chans "             1\n\
  " S_raw_format "            " S_mus_bshort "\n\
  " S_raw_srate "             44100\n\
  " S_recorder_autoload "     #f\n\
  " S_recorder_buffer_size "  4096\n\
  " S_recorder_file "         nil\n\
  " S_recorder_in_format "    " S_mus_bshort "\n\
  " S_recorder_max_duration " 1000000.0\n\
  " S_recorder_out_chans "    2\n\
  " S_recorder_out_format "   same as above\n\
  " S_recorder_srate "        22050\n\
  " S_recorder_trigger "      0.0\n\
  " S_reverb_decay "          1.0\n\
  " S_save_dir "              nil\n\
  " S_save_state_on_exit "    #f\n\
  " S_save_state_file "       nil\n\
  " S_selected_data_color "   black\n\
  " S_selected_graph_color "  white\n\
  " S_selection_color "       lightsteelblue1\n\
  " S_show_axes "             show-all-axes\n\
  " S_show_fft_peaks "        #f\n\
  " S_show_marks "            #t\n\
  " S_show_mix_consoles "     #t\n\
  " S_show_mix_waveforms "    #f\n\
  " S_show_selection_transform " #f\n\
  " S_show_usage_stats "      #f\n\
  " S_show_y_zero "           #f\n\
  " S_sinc_width "            10\n\
  " S_spectro_cutoff "        1.0\n\
  " S_spectro_hop "           4\n\
  " S_spectro_start "         0.0\n\
  " S_spectro_x_angle "       90.0\n\
  " S_spectro_x_scale "       1.0\n\
  " S_spectro_y_angle "       0.0\n\
  " S_spectro_y_scale "       1.0\n\
  " S_spectro_z_angle "      -2.0\n\
  " S_spectro_z_scale "       0.1\n\
  " S_speed_style "           " S_speed_as_float "\n\
  " S_speed_tones "           12\n\
  " S_temp_dir "              nil\n\
  " S_text_focus_color "      white\n\
  " S_transform_type "        " S_fourier_transform "\n\
  " S_trap_segfault "         #t\n\
  " S_use_raw_defaults "      #f\n\
  " S_use_sinc_interp "       #f\n\
  " S_verbose_cursor "        #f\n\
  " S_vu_font "               nil\n\
  " S_vu_font_size "          1.0\n\
  " S_vu_size "               1.0\n\
  " S_wavelet_type "          0\n\
  " S_wavo "                  #f\n\
  " S_wavo_hop "              3\n\
  " S_wavo_trace "            64\n\
  " S_window_height "         0\n\
  " S_window_width "          0\n\
  " S_window_x "             -1\n\
  " S_window_y "             -1\n\
  " S_with_mix_consoles "     #t\n\
  " S_x_axis_style "          " S_x_in_seconds "\n\
  " S_xmax "                  0.0\n\
  " S_xmin "                  0.0\n\
  " S_ymax "                  1.0\n\
  " S_ymin "                  1.0\n\
  " S_zero_pad "              0\n\
  " S_zoom_color "            ivory4\n\
  " S_zoom_focus_style "      " S_focus_active "\n\
\n\
";

static char hooks_help_string[] =
"The hooks provide a way to customize various situations that arise through\n\
user-interface manipulations.\n\
\n\
  " S_open_hook "\n\
  " S_during_open_hook "\n\
  " S_after_open_hook "\n\
  " S_close_hook "\n\
  " S_save_hook "\n\
  " S_fft_hook "\n\
  " S_graph_hook "\n\
  " S_after_graph_hook "(snd chn)\n\
  " S_exit_hook "\n\
  " S_start_hook "\n\
  " S_stop_playing_hook "\n\
  " S_stop_playing_region_hook "\n\
  " S_start_playing_hook "\n\
  " S_mark_click_hook "\n\
  " S_mix_amp_changed_hook "\n\
  " S_mix_speed_changed_hook "\n\
  " S_mix_position_changed_hook "\n\
  " S_mix_console_state_changed_hook "\n\
  " S_multichannel_mix_hook "\n\
  " S_mus_error_hook "\n\
  " S_snd_error_hook "\n\
  " S_snd_warning_hook "\n\
  " S_edit_hook "(snd chn)\n\
  " S_undo_hook "(snd chn)\n\
  " S_output_comment_hook "(str)\n\
  " S_output_name_hook "()\n\
  " S_mouse_drag_hook "(snd chn button state x y)\n\
  " S_mouse_press_hook "(snd chn button state x y)\n\
  " S_mouse_release_hook "(snd chn button state x y)\n\
  " S_name_click_hook "(snd-index)\n\
\n\
";

static char functions_help_string[] =
"In the argument lists below, snd as an\n\
argument refers to the sound's index, and defaults to the currently selected\n\
sound. Similarly, chn is the channel number, starting from 0, and defaults\n\
to the currently selected channel. So if there's only one sound active, and\n\
it has only one channel, (" S_cursor ") (" S_cursor " 0), and (" S_cursor " 0 0)\n\
all refer to the same thing.\n\
\n\
  " S_abort "             ()\n\
  " S_abortQ "            ()\n\
  " S_activate_listener " ()\n\
  " S_add_mark "          (sample snd chn)\n\
  " S_add_to_main_menu "  (menu-label)\n\
  " S_add_to_menu "       (top-menu menu-label callback)\n\
  " S_add_transform "     (name xlab lo hi transform)\n\
  " S_amp "               (snd)\n\
  " S_append_to_minibuffer " (msg snd)\n\
  " S_as_one_edit "       (func snd chn)\n\
  " S_autocorrelate "     (data)\n\
  " S_backward_graph "    (count)\n\
  " S_backward_mark "     (count)\n\
  " S_backward_mix "      (count)\n\
  " S_backward_sample "   (count)\n\
  " S_bind_key "          (key state code ignore-prefix)\n\
  " S_change_menu_label " (top-menu old-label new-label)\n\
  " S_channels "          (snd)\n\
  " S_chans "             (snd)\n\
  " S_clear_audio_inputs "()\n\
  " S_close_sound "       (snd)\n\
  " S_close_sound_file "  (fd bytes)\n\
  " S_color_dialog "      ()\n\
  " S_colorQ "            (obj)\n\
  " S_comment "           (snd)\n\
  " S_contrast "          (snd)\n\
  " S_contrast_amp "      (snd)\n\
  " S_contrasting "       (snd)\n\
  " S_convolve_arrays "   (vect1 vect2)\n\
  " S_convolve_selection_with " (file amp)\n\
  " S_convolve_with "     (file amp snd chn)\n\
  " S_count_matches "     (c-expr start snd chn)\n\
  " S_cursor "            (snd chn)\n\
  " S_cursor_follows_play "(snd)\n\
  " S_cursor_style "      (val snd chn)\n\
  " S_cut "               ()\n\
  " S_data_format "       (snd)\n\
  " S_data_location "     (snd)\n\
  " S_delete_mark "       (id snd chn)\n\
  " S_delete_marks "      (snd chn)\n\
  " S_delete_region "     (reg)\n\
  " S_delete_sample "     (samp snd chn)\n\
  " S_delete_samples "    (samp samps snd chn)\n\
  " S_describe_audio "    ()\n\
  " S_dismiss_all_dialogs "()\n\
  " S_edit_fragment "     (num snd chn)\n\
  " S_edit_header_dialog "()\n\
  " S_edits "             (snd chn)\n\
  " S_env_selection "     (envelope env-base snd chn)\n\
  " S_env_sound "         (envelope samp samps env-base snd chn)\n\
  " S_enved_dialog "      ()\n\
  " S_exit "              ()\n\
  " S_expand "            (snd)\n\
  " S_expand_hop "        (snd)\n\
  " S_expand_length "     (snd)\n\
  " S_expand_ramp "       (snd)\n\
  " S_expanding "         (snd)\n\
  " S_fft "               (rl im sgn)\n\
  " S_ffting "            (snd chn)\n\
  " S_file_dialog "       ()\n\
  " S_file_name "         (snd)\n\
  " S_filter_env "        (snd)\n\
  " S_filter_order "      (snd)\n\
  " S_filter_selection "  (env order)\n\
  " S_filter_sound "      (env order snd chn)\n\
  " S_filtering "         (snd)\n\
  " S_find "              (c-expr start snd chn)\n\
  " S_find_mark "         (samp snd chn)\n\
  " S_find_sound "        (filename)\n\
  " S_forward_graph "     (count)\n\
  " S_forward_mark "      (count)\n\
  " S_forward_mix "       (count)\n\
  " S_forward_sample "    (count)\n\
  " S_free_sample_reader "(rd)\n\
  " S_graph "             (data xlabel x0 x1 snd chn)\n\
  " S_graphing "          (snd chn)\n\
  " S_graph_ps "         ()\n\
  " S_header_type "       (snd)\n\
  " S_help_dialog "       (subject help)\n\
  " S_hide_listener "     ()\n\
  " S_id_region "         (id)\n\
  " S_in "                (ms code)\n\
  " S_insert_sound "      (file in_chan snd chn)\n\
  " S_insert_region "     (beg reg snd chn)\n\
  " S_insert_sample "     (samp value snd chn)\n\
  " S_insert_samples "    (samp data snd chn)\n\
  " S_key "               (key state)\n\
  " S_left_sample "       (snd chn)\n\
  " S_list2vct "         (lst)\n\
  " S_loop_samples "      (reader function calls origin)\n\
  " S_mus_sound_length "      (snd chn)\n\
  " S_make_color "        (r g b)\n\
  " S_make_region "       (beg end snd chn)\n\
  " S_make_region_sample_reader "(start snd chn dir)\n\
  " S_make_sample_reader "(start snd chn dir)\n\
  " S_make_vct "          (len)\n\
  " S_mark_name "         (mark)\n\
  " S_mark_sample "       (mark)\n\
  " S_mark_to_sound "     (mark)\n\
  " S_mark_sync "         (mark)\n\
  " S_mark_sync_max "     ()\n\
  " S_marks "             (snd chn pos)\n\
  " S_markQ "             (id)\n\
  " S_max_sounds "        ()\n\
  " S_maxamp "            (snd chn)\n\
  " S_mix "               (file samp in_chan snd chn)\n\
  " S_mix_sound "         (file samp scaler)\n\
  " S_mixes "             ()\n\
  " S_mix_amp "           (mix chan)\n\
  " S_mix_amp_env "       (mix chan)\n\
  " S_mix_anchor "        (mix)\n\
  " S_mix_chans "         (mix)\n\
  " S_mix_console_state " (mix)\n\
  " S_mix_length "        (mix)\n\
  " S_mix_locked "        (mix)\n\
  " S_mix_name "          (mix)\n\
  " S_mix_position "      (mix)\n\
  " S_mix_region "        (samp scaler reg snd chn)\n\
  " S_mix_sound_channel " (mix)\n\
  " S_mix_sound_index "   (mix)\n\
  " S_mix_speed "         (mix)\n\
  " S_mix_track "         (mix)\n\
  " S_mix_vct "           (vct beg chans snd chn)\n\
  " S_mixQ "              (id)\n\
  " S_next_sample "       (rd)\n\
  " S_new_sound "         (name type format srate chans)\n\
  " S_normalize_view "    ()\n\
  " S_open_raw_sound "    (name chans srate format)\n\
  " S_open_sound "        (name)\n\
  " S_open_sound_file "   (name chans srate comment)\n\
  " S_open_alternate_sound "(name)\n\
  " S_orientation_dialog "()\n\
  " S_peaks "             (file snd chn)\n\
  " S_play "              (samp snd chn sync end)\n\
  " S_play_and_wait "     (samp snd chn sync end)\n\
  " S_play_region "       (reg to-end)\n\
  " S_play_selection "    ()\n\
  " S_preload_directory " (dir)\n\
  " S_preload_file "      (file)\n\
  " S_previous_sample "   (rd)\n\
  " S_prompt_in_minibuffer "(prompt callback snd)\n\
  " S_protect_region "    (reg protect)\n\
  " S_read_only "         (snd)\n\
  " S_recorder_dialog "   ()\n\
  " S_recorder_gain "     (gain)\n\
  " S_recorder_in_amp "   (in out)\n\
  " S_recorder_out_amp "  (out)\n\
  " S_redo "              (edits snd chn)\n\
  " S_region_chans "      (reg)\n\
  " S_region_dialog "     ()\n\
  " S_region_id "         (reg)\n\
  " S_region_length "     (reg)\n\
  " S_region_maxamp "     (reg)\n\
  " S_region_sample "     (samp reg chn)\n\
  " S_region_samples "    (samp samps reg chn)\n\
  " S_region_samples_vct "(samp samps reg chn)\n\
  " S_region_srate "      (reg)\n\
  " S_regions "           ()\n\
  " S_regionQ "           (id)\n\
  " S_report_in_minibuffer "(msg snd)\n\
  " S_restore_control_panel "(snd)\n\
  " S_reverb_feedback "   (snd)\n\
  " S_reverb_length "     (snd)\n\
  " S_reverb_lowpass "    (snd)\n\
  " S_reverb_scale "      (snd)\n\
  " S_reverbing "         (snd)\n\
  " S_reverse_selection " ()\n\
  " S_reverse_sound "     (snd chn)\n\
  " S_revert_sound "      (snd)\n\
  " S_right_sample "      (snd chn)\n\
  " S_sample "            (samp snd chn)\n\
  " S_sample_reader_at_endQ "(rd)\n\
  " S_sample_readerQ "    (rd)\n\
  " S_samples "           (samp samps snd chn)\n\
  " S_samples_vct "      (samp samps snd chn)\n\
  " S_save_control_panel "(snd)\n\
  " S_save_edit_history " (file snd chn)\n\
  " S_save_macros "       ()\n\
  " S_save_marks "        (snd)\n\
  " S_save_region "       (reg filename format)\n\
  " S_save_selection "    (file)\n\
  " S_save_sound "        (snd)\n\
  " S_save_sound_as "     (filename snd type format srate)\n\
  " S_save_state "        (filename)\n\
  " S_scale_by "          (scalers snd chn)\n\
  " S_scale_selection_by "(scalers)\n\
  " S_scale_selection_to "(scalers)\n\
  " S_scale_to "          (scalers snd chn)\n\
  " S_select_channel "    (chn)\n\
  " S_select_mix "        (id)\n\
  " S_select_region "     (reg)\n\
  " S_select_sound "      (snd)\n\
  " S_selected_channel "  (snd)\n\
  " S_selected_mix "      ()\n\
  " S_selected_sound "    ()\n\
  " S_selection_beg "     ()\n\
  " S_selection_length "  ()\n\
  " S_selection_member "  (snd chn)\n\
  " S_selection_to_temp " (type format)\n\
  " S_selection_to_temps "(type format)\n\
  " S_selectionQ "        ()\n\
  " S_set_amp "           (amp snd)\n\
  " S_set_contrast "      (contrast snd)\n\
  " S_set_contrast_amp "  (contrast-amp snd)\n\
  " S_set_contrast_func " (contrast)\n\
  " S_set_contrasting "   (contrasting snd)\n\
  " S_set_cursor "        (samp snd chn)\n\
  " S_set_cursor_follows_play " (cursor-follows snd)\n\
  " S_set_expand "        (expand-amount snd)\n\
  " S_set_expand_funcs "  (expand make-expand free-expand)\n\
  " S_set_expand_hop "    (expand-hop snd)\n\
  " S_set_expand_length " (expand-length snd)\n\
  " S_set_expand_ramp "   (expand-ramp snd)\n\
  " S_set_expanding "     (contrasting snd)\n\
  " S_set_ffting "        (on snd chn)\n\
  " S_set_filter_order "  (filter-order snd)\n\
  " S_set_filter_env "    (filter-env snd)\n\
  " S_set_filtering "     (filtering snd)\n\
  " S_set_graph_style "   (style)\n\
  " S_set_graphing "      (on snd chn)\n\
  " S_set_just_sounds "   (just-sounds)\n\
  " S_set_left_sample "   (samp snd chn)\n\
  " S_set_mark_name "     (mark name)\n\
  " S_set_mark_sample "   (mark sample)\n\
  " S_set_mark_sync "     (mark sync)\n\
  " S_set_menu_sensitive "(top-menu label on)\n\
  " S_set_mix_amp "       (mix chan amp)\n\
  " S_set_mix_amp_env "   (mix chan env)\n\
  " S_set_mix_anchor "    (mix anchor)\n\
  " S_set_mix_length "    (mix length)\n\
  " S_set_mix_locked "    (mix locked)\n\
  " S_set_mix_name "      (mix name)\n\
  " S_set_mix_position "  (mix samp)\n\
  " S_set_mix_speed "     (mix speed)\n\
  " S_set_mix_track "     (mix track)\n\
  " S_set_mix_console_state "(mix state)\n\
  " S_set_read_only "     (read-only snd)\n\
  " S_set_recorder_gain "(gain amp)\n\
  " S_set_recorder_in_amp "(in out amp)\n\
  " S_set_recorder_out_amp "(out amp)\n\
  " S_set_reverb_decay "  (decay snd)\n\
  " S_set_reverb_feedback "(feedback snd)\n\
  " S_set_reverb_funcs "  (reverb make-reverb free-reverb)\n\
  " S_set_reverb_length "(length snd)\n\
  " S_set_reverb_lowpass "(lowpass snd)\n\
  " S_set_reverb_scale "(scale snd)\n\
  " S_set_reverbing "     (on snd)\n\
  " S_set_right_sample "  (samp snd chn)\n\
  " S_set_sample "        (samp value snd chn)\n\
  " S_set_samples "       (samp samps data snd chn)\n\
  " S_set_showing_controls " (showing snd)\n\
  " S_set_speed "         (speed snd)\n\
  " S_set_syncing "       (syncing snd)\n\
  " S_set_uniting "       (style snd)\n\
  " S_set_waving "        (on snd chn)\n\
  " S_set_x_bounds "      (x0 x1 snd chn)\n\
  " S_set_y_bounds "      (y0 y1 snd chn)\n\
  " S_short_file_name "   (snd)\n\
  " S_showing_controls "  (snd)\n\
  " S_show_listener "     ()\n\
  " S_smooth "            (beg num snd chn)\n\
  " S_smooth_selection "  ()\n\
  " S_sound_files_in_directory "(dir)\n\
  " S_sound_to_temp "     (type format)\n\
  " S_sound_to_temps "    (type format)\n\
  " S_soundQ "            (snd)\n\
  " S_sounds "            ()\n\
  " S_snd_apropos "       (name)\n\
  " S_snd_error "         (str)\n\
  " S_snd_help "          (name)\n\
  " S_snd_print "         (str)\n\
  " S_snd_spectrum "      (data window length linear)\n\
  " S_snd_tempnam "       ()\n\
  " S_snd_version "       ()\n\
  " S_snd_warning "       (str)\n\
  " S_speed "             (snd)\n\
  " S_squelch_update "    (snd chn)\n\
  " S_srate "             (snd)\n\
  " S_src_selection "     (num-or-env base)\n\
  " S_src_sound "         (num-or-env base)\n\
  " S_stop_playing "      (snd)\n\
  " S_swap_channels "     (snd1 chn1 snd2 chn2 beg dur)\n\
  " S_syncd_marks "       (sync)\n\
  " S_syncing "           (snd)\n\
  " S_temp_filenames "    (data)\n\
  " S_temp_to_selection " ()\n\
  " S_temp_to_sound "     ()\n\
  " S_temps_to_selection "()\n\
  " S_temps_to_sound "    ()\n\
  " S_transform_dialog "  ()\n\
  " S_transform_sample "  (bin slice snd chn)\n\
  " S_transform_samples " (snd chn()\n\
  " S_transform_samples_vct " (snd chn)\n\
  " S_transform_size "    (snd chn)\n\
  " S_unbind_key "        (key state)\n\
  " S_undo "              (edits snd chn)\n\
  " S_uniting "           (snd)\n\
  " S_update_sound "      ()\n\
  " S_update_fft "        (snd chn)\n\
  " S_update_graph "      (snd chn)\n\
  " S_vct_p "              (vobj)\n\
  " S_vct_addB "          (vobj1 vobj2)\n\
  " S_vct_copy "          (obj)\n\
  " S_vct_doB "           (obj proc)\n\
  " S_vct_fillB "         (vobj val)\n\
  " S_vct_length "        (vobj)\n\
  " S_vct_mapB "          (obj proc)\n\
  " S_vct_moveB "         (obj new old)\n\
  " S_vct_multiplyB "     (vobj1 vobj2)\n\
  " S_vct_offsetB "       (vobj val)\n\
  " S_vct_ref "           (vobj pos)\n\
  " S_vct_scaleB "        (vobj scl)\n\
  " S_vct_setB "          (vobj pos val)\n\
  " S_vct_samples "      (samp samps data snd chn)\n\
  " S_vct_sound_file "   (fd vobj vals)\n\
  " S_vcts_doB "          (obj ... proc)\n\
  " S_vcts_mapB "         (obj ... proc)\n\
  " S_vector2vct "       (vect)\n\
  " S_view_sound "        (filename)\n\
  " S_waving "            (snd chn()\n\
  " S_window_height "     ()\n\
  " S_window_width "      ()\n\
  " S_x_bounds "          (snd chn)\n\
  " S_y_bounds "          (snd chn)\n\
  " S_yes_or_no_p "       (ques)\n\
\n\
";

static char sndlib_help_string[] =
"Some of the underlying sound library (Sndlib functions are available in lisp\n\
(and more could be made available, if they're needed).\n\
\n\
  " S_mus_sound_samples " (filename)\n\
  " S_mus_sound_frames " (filename)\n\
  " S_mus_sound_datum_size " (filename)\n\
  " S_mus_sound_data_location " (filename)\n\
  " S_mus_sound_chans " (filename)\n\
  " S_mus_sound_srate " (filename)\n\
  " S_mus_sound_header_type " (filename)\n\
  " S_mus_sound_data_format " (filename)\n\
  " S_mus_sound_length " (filename)\n\
  " S_mus_sound_type_specifier " (filename)\n\
  " S_mus_header_type_name " (type)\n\
  " S_mus_data_format_name " (format)\n\
  " S_mus_sound_comment " (filename)\n\
  " S_mus_data_format_bytes_per_sample " (format)\n\
  " S_mus_audio_error " ()\n\
  " S_mus_audio_error_name "(err)\n\
\n\
";

static char resource_help_string[] =
"Snd-specific resources are:\n\
\n\
initFile            \"~/.snd\"\n\
epsFile             \"snd.eps\"\n\
overwriteCheck      0\n\
autoResize          1\n\
horizontalPanes     0\n\
defaultOutputType   NeXT_sound_file\n\
\n\
buttonFont          -*-times-medium-r-*-*-14-*-*-*-*-*-iso8859-1\n\
boldbuttonFont      -*-times-bold-r-*-*-14-*-*-*-*-*-iso8859-1\n\
axisLabelFont       -*-times-medium-r-normal-*-20-*-*-*-*-*-iso8859-1\n\
axisNumbersFont     -*-courier-medium-r-normal-*-14-*-*-*-*-*-iso8859-1\n\
helpTextFont        9x15\n\
listenerFont        9x15\n\
\n\
useSchemes          none\n\
highlightcolor      ivory1\n\
basiccolor          ivory2\n\
positioncolor       ivory3\n\
zoomcolor           ivory4\n\
cursorcolor         red\n\
selectioncolor      lightsteelblue1\n\
mixcolor            lightgreen\n\
mixfocuscolor       green2\n\
listenercolor       aliceblue\n\
envedwaveformcolor  blue\n\
filterwaveformcolor blue\n\
mixwaveformcolor    darkgray\n\
graphcolor          white\n\
selectedgraphcolor  white\n\
datacolor           black\n\
selecteddatacolor   black\n\
markcolor           red\n\
pushedbuttoncolor   lightsteelblue1\n\
sashcolor           green\n\
\n\
zoomSliderWidth     10\n\
positionSliderWidth 13\n\
toggleSize           0\n\
envedPointSize      10\n\
channelSashIndent  -10\n\
channelSashSize      0\n\
sashSize            14\n\
sashIndent          -6\n\
\n";

#ifndef _MSC_VER
static char mix_help_string[] =
"Since mixing is the most common and most useful\n\
editing operation performed on sounds, there is\n\
relatively elaborate support for it in Snd. Each\n\
individual mix portion has a mix console, a\n\
small box displayed in above the waveform\n\
containing various controls.\n\
\n\
To mix in a file, use either the File Mix menu\n\
option or the command C-x C-q. Currently the only\n\
difference between these two is that the Mix menu\n\
option tries to take the current sync state into\n\
account, whereas the C-x C-q command does not. To\n\
mix a selection, use C-x q. The mix starts at the\n\
current cursor location.\n\
\n\
When a section or file is mixed into the current\n\
file a mix console is associated with it, each\n\
output channel getting its own console. The\n\
console is displayed at first as a row of widgets\n\
giving the input file name, the begin and end\n\
times of the mixed-in portion (click to change\n\
from seconds to samples), then three icons:\n\
\n\
  a speaker:   while pushed, the input is played\n\
  an 'x':      click to remove the console\n\
  a box:       click to open (or close) the console\n\
\n\
You can drag the widgets to change the position\n\
of the mix. Once opened, each console presents a\n\
pane with an amplitude slider for each input\n\
channel, and a speed control (srate change on the\n\
input). The initial state of the console sets the\n\
speed to 1.0, and all the input amplitudes 0.0\n\
except the channel that matches the current\n\
output channel, which is set to 1.0 (a straight\n\
mix). To return to this state at any time, click\n\
the 'amp:' label. To turn the mix off ('mute' it,\n\
set all amps to 0.0), double click the label; to\n\
return to the last settings you made, click it\n\
with control or meta down. Similarly, click the\n\
'speed:' label to reset it to 1.0, and click it\n\
with control or meta to return to your last\n\
settings. Each time you release the mouse button\n\
(or click the amp label) counts as another 'edit'\n\
of the file, so it is usually better to use\n\
'undo' and 'redo' in this context, rather than\n\
repeated clicks and shift-clicks. The srate scale\n\
is interpreted in the same way as the sound pane\n\
speed control -- as a float normally, but also,\n\
if you like, quantized to semitones or integer\n\
ratios.\n\
\n\
To change the scale interpretation, set the\n\
variables " S_mix_console_amp_scaler " and " S_mix_console_speed_scaler ".\n\
These default to 1.0 which gives a scaling from 0 to around\n\
12. These numbers actually scale an exponent, so\n\
(for example) if " S_mix_console_amp_scaler " is set to 0.5,\n\
the scale goes from 0.0 to around 3.5.\n\
\n\
To reduce the mix console title to a single letter,\n\
double click the file name. Double click the\n\
label to return to the original row of icons. Since\n\
screen space is at a premium, this minimal form\n\
of the console can reduce clutter. You can still\n\
drag the little label to reposition the mix.\n\
\n\
To reduce the mix console to a single letter, double\n\
click the file name.  Double click the label to return\n\
to the original row of icons.  Since screen space is at\n\
a premium, this minimal form of the console can reduce\n\
clutter.  You can still drag the label to reposition\n\
the mix.  To remove the console altogether, click the 'x'.\n\
When any edit is performed that changes the file\n\
within the mixed portion, the affected mix\n\
consoles are removed from the display, and the\n\
only way to return to them is to undo the\n\
offending edit. That is, if you mix a portion,\n\
then cut some part of that portion, the mix will\n\
be locked in place from then on, as if you had\n\
clicked the 'x' button on the mix console.\n\
\n\
To turn off the constant graphics updates (which\n\
can slow down old machines like mine, and which\n\
can also be annoying when you know what you're\n\
doing), set the variable movies to #f.\n\
\n";
#else
static char mix_help_string[] = "";
#endif

static char recording_help_string[] = 
"To make a recording, choose 'Record' from the\n\
File menu. A window opens with the various\n\
recording controls.  The top three panes display\n\
the status of the input and output lines. If a\n\
channel is active, its meter will glow\n\
yellow. If some signal clips during recording,\n\
the meter will flash red. The numbers below the\n\
channel buttons indicate the signal maximum\n\
since it was last reset. The sliders underneath\n\
the meters scale the audio data in various ways\n\
before it is mixed into the output. The vertical\n\
sliders on the right scale the line-in and\n\
microphone signals before the meter, and the\n\
output signal before it gets to the speaker\n\
(these are needed to avoid clipping on input,\n\
and to set the 'monitor' volume of the output\n\
independent of the output file volume).\n\
\n\
The fourth pane has information about the\n\
current output file (its name and so on), and\n\
the layout of the window. The buttons on the\n\
right can be used to open and close panes\n\
painlessly. If the button is not square (a\n\
diamond on the SGI), the underlying audio\n\
hardware can't handle input from that device at\n\
the same time as it reads other 'radio' button\n\
devices. So, in that case, opening the panel via\n\
the button also turns off the other incompatible\n\
device. The fifth pane contains a history of\n\
whatever the recorder thought worth\n\
reporting. The duration field gives the current\n\
output file's duration. The bottom row of\n\
buttons dismiss the window, start recording,\n\
cancel the current take, and provide some\n\
help. There's also a slider on the far right\n\
that controls the speaker output volume\n\
(independent of the output file volume).\n\
\n\
To make a recording, choose the inputs and\n\
outputs you want; for example, to record channel\n\
A from the microphone to channel A of the output\n\
file, click the Microphone panel's A button and\n\
the Output panel's A button. Then when you're\n\
ready to go, click the Record button. Click it\n\
again to finish the recording.\n\
\n\
If the record window's VU meters are too big (or\n\
too small) for your screen, you can fool around\n\
with the variable " S_vu_size " which defaults to 1.0.\n\
Similarly the variable " S_vu_font_size " tries to\n\
change the size of the numbers on the label, and\n\
" S_vu_font " chooses the family name of the font\n\
used.\n\
\n\
Digital input is slightly tricky -- you\n\
need to set the sampling rate before you\n\
click the 'digital input' button; otherwise\n\
you'll get a stuttering effect because the output\n\
(monitor) rate doesn't match the input rate.\n\
";

static char envelope_editor_help_string[] = 
"The Edit Envelope dialog (under the Edit menu)\n\
fires up a window for viewing and editing\n\
envelopes. The dialog has a display showing either\n\
the envelope currently being edited or a panorama\n\
of all currently loaded envelopes. The current\n\
envelope can be edited with the mouse: click at\n\
some spot in the graph to place a new breakpoint,\n\
drag an existing breakpoint to change its\n\
position, and click an existing breakpoint to\n\
delete it. The Undo and Redo buttons can be used\n\
to move around in the list of envelope edits; the\n\
current state of the envelope can be saved with\n\
the 'save' button, or printed with 'print'.\n\
\n\
Envelopes can be defined using defvar, and loaded\n\
from a separate file of envelope definitions via\n\
the load function.  For example, the file:\n\
\n\
  (defvar ramp '(0 0 1 1))\n\
  (defvar pyramid '(0 0 1 1 2 0))\n\
\n\
defines two envelopes that can be used in Snd\n\
wherever an envelope is needed (e.g. C-x C-a). You\n\
can also define a new envelope in the dialog's\n\
text field; '(0 0 1 1) followed by return fires up\n\
a ramp as a new envelope.\n\
\n\
In the overall view of envelopes, click an\n\
envelope, or click its name in the scrolled list\n\
on the left to select it; click the selected\n\
envelope to load it into the editor portion,\n\
clearing out whatever was previously there.  To\n\
load an exisiting envelope into the editor, you\n\
can also type its name in the text field; to give\n\
a name to the envelope as it is currently defined\n\
in the graph viewer, type its name in this field,\n\
then either push return or the 'save' button.\n\
\n\
Once you have an envelope in the editor, it can be\n\
applied to the currently active sounds via the\n\
'Apply' or 'Undo&Apply' buttons; the latter first\n\
tries to undo the previous edit, then applies the\n\
envelope. The envelope can be applied to the\n\
amplitude, the spectrum, or the sampling rate. The\n\
choice is made via the three buttons marked 'amp',\n\
'flt', and 'src'. The filter order is the variable\n\
" S_filter_env_order " which defaults to 40.\n\
To apply the changes to the current selection,\n\
rather than the current sound, set the 'selection'\n\
button.  To apply it to the currently selected mix,\n\
set the 'mix' button.\n\
\n\
The two toggle buttons at the lower right choose\n\
whether to show a light-colored version of the\n\
currently active sound (the 'wave' button), and\n\
whether to clip mouse movement at the current y\n\
axis bounds (the 'clip' button).\n\
";


void about_snd_help(snd_state *ss)
{
  ssnd_help_with_url(ss,
		     "Snd",
		     "#gettingstarted",
about_snd_help_string,
"The various Help menu items are:\n\
\n",
help_menu_help_string,
"\n",
"The main menu items are:\n\
  File: operations on files.\n\
  Edit: operations on the current selection.\n\
  View: change Snd display choices.\n\
  Options: change Snd analysis choices.\n\
  Help: this menu.\n\
The main menu bar itself is Snd's 'drop' box.\n\
\n\
The File menu's options are:\n\
",
file_menu_help_string,
"\n\
The Edit menu's options are:\n\
",
edit_menu_help_string,
"\n\
The View menu's options are:\n\
",
view_menu_help_string,
"\n\
The Options menu's items are:\n\
",
options_menu_help_string,
"\n\
The graph editing commands are:\n\
",
graph_help_string,
"\n\
",
fft_keypad_help_string,
"\n\
",
mark_help_string,
"\n\
",
init_file_help_string,
constants_help_string,
variables_help_string,
hooks_help_string,
functions_help_string,
sndlib_help_string,
"\n\
",
resource_help_string,
NULL);
}

void fft_help(snd_state *ss)
{
  ssnd_help_with_url(ss,
		     STR_FFT,
		     "#viewfft",
fft_help_string,
"\n\
",
fft_keypad_help_string,
NULL);
}

void find_help(snd_state *ss) {snd_help_with_url(ss,STR_Find,"#find",find_help_string);}
void undo_help(snd_state *ss) {snd_help_with_url(ss,STR_Undo,"#undoredo",undo_help_string);}
void sync_help(snd_state *ss) {snd_help_with_url(ss,STR_Sync,"#multichannel",sync_help_string);}
void speed_help(snd_state *ss) {snd_help_with_url(ss,STR_Speed,"#speed",speed_help_string);}
void expand_help(snd_state *ss) {snd_help_with_url(ss,STR_Expand,"#expand",expand_help_string);}
void reverb_help(snd_state *ss) {snd_help_with_url(ss,STR_Reverb,"#reverb",reverb_help_string);}
void contrast_help(snd_state *ss) {snd_help_with_url(ss,STR_Contrast,"#contrast",contrast_help_string);}
void env_help(snd_state *ss) {snd_help_with_url(ss,STR_Envelope,"#editenvelope",env_help_string);}
void marks_help(snd_state *ss) {snd_help_with_url(ss,STR_Marks,"#marks",mark_help_string);}
void mix_help(snd_state *ss) {snd_help_with_url(ss,STR_Mixing,"#mixingfiles",mix_help_string);}
void sound_files_help(snd_state *ss) {snd_help_with_url(ss,STR_Format,"#formats",sound_files_help_string);}
void recording_help(snd_state *ss) {snd_help_with_url(ss,STR_Recording,"#recordfile",recording_help_string);}
void init_file_help(snd_state *ss) {ssnd_help_with_url(ss,STR_Customization,"extsnd.html",init_file_help_string,"\n",resource_help_string,NULL);}

#if HAVE_GUILE
char *CLM_help(void);
#else
static char *CLM_help(void) {return("");}
#endif

void clm_help(snd_state *ss) {snd_help_with_url(ss,STR_CLM,"grfsnd.html#sndwithclm",CLM_help());}

#if HAVE_CLICK_FOR_HELP

/* -------- click for help -------- */

void click_for_file_menu_help(snd_state *ss)
{
  ssnd_help_with_url(ss,
		     "File Menu",
		     "#fileoperations",
"The File menu provides one way to open,\n\
close, and save files. Its options are:\n\
\n",
file_menu_help_string,
"\n\
The Print option produces a Postscript file;\n\
you can print it with lpr.\n",
NULL);
}

void click_for_edit_menu_help(snd_state *ss)
{
  ssnd_help_with_url(ss,
		     "Edit Menu",
		     "#editoperations",
"The Edit Menu options apply to the\n\
current selection in most cases.  The\n\
successive selections are saved on a stack\n\
of 'regions' accessible via ctrl-Y.\n\
The selection can be retrieved by or\n\
from other programs or within Snd with\n\
the middle mouse button.  The options are:\n\
\n",
edit_menu_help_string,
NULL);
}

void click_for_view_menu_help(snd_state *ss)
{
  ssnd_help_with_url(ss,
		     "View Menu",
		     "#viewing",
"The View Menu affects the overall Snd display.\n\
Its options are:\n\
\n\
",
view_menu_help_string,
NULL);
}

void click_for_options_menu_help(snd_state *ss)
{
  ssnd_help_with_url(ss,
		     "Options Menu",
		     "#options",
"The Options menu items affect how the FFT\n\
operates, and whatnot.  The items are:\n\
\n\
",
options_menu_help_string,
NULL);
}

void click_for_help_menu_help(snd_state *ss)
{
  ssnd_help_with_url(ss,
		     "Help Menu",
		     "#menus",
"The Help menu tries to provide quick\n\
help for the most common Snd operations.\n\
The menu items are:\n\
\n",
help_menu_help_string,
NULL);
}

void click_for_graph_help(snd_state *ss)
{
  ssnd_help_with_url(ss,
	    "Graph",
	    "#panelayout",
"This portion of the Snd display shows the\n\
sound data in the time and/or frequency domains.\n\
If you click on the time domain wave, you can\n\
edit it using emacs-like keyboard commands, as\n\
well as using mouse-click-and-drag to define the\n\
selection.  Once defined, the selected portion\n\
can be cut, deleted, or pasted elsewhere, the\n\
latter with the middle mouse button.  The keyboard\n\
commands are (c = control):\n\
\n\
",
graph_help_string,
"\n",
mark_help_string,
NULL);
}

void click_for_history_help(snd_state *ss)
{
  snd_help_with_url(ss,
		    "Edit History",
		    "#edithistory",
"The current state of the undo/redo list can be\n\
viewed as a scrolled list of strings in the pane\n\
on the left of the graph (in Motif 1, there's a\n\
'Show Edit History' menu option).  If there are no\n\
current edits, it just lists the associated file\n\
name (i.e. the zero-edits state).  As you edit the\n\
sound, the operations appear in the edit list\n\
window.  Click on a member of the list to move to\n\
that point in the edit list (equivalent to some\n\
number of undo's or redo's).  To move to a given\n\
edit point and follow the sync chain (if any), use\n\
control-click.\n\
");
}

void listener_dialog_help(snd_state *ss)
{
  ssnd_help_with_url(ss,
		     "Lisp Listener",
		     "#customization",
"This is the lisp listener pane; it is one way to\n\
access the Guile Scheme interpreter.\n\
\n",
	   init_file_help_string,
	   NULL);
}

static char sp_name_separator_help_string[] =
"When reading a very large file, Snd tries to\n\
keep an overview at hand of the channels so\n\
that you can move around quickly in very large\n\
data sets; when first read in, these overviews\n\
are set underway, and when they are finally\n\
ready for use, the line after the file name\n\
appears.  If you try to zoom out to a large\n\
view before the separator line appears, the\n\
graphics update process may be slow.\n\
";

void click_for_name_separator_help(snd_state *ss)
{
  snd_help(ss,"Name Separator",sp_name_separator_help_string);
}

static char sp_amp_help_string[] = 
"This scrollbar controls the amplitude\n\
at which the sound is played.  Click the\n\
amp label to return to 1.0. Control-Click\n\
returns to the previous value.\n\
";

void click_for_amp_help(snd_state *ss)
{
  snd_help(ss,"Amp",sp_amp_help_string);
}

static char sp_srate_arrow_help_string[] = 
"This button determines which direction\n\
the sound file is played.  When pointing\n\
to the right, the sound is played forwards;\n\
to the left, backwards.\n\
";

void click_for_srate_arrow_help(snd_state *ss)
{
  snd_help(ss,"Srate Arrow",sp_srate_arrow_help_string);
}

static char sp_srate_help_string[] =
"This scrollbar controls the sampling rate\n\
at which the sound is played.  The arrow\n\
controls the direction (forwards or backwards)\n\
of playback.  Label clicks behave as with amp.\n\
";

void click_for_speed_help(snd_state *ss)
{
  ssnd_help_with_url(ss,"Srate","#speed",sp_srate_help_string,"\n",speed_help_string,NULL);
}

void click_for_minibuffer_help(snd_state *ss)
{
  snd_help_with_url(ss,"Minibuffer","#panelayout",
"This is the 'minibuffer', to use Emacs\n\
jargon.  Although it looks inert and wasted,\n\
there is in fact a text window lurking beneath\n\
that has access to the Lisp evaluator, not\n\
to mention much of the innards of the Snd program.\n\
");
}

void click_for_play_help(snd_state *ss)
{
  snd_help_with_url(ss,"Play","#play",
"Snd can play any number of sounds at once\n\
or should be able to anyway.  A sort of\n\
clumsy realtime mixer, although it was not\n\
intended to fill that role. \n\
\n\
");
}

static char sp_expand_help_string[] =
"This scrollbar controls the tempo at which\n\
the sound is played back, using granular\n\
synthesis. The expand button must be down\n\
to get any expansion. Label clicks as in amp.\n\
";

void click_for_expand_help(snd_state *ss)
{
  ssnd_help_with_url(ss,STR_Expand,"#expand",sp_expand_help_string,"\n",expand_help_string,NULL);
}

static char sp_contrast_help_string[] =
"This scrollbar controls the amount of\n\
'contrast enhancement' applied during\n\
playback.  The contrast button must be\n\
down to get any effect.  Label clicks as in amp.\n\
";

void click_for_contrast_help(snd_state *ss)
{
  ssnd_help_with_url(ss,STR_Contrast,"#contrast",sp_contrast_help_string,"\n",contrast_help_string,NULL);
}

static char sp_revscl_help_string[] =
"This scrollbar controls the amount of the\n\
sound that is fed into the reverberator.\n\
The reverb button must be down to get any\n\
reverb during playback.  Label clicks as in amp.\n\
";

void click_for_reverb_scale_help(snd_state *ss)
{
  ssnd_help_with_url(ss,"Reverb amount","#reverb",sp_revscl_help_string,"\n",reverb_help_string,NULL);
}

static char sp_revlen_help_string[] =
"This scrollbar controls the lengths of\n\
the various delay lines in the reverb.\n\
It only takes effect when the reverb is\n\
created, that is, only when the play\n\
operation starts from silence.  Label clicks\n\
as in amp.\n\
";

void click_for_reverb_length_help(snd_state *ss)
{
  ssnd_help_with_url(ss,"Reverb length","#reverb",sp_revlen_help_string,"\n",reverb_help_string,NULL);
}

static char sp_filter_help_string[] =
"The Snd filter is an FIR filter of arbitrary\n\
order.  You specify the filter you want by\n\
defining the frequency response as an envelope\n\
in the 'env' window; set the desired order in\n\
the 'order' window; then turn it on by pushing\n\
the filter button at the right.  The filter\n\
design algorithm uses frequency sampling.\n\
The higher the order, the closer the filter\n\
can approximate the envelope you draw.\n\
You can also specify the filter coefficients\n\
in a file of floats, then load them into the\n\
Snd filter by typing the file name in the\n\
filter envelope text window.\n\
";

void click_for_filter_help(snd_state *ss)
{
  snd_help(ss,"Filter",sp_filter_help_string);
}

static char sp_filter_order_help_string[] =
"The filter order determines how closely\n\
the filter approximates the frequency response\n\
curve you drew in the 'env' window.\n\
";

void click_for_filter_order_help(snd_state *ss)
{
  snd_help(ss,"Filter Order",sp_filter_order_help_string);
}

static char sp_filter_envelope_help_string[] =
"The filter envelope is a line-segment\n\
description of the frequency response\n\
you want.  It consists of a sequence of\n\
x,y pairs; normally the x axis goes\n\
from 0 to .5 or 0 to 1.0.  For example,\n\
a low-pass filter envelope could be:\n\
0.0 1.0 .25 1.0 .5 0.0 1.0 0.0\n\
";

void click_for_filter_envelope_help(snd_state *ss)
{
  snd_help(ss,"Filter Envelope",sp_filter_envelope_help_string);
}

void click_for_sound_help(snd_state *ss)
{
  ssnd_help(ss,
	   "Minibuffer",
"This portion of the snd display has several\n\
parts: the sound file name, with an asterisk if\n\
the file has unsaved edits; a minibuffer for\n\
various expression evaluations; a sync button\n\
that causes operations on one channel to be\n\
applied to all channels; and a play button\n\
that causes the sound to be played.  The\n\
lower portion of the pane, normally hidden,\n\
contains a variety of sound manipulation\n\
controls that can be applied while it is\n\
playing:\n\
\n\
    file name, followed by a separator\n\
\n",
sp_name_separator_help_string,
"\n\n\
    minibuffer, sync, and play buttons\n\
    amplitude scrollbar\n\
\n",
sp_amp_help_string,
"\n\n\
    srate scrollbar\n\
\n",
sp_srate_help_string,
"\n\
    srate arrow button\n\
\n",
sp_srate_arrow_help_string,
"\n\n\
    expand scrollbar and button\n\
\n",
sp_expand_help_string,
"\n\n\
    contrast scrollbar and button\n\
\n",
sp_contrast_help_string,
"\n\n\
    reverb amount scrollbar\n\
\n",
sp_revscl_help_string,
"\n\
    reverb length scrollbar\n\
\n",
sp_revlen_help_string,
"\n\n\
    filter section\n\
\n",
sp_filter_help_string,
"\n\
    filter order text window\n\
\n",
sp_filter_order_help_string,
"\n\
    filter envelope window\n\
\n",
sp_filter_envelope_help_string,
NULL);
}

void click_for_mix_console_help(mixmark *m)
{
  snd_state *ss;
  mixdata *md;
  md = (mixdata *)(m->owner);
  ss = md->ss;
  snd_help_with_url(ss,"Mix","#mixingfiles",mix_help_string);
}

void click_for_save_as_help(snd_state *ss)
{
  snd_help(ss,
	   "Save As",
"You can save the current state of a file or region\n\
under a different file name using the Save\n\
As option.  The output header type, data format,\n\
and sampling rate can also be set.  The data formats\n\
are big-endian where relevant except for 'wave'\n\
output.  If a file by the chosen name already exists\n\
it is silently overwritten, unless that file is\n\
already open in Snd and has edits.  In that case,\n\
you'll be asked what to do.  If you want to be warned\n\
whenever a file is about to be overwritten by this\n\
option, set the resource overwriteCheck to 1.\n\
If you give the current file name to Save As,\n\
any current edits will be saved and the current\n\
version in Snd will be updated (that is, in this\n\
case, the current edit tree is not preserved).\n\
");
}


#endif


/* -------- dialog help button -------- */

void help_dialog_help(snd_state *ss)
{
  snd_help(ss,
	   "Help",
"You can get help within Snd either from\n\
the Help Menu items, or by clicking on\n\
some portion of the Snd display while the\n\
cursor is '?'.  See Click for Help in the\n\
Help Menu.\n\
");
}

void transform_dialog_help(snd_state *ss)
{
  snd_help(ss,
       "Transform Options",
"This dialog presents the various transform (fft)\n\
related choices.\n\
\n\
On the upper left is a list of available transform\n\
types; next on the right is a list of fft sizes;\n\
next is a panel of buttons that sets various\n\
display-oriented choices; the lower left panel\n\
sets the current wavelet, when relevant; next is\n\
the fft data window choice; and next to it is a\n\
graph of the current fft window; when the window\n\
has an associated parameter (sometimes known as\n\
'alpha' or 'beta'), the slider beneath the window\n\
list is highlighted and can be used to choose the\n\
desired member of that family of windows.\n\
\n\
If the 'selection' button is not set, the FFT is\n\
taken from the start (the left edge) of the\n\
current window and is updated as the window bounds\n\
change; otherwise the FFT is taken over the extent\n\
of the current selection, if any is active in the\n\
current channel.  The fft data is scaled to fit\n\
between 0.0 and 1.0 unless the fft normalization\n\
is off. The full frequency axis is normally\n\
displayed, but the axis is 'dragable' -- put the\n\
mouse on the axis and drag it either way to change\n\
the range (this is equivalent to changing the\n\
variable " S_spectro_cutoff "). You can also click on\n\
any point in the fft to get the associated fft\n\
data displayed; if " S_verbose_cursor " is on, you can\n\
drag the mouse through the fft display and the\n\
description in the minibuffer will be constantly\n\
updated.\n\
\n\
The harmonic analysis function is normally the\n\
Fourier Transform, but others are available,\n\
including about 20 wavelet choices, the Hankel and\n\
Chebyshev transforms, and perhaps others. (The\n\
Hankel transform returns the Bessel function\n\
spectrum, the Chebyshev transform returns the\n\
Chebyshev polynomial spectrum).\n\
\n\
The top three buttons in the transform dialog\n\
choose between a normal fft, a sonogram, or a\n\
spectrogram. The 'peaks' button affects whether\n\
peak info is displayed alongside the graph of the\n\
spectrum. The 'dB' button selects between a linear\n\
and logarithmic Y (magnitude) axis. The 'log freq'\n\
button makes a similar choice along the frequency\n\
axis.\n\
");	   
}

void color_dialog_help(snd_state *ss)
{
  snd_help(ss,
       "View Color",
"This dialog sets the colormap and associated\n\
variables used during sonogram, spectrogram,\n\
and perhaps wavogram display. The cutoff scale refers\n\
to the minimum data value to be displayed.\n\
");	   
}

void orientation_dialog_help(snd_state *ss)
{
  snd_help(ss,
       "View Orientation",
"This dialog sets the rotation and scaling\n\
variables used during sonogram, spectrogram,\n\
and wavogram display.\n\
");	   
}

void record_dialog_help(snd_state *ss)
{
  ssnd_help_with_url(ss,
		     "Record",
		     "#recordfile",
recording_help_string,
"\n\
If you go to the main Snd window while the\n\
recorder is active and play a sound, the\n\
recorder's audio lines are made inactive\n\
to try to reduce confusion.  To re-activate\n\
the recorder, press the 'reset' button at\n\
the bottom of the window.\n\
",
NULL);
}

void envelope_editor_dialog_help(snd_state *ss)
{
  snd_help_with_url(ss,"Envelope Editor","editenvelope",envelope_editor_help_string);
}

void region_dialog_help(snd_state *ss)
{
  snd_help_with_url(ss,STR_Region_Browser,"#regionbrowser",
"This is the 'region browser'.  The scrolled\n\
window contains the list of current regions\n\
with a brief title to indicate the provenance\n\
thereof, and two buttons.  The 'save' button\n\
protects or unprotects the region from deletion.\n\
The 'play' button plays the associated region.\n\
One channel of the currently selected region\n\
is displayed in the graph window.  The up and\n\
down arrows move up or down in the region's\n\
list of channels.  If you click a region's\n\
title, the text is highlighted, and that region\n\
is displayed in the graph area.  You can cause\n\
that region to become the current 'selection'\n\
by clicking the 'Select' button (this merely\n\
moves the region to the top slot in the region\n\
list).  You can delete the selected region by\n\
clicking the 'Delete' button.  To dismiss the\n\
browser, click 'Ok'.  The 'edit' button\n\
loads the region into the main editor as a temporary\n\
file.  It can be edited or renamed, etc.  If you save\n\
the file, the region is updated to reflect any edits\n\
you made.\n\
\n");
}

void raw_data_dialog_help(snd_state *ss)
{
  snd_help(ss,
       STR_Raw_Data,
"To display and edit sound data, Snd needs\n\
to know how the data's sampling rate, number\n\
of channels, and numerical format.  This dialog\n\
gives you a chance to set those fields.\n\
To make the current settings the default\n\
for any future headerless files, click the\n\
'Default' button.\n\
");
}

void new_file_dialog_help(snd_state *ss)
{
  snd_help(ss,
	   "New File",
"This dialog sets the new file's output header type,\n\
data format, srate, chans, and comment if any.\n\
");
}

void file_mix_dialog_help(snd_state *ss)
{
  snd_help(ss,
	   "File Mix",
"The file you specify to the mix file prompt\n\
will be mixed into the current active sound at\n\
the current cursor location of the active channel.\n\
The equivalent keyboard command is C-x C-q.\n\
");
}

void edit_header_dialog_help(snd_state *ss)
{
  snd_help(ss,
       STR_Edit_Header,
"This dialog edits the header of a sound file.\n\
No change is made to the actual sound data; the\n\
new header is blindly written, any unsaved edits\n\
are ignored. If you specify 'raw' as the type,\n\
any existing header is removed.  This dialog is\n\
aimed at adding or removing an entire header, \n\
or editing the header comments; anything else\n\
is obviously dangerous.\n\
");
}

void print_dialog_help(snd_state *ss)
{
  snd_help(ss,
	   "File Print",
"Print causes the currently active display to be\n\
either printed (via the lpr command) or saved as\n\
an eps file.  In the latter case, the file name\n\
is set either by the dialog, or taken from the\n\
resource epsFile (normally snd.eps).\n\
");
}

void view_files_dialog_help(snd_state *ss)
{
  snd_help(ss,
       "File Browser",
"This dialog provides two lists, one of\n\
the currently active files in Snd, and\n\
the other of previously active files.\n\
The save button saves current edits, the\n\
play button plays the file, and the\n\
unlist button removes a file from the\n\
previous files list.  If a file is deleted\n\
while Snd is running, it will not notice\n\
its deletion automatically.  To update the\n\
previous files list to account for such\n\
actions, click on the 'update' button.\n\
To reopen a previous file, simply select\n\
that file from the previous files list.\n\
To remove all files from the previous files\n\
list, click the 'clear' button.  To select\n\
one of the current files in Snd, opening\n\
its associated windows, select it in the\n\
current files list.\n\
\n\
To preload all the sound files in a directory\n\
into the previous files list, use either the\n\
command (" S_preload_directory " dir), as in\n\
\n\
  M-x (" S_preload_directory " \"/usr/people/bil/hdr)\"\n\
\n\
or give the directory name to the -p flag\n\
when starting Snd:\n\
\n\
  snd -p . oboe.snd\n\
\n\
To preload a specific file,\n\
\n\
  (" S_preload_file " <name>)\n\
\n\
The 'sort' label on the right activates\n\
a menu of sorting choices; 'name' sorts the\n\
previous files list alphabetically, 'date'\n\
sorts by date written, 'size' sorts by the\n\
number of samples in the sound, and 'entry'\n\
sorts by the order the sound appears in the\n\
absence of explicit sorting.  The variable\n\
" S_previous_files_sort " (default 0:\n\
unsorted) refers to this menu.\n\
");	   
}

void stats_dialog_help(snd_state *ss)
{
  snd_help(ss,
       STR_Disk_and_Memory_Usage,
"This window gives an approximate notion of how\n\
much memory (RAM) and disk space each channel is\n\
taking up.  As a channel is edited, the relevant\n\
data is saved either in arrays or temporary files.\n\
The number of bytes in these arrays, and the number\n\
of such arrays are the first two numbers; then\n\
comes the space in bytes the channel takes up in\n\
the main (presumably permanent) file; the next\n\
three numbers give the number of bytes in the\n\
temporary files, the number of such files, and\n\
the number of these files that are currently\n\
being held open.  The related variable is\n\
" S_show_usage_stats ".  The 'Update' button forces\n\
the stats to be regathered, in case the display\n\
somehow gets out of sync with the actual data.\n\
\n\
");
}

void ssnd_help(snd_state *ss, char *subject, ...)
{
  va_list ap;
  char *helpstr,*newstr;
  int len,size;
  va_start(ap,subject);
  size = 1024;
  newstr = (char *)CALLOC(size,sizeof(char));
  len = 0;
  while ((helpstr = va_arg(ap,char *)))
    {
      len += strlen(helpstr);
      if (len >= size)
	{
	  size = len+1024;
	  newstr = (char *)REALLOC(newstr,size * sizeof(char));
	}
      strcat(newstr,helpstr);
    }
  va_end(ap);
  snd_help(ss,subject,newstr);
  FREE(newstr);
}  

void ssnd_help_with_url(snd_state *ss, char *subject, char *url, ...)
{
#if HAVE_HTML
  snd_help(ss,subject,url);
#else
  /* groan! */
  va_list ap;
  char *helpstr,*newstr;
  int len,size;
  va_start(ap,url);
  size = 1024;
  newstr = (char *)CALLOC(size,sizeof(char));
  len = 0;
  while ((helpstr = va_arg(ap,char *)))
    {
      len += strlen(helpstr);
      if (len >= size)
	{
	  size = len+1024;
	  newstr = (char *)REALLOC(newstr,size * sizeof(char));
	}
      strcat(newstr,helpstr);
    }
  va_end(ap);
  snd_help_with_url(ss,subject,url,newstr);
  FREE(newstr);
#endif
}

void snd_help_with_url(snd_state *ss, char *subject, char *url, char *helpstr)
{
#if HAVE_HTML
  snd_help(ss,subject,url);
#else
  snd_help(ss,subject,helpstr);
#endif
}

