#include "snd.h"
#include "sndlib-strings.h"
#include "vct.h"
#include "clm2xen.h"

void ssnd_help(snd_state *ss, char *subject, ...)
{
  va_list ap;
  char *helpstr, *newstr;
  int len, size;
  va_start(ap, subject);
  size = 1024;
  newstr = (char *)CALLOC(size, sizeof(char));
  len = 0;
  while ((helpstr = va_arg(ap, char *)))
    {
      len += snd_strlen(helpstr);
      if (len >= size)
	{
	  size = len + 1024;
	  newstr = (char *)REALLOC(newstr, size * sizeof(char));
	}
      strcat(newstr, helpstr);
    }
  va_end(ap);
  snd_help(ss, subject, newstr);
  FREE(newstr);
}  

static void snd_help_with_url(snd_state *ss, char *subject, char *url, char *helpstr)
{
#if HAVE_HTML
  snd_help(ss, subject, url);
#else
  snd_help(ss, subject, helpstr);
#endif
}

static void snd_help_with_url_and_wrap(snd_state *ss, char *subject, char *url, char *helpstr)
{
#if HAVE_HTML
  snd_help(ss, subject, url);
#else
  snd_help_with_wrap(ss, subject, helpstr);
#endif
}

static void ssnd_help_with_url(snd_state *ss, char *subject, char *url, ...)
{
#if HAVE_HTML
  snd_help(ss, subject, url);
#else
  /* groan! */
  va_list ap;
  char *helpstr, *newstr;
  int len, size;
  va_start(ap, url);
  size = 1024;
  newstr = (char *)CALLOC(size, sizeof(char));
  len = 0;
  while ((helpstr = va_arg(ap, char *)))
    {
      len += snd_strlen(helpstr);
      if (len >= size)
	{
	  size = len + 1024;
	  newstr = (char *)REALLOC(newstr, size * sizeof(char));
	}
      strcat(newstr, helpstr);
    }
  va_end(ap);
  snd_help_with_url(ss, subject, url, newstr);
  FREE(newstr);
#endif
}

/* ---------------- help 'news' menu item ---------------- */

static char *snd_itoa(int n)
{
  char *str;
  str = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
  mus_snprintf(str, LABEL_BUFFER_SIZE, "%d", n);
  return(str);
}

#if USE_MOTIF
  #include <X11/IntrinsicP.h>
  #if HAVE_XPM
    #include <X11/xpm.h>
  #endif
  #if HAVE_HTML
    #include <XmHTML/XmHTML.h>
  #endif
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
	buf = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char)); /* memory leak here is the least of our worries... */
	mus_snprintf(buf, LABEL_BUFFER_SIZE, " Snd expects %d bit int samples, but sndlib uses %d bits!",
		     MUS_SAMPLE_BITS,
		     mus_sample_bits());
	return(buf);
      }
#endif  
  return("");
}

#if HAVE_GNU_LIBC_VERSION_H
  #include <gnu/libc-version.h>
#endif

static char* vstrcat(char *buf, ...)
{
  va_list ap;
  char *str;
  va_start(ap, buf);
  while ((str = va_arg(ap, char *)))
    {
      strcat(buf, str);
    }
  va_end(ap);
  return(buf);
}

#if HAVE_GUILE
static char *xm_version(void)
{
  char *version = NULL;
  XEN xm_val;
  xm_val = XEN_EVAL_C_STRING("(and (provided? 'xm) xm-version)");
  if (XEN_STRING_P(xm_val))
    {
      version = (char *)CALLOC(32, sizeof(char));
      mus_snprintf(version, 32, "\n    xm: %s", XEN_TO_C_STRING(xm_val));
      return(version);
    }
  return("");
}
#endif

char *version_info(void)
{
  #define NUM_ITOAS 19
  char *buf;
  char **itoa;
  int i;
  buf = (char *)CALLOC(1024, sizeof(char));
  itoa = (char **)CALLOC(NUM_ITOAS, sizeof(char *));
  vstrcat(buf,
	  "This is Snd version ",
	  SND_RPM_VERSION,
	  " of ",
	  SND_VERSION,
	  ":\n    ", xen_version(),
	  "\n    ", mus_audio_moniker(),
	  "\n    Sndlib ", itoa[15] = snd_itoa(SNDLIB_VERSION), ".", 
                           itoa[16] = snd_itoa(SNDLIB_REVISION), 
                           " (", SNDLIB_DATE,
#if SNDLIB_USE_FLOATS
	  ", float samples",
#else
	  ", int", itoa[17] = snd_itoa(MUS_SAMPLE_BITS), " samples",
#endif
#if LONG_INT_P
	  " and long int* tables",
#endif
	  ")", sndlib_consistency_check(),
	  "\n    CLM ", itoa[0] = snd_itoa(MUS_VERSION), ".", 
	                itoa[1] = snd_itoa(MUS_REVISION), " (", 
                        MUS_DATE, ")",
#if HAVE_GSL
	  "\n    GSL",
  #ifdef GSL_VERSION
          ": ", GSL_VERSION,
  #endif
#endif
#if USE_MOTIF
  #ifdef LESSTIF_VERSION
	  "\n    Lesstif ", itoa[8] = snd_itoa(LESSTIF_VERSION), ".", 
                            itoa[9] = snd_itoa(LESSTIF_REVISION), " ",
  #endif
	  "\n    Motif ", itoa[10] = snd_itoa(XmVERSION), ".", 
                          itoa[11] = snd_itoa(XmREVISION), ".", 
                          itoa[12] = snd_itoa(XmUPDATE_LEVEL),
	  " X", itoa[13] = snd_itoa(X_PROTOCOL), "R", 
                itoa[14] = snd_itoa(XT_REVISION),
#endif
#if USE_GTK
	  "\n    Gtk+ ", itoa[9] = snd_itoa(GTK_MAJOR_VERSION), ".", 
                         itoa[10] = snd_itoa(GTK_MINOR_VERSION), ".", 
                         itoa[11] = snd_itoa(GTK_MICRO_VERSION),
	  ", Glib ",     itoa[12] = snd_itoa(GLIB_MAJOR_VERSION), ".", 
                         itoa[13] = snd_itoa(GLIB_MINOR_VERSION), ".", 
                         itoa[14] = snd_itoa(GLIB_MICRO_VERSION),
#endif
#if HAVE_GUILE
	  xm_version(),
#endif
#if HAVE_GTKEXTRA
	  "\n    gtkextra",
  #ifdef GTKEXTRA_VERSION
          ": ", GTKEXTRA_VERSION,
  #endif
#endif
#if (!(defined(USE_MOTIF))) && (!(defined(USE_GTK)))
	  "\n    without any graphics system",
#endif
#if HAVE_HTML
  #if USE_MOTIF
	  "\n    XmHTML ", itoa[5] = snd_itoa(XmHTMLVERSION), ".", 
                           itoa[6] = snd_itoa(XmHTMLREVISION), ".", 
                           itoa[7] = snd_itoa(XmHTMLUPDATE_LEVEL),
  #else
	  "\n    with mozilla browser",
  #endif
#endif
#if ((HAVE_XPM) && (defined(USE_MOTIF)))
	  "\n    Xpm ", itoa[2] = snd_itoa(XpmFormat), ".", 
                        itoa[3] = snd_itoa(XpmVersion), ".", 
                        itoa[4] = snd_itoa(XpmRevision),
#endif
#if HAVE_LADSPA
	  "\n    with LADSPA",
#endif
#if SND_AS_WIDGET
	  "\n    (compiled as a widget)",
#endif
#ifdef __DATE__
	  "\n    Compiled ", __DATE__, " ", __TIME__,
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
	  "\n    Libc: ", gnu_get_libc_version(), ".", 
                          gnu_get_libc_release(),
#endif
	  "\n", NULL);
  for (i = 0; i < NUM_ITOAS; i++) 
    if (itoa[i])
      FREE(itoa[i]);
  FREE(itoa);
  return(buf);
}

void news_help(snd_state *ss)
{
  char *info;
  info = version_info();
  ssnd_help(ss, STR_News,
	    info,
	    "\nRecent changes include:\n\
\n\
28-Nov:  add-tooltip in snd-motif.scm.\n\
23-Nov:  draw-mark-hook.\n\
20-Nov:  xm-enved.scm.\n\
19-Nov:  reopen menu example in examp.scm.\n\
13-Nov:  edit123.scm by Tom Roth added to contrib directory.\n\
         emacs-style-save-as\n\
10-Nov:  new-widget-hook.\n\
5-Nov:   snd 5.4.\n\
29-Oct:  added contrib/dlp directory with Dave Phillips' ladspa plugin stuff.\n\
16-Oct:  multichannel ladspa plugins, ladspa-dir.\n\
",
#if HAVE_GUILE
	    "\n    *features*: \n'",
	    word_wrap(XEN_TO_C_STRING(XEN_TO_STRING(XEN_EVAL_C_STRING("*features*"))), 600),
	    "\n\n",
#else
	    "\n",
#endif
NULL);
  FREE(info);
}

/* ---------------- help menu strings ---------------- */

static char file_menu_help_string[] =
"  Open: open a file.\n\
  Close: close the currently selected file.\n\
  Save: save any edits on the current file.\n\
  Save as: save edits on the current file in some new file.\n\
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
  Find: a global search -- operates across all currently sync'd sounds.\n\
  Delete selection: delete selected portion.\n\
  Insert selection: insert the selection at the current location\n\
  Mix selection: add the selection at the current location\n\
  Play selection: play the current selection.\n\
  Save selection: save the current selection in a file.\n\
  Select all: select entire file (following sync).\n\
  Edit Envelope: start the envelope editor dialog.\n\
  Edit Header: view or edit file's header.\n\
";

static char view_menu_help_string[] =
"  Show Controls: display the control pane.\n\
  Show Listener: open lisp listener panel.\n\
  Mix Panel: open the mix controller.\n\
  Regions: fire up the region browser.\n\
  Files: fire up the file browser.\n\
  Color: col" STR_OR " browser for sonogram.\n\
  Orientation: sonogram orientation.\n\
  Equalize Panes: During editing with multiple\n\
     files and channels, some data may be\n\
     obscured or compressed by changed window\n\
     sizes.  Equalize Panes returns Snd to a state\n\
     where everything is equally compressed.\n\
  Channel style: combine all channels into one graph.\n\
  Graph style: display data as dots, lines or, filled polygons.\n\
  Show Y = 0: display the y = 0 line.\n\
  Verbose cursor: show cursor loc and sample value.\n\
  X axis units: x axis labels in seconds, samples, ect.\n\
";

static char options_menu_help_string[] = 
"   Transform Options: various fft-related settings\n\
   Speed style: control panel speed scrollbar interpretation.\n\
   Zoom focus: where to focus during zooms.\n\
   Save Options: save the current Options and View menu settings.\n\
   Save state: save current state of Snd.\n\
   Show stats: show some memory and disk usage statistics.\n\
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
  Recording: how to use the recorder.\n\
  CLM: brief info about CLM functions.\n\
  News: description of this version of Snd.\n\
";

static char about_snd_help_string[] = 
"Snd is a sound editor. See snd.html for full\n\
details.  Please send bug reports or suggestions to\n\
bil@ccrma.stanford.edu.\n\
\n\
To get started, go to the File menu, and\n\
open a sound file.  To hear the sound, click\n\
the 'play' button. To see an fft, click the\n\
'f' button on the left.  The left mouse button\n\
is used for most pointing operations; the\n\
middle button pastes in the current selection; \n\
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
  c-s: search until a function returns true\n\
       The function should take one argument.\n\
       the current sample value.  For example.\n\
       to search for a sample greater than .1,\n\
       (lambda (val) (> val .1))\n\
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
  c-[Space]: start selection definition\n\
       - c-[Space] to deactivate selection\n\
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
  o: move to next or previous graph\n\
  p: play selection or region n\n\
  q: mix in region\n\
  r: redo last undone edit\n\
  u: undo last edit\n\
  v: position window over current selection\n\
  w: save selection as file\n\
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
  c-o: show controls\n\
  c-p: set window size (preceded by 1 arg)\n\
  c-q: mix in file\n\
  c-r: redo last undone edit\n\
  c-s: save file\n\
  c-u: undo last edit\n\
       Snd supports 'unlimited undo/redo'\n\
  c-v: set window size as percentage of total\n\
  c-w: save current channel in file\n\
  c-z: smooth using cosine\n\
\n\
Unless otherwise noted, case is not significant; \n\
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
  " S_transform_size "   Multiply (*)        Divide (/)\n\
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
" S_transform_normalization " to " S_dont_normalize_transform ".\n\
\n\
The harmonic analysis function is normally the\n\
Fourier Transform, but others are available,\n\
including about 20 wavelet choices, the Hankel and\n\
Chebyshev transforms, and perhaps others.\n\
\n\
";


void find_help(snd_state *ss) 
{
  snd_help_with_url_and_wrap(ss, STR_Find, "#find", 
"Searches in Snd refer to the sound data, and are in general patterned after Emacs.  When you type \
c-s or c-r, the minibuffer below the graph is activated and you are asked for the search function. \
The expression is a Scheme function of one argument, the current sample value.  It should return #t when the \
search is satisified.  For example, (lambda (n) (> n .1) looks for the next sample that is greater than .1. \
Successive c-s or c-r repeat the search.  c-x c-s can redefine the search pattern, which is also cleared in other \
events, much like Emacs. \
\n\n\
Normally, the search applies only to the current channel. To search all current files at once, use the Edit menu's find option.");
}

void undo_help(snd_state *ss) 
{
  snd_help_with_url_and_wrap(ss, STR_Undo, "#undoredo",
"Snd supports unlimited undo in the sense that you can backup through all \
the edits since the last save, and at any point redo those edits.  Certain \
operations require that temporary files be written, so disk space may eventually \
become a problem.  Revert is the same as backing up to the last save. \
\n\n\
In addition, eight or so of the previous selections are saved on a stack accessible via c-y.");
}

void sync_help(snd_state *ss) 
{
  snd_help_with_url_and_wrap(ss, STR_Sync, "#multichannel",
"The sync button causes certain operations to apply to all channels simultaneously.  In mono \
sounds, the sync button has a similar effect, but applied across multiple sounds. \
\n\n\
To get multi-channel selections, set the sync button, then define the selection (by dragging \
the mouse) in one channel, and the parallel portions of the other channels will also be selected. ");
}

static char speed_help_string[] = 
"'Speed' refers to the rate at which the sound data is consumed during playback. \
Another term might be 'srate'.  Snd uses linear interpolation to perform the speed \
change.  The arrow button on the right determines the direction we move through the data. \
The scroll bar position is normally interpreted as a float between .05 and 20.  The Options \
Speed Style menu (or the " S_speed_control_style " variable) can change this to use semitones (actually microtones) \
or just-intonation ratios.  The number of equal divisions to the octave in the semitone case is \
set by the variable " S_speed_control_tones " (normally 12). \
\n\
";

static char expand_help_string[] = 
"'Expand' refers to a kind of granular synthesis used to change the tempo of events \
in the sound without changing pitch.  Successive short slices of the file are overlapped with \
the difference in size between the input and output hops (between successive slices) giving \
the change in tempo.  This doesn't work in all files -- it sometimes sounds like execrable reverb \
or is too buzzy -- but it certainly is more robust than the phase vocoder approach to the \
same problem.  \
\n\n\
There are a variety of variables that control hop sizes, segment lengths, and overall segment \
envelopes.  These can be set in the minibuffer via m-x and setf, or in your Snd init file. \
The variables are:\n\
\n\
  " S_expand_control_ramp ": the length of the ramp up (.4, 0 to .5)\n\
  " S_expand_control_length ": the length of each slice (.15)\n\
  " S_expand_control_hop ": the hop size (.05)\n\
\n\
The expander is on only if the expand button is set. \
\n\
";

static char reverb_help_string[] = 
"The Snd reverberator is a version of Michael McNabb's Nrev.  In addition to the controls \
in the control pane, you can set the reverb feedback gains and the coefficient of the low \
pass filter in the allpass bank. The variables are '" S_reverb_control_feedback "' and '" S_reverb_control_lowpass "'. \
The reverb is on only if the reverb button is set.\
";

void contrast_help(snd_state *ss) 
{
  snd_help_with_url_and_wrap(ss, STR_Contrast, "#contrast",
"'Contrast enhancement' is my name for this somewhat weird waveshaper or compander.  It \
phase-modulates a sound, which can in some cases make it sound sharper or brighter. \
For softer sounds, it causes only an amplitude change.  Contrast is on only if the contrast button is set.");
}

void env_help(snd_state *ss) 
{
  snd_help_with_url_and_wrap(ss, STR_Envelope, "#editenvelope",
"An envelope in Snd is a list of x y break-point pairs. The x axis range is \
arbitrary. For example, to define a triangle curve: '(0 0 1 1 2 0). There is no (obvious) limit \
on the number of breakpoints. \
\n\n\
To apply an envelope to a sound, use the extended command C-x C-a. If this command gets a numeric \
argument, the envelope is applied from the cursor for that many samples. \
\n\n\
  C-x a     apply amplitude envelope to selection\n\
  C-x C-a   apply amplitude envelope to channel\n\
\n\n\
You can also specify a envelope name to the C-x C-a prompt. \
\n\n\
To scale a file or selection by or to some amplitude, use the M-x commands:\n\
\n\
  " S_scale_by " args\n\
  " S_scale_to " args\n\
  " S_scale_selection_by " args\n\
  " S_scale_selection_to " args\n\
\n\
" S_scale_by " scales the current sync'd channels by its arguments, and " S_scale_to " scales them to its \
arguments (a normalization). The arguments in each case are either a list of floats \
corresponding to each successsive member of the current set of sync'd channels, or just one \
argument. In the latter case, " S_scale_by " uses that scaler for all its channels, and " S_scale_to " \
normalizes all the channels together so that the loudest reaches that amplitude (that is, " S_scale_to " \
.5) when applied to a stereo file means that both channels are scaled by the same amount so that the \
loudest point in the file becomes .5). ");
}

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
     SPPACK, ADC (OGI), AVR, VOC, PVF,\n\
     Sound Tools, Turtle Beach SMP, SoundFont 2.0,\n\
     Sound Designer I and II, PSION, MAUD, Kurzweil 2000,\n\
     Tandy DeskMate, Gravis Ultrasound, ASF, PAF, CSL,\n\
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
macro, or dealt with from the lisp listener panel. The syntax used is lisp; \n\
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
Transform Graph style (the Transform Options Display choice):\n\
  " S_graph_transform_once "   " S_graph_transform_as_sonogram "  " S_graph_transform_as_spectrogram "\n\
\n\
Transform type:\n\
  " S_fourier_transform "  " S_wavelet_transform "   " S_hankel_transform "  " S_chebyshev_transform "   " S_cepstrum "\n\
  " S_autocorrelation "    " S_walsh_transform "  " S_hadamard_transform " " S_haar_transform "\n\
\n\
FFT Window type:\n\
  " S_rectangular_window " " S_hann_window " " S_welch_window " "  S_parzen_window "\n\
  " S_bartlett_window " " S_hamming_window " " S_blackman2_window " "  S_blackman3_window "\n\
  " S_blackman4_window " " S_exponential_window " " S_riemann_window " " S_kaiser_window "\n\
  " S_cauchy_window " " S_poisson_window " " S_gaussian_window " " S_tukey_window "\n\
  " S_dolph_chebyshev_window "\n\
\n\
Transform normalization choice:\n\
  " S_dont_normalize_transform " " S_normalize_transform_by_channel " " S_normalize_transform_by_sound " " S_normalize_transform_globally "\n\
\n\
Zoom Focus style:\n\
  " S_zoom_focus_left "    " S_zoom_focus_right "   " S_zoom_focus_active " " S_zoom_focus_middle "\n\
\n\
X-axis Label:\n\
  " S_x_axis_in_seconds "  " S_x_axis_in_samples "  " S_x_axis_as_percentage "\n\
\n\
Speed Control style:\n\
  " S_speed_control_as_float "     " S_speed_control_as_ratio "     " S_speed_control_as_semitone "\n\
\n\
Channel Combination style; \n\
  " S_channels_separate "  " S_channels_combined "  " S_channels_superimposed "\n\
\n\
Envelope Editor target:\n\
  " S_enved_amplitude "      " S_enved_spectrum "       " S_enved_srate "\n\
\n\
Graph Line style:\n\
  " S_graph_lines "        " S_graph_dots "         " S_graph_filled "    " S_graph_lollipops "\n\
  " S_graph_dots_and_lines "\n\
\n\
Keyboard action choices:\n\
  " S_cursor_in_view "     " S_cursor_on_left "     " S_cursor_on_right "   " S_cursor_in_middle "\n\
  " S_cursor_update_display " " S_cursor_no_action " " S_keyboard_no_action "\n\
\n\
Cursor style:\n\
  " S_cursor_cross "    " S_cursor_line "\n\
\n\
Axis choice:\n\
  " S_show_no_axes "    " S_show_all_axes "         " S_show_x_axis "\n\
\n\
Graph:\n\
  " S_time_graph "      " S_transform_graph "       " S_lisp_graph "\n\
";

static char variables_help_string[] =
"These variables are accessed as though each were a function\n\
of no arguments, and set using a function with \"set!\".\n\
For example, " S_auto_resize "'s current value can be\n\
accessed via (" S_auto_resize "), and set to a\n\
new value via (set! (" S_auto_resize ") #t). \n\
\n\
  " S_ask_before_overwrite "  #f\n\
  " S_audio_input_device "    " S_mus_audio_default "\n\
  " S_audio_output_device "   " S_mus_audio_default "\n\
  " S_audio_state_file "      \"" AUDIO_STATE_FILE "\"\n\
  " S_auto_resize "           #t\n\
  " S_auto_update "           #f\n\
  " S_axis_label_font "       varies\n\
  " S_axis_numbers_font "     varies\n\
  " S_basic_color "           ivory2\n\
  " S_beats_per_minute "      60\n\
  " S_bold_button_font "      varies\n\
  " S_button_font "           varies\n\
  " S_color_cutoff "          0.003\n\
  " S_color_inverted "        #t\n\
  " S_color_scale "           0.5\n\
  " S_colormap "             -1\n\
  " S_cursor_color "          red\n\
  " S_dac_combines_channels " #t\n\
  " S_dac_size "              256\n\
  " S_data_color "            black\n\
  " S_dot_size "              1 (snd #t) (chn #t)\n\
  " S_emacs_style_save_as "   #f\n\
  " S_enved_active_env "      '()\n\
  " S_enved_base "            1.0\n\
  " S_enved_clip_p "           #f\n\
  " S_enved_in_dB "           #f\n\
  " S_enved_exp_p "            #f\n\
  " S_enved_filter "          #t\n\
  " S_enved_filter_order "    40\n\
  " S_enved_power "           3.0\n\
  " S_enved_selected_env "    '()\n\
  " S_enved_target "          " S_enved_amplitude "\n\
  " S_enved_waveform_color "  blue\n\
  " S_enved_wave_p "           #f\n\
  " S_eps_bottom_margin "     0\n\
  " S_eps_file "              \"snd.eps\"\n\
  " S_eps_left_margin "       0\n\
  " S_eps_size "              1\n\
  " S_fft_window_beta "       0.0 (snd #t) (chn #t)\n\
  " S_fft_log_frequency "     #f (snd #t) (chn #t)\n\
  " S_fft_log_magnitude "     #f (snd #t) (chn #t)\n\
  " S_fft_window "            " S_blackman2_window " (snd #t) (chn #t)\n\
  " S_filter_env_in_hz "      #f\n\
  " S_filter_waveform_color " blue\n\
  " S_graph_color "           white\n\
  " S_graph_cursor "          XC_crosshair (34)\n\
  " S_graph_style "           " S_graph_lines " (snd #t) (chn #t)\n\
  " S_graphs_horizontal "     #t (snd #t) (chn #t)\n\
  " S_help_text_font "        varies\n\
  " S_highlight_color "       ivory1\n\
  " S_just_sounds "           #f\n\
  " S_ladspa_dir "            nil\n\
  " S_listener_color "        aliceblue\n\
  " S_listener_font "         varies\n\
  " S_listener_prompt "       \">\"\n\
  " S_listener_text_color "   black\n\
  " S_mark_color "            red\n\
  " S_max_transform_peaks "   100 (snd #t) (chn #t)\n\
  " S_min_dB "               -60.0 (snd #t) (chn #t)\n\
  " S_minibuffer_history_length " 8\n\
  " S_mix_color "             lightgreen\n\
  " S_mix_tag_width "         6\n\
  " S_mix_tag_height "        14\n\
  " S_mix_tag_y "             0\n\
  " S_mix_waveform_height "   20\n\
  " S_movies "                #t\n\
  " S_transform_normalization " " S_normalize_transform_by_channel " (snd #t) (chn #t)\n\
  " S_position_color "        ivory3\n\
  " S_print_length "          12\n\
  " S_pushed_button_color "   lightsteelblue1\n\
  " S_recorder_autoload "     #f\n\
  " S_recorder_buffer_size "  4096\n\
  " S_recorder_file "         nil\n\
  " S_recorder_in_format "    " S_mus_bshort "\n\
  " S_recorder_max_duration " 1000000.0\n\
  " S_recorder_out_chans "    2\n\
  " S_recorder_out_format "   same as above\n\
  " S_recorder_srate "        22050\n\
  " S_recorder_trigger "      0.0\n\
  " S_reverb_control_decay "  1.0 &optional (snd #t)\n\
  " S_sash_color "            lightgreen\n\
  " S_save_dir "              nil\n\
  " S_save_state_file "       nil\n\
  " S_selected_data_color "   black\n\
  " S_selected_graph_color "  white\n\
  " S_selected_mix_color "    green2\n\
  " S_selection_color "       lightsteelblue1\n\
  " S_selection_creates_region " #t\n\
  " S_show_axes "             show-all-axes (snd #t) (chn #t)\n\
  " S_show_backtrace "        #f\n\
  " S_show_indices "          #f\n\
  " S_show_marks "            #t (snd #t) (chn #t)\n\
  " S_show_mix_waveforms "    #f (snd #t) (chn #t)\n\
  " S_show_selection_transform " #f\n\
  " S_show_transform_peaks "  #f (snd #t) (chn #t)\n\
  " S_show_usage_stats "      #f\n\
  " S_show_y_zero "           #f (snd #t) (chn #t)\n\
  " S_sinc_width "            10\n\
  " S_spectro_cutoff "        1.0 (snd #t) (chn #t)\n\
  " S_spectro_hop "           4 (snd #t) (chn #t)\n\
  " S_spectro_start "         0.0 (snd #t) (chn #t)\n\
  " S_spectro_x_angle "       90.0 (snd #t) (chn #t)\n\
  " S_spectro_x_scale "       1.0 (snd #t) (chn #t)\n\
  " S_spectro_y_angle "       0.0 (snd #t) (chn #t)\n\
  " S_spectro_y_scale "       1.0 (snd #t) (chn #t)\n\
  " S_spectro_z_angle "      -2.0 (snd #t) (chn #t)\n\
  " S_spectro_z_scale "       0.1 (snd #t) (chn #t)\n\
  " S_speed_control_style "   " S_speed_control_as_float " (snd #t)\n\
  " S_speed_control_tones "   12 (snd #t)\n\
  " S_temp_dir "              nil\n\
  " S_text_focus_color "      white\n\
  " S_transform_size "        256 (snd #t) (chn #t)\n\
  " S_transform_graph_type "  " S_graph_transform_once " (snd #t) (chn #t)\n\
  " S_transform_type "        " S_fourier_transform " (snd #t) (chn #t)\n\
  " S_trap_segfault "         #t\n\
  " S_use_sinc_interp "       #f\n\
  " S_verbose_cursor "        #f (snd #t) (chn #t)\n\
  " S_vu_font "               nil\n\
  " S_vu_font_size "          1.0\n\
  " S_vu_size "               1.0\n\
  " S_wavelet_type "          0 (snd #t) (chn #t)\n\
  " S_wavo_hop "              3 (snd #t) (chn #t)\n\
  " S_wavo_trace "            64 (snd #t) (chn #t)\n\
  " S_window_height "         0\n\
  " S_window_width "          0\n\
  " S_window_x "             -1\n\
  " S_window_y "             -1\n\
  " S_with_mix_tags "         #t\n\
  " S_x_axis_style "          " S_x_axis_in_seconds "\n\
  " S_zero_pad "              0 (snd #t) (chn #t)\n\
  " S_zoom_color "            ivory4\n\
  " S_zoom_focus_style "      " S_zoom_focus_active "\n\
\n\
";

static char hooks_help_string[] =
"The hooks provide a way to customize various situations that arise through\n\
user-interface manipulations.\n\
\n\
  " S_after_edit_hook " ()\n\
  " S_after_graph_hook " (snd chn)\n\
  " S_after_open_hook " (snd)\n\
  " S_before_transform_hook " (snd chn)\n\
  " S_close_hook " (snd)\n\
  " S_drop_hook " (filename\n\
  " S_during_open_hook " (fd name reason)\n\
  " S_edit_hook " ()\n\
  " S_enved_hook " (env pt new-x new-y)\n\
  " S_exit_hook "\n\
  " S_transform_hook " (snd chn scaler)\n\
  " S_graph_hook " (snd chn y0 y1)\n\
  " S_initial_graph_hook " (snd chn dur)\n\
  " S_just_sounds_hook " (filename)\n\
  " S_lisp_graph_hook "(snd chn)\n\
  " S_mark_click_hook " (id)\n\
  " S_mark_drag_hook " (id)\n\
  " S_menu_hook " (name option)\n\
  " S_mix_amp_changed_hook " (id)\n\
  " S_mix_position_changed_hook " (id samps)\n\
  " S_mix_speed_changed_hook " (id)\n\
  " S_mouse_drag_hook " (snd chn button state x y)\n\
  " S_mouse_enter_graph_hook " (snd chn)\n\
  " S_mouse_enter_label_hook " (type position name)\n\
  " S_mouse_enter_listener_hook " (widget)\n\
  " S_mouse_enter_text_hook " (widget)\n\
  " S_mouse_leave_graph_hook " (snd chn)\n\
  " S_mouse_leave_label_hook " (type position name)\n\
  " S_mouse_leave_listener_hook " (widget)\n\
  " S_mouse_leave_text_hook " (widget)\n\
  " S_mouse_press_hook " (snd chn button state x y)\n\
  " S_mouse_release_hook " (snd chn button state x y)\n\
  " S_multichannel_mix_hook " (ids)\n\
  " S_mus_error_hook " (type msg)\n\
  " S_name_click_hook " (snd)\n\
  " S_open_hook " (filename)\n\
  " S_output_comment_hook " (str)\n\
  " S_output_name_hook "()\n\
  " S_play_hook " (samps)\n\
  " S_property_changed_hook " (command)\n\
  " S_save_hook " (snd name)\n\
  " S_select_channel_hook " (snd chn)\n\
  " S_select_mix_hook " (id)\n\
  " S_select_sound_hook " (snd)\n\
  " S_snd_error_hook " (msg)\n\
  " S_snd_warning_hook " (msg)\n\
  " S_start_hook " (filename)\n\
  " S_start_playing_hook " (snd)\n\
  " S_stop_playing_channel_hook " (snd chn)\n\
  " S_stop_playing_hook " (snd)\n\
  " S_stop_playing_region_hook " (reg)\n\
  " S_undo_hook " ()\n\
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
  " S_add_mark "          (sample snd chn)\n\
  " S_add_player "        (player start end)\n\
  " S_add_to_main_menu "  (menu-label)\n\
  " S_add_to_menu "       (top-menu menu-label callback)\n\
  " S_add_transform "     (name xlab lo hi transform)\n\
  " S_amp_control "       (snd)\n\
  " S_append_to_minibuffer " (msg snd)\n\
  " S_as_one_edit "       (func snd chn)\n\
  " S_autocorrelate "     (data)\n\
  " S_axis_info "         (snd chn grf)\n\
  " S_backward_graph "    (count)\n\
  " S_backward_mark "     (count)\n\
  " S_backward_mix "      (count)\n\
  " S_backward_sample "   (count)\n\
  " S_bind_key "          (key state code ignore-prefix)\n\
  " S_c_g "               ()\n\
  " S_c_g_x "              ()\n\
  " S_change_menu_label " (top-menu old-label new-label)\n\
  " S_channel_style "     (snd)\n\
  " S_channel_sync "      (snd chn)\n\
  " S_channels "          (snd)\n\
  " S_chans "             (snd)\n\
  " S_clear_audio_inputs "()\n\
  " S_close_sound "       (snd)\n\
  " S_close_sound_file "  (fd bytes)\n\
  " S_color_dialog "      ()\n\
  " S_color_p "            (obj)\n\
  " S_comment "           (snd)\n\
  " S_contrast_control "  (snd)\n\
  " S_contrast_control_amp " (snd)\n\
  " S_contrast_control_p " (snd)\n\
  " S_convolve_arrays "   (vect1 vect2)\n\
  " S_convolve_selection_with " (file amp)\n\
  " S_convolve_with "     (file amp snd chn)\n\
  " S_count_matches "     (c-expr start snd chn)\n\
  " S_cursor "            (snd chn)\n\
  " S_cursor_follows_play "(snd)\n\
  " S_cursor_position "   (snd chn)\n\
  " S_cursor_size "       (val snd chn)\n\
  " S_cursor_style "      (val snd chn)\n\
  " S_data_format "       (snd)\n\
  " S_data_location "     (snd)\n\
  " S_delete_mark "       (id snd chn)\n\
  " S_delete_marks "      (snd chn)\n\
  " S_delete_sample "     (samp snd chn edpos)\n\
  " S_delete_samples "    (samp samps snd chn edpos)\n\
  " S_delete_selection "  ()\n\
  " S_dismiss_all_dialogs "()\n\
  " S_edit_fragment "     (num snd chn)\n\
  " S_edit_header_dialog "()\n\
  " S_edits "             (snd chn)\n\
  " S_env_selection "     (envelope env-base snd chn)\n\
  " S_env_sound "         (envelope samp samps env-base snd chn)\n\
  " S_enved_dialog "      ()\n\
  " S_equalize_panes "    (snd)\n\
  " S_exit "              ()\n\
  " S_expand_control "    (snd)\n\
  " S_expand_control_hop " (snd)\n\
  " S_expand_control_length " (snd)\n\
  " S_expand_control_ramp " (snd)\n\
  " S_expand_control_p "   (snd)\n\
  " S_fft "               (rl im sgn)\n\
  " S_fht "               (rl)\n\
  " S_file_dialog "       ()\n\
  " S_file_name "         (snd)\n\
  " S_filter_control_env " (snd)\n\
  " S_filter_control_order " (snd)\n\
  " S_filter_control_p "   (snd)\n\
  " S_filter_selection "  (env order)\n\
  " S_filter_sound "      (env order snd chn)\n\
  " S_find "              (c-expr start snd chn)\n\
  " S_find_mark "         (samp snd chn)\n\
  " S_find_sound "        (filename)\n\
  " S_forget_region "     (reg)\n\
  " S_forward_graph "     (count)\n\
  " S_forward_mark "      (count)\n\
  " S_forward_mix "       (count)\n\
  " S_forward_sample "    (count)\n\
  " S_free_sample_reader "(rd)\n\
  " S_graph "             (data xlabel x0 x1 snd chn)\n\
  " S_graph_lisp_p "       (snd chn)\n\
  " S_graph_time_p "       (snd chn)\n\
  " S_graph_transform_p "  (snd chn)\n\
  " S_graph2ps "         ()\n\
  " S_header_type "       (snd)\n\
  " S_help_dialog "       (subject help)\n\
  " S_in "                (ms code)\n\
  " S_insert_region "     (beg reg snd chn)\n\
  " S_insert_sample "     (samp value snd chn edpos)\n\
  " S_insert_samples "    (samp data snd chn edpos)\n\
  " S_insert_selection "  (beg snd chn)\n\
  " S_insert_silence "    (beg num snd chn)\n\
  " S_insert_sound "      (file beg in_chan snd chn edpos)\n\
  " S_key "               (key state)\n\
  " S_key_binding "       (key state)\n\
  " S_left_sample "       (snd chn)\n\
  " S_listener_selection "()\n\
  " S_list2vct "         (lst)\n\
  " S_loop_samples "      (reader function calls origin environ)\n\
  " S_make_color "        (r g b)\n\
  " S_make_player "       (snd chn)\n\
  " S_make_region "       (beg end snd chn)\n\
  " S_make_region_sample_reader "(start snd chn dir)\n\
  " S_make_sample_reader "(start snd chn dir)\n\
  " S_make_vct "          (len)\n\
  " S_mark_home "         (mark)\n\
  " S_mark_name "         (mark)\n\
  " S_mark_sample "       (mark)\n\
  " S_mark_sync "         (mark)\n\
  " S_mark_sync_max "     ()\n\
  " S_marks "             (snd chn pos)\n\
  " S_mark_p "             (id)\n\
  " S_max_sounds "        ()\n\
  " S_maxamp "            (snd chn)\n\
  " S_mix "               (file samp in_chan snd chn)\n\
  " S_mix_panel "         ()\n\
  " S_mix_sound "         (file samp scaler)\n\
  " S_mixes "             ()\n\
  " S_mix_amp "           (mix chan)\n\
  " S_mix_amp_env "       (mix chan)\n\
  " S_mix_anchor "        (mix)\n\
  " S_mix_chans "         (mix)\n\
  " S_mix_home "          (mix)\n\
  " S_mix_length "        (mix)\n\
  " S_mix_locked "        (mix)\n\
  " S_mix_name "          (mix)\n\
  " S_mix_position "      (mix)\n\
  " S_mix_region "        (samp reg snd chn)\n\
  " S_mix_selection "     (samp snd chn)\n\
  " S_mix_speed "         (mix)\n\
  " S_mix_track "         (mix)\n\
  " S_mix_vct "           (vct beg chans snd chn)\n\
  " S_mix_p "              (id)\n\
  " S_mus_sound_length "  (snd chn)\n\
  " S_next_sample "       (rd)\n\
  " S_new_sound "         (name type format srate chans)\n\
  " S_open_raw_sound "    (name chans srate format)\n\
  " S_open_sound "        (name)\n\
  " S_open_sound_file "   (name chans srate comment)\n\
  " S_orientation_dialog "()\n\
  " S_peaks "             (file snd chn)\n\
  " S_play "              (samp snd chn sync end)\n\
  " S_play_and_wait "     (samp snd chn sync end)\n\
  " S_play_region "       (reg to-end)\n\
  " S_play_selection "    ()\n\
  " S_player_home "       (obj)\n\
  " S_player_p "           (obj)\n\
  " S_position2x "       (xpos snd chn ap)\n\
  " S_position2y "       (ypos snd chn ap)\n\
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
  " S_region_length "     (reg)\n\
  " S_region_maxamp "     (reg)\n\
  " S_region_sample "     (samp reg chn)\n\
  " S_region_samples "    (samp samps reg chn)\n\
  " S_region_samples2vct "(samp samps reg chn)\n\
  " S_region_srate "      (reg)\n\
  " S_regions "           ()\n\
  " S_region_p "           (id)\n\
  " S_report_in_minibuffer "(msg snd)\n\
  " S_reset_controls "    (snd)\n\
  " S_restore_controls "  (snd)\n\
  " S_reverb_control_feedback " (snd)\n\
  " S_reverb_control_length " (snd)\n\
  " S_reverb_control_lowpass " (snd)\n\
  " S_reverb_control_scale " (snd)\n\
  " S_reverb_control_p "   (snd)\n\
  " S_reverse_selection " ()\n\
  " S_reverse_sound "     (snd chn)\n\
  " S_revert_sound "      (snd)\n\
  " S_right_sample "      (snd chn)\n\
  " S_sample "            (samp snd chn)\n\
  " S_sample_reader_at_end_p "(rd)\n\
  " S_sample_reader_home "(rd)\n\
  " S_sample_reader_p "    (rd)\n\
  " S_samples "           (samp samps snd chn)\n\
  " S_samples2vct "      (samp samps snd chn)\n\
  " S_save_controls "     (snd)\n\
  " S_save_edit_history " (file snd chn)\n\
  " S_save_listener "     (filename)\n\
  " S_save_macros "       ()\n\
  " S_save_marks "        (snd)\n\
  " S_save_region "       (reg filename format)\n\
  " S_save_selection "    (file header-type data-format srate comment chan)\n\
  " S_save_sound "        (snd)\n\
  " S_save_sound_as "     (filename snd type format srate)\n\
  " S_save_state "        (filename)\n\
  " S_scale_by "          (scalers snd chn)\n\
  " S_scale_selection_by "(scalers)\n\
  " S_scale_selection_to "(scalers)\n\
  " S_scale_sound_by "    (scaler beg num snd chn)\n\
  " S_scale_sound_to "    (norm beg num snd chn)\n\
  " S_scale_to "          (scalers snd chn)\n\
  " S_search_procedure "  (snd)\n\
  " S_selected_channel "  (snd)\n\
  " S_selected_mix "      ()\n\
  " S_selected_sound "    ()\n\
  " S_selection_chans "   ()\n\
  " S_selection_length "  ()\n\
  " S_selection_member "  (snd chn)\n\
  " S_selection_position "()\n\
  " S_selection_srate "   ()\n\
  " S_selection_p "        ()\n\
  " S_short_file_name "   (snd)\n\
  " S_show_controls "     (snd)\n\
  " S_show_listener "     ()\n\
  " S_smooth_selection "  ()\n\
  " S_smooth_sound "      (beg num snd chn)\n\
  " S_sound_files_in_directory "(dir)\n\
  " S_sound_p "            (snd)\n\
  " S_sounds "            ()\n\
  " S_snd_apropos "       (name)\n\
  " S_snd_error "         (str)\n\
  " S_snd_help "          (name)\n\
  " S_snd_print "         (str)\n\
  " S_snd_spectrum "      (data window length linear)\n\
  " S_snd_tempnam "       ()\n\
  " S_snd_version "       ()\n\
  " S_snd_warning "       (str)\n\
  " S_speed_control "     (snd)\n\
  " S_speed_control_tones " (snd)\n\
  " S_squelch_update "    (snd chn)\n\
  " S_srate "             (snd)\n\
  " S_src_selection "     (num-or-env base)\n\
  " S_src_sound "         (num-or-env base)\n\
  " S_start_playing "     (chans srate background)\n\
  " S_stop_player "       (player)\n\
  " S_stop_playing "      (snd)\n\
  " S_swap_channels "     (snd1 chn1 snd2 chn2 beg dur)\n\
  " S_syncd_marks "       (sync)\n\
  " S_sync "              (snd)\n\
  " S_transform_dialog "  ()\n\
  " S_transform_sample "  (bin slice snd chn)\n\
  " S_transform_samples " (snd chn()\n\
  " S_transform_samples2vct " (snd chn)\n\
  " S_transform_samples_size " (snd chn)\n\
  " S_unbind_key "        (key state)\n\
  " S_undo "              (edits snd chn)\n\
  " S_update_lisp_graph " (snd chn)\n\
  " S_update_sound "      ()\n\
  " S_update_time_graph " (snd chn)\n\
  " S_update_transform "  (snd chn)\n\
  " S_widget_position "   (wid)\n\
  " S_widget_size "       (wid)\n\
  " S_vct_p "              (vobj)\n\
  " S_vct_addB "          (vobj1 vobj2)\n\
  " S_vct_copy "          (obj)\n\
  " S_vct_doB "           (obj proc)\n\
  " S_vct_fillB "         (vobj val)\n\
  " S_vct_length "        (vobj)\n\
  " S_vct_mapB "          (obj proc)\n\
  " S_vct_moveB "         (obj new old back)\n\
  " S_vct_subseq "        (obj start end v)\n\
  " S_vct_multiplyB "     (vobj1 vobj2)\n\
  " S_vct_offsetB "       (vobj val)\n\
  " S_vct_ref "           (vobj pos)\n\
  " S_vct_scaleB "        (vobj scl)\n\
  " S_vct_setB "          (vobj pos val)\n\
  " S_vct2samples "      (samp samps data snd chn)\n\
  " S_vct2sound_file "   (fd vobj vals)\n\
  " S_vcts_doB "          (obj ... proc)\n\
  " S_vcts_mapB "         (obj ... proc)\n\
  " S_vector2vct "       (vect)\n\
  " S_view_sound "        (filename)\n\
  " S_window_height "     ()\n\
  " S_window_width "      ()\n\
  " S_x_bounds "          (snd chn)\n\
  " S_x2position "       (x snd chn ap)\n\
  " S_y_bounds "          (snd chn)\n\
  " S_y2position "       (y snd chn ap)\n\
  " S_yes_or_no_p "        (ques)\n\
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
"Since mixing is the most common and most useful editing operation performed on \
sounds, there is relatively elaborate support for it in Snd. To mix in a file, \
use the File Mix menu option, the command C-x C-q, or one of the various \
mixing functions. Currently the only difference between the first two is that \
the Mix menu option tries to take the current sync state into account, whereas \
the C-x C-q command does not. To mix a selection, use C-x q. The mix starts at \
the current cursor location. It is displayed as a separate waveform above \
the main waveform with a red tag at the beginning.  You can drag the tag to \
reposition the mix. The underlying sound being mixed can be edited by the same \
functions used throughout Snd; the mix number is used as the first (only) \
member of a list where the functions take the sound index argument. It is \
usually handier, however, to adjust the mix via the Mix Panel. \
\n\n\
The Mix Panel is a dialog (under the View Menu) that provides various \
commonly-used controls on the currently selected mix. At the top are the mix id, \
name, begin and end times, track number, and a play button. Beneath that are \
various sliders controlling the speed (sampling rate) of the mix, amplitude of \
each input channel, and the amplitude envelope. This part of Snd is in flux currently. \
\n\n\
To move the cursor from one mix to the next, in the same manner as C-j moves through marks, use C-x C-j. \
\n\n\
It is often handy to collect several mixes into a 'track'; mix.scm implements a variety of track-related operations. \
";
#else
static char mix_help_string[] = "";
#endif

static char recording_help_string[] = 
"To make a recording, choose 'Record' from the File menu. A window opens with the various \
recording controls.  The top three panes display the status of the input and output lines. If a \
channel is active, its meter will glow yellow. If some signal clips during recording, \
the meter will flash red. The numbers below the channel buttons indicate the signal maximum \
since it was last reset. The sliders underneath the meters scale the audio data in various ways \
before it is mixed into the output. The vertical sliders on the right scale the line-in and \
microphone signals before the meter, and the output signal before it gets to the speaker \
(these are needed to avoid clipping on input,  and to set the 'monitor' volume of the output \
independent of the output file volume). \
\n\n\
The fourth pane has information about the current output file (its name and so on), and \
the layout of the window. The buttons on the right can be used to open and close panes \
painlessly. If the button is not square (a diamond on the SGI), the underlying audio \
hardware can't handle input from that device at the same time as it reads other 'radio' button \
devices. So, in that case, opening the panel via the button also turns off the other incompatible \
device. The fifth pane contains a history of whatever the recorder thought worth \
reporting. The duration field gives the current output file's duration. The bottom row of \
buttons dismiss the window, start recording, cancel the current take, and provide some \
help. There's also a slider on the far right that controls the speaker output volume \
(independent of the output file volume). \
\n\n\
To make a recording, choose the inputs and outputs you want; for example, to record channel \
A from the microphone to channel A of the output file, click the Microphone panel's A button and \
the Output panel's A button. Then when you're ready to go, click the Record button. Click it \
again to finish the recording. \
\n\n\
If the record window's VU meters are too big (or too small) for your screen, you can fool around \
with the variable " S_vu_size " which defaults to 1.0. Similarly the variable " S_vu_font_size " tries to \
change the size of the numbers on the label, and " S_vu_font " chooses the family name of the font used. \
\n\n\
If you go to the main Snd window while the recorder is active and play a sound, the \
recorder's audio lines are made inactive to try to reduce confusion.  To re-activate \
the recorder, press the 'reset' button at the bottom of the window. \
\n\n\
Digital input is slightly tricky -- you need to set the sampling rate before you \
click the 'digital input' button; otherwise you'll get a stuttering effect because the output \
(monitor) rate doesn't match the input rate. ";

void envelope_editor_dialog_help(snd_state *ss)
{
  snd_help_with_url_and_wrap(ss, "Envelope Editor", "#editenvelope", 
"The Edit Envelope dialog (under the Edit menu) fires up a window for viewing and editing \
envelopes. The dialog has a display showing either the envelope currently being edited or a panorama \
of all currently loaded envelopes. The current envelope can be edited with the mouse: click at \
some spot in the graph to place a new breakpoint, drag an existing breakpoint to change its \
position, and click an existing breakpoint to delete it. The Undo and Redo buttons can be used \
to move around in the list of envelope edits; the current state of the envelope can be saved with \
the 'save' button, or printed with 'print'. \
\n\n\
Envelopes can be defined using defvar, and loaded from a separate file of envelope definitions via \
the load function.  For example, the file: \
\n\n\
  (defvar ramp '(0 0 1 1))\n\
  (defvar pyramid '(0 0 1 1 2 0))\n\
\n\n\
defines two envelopes that can be used in Snd wherever an envelope is needed (e.g. C-x C-a). You \
can also define a new envelope in the dialog's text field; '(0 0 1 1) followed by return fires up \
a ramp as a new envelope. \
\n\n\
In the overall view of envelopes, click an envelope, or click its name in the scrolled list \
on the left to select it; click the selected envelope to load it into the editor portion, \
clearing out whatever was previously there.  To load an exisiting envelope into the editor, you \
can also type its name in the text field; to give a name to the envelope as it is currently defined \
in the graph viewer, type its name in this field, then either push return or the 'save' button. \
\n\n\
Once you have an envelope in the editor, it can be applied to the currently active sounds via the \
'Apply' or 'Undo&Apply' buttons; the latter first tries to undo the previous edit, then applies the \
envelope. The envelope can be applied to the amplitude, the spectrum, or the sampling rate. The \
choice is made via the three buttons marked 'amp', 'flt', and 'src'. The filter order is the variable \
" S_enved_filter_order " which defaults to 40. To apply the changes to the current selection, \
rather than the current sound, set the 'selection' button.  To apply it to the currently selected mix, \
set the 'mix' button. \
\n\n\
The two toggle buttons at the lower right choose whether to show a light-colored version of the \
currently active sound (the 'wave' button), and whether to clip mouse movement at the current y \
axis bounds (the 'clip' button).");
}

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

void speed_help(snd_state *ss) {snd_help_with_url_and_wrap(ss, STR_Speed, "#speed", speed_help_string);}
void expand_help(snd_state *ss) {snd_help_with_url_and_wrap(ss, STR_Expand, "#expand", expand_help_string);}
void reverb_help(snd_state *ss) {snd_help_with_url_and_wrap(ss, STR_Reverb, "#reverb", reverb_help_string);}
void marks_help(snd_state *ss) {snd_help_with_url(ss, STR_Marks, "#marks", mark_help_string);}
void mix_help(snd_state *ss) {snd_help_with_url_and_wrap(ss, STR_Mixing, "#mixingfiles", mix_help_string);}
void sound_files_help(snd_state *ss) {snd_help_with_url(ss, STR_Format, "#formats", sound_files_help_string);}
void recording_help(snd_state *ss) {snd_help_with_url_and_wrap(ss, STR_Recording, "#recordfile", recording_help_string);}
void init_file_help(snd_state *ss) {ssnd_help_with_url(ss, STR_Customization, "grfsnd.html", init_file_help_string, "\n", resource_help_string, NULL);}

#if HAVE_CLICK_FOR_HELP

/* -------- click for help -------- */

void click_for_file_menu_help(snd_state *ss)
{
  ssnd_help_with_url(ss,
		     "File Menu",
		     "#fileoperations",
"The File menu provides one way to open, \n\
close, and save files. Its options are:\n\
\n",
file_menu_help_string,
"\n\
The Print option produces a Postscript file; \n\
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
NULL);
}

void click_for_history_help(snd_state *ss)
{
  snd_help_with_url_and_wrap(ss,
		    "Edit History",
		    "#edithistory",
"The current state of the undo/redo list can be viewed as a scrolled list of strings in the pane \
on the left of the graph (in Motif 1, there's a 'Show Edit History' menu option).  If there are no \
current edits, it just lists the associated file name (i.e. the zero-edits state).  As you edit the \
sound, the operations appear in the edit list window.  Click on a member of the list to move to \
that point in the edit list (equivalent to some number of undo's or redo's).  To move to a given \
edit point and follow the sync chain (if any), use control-click.");
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

void click_for_name_separator_help(snd_state *ss)
{
  snd_help_with_wrap(ss, "Name Separator", 
"When reading a very large file, Snd tries to keep an overview at hand of the channels so \
that you can move around quickly in very large data sets; when first read in, these overviews \
are set underway, and when they are finally ready for use, the line after the file name \
appears.  If you try to zoom out to a large view before the separator line appears, the graphics update process may be slow. ");
}

void click_for_amp_help(snd_state *ss)
{
  snd_help_with_wrap(ss, "Amp", 
"This scrollbar controls the amplitude at which the sound is played.  Click the \
amp label to return to 1.0. Control-Click returns to the previous value.");
}

void click_for_srate_arrow_help(snd_state *ss)
{
  snd_help_with_wrap(ss, "Srate Arrow",
"This button determines which direction the sound file is played.  When pointing \
to the right, the sound is played forwards;  to the left, backwards.");
}

void click_for_speed_help(snd_state *ss)
{
  snd_help_with_url_and_wrap(ss, "Srate", "#speed", 
"This scrollbar controls the sampling rate at which the sound is played.  The arrow \
controls the direction (forwards or backwards) of playback.  Label clicks behave as with amp.");
}

void click_for_minibuffer_help(snd_state *ss)
{
  snd_help_with_url_and_wrap(ss, "Minibuffer", "#panelayout",
"This is the 'minibuffer', to use Emacs jargon.  Although it looks inert and wasted,  \
there is in fact a text window lurking beneath that has access to the Lisp evaluator, not \
to mention much of the innards of the Snd program.");
}

void click_for_play_help(snd_state *ss)
{
  snd_help_with_url_and_wrap(ss, "Play", "#play",
"Snd can play any number of sounds at once or should be able to anyway.  A sort of \
clumsy realtime mixer, although it was not intended to fill that role.");
}

void click_for_expand_help(snd_state *ss)
{
  snd_help_with_url_and_wrap(ss, STR_Expand, "#expand",
"This scrollbar controls the tempo at which the sound is played back, using granular \
synthesis. The expand button must be down to get any expansion. Label clicks as in amp.");
}

void click_for_contrast_help(snd_state *ss)
{
  snd_help_with_url(ss, STR_Contrast, "#contrast",
"This scrollbar controls the amount of 'contrast enhancement' applied during \
playback.  The contrast button must be down to get any effect.  Label clicks as in amp.");
}

void click_for_reverb_scale_help(snd_state *ss)
{
  snd_help_with_url_and_wrap(ss, "Reverb amount", "#reverb",
"This scrollbar controls the amount of the sound that is fed into the reverberator. \
The reverb button must be down to get any reverb during playback.  Label clicks as in amp.");
}

void click_for_reverb_length_help(snd_state *ss)
{
  snd_help_with_url_and_wrap(ss, "Reverb length", "#reverb", 
"This scrollbar controls the lengths of the various delay lines in the reverb. \
It only takes effect when the reverb is created, that is, only when the play \
operation starts from silence.  Label clicks as in amp.");
}

void click_for_filter_help(snd_state *ss)
{
  snd_help_with_wrap(ss, "Filter", 
"The Snd filter is an FIR filter of arbitrary order.  You specify the filter you want by \
defining the frequency response as an envelope in the 'env' window; set the desired order in \
the 'order' window; then turn it on by pushing the filter button at the right.  The filter \
design algorithm uses frequency sampling. The higher the order, the closer the filter \
can approximate the envelope you draw. You can also specify the filter coefficients \
in a file of floats, then load them into the Snd filter by typing the file name in the \
filter envelope text window.");
}

void click_for_filter_order_help(snd_state *ss)
{
  snd_help_with_wrap(ss, "Filter Order", 
"The filter order determines how closely the filter approximates the frequency response curve you drew in the 'env' window. ");
}

void click_for_filter_envelope_help(snd_state *ss)
{
  snd_help_with_wrap(ss, "Filter Envelope", 
"The filter envelope is a line-segment description of the frequency response \
you want.  It consists of a sequence of x, y pairs; normally the x axis goes \
from 0 to .5 or 0 to 1.0.  For example, a low-pass filter envelope could be: \
0.0 1.0 .25 1.0 .5 0.0 1.0 0.0");
}

void click_for_sound_help(snd_state *ss)
{
  snd_help_with_wrap(ss,
		     "Minibuffer",
"This portion of the snd display has several parts: the sound file name, with an asterisk if \
the file has unsaved edits; a minibuffer for various expression evaluations; a sync button \
that causes operations on one channel to be applied to all channels; and a play button \
that causes the sound to be played.  The lower portion of the pane, normally hidden, \
contains a variety of sound manipulation controls that can be applied while it is playing.");
}

void click_for_save_as_help(snd_state *ss)
{
  snd_help_with_wrap(ss,
		     "Save As",
"You can save the current state of a file or region under a different file name using the Save \
As option.  The output header type, data format,  and sampling rate can also be set.  The data formats \
are big-endian where relevant except for 'wave' output.  If a file by the chosen name already exists \
it is silently overwritten, unless that file is already open in Snd and has edits.  In that case,  \
you'll be asked what to do.  If you want to be warned whenever a file is about to be overwritten by this \
option, set the resource overwriteCheck to 1. If you give the current file name to Save As,  \
any current edits will be saved and the current version in Snd will be updated (that is, in this \
case, the current edit tree is not preserved).");
}
#endif


/* -------- dialog help button -------- */

void help_dialog_help(snd_state *ss)
{
  snd_help_with_wrap(ss,
		     "Help",
"You can get help within Snd either from the Help Menu items, or by clicking on \
some portion of the Snd display while the cursor is '?'.  See Click for Help in the Help Menu.");
}

void transform_dialog_help(snd_state *ss)
{
  snd_help_with_wrap(ss,
		     "Transform Options",
"This dialog presents the various transform (fft) related choices. \
\n\n\
On the upper left is a list of available transform types; next on the right is a list of fft sizes;  \
next is a panel of buttons that sets various display-oriented choices; the lower left panel \
sets the current wavelet, when relevant; next is the fft data window choice; and next to it is a \
graph of the current fft window; when the window has an associated parameter (sometimes known as \
'alpha' or 'beta'), the slider beneath the window list is highlighted and can be used to choose the \
desired member of that family of windows. \
\n\n\
If the 'selection' button is not set, the FFT is taken from the start (the left edge) of the \
current window and is updated as the window bounds change; otherwise the FFT is taken over the extent \
of the current selection, if any is active in the current channel.  The fft data is scaled to fit \
between 0.0 and 1.0 unless the fft normalization is off. The full frequency axis is normally \
displayed, but the axis is 'dragable' -- put the mouse on the axis and drag it either way to change \
the range (this is equivalent to changing the variable " S_spectro_cutoff "). You can also click on \
any point in the fft to get the associated fft data displayed; if " S_verbose_cursor " is on, you can \
drag the mouse through the fft display and the description in the minibuffer will be constantly updated. \
\n\n\
The harmonic analysis function is normally the Fourier Transform, but others are available, \
including about 20 wavelet choices, the Hankel and Chebyshev transforms, and perhaps others. (The \
Hankel transform returns the Bessel function spectrum, the Chebyshev transform returns the \
Chebyshev polynomial spectrum). \
\n\n\
The top three buttons in the transform dialog choose between a normal fft, a sonogram, or a \
spectrogram. The 'peaks' button affects whether peak info is displayed alongside the graph of the \
spectrum. The 'dB' button selects between a linear and logarithmic Y (magnitude) axis. The 'log freq' \
button makes a similar choice along the frequency axis.");	   
}

void color_dialog_help(snd_state *ss)
{
  snd_help_with_wrap(ss,
		     "View Color",
"This dialog sets the colormap and associated variables used during sonogram, spectrogram,  \
and perhaps wavogram display. The cutoff scale refers to the minimum data value to be displayed.");	   
}

void orientation_dialog_help(snd_state *ss)
{
  snd_help_with_wrap(ss,
		     "View Orientation",
"This dialog sets the rotation and scaling variables used during sonogram, spectrogram, and wavogram display.");	   
}

void region_dialog_help(snd_state *ss)
{
  snd_help_with_url_and_wrap(ss, STR_Region_Browser, "#regionbrowser",
"This is the 'region browser'.  The scrolled window contains the list of current regions \
with a brief title to indicate the provenance thereof, and two buttons.  The 'save' button \
protects or unprotects the region from deletion. The 'play' button plays the associated region. \
One channel of the currently selected region is displayed in the graph window.  The up and \
down arrows move up or down in the region's list of channels.  If you click a region's \
title, the text is highlighted, and that region is displayed in the graph area.  You can delete the \
region by clicking the 'Delete' button.  To dismiss the browser, click 'Ok'.  The 'edit' button \
loads the region into the main editor as a temporary file.  It can be edited or renamed, etc.  If you save \
the file, the region is updated to reflect any edits you made.");
}

void raw_data_dialog_help(snd_state *ss)
{
  snd_help_with_wrap(ss,
		     STR_Raw_Data,
"To display and edit sound data, Snd needs to know how the data's sampling rate, number \
of channels, and numerical format.  This dialog gives you a chance to set those fields. \
To make the current settings the default for any future headerless files, click the \
'Default' button.");
}

void new_file_dialog_help(snd_state *ss)
{
  snd_help_with_wrap(ss,
		     "New File",
"This dialog sets the new file's output header type, data format, srate, chans, and comment if any.");
}

void edit_header_dialog_help(snd_state *ss)
{
  snd_help_with_wrap(ss,
		     STR_Edit_Header,
"This dialog edits the header of a sound file. No change is made to the actual sound data; the \
new header is blindly written, any unsaved edits are ignored. If you specify 'raw' as the type, \
any existing header is removed.  This dialog is aimed at adding or removing an entire header,  \
or editing the header comments; anything else is obviously dangerous.");
}

void print_dialog_help(snd_state *ss)
{
  snd_help_with_wrap(ss,
		     "File Print",
"Print causes the currently active display to be either printed (via the lpr command) or saved as \
an eps file.  In the latter case, the file name is set either by the dialog, or taken from the \
resource epsFile (normally snd.eps).");
}

void view_files_dialog_help(snd_state *ss)
{
  snd_help_with_wrap(ss,
		     "File Browser",
"This dialog provides two lists, one of the currently active files in Snd, and \
the other of previously active files. The save button saves current edits, the \
play button plays the file, and the unlist button removes a file from the \
previous files list.  If a file is deleted while Snd is running, it will not notice \
its deletion automatically.  To update the previous files list to account for such \
actions, click on the 'update' button. To reopen a previous file, simply select \
that file from the previous files list. To remove all files from the previous files \
list, click the 'clear' button.  To select one of the current files in Snd, opening \
its associated windows, select it in the current files list. \
\n\n\
To preload all the sound files in a directory into the previous files list, use either the \
command (" S_preload_directory " dir), as in \
\n\n\
  M-x (" S_preload_directory " \"/usr/people/bil/hdr)\"\n\
\n\
or give the directory name to the -p flag when starting Snd: \
\n\n\
  snd -p . oboe.snd\n\
\n\
To preload a specific file, \n\
\n\
  (" S_preload_file " <name>)\n\
\n\
The 'sort' label on the right activates a menu of sorting choices; 'name' sorts the \
previous files list alphabetically, 'date' sorts by date written, 'size' sorts by the \
number of samples in the sound, and 'entry' sorts by the order the sound appears in the \
absence of explicit sorting.  The variable " S_previous_files_sort " (default 0: \
unsorted) refers to this menu.");	   
}

void stats_dialog_help(snd_state *ss)
{
  snd_help_with_wrap(ss,
		     STR_Disk_and_Memory_Usage,
"This window gives an approximate notion of how much memory (RAM) and disk space each channel is \
taking up.  As a channel is edited, the relevant data is saved either in arrays or temporary files. \
The number of bytes in these arrays, and the number of such arrays are the first two numbers; then \
comes the space in bytes the channel takes up in the main (presumably permanent) file; the next \
three numbers give the number of bytes in the temporary files, the number of such files, and \
the number of these files that are currently being held open.  The related variable is \
" S_show_usage_stats ".  The 'Update' button forces the stats to be regathered, in case the display \
somehow gets out of sync with the actual data.");
}

#ifndef _MSC_VER
static char CLM_help_string[] = 
"  all-pass            (gen input pm)       all-pass filter\n\
  all-pass?           (gen)                #t if gen is all-pass filter\n\
  amplitude-modulate  (carrier in1 in2)    amplitude modulation\n\
  array-interp        (arr x)              interpolated array lookup\n\
  array->file         (filename vct len srate channels)\n\
  asymmetric-fm       (gen index fm)       asymmetric-fm generator\n\
  asymmetric-fm?      (gen)                #t if gen is asymmetric-fm generator\n\
  buffer->frame       (gen frame           buffer generator returning frame\n\
  buffer->sample      (gen)                buffer generator returning sample\n\
  buffer-empty?       (gen)                #t if buffer has no data\n\
  buffer?             (gen)                #t if gen is buffer generator\n\
  clear-array         (arr)                set all elements of arr to 0.0\n\
  comb                (gen input pm)       comb filter\n\
  comb?               (gen)                #t if gen is comb filter\n\
  contrast-enhancement(input (index 1.0))  a kind of phase modulation\n\
  convolution         (sig1 sig2 n)        convolve sig1 with sig2 (size n), returning new sig1\n\
  convolve            (gen input-function) convolve generator\n\
  convolve?           (gen)                #t if gen is convolve generator\n\
  convolve-files      (f1 f2 maxamp outf)  convolve f1 with f2, normalize to maxamp, write outf\n\
  db->linear          (db)                 translate dB value to linear\n\
  degrees->radians    (deg)                translate degrees to radians\n\
  delay               (gen input pm)       delay line\n\
  delay?              (gen)                #t if gen is delay line\n\
  dot-product         (sig1 sig2)          return dot-product of sig1 with sig2\n\
  env                 (gen)                envelope generator\n\
  env-interp          (x env (base 1.0))   return value of env at x\n\
  env?                (gen)                #t if gen is env (from make-env)\n\
  mus-fft             (rl im n sign)       fft of rl and im (sign = -1 for ifft), result in rl\n\
  file->array         (filename chan start len vct)\n\
  file->frame         (gen loc frame)      return frame from file at loc\n\
  file->frame?        (gen)                #t if gen is file->frame generator\n\
  file->sample        (gen loc chan)       return sample from file at loc\n\
  file->sample?       (gen)                #t if gen is file->sample generator\n\
  filter              (gen input)          filter\n\
  filter?             (gen)                #t if gen is filter\n\
  fir-filter          (gen input)          FIR filter\n\
  fir-filter?         (gen)                #t if gen is fir filter\n\
  formant             (gen input)          formant generator\n\
  formant-bank        (scls gens inval)    bank for formants\n\
  formant?            (gen)                #t if gen is formant generator\n\
  frame*              (fr1 fr2 outfr)      element-wise multiply\n\
  frame+              (fr1 fr2 outfr)      element-wise add\n\
  frame->buffer       (buf frame)          add frame to buffer\n\
  frame->file         (gen loc frame)      write (add) frame to file at loc\n\
  frame->file?        (gen)                #t if gen is frame->file generator\n\
  frame->frame        (mixer frame outfr)  pass frame through mixer\n\
  frame->list         (frame)              return list of  frame's contents\n\
  frame-ref           (frame chan)         return frame[chan]\n\
  frame->sample       (frmix frame)        pass frame through frame or mixer to produce sample\n\
  frame-set!          (frame chan val)     frame[chan] = val\n\
  frame?              (gen)                #t if gen is frame object\n\
  granulate           (gen input-function) granular synthesis generator\n\
  granulate?          (gen)                #t if gen is granulate generator\n\
  hz->radians         (freq)               translate freq to radians/sample\n\
  iir-filter          (gen input)          IIR filter\n\
  iir-filter?         (gen)                #t if gen is iir-filter\n\
  in-any              (loc chan stream)    return sample in stream at loc and chan\n\
  in-hz               (freq)               translate freq to radians/sample\n\
  ina                 (loc stream)         return sample in stream at loc, chan 0\n\
  inb                 (loc stream)         return sample in stream at loc, chan 1\n\
  linear->db          (val)                translate linear val to dB\n\
  locsig              (gen loc input)      place input in output channels at loc\n\
  locsig-ref          (gen chan)           locsig-scaler[chan]\n\
  locsig-reverb-ref   (gen chan)           locsig-reverb-scaler[chan]\n\
  locsig-set!         (gen chan val)       locsig-scaler[chan] = val\n\
  locsig-reverb-set!  (gen chan val)       locsig-reverb-scaler[chan] = val\n\
  locsig?             (gen)                #t if gen is locsig generator\n\
  make-all-pass       (feedback feedforward size max-size initial-contents initial-element)\n\
  make-asymmetric-fm  (frequency initial-phase r ratio)\n\
  make-buffer         (size fill-time)\n\
  make-comb           (scaler size max-size initial-contents initial-element)\n\
  make-convolve       (input filter fft-size)\n\
  make-delay          (size initial-contents initial-element max-size)\n\
  make-env            (envelope scaler duration offset base end start)\n\
  make-fft-window     (type size)\n\
  make-file->frame    (name)\n\
  make-file->sample   (name)\n\
  make-filter         (order xcoeffs ycoeffs)\n\
  make-fir-filter     (order xcoeffs)\n\
  make-formant        (radius frequency gain)\n\
  make-frame          (chans &rest vals)\n\
  make-frame->file    (name chans format type)\n\
  make-granulate      (input expansion length scaler hop ramp jitter max-size)\n\
  make-iir-filter     (order ycoeffs)\n\
  make-locsig         (degree distance reverb output revout channels)\n\
  make-mixer          (chans &rest vals)\n\
  make-notch          (scaler size max-size initial-contents initial-element)\n\
  make-one-pole       (a0 b1)\n\
  make-one-zero       (a0 a1)\n\
  make-oscil          (frequency initial-phase)\n\
  make-phase-vocoder  (input fftsize overlap interp pitch analyze edit synthesize)\n\
  make-ppolar         (radius frequency)\n\
  make-pulse-train    (frequency amplitude initial-phase)\n\
  make-rand           (frequency amplitude)\n\
  make-rand-interp    (frequency amplitude)\n\
  make-readin         (file channel start)\n\
  make-sample->file   (name chans format type)\n\
  make-sawtooth-wave  (frequency amplitude initial-phase)\n\
  make-sine-summation (frequency initial-phase n a ratio)\n\
  make-square-wave    (frequency amplitude initial-phase)\n\
  make-src            (input srate width)\n\
  make-sum-of-cosines (frequency initial-phase cosines)\n\
  make-table-lookup   (frequency initial-phase wave)\n\
  make-triangle-wave  (frequency amplitude initial-phase)\n\
  make-two-pole       (a0 b1 b2)\n\
  make-two-zero       (a0 a1 a2)\n\
  make-wave-train     (frequency initial-phase wave)\n\
  make-waveshape      (frequency partials)\n\
  make-zpolar         (radius frequency)\n\
  mixer*              (mix1 mix2 outmx)    matrix multiply of mix1 and mix2\n\
  mixer-ref           (mix in out)         mix-scaler[in, out]\n\
  mixer-set!          (mix in out val)     mix-scaler[in, out] = val\n\
  mixer?              (gen)                #t if gen is mixer object\n\
  multiply-arrays     (arr1 arr2)          arr1[i] *= arr2[i]\n\
  ;; the \"mus-\" functions are generic functions, to set use mus-set-var as in mus-set-frequency\n\
  mus-a0              (gen)                a0 field (simple filters)\n\
  mus-a1              (gen)                a1 field (simple filters)\n\
  mus-a2              (gen)                a2 field (simple filters)\n\
  mus-array-print-length ()                how many array elements to print in mus_describe\n\
  mus-b1              (gen)                b1 field (simple filters)\n\
  mus-b2              (gen)                b2 field (simple filters)\n\
  mus-bank            (gens amps &optional args1 args2)\n\
  mus-channel         (gen)                channel of gen\n\
  mus-channels        (gen)                channels of gen\n\
  mus-cosines         (gen)                cosines of sum-of-cosines gen\n\
  mus-data            (gen)                data array of gen\n\
  mus-feedback        (gen)                feedback term of gen (simple filters)\n\
  mus-feedforward     (gen)                feedforward term of gen (all-pass)\n\
  mus-formant-radius  (gen)                formant radius\n\
  mus-frequency       (gen)                frequency of gen (Hz)\n\
  mus-hop             (gen)                hop amount of gen (granulate)\n\
  mus-increment       (gen)                increment of gen (src, readin, granulate)\n\
  mus-input?          (gen)                #t if gen is input source\n\
  mus-length          (gen)                length of gen\n\
  mus-location        (gen)                location (read point) of gen\n\
  mus-mix             (outfile infile outloc frames inloc mixer envs)\n\
  mus-order           (gen)                order of gen (filters)\n\
  mus-output?         (gen)                #t if gen is output generator\n\
  mus-phase           (gen)                phase of gen (radians)\n\
  mus-ramp            (gen)                ramp time of gen (granulate)\n\
  mus-random          (val)                random numbers bewteen -val and val\n\
  mus-run             (gen arg1 arg2)      apply gen to args\n\
  mus-scaler          (gen)                scaler of gen\n\
  mus-set-rand-seed   (val)                set random number generator seed to val\n\
  mus-set-srate       (val)                also (set! (mus-srate) val)\n\
  mus-srate           ()                   current sampling rate\n\
  mus-xcoeffs         (gen)                feedforward (FIR) coeffs of filter\n\
  mus-ycoeffs         (gen)                feedback (IIR) coeefs of filter\n\
  notch               (gen input pm)       notch filter\n\
  notch?              (gen)                #t if gen is notch filter\n\
  one-pole            (gen input)          one-pole filter\n\
  one-pole?           (gen)                #t if gen is one-pole filter\n\
  one-zero            (gen input)          one-zero filter\n\
  one-zero?           (gen)                #t if gen is one-zero filter\n\
  oscil               (gen fm pm)          sine wave generator\n\
  oscil-bank          (scls gens invals)   bank for oscils\n\
  oscil?              (gen)                #t if gen is oscil generator\n\
  out-any             (loc samp chan stream) write (add) samp to stream at loc in channel chan\n\
  outa                (loc samp stream)    write (add) samp to stream at loc in chan 0\n\
  outb                (loc samp stream)    write (add) samp to stream at loc in chan 1\n\
  outc                (loc samp stream)    write (add) samp to stream at loc in chan 2\n\
  outd                (loc samp stream)    write (add) samp to stream at loc in chan 3\n\
  partials->polynomial(partials kind)      create waveshaping polynomial from partials\n\
  partials->wave      (synth-data table norm) load table from synth-data\n\
  partials->waveshape (partials norm size) create waveshaping table from partials\n\
  phase-partials->wave(synth-data table norm) load table from synth-data\n\
  phase-vocoder       (gen input)          phase vocoder generator\n\
  phase-vocoder?      (gen)                #t if gen is a phase-vocoder generator\n\
  polynomial          (coeffs x)           evaluate polynomial at x\n\
  pulse-train         (gen fm)             pulse-train generator\n\
  pulse-train?        (gen)                #t if gen is pulse-train generator\n\
  radians->degrees    (rads)               convert radians to degrees\n\
  radians->hz         (rads)               convert radians/sample to Hz\n\
  rand                (gen fm)             random number generator\n\
  rand-interp         (gen fm)             interpolating random number generator\n\
  rand-interp?        (gen)                #t if gen is interpolating random number generator\n\
  rand?               (gen)                #t if gen is random number generator\n\
  readin              (gen)                read one value from associated input stream\n\
  readin?             (gen)                #t if gen is readin generator\n\
  rectangular->polar  (rl im)              translate from rectangular to polar coordinates\n\
  restart-env         (env)                return to start of env\n\
  ring-modulate       (sig1 sig2)          sig1 * sig2 (element-wise)\n\
  sample->buffer      (buf samp)           store samp in buffer\n\
  sample->file        (gen loc chan val)   store val in file at loc in channel chan\n\
  sample->file?       (gen)                #t if gen is sample->file generator\n\
  sample->frame       (frmix samp outfr)   convert samp to frame\n\
  sawtooth-wave       (gen fm)             sawtooth-wave generator\n\
  sawtooth-wave?      (gen)                #t if gen is sawtooth-wave generator\n\
  sine-summation      (gen fm)             sine-summation generator\n\
  sine-summation?     (gen)                #t if gen is sine-summation generator\n\
  spectrum            (rl im win type)     produce spectrum of data in rl\n\
  square-wave         (gen fm)             square-wave generator\n\
  square-wave?        (gen)                #t if gen is square-wave generator\n\
  src                 (gen fm input-function) sample rate converter\n\
  src?                (gen)                #t if gen is sample-rate converter\n\
  sum-of-cosines      (gen fm)             sum-of-cosines (pulse-train) generator\n\
  sum-of-cosines?     (gen)                #t if gen is sum-of-cosines generator\n\
  sum-of-sines        (amps phases)        additive synthesis\n\
  table-lookup        (gen fm)             table-lookup generator\n\
  table-lookup?       (gen)                #t if gen is table-lookup generator\n\
  tap                 (gen pm)             delay line tap\n\
  triangle-wave       (gen fm)             triangle-wave generator\n\
  triangle-wave?      (gen)                #t if gen is triangle-wave generator\n\
  two-pole            (gen input)          two-pole filter\n\
  two-pole?           (gen)                #t if gen is two-pole filter\n\
  two-zero            (gen input)          two-zero filter\n\
  two-zero?           (gen)                #t if gen is two-zero filter\n\
  wave-train          (gen fm)             wave-train generator\n\
  wave-train?         (gen)                #t if gen is wave-train generator\n\
  waveshape           (gen index fm)       waveshaping generator\n\
  waveshape?          (gen)                #t if gen is waveshape generator\n\
";
#else
static char CLM_help_string[] = "";
#endif

static char *CLM_help(void) {return(CLM_help_string);}
void clm_help(snd_state *ss) {snd_help_with_url(ss, STR_CLM, "grfsnd.html#sndwithclm", CLM_help());}


#define GLYPH_WIDTH 11

char* word_wrap(char *text, int widget_len)
{
  char *new_text;
  int new_len, old_len, i, j, line_len = 0, desired_len;
  old_len = snd_strlen(text);
  new_len = old_len + 32;
  desired_len = (int)(widget_len / GLYPH_WIDTH);
  if (desired_len <= 8)
    return(copy_string(text));
  new_text = (char *)CALLOC(new_len, sizeof(char));
  for (i = 0, j = 0; i < old_len; i++)
    if ((line_len >= desired_len) &&
	((text[i] == '\n') || (text[i] == ' ')))
      {
	new_text[j++] = '\n';
	line_len = 0;
      }
    else
      {
	new_text[j++] = text[i];
	if (text[i] == '\n')
	  line_len = 0;
	line_len++;
      }
  return(new_text);
}

XEN g_help(XEN text, int widget_wid)
{
  #define H_snd_help "(" S_snd_help " arg) returns the documentation associated with its argument. (snd-help make-vct) \
for example, prints out a brief description of make-vct. \
The argument can be a string, a symbol, or the object itself.  In some cases, only the symbol has the documentation. \
In the help descriptions, '&optional' marks optional arguments, and \
'&opt-key' marks CLM-style optional keyword arguments.  If you load index.scm \
the functions html and ? can be used in place of help to go to the HTML description, \
and the location of the associated C code will be displayed, if it can be found."

  XEN help_text = XEN_FALSE; 
  char *str = NULL;
#if HAVE_GUILE
  XEN value; XEN loc_val = XEN_UNDEFINED; XEN snd_urls; 
  char *estr;
  if (XEN_EQ_P(text, XEN_UNDEFINED))                              /* if no arg, describe snd-help */
    help_text = C_TO_XEN_STRING(H_snd_help);
  else
    {
      if ((XEN_STRING_P(text)) || (XEN_SYMBOL_P(text)))            /* arg can be name (string), symbol, or the value */
	{
	  if (XEN_STRING_P(text))
	    str = XEN_TO_C_STRING(text);
	  else str = XEN_SYMBOL_TO_C_STRING(text);
	  value = XEN_NAME_AS_C_STRING_TO_VALUE(str);
	}
      else value = text;
      help_text = scm_object_property(value, XEN_DOCUMENTATION_SYMBOL);         /* (object-property ...) */
      if ((XEN_FALSE_P(help_text)) &&
	  (XEN_PROCEDURE_P(value)))
	{
	  help_text = scm_procedure_property(value, XEN_DOCUMENTATION_SYMBOL);  /* (procedure-property ...) */
	  if (XEN_FALSE_P(help_text))
	    help_text = scm_procedure_documentation(value);      /* (procedure-documentation ...) -- this is the first line of source if string */
	}
      if ((XEN_FALSE_P(help_text)) &&
	  (str))
	help_text = scm_object_property(C_STRING_TO_XEN_SYMBOL(str), XEN_DOCUMENTATION_SYMBOL);
      if (XEN_STRING_P(help_text))
        {
	  /* look for C code location (if index.scm has been loaded) */
	  snd_urls = XEN_NAME_AS_C_STRING_TO_VALUE("snd-names-and-urls");
	  if (XEN_LIST_P(snd_urls)) 
	    {
	      estr = (char *)CALLOC(128, sizeof(char));
	      sprintf(estr, "(c? \"%s\")", (str) ? str : (XEN_TO_C_STRING(XEN_TO_STRING(value))));
	      loc_val = XEN_EVAL_C_STRING(estr);
	      FREE(estr);
	      if ((XEN_LIST_P(loc_val)) &&
		  (XEN_LIST_LENGTH(loc_val) == 3))
		{ 
		  str = word_wrap(XEN_TO_C_STRING(help_text), widget_wid);
		  estr = (char *)CALLOC(snd_strlen(str) + 128, sizeof(char));
		  sprintf(estr, "%s (%s[%d]:%s)", 
			  str, 
			  XEN_TO_C_STRING(XEN_CAR(loc_val)),
			  XEN_TO_C_INT(XEN_CADR(loc_val)),
			  XEN_TO_C_STRING(XEN_CADDR(loc_val)));
		  help_text = C_TO_XEN_STRING(estr);
		  if (str) FREE(str);
		  if (estr) FREE(estr);
		  return(help_text);
		}
	    }
	}
    }
  
  /* help strings are always processed through the word-wrapper to fit whichever widget they are posted to */
  /*   this means all the H_doc strings in Snd need to omit line-feeds except where necessary (i.e. code) */

  if (XEN_STRING_P(help_text))
    {
      str = word_wrap(XEN_TO_C_STRING(help_text), widget_wid);
      help_text = C_TO_XEN_STRING(str);
      if (str) FREE(str);
    }
#endif
#if HAVE_RUBY
  if (XEN_STRING_P(text))
     str = xen_help(XEN_TO_C_STRING(text));
  else 
    if (XEN_SYMBOL_P(text))
       str = xen_help(XEN_SYMBOL_TO_C_STRING(text));
    else str = H_snd_help;
  if (str)
    {
      str = word_wrap(str, widget_wid);
      help_text = C_TO_XEN_STRING(str);
      if (str) FREE(str);
    }
#endif
  return(help_text);
}

static XEN g_listener_help(XEN arg)
{
  return(g_help(arg, listener_width()));
}

void set_html_dir(snd_state *ss, char *new_dir)
{
#if HAVE_HTML
  if (html_dir(ss)) FREE(html_dir(ss));
  set_html_dir_1(ss, new_dir);
#endif
}

#ifdef XEN_ARGIFY_1
XEN_ARGIFY_1(g_listener_help_w, g_listener_help)
#else
#define g_listener_help_w g_listener_help
#endif

void g_init_help(void)
{
  XEN_DEFINE_PROCEDURE(S_snd_help, g_listener_help_w, 0, 1, 0, H_snd_help);
}
