#include "snd.h"

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
#if (HAVE_GSL_GSL_VERSION_H) && (!(defined(GSL_VERSION)))
  #include <gsl/gsl_version.h>
#endif

#if HAVE_LADSPA
  #include <ladspa.h>
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

static char *xm_version(void)
{
  char *version = NULL;
  XEN xm_val = XEN_FALSE;
#if HAVE_GUILE
  xm_val = XEN_EVAL_C_STRING("(and (or (provided? 'xm) (provided? 'xg)) xm-version)");
#else
  #if HAVE_RUBY
  if (rb_const_defined(rb_cObject, rb_intern("Xm_Version")))
    xm_val = XEN_EVAL_C_STRING("Xm_Version");
  #endif
#endif
  if (XEN_STRING_P(xm_val))
    {
      version = (char *)CALLOC(32, sizeof(char));
      mus_snprintf(version, 32, "\n    %s: %s", 
#if USE_MOTIF
		   "xm",
#else
		   "xg",
#endif
		   XEN_TO_C_STRING(xm_val));
      return(version);
    }
  return("");
}

#if HAVE_GL
static char *gl_version(void)
{
  char *version = NULL;
  XEN gl_val = XEN_FALSE;
#if HAVE_GUILE
  gl_val = XEN_EVAL_C_STRING("(and (provided? 'gl) gl-version)");
#else
  #if HAVE_RUBY
  if (rb_const_defined(rb_cObject, rb_intern("Gl_Version")))
    gl_val = XEN_EVAL_C_STRING("Gl_Version");
  #endif
#endif
  if (XEN_STRING_P(gl_val))
    {
      version = (char *)CALLOC(32, sizeof(char));
      mus_snprintf(version, 32, "\n    gl: %s", XEN_TO_C_STRING(gl_val));
      return(version);
    }
  return("");
}

static char *glx_version(void)
{
  snd_state *ss;
  int major = 0, minor = 0;
  char *version;
  ss = get_global_state();
  if (ss == NULL) return(""); /* snd --help for example */
  version = (char *)CALLOC(128, sizeof(char));
#if USE_MOTIF
  if ((ss->sgx) && (ss->sgx->cx))
    {
      glXMakeCurrent(MAIN_DISPLAY(ss), XtWindow(ss->sgx->mainshell), ss->sgx->cx);
      mus_snprintf(version, 128, " %s", glGetString(GL_VERSION));
    }
  else 
    {
      glXQueryVersion(MAIN_DISPLAY(ss), &major, &minor);
      mus_snprintf(version, 128, " %d.%d", major, minor);
    }
#else
  if (gdk_gl_query_extension() != 0)
    {
      gdk_gl_query_version (&major, &minor);
      mus_snprintf(version, 128, " %d.%d", major, minor);
    }
  else mus_snprintf(version, 128, "gtkGL not supported?");
#endif
  return(version);
}
#endif

#if HAVE_FFTW3
  #include <fftw3.h>
#else
  #if HAVE_FFTW
    #include <fftw.h>
  #endif
#endif

char *version_info(void)
{
  char *buf;
  buf = (char *)CALLOC(1024, sizeof(char));
  vstrcat(buf,
	  _("This is Snd version "),
	  SND_RPM_VERSION,
	  " of ",
	  SND_VERSION,
	  ":\n    ", xen_version(),
	  "\n    ", mus_audio_moniker(),
	  "\n    Sndlib ", snd_itoa(SNDLIB_VERSION), ".", 
                           snd_itoa(SNDLIB_REVISION), 
                           " (", SNDLIB_DATE,
#if SNDLIB_USE_FLOATS
	  _(", float samples"),
#else
	  ", int", snd_itoa(MUS_SAMPLE_BITS), _(" samples"),
#endif
#if WITH_MODULES
	  _(", with modules"),
#endif
	  ")", 
	  sndlib_consistency_check(),
	  "\n    CLM ", snd_itoa(MUS_VERSION), ".", 
	                snd_itoa(MUS_REVISION), " (", 
                        MUS_DATE, ")",
#if HAVE_GSL
	  "\n    GSL",
  #ifdef GSL_VERSION
          " ", GSL_VERSION,
  #endif
#endif
#if HAVE_FFTW || HAVE_FFTW3
	  "\n    ", fftw_version,
#endif
#if USE_MOTIF
  #ifdef LESSTIF_VERSION
	  "\n    Lesstif ", snd_itoa(LESSTIF_VERSION), ".", 
                            snd_itoa(LESSTIF_REVISION), " ",
  #endif
	  "\n    Motif ", snd_itoa(XmVERSION), ".", 
                          snd_itoa(XmREVISION), ".", 
                          snd_itoa(XmUPDATE_LEVEL),
	  " X", snd_itoa(X_PROTOCOL), "R", 
                snd_itoa(XT_REVISION),
#endif
#if USE_GTK
	  "\n    Gtk+ ", snd_itoa(GTK_MAJOR_VERSION), ".", 
                         snd_itoa(GTK_MINOR_VERSION), ".", 
                         snd_itoa(GTK_MICRO_VERSION),
	  ", Glib ",     snd_itoa(GLIB_MAJOR_VERSION), ".", 
                         snd_itoa(GLIB_MINOR_VERSION), ".", 
                         snd_itoa(GLIB_MICRO_VERSION),
#endif
	  xm_version(),
#if WITH_GTK_AND_X11
	  " (with x11)",
#endif
#if HAVE_GL
	  gl_version(),
	  "\n    OpenGL", glx_version(),
  #if USE_GTK
	  ", gtkglext ",
    #ifdef GTKGLEXT_MAJOR_VERSION
	  snd_itoa(GTKGLEXT_MAJOR_VERSION), ".",
	  snd_itoa(GTKGLEXT_MINOR_VERSION), ".",
	  snd_itoa(GTKGLEXT_MICRO_VERSION),
    #else
      #if HAVE_GTK_GL_EXT_0_1
	  "0.1",
      #else
        #if HAVE_GDK_GL_FONT_USE_GDK_FONT
	  "0.2",
        #else
          #if HAVE_GDK_GL_CONTEXT_COPY
	  "0.4",
          #else
	  "0.3",
          #endif
        #endif
      #endif
    #endif
  #endif
#endif
#if (!USE_MOTIF) && (!USE_GTK)
	  _("\n    without any graphics system"),
#endif
#if HAVE_XPM && USE_MOTIF
	  "\n    Xpm ", snd_itoa(XpmFormat), ".", 
                        snd_itoa(XpmVersion), ".", 
                        snd_itoa(XpmRevision),
#endif
#if HAVE_LADSPA
	  "\n    LADSPA",
  #ifdef LADSPA_HINT_DEFAULT_MASK
	  " 1.1",
 #else
	  " 1.0",
  #endif
#endif
#if SND_AS_WIDGET
	  _("\n    compiled as a widget"),
#endif
#if (defined(SIZEOF_OFF_T) && (SIZEOF_OFF_T > 4)) || (defined(_FILE_OFFSET_BITS) && (_FILE_OFFSET_BITS == 64))
	  _("\n    with large file support"),
#endif
#if ENABLE_NLS && HAVE_GETTEXT
	  _("\n    with gettext: "),
	  setlocale(LC_ALL, NULL),
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
  return(buf);
}

void news_help(snd_state *ss)
{
  char *info = NULL, *features = NULL;
  info = version_info();
#if HAVE_GUILE
  features = word_wrap(XEN_AS_STRING(XEN_EVAL_C_STRING("*features*")), 600);
#endif
#if HAVE_RUBY
  features = word_wrap(XEN_AS_STRING(XEN_EVAL_C_STRING("$\".join(' ')")), 600);
#endif
  ssnd_help(ss, "News",
	    info,
	    "\nRecent changes include:\n\
\n\
23-Jun:  mix-amp-env-changed-hook.\n\
         changed mix-amp-changed-hook to take 2nd chan arg.\n\
18-Jun:  mix-sound moved to mix.scm.\n\
16-Jun:  mark-drag-triangle-hook.\n\
         renamed mix-position-changed-hook to mix-dragged-hook.\n\
         mix.scm name changes (see snd6.scm for old forms).\n\
         with-mix (ws.scm).\n\
12-Jun:  bess1.scm and bess1.rb from Michael Scholz.\n\
10-Jun:  added initial-length arg to new-sound.\n\
2-June:  debug.scm and with-sound debugger in ws.scm.\n\
28-May:  snd 6.9.\n\
26-May:  x-axis-label.\n\
20-May:  make-variable-display in snd-motif.scm.\n\
16-May:  definstrument changes, added clm-ins.scm.\n\
14-May:  optional begin-time arg to make-mix-sample-reader.\n\
12-May:  removed --with-html and associated variables. added html-program.\n\
",
#if HAVE_GUILE
	    "\n    *features*: \n'", features, "\n\n",
#else
  #if HAVE_RUBY	    
	    "\n    $LOADED_FEATURES: \n", features, "\n\n",
  #else
	    "\n",
  #endif
#endif
NULL);
  if (info) FREE(info);
  if (features) FREE(features);
}


/* -------- basic helpers -------- */

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
  snd_help(ss, subject, newstr, FALSE);
  FREE(newstr);
}  

/* ---------------- help menu strings ---------------- */

static char file_menu_help_string[] =
"  Open: open a file.\n\
  Close: close the current file.\n\
  Save: save edits in the current file.\n\
  Save as: save edits in the current file in some new file.\n\
  Revert: undo all edits in the current file.\n\
  Mix: mix in a file.\n\
  Update: re-read current sound from disk.\n\
  New: create a new empty sound file.\n\
  Record: start the recorder\n\
  View: open a file in read-only mode.\n\
  Print: produce a Postscript file of the current graph.\n\
  Exit: leave Snd, without saving any edits.\n\
";

static char edit_menu_help_string[] =
"  Undo: undo the last edit in the current file.\n\
  Redo: redo a previously undone edit.\n\
  Find: a global search -- operates across all currently sync'd sounds.\n\
  Delete selection: delete selected portion.\n\
  Insert selection: insert the selection at the cursor.\n\
  Mix selection: add the selection at the cursor.\n\
  Play selection: play the selection.\n\
  Save selection: save the selection in a file.\n\
  Select all: select entire sound.\n\
  Edit Envelope: start the envelope editor dialog.\n\
  Edit Header: view or edit file's header.\n\
";

static char view_menu_help_string[] =
"  Show Controls: display the control panel.\n\
  Show Listener: open the lisp listener.\n\
  Mix Panel: open the mix dialog.\n\
  Regions: start the region browser.\n\
  Files: start the previous file browser.\n\
  Color: start the color browser.\n\
  Orientation: start the graph orientation dialog.\n\
  Graph style: display data as dots, lines or, filled polygons.\n\
  Verbose cursor: show cursor location and sample value.\n\
  Channel style: combine all channels into one graph.\n\
  Equalize panes: give panes equal sizes.\n\
  Show Y = 0: display the y = 0 line.\n\
  X axis units: x axis labels in seconds, samples, etc.\n\
  Error history: show errors encountered so far.\n\
";

static char options_menu_help_string[] = 
"   Transform Options: start dialog to set various fft-related variables\n\
   Speed style: control panel speed scrollbar interpretation.\n\
   Zoom focus: where to focus during zooms.\n\
   Save Options: save the current Options and View menu settings.\n\
   Save state: save current state of Snd.\n\
\n\
";

static char help_menu_help_string[] =   
"  Overview: this text.\n\
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
middle button pastes in the selection; \n\
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
  C-<: move cursor to sample 0\n\
  C->: move cursor to last sample\n\
  C-a: move cursor to window start\n\
  C-b: move cursor back one sample\n\
  C-d: delete sample at cursor\n\
  C-e: move cursor to window end\n\
  C-f: move cursor ahead one sample\n\
  C-g: abort current command\n\
  C-h: delete previous sample\n\
  C-i: display cursor info\n\
  C-j: goto mark\n\
  C-k: delete one line's worth of samples\n\
  C-l: position window so cursor is in the middle\n\
  C-m: place (or remove) mark at cursor location\n\
  C-n: move cursor ahead one 'line'\n\
  C-o: insert one zero sample at cursor\n\
  C-p: move cursor back one 'line'\n\
  C-q: play current channel starting at cursor\n\
  C-r: repeat last search backwards\n\
  C-s: search until a function returns true\n\
       The function should take one argument.\n\
       the current sample value.  For example.\n\
       to search for a sample greater than .1,\n\
       (lambda (val) (> val .1))\n\
  C-t: stop playing\n\
  C-u: start count definition.  If followed by a\n\
       float, the actual count is that number multiplied\n\
       by the current sampling rate.  If the optional\n\
       number is followed by C-m, the count returned\n\
       is the distance from the cursor to the n-th\n\
       successive mark.  That is, C-u C-m C-f is the\n\
       same as C-j.\n\
  C-v: move cursor to mid-window\n\
  C-w: delete current region\n\
  C-x: start extended command (see below)\n\
  C-y: paste in last deleted region. Use C-u\n\
       to paste in earlier regions.\n\
  C-z: set sample at cursor to 0.0\n\
  C-_: undo\n\
  C-[Space]: start selection definition\n\
       - C-[Space] to deactivate selection\n\
  C-m-g: clear listener\n\
\n\
The extended commands (preceded by C-x) are:\n\
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
  v: position window over selection\n\
  w: save selection as file\n\
  z: smooth selection\n\
  /: place named mark\n\
  (: begin keyboard macro definition\n\
  ): end keyboard macro definition\n\
\n\
  C-a: apply envelope.  If a count is specified,\n\
     the envelope is applied from the cursor for\n\
     that number of samples.  Otherwise, the\n\
     envelope is applied to the entire file, and\n\
     if syncing is on, all sync'd channels.\n\
  C-b: set x window bounds (preceded by 1 arg)\n\
  C-c: hide controls\n\
  C-d: print\n\
  C-e: give last keyboard macro a name\n\
  C-f: open file\n\
  C-g: abort command\n\
  C-i: insert file\n\
  C-m: add named mark\n\
  C-o: show controls\n\
  C-p: set window size (preceded by 1 arg)\n\
  C-q: mix in file\n\
  C-r: redo last undone edit\n\
  C-s: save file\n\
  C-u: undo last edit\n\
       Snd supports 'unlimited undo/redo'\n\
  C-v: set window size as percentage of total\n\
  C-w: save current channel in file\n\
  C-z: smooth using cosine\n\
\n\
Unless otherwise noted, case is not significant; \n\
C-a is the same as C-A.\n\
\n\
Most commands can be prefaced by an integer or\n\
a float; the integer causes the command to be repeated\n\
that many times; the float is multiplied by the\n\
sound's sampling rate, then applied that many times.\n\
So, for example, C-u 1.0 C-f causes the cursor to move\n\
ahead one second in the sound.\n\
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
" S_spectro_x_scale " and " S_spectro_y_scale ".\n\
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
" S_transform_normalization " to " S_dont_normalize ".\n\
\n\
";


void find_help(snd_state *ss) 
{
  snd_help(ss, "Find", 
"Searches in Snd refer to the sound data, and are in general patterned after Emacs.  When you type \
C-s or C-r, the minibuffer below the graph is activated and you are asked for the search function. \
The expression is a function of one argument, the current sample value.  It should return #t when the \
search is satisified.  For example, (lambda (n) (> n .1) looks for the next sample that is greater than .1. \
Successive C-s or C-r repeat the search.  C-x C-s can redefine the search pattern, which is also cleared in other \
events, much like Emacs. \
\n\n\
Normally, the search applies only to the current channel. To search all current files at once, use the Edit:Find dialog.",
	   TRUE);
}

void undo_help(snd_state *ss) 
{
  snd_help(ss, "Undo", 
"Snd supports unlimited undo in the sense that you can back up through all \
the edits since the last save, and at any point redo those edits.  Certain \
operations require that temporary files be written, so disk space may eventually \
become a problem.  Revert is the same as backing up to the last save. \
\n\n\
In addition, eight or so of the previous selections are saved on a 'region' stack accessible via C-y.",
	   TRUE);
}

void sync_help(snd_state *ss) 
{
  snd_help(ss, "Sync", 
"The sync button causes certain operations to apply to all channels simultaneously.  In mono \
sounds, the sync button has a similar effect, but applied across multiple sounds. \
\n\n\
To get multi-channel selections, set the sync button, then define the selection (by dragging \
the mouse) in one channel, and the parallel portions of the other channels will also be selected. ",
	   TRUE);
}

static char speed_help_string[] = 
"'Speed' refers to the rate at which the sound data is consumed during playback. \
Another term might be 'srate'.  Snd uses sinc interpolation to perform the speed \
change.  The arrow button on the right determines the direction it moves through the data. \
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
envelopes: \
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
  snd_help(ss, "Contrast", 
"'Contrast enhancement' is my name for this somewhat weird waveshaper or compander.  It \
phase-modulates a sound, which can in some cases make it sound sharper or brighter. \
For softer sounds, it causes only an amplitude change.  Contrast is on only if the contrast button is set.",
	   TRUE);
}

void env_help(snd_state *ss) 
{
  snd_help(ss, "Envelope", 
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
loudest point in the file becomes .5). ",
	   TRUE);
}

static char sound_files_help_string[] = 
"Snd can read and write any of the sound\n\
file data and header formats that Snd can\n\
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
     Sound Designer I, PSION, MAUD, Kurzweil 2000,\n\
     Gravis Ultrasound, ASF, PAF, CSL,\n\
     Comdisco SPW, Goldwave sample, omf, quicktime\n\
     Sonic Foundry, SBStudio II, Delusion digital,\n\
     Digiplayer ST3, Farandole Composer WaveSample,\n\
     Ultratracker WaveSample, Sample Dump exchange,\n\
     Yamaha SY85, SY99, and TX16w, Covox v8, SPL, AVI,\n\
     Impulse tracker, Korg, Akai, Turtle Beach\n\
     ----\n\
automatically translated to Sun 16-bit, then read/write:\n\
     IEEE text, Mus10, SAM 16-bit (modes 1 and 4), AVI\n\
     NIST shortpack, HCOM, Intel, IBM, and Oki (Dialogic) ADPCM, \n\
     G721, G723_24, G723_40, MIDI sample dump\n\
\n\
'Linear' here means 2's complement integer.\n\
The files can have any number of channels.\n\
Data can be either big or little endian.\n\
For MPEG and OGG translation, see the code in examp.scm.\n\
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
macro, or dealt with from the lisp listener panel. I've tried to bring out to lisp nearly\n\
every portion of Snd, both the signal-processing functions, and much of the\n\
user interface. You can, for example, add your own menu choices, editing\n\
operations, or graphing alternatives. These extensions can be loaded at any\n\
time.  See extsnd.html and grfsnd.html for details.\n\
\n\
";


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
  snd_help(ss, "Envelope Editor",
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
" S_enved_filter_order " which defaults to 40. To apply the changes to the selection, \
rather than the current sound, set the 'selection' button.  To apply it to the currently selected mix, \
set the 'mix' button. \
\n\n\
The two toggle buttons at the lower right choose whether to show a light-colored version of the \
currently active sound (the 'wave' button), and whether to clip mouse movement at the current y \
axis bounds (the 'clip' button).",
	   TRUE);
}

void about_snd_help(snd_state *ss)
{
  ssnd_help(ss,
	    "Snd",
about_snd_help_string,
"The various Help menu items are:\n\
\n",
help_menu_help_string,
"\n",
"The main menu items are:\n\
  File: operations on files.\n\
  Edit: operations on the selection.\n\
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
NULL);
}

void fft_help(snd_state *ss)
{
  ssnd_help(ss,
	    "FFT",
fft_help_string,
"\n\
",
fft_keypad_help_string,
NULL);
}

void speed_help(snd_state *ss) {snd_help(ss, "Speed", speed_help_string, TRUE);}
void expand_help(snd_state *ss) {snd_help(ss, "Expand", expand_help_string, TRUE);}
void reverb_help(snd_state *ss) {snd_help(ss, "Reverb", reverb_help_string, TRUE);}
void marks_help(snd_state *ss) {snd_help(ss, "Marks", mark_help_string, FALSE);}
void mix_help(snd_state *ss) {snd_help(ss, "Mixing", mix_help_string, TRUE);}
void sound_files_help(snd_state *ss) {snd_help(ss, "Format", sound_files_help_string, FALSE);}
void recording_help(snd_state *ss) {snd_help(ss, "Recording", recording_help_string, TRUE);}
void init_file_help(snd_state *ss) {snd_help(ss, "Customization", init_file_help_string, TRUE);}


/* -------- dialog help button -------- */

void transform_dialog_help(snd_state *ss)
{
  snd_help(ss,
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
of the selection, if any is active in the current channel.  The fft data is scaled to fit \
between 0.0 and 1.0 unless the fft normalization is off. The full frequency axis is normally \
displayed, but the axis is 'dragable' -- put the mouse on the axis and drag it either way to change \
the range (this is equivalent to changing the variable " S_spectro_cutoff "). You can also click on \
any point in the fft to get the associated fft data displayed; if " S_verbose_cursor " is on, you can \
drag the mouse through the fft display and the description in the minibuffer will be constantly updated. \
\n\n\
The harmonic analysis function is normally the Fourier Transform, but others are available, \
including about 20 wavelet choices. \
\n\n\
The top three buttons in the transform dialog choose between a normal fft, a sonogram, or a \
spectrogram. The 'peaks' button affects whether peak info is displayed alongside the graph of the \
spectrum. The 'dB' button selects between a linear and logarithmic Y (magnitude) axis. The 'log freq' \
button makes a similar choice along the frequency axis.",
	   TRUE);	   
}

void color_dialog_help(snd_state *ss)
{
  snd_help(ss,
	   "View Color",
"This dialog sets the colormap and associated variables used during sonogram, spectrogram,  \
and perhaps wavogram display. The cutoff scale refers to the minimum data value to be displayed.",
	   TRUE);	   
}

void orientation_dialog_help(snd_state *ss)
{
  snd_help(ss,
	   "View Orientation",
	   "This dialog sets the rotation and scaling variables used during sonogram, spectrogram, and wavogram display.",
	   TRUE);	   
}

void region_dialog_help(snd_state *ss)
{
  snd_help(ss, "Region Browser",
"This is the 'region browser'.  The scrolled window contains the list of current regions \
with a brief title to indicate the provenance thereof, and two buttons.  The 'save' button \
protects or unprotects the region from deletion. The 'play' button plays the associated region. \
One channel of the currently selected region is displayed in the graph window.  The up and \
down arrows move up or down in the region's list of channels.  If you click a region's \
title, the text is highlighted, and that region is displayed in the graph area.  You can delete the \
region by clicking the 'Delete' button.  To dismiss the browser, click 'Ok'.  The 'edit' button \
loads the region into the main editor as a temporary file.  It can be edited or renamed, etc.  If you save \
the file, the region is updated to reflect any edits you made.",
	   TRUE);
}

void raw_data_dialog_help(snd_state *ss)
{
  snd_help(ss,
	   "Raw Data",
"To display and edit sound data, Snd needs to know how the data's sampling rate, number \
of channels, and numerical format.  This dialog gives you a chance to set those fields. \
To make the current settings the default for any future headerless files, click the \
'Default' button.",
	   TRUE);
}

void new_file_dialog_help(snd_state *ss)
{
  snd_help(ss,
	   "New File",
	   "This dialog sets the new file's output header type, data format, srate, chans, and comment if any.",
	   TRUE);
}

void edit_header_dialog_help(snd_state *ss)
{
  snd_help(ss,
	   "Edit Header",
"This dialog edits the header of a sound file. No change is made to the actual sound data; the \
new header is blindly written, any unsaved edits are ignored. If you specify 'raw' as the type, \
any existing header is removed.  This dialog is aimed at adding or removing an entire header,  \
or editing the header comments; anything else is obviously dangerous.",
	   TRUE);
}

void print_dialog_help(snd_state *ss)
{
  snd_help(ss,
	   "File Print",
"Print causes the currently active display to be either printed (via the lpr command) or saved as \
an eps file.  In the latter case, the file name is set either by the dialog, or taken from the \
resource epsFile (normally snd.eps).",
	   TRUE);
}

void view_files_dialog_help(snd_state *ss)
{
  snd_help(ss,
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
unsorted) refers to this menu.",
	   TRUE);	   
}

#define GLYPH_WIDTH 11

char* word_wrap(char *text, int widget_len)
{
  char *new_text;
  int new_len, old_len, i, j, line_len = 0, desired_len;
#if HAVE_RUBY
  int move_paren = FALSE;
#endif
  old_len = snd_strlen(text);
#if HAVE_RUBY
  new_len = old_len + 64;
#else
  new_len = old_len + 32;
#endif
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
#if HAVE_RUBY
	/* try to change the reported names to Ruby names */
	/* TODO: capitalize the constants' names, prepend $ to hook names, add commas in arg lists, show key/opt args in Ruby syntax */
	if (text[i] == '-')
	  {
	    if ((i > 0) && (isalnum((int)(text[i - 1]))) && (i < old_len))
	      {
		if (isalnum((int)(text[i + 1])))
		  new_text[j++] = '_';
		else
		  {
		    if (text[i + 1] == '>')
		      {
			new_text[j++] = '2';
			i++;
		      }
		    else new_text[j++] = text[i];
		  }
	      }
	    else new_text[j++] = text[i];
	  }
	else
	  {
	    if ((text[i] == '#') && (i < old_len))
	      {
		if (text[i + 1] == 'f')
		  {
		    new_text[j++] = 'f';
		    new_text[j++] = 'a';
		    new_text[j++] = 'l';
		    new_text[j++] = 's';
		    new_text[j++] = 'e';
		    i++;
		  }
		else
		  {
		    if (text[i + 1] == 't')
		      {
			new_text[j++] = 't';
			new_text[j++] = 'r';
			new_text[j++] = 'u';
			new_text[j++] = 'e';
			i++;
		      }
		    else new_text[j++] = text[i];
		  }
	      }
	    else
	      {
		if ((i == 0) && (text[i] == '('))
		  {
		    move_paren = TRUE;
		  }
		else 
		  {
		    if ((move_paren) && (text[i] == ' '))
		      {
			new_text[j++] = '(';
			move_paren = FALSE;
		      }
		    else new_text[j++] = text[i];
		  }
	      }
	  }
#else
	new_text[j++] = text[i];
#endif
	if (text[i] == '\n')
	  line_len = 0;
	line_len++;
      }
  return(new_text);
}

static XEN help_hook = XEN_FALSE;
static XEN output_comment_hook = XEN_FALSE;

static char *run_string_hook(XEN hook, const char *caller, char *initial_string, char *subject)
{
  /* no longer concats -- now just passes successive results along */
  if (XEN_HOOKED(hook))
    {
      XEN result;
      XEN procs = XEN_HOOK_PROCEDURES(hook);
#if HAVE_GUILE
      result = C_TO_XEN_STRING(initial_string);
      while (XEN_NOT_NULL_P(procs))
	{
	  if (subject)
	    result = XEN_CALL_2(XEN_CAR(procs),
				C_TO_XEN_STRING(subject),
				result,
				caller);
	  else result = XEN_CALL_1(XEN_CAR(procs),
				   result,
				   caller);
	  procs = XEN_CDR(procs);
	}
#else
      if (subject)
	result = XEN_CALL_2(procs,
			    C_TO_XEN_STRING(subject),
			    C_TO_XEN_STRING(initial_string),
			    caller);
      else result = XEN_CALL_1(procs,
			       C_TO_XEN_STRING(initial_string),
			       caller);
#endif
      if (XEN_STRING_P(result))
	return(copy_string(XEN_TO_C_STRING(result)));
    }
  return(copy_string(initial_string));
}

char *output_comment(file_info *hdr)
{
  return(run_string_hook(output_comment_hook, S_output_comment_hook, (hdr) ? hdr->comment : NULL, NULL));
}

XEN g_snd_help(XEN text, int widget_wid)
{
  #define H_snd_help "(" S_snd_help " (arg 'snd-help) (formatted #t)): return the documentation \
associated with its argument. (snd-help 'make-vct) for example, prints out a brief description of make-vct. \
The argument can be a string, a symbol, or the object itself.  In some cases, only the symbol has the documentation. \
In the help descriptions, optional arguments are in parens with the default value (if any) as the 2nd entry. \
A ':' as the start of the argument name marks a CLM-style optional keyword argument.  If you load index.scm \
the functions html and ? can be used in place of help to go to the HTML description, \
and the location of the associated C code will be displayed, if it can be found. \
If help-hook is not empty, it is invoked with the subject and the snd-help result \
and its value is returned."

  XEN help_text = XEN_FALSE; 
  char *str = NULL, *new_str, *subject = NULL;
#if HAVE_GUILE
  XEN value;
  if (XEN_EQ_P(text, XEN_UNDEFINED))                              /* if no arg, describe snd-help */
    {
      help_text = C_TO_XEN_STRING(H_snd_help);
      subject = H_snd_help;
    }
  else
    {
      if ((XEN_STRING_P(text)) || (XEN_SYMBOL_P(text)))            /* arg can be name (string), symbol, or the value */
	{
	  if (XEN_STRING_P(text))
	    str = XEN_TO_C_STRING(text);
	  else str = XEN_SYMBOL_TO_C_STRING(text);
	  subject = str;
	  value = XEN_NAME_AS_C_STRING_TO_VALUE(str);
	}
      else value = text;
      help_text = XEN_OBJECT_HELP(value);         /* (object-property ...) */
      if ((XEN_FALSE_P(help_text)) &&
	  (XEN_PROCEDURE_P(value)))
	{
	  help_text = XEN_PROCEDURE_HELP(value);  /* (procedure-property ...) */
	  if (XEN_FALSE_P(help_text))
	    help_text = XEN_PROCEDURE_SOURCE_HELP(value);      /* (procedure-documentation ...) -- this is the first line of source if string */
	}
      if ((XEN_FALSE_P(help_text)) &&
	  (str))
	help_text = XEN_OBJECT_HELP(C_STRING_TO_XEN_SYMBOL(str));
    }
  
  /* help strings are always processed through the word-wrapper to fit whichever widget they are posted to */
  /*   this means all the H_doc strings in Snd need to omit line-feeds except where necessary (i.e. code) */

  if (XEN_STRING_P(help_text))
    str = XEN_TO_C_STRING(help_text);
#endif
#if HAVE_RUBY
  if (XEN_STRING_P(text))
    subject = XEN_TO_C_STRING(text);
  else 
    if (XEN_SYMBOL_P(text))
      subject = XEN_SYMBOL_TO_C_STRING(text);
    else subject = H_snd_help;
  str = xen_help(subject);
#endif
  if (str)
    {
      if (subject)
	new_str = run_string_hook(help_hook, S_help_hook, str, subject);
      else new_str = copy_string(str);
      if (widget_wid > 0)
	{
	  str = word_wrap(new_str, widget_wid);
	  if (new_str) FREE(new_str);
	}
      else str = new_str;
      help_text = C_TO_XEN_STRING(str);
      if (str) FREE(str);
    }
  return(help_text);
}

static XEN g_listener_help(XEN arg, XEN formatted)
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(formatted), formatted, XEN_ARG_2, S_snd_help, "a boolean");
  if (XEN_FALSE_P(formatted))
    return(g_snd_help(arg, 0));
  return(g_snd_help(arg, listener_width()));
}

void set_html_dir(snd_state *ss, char *new_dir)
{
  if (html_dir(ss)) FREE(html_dir(ss));
  set_html_dir_1(ss, new_dir);
}

static XEN g_html_dir(void) 
{
  #define H_html_dir "(" S_html_dir "): location of Snd documentation"
  return(C_TO_XEN_STRING(html_dir(get_global_state())));
}

static XEN g_set_html_dir(XEN val) 
{
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_html_dir, "a string");
  set_html_dir(get_global_state(), copy_string(XEN_TO_C_STRING(val))); 
  return(val);
}

static XEN g_html_program(void) 
{
  #define H_html_program "(" S_html_program "): name of documentation reader (netscape, by default)"
  return(C_TO_XEN_STRING(html_program(get_global_state())));
}

static XEN g_set_html_program(XEN val) 
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_html_program, "a string");
  ss = get_global_state();
  if (html_program(ss)) FREE(html_program(ss));
  set_html_program(ss, copy_string(XEN_TO_C_STRING(val))); 
  return(val);
}


#ifdef XEN_ARGIFY_1
XEN_ARGIFY_2(g_listener_help_w, g_listener_help)
XEN_NARGIFY_0(g_html_dir_w, g_html_dir)
XEN_NARGIFY_1(g_set_html_dir_w, g_set_html_dir)
XEN_NARGIFY_0(g_html_program_w, g_html_program)
XEN_NARGIFY_1(g_set_html_program_w, g_set_html_program)
#else
#define g_listener_help_w g_listener_help
#define g_html_dir_w g_html_dir
#define g_set_html_dir_w g_set_html_dir
#define g_html_program_w g_html_program
#define g_set_html_program_w g_set_html_program
#endif

void g_init_help(void)
{
  XEN_DEFINE_PROCEDURE(S_snd_help, g_listener_help_w, 0, 2, 0, H_snd_help);

  #define H_help_hook S_help_hook "(subject help-string): called from snd-help.  If \
if returns a string, it replaces 'help-string' (the default help)"

  XEN_DEFINE_HOOK(help_hook, S_help_hook, 2, H_help_hook);    /* args = subject help-string */

  #define H_output_comment_hook S_output_comment_hook " (str): called in Save-As dialog, passed current sound's comment, if any. \
If more than one hook function, each function gets the previous function's output as its input.\n\
  (add-hook! output-comment-hook\n\
    (lambda (str)\n\
      (string-append str \": written \"\n\
        (strftime \"%a %d-%b-%Y %H:%M %Z\"\n\
          (localtime (current-time))))))"

  XEN_DEFINE_HOOK(output_comment_hook, S_output_comment_hook, 1, H_output_comment_hook); /* arg = current mus_sound_comment(hdr) if any */

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_html_dir, g_html_dir_w, H_html_dir, S_setB S_html_dir, g_set_html_dir_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_html_program, g_html_program_w, H_html_program, S_setB S_html_program, g_set_html_program_w,  0, 0, 1, 0);
}
