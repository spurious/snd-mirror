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
    strcat(buf, str);
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
  int major = 0, minor = 0;
  char *version;
  if ((ss == NULL) || (ss->sgx == NULL)) return(""); /* snd --help for example */
  version = (char *)CALLOC(128, sizeof(char));
#if USE_MOTIF
  if (MAIN_DISPLAY(ss) != NULL)
    {
      if (ss->sgx->cx)
	{
	  glXMakeCurrent(MAIN_DISPLAY(ss), XtWindow(ss->sgx->mainshell), ss->sgx->cx);
	  mus_snprintf(version, 128, " %s", glGetString(GL_VERSION));
	}
      else 
	{
	  glXQueryVersion(MAIN_DISPLAY(ss), &major, &minor);
	  mus_snprintf(version, 128, " %d.%d", major, minor);
	}
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
  buf = (char *)CALLOC(2048, sizeof(char));
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
#else
#ifdef __SUNPRO_C
	  "\n    Forte C ",
#endif
#endif
#if HAVE_GNU_LIBC_VERSION_H
	  "\n    Libc: ", gnu_get_libc_version(), ".", 
                          gnu_get_libc_release(),
#endif
	  "\n", NULL);
  return(buf);
}

static void ssnd_help(const char *subject, ...);

void about_snd_help(void)
{
  char *info = NULL, *features = NULL;
  info = version_info();
#if HAVE_GUILE
  features = word_wrap(XEN_AS_STRING(XEN_EVAL_C_STRING("*features*")), 600);
#endif
#if HAVE_RUBY
  features = word_wrap(XEN_AS_STRING(XEN_EVAL_C_STRING("$\".join(' ')")), 600);
#endif
  ssnd_help("Snd is a sound editor.",
	    info,
	    "\nRecent changes include:\n\
\n\
19-Sep:  clm-ins.rb thanks to Michael Scholz.\n\
         removed menu-hook.\n\
18-Sep:  info-dialog.\n\
17-Sep:  removed finder.scm, changed index.scm|rb to use snd-xref.c tables.\n\
15-Sep:  just-sounds support in Gtk.\n\
12-Sep:  quit-button-color, help-button-color, reset-button-color, \n\
         doit-button-color, doit-again-button-color.\n\
8-Sep:   added show-all-axes-unlabelled and show-x-axis-unlabelled.\n\
         removed parse-rc-file, added support for Snd.gtkrc.\n\
2-Sep:   removed bold-button-font and boldbuttonFont resource.\n\
29-Aug:  gcc complex trig replaces GSL if it's available.\n\
21-Aug:  added snd->sample and xen->sample (Snd-specific) generators to redirect\n\
           ina and friends automatically to Snd data.\n\
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
	    "Please send bug reports or suggestions to bil@ccrma.stanford.edu.",
NULL);
  if (info) FREE(info);
  if (features) FREE(features);
}


/* -------- basic helpers -------- */

static void ssnd_help(const char *subject, ...)
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
  snd_help(subject, newstr, false);
  FREE(newstr);
}  



void find_help(void) 
{
  snd_help_with_xrefs("Find", 
"Searches in Snd refer to the sound data, and are in general patterned after Emacs.  When you type \
C-s or C-r, the minibuffer below the graph is activated and you are asked for the search function. \
The expression is a function of one argument, the current sample value.  It should return #t when the \
search is satisified.  For example, (lambda (n) (> n .1) looks for the next sample that is greater than .1. \
Successive C-s or C-r repeat the search.  C-x C-s can redefine the search pattern, which is also cleared in other \
events, much like Emacs. \
\n\n\
Normally, the search applies only to the current channel. To search all current files at once, use the Edit:Find dialog.",
		      true,
		      "Search");
}

void undo_help(void) 
{
  snd_help("Undo and Redo", 
"Snd supports unlimited undo in the sense that you can back up through all \
the edits since the last save, and at any point redo those edits.  Certain \
operations require that temporary files be written, so disk space may eventually \
become a problem.  Revert is the same as backing up to the last save.",
	   true);
}

void sync_help(void) 
{
  snd_help("Sync", 
"The sync button causes certain operations to apply to all channels simultaneously.  In mono \
sounds, the sync button has a similar effect, but applied across multiple sounds. \
\n\n\
To get multi-channel selections, set the sync button, then define the selection (by dragging \
the mouse) in one channel, and the parallel portions of the other channels will also be selected. ",
	   true);
}

void env_help(void) 
{
  snd_help_with_xrefs("Envelope", 
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
		      true,
		      "Envelope");
}

void envelope_editor_dialog_help(void)
{
  snd_help("Envelope Editor",
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
	   true);
}

void fft_help(void)
{
  snd_help_with_xrefs("FFT",
"The FFT performs a projection of the time domain into the frequency domain. Good discussions of the Fourier Transform \
and the trick used in the FFT itself can be found in many DSP books; those I know of include 'A Digital Signal Processing \
Primer', Ken Steiglitz, Addison-Wesley, 1996; or 'Numerical Recipes in C'. \
\n\n\
The FFT size can be any power of 2. The larger, the longer it takes to compute, and the larger the amount of the time domain \
that gets consumed.  Interpretation of the FFT results is not straightforward! The window choices are taken primarily \
from Harris' article: Fredric J. Harris, 'On the Use of Windows for Harmonic Analysis with the Discrete Fourier Transform', Proceedings of the \
IEEE, Vol. 66, No. 1, January 1978, with updates from: Albert H. Nuttall, 'Some Windows with Very Good Sidelobe Behaviour', IEEE Transactions \
of Acoustics, Speech, and Signal Processing, Vol. ASSP-29, 1, February 1981. \
\n\n\
Nearly all the transform-related choices are set by the transform dialog launched from the Options \
Menu Transform item. Most of this dialog should be self-explanatory.  Some of the windows take an \
additional parameter sometimes known as alpha or beta.  This is set in Snd by the scroller in the \
transform dialog. \
\n\n\
The FFT display is activated by setting the 'f' button on the channel's window.  It then updates \
itself each time the time domain waveform moves or changes.  The update function runs in the \
background, so in some cases, notably very large FFTs, you will notice that the FFT updates less \
often than the time domain. \
\n\n\
The spectrum data is usually normalized to fit between 0.0 to 1.0; if you'd rather have un-normalized \
data (the y-axis in this case changes to reflect the data values, to some extent), set the variable \
transform-normalization to dont-normalize.",
		      true,
		      "FFT");
}

void controls_help(void) 
{
  snd_help("The Control Panel", 
"The control panel can provide a quick preview of various common ways to process a sound. \
\n\n\
'Speed' refers to the rate at which the sound data is consumed during playback.  Another term might be 'srate'.  Snd uses sinc interpolation \
to perform the speed change.  The arrow button on the right determines the direction it moves through the data.  The scroll bar position is \
normally interpreted as a float between .05 and 20.  The Options Speed Style menu (oor the speed-control-style variable) can change this to \
use semitones (actually microtones) or just-intonation ratios.  The number of equal divisions to the octave in the semitone case is set by \
the variable speed-control-tones (normally 12). \
\n\n\
'Expand' refers to a kind of granular synthesis used to change the tempo of events in the sound without changing pitch.  Successive short \
slices of the file are overlapped with the difference in size between the input and output hops (between successive slices) giving the \
change in tempo.  This doesn't work in all files -- it sometimes sounds like execrable reverb or is too buzzy -- but it certainly is \
more robust than the phase vocoder approach to the same problem. There are a variety of variables that control hop sizes,  \
segment lengths, and overall segment envelopes: expand-control-ramp, etc.  The expander is on only if the expand button is set. \
\n\n\
'Contrast enhancement' is my name for this somewhat weird waveshaper or compander.  It phase-modulates a sound, which can in some cases \
make it sound sharper or brighter.  For softer sounds, it causes only an amplitude change.  Contrast is on only if the contrast button is set. \
\n\n\
The control panel reverberator is a version of Michael McNabb's Nrev. In addition to the controls in the control pane, you can set the \
reverb feedback gains and the coefficient of the low pass filter in the allpass bank (reverb-control-feedback and reverb-control-lowpass). \
The reverb is on only if the reverb button is set. ",
	   true);
}

void marks_help(void) 
{
  snd_help_with_xrefs("Marks", 
"A mark in Snd is attached to a particular sample in the sound data.  It moves with that \
sample as you edit the data, and if the sample is deleted, so is its mark.  Marks also follow \
the undo/redo edit history -- I'm not sure this is a good idea, but it seemed more intuitive \
than other alternatives.  This means that marks are 'undone' and 'redone' alongside the edits \
that they accompany.  The mark symbol itself has three or four \
parts.  The name, if any, is at the top. Then a 'tab'.  You can click the name or \
tab portion and drag the mark to redefine it. Then a line to the bottom of the graph, showing \
where the mark is. And, below the x axis, an arrow.  You can click and drag the arrow to \
play the data following the mouse -- sort of like listening to a tape as you rock it back \
and forth by hand on the spindles. Or just click the arrow to play the data starting \
at the mark.",
		      true, 
		      "Mark");
}

void mix_help(void) 
{
  snd_help_with_xrefs("Mixing", 
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
It is often handy to collect several mixes into a 'track'; mix.scm implements a variety of track-related operations.",
		      true, 
		      "Mix");
}

void recording_help(void) 
{
  snd_help("Record", 
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
with the variable vu-size which defaults to 1.0. Similarly the variable vu-font-size tries to \
change the size of the numbers on the label, and vu-font chooses the family name of the font used. \
\n\n\
If you go to the main Snd window while the recorder is active and play a sound, the \
recorder's audio lines are made inactive to try to reduce confusion.  To re-activate \
the recorder, press the 'reset' button at the bottom of the window. \
\n\n\
Digital input is slightly tricky -- you need to set the sampling rate before you \
click the 'digital input' button; otherwise you'll get a stuttering effect because the output \
(monitor) rate doesn't match the input rate.",
	   true);
}

void sound_files_help(void) 
{
  snd_help("Headers and Data", 
"Snd can read and write any of the sound file data and header formats that Sndlib can handle, \
the most useful being AIFC, NeXT, RIFF, and raw (no header). The files can have any number of channels. \
Data can be either big or little endian. For MPEG and OGG translation, see the code in examp.scm.",
	   true);
}

void init_file_help(void) 
{
  snd_help("Customization",
"Nearly everything in Snd can be set in an initialization file, loaded at any time from a saved-state (Guile) file, specified \
via inter-process communciation from any other program, invoked via M-x in the minibuffer, imbedded in a keyboard macro, or  \
dealt with from the lisp listener panel. I've tried to bring out to lisp nearly every portion of Snd, \
both the signal-processing functions, and much of the user interface. You can, for example, add your own menu choices, \
editing operations, or graphing alternatives. These extensions can be loaded at any time.  See extsnd.html and \
grfsnd.html for details.",
	   true);
}

void key_binding_help(void)
{
  snd_help("Key bindings",
"[Down] zoom out\n\
[Up] zoom in\n\
[Left] move window left\n\
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
C-r: search backwards\n\
C-s: search forwards\n\
C-t: stop playing\n\
C-u: start count definition.  If followed by a\n\
     float, the actual count is that number multiplied\n\
     by the current sampling rate.  If the optional\n\
     number is followed by C-m, the count returned\n\
     is the distance from the cursor to the n-th\n\
     successive mark.  That is, C-u C-m C-f is the\n\
     same as C-j.\n\
C-v: move cursor to mid-window\n\
C-w: delete selection\n\
C-y: insert selection.\n\
C-z: set sample at cursor to 0.0\n\
C-_: undo\n\
C-[Space]: start selection definition\n\
C-M-g: clear listener\n\
\n\
C-x a: apply envelope to selection\n\
C-x b: position window so cursor is on left margin\n\
C-x c: define selection from cursor to nth mark\n\
C-x d: set temp dir name\n\
C-x e: execute keyboard macro\n\
C-x f: position window so cursor is on right margin\n\
C-x i: insert region\n\
C-x j: goto named mark\n\
C-x k: close file\n\
C-x l: position selection in mid-view\n\
C-x o: move to next or previous graph\n\
C-x p: play selection or region n\n\
C-x q: mix in selection\n\
C-x r: redo\n\
C-x u: undo\n\
C-x v: position window over selection\n\
C-x w: save selection as file\n\
C-x z: smooth selection\n\
C-x /: place named mark\n\
C-x (: begin keyboard macro definition\n\
C-x ): end keyboard macro definition\n\
\n\
C-x C-a: apply envelope.\n\
C-x C-b: set x window bounds (preceded by 1 arg)\n\
C-x C-c: hide control panel\n\
C-x C-d: print\n\
C-x C-e: give last keyboard macro a name\n\
C-x C-f: open file\n\
C-x C-g: abort command\n\
C-x C-i: insert file\n\
C-x C-m: add named mark\n\
C-x C-o: show control panel\n\
C-x C-p: set window size (preceded by 1 arg)\n\
C-x C-q: mix in file\n\
C-x C-r: redo\n\
C-x C-s: save file\n\
C-x C-u: undo\n\
C-x C-v: set window size as percentage of total\n\
C-x C-w: save current channel in file\n\
C-x C-z: smooth using cosine\n\
\n\
To change a key binding, use bind-key",
	   false);
}

void play_help(void)
{
  snd_help_with_xrefs("Play",
"To play a sound, click the 'play' button.  If the sound has more \
channels than your DAC(s), Snd will (normally) try to mix the extra channels \
into the available DAC outputs.  While it is playing, \
you can click the button again to stop it, or click some other \
file's 'play' button to mix it into the current set of sounds \
being played. To play from a particular point, set a mark \
there, then click its 'play triangle' (the triangular portion \
below the x axis).  (Use control-click here to play all channels \
from the mark point). To play simultaneously from an arbitrary \
group of start points (possibly spread among many sounds), \
set syncd marks at the start points, then click the play \
triangle of one of them. \
\n\n\
The Edit menu 'Play' option plays the current \
selection, if any.  The Popup menu's 'Play' option plays the \
currently selected sound.  And the region and file browsers provide \
play buttons for each of the listed regions or files.  If you \
hold down the control key when you click 'play', the cursor \
follows along as the sound is played.   \
\n\n\
In a multi-channel file, C-q plays all channels from the current channel's \
cursor if the sync button is on, and otherwise plays only the current channel. \
Except in the browsers, what is actually \
played depends on the control panel.",
		      true,
		      "Play");
}

void reverb_help(void)
{
  snd_help_with_xrefs("Reverb",
"The reverb in the control panel is a version of Michael McNabb's Nrev.  There are other \
reverbs mentioned in the related topics list.",
		      true,
		      "Reverb");
}

void save_help(void)
{
  snd_help_with_xrefs("Save",
"To save the current edited state of a file, use the Save option (to overwrite the old version of the \
file), or Save as (to write to a new file, leaving the old file unchanged).  The equivalent keyboard \
command is C-x C-s (save).  Other related keyboard commands are C-x w (save selection as file), and \
C-x C-w (extract and save the current channel as a file). Normally, if the new file already exists, and it is \
not currently being edited in Snd, it is silently overwritten.  If you try to overwrite a file, and \
that file has active edits in a different Snd window, you'll be asked for confirmation. \
If you want Snd to ask before overwriting a file in any case, set the resource overwriteCheck to 1, \
or include the expression (set! (ask-before-overwrite) #t) in your Snd initialization file.",
		      true,
		      "Save");
}

void filter_help(void)
{
  snd_help_with_xrefs("Filter",
"There is an FIR Filter in the control panel, and a variety of other filters scattered around; \
see dsp.scm in particular.",
		      true,
		      "Filter");
}

void resample_help(void)
{
  snd_help_with_xrefs("Resample",
"There is a sampling rate changer in the control panel; see the related topics list below.",
		      true,
		      "Resample");
}

void insert_help(void)
{
  snd_help_with_xrefs("Insert",
"To insert a file, use C-x C-i, and to insert the selection C-x i.  C-o inserts a \
zero sample at the cursor",
		      true,
		      "Insert");
}

void delete_help(void)
{
  snd_help_with_xrefs("Delete",
"To delete a sample, use C-d; to delete the selection, C-w",
		      true,
		      "Delete");
}

/* -------- dialog help button -------- */

void transform_dialog_help(void)
{
  snd_help_with_xrefs("Transform Options",
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
		      true,
		      "FFT");
}

void color_dialog_help(void)
{
  snd_help("View Color",
"This dialog sets the colormap and associated variables used during sonogram, spectrogram,  \
and perhaps wavogram display. The cutoff scale refers to the minimum data value to be displayed.",
	   true);	   
}

void orientation_dialog_help(void)
{
  snd_help("View Orientation",
	   "This dialog sets the rotation and scaling variables used during sonogram, spectrogram, and wavogram display.",
	   true);	   
}

void region_dialog_help(void)
{
  snd_help_with_xrefs("Region Browser",
"This is the 'region browser'.  The scrolled window contains the list of current regions \
with a brief title to indicate the provenance thereof, and two buttons.  The 'save' button \
protects or unprotects the region from deletion. The 'play' button plays the associated region. \
One channel of the currently selected region is displayed in the graph window.  The up and \
down arrows move up or down in the region's list of channels.  If you click a region's \
title, the text is highlighted, and that region is displayed in the graph area.  You can delete the \
region by clicking the 'Delete' button.  To dismiss the browser, click 'Ok'.  The 'edit' button \
loads the region into the main editor as a temporary file.  It can be edited or renamed, etc.  If you save \
the file, the region is updated to reflect any edits you made.",
		      true,
		      "Region");
}

void raw_data_dialog_help(void)
{
  snd_help("Raw Data",
"To display and edit sound data, Snd needs to know how the data's sampling rate, number \
of channels, and numerical format.  This dialog gives you a chance to set those fields. \
To make the current settings the default for any future headerless files, click the \
'Default' button.",
	   true);
}

void completion_dialog_help(void)
{
  snd_help("completion",
	   "These are the completions that Snd thinks might be likely. If you select one, it will be used to complete the current name.",
	   true);
}

void save_as_dialog_help(void)
{
  snd_help_with_xrefs("Save As",
"You can save the current state of a file or region under a different file name using the Save \
As option.  The output header type, data format,  and sampling rate can also be set.  The data formats \
are little-endian where relevant except for 'aifc' output.  If a file by the chosen name already exists \
it is silently overwritten, unless that file is already open in Snd and has edits.  In that case,  \
you'll be asked what to do.  If you want to be warned whenever a file is about to be overwritten by this \
option, set the resource overwriteCheck to 1 (or the ask-before-overwrite variable to #t). \
If you give the current file name to Save As,  \
any current edits will be saved and the current version in Snd will be updated (that is, in this \
case, the edit tree is not preserved).",
		      true,
		      "Save");
}

void open_file_dialog_help(void)
{
  snd_help("File",
"If you click the 'Sound Files Only' button, only those files in the current directory that look vaguely like sound files will be displayed.",
	   true);
}

void find_dialog_help(void)
{
  snd_help_with_xrefs("Global Find",
"This search travels through all the current channels in parallel until a match is found.  The find \
expression is a Scheme function of one argument,  the current sample value.  It should return #t when the \
search is satisified.  For example, (lambda (n) (> n .1)) looks for the next sample that is greater than .1.",
		      true,
		      "Find");
}

void mix_dialog_help(void)
{
  snd_help_with_xrefs("Mix Panel",
"This dialog provides various commonly-used controls on the currently \
selected mix.  At the top are the mix id, begin and end times, \
track number, and a play button.  Beneath that are various sliders \
controlling the speed (sampling rate) of the mix, and the amplitude of each \
input channel; and finally, an envelope editor for the mix's (input) channels. \
The current mix amp env is not actually changed until you click 'Apply Env'.\
The editor envelope is drawn in black with dots whereas the current \
mix amp env (if any) is drawn in blue.",
		      true,
		      "Mix");
}

void new_file_dialog_help(void)
{
  snd_help("New File",
	   "This dialog sets the new file's output header type, data format, srate, chans, and comment if any.",
	   true);
}

void edit_header_dialog_help(void)
{
  snd_help("Edit Header",
"This dialog edits the header of a sound file. No change is made to the actual sound data; the \
new header is blindly written, any unsaved edits are ignored. If you specify 'raw' as the type, \
any existing header is removed.  This dialog is aimed at adding or removing an entire header,  \
or editing the header comments; anything else is obviously dangerous.",
	   true);
}

void print_dialog_help(void)
{
  snd_help("File Print",
"Print causes the currently active display to be either printed (via the lpr command) or saved as \
an eps file.  In the latter case, the file name is set either by the dialog, or taken from the \
resource epsFile (normally snd.eps).",
	   true);
}

void view_files_dialog_help(void)
{
  snd_help("File Browser",
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
	   true);	   
}

#include "snd-xref.c"

#if 0
#define NUM_TOPICS 27
static char *topic_names[NUM_TOPICS] = {
  "Hook", "Vct", "Sample-reader", "Mark", "Mix", "Region", "Edit List", "Transform", "Error",
  "Color", "Font", "Graphic", "Widget", "Emacs",
  "CLM", "Instrument", "CM", "CMN", "libxm", "sndlib", 
  "Motif", "Gtk", "Script", "Ruby", "LADSPA", "OpenGL", "gdb"
};

static char *topic_urls[NUM_TOPICS] = {
  "extsnd.html#sndhooks", "extsnd.html#Vcts", "extsnd.html#samplereaders", "extsnd.html#sndmarks", 
  "extsnd.html#sndmixes", "extsnd.html#sndregions", "extsnd.html#editlists", "extsnd.html#sndtransforms", 
  "extsnd.html#snderrors", "extsnd.html#colors", "extsnd.html#fonts", "extsnd.html#sndgraphics", 
  "extsnd.html#sndwidgets", "grfsnd.html#emacssnd", "grfsnd.html#sndwithclm", 
  "grfsnd.html#sndinstruments", "grfsnd.html#sndwithcm", "sndscm.html#musglyphs", 
  "libxm.html#xm", "sndlib.html#introduction", "grfsnd.html#sndwithmotif", 
  "grfsnd.html#sndwithgtk", "grfsnd.html#sndwithnogui", "grfsnd.html#sndandruby", "grfsnd.html#sndandladspa", 
  "grfsnd.html#sndandgl", "grfsnd.html#sndandgdb"
};
#endif

#define NUM_XREFS 21
static char *xrefs[NUM_XREFS] = {
  "Mark", "Mix", "Region", "Selection", "Cursor", "Tracking cursor", "Delete", "Envelope", "Filter",
  "Search", "Insert", "Maxamp", "Play", "Reverse", "Save", "Smooth", "Resample", "FFT", "Reverb",
  "Src", "Find"};

static char **xref_tables[NUM_XREFS] = {
  Marking_xrefs, Mixing_xrefs, Regions_xrefs, Selections_xrefs, Cursors_xrefs, Tracking_cursors_xrefs,
  Deletions_xrefs, Envelopes_xrefs, Filters_xrefs, Searching_xrefs, Insertions_xrefs, Maxamps_xrefs,
  Playing_xrefs, Reversing_xrefs, Saving_xrefs, Smoothing_xrefs, Resampling_xrefs, FFTs_xrefs, Reverb_xrefs,
  Resampling_xrefs, Searching_xrefs};

#if HAVE_STRCASECMP
  #define STRCMP(a, b) strcasecmp(a, b)
#else
  #define STRCMP(a, b) strcmp(a, b)
#endif

char **snd_xrefs(const char *topic)
{
  int i;
  for (i = 0; i < NUM_XREFS; i++)
    if (STRCMP(topic, xrefs[i]) == 0)
      return(xref_tables[i]);
  return(NULL);
}

/* TODO: add help menu Effects? */
/* TODO: pass xref arg out to xen callers */
/* TODO: perhaps pass out the url lists as well? */
/*   about relate: clm, cm, cmn, libxm, sndlib
 *   customization: init file, emacs?
 *   control panel: 
 *   keys: 
 *   record: 
 *   undo/redo
 *   sync:
 *   headers/data
 */
/* TODO: if general topic (or menu label) in help text widget, go to topic */
/*
   includable: CLM ins, CLM gens
   ?? Plugins (current plugins? -- list-ladspa or equivalent)
   ?? FM, PhysMod, etc
*/

/* TODO: regexp access to help lists, tables */
/* TODO: regexp to g_snd_url (for index.rb) */

char *snd_url(const char *name)
{
  int i;
  for (i = 0; i < help_names_size; i++)
    if (STRCMP(help_names[i], name) == 0)
      return(help_urls[i]);
  return(NULL);
}

#define GLYPH_WIDTH 11

char* word_wrap(const char *text, int widget_len)
{
  char *new_text;
  int new_len, old_len, i, j, line_len = 0, desired_len;
#if HAVE_RUBY
  bool move_paren = false;
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
		    move_paren = true;
		  }
		else 
		  {
		    if ((move_paren) && (text[i] == ' '))
		      {
			new_text[j++] = '(';
			move_paren = false;
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
  XEN value = XEN_FALSE, sym = XEN_FALSE;
  if (XEN_EQ_P(text, XEN_UNDEFINED))                              /* if no arg, describe snd-help */
    {
      help_text = C_TO_XEN_STRING(H_snd_help);
      subject = H_snd_help;
    }
  else
    {
      if (XEN_SYMBOL_P(text)) 
	{
	  help_text = XEN_OBJECT_HELP(text);
	  sym = text;
	  subject = XEN_SYMBOL_TO_C_STRING(text);
	}
      else 
	{
	  if (XEN_STRING_P(text))
	    {
	      subject = XEN_TO_C_STRING(text);
	      sym = C_STRING_TO_XEN_SYMBOL(subject);
	      help_text = XEN_OBJECT_HELP(sym);
	    }
	  else
	    {
	      value = text;
	      help_text = XEN_OBJECT_HELP(text);
	    }
	}
      if (XEN_FALSE_P(help_text))
	{
#if HAVE_SCM_C_DEFINE
	  XEN lookup;
	  if ((XEN_FALSE_P(value)) && (XEN_SYMBOL_P(sym)))
	    {
	      lookup = scm_sym2var(sym, scm_current_module_lookup_closure(), XEN_FALSE); /* don't define in current module! */
	      if (!(XEN_FALSE_P(lookup)))
		value = XEN_VARIABLE_REF(lookup);
	    }
#endif
	  help_text = XEN_OBJECT_HELP(value);         /* (object-property ...) */	      
	  if ((XEN_FALSE_P(help_text)) &&
	      (XEN_PROCEDURE_P(value)))
	    {
	      help_text = XEN_PROCEDURE_HELP(value);  /* (procedure-property ...) */
	      if (XEN_FALSE_P(help_text))
		help_text = XEN_PROCEDURE_SOURCE_HELP(value);      /* (procedure-documentation ...) -- this is the first line of source if string */
	    }
	}
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

void set_html_dir(char *new_dir)
{
  if (html_dir(ss)) FREE(html_dir(ss));
  set_html_dir_1(new_dir);
}

static XEN g_html_dir(void) 
{
  #define H_html_dir "(" S_html_dir "): location of Snd documentation"
  return(C_TO_XEN_STRING(html_dir(ss)));
}

static XEN g_set_html_dir(XEN val) 
{
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_html_dir, "a string");
  set_html_dir(copy_string(XEN_TO_C_STRING(val))); 
  return(val);
}

static XEN g_html_program(void) 
{
  #define H_html_program "(" S_html_program "): name of documentation reader (netscape, by default)"
  return(C_TO_XEN_STRING(html_program(ss)));
}

static XEN g_set_html_program(XEN val) 
{
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_html_program, "a string");
  if (html_program(ss)) FREE(html_program(ss));
  set_html_program(copy_string(XEN_TO_C_STRING(val))); 
  return(val);
}

static XEN g_snd_url(XEN name)
{
  #define H_snd_url "(" S_snd_url " name): url corresponding to 'name'"
  /* given Snd entity ('open-sound) as symbol or string return associated url */
  XEN_ASSERT_TYPE(XEN_STRING_P(name) || XEN_SYMBOL_P(name), name, XEN_ONLY_ARG, S_snd_url, "a string or symbol");
  if (XEN_STRING_P(name))
    return(C_TO_XEN_STRING(snd_url(XEN_TO_C_STRING(name))));
  return(C_TO_XEN_STRING(snd_url(XEN_SYMBOL_TO_C_STRING(name))));
}

static XEN g_help_dialog(XEN subject, XEN msg)
{
  widget_t w;
  #define H_help_dialog "(" S_help_dialog " subject message): start the Help window with subject and message"
  XEN_ASSERT_TYPE(XEN_STRING_P(subject), subject, XEN_ARG_1, S_help_dialog, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, XEN_ARG_2, S_help_dialog, "a string");
  w = snd_help(XEN_TO_C_STRING(subject), XEN_TO_C_STRING(msg), true);
  return(XEN_WRAP_WIDGET(w));
}


#ifdef XEN_ARGIFY_1
XEN_ARGIFY_2(g_listener_help_w, g_listener_help)
XEN_NARGIFY_0(g_html_dir_w, g_html_dir)
XEN_NARGIFY_1(g_set_html_dir_w, g_set_html_dir)
XEN_NARGIFY_0(g_html_program_w, g_html_program)
XEN_NARGIFY_1(g_set_html_program_w, g_set_html_program)
XEN_NARGIFY_1(g_snd_url_w, g_snd_url)
XEN_NARGIFY_2(g_help_dialog_w, g_help_dialog)
#else
#define g_listener_help_w g_listener_help
#define g_html_dir_w g_html_dir
#define g_set_html_dir_w g_set_html_dir
#define g_html_program_w g_html_program
#define g_set_html_program_w g_set_html_program
#define g_snd_url_w g_snd_url
#define g_help_dialog_w g_help_dialog
#endif

void g_init_help(void)
{
  XEN_DEFINE_PROCEDURE(S_snd_help, g_listener_help_w, 0, 2, 0, H_snd_help);
  XEN_DEFINE_PROCEDURE(S_snd_url, g_snd_url_w, 1, 0, 0, H_snd_url);
  XEN_DEFINE_PROCEDURE(S_help_dialog, g_help_dialog_w, 2, 0, 0, H_help_dialog);

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
