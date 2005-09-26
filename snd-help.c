#include "snd.h"
#include "sndlib-strings.h"
#include "clm-strings.h"

#if USE_MOTIF
  #include <X11/IntrinsicP.h>
  #if HAVE_XPM
    #include <X11/xpm.h>
  #endif
#endif

#if HAVE_GNU_LIBC_VERSION_H
  #include <gnu/libc-version.h>
#endif
#if (HAVE_GSL_GSL_VERSION_H) && (!(defined(GSL_VERSION)))
  #include <gsl/gsl_version.h>
#endif

#if HAVE_LADSPA && HAVE_DLFCN_H && HAVE_DIRENT_H
  #include <ladspa.h>
#endif

#if HAVE_FFTW3
  #include <fftw3.h>
#else
  #if HAVE_FFTW
    #include <fftw.h>
  #endif
#endif

static char **snd_xrefs(const char *topic);
static char **snd_xref_urls(const char *topic);

static char **snd_itoa_strs = NULL;
static int snd_itoa_ctr = 0, snd_itoa_size = 0;
static char *snd_itoa(int n)
{
  char *str;
  if (snd_itoa_strs == NULL)
    {
      snd_itoa_size = 32;
      snd_itoa_strs = (char **)CALLOC(snd_itoa_size, sizeof(char *));
    }
  else
    {
      if (snd_itoa_ctr >= snd_itoa_size)
	{
	  int i;
	  snd_itoa_size += 32;
	  snd_itoa_strs = (char **)REALLOC(snd_itoa_strs, snd_itoa_size * sizeof(char *));
	  for (i = snd_itoa_ctr; i < snd_itoa_size; i++) snd_itoa_strs[i] = NULL;
	}
    }
  str = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
  mus_snprintf(str, LABEL_BUFFER_SIZE, "%d", n);
  snd_itoa_strs[snd_itoa_ctr++] = str;
  return(str);
}

static void free_snd_itoa(void)
{
  int i;
  for (i = 0; i < snd_itoa_ctr; i++)
    if (snd_itoa_strs[i]) 
      {
	FREE(snd_itoa_strs[i]);
	snd_itoa_strs[i] = NULL;
      }
  snd_itoa_ctr = 0;
}

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
	if (snd_itoa_ctr < snd_itoa_size) snd_itoa_strs[snd_itoa_ctr++] = buf;
	return(buf);
      }
#endif  
  return("");
}

static char* vstrcat(char *arg1, ...)
{
  char *buf;
  va_list ap;
  int len = 0;
  char *str;
  len = strlen(arg1);
  va_start(ap, arg1);
  while ((str = va_arg(ap, char *)))
    len += strlen(str);
  va_end(ap);
  buf = (char *)CALLOC(len + 32, sizeof(char));
  strcat(buf, arg1);
  va_start(ap, arg1);
  while ((str = va_arg(ap, char *)))
    strcat(buf, str);
  va_end(ap);
  return(buf);
}

static char *main_snd_xrefs[12] = {
  "{CLM}: sound synthesis",
  "{CM}: algorithmic composition",
  "{CMN}: music notation",
  "{Ruby}: extension language",
  "{Emacs}: Snd as Emacs subjob",
  "{Libxm}: graphics module",
  "{Sndlib}: underlying sound support library",
  "{Scripting}: Snd as scripting language",
  "{Motif}: Motif extensions via Libxm",
  "{Gtk}: Gtk extensions via Libxm",
  "{Ladspa}: plugins",
  NULL
};

static char *main_snd_xref_urls[12] = {
  "grfsnd.html#sndwithclm",
  "grfsnd.html#sndwithcm",
  "sndscm.html#musglyphs",
  "grfsnd.html#sndandruby",
  "grfsnd.html#emacssnd",
  "libxm.html#xm",
  "sndlib.html#introduction",
  "grfsnd.html#sndwithnogui",
  "grfsnd.html#sndwithmotif",
  "grfsnd.html#sndwithgtk",  
  "grfsnd.html#sndandladspa",
  NULL,
};

static void main_snd_help(const char *subject, ...)
{
  va_list ap;
  char *helpstr;
  snd_help_with_xrefs(subject, "", WITHOUT_WORD_WRAP, main_snd_xrefs, main_snd_xref_urls);
  va_start(ap, subject);
  while ((helpstr = va_arg(ap, char *))) snd_help_append(helpstr);
  va_end(ap);
  snd_help_back_to_top();
}  

static char *xm_version(void)
{
  XEN xm_val = XEN_FALSE;
#if HAVE_SCHEME
  #if USE_MOTIF
    xm_val = XEN_EVAL_C_STRING("(and (defined? 'xm-version) xm-version)");
  #else
    #if USE_GTK
      xm_val = XEN_EVAL_C_STRING("(and (defined? 'xg-version) xg-version)");
    #endif
  #endif
#endif
#if HAVE_RUBY
  #if USE_MOTIF
    if (rb_const_defined(rb_cObject, rb_intern("Xm_Version")))
      xm_val = XEN_EVAL_C_STRING("Xm_Version");
  #else
    #if USE_GTK
      if (rb_const_defined(rb_cObject, rb_intern("Xg_Version")))
        xm_val = XEN_EVAL_C_STRING("Xg_Version");
    #endif
  #endif
#endif
  if (XEN_STRING_P(xm_val))
    {
      char *version = NULL;
      version = (char *)CALLOC(32, sizeof(char));
      mus_snprintf(version, 32, "\n    %s: %s", 
#if USE_MOTIF
		   "xm",
#else
		   "xg",
#endif
		   XEN_TO_C_STRING(xm_val));
      if (snd_itoa_ctr < snd_itoa_size) snd_itoa_strs[snd_itoa_ctr++] = version;
      return(version);
    }
  return("");
}

#if HAVE_GL
static char *gl_version(void)
{
  XEN gl_val = XEN_FALSE;
#if HAVE_SCHEME
  gl_val = XEN_EVAL_C_STRING("(and (provided? 'gl) gl-version)");
#endif
#if HAVE_RUBY
  if (rb_const_defined(rb_cObject, rb_intern("Gl_Version")))
    gl_val = XEN_EVAL_C_STRING("Gl_Version");
#endif
  if (XEN_STRING_P(gl_val))
    {
      char *version = NULL;
      version = (char *)CALLOC(32, sizeof(char));
      mus_snprintf(version, 32, "\n    gl: %s", XEN_TO_C_STRING(gl_val));
      if (snd_itoa_ctr < snd_itoa_size) snd_itoa_strs[snd_itoa_ctr++] = version;
      return(version);
    }
  return("");
}

static char *glx_version(void)
{
  int major = 0, minor = 0;
  char *version;
  if (ss->sgx == NULL) return(""); /* snd --help for example */
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
  if (snd_itoa_ctr < snd_itoa_size) snd_itoa_strs[snd_itoa_ctr++] = version;
  return(version);
}
#endif

char *version_info(void)
{
  char *result, *xversion = NULL;
  snd_itoa_ctr = 0;
  xversion = xen_version();
  result = vstrcat(
	  _("This is Snd version "),
	  SND_VERSION,
	  " of ",
	  SND_DATE,
	  ":\n    ", xversion,
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
  #ifdef MUS_GSL_VERSION
          " ", MUS_GSL_VERSION,
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
  #ifdef MUS_PANGO_VERSION
	  ", Pango ", MUS_PANGO_VERSION,
  #endif
  #ifdef MUS_ATK_VERSION
	  ", Atk ", MUS_ATK_VERSION,
  #endif
  #ifdef MUS_CAIRO_VERSION
	  ", Cairo ", MUS_CAIRO_VERSION,
  #endif
	  /* would be nice, I suppose, to give the other infinite dependencies here (fontconfig...) */
#endif
	  xm_version(), /* omitted if --version/--help because the init procs haven't run at that point */
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
#if HAVE_LADSPA && HAVE_DLFCN_H && HAVE_DIRENT_H
	  "\n    LADSPA",
  #ifdef LADSPA_HINT_DEFAULT_MASK
	  " 1.1",
  #else
	  " 1.0",
  #endif
#endif
#ifdef MUS_JACK_VERSION
	  ", Jack: ", MUS_JACK_VERSION,
#endif
#if HAVE_FAM
  #ifdef MUS_GAMIN_VERSION
	  "\n    Gamin: ", MUS_GAMIN_VERSION,
  #else
	  "\n    with fam",
  #endif
#endif
#if WITH_THREADS
	  "\n    with threads",
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
	  "\n", 
#ifdef CONFIGURE_PROG
	  "\n    configured via: ", CONFIGURE_PROG, " ", CONFIGURE_ARGS,
	  "\n",
#endif
	  NULL);
  free_snd_itoa();
  if (xversion) free(xversion); /* calloc in xen.c */
  return(result);
}

void about_snd_help(void)
{
  char *info = NULL, *features = NULL;
#if HAVE_GUILE
  char *files = NULL;
  features = word_wrap(XEN_AS_STRING(XEN_EVAL_C_STRING("*features*")), 400);
  files = word_wrap(XEN_AS_STRING(XEN_EVAL_C_STRING("*snd-loaded-files*")), 400);
#endif
#if HAVE_RUBY
  features = word_wrap(XEN_AS_STRING(XEN_EVAL_C_STRING("$\".join(' ')")), 400);
#endif
  info = version_info();
  main_snd_help("Snd is a sound editor.",
	    info,
	    "\nRecent changes include:\n\
\n\
26-Sep:  save-region-dialog, more buttons.\n\
15-Sep:  --enable-threads configure switch, and first use thereof (FIR filtering).\n\
6-Sep:   snd-error is now (throw 'snd-error ...).\n\
31-Aug:  mix|track-speed-style.\n\
         changed recorder-in|out-format -> recorder-in|out-data-format,\n\
                 recorder-out-type -> recorder-out-header-type\n\
28-Aug:  snd 7.15.\n\
",
#if HAVE_GUILE
	    "\n    *features*: \n'", features, "\n\n",
            "\n    loaded files: ", files, "\n\n",
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
#if HAVE_GUILE
  if (files) FREE(files);
#endif
}


/* ---------------- help menu help texts ---------------- */

void find_help(void) 
{
  snd_help_with_xrefs("Find", 
#if HAVE_EXTENSION_LANGUAGE
"Searches in Snd refer to the sound data, and are, in general, patterned after Emacs.  When you type \
C-s or C-r, the minibuffer below the graph is activated and you are asked for the search expression. \
The expression is a function that takes one argument, the current sample value, and returns #t when it finds a match. \
To look for the next sample that is greater than .1, (lambda (y) (> y .1)).  The cursor then moves \
to the next such sample, if any. Successive C-s's or C-r's repeat the search.  C-x C-s can redefine the \
search pattern, which is also cleared by various other commands, much as in Emacs. \
\n\n\
Normally, the search applies only to the current channel. To search all current files at once, use the Edit:Find dialog.",
#else
"Searches in Snd depend completely on either Guile or Ruby.  Since neither is loaded,\
the searching mechanisms are disabled.",
#endif
		      WITH_WORD_WRAP,
		      snd_xrefs("Search"),
		      snd_xref_urls("Search"));
}

/* TODO: key command help lists omitted if rebound */

void undo_help(void) 
{
  snd_help_with_xrefs("Undo and Redo", 
"Snd supports 'unlimited undo' in the sense that you can move back and forth in the list of edits without any \
limit on how long that list can get.  The data displayed is always the edited form thereof.  Each editing operation \
extends the current edit list; each undo backs up in that list, and each redo moves forward in the list of previously \
un-done edits.  Besides the Edit and Popup menu options, and the " S_undo " and " S_redo " functions, \
there are these keyboard sequences: \
\n\n\
  C-x r     redo last edit\n\
  C-x u     undo last edit\n\
  C-x C-r   redo last edit\n\
  C-x C-u   undo last edit\n\
  C-_       undo last edit\n\
\n\
Revert is the same as undo all edits.\n\n\
In the listener, C-M-g deletes all text, and C-_ deletes back to the previous command.",
		      WITH_WORD_WRAP,
		      snd_xrefs("Undo"),
		      snd_xref_urls("Undo"));
}

static char *sync_xrefs[4] = {
  "sound sync field: {" S_sync "}",
  "mark sync field: {" S_mark_sync "}, {" S_mark_sync_max "}, {mark-sync-color}, {" S_syncd_marks "}",
  "mix sync (track) field: {" S_mix_track "}",
  NULL};

void sync_help(void) 
{
  snd_help_with_xrefs("Sync", 
"The sync button causes certain operations to apply to all channels or multiple sounds simultaneously. \
For example, to get a multichannel selection, set the sync button, then define the selection (by dragging \
the mouse) in one channel, and the parallel portions of the other channels will also be selected. \
Marks and mixes can also be sync'd together.",
		      WITH_WORD_WRAP,
		      sync_xrefs,
		      NULL);
}

static char *debug_xrefs[8] = {
  "C debugging: {gdb}",
  "Scheme/Ruby debugging: {snd-debug}",
  "CLM Instrument debugging: {variable-display}",
  "Notelist debugging: {ws-backtrace}",
  "Break and trace points: {snd-break}",
  "Error handling",
  "Print statement: {" S_snd_print "}",
  NULL};

static char *debug_urls[8] = {
  "extsnd.html#cdebugging",
  "sndscm.html#debugdoc",
  "sndscm.html#variabledisplay",
  "sndscm.html#wsdebug",
  "sndscm.html#debugdoc",
  "extsnd.html#snderrors",
  "extsnd.html#sndprint",
  NULL};

void debug_help(void)
{
  snd_help_with_xrefs("Debugging", 
"There are several sets of debugging aids, each aimed at a different level of code. \
C code is normally debugged with gdb.  If you hit a segfault in Snd, please tell me \
about it!  If possible, run Snd in gdb and send me the stack trace: \n\n\
  gdb snd\n\
  run\n\
  <get error to happen>\n\
  where\n\
\n\
See README.Snd for more about C-level troubles.  For CLM-based instruments, \
variable-display in snd-motif.scm might help.  For debugging your own Scheme/Ruby \
code (or Snd's for that matter), see the \"Errors and Debugging\" section of \
extsnd.html, or snd-debug.  For notelist debugging, see ws-backtrace.",
		      WITH_WORD_WRAP,
		      debug_xrefs,
		      debug_urls);
}

void env_help(void) 
{
  snd_help_with_xrefs("Envelope", 
"An envelope in Snd is a list (array in Ruby) of x y break-point pairs. The x axis range is arbitrary. To define a triangle curve: '(0 0 1 1 2 0). \
There is no preset limit on the number of breakpoints. Use the envelope editor to draw envelopes with the mouse. \
\n\n\
To apply an envelope to a sound, use " S_env_sound " or the extended command C-x C-a.  If this command gets a numeric \
argument, the envelope is applied from the cursor for that many samples. Otherwise, the envelope is \
applied to the entire file. \
\n\n\
  C-x a     apply amplitude envelope to selection\n\
  C-x C-a   apply amplitude envelope to channel",
		      WITH_WORD_WRAP,
		      snd_xrefs("Envelope"),
		      snd_xref_urls("Envelope"));
}

void fft_help(void)
{
  snd_help_with_xrefs("FFT",
"The FFT performs a projection of the time domain into the frequency domain. Good discussions of the Fourier Transform \
and the trick used in the FFT itself can be found in many DSP books; those I know of include 'A Digital Signal Processing \
Primer', Ken Steiglitz, or 'Numerical Recipes in C'. \
For the Fourier transform itself, good books abound: \
'Mathematics of the DFT', Julius Smith, or 'Understanding DSP', Lyons, etc.  For more sophistication, \
'Fourier Analysis', Korner, 'Fourier Analysis', Stein and Shakarchi, or 'Trigonometric Series', Zygmund. \
\n\n\
The FFT size can be any power of 2. The larger, the longer it takes to compute, and the larger the amount of the time domain \
that gets consumed.  Interpretation of the FFT results is not straightforward!  The window choices are taken primarily \
from Harris' article: Fredric J. Harris, 'On the Use of Windows for Harmonic Analysis with the Discrete Fourier Transform', Proceedings of the \
IEEE, Vol. 66, No. 1, January 1978, with updates from: Albert H. Nuttall, 'Some Windows with Very Good Sidelobe Behaviour', IEEE Transactions \
of Acoustics, Speech, and Signal Processing, Vol. ASSP-29, 1, February 1981. \
\n\n\
Nearly all the transform-related choices are set by the transform dialog launched from the Options \
Menu Transform item. Most of this dialog should be self-explanatory.  Some of the windows take an \
additional parameter sometimes known as alpha or beta.  This can be set by the scale next to the fft window graph in the \
transform dialog, or via " S_fft_window_beta ". \
\n\n\
The FFT display is activated by setting the 'f' button on the channel's window.  It then updates \
itself each time the time domain waveform moves or changes.  \
The spectrum data is usually normalized to fit between 0.0 to 1.0; if you'd rather have un-normalized \
data (the y-axis in this case changes to reflect the data values, to some extent), set the \
variable " S_transform_normalization " to " S_dont_normalize ".",
		      WITH_WORD_WRAP,
		      snd_xrefs("FFT"),
		      snd_xref_urls("FFT"));
}

static char *control_xrefs[9] = {
  "various control panel variables: {Control panel}",
  "amplitude: {" S_scale_by "}",
  "speed or srate: {" S_src_sound "}",
  "expand: {granulate}",
  "contrast: {" S_contrast_enhancement "}",
  "filter: {" S_filter_sound "}",
  "{" S_apply_controls "}",
  "{" S_controls_to_channel "}",
  NULL};

void controls_help(void) 
{
  snd_help_with_xrefs("The Control Panel", 
"The control panel is the portion of each sound's pane beneath the channel graphs. \
The controls are: amp, speed, expand, contrast, reverb, and filter. \
\n\n\
'Speed' here refers to the rate at which the sound data is consumed during playback. \
Another term might be 'srate'.  The arrow button on the right determines \
the direction Snd moves through the data. The scroll bar position is normally interpreted \
as a float between .05 and 20. \
\n\n\
'Expand' refers to granular synthesis used to change the tempo of events \
in the sound without changing pitch.  Successive short slices of the file are overlapped with \
the difference in size between the input and output hops (between successive slices) giving \
the change in tempo.  This doesn't work in all files -- it sometimes sounds like execrable reverb \
or is too buzzy -- but it certainly is more robust than the phase vocoder approach to the \
same problem. The expander is on only if the expand button is set. \
\n\n\
The reverberator is a version of Michael McNabb's Nrev.  In addition to the controls \
in the control pane, you can set the reverb feedback gain and the coefficient of the lowpass \
filter in the allpass bank. The reverb is on only if the reverb button is set.  The reverb length \
field takes effect only when the reverb is set up (when the DAC is started by clicking \
'play' when nothing else is being played). \
\n\n\
'Contrast enhancement' is my name for a somewhat weird waveshaper or compander.  It \
phase-modulates a sound, which can in some cases make it sound sharper or brighter. \
For softer sounds, it causes only an amplitude change.  To scale a soft sound up before \
being 'contrasted', use the variable " S_contrast_control_amp ".  Contrast is on only if the contrast button is set. \
\n\n\
The filter is an arbitrary (even) order FIR filter specified by giving the frequency response \
envelope and filter order in the text windows provided. \
The envelope X axis goes from 0 to half the sampling rate. The actual frequency response (given the current filter order) \
is displayed in blue.  The filter is on only if the filter button is set. \
\n\n\
The keyboard commands associated with the control panel are: \
\n\n\
  C-x C-o   show control panel\n\
  C-x C-c   hide control panel",
		      WITH_WORD_WRAP,
		      control_xrefs,
		      NULL);
  global_control_panel_state();
}

void marks_help(void) 
{
  snd_help_with_xrefs("Marks", 
"A 'mark' marks a particular sample in a sound (not a position in that sound). \
If we mark a sample, then delete 100 samples before it, the mark follows the sample, changing its current position \
in the data.  If we delete the sample, the mark is also deleted; a subsequent undo that returns the sample also \
returns its associated mark.  I'm not sure this is the right thing, but it's a lot less stupid than marking \
a position. \
\n\n\
Once set, a mark can be moved by dragging the horizontal tab at the top.  Control-click of the tab followed by mouse \
drag will drag the underlying data too, either inserting zeros or deleting data. \
Click on the triangle at the bottom to play (or stop playing) from the mark; drag the \
triangle to play following the mouse. \
\n\n\
A mark can be named or unnamed.  It it has a name, it is displayed above the horizontal tab at the top of the window. As with \
sounds and mixes, marks can be grouped together through the sync field; marks sharing the same sync value (other \
than 0) will move together when one is moved, and so on.  The following keyboard commands relate to marks: \
\n\n\
  C-m       place (or remove if argument negative) mark at cursor\n\
  C-M       place syncd marks at all currently syncd chan cursors\n\
  C-x /     place named mark at cursor\n\
  C-x C-m   add named mark\n\
  C-j       go to mark\n\
  C-x j     go to named mark",
		      WITH_WORD_WRAP, 
		      snd_xrefs("Mark"),
		      snd_xref_urls("Mark"));
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
usually handier, however, to adjust the mix via the Mix dialog. \
\n\n\
The Mix dialog (under the View Menu) provides various \
commonly-used controls on the currently chosen mix. At the top are the mix id, \
name, begin and end times, track number, and a play button. Beneath that are \
various sliders controlling the speed (sampling rate) of the mix, amplitude of \
each input channel, and the amplitude envelopes. \
\n\n\
To move the cursor from one mix to the next, in the same manner as C-j moves through marks, use C-x C-j. \
\n\n\
A set of associated mixes is called a 'track' in Snd, and there's a help menu item for that subject.",
		      WITH_WORD_WRAP, 
		      snd_xrefs("Mix"),
		      snd_xref_urls("Mix"));
}

void track_help(void) 
{
  snd_help_with_xrefs("Tracks",
"A track is a list of mixes, each member mix having its track set to the track id.  The " S_make_track " \
function takes the initial mixes, returning the track id (an integer).  The \
track function returns the list of mixes that are members of the given track.  The rest of the track functions \
take the track id as their initial argument.  A track has much the same structure as a mix: an amplitude, speed, \
amplitude envelope, track, position, and so on.  If its track field is not 0, the entire track is a member \
of the given track, just as a mix would be.",
		      WITH_WORD_WRAP,
		      snd_xrefs("Track"),
		      snd_xref_urls("Track"));
}

static char *record_xrefs[4] = {
  "recorder variables: {" S_recorder_gain "}, etc",
  "low-level ADC input: {" S_mus_audio_open_input "}",
  "process incoming sound: rtio.scm",
  NULL};

void recording_help(void) 
{
  snd_help_with_xrefs("Record", 
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
(monitor) rate doesn't match the input rate.",
		      WITH_WORD_WRAP,
		      record_xrefs,
		      NULL);
}

static char *header_and_data_xrefs[10] = {
  "data format discussion: {" S_data_format "}",
  "data format constants: {" S_mus_data_format_name "}",
  "header type discussion: {" S_header_type "}",
  "header type constants: {" S_mus_header_type_name "}",
  "MPEG support: mpg in examp.scm",
  "OGG support: read-ogg in examp.scm",
  "Speex support: read-speex in examp.scm",
  "Flac support: read-flac in examp.scm",
  "{Sndlib}: underlying support",
  NULL};

static char *header_and_data_urls[10] = {
  "extsnd.html#snddataformat",
  "extsnd.html#soundformatname",
  "extsnd.html#sndheadertype",
  "extsnd.html#soundtypename",
  "sndscm.html#exmpg",
  "sndscm.html#exmpg",
  "sndscm.html#exmpg",
  "sndscm.html#exmpg",
  "sndlib.html#introduction",
  NULL};

void sound_files_help(void) 
{
  snd_help_with_xrefs("Headers and Data", 
"Snd can handle the following file and data types: \
\n\n\
  read/write (many data formats):\n\
    NeXT/Sun/DEC/AFsp\n\
    AIFF/AIFC\n\
    RIFF (Microsoft wave)\n\
    IRCAM (old style)\n\
    NIST-sphere\n\
    no header ('raw')\n\
\n\n\
  read-only (in selected data formats):\n\
    8SVX (IFF), EBICSF, INRS, ESPS, SPPACK, ADC (OGI), AVR, VOC, PVF,\n\
    Sound Tools, Turtle Beach SMP, SoundFont 2.0, Sound Designer I, PSION, MAUD, Kurzweil 2000,\n\
    Gravis Ultrasound, ASF, PAF, CSL, Comdisco SPW, Goldwave sample, omf, quicktime\n\
    Sonic Foundry, SBStudio II, Delusion digital, Digiplayer ST3, Farandole Composer WaveSample,\n\
    Ultratracker WaveSample, Sample Dump exchange, Yamaha SY85, SY99, and TX16, Covox v8, AVI, \n\
    Impulse tracker, Korg, Akai, Turtle Beach\n\
\n\n\
  automatically translated to a readable format:\n\
    IEEE text, Mus10, SAM 16-bit (modes 1 and 4), AVI\n\
    NIST shortpack, HCOM, Intel, IBM, and Oki (Dialogic) ADPCM,\n\
    G721, G723_24, G723_40, MIDI sample dump\n\
\n\n\
The files can have any number of channels. Data can be either big or little endian. \
The file types listed above as 'automatically translated' are \
decoded upon being opened, translated to some format Snd can read and write, \
and rewritten as a new file with an added (possibly redundant) extension .snd, \
and that file is the one the editor sees from then on.",
		      WITH_WORD_WRAP,
		      header_and_data_xrefs,
		      header_and_data_urls);
}

static char *init_file_xrefs[6] = {
  "{X resources}:  .Xdefaults settings",
  "{Invocation flags}", 
  "~/.snd: {Initialization file}",
  "{Customization}",
  "examples of ~/.snd",
  NULL};

static char *init_file_urls[6] = {
  "grfsnd.html#sndresources",
  "grfsnd.html#sndswitches",
  "grfsnd.html#sndinitfile",
  "extsnd.html#lisplistener",
  "grfsnd.html#sndinitfile",
  NULL};

void init_file_help(void) 
{
  snd_help_with_xrefs("Customization",
#if HAVE_EXTENSION_LANGUAGE
"Nearly everything in Snd can be set in an initialization file, loaded at any time from a saved-state file, specified \
via inter-process communciation from any other program, invoked via M-x in the minibuffer, imbedded in a keyboard macro, or  \
dealt with from the lisp listener panel. I've tried to bring out to lisp nearly every portion of Snd, \
both the signal-processing functions, and much of the user interface. You can, for example, add your own menu choices, \
editing operations, or graphing alternatives. These extensions can be loaded at any time.",
#else
"Snd depends heavily on either Guile or Ruby to provide much of its functionality.  Since neither \
is loaded, there's not much customization you can do.  Check out the X resource stuff in Snd.ad or \
the gtk resource stuff in Snd.gtkrc.",
#endif
		      WITH_WORD_WRAP,
		      init_file_xrefs,
		      init_file_urls);
}

static char *key_xrefs[4] = {
  "To change a key binding: {" S_bind_key "}",
  "To undefine a key: {" S_unbind_key "}",
  "Current key binding: {" S_key_binding "}",
  NULL};

static void show_key_help(int key, int state, bool cx, char *help)
{
  if (help)
    {
      char buf[1024];
      char cbuf[256];
      make_key_name(cbuf, 256, key, state, cx);
      mus_snprintf(buf, 1024, "\n%s: %s", cbuf, help);
      snd_help_append(buf);
    }
}

static bool find_unbuckified_keys(int key, int state, bool cx, XEN func)
{
  if ((key > 256) && (state == 0) && (!cx) && (XEN_BOUND_P(func)))
    show_key_help(key, state, cx, key_binding_description(key, state, cx));
  return(false);
}

static bool find_buckified_keys(int key, int state, bool cx, XEN func)
{
  if ((key > 256) && (state == snd_ControlMask) && (!cx) && (XEN_BOUND_P(func)))
    show_key_help(key, state, cx, key_binding_description(key, state, cx));
  return(false);
}

static bool find_unbuckified_cx_keys(int key, int state, bool cx, XEN func)
{
  if ((key > 256) && (state == 0) && (cx) && (XEN_BOUND_P(func)))
    show_key_help(key, state, cx, key_binding_description(key, state, cx));
  return(false);
}

static bool find_buckified_cx_keys(int key, int state, bool cx, XEN func)
{
  if ((key > 256) && (state == snd_ControlMask) && (cx) && (XEN_BOUND_P(func)))
    show_key_help(key, state, cx, key_binding_description(key, state, cx));
  return(false);
}

static bool find_leftover_keys(int key, int state, bool cx, XEN func)
{
  if ((key > 256) && (state & snd_MetaMask))
    show_key_help(key, state, cx, key_binding_description(key, state, cx));
  return(false);
}

void key_binding_help(void)
{
  int i;
  snd_help_with_xrefs("Key bindings",
		      "",
		      WITHOUT_WORD_WRAP,
		      key_xrefs,
		      NULL);
  /* run through bindings straight, C-key, C-x key, C-x C-key appending description in help dialog */
  for (i = 0; i < 256; i++)
    show_key_help(i, 0, false, key_binding_description(i, 0, false));
  map_over_key_bindings(find_unbuckified_keys);
  for (i = 0; i < 256; i++)
    show_key_help(i, snd_ControlMask, false, key_binding_description(i, snd_ControlMask, false));
  map_over_key_bindings(find_buckified_keys);
  for (i = 0; i < 256; i++)
    show_key_help(i, snd_MetaMask, false, key_binding_description(i, snd_MetaMask, false));
  for (i = 0; i < 256; i++)
    show_key_help(i, snd_ControlMask | snd_MetaMask, false, key_binding_description(i, snd_ControlMask | snd_MetaMask, false));
  for (i = 0; i < 256; i++)
    show_key_help(i, 0, true, key_binding_description(i, 0, true));
  map_over_key_bindings(find_unbuckified_cx_keys);
  for (i = 0; i < 256; i++)
    show_key_help(i, snd_ControlMask, true, key_binding_description(i, snd_ControlMask, true));
  map_over_key_bindings(find_buckified_cx_keys);
  map_over_key_bindings(find_leftover_keys);
  snd_help_back_to_top();
}

void play_help(void)
{
  snd_help_with_xrefs("Play",
"To play a sound, click the 'play' button.  If the sound has more channels than your DAC(s), Snd will (normally) try to mix the extra channels \
into the available DAC outputs.  While it is playing, you can click the button again to stop it, or click some other \
file's 'play' button to mix it into the current set of sounds being played. To play from a particular point, set a mark \
there, then click its 'play triangle' (the triangular portion below the x axis).  (Use control-click here to play all channels \
from the mark point). To play simultaneously from an arbitrary group of start points (possibly spread among many sounds), \
set syncd marks at the start points, then click the play triangle of one of them. \
\n\n\
The Edit menu 'Play' option plays the current selection, if any.  The Popup menu's 'Play' option plays the \
currently selected sound.  And the region and file browsers provide play buttons for each of the listed regions or files.  If you \
hold down the control key when you click 'play', the cursor follows along as the sound is played.   \
\n\n\
In a multichannel file, C-q plays all channels from the current channel's \
cursor if the sync button is on, and otherwise plays only the current channel. \
Except in the browsers, what is actually played depends on the control panel.",
		      WITH_WORD_WRAP,
		      snd_xrefs("Play"),
		      snd_xref_urls("Play"));
}

void reverb_help(void)
{
  snd_help_with_xrefs("Reverb",
"The reverb in the control panel is a version of Michael McNabb's Nrev.  There are other \
reverbs mentioned in the related topics list.",
		      WITH_WORD_WRAP,
		      snd_xrefs("Reverb"),
		      snd_xref_urls("Reverb"));
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
If you want Snd to ask before overwriting a file in any case, set the variable " S_ask_before_overwrite " to \
#t in your Snd initialization file.",
		      WITH_WORD_WRAP,
		      snd_xrefs("Save"),
		      snd_xref_urls("Save"));
}

void filter_help(void)
{
  snd_help_with_xrefs("Filter",
"There is an FIR Filter in the control panel, and a variety of other filters scattered around; \
see dsp.scm in particular.",
		      WITH_WORD_WRAP,
		      snd_xrefs("Filter"),
		      snd_xref_urls("Filter"));
}

void resample_help(void)
{
  snd_help_with_xrefs("Resample",
"There is a sampling rate changer in the control panel; see the related topics list below.",
		      WITH_WORD_WRAP,
		      snd_xrefs("Resample"),
		      snd_xref_urls("Resample"));
}

void insert_help(void)
{
  snd_help_with_xrefs("Insert",
"To insert a file, use C-x C-i, and to insert the selection C-x i.  C-o inserts a \
zero sample at the cursor",
		      WITH_WORD_WRAP,
		      snd_xrefs("Insert"),
		      snd_xref_urls("Insert"));
}

void delete_help(void)
{
  snd_help_with_xrefs("Delete",
"To delete a sample, use C-d; to delete the selection, C-w",
		      WITH_WORD_WRAP,
		      snd_xrefs("Delete"),
		      snd_xref_urls("Delete"));
}


/* -------- dialog help texts -------- */

void envelope_editor_dialog_help(void)
{
  snd_help_with_xrefs("Envelope Editor",
"The Edit Envelope dialog (under the Edit menu) opens a window for viewing and editing envelopes. \
The dialog has a display showing either the envelope currently being edited or \
a panorama of all currently loaded envelopes.  The current envelope can be edited with the mouse: click at some spot in the graph to place a \
new breakpoint, drag an existing breakpoint to change its position, and click an existing breakpoint to delete it. \
The Undo and Redo buttons can be used to move around in the list of envelope edits; the current state \
of the envelope can be saved with the 'save' button, or printed with 'print'. Envelopes can be defined using defvar: \
\n\n\
  (defvar ramp '(0 0 1 1))\n\
  (defvar pyramid '(0 0 1 1 2 0))\n\
\n\n\
defines two envelopes that can be used in Snd wherever an envelope is needed (e.g. C-x C-a).  You can also define \
a new envelope in the dialog's text field; '(0 0 1 1) followed by return creates a ramp as a new envelope. \
\n\n\
In the overall view of envelopes, click an envelope, or click its name in the scrolled \
list on the left to select it; click the selected envelope to load it into the editor portion, clearing out whatever \
was previously there.  To load an existing envelope into the editor, you can also type its name in the text field; to give a name to \
the envelope as it is currently defined in the graph viewer, type its name in this field, then \
either push return or the 'save' button. \
\n\n\
Once you have an envelope in the editor, you can apply it to the current sounds with the 'Apply' or 'Undo&amp;Apply' buttons; the latter \
first tries to undo the previous edit, then applies the envelope. The envelope can be applied to \
the amplitude, the spectrum, or the sampling rate. The choice is made via the three buttons marked 'amp', \
'flt', and 'src'. The filter order is the variable " S_enved_filter_order " which defaults to 40. To use fft-filtering (convolution) \
instead, click the 'fir' button, changing its label to 'fft'. If you are displaying the fft graph of the current channel, \
and the fft is large enough to include the entire sound, and the 'wave' button is set, \
the spectrum is also displayed in the envelope editor, making it easier to perform accurate (fussy?) filtering operations. \
\n\n\
To apply the envelope to the current selection, rather than the current sound, set the 'selection' button. \
To apply it to the currently selected mix, set the 'mix' button. In this case, if the mix has multiple input channels, the envelope \
is applied to the currently selected channel (the one with the red label); control-click 'mix' to load the current mix amplitude \
envelope into the editor. \
\n\n\
The two toggle buttons at the lower right choose whether to show a light-colored version of \
the currently active sound (the 'wave' button), and whether to clip mouse movement at the current y axis bounds (the \
'clip' button).  The 'linear' and 'exp' buttons choose the type of connecting lines, and the 'exp base' slider at \
the bottom sets the 'base' of the exponential curves, just as in CLM.  If the envelope is being treated as a spectrum ('flt' \
is selected), the 'wave' button shows the actual frequency response of the filter that will be applied to the waveform \
by the 'apply' buttons.  Increase the " S_enved_filter_order " to \
improve the fit.  In this case, the X axis goes from 0 Hz to half the sampling rate, labelled as 1.0.",
		      WITH_WORD_WRAP,
		      snd_xrefs("Envelope"),
		      snd_xref_urls("Envelope"));
}

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
displayed, but the axis is 'draggable' -- put the mouse on the axis and drag it either way to change \
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
button makes a similar choice along the frequency axis. \
\n\n\
If you choose dB, the fft window graph (in the lower right) displays the window spectrum in dB, \
but the y axis labelling is still 0.0 to 1.0 to reflect the fact that the fft window itself is still displayed \
in linear terms.",
		      WITH_WORD_WRAP,
		      snd_xrefs("FFT"),
		      snd_xref_urls("FFT"));
}

static char *color_dialog_xrefs[9] = {
  "colormap variable: {colormap}",
  "colormap constants: rgb.scm",
  "colormap colors: {" S_colormap_ref "}",
  "color dialog variables: {" S_color_cutoff "}, {" S_color_inverted "}, {" S_color_scale "}",
  "specialize color dialog actions: {" S_color_hook "}",
  "start the color dialog: {" S_color_dialog "}",
  "add a new colormap: {" S_add_colormap "}",
  "remove a colormap: {" S_delete_colormap "}",
  NULL};

void color_dialog_help(void)
{
  snd_help_with_xrefs("View Color",
"This dialog sets the colormap and associated variables used during sonogram, spectrogram,  \
and perhaps wavogram display. The cutoff scale refers to the minimum data value to be displayed.",
		      WITH_WORD_WRAP,
		      color_dialog_xrefs,
		      NULL);
}

static char *orientation_dialog_xrefs[4] = {
  "orientation variables: {" S_spectro_x_scale "}, {" S_spectro_x_angle "}, etc",
  "start orientation dialog: {" S_orientation_dialog "}",
  "specialize orientation dialog actions: {" S_orientation_hook "}",
  NULL};

void orientation_dialog_help(void)
{
  snd_help_with_xrefs("View Orientation",
"This dialog sets the rotation and scaling variables used during sonogram, spectrogram, and wavogram display. \
The 'angle' scalers change the viewing angle, the 'scale' scalers change the 'stretch' amount \
along a given axis, 'hop' refers to the density of trace (the jump in samples between successive \
ffts), and 'percent of spectrum' is equivalent to dragging the fft frequency axis -- it changes \
the amount of the spectrum that is displayed.  If the 'use openGL' button is set, the \
spectrogram is drawn by openGL.",
		      WITH_WORD_WRAP,
		      orientation_dialog_xrefs,
		      NULL);
}

void region_dialog_help(void)
{
  snd_help_with_xrefs("Region Browser",
"This is the 'region browser'.  The scrolled window contains the list of current regions \
with a brief title to indicate the source thereof, and a button to play the region. \
One channel of the currently selected region is displayed in the graph window.  The up and \
down arrows move up or down in the region's list of channels.  If you click a region's \
title, that region is displayed in the graph area.  The 'print' button produces a Postscript \
rendition of the current graph contents, using the default eps output name. 'play' plays the region.  \
The 'edit' button loads the region into the main editor as a temporary file.  It can be edited or renamed, etc.  If you save \
the file, the region is updated to reflect any edits you made.",
		      WITH_WORD_WRAP,
		      snd_xrefs("Region"),
		      snd_xref_urls("Region"));
}

static char *raw_xrefs[7] = {
  "specialize handing of raw sounds: {" S_open_raw_sound_hook "}",
  "open a headerless sound: {" S_open_raw_sound "}",
  "header type constants: {" S_mus_header_type_name "}",
  "data format constants: {" S_mus_data_format_name "}",
  "what are these data formats?",
  "what are these headers?",
  NULL};

static char *raw_urls[7] = {
  NULL, NULL, NULL, NULL, 
  "extsnd.html#describedataformats",
  "extsnd.html#describeheadertypes",
  NULL
};


void raw_data_dialog_help(char *info)
{
  if (info)
    {
      snd_help_with_xrefs("Raw Data",
"This file seems to have an invalid header.  I'll append what sense I can make of it. \
To display and edit sound data, Snd needs to know how the data's sampling rate, number \
of channels, and numerical format.\n\n",
			  WITH_WORD_WRAP,
			  raw_xrefs,
			  raw_urls);
      snd_help_append(info);
    }
  else
    {
      snd_help_with_xrefs("Raw Data",
"To display and edit sound data, Snd needs to know how the data's sampling rate, number \
of channels, and numerical format.  This dialog gives you a chance to set those fields. \
To use the defaults, click the 'Reset' button.",
			  WITH_WORD_WRAP,
			  raw_xrefs,
			  NULL);
    }
}

/* PERHAPS completions embedded as pull down menu */
/*    this could be handled locally like errors -- local pulldown menu (with continuations etc)
 */

/*  appears to be Name_completion in snd-xlistener, so triggered via XtAction TAB (different from Listener_completion)
    add_completer:
    snd-xfile.c:    add_completer_to_textfield(wtmp, add_completer_func(filename_completer));
    snd-xfile.c:    add_completer_to_textfield(wtmp, add_completer_func(filename_completer));
    snd-xfile.c:  fdat->srate_text = make_textfield_widget("srate-text", form, args, n, NOT_ACTIVATABLE, add_completer_func(srate_completer));
    snd-xfile.c:      new_file_name = make_textfield_widget("newtext", form, args, n, ACTIVATABLE, add_completer_func(filename_completer));
    snd-xhelp.c:  help_search = make_textfield_widget("help-search", holder, args, n, ACTIVATABLE, add_completer_func(help_completer));
    snd-xsnd.c:      sw[W_info] = make_textfield_widget("snd-info", sw[W_name_form], args, n, ACTIVATABLE, add_completer_func(info_completer));
    snd-xsnd.c:      sw[W_filter] = make_textfield_widget("filter-text", sw[W_amp_form], args, n, ACTIVATABLE, add_completer_func(filename_completer));
 */

void completion_dialog_help(void)
{
  snd_help("completion",
	   "These are the completions that Snd thinks might be likely. If you select one, it will be used to complete the current name.",
	   WITH_WORD_WRAP);
}

void save_as_dialog_help(void)
{
  snd_help_with_xrefs("Save As",
"You can save the current state of a file with File:Save As, or the current selection with Edit:Save as. \
The output header type, data format, sampling rate, and comment can also be set.  Setting the srate \
does not affect the data -- it is just a number placed in the sound file header. \
If a file by the chosen name already exists \
it is overwritten, unless that file is already open in Snd and has edits.  In that case,  \
you'll be asked what to do.  If you want to be warned whenever a file is about to be overwritten by this \
option, set the variable " S_ask_before_overwrite " to #t. \
If you give the current file name to Save As,  \
any current edits will be saved and the current version in Snd will be updated (that is, in this \
case, the edit tree is not preserved).  To save (extract) just one channel of a multichannel file, \
put the (0-based) channel number in the 'extract channel' field, then click 'Extract', rather \
than 'Save'.",
		      WITH_WORD_WRAP,
		      snd_xrefs("Save"),
		      snd_xref_urls("Save"));
}

static char *open_file_xrefs[7] = {
  "open file: {open-sound}",
  "add to sound file extension list (for '" S_just_sounds "'): {" S_add_sound_file_extension "}",
  "specialize open: {" S_open_hook "}, {" S_after_open_hook "}, etc",
  "start the file dialog: {" S_open_file_dialog "}",
  "specialize file list: {" S_just_sounds_hook "}, {install-searcher} in snd-motif.scm",
  "keep dialog active after opening: {keep-file-dialog-open-upon-ok} in snd-motif.scm",
  NULL};

void open_file_dialog_help(void)
{
  snd_help_with_xrefs("Open File",
"The file will be opened in a new pane, if it's a sound file. \
If you click the 'Sound Files Only' button, only those files in the current directory that look vaguely like sound files will be displayed.",
		      WITH_WORD_WRAP,
		      open_file_xrefs,
		      NULL);
}

void mix_file_dialog_help(void)
{
  snd_help_with_xrefs("Mix File",
"The file will be mixed (added into) at the cursor in the selected sound. If you click the 'Sound Files Only' button, \
only those files in the current directory that look vaguely like sound files will be displayed.",
		      WITH_WORD_WRAP,
		      snd_xrefs("Mix"),
		      snd_xref_urls("Mix"));
}

void insert_file_dialog_help(void)
{
  snd_help_with_xrefs("Insert File",
"The file will be inserted (pasted in) at the cursor in the selected sound. If you click the 'Sound Files Only' button, \
only those files in the current directory that look vaguely like sound files will be displayed.",
		      WITH_WORD_WRAP,
		      snd_xrefs("Insert"),
		      snd_xref_urls("Insert"));
}

void find_dialog_help(void)
{
  snd_help_with_xrefs("Global Find",
"This search travels through all the current channels in parallel until a match is found.  The find \
expression is a function of one argument,  the current sample value.  It should return #t when the \
search is satisified.  For example, (lambda (n) (> n .1)) looks for the next sample that is greater than .1.",
		      WITH_WORD_WRAP,
		      snd_xrefs("Find"),
		      snd_xref_urls("Find"));
}

void mix_dialog_help(void)
{
  snd_help_with_xrefs("Mixes",
"This dialog provides various commonly-used controls on the currently \
chosen mix.  At the top are the mix id, begin and end times, \
track number, and a play button.  Beneath that are various sliders \
controlling the speed (sampling rate) of the mix, and the amplitude of each \
input channel; and finally, an envelope editor for the mix's (input) channels. \
The current mix amp env is not actually changed until you click 'Apply Env'.\
The editor envelope is drawn in black with dots whereas the current \
mix amp env (if any) is drawn in blue.",
		      WITH_WORD_WRAP,
		      snd_xrefs("Mix"),
		      snd_xref_urls("Mix"));
}

void track_dialog_help(void)
{
  snd_help_with_xrefs("Tracks",
"This dialog provides various commonly-used controls on the currently \
chosen track.  At the top are the track id, begin and end times, \
track number, and a play button.  Beneath that are various sliders \
controlling the speed (sampling rate) and the amplitude of the track, \
and an envelope editor for the track's overall amplitude envelope. \
The current track's amp env is not actually changed until you click 'Apply Env'.\
The editor envelope is drawn in black with dots whereas the current \
mix amp env (if any) is drawn in blue.",
		      WITH_WORD_WRAP,
		      snd_xrefs("Track"),
		      snd_xref_urls("Track"));
}

/* TODO: shouldn't the Ruby xrefs be in Ruby syntax? (would this mess up the indexing?) */

static char *new_file_xrefs[5] = {
  "open a new sound: {" S_new_sound "}",
  "specialize making a new sound: {" S_new_sound_hook "}",
  "header type constants: {" S_mus_header_type_name "}",
  "data format constants: {" S_mus_data_format_name "}",
  NULL};

void new_file_dialog_help(void)
{
  snd_help_with_xrefs("New File",
"This dialog sets the new file's output header type, data format, srate, chans, and comment. \
The 'srate:' and 'channels:' labels are actually drop-down menus providing quick access to common choices. \
The default values for the fields can be set by clicking 'Reset'.  These values \
are " S_default_output_chans ", " S_default_output_data_format ", " S_default_output_srate ", and " S_default_output_header_type ".  \
The file name field can be set upon each invocation through " S_output_name_hook ", and the \
comment field via " S_output_comment_hook ".  The actual new file representing the new sound is not written \
until you save the new sound.",
		      WITH_WORD_WRAP,
		      new_file_xrefs,
		      NULL);
}

static char *edit_header_xrefs[11] = {
  "change srate: {" S_src_channel "}",
  "convert data to a new format: {" S_save_sound_as "}",
  "interpret current data in new data format: {" S_data_format "}",
  "convert header to a new type: {" S_save_sound_as "}",
  "interpret current header differently: {" S_header_type "}",
  "extract or combine chans: {mono->stereo}",
  "change data location: {" S_data_location "}",
  "change number of samples: {frames}",
  "what are these data formats?",
  "what are these headers?",
  NULL
};

static char *edit_header_urls[11] = {
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  "extsnd.html#describedataformats",
  "extsnd.html#describeheadertypes",
  NULL
};

void edit_header_dialog_help(void)
{
  snd_help_with_xrefs("Edit Header",
"This dialog edits the header of a sound file; no change is made to the actual sound data. \
If you specify 'raw' as the type, any existing header is removed.  This dialog is aimed at adding or removing an entire header,  \
or editing the header comments; anything else is obviously dangerous.",
		      WITH_WORD_WRAP,
		      edit_header_xrefs,
		      edit_header_urls);
}

static char *print_xrefs[4] = {
  "default eps file name: {" S_eps_file "}",
  "eps overall size: {" S_eps_size "}",
  "eps margins: {" S_eps_bottom_margin "}, {" S_eps_left_margin "}",
  NULL};

void print_dialog_help(void)
{
  snd_help_with_xrefs("File Print",
"Print causes the currently active display to be either printed (via the lpr command) or saved as \
an eps file.  In the latter case, the file name is set either by the dialog, or taken from the \
variable " S_eps_file " (normally snd.eps).  Currently the openGL graphics can't be printed by Snd, \
but you can use Gimp or some such program to get a screenshot, and print that.",
		      WITH_WORD_WRAP,
		      print_xrefs,
		      NULL);
}

static char *view_files_xrefs[6] = {
  "place sound in view files list: {" S_add_file_to_view_files_list "}",
  "place all sounds from a directory in view files list: {" S_add_directory_to_view_files_list "}",
  "specialize view files selection: {" S_view_files_select_hook "}",
  "the sort choice: {" S_view_files_sort "}",
  NULL};

void view_files_dialog_help(void)
{
  snd_help_with_xrefs("File Browser",
"The View:Files dialog provides a list of sounds and various things to do with them.\
The play button plays the file. \
Double click a file name, and that file is opened in Snd. The 'update' button runs through the files \
list checking for files that have been deleted or moved behind Snd's back. 'Clear' clears the files list. \
\n\n\
Files can be added to the list via the -p startup switch, and by the functions " S_add_file_to_view_files_list " \
and " S_add_directory_to_view_files_list ". \
\n\n\
The 'sort' label on the right activates a menu of sorting choices; 'name' sorts the \
files list alphabetically, 'date' sorts by date written, 'size' sorts by the \
number of samples in the sound, and 'entry' sorts by the order the sound appears in the \
absence of explicit sorting.  The variable " S_view_files_sort " refers to this menu.",
		      WITH_WORD_WRAP,
		      view_files_xrefs,
		      NULL);
}

static void copy_help(void)
{
  snd_help_with_xrefs("Copy",
		      "See also 'Save'",
		      WITH_WORD_WRAP,
		      snd_xrefs("Copy"),
		      snd_xref_urls("Copy"));
}

static void region_help(void)
{
  snd_help_with_xrefs("Region",
"A region is a portion of the sound data. When a sound portion is selected, it is (by default) saved \
as the new region; subsequent edits will not affect the region data. You can disable the region creation \
by setting the variable " S_selection_creates_region " to #f (its default is #t which can slow down editing \
of very large sounds). Regions can be defined by " S_make_region ", by dragging the mouse through a portion \
of the data, or via the Select All menu option. If the mouse drags off the end of the graph, the x axis \
moves, in a sense dragging the data along to try to keep up with the mouse; the further away the mouse \
is from the display, the faster the axis moves. A region can also be defined with keyboard commands, \
much as in Emacs. C-[space] starts the region definition and the various cursor moving commands \
continue the definition.",
		      WITH_WORD_WRAP,
		      snd_xrefs("Region"),
		      snd_xref_urls("Region"));
}

static void selection_help(void)
{
  snd_help_with_xrefs("Selection",
"The selection is a high-lighted portion of the current sound. \
You can create it by dragging the mouse, or via various functions.",
		      WITH_WORD_WRAP,
		      snd_xrefs("Selection"),
		      snd_xref_urls("Selection"));
}

static void cursor_help(void)
{
  snd_help_with_xrefs("Cursor",
"A big '+' marks the current sample.  This is Snd's cursor, and the \
various Emacs cursor moving commands apply to it.  See also 'Tracking cursor'",
		      WITH_WORD_WRAP,
		      snd_xrefs("Cursor"),
		      snd_xref_urls("Cursor"));
}

static void tracking_cursor_help(void)
{
  snd_help_with_xrefs("Tracking cursor",
"If you want the cursor to follow along more-or-less in time while \
playing a sound, set " S_cursor_follows_play " to #t. See also 'Cursor'",
		      WITH_WORD_WRAP,
		      snd_xrefs("Tracking cursor"),
		      snd_xref_urls("Tracking cursor"));
}

static void smooth_help(void)
{
  snd_help_with_xrefs("Smoothing",
"Smoothing applies a sinusoidal curve to a portion of a sound to smooth \
out any clicks.  See also 'Filter'",
		      WITH_WORD_WRAP,
		      snd_xrefs("Smooth"),
		      snd_xref_urls("Smooth"));
}

static void maxamp_help(void)
{
  snd_help_with_xrefs("Maxamp",
"Maxamp refers to the maximium amplitude in a sound",
		      WITH_WORD_WRAP,
		      snd_xrefs("Maxamp"),
		      snd_xref_urls("Maxamp"));
}

static void reverse_help(void)
{
  snd_help_with_xrefs("Reverse",
"There are various things that can be reversed.  See the list below.",
		      WITH_WORD_WRAP,
		      snd_xrefs("Reverse"),
		      snd_xref_urls("Reverse"));
}

static void noise_reduction_help(void)
{
  snd_help_with_xrefs("Noise Reduction",
		      "",
		      WITH_WORD_WRAP,
		      snd_xrefs("Noise Reduction"),
		      snd_xref_urls("Noise Reduction"));
}

static void random_numbers_help(void)
{
  snd_help_with_xrefs("Random Numbers",
		      "",
		      WITH_WORD_WRAP,
		      snd_xrefs("Random Numbers"),
		      snd_xref_urls("Random Numbers"));
}

static void colors_help(void)
{
  snd_help_with_xrefs("Colors",
"A color in Snd is an object with three fields representing the rgb (red green blue) settings \
as numbers between 0.0 and 1.0. A color object is created via " S_make_color ":\n\
\n\
>(define blue (make-color 0.0 0.0 1.0))\n\
\n\
or in Ruby:\n\
\n\
Blue = make_color(0.0, 0.0, 1.0)\n\
\n\
This declares the Scheme variable \"blue\" and gives it the value of the color whose rgb components \
include only blue in full force. The X11 color names are defined in rgb.scm. The overall widget background color is " S_basic_color ".\n\
\n\
>(set! (basic-color) blue)  ; in Ruby: set_basic_color(Blue)\n\
\n\
The color variables are:\n\
" S_basic_color ":  main Snd color.\n\
" S_cursor_color ":  cursor color.\n\
" S_data_color ":  color of data in unselected graph.\n\
" S_doit_button_color ":  color of Ok and Apply buttons.\n\
" S_doit_again_button_color ":  color of Undo&Apply buttons.\n\
" S_enved_waveform_color ":  color of waveform displayed in envelope editor.\n\
" S_filter_control_waveform_color ":  color of control panel filter waveform.\n\
" S_graph_color ":  background color of unselected graph.\n\
" S_help_button_color ":  color of Help buttons.\n\
" S_highlight_color ":  highlighting color.\n\
" S_listener_color ":  background color of lisp listener.\n\
" S_listener_text_color ":  text color in lisp listener.\n\
" S_mark_color ":  color of mark indicator.\n\
" S_mix_color ":  color of mix waveforms.\n\
" S_position_color ":  position slider color\n\
" S_pushed_button_color ":  color of pushed button.\n\
" S_quit_button_color ":  color of Dismiss and Cancel buttons.\n\
" S_reset_button_color ":  color of Reset buttons.\n\
" S_sash_color ":  color of paned window sashes.\n\
" S_selected_data_color ":  color of data in currently selected graph.\n\
" S_selected_graph_color ":  background color of currently selected graph.\n\
" S_selection_color ":  color of selected portion of graph.\n\
" S_text_focus_color ":  color of text field when it has focus.\n\
" S_zoom_color ":  zoom slider color.\n\
",
		      WITH_WORD_WRAP,
		      snd_xrefs("Colors"),
		      snd_xref_urls("Colors"));
}

static void window_size_help(void)
{
  snd_help_with_xrefs("Window Size",
		      "",
		      WITH_WORD_WRAP,
		      snd_xrefs("Window Size"),
		      snd_xref_urls("Window Size"));
}



#include "snd-xref.c"

#define NUM_TOPICS 36
static char *topic_names[NUM_TOPICS] = {
  "Hook", "Vct", "Sample reader", "Mark", "Mix", "Region", "Edit list", "Transform", "Error",
  "Color", "Font", "Graphic", "Widget", "Emacs",
  "CLM", "Instrument", "CM", "CMN", "Libxm", "Sndlib", 
  "Motif", "Gtk", "Script", "Ruby", "LADSPA", "OpenGL", "Gdb", "Control panel",
  "X resources", "Invocation flags", "Initialization file", "Customization",
  "Noise Reduction", "Window Size", "Color", "Random Number"
};

static char *topic_urls[NUM_TOPICS] = {
  "extsnd.html#sndhooks", "extsnd.html#Vcts", "extsnd.html#samplereaders", "extsnd.html#sndmarks", 
  "extsnd.html#sndmixes", "extsnd.html#sndregions", "extsnd.html#editlists", "extsnd.html#sndtransforms", 
  "extsnd.html#snderrors", "extsnd.html#colors", "extsnd.html#fonts", "extsnd.html#sndgraphics", 
  "extsnd.html#sndwidgets", "grfsnd.html#emacssnd", "grfsnd.html#sndwithclm", 
  "grfsnd.html#sndinstruments", "grfsnd.html#sndwithcm", "sndscm.html#musglyphs", 
  "libxm.html#xm", "sndlib.html#introduction", "grfsnd.html#sndwithmotif", 
  "grfsnd.html#sndwithgtk", "grfsnd.html#sndwithnogui", "grfsnd.html#sndandruby", "grfsnd.html#sndandladspa", 
  "grfsnd.html#sndandgl", "grfsnd.html#sndandgdb", "extsnd.html#customcontrols",
  "grfsnd.html#sndresources", "grfsnd.html#sndswitches", "grfsnd.html#sndinitfile", "extsnd.html#extsndcontents",
  "extsnd.html#noisystory", "extsnd.html#movingwindows", "extsnd.html#colors", "sndscm.html#allrandomnumbers"
};

#if HAVE_STRCASECMP
  #define STRCMP(a, b) strcasecmp(a, b)
  #define STRNCMP(a, b, c) strncasecmp(a, b, c)
#else
  #define STRCMP(a, b) strcmp(a, b)
  #define STRNCMP(a, b, c) strncmp(a, b, c)
#endif

static int min_strlen(const char *a, const char *b)
{
  int lena, lenb;
  lena = strlen(a);
  lenb = strlen(b);
  if (lena < lenb) return(lena);
  return(lenb);
}

static char *topic_url(const char *topic)
{
  int i;
  for (i = 0; i < NUM_TOPICS; i++)
    if (STRNCMP(topic, topic_names[i], min_strlen(topic, topic_names[i])) == 0)
      return(topic_urls[i]);
  return(NULL);
}

#define NUM_XREFS 35
static char *xrefs[NUM_XREFS] = {
  "Mark", "Mix", "Region", "Selection", "Cursor", "Tracking cursor", "Delete", "Envelope", "Filter",
  "Search", "Insert", "Maxamp", "Play", "Reverse", "Save", "Smooth", "Resample", "FFT", "Reverb",
  "Src", "Find", "Undo", "Redo", "Sync", "Control panel", "Record", "Header", "Key", "Track", "Copy",
  "Noise Reduction", "Window Size", "Color", "Control", "Random Numbers"
};

static char **xref_tables[NUM_XREFS] = {
  Marking_xrefs, Mixing_xrefs, Regions_xrefs, Selections_xrefs, Cursors_xrefs, Tracking_cursors_xrefs,
  Deletions_xrefs, Envelopes_xrefs, Filters_xrefs, Searching_xrefs, Insertions_xrefs, Maxamps_xrefs,
  Playing_xrefs, Reversing_xrefs, Saving_xrefs, Smoothing_xrefs, Resampling_xrefs, FFTs_xrefs, Reverb_xrefs,
  Resampling_xrefs, Searching_xrefs, Undo_and_Redo_xrefs, Undo_and_Redo_xrefs, 
  sync_xrefs, control_xrefs, record_xrefs, header_and_data_xrefs, key_xrefs, Tracks_xrefs, Copying_xrefs,
  Noise_Reduction_xrefs, Window_size_and_position_xrefs, Colors_xrefs, control_xrefs, Random_Numbers_xrefs
};

static char **xref_url_tables[NUM_XREFS] = {
  Marking_urls, Mixing_urls, Regions_urls, Selections_urls, Cursors_urls, Tracking_cursors_urls,
  Deletions_urls, Envelopes_urls, Filters_urls, Searching_urls, Insertions_urls, Maxamps_urls,
  Playing_urls, Reversing_urls, Saving_urls, Smoothing_urls, Resampling_urls, FFTs_urls, Reverb_urls,
  Resampling_urls, Searching_urls, Undo_and_Redo_urls, Undo_and_Redo_urls, 
  NULL, NULL, NULL, NULL, NULL, Tracks_urls, Copying_urls, 
  Noise_Reduction_urls, Window_size_and_position_urls, Colors_urls, NULL, Random_Numbers_urls,
};

typedef void (*help_func)(void);
/* if an entry is null here, the main help window will display "(no help found)" */
static help_func help_funcs[NUM_XREFS] = {
  &marks_help, &mix_help, &region_help, &selection_help, &cursor_help, &tracking_cursor_help,
  &delete_help, &env_help, &filter_help, &find_help, &insert_help, &maxamp_help,
  &play_help, &reverse_help, &save_help, &smooth_help, &resample_help, &fft_help, &reverb_help,
  &resample_help, &find_help, &undo_help, &undo_help,
  &sync_help, &controls_help, recording_help, &sound_files_help, &key_binding_help, &track_help, &copy_help,
  &noise_reduction_help, &window_size_help, &colors_help, &controls_help, &random_numbers_help
};

static char **snd_xrefs(const char *topic)
{
  int i;
  for (i = 0; i < NUM_XREFS; i++)
    if (STRCMP(topic, xrefs[i]) == 0)
      return(xref_tables[i]);
  return(NULL);
}

static char **snd_xref_urls(const char *topic)
{
  int i;
  for (i = 0; i < NUM_XREFS; i++)
    if (STRCMP(topic, xrefs[i]) == 0)
      return(xref_url_tables[i]);
  return(NULL);
}

static int levenstein(const char *s1, const char *s2)
{
  /* taken with bug fixes from "The Ruby Way" by Hal Fulton, SAMS Pubs */
  int l1, l2, i, j, val, insert_cost = 2, delete_cost = 2, substitute_cost = 1;
  int **distance;
  l1 = snd_strlen(s1);
  l2 = snd_strlen(s2);
  if ((l1 == 0) || (l2 == 0)) return(1000);
  distance = (int **)CALLOC(l2 + 1, sizeof(int *));
  for (i = 0; i <= l2; i++) distance[i] = (int *)CALLOC(l1 + 1, sizeof(int));
  for (j = 0; j <= l1; j++) distance[0][j] = j * insert_cost;
  for (i = 0; i <= l2; i++) distance[i][0] = i * delete_cost;
  for (i = 1; i <= l2; i++)
    for (j = 1; j <= l1; j++)
      {
	int c1, c2, c3;
	c1 = distance[i][j - 1] + insert_cost;
	c2 = distance[i - 1][j] + delete_cost;
	c3 = distance[i - 1][j - 1] + (((s2[i - 1] == s1[j - 1]) || (toupper(s2[i - 1]) == toupper(s1[j - 1]))) ? 0 : substitute_cost);
	if (c1 > c2) c1 = c2;
	if (c1 > c3) c1 = c3;
	distance[i][j] = c1;
      }
  val = distance[l2][l1];
  for (i = 0; i <= l2; i++) FREE(distance[i]);
  FREE(distance);
  return(val);
}

/* PERHAPS: since help-names is alphabetized (tnames in index.cl), we could use binary search rather than linear in snd-url and others */
char *snd_url(const char *name)
{
  /* (snd-url "save-sound-as") -> "extsnd.html#savesoundas" */
  int i;
  if (help_names) /* no ext lang, but wily user typed play-selection to the help dialog... */
    for (i = 0; i < HELP_NAMES_SIZE; i++)
      if (STRCMP(help_names[i], name) == 0)
	return(help_urls[i]);
  return(NULL);
}

static char *snd_finder(const char *name, bool got_help)
{
  /* desperation -- search *.scm/rb then even *.html? for 'name' */
  char *url = NULL, *fgrep = NULL, *tempnam = NULL, *command = NULL;
  bool is_defined = false;
  int a_def = 0, dir_len = 0, i;
  XEN dirs = XEN_EMPTY_LIST;

#if HAVE_SCHEME || (!HAVE_EXTENSION_LANGUAGE)
  #define NUM_DEFINES 5
  #define TRAILER " "
  char *defines[NUM_DEFINES] = {"(define (", "(define* (", "(define ", "(defmacro ", "(defmacro* "};
  dirs = XEN_EVAL_C_STRING("%load-path");
#endif
#if HAVE_RUBY
  #define NUM_DEFINES 1
  #define TRAILER ""
  char *defines[NUM_DEFINES] = {"def "}; /* PERHAPS: add "class " to this search list? */
  extern VALUE rb_load_path;
  dirs = rb_load_path;
#endif

  is_defined = XEN_DEFINED_P(name);
  url = snd_url(name);
  tempnam = snd_tempnam();
  dir_len = XEN_LIST_LENGTH(dirs);

  for (i = 0; (!fgrep) && (i < dir_len); i++)
    {
      char *path;
      path = XEN_TO_C_STRING(XEN_LIST_REF(dirs, i));
      if (!path) continue;

      for (a_def = 0; (!fgrep) && (a_def < NUM_DEFINES); a_def++)
	{
#if (!MUS_SUN)
	  /* Gnu fgrep: -s switch to fgrep = "silent", I guess (--no-messages) [OSX uses Gnu fgrep] */
	  /* configure script looks for grep -F or fgrep, setting FGREP_PROG (fgrep is supposedly obsolete) */
	  command = mus_format(FGREP_PROG " -s \"%s%s" TRAILER "\" %s/*." XEN_FILE_EXTENSION " --line-number > %s", 
#else
          /* Sun fgrep: here -s means -q and --line-number prints an error message */
	  command = mus_format(FGREP_PROG " \"%s%s" TRAILER "\" %s/*." XEN_FILE_EXTENSION " > %s", 
#endif
			       defines[a_def], 
			       name,
			       path,
			       tempnam);
	  system(command);
	  FREE(command);
	  fgrep = file_to_string(tempnam);
	}
    }
  snd_remove(tempnam, IGNORE_CACHE);
  FREE(tempnam);

  if (url)
    {
      if (fgrep)
	command = mus_format("%s is %sdefined%s; it appears to be defined in:\n%sand documented at %s", 
			     name,
			     (is_defined) ? "" : "not ",
			     (is_defined && (!got_help)) ? ", but has no help string" : "",
			     fgrep, 
			     url);
      else command = mus_format("%s is %sdefined%s; it is documented at %s", 
				name,
				(is_defined) ? "" : "not ",
				(is_defined && (!got_help)) ? ", but has no help string" : "",
				url);
    }
  else
    {
      if (fgrep)
	command = mus_format("%s is %sdefined%s; it appears to be defined in:\n%s",
			     name,
			     (is_defined) ? "" : "not ",
			     (is_defined && (!got_help)) ? ", but has no help string" : "",
			     fgrep);
      else command = NULL;
    }
  if (fgrep) FREE(fgrep); /* don't free url! */
  return(command);
}

bool snd_topic_help(const char *topic)
{
  /* called only in snd-x|ghelp.c */
  int i, topic_len;
  for (i = 0; i < NUM_XREFS; i++)
    if (STRCMP(topic, xrefs[i]) == 0)
      {
	if (help_funcs[i])
	  {
	    (*help_funcs[i])();
	    return(true);
	  }
      }
  topic_len = snd_strlen(topic);
  for (i = 0; i < NUM_XREFS; i++)
    {
      int xref_len, j, diff, min_len;
      const char *a, *b;
      xref_len = strlen(xrefs[i]);
      if (xref_len < topic_len)
	{
	  diff = topic_len - xref_len;
	  min_len = xref_len;
	  a = topic;
	  b = xrefs[i];
	}
      else
	{
	  diff = xref_len - topic_len;
	  min_len = topic_len;
	  a = xrefs[i];
	  b = topic;
	}
      for (j = 0; j < diff; j++)
	if (STRNCMP((char *)(a + j), b, min_len) == 0)
	  {
	    if (help_funcs[i])
	      {
		(*help_funcs[i])();
		return(true);
	      }
	  }
    }
  /* try respelling topic */
  {
    int min_diff = 1000, min_loc = 0, this_diff;
    for (i = 0; i < NUM_XREFS; i++)
      if (help_funcs[i])
	{
	  this_diff = levenstein(topic, xrefs[i]);
	  if (this_diff < min_diff)
	    {
	      min_diff = this_diff;
	      min_loc = i;
	    }
	}
    if (min_diff < snd_ilog2(topic_len)) /* was topic_len / 2, but this gives too much leeway for substitutions */
      {
	(*help_funcs[min_loc])();
	return(true);
      }
  }
  /* go searching for it */
  {
    char *str;
    str = snd_finder(topic, false);
    if (str)
      {
	snd_help(topic, str, WITH_WORD_WRAP);
	FREE(str);
	return(true);
      }
  }

  return(false);
}

static bool strings_might_match(const char *a, const char *b, int len)
{
  int i;
  for (i = 0; i < len; i++)
    {
      if (a[i] != b[i]) return(false);
#if HAVE_RUBY
      if (a[i] == '_') return(true);
#endif
#if HAVE_SCHEME
      if (a[i] == '-') return(true);
#endif
    }
  return(true);
}

char **help_name_to_xrefs(const char *name)
{
  char **xrefs = NULL;
  int i, xref_ctr = 0, xrefs_size = 0, name_len;
  if (!help_names) return(NULL);
  name_len = strlen(name);
  for (i = 0; i < HELP_NAMES_SIZE; i++)
    if (name[0] == help_names[i][0])
      {
	int cur_len;
	cur_len = strlen(help_names[i]);
	if (strings_might_match(name, help_names[i], (name_len < cur_len) ? name_len : cur_len))
	  {
	    if (xref_ctr >= (xrefs_size - 1)) /* need trailing NULL to mark end of table */
	      {
		xrefs_size += 8;
		if (xref_ctr == 0)
		  xrefs = (char **)CALLOC(xrefs_size, sizeof(char *));
		else
		  {
		    int k;
		    xrefs = (char **)REALLOC(xrefs, xrefs_size * sizeof(char *));
		    for (k = xref_ctr; k < xrefs_size; k++) xrefs[k] = NULL;
		  }
	      }
	    xrefs[xref_ctr++] = help_names[i];
	  }
      }
  return(xrefs);
}

char* word_wrap(const char *text, int widget_len)
{
  char *new_text;
  int new_len, old_len, i, j, desired_len, line_start = 0;
#if HAVE_RUBY
  bool move_paren = false;
  int in_paren = 0;
#endif
  old_len = snd_strlen(text);
  new_len = old_len + 64;
  desired_len = (int)(widget_len * .8);
  new_text = (char *)CALLOC(new_len, sizeof(char));
  for (i = 0, j = 0; i < old_len; i++)
    if (text[i] == '\n')
      {
	new_text[j++] = '\n';
	line_start = j;
      }
    else
      {
	if ((text[i] == ' ') &&
	    (help_text_width(new_text, line_start, j) >= desired_len))
	  {
	    new_text[j++] = '\n';
	    line_start = j;
	  }
	else 
	  {
#if (!HAVE_RUBY)
	    new_text[j++] = text[i];
#else
	    /* try to change the reported names to Ruby names */
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
			    in_paren = 1;
			  }
			else
			  {
			    if (in_paren > 0)
			      {
				if ((in_paren == 1) && (text[i] == ' '))
				  {
				    new_text[j++] = ',';
				    new_text[j++] = ' ';
				  }
				else
				  {
				    if (text[i] == ')')
				      in_paren--;
				    else 
				      if (text[i] == '(')
					in_paren++;
				    new_text[j++] = text[i];					
				  }
			      }
			    else new_text[j++] = text[i];
			  }
		      }
		  }
	      }
#endif
	  }
      }
  return(new_text);
}

#define DOC_DIRECTORIES 6
static char *doc_directories[DOC_DIRECTORIES] = {
  "/usr/share/doc/snd-" SND_VERSION,
  "/usr/share/doc/snd-" SND_MAJOR_VERSION,
  "/usr/local/share/doc/snd-" SND_VERSION,
  "/usr/local/share/doc/snd-" SND_MAJOR_VERSION,
  "/usr/doc/snd-" SND_MAJOR_VERSION,
  "/usr/share/docs/snd-" SND_VERSION
};
  
static char *doc_files[DOC_DIRECTORIES] = {
  "/usr/share/doc/snd-" SND_VERSION "/snd.html",
  "/usr/share/doc/snd-" SND_MAJOR_VERSION "/snd.html",
  "/usr/local/share/doc/snd-" SND_VERSION "/snd.html",
  "/usr/local/share/doc/snd-" SND_MAJOR_VERSION "/snd.html",
  "/usr/doc/snd-" SND_MAJOR_VERSION "/snd.html",
  "/usr/share/docs/snd-" SND_VERSION "/snd.html"
};
  
static char *html_directory(void)
{
  int i;
  if (mus_file_probe("snd.html"))
    {
      char *path;
      path = (char *)CALLOC(512, sizeof(char));
      getcwd(path, 512);
      return(path);
    }
  if (html_dir(ss))
    {
      bool happy;
      char *hd = NULL;
      hd = (char *)CALLOC(snd_strlen(html_dir(ss)) + 16, sizeof(char));
      sprintf(hd, html_dir(ss), "/snd.html");
      happy = mus_file_probe(hd);
      FREE(hd);
      if (happy) return(copy_string(html_dir(ss)));
    }
#ifdef DEFAULT_DOC_DIR
  if (mus_file_probe(DEFAULT_DOC_DIR "/snd.html"))
    return(copy_string(DEFAULT_DOC_DIR "/snd.html"));
#endif
  for (i = 0; i < DOC_DIRECTORIES; i++)
    if (mus_file_probe(doc_files[i])) return(copy_string(doc_directories[i]));
  return(NULL);
}

void url_to_html_viewer(char *url)
{
  char *dir_path;
  dir_path = html_directory();
  if (dir_path)
    {
      char *program;
      program = html_program(ss);
      if (program)
	{
	  char *path;
	  path = (char *)CALLOC(strlen(dir_path) + strlen(url) + 256, sizeof(char));
	  if ((strcmp(program, "netscape") == 0) ||
	      (strcmp(program, "mozilla") == 0))
	    {
	      sprintf(path, "%s/%s", dir_path, url);
	      send_mozilla(program, path);
	    }
	  else
	    {
	      sprintf(path, "%s file:%s/%s", program, dir_path, url);
	      system(path);
	    }
	  FREE(path);
	}
      FREE(dir_path);
    }
}


void name_to_html_viewer(char *red_text)
{
  char *url;
  url = snd_url(red_text);
  if (url == NULL) url = topic_url(red_text);
  if (url)
    url_to_html_viewer(url);
}

static XEN help_hook;
static XEN output_comment_hook;

static char *run_string_hook(XEN hook, const char *caller, char *initial_string, char *subject)
{
  /* no longer concats -- now just passes successive results along */
  if (XEN_HOOKED(hook))
    {
      XEN result;
      XEN procs = XEN_HOOK_PROCEDURES(hook);
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
      if (XEN_STRING_P(result))
	return(copy_string(XEN_TO_C_STRING(result)));
    }
  return(copy_string(initial_string));
}

char *output_comment(file_info *hdr)
{
  return(run_string_hook(output_comment_hook, 
			 S_output_comment_hook, 
			 (hdr) ? hdr->comment : NULL, 
			 NULL));
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
If " S_help_hook " is not empty, it is invoked with the subject and the snd-help result \
and its value is returned."

  XEN help_text = XEN_FALSE; 
  char *str = NULL, *new_str, *subject = NULL;
  int min_diff = 1000;
  bool need_free = false;

#if HAVE_GUILE
  int topic_min = 0;
  bool already_looped = false;
  XEN value = XEN_FALSE, sym = XEN_FALSE;
  if (XEN_EQ_P(text, XEN_UNDEFINED))                              /* if no arg, describe snd-help */
    {
      help_text = C_TO_XEN_STRING(H_snd_help);
      subject = S_snd_help;
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
      topic_min = snd_ilog2(snd_strlen(subject));

    HELP_LOOP:
      if (XEN_FALSE_P(help_text))
	{
#if HAVE_SCM_C_DEFINE
	  XEN lookup;
	  if ((XEN_FALSE_P(value)) && (XEN_SYMBOL_P(sym)))
	    {
	      lookup = XEN_SYMBOL_TO_VARIABLE(sym);
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
	  if ((XEN_FALSE_P(help_text)) && (!already_looped) && (help_names))
	    {
	      /* we're getting desperate! */
	      int i, min_loc = 0, this_diff;
	      already_looped = true;
	      for (i = 0; i < HELP_NAMES_SIZE; i++)
		{
		  this_diff = levenstein(subject, help_names[i]);
		  if (this_diff < min_diff)
		    {
		      min_diff = this_diff;
		      min_loc = i;
		    }
		}
	      if (min_diff < topic_min)
		{
		  subject = help_names[min_loc];
		  sym = C_STRING_TO_XEN_SYMBOL(subject);
		  help_text = XEN_OBJECT_HELP(sym);
		  goto HELP_LOOP;
		}
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
    if ((XEN_SYMBOL_P(text)) && (XEN_BOUND_P(text)))
      {
	text = XEN_SYMBOL_TO_STRING(text);
	subject = XEN_TO_C_STRING(text);
      }
    else text = C_TO_XEN_STRING(xen_scheme_procedure_to_ruby(S_snd_help));
  str = XEN_AS_STRING(XEN_OBJECT_HELP(text));
#endif

  if ((str == NULL) || 
      (snd_strlen(str) == 0) ||
      (strcmp(str, PROC_FALSE) == 0)) /* Ruby returns "false" here */
    {
      str = snd_finder(subject, false);
      need_free = true;
    }
  else 
    {
      if ((min_diff < 1000) && (min_diff > 0))
	{
	  char *more_str;
	  more_str = snd_finder(subject, true);
	  if (more_str)
	    {
	      str = mus_format("%s\nOther possibilities:\n%s", str, more_str);
	      need_free = true;
	      FREE(more_str);
	    }
	}
    }

  if (str)
    {
      if (subject)
	new_str = run_string_hook(help_hook, S_help_hook, str, subject);
      else new_str = copy_string(str);
      if (need_free)
	{
	  FREE(str);
	  str = NULL;
	}
      if (widget_wid > 0)
	{
	  str = word_wrap(new_str, widget_wid);
	  if (new_str) FREE(new_str);
	}
      else str = new_str;
      help_text = C_TO_XEN_STRING(str);
      if (str) FREE(str);
    }
  return(xen_return_first(help_text, text));
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
  #define H_html_program "(" S_html_program "): name of documentation reader (mozilla, by default)"
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

static XEN g_snd_urls(void)
{
  #define H_snd_urls "(" S_snd_urls ") -> list of all snd names with the associated url (a list of lists)"
  XEN lst = XEN_EMPTY_LIST;
  int i;
  if (help_names)
    for (i = 0; i < HELP_NAMES_SIZE; i++)
      lst = XEN_CONS(XEN_CONS(C_TO_XEN_STRING(help_names[i]), C_TO_XEN_STRING(help_urls[i])), lst);
  return(lst);
}

static char **refs = NULL, **urls = NULL;
static XEN g_help_dialog(XEN subject, XEN msg, XEN xrefs, XEN xurls)
{
  #define H_help_dialog "(" S_help_dialog " subject message xrefs urls): start the Help window with subject and message"
  widget_t w;
  XEN_ASSERT_TYPE(XEN_STRING_P(subject), subject, XEN_ARG_1, S_help_dialog, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, XEN_ARG_2, S_help_dialog, "a string");
  XEN_ASSERT_TYPE(XEN_LIST_P(xrefs) || XEN_NOT_BOUND_P(xrefs), xrefs, XEN_ARG_3, S_help_dialog, "a list of related references");
  XEN_ASSERT_TYPE(XEN_LIST_P(xurls) || XEN_NOT_BOUND_P(xurls), xurls, XEN_ARG_4, S_help_dialog, "a list of urls");
  if (refs) {FREE(refs); refs = NULL;}
  if (urls) {FREE(urls); urls = NULL;}
  if (XEN_LIST_P(xrefs))
    {
      int i, len;
      len = XEN_LIST_LENGTH(xrefs);
      refs = (char **)CALLOC(len + 1, sizeof(char *));
      for (i = 0; i < len; i++)
	if (XEN_STRING_P(XEN_LIST_REF(xrefs, i)))
	  refs[i] = XEN_TO_C_STRING(XEN_LIST_REF(xrefs, i));
      if (XEN_LIST_P(xurls))
	{
	  int ulen;
	  ulen = XEN_LIST_LENGTH(xurls);
	  if (ulen > len) ulen = len;
	  urls = (char **)CALLOC(ulen + 1, sizeof(char *));
	  for (i = 0; i < ulen; i++)
	    if (XEN_STRING_P(XEN_LIST_REF(xurls, i)))
	      urls[i] = XEN_TO_C_STRING(XEN_LIST_REF(xurls, i));
	}
      w = snd_help_with_xrefs(XEN_TO_C_STRING(subject),
			      XEN_TO_C_STRING(msg), 
			      WITH_WORD_WRAP,
			      refs,
			      urls);
    }
  else w = snd_help(XEN_TO_C_STRING(subject), 
		    XEN_TO_C_STRING(msg), 
		    WITH_WORD_WRAP);
  return(xen_return_first(XEN_WRAP_WIDGET(w), xrefs, xurls));
}


#ifdef XEN_ARGIFY_1
XEN_ARGIFY_2(g_listener_help_w, g_listener_help)
XEN_NARGIFY_0(g_html_dir_w, g_html_dir)
XEN_NARGIFY_1(g_set_html_dir_w, g_set_html_dir)
XEN_NARGIFY_0(g_html_program_w, g_html_program)
XEN_NARGIFY_1(g_set_html_program_w, g_set_html_program)
XEN_NARGIFY_1(g_snd_url_w, g_snd_url)
XEN_NARGIFY_0(g_snd_urls_w, g_snd_urls)
XEN_ARGIFY_4(g_help_dialog_w, g_help_dialog)
#else
#define g_listener_help_w g_listener_help
#define g_html_dir_w g_html_dir
#define g_set_html_dir_w g_set_html_dir
#define g_html_program_w g_html_program
#define g_set_html_program_w g_set_html_program
#define g_snd_url_w g_snd_url
#define g_snd_urls_w g_snd_urls
#define g_help_dialog_w g_help_dialog
#endif

void g_init_help(void)
{
  XEN_DEFINE_PROCEDURE(S_snd_help,    g_listener_help_w,  0, 2, 0, H_snd_help);
  XEN_DEFINE_PROCEDURE(S_snd_url,     g_snd_url_w,        1, 0, 0, H_snd_url);
  XEN_DEFINE_PROCEDURE(S_snd_urls,    g_snd_urls_w,       0, 0, 0, H_snd_urls);
  XEN_DEFINE_PROCEDURE(S_help_dialog, g_help_dialog_w,    2, 2, 0, H_help_dialog);

  #define H_help_hook S_help_hook "(subject help-string): called from " S_snd_help ".  If \
if returns a string, it replaces 'help-string' (the default help)"

  help_hook = XEN_DEFINE_HOOK(S_help_hook, 2, H_help_hook);    /* args = subject help-string */

#if HAVE_SCHEME
  #define H_output_comment_hook S_output_comment_hook " (str): called in Save-As dialog, passed current sound's comment, if any. \
If more than one hook function, each function gets the previous function's output as its input.\n\
  (add-hook! " S_output_comment_hook "\n\
    (lambda (str)\n\
      (string-append str \": written \"\n\
        (strftime \"%a %d-%b-%Y %H:%M %Z\"\n\
          (localtime (current-time))))))"
#endif
#if HAVE_RUBY
  #define H_output_comment_hook S_output_comment_hook " (str): called in Save-As dialog, passed current sound's comment, if any. \
If more than one hook function, each function gets the previous function's output as its input."
#endif

  output_comment_hook = XEN_DEFINE_HOOK(S_output_comment_hook, 1, H_output_comment_hook); /* arg = current mus_sound_comment(hdr) if any */

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_html_dir, g_html_dir_w, H_html_dir, S_setB S_html_dir, g_set_html_dir_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_html_program, g_html_program_w, H_html_program, S_setB S_html_program, g_set_html_program_w,  0, 0, 1, 0);
}
