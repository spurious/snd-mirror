#include "snd.h"
#include "sndlib-strings.h"
#include "clm-strings.h"

/* TODO: audio setup help -- could check mutes/vols etc and report suspicious settings
 *       also the ALSA env vars, mus_audio_report etc
 *       also in special unsupported cases, like x86 solaris
 *       perhaps put bad vals in red? 
 */


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

static char *format_to_name(int bits)
{
  if (bits == 4)
    return(copy_string("float"));
  if (bits == 8)
    return(copy_string("double"));
  return(mus_format("int%d", bits));
}

static char *sndlib_consistency_check(void)
{
  /* sizeof is not usable in this context, unfortunately; bits/bytes jumbled together here */
  int sndlib_bits, snd_bits;
  sndlib_bits = mus_sample_bits();
#if SNDLIB_USE_FLOATS
  snd_bits = mus_bytes_per_sample(MUS_OUT_FORMAT);
#else
  snd_bits = MUS_SAMPLE_BITS;
#endif
  if (snd_bits != sndlib_bits)
    {
      char *snd_name, *sndlib_name, *val;
      snd_name = format_to_name(snd_bits);
      sndlib_name = format_to_name(sndlib_bits);
      val = mus_format(" Snd expects %s samples, but sndlib uses %s!", snd_name, sndlib_name);
      FREE(snd_name);
      FREE(sndlib_name);
      return(val);
    }
  return(NULL);
}

static char *vstrcat(char *arg1, ...)
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

static char *main_snd_xrefs[13] = {
  "{CLM}: sound synthesis",
  "{CM}: algorithmic composition",
  "{CMN}: music notation",
  "{Ruby}: extension language",
  "{Forth}: extension language",
  "{Emacs}: Snd as Emacs subjob",
  "{Libxm}: graphics module",
  "{Sndlib}: underlying sound support library",
  "{Scripting}: Snd as scripting language",
  "{Motif}: Motif extensions via Libxm",
  "{Gtk}: Gtk extensions via Libxm",
  "{Ladspa}: plugins",
  NULL
};

static char *main_snd_xref_urls[13] = {
  "grfsnd.html#sndwithclm",
  "grfsnd.html#sndwithcm",
  "sndscm.html#musglyphs",
  "grfsnd.html#sndandruby",
  "grfsnd.html#sndandforth",
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

#if USE_MOTIF
  #include <X11/IntrinsicP.h>
  #if HAVE_XPM
    #include <X11/xpm.h>
  #endif
#endif

#if HAVE_GNU_LIBC_VERSION_H
  #include <gnu/libc-version.h>
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

#if USE_MOTIF
  #define XM_VERSION_NAME "xm-version"
#else
  #define XM_VERSION_NAME "xg-version"
#endif

static char *xm_version(void)
{
  XEN xm_val = XEN_FALSE;
#if HAVE_GUILE
  #if USE_MOTIF
    xm_val = XEN_EVAL_C_STRING("(and (defined? 'xm-version) xm-version)");
  #else
    #if USE_GTK
      xm_val = XEN_EVAL_C_STRING("(and (defined? 'xg-version) xg-version)");
    #endif
  #endif
#endif
#if HAVE_GAUCHE || HAVE_FORTH
      xm_val = XEN_VARIABLE_REF(XM_VERSION_NAME);
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
#if (!JUST_GL)
 void Init_libgl(void);
#endif
static char *gl_version(void)
{
  XEN gl_val = XEN_FALSE;
#if (!JUST_GL)
  Init_libgl(); /* define the version string, if ./snd --version */
#endif
#if HAVE_GUILE
  gl_val = XEN_EVAL_C_STRING("(and (provided? 'gl) gl-version)"); /* this refers to gl.c, not the GL library */
#endif
#if HAVE_RUBY
  if (rb_const_defined(rb_cObject, rb_intern("Gl_Version")))
    gl_val = XEN_EVAL_C_STRING("Gl_Version");
#endif
#if HAVE_FORTH
  if (fth_provided_p("gl"))
    gl_val = XEN_VARIABLE_REF("gl-version");
#endif
#if HAVE_GAUCHE
  if (Scm_ProvidedP(C_TO_XEN_STRING("gl")))
    gl_val = XEN_VARIABLE_REF("gl-version");
#endif
  if (XEN_STRING_P(gl_val))
    {
      char *version = NULL;
      version = (char *)CALLOC(32, sizeof(char));
      mus_snprintf(version, 32, " (snd gl: %s)", XEN_TO_C_STRING(gl_val));
      if (snd_itoa_ctr < snd_itoa_size) snd_itoa_strs[snd_itoa_ctr++] = version;
      return(version);
    }
  return("");
}

#if MUS_WITH_GL2PS
  char *gl2ps_version(void); /* snd-print.c */
#endif

#if USE_GTK
  #include <X11/Xlib.h>
  #include <X11/Xutil.h>
  #include <GL/gl.h>
  #include <GL/glx.h>
#endif

static char *glx_version(void)
{
  #define VERSION_SIZE 128
  int major = 0, minor = 0;
  char *version;
  version = (char *)CALLOC(VERSION_SIZE, sizeof(char));
  if (ss->sgx == NULL) /* snd --help for example */
    {
#if HAVE_X
      /* Mesa has version.h with all the info we want at compile time, but insists on hiding it! */
      /*   so we go to the trouble of creating a context... */
      int glAttribs[] = {GLX_DOUBLEBUFFER, GLX_RGBA, GLX_DEPTH_SIZE, 1, None};
      Display *dpy;
      XVisualInfo *visInfo;
      int scrn;
      Window win;
      GLXContext glCtx;
      dpy = XOpenDisplay(NULL);
      if (!dpy) return("");
      scrn = DefaultScreen(dpy);
      visInfo = glXChooseVisual(dpy, scrn, glAttribs);
      if (!visInfo) return("");
      glCtx = glXCreateContext(dpy, visInfo, 0, True);
      if (!glCtx) return("");
      win = XCreateSimpleWindow(dpy, RootWindow(dpy, scrn), 0, 0, 1, 1, 0, 0, 0);
      glXMakeCurrent(dpy, win, glCtx);
      mus_snprintf(version, VERSION_SIZE, " %s", glGetString(GL_VERSION));
      return(version);
#else
      return("");
#endif
    }

#if USE_MOTIF
  if (MAIN_DISPLAY(ss) != NULL)
    {
      if (ss->sgx->cx)
	{
	  glXMakeCurrent(MAIN_DISPLAY(ss), XtWindow(ss->sgx->mainshell), ss->sgx->cx);
	  mus_snprintf(version, VERSION_SIZE, " %s", glGetString(GL_VERSION));
	}
      else 
	{
	  glXQueryVersion(MAIN_DISPLAY(ss), &major, &minor);
	  mus_snprintf(version, VERSION_SIZE, " %d.%d", major, minor);
	}
    }
#else
  if (gdk_gl_query_extension() != 0)
    {
      /* can't get GL_VERSION here -- segfaults in FC6 x86_64 */
      gdk_gl_query_version(&major, &minor);
      mus_snprintf(version, VERSION_SIZE, " GtkGL version: %d.%d", major, minor);
    }
  else mus_snprintf(version, VERSION_SIZE, " gtkGL not supported?");
#endif
  if (snd_itoa_ctr < snd_itoa_size) snd_itoa_strs[snd_itoa_ctr++] = version;
  return(version);
}
#endif

char *version_info(void)
{
  char *result, *xversion = NULL, *consistent = NULL;
#if HAVE_GL && MUS_WITH_GL2PS
  char *gl2ps_name = NULL;
#endif
  snd_itoa_ctr = 0;
  xversion = xen_version();
  consistent = sndlib_consistency_check();
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
	  ", ", (sizeof(mus_sample_t) == sizeof(float)) ? "float" : "double", " samples",
#else
	  ", int", snd_itoa(MUS_SAMPLE_BITS), _(" samples"),
#endif
#if WITH_MODULES
	  _(", with modules"),
#endif
	  ")",
	  (consistent) ? consistent : "",
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
  #ifdef MUS_CAIRO_VERSION
	  ", Cairo ", MUS_CAIRO_VERSION,
  #endif
#endif
	  xm_version(), /* omitted if --version/--help because the init procs haven't run at that point */
#if HAVE_GL
	  "\n    OpenGL", glx_version(),
	  gl_version(),
  #if USE_GTK
	  ", gtkglext ",
    #ifdef GTKGLEXT_MAJOR_VERSION
	  snd_itoa(GTKGLEXT_MAJOR_VERSION), ".",
	  snd_itoa(GTKGLEXT_MINOR_VERSION), ".",
	  snd_itoa(GTKGLEXT_MICRO_VERSION),
    #endif
  #endif
  #if MUS_WITH_GL2PS
          ", ", gl2ps_name = gl2ps_version(),
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
#if MUS_WITH_THREADS
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
#ifdef SND_HOST
	  "\n    host: ", SND_HOST,
#endif
	  "\n",
	  NULL);
  free_snd_itoa();
  if (xversion) free(xversion); /* calloc in xen.c */
  if (consistent) FREE(consistent);
#if HAVE_GL && MUS_WITH_GL2PS
  if (gl2ps_name) FREE(gl2ps_name);
#endif
  return(result);
}

void about_snd_help(void)
{
  char *info = NULL, *features = NULL;
#if HAVE_GUILE || HAVE_FORTH
  char *files = NULL;
#endif

#if HAVE_GAUCHE
  features = word_wrap(xen_gauche_features(), 400);
#endif
#if HAVE_GUILE
  features = word_wrap(XEN_AS_STRING(XEN_EVAL_C_STRING("*features*")), 400);
  files = word_wrap(XEN_AS_STRING(XEN_EVAL_C_STRING("*snd-loaded-files*")), 400);
#endif
#if HAVE_RUBY
  features = word_wrap(XEN_AS_STRING(XEN_EVAL_C_STRING("$\".join(' ')")), 400);
#endif
#if HAVE_FORTH
  features = word_wrap(XEN_AS_STRING(XEN_VARIABLE_REF("*features*")), 400);
  files = word_wrap(XEN_AS_STRING(XEN_VARIABLE_REF("*loaded-files*")), 400);
#endif
  info = version_info();

  main_snd_help("Snd is a sound editor.",
		info,
		"\nRecent changes include:\n\
\n\
2-Jan:   Snd 8.7.\n\
29-Dec:  -nogtkrc startup switch.\n\
27-Dec:  add-source-file-extension.\n\
21-Dec:  unclip-channel.\n\
17-Dec:  region-home.\n\
14-Dec:  bird.fsm (Mike Scholz).\n\
12-Dec:  clip-hook.\n\
11-Dec:  sound-data-multiply! sound-data-add! sound-data-offset! sound-data*\n\
         sound-data+ sound-data-copy sound-data-reverse! sound-data-peak.\n\
4-Dec:   frame.scm.\n\
         moved open-sound-file, close-sound-file, and vct->sound-file to snd8.scm.\n\
29-Nov:  Snd 8.6.\n\
21-Nov:  sound-data-scale! and sound-data-fill! with vct/sound-data with-sound output.\n\
         mus-float-equal-fudge-factor\n\
         fade.scm instruments changed to use outa rather than vct-map!\n\
20-Nov:  generic out-any, in-any, locsig, move-sound.\n\
13-Nov:  RF64 headers.\n\
",
#if HAVE_GUILE
	    "\n    *features*:\n    '", features, "\n\n",
            "\n    loaded files: ", files, "\n\n",
#endif
#if HAVE_RUBY	    
	    "\n    $LOADED_FEATURES: \n", features, "\n\n",
#endif
#if HAVE_FORTH    
	    "\n    *features*:\n", features, "\n\n",
            "\n    *loaded-files*:\n", files, "\n\n",
#endif
#if HAVE_GAUCHE
  	    "\n    *features*:\n    '", features, "\n\n",
#endif
#if (!HAVE_EXTENSION_LANGUAGE)
	    "\n",
#endif
	    "Please send bug reports or suggestions to bil@ccrma.stanford.edu.",
NULL);

  if (info) FREE(info);
  if (features) FREE(features);
#if HAVE_GUILE || HAVE_FORTH
  if (files) FREE(files);
#endif
}


/* ---------------- help menu help texts ---------------- */

static bool append_key_help(const char *name, int key, int state, bool cx_extended, bool first_time)
{
  int pos;
  pos = in_user_keymap(key, state, cx_extended);
  if (pos != -1)
    {
      char *msg;
      msg = mus_format("%s%s is bound to %s",
		       (first_time) ? "\n\ncurrently " : "\n          ",
		       name,
		       key_binding_description(key, state, cx_extended));
      snd_help_append(msg);
      FREE(msg);
      return(false);
    }
  return(first_time);
}



/* ---------------- Find ---------------- */

void find_help(void) 
{
  #if HAVE_SCHEME
    #define basic_example "(lambda (y) (> y 0.1))"
    #define find_channel_example "    >(find-channel (lambda (y) (> y .1)))\n    (#t 4423)"
    #define count_matches_example "    >(count-matches (lambda (y) (> y .1)))\n    2851"
    #define search_procedure_example "    >(set! (search-procedure) (lambda (y) (> y .1)))"
  #endif
  #if HAVE_RUBY
    #define basic_example "lambda do |y| y > 0.1 end"
    #define find_channel_example "    >find_channel(lambda do |y| y > 0.1 end)\n    [true, 4423]"
    #define count_matches_example "    >count_matches(lambda do |y| y > 0.1 end)\n    2851"
    #define search_procedure_example "    >set_search_procedure(lambda do |y| y > 0.1 end)"
  #endif
  #if HAVE_FORTH
    #define basic_example "lambda: <{ y }> y 0.1 f> ;"
    #define find_channel_example "    >lambda: <{ y }> 0.1 y f< ; find-channel\n    '( #t 4423 )"
    #define count_matches_example "     >lambda: <{ y }> 0.1 y f< ; count-matches\n     2851"
    #define search_procedure_example "   >lambda: <{ y }> 0.1 y f< ; set-search-procedure"
  #endif

  snd_help_with_xrefs("Find", 

#if HAVE_EXTENSION_LANGUAGE
"Searches in Snd refer to the sound data, and are, in general, patterned after Emacs.  When you type \
C-s or C-r, the minibuffer below the graph is activated and you are asked for the search expression. \
The expression is a function that takes one argument, the current sample value, and returns " PROC_TRUE " when it finds a match. \
To look for the next sample that is greater than .1, " basic_example ".  The cursor then moves \
to that sample, if any. Successive C-s's or C-r's continue the search starting from the next sample.\
Normally, the search applies only to the current channel. To search all current files at once, use the Edit:Find dialog.\
\n\n\
The primary searching function is:\n\
\n\
 " S_find_channel " (proc :optional (sample 0) snd chn edpos)\n\
    This function finds the sample that satisfies the function 'proc'. \n\
    'sample' determines where to start the search. \n\
" find_channel_example "\n\
\n\
Closely related is:\n\
\n\
  " S_count_matches " (proc :optional (sample 0) snd chn edpos)\n\
    This returns how many samples satisfy the function 'proc'.\n\
" count_matches_example "\n\
\n\
To find whether a given sound is currently open in Snd, use:\n\
\n\
  " S_find_sound " (filename :optional (nth 0))\n\
    " S_find_sound " returns the index of 'filename' or " PROC_FALSE ".\n\
\n\
The current search procedure (for the Edit:Find dialog) is:\n\
\n\
  " S_search_procedure " (:optional snd)\n\
" search_procedure_example "\n\
",

#else
"Searches in Snd depend completely on the extension language.  Since none is loaded,\
the searching mechanisms are disabled.",
#endif
		      WITH_WORD_WRAP,
		      snd_xrefs("Search"),
		      snd_xref_urls("Search"));

  append_key_help("C-r", snd_K_r, snd_ControlMask, false,
    append_key_help("C-s", snd_K_s, snd_ControlMask, false, true));
}


/* ---------------- Undo ---------------- */

void undo_help(void) 
{
  #if HAVE_SCHEME
    #define H_undo S_undo
    #define H_redo S_redo
    #define edit_position_example "(set! (edit-position) 0) ; revert channel"
  #endif
  #if HAVE_RUBY
    #define H_undo "undo_edit"
    #define H_redo "redo_edit"
    #define edit_position_example "set_edit_position(0) # revert channel"
  #endif
  #if HAVE_FORTH
    #define H_undo S_undo
    #define H_redo S_redo
    #define edit_position_example "0 set-edit-position \\ revert channel"
  #endif

  snd_help_with_xrefs("Undo and Redo", 

#if HAVE_EXTENSION_LANGUAGE
"Snd supports 'unlimited undo' in the sense that you can move back and forth in the list of edits without any \
limit on how long that list can get.  Each editing operation \
extends the current edit list; each undo backs up in that list, and each redo moves forward in the list of previously \
un-done edits.  Besides the Edit and Popup menu options, and the " H_undo " and " H_redo " functions, \
there are these keyboard sequences: \
\n\n\
  C-x r     redo last edit\n\
  C-x u     undo last edit\n\
  C-x C-r   redo last edit\n\
  C-x C-u   undo last edit\n\
  C-_       undo last edit\n\
\n\
File:Revert is the same as undo all edits.\
In the listener, C-M-g deletes all text, and C-_ deletes back to the previous command.\
In the sound display, the number at the lower left shows the current edit position and the channel number.\
The main functions that affect the edit position are:\n\
\n\
  " H_undo " (:optional (edits 1) snd chn)\n\
    This undoes 'edits' edits in snd's channel chn.\n\
\n\
  " H_redo " (:optional (edits 1) snd chn)\n\
    This re-activates 'edits' edits in snd's channel chn.\n\
\n\
  " S_revert_sound " (:optional snd)\n\
    This reverts 'snd' to its saved (unedited) state.\n\
\n\
  " S_edit_position " (:optional snd chn)\n\
    This is the current position in the edit history list.\n\
    " edit_position_example "\n\
",

#else
"Snd supports 'unlimited undo' in the sense that you can move back and forth in the list of edits without any \
limit on how long that list can get.  Each editing operation \
extends the current edit list; each undo backs up in that list, and each redo moves forward in the list of previously \
un-done edits.  Besides the Edit and Popup menu options,\
there are these keyboard sequences: \
\n\n\
  C-x r     redo last edit\n\
  C-x u     undo last edit\n\
  C-x C-r   redo last edit\n\
  C-x C-u   undo last edit\n\
  C-_       undo last edit\n\
\n\
File:Revert is the same as undo all edits.",
#endif

		      WITH_WORD_WRAP,
		      snd_xrefs("Undo"),
		      snd_xref_urls("Undo"));

  append_key_help("C-M-g", snd_K_g, snd_ControlMask | snd_MetaMask, false,
    append_key_help("C-_", snd_K_underscore, snd_ControlMask, false,
      append_key_help("C-x C-u", snd_K_u, snd_ControlMask, true,
        append_key_help("C-x C-r", snd_K_r, snd_ControlMask, true,
          append_key_help("C-x u", snd_K_u, 0, true,
	    append_key_help("C-x r", snd_K_r, 0, true, true))))));
}


/* ---------------- Sync and Unite ---------------- */

static char *sync_xrefs[5] = {
  "sound sync field: {" S_sync "}, {" S_sync_max "}",
  "mark sync field: {" S_mark_sync "}, {" S_mark_sync_max "}, {mark-sync-color}, {" S_syncd_marks "}",
  "mix sync (track) field: {" S_mix_track "}",
  "channel display choice: {" S_channel_style "}",
  NULL};

void sync_help(void) 
{
  #if HAVE_SCHEME
    #define channel_style_example "(set! (channel-style snd) channels-combined)"
    #define H_channels_separate S_channels_separate
    #define H_channels_combined S_channels_combined
    #define H_channels_superimposed S_channels_superimposed
  #endif
  #if HAVE_RUBY
    #define channel_style_example "set_channel_style(Channels_combined, snd)"
    #define H_channels_separate "Channels_separate"
    #define H_channels_combined "Channels_combined"
    #define H_channels_superimposed "Channels_superimposed"
  #endif
  #if HAVE_FORTH
    #define channel_style_example "channels-combined set-channel-style"
    #define H_channels_separate S_channels_separate
    #define H_channels_combined S_channels_combined
    #define H_channels_superimposed S_channels_superimposed
  #endif

  snd_help_with_xrefs("Sync", 

#if HAVE_EXTENSION_LANGUAGE
"The sync button causes certain operations to apply to all channels or multiple sounds simultaneously. \
For example, to get a multichannel selection, set the sync button, then define the selection (by dragging \
the mouse) in one channel, and the parallel portions of the other channels are also selected. \
Marks and mixes can also be sync'd together.\n\
\n\
Similarly, the unite button combines channels of a \
multichannel sound into one display window.  control-click the unite button to have the channels \
superimposed on each other. The function associated with this is:\n\
\n\
  " S_channel_style " (:optional snd)\n\
    " S_channel_style " reflects the value of the 'unite' button in multichannel files.\n\
    Possible values are " H_channels_separate ", " H_channels_combined ", and " H_channels_superimposed ".\n\
    " channel_style_example "\n\
",

#else
"The sync button causes certain operations to apply to all channels or multiple sounds simultaneously. \
For example, to get a multichannel selection, set the sync button, then define the selection (by dragging \
the mouse) in one channel, and the parallel portions of the other channels are also selected. \
Marks and mixes can also be sync'd together.\n\
\n\
Similarly, the unite button combines channels of a \
multichannel sound into one display window.  control-click the unite button to have the channels \
superimposed on each other.",
#endif
		      WITH_WORD_WRAP,
		      sync_xrefs,
		      NULL);
}


/* ---------------- Debug ---------------- */

static char *debug_xrefs[8] = {
  "C debugging: {gdb}",
  "Scheme/Ruby/Forth debugging: {snd-debug}",
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
  #if HAVE_SCHEME
    #define vardpy_reference "variable-display in snd-motif.scm"
  #endif
  #if HAVE_RUBY
    #define vardpy_reference "variable_display in snd-xm.rb"
  #endif
  #if HAVE_FORTH
    #define vardpy_reference "variable-display, which isn't written yet"
  #endif

  snd_help_with_xrefs("Debugging", 

#if HAVE_EXTENSION_LANGUAGE
"There are several sets of debugging aids, each aimed at a different level of code. \
C code is normally debugged with gdb.  If you hit a segfault in Snd, please tell me \
about it!  If possible, run Snd in gdb and send me the stack trace: \n\n\
  gdb snd\n\
  run\n\
  <get error to happen>\n\
  where\n\
\n\
See README.Snd for more about C-level troubles.  For CLM-based instruments, " vardpy_reference " might \
help.  For debugging your own Scheme/Ruby/Forth \
code (or Snd's for that matter), see the \"Errors and Debugging\" section of \
extsnd.html, or snd-debug.  For notelist debugging, see ws-backtrace.",
#else
"If you hit a segfault in Snd, please tell me \
about it!  If possible, run Snd in gdb and send me the stack trace: \n\n\
  gdb snd\n\
  run\n\
  <get error to happen>\n\
  where\n\
  <much info here that is useful to me>\n",
#endif

		      WITH_WORD_WRAP,
		      debug_xrefs,
		      debug_urls);
}


/* ---------------- Envelope ---------------- */

void env_help(void) 
{
  #if HAVE_SCHEME
    #define envelope_example "'(0 0 1 1 2 0)"
    #define env_sound_example "(env-sound '(0 0 1 1 2 0))"
  #endif
  #if HAVE_RUBY
    #define envelope_example "[0.0, 0.0, 1.0, 1.0, 2.0, 0.0]"
    #define env_sound_example "env_sound([0.0, 0.0, 1.0, 1.0, 2.0, 0.0])"
  #endif
  #if HAVE_FORTH
    #define envelope_example "'( 0.0 0.0 1.0 1.0 2.0 0.0 )"
    #define env_sound_example "'( 0.0 0.0 1.0 1.0 2.0 0.0 ) env-sound"
  #endif

  snd_help_with_xrefs("Envelope", 

#if HAVE_EXTENSION_LANGUAGE
"An envelope in Snd is a list (an array in Ruby) of x y break-point pairs. The x axis range is arbitrary. \
To define a triangle curve: " envelope_example ".\
There is no preset limit on the number of breakpoints. Use the envelope editor to draw envelopes with the mouse. \
\n\n\
To apply an envelope to a sound, use " S_env_sound " or the extended command C-x C-a.  If this command gets a numeric \
argument, the envelope is applied from the cursor for that many samples. Otherwise, the envelope is \
applied to the entire file. \
\n\
  C-x a     apply amplitude envelope to selection\n\
  C-x C-a   apply amplitude envelope to channel\n\
\n\
The primary enveloping functions are:\n\
\n\
 " S_env_sound " (envelope :optional (samp 0) samps (env-base 1.0) snd chn edpos)\n\
    " S_env_sound " applies the amplitude envelope to snd's channel chn.\n\
    " env_sound_example "\n\
\n\
 " S_env_channel " (clm-env-gen :optional beg dur snd chn edpos)\n\
    This is the channel-specific version of " S_env_sound ".\n\
",

#else
"It is hard to define or use an envelope if there's no extension language.",
#endif

		      WITH_WORD_WRAP,
		      snd_xrefs("Envelope"),           /* snd-xref.c */
		      snd_xref_urls("Envelope"));

  append_key_help("C-x a", snd_K_a, 0, true,
    append_key_help("C-x C-a", snd_K_s, snd_ControlMask, true, true));
}


/* ---------------- FFT ---------------- */

void fft_help(void)
{
  #if HAVE_SCHEME
    #define transform_normalization_example "(set! (transform-normalization) dont-normalize)"
    #define transform_size_example "(set! (transform-size) 512)"
    #define transform_type_example "(set! (transform-type) autocorrelation)"
    #define fft_window_example "(set! (fft-window) rectangular-window)"
    #define transform_graph_example "(set! (transform-graph?) #t)"
    #define transform_graph_type_example "(set! (transform-graph-type) graph-as-sonogram)"
    #define transform_log_magnitude_example "(set! (transform-log-magnitude) #f)"
    #define transform_types "fourier-transform wavelet-transform  haar-transform\n      autocorrelation   walsh-transform    cepstrum"
    #define transform_graph_types "graph-once  graph-as-sonogram  graph-as-spectrogram"
    #define transform_normalizations "dont-normalize normalize-by-channel normalize-by-sound normalize-globally"
    #define fft_windows "      bartlett-window blackman2-window blackman3-window blackman4-window\n      cauchy-window connes-window dolph-chebyshev-window exponential-window\n      gaussian-window hamming-window hann-poisson-window hann-window\n      kaiser-window parzen-window poisson-window rectangular-window\n      riemann-window samaraki-window tukey-window ultraspherical-window\n      welch-window"
  #endif
  #if HAVE_RUBY
    #define transform_normalization_example "set_transform_normalization(Dont_normalize)"
    #define transform_size_example "set_transform_size(512)"
    #define transform_type_example "set_transform_type(Autocorrelation)"
    #define fft_window_example "set_fft_window(Rectangular_window)"
    #define transform_graph_example "set_transform_graph?(#t)"
    #define transform_graph_type_example "set_transform_graph_type(Graph_as_sonogram)"
    #define transform_log_magnitude_example "set_transform_log_magnitude(#f)"
    #define transform_types "Fourier_transform Wavelet_transform  Haar_transform\n      Autocorrelation   Walsh_transform    Cepstrum"
    #define transform_graph_types "graph_once  graph_as_sonogram  graph_as_spectrogram"
    #define transform_normalizations "Dont_normalize Normalize_by_channel Normalize_by_sound Normalize_globally"
    #define fft_windows "      Bartlett_window Blackman2_window Blackman3_window Blackman4_window\n      Cauchy_window Connes_window Dolph_chebyshev_window Exponential_window\n      Gaussian_window Hamming_window Hann_poisson_window Hann_window\n      Kaiser_window Parzen_window Poisson_window Rectangular_window\n      Riemann_window Samaraki_window Tukey_window Ultraspherical_window\n      Welch_window"
  #endif
  #if HAVE_FORTH
    #define transform_normalization_example "dont-normalize set-transform-normalization"
    #define transform_size_example "512 set-transform-size"
    #define transform_type_example "autocorrelation set-transform-type"
    #define fft_window_example "rectangular-window set-fft-window"
    #define transform_graph_example "#t set-transform-graph?"
    #define transform_graph_type_example "graph-as-sonogram set-transform-graph-type"
    #define transform_log_magnitude_example "#f set-transform-log-magnitude"
    #define transform_types "fourier-transform wavelet-transform  haar-transform\n      autocorrelation   walsh-transform    cepstrum"
    #define transform_graph_types "graph-once  graph-as-sonogram  graph-as-spectrogram"
    #define transform_normalizations "dont-normalize normalize-by-channel normalize-by-sound normalize-globally"
    #define fft_windows "      bartlett-window blackman2-window blackman3-window blackman4-window\n      cauchy-window connes-window dolph-chebyshev-window exponential-window\n      gaussian-window hamming-window hann-poisson-window hann-window\n      kaiser-window parzen-window poisson-window rectangular-window\n      riemann-window samaraki-window tukey-window ultraspherical-window\n      welch-window"
  #endif

  snd_help_with_xrefs("FFT",

#if HAVE_EXTENSION_LANGUAGE
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
additional parameter sometimes known as alpha or beta.  This can be set by the scale(s) next to the fft window graph in the \
transform dialog, or via " S_fft_window_alpha " and " S_fft_window_beta ". \
\n\n\
To change the defaults, you can set the various values in your initialization file (see examples below), \
or use the Options:Preferences dialog's Transforms section.\
\n\n\
The FFT display is activated by setting the 'f' button on the channel's window.  It then updates \
itself each time the time domain waveform moves or changes.  \
The FFT is taken from the start (the left edge) of the current window and is updated as the window bounds change. \
The data is scaled to fit between 0.0 and 1.0 unless transform normalization is off. \
The full frequency axis is normally displayed, but the axis is draggable -- put the mouse on the axis \
and drag it either way to change the range (this is equivalent to changing the variable " S_spectro_cutoff "). \
You can also click on any point in the fft to get the associated fft value at that point displayed; \
if " S_with_verbose_cursor " is " PROC_TRUE ", you can drag the mouse through the fft display and \
the description in the minibuffer will be constantly updated.  To change the fft size by powers of two, \
you can use the keypad keys '*' and '/'.  There are also various keys bindings for the spectrogram \
viewing angles and so on, but it's always easier to use the View:Orientation dialog's sliders. \
\n\n\
The main FFT-related functions are:\
\n\n\
  " S_transform_size " (:optional snd chn)\n\
    the fft size (a power of 2): " transform_size_example "\n\
\n\
  " S_transform_type " (:optional snd chn)\n\
    which transform is performed: " transform_type_example "\n\
    the built-in transform choices are:\n\
      " transform_types "\n\
\n\
  " S_transform_graph_type " (:optional snd chn)\n\
    the display choice, one of: " transform_graph_types "\n\
    " transform_graph_type_example "\n\
\n\
  " S_fft_window " (:optional snd chn)\n\
    the fft data window: " fft_window_example "\n\
    the windows are:\n\
" fft_windows "\n\
\n\
  " S_show_transform_peaks " and " S_max_transform_peaks "\n\
    these control the peak info display\n\
\n\
  " S_fft_log_magnitude " and " S_fft_log_frequency "\n\
    these set whether the axes are linear or logarithmic\n\
\n\
  " S_transform_graph_p " (:optional snd chn)\n\
    if " PROC_TRUE ", the fft graph is displayed: " transform_graph_example "\n\
\n\
  " S_transform_normalization " (:optional snd chn)\n\
    how fft data is normalized.  " transform_normalization_example "\n\
    the choices are:\n\
    " transform_normalizations "\n\
\n\
  " S_peaks " (:optional file snd chn) writes out current peak info as text\n\
\n\
  Other related functions: " S_zero_pad ", " S_wavelet_type ", " S_add_transform "\n\
    " S_min_dB ", " S_snd_spectrum ", " S_show_selection_transform ".",

#else
"Nearly all the transform-related choices can be set in the transform dialog launched from the Options \
Menu Transform item. Most of this dialog should be self-explanatory.  Some of the windows take an \
additional parameter sometimes known as alpha or beta.  This can be set by the scale(s) next to the fft window graph in the \
transform dialog. \
\n\n\
The FFT display is activated by setting the 'f' button on the channel's window.  It then updates \
itself each time the time domain waveform moves or changes.  \
The FFT is taken from the start (the left edge) of the current window and is updated as the window bounds change. \
The data is scaled to fit between 0.0 and 1.0 unless transform normalization is off. \
The full frequency axis is normally displayed, but the axis is draggable -- put the mouse on the axis \
and drag it either way to change the range. \
You can also click on any point in the fft to get the associated fft value at that point displayed. \
To change the fft size by powers of two, \
you can use the keypad keys '*' and '/'.  There are also various keys bindings for the spectrogram \
viewing angles and so on, but it's always easier to use the View:Orientation dialog's sliders.",
#endif

		      WITH_WORD_WRAP,
		      snd_xrefs("FFT"),
		      snd_xref_urls("FFT"));

  global_fft_state();
}


/* ---------------- Control Panel ---------------- */

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
  #if HAVE_SCHEME
    #define amp_control_example "(set! (amp-control) 0.5)"
    #define amp_control_bounds_example "(set! (amp-control-bounds) (list 0.0 20.0))"
    #define speed_control_styles "speed-control-as-float speed-control-as-ratio speed-control-as-semitone"
  #endif
  #if HAVE_RUBY
    #define amp_control_example "set_amp_control(0.5)"
    #define amp_control_bounds_example "set_amp_control_bounds([0.0, 20.0])"
    #define speed_control_styles "Speed_control_as_float Speed_control_as_ratio Speed_control_as_semitone"
  #endif
  #if HAVE_FORTH
    #define amp_control_example "0.5 set-amp-control"
    #define amp_control_bounds_example "'( 0.0 20.0 ) set-amp-control-bounds"
    #define speed_control_styles "speed-control-as-float speed-control-as-ratio speed-control-as-semitone"
  #endif

  snd_help_with_xrefs("The Control Panel", 

#if HAVE_EXTENSION_LANGUAGE
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
  C-x C-c   hide control panel\n\
\n\
There are functions to get and set every aspect of the control panel -- far too many to list! \
As an example of how they all work, to set the amplitude slider from a function: \n\
\n\
  " amp_control_example "\n\
\n\
To set its overall bounds:\n\
\n\
  " amp_control_bounds_example "\n\
\n\
The speed control can be interpreted as a float, a ratio, or a semitone. The choices are:\n\
\n\
    " speed_control_styles "\n\
\n\
To apply the current settings as an edit, call:\n\
\n\
  " S_apply_controls " (:optional snd (target 0) beg dur)\n\
    where 'target' selects what gets edited: \n\
      0 = sound, 1 = channel, 2 = selection.",

#else
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
For softer sounds, it causes only an amplitude change. \
Contrast is on only if the contrast button is set. \
\n\n\
The filter is an arbitrary (even) order FIR filter specified by giving the frequency response \
envelope and filter order in the text windows provided. \
The envelope X axis goes from 0 to half the sampling rate. The actual frequency response (given the current filter order) \
is displayed in blue.  The filter is on only if the filter button is set.",
#endif

		      WITH_WORD_WRAP,
		      control_xrefs,
		      NULL);

  global_control_panel_state();

  append_key_help("C-x C-o", snd_K_o, snd_ControlMask, true,
    append_key_help("C-x C-c", snd_K_c, snd_ControlMask, true, true));
}


/* ---------------- Marks ---------------- */

void marks_help(void) 
{
  #if HAVE_SCHEME
    #define add_mark_example "(add-mark 1234)"
    #define mark_name_example "(set! (mark-name 0) \"mark-0\")"
  #endif
  #if HAVE_RUBY
    #define add_mark_example "add_mark(1234)"
    #define mark_name_example "set_mark_name(0, \"mark-0\")"
  #endif
  #if HAVE_FORTH
    #define add_mark_example "1234 add-mark"
    #define mark_name_example "0 \"mark-0\" set-mark-name"
  #endif

  snd_help_with_xrefs("Marks", 

#if HAVE_EXTENSION_LANGUAGE
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
  C-x j     go to named mark\n\
\n\
The main mark-related functions are:\n\
\n\
  " S_add_mark " (sample :optional snd chn)\n\
   add a mark at sample 'sample': " add_mark_example "\n\
\n\
  " S_delete_mark " (mark-id) \n\
    delete mark (the 'mark-id' is returned by " S_add_mark ")\n\
\n\
  " S_mark_sample " (mark-id): sample marked by the mark\n\
  " S_mark_name " (mark-id): mark's name, if any\n\
    " mark_name_example "\n\
\n\
Other such functions: " S_find_mark ", " S_mark_color ", " S_mark_tag_width ",\n\
    " S_mark_tag_height ", " S_mark_sync ", " S_show_marks ", " S_save_marks ",\n\
    " S_marks "\n",

#else
"A 'mark' marks a particular sample in a sound (not a position in that sound). \
If we mark a sample, then delete 100 samples before it, the mark follows the sample, changing its current position \
in the data.  If we delete the sample, the mark is also deleted; a subsequent undo that returns the sample also \
returns its associated mark.\
\n\n\
Once set, a mark can be moved by dragging the horizontal tab at the top.  Control-click of the tab followed by mouse \
drag will drag the underlying data too, either inserting zeros or deleting data. \
Click on the triangle at the bottom to play (or stop playing) from the mark; drag the \
triangle to play following the mouse. \
The following keyboard commands relate to marks: \
\n\n\
  C-m       place (or remove if argument negative) mark at cursor\n\
  C-M       place syncd marks at all currently syncd chan cursors\n\
  C-x /     place named mark at cursor\n\
  C-x C-m   add named mark\n\
  C-j       go to mark\n\
  C-x j     go to named mark",
#endif

		      WITH_WORD_WRAP, 
		      snd_xrefs("Mark"),
		      snd_xref_urls("Mark"));

  append_key_help("C-x j", snd_K_j, 0, true,
    append_key_help("C-j", snd_K_j, snd_ControlMask, false,
      append_key_help("C-x C-m", snd_K_m, snd_ControlMask, true,
        append_key_help("C-x /", snd_K_slash, 0, true,
	  append_key_help("C-m", snd_K_m, snd_ControlMask, false, true)))));
}


/* ---------------- Mixes ---------------- */

void mix_help(void) 
{
  #if HAVE_SCHEME
    #define mix_example "(mix \"oboe.snd\" 1234)"
    #define mix_vct_example "(mix-vct (vct 0 .1 .2) 1234)"
    #define mix_amp_example "(set! (mix-amp 0 0) .5)"
    #define mix_amp_env_example "(set! (mix-amp-env 0 0) '(0 0 1 1))"
  #endif
  #if HAVE_RUBY
    #define mix_example "mix(\"oboe.snd\", 1234)"
    #define mix_vct_example "mix_vct(vct(0.0, 0.1, 0.2), 1234)"
    #define mix_amp_example "set_mix_amp(0, 0, .5)"
    #define mix_amp_env_example "set_mix_amp_env(0, 0, [0.0, 0.0, 1.0, 1.0])"
  #endif
  #if HAVE_FORTH
    #define mix_example "\"oboe.snd\" 1234 mix"
    #define mix_vct_example "0.0 0.1 0.2 vct 1234 mix-vct"
    #define mix_amp_example "0 0 0.5 set-mix-amp"
    #define mix_amp_env_example "0 0 '( 0.0 0.0 1.0 1.0 ) set-mix-amp-env"
  #endif

  snd_help_with_xrefs("Mixing", 

#if HAVE_EXTENSION_LANGUAGE
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
A set of associated mixes is called a 'track' in Snd, and there's a help menu item for that subject.\n\
\n\
The main mix-related functions are:\n\
\n\
  " S_mix " (file :optional samp in-chan snd chn tags delete trk)\n\
    mix file's channel in-chan starting at samp\n\
    " mix_example "\n\
\n\
  " S_mix_selection " (:optional beg snd chn)\n\
    mix (add) selection starting at beg\n\
\n\
  " S_mix_region " (:optional samp reg snd chn track)\n\
    mix region reg at sample samp (default is cursor sample)\n\
\n\
  " S_mix_vct " (vct :optional beg snd chn with-mix-tags origin track)\n\
    mix the contents of vct starting at beg\n\
    " mix_vct_example "\n\
\n\
  " S_mix_amp " (mix :optional chan)\n\
    amplitude of mix's channel chan:\n\
    " mix_amp_example "\n\
\n\
  " S_mix_amp_env " (mix :optional chan)\n\
    amplitude envelope of mix's channel chan:\n\
    " mix_amp_env_example "\n\
\n\
  " S_mix_speed " (mix)\n\
    speed (resampling ratio) of mix; 1.0 means no change;\n\
      2.0 reads the mix data twice as fast\n\
\n\
  " S_mix_position " (mix)\n\
    position (a sample number) of mix\n\
\n\
  " S_mix_frames " (mix)\n\
    mix's length in samples\n\
\n\
Other such function include: " S_mix_waveform_height ", " S_with_mix_tags ", " S_mix_tag_width ",\n\
    " S_mix_tag_height ", " S_mix_tag_y ", " S_play_mix ", " S_mixes ".",

#else
"Since mixing is the most common and most useful editing operation performed on \
sounds, there is relatively elaborate support for it in Snd. To mix in a file, \
use the File Mix menu option, the command C-x C-q, or one of the various \
mixing functions. Currently the only difference between the first two is that \
the Mix menu option tries to take the current sync state into account, whereas \
the C-x C-q command does not. To mix a selection, use C-x q. The mix starts at \
the current cursor location. It is displayed as a separate waveform above \
the main waveform with a red tag at the beginning.  You can drag the tag to \
reposition the mix. \
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
#endif

		      WITH_WORD_WRAP, 
		      snd_xrefs("Mix"),
		      snd_xref_urls("Mix"));

  append_key_help("C-x q", snd_K_q, 0, true,
    append_key_help("C-x C-q", snd_K_q, snd_ControlMask, true, true));
}


/* ---------------- Tracks ---------------- */

void track_help(void) 
{
  #if HAVE_SCHEME
    #define make_track_example "(make-track 1 3) ; mixes 1 and 3 are in this track"
    #define track_amp_env_example "(set! (track-amp-env 1) '(0 0 1 1))"
    #define track_color_example "(set! (track-color 1) (make-color 0 0 1)) ; blue"
  #endif
  #if HAVE_RUBY
    #define make_track_example "make_track(1, 3) # mixes 1 and 3 are in this track"
    #define track_amp_env_example "set_track_amp_env(1, [0.0, 0.0, 1.0, 1.0])"
    #define track_color_example "set_track_color(1, make_color(0.0, 0.0, 1.0)) # blue" 
  #endif
  #if HAVE_FORTH
    #define make_track_example "1 3 make-track \\ mixes 1 and 3 are in this track"
    #define track_amp_env_example "1 '( 0.0 0.0 1.0 1.0 ) set-track-amp-env"
    #define track_color_example "1 0.0 0.0 1.0 make-color set-track-color \\ blue"
  #endif

  snd_help_with_xrefs("Tracks",

#if HAVE_EXTENSION_LANGUAGE
"A track is a list of mixes, each constituent mix having its 'mix-track' field set to the track id.  The " S_make_track " \
function takes the initial list of mix id's, returning the track id (an integer). \
The rest of the track functions \
take the track id as their initial argument.  A track has much the same structure as a mix: an amplitude, speed, \
amplitude envelope, track, position, and so on.  If its own 'track-track' field is not 0, the entire track is a member \
of the another (higher-level) track. Tracks provide a mechanism to group together related notes or mixes, \
for panning, global envelopes, intra-voice tempo control, and so on. \
\n\n\
There is View:Tracks dialog to manipulate tracks from sliders and so on.  You can also use the \
track-related functions, the more useful of which are: \n\
\n\
  " S_make_track " (mix-ids...)\n\
    create a new track, returning its id:\n\
    " make_track_example "\n\
\n\
  " S_play_track " (track-id :optional chn beg)\n\
    play the given track.\n\
\n\
  " S_track_amp " (track-id)\n\
    track overall amplitude.\n\
\n\
  " S_track_amp_env " (track-id)\n\
    overall track amplitude envelope:\n\
    " track_amp_env_example "\n\
\n\
  " S_track_color " (track-id)\n\
    track waveform color:\n\
    " track_color_example "\n\
\n\
  " S_track_position " (track-id)\n\
    track position (begin of first mix) in samples\n\
\n\
  " S_track_frames " (track-id :optional chan)\n\
    track length in samples\n\
\n\
  " S_track_speed " (track-id)\n\
    track speed. This affects the resampling of each mix,\n\
    not the speed at which the mixes occur (see track-tempo)\n\
\n\
  " S_track_tempo " (track-id)\n\
    track tempo. This affects the spacing between mixes;\n\
    a higher track-tempo corresponds to tighter spacing between mixes.\n\
\n\
Other track-related functions include: " S_track_track ", " S_track_chans ",\n\
    " S_tracks ", " S_track_track ", " S_delete_track ", " S_copy_track ".",

#else
"A track is a list of mixes, but I can't see how to use it when there's no extension language.",
#endif

		      WITH_WORD_WRAP,
		      snd_xrefs("Track"),
		      snd_xref_urls("Track"));
}


/* ---------------- Recording ---------------- */

static char *record_xrefs[] = {
  "recorder variables: {" S_recorder_gain "}, etc",
  "low-level ADC input: {" S_mus_audio_open_input "}",
#if (!HAVE_FORTH)
  "process incoming sound: rtio." XEN_FILE_EXTENSION,
#endif
  NULL};

void recording_help(void) 
{
  snd_help_with_xrefs("Record", 

#if HAVE_EXTENSION_LANGUAGE
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
with the variable " S_vu_size " which defaults to 1.0. Set " S_vu_in_dB " to " PROC_TRUE " to get dB scaling. \
\n\n\
If you go to the main Snd window while the recorder is active and play a sound, the \
recorder's audio lines are made inactive to try to reduce confusion.  To re-activate \
the recorder, press the 'reset' button at the bottom of the window. \
\n\n\
Digital input is slightly tricky -- you need to set the sampling rate before you \
click the 'digital input' button; otherwise you'll get a stuttering effect because the output \
(monitor) rate doesn't match the input rate.\n\
\n\
The recorder-related functions are:\n\
\n\
    " S_recorder_autoload ": open recording automatically\n\
    " S_recorder_buffer_size ": input buffer size\n\
    " S_recorder_dialog ": start the recorder dialog\n\
    " S_recorder_file ": output file name\n\
    " S_recorder_file_hook ": hook for setting output file name\n\
    " S_recorder_gain "(which-gain): input (sound card) gain\n\
    " S_recorder_in_amp " (in-chan out-chan): input -> output gain\n\
    " S_recorder_in_chans ": number of input chans\n\
    " S_recorder_in_data_format ": incoming data format\n\
    " S_recorder_in_device ": input device (mus-microphone)\n\
    " S_recorder_max_duration ": maximum output file length (if set)\n\
    " S_recorder_out_amp "(out-chan): output gain on out-chan\n\
    " S_recorder_out_chans ": number of output chans\n\
    " S_recorder_out_data_format ": output sound data format\n\
    " S_recorder_out_header_type ": output sound header choice\n\
    " S_recorder_srate ": sampling rate of recording\n\
    " S_recorder_trigger ": amplitude at which to start recording (if set)\n",

#else
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
again to finish the recording.",
#endif
		      WITH_WORD_WRAP,
		      record_xrefs,
		      NULL);
}


/* ---------------- Headers etc ---------------- */

static char *header_and_data_xrefs[10] = {
  "data format discussion: {" S_data_format "}",
  "data format constants: {" S_mus_data_format_name "}",
  "header type discussion: {" S_header_type "}",
  "header type constants: {" S_mus_header_type_name "}",
  "MPEG support: mpg in examp." XEN_FILE_EXTENSION,
  "OGG support: read-ogg in examp." XEN_FILE_EXTENSION,
  "Speex support: read-speex in examp." XEN_FILE_EXTENSION,
  "Flac support: read-flac in examp." XEN_FILE_EXTENSION,
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
    RF64\n\
    IRCAM (old style)\n\
    NIST-sphere\n\
    no header ('raw')\n\
\n\n\
  read-only (in selected data formats):\n\
    8SVX (IFF), EBICSF, INRS, ESPS, SPPACK, ADC (OGI), AVR, VOC, PVF,\n\
    Sound Tools, Turtle Beach SMP, SoundFont 2.0, Sound Designer I, PSION, MAUD, Kurzweil 2000,\n\
    Gravis Ultrasound, ASF, PAF, CSL, Comdisco SPW, Goldwave sample, omf, quicktime\n\
    Sonic Foundry (w64), SBStudio II, Delusion digital, Digiplayer ST3, Farandole Composer WaveSample,\n\
    Ultratracker WaveSample, Sample Dump exchange, Yamaha SY85, SY99, and TX16, Covox v8, AVI, \n\
    Impulse tracker, Korg, Akai, Turtle Beach\n\
\n\n\
  automatically translated to a readable format:\n\
    IEEE text, Mus10, SAM 16-bit (modes 1 and 4), AVI, \n\
    NIST shortpack, HCOM, Intel, IBM, and Oki (Dialogic) ADPCM, \n\
    G721, G723_24, G723_40, MIDI sample dump, Ogg, Speex, \n\
    Flac, Midi, Mpeg, Shorten, Wavepack, tta (via external programs)\n\
\n\n\
The files can have any number of channels. Data can be either big or little endian. \
The file types listed above as 'automatically translated' are \
decoded upon being opened, translated to some format Snd can read and write, \
and rewritten as a new file with an added (possibly redundant) extension .snd, \
and that file is the one the editor sees from then on.\
\n\n\
" S_data_format " returns the current sound's data format, and " S_header_type " returns \
its header type.",

		      WITH_WORD_WRAP,
		      header_and_data_xrefs,
		      header_and_data_urls);
}


/* ---------------- Initialization File ---------------- */

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
editing operations, or graphing alternatives. These extensions can be loaded at any time.  One of the \
easier ways to start an initialization file is to go to the Options:Preferences dialog, set the choices \
you want, then save those choices.  The initialization file is just program text, so you can edit it and so on.",

#else
"Snd depends heavily on the extension language to provide much of its functionality.  Since none \
is loaded, there's not much customization you can do.  Check out the X resource stuff in Snd.ad or \
the gtk resource stuff in Snd.gtkrc.",
#endif
		      WITH_WORD_WRAP,
		      init_file_xrefs,
		      init_file_urls);
}


/* ---------------- Key Bindings ---------------- */

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

static bool find_unbuckified_keys(int key, int state, bool cx, char *ignored, XEN func)
{
  if ((key > 256) && (state == 0) && (!cx) && (XEN_BOUND_P(func)))
    show_key_help(key, state, cx, key_binding_description(key, state, cx));
  return(false);
}

static bool find_buckified_keys(int key, int state, bool cx, char *ignored, XEN func)
{
  if ((key > 256) && (state == snd_ControlMask) && (!cx) && (XEN_BOUND_P(func)))
    show_key_help(key, state, cx, key_binding_description(key, state, cx));
  return(false);
}

static bool find_unbuckified_cx_keys(int key, int state, bool cx, char *ignored, XEN func)
{
  if ((key > 256) && (state == 0) && (cx) && (XEN_BOUND_P(func)))
    show_key_help(key, state, cx, key_binding_description(key, state, cx));
  return(false);
}

static bool find_buckified_cx_keys(int key, int state, bool cx, char *ignored, XEN func)
{
  if ((key > 256) && (state == snd_ControlMask) && (cx) && (XEN_BOUND_P(func)))
    show_key_help(key, state, cx, key_binding_description(key, state, cx));
  return(false);
}

static bool find_leftover_keys(int key, int state, bool cx, char *ignored, XEN func)
{
  if ((key > 256) && (state & snd_MetaMask))
    show_key_help(key, state, cx, key_binding_description(key, state, cx));
  return(false);
}

void key_binding_help(void)
{
  #if HAVE_SCHEME
    #define bind_key_example "(bind-key \"End\" 0\n      (lambda () \"view full sound\"\n        (set! (x-bounds) (list 0.0 (/ (frames) (srate))))))"
  #endif
  #if HAVE_RUBY
    #define bind_key_example "bind_key(\"End\", 0,\n       lambda do ||\n         set_x_bounds([0.0, frames.to_f / srate.to_f])\n       end)"
  #endif
  #if HAVE_FORTH
    #define bind_key_example "\"End\" 0\n       lambda: <{ -- val }> doc\"view full sound\"\n         '( 0.0  #f #f #f frames  #f srate  f/ ) #f #f set-x-bounds ; bind-key"
  #endif

  int i;
  snd_help_with_xrefs("Key bindings",

#if HAVE_EXTENSION_LANGUAGE
"Although Snd has a number of built-in key bindings (listed below), you can redefine \
any key via:\n\
\n\
  " S_bind_key " (key state func :optional extended origin)\n\
    this function causes 'key' (an integer or a key name) with \n\
    modifiers 'state' (and preceding C-x if 'extended') to evaluate\n\
    'func'.  For example, to set the End key to cause the full sound\n\
    to be displayed:\n\n\
    " bind_key_example "\n\n\nKey Bindings:\n",

#else
"If there's no extension language, you're stuck with the built-in key bindings:",
#endif
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


/* ---------------- Play ---------------- */

void play_help(void)
{
  #if HAVE_SCHEME
    #define play_cursor_example "(play (cursor))"
    #define play_file_example "(play \"oboe.snd\")"
    #define play_previous_version_example "(play 0 #f #f #f #f (1- (edit-position)))"
    #define H_cursor_line S_cursor_line
  #endif
  #if HAVE_RUBY
    #define play_cursor_example "play(cursor())"
    #define play_file_example "play(\"oboe.snd\")"
    #define play_previous_version_example "play(0, false, false, false, false, edit_position() - 1)"
    #define H_cursor_line "Cursor_line"
  #endif
  #if HAVE_FORTH
    #define play_cursor_example "cursor play"
    #define play_file_example "\"oboe.snd\" play"
    #define play_previous_version_example "0 #f #f #f #f 0 0 edit-position 1- play"
    #define H_cursor_line S_cursor_line
  #endif

  snd_help_with_xrefs("Play",

#if HAVE_EXTENSION_LANGUAGE
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
Except in the browsers, what is actually played depends on the control panel.\
\n\n\
The following functions are related to playing sounds:\n\
\n\
  " S_play " (:optional samp snd chn sync end edpos stop-proc out-chan)\n\
    To play from the cursor: " play_cursor_example "\n\
    To play a file: " play_file_example "\n\
    To play the previous version of a sound: \n\
      " play_previous_version_example "\n\
\n\
    To get a \"tracking cursor\" (a moving line that tries to follow along)\n\
      set " S_with_tracking_cursor " to " PROC_TRUE ", and " S_tracking_cursor_style "\n\
      to " H_cursor_line ".\n\
\n\
  " S_play_and_wait " (:optional samp snd chn sync end edpos stop-proc out-chan)\n\
    a version of 'play' that does not return until the playing is complete.\n\
\n\
  " S_play_channel " (:optional beg dur snd chn edpos stop-proc out-chan)\n\
  " S_play_mix " (mix :optional beg)\n\
  " S_play_region " (:optional reg wait stop-func)\n\
  " S_play_selection " (:optional wait pos stop-proc)\n\
  " S_play_track " (track-id :optional chn beg)\n\
\n\
  " S_pausing ": set to " PROC_TRUE " to pause output\n",

#else
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
#endif

		      WITH_WORD_WRAP,
		      snd_xrefs("Play"),
		      snd_xref_urls("Play"));

  append_key_help("C-q", snd_K_q, snd_ControlMask, true, true);
}


/* ---------------- Reverb ---------------- */

void reverb_help(void)
{
  #if HAVE_SCHEME
    #define reverb_control_length_bounds_example "(set! (reverb-control-length-bounds) (list 0.0 10.0))"
    #define reverb_control_p_example "(set! (reverb-control?) #t)"
    #define mention_hidden_controls "\nThe lowpass and feedback controls are accessible from the \"Hidden controls\" dialog in snd-motif.scm and snd-gtk.scm."
  #endif
  #if HAVE_RUBY
    #define reverb_control_length_bounds_example "set_reverb_control_length_bounds([0.0, 10.0])"
    #define reverb_control_p_example "set_reverb_control?(true)"
    #define mention_hidden_controls ""
  #endif
  #if HAVE_FORTH
    #define reverb_control_length_bounds_example "'( 0.0 10.0 ) set-reverb-control-length-bounds"
    #define reverb_control_p_example "#t set-reverb-control?"
    #define mention_hidden_controls ""
  #endif

  snd_help_with_xrefs("Reverb",

#if HAVE_EXTENSION_LANGUAGE
"The reverb in the control panel is a version of Michael McNabb's Nrev (if it seems to be \
non-functional, make sure the reverb button on the far right is clicked).  There are other \
reverbs mentioned in the related topics list.  The control panel reverb functions are:\n\
\n\
  " S_reverb_control_decay " (:optional snd):\n\
    length (in seconds) of the reverberation after the sound has finished\n\
\n\
  " S_reverb_control_feedback " (:optional snd):\n\
    reverb feedback coefficient\n\
\n\
  " S_reverb_control_length " (:optional snd):\n\
    reverb delay line length scaler. \n\
    Longer reverb simulates a bigger hall.\n\
\n\
  " S_reverb_control_length_bounds " (:optional snd):\n\
    reverb-control-length min and max amounts as a list\n\
      " reverb_control_length_bounds_example "\n\
\n\
  " S_reverb_control_lowpass " (:optional snd):\n\
    reverb low pass filter coefficient. (filter in feedback loop).\n\
\n\
  " S_reverb_control_p " (:optional snd):\n\
    #t if the reverb button is set (i.e. the reverberator is active).\n\
      " reverb_control_p_example "\n\
\n\
  " S_reverb_control_scale " (:optional snd):\n\
    reverb amount (the amount of the direct signal sent to the reverb).\n\
\n\
  " S_reverb_control_scale_bounds " (:optional snd):\n\
    reverb-control-scale min and max amounts as a list.\n" mention_hidden_controls "\n",

#else
"The reverb in the control panel is a version of Michael McNabb's Nrev (if it seems to be \
non-functional, make sure the reverb button on the far right is clicked).",
#endif


		      WITH_WORD_WRAP,
		      snd_xrefs("Reverb"),
		      snd_xref_urls("Reverb"));
}


/* ---------------- Save ---------------- */
void save_help(void)
{
  snd_help_with_xrefs("Save",
"To save the current edited state of a file, use the File:Save option (to overwrite the old version of the \
file), or File:Save as (to write to a new file, leaving the old file unchanged).  The equivalent keyboard \
command is C-x C-s (save).  Other related keyboard commands are C-x w (save selection as file), and \
C-x C-w (extract and save the current channel as a file). Normally, if the new file already exists, and it is \
not currently being edited in Snd, it is silently overwritten. \
If you try to overwrite a file, and \
that file has active edits in a different Snd window, you'll be asked for confirmation. \
If you want Snd to ask before overwriting a file in any case, set the variable " S_ask_before_overwrite " to " PROC_TRUE ".\n",

		      WITH_WORD_WRAP,
		      snd_xrefs("Save"),
		      snd_xref_urls("Save"));

  append_key_help("C-x w", snd_K_w, 0, true,
    append_key_help("C-x C-w", snd_K_w, snd_ControlMask, true,
      append_key_help("C-x C-s", snd_K_s, snd_ControlMask, true, true)));
}


/* ---------------- Filter ---------------- */

void filter_help(void)
{
  #if HAVE_SCHEME
    #define filter_sound_env_example "(filter-sound '(0 1 1 0) 1024)"
    #define filter_sound_vct_example "(filter-sound (vct .1 .2 .3 .3 .2 .1) 6)"
    #define filter_sound_clm_example "(filter-sound (make-filter 2 (vct 1 -1) (vct 0 -0.99)))"
  #endif
  #if HAVE_RUBY
    #define filter_sound_env_example "filter_sound([0.0, 1.0, 1.0, 0.0], 1024)"
    #define filter_sound_vct_example "filter_sound(vct(0.1, 0.2, 0.3, 0.3, 0.2, 0.1), 6)"
    #define filter_sound_clm_example "filter_sound(make_filter(2, vct(1.0, -1.0), vct(0.0, -0.99)))"
  #endif
  #if HAVE_FORTH
    #define filter_sound_env_example "'( 0.0 1.0 1.0 0.0 ) 1024 filter-sound"
    #define filter_sound_vct_example "'( .1 .2 .3 .3 .2 .1 ) list->vct 6 filter-sound"
    #define filter_sound_clm_example "2 '( 1 -1 ) list->vct '( 0.0 -0.99 ) list->vct make-filter filter-sound"
  #endif

  snd_help_with_xrefs("Filter",

#if HAVE_EXTENSION_LANGUAGE
"There is an FIR Filter in the control panel, a filtering option in the envelope editor dialog, and a variety of other filters scattered around; \
see clm.html, dsp." XEN_FILE_EXTENSION " and analog-filters." XEN_FILE_EXTENSION " in particular. The \
built-in filtering functions include: \n\
\n\
  " S_filter_channel " (env :optional order beg dur snd chn edpos trunc origin)\n\
    apply FIR filter to channel; 'env' is frequency response.\n\
\n\
  " S_filter_selection " (env :optional order truncate)\n\
    apply FIR filter with frequency response 'env' to the selection.\n\
\n\
  " S_filter_sound " (env :optional order snd chn edpos origin)\n\
    apply FIR filter with freq response, clm gen, or coeffs 'env'\n\
\n\
    " filter_sound_env_example "\n\
    " filter_sound_vct_example "\n\
    " filter_sound_clm_example "\n\
\n\
The control filter functions are:\n\
\n\
  " S_filter_control_coeffs " (:optional snd)\n\
    filter coefficients (read-only currently)\n\
\n\
  " S_filter_control_envelope " (:optional snd)\n\
    filter (frequency reponse) envelope\n\
\n\
  " S_filter_control_in_dB " (:optional snd)\n\
    The filter dB button. If " PROC_TRUE ", the graph is displayed in dB.\n\
\n\
  " S_filter_control_in_hz " (:optional snd)\n\
    If " PROC_TRUE ", the filter frequency response envelope x axis is in Hz,\n\
    otherwise 0 to 1.0 (where 1.0 corresponds to srate/2).\n\
\n\
  " S_filter_control_order " (:optional snd)\n\
    The filter order\n\
\n\
  " S_filter_control_p " (:optional snd)\n\
    " PROC_TRUE " if the filter is active.\n\
\n\
  " S_filter_control_waveform_color "\n\
    The filter frequency response waveform color.\n",

#else
"There is an FIR Filter in the control panel, and a filtering option in the Edit envelope dialog.",
#endif

		      WITH_WORD_WRAP,
		      snd_xrefs("Filter"),
		      snd_xref_urls("Filter"));
}


/* ---------------- Resample ---------------- */

void resample_help(void)
{
  #if HAVE_SCHEME
    #define src_number_example "(src-channel 2.0) ; go twice as fast"
    #define src_env_example "(src-channel '(0 1 1 2))"
    #define src_clm_example "(src-channel (make-env '(0 1 1 2) :end (frames)))"
    #define H_speed_control_as_float S_speed_control_as_float
    #define H_speed_control_as_ratio S_speed_control_as_ratio
    #define H_speed_control_as_semitone S_speed_control_as_semitone
    #define H_src_duration "src-duration"
  #endif
  #if HAVE_RUBY
    #define src_number_example "src_channel(2.0) # go twice as fast"
    #define src_env_example "src_channel([0.0, 1.0, 1.0, 2.0])"
    #define src_clm_example "src_channel(make_env([0.0, 1.0, 1.0, 2.0], :end, frames()))"
    #define H_speed_control_as_float "Speed_control_as_float"
    #define H_speed_control_as_ratio "Speed_control_as_ratio"
    #define H_speed_control_as_semitone "Speed_control_as_semitone"
    #define H_src_duration "src_duration"
  #endif
  #if HAVE_FORTH
    #define src_number_example "2.0 src-channel \\ go twice as fast"
    #define src_env_example "'( 0.0 1.0 1.0 2.0 ) src-channel"
    #define src_clm_example "'( 0.0 1.0 1.0 2.0 ) :end 0 0 -1 frames make-env src-channel"
    #define H_speed_control_as_float S_speed_control_as_float
    #define H_speed_control_as_ratio S_speed_control_as_ratio
    #define H_speed_control_as_semitone S_speed_control_as_semitone
    #define H_src_duration "src-duration"
  #endif

  snd_help_with_xrefs("Resample",

#if HAVE_EXTENSION_LANGUAGE
"There is a sampling rate changer in the control panel, and a resampling option in the envelope \
editor dialog; see also clm.html and examp." XEN_FILE_EXTENSION ". \
The basic resampling functions are:\n\
\n\
  " S_src_channel " (num-or-env :optional beg dur snd chn edpos)\n\
    resample (change the speed/pitch of) a channel.\n\
    'num-or-env' can be the resampling ratio (higher=faster)\n\
    an envelope, or a CLM env generator:\n\
\n\
      " src_number_example "\n\
      " src_env_example "\n\
      " src_clm_example "\n\
\n\
  In the envelope case, the function " H_src_duration " can help \n\
  you predict the result's duration.\n\
\n\
  " S_src_selection " (num-or-env :optional base)\n\
    apply sampling rate conversion to the selection\n\
\n\
  " S_src_sound " (num-or-env :optional base snd chn edpos)\n\
    resample sound\n\
\n\
The control panel 'speed' functions are:\n\
\n\
  " S_speed_control " (:optional snd)\n\
    current speed (sampling rate conversion factor)\n\
\n\
  " S_speed_control_bounds " (:optional snd)\n\
    speed-control min and max amounts as a list.\n\
    The default is (list 0.05 20.0). If no 'snd' argument\n\
    is given, this affects Mix, Track, and View:Files dialogs.\n\
\n\
  " S_speed_control_style " (:optional snd)\n\
    The speed control can be a float, (" H_speed_control_as_float "),\n\
    a ratio of integers (" H_speed_control_as_ratio "), or a step \n\
    in a (possibly microtonal) scale (" H_speed_control_as_semitone ")\n\
\n\
  " S_speed_control_tones " (:optional snd)\n\
    number of tones per octave in the " H_speed_control_as_semitone "\n\
    speed style (default: 12).",

#else
"There is a sampling rate changer in the control panel, and a resampling option in the Edit envelope dialog.",
#endif

		      WITH_WORD_WRAP,
		      snd_xrefs("Resample"),
		      snd_xref_urls("Resample"));
}


/* ---------------- Insert ---------------- */

void insert_help(void)
{
  snd_help_with_xrefs("Insert",

#if HAVE_EXTENSION_LANGUAGE
"To insert a file, use C-x C-i, and to insert the selection C-x i.  C-o inserts a \
zero sample at the cursor.  There are also the File:Insert and Edit:Insert Selection \
dialogs. The insertion-related functions are:\n\
\n\
  " S_pad_channel " (beg dur :optional snd chn edpos)\n\
    insert 'dur' zeros at 'beg'\n\
\n\
  " S_insert_silence " (beg num :optional snd chn)\n\
    insert 'num' zeros at 'beg'\n\
\n\
  " S_insert_region " (:optional beg reg snd chn)\n\
    insert region 'reg' at sample 'beg'\n\
\n\
  " S_insert_selection " (:optional beg snd chn)\n\
    insert selection starting at 'beg'\n\
\n\
  " S_insert_sample " (samp value :optional snd chn edpos)\n\
    insert sample 'value' at sample 'samp'\n\
\n\
  " S_insert_samples " (samp samps data :optional snd chn pos del org)\n\
    insert 'samps' samples of 'data' (normally a vct) starting at\n\
    sample 'samp'.  'data' can also be a filename.\n\
\n\
  " S_insert_sound " (file :optional beg in-chan snd chn pos del)\n\
    insert channel 'in-chan' of 'file' at sample 'beg' (cursor position\n\
    by default).  If 'in-chan' is not given (or is " PROC_FALSE "), all\n\
    channels are inserted.\n",

#else
"To insert a file, use C-x C-i, and to insert the selection C-x i.  C-o inserts a \
zero sample at the cursor.  There are also the File:Insert and Edit:Insert Selection dialogs.",
#endif

		      WITH_WORD_WRAP,
		      snd_xrefs("Insert"),
		      snd_xref_urls("Insert"));

  append_key_help("C-x i", snd_K_i, 0, true,
    append_key_help("C-o", snd_K_o, snd_ControlMask, false,
      append_key_help("C-x C-i", snd_K_i, snd_ControlMask, true, true)));
}


/* ---------------- Delete ---------------- */

void delete_help(void)
{
  snd_help_with_xrefs("Delete",

#if HAVE_EXTENSION_LANGUAGE
"To delete a sample, use C-d; to delete the selection, C-w.  The main deletion-related \
functions are:\n\
\n\
  " S_delete_sample " (samp :optional snd chn edpos)\n\
    delete sample number 'samp'.\n\
\n\
  " S_delete_samples " (samp samps :optional snd chn edpos)\n\
    delete 'samps' samples starting at 'samp'\n\
\n\
  " S_delete_selection " (): delete selected portions.\n\
  " S_delete_mark " (id): delete mark 'id'\n\
  " S_delete_mix " (id): delete mix 'id'\n\
  " S_delete_track " (id): delete track 'id'\n",

#else
"To delete a sample, use C-d; to delete the selection, C-w, or the Edit menu Delete Selection option.",
#endif

		      WITH_WORD_WRAP,
		      snd_xrefs("Delete"),
		      snd_xref_urls("Delete"));

  append_key_help("C-w", snd_K_w, snd_ControlMask, false,
    append_key_help("C-d", snd_K_d, snd_ControlMask, false, true));
}


/* ---------------- Regions ---------------- */

void region_help(void)
{
  #if HAVE_SCHEME
    #define region_to_vct_example "(region->vct 0 0 reg) ; len=0 => entire region"
    #define save_region_example "(save-region reg :file \"reg0.snd\" :header-type mus-next)"
  #endif
  #if HAVE_RUBY
    #define region_to_vct_example "region2vct(0, 0, reg) # len=0 => entire region"
    #define save_region_example "save_region(reg, :file, \"reg0.snd\", :header_type, Mus_next)"
  #endif
  #if HAVE_FORTH
    #define region_to_vct_example "0 0 reg region->vct \\ len=0 => entire region"
    #define save_region_example "reg :file \"reg0.snd\" :header-type mus-next save-region"
  #endif

  snd_help_with_xrefs("Region",
#if HAVE_EXTENSION_LANGUAGE
"A region is a saved portion of the sound data. When a sound portion is selected, it is (by default) saved \
as the new region; subsequent edits will not affect the region data. You can disable the region creation \
by setting the variable " S_selection_creates_region " to " PROC_FALSE " (its default is " PROC_TRUE " which can slow down editing \
of very large sounds). Regions can be defined by " S_make_region ", by dragging the mouse through a portion \
of the data, or via the Select All menu option. If the mouse drags off the end of the graph, the x axis \
moves, in a sense dragging the data along to try to keep up with the mouse; the further away the mouse \
is from the display, the faster the axis moves. A region can also be defined with keyboard commands, \
much as in Emacs. C-[space] starts the region definition and the various cursor moving commands \
continue the definition.  As regions are defined, the new ones are pushed on a stack, and if enough regions already exist, \
old ones are pushed off (and deleted) to make room. Each region has a unique id returned \
by " S_make_region " and shown beside the region name in the Region Browser. \
Most of the region arguments below default to the current region (the top of the regions stack). \
The main region-related functions are:\n\
\n\
  " S_view_regions_dialog ": start the Region browser\n\
  " S_save_region_dialog ": start the Save region dialog\n\
\n\
  " S_insert_region " (:optional beg reg snd chn)\n\
    insert region 'reg' at sample 'beg'\n\
\n\
  " S_mix_region " (:optional samp reg snd chn track)\n\
    mix in region 'reg' at sample 'samp' (defaults to the cursor sample)\n\
\n\
  " S_play_region " (:optional reg wait stop-func)\n\
    play region\n\
\n\
  " S_save_region " (reg :optional-key :file :header-type :data-format :comment)\n\
    save region 'reg' in 'file' in data format (default is mus-bshort),\n\
    header type (default is mus-next), and comment. Return the output\n\
    filename.\n\
\n\
      " save_region_example "\n\
\n\
  " S_region_to_vct " (:optional samp samps reg chn v)\n\
    return a vct containing 'samps' samples starting at 'samp'\n\
    in region 'reg's channel 'chn'. If 'v' (a vct) is provided,\n\
    it is filled, rather than creating a new vct.\n\
\n\
      " region_to_vct_example "\n\
\n\
  " S_make_region " (:optional beg end snd chn)\n\
    create a new region spanning samples 'beg' to 'end';\n\
    return the new region's id.\n\
\n\
  " S_forget_region " (:optional reg): remove region from region list\n\
\n\
  " S_region_p " (reg): " PROC_TRUE " if 'reg' is a known region id\n\
\n\
  " S_regions ": list of currently active regions\n\
\n\
  " S_max_regions ": how big the region stack can get\n\
\n\
  " S_selection_creates_region ": does making a selection create a region\n\
\n\
  " S_region_chans " (:optional reg): region chans\n\
  " S_region_frames " (:optional reg chan): region length\n\
  " S_region_maxamp " (:optional reg): region maxamp\n\
  " S_region_maxamp_position " (:optional reg): maxamp location (sample number)\n\
  " S_region_position " (:optional reg chan): original begin time of region\n\
  " S_region_sample " (:optional samp reg chan): sample value\n\
  " S_region_srate " (:optional reg): original data's sampling rate\n",
#else
"A region is a portion of the sound data. When a sound portion is selected, it is (by default) saved \
as the new region; subsequent edits will not affect the region data. \
of very large sounds). Regions can be defined by dragging the mouse through a portion \
of the data, or via the Select All menu option. If the mouse drags off the end of the graph, the x axis \
moves, in a sense dragging the data along to try to keep up with the mouse; the further away the mouse \
is from the display, the faster the axis moves. A region can also be defined with keyboard commands, \
much as in Emacs. C-[space] starts the region definition and the various cursor moving commands \
continue the definition.",
#endif
		      WITH_WORD_WRAP,
		      snd_xrefs("Region"),
		      snd_xref_urls("Region"));

  append_key_help("C-[space]", snd_K_space, snd_ControlMask, false, true);
}


/* ---------------- Selections ---------------- */

void selection_help(void)
{
  #if HAVE_SCHEME
    #define env_selection_example "(env-selection '(0 0 1 1 2 0))"
    #define save_selection_example "(save-selection \"sel.snd\" :channel 1)"
    #define scale_selection_list_example "(scale-selection-by '(0.0 2.0))"
    #define scale_selection_number_example "(scale-selection-by 2.0)"
  #endif
  #if HAVE_RUBY
    #define env_selection_example "env_selection([0.0, 0.0, 1.0, 1.0, 2.0, 0.0])"
    #define save_selection_example "save_selection(\"sel.snd\", :channel, 1)"
    #define scale_selection_list_example "scale_selection_by([0.0, 2.0])"
    #define scale_selection_number_example "scale_selection_by(2.0)"
  #endif
  #if HAVE_FORTH
    #define env_selection_example "'( 0.0 0.0 1.0 1.0 2.0 0.0 ) env-selection"
    #define save_selection_example "\"sel.snd\" :channel 1 save-selection"
    #define scale_selection_list_example "'( 0.0 2.0 ) scale-selection-by"
    #define scale_selection_number_example "2.0 scale-selection-by"
  #endif

  snd_help_with_xrefs("Selection",

#if HAVE_EXTENSION_LANGUAGE
"The selection is a high-lighted portion of the current sound. \
You can create it by dragging the mouse through the data, or via various functions. \
The primary selection-related functions are:\n\
\n\
  " S_convolve_selection_with " (file :optional amp)\n\
    convolve the selected portion with 'file'\n\
\n\
  " S_delete_selection " (): delete the selected portion\n\
\n\
  " S_env_selection " (envelope :optional env-base)\n\
    apply amplitude envelope to selected portion\n\
\n\
      " env_selection_example "\n\
\n\
  " S_filter_selection " (env :optional order truncate)\n\
    apply an FIR filter with frequency response 'env' to the \n\
    selection. env can be the filter coefficients themselves in\n\
    a vct with at least 'order' elements, or a CLM filter\n\
\n\
  " S_insert_selection " (:optional beg snd chn)\n\
    insert (a copy of) selection starting at 'beg'\n\
\n\
  " S_mix_selection " (:optional beg snd chn)\n\
    mix (add) selection starting at 'beg'\n\
\n\
  " S_play_selection " (:optional wait pos stop-proc)\n\
    play the selection\n\
\n\
  " S_reverse_selection "(): reverse selected portion\n\
\n\
  " S_save_selection " (:optional-key :file (:header-type mus-next)\n\
                           :data-format :srate :comment :channel)\n\
    save the selection in file. If channel is given, save\n\
    only that channel.\n\
\n\
      " save_selection_example "\n\
\n\
  " S_scale_selection_by " (scalers)\n\
    scale the selection by 'scalers' which can be either a float,\n\
    a list of floats, or a vct. In a multichannel selection, each\n\
    member of the vct or list is applied to the next channel in the\n\
    selection. " scale_selection_list_example " scales the first\n\
    channel by 0.0, the second (if any) by 2.0.\n\
    " scale_selection_number_example " scales all channels by 2.0.\n\
\n\
  " S_scale_selection_to " (norms)\n\
    normalize the selection to norms which can be either a float,\n\
    a list of floats, or a vct.\n\
\n\
  " S_select_all " (:optional snd chn)\n\
    select all samples\n\
\n\
  " S_smooth_selection "()\n\
    apply a smoothing function to the selection. \n\
    This produces a sinusoid between the end points.\n\
\n\
  " S_src_selection " (num-or-env :optional base)\n\
    apply sampling rate conversion to the selection\n\
\n\
  " S_save_selection_dialog " (:optional managed)\n\
    start the Save Selection dialog.\n\
\n\
  " S_selection_creates_region "\n\
    if " PROC_TRUE ", a region is created whenever a selection is made.\n\
\n\
  " S_selection_chans ": chans in selection\n\
  " S_selection_color ": color to highlight selection\n\
  " S_selection_frames " (:optional snd chn): length of selection\n\
  " S_selection_maxamp " (:optional snd chn): maxamp of selection\n\
  " S_selection_maxamp_position " (:optional snd chn): sample number of maxamp\n\
  " S_selection_member " (:optional snd chn): \n\
    " PROC_TRUE " if snd's channel chn has selected data.\n\
  " S_selection_p ": " PROC_TRUE " if there's a selection\n\
  " S_selection_position " (:optional snd chn): \n\
    sample number where selection starts\n\
  " S_selection_srate ": nominal srate of selected portion.\n\
\n\
There are many more selection-oriented functions scattered around the various *." XEN_FILE_EXTENSION " files. \
See the related topics list below.",

#else
"The selection is a high-lighted portion of the current sound. \
You can create it by dragging the mouse.",
#endif
		      WITH_WORD_WRAP,
		      snd_xrefs("Selection"),
		      snd_xref_urls("Selection"));
}


/* ---------------- Colors ---------------- */

void colors_help(void)
{
  #if HAVE_SCHEME
    #define make_color_example "(define blue (make-color 0.0 0.0 1.0))"
    #define set_basic_color_example "(set! (basic-color) blue)"
  #endif
  #if HAVE_RUBY
    #define make_color_example "Blue = make_color(0.0, 0.0, 1.0)"
    #define set_basic_color_example "set_basic_color(Blue)"
  #endif
  #if HAVE_FORTH
    #define make_color_example ": blue 0 0 1 make-color ;"
    #define set_basic_color_example "blue set-basic-color"
  #endif

  snd_help_with_xrefs("Colors",

#if HAVE_EXTENSION_LANGUAGE
"A color in Snd is an object with three fields representing the rgb (red green blue) settings \
as numbers between 0.0 and 1.0. A color object is created via " S_make_color ":\n\
  " make_color_example "\n\
\n\
This declares the variable \"blue\" and gives it the value of the color whose rgb components \
include only blue in full force. The X11 color names are defined in rgb." XEN_FILE_EXTENSION ". The overall widget background color is " S_basic_color ".\n\
\n\
  " set_basic_color_example "\n\
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
\n\
The easiest way to try out other colors is to use the Options:Preferences dialog. \
The sonogram colors can be set in the View:Colors dialog.",

#else
"To change the various Snd colors, use the Options:Preferences Dialog. The sonogram colors can be set in the View:Colors dialog.",
#endif
		      WITH_WORD_WRAP,
		      snd_xrefs("Color"),
		      snd_xref_urls("Color"));
}



/* -------- dialog help texts -------- */

/* ---------------- Envelope Editor ---------------- */

void envelope_editor_dialog_help(void)
{
  #if HAVE_SCHEME
    #define define_envelope_name "defvar, define, or define-envelope"
    #define ramp_envelope_example "'(0 0 1 1)"
    #define define_envelope_example "  (defvar ramp '(0 0 1 1))\n  (define-envelope pyramid '(0 0 1 1 2 0))"
  #endif
  #if HAVE_RUBY
    #define define_envelope_name "define_envelope"
    #define ramp_envelope_example "[0.0, 0.0, 1.0, 1.0]"
    #define define_envelope_example "  define_envelope(\"ramp\", [0.0, 0.0, 1.0, 1.0])\n  define_envelope(\"pyramid\", [0.0, 0.0, 1.0, 1.0, 2.0, 0.0])"
  #endif
  #if HAVE_FORTH
    #define define_envelope_name "define-envelope"
    #define ramp_envelope_example "'( 0.0 0.0 1.0 1.0 )"
    #define define_envelope_example "  \"ramp\" '( 0.0 0.0 1.0 1.0 ) define-envelope\n  \"pyramid\" '( 0.0 0.0 1.0 1.0 2.0 0.0 ) define-envelope"
  #endif
  #if (!HAVE_EXTENSION_LANGUAGE)
    #define define_envelope_name "<needs extension language>"
    #define ramp_envelope_example "'(0 0 1 1)"
    #define define_envelope_example "<needs extension language>"
  #endif

  snd_help_with_xrefs("Envelope Editor",
"The Edit Envelope dialog (under the Edit menu) opens a window for viewing and editing envelopes. \
The dialog has a display showing either the envelope currently being edited or \
a panorama of all currently loaded envelopes.  The current envelope can be edited with the mouse: click at some spot in the graph to place a \
new breakpoint, drag an existing breakpoint to change its position, and click an existing breakpoint to delete it. \
The Undo and Redo buttons can be used to move around in the list of envelope edits; the current state \
of the envelope can be saved with the 'save' button, or printed with 'print'. Envelopes can be defined via " define_envelope_name ": \
\n\n" define_envelope_example "\n\n\
defines two envelopes that can be used in Snd wherever an envelope is needed (e.g. C-x C-a).  You can also define \
a new envelope in the dialog's text field; " ramp_envelope_example " creates a ramp as a new envelope. \
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

  append_key_help("C-x C-a", snd_K_a, snd_ControlMask, true, true);
}


/* ---------------- Transform Options ---------------- */

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
desired member of that family of windows. The lower (second) scale sets the 'mu' parameter in the ultraspherical window. \
\n\n\
If the 'selection' button is not set, the FFT is taken from the start (the left edge) of the \
current window and is updated as the window bounds change; otherwise the FFT is taken over the extent \
of the selection, if any is active in the current channel.  The fft data is scaled to fit \
between 0.0 and 1.0 unless the fft normalization is off. The full frequency axis is normally \
displayed, but the axis is 'draggable' -- put the mouse on the axis and drag it either way to change \
the range (this is equivalent to changing the variable " S_spectro_cutoff "). You can also click on \
any point in the fft to get the associated fft data displayed; if " S_with_verbose_cursor " is on, you can \
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


/* ---------------- Color Dialog ---------------- */

static char *color_dialog_xrefs[9] = {
  "colormap variable: {colormap}",
  "colormap constants: rgb." XEN_FILE_EXTENSION,
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

#if HAVE_EXTENSION_LANGUAGE
"This dialog sets the colormap and associated settings used during sonogram, spectrogram,  \
and wavogram display. The cutoff scale refers to the minimum data value to be displayed. \
You can add your own colormaps to the list via " S_add_colormap ", or delete one with " S_delete_colormap ".",
#else
"This dialog sets the colormap and associated variables used during sonogram, spectrogram,  \
and wavogram display. The cutoff scale refers to the minimum data value to be displayed.",
#endif
		      WITH_WORD_WRAP,
		      color_dialog_xrefs,
		      NULL);
}


/* ---------------- Orientation Dialog ---------------- */

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
along a given axis, 'hop' refers to the density of the traces (the jump in samples between successive \
ffts or time domain scans), and 'percent of spectrum' is equivalent to dragging the fft frequency axis -- it changes \
the amount of the spectrum that is displayed.  If the 'use openGL' button is set, the \
spectrogram is drawn by openGL.  In the spectrogram, 'x' refers to the time axis, \
'y' to the amplitude axis, and 'z' to the frequency axis.",
		      WITH_WORD_WRAP,
		      orientation_dialog_xrefs,
		      NULL);
}


/* ---------------- Region Dialog ---------------- */

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


/* ---------------- Raw Sound Dialog ---------------- */

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


void raw_data_dialog_help(const char *info)
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


/* ---------------- Completion Dialog ---------------- */

void completion_dialog_help(void)
{
  snd_help("completion",
	   "These are the completions that Snd thinks might be likely. If you select one, it will be used to complete the current name.",
	   WITH_WORD_WRAP);
}


/* ---------------- Save as Dialog ---------------- */

void save_as_dialog_help(void)
{
  snd_help_with_xrefs("Save As",

"You can save the current state of a file with File:Save As, or the current selection with Edit:Save as. \
The output header type, data format, sampling rate, and comment can also be set.  Setting the srate \
does not affect the data -- it is just a number placed in the sound file header. \
If a file by the chosen name already exists \
it is overwritten, unless that file is already open in Snd and has edits.  In that case,  \
you'll be asked what to do.  If you want to be warned whenever a file is about to be overwritten by this \
option, set the variable " S_ask_before_overwrite " to " PROC_TRUE ". \
If you give the current file name to Save As,  \
any current edits will be saved and the current version in Snd will be updated (that is, in this \
case, the edit tree is not preserved).  To save (extract) just one channel of a multichannel file, \
put the (0-based) channel number in the 'extract channel' field, then click 'Extract', rather \
than 'Save'.",
		      WITH_WORD_WRAP,
		      snd_xrefs("Save"),
		      snd_xref_urls("Save"));
}


/* ---------------- Open File ---------------- */

static char *open_file_xrefs[] = {
  "open file: {" S_open_sound "}",
  "add to sound file extension list (for '" S_just_sounds "'): {" S_add_sound_file_extension "}",
  "specialize open: {" S_open_hook "}, {" S_after_open_hook "}, etc",
  "start the file dialog: {" S_open_file_dialog "}",
#if HAVE_SCHEME
  "specialize file list: {install-searcher} in snd-motif.scm",
  "keep dialog active after opening: {keep-file-dialog-open-upon-ok} in snd-motif.scm",
#endif
  NULL};

void open_file_dialog_help(void)
{
  snd_help_with_xrefs("Open File",

"The file selection dialog is slightly different from the Gtk or Motif default.  If you single click \
in the directory list, that directory is immediately opened and displayed.  Also there are \
a variety of context-sensitive popup menus to handle special chores such as setting the \
current sort routine (right click over the file list), jump to any higher level directory (right click \
in the directory list), or choose a recently opened file (click in the filename text widget), or \
return to a previous list of files (click in the filter text widget). \
The 'sound files only' button filters out all non-sound files from the files list (using the \
extension -- you can add to the list of sound file extensions via " S_add_sound_file_extension ". \
When a sound file is selected, information about it is posted under the lists, and a 'play' \
button is displayed.  If you have libgamin (fam), it is tied into the file list, so what you \
see should always be up-to-date. The name field has <TAB> completion, of course, and also \
watches as you type a new name, reflecting that partial name by moving the file list to \
display possible matches.",
		      WITH_WORD_WRAP,
		      open_file_xrefs,
		      NULL);
}


/* ---------------- Mix File ---------------- */

void mix_file_dialog_help(void)
{
  snd_help_with_xrefs("Mix File",

"The file will be mixed (added into the selected sound) at the cursor. If you click the 'Sound Files Only' button, \
only those files in the current directory that look vaguely like sound files will be displayed.  For more control \
of the initial mix, use the View:Files dialog.  To edit the mix, use the View:Mixes dialog.",

		      WITH_WORD_WRAP,
		      snd_xrefs("Mix"),
		      snd_xref_urls("Mix"));
}


/* ---------------- Insert File ---------------- */

void insert_file_dialog_help(void)
{
  snd_help_with_xrefs("Insert File",

"The file will be inserted (pasted into the selected file) at the cursor. If you click the 'Sound Files Only' button, \
only those files in the current directory that look vaguely like sound files will be displayed.  For more control \
of the insertion, use the View:Files dialog.",

		      WITH_WORD_WRAP,
		      snd_xrefs("Insert"),
		      snd_xref_urls("Insert"));
}


/* ---------------- Find Dialog ---------------- */

void find_dialog_help(void)
{
  #if HAVE_SCHEME
    #define find_example "(lambda (n) (> n .1))"
    #define zero_plus "zero+"
    #define closure_example "  (define (zero+)\n  (let ((lastn 0.0))\n      (lambda (n)\n        (let ((rtn (and (< lastn 0.0) (>= n 0.0) -1)))\n          (set! lastn n)\n          rtn)))"
  #endif
  #if HAVE_RUBY
    #define find_example "lambda do |y| y > 0.1 end"
    #define zero_plus "zero_plus"
    #define closure_example "def zero_plus\n  lastn = 0.0\n  lambda do |n|\n    rtn = lastn < 0.0 and n >= 0.0 and -1\n    lastn = n\n    rtn\n  end\nend"
  #endif
  #if HAVE_FORTH
    #define find_example "lambda: <{ y }> 0.1 y f< ;"
    #define zero_plus "zero+"
    #define closure_example ": zero+ ( -- prc; n self -- val )\n  lambda-create 0.0 ( lastn ) , latestxt 1 make-proc\n does> { n self -- val }\n  self @ ( lastn ) f0<  n f0>= &&  -1 && { rtn }\n  n self ! ( lastn = n )\n  rtn\n;" 
  #endif

  snd_help_with_xrefs("Global Find",

#if HAVE_EXTENSION_LANGUAGE
"This search travels through all the current channels in parallel until a match is found.  The find \
expression is a function of one argument, the current sample value.  It is evaluated on each sample, and should return " PROC_TRUE " when the \
search is satisfied.  For example, \n\n  " find_example "\n\nlooks for the next sample that is greater than .1. \
If you need to compare the current sample with a previous one, use a 'closure' as in " zero_plus " in \
examp." XEN_FILE_EXTENSION ": \n\n" closure_example "\n\nThere are several other \
search function examples in that file that search for peaks, clicks, or a particular pitch.",
#else
"This search mechanism is built on the extension language, which isn't available in this version of Snd.",
#endif

		      WITH_WORD_WRAP,
		      snd_xrefs("Find"),
		      snd_xref_urls("Find"));
}


/* ---------------- Mix Dialog ---------------- */

void mix_dialog_help(void)
{
  #if HAVE_EXTENSION_LANGUAGE
    #define mix_dialog_mix_help "The function " S_mix_dialog_mix " gives (or sets) the currently displayed mix's id. "
  #else
    #define mix_dialog_mix_help ""
  #endif
  snd_help_with_xrefs("Mixes",

"This dialog provides various commonly-used controls on the currently \
chosen mix.  At the top are the mix id, begin and end times, \
track number, and a play button.  " mix_dialog_mix_help "Beneath that are various sliders \
controlling the speed (sampling rate) of the mix, and the amplitude of each \
input channel; and finally, an envelope editor for the mix's (input) channels. \
The current mix amp env is not actually changed until you click 'Apply Env'.\
The editor envelope is drawn in black with dots whereas the current \
mix amp env (if any) is drawn in blue.",

		      WITH_WORD_WRAP,
		      snd_xrefs("Mix"),
		      snd_xref_urls("Mix"));
}


/* ---------------- Track Dialog ---------------- */

void track_dialog_help(void)
{
  #if HAVE_EXTENSION_LANGUAGE
    #define track_dialog_track_help "The function " S_track_dialog_track " gives (or sets) the currently displayed track's id. "
  #else
    #define track_dialog_track_help ""
  #endif

  snd_help_with_xrefs("Tracks",

"This dialog provides various commonly-used controls on the currently \
chosen track.  At the top are the track id, begin and end times, \
track number, and a play button.  " track_dialog_track_help "Beneath that are various sliders \
controlling the speed (sampling rate) and the amplitude of the track, \
and an envelope editor for the track's overall amplitude envelope. \
The current track's amp env is not actually changed until you click 'Apply Env'.\
The editor envelope is drawn in black with dots whereas the current \
mix amp env (if any) is drawn in blue. ",

		      WITH_WORD_WRAP,
		      snd_xrefs("Track"),
		      snd_xref_urls("Track"));
}



/* ---------------- New File ---------------- */

static char *new_file_xrefs[5] = {
  "open a new sound: {" S_new_sound "}",
  "specialize making a new sound: {" S_new_sound_hook "}",
  "header type constants: {" S_mus_header_type_name "}",
  "data format constants: {" S_mus_data_format_name "}",
  NULL};

void new_file_dialog_help(void)
{
  snd_help_with_xrefs("New File",

#if HAVE_EXTENSION_LANGUAGE
"This dialog sets the new file's output header type, data format, srate, chans, and comment. \
The 'srate:' and 'channels:' labels are actually drop-down menus providing quick access to common choices. \
The default values for the fields can be set by clicking 'Reset'.  These values \
are " S_default_output_chans ", " S_default_output_data_format ", " S_default_output_srate ", and " S_default_output_header_type ".  \
The file name field can be set upon each invocation through " S_output_name_hook ", and the \
comment field via " S_output_comment_hook ".  Click 'Ok' to open the new sound. The actual new file representing the new sound is not written \
until you save the new sound.",
#else
"This dialog sets the new file's output header type, data format, srate, chans, and comment. \
The 'srate:' and 'channels:' labels are actually drop-down menus providing quick access to common choices. \
The default values for the fields can be set by clicking 'Reset'. Click 'Ok' to open the new sound. \
The actual new file representing the new sound is not written \
until you save the new sound.",
#endif
		      WITH_WORD_WRAP,
		      new_file_xrefs,
		      NULL);
}


/* ---------------- Edit Header ---------------- */

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


/* ---------------- Print Dialog ---------------- */

static char *print_xrefs[4] = {
  "default eps file name: {" S_eps_file "}",
  "eps overall size: {" S_eps_size "}",
  "eps margins: {" S_eps_bottom_margin "}, {" S_eps_left_margin "}",
  NULL};

void print_dialog_help(void)
{
  snd_help_with_xrefs("File Print",

#if HAVE_EXTENSION_LANGUAGE
"Print causes the currently active graph(s) to be printed (via the lpr command) or saved as \
a Postscript file.  In the latter case, the file name is set either by the dialog, or taken from the \
variable " S_eps_file " (normally \"snd.eps\").  The functions that refer to this dialog are:\n\
\n\
  " S_print_dialog " (:optional managed print): start the print dialog\n\
  " S_eps_file ": eps file name (\"snd.eps\")\n\
  " S_eps_bottom_margin ": bottom margin (0.0)\n\
  " S_eps_left_margin ": left margin (0.0)\n\
  " S_eps_size ": overall eps size scaler (1.0)\n\
  " S_graph_to_ps " (:optional file): write current graph to eps file\n\n\
For openGL graphics, use " S_gl_graph_to_ps ".\n",
#else
"Print causes the currently active display to be either printed (via the lpr command) or saved as \
an eps file.  Currently the openGL graphics can't be printed by Snd, \
but you can use Gimp or some such program to get a screenshot, and print that.",
#endif

		      WITH_WORD_WRAP,
		      print_xrefs,
		      NULL);
}

/* ---------------- View Files ---------------- */

static char *view_files_xrefs[5] = {
  "place sound in view files list: {" S_add_file_to_view_files_list "}",
  "place all sounds from a directory in view files list: {" S_add_directory_to_view_files_list "}",
  "specialize view files selection: {" S_view_files_select_hook "}",
  "the sort choice: {" S_view_files_sort "}",
  NULL};

void view_files_dialog_help(void)
{
  snd_help_with_xrefs("File Browser",

#if HAVE_EXTENSION_LANGUAGE
"The View:Files dialog provides a list of sounds and various things to do with them.\
The play button plays the file. \
Double click a file name, and that file is opened in Snd.  You can also mix or insert the \
selected file with amplitude envelopes and so on. \
\n\n\
Files can be added to the list via the -p startup switch, and by the functions " S_add_file_to_view_files_list " \
and " S_add_directory_to_view_files_list ". \
\n\n\
The 'sort' label on the right activates a menu of sorting choices; 'name' sorts the \
files list alphabetically, 'date' sorts by date written, and 'size' sorts by the \
number of samples in the sound. The variable " S_view_files_sort " refers to this menu. \
The functions that refer to this dialog are: \n\
\n\
  " S_view_files_dialog " (:optional managed): start this dialog\n\
  " S_add_directory_to_view_files_list "  (dir): add directory's files to list\n\
  " S_add_file_to_view_files_list " (file): add file to list\n\
  " S_view_files_files ": list of all files in dialog's file list\n\
  " S_view_files_selected_files ": list of currently selected files\n\
  " S_view_files_sort ": dialog's sort choice\n\
  " S_view_files_amp ": dialog's amp slider value\n\
  " S_view_files_amp_env ": dialog's amp env\n\
  " S_view_files_speed ": dialog's speed value\n\
  " S_view_files_speed_style ": dialog's speed style\n",
#else
"The View:Files dialog provides a list of sounds and various things to do with them.\
The play button plays the file. \
Double click a file name, and that file is opened in Snd.  You can also mix or insert the \
selected file with amplitude envelopes and so on.",
#endif
		      WITH_WORD_WRAP,
		      view_files_xrefs,
		      NULL);
}


/* ---------------- help dialog special cases ---------------- */

static void copy_help(void)
{
  snd_help_with_xrefs("Copy",
		      "The copying functions are listed in the 'related topics' section. See also 'Save'",
		      WITH_WORD_WRAP,
		      snd_xrefs("Copy"),
		      snd_xref_urls("Copy"));
}

static void cursor_help(void)
{
  snd_help_with_xrefs("Cursor",
"A big '+' marks the current sample.  This is Snd's cursor, and the \
various cursor moving commands apply to it.  See also 'Tracking cursor'",
		      WITH_WORD_WRAP,
		      snd_xrefs("Cursor"),
		      snd_xref_urls("Cursor"));
}

static void tracking_cursor_help(void)
{
  snd_help_with_xrefs("Tracking cursor",
"If you want the cursor to follow along more-or-less in time while \
playing a sound, set " S_with_tracking_cursor " to " PROC_TRUE ". See also 'Cursor'",
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


static char *Wavogram_xrefs[] = {
  "wavogram picture",
  "{wavo-hop}",
  "{wavo-trace}",
  "time-graph-type: {graph-as-wavogram}",
  NULL};

static char *Wavogram_urls[] = {
  "snd.html#wavogram",
  "extsnd.html#wavohop",
  "extsnd.html#wavotrace",
  "extsnd#timegraphtype",
  NULL};

static void wavogram_help(void)
{
  snd_help_with_xrefs("Wavogram",
"The 'wavogram' is a 3-D view of the time-domain data.  If you set each trace length correctly, \
you can often get periods to line up vertically, making a pretty picture.",
		      WITH_WORD_WRAP,
		      Wavogram_xrefs,
		      Wavogram_urls);
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

#define NUM_TOPICS 38
static char *topic_names[NUM_TOPICS] = {
  "Hook", "Vct", "Sample reader", "Mark", "Mix", "Region", "Edit list", "Transform", "Error",
  "Color", "Font", "Graphic", "Widget", "Emacs",
  "CLM", "Instrument", "CM", "CMN", "Libxm", "Sndlib", 
  "Motif", "Gtk", "Script", "Ruby", "LADSPA", "OpenGL", "Gdb", "Control panel",
  "X resources", "Invocation flags", "Initialization file", "Customization",
  "Noise Reduction", "Window Size", "Color", "Random Number", "Wavogram",
  "Forth"
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
  "extsnd.html#noisystory", "extsnd.html#movingwindows", "extsnd.html#colors", "sndscm.html#allrandomnumbers",
  "snd.html#wavogram", "grfsnd.html#sndandforth"
};

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

#define NUM_XREFS 36
static char *xrefs[NUM_XREFS] = {
  "Mark", "Mix", "Region", "Selection", "Cursor", "Tracking cursor", "Delete", "Envelope", "Filter",
  "Search", "Insert", "Maxamp", "Play", "Reverse", "Save", "Smooth", "Resample", "FFT", "Reverb",
  "Src", "Find", "Undo", "Redo", "Sync", "Control panel", "Record", "Header", "Key", "Track", "Copy",
  "Noise Reduction", "Window Size", "Color", "Control", "Random Numbers", "Wavogram"
};

static char **xref_tables[NUM_XREFS] = {
  Marking_xrefs, Mixing_xrefs, Regions_xrefs, Selections_xrefs, Cursors_xrefs, Tracking_cursors_xrefs,
  Deletions_xrefs, Envelopes_xrefs, Filters_xrefs, Searching_xrefs, Insertions_xrefs, Maxamps_xrefs,
  Playing_xrefs, Reversing_xrefs, Saving_xrefs, Smoothing_xrefs, Resampling_xrefs, FFTs_xrefs, Reverb_xrefs,
  Resampling_xrefs, Searching_xrefs, Undo_and_Redo_xrefs, Undo_and_Redo_xrefs, 
  sync_xrefs, control_xrefs, record_xrefs, header_and_data_xrefs, key_xrefs, Tracks_xrefs, Copying_xrefs,
  Noise_Reduction_xrefs, Window_size_and_position_xrefs, Colors_xrefs, control_xrefs, Random_Numbers_xrefs,
  Wavogram_xrefs
};

static char **xref_url_tables[NUM_XREFS] = {
  Marking_urls, Mixing_urls, Regions_urls, Selections_urls, Cursors_urls, Tracking_cursors_urls,
  Deletions_urls, Envelopes_urls, Filters_urls, Searching_urls, Insertions_urls, Maxamps_urls,
  Playing_urls, Reversing_urls, Saving_urls, Smoothing_urls, Resampling_urls, FFTs_urls, Reverb_urls,
  Resampling_urls, Searching_urls, Undo_and_Redo_urls, Undo_and_Redo_urls, 
  NULL, NULL, NULL, NULL, NULL, Tracks_urls, Copying_urls, 
  Noise_Reduction_urls, Window_size_and_position_urls, Colors_urls, NULL, Random_Numbers_urls,
  Wavogram_urls
};

typedef void (*help_func)(void);
/* if an entry is null here, the main help window will display "(no help found)" */
static help_func help_funcs[NUM_XREFS] = {
  &marks_help, &mix_help, &region_help, &selection_help, &cursor_help, &tracking_cursor_help,
  &delete_help, &env_help, &filter_help, &find_help, &insert_help, &maxamp_help,
  &play_help, &reverse_help, &save_help, &smooth_help, &resample_help, &fft_help, &reverb_help,
  &resample_help, &find_help, &undo_help, &undo_help,
  &sync_help, &controls_help, recording_help, &sound_files_help, &key_binding_help, &track_help, &copy_help,
  &noise_reduction_help, &window_size_help, &colors_help, &controls_help, &random_numbers_help,
  &wavogram_help
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

static int help_name_to_url(const char *name)
{
  int i, l = 0, u = HELP_NAMES_SIZE - 1;
  while (true)
    {
      int comp;
      if (u < l) return(-1);
      i = (l + u) / 2;
      comp = STRCMP(help_names[i], name);
      if (comp == 0) return(i);
      if (comp < 0)
	l = i + 1;
      else u = i - 1;
    }
  return(-1);
}

char *snd_url(const char *name)
{
  /* (snd-url "save-sound-as") -> "extsnd.html#savesoundas" */
  int i;
  if (help_names) /* no ext lang, but wily user typed play-selection to the help dialog... */
    {
      i = help_name_to_url(name);
      if (i >= 0) return(help_urls[i]);
    }
  return(NULL);
}

static char *call_grep(const char *defstr, const char *name, const char *endstr, char *path, char *tempfile)
{
  int err;
  char *command;
#if (!MUS_SUN)
  /* Gnu fgrep: -s switch to fgrep = "silent", I guess (--no-messages) [OSX uses Gnu fgrep] */
  /* configure script looks for grep -F or fgrep, setting FGREP_PROG (fgrep is supposedly obsolete) */
  command = mus_format(FGREP_PROG " -s \"%s%s%s\" %s/*." XEN_FILE_EXTENSION " --line-number > %s", defstr, name, endstr, path, tempfile);
#else
  /* Sun fgrep: here -s means -q and --line-number prints an error message */
  command = mus_format(FGREP_PROG " \"%s%s%s\" %s/*." XEN_FILE_EXTENSION " > %s", defstr, name, endstr, path, tempfile);
#endif
  err = system(command);
  FREE(command);
  if (err != -1)                      /* no error, so I guess tempfile exists, but might be empty */
    return(file_to_string(tempfile)); /* NULL if nothing found */
  return(NULL);
}

static char *snd_finder(const char *name, bool got_help)
{
  /* desperation -- search *.scm/rb then even *.html? for 'name' */
  char *url = NULL, *fgrep = NULL, *tempfile = NULL, *command = NULL;
  bool is_defined = false;
  int a_def = 0, dir_len = 0, i;
  XEN dirs = XEN_EMPTY_LIST;

#if HAVE_GUILE || (!HAVE_EXTENSION_LANGUAGE)
  #define NUM_DEFINES 6
  #define TRAILER " "
  const char *defines[NUM_DEFINES] = {"(define (", "(define* (", "(define ", "(defmacro ", "(defmacro* ", "(definstrument ("};
#endif
#if HAVE_GAUCHE
  #define NUM_DEFINES 6
  #define TRAILER " "
  const char *defines[NUM_DEFINES] = {"(define (", "(define* (", "(define ", "(defmacro ", "(defmacro* ", "(definstrument ("};
#endif
#if HAVE_RUBY
  #define NUM_DEFINES 2
  #define TRAILER ""
  const char *defines[NUM_DEFINES] = {"def ", "class "};
#endif
#if HAVE_FORTH
  #define NUM_DEFINES 3
  #define TRAILER ""
  const char *defines[NUM_DEFINES] = {": ", "instrument: ", "event: "};
#endif

  if (snd_strlen(FGREP_PROG) == 0) return(NULL); /* configure didn't find a plausible fgrep */

  is_defined = XEN_DEFINED_P(name);
  url = snd_url(name);
  tempfile = snd_tempnam(); /* this will have a .snd extension */
  dirs = XEN_LOAD_PATH;
  dir_len = XEN_LIST_LENGTH(dirs);

  for (i = 0; (!fgrep) && (i < dir_len); i++)
    {
      char *path;
      path = XEN_TO_C_STRING(XEN_LIST_REF(dirs, i));
      if (!path) continue;

      for (a_def = 0; (!fgrep) && (a_def < NUM_DEFINES); a_def++)
	fgrep = call_grep(defines[a_def], name, TRAILER, path, tempfile);
#if HAVE_SCHEME
      if (!fgrep)
	fgrep = call_grep("(define (", name, ")", path, tempfile);
      if (!fgrep)
	fgrep = call_grep("(define ", name, "\n", path, tempfile);
#endif
    }
  snd_remove(tempfile, IGNORE_CACHE);
  FREE(tempfile);

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
    if (min_diff < snd_int_log2(topic_len)) /* was topic_len / 2, but this gives too much leeway for substitutions */
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
#if HAVE_SCHEME || HAVE_FORTH
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
			if ((move_paren) && (text[i] == ')'))
			  {
			    /* no args: use () */
			    new_text[j++] = '(';
			    new_text[j++] = ')';
			    move_paren = false;
			    in_paren = 0;
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
			      }}}}}
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
    return(mus_getcwd());
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
#ifdef MUS_DEFAULT_DOC_DIR
  if (mus_file_probe(MUS_DEFAULT_DOC_DIR "/snd.html"))
    return(copy_string(MUS_DEFAULT_DOC_DIR "/snd.html"));
#endif
  for (i = 0; i < DOC_DIRECTORIES; i++)
    if (mus_file_probe(doc_files[i])) return(copy_string(doc_directories[i]));
  return(NULL);
}

void url_to_html_viewer(const char *url)
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
	      int err;
	      sprintf(path, "%s file:%s/%s", program, dir_path, url);
	      err = system(path);
	      if (err == -1)
		fprintf(stderr, "can't start %s?", program);
	    }
	  FREE(path);
	}
      FREE(dir_path);
    }
}


void name_to_html_viewer(const char *red_text)
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
  #if HAVE_SCHEME
    #define snd_help_example "(snd-help 'make-vct)"
    #define snd_help_arg_type "can be a string, symbol, or in some cases, the object itself"
  #endif
  #if HAVE_RUBY
    #define snd_help_example "snd_help(\"make_vct\")"
    #define snd_help_arg_type "can be a string or a symbol"
  #endif
  #if HAVE_FORTH
    #define snd_help_example "\"make-vct\" snd-help"
    #define snd_help_arg_type "is a string"
  #endif

  #define H_snd_help "(" S_snd_help " :optional (arg 'snd-help) (formatted " PROC_TRUE ")): return the documentation \
associated with its argument. " snd_help_example " for example, prints out a brief description of make-vct. \
The argument " snd_help_arg_type ". \
In the help descriptions, optional arguments are in parens with the default value (if any) as the 2nd entry. \
A ':' as the start of the argument name marks a CLM-style optional keyword argument.  If you load index." XEN_FILE_EXTENSION " \
the functions html and ? can be used in place of help to go to the HTML description, \
and the location of the associated C code will be displayed, if it can be found. \
If " S_help_hook " is not empty, it is invoked with the subject and the snd-help result \
and its value is returned."

  char *str = NULL, *subject = NULL;
  int min_diff = 1000;

#if HAVE_GUILE
  {
    XEN help_text = XEN_FALSE; 
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
	topic_min = snd_int_log2(snd_strlen(subject));
	
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
#if HAVE_GUILE
		/* if procedure-with-setter, documentation string might be on the getter? */
		if ((XEN_FALSE_P(help_text)) &&
		    (SCM_PROCEDURE_WITH_SETTER_P(value)))
		  help_text = XEN_PROCEDURE_SOURCE_HELP(SCM_PROCEDURE(value));
#endif
		    
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
  }
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

#if HAVE_FORTH
  if (XEN_STRING_P(text))	/* "play" snd-help */
    subject = XEN_TO_C_STRING(text);
  else if (XEN_NOT_BOUND_P(text)) /* snd-help play */
    {
      subject = fth_parse_word();
      text = C_TO_XEN_STRING(subject);
    }
  if (!subject)
    {
      subject = S_snd_help;
      text = C_TO_XEN_STRING(S_snd_help);
    }
  str = XEN_AS_STRING(XEN_OBJECT_HELP(text));
#endif

#if HAVE_GAUCHE
  {
    XEN sym = XEN_FALSE;
    if (XEN_STRING_P(text))
      {
	subject = XEN_TO_C_STRING(text);
	sym = C_STRING_TO_XEN_SYMBOL(subject);
      }
    else
      {
	if (XEN_SYMBOL_P(text))
	  {
	    subject = XEN_SYMBOL_TO_C_STRING(text);
	    sym = text;
	  }
	else 
	  {
	    if (XEN_PROCEDURE_P(text))
	      {
		XEN name;
		name = SCM_PROCEDURE_INFO(text);
		if (XEN_STRING_P(name))
		  {
		    subject = XEN_TO_C_STRING(name);
		    sym = C_STRING_TO_XEN_SYMBOL(subject);
		  }
	      }
	    else
	      {
		subject = S_snd_help;
		str = H_snd_help;
	      }
	  }
      }
    if (!str)
      {
	XEN hlp;
	hlp = XEN_OBJECT_HELP(sym);
	if (XEN_STRING_P(hlp))
	  str = XEN_TO_C_STRING(hlp);
	else
	  {
	    int i, min_loc = 0, this_diff, topic_min = 0;
	    topic_min = snd_int_log2(snd_strlen(subject));
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
		hlp = XEN_OBJECT_HELP(sym);
		if (XEN_STRING_P(hlp))
		  str = XEN_TO_C_STRING(hlp);
	      }
	  }
      }
  }
#endif

  {
    bool need_free = false;
    XEN help_text = XEN_FALSE; 

    if ((str == NULL) || 
	(snd_strlen(str) == 0) ||
	(strcmp(str, PROC_FALSE) == 0)) /* Ruby returns "false" here */
      {
	if (!subject) return(XEN_FALSE);
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
	char *new_str = NULL;
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
      lst = XEN_CONS(XEN_CONS(C_TO_XEN_STRING(help_names[i]), 
			      C_TO_XEN_STRING(help_urls[i])), 
		     lst);
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
#if HAVE_FORTH
  #define H_output_comment_hook S_output_comment_hook " (str): called in Save-As dialog, passed current sound's comment, if any. \
If more than one hook function, each function gets the previous function's output as its input.\n\
" S_output_comment_hook " lambda: <{ str }>\n\
  \"%s: written %s\" '( str date ) format\n\
; add-hook!"
#endif

  output_comment_hook = XEN_DEFINE_HOOK(S_output_comment_hook, 1, H_output_comment_hook); /* arg = current mus_sound_comment(hdr) if any */

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_html_dir, g_html_dir_w, H_html_dir, S_setB S_html_dir, g_set_html_dir_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_html_program, g_html_program_w, H_html_program, S_setB S_html_program, g_set_html_program_w,  0, 0, 1, 0);
}
