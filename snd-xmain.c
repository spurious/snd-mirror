#include "snd.h"

/* TODO: basic-[small|large]-[bodl]-font (somehow set default font in motif)
 */

#include <X11/cursorfont.h>

#define FALLBACK_FONT "fixed"

/* our default basic colors (resource file can override these): */
#define HIGHLIGHT_COLOR      "ivory1"
#define BASIC_COLOR          "ivory2"
#define POSITION_COLOR       "ivory3"
#define ZOOM_COLOR           "ivory4"
#define CURSOR_COLOR         "red"
#define SELECTION_COLOR      "lightsteelblue1"
#define SELECTED_MIX_COLOR   "lightgreen"
#define ENVED_WAVEFORM_COLOR "blue"
#define MIX_COLOR            "darkgray"
#define GRAPH_COLOR          "white"
#define SELECTED_GRAPH_COLOR "white"
#define DATA_COLOR           "black"
#define SELECTED_DATA_COLOR  "black"
#define MARK_COLOR           "red"
#define LISTENER_COLOR       "AliceBlue"
#define LISTENER_TEXT_COLOR  "black"
#define LIGHT_BLUE_COLOR     "lightsteelblue1"
#define LIGHTER_BLUE_COLOR   "AliceBlue"
#define WHITE_COLOR          "white"
#define BLACK_COLOR          "black"
#define GREEN_COLOR          "green2"
#define RED_COLOR            "red"
#define YELLOW_COLOR         "yellow"
#define TEXT_FOCUS_COLOR     "white"
#define FILTER_WAVEFORM_COLOR "blue"
#define PUSHED_BUTTON_COLOR  "lightsteelblue1"
#define SASH_COLOR           "lightgreen"

#define CHANNEL_SASH_INDENT -10
#define CHANNEL_SASH_SIZE 0
/* 0 means: use Motif default size */
#define ENVED_POINT_SIZE 10
#define NOTEBOOK_BINDING_WIDTH 16

#define NO_ICON 0
#define PLAIN_ICON 1
#define XPM_ICON 2

#define TINY_FONT "6x12"

/* we assume later that we can always find these fonts (if resource file gives bogus entry, we fall back on these) */

#define DEFAULT_PEAKS_FONT "-*-times-medium-r-*-*-14-*-*-*-*-*-*-*"
#define DEFAULT_BOLD_PEAKS_FONT "-*-times-bold-r-*-*-14-*-*-*-*-*-*-*"
#define DEFAULT_AXIS_NUMBERS_FONT "-*-courier-medium-r-*-*-14-*-*-*-*-*-*-*"
#define DEFAULT_AXIS_LABEL_FONT "-*-times-medium-r-*-*-14-*-*-*-*-*-*-*"

#ifdef SGI
  #define ICON_TYPE PLAIN_ICON
#else
  #define ICON_TYPE XPM_ICON
#endif

#define POSITION_SLIDER_WIDTH 13
#define ZOOM_SLIDER_WIDTH 10
#ifndef LINUX
  #define TOGGLE_SIZE 0
#else
  #define TOGGLE_SIZE 15
#endif
#define CHANNEL_MIN_HEIGHT 150  /* open size (set to 5 immediately thereafter) */
                                /* paned window default setup is not very smart, so we force these to be this size to begin with */
                                /* this number is only a first approximation -- we try not to expand below the screen */
                                /* if too small (i.e. 100), the scrollbars are sometimes messed up on the initial layout */

#define SASH_SIZE 14
#define SASH_INDENT -6
#define AUTO_RESIZE_DEFAULT 1

/* /usr/lib/X11/rgb.txt, /usr/lib/X11/fonts/Type1/fonts.dir, /usr/lib/X11/fonts/misc */
/* schemes are in /usr/lib/X11/schemes */

typedef struct {
  char *highlight_color;
  char *basic_color;
  char *position_color;
  char *zoom_color;
  char *cursor_color;
  char *selection_color;
  char *mix_color;
  char *selected_mix_color;
  char *text_focus_color;
  char *graph_color;
  char *selected_graph_color;
  char *data_color;
  char *selected_data_color;
  char *listener_color;
  char *listener_text_color;
  char *mark_color;
  char *pushed_button_color;
  char *enved_waveform_color;
  char *filter_waveform_color;
  char *sash_color;
  char *white_color;
  char *black_color;
  char *red_color;
  char *yellow_color;
  char *green_color;
  char *light_blue_color;
  char *lighter_blue_color;
  char *use_schemes;
  char *peaks_font;
  char *listener_font;
  char *bold_peaks_font;
  char *axis_label_font;
  char *axis_numbers_font;
  char *init_file_name;
  char *eps_file_name;
  int spectrogram_color;
  int overwrite_check;
  int auto_resize;
  int horizontal_panes;
  int zoom_slider_width;
  int position_slider_width;
  int toggle_size;
  int enved_point_size;
  int channel_sash_indent;
  int channel_sash_size;
  int sash_size;
  int sash_indent;
  char *html_dir;
} sndres;

static XtResource resources[] = {
  {"highlightcolor", "Highlightcolor", XmRString, sizeof(char *), XtOffset(sndres *, highlight_color), XmRString, (XtPointer)HIGHLIGHT_COLOR},
  {"basiccolor", "Basiccolor", XmRString, sizeof(char *), XtOffset(sndres *, basic_color), XmRString, (XtPointer)BASIC_COLOR},
  {"positioncolor", "Positioncolor", XmRString, sizeof(char *), XtOffset(sndres *, position_color), XmRString, (XtPointer)POSITION_COLOR},
  {"zoomcolor", "Zoomcolor", XmRString, sizeof(char *), XtOffset(sndres *, zoom_color), XmRString, (XtPointer)ZOOM_COLOR},
  {"listenercolor", "Listenercolor", XmRString, sizeof(char *), XtOffset(sndres *, listener_color), XmRString, (XtPointer)LISTENER_COLOR},
  {"listenertextcolor", "Listenertextcolor", XmRString, sizeof(char *), XtOffset(sndres *, listener_text_color), XmRString, (XtPointer)LISTENER_TEXT_COLOR},
  {"cursorcolor", "Cursorcolor", XmRString, sizeof(char *), XtOffset(sndres *, cursor_color), XmRString, (XtPointer)CURSOR_COLOR},
  {"selectioncolor", "Selectioncolor", XmRString, sizeof(char *), XtOffset(sndres *, selection_color), XmRString, (XtPointer)SELECTION_COLOR},
  {"mixcolor", "Mixcolor", XmRString, sizeof(char *), XtOffset(sndres *, mix_color), XmRString, (XtPointer)MIX_COLOR},
  {"selectedmixcolor", "Selectedmixcolor", XmRString, sizeof(char *), XtOffset(sndres *, selected_mix_color), XmRString, (XtPointer)SELECTED_MIX_COLOR},
  {"textfocuscolor", "Textfocuscolor", XmRString, sizeof(char *), XtOffset(sndres *, text_focus_color), XmRString, (XtPointer)TEXT_FOCUS_COLOR},
  {"redcolor", "Redcolor", XmRString, sizeof(char *), XtOffset(sndres *, red_color), XmRString, (XtPointer)RED_COLOR},
  {"greencolor", "Greencolor", XmRString, sizeof(char *), XtOffset(sndres *, green_color), XmRString, (XtPointer)GREEN_COLOR},
  {"whitecolor", "Whitecolor", XmRString, sizeof(char *), XtOffset(sndres *, white_color), XmRString, (XtPointer)WHITE_COLOR},
  {"blackcolor", "Blackcolor", XmRString, sizeof(char *), XtOffset(sndres *, black_color), XmRString, (XtPointer)BLACK_COLOR},
  {"lightbluecolor", "Lightbluecolor", XmRString, sizeof(char *), XtOffset(sndres *, light_blue_color), XmRString, (XtPointer)LIGHT_BLUE_COLOR},
  {"lighterbluecolor", "Ligterbluecolor", XmRString, sizeof(char *), XtOffset(sndres *, lighter_blue_color), XmRString, (XtPointer)LIGHTER_BLUE_COLOR},
  {"yellowcolor", "Yellowcolor", XmRString, sizeof(char *), XtOffset(sndres *, yellow_color), XmRString, (XtPointer)YELLOW_COLOR},
  {"envedwaveformcolor", "Envedwaveformcolor", XmRString, sizeof(char *), XtOffset(sndres *, enved_waveform_color), XmRString, (XtPointer)ENVED_WAVEFORM_COLOR},
  {"filterwaveformcolor", "Filterwaveformcolor", XmRString, sizeof(char *), XtOffset(sndres *, filter_waveform_color), XmRString, (XtPointer)FILTER_WAVEFORM_COLOR},
  {"graphcolor", "Graphcolor", XmRString, sizeof(char *), XtOffset(sndres *, graph_color), XmRString, (XtPointer)GRAPH_COLOR},
  {"selectedgraphcolor", "Selectedgraphcolor", XmRString, sizeof(char *), XtOffset(sndres *, selected_graph_color), XmRString, (XtPointer)SELECTED_GRAPH_COLOR},
  {"datacolor", "Datacolor", XmRString, sizeof(char *), XtOffset(sndres *, data_color), XmRString, (XtPointer)DATA_COLOR},
  {"selecteddatacolor", "Selecteddatacolor", XmRString, sizeof(char *), XtOffset(sndres *, selected_data_color), XmRString, (XtPointer)SELECTED_DATA_COLOR},
  {"markcolor", "Markcolor", XmRString, sizeof(char *), XtOffset(sndres *, mark_color), XmRString, (XtPointer)MARK_COLOR},
  {"sashcolor", "Sashcolor", XmRString, sizeof(char *), XtOffset(sndres *, sash_color), XmRString, (XtPointer)SASH_COLOR},
  {"pushedbuttoncolor", "Pushedbuttoncolor", XmRString, sizeof(char *), XtOffset(sndres *, pushed_button_color), XmRString, (XtPointer)PUSHED_BUTTON_COLOR},
  {"useSchemes", "UseSchemes", XmRString, sizeof(char *), XtOffset(sndres *, use_schemes), XmRString, (XtPointer)"none"},
  {"peaksFont", "PeaksFont", XmRString, sizeof(char *), XtOffset(sndres *, peaks_font), XmRString, (XtPointer)DEFAULT_PEAKS_FONT},
  {"listenerFont", "ListenerFont", XmRString, sizeof(char *), XtOffset(sndres *, listener_font), XmRString, (XtPointer)NULL},
  {"boldpeaksFont", "BoldPeaksFont", XmRString, sizeof(char *), XtOffset(sndres *, bold_peaks_font), XmRString, (XtPointer)DEFAULT_BOLD_PEAKS_FONT},
  {"axisLabelFont", "AxisLabelFont", XmRString, sizeof(char *), XtOffset(sndres *, axis_label_font), XmRString, (XtPointer)DEFAULT_AXIS_LABEL_FONT},
  {"axisNumbersFont", "AxisNumbersFont", XmRString, sizeof(char *), XtOffset(sndres *, axis_numbers_font), XmRString, (XtPointer)DEFAULT_AXIS_NUMBERS_FONT},
  {"initFile", "InitFile", XmRString, sizeof(char *), XtOffset(sndres *, init_file_name), XmRString, (XtPointer)INIT_FILE_NAME},
  {"epsFile", "EpsFile", XmRString, sizeof(char *), XtOffset(sndres *, eps_file_name), XmRString, (XtPointer)DEFAULT_EPS_FILE},
  {"spectrogramColor", "SpectrogramColor", XmRInt, sizeof(int), XtOffset(sndres *, spectrogram_color), XmRImmediate, (XtPointer)DEFAULT_COLOR_MAP},
  {"overwriteCheck", "OverwriteCheck", XmRInt, sizeof(int), XtOffset(sndres *, overwrite_check), XmRImmediate, (XtPointer)0},
  {"autoResize", "AutoResize", XmRInt, sizeof(int), XtOffset(sndres *, auto_resize), XmRImmediate, (XtPointer)AUTO_RESIZE_DEFAULT},
  {"horizontalPanes", "HorizontalPanes", XmRInt, sizeof(int), XtOffset(sndres *, horizontal_panes), XmRImmediate, (XtPointer)SOUNDS_VERTICAL},
  {"zoomSliderWidth", "ZoomSliderWidth", XmRInt, sizeof(int), XtOffset(sndres *, zoom_slider_width), XmRImmediate, (XtPointer)ZOOM_SLIDER_WIDTH},
  {"positionSliderWidth", "PositionSliderWidth", XmRInt, sizeof(int), XtOffset(sndres *, position_slider_width), XmRImmediate, (XtPointer)POSITION_SLIDER_WIDTH},
  {"toggleSize", "ToggleSize", XmRInt, sizeof(int), XtOffset(sndres *, toggle_size), XmRImmediate, (XtPointer)TOGGLE_SIZE},
  {"envedPointSize", "EnvedPointSize", XmRInt, sizeof(int), XtOffset(sndres *, enved_point_size), XmRImmediate, (XtPointer)ENVED_POINT_SIZE},
  {"channelSashIndent", "ChannelSashIndent", XmRInt, sizeof(int), XtOffset(sndres *, channel_sash_indent), XmRImmediate, (XtPointer)CHANNEL_SASH_INDENT},
  {"channelSashSize", "ChannelSashSize", XmRInt, sizeof(int), XtOffset(sndres *, channel_sash_size), XmRImmediate, (XtPointer)CHANNEL_SASH_SIZE},
  {"sashSize", "SashSize", XmRInt, sizeof(int), XtOffset(sndres *, sash_size), XmRImmediate, (XtPointer)SASH_SIZE},
  {"sashIndent", "SashIndent", XmRInt, sizeof(int), XtOffset(sndres *, sash_indent), XmRImmediate, (XtPointer)SASH_INDENT},
  {"htmldir", "HtmlDir", XmRString, sizeof(char *), XtOffset(sndres *, html_dir), XmRString, (XtPointer)DEFAULT_HTML_DIR},
};


#ifndef SND_AS_WIDGET
static void window_close(Widget w, XtPointer context, XtPointer info)
{
  snd_exit_cleanly(true);
}
#endif

static void auto_update_check(XtPointer context, XtIntervalId *id)
{
  if (auto_update_interval(ss) > 0.0)
    {
      if ((!(play_in_progress())) && 
	  (!(record_in_progress())))
	for_each_sound(sound_not_current, NULL);
      XtAppAddTimeOut(MAIN_APP(ss),
		      (unsigned long)(auto_update_interval(ss) * 1000),
		      (XtTimerCallbackProc)auto_update_check,
		      context);
    }
}

#ifndef SND_AS_WIDGET
static void dismiss_all_dialogs(void)
{
  state_context *sx;
  int i;
  sx = ss->sgx;
  if (record_dialog_is_active()) close_recorder_audio();
  if (sx->dialogs)
    for (i = 0; i < NUM_DIALOGS; i++)
      if (sx->dialogs[i])
	if (XtIsManaged(sx->dialogs[i])) 
	  XtUnmanageChild(sx->dialogs[i]);
}

static int *live_dialogs = NULL;
static void minify_maxify_window(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  XMapEvent *ev = (XMapEvent *)event;
  state_context *sx;
  int i;
  sx = ss->sgx;
  /* ev->type can be several things, but the ones we care about here are
   * MapNotify and UnmapNotify.  Snd dialogs are "windows" in X-jargon, so
   * when the main window is minimized (iconified), other active dialogs
   * aren't also closed unless we mess with them explicitly here.  We keep
   * a list of created dialogs, and depend on XtIsManaged to tell us which
   * ones need to be unmanaged. But, if the user has several "desks", a change
   * of desk can also generate an unmap event, and if we dismiss the dialogs,
   * they don't come back upon remap; if we save a list of live dialogs, they
   * don't return to their previous location upon being re-managed.  We
   * need to see just iconfication events here, but there's no way I can
   * see to distinguish an iconify event from a desk event (WM_STATE atom state
   * of property changed event is identical etc), so I'll do what I can...
   * This problem may be a side-effect of using non-transient dialogs:
   * perhaps XSetTransientFor would handle this more cleanly?
   *
   * Also, ideally we'd equalize/relative-panes upon maxify, but ICCC thinks maxify
   *   is the same as map (i.e. from iconified state), and the only difference
   *   I can see in the mwm code is the window size.
   */
  if (ev->type == UnmapNotify) 
    {
      if ((sx) && (sx->dialogs))
	{
	  if (live_dialogs == NULL)
	    live_dialogs = (int *)CALLOC(NUM_DIALOGS, sizeof(int));
	  for (i = 0; i < NUM_DIALOGS; i++)
	    live_dialogs[i] = ((sx->dialogs[i]) && (XtIsManaged(sx->dialogs[i])));
	}
      dismiss_all_dialogs();
    }
  else
    {
      if ((ev->type == MapNotify) && (sx) && (sx->dialogs) && (live_dialogs))
	  for (i = 0; i < NUM_DIALOGS; i++)
	    if ((live_dialogs[i]) && (sx->dialogs[i]))
	      XtManageChild(sx->dialogs[i]);
    }
}
#endif

static Atom snd_v, snd_c;

#if HAVE_EXTENSION_LANGUAGE
static XEN window_property_changed_hook;

#ifndef SND_AS_WIDGET
static void who_called(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  /* watch for communication from some other program via the SND_COMMAND property */
  XPropertyEvent *ev = (XPropertyEvent *)event;
  Atom type;
  int format;
  unsigned long nitems, bytesafter;
  unsigned char *version[1];
  if (ev->atom == snd_c)
    {
      if (((XGetWindowProperty(XtDisplay(w), XtWindow(w), snd_c, 0L, (long)BUFSIZ, 0,
			       XA_STRING, &type, &format, &nitems, &bytesafter, 
			       (unsigned char **)version)) == Success) && 
	  (type != None))
	if (version[0])
	  {
	    if ((!(XEN_HOOKED(window_property_changed_hook))) ||
		(!(XEN_TRUE_P(run_or_hook(window_property_changed_hook,
					  XEN_LIST_1(C_TO_XEN_STRING((char *)(version[0]))),
					  S_window_property_changed_hook)))))
	    snd_eval_property_str((char *)(version[0]));
	    free(version[0]);
	  }
    }
}
#endif
#endif

#if HAVE_SETJMP_H
#include <setjmp.h>

#if TRAP_SEGFAULT
/* stolen from scwm.c */
static sigjmp_buf envHandleEventsLoop;

static RETSIGTYPE segv(int ignored)
{
  siglongjmp(envHandleEventsLoop, 1);
}
#endif

static jmp_buf top_level_jump;
RETSIGTYPE top_level_catch(int ignore);
RETSIGTYPE top_level_catch(int ignore)
{
  longjmp(top_level_jump, 1);
}
#endif


static char **auto_open_file_names = NULL;
static int auto_open_files = 0;
static bool noglob = false, noinit = false, batch = false, nostdin = false;

#if HAVE_EXTENSION_LANGUAGE
static XtInputId stdin_id = 0;

static void get_stdin_string (XtPointer context, int *fd, XtInputId *id)
{
  int bytes, size;
  char *buf;
  buf = (char *)CALLOC(1024, sizeof(char));
  size = 1024;
  bytes = read(*fd, buf, 1024);
  if (bytes <= 0) 
    {
      /* redirected to /dev/null?? -- apparently in kde/kfm the process is started without stdin? */
      XtRemoveInput(stdin_id);
      stdin_id = 0;
    }
  else
    {
      while (bytes == 1024)
	{
	  size += 1024;
	  buf = (char *)REALLOC(buf, size);
	  bytes = read(*fd, (char *)(buf + size - 1024), 1024);
	}
      snd_eval_stdin_str(buf);
    }
  FREE(buf);
}
#endif

static int tm_slice = 0;

static Cessate startup_funcs(XtPointer context)
{
#ifndef SND_AS_WIDGET
  Atom wm_delete_window;
#endif
  snd_info *sp;
  static int auto_open_ctr = 0;
  Widget shell;
  Display *dpy;
  shell = ss->sgx->mainshell;
  dpy = MAIN_DISPLAY(ss);
  switch (tm_slice)
    {
    case 0:
      create_popup_menu();
#ifndef SND_AS_WIDGET
#ifndef __alpha__
      add_menu_drop();
#endif
#endif
#ifndef SND_AS_WIDGET
      /* trap outer-level Close for cleanup check */
      wm_delete_window = XmInternAtom(dpy, "WM_DELETE_WINDOW", false);
      XmAddWMProtocolCallback(shell, wm_delete_window, window_close, NULL);

      snd_v = XInternAtom(dpy, "SND_VERSION", false);
      snd_c = XInternAtom(dpy, "SND_COMMAND", false);
      XChangeProperty(dpy, XtWindow(shell), snd_v, XA_STRING, 8, PropModeReplace, 
		      (unsigned char *)(SND_VERSION), strlen(SND_VERSION) + 1);
#if HAVE_EXTENSION_LANGUAGE
      XtAddEventHandler(shell, PropertyChangeMask, false, who_called, NULL);
#endif
      XtAddEventHandler(shell, StructureNotifyMask, false, minify_maxify_window, NULL);
#endif
      (ss->sgx)->graph_cursor = XCreateFontCursor(XtDisplay(MAIN_SHELL(ss)), in_graph_cursor(ss));
      (ss->sgx)->wait_cursor = XCreateFontCursor(XtDisplay(MAIN_SHELL(ss)), XC_watch);
      break;
    case 1:
#if HAVE_EXTENSION_LANGUAGE
      snd_load_init_file(noglob, noinit);
#endif
#if HAVE_SIGNAL && HAVE_EXTENSION_LANGUAGE
      if (!nostdin)
	{
	  signal(SIGTTIN, SIG_IGN);
	  signal(SIGTTOU, SIG_IGN);
	  /* these signals are sent by a shell if we start Snd as a background process,
	   * but try to read stdin (needed to support the emacs subjob connection).  If
	   * we don't do this, the background job is suspended when the shell sends SIGTTIN.
	   */
	  stdin_id = XtAppAddInput(MAIN_APP(ss), 
				   fileno(stdin), 
				   (XtPointer)XtInputReadMask, 
				   get_stdin_string, 
				   NULL);
	}
#endif
      break;
    case 2: 
      if (auto_open_files > 0)
	{
	  auto_open_ctr = handle_next_startup_arg(auto_open_ctr, auto_open_file_names, false, auto_open_files);
	  if (auto_open_ctr < auto_open_files) 
	    return(BACKGROUND_CONTINUE); /* i.e. come back to this branch */
	}
      break;
    case 3:
      if (ss->init_window_width > 0) set_widget_width(MAIN_SHELL(ss), ss->init_window_width);
      if (ss->init_window_height > 0) set_widget_height(MAIN_SHELL(ss), ss->init_window_height);
      if (ss->init_window_x != DEFAULT_INIT_WINDOW_X) set_widget_x(MAIN_SHELL(ss), ss->init_window_x);
      if (ss->init_window_y != DEFAULT_INIT_WINDOW_Y) set_widget_y(MAIN_SHELL(ss), ss->init_window_y);
      XtAppAddTimeOut(MAIN_APP(ss), 
		      (unsigned long)(auto_update_interval(ss) * 1000), 
		      auto_update_check, 
		      NULL);
#if TRAP_SEGFAULT
      if (trap_segfault(ss)) signal(SIGSEGV, segv);
#endif
      if ((ss->sounds) &&
	  (ss->selected_sound == NO_SELECTION))
	{
	  sp = ss->sounds[0];
	  if ((sp) && 
	      (sp->inuse == SOUND_NORMAL) &&
	      (sp->selected_channel == NO_SELECTION)) /* don't clobber possible select-channel in loaded startup files */
	    select_channel(sp, 0);
	}
      if ((ss->init_window_height == 0) && (sound_style(ss) == SOUNDS_HORIZONTAL))
	set_widget_height(MAIN_SHELL(ss), 200); /* otherwise it's just a title bar! */
      if ((ss->active_sounds > 1) &&
	  ((sound_style(ss) == SOUNDS_VERTICAL) || (sound_style(ss) == SOUNDS_HORIZONTAL)))
	equalize_all_panes();
      return(BACKGROUND_QUIT); 
      break;
    }
  tm_slice++;
  return(BACKGROUND_CONTINUE);
}

#ifndef SND_AS_WIDGET
#if (ICON_TYPE == PLAIN_ICON) || ((ICON_TYPE == XPM_ICON) && (!HAVE_XPM))
static void SetupIcon(Widget shell)
{
  Display *dpy;
  Window root;
  Pixmap bitmap;
  dpy = XtDisplay(shell);
  root = DefaultRootWindow(dpy);
  bitmap = XCreateBitmapFromData(dpy, root, (const char *)snd_plain_icon_bits(), 48, 48);
  XtVaSetValues(shell, XmNiconPixmap, bitmap, NULL);
}
#endif

#if (ICON_TYPE == XPM_ICON) && (HAVE_XPM)
#include <X11/xpm.h>
static void SetupIcon(Widget shell)
{
  Display *dpy;
  Window root;
  int status, scr;
  Pixmap pix, mask;
  XpmAttributes attributes;
  dpy = XtDisplay(shell);
  root = DefaultRootWindow(dpy);
  scr = DefaultScreen(dpy);
  XtVaGetValues(shell, XmNdepth, &attributes.depth, XmNcolormap, &attributes.colormap, NULL);
  attributes.visual = DefaultVisual(dpy, scr);
  attributes.valuemask = XpmDepth | XpmColormap | XpmVisual;
  status = XpmCreatePixmapFromData(dpy, root, snd_icon_bits(), &pix, &mask, &attributes);
  if (mask) XFreePixmap(dpy, mask);
  XtVaSetValues(shell, XmNiconPixmap, pix, NULL);
}
#endif
#endif

#ifndef SND_AS_WIDGET
static void muffle_warning(char *name, char *type, char *klass, char *defaultp, char **params, unsigned int *num_params)
{
#if DEBUGGING
  int i;
  fprintf(stderr, "Xt warning: %s, %s: %s", type, name, defaultp);
  if (num_params) /* can be null! */
    for (i = 0; i < (*num_params); i++)
      fprintf(stderr, " %s", params[i]);
  fprintf(stderr, "\n");
#endif
}
#endif

static Pixel get_color(Widget shell,
		       char *rs_color, char *defined_color, char *fallback_color, char *second_fallback_color,
		       bool use_white)
{
  Colormap cmap;
  Display *dpy;
  int scr;
  XColor tmp_color, ignore;
  /* this apparently only gets called in Motif 1 on the SGI */
  dpy = XtDisplay(shell);
  scr = DefaultScreen(dpy);
  cmap = DefaultColormap(dpy, scr);
  if ((!XAllocNamedColor(dpy, cmap, rs_color, &tmp_color, &ignore)) &&
      (!XAllocNamedColor(dpy, cmap, defined_color, &tmp_color, &ignore)) &&
      ((!fallback_color) || 
       (!XAllocNamedColor(dpy, cmap, fallback_color, &tmp_color, &ignore))) &&
      ((!second_fallback_color) || 
       (!XAllocNamedColor(dpy, cmap, second_fallback_color, &tmp_color, &ignore))))
    {
      /* snd_error here causes a seg fault (it builds on mainpane which has not yet been created) */
      if (use_white)
	{
	  fprintf(stderr, _("can't get color %s -- will use white\n"), rs_color);
	  return(WhitePixel(dpy, scr));
	}
      else
	{
	  fprintf(stderr, _("can't get color %s -- will use black\n"), rs_color);
	  return(BlackPixel(dpy, scr));
	}
    }
  return(tmp_color.pixel);
}

#ifdef SND_AS_WIDGET
void snd_as_widget(int argc, char **argv, XtAppContext app, Widget parent, Arg *caller_args, int caller_argn)
{
#else
void snd_doit(int argc, char **argv)
{
  XtAppContext app;     
#endif
  Widget shell;
  Display *dpy;
  Drawable wn;
  Arg args[32];
  int i, n, err;
  sndres snd_rs;
  state_context *sx;
  Widget menu;
  XGCValues gv;
  char *app_title = NULL;
#ifdef SND_AS_WIDGET
  ss = snd_main(argc, argv);
#else
  XtSetLanguageProc(NULL, NULL, NULL);
#endif

  ss->ctrls_height = CLOSED_CTRLS_HEIGHT;
  ss->channel_min_height = CHANNEL_MIN_HEIGHT;
  ss->Graph_Cursor = XC_crosshair;

#ifndef SND_AS_WIDGET
#if SCO5 || UW2 || SUN || HPUX || ALPHA
  /*
  ** Use of the lower level Xt routines does not work.
  */
  XtSetArg(args[0], XtNwidth, 640);
  XtSetArg(args[1], XtNheight, 256);
  shell = XtAppInitialize(&app, "Snd", NULL, 0, &argc, argv, NULL, args, 2);
#else
  shell = XtVaOpenApplication(&app, "Snd", NULL, 0, &argc, argv, NULL, applicationShellWidgetClass,
			      XmNallowShellResize, AUTO_RESIZE_DEFAULT,
			      NULL);
#endif

  /* if user has *keyboardFocusPolicy: Pointer in .Xdefaults, it can screw up Snd's attempt to
   * keep the channel graphics window as the active widget in case of keyboard input.  So,
   * we try to force Snd's focus policy to be XmEXPLICIT
   */
  XtVaGetValues(shell, XmNkeyboardFocusPolicy, &err, NULL);
  if (err != XmEXPLICIT)
    XtVaSetValues(shell, XmNkeyboardFocusPolicy, XmEXPLICIT, NULL);

#else 
  /* SND_AS_WIDGET */
  shell = parent;
#endif

  auto_open_files = argc - 1;
  if (argc > 1) auto_open_file_names = (char **)(argv + 1);

  dpy = XtDisplay(shell);

  XtGetApplicationResources(shell, &snd_rs, resources, XtNumber(resources), NULL, 0);
  XtVaGetValues(shell, XmNtitle, &app_title, NULL);  /* perhaps caller included -title arg */
  if (app_title) 
    ss->startup_title = copy_string(app_title); 
  else ss->startup_title = copy_string("snd");

  set_sound_style((sound_style_t)snd_rs.horizontal_panes);
  for (i = 1; i < argc; i++)
    if ((strcmp(argv[i], "-h") == 0) || 
	(strcmp(argv[i], "-horizontal") == 0))
      set_sound_style(SOUNDS_HORIZONTAL);
    else
      if ((strcmp(argv[i], "-v") == 0) || 
	  (strcmp(argv[i], "-vertical") == 0))
	set_sound_style(SOUNDS_VERTICAL);
      else
	if (strcmp(argv[i], "-notebook") == 0)
	  {
	    set_sound_style(SOUNDS_IN_NOTEBOOK);
	    snd_rs.auto_resize = 0;
	  }
	else
	  if (strcmp(argv[i], "-separate") == 0)
	    set_sound_style(SOUNDS_IN_SEPARATE_WINDOWS);
	  else
	    if (strcmp(argv[i], "-noglob") == 0)
	      noglob = true;
	    else
	      if (strcmp(argv[i], "-noinit") == 0)
		noinit = true;
	      else
		if (strcmp(argv[i], "-nostdin") == 0)
		  nostdin = true;
		else
		  if ((strcmp(argv[i], "-b") == 0) || 
		      (strcmp(argv[i], "-batch") == 0))
		    batch = true;

  ss->batch_mode = batch;
  if (batch) XtSetMappedWhenManaged(shell, 0);
  set_html_dir(copy_string(snd_rs.html_dir));
#if (!SPLINT)
  ss->using_schemes = ((snd_rs.use_schemes) &&
		       ((strcmp(snd_rs.use_schemes, "all") == 0) || (strcmp(snd_rs.use_schemes, "All") == 0)));
  set_auto_resize(snd_rs.auto_resize);
#endif
  ss->zoom_slider_width = snd_rs.zoom_slider_width;
  ss->position_slider_width = snd_rs.position_slider_width;
  ss->channel_sash_indent = snd_rs.channel_sash_indent;
  ss->channel_sash_size = snd_rs.channel_sash_size;
  ss->sash_size = snd_rs.sash_size;
  ss->sash_indent = snd_rs.sash_indent;
  ss->toggle_size = snd_rs.toggle_size;
  ss->enved_point_size = snd_rs.enved_point_size;
  ss->sgx = (state_context *)CALLOC(1, sizeof(state_context));
  sx = ss->sgx;
#if (HAVE_GL) && (!SND_AS_WIDGET)
  {
    /* this taken from glxmotif.c from xjournal/sgi */
    XVisualInfo *vi = NULL;
    Colormap cmap;
    GLXContext cx;
    int snglBuf[] = {GLX_RGBA, GLX_DEPTH_SIZE, 16, None};
    int dblBuf[] = {GLX_RGBA, GLX_DEPTH_SIZE, 16, GLX_DOUBLEBUFFER, None};
    vi = glXChooseVisual(dpy, DefaultScreen(dpy), dblBuf);
    if (vi) 
      ss->gl_has_double_buffer = true;
    else
      {
	ss->gl_has_double_buffer = false;
	vi = glXChooseVisual(dpy, DefaultScreen(dpy), snglBuf);
      }
    if (vi == NULL) 
      fprintf(stderr, _("no RGB visual with desired depth\n")); /* not snd_error -- shell not ready yet */
    else
      {
	/* create an OpenGL rendering context */
	cx = glXCreateContext(dpy, vi, /* no display list sharing */ None, /* favor direct */ GL_TRUE);
	if (cx == NULL) 
	  fprintf(stderr, _("could not create rendering context\n"));
	else
	  {
	    /* create an X colormap since probably not using default visual */
	    cmap = XCreateColormap(dpy, RootWindow(dpy, vi->screen), vi->visual, AllocNone);
	    XtVaSetValues(shell, XtNvisual, vi->visual, XtNdepth, vi->depth, XtNcolormap, cmap, NULL);
	    ss->sgx->cx = cx;
	  }
      }
  }
#endif

#ifndef SND_AS_WIDGET
  XtAppSetWarningMsgHandler(app, muffle_warning);
#endif

  sx->mainapp = app;
  sx->mainshell = shell;
  sx->mdpy = dpy;
  /* the gray shades are an attempt to get around Netscape which hogs all the colors */
  sx->white =                 get_color(shell, snd_rs.white_color,           WHITE_COLOR,           NULL, NULL, true);
  sx->black =                 get_color(shell, snd_rs.black_color,           BLACK_COLOR,           NULL, NULL, false);
  sx->light_blue =            get_color(shell, snd_rs.light_blue_color,      LIGHT_BLUE_COLOR,      NULL, NULL, true);
  sx->lighter_blue =          get_color(shell, snd_rs.lighter_blue_color,    LIGHTER_BLUE_COLOR,    NULL, NULL, true);
  sx->red =                   get_color(shell, snd_rs.red_color,             RED_COLOR,             NULL, NULL, false);
  sx->green =                 get_color(shell, snd_rs.green_color,           GREEN_COLOR,           NULL, NULL, false);
  sx->yellow =                get_color(shell, snd_rs.yellow_color,          YELLOW_COLOR,          NULL, NULL, true);
  sx->highlight_color =       get_color(shell, snd_rs.highlight_color,       HIGHLIGHT_COLOR,       "gray90", NULL, true);
  sx->basic_color =           get_color(shell, snd_rs.basic_color,           BASIC_COLOR,           "gray80", "gray", true);
  sx->position_color =        get_color(shell, snd_rs.position_color,        POSITION_COLOR,        "gray60", "gray", false);
  sx->zoom_color =            get_color(shell, snd_rs.zoom_color,            ZOOM_COLOR,            "gray20", "gray", false);
  sx->cursor_color =          get_color(shell, snd_rs.cursor_color,          CURSOR_COLOR,          NULL, NULL, false);
  sx->selection_color =       get_color(shell, snd_rs.selection_color,       SELECTION_COLOR,       "gray80", NULL, false);
  sx->mix_color =             get_color(shell, snd_rs.mix_color,             MIX_COLOR,             NULL, NULL, false);
  sx->selected_mix_color =    get_color(shell, snd_rs.selected_mix_color,    SELECTED_MIX_COLOR,    NULL, NULL, false);
  sx->enved_waveform_color =  get_color(shell, snd_rs.enved_waveform_color,  ENVED_WAVEFORM_COLOR,  NULL, NULL, false);
  sx->filter_waveform_color = get_color(shell, snd_rs.filter_waveform_color, FILTER_WAVEFORM_COLOR, NULL, NULL, false);
  sx->listener_color =        get_color(shell, snd_rs.listener_color,        LISTENER_COLOR,        NULL, NULL, true);
  sx->listener_text_color =   get_color(shell, snd_rs.listener_text_color,   LISTENER_TEXT_COLOR,   NULL, NULL, true);
  sx->graph_color =           get_color(shell, snd_rs.graph_color,           GRAPH_COLOR,           NULL, NULL, true);
  sx->selected_graph_color =  get_color(shell, snd_rs.selected_graph_color,  SELECTED_GRAPH_COLOR,  NULL, NULL, true);
  sx->data_color =            get_color(shell, snd_rs.data_color,            DATA_COLOR,            NULL, NULL, false);
  sx->selected_data_color =   get_color(shell, snd_rs.selected_data_color,   SELECTED_DATA_COLOR,   NULL, NULL, false);
  sx->mark_color =            get_color(shell, snd_rs.mark_color,            MARK_COLOR,            NULL, NULL, false);
  sx->sash_color =            get_color(shell, snd_rs.sash_color,            SASH_COLOR,            NULL, NULL, false);
  sx->pushed_button_color =   get_color(shell, snd_rs.pushed_button_color,   PUSHED_BUTTON_COLOR,   NULL, NULL, false);
  sx->text_focus_color =      get_color(shell, snd_rs.text_focus_color,      TEXT_FOCUS_COLOR,      NULL, NULL, false);

  if ((!(set_peaks_font(snd_rs.peaks_font))) &&
      (!(set_peaks_font(DEFAULT_PEAKS_FONT))) &&
      (!(set_peaks_font(FALLBACK_FONT))))
    fprintf(stderr, _("can't find font %s"), snd_rs.peaks_font);

  if ((!(set_tiny_font(TINY_FONT))) &&
      (!(set_tiny_font(FALLBACK_FONT))))
    fprintf(stderr, _("can't find font %s"), TINY_FONT);

  if ((!(set_bold_peaks_font(snd_rs.bold_peaks_font))) &&
      (!(set_bold_peaks_font(DEFAULT_BOLD_PEAKS_FONT))) &&
      (!(set_bold_peaks_font(FALLBACK_FONT))))
    fprintf(stderr, _("can't find font %s"), snd_rs.bold_peaks_font);

  if ((!(set_axis_label_font(snd_rs.axis_label_font))) &&
      (!(set_axis_label_font(DEFAULT_AXIS_LABEL_FONT))) &&
      (!(set_axis_label_font(FALLBACK_FONT))))
    fprintf(stderr, _("can't find font %s"), snd_rs.axis_label_font);

  if ((!(set_axis_numbers_font(snd_rs.axis_numbers_font))) &&
      (!(set_axis_numbers_font(DEFAULT_AXIS_NUMBERS_FONT))) &&
      (!(set_axis_numbers_font(FALLBACK_FONT))))
    fprintf(stderr, _("can't find font %s"), snd_rs.axis_numbers_font);

  if ((snd_rs.listener_font) &&
      (!(set_listener_font(snd_rs.listener_font))))
    fprintf(stderr, _("can't find font %s"), snd_rs.listener_font);

  if (!(ss->using_schemes)) XtVaSetValues(shell, XmNbackground, sx->basic_color, NULL);
  ss->init_file = copy_string(getenv(SND_INIT_FILE_ENVIRONMENT_NAME));
  if (ss->init_file == NULL)
    ss->init_file = snd_rs.init_file_name; /* doesn't make any sense to pass this out to the user -- what can he do? */
#if DEBUGGING && HAVE_RUBY
  ss->init_file = copy_string("~/.sndrb"); /* save me some time... */
#endif
  if (eps_file(ss)) FREE(eps_file(ss));
  set_eps_file(copy_string(snd_rs.eps_file_name));
  set_color_map(snd_rs.spectrogram_color);
  set_ask_before_overwrite(snd_rs.overwrite_check);

#ifndef SND_AS_WIDGET
  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
#ifdef UW2
  XtSetArg(args[n], XmNforeground, sx->black); n++;
#endif
  n = attach_all_sides(args, n);
  XtSetArg(args[n], XmNallowResize, true); n++;
  sx->mainpane = XtCreateManagedWidget("mainpane", xmFormWidgetClass, shell, args, n);
#else
  sx->mainpane = XtCreateManagedWidget("mainpane", xmFormWidgetClass, parent, caller_args, caller_argn);
#endif
  menu = add_menu();

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, menu); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNallowResize, true); n++;
  switch (sound_style(ss))
    {
    case SOUNDS_IN_SEPARATE_WINDOWS:
      sx->soundpane = XtCreateManagedWidget("soundpane", xmFormWidgetClass, sx->mainpane, args, n);
      break;
    case SOUNDS_HORIZONTAL:
      XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
      sx->soundpanebox = XtCreateManagedWidget("soundpane", xmPanedWindowWidgetClass, sx->mainpane, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNsashHeight, ss->sash_size); n++;
      XtSetArg(args[n], XmNsashWidth, ss->sash_size); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNsashIndent, ss->sash_indent); n++;
      sx->soundpane = XtCreateManagedWidget("soundpane", xmPanedWindowWidgetClass, sx->soundpanebox, args, n);
      break;
    case SOUNDS_VERTICAL:
      XtSetArg(args[n], XmNsashHeight, ss->sash_size); n++;
      XtSetArg(args[n], XmNsashWidth, ss->sash_size); n++;
      XtSetArg(args[n], XmNsashIndent, ss->sash_indent); n++;
      sx->soundpane = XtCreateManagedWidget("soundpane", xmPanedWindowWidgetClass, sx->mainpane, args, n);
      break;
#if (XmVERSION > 1)
    case SOUNDS_IN_NOTEBOOK:
      XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
      sx->soundpanebox = XtCreateManagedWidget("soundpane", xmPanedWindowWidgetClass, sx->mainpane, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNframeBackground, sx->zoom_color); n++;
      XtSetArg(args[n], XmNbindingWidth, NOTEBOOK_BINDING_WIDTH); n++;
      sx->soundpane = XtCreateManagedWidget("nb", xmNotebookWidgetClass, sx->soundpanebox, args, n);
      map_over_children(sx->soundpane, set_main_color_of_widget, NULL); /* appears to be a no-op */
      break;
#endif
    }

#ifndef SND_AS_WIDGET
  #if ICON_TYPE
    SetupIcon(shell);
  #endif

  XtRealizeWidget(shell);

  if (auto_resize(ss) != AUTO_RESIZE_DEFAULT) 
    XtVaSetValues(shell, XmNallowShellResize, auto_resize(ss), NULL);
#endif

  wn = XtWindow(shell);
  gv.background = sx->graph_color;
  gv.foreground = sx->data_color;
  sx->basic_gc = XCreateGC(dpy, wn, GCForeground | GCBackground, &gv);

  gv.background = sx->graph_color;
  gv.foreground = sx->data_color;
  sx->combined_basic_gc = XCreateGC(dpy, wn, GCForeground | GCBackground, &gv);

  gv.background = sx->graph_color;
  gv.foreground = sx->mix_color;
  sx->mix_gc = XCreateGC(dpy, wn, GCForeground | GCBackground, &gv);

  gv.background = sx->graph_color;
  gv.foreground = sx->selected_mix_color;
  sx->selected_mix_gc = XCreateGC(dpy, wn, GCForeground | GCBackground, &gv);

  gv.function = GXxor;
  gv.background = sx->graph_color;
  gv.foreground = (Pixel)(XOR(sx->cursor_color, gv.background));
  sx->cursor_gc = XCreateGC(dpy, wn, GCForeground | GCFunction, &gv);

  gv.function = GXxor;
  gv.background = sx->graph_color;
  gv.foreground = (Pixel)(XOR(sx->selection_color, gv.background));
  sx->selection_gc = XCreateGC(dpy, wn, GCForeground | GCFunction, &gv);

  gv.function = GXxor;
  gv.background = sx->graph_color;
  gv.foreground = (Pixel)(XOR(sx->mark_color, gv.background));
  sx->mark_gc = XCreateGC(dpy, wn, GCForeground | GCFunction, &gv);

  gv.function = GXcopy;
  gv.background = sx->data_color;
  gv.foreground = sx->graph_color;
  sx->erase_gc = XCreateGC(dpy, wn, GCForeground | GCBackground | GCFunction, &gv);

  gv.background = sx->selected_graph_color;
  gv.foreground = sx->selected_data_color;
  sx->selected_basic_gc = XCreateGC(dpy, wn, GCForeground | GCBackground, &gv);

  gv.function = GXxor;
  gv.background = sx->selected_graph_color;
  gv.foreground = (Pixel)(XOR(sx->cursor_color, gv.background));
  sx->selected_cursor_gc = XCreateGC(dpy, wn, GCForeground | GCFunction, &gv);

  gv.function = GXxor;
  gv.background = sx->selected_graph_color;
  gv.foreground = (Pixel)(XOR(sx->selection_color, gv.background));
  sx->selected_selection_gc = XCreateGC(dpy, wn, GCForeground | GCFunction, &gv);

  gv.function = GXxor;
  gv.background = sx->selected_graph_color;
  gv.foreground = (Pixel)(XOR(sx->mark_color, gv.background));
  sx->selected_mark_gc = XCreateGC(dpy, wn, GCForeground | GCFunction, &gv);

  gv.function = GXcopy;
  gv.background = sx->selected_data_color;
  gv.foreground = sx->selected_graph_color;
  sx->selected_erase_gc = XCreateGC(dpy, wn, GCForeground | GCBackground | GCFunction, &gv);

  gv.function = GXcopy;
  gv.background = sx->basic_color;
  gv.foreground = sx->black;
  sx->fltenv_basic_gc = XCreateGC(dpy, wn, GCBackground | GCForeground | GCFunction, &gv);

  gv.function = GXcopy;
  gv.background = sx->basic_color;
  gv.foreground = sx->filter_waveform_color;
  sx->fltenv_data_gc = XCreateGC(dpy, wn, GCBackground | GCForeground | GCFunction, &gv);

  initialize_colormap();
  make_icons_transparent(snd_rs.basic_color);

  BACKGROUND_ADD(startup_funcs, NULL);
  /* this complication seems necessary because we might be loading Scheme code files
   *   from the startup args (via -l) and they might contain errors etc -- we want to
   *   be sure the complete interface is running (via the XtAppMainLoop below) when
   *   we drop into the error handler.  (Also we need the two longjmp sets, but they
   *   must follow this startup loop).
   */

#if HAVE_SETJMP_H
#if TRAP_SEGFAULT
  if (sigsetjmp(envHandleEventsLoop, 1))
    {
      snd_error(_("Caught seg fault (will try to continue):\n"));
    }
#endif
  if (setjmp(top_level_jump))
    {
      if (!(ss->jump_ok))
	snd_error(_("Caught top level error (will try to continue):\n"));
      else ss->jump_ok = false;
    }
#endif

#ifndef SND_AS_WIDGET
  XtAppMainLoop(app);
#endif
  
}

#if HAVE_GL
static XEN g_snd_glx_context(void)
{
  return(XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("GLXContext"), 
		    C_TO_XEN_ULONG((unsigned long)(ss->sgx->cx))));
} 

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_snd_glx_context_w, g_snd_glx_context)
#else
#define g_snd_glx_context_w g_snd_glx_context
#endif
#endif

void g_init_gxmain(void)
{
  #define H_window_property_changed_hook S_window_property_changed_hook "(command): called upon receipt of a change in SND_COMMAND (an X window property)"
  XEN_DEFINE_HOOK(window_property_changed_hook, S_window_property_changed_hook, 1, H_window_property_changed_hook);

#if HAVE_GL
  XEN_DEFINE_PROCEDURE("snd-glx-context", g_snd_glx_context_w, 0, 0, 0, "OpenGL GLXContext");
#endif
}
