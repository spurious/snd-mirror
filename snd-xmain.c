#include "snd.h"

#define FALLBACK_FONT "fixed"
#define DEFAULT_FONTLIST "9x15"

/* our default basic colors (resource file can override these): */
#define HIGHLIGHT_COLOR      "ivory1"
#define BASIC_COLOR          "ivory2"
#define POSITION_COLOR       "ivory3"
#define ZOOM_COLOR           "ivory4"
#define CURSOR_COLOR         "red"
#define SELECTION_COLOR      "lightsteelblue1"
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
#define FILTER_CONTROL_WAVEFORM_COLOR "blue"
#define PUSHED_BUTTON_COLOR  "lightsteelblue1"
#define SASH_COLOR           "lightgreen"

#define HELP_BUTTON_COLOR    "ivory1"
#define QUIT_BUTTON_COLOR    "ivory1"
#define RESET_BUTTON_COLOR   "ivory1"
#define DOIT_BUTTON_COLOR    "ivory1"
#define DOIT_AGAIN_BUTTON_COLOR "ivory1"

#define CHANNEL_SASH_INDENT -10
#define CHANNEL_SASH_SIZE 0
/* 0 means: use Motif default size */

#ifndef SND_AS_WIDGET
  #define PLAIN_ICON 1
  #define XPM_ICON 2
  #ifdef MUS_SGI
    #define ICON_TYPE PLAIN_ICON
  #else
    #define ICON_TYPE XPM_ICON
  #endif
#endif

#define POSITION_SLIDER_WIDTH 13
#define ZOOM_SLIDER_WIDTH 10
#ifndef MUS_LINUX
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

typedef struct {
  char *highlight_color;
  char *basic_color;
  char *position_color;
  char *zoom_color;
  char *cursor_color;
  char *selection_color;
  char *mix_color;
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
  char *filter_control_waveform_color;
  char *sash_color;
  char *white_color;
  char *black_color;
  char *red_color;
  char *yellow_color;
  char *green_color;
  char *light_blue_color;
  char *lighter_blue_color;
  char *peaks_font;
  char *listener_font;
  char *bold_peaks_font;
  char *axis_label_font;
  char *axis_numbers_font;
  int auto_resize;
  int horizontal_panes;
  int zoom_slider_width;
  int position_slider_width;
  int toggle_size;
  int channel_sash_indent;
  int channel_sash_size;
  int sash_size;
  int sash_indent;
  char *help_button_color;
  char *quit_button_color;
  char *reset_button_color;
  char *doit_button_color;
  char *doit_again_button_color;
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
  {"textfocuscolor", "Textfocuscolor", XmRString, sizeof(char *), XtOffset(sndres *, text_focus_color), XmRString, (XtPointer)TEXT_FOCUS_COLOR},
  {"redcolor", "Redcolor", XmRString, sizeof(char *), XtOffset(sndres *, red_color), XmRString, (XtPointer)RED_COLOR},
  {"greencolor", "Greencolor", XmRString, sizeof(char *), XtOffset(sndres *, green_color), XmRString, (XtPointer)GREEN_COLOR},
  {"whitecolor", "Whitecolor", XmRString, sizeof(char *), XtOffset(sndres *, white_color), XmRString, (XtPointer)WHITE_COLOR},
  {"blackcolor", "Blackcolor", XmRString, sizeof(char *), XtOffset(sndres *, black_color), XmRString, (XtPointer)BLACK_COLOR},
  {"lightbluecolor", "Lightbluecolor", XmRString, sizeof(char *), XtOffset(sndres *, light_blue_color), XmRString, (XtPointer)LIGHT_BLUE_COLOR},
  {"lighterbluecolor", "Ligterbluecolor", XmRString, sizeof(char *), XtOffset(sndres *, lighter_blue_color), XmRString, (XtPointer)LIGHTER_BLUE_COLOR},
  {"yellowcolor", "Yellowcolor", XmRString, sizeof(char *), XtOffset(sndres *, yellow_color), XmRString, (XtPointer)YELLOW_COLOR},
  {"envedwaveformcolor", "Envedwaveformcolor", XmRString, sizeof(char *), XtOffset(sndres *, enved_waveform_color), XmRString, (XtPointer)ENVED_WAVEFORM_COLOR},
  {"filterwaveformcolor", "Filterwaveformcolor", XmRString, sizeof(char *), XtOffset(sndres *, filter_control_waveform_color), XmRString, (XtPointer)FILTER_CONTROL_WAVEFORM_COLOR},
  {"graphcolor", "Graphcolor", XmRString, sizeof(char *), XtOffset(sndres *, graph_color), XmRString, (XtPointer)GRAPH_COLOR},
  {"selectedgraphcolor", "Selectedgraphcolor", XmRString, sizeof(char *), XtOffset(sndres *, selected_graph_color), XmRString, (XtPointer)SELECTED_GRAPH_COLOR},
  {"datacolor", "Datacolor", XmRString, sizeof(char *), XtOffset(sndres *, data_color), XmRString, (XtPointer)DATA_COLOR},
  {"selecteddatacolor", "Selecteddatacolor", XmRString, sizeof(char *), XtOffset(sndres *, selected_data_color), XmRString, (XtPointer)SELECTED_DATA_COLOR},
  {"markcolor", "Markcolor", XmRString, sizeof(char *), XtOffset(sndres *, mark_color), XmRString, (XtPointer)MARK_COLOR},
  {"sashcolor", "Sashcolor", XmRString, sizeof(char *), XtOffset(sndres *, sash_color), XmRString, (XtPointer)SASH_COLOR},
  {"pushedbuttoncolor", "Pushedbuttoncolor", XmRString, sizeof(char *), XtOffset(sndres *, pushed_button_color), XmRString, (XtPointer)PUSHED_BUTTON_COLOR},
  {"peaksFont", "PeaksFont", XmRString, sizeof(char *), XtOffset(sndres *, peaks_font), XmRString, (XtPointer)DEFAULT_PEAKS_FONT},
  {"listenerFont", "ListenerFont", XmRString, sizeof(char *), XtOffset(sndres *, listener_font), XmRString, (XtPointer)NULL},
  {"boldpeaksFont", "BoldPeaksFont", XmRString, sizeof(char *), XtOffset(sndres *, bold_peaks_font), XmRString, (XtPointer)DEFAULT_BOLD_PEAKS_FONT},
  {"axisLabelFont", "AxisLabelFont", XmRString, sizeof(char *), XtOffset(sndres *, axis_label_font), XmRString, (XtPointer)DEFAULT_AXIS_LABEL_FONT},
  {"axisNumbersFont", "AxisNumbersFont", XmRString, sizeof(char *), XtOffset(sndres *, axis_numbers_font), XmRString, (XtPointer)DEFAULT_AXIS_NUMBERS_FONT},
  {"autoResize", "AutoResize", XmRInt, sizeof(int), XtOffset(sndres *, auto_resize), XmRImmediate, (XtPointer)AUTO_RESIZE_DEFAULT},
  {"horizontalPanes", "HorizontalPanes", XmRInt, sizeof(int), XtOffset(sndres *, horizontal_panes), XmRImmediate, (XtPointer)SOUNDS_VERTICAL},
  {"zoomSliderWidth", "ZoomSliderWidth", XmRInt, sizeof(int), XtOffset(sndres *, zoom_slider_width), XmRImmediate, (XtPointer)ZOOM_SLIDER_WIDTH},
  {"positionSliderWidth", "PositionSliderWidth", XmRInt, sizeof(int), XtOffset(sndres *, position_slider_width), XmRImmediate, (XtPointer)POSITION_SLIDER_WIDTH},
  {"toggleSize", "ToggleSize", XmRInt, sizeof(int), XtOffset(sndres *, toggle_size), XmRImmediate, (XtPointer)TOGGLE_SIZE},
  {"channelSashIndent", "ChannelSashIndent", XmRInt, sizeof(int), XtOffset(sndres *, channel_sash_indent), XmRImmediate, (XtPointer)CHANNEL_SASH_INDENT},
  {"channelSashSize", "ChannelSashSize", XmRInt, sizeof(int), XtOffset(sndres *, channel_sash_size), XmRImmediate, (XtPointer)CHANNEL_SASH_SIZE},
  {"sashSize", "SashSize", XmRInt, sizeof(int), XtOffset(sndres *, sash_size), XmRImmediate, (XtPointer)SASH_SIZE},
  {"sashIndent", "SashIndent", XmRInt, sizeof(int), XtOffset(sndres *, sash_indent), XmRImmediate, (XtPointer)SASH_INDENT},
  {"helpbuttoncolor", "Helpbuttoncolor", XmRString, sizeof(char *), XtOffset(sndres *, help_button_color), XmRString, (XtPointer)HELP_BUTTON_COLOR},
  {"quitbuttoncolor", "Quitbuttoncolor", XmRString, sizeof(char *), XtOffset(sndres *, quit_button_color), XmRString, (XtPointer)QUIT_BUTTON_COLOR},
  {"resetbuttoncolor", "Resetbuttoncolor", XmRString, sizeof(char *), XtOffset(sndres *, reset_button_color), XmRString, (XtPointer)RESET_BUTTON_COLOR},
  {"doitbuttoncolor", "Doitbuttoncolor", XmRString, sizeof(char *), XtOffset(sndres *, doit_button_color), XmRString, (XtPointer)DOIT_BUTTON_COLOR},
  {"doitagainbuttoncolor", "Doitagainbuttoncolor", XmRString, sizeof(char *), XtOffset(sndres *, doit_again_button_color), XmRString, (XtPointer)DOIT_AGAIN_BUTTON_COLOR}
};

#ifndef SND_AS_WIDGET
static void window_close(Widget w, XtPointer context, XtPointer info)
{
  /* this is called from the window manager close event, not (exit) or the File:Exit item */
  snd_exit_cleanly(EXIT_FORCED);
}
#endif

#if (!HAVE_FAM)
static XtIntervalId auto_update_proc = 0;

static void auto_update_check(XtPointer context, XtIntervalId *id)
{
  if (auto_update_interval(ss) > 0.0)
    {
      if ((!(play_in_progress())) && 
	  (!(record_in_progress())))
	for_each_sound(sound_not_current);
      auto_update_proc = XtAppAddTimeOut(MAIN_APP(ss),
					 (unsigned long)(auto_update_interval(ss) * 1000),
					 (XtTimerCallbackProc)auto_update_check,
					 context);
    }
  else auto_update_proc = 0;
}

void auto_update_restart(void)
{
  if (auto_update_proc == 0)
    auto_update_proc = XtAppAddTimeOut(MAIN_APP(ss),
				       (unsigned long)(auto_update_interval(ss) * 1000),
				       (XtTimerCallbackProc)auto_update_check,
				       (XtPointer)NULL);
}
#else
void auto_update_restart(void) {}
#endif

#ifndef SND_AS_WIDGET
/* handle iconification */
static Widget *iconify_active_dialogs = NULL;
static void minify_maxify_window(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  XMapEvent *ev = (XMapEvent *)event;
  state_context *sx;
  int i;
  if ((!ss) || (!(ss->sgx)) || (!(ss->sgx->dialogs)))
    return;
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
   *
   * a rumor in the air that what we need is to catch StructureNotify event, then
   *   check in that for UnmapNotify
   */
  if (ev->type == UnmapNotify) 
    {
      if (iconify_active_dialogs) FREE(iconify_active_dialogs);
      iconify_active_dialogs = (Widget *)CALLOC(ss->sgx->num_dialogs, sizeof(Widget));

      for (i = 0; i < ss->sgx->num_dialogs; i++)
	if (ss->sgx->dialogs[i])
	  {
	    if (XtIsManaged(ss->sgx->dialogs[i]))
	      iconify_active_dialogs[i] = ss->sgx->dialogs[i];
	    XtUnmanageChild(ss->sgx->dialogs[i]);
	  }
    }
  else
    {
      if (ev->type == MapNotify)
	{
	  if (iconify_active_dialogs) 
	    {
	      for (i = 0; i < ss->sgx->num_dialogs; i++)
		if (iconify_active_dialogs[i])
		  XtManageChild(iconify_active_dialogs[i]);

	      FREE(iconify_active_dialogs);
	      iconify_active_dialogs = NULL;
	    }
	}
    }
}
#endif

#ifndef SND_AS_WIDGET
static Atom snd_v, snd_c;
#endif

#if HAVE_EXTENSION_LANGUAGE
static XEN window_property_changed_hook;

#ifndef SND_AS_WIDGET
static void who_called(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  /* watch for communication from some other program via the SND_COMMAND property */
  XPropertyEvent *ev = (XPropertyEvent *)event;
  if (ev->atom == snd_c)
    {
      Atom type;
      int format;
      unsigned long nitems, bytesafter;
      unsigned char *version[1];
      if (((XGetWindowProperty(XtDisplay(w), XtWindow(w), snd_c, 0L, (long)BUFSIZ, False,
			       XA_STRING, &type, &format, &nitems, &bytesafter, 
			       (unsigned char **)version)) == Success) && 
	  (type != None))
	if (version[0])
	  {
	    char *buf;
	    buf = (char *)(version[0]);
	    if ((snd_strlen(buf) > 1) ||
		((snd_strlen(buf) == 1) && (buf[0] != '\n')))
	      {
		if ((!(XEN_HOOKED(window_property_changed_hook))) ||
		    (!(XEN_TRUE_P(run_or_hook(window_property_changed_hook,
					      XEN_LIST_1(C_TO_XEN_STRING(buf)),
					      S_window_property_changed_hook)))))
		  snd_report_result(snd_catch_any(eval_str_wrapper, (void *)buf, buf), NULL);
	      }
	    free(version[0]);
	  }
    }
}
#endif
#endif

#if HAVE_SETJMP_H
#include <setjmp.h>

#if MUS_TRAP_SEGFAULT
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

#if MUS_DEBUGGING
static void trap_xt_error(String message)
{
  fprintf(stderr, message);
  XEN_ERROR(XEN_ERROR_TYPE("xt-error"),
	    XEN_LIST_2(C_TO_XEN_STRING("Xt error:"),
		       C_TO_XEN_STRING(message)));
}
#endif
#endif


static char **auto_open_file_names = NULL;
static int auto_open_files = 0;
static bool noglob = false, noinit = false, batch = false, nostdin = false;

#if HAVE_EXTENSION_LANGUAGE
static XtInputId stdin_id = 0;

static void get_stdin_string(XtPointer context, int *fd, XtInputId *id)
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
		      (unsigned char *)(SND_DATE), strlen(SND_DATE) + 1);
#if HAVE_EXTENSION_LANGUAGE
      XtAddEventHandler(shell, PropertyChangeMask, false, who_called, NULL);
#endif
      XtAddEventHandler(shell, StructureNotifyMask, false, minify_maxify_window, NULL);
#endif
      ss->sgx->graph_cursor = XCreateFontCursor(XtDisplay(MAIN_SHELL(ss)), in_graph_cursor(ss));
      ss->sgx->wait_cursor = XCreateFontCursor(XtDisplay(MAIN_SHELL(ss)), XC_watch);
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
#if (!HAVE_FAM)
      if (auto_update_interval(ss) > 0.0)
	XtAppAddTimeOut(MAIN_APP(ss), 
			(unsigned long)(auto_update_interval(ss) * 1000), 
			auto_update_check, 
			NULL);
#endif
#if MUS_TRAP_SEGFAULT
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
      if (ss->startup_errors)
	{
	  handle_listener(true); /* create it, if necessary */
	  listener_append(ss->startup_errors);
	  FREE(ss->startup_errors);
	  ss->startup_errors = NULL;
	}
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
#if 0
  int i;
  fprintf(stderr, "Xt warning: %s, %s: %s", type, name, defaultp);
  if (num_params) /* can be null! */
    for (i = 0; i < (int)(*num_params); i++)
      fprintf(stderr, " %s", params[i]);
  fprintf(stderr, "\n");
#endif
}
#endif

static void notebook_page_changed_callback(Widget w, XtPointer context, XtPointer info)
{
  /* if page chosen via major tab click, select that sound */
  XmNotebookCallbackStruct *nb = (XmNotebookCallbackStruct *)info;
  Widget page;
  if ((nb->reason == XmCR_MAJOR_TAB) && (nb->page_widget))
    {
      int index = 0;
      snd_info *sp;
      page = nb->page_widget;
      if (page)
	{
	  XtVaGetValues(page, XmNuserData, &index, NULL);
	  if ((index < ss->max_sounds) && 
	      (snd_ok(ss->sounds[index])))
	    {
	      sp = ss->sounds[index];
	      if (sp->selected_channel == NO_SELECTION)
		select_channel(ss->sounds[index], 0);
	      else select_channel(ss->sounds[index], sp->selected_channel);
	    }
	}
    }
}

color_t get_in_between_color(color_t fg, color_t bg)
{
  Colormap cmap;
  Display *dpy;
  int scr;
  XColor fg_color, bg_color, new_color;
  dpy = MAIN_DISPLAY(ss);
  scr = DefaultScreen(dpy);
  cmap = DefaultColormap(dpy, scr);
  fg_color.flags = DoRed | DoGreen | DoBlue;
  fg_color.pixel = fg;
  XQueryColor(dpy, cmap, &fg_color);
  bg_color.flags = DoRed | DoGreen | DoBlue;
  bg_color.pixel = bg;
  XQueryColor(dpy, cmap, &bg_color);
  new_color.flags = DoRed | DoGreen | DoBlue;
  new_color.red = (unsigned short)((fg_color.red + (2 * bg_color.red)) / 3);
  new_color.green = (unsigned short)((fg_color.green + (2 * bg_color.green)) / 3);
  new_color.blue = (unsigned short)((fg_color.blue + (2 * bg_color.blue)) / 3);
  if ((XAllocColor(dpy, cmap, &new_color)) == 0)
    return(fg);
  return(new_color.pixel);
}

static Pixel get_color(Widget shell, const char *rs_color, const char *defined_color, 
		       const char *fallback_color, const char *second_fallback_color, bool use_white)
{
  Colormap cmap;
  Display *dpy;
  int scr;
  XColor tmp_color, ignore;
  dpy = XtDisplay(shell);
  scr = DefaultScreen(dpy);
  cmap = DefaultColormap(dpy, scr);
  /* I suppose we could use XQueryColors and search for the closest available, or try yet again to get XCreateColormap to behave itself */
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

static void save_a_color(FILE *Fp, Display *dpy, Colormap cmap, const char *rs_name, const char *def_name, Pixel pix, const char *ext_name)
{
#if HAVE_EXTENSION_LANGUAGE
  Status lookup_ok;
  XColor default_color, ignore;
  lookup_ok = XLookupColor(dpy, cmap, rs_name, &default_color, &ignore);
  if (!lookup_ok) 
    lookup_ok = XLookupColor(dpy, cmap, def_name, &default_color, &ignore);
  if (lookup_ok)
    {
      XColor current_color;
      current_color.flags = DoRed | DoGreen | DoBlue;
      current_color.pixel = pix;
      XQueryColor(dpy, cmap, &current_color);
      if ((current_color.red != default_color.red) ||
	  (current_color.green != default_color.green) ||
	  (current_color.blue != default_color.blue))
#if HAVE_FORTH
	fprintf(Fp, "%.3f %.3f %.3f %s set-%s drop\n", 
		(float)current_color.red / 65535.0,
		(float)current_color.green / 65535.0,
		(float)current_color.blue / 65535.0,
		S_make_color,
		ext_name);
#else
#if HAVE_SCHEME
	fprintf(Fp, "(set! (%s) (%s %.3f %.3f %.3f))\n", 
#endif
#if HAVE_RUBY
	fprintf(Fp, "set_%s(%s(%.3f, %.3f, %.3f))\n", 
#endif
		TO_PROC_NAME(ext_name), 
		TO_PROC_NAME(S_make_color),
		(float)current_color.red / 65535.0,
		(float)current_color.green / 65535.0,
		(float)current_color.blue / 65535.0);
#endif
    }
#endif
}

void save_colors(FILE *Fp)
{
  Colormap cmap;
  Display *dpy;
  int scr;
  sndres snd_rs;

  dpy = XtDisplay(ss->sgx->mainshell);
  scr = DefaultScreen(dpy);
  cmap = DefaultColormap(dpy, scr);
  XtGetApplicationResources(ss->sgx->mainshell, &snd_rs, resources, XtNumber(resources), NULL, 0);

  save_a_color(Fp, dpy, cmap, snd_rs.basic_color,             BASIC_COLOR,             ss->sgx->basic_color,             S_basic_color);
  save_a_color(Fp, dpy, cmap, snd_rs.cursor_color,            CURSOR_COLOR,            ss->sgx->cursor_color,            S_cursor_color);
  save_a_color(Fp, dpy, cmap, snd_rs.data_color,              DATA_COLOR,              ss->sgx->data_color,              S_data_color);
  save_a_color(Fp, dpy, cmap, snd_rs.selected_data_color,     SELECTED_DATA_COLOR,     ss->sgx->selected_data_color,     S_selected_data_color);
  save_a_color(Fp, dpy, cmap, snd_rs.highlight_color,         HIGHLIGHT_COLOR,         ss->sgx->highlight_color,         S_highlight_color);
  save_a_color(Fp, dpy, cmap, snd_rs.position_color,          POSITION_COLOR,          ss->sgx->position_color,          S_position_color);
  save_a_color(Fp, dpy, cmap, snd_rs.zoom_color,              ZOOM_COLOR,              ss->sgx->zoom_color,              S_zoom_color);
  save_a_color(Fp, dpy, cmap, snd_rs.selection_color,         SELECTION_COLOR,         ss->sgx->selection_color,         S_selection_color);
  save_a_color(Fp, dpy, cmap, snd_rs.mix_color,               MIX_COLOR,               ss->sgx->mix_color,               S_mix_color);
  save_a_color(Fp, dpy, cmap, snd_rs.enved_waveform_color,    ENVED_WAVEFORM_COLOR,    ss->sgx->enved_waveform_color,    S_enved_waveform_color);
  save_a_color(Fp, dpy, cmap, snd_rs.listener_color,          LISTENER_COLOR,          ss->sgx->listener_color,          S_listener_color);
  save_a_color(Fp, dpy, cmap, snd_rs.listener_text_color,     LISTENER_TEXT_COLOR,     ss->sgx->listener_text_color,     S_listener_text_color);
  save_a_color(Fp, dpy, cmap, snd_rs.graph_color,             GRAPH_COLOR,             ss->sgx->graph_color,             S_graph_color);
  save_a_color(Fp, dpy, cmap, snd_rs.selected_graph_color,    SELECTED_GRAPH_COLOR,    ss->sgx->selected_graph_color,    S_selected_graph_color);
  save_a_color(Fp, dpy, cmap, snd_rs.mark_color,              MARK_COLOR,              ss->sgx->mark_color,              S_mark_color);
  save_a_color(Fp, dpy, cmap, snd_rs.sash_color,              SASH_COLOR,              ss->sgx->sash_color,              S_sash_color);
  save_a_color(Fp, dpy, cmap, snd_rs.pushed_button_color,     PUSHED_BUTTON_COLOR,     ss->sgx->pushed_button_color,     S_pushed_button_color);
  save_a_color(Fp, dpy, cmap, snd_rs.text_focus_color,        TEXT_FOCUS_COLOR,        ss->sgx->text_focus_color,        S_text_focus_color);
  save_a_color(Fp, dpy, cmap, snd_rs.doit_button_color,       DOIT_BUTTON_COLOR,       ss->sgx->doit_button_color,       S_doit_button_color);
  save_a_color(Fp, dpy, cmap, snd_rs.doit_again_button_color, DOIT_AGAIN_BUTTON_COLOR, ss->sgx->doit_again_button_color, S_doit_again_button_color);
  save_a_color(Fp, dpy, cmap, snd_rs.help_button_color,       HELP_BUTTON_COLOR,       ss->sgx->help_button_color,       S_help_button_color);
  save_a_color(Fp, dpy, cmap, snd_rs.quit_button_color,       QUIT_BUTTON_COLOR,       ss->sgx->quit_button_color,       S_quit_button_color);
  save_a_color(Fp, dpy, cmap, snd_rs.reset_button_color,      RESET_BUTTON_COLOR,      ss->sgx->reset_button_color,      S_reset_button_color);
  save_a_color(Fp, dpy, cmap, snd_rs.filter_control_waveform_color, FILTER_CONTROL_WAVEFORM_COLOR, ss->sgx->filter_control_waveform_color, S_filter_control_waveform_color);
}

#ifdef SND_AS_WIDGET

void snd_as_widget(int argc, char **argv, XtAppContext app, Widget parent, Arg *caller_args, int caller_argn)
{

#else

static char *fallbacks[] = {
  "*fontList: " DEFAULT_FONTLIST,
  "*enableEtchedInMenu: True",
  "*enableThinThickness: True",
  "*enableToggleColor: True",
  "*enableToggleVisual: True",
  NULL
};

void snd_doit(int argc, char **argv)
{
  XtAppContext app;     
#endif
  Widget shell;
  Display *dpy;
  Drawable wn;
  Arg args[32];
  int i, n, err = 0;
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
  ss->channel_min_height = CHANNEL_MIN_HEIGHT;
  ss->Graph_Cursor = DEFAULT_GRAPH_CURSOR;
#ifndef SND_AS_WIDGET
#if MUS_ALPHA
  XtSetArg(args[0], XtNwidth, 640);
  XtSetArg(args[1], XtNheight, 256);
  shell = XtAppInitialize(&app, "Snd", NULL, 0, &argc, argv, fallbacks, args, 2);
#else
  shell = XtVaOpenApplication(&app, "Snd", NULL, 0, &argc, argv, fallbacks, applicationShellWidgetClass,
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

  if ((snd_rs.horizontal_panes >= (int)SOUNDS_VERTICAL) &&
      (snd_rs.horizontal_panes <= (int)SOUNDS_IN_SEPARATE_WINDOWS))
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
		  else
		    if (strcmp(argv[i], "--features") == 0) /* testing (compsnd) */
		      check_features_list(argv[i + 1]);

  ss->batch_mode = batch;
  if (batch) XtSetMappedWhenManaged(shell, 0);
  set_auto_resize(snd_rs.auto_resize);
  ss->zoom_slider_width = snd_rs.zoom_slider_width;
  ss->position_slider_width = snd_rs.position_slider_width;
  ss->channel_sash_indent = snd_rs.channel_sash_indent;
  ss->channel_sash_size = snd_rs.channel_sash_size;
  ss->sash_size = snd_rs.sash_size;
  ss->sash_indent = snd_rs.sash_indent;
  ss->toggle_size = snd_rs.toggle_size;
#ifdef MUS_MAC_OS
  ss->click_time = XtGetMultiClickTime(dpy);
#else
  ss->click_time = (Tempus)(0.5 * XtGetMultiClickTime(dpy));
#endif
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
	XFree(vi);
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
  sx->white =                   get_color(shell, snd_rs.white_color,             WHITE_COLOR,             NULL, NULL, true);
  sx->black =                   get_color(shell, snd_rs.black_color,             BLACK_COLOR,             NULL, NULL, false);
  sx->light_blue =              get_color(shell, snd_rs.light_blue_color,        LIGHT_BLUE_COLOR,        "blue", NULL, true);
  sx->lighter_blue =            get_color(shell, snd_rs.lighter_blue_color,      LIGHTER_BLUE_COLOR,      "blue", NULL, true);
  sx->red =                     get_color(shell, snd_rs.red_color,               RED_COLOR,               NULL, NULL, false);
  sx->green =                   get_color(shell, snd_rs.green_color,             GREEN_COLOR,             NULL, NULL, false);
  sx->yellow =                  get_color(shell, snd_rs.yellow_color,            YELLOW_COLOR,            NULL, NULL, true);
  sx->highlight_color =         get_color(shell, snd_rs.highlight_color,         HIGHLIGHT_COLOR,         "gray90", NULL, true);
  sx->basic_color =             get_color(shell, snd_rs.basic_color,             BASIC_COLOR,             "gray80", "gray", true);
  sx->position_color =          get_color(shell, snd_rs.position_color,          POSITION_COLOR,          "gray60", "blue", false);
  sx->zoom_color =              get_color(shell, snd_rs.zoom_color,              ZOOM_COLOR,              "gray20", "gray", false);
  sx->cursor_color =            get_color(shell, snd_rs.cursor_color,            CURSOR_COLOR,            NULL, NULL, false);
  sx->selection_color =         get_color(shell, snd_rs.selection_color,         SELECTION_COLOR,         "gray80", NULL, false);
  sx->mix_color =               get_color(shell, snd_rs.mix_color,               MIX_COLOR,               "red", NULL, false);
  sx->enved_waveform_color =    get_color(shell, snd_rs.enved_waveform_color,    ENVED_WAVEFORM_COLOR,    "red", NULL, false);
  sx->filter_control_waveform_color = get_color(shell, snd_rs.filter_control_waveform_color, FILTER_CONTROL_WAVEFORM_COLOR, "blue", NULL, false);
  sx->listener_color =          get_color(shell, snd_rs.listener_color,          LISTENER_COLOR,          NULL, NULL, true);
  sx->listener_text_color =     get_color(shell, snd_rs.listener_text_color,     LISTENER_TEXT_COLOR,     NULL, NULL, false);
  sx->graph_color =             get_color(shell, snd_rs.graph_color,             GRAPH_COLOR,             NULL, NULL, true);
  sx->selected_graph_color =    get_color(shell, snd_rs.selected_graph_color,    SELECTED_GRAPH_COLOR,    NULL, NULL, true);
  sx->data_color =              get_color(shell, snd_rs.data_color,              DATA_COLOR,              NULL, NULL, false);
  sx->selected_data_color =     get_color(shell, snd_rs.selected_data_color,     SELECTED_DATA_COLOR,     NULL, NULL, false);
  sx->mark_color =              get_color(shell, snd_rs.mark_color,              MARK_COLOR,              "red", NULL, false);
  sx->sash_color =              get_color(shell, snd_rs.sash_color,              SASH_COLOR,              NULL, NULL, false);
  sx->pushed_button_color =     get_color(shell, snd_rs.pushed_button_color,     PUSHED_BUTTON_COLOR,     "blue", NULL, false);
  sx->text_focus_color =        get_color(shell, snd_rs.text_focus_color,        TEXT_FOCUS_COLOR,        NULL, NULL, true);
  sx->help_button_color =       get_color(shell, snd_rs.help_button_color,       HELP_BUTTON_COLOR,       "green", NULL, true);
  sx->quit_button_color =       get_color(shell, snd_rs.quit_button_color,       QUIT_BUTTON_COLOR,       "blue", NULL, true);
  sx->reset_button_color =      get_color(shell, snd_rs.reset_button_color,      RESET_BUTTON_COLOR,      "red", NULL, true);
  sx->doit_button_color =       get_color(shell, snd_rs.doit_button_color,       DOIT_BUTTON_COLOR,       "red", NULL, true);
  sx->doit_again_button_color = get_color(shell, snd_rs.doit_again_button_color, DOIT_AGAIN_BUTTON_COLOR, "red", NULL, true);

  sx->grid_color = get_in_between_color(sx->data_color, sx->graph_color);
  sx->selected_grid_color = get_in_between_color(sx->selected_data_color, sx->selected_graph_color);

  sx->orig_data_color = sx->data_color;
  sx->orig_selected_data_color = sx->selected_data_color;
  sx->orig_mark_color = sx->mark_color;
  sx->orig_mix_color = sx->mix_color;
  sx->orig_graph_color = sx->graph_color;
  sx->orig_selected_graph_color = sx->selected_graph_color;
  sx->orig_listener_color = sx->listener_color;
  sx->orig_listener_text_color = sx->listener_text_color;
  sx->orig_cursor_color = sx->cursor_color;
  sx->orig_basic_color = sx->basic_color;
  sx->orig_selection_color = sx->selection_color;
  sx->orig_zoom_color = sx->zoom_color;
  sx->orig_position_color = sx->position_color;
  sx->orig_highlight_color = sx->highlight_color;

  if ((!(set_peaks_font(snd_rs.peaks_font))) &&
      (!(set_peaks_font(DEFAULT_PEAKS_FONT))) &&
      (!(set_peaks_font(FALLBACK_FONT))))
    fprintf(stderr, _("can't find peaks font %s"), snd_rs.peaks_font);

  if ((!(set_tiny_font(DEFAULT_TINY_FONT))) &&
      (!(set_tiny_font(FALLBACK_FONT))))
    fprintf(stderr, _("can't find tiny font %s"), DEFAULT_TINY_FONT);

  if ((!(set_bold_peaks_font(snd_rs.bold_peaks_font))) &&
      (!(set_bold_peaks_font(DEFAULT_BOLD_PEAKS_FONT))) &&
      (!(set_bold_peaks_font(FALLBACK_FONT))))
    fprintf(stderr, _("can't find bold peaks font %s"), snd_rs.bold_peaks_font);

  if ((!(set_axis_label_font(snd_rs.axis_label_font))) &&
      (!(set_axis_label_font(DEFAULT_AXIS_LABEL_FONT))) &&
      (!(set_axis_label_font(FALLBACK_FONT))))
    fprintf(stderr, _("can't find axis label font %s"), snd_rs.axis_label_font);

  if ((!(set_axis_numbers_font(snd_rs.axis_numbers_font))) &&
      (!(set_axis_numbers_font(DEFAULT_AXIS_NUMBERS_FONT))) &&
      (!(set_axis_numbers_font(FALLBACK_FONT))))
    fprintf(stderr, _("can't find axis numbers font %s"), snd_rs.axis_numbers_font);

  if ((snd_rs.listener_font) &&
      (!(set_listener_font(snd_rs.listener_font))))
    fprintf(stderr, _("can't find listener font %s"), snd_rs.listener_font);

  ss->orig_axis_label_font = copy_string(axis_label_font(ss));
  ss->orig_axis_numbers_font = copy_string(axis_numbers_font(ss));
  ss->orig_peaks_font = copy_string(peaks_font(ss));
  ss->orig_bold_peaks_font = copy_string(bold_peaks_font(ss));
  ss->orig_listener_font = copy_string(listener_font(ss));
  ss->orig_tiny_font = copy_string(tiny_font(ss));

  XtVaSetValues(shell, XmNbackground, sx->basic_color, NULL);

#ifndef SND_AS_WIDGET
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
  n = attach_all_sides(args, n);
  XtSetArg(args[n], XmNallowResize, true); n++;
  sx->mainpane = XtCreateManagedWidget("mainpane", xmFormWidgetClass, shell, args, n);
#else
  sx->mainpane = XtCreateManagedWidget("mainpane", xmFormWidgetClass, parent, caller_args, caller_argn);
#endif
  menu = add_menu();

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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

    case SOUNDS_IN_NOTEBOOK:
      XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
      sx->soundpanebox = XtCreateManagedWidget("soundpane", xmPanedWindowWidgetClass, sx->mainpane, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNframeBackground, sx->zoom_color); n++;
      XtSetArg(args[n], XmNbindingType, XmNONE); n++;
      XtSetArg(args[n], XmNbackPagePlacement, XmTOP_RIGHT); n++;
      XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
      sx->soundpane = XtCreateWidget("nb", xmNotebookWidgetClass, sx->soundpanebox, args, n);

      {
	/* get rid of the useless spinbox */
	Widget scroll;
	n = 0;
	XtSetArg(args[n], XmNnotebookChildType, XmPAGE_SCROLLER); n++;
	scroll = XtCreateWidget("scroller", xmScrollBarWidgetClass, sx->soundpane, NULL, 0);
      }
      XtManageChild(sx->soundpane);
      XtAddCallback(sx->soundpane, XmNpageChangedCallback, notebook_page_changed_callback, NULL);
      map_over_children(sx->soundpane, set_main_color_of_widget); /* appears to be a no-op */
      break;
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
  gv.foreground = sx->filter_control_waveform_color;
  sx->fltenv_data_gc = XCreateGC(dpy, wn, GCBackground | GCForeground | GCFunction, &gv);

  initialize_colormap(); /* X11 not ours */
  make_icons_transparent(snd_rs.basic_color);

  BACKGROUND_ADD(startup_funcs, NULL);
  /* this complication seems necessary because we might be loading Scheme code files
   *   from the startup args (via -l) and they might contain errors etc -- we want to
   *   be sure the complete interface is running (via the XtAppMainLoop below) when
   *   we drop into the error handler.  (Also we need the two longjmp sets, but they
   *   must follow this startup loop).
   */

#if HAVE_SETJMP_H
#if MUS_TRAP_SEGFAULT
  if (sigsetjmp(envHandleEventsLoop, 1))
    {
      if (!(ss->exiting))
	snd_error_without_format(_("Caught seg fault (will try to continue):\n"));
      else
	{
	  snd_error_without_format(_("Caught seg fault while trying to exit.\n"));
	  exit(0);
	}
    }
#endif
  if (setjmp(top_level_jump))
    {
      if (!(ss->jump_ok))
	snd_error_without_format(_("Caught top level error (will try to continue):\n"));
      else ss->jump_ok = false;
    }
#if MUS_DEBUGGING
  XtAppSetErrorHandler(MAIN_APP(ss), trap_xt_error);
#endif
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
#if HAVE_EXTENSION_LANGUAGE
  #define H_window_property_changed_hook S_window_property_changed_hook "(command): called upon receipt of a change in SND_COMMAND (an X window property)"
  window_property_changed_hook = XEN_DEFINE_HOOK(S_window_property_changed_hook, 1, H_window_property_changed_hook);
#endif
#if HAVE_GL
  XEN_DEFINE_PROCEDURE("snd-glx-context", g_snd_glx_context_w, 0, 0, 0, "OpenGL GLXContext");
#endif
}
