#include "snd.h"

#define FALLBACK_FONT        "fixed"
#define DEFAULT_LISTENER_FONT "9x15"
#define DEFAULT_FONTLIST     "9x15"

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
#define SASH_COLOR           "lightgreen"
#define CHANNEL_SASH_INDENT  -10
#define CHANNEL_SASH_SIZE    0
/* 0 means: use Motif default size */

#define POSITION_SLIDER_WIDTH 13
#define ZOOM_SLIDER_WIDTH 10
#if (!__linux__) && (!_LINUX__)
  #define TOGGLE_SIZE 0
#else
  #define TOGGLE_SIZE 15
#endif
#define CHANNEL_MIN_HEIGHT 150  /* open size (set to 5 immediately thereafter) */
                                /* paned window default setup is not very smart, so we force these to be this size to begin with */
                                /* this number is only a first approximation -- we try not to expand below the screen */
                                /* if too small (i.e. 100), the scrollbars are sometimes messed up on the initial layout */

#define SASH_SIZE 16
#define SASH_INDENT -20
#define AUTO_RESIZE_DEFAULT 1

#define INITIAL_WINDOW_WIDTH 700
#define INITIAL_WINDOW_HEIGHT 300


static void window_close(Widget w, XtPointer context, XtPointer info)
{
  /* this is called from the window manager close event, not (exit) or the File:Exit item */
  snd_exit_cleanly(EXIT_FORCED);
}


static XtIntervalId auto_update_proc = 0;

static void auto_update_check(XtPointer context, XtIntervalId *id)
{
  if (auto_update_interval(ss) > 0.0)
    {
      if (!(play_in_progress()))
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



/* handle iconification */
static Widget *iconify_active_dialogs = NULL;

static void minify_maxify_window(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  XMapEvent *ev = (XMapEvent *)event;
  int i;
  if ((!ss) || (!(ss->dialogs)))
    return;

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
      if (iconify_active_dialogs) free(iconify_active_dialogs);
      iconify_active_dialogs = (Widget *)calloc(ss->num_dialogs, sizeof(Widget));

      for (i = 0; i < ss->num_dialogs; i++)
	if (ss->dialogs[i])
	  {
	    if (XtIsManaged(ss->dialogs[i]))
	      iconify_active_dialogs[i] = ss->dialogs[i];
	    XtUnmanageChild(ss->dialogs[i]);
	  }
    }
  else
    {
      if (ev->type == MapNotify)
	{
	  if (iconify_active_dialogs) 
	    {
	      for (i = 0; i < ss->num_dialogs; i++)
		if (iconify_active_dialogs[i])
		  XtManageChild(iconify_active_dialogs[i]);

	      free(iconify_active_dialogs);
	      iconify_active_dialogs = NULL;
	    }
	}
    }
}


#ifndef _MSC_VER
#include <setjmp.h>

static jmp_buf top_level_jump;

void top_level_catch(int ignore);
void top_level_catch(int ignore)
{
  longjmp(top_level_jump, 1);
}
#endif


static char **auto_open_file_names = NULL;
static int auto_open_files = 0;
static bool noglob = false, noinit = false, batch = false, nostdin = false;

#if HAVE_EXTENSION_LANGUAGE
static XtInputId stdin_id = 0;

static void get_stdin_string(XtPointer context, int *fd, XtInputId *id)
{
  int size;
  ssize_t bytes;
  char *buf;
  buf = (char *)calloc(1024, sizeof(char));
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
	  buf = (char *)realloc(buf, size);
	  bytes = read(*fd, (char *)(buf + size - 1024), 1024);
	}
      snd_eval_stdin_str(buf);
    }
  free(buf);
}
#endif


static void startup_funcs(void)
{
  Atom wm_delete_window;
  Display *dpy;
  Widget shell;
  static int auto_open_ctr = 0;

  ss->file_monitor_ok = initialize_file_monitor();

  shell = ss->mainshell;
  dpy = MAIN_DISPLAY(ss);

#ifndef __alpha__
  add_menu_drop();
#endif

  /* trap outer-level Close for cleanup check */
  wm_delete_window = XmInternAtom(dpy, (char *)"WM_DELETE_WINDOW", false);
  XmAddWMProtocolCallback(shell, wm_delete_window, window_close, NULL);
  XtAddEventHandler(shell, StructureNotifyMask, false, minify_maxify_window, NULL);

  ss->graph_cursor = XCreateFontCursor(XtDisplay(MAIN_SHELL(ss)), in_graph_cursor(ss));
  ss->wait_cursor = XCreateFontCursor(XtDisplay(MAIN_SHELL(ss)), XC_watch);
  ss->bounds_cursor = XCreateFontCursor(XtDisplay(MAIN_SHELL(ss)), XC_sb_h_double_arrow);
  ss->yaxis_cursor = XCreateFontCursor(XtDisplay(MAIN_SHELL(ss)), XC_sb_v_double_arrow);
  ss->play_cursor = XCreateFontCursor(XtDisplay(MAIN_SHELL(ss)), XC_sb_right_arrow);
  ss->loop_play_cursor = XCreateFontCursor(XtDisplay(MAIN_SHELL(ss)), XC_sb_left_arrow);

#if HAVE_EXTENSION_LANGUAGE
  snd_load_init_file(noglob, noinit);
#endif

#if (!_MSC_VER) && HAVE_EXTENSION_LANGUAGE && !__MINGW32__
  if (!nostdin)
    {
      signal(SIGTTIN, SIG_IGN);
      signal(SIGTTOU, SIG_IGN);
      /* these signals are sent by a shell if we start Snd as a background process,
       * but try to read stdin (needed to support the emacs subjob connection).  If
       * we don't do this, the background job is suspended when the shell sends SIGTTIN.
       */
      stdin_id = XtAppAddInput(MAIN_APP(ss), 
			       STDIN_FILENO, 
			       (XtPointer)XtInputReadMask, 
			       get_stdin_string, 
			       NULL);
    }
#endif

  while (auto_open_ctr < auto_open_files)
    auto_open_ctr = handle_next_startup_arg(auto_open_ctr, auto_open_file_names, false, auto_open_files);

  if (ss->init_window_width > 0) set_widget_width(MAIN_SHELL(ss), ss->init_window_width);
  if (ss->init_window_height > 0) set_widget_height(MAIN_SHELL(ss), ss->init_window_height);
  if (ss->init_window_x != DEFAULT_INIT_WINDOW_X) set_widget_x(MAIN_SHELL(ss), ss->init_window_x);
  if (ss->init_window_y != DEFAULT_INIT_WINDOW_Y) set_widget_y(MAIN_SHELL(ss), ss->init_window_y);

  if (!ss->file_monitor_ok)
    {
      if (auto_update_interval(ss) > 0.0)
	XtAppAddTimeOut(MAIN_APP(ss), 
			(unsigned long)(auto_update_interval(ss) * 1000), 
			auto_update_check, 
			NULL);
    }


  if ((ss->sounds) &&
      (ss->selected_sound == NO_SELECTION))
    {
      snd_info *sp;
      sp = ss->sounds[0];
      if ((sp) && 
	  (sp->inuse == SOUND_NORMAL) &&
	  (sp->selected_channel == NO_SELECTION)) /* don't clobber possible select-channel in loaded startup files */
	select_channel(sp, 0);
    }

  if ((ss->init_window_height == 0) && (sound_style(ss) == SOUNDS_HORIZONTAL))
    set_widget_height(MAIN_SHELL(ss), 200); /* otherwise it's just a title bar! */
}


#include <X11/xpm.h>

static void SetupIcon(Widget shell)
{
  Display *dpy;
  Window root;
  int scr;
  Pixmap pix, mask;
  XpmAttributes attributes;
  dpy = XtDisplay(shell);
  root = DefaultRootWindow(dpy);
  scr = DefaultScreen(dpy);
  XtVaGetValues(shell, XmNdepth, &attributes.depth, XmNcolormap, &attributes.colormap, NULL);
  attributes.visual = DefaultVisual(dpy, scr);
  attributes.valuemask = XpmDepth | XpmColormap | XpmVisual;
  XpmCreatePixmapFromData(dpy, root, (char **)snd_icon_bits(), &pix, &mask, &attributes);
  if (mask) XFreePixmap(dpy, mask);
  XtVaSetValues(shell, XmNiconPixmap, pix, NULL);
}


static void muffle_warning(char *name, char *type, char *klass, char *defaultp, char **params, unsigned int *num_params)
{
}


static void notebook_page_changed_callback(Widget w, XtPointer context, XtPointer info)
{
  /* if page chosen via major tab click, select that sound */
  XmNotebookCallbackStruct *nb = (XmNotebookCallbackStruct *)info;
  Widget page;
  if ((nb->reason == XmCR_MAJOR_TAB) && (nb->page_widget))
    {
      page = nb->page_widget;
      if (page)
	{
	  int index;
	  pointer_or_int_t data;
	  XtVaGetValues(page, XmNuserData, &data, NULL);
	  index = (int)data;
	  if ((index < ss->max_sounds) && 
	      (snd_ok(ss->sounds[index])))
	    {
	      snd_info *sp;
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
  new_color.red = (rgb_t)((fg_color.red + (2 * bg_color.red)) / 3);
  new_color.green = (rgb_t)((fg_color.green + (2 * bg_color.green)) / 3);
  new_color.blue = (rgb_t)((fg_color.blue + (2 * bg_color.blue)) / 3);
  if ((XAllocColor(dpy, cmap, &new_color)) == 0)
    return(fg);
  return(new_color.pixel);
}


static Pixel get_color(Widget shell, const char *defined_color, 
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

  if ((!XAllocNamedColor(dpy, cmap, defined_color, &tmp_color, &ignore)) &&
      ((!fallback_color) || 
       (!XAllocNamedColor(dpy, cmap, fallback_color, &tmp_color, &ignore))) &&
      ((!second_fallback_color) || 
       (!XAllocNamedColor(dpy, cmap, second_fallback_color, &tmp_color, &ignore))))
    {
      /* snd_error here causes a seg fault (it builds on mainpane which has not yet been created) */
      if (use_white)
	{
	  fprintf(stderr, "can't get color %s -- will use white\n", defined_color);
	  return(WhitePixel(dpy, scr));
	}
      else
	{
	  fprintf(stderr, "can't get color %s -- will use black\n", defined_color);
	  return(BlackPixel(dpy, scr));
	}
    }
  return(tmp_color.pixel);
}


static void save_a_color(FILE *Fp, Display *dpy, Colormap cmap, const char *name, Pixel pix, const char *ext_name)
{
#if HAVE_EXTENSION_LANGUAGE
  Status lookup_ok;
  XColor default_color, ignore;

  lookup_ok = XLookupColor(dpy, cmap, name, &default_color, &ignore);
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
		rgb_to_float(current_color.red),
		rgb_to_float(current_color.green),
		rgb_to_float(current_color.blue),
		S_make_color,
		ext_name);
#else

#if HAVE_SCHEME
	fprintf(Fp, "(set! (%s) (%s %.3f %.3f %.3f))\n", 
#endif

#if HAVE_RUBY
	fprintf(Fp, "set_%s(%s(%.3f, %.3f, %.3f))\n", 
#endif
		to_proc_name(ext_name), 
		to_proc_name(S_make_color),
		rgb_to_float(current_color.red),
		rgb_to_float(current_color.green),
		rgb_to_float(current_color.blue));
#endif
    }
#endif
}


void save_colors(FILE *Fp)
{
  Colormap cmap;
  Display *dpy;
  int scr;

  dpy = XtDisplay(ss->mainshell);
  scr = DefaultScreen(dpy);
  cmap = DefaultColormap(dpy, scr);

  save_a_color(Fp, dpy, cmap, BASIC_COLOR,          ss->basic_color,          S_basic_color);
  save_a_color(Fp, dpy, cmap, CURSOR_COLOR,         ss->cursor_color,         S_cursor_color);
  save_a_color(Fp, dpy, cmap, DATA_COLOR,           ss->data_color,           S_data_color);
  save_a_color(Fp, dpy, cmap, SELECTED_DATA_COLOR,  ss->selected_data_color,  S_selected_data_color);
  save_a_color(Fp, dpy, cmap, HIGHLIGHT_COLOR,      ss->highlight_color,      S_highlight_color);
  save_a_color(Fp, dpy, cmap, POSITION_COLOR,       ss->position_color,       S_position_color);
  save_a_color(Fp, dpy, cmap, ZOOM_COLOR,           ss->zoom_color,           S_zoom_color);
  save_a_color(Fp, dpy, cmap, SELECTION_COLOR,      ss->selection_color,      S_selection_color);
  save_a_color(Fp, dpy, cmap, MIX_COLOR,            ss->mix_color,            S_mix_color);
  save_a_color(Fp, dpy, cmap, ENVED_WAVEFORM_COLOR, ss->enved_waveform_color, S_enved_waveform_color);
  save_a_color(Fp, dpy, cmap, LISTENER_COLOR,       ss->listener_color,       S_listener_color);
  save_a_color(Fp, dpy, cmap, LISTENER_TEXT_COLOR,  ss->listener_text_color,  S_listener_text_color);
  save_a_color(Fp, dpy, cmap, GRAPH_COLOR,          ss->graph_color,          S_graph_color);
  save_a_color(Fp, dpy, cmap, SELECTED_GRAPH_COLOR, ss->selected_graph_color, S_selected_graph_color);
  save_a_color(Fp, dpy, cmap, MARK_COLOR,           ss->mark_color,           S_mark_color);
  save_a_color(Fp, dpy, cmap, SASH_COLOR,           ss->sash_color,           S_sash_color);
  save_a_color(Fp, dpy, cmap, TEXT_FOCUS_COLOR,     ss->text_focus_color,     S_text_focus_color);
  save_a_color(Fp, dpy, cmap, FILTER_CONTROL_WAVEFORM_COLOR, ss->filter_control_waveform_color, S_filter_control_waveform_color);
}


static char *fallbacks[] = {
  (char *)"*fontList: " DEFAULT_FONTLIST,
  (char *)"*enableEtchedInMenu: True",
  (char *)"*enableThinThickness: True",
  (char *)"*enableToggleColor: True",
  (char *)"*enableToggleVisual: True",
 NULL
};


void snd_doit(int argc, char **argv)
{
  XtAppContext app;     
  Widget shell;
  Display *dpy;
  Drawable wn;
  Arg args[32];
  int i, n;
  Widget menu;
  XGCValues gv;
  char *app_title = NULL;

  int err = 0;
  XtSetLanguageProc(NULL, NULL, NULL);

  ss->channel_min_height = CHANNEL_MIN_HEIGHT;
  ss->Graph_Cursor = DEFAULT_GRAPH_CURSOR;

#ifndef __alpha__
  XtSetArg(args[0], XtNwidth, INITIAL_WINDOW_WIDTH);
  XtSetArg(args[1], XtNheight, INITIAL_WINDOW_HEIGHT);
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

  auto_open_files = argc - 1;
  if (argc > 1) auto_open_file_names = (char **)(argv + 1);

  dpy = XtDisplay(shell);

  XtVaGetValues(shell, XmNtitle, &app_title, NULL);  /* perhaps caller included -title arg */
  if (app_title) 
    ss->startup_title = mus_strdup(app_title); 
  else ss->startup_title = mus_strdup("snd");

  for (i = 1; i < argc; i++)
    if ((strcmp(argv[i], "-h") == 0) || 
	(strcmp(argv[i], "-horizontal") == 0) ||
	(strcmp(argv[i], "--horizontal") == 0))
      set_sound_style(SOUNDS_HORIZONTAL);
    else
      if ((strcmp(argv[i], "-v") == 0) || 
	  (strcmp(argv[i], "-vertical") == 0) ||
	  (strcmp(argv[i], "--vertical") == 0))
	set_sound_style(SOUNDS_VERTICAL);
      else
	if ((strcmp(argv[i], "-notebook") == 0) ||
	    (strcmp(argv[i], "--notebook") == 0))
	  {
	    set_sound_style(SOUNDS_IN_NOTEBOOK);
	    set_auto_resize(false);
	  }
	else
	  if ((strcmp(argv[i], "-separate") == 0) ||
	      (strcmp(argv[i], "--separate") == 0))
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
		      (strcmp(argv[i], "-batch") == 0) ||
		      (strcmp(argv[i], "--batch") == 0))
		    batch = true;
		  else
		    if (strcmp(argv[i], "--features") == 0) /* testing (compsnd) */
		      check_features_list(argv[i + 1]);

  ss->batch_mode = batch;
  if (batch) XtSetMappedWhenManaged(shell, 0);

  ss->zoom_slider_width = ZOOM_SLIDER_WIDTH;
  ss->position_slider_width = POSITION_SLIDER_WIDTH;
  ss->channel_sash_indent = CHANNEL_SASH_INDENT;
  ss->channel_sash_size = CHANNEL_SASH_SIZE;
  ss->sash_size = SASH_SIZE;
  ss->sash_indent = SASH_INDENT;
  ss->toggle_size = TOGGLE_SIZE;
  ss->click_time = (oclock_t)XtGetMultiClickTime(dpy);

#if HAVE_GL
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
      fprintf(stderr, "%s", "no RGB visual with desired depth\n"); /* not snd_error -- shell not ready yet */
    else
      {
	/* create an OpenGL rendering context */
	cx = glXCreateContext(dpy, vi, /* no display list sharing */ None, /* favor direct */ GL_TRUE);
	if (cx == NULL) 
	  fprintf(stderr, "%s", "could not create rendering context\n");
	else
	  {
	    /* create an X colormap since probably not using default visual */
	    cmap = XCreateColormap(dpy, RootWindow(dpy, vi->screen), vi->visual, AllocNone);
	    XtVaSetValues(shell, XtNvisual, vi->visual, XtNdepth, vi->depth, XtNcolormap, cmap, NULL);
	    ss->cx = cx;
	  }
	XFree(vi);
      }
  }
#endif

  XtAppSetWarningMsgHandler(app, muffle_warning);

  ss->mainapp = app;
  ss->mainshell = shell;
  ss->mdpy = dpy;
  ss->toolbar = NULL;

  ss->white =                   get_color(shell, WHITE_COLOR,             NULL, NULL, true);
  ss->black =                   get_color(shell, BLACK_COLOR,             NULL, NULL, false);
  ss->light_blue =              get_color(shell, LIGHT_BLUE_COLOR,        "blue", NULL, true);
  ss->lighter_blue =            get_color(shell, LIGHTER_BLUE_COLOR,      "blue", NULL, true);
  ss->red =                     get_color(shell, RED_COLOR,               NULL, NULL, false);
  ss->green =                   get_color(shell, GREEN_COLOR,             NULL, NULL, false);
  ss->blue =                    get_color(shell, "blue",                  NULL, NULL, false);
  ss->yellow =                  get_color(shell, YELLOW_COLOR,            NULL, NULL, true);
  ss->highlight_color =         get_color(shell, HIGHLIGHT_COLOR,         "gray90", NULL, true);
  ss->basic_color =             get_color(shell, BASIC_COLOR,             "gray80", "gray", true);
  ss->position_color =          get_color(shell, POSITION_COLOR,          "gray60", "blue", false);
  ss->zoom_color =              get_color(shell, ZOOM_COLOR,              "gray20", "gray", false);
  ss->cursor_color =            get_color(shell, CURSOR_COLOR,            NULL, NULL, false);
  ss->selection_color =         get_color(shell, SELECTION_COLOR,         "gray80", NULL, false);
  ss->mix_color =               get_color(shell, MIX_COLOR,               "red", NULL, false);
  ss->enved_waveform_color =    get_color(shell, ENVED_WAVEFORM_COLOR,    "red", NULL, false);
  ss->filter_control_waveform_color = get_color(shell, FILTER_CONTROL_WAVEFORM_COLOR, "blue", NULL, false);
  ss->listener_color =          get_color(shell, LISTENER_COLOR,          NULL, NULL, true);
  ss->listener_text_color =     get_color(shell, LISTENER_TEXT_COLOR,     NULL, NULL, false);
  ss->graph_color =             get_color(shell, GRAPH_COLOR,             NULL, NULL, true);
  ss->selected_graph_color =    get_color(shell, SELECTED_GRAPH_COLOR,    NULL, NULL, true);
  ss->data_color =              get_color(shell, DATA_COLOR,              NULL, NULL, false);
  ss->selected_data_color =     get_color(shell, SELECTED_DATA_COLOR,     NULL, NULL, false);
  ss->mark_color =              get_color(shell, MARK_COLOR,              "red", NULL, false);
  ss->sash_color =              get_color(shell, SASH_COLOR,              NULL, NULL, false);
  ss->text_focus_color =        get_color(shell, TEXT_FOCUS_COLOR,        NULL, NULL, true);
  ss->grid_color = get_in_between_color(ss->data_color, ss->graph_color);
  ss->selected_grid_color = get_in_between_color(ss->selected_data_color, ss->selected_graph_color);

#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->highlight_color_symbol,      Xen_wrap_pixel(ss->highlight_color));
  s7_symbol_set_value(s7, ss->basic_color_symbol,          Xen_wrap_pixel(ss->basic_color));
  s7_symbol_set_value(s7, ss->position_color_symbol,       Xen_wrap_pixel(ss->position_color));
  s7_symbol_set_value(s7, ss->zoom_color_symbol,           Xen_wrap_pixel(ss->zoom_color));
  s7_symbol_set_value(s7, ss->cursor_color_symbol,         Xen_wrap_pixel(ss->cursor_color));
  s7_symbol_set_value(s7, ss->selection_color_symbol,      Xen_wrap_pixel(ss->selection_color));
  s7_symbol_set_value(s7, ss->mix_color_symbol,            Xen_wrap_pixel(ss->mix_color));
  s7_symbol_set_value(s7, ss->enved_waveform_color_symbol, Xen_wrap_pixel(ss->enved_waveform_color));
  s7_symbol_set_value(s7, ss->filter_control_waveform_color_symbol, Xen_wrap_pixel(ss->filter_control_waveform_color));
  s7_symbol_set_value(s7, ss->listener_color_symbol,       Xen_wrap_pixel(ss->listener_color));
  s7_symbol_set_value(s7, ss->listener_text_color_symbol,  Xen_wrap_pixel(ss->listener_text_color));
  s7_symbol_set_value(s7, ss->graph_color_symbol,          Xen_wrap_pixel(ss->graph_color));
  s7_symbol_set_value(s7, ss->selected_graph_color_symbol, Xen_wrap_pixel(ss->selected_graph_color));
  s7_symbol_set_value(s7, ss->data_color_symbol,           Xen_wrap_pixel(ss->data_color));
  s7_symbol_set_value(s7, ss->selected_data_color_symbol,  Xen_wrap_pixel(ss->selected_data_color));
  s7_symbol_set_value(s7, ss->mark_color_symbol,           Xen_wrap_pixel(ss->mark_color));
  s7_symbol_set_value(s7, ss->sash_color_symbol,           Xen_wrap_pixel(ss->sash_color));
  s7_symbol_set_value(s7, ss->text_focus_color_symbol,     Xen_wrap_pixel(ss->text_focus_color));
#endif

  ss->axis_color_set = false;

  ss->orig_data_color = ss->data_color;
  ss->orig_selected_data_color = ss->selected_data_color;
  ss->orig_mark_color = ss->mark_color;
  ss->orig_mix_color = ss->mix_color;
  ss->orig_graph_color = ss->graph_color;
  ss->orig_selected_graph_color = ss->selected_graph_color;
  ss->orig_listener_color = ss->listener_color;
  ss->orig_listener_text_color = ss->listener_text_color;
  ss->orig_cursor_color = ss->cursor_color;
  ss->orig_basic_color = ss->basic_color;
  ss->orig_selection_color = ss->selection_color;
  ss->orig_zoom_color = ss->zoom_color;
  ss->orig_position_color = ss->position_color;
  ss->orig_highlight_color = ss->highlight_color;

  if ((!(set_peaks_font(DEFAULT_PEAKS_FONT))) &&
      (!(set_peaks_font(FALLBACK_FONT))))
    fprintf(stderr, "can't find peaks font %s", DEFAULT_PEAKS_FONT);

  if ((!(set_tiny_font(DEFAULT_TINY_FONT))) &&
      (!(set_tiny_font(FALLBACK_FONT))))
    fprintf(stderr, "can't find tiny font %s", DEFAULT_TINY_FONT);

  if ((!(set_bold_peaks_font(DEFAULT_BOLD_PEAKS_FONT))) &&
      (!(set_bold_peaks_font(FALLBACK_FONT))))
    fprintf(stderr, "can't find bold peaks font %s", DEFAULT_BOLD_PEAKS_FONT);

  if ((!(set_axis_label_font(DEFAULT_AXIS_LABEL_FONT))) &&
      (!(set_axis_label_font(FALLBACK_FONT))))
    fprintf(stderr, "can't find axis label font %s", DEFAULT_AXIS_LABEL_FONT);

  if ((!(set_axis_numbers_font(DEFAULT_AXIS_NUMBERS_FONT))) &&
      (!(set_axis_numbers_font(FALLBACK_FONT))))
    fprintf(stderr, "can't find axis numbers font %s", DEFAULT_AXIS_NUMBERS_FONT);

  set_listener_font(DEFAULT_LISTENER_FONT); /* we need some sort of font here! */

  ss->orig_axis_label_font = mus_strdup(axis_label_font(ss));
  ss->orig_axis_numbers_font = mus_strdup(axis_numbers_font(ss));
  ss->orig_peaks_font = mus_strdup(peaks_font(ss));
  ss->orig_bold_peaks_font = mus_strdup(bold_peaks_font(ss));
  ss->orig_listener_font = mus_strdup(listener_font(ss));
  ss->orig_tiny_font = mus_strdup(tiny_font(ss));

  XtVaSetValues(shell, XmNbackground, ss->basic_color, NULL);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
  n = attach_all_sides(args, n);
  XtSetArg(args[n], XmNallowResize, true); n++;
  ss->mainpane = XtCreateManagedWidget("mainpane", xmFormWidgetClass, shell, args, n);

  menu = add_menu();

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, menu); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNallowResize, true); n++;
  XtSetArg(args[n], XmNpaneMaximum, LOTSA_PIXELS); n++; 
  switch (sound_style(ss))
    {
    case SOUNDS_IN_SEPARATE_WINDOWS:
      ss->soundpane = XtCreateManagedWidget("soundpane", xmFormWidgetClass, ss->mainpane, args, n);
      break;

    case SOUNDS_HORIZONTAL:
      XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
      ss->soundpanebox = XtCreateManagedWidget("soundpane", xmPanedWindowWidgetClass, ss->mainpane, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNsashHeight, ss->sash_size); n++;
      XtSetArg(args[n], XmNsashWidth, ss->sash_size); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNsashIndent, ss->sash_indent); n++;
      ss->soundpane = XtCreateManagedWidget("soundpane", xmPanedWindowWidgetClass, ss->soundpanebox, args, n);
      break;

    case SOUNDS_VERTICAL:
      XtSetArg(args[n], XmNsashHeight, ss->sash_size); n++;
      XtSetArg(args[n], XmNsashWidth, ss->sash_size); n++;
      XtSetArg(args[n], XmNsashIndent, ss->sash_indent); n++;
      ss->soundpane = XtCreateManagedWidget("soundpane", xmPanedWindowWidgetClass, ss->mainpane, args, n);
      break;

    case SOUNDS_IN_NOTEBOOK:
      XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
      ss->soundpanebox = XtCreateManagedWidget("soundpane", xmPanedWindowWidgetClass, ss->mainpane, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNframeBackground, ss->zoom_color); n++;
      XtSetArg(args[n], XmNbindingType, XmNONE); n++;
      XtSetArg(args[n], XmNbackPagePlacement, XmTOP_RIGHT); n++;
      XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
      ss->soundpane = XtCreateWidget("nb", xmNotebookWidgetClass, ss->soundpanebox, args, n);

      {
	/* get rid of the useless spinbox */
	n = 0;
	XtSetArg(args[n], XmNnotebookChildType, XmPAGE_SCROLLER); n++;
	XtCreateWidget("scroller", xmScrollBarWidgetClass, ss->soundpane, NULL, 0);
      }
      XtManageChild(ss->soundpane);
      XtAddCallback(ss->soundpane, XmNpageChangedCallback, notebook_page_changed_callback, NULL);
      map_over_children(ss->soundpane, set_main_color_of_widget); /* appears to be a no-op */
      break;
    }

  SetupIcon(shell);
  XtRealizeWidget(shell);
  if (auto_resize(ss) != AUTO_RESIZE_DEFAULT) 
    XtVaSetValues(shell, XmNallowShellResize, auto_resize(ss), NULL);

  wn = XtWindow(shell);
  gv.background = ss->graph_color;
  gv.foreground = ss->data_color;
  ss->basic_gc = XCreateGC(dpy, wn, GCForeground | GCBackground, &gv);

  gv.background = ss->graph_color;
  gv.foreground = ss->data_color;
  ss->combined_basic_gc = XCreateGC(dpy, wn, GCForeground | GCBackground, &gv);

  gv.background = ss->graph_color;
  gv.foreground = ss->mix_color;
  ss->mix_gc = XCreateGC(dpy, wn, GCForeground | GCBackground, &gv);

  gv.function = GXxor;
  gv.background = ss->graph_color;
  gv.foreground = (Pixel)(XOR(ss->cursor_color, gv.background));
  ss->cursor_gc = XCreateGC(dpy, wn, GCForeground | GCFunction, &gv);

  gv.function = GXxor;
  gv.background = ss->graph_color;
  gv.foreground = (Pixel)(XOR(ss->selection_color, gv.background));
  ss->selection_gc = XCreateGC(dpy, wn, GCForeground | GCFunction, &gv);

  gv.function = GXxor;
  gv.background = ss->graph_color;
  gv.foreground = (Pixel)(XOR(ss->mark_color, gv.background));
  ss->mark_gc = XCreateGC(dpy, wn, GCForeground | GCFunction, &gv);

  gv.function = GXcopy;
  gv.background = ss->data_color;
  gv.foreground = ss->graph_color;
  ss->erase_gc = XCreateGC(dpy, wn, GCForeground | GCBackground | GCFunction, &gv);

  gv.background = ss->selected_graph_color;
  gv.foreground = ss->selected_data_color;
  ss->selected_basic_gc = XCreateGC(dpy, wn, GCForeground | GCBackground, &gv);

  gv.function = GXxor;
  gv.background = ss->selected_graph_color;
  gv.foreground = (Pixel)(XOR(ss->cursor_color, gv.background));
  ss->selected_cursor_gc = XCreateGC(dpy, wn, GCForeground | GCFunction, &gv);

  gv.function = GXxor;
  gv.background = ss->selected_graph_color;
  gv.foreground = (Pixel)(XOR(ss->selection_color, gv.background));
  ss->selected_selection_gc = XCreateGC(dpy, wn, GCForeground | GCFunction, &gv);

  gv.function = GXxor;
  gv.background = ss->selected_graph_color;
  gv.foreground = (Pixel)(XOR(ss->mark_color, gv.background));
  ss->selected_mark_gc = XCreateGC(dpy, wn, GCForeground | GCFunction, &gv);

  gv.function = GXcopy;
  gv.background = ss->selected_data_color;
  gv.foreground = ss->selected_graph_color;
  ss->selected_erase_gc = XCreateGC(dpy, wn, GCForeground | GCBackground | GCFunction, &gv);

  gv.function = GXcopy;
  gv.background = ss->basic_color;
  gv.foreground = ss->black;
  ss->fltenv_basic_gc = XCreateGC(dpy, wn, GCBackground | GCForeground | GCFunction, &gv);

  gv.function = GXcopy;
  gv.background = ss->basic_color;
  gv.foreground = ss->filter_control_waveform_color;
  ss->fltenv_data_gc = XCreateGC(dpy, wn, GCBackground | GCForeground | GCFunction, &gv);

  initialize_colormap(); /* X11 not ours */
  make_icons_transparent(BASIC_COLOR);

  if (with_toolbar(ss)) show_toolbar();
  startup_funcs();

#ifndef _MSC_VER
  if (setjmp(top_level_jump))
    {
      if (!(ss->jump_ok))
	snd_error_without_format("Caught top level error (will try to continue):\n");
      else ss->jump_ok = false;
    }
#endif

  if (ss->startup_errors)
    {
      post_it("Error in initialization", ss->startup_errors);
      free(ss->startup_errors);
      ss->startup_errors = NULL;
    }

  XtAppMainLoop(app);
}
