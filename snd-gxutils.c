#include "snd.h"

#if HAVE_X

#if USE_GTK
  #include <gdk/gdkx.h>
  #define MAIN_DISPLAY(a) GDK_DISPLAY()
#endif

#include <X11/Xatom.h>

/* this code based on XmHTML's netscape.c */

#define NS_VERSION      "_MOZILLA_VERSION"
#define NS_COMMAND      "_MOZILLA_COMMAND"

static Window compare_window(Display *display, Window window, char *id)
{
  Atom type;
  int format;
  unsigned long nitems, bytesafter;
  unsigned char *version[1];
  Window found = (Window)None;
  if (((XGetWindowProperty(display, window, XInternAtom (display, id, False), 0L, (long)BUFSIZ, False,
			   XA_STRING, &type, &format, &nitems, &bytesafter, 
			   (unsigned char **)version)) == Success) && (type != None))
    {
      found = window;
      if (version[0]) 
	XFree((char *)(version[0]));
    }
  return(found);
}

static Window find_window(Display *display, 
			  Window starting_window, 
			  char *name, 
			  Window (*compare_func)(Display *dpy, Window win, char *id))
{
  Window rootwindow, window_parent;
  unsigned int i = 0, num_children = 0;
  Window *children = NULL;
  Window window = (compare_func)(display, starting_window, name);
  if (window != (Window)None) return (window);
  if ((XQueryTree(display, starting_window, &rootwindow, &window_parent, &children, &num_children)) == 0) 
    return ((Window)None);
  while ((i < num_children) && (window == (Window)None))
    window = find_window(display, children[i++], name, compare_func);
  if (children) 
    XFree((char *)children);
  return(window);
}

static SCM send_netscape(SCM cmd)
{
  Window window;
  snd_state *ss;
  Display *dpy;
  char *command, *tmp = NULL;
  ASSERT_TYPE(STRING_P(cmd), cmd, ARGn, "send-netscape", "a string");
  ss = get_global_state();
  dpy = MAIN_DISPLAY(ss);
  command = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  tmp = TO_C_STRING(cmd);
  if ((window = find_window(dpy, DefaultRootWindow(dpy), NS_VERSION, compare_window)))
    {
      mus_snprintf(command, PRINT_BUFFER_SIZE, "openURL(file:%s)", tmp);
      XChangeProperty(dpy, 
		      window, 
		      XInternAtom(dpy, NS_COMMAND, False), 
		      XA_STRING, 8, 
		      PropModeReplace, 
		      (unsigned char *)command, 
		      strlen(command) + 1);
      XFlush(dpy);
    }
  else
    {
      if (!(fork()))
        {
	  mus_snprintf(command, PRINT_BUFFER_SIZE, "netscape file:%s", tmp);
	  if (execl("/bin/sh", "/bin/sh", "-c", command, NULL) == -1)
	    return(FALSE_VALUE);
	}
    }
  FREE(command);
  return(TRUE_VALUE);
}

static void change_property(snd_state *ss, char *winat, char *name, char *command)
{
  #define H_change_property "(" S_change_property " version-name command-name command) looks for the \
X atom 'version-name', and if it is found, sets the property 'command-name' to the string 'command'.\n\
(change-property \"SND_VERSION\" \"SND_COMMAND\" \"(snd-print (+ 1 2))\"\n\
for example"

  Window window;
  Display *dpy;
  dpy = MAIN_DISPLAY(ss);
  if ((window = find_window(dpy, DefaultRootWindow(dpy), winat, compare_window)))
    {
      XChangeProperty(dpy, 
		      window, 
		      XInternAtom(dpy, name, False), 
		      XA_STRING, 8, 
		      PropModeReplace, 
		      (unsigned char *)command, 
		      strlen(command) + 1);
      XFlush(dpy);
    }
}

static SCM g_change_property(SCM winat, SCM name, SCM command)
{
  char *c;
  /* winat arg needed as well as command arg because we need an atom that is guaranteed to have a value */
  ASSERT_TYPE(STRING_P(winat), name, ARG1, S_change_property, "a string");
  ASSERT_TYPE(STRING_P(name), name, ARG2, S_change_property, "a string");
  if (STRING_P(command))
    c = TO_NEW_C_STRING(command);
  else
    {
      /* turn it into a string before passing it to change_property */
      c = g_print_1(command, S_change_property);
    }
  change_property(get_global_state(), 
		  TO_C_STRING(winat), 
		  TO_C_STRING(name), c);
  if (c) free(c);
  return(FALSE_VALUE);
}

#if DEBUGGING
/* these are intended for auto-testing the user-interface via scheme-generated X events */
static SCM g_key_event(SCM win, SCM key, SCM state)
{
  Window window;
  Display *dpy;
  snd_state *ss;
  XKeyEvent ev;
  KeySym k;
  int key_state = 0;
  Status err;
  ss = get_global_state();
  dpy = MAIN_DISPLAY(ss);
  ev.type = KeyPress;
  window = (Window)TO_C_UNSIGNED_LONG(win);
  ev.window = window;
  ev.display = dpy;
  ev.root = RootWindow(dpy, DefaultScreen(dpy));
  ev.x = 0;
  ev.y = 0;
  ev.x_root = 0;
  ev.y_root = 0;
  k = (KeySym)TO_C_INT(key);
  key_state = TO_C_INT(state);
  if ((k >= snd_K_A) && (k <= snd_K_Z)) 
    key_state |= ShiftMask;
  ev.keycode = XKeysymToKeycode(dpy, k);
  ev.state = key_state;
  ev.time = CurrentTime;
  ev.same_screen = True;
  ev.subwindow = (Window)None;
  err = XSendEvent(dpy, window, False, KeyPressMask, (XEvent *)(&ev));
  if (err != 0)
    {
      ev.time = CurrentTime;
      ev.type = KeyRelease;
      err = XSendEvent(dpy, window, False, KeyReleaseMask, (XEvent *)(&ev));
    }
  return(TO_SCM_INT(err));
}

static SCM g_click_event(SCM win, SCM button, SCM state, SCM x, SCM y)
{
  Window window;
  Display *dpy;
  snd_state *ss;
  XButtonEvent ev;
  Status err;
  int b;
  ss = get_global_state();
  dpy = MAIN_DISPLAY(ss);
  window = (Window)TO_C_UNSIGNED_LONG(win);
  ev.type = ButtonPress;
  ev.window = window;
  ev.display = dpy;
  ev.root = RootWindow(dpy, DefaultScreen(dpy));
  ev.x = TO_C_INT(x);
  ev.y = TO_C_INT(y);
  ev.x_root = 0;
  ev.y_root = 0;
  ev.state = TO_C_INT(state);
  b = TO_C_INT(button);
  switch (b)
    {
    case 1: ev.button = Button1; break;
    case 2: ev.button = Button2; break;
    case 3: ev.button = Button3; break;
    default: ev.button = Button1; break;
    }
  ev.time = CurrentTime;
  ev.same_screen = True;
  ev.subwindow = (Window)None;
  err = XSendEvent(dpy, window, False, ButtonPressMask, (XEvent *)(&ev));
  if (err != 0)
    {
      ev.time = CurrentTime;
      ev.type = ButtonRelease;
      err = XSendEvent(dpy, window, False, ButtonReleaseMask, (XEvent *)(&ev));
    }
  return(TO_SCM_INT(err));
}

static SCM g_drag_event(SCM win, SCM button, SCM state, SCM x0, SCM y0, SCM x1, SCM y1)
{
  /* aimed at Snd's selection creation in the graph widget */
  Window window;
  Display *dpy;
  snd_state *ss;
  XButtonEvent ev;
  XMotionEvent evm;
  Status err;
  int b;
  ss = get_global_state();
  dpy = MAIN_DISPLAY(ss);
  window = (Window)TO_C_UNSIGNED_LONG(win);
  ev.type = ButtonPress;
  ev.window = window;
  ev.display = dpy;
  ev.root = RootWindow(dpy, DefaultScreen(dpy));
  ev.x = TO_C_INT(x0);
  ev.y = TO_C_INT(y0);
  ev.x_root = 0;
  ev.y_root = 0;
  ev.state = TO_C_INT(state);
  b = TO_C_INT(button);
  switch (b)
    {
    case 1: ev.button = Button1; break;
    case 2: ev.button = Button2; break;
    case 3: ev.button = Button3; break;
    default: ev.button = Button1; break;
    }
  ev.time = CurrentTime;
  ev.same_screen = True;
  ev.subwindow = (Window)None;
  err = XSendEvent(dpy, window, False, ButtonPressMask, (XEvent *)(&ev));
  if (err != 0)
    {
      evm.window = window;
      evm.display = dpy;
      evm.root = RootWindow(dpy, DefaultScreen(dpy));
      evm.x_root = 0;
      evm.y_root = 0;
      evm.state = TO_C_INT(state);
      evm.type = MotionNotify;
      evm.x_root = ev.x;
      evm.y_root = ev.y;
      evm.x = TO_C_INT(x1);
      evm.y = TO_C_INT(y1);
      evm.same_screen = True;
      evm.time = CurrentTime + 300;
      evm.is_hint = NotifyNormal;
      err = XSendEvent(dpy, window, False, ButtonMotionMask, (XEvent *)(&evm));
      ev.type = ButtonRelease;
      ev.time = CurrentTime + 500;
      ev.x = TO_C_INT(x1);
      ev.y = TO_C_INT(y1);
      err = XSendEvent(dpy, window, False, ButtonReleaseMask, (XEvent *)(&ev));
    }
  return(TO_SCM_INT(err));
}

static SCM g_expose_event(SCM win, SCM x, SCM y, SCM width, SCM height)
{
  Window window;
  Display *dpy;
  snd_state *ss;
  XExposeEvent ev;
  ss = get_global_state();
  dpy = MAIN_DISPLAY(ss);
  window = (Window)TO_C_UNSIGNED_LONG(win);
  ev.type = Expose;
  ev.window = window;
  ev.display = dpy;
  ev.x = TO_C_INT(x);
  ev.y = TO_C_INT(y);
  ev.width = TO_C_INT(width);
  ev.height = TO_C_INT(height);
  ev.count = 0;
  return(TO_SCM_INT(XSendEvent(dpy, window, False, ExposureMask, (XEvent *)(&ev))));
}

static SCM g_resize_event(SCM win, SCM width, SCM height)
{
  Window window;
  Display *dpy;
  snd_state *ss;
  XResizeRequestEvent ev;
  ss = get_global_state();
  dpy = MAIN_DISPLAY(ss);
  window = (Window)TO_C_UNSIGNED_LONG(win);
  ev.type = ResizeRequest;
  ev.window = window;
  ev.display = dpy;
  ev.width = TO_C_INT(width);
  ev.height = TO_C_INT(height);
  return(TO_SCM_INT(XSendEvent(dpy, window, False, ResizeRedirectMask, (XEvent *)(&ev))));
}

static SCM g_force_event(void)
{
  int evs = 0;
#if USE_MOTIF
  XEvent event;
  XtInputMask msk = 0;
  XtAppContext app;
  snd_state *ss;
  ss = get_global_state();
  app = MAIN_APP(ss);
  while (1)
    {
      msk = XtAppPending(app);
      if (msk & (XtIMXEvent | XtIMAlternateInput))
	{
	  XtAppNextEvent(app, &event);
	  XtDispatchEvent(&event);
	  evs++;
	}
      else break;
    }
#endif
#if USE_GTK
  while (gtk_events_pending()) 
    {
      gtk_main_iteration();
      evs++;
    }
#endif
  return(TO_SCM_INT(evs));
}

static SCM g_widget_window(SCM wid)
{
#if USE_MOTIF
  return(TO_SCM_UNSIGNED_LONG(XtWindow((Widget)(SND_UNWRAP(wid)))));
#endif
#if USE_GTK
  return(TO_SCM_UNSIGNED_LONG((unsigned long)(((GtkWidget *)(SND_UNWRAP(wid)))->window)));
  /* this can't be used directly: Gdk-ERROR **: BadWindow (invalid Window parameter) */
#endif
  return(FALSE_VALUE);
}

static SCM g_x_synchronize(SCM on)
{
  snd_state *ss;
  ss = get_global_state();
  XSynchronize(MAIN_DISPLAY(ss),
	       TO_C_BOOLEAN(on));
  return(on);
}

static SCM g_click_button(SCM button)
{
#if USE_MOTIF
  Widget w;
  w = (Widget)(SND_UNWRAP(button));
  if ((XmIsPushButton(w)) || (XmIsPushButtonGadget(w)))
    { 
      if (XtIsSensitive(w))
	XtCallCallbacks(w, XmNactivateCallback, (void *)get_global_state());
    }
  else
    {
      if ((XmIsToggleButton(w)) || (XmIsToggleButtonGadget(w)))
	{
	  if (XtIsSensitive(w))
	    XtCallCallbacks(w, XmNvalueChangedCallback, (void *)get_global_state());
	}
      else fprintf(stderr,"bad type");
    }
#endif
  /* TODO: gtk click_button */
  /*       send click to button coordinates? */
  return(button);
}

static SCM g_resize_pane(SCM wid, SCM height)
{
#if USE_MOTIF
  Widget w;
  int hgt;
  w = (Widget)(SND_UNWRAP(wid));
  hgt = TO_C_INT(height);
  XtUnmanageChild(w);
  XtVaSetValues(w,
		XmNpaneMinimum, (hgt >= 5) ? (hgt - 5) : 0,
		XmNpaneMaximum, hgt + 5,
		NULL);
  XtManageChild(w);
  XtVaSetValues(w,
		XmNpaneMinimum, 5,
		XmNpaneMaximum, LOTSA_PIXELS,
		NULL);
#endif
  /* TODO: gtk resize pane */
  return(height);
}

static SCM g_select_item(SCM wid, SCM pos)
{
#if USE_MOTIF
  Widget w;
  int id;
  w = (Widget)(SND_UNWRAP(wid));
  if (!(XmIsList(w)))
    snd_error("not a list");
  else
    {
      id = TO_C_INT(pos);
      XmListSelectPos(w, id + 1, TRUE);
    }
#endif
#if USE_GTK
  GtkWidget *w;
  int id;
  w = (GtkWidget *)SND_UNWRAP(wid);
  id = TO_C_INT(pos);
  gtk_clist_select_row(GTK_CLIST(w), id, 0); /* does this trigger the callback? */
#endif
  return(pos);
}

#endif


#ifdef ARGIFY_1
NARGIFY_1(send_netscape_w, send_netscape)
NARGIFY_3(g_change_property_w, g_change_property)
#if DEBUGGING
NARGIFY_3(g_key_event_w, g_key_event)
NARGIFY_5(g_click_event_w, g_click_event)
NARGIFY_5(g_expose_event_w, g_expose_event)
NARGIFY_3(g_resize_event_w, g_resize_event)
NARGIFY_7(g_drag_event_w, g_drag_event)
NARGIFY_1(g_widget_window_w, g_widget_window)
NARGIFY_0(g_force_event_w, g_force_event)
NARGIFY_1(g_x_synchronize_w, g_x_synchronize)
NARGIFY_1(g_click_button_w, g_click_button)
NARGIFY_2(g_resize_pane_w, g_resize_pane)
NARGIFY_2(g_select_item_w, g_select_item)
#endif
#else
#define send_netscape_w send_netscape
#define g_change_property_w g_change_property
#if DEBUGGING
#define g_key_event_w g_key_event
#define g_click_event_w g_click_event
#define g_expose_event_w g_expose_event
#define g_resize_event_w g_resize_event
#define g_drag_event_w g_drag_event
#define g_widget_window_w g_widget_window
#define g_force_event_w g_force_event
#define g_x_synchronize_w g_x_synchronize
#define g_click_button_w g_click_button
#define g_resize_pane_w g_resize_pane
#define g_select_item_w g_select_item
#endif
#endif

void g_init_gxutils(SCM local_doc)
{
  DEFINE_PROC("send-netscape", send_netscape_w, 1, 0, 0, "");
  DEFINE_PROC(S_change_property, g_change_property_w, 3, 0, 0, H_change_property);
#if DEBUGGING
  DEFINE_PROC("key-event", g_key_event_w, 3, 0, 0, "");
  DEFINE_PROC("click-event", g_click_event_w, 5, 0, 0, "");
  DEFINE_PROC("expose-event", g_expose_event_w, 5, 0, 0, "");
  DEFINE_PROC("resize-event", g_resize_event_w, 3, 0, 0, "");
  DEFINE_PROC("drag-event", g_drag_event_w, 7, 0, 0, "");
  DEFINE_PROC("widget-window", g_widget_window_w, 1, 0, 0, "");
  DEFINE_PROC("force-event", g_force_event_w, 0, 0, 0, "");
  DEFINE_PROC("x-synchronize", g_x_synchronize_w, 1, 0, 0, "");
  DEFINE_PROC("click-button", g_click_button_w, 1, 0, 0, "");
  DEFINE_PROC("resize-pane", g_resize_pane_w, 2, 0, 0, "");
  DEFINE_PROC("select-item", g_select_item_w, 2, 0, 0, "");
#if USE_MOTIF
  /* gtk version needs to sort out windows/click-events etc */
  YES_WE_HAVE("snd-events");
#endif
#endif
}

#endif

