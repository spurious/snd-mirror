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

static XEN send_netscape(XEN cmd)
{
  Window window;
  snd_state *ss;
  Display *dpy;
  char *command, *tmp = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_P(cmd), cmd, XEN_ONLY_ARG, "send-netscape", "a string");
  ss = get_global_state();
  dpy = MAIN_DISPLAY(ss);
  command = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  tmp = XEN_TO_C_STRING(cmd);
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
	    return(XEN_FALSE);
	}
    }
  FREE(command);
  return(XEN_TRUE);
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

static XEN g_change_property(XEN winat, XEN name, XEN command)
{
  char *c;
  /* winat arg needed as well as command arg because we need an atom that is guaranteed to have a value */
  XEN_ASSERT_TYPE(XEN_STRING_P(winat), name, XEN_ARG_1, S_change_property, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_2, S_change_property, "a string");
  if (XEN_STRING_P(command))
    c = XEN_TO_NEW_C_STRING(command);
  else
    {
      /* turn it into a string before passing it to change_property */
      c = g_print_1(command, S_change_property);
    }
  change_property(get_global_state(), 
		  XEN_TO_C_STRING(winat), 
		  XEN_TO_C_STRING(name), c);
  if (c) free(c);
  return(XEN_FALSE);
}

#if DEBUGGING
/* these are intended for auto-testing the user-interface via scheme-generated X events */
static XEN g_key_event(XEN win, XEN key, XEN state)
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
  window = (Window)XEN_TO_C_ULONG(win);
  ev.window = window;
  ev.display = dpy;
  ev.root = RootWindow(dpy, DefaultScreen(dpy));
  ev.x = 0;
  ev.y = 0;
  ev.x_root = 0;
  ev.y_root = 0;
  k = (KeySym)XEN_TO_C_INT(key);
  key_state = XEN_TO_C_INT(state);
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
  return(C_TO_XEN_INT(err));
}

static XEN g_click_event(XEN win, XEN button, XEN state, XEN x, XEN y)
{
  Window window;
  Display *dpy;
  snd_state *ss;
  XButtonEvent ev;
  Status err;
  int b;
  ss = get_global_state();
  dpy = MAIN_DISPLAY(ss);
  window = (Window)XEN_TO_C_ULONG(win);
  ev.type = ButtonPress;
  ev.window = window;
  ev.display = dpy;
  ev.root = RootWindow(dpy, DefaultScreen(dpy));
  ev.x = XEN_TO_C_INT(x);
  ev.y = XEN_TO_C_INT(y);
  ev.x_root = 0;
  ev.y_root = 0;
  ev.state = XEN_TO_C_INT(state);
  b = XEN_TO_C_INT(button);
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
  return(C_TO_XEN_INT(err));
}

static XEN g_drag_event(XEN win, XEN button, XEN state, XEN x0, XEN y0, XEN x1, XEN y1)
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
  window = (Window)XEN_TO_C_ULONG(win);
  ev.type = ButtonPress;
  ev.window = window;
  ev.display = dpy;
  ev.root = RootWindow(dpy, DefaultScreen(dpy));
  ev.x = XEN_TO_C_INT(x0);
  ev.y = XEN_TO_C_INT(y0);
  ev.x_root = 0;
  ev.y_root = 0;
  ev.state = XEN_TO_C_INT(state);
  b = XEN_TO_C_INT(button);
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
      evm.state = XEN_TO_C_INT(state);
      evm.type = MotionNotify;
      evm.x_root = ev.x;
      evm.y_root = ev.y;
      evm.x = XEN_TO_C_INT(x1);
      evm.y = XEN_TO_C_INT(y1);
      evm.same_screen = True;
      evm.time = CurrentTime + 300;
      evm.is_hint = NotifyNormal;
      err = XSendEvent(dpy, window, False, ButtonMotionMask, (XEvent *)(&evm));
      ev.type = ButtonRelease;
      ev.time = CurrentTime + 500;
      ev.x = XEN_TO_C_INT(x1);
      ev.y = XEN_TO_C_INT(y1);
      err = XSendEvent(dpy, window, False, ButtonReleaseMask, (XEvent *)(&ev));
    }
  return(C_TO_XEN_INT(err));
}

static XEN g_expose_event(XEN win, XEN x, XEN y, XEN width, XEN height)
{
  Window window;
  Display *dpy;
  snd_state *ss;
  XExposeEvent ev;
  ss = get_global_state();
  dpy = MAIN_DISPLAY(ss);
  window = (Window)XEN_TO_C_ULONG(win);
  ev.type = Expose;
  ev.window = window;
  ev.display = dpy;
  ev.x = XEN_TO_C_INT(x);
  ev.y = XEN_TO_C_INT(y);
  ev.width = XEN_TO_C_INT(width);
  ev.height = XEN_TO_C_INT(height);
  ev.count = 0;
  return(C_TO_XEN_INT(XSendEvent(dpy, window, False, ExposureMask, (XEvent *)(&ev))));
}

static XEN g_resize_event(XEN win, XEN width, XEN height)
{
  Window window;
  Display *dpy;
  snd_state *ss;
  XResizeRequestEvent ev;
  ss = get_global_state();
  dpy = MAIN_DISPLAY(ss);
  window = (Window)XEN_TO_C_ULONG(win);
  ev.type = ResizeRequest;
  ev.window = window;
  ev.display = dpy;
  ev.width = XEN_TO_C_INT(width);
  ev.height = XEN_TO_C_INT(height);
  return(C_TO_XEN_INT(XSendEvent(dpy, window, False, ResizeRedirectMask, (XEvent *)(&ev))));
}

static XEN g_force_event(void)
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
  return(C_TO_XEN_INT(evs));
}

static XEN g_widget_window(XEN wid)
{
#if USE_MOTIF
  return(C_TO_XEN_ULONG(XtWindow((Widget)(XEN_UNWRAP_C_POINTER(wid)))));
#endif
#if USE_GTK
  GtkWidget *w;
  w = (GtkWidget *)(XEN_UNWRAP_C_POINTER(wid));
  while (w)
    {
      if (GTK_IS_WINDOW(w))
	return(C_TO_XEN_ULONG((unsigned long)(GTK_WINDOW(w))));
      else
	if (GTK_IS_WINDOW(w->window))
	  return(C_TO_XEN_ULONG((unsigned long)(GTK_WINDOW(w->window))));
	else w = w->parent;
    }
  /* this can't be used directly: Gdk-XEN_ERROR **: BadWindow (invalid Window parameter) */
#endif
  return(XEN_FALSE);
}

static XEN g_x_synchronize(XEN on)
{
  snd_state *ss;
  ss = get_global_state();
  XSynchronize(MAIN_DISPLAY(ss),
	       XEN_TO_C_BOOLEAN(on));
  return(on);
}

static XEN g_click_button(XEN button)
{
#if USE_MOTIF
  Widget w;
  w = (Widget)(XEN_UNWRAP_C_POINTER(button));
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
#if USE_GTK
  gtk_button_clicked(GTK_BUTTON((GtkWidget *)(XEN_UNWRAP_C_POINTER(button))));
#endif
  return(button);
}

static XEN g_resize_pane(XEN wid, XEN height)
{
#if USE_MOTIF
  Widget w;
  int hgt;
  w = (Widget)(XEN_UNWRAP_C_POINTER(wid));
  hgt = XEN_TO_C_INT(height);
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
#if USE_GTK
  GtkWidget *w;
  w = (GtkWidget *)(XEN_UNWRAP_C_POINTER(wid));
  gtk_paned_set_position(GTK_PANED(w), XEN_TO_C_INT(height));
#endif
  return(height);
}

static XEN g_select_item(XEN wid, XEN pos)
{
#if USE_MOTIF
  Widget w;
  int id;
  w = (Widget)(XEN_UNWRAP_C_POINTER(wid));
  if (!(XmIsList(w)))
    snd_error("not a list");
  else
    {
      id = XEN_TO_C_INT(pos);
      XmListSelectPos(w, id + 1, TRUE);
    }
#endif
#if USE_GTK
  GtkWidget *w;
  int id;
  w = (GtkWidget *)XEN_UNWRAP_C_POINTER(wid);
  id = XEN_TO_C_INT(pos);
  gtk_clist_select_row(GTK_CLIST(w), id, 0); /* does this trigger the callback? */
#endif
  return(pos);
}

#endif


#ifdef XEN_ARGIFY_1
XEN_NARGIFY_1(send_netscape_w, send_netscape)
XEN_NARGIFY_3(g_change_property_w, g_change_property)
#if DEBUGGING
XEN_NARGIFY_3(g_key_event_w, g_key_event)
XEN_NARGIFY_5(g_click_event_w, g_click_event)
XEN_NARGIFY_5(g_expose_event_w, g_expose_event)
XEN_NARGIFY_3(g_resize_event_w, g_resize_event)
XEN_NARGIFY_7(g_drag_event_w, g_drag_event)
XEN_NARGIFY_1(g_widget_window_w, g_widget_window)
XEN_NARGIFY_0(g_force_event_w, g_force_event)
XEN_NARGIFY_1(g_x_synchronize_w, g_x_synchronize)
XEN_NARGIFY_1(g_click_button_w, g_click_button)
XEN_NARGIFY_2(g_resize_pane_w, g_resize_pane)
XEN_NARGIFY_2(g_select_item_w, g_select_item)
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

void g_init_gxutils(void)
{
  XEN_DEFINE_PROCEDURE("send-netscape", send_netscape_w, 1, 0, 0, "");
  XEN_DEFINE_PROCEDURE(S_change_property, g_change_property_w, 3, 0, 0, H_change_property);
#if DEBUGGING
  XEN_DEFINE_PROCEDURE("key-event", g_key_event_w, 3, 0, 0, "");
  XEN_DEFINE_PROCEDURE("click-event", g_click_event_w, 5, 0, 0, "");
  XEN_DEFINE_PROCEDURE("expose-event", g_expose_event_w, 5, 0, 0, "");
  XEN_DEFINE_PROCEDURE("resize-event", g_resize_event_w, 3, 0, 0, "");
  XEN_DEFINE_PROCEDURE("drag-event", g_drag_event_w, 7, 0, 0, "");
  XEN_DEFINE_PROCEDURE("widget-window", g_widget_window_w, 1, 0, 0, "");
  XEN_DEFINE_PROCEDURE("force-event", g_force_event_w, 0, 0, 0, "");
  XEN_DEFINE_PROCEDURE("x-synchronize", g_x_synchronize_w, 1, 0, 0, "");
  XEN_DEFINE_PROCEDURE("click-button", g_click_button_w, 1, 0, 0, "");
  XEN_DEFINE_PROCEDURE("resize-pane", g_resize_pane_w, 2, 0, 0, "");
  XEN_DEFINE_PROCEDURE("select-item", g_select_item_w, 2, 0, 0, "");
#if USE_MOTIF
  /* gtk version needs to sort out windows/click-events etc */
  XEN_YES_WE_HAVE("snd-events");
#endif
#endif
}

#endif

