/* sndctrl shows how to send a running Snd program a command through X */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#define SND_VERSION "SND_VERSION"
#define SND_COMMAND "SND_COMMAND"

static Window compare_window(Display *display, Window window, char *id)
{
  Atom type;
  int format;
  unsigned long nitems, bytesafter;
  unsigned char *version[1];
  Window found=(Window)None;
  if (((XGetWindowProperty(display, window, XInternAtom (display, id, False), 0L, (long)BUFSIZ, False,
			   XA_STRING, &type, &format, &nitems, &bytesafter, (unsigned char **)version)) == Success) && (type != None))
    {
      found = window;
      if (version[0]) XFree((char *)(version[0]));
    }
  return(found);
}

static Window find_window(Display *display, Window starting_window, char *name, Window (*compare_func)())
{
  Window rootwindow, window_parent;
  int i = 0;
  unsigned int num_children = 0;
  Window *children = NULL;
  Window window = (compare_func)(display, starting_window, name);
  if (window != (Window)None)return (window);
  if ((XQueryTree(display, starting_window, &rootwindow, &window_parent, &children, &num_children)) == 0) return ((Window)None);
  while ((i < num_children) && (window == (Window)None))
    window = find_window(display, children[i++], name, compare_func);
  if (children) XFree((char *)children);
  return(window);
}

static void send_snd(Display *dpy, char *command)
{
  Window window;
  if ((window = find_window(dpy, DefaultRootWindow(dpy), SND_VERSION, compare_window)))
    {
      XChangeProperty(dpy, window, XInternAtom(dpy, SND_COMMAND, False), XA_STRING, 8, PropModeReplace, (unsigned char *)command, strlen(command)+1);
      XFlush(dpy);
    }
}

static void send_snd_char(Display *dpy, Window window, int keycode, int state)
{
  /* this sends Snd a key event */
  XKeyEvent event;
  int status;
  event.type = KeyPress;
  event.display = dpy;
  event.window = window;
  event.root = RootWindow(dpy, DefaultScreen(dpy));
  event.keycode = keycode;
  event.state = state;
  event.time = CurrentTime;
  event.same_screen = True;
  event.x = 0;
  event.y = 0;
  event.x_root = 0;
  event.y_root = 0;
  event.subwindow = (Window)None;
  status =  XSendEvent(dpy, window, False, KeyPressMask, (XEvent *)(&event));
  if (status != 0)
    {
      event.type = KeyRelease;
      event.time = CurrentTime;
      XSendEvent(dpy, window, True, KeyReleaseMask, (XEvent *)(&event));
    }
}

int main(int argc, char **argv)
{
    Display *dpy;
    dpy = XOpenDisplay(NULL);
    send_snd(dpy, "(snd-print \"hiho\")");
}

/* cc sndctrl.c -g -o sndctrl -L/usr/X11R6/lib -lX11 */

#if 0
/*
  Window window;
  dpy = XOpenDisplay(NULL);
  if ((window = find_window(dpy, DefaultRootWindow(dpy), SND_VERSION, compare_window)))
    {
      send_snd_char(dpy, window, XKeysymToKeycode(dpy, XK_greater), ShiftMask | Mod1Mask);
      XFlush(dpy);
    }
*/
#endif
