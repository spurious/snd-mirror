#include "snd.h"

#if HAVE_X

#if USE_GTK
  #include <gdk/gdkx.h>
  #define MAIN_DISPLAY(a) GDK_DISPLAY()
#endif

#if (!USE_GTK) || (!MAC_OSX)

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
  if (((XGetWindowProperty(display, window, XInternAtom (display, id, 0), 0L, (long)BUFSIZ, 0,
			   XA_STRING, &type, &format, &nitems, &bytesafter, 
			   (unsigned char **)version)) == Success) && 
      (type != None))
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

bool send_netscape(const char *html_viewer, const char *url)
{
  Window window;
  Display *dpy;
  char *command;
  dpy = MAIN_DISPLAY(ss);
  command = (char *)CALLOC(snd_strlen(url) + snd_strlen(html_viewer) + 32, sizeof(char));
  window = find_window(dpy, DefaultRootWindow(dpy), NS_VERSION, compare_window);
  if (window)
    {
      sprintf(command, "openURL(file:%s)", url);
      XChangeProperty(dpy, 
		      window, 
		      XInternAtom(dpy, NS_COMMAND, 0), 
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
	  sprintf(command, "%s file:%s", html_viewer, url);
	  if (execl("/bin/sh", "/bin/sh", "-c", command, NULL) == -1)
	    {
	      FREE(command);
	      return(false);
	    }
	}
    }
  FREE(command);
  return(true);
}

static XEN g_send_netscape(XEN cmd)
{
  #define H_send_netscape "(" S_send_netscape " cmd): find an html-reader (or start it if necessary), and send it the \
string 'cmd'.  cmd should be a URL."
  XEN_ASSERT_TYPE(XEN_STRING_P(cmd), cmd, XEN_ONLY_ARG, S_send_netscape, "a string");
  return(C_TO_XEN_BOOLEAN(send_netscape(html_program(ss), XEN_TO_C_STRING(cmd))));
}

static void change_window_property(char *winat, char *name, char *command)
{
  Window window;
  Display *dpy;
  dpy = MAIN_DISPLAY(ss);
  if ((window = find_window(dpy, DefaultRootWindow(dpy), winat, compare_window)))
    {
      XChangeProperty(dpy, 
		      window, 
		      XInternAtom(dpy, name, 0), 
		      XA_STRING, 8, 
		      PropModeReplace, 
		      (unsigned char *)command, 
		      strlen(command) + 1);
      XFlush(dpy);
    }
}

static XEN g_change_window_property(XEN winat, XEN name, XEN command)
{
  #define H_change_window_property "(" S_change_window_property " version-name command-name command): look for the \
X atom 'version-name', and if it is found, set the property 'command-name' to the string 'command'.\n\
(change-property \"SND_VERSION\" \"SND_COMMAND\" \"(snd-print (+ 1 2))\"\n\
for example"

  char *c = NULL;
  /* winat arg needed as well as command arg because we need an atom that is guaranteed to have a value */
  /*   Supposedly WM_STATE is just such an atom */
  XEN_ASSERT_TYPE(XEN_STRING_P(winat), name, XEN_ARG_1, S_change_window_property, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_2, S_change_window_property, "a string");
  if (XEN_STRING_P(command))
    c = copy_string(XEN_TO_C_STRING(command));
  else c = g_print_1(command);
  change_window_property(XEN_TO_C_STRING(winat), XEN_TO_C_STRING(name), c);
  if (c) FREE(c);
  return(XEN_FALSE);
}

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_1(g_send_netscape_w, g_send_netscape)
XEN_NARGIFY_3(g_change_window_property_w, g_change_window_property)
#else
#define g_send_netscape_w g_send_netscape
#define g_change_window_property_w g_change_window_property
#endif

void g_init_gxutils(void)
{
  XEN_DEFINE_PROCEDURE(S_send_netscape, g_send_netscape_w,                   1, 0, 0, H_send_netscape);
  XEN_DEFINE_PROCEDURE(S_change_window_property, g_change_window_property_w, 3, 0, 0, H_change_window_property);
}

#else
/* gtk+osx = no X window property access, apparently */
bool send_netscape(const char *html_viewer, const char *url)
{
}
#endif

#endif

