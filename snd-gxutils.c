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
  ASSERT_TYPE(STRING_P(cmd), cmd, SCM_ARGn, "send-netscape", "a string");
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
	    return(SCM_BOOL_F);
	}
    }
  FREE(command);
  return(SCM_BOOL_T);
}

static void change_property(snd_state *ss, char *winat, char *name, char *command)
{
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
  ASSERT_TYPE(STRING_P(winat), name, SCM_ARG1, S_change_property, "a string");
  ASSERT_TYPE(STRING_P(name), name, SCM_ARG2, S_change_property, "a string");
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
  return(SCM_BOOL_F);
}

void g_init_gxutils(SCM local_doc)
{
  DEFINE_PROC("send-netscape", send_netscape, 1, 0, 0, "");
  DEFINE_PROC(S_change_property, g_change_property, 3, 0, 0, "");
}

#endif

