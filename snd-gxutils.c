#include "snd.h"

#if HAVE_X

#if USE_GTK
  #include <gdk/gdkx.h>
  #define MAIN_DISPLAY(a) GDK_DISPLAY()
#endif

#if HAVE_GUILE
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
  SCM_ASSERT(gh_string_p(cmd), cmd, SCM_ARG1, "send-netscape");
  ss = get_global_state();
  dpy = MAIN_DISPLAY(ss);
  command = (char *)CALLOC(256, sizeof(char));
  tmp = TO_NEW_C_STRING(cmd);
  if ((window = find_window(dpy, DefaultRootWindow(dpy), NS_VERSION, compare_window)))
    {
      sprintf(command, "openURL(file:%s)", tmp);
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
	  sprintf(command, "netscape file:%s", tmp);
	  if (execl("/bin/sh", "/bin/sh", "-c", command, NULL) == -1)
	    return(SCM_BOOL_F);
	}
    }
  FREE(command);
  if (tmp) free(tmp);
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
  SCM_ASSERT(gh_string_p(winat), name, SCM_ARG1, "change-property");
  SCM_ASSERT(gh_string_p(name), name, SCM_ARG2, "change-property");
  if (gh_string_p(command))
    c = TO_NEW_C_STRING(command);
  else
    {
      /* turn it into a string before passing it to change_property */
      c = gh_print_1(command, __FUNCTION__);
    }
  change_property(get_global_state(), 
		  TO_C_STRING(winat), 
		  TO_C_STRING(name), c);
  if (c) free(c);
  return(SCM_BOOL_F);
}

void g_init_gxutils(void)
{
  gh_new_procedure1_0("send-netscape", send_netscape);
  gh_new_procedure3_0("change-property", g_change_property);
}

#endif
#endif
