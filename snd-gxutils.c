#include "snd.h"

#if HAVE_X

#if (!USE_GTK) && (!HAVE_OSX)

#include <X11/Xatom.h>

#define NS_VERSION      "_MOZILLA_VERSION"
#define NS_COMMAND      "_MOZILLA_COMMAND"


static Window compare_window(Display *display, Window window, const char *id)
{
  Atom type;
  int format;
  unsigned long nitems, bytesafter;
  unsigned char *version[1];
  Window found = (Window)None;
  if (((XGetWindowProperty(display, window, XInternAtom(display, id, 0), 0L, (long)BUFSIZ, False,
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
			  const char *name, 
			  Window (*compare_func)(Display *dpy, Window win, const char *id))
{
  Window rootwindow, window_parent;
  unsigned int i = 0, num_children = 0;
  Window *children = NULL;
  Window window = (compare_func)(display, starting_window, name);
  if (window != (Window)None) return(window);
  if ((XQueryTree(display, starting_window, &rootwindow, &window_parent, &children, &num_children)) == 0) 
    return((Window)None);
  while ((i < num_children) && (window == (Window)None))
    window = find_window(display, children[i++], name, compare_func);
  if (children) 
    XFree((char *)children);
  return(window);
}


bool send_mozilla(const char *html_viewer, const char *url)
{
  Window window;
  Display *dpy;
  char *command;
  int len;

  len = mus_strlen(url) + mus_strlen(html_viewer) + 32;
  dpy = MAIN_DISPLAY(ss);
  command = (char *)calloc(len, sizeof(char));
  window = find_window(dpy, DefaultRootWindow(dpy), NS_VERSION, compare_window);

  if (window)
    {
      snprintf(command, len, "openURL(file:%s)", url);
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
	  snprintf(command, len, "%s file:%s", html_viewer, url);
	  if (execl("/bin/sh", "/bin/sh", "-c", command, NULL) == -1)
	    {
	      free(command);
	      return(false);
	    }
	}
    }

  free(command);
  return(true);
}


static XEN g_send_mozilla(XEN cmd)
{
  #define H_send_mozilla "(" S_send_mozilla " cmd): find an html-reader (or start it if necessary), and send it the \
string 'cmd'.  cmd should be a URL."
  XEN_ASSERT_TYPE(XEN_STRING_P(cmd), cmd, XEN_ONLY_ARG, S_send_mozilla, "a string");
  return(C_TO_XEN_BOOLEAN(send_mozilla(html_program(ss), XEN_TO_C_STRING(cmd))));
}


#ifdef XEN_ARGIFY_1
XEN_NARGIFY_1(g_send_mozilla_w, g_send_mozilla)
#else
#define g_send_mozilla_w g_send_mozilla
#endif

void g_init_gxutils(void)
{
  XEN_DEFINE_PROCEDURE(S_send_mozilla, g_send_mozilla_w, 1, 0, 0, H_send_mozilla);
}

#else
/* gtk+osx = no X window property access, apparently */
bool send_mozilla(const char *html_viewer, const char *url) {return(false);}
void g_init_gxutils(void) {}
#endif

#else
/* not X */
bool send_mozilla(const char *html_viewer, const char *url) {return(false);}
void g_init_gxutils(void) {}
#endif

