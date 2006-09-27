#include "snd.h"

#if HAVE_X

#if USE_GTK
  #include <gdk/gdkx.h>
  #define MAIN_DISPLAY(a) GDK_DISPLAY()
#endif

#if (!USE_GTK) || (!MUS_MAC_OSX)

#include <X11/Xatom.h>

#define NS_VERSION      "_MOZILLA_VERSION"
#define NS_COMMAND      "_MOZILLA_COMMAND"

static Window compare_window(Display *display, Window window, char *id)
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
			  char *name, 
			  Window (*compare_func)(Display *dpy, Window win, char *id))
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

static XEN g_send_mozilla(XEN cmd)
{
  #define H_send_mozilla "(" S_send_mozilla " cmd): find an html-reader (or start it if necessary), and send it the \
string 'cmd'.  cmd should be a URL."
  XEN_ASSERT_TYPE(XEN_STRING_P(cmd), cmd, XEN_ONLY_ARG, S_send_mozilla, "a string");
  return(C_TO_XEN_BOOLEAN(send_mozilla(html_program(ss), XEN_TO_C_STRING(cmd))));
}

static void change_window_property(char *winat, char *name, char *command)
{
  Window window;
  Display *dpy;
  dpy = MAIN_DISPLAY(ss);
  window = find_window(dpy, DefaultRootWindow(dpy), winat, compare_window);
  if (window)
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

static XEN g_window_property(XEN winat, XEN name)
{
  #define H_window_property "(" S_window_property " win-name name): get or set the window property."
  Window window;
  Display *dpy;
  Atom type = None;
  int format;
  unsigned long len, bytesafter;
  unsigned char *data[1];
  XEN result = XEN_FALSE;
  XEN_ASSERT_TYPE(XEN_STRING_P(winat), winat, XEN_ARG_1, S_window_property, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_2, S_window_property, "a string");
  dpy = MAIN_DISPLAY(ss);
  if (((window = find_window(dpy, DefaultRootWindow(dpy), XEN_TO_C_STRING(winat), compare_window))) &&
      ((XGetWindowProperty(dpy, window, 
			   XInternAtom(dpy, XEN_TO_C_STRING(name), 0), 
			   0L, (long)BUFSIZ, False, 
			   XA_STRING, &type, &format, &len, &bytesafter,
			   (unsigned char **)data)) == Success) &&
      (type != None) &&
      (len > 0))
    {
      if (type == XA_STRING)
	result = C_TO_XEN_STRING((char *)data[0]);
      else result = C_TO_XEN_STRINGN((char *)data[0], len * format / 8); 
      if (data[0]) 
	XFree((char *)(data[0]));
    }
  return(result);
}

static XEN g_set_window_property(XEN winat, XEN name, XEN command)
{
  char *c = NULL;
  /* winat arg needed as well as command arg because we need an atom that is guaranteed to have a value */
  /*   Supposedly WM_STATE is just such an atom */
  XEN_ASSERT_TYPE(XEN_STRING_P(winat), name, XEN_ARG_1, S_setB S_window_property, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_2, S_setB S_window_property, "a string");
  if (XEN_STRING_P(command))
    c = copy_string(XEN_TO_C_STRING(command));
  else c = g_print_1(command);
  change_window_property(XEN_TO_C_STRING(winat), XEN_TO_C_STRING(name), c);
  if (c) FREE(c);
  return(XEN_FALSE);
}

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_1(g_send_mozilla_w, g_send_mozilla)
XEN_NARGIFY_2(g_window_property_w, g_window_property)
XEN_NARGIFY_3(g_set_window_property_w, g_set_window_property)
#else
#define g_send_mozilla_w g_send_mozilla
#define g_window_property_w g_window_property
#define g_set_window_property_w g_set_window_property
#endif

void g_init_gxutils(void)
{
  XEN_DEFINE_PROCEDURE(S_send_mozilla, g_send_mozilla_w, 1, 0, 0, H_send_mozilla);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_window_property, g_window_property_w, H_window_property,
				   S_setB S_window_property, g_set_window_property_w, 2, 0, 3, 0);
}

#else
/* gtk+osx = no X window property access, apparently */
bool send_mozilla(const char *html_viewer, const char *url) {return(false);}
void g_init_gxutils(void) {}
#endif

#endif

