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
    c = XEN_TO_C_STRING(command);
  else c = g_print_1(command, S_change_property);
  change_property(get_global_state(), 
		  XEN_TO_C_STRING(winat), 
		  XEN_TO_C_STRING(name), c);
  return(XEN_FALSE);
}

#if DEBUGGING
/* this is intended for auto-testing the user-interface */

static XEN g_move_scale(XEN scale, XEN val)
{
#if USE_MOTIF
  /* weird that Motif does not have a notify arg (or any equivalent anywhere) for XmScaleSetValue */
  Widget scl;
  XmScaleCallbackStruct *cbs;
  cbs = (XmScaleCallbackStruct *)calloc(1, sizeof(XmScaleCallbackStruct));
  cbs->value = XEN_TO_C_INT(val);
  cbs->event = (XEvent *)calloc(1, sizeof(XEvent)); /* needed else freed mem troubles -- who is freeing these pointers? */
  scl = (Widget)XEN_UNWRAP_WIDGET(scale);
  XmScaleSetValue(scl, cbs->value);
  XtCallCallbacks(scl, XmNvalueChangedCallback, (XtPointer)cbs);
#endif
  return(scale);
}

#endif


#ifdef XEN_ARGIFY_1
XEN_NARGIFY_1(send_netscape_w, send_netscape)
XEN_NARGIFY_3(g_change_property_w, g_change_property)
#if DEBUGGING
XEN_NARGIFY_2(g_move_scale_w, g_move_scale)
#endif
#else
#define send_netscape_w send_netscape
#define g_change_property_w g_change_property
#if DEBUGGING
#define g_move_scale_w g_move_scale
#endif
#endif

void g_init_gxutils(void)
{
  XEN_DEFINE_PROCEDURE("send-netscape", send_netscape_w, 1, 0, 0, "");
  XEN_DEFINE_PROCEDURE(S_change_property, g_change_property_w, 3, 0, 0, H_change_property);
#if DEBUGGING
  XEN_DEFINE_PROCEDURE("move-scale", g_move_scale_w, 2, 0, 0, "");
#endif
}

#endif

