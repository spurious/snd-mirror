/* TODO: make this look fancy */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#include <fcntl.h>
#include <Xm/XmAll.h>
#include <GL/xmesa.h>
#include <GL/gl.h>
#include <GL/GLwMDrawA.h>

static int file_probe(const char *arg) 
{
  int fd;
  #ifdef O_NONBLOCK
    fd = open(arg, O_RDONLY, O_NONBLOCK);
  #else
    fd = open(arg, O_RDONLY, 0);
  #endif
  if (fd == -1) return(0);
  close(fd);
  return(1);
}

Widget mesa;
float desc[5];
float **data;
XtAppContext app_context;
static GLXContext glx_context;

static void read_file(Widget mesa)
{
  int fd, bins, slices, srate, i, j;
  float cutoff, scaler, x, y, z;
  if (file_probe("glfft.lock"))
    {
      fd = open("glfft.data", O_RDONLY, 0);
      read(fd, (char *)desc, 5*sizeof(float));
      srate = (int)desc[0];
      scaler = desc[1];
      cutoff = desc[2];
      slices = (int)desc[3];
      bins = (int)desc[4];
      fprintf(stderr, "srate: %d, scaler: %f, cutoff: %f, slices: %d, bins: %d\n", srate, scaler, cutoff, slices, bins);
      data = (float **)malloc(slices * sizeof(float *));
      for (i = 0; i < slices; i++) 
	{
	  data[i] = (float *)malloc(bins * sizeof(float));
	  read(fd, (char *)(data[i]), bins*sizeof(float));
	}
      close(fd);
      remove("glfft.data");
      remove("glfft.lock");

      /* now display data */

      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
      glColor3f(1.0, 0.0, 0.0);

      y = -1.0;
      for (i = 0; i < slices; i++)
	{
	  glBegin(GL_LINE_STRIP);
	  x = -1.0;
	  for (j = 0; j < bins; j++)
	    {
	      glVertex3f(x, y + data[i][j], 0.0);
	      x += (2.0/bins);
	    }
	  y += (2.0/slices);
	  glEnd();
	}
      glFlush();
      glXSwapBuffers(XtDisplay (mesa), XtWindow (mesa));

      for (i = 0; i < slices; i++) free(data[i]);
      free(data);
    }
}

static void check_files(XtPointer clientData, XtIntervalId *id)
{
  /* get current if any and display, loop until none, then addapptimeout */
  read_file(mesa);
  XtAppAddTimeOut(app_context, 100, (XtTimerCallbackProc)check_files, NULL);
}

static void quit_function (Widget w, XtPointer closure, XtPointer call_data) {exit (0);}

static void translate_pixels (Widget to, Widget from, ...)
{
  va_list ap;
  char *name;
  Colormap from_cmap, to_cmap;
  XColor xcolor;

  XtVaGetValues (from, XtNcolormap, &from_cmap, NULL);
  XtVaGetValues (to, XtNcolormap, &to_cmap, NULL);

  va_start (ap, from);
  for (name = va_arg (ap, char *); name != NULL; name = va_arg (ap, char *))
    {
      XtVaGetValues (from, name, &xcolor.pixel, NULL);
      XQueryColor (XtDisplay (from), from_cmap, &xcolor);
      if (!XAllocColor (XtDisplay (to), to_cmap, &xcolor))
	XtAppWarning (XtWidgetToApplicationContext (to),
		      "Couldn't allocate color!\n");
      else
	XtVaSetValues (from, name, xcolor.pixel, NULL);
    }
  va_end (ap);
}

static String fallback_resources[] =
{
  "*GLwMDrawingArea.width: 300",
  "*GLwMDrawingArea.height: 300",
  "*GLwMDrawingArea.rgba: true",
  "*GLwMDrawingArea.installColormap: true",
  "*GLwMDrawingArea.doublebuffer: true",
  NULL
};

int main (int argc, char **argv)
{
  Widget top;
  Widget frame, quit;
  XVisualInfo *vi;
  Boolean rgba, doublebuffer, cmap_installed;

  top = XtVaAppInitialize (&app_context, "Cube",
			   NULL, 0,
			   &argc, argv, fallback_resources,
			   NULL);

  frame = XtVaCreateManagedWidget ("frame", xmFormWidgetClass,
				   top,
				   NULL);
  mesa = XtVaCreateManagedWidget ("mesa", glwMDrawingAreaWidgetClass,
				  frame,
				  XmNtopAttachment, XmATTACH_FORM,
				  XmNtopOffset, 10,
				  XmNleftAttachment, XmATTACH_FORM,
				  XmNleftOffset, 10,
				  XmNrightAttachment, XmATTACH_NONE,
				  XmNbottomAttachment,  XmATTACH_FORM,
				  XmNbottomOffset, 10,
				  XmNwidth, 300,
				  XmNheight, 300,
				  NULL);
  quit = XtVaCreateManagedWidget ("quit", xmPushButtonWidgetClass,
				  frame,
				  XmNtopAttachment, XmATTACH_FORM,
				  XmNtopOffset, 10,
				  XmNleftAttachment, XmATTACH_WIDGET,
				  XmNleftOffset, 10,
				  XmNleftWidget, mesa,
				  XmNrightAttachment, XmATTACH_FORM,
				  XmNrightOffset, 10,
				  XmNbottomAttachment, XmATTACH_NONE,
				  NULL);
  XtAddCallback (quit, XmNarmCallback, quit_function, NULL);
  XtRealizeWidget (top);

  XtVaGetValues (mesa,
		 GLwNrgba, &rgba,
		 GLwNinstallColormap, &cmap_installed,
		 GLwNdoublebuffer, &doublebuffer,
		 GLwNvisualInfo, &vi,
		 NULL);

  /* create a visual context */
  glx_context = glXCreateContext (XtDisplay(mesa), vi, NULL, GL_FALSE);

  GLwDrawingAreaMakeCurrent (mesa, glx_context);
			     
  translate_pixels (mesa, quit, XtNbackground, XtNforeground, XtNborder, NULL);
  translate_pixels (mesa, frame, XtNbackground, XtNborder, NULL);
  XWarpPointer (XtDisplay (mesa), None, XtWindow (mesa), 0, 0, 0, 0, 0, 0);

  glEnable(GL_DEPTH_TEST);
  glClearColor (1.0, 1.0, 1.0, 0.0);
  glOrtho(-1.0, 1.0, -1.0, 1.0, 0.0, 1.0);

  XtAppAddTimeOut(app_context, 100, (XtTimerCallbackProc)check_files, NULL);

  XtAppMainLoop (app_context);
  return (0);
}

/*
 cc -g glfft.c -o glfft /home/bil/test/Mesa-3.1/widgets-mesa/src/libMesaGLwM.a -L/usr/X11R6/lib -lGL -lXm -lXt -lX11 -I/home/bil/test/Mesa-3.1/widgets-mesa/include 
*/

