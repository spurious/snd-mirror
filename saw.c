/* try out Snd as a mere widget in some grander context */

#include "snd.h"

#ifndef USE_GTK

/* ---------------- MOTIF VERSION ---------------- */

static int snd_running = 0;
Widget form, shell, label, snd;
XtAppContext app;     
int n;
Arg args[20];

static void snd_callback(Widget w, XtPointer clientData, XtPointer callData) 
{
  if (!snd_running)
    {
      n = 0;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, TRUE); n++;
      
      snd_as_widget(0, NULL, app, form, args, n);
      snd_open_file("~/cl/oboe.snd", get_global_state(), FALSE);

      snd_running = 1;
    }
}

int main (int argc, char **argv )
{
  shell = XtVaAppInitialize (&app, "Snd-as-widget", NULL, 0, &argc, argv, NULL,
			     XmNminWidth, 200,
			     XmNminHeight, 40,
			     XmNallowResize, TRUE,
			     NULL);
  form = XtCreateManagedWidget("form", xmFormWidgetClass, shell, NULL, 0);

  n = 0;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  label =  XtCreateManagedWidget("push for Snd", xmPushButtonWidgetClass, form, args, n);
  XtAddCallback(label, XmNactivateCallback, snd_callback, NULL);
  
  XtRealizeWidget(shell);
  XtAppMainLoop(app);
}

/* here's a sample makefile:

CC = gcc
CFLAGS = -g -DLINUX -DUSR_LIB_OSS = 1 -I/usr/X11R6/include -DSND_AS_WIDGET -DUSE_SND

LIBS = /usr/X11R6/lib/libMrm.a /usr/X11R6/lib/libXm.a /usr/X11R6/lib/libXp.a /usr/X11R6/lib/libXpm.a /usr/X11R6/lib/libXext.a /usr/X11R6/lib/libXt.a /usr/X11R6/lib/libSM.a /usr/X11R6/lib/libICE.a /usr/X11R6/lib/libFS.a /usr/X11R6/lib/libX11.a -lm

include makesnd.files

saw: $(SNDLIB_HEADERS) $(SND_HEADERS) $(SND_X_HEADERS) $(SNDLIB_O_FILES) $(O_FILES) $(X_O_FILES) saw.o
	$(CC) $(SNDLIB_O_FILES) $(O_FILES) $(X_O_FILES) saw.o -o saw $(LIBS)

include makesnd.back
*/
/* 
   if anyone uses this feature, and Snd doesn't do what you want, or you
   need more hooks into Snd, send me (bil@ccrma.stanford.edu) a description
   of the problem and so on.

   if you're including Guile, remember that it needs to be the top-level,
   so we'd use something like:

     void saw_main(int argc, char **argv )
     ...
  
   in place of the current "main", and then

     int main (int argc, char **argv )
     {
       gh_enter(argc, argv, saw_main);
       return(0);
     }

   as our new main program.  Also, include -lguile in the LIBS statement above,
   and (if not using configure), -DHAVE_GUILE in the CFLAGS statement.
*/

#else

/* ---------------- GTK VERSION ---------------- */

/* Gtk+ version using Guile */

static int snd_running = 0;
GtkWidget *form, *shell, *label, *snd;

static void snd_callback(GtkWidget *w, gpointer data) 
{
  if (!snd_running)
    {
      snd = snd_as_widget(0, NULL, w, NULL);
      gtk_box_pack_start(GTK_BOX(form), snd, TRUE, TRUE, 0);
      gtk_widget_show(snd);
      snd_open_file("~/cl/oboe.snd", get_global_state(), FALSE); 
      snd_running = 1;
      gtk_label_set_text(GTK_LABEL(GTK_BIN(label)->child), "Push to quit Snd");
    }
  else
    {
      gtk_label_set_text(GTK_LABEL(GTK_BIN(label)->child), "Push for Snd");
      gtk_widget_hide(snd);
      snd_running = 0;
    }
}

static gint window_close(GtkWidget *w, GdkEvent *event, gpointer clientData)
{
  gtk_main_quit();
  return(FALSE);
}

static void gsnd_main (int argc, char **argv )
{
  gtk_init(&argc, &argv);
  shell = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  ALLOW_RESIZE(shell);
  SET_USIZE(shell, 200, 40);
  form = gtk_vbox_new(FALSE, 0);
  gtk_container_add(GTK_CONTAINER(shell), form);
  gtk_widget_show(form);

  label = gtk_button_new_with_label("push for Snd");
  gtk_box_pack_start(GTK_BOX(form), label, FALSE, FALSE, 0);
  gtk_widget_show(label);
  gtk_widget_show(shell);

  gtk_signal_connect(GTK_OBJECT(label), "clicked", GTK_SIGNAL_FUNC(snd_callback), (gpointer)form);
  gtk_signal_connect(GTK_OBJECT(shell), "delete_event", GTK_SIGNAL_FUNC(window_close), NULL);
  gtk_main();
}

int main(int argc, char *argv[])
{
  gh_enter(argc, argv, gsnd_main);
  return(0);
}

#endif
