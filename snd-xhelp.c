#include "snd.h"

/* ---------------- HELP MONOLOG ---------------- */

#define HELP_ROWS 12
#define HELP_COLUMNS 56
/* these set the initial size of the (non XmHTML) help dialog text area */

static Widget help_dialog = NULL;
static Widget help_text = NULL;
static char help_window_label[LABEL_BUFFER_SIZE];

static char *cr_to_space(char *val)
{
  int i, len;
  if (val)
    {
      len = strlen(val);
      for (i = 0; i < len; i++)
	if (val[i] == '\n')
	  val[i] = ' ';
    }
  return(val);
}

static bool no_cr(const char *val)
{
  int i, len;
  if (val)
    {
      len = strlen(val);
      for (i = 0; i < len; i++)
	if (val[i] == '\n')
	  return(false);
    }
  return(true);
}

static int help_text_width = 0;
static bool outer_with_wrap = false;
static void help_expose(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  int curwid;
  curwid = widget_width(help_text);
  if (help_text_width == 0)
    help_text_width = curwid;
  else
    {
      if ((outer_with_wrap) && (abs(curwid - help_text_width) > 10))
	{
	  char *cur_help;
	  char *new_help = NULL;
	  cur_help = cr_to_space(XmTextGetString(help_text));
	  new_help = word_wrap(cur_help, curwid);
	  XmTextSetString(help_text, new_help);
	  if (new_help) FREE(new_help);
	  if (cur_help) XtFree(cur_help);
	  help_text_width = curwid;
	}
    }
}

static void create_help_monolog(void)
{
  /* create scrollable but not editable text window */
  Arg args[20];
  int n;
  XmString titlestr;
  titlestr = XmStringCreate(_("Help"), XmFONTLIST_DEFAULT_TAG);
  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
  XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
  /* this window should be resizable by the user (i.e. have the resize bars), but not resize itself */
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
  XtSetArg(args[n], XmNnoResize, false); n++;
  XtSetArg(args[n], XmNtransient, false); n++;
  help_dialog = XmCreateMessageDialog(MAIN_PANE(ss), "snd-help", args, n);
  XtAddEventHandler(help_dialog, ExposureMask, false, help_expose, NULL);

  XtUnmanageChild(XmMessageBoxGetChild(help_dialog, XmDIALOG_CANCEL_BUTTON));
  XtUnmanageChild(XmMessageBoxGetChild(help_dialog, XmDIALOG_HELP_BUTTON));
  XtUnmanageChild(XmMessageBoxGetChild(help_dialog, XmDIALOG_SYMBOL_LABEL));
      
  XmStringFree(titlestr);

  n = 0;
  XtSetArg(args[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;
  XtSetArg(args[n], XmNeditable, false); n++;
  XtSetArg(args[n], XmNcolumns, HELP_COLUMNS); n++;
  XtSetArg(args[n], XmNrows, HELP_ROWS); n++;
  if (!(ss->using_schemes))
    {
      XtSetArg(args[n], XmNforeground, (ss->sgx)->black); n++; /* needed if color allocation fails completely */
      XtSetArg(args[n], XmNbackground, (ss->sgx)->white); n++;
    }
  help_text = XmCreateScrolledText(help_dialog, "help-text", args, n);
  XtManageChild(help_text);

  XtManageChild(help_dialog);

  if (!(ss->using_schemes))
    {
      map_over_children(help_dialog, set_main_color_of_widget, NULL);
      XtVaSetValues(help_text, XmNbackground, (ss->sgx)->white, XmNforeground, (ss->sgx)->black, NULL);
      XtVaSetValues(XmMessageBoxGetChild(help_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
    }
  set_dialog_widget(HELP_DIALOG, help_dialog);
}

Widget snd_help(const char *subject, const char *helpstr, bool with_wrap)
{
  /* place help string in scrollable help window */
  /* if window is already active, add this help at the top and reposition */
  /* if XmHTML, this is writing the text to the root window if called from Guile?!? */
  XmString xstr1;
  outer_with_wrap = ((with_wrap) && (no_cr(helpstr)));
  if (!(help_dialog)) 
    create_help_monolog(); 
  else raise_dialog(help_dialog);
  mus_snprintf(help_window_label, LABEL_BUFFER_SIZE, _("%s help"), subject);
  xstr1 = XmStringCreate(help_window_label, XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(help_dialog, XmNmessageString, xstr1, NULL);
  if (with_wrap)
    {
      char *new_help = NULL;
      new_help = word_wrap(helpstr, widget_width(help_text));
      XmTextSetString(help_text, new_help);
      if (new_help) FREE(new_help);
    }
  else XmTextSetString(help_text, (char *)helpstr);
  if (!XtIsManaged(help_dialog)) 
    XtManageChild(help_dialog);
  XmStringFree(xstr1);
  return(help_dialog);
}
