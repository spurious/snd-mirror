#include "snd.h"

/* error handlers -- these include the error dialog (in case no sound is active) and an error history list */

static Widget snd_error_dialog = NULL;
static Widget snd_error_history = NULL;

static void create_snd_error_dialog(snd_state *ss, int popup)
{
  Arg args[32];
  int n;
  XmString titlestr;
  titlestr = XmStringCreate(STR_Error, XmFONTLIST_DEFAULT_TAG);
  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
  XtSetArg(args[n], XmNnoResize, FALSE); n++;
  XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
  snd_error_dialog = XmCreateErrorDialog(MAIN_PANE(ss), "error", args, n);
  set_dialog_widget(ss, ERROR_DIALOG, snd_error_dialog);
  XtUnmanageChild(XmMessageBoxGetChild(snd_error_dialog, XmDIALOG_SYMBOL_LABEL));
  XtUnmanageChild(XmMessageBoxGetChild(snd_error_dialog, XmDIALOG_CANCEL_BUTTON));
  XtUnmanageChild(XmMessageBoxGetChild(snd_error_dialog, XmDIALOG_HELP_BUTTON));

  n = 0;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;
  XtSetArg(args[n], XmNscrollBarDisplayPolicy, XmAS_NEEDED); n++;
  XtSetArg(args[n], XmNeditable, FALSE); n++;
  XtSetArg(args[n], XmNautoShowCursorPosition, FALSE); n++;
  XtSetArg(args[n], XmNcursorPositionVisible, FALSE); n++;
  XtSetArg(args[n], XmNscrollingPolicy, XmAUTOMATIC); n++;
  snd_error_history = XmCreateScrolledText(snd_error_dialog, STR_Error_History, args, n);
  XtManageChild(snd_error_history);

  if (popup) 
    XtManageChild(snd_error_dialog);

  if (!(ss->using_schemes)) map_over_children(snd_error_dialog, set_main_color_of_widget, (void *)ss);
  XmStringFree(titlestr);
  if (!(ss->using_schemes))
    {
      XtVaSetValues(XtNameToWidget(snd_error_dialog, "OK"), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
      XtVaSetValues(snd_error_history, XmNbackground, (ss->sgx)->white, XmNforeground, (ss->sgx)->black, NULL);
    }
}

void add_to_error_history(snd_state *ss, char *msg, int popup)
{
#if HAVE_STRFTIME
  char *tim, *buf;
  time_t ts;
#endif
  XmTextPosition pos;
  if (!snd_error_dialog) 
    create_snd_error_dialog(ss, popup);
  else
    if ((popup) && 
	(!(XtIsManaged(snd_error_dialog))))
      XtManageChild(snd_error_dialog);
#if HAVE_STRFTIME
  tim = (char *)CALLOC(TIME_STR_SIZE, sizeof(char));
  buf = (char *)CALLOC(TIME_STR_SIZE, sizeof(char));
  time(&ts);
  strftime(tim, TIME_STR_SIZE, "%H:%M:%S", localtime(&ts));
  sprintf(buf, "\n[%s] ", tim);
  pos = XmTextGetLastPosition(snd_error_history);
  if (pos == 0) 
    XmTextSetString(snd_error_history, buf);
  else XmTextInsert(snd_error_history, pos, buf);
  FREE(buf);
  FREE(tim);
#endif
  pos = XmTextGetLastPosition(snd_error_history);
  if (pos == 0) 
    XmTextSetString(snd_error_history, msg);
  else 
    {
      XmTextInsert(snd_error_history, pos, msg);
      if (XmGetVisibility(snd_error_history) != XmVISIBILITY_FULLY_OBSCURED)
	{
	  pos = XmTextGetLastPosition(snd_error_history);
	  XmTextShowPosition(snd_error_history, pos-1); /* if pos here, stupid thing segfaults! */
	}
    }
}

void post_error_dialog(snd_state *ss, char *msg)
{
  XmString error_msg;
  if (!snd_error_dialog) create_snd_error_dialog(ss, TRUE);
  error_msg = XmStringCreateLtoR(msg, XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(snd_error_dialog, XmNmessageString, error_msg, NULL);
  if (!(XtIsManaged(snd_error_dialog)))
    XtManageChild(snd_error_dialog);
  XmStringFree(error_msg);
}

void show_snd_errors(snd_state *ss)
{
  if (snd_error_dialog)
    {
      if (!(XtIsManaged(snd_error_dialog))) 
	XtManageChild(snd_error_dialog);
      else raise_dialog(snd_error_dialog);
    }
  else post_error_dialog(ss, "no errors yet");
}

static int yes_or_no = 0;

static void YesCallback(Widget w, XtPointer context, XtPointer info) {yes_or_no = 1;}
static void NoCallback(Widget w, XtPointer context, XtPointer info) {yes_or_no = 0;}

#define YES_OR_NO_BUFFER_SIZE 1024

int snd_yes_or_no_p(snd_state *ss, const char *format, ...)
{
  static Widget yes_or_no_dialog = NULL;
  Arg args[20];
  int n;
  XmString titlestr, error_msg, xmstr1, xmstr2;

  char *yes_buf;
#if HAVE_VPRINTF
  va_list ap;
  yes_buf = (char *)CALLOC(YES_OR_NO_BUFFER_SIZE, sizeof(char));
  va_start(ap, format);
#if HAVE_VSNPRINTF
  vsnprintf(yes_buf, YES_OR_NO_BUFFER_SIZE, format, ap);
#else
  vsprintf(yes_buf, format, ap);
#endif
  va_end(ap);
#else
  yes_buf = (char *)CALLOC(256, sizeof(char));
#if HAVE_SNPRINTF
  snprintf(yes_buf, YES_OR_NO_BUFFER_SIZE, "%s...[you need vprintf]", format);
#else
  sprintf(yes_buf, "%s...[you need vprintf]", format);
#endif
#endif
  yes_or_no = 0;
  if (!yes_or_no_dialog)
    {
      titlestr = XmStringCreate(STR_Big_Trouble, XmFONTLIST_DEFAULT_TAG);
      xmstr1 = XmStringCreate(STR_Yes, XmFONTLIST_DEFAULT_TAG);
      xmstr2 = XmStringCreate(STR_No, XmFONTLIST_DEFAULT_TAG);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, FALSE); n++;
      XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
      XtSetArg(args[n], XmNokLabelString, xmstr1); n++;
      XtSetArg(args[n], XmNcancelLabelString, xmstr2); n++;
      yes_or_no_dialog = XmCreateQuestionDialog(MAIN_PANE(ss), "yow!", args, n);
      set_dialog_widget(ss, YES_OR_NO_DIALOG, yes_or_no_dialog);
      XtManageChild(yes_or_no_dialog);

      XtUnmanageChild(XmMessageBoxGetChild(yes_or_no_dialog, XmDIALOG_SYMBOL_LABEL));
      XtUnmanageChild(XmMessageBoxGetChild(yes_or_no_dialog, XmDIALOG_HELP_BUTTON));
      if (!(ss->using_schemes)) map_over_children(yes_or_no_dialog, set_main_color_of_widget, (void *)ss);
      XtAddCallback(yes_or_no_dialog, XmNokCallback, YesCallback, NULL);
      XtAddCallback(yes_or_no_dialog, XmNcancelCallback, NoCallback, NULL);

      if (!(ss->using_schemes))
	{
	  XtVaSetValues(XmMessageBoxGetChild(yes_or_no_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(yes_or_no_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	}
      XmStringFree(titlestr);
      XmStringFree(xmstr1);
      XmStringFree(xmstr2);
    }
  error_msg = XmStringCreateLtoR(yes_buf, XmFONTLIST_DEFAULT_TAG);
  if (!(XtIsManaged(yes_or_no_dialog))) 
    XtManageChild(yes_or_no_dialog);
  XtVaSetValues(yes_or_no_dialog, XmNmessageString, error_msg, NULL);
  ss->error_lock = 1;
  while ((XtIsManaged(yes_or_no_dialog)) && (ss->error_lock == 1))
    check_for_event(ss);
  ss->error_lock = 0;
  if (XtIsManaged(yes_or_no_dialog))
    XtUnmanageChild(yes_or_no_dialog);
  XmStringFree(error_msg);
  FREE(yes_buf);
  return(yes_or_no);
}
  
