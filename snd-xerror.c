#include "snd.h"

/* TODO: after yes_or_no_p removal, ss->error_lock not needed */

/* ---------------- yes or no dialog: will go away soon! ---------------- */

static bool yes_or_no = false;

static void yes_callback(Widget w, XtPointer context, XtPointer info) {yes_or_no = true;}
static void no_callback(Widget w, XtPointer context, XtPointer info) {yes_or_no = false;}

#define YES_OR_NO_BUFFER_SIZE 1024

bool snd_yes_or_no_p(const char *format, ...)
{
  static Widget yes_or_no_dialog = NULL;
  XmString error_msg;
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
  yes_or_no = false;
  if (!yes_or_no_dialog)
    {
      Arg args[20];
      int n;
      XmString titlestr, xmstr1, xmstr2;

      titlestr = XmStringCreate(_("Yow!"), XmFONTLIST_DEFAULT_TAG);
      xmstr1 = XmStringCreate(_("Yes"), XmFONTLIST_DEFAULT_TAG);
      xmstr2 = XmStringCreate(_("No"), XmFONTLIST_DEFAULT_TAG);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
      XtSetArg(args[n], XmNokLabelString, xmstr1); n++;
      XtSetArg(args[n], XmNcancelLabelString, xmstr2); n++;
      yes_or_no_dialog = XmCreateQuestionDialog(MAIN_PANE(ss), "yow!", args, n);

      XtManageChild(yes_or_no_dialog);

      XtUnmanageChild(XmMessageBoxGetChild(yes_or_no_dialog, XmDIALOG_SYMBOL_LABEL));
      XtUnmanageChild(XmMessageBoxGetChild(yes_or_no_dialog, XmDIALOG_HELP_BUTTON));
      if (!(ss->using_schemes)) map_over_children(yes_or_no_dialog, set_main_color_of_widget, NULL);
      XtAddCallback(yes_or_no_dialog, XmNokCallback, yes_callback, NULL);
      XtAddCallback(yes_or_no_dialog, XmNcancelCallback, no_callback, NULL);

      if (!(ss->using_schemes))
	{
	  XtVaSetValues(XmMessageBoxGetChild(yes_or_no_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(yes_or_no_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(yes_or_no_dialog, XmDIALOG_OK_BUTTON), XmNbackground, ss->sgx->doit_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(yes_or_no_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->quit_button_color, NULL);
	}
      XmStringFree(titlestr);
      XmStringFree(xmstr1);
      XmStringFree(xmstr2);
      set_dialog_widget(YES_OR_NO_DIALOG, yes_or_no_dialog);
    }
  error_msg = XmStringCreate(yes_buf, XmFONTLIST_DEFAULT_TAG);
  if (!(XtIsManaged(yes_or_no_dialog))) 
    XtManageChild(yes_or_no_dialog);
  XtVaSetValues(yes_or_no_dialog, XmNmessageString, error_msg, NULL);
#if DEBUGGING
  if (with_background_processes(ss))
#endif
    {
      ss->error_lock = true;
      while ((XtIsManaged(yes_or_no_dialog)) && (ss->error_lock))
	check_for_event();
      ss->error_lock = false;
    }
  if (XtIsManaged(yes_or_no_dialog))
    XtUnmanageChild(yes_or_no_dialog);
  XmStringFree(error_msg);
  FREE(yes_buf);
  return(yes_or_no);
}
  


