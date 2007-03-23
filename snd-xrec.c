#include "snd.h"

static Widget recorder = NULL;

static void close_recorder(Widget w, XtPointer context, XtPointer info)
{
  /* window manager close event */
}

static void quit_recorder(Widget w, XtPointer context, XtPointer info) 
{
  /* Quit button in dialog */
}

static void recorder_help(Widget w, XtPointer context, XtPointer info) 
{
  recording_help();
}

static void start_or_stop_recorder(Widget w, XtPointer context, XtPointer info) 
{
}


widget_t record_file(void) 
{
  if (!recorder)
    {
      Arg args[32];
      int n;
      XmString xquit, xhelp, xrecord, xtitle;
      Atom wm_delete;

      xquit = XmStringCreateLocalized(_("Quit"));
      xhelp = XmStringCreateLocalized(_("Help"));
      xrecord = XmStringCreateLocalized(_("Record"));
      xtitle = XmStringCreateLocalized(_("Record"));

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNcancelLabelString, xquit); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNokLabelString, xrecord); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNdialogTitle, xtitle); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_ANY); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNtransient, false); n++;
      recorder = XmCreateTemplateDialog(MAIN_SHELL(ss), _("Record"), args, n);

      XtAddCallback(recorder, XmNcancelCallback, quit_recorder, NULL);
      XtAddCallback(recorder, XmNhelpCallback, recorder_help, NULL);
      XtAddCallback(recorder, XmNokCallback, start_or_stop_recorder, NULL);

      XmStringFree(xhelp);
      XmStringFree(xquit);
      XmStringFree(xrecord);
      XmStringFree(xtitle);

      XtVaSetValues(XmMessageBoxGetChild(recorder, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(recorder, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(recorder, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(recorder, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->quit_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(recorder, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->sgx->help_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(recorder, XmDIALOG_OK_BUTTON),     XmNbackground, ss->sgx->doit_button_color, NULL);

      XtManageChild(recorder);

      wm_delete = XmInternAtom(XtDisplay(recorder), "WM_DELETE_WINDOW", false);
      XmAddWMProtocolCallback(XtParent(recorder), wm_delete, close_recorder, NULL);


    }


  return(recorder);
}


