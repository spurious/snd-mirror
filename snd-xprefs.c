#include "snd.h"

/* preferences dialog; layout design taken from webmail */

/* start automatically if no .snd? -- create empty if not wanted?
 */


static void preferences_help_callback(Widget w, XtPointer context, XtPointer info) 
{
}

static void preferences_quit_callback(Widget w, XtPointer context, XtPointer info) 
{
}

static void preferences_reset_callback(Widget w, XtPointer context, XtPointer info) 
{
}

static void preferences_save_callback(Widget w, XtPointer context, XtPointer info) 
{
}


static Widget preferences_dialog = NULL;

void start_preferences_dialog(void)
{
  Arg args[20];
  int n;
  XmString title, help, reset, save, dismiss;
  Widget reset_button;

  title = XmStringCreate(_("Preferences"), XmFONTLIST_DEFAULT_TAG);
  help = XmStringCreate(_("Help"), XmFONTLIST_DEFAULT_TAG);
  reset = XmStringCreate(_("Reset"), XmFONTLIST_DEFAULT_TAG);
  save = XmStringCreate(_("Save"), XmFONTLIST_DEFAULT_TAG);
  dismiss = XmStringCreate(_("Dismiss"), XmFONTLIST_DEFAULT_TAG);

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
  XtSetArg(args[n], XmNnoResize, false); n++;
  XtSetArg(args[n], XmNtransient, false); n++;
  XtSetArg(args[n], XmNcancelLabelString, dismiss); n++;
  XtSetArg(args[n], XmNhelpLabelString, help); n++;
  XtSetArg(args[n], XmNokLabelString, save); n++;
  XtSetArg(args[n], XmNdialogTitle, title); n++;
  XtSetArg(args[n], XmNallowShellResize, true); n++;
  XtSetArg(args[n], XmNautoUnmanage, false); n++;

  preferences_dialog = XmCreateTemplateDialog(MAIN_PANE(ss), "preferences", args, n);

  n = 0;
  if (!(ss->using_schemes)) 
    {
      XtSetArg(args[n], XmNbackground, ss->sgx->reset_button_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;
    }
  reset_button = XtCreateManagedWidget(_("Reset"), xmPushButtonGadgetClass, preferences_dialog, args, n);

  XtAddCallback(preferences_dialog, XmNcancelCallback, preferences_quit_callback, NULL);
  XtAddCallback(preferences_dialog, XmNhelpCallback, preferences_help_callback, NULL);
  XtAddCallback(preferences_dialog, XmNokCallback, preferences_save_callback, NULL);
  XtAddCallback(reset_button, XmNactivateCallback, preferences_reset_callback, NULL);

  XmStringFree(title);
  XmStringFree(help);
  XmStringFree(save);
  XmStringFree(dismiss);
  XmStringFree(reset);

  if (!(ss->using_schemes))
    {
      map_over_children(preferences_dialog, set_main_color_of_widget, NULL);
      XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_OK_BUTTON),     XmNbackground, ss->sgx->doit_button_color,   NULL);
      XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->quit_button_color,   NULL);
      XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->sgx->help_button_color,   NULL);
    }

  n = 0;



  XtManageChild(preferences_dialog);
  set_dialog_widget(PREFERENCES_DIALOG, preferences_dialog);

}

