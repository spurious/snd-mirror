#include "snd.h"

/* -------- STATS WINDOW -------- */

static Widget stats_window = NULL;
static Widget stats_form = NULL;

static void stats_help(Widget w, XtPointer context, XtPointer info) 
{
  stats_dialog_help((snd_state *)context);
}

static void stats_dismiss(Widget w, XtPointer context, XtPointer info) 
{
  set_show_usage_stats((snd_state *)context, FALSE);
}

static void stats_update(Widget w, XtPointer context, XtPointer info) 
{
  snd_state *ss = (snd_state *)context;
  update_all_usage_stats(ss);
  check_stats_window(ss, TRUE);
}

void update_stats(snd_state *ss) 
{
  update_stats_with_widget(ss, stats_form);
}

void update_stats_display(snd_state *ss, int all)
{
  /* dismiss update help -- update forces recalc of all stats */
  XmString xstr1, xstr2, xstr3, titlestr;
  int n;
  Arg args[20];

  if (!stats_window)
    {
      n = 0;
      xstr1 = XmStringCreate(STR_Dismiss, XmFONTLIST_DEFAULT_TAG);
      xstr2 = XmStringCreate(STR_Help, XmFONTLIST_DEFAULT_TAG);
      xstr3 = XmStringCreate(STR_Update, XmFONTLIST_DEFAULT_TAG);
      titlestr = XmStringCreate(STR_Disk_and_Memory_Usage, XmFONTLIST_DEFAULT_TAG);
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNcancelLabelString, xstr3); n++;
      XtSetArg(args[n], XmNhelpLabelString, xstr2); n++;
      XtSetArg(args[n], XmNokLabelString, xstr1); n++;
      XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
      XtSetArg(args[n], XmNautoUnmanage, FALSE); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, FALSE); n++;
      XtSetArg(args[n], XmNtransient, FALSE); n++;
      stats_window = XmCreateTemplateDialog(MAIN_SHELL(ss), STR_Disk_and_Memory_Usage, args, n);
      set_dialog_widget(STATS_DIALOG, stats_window);
      add_dialog(ss, stats_window);

      XtAddCallback(stats_window, XmNcancelCallback, stats_update, ss);
      XtAddCallback(stats_window, XmNhelpCallback, stats_help, ss);
      XtAddCallback(stats_window, XmNokCallback, stats_dismiss, ss);
      XmStringFree(xstr1);
      XmStringFree(xstr2);
      XmStringFree(xstr3);
      XmStringFree(titlestr);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNheight,250); n++;
      XtSetArg(args[n], XmNwidth, 600); n++;
      stats_form = sndCreateTextWidget(ss, "stats", stats_window, args, n);

      XtManageChild(stats_window);

      if (!(ss->using_schemes)) 
	{
	  XtVaSetValues(XmMessageBoxGetChild(stats_window, XmDIALOG_OK_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(stats_window, XmDIALOG_CANCEL_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(stats_window, XmDIALOG_HELP_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  map_over_children(stats_window, set_main_color_of_widget, ss);
	  XtVaSetValues(stats_form, XmNbackground, (ss->sgx)->white, XmNforeground, (ss->sgx)->black, NULL);
	}
    }
  else raise_dialog(stats_window);
  if (all) update_all_usage_stats(ss);
  update_stats(ss);
  if (!(XtIsManaged(stats_window))) 
    XtManageChild(stats_window);
}

void check_stats_window(snd_state *ss, int val)
{
  /* if val==0, close active display if any, if val==1, open and spin through all current chans setting/gathering */
  if (val == 0)
    {
      if ((stats_window) && (XtIsManaged(stats_window)))
	XtUnmanageChild(stats_window);
    }
  else
    {
      update_stats_display(ss, TRUE);
    }
}

#if DEBUGGING
char *stats_window_state(void);
char *stats_window_state(void)
{
  if (stats_form)
    return(XmTextGetString(stats_form));
  return(NULL);
}
#endif
