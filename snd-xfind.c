#include "snd.h"

/* -------- edit find -------- */

static Widget edit_find_dialog, edit_find_text, cancelB, edit_find_label, findnextB;

static void edit_find_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help((snd_state *)context,
	   "Global Find",
"This search travels through all the current channels\n\
in parallel until a match is found.  The find\n\
expression is a Scheme function of one argument, \n\
the current sample value.  It should return #t when the\n\
search is satisified.  For example, (lambda (n) (> n .1)\n\
looks for the next sample that is greater than .1.\n\
");
} 

static void edit_find_ok_callback(int direction, Widget w, XtPointer context, XtPointer info)
{ /* "Find" is the label here */
  char *str, *buf = NULL;
  XmString s1;
  snd_state *ss = (snd_state *)context;
  SCM proc;
  str = XmTextGetString(edit_find_text);
  if ((str) && (*str))
    { 
      if (ss->search_expr) XtFree(ss->search_expr);
      ss->search_expr = str;
      if (PROCEDURE_P(ss->search_proc)) snd_unprotect(ss->search_proc);
      ss->search_proc = SCM_UNDEFINED;
      proc = snd_catch_any(eval_str_wrapper, str, str);
      if (procedure_ok_with_error(proc, 1, "find", "find", 1))
	{
	  ss->search_proc = proc;
	  snd_protect(proc);
	}
      buf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
      mus_snprintf(buf, PRINT_BUFFER_SIZE, "find: %s", str);
      set_label(edit_find_label, buf);
      XmTextSetString(edit_find_text, NULL);
      FREE(buf);
    }
  if (PROCEDURE_P(ss->search_proc))
    {
      s1 = XmStringCreate(STR_Stop, XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(cancelB, XmNlabelString, s1, NULL);
      XmStringFree(s1);
      str = global_search(ss, direction);
      s1 = XmStringCreate(STR_Dismiss, XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(cancelB, XmNlabelString, s1, NULL);
      XmStringFree(s1);
      if ((str) && (*str)) set_label(edit_find_label, str);
    }
} 

static void edit_find_next_callback(Widget w, XtPointer context, XtPointer info) {edit_find_ok_callback(READ_FORWARD, w, context, info);}
static void edit_find_previous_callback(Widget w, XtPointer context, XtPointer info) {edit_find_ok_callback(READ_BACKWARD, w, context, info);}

static void edit_find_cancel_callback(Widget w, XtPointer context, XtPointer info)
{ /* "Done" */
  snd_state *ss = (snd_state *)context;
  XmAnyCallbackStruct *cb = (XmAnyCallbackStruct *)info;
  if (cb->event != (ss->sgx)->text_activate_event)
    {
      if (ss->checking_explicitly)
	ss->stopped_explicitly = 1;
      else XtUnmanageChild(edit_find_dialog);
    }
  else edit_find_next_callback(w, context, info);
} 

static void make_edit_find_dialog(snd_state *ss)
{
  Widget dl, rc;
  Arg args[20];
  int n;
  XmString xmstr1, xmstr2, xmstr3, titlestr;

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
  xmstr1 = XmStringCreate(STR_Dismiss, XmFONTLIST_DEFAULT_TAG);
  xmstr2 = XmStringCreate(STR_Next, XmFONTLIST_DEFAULT_TAG);
  xmstr3 = XmStringCreate(STR_Previous, XmFONTLIST_DEFAULT_TAG);
  titlestr = XmStringCreate(STR_Find, XmFONTLIST_DEFAULT_TAG);
  XtSetArg(args[n], XmNokLabelString, xmstr1); n++;
  XtSetArg(args[n], XmNcancelLabelString, xmstr3); n++;
  XtSetArg(args[n], XmNautoUnmanage, FALSE); n++;
  XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
  XtSetArg(args[n], XmNnoResize, FALSE); n++;
  XtSetArg(args[n], XmNtransient, FALSE); n++;
  edit_find_dialog = XmCreateMessageDialog(MAIN_SHELL(ss), "find", args, n);
  set_dialog_widget(FIND_DIALOG, edit_find_dialog);
  add_dialog(ss, edit_find_dialog);

  XmStringFree(xmstr1);
  XmStringFree(xmstr2);
  XmStringFree(titlestr);

  XtUnmanageChild(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_SYMBOL_LABEL));
  XtUnmanageChild(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_MESSAGE_LABEL));

  XtAddCallback(edit_find_dialog, XmNhelpCallback, edit_find_help_callback, ss);
  XtAddCallback(edit_find_dialog, XmNcancelCallback, edit_find_previous_callback, ss);
  XtAddCallback(edit_find_dialog, XmNokCallback, edit_find_cancel_callback, ss);

  n = 0;
  if (!(ss->using_schemes)) 
    {
      XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;
      XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;
    }
  findnextB = XtCreateManagedWidget(STR_Next, xmPushButtonWidgetClass, edit_find_dialog, args, n);
  XtAddCallback(findnextB, XmNactivateCallback, edit_find_next_callback, ss);

  rc = sndCreateFormWidget("row", edit_find_dialog, NULL, 0);

  n = 0;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  dl = XtCreateManagedWidget("find:", xmLabelWidgetClass, rc, args, n);

  n = 0;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, dl); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  edit_find_text = sndCreateTextFieldWidget(ss, "text", rc, args, n, ACTIVATABLE, NO_COMPLETER);

  n = 0;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, edit_find_text); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  edit_find_label = XtCreateManagedWidget(STR_global_search, xmLabelWidgetClass, rc, args, n);
}

void Edit_Find_Callback(Widget w, XtPointer context, XtPointer info)
{
  snd_state *ss = (snd_state *)context;
  if (!edit_find_dialog)
    {
      make_edit_find_dialog(ss);
      XtManageChild(edit_find_dialog);

      if (!(ss->using_schemes)) 
	{
	  map_over_children(edit_find_dialog, set_main_color_of_widget, (void *)context);
	  XtVaSetValues(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	}
      cancelB = XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_OK_BUTTON);
    }
  else raise_dialog(edit_find_dialog);
  if (!XtIsManaged(edit_find_dialog)) XtManageChild(edit_find_dialog);
}

#if DEBUGGING
static SCM g_find_dialog_widgets(void)
{
  if (edit_find_dialog)
    return(CONS(SND_WRAP(edit_find_dialog),
	     CONS(SND_WRAP(edit_find_text),
  	       CONS(SND_WRAP(findnextB),
		 CONS(SND_WRAP(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_CANCEL_BUTTON)), /* find previous */
		   CONS(SND_WRAP(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_OK_BUTTON)),   /* cancel */
			SCM_EOL))))));
  return(SCM_EOL);
}

static SCM g_edit_find_dialog(void)
{
  Edit_Find_Callback(NULL, (XtPointer *)(get_global_state()), NULL);
  return(SCM_BOOL_F);
}

void g_init_gxfind(SCM local_doc)
{
  DEFINE_PROC("edit-find-dialog", g_edit_find_dialog, 0, 0, 0, "");
  DEFINE_PROC("find-dialog-widgets", g_find_dialog_widgets, 0, 0, 0, "");
}

#endif
