#include "snd.h"

/* -------- edit find -------- */

static Widget edit_find_dialog, edit_find_text, cancelB, edit_find_label, findnextB;

static void edit_find_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help("Global Find",
"This search travels through all the current channels in parallel until a match is found.  The find \
expression is a Scheme function of one argument,  the current sample value.  It should return #t when the \
search is satisified.  For example, (lambda (n) (> n .1)) looks for the next sample that is greater than .1.",
	   true);
} 

static void edit_find_ok_callback(read_direction_t direction, Widget w, XtPointer context, XtPointer info)
{ /* "Find" is the label here */
  char *str, *buf = NULL;
  XmString s1;
  XEN proc;
  str = XmTextGetString(edit_find_text);
  if ((str) && (*str))
    { 
      if (ss->search_expr) FREE(ss->search_expr);
      ss->search_expr = copy_string(str);
      if (XEN_PROCEDURE_P(ss->search_proc)) snd_unprotect(ss->search_proc);
      ss->search_proc = XEN_UNDEFINED;
      if (ss->search_tree)
	ss->search_tree = free_ptree(ss->search_tree);
      proc = snd_catch_any(eval_str_wrapper, str, str);
      if ((XEN_PROCEDURE_P(proc)) &&
	  (XEN_TO_C_INT(XEN_CAR(XEN_ARITY(proc))) == 1))
	{
	  ss->search_proc = proc;
	  snd_protect(proc);
	  if (optimization(ss) > 0)
	    ss->search_tree = form_to_ptree_1_b_without_env(C_STRING_TO_XEN_FORM(str));
	  buf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
	  mus_snprintf(buf, PRINT_BUFFER_SIZE, _("find: %s"), str);
	  set_label(edit_find_label, buf);
	  XmTextSetString(edit_find_text, NULL);
	  FREE(buf);
	}
      if (str) XtFree(str);
    }
  else
    {
      if (ss->search_expr == NULL)
	{
	  /* using global search_proc set by user */
	  buf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
	  mus_snprintf(buf, PRINT_BUFFER_SIZE, _("find: %s"), XEN_AS_STRING(ss->search_proc));
	  set_label(edit_find_label, buf);
	  XmTextSetString(edit_find_text, NULL);
	  FREE(buf);
	}
    }
  if ((XEN_PROCEDURE_P(ss->search_proc)) || (ss->search_tree))
    {
      s1 = XmStringCreate(_("Stop"), XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(cancelB, XmNlabelString, s1, NULL);
      XmStringFree(s1);
      str = global_search(direction);
      s1 = XmStringCreate(_("Dismiss"), XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(cancelB, XmNlabelString, s1, NULL);
      XmStringFree(s1);
      if ((str) && (*str)) set_label(edit_find_label, str);
    }
} 

static void edit_find_next_callback(Widget w, XtPointer context, XtPointer info) {edit_find_ok_callback(READ_FORWARD, w, context, info);}
static void edit_find_previous_callback(Widget w, XtPointer context, XtPointer info) {edit_find_ok_callback(READ_BACKWARD, w, context, info);}

static void edit_find_cancel_callback(Widget w, XtPointer context, XtPointer info)
{ /* "Done" */
  XmAnyCallbackStruct *cb = (XmAnyCallbackStruct *)info;
  if (cb->event != (ss->sgx)->text_activate_event)
    {
      if (ss->checking_explicitly)
	ss->stopped_explicitly = true;
      else XtUnmanageChild(edit_find_dialog);
    }
  else edit_find_next_callback(w, context, info);
} 

static void make_edit_find_dialog(void)
{
  Widget dl, rc;
  Arg args[20];
  int n;
  XmString xmstr1, xmstr3, titlestr;

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
  xmstr1 = XmStringCreate(_("Dismiss"), XmFONTLIST_DEFAULT_TAG);
  xmstr3 = XmStringCreate(_("Previous"), XmFONTLIST_DEFAULT_TAG);
  titlestr = XmStringCreate(_("Find"), XmFONTLIST_DEFAULT_TAG);
  XtSetArg(args[n], XmNokLabelString, xmstr1); n++;
  XtSetArg(args[n], XmNcancelLabelString, xmstr3); n++;
  XtSetArg(args[n], XmNautoUnmanage, false); n++;
  XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
  XtSetArg(args[n], XmNnoResize, false); n++;
  XtSetArg(args[n], XmNtransient, false); n++;
  edit_find_dialog = XmCreateMessageDialog(MAIN_SHELL(ss), _("find"), args, n);

  XmStringFree(xmstr1);
  XmStringFree(xmstr3);
  XmStringFree(titlestr);

  XtUnmanageChild(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_SYMBOL_LABEL));
  XtUnmanageChild(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_MESSAGE_LABEL));

  XtAddCallback(edit_find_dialog, XmNhelpCallback, edit_find_help_callback, NULL);
  XtAddCallback(edit_find_dialog, XmNcancelCallback, edit_find_previous_callback, NULL);
  XtAddCallback(edit_find_dialog, XmNokCallback, edit_find_cancel_callback, NULL);

  n = 0;
  if (!(ss->using_schemes)) 
    {
      XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;
      XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;
    }
  findnextB = XtCreateManagedWidget(_("Next"), xmPushButtonGadgetClass, edit_find_dialog, args, n);
  XtAddCallback(findnextB, XmNactivateCallback, edit_find_next_callback, NULL);

  rc = XtCreateManagedWidget("row", xmFormWidgetClass, edit_find_dialog, NULL, 0);

  n = 0;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  dl = XtCreateManagedWidget(_("find:"), xmLabelWidgetClass, rc, args, n);

  n = 0;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, dl); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  edit_find_text = make_textfield_widget("text", rc, args, n, ACTIVATABLE, NO_COMPLETER);

  n = 0;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, edit_find_text); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  edit_find_label = XtCreateManagedWidget("global search", xmLabelWidgetClass, rc, args, n);
  set_dialog_widget(FIND_DIALOG, edit_find_dialog);
}

void edit_find_callback(Widget w, XtPointer context, XtPointer info)
{
  if (!edit_find_dialog)
    {
      make_edit_find_dialog();
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
static XEN g_find_dialog_widgets(void)
{
  if (edit_find_dialog)
    return(XEN_CONS(XEN_WRAP_WIDGET(edit_find_dialog),
	     XEN_CONS(XEN_WRAP_WIDGET(edit_find_text),
  	       XEN_CONS(XEN_WRAP_WIDGET(findnextB),
		 XEN_CONS(XEN_WRAP_WIDGET(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_CANCEL_BUTTON)), /* find previous */
		   XEN_CONS(XEN_WRAP_WIDGET(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_OK_BUTTON)),   /* cancel */
			XEN_EMPTY_LIST))))));
  return(XEN_EMPTY_LIST);
}

static XEN g_edit_find_dialog(void)
{
  edit_find_callback(NULL, NULL, NULL);
  return(XEN_FALSE);
}

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_edit_find_dialog_w, g_edit_find_dialog)
XEN_NARGIFY_0(g_find_dialog_widgets_w, g_find_dialog_widgets)
#else
#define g_edit_find_dialog_w g_edit_find_dialog
#define g_find_dialog_widgets_w g_find_dialog_widgets
#endif

void g_init_gxfind(void)
{
  XEN_DEFINE_PROCEDURE("edit-find-dialog", g_edit_find_dialog_w, 0, 0, 0, "");
  XEN_DEFINE_PROCEDURE("find-dialog-widgets", g_find_dialog_widgets_w, 0, 0, 0, "");
}

#endif
