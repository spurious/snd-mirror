#include "snd.h"

/* -------- edit find -------- */

static Widget edit_find_dialog, edit_find_text, cancelB, edit_find_label, findnextB;

static Widget find_error_frame = NULL, find_error_label = NULL;

static void clear_find_error(void);
static void edit_find_modify_callback(Widget w, XtPointer context, XtPointer info)
{
  clear_find_error();
}

static void clear_find_error(void)
{
  if ((find_error_frame) && (XtIsManaged(find_error_frame)))
    XtUnmanageChild(find_error_frame);
  XtRemoveCallback(edit_find_text, XmNmodifyVerifyCallback, edit_find_modify_callback, NULL);
  /* squeezing out the error label room here moves the text widget, which is irritating since it
   *   means the text we're typing gets lost 
   */
}

static void errors_to_find_text(const char *msg, void *data)
{
  Dimension find_height = 0;
  int lines = 0;
  XmString label;
  set_find_dialog_label("error");
  label = multi_line_label(msg, &lines);
  XtVaSetValues(find_error_label, 
		XmNlabelString, label, 
		XmNheight, lines * 20,
		NULL);
  XtVaSetValues(find_error_frame, XmNheight, lines * 20, NULL);
  XtVaGetValues(edit_find_dialog, XmNheight, &find_height, NULL);
  if (find_height < (lines * 20 + 140))
    {
      XtUnmanageChild(edit_find_dialog);
      XtVaSetValues(edit_find_dialog, XmNheight, 140 + 20 * lines, NULL);
      XtManageChild(edit_find_dialog);
    }
  XmStringFree(label);
  XtManageChild(find_error_frame);
  XtAddCallback(edit_find_text, XmNmodifyVerifyCallback, edit_find_modify_callback, NULL);
}

static void stop_search_if_error(const char *msg, void *data)
{
  errors_to_find_text(msg, data);
  ss->stopped_explicitly = true; /* should be noticed in global_search in snd-find.c */
}

static void edit_find_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  find_dialog_help();
} 

static void edit_find_ok_callback(read_direction_t direction, Widget w, XtPointer context, XtPointer info)
{ /* "Find" is the label here */
  char *str = NULL, *buf = NULL;
  XmString s1;
  XEN proc;
  str = XmTextGetString(edit_find_text);
  if ((str) && (*str))
    { 
      clear_global_search_procedure(true);
      ss->search_expr = copy_string(str);
      redirect_errors_to(errors_to_find_text, NULL);
      proc = snd_catch_any(eval_str_wrapper, str, str);
      redirect_errors_to(NULL, NULL);
      if ((XEN_PROCEDURE_P(proc)) && (procedure_arity_ok(proc, 1)))
	{
	  ss->search_proc = proc;
	  ss->search_proc_loc = snd_protect(proc);
#if WITH_RUN
	  if (optimization(ss) > 0)
	    ss->search_tree = form_to_ptree_1_b_without_env(C_STRING_TO_XEN_FORM(str));
#endif
	  buf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
	  mus_snprintf(buf, PRINT_BUFFER_SIZE, _("find: %s"), str);
	  set_label(edit_find_label, buf);
	  /* XmTextSetString(edit_find_text, NULL); */
	  FREE(buf);
	}
    }
  else
    {
      if (ss->search_expr == NULL)
	{
	  /* using global search_proc set by user */
	  buf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
	  mus_snprintf(buf, PRINT_BUFFER_SIZE, _("find: %s"), XEN_AS_STRING(ss->search_proc));
	  set_label(edit_find_label, buf);
	  /* XmTextSetString(edit_find_text, NULL); */
	  FREE(buf);
	}
    }
  if (str) XtFree(str);
  if ((XEN_PROCEDURE_P(ss->search_proc)) || (ss->search_tree))
    {
      s1 = XmStringCreateLocalized(_("Stop"));
      XtVaSetValues(cancelB, XmNlabelString, s1, NULL);
      XmStringFree(s1);
      redirect_xen_error_to(stop_search_if_error, NULL);
      str = global_search(direction);
      redirect_xen_error_to(NULL, NULL);
      s1 = XmStringCreateLocalized(_("Dismiss"));
      XtVaSetValues(cancelB, XmNlabelString, s1, NULL);
      XmStringFree(s1);
      if ((str) && (*str)) set_label(edit_find_label, str);
    }
} 

void set_find_dialog_label(const char *str) {if (edit_find_label) set_label(edit_find_label, str);}
static void edit_find_next_callback(Widget w, XtPointer context, XtPointer info) {edit_find_ok_callback(READ_FORWARD, w, context, info);}
static void edit_find_previous_callback(Widget w, XtPointer context, XtPointer info) {edit_find_ok_callback(READ_BACKWARD, w, context, info);}

static void find_dialog_close(Widget w, XtPointer context, XtPointer info)
{
  clear_find_error();
}

static void edit_find_cancel_callback(Widget w, XtPointer context, XtPointer info)
{
  if (XmGetFocusWidget(edit_find_dialog) == XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_OK_BUTTON))
    {
      if (ss->checking_explicitly)
	ss->stopped_explicitly = true;
      else 
	{
	  XtUnmanageChild(edit_find_dialog);
	  clear_find_error();
	}
    }
  else edit_find_next_callback(w, context, info);
} 

static void make_edit_find_dialog(bool managed)
{
  if (!edit_find_dialog)
    {
      Widget dl, rc;
      Arg args[20];
      int n;
      XmString xmstr1, xmstr3, titlestr;

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      xmstr1 = XmStringCreateLocalized(_("Dismiss"));
      xmstr3 = XmStringCreateLocalized(_("Previous"));
      titlestr = XmStringCreateLocalized(_("Find"));
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
      XtSetArg(args[n], XmNbackground, ss->sgx->doit_button_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;
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
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, edit_find_text); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      edit_find_label = XtCreateManagedWidget("global search", xmLabelWidgetClass, rc, args, n);
      
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, edit_find_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 2); n++;
      find_error_frame = XtCreateManagedWidget("find-error-frame", xmFrameWidgetClass, rc, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      find_error_label = XtCreateManagedWidget("", xmLabelWidgetClass, find_error_frame, args, n);
      
      map_over_children(edit_find_dialog, set_main_color_of_widget);
      XtVaSetValues(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_OK_BUTTON), XmNbackground, ss->sgx->quit_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->doit_again_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_HELP_BUTTON), XmNbackground, ss->sgx->help_button_color, NULL);

      cancelB = XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_OK_BUTTON);
      set_dialog_widget(FIND_DIALOG, edit_find_dialog);

      XtUnmanageChild(find_error_frame);

      if (managed) XtManageChild(edit_find_dialog);

      {
	Atom wm_delete_window;
	wm_delete_window = XmInternAtom(MAIN_DISPLAY(ss), "WM_DELETE_WINDOW", false);
	XmAddWMProtocolCallback(XtParent(edit_find_dialog), wm_delete_window, find_dialog_close, NULL);
      }
    }
  else
    {
      if (managed)
	{
	  if (!XtIsManaged(edit_find_dialog)) XtManageChild(edit_find_dialog);
	  raise_dialog(edit_find_dialog);
	}
    }
}

void edit_find_callback(Widget w, XtPointer context, XtPointer info)
{
  make_edit_find_dialog(true);
}

void save_find_dialog_state(FILE *fd)
{
  if ((edit_find_dialog) && (XtIsManaged(edit_find_dialog)))
    {
      char *text = NULL;
      text = XmTextGetString(edit_find_text);
      if ((text) && (*text))
	{
#if HAVE_SCHEME
	  fprintf(fd, "(%s #t \"%s\")\n", S_find_dialog, text);
#endif
#if HAVE_RUBY
	  fprintf(fd, "%s(true, \"%s\")\n", TO_PROC_NAME(S_find_dialog), text);
#endif
#if HAVE_FORTH
	  fprintf(fd, "#t \"%s\" %s drop\n", text, S_find_dialog);
#endif
	  XtFree(text);
	}
      else 
	{
#if HAVE_SCHEME
	  if (ss->search_expr)
	    fprintf(fd, "(%s #t \"%s\")\n", S_find_dialog, ss->search_expr);
	  else fprintf(fd, "(%s #t)\n", S_find_dialog);
#endif
#if HAVE_RUBY
	  if (ss->search_expr)
	    fprintf(fd, "%s(true, \"%s\")\n", TO_PROC_NAME(S_find_dialog), ss->search_expr);
	  else fprintf(fd, "%s(true)\n", TO_PROC_NAME(S_find_dialog));
#endif
#if HAVE_FORTH
	  if (ss->search_expr)
	    fprintf(fd, "#t \"%s\" %s drop\n", ss->search_expr, S_find_dialog);
	  else fprintf(fd, "#t %s drop\n", S_find_dialog);
#endif
	}
    }
}

static XEN g_find_dialog(XEN managed, XEN text)
{
  #define H_find_dialog "(" S_find_dialog " :optional managed text): create and activate the Edit:Find dialog, return the dialog widget. \
If 'text' is included, it is preloaded into the find dialog text widget."
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ARG_1, S_find_dialog, "a boolean");
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(text), text, XEN_ARG_2, S_find_dialog, "a string");
  make_edit_find_dialog(XEN_TO_C_BOOLEAN(managed));
  if ((edit_find_text) && (XEN_STRING_P(text)))
    XmTextSetString(edit_find_text, XEN_TO_C_STRING(text));
  return(XEN_WRAP_WIDGET(edit_find_dialog));
}

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

#ifdef XEN_ARGIFY_1
XEN_ARGIFY_2(g_find_dialog_w, g_find_dialog)
XEN_NARGIFY_0(g_find_dialog_widgets_w, g_find_dialog_widgets)
#else
#define g_find_dialog_w g_find_dialog
#define g_find_dialog_widgets_w g_find_dialog_widgets
#endif

void g_init_gxfind(void)
{
  XEN_DEFINE_PROCEDURE(S_find_dialog, g_find_dialog_w, 0, 2, 0, H_find_dialog);
  XEN_DEFINE_PROCEDURE("find-dialog-widgets", g_find_dialog_widgets_w, 0, 0, 0, "internal auto-test function");
}

