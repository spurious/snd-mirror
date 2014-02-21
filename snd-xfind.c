#include "snd.h"

static Widget edit_find_dialog, edit_find_text, cancelB, edit_find_label, previousB;
static Widget find_error_frame = NULL, find_error_label = NULL;
static chan_info *find_channel = NULL; /* sigh */


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


void find_dialog_stop_label(bool show_stop)
{
  XmString s1;
  if (show_stop)
    s1 = XmStringCreateLocalized((char *)I_STOP);
  else s1 = XmStringCreateLocalized((char *)I_GO_AWAY);
  XtVaSetValues(cancelB, XmNlabelString, s1, NULL);
  XmStringFree(s1);
}


void errors_to_find_text(const char *msg, void *data)
{
  Dimension find_height = 0;
  int lines = 0;
  XmString label;
  find_dialog_set_label("error");
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


void stop_search_if_error(const char *msg, void *data)
{
  errors_to_find_text(msg, data);
  ss->stopped_explicitly = true; /* should be noticed in global_search in snd-find.c */
}


static void edit_find_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  find_dialog_help();
} 


static void edit_find_ok_callback(read_direction_t direction, Widget w, XtPointer context, XtPointer info)
{ 
  char *str;
  str = XmTextGetString(edit_find_text);
#if HAVE_EXTENSION_LANGUAGE
  find_dialog_find(str, direction, find_channel);
#endif
  if (str) free(str);
}


void find_dialog_set_label(const char *str) 
{
  if (edit_find_label) 
    set_label(edit_find_label, str);
}


static void edit_find_next_callback(Widget w, XtPointer context, XtPointer info) 
{
  edit_find_ok_callback(READ_FORWARD, w, context, info);
}


static void edit_find_previous_callback(Widget w, XtPointer context, XtPointer info) 
{
  edit_find_ok_callback(READ_BACKWARD, w, context, info);
}


static void find_dialog_close(Widget w, XtPointer context, XtPointer info)
{
  clear_find_error();
}


static void edit_find_cancel_callback(Widget w, XtPointer context, XtPointer info)
{
  if (ss->checking_explicitly)
    ss->stopped_explicitly = true;
  else 
    {
      XtUnmanageChild(edit_find_dialog);
      clear_find_error();
    }
} 


static void make_edit_find_dialog(bool managed, chan_info *cp)
{
  find_channel = cp;

  if (!edit_find_dialog)
    {
      Widget dl, rc;
      Arg args[20];
      int n;
      XmString go_away, next;

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;

      go_away = XmStringCreateLocalized((char *)I_GO_AWAY);
      next = XmStringCreateLocalized((char *)I_NEXT);

      XtSetArg(args[n], XmNokLabelString, next); n++;
      XtSetArg(args[n], XmNcancelLabelString, go_away); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNtransient, false); n++;
      edit_find_dialog = XmCreateMessageDialog(MAIN_SHELL(ss), (char *)I_FIND, args, n);
      
      XmStringFree(go_away);
      XmStringFree(next);
      
      XtUnmanageChild(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_SYMBOL_LABEL));
      XtUnmanageChild(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_MESSAGE_LABEL));
      
      XtAddCallback(edit_find_dialog, XmNhelpCallback, edit_find_help_callback, NULL);
      XtAddCallback(edit_find_dialog, XmNcancelCallback, edit_find_cancel_callback, NULL);
      XtAddCallback(edit_find_dialog, XmNokCallback, edit_find_next_callback, NULL);
      
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->selection_color); n++;
      previousB = XtCreateManagedWidget(I_PREVIOUS, xmPushButtonGadgetClass, edit_find_dialog, args, n);
      XtAddCallback(previousB, XmNactivateCallback, edit_find_previous_callback, NULL);
      
      rc = XtCreateManagedWidget("row", xmFormWidgetClass, edit_find_dialog, NULL, 0);
      
      n = 0;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      dl = XtCreateManagedWidget(I_find, xmLabelWidgetClass, rc, args, n);
      
      n = 0;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, dl); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      edit_find_text = make_textfield_widget("text", rc, args, n, ACTIVATABLE, add_completer_func(expression_completer, NULL));
      
      n = 0;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, edit_find_text); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNmarginHeight, 10); n++;
      edit_find_label = XtCreateManagedWidget("    ", xmLabelWidgetClass, rc, args, n);
      
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      find_error_label = XtCreateManagedWidget("", xmLabelWidgetClass, find_error_frame, args, n);
      
      map_over_children(edit_find_dialog, set_main_color_of_widget);
      XtVaSetValues(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, ss->selection_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, ss->selection_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, ss->selection_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_OK_BUTTON), XmNbackground, ss->highlight_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->highlight_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_HELP_BUTTON), XmNbackground, ss->highlight_color, NULL);

      cancelB = XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_CANCEL_BUTTON);
      set_dialog_widget(FIND_DIALOG, edit_find_dialog);

      XtUnmanageChild(find_error_frame);
      if (managed) XtManageChild(edit_find_dialog);
      {
	Atom wm_delete_window;
	wm_delete_window = XmInternAtom(MAIN_DISPLAY(ss), (char *)"WM_DELETE_WINDOW", false);
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

  {
    XmString titlestr;
    if (cp)
      titlestr = XmStringCreateLocalized(mus_format("%s in %s channel %d", (char *)I_FIND, cp->sound->short_filename, cp->chan));
    else titlestr = XmStringCreateLocalized((char *)I_FIND);
    XtVaSetValues(edit_find_dialog, XmNdialogTitle, titlestr, NULL);
    XmStringFree(titlestr);
  }
}


void edit_find_callback(Widget w, XtPointer context, XtPointer info)
{
  make_edit_find_dialog(true, NULL);
}


void find_dialog(chan_info *cp)
{
  make_edit_find_dialog(true, cp);
}


bool find_dialog_is_active(void)
{
  return((edit_find_dialog) && (XtIsManaged(edit_find_dialog)));
}


void save_find_dialog_state(FILE *fd)
{
  if (find_dialog_is_active())
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

  XEN_ASSERT_TYPE(Xen_is_boolean_or_unbound(managed), managed, 1, S_find_dialog, "a boolean");
  XEN_ASSERT_TYPE(Xen_is_string_or_unbound(text), text, 2, S_find_dialog, "a string");

  make_edit_find_dialog(XEN_TO_C_BOOLEAN(managed), NULL);
  if ((edit_find_text) && (XEN_STRING_P(text)))
    XmTextSetString(edit_find_text, (char *)XEN_TO_C_STRING(text));

  return(XEN_WRAP_WIDGET(edit_find_dialog));
}


static XEN g_find_dialog_widgets(void)
{
  if (edit_find_dialog)
    return(XEN_CONS(XEN_WRAP_WIDGET(edit_find_dialog),
	     XEN_CONS(XEN_WRAP_WIDGET(edit_find_text),
  	       XEN_CONS(XEN_WRAP_WIDGET(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_OK_BUTTON)),           /* find next */
		 XEN_CONS(XEN_WRAP_WIDGET(previousB),                                                          /* find previous */
		   XEN_CONS(XEN_WRAP_WIDGET(XmMessageBoxGetChild(edit_find_dialog, XmDIALOG_CANCEL_BUTTON)),   /* go away */
		     XEN_EMPTY_LIST))))));
  return(XEN_EMPTY_LIST);
}


XEN_ARGIFY_2(g_find_dialog_w, g_find_dialog)
XEN_NARGIFY_0(g_find_dialog_widgets_w, g_find_dialog_widgets)

void g_init_gxfind(void)
{
  XEN_DEFINE_PROCEDURE(S_find_dialog, g_find_dialog_w, 0, 2, 0, H_find_dialog);
  XEN_DEFINE_PROCEDURE("find-dialog-widgets", g_find_dialog_widgets_w, 0, 0, 0, "internal auto-test function");
}


