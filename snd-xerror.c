#include "snd.h"

/* TODO: replace error dialog popup */
void post_error_dialog(char *msg)
{
  post_it("Error", msg);
}


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
  

/* ---------------- POST-IT MONOLOG ---------------- */

#define POST_IT_ROWS 12
#define POST_IT_COLUMNS 56

static Widget post_it_dialog = NULL;
static Widget post_it_text = NULL;

static void create_post_it_monolog(void)
{
  /* create scrollable but not editable text window; used for fft peaks, sp info, and raw data help displays */
  Arg args[20];
  int n;

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
  XtSetArg(args[n], XmNnoResize, false); n++;
  XtSetArg(args[n], XmNtransient, false); n++;
  post_it_dialog = XmCreateMessageDialog(MAIN_PANE(ss), "info", args, n);

  XtUnmanageChild(XmMessageBoxGetChild(post_it_dialog, XmDIALOG_CANCEL_BUTTON));
  XtUnmanageChild(XmMessageBoxGetChild(post_it_dialog, XmDIALOG_HELP_BUTTON));
  XtUnmanageChild(XmMessageBoxGetChild(post_it_dialog, XmDIALOG_SYMBOL_LABEL));

  if (!(ss->using_schemes))
    XtVaSetValues(XmMessageBoxGetChild(post_it_dialog, XmDIALOG_MESSAGE_LABEL), XmNbackground, ss->sgx->help_button_color, NULL);
      
  n = 0;
  XtSetArg(args[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;
  XtSetArg(args[n], XmNeditable, false); n++;
  XtSetArg(args[n], XmNcolumns, POST_IT_COLUMNS); n++;
  XtSetArg(args[n], XmNrows, POST_IT_ROWS); n++;
  if (!(ss->using_schemes))
    {
      XtSetArg(args[n], XmNforeground, ss->sgx->black); n++; /* needed if color allocation fails completely */
      XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
    }
  post_it_text = XmCreateScrolledText(post_it_dialog, "post-it-text", args, n);
  XtManageChild(post_it_text);
  XtManageChild(post_it_dialog);

  if (!(ss->using_schemes))
    {
      map_over_children(post_it_dialog, set_main_color_of_widget, NULL);
      XtVaSetValues(post_it_text, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
      XtVaSetValues(XmMessageBoxGetChild(post_it_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(post_it_dialog, XmDIALOG_OK_BUTTON), XmNbackground, ss->sgx->quit_button_color, NULL);
    }
  set_dialog_widget(POST_IT_DIALOG, post_it_dialog);
}

widget_t post_it(const char *subject, const char *str)
{
  /* place string in scrollable help window */
  XmString xstr1, xstr2;
  if ((ss == NULL) || (ss->sgx == NULL)) return(NULL); /* an attempt to call this before X/Motif is ready */
  if (!(post_it_dialog)) 
    create_post_it_monolog(); 
  else raise_dialog(post_it_dialog);
  xstr1 = XmStringCreate((char *)subject, XmFONTLIST_DEFAULT_TAG);
  xstr2 = XmStringCreate((char *)subject, XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(post_it_dialog, 
		XmNmessageString, xstr1, 
		XmNdialogTitle, xstr2,
		NULL);
  XmTextSetString(post_it_text, (char *)str);
  if (!XtIsManaged(post_it_dialog)) 
    XtManageChild(post_it_dialog);
  XmStringFree(xstr1);
  XmStringFree(xstr2);
  return(post_it_dialog);
}

void save_post_it_dialog_state(FILE *fd)
{
  if ((post_it_dialog) && (XtIsManaged(post_it_dialog)))
    {
      char *subject, *text;
      XmString title;
      text = XmTextGetString(post_it_text);
      XtVaGetValues(post_it_dialog, XmNdialogTitle, &title, NULL);
      subject = (char *)XmStringUnparse(title, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
#if HAVE_SCHEME
      fprintf(fd, "(%s \"%s\" \"%s\")\n", S_info_dialog, subject, text);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(\"%s\", \"%s\")\n", TO_PROC_NAME(S_info_dialog), subject, text);
#endif
      if (subject) XtFree(subject);
      if (text) XtFree(text);
    }
}


