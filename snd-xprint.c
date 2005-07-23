#include "snd.h"

/* X side of file print */

static Widget file_print_dialog = NULL;
static Widget file_print_name = NULL;
static Widget file_print_eps_or_lpr = NULL;
static char print_string[PRINT_BUFFER_SIZE];
static Widget error_info_box, error_info_frame, error_info;

static void file_print_help_callback(Widget w, XtPointer context, XtPointer info)
{
  print_dialog_help();
}

static void clear_error(void);

static void file_print_cancel_callback(Widget w, XtPointer context, XtPointer info)
{
  ss->print_choice = PRINT_SND;
  clear_error();
  XtUnmanageChild(file_print_dialog);
}

static int lpr (char *name)
{
  /* make some desultory effort to print the file */
  mus_snprintf(print_string, PRINT_BUFFER_SIZE, "lpr %s", name);
  return(system(print_string));
}

static void watch_print(Widget w, XtPointer context, XtPointer info)
{
  clear_error();
}

static Widget rc;
static bool print_watching = false, print_error = false;
static void clear_error(void)
{
  XtUnmanageChild(rc);
  XtUnmanageChild(error_info_box);
  XtVaSetValues(file_print_eps_or_lpr, XmNbottomAttachment, XmATTACH_FORM, NULL);
  XtManageChild(rc);
  print_error = false;
  if (print_watching)
    {
      print_watching = false;
      XtRemoveCallback(file_print_name, XmNvalueChangedCallback, watch_print, NULL);
      XtRemoveCallback(file_print_name, XmNvalueChangedCallback, watch_print, NULL);
    }
}

static void report_in_error_info(const char *msg, void *ignore)
{
  XmString s1;
  print_error = true;
  s1 = XmStringCreate((char *)msg, XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(error_info, XmNlabelString, s1, NULL);
  XmStringFree(s1);
  if (!(XtIsManaged(error_info_box)))
    {
      XtUnmanageChild(rc);
      XtVaSetValues(file_print_eps_or_lpr, XmNbottomAttachment, XmATTACH_NONE, NULL);
      XtManageChild(error_info_box);
      XtManageChild(rc);
      print_watching = true;
      XtAddCallback(file_print_name, XmNvalueChangedCallback, watch_print, NULL);
      XtAddCallback(file_print_eps_or_lpr, XmNvalueChangedCallback, watch_print, NULL);
    }
}

static printing_t printing = NOT_PRINTING;

static void file_print_ok_callback(Widget w, XtPointer context, XtPointer info)
{
  bool quit = false;
  XmString plab, slab;
  snd_info *nsp = NULL;
  if (printing) 
    ss->stopped_explicitly = true;
  else
    {
      bool print_it;
      char *str = NULL;
      if (ss->print_choice == PRINT_SND)
	{
	  plab = XmStringCreate(_("Stop"), XmFONTLIST_DEFAULT_TAG);
	  nsp = any_selected_sound();
	  mus_snprintf(print_string, PRINT_BUFFER_SIZE, _("printing %s"), nsp->short_filename);
	  slab = XmStringCreate(print_string, XmFONTLIST_DEFAULT_TAG);
	  XtVaSetValues(file_print_dialog, 
			XmNokLabelString, plab, 
			XmNmessageString, slab, 
			NULL);
	  XmStringFree(plab);
	  XmStringFree(slab);
	}
      printing = PRINTING;
      print_it = (bool)XmToggleButtonGetState(file_print_eps_or_lpr);
      quit = (ss->print_choice == PRINT_ENV);
      if (print_it)
	{
	  int err = 0;
	  char *name;
	  name = snd_tempnam();

	  redirect_snd_error_to(report_in_error_info, NULL);
	  switch (ss->print_choice)
	    {
	    case PRINT_SND: snd_print(name); break;
	    case PRINT_ENV: enved_print(name); break;
	    }
	  redirect_snd_error_to(NULL, NULL);
	  if (!print_error)
	    {
	      err = lpr(name); /* lpr apparently insists on printing to stderr? */
	      if (err != 0)
		report_in_error_info(_("can't print!"), NULL);
	      snd_remove(name, IGNORE_CACHE);
	    }
	  FREE(name);
	}
      else 
	{
	  redirect_snd_error_to(report_in_error_info, NULL);
	  switch (ss->print_choice)
	    {
	    case PRINT_SND: snd_print(str = XmTextGetString(file_print_name)); break;
	    case PRINT_ENV: enved_print(str = XmTextGetString(file_print_name)); break;
	    }
	  redirect_snd_error_to(NULL, NULL);
	  if (str) XtFree(str);
	}
    }
  printing = NOT_PRINTING;
  if (ss->print_choice == PRINT_SND)
    {
      plab = XmStringCreate(_("Print"), XmFONTLIST_DEFAULT_TAG);
      mus_snprintf(print_string, PRINT_BUFFER_SIZE, _("print %s"), nsp->short_filename);
      slab = XmStringCreate(print_string, XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(file_print_dialog, 
		    XmNokLabelString, plab, 
		    XmNmessageString, slab, 
		    NULL);
      XmStringFree(plab);
      XmStringFree(slab);
    }
  ss->print_choice = PRINT_SND;
  if (quit) 
    XtUnmanageChild(file_print_dialog);
}

static void start_file_print_dialog(XmString xmstr4, bool managed)
{
  if (!file_print_dialog)
    {
      Widget dl;
      XmString xmstr1, xmstr2, xmstr3, titlestr;
      Arg args[20];
      int n;

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      xmstr1 = XmStringCreate(_("Print"), XmFONTLIST_DEFAULT_TAG);  /* "ok" here is confusing -- might mean, ok I'm done */
      xmstr2 = XmStringCreate(_("Help"), XmFONTLIST_DEFAULT_TAG);
      xmstr3 = XmStringCreate(_("Dismiss"), XmFONTLIST_DEFAULT_TAG);
      titlestr = XmStringCreate(_("Print"), XmFONTLIST_DEFAULT_TAG);

      XtSetArg(args[n], XmNmessageString, xmstr4); n++;
      XtSetArg(args[n], XmNokLabelString, xmstr1); n++;
      XtSetArg(args[n], XmNhelpLabelString, xmstr2); n++;
      XtSetArg(args[n], XmNcancelLabelString, xmstr3); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      file_print_dialog = XmCreateMessageDialog(MAIN_PANE(ss), _("eps file:"), args, n);

      XmStringFree(xmstr1);
      XmStringFree(xmstr2);
      XmStringFree(xmstr3);
      XmStringFree(titlestr);
      XtUnmanageChild(XmMessageBoxGetChild(file_print_dialog, XmDIALOG_SYMBOL_LABEL));
      XtAddCallback(file_print_dialog, XmNhelpCallback, file_print_help_callback, NULL);
      XtAddCallback(file_print_dialog, XmNcancelCallback, file_print_cancel_callback, NULL);
      XtAddCallback(file_print_dialog, XmNokCallback, file_print_ok_callback, NULL);

      n = 0;
      rc = XtCreateManagedWidget("form", xmFormWidgetClass, file_print_dialog, args, n);

      n = 0;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      dl = XtCreateManagedWidget(_("eps file:"), xmLabelWidgetClass, rc, args, n);

      n = 0;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, dl); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNvalue, eps_file(ss)); n++;
      file_print_name = make_textfield_widget("text", rc, args, n, NOT_ACTIVATABLE, NO_COMPLETER);

      n = 0;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, file_print_name); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      file_print_eps_or_lpr = make_togglebutton_widget(_("direct to printer"), rc, args, n);

      /* error display */

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, file_print_eps_or_lpr); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, true); n++; 

      error_info_box = XtCreateWidget("error-box", xmRowColumnWidgetClass, rc, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;}
      XtSetArg(args[n], XmNmarginHeight, 4); n++;
      error_info_frame = XtCreateManagedWidget("error-frame", xmFrameWidgetClass, error_info_box, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      error_info = XtCreateManagedWidget("error-info", xmLabelWidgetClass, error_info_frame, args, n);

      XtVaSetValues(file_print_eps_or_lpr, XmNbottomAttachment, XmATTACH_FORM, NULL);

      if (managed) XtManageChild(file_print_dialog);

      if (!(ss->using_schemes))	
	{
	  map_over_children(file_print_dialog, set_main_color_of_widget, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(file_print_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(file_print_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(file_print_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(file_print_dialog, XmDIALOG_OK_BUTTON), XmNbackground, ss->sgx->doit_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(file_print_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->quit_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(file_print_dialog, XmDIALOG_HELP_BUTTON), XmNbackground, ss->sgx->help_button_color, NULL);
	  XtVaSetValues(file_print_eps_or_lpr, XmNselectColor, ss->sgx->pushed_button_color, NULL);
	}
      set_dialog_widget(PRINT_DIALOG, file_print_dialog);
    }
  else
    {
      XtVaSetValues(file_print_dialog, XmNmessageString, xmstr4, NULL);
      if (managed)
	{
	  if (!XtIsManaged(file_print_dialog))
	    XtManageChild(file_print_dialog);
	  raise_dialog(file_print_dialog); /* a no-op unless already managed */
	}
    }
}

widget_t make_file_print_dialog(bool managed, bool direct_to_printer)
{
  XmString xmstr4;
  xmstr4 = XmStringCreate("print", XmFONTLIST_DEFAULT_TAG);
  start_file_print_dialog(xmstr4, managed);
  XmStringFree(xmstr4);
  XmToggleButtonSetState(file_print_eps_or_lpr, direct_to_printer, false);
  return(file_print_dialog);
}

void file_print_callback(Widget w, XtPointer context, XtPointer info)
{
  XmString xmstr4;
  if (ss->print_choice == PRINT_SND)
    {
      snd_info *nsp;
      nsp = any_selected_sound();
      if (!nsp) return;
      mus_snprintf(print_string, PRINT_BUFFER_SIZE, _("print %s"), nsp->short_filename);
      xmstr4 = XmStringCreate(print_string, XmFONTLIST_DEFAULT_TAG);
    }
  else xmstr4 = XmStringCreate(_("print env"), XmFONTLIST_DEFAULT_TAG);
  start_file_print_dialog(xmstr4, true);
  XmStringFree(xmstr4);
}

void save_print_dialog_state(FILE *fd)
{
  if ((file_print_dialog) && (XtIsManaged(file_print_dialog)))
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s #t %s)\n", S_print_dialog, ((bool)(XmToggleButtonGetState(file_print_eps_or_lpr))) ? "#t" : "#f");
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(true, %s)\n", TO_PROC_NAME(S_print_dialog), ((bool)(XmToggleButtonGetState(file_print_eps_or_lpr))) ? "true" : "false");
#endif
    }
}
