#include "snd.h"

/* X side of file print */

static Widget file_print_dialog = NULL;
static Widget file_print_name = NULL;
static Widget file_print_eps_or_lpr = NULL;
static char print_string[PRINT_BUFFER_SIZE];

static void file_print_help_callback(Widget w, XtPointer context, XtPointer info)
{
  print_dialog_help();
}

static void file_print_cancel_callback(Widget w, XtPointer context, XtPointer info)
{
  ss->print_choice = PRINT_SND;
  XtUnmanageChild(file_print_dialog);
}

static int lpr (char *name)
{
  /* make some desultory effort to print the file */
  mus_snprintf(print_string, PRINT_BUFFER_SIZE, "lpr %s", name);
  return(system(print_string));
}

static bool printing = false;

static void file_print_ok_callback(Widget w, XtPointer context, XtPointer info)
{
  bool print_it, quit = false;
  int err = 0;
  char *name, *str = NULL;
  XmString plab, slab;
  snd_info *nsp = NULL;
  if (printing) 
    ss->stopped_explicitly = true;
  else
    {
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
      printing = true;
      print_it = (bool)XmToggleButtonGetState(file_print_eps_or_lpr);
      quit = (ss->print_choice == PRINT_ENV);
      if (print_it)
	{
	  name = snd_tempnam();
	  switch (ss->print_choice)
	    {
	    case PRINT_SND: snd_print(name); break;
	    case PRINT_ENV: enved_print(name); break;
	    }
	  err = lpr(name);
	  if ((err != 0) && (nsp)) report_in_minibuffer(nsp, _("can't print!"));
	  /* tried to redirect stderr here and pick it up afterwards, to no avail */
	  snd_remove(name, IGNORE_CACHE);
	  FREE(name);
	}
      else 
	{
	  switch (ss->print_choice)
	    {
	    case PRINT_SND: snd_print(str = XmTextGetString(file_print_name)); break;
	    case PRINT_ENV: enved_print(str = XmTextGetString(file_print_name)); break;
	    }
	  if (str) XtFree(str);
	}
    }
  printing = false;
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

void file_print_callback(Widget w, XtPointer context, XtPointer info)
{
  Arg args[20];
  int n;
  Widget dl, rc;
  XmString xmstr1, xmstr2, xmstr3, xmstr4, titlestr;
  snd_info *nsp;
  if (ss->print_choice == PRINT_SND)
    {
      nsp = any_selected_sound();
      if (!nsp) return;
      mus_snprintf(print_string, PRINT_BUFFER_SIZE, _("print %s"), nsp->short_filename);
      xmstr4 = XmStringCreate(print_string, XmFONTLIST_DEFAULT_TAG);
    }
  else xmstr4 = XmStringCreate(_("print env"), XmFONTLIST_DEFAULT_TAG);

  if (!file_print_dialog)
    {
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
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
      file_print_dialog = XmCreateMessageDialog(w, _("eps file:"), args, n);

      XmStringFree(xmstr1);
      XmStringFree(xmstr2);
      XmStringFree(xmstr3);
      XmStringFree(xmstr4);
      XmStringFree(titlestr);
      XtUnmanageChild(XmMessageBoxGetChild(file_print_dialog, XmDIALOG_SYMBOL_LABEL));
      XtAddCallback(file_print_dialog, XmNhelpCallback, file_print_help_callback, NULL);
      XtAddCallback(file_print_dialog, XmNcancelCallback, file_print_cancel_callback, NULL);
      XtAddCallback(file_print_dialog, XmNokCallback, file_print_ok_callback, NULL);

      rc = XtCreateManagedWidget("form", xmFormWidgetClass, file_print_dialog, NULL, 0);

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

      XtManageChild(file_print_dialog);

      if (!(ss->using_schemes))	
	{
	  map_over_children(file_print_dialog, set_main_color_of_widget, (void *)context);
	  XtVaSetValues(XmMessageBoxGetChild(file_print_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(file_print_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(file_print_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(file_print_eps_or_lpr, XmNselectColor, (ss->sgx)->pushed_button_color, NULL);
	}
      set_dialog_widget(PRINT_DIALOG, file_print_dialog);
    }
  else
    {
      XtVaSetValues(file_print_dialog, XmNmessageString, xmstr4, NULL);
      XmStringFree(xmstr4);
      raise_dialog(file_print_dialog);
    }
  if (!XtIsManaged(file_print_dialog)) 
    XtManageChild(file_print_dialog);
}
