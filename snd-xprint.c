#include "snd.h"

/* X side of file print */

static Widget file_print_dialog = NULL;
static Widget file_print_name = NULL;
static Widget file_print_eps_or_lpr = NULL;
static char print_string[PRINT_BUFFER_SIZE];

static void file_print_help_callback(Widget w, XtPointer context, XtPointer info)
{
  print_dialog_help((snd_state *)context);
}

static void file_print_cancel_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_state *ss = (snd_state *)context;
  ss->print_choice = PRINT_SND;
  XtUnmanageChild(file_print_dialog);
}

static int lpr (char *name)
{
  /* make some desultory effort to print the file */
  mus_snprintf(print_string, PRINT_BUFFER_SIZE, "lpr %s", name);
  return(system(print_string));
}

static int printing = 0;

static void file_print_ok_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_state *ss = (snd_state *)context;
  int print_it, quit = 0, err = 0;
  char *name, *str = NULL;
  XmString plab, slab;
  snd_info *nsp = NULL;
  if (printing) 
    ss->stopped_explicitly = 1;
  else
    {
      if (ss->print_choice == PRINT_SND)
	{
	  plab = XmStringCreate(STR_Stop, XmFONTLIST_DEFAULT_TAG);
	  nsp = any_selected_sound(ss);
	  mus_snprintf(print_string, PRINT_BUFFER_SIZE, "printing %s", nsp->shortname);
	  slab = XmStringCreate(print_string, XmFONTLIST_DEFAULT_TAG);
	  XtVaSetValues(file_print_dialog, 
			XmNokLabelString, plab, 
			XmNmessageString, slab, 
			NULL);
	  XmStringFree(plab);
	  XmStringFree(slab);
	}
      printing = 1;
      print_it = XmToggleButtonGetState(file_print_eps_or_lpr);
      quit = (ss->print_choice == PRINT_ENV);
      if (print_it)
	{
	  name = snd_tempnam(ss);
	  switch (ss->print_choice)
	    {
	    case PRINT_SND: snd_print(ss, name); break;
	    case PRINT_ENV: enved_print(name); break;
	    }
	  err = lpr(name);
	  if ((err != 0) && (nsp)) report_in_minibuffer(nsp, "can't print!");
	  /* tried to redirect stderr here and pick it up afterwards, to no avail */
	  if (remove(name) == -1)
	    snd_error("can't remove %s: %s", name, strerror(errno));
	  FREE(name);
	}
      else 
	{
	  switch (ss->print_choice)
	    {
	    case PRINT_SND: snd_print(ss, str = XmTextGetString(file_print_name)); break;
	    case PRINT_ENV: enved_print(str = XmTextGetString(file_print_name)); break;
	    }
	  if (str) XtFree(str);
	}
    }
  printing = 0;
  if (ss->print_choice == PRINT_SND)
    {
      plab = XmStringCreate(STR_Print, XmFONTLIST_DEFAULT_TAG);
      mus_snprintf(print_string, PRINT_BUFFER_SIZE, "print %s", nsp->shortname);
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

void File_Print_Callback(Widget w, XtPointer context, XtPointer info)
{
  Arg args[20];
  int n;
  Widget dl, rc;
  XmString xmstr1, xmstr2, xmstr3, xmstr4, titlestr;
  snd_info *nsp;
  snd_state *ss = (snd_state *)context;
  if (ss->print_choice == PRINT_SND)
    {
      nsp = any_selected_sound(ss);
      if (!nsp) return;
      mus_snprintf(print_string, PRINT_BUFFER_SIZE, "print %s", nsp->shortname);
      xmstr4 = XmStringCreate(print_string, XmFONTLIST_DEFAULT_TAG);
    }
  else xmstr4 = XmStringCreate(STR_print_env, XmFONTLIST_DEFAULT_TAG);

  if (!file_print_dialog)
    {
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      xmstr1 = XmStringCreate(STR_Print, XmFONTLIST_DEFAULT_TAG);  /* "ok" here is confusing -- might mean, ok I'm done */
      xmstr2 = XmStringCreate(STR_Help, XmFONTLIST_DEFAULT_TAG);
      xmstr3 = XmStringCreate(STR_Dismiss, XmFONTLIST_DEFAULT_TAG);
      titlestr = XmStringCreate(STR_Print, XmFONTLIST_DEFAULT_TAG);

      XtSetArg(args[n], XmNmessageString, xmstr4); n++;
      XtSetArg(args[n], XmNokLabelString, xmstr1); n++;
      XtSetArg(args[n], XmNhelpLabelString, xmstr2); n++;
      XtSetArg(args[n], XmNcancelLabelString, xmstr3); n++;
      XtSetArg(args[n], XmNautoUnmanage, FALSE); n++;
      XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, FALSE); n++;
      file_print_dialog = XmCreateMessageDialog(w, STR_eps_file_p, args, n);
      set_dialog_widget(PRINT_DIALOG, file_print_dialog);
#if OVERRIDE_TOGGLE
      override_form_translation(file_print_dialog);
#endif

      XmStringFree(xmstr1);
      XmStringFree(xmstr2);
      XmStringFree(xmstr3);
      XmStringFree(xmstr4);
      XmStringFree(titlestr);
      XtUnmanageChild(XmMessageBoxGetChild(file_print_dialog, XmDIALOG_SYMBOL_LABEL));
      XtAddCallback(file_print_dialog, XmNhelpCallback, file_print_help_callback, ss);
      XtAddCallback(file_print_dialog, XmNcancelCallback, file_print_cancel_callback, ss);
      XtAddCallback(file_print_dialog, XmNokCallback, file_print_ok_callback, ss);

      rc = sndCreateFormWidget("form", file_print_dialog, NULL, 0);

      n = 0;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      dl = XtCreateManagedWidget(STR_eps_file_p, xmLabelWidgetClass, rc, args, n);

      n = 0;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, dl); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNvalue, eps_file(ss)); n++;
      file_print_name = sndCreateTextFieldWidget(ss, "text", rc, args, n, NOT_ACTIVATABLE, NO_COMPLETER);

      n = 0;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, file_print_name); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      file_print_eps_or_lpr = sndCreateToggleButtonWidget(STR_direct_to_printer, rc, args, n);

      XtManageChild(file_print_dialog);

      if (!(ss->using_schemes))	
	{
	  map_over_children(file_print_dialog, set_main_color_of_widget, (void *)context);
	  XtVaSetValues(XmMessageBoxGetChild(file_print_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(file_print_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(file_print_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(file_print_eps_or_lpr, XmNselectColor, (ss->sgx)->pushed_button_color, NULL);
	}
      add_dialog(ss, file_print_dialog);
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

char *ps_rgb(snd_state *ss, int pchan)
{
  char *buf;
  state_context *sx;
  Colormap cmap;
  XColor tmp_color;
  Display *dpy;
  Pixel color;
  sx = ss->sgx;
  switch (pchan)
    {
    case 0: color = sx->black;      break;
    case 1: color = sx->red;        break;
    case 2: color = sx->green;      break;
    case 3: color = sx->light_blue; break;
    default: color = sx->black;     break;
    }
  dpy = XtDisplay(MAIN_SHELL(ss));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  tmp_color.pixel = color;
  XQueryColor(dpy, cmap, &tmp_color);
  buf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  mus_snprintf(buf, PRINT_BUFFER_SIZE, " %.2f %.2f %.2f RG\n",
	  (float)tmp_color.red / 65535.0,
	  (float)tmp_color.green / 65535.0,
	  (float)tmp_color.blue / 65535.0);
  return(buf);
}
