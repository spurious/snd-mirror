#include "snd.h"

/* ---------------- HELP MONOLOG ---------------- */

#define HELP_ROWS 12
#define HELP_COLUMNS 56
/* these set the initial size of the (non XmHTML) help dialog text area */

static Widget help_dialog = NULL;
static Widget help_text = NULL;
static char help_window_label[LABEL_BUFFER_SIZE];

static char *cr_to_space(char *val)
{
  int i, len;
  if (val)
    {
      len = strlen(val);
      for (i = 0; i < len; i++)
	if (val[i] == '\n')
	  val[i] = ' ';
    }
  return(val);
}

static bool no_cr(const char *val)
{
  int i, len;
  if (val)
    {
      len = strlen(val);
      for (i = 0; i < len; i++)
	if (val[i] == '\n')
	  return(false);
    }
  return(true);
}

static int help_text_width = 0;
static bool outer_with_wrap = false;
static void help_expose(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  int curwid;
  curwid = widget_width(help_text);
  if (help_text_width == 0)
    help_text_width = curwid;
  else
    {
      if ((outer_with_wrap) && (abs(curwid - help_text_width) > 10))
	{
	  char *cur_help;
	  char *new_help = NULL;
	  cur_help = cr_to_space(XmTextGetString(help_text));
	  new_help = word_wrap(cur_help, curwid);
	  XmTextSetString(help_text, new_help);
	  if (new_help) FREE(new_help);
	  if (cur_help) XtFree(cur_help);
	  help_text_width = curwid;
	}
    }
}

#if (XmVERSION > 1)
static XmString parse_crossref(const char *xref)
{
  XmString xs = NULL, tmp;
  int i, len, start = 0, j, k;
  char *str;
  /* crossref has text for scrolled list entry, but url is in '{}'.  It is displayed via the texts rendition */
  len = strlen(xref);
  for (i = 0; i < len; i++)
    {
      if (xref[i] == '{')
	{
	  if (i > 0)
	    {
	      str = (char *)CALLOC(i - start + 1, sizeof(char));
	      for (k = 0, j = start; j < i; k++, j++) str[k] = xref[j];
	      tmp = XmStringGenerate(str, NULL, XmCHARSET_TEXT, "normal_text");
	      FREE(str);
	      if (xs) 
		xs = XmStringConcatAndFree(xs, tmp);
	      else xs = tmp;
	    }
	  start = i + 1;
	}
      else
	{
	  if (xref[i] == '}')
	    {
	      str = (char *)CALLOC(i - start + 1, sizeof(char));
	      for (k = 0, j = start; j < i; k++, j++) str[k] = xref[j];
	      if (xs)
		xs = XmStringConcatAndFree(xs, XmStringGenerate(str, NULL, XmCHARSET_TEXT, "url_text"));
	      else xs = XmStringGenerate(str, NULL, XmCHARSET_TEXT, "url_text");
	      FREE(str);
	      start = i + 1;
	    }
	}
    }
  if (start < len)
    {
      str = (char *)CALLOC(len - start + 1, sizeof(char));
      for (k = 0, j = start; j < len; k++, j++) str[k] = xref[j];
      if (xs)
	xs = XmStringConcatAndFree(xs, XmStringGenerate(str, NULL, XmCHARSET_TEXT, "normal_text"));
      else xs = XmStringGenerate(str, NULL, XmCHARSET_TEXT, "normal_text");
      FREE(str);
    }
  return(xs);
}

static char *help_completer(char *text) {return(NULL);}
/* TODO: help completion, cref tables, help search mechanism (via help strings I guess) */
/* TODO: activation of help search -> dialog quits */
/* TODO: find default font for xref table */
/* TODO: mozilla if related is not top level, else go to it. */
#endif


static void create_help_monolog(void)
{
  /* create scrollable but not editable text window */
  Arg args[20];
  int n;
  XmString titlestr;
  Widget holder, table, xref_label; /* documentation says this isn't needed, but it is */
  titlestr = XmStringCreate(_("Help"), XmFONTLIST_DEFAULT_TAG);
  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
  XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
  /* this window should be resizable by the user (i.e. have the resize bars), but not resize itself */
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
  XtSetArg(args[n], XmNnoResize, false); n++;
  XtSetArg(args[n], XmNtransient, false); n++;
  help_dialog = XmCreateMessageDialog(MAIN_PANE(ss), "snd-help", args, n);
  XtAddEventHandler(help_dialog, ExposureMask, false, help_expose, NULL);

  XtUnmanageChild(XmMessageBoxGetChild(help_dialog, XmDIALOG_CANCEL_BUTTON));
  XtUnmanageChild(XmMessageBoxGetChild(help_dialog, XmDIALOG_HELP_BUTTON));
  XtUnmanageChild(XmMessageBoxGetChild(help_dialog, XmDIALOG_SYMBOL_LABEL));

  XtVaSetValues(XmMessageBoxGetChild(help_dialog, XmDIALOG_MESSAGE_LABEL), XmNbackground, ss->sgx->help_button_color, NULL);
      
  XmStringFree(titlestr);

#if (XmVERSION > 1)
  holder = XtCreateManagedWidget("holder", xmFormWidgetClass, help_dialog, NULL, 0);
#else
  holder = edit_dialog;
#endif

  n = 0;
#if (XmVERSION > 1)
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
#endif
  XtSetArg(args[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;
  XtSetArg(args[n], XmNeditable, false); n++;
  XtSetArg(args[n], XmNcolumns, HELP_COLUMNS); n++;
  XtSetArg(args[n], XmNrows, HELP_ROWS); n++;
  if (!(ss->using_schemes))
    {
      XtSetArg(args[n], XmNforeground, (ss->sgx)->black); n++; /* needed if color allocation fails completely */
      XtSetArg(args[n], XmNbackground, (ss->sgx)->white); n++;
    }
  help_text = XmCreateScrolledText(holder, "help-text", args, n);
  XtManageChild(help_text);

#if (XmVERSION > 1)
  {
    XmRendition texts[2];
    Widget frame, search, label, inner_holder, sep;
    XmString *strs;
    XmRenderTable rs;
    
    n = 0;
    XtSetArg(args[n], XmNfontName, listener_font(ss)); n++;
    XtSetArg(args[n], XmNfontType, XmFONT_IS_FONT); n++; 
    XtSetArg(args[n], XmNloadModel, XmLOAD_IMMEDIATE); n++;
    XtSetArg(args[n], XmNrenditionBackground, ss->sgx->white); n++;
    XtSetArg(args[n], XmNrenditionForeground, ss->sgx->quit_button_color); n++;
    /* XtSetArg(args[n], XmNunderlineType, XmSINGLE_LINE); n++; */
    texts[0] = XmRenditionCreate(MAIN_SHELL(ss), "url_text", args, n);
    XtSetArg(args[n - 1], XmNrenditionForeground, ss->sgx->black); 
    texts[1] = XmRenditionCreate(MAIN_SHELL(ss), "normal_text", args, n);
    rs = XmRenderTableAddRenditions(NULL, texts, 2, XmMERGE_NEW);
    XmRenditionFree(texts[0]);
    XmRenditionFree(texts[1]);

    /* just some place holders for testing */
    strs = (XmString *)CALLOC(3, sizeof(XmString)); 
    strs[0] = parse_crossref("Max length of region list: {max-regions}");
    strs[1] = parse_crossref("Whether selection {selection-creates-region} creates a region");
    strs[2] = parse_crossref("{stop-playing-region-hook}: play hook");
    
    n = 0;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget, XtParent(help_text)); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNheight, 6); n++;
    XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
    XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
    sep = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, holder, args, n);

    n = 0;
    if (!(ss->using_schemes))
      {
	XtSetArg(args[n], XmNbackground, (ss->sgx)->doit_button_color); n++;
      }
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
    XtSetArg(args[n], XmNheight, 24); n++;
    /* XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++; */
    label = XtCreateManagedWidget("help topic:", xmLabelWidgetClass, holder, args, n);

    n = 0;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNleftWidget, label); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    search = make_textfield_widget("help-search", holder, args, n, ACTIVATABLE, add_completer_func(help_completer));

    n = 0;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget, sep); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNbottomWidget, search); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNshadowThickness, 4); n++;
    frame = XtCreateManagedWidget("frame", xmFrameWidgetClass, holder, args, n);

    inner_holder = XtCreateManagedWidget("inner-holder", xmFormWidgetClass, frame, NULL, 0);
    
    n = 0;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
    xref_label = XtCreateManagedWidget("related topics:", xmLabelWidgetClass, inner_holder, args, n);

    n = 0;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget, xref_label); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
#if USE_RENDITIONS
    XtSetArg(args[n], XmNfontList, NULL); n++; /* needed or new rendertable doesn't take effect! */
#endif
    XtSetArg(args[n], XmNrenderTable, rs); n++;
    XtSetArg(args[n], XmNitems, strs); n++;
    XtSetArg(args[n], XmNitemCount, 3); n++;
    XtSetArg(args[n], XmNvisibleItemCount, 4); n++;
    XtSetArg(args[n], XmNscrollBarDisplayPolicy, XmAS_NEEDED); n++;
    table = XmCreateScrolledList(inner_holder, "help-list", args, n);
    XtManageChild(table);
  }
#endif
  XtManageChild(help_dialog);

  if (!(ss->using_schemes))
    {
      map_over_children(help_dialog, set_main_color_of_widget, NULL);
      XtVaSetValues(help_text, XmNbackground, (ss->sgx)->white, XmNforeground, (ss->sgx)->black, NULL);
      XtVaSetValues(table, XmNbackground, (ss->sgx)->highlight_color, XmNforeground, (ss->sgx)->black, NULL);
      XtVaSetValues(xref_label, XmNbackground, (ss->sgx)->reset_button_color, XmNforeground, (ss->sgx)->black, NULL);
      XtVaSetValues(XmMessageBoxGetChild(help_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(help_dialog, XmDIALOG_OK_BUTTON), XmNbackground, (ss->sgx)->quit_button_color, NULL);
    }
  set_dialog_widget(HELP_DIALOG, help_dialog);
}

Widget snd_help(const char *subject, const char *helpstr, bool with_wrap)
{
  /* place help string in scrollable help window */
  /* if window is already active, add this help at the top and reposition */
  /* if XmHTML, this is writing the text to the root window if called from Guile?!? */
  XmString xstr1;
  outer_with_wrap = ((with_wrap) && (no_cr(helpstr)));
  if (!(help_dialog)) 
    create_help_monolog(); 
  else raise_dialog(help_dialog);
  mus_snprintf(help_window_label, LABEL_BUFFER_SIZE, _("%s help"), subject);
  xstr1 = XmStringCreate(help_window_label, XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(help_dialog, XmNmessageString, xstr1, NULL);
  if (with_wrap)
    {
      char *new_help = NULL;
      new_help = word_wrap(helpstr, widget_width(help_text));
      XmTextSetString(help_text, new_help);
      if (new_help) FREE(new_help);
    }
  else XmTextSetString(help_text, (char *)helpstr);
  if (!XtIsManaged(help_dialog)) 
    XtManageChild(help_dialog);
  XmStringFree(xstr1);
  return(help_dialog);
}


/* TODO: change help menu to 2-level:
   About Snd (much shorter than current overview)
   News
   Topics -> current + all that have crossref tables, maybe as help dialog table itself -- can use underlying support
   Howto (presents table of plausible entries given some "natural" request)
   Index (goes to index.html)

   table version can be list of names(urls)
        or two level: list as before plus thumbnail desc

   then presumably index.scm/rb become redundant

   need current post-a-wrapped-string for snd-help function, but it needs a way to present urls.
*/


