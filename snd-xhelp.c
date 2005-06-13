#include "snd.h"

#define HELP_ROWS 10
#define HELP_XREFS 8
#define HELP_COLUMNS 56
/* these set the initial size of the help dialog text area */

static Widget help_dialog = NULL;
static Widget help_text = NULL;
static char *original_help_text = NULL;
static int old_help_text_width = 0; 
static with_word_wrap_t outer_with_wrap = WITHOUT_WORD_WRAP;
static char **help_urls = NULL;

static void help_expose(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  int curwid;
  curwid = widget_width(help_text);
  if (old_help_text_width == 0)
    old_help_text_width = curwid;
  else
    {
      if ((outer_with_wrap == WITH_WORD_WRAP) && (abs(curwid - old_help_text_width) > 10))
	{
	  char *cur_help_str, *new_help_str = NULL;
	  cur_help_str = XmTextGetString(help_text);
	  new_help_str = word_wrap(original_help_text, curwid);
	  XmTextSetString(help_text, new_help_str);
	  if (new_help_str) FREE(new_help_str);
	  if (cur_help_str) XtFree(cur_help_str);
	  old_help_text_width = curwid;
	}
    }
}

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

static char *find_highlighted_text(XmString xs)
{
  /* search xs for text in "url_text" rendition, returning first such portion */
  XtPointer text;
  bool in_red_text = false;
  unsigned int len;
  char *result;
  XmStringComponentType type;
  XmStringContext ctx;
  XmStringInitContext(&ctx, xs);
  while ((type = XmStringGetNextTriple(ctx, &len, &text)) != XmSTRING_COMPONENT_END)
    {
      switch (type)
	{
	case XmSTRING_COMPONENT_RENDITION_BEGIN: 
	  if (text) in_red_text = (strcmp((char *)text, "url_text") == 0);
	  break;
	case XmSTRING_COMPONENT_RENDITION_END:
	  in_red_text = false;
	  break;
	case XmSTRING_COMPONENT_TEXT:
	  if (in_red_text) 
	    {
	      result = copy_string((char *)text);
	      XtFree((char *)text);
	      XmStringFreeContext(ctx);
	      return(result);
	    }
	}
      /* this from the Motif docs, though it looks odd to me */
      if (text) XtFree((char *)text);
      text = NULL;
    }
  XmStringFreeContext(ctx);
  return(NULL);
}

static Widget related_items = NULL;
static char *help_completer(char *text) {return(NULL);}

static bool new_help(const char *pattern)
{
  char *url = NULL;
  char **xrefs;
  url = snd_url(pattern);
  if (url)
    {
      /* given name, find doc string, if any */
      XEN xstr;
      xstr = g_snd_help(C_TO_XEN_STRING(pattern), 0);
      if (XEN_STRING_P(xstr))
	{
	  int gc_loc;
	  gc_loc = snd_protect(xstr);
	  xrefs = help_name_to_xrefs(pattern);
	  snd_help_with_xrefs(pattern, XEN_TO_C_STRING(xstr), WITH_WORD_WRAP, xrefs, NULL);
	  snd_unprotect_at(gc_loc);
	  if (xrefs) FREE(xrefs);
	  return(true);
	}
    }
  if (!(snd_topic_help(pattern)))
    {
      xrefs = help_name_to_xrefs(pattern);
      if (xrefs)
	{
	  snd_help_with_xrefs(pattern, "(no help found)", WITH_WORD_WRAP, xrefs, NULL);
	  FREE(xrefs);
	  return(true);
	}
      else snd_help_with_xrefs(pattern, "(no help found)", WITH_WORD_WRAP, NULL, NULL);
    }
  return(false);
}

static char **help_history = NULL;
static int help_history_size = 0;
static int help_history_pos = 0;
static bool help_needed = true;

static void add_pattern_to_help_history(const char *pattern)
{
  if (!help_needed) return;
  if (help_history_size == 0)
    {
      help_history_size = 16;
      help_history = (char **)CALLOC(help_history_size, sizeof(char *));
    }
  else
    {
      if (help_history_pos >= help_history_size)
	{
	  int i;
	  for (i = 0; i < 8; i++) 
	    {
	      if (help_history[i]) FREE(help_history[i]);
	      help_history[i] = help_history[i + 8];
	      help_history[i + 8] = NULL;
	    }
	  help_history_pos = 8;
	}
    }
  if (help_history[help_history_pos]) FREE(help_history[help_history_pos]);
  help_history[help_history_pos++] = copy_string(pattern);
}

static void help_next_callback(Widget w, XtPointer context, XtPointer info) 
{
  if ((help_history_pos < help_history_size) && 
      (help_history[help_history_pos]))
    {
      help_needed = false;
      help_history_pos++;
      new_help(help_history[help_history_pos - 1]);
      help_needed = true;
    }
}

static void help_previous_callback(Widget w, XtPointer context, XtPointer info) 
{
  if ((help_history_pos > 1) &&
      (help_history[help_history_pos - 2]))
    {
      help_needed = false;
      help_history_pos--;
      new_help(help_history[help_history_pos - 1]);
      help_needed = true;
    }
}

static void help_browse_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* single-click to select item in "related items" list */
  char *red_text = NULL;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsList(w), w);
  if ((help_urls) && (help_urls[cbs->item_position - 1]))
    url_to_html_viewer(help_urls[cbs->item_position - 1]);
  else
    {
      red_text = find_highlighted_text(cbs->item);
      if (red_text)
	{
	  name_to_html_viewer(red_text);
	  FREE(red_text);
	}
      else
	{
	  red_text = (char *)XmStringUnparse(cbs->item, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
	  if (red_text) 
	    {
	      new_help(red_text);
	      XtFree(red_text);
	    }
	}
    }
}

static void help_double_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* double-click item in "related items" list */
  char *red_text = NULL;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsList(w), w);
  if ((help_urls) && (help_urls[cbs->item_position - 1]))
    url_to_html_viewer(help_urls[cbs->item_position - 1]);
  else
    {
      red_text = find_highlighted_text(cbs->selected_items[0]);
      if (red_text)
	{
	  name_to_html_viewer(red_text);
	  FREE(red_text);
	}
      else
	{
	  red_text = (char *)XmStringUnparse(cbs->selected_items[0], NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
	  if (red_text)
	    {
	      name_to_html_viewer(red_text);
	      XtFree(red_text);
	    }
	}
    }
}

static Widget help_search = NULL;

static void ok_callback(Widget w, XtPointer context, XtPointer info) 
{
  Widget active_widget;
  active_widget = XmGetFocusWidget(help_dialog);
  if ((!active_widget) || (active_widget != help_search))
    XtUnmanageChild(help_dialog);
}

static void text_release_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  char *help_str;
  help_str = XmTextGetSelection(w);
  if (help_str)
    {
      new_help(help_str);
      XtFree(help_str);
    }
}

static void help_search_callback(Widget w, XtPointer context, XtPointer info)
{
  char *pattern = NULL;
  pattern = XmTextFieldGetString(w);
  if (new_help(pattern))
    XmTextFieldSetString(w, "");
  if (pattern) XtFree(pattern);
}

static Widget help_next_button = NULL, help_previous_button = NULL;

static void create_help_monolog(void)
{
  /* create scrollable but not editable text window */
  Arg args[20];
  int n;
  XmString titlestr, forward, dismiss;
  Widget holder, xref_label; /* documentation says this isn't needed, but it is */
  XmRendition texts[2];
  Widget frame, label, inner_holder, sep, parent;
  XmRenderTable rs = NULL;
  titlestr = XmStringCreate(_("Help"), XmFONTLIST_DEFAULT_TAG);
  forward = XmStringCreate(_("Forward"), XmFONTLIST_DEFAULT_TAG);
  dismiss = XmStringCreate(_("Dismiss"), XmFONTLIST_DEFAULT_TAG);

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
  XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
  /* this window should be resizable by the user (i.e. have the resize bars), but not resize itself */
  XtSetArg(args[n], XmNautoUnmanage, false); n++;
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
  XtSetArg(args[n], XmNnoResize, false); n++;
  XtSetArg(args[n], XmNtransient, false); n++;
  XtSetArg(args[n], XmNokLabelString, dismiss); n++;
  XtSetArg(args[n], XmNcancelLabelString, forward); n++;

  help_dialog = XmCreateMessageDialog(MAIN_PANE(ss), "snd-help", args, n);
  XtAddEventHandler(help_dialog, ExposureMask, false, help_expose, NULL);
  XtAddCallback(help_dialog, XmNokCallback, ok_callback, NULL);
  XtAddCallback(help_dialog, XmNcancelCallback, help_next_callback, NULL);
  help_next_button = XmMessageBoxGetChild(help_dialog, XmDIALOG_CANCEL_BUTTON);

  XtUnmanageChild(XmMessageBoxGetChild(help_dialog, XmDIALOG_HELP_BUTTON));
  XtUnmanageChild(XmMessageBoxGetChild(help_dialog, XmDIALOG_SYMBOL_LABEL));

  if (!(ss->using_schemes))
    XtVaSetValues(XmMessageBoxGetChild(help_dialog, XmDIALOG_MESSAGE_LABEL), XmNbackground, ss->sgx->help_button_color, NULL);

  n = 0;
  if (!(ss->using_schemes)) 
    {
      XtSetArg(args[n], XmNbackground, ss->sgx->doit_button_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;
    }
  help_previous_button = XtCreateManagedWidget(_("Back"), xmPushButtonGadgetClass, help_dialog, args, n);
  XtAddCallback(help_previous_button, XmNactivateCallback, help_previous_callback, NULL);
  XtSetSensitive(help_next_button, false);
  XtSetSensitive(help_previous_button, false);
      
  XmStringFree(titlestr);
  XmStringFree(forward);
  holder = XtCreateManagedWidget("holder", xmFormWidgetClass, help_dialog, NULL, 0);

  n = 0;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;
  XtSetArg(args[n], XmNeditable, false); n++;
  XtSetArg(args[n], XmNcolumns, HELP_COLUMNS); n++;
  XtSetArg(args[n], XmNrows, HELP_ROWS); n++;
  if (!(ss->using_schemes))
    {
      XtSetArg(args[n], XmNforeground, ss->sgx->black); n++; /* needed if color allocation fails completely */
      XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
    }
  help_text = XmCreateScrolledText(holder, "help-text", args, n);
  XtAddEventHandler(help_text, ButtonReleaseMask, false, text_release_callback, NULL);
  XtManageChild(help_text);

  /* to display the url-related portion of the text in red, we need to a rendition for it in the rendertable */
  /* try to find the current default render table. */
  parent = help_text;
  while ((parent != NULL) && (rs == NULL))
    {
      XtVaGetValues(parent, XmNrenderTable, &rs, NULL);
      parent = XtParent(parent);
    }
  n = 0;
  if (rs == NULL)
    {
      /* failed to find a rendertable to specialize, so we need an explicit font */
      XtSetArg(args[n], XmNfontName, listener_font(ss)); n++;
      XtSetArg(args[n], XmNfontType, XmFONT_IS_FONT); n++; 
      XtSetArg(args[n], XmNloadModel, XmLOAD_IMMEDIATE); n++;
    }
  XtSetArg(args[n], XmNrenditionBackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNrenditionForeground, ss->sgx->quit_button_color); n++;
  texts[0] = XmRenditionCreate(help_text, "url_text", args, n);
  XtSetArg(args[n - 1], XmNrenditionForeground, ss->sgx->black); 
  texts[1] = XmRenditionCreate(help_text, "normal_text", args, n);
  rs = XmRenderTableCopy(XmRenderTableAddRenditions(rs, texts, 2, XmMERGE_NEW), NULL, 0);
  /*
   * valgrind says this data is used later
  XmRenditionFree(texts[0]);
  XmRenditionFree(texts[1]);
  */

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
      XtSetArg(args[n], XmNbackground, ss->sgx->doit_button_color); n++;
    }
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNheight, 24); n++;
  /* XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++; */
  label = XtCreateManagedWidget(_("help topic:"), xmLabelWidgetClass, holder, args, n);
  
  n = 0;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, label); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  help_search = make_textfield_widget("help-search", holder, args, n, ACTIVATABLE, add_completer_func(help_completer));
  XtAddCallback(help_search, XmNactivateCallback, help_search_callback, NULL);
  
  n = 0;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, sep); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNbottomWidget, help_search); n++;
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
  xref_label = XtCreateManagedWidget(_("related topics:"), xmLabelWidgetClass, inner_holder, args, n);
  
  n = 0;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, xref_label); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNfontList, NULL); n++; /* needed or new rendertable doesn't take effect! */
  XtSetArg(args[n], XmNrenderTable, rs); n++;
  XtSetArg(args[n], XmNvisibleItemCount, HELP_XREFS); n++;
  XtSetArg(args[n], XmNscrollBarDisplayPolicy, XmAS_NEEDED); n++;
  related_items = XmCreateScrolledList(inner_holder, "help-list", args, n);
  XtManageChild(related_items);
  XtAddCallback(related_items, XmNbrowseSelectionCallback, help_browse_callback, NULL);
  XtAddCallback(related_items, XmNdefaultActionCallback, help_double_click_callback, NULL);
  
  XtManageChild(help_dialog);
  
  if (!(ss->using_schemes))
    {
      map_over_children(help_dialog, set_main_color_of_widget, NULL);
      XtVaSetValues(help_text, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
      XtVaSetValues(related_items, XmNbackground, ss->sgx->highlight_color, XmNforeground, ss->sgx->black, NULL);
      XtVaSetValues(xref_label, XmNbackground, ss->sgx->reset_button_color, XmNforeground, ss->sgx->black, NULL);
      XtVaSetValues(XmMessageBoxGetChild(help_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(help_dialog, XmDIALOG_OK_BUTTON), XmNbackground, ss->sgx->quit_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(help_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(help_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->doit_again_button_color, NULL);
    }
  set_dialog_widget(HELP_DIALOG, help_dialog);
}

static XFontStruct *help_font_struct = NULL;
static XFontStruct *help_font(void)
{
  Widget parent;
  XmRenderTable rs = NULL;
  if (help_font_struct) return(help_font_struct);
  parent = help_text;
  while ((parent != NULL) && (rs == NULL))
    {
      XtVaGetValues(parent, XmNrenderTable, &rs, NULL);
      parent = XtParent(parent);
    }
  if (rs)
    {
      XmRendition rend;
      Arg args[2];
      XtPointer font;
      rend = XmRenderTableGetRendition(rs, XmFONTLIST_DEFAULT_TAG);
      if (rend)
	{
	  XtSetArg(args[0], XmNfont, &font);
	  XmRenditionRetrieve(rend, args, 1);
	  if (font)
	    {
	      help_font_struct = (XFontStruct *)font;
	      return((XFontStruct *)font);
	    }
	}
    }
  return(NULL);
}

int help_text_width(const char *txt, int start, int end)
{
  XFontStruct *font;
  if (txt[start] != '\0')
    {
      font = help_font();
      if (font)
	{
	  int width = 0;
	  width = XTextWidth(font, (char *)(txt + start), end - start);
	  if (width > 0) return(width);
	}
    }
  return((end - start) * 8);
}


Widget snd_help(const char *subject, const char *helpstr, with_word_wrap_t with_wrap)
{
  /* place help string in scrollable help window */
  /* if window is already active, add this help at the top and reposition */
  XmString xstr1;
  outer_with_wrap = with_wrap;
  if (!(help_dialog)) 
    create_help_monolog(); 
  else raise_dialog(help_dialog);
  xstr1 = XmStringCreate((char *)subject, XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(help_dialog, XmNmessageString, xstr1, NULL);
  original_help_text = (char *)helpstr;
  if (with_wrap == WITH_WORD_WRAP)
    {
      char *new_help_str = NULL;
      new_help_str = word_wrap(helpstr, widget_width(help_text));
      XmTextSetString(help_text, new_help_str);
      if (new_help_str) FREE(new_help_str);
    }
  else XmTextSetString(help_text, (char *)helpstr);
  if (!XtIsManaged(help_dialog)) 
    XtManageChild(help_dialog);
  XmStringFree(xstr1);
  XtVaSetValues(related_items, XmNitems, NULL, XmNitemCount, 0, NULL);
  if (help_needed) add_pattern_to_help_history(subject);
  XtSetSensitive(help_next_button, (help_history_pos < help_history_size) && (help_history[help_history_pos]));
  XtSetSensitive(help_previous_button, (help_history_pos > 1));
  return(help_dialog);
}

Widget snd_help_with_xrefs(const char *subject, const char *helpstr, with_word_wrap_t with_wrap, char **xrefs, char **urls)
{
  Widget w;
  w = snd_help(subject, helpstr, with_wrap);
  help_urls = urls; /* can't associate the url with the help item in any "natural" way in Motif (no user-data per item) */
  if (xrefs)
    {
      XmString *strs;
      int i = 0, len = 0, strs_size = 32;
      strs = (XmString *)CALLOC(strs_size, sizeof(XmString));	  
      while (true)
	{
	  if (i >= strs_size)
	    {
	      int k;
	      strs_size *= 2;
	      strs = (XmString *)REALLOC(strs, strs_size * sizeof(XmString));
	      for (k = i; k < strs_size; k++) strs[k] = NULL;
	    }
	  if (xrefs[i])
	    {
	      strs[i] = parse_crossref((const char *)(xrefs[i]));
	      i++;
	    }
	  else 
	    {
	      len = i;
	      break;
	    }
	}
      XtVaSetValues(related_items, XmNitems, strs, XmNitemCount, len, NULL);
      for (i = 0; i < len; i++)
	XmStringFree(strs[i]);
      FREE(strs);
    }
  return(w);
}

void snd_help_append(char *text)
{
  if (help_text) XmTextInsert(help_text, XmTextGetLastPosition(help_text), text);
}

void snd_help_back_to_top(void)
{
  if (help_text) XmTextShowPosition(help_text, 0);
}

void save_help_dialog_state(FILE *fd)
{
  if ((help_dialog) && (XtIsManaged(help_dialog)))
    {
      char *topic = NULL, *help = NULL;
      if ((help_history) && (help_history_pos > 0))
	{
	  topic = help_history[help_history_pos - 1];
	  if (topic)
	    {
	      help = XEN_TO_C_STRING(g_snd_help(C_TO_XEN_STRING(topic), 0));
	      if (help)
		{
#if HAVE_SCHEME
		  fprintf(fd, "(%s \"%s\" \"%s\")\n", S_help_dialog, topic, help);
#endif
#if HAVE_RUBY
		  fprintf(fd, "%s(\"%s\", \"%s\")\n", TO_PROC_NAME(S_help_dialog), topic, help);
#endif
		}
	    }
	}
    }
}
