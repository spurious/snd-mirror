#include "snd.h"

#define HELP_ROWS 10
#define HELP_XREFS 8
#define HELP_COLUMNS 72
/* these set the initial size of the help dialog text area */

static Widget help_dialog = NULL;
static Widget help_text = NULL;
static char *original_help_text = NULL;
static with_word_wrap_t outer_with_wrap = WITHOUT_WORD_WRAP;
static const char **help_urls = NULL; /* shouldn't this be static char* const char*? */

static int old_help_text_width = 0; 

static void help_expose(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  int curwid;
  curwid = widget_width(help_text);
  if (old_help_text_width == 0)
    old_help_text_width = curwid;
  else
    {
      if ((outer_with_wrap == WITH_WORD_WRAP) && 
	  (abs(curwid - old_help_text_width) > 10))
	{
	  char *cur_help_str, *new_help_str = NULL;
	  cur_help_str = XmTextGetString(help_text);
	  new_help_str = word_wrap(original_help_text, curwid);
	  XmTextSetString(help_text, new_help_str);
	  if (new_help_str) free(new_help_str);
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
	      str = (char *)calloc(i - start + 1, sizeof(char));
	      for (k = 0, j = start; j < i; k++, j++) str[k] = xref[j];
	      tmp = XmStringGenerate(str, NULL, XmCHARSET_TEXT, (char *)"normal_text");
	      free(str);
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
	      str = (char *)calloc(i - start + 1, sizeof(char));
	      for (k = 0, j = start; j < i; k++, j++) str[k] = xref[j];
	      if (xs)
		xs = XmStringConcatAndFree(xs, XmStringGenerate(str, NULL, XmCHARSET_TEXT, (char *)"url_text"));
	      else xs = XmStringGenerate(str, NULL, XmCHARSET_TEXT, (char *)"url_text");
	      free(str);
	      start = i + 1;
	    }
	}
    }
  if (start < len)
    {
      str = (char *)calloc(len - start + 1, sizeof(char));
      for (k = 0, j = start; j < len; k++, j++) str[k] = xref[j];
      if (xs)
	xs = XmStringConcatAndFree(xs, XmStringGenerate(str, NULL, XmCHARSET_TEXT, (char *)"normal_text"));
      else xs = XmStringGenerate(str, NULL, XmCHARSET_TEXT, (char *)"normal_text");
      free(str);
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
	  in_red_text = mus_strcmp((char *)text, "url_text");
	  break;

	case XmSTRING_COMPONENT_RENDITION_END:
	  in_red_text = false;
	  break;

	case XmSTRING_COMPONENT_TEXT:
	  if (in_red_text) 
	    {
	      result = mus_strdup((char *)text);
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

static char *help_completer(widget_t w, const char *text, void *data) 
{
  return(expression_completer(w, text, data));
  /* might want to look at help topics too */
} 


static bool new_help(const char *pattern, bool complain)
{
  const char *url = NULL;
  const char **xrefs;

  url = snd_url(pattern);
  if (url)
    {
      /* given name, find doc string, if any */
      XEN xstr;
      xstr = g_snd_help(C_TO_XEN_STRING(pattern), 0);
      if (Xen_is_string(xstr))
	{
	  int gc_loc;
	  gc_loc = snd_protect(xstr);
	  xrefs = help_name_to_xrefs(pattern);
	  snd_help_with_xrefs(pattern, XEN_TO_C_STRING(xstr), WITH_WORD_WRAP, xrefs, NULL);
	  snd_unprotect_at(gc_loc);
	  if (xrefs) free(xrefs);
	  return(true);
	}
      url_to_html_viewer(url);
      return(true);
    }

  if ((!(snd_topic_help(pattern))) && (complain))
    {
      xrefs = help_name_to_xrefs(pattern);
      if (xrefs)
	{
	  snd_help_with_xrefs(pattern, "(no help found)", WITH_WORD_WRAP, xrefs, NULL);
	  free(xrefs);
	  return(true);
	}
      else snd_help_with_xrefs(pattern, "(no help found)", WITH_WORD_WRAP, NULL, NULL);
    }

  return(false);
}


static void help_browse_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* single-click to select item in "related items" list */
  char *red_text = NULL;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  if ((help_urls) && (help_urls[cbs->item_position - 1]))
    url_to_html_viewer(help_urls[cbs->item_position - 1]);
  else
    {
      red_text = find_highlighted_text(cbs->item);
      if (red_text)
	{
	  name_to_html_viewer(red_text);
	  free(red_text);
	}
      else
	{
	  red_text = (char *)XmStringUnparse(cbs->item, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
	  if (red_text) 
	    {
	      new_help(red_text, true);
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
  if ((help_urls) && (help_urls[cbs->item_position - 1]))
    url_to_html_viewer(help_urls[cbs->item_position - 1]);
  else
    {
      red_text = find_highlighted_text(cbs->selected_items[0]);
      if (red_text)
	{
	  name_to_html_viewer(red_text);
	  free(red_text);
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

static void help_quit_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* this focus widget check is actually needed! */
  if (XmGetFocusWidget(help_dialog) == XmMessageBoxGetChild(help_dialog, XmDIALOG_CANCEL_BUTTON))
    XtUnmanageChild(help_dialog);
}


static void text_release_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  char *help_str;
  help_str = XmTextGetSelection(w);
  if (help_str)
    {
      int i, len;
      bool one_word = true;
      len = mus_strlen(help_str);
      for (i = 0; i < len; i++)
	if (isspace(help_str[i]))
	  {
	    one_word = false;
	    break;
	  }
      if (one_word) new_help(help_str, false);
      XtFree(help_str);
    }
}


static void help_search_callback(Widget w, XtPointer context, XtPointer info)
{
  char *pattern = NULL;
  pattern = XmTextFieldGetString(w);
  if (new_help(pattern, true))
    XmTextFieldSetString(w, (char *)"");
  if (pattern) XtFree(pattern);
}


static XmRendition texts[2];

static void create_help_monolog(void)
{
  /* create scrollable but not editable text window */
  Arg args[20];
  int n;
  XmString titlestr, go_away;
  Widget holder, xref_label; /* documentation says this isn't needed, but it is */
  Widget frame, label, inner_holder, sep, parent;
  XmRenderTable rs = NULL;

  titlestr = XmStringCreateLocalized((char *)I_HELP);
  go_away = XmStringCreateLocalized((char *)I_GO_AWAY);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
  XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
  /* this window should be resizable by the user (i.e. have the resize bars), but not resize itself */
  XtSetArg(args[n], XmNautoUnmanage, false); n++;
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
  XtSetArg(args[n], XmNnoResize, false); n++;
  XtSetArg(args[n], XmNtransient, false); n++;
  XtSetArg(args[n], XmNcancelLabelString, go_away); n++;

  help_dialog = XmCreateMessageDialog(MAIN_PANE(ss), (char *)"snd-help", args, n);
  XtAddEventHandler(help_dialog, ExposureMask, false, help_expose, NULL);

  XtAddCallback(help_dialog, XmNcancelCallback, help_quit_callback, NULL);

  XtUnmanageChild(XmMessageBoxGetChild(help_dialog, XmDIALOG_OK_BUTTON));
  XtUnmanageChild(XmMessageBoxGetChild(help_dialog, XmDIALOG_HELP_BUTTON));
  XtUnmanageChild(XmMessageBoxGetChild(help_dialog, XmDIALOG_SYMBOL_LABEL));

  XtVaSetValues(XmMessageBoxGetChild(help_dialog, XmDIALOG_MESSAGE_LABEL), XmNbackground, ss->highlight_color, NULL);

  XmStringFree(titlestr);
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
  XtSetArg(args[n], XmNforeground, ss->black); n++; /* needed if color allocation fails completely */
  XtSetArg(args[n], XmNbackground, ss->white); n++;
  help_text = XmCreateScrolledText(holder, (char *)"help-text", args, n);
  XtAddEventHandler(help_text, ButtonReleaseMask, false, text_release_callback, NULL);
  XtManageChild(help_text);

  /* to display the url-related portion of the text in red, we need a rendition for it in the rendertable */
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
  XtSetArg(args[n], XmNrenditionBackground, ss->white); n++;
  XtSetArg(args[n], XmNrenditionForeground, ss->red); n++;
  texts[0] = XmRenditionCreate(help_text, (char *)"url_text", args, n);
  XtSetArg(args[n - 1], XmNrenditionForeground, ss->black); 
  texts[1] = XmRenditionCreate(help_text, (char *)"normal_text", args, n);
  rs = XmRenderTableCopy(XmRenderTableAddRenditions(rs, texts, 2, XmMERGE_NEW), NULL, 0);
  /*
   * valgrind says this data is used later
   * XmRenditionFree(texts[0]);
   * XmRenditionFree(texts[1]);
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
  XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
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
  help_search = make_textfield_widget("help-search", holder, args, n, ACTIVATABLE, add_completer_func(help_completer, NULL));
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
  xref_label = XtCreateManagedWidget("related topics:", xmLabelWidgetClass, inner_holder, args, n);
  
  n = 0;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, xref_label); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  
  /* in operton-based solaris 10 this (next) line causes an X server error (with no stack...):
   *   complaint is X_ChangeGC got an invalid font.
   *   Do I need a configure test program for this?  Is there some other way to force the render table to take effect?
   */
#if (!HAVE_SUN) || (!MUS_LITTLE_ENDIAN)
  XtSetArg(args[n], XmNfontList, 0); n++; /* needed or new rendertable doesn't take effect! */
                                          /* also, 0, not NULL so types match */
  XtSetArg(args[n], XmNrenderTable, rs); n++;
#endif

  XtSetArg(args[n], XmNvisibleItemCount, HELP_XREFS); n++; /* appears to be a no-op */
  XtSetArg(args[n], XmNheight, 150); n++;

  XtSetArg(args[n], XmNscrollBarDisplayPolicy, XmAS_NEEDED); n++;
  related_items = XmCreateScrolledList(inner_holder, (char *)"help-list", args, n);
  XtManageChild(related_items);
  XtAddCallback(related_items, XmNbrowseSelectionCallback, help_browse_callback, NULL);
  XtAddCallback(related_items, XmNdefaultActionCallback, help_double_click_callback, NULL);
  
  XtManageChild(help_dialog);
  
  map_over_children(help_dialog, set_main_color_of_widget);
  XtVaSetValues(help_text, XmNbackground, ss->white, XmNforeground, ss->black, NULL);
  XtVaSetValues(related_items, XmNbackground, ss->highlight_color, XmNforeground, ss->black, NULL);
  XtVaSetValues(xref_label, XmNbackground, ss->highlight_color, XmNforeground, ss->black, NULL);

  XtVaSetValues(XmMessageBoxGetChild(help_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, ss->selection_color, NULL);
  XtVaSetValues(XmMessageBoxGetChild(help_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->highlight_color, NULL);

  set_dialog_widget(HELP_DIALOG, help_dialog);
}


int help_text_width(const char *txt, int start, int end)
{
#if 0
  /* this is full of problems... -- adding renditions below makes everything else flakey */
  if ((help_text) && (end > start))
    {
      char *msg;
      int i, j;
      XmString s1;
      Dimension text_wid = 0;
      XmFontList fonts;
      XtVaGetValues(help_text, XmNfontList, &fonts, NULL);
      msg = (char *)calloc(end - start + 1, sizeof(char));
      for (i = start, j = 0; i < end; i++, j++) msg[j] = txt[i];
      s1 = XmStringCreateLocalized(msg);
      text_wid = XmStringWidth(fonts, s1);
      XmStringFree(s1);
      free(msg);
      return((int)text_wid);
    }
#endif
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

  xstr1 = XmStringCreateLocalized((char *)subject);
  XtVaSetValues(help_dialog, XmNmessageString, xstr1, NULL);
  original_help_text = (char *)helpstr;

  if (with_wrap == WITH_WORD_WRAP)
    {
      char *new_help_str = NULL;
      new_help_str = word_wrap(helpstr, widget_width(help_text));
      XmTextSetString(help_text, new_help_str);
      if (new_help_str) free(new_help_str);
    }
  else XmTextSetString(help_text, (char *)helpstr);

  if (!XtIsManaged(help_dialog)) 
    XtManageChild(help_dialog);

  XmStringFree(xstr1);
  XtVaSetValues(related_items, XmNitems, NULL, XmNitemCount, 0, NULL);
  return(help_dialog);
}


Widget snd_help_with_xrefs(const char *subject, const char *helpstr, with_word_wrap_t with_wrap, const char **xrefs, const char **urls)
{
  Widget w;
  w = snd_help(subject, helpstr, with_wrap);
  help_urls = urls; /* can't associate the url with the help item in any "natural" way in Motif (no user-data per item) */
  if (xrefs)
    {
      int i, len;

      for (i = 0; ; i++)
	if (!xrefs[i])
	  {
	    len = i;
	    break;
	  }

      if (len > 0)
	{
	  XmString *strs;
	  strs = (XmString *)calloc(len, sizeof(XmString));
	  
	  for (i = 0; i < len; i++)
	    strs[i] = parse_crossref((const char *)(xrefs[i]));
	  XtVaSetValues(related_items, XmNitems, strs, XmNitemCount, len, NULL);

	  for (i = 0; i < len; i++)
	    XmStringFree(strs[i]);
	  free(strs);
	}
    }
  return(w);
}


void snd_help_append(const char *text)
{
  if (help_text) 
    XmTextInsert(help_text,
		 XmTextGetLastPosition(help_text), 
		 (char *)text);
}


void snd_help_back_to_top(void)
{
  if (help_text) XmTextShowPosition(help_text, 0);
}
