#include "snd.h"

/* TODO:   xmhtml prints to root window in help-dialog!!
   TODO:   probably xhelp should load index.scm and use its code to call netscape
 */

/* ---------------- HELP MONOLOG ---------------- */

#define HELP_ROWS 12
#define HELP_COLUMNS 56
/* these set the initial size of the (non XmHTML) help dialog text area */

static Widget help_dialog = NULL;
static Widget help_text = NULL;
static char help_window_label[64];

#if HAVE_HTML

#include <XmHTML/XmHTML.h>
/* CFLAGS = -g -Wall -DLINUX -DUSR_LIB_OSS=1 -DHAVE_GUILE -DHAVE_HTML=1 -I/home/bil/test/XmHTML-1.1.4/include */
/* LIBS = /home/bil/test/XmHTML-1.1.4/src/libXmHTML.a -L/usr/X11R6/lib -lMrm -lXp -lXm -lXpm -lXmu -lXt -lXext -lX11 /usr/local/lib/libguile.a -lm -ldl */

static char *file_to_string(char *filename)
{ /* from XmHTML/examples/example_1.c */
  FILE *file;
  int size;
  char *content;
  if ((file = fopen(filename, "r")) == NULL)
    {
      snd_error("can't open %s: %s", filename, strerror(errno));
      return(NULL);
    }
  fseek(file, 0, SEEK_END);
  size = ftell(file);
  rewind(file);
  content = (char *)CALLOC(size + 1, sizeof(char));
  if (content == NULL) return(NULL);
  if ((fread(content, 1, size, file)) != size)
    snd_error("%s[%d] %s: did not read entire file!\n", 
	      __FILE__, __LINE__, __FUNCTION__);
  fclose(file);
  return(content);
}

enum {NO_HTML, SND_HTML, EXTSND_HTML, GRFSND_HTML, SNDLIB_HTML, CLM_HTML, SNDSCM_HTML};
static char *htmls[7] = {"","snd.html","extsnd.html","grfsnd.html","sndlib.html","clm.html","sndscm.html"};
static char *html_text = NULL;
static int html_loaded = NO_HTML;

#define MAX_HTML_HISTORY 32
static int current_anchor = -1;
static char *anchors[MAX_HTML_HISTORY];
static int html_files[MAX_HTML_HISTORY];
static Widget back_button = NULL, forward_button = NULL;

static void add_anchor(int html, char *anchor)
{
  int i;
  if (current_anchor == (MAX_HTML_HISTORY - 1))
    {
      FREE(anchors[0]);
      for (i = 0; i < MAX_HTML_HISTORY - 1; i++) 
	anchors[i] = anchors[i + 1];
    }
  else 
    {
      current_anchor++;
      if (anchors[current_anchor]) 
	for (i = current_anchor; i < MAX_HTML_HISTORY; i++) 
	  if (anchors[i])
	    {
	      FREE(anchors[i]);
	      anchors[i] = NULL;
	    }
	  else break;
    }
  anchors[current_anchor] = copy_string(anchor);
  html_files[current_anchor] = html;
  if (current_anchor > 0) XtSetSensitive(back_button, TRUE);
  XtSetSensitive(forward_button, FALSE);
}

static void load_html(snd_state *ss, int which_html, char *anchor, int added)
{
  char *buf, *temp = NULL;
  if (anchor) temp = copy_string(anchor);
  if (html_loaded != which_html)
    {
      if (html_dir(ss) && (*(html_dir(ss))))
	{
	  buf = (char *)CALLOC(256, sizeof(char));
	  mus_snprintf(buf, 256, "%s/%s", html_dir(ss), htmls[which_html]);
	  html_text = file_to_string(buf);
	  FREE(buf);
	}
      else  html_text = file_to_string(htmls[which_html]);
      if (added) add_anchor(which_html, anchor);
    }
  XmHTMLTextSetString(help_text, html_text);
  html_loaded = which_html;
  if (temp)
    {
      XmHTMLAnchorScrollToName(help_text, temp);
      FREE(temp);
    }
}

static void back_anchor_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_state *ss = (snd_state *)context;
  if (current_anchor > 0) 
    {
      current_anchor--;
      load_html(ss, html_files[current_anchor], anchors[current_anchor], FALSE);
      if (current_anchor == 0) 
	XtSetSensitive(back_button, FALSE);
      XtSetSensitive(forward_button, TRUE);
    }
  if (!XtIsManaged(help_dialog)) XtManageChild(help_dialog);
}

static void forward_anchor_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_state *ss = (snd_state *)context;
  if ((current_anchor < (MAX_HTML_HISTORY - 1)) &&
      (anchors[current_anchor + 1]))
    {
      current_anchor++;
      load_html(ss, html_files[current_anchor], anchors[current_anchor], FALSE);
      XtSetSensitive(back_button, TRUE);
      if ((current_anchor == (MAX_HTML_HISTORY - 1)) ||
	  (anchors[current_anchor + 1] == NULL))
	XtSetSensitive(forward_button, FALSE);
    }
  if (!XtIsManaged(help_dialog)) XtManageChild(help_dialog);
}

static void anchorCB(Widget widget, XtPointer client_data, XmHTMLAnchorCallbackStruct *cbs)
{
  snd_state *ss;
  if (cbs->reason != XmCR_ACTIVATE) return;
  if (cbs->url_type == ANCHOR_FILE_LOCAL)
    {
      ss = get_global_state();
      /* all contents of cbs struct are pointers into current text, so we have to save them before changing that text */
      if (strncmp(cbs->href, "extsnd", 6) == 0)
	load_html(ss, EXTSND_HTML, (char *)(cbs->href + 11), TRUE);
      else 
	if (strncmp(cbs->href, "grfsnd", 6) == 0)
	  load_html(ss, GRFSND_HTML, (char *)(cbs->href + 11), TRUE);
	else 
	  if (strncmp(cbs->href, "sndlib", 6) == 0)
	    load_html(ss, SNDLIB_HTML, (char *)(cbs->href + 11), TRUE);
	  else 
	    if (strncmp(cbs->href, "sndscm", 6) == 0)
	      load_html(ss, SNDSCM_HTML, (char *)(cbs->href + 11), TRUE);
	    else 
	      if (strncmp(cbs->href, "snd", 3) == 0)
		load_html(ss, SND_HTML, (char *)(cbs->href + 8), TRUE);
	      else 
		if (strncmp(cbs->href, "clm", 3) == 0)
		  load_html(ss, CLM_HTML, (char *)(cbs->href + 8), TRUE);
    }
  else
    {
      cbs->doit = True;
      cbs->visited = True;
    }
}

static XmImageInfo* loadImage(Widget w, String url, Dimension width, Dimension height, XtPointer client_data)
{
  /* fix up path if necessary */
  char *buf;
  snd_state *ss;
  XmImageInfo *result;
  ss = get_global_state();
  if (html_dir(ss) && (*(html_dir(ss))))
    {
      buf = (char *)CALLOC(256, sizeof(char));
      mus_snprintf(buf, 256, "%s/%s", html_dir(ss), url);
      result = XmHTMLImageDefaultProc(w, buf, NULL, 0);
      FREE(buf);
    }
  else result = XmHTMLImageDefaultProc(w, url, NULL, 0);
  return(result);
}
            
#endif

static void help_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  help_dialog_help((snd_state *)context);
}

#if (!HAVE_HTML)
static void hit_help(Widget w, XtPointer context, XtPointer info) 
{
  char *selection;
  selection = XmTextGetSelection(w);
  if (selection)
    {
      /* find help relating to selection and post it */
#ifdef SCM_MODULE_OBARRAY
      {
	SCM help_text;
	help_text = g_help(TO_SCM_STRING(selection),
			   widget_width(w));
	if (STRING_P(help_text))
	  snd_help(get_global_state(),
		   selection,
		   TO_C_STRING(help_text));
      }
#endif
      XtFree(selection);
    }
}
#endif

static void create_help_monolog(snd_state *ss)
{
  /* create scrollable but not editable text window */
  Arg args[20];
  int n;
  XmString titlestr;
#if HAVE_HTML
  Widget ww;
  int i;
  XmString forwardstr, backstr;
#endif

  titlestr = XmStringCreate(STR_Help, XmFONTLIST_DEFAULT_TAG);
#if HAVE_HTML
  backstr = XmStringCreate(STR_Back, XmFONTLIST_DEFAULT_TAG);
  forwardstr = XmStringCreate(STR_Forward, XmFONTLIST_DEFAULT_TAG);
#endif

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
  XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
  /* this window should be resizable by the user (i.e. have the resize bars), but not resize itself */
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
  XtSetArg(args[n], XmNnoResize, FALSE); n++;
  XtSetArg(args[n], XmNtransient, FALSE); n++;
  help_dialog = XmCreateMessageDialog(MAIN_PANE(ss), "snd-help", args, n);
  set_dialog_widget(HELP_DIALOG, help_dialog);
  add_dialog(ss, help_dialog);
#if OVERRIDE_TOGGLE
  override_form_translation(help_dialog);
#endif

  n = 0;
  if (!(ss->using_schemes)) 
  {
    XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;
    XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;
  }
#if HAVE_HTML
  for (i = 0; i < MAX_HTML_HISTORY; i++) anchors[i] = NULL;
  back_button = XtCreateManagedWidget(STR_Back, xmPushButtonWidgetClass, help_dialog, args, n);
  XtAddCallback(back_button, XmNactivateCallback, back_anchor_callback, ss);
  XtSetSensitive(back_button, FALSE);
  forward_button = XtCreateManagedWidget(STR_Forward, xmPushButtonWidgetClass, help_dialog, args, n);
  XtAddCallback(forward_button, XmNactivateCallback, forward_anchor_callback, ss);
  XtSetSensitive(forward_button, FALSE);
#endif

  XtUnmanageChild(XmMessageBoxGetChild(help_dialog, XmDIALOG_CANCEL_BUTTON));
  XtUnmanageChild(XmMessageBoxGetChild(help_dialog, XmDIALOG_SYMBOL_LABEL));
  XtAddCallback(help_dialog, XmNhelpCallback, help_help_callback, ss);
      
  XmStringFree(titlestr);
#if HAVE_HTML
  XmStringFree(backstr);
  XmStringFree(forwardstr);
#endif

  n = 0;
#if HAVE_HTML
  XtSetArg(args[n], XmNwidth, html_width(ss)); n++;
  XtSetArg(args[n], XmNheight, html_height(ss)); n++;
  XtSetArg(args[n], XmNfontSizeList, html_font_size_list(ss)); n++;
  XtSetArg(args[n], XmNfontSizeFixedList, html_fixed_font_size_list(ss)); n++;
  XtSetArg(args[n], XmNimageProc, loadImage); n++;
  help_text = XtCreateManagedWidget("html", xmHTMLWidgetClass, help_dialog, args, n);
  XtAddCallback(help_text, XmNactivateCallback, (XtCallbackProc)anchorCB, NULL);
#else
  XtSetArg(args[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;
  XtSetArg(args[n], XmNeditable, FALSE); n++;
  XtSetArg(args[n], XmNcolumns, HELP_COLUMNS); n++;
  XtSetArg(args[n], XmNrows, HELP_ROWS); n++;
  XtSetArg(args[n], XM_FONT_RESOURCE, HELP_TEXT_FONT(ss)); n++;
  if (!(ss->using_schemes))
    {
      XtSetArg(args[n], XmNforeground, (ss->sgx)->black); n++; /* needed if color allocation fails completely */
      XtSetArg(args[n], XmNbackground, (ss->sgx)->white); n++;
    }
  help_text = XmCreateScrolledText(help_dialog, "help-text", args, n);
  XtAddCallback(help_text,XmNgainPrimaryCallback,(XtCallbackProc)hit_help,NULL);
  XtManageChild(help_text);
#endif

  XtManageChild(help_dialog);

  if (!(ss->using_schemes))
    {
      map_over_children(help_dialog, set_main_color_of_widget, (void *)ss);
      XtVaSetValues(help_text, XmNbackground, (ss->sgx)->white, XmNforeground, (ss->sgx)->black, NULL);
      XtVaSetValues(XmMessageBoxGetChild(help_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(help_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
#if HAVE_HTML
      XtVaGetValues(help_text, XmNworkWindow, &ww, NULL);
      XtVaSetValues(ww, XmNbackground, (ss->sgx)->white, NULL);
      XtVaSetValues(forward_button, XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
      XtVaSetValues(back_button, XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
#endif
    }
}

void snd_help(snd_state *ss, char *subject, char *helpstr)
{
  /* place help string in scrollable help window */
  /* if window is already active, add this help at the top and reposition */
  /* if XmHTML, this is writing the text to the root window if called from Guile?!? */
  XmString xstr1;
#if HAVE_HTML
  char *newhelp;
#endif
  if (!(help_dialog)) 
    create_help_monolog(ss); 
  else 
    {
      /* XtVaSetValues(help_text, XM_FONT_RESOURCE, HELP_TEXT_FONT(ss), NULL); */ /* in case it changed */ 
      /* this causes segfaults for no reason that I can see */
      raise_dialog(help_dialog);
    }

  mus_snprintf(help_window_label, 64, "%s help", subject);
  xstr1 = XmStringCreate(help_window_label, XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(help_dialog, XmNmessageString, xstr1, NULL);
#if HAVE_HTML
  if (helpstr[0] == '#')
    load_html(ss, SND_HTML, helpstr, TRUE);
  else
    {
      if (strncmp(helpstr, "snd.html", 8) == 0)
	{
	  if (strlen(helpstr) > 9)
	    load_html(ss, SND_HTML, (char *)(helpstr + 8), TRUE);
	  else load_html(ss, SND_HTML, NULL, TRUE);
	}
      else
	{
	  if (strncmp(helpstr, "extsnd.html", 11) == 0)
	    {
	      if (strlen(helpstr) > 12)
		load_html(ss, EXTSND_HTML, (char *)(helpstr + 11), TRUE);
	      else load_html(ss, EXTSND_HTML, NULL, TRUE);
	    }
	  else
	    {
	      if (strncmp(helpstr, "sndlib.html", 11) == 0)
		{
		  if (strlen(helpstr) > 12)
		    load_html(ss, SNDLIB_HTML, (char *)(helpstr + 11), TRUE);
		  else load_html(ss, SNDLIB_HTML, NULL, TRUE);
		}
	      else
		{
		  if (strncmp(helpstr, "clm.html", 8) == 0)
		    {
		      if (strlen(helpstr) > 9)
			load_html(ss, CLM_HTML, (char *)(helpstr + 8), TRUE);
		      else load_html(ss, CLM_HTML, NULL, TRUE);
		    }	
		  else
		    {
		      if (strncmp(helpstr, "grfsnd.html", 11) == 0)
			{
			  if (strlen(helpstr) > 12)
			    load_html(ss, GRFSND_HTML, (char *)(helpstr + 11), TRUE);
			  else load_html(ss, GRFSND_HTML, NULL, TRUE);
			}
		      else
			{
			  newhelp = (char *)CALLOC(strlen(helpstr) + 64, sizeof(char));
			  sprintf(newhelp, "<html><body><pre>%s</pre></body></html>", helpstr);
			  XmHTMLTextSetString(help_text, newhelp);
			  html_loaded = NO_HTML;
			  FREE(newhelp);
			}
		    }
		}
	    }
	}
    }
#else
  XmTextSetString(help_text, helpstr);
#endif
  if (!XtIsManaged(help_dialog)) 
    XtManageChild(help_dialog);
  XmStringFree(xstr1);
}

