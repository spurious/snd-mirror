#include "snd.h"

/* TODO:   add minihistory support in listener
 * TODO    bubble args help if tab at end of name? (or click name?)
 */

static Widget listener_text = NULL;

static Widget completion_help_dialog = NULL, completion_help_list = NULL;

static void completion_help_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_state *ss = (snd_state *)context;
  snd_help(ss, "completion",
"These are the completions that Snd thinks might be likely.\n\
If you select one, it will be used to complete the current name.\n\
");
}

static void completion_help_browse_callback(Widget w, XtPointer context, XtPointer info) 
{
  int choice, i, j, old_len, new_len;
  char *text = NULL, *old_text = NULL;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  choice = cbs->item_position - 1;
  XmStringGetLtoR(cbs->item, XmFONTLIST_DEFAULT_TAG, &text);
  old_text = XmTextGetString(listener_text);
  old_len = snd_strlen(old_text);
  new_len = snd_strlen(text);
  for (i = old_len-1, j = new_len-1; j >= 0; j--)
    {
      if (old_text[i] != text[j])
	i = old_len - 1;
      else i--;
    }
  append_listener_text(XmTextGetLastPosition(listener_text), 
		       (char *)(text - 1 + old_len - i));
  if (text) XtFree(text);
  if (old_text) XtFree(old_text);
  XtUnmanageChild(completion_help_dialog);
}

static void create_completion_help_dialog(snd_state *ss, char *title)
{
  Arg args[20];
  int n;
  XmString titlestr;
  titlestr = XmStringCreate(title, XmFONTLIST_DEFAULT_TAG);

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
  XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
  XtSetArg(args[n], XmNnoResize, FALSE); n++;
  /* XtSetArg(args[n], XmNtransient, FALSE); n++; */
  completion_help_dialog = XmCreateMessageDialog(MAIN_PANE(ss), "snd-completion-help", args, n);
  set_dialog_widget(COMPLETION_DIALOG, completion_help_dialog);
  add_dialog(ss, completion_help_dialog);
#if OVERRIDE_TOGGLE
  override_form_translation(completion_help_dialog);
#endif

  XtUnmanageChild(XmMessageBoxGetChild(completion_help_dialog, XmDIALOG_CANCEL_BUTTON));
  XtUnmanageChild(XmMessageBoxGetChild(completion_help_dialog, XmDIALOG_SYMBOL_LABEL));
  XmStringFree(titlestr);

  n = 0;
  completion_help_list = XmCreateScrolledList(completion_help_dialog, "completion-help-text", args, n);
  if (!(ss->using_schemes)) XtVaSetValues(completion_help_list, XmNbackground, (ss->sgx)->white, XmNforeground, (ss->sgx)->black, NULL);

  XtManageChild(completion_help_list);

  XtAddCallback(completion_help_list, XmNbrowseSelectionCallback, completion_help_browse_callback, ss);
  XtAddCallback(completion_help_dialog, XmNhelpCallback, completion_help_help_callback, ss);

  XtManageChild(completion_help_dialog);

  if (!(ss->using_schemes))
    {
      map_over_children(completion_help_dialog, set_main_color_of_widget, (void *)ss);
      XtVaSetValues(completion_help_list, XmNbackground, (ss->sgx)->white, XmNforeground, (ss->sgx)->black, NULL);
      XtVaSetValues(XmMessageBoxGetChild(completion_help_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(completion_help_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
    }
}

void snd_completion_help(snd_state *ss, int matches, char **buffer)
{
  int i;
  Dimension w, h;
  XmString *match;
  if (completion_help_dialog)
    XtManageChild(completion_help_dialog);
  else create_completion_help_dialog(ss, "Completions");
  match = (XmString *)CALLOC(matches, sizeof(XmString));
  for (i = 0; i < matches; i++) 
    match[i] = XmStringCreate(buffer[i], XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(completion_help_list, 
		XmNitems, match, 
		XmNitemCount, matches, 
		XmNvisibleItemCount, iclamp(1, matches, 12), 
		NULL);
  XtVaGetValues(completion_help_list, 
		XmNwidth, &w, 
		XmNheight, &h, 
		NULL);
  if ((w < 100) || (h < 100)) 
    XtVaSetValues(completion_help_dialog, 
		  XmNwidth, 200,
		  XmNheight, 200, 
		  NULL);
  for (i = 0; i < matches; i++) 
    XmStringFree(match[i]);
  FREE(match);
}



/* ---------------- text widget specializations ---------------- */

void textfield_focus_Callback(Widget w, XtPointer context, XtPointer info)
{
  snd_state *ss = (snd_state *)context;
  if (!(ss->using_schemes)) 
    XtVaSetValues(w, XmNbackground, (ss->sgx)->text_focus_color, NULL);
}

void textfield_unfocus_Callback(Widget w, XtPointer context, XtPointer info)
{
  snd_state *ss = (snd_state *)context;
  if (!(ss->using_schemes)) 
    XtVaSetValues(w, XmNbackground, (ss->sgx)->basic_color, NULL);
}


/* -------- specialized action procs -------- */

static int actions_loaded = 0;
#define CONTROL_KEY 4

static void No_op (Widget w, XEvent *ev, char **str, Cardinal *num) 
{
  /* return does not cause widget activation in many textfield cases -- it is a true no-op */
}

#define snd_K_u XK_u 
#define snd_K_x XK_x 

static void Activate_keyboard (Widget w, XEvent *ev, char **str, Cardinal *num) 
{
  /* make the current channel active preloading kbd cmd with str[0]+ctrl bit */
  chan_info *cp;
  cp = current_channel(get_global_state());
  if (cp) 
    {
      goto_graph(cp);
      keyboard_command(cp, (str[0][0] == 'u') ? snd_K_u : snd_K_x, CONTROL_KEY);
    }
}

static void Activate_channel (Widget w, XEvent *ev, char **str, Cardinal *num) 
{
  /* make the current channel active and abort if anything in progress */
  chan_info *cp;
  snd_state *ss;
  ss = get_global_state();
  clear_listener();
  if ((ss->checking_explicitly) || 
      (play_in_progress())) 
    ss->stopped_explicitly = 1; 
  cp = current_channel(ss);
  if (cp) goto_graph(cp);
}

static char *listener_selection = NULL;

static void Kill_line(Widget w, XEvent *ev, char **str, Cardinal *num) 
{
  /* C-k with storage of killed text */
  XmTextPosition curpos, loc;
  Boolean found;
  curpos = XmTextGetCursorPosition(w);
  found = XmTextFindString(w, curpos, "\n", XmTEXT_FORWARD, &loc);
  if (!found) loc = XmTextGetLastPosition(w);
  if (loc > curpos)
    {
      if (listener_selection) XtFree(listener_selection);
      XmTextSetSelection(w, curpos, loc, CurrentTime);
      listener_selection = XmTextGetSelection(w); /* xm manual p329 sez storage is allocated here */
      XmTextCut(w, CurrentTime);
    }
}

static void Yank(Widget w, XEvent *ev, char **str, Cardinal *num) 
{
  /* copy current selection at current cursor position */
  XmTextPosition curpos;
  if (listener_selection) 
    {
      curpos = XmTextGetCursorPosition(w);
      XmTextInsert(w, curpos, listener_selection);
      curpos+=strlen(listener_selection);
      XmTextShowPosition(w, curpos);
      XmTextSetCursorPosition(w, curpos);
      XmTextClearSelection(w, ev->xkey.time); /* so C-y + edit doesn't forbid the edit */
    }
}

static int last_prompt = 0;

static void Begin_of_line(Widget w, XEvent *ev, char **ustr, Cardinal *num) 
{
  /* don't back up before listener prompt */
  XmTextPosition curpos, loc;
  int prompt_len;
  char *str = NULL;
  snd_state *ss;
  Boolean found;
  curpos = XmTextGetCursorPosition(w) - 1;
  found = XmTextFindString(w, curpos, "\n", XmTEXT_BACKWARD, &loc);
  if (found) 
    {
      ss = get_global_state();
      prompt_len = snd_strlen(listener_prompt(ss));
      str = (char *)CALLOC(prompt_len + 3, sizeof(char));
      XmTextGetSubstring(w, loc+1, prompt_len, prompt_len + 2, str);
      if (strncmp(listener_prompt(ss), str, prompt_len) == 0)
	XmTextSetCursorPosition(w, loc+prompt_len + 1);
      else XmTextSetCursorPosition(w, loc + 1);
      FREE(str);
    }
  else XmTextSetCursorPosition(w, 1);
}

static void Delete_region(Widget w, XEvent *ev, char **str, Cardinal *num) 
{
  XmTextCut(w, CurrentTime);
}

static XmTextPosition down_pos, last_pos;

static void B1_press(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  XmTextPosition pos;
  XButtonEvent *ev = (XButtonEvent *)event;
  snd_state *ss;
  ss = get_global_state();
  XmProcessTraversal(w, XmTRAVERSE_CURRENT);
  /* we're replacing the built-in take_focus action here, so do it by hand, but leave listener blue, so to speak */
  if ((!(ss->using_schemes)) && 
      (w != listener_text))
    XtVaSetValues(w, XmNbackground, (ss->sgx)->white, NULL);
  if (w == listener_text) 
    XmTextClearSelection(listener_text, CurrentTime); /* should this happen in other windows as well? */
  pos = XmTextXYToPos(w, ev->x, ev->y);
  XmTextSetCursorPosition(w, pos);
  down_pos = pos;
  last_pos = pos;
}

static void B1_move(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  XmTextPosition pos;
  XButtonEvent *ev = (XButtonEvent *)event;
  pos = XmTextXYToPos(w, ev->x, ev->y);
  if (last_pos > pos)                                 /* must have backed up the cursor */
    XmTextSetHighlight(w, pos, last_pos, XmHIGHLIGHT_NORMAL);
  if (down_pos != pos)
    XmTextSetHighlight(w, down_pos, pos, XmHIGHLIGHT_SELECTED);
  last_pos = pos;
}

static void B1_release(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  XmTextPosition pos;
  XButtonEvent *ev = (XButtonEvent *)event;
  pos = XmTextXYToPos(w, ev->x, ev->y);
  XmTextSetCursorPosition(w, pos);
  if (down_pos != pos)
    {
      XmTextSetHighlight(w, down_pos, pos, XmHIGHLIGHT_SELECTED);
      if (listener_selection) XtFree(listener_selection);
      XmTextSetSelection(w, down_pos, pos, CurrentTime);
      listener_selection = XmTextGetSelection(w);
    }
}

static void Text_transpose(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  XmTextPosition curpos;
  char buf[3]; /* needs room for null */
  char tmp;
  curpos = XmTextGetCursorPosition(w);
  if (curpos > 1)
    {
      XmTextGetSubstring(w, (XmTextPosition)(curpos - 1), 2, 3, buf);
      tmp = buf[0];
      buf[0] = buf[1];
      buf[1] = tmp;
      XmTextReplace(w, curpos - 1, curpos + 1, buf);
      XmTextSetCursorPosition(w, curpos + 1);
    }
}

static void Word_upper(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  int i, j, length, wstart, wend, up, cap;
  XmTextPosition curpos, endpos;
  char *buf;
  up = (str[0][0] == 'u');
  cap = (str[0][0] == 'c');
  curpos = XmTextGetCursorPosition(w);
  endpos = XmTextGetLastPosition(w);
  if (curpos < endpos)
    {
      length = endpos - curpos;
      buf = (char *)CALLOC(length + 1, sizeof(char));
      XmTextGetSubstring(w, curpos, length, length + 1, buf);
      wstart = 0;
      wend = length;
      for (i = 0; i < length; i++)
	if (!isspace((int)(buf[i])))
	  {
	    wstart = i;
	    break;
	  }
      for (i = wstart+1; i < length; i++)
	if (isspace((int)(buf[i])))
	  {
	    wend = i;
	    break;
	  }
      if (cap)
	{
	  buf[0] = toupper(buf[wstart]);
	  buf[1] = '\0';
	  XmTextReplace(w, curpos + wstart, curpos + wstart + 1, buf);
	}
      else
	{
	  for (i = wstart, j = 0; i < wend; i++, j++)
	    if (up) 
	      buf[j] = toupper(buf[i]);
	    else buf[j] = tolower(buf[i]);
	  buf[j] = '\0';
	  XmTextReplace(w, curpos + wstart, curpos + wend, buf);
	}
      XmTextSetCursorPosition(w, curpos+wend);
    }
}

static Widget *cmpwids = NULL;
static int cmpwids_size = 0;

static void add_completer_widget(Widget w, int row)
{
  int i;
  if (row >= 0)
    {
      if (cmpwids_size <= row)
	{
	  i = cmpwids_size;
	  cmpwids_size = row + 8;
	  if (cmpwids == NULL)
	    cmpwids = (Widget *)CALLOC(cmpwids_size, sizeof(Widget));
	  else 
	    {
	      cmpwids = (Widget *)REALLOC(cmpwids, cmpwids_size * sizeof(Widget));
	      for (; i < cmpwids_size; i++) cmpwids[i] = NULL;
	    }
	}
      cmpwids[row] = w;
    }
}

static void Name_completion(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  int data, i, matches, need_position;
  Position wx, wy;
  int xoff, yoff; 
  Window wn;
  Pixel old_color;
  char *old_text, *new_text, *search_text;
  snd_state *ss;
  ss = get_global_state();
  data = -1;
  for (i = 0; i < cmpwids_size; i++)
    if (w == cmpwids[i])
      {
	data = i;
	break;
      }
  if (data >= 0)
    {
      old_text = XmTextGetString(w);
      new_text = complete_text(old_text, data);
      matches = get_completion_matches();
      XmTextSetString(w, new_text);
      XmTextSetCursorPosition(w, XmTextGetLastPosition(w));
      if ((strcmp(old_text, new_text) == 0) && 
	  (matches != -1))
	{
	  XtVaGetValues(w, XmNforeground, &old_color, NULL);
	  if (matches > 1)
	    XtVaSetValues(w, XmNforeground, (ss->sgx)->green, NULL);
	  else 
	    if (matches == 0) 
	      XtVaSetValues(w, XmNforeground, (ss->sgx)->red, NULL);
	  XmUpdateDisplay(w);
#if HAVE_SLEEP
	  sleep(1);
#endif
	  XtVaSetValues(w, XmNforeground, old_color, NULL);
	  XmUpdateDisplay(w);
	  if (matches > 1) 
	    {
	      set_save_completions(TRUE);
	      search_text = complete_text(old_text, data);
	      if (search_text) FREE(search_text);
	      need_position = (completion_help_dialog == NULL);
	      display_completions(ss);
	      set_save_completions(FALSE);
	      if (need_position)
		{
		  /* try to position the newly popped up help window below the text field */
		  XtVaGetValues(w, XmNx, &wx, XmNy, &wy, NULL);
		  XTranslateCoordinates(XtDisplay(w), XtWindow(w), DefaultRootWindow(XtDisplay(w)), 0, 0, &xoff, &yoff, &wn);
		  wx += xoff; 
		  wy += yoff;
		  XtVaSetValues(completion_help_dialog, XmNx, wx, XmNy, wy+40, NULL);
		}
	    }
	  else
	    {
	      /* (if matches == 0) here we could back up the text looking for the nearest completion */
	    }
	}
      if (old_text) XtFree(old_text);
      if (new_text) FREE(new_text);
    }
}

void append_listener_text(int end, char *msg)
{
  XmTextInsert(listener_text, end, msg);
  XmTextSetCursorPosition(listener_text, XmTextGetLastPosition(listener_text));
}

void save_listener_text(FILE *fp)
{
  char *str = NULL;
  str = XmTextGetString(listener_text);
  if (str)
    {
      fwrite((void *)str, sizeof(char), snd_strlen(str), fp);
      XtFree(str);
    }
}

static void Listener_completion(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  /* used only by the listener widget -- needs to be smart about text since overall string can be enormous 
   *   and we don't want to back up past the last prompt
   *   also if at start of line (or all white-space to previous \n, indent
   */
  int beg, end, len, matches = 0, need_position;
  char *old_text, *new_text = NULL, *file_text = NULL;
  int try_completion = 1;
  Position wx, wy;
  int xoff, yoff; 
  Window wn;
  snd_state *ss;
  ss = get_global_state();
  beg = last_prompt+1;
  end = XmTextGetLastPosition(w);
  if (end <= beg) return;
  len = end - beg + 1;
  old_text = (char *)CALLOC(len + 1, sizeof(char));
  XmTextGetSubstring(w, beg, len, len + 1, old_text);
  /* now old_text is the stuff typed since the last prompt */
  if (old_text)
    {
      new_text = complete_listener_text(old_text, end, &try_completion, &file_text);
      if (try_completion == 0)
	{
	  FREE(old_text);
	  return;
	}
      if (strcmp(old_text, new_text) == 0) 
	matches = get_completion_matches();
      XmTextReplace(w, beg, end, new_text);
      XmTextSetCursorPosition(w, XmTextGetLastPosition(w));
      if (new_text) 
	{
	  FREE(new_text); 
	  new_text = NULL;
	}
      if (matches > 1)
	{
	  clear_possible_completions();
	  set_save_completions(TRUE);
	  if (file_text) 
	    new_text = filename_completer(file_text);
	  else new_text = command_completer(old_text);
	  if (new_text) 
	    {
	      FREE(new_text); 
	      new_text = NULL;
	    }
	  need_position = (completion_help_dialog == NULL);
	  display_completions(ss);
	  set_save_completions(FALSE);
	  if (need_position)
	    {
	      /* try to position the newly popped up help window below the text field */
	      XtVaGetValues(w, XmNx, &wx, XmNy, &wy, NULL);
	      XTranslateCoordinates(XtDisplay(w), XtWindow(w), DefaultRootWindow(XtDisplay(w)), 0, 0, &xoff, &yoff, &wn);
	      wx += xoff; 
	      wy += yoff;
	      XtVaSetValues(completion_help_dialog, XmNx, wx, XmNy, wy + 140, NULL);
	    }
	  if (file_text) FREE(file_text);
	}
      if (old_text) FREE(old_text);
    }
}

#define NUM_ACTS 14
static XtActionsRec acts[] = {
  {"no-op", No_op},
  {"activate-keyboard", Activate_keyboard},
  {"activate-channel", Activate_channel},
  {"yank", Yank},
  {"delete-region", Delete_region},
  {"kill-line", Kill_line},
  {"begin-of-line", Begin_of_line},
  {"b1-press", B1_press},
  {"b1-move", B1_move},
  {"b1-release", B1_release},
  {"text-transpose", Text_transpose},
  {"word-upper", Word_upper},
  {"name-completion", Name_completion},
  {"listener-completion", Listener_completion},
};

/* translation tables for emacs compatibility and better inter-widget communication */

/* for textfield (single-line) widgets */
static char TextTrans2[] =
       "Ctrl <Key>a:	    beginning-of-line()\n\
	Ctrl <Key>b:	    backward-character()\n\
	Mod1 <Key>b:	    backward-word()\n\
	Mod1 <Key>c:	    word-upper(c)\n\
	Ctrl <Key>d:	    delete-next-character()\n\
	Mod1 <Key>d:	    delete-next-word()\n\
	Ctrl <Key>e:	    end-of-line()\n\
	Ctrl <Key>f:	    forward-character()\n\
	Mod1 <Key>f:	    forward-word()\n\
	Ctrl <Key>g:	    activate()\n\
	Ctrl <Key>h:	    delete-previous-character()\n\
	Ctrl <Key>k:	    delete-to-end-of-line()\n\
	Mod1 <Key>l:	    word-upper(l)\n\
	Mod1 <Key>n:	    activate()\n\
	Mod1 <Key>p:	    activate()\n\
        Ctrl <Key>r:        activate()\n\
        Ctrl <Key>s:        activate()\n\
	Ctrl <Key>t:	    text-transpose()\n\
	Mod1 <Key>u:	    word-upper(u)\n\
	Mod1 <Key><:	    beginning-of-line()\n\
	Mod1 <Key>>:	    end-of-line()\n\
	<Key>Delete:	    delete-previous-character()\n\
	Mod1 <Key>Delete:   delete-to-start-of-line()\n\
	<Key>Tab:	    name-completion()\n\
	<Key>Return:	    activate()\n";
static XtTranslations transTable2 = NULL;

/* same but try to avoid causing the currently active pushbutton widget to appear to be activated by <cr> in the text widget */
static char TextTrans6[] =
       "Ctrl <Key>a:	    beginning-of-line()\n\
	Ctrl <Key>b:	    backward-character()\n\
	Mod1 <Key>b:	    backward-word()\n\
	Mod1 <Key>c:	    word-upper(c)\n\
	Ctrl <Key>d:	    delete-next-character()\n\
	Mod1 <Key>d:	    delete-next-word()\n\
	Ctrl <Key>e:	    end-of-line()\n\
	Ctrl <Key>f:	    forward-character()\n\
	Mod1 <Key>f:	    forward-word()\n\
	Ctrl <Key>g:	    no-op()\n\
	Ctrl <Key>h:	    delete-previous-character()\n\
	Ctrl <Key>k:	    delete-to-end-of-line()\n\
	Mod1 <Key>l:	    word-upper(l)\n\
	Ctrl <Key>t:	    text-transpose()\n\
	Mod1 <Key>u:	    word-upper(u)\n\
	Mod1 <Key><:	    beginning-of-line()\n\
	Mod1 <Key>>:	    end-of-line()\n\
	<Key>Delete:	    delete-previous-character()\n\
	Mod1 <Key>Delete:   delete-to-start-of-line()\n\
	<Key>Tab:	    name-completion()\n\
	<Key>Return:	    no-op()\n";
static XtTranslations transTable6 = NULL;

/* for text (multi-line) widgets */
static char TextTrans3[] =
       "Ctrl <Key>a:	    beginning-of-line()\n\
	Ctrl <Key>b:	    backward-character()\n\
	Mod1 <Key>b:	    backward-word()\n\
	Mod1 <Key>c:	    word-upper(c)\n\
	Ctrl <Key>d:	    delete-next-character()\n\
	Mod1 <Key>d:	    delete-next-word()\n\
	Ctrl <Key>e:	    end-of-line()\n\
	Ctrl <Key>f:	    forward-character()\n\
	Mod1 <Key>f:	    forward-word()\n\
	Ctrl <Key>g:	    activate()\n\
	Ctrl <Key>h:	    delete-previous-character()\n\
	Ctrl <Key>j:	    newline-and-indent()\n\
	Ctrl <Key>k:	    kill-line()\n\
	Mod1 <Key>l:	    word-upper(l)\n\
	Ctrl <Key>n:	    next-line()\n\
	Ctrl <Key>o:	    newline-and-backup()\n\
	Ctrl <Key>p:	    previous-line()\n\
	Ctrl <Key>t:	    text-transpose()\n\
	Mod1 <Key>u:	    word-upper(u)\n\
	Ctrl <Key>v:	    next-page()\n\
	Mod1 <Key>v:	    previous-page()\n\
	Ctrl <Key>w:	    delete-region()\n\
	Ctrl <Key>y:	    yank()\n\
	Ctrl <Key>z:	    activate()\n\
	Mod1 <Key>[:	    backward-paragraph()\n\
	Mod1 <Key>]:	    forward-paragraph()\n\
	Mod1 <Key><:	    beginning-of-file()\n\
	Mod1 <Key>>:	    end-of-file()\n\
	<Key>Delete:	    delete-previous-character()\n\
	Mod1 <Key>Delete:   delete-to-start-of-line()\n\
	Ctrl <Key>osfLeft:  page-left()\n\
	Ctrl <Key>osfRight: page-right()\n\
	Ctrl <Key>osfDown:  next-page()\n\
	Ctrl <Key>osfUp:    previous-page()\n\
	Ctrl <Key>space:    set-anchor()\n\
	<Btn1Down>:	    b1-press()\n\
	<Btn1Up>:	    b1-release()\n\
	<Btn1Motion>:	    b1-move()\n\
	<Key>Return:	    newline()\n";
static XtTranslations transTable3 = NULL;

/* for lisp listener */
static char TextTrans4[] =
       "Ctrl <Key>a:	    begin-of-line()\n\
	Ctrl <Key>b:	    backward-character()\n\
	Mod1 <Key>b:	    backward-word()\n\
	Mod1 <Key>c:	    word-upper(c)\n\
	Ctrl <Key>d:	    delete-next-character()\n\
	Mod1 <Key>d:	    delete-next-word()\n\
	Ctrl <Key>e:	    end-of-line()\n\
	Ctrl <Key>f:	    forward-character()\n\
	Mod1 <Key>f:	    forward-word()\n\
	Ctrl <Key>g:	    activate-channel()\n\
	Ctrl <Key>h:	    delete-previous-character()\n\
	Ctrl <Key>j:	    newline-and-indent()\n\
	Ctrl <Key>k:	    kill-line()\n\
	Ctrl <Key>l:	    redraw-display()\n\
	Mod1 <Key>l:	    word-upper(l)\n\
	Ctrl <Key>n:	    next-line()\n\
	Ctrl <Key>o:	    newline-and-backup()\n\
	Ctrl <Key>p:	    previous-line()\n\
	Ctrl <Key>t:	    text-transpose()\n\
	Ctrl <Key>u:	    activate-keyboard(u)\n\
	Mod1 <Key>u:	    word-upper(u)\n\
	Ctrl <Key>v:	    next-page()\n\
	Mod1 <Key>v:	    previous-page()\n\
	Ctrl <Key>w:	    delete-region()\n\
	Ctrl <Key>x:	    activate-keyboard(x)\n\
	Ctrl <Key>y:	    yank()\n\
	Ctrl <Key>z:	    activate()\n\
	Mod1 <Key>[:	    backward-paragraph()\n\
	Mod1 <Key>]:	    forward-paragraph()\n\
	Mod1 <Key><:	    beginning-of-file()\n\
	Mod1 <Key>>:	    end-of-file()\n\
	<Key>Delete:	    delete-previous-character()\n\
	Mod1 <Key>Delete:   delete-to-start-of-line()\n\
	Ctrl <Key>osfLeft:  page-left()\n\
	Ctrl <Key>osfRight: page-right()\n\
	Ctrl <Key>osfDown:  next-page()\n\
	Ctrl <Key>osfUp:    previous-page()\n\
	Ctrl <Key>space:    set-anchor()\n\
	<Btn1Down>:	    b1-press()\n\
	<Btn1Up>:	    b1-release()\n\
	<Btn1Motion>:	    b1-move()\n\
	<Key>Tab:	    listener-completion()\n\
	<Key>Return:	    activate()\n";
static XtTranslations transTable4 = NULL;


/* -------- text related widgets -------- */

static void remember_event(Widget w, XtPointer context, XtPointer info) 
{
  snd_state *ss = (snd_state *)context;
  XmAnyCallbackStruct *cb = (XmAnyCallbackStruct *)info;
  (ss->sgx)->text_activate_event = cb->event;
  (ss->sgx)->text_widget = w;
}

Widget sndCreateTextFieldWidget(snd_state *ss, char *name, Widget parent, Arg *args, int n, int activatable, int completer)
{
  /* white background when active, emacs translations, text_activate_event in ss->sgx for subsequent activation check */
  Widget df;
  XtCallbackList n1;
  if (!actions_loaded) 
    {
      XtAppAddActions(MAIN_APP(ss), acts, NUM_ACTS); 
      actions_loaded = 1;
    }
  XtSetArg(args[n], XmNactivateCallback, n1 = make_callback_list(remember_event, (XtPointer)ss)); n++;
  /* can't use XmNuserData here because it is in use elsewhere (snd-xmix.c) */
  df = XtCreateManagedWidget(name, xmTextFieldWidgetClass, parent, args, n);
  XtAddCallback(df, XmNfocusCallback, textfield_focus_Callback, ss);
  XtAddCallback(df, XmNlosingFocusCallback, textfield_unfocus_Callback, ss);
  if (activatable == ACTIVATABLE)
    {
      if (!transTable2) 
	transTable2 = XtParseTranslationTable(TextTrans2);
      XtOverrideTranslations(df, transTable2);
    }
  else
    {
      if (!transTable6) 
	transTable6 = XtParseTranslationTable(TextTrans6);
      XtOverrideTranslations(df, transTable6);
    }
  add_completer_widget(df, completer);
  FREE(n1);
  return(df);
}

void add_completer_to_textfield(snd_state *ss, Widget w, int completer)
{
  /* used to make file selection dialog act like other text field widgets */
  if (!actions_loaded) 
    {
      XtAppAddActions(MAIN_APP(ss), acts, NUM_ACTS); 
      actions_loaded = 1;
    }
  if (!transTable2) 
    transTable2 = XtParseTranslationTable(TextTrans2);
  XtOverrideTranslations(w, transTable2);
  add_completer_widget(w, completer);
}

Widget sndCreateTextWidget(snd_state *ss, char *name, Widget parent, Arg *args, int n)
{
  /* white background when active, emacs translations, text_activate_event in ss->sgx for subsequent activation check */
  Widget df;
  XtCallbackList n1;
  if (!actions_loaded) 
    {
      XtAppAddActions(MAIN_APP(ss), acts, NUM_ACTS); 
      actions_loaded = 1;
    }
  XtSetArg(args[n], XmNactivateCallback, n1 = make_callback_list(remember_event, (XtPointer)ss)); n++;
  XtSetArg(args[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;
  df = XmCreateScrolledText(parent, name, args, n);
  XtManageChild(df);
  /* df = XtCreateManagedWidget(name, xmTextWidgetClass, parent, args, n); */
  /* XtAddCallback(df, XmNfocusCallback, textfield_focus_Callback, ss); */
  XtAddCallback(df, XmNlosingFocusCallback, textfield_unfocus_Callback, ss);
  /* b1_press action overrides focus */
  if (!transTable3) 
    transTable3 = XtParseTranslationTable(TextTrans3);
  XtOverrideTranslations(df, transTable3);
  FREE(n1);
  return(df);
}


/* ---------------- command widget replacement ---------------- */

static Widget lisp_window = NULL;

void snd_append_char(snd_state *ss, char *msg)
{
  if (listener_text)
    XmTextInsert(listener_text, XmTextGetLastPosition(listener_text), msg);
}
 
static Widget listener_pane = NULL; 

void snd_append_command(snd_state *ss, char *msg)
{
  int cmd_eot;
  if (listener_text)
    {
      if (ss->result_printout != PLAIN_MESSAGE) 
	XmTextInsert(listener_text, XmTextGetLastPosition(listener_text), "\n");
      if (msg)
	XmTextInsert(listener_text, XmTextGetLastPosition(listener_text), msg);
      cmd_eot = XmTextGetLastPosition(listener_text);
      if (ss->result_printout == MESSAGE_WITH_CARET) 
	XmTextInsert(listener_text, cmd_eot, listener_prompt_with_cr(ss));
      ss->result_printout = 0;
      cmd_eot = XmTextGetLastPosition(listener_text);
      last_prompt = cmd_eot - 1;
      XmTextShowPosition(listener_text, cmd_eot - 1);
      /*      XmUpdateDisplay(listener_text); */ /* causes segfaults in libefence!! */
    }
}

static void Command_Return_Callback(Widget w, XtPointer context, XtPointer info)
{
  command_return(w, (snd_state *)context, last_prompt);
}

static int last_highlight_position = -1;

static void Command_Motion_Callback(Widget w, XtPointer context, XtPointer info)
{
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;
  snd_state *ss = (snd_state *)context;
  char *str = NULL, *prompt;
  int pos, i, parens;
  cbs->doit = TRUE; 
  if (last_highlight_position != -1)
    {
      XmTextSetHighlight(w, last_highlight_position, last_highlight_position + 1, XmHIGHLIGHT_NORMAL);
      last_highlight_position = -1;
    }
  pos = cbs->newInsert - 1;
  if (pos > 0)
    {
      prompt = listener_prompt(ss);
      str = XmTextGetString(w);
      /* this can be confused by comments, quoted parens, and string constants */
      if (str[pos] == ')')
	{
	  parens = 1;
	  for (i = pos-1; i > 0; i--)
	    {
	      if ((i > 0) && (str[i] == prompt[0]) && (str[i - 1] == '\n'))
		break;
	      if (str[i] == ')') parens++;
	      if (str[i] == '(') parens--;
	      if (parens == 0)
		{
		  XmTextSetHighlight(w, i, i + 1, XmHIGHLIGHT_SECONDARY_SELECTED);
		  last_highlight_position = i;
		  break;
		}
	    }
	}
      if (str) XtFree(str);
    }
}

static void Command_Modify_Callback(Widget w, XtPointer context, XtPointer info)
{
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;
  snd_state *ss = (snd_state *)context;
  char *str = NULL, *prompt;
  int len;
  if ((cbs->text)->length > 0)
    cbs->doit = TRUE;
  else
    { 
      if (cbs->currInsert < 2) 
	cbs->doit = FALSE;
      else
	{
	  prompt = listener_prompt(ss);
	  str = XmTextGetString(w);
	  len = XmTextGetLastPosition(w);
	  if ( ((str[cbs->currInsert-1] == prompt[0]) && 
		(str[cbs->currInsert - 2] == '\n')) ||
	       ((cbs->currInsert < (len - 1)) && 
		(str[cbs->currInsert] == prompt[0]) && 
		(str[cbs->currInsert - 1] == '\n')))
	    cbs->doit = FALSE;
	  else cbs->doit = TRUE;
	  if (str) XtFree(str);
	}
    }
}

static void Command_Help_Callback(Widget w, XtPointer context, XtPointer info)
{
  listener_dialog_help((snd_state *)context);
}

static SCM mouse_enter_listener_hook, mouse_leave_listener_hook;

static void listener_focus_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  if (HOOKED(mouse_enter_listener_hook))
    g_c_run_progn_hook(mouse_enter_listener_hook,
		       SCM_LIST1(SND_WRAP(listener_text)), /* not w */
		       S_mouse_enter_listener_hook);
}

static void listener_unfocus_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  if (HOOKED(mouse_leave_listener_hook))
    g_c_run_progn_hook(mouse_leave_listener_hook,
		       SCM_LIST1(SND_WRAP(listener_text)), /* not w */
		       S_mouse_leave_listener_hook);
}

static void sndCreateCommandWidget(snd_state *ss, int height)
{
  Arg args[32];
  Widget wv, wh;
  XtCallbackList n1;
  int n;
  if (!listener_text)
    {
      if (!actions_loaded) {XtAppAddActions(MAIN_APP(ss), acts, NUM_ACTS); actions_loaded = 1;}

      n = 0;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNheight, height); n++;
      if ((sound_style(ss) == SOUNDS_IN_NOTEBOOK) || (sound_style(ss) == SOUNDS_HORIZONTAL))
	listener_pane = sndCreateFormWidget("frm", SOUND_PANE_BOX(ss), args, n);
      else listener_pane = sndCreateFormWidget("frm", SOUND_PANE(ss), args, n);
      /* this widget is not redundant at least in Metroworks Motif */

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->listener_color); n++;
	  XtSetArg(args[n], XmNforeground, (ss->sgx)->listener_text_color); n++;
	}
      if ((ss->sgx)->listener_fontlist) {XtSetArg(args[n], XM_FONT_RESOURCE, (ss->sgx)->listener_fontlist); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNactivateCallback, n1 = make_callback_list(remember_event, (XtPointer)ss)); n++;
      XtSetArg(args[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;
      XtSetArg(args[n], XmNskipAdjust, TRUE); n++;
      XtSetArg(args[n], XmNvalue, listener_prompt(ss)); n++;
      XtSetArg(args[n], XmNpendingDelete, FALSE); n++; /* don't cut selection upon paste */
      XtSetArg(args[n], XmNpositionIndex, XmLAST_POSITION); n++;
      XtSetArg(args[n], XmNpaneMinimum, height); n++;
      listener_text = XmCreateScrolledText(listener_pane, "lisp-listener", args, n);
      set_dialog_widget(LISTENER_PANE, listener_text);

      XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, FALSE, NULL);

      XtManageChild(listener_text);
      XmTextSetCursorPosition(listener_text, 1);
      if (!transTable4) 
	transTable4 = XtParseTranslationTable(TextTrans4);
      XtOverrideTranslations(listener_text, transTable4);
      last_prompt = 0;
      XtAddCallback(listener_text, XmNactivateCallback, Command_Return_Callback, ss);
      XtAddCallback(listener_text, XmNmodifyVerifyCallback, Command_Modify_Callback, ss);
      XtAddCallback(listener_text, XmNmotionVerifyCallback, Command_Motion_Callback, ss);
      XtAddCallback(listener_text, XmNhelpCallback, Command_Help_Callback, ss);
      FREE(n1);
      lisp_window = XtParent(listener_text);
      XtAddEventHandler(lisp_window, EnterWindowMask, FALSE, listener_focus_callback, NULL);
      XtAddEventHandler(lisp_window, LeaveWindowMask, FALSE, listener_unfocus_callback, NULL);
      
      if (!(ss->using_schemes))
	{
	  XmChangeColor(lisp_window, (ss->sgx)->basic_color);
	  XtVaGetValues(lisp_window, XmNverticalScrollBar, &wv, XmNhorizontalScrollBar, &wh, NULL);
	  XmChangeColor(wv, (ss->sgx)->basic_color);
	  XmChangeColor(wh, (ss->sgx)->basic_color);
	  map_over_children(SOUND_PANE(ss), color_sashes, (void *)ss);
	}
      XtVaSetValues(listener_pane, XmNpaneMinimum, 60, NULL);
      XtManageChild(listener_pane);
      XtVaSetValues(listener_pane, XmNpaneMinimum, 1, NULL);

      if (auto_resize(ss))
	XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, TRUE, NULL);
    }
}

void goto_listener(void) 
{
  goto_window(listener_text);
  XmTextSetCursorPosition(listener_text, XmTextGetLastPosition(listener_text) + 1);
  XmTextSetInsertionPosition(listener_text, XmTextGetLastPosition(listener_text) + 1);
}

void color_listener(Pixel pix)
{
  snd_state *ss;
  ss = get_global_state();
  (ss->sgx)->listener_color = pix;
  if (listener_text)
    XmChangeColor(listener_text, pix);
}

void color_listener_text(Pixel pix)
{
  snd_state *ss;
  ss = get_global_state();
  (ss->sgx)->listener_text_color = pix;
  if (listener_text)
    XtVaSetValues(listener_text, XmNforeground, pix, NULL);
}

void handle_listener(snd_state *ss, int new_state)
{
  if (!listener_text)
    {
      /* fire up listener at bottom of overall snd window */
      if (new_state == LISTENER_OPEN) 
	{
	  sndCreateCommandWidget(ss, 100);
	  set_view_listener_label(STR_Hide_listener);
	  goto_window(listener_text);
	}
      else sndCreateCommandWidget(ss, 1);
      ss->listening = new_state;
    }
  else
    {
      if (ss->listening == LISTENER_OPEN)
	{
	  /* close listener window but it remains active */
	  ss->listening = LISTENER_LISTENING;
	  set_view_listener_label(STR_Show_listener);
	  XtUnmanageChild(listener_pane);
	  XtVaSetValues(listener_pane, XmNpaneMaximum, 1, XmNpaneMinimum, 1, NULL);
	  XtManageChild(listener_pane);
	  XtVaSetValues(listener_pane, XmNpaneMaximum, LOTSA_PIXELS, XmNpaneMinimum, 1, NULL);
	}
      else
	{
	  /* reopen listener pane */
	  ss->listening = LISTENER_OPEN;
	  set_view_listener_label(STR_Hide_listener);
	  XtUnmanageChild(listener_pane);
	  XtVaSetValues(listener_pane, XmNpaneMinimum, 100, NULL);
	  XtManageChild(listener_pane);
	  XtVaSetValues(listener_pane, XmNpaneMinimum, 1, NULL);
	}
    }
}

int listener_height(void)
{
  if (listener_text) 
    return(widget_height(listener_text)); 
  else return(0);
}

int listener_width(void)
{
  if (listener_text) 
    return(widget_width(listener_text)); 
  else return(0);
}

#if OVERRIDE_TOGGLE
/* Metrolink Motif defines control-button1 to be "take focus" and then segfaults if you use it!! */
static char ToggleTrans2[] =
       "c<Btn1Down>:   ArmAndActivate()\n";
static XtTranslations toggleTable2 = NULL;

static void override_toggle_translation(Widget w)
{
  if (!toggleTable2) toggleTable2 = XtParseTranslationTable(ToggleTrans2);
  XtOverrideTranslations(w, toggleTable2);
}

static char ToggleTrans3[] =
       "c<Btn1Down>:   DrawingAreaInput()\n";
static XtTranslations toggleTable3 = NULL;

static void override_drawing_translation(Widget w)
{
  if (!toggleTable3) toggleTable3 = XtParseTranslationTable(ToggleTrans3);
  XtOverrideTranslations(w, toggleTable3);
}

static char ToggleTrans4[] =
       "c<Btn1Down>:   Activate()\n";
static XtTranslations toggleTable4 = NULL;

static void override_manager_translation(Widget w)
{
  if (!toggleTable4) toggleTable4 = XtParseTranslationTable(ToggleTrans4);
  XtOverrideTranslations(w, toggleTable4);
}

static char ToggleTrans5[] =
       "c<Btn1Down>:   Return()\n";
static XtTranslations toggleTable5 = NULL;

void override_form_translation(Widget w)
{
  if (!toggleTable5) toggleTable5 = XtParseTranslationTable(ToggleTrans5);
  XtOverrideTranslations(w, toggleTable5);
}

/* push buttons also need the override, but they aren't causing Snd to crash for some reason */

#endif

Widget sndCreateFormWidget(char *name, Widget parent, Arg *args, int n)
{
  Widget w;
  w = XtCreateManagedWidget(name, xmFormWidgetClass, parent, args, n);
#if OVERRIDE_TOGGLE
  override_form_translation(w);
#endif
  return(w);
}

Widget sndCreateToggleButtonWidget(char *name, Widget parent, Arg *args, int n)
{
  Widget w;
  w = XtCreateManagedWidget(name, xmToggleButtonWidgetClass, parent, args, n);
#if OVERRIDE_TOGGLE
  override_toggle_translation(w);
#endif
  return(w);
}

Widget sndCreatePushButtonWidget(char *name, Widget parent, Arg *args, int n)
{
  Widget w;
  w = XtCreateManagedWidget(name, xmPushButtonWidgetClass, parent, args, n);
#if OVERRIDE_TOGGLE
  override_toggle_translation(w);
#endif
  return(w);
}

Widget sndCreateFrameWidget(char *name, Widget parent, Arg *args, int n)
{
  Widget w;
  w = XtCreateManagedWidget(name, xmFrameWidgetClass, parent, args, n);
#if OVERRIDE_TOGGLE
  override_manager_translation(w);
#endif
  return(w);
}

Widget sndCreateRowColumnWidget(char *name, Widget parent, Arg *args, int n)
{
  Widget w;
  w = XtCreateManagedWidget(name, xmRowColumnWidgetClass, parent, args, n);
#if OVERRIDE_TOGGLE
  override_manager_translation(w);
#endif
  return(w);
}

Widget sndCreateDrawingAreaWidget(char *name, Widget parent, Arg *args, int n)
{
  Widget w;
  w = XtCreateManagedWidget(name, xmDrawingAreaWidgetClass, parent, args, n);
#if OVERRIDE_TOGGLE
  override_drawing_translation(w);
#endif
  return(w);
}

Widget sndCreatePanedWindowWidget(char *name, Widget parent, Arg *args, int n)
{
  Widget w;
  w = XtCreateManagedWidget(name, xmPanedWindowWidgetClass, parent, args, n);
#if OVERRIDE_TOGGLE
  override_manager_translation(w);
#endif
  return(w);
}

void g_init_gxlistener(SCM local_doc)
{
  #define H_mouse_enter_listener_hook S_mouse_enter_listener_hook " (snd chn) is called when the mouse \
enters the lisp listener pane:\n\
  (add-hook! mouse-enter-listener-hook\n\
    (lambda (widget)\n\
      (focus-widget widget)))"

  #define H_mouse_leave_listener_hook S_mouse_leave_listener_hook " (snd chn) is called when the mouse \
leaves the lisp listener pane"

  mouse_enter_listener_hook = MAKE_HOOK(S_mouse_enter_listener_hook, 1, H_mouse_enter_listener_hook);    /* arg = listener_text widget */
  mouse_leave_listener_hook = MAKE_HOOK(S_mouse_leave_listener_hook, 1, H_mouse_leave_listener_hook);    /* arg = listener_text widget */
}

