#include "snd.h"

#if (XmVERSION >= 2)
  #define OVERRIDE_TOGGLE 1
  /* Motif 2.0 defines control-button1 to be "take focus" -- this is not a good idea!! */
#else
  #define OVERRIDE_TOGGLE 0
#endif

static Widget listener_text = NULL;

static Widget completion_help_dialog = NULL, completion_help_list = NULL;

static void completion_help_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  completion_dialog_help();
}

static void completion_help_browse_callback(Widget w, XtPointer context, XtPointer info) 
{
  int i, j, old_len, new_len;
  char *text = NULL, *old_text = NULL;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  /* choice = cbs->item_position - 1; */
  XmStringGetLtoR(cbs->item, XmFONTLIST_DEFAULT_TAG, &text);
  if ((ss->sgx->completion_requestor == NULL) || (ss->sgx->completion_requestor == listener_text))
    old_text = XmTextGetString(listener_text);
  else old_text = XmTextGetString(ss->sgx->completion_requestor);
  old_len = snd_strlen(old_text);
  new_len = snd_strlen(text);
  for (i = old_len - 1, j = new_len - 1; j >= 0; j--)
    {
      if (old_text[i] != text[j])
	{
	  i = old_len - 1;
	  if (old_text[i] == text[j]) i--;
	  /* this added 15-Apr-02 for case like map-chan(nel) */
	  /*   probably should go back new_len and scan forwards instead */
	}
      else i--;
    }
  if ((ss->sgx->completion_requestor == NULL) || (ss->sgx->completion_requestor == listener_text))
    append_listener_text(XmTextGetLastPosition(listener_text), 
			 (char *)(text - 1 + old_len - i));
  else
    {
      /* try to append to who(m?)ever asked for completion help */
      XmTextInsert(ss->sgx->completion_requestor, 
		   XmTextGetLastPosition(ss->sgx->completion_requestor), 
		   (char *)(text - 1 + old_len - i));
    }
  if (text) XtFree(text);
  if (old_text) XtFree(old_text);
  XtUnmanageChild(completion_help_dialog);
  ss->sgx->completion_requestor = NULL;
}

static void help_completion_ok_callback(Widget w, XtPointer context, XtPointer info) 
{
  ss->sgx->completion_requestor = NULL;
}

static void create_completion_help_dialog(char *title)
{
  Arg args[20];
  int n;
  XmString titlestr;
  titlestr = XmStringCreate(title, XmFONTLIST_DEFAULT_TAG);

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
  XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
  XtSetArg(args[n], XmNnoResize, false); n++;
  /* XtSetArg(args[n], XmNtransient, false); n++; */
  completion_help_dialog = XmCreateMessageDialog(MAIN_PANE(ss), "snd-completion-help", args, n);

  XtUnmanageChild(XmMessageBoxGetChild(completion_help_dialog, XmDIALOG_CANCEL_BUTTON));
  XtUnmanageChild(XmMessageBoxGetChild(completion_help_dialog, XmDIALOG_SYMBOL_LABEL));
  XmStringFree(titlestr);

  n = 0;
  completion_help_list = XmCreateScrolledList(completion_help_dialog, "completion-help-text", args, n);
  if (!(ss->using_schemes)) XtVaSetValues(completion_help_list, XmNbackground, (ss->sgx)->white, XmNforeground, (ss->sgx)->black, NULL);

  XtManageChild(completion_help_list);

  XtAddCallback(completion_help_list, XmNbrowseSelectionCallback, completion_help_browse_callback, NULL);
  XtAddCallback(completion_help_dialog, XmNhelpCallback, completion_help_help_callback, NULL);
  XtAddCallback(completion_help_dialog, XmNokCallback, help_completion_ok_callback, NULL);

  XtManageChild(completion_help_dialog);

  if (!(ss->using_schemes))
    {
      map_over_children(completion_help_dialog, set_main_color_of_widget, NULL);
      XtVaSetValues(completion_help_list, XmNbackground, (ss->sgx)->white, XmNforeground, (ss->sgx)->black, NULL);
      XtVaSetValues(XmMessageBoxGetChild(completion_help_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(completion_help_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(completion_help_dialog, XmDIALOG_OK_BUTTON), XmNbackground, (ss->sgx)->quit_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(completion_help_dialog, XmDIALOG_HELP_BUTTON), XmNbackground, (ss->sgx)->help_button_color, NULL);
    }
  set_dialog_widget(COMPLETION_DIALOG, completion_help_dialog);
}

void snd_completion_help(int matches, char **buffer)
{
  int i;
  Dimension w, h;
  XmString *match;
  if (completion_help_dialog)
    XtManageChild(completion_help_dialog);
  else create_completion_help_dialog(_("Completions"));
  match = (XmString *)CALLOC(matches, sizeof(XmString));
  for (i = 0; i < matches; i++) 
    match[i] = XmStringCreate(buffer[i], XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(completion_help_list, 
		XmNitems, match, 
		XmNitemCount, matches, 
		XmNvisibleItemCount, mus_iclamp(1, matches, 12), 
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

void textfield_focus_callback(Widget w, XtPointer context, XtPointer info)
{
  if (!(ss->using_schemes)) 
    XtVaSetValues(w, XmNbackground, (ss->sgx)->text_focus_color, NULL);
  XtVaSetValues(w, XmNcursorPositionVisible, true, NULL);
}

void textfield_unfocus_callback(Widget w, XtPointer context, XtPointer info)
{
  if (!(ss->using_schemes)) 
    XtVaSetValues(w, XmNbackground, (ss->sgx)->basic_color, NULL);
  XtVaSetValues(w, XmNcursorPositionVisible, false, NULL);
}


/* -------- specialized action procs -------- */

static bool actions_loaded = false;
#define CONTROL_KEY 4

static void No_op (Widget w, XEvent *ev, char **str, Cardinal *num) 
{
  /* return does not cause widget activation in many textfield cases -- it is a true no-op */
}

#define snd_K_u XK_u 
#define snd_K_x XK_x 

static void Activate_keyboard(Widget w, XEvent *ev, char **str, Cardinal *num) 
{
  /* make the current channel active preloading kbd cmd with str[0]+ctrl bit */
  chan_info *cp;
  cp = current_channel();
  if (cp) 
    {
      goto_graph(cp);
      keyboard_command(cp, (str[0][0] == 'u') ? snd_K_u : snd_K_x, CONTROL_KEY);
    }
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
      curpos += strlen(listener_selection);
      XmTextShowPosition(w, curpos);
      XmTextSetCursorPosition(w, curpos);
      XmTextClearSelection(w, ev->xkey.time); /* so C-y + edit doesn't forbid the edit */
    }
}

static int printout_end = 0;

static void Begin_of_line(Widget w, XEvent *ev, char **ustr, Cardinal *num) 
{
  /* don't back up before listener prompt */
  XmTextPosition curpos, loc;
  int prompt_len;
  char *str = NULL;
  Boolean found;
  curpos = XmTextGetCursorPosition(w) - 1;
  found = XmTextFindString(w, curpos, "\n", XmTEXT_BACKWARD, &loc);
  if (found) 
    {

      prompt_len = snd_strlen(listener_prompt(ss));
      str = (char *)CALLOC(prompt_len + 3, sizeof(char));
      XmTextGetSubstring(w, loc + 1, prompt_len, prompt_len + 2, str);
      if (strncmp(listener_prompt(ss), str, prompt_len) == 0)
	XmTextSetCursorPosition(w, loc + prompt_len + 1);
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
static XEN listener_click_hook; 

static void B1_press(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  XmTextPosition pos;
  XButtonEvent *ev = (XButtonEvent *)event;
  XmProcessTraversal(w, XmTRAVERSE_CURRENT);
  /* we're replacing the built-in take_focus action here, so do it by hand, but leave listener blue, so to speak */
  if ((!(ss->using_schemes)) && 
      (w != listener_text))
    XtVaSetValues(w, XmNbackground, (ss->sgx)->white, NULL);
  if (w == listener_text) 
    XmTextClearSelection(listener_text, CurrentTime); /* should this happen in other windows as well? */
  pos = XmTextXYToPos(w, (Position)(ev->x), (Position)(ev->y));
  XmTextSetCursorPosition(w, pos);
  down_pos = pos;
  last_pos = pos;
  if (XEN_HOOKED(listener_click_hook))
    run_hook(listener_click_hook,
	     XEN_LIST_1(C_TO_XEN_INT((int)pos)),
	     S_listener_click_hook);
}

static void B1_move(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  XmTextPosition pos;
  XButtonEvent *ev = (XButtonEvent *)event;
  pos = XmTextXYToPos(w, (Position)(ev->x), (Position)(ev->y));
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
  pos = XmTextXYToPos(w, (Position)(ev->x), (Position)(ev->y));
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

static void Complain(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  /* if the minibuffer has focus (via pointer movement) and user types C-j (for example),
   *   the textfield widget doesn't have any action associated with that, so it prints
   *   C-j and leaves the cursor where it was; without this action, Motif posts a small
   *   empty box where the character should be, which can be confusing; another option
   *   would be to activate the keyboard instead (as in C-x), but I think that would only
   *   add to the confusion.
   */
  char *old_text, *new_text;
  XmTextPosition curpos;
  curpos = XmTextGetCursorPosition(w);
  old_text = XmTextGetString(w);
  new_text = (char *)CALLOC(snd_strlen(old_text) + 5, sizeof(char));
  sprintf(new_text, "%s C-%c", (old_text) ? old_text : "", str[0][0]);
  XmTextSetString(w, new_text);
  XmTextSetCursorPosition(w, curpos);
  if (old_text) XtFree(old_text);
  FREE(new_text);
}

static void Word_upper(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  int i, j, length, wstart, wend;
  bool up, cap;
  XmTextPosition curpos, endpos;
  char *buf = NULL;
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
      for (i = wstart + 1; i < length; i++)
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
      XmTextSetCursorPosition(w, curpos + wend);
      if (buf) FREE(buf);
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
  /* non-listener tab completion */
  int data, i, matches;
  bool need_position;
  Position wx, wy;
  int xoff, yoff; 
  Window wn;
  Pixel old_color;
  char *old_text, *new_text, *search_text;
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
      if (snd_strlen(old_text) == 0) return; /* C-x C-f TAB in minibuffer, for example */
      new_text = complete_text(old_text, data);
      if (snd_strlen(new_text) == 0) return; /* can this happen? */
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
	      ss->sgx->completion_requestor = w;
	      set_save_completions(true);
	      search_text = complete_text(old_text, data);
	      if (search_text) FREE(search_text);
	      need_position = (completion_help_dialog == NULL);
	      display_completions();
	      set_save_completions(false);
	      if (need_position)
		{
		  /* try to position the newly popped up help window below the text field */
		  XtVaGetValues(w, XmNx, &wx, XmNy, &wy, NULL);
		  XTranslateCoordinates(XtDisplay(w), XtWindow(w), DefaultRootWindow(XtDisplay(w)), 0, 0, &xoff, &yoff, &wn);
		  wx += xoff; 
		  wy += yoff;
		  XtVaSetValues(completion_help_dialog, XmNx, wx, XmNy, wy + 40, NULL);
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
  if (listener_text)
    {
      if (end == -1) end = XmTextGetLastPosition(listener_text);
      XmTextInsert(listener_text, end, msg);
      XmTextSetCursorPosition(listener_text, XmTextGetLastPosition(listener_text));
    }
}

static bool dont_check_motion = false;

void listener_delete_text(int new_end)
{
  int old_end;
  old_end = XmTextGetLastPosition(listener_text);
  if (old_end > new_end)
    {
      dont_check_motion = true;
      XmTextSetSelection(listener_text, new_end, old_end, CurrentTime);
      XmTextRemove(listener_text);
      dont_check_motion = false;
    }
}

static void Listener_Meta_P(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  listener_delete_text(printout_end + 1);
  restore_listener_string(true);
}

static void Listener_Meta_N(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  listener_delete_text(printout_end + 1);
  restore_listener_string(false);
}

void save_listener_text(FILE *fp)
{
  char *str = NULL;
  if (listener_text)
    {
      str = XmTextGetString(listener_text);
      if (str)
	{
	  fwrite((void *)str, sizeof(char), snd_strlen(str), fp);
	  XtFree(str);
	}
    }
}

static void Listener_help(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  char *source = NULL;
  int end = 0, beg, len;
  end = XmTextGetLastPosition(w); /* cursor perhaps? */
  beg = end - 1024;
  if (beg < 0) beg = 0;
  if (beg >= end) return;
  len = end - beg + 1;
  source = (char *)CALLOC(len + 1, sizeof(char));
  XmTextGetSubstring(w, beg, len, len + 1, source);
  provide_listener_help(source);
  FREE(source);
}

static void Listener_completion(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  /* used only by the listener widget -- needs to be smart about text since overall string can be enormous 
   *   and we don't want to back up past the last prompt
   *   also if at start of line (or all white-space to previous \n, indent
   */
  int beg, end, len, matches = 0;
  bool need_position;
  char *old_text, *new_text = NULL, *file_text = NULL;
  bool try_completion = true;
  Position wx, wy;
  int xoff, yoff; 
  Window wn;
  ss->sgx->completion_requestor = listener_text;
  beg = printout_end + 1;
  end = XmTextGetLastPosition(w);
  if (end <= beg) return;
  len = end - beg + 1;
  old_text = (char *)CALLOC(len + 1, sizeof(char));
  XmTextGetSubstring(w, beg, len, len + 1, old_text);
  /* now old_text is the stuff typed since the last prompt */
  if (old_text)
    {
      new_text = complete_listener_text(old_text, end, &try_completion, &file_text);
      if (!try_completion)
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
	  set_save_completions(true);
	  if (file_text) 
	    new_text = filename_completer(file_text);
	  else new_text = command_completer(old_text);
	  if (new_text) 
	    {
	      FREE(new_text); 
	      new_text = NULL;
	    }
	  need_position = (completion_help_dialog == NULL);
	  display_completions();
	  set_save_completions(false);
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

static void Listener_clear(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  clear_listener();
}

static void Listener_g(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  control_g(any_selected_sound());
}

static void Listener_Backup(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  backup_listener_to_previous_command();
}

#define NUM_ACTS 20
static XtActionsRec acts[] = {
  {"no-op", No_op},
  {"activate-keyboard", Activate_keyboard},
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
  {"listener-clear", Listener_clear},
  {"listener-g", Listener_g},
  {"listener-help", Listener_help},
  {"listener-meta-p", Listener_Meta_P},
  {"listener-meta-n", Listener_Meta_N},
  {"delete-to-previous-command", Listener_Backup},
  {"complain", Complain},
};

/* translation tables for emacs compatibility and better inter-widget communication */

/* for textfield (single-line) widgets */
static char TextTrans2[] =
       "Ctrl <Key>a:	    beginning-of-line()\n\
	Ctrl <Key>b:	    backward-character()\n\
	Mod1 <Key>b:	    backward-word()\n\
	Mod1 <Key>c:	    word-upper(c)\n\
        Ctrl <Key>c:        complain(c)\n\
	Ctrl <Key>d:	    delete-next-character()\n\
	Mod1 <Key>d:	    delete-next-word()\n\
	Ctrl <Key>e:	    end-of-line()\n\
	Ctrl <Key>f:	    forward-character()\n\
	Mod1 <Key>f:	    forward-word()\n\
	Ctrl <Key>g:	    activate()\n\
	Ctrl <Key>h:	    delete-previous-character()\n\
        Ctrl <Key>i:        complain(i)\n\
        Ctrl <Key>j:        complain(j)\n\
	Ctrl <Key>k:	    delete-to-end-of-line()\n\
	Mod1 <Key>l:	    word-upper(l)\n\
        Ctrl <Key>m:        complain(m)\n\
        Ctrl <Key>n:        complain(n)\n\
	Mod1 <Key>n:	    activate()\n\
        Ctrl <Key>o:        complain(o)\n\
	Mod1 <Key>p:	    activate()\n\
        Ctrl <Key>p:        complain(p)\n\
        Ctrl <Key>q:        complain(q)\n\
        Ctrl <Key>r:        activate()\n\
        Ctrl <Key>s:        activate()\n\
	Ctrl <Key>t:	    text-transpose()\n\
	Mod1 <Key>u:	    word-upper(u)\n\
        Ctrl <Key>u:        complain(u)\n\
        Ctrl <Key>v:        complain(v)\n\
        Ctrl <Key>w:        complain(w)\n\
	Ctrl <Key>x:	    activate-keyboard(x)\n\
        Ctrl <Key>y:        complain(y)\n\
        Ctrl <Key>z:        complain(z)\n\
	Mod1 <Key><:	    beginning-of-line()\n\
	Mod1 <Key>>:	    end-of-line()\n\
	<Key>Delete:	    delete-previous-character()\n\
	Mod1 <Key>Delete:   delete-to-start-of-line()\n\
	<Key>Tab:	    name-completion()\n\
	<Key>Return:	    activate()\n";
static XtTranslations transTable2 = NULL;

/* same (but not activatable), try to avoid causing the currently active pushbutton widget to appear to be activated by <cr> in the text widget */
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
	Ctrl Meta <Key>g:   listener-clear()\n\
	Ctrl <Key>g:	    listener-g()\n\
	Ctrl <Key>h:	    delete-previous-character()\n\
	Ctrl <Key>j:	    newline-and-indent()\n\
	Ctrl <Key>k:	    kill-line()\n\
	Ctrl <Key>l:	    redraw-display()\n\
	Mod1 <Key>l:	    word-upper(l)\n\
	Ctrl <Key>n:	    next-line()\n\
        Mod1 <Key>n:        listener-meta-n()\n\
	Ctrl <Key>o:	    newline-and-backup()\n\
	Ctrl <Key>p:	    previous-line()\n\
        Mod1 <Key>p:        listener-meta-p()\n\
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
        Shift Ctrl <Key>-:  delete-to-previous-command()\n\
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
        Ctrl <Key>?:        listener-help()\n\
	<Key>Return:	    activate()\n";
static XtTranslations transTable4 = NULL;


/* -------- text related widgets -------- */

static XEN mouse_enter_text_hook;
static XEN mouse_leave_text_hook;

void mouse_enter_text_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  if (XEN_HOOKED(mouse_enter_text_hook))
    run_hook(mouse_enter_text_hook,
	     XEN_LIST_1(XEN_WRAP_WIDGET(w)),
	     S_mouse_enter_text_hook);
}

void mouse_leave_text_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  if (XEN_HOOKED(mouse_leave_text_hook))
    run_hook(mouse_leave_text_hook,
	     XEN_LIST_1(XEN_WRAP_WIDGET(w)),
	     S_mouse_leave_text_hook);
}

Widget make_textfield_widget(char *name, Widget parent, Arg *args, int n, text_cr_t activatable, int completer)
{
  /* white background when active, emacs translations */
  Widget df;
  if (!actions_loaded) 
    {
      XtAppAddActions(MAIN_APP(ss), acts, NUM_ACTS); 
      actions_loaded = true;
    }
  /* can't use XmNuserData here because it is in use elsewhere (snd-xmix.c) */
  XtSetArg(args[n], XmNcursorPositionVisible, false); n++;
  df = XtCreateManagedWidget(name, xmTextFieldWidgetClass, parent, args, n);
  XtAddCallback(df, XmNfocusCallback, textfield_focus_callback, NULL);
  XtAddCallback(df, XmNlosingFocusCallback, textfield_unfocus_callback, NULL);
  XtAddEventHandler(df, EnterWindowMask, false, mouse_enter_text_callback, NULL);
  XtAddEventHandler(df, LeaveWindowMask, false, mouse_leave_text_callback, NULL);

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
  return(df);
}

void add_completer_to_textfield(Widget w, int completer)
{
  /* used to make file selection dialog act like other text field widgets */
  if (!actions_loaded) 
    {
      XtAppAddActions(MAIN_APP(ss), acts, NUM_ACTS); 
      actions_loaded = true;
    }
  if (!transTable2) 
    transTable2 = XtParseTranslationTable(TextTrans2);
  XtOverrideTranslations(w, transTable2);
  add_completer_widget(w, completer);
}

Widget make_text_widget(char *name, Widget parent, Arg *args, int n)
{
  /* white background when active, emacs translations */
  /* used only for comment widget in file data box (snd-xfile.c) */
  Widget df;
  if (!actions_loaded) 
    {
      XtAppAddActions(MAIN_APP(ss), acts, NUM_ACTS); 
      actions_loaded = true;
    }
  XtSetArg(args[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;
  /* XmNblinkRate 0 turns off the cursor blink */
  XtSetArg(args[n], XmNcursorPositionVisible, false); n++;
  df = XmCreateScrolledText(parent, name, args, n);
  XtManageChild(df);
  XtAddCallback(df, XmNfocusCallback, textfield_focus_callback, NULL);
  XtAddCallback(df, XmNlosingFocusCallback, textfield_unfocus_callback, NULL);
  XtAddEventHandler(df, EnterWindowMask, false, mouse_enter_text_callback, NULL);
  XtAddEventHandler(df, LeaveWindowMask, false, mouse_leave_text_callback, NULL);
  if (!transTable3) 
    transTable3 = XtParseTranslationTable(TextTrans3);
  XtOverrideTranslations(df, transTable3);
  return(df);
}


/* ---------------- command widget replacement ---------------- */

static Widget lisp_window = NULL;

void listener_append(char *msg)
{
  if (listener_text)
    {
      if (listener_print_p(msg))
	XmTextInsert(listener_text, XmTextGetLastPosition(listener_text), msg);
      printout_end = XmTextGetLastPosition(listener_text) - 1;
    }
}
 
static Widget listener_pane = NULL; 

void listener_append_and_prompt(char *msg)
{
  XmTextPosition cmd_eot;
  if (listener_text)
    {
      if (listener_print_p(msg))
	{
	  if (msg)
	    XmTextInsert(listener_text, XmTextGetLastPosition(listener_text), msg);
	  cmd_eot = XmTextGetLastPosition(listener_text);
	  XmTextInsert(listener_text, cmd_eot, listener_prompt_with_cr());
	}
      cmd_eot = XmTextGetLastPosition(listener_text);
      printout_end = cmd_eot - 1;
      XmTextShowPosition(listener_text, cmd_eot - 1);
    }
}

static void command_return_callback(Widget w, XtPointer context, XtPointer info)
{
  if (!(ss->error_lock))
    command_return(w, printout_end);
}

static int flashes = 0;
static int paren_pos = -1;
#define FLASH_TIME 150

static void flash_unbalanced_paren(XtPointer context, XtIntervalId *id)
{
  flashes--;
  XmTextSetHighlight(listener_text, paren_pos, paren_pos + 1, (flashes & 1) ? XmHIGHLIGHT_NORMAL : XmHIGHLIGHT_SELECTED);
  if (flashes > 0)
    XtAppAddTimeOut(MAIN_APP(ss),
		    (unsigned long)FLASH_TIME,
		    (XtTimerCallbackProc)flash_unbalanced_paren,
		    NULL);
  else 
    {
      XmTextSetHighlight(listener_text, paren_pos, paren_pos + 1, XmHIGHLIGHT_NORMAL);
      paren_pos = -1;
    }
}

bool highlight_unbalanced_paren(void)
{
  /* if cursor is positioned at close paren, try to find reason for unbalanced expr and highlight it */
  int pos;
  bool success = true;
  char *str = NULL;
  pos = XmTextGetInsertionPosition(listener_text);
  if (pos > 2)
    {
      str = XmTextGetString(listener_text);
      if ((str[pos - 1] == ')') &&
	  ((str[pos - 2] != '\\') || (str[pos - 3] != '#')))
	{
	  int parens;
	  parens = find_matching_paren(str, 2, pos - 1, listener_prompt(ss), &paren_pos);
	  if (parens == 0)
	    {
	      XmTextSetHighlight(listener_text, paren_pos, paren_pos + 1, XmHIGHLIGHT_SELECTED);
	      flashes = 4;
	      XtAppAddTimeOut(MAIN_APP(ss),
			      (unsigned long)FLASH_TIME,
			      (XtTimerCallbackProc)flash_unbalanced_paren,
			      NULL);
	    }
	  else success = false;
	}
      if (str) XtFree(str);
    }
  return(success);
}

static int last_highlight_position = -1;

static void command_motion_callback(Widget w, XtPointer context, XtPointer info)
{
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;
  char *str = NULL;
  int pos, parens;
  cbs->doit = true; 
  if (dont_check_motion) return;
  if (last_highlight_position != -1)
    {
      XmTextSetHighlight(w, last_highlight_position, last_highlight_position + 1, XmHIGHLIGHT_NORMAL);
      last_highlight_position = -1;
    }
  pos = cbs->newInsert - 1;
  if (pos > 0)
    {
      str = XmTextGetString(w);
      if ((str[pos] == ')') && 
	  ((pos <= 1) || (str[pos - 1] != '\\') || (str[pos - 2] != '#')))
	{
	  parens = find_matching_paren(str, 1, pos, listener_prompt(ss), &last_highlight_position);
	  if (parens == 0)
	    XmTextSetHighlight(w, last_highlight_position, last_highlight_position + 1, XmHIGHLIGHT_SECONDARY_SELECTED);
	}
      if (str) XtFree(str);
    }
}

static void command_modify_callback(Widget w, XtPointer context, XtPointer info)
{
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;
  char *str = NULL, *prompt;
  int len;
  if (((cbs->text)->length > 0) || (dont_check_motion))
    cbs->doit = true;
  else
    { 
      if (cbs->currInsert < 2) 
	cbs->doit = false;
      else
	{
	  prompt = listener_prompt(ss);
	  str = XmTextGetString(w);
	  len = XmTextGetLastPosition(w);
	  if ( ((str[cbs->currInsert - 1] == prompt[0]) && 
		(str[cbs->currInsert - 2] == '\n')) ||
	       ((cbs->currInsert < (len - 1)) && 
		(str[cbs->currInsert] == prompt[0]) && 
		(str[cbs->currInsert - 1] == '\n')))
	    cbs->doit = false;
	  else cbs->doit = true;
	  if (str) XtFree(str);
	}
    }
}

static XEN mouse_enter_listener_hook;
static XEN mouse_leave_listener_hook;

static void listener_focus_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  if (XEN_HOOKED(mouse_enter_listener_hook))
    run_hook(mouse_enter_listener_hook,
	     XEN_LIST_1(XEN_WRAP_WIDGET(listener_text)), /* not w */
	     S_mouse_enter_listener_hook);
}

static void listener_unfocus_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  if (XEN_HOOKED(mouse_leave_listener_hook))
    run_hook(mouse_leave_listener_hook,
	     XEN_LIST_1(XEN_WRAP_WIDGET(listener_text)), /* not w */
	     S_mouse_leave_listener_hook);
}

static void make_command_widget(int height)
{
  Arg args[32];
  Widget wv, wh;
  int n;
  if (!listener_text)
    {
      if (!actions_loaded) {XtAppAddActions(MAIN_APP(ss), acts, NUM_ACTS); actions_loaded = true;}

      n = attach_all_sides(args, 0);
      XtSetArg(args[n], XmNheight, height); n++;
      if ((sound_style(ss) == SOUNDS_IN_NOTEBOOK) || (sound_style(ss) == SOUNDS_HORIZONTAL))
	listener_pane = XtCreateManagedWidget("frm", xmFormWidgetClass, SOUND_PANE_BOX(ss), args, n);
      else listener_pane = XtCreateManagedWidget("frm", xmFormWidgetClass, SOUND_PANE(ss), args, n);
      /* this widget is not redundant at least in Metroworks Motif */

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->listener_color); n++;
	  XtSetArg(args[n], XmNforeground, (ss->sgx)->listener_text_color); n++;
	}
      if ((ss->sgx)->listener_fontlist) {XtSetArg(args[n], XM_FONT_RESOURCE, (ss->sgx)->listener_fontlist); n++;}
      n = attach_all_sides(args, n);
      XtSetArg(args[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;
      XtSetArg(args[n], XmNskipAdjust, true); n++;
      XtSetArg(args[n], XmNvalue, listener_prompt(ss)); n++;
      XtSetArg(args[n], XmNpendingDelete, false); n++; /* don't cut selection upon paste */
      XtSetArg(args[n], XmNpositionIndex, XmLAST_POSITION); n++;
      listener_text = XmCreateScrolledText(listener_pane, "lisp-listener", args, n);
      ss->sgx->listener_pane = listener_text;

      XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, false, NULL);

      XtManageChild(listener_text);
      XmTextSetCursorPosition(listener_text, 1);
      if (!transTable4) 
	transTable4 = XtParseTranslationTable(TextTrans4);
      XtOverrideTranslations(listener_text, transTable4);
      printout_end = 0;
      XtAddCallback(listener_text, XmNactivateCallback, command_return_callback, NULL);
      XtAddCallback(listener_text, XmNmodifyVerifyCallback, command_modify_callback, NULL);
      XtAddCallback(listener_text, XmNmotionVerifyCallback, command_motion_callback, NULL);

      lisp_window = XtParent(listener_text);
      XtAddEventHandler(lisp_window, EnterWindowMask, false, listener_focus_callback, NULL);
      XtAddEventHandler(lisp_window, LeaveWindowMask, false, listener_unfocus_callback, NULL);
      
      if (!(ss->using_schemes))
	{
	  XmChangeColor(lisp_window, (ss->sgx)->basic_color);
	  XtVaGetValues(lisp_window, XmNverticalScrollBar, &wv, XmNhorizontalScrollBar, &wh, NULL);
	  XmChangeColor(wv, (ss->sgx)->basic_color);
	  XmChangeColor(wh, (ss->sgx)->basic_color);
	  map_over_children(SOUND_PANE(ss), color_sashes, NULL);
	}
      if (auto_resize(ss))
	XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, true, NULL);
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
  (ss->sgx)->listener_color = pix;
  if (listener_text)
    XmChangeColor(listener_text, pix);
}

void color_listener_text(Pixel pix)
{
  (ss->sgx)->listener_text_color = pix;
  if (listener_text)
    XtVaSetValues(listener_text, XmNforeground, pix, NULL);
}

void handle_listener(bool open)
{
  if (open)
    {
      if (!listener_text)
	make_command_widget(100);
      else 
	{
	  XtManageChild(listener_pane);
	  if (listener_height() < 5)
	    {
	      XtUnmanageChild(listener_pane);
	      XtVaSetValues(listener_pane, XmNpaneMinimum, 100, XmNpaneMaximum, LOTSA_PIXELS, NULL);
	      XtManageChild(listener_pane);
	      XtVaSetValues(listener_pane, XmNpaneMinimum, 1, NULL);
	    }
	}
    }
  else
    {
      XtUnmanageChild(listener_pane);
    }
}

int listener_height(void)
{
  if ((listener_text) && (XtIsManaged(listener_pane)))
    return(widget_height(listener_text)); 
  else return(0);
}

int listener_width(void)
{
  if ((listener_text) && (XtIsManaged(listener_pane)))
    return(widget_width(listener_text)); 
  else return(0);
}

#if OVERRIDE_TOGGLE
static char ToggleTrans2[] =
       "c<Btn1Down>:   ArmAndActivate()\n";
static XtTranslations toggleTable2 = NULL;

static void override_toggle_translation(Widget w)
{
  if (!toggleTable2) toggleTable2 = XtParseTranslationTable(ToggleTrans2);
  XtOverrideTranslations(w, toggleTable2);
}
#endif

Widget make_togglebutton_widget(char *name, Widget parent, Arg *args, int n)
{
  Widget w;
  w = XtCreateManagedWidget(name, xmToggleButtonWidgetClass, parent, args, n);
#if OVERRIDE_TOGGLE
  override_toggle_translation(w);
#endif
  return(w);
}

Widget make_pushbutton_widget(char *name, Widget parent, Arg *args, int n)
{
  Widget w;
  w = XtCreateManagedWidget(name, xmPushButtonWidgetClass, parent, args, n);
#if OVERRIDE_TOGGLE
  override_toggle_translation(w); /* ??? activate here (rather than armandactivate) fails? */
#endif
  return(w);
}

static XEN g_listener_selected_text(void)
{
  #define H_listener_selection "(" S_listener_selection "): currently selected text in listener or #f"
  char *txt;
  XEN res = XEN_FALSE;
  if (listener_text)
    {
      txt = XmTextGetSelection(listener_text);
      if (txt) 
	{
	  res = C_TO_XEN_STRING(txt);
	  XtFree(txt);
	}
    }
  return(res);
}

static XEN g_reset_listener_cursor(void)
{
  #define H_reset_listener_cursor "(" S_reset_listener_cursor "): reset listener cursor to the default pointer"
  if (listener_text)
    XUndefineCursor(XtDisplay(listener_text), 
		    XtWindow(listener_text)); 
  return(XEN_FALSE);
}

void clear_listener(void)
{
  if (listener_text) /* this can be called even when there is no listener */
    {
      dont_check_motion = true;
      XmTextSetSelection(listener_text, 1, XmTextGetCursorPosition(listener_text), CurrentTime);
      XmTextRemove(listener_text);
      dont_check_motion = false;
    }
}

void lock_listener_pane(void)
{
  int val;
  val = listener_height();
  if (val > 0)
    XtVaSetValues(listener_pane, XmNpaneMinimum, val - 1, XmNpaneMaximum, val + 1, NULL);
}

void unlock_listener_pane(void)
{
  if ((listener_pane) && (XtIsManaged(listener_pane)))
    XtVaSetValues(listener_pane, XmNpaneMinimum, 1, XmNpaneMaximum, LOTSA_PIXELS, NULL);
}

static XEN g_goto_listener_end(void)
{
  XmTextPosition eot;
  eot = XmTextGetLastPosition(listener_text);
  XmTextShowPosition(listener_text, eot);
  XmTextSetInsertionPosition(listener_text, eot);
  return(XEN_FALSE);
}


#ifdef XEN_ARGIFY_1
  XEN_NARGIFY_0(g_listener_selected_text_w, g_listener_selected_text)
  XEN_NARGIFY_0(g_reset_listener_cursor_w, g_reset_listener_cursor)
  XEN_NARGIFY_0(g_goto_listener_end_w, g_goto_listener_end)
#else
  #define g_listener_selected_text_w g_listener_selected_text
  #define g_reset_listener_cursor_w g_reset_listener_cursor
  #define g_goto_listener_end_w g_goto_listener_end
#endif

void g_init_gxlistener(void)
{
  #define H_mouse_enter_listener_hook S_mouse_enter_listener_hook " (listener): called when the mouse \
enters the lisp listener pane:\n\
  (add-hook! mouse-enter-listener-hook\n\
    (lambda (widget)\n\
      (focus-widget widget)))"

  #define H_mouse_leave_listener_hook S_mouse_leave_listener_hook " (listener): called when the mouse \
leaves the lisp listener pane"

  XEN_DEFINE_HOOK(mouse_enter_listener_hook, S_mouse_enter_listener_hook, 1, H_mouse_enter_listener_hook);    /* arg = listener_text widget */
  XEN_DEFINE_HOOK(mouse_leave_listener_hook, S_mouse_leave_listener_hook, 1, H_mouse_leave_listener_hook);    /* arg = listener_text widget */

  #define H_mouse_enter_text_hook S_mouse_enter_text_hook " (widget): called when the mouse enters a text widget:\n\
(add-hook! mouse-enter-text-hook\n\
  (lambda (w)\n\
    (focus-widget w)))"

  #define H_mouse_leave_text_hook S_mouse_leave_text_hook " (widget): called when the mouse leaves a text widget"
  
  XEN_DEFINE_HOOK(mouse_enter_text_hook, S_mouse_enter_text_hook, 1, H_mouse_enter_text_hook);    /* arg = text widget */
  XEN_DEFINE_HOOK(mouse_leave_text_hook, S_mouse_leave_text_hook, 1, H_mouse_leave_text_hook);    /* arg = text widget */

  XEN_DEFINE_PROCEDURE(S_listener_selection, g_listener_selected_text_w, 0, 0, 0, H_listener_selection);
  XEN_DEFINE_PROCEDURE(S_reset_listener_cursor, g_reset_listener_cursor_w, 0, 0, 0, H_reset_listener_cursor);
  XEN_DEFINE_PROCEDURE("goto-listener-end", g_goto_listener_end_w, 0, 0, 0, "move cursor and scroll to bottom of listener pane");

  #define H_listener_click_hook S_listener_click_hook " (pos): called when listener clicked; pos is text pos of click in listener"
  XEN_DEFINE_HOOK(listener_click_hook,    S_listener_click_hook, 1,    H_listener_click_hook);    /* arg = pos */
}

