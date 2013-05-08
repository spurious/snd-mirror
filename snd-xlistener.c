#include "snd.h"


#define OVERRIDE_TOGGLE 1
/* Motif 2.0 defines control-button1 to be "take focus" -- this is not a good idea!! */


static void Tab_completion(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  int completer;
  pointer_or_int_t data;

  XtVaGetValues(w, XmNuserData, &data, NULL);
  completer = (int)data;

  if (completer >= 0)
    {
      int matches;
      char *old_text, *new_text;

      old_text = XmTextGetString(w);
      if (mus_strlen(old_text) == 0) return; /* C-x C-f TAB in kbd??, for example */

      new_text = complete_text(w, old_text, completer);
      if (mus_strlen(new_text) == 0) return; /* can this happen? */
      XmTextSetString(w, new_text);
      XmTextSetCursorPosition(w, XmTextGetLastPosition(w));

      matches = get_completion_matches();

      if ((mus_strcmp(old_text, new_text)) && 
	  (matches != -1))
	{
	  Pixel old_color;
	  XtVaGetValues(w, XmNforeground, &old_color, NULL);
	  if (matches > 1)
	    XtVaSetValues(w, XmNforeground, ss->green, NULL);
	  else 
	    if (matches == 0) 
	      XtVaSetValues(w, XmNforeground, ss->red, NULL);
	  if (matches != 1)
	    {
	      XmUpdateDisplay(w);
#if HAVE_SLEEP
	      sleep(1);
#endif
	      XtVaSetValues(w, XmNforeground, old_color, NULL);
	      XmUpdateDisplay(w);
	    }

	  if (matches > 1)                          /* there are several possible completions -- let the widget decide how to handle it */
	    handle_completions(w, completer);
	}

      if (old_text) XtFree(old_text);
      if (new_text) free(new_text);
    }
}


/* listener completions */

static Widget listener_text = NULL;
static Widget listener_pane = NULL;  /* form widget that hold the listener scrolled text widget */

static Widget completions_list = NULL;
static Widget completions_pane = NULL;


static void perform_completion(XmString selection)
{
  int i, j, old_len, new_len;
  char *text = NULL, *old_text = NULL;

  text = (char *)XmStringUnparse(selection, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
  save_completion_choice(text);

  old_text = XmTextGetString(listener_text);
  old_len = mus_strlen(old_text);
  new_len = mus_strlen(text);
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

  append_listener_text(XmTextGetLastPosition(listener_text), (char *)(text - 1 + old_len - i));
 
  if (text) XtFree(text);
  if (old_text) XtFree(old_text);
}


static void listener_completions_browse_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  /* choice = cbs->item_position - 1; */
  perform_completion(cbs->item);
  XtUnmanageChild(completions_pane);
}


static int alphabetize(const void *a, const void *b)
{
  return(strcmp(*((const char **)a), (*(const char **)b)));
}


static int find_prompt(Widget w, XmTextPosition start)
{
  Boolean found_prompt;
  XmTextPosition loc = 0;

  if (start == 0) /* try to avoid strlen null in motif */
    return(0);
  found_prompt = XmTextFindString(w, start, listener_prompt(ss), XmTEXT_BACKWARD, &loc);
  if (!found_prompt) 
    return(0);
  else return((int)loc + mus_strlen(listener_prompt(ss)));
}


static void motif_listener_completion(Widget w, XEvent *event, char **str, Cardinal *num)  /* change name because emacs is confused */
{
  /* used only by the listener widget -- needs to be smart about text since overall string can be enormous 
   *   and we don't want to back up past the last prompt
   *   also if at start of line (or all white-space to previous \n), indent
   */
  int beg, end, replace_end, len, matches = 0;
  char *old_text;

  if ((completions_pane) &&
      (XtIsManaged(completions_pane)))
    {
      XmString *strs;
      XtVaGetValues(completions_list, 
		    XmNselectedItems, &strs, 
		    NULL);
      perform_completion(strs[0]);
      XtUnmanageChild(completions_pane);
      return;
    }

  beg = 0;
  end = XmTextGetInsertionPosition(w);
  replace_end = end;

  beg = find_prompt(w, (XmTextPosition)end);
  len = end - beg + 1;

  old_text = (char *)calloc(len + 1, sizeof(char));
  XmTextGetSubstring(w, beg, len, len + 1, old_text);
  /* now old_text is the stuff typed since the last prompt */

  if (old_text[len - 1] == '\n')
    {
      old_text[len - 1] = 0;
      end--;
    }

  if (old_text)
    {
      char *new_text = NULL, *file_text = NULL;
      bool try_completion = true;
      new_text = complete_listener_text(old_text, end, &try_completion, &file_text);

      if (!try_completion)
	{
	  free(old_text);
	  return;
	}

      if (mus_strcmp(old_text, new_text))
	matches = get_completion_matches();
      else XmTextReplace(w, beg, replace_end, new_text);

      if (new_text) 
	{
	  free(new_text); 
	  new_text = NULL;
	}

      if (matches > 1)
	{
	  int num;
	  char **buffer;
	  clear_possible_completions();
	  set_save_completions(true);
	  if (file_text) 
	    new_text = filename_completer(w, file_text, NULL);
	  else new_text = expression_completer(w, old_text, NULL);
	  if (new_text) 
	    {
	      free(new_text); 
	      new_text = NULL;
	    }
	  num = get_possible_completions_size();
	  if (num > 0)
	    {
	      int i;
	      XmString *match;

	      if (!completions_list)
		{
		  Arg args[20];
		  int n = 0;

		  XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
		  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
		  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
		  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
		  XtSetArg(args[n], XmNscrollBarPlacement, XmBOTTOM_LEFT); n++;
		  XtSetArg(args[n], XmNbackground, ss->white); n++;
		  XtSetArg(args[n], XmNforeground, ss->black); n++;
		  XtSetArg(args[n], XmNselectColor, ss->selection_color); n++;

		  completions_list = XmCreateScrolledList(listener_pane, (char *)"completion-help-text", args, n);
		  completions_pane = XtParent(completions_list);

		  XtAddCallback(completions_list, XmNbrowseSelectionCallback, listener_completions_browse_callback, NULL);
		  XtManageChild(completions_list);
		}

	      buffer = get_possible_completions();
	      qsort((void *)buffer, num, sizeof(char *), alphabetize);

	      match = (XmString *)calloc(num, sizeof(XmString));
	      for (i = 0; i < num; i++) 
		match[i] = XmStringCreateLocalized(buffer[i]);

	      XtVaSetValues(completions_list, 
			    XmNitems, match, 
			    XmNitemCount, num, 
			    XmNvisibleItemCount, mus_iclamp(1, num, 20), 
			    NULL);

	      if (!(XtIsManaged(completions_pane)))
		XtManageChild(completions_pane);

	      /* look at previous completions list first for a match, then
	       *   look back from "beg" for any member of the match list previously typed
	       */
	      {
		int row;
		row = find_best_completion(buffer, num);
		XmListSelectPos(completions_list, row, false);
		ensure_list_row_visible(completions_list, row);
	      }

	      for (i = 0; i < num; i++) 
		XmStringFree(match[i]);
	      free(match);
	    }
	  set_save_completions(false);
	}

      if (file_text) free(file_text);
      if (old_text) free(old_text);
    }
}




/* ---------------- text widget specializations ---------------- */

void textfield_focus_callback(Widget w, XtPointer context, XtPointer info)
{
  XtVaSetValues(w, XmNbackground, ss->text_focus_color, NULL);
  XtVaSetValues(w, XmNcursorPositionVisible, true, NULL);
}


void textfield_unfocus_callback(Widget w, XtPointer context, XtPointer info)
{
  XtVaSetValues(w, XmNbackground, ss->basic_color, NULL);
  XtVaSetValues(w, XmNcursorPositionVisible, false, NULL);
}


static void textfield_no_color_focus_callback(Widget w, XtPointer context, XtPointer info)
{
  XtVaSetValues(w, XmNcursorPositionVisible, true, NULL);
}


static void textfield_no_color_unfocus_callback(Widget w, XtPointer context, XtPointer info)
{
  XtVaSetValues(w, XmNcursorPositionVisible, false, NULL);
}


/* -------- specialized action procs -------- */

static bool actions_loaded = false;
#define CONTROL_KEY 4

static void No_op(Widget w, XEvent *ev, char **str, Cardinal *num) 
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
  found = XmTextFindString(w, curpos, (char *)"\n", XmTEXT_FORWARD, &loc);
  if (!found) loc = XmTextGetLastPosition(w);
  if (loc > curpos)
    {
      if (listener_selection) {XtFree(listener_selection); listener_selection = NULL;}
      XmTextSetSelection(w, curpos, loc, CurrentTime);
      listener_selection = XmTextGetSelection(w); /* xm manual p329 sez storage is allocated here */
      XmTextCut(w, CurrentTime);
    }
}


static void Yank(Widget w, XEvent *ev, char **str, Cardinal *num) 
{
  /* copy current selection at current cursor position */
  if (listener_selection) 
    {
      XmTextPosition curpos;
      curpos = XmTextGetCursorPosition(w);
      XmTextInsert(w, curpos, listener_selection);
      curpos += strlen(listener_selection);
      XmTextShowPosition(w, curpos);
      XmTextSetCursorPosition(w, curpos);
      XmTextClearSelection(w, ev->xkey.time); /* so C-y + edit doesn't forbid the edit */
    }
}


static void Begin_of_line(Widget w, XEvent *ev, char **ustr, Cardinal *num) 
{
  /* don't back up before listener prompt */
  XmTextPosition curpos, loc;
  Boolean found;
  curpos = XmTextGetCursorPosition(w) - 1;
  found = XmTextFindString(w, curpos, (char *)"\n", XmTEXT_BACKWARD, &loc);
  if (found) 
    {
      char *str = NULL;
      str = (char *)calloc(ss->listener_prompt_length + 3, sizeof(char));
      XmTextGetSubstring(w, loc + 1, ss->listener_prompt_length, ss->listener_prompt_length + 2, str);
      if (strncmp(listener_prompt(ss), str, ss->listener_prompt_length) == 0)
	XmTextSetCursorPosition(w, loc + ss->listener_prompt_length + 1);
      else XmTextSetCursorPosition(w, loc + 1);
      free(str);
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
  if (w != listener_text)
    XtVaSetValues(w, XmNbackground, ss->white, NULL);
  else XmTextClearSelection(listener_text, CurrentTime); /* should this happen in other windows as well? */
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
      if (listener_selection) {XtFree(listener_selection); listener_selection = NULL;}
      XmTextSetSelection(w, down_pos, pos, CurrentTime);
      listener_selection = XmTextGetSelection(w);
    }
}


static void Text_transpose(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  XmTextPosition curpos;
  curpos = XmTextGetCursorPosition(w);
  if (curpos > 1)
    {
      char buf[3]; /* needs room for null */
      char tmp;
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
  char *old_text, *new_text;
  XmTextPosition curpos;
  int len;

  curpos = XmTextGetCursorPosition(w);
  old_text = XmTextGetString(w);
  len = mus_strlen(old_text) + 5;
  new_text = (char *)calloc(len, sizeof(char));
  snprintf(new_text, len, "%s C-%c", (old_text) ? old_text : "", str[0][0]);

  XmTextSetString(w, new_text);
  XmTextSetCursorPosition(w, curpos);

  if (old_text) XtFree(old_text);
  free(new_text);
}


static void text_at_cursor(Widget w)
{
  XmTextPosition curpos, endpos, start, end;
  int len, prompt_pos;
  char *buf;

  curpos = XmTextGetCursorPosition(w);
  if (curpos <= 1)
    curpos = XmTextGetInsertionPosition(w);
  if (curpos <= 1)
    {
      snd_help("Listener", "This is the 'listener', a text widget in which you can interact with Snd's extension language.  See extsnd.html.", WITH_WORD_WRAP);
      return;
    }

  prompt_pos = find_prompt(w, curpos);

  if (curpos > 40)
    start = curpos - 40;
  else start = 0;
  if (start < prompt_pos)
    start = prompt_pos;

  endpos = XmTextGetLastPosition(w);
  if ((endpos - curpos) > 40)
    end = curpos + 40;
  else end = endpos;

  len = end - start + 1;
  buf = (char *)calloc(len + 1, sizeof(char));
  XmTextGetSubstring(w, start, len, len + 1, buf);

  listener_help_at_cursor(buf, curpos - start - 1, len, prompt_pos);
  free(buf);
}


static void Help_At_Cursor(Widget w, XEvent *ev, char **str, Cardinal *num) 
{
  text_at_cursor(w);
}


static void Word_upper(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  bool up, cap;
  XmTextPosition curpos, endpos;
  up = (str[0][0] == 'u');
  cap = (str[0][0] == 'c');
  curpos = XmTextGetCursorPosition(w);
  endpos = XmTextGetLastPosition(w);
  if (curpos < endpos)
    {
      int i, length, wstart, wend;
      char *buf = NULL;
      length = endpos - curpos;
      buf = (char *)calloc(length + 1, sizeof(char));
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
	  int j;
	  for (i = wstart, j = 0; i < wend; i++, j++)
	    if (up) 
	      buf[j] = toupper(buf[i]);
	    else buf[j] = tolower(buf[i]);
	  buf[j] = '\0';
	  XmTextReplace(w, curpos + wstart, curpos + wend, buf);
	}
      XmTextSetCursorPosition(w, curpos + wend);
      if (buf) free(buf);
    }
}


void append_listener_text(int end, const char *msg)
{
  if (listener_text)
    {
      if (end == -1) end = XmTextGetLastPosition(listener_text);
      XmTextInsert(listener_text, end, (char *)msg);
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
  listener_delete_text(find_prompt(w, XmTextGetInsertionPosition(w)));
  restore_listener_string(true);
}


static void Listener_Meta_N(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  listener_delete_text(find_prompt(w, XmTextGetInsertionPosition(w)));
  restore_listener_string(false);
}


int save_listener_text(FILE *fp)
{
  /* return -1 if fwrite problem */
  if (listener_text)
    {
      char *str = NULL;
      str = XmTextGetString(listener_text);
      if (str)
	{
	  size_t bytes;
	  bytes = fwrite((void *)str, sizeof(char), mus_strlen(str), fp);
	  XtFree(str);
	  if (bytes == 0) return(-1);
	}
    }
  return(0);
}


static void Listener_clear(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  clear_listener();
}


static void Listener_g(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  ss->C_g_typed = true;
  control_g(any_selected_sound());
}


static void Listener_Backup(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  backup_listener_to_previous_expression();
}


static void Listener_Arrow_Up(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  if ((completions_pane) &&
      (XtIsManaged(completions_pane)))
    {
      int *ns;
      int n;
      XmListGetSelectedPos(completions_list, &ns, &n);
      if (ns[0] > 1)
	XmListSelectPos(completions_list, ns[0] - 1, false);
      free(ns);
    }
  else XtCallActionProc(w, "previous-line", event, str, *num);
}


static void Listener_Arrow_Down(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  if ((completions_pane) &&
      (XtIsManaged(completions_pane)))
    {
      int *ns;
      int n;
      XmListGetSelectedPos(completions_list, &ns, &n);
      XmListSelectPos(completions_list, ns[0] + 1, false);
      free(ns);
    }
  else XtCallActionProc(w, "next-line", event, str, *num);
}


static void Listener_Return(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  if ((completions_pane) &&
      (XtIsManaged(completions_pane)))
    {
      XmString *strs;
      XtVaGetValues(completions_list, 
		    XmNselectedItems, &strs, 
		    NULL);
      perform_completion(strs[0]);
      XtUnmanageChild(completions_pane);
    }
  else XtCallActionProc(w, "activate", event, str, *num);
}


#define NUM_ACTS 24
static XtActionsRec acts[NUM_ACTS] = {
  {(char *)"no-op",                      No_op},
  {(char *)"activate-keyboard",          Activate_keyboard},
  {(char *)"yank",                       Yank},
  {(char *)"delete-region",              Delete_region},
  {(char *)"kill-line",                  Kill_line},
  {(char *)"begin-of-line",              Begin_of_line},
  {(char *)"b1-press",                   B1_press},
  {(char *)"b1-move",                    B1_move},
  {(char *)"b1-release",                 B1_release},
  {(char *)"text-transpose",             Text_transpose},
  {(char *)"word-upper",                 Word_upper},
  {(char *)"tab-completion",             Tab_completion},
  {(char *)"listener-completion",        motif_listener_completion},
  {(char *)"listener-clear",             Listener_clear},
  {(char *)"listener-g",                 Listener_g},
  {(char *)"listener-meta-p",            Listener_Meta_P},
  {(char *)"listener-meta-n",            Listener_Meta_N},
  {(char *)"listener-next-line",         Listener_Arrow_Down},
  {(char *)"listener-previous-line",     Listener_Arrow_Up},
  {(char *)"listener-return",            Listener_Return},
  {(char *)"delete-to-previous-command", Listener_Backup},
  {(char *)"complain",                   Complain},
  {(char *)"help-at-cursor",             Help_At_Cursor},
};


/* translation tables for emacs compatibility and better inter-widget communication */
/* these values are listed in lib/Xm/Transltns.c */

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
	<Key>Tab:	    tab-completion()\n\
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
	<Key>Tab:	    tab-completion()\n\
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
        Ctrl <Key>?:        help-at-cursor()\n\
        Mod1 <Key>.:        help-at-cursor()\n\
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
	<Key>osfDown:       listener-next-line()\n\
	<Key>osfUp:         listener-previous-line()\n\
	Ctrl <Key>space:    set-anchor()\n\
	<Btn1Down>:	    b1-press()\n\
	<Btn1Up>:	    b1-release()\n\
	<Btn1Motion>:	    b1-move()\n\
	<Key>Tab:	    listener-completion()\n\
	<Key>Return:	    listener-return()\n";
static XtTranslations transTable4 = NULL;


void add_completer_to_builtin_textfield(Widget w, int completer)
{
  /* used to make file selection dialog's file and filter text widgets act like other text field widgets */
  if (!actions_loaded) 
    {
      XtAppAddActions(MAIN_APP(ss), acts, NUM_ACTS); 
      actions_loaded = true;
    }
  if (!transTable2) 
    transTable2 = XtParseTranslationTable(TextTrans2);

  XtOverrideTranslations(w, transTable2);
  XtVaSetValues(w, XmNuserData, completer, NULL);
}



/* -------- text related widgets -------- */

static XEN mouse_enter_text_hook;
static XEN mouse_leave_text_hook;

void mouse_enter_text_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  if (with_pointer_focus(ss))
    goto_window(w);

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


Widget make_textfield_widget(const char *name, Widget parent, Arg *args, int n, text_cr_t activatable, int completer)
{
  /* white background when active, emacs translations */
  Widget df;

  if (!actions_loaded) 
    {
      XtAppAddActions(MAIN_APP(ss), acts, NUM_ACTS); 
      actions_loaded = true;
    }

  XtSetArg(args[n], XmNuserData, completer); n++;
  XtSetArg(args[n], XmNhighlightThickness, 1); n++;
  XtSetArg(args[n], XmNcursorPositionVisible, false); n++;
  df = XtCreateManagedWidget(name, xmTextFieldWidgetClass, parent, args, n);

  if ((activatable != NOT_ACTIVATABLE_OR_FOCUSED) &&
      (activatable != ACTIVATABLE_BUT_NOT_FOCUSED))
    {
      XtAddCallback(df, XmNfocusCallback, textfield_focus_callback, NULL);
      XtAddCallback(df, XmNlosingFocusCallback, textfield_unfocus_callback, NULL);
    }
  else
    {
      XtAddCallback(df, XmNfocusCallback, textfield_no_color_focus_callback, NULL);
      XtAddCallback(df, XmNlosingFocusCallback, textfield_no_color_unfocus_callback, NULL);
    }

  XtAddEventHandler(df, EnterWindowMask, false, mouse_enter_text_callback, NULL);
  XtAddEventHandler(df, LeaveWindowMask, false, mouse_leave_text_callback, NULL);

  if ((activatable == ACTIVATABLE) ||
      (activatable == ACTIVATABLE_BUT_NOT_FOCUSED))
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

  return(df);
}



Widget make_text_widget(const char *name, Widget parent, Arg *args, int n)
{
  /* white background when active, emacs translations */
  /* used only for comment widget in file data box (snd-xfile.c), but needs to be in this file to pick up actions etc */
  Widget df;
  if (!actions_loaded) 
    {
      XtAppAddActions(MAIN_APP(ss), acts, NUM_ACTS); 
      actions_loaded = true;
    }
  XtSetArg(args[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;
  /* XmNblinkRate 0 turns off the cursor blink */
  XtSetArg(args[n], XmNcursorPositionVisible, false); n++;
  XtSetArg(args[n], XmNhighlightThickness, 1); n++;
  df = XmCreateScrolledText(parent, (char *)name, args, n);
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


/* ---------------- listener widget ---------------- */

static Widget lisp_window = NULL;

void listener_append(const char *msg)
{
  if (listener_text)
    XmTextInsert(listener_text, XmTextGetLastPosition(listener_text), (char *)msg);
}
 

void listener_append_and_prompt(const char *msg)
{
  if (listener_text)
    {
      XmTextPosition cmd_eot;
      if (msg)
	XmTextInsert(listener_text, XmTextGetLastPosition(listener_text), (char *)msg);
      cmd_eot = XmTextGetLastPosition(listener_text);
      XmTextInsert(listener_text, cmd_eot, listener_prompt_with_cr());
      cmd_eot = XmTextGetLastPosition(listener_text);
      XmTextShowPosition(listener_text, cmd_eot - 1);
    }
}


static void listener_return_callback(Widget w, XtPointer context, XtPointer info)
{
  listener_return(w, find_prompt(w, XmTextGetInsertionPosition(w)));
  /* prompt loc (last prompt pos) used only by read hook */
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
  pos = XmTextGetInsertionPosition(listener_text);
  if (pos > 2)
    {
      char *str;
      str = XmTextGetString(listener_text);
      if ((str[pos - 1] == ')') &&
	  ((str[pos - 2] != '\\') || (str[pos - 3] != '#')))
	{
	  int parens;
	  parens = find_matching_paren(str, 2, pos - 1, &paren_pos);
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

static void listener_motion_callback(Widget w, XtPointer context, XtPointer info)
{
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;
  int pos;

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
      char *str = NULL;
      str = XmTextGetString(w);
      if ((str[pos] == ')') && 
	  ((pos <= 1) || (str[pos - 1] != '\\') || (str[pos - 2] != '#')))
	{
	  int parens;
	  parens = find_matching_paren(str, 1, pos, &last_highlight_position);
	  if (parens == 0)
	    XmTextSetHighlight(w, last_highlight_position, last_highlight_position + 1, XmHIGHLIGHT_SECONDARY_SELECTED);
	}
      if (str) XtFree(str);
    }
}


static void listener_modify_callback(Widget w, XtPointer context, XtPointer info)
{
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;

  /* pure motion stuff (arrow keys) does not trigger this callback */

  if ((completions_pane) &&
      (XtIsManaged(completions_pane)))
    XtUnmanageChild(completions_pane);

  if (((cbs->text)->length > 0) || (dont_check_motion))
    cbs->doit = true;
  else
    { 
      char *str = NULL;
      int len;
      str = XmTextGetString(w);
      len = XmTextGetLastPosition(w);
      if (within_prompt(str, cbs->startPos, len))
	cbs->doit = false;
      else cbs->doit = true;
      if (str) XtFree(str);
    }
}


static XEN mouse_enter_listener_hook;
static XEN mouse_leave_listener_hook;

static void listener_focus_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  if (with_pointer_focus(ss))
    goto_window(listener_text);

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


/* ---------------- popup callbacks ---------------- */

static Widget listener_popup = NULL;

static void listener_help_callback(Widget w, XtPointer context, XtPointer info)
{
  char *txt;
  txt = XmTextGetSelection(listener_text);
  if (txt)
    {
      char *trim_txt;
      trim_txt = trim(txt);
      if (trim_txt)
	{
	  snd_help(trim_txt, XEN_TO_C_STRING(g_snd_help(C_TO_XEN_STRING(trim_txt), 0)), WITH_WORD_WRAP);
	  free(trim_txt);
	}
      XtFree(txt);
    }
  else text_at_cursor(listener_text);
}

static void listener_save_callback(Widget w, XtPointer context, XtPointer info)
{
  FILE *fp = NULL;
  fp = FOPEN("listener.txt", "w");
  if (fp) 
    {
      save_listener_text(fp);
      snd_fclose(fp, "listener.txt");
    }
}


static void listener_clear_callback(Widget w, XtPointer context, XtPointer info)
{
  clear_listener();
}


#if HAVE_SCHEME
static void listener_stacktrace_callback(Widget w, XtPointer context, XtPointer info)
{
  s7_pointer str;
  str = s7_eval_c_string(s7, "(stacktrace)");
  if (s7_string_length(str) == 0)
    str = s7_eval_c_string(s7, "(object->string (error-environment))");
  snd_display_result(s7_string(str), NULL);
}
#endif


static void listener_stop_callback(Widget w, XtPointer context, XtPointer info)
{
  control_g(any_selected_sound());
}


#if HAVE_SCHEME
static Widget stacktrace_popup_menu = NULL;
#endif

static void listener_popup_callback(Widget w, XtPointer context, XtPointer info)
{
  XmPopupHandlerCallbackStruct *cb = (XmPopupHandlerCallbackStruct *)info;
  XEvent *e;
  e = cb->event;
  if (e->type == ButtonPress)
    {
#if HAVE_SCHEME
      if (stacktrace_popup_menu)
	set_menu_label(stacktrace_popup_menu, (s7_is_null(s7, s7_current_environment(s7))) ? "Error info" : "Stacktrace");
#endif
      cb->menuToPost = listener_popup;
    }
}


static void make_listener_widget(int height)
{
  if (!listener_text)
    {
      Arg args[32];
      Widget wv, wh, w;
      int n;

      if (!actions_loaded) {XtAppAddActions(MAIN_APP(ss), acts, NUM_ACTS); actions_loaded = true;}

      n = attach_all_sides(args, 0);
      XtSetArg(args[n], XmNheight, height); n++;
      XtSetArg(args[n], XmNpaneMaximum, LOTSA_PIXELS); n++; /* Xm/Paned initializes each pane max to 1000 apparently! */

      if ((sound_style(ss) == SOUNDS_IN_NOTEBOOK) || (sound_style(ss) == SOUNDS_HORIZONTAL))
	listener_pane = XtCreateManagedWidget("frm", xmFormWidgetClass, SOUND_PANE_BOX(ss), args, n);
      else listener_pane = XtCreateManagedWidget("frm", xmFormWidgetClass, SOUND_PANE(ss), args, n);
      /* this widget is not redundant at least in Metroworks Motif */

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->listener_color); n++;
      XtSetArg(args[n], XmNforeground, ss->listener_text_color); n++;
      if (ss->listener_fontlist) {XtSetArg(args[n], XM_FONT_RESOURCE, ss->listener_fontlist); n++;}
      n = attach_all_sides(args, n);
      XtSetArg(args[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;
      XtSetArg(args[n], XmNskipAdjust, true); n++;
      XtSetArg(args[n], XmNvalue, listener_prompt(ss)); n++;
      XtSetArg(args[n], XmNpendingDelete, false); n++; /* don't cut selection upon paste */
      XtSetArg(args[n], XmNpositionIndex, XmLAST_POSITION); n++;
      XtSetArg(args[n], XmNhighlightThickness, 1); n++;
      listener_text = XmCreateScrolledText(listener_pane, (char *)"lisp-listener", args, n);
      ss->listener_pane = listener_text;

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNpopupEnabled, XmPOPUP_AUTOMATIC); n++;
      listener_popup = XmCreatePopupMenu(listener_text, (char *)"listener-popup", args, n);

      w = XtCreateManagedWidget(I_STOP, xmPushButtonWidgetClass, listener_popup, args, n);
      XtAddCallback(w, XmNactivateCallback, listener_stop_callback, NULL);

#if HAVE_SCHEME
      w = XtCreateManagedWidget("Stacktrace", xmPushButtonWidgetClass, listener_popup, args, n);
      XtAddCallback(w, XmNactivateCallback, listener_stacktrace_callback, NULL);
      stacktrace_popup_menu = w;
#endif

      w = XtCreateManagedWidget(I_HELP, xmPushButtonWidgetClass, listener_popup, args, n);
      XtAddCallback(w, XmNactivateCallback, listener_help_callback, NULL);

      w = XtCreateManagedWidget("Clear", xmPushButtonWidgetClass, listener_popup, args, n);
      XtAddCallback(w, XmNactivateCallback, listener_clear_callback, NULL);

      w = XtCreateManagedWidget("Save", xmPushButtonWidgetClass, listener_popup, args, n);
      XtAddCallback(w, XmNactivateCallback, listener_save_callback, NULL);

      XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, false, NULL);

      XtManageChild(listener_text);
      XmTextSetCursorPosition(listener_text, 1);
      if (!transTable4) 
	transTable4 = XtParseTranslationTable(TextTrans4);
      XtOverrideTranslations(listener_text, transTable4);
      XtAddCallback(listener_text, XmNactivateCallback, listener_return_callback, NULL);
      XtAddCallback(listener_text, XmNmodifyVerifyCallback, listener_modify_callback, NULL);
      XtAddCallback(listener_text, XmNmotionVerifyCallback, listener_motion_callback, NULL);

      lisp_window = XtParent(listener_text);
      XtAddEventHandler(lisp_window, EnterWindowMask, false, listener_focus_callback, NULL);
      XtAddEventHandler(lisp_window, LeaveWindowMask, false, listener_unfocus_callback, NULL);

      XtAddCallback(listener_text, XmNpopupHandlerCallback, listener_popup_callback, NULL);

      XmChangeColor(lisp_window, ss->basic_color);
      XtVaGetValues(lisp_window, XmNverticalScrollBar, &wv, 
		                 XmNhorizontalScrollBar, &wh, 
		                 NULL);
      XmChangeColor(wv, ss->basic_color);
      XmChangeColor(wh, ss->basic_color);
      map_over_children(SOUND_PANE(ss), color_sashes);

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
  ss->listener_color = pix;
  if (listener_text)
    XmChangeColor(listener_text, pix);
}


void color_listener_text(Pixel pix)
{
  ss->listener_text_color = pix;
  if (listener_text)
    XtVaSetValues(listener_text, XmNforeground, pix, NULL);
}


void handle_listener(bool open)
{
  if (open)
    {
      if (!listener_text)
	make_listener_widget(100);
      else 
	{
	  XtManageChild(listener_pane);
	  if (!(listener_is_visible()))
	    {
	      XtUnmanageChild(listener_pane);
	      XtVaSetValues(listener_pane, XmNpaneMinimum, 100, XmNpaneMaximum, LOTSA_PIXELS, NULL);
	      XtManageChild(listener_pane);
	      XtVaSetValues(listener_pane, XmNpaneMinimum, 1, NULL);
	    }
	}
    }
  else XtUnmanageChild(listener_pane);
}


bool listener_exists(void)
{
  return((bool)listener_text);
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


Widget make_togglebutton_widget(const char *name, Widget parent, Arg *args, int n)
{
  Widget w;
  w = XtCreateManagedWidget(name, xmToggleButtonWidgetClass, parent, args, n);
#if OVERRIDE_TOGGLE
  override_toggle_translation(w);
#endif
  return(w);
}


Widget make_pushbutton_widget(const char *name, Widget parent, Arg *args, int n)
{
  Widget w;
  w = XtCreateManagedWidget(name, xmPushButtonWidgetClass, parent, args, n);
#if OVERRIDE_TOGGLE
  override_toggle_translation(w); /* ??? activate here (rather than armandactivate) fails? */
#endif
  return(w);
}


static XEN g_listener_selection(void)
{
  #define H_listener_selection "(" S_listener_selection "): currently selected text in listener or " PROC_FALSE
  XEN res = XEN_FALSE;
  if (listener_text)
    {
      char *txt;
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
  if ((listener_text) && /* this can be called even when there is no listener */
      (XmTextGetCursorPosition(listener_text) > 1))
    {
      dont_check_motion = true;
      XmTextSetSelection(listener_text, 1, XmTextGetCursorPosition(listener_text), CurrentTime);
      XmTextRemove(listener_text);
      dont_check_motion = false;
    }
}


void set_listener_text_font(void)
{
  if (listener_text)
    XtVaSetValues(listener_text, XM_FONT_RESOURCE, ss->listener_fontlist, NULL);
}


static XEN g_goto_listener_end(void)
{
  #define H_goto_listener_end "(" S_goto_listener_end "): move cursor and scroll to bottom of listener pane"
  if (listener_text)
    {
      XmTextPosition eot;
      eot = XmTextGetLastPosition(listener_text);
      XmTextShowPosition(listener_text, eot);
      XmTextSetInsertionPosition(listener_text, eot);
      return(C_TO_XEN_INT(eot));
    }
  return(XEN_FALSE);
}


#ifdef XEN_ARGIFY_1
  XEN_NARGIFY_0(g_listener_selection_w, g_listener_selection)
  XEN_NARGIFY_0(g_reset_listener_cursor_w, g_reset_listener_cursor)
  XEN_NARGIFY_0(g_goto_listener_end_w, g_goto_listener_end)
#else
  #define g_listener_selection_w g_listener_selection
  #define g_reset_listener_cursor_w g_reset_listener_cursor
  #define g_goto_listener_end_w g_goto_listener_end
#endif

void g_init_gxlistener(void)
{
#if HAVE_SCHEME
  #define H_mouse_enter_listener_hook S_mouse_enter_listener_hook " (widget): called when the mouse \
enters the lisp listener pane:\n\
  (hook-push " S_mouse_enter_listener_hook "\n\
    (lambda (hook)\n\
      (" S_focus_widget " (hook 'widget))))"
#endif

#if HAVE_RUBY
  #define H_mouse_enter_listener_hook S_mouse_enter_listener_hook " (listener): called when the mouse \
enters the lisp listener pane:\n\
  $mouse_enter_listener_hook.add-hook!(\"enter\") do |widget|\n\
    focus_widget(widget)\n\
  end"
#endif

#if HAVE_FORTH
  #define H_mouse_enter_listener_hook S_mouse_enter_listener_hook " (listener): called when the mouse \
enters the lisp listener pane:\n\
" S_mouse_enter_listener_hook " lambda: <{ wid }> wid " S_focus_widget " ; add-hook!"
#endif

  #define H_mouse_leave_listener_hook S_mouse_leave_listener_hook " (widget): called when the mouse \
leaves the lisp listener pane"

  mouse_enter_listener_hook = XEN_DEFINE_HOOK(S_mouse_enter_listener_hook, "(make-hook 'widget)", 1, H_mouse_enter_listener_hook);
  mouse_leave_listener_hook = XEN_DEFINE_HOOK(S_mouse_leave_listener_hook, "(make-hook 'widget)", 1, H_mouse_leave_listener_hook);

#if HAVE_SCHEME
  #define H_mouse_enter_text_hook S_mouse_enter_text_hook " (widget): called when the mouse enters a text widget:\n\
(hook-push " S_mouse_enter_text_hook "\n\
  (lambda (w)\n\
    (" S_focus_widget " w)))"
#endif

#if HAVE_RUBY
  #define H_mouse_enter_text_hook S_mouse_enter_text_hook " (widget): called when the mouse enters a text widget:\n\
$mouse_enter_text_hook.add_hook!(\"enter\") do |w|\n\
    focus_widget(w)\n\
  end"
#endif

#if HAVE_FORTH
  #define H_mouse_enter_text_hook S_mouse_enter_text_hook " (widget): called when the mouse enters a text widget:\n\
" S_mouse_enter_text_hook " lambda: <{ wid }> wid " S_focus_widget " ; add-hook!"
#endif

  #define H_mouse_leave_text_hook S_mouse_leave_text_hook " (widget): called when the mouse leaves a text widget"
  
  mouse_enter_text_hook = XEN_DEFINE_HOOK(S_mouse_enter_text_hook, "(make-hook 'widget)", 1, H_mouse_enter_text_hook);
  mouse_leave_text_hook = XEN_DEFINE_HOOK(S_mouse_leave_text_hook, "(make-hook 'widget)", 1, H_mouse_leave_text_hook);

  XEN_DEFINE_PROCEDURE(S_listener_selection,    g_listener_selection_w,     0, 0, 0, H_listener_selection);
  XEN_DEFINE_PROCEDURE(S_reset_listener_cursor, g_reset_listener_cursor_w,  0, 0, 0, H_reset_listener_cursor);
  XEN_DEFINE_PROCEDURE(S_goto_listener_end,     g_goto_listener_end_w,      0, 0, 0, H_goto_listener_end);

  #define H_listener_click_hook S_listener_click_hook " (position): called when listener clicked; position is text pos of click in listener"
  listener_click_hook = XEN_DEFINE_HOOK(S_listener_click_hook, "(make-hook 'position)", 1,   H_listener_click_hook);

  preload_best_completions();
}
