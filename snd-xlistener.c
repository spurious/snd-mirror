#include "snd.h"

/* TODO:   To have the emacs/c-shell-style M-p and M-n paste in entries from
 * TODO        a history-list, whether in the listener or in the minibuffer.
 * TODO    bubble args help if tab at end of name? (or click name?)
 */


/* ---------------- text widget specializations ---------------- */

void textfield_focus_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  snd_state *ss = (snd_state *)clientData;
  if (!(ss->using_schemes)) XtVaSetValues(w,XmNbackground,(ss->sgx)->text_focus_color,NULL);
}

void textfield_unfocus_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  snd_state *ss = (snd_state *)clientData;
  if (!(ss->using_schemes)) XtVaSetValues(w,XmNbackground,(ss->sgx)->basic_color,NULL);
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
      keyboard_command(cp,(str[0][0] == 'u') ? snd_K_u : snd_K_x,CONTROL_KEY);
    }
}

static void Activate_channel (Widget w, XEvent *ev, char **str, Cardinal *num) 
{
  /* make the current channel active and abort if anything in progress */
  chan_info *cp;
  snd_state *ss;
  ss = get_global_state();
  clear_listener();
  if (ss->checking_explicitly) ss->stopped_explicitly = 1; 
  cp = current_channel(ss);
  if (cp) goto_graph(cp);
}

static char *listener_selection = NULL;

static void Kill_line(Widget w, XEvent *ev, char **str, Cardinal *num) 
{
  /* C-k with storage of killed text */
  XmTextPosition curpos,loc;
  Boolean found;
  curpos = XmTextGetCursorPosition(w);
  found = XmTextFindString(w,curpos,"\n",XmTEXT_FORWARD,&loc);
  if (!found) loc = XmTextGetLastPosition(w);
  if (loc > curpos)
    {
      if (listener_selection) XtFree(listener_selection);
      XmTextSetSelection(w,curpos,loc,CurrentTime);
      listener_selection = XmTextGetSelection(w); /* xm manual p329 sez storage is allocated here */
      XmTextCut(w,CurrentTime);
    }
}

static void Yank(Widget w, XEvent *ev, char **str, Cardinal *num) 
{
  /* copy current selection at current cursor position */
  XmTextPosition curpos;
  if (listener_selection) 
    {
      curpos = XmTextGetCursorPosition(w);
      XmTextInsert(w,curpos,listener_selection);
      curpos+=strlen(listener_selection);
      XmTextShowPosition(w,curpos);
      XmTextSetCursorPosition(w,curpos);
      XmTextClearSelection(w,ev->xkey.time); /* so C-y + edit doesn't forbid the edit */
    }
}

static int last_prompt;

static void Begin_of_line(Widget w, XEvent *ev, char **str, Cardinal *num) 
{
  /* don't back up before listener prompt */
  XmTextPosition curpos,loc;
  Boolean found;
  curpos = XmTextGetCursorPosition(w) - 1;
  found = XmTextFindString(w,curpos,"\n",XmTEXT_BACKWARD,&loc);
  XmTextSetCursorPosition(w,((found) && (loc>last_prompt)) ? (loc+1) : (last_prompt+1));
}

static void Delete_region(Widget w, XEvent *ev, char **str, Cardinal *num) 
{
  XmTextCut(w,CurrentTime);
}

static XmTextPosition down_pos, last_pos;
static Widget listener_text = NULL;

static void B1_press(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  XmTextPosition pos;
  XButtonEvent *ev = (XButtonEvent *)event;
  snd_state *ss;
  ss = get_global_state();
  XmProcessTraversal(w,XmTRAVERSE_CURRENT);
  /* we're replacing the built-in take_focus action here, so do it by hand, but leave listener blue, so to speak */
  if ((!(ss->using_schemes)) && (w != listener_text))
    XtVaSetValues(w,XmNbackground,(ss->sgx)->white,NULL);
  if (w == listener_text) XmTextClearSelection(listener_text,CurrentTime); /* should this happen in other windows as well? */
  pos = XmTextXYToPos(w,ev->x,ev->y);
  XmTextSetCursorPosition(w,pos);
  down_pos = pos;
  last_pos = pos;
}

static void B1_move(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  XmTextPosition pos;
  XButtonEvent *ev = (XButtonEvent *)event;
  pos = XmTextXYToPos(w,ev->x,ev->y);
  if (last_pos > pos) /* must have backed up the cursor */
    XmTextSetHighlight(w,pos,last_pos,XmHIGHLIGHT_NORMAL);
  if (down_pos != pos)
    XmTextSetHighlight(w,down_pos,pos,XmHIGHLIGHT_SELECTED);
  last_pos = pos;
}

static void B1_release(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  XmTextPosition pos;
  XButtonEvent *ev = (XButtonEvent *)event;
  pos = XmTextXYToPos(w,ev->x,ev->y);
  XmTextSetCursorPosition(w,pos);
  if (down_pos != pos)
    {
      XmTextSetHighlight(w,down_pos,pos,XmHIGHLIGHT_SELECTED);
      if (listener_selection) XtFree(listener_selection);
      XmTextSetSelection(w,down_pos,pos,CurrentTime);
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
      XmTextGetSubstring(w,(XmTextPosition)(curpos-1),2,3,buf);
      tmp = buf[0];
      buf[0]=buf[1];
      buf[1]=tmp;
      XmTextReplace(w,curpos-1,curpos+1,buf);
      XmTextSetCursorPosition(w,curpos+1);
    }
}

static void Word_upper(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  int i,j,length,wstart,wend,up,cap;
  XmTextPosition curpos,endpos;
  char *buf;
  up = (str[0][0] == 'u');
  cap = (str[0][0] == 'c');
  curpos = XmTextGetCursorPosition(w);
  endpos = XmTextGetLastPosition(w);
  if (curpos < endpos)
    {
      length = endpos - curpos;
      buf = (char *)CALLOC(length+1,sizeof(char));
      XmTextGetSubstring(w,curpos,length,length+1,buf);
      wstart = 0;
      wend = length;
      for (i=0;i<length;i++)
	if (!isspace((int)(buf[i])))
	  {
	    wstart=i;
	    break;
	  }
      for (i=wstart+1;i<length;i++)
	if (isspace((int)(buf[i])))
	  {
	    wend = i;
	    break;
	  }
      if (cap)
	{
	  buf[0] = toupper(buf[wstart]);
	  buf[1]='\0';
	  XmTextReplace(w,curpos+wstart,curpos+wstart+1,buf);
	}
      else
	{
	  for (i=wstart,j=0;i<wend;i++,j++)
	    if (up) 
	      buf[j] = toupper(buf[i]);
	    else buf[j] = tolower(buf[i]);
	  buf[j]='\0';
	  XmTextReplace(w,curpos+wstart,curpos+wend,buf);
	}
      XmTextSetCursorPosition(w,curpos+wend);
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
	  cmpwids_size = row+8;
	  if (cmpwids == NULL)
	    cmpwids = (Widget *)CALLOC(cmpwids_size,sizeof(Widget));
	  else 
	    {
	      cmpwids = (Widget *)REALLOC(cmpwids,cmpwids_size * sizeof(Widget));
	      for (;i<cmpwids_size;i++) cmpwids[i] = NULL;
	    }
	}
      cmpwids[row] = w;
    }
}

static void Name_completion(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  int data,i,matches,need_position;
  Position wx,wy;
  int xoff,yoff; 
  Window wn;
  Pixel old_color;
  char *old_text,*new_text,*search_text;
  snd_state *ss;
  ss = get_global_state();
  data = -1;
  for (i=0;i<cmpwids_size;i++)
    if (w == cmpwids[i])
      {
	data = i;
	break;
      }
  if (data >= 0)
    {
      old_text = XmTextGetString(w);
      new_text = complete_text(old_text,data);
      matches = get_completion_matches();
      XmTextSetString(w,new_text);
      XmTextSetCursorPosition(w,XmTextGetLastPosition(w));
      if ((strcmp(old_text,new_text) == 0) && (matches != -1))
	{
	  XtVaGetValues(w,XmNforeground,&old_color,NULL);
	  if (matches > 1)
	    XtVaSetValues(w,XmNforeground,(ss->sgx)->green,NULL);
	  else if (matches == 0) XtVaSetValues(w,XmNforeground,(ss->sgx)->red,NULL);
	  XmUpdateDisplay(w);
#if HAVE_SLEEP
	  sleep(1);
#endif
	  XtVaSetValues(w,XmNforeground,old_color,NULL);
	  XmUpdateDisplay(w);
	  if (matches > 1) 
	    {
	      set_save_completions(TRUE);
	      search_text = complete_text(old_text,data);
	      if (search_text) FREE(search_text);
	      need_position = (!(help_dialog_is_active()));
	      display_completions(ss);
	      set_save_completions(FALSE);
	      if (need_position)
		{
		  /* try to position the newly popped up help window below the text field */
		  XtVaGetValues(w,XmNx,&wx,XmNy,&wy,NULL);
		  XTranslateCoordinates(XtDisplay(w),XtWindow(w),DefaultRootWindow(XtDisplay(w)),0,0,&xoff,&yoff,&wn);
		  wx+=xoff; 
		  wy+=yoff;
		  move_help_dialog_to(wx,wy+40);
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
  XmTextInsert(listener_text,end,msg);
  XmTextSetCursorPosition(listener_text,XmTextGetLastPosition(listener_text));
}

static void Listener_completion(Widget w, XEvent *event, char **str, Cardinal *num) 
{
  /* used only by the listener widget -- needs to be smart about text since overall string can be enormous 
   *   and we don't want to back up past the last prompt
   *   also if at start of line (or all white-space to previous \n, indent
   */
  int beg,end,len,matches = 0,need_position;
  char *old_text,*new_text = NULL,*file_text = NULL;
  int try_completion = 1;
  Position wx,wy;
  int xoff,yoff; 
  Window wn;
  snd_state *ss;
  ss = get_global_state();
  beg = last_prompt+1;
  end = XmTextGetLastPosition(w);
  if (end <= beg) return;
  len = end-beg+1;
  old_text = (char *)CALLOC(len+1,sizeof(char));
  XmTextGetSubstring(w,beg,len,len+1,old_text);
  /* now old_text is the stuff typed since the last prompt */
  if (old_text)
    {
      new_text = complete_listener_text(old_text, end,&try_completion,&file_text);
      if (try_completion == 0)
	{
	  FREE(old_text);
	  return;
	}
      if (strcmp(old_text,new_text) == 0) matches = get_completion_matches();
      XmTextReplace(w,beg,end,new_text);
      XmTextSetCursorPosition(w,XmTextGetLastPosition(w));
      if (new_text) {FREE(new_text); new_text = NULL;}
      if (matches > 1)
	{
	  clear_possible_completions();
	  set_save_completions(TRUE);
	  if (file_text) new_text = filename_completer(file_text); else new_text = command_completer(old_text);
	  if (new_text) {FREE(new_text); new_text = NULL;}
	  need_position = (!(help_dialog_is_active()));
	  display_completions(ss);
	  set_save_completions(FALSE);
	  if (need_position)
	    {
	      /* try to position the newly popped up help window below the text field */
	      XtVaGetValues(w,XmNx,&wx,XmNy,&wy,NULL);
	      XTranslateCoordinates(XtDisplay(w),XtWindow(w),DefaultRootWindow(XtDisplay(w)),0,0,&xoff,&yoff,&wn);
	      wx+=xoff; 
	      wy+=yoff;
	      move_help_dialog_to(wx,wy+140);
	    }
	  if (file_text) FREE(file_text);
	}
      if (old_text) FREE(old_text);
    }
}

#define NUM_ACTS 14
static XtActionsRec acts[] = {
  {"no-op",No_op},
  {"activate-keyboard",Activate_keyboard},
  {"activate-channel",Activate_channel},
  {"yank",Yank},
  {"delete-region",Delete_region},
  {"kill-line",Kill_line},
  {"begin-of-line",Begin_of_line},
  {"b1-press",B1_press},
  {"b1-move",B1_move},
  {"b1-release",B1_release},
  {"text-transpose",Text_transpose},
  {"word-upper",Word_upper},
  {"name-completion",Name_completion},
  {"listener-completion",Listener_completion},
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

static void remember_event(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  XmAnyCallbackStruct *cb = (XmAnyCallbackStruct *)callData;
  (ss->sgx)->text_activate_event = cb->event;
  (ss->sgx)->text_widget = w;
}

Widget sndCreateTextFieldWidget(snd_state *ss, char *name, Widget parent, Arg *args, int n, int activatable, int completer)
{
  /* white background when active, emacs translations, text_activate_event in ss->sgx for subsequent activation check */
  Widget df;
  if (!actions_loaded) {XtAppAddActions(MAIN_APP(ss),acts,NUM_ACTS); actions_loaded = 1;}
  XtSetArg(args[n],XmNactivateCallback,make_callback_list(remember_event,(XtPointer)ss)); n++;
  /* can't use XmNuserData here because it is in use elsewhere (snd-xmix.c) */
  df = XtCreateManagedWidget(name,xmTextFieldWidgetClass,parent,args,n);
  XtAddCallback(df,XmNfocusCallback,textfield_focus_Callback,ss);
  XtAddCallback(df,XmNlosingFocusCallback,textfield_unfocus_Callback,ss);
  if (activatable == ACTIVATABLE)
    {
      if (!transTable2) transTable2 = XtParseTranslationTable(TextTrans2);
      XtOverrideTranslations(df,transTable2);
    }
  else
    {
      if (!transTable6) transTable6 = XtParseTranslationTable(TextTrans6);
      XtOverrideTranslations(df,transTable6);
    }
  add_completer_widget(df,completer);
  return(df);
}

void add_completer_to_textfield(snd_state *ss, Widget w, int completer)
{
  /* used to make file selection dialog act like other text field widgets */
  if (!actions_loaded) {XtAppAddActions(MAIN_APP(ss),acts,NUM_ACTS); actions_loaded = 1;}
  if (!transTable2) transTable2 = XtParseTranslationTable(TextTrans2);
  XtOverrideTranslations(w,transTable2);
  add_completer_widget(w,completer);
}

Widget sndCreateTextWidget(snd_state *ss, char *name, Widget parent, Arg *args, int n)
{
  /* white background when active, emacs translations, text_activate_event in ss->sgx for subsequent activation check */
  Widget df;
  if (!actions_loaded) {XtAppAddActions(MAIN_APP(ss),acts,NUM_ACTS); actions_loaded = 1;}
  XtSetArg(args[n],XmNactivateCallback,make_callback_list(remember_event,(XtPointer)ss)); n++;
  XtSetArg(args[n],XmNeditMode,XmMULTI_LINE_EDIT); n++;
  df = XmCreateScrolledText(parent,name,args,n);
  XtManageChild(df);
  /* df = XtCreateManagedWidget(name,xmTextWidgetClass,parent,args,n); */
  /* XtAddCallback(df,XmNfocusCallback,textfield_focus_Callback,ss); */
  XtAddCallback(df,XmNlosingFocusCallback,textfield_unfocus_Callback,ss);
  /* b1_press action overrides focus */
  if (!transTable3) transTable3 = XtParseTranslationTable(TextTrans3);
  XtOverrideTranslations(df,transTable3);
  return(df);
}


/* ---------------- command widget replacement ---------------- */

static Widget lisp_window = NULL;

void snd_append_char(snd_state *ss, char *msg)
{
  if (listener_text)
    {
      XmTextInsert(listener_text,XmTextGetLastPosition(listener_text),msg);
    }
}
 
static Widget listener_pane = NULL; 

static char listener_prompt_buffer[4];
static char *listener_prompt_with_cr(snd_state *ss)
{
  sprintf(listener_prompt_buffer,"\n%s",listener_prompt(ss));
  return(listener_prompt_buffer);
}
 
void snd_append_command(snd_state *ss, char *msg)
{
  int cmd_eot;
  if (listener_text)
    {
      if (ss->result_printout != PLAIN_MESSAGE) 
	XmTextInsert(listener_text,XmTextGetLastPosition(listener_text),"\n");
      if (msg)
	XmTextInsert(listener_text,XmTextGetLastPosition(listener_text),msg);
      cmd_eot = XmTextGetLastPosition(listener_text);
      if (ss->result_printout == MESSAGE_WITH_CARET) XmTextInsert(listener_text,cmd_eot,listener_prompt_with_cr(ss));
      ss->result_printout = 0;
      cmd_eot = XmTextGetLastPosition(listener_text);
      last_prompt = cmd_eot-1;
      XmTextShowPosition(listener_text,cmd_eot-1);
      XmUpdateDisplay(listener_text);
    }
}

static void Command_Return_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  /* try to find complete form either enclosing current cursor, or just before it */
  XmTextPosition new_eot=0,cmd_eot=0;
  char *str = NULL,*full_str = NULL,*prompt;
  int i,j,slen;
  snd_state *ss = (snd_state *)clientData;
  int end_of_text,start_of_text,last_position,current_position,parens;
  full_str = XmTextGetString(w);
  current_position = XmTextGetInsertionPosition(w);
  start_of_text = current_position;
  end_of_text = current_position;
  last_position = XmTextGetLastPosition(w);
  prompt = listener_prompt(ss);
  if (last_position > end_of_text)
    {
      for (i=current_position;i<last_position;i++)
	if ((full_str[i+1] == prompt[0]) && (full_str[i] == '\n'))
	  {
	    end_of_text = i-1;
	    break;
	  }
    }
  if (start_of_text > 0)
    {
      for (i=end_of_text;i>=0;i--)
	if ((full_str[i] == prompt[0]) && ((i == 0) || (full_str[i-1] == '\n')))
	  {
	    start_of_text = i+1;
	    break;
	  }
    }
  str = NULL;
#if 0
  fprintf(stderr,"eot: %d, sot: %d, curpos: %d, lastpos: %d\n",end_of_text,start_of_text,current_position,last_position);
#endif
  if (end_of_text > start_of_text)
    {
      parens = 0;
      slen = end_of_text - start_of_text + 2;
      str = (char *)CALLOC(slen,sizeof(char));
      for (i=start_of_text,j=0;i<=end_of_text;j++,i++) {str[j] = full_str[i]; if (str[j] == '(') parens++;}
      str[end_of_text-start_of_text+1] = 0;
      end_of_text = snd_strlen(str);
      if (parens)
	{
	  end_of_text = check_balance(str,0,end_of_text);
#if 0
	  fprintf(stderr,"eot: %d [%d], str: %s\n",end_of_text,slen,str);
#endif
	  if ((end_of_text > 0) && (end_of_text < (slen-1)))
	    {
	      str[end_of_text+1] = 0;
	      if (str[end_of_text] == '\n') str[end_of_text]=0;
	    }
	  else
	    {
	      FREE(str);
	      str = NULL;
	      new_eot = XmTextGetLastPosition(w);
	      XmTextInsert(w,new_eot,"\n");
	      return;
	    }
	}
      if (str)
	{
	  if (current_position < (last_position-2))
	    {
	      XmTextInsert(listener_text,XmTextGetLastPosition(listener_text),str);
	    }
	  XDefineCursor(XtDisplay(listener_text),XtWindow(listener_text),(ss->sgx)->wait_cursor);
	  XmUpdateDisplay(listener_text); /* not sure about this... */
#if 0
	  fprintf(stderr,"eval: %s\n",str);
#endif
	  snd_eval_listener_str(ss,str);
	  XUndefineCursor(XtDisplay(listener_text),XtWindow(listener_text));
	  FREE(str);
	  str = NULL;
	}
      else
	{
	  new_eot = XmTextGetLastPosition(w);
	  XmTextInsert(w,new_eot,listener_prompt_with_cr(ss));
	}
      last_prompt = XmTextGetLastPosition(w) - 1;
    }
  else 
    {
      new_eot = XmTextGetLastPosition(w);
      XmTextInsert(w,new_eot,"\n");
    }
  cmd_eot = XmTextGetLastPosition(w);
  XmTextShowPosition(w,cmd_eot-1);
  XmTextSetInsertionPosition(w,cmd_eot+1);
  if (full_str) XtFree(full_str);
}

static int last_highlight_position = -1;

static void Command_Motion_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)callData;
  snd_state *ss = (snd_state *)clientData;
  char *str=NULL,*prompt;
  int pos,i,parens;
  cbs->doit = TRUE; 
  if (last_highlight_position != -1)
    {
      XmTextSetHighlight(w,last_highlight_position,last_highlight_position+1,XmHIGHLIGHT_NORMAL);
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
	  for (i=pos-1;i>0;i--)
	    {
	      if ((i>0) && (str[i] == prompt[0]) && (str[i-1] == '\n'))
		break;
	      if (str[i] == ')') parens++;
	      if (str[i] == '(') parens--;
	      if (parens == 0)
		{
		  XmTextSetHighlight(w,i,i+1,XmHIGHLIGHT_SECONDARY_SELECTED);
		  last_highlight_position = i;
		  break;
		}
	    }
	}
      if (str) XtFree(str);
    }
}

static void Command_Modify_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)callData;
  snd_state *ss = (snd_state *)clientData;
  char *str=NULL,*prompt;
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
	  if ( ((str[cbs->currInsert-1] == prompt[0]) && (str[cbs->currInsert - 2] == '\n')) ||
	       ((cbs->currInsert < (len-1)) && (str[cbs->currInsert] == prompt[0]) && (str[cbs->currInsert-1] == '\n')))
	    cbs->doit = FALSE;
	  else cbs->doit = TRUE;
#if 0
	  fprintf(stderr,"insert: %d (%c %c %c), len: %d\n",
		  cbs->currInsert,
		  (cbs->currInsert > 0) ? str[cbs->currInsert - 1] : '?',
		  str[cbs->currInsert],
		  (cbs->currInsert < len) ? str[cbs->currInsert + 1] : '?',
		  (cbs->text)->length);
#endif	  
	  if (str) XtFree(str);
	}
    }
}

static void Command_Help_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  listener_dialog_help((snd_state *)clientData);
}

static void sndCreateCommandWidget(snd_state *ss, int height)
{
  Arg args[32];
  Widget wv,wh;
  int n;
  if (!listener_text)
    {
      if (!actions_loaded) {XtAppAddActions(MAIN_APP(ss),acts,NUM_ACTS); actions_loaded = 1;}

      n=0;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNheight,height); n++;
      if ((sound_style(ss) == SOUNDS_IN_NOTEBOOK) || (sound_style(ss) == SOUNDS_HORIZONTAL))
	listener_pane = sndCreateFormWidget("frm",SOUND_PANE_BOX(ss),args,n);
      else listener_pane = sndCreateFormWidget("frm",SOUND_PANE(ss),args,n);
      /* this widget is not redundant at least in Metroworks Motif */

      n=0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n],XmNbackground,(ss->sgx)->listener_color); n++;
	}
      if ((ss->sgx)->listener_fontlist) {XtSetArg(args[n],XM_FONT_RESOURCE,(ss->sgx)->listener_fontlist); n++;}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNactivateCallback,make_callback_list(remember_event,(XtPointer)ss)); n++;
      XtSetArg(args[n],XmNeditMode,XmMULTI_LINE_EDIT); n++;
      XtSetArg(args[n],XmNskipAdjust,TRUE); n++;
      XtSetArg(args[n],XmNvalue,listener_prompt(ss)); n++;
      XtSetArg(args[n],XmNpendingDelete,FALSE); n++; /* don't cut selection upon paste */
      XtSetArg(args[n],XmNpositionIndex,XmLAST_POSITION); n++;
      XtSetArg(args[n],XmNpaneMinimum,height); n++;

      listener_text = XmCreateScrolledText(listener_pane,"lisp-listener",args,n);
#if 0
      {
	XtActionList lst;
	XtActionsRec *ls;
	Cardinal num,i;
	XtGetActionList(xmFormWidgetClass,&lst,&num);
	ls = lst;
	for (i=0;i<num;i++) fprintf(stderr,"%s\n",ls[i].string);
      }
#endif

      XtManageChild(listener_text);
      XmTextSetCursorPosition(listener_text,1);
      if (!transTable4) transTable4 = XtParseTranslationTable(TextTrans4);
      XtOverrideTranslations(listener_text,transTable4);
      last_prompt = 0;
      XtAddCallback(listener_text,XmNactivateCallback,Command_Return_Callback,ss);
      XtAddCallback(listener_text,XmNmodifyVerifyCallback,Command_Modify_Callback,ss);
      XtAddCallback(listener_text,XmNmotionVerifyCallback,Command_Motion_Callback,ss);
      XtAddCallback(listener_text,XmNhelpCallback,Command_Help_Callback,ss);
      
      lisp_window = XtParent(listener_text);
      
      if (!(ss->using_schemes))
	{
	  XmChangeColor(lisp_window,(ss->sgx)->basic_color);
	  XtVaGetValues(lisp_window,XmNverticalScrollBar,&wv,XmNhorizontalScrollBar,&wh,NULL);
	  XmChangeColor(wv,(ss->sgx)->basic_color);
	  XmChangeColor(wh,(ss->sgx)->basic_color);
	  map_over_children(SOUND_PANE(ss),color_sashes,(void *)ss);
	}
      XtVaSetValues(lisp_window,XmNpaneMinimum,1,NULL);
    }
}

void goto_listener(void) 
{
  goto_window(listener_text);
  XmTextSetCursorPosition(listener_text,XmTextGetLastPosition(listener_text)+1);
  XmTextSetInsertionPosition(listener_text,XmTextGetLastPosition(listener_text)+1);
}

void color_listener(Pixel pix)
{
  snd_state *ss;
  ss = get_global_state();
  (ss->sgx)->listener_color = pix;
  if (listener_text)
    XmChangeColor(listener_text,pix);
}

void handle_listener(snd_state *ss, int new_state)
{
  if (!listener_text)
    {
      /* fire up listener at bottom of overall snd window */
      if (new_state == LISTENER_OPEN) 
	{
	  sndCreateCommandWidget(ss,100);
	  set_view_listener_label(STR_Hide_listener);
	  goto_window(listener_text);
	}
      else sndCreateCommandWidget(ss,1);
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
	  XtVaSetValues(listener_pane,XmNpaneMaximum,1,XmNpaneMinimum,1,NULL);
	  XtManageChild(listener_pane);
	  XtVaSetValues(listener_pane,XmNpaneMaximum,LOTSA_PIXELS,XmNpaneMinimum,1,NULL);
	}
      else
	{
	  /* reopen listener pane */
	  ss->listening = LISTENER_OPEN;
	  set_view_listener_label(STR_Hide_listener);
	  XtUnmanageChild(listener_pane);
	  XtVaSetValues(listener_pane,XmNpaneMinimum,100,NULL);
	  XtManageChild(listener_pane);
	  XtVaSetValues(listener_pane,XmNpaneMinimum,1,NULL);
	}
    }
}

int listener_height(void)
{
  if (listener_text) return(widget_height(listener_text)); else return(0);
}

#if OVERRIDE_TOGGLE
/* Metrolink Motif defines control-button1 to be "take focus" and then segfaults if you use it!! */
static char ToggleTrans2[] =
       "c<Btn1Down>:   ArmAndActivate()\n";
static XtTranslations toggleTable2 = NULL;

static void override_toggle_translation(Widget w)
{
  if (!toggleTable2) toggleTable2 = XtParseTranslationTable(ToggleTrans2);
  XtOverrideTranslations(w,toggleTable2);
}

static char ToggleTrans3[] =
       "c<Btn1Down>:   DrawingAreaInput()\n";
static XtTranslations toggleTable3 = NULL;

static void override_drawing_translation(Widget w)
{
  if (!toggleTable3) toggleTable3 = XtParseTranslationTable(ToggleTrans3);
  XtOverrideTranslations(w,toggleTable3);
}

static char ToggleTrans4[] =
       "c<Btn1Down>:   Activate()\n";
static XtTranslations toggleTable4 = NULL;

static void override_manager_translation(Widget w)
{
  if (!toggleTable4) toggleTable4 = XtParseTranslationTable(ToggleTrans4);
  XtOverrideTranslations(w,toggleTable4);
}

static char ToggleTrans5[] =
       "c<Btn1Down>:   Return()\n";
static XtTranslations toggleTable5 = NULL;

void override_form_translation(Widget w)
{
  if (!toggleTable5) toggleTable5 = XtParseTranslationTable(ToggleTrans5);
  XtOverrideTranslations(w,toggleTable5);
}

/* push buttons also need the override, but they aren't causing Snd to crash for some reason */

#endif

Widget sndCreateFormWidget(char *name, Widget parent, Arg *args, int n)
{
  Widget w;
  w = XtCreateManagedWidget(name,xmFormWidgetClass,parent,args,n);
#if OVERRIDE_TOGGLE
  override_form_translation(w);
#endif
  return(w);
}

Widget sndCreateToggleButtonWidget(char *name, Widget parent, Arg *args, int n)
{
  Widget w;
  w = XtCreateManagedWidget(name,xmToggleButtonWidgetClass,parent,args,n);
#if OVERRIDE_TOGGLE
  override_toggle_translation(w);
#endif
  return(w);
}

Widget sndCreatePushButtonWidget(char *name, Widget parent, Arg *args, int n)
{
  Widget w;
  w = XtCreateManagedWidget(name,xmPushButtonWidgetClass,parent,args,n);
#if OVERRIDE_TOGGLE
  override_toggle_translation(w);
#endif
  return(w);
}

Widget sndCreateFrameWidget(char *name, Widget parent, Arg *args, int n)
{
  Widget w;
  w = XtCreateManagedWidget(name,xmFrameWidgetClass,parent,args,n);
#if OVERRIDE_TOGGLE
  override_manager_translation(w);
#endif
  return(w);
}

Widget sndCreateRowColumnWidget(char *name, Widget parent, Arg *args, int n)
{
  Widget w;
  w = XtCreateManagedWidget(name,xmRowColumnWidgetClass,parent,args,n);
#if OVERRIDE_TOGGLE
  override_manager_translation(w);
#endif
  return(w);
}

Widget sndCreateDrawingAreaWidget(char *name, Widget parent, Arg *args, int n)
{
  Widget w;
  w = XtCreateManagedWidget(name,xmDrawingAreaWidgetClass,parent,args,n);
#if OVERRIDE_TOGGLE
  override_drawing_translation(w);
#endif
  return(w);
}

Widget sndCreatePanedWindowWidget(char *name, Widget parent, Arg *args, int n)
{
  Widget w;
  w = XtCreateManagedWidget(name,xmPanedWindowWidgetClass,parent,args,n);
#if OVERRIDE_TOGGLE
  override_manager_translation(w);
#endif
  return(w);
}

