#include "snd.h"

/* check_balance stolen from scwm-0.9/utilities/scwmrepl/scwmrepl.c */

int check_balance(char *expr, int start, int end) 
{
  /* If you think _this_ is hairy, try doing it for C statements. */
  int i;
  int non_whitespace_p = 0;
  int paren_count = 0;
  int prev_separator = 1;
  int quote_wait = 0;

  i = start;
  while (i < end) {
    switch(expr[i]) {
    case ';' :
      /* skip till newline. */
      do {
	i++;
      } while (expr[i]!='\n' && i < end);
      break;
    case ' ':
    case '\n':
    case '\t':
    case '\r':
      if (non_whitespace_p && paren_count==0 && !quote_wait) {
	return i;
      } else {
	prev_separator = 1;
	i++;
      }
      break;
    case '\"' :
      if (non_whitespace_p && paren_count==0 &&
	  !quote_wait) {
	return i;
      } else {
	/* skip past ", ignoring \" */
	do {
 	  i++;
	  if (i < end && expr[i] =='\\') {
	    i++;
	  }
	} while (i < end && expr[i]!='\"');
	i++;
	if (paren_count==0) {
	  if (i < end) {
	    return i;
	  } else {
	    return 0;
	  }
	} else {
	  prev_separator = 1;
	  non_whitespace_p = 1;
	  quote_wait = 0;
	}
      }
      break;
    case '#' :
      if (non_whitespace_p && paren_count==0 &&
	  !quote_wait) {
	return i;
      } else {
	if (prev_separator && i + 1 < end && expr[i + 1] =='{') {
	  /* skip past }#, ignoring \} */
	  do {
	    i++;
	    if (i < end && expr[i] =='\\') {
	      i++;
	    }
	  } while (i < end && !(expr[i] =='}' && i + 1 < end
				&& expr[i + 1] =='#'));
	  i += 2;
	  if (paren_count==0) {
	    if (i < end) {
	      return i;
	    } else {
	      return 0;
	    }
	  } else {
	    prev_separator = 1;
	    non_whitespace_p = 1;
	    quote_wait = 0;
	  }
	  /* MS:FIXME:: Handle #\) properly! */
	} else {
	  prev_separator = 0;
	  quote_wait = 0;
	  non_whitespace_p = 1;
	  i++;
	}
      }
      break;
    case '(' :
      if (non_whitespace_p && paren_count==0 &&!quote_wait) {
	return i;
      } else {
	i++;
	paren_count++;
	non_whitespace_p = 1;
	prev_separator = 1;
	quote_wait = 0;
      }
      break;
    case ')' :
      paren_count--;
      if (non_whitespace_p && paren_count==0) {
	return i + 1;
      } else {
	i++;
	non_whitespace_p = 1;
	prev_separator = 1;
	quote_wait = 0;
      }
      break;
    case '\'' :
      if (prev_separator) {
	non_whitespace_p = 1;
	quote_wait = 1;
	prev_separator = 1;
	i++;
      } else {
	non_whitespace_p = 1;
	prev_separator = 0;
	i++;
      }
      break;
    default :
      prev_separator = 0;
      quote_wait = 0;
      non_whitespace_p = 1;
      i++;
      break;
    }
  }
  return 0;
}

static char listener_prompt_buffer[LABEL_BUFFER_SIZE];
char *listener_prompt_with_cr(snd_state *ss)
{
  mus_snprintf(listener_prompt_buffer, LABEL_BUFFER_SIZE, "\n%s", listener_prompt(ss));
  return(listener_prompt_buffer);
}
 
#if (!USE_NO_GUI)
#if USE_GTK
#define GUI_TEXT_END(w) gtk_text_get_length(GTK_TEXT(w))
#define GUI_TEXT_POSITION_TYPE gint
#define GUI_TEXT(w) gtk_editable_get_chars(GTK_EDITABLE(w), 0, -1)
#define GUI_SET_TEXT(w, text) gtk_text_insert(GTK_TEXT(w), (ss->sgx)->help_text_fnt, (ss->sgx)->black, (ss->sgx)->white, text, -1)
#define GUI_TEXT_INSERTION_POSITION(w) gtk_editable_get_position(GTK_EDITABLE(w))
#define GUI_TEXT_SET_INSERTION_POSITION(w, pos) gtk_editable_set_position(GTK_EDITABLE(w), pos-1)
#define GUI_LISTENER_TEXT_INSERT(w, pos, text) append_listener_text(0, text)
#define GUI_STATS_TEXT_INSERT(w, pos, text) gtk_text_insert(GTK_TEXT(w), (ss->sgx)->help_text_fnt, (ss->sgx)->black, (ss->sgx)->white, text, -1)
#define GUI_FREE(w) g_free(w)
#define GUI_SET_CURSOR(w, cursor) gdk_window_set_cursor(w->window, cursor)
#define GUI_UNSET_CURSOR(w, cursor) gdk_window_set_cursor(w->window, cursor)
#define GUI_UPDATE(w) 
#define GUI_TEXT_GOTO(w, pos) gtk_editable_set_position(GTK_EDITABLE(w), pos)
#else
#define GUI_TEXT_END(w) XmTextGetLastPosition(w)
#define GUI_TEXT_POSITION_TYPE XmTextPosition
#define GUI_TEXT(w) XmTextGetString(w)
#define GUI_SET_TEXT(w, text) XmTextSetString(w, text)
#define GUI_TEXT_INSERTION_POSITION(w) XmTextGetInsertionPosition(w)
#define GUI_TEXT_SET_INSERTION_POSITION(w, pos) XmTextSetInsertionPosition(w, pos)
#define GUI_LISTENER_TEXT_INSERT(w, pos, text) XmTextInsert(w, pos, text)
#define GUI_STATS_TEXT_INSERT(w, pos, text) XmTextInsert(w, pos, text)
#define GUI_FREE(w) XtFree(w)
#define GUI_SET_CURSOR(w, cursor) XUndefineCursor(XtDisplay(w), XtWindow(w)); XDefineCursor(XtDisplay(w), XtWindow(w), cursor)
#define GUI_UNSET_CURSOR(w, cursor) XUndefineCursor(XtDisplay(w), XtWindow(w)); XDefineCursor(XtDisplay(w), XtWindow(w), None)
#define GUI_UPDATE(w) XmUpdateDisplay(w)
#define GUI_TEXT_GOTO(w, pos) XmTextShowPosition(w, pos)
#endif
#endif

static SCM read_str_wrapper(void *data)
{
  return(TO_SCM_FORM((char *)data));
}

static SCM read_hook;

void command_return(GUI_WIDGET w, snd_state *ss, int last_prompt)
{
#if (!USE_NO_GUI)
  /* try to find complete form either enclosing current cursor, or just before it */
  GUI_TEXT_POSITION_TYPE new_eot = 0, cmd_eot = 0;
  char *str = NULL, *full_str = NULL, *prompt;
  int i, j, slen;
  SCM form = SCM_UNDEFINED;
  int end_of_text, start_of_text, last_position, current_position, parens;
  full_str = GUI_TEXT(w);
  current_position = GUI_TEXT_INSERTION_POSITION(w);
  start_of_text = current_position;
  end_of_text = current_position;
  last_position = GUI_TEXT_END(w);
  
  if (HOOKED(read_hook))
    {
      SCM result;
      str = (char *)CALLOC(last_position - last_prompt + 1, sizeof(char));
      for (i = last_prompt + 1, j = 0; i < last_position; i++, j++) str[j] = full_str[i];
      result = g_c_run_or_hook(read_hook, 
			       SCM_LIST1(TO_SCM_STRING(str)),
			       S_read_hook);
      FREE(str);
      if (TRUE_P(result)) return;
    }

  prompt = listener_prompt(ss);
  /* first look for a form just before the current mouse location,
   *   independent of everything (i.e. user may have made changes
   *   in a form included in a comment, then typed return, expecting
   *   us to use the new version, but the check_balance procedure
   *   tries to ignore comments).
   */
  str = NULL;
  for (i = current_position - 1; i >= 0; i--)
    if (full_str[i] == '\n')
      break;
    else
      if (full_str[i] == ';')
	{
	  /* now look for complete form from current_position backwards */
	  parens = 0;
	  start_of_text = i;
	  end_of_text = -1;
	  for (i = current_position; i > start_of_text; i--)
	    if (full_str[i] == ')')
	      {
		if (end_of_text == -1)
		  end_of_text = i;
		parens--;
	      }
	    else
	      if (full_str[i] == '(')
		{
		  parens++;
		  start_of_text = i;
		}
	  if ((parens == 0) && (end_of_text != -1))
	    {
	      str = (char *)CALLOC(end_of_text - start_of_text + 2, sizeof(char));
	      for (i = start_of_text, j = 0; i <= end_of_text; j++, i++) 
		str[j] = full_str[i]; 
	    }
	  else
	    {
	      start_of_text = current_position;
	      end_of_text = current_position;
	    }
	  break;
	}
  if (str == NULL)
    {
      if (last_position > end_of_text)
	{
	  for (i = current_position; i < last_position; i++)
	    if ((full_str[i + 1] == prompt[0]) && 
		(full_str[i] == '\n'))
	      {
		end_of_text = i - 1;
		break;
	      }
	}
      if (start_of_text > 0)
	{
	  for (i = end_of_text; i >= 0; i--)
	    if ((full_str[i] == prompt[0]) && 
		((i == 0) || 
		 (full_str[i - 1] == '\n')))
	      {
		start_of_text = i + 1;
		break;
	      }
	}
      if (end_of_text > start_of_text)
	{
	  parens = 0;
	  slen = end_of_text - start_of_text + 2;
	  str = (char *)CALLOC(slen, sizeof(char));
	  for (i = start_of_text, j = 0; i <= end_of_text; j++, i++) 
	    {
	      str[j] = full_str[i]; 
	      if (str[j] == '(') 
		parens++;
	    }
	  str[end_of_text - start_of_text + 1] = 0;
	  end_of_text = snd_strlen(str);
	  /* fprintf(stderr, "got: %s %d parens ", str, parens); */
	  if (parens)
	    {
	      end_of_text = check_balance(str, 0, end_of_text);
	      /* fprintf(stderr, "now eot: %d (%d) ", end_of_text, slen); */
	      if ((end_of_text > 0) && 
		  (end_of_text < slen))
		{
		  if (end_of_text < (slen - 1))
		    str[end_of_text + 1] = 0;
		  else str[end_of_text] = 0;
		  if (str[end_of_text] == '\n') str[end_of_text] = 0;
		  /* fprintf(stderr, "now str: %s ", str); */
		}
	      else
		{
		  FREE(str);
		  str = NULL;
		  new_eot = GUI_TEXT_END(w);
		  GUI_LISTENER_TEXT_INSERT(w, new_eot, "\n");
		  return;
		}
	    }
	  else
	    {
	      /* no parens -- pick up closest entity */
	      int loc, k, len;
	      char *tmp;
	      loc = current_position - start_of_text - 1;
	      for (i = loc; i >= 0; i--)
		if ((str[i] == '\n') || (i == 0))
		  {
		    len = snd_strlen(str);
		    tmp = (char *)CALLOC(len + 1, sizeof(char));
		    if (i != 0) i++;
		    for (k = 0; i < len; i++, k++) 
		      if ((i > loc) &&
			  ((str[i] == '\n') || 
			   (str[i] == ' ')))
			break;
		      else tmp[k] = str[i];
		    FREE(str);
		    str = tmp;
		    break;
		  }
	    }
	}
    }
  if (full_str) GUI_FREE(full_str);
  if (str)
    {
      if (current_position < (last_position - 2))
	GUI_LISTENER_TEXT_INSERT(w, GUI_TEXT_END(w), str);
      GUI_SET_CURSOR(w, (ss->sgx)->wait_cursor);
      GUI_UPDATE(w); /* not sure about this... */
      /* 
       * the division into a read, a free, then an eval is needed to handle continuations correctly:
       *   
       *    (set! (sample (cursor))
       *      (call-with-current-continuation
       *       (lambda (rsvp)
       *	 (prompt-in-minibuffer "sample:" rsvp)
       *	 (sample (cursor)))))
       *
       *   will return ((long)jump) to the eval_form_wrapper below.  If we make any assumptions
       *   about pointer allocation here (i.e. str not NULL), or cursor style, then the
       *   global jump will confuse free (i.e. try to free a not-allocated string).  So, 
       *   eval_str_wrapper can't be used since it assumes the string is still valid.
       *   I assume the form is ok because the continuation will preserve it somehow.
       *
       */
      if ((snd_strlen(str) > 1) || (str[0] != '\n'))
	form = snd_catch_any(read_str_wrapper, (void *)str, str);  /* catch needed else #< in input exits Snd! */
      FREE(str);
      str = NULL;
      if (BOUND_P(form))
	snd_report_listener_result(ss, form);
      GUI_UNSET_CURSOR(w, (ss->sgx)->arrow_cursor);
    }
  else
    {
      new_eot = GUI_TEXT_END(w);
      GUI_LISTENER_TEXT_INSERT(w, new_eot, listener_prompt_with_cr(ss));
      last_prompt = GUI_TEXT_END(w) - 1;
    }
  cmd_eot = GUI_TEXT_END(w);
  GUI_TEXT_GOTO(w, cmd_eot - 1);
  GUI_TEXT_SET_INSERTION_POSITION(w, cmd_eot + 1);
#endif
}

#if HAVE_MALLINFO
  #include <malloc.h>
#endif

#ifdef DEBUG_MEMORY
  char *mem_stats(snd_state *ss, int ub);
#endif

#if DEBUGGING
  void report_header_stats(int *vals);
  void report_sound_stats(int *vals);
  void report_io_stats(int *vals);
#endif

#define STATS_BUFFER_SIZE 2048

void update_stats_with_widget(snd_state *ss, GUI_WIDGET stats_form)
{
#if (!USE_NO_GUI)
  int i, j, regs;
  int used_bytes = 0;
  GUI_TEXT_POSITION_TYPE pos;
  int vals[2];
  snd_info *sp;
  chan_info *cp;
  char *str, *r0 = NULL, *r1 = NULL;
  if (stats_form == NULL) return;
  GUI_SET_TEXT(stats_form, "file chn: mem(#bufs), main, temp(#files), amp envs\n\n");
  for (i = 0; i < ss->max_sounds; i++)
    if ((sp = ((snd_info *)(ss->sounds[i]))) &&	(sp->inuse))
      for (j = 0; j<(sp->nchans); j++)
	if ((cp = ((chan_info *)(sp->chans[j]))) && (cp->stats))
	  {
	    pos = GUI_TEXT_END(stats_form);
	    str = update_chan_stats(cp);
	    GUI_STATS_TEXT_INSERT(stats_form, pos, str);
	    FREE(str);
	  }
  regs = snd_regions();
  if (regs > 0)
    {
      region_stats(vals);
      str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
      mus_snprintf(str, PRINT_BUFFER_SIZE, "\nregions (%d): array: %s + file: %s\n",
	      regs,
	      r0 = kmg(vals[0]),
	      r1 = kmg(vals[1]));
      pos = GUI_TEXT_END(stats_form);
      GUI_STATS_TEXT_INSERT(stats_form, pos, str);
      if (r0) free(r0);
      if (r1) free(r1);
      FREE(str);
    }
#if HAVE_MALLINFO
  {
    struct mallinfo mall;
    char *m0 = NULL, *m1 = NULL, *m2 = NULL, *m3 = NULL;
    mall = mallinfo();
    str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
    mus_snprintf(str, PRINT_BUFFER_SIZE, "\nmalloc: %s + %s (in use: %s, freed: %s)\n",
	    m0 = kmg(mall.arena),
	    m1 = kmg(mall.hblkhd),
	    m2 = kmg(used_bytes = mall.uordblks),
	    m3 = kmg(mall.fordblks));
    pos = GUI_TEXT_END(stats_form);
    GUI_STATS_TEXT_INSERT(stats_form, pos, str);
    if (m0) free(m0);
    if (m1) free(m1);
    if (m2) free(m2);
    if (m3) free(m3);
    FREE(str);
  }
#endif
#ifdef DEBUG_MEMORY
  str = mem_stats(ss, used_bytes);
  pos = GUI_TEXT_END(stats_form);
  GUI_STATS_TEXT_INSERT(stats_form, pos, str);
  free(str);
#endif
#if HAVE_GUILE && HAVE_CLOCK && HAVE_LONG_LONGS && HAVE_SCM_NUM2LONG_LONG
  {
    int len;
    SCM stats;
    long long gc_swept = 0, gc_heap = 0, gc_cells = 0;
    Float gc_time;
    stats = scm_gc_stats();
    if (LIST_P_WITH_LENGTH(stats, len))
      {
#ifdef SCM_NUM2LONG_LONG
#define FUNC_NAME __FUNCTION__
	if (len > 7)
	  gc_swept = SCM_NUM2LONG_LONG(SCM_ARG1, SCM_CDR(LIST_REF(stats, 9)));
	gc_heap = SCM_NUM2LONG_LONG(SCM_ARG1, SCM_CDR(LIST_REF(stats, 2)));
	gc_cells = SCM_NUM2LONG_LONG(SCM_ARG1, SCM_CDR(LIST_REF(stats, 1)));
#undef FUNC_NAME
#else
	if (len > 7)
	  gc_swept = scm_num2long_long(SCM_CDR(LIST_REF(stats, 9)), (char *)SCM_ARG1, __FUNCTION__);
	gc_heap = scm_num2long_long(SCM_CDR(LIST_REF(stats, 2)), (char *)SCM_ARG1, __FUNCTION__);
	gc_cells = scm_num2long_long(SCM_CDR(LIST_REF(stats, 1)), (char *)SCM_ARG1, __FUNCTION__);
#endif
	gc_time = (float)(TO_C_INT(SCM_CDR(LIST_REF(stats, 0)))) / 1000.0;
	str = (char *)CALLOC(STATS_BUFFER_SIZE,sizeof(char));
	if (len > 7)
	  mus_snprintf(str, STATS_BUFFER_SIZE,
		  "\nGuile:\n  gc time: %.2f secs (%d sweeps)\n  cells: %Ld (%Ld gc'd)\n  heap size: %Ld",
		  gc_time,
		  TO_C_INT(SCM_CDR(LIST_REF(stats, 5))),     /* times */
		  gc_cells,
		  gc_swept,
		  gc_heap);
	else
	  mus_snprintf(str, STATS_BUFFER_SIZE,
		  "\nGuile:\n  gc time: %.2f secs\n  cells: %Ld\n  heap size: %Ld",
		  gc_time,
		  gc_cells,
		  gc_heap);
	pos = GUI_TEXT_END(stats_form);
	GUI_STATS_TEXT_INSERT(stats_form, pos, str);
	FREE(str);
      }
  }
#endif
#endif
}

static SCM g_save_listener(SCM filename)
{
  #define H_save_listener "(" S_save_listener " filename) saves the current listener text in filename"
  FILE *fp = NULL;
  ASSERT_TYPE(STRING_P(filename), filename, SCM_ARGn, S_save_listener, "a string");
  fp = fopen(TO_C_STRING(filename), "w");
  if (fp) save_listener_text(fp);
  if ((!fp) || (fclose(fp) != 0))
    ERROR(CANNOT_SAVE,
	  SCM_LIST3(TO_SCM_STRING(S_save_listener),
		    filename,
		    TO_SCM_STRING(strerror(errno))));
  return(filename);
}

void g_init_listener(SCM local_doc)
{
  DEFINE_PROC(S_save_listener, g_save_listener, 1, 0, 0, H_save_listener);

  #define H_read_hook S_read_hook " (text) is called each time a line is typed into the listener (triggered by the carriage return). \
If it returns #t, Snd assumes you've dealt the text yourself, and does not try to evaluate it. \n\
(define (read-listener-line prompt) \n\
  (let ((res #f)) \n\
    (add-hook! read-hook (lambda (str) (set! res str) #t)) \n\
    (snd-print prompt) \n\
    (do () ((or (c-g?) res))) \n\
    (reset-hook! read-hook) \n\
    res))"

  read_hook = MAKE_HOOK(S_read_hook, 1, H_read_hook);
}
