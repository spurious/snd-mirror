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
#define GUI_TEXT_END(w) SG_TEXT_LENGTH(w)
#define GUI_TEXT_POSITION_TYPE gint
#define GUI_TEXT(w) SG_TEXT_CHARS(w, 0, -1)
#define GUI_SET_TEXT(w, text) SG_TEXT_INSERT(w, (ss->sgx)->help_text_fnt, (ss->sgx)->black, (ss->sgx)->white, text, -1)
#define GUI_TEXT_INSERTION_POSITION(w) SG_TEXT_GET_POINT(w)
#define GUI_TEXT_SET_INSERTION_POSITION(w, pos) SG_TEXT_SET_POINT(w, pos - 1)
#define GUI_LISTENER_TEXT_INSERT(w, pos, text) append_listener_text(0, text)
#define GUI_FREE(w) g_free(w)
#define GUI_SET_CURSOR(w, cursor) gdk_window_set_cursor(w->window, cursor)
#define GUI_UNSET_CURSOR(w, cursor) gdk_window_set_cursor(w->window, cursor)
#define GUI_UPDATE(w) 
#define GUI_TEXT_GOTO(w, pos) SG_TEXT_SET_POINT(w, pos)
#else
#define GUI_TEXT_END(w) XmTextGetLastPosition(w)
#define GUI_TEXT_POSITION_TYPE XmTextPosition
#define GUI_TEXT(w) XmTextGetString(w)
#define GUI_SET_TEXT(w, text) XmTextSetString(w, text)
#define GUI_TEXT_INSERTION_POSITION(w) XmTextGetInsertionPosition(w)
#define GUI_TEXT_SET_INSERTION_POSITION(w, pos) XmTextSetInsertionPosition(w, pos)
#define GUI_LISTENER_TEXT_INSERT(w, pos, text) XmTextInsert(w, pos, text)
#define GUI_FREE(w) XtFree(w)
#define GUI_SET_CURSOR(w, cursor) XUndefineCursor(XtDisplay(w), XtWindow(w)); XDefineCursor(XtDisplay(w), XtWindow(w), cursor)
#define GUI_UNSET_CURSOR(w, cursor) XUndefineCursor(XtDisplay(w), XtWindow(w)); XDefineCursor(XtDisplay(w), XtWindow(w), None)
#define GUI_UPDATE(w) XmUpdateDisplay(w)
#define GUI_TEXT_GOTO(w, pos) XmTextShowPosition(w, pos)
#endif
#endif

static XEN read_hook;

void command_return(GUI_WIDGET w, snd_state *ss, int last_prompt)
{
#if (!USE_NO_GUI)
  /* try to find complete form either enclosing current cursor, or just before it */
  GUI_TEXT_POSITION_TYPE new_eot = 0, cmd_eot = 0;
  char *str = NULL, *full_str = NULL, *prompt;
  int i, j, slen;
  XEN form = XEN_UNDEFINED;
  int end_of_text, start_of_text, last_position, current_position, parens;
  full_str = GUI_TEXT(w);
  current_position = GUI_TEXT_INSERTION_POSITION(w);
  start_of_text = current_position;
  end_of_text = current_position;
  last_position = GUI_TEXT_END(w);
  
  if (XEN_HOOKED(read_hook))
    {
      XEN result;
      str = (char *)CALLOC(last_position - last_prompt + 1, sizeof(char));
      for (i = last_prompt + 1, j = 0; i < last_position; i++, j++) str[j] = full_str[i];
      result = g_c_run_or_hook(read_hook, 
			       XEN_LIST_1(C_TO_XEN_STRING(str)),
			       S_read_hook);
      FREE(str);
      if (XEN_TRUE_P(result)) return;
    }

  prompt = listener_prompt(ss);
  /* first look for a form just before the current mouse location,
   *   independent of everything (i.e. user may have made changes
   *   in a form included in a comment, then typed return, expecting
   *   us to use the new version, but the check_balance procedure
   *   tries to ignore comments).
   */
  str = NULL;
#if HAVE_RUBY
  {
    int k, len, start;
    for (i = current_position - 1; i >= 0; i--)
      if (((full_str[i] == '\n') &&
	   (full_str[i + 1] == prompt[0])) ||
	  (i == 0))
	{
	  for (k = current_position - 1; k < strlen(full_str); k++)
	    if (full_str[k] == '\n')
	      break;
	  if (i == 0) start = 1; else start = i + 2;
	  len = (k - start + 1);
	  str = (char *)CALLOC(len, sizeof(char));
	  for (k = 0; k < len - 1; k++)
	    str[k] = full_str[k + start];
	  /* fprintf(stderr,"str: [%s]\n", str); */
          break; 
	}
  }
#else
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
#endif
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
       */
#if HAVE_GUILE
      if ((snd_strlen(str) > 1) || (str[0] != '\n'))
	form = string_to_form(str);
#else
      form = XEN_EVAL_C_STRING(str);
#endif
      FREE(str);
      str = NULL;
      if (XEN_BOUND_P(form))
	snd_report_listener_result(ss, form);
      GUI_UNSET_CURSOR(w, (ss->sgx)->arrow_cursor);
    }
  else
    {
      new_eot = GUI_TEXT_END(w);
      GUI_LISTENER_TEXT_INSERT(w, new_eot, listener_prompt_with_cr(ss));
      /* last_prompt = GUI_TEXT_END(w) - 1; */
    }
  cmd_eot = GUI_TEXT_END(w);
  GUI_TEXT_GOTO(w, cmd_eot - 1);
  GUI_TEXT_SET_INSERTION_POSITION(w, cmd_eot + 1);
#endif
}

static XEN g_save_listener(XEN filename)
{
  #define H_save_listener "(" S_save_listener " filename) saves the current listener text in filename"
  FILE *fp = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ONLY_ARG, S_save_listener, "a string");
  fp = fopen(XEN_TO_C_STRING(filename), "w");
  if (fp) save_listener_text(fp);
  if ((!fp) || (fclose(fp) != 0))
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_save_listener),
			 filename,
			 C_TO_XEN_STRING(strerror(errno))));
  return(filename);
}

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_1(g_save_listener_w, g_save_listener)
#else
#define g_save_listener_w g_save_listener
#endif

void g_init_listener(void)
{
  XEN_DEFINE_PROCEDURE(S_save_listener, g_save_listener_w, 1, 0, 0, H_save_listener);

  #define H_read_hook S_read_hook " (text) is called each time a line is typed into the listener (triggered by the carriage return). \
If it returns #t, Snd assumes you've dealt the text yourself, and does not try to evaluate it. \n\
(define (read-listener-line prompt) \n\
  (let ((res #f)) \n\
    (add-hook! read-hook (lambda (str) (set! res str) #t)) \n\
    (snd-print prompt) \n\
    (do () ((or (c-g?) res))) \n\
    (reset-hook! read-hook) \n\
    res))"
  
  XEN_DEFINE_HOOK(read_hook, S_read_hook, 1, H_read_hook);
}
