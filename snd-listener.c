#include "snd.h"

/* check_balance originally from scwm-0.9/utilities/scwmrepl/scwmrepl.c, 
 *   revised by bil 9-Oct-02:
 *   original did not handle #\) (and many others) correctly or #()
 *            had support for #{} which is (apparently) a Guile documentation kludge
 *            mishandled \" in a quoted string
 *            and more changes followed
 */

/*
(+ 1 ;a comment
   (char->integer #\a) 
   (char->integer (string-ref "01\"" 2)) 
   (char->integer (string-ref "01\";#" 2)) 
   (vector-ref #(1 2) 0)
  (char->integer #\))
  )
*/

int find_matching_paren(char *str, int parens, int pos, char *prompt, int *highlight_pos)
{
  int i, j, quoting = FALSE, up_comment = -1;
  for (i = pos - 1; i > 0;)
    {
      if ((i > 0) && (str[i] == prompt[0]) && (str[i - 1] == '\n'))
	break;
      if ((str[i] == '\"') && (str[i - 1] != '\\'))
	quoting = !quoting;
      if (!quoting)
	{
	  if ((i <= 1) || (str[i - 1] != '\\') || (str[i - 2] != '#'))
	    {
	      if (str[i] == ')') parens++; else
	      if (str[i] == '(') parens--; else
	      if (str[i] == '\n')
		{
		  /* check for comment on next line up */
		  int up_quoting = FALSE, quote_comment = -1;
		  for (j = i - 1; j > 0; j--)
		    {
		      if (str[j] == '\n') break;
		      if ((str[j] == '\"') && (str[j - 1] != '\\'))
			up_quoting = !up_quoting;
		      if ((str[j] == ';') &&
			  ((j <= 1) || (str[j - 1] != '\\') || (str[j - 2] != '#')))
			{
			  if (!up_quoting)
			    up_comment = j;
			  else quote_comment = j;
			}
		    }
		  if (up_comment < i)
		    {
		      if (up_comment > 0)
			i = up_comment;
		      else
			{
			  if ((up_quoting) && (quote_comment > 0))
			    i = quote_comment;
			}
		    }
		}
	    }
	}
      if (parens == 0)
	{
	  (*highlight_pos) = i;
	  break;
	}
      i--;
    }
  return(parens);
}

int check_balance(char *expr, int start, int end, int in_listener) 
{
  int i;
  int non_whitespace_p = FALSE;
  int paren_count = 0;
  int prev_separator = TRUE;
  int quote_wait = FALSE;
  i = start;
  while (i < end) 
    {
      switch (expr[i]) 
	{
	case ';' :
	  /* skip till newline. */
	  do {
	    i++;
	  } while ((expr[i] != '\n') && (i < end));
	  break;
	case ' ':
	case '\n':
	case '\t':
	case '\r':
	  if ((non_whitespace_p) && (paren_count == 0) && (!quote_wait))
	    return(i);
	  else 
	    {
	      prev_separator = TRUE;
	      i++;
	    }
	  break;
	case '\"' :
	  if ((non_whitespace_p) && (paren_count == 0) && (!quote_wait))
	    return(i);
	  else 
	    {
	      /* skip past ", ignoring \" */
	      do {
		i++;
		if ((i < (end - 1)) && (expr[i] =='\\') && (expr[i + 1] == '\"'))
		  i += 2;
	      } while ((i < end) && (expr[i] != '\"'));
	      i++;
	      if (paren_count == 0) 
		{
		  if (i < end) 
		    return(i);
		  else return(0);
		} 
	      else 
		{
		  prev_separator = TRUE;
		  non_whitespace_p = TRUE;
		  quote_wait = FALSE;
		}
	    }
	  break;
	case '#' :
	  if ((non_whitespace_p) && (paren_count == 0) && (!quote_wait))
	    return(i);
	  else 
	    {
	      if ((prev_separator) && (i + 1 < end) && (expr[i + 1] =='('))
		i++;
	      else
		{
		  if ((i + 2 < end) && (expr[i + 1] == '\\') && 
		      ((expr[i + 2] == ')') || (expr[i + 2] == ';') || (expr[i + 2] == '\"') || (expr[i + 2] == '(')))
		    i += 3;
		  else
		    {
		      prev_separator = FALSE;
		      quote_wait = FALSE;
		      non_whitespace_p = TRUE;
		      i++;
		    }
		}
	    }
	  break;
	case '(' :
	  if ((non_whitespace_p) && (paren_count == 0) && (!quote_wait))
	    return(i);
	  else 
	    {
	      i++;
	      paren_count++;
	      non_whitespace_p = TRUE;
	      prev_separator = TRUE;
	      quote_wait = FALSE;
	    }
	  break;
	case ')' :
	  paren_count--;
	  if ((non_whitespace_p) && (paren_count == 0))
	    return(i + 1);
	  else 
	    {
	      i++;
	      non_whitespace_p = TRUE;
	      prev_separator = TRUE;
	      quote_wait = FALSE;
	    }
	  break;
	case '\'' :
	  if (prev_separator) 
	    quote_wait = TRUE;
	  non_whitespace_p = TRUE;
	  i++;
	  break;
	default:
	  prev_separator = FALSE;
	  quote_wait = FALSE;
	  non_whitespace_p = TRUE;
	  i++;
	  break;
	}
    }
  if ((in_listener) && (!(highlight_unbalanced_paren()))) return(-1);
  return(0);
}

static char listener_prompt_buffer[LABEL_BUFFER_SIZE];
char *listener_prompt_with_cr(snd_state *ss)
{
  mus_snprintf(listener_prompt_buffer, LABEL_BUFFER_SIZE, "\n%s", listener_prompt(ss));
  return(listener_prompt_buffer);
}

static XEN provide_listener_help_1(char *source, int start, int end)
{
  XEN result = XEN_FALSE;
  char *name;
  name = (char *)CALLOC(end - start + 1, sizeof(char));
  strncpy(name, (char *)(source + start), end - start);
  /* fprintf(stderr,"%s[%d:%d] -> %s\n", source, start, end, name); */
  result = g_snd_help(C_TO_XEN_STRING(name), listener_width());
  FREE(name);
  if (XEN_STRING_P(result))
    {
      listener_append("\n;");
      listener_append_and_prompt(XEN_TO_C_STRING(result));
    }
  return(result);
}

XEN provide_listener_help(char *source)
{
  int i, len, j, start_of_name = -1;
  snd_state *ss;
  char *prompt;
  if (source)
    {
      len = snd_strlen(source);
      /* look for "(name...)" or "\n>name" */
      ss = get_global_state();
      prompt = listener_prompt(ss);
      for (i = len - 1; i >= 0; i--)
	{
	  if ((source[i] == '(') || 
	      ((source[i] == prompt[0]) && ((i == 0) || (source[i - 1] == '\n'))))
	    {
	      start_of_name = i + 1;
	      /* look forward for a name */
	      for (j = i + 2; j < len; j++)
		if (is_separator_char(source[j]))
		  return(provide_listener_help_1(source, start_of_name, j));
	      return(provide_listener_help_1(source, start_of_name, len)); /* ran off end with no separator (cursor is at end) */
	    }
	}
    }
  return(XEN_FALSE);
}

#if (!USE_NO_GUI)
#if USE_GTK
#define GUI_TEXT_END(w) gtk_text_buffer_get_char_count(gtk_text_view_get_buffer(GTK_TEXT_VIEW(w)))
#define GUI_TEXT_POSITION_TYPE gint
#define GUI_TEXT(w) sg_get_text(w, 0, -1)
#define GUI_SET_TEXT(w, text) sg_text_insert(w, text)
#define GUI_TEXT_INSERTION_POSITION(w) sg_cursor_position(w)
#define GUI_TEXT_SET_INSERTION_POSITION(w, pos) sg_set_cursor(w, pos)
#define GUI_LISTENER_TEXT_INSERT(w, pos, text) append_listener_text(0, text)
#define GUI_FREE(w) g_free(w)
#define GUI_SET_CURSOR(w, cursor) gdk_window_set_cursor(w->window, cursor)
#define GUI_UNSET_CURSOR(w, cursor) gdk_window_set_cursor(w->window, cursor)
#define GUI_UPDATE(w) 
#define GUI_TEXT_GOTO(w, pos) sg_set_cursor(w, pos + 1)
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

void command_return(widget_t w, snd_state *ss, int last_prompt)
{
#if (!USE_NO_GUI)
  /* try to find complete form either enclosing current cursor, or just before it */
  GUI_TEXT_POSITION_TYPE new_eot = 0, cmd_eot = 0;
  char *str = NULL, *full_str = NULL, *prompt;
  int i, j;
  XEN form = XEN_UNDEFINED;
  GUI_TEXT_POSITION_TYPE end_of_text = 0, start_of_text = 0, last_position = 0, current_position = 0;
#if (!HAVE_RUBY)
  int parens;
#endif
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
      result = run_or_hook(read_hook, 
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
	  int slen;
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
	  if (parens)
	    {
	      end_of_text = check_balance(str, 0, (int)end_of_text, TRUE); /* last-arg->we are in the listener */
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
		  if (end_of_text < 0)
		    GUI_LISTENER_TEXT_INSERT(w, new_eot, listener_prompt_with_cr(ss));
		  else GUI_LISTENER_TEXT_INSERT(w, new_eot, "\n");
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
      if ((snd_strlen(str) > 1) || (str[0] != '\n'))
	remember_listener_string(str);
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
      snd_report_listener_result(form); /* used to check for unbound form here, but that's no good in Ruby,
					 *   and doesn't seem sensible in Guile
					 */
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
  #define H_save_listener "(" S_save_listener " filename): saves the current listener text in filename"
  FILE *fp = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ONLY_ARG, S_save_listener, "a string");
  fp = FOPEN(XEN_TO_C_STRING(filename), "w");
  if (fp) save_listener_text(fp);
  if ((!fp) || (FCLOSE(fp) != 0))
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_save_listener),
			 filename,
			 C_TO_XEN_STRING(strerror(errno))));
  return(filename);
}

static XEN g_clear_listener(void)
{
  #define H_clear_listener "(" S_clear_listener "): removes listener text from the beginning to the cursor"
  clear_listener();
  return(XEN_FALSE);
}

static XEN g_show_listener(void) 
{
  #define H_show_listener "(" S_show_listener "): opens the lisp listener pane"
  snd_state *ss;
  ss = get_global_state();
  handle_listener(ss, TRUE); 
  return(C_TO_XEN_BOOLEAN(listener_height() > 5));
}

static XEN g_set_show_listener(XEN val)
{
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_show_listener, "a boolean");
  handle_listener(ss, XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(listener_height() > 5));
}

static XEN g_listener_prompt(void) {return(C_TO_XEN_STRING(listener_prompt(get_global_state())));}
static XEN g_set_listener_prompt(XEN val) 
{
  #define H_listener_prompt "(" S_listener_prompt "): the current lisp listener prompt character ('>') "
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_listener_prompt, "a string"); 
  if (listener_prompt(ss)) FREE(listener_prompt(ss));
  set_listener_prompt(ss, copy_string(XEN_TO_C_STRING(val)));
#if USE_NO_GUI
  {
#if HAVE_GUILE
    char *str;
    str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
    mus_snprintf(str, PRINT_BUFFER_SIZE, "(set! scm-repl-prompt \"%s\")", listener_prompt(ss));
    XEN_EVAL_C_STRING(str);
    FREE(str);
#endif
  }
#endif
  return(C_TO_XEN_STRING(listener_prompt(ss)));
}


#ifdef XEN_ARGIFY_1
XEN_NARGIFY_1(g_save_listener_w, g_save_listener)
XEN_NARGIFY_0(g_clear_listener_w, g_clear_listener);
XEN_NARGIFY_0(g_show_listener_w, g_show_listener)
XEN_NARGIFY_1(g_set_show_listener_w, g_set_show_listener)
XEN_NARGIFY_0(g_listener_prompt_w, g_listener_prompt)
XEN_NARGIFY_1(g_set_listener_prompt_w, g_set_listener_prompt)
#else
#define g_save_listener_w g_save_listener
#define g_clear_listener_w g_clear_listener
#define g_show_listener_w g_show_listener
#define g_set_show_listener_w g_set_show_listener
#define g_listener_prompt_w g_listener_prompt
#define g_set_listener_prompt_w g_set_listener_prompt
#endif

void g_init_listener(void)
{
  XEN_DEFINE_PROCEDURE(S_save_listener, g_save_listener_w, 1, 0, 0, H_save_listener);
  XEN_DEFINE_PROCEDURE(S_clear_listener, g_clear_listener_w, 0, 0, 0, H_clear_listener);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_show_listener, g_show_listener_w, H_show_listener,
				   S_setB S_show_listener, g_set_show_listener_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_listener_prompt, g_listener_prompt_w, H_listener_prompt,
				   S_setB S_listener_prompt, g_set_listener_prompt_w,  0, 0, 1, 0);

  #define H_read_hook S_read_hook " (text): called each time a line is typed into the listener (triggered by the carriage return). \
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
