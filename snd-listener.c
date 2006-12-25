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

bool is_prompt(const char *str, int beg)
{
  int i, j;
  for (i = beg, j = ss->listener_prompt_length - 1; (i >= 0) && (j >= 0); i--, j--)
    if (str[i] != ss->Listener_Prompt[j])
      return(false);
  if (j != -1) return(false);
  if ((i == -1) || (str[i] == '\n'))
    return(true);
  return(false);
}

bool within_prompt(const char *str, int beg, int end)
{
  /* search backwards up to prompt length for cr (or 0), check for prompt */
  int i, lim;
  if ((beg + 1 == end) && (str[beg] == '\n')) return(false); /* end-of-page cr within double quotes probably */
  lim = beg - ss->listener_prompt_length;
  if (lim < 0) return(true);
  for (i = beg; i >= lim; i--)
    if ((str[i] == '\n') || (i == 0))
      {
	int j, k;
	for (j = 0, k = i + 1; (j < ss->listener_prompt_length) && (k < end); j++, k++)
	  if (str[k] != ss->Listener_Prompt[j])
	    return(false);
	return(true);
      }
  return(false);
}

int find_matching_paren(char *str, int parens, int pos, int *highlight_pos)
{
  int i, j;
  bool quoting = false;
  int up_comment = -1;
  for (i = pos - 1; i > 0;)
    {
      if (is_prompt(str, i))
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
		  bool up_quoting = false;
		  int quote_comment = -1;
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

int check_balance(char *expr, int start, int end, bool in_listener) 
{
  int i;
  bool non_whitespace_p = false;
  int paren_count = 0;
  bool prev_separator = true;
  bool quote_wait = false;
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
	      prev_separator = true;
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
		  prev_separator = true;
		  non_whitespace_p = true;
		  quote_wait = false;
		}
	    }
	  break;
	case '\\': 
	  /* this is an error of some sort */
	  i += 2; 
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
		      prev_separator = false;
		      quote_wait = false;
		      non_whitespace_p = true;
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
	      non_whitespace_p = true;
	      prev_separator = true;
	      quote_wait = false;
	    }
	  break;
	case ')' :
	  paren_count--;
	  if ((non_whitespace_p) && (paren_count == 0))
	    return(i + 1);
	  else 
	    {
	      i++;
	      non_whitespace_p = true;
	      prev_separator = true;
	      quote_wait = false;
	    }
	  break;
	case '\'' :
	  if (prev_separator) 
	    quote_wait = true;
	  non_whitespace_p = true;
	  i++;
	  break;
	default:
	  prev_separator = false;
	  quote_wait = false;
	  non_whitespace_p = true;
	  i++;
	  break;
	}
    }
  if ((in_listener) && (!(highlight_unbalanced_paren()))) return(-1);
  return(0);
}

static char listener_prompt_buffer[LABEL_BUFFER_SIZE];
char *listener_prompt_with_cr(void)
{
  mus_snprintf(listener_prompt_buffer, LABEL_BUFFER_SIZE, "\n%s", listener_prompt(ss));
  return(listener_prompt_buffer);
}

static void provide_listener_help_1(char *source, int start, int end)
{
  XEN result = XEN_FALSE;
  char *name;
  name = (char *)CALLOC(end - start + 1, sizeof(char));
  strncpy(name, (char *)(source + start), end - start);
  result = g_snd_help(C_TO_XEN_STRING(name), listener_width());
  FREE(name);
  if (XEN_STRING_P(result))
    {
      listener_append("\n;");
      listener_append_and_prompt(XEN_TO_C_STRING(result));
    }
}

void provide_listener_help(char *source)
{
  if (source)
    {
      int i, len;
      char *prompt;
      len = snd_strlen(source);
      /* look for "(name...)" or "\n>name" */
      prompt = listener_prompt(ss);
      for (i = len - 1; i >= 0; i--)
	{
	  if ((source[i] == '(') || 
	      (is_prompt(source, i)))
	    {
	      int j, start_of_name = -1;
	      start_of_name = i + 1;
	      /* look forward for a name */
	      for (j = i + 2; j < len; j++)
		if (separator_char_p(source[j]))
		  {
		    provide_listener_help_1(source, start_of_name, j);
		    return;
		  }
	      provide_listener_help_1(source, start_of_name, len); /* ran off end with no separator (cursor is at end) */
	      return;
	    }
	}
    }
}

bool listener_is_visible(void)
{
  return(listener_height() > 5);
}

#if (!USE_NO_GUI)
#if USE_GTK
#define GUI_TEXT_END(w) gtk_text_buffer_get_char_count(gtk_text_view_get_buffer(GTK_TEXT_VIEW(w)))
#define GUI_TEXT_POSITION_TYPE gint
#define GUI_TEXT(w) sg_get_text(w, 0, -1)
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

#if (!USE_NO_GUI)
static int current_listener_position = -1, listener_positions_size = 0;
static int *listener_positions = NULL;

static void add_listener_position(int pos)
{
  if (listener_positions_size == 0)
    {
      listener_positions_size = 32;
      listener_positions = (int *)CALLOC(listener_positions_size, sizeof(int));
      current_listener_position = 0;
    }
  else
    {
      int i;
      if (pos > listener_positions[current_listener_position])
	{
	  current_listener_position++;
	  if (current_listener_position >= listener_positions_size)
	    {
	      listener_positions_size += 32;
	      listener_positions = (int *)REALLOC(listener_positions, listener_positions_size * sizeof(int));
	      for (i = current_listener_position + 1; i < listener_positions_size; i++) listener_positions[i] = 0;
	    }
	}
      else
	{
	  for (i = current_listener_position - 1; i >= 0; i--)
	    if (listener_positions[i] < pos)
	      break;
	  current_listener_position = i + 1;
	}
    }
  listener_positions[current_listener_position] = pos;
}

void backup_listener_to_previous_command(void)
{
  if (current_listener_position > 0)
    {
      current_listener_position--;
      listener_delete_text(listener_positions[current_listener_position]);
    }
}
#else
void backup_listener_to_previous_command(void) {}
#endif

void command_return(widget_t w, int last_prompt)
{
#if (!USE_NO_GUI)
  /* try to find complete form either enclosing current cursor, or just before it */
  GUI_TEXT_POSITION_TYPE new_eot = 0, cmd_eot = 0;
  char *str = NULL, *full_str = NULL, *prompt;
  int i, j;
  XEN form = XEN_UNDEFINED;
  GUI_TEXT_POSITION_TYPE end_of_text = 0, start_of_text = 0, last_position = 0, current_position = 0;
#if (!HAVE_RUBY && !HAVE_FORTH)
  int parens;
#endif
  full_str = GUI_TEXT(w);
  current_position = GUI_TEXT_INSERTION_POSITION(w);
  start_of_text = current_position;
  end_of_text = current_position;
  last_position = GUI_TEXT_END(w);
  add_listener_position(last_position);
  if (XEN_HOOKED(read_hook))
    {
      XEN result;
      int len;
      len = last_position - last_prompt + 1;
      if (len > 0)
	{
	  str = (char *)CALLOC(len, sizeof(char));
	  for (i = last_prompt + 1, j = 0; i < last_position; i++, j++) str[j] = full_str[i];
	  result = run_or_hook(read_hook, 
			       XEN_LIST_1(C_TO_XEN_STRING(str)),
			       S_read_hook);
	  FREE(str);
	  if (XEN_TRUE_P(result)) 
	    {
	      if (full_str) GUI_FREE(full_str);
	      return;
	    }
	}
    }
  prompt = listener_prompt(ss);
  /* first look for a form just before the current mouse location,
   *   independent of everything (i.e. user may have made changes
   *   in a form included in a comment, then typed return, expecting
   *   us to use the new version, but the check_balance procedure
   *   tries to ignore comments).
   */
  str = NULL;
#if HAVE_RUBY || HAVE_FORTH
  {
    int k, len, start, full_len;
    for (i = current_position - 1; i >= 0; i--)
      if (is_prompt(full_str, i))
	{
	  full_len = strlen(full_str);
	  for (k = current_position - 1; k < full_len; k++)
	    if (full_str[k] == '\n')
	      break;
	  start = i + 1;
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
	    if (is_prompt(full_str, i + 1))
	      {
		end_of_text = i - ss->listener_prompt_length + 1;
		break;
	      }
	}
      if (start_of_text > 0)
	{
	  for (i = end_of_text; i >= 0; i--)
	    if (is_prompt(full_str, i))
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
	      end_of_text = check_balance(str, 0, (int)end_of_text, true); /* last-arg->we are in the listener */
	      if ((end_of_text > 0) && 
		  (end_of_text < slen))
		{
		  if (end_of_text < (slen - 1))
		    str[end_of_text + 1] = 0;
		  else str[end_of_text] = 0;
		  if (str[end_of_text] == '\n') str[end_of_text] = 0;
		}
	      else
		{
		  FREE(str);
		  str = NULL;
		  new_eot = GUI_TEXT_END(w);
		  if (end_of_text < 0)
		    GUI_LISTENER_TEXT_INSERT(w, new_eot, listener_prompt_with_cr());
		  else GUI_LISTENER_TEXT_INSERT(w, new_eot, "\n");
		  if (full_str) GUI_FREE(full_str);
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
      GUI_SET_CURSOR(w, ss->sgx->wait_cursor);
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
#if HAVE_GAUCHE
      if ((snd_strlen(str) > 1) || (str[0] != '\n'))
	form = xen_gauche_eval_c_string(str);
#else
      form = XEN_EVAL_C_STRING(str);
#endif
#endif
      FREE(str);
      str = NULL;
      snd_report_listener_result(form); /* used to check for unbound form here, but that's no good in Ruby,
					 *   and doesn't seem sensible in Guile
					 */
      GUI_UNSET_CURSOR(w, ss->sgx->arrow_cursor);
    }
  else
    {
      new_eot = GUI_TEXT_END(w);
      GUI_LISTENER_TEXT_INSERT(w, new_eot, listener_prompt_with_cr());
    }
  cmd_eot = GUI_TEXT_END(w);
  add_listener_position(cmd_eot);
  GUI_TEXT_GOTO(w, cmd_eot - 1);
  GUI_TEXT_SET_INSERTION_POSITION(w, cmd_eot + 1);
#endif
}

static XEN g_save_listener(XEN filename)
{
  #define H_save_listener "(" S_save_listener " filename): saves the current listener text in filename"
  FILE *fp = NULL;
  char *name;
  int err = 0;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ONLY_ARG, S_save_listener, "a string");
  name = XEN_TO_C_STRING(filename);
  fp = FOPEN(name, "w");
  if (fp) 
    {
      err = save_listener_text(fp);
      snd_fclose(fp, name);
    }
  if ((!fp) || (err == -1))
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_save_listener),
			 filename,
			 C_TO_XEN_STRING(snd_io_strerror())));
  return(filename);
}

static XEN g_clear_listener(void)
{
  #define H_clear_listener "(" S_clear_listener "): removes listener text from the beginning to the cursor"
  clear_listener();
  return(XEN_FALSE);
}

static XEN g_show_listener(XEN val) 
{
  #define H_show_listener "(" S_show_listener " :optional (open " PROC_TRUE ")): if 'open' opens the lisp listener; returns whether the listener is visible."
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, S_show_listener, "a boolean");
  if ((XEN_NOT_BOUND_P(val)) || (!(XEN_FALSE_P(val)))) /* explicit #f arg turns off listener creation/display */
    handle_listener(true); 
  return(C_TO_XEN_BOOLEAN(listener_is_visible()));
}

static XEN g_set_show_listener(XEN val)
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_show_listener, "a boolean");
  handle_listener(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(listener_is_visible()));
}

void set_listener_prompt(const char *new_prompt)
{
  in_set_listener_prompt((char *)new_prompt);
  ss->listener_prompt_length = snd_strlen(new_prompt);

#if USE_NO_GUI
  {
#if HAVE_GAUCHE
    char *str;
    XEN_EVAL_C_STRING("(if (not (defined? 'gauche-repl-prompt)) (define gauche-repl-prompt \">\"))");
    str = mus_format("(set! gauche-repl-prompt \"%s\")", listener_prompt(ss)); /* defined in xen.c */
    XEN_EVAL_C_STRING(str);
    FREE(str);
#endif

#if HAVE_FORTH
    char *str;
    XEN_EVAL_C_STRING("before-prompt-hook reset-hook!\n");
    str = mus_format("before-prompt-hook lambda: <{ prompt pos }> \"%s\" ; add-hook!", listener_prompt(ss));
    XEN_EVAL_C_STRING(str);
    FREE(str);
#endif

#if HAVE_GUILE
    char *str;
    str = mus_format("(set! scm-repl-prompt \"%s\")", listener_prompt(ss)); /* defined in ice-9/boot9.scm */
    XEN_EVAL_C_STRING(str);
    FREE(str);
#endif

#if HAVE_RUBY
    xen_rb_repl_set_prompt(listener_prompt(ss));
#endif
  }

#else
  /* not USE_NO_GUI */

  /* here if the prompt changes and the listener exists, we need to make sure
   *   we output a new prompt; otherwise the expression finder gets confused
   *   by the old prompt.
   */
  listener_append_and_prompt(NULL); /* this checks first that the listener exists */
  
#endif
  
}

static XEN g_listener_prompt(void) {return(C_TO_XEN_STRING(listener_prompt(ss)));}
static XEN g_set_listener_prompt(XEN val) 
{
  #define H_listener_prompt "(" S_listener_prompt "): the current lisp listener prompt character ('>') "
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_listener_prompt, "a string"); 
  if (listener_prompt(ss)) FREE(listener_prompt(ss));
  set_listener_prompt(copy_string(XEN_TO_C_STRING(val)));
  return(C_TO_XEN_STRING(listener_prompt(ss)));
}

static XEN g_snd_completion(XEN text)
{
  /* perhaps callable from emacs? */
  char *str, *temp;
  XEN res;
  temp = copy_string(XEN_TO_C_STRING(text));
  str = command_completer(temp, NULL);
  res = C_TO_XEN_STRING(str);
  FREE(str);
  FREE(temp);
  return(res);
}


#ifdef XEN_ARGIFY_1
XEN_NARGIFY_1(g_save_listener_w, g_save_listener)
XEN_NARGIFY_0(g_clear_listener_w, g_clear_listener);
XEN_ARGIFY_1(g_show_listener_w, g_show_listener)
XEN_NARGIFY_1(g_set_show_listener_w, g_set_show_listener)
XEN_NARGIFY_0(g_listener_prompt_w, g_listener_prompt)
XEN_NARGIFY_1(g_set_listener_prompt_w, g_set_listener_prompt)
XEN_NARGIFY_1(g_snd_completion_w, g_snd_completion)
#else
#define g_save_listener_w g_save_listener
#define g_clear_listener_w g_clear_listener
#define g_show_listener_w g_show_listener
#define g_set_show_listener_w g_set_show_listener
#define g_listener_prompt_w g_listener_prompt
#define g_set_listener_prompt_w g_set_listener_prompt
#define g_snd_completion_w g_snd_completion
#endif

void g_init_listener(void)
{
  XEN_DEFINE_PROCEDURE(S_save_listener,  g_save_listener_w,  1, 0, 0, H_save_listener);
  XEN_DEFINE_PROCEDURE(S_clear_listener, g_clear_listener_w, 0, 0, 0, H_clear_listener);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_show_listener, g_show_listener_w, H_show_listener,
				   S_setB S_show_listener, g_set_show_listener_w,  0, 1, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_listener_prompt, g_listener_prompt_w, H_listener_prompt,
				   S_setB S_listener_prompt, g_set_listener_prompt_w,  0, 0, 1, 0);

#if HAVE_SCHEME
  #define H_read_hook S_read_hook " (text): called each time a line is typed into the listener (triggered by the carriage return). \
If it returns " PROC_TRUE ", Snd assumes you've dealt the text yourself, and does not try to evaluate it. \n\
(define (read-listener-line prompt) \n\
  (let ((res #f)) \n\
    (add-hook! " S_read_hook " (lambda (str) (set! res str) #t)) \n\
    (" S_snd_print " prompt) \n\
    (do () ((or (" S_c_g ") res))) \n\
    (reset-hook! " S_read_hook ") \n\
    res))"
#endif
#if HAVE_RUBY || HAVE_FORTH
  #define H_read_hook S_read_hook " (text): called each time a line is typed into the listener (triggered by the carriage return). \
If it returns true, Snd assumes you've dealt the text yourself, and does not try to evaluate it."
#endif
  
  read_hook = XEN_DEFINE_HOOK(S_read_hook, 1, H_read_hook);

  XEN_DEFINE_PROCEDURE("snd-completion",        g_snd_completion_w,        1, 0, 0, "return completion of arg");
}
