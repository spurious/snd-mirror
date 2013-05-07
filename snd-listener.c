#include "snd.h"

/* ---------------- listener text history ---------------- */

static char **listener_strings = NULL;
static int listener_strings_size = 0, listener_strings_pos = 0;
static bool listener_first_time = true;

#if (!USE_NO_GUI)
static void remember_listener_string(const char *str)
{
  int i, top;
  if (!str) return;

  if (listener_strings_size == 0)
    {
      listener_strings_size = 8;
      listener_strings = (char **)calloc(listener_strings_size, sizeof(char *));
    }
  
  listener_strings_pos = 0;
  listener_first_time = true;

  /* if str matches current history top entry, ignore it (as in tcsh) */
  if ((listener_strings[0]) &&
      (strcmp(str, listener_strings[0]) == 0))
    return;

  top = listener_strings_size - 1;
  if (listener_strings[top]) free(listener_strings[top]);
  for (i = top; i > 0; i--) listener_strings[i] = listener_strings[i - 1];

  listener_strings[0] = mus_strdup(str);
}
#endif


void restore_listener_string(bool back)
{
  if (listener_strings)
    {
      char *str;
      if (!(listener_first_time))
	{
	  if (back)
	    listener_strings_pos++;
	  else listener_strings_pos--;
	}
      listener_first_time = false;
      if (listener_strings_pos < 0) listener_strings_pos = 0;
      if (listener_strings_pos > (listener_strings_size - 1)) listener_strings_pos = listener_strings_size - 1;
      str = listener_strings[listener_strings_pos];
      if (str)
	append_listener_text(-1, str); 
    }
}




/* -------------------------------------------------------------------------------- */

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


void listener_help_at_cursor(char *buf, int name_curpos, int len, int prompt_pos)
{
  int i, name_start, name_end;

  if (isspace(buf[name_curpos]))
    {
      for (i = name_curpos - 1; i >= 0; i--)
	if ((!isspace(buf[i])) &&
	    (buf[i] != '(') &&
	    (buf[i] != ')'))
	  break;
      if (i > 0)
	name_curpos = i;
    }

  for (i = name_curpos; i >= 0; i--)
    if ((isspace(buf[i])) ||
	(buf[i] == '(') ||
	(buf[i] == ')'))
      break;
  name_start = i + 1;

  for (i = name_curpos + 1; i < len; i++)
    if ((isspace(buf[i])) ||
	(buf[i] == '(') ||
	(buf[i] == ')'))
      break;
  name_end = i - 1;

  if (name_end > name_start)
    {
      XEN help;
      char *new_text;

      buf[name_end + 1] = '\0';
      new_text = direct_completions((char *)(buf + name_start));

      if (new_text)
	{
	  int matches;
	  matches = get_possible_completions_size();

	  if (matches == 1)
	    {
	      help = g_snd_help(C_TO_XEN_STRING(new_text), 0);
	      if (XEN_STRING_P(help))
		snd_help((char *)(buf + name_start), XEN_TO_C_STRING(help), WITH_WORD_WRAP);
	    }
	  else
	    {
	      if (matches > 1)
		{
		  char **buffer;
		  char *help_str;
		  int match_len = 0;

		  buffer = get_possible_completions();
		  for (i = 0; i < matches; i++)
		    match_len += mus_strlen(buffer[i]);
		  
		  help_str = (char *)calloc(match_len + matches * 8, sizeof(char));
		  for (i = 0; i < matches; i++)
		    {
		      strcat(help_str, buffer[i]);
		      strcat(help_str, "\n");
		    }
		  snd_help((char *)(buf + name_start), help_str, WITHOUT_WORD_WRAP);
		  free(help_str);
		}
	    }

	  free(new_text);
	}
    }
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


char *trim(char *orig)
{
  int i, j, start = -1, end = -1, len;
  char *str;

  len = mus_strlen(orig);
  if (len == 0) return(NULL);

  for (start = 0; start < len; start++)
    if ((!isspace(orig[start])) &&
	(orig[start] != '(') &&
	(orig[start] != ')'))
      break;
  if (start >= len) return(NULL);
  for (end = len - 1; end > start; end--)
    if ((!isspace(orig[end])) &&
	(orig[end] != '(') &&
	(orig[end] != ')'))
      break;
  if (start == end) return(NULL);
  
  len = end - start + 1;
  str = (char *)calloc(len + 1, sizeof(char));
  for (i = start, j = 0; i <= end; i++, j++)
    str[j] = orig[i];
  return(str);
}


int find_matching_paren(const char *str, int parens, int pos, int *highlight_pos)
{
  int i, j;
  bool quoting = false;
  int up_comment = -1;
  for (i = pos - 1; i > 0;)
    {
      if (is_prompt(str, i))
	break;
      if (str[i] == '\"')
	{
	  /* there could be any number of slashified slashes before this quote. */
	  int k, slashes = 0;
	  for (k = i - 1; k >= 0; k--)
	    {
	      if (str[k] == '\\')
		slashes++;
	      else break;
	    }
	  if ((slashes & 1) == 0)
	    quoting = !quoting;
	}
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


int check_balance(const char *expr, int start, int end, bool in_listener) 
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
	      /* skip past ", ignoring \", some cases:
	       *  "\"\"" '("\"\"") "\\" "#\\(" "'(\"#\\\")"
	       */
	      while (i < end)
		{
		  i++;
		  if (expr[i] == '\\') 
		    i++;
		  else
		    {
		      if (expr[i] == '\"')
			break;
		    }
		}
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

	case '#':
	  if ((i < end - 1) &&
	      (expr[i + 1] == '|'))
	    {
	      /* (+ #| a comment |# 2 1) */
	      i++;
	      do {
		i++;
	      } while (((expr[i] != '|') || (expr[i + 1] != '#')) && (i < end));
	      i++;
	      break;
	    }
	  else
	    {
	      /* (set! *#readers* (cons (cons #\c (lambda (str) (apply make-rectangular (read)))) *#readers*))
	       */
	      if ((non_whitespace_p) && (paren_count == 0) && (!quote_wait))
		return(i);
	      else 
		{
		  bool found_it = false;
		  if (prev_separator)
		    {
		      int k, incr = 0;
		      for (k = i + 1; k < end; k++)
			{
			  if (expr[k] == '(')
			    {
			      /* should we look at the readers here? I want to support #c(1 2) for example */
			      non_whitespace_p = false;
			      prev_separator = false;
			      incr = k - i;
			      break;
			    }
			  else
			    {
			      if ((!isdigit(expr[k])) && /* #2d(...)? */
				  (!isalpha(expr[k])) && /* #c(1 2)? */
				  (expr[k] != 'D') && 
				  (expr[k] != 'd') &&
				  (expr[k] != '=') &&   /* what is this for? */
				  (expr[k] != '#'))     /* perhaps #1d(#(1 2) 3) ? */
				break;
			    }
			}
		      if (incr > 0)
			{
			  i += incr;
			  found_it = true;
			}
		    }
		  if (!found_it)
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
	    }
	  break;

	case '(' :
	  if ((non_whitespace_p) && (paren_count == 0) && (!quote_wait))
	    return(i - 1); /* 'a(...) -- ignore the (...) */
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
	case '`' :                  /* `(1 2) */
	  if (prev_separator) 
	    quote_wait = true;
	  non_whitespace_p = true;
	  i++;
	  break;

	case ',':                   /* `,(+ 1 2) */
	case '@':                   /* `,@(list 1 2) */
	  prev_separator = false;
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

  if ((in_listener) && (!(highlight_unbalanced_paren()))) 
    return(-1);
  return(0);
}


static char listener_prompt_buffer[LABEL_BUFFER_SIZE];

char *listener_prompt_with_cr(void)
{
  mus_snprintf(listener_prompt_buffer, LABEL_BUFFER_SIZE, "\n%s", listener_prompt(ss));
  return(listener_prompt_buffer);
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
#define GUI_SET_CURSOR(w, cursor) gdk_window_set_cursor(gtk_text_view_get_window(GTK_TEXT_VIEW(w), GTK_TEXT_WINDOW_TEXT), cursor)
#define GUI_UNSET_CURSOR(w, cursor) gdk_window_set_cursor(gtk_text_view_get_window(GTK_TEXT_VIEW(w), GTK_TEXT_WINDOW_TEXT), cursor)
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
      listener_positions = (int *)calloc(listener_positions_size, sizeof(int));
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
	      listener_positions = (int *)realloc(listener_positions, listener_positions_size * sizeof(int));
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


void backup_listener_to_previous_expression(void)
{
  if (current_listener_position > 0)
    {
      current_listener_position--;
      listener_delete_text(listener_positions[current_listener_position]);
    }
}
#else
void backup_listener_to_previous_expression(void) {}
#endif



#if HAVE_SCHEME && (!USE_NO_GUI)

static int skipped_calls = 0;
#define SKIPPING 10

static bool listener_begin_hook(s7_scheme *sc)
{
  if (skipped_calls < SKIPPING)
    {
      skipped_calls++;
      return(false);
    }
  skipped_calls = 0;
  ss->C_g_typed = false;

#if USE_MOTIF
  if (XtAppPending(MAIN_APP(ss)) & XtIMXEvent)
    {
      XEvent event;
      XtAppNextEvent(MAIN_APP(ss), &event);
      XtDispatchEvent(&event);
    }
#endif

#if USE_GTK
#if 0
  if (gdk_events_pending()) /* necessary -- otherwise Snd hangs in gtk_main_iteration */
    gtk_main_iteration();
#else
  {
    int i = 50;
    /* we need to let more than 1 event through at a time, else (for example) the listener popup
     *   menu never actually pops up.
     *
     * if no threads (as here) this is just g_main_context_pending(NULL)
     *   then gtk_main_iteration calls g_main_context_iteration(NULL, true), 
     *   so g_main_context_iteration(NULL, false) might combine the two.
     *   But the overhead of gtk_events_pending is insignificant.  This code
     *   is extremely slow -- it more than doubles the compute time of s7test
     *   for example, and spends 10% of its time fiddling with useless locks.
     */

    while ((gtk_events_pending()) && (i != 0))
      {
	gtk_main_iteration();
	i--; 
      }
  }
#endif
#endif

  return(ss->C_g_typed);
}
#endif


void listener_return(widget_t w, int last_prompt)
{
#if (!USE_NO_GUI)
  /* try to find complete form either enclosing current cursor, or just before it */
  GUI_TEXT_POSITION_TYPE cmd_eot = 0;
  char *str = NULL, *full_str = NULL;
  int i, j;
  XEN form = XEN_UNDEFINED;
  GUI_TEXT_POSITION_TYPE last_position = 0, current_position = 0;

#if (!HAVE_RUBY && !HAVE_FORTH)
  GUI_TEXT_POSITION_TYPE end_of_text = 0, start_of_text = 0;
  int parens;
#if USE_MOTIF
  GUI_TEXT_POSITION_TYPE new_eot = 0;
#endif
#endif

  full_str = GUI_TEXT(w);
  current_position = GUI_TEXT_INSERTION_POSITION(w);
#if (!HAVE_RUBY && !HAVE_FORTH)
  start_of_text = current_position;
  end_of_text = current_position;
#endif
  last_position = GUI_TEXT_END(w);
  add_listener_position(last_position);

#if (!HAVE_SCHEME)
  if (XEN_HOOKED(read_hook))
    {
      XEN result;
      int len;
      len = last_position - last_prompt;
      if (len > 0)
	{
	  str = (char *)calloc(len + 1, sizeof(char)); 
	  for (i = last_prompt, j = 0; i < last_position; i++, j++) str[j] = full_str[i]; 
	  result = run_or_hook(read_hook, 
			       XEN_LIST_1(C_TO_XEN_STRING(str)),
			       S_read_hook);
	  free(str);
	  if (XEN_TRUE_P(result)) 
	    {
	      if (full_str) GUI_FREE(full_str);
	      return;
	    }
	}
    }
#endif

  /* prompt = listener_prompt(ss); */

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
	  str = (char *)calloc(len, sizeof(char));
	  for (k = 0; k < len - 1; k++)
	    str[k] = full_str[k + start];
          break; 
	}
  }
#else
  for (i = current_position - 1; i >= 0; i--)
    if ((full_str[i] == '\n') && (is_prompt(full_str, i + 1)))
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
	      str = (char *)calloc(end_of_text - start_of_text + 2, sizeof(char));
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
	  end_of_text = last_position; /* added 12-Nov-07 for first form */
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
	  str = (char *)calloc(slen, sizeof(char));
	  for (i = start_of_text, j = 0; i <= end_of_text; j++, i++) 
	    {
	      str[j] = full_str[i]; 
	      if (str[j] == '(') 
		parens++;
	    }
	  str[end_of_text - start_of_text + 1] = 0;
	  end_of_text = mus_strlen(str);

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
		  free(str);
		  str = NULL;
		  if (end_of_text < 0)
		    listener_append_and_prompt(NULL);
		  else 
		    {
#if USE_MOTIF
		      new_eot = GUI_TEXT_END(w);
		      GUI_LISTENER_TEXT_INSERT(w, new_eot, (char *)"\n");
#else
		      GUI_LISTENER_TEXT_INSERT(w, 0, (char *)"\n");
#endif
		    }
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
		    len = mus_strlen(str);
		    tmp = (char *)calloc(len + 1, sizeof(char));
		    if (i != 0) i++;
		    for (k = 0; i < len; i++, k++) 
		      if ((i > loc) &&
			  ((str[i] == '\n') || 
			   (str[i] == ' ')))
			break;
		      else tmp[k] = str[i];
		    free(str);
		    str = tmp;
		    break;
		  }
	    }
	}
    }
#endif

  if (full_str) GUI_FREE(full_str);
  {
    bool need_eval = false;
    int i, len;
    len = mus_strlen(str);
    for (i = 0; i < len; i++)
      if ((str[i] != ' ') &&
	  (str[i] != '\n') &&
	  (str[i] != '\r') &&
	  (str[i] != '\t'))
	{
	  need_eval = true;
	  break;
	}
    if (!need_eval)
      append_listener_text(-1, "\n");
    else
      {
	if (str)
	  {
	    char *errmsg = NULL;
	    
	    /* fprintf(stderr, "str: [%c]\n", str[0]); */
	    
	    if (current_position < (last_position - 2))
	      GUI_LISTENER_TEXT_INSERT(w, GUI_TEXT_END(w), str);
	    
	    GUI_SET_CURSOR(w, ss->wait_cursor);
	    GUI_UPDATE(w); /* not sure about this... */
	    
	    if ((mus_strlen(str) > 1) || (str[0] != '\n'))
	      remember_listener_string(str);
	    
#if HAVE_RUBY || HAVE_FORTH
	    form = XEN_EVAL_C_STRING(str);
#endif
	    
#if HAVE_SCHEME
	    /* very tricky -- we need the interface running to see C-g, and in ordinary (not-hung, but very slow-to-compute) code
	     *   we need to let other interface stuff run.  We can't look at each event and flush all but the C-g we're waiting
	     *   for because that confuses the rest of the GUI, and some such interactions are expected.  But if interface actions
	     *   are tied to scheme code, the check_for_event lets that code be evaluated, even though we're actually running
	     *   the evaluator already in a separate thread.  If we block on the thread ID (pthread_self), bad stuff still gets
	     *   through somehow.  
	     *
	     * s7 threads here only solves the s7 side of the problem.  To make the Gtk calls thread-safe,
	     *   we have to use gdk threads, and that means either wrapping every gtk section thoughout Snd in
	     *   gdk_thread_enter/leave, or expecting the caller to do that in every expression he types in the listener.
	     *
	     * Using clone_s7 code in s7 for CL-like stack-groups is much trickier
	     *   than I thought -- it's basically importing all the pthread GC and alloc stuff into the main s7.
	     *
	     * So... set begin_hook to a func that calls gtk_main_iteration or check_for_event;
	     *   if C-g, the begin_hook func returns true, and s7 calls s7_quit, and C_g_typed is true here.
	     *   Otherwise, I think anything is safe because we're only looking at the block start, and
	     *   we're protected there by a stack barrier.  
	     *
	     * But this polling at block starts is expensive, mainly because XtAppPending and gtk_events_pending
	     *   are very slow.  So with_interrupts can turn off this check.
	     */
	    
	    if (s7_begin_hook(s7) != NULL) return;      /* s7 is already running (user typed <cr> during computation) */
	    
	    if ((mus_strlen(str) > 1) || (str[0] != '\n'))
	      {
		
		int gc_loc;
		s7_pointer old_port;
		
		old_port = s7_set_current_error_port(s7, s7_open_output_string(s7));
		gc_loc = s7_gc_protect(s7, old_port);
		
		if (with_interrupts(ss))
		  s7_set_begin_hook(s7, listener_begin_hook);
		
		if (XEN_HOOKED(read_hook))
		  form = run_or_hook(read_hook, 
				     XEN_LIST_1(C_TO_XEN_STRING(str)),
				     S_read_hook);
		else form = XEN_EVAL_C_STRING(str);
		
		s7_set_begin_hook(s7, NULL);
		if (ss->C_g_typed)
		  {
		    errmsg = mus_strdup("\nSnd interrupted!");
		    ss->C_g_typed = false;
		  }
		else errmsg = mus_strdup(s7_get_output_string(s7, s7_current_error_port(s7)));
		
		s7_close_output_port(s7, s7_current_error_port(s7));
		s7_set_current_error_port(s7, old_port);
		s7_gc_unprotect_at(s7, gc_loc);
	      }
#endif
	    
	    if (errmsg)
	      {
		if (*errmsg)
		  snd_display_result(errmsg, NULL);
		free(errmsg);
	      }
	    else snd_report_listener_result(form); /* used to check for unbound form here, but that's no good in Ruby */
	    
	    free(str);
	    str = NULL;
	    GUI_UNSET_CURSOR(w, ss->arrow_cursor); 
	  }
	else
	  {
	    listener_append_and_prompt(NULL);
	  }
      }
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
  const char *name;
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
	      XEN_LIST_3(C_TO_XEN_STRING(S_save_listener ": can't save ~S, ~A"),
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


static XEN g_show_listener(void) 
{
  #define H_show_listener "(" S_show_listener ") returns " PROC_TRUE " if the listener is open, otherwise " PROC_FALSE "."
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
  ss->listener_prompt_length = mus_strlen(new_prompt);

#if USE_NO_GUI
  {
#if HAVE_FORTH
    char *str;
    XEN_EVAL_C_STRING("before-prompt-hook reset-hook!\n");
    str = mus_format("before-prompt-hook lambda: <{ prompt pos }> \"%s\" ; add-hook!", listener_prompt(ss));
    XEN_EVAL_C_STRING(str);
    free(str);
#endif

#if HAVE_RUBY
    xen_rb_repl_set_prompt(listener_prompt(ss));
#endif

#if HAVE_SCHEME
    xen_s7_set_repl_prompt(listener_prompt(ss));
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

  if (listener_prompt(ss)) free(listener_prompt(ss));
  set_listener_prompt(mus_strdup(XEN_TO_C_STRING(val)));

  return(C_TO_XEN_STRING(listener_prompt(ss)));
}


static XEN g_snd_completion(XEN text)
{
  /* perhaps callable from emacs? */
  char *str, *temp;
  XEN res;

  XEN_ASSERT_TYPE(XEN_STRING_P(text), text, XEN_ONLY_ARG, "snd-completion", "a string"); 

  temp = mus_strdup(XEN_TO_C_STRING(text));
  str = expression_completer(NULL_WIDGET, temp, NULL);
  res = C_TO_XEN_STRING(str);

  free(str);
  free(temp);

  return(res);
}


#ifdef XEN_ARGIFY_1
XEN_NARGIFY_1(g_save_listener_w, g_save_listener)
XEN_NARGIFY_0(g_clear_listener_w, g_clear_listener);
XEN_NARGIFY_0(g_show_listener_w, g_show_listener)
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
				   S_setB S_show_listener, g_set_show_listener_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_listener_prompt, g_listener_prompt_w, H_listener_prompt,
				   S_setB S_listener_prompt, g_set_listener_prompt_w,  0, 0, 1, 0);

  #define H_read_hook S_read_hook " (text): called each time a line is typed into the listener (triggered by the carriage return). \
If it returns true, Snd assumes you've dealt the text yourself, and does not try to evaluate it."
  
  read_hook = XEN_DEFINE_HOOK(S_read_hook, "(make-hook 'text)", 1, H_read_hook);

  XEN_DEFINE_PROCEDURE("snd-completion",        g_snd_completion_w,        1, 0, 0, "return completion of arg");
}
