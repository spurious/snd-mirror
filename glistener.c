#include "glistener.h"

/* compile-time switches: HAVE_GTK_3
 * supplied: help finder, evaluator, symbol table lookup
 */

#if (!HAVE_GTK_3)
  #define GDK_KEY_BackSpace GDK_BackSpace
  #define GDK_KEY_Down      GDK_Down
  #define GDK_KEY_G         GDK_G
  #define GDK_KEY_Left      GDK_Left
  #define GDK_KEY_Return    GDK_Return
  #define GDK_KEY_Right     GDK_Right
  #define GDK_KEY_Tab       GDK_Tab
  #define GDK_KEY_Up        GDK_Up
  #define GDK_KEY_a         GDK_a
  #define GDK_KEY_b         GDK_b
  #define GDK_KEY_c         GDK_c
  #define GDK_KEY_d         GDK_d
  #define GDK_KEY_e         GDK_e
  #define GDK_KEY_f         GDK_f
  #define GDK_KEY_g         GDK_g
  #define GDK_KEY_greater   GDK_greater
  #define GDK_KEY_k         GDK_k
  #define GDK_KEY_l         GDK_l
  #define GDK_KEY_less      GDK_less
  #define GDK_KEY_n         GDK_n
  #define GDK_KEY_p         GDK_p
  #define GDK_KEY_period    GDK_period
  #define GDK_KEY_question  GDK_question
  #define GDK_KEY_t         GDK_t
  #define GDK_KEY_u         GDK_u
  #define GDK_KEY_v         GDK_v
  #define GDK_KEY_w         GDK_w
  #define GDK_KEY_y         GDK_y
#endif 

#define EVENT_KEYVAL(Ev) (Ev)->keyval

#if (HAVE_GTK_3) && defined(__GNUC__) && (!(defined(__cplusplus)))
  #define EVENT_STATE(Ev) ({ GdkModifierType Type;  gdk_event_get_state((GdkEvent *)Ev, &Type); Type; })
#else
  #define EVENT_STATE(Ev) (Ev)->state
#endif

#define ControlMask GDK_CONTROL_MASK
#define MetaMask GDK_MOD1_MASK




static GtkWidget *listener_text = NULL;
static GtkTextBuffer *listener_buffer = NULL;
static GtkWidget *listener_statusbar = NULL;
static GtkWidget *listener_scrolled_window = NULL;

#define LISTENER_TEXT GTK_TEXT_VIEW(listener_text)


void glistener_set_font(PangoFontDescription *font)
{
#if (!HAVE_GTK_3)
  if (listener_text)
    gtk_widget_modify_font(GTK_WIDGET(listener_text), font);
#else
  if (listener_text)
    gtk_widget_override_font(GTK_WIDGET(listener_text), font);
#endif
}


#if (!HAVE_GTK_3)
void glistener_set_text_color(GdkColor *p)
{
  if (listener_text) 
    gtk_widget_modify_text(listener_text, GTK_STATE_NORMAL, p);
}

void glistener_set_background_color(GdkColor *p)
{
  if (listener_text) 
    gtk_widget_modify_base(listener_text, GTK_STATE_NORMAL, p);
}
#else
void glistener_set_text_color(GdkRGBA *p)
{
  if (listener_text) 
    gtk_widget_override_color(listener_text, GTK_STATE_FLAG_ACTIVE, p);
}

void glistener_set_background_color(GdkRGBA *p)
{
  if (listener_text) 
    gtk_widget_override_background_color(listener_text, GTK_STATE_FLAG_ACTIVE, p);
}
#endif



/* TODO: clean this up eventually */
void post_status(const char *msg)
{
  if (listener_statusbar)
    {
      gtk_statusbar_push(GTK_STATUSBAR(listener_statusbar), 1, msg);
    }
}

void clear_status(void)
{
  if (listener_statusbar)
    {
      gtk_statusbar_pop(GTK_STATUSBAR(listener_statusbar), 1);
    }
}





/* ---------------- cursor ---------------- */

static void glistener_set_cursor_position(int position)
{
  GtkTextIter pos;
  /* -1 -> goto to end */
  if (position == -1)
    gtk_text_buffer_get_end_iter(listener_buffer, &pos);
  else gtk_text_buffer_get_iter_at_offset(listener_buffer, &pos, position);
  gtk_text_buffer_place_cursor(listener_buffer, &pos);

  gtk_text_view_scroll_mark_onscreen(LISTENER_TEXT, gtk_text_buffer_get_insert(listener_buffer));
}


static int glistener_cursor_position(void)
{
  GtkTextIter pos;
  GtkTextBuffer *buf;
  buf = listener_buffer;
  gtk_text_buffer_get_iter_at_mark(buf, &pos, gtk_text_buffer_get_insert(buf));
  return(gtk_text_iter_get_offset(&pos));
}

static int glistener_cursor(GtkTextIter *cursor)
{
  gtk_text_buffer_get_iter_at_mark(listener_buffer, cursor, gtk_text_buffer_get_insert(listener_buffer));
  return(gtk_text_iter_get_offset(cursor));
}


void glistener_set_cursor_shape(GdkCursor *cursor_shape)
{
  gdk_window_set_cursor(gtk_text_view_get_window(GTK_TEXT_VIEW(listener_text), GTK_TEXT_WINDOW_TEXT), cursor_shape);
}



/* ---------------- prompt ---------------- */

static char *prompt = NULL;
static int prompt_length = 0;
static GtkTextTag *prompt_tag = NULL;

static void prompt_insert(GtkTextIter *pos, bool at_top)
{
  if (at_top)
    {
      if (prompt_tag)
	gtk_text_buffer_insert_with_tags(listener_buffer, pos, (char *)(prompt + 1), -1, prompt_tag, NULL);
      else gtk_text_buffer_insert(listener_buffer, pos, (char *)(prompt + 1), -1);
    }
  else
    {
      if (prompt_tag)
	gtk_text_buffer_insert_with_tags(listener_buffer, pos, prompt, -1, prompt_tag, NULL);
      else gtk_text_buffer_insert(listener_buffer, pos, prompt, -1);
      /* TODO: scroll it down? */
      /* scroll fully left so the new prompt is in view */
      gtk_adjustment_set_value(GTK_ADJUSTMENT(gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDOW(gtk_widget_get_parent(listener_text)))), 0.0);
    }
}


void glistener_set_prompt_tag(GtkTextTag *m)
{
  prompt_tag = m;
  /* should this redisplay all the prompts? */
}


void glistener_set_prompt(const char *str)
{
  /* the internal version includes a preceding <cr>, and its length is in unicode terms
   *   also, if we have prompts displayed in the listener, they need to change to match
   *   the new one.
   *
   * glistener_set_prompt("s7>")
   *
   * from Snd, to get a prompt of lambda: in Greek font: 
   *   (set! (listener-prompt) (string (integer->char #xce) (integer->char #xbb) #\:))
   *   (set! (listener-prompt) (bytevector #xce #xbb (char->integer #\:)))
   */
  char *old_prompt;
  int old_prompt_length;

  if (!str) return;
  old_prompt = prompt;
  old_prompt_length = prompt_length;

  prompt = calloc(strlen(str) + 2, sizeof(char));
  prompt[0] = '\n';
  strcat(prompt, str);
  prompt_length = g_utf8_strlen(prompt, -1);
  /* fprintf(stderr, "prompt: %s, len: %d\n", prompt, prompt_length); */

  if (!listener_text) return;

  if (old_prompt)
    {
      /* nothing will work if the prompt is changed in midcareer unless we remake all the preceding prompts */
      GtkTextIter scan, start, end;

      /* the first prompt does not have a <cr> so we handle it directly */
      gtk_text_buffer_get_start_iter(listener_buffer, &start);
      gtk_text_buffer_get_iter_at_offset(listener_buffer, &end, old_prompt_length - 1);
      gtk_text_buffer_delete(listener_buffer, &start, &end);
      prompt_insert(&start, true);

      gtk_text_buffer_get_start_iter(listener_buffer, &scan);
      while (gtk_text_iter_forward_search(&scan, old_prompt, 0, &start, &end, NULL))
	{
	  /* TODO: create mark once, move it thereafter? */
	  gtk_text_buffer_create_mark(listener_buffer, "prompt_pos", &end, false); /* false -> "right gravity" */
	  gtk_text_buffer_delete(listener_buffer, &start, &end);
	  prompt_insert(&start, false);
	  gtk_text_buffer_get_iter_at_mark(listener_buffer, &scan, gtk_text_buffer_get_mark(listener_buffer, "prompt_pos"));
	}
      free(old_prompt);
    }
}


void glistener_append_prompt(void)
{
  if (listener_text)
    {
      GtkTextIter end;
      gtk_text_buffer_get_end_iter(listener_buffer, &end);
      prompt_insert(&end, false);
    }
}


#if (!HAVE_GTK_3)
/* backward search is buggy in gtk 2.20 (it's ok in gtk3 I think), and we depend on it!
 *   this code is ridiculous, but it's the least stupid thing I can find that seems to work.
 */
static gboolean prompt_backward_search(const GtkTextIter *iter, GtkTextIter *start, GtkTextIter *end)
{
  int pos, old_pos = 0, cur_pos;
  GtkTextIter scan1, scan2, s1, s2;
  bool found_it = false;
  
  cur_pos = gtk_text_iter_get_offset(iter);
  gtk_text_buffer_get_start_iter(listener_buffer, &scan1);
  gtk_text_buffer_get_start_iter(listener_buffer, &scan2);

  while (true)
    {
      if (!gtk_text_iter_forward_search(&scan1, prompt, 0, &s1, &s2, NULL))
	return(found_it);
      if (gtk_text_iter_get_offset(&s2) > cur_pos)
	return(found_it);
      found_it = true;
      gtk_text_iter_forward_search(&scan2, prompt, 0, start, end, NULL);
      scan1 = s2;
      scan2 = s2;
    }
  return(false);
}

#else

static gboolean prompt_backward_search(const GtkTextIter *iter, GtkTextIter *start, GtkTextIter *end)
{
  return(gtk_text_iter_backward_search(iter, prompt, 0, start, end, NULL));
}
#endif


static int find_current_prompt()
{
  GtkTextIter it, start, end;
  int pos;
  
  pos = glistener_cursor_position();
  if (pos < prompt_length - 1)
    return(prompt_length - 1);

  gtk_text_buffer_get_iter_at_offset(listener_buffer, &it, pos + prompt_length - 1);
  if (!prompt_backward_search(&it, &start, &end))
    return(prompt_length - 1);

  return(gtk_text_iter_get_offset(&start) + prompt_length);
}


static int find_previous_prompt(int pos)
{
  GtkTextIter it, start, end;
  int new_pos;
  
  if (pos < prompt_length - 1)
    return(prompt_length - 1);

  gtk_text_buffer_get_iter_at_offset(listener_buffer, &it, pos);
  if (!prompt_backward_search(&it, &start, &end))
    return(prompt_length - 1);
  new_pos = gtk_text_iter_get_offset(&end);
  if (new_pos < pos)
    return(new_pos);
  it = start;
  gtk_text_iter_backward_char(&it);
  if (!prompt_backward_search(&it, &start, &end))
    return(prompt_length - 1);
  return(gtk_text_iter_get_offset(&end));
}


static int find_next_prompt(void)
{
  GtkTextIter it, start, end;
  int pos;
  pos = glistener_cursor(&it);
  if (!gtk_text_iter_forward_search(&it, prompt, 0, &start, &end, NULL))
    gtk_text_buffer_get_end_iter(listener_buffer, &end);
  return(gtk_text_iter_get_offset(&end) + 1);
}


static bool is_prompt_end(int end_pos)
{
  /* the prompt includes the preceding <cr> */
  GtkTextIter start, end;
  bool result = false;

  if (end_pos < prompt_length)
    return(true);
  gtk_text_buffer_get_iter_at_offset(listener_buffer, &end, end_pos);
  start = end;
  if (gtk_text_iter_backward_chars(&start, prompt_length))
    {
      char *txt;
      txt = gtk_text_iter_get_text(&start, &end);
      if (txt)
	{
	  result = (strcmp(txt, prompt) == 0);
	  g_free(txt);
	}
    }
  return(result);
}





/* ---------------- listener text ---------------- */

static char **listener_strings = NULL;
static int listener_strings_size = 0, listener_strings_pos = 0;
static bool listener_first_time = true;

static void remember_listener_string(const char *str)
{
  int i, top, len;
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

  len = strlen(str);
  listener_strings[0] = (char *)calloc(len, sizeof(char));
  strcpy(listener_strings[0], str);
}


static void restore_listener_string(bool back)
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
	glistener_append_text(str); 
    }
}

void glistener_clear(void)
{
  if (listener_text)
    {
      GtkTextIter start, end;
      GtkTextBuffer *buf;
      buf = listener_buffer;
      gtk_text_buffer_get_iter_at_offset(buf, &start, prompt_length);
      gtk_text_buffer_get_end_iter(buf, &end); 
      gtk_text_buffer_delete(buf, &start, &end);
    }
}


bool glistener_write(FILE *fp)
{
  char *str = NULL;
  GtkTextIter start, end;
  GtkTextBuffer *buf;
  
  buf = listener_buffer;
  gtk_text_buffer_get_start_iter(buf, &start);
  gtk_text_buffer_get_end_iter(buf, &end);
  str = gtk_text_buffer_get_text(buf, &start, &end, true);
  if (str)
    {
      fwrite((void *)str, sizeof(char), strlen(str), fp);
      g_free(str);
    }

  return(true);
}


bool glistener_write_to_file(const char *filename)
{
  if (listener_text)
    {
      FILE *fp;
      fp = fopen(filename, "w");
      if (fp)
	{
	  bool result;
	  result = glistener_write(fp);
	  fclose(fp);
	  return(result);
	}
    }
  return(false);
}


void glistener_append_text(const char *msg)
{
  if (listener_text)
    {
      GtkTextIter end;
      gtk_text_buffer_get_end_iter(listener_buffer, &end);
      gtk_text_buffer_insert(listener_buffer, &end, (char *)msg, -1);
    }
}


void glistener_scroll_to_end(void)
{
  if (listener_text)
    {
      GtkTextIter end;
      gtk_text_buffer_get_end_iter(listener_buffer, &end);
      gtk_text_buffer_place_cursor(listener_buffer, &end);
      gtk_text_view_scroll_mark_onscreen(LISTENER_TEXT, gtk_text_buffer_get_insert(listener_buffer));
      /* TODO: also scroll left all the way, same for new prompt 
       *  gtk_adjustment_set_value(GTK_ADJUSTMENT(gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDOW(gtk_widget_get_parent(listener_text)))), 0.0)?
       */
    }
}



/* ---------------- paren matching ---------------- */

static gboolean is_not_whitespace(gunichar c, gpointer data)
{
  return(!g_unichar_isspace(c));
}


static bool find_not_whitespace(int pos, GtkTextIter *limit)
{
  GtkTextIter scan;
  gtk_text_buffer_get_iter_at_offset(listener_buffer, &scan, pos);  
  return(gtk_text_iter_forward_find_char(&scan, is_not_whitespace, NULL, limit));
}


static gboolean is_unslashed_double_quote(gunichar c, gpointer data)
{
  int *slashes = (int *)data;
  if (c == '\\')
    (*slashes)++;
  else
    {
      if ((c == '\"') &&
	  (*slashes & 1) == 0)
	return(true);
      *slashes = 0;
    }
  return(false);
}


static int find_string_end(int pos, GtkTextIter *limit)
{
  /* returns -1 if no close quote */

  GtkTextIter scan;
  int slashes = 0;
  gtk_text_buffer_get_iter_at_offset(listener_buffer, &scan, pos);  
  if (!gtk_text_iter_forward_find_char(&scan, is_unslashed_double_quote, &slashes, limit))
    return(-1);
  return(gtk_text_iter_get_offset(&scan));
}


static gboolean is_double_quote(gunichar c, gpointer data)
{
  return(c == '\"');
}


static int find_string_start(int pos, GtkTextIter *limit)
{
  /* returns -1 if no close quote */
  GtkTextIter scan, slasher;
  int slashes = 0;
  gunichar cs;

  gtk_text_buffer_get_iter_at_offset(listener_buffer, &scan, pos);  
  while (!gtk_text_iter_equal(limit, &scan))
    {
      if (!gtk_text_iter_backward_find_char(&scan, is_double_quote, NULL, limit))
	return(-1);
      slashes = 0;
      slasher = scan;
      while (!gtk_text_iter_equal(limit, &slasher))
	{
	  gtk_text_iter_backward_char(&slasher);
	  cs = gtk_text_iter_get_char(&slasher);
	  if (cs == (gunichar)'\\')
	    slashes++;
	  else break;
	}
      if ((slashes & 1) == 0)
	break;
      scan = slasher;
    }
  return(gtk_text_iter_get_offset(&scan));
}


static gboolean is_block_comment(gunichar c, gpointer data)
{
  int *last_c = (int *)data;
  if ((c == '#') &&
      (*last_c == '|'))
    return(true);
  *last_c = c;
  return(false);
}


static int find_open_block_comment(int pos, GtkTextIter *limit)
{
  GtkTextIter scan;
  int last_cs = 0;

  gtk_text_buffer_get_iter_at_offset(listener_buffer, &scan, pos);  
  gtk_text_iter_backward_find_char(&scan, is_block_comment, &last_cs, limit);
  return(gtk_text_iter_get_offset(&scan));
}


static int find_close_block_comment(int pos, GtkTextIter *limit)
{
  GtkTextIter scan;
  int last_cs = 0;

  gtk_text_buffer_get_iter_at_offset(listener_buffer, &scan, pos);  
  gtk_text_iter_forward_find_char(&scan, is_block_comment, &last_cs, limit);
  return(gtk_text_iter_get_offset(&scan));
}


static gboolean is_delimiter(gunichar c, gpointer data)
{
  return((g_unichar_isspace(c)) ||
	 (c == '(') ||
	 (c == ')') ||
	 (c == ';') ||
	 (c == '\"'));
  /* in s7, single-quote can appear in a name */
}


static gboolean is_delimiter_q(gunichar c, gpointer data)
{
  return((g_unichar_isspace(c)) ||
	 (c == '(') ||
	 (c == ')') ||
	 (c == ';'));
}


static void find_surrounding_word(int pos, 
				  gboolean (*checker)(gunichar c, gpointer data),
				  int *start_pos, int *end_pos, 
				  GtkTextIter *start_limit, GtkTextIter *end_limit)
{
  GtkTextIter start, end;

  *start_pos = gtk_text_iter_get_offset(start_limit);
  *end_pos = gtk_text_iter_get_offset(end_limit);

  gtk_text_buffer_get_iter_at_offset(listener_buffer, &start, pos);
  end = start;

  if (!gtk_text_iter_equal(start_limit, &start))
    {
      if (gtk_text_iter_backward_find_char(&start, checker, NULL, start_limit))
	*start_pos = gtk_text_iter_get_offset(&start) + 1;
    }
  if ((!gtk_text_iter_equal(&end, end_limit)) &&
      (!g_unichar_isspace(gtk_text_iter_get_char(&end))))
    {
      if (gtk_text_iter_forward_find_char(&end, checker, NULL, end_limit))
	*end_pos = gtk_text_iter_get_offset(&end);
    }
}


static char *get_preceding_text(int pos, bool *in_string)
{
  GtkTextIter s1, e1, elimit;
  int start = 0, end = 0;
  char *text;

  gtk_text_buffer_get_iter_at_offset(listener_buffer, &s1, find_current_prompt());
  gtk_text_buffer_get_iter_at_offset(listener_buffer, &e1, pos);
  gtk_text_buffer_get_end_iter(listener_buffer, &elimit);
  *in_string = false;

  if (gtk_text_iter_equal(&s1, &elimit))
    return(NULL);
  
  if ((gtk_text_iter_equal(&e1, &elimit)) ||
      (g_unichar_isspace(gtk_text_iter_get_char(&e1))))
    {
      find_surrounding_word(pos, is_delimiter, &start, &end, &s1, &e1);
      gtk_text_buffer_get_iter_at_offset(listener_buffer, &elimit, start - 1);
      *in_string = (gtk_text_iter_get_char(&elimit) == '\"');
      gtk_text_buffer_get_iter_at_offset(listener_buffer, &s1, start);
      gtk_text_buffer_get_iter_at_offset(listener_buffer, &e1, end);
      return(gtk_text_buffer_get_text(listener_buffer, &s1, &e1, true));
    }
  return(NULL);
}


static bool at_character_constant(int end_pos)
{
  GtkTextIter start, end;
  gtk_text_buffer_get_iter_at_offset(listener_buffer, &end, end_pos);
  start = end;
  if (gtk_text_iter_backward_chars(&start, 2))
    {
      char *txt;
      bool result = false;
      txt = gtk_text_iter_get_text(&start, &end);
      /* fprintf(stderr, "char: [%s: %c%c]\n", txt, txt[0], txt[1]); */
      if (txt)
	{
	  result = (strcmp(txt, "#\\") == 0);
	  g_free(txt);
	}
      return(result);
    }
  return(false);
}


static bool find_open_paren(int parens, int pos, int *highlight_pos, GtkTextIter *limit)
{
  GtkTextIter scan;
  int parens_at_line_end = parens, ppos;
  bool end_scan = false;
  gunichar c = 0, last_c;

  if (at_character_constant(pos + 1))
    return(false);

  gtk_text_buffer_get_iter_at_offset(listener_buffer, &scan, pos);  
  while (!gtk_text_iter_equal(limit, &scan))
    {
      last_c = c;
      c = gtk_text_iter_get_char(&scan);
      /* fprintf(stderr, "%d %c\n", __LINE__, c); */
      if (c == (gunichar)'\"') 
	{
	  ppos = find_string_start(gtk_text_iter_get_offset(&scan), limit);
	  /* TODO: if no matching quote then we're currently in a string? -- this is needed anyway for <tab> */
	  gtk_text_buffer_get_iter_at_offset(listener_buffer, &scan, ppos);
	  last_c = '\"';
	}
      else
	{
	  if (!at_character_constant(gtk_text_iter_get_offset(&scan)))
	    {
	      if (c == '\n')
		{
		  if (end_scan)
		    return(true);
		  parens_at_line_end = parens;
		}
	      else
		{
		  if (c == (gunichar)';')
		    {
		      parens = parens_at_line_end;
		      end_scan = false;
		    }
		  else
		    {
		      if ((c == (gunichar)'|') &&
			  (last_c == (gunichar)'#')) /* we're looking backwards here, so in the end marker |# we see the # first */
			{
			  ppos = find_open_block_comment(gtk_text_iter_get_offset(&scan), limit);
			  gtk_text_buffer_get_iter_at_offset(listener_buffer, &scan, ppos);
			  last_c = '#';
			}
		      else
			{
			  if (!end_scan)
			    {
			      if (c == ')') 
				parens++; 
			      else
				{
				  if (c == '(')
				    {
				      parens--; 
				      if (parens == 0)
					{
					  (*highlight_pos) = gtk_text_iter_get_offset(&scan);
					  end_scan = true;
					}
				    }
				}
			    }
			}
		    }
		}
	    }
	}
      gtk_text_iter_backward_char(&scan);
    }

  return(parens == 0);
}


static bool find_close_paren(int parens, int pos, int *highlight_pos, GtkTextIter *limit)
{
  GtkTextIter scan;
  int parens_at_line_end = parens, ppos;
  bool end_scan = false;
  gunichar c, prev_c = 0, prev_prev_c = 0;

  /* fprintf(stderr, "start ) search at %d\n", pos); */

  gtk_text_buffer_get_iter_at_offset(listener_buffer, &scan, pos);  
  while (!gtk_text_iter_equal(&scan, limit))
    {
      prev_prev_c = prev_c;
      prev_c = c;
      c = gtk_text_iter_get_char(&scan);
      /* fprintf(stderr, "%d: %c\n", __LINE__, c); */

      if ((prev_c != '\\') || (prev_prev_c != '#'))
	{
	  if (c == (gunichar)'\"')
	    {
	      ppos = find_string_end(gtk_text_iter_get_offset(&scan), limit);
	      gtk_text_buffer_get_iter_at_offset(listener_buffer, &scan, ppos);
	      prev_prev_c = 0;
	      prev_c = '\"';
	    }
	  else
	    {
	      if (c == (gunichar)';')
		gtk_text_iter_forward_to_line_end(&scan);
	      else
		{
		  if ((c == (gunichar)'|') &&
		      (prev_c == (gunichar)'#'))
		    {
		      ppos = find_close_block_comment(gtk_text_iter_get_offset(&scan), limit);
		      gtk_text_buffer_get_iter_at_offset(listener_buffer, &scan, ppos);
		    }
		  else
		    {
		      if (c == ')') 
			{
			  parens--; 
			  if (parens == 0)
			    {
			      (*highlight_pos) = gtk_text_iter_get_offset(&scan);
			      return(true);
			    }
			}
		      else
			{
			  if (c == '(')
			    parens++;
			}
		    }
		}
	    }
	}
      gtk_text_iter_forward_char(&scan);
    }
  return(parens == 0);
}


static GtkTextTag *flash_tag = NULL;
static int flashes = 0;
static int flash_paren_pos = -1;
#define FLASH_TIME 150

/* TODO: make sure we can't get into contention over flash_paren_pos during the timeouts */

static void add_inverse(int pos)
{
  GtkTextIter start, end;
  GtkTextBuffer *buf;
  buf = listener_buffer;
  if (flash_paren_pos == -1) flash_paren_pos = pos;
  gtk_text_buffer_get_iter_at_offset(buf, &start, pos);
  gtk_text_buffer_get_iter_at_offset(buf, &end, pos + 1);
  if (!flash_tag) flash_tag = gtk_text_buffer_create_tag(buf, "red_background", "background", "red", NULL);
  gtk_text_buffer_apply_tag(buf, flash_tag, &start, &end);
}


static void remove_inverse(int pos)
{
  GtkTextIter start, end;
  GtkTextBuffer *buf;
  buf = listener_buffer;
  gtk_text_buffer_get_iter_at_offset(buf, &start, pos);
  gtk_text_buffer_get_iter_at_offset(buf, &end, pos + 1);
  if (!flash_tag) flash_tag = gtk_text_buffer_create_tag(buf, "red_background", "background", "red", NULL);
  gtk_text_buffer_remove_tag(buf, flash_tag, &start, &end);
}


static gint flash_unbalanced_paren(gpointer data)
{
  flashes--;
  if (flashes & 1) 
    remove_inverse(flash_paren_pos); 
  else add_inverse(flash_paren_pos);
  if (flashes > 0)
    g_timeout_add_full(0, (guint32)FLASH_TIME, flash_unbalanced_paren, NULL, NULL);
  else 
    {
      remove_inverse(flash_paren_pos);
      flash_paren_pos = -1;
    }
  return(0);
}


static GtkTextTag *underline_tag = NULL;
static int underline_start = -1, underline_end = -1;

static void add_underline(int bpos, int epos)
{
  GtkTextIter start, end;
  GtkTextBuffer *buf;
  buf = listener_buffer;
  gtk_text_buffer_get_iter_at_offset(buf, &start, bpos);
  gtk_text_buffer_get_iter_at_offset(buf, &end, epos);
  if (!underline_tag) underline_tag = gtk_text_buffer_create_tag(buf, "underline", "underline", PANGO_UNDERLINE_DOUBLE, NULL);
  gtk_text_buffer_apply_tag(buf, underline_tag, &start, &end);
  underline_start = bpos;
  underline_end = epos;
}


static void remove_underline()
{
  if ((underline_tag) &&
      (underline_start != -1))
    {
      GtkTextIter start, end;
      GtkTextBuffer *buf;
      buf = listener_buffer;
      gtk_text_buffer_get_iter_at_offset(buf, &start, underline_start);
      gtk_text_buffer_get_iter_at_offset(buf, &end, underline_end);
      gtk_text_buffer_remove_tag(buf, underline_tag, &start, &end);
      underline_start = -1;
      underline_end = -1;
    }
}


static void check_parens(void)
{
  int pos;
  GtkTextIter scan, limit;
  gunichar c;

  remove_underline();

  pos = glistener_cursor_position();
  gtk_text_buffer_get_iter_at_offset(listener_buffer, &scan, pos - 1);

  c = gtk_text_iter_get_char(&scan);
  if (c == ')')
    {
      gtk_text_buffer_get_iter_at_offset(listener_buffer, &limit, find_current_prompt() - 1);
      if (find_open_paren(1, pos - 2, &pos, &limit))
	add_underline(pos, pos + 1);
    }
  else
    {
      gtk_text_iter_forward_char(&scan);
      c = gtk_text_iter_get_char(&scan);
      if (c == '(')
	{
	  gtk_text_buffer_get_iter_at_offset(listener_buffer, &limit, find_next_prompt());
	  if (find_close_paren(1, pos + 1, &pos, &limit))
	    add_underline(pos, pos + 1);
	}
    }
}



/* ---------------- text editing ---------------- */

static void word_upper(GtkWidget *w, bool capitalize, bool upcase)
{
  int pos;
  GtkTextIter start, end;
  char *text;

  pos = glistener_cursor_position();
  gtk_text_buffer_get_iter_at_offset(listener_buffer, &start, pos);
  end = start;
  gtk_text_iter_forward_word_end(&end);
  text = gtk_text_buffer_get_text(listener_buffer, &start, &end, true);
  if (text)
    {
      if (upcase)
	text = g_utf8_strup(text, -1);
      else
	{
	  text = g_utf8_strdown(text, -1);
	  if (capitalize)
	    {
	      /* a temporary(?) kludge... (we actually need to find the first non-space char, figure out how to "title-case" it, strcat everything back together)
	       */
	      text[0] = toupper(text[0]);
	    }
	}
      gtk_text_buffer_delete(listener_buffer, &start, &end);
      gtk_text_buffer_insert(listener_buffer, &start, text, -1);
      g_free(text);
    }
}


static void text_transpose(GtkWidget *w)
{
  int curpos;
  GtkTextIter start, end;
  curpos = glistener_cursor(&end);
  if (!is_prompt_end(curpos))
    {
      char *text, *new_text;
      start = end;
      gtk_text_iter_backward_char(&start);
      gtk_text_iter_forward_char(&end);
      text = gtk_text_buffer_get_text(listener_buffer, &start, &end, false);
      new_text = g_utf8_strreverse(text, -1);
      gtk_text_buffer_delete(listener_buffer, &start, &end);
      gtk_text_buffer_insert(listener_buffer, &start, new_text, -1);
      g_free(text);
      g_free(new_text);
    }
}


static void clear_back_to_prompt(GtkWidget *w)
{
  /* buggy in gtk 2.20 (backward search) */
  int beg, end;
  GtkTextIter start, last;

  end = glistener_cursor_position();
  beg = find_current_prompt();
  if (end <= beg) return;

  gtk_text_buffer_get_iter_at_offset(listener_buffer, &start, beg);
  gtk_text_buffer_get_iter_at_offset(listener_buffer, &last, end); 
  gtk_text_buffer_delete(listener_buffer, &start, &last);
}




/* ---------------- key bindings ---------------- */

/* a temporary kludge */
GtkWidget *glistener_text(void) {return(listener_text);}
GtkTextBuffer *glistener_buffer(void) {return(listener_buffer);}

static const char *(*glistener_helper)(const char *text) = NULL;
static void (*glistener_evaluator)(const char *text) = NULL;
static void (*glistener_symbol_completer)(bool (*symbol_func)(const char *symbol_name, void *data), void *data) = NULL;

void glistener_set_help(const char *(*help)(const char *text))
{
  glistener_helper = help;
}

void glistener_set_evaluator(void (*eval)(const char *text))
{
  glistener_evaluator = eval;
}

void glistener_set_symbol_completer(void (*completer)(bool (*symbol_func)(const char *symbol_name, void *data), void *data))
{
  glistener_symbol_completer = completer;
}




void glistener_bindings(gpointer cls)
{
  /* emacs key bindings for the most part
   */
  GtkBindingSet *set;
  set = gtk_binding_set_by_class(cls);
  
  /* C-a start of line */
  gtk_binding_entry_remove(set, GDK_KEY_a, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_a, GDK_CONTROL_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINE_ENDS,
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);

  /* C-b back char */
  gtk_binding_entry_remove(set, GDK_KEY_b, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_b, GDK_CONTROL_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_VISUAL_POSITIONS,
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);

  /* M-b back word */
  gtk_binding_entry_remove(set, GDK_KEY_b, GDK_MOD1_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_b, GDK_MOD1_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_WORDS,
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);

  /* C-d delete at cursor */
  gtk_binding_entry_remove(set, GDK_KEY_d, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_d, GDK_CONTROL_MASK,
			       "delete_from_cursor", 2,
			       G_TYPE_ENUM, GTK_DELETE_CHARS,
			       G_TYPE_INT, 1); /* -1 = delete to left of cursor */

  /* C-e end of line */
  gtk_binding_entry_remove(set, GDK_KEY_e, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_e, GDK_CONTROL_MASK, "move_cursor", 3, /* 3 = n_args */
			       G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINE_ENDS,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);

 /* C-f forward char */
  gtk_binding_entry_remove(set, GDK_KEY_f, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_f, GDK_CONTROL_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_VISUAL_POSITIONS,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);

   /* M-f forward word */
  gtk_binding_entry_remove(set, GDK_KEY_f, GDK_MOD1_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_f, GDK_MOD1_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_WORDS,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);

  /* C-n down line */
  gtk_binding_entry_remove(set, GDK_KEY_n, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_n, GDK_CONTROL_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINES,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);

  /* C-p up line */
  gtk_binding_entry_remove(set, GDK_KEY_p, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_p, GDK_CONTROL_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINES,
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);

  /* C-y yank <- clipboard */
  gtk_binding_entry_remove(set, GDK_KEY_y, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_y, GDK_CONTROL_MASK,
			       "paste_clipboard", 0);

  /* C-w delete region -> clipboard -- it's possible to clobber a prompt here */
  gtk_binding_entry_remove(set, GDK_KEY_w, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_w, GDK_CONTROL_MASK,
			       "cut_clipboard", 0);

  /* M-< start of file */
  gtk_binding_entry_remove(set, GDK_KEY_less, GDK_MOD1_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_less, GDK_MOD1_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_BUFFER_ENDS,
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);

  /* M-> end of file */
  gtk_binding_entry_remove(set, GDK_KEY_greater, GDK_MOD1_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_greater, GDK_MOD1_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_BUFFER_ENDS,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);

  /* down-arrow end of file */
  gtk_binding_entry_remove(set, GDK_KEY_Down, 0);
  gtk_binding_entry_add_signal(set, GDK_KEY_Down, 0, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_BUFFER_ENDS,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);

  /* up-arrow start of file */
  gtk_binding_entry_remove(set, GDK_KEY_Up, 0);
  gtk_binding_entry_add_signal(set, GDK_KEY_Up, 0, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_BUFFER_ENDS,
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);

  /* right-arrow end of line */
  gtk_binding_entry_remove(set, GDK_KEY_Right, 0);
  gtk_binding_entry_add_signal(set, GDK_KEY_Right, 0, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINE_ENDS,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);

  /* left-arrow start of line */
  gtk_binding_entry_remove(set, GDK_KEY_Left, 0);
  gtk_binding_entry_add_signal(set, GDK_KEY_Left, 0, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINE_ENDS,
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);

  /* C-v move down a window */
  gtk_binding_entry_remove(set, GDK_KEY_v, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_v, GDK_CONTROL_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_PAGES,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);

  /* M-v for up window */
  gtk_binding_entry_remove(set, GDK_KEY_v, GDK_MOD1_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_v, GDK_MOD1_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_PAGES,
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);

  /* M-d delete word at cursor */
  gtk_binding_entry_remove(set, GDK_KEY_d, GDK_MOD1_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_d, GDK_MOD1_MASK,
			       "delete_from_cursor", 2,
			       G_TYPE_ENUM, GTK_DELETE_WORD_ENDS,
			       G_TYPE_INT, 1);
}



static void glistener_return_callback(GtkWidget *w);
static void listener_completion(int end);


static gboolean glistener_key_press(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  int pos;
  char *text;
  guint key;
  GdkModifierType state;

  key = EVENT_KEYVAL(event);
  state = (GdkModifierType)EVENT_STATE(event);

  /* fprintf(stderr, "key: %d, state: %d\n", key, state);  */

  switch (key)
    {
      /* further processing (by gtk) of the keystroke is blocked if we fall through */
    case GDK_KEY_a:
      if (state & MetaMask)
	glistener_set_cursor_position(find_previous_prompt(glistener_cursor_position()));
      else return(false);
      break;
      
    case GDK_KEY_c:
      if (state & MetaMask)
	word_upper(listener_text, true, false);
      else return(false);
      break;

    case GDK_KEY_d:
      if (state & ControlMask)
	{
	  /* need to check for prompt just ahead */
	  if (!is_prompt_end(glistener_cursor_position() + prompt_length))
	    return(false);
	  /* else we're sitting at (just in front of) the prompt so drop through and block the signal */
	}
      else
	{
	  if (state & MetaMask)
	    {
	      GtkTextIter p;
	      int i, pos, new_pos, cur;
	      char *text;
	      bool hits_prompt = false;

	      pos = glistener_cursor(&p);
	      gtk_text_iter_forward_word_end(&p);
	      new_pos = gtk_text_iter_get_offset(&p);
	      cur = pos + prompt_length;

	      /* if there's a prompt somewhere between pos and new_pos, block this deletion */
	      for (i = 0; i < (new_pos - pos); i++)
		{
		  hits_prompt = is_prompt_end(cur + i);
		  if (hits_prompt)
		    break;
		}
	      if (!hits_prompt)
		return(false);
	    }
	  else return(false);
	}
      break;

    case GDK_KEY_e:
      if (state & MetaMask)
	glistener_set_cursor_position(find_next_prompt() - 1);
      else return(false);
      break;
      
    case GDK_KEY_g: 
    case GDK_KEY_G:
      if (state & ControlMask)
	{
	  if (state & MetaMask)
	    clear_listener();
	  else control_g(any_selected_sound());
	}
      else return(false);
      break;

    case GDK_KEY_BackSpace:
      /* need to check for prompt at cursor */
      if (!is_prompt_end(glistener_cursor_position()))
	return(false);
      break;

    case GDK_KEY_k:
      if (state & ControlMask)
	{
	  /* select to line end, copy to clipboard, delete */
	  GtkTextIter beg, end;
	  GtkTextBuffer *buf;
	  buf = listener_buffer;
	  gtk_text_buffer_get_iter_at_mark(buf, &beg, gtk_text_buffer_get_mark(buf, "insert"));
	  end = beg;
	  gtk_text_iter_forward_to_line_end(&end); /* was forward_to_end! */
	  if (!gtk_text_iter_equal(&beg, &end))
	    {
	      gtk_text_buffer_select_range(buf, &beg, &end);
	      gtk_text_buffer_cut_clipboard(buf, gtk_widget_get_clipboard(w, GDK_SELECTION_CLIPBOARD), true);
	    }
	}
      else return(false);
      break;
      
    case GDK_KEY_l:
      if (state & MetaMask)
	word_upper(listener_text, false, false);
      else return(false);
      break;

    case GDK_KEY_n:
      if (state & MetaMask)
	{
	  clear_back_to_prompt(listener_text);
	  restore_listener_string(false);
	}
      else return(false);
      break;

    case GDK_KEY_p:
      if (state & MetaMask)
	{
	  clear_back_to_prompt(listener_text);
	  restore_listener_string(true);
	}
      else return(false);
      break;

    case GDK_KEY_t:
      if (state & ControlMask)
	{
	  pos = glistener_cursor_position();
	  if ((!is_prompt_end(pos)) &&
	      (!is_prompt_end(pos + prompt_length)))
	    text_transpose(listener_text);
	  else return(false);
	}
      else return(false);
      break;

    case GDK_KEY_u:
      if (state & MetaMask)
	word_upper(listener_text, false, true);
      else return(false);
      break;

    case GDK_KEY_w:
      if (state & ControlMask)
	{
	  GtkTextIter start, end;
	  bool has_selection;
	  has_selection = gtk_text_buffer_get_selection_bounds(listener_buffer, &start, &end);
	  if (!has_selection)
	    return(false);
	  if (gtk_text_iter_get_offset(&start) >= prompt_length)
	    return(false);
	}
      else return(false);
      break;

    case GDK_KEY_Tab:
      listener_completion(glistener_cursor_position());
      return(true);

    case GDK_KEY_Return:
      glistener_return_callback(listener_text);
      break;

    default: 
      return(false);
    }

  g_signal_stop_emission((gpointer)w, g_signal_lookup("key_press_event", G_OBJECT_TYPE((gpointer)w)), 0);
  return(false);
}


static gboolean glistener_key_release(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  int cpos, bpos;
  /* before doing anything, make sure we're not in the prompt! */

  cpos = glistener_cursor_position();
  if (cpos < prompt_length)
    glistener_set_cursor_position(prompt_length - 1);
  else
    {
      bpos = find_current_prompt();
      if (cpos < bpos)
	glistener_set_cursor_position(bpos);
    }
 
  /* and mark matching paren, if any */
  check_parens();
  return(false);
}


static int listener_insertion_position = 0;

static void text_insert(GtkTextBuffer *textbuffer, GtkTextIter *location, gchar *text, gint len, gpointer user_data)
{
  listener_insertion_position = gtk_text_iter_get_offset(location);
}


static gboolean glistener_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  if (EVENT_STATE(ev) & GDK_BUTTON2_MASK)
    glistener_set_cursor_position(listener_insertion_position);
  else
    {
      int cpos, bpos;
      /* before doing anything, make sure we're not in the prompt! */

      cpos = glistener_cursor_position();
      if (cpos < prompt_length)
	glistener_set_cursor_position(prompt_length - 1);
      else
	{
	  bpos = find_current_prompt();
	  if (cpos < bpos)
	    glistener_set_cursor_position(bpos);
	}
    }
  check_parens();

  /* an experiment */
#if 1
  {
    GtkTextIter s1, e1;
    int start = 0, end = 0;
    char *text;
    const char *help;
    gtk_text_buffer_get_iter_at_offset(listener_buffer, &s1, find_current_prompt());
    gtk_text_buffer_get_iter_at_offset(listener_buffer, &e1, find_next_prompt());
    find_surrounding_word(glistener_cursor_position(), is_delimiter_q, &start, &end, &s1, &e1);
    gtk_text_buffer_get_iter_at_offset(listener_buffer, &s1, start);
    gtk_text_buffer_get_iter_at_offset(listener_buffer, &e1, end);
    /*
    fprintf(stderr, "[%s], %d %d, %d %d\n", 
	    gtk_text_buffer_get_text(listener_buffer, &s1, &e1, true),
	    start, find_current_prompt(),
	    end, find_next_prompt());
    */
    text = gtk_text_buffer_get_text(listener_buffer, &s1, &e1, true);
    if (text)
      {
	help = glistener_helper(text);
	if (help)
	  post_status(help);
	g_free(text);
      }
  }
#endif

  return(false);
}


/* ---------------- new listener ---------------- */

static GdkCursor *wait_cursor = NULL, *arrow_cursor = NULL;

void glistener_set_wait_cursor_shape(GdkCursor *wait)
{
  wait_cursor = wait;
}

void glistener_set_normal_cursor_shape(GdkCursor *arrow)
{
  arrow_cursor = arrow;
}




#define SIGNAL_CONNECT(Widget, Signal, Function, Data) g_signal_connect(G_OBJECT(Widget), Signal, G_CALLBACK(Function), (gpointer)Data)
#define SIGNAL_CONNECT_AFTER(Widget, Signal, Function, Data) g_signal_connect_after(G_OBJECT(Widget), Signal, G_CALLBACK(Function), (gpointer)Data)

GtkWidget *glistener_new(GtkWidget *parent, void (*initializations)(GtkWidget *new_listener))
{
  GtkWidget *sw, *new_text, *vb, *sb;
  GtkTextBuffer *buf;

  sw = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(sw), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

  new_text = gtk_text_view_new();
  buf = gtk_text_buffer_new(NULL);

  gtk_text_view_set_buffer(GTK_TEXT_VIEW(new_text), buf);
  gtk_text_view_set_editable(GTK_TEXT_VIEW(new_text), true);
  gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(new_text), GTK_WRAP_NONE);
  gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(new_text), true);
  gtk_text_view_set_left_margin(GTK_TEXT_VIEW(new_text), 4);
  gtk_container_add(GTK_CONTAINER(sw), new_text);
  gtk_widget_set_events(new_text, GDK_ALL_EVENTS_MASK);

  listener_text = new_text;
  listener_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(listener_text));
  listener_scrolled_window = sw;

  glistener_set_font(pango_font_description_from_string("Monospace 11"));
  glistener_bindings(GTK_TEXT_VIEW_GET_CLASS(LISTENER_TEXT));

  if (initializations)
    initializations(listener_text);

  SIGNAL_CONNECT(listener_text, "key_press_event", glistener_key_press, NULL);
  SIGNAL_CONNECT(listener_text, "key_release_event", glistener_key_release, NULL);
  SIGNAL_CONNECT(listener_text, "button_release_event", glistener_button_release, NULL);
  SIGNAL_CONNECT_AFTER(listener_buffer, "insert-text", text_insert, NULL);

  if (!prompt)
    {
      prompt = calloc(3, sizeof(char));
      prompt[0] = '\n';
      prompt[1] = '>';
      prompt_length = g_utf8_strlen(prompt, -1);
    }

  /* put in the first prompt without the preceding <cr> */
  {
    GtkTextIter start;
    gtk_text_buffer_get_start_iter(listener_buffer, &start);
    prompt_insert(&start, true);
  }

  if (!wait_cursor) wait_cursor = gdk_cursor_new(GDK_WATCH);
  if (!arrow_cursor) arrow_cursor = gdk_cursor_new(GDK_LEFT_PTR);

#if (!HAVE_GTK_3)
  vb = gtk_table_new(2, 1, false);
  if (parent)
    gtk_container_add(GTK_CONTAINER(parent), vb);
  gtk_table_attach(GTK_TABLE(vb), sw, 0, 1, 0, 1, /* left right top bottom */
		   (GtkAttachOptions)(GTK_FILL | GTK_EXPAND), 
		   (GtkAttachOptions)(GTK_FILL | GTK_EXPAND | GTK_SHRINK), 
		   0, 0);

  sb = gtk_statusbar_new();
  gtk_table_attach(GTK_TABLE(vb), sb, 0, 1, 1, 2,
		   (GtkAttachOptions)(GTK_EXPAND | GTK_FILL), 
		   (GtkAttachOptions)(GTK_FILL), 
		   0, 0);
#else
  vb = gtk_grid_new();
  if (parent)
    gtk_container_add(GTK_CONTAINER(parent), vb);
  gtk_widget_set_halign(sw, GTK_ALIGN_FILL);
  gtk_widget_set_valign(sw, GTK_ALIGN_FILL);
  gtk_widget_set_hexpand(sw, TRUE);
  gtk_widget_set_vexpand(sw, TRUE);
  gtk_grid_attach(GTK_GRID(vb), sw, 0, 0, 1, 1); /* left top w h */
 
  sb = gtk_statusbar_new();
  gtk_widget_set_halign(sb, GTK_ALIGN_FILL); 
  gtk_grid_attach(GTK_GRID(vb), sb, 0, 1, 1, 1);
#endif

  listener_statusbar = sb;

  gtk_widget_show(new_text);
  gtk_widget_show(sw);
  gtk_widget_show(sb);
  gtk_widget_show(vb);

  return(vb); 
}




/* ---------------- <cr> evaluation ---------------- */

static void eval_text(char *text, int pos)
{
  /* fprintf(stderr, "eval %s\n", text); */
  if (text)
    {
      if (pos < gtk_text_buffer_get_char_count(listener_buffer))
	glistener_append_text(text);
      
      glistener_set_cursor_shape(wait_cursor);
      
      remember_listener_string(text);
      
      glistener_evaluator(text);

      g_free(text);
      glistener_set_cursor_shape(arrow_cursor); 
      glistener_set_cursor_position(gtk_text_buffer_get_char_count(listener_buffer));
    }
}


static int find_form_limits(int *bpos, int *epos)
{
  GtkTextIter cursor, scan, start, end;
  char *text;
  int pos;
  
  pos = glistener_cursor_position();
  if (pos < prompt_length - 1)
    *bpos = prompt_length - 1;
  else
    {
      gtk_text_buffer_get_iter_at_offset(listener_buffer, &cursor, pos + prompt_length - 1);
      if (!prompt_backward_search(&cursor, &start, &end))
	*bpos = prompt_length - 1;
      else *bpos = gtk_text_iter_get_offset(&start) + prompt_length;
    }
  
  pos = glistener_cursor(&cursor);
  if (!gtk_text_iter_forward_search(&cursor, prompt, 0, &start, &end, NULL))
    {
      gtk_text_buffer_get_end_iter(listener_buffer, &end);
      *epos = gtk_text_iter_get_offset(&end);
      /* fprintf(stderr, "no end prompt: %d %d\n", epos, gtk_text_buffer_get_char_count(listener_buffer)); */
    }
  else *epos = gtk_text_iter_get_offset(&start);
  /* now the expression is between bpos and epos */

  return(pos);
}


static void glistener_return_callback(GtkWidget *w)
{
  GtkTextIter scan, s1, s2, start, end;
  char *text;
  int pos, bpos, epos, oparen_pos, inner_bpos, inner_epos;
  gunichar c;
  
  remove_underline();
  pos = find_form_limits(&bpos, &epos);
#if 0
  {
    GtkTextIter a, b;
    gtk_text_buffer_get_iter_at_offset(listener_buffer, &a, bpos);
    gtk_text_buffer_get_iter_at_offset(listener_buffer, &b, epos);
    /* fprintf(stderr, "full: %s\n", gtk_text_buffer_get_text(listener_buffer, &a, &b, false)); */
  }
#endif
  
  if (bpos == epos) /* <cr> at end? */
    {
      glistener_append_text("\n");
      return;
    }
  
  gtk_text_buffer_get_iter_at_offset(listener_buffer, &end, epos);
  if (!find_not_whitespace(bpos - 1, &end))
    {
      glistener_append_text("\n");
      return;
    }


  gtk_text_buffer_get_iter_at_offset(listener_buffer, &start, bpos - 1);
  gtk_text_buffer_get_iter_at_offset(listener_buffer, &scan, pos - 1);

  {
    gtk_text_buffer_get_iter_at_offset(listener_buffer, &s1, bpos);
    gtk_text_iter_forward_find_char(&s1, is_not_whitespace, NULL, &end);
    oparen_pos = gtk_text_iter_get_offset(&s1);
    if (oparen_pos > pos)
      {
	/* fprintf(stderr, "cursor %d -> %d\n", pos, oparen_pos + 1); */
      pos = oparen_pos + 1;
      }
  }

  {
    /* fprintf(stderr, "first section (with prompt end): %s\n", gtk_text_buffer_get_text(listener_buffer, &start, &scan, false)); */
  }

  /* TODO: unmatched flash is off by 2 if (+ 1 2(
   * also wherever we send out newline, scroll to it
   * also don't forget ruby/forth
   */


  /* first look for easy cases:
   *   if all whitespace, send out newline
   *   if just atom (no open paren on left), eval and print result
   *   if close paren, but no matching open paren, post message and wait
   *   else set up the current expression limits (we're looking for the maximal form enclosing the cursor)
   */
  inner_bpos = pos - 1;
  inner_epos = pos;

  c = gtk_text_iter_get_char(&scan);
  if (c == ')')
    {
      if (!at_character_constant(pos - 1))
	{
	  if (!find_open_paren(1, pos - 2, &oparen_pos, &start))
	    {
	      /* fprintf(stderr, "%d: no (\n", __LINE__); */
	      add_inverse(pos - 1);
	      flashes = 4;
	      g_timeout_add_full(0, (guint32)FLASH_TIME, flash_unbalanced_paren, NULL, NULL);
	      post_status("unmatched ')'");
	      return;
	    }
	  inner_bpos = oparen_pos - 1;
	}
      else inner_bpos = pos - 3;
    }
  else
    {
      if (!find_open_paren(1, pos - 1, &oparen_pos, &start))
	{
	  /* not at ) and no ( */
	  gtk_text_buffer_get_iter_at_offset(listener_buffer, &start, bpos);
	  find_surrounding_word(pos, is_delimiter_q, &bpos, &pos, &start, &end);
	  if (bpos < pos)
	    {
	      gtk_text_buffer_get_iter_at_offset(listener_buffer, &start, bpos);
	      gtk_text_buffer_get_iter_at_offset(listener_buffer, &end, pos);
	      /* fprintf(stderr, "%d: ", __LINE__);  */
	      text = gtk_text_buffer_get_text(listener_buffer, &start, &end, false);
	      eval_text(text, pos);
	      g_free(text);
	      return;
	    }
	}
    }
#if 0
  {
    gtk_text_buffer_get_iter_at_offset(listener_buffer, &s1, inner_bpos);
    gtk_text_buffer_get_iter_at_offset(listener_buffer, &s2, inner_epos);
    fprintf(stderr, "%d: %d %d [%d %d], [%s]\n", __LINE__, inner_bpos, inner_epos, bpos, epos, gtk_text_buffer_get_text(listener_buffer, &s1, &s2, false));
  }
#endif

  while (bpos <= inner_bpos)
    {
      /* 3 cases: 
       *   no left open paren: send current expr to eval
       *   left paren but no matching close: warn, add newline
       *   left and right parens: expand inner expr bounds and try again
       */
      /* fprintf(stderr, "%d: %d %d (%d %d)\n", __LINE__, inner_bpos, inner_epos, bpos, epos); */
#if 0
  {
    gtk_text_buffer_get_iter_at_offset(listener_buffer, &s1, inner_bpos);
    gtk_text_buffer_get_iter_at_offset(listener_buffer, &s2, inner_epos);
    fprintf(stderr, "%d: %d %d [%d %d], %s\n", __LINE__, inner_bpos, inner_epos, bpos, epos, gtk_text_buffer_get_text(listener_buffer, &s1, &s2, false));
  }
#endif
      if (!find_open_paren(1, inner_bpos, &oparen_pos, &start))
	{
	  /* fprintf(stderr, "%d: no (\n", __LINE__); */
	  gtk_text_buffer_get_iter_at_offset(listener_buffer, &start, inner_bpos + 1);
	  gtk_text_buffer_get_iter_at_offset(listener_buffer, &end, inner_epos);
	  /* fprintf(stderr, "%d: \n", __LINE__);  */
	  text = gtk_text_buffer_get_text(listener_buffer, &start, &end, false);
	  eval_text(text, epos);
	  g_free(text);
	  return;
	}
      /* fprintf(stderr, "bpos %d -> %d\n", inner_bpos, oparen_pos); */
      inner_bpos = oparen_pos - 1;
#if 0
  {
    gtk_text_buffer_get_iter_at_offset(listener_buffer, &s1, inner_bpos);
    gtk_text_buffer_get_iter_at_offset(listener_buffer, &s2, inner_epos);
    fprintf(stderr, "%d: %d %d [%d %d], %s\n", __LINE__, inner_bpos, inner_epos, bpos, epos, gtk_text_buffer_get_text(listener_buffer, &s1, &s2, false));
  }
#endif      
      if (!find_close_paren(1, inner_epos, &oparen_pos, &end))
	{
	  /* fprintf(stderr, "%d: no ) [%d %d]\n", __LINE__, inner_epos, epos); */
	  add_inverse(inner_bpos + 1);
	  flashes = 4;
	  g_timeout_add_full(0, (guint32)FLASH_TIME, flash_unbalanced_paren, NULL, NULL);
	  post_status("unmatched '('");
	  glistener_append_text("\n");
	  return;
	}
      /* fprintf(stderr, "epos %d -> %d\n", inner_epos, oparen_pos); */
      inner_epos = oparen_pos + 1;
    }

  if (inner_bpos < bpos) inner_bpos = bpos;
  gtk_text_buffer_get_iter_at_offset(listener_buffer, &start, inner_bpos);
  gtk_text_buffer_get_iter_at_offset(listener_buffer, &end, inner_epos);
  /* fprintf(stderr, "%d: \n", __LINE__);  */
  text = gtk_text_buffer_get_text(listener_buffer, &start, &end, false);
  eval_text(text, epos);
}





/* ---------------- <tab> completion and indentation ---------------- */

/* realpath eqv in glib? -- not available yet (many complaints ...)
 * TODO: show in status why not complete
 */

static char *filename_completion(const char *partial_name)
{
  char *file_name = NULL, *directory_name = NULL, *temp, *new_name = NULL, *slash, *current_match = NULL, *result;
  const char *rname;
  int j, len, flen, matches = 0;
  GDir *dir;

  if (partial_name[0] == '~')
    {
      const char *home;
      home = g_getenv("HOME");
      if (home)
	{
	  new_name = (char *)calloc(strlen(partial_name) + strlen(home) + 1, sizeof(char));
	  strncpy(new_name, home, strlen(home));
	  strcat(new_name, (char *)(partial_name + 1));
	}
    }
  if (!new_name)
    {
      new_name = (char *)calloc(strlen(partial_name) + 1, sizeof(char));
      strcpy(new_name, partial_name);
    }

  slash = g_utf8_strrchr(new_name, -1, (gunichar)'/');
  if (slash)
    {
      len = slash - new_name + 1;
      temp = (char *)malloc(len * sizeof(char));
      file_name = (char *)(new_name + len);
      strncpy(temp, new_name, len);
      temp[len - 1] = '\0';
      directory_name = realpath(temp, NULL);
      free(temp);
    }
  else
    {
      file_name = (char *)partial_name;
      directory_name = g_get_current_dir();
    }
  if (file_name)
    flen = strlen(file_name);
  else return(NULL);

  if ((directory_name) && (file_name))
    {
      dir = g_dir_open(directory_name, 0, NULL);
      while (rname = g_dir_read_name(dir))
	{
	  if (strncmp(rname, file_name, flen) == 0)
	    {
	      if (current_match == NULL)
		{
		  len = strlen(rname);
		  current_match = (char *)calloc(len + 2, sizeof(char));
		  strcpy(current_match, rname);
		}
	      else 
		{
		  matches++;
		  for (j = 0; j < len; j++)
		    if (current_match[j] != rname[j])
		      {
			current_match[j] = '\0';
			len = j;
			if (len <= flen)
			  {
			    /* can't extend current name because of ambiguous matches, so give up */
			    g_dir_close(dir);
			    if (directory_name) free(directory_name);
			    if (new_name) free(new_name);
			    return(NULL);
			  }
			break;
		      }
		}
	    }
	}
      if (dir) g_dir_close(dir);
    }

  if (len == flen)
    result = NULL;
  else
    {
      result = current_match;
      if ((slash) &&
	  (current_match))
	{
	  /* attach matched portion to user's indication of dir */
	  result = (char *)calloc(strlen(partial_name) + strlen(current_match) + 3, sizeof(char));
	  temp = g_utf8_strrchr(partial_name, -1, (gunichar)'/');
	  strncpy(result, partial_name, temp - partial_name + 1);
	  strcat(result, current_match);
	  free(current_match);
	}
      if (matches == 0)
	strcat(result, "\"");
    }
  if (directory_name) free(directory_name);
  if (new_name) free(new_name);
  return(result);
}


typedef struct {
  const char *text;
  char *current_match;
  int len, tlen;
} match_info;

static bool compare_names(const char *symbol_name, void *data)
{
  match_info *m = (match_info *)data;
  if (strncmp(m->text, symbol_name, m->tlen) == 0)
    {
      if (m->current_match == NULL)
	{
	  m->len = strlen(symbol_name);
	  m->current_match = (char *)calloc(m->len + 1, sizeof(char));
	  strcpy(m->current_match, symbol_name);
	}
      else 
	{
	  int j;
	  for (j = 0; j < m->len; j++)
	    if (m->current_match[j] != symbol_name[j])
	      {
		m->current_match[j] = '\0';
		m->len = j;
		break;
	      }
	}
    }
  return((m->len > 0) && (m->len <= m->tlen));
}

static char *symbol_completion(const char *text)
{
  match_info *m;
  char *result = NULL;
  m = (match_info *)calloc(1, sizeof(match_info));
  m->text = text;
  m->tlen = strlen(text);
  m->len = 0;
  m->current_match = NULL;
  glistener_symbol_completer(compare_names, (void *)m);
  if (m->len > m->tlen)
    result = m->current_match;
  else 
    {
      if (m->current_match) 
	free(m->current_match);
    }
  free(m);
  return(result);
}


static void listener_completion(int pos)
{
  #define INDENT_SPACES_LENGTH 80
  static char *indent_spaces = "                                                                                ";
  
  /* <whitespace><tab><any> -> indent (no-op if on prompt line and at end?) [tmp has old indentation code]
   * <text><tab><whitespace> -> try to complete
   * <text><tab><text> -> ???
   *
   * in check_parens, if paren found, check expr for 
   *   undefined vars, refedined globals, run lint for other errors
   *
   * when key release, also perhaps look for possible completions (with list if > 1) 
   */

  {
    char *text, *new_name;
    bool in_string = false;

    text = get_preceding_text(pos, &in_string);
    /* fprintf(stderr, "text: [%s %d] %d\n", text, mus_strlen(text), in_string); */
    if ((text) && (*text))
      {
	if (!in_string)
	  new_name = symbol_completion(text);
	else new_name = filename_completion(text);
	if (new_name)
	  {
	    int old_len, new_len;
	    old_len = strlen(text);
	    new_len = strlen(new_name);
	    gtk_text_buffer_insert_at_cursor(listener_buffer, (char *)(new_name + old_len), new_len - old_len);
	    free(new_name);
	  }
	g_free(text);
      }
    else
      {
	/* here we're indenting */
	static const char indent_spaces[] = "                                                                                ";
	#define INDENT_SPACES 80

	int pos, bpos, epos, bline, eline, cline, linepos, linecol;
	GtkTextIter cursor, curline, start_limit, end_limit;

	pos = find_form_limits(&bpos, &epos);
	gtk_text_buffer_get_iter_at_offset(listener_buffer, &start_limit, bpos - 1);
	gtk_text_buffer_get_iter_at_offset(listener_buffer, &end_limit, epos);
	gtk_text_buffer_get_iter_at_offset(listener_buffer, &cursor, pos);
	bline = gtk_text_iter_get_line(&start_limit);
	eline = gtk_text_iter_get_line(&end_limit);
	cline = gtk_text_iter_get_line(&cursor);
	gtk_text_buffer_get_iter_at_line(listener_buffer, &curline, cline);
	linepos = gtk_text_iter_get_offset(&curline);
	linecol = gtk_text_iter_get_line_offset(&cursor);

	/* fprintf(stderr, "%d %d %d %d %d\n", bline, cline, eline, linepos, linecol); */

	if ((bline == cline) ||
	    (find_not_whitespace(linepos, &cursor)))
	  glistener_append_text("    ");
	else
	  {
	    GtkTextIter paren;
	    int oparen_pos, oparen_col;
	    if (!find_open_paren(1, pos, &oparen_pos, &start_limit))
	      glistener_append_text("    ");
	    else
	      {
		gtk_text_buffer_get_iter_at_offset(listener_buffer, &paren, oparen_pos);
		oparen_col = gtk_text_iter_get_line_offset(&paren);
#if 0
		fprintf(stderr, "in %s found unmatched at %d, %d %d, %c\n",
			gtk_text_buffer_get_text(listener_buffer, &start_limit, &end_limit, false),
			oparen_pos,
			linecol, oparen_col,
			gtk_text_iter_get_char(&paren));
#endif
		/* we're at linecol, it's at oparen_col
		 */
		if (oparen_col > linecol)
		  {
		    int cols;
		    cols = INDENT_SPACES - oparen_col + linecol;

		    /* now see what follows the unmatched ( */
		    gtk_text_iter_forward_char(&paren);

		    /* fprintf(stderr, "p1: %c, cols: %d\n", gtk_text_iter_get_char(&paren), cols); */

		    if (gtk_text_iter_get_char(&paren) == '(')
		      glistener_append_text((const char *)(indent_spaces + cols - 1));
		    else
		      {
			GtkTextIter s1, e1;
			int start = 0, end = 0;
			char *text;

			glistener_append_text((const char *)(indent_spaces + cols - 2)); /* default indentation */

			gtk_text_buffer_get_iter_at_offset(listener_buffer, &start_limit, oparen_pos);
			gtk_text_buffer_get_iter_at_offset(listener_buffer, &end_limit, epos);
			find_surrounding_word(oparen_pos + 1, is_delimiter, &start, &end, &start_limit, &end_limit);
			gtk_text_buffer_get_iter_at_offset(listener_buffer, &s1, start);
			gtk_text_buffer_get_iter_at_offset(listener_buffer, &e1, end);
			text = gtk_text_buffer_get_text(listener_buffer, &s1, &e1, true);
			/* fprintf(stderr, "car: %s\n", text); */

			if (text)
			  {
			    if (strcmp(text, "or") == 0)
			      glistener_append_text("  ");
			    else
			      {
				if (strcmp(text, "and") == 0)
				  glistener_append_text("   ");
				else
				  {
				    if (strcmp(text, "cond") == 0)
				      glistener_append_text("    ");
				    else
				      {
					/* TODO: do and if need expr counts */
				      }
				  }
			      }
			    g_free(text);
			  }
			/* TODO: remember g_free of get_text!
			 * TODO: if trailing white space, no indentation of cursor?
			 */
		      }
		  }
	      }
	  }
      }
  }
}
