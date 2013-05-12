#include "snd.h"


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



static GtkWidget *listener_text = NULL;

#define LISTENER_TEXT GTK_TEXT_VIEW(listener_text)
#define LISTENER_BUFFER gtk_text_view_get_buffer(LISTENER_TEXT)


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



/* ---------------- cursor ---------------- */

static void glistener_set_cursor_position(int position)
{
  GtkTextIter pos;
  GtkTextBuffer *buf;

  buf = LISTENER_BUFFER;
  gtk_text_buffer_get_iter_at_offset(buf, &pos, position);
  gtk_text_buffer_place_cursor(buf, &pos);
  gtk_text_view_scroll_mark_onscreen(LISTENER_TEXT, gtk_text_buffer_get_insert(buf));
}


static int glistener_cursor_position(void)
{
  GtkTextIter pos;
  GtkTextBuffer *buf;
  buf = LISTENER_BUFFER;
  gtk_text_buffer_get_iter_at_mark(buf, &pos, gtk_text_buffer_get_insert(buf));
  return(gtk_text_iter_get_offset(&pos));
}

static int glistener_cursor(GtkTextIter *cursor)
{
  gtk_text_buffer_get_iter_at_mark(LISTENER_BUFFER, cursor, gtk_text_buffer_get_insert(LISTENER_BUFFER));
  return(gtk_text_iter_get_offset(cursor));
}


static void glistener_set_cursor_shape(GdkCursor *cursor_shape)
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
	gtk_text_buffer_insert_with_tags(LISTENER_BUFFER, pos, (char *)(prompt + 1), -1, prompt_tag, NULL);
      else gtk_text_buffer_insert(LISTENER_BUFFER, pos, (char *)(prompt + 1), -1);
    }
  else
    {
      if (prompt_tag)
	gtk_text_buffer_insert_with_tags(LISTENER_BUFFER, pos, prompt, -1, prompt_tag, NULL);
      else gtk_text_buffer_insert(LISTENER_BUFFER, pos, prompt, -1);
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
      gtk_text_buffer_get_start_iter(LISTENER_BUFFER, &start);
      gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &end, old_prompt_length - 1);
      gtk_text_buffer_delete(LISTENER_BUFFER, &start, &end);
      prompt_insert(&start, true);

      gtk_text_buffer_get_start_iter(LISTENER_BUFFER, &scan);
      while (gtk_text_iter_forward_search(&scan, old_prompt, 0, &start, &end, NULL))
	{
	  /* TODO: create mark once, move it thereafter? */
	  gtk_text_buffer_create_mark(LISTENER_BUFFER, "prompt_pos", &end, false); /* false -> "right gravity" */
	  gtk_text_buffer_delete(LISTENER_BUFFER, &start, &end);
	  prompt_insert(&start, false);
	  gtk_text_buffer_get_iter_at_mark(LISTENER_BUFFER, &scan, gtk_text_buffer_get_mark(LISTENER_BUFFER, "prompt_pos"));
	}
      free(old_prompt);
    }
}


static void glistener_append_prompt(void)
{
  if (listener_text)
    {
      GtkTextIter end;
      gtk_text_buffer_get_end_iter(LISTENER_BUFFER, &end);
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
  gtk_text_buffer_get_start_iter(LISTENER_BUFFER, &scan1);
  gtk_text_buffer_get_start_iter(LISTENER_BUFFER, &scan2);

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

  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &it, pos + prompt_length - 1);
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

  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &it, pos);
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
    gtk_text_buffer_get_end_iter(LISTENER_BUFFER, &end);
  return(gtk_text_iter_get_offset(&end) + 1);
}


static bool is_prompt_end(int end_pos)
{
  /* the prompt includes the preceding <cr> */
  GtkTextIter start, end;
  if (end_pos < prompt_length)
    return(true);
  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &end, end_pos);
  start = end;
  if (gtk_text_iter_backward_chars(&start, prompt_length))
    {
      char *txt;
      txt = gtk_text_iter_get_text(&start, &end);
      return(mus_strcmp(txt, prompt));
    }
  return(false);
}





/* ---------------- listener text ---------------- */

static char **listener_strings = NULL;
static int listener_strings_size = 0, listener_strings_pos = 0;
static bool listener_first_time = true;

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
	append_listener_text(-1, str); 
    }
}

void glistener_clear(void)
{
  if (listener_text)
    {
      GtkTextIter start, end;
      GtkTextBuffer *buf;
      buf = LISTENER_BUFFER;
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
  
  buf = LISTENER_BUFFER;
  gtk_text_buffer_get_start_iter(buf, &start);
  gtk_text_buffer_get_end_iter(buf, &end);
  str = gtk_text_buffer_get_text(buf, &start, &end, true);
  if (str)
    {
      fwrite((void *)str, sizeof(char), mus_strlen(str), fp);
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
      gtk_text_buffer_get_end_iter(LISTENER_BUFFER, &end);
      gtk_text_buffer_insert(LISTENER_BUFFER, &end, (char *)msg, -1);
    }
}


void glistener_scroll_to_end(void)
{
  if (listener_text)
    {
      GtkTextIter end;
      gtk_text_buffer_get_end_iter(LISTENER_BUFFER, &end);
      gtk_text_buffer_place_cursor(LISTENER_BUFFER, &end);
      gtk_text_view_scroll_mark_onscreen(LISTENER_TEXT, gtk_text_buffer_get_insert(LISTENER_BUFFER));
      /* TODO: also scroll left all the way, same for new prompt 
       *  gtk_adjustment_set_value(GTK_ADJUSTMENT(gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDWO(gtk_widget_get_parent(listener_text)))), 0.0)?
       */
    }
}



/* ---------------- paren matching ---------------- */

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
  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &scan, pos);  
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

  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &scan, pos);  
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

  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &scan, pos);  
  gtk_text_iter_backward_find_char(&scan, is_block_comment, &last_cs, limit);
  return(gtk_text_iter_get_offset(&scan));
}


static int find_close_block_comment(int pos, GtkTextIter *limit)
{
  GtkTextIter scan;
  int last_cs = 0;

  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &scan, pos);  
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


static void find_surrounding_word(int pos, int *start_pos, int *end_pos, GtkTextIter *start_limit, GtkTextIter *end_limit)
{
  GtkTextIter start, end;

  *start_pos = gtk_text_iter_get_offset(start_limit);
  *end_pos = gtk_text_iter_get_offset(end_limit);

  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &start, pos);
  end = start;
  if (!gtk_text_iter_equal(start_limit, &start))
    {
      if (gtk_text_iter_backward_find_char(&start, is_delimiter, NULL, start_limit))
	*start_pos = gtk_text_iter_get_offset(&start) + 1;
    }
  if (!gtk_text_iter_equal(&end, end_limit))
    {
      if (gtk_text_iter_forward_find_char(&end, is_delimiter, NULL, end_limit))
	*end_pos = gtk_text_iter_get_offset(&end);
    }
}


static bool at_character_constant(int end_pos)
{
  GtkTextIter start, end;
  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &end, end_pos);
  start = end;
  if (gtk_text_iter_backward_chars(&start, 2))
    {
      char *txt;
      bool result;
      txt = gtk_text_iter_get_text(&start, &end);
      /* fprintf(stderr, "char: [%s: %c%c]\n", txt, txt[0], txt[1]); */
      result = (mus_strcmp(txt, "#\\"));
      g_free(txt);
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

  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &scan, pos);  
  while (!gtk_text_iter_equal(limit, &scan))
    {
      last_c = c;
      c = gtk_text_iter_get_char(&scan);
      if (c == (gunichar)'\"') 
	{
	  ppos = find_string_start(gtk_text_iter_get_offset(&scan), limit);
	  /* TODO: if no matching quote then we're currently in a string? */
	  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &scan, ppos);
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
			  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &scan, ppos);
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

  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &scan, pos);  
  while (!gtk_text_iter_equal(&scan, limit))
    {
      prev_prev_c = prev_c;
      prev_c = c;
      c = gtk_text_iter_get_char(&scan);
      if ((prev_c != '\\') || (prev_prev_c != '#'))
	{
	  if (c == (gunichar)'\"')
	    {
	      ppos = find_string_end(gtk_text_iter_get_offset(&scan), limit);
	      gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &scan, ppos);
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
		      gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &scan, ppos);
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

static void add_inverse(int pos)
{
  GtkTextIter start, end;
  GtkTextBuffer *buf;
  buf = LISTENER_BUFFER;
  gtk_text_buffer_get_iter_at_offset(buf, &start, pos);
  gtk_text_buffer_get_iter_at_offset(buf, &end, pos + 1);
  if (!flash_tag) flash_tag = gtk_text_buffer_create_tag(buf, "red_background", "background", "red", NULL);
  gtk_text_buffer_apply_tag(buf, flash_tag, &start, &end);
}


static void remove_inverse(int pos)
{
  GtkTextIter start, end;
  GtkTextBuffer *buf;
  buf = LISTENER_BUFFER;
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
  buf = LISTENER_BUFFER;
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
      buf = LISTENER_BUFFER;
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
  flash_paren_pos = pos;
  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &scan, pos - 1);

  c = gtk_text_iter_get_char(&scan);
  if (c == ')')
    {
      gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &limit, find_current_prompt() - 1);
      if (find_open_paren(1, pos - 2, &pos, &limit))
	add_underline(pos, pos + 1);
    }
  else
    {
      gtk_text_iter_forward_char(&scan);
      c = gtk_text_iter_get_char(&scan);
      if (c == '(')
	{
	  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &limit, find_next_prompt());
	  if (find_close_paren(1, pos + 1, &pos, &limit))
	    add_underline(pos, pos + 1);
	}
#if 0 
/* TODO: if there's help and there's a status area, post it?
 *       also the "loading..." message in the same area
 */
      else
	{

	  GtkTextIter s1, e1;
	  int start = 0, end = 0;
	  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &s1, find_current_prompt());
	  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &e1, find_next_prompt());
	  find_surrounding_word(glistener_cursor_position(), &start, &end, &s1, &e1);
	  if (start < end)
	    add_underline(start, end);
	}
#endif
    }
}



/* ---------------- text editing ---------------- */

static void word_upper(GtkWidget *w, bool capitalize, bool upcase)
{
  int pos;
  GtkTextIter start, end;
  char *text;

  pos = glistener_cursor_position();
  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &start, pos);
  end = start;
  gtk_text_iter_forward_word_end(&end);
  text = gtk_text_buffer_get_text(LISTENER_BUFFER, &start, &end, true);
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
      gtk_text_buffer_delete(LISTENER_BUFFER, &start, &end);
      gtk_text_buffer_insert(LISTENER_BUFFER, &start, text, -1);
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
      text = gtk_text_buffer_get_text(LISTENER_BUFFER, &start, &end, false);
      new_text = g_utf8_strreverse(text, -1);
      gtk_text_buffer_delete(LISTENER_BUFFER, &start, &end);
      gtk_text_buffer_insert(LISTENER_BUFFER, &start, new_text, -1);
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

  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &start, beg);
  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &last, end); 
  gtk_text_buffer_delete(LISTENER_BUFFER, &start, &last);
}




/* ---------------- key bindings ---------------- */

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


/* TODO: fix these! */
static void glistener_return_callback(GtkWidget *w);
static void listener_completion(int end);
static void text_at_cursor(GtkWidget *w);


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
      if (state & snd_MetaMask)
	glistener_set_cursor_position(find_previous_prompt(glistener_cursor_position()));
      else return(false);
      break;
      
    case GDK_KEY_c:
      if (state & snd_MetaMask)
	word_upper(listener_text, true, false);
      else return(false);
      break;

    case GDK_KEY_d:
      if (state & snd_ControlMask)
	{
	  /* need to check for prompt just ahead */
	  if (!is_prompt_end(glistener_cursor_position() + prompt_length))
	    return(false);
	  /* else we're sitting at (just in front of) the prompt so drop through and block the signal */
	}
      else
	{
	  if (state & snd_MetaMask)
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
      if (state & snd_MetaMask)
	glistener_set_cursor_position(find_next_prompt() - 1);
      else return(false);
      break;
      
    case GDK_KEY_g: 
    case GDK_KEY_G:
      if (state & snd_ControlMask)
	{
	  if (state & snd_MetaMask)
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
      if (state & snd_ControlMask)
	{
	  /* select to line end, copy to clipboard, delete */
	  GtkTextIter beg, end;
	  GtkTextBuffer *buf;
	  buf = LISTENER_BUFFER;
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
      if (state & snd_MetaMask)
	word_upper(listener_text, false, false);
      else return(false);
      break;

    case GDK_KEY_n:
      if (state & snd_MetaMask)
	{
	  clear_back_to_prompt(listener_text);
	  restore_listener_string(false);
	}
      else return(false);
      break;

    case GDK_KEY_p:
      if (state & snd_MetaMask)
	{
	  clear_back_to_prompt(listener_text);
	  restore_listener_string(true);
	}
      else return(false);
      break;

    case GDK_KEY_t:
      if (state & snd_ControlMask)
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
      if (state & snd_MetaMask)
	word_upper(listener_text, false, true);
      else return(false);
      break;

    case GDK_KEY_w:
      if (state & snd_ControlMask)
	{
	  GtkTextIter start, end;
	  bool has_selection;
	  has_selection = gtk_text_buffer_get_selection_bounds(LISTENER_BUFFER, &start, &end);
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

    case GDK_KEY_question:
      if (state & snd_ControlMask)
	text_at_cursor(listener_text);
      else return(false);
      break;

    case GDK_KEY_period:
      if (state & snd_MetaMask)
	text_at_cursor(listener_text);
      else return(false);
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

#if 0
  {
    GtkTextIter s1, e1;
    int start = 0, end = 0;
    gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &s1, find_current_prompt());
    gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &e1, find_next_prompt());
    find_surrounding_word(glistener_cursor_position(), &start, &end, &s1, &e1);
    gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &s1, start);
    gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &e1, end);
    fprintf(stderr, "[%s], %d %d, %d %d\n", 
	    gtk_text_buffer_get_text(LISTENER_BUFFER, &s1, &e1, true),
	    start, find_current_prompt(),
	    end, find_next_prompt());
  }
#endif

  return(false);
}


/* ---------------- new listener ---------------- */

#define SIGNAL_CONNECT(Widget, Signal, Function, Data) g_signal_connect(G_OBJECT(Widget), Signal, G_CALLBACK(Function), (gpointer)Data)
#define SIGNAL_CONNECT_AFTER(Widget, Signal, Function, Data) g_signal_connect_after(G_OBJECT(Widget), Signal, G_CALLBACK(Function), (gpointer)Data)

GtkWidget *glistener_new(GtkWidget *parent, void (*initializations)(GtkWidget *new_listener))
{
  GtkWidget *sw, *new_text;
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
  if (parent)
    gtk_container_add(GTK_CONTAINER(parent), sw);

  listener_text = new_text;

  glistener_set_font(pango_font_description_from_string("Monospace 11"));
  glistener_bindings(GTK_TEXT_VIEW_GET_CLASS(LISTENER_TEXT));

  if (initializations)
    initializations(listener_text);

  SIGNAL_CONNECT(listener_text, "key_press_event", glistener_key_press, NULL);
  SIGNAL_CONNECT(listener_text, "key_release_event", glistener_key_release, NULL);
  SIGNAL_CONNECT(listener_text, "button_release_event", glistener_button_release, NULL);
  SIGNAL_CONNECT_AFTER(LISTENER_BUFFER, "insert-text", text_insert, NULL);

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
    gtk_text_buffer_get_start_iter(LISTENER_BUFFER, &start);
    prompt_insert(&start, true);
  }

  gtk_widget_show(new_text);
  gtk_widget_show(sw);

  return(sw);  /* gtk_bin_get_child(GTK_BIN(sw)) -> listener_text(?) */
}



/* ---------------- <tab> completion ---------------- */


/* ---------------- <tab> indentation ---------------- */


/* ---------------- <cr> evaluation ---------------- */


/* ---------------- help ---------------- */





/* -------------------------------------------------------------------------------- */


/* not rewritten yet */

static void sg_text_insert_at_pos(GtkWidget *w, int start, char *text)
{
  GtkTextIter pos;
  GtkTextBuffer *buf;
  buf = LISTENER_BUFFER;
  gtk_text_buffer_get_iter_at_offset(buf, &pos, start);
  gtk_text_buffer_insert(buf, &pos, text, -1);
}


static bool is_prompt(const char *str, int beg)
{
  int i, j;
  /* fprintf(stderr, "check %s %s for %d at %d\n", str, ss->Listener_Prompt, ss->listener_prompt_length, beg); */

  for (i = beg, j = ss->listener_prompt_length - 1; (i >= 0) && (j >= 0); i--, j--)
    if (str[i] != ss->Listener_Prompt[j])
      {
	/* fprintf(stderr, "%c != %c at %d\n", str[i], ss->Listener_Prompt[j], j); */
      return(false);
      }
  if (j != -1) 
    {
      /* fprintf(stderr, "j: %d\n", j); */
    return(false);
    }
  if ((i == -1) || (str[i] == '\n'))
    {
      /* fprintf(stderr, "found prompt!\n"); */
    return(true);
    }
  /* fprintf(stderr, "i: %d, str[i]: %c\n", i, str[i]); */
  return(false);
}



static bool whitespace_p(char c)
{
  return((c == ' ') ||
	 (c == '\n') ||
	 (c == '\r') ||
	 (c == '\t'));
}


static void listener_completion(int end)
{
  #define INDENT_SPACES_LENGTH 80
  static char *indent_spaces = "                                                                                ";
  int beg;
  char *old_text = NULL, *previous_text = NULL;

  /* if previous char was whitespace, treat as indentation */
  beg = find_current_prompt(); /* TODO: actually, just go back to line start in the previous line */

  if (end <= beg) return;
  old_text = sg_get_text(listener_text, beg - 1, end);
  /* now old_text is the stuff typed since the last prompt */
  /* fprintf(stderr, "completion %s\n", old_text); */

  previous_text = sg_get_text(listener_text, end - 1, end);
  /* fprintf(stderr, "old(%d): [%c]\n", end, previous_text[0]); */

  if (whitespace_p(previous_text[0]))
    {
      /* look back for \n, on that line look for complete expr closest to \n, if none go back another
       *   back up to key? or align with it?
       */
      int k, len, parens = 0, wpos = 0, crs = 0, exprs = 0;
      bool in_whitespace;

      len = mus_strlen(old_text);
      in_whitespace = whitespace_p(old_text[len - 1]);

      for (k = len - 1; k >= 0; k--)
	{
	  if (whitespace_p(old_text[k]))
	    {
	      if (parens == 0)
		{
		  if (!in_whitespace) exprs++;
		  in_whitespace = true;
		}
	      wpos = k;
	      if (old_text[k] == '\n')
		crs++;
	    }
	  else
	    {
	      if (crs == 0)
		{
		  sg_text_insert_at_pos(listener_text, end, "    ");
		  return;
		}
	      /* lots of memory leaks throughout... */
	      if (old_text[k] == '(')
		{
		  parens--;
		  if (parens < 0) break;
		  if (parens == 0) 
		    {
		      exprs++;
		      in_whitespace = true;
		    }
		}
	      else
		{
		  if (old_text[k] == ')')
		    parens++;
		}
	    }
	}
      if (parens == 0)
	sg_text_insert_at_pos(listener_text, end, "    ");
      else
	{
	  int j, col = 0;
	  /* we have an unmatched open paren at k, next whitespace at wpos */
	  if (crs == 0) return;
	  for (j = k - 1; j >= 0; j--)
	    if (old_text[j] == '\n')
	      {
		col = k - j;
		break;
	      }

	  if (col == 0)
	    col = ss->listener_prompt_length;


	  /* fprintf(stderr, "check forward from %d to %d %s, exprs: %d, crs: %d, column: %d\n", k, wpos, (char *)(old_text + k), exprs, crs, col); 
	   */
	  /*
	   * TODO: exprs misses non-paren cases
	   */
	  /* fprintf(stderr, "k: %d, col: %d\n", k, col); */

	  if (old_text[k + 1] == '(') /* got ((...) */
	    sg_text_insert_at_pos(listener_text, end, (char *)(indent_spaces + INDENT_SPACES_LENGTH - col - k - 1));
	  else
	    {
	      switch (wpos - k)
		{
		case 3:
		  if (strncmp((const char *)(old_text + k + 1), "if", 2) == 0)
		    {
		      if (exprs < 2)
			sg_text_insert_at_pos(listener_text, end, (char *)(indent_spaces + INDENT_SPACES_LENGTH - col - 4));
		      else sg_text_insert_at_pos(listener_text, end, (char *)(indent_spaces + INDENT_SPACES_LENGTH - col - 2));
		    }
		  else
		    {
		      if (strncmp((const char *)(old_text + k + 1), "do", 2) == 0)
			{
			  /* line up steppers also */
			  if (exprs < 2)
			    sg_text_insert_at_pos(listener_text, end, (char *)(indent_spaces + INDENT_SPACES_LENGTH - col - 4));
			  else sg_text_insert_at_pos(listener_text, end, (char *)(indent_spaces + INDENT_SPACES_LENGTH - col - 2));
			}
		      else
			{
			  if (strncmp((const char *)(old_text + k + 1), "or", 2) == 0)
			    sg_text_insert_at_pos(listener_text, end, (char *)(indent_spaces + INDENT_SPACES_LENGTH - col - 4));
			  else sg_text_insert_at_pos(listener_text, end, (char *)(indent_spaces + INDENT_SPACES_LENGTH - col - 2));
			}
		    }
		  break;
		  
		case 4:
		  if (strncmp((const char *)(old_text + k + 1), "let", 3) == 0) /* we're in the body? */
		    sg_text_insert_at_pos(listener_text, end, (char *)(indent_spaces + INDENT_SPACES_LENGTH - col - 2));
		  else
		    {
		      if (strncmp((const char *)(old_text + k + 1), "and", 3) == 0)
			sg_text_insert_at_pos(listener_text, end, (char *)(indent_spaces + INDENT_SPACES_LENGTH - col - 5));
		      else sg_text_insert_at_pos(listener_text, end, (char *)(indent_spaces + INDENT_SPACES_LENGTH - col - 2));
		    }
		  break;
		  
		case 5:
		  if (strncmp((const char *)(old_text + k + 1), "cond", 4) == 0)
		    sg_text_insert_at_pos(listener_text, end, (char *)(indent_spaces + INDENT_SPACES_LENGTH - col - 6));
		  else
		    {
		      if (strncmp((const char *)(old_text + k + 1), "case", 4) == 0)
			sg_text_insert_at_pos(listener_text, end, (char *)(indent_spaces + INDENT_SPACES_LENGTH - col - 2));
		      else
			{
			  if (strncmp((const char *)(old_text + k + 1), "let*", 4) == 0) /* we're in the body? -- make this the default */
			    sg_text_insert_at_pos(listener_text, end, (char *)(indent_spaces + INDENT_SPACES_LENGTH - col - 2));
			  else sg_text_insert_at_pos(listener_text, end, (char *)(indent_spaces + INDENT_SPACES_LENGTH - col - 2));
			}
		    }
		  break;

		default:
		  sg_text_insert_at_pos(listener_text, end, (char *)(indent_spaces + INDENT_SPACES_LENGTH - col - 2));
		  break;
		}
	    }
	}
      if (old_text) g_free(old_text);
      if (previous_text) g_free(previous_text);
      return;
    }


  if (old_text)
    {
      char *new_text = NULL, *file_text = NULL;
      bool try_completion = true;

      new_text = complete_listener_text(old_text, end, &try_completion, &file_text);
      /* fprintf(stderr, "completion: %s %d\n", new_text, try_completion); */

      /* snd-completion:
       *  filename-completer
       *  completions (s7_for_each_symbol)
       */

      if (try_completion)
	{
	  if (mus_strcmp(old_text, new_text))
	    get_completion_matches();
	  {
	    int old_len, new_len;
	    old_len = end - beg + 1;
	    new_len = mus_strlen(new_text);
	    if (new_len > old_len)
	      {
		/* fprintf(stderr, "completion: %d to %d\n", old_len, new_len); */
		sg_text_insert_at_pos(listener_text, end, (char *)(new_text + old_len));
	      }
	  }
	  goto_window(listener_text);
	}
      else
	{
	  /* tab to indent? */
	  sg_text_insert_at_pos(listener_text, end, "    ");
	}

      if (new_text) free(new_text); 
      if (file_text) free(file_text);
      g_free(old_text);
    }
  if (previous_text) g_free(previous_text);  
}


static void listener_help_at_cursor(char *buf, int name_curpos, int len, int prompt_pos)
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

static void text_at_cursor(GtkWidget *w)
{
  int curpos, endpos, start, end, len, prompt_pos;
  char *buf;

  curpos = sg_cursor_position(w);
  if (curpos <= 1)
    {
      snd_help("Listener", "This is the 'listener', a text widget in which you can interact with Snd's extension language.  See extsnd.html.", WITH_WORD_WRAP);
      return;
    }

  prompt_pos = find_current_prompt();

  if (curpos > 40)
    start = curpos - 40;
  else start = 0;
  if (start < prompt_pos)
    start = prompt_pos;

  endpos = gtk_text_buffer_get_char_count(LISTENER_BUFFER) - 1;
  if ((endpos - curpos) > 40)
    end = curpos + 40;
  else end = endpos;

  len = end - start + 1;
  buf = sg_get_text(w, start, end + 1);
  listener_help_at_cursor(buf, curpos - start - 1, len, prompt_pos);
  g_free(buf);
}


static int check_balance(const char *expr, int start, int end, bool in_listener) 
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

  /* if cursor is positioned at close paren, try to find reason for unbalanced expr and highlight it */
  {
    int pos;
    bool success = true;
    pos = sg_cursor_position(listener_text);
    if (pos > 2)
      {
	char *str = NULL;
	str = sg_get_text(listener_text, 0, pos);
	if ((str[pos - 1] == ')') &&
	    ((str[pos - 2] != '\\') || (str[pos - 3] != '#')))
	  {
	    GtkTextIter limit;
	    gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &limit, find_current_prompt() - 1);
	    if (find_open_paren(2, pos - 1, &flash_paren_pos, &limit))
	      {
		add_inverse(flash_paren_pos);
		flashes = 4;
		g_timeout_add_full(0, (guint32)FLASH_TIME, flash_unbalanced_paren, NULL, NULL);
	      }
	    else success = false;
	  }
	if (str) g_free(str);
      }
    if (success) return(0);
  }
  return(-1);
}



void listener_append(const char *msg)
{
  append_listener_text(0, msg);
  if (listener_text)
    ss->graph_is_active = false;
}


void listener_append_and_prompt(const char *msg)
{
  if (msg)
    {
      append_listener_text(0, msg);
      glistener_append_prompt();
    }
  else append_listener_text(0, "\n");
}

static void glistener_return_callback(GtkWidget *w)
{

  remove_underline();

#if 0
  GtkTextIter cursor, scan, s1, s2, start, end;
  char *text;
  int pos, bpos, epos, oparen_pos;
  
  pos = glistener_cursor_position();
  if (pos < prompt_length - 1)
    bpos = prompt_length - 1;
  else
    {
      gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &cursor, pos + prompt_length - 1);
      if (!prompt_backward_search(&cursor, &start, &end))
	bpos = prompt_length - 1;
      else bpos = gtk_text_iter_get_offset(&start) + prompt_length;
    }
  
  pos = glistener_cursor(&cursor);
  if (!gtk_text_iter_forward_search(&cursor, prompt, 0, &start, &end, NULL))
    {
      gtk_text_buffer_get_end_iter(LISTENER_BUFFER, &end);
      epos = gtk_text_iter_get_offset(&end);
    }
  else epos = gtk_text_iter_get_offset(&start);
  
  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &start, bpos);
  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &end, epos);
  text = gtk_text_buffer_get_text(LISTENER_BUFFER, &start, &end, 0);
  fprintf(stderr, "full text: [%s]\n", text);

  /* now the expression is between start and end */

  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &scan, pos - 1);

  c = gtk_text_iter_get_char(&scan);

  if (c == ')')
    {
      if (find_open_paren(1, pos - 2, &oparen_pos, &start)) /* PERHAPS: bpos - 1 */
	{
	  /* look back for "(", if none, send current expr (oparen_pos to pos)
	     if found, look forward from pos for ")"
	        if none, insert "\n", flash paren, return
		else, find_matching, and repeat this loop
	   */
	}


	if (text)
	  {
	    int gc_loc;
	    s7_pointer old_port, result;
	    char *errmsg = NULL;
	    
	    if (pos < gtk_text_buffer_get_char_count(LISTENER_BUFFER))
	      append_listener_text(0, text);
	    
	    glistener_set_cursor_shape(ss->wait_cursor);
	    
	    remember_listener_string(text);
	    
	    if (s7_begin_hook(s7) != NULL) return;      /* s7 is already running (user typed <cr> during computation) */
	    
		old_port = s7_set_current_error_port(s7, s7_open_output_string(s7));
		gc_loc = s7_gc_protect(s7, old_port);
		
		if (with_interrupts(ss))
		  s7_set_begin_hook(s7, listener_begin_hook);
		
		result = s7_eval_c_string(s7, text);

		s7_set_begin_hook(s7, NULL);
		errmsg = mus_strdup(s7_get_output_string(s7, s7_current_error_port(s7)));
		
		s7_close_output_port(s7, s7_current_error_port(s7));
		s7_set_current_error_port(s7, old_port);
		s7_gc_unprotect_at(s7, gc_loc);


	    if (errmsg)
	      {
		if (*errmsg)
		  snd_display_result(errmsg, NULL);
		free(errmsg);
	      }
	    else snd_report_listener_result(result);
	    
	    g_free(text);
	    glistener_set_cursor_shape(ss->arrow_cursor); 
	  }
#else

  /* try to find complete form either enclosing current cursor, or just before it */
  int cmd_eot = 0;
  char *str = NULL, *full_str = NULL;
  int i, j;
  XEN form = XEN_UNDEFINED;
  int last_position = 0, current_position = 0;
  int end_of_text = 0, start_of_text = 0;
  int parens;

  int last_prompt = 0; /* TODO: get previous prompt position here */

  full_str = sg_get_text(listener_text, 0, -1);
  current_position = sg_cursor_position(listener_text);


  start_of_text = current_position;
  end_of_text = current_position;

  last_position = gtk_text_buffer_get_char_count(LISTENER_BUFFER);

  str = NULL;

#if (!HAVE_SCHEME)
  if (have_read_hook())
    {
      XEN result;
      int len;
      len = last_position - last_prompt; 
      if (len > 0)
	{
	  str = (char *)calloc(len + 1, sizeof(char)); 
	  for (i = last_prompt, j = 0; i < last_position; i++, j++) str[j] = full_str[i]; 
	  result = run_read_hook(str);
	  free(str);
	  if (XEN_TRUE_P(result)) 
	    {
	      if (full_str) g_free(full_str);
	      return;
	    }
	}
    }
#endif

  if (last_position > end_of_text)
    {
      end_of_text = last_position; /* added 12-Nov-07 for first form */
      for (i = current_position; i < last_position; i++)
	if (is_prompt(full_str, i + 1))
	  {
	    /* fprintf(stderr, "set end to %d (%d)\n", i - ss->listener_prompt_length + 1, i); */
	    end_of_text = i - ss->listener_prompt_length + 1;
	    break;
	  }
    }
  if (start_of_text > 0)
    {
      for (i = end_of_text; i >= 0; i--)
	if (is_prompt(full_str, i))
	  {
	    /* fprintf(stderr, "set start to %d\n", i + 1); */
	    start_of_text = i + 1;
	    break;
	  }
    }

  /* fprintf(stderr, "found %d %d\n", start_of_text, end_of_text); */
  
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
		  append_listener_text(0, (char *)"\n");
		}
	      if (full_str) g_free(full_str);
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

  if (full_str) g_free(full_str);
  {
    bool need_eval = false;
    int i, len;
    /* fprintf(stderr, "return: %s\n", str); */
    
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
	    
	    if (current_position < (last_position - 2))
	      append_listener_text(0, str);
	    
	    glistener_set_cursor_shape(ss->wait_cursor);
	    
	    if ((mus_strlen(str) > 1) || (str[0] != '\n'))
	      remember_listener_string(str);
	    
#if HAVE_RUBY || HAVE_FORTH
	    form = XEN_EVAL_C_STRING(str);
#endif

#if HAVE_SCHEME
	    if (s7_begin_hook(s7) != NULL) return;      /* s7 is already running (user typed <cr> during computation) */
	    
	    if ((mus_strlen(str) > 1) || (str[0] != '\n'))
	      {
		
		int gc_loc;
		s7_pointer old_port;
		
		old_port = s7_set_current_error_port(s7, s7_open_output_string(s7));
		gc_loc = s7_gc_protect(s7, old_port);
		
		if (with_interrupts(ss))
		  s7_set_begin_hook(s7, listener_begin_hook);
		
		if (have_read_hook())
		  form = run_read_hook(str);
		else form = s7_eval_c_string(s7, str);

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
	    glistener_set_cursor_shape(ss->arrow_cursor); 
	  }
	else
	  {
	    listener_append_and_prompt(NULL);
	  }
      }
  }

  cmd_eot = gtk_text_buffer_get_char_count(LISTENER_BUFFER);
  glistener_set_cursor_position(cmd_eot + 1);
#endif
}












#define WITH_LISTENER_TIPS 0

#if WITH_LISTENER_TIPS

static GtkTextTag *hover_tag = NULL;
static int old_hover_start, old_hover_end;
static bool hover_underlined = false;

static void remove_hover_underline(void)
{
  if (hover_underlined)
    {
      GtkTextBuffer *buf;
      GtkTextIter hover_start, hover_end;
      buf = LISTENER_BUFFER;
      gtk_text_buffer_get_iter_at_offset(buf, &hover_start, old_hover_start);
      gtk_text_buffer_get_iter_at_offset(buf, &hover_end, old_hover_end);
      gtk_text_buffer_remove_tag(buf, hover_tag, &hover_start, &hover_end);
      hover_underlined = false;
    }
}

static void add_hover_underline(GtkTextIter hover_start, GtkTextIter hover_end)
{
  GtkTextBuffer *buf;

  buf = LISTENER_BUFFER;
  if (!hover_tag)
    hover_tag = gtk_text_buffer_create_tag(buf, "tipped", "underline", PANGO_UNDERLINE_SINGLE, NULL);
  gtk_text_buffer_apply_tag(buf, hover_tag, &hover_start, &hover_end);

  old_hover_start = gtk_text_iter_get_offset(&hover_start);
  old_hover_end = gtk_text_iter_get_offset(&hover_end);
  hover_underlined = true;
}


static gboolean listener_mouse_move(GtkWidget *w, GdkEvent *ev, gpointer data)
{
  gint ev_x, ev_y, wx, wy;
  gdouble x, y;
  GtkTextIter iter;

  gdk_event_get_coords(ev, &x, &y);
  ev_x = x;
  ev_y = y;

  remove_hover_underline();

  gtk_text_view_window_to_buffer_coords(LISTENER_TEXT, GTK_TEXT_WINDOW_TEXT, ev_x, ev_y, &wx, &wy);
  gtk_text_view_get_iter_at_location(LISTENER_TEXT, &iter, wx, wy);

  if (gtk_text_iter_inside_word(&iter))
    {
      GtkTextIter start, end, back, front;
      char *txt;
      char c;
  
      start = iter;
      end = iter;

      if (!gtk_text_iter_starts_word(&start))
	gtk_text_iter_backward_word_start(&start);
      while (true)
	{
	  back = start;
	  gtk_text_iter_backward_char(&back);
	  c = gtk_text_iter_get_char(&back);
	  if (!separator_char_p(c))
	    gtk_text_iter_backward_word_start(&start);
	  else break;
	}

      if (!gtk_text_iter_ends_word(&end))
	gtk_text_iter_forward_word_end(&end);
      while (true)
	{
	  front = end;
	  gtk_text_iter_forward_char(&front);
	  c = gtk_text_iter_get_char(&front);
	  if (!separator_char_p(c))
	    gtk_text_iter_forward_word_end(&end);
	  else break;
	}
      txt = gtk_text_buffer_get_text(LISTENER_BUFFER, &start, &end, true);
      /* TODO: add this only if help is actually available */
      add_hover_underline(start, end);
    }
  return(false);
}
#endif


#if WITH_LISTENER_TIPS
      SG_SIGNAL_CONNECT(listener_text, "motion_notify_event", listener_mouse_move, NULL);
#endif
static void listener_help_callback(GtkWidget *w, gpointer info)
{
  /* for selected text or text near cursor or text near pointer, get help or completion,
   *    or at least apropos, show in help dialog
   */
  GtkTextIter start, end;
  if (gtk_text_buffer_get_selection_bounds(LISTENER_BUFFER, &start, &end))
    {
      GtkTextIter trim_start, trim_end;
      char *txt;
      trim_start = start;
      trim_end = end;
      while (!gtk_text_iter_ends_word(&trim_end)) gtk_text_iter_backward_char(&trim_end);
      while (!gtk_text_iter_starts_word(&trim_start)) gtk_text_iter_forward_char(&trim_start);
      txt = gtk_text_buffer_get_text(LISTENER_BUFFER, &trim_start, &trim_end, true);
      if (txt) 
	{
	  fprintf(stderr, "txt: [%s]\n", txt);
	  snd_help(txt, XEN_TO_C_STRING(g_snd_help(C_TO_XEN_STRING(txt), 0)), WITH_WORD_WRAP);
	  g_free(txt);
	}
    }
  else text_at_cursor(listener_text);
}






/* ---------------- snd connection ---------------- */


static void listener_save_callback(GtkWidget *w, gpointer info)
{
  FILE *fp = NULL;
  fp = FOPEN("listener.txt", "w");
  if (fp) 
    {
      save_listener_text(fp);
      snd_fclose(fp, "listener.txt");
    }
}


static void listener_clear_callback(GtkWidget *w, gpointer info)
{
  glistener_clear();
}


static void listener_stop_callback(GtkWidget *w, gpointer info)
{
  control_g(any_selected_sound());
}


#if HAVE_SCHEME
static void listener_stacktrace_callback(GtkWidget *w, gpointer info)
{
  s7_pointer str;
  str = s7_eval_c_string(s7, "(stacktrace)");
  if (s7_string_length(str) == 0)
    str = s7_eval_c_string(s7, "(object->string (error-environment))");
  snd_display_result(s7_string(str), NULL);
}
#endif


static void listener_popup_populate_callback(GtkTextView *view, GtkMenu *menu, gpointer ignored)
{
  /* this apparently only happens once */
  GtkWidget *w;
  
  /* prepending (to force the idiotic defaults to the end), so do everything backwards */
  w = gtk_separator_menu_item_new(); 
  gtk_menu_shell_prepend(GTK_MENU_SHELL(menu), w); 
  gtk_widget_show(w); 

  w = gtk_menu_item_new_with_label("Save"); 
  gtk_menu_shell_prepend(GTK_MENU_SHELL(menu), w); 
  g_signal_connect(w, "activate", G_CALLBACK(listener_save_callback), NULL);
  gtk_widget_show(w); 

  w = gtk_menu_item_new_with_label("Clear"); 
  gtk_menu_shell_prepend(GTK_MENU_SHELL(menu), w); 
  g_signal_connect(w, "activate", G_CALLBACK(listener_clear_callback), NULL);
  gtk_widget_show(w); 

  w = gtk_menu_item_new_with_label("Help"); 
  gtk_menu_shell_prepend(GTK_MENU_SHELL(menu), w); 
  g_signal_connect(w, "activate", G_CALLBACK(listener_help_callback), NULL);
  gtk_widget_show(w); 

#if HAVE_SCHEME
  w = gtk_menu_item_new_with_label("Error info");  /* how to set this to "Stacktrace" if s7_current_environment is nil? */
  gtk_menu_shell_prepend(GTK_MENU_SHELL(menu), w); 
  g_signal_connect(w, "activate", G_CALLBACK(listener_stacktrace_callback), NULL);
  gtk_widget_show(w); 
#endif

  w = gtk_menu_item_new_with_label("Stop"); 
  gtk_menu_shell_prepend(GTK_MENU_SHELL(menu), w); 
  g_signal_connect(w, "activate", G_CALLBACK(listener_stop_callback), NULL);
  gtk_widget_show(w); 
}


static XEN listener_click_hook; 
static XEN mouse_enter_listener_hook;
static XEN mouse_leave_listener_hook;

static gboolean listener_focus_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  if (with_pointer_focus(ss))
    goto_window(listener_text);

  if (XEN_HOOKED(mouse_enter_listener_hook))
    run_hook(mouse_enter_listener_hook,
	     XEN_LIST_1(XEN_WRAP_WIDGET(listener_text)),
	     S_mouse_enter_listener_hook);
  cursor_set_blinks(w, true);
  return(false);
}


static gboolean listener_unfocus_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  if (XEN_HOOKED(mouse_leave_listener_hook))
    run_hook(mouse_leave_listener_hook,
	     XEN_LIST_1(XEN_WRAP_WIDGET(listener_text)),
	     S_mouse_leave_listener_hook);
  cursor_set_blinks(w, false);
  return(false);
}

static gboolean listener_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  ss->graph_is_active = false;
  return(false);
}

static gboolean listener_key_press(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  guint key;
  GdkModifierType state;

  key = EVENT_KEYVAL(event);
  state = (GdkModifierType)EVENT_STATE(event);

  if ((state & snd_ControlMask) &&
      ((key == GDK_KEY_g) || (key == GDK_KEY_G)))
    ss->C_g_typed = true; 

  if (ss->graph_is_active) 
    {
      chan_info *cp;
      cp = current_channel();
      if (!cp) 
	ss->graph_is_active = false;
      else
	{
	  graph_key_press(channel_graph(cp), event, (gpointer)cp); 
	  return(true); /* don't repeat the keystroke?? */
	}
    }
  return(false);
}


static void listener_init(GtkWidget *w)
{
  set_listener_text_font();
  add_listener_style(w);
  SIGNAL_CONNECT(w, "key_press_event", listener_key_press, NULL); /* Snd's should run first */
  SIGNAL_CONNECT(listener_text, "button_press_event", listener_button_press, NULL);
  SIGNAL_CONNECT(listener_text, "enter_notify_event", listener_focus_callback, NULL);
  SIGNAL_CONNECT(listener_text, "leave_notify_event", listener_unfocus_callback, NULL);
  SIGNAL_CONNECT(listener_text, "populate-popup", listener_popup_populate_callback, NULL);

  glistener_set_prompt_tag(gtk_text_buffer_create_tag(LISTENER_BUFFER, "glistener_prompt_tag", "weight", PANGO_WEIGHT_BOLD, NULL));
  ss->listener_pane = w;
}


static void make_listener_widget(int height)
{
  if (!listener_text)
    {
      GtkWidget *frame;

      frame = gtk_frame_new(NULL);
      gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_ETCHED_IN);
      gtk_widget_show(frame);

      if (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS)
	gtk_paned_pack2(GTK_PANED(SOUND_PANE(ss)), frame, false, true); /* add2 but resize=false */
      else gtk_container_add(GTK_CONTAINER(MAIN_PANE(ss)), frame);

#if (!HAVE_GTK_3)
      {
	GtkWidget *vb, *sb, *sw;
	vb = gtk_table_new(2, 1, false);
	gtk_container_add(GTK_CONTAINER(frame), vb);
	gtk_widget_show(vb);

	sw = glistener_new(NULL, listener_init);
	gtk_table_attach(GTK_TABLE(vb), sw, 0, 1, 0, 1, /* left right top bottom */
			 (GtkAttachOptions)(GTK_FILL | GTK_EXPAND), 
			 (GtkAttachOptions)(GTK_FILL | GTK_EXPAND | GTK_SHRINK), 
			 0, 0);

	sb = gtk_statusbar_new();
	gtk_table_attach(GTK_TABLE(vb), sb, 0, 1, 1, 2,
			(GtkAttachOptions)(GTK_EXPAND | GTK_FILL), 
		       (GtkAttachOptions)(GTK_FILL), 
		       0, 0);
			 
	gtk_widget_show(sb);
	gtk_widget_show(vb);
      }
#else
      {
	GtkWidget *vb, *sb, *sw;
	vb = gtk_grid_new();
	gtk_container_add(GTK_CONTAINER(frame), vb);
	gtk_widget_show(vb);

	sw = glistener_new(NULL, listener_init);
	
	gtk_widget_set_halign(sw, GTK_ALIGN_FILL);
	gtk_widget_set_valign(sw, GTK_ALIGN_FILL);
	gtk_widget_set_hexpand(sw, TRUE);
	gtk_widget_set_vexpand(sw, TRUE);
	gtk_grid_attach(GTK_GRID(vb), sw, 0, 0, 1, 1); /* left top w h */
 
	sb = gtk_statusbar_new();
	gtk_widget_set_halign(sb, GTK_ALIGN_FILL); 
	gtk_grid_attach(GTK_GRID(vb), sb, 0, 1, 1, 1);
 
	gtk_widget_show(sb);
	gtk_widget_show(vb);
      }
#endif
    }
}


void goto_listener(void) 
{
  goto_window(listener_text);
}


void color_listener(color_info *pix)
{
  ss->listener_color = pix;
#if (!HAVE_GTK_3)
  glistener_set_background_color(rgb_to_gdk_color(ss->listener_color));
#else
  glistener_set_background_color((GdkRGBA *)(ss->listener_color));
#endif
}


void color_listener_text(color_info *pix)
{
  ss->listener_text_color = pix;
#if (!HAVE_GTK_3)
  glistener_set_text_color(rgb_to_gdk_color(ss->listener_text_color));
#else
  glistener_set_text_color((GdkRGBA *)(ss->listener_text_color));
#endif
}


void handle_listener(bool open)
{
  if ((open) && (!listener_text))
    {
      make_listener_widget(100);

      color_listener(ss->listener_color);
      color_listener_text(ss->listener_text_color);
    }

  if ((SOUND_PANE(ss)) && /* might be run -separate with no sound open */
      (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS))
    {
      int hgt;
      hgt = widget_height(SOUND_PANE(ss));
      if (open)
	{
	  if (hgt > 100) /* we can get here before the sound window has opened, but with one pending.
			  *   the position is in terms of current size, which is useless in this case.
			  */
	    gtk_paned_set_position(GTK_PANED(SOUND_PANE(ss)), (gint)(hgt * .75));
	}
      else gtk_paned_set_position(GTK_PANED(SOUND_PANE(ss)), hgt);
    }
}


bool listener_exists(void)
{
  return((bool)listener_text);
}


int listener_height(void) 
{
#if HAVE_GTK_3
  int hgt, pos;
  if (!listener_text) 
    return(0);
  hgt = widget_height(SOUND_PANE(ss));
  pos = gtk_paned_get_position(GTK_PANED(SOUND_PANE(ss)));
  return(hgt - pos);
#else
  if ((listener_text) && (widget_is_active(listener_text)))
    return(widget_height(listener_text));
  else return(0);
#endif
}


int listener_width(void) 
{
  if ((listener_text) && (widget_is_active(listener_text)))
    return(widget_width(listener_text)); 
  else return(0);
}


static XEN g_listener_selection(void)
{
  #define H_listener_selection "(" S_listener_selection "): currently selected text in listener or " PROC_FALSE
  XEN res = XEN_FALSE;
  if (listener_text)
    {
      GtkTextIter start, end;
      if (gtk_text_buffer_get_selection_bounds(LISTENER_BUFFER, &start, &end))
	{
	  char *txt;
	  txt = gtk_text_buffer_get_text(LISTENER_BUFFER, &start, &end, true);
	  if (txt) 
	    {
	      res = C_TO_XEN_STRING(txt);
	      g_free(txt);
	    }
	}
    }
  return(res);
}


void set_listener_text_font(void)
{
  glistener_set_font(LISTENER_FONT(ss));
}


static XEN g_reset_listener_cursor(void)
{
  #define H_reset_listener_cursor "(" S_reset_listener_cursor "): reset listener cursor to the default pointer"
  if (listener_text)
    glistener_set_cursor_shape(ss->arrow_cursor);
  return(XEN_FALSE);
}


void clear_listener(void)
{
  glistener_clear();
}

void append_listener_text(int end, const char *msg)
{
  /* "end" arg needed in Motif */
  glistener_append_text(msg);
}

int save_listener_text(FILE *fp)
{
  
  if ((!listener_text) ||
      (glistener_write(fp)))
    return(0);
  return(-1);
}

static XEN g_goto_listener_end(void)
{
  #define H_goto_listener_end "(" S_goto_listener_end "): move cursor and scroll to bottom of listener pane"
  glistener_scroll_to_end();
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

  XEN_DEFINE_PROCEDURE(S_listener_selection, g_listener_selection_w,       0, 0, 0, H_listener_selection);
  XEN_DEFINE_PROCEDURE(S_reset_listener_cursor, g_reset_listener_cursor_w, 0, 0, 0, H_reset_listener_cursor);
  XEN_DEFINE_PROCEDURE(S_goto_listener_end, g_goto_listener_end_w,         0, 0, 0, H_goto_listener_end);

  #define H_listener_click_hook S_listener_click_hook " (position): called when listener clicked; position is text pos of click in listener"
  listener_click_hook = XEN_DEFINE_HOOK(S_listener_click_hook, "(make-hook 'position)", 1,   H_listener_click_hook); 

}


/* TODO: the edit history window is sometimes empty in gtk
 * why not c-r c-s in listener? where to prompt?
 * split this out as a separate "widget"
 * c-_ to undo--it's sort of working, completion in g? 
 *
 * what about all the other gtk key bindings?
 * scroll to new prompt -- when does this fail?
 *    when snd opens a sound, listener size changes and we end up somewhere random
 *    when sound is closed, listener should fill space
 *
 * use all the gtk_text_iter stuff, not strcmp et al
 * selection isn't transferable?
 *
 * motif case needs prompt length fixups
 *
 * why is gtk 3 default font so small?  and snd looks terrible in gtk 3, but not across the net?
 * why not add the closing " when completing a name like "oboe. if completion is full filename
 */



/* to get rid of 
 *    Fontconfig error: Cannot load default config file
 * and the consequent ridiculous fonts, since I think I built fontconfig from scratch,
 * copy (as root) /etc/fonts/fonts.conf to /usr/local/etc/fonts/fonts.conf
 *
 * to disable the goddamn beep put
 *   gtk-error-bell = 0
 * in /etc/gtk-2.0/gtkrc
 *
 * to build fontconfig, use --disable-docs (there's no way to make the docbook chain happy)
 * atk-bridge-2.0 needed, glib needs automake 1.13.1, at-spi2-atk needs at-spi2-code which
 * is incompatible with glib 2.37.0, my FC18 machine is dead, so I'm stuck.  Wait for FC19...
 */
