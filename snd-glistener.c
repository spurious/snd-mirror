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
    }
}


void glistener_set_prompt_tag(GtkTextTag *m)
{
  prompt_tag = m;
}


void glistener_set_prompt(const char *str)
{
  /* the internal version includes a preceding <cr>, and its length is in unicode terms
   *   also, if we have prompts displayed in the listener, they need to change to match
   *   the new one.
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
    return(pos + 1);
  return(gtk_text_iter_get_offset(&end) + 1);
}


static bool is_prompt_end(int end_pos)
{
  /* the prompt includes the preceding <cr> */
  GtkTextIter start, end;
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





/* ---------------- listener text history ---------------- */

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



/* ---------------- paren matching ---------------- */

static int find_string_start(int pos)
{
  /* there could be any number of slashified slashes before this quote. */
  int slashes = 0;
  GtkTextIter slasher;
  gunichar cs;

  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &slasher, pos);  
  while (!gtk_text_iter_is_start(&slasher))
    {
      gtk_text_iter_backward_char(&slasher);
      cs = gtk_text_iter_get_char(&slasher);
      if (cs == (gunichar)'\\')
	slashes++;
      else break;
    }
  if ((slashes & 1) == 0)
    {
      /* we have a string -- scan backwards for its start */
      while (!gtk_text_iter_is_start(&slasher))
	{
	  gtk_text_iter_backward_char(&slasher);
	  cs = gtk_text_iter_get_char(&slasher);
	  if (cs == (gunichar)'\"')
	    {
	      /* now the same business as before -- is it slashified */
	      slashes = 0;
	      while (!gtk_text_iter_is_start(&slasher))
		{
		  gtk_text_iter_backward_char(&slasher);
		  cs = gtk_text_iter_get_char(&slasher);
		  if (cs == (gunichar)'\\')
		    slashes++;
		  else break;
		}
	      if ((slashes & 1) == 0)
		break;
	    }
	}
    }
  return(gtk_text_iter_get_offset(&slasher));
}


static bool at_character_constant(int end_pos)
{
  GtkTextIter start, end;
  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &end, end_pos);
  start = end;
  if (gtk_text_iter_backward_chars(&start, 2))
    {
      char *txt;
      txt = gtk_text_iter_get_text(&start, &end);
      /* fprintf(stderr, "char: [%s: %c%c]\n", txt, txt[0], txt[1]); */
      return(mus_strcmp(txt, "#\\"));
    }
  return(false);
}


static bool find_matching_paren(int parens, int pos, int *highlight_pos)
{
  GtkTextIter scan;
  int parens_at_line_end = parens;
  bool end_scan = false;
  gunichar c;

  if (at_character_constant(pos + 1))
    return(false);

  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &scan, pos);  
  while (!gtk_text_iter_is_start(&scan))
    {
      if (is_prompt_end(gtk_text_iter_get_offset(&scan) + 1))
	return(parens == 0);

      c = gtk_text_iter_get_char(&scan);
      if (c == (gunichar)'\"')
	gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &scan, find_string_start(gtk_text_iter_get_offset(&scan)));
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
      gtk_text_iter_backward_char(&scan);
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
static int underline_pos = -1;

static void add_underline(int pos)
{
  GtkTextIter start, end;
  GtkTextBuffer *buf;
  buf = LISTENER_BUFFER;
  gtk_text_buffer_get_iter_at_offset(buf, &start, pos);
  gtk_text_buffer_get_iter_at_offset(buf, &end, pos + 1);
  if (!underline_tag) underline_tag = gtk_text_buffer_create_tag(buf, "underline", "underline", PANGO_UNDERLINE_DOUBLE, NULL);
  gtk_text_buffer_apply_tag(buf, underline_tag, &start, &end);
}


static void remove_underline(int pos)
{
  if (underline_tag)
    {
      GtkTextIter start, end;
      GtkTextBuffer *buf;
      buf = LISTENER_BUFFER;
      gtk_text_buffer_get_iter_at_offset(buf, &start, pos);
      gtk_text_buffer_get_iter_at_offset(buf, &end, pos + 1);
      gtk_text_buffer_remove_tag(buf, underline_tag, &start, &end);
    }
}


static void check_parens(void)
{
  int pos;
  GtkTextIter scan;
  gunichar c;

  if (underline_pos != -1)
    {
      remove_underline(underline_pos);
      underline_pos = -1;
    }
  pos = glistener_cursor_position();
  gtk_text_buffer_get_iter_at_offset(LISTENER_BUFFER, &scan, pos - 1);
  c = gtk_text_iter_get_char(&scan);
  if (c == ')')
    {
      if (find_matching_paren(1, pos - 2, &pos))
	{
	  add_underline(pos);
	  underline_pos = pos;
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

/* GDK_<char> became GDK_KEY_<char> in 2.90.7, so we need to define both, or use the old form */

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





/* -------------------------------------------------------------------------------- */

static int printout_end = 0;


void append_listener_text(int end, const char *msg)
{
  /* "end" arg needed in Motif */
  if (listener_text)
    {
      int chars;
      chars = gtk_text_buffer_get_char_count(LISTENER_BUFFER);
      if (chars > 0) glistener_set_cursor_position(chars);
      sg_text_insert(listener_text, (char *)msg);
    }
}


static void append_listener_prompt(void)
{
  if (listener_text)
    {
      int chars;
      GtkTextIter pos;
      chars = gtk_text_buffer_get_char_count(LISTENER_BUFFER);
      if (chars > 0) glistener_set_cursor_position(chars);
      gtk_text_buffer_get_end_iter(LISTENER_BUFFER, &pos);
      prompt_insert(&pos, false);
    }
}


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



/* ---------------- listener widget ---------------- */

void listener_append(const char *msg)
{
  append_listener_text(0, msg);
  if (listener_text)
    {
      ss->graph_is_active = false;
      printout_end = gtk_text_buffer_get_char_count(LISTENER_BUFFER) - 1;
    }
}


void listener_append_and_prompt(const char *msg)
{
  if (msg)
    {
      append_listener_text(0, msg);
      append_listener_prompt();
    }
  else append_listener_text(0, "\n");
  if (listener_text)
    {
      int cmd_eot;
      cmd_eot = gtk_text_buffer_get_char_count(LISTENER_BUFFER);
      printout_end = cmd_eot - 1;
    }
}

#define GUI_TEXT_END(w) gtk_text_buffer_get_char_count(gtk_text_view_get_buffer(GTK_TEXT_VIEW(w)))
#define GUI_TEXT_POSITION_TYPE gint
#define GUI_TEXT(w) sg_get_text(w, 0, -1)
#define GUI_TEXT_INSERTION_POSITION(w) sg_cursor_position(w)
#define GUI_TEXT_SET_INSERTION_POSITION(w, pos) glistener_set_cursor_position(pos + 1)
#define GUI_LISTENER_TEXT_INSERT(w, pos, text) append_listener_text(0, text)
#define GUI_FREE(w) g_free(w)
#define GUI_SET_CURSOR(w, cursor) gdk_window_set_cursor(gtk_text_view_get_window(GTK_TEXT_VIEW(w), GTK_TEXT_WINDOW_TEXT), cursor)
#define GUI_UNSET_CURSOR(w, cursor) gdk_window_set_cursor(gtk_text_view_get_window(GTK_TEXT_VIEW(w), GTK_TEXT_WINDOW_TEXT), cursor)
#define GUI_UPDATE(w) 
#define GUI_TEXT_GOTO(w, pos) glistener_set_cursor_position(pos)


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
	    if (find_matching_paren(2, pos - 1, &flash_paren_pos))
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


static void glistener_return_callback(GtkWidget *w)
{
  /* snd-listener.c version assumes ASCII which we are hereby starting to try to think about maybe avoiding
   */

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

  last_position = GUI_TEXT_END(w);

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
	      if (full_str) GUI_FREE(full_str);
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
		  GUI_LISTENER_TEXT_INSERT(w, 0, (char *)"\n");
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

  if (full_str) GUI_FREE(full_str);
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
	      GUI_LISTENER_TEXT_INSERT(w, GUI_TEXT_END(w), str);
	    
	    GUI_SET_CURSOR(w, ss->wait_cursor);
	    GUI_UPDATE(w); /* not sure about this... */
	    
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
	    GUI_UNSET_CURSOR(w, ss->arrow_cursor); 
	  }
	else
	  {
	    listener_append_and_prompt(NULL);
	  }
      }
  }

  cmd_eot = GUI_TEXT_END(w);
  GUI_TEXT_GOTO(w, cmd_eot - 1);
  GUI_TEXT_SET_INSERTION_POSITION(w, cmd_eot + 1);
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


static gboolean listener_key_press(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  int pos;
  char *text;
  guint key;
  GdkModifierType state;

  if (ss->graph_is_active) 
    {
      chan_info *cp;
      cp = current_channel();
      if (!cp) 
	ss->graph_is_active = false;
      else
	{
	  graph_key_press(channel_graph(cp), event, (gpointer)cp); 
	  return(true); /* don't repeat the keystroke */
	}
    }

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
	  ss->C_g_typed = true; 
	  if (state & snd_MetaMask)
	    clear_listener();
	  else control_g(any_selected_sound());
	}
      else return(false);
      break;

    case GDK_KEY_BackSpace:
      {
	/* make sure we don't backspace over the prompt */
	int current_position;
	char *fstr;
	
	current_position = glistener_cursor_position();
	if (current_position > ss->listener_prompt_length)
	  {
	    fstr = sg_get_text(listener_text, current_position - ss->listener_prompt_length - 1, current_position);
	    if ((current_position != (printout_end - ss->listener_prompt_length - 1)) && 
		(!(mus_strcmp(fstr, prompt)))) /* TODO: is_prompt? */
	      {
		g_free(fstr);
		return(false);
	      }
	    g_free(fstr);
	  }
      }
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
	  if (pos > ss->listener_prompt_length)
	    {
	      bool at_prompt;
	      text = sg_get_text(listener_text, pos - ss->listener_prompt_length - 2, pos);
	      at_prompt = is_prompt(text, ss->listener_prompt_length + 1);
	      if (text) g_free(text);
	      if (!at_prompt)
		{
		  /* it might be the next thing! */
		  text = sg_get_text(listener_text, pos + ss->listener_prompt_length + 2, pos);
		  at_prompt = is_prompt(text, ss->listener_prompt_length);
		  if (text) g_free(text);
		  if (!at_prompt)
		    text_transpose(listener_text);
		}
	    }
	  /* else we're sitting at the prompt so drop through and block the signal */
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
	  if (gtk_text_iter_get_offset(&start) >= ss->listener_prompt_length)
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




static XEN listener_click_hook; 

static gboolean listener_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  ss->graph_is_active = false;
  return(false);
}


static int listener_insertion_position = 0;

static void text_insert(GtkTextBuffer *textbuffer, GtkTextIter *location, gchar *text, gint len, gpointer user_data)
{
  listener_insertion_position = gtk_text_iter_get_offset(location);
}


static gboolean listener_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
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
  return(false);
}


static XEN mouse_enter_listener_hook;
static XEN mouse_leave_listener_hook;

#if 0
static bool cursor_blinks(GtkWidget *w)
{
  GtkSettings *settings;
  gboolean blink = false;
  settings = gtk_widget_get_settings(w);
  g_object_get(settings, "gtk-cursor-blink", &blink, NULL);
  return((bool)blink);
}
#endif


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

  w = gtk_menu_item_new_with_label(I_HELP); 
  gtk_menu_shell_prepend(GTK_MENU_SHELL(menu), w); 
  g_signal_connect(w, "activate", G_CALLBACK(listener_help_callback), NULL);
  gtk_widget_show(w); 

#if HAVE_SCHEME
  w = gtk_menu_item_new_with_label("Error info");  /* how to set this to "Stacktrace" if s7_current_environment is nil? */
  gtk_menu_shell_prepend(GTK_MENU_SHELL(menu), w); 
  g_signal_connect(w, "activate", G_CALLBACK(listener_stacktrace_callback), NULL);
  gtk_widget_show(w); 
#endif

  w = gtk_menu_item_new_with_label(I_STOP); 
  gtk_menu_shell_prepend(GTK_MENU_SHELL(menu), w); 
  g_signal_connect(w, "activate", G_CALLBACK(listener_stop_callback), NULL);
  gtk_widget_show(w); 
}


static void make_listener_widget(int height)
{
  if (!listener_text)
    {
      GtkWidget *frame;
      GtkTextIter pos, start;

      frame = gtk_frame_new(NULL);
      gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_ETCHED_IN);
      gtk_widget_show(frame);

      if (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS)
	gtk_paned_pack2(GTK_PANED(SOUND_PANE(ss)), frame, false, true); /* add2 but resize=false */
      else gtk_container_add(GTK_CONTAINER(MAIN_PANE(ss)), frame);

      listener_text = make_scrolled_text(frame, true, CONTAINER_ADD, false); 
      set_listener_text_font();
      add_listener_style(listener_text);

      glistener_bindings(GTK_TEXT_VIEW_GET_CLASS(LISTENER_TEXT));

      SG_SIGNAL_CONNECT(listener_text, "key_press_event", listener_key_press, NULL);
      SG_SIGNAL_CONNECT(listener_text, "key_release_event", glistener_key_release, NULL);
      SG_SIGNAL_CONNECT(listener_text, "button_press_event", listener_button_press, NULL);
      SG_SIGNAL_CONNECT(listener_text, "button_release_event", listener_button_release, NULL);
      SG_SIGNAL_CONNECT(listener_text, "enter_notify_event", listener_focus_callback, NULL);
      SG_SIGNAL_CONNECT(listener_text, "leave_notify_event", listener_unfocus_callback, NULL);
      SG_SIGNAL_CONNECT(listener_text, "populate-popup", listener_popup_populate_callback, NULL);
      SG_SIGNAL_CONNECT_AFTER(LISTENER_BUFFER, "insert-text", text_insert, NULL);
      ss->listener_pane = listener_text;

      if (!prompt)
	{
	  prompt = calloc(3, sizeof(char));
	  prompt[0] = '\n';
	  prompt[1] = '>';
	  prompt_length = g_utf8_strlen(prompt, -1);
	}

      glistener_set_prompt_tag(gtk_text_buffer_create_tag(LISTENER_BUFFER, "prompt_tag", 
							  "weight", PANGO_WEIGHT_BOLD,
							  NULL));

      /* put in the first prompt without the preceding <cr> */
      gtk_text_buffer_get_start_iter(LISTENER_BUFFER, &start);
      prompt_insert(&start, true);
      gtk_widget_show(listener_text);
    }
}



/* ---------------- snd connection ---------------- */

void goto_listener(void) 
{
  goto_window(listener_text);
}


void color_listener(color_info *pix)
{
  ss->listener_color = pix;
  if (listener_text) 
    widget_modify_base(listener_text, GTK_STATE_NORMAL, ss->listener_color);
}


void color_listener_text(color_info *pix)
{
  ss->listener_text_color = pix;
#if (!HAVE_GTK_3)
  if (listener_text) 
    gtk_widget_modify_text(listener_text, GTK_STATE_NORMAL, rgb_to_gdk_color(ss->listener_text_color));
#else
  if (listener_text) 
    gtk_widget_override_color(listener_text, GTK_STATE_FLAG_ACTIVE, (GdkRGBA *)(ss->listener_text_color));
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
#if (!HAVE_GTK_3)
  if (listener_text)
    gtk_widget_modify_font(GTK_WIDGET(listener_text), LISTENER_FONT(ss));
#else
  if (listener_text)
    gtk_widget_override_font(GTK_WIDGET(listener_text), LISTENER_FONT(ss));
#endif
}


static XEN g_reset_listener_cursor(void)
{
  #define H_reset_listener_cursor "(" S_reset_listener_cursor "): reset listener cursor to the default pointer"
  if (listener_text)
    gdk_window_set_cursor(WIDGET_TO_WINDOW(listener_text), ss->arrow_cursor);
  return(XEN_FALSE);
}


void clear_listener(void)
{
  glistener_clear();
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
  if (listener_text)
    {
      int chars;
      chars = gtk_text_buffer_get_char_count(LISTENER_BUFFER);
      if (chars > 0) glistener_set_cursor_position(chars);
      return(C_TO_XEN_INT(chars));
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
 *    gtk's paned window widget is junk
 *
 * use all the gtk_text_iter stuff, not strcmp et al
 * selection isn't transferable?
 *
 * to get a prompt of lambda: in Greek font: 
 *   (set! (listener-prompt) (string (integer->char #xce) (integer->char #xbb) #\:))
 *   (set! (listener-prompt) (bytevector #xce #xbb (char->integer #\>)))
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
