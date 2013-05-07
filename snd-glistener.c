#include "snd.h"


static GtkWidget *listener_text = NULL;
static int printout_end = 0;

#define LISTENER_BUFFER gtk_text_view_get_buffer(GTK_TEXT_VIEW(listener_text))


int save_listener_text(FILE *fp)
{
  /* return -1 if fwrite error */
  if (listener_text)
    {
      char *str = NULL;
      str = sg_get_text(listener_text, 0, -1);
      if (str)
	{
	  size_t bytes;
	  bytes = fwrite((void *)str, sizeof(char), mus_strlen(str), fp);
	  g_free(str);
	  if (bytes == 0) return(-1);
	}
    }
  return(0);
}


void append_listener_text(int end, const char *msg)
{
  /* "end" arg needed in Motif */
  if (listener_text)
    {
      int chars;
      chars = gtk_text_buffer_get_char_count(LISTENER_BUFFER);
      if (chars > 0) sg_set_cursor(listener_text, chars + 1);
      sg_text_insert(listener_text, (char *)msg);
    }
}


static GtkTextTag *prompt_not_editable = NULL;

static void append_listener_prompt(void)
{
  /* append cr + prompt with not-editable tag */
  if (listener_text)
    {
      int chars;
      GtkTextIter pos;
      chars = gtk_text_buffer_get_char_count(LISTENER_BUFFER);
      if (chars > 0) sg_set_cursor(listener_text, chars + 1);
      gtk_text_buffer_get_end_iter(LISTENER_BUFFER, &pos);
      gtk_text_buffer_insert_with_tags(LISTENER_BUFFER, &pos, listener_prompt_with_cr(), ss->listener_prompt_length + 1, prompt_not_editable, NULL);
    }
}


static void sg_text_delete(GtkWidget *w, int start, int end)
{
  GtkTextIter s, e;
  GtkTextBuffer *buf;
  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(w));
  gtk_text_buffer_get_iter_at_offset(buf, &s, start);
  gtk_text_buffer_get_iter_at_offset(buf, &e, end); 
  gtk_text_buffer_delete(buf, &s, &e);
}


static void listener_completion(int end)
{
  int beg;
  char *old_text = NULL;

  beg = printout_end + 1;
  if (end <= beg) return;

  old_text = sg_get_text(listener_text, beg, end);
  /* now old_text is the stuff typed since the last prompt */

  if (old_text)
    {
      char *new_text = NULL, *file_text = NULL;
      bool try_completion = true;

      new_text = complete_listener_text(old_text, end, &try_completion, &file_text);
      if (try_completion)
	{
	  if (mus_strcmp(old_text, new_text))
	    get_completion_matches();
	  sg_text_delete(listener_text, beg, end);
	  append_listener_text(0, new_text);
	  goto_window(listener_text);
	}

      if (new_text) free(new_text); 
      if (file_text) free(file_text);
      g_free(old_text);
    }
  
}


static int back_to_start(bool move_cursor)
{
  char *full_str = NULL;
  int start_of_text;
  full_str = sg_get_text(listener_text, 0, -1);
  start_of_text = sg_cursor_position(listener_text);
  if (start_of_text > 0)
    {
      int i;
      for (i = start_of_text; i >= 0; i--)
	if (is_prompt(full_str, i))
	  {
	    start_of_text = i + 1;
	    break;
	  }
    }
  if (move_cursor) 
    sg_set_cursor(listener_text, start_of_text + 1);
  if (full_str) g_free(full_str);
  return(start_of_text + 1);
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

  prompt_pos = back_to_start(false);

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


static void listener_return_callback(void)
{
  listener_return(listener_text, printout_end);
}


void listener_delete_text(int new_end)
{
  int old_end;
  old_end = gtk_text_buffer_get_char_count(gtk_text_view_get_buffer(GTK_TEXT_VIEW(listener_text)));
  if (new_end < old_end)
    sg_text_delete(listener_text, new_end, old_end);
}


static void clear_back_to_prompt(GtkWidget *w)
{
  int beg, end;
  end = sg_cursor_position(w);
  back_to_start(true);
  beg = sg_cursor_position(w);
  if (end <= beg) return;
  sg_text_delete(w, beg, end);
}



static void ctrl_k(GtkWidget *w)
{
  GtkTextIter beg, end;
  GtkTextBuffer *buf;
  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(w));
  gtk_text_buffer_get_iter_at_mark(buf, &beg, gtk_text_buffer_get_mark(buf, "insert"));
  end = beg;
  gtk_text_iter_forward_to_line_end(&end); /* was forward_to_end! */
  if (!gtk_text_iter_equal(&beg, &end))
    {
      gtk_text_buffer_select_range(buf, &beg, &end);
      gtk_text_buffer_cut_clipboard(buf, gtk_widget_get_clipboard(w, GDK_SELECTION_CLIPBOARD), true);
    }
}



static void sg_text_replace(GtkWidget *w, int beg, int end, char *text)
{
  GtkTextIter pos, endpos, pos1;
  GtkTextBuffer *buf;
  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(w));
  gtk_text_buffer_get_iter_at_offset(buf, &pos, beg);
  gtk_text_buffer_get_iter_at_offset(buf, &endpos, end);
  gtk_text_buffer_delete(buf, &pos, &endpos);
  gtk_text_buffer_get_iter_at_offset(buf, &pos1, beg);
  gtk_text_buffer_insert(buf, &pos1, text, strlen(text));
}


static void text_transpose(GtkWidget *w)
{
  int curpos;
  curpos = sg_cursor_position(w);
  if (curpos > 1)
    {
      char *buf = NULL;
      char tmp;
      buf = sg_get_text(w, curpos - 1, curpos + 1);
      tmp = buf[0];
      buf[0] = buf[1];
      buf[1] = tmp;
      sg_text_replace(w, curpos - 1, curpos + 1, buf);
      sg_set_cursor(w, curpos + 2);
    }
}


static void word_upper(GtkWidget *w, int cap, int up)
{
  int curpos, endpos;
  curpos = sg_cursor_position(w);
  endpos = gtk_text_buffer_get_char_count(gtk_text_view_get_buffer(GTK_TEXT_VIEW(w)));
  if (curpos < endpos)
    {
      int i, length, wstart, wend;
      char *buf = NULL;
      length = endpos - curpos;
      buf = sg_get_text(w, curpos, endpos);
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
	  sg_text_replace(w, curpos, curpos + 1, buf);
	}
      else
	{
	  int j;
	  for (i = wstart, j = 0; i < wend; i++, j++)
	    if (up) 
	      buf[j] = toupper(buf[i]);
	    else buf[j] = tolower(buf[i]);
	  buf[j] = '\0';
	  sg_text_replace(w, curpos + wstart, curpos + wend, buf);
	}
      sg_set_cursor(w, curpos + wend + 1);
      if (buf) g_free(buf);
    }
}


static GtkTextTag *flash_tag = NULL;
static int flashes = 0;
static int paren_pos = -1;
#define FLASH_TIME 150

static void add_inverse(int pos)
{
  GtkTextIter start, end;
  GtkTextBuffer *buf;
  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(listener_text));
  gtk_text_buffer_get_iter_at_offset(buf, &start, pos);
  gtk_text_buffer_get_iter_at_offset(buf, &end, pos + 1);
  if (!flash_tag) flash_tag = gtk_text_buffer_create_tag(buf, "red_background", "background", "red", NULL);
  gtk_text_buffer_apply_tag(buf, flash_tag, &start, &end);
}


static void remove_inverse(int pos)
{
  GtkTextIter start, end;
  GtkTextBuffer *buf;
  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(listener_text));
  gtk_text_buffer_get_iter_at_offset(buf, &start, pos);
  gtk_text_buffer_get_iter_at_offset(buf, &end, pos + 1);
  if (!flash_tag) flash_tag = gtk_text_buffer_create_tag(buf, "red_background", "background", "red", NULL);
  gtk_text_buffer_remove_tag(buf, flash_tag, &start, &end);
}


static gint flash_unbalanced_paren(gpointer data)
{
  flashes--;
  if (flashes & 1) remove_inverse(paren_pos); else add_inverse(paren_pos);
  if (flashes > 0)
    g_timeout_add_full(0, (guint32)FLASH_TIME, flash_unbalanced_paren, NULL, NULL);
  else 
    {
      remove_inverse(paren_pos);
      paren_pos = -1;
    }
  return(0);
}


bool highlight_unbalanced_paren(void)
{
  /* if cursor is positioned at close paren, try to find reason for unbalanced expr and highlight it */
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
	  int parens;
	  parens = find_matching_paren(str, 2, pos - 1, &paren_pos);
	  if (parens == 0)
	    {
	      add_inverse(paren_pos);
	      flashes = 4;
	      g_timeout_add_full(0, (guint32)FLASH_TIME, flash_unbalanced_paren, NULL, NULL);
	    }
	  else success = false;
	}
      if (str) g_free(str);
    }
  return(success);
}


static GtkTextTag *tag = NULL;
static int old_pos = -1;

static void add_underline(int pos)
{
  GtkTextIter start, end;
  GtkTextBuffer *buf;
  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(listener_text));
  gtk_text_buffer_get_iter_at_offset(buf, &start, pos);
  gtk_text_buffer_get_iter_at_offset(buf, &end, pos + 1);
  if (!tag) tag = gtk_text_buffer_create_tag(buf, "underline", "underline", PANGO_UNDERLINE_DOUBLE, NULL);
  gtk_text_buffer_apply_tag(buf, tag, &start, &end);
}


static void remove_underline(int pos)
{
  GtkTextIter start, end;
  GtkTextBuffer *buf;
  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(listener_text));
  gtk_text_buffer_get_iter_at_offset(buf, &start, pos);
  gtk_text_buffer_get_iter_at_offset(buf, &end, pos + 1);
  if (!tag) tag = gtk_text_buffer_create_tag(buf, "underline", "underline", PANGO_UNDERLINE_DOUBLE, NULL);
  gtk_text_buffer_remove_tag(buf, tag, &start, &end);
}


static void check_parens(void)
{
  int current_position, parens, pos;
  if (old_pos != -1)
    {
      remove_underline(old_pos);
      old_pos = -1;
    }
  current_position = sg_cursor_position(listener_text);
  if (current_position > 2)
    {
      char *fstr = NULL;
      fstr = sg_get_text(listener_text, 0, current_position);
      if (fstr[current_position - 1] == ')')
	{
	  parens = find_matching_paren(fstr, 1, current_position - 1, &pos);
	  if (parens == 0)
	    {
	      add_underline(pos);
	      old_pos = pos;
	    }
	}
      g_free(fstr);
    }
}


#define WITH_LISTENER_TIPS 0

#if WITH_LISTENER_TIPS

static GtkTextTag *hover_tag = NULL;
static int old_hover_start, old_hover_end; /* these can't be GtkTextIters! */
static bool hover_underlined = false;

static void add_hover_underline(GtkTextIter hover_start, GtkTextIter hover_end)
{
  GtkTextBuffer *buf;
  
  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(listener_text));
  if (!hover_tag)
    hover_tag = gtk_text_buffer_create_tag(buf, "underline", "underline", PANGO_UNDERLINE_SINGLE, NULL);
  gtk_text_buffer_apply_tag(buf, hover_tag, &hover_start, &hover_end);

  old_hover_start = gtk_text_iter_get_offset(&hover_start);
  old_hover_end = gtk_text_iter_get_offset(&hover_end);
  hover_underlined = true;
}


static void remove_hover_underline(void)
{
  if (hover_underlined)
    {
      GtkTextBuffer *buf;
      GtkTextIter hover_start, hover_end;
      buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(listener_text));
      gtk_text_buffer_get_iter_at_offset(buf, &hover_start, old_hover_start);
      gtk_text_buffer_get_iter_at_offset(buf, &hover_end, old_hover_end);
      gtk_text_buffer_remove_tag(buf, hover_tag, &hover_start, &hover_end);
      hover_underlined = false;
    }
}


static gboolean listener_mouse_move(GtkWidget *w, GdkEvent *ev, gpointer data)
{
  gint ev_x, ev_y, wx, wy;
  gdouble x, y;
  GtkTextIter iter, start, end;
  char *txt;

  gdk_event_get_coords(ev, &x, &y);
  ev_x = x;
  ev_y = y;

  remove_hover_underline();

  gtk_text_view_window_to_buffer_coords(GTK_TEXT_VIEW(listener_text), GTK_TEXT_WINDOW_TEXT, ev_x, ev_y, &wx, &wy);
  gtk_text_view_get_iter_at_location(GTK_TEXT_VIEW(listener_text), &iter, wx, wy);

#if 0
  start = iter;
  end = iter;

  /* if (gtk_text_iter_inside_word(&iter))
   */

  if (!gtk_text_iter_starts_word(&start))
    gtk_text_iter_backward_word_start(&start);
  if (!gtk_text_iter_ends_word(&end))
    gtk_text_iter_forward_word_end(&end);
  txt = gtk_text_buffer_get_text(LISTENER_BUFFER, &start, &end, true);

  /* now we have a word that is beneath the cursor, but this breaks at "-"! and not space!
   *   and typing doesn't count as a mouse movement, so we don't have anything we can use!
   *
   * so... I guess I have to get the entire text, go to the current position, and
   *    do all the text handling myself.  Suddenly this seems like a bad idea.
   *
   * I suppose I could check the mouse position against the text end
   *    then go to the mouse position in that and search backwards/forwards for a scheme word
   *    and if found, add underline using those "iters" -- the ones above are completely useless.
   *
   * snd-completion.c has separator_char_p
   * 
   */
#endif

  add_hover_underline(start, end);
  

  /* go into wait for nms timing out
   *        if motion kill and restart timer
   *        if timeout, post info about the underlined thing
   */
  return(false);
}

#endif



static bool cursor_set_blinks(GtkWidget *w, bool blinks)
{
  GtkSettings *settings;
  settings = gtk_widget_get_settings(w);
  g_object_set(settings, "gtk-cursor-blink", (gboolean)blinks, NULL);
  return(blinks);
}


static gboolean listener_key_release(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  check_parens();
#if WITH_LISTENER_TIPS
  remove_hover_underline();
#endif
  return(false);
}


static gboolean listener_key_press(GtkWidget *w, GdkEventKey *event, gpointer data)
{
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

  /* fprintf(stderr, "key: %d, state: %d\n", key, state); */

  if (key == snd_K_Tab)
    {
      /* this goes to end?  we need to look at current cursor and go from there */

      listener_completion(gtk_text_buffer_get_char_count(LISTENER_BUFFER));
      return(true);
    }

  if (key == snd_K_Return)
    listener_return_callback();
  else
    {
      if (((key == snd_K_g) || (key == snd_K_G)) && 
	  (state & snd_ControlMask))
	{
	  ss->C_g_typed = true; 
	  if (state & snd_MetaMask)
	    clear_listener();
	  else control_g(any_selected_sound());
	}
      else
	{
	  if ((((key == snd_K_a) || (key == snd_K_A)) && (state & snd_ControlMask)) ||
	      ((key == snd_K_Left) && (state == 0)))
	    {
	      back_to_start(true);
	    }
	  else
	    {
	      if (key == snd_K_BackSpace)
		{
		  int current_position;
		  char *fstr;
		  current_position = sg_cursor_position(listener_text);
		  if (current_position > 1)
		    {
		      fstr = sg_get_text(listener_text, current_position - 2, current_position);
		      if ((current_position != (printout_end - 2)) && 
			  (!(mus_strcmp(fstr, listener_prompt_with_cr()))))
			{
			  g_free(fstr);
			  return(false);
			}
		      g_free(fstr);
		    }
		}
	      else
		{
		  if ((key == snd_K_greater) && (state & snd_MetaMask))
		    {
		      int end;
		      end = gtk_text_buffer_get_char_count(LISTENER_BUFFER);
		      sg_set_cursor(listener_text, end + 1);
		    }
		  else
		    {
		      if (((key == snd_K_less) && (state & snd_MetaMask)) ||
			  ((key == snd_K_Up) && (state == 0)))
			{
			  sg_set_cursor(listener_text, ss->listener_prompt_length + 1);
			  return(true); /* keep the cursor on! */
			}
		      else 
			{
			  if (((key == snd_K_p) || (key == snd_K_P)) && (state & snd_MetaMask))
			    {
			      clear_back_to_prompt(listener_text);
			      restore_listener_string(true);
			    }
			  else 
			    {
			      if (((key == snd_K_n) || (key == snd_K_N)) && (state & snd_MetaMask))
				{
				  clear_back_to_prompt(listener_text);
				  restore_listener_string(false);
				}
			      else 
				{
				  if (((key == snd_K_question) && (state & snd_ControlMask)) ||
				      ((key == snd_K_period) && (state & snd_MetaMask)))
				    {
				      text_at_cursor(listener_text);
				    }
				  else
				    {
				      if (((key == snd_K_c) || (key == snd_K_C)) && (state & snd_MetaMask))
					{
					  /* M-c (as opposed to M-C) is trapped somewhere else */
					  word_upper(listener_text, true, false);
					} 
				      else
					{
					  if (((key == snd_K_l) || (key == snd_K_L)) && (state & snd_MetaMask))
					    {
					      word_upper(listener_text, false, false);
					    }
					  else
					    {
					      if (((key == snd_K_u) || (key == snd_K_U)) && (state & snd_MetaMask))
						{
						  word_upper(listener_text, false, true);
						}
					      else
						{
						  if (((key == snd_K_t) || (key == snd_K_T)) && (state & snd_ControlMask))
						    {
						      text_transpose(listener_text);
						    }
						  else
						    {
						      if ((key == snd_K_underscore) && (state & snd_ControlMask))
							{
							  backup_listener_to_previous_expression();
							}
						      else
							{
							  if ((key == snd_K_k) && (state & snd_ControlMask))
							    {
							      /* select to line end, copy to clipboard, delete */
							      ctrl_k(listener_text);
							    }
							  else
							    return(false);
							}}}}}}}}}}}}}}
  g_signal_stop_emission((gpointer)w, g_signal_lookup("key_press_event", G_OBJECT_TYPE((gpointer)w)), 0);
  return(false);
}


static XEN listener_click_hook; 

static gboolean listener_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  ss->graph_is_active = false;
  return(false);
}


static gboolean listener_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  if (EVENT_STATE(ev) & GDK_BUTTON2_MASK)
    {
      int end;
      end = gtk_text_buffer_get_char_count(LISTENER_BUFFER);
      sg_set_cursor(listener_text, end + 1);
      /* I guess ideally we'd go to the end of the current form.  At least this
       *   squelches the redundant copy of the text and leaves the cursor in most
       *   cases at the end of the form.
       */
    }
  check_parens();
  return(false);
}


static XEN mouse_enter_listener_hook;
static XEN mouse_leave_listener_hook;
static XEN mouse_enter_text_hook;
static XEN mouse_leave_text_hook;

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


static gboolean mouse_enter_text_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  if (with_pointer_focus(ss))
    goto_window(w);
  widget_modify_base(w, GTK_STATE_NORMAL, ss->white);
  if (XEN_HOOKED(mouse_enter_text_hook))
    run_hook(mouse_enter_text_hook,
	     XEN_LIST_1(XEN_WRAP_WIDGET(w)),
	     S_mouse_enter_text_hook);
  cursor_set_blinks(w, true);
  return(false);
}


static gboolean mouse_leave_text_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  widget_modify_base(w, GTK_STATE_NORMAL, ss->basic_color);
  if (XEN_HOOKED(mouse_leave_text_hook))
    run_hook(mouse_leave_text_hook,
	     XEN_LIST_1(XEN_WRAP_WIDGET(w)),
	     S_mouse_leave_text_hook);
  cursor_set_blinks(w, false);
  return(false);
}


void connect_mouse_to_text(GtkWidget *text)
{
  SG_SIGNAL_CONNECT(text, "enter_notify_event", mouse_enter_text_callback, NULL);
  SG_SIGNAL_CONNECT(text, "leave_notify_event", mouse_leave_text_callback, NULL);
}


static void make_bindings(gpointer cls)
{
  /* sigh... activate Emacs key bindings to some extent */
  GtkBindingSet *set;
  set = gtk_binding_set_by_class(cls);
  
  /* C-b back char */
  gtk_binding_entry_remove(set, snd_K_b, GDK_CONTROL_MASK);                  /* gdk/gdktypes.h */
  gtk_binding_entry_add_signal(set, snd_K_b, GDK_CONTROL_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_VISUAL_POSITIONS,   /* these are in gtk/gtkenums.h */
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);
  /* M-b back word */
  gtk_binding_entry_remove(set, snd_K_b, GDK_MOD1_MASK);
  gtk_binding_entry_add_signal(set, snd_K_b, GDK_MOD1_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_WORDS,
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);
  /* C-f forward char */
  gtk_binding_entry_remove(set, snd_K_f, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, snd_K_f, GDK_CONTROL_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_VISUAL_POSITIONS,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);
  /* OVERRIDDEN: M-f forward word */
  gtk_binding_entry_remove(set, snd_K_f, GDK_MOD1_MASK);
  gtk_binding_entry_add_signal(set, snd_K_f, GDK_MOD1_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_WORDS,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);
  /* C-e end of line */
  gtk_binding_entry_remove(set, snd_K_e, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, snd_K_e, GDK_CONTROL_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINE_ENDS,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);
  /* right-arrow end of line */
  gtk_binding_entry_remove(set, snd_K_Right, 0);
  gtk_binding_entry_add_signal(set, snd_K_Right, 0, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINE_ENDS,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);
  /* C-a start of line */
  gtk_binding_entry_remove(set, snd_K_a, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, snd_K_a, GDK_CONTROL_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINE_ENDS,
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);
  /* left-arrow start of line */
  gtk_binding_entry_remove(set, snd_K_Left, 0);
  gtk_binding_entry_add_signal(set, snd_K_Left, 0, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINE_ENDS,
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);
  /* M-< start of file */
  gtk_binding_entry_remove(set, snd_K_less, GDK_MOD1_MASK);
  gtk_binding_entry_add_signal(set, snd_K_less, GDK_MOD1_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_BUFFER_ENDS,
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);
  /* up-arrow start of file */
  gtk_binding_entry_remove(set, snd_K_Up, 0);
  gtk_binding_entry_add_signal(set, snd_K_Up, 0, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_BUFFER_ENDS,
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);
  /* M-> end of file */
  gtk_binding_entry_remove(set, snd_K_greater, GDK_MOD1_MASK);
  gtk_binding_entry_add_signal(set, snd_K_greater, GDK_MOD1_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_BUFFER_ENDS,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);
  /* down-arrow end of file */
  gtk_binding_entry_remove(set, snd_K_Down, 0);
  gtk_binding_entry_add_signal(set, snd_K_Down, 0, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_BUFFER_ENDS,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);
  /* C-n down line */
  gtk_binding_entry_remove(set, snd_K_n, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, snd_K_n, GDK_CONTROL_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINES,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);
  /* C-p up line */
  gtk_binding_entry_remove(set, snd_K_p, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, snd_K_p, GDK_CONTROL_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINES,
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);
  /* C-v down window */
  gtk_binding_entry_remove(set, snd_K_v, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, snd_K_v, GDK_CONTROL_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_PAGES,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);
  /* OVERRIDEN: M-v up window */
  gtk_binding_entry_remove(set, snd_K_v, GDK_MOD1_MASK);
  gtk_binding_entry_add_signal(set, snd_K_v, GDK_MOD1_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_PAGES,
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);
  /* C-d delete at cursor */
  gtk_binding_entry_remove(set, snd_K_d, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, snd_K_d, GDK_CONTROL_MASK,
			       "delete_from_cursor", 2,
			       G_TYPE_ENUM, GTK_DELETE_CHARS,
			       G_TYPE_INT, -1);
  /* M-d delete word at cursor */
  gtk_binding_entry_remove(set, snd_K_d, GDK_MOD1_MASK);
  gtk_binding_entry_add_signal(set, snd_K_d, GDK_MOD1_MASK,
			       "delete_from_cursor", 2,
			       G_TYPE_ENUM, GTK_DELETE_WORD_ENDS,
			       G_TYPE_INT, 1);
  
  /* C-k delete to end of line -- see explicit handling above */
  gtk_binding_entry_remove(set, snd_K_k, GDK_CONTROL_MASK);
  
  /* M-delete delete to start of line */
  gtk_binding_entry_remove(set, snd_K_Delete, GDK_MOD1_MASK);
  gtk_binding_entry_add_signal(set, snd_K_Delete, GDK_MOD1_MASK,
			       "delete_from_cursor", 2,
			       G_TYPE_ENUM, GTK_DELETE_PARAGRAPH_ENDS,
			       G_TYPE_INT, -1);
  
  /* C-w delete region -> clipboard */
  gtk_binding_entry_remove(set, snd_K_w, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, snd_K_w, GDK_CONTROL_MASK,
			       "cut_clipboard", 0);
  
  /* C-y yank <- clipboard */
  gtk_binding_entry_remove(set, snd_K_y, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, snd_K_y, GDK_CONTROL_MASK,
			       "paste_clipboard", 0);
}


static bool bindings_ok = false;

GtkWidget *snd_entry_new(GtkWidget *container, GtkWidget *prev, snd_entry_bg_t with_white_background)
{
  GtkWidget *text;
  GtkSettings *settings;

  text = gtk_entry_new();
  gtk_editable_set_editable(GTK_EDITABLE(text), true);
  add_entry_style(text);

  settings = gtk_widget_get_settings(text);
  g_object_set(settings, "gtk-entry-select-on-focus", false, NULL);

#if HAVE_GTK_GRID_NEW
  if (prev)
    {
      g_object_set(text, "margin", 2, NULL);
      gtk_widget_set_halign(text, GTK_ALIGN_FILL);
      gtk_widget_set_hexpand(text, true);
      gtk_grid_attach_next_to(GTK_GRID(container), text, prev, GTK_POS_RIGHT, 1, 1);
    }
  else gtk_box_pack_start(GTK_BOX(container), text, true, true, 2);
#else
  gtk_box_pack_start(GTK_BOX(container), text, true, true, 2);
#endif

  if (!bindings_ok)
    {
      bindings_ok = true;
      make_bindings(GTK_ENTRY_GET_CLASS(GTK_ENTRY(text)));
    }
  gtk_widget_show(text);

#if (!HAVE_GTK_3)
  if (with_white_background == WITH_WHITE_BACKGROUND) 
    {
      widget_modify_bg(text, GTK_STATE_NORMAL, ss->white);
      widget_modify_base(text, GTK_STATE_SELECTED, ss->white); 
    }
#endif
  connect_mouse_to_text(text);
  return(text);
}


GtkWidget *snd_entry_new_with_size(GtkWidget *container, int size)
{
  GtkWidget *text;
  GtkSettings *settings;

  text = gtk_entry_new();
  gtk_editable_set_editable(GTK_EDITABLE(text), true);
  gtk_entry_set_width_chars(GTK_ENTRY(text), size);
  add_entry_style(text);

  settings = gtk_widget_get_settings(text);
  g_object_set(settings, "gtk-entry-select-on-focus", false, NULL);

  gtk_box_pack_start(GTK_BOX(container), text, false, false, 4);

  if (!bindings_ok)
    {
      bindings_ok = true;
      make_bindings(GTK_ENTRY_GET_CLASS(GTK_ENTRY(text)));
    }
  gtk_widget_show(text);

#if (!HAVE_GTK_3)
  widget_modify_bg(text, GTK_STATE_NORMAL, ss->white);
  widget_modify_base(text, GTK_STATE_SELECTED, ss->white); 
#endif
  connect_mouse_to_text(text);
  return(text);
}


static void listener_help_callback(GtkWidget *w, gpointer info)
{
  /* for selected text or text near cursor or text near pointer, get help or completion,
   *    or at least apropos, show in help dialog
   */
  GtkTextIter start, end;
  if (gtk_text_buffer_get_selection_bounds(LISTENER_BUFFER, &start, &end))
    {
      char *txt;
      txt = gtk_text_buffer_get_text(LISTENER_BUFFER, &start, &end, true);
      if (txt) 
	{
	  char *trim_txt;
	  trim_txt = trim(txt);
	  if (trim_txt)
	    {
	      snd_help(trim_txt, XEN_TO_C_STRING(g_snd_help(C_TO_XEN_STRING(trim_txt), 0)), WITH_WORD_WRAP);
	      free(trim_txt);
	    }
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
  clear_listener();
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

      frame = gtk_frame_new(NULL);
      gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_ETCHED_IN);
      gtk_widget_show(frame);

      if (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS)
	gtk_paned_pack2(GTK_PANED(SOUND_PANE(ss)), frame, false, true); /* add2 but resize=false */
      else gtk_container_add(GTK_CONTAINER(MAIN_PANE(ss)), frame);

      listener_text = make_scrolled_text(frame, true, CONTAINER_ADD, false); 
      /* gtk_widget_set_name(listener_text, "listener_text"); */
      /* gtk_widget_set_margin_top(listener_text, 8); -- looks really dumb because it uses the outer bgcolor */
      set_listener_text_font();
      add_listener_style(listener_text);

      make_bindings(GTK_TEXT_VIEW_GET_CLASS(GTK_TEXT_VIEW(listener_text)));

      SG_SIGNAL_CONNECT(listener_text, "key_press_event", listener_key_press, NULL);
      SG_SIGNAL_CONNECT(listener_text, "key_release_event", listener_key_release, NULL);
      SG_SIGNAL_CONNECT(listener_text, "button_press_event", listener_button_press, NULL);
      SG_SIGNAL_CONNECT(listener_text, "button_release_event", listener_button_release, NULL);
      SG_SIGNAL_CONNECT(listener_text, "enter_notify_event", listener_focus_callback, NULL);
      SG_SIGNAL_CONNECT(listener_text, "leave_notify_event", listener_unfocus_callback, NULL);
      SG_SIGNAL_CONNECT(listener_text, "populate-popup", listener_popup_populate_callback, NULL);
#if WITH_LISTENER_TIPS
      SG_SIGNAL_CONNECT(listener_text, "motion_notify_event", listener_mouse_move, NULL);
#endif
      ss->listener_pane = listener_text;

      if (!prompt_not_editable) 
	prompt_not_editable = gtk_text_buffer_create_tag(LISTENER_BUFFER, "prompt_not_editable", 
							 "editable", false, 
							 "weight", PANGO_WEIGHT_BOLD,
							 NULL);
      {
	GtkTextIter pos;
	gtk_text_buffer_get_end_iter(LISTENER_BUFFER, &pos);
	gtk_text_buffer_insert_with_tags(LISTENER_BUFFER, &pos, listener_prompt(ss), ss->listener_prompt_length, prompt_not_editable, NULL);
      }

      gtk_widget_show(listener_text);

      /* an attempt to add the completions list here as the end of an hbox failed */
    }
}


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
  if (listener_text)
    sg_text_delete(listener_text, 1, sg_cursor_position(listener_text));
}


static XEN g_goto_listener_end(void)
{
  #define H_goto_listener_end "(" S_goto_listener_end "): move cursor and scroll to bottom of listener pane"
  if (listener_text)
    {
      int chars;
      chars = gtk_text_buffer_get_char_count(LISTENER_BUFFER);
      if (chars > 0) sg_set_cursor(listener_text, chars + 1);
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

  XEN_DEFINE_PROCEDURE(S_listener_selection, g_listener_selection_w,       0, 0, 0, H_listener_selection);
  XEN_DEFINE_PROCEDURE(S_reset_listener_cursor, g_reset_listener_cursor_w, 0, 0, 0, H_reset_listener_cursor);
  XEN_DEFINE_PROCEDURE(S_goto_listener_end, g_goto_listener_end_w,         0, 0, 0, H_goto_listener_end);

  #define H_listener_click_hook S_listener_click_hook " (position): called when listener clicked; position is text pos of click in listener"
  listener_click_hook = XEN_DEFINE_HOOK(S_listener_click_hook, "(make-hook 'position)", 1,   H_listener_click_hook); 
}


/* TODO: the edit history window is sometimes empty in gtk
 * doesn't c-g unselect? -- it was held up somewhere
 * why not c-r c-s in listener? where to prompt?
 * split this out as a separate "widget"
 * c-_ to undo, completion in g? and deletion from end is sometimes blocked?
 */

/* to get rid of 
 *    Fontconfig error: Cannot load default config file
 * and the consequent ridiculous fonts, since I think I built fontconfig from scratch,
 * copy (as root) /etc/fonts/fonts.conf to /usr/local/etc/fonts/fonts.conf
 */
