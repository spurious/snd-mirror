#include "snd.h"

static GtkWidget *completion_dialog = NULL;
static GtkWidget *listener_text = NULL;
static slist *completion_list = NULL;
static int printout_end;
#define LISTENER_BUFFER gtk_text_view_get_buffer(GTK_TEXT_VIEW(listener_text))

static bool listener_awaiting_completion = false;

static void list_completions_callback(const char *name, int row, void *data)
{
  int beg, end, i, j, old_len, new_len;
  char *old_text;
  beg = printout_end + 1;
  end = gtk_text_buffer_get_char_count(LISTENER_BUFFER);
  old_text = sg_get_text(listener_text, beg, end);
  old_len = snd_strlen(old_text);
  new_len = snd_strlen(name);
  for (i = old_len - 1, j = new_len - 1; j >= 0; j--)
    {
      if (old_text[i] != name[j])
	{
	  i = old_len - 1;
	  if (old_text[i] == name[j]) i--;
	}
      else i--;
    }
  listener_awaiting_completion = false;
  append_listener_text(0, (char *)(name - 1 + old_len - i));
  if (old_text) g_free(old_text);
  gtk_widget_hide(completion_dialog);
}

static void dismiss_completion_callback(GtkWidget *w, gpointer context)
{
  listener_awaiting_completion = false;
  gtk_widget_hide(completion_dialog);
}

static void help_completion_callback(GtkWidget *w, gpointer context)
{
  completion_dialog_help();
}

static gint delete_completion_dialog(GtkWidget *w, GdkEvent *event, gpointer context)
{
  listener_awaiting_completion = false;
  gtk_widget_hide(completion_dialog);
  return(true);
}

static void start_completion_dialog(int num_items, char **items)
{
  if (!completion_dialog)
    {
      GtkWidget *help_button, *dismiss_button;
      completion_dialog = snd_gtk_dialog_new();
      SG_SIGNAL_CONNECT(completion_dialog, "delete_event", delete_completion_dialog, NULL);
      gtk_window_set_title(GTK_WINDOW(completion_dialog), _("Completions"));
      sg_make_resizable(completion_dialog);
      gtk_container_set_border_width (GTK_CONTAINER(completion_dialog), 4);
      gtk_widget_realize(completion_dialog);
      gtk_window_resize(GTK_WINDOW(completion_dialog), 260, 200);

      help_button = gtk_button_new_from_stock(GTK_STOCK_HELP);
      gtk_widget_set_name(help_button, "help_button");

      dismiss_button = gtk_button_new_from_stock(GTK_STOCK_QUIT);
      gtk_widget_set_name(dismiss_button, "quit_button");

      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(completion_dialog)->action_area), dismiss_button, false, true, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(completion_dialog)->action_area), help_button, false, true, 10);
      SG_SIGNAL_CONNECT(dismiss_button, "clicked", dismiss_completion_callback, NULL);
      SG_SIGNAL_CONNECT(help_button, "clicked", help_completion_callback, NULL);
      gtk_widget_show(dismiss_button);
      gtk_widget_show(help_button);
  
      completion_list = slist_new(GTK_DIALOG(completion_dialog)->vbox, items, num_items, CONTAINER_ADD);
      completion_list->select_callback = list_completions_callback;
      set_dialog_widget(COMPLETION_DIALOG, completion_dialog);
    }
  else 
    {
      int i;
      slist_clear(completion_list);
      for (i = 0; i < num_items; i++) 
	slist_append(completion_list, items[i]);
      raise_dialog(completion_dialog);
    }
  gtk_widget_show(completion_dialog);
}


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
	  bytes = fwrite((void *)str, sizeof(char), snd_strlen(str), fp);
	  g_free(str);
	  if (bytes == 0) return(-1);
	}
    }
  return(0);
}

void append_listener_text(int end, const char *msg)
{
  /* "end" arg needed in Motif */
  if ((listener_print_p(msg)) && 
      (listener_text))
    {
      int chars;
      chars = gtk_text_buffer_get_char_count(LISTENER_BUFFER);
      if (chars > 0) sg_set_cursor(listener_text, chars + 1);
      sg_text_insert(listener_text, (char *)msg);
    }
}

static GtkTextTag *prompt_not_editable = NULL;

static void append_listener_prompt()
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

static void listener_completion(int end)
{
  int beg;
  char *old_text;
  beg = printout_end + 1;
  if (end <= beg) return;
  old_text = sg_get_text(listener_text, beg, end);
  /* now old_text is the stuff typed since the last prompt */
  if (old_text)
    {
      char *new_text = NULL, *file_text = NULL;
      int matches = 0;
      bool try_completion = true;
      new_text = complete_listener_text(old_text, end, &try_completion, &file_text);
      if (!try_completion)
	{
	  g_free(old_text);
	  return;
	}
      if (strcmp(old_text, new_text) == 0) 
	matches = get_completion_matches();
      sg_text_delete(listener_text, beg, end);
      append_listener_text(0, new_text);
      goto_window(listener_text);
      if (new_text) 
	{
	  FREE(new_text); 
	  new_text = NULL;
	}
      if (matches > 1)
	{
	  clear_possible_completions();
	  set_save_completions(true);
	  if (file_text) 
	    new_text = filename_completer(file_text, NULL); 
	  else new_text = command_completer(old_text, NULL);
	  if (new_text) 
	    {
	      FREE(new_text); 
	      new_text = NULL;
	    }
	  display_completions();
	  listener_awaiting_completion = true;
	  set_save_completions(false);
	}
      if (file_text) FREE(file_text);
      if (old_text) g_free(old_text);
    }
}

void snd_completion_help(int matches, char **pbuffer) 
{
  if (matches > 0)
    start_completion_dialog(matches, pbuffer);
}


/* ---------------- command widget replacement ---------------- */

void listener_append(const char *msg)
{
  append_listener_text(0, msg); /* do this in any case to make sure print-hook gets to run (listener_print_p in append_listener_text) */
  if (listener_text)
    {
      if (ss->sgx->graph_is_active)
	ss->sgx->graph_is_active = false;
      printout_end = gtk_text_buffer_get_char_count(LISTENER_BUFFER) - 1;
    }
}

void listener_append_and_prompt(const char *msg)
{
  if (msg)
    append_listener_text(0, msg);
  append_listener_prompt();
  if (listener_text)
    {
      int cmd_eot;
      cmd_eot = gtk_text_buffer_get_char_count(LISTENER_BUFFER);
      printout_end = cmd_eot - 1;
    }
}

static void command_return_callback(void)
{
  if (!(ss->error_lock))
    command_return(listener_text, printout_end);
}

static void back_to_start(void)
{
  char *full_str = NULL, *prompt;
  int start_of_text;
  full_str = sg_get_text(listener_text, 0, -1);
  start_of_text = sg_cursor_position(listener_text);
  prompt = listener_prompt(ss);
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
  sg_set_cursor(listener_text, start_of_text + 1);
  if (full_str) g_free(full_str);
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
  back_to_start();
  beg = sg_cursor_position(w);
  if (end <= beg) return;
  sg_text_delete(w, beg, end);
}

#if HAVE_GTK_TEXT_BUFFER_SELECT_RANGE && HAVE_GTK_WIDGET_GET_CLIPBOARD
static void ctrl_k(GtkWidget *w)
{
  GtkTextIter beg, end;
  GtkTextBuffer *buf;
  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(w));
  gtk_text_buffer_get_iter_at_mark(buf, &beg, gtk_text_buffer_get_mark(buf, "insert"));
  end = beg;
  gtk_text_iter_forward_to_end(&end);
  if (!gtk_text_iter_equal(&beg, &end))
    {
      gtk_text_buffer_select_range(buf, &beg, &end);
      gtk_text_buffer_cut_clipboard(buf, gtk_widget_get_clipboard(w, GDK_SELECTION_CLIPBOARD), true);
    }
}
#endif

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


static void listener_help(void)
{
  char *source = NULL;
  source = sg_get_text(listener_text, 0, -1);
  if (source)
    {
      provide_listener_help(source);
      g_free(source);
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

static gboolean listener_key_release(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  check_parens();
  return(false);
}

static gboolean listener_key_press(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  if ((completion_dialog) &&
      (listener_awaiting_completion))
    {
      gtk_widget_hide(completion_dialog);
      listener_awaiting_completion = false;
    }
  if (ss->sgx->graph_is_active) 
    {
      chan_info *cp;
      cp = current_channel();
      graph_key_press(channel_graph(cp), event, (gpointer)cp); 
      return(false);
    }
  if (event->keyval == GDK_Tab)
    {
      listener_completion(gtk_text_buffer_get_char_count(LISTENER_BUFFER));
      return(true);
    }
  if (event->keyval == GDK_Return)
    command_return_callback();
  else
    {
      if (((event->keyval == snd_K_g) || (event->keyval == snd_K_G)) && 
	  (event->state & snd_ControlMask))
	{
	  if (event->state & snd_MetaMask)
	    clear_listener();
	  else control_g(any_selected_sound());
	}
      else
	{
	  if (((event->keyval == snd_K_a) || (event->keyval == snd_K_A)) && 
	      (event->state & snd_ControlMask))
	    {
	      back_to_start();
	    }
	  else
	    {
	      if (event->keyval == GDK_BackSpace)
		{
		  int current_position;
		  char *fstr;
		  current_position = sg_cursor_position(listener_text);
		  if (current_position > 1)
		    {
		      fstr = sg_get_text(listener_text, current_position - 2, current_position);
		      if ((current_position != (printout_end - 2)) && 
			  (strcmp(fstr, listener_prompt_with_cr()) != 0))
			{
			  g_free(fstr);
			  return(false);
			}
		      g_free(fstr);
		    }
		}
	      else
		{
		  if ((event->keyval == snd_K_greater) && (event->state & snd_MetaMask))
		    {
		      int end;
		      end = gtk_text_buffer_get_char_count(LISTENER_BUFFER);
		      sg_set_cursor(listener_text, end + 1);
		    }
		  else
		    {
		      if ((event->keyval == snd_K_less) && (event->state & snd_MetaMask))
			{
			  sg_set_cursor(listener_text, 2);
			}
		      else 
			{
			  if (((event->keyval == snd_K_p) || (event->keyval == snd_K_P)) && (event->state & snd_MetaMask))
			    {
			      clear_back_to_prompt(listener_text);
			      restore_listener_string(true);
			    }
			  else 
			    {
			      if (((event->keyval == snd_K_n) || (event->keyval == snd_K_N)) && (event->state & snd_MetaMask))
				{
				  clear_back_to_prompt(listener_text);
				  restore_listener_string(false);
				}
			      else 
				{
				  if ((event->keyval == GDK_question) && (event->state & snd_ControlMask))
				    {
				      listener_help();
				    }
				  else
				    {
				      if (((event->keyval == snd_K_c) || (event->keyval == snd_K_C)) && (event->state & snd_MetaMask))
					{
					  /* M-c (as opposed to M-C) is trapped somewhere else */
					  word_upper(listener_text, true, false);
					} 
				      else
					{
					  if (((event->keyval == snd_K_l) || (event->keyval == snd_K_L)) && (event->state & snd_MetaMask))
					    {
					      word_upper(listener_text, false, false);
					    }
					  else
					    {
					      if (((event->keyval == snd_K_u) || (event->keyval == snd_K_U)) && (event->state & snd_MetaMask))
						{
						  word_upper(listener_text, false, true);
						}
					      else
						{
						  if (((event->keyval == snd_K_t) || (event->keyval == snd_K_T)) && (event->state & snd_ControlMask))
						    {
						      text_transpose(listener_text);
						    }
						  else
						    {
						      if ((event->keyval == snd_K_underscore) && (event->state & snd_ControlMask))
							{
							  backup_listener_to_previous_command();
							}
						      else
							{
#if HAVE_GTK_TEXT_BUFFER_SELECT_RANGE && HAVE_GTK_WIDGET_GET_CLIPBOARD
							  if ((event->keyval == snd_K_k) && (event->state & snd_ControlMask))
							    {
							      /* select to line end, copy to clipboard, delete */
							      ctrl_k(listener_text);
							    }
							  else
#endif
							    return(false);
							}}}}}}}}}}}}}}
  g_signal_stop_emission(GTK_OBJECT(w), g_signal_lookup("key_press_event", G_OBJECT_TYPE(GTK_OBJECT(w))), 0);
  return(false);
}

static XEN listener_click_hook; 

static gboolean listener_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  ss->sgx->graph_is_active = false;
  return(false);
}

static gboolean listener_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
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

static bool cursor_set_blinks(GtkWidget *w, bool blinks)
{
  GtkSettings *settings;
  settings = gtk_widget_get_settings(w);
  g_object_set(settings, "gtk-cursor-blink", (gboolean)blinks, NULL);
  return(blinks);
}

static gboolean listener_focus_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
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
  gtk_widget_modify_base(w, GTK_STATE_NORMAL, ss->sgx->white);
  if (XEN_HOOKED(mouse_enter_text_hook))
    run_hook(mouse_enter_text_hook,
	     XEN_LIST_1(XEN_WRAP_WIDGET(w)),
	     S_mouse_enter_text_hook);
  cursor_set_blinks(w, true);
  return(false);
}

static gboolean mouse_leave_text_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  gtk_widget_modify_base(w, GTK_STATE_NORMAL, ss->sgx->basic_color);
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

GtkWidget *snd_entry_new(GtkWidget *container, snd_entry_bg_t with_white_background)
{
  GtkWidget *text;
  GtkSettings *settings;
  text = gtk_entry_new();
  gtk_editable_set_editable(GTK_EDITABLE(text), true);
  settings = gtk_widget_get_settings(text);
  g_object_set(settings, "gtk-entry-select-on-focus", false, NULL);
  gtk_box_pack_start(GTK_BOX(container), text, true, true, 2);
  gtk_widget_show(text);
  if (with_white_background == WITH_WHITE_BACKGROUND) 
    {
      gtk_widget_modify_bg(text, GTK_STATE_NORMAL, ss->sgx->white);
      gtk_widget_modify_base(text, GTK_STATE_SELECTED, ss->sgx->white); 
    }
  connect_mouse_to_text(text);
  return(text);
}

static void make_command_widget(int height)
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
      listener_text = make_scrolled_text(frame, true, NULL, false); /* last arg ignored here */
      gtk_widget_set_name(listener_text, "listener_text");

      {
	/* sigh... activate Emacs key bindings to some extent */
	/*   more are handled by the gtkrc mechanism (taken from gtk/gtrc.key.emacs) */
	GtkBindingSet *set;
	set = gtk_binding_set_by_class(GTK_TEXT_VIEW_GET_CLASS(GTK_TEXT_VIEW(listener_text)));

	/* C-b back char */
	gtk_binding_entry_remove(set, GDK_b, GDK_CONTROL_MASK);
	gtk_binding_entry_add_signal(set, GDK_b, GDK_CONTROL_MASK, "move_cursor", 3,
				     G_TYPE_ENUM, GTK_MOVEMENT_VISUAL_POSITIONS,
				     G_TYPE_INT, -1,
				     G_TYPE_BOOLEAN, false);
	/* M-b back word */
	gtk_binding_entry_remove(set, GDK_b, GDK_MOD1_MASK);
	gtk_binding_entry_add_signal(set, GDK_b, GDK_MOD1_MASK, "move_cursor", 3,
				     G_TYPE_ENUM, GTK_MOVEMENT_WORDS,
				     G_TYPE_INT, -1,
				     G_TYPE_BOOLEAN, false);
	/* C-f forward char */
	gtk_binding_entry_remove(set, GDK_f, GDK_CONTROL_MASK);
	gtk_binding_entry_add_signal(set, GDK_f, GDK_CONTROL_MASK, "move_cursor", 3,
				     G_TYPE_ENUM, GTK_MOVEMENT_VISUAL_POSITIONS,
				     G_TYPE_INT, 1,
				     G_TYPE_BOOLEAN, false);
	/* OVERRIDDEN: M-f forward word */
	gtk_binding_entry_remove(set, GDK_f, GDK_MOD1_MASK);
	gtk_binding_entry_add_signal(set, GDK_f, GDK_MOD1_MASK, "move_cursor", 3,
				     G_TYPE_ENUM, GTK_MOVEMENT_WORDS,
				     G_TYPE_INT, 1,
				     G_TYPE_BOOLEAN, false);
	/* C-e end of line */
	gtk_binding_entry_remove(set, GDK_e, GDK_CONTROL_MASK);
	gtk_binding_entry_add_signal(set, GDK_e, GDK_CONTROL_MASK, "move_cursor", 3,
				     G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINE_ENDS,
				     G_TYPE_INT, 1,
				     G_TYPE_BOOLEAN, false);
	/* C-a start of line */
	gtk_binding_entry_remove(set, GDK_a, GDK_CONTROL_MASK);
	gtk_binding_entry_add_signal(set, GDK_a, GDK_CONTROL_MASK, "move_cursor", 3,
				     G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINE_ENDS,
				     G_TYPE_INT, -1,
				     G_TYPE_BOOLEAN, false);
	/* M-< start of file */
	gtk_binding_entry_remove(set, GDK_less, GDK_MOD1_MASK);
	gtk_binding_entry_add_signal(set, GDK_less, GDK_MOD1_MASK, "move_cursor", 3,
				     G_TYPE_ENUM, GTK_MOVEMENT_BUFFER_ENDS,
				     G_TYPE_INT, -1,
				     G_TYPE_BOOLEAN, false);
	/* M-> end of file */
	gtk_binding_entry_remove(set, GDK_greater, GDK_MOD1_MASK);
	gtk_binding_entry_add_signal(set, GDK_greater, GDK_MOD1_MASK, "move_cursor", 3,
				     G_TYPE_ENUM, GTK_MOVEMENT_BUFFER_ENDS,
				     G_TYPE_INT, 1,
				     G_TYPE_BOOLEAN, false);
	/* C-n down line */
	gtk_binding_entry_remove(set, GDK_n, GDK_CONTROL_MASK);
	gtk_binding_entry_add_signal(set, GDK_n, GDK_CONTROL_MASK, "move_cursor", 3,
				     G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINES,
				     G_TYPE_INT, 1,
				     G_TYPE_BOOLEAN, false);
	/* C-p up line */
	gtk_binding_entry_remove(set, GDK_p, GDK_CONTROL_MASK);
	gtk_binding_entry_add_signal(set, GDK_p, GDK_CONTROL_MASK, "move_cursor", 3,
				     G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINES,
				     G_TYPE_INT, -1,
				     G_TYPE_BOOLEAN, false);
	/* C-v down window */
	gtk_binding_entry_remove(set, GDK_v, GDK_CONTROL_MASK);
	gtk_binding_entry_add_signal(set, GDK_v, GDK_CONTROL_MASK, "move_cursor", 3,
				     G_TYPE_ENUM, GTK_MOVEMENT_PAGES,
				     G_TYPE_INT, 1,
				     G_TYPE_BOOLEAN, false);
	/* OVERRIDEN: M-v up window */
	gtk_binding_entry_remove(set, GDK_v, GDK_MOD1_MASK);
	gtk_binding_entry_add_signal(set, GDK_v, GDK_MOD1_MASK, "move_cursor", 3,
				     G_TYPE_ENUM, GTK_MOVEMENT_PAGES,
				     G_TYPE_INT, -1,
				     G_TYPE_BOOLEAN, false);
	/* C-d delete at cursor */
	gtk_binding_entry_remove(set, GDK_d, GDK_CONTROL_MASK);
	gtk_binding_entry_add_signal(set, GDK_d, GDK_CONTROL_MASK,
				     "delete_from_cursor", 2,
				     G_TYPE_ENUM, GTK_DELETE_CHARS,
				     G_TYPE_INT, -1);
	/* M-d delete word at cursor */
	gtk_binding_entry_remove(set, GDK_d, GDK_MOD1_MASK);
	gtk_binding_entry_add_signal(set, GDK_d, GDK_MOD1_MASK,
				     "delete_from_cursor", 2,
				     G_TYPE_ENUM, GTK_DELETE_WORD_ENDS,
				     G_TYPE_INT, 1);

	/* C-k delete to end of line -- see explicit handling above */
	gtk_binding_entry_remove(set, GDK_k, GDK_CONTROL_MASK);
#if (!(HAVE_GTK_TEXT_BUFFER_SELECT_RANGE && HAVE_GTK_WIDGET_GET_CLIPBOARD))
	gtk_binding_entry_add_signal(set, GDK_k, GDK_CONTROL_MASK,
				     "delete_from_cursor", 2,
				     G_TYPE_ENUM, GTK_DELETE_PARAGRAPH_ENDS,
				     G_TYPE_INT, 1);
#endif

	/* M-delete delete to start of line */
	gtk_binding_entry_remove(set, GDK_Delete, GDK_MOD1_MASK);
	gtk_binding_entry_add_signal(set, GDK_Delete, GDK_MOD1_MASK,
				     "delete_from_cursor", 2,
				     G_TYPE_ENUM, GTK_DELETE_PARAGRAPH_ENDS,
				     G_TYPE_INT, -1);

	/* C-w delete region -> clipboard */
	gtk_binding_entry_remove(set, GDK_w, GDK_CONTROL_MASK);
	gtk_binding_entry_add_signal(set, GDK_w, GDK_CONTROL_MASK,
				     "cut_clipboard", 0);

	/* C-y yank <- clipboard */
	gtk_binding_entry_remove(set, GDK_y, GDK_CONTROL_MASK);
	gtk_binding_entry_add_signal(set, GDK_y, GDK_CONTROL_MASK,
				     "paste_clipboard", 0);
      }

      SG_SIGNAL_CONNECT(listener_text, "key_press_event", listener_key_press, NULL);
      SG_SIGNAL_CONNECT(listener_text, "key_release_event", listener_key_release, NULL);
      SG_SIGNAL_CONNECT(listener_text, "button_press_event", listener_button_press, NULL);
      SG_SIGNAL_CONNECT(listener_text, "button_release_event", listener_button_release, NULL);
      SG_SIGNAL_CONNECT(listener_text, "enter_notify_event", listener_focus_callback, NULL);
      SG_SIGNAL_CONNECT(listener_text, "leave_notify_event", listener_unfocus_callback, NULL);
      ss->sgx->listener_pane = listener_text;

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
    }
}

void goto_listener(void) 
{
  goto_window(listener_text);
}

void color_listener(GdkColor *pix)
{
  ss->sgx->listener_color = pix;
  if (listener_text) 
    gtk_widget_modify_base(listener_text, GTK_STATE_NORMAL, ss->sgx->listener_color);
}

void color_listener_text(GdkColor *pix)
{
  ss->sgx->listener_text_color = pix;
  if (listener_text) 
    gtk_widget_modify_text(listener_text, GTK_STATE_NORMAL, ss->sgx->listener_text_color);
}

void handle_listener(bool open)
{
  if (open)
    {
      int hgt;
      if (!listener_text)
	make_command_widget(100);
      hgt = widget_height(SOUND_PANE(ss));
      if (hgt > 100) /* we can get here before the sound window has opened, but with one pending.
		      *   the position is in terms of current size, which is useless in this case.
		      */
	{
	  if (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS)
	    gtk_paned_set_position(GTK_PANED(SOUND_PANE(ss)), (gint)(hgt * .75));
	}
    }
  else
    {
      if (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS)
	gtk_paned_set_position(GTK_PANED(SOUND_PANE(ss)), widget_height(SOUND_PANE(ss)));
    }
}

bool listener_exists(void)
{
  return((bool)listener_text);
}

int listener_height(void) 
{
  if ((listener_text) && (GTK_WIDGET_VISIBLE(listener_text)))
    return(widget_height(listener_text));
  else return(0);
}

int listener_width(void) 
{
  if ((listener_text) && (GTK_WIDGET_VISIBLE(listener_text)))
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
  if (listener_text)
    gtk_widget_modify_font(GTK_WIDGET(listener_text), LISTENER_FONT(ss));
}

static XEN g_reset_listener_cursor(void)
{
  #define H_reset_listener_cursor "(" S_reset_listener_cursor "): reset listener cursor to the default pointer"
  if (listener_text)
    gdk_window_set_cursor(listener_text->window, ss->sgx->arrow_cursor);
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
  #define H_mouse_enter_listener_hook S_mouse_enter_listener_hook " (listener): called when the mouse \
enters the lisp listener pane:\n\
  (add-hook! " S_mouse_enter_listener_hook "\n\
    (lambda (widget)\n\
      (" S_focus_widget " widget)))"
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

  #define H_mouse_leave_listener_hook S_mouse_leave_listener_hook " (listener): called when the mouse \
leaves the lisp listener pane"

  mouse_enter_listener_hook = XEN_DEFINE_HOOK(S_mouse_enter_listener_hook, 1, H_mouse_enter_listener_hook);    /* arg = listener_text widget */
  mouse_leave_listener_hook = XEN_DEFINE_HOOK(S_mouse_leave_listener_hook, 1, H_mouse_leave_listener_hook);    /* arg = listener_text widget */

#if HAVE_SCHEME
  #define H_mouse_enter_text_hook S_mouse_enter_text_hook " (widget): called when the mouse enters a text widget:\n\
(add-hook! " S_mouse_enter_text_hook "\n\
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

  mouse_enter_text_hook = XEN_DEFINE_HOOK(S_mouse_enter_text_hook, 1, H_mouse_enter_text_hook);    /* arg = text widget */
  mouse_leave_text_hook = XEN_DEFINE_HOOK(S_mouse_leave_text_hook, 1, H_mouse_leave_text_hook);    /* arg = text widget */

  XEN_DEFINE_PROCEDURE(S_listener_selection, g_listener_selection_w,       0, 0, 0, H_listener_selection);
  XEN_DEFINE_PROCEDURE(S_reset_listener_cursor, g_reset_listener_cursor_w, 0, 0, 0, H_reset_listener_cursor);
  XEN_DEFINE_PROCEDURE(S_goto_listener_end, g_goto_listener_end_w,         0, 0, 0, H_goto_listener_end);

  #define H_listener_click_hook S_listener_click_hook " (pos): called when listener clicked; pos is text pos of click in listener"
  listener_click_hook = XEN_DEFINE_HOOK(S_listener_click_hook, 1,    H_listener_click_hook);    /* arg = pos */
}

