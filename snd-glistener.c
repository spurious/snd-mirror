#include "snd.h"

static GtkWidget *completion_dialog = NULL;
static bool first_time = true;
static GtkWidget *listener_text = NULL, *completion_list = NULL;
static int printout_end;
#define LISTENER_BUFFER gtk_text_view_get_buffer(GTK_TEXT_VIEW(listener_text))

static void list_completions_callback(GtkTreeSelection *selection, gpointer *gp)
{
  GtkTreeIter iter;
  gchar *value = NULL;
  GtkTreeModel *model;
  int beg, end, i, j, old_len, new_len;
  char *old_text;
  if (first_time)
    {
      GtkTreeSelection *tree;
      first_time = false;
      tree = gtk_tree_view_get_selection(GTK_TREE_VIEW(completion_list));
      gtk_tree_selection_unselect_all(tree);
      return;
    }
  if (!(gtk_tree_selection_get_selected(selection, &model, &iter))) return;
  gtk_tree_model_get(model, &iter, 0, &value, -1);
  beg = printout_end + 1;
  end = gtk_text_buffer_get_char_count(LISTENER_BUFFER);
  old_text = sg_get_text(listener_text, beg, end);
  old_len = snd_strlen(old_text);
  new_len = snd_strlen(value);
  for (i = old_len - 1, j = new_len - 1; j >= 0; j--)
    {
      if (old_text[i] != value[j])
	{
	  i = old_len - 1;
	  if (old_text[i] == value[j]) i--;
	}
      else i--;
    }
  append_listener_text(0, (char *)(value - 1 + old_len - i));
  if (old_text) g_free(old_text);
  gtk_widget_hide(completion_dialog);
  if (value) g_free(value);
}

static void dismiss_completion_callback(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(completion_dialog);
}

static void help_completion_callback(GtkWidget *w, gpointer context)
{
  completion_dialog_help();
}

static gint delete_completion_dialog(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(completion_dialog);
  return(true);
}

static void start_completion_dialog(int num_items, char **items)
{
  GtkTreeSelection *tree;
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

      help_button = gtk_button_new_with_label(_("Help"));
      gtk_widget_set_name(help_button, "help_button");
      dismiss_button = gtk_button_new_with_label(_("Dismiss"));
      gtk_widget_set_name(dismiss_button, "quit_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(completion_dialog)->action_area), dismiss_button, false, true, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(completion_dialog)->action_area), help_button, false, true, 10);
      SG_SIGNAL_CONNECT(dismiss_button, "clicked", dismiss_completion_callback, NULL);
      SG_SIGNAL_CONNECT(help_button, "clicked", help_completion_callback, NULL);
      gtk_widget_show(dismiss_button);
      gtk_widget_show(help_button);
  
      first_time = true;
      completion_list = sg_make_list(_("Completions"), 
				     GTK_DIALOG(completion_dialog)->vbox,
				     CONTAINER_ADD,
				     NULL,
				     num_items, items,
				     GTK_SIGNAL_FUNC(list_completions_callback),
				     0, 0, 0, 0);

      set_dialog_widget(COMPLETION_DIALOG, completion_dialog);
    }
  else 
    {
      int i;
      GtkTreeIter iter;
      GtkListStore *model;
      model = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(completion_list)));
      gtk_list_store_clear(model);
      for (i = 0; i < num_items; i++) 
	{
	  gtk_list_store_append(model, &iter);
	  gtk_list_store_set(model, &iter, 0, items[i], -1);
	}
      raise_dialog(completion_dialog);
    }
  tree = gtk_tree_view_get_selection(GTK_TREE_VIEW(completion_list));
  gtk_tree_selection_unselect_all(tree);
  gtk_widget_show(completion_dialog);
}


void save_listener_text(FILE *fp)
{
  if (listener_text)
    {
      char *str = NULL;
      str = sg_get_text(listener_text, 0, -1);
      if (str)
	{
	  fwrite((void *)str, sizeof(char), snd_strlen(str), fp);
	  g_free(str);
	}
    }
}

void append_listener_text(int end, char *msg)
{
  /* "end" arg needed in Motif */
  if ((listener_print_p(msg)) && (listener_text))
    {
      int chars;
      chars = gtk_text_buffer_get_char_count(LISTENER_BUFFER);
      if (chars > 0) sg_set_cursor(listener_text, chars + 1);
      sg_text_insert(listener_text, msg);
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
	    new_text = filename_completer(file_text); 
	  else new_text = command_completer(old_text);
	  if (new_text) 
	    {
	      FREE(new_text); 
	      new_text = NULL;
	    }
	  display_completions();
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

void listener_append(char *msg)
{
  if (listener_text)
    {
      if (ss->sgx->graph_is_active)
	ss->sgx->graph_is_active = false;
      append_listener_text(0, msg);
      printout_end = gtk_text_buffer_get_char_count(LISTENER_BUFFER) - 1;
    }
}

void listener_append_and_prompt(char *msg)
{
  if (listener_text)
    {
      int cmd_eot;
      if (msg)
	append_listener_text(0, msg);
      append_listener_text(0, listener_prompt_with_cr());
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
	if ((full_str[i] == prompt[0]) && 
	    ((i == 0) || (full_str[i - 1] == '\n')))
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
  if (!flash_tag) flash_tag = gtk_text_buffer_create_tag (buf, "red_background", "background", "red", NULL);
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
	  parens = find_matching_paren(str, 2, pos - 1, listener_prompt(ss), &paren_pos);
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
	  parens = find_matching_paren(fstr, 1, current_position - 1, listener_prompt(ss), &pos);
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
  if (ss->sgx->graph_is_active) 
    {
      chan_info *cp;
      cp = current_channel();
      graph_key_press(channel_graph(cp), event, (gpointer)cp); 
      return(false);
    }
  else
    {
      if (event->keyval == GDK_Tab)
	{
	  listener_completion(gtk_text_buffer_get_char_count(LISTENER_BUFFER));
	  return(true);
	}
      else
	{
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
								return(false);
							    }}}}}}}}}}}}}}}
  g_signal_stop_emission(GTK_OBJECT(w), g_signal_lookup("key_press_event", G_OBJECT_TYPE(GTK_OBJECT(w))), 0);
  return(false);
}

static XEN listener_click_hook; 

static gboolean listener_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  ss->sgx->graph_is_active = false;
  /* this code from gedit/src/gedit-view.c.
   *    click in listener can place cursor (and autoscroll window) at any random place!
   *    so we have to explicitly find the correct place and put the cursor there.
   */
  {
    gint x, y;
    GtkTextIter iter;
    GtkTextIter start, end;
    GtkTextView *view = (GtkTextView *)w;
    gtk_text_view_window_to_buffer_coords(view, GTK_TEXT_WINDOW_TEXT, (gint)(ev->x), (gint)(ev->y), &x, &y);
    gtk_text_view_get_iter_at_location(view, &iter, x, y);
    if (!(gtk_text_buffer_get_selection_bounds(gtk_text_view_get_buffer(view), &start, &end) &&          
	  gtk_text_iter_in_range(&iter, &start, &end)))
      gtk_text_buffer_place_cursor(gtk_text_view_get_buffer(view), &iter);
    if (XEN_HOOKED(listener_click_hook))
      run_hook(listener_click_hook,
	       XEN_LIST_1(C_TO_XEN_INT((int)gtk_text_iter_get_offset(&iter))),
	       S_listener_click_hook);
  }
  goto_listener();
  check_parens();
  return(false);
}

static XEN mouse_enter_listener_hook;
static XEN mouse_leave_listener_hook;
static XEN mouse_enter_text_hook;
static XEN mouse_leave_text_hook;

static gboolean listener_focus_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  if (XEN_HOOKED(mouse_enter_listener_hook))
    run_hook(mouse_enter_listener_hook,
	     XEN_LIST_1(XEN_WRAP_WIDGET(listener_text)),
	     S_mouse_enter_listener_hook);
  return(false);
}

static gboolean listener_unfocus_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  if (XEN_HOOKED(mouse_leave_listener_hook))
    run_hook(mouse_leave_listener_hook,
	     XEN_LIST_1(XEN_WRAP_WIDGET(listener_text)),
	     S_mouse_leave_listener_hook);
  gtk_window_set_focus(GTK_WINDOW(gtk_widget_get_toplevel(w)), NULL);
  return(false);
}

static gboolean mouse_enter_text_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  gtk_widget_modify_base(w, GTK_STATE_NORMAL, ss->sgx->white);
  if (XEN_HOOKED(mouse_enter_text_hook))
    run_hook(mouse_enter_text_hook,
	     XEN_LIST_1(XEN_WRAP_WIDGET(w)),
	     S_mouse_enter_text_hook);
  return(false);
}

static gboolean mouse_leave_text_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  gtk_widget_modify_base(w, GTK_STATE_NORMAL, ss->sgx->basic_color);
  if (XEN_HOOKED(mouse_leave_text_hook))
    run_hook(mouse_leave_text_hook,
	     XEN_LIST_1(XEN_WRAP_WIDGET(w)),
	     S_mouse_leave_text_hook);
  gtk_window_set_focus(GTK_WINDOW(gtk_widget_get_toplevel(w)), NULL);
  /* apparently this widget hangs onto "focus" when the mouse leaves it,
   *  but that means the cursor keeps blinking, and that causes a noticeable
   *  amount of useless traffic to the X server, which we want to minimize
   *  during playing.
   */
  return(false);
}

void connect_mouse_to_text(GtkWidget *text)
{
  SG_SIGNAL_CONNECT(text, "enter_notify_event", mouse_enter_text_callback, NULL);
  SG_SIGNAL_CONNECT(text, "leave_notify_event", mouse_leave_text_callback, NULL);
}

GtkWidget *snd_entry_new(GtkWidget *container, bool with_white_background)
{
  GtkWidget *text;
  text = gtk_entry_new();
  gtk_editable_set_editable(GTK_EDITABLE(text), true);
  gtk_box_pack_start(GTK_BOX(container), text, true, true, 2);
  gtk_widget_show(text);
  if (with_white_background) gtk_widget_modify_bg(text, GTK_STATE_NORMAL, ss->sgx->white);
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
	gtk_paned_add2(GTK_PANED(SOUND_PANE(ss)), frame);
      else gtk_container_add(GTK_CONTAINER(MAIN_PANE(ss)), frame);
      listener_text = make_scrolled_text(frame, true, NULL, NULL);
      gtk_widget_set_name(listener_text, "listener_text");

      {
	/* sigh... activate Emacs key bindings to some extent */
	/*   these appear to be set in gtk+-2.1.1/gtk/gtkrc.key.emacs */
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

	/* C-k delete to end of line */
	gtk_binding_entry_remove(set, GDK_k, GDK_CONTROL_MASK);
	gtk_binding_entry_add_signal(set, GDK_k, GDK_CONTROL_MASK,
				     "delete_from_cursor", 2,
				     G_TYPE_ENUM, GTK_DELETE_PARAGRAPH_ENDS,
				     G_TYPE_INT, 1);

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
      SG_SIGNAL_CONNECT(listener_text, "enter_notify_event", listener_focus_callback, NULL);
      SG_SIGNAL_CONNECT(listener_text, "leave_notify_event", listener_unfocus_callback, NULL);
      ss->sgx->listener_pane = listener_text;
      sg_text_insert(listener_text, listener_prompt(ss));
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
      if (!listener_text)
	make_command_widget(100);
      else gtk_widget_show(listener_text);
      set_view_listener_label(_("Hide listener"));
    }
  else
    {
      set_view_listener_label(_("Show listener"));
      gtk_widget_hide(listener_text);
    }
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

static XEN g_listener_selected_text(void)
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
XEN_NARGIFY_0(g_listener_selected_text_w, g_listener_selected_text)
XEN_NARGIFY_0(g_reset_listener_cursor_w, g_reset_listener_cursor)
XEN_NARGIFY_0(g_goto_listener_end_w, g_goto_listener_end)
#else
#define g_listener_selected_text_w g_listener_selected_text
#define g_reset_listener_cursor_w g_reset_listener_cursor
#define g_goto_listener_end_w g_goto_listener_end
#endif

void g_init_gxlistener(void)
{
#if HAVE_GUILE
  #define H_mouse_enter_listener_hook S_mouse_enter_listener_hook " (listener): called when the mouse \
enters the lisp listener pane:\n\
  (add-hook! " S_mouse_enter_listener_hook "\n\
    (lambda (widget)\n\
      (" S_focus_widget " widget)))"
#else
  #define H_mouse_enter_listener_hook S_mouse_enter_listener_hook " (listener): called when the mouse \
enters the lisp listener pane:\n\
  $mouse_enter_listener_hook.add-hook!(\"enter\") do |widget|\n\
    focus_widget(widget)\n\
  end"
#endif

  #define H_mouse_leave_listener_hook S_mouse_leave_listener_hook " (listener): called when the mouse \
leaves the lisp listener pane"

  XEN_DEFINE_HOOK(mouse_enter_listener_hook, S_mouse_enter_listener_hook, 1, H_mouse_enter_listener_hook);    /* arg = listener_text widget */
  XEN_DEFINE_HOOK(mouse_leave_listener_hook, S_mouse_leave_listener_hook, 1, H_mouse_leave_listener_hook);    /* arg = listener_text widget */

#if HAVE_GUILE
  #define H_mouse_enter_text_hook S_mouse_enter_text_hook " (widget): called when the mouse enters a text widget:\n\
(add-hook! " S_mouse_enter_text_hook "\n\
  (lambda (w)\n\
    (" S_focus_widget " w)))"
#else
  #define H_mouse_enter_text_hook S_mouse_enter_text_hook " (widget): called when the mouse enters a text widget:\n\
$mouse_enter_text_hook.add_hook!(\"enter\") do |w|\n\
    focus_widget(w)\n\
  end"
#endif

  #define H_mouse_leave_text_hook S_mouse_leave_text_hook " (widget): called when the mouse leaves a text widget"

  XEN_DEFINE_HOOK(mouse_enter_text_hook, S_mouse_enter_text_hook, 1, H_mouse_enter_text_hook);    /* arg = text widget */
  XEN_DEFINE_HOOK(mouse_leave_text_hook, S_mouse_leave_text_hook, 1, H_mouse_leave_text_hook);    /* arg = text widget */

  XEN_DEFINE_PROCEDURE(S_listener_selection, g_listener_selected_text_w, 0, 0, 0, H_listener_selection);
  XEN_DEFINE_PROCEDURE(S_reset_listener_cursor, g_reset_listener_cursor_w, 0, 0, 0, H_reset_listener_cursor);
  XEN_DEFINE_PROCEDURE(S_goto_listener_end, g_goto_listener_end_w, 0, 0, 0, H_goto_listener_end);

  #define H_listener_click_hook S_listener_click_hook " (pos): called when listener clicked; pos is text pos of click in listener"
  XEN_DEFINE_HOOK(listener_click_hook,    S_listener_click_hook, 1,    H_listener_click_hook);    /* arg = pos */
}

