#include "snd.h"

static GtkWidget *completion_dialog = NULL;
static int first_time = TRUE;
static GtkWidget *listener_text = NULL, *completion_list = NULL;
static int printout_end;
#define LISTENER_BUFFER gtk_text_view_get_buffer(GTK_TEXT_VIEW(listener_text))

static void list_completions_callback(GtkTreeSelection *selection, gpointer *gp)
{
  GtkTreeIter iter;
  gchar *value;
  GtkTreeModel *model;
  GtkTreeSelection *tree;
  int beg, end, i, j, old_len, new_len;
  char *old_text;
  snd_state *ss = (snd_state *)gp;
  if (first_time)
    {
      first_time = FALSE;
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
}

static void dismiss_completion_callback(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(completion_dialog);
}

static void help_completion_callback(GtkWidget *w, gpointer context)
{
  snd_help(get_global_state(), "completion",
"These are the completions that Snd thinks might be likely.\n\
If you select one, it will be used to complete the current name.\n\
");
}

static void delete_completion_dialog(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(completion_dialog);
}

static void start_completion_dialog(int num_items, char **items)
{
  GtkWidget *help_button, *dismiss_button;
  snd_state *ss;
  GtkTreeSelection *tree;
  if (!completion_dialog)
    {
      ss = get_global_state();
      completion_dialog = gtk_dialog_new();
      g_signal_connect_closure_by_id(GTK_OBJECT(completion_dialog),
				     g_signal_lookup("delete_event", G_OBJECT_TYPE(GTK_OBJECT(completion_dialog))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(delete_completion_dialog), (gpointer)ss, 0),
				     0);
      gtk_window_set_title(GTK_WINDOW(completion_dialog), "Completions");
      sg_make_resizable(completion_dialog);
      set_background(completion_dialog, (ss->sgx)->basic_color);
      gtk_container_set_border_width (GTK_CONTAINER(completion_dialog), 4);
      gtk_widget_realize(completion_dialog);
      gtk_window_resize(GTK_WINDOW(completion_dialog), 260, 200);

      help_button = gtk_button_new_with_label("Help");
      dismiss_button = gtk_button_new_with_label("Dismiss");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(completion_dialog)->action_area), dismiss_button, FALSE, TRUE, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(completion_dialog)->action_area), help_button, FALSE, TRUE, 10);
      g_signal_connect_closure_by_id(GTK_OBJECT(dismiss_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(dismiss_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(dismiss_completion_callback), (gpointer)ss, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(help_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(help_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(help_completion_callback), (gpointer)ss, 0),
				     0);
      gtk_widget_show(dismiss_button);
      gtk_widget_show(help_button);
      first_time = TRUE;
      completion_list = sg_make_list("Completions", 
				     GTK_DIALOG(completion_dialog)->vbox,
				     CONTAINER_ADD,
				     (gpointer)ss,
				     num_items, items,
				     GTK_SIGNAL_FUNC(list_completions_callback),
				     0, 0, 0, 0);

      set_dialog_widget(ss, COMPLETION_DIALOG, completion_dialog);
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
  char *str = NULL;
  str = sg_get_text(listener_text, 0, -1);
  if (str)
    {
      fwrite((void *)str, sizeof(char), snd_strlen(str), fp);
      g_free(str);
    }
}

void append_listener_text(int end, char *msg)
{
  /* "end" arg needed in Motif */
  int chars;
  snd_state *ss;
  if (listener_print_p(msg))
    {
      ss = get_global_state();
      chars = gtk_text_buffer_get_char_count(LISTENER_BUFFER);
      if (chars > 0) sg_set_cursor(listener_text, chars + 1);
      sg_text_insert(listener_text, msg);
    }
}

static void listener_completion(snd_state *ss)
{
  int beg, end, matches = 0;
  char *old_text, *new_text = NULL, *file_text = NULL;
  int try_completion = 1;
  beg = printout_end + 1;
  end = gtk_text_buffer_get_char_count(LISTENER_BUFFER);
  if (end <= beg) return;
  old_text = sg_get_text(listener_text, beg, end);
  /* now old_text is the stuff typed since the last prompt */
  if (old_text)
    {
      new_text = complete_listener_text(old_text, end, &try_completion, &file_text);
      if (try_completion == 0)
	{
	  g_free(old_text);
	  return;
	}
      if (strcmp(old_text, new_text) == 0) 
	matches = get_completion_matches();
      sg_text_delete(listener_text, beg, end);
      append_listener_text(0, new_text);
      goto_window(listener_text);
      /*
      fprintf(stderr,"old: %s (%d %d), new: %s, delete from %d %d\n",
	      old_text,
	      beg,end,
	      new_text,
	      sg_cursor_position(listener_text) - (end - beg), end - beg);
      */
      if (new_text) 
	{
	  FREE(new_text); 
	  new_text = NULL;
	}
      if (matches > 1)
	{
	  clear_possible_completions();
	  set_save_completions(TRUE);
	  if (file_text) 
	    new_text = filename_completer(file_text); 
	  else new_text = command_completer(old_text);
	  if (new_text) 
	    {
	      FREE(new_text); 
	      new_text = NULL;
	    }
	  display_completions(ss);
	  set_save_completions(FALSE);
	  if (file_text) FREE(file_text);
	}
      if (old_text) g_free(old_text);
    }
}

void snd_completion_help(snd_state *ss, int matches, char **pbuffer) 
{
  if (matches > 0)
    start_completion_dialog(matches, pbuffer);
}


/* ---------------- command widget replacement ---------------- */

void listener_append(char *msg)
{
  if (listener_text)
    {
      snd_state *ss;
      ss = get_global_state();
      if ((ss->sgx)->graph_is_active)
	(ss->sgx)->graph_is_active = FALSE;
      append_listener_text(0, msg);
      printout_end = gtk_text_buffer_get_char_count(LISTENER_BUFFER) - 1;
    }
}

void listener_append_and_prompt(char *msg)
{
  int cmd_eot;
  if (listener_text)
    {
      if (msg)
	append_listener_text(0, msg);
      append_listener_text(0, listener_prompt_with_cr(get_global_state()));
      cmd_eot = gtk_text_buffer_get_char_count(LISTENER_BUFFER);
      printout_end = cmd_eot - 1;
    }
}

static void command_return_callback(snd_state *ss)
{
  if (!(ss->error_lock))
    command_return(listener_text, ss, printout_end);
}

static void back_to_start(snd_state *ss)
{
  char *full_str = NULL, *prompt;
  int i, start_of_text;
  full_str = sg_get_text(listener_text, 0, -1);
  start_of_text = sg_cursor_position(listener_text);
  prompt = listener_prompt(ss);
  if (start_of_text > 0)
    {
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

static void clear_back_to_prompt(GtkWidget *w)
{
  int beg, end;
  end = sg_cursor_position(w);
  back_to_start(get_global_state());
  beg = sg_cursor_position(w);
  if (end <= beg) return;
  sg_text_delete(w, beg, end);
}

static void listener_help(snd_state *ss)
{
  char *source = NULL;
  source = sg_get_text(listener_text, 0, -1);
  if (source)
    {
      provide_listener_help(source);
      g_free(source);
    }
}

static gboolean listener_key_press(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  snd_state *ss = (snd_state *)data;
  chan_info *cp;
  int end;

  /* fprintf(stderr,"got %d %c\n", event->state & snd_ControlMask, event->keyval); */
  if ((ss->sgx)->graph_is_active) 
    {
      cp = current_channel(ss);
      graph_key_press(channel_graph(cp), event, (gpointer)cp); 
      return(FALSE);
    }
  else
    {
      if (event->keyval == GDK_Tab)
	listener_completion(ss);
      else
	{
	  if (event->keyval == GDK_Return)
	    command_return_callback(ss);
	  else
	    {
	      if (((event->keyval == snd_K_g) || (event->keyval == snd_K_G)) && 
		  (event->state & snd_ControlMask))
		{
		  if (event->state & snd_MetaMask)
		    clear_listener();
		  else control_g(ss, any_selected_sound(ss));
		}
	      else
		{
		  if (((event->keyval == snd_K_a) || (event->keyval == snd_K_A)) && 
		      (event->state & snd_ControlMask))
		    {
		      back_to_start(ss);
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
				  (strcmp(fstr, listener_prompt_with_cr(ss)) != 0))
				{
				  g_free(fstr);
				  return(FALSE);
				}
			      g_free(fstr);
			    }
			}
		      else
			{
			  if ((event->keyval == snd_K_greater) && (event->state & snd_MetaMask))
			    {
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
				      restore_listener_string(TRUE);
				    }
				  else 
				    {
				      if (((event->keyval == snd_K_n) || (event->keyval == snd_K_N)) && (event->state & snd_MetaMask))
					{
					  clear_back_to_prompt(listener_text);
					  restore_listener_string(FALSE);
					}
				      else 
					{
					  if ((event->keyval == GDK_question) && (event->state & snd_ControlMask))
					    {
					      listener_help(ss);
					    }
					  else
					    {
					      return(FALSE);
					    }}}}}}}}}}}
  g_signal_stop_emission(GTK_OBJECT(w), g_signal_lookup("key_press_event", G_OBJECT_TYPE(GTK_OBJECT(w))), 0);
  return(FALSE);
}

static gint clear_paren_check(gpointer nada)
{
  /* major gtk bug here -- their own internal iterator gets fatally screwed up */
  return(0);
}

static gboolean check_parens(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  return(FALSE);
}

static gboolean listener_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_state *ss = (snd_state *)data;
  (ss->sgx)->graph_is_active = 0;
  goto_listener();
  return(FALSE);
}

static XEN mouse_enter_listener_hook;
static XEN mouse_leave_listener_hook;
static XEN mouse_enter_text_hook;
static XEN mouse_leave_text_hook;

static gboolean listener_focus_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  /* apparently called in gtkmarshal.c via gtk_marshal_BOOL__POINTER which passes 3 args */
  if (XEN_HOOKED(mouse_enter_listener_hook))
    run_hook(mouse_enter_listener_hook,
	     XEN_LIST_1(XEN_WRAP_WIDGET(listener_text)),
	     S_mouse_enter_listener_hook);
  return(FALSE);
}

static gboolean listener_unfocus_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  if (XEN_HOOKED(mouse_leave_listener_hook))
    run_hook(mouse_leave_listener_hook,
	     XEN_LIST_1(XEN_WRAP_WIDGET(listener_text)),
	     S_mouse_leave_listener_hook);
  return(FALSE);
}

static gboolean mouse_enter_text_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  if (XEN_HOOKED(mouse_enter_text_hook))
    run_hook(mouse_enter_text_hook,
	     XEN_LIST_1(XEN_WRAP_WIDGET(w)),
	     S_mouse_enter_text_hook);
  return(FALSE);
}

static gboolean mouse_leave_text_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  if (XEN_HOOKED(mouse_leave_text_hook))
    run_hook(mouse_leave_text_hook,
	     XEN_LIST_1(XEN_WRAP_WIDGET(w)),
	     S_mouse_leave_text_hook);
  return(FALSE);
}

GtkWidget *snd_entry_new(snd_state *ss, GtkWidget *container, int with_white_background)
{
  GtkWidget *text;
  text = gtk_entry_new();
  gtk_editable_set_editable(GTK_EDITABLE(text), TRUE);
  gtk_box_pack_start(GTK_BOX(container), text, TRUE, TRUE, 2);
  if (with_white_background) set_background(text, (ss->sgx)->white);
  gtk_widget_show(text);
  g_signal_connect_closure_by_id(GTK_OBJECT(text),
				 g_signal_lookup("enter_notify_event", G_OBJECT_TYPE(GTK_OBJECT(text))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(mouse_enter_text_callback), (gpointer)ss, 0),
				 0);
  g_signal_connect_closure_by_id(GTK_OBJECT(text),
				 g_signal_lookup("leave_notify_event", G_OBJECT_TYPE(GTK_OBJECT(text))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(mouse_leave_text_callback), (gpointer)ss, 0),
				 0);
  return(text);
}


static void make_command_widget(snd_state *ss, int height)
{
  GtkWidget *frame;
  if (!listener_text)
    {
      frame = gtk_frame_new(NULL);
      gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_ETCHED_IN);
      gtk_widget_show(frame);
      if (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS)
	gtk_paned_add2(GTK_PANED(SOUND_PANE(ss)), frame);
      else gtk_container_add(GTK_CONTAINER(MAIN_PANE(ss)), frame);
      listener_text = make_scrolled_text(ss, frame, TRUE, NULL, NULL);

      if (ss->sgx->listener_fnt) gtk_widget_modify_font(listener_text, ss->sgx->listener_fnt);
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
				     G_TYPE_BOOLEAN, FALSE);
	/* M-b back word */
	gtk_binding_entry_remove(set, GDK_b, GDK_MOD1_MASK);
	gtk_binding_entry_add_signal(set, GDK_b, GDK_MOD1_MASK, "move_cursor", 3,
				     G_TYPE_ENUM, GTK_MOVEMENT_WORDS,
				     G_TYPE_INT, -1,
				     G_TYPE_BOOLEAN, FALSE);
	/* C-f forward char */
	gtk_binding_entry_remove(set, GDK_f, GDK_CONTROL_MASK);
	gtk_binding_entry_add_signal(set, GDK_f, GDK_CONTROL_MASK, "move_cursor", 3,
				     G_TYPE_ENUM, GTK_MOVEMENT_VISUAL_POSITIONS,
				     G_TYPE_INT, 1,
				     G_TYPE_BOOLEAN, FALSE);
	/* OVERRIDDEN: M-f forward word */
	gtk_binding_entry_remove(set, GDK_f, GDK_MOD1_MASK);
	gtk_binding_entry_add_signal(set, GDK_f, GDK_MOD1_MASK, "move_cursor", 3,
				     G_TYPE_ENUM, GTK_MOVEMENT_WORDS,
				     G_TYPE_INT, 1,
				     G_TYPE_BOOLEAN, FALSE);
	/* C-e end of line */
	gtk_binding_entry_remove(set, GDK_e, GDK_CONTROL_MASK);
	gtk_binding_entry_add_signal(set, GDK_e, GDK_CONTROL_MASK, "move_cursor", 3,
				     G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINE_ENDS,
				     G_TYPE_INT, 1,
				     G_TYPE_BOOLEAN, FALSE);
	/* C-a start of line */
	gtk_binding_entry_remove(set, GDK_a, GDK_CONTROL_MASK);
	gtk_binding_entry_add_signal(set, GDK_a, GDK_CONTROL_MASK, "move_cursor", 3,
				     G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINE_ENDS,
				     G_TYPE_INT, -1,
				     G_TYPE_BOOLEAN, FALSE);
	/* M-< start of file */
	gtk_binding_entry_remove(set, GDK_less, GDK_MOD1_MASK);
	gtk_binding_entry_add_signal(set, GDK_less, GDK_MOD1_MASK, "move_cursor", 3,
				     G_TYPE_ENUM, GTK_MOVEMENT_BUFFER_ENDS,
				     G_TYPE_INT, -1,
				     G_TYPE_BOOLEAN, FALSE);
	/* M-> end of file */
	gtk_binding_entry_remove(set, GDK_greater, GDK_MOD1_MASK);
	gtk_binding_entry_add_signal(set, GDK_greater, GDK_MOD1_MASK, "move_cursor", 3,
				     G_TYPE_ENUM, GTK_MOVEMENT_BUFFER_ENDS,
				     G_TYPE_INT, 1,
				     G_TYPE_BOOLEAN, FALSE);
	/* C-n down line */
	gtk_binding_entry_remove(set, GDK_n, GDK_CONTROL_MASK);
	gtk_binding_entry_add_signal(set, GDK_n, GDK_CONTROL_MASK, "move_cursor", 3,
				     G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINES,
				     G_TYPE_INT, 1,
				     G_TYPE_BOOLEAN, FALSE);
	/* C-p up line */
	gtk_binding_entry_remove(set, GDK_p, GDK_CONTROL_MASK);
	gtk_binding_entry_add_signal(set, GDK_p, GDK_CONTROL_MASK, "move_cursor", 3,
				     G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINES,
				     G_TYPE_INT, -1,
				     G_TYPE_BOOLEAN, FALSE);
	/* C-v down window */
	gtk_binding_entry_remove(set, GDK_v, GDK_CONTROL_MASK);
	gtk_binding_entry_add_signal(set, GDK_v, GDK_CONTROL_MASK, "move_cursor", 3,
				     G_TYPE_ENUM, GTK_MOVEMENT_PAGES,
				     G_TYPE_INT, 1,
				     G_TYPE_BOOLEAN, FALSE);
	/* OVERRIDEN: M-v up window */
	gtk_binding_entry_remove(set, GDK_v, GDK_MOD1_MASK);
	gtk_binding_entry_add_signal(set, GDK_v, GDK_MOD1_MASK, "move_cursor", 3,
				     G_TYPE_ENUM, GTK_MOVEMENT_PAGES,
				     G_TYPE_INT, -1,
				     G_TYPE_BOOLEAN, FALSE);
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

	/*
	  TODO: more emacs keybinds from gtk+-2.1.1/gtk/gtktextview.c
	        C-k should place text in clipboard but gtk_clipboard_set_text has no effect
		Mod1 <Key>c:	    word-upper(c)\n\
		Ctrl <Key>t:	    text-transpose()\n\
		Mod1 <Key>u:	    word-upper(u)\n\
	*/
      }

      g_signal_connect_closure_by_id(GTK_OBJECT(listener_text),
				     g_signal_lookup("key_press_event", G_OBJECT_TYPE(GTK_OBJECT(listener_text))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(listener_key_press), (gpointer)ss, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(listener_text), 
				     g_signal_lookup("key_press_event", G_OBJECT_TYPE(GTK_OBJECT(listener_text))), 
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(check_parens), (gpointer)ss, 0),
				     1);
      g_signal_connect_closure_by_id(GTK_OBJECT(listener_text),
				     g_signal_lookup("button_press_event", G_OBJECT_TYPE(GTK_OBJECT(listener_text))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(listener_button_press), (gpointer)ss, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(listener_text),
				     g_signal_lookup("enter_notify_event", G_OBJECT_TYPE(GTK_OBJECT(listener_text))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(listener_focus_callback), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(listener_text),
				     g_signal_lookup("leave_notify_event", G_OBJECT_TYPE(GTK_OBJECT(listener_text))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(listener_unfocus_callback), NULL, 0),
				     0);
      ss->sgx->listener_pane = listener_text;
      sg_text_insert(listener_text, listener_prompt(ss));
      set_text_background(listener_text, (ss->sgx)->listener_color);
    }
}

void goto_listener(void) 
{
  goto_window(listener_text);
}

void color_listener(GdkColor *pix)
{
  snd_state *ss;
  ss = get_global_state();
  (ss->sgx)->listener_color = pix;
  if (listener_text) 
    set_text_background(listener_text, (ss->sgx)->listener_color);
}

void color_listener_text(GdkColor *pix)
{
  snd_state *ss;
  ss = get_global_state();
  (ss->sgx)->listener_text_color = pix;
}

void handle_listener(snd_state *ss, int open)
{
  if (open)
    {
      if (!listener_text)
	make_command_widget(ss, 100);
      else gtk_widget_show(listener_text);
      set_view_listener_label("Hide listener");
    }
  else
    {
      set_view_listener_label("Show listener");
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
  #define H_listener_selection "returns current selection in listener or #f"
  char *txt;
  XEN res = XEN_FALSE;
  if (listener_text)
    {
      GtkTextIter start, end;
      if (gtk_text_buffer_get_selection_bounds(LISTENER_BUFFER, &start, &end))
	{
	  txt = gtk_text_buffer_get_text(LISTENER_BUFFER, &start, &end, TRUE);
	  if (txt) 
	    {
	      res = C_TO_XEN_STRING(txt);
	      g_free(txt);
	    }
	}
    }
  return(res);
}

static XEN g_reset_listener_cursor(void)
{
  #define H_reset_listener_cursor "resets listener cursor to default pointer"
  snd_state *ss;
  if (listener_text)
    {
      ss = get_global_state();
      gdk_window_set_cursor(listener_text->window, (ss->sgx)->arrow_cursor);
    }
  return(XEN_FALSE);
}

void clear_listener(void)
{
  sg_text_delete(listener_text, 1, sg_cursor_position(listener_text));
}

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_listener_selected_text_w, g_listener_selected_text)
XEN_NARGIFY_0(g_reset_listener_cursor_w, g_reset_listener_cursor)
#else
#define g_listener_selected_text_w g_listener_selected_text
#define g_reset_listener_cursor_w g_reset_listener_cursor
#endif

void g_init_gxlistener(void)
{
  #define H_mouse_enter_listener_hook S_mouse_enter_listener_hook " (listener) is called when the mouse \
enters the lisp listener pane:\n\
  (add-hook! mouse-enter-listener-hook\n\
    (lambda (widget)\n\
      (focus-widget widget)))"

  #define H_mouse_leave_listener_hook S_mouse_leave_listener_hook " (listener) is called when the mouse \
leaves the lisp listener pane"

  XEN_DEFINE_HOOK(mouse_enter_listener_hook, S_mouse_enter_listener_hook, 1, H_mouse_enter_listener_hook);    /* arg = listener_text widget */
  XEN_DEFINE_HOOK(mouse_leave_listener_hook, S_mouse_leave_listener_hook, 1, H_mouse_leave_listener_hook);    /* arg = listener_text widget */

  #define H_mouse_enter_text_hook S_mouse_enter_text_hook " (widget) is called when the mouse enters a text widget:\n\
(add-hook! mouse-enter-text-hook\n\
  (lambda (w)\n\
    (focus-widget w)))"

  #define H_mouse_leave_text_hook S_mouse_leave_text_hook " (widget) is called when the mouse leaves a text widget"

  XEN_DEFINE_HOOK(mouse_enter_text_hook, S_mouse_enter_text_hook, 1, H_mouse_enter_text_hook);    /* arg = text widget */
  XEN_DEFINE_HOOK(mouse_leave_text_hook, S_mouse_leave_text_hook, 1, H_mouse_leave_text_hook);    /* arg = text widget */

  XEN_DEFINE_PROCEDURE(S_listener_selection, g_listener_selected_text_w, 0, 0, 0, H_listener_selection);
  XEN_DEFINE_PROCEDURE(S_reset_listener_cursor, g_reset_listener_cursor_w, 0, 0, 0, H_reset_listener_cursor);
}

