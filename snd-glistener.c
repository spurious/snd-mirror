#include "snd.h"

/* TODO  make completions list mouse sensitive as in Motif version (requires dialog etc)
 *        -> use click(select) callback!
 */

static GtkWidget *listener_text = NULL;
static int printout_end;

void save_listener_text(FILE *fp)
{
  char *str = NULL;
  str = SG_TEXT_CHARS(listener_text, 0, -1);
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
  ss = get_global_state();
  chars = SG_TEXT_LENGTH(listener_text);
  if (chars > 0) SG_TEXT_SET_POINT(listener_text, chars);
  SG_TEXT_INSERT(listener_text,
		 (ss->sgx)->listener_fnt,
		 (ss->sgx)->listener_text_color,
		 (ss->sgx)->listener_color,
		 msg,
		 -1);
}

static void activate_channel (snd_state *ss)
{
  /* make the current channel active and abort if anything in progress */
  chan_info *cp;
  clear_listener();
  ss->error_lock = 0;
  if (ss->checking_explicitly) ss->stopped_explicitly = 1; 
  cp = current_channel(ss);
  if (cp) goto_graph(cp);
  (ss->sgx)->graph_is_active = 1;
}

static void listener_completion(snd_state *ss)
{
  int beg, end, matches = 0, need_position;
  char *old_text, *new_text = NULL, *file_text = NULL;
  gint xoff, yoff; 
  int try_completion = 1;
  beg = printout_end + 1;
  end = SG_TEXT_LENGTH(listener_text);
  if (end <= beg) return;
  old_text = SG_TEXT_CHARS(listener_text, beg, end);
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
      SG_TEXT_BACKWARD_DELETE(listener_text, (end - beg));
      append_listener_text(0, new_text);
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
	  need_position = (!(help_dialog_is_active()));
	  display_completions(ss);
	  set_save_completions(FALSE);
	  if (need_position)
	    {
	      /* try to position the newly popped up help window below the text field */
	      gdk_window_get_origin(listener_text->window, &xoff, &yoff);
	      /* move_help_dialog_to(widget_x(listener_text) + xoff, widget_y(listener_text) + yoff + 140); */
	      move_help_dialog_to(widget_x(listener_text) + xoff, 
				  widget_y(listener_text) + yoff + 40);
	    }
	  if (file_text) FREE(file_text);
	}
      if (old_text) g_free(old_text);
    }
}

void snd_completion_help(snd_state *ss, int matches, char **pbuffer) 
{
  int i, len;
  char *buffer;
  if (matches > 0)
    {
      len = 0;
      for (i = 0; i < matches; i++) 
	len += (snd_strlen(pbuffer[i]) + 3);
      buffer = (char *)CALLOC(len, sizeof(char));
      for (i = 0; i < matches; i++)
	{
	  strcat(buffer, pbuffer[i]);
	  strcat(buffer, "\n");
	}
      snd_help(ss, "completions", buffer);
      FREE(buffer);
    }
}


/* ---------------- command widget replacement ---------------- */

void listener_append(snd_state *ss, char *msg)
{
  if (listener_text)
    {
      if ((ss->sgx)->graph_is_active)
	(ss->sgx)->graph_is_active = FALSE;
      append_listener_text(0, msg);
      printout_end = SG_TEXT_LENGTH(listener_text) - 1;
    }
}

void listener_append_and_prompt(snd_state *ss, char *msg)
{
  int cmd_eot;
  if (listener_text)
    {
      if (msg)
	append_listener_text(0, msg);
      append_listener_text(0, listener_prompt_with_cr(ss));
      cmd_eot = SG_TEXT_LENGTH(listener_text);
      printout_end = cmd_eot - 1;
    }
}

static void command_return_callback(snd_state *ss)
{
  if (!(ss->error_lock))
    command_return(listener_text, ss, printout_end);
}

static char *C_k_str = NULL;
static void grab_line(snd_state *ss)
{
  char *full_str;
  int current_position, last_position, i, j, k;
  full_str = SG_TEXT_CHARS(listener_text, 0, -1);
  current_position = SG_TEXT_GET_POINT(listener_text);
  last_position = SG_TEXT_LENGTH(listener_text);
  for (i = current_position; i < last_position; i++)
    if (full_str[i] == '\n')
      break;
  if (C_k_str) FREE(C_k_str);
  C_k_str = NULL;
  if (i > current_position)
    {
      C_k_str = (char *)CALLOC(i - current_position + 2, sizeof(char));
      for (j = current_position, k = 0; j < i; j++, k++) 
	C_k_str[k] = full_str[j];
    }
  if (full_str) g_free(full_str);
}

static void insert_line(snd_state *ss)
{
  if (C_k_str)
    SG_TEXT_INSERT(listener_text,
		   (ss->sgx)->listener_fnt,
		   (ss->sgx)->listener_text_color,
		   (ss->sgx)->listener_color,
		   C_k_str,
		   snd_strlen(C_k_str));
}

static void back_to_start(snd_state *ss)
{
  char *full_str = NULL, *prompt;
  int i, start_of_text;
  full_str = SG_TEXT_CHARS(listener_text, 0, -1);
  start_of_text = SG_TEXT_GET_POINT(listener_text);
  prompt = listener_prompt(ss);
  if (start_of_text > 0)
    {
      for (i = start_of_text; i >= 0; i--)
	if ((full_str[i] == prompt[0]) && 
	    ((i == 0) || (full_str[i-1] == '\n')))
	  {
	    start_of_text = i + 1;
	    break;
	  }
    }
  SG_TEXT_SET_POINT(listener_text, start_of_text);
  if (full_str) g_free(full_str);
}

static int last_highlight_position = -1;

static gint listener_key_press(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  snd_state *ss = (snd_state *)data;
  chan_info *cp;
  int end;

  if (last_highlight_position != -1)
    {
      SG_TEXT_UNSELECT(listener_text);
      last_highlight_position = -1;
    }

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
		activate_channel(ss);
	      else
		{
		  if (((event->keyval == snd_K_k) || (event->keyval == snd_K_K)) && 
		      (event->state & snd_ControlMask))
		    {
		      grab_line(ss);
		      return(FALSE);
		    }
		  else
		    {
		      if (((event->keyval == snd_K_y) || (event->keyval == snd_K_Y)) && 
			  (event->state & snd_ControlMask))
			{
			  insert_line(ss);
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
				  current_position = SG_TEXT_GET_POINT(listener_text);
				  if (current_position > 1)
				    {
				      fstr = SG_TEXT_CHARS(listener_text, current_position - 2, current_position);
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
				      end = SG_TEXT_LENGTH(listener_text);
				      SG_TEXT_SET_POINT(listener_text, end);
				    }
				  else
				    {
				      if ((event->keyval == snd_K_less) && (event->state & snd_MetaMask))
					{
					  SG_TEXT_SET_POINT(listener_text, 1);
					}
				      else 
					{
					  return(FALSE);
					}
				    }
				}
			    }
			}
		    }
		}
	    }
	}
    }
  SG_SIGNAL_EMIT_STOP_BY_NAME(GTK_OBJECT(w), "key_press_event");
  return(FALSE);
}

static gint clear_paren_check(gpointer nada)
{
  if (last_highlight_position != -1)
    {
      SG_TEXT_UNSELECT(listener_text);
      last_highlight_position = -1;
    }
  return(0);
}

static gint check_parens(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  int current_position;
  char *fstr, *prompt;
  int parens = 0, i;
  snd_state *ss;
  
  current_position = SG_TEXT_GET_POINT(listener_text);
  fstr = SG_TEXT_CHARS(listener_text, 0, -1);

  if (last_highlight_position != -1)
    {
      SG_TEXT_UNSELECT(listener_text);
      last_highlight_position = -1;
    }

  if ((current_position > 1) && 
      (fstr[current_position - 1] == ')'))
    {
      ss = get_global_state();
      parens = 1;
      prompt = listener_prompt(ss);
      for (i = current_position - 2; i > 0; i--)
	{
	  if ((i > 0) && (fstr[i] == prompt[0]) && (fstr[i - 1] == '\n'))
	    break;
	  if (fstr[i] == ')') parens++;
	  if (fstr[i] == '(') parens--;
	  if (parens == 0)
	    {
	      SG_TEXT_SELECT(listener_text, i, i + 1);
	      last_highlight_position = i;
	      gtk_timeout_add(300, clear_paren_check, NULL);
	      break;
	    }
	}
    }
  return(FALSE);
}

static void listener_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_state *ss = (snd_state *)data;
  (ss->sgx)->graph_is_active = 0;
  goto_listener();
}

static XEN mouse_enter_listener_hook;
static XEN mouse_leave_listener_hook;
static XEN mouse_enter_text_hook;
static XEN mouse_leave_text_hook;

static gint listener_focus_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  /* apparently called in gtkmarshal.c via gtk_marshal_BOOL__POINTER which passes 3 args */
  if (XEN_HOOKED(mouse_enter_listener_hook))
    g_c_run_progn_hook(mouse_enter_listener_hook,
		       XEN_LIST_1(XEN_WRAP_WIDGET(listener_text)),
		       S_mouse_enter_listener_hook);
  return(0);
}

static gint listener_unfocus_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  if (XEN_HOOKED(mouse_leave_listener_hook))
    g_c_run_progn_hook(mouse_leave_listener_hook,
		       XEN_LIST_1(XEN_WRAP_WIDGET(listener_text)),
		       S_mouse_leave_listener_hook);
  return(0);
}

static void mouse_enter_text_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  if (XEN_HOOKED(mouse_enter_text_hook))
    g_c_run_progn_hook(mouse_enter_text_hook,
		       XEN_LIST_1(XEN_WRAP_WIDGET(w)),
		       S_mouse_enter_text_hook);
}

static void mouse_leave_text_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  if (XEN_HOOKED(mouse_leave_text_hook))
    g_c_run_progn_hook(mouse_leave_text_hook,
		       XEN_LIST_1(XEN_WRAP_WIDGET(w)),
		       S_mouse_leave_text_hook);
}

GtkWidget *snd_entry_new(snd_state *ss, GtkWidget *container, int with_white_background)
{
  GtkWidget *text;
  text = gtk_entry_new();
  gtk_entry_set_editable(GTK_ENTRY(text), TRUE);
  gtk_box_pack_start(GTK_BOX(container), text, TRUE, TRUE, 2);
  if (with_white_background) set_background(text, (ss->sgx)->white);
  gtk_widget_show(text);
  SG_SIGNAL_CONNECT(GTK_OBJECT(text), "enter_notify_event", GTK_SIGNAL_FUNC(mouse_enter_text_callback), (gpointer)ss);
  SG_SIGNAL_CONNECT(GTK_OBJECT(text), "leave_notify_event", GTK_SIGNAL_FUNC(mouse_leave_text_callback), (gpointer)ss);
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
      SG_SIGNAL_CONNECT(GTK_OBJECT(listener_text), "key_press_event", GTK_SIGNAL_FUNC(listener_key_press), (gpointer)ss);
      SG_SIGNAL_CONNECT_AFTER(GTK_OBJECT(listener_text), "key_press_event", GTK_SIGNAL_FUNC(check_parens), (gpointer)ss);
      SG_SIGNAL_CONNECT(GTK_OBJECT(listener_text), "button_press_event", GTK_SIGNAL_FUNC(listener_button_press), (gpointer)ss);
      /* SG_SIGNAL_CONNECT_AFTER(GTK_OBJECT(listener_text), "button_press_event", GTK_SIGNAL_FUNC(after_listener_button_press), (gpointer)ss); */
      SG_SIGNAL_CONNECT(GTK_OBJECT(listener_text), "enter_notify_event", GTK_SIGNAL_FUNC(listener_focus_callback), NULL);
      SG_SIGNAL_CONNECT(GTK_OBJECT(listener_text), "leave_notify_event", GTK_SIGNAL_FUNC(listener_unfocus_callback), NULL);
      ss->sgx->listener_pane = listener_text;
      SG_TEXT_INSERT(listener_text,
		     (ss->sgx)->listener_fnt,
		     (ss->sgx)->listener_text_color,
		     (ss->sgx)->listener_color,
		     listener_prompt(ss),
		     -1);
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
      set_view_listener_label(STR_Hide_listener);
    }
  else
    {
      set_view_listener_label(STR_Show_listener);
      gtk_widget_hide(listener_text);
    }
  ss->listening = open;
}

int listener_height(void) {if (listener_text) return(widget_height(listener_text)); else return(0);}
int listener_width(void) {if (listener_text) return(widget_width(listener_text)); else return(0);}

static XEN g_listener_selected_text(void)
{
  #define H_listener_selection "returns current selection in listener or #f"
  char *txt;
  XEN res = XEN_FALSE;
  if (listener_text)
    {
#if HAVE_GTK2
      GtkTextIter start, end;
      if (gtk_text_buffer_get_selection_bounds(gtk_text_view_get_buffer(GTK_TEXT_VIEW(listener_text)), &start, &end))
	{
	  txt = gtk_text_buffer_get_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(listener_text)), &start, &end, TRUE);
#else
      if (GTK_EDITABLE(listener_text)->has_selection)
	{
	  txt = SG_TEXT_CHARS(listener_text,
			      GTK_EDITABLE(listener_text)->selection_start_pos,
			      GTK_EDITABLE(listener_text)->selection_end_pos);
#endif
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

