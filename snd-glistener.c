#include "snd.h"

/* TODO  make completions list mouse sensitive as in Motif version
 *        -> use click(select) callback!
 */

static GtkWidget *listener_text = NULL;
static GtkWidget *listener_pane = NULL; 
static int last_prompt;

void save_listener_text(FILE *fp)
{
  char *str = NULL;
  str = gtk_editable_get_chars(GTK_EDITABLE(listener_text), 0, -1);
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
  chars = gtk_text_get_length(GTK_TEXT(listener_text));
  if (chars > 0) gtk_text_set_point(GTK_TEXT(listener_text), chars);
  gtk_text_insert(GTK_TEXT(listener_text),
		  (ss->sgx)->listener_fnt,
		  (ss->sgx)->black,
		  (ss->sgx)->listener_color,
		  msg,
		  -1);
}

static void activate_channel (snd_state *ss)
{
  /* make the current channel active and abort if anything in progress */
  chan_info *cp;
  clear_listener();
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
  beg = last_prompt + 1;
  end = gtk_text_get_length(GTK_TEXT(listener_text));
  if (end <= beg) return;
  old_text = gtk_editable_get_chars(GTK_EDITABLE(listener_text), beg, end);
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
      gtk_text_backward_delete(GTK_TEXT(listener_text), (end - beg));
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

void snd_append_char(snd_state *ss, char *msg)
{
  if (listener_text)
    {
      if ((ss->sgx)->graph_is_active)
	(ss->sgx)->graph_is_active = FALSE;
      append_listener_text(0, msg);
    }
}

void snd_append_command(snd_state *ss, char *msg)
{
  int cmd_eot;
  if (listener_text)
    {
      if (ss->result_printout != PLAIN_MESSAGE) 
	append_listener_text(0, "\n");
      if (msg)
	append_listener_text(0, msg);
      if (ss->result_printout == MESSAGE_WITH_CARET) 
	append_listener_text(0, listener_prompt_with_cr(ss));
      ss->result_printout = 0;
      cmd_eot = gtk_text_get_length(GTK_TEXT(listener_text));
      last_prompt = cmd_eot - 1;
    }
}

static void command_return_callback(snd_state *ss)
{
  command_return(listener_text, ss, last_prompt);
}

static char *C_k_str = NULL;
static void grab_line(snd_state *ss)
{
  char *full_str;
  int current_position, last_position, i, j, k;
  full_str = gtk_editable_get_chars(GTK_EDITABLE(listener_text), 0, -1);
  current_position = gtk_editable_get_position(GTK_EDITABLE(listener_text));
  last_position = gtk_text_get_length(GTK_TEXT(listener_text));
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
    gtk_text_insert(GTK_TEXT(listener_text),
		    (ss->sgx)->listener_fnt,
		    (ss->sgx)->black,
		    (ss->sgx)->listener_color,
		    C_k_str,
		    snd_strlen(C_k_str));
}

static void back_to_start(snd_state *ss)
{
  char *full_str = NULL, *prompt;
  int i, start_of_text;
  full_str = gtk_editable_get_chars(GTK_EDITABLE(listener_text), 0, -1);
  start_of_text = gtk_editable_get_position(GTK_EDITABLE(listener_text));
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
  gtk_editable_set_position(GTK_EDITABLE(listener_text), start_of_text);
  if (full_str) g_free(full_str);
}

static gint listener_key_press(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  snd_state *ss = (snd_state *)data;
  chan_info *cp;
  int end;
  if ((ss->sgx)->graph_is_active) 
    {
      cp = current_channel(ss);
      graph_key_press(channel_graph(cp), event, (gpointer)cp); 
      return(TRUE);
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
		      return(TRUE);
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
				  current_position = gtk_editable_get_position(GTK_EDITABLE(listener_text));
				  if (current_position > 1)
				    {
				      fstr = gtk_editable_get_chars(GTK_EDITABLE(listener_text), current_position - 2, current_position);
				      if ((current_position != (last_prompt - 2)) && 
					  (strcmp(fstr, listener_prompt_with_cr(ss)) != 0))
					{
					  g_free(fstr);
					  return(TRUE);
					}
				      g_free(fstr);
				    }
				}
			      else
				{
				  if ((event->keyval == snd_K_greater) && (event->state & snd_MetaMask))
				    {
				      end = gtk_text_get_length(GTK_TEXT(listener_text));
				      gtk_text_set_point(GTK_TEXT(listener_text), end);
				      gtk_editable_set_position(GTK_EDITABLE(listener_text), end);
				    }
				  else
				    {
				      if ((event->keyval == snd_K_less) && (event->state & snd_MetaMask))
					{
					  gtk_text_set_point(GTK_TEXT(listener_text), 1);
					  gtk_editable_set_position(GTK_EDITABLE(listener_text), 1);
					}
				      else return(TRUE);
				    }
				}
			    }
			}
		    }
		}
	    }
	}
    }
  gtk_signal_emit_stop_by_name(GTK_OBJECT(w), "key_press_event");
  return(TRUE);
}

static void listener_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_state *ss = (snd_state *)data;
  (ss->sgx)->graph_is_active = 0;
}

static void sndCreateCommandWidget(snd_state *ss, int height)
{
  GtkWidget *hscrollbar, *vscrollbar, *frame;
  if (!listener_text)
    {
      frame = gtk_frame_new(NULL);
      gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_ETCHED_IN);
      gtk_widget_show(frame);
      if (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS)
	gtk_paned_add2(GTK_PANED(SOUND_PANE(ss)), frame);
      else gtk_container_add(GTK_CONTAINER(MAIN_PANE(ss)), frame);
      listener_pane = gtk_table_new (2, 2, FALSE);
      gtk_container_add(GTK_CONTAINER(frame), listener_pane);

      listener_text = gtk_text_new(NULL, NULL);
      gtk_table_attach (GTK_TABLE(listener_pane), listener_text, 0, 1, 0, 1, 
			(GtkAttachOptions)(GTK_FILL | GTK_EXPAND), 
			(GtkAttachOptions)(GTK_FILL | GTK_EXPAND | GTK_SHRINK),
			0, 0);
      gtk_text_set_editable(GTK_TEXT(listener_text), TRUE);
      gtk_text_set_word_wrap(GTK_TEXT(listener_text), FALSE);
      gtk_text_set_line_wrap(GTK_TEXT(listener_text), FALSE);
      gtk_signal_connect(GTK_OBJECT(listener_text), "key_press_event", GTK_SIGNAL_FUNC(listener_key_press), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(listener_text), "button_press_event", GTK_SIGNAL_FUNC(listener_button_press), (gpointer)ss);

      gtk_widget_show(listener_text);
      gtk_text_insert(GTK_TEXT(listener_text),
		      (ss->sgx)->listener_fnt,
		      (ss->sgx)->black,
		      (ss->sgx)->listener_color,
		      listener_prompt(ss),
		      -1);

      hscrollbar = gtk_hscrollbar_new(GTK_TEXT(listener_text)->hadj);
      set_background(hscrollbar, (ss->sgx)->position_color);
      gtk_table_attach(GTK_TABLE(listener_pane), hscrollbar, 0, 1, 1, 2, 
		       (GtkAttachOptions)(GTK_EXPAND | GTK_FILL), 
		       (GtkAttachOptions)(GTK_FILL), 
		       0, 0);
      gtk_widget_show(hscrollbar);
      vscrollbar = gtk_vscrollbar_new(GTK_TEXT(listener_text)->vadj);
      set_background(vscrollbar, (ss->sgx)->position_color);
      gtk_table_attach(GTK_TABLE(listener_pane), vscrollbar, 1, 2, 0, 1, 
		       (GtkAttachOptions)(GTK_FILL), 
		       (GtkAttachOptions)(GTK_EXPAND | GTK_FILL | GTK_SHRINK), 
		       0, 0);
      gtk_widget_show (vscrollbar);

      set_text_background(listener_text, (ss->sgx)->listener_color);
    }
  gtk_widget_show(listener_pane);
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

void handle_listener(snd_state *ss, int new_state)
{
  if (!listener_text)
    {
      /* fire up listener at bottom of overall snd window */
      if (new_state == LISTENER_OPEN) 
	{
	  sndCreateCommandWidget(ss, 100);
	  set_view_listener_label(STR_Hide_listener);
	  goto_window(listener_text);
	}
      else sndCreateCommandWidget(ss, 1);
      ss->listening = new_state;
    }
  else
    {
      if (ss->listening == LISTENER_OPEN)
	{
	  /* close listener window but it remains active */
	  ss->listening = LISTENER_LISTENING;
	  set_view_listener_label(STR_Show_listener);
	  gtk_widget_hide(listener_pane);
	}
      else
	{
	  /* reopen listener pane */
	  ss->listening = LISTENER_OPEN;
	  set_view_listener_label(STR_Hide_listener);
	  gtk_widget_show(listener_pane);
	}
    }
}

int listener_height(void) {if (listener_text) return(widget_height(listener_text)); else return(0);}

#if HAVE_GUILE_GTK
#include <guile-gtk.h>

#define Sg_listener_text_widget  "sg-listener-text-widget"

static SCM sg_listener_text_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(listener_text)));}

void init_listener_widgets(SCM local_doc)
{
  gh_new_procedure0_0(Sg_listener_text_widget, sg_listener_text_widget);
}

#endif
