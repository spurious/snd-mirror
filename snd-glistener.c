#include "snd.h"

/* TODO: kill ring, c-a should not back over prompt
 *       there's cursor move and delete_text events apparently
 *         also changed -- see gtkeditable.h
 */

static GtkWidget *listener_text = NULL;
static GtkWidget *listener_pane = NULL; 
static int last_prompt;

static void append_listener_text(snd_state *ss, char *msg)
{
  int chars;
  chars = gtk_text_get_length(GTK_TEXT(listener_text));
  if (chars > 0) gtk_text_set_point(GTK_TEXT(listener_text),chars);
  gtk_text_insert(GTK_TEXT(listener_text),(ss->sgx)->listener_fnt,(ss->sgx)->black,(ss->sgx)->listener_color,msg,-1);
}

void set_graph_active(int val)
{
  snd_state *ss;
  ss = get_global_state();
  /* fprintf(stderr,"set: %d ",val); */
  (ss->sgx)->graph_is_active = val;
}

static void Activate_channel (snd_state *ss)
{
  /* make the current channel active and abort if anything in progress */
  chan_info *cp;
  if (ss->checking_explicitly) ss->stopped_explicitly = 1; 
  cp = current_channel(ss);
  if (cp) goto_graph(cp);
  (ss->sgx)->graph_is_active = 1;
  /* fprintf(stderr,"activate -> true "); */
}

static int find_indentation(char *str,int loc)
{
  int line_beg = 0,open_paren = -1,parens,i;
  parens = 0;
  for (i=loc-1;i>=0;i--)
    {
      if (str[i] == ')') parens--;
      if (str[i] == '(') parens++;
      if (parens == 1) {open_paren = i; break;}
    }
  if (open_paren == -1) return(1);
  if (open_paren == 0) return(3);
  for (i=open_paren-1;i>0;i--)
    {
      if (str[i] == '\n') {line_beg = i; break;}
    }
  if (line_beg == 0) return(1);
  return(open_paren - line_beg + 2);
}

static void Listener_completion(snd_state *ss)
{
  /* used only by the listener widget -- needs to be smart about text since overall string can be enormous 
   *   and we don't want to back up past the last prompt
   *   also if at start of line (or all white-space to previous \n, indent
   */
  int beg,end,len,i,k,matches = 0,need_position,spaces,text_pos = 0,cr_pos = 0;
  char *old_text,*new_text = NULL,*file_text = NULL,*new_file = NULL;
  gint xoff,yoff; 
  beg = last_prompt+1;
  end = gtk_text_get_length(GTK_TEXT(listener_text));
  if (end <= beg) return;
  old_text = gtk_editable_get_chars(GTK_EDITABLE(listener_text),beg,end);
  /* now old_text is the stuff typed since the last prompt */
  if (old_text)
    {
      len = strlen(old_text);
      for (i=len-1;i>0;i--)
	{
	  if (old_text[i] == '\n')
	    {
	      /* tab as indentation */
	      /* look at previous line to decide */
	      spaces = find_indentation(old_text,i);
	      if (spaces > 0)
		{
		  file_text = (char *)CALLOC(spaces+1,sizeof(char));
		  for (k=0;k<spaces;k++) file_text[k] = ' ';
		  file_text[spaces] = 0;
		  append_listener_text(ss,file_text);
		  FREE(file_text);
		  file_text = NULL;
		}
	      free(old_text);
	      old_text = NULL;
	      return;
	    }
	  if (old_text[i] == ';')
	    {
	      /* this isn't quite right, but how much effort should we put in it? */
	      spaces = 20;
	      for (k=i-1;k>0;k--) 
		if (old_text[k] == '\n') 
		  {cr_pos = k; break;} 
		else 
		  if ((!(isspace((int)(old_text[k])))) && (text_pos == 0)) 
		    text_pos = k;
	      if (text_pos > 0)
		text_pos -= cr_pos;
	      if (cr_pos == 0) spaces--; 
	      if (text_pos < spaces)
		{
		  file_text = (char *)CALLOC(spaces+2,sizeof(char));
		  for (k=text_pos+1;k<spaces;k++) file_text[k-text_pos-1] = ' ';
		  file_text[spaces] = ';';
		  file_text[spaces+1] = 0;
		  append_listener_text(ss,file_text);
		  FREE(file_text);
		}
	      free(old_text);
	      return;
	    }
	  if (old_text[i] == '\"')
	    {
	      file_text = copy_string((char *)(old_text+i+1));
	      new_file = filename_completer(file_text);
	      len = i + 2 + snd_strlen(new_file);
	      new_text = (char *)CALLOC(len,sizeof(char));
	      strncpy(new_text,old_text,i+1);
	      strcat(new_text,new_file);
	      if (new_file) FREE(new_file);
	      break;
	    }
	  if (isspace((int)(old_text[i]))) break;
	}
      if (new_text == NULL) new_text = command_completer(old_text);
      if (strcmp(old_text,new_text) == 0) matches = get_completion_matches();
      gtk_text_backward_delete(GTK_TEXT(listener_text),(end-beg));
      append_listener_text(ss,new_text);
      if (new_text) {FREE(new_text); new_text = NULL;}
      if (matches > 1)
	{
	  clear_possible_completions();
	  set_save_completions(TRUE);
	  if (file_text) new_text = filename_completer(file_text); else new_text = command_completer(old_text);
	  if (new_text) {FREE(new_text); new_text = NULL;}
	  need_position = (!(help_dialog_is_active()));
	  display_completions(ss);
	  set_save_completions(FALSE);
	  if (need_position)
	    {
	      /* try to position the newly popped up help window below the text field */
	      gdk_window_get_origin(listener_text->window,&xoff,&yoff);
	      /* move_help_dialog_to(widget_x(listener_text)+xoff,widget_y(listener_text)+yoff+140); */
	      move_help_dialog_to(widget_x(listener_text)+xoff,widget_y(listener_text)+yoff+40);
	    }
	  if (file_text) FREE(file_text);
	}
      if (old_text) free(old_text);
    }
}

/* ---------------- command widget replacement ---------------- */

void snd_append_char(snd_state *ss, char *msg)
{
  if (listener_text)
    {
      if ((ss->sgx)->graph_is_active)
	{
	  /* fprintf(stderr,"append->false "); */
	  (ss->sgx)->graph_is_active = FALSE;
	}
      else append_listener_text(ss,msg);
    }
}

static char listener_prompt_buffer[4];
static char *listener_prompt_with_cr(snd_state *ss)
{
  sprintf(listener_prompt_buffer,"\n%s",listener_prompt(ss));
  return(listener_prompt_buffer);
}
 
void snd_append_command(snd_state *ss, char *msg)
{
  int cmd_eot;
  if (listener_text)
    {
      if (ss->result_printout != PLAIN_MESSAGE) 
	append_listener_text(ss,"\n");
      if (msg)
	append_listener_text(ss,msg);
      if (ss->result_printout == MESSAGE_WITH_CARET) 
	append_listener_text(ss,listener_prompt_with_cr(ss));
      ss->result_printout = 0;
      cmd_eot = gtk_text_get_length(GTK_TEXT(listener_text));
      last_prompt = cmd_eot-1;
    }
}

static void Command_Return_Callback(snd_state *ss)
{
  /* try to find complete form either enclosing current cursor, or just before it */
  gint new_eot=0,cmd_eot=0;
  char *str = NULL,*full_str = NULL,*prompt;
  int i,j,slen;
  int end_of_text,start_of_text,last_position,current_position,parens;
  full_str = gtk_editable_get_chars(GTK_EDITABLE(listener_text),0,-1);
  current_position = (GTK_EDITABLE(listener_text))->current_pos; /* is this gtk_editable_get_position? */
  start_of_text = current_position;
  end_of_text = current_position;
  last_position = gtk_text_get_length(GTK_TEXT(listener_text));
  prompt = listener_prompt(ss);
  if (last_position > end_of_text)
    {
      for (i=current_position;i<last_position;i++)
	if ((full_str[i+1] == prompt[0]) && (full_str[i] == '\n'))
	  {
	    end_of_text = i-1;
	    break;
	  }
    }
  if (start_of_text > 0)
    {
      for (i=current_position;i>=0;i--)
	if ((full_str[i] == prompt[0]) && ((i == 0) || (full_str[i-1] == '\n')))
	  {
	    start_of_text = i+1;
	    break;
	  }
      if (start_of_text == end_of_text)
	start_of_text = 0; /* user erased prompt? */
    }
  str = NULL;
  if (end_of_text > start_of_text)
    {
      parens = 0;
      slen = end_of_text - start_of_text + 2;
      str = (char *)CALLOC(slen,sizeof(char));
      for (i=start_of_text,j=0;i<=end_of_text;j++,i++) {str[j] = full_str[i]; if (str[j] == '(') parens++;}
      str[end_of_text-start_of_text+1] = 0;
      end_of_text = snd_strlen(str);
      if (parens)
	{
	  end_of_text = check_balance(str,0,end_of_text);
	  if ((end_of_text > 0) && (end_of_text < (slen-1)))
	    {
	      str[end_of_text+1] = 0;
	      if (str[end_of_text] == '\n') str[end_of_text]=0;
	    }
	  else
	    {
	      FREE(str);
	      str = NULL;
	      new_eot = gtk_text_get_length(GTK_TEXT(listener_text));
	      append_listener_text(ss,"\n");
	      /* gtk_text_set_point(GTK_TEXT(listener_text),gtk_text_get_length(GTK_TEXT(listener_text))); */
	      gtk_editable_set_position(GTK_EDITABLE(listener_text),gtk_text_get_length(GTK_TEXT(listener_text)));
	      return;
	    }
	}
      if (str)
	{
	  if (current_position < (last_position-2))
	    {
	      append_listener_text(ss,str);
	    }
	  gdk_window_set_cursor(listener_text->window,(ss->sgx)->wait_cursor);
	  snd_eval_listener_str(ss,str);
	  gdk_window_set_cursor(listener_text->window,(ss->sgx)->arrow_cursor);
	  FREE(str);
	  str = NULL;
	}
      else
	{
	  new_eot = gtk_text_get_length(GTK_TEXT(listener_text));
	  append_listener_text(ss,listener_prompt_with_cr(ss));
	}
      last_prompt = gtk_text_get_length(GTK_TEXT(listener_text)) - 1;
    }
  else 
    {
      new_eot = gtk_text_get_length(GTK_TEXT(listener_text));
      append_listener_text(ss,"\n");
    }
  cmd_eot = gtk_text_get_length(GTK_TEXT(listener_text));
  /* gtk_text_set_point(GTK_TEXT(listener_text),cmd_eot); */
  gtk_editable_set_position(GTK_EDITABLE(listener_text),cmd_eot); 
  if (full_str) free(full_str);
}

static gint listener_key_press(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  snd_state *ss = (snd_state *)data;
  chan_info *cp;
  if ((ss->sgx)->graph_is_active) 
    {
      cp = current_channel(ss);
      graph_key_press(channel_graph(cp),event,(gpointer)cp); 
      gtk_signal_emit_stop_by_name(GTK_OBJECT(w),"key_press_event");
      return(TRUE);
    }
  if (event->keyval == GDK_Tab)
    {
      Listener_completion(ss);
      gtk_signal_emit_stop_by_name(GTK_OBJECT(w),"key_press_event");
      return(TRUE);
    }
  if (event->keyval == GDK_Return)
    {
      Command_Return_Callback(ss);
      gtk_signal_emit_stop_by_name(GTK_OBJECT(w),"key_press_event");
      return(TRUE);
    }
  if ((event->keyval == snd_K_g) && (event->state & snd_ControlMask))
    {
      Activate_channel(ss);
      gtk_signal_emit_stop_by_name(GTK_OBJECT(w),"key_press_event");
      return(TRUE);
    }

  /* also backspace over our prompt */
  
  return(FALSE);
}

static void listener_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_state *ss = (snd_state *)data;
  (ss->sgx)->graph_is_active = 0;
}

static void sndCreateCommandWidget(snd_state *ss, int height)
{
  GtkWidget *hscrollbar,*vscrollbar,*frame;
  if (!listener_text)
    {
      frame = gtk_frame_new(NULL);
      gtk_frame_set_shadow_type(GTK_FRAME(frame),GTK_SHADOW_ETCHED_IN);
      gtk_widget_show(frame);
      gtk_paned_add2(GTK_PANED(SOUND_PANE(ss)),frame);
      listener_pane = gtk_table_new (2, 2, FALSE);
      gtk_container_add(GTK_CONTAINER(frame),listener_pane);

      listener_text = gtk_text_new(NULL, NULL);
      gtk_table_attach (GTK_TABLE(listener_pane), listener_text, 0, 1, 0, 1, GTK_FILL | GTK_EXPAND, GTK_FILL | GTK_EXPAND | GTK_SHRINK, 0, 0);
      gtk_text_set_editable(GTK_TEXT(listener_text),TRUE);
      gtk_text_set_word_wrap(GTK_TEXT(listener_text),FALSE);
      gtk_text_set_line_wrap(GTK_TEXT(listener_text),FALSE);
      gtk_signal_connect(GTK_OBJECT(listener_text),"key_press_event",GTK_SIGNAL_FUNC(listener_key_press),(gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(listener_text),"button_press_event",GTK_SIGNAL_FUNC(listener_button_press),(gpointer)ss);

      gtk_widget_show(listener_text);
      gtk_text_insert(GTK_TEXT(listener_text),(ss->sgx)->listener_fnt,(ss->sgx)->black,(ss->sgx)->listener_color,listener_prompt(ss),-1);

      hscrollbar = gtk_hscrollbar_new(GTK_TEXT(listener_text)->hadj);
      set_background(hscrollbar,(ss->sgx)->position_color);
      gtk_table_attach(GTK_TABLE(listener_pane), hscrollbar, 0, 1, 1, 2, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 0);
      gtk_widget_show(hscrollbar);
      vscrollbar = gtk_vscrollbar_new(GTK_TEXT(listener_text)->vadj);
      set_background(vscrollbar,(ss->sgx)->position_color);
      gtk_table_attach(GTK_TABLE(listener_pane),vscrollbar, 1, 2, 0, 1, GTK_FILL, GTK_EXPAND | GTK_FILL | GTK_SHRINK, 0, 0);
      gtk_widget_show (vscrollbar);

      set_text_background(listener_text,(ss->sgx)->listener_color);
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
  if (listener_text) set_text_background(listener_text,(ss->sgx)->listener_color);
}

void handle_listener(snd_state *ss, int new_state)
{
  if (!listener_text)
    {
      /* fire up listener at bottom of overall snd window */
      if (new_state == LISTENER_OPEN) 
	{
	  sndCreateCommandWidget(ss,100);
	  set_view_listener_label(STR_Hide_listener);
	  goto_window(listener_text);
	}
      else sndCreateCommandWidget(ss,1);
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
  gh_new_procedure0_0(Sg_listener_text_widget,sg_listener_text_widget);
}

#endif
