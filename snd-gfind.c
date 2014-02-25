#include "snd.h"

static GtkWidget *edit_find_dialog, *edit_find_text, *cancelB, *edit_find_label, *next_button, *previous_button;
static GtkWidget *find_error_frame = NULL, *find_error_label = NULL;
static chan_info *find_channel = NULL;
static gulong find_key_press_handler_id = 0;


static void clear_find_error(void)
{
  if ((find_error_frame) && (widget_is_active(find_error_frame)))
    set_label(find_error_label, "");
  if (find_key_press_handler_id)
    {
      g_signal_handler_disconnect(edit_find_text, find_key_press_handler_id);
      find_key_press_handler_id = 0;
    }
}


static gboolean find_modify_key_press(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  clear_find_error();
  return(false);
}


void errors_to_find_text(const char *msg, void *data)
{
  find_dialog_set_label(msg);
  gtk_widget_show(find_error_frame);
  find_key_press_handler_id = SG_SIGNAL_CONNECT(edit_find_text, "key_press_event", find_modify_key_press, NULL);
}


void stop_search_if_error(const char *msg, void *data)
{
  errors_to_find_text(msg, data);
  ss->stopped_explicitly = true; /* should be noticed in global_search in snd-find.c */
}


static void edit_find_dismiss(GtkWidget *w, gpointer context) 
{ 
  if (ss->checking_explicitly)
    ss->stopped_explicitly = true;
  else 
    {
      gtk_widget_hide(edit_find_dialog);
      clear_find_error();
    }
} 


static gint edit_find_delete(GtkWidget *w, GdkEvent *event, gpointer context)
{
  clear_find_error();
  gtk_widget_hide(edit_find_dialog);
  return(true);
}


static void edit_find_help(GtkWidget *w, gpointer context) 
{
  find_dialog_help();
} 


static void edit_find_find(read_direction_t direction, GtkWidget *w, gpointer context) 
{
#if HAVE_EXTENSION_LANGUAGE
  find_dialog_find((char *)gtk_entry_get_text(GTK_ENTRY(edit_find_text)), direction, find_channel);
#endif
}


void find_dialog_stop_label(bool show_stop)
{
  if (show_stop)
    set_stock_button_label(cancelB, I_STOP);
  else set_stock_button_label(cancelB, I_GO_AWAY); 
}


void find_dialog_set_label(const char *str) 
{
  if (edit_find_label) 
    set_label(edit_find_label, str);
}


static void edit_find_next(GtkWidget *w, gpointer context) 
{
  edit_find_find(READ_FORWARD, w, context);
}


static void edit_find_previous(GtkWidget *w, gpointer context) 
{
  edit_find_find(READ_BACKWARD, w, context);
}


static void make_edit_find_dialog(bool managed, chan_info *cp)
{
  if (!edit_find_dialog)
    {
      GtkWidget *dl, *rc;
      GtkWidget *help_button;
      edit_find_dialog = snd_gtk_dialog_new();
      SG_SIGNAL_CONNECT(edit_find_dialog, "delete_event", edit_find_delete, NULL);
      gtk_window_set_title(GTK_WINDOW(edit_find_dialog), I_FIND);
      sg_make_resizable(edit_find_dialog);
      gtk_container_set_border_width (GTK_CONTAINER(edit_find_dialog), 10);
      gtk_window_resize(GTK_WINDOW(edit_find_dialog), 350, 120);
      gtk_widget_realize(edit_find_dialog);

      help_button = button_new_with_icon(ICON_HELP);
      gtk_widget_set_name(help_button, "dialog_button");

      cancelB = sg_button_new_with_label_and_icon(I_GO_AWAY, ICON_QUIT);
      gtk_widget_set_name(cancelB, "dialog_button");

      previous_button = button_new_with_icon(ICON_GO_BACK);
      gtk_widget_set_name(previous_button, "dialog_button");

      next_button = button_new_with_icon(ICON_GO_FORWARD);
      gtk_widget_set_name(next_button, "dialog_button");

      gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(edit_find_dialog)), next_button, true, true, 10);
      gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(edit_find_dialog)), previous_button, true, true, 10);
      gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(edit_find_dialog)), cancelB, true, true, 10);
      gtk_box_pack_end(GTK_BOX(DIALOG_ACTION_AREA(edit_find_dialog)), help_button, true, true, 10);

#if HAVE_GTK_3
      add_highlight_button_style(cancelB);
      add_highlight_button_style(help_button);
      add_highlight_button_style(previous_button);
      add_highlight_button_style(next_button);
#endif

      SG_SIGNAL_CONNECT(cancelB, "clicked", edit_find_dismiss, NULL);
      SG_SIGNAL_CONNECT(help_button, "clicked", edit_find_help, NULL);
      SG_SIGNAL_CONNECT(next_button, "clicked", edit_find_next, NULL);
      SG_SIGNAL_CONNECT(previous_button, "clicked", edit_find_previous, NULL);

      gtk_widget_show(cancelB);
      gtk_widget_show(next_button);
      gtk_widget_show(previous_button);
      gtk_widget_show(help_button);
      

      rc = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(edit_find_dialog)), rc, true, true, 4);
      gtk_widget_show(rc);

      dl = gtk_label_new(I_find);
      gtk_box_pack_start(GTK_BOX(rc), dl, false, false, 4);
      gtk_widget_show(dl);

      edit_find_text = snd_entry_new(rc, NULL, WITH_WHITE_BACKGROUND);
      SG_SIGNAL_CONNECT(edit_find_text, "activate", edit_find_next, NULL);
      
      edit_find_label = gtk_label_new("");
      gtk_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(edit_find_dialog)), edit_find_label, false, false, 4);
      gtk_widget_show(edit_find_label);


      find_error_frame = gtk_frame_new(NULL);
      gtk_box_pack_end(GTK_BOX(DIALOG_CONTENT_AREA(edit_find_dialog)), find_error_frame, false, false, 4);

      find_error_label = gtk_label_new("");
      gtk_container_add(GTK_CONTAINER(find_error_frame), find_error_label);
      gtk_widget_show(find_error_label);


      if (managed) gtk_widget_show(edit_find_dialog);
      set_dialog_widget(FIND_DIALOG, edit_find_dialog);
    }
  else 
    {
      if (managed) raise_dialog(edit_find_dialog);
    }
}


void edit_find_callback(GtkWidget *w, gpointer context)
{
  make_edit_find_dialog(true, NULL);
}


void find_dialog(chan_info *cp)
{
  /* used in snd-kbd.c */
  make_edit_find_dialog(true, cp);
}


bool find_dialog_is_active(void)
{
  return((edit_find_dialog) && (widget_is_active(edit_find_dialog)));
}


void save_find_dialog_state(FILE *fd)
{
  if (find_dialog_is_active())
    {
      char *text = NULL;
      text = sg_get_text(edit_find_text, 0, -1);
#if HAVE_SCHEME
      if (text)
	fprintf(fd, "(%s #t \"%s\")\n", S_find_dialog, text);
      else
	{
	  if (ss->search_expr)
	    fprintf(fd, "(%s #t \"%s\")\n", S_find_dialog, ss->search_expr);
	  else fprintf(fd, "(%s #t)\n", S_find_dialog);
	}
#endif
#if HAVE_RUBY
      if (text)
	fprintf(fd, "%s(true, \"%s\")\n", TO_PROC_NAME(S_find_dialog), text);
      else
	{
	  if (ss->search_expr)
	    fprintf(fd, "%s(true, \"%s\")\n", TO_PROC_NAME(S_find_dialog), ss->search_expr);
	  else fprintf(fd, "%s(true)\n", TO_PROC_NAME(S_find_dialog));
	}
#endif
#if HAVE_FORTH
      if (text)
	fprintf(fd, "#t \"%s\" %s drop\n", text, S_find_dialog);
      else
	{
	  if (ss->search_expr)
	    fprintf(fd, "#t \"%s\" %s drop\n", ss->search_expr, S_find_dialog);
	  else fprintf(fd, "#t %s drop\n", S_find_dialog);
	}
#endif
      if (text) g_free(text);
    }
}


static XEN g_find_dialog(XEN managed, XEN text)
{
  #define H_find_dialog "(" S_find_dialog " :optional managed text): create and activate the Edit:Find dialog, return the dialog widget"

  XEN_ASSERT_TYPE(Xen_is_boolean_or_unbound(managed), managed, 1, S_find_dialog, "a boolean");
  XEN_ASSERT_TYPE(Xen_is_string_or_unbound(text), text, 2, S_find_dialog, "a string");

  make_edit_find_dialog(XEN_TO_C_BOOLEAN(managed), NULL);
  if ((edit_find_text) && (Xen_is_string(text)))
    gtk_entry_get_text(GTK_ENTRY(edit_find_text));

  return(XEN_WRAP_WIDGET(edit_find_dialog));
}


static XEN g_find_dialog_widgets(void)
{
  if (edit_find_dialog)
    return(XEN_CONS(XEN_WRAP_WIDGET(edit_find_dialog),
	     XEN_CONS(XEN_WRAP_WIDGET(edit_find_text),
  	       XEN_CONS(XEN_WRAP_WIDGET(next_button),
		 XEN_CONS(XEN_WRAP_WIDGET(previous_button),
		   XEN_CONS(XEN_WRAP_WIDGET(cancelB),
		     XEN_EMPTY_LIST))))));
  return(XEN_EMPTY_LIST);
}


XEN_ARGIFY_2(g_find_dialog_w, g_find_dialog)
XEN_NARGIFY_0(g_find_dialog_widgets_w, g_find_dialog_widgets)

void g_init_gxfind(void)
{
  XEN_DEFINE_PROCEDURE(S_find_dialog, g_find_dialog_w, 0, 2, 0, H_find_dialog);
  XEN_DEFINE_PROCEDURE("find-dialog-widgets", g_find_dialog_widgets_w, 0, 0, 0, "internal auto-test function");
}

