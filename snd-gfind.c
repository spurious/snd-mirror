#include "snd.h"

/* -------- edit find -------- */

static GtkWidget *edit_find_dialog, *edit_find_text, *cancelB, *edit_find_label, *next_button, *previous_button;

static void edit_find_dismiss(GtkWidget *w, gpointer context) 
{ /* "Done" */
  if (ss->checking_explicitly)
    ss->stopped_explicitly = true;
  else gtk_widget_hide(edit_find_dialog);
} 

static void edit_find_delete(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(edit_find_dialog);
}

static void edit_find_help(GtkWidget *w, gpointer context) 
{
  snd_help("Global Find",
"This search travels through all the current channels in parallel until a match is found.  The find \
expression is a Scheme function of one argument,  the current sample value.  It should return #t when the \
search is satisified.  For example, (lambda (n) (> n .1) looks for the next sample that is greater than .1.",
	   true);
} 

static void edit_find_find(read_direction_t direction, GtkWidget *w, gpointer context) 
{ /* "Find" is the label here */
  char *str, *buf = NULL;
  XEN proc;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(edit_find_text));
  if ((str) && (*str))
    {
      ss->search_expr = copy_string(str);
      if (XEN_PROCEDURE_P(ss->search_proc)) snd_unprotect(ss->search_proc);
      ss->search_proc = XEN_UNDEFINED;
      if (ss->search_tree)
	ss->search_tree = free_ptree(ss->search_tree);
      proc = snd_catch_any(eval_str_wrapper, str, str);
      if ((XEN_PROCEDURE_P(proc)) &&
	  (XEN_TO_C_INT(XEN_CAR(XEN_ARITY(proc))) == 1))
	{
	  ss->search_proc = proc;
	  snd_protect(proc);
	  if (optimization(ss) > 0)
	    ss->search_tree = form_to_ptree_1_b_without_env(C_STRING_TO_XEN_FORM(str));
	  buf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
	  mus_snprintf(buf, PRINT_BUFFER_SIZE, _("find: %s"), str);
	  set_label(edit_find_label, buf);
	  gtk_entry_set_text(GTK_ENTRY(edit_find_text), "");
	  FREE(buf);
	}
    }
  else
    {
      if (ss->search_expr == NULL)
	{
	  /* using global search_proc set by user */
	  buf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
	  mus_snprintf(buf, PRINT_BUFFER_SIZE, _("find: %s"), XEN_AS_STRING(ss->search_proc));
	  set_label(edit_find_label, buf);
	  gtk_entry_set_text(GTK_ENTRY(edit_find_text), "");
	  FREE(buf);
	}
    }
  if ((XEN_PROCEDURE_P(ss->search_proc)) || (ss->search_tree))
    {
      set_button_label(cancelB, _("Stop"));
      str = global_search(direction);
      set_button_label(cancelB, _("Dismiss"));
      if ((str) && (*str)) set_label(edit_find_label, str);
    }
} 

static void edit_find_next(GtkWidget *w, gpointer context) {edit_find_find(READ_FORWARD, w, context);}
static void edit_find_previous(GtkWidget *w, gpointer context) {edit_find_find(READ_BACKWARD, w, context);}

void edit_find_callback(GtkWidget *w, gpointer context)
{
  GtkWidget *dl, *rc;
  GtkWidget *help_button;
  if (!edit_find_dialog)
    {
      edit_find_dialog = gtk_dialog_new();
      g_signal_connect_closure_by_id(GTK_OBJECT(edit_find_dialog),
				     g_signal_lookup("delete_event", G_OBJECT_TYPE(GTK_OBJECT(edit_find_dialog))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(edit_find_delete), NULL, 0),
				     0);
      gtk_window_set_title(GTK_WINDOW(edit_find_dialog), _("Find"));
      sg_make_resizable(edit_find_dialog);
      set_background(edit_find_dialog, (ss->sgx)->basic_color);
      gtk_container_set_border_width (GTK_CONTAINER(edit_find_dialog), 10);
      gtk_window_resize(GTK_WINDOW(edit_find_dialog), 350, 120);
      gtk_widget_realize(edit_find_dialog);

      help_button = gtk_button_new_with_label(_("Help"));
      cancelB = gtk_button_new_with_label(_("Dismiss"));
      next_button = gtk_button_new_with_label(_("Next"));
      previous_button = gtk_button_new_with_label(_("Previous"));
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(edit_find_dialog)->action_area), cancelB, true, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(edit_find_dialog)->action_area), next_button, true, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(edit_find_dialog)->action_area), previous_button, true, true, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(edit_find_dialog)->action_area), help_button, true, true, 10);
      g_signal_connect_closure_by_id(GTK_OBJECT(cancelB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(cancelB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(edit_find_dismiss), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(help_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(help_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(edit_find_help), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(next_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(next_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(edit_find_next), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(previous_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(previous_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(edit_find_previous), NULL, 0),
				     0);
      gtk_widget_show(cancelB);
      gtk_widget_show(next_button);
      gtk_widget_show(previous_button);
      gtk_widget_show(help_button);
      
      rc = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(edit_find_dialog)->vbox), rc, true, true, 4);
      gtk_widget_show(rc);

      dl = gtk_label_new(_("find:"));
      gtk_box_pack_start(GTK_BOX(rc), dl, false, false, 4);
      gtk_widget_show(dl);

      edit_find_text = snd_entry_new(rc, true);
      g_signal_connect_closure_by_id(GTK_OBJECT(edit_find_text),
				     g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(edit_find_text))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(edit_find_next), NULL, 0),
				     0);
      
      edit_find_label = gtk_label_new(_("global search"));
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(edit_find_dialog)->vbox), edit_find_label, false, false, 4);
      gtk_widget_show(edit_find_label);
      gtk_widget_show(edit_find_dialog);
      set_dialog_widget(FIND_DIALOG, edit_find_dialog);
    }
  else raise_dialog(edit_find_dialog);
}

#if DEBUGGING
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

static XEN g_edit_find_dialog(void)
{
  edit_find_callback(NULL, (gpointer)(ss));
  return(XEN_FALSE);
}


#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_edit_find_dialog_w, g_edit_find_dialog)
XEN_NARGIFY_0(g_find_dialog_widgets_w, g_find_dialog_widgets)
#else
#define g_edit_find_dialog_w g_edit_find_dialog
#define g_find_dialog_widgets_w g_find_dialog_widgets
#endif

void g_init_gxfind(void)
{
  XEN_DEFINE_PROCEDURE("edit-find-dialog", g_edit_find_dialog_w, 0, 0, 0, "");
  XEN_DEFINE_PROCEDURE("find-dialog-widgets", g_find_dialog_widgets_w, 0, 0, 0, "");
}
#endif
