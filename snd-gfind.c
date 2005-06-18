#include "snd.h"

/* -------- edit find -------- */

static GtkWidget *edit_find_dialog, *edit_find_text, *cancelB, *edit_find_label, *next_button, *previous_button;

static void edit_find_dismiss(GtkWidget *w, gpointer context) 
{ /* "Done" */
  if (ss->checking_explicitly)
    ss->stopped_explicitly = true;
  else gtk_widget_hide(edit_find_dialog);
} 

static gint edit_find_delete(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(edit_find_dialog);
  return(true);
}

static void edit_find_help(GtkWidget *w, gpointer context) 
{
  find_dialog_help();
} 

static void edit_find_find(read_direction_t direction, GtkWidget *w, gpointer context) 
{ /* "Find" is the label here */
  char *str, *buf = NULL;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(edit_find_text));
  if ((str) && (*str))
    {
      XEN proc;
      ss->search_expr = copy_string(str);
      if (XEN_PROCEDURE_P(ss->search_proc)) 
	{
	  snd_unprotect_at(ss->search_proc_loc);
	  ss->search_proc_loc = NOT_A_GC_LOC;
	}
      ss->search_proc = XEN_UNDEFINED;
      if (ss->search_tree)
	{
	  free_ptree(ss->search_tree);
	  ss->search_tree = NULL;
	}
      proc = snd_catch_any(eval_str_wrapper, str, str);
      if ((XEN_PROCEDURE_P(proc)) && (procedure_arity_ok(proc, 1)))
	{
	  ss->search_proc = proc;
	  ss->search_proc_loc = snd_protect(proc);
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

void set_find_dialog_label(const char *str) {if (edit_find_label) set_label(edit_find_label, str);}
static void edit_find_next(GtkWidget *w, gpointer context) {edit_find_find(READ_FORWARD, w, context);}
static void edit_find_previous(GtkWidget *w, gpointer context) {edit_find_find(READ_BACKWARD, w, context);}

static void make_edit_find_dialog(bool managed)
{
  if (!edit_find_dialog)
    {
      GtkWidget *dl, *rc;
      GtkWidget *help_button;
      edit_find_dialog = snd_gtk_dialog_new();
      SG_SIGNAL_CONNECT(edit_find_dialog, "delete_event", edit_find_delete, NULL);
      gtk_window_set_title(GTK_WINDOW(edit_find_dialog), _("Find"));
      sg_make_resizable(edit_find_dialog);
      gtk_container_set_border_width (GTK_CONTAINER(edit_find_dialog), 10);
      gtk_window_resize(GTK_WINDOW(edit_find_dialog), 350, 120);
      gtk_widget_realize(edit_find_dialog);

      help_button = gtk_button_new_with_label(_("Help"));
      gtk_widget_set_name(help_button, "help_button");
      cancelB = gtk_button_new_with_label(_("Dismiss"));
      gtk_widget_set_name(cancelB, "quit_button");
      next_button = gtk_button_new_with_label(_("Next"));
      gtk_widget_set_name(next_button, "doit_button");
      previous_button = gtk_button_new_with_label(_("Previous"));
      gtk_widget_set_name(previous_button, "reset_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(edit_find_dialog)->action_area), cancelB, true, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(edit_find_dialog)->action_area), next_button, true, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(edit_find_dialog)->action_area), previous_button, true, true, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(edit_find_dialog)->action_area), help_button, true, true, 10);
      SG_SIGNAL_CONNECT(cancelB, "clicked", edit_find_dismiss, NULL);
      SG_SIGNAL_CONNECT(help_button, "clicked", edit_find_help, NULL);
      SG_SIGNAL_CONNECT(next_button, "clicked", edit_find_next, NULL);
      SG_SIGNAL_CONNECT(previous_button, "clicked", edit_find_previous, NULL);
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
      SG_SIGNAL_CONNECT(edit_find_text, "activate", edit_find_next, NULL);
      
      edit_find_label = gtk_label_new(_("global search"));
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(edit_find_dialog)->vbox), edit_find_label, false, false, 4);
      gtk_widget_show(edit_find_label);
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
  make_edit_find_dialog(true);
}

void save_find_dialog_state(FILE *fd)
{
  if ((edit_find_dialog) && (GTK_WIDGET_VISIBLE(edit_find_dialog)))
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
      if (text) g_free(text);
    }
}

static XEN g_find_dialog(XEN managed, XEN text)
{
  #define H_find_dialog "(" S_find_dialog "): create and activate the Edit:Find dialog, return the dialog widget"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ARG_1, S_find_dialog, "a boolean");
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(text), text, XEN_ARG_2, S_find_dialog, "a string");
  make_edit_find_dialog(XEN_TO_C_BOOLEAN(managed));
  if ((edit_find_text) && (XEN_STRING_P(text)))
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

#ifdef XEN_ARGIFY_1
XEN_ARGIFY_2(g_find_dialog_w, g_find_dialog)
XEN_NARGIFY_0(g_find_dialog_widgets_w, g_find_dialog_widgets)
#else
#define g_find_dialog_w g_find_dialog
#define g_find_dialog_widgets_w g_find_dialog_widgets
#endif

void g_init_gxfind(void)
{
  XEN_DEFINE_PROCEDURE(S_find_dialog, g_find_dialog_w, 0, 2, 0, H_find_dialog);
  XEN_DEFINE_PROCEDURE("find-dialog-widgets", g_find_dialog_widgets_w, 0, 0, 0, "internal auto-test function");
}

