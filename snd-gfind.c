#include "snd.h"

/* -------- edit find -------- */

static GtkWidget *edit_find_dialog, *edit_find_text, *cancelB, *edit_find_label;

static void edit_find_dismiss(GtkWidget *w, gpointer context) 
{ /* "Done" */
  snd_state *ss = (snd_state *)context;
  if (ss->checking_explicitly)
    ss->stopped_explicitly = 1;
  else gtk_widget_hide(edit_find_dialog);
} 

static void edit_find_delete(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(edit_find_dialog);
}

static void edit_find_help(GtkWidget *w, gpointer context) 
{
  snd_help((snd_state *)context,
	   "Global Find",
"This search travels through all the current channels\n\
in parallel until a match is found.  The find\n\
expression is a Scheme function of one argument, \n\
the current sample value.  It should return #t when the\n\
search is satisified.  For example, (lambda (n) (> n .1)\n\
looks for the next sample that is greater than .1.\n\
");
} 

static void edit_find_find(int direction, GtkWidget *w, gpointer context) 
{ /* "Find" is the label here */
  char *str, *buf = NULL;
  snd_state *ss = (snd_state *)context;
  SCM proc;
  str = gtk_entry_get_text(GTK_ENTRY(edit_find_text));
  if ((str) && (*str))
    {
      ss->search_expr = str;
      if (PROCEDURE_P(ss->search_proc)) snd_unprotect(ss->search_proc);
      ss->search_proc = SCM_UNDEFINED;
      proc = snd_catch_any(eval_str_wrapper, str, str);
      if (procedure_ok_with_error(proc, 1, 0, "find", "find procedure", 1))
	{
	  ss->search_proc = proc;
	  snd_protect(proc);
	}
      buf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
      mus_snprintf(buf, PRINT_BUFFER_SIZE, "find: %s", str);
      set_label(edit_find_label, buf);
      gtk_entry_set_text(GTK_ENTRY(edit_find_text), "");
      FREE(buf);
    }
  if (PROCEDURE_P(ss->search_proc))
    {
      set_button_label(cancelB, STR_Stop);
      str = global_search(ss, direction);
      set_button_label(cancelB, STR_Dismiss);
      if ((str) && (*str)) set_label(edit_find_label, str);
    }
} 

static void edit_find_next(GtkWidget *w, gpointer context) {edit_find_find(READ_FORWARD, w, context);}
static void edit_find_previous(GtkWidget *w, gpointer context) {edit_find_find(READ_BACKWARD, w, context);}

void Edit_Find_Callback(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  GtkWidget *dl, *rc;
  GtkWidget *help_button, *next_button, *previous_button;
  if (!edit_find_dialog)
    {
      edit_find_dialog = gtk_dialog_new();
      set_dialog_widget(FIND_DIALOG, edit_find_dialog);
      gtk_signal_connect(GTK_OBJECT(edit_find_dialog), "delete_event", GTK_SIGNAL_FUNC(edit_find_delete), (gpointer)ss);
      gtk_window_set_title(GTK_WINDOW(edit_find_dialog), STR_Find);
      gtk_window_set_policy(GTK_WINDOW(edit_find_dialog), TRUE, TRUE, FALSE); /* allow shrink or grow */
      set_background(edit_find_dialog, (ss->sgx)->basic_color);
      gtk_container_set_border_width (GTK_CONTAINER(edit_find_dialog), 10);
      gtk_widget_set_usize(GTK_WIDGET(edit_find_dialog), 350, 120);
      gtk_widget_realize(edit_find_dialog);
      add_dialog(ss, edit_find_dialog);

      help_button = gtk_button_new_with_label(STR_Help);
      cancelB = gtk_button_new_with_label(STR_Dismiss);
      next_button = gtk_button_new_with_label(STR_Next);
      previous_button = gtk_button_new_with_label(STR_Previous);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(edit_find_dialog)->action_area), cancelB, TRUE, TRUE, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(edit_find_dialog)->action_area), next_button, TRUE, TRUE, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(edit_find_dialog)->action_area), previous_button, TRUE, TRUE, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(edit_find_dialog)->action_area), help_button, TRUE, TRUE, 10);
      gtk_signal_connect(GTK_OBJECT(cancelB), "clicked", GTK_SIGNAL_FUNC(edit_find_dismiss), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(help_button), "clicked", GTK_SIGNAL_FUNC(edit_find_help), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(next_button), "clicked", GTK_SIGNAL_FUNC(edit_find_next), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(previous_button), "clicked", GTK_SIGNAL_FUNC(edit_find_previous), (gpointer)ss);
      set_pushed_button_colors(help_button, ss);
      set_pushed_button_colors(cancelB, ss);
      set_pushed_button_colors(next_button, ss);
      set_pushed_button_colors(previous_button, ss);
      gtk_widget_show(cancelB);
      gtk_widget_show(next_button);
      gtk_widget_show(previous_button);
      gtk_widget_show(help_button);
      
      rc = gtk_hbox_new(FALSE, 0);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(edit_find_dialog)->vbox), rc, TRUE, TRUE, 4);
      gtk_widget_show(rc);

      dl = gtk_label_new("find:");
      gtk_box_pack_start(GTK_BOX(rc), dl, FALSE, FALSE, 4);
      gtk_widget_show(dl);

      edit_find_text = snd_entry_new(ss, rc, TRUE);
      gtk_signal_connect(GTK_OBJECT(edit_find_text), "activate", GTK_SIGNAL_FUNC(edit_find_next), (gpointer)ss);
      
      edit_find_label = gtk_label_new(STR_global_search);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(edit_find_dialog)->vbox), edit_find_label, FALSE, FALSE, 4);
      gtk_widget_show(edit_find_label);
      gtk_widget_show(edit_find_dialog);
    }
  else raise_dialog(edit_find_dialog);
}

