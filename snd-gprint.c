#include "snd.h"

/* X side of file print */

static GtkWidget *file_print_dialog = NULL;
static GtkWidget *file_print_name = NULL;
static GtkWidget *file_print_eps_or_lpr = NULL;
static GtkWidget *file_print_ok_button, *file_print_message;
static char print_string[PRINT_BUFFER_SIZE];

static void file_print_help_callback(GtkWidget *w, gpointer context)
{
  print_dialog_help((snd_state *)context);
}

static void file_print_cancel_callback(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  ss->print_choice = PRINT_SND;
  gtk_widget_hide(file_print_dialog);
}

static void file_print_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(file_print_dialog);
}

static int lpr (char *name)
{
  /* make some desultory effort to print the file */
  mus_snprintf(print_string, PRINT_BUFFER_SIZE, "lpr %s", name);
  return(system(print_string));
}

static int printing = 0;

static void file_print_ok_callback(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  int print_it, quit = 0, err = 0;
  char *name;
  snd_info *nsp = NULL;
  if (printing) 
    ss->stopped_explicitly = 1;
  else
    {
      if (ss->print_choice == PRINT_SND)
	{
	  set_button_label(file_print_ok_button, "Stop");
	  nsp = any_selected_sound(ss);
	  mus_snprintf(print_string, PRINT_BUFFER_SIZE, "printing %s", nsp->short_filename);
	  set_label(file_print_message, print_string);
	}
      printing = 1;
      print_it = GTK_TOGGLE_BUTTON(file_print_eps_or_lpr)->active;
      quit = (ss->print_choice == PRINT_ENV);
      if (print_it)
	{
	  name = snd_tempnam(ss);
	  switch (ss->print_choice)
	    {
	    case PRINT_SND: 
	      snd_print(ss, name); 
	      break;
	    case PRINT_ENV: 
	      enved_print(name); 
	      break;
	    }
	  err = lpr(name);
	  if ((err != 0) && (nsp)) report_in_minibuffer(nsp, "can't print!");
	  /* tried to redirect stderr here and pick it up afterwards, to no avail */
	  snd_remove(name, FALSE);
	  FREE(name);
	}
      else 
	{
	  switch (ss->print_choice)
	    {
	    case PRINT_SND: 
	      snd_print(ss, (char *)gtk_entry_get_text(GTK_ENTRY(file_print_name))); 
	      break;
	    case PRINT_ENV: 
	      enved_print((char *)gtk_entry_get_text(GTK_ENTRY(file_print_name))); 
	      break;
	    }
	}
    }
  printing = 0;
  if (ss->print_choice == PRINT_SND)
    {
      set_button_label(file_print_ok_button, "Print");
      mus_snprintf(print_string, PRINT_BUFFER_SIZE, "print %s", nsp->short_filename);
      set_label(file_print_message, print_string);
    }
  ss->print_choice = PRINT_SND;
  if (quit) gtk_widget_hide(file_print_dialog);
}

void file_print_callback(GtkWidget *w, gpointer context)
{
  GtkWidget *print_button, *help_button, *dismiss_button, *epsbox, *epslabel;
  snd_info *nsp;
  snd_state *ss = (snd_state *)context;
  if (!file_print_dialog)
    {
      file_print_dialog = gtk_dialog_new();
      g_signal_connect_closure_by_id(GTK_OBJECT(file_print_dialog),
				     g_signal_lookup("delete_event", G_OBJECT_TYPE(GTK_OBJECT(file_print_dialog))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(file_print_delete_callback), (gpointer)ss, 0),
				     0);
      gtk_window_set_title(GTK_WINDOW(file_print_dialog), "Print");
      sg_make_resizable(file_print_dialog);
      set_background(file_print_dialog, (ss->sgx)->basic_color);
      gtk_container_set_border_width (GTK_CONTAINER(file_print_dialog), 10);
      gtk_window_resize(GTK_WINDOW(file_print_dialog), 220, 160);
      gtk_widget_realize(file_print_dialog);

      help_button = gtk_button_new_with_label("Help");
      dismiss_button = gtk_button_new_with_label("Dismiss");
      print_button = gtk_button_new_with_label("Print");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(file_print_dialog)->action_area), print_button, TRUE, TRUE, 4);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(file_print_dialog)->action_area), dismiss_button, TRUE, TRUE, 4);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(file_print_dialog)->action_area), help_button, TRUE, TRUE, 4);
      g_signal_connect_closure_by_id(GTK_OBJECT(dismiss_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(dismiss_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(file_print_cancel_callback), (gpointer)ss, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(help_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(help_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(file_print_help_callback), (gpointer)ss, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(print_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(print_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(file_print_ok_callback), (gpointer)ss, 0),
				     0);
      set_pushed_button_colors(help_button, ss);
      set_pushed_button_colors(dismiss_button, ss);
      set_pushed_button_colors(print_button, ss);
      gtk_widget_show(dismiss_button);
      gtk_widget_show(print_button);
      gtk_widget_show(help_button);

      file_print_ok_button = print_button;

      file_print_message = gtk_label_new("");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(file_print_dialog)->vbox), file_print_message, FALSE, FALSE, 6);
      gtk_widget_show(file_print_message);

      epsbox = gtk_hbox_new(FALSE, 0);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(file_print_dialog)->vbox), epsbox, FALSE, FALSE, 6);
      gtk_widget_show(epsbox);
      
      epslabel = gtk_label_new("eps file:");
      gtk_box_pack_start(GTK_BOX(epsbox), epslabel, FALSE, FALSE, 2);
      gtk_widget_show(epslabel);
      
      file_print_name = snd_entry_new(ss, epsbox, TRUE);
      gtk_entry_set_text(GTK_ENTRY(file_print_name), eps_file(ss));

      file_print_eps_or_lpr = gtk_check_button_new_with_label("direct to printer");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(file_print_dialog)->vbox), file_print_eps_or_lpr, FALSE, FALSE, 6);
      gtk_widget_show(file_print_eps_or_lpr);
      set_dialog_widget(ss, PRINT_DIALOG, file_print_dialog);
    }
  if (ss->print_choice == PRINT_SND)
    {
      nsp = any_selected_sound(ss);
      mus_snprintf(print_string, PRINT_BUFFER_SIZE, "print %s", nsp->short_filename);
    }
  else mus_snprintf(print_string, PRINT_BUFFER_SIZE, "%s", "print env");
  gtk_label_set_text(GTK_LABEL(file_print_message), print_string);
  gtk_widget_show(file_print_dialog);
}

