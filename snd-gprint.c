#include "snd.h"

/* X side of file print */

static GtkWidget *file_print_dialog = NULL;
static GtkWidget *file_print_name = NULL;
static GtkWidget *file_print_eps_or_lpr = NULL;
static GtkWidget *file_print_ok_button, *file_print_message;
static char print_string[PRINT_BUFFER_SIZE];

static void file_print_help_callback(GtkWidget *w, gpointer context)
{
  print_dialog_help();
}

static void file_print_cancel_callback(GtkWidget *w, gpointer context)
{
  ss->print_choice = PRINT_SND;
  gtk_widget_hide(file_print_dialog);
}

static gint file_print_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(file_print_dialog);
  return(true);
}

static int lpr (char *name)
{
  /* make some desultory effort to print the file */
  mus_snprintf(print_string, PRINT_BUFFER_SIZE, "lpr %s", name);
  return(system(print_string));
}

static printing_t printing = NOT_PRINTING;

static void file_print_ok_callback(GtkWidget *w, gpointer context)
{
  bool print_it, quit = false;
  int err = 0;
  char *name;
  snd_info *nsp = NULL;
  if (printing) 
    ss->stopped_explicitly = true;
  else
    {
      if (ss->print_choice == PRINT_SND)
	{
	  set_button_label(file_print_ok_button, _("Stop"));
	  nsp = any_selected_sound();
	  mus_snprintf(print_string, PRINT_BUFFER_SIZE, _("printing %s"), nsp->short_filename);
	  set_label(file_print_message, print_string);
	}
      printing = PRINTING;
      print_it = (bool)GTK_TOGGLE_BUTTON(file_print_eps_or_lpr)->active;
      quit = (ss->print_choice == PRINT_ENV);
      if (print_it)
	{
	  name = snd_tempnam();
	  switch (ss->print_choice)
	    {
	    case PRINT_SND: 
	      snd_print(name); 
	      break;
	    case PRINT_ENV: 
	      enved_print(name); 
	      break;
	    }
	  err = lpr(name);
	  if ((err != 0) && (nsp)) report_in_minibuffer(nsp, _("can't print!"));
	  /* tried to redirect stderr here and pick it up afterwards, to no avail */
	  snd_remove(name, IGNORE_CACHE);
	  FREE(name);
	}
      else 
	{
	  switch (ss->print_choice)
	    {
	    case PRINT_SND: 
	      snd_print((char *)gtk_entry_get_text(GTK_ENTRY(file_print_name))); 
	      break;
	    case PRINT_ENV: 
	      enved_print((char *)gtk_entry_get_text(GTK_ENTRY(file_print_name))); 
	      break;
	    }
	}
    }
  printing = NOT_PRINTING;
  if (ss->print_choice == PRINT_SND)
    {
      set_button_label(file_print_ok_button, _("Print"));
      mus_snprintf(print_string, PRINT_BUFFER_SIZE, _("print %s"), nsp->short_filename);
      set_label(file_print_message, print_string);
    }
  ss->print_choice = PRINT_SND;
  if (quit) gtk_widget_hide(file_print_dialog);
}

static void start_file_print_dialog(void)
{
  GtkWidget *print_button, *help_button, *dismiss_button, *epsbox, *epslabel;
  if (!file_print_dialog)
    {
      file_print_dialog = snd_gtk_dialog_new();
      SG_SIGNAL_CONNECT(file_print_dialog, "delete_event", file_print_delete_callback, NULL);
      gtk_window_set_title(GTK_WINDOW(file_print_dialog), _("Print"));
      sg_make_resizable(file_print_dialog);
      gtk_container_set_border_width (GTK_CONTAINER(file_print_dialog), 10);
      gtk_window_resize(GTK_WINDOW(file_print_dialog), 220, 160);
      gtk_widget_realize(file_print_dialog);

      help_button = gtk_button_new_with_label(_("Help"));
      gtk_widget_set_name(help_button, "help_button");
      dismiss_button = gtk_button_new_with_label(_("Dismiss"));
      gtk_widget_set_name(dismiss_button, "quit_button");
      print_button = gtk_button_new_with_label(_("Print"));
      gtk_widget_set_name(print_button, "doit_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(file_print_dialog)->action_area), print_button, true, true, 4);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(file_print_dialog)->action_area), dismiss_button, true, true, 4);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(file_print_dialog)->action_area), help_button, true, true, 4);
      SG_SIGNAL_CONNECT(dismiss_button, "clicked", file_print_cancel_callback, NULL);
      SG_SIGNAL_CONNECT(help_button, "clicked", file_print_help_callback, NULL);
      SG_SIGNAL_CONNECT(print_button, "clicked", file_print_ok_callback, NULL);
      gtk_widget_show(dismiss_button);
      gtk_widget_show(print_button);
      gtk_widget_show(help_button);
      file_print_ok_button = print_button;

      file_print_message = gtk_label_new("");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(file_print_dialog)->vbox), file_print_message, false, false, 6);
      gtk_widget_show(file_print_message);

      epsbox = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(file_print_dialog)->vbox), epsbox, false, false, 6);
      gtk_widget_show(epsbox);
      
      epslabel = gtk_label_new(_("eps file:"));
      gtk_box_pack_start(GTK_BOX(epsbox), epslabel, false, false, 2);
      gtk_widget_show(epslabel);
      
      file_print_name = snd_entry_new(epsbox, true);
      gtk_entry_set_text(GTK_ENTRY(file_print_name), eps_file(ss));

      file_print_eps_or_lpr = gtk_check_button_new_with_label(_("direct to printer"));
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(file_print_dialog)->vbox), file_print_eps_or_lpr, false, false, 6);
      gtk_widget_show(file_print_eps_or_lpr);
      set_dialog_widget(PRINT_DIALOG, file_print_dialog);
    }
}

void file_print_callback(GtkWidget *w, gpointer context)
{
  snd_info *nsp;
  start_file_print_dialog();
  if (ss->print_choice == PRINT_SND)
    {
      nsp = any_selected_sound();
      mus_snprintf(print_string, PRINT_BUFFER_SIZE, _("print %s"), nsp->short_filename);
    }
  else mus_snprintf(print_string, PRINT_BUFFER_SIZE, "%s", _("print env"));
  gtk_label_set_text(GTK_LABEL(file_print_message), print_string);
  gtk_widget_show(file_print_dialog);
}

widget_t make_file_print_dialog(bool managed)
{
  start_file_print_dialog();
  if (managed) gtk_widget_show(file_print_dialog);
  return(file_print_dialog);
}
