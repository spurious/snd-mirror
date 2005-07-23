#include "snd.h"

/* X side of file print */

/* TODO error label and redirection */

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
  bool quit = false;
  snd_info *nsp = NULL;
  if (printing) 
    ss->stopped_explicitly = true;
  else
    {
      bool print_it;
      if (ss->print_choice == PRINT_SND)
	{
	  set_button_label(file_print_ok_button, _("Stop"));
	  nsp = any_selected_sound();
	  mus_snprintf(print_string, PRINT_BUFFER_SIZE, _("printing %s"), nsp->short_filename);
	  set_label(file_print_message, print_string);
	}
      printing = PRINTING;
      print_it = (bool)(GTK_TOGGLE_BUTTON(file_print_eps_or_lpr)->active);
      quit = (ss->print_choice == PRINT_ENV);
      if (print_it)
	{
	  char *name;
	  int err = 0;
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
	  /* TODO: redirect to print dialog */
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
  if (!file_print_dialog)
    {
      GtkWidget *help_button, *dismiss_button, *epsbox, *epslabel;

      file_print_dialog = snd_gtk_dialog_new();
      SG_SIGNAL_CONNECT(file_print_dialog, "delete_event", file_print_delete_callback, NULL);
      gtk_window_set_title(GTK_WINDOW(file_print_dialog), _("Print"));
      sg_make_resizable(file_print_dialog);
      gtk_container_set_border_width (GTK_CONTAINER(file_print_dialog), 10);
      gtk_window_resize(GTK_WINDOW(file_print_dialog), 220, 160);
      gtk_widget_realize(file_print_dialog);

      help_button = gtk_button_new_from_stock(GTK_STOCK_HELP);
      gtk_widget_set_name(help_button, "help_button");

      dismiss_button = gtk_button_new_from_stock(GTK_STOCK_QUIT);
      gtk_widget_set_name(dismiss_button, "quit_button");

      file_print_ok_button = gtk_button_new_with_label(_("Print"));
      gtk_widget_set_name(file_print_ok_button, "doit_button");

      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(file_print_dialog)->action_area), file_print_ok_button, true, true, 4);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(file_print_dialog)->action_area), dismiss_button, true, true, 4);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(file_print_dialog)->action_area), help_button, true, true, 4);
      SG_SIGNAL_CONNECT(dismiss_button, "clicked", file_print_cancel_callback, NULL);
      SG_SIGNAL_CONNECT(help_button, "clicked", file_print_help_callback, NULL);
      SG_SIGNAL_CONNECT(file_print_ok_button, "clicked", file_print_ok_callback, NULL);
      gtk_widget_show(dismiss_button);
      gtk_widget_show(file_print_ok_button);
      gtk_widget_show(help_button);

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
  start_file_print_dialog();
  if (ss->print_choice == PRINT_SND)
    {
      snd_info *nsp;
      nsp = any_selected_sound();
      mus_snprintf(print_string, PRINT_BUFFER_SIZE, _("print %s"), nsp->short_filename);
    }
  else mus_snprintf(print_string, PRINT_BUFFER_SIZE, "%s", _("print env"));
  gtk_label_set_text(GTK_LABEL(file_print_message), print_string);
  gtk_widget_show(file_print_dialog);
}

widget_t make_file_print_dialog(bool managed, bool direct_to_printer)
{
  start_file_print_dialog();
  set_toggle_button(file_print_eps_or_lpr, direct_to_printer, false, NULL);
  if (managed) gtk_widget_show(file_print_dialog);
  return(file_print_dialog);
}

void save_print_dialog_state(FILE *fd)
{
  if ((file_print_dialog) && (GTK_WIDGET_VISIBLE(file_print_dialog)))
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s #t %s)\n", S_print_dialog, ((bool)(GTK_TOGGLE_BUTTON(file_print_eps_or_lpr)->active)) ? "#t" : "#f");
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(true, %s)\n", TO_PROC_NAME(S_print_dialog), ((bool)(GTK_TOGGLE_BUTTON(file_print_eps_or_lpr)->active)) ? "true" : "false");
#endif
    }
}
