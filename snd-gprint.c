#include "snd.h"

static GtkWidget *print_dialog = NULL;
static GtkWidget *print_name = NULL;
static GtkWidget *print_eps_or_lpr = NULL;
static GtkWidget *print_ok_button, *print_message;
static GtkWidget *print_error_text = NULL;
static char print_string[PRINT_BUFFER_SIZE];

static void print_help_callback(GtkWidget *w, gpointer context)
{
  print_dialog_help();
}

static void print_cancel_callback(GtkWidget *w, gpointer context)
{
  ss->print_choice = PRINT_SND;
  gtk_widget_hide(print_dialog);
}

static gint print_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(print_dialog);
  return(true);
}

static bool print_watching = false, print_error = false;
static gulong print_text_id = 0, print_button_id = 0;

static void clear_error(void)
{
  gtk_widget_hide(print_error_text);
  print_error = false;
  if (print_watching)
    {
      print_watching = false;
      g_signal_handler_disconnect(print_name, print_text_id);
      g_signal_handler_disconnect(print_eps_or_lpr, print_button_id);
      print_text_id = 0;
      print_button_id = 0;
    }
}

static void watch_print(GtkWidget *w, gpointer context)
{
  clear_error();
}

static void report_in_error_info(const char *msg, void *ignore)
{
  gtk_entry_set_text(GTK_ENTRY(print_error_text), msg);
  gtk_entry_set_width_chars(GTK_ENTRY(print_error_text), 1 + snd_strlen(msg));
  print_error = true;
  if (!(GTK_WIDGET_VISIBLE(print_error_text)))
    {
      gtk_widget_show(print_error_text);
      print_watching = true;
      print_text_id = SG_SIGNAL_CONNECT(print_name, "changed", watch_print, NULL);
      print_button_id = SG_SIGNAL_CONNECT(print_eps_or_lpr, "toggled", watch_print, NULL);
    }
}

static int lpr(char *name)
{
  /* make some desultory effort to print the file */
  mus_snprintf(print_string, PRINT_BUFFER_SIZE, "lpr %s", name);
  return(system(print_string));
}

static printing_t printing = NOT_PRINTING;

static void print_ok_callback(GtkWidget *w, gpointer context)
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
	  set_stock_button_label(print_ok_button, _("Stop"));
	  nsp = any_selected_sound();
	  mus_snprintf(print_string, PRINT_BUFFER_SIZE, _("printing %s"), nsp->short_filename);
	  set_label(print_message, print_string);
	}
      printing = PRINTING;
      print_it = (bool)(GTK_TOGGLE_BUTTON(print_eps_or_lpr)->active);
      quit = (ss->print_choice == PRINT_ENV);
      if (print_it)
	{
	  char *name;
	  int err = 0;
	  name = snd_tempnam();
	  redirect_snd_error_to(report_in_error_info, NULL);
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
	  redirect_snd_error_to(NULL, NULL);
	  if (!print_error)
	    {
	      err = lpr(name); /* lpr apparently insists on printing to stderr? */
	      if (err != 0)
		report_in_error_info(_("can't print!"), NULL);
	      snd_remove(name, IGNORE_CACHE);
	    }
	  FREE(name);
	}
      else 
	{
	  char *str;
	  str = (char *)gtk_entry_get_text(GTK_ENTRY(print_name));
	  redirect_snd_error_to(report_in_error_info, NULL);
	  switch (ss->print_choice)
	    {
	    case PRINT_SND: 
	      if (snd_print(str))
		report_in_minibuffer(nsp, "printed current view to %s", str);
	      break;
	    case PRINT_ENV: 
	      enved_print(str);
	      break;
	    }
	  redirect_snd_error_to(NULL, NULL);
	}
    }
  printing = NOT_PRINTING;
  if (ss->print_choice == PRINT_SND)
    {
      set_stock_button_label(print_ok_button, _("Print"));
      mus_snprintf(print_string, PRINT_BUFFER_SIZE, _("print %s"), nsp->short_filename);
      set_label(print_message, print_string);
    }
  ss->print_choice = PRINT_SND;
  if (quit) gtk_widget_hide(print_dialog);
}

static void start_print_dialog(void)
{
  if (!print_dialog)
    {
      GtkWidget *help_button, *dismiss_button, *epsbox, *epslabel;

      print_dialog = snd_gtk_dialog_new();
      SG_SIGNAL_CONNECT(print_dialog, "delete_event", print_delete_callback, NULL);
      gtk_window_set_title(GTK_WINDOW(print_dialog), _("Print"));
      sg_make_resizable(print_dialog);
      gtk_container_set_border_width (GTK_CONTAINER(print_dialog), 10);
      gtk_window_resize(GTK_WINDOW(print_dialog), 220, 160);
      gtk_widget_realize(print_dialog);

      help_button = gtk_button_new_from_stock(GTK_STOCK_HELP);
      gtk_widget_set_name(help_button, "help_button");

      dismiss_button = gtk_button_new_from_stock(GTK_STOCK_QUIT);
      gtk_widget_set_name(dismiss_button, "quit_button");

      print_ok_button = sg_button_new_from_stock_with_label(_("Print"), GTK_STOCK_PRINT);
      gtk_widget_set_name(print_ok_button, "doit_button");

      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(print_dialog)->action_area), print_ok_button, true, true, 4);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(print_dialog)->action_area), dismiss_button, true, true, 4);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(print_dialog)->action_area), help_button, true, true, 4);
      SG_SIGNAL_CONNECT(dismiss_button, "clicked", print_cancel_callback, NULL);
      SG_SIGNAL_CONNECT(help_button, "clicked", print_help_callback, NULL);
      SG_SIGNAL_CONNECT(print_ok_button, "clicked", print_ok_callback, NULL);
      gtk_widget_show(dismiss_button);
      gtk_widget_show(print_ok_button);
      gtk_widget_show(help_button);

      print_message = gtk_label_new("");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(print_dialog)->vbox), print_message, false, false, 6);
      gtk_widget_show(print_message);

      epsbox = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(print_dialog)->vbox), epsbox, false, false, 6);
      gtk_widget_show(epsbox);
      
      epslabel = gtk_label_new(_("eps file:"));
      gtk_box_pack_start(GTK_BOX(epsbox), epslabel, false, false, 2);
      gtk_widget_show(epslabel);
      
      print_name = snd_entry_new(epsbox, WITH_WHITE_BACKGROUND);
      gtk_entry_set_text(GTK_ENTRY(print_name), eps_file(ss));

      print_eps_or_lpr = gtk_check_button_new_with_label(_("direct to printer"));
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(print_dialog)->vbox), print_eps_or_lpr, false, false, 6);
      gtk_widget_show(print_eps_or_lpr);
      set_dialog_widget(PRINT_DIALOG, print_dialog);

      print_error_text = snd_gtk_entry_label_new(NULL, ss->sgx->highlight_color);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(print_dialog)->vbox), print_error_text, false, false, 0);
      gtk_widget_hide(print_error_text);
    }
}

void file_print_callback(GtkWidget *w, gpointer context)
{
  start_print_dialog();
  if (ss->print_choice == PRINT_SND)
    {
      snd_info *nsp;
      nsp = any_selected_sound();
      mus_snprintf(print_string, PRINT_BUFFER_SIZE, _("print %s"), nsp->short_filename);
    }
  else mus_snprintf(print_string, PRINT_BUFFER_SIZE, "%s", _("print env"));
  gtk_label_set_text(GTK_LABEL(print_message), print_string);
  gtk_widget_show(print_dialog);
}

widget_t make_file_print_dialog(bool managed, bool direct_to_printer)
{
  start_print_dialog();
  set_toggle_button(print_eps_or_lpr, direct_to_printer, false, NULL);
  if (managed) gtk_widget_show(print_dialog);
  return(print_dialog);
}

void save_print_dialog_state(FILE *fd)
{
  if ((print_dialog) && (GTK_WIDGET_VISIBLE(print_dialog)))
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s #t %s)\n", S_print_dialog, ((bool)(GTK_TOGGLE_BUTTON(print_eps_or_lpr)->active)) ? "#t" : "#f");
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(true, %s)\n", TO_PROC_NAME(S_print_dialog), ((bool)(GTK_TOGGLE_BUTTON(print_eps_or_lpr)->active)) ? "true" : "false");
#endif
#if HAVE_FORTH
      fprintf(fd, "#t %s %s drop\n", ((bool)(GTK_TOGGLE_BUTTON(print_eps_or_lpr)->active)) ? "#t" : "#f", S_print_dialog);
#endif
    }
}
