#include "snd.h"

/* X side of file print */

static GtkWidget *file_print_dialog = NULL;
static GtkWidget *file_print_name = NULL;
static GtkWidget *file_print_eps_or_lpr = NULL;
static GtkWidget *file_print_ok_button,*file_print_message;
static char print_string[256];

static void file_print_help_callback(GtkWidget *w, gpointer clientData)
{
  print_dialog_help((snd_state *)clientData);
}

static void file_print_cancel_callback(GtkWidget *w, gpointer clientData)
{
  snd_state *ss = (snd_state *)clientData;
  ss->print_choice = PRINT_SND;
  gtk_widget_hide(file_print_dialog);
}

static void file_print_delete_callback(GtkWidget *w, GdkEvent *event, gpointer clientData)
{
  gtk_widget_hide(file_print_dialog);
}

static int lpr (char *name)
{
  /* make some desultory effort to print the file */
  sprintf(print_string, "lpr %s", name);
  return(system(print_string));
}

static int printing = 0;

static void file_print_ok_callback(GtkWidget *w, gpointer clientData)
{
  snd_state *ss = (snd_state *)clientData;
  int print_it,quit = 0,err = 0;
  char *name;
  snd_info *nsp = NULL;
  if (printing) 
    ss->stopped_explicitly = 1;
  else
    {
      if (ss->print_choice == PRINT_SND)
	{
	  set_button_label(file_print_ok_button, STR_Stop);
	  nsp = any_selected_sound(ss);
	  sprintf(print_string, "printing %s", nsp->shortname);
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
	    case PRINT_SND: snd_print(ss, name); break;
	    case PRINT_ENV: enved_print(name); break;
	    }
	  err = lpr(name);
	  remove(name);
	  if ((err != 0) && (nsp)) report_in_minibuffer(nsp, "can't print!");
	  /* tried to redirect stderr here and pick it up afterwards, to no avail */
	  free(name);
	}
      else 
	{
	  switch (ss->print_choice)
	    {
	    case PRINT_SND: snd_print(ss, gtk_entry_get_text(GTK_ENTRY(file_print_name))); break;
	    case PRINT_ENV: enved_print(gtk_entry_get_text(GTK_ENTRY(file_print_name))); break;
	    }
	}
    }
  printing = 0;
  if (ss->print_choice == PRINT_SND)
    {
      set_button_label(file_print_ok_button, STR_Print);
      sprintf(print_string, "print %s", nsp->shortname);
      set_label(file_print_message, print_string);
    }
  ss->print_choice = PRINT_SND;
  if (quit) gtk_widget_hide(file_print_dialog);
}

void File_Print_Callback(GtkWidget *w, gpointer clientData)
{
  GtkWidget *print_button,*help_button,*dismiss_button,*epsbox,*epslabel;
  snd_info *nsp;
  snd_state *ss = (snd_state *)clientData;
  if (!file_print_dialog)
    {
      file_print_dialog = gtk_dialog_new();
      gtk_signal_connect(GTK_OBJECT(file_print_dialog), "delete_event", GTK_SIGNAL_FUNC(file_print_delete_callback), (gpointer)ss);
      gtk_window_set_title(GTK_WINDOW(file_print_dialog), STR_Print);
      gtk_window_set_policy(GTK_WINDOW(file_print_dialog), TRUE, TRUE, FALSE); /* allow shrink or grow */
      set_background(file_print_dialog, (ss->sgx)->basic_color);
      gtk_container_set_border_width (GTK_CONTAINER(file_print_dialog), 10);
      gtk_widget_set_usize(GTK_WIDGET(file_print_dialog), 220, 160);
      gtk_widget_realize(file_print_dialog);
      add_dialog(ss, file_print_dialog);

      help_button = gtk_button_new_with_label(STR_Help);
      dismiss_button = gtk_button_new_with_label(STR_Dismiss);
      print_button = gtk_button_new_with_label(STR_Print);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(file_print_dialog)->action_area), print_button, TRUE, TRUE, 4);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(file_print_dialog)->action_area), dismiss_button, TRUE, TRUE, 4);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(file_print_dialog)->action_area), help_button, TRUE, TRUE, 4);
      gtk_signal_connect(GTK_OBJECT(dismiss_button), "clicked", GTK_SIGNAL_FUNC(file_print_cancel_callback), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(help_button), "clicked", GTK_SIGNAL_FUNC(file_print_help_callback), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(print_button), "clicked", GTK_SIGNAL_FUNC(file_print_ok_callback), (gpointer)ss);
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
      
      epslabel = gtk_label_new(STR_eps_file_p);
      gtk_box_pack_start(GTK_BOX(epsbox), epslabel, FALSE, FALSE, 2);
      gtk_widget_show(epslabel);
      
      file_print_name = gtk_entry_new();
      gtk_entry_set_editable(GTK_ENTRY(file_print_name), TRUE);
      gtk_box_pack_start(GTK_BOX(epsbox), file_print_name, TRUE, TRUE, 2);
      set_background(file_print_name, (ss->sgx)->white);
      gtk_entry_set_text(GTK_ENTRY(file_print_name), eps_file(ss));
      gtk_widget_show(file_print_name);

      file_print_eps_or_lpr = gtk_check_button_new_with_label(STR_direct_to_printer);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(file_print_dialog)->vbox), file_print_eps_or_lpr, FALSE, FALSE, 6);
      gtk_widget_show(file_print_eps_or_lpr);
    }
  if (ss->print_choice == PRINT_SND)
    {
      nsp = any_selected_sound(ss);
      sprintf(print_string, "print %s", nsp->shortname);
    }
  else sprintf(print_string, "%s", STR_print_env);
  gtk_label_set_text(GTK_LABEL(file_print_message), print_string);
  gtk_widget_show(file_print_dialog);
}

char *ps_rgb(snd_state *ss, int pchan)
{
  char *buf;
  state_context *sx;
  GdkColor *color;
  sx = ss->sgx;
  switch (pchan)
    {
    case 0: color = sx->black;      break;
    case 1: color = sx->red;        break;
    case 2: color = sx->green;      break;
    case 3: color = sx->light_blue; break;
    default: color = sx->black;     break;
    }
  buf = (char *)CALLOC(128, sizeof(char));
  sprintf(buf, " %.2f %.2f %.2f RG\n", (float)color->red / 65535.0, (float)color->green / 65535.0, (float)color->blue / 65535.0);
  return(buf);
}

#if HAVE_GUILE_GTK
#include <guile-gtk.h>

#define Sg_file_print_dialog_widget  "sg-file-print-dialog-widget"

static SCM sg_file_print_dialog_widget(void) 
{
  #define H_file_print_dialog_widget "HELP"
  return(sgtk_wrap_gtkobj((GtkObject *)(file_print_dialog)));
}

void init_print_widgets(SCM local_doc)
{
  DEFINE_PROC(gh_new_procedure0_0(Sg_file_print_dialog_widget, sg_file_print_dialog_widget), H_file_print_dialog_widget);
}

#endif
