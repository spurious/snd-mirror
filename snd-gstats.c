#include "snd.h"

/* -------- STATS WINDOW -------- */

static GtkWidget *stats_window = NULL;
static GtkWidget *stats_form = NULL;

static void stats_help(GtkWidget *w, gpointer context) 
{
  stats_dialog_help((snd_state *)context);
}

static void stats_dismiss(GtkWidget *w, gpointer context)
{
  set_show_usage_stats((snd_state *)context, FALSE);
}

static void stats_delete(GtkWidget *w, GdkEvent *event, gpointer context)
{
  set_show_usage_stats((snd_state *)context, FALSE);
}

static void stats_update(GtkWidget *w, gpointer context) 
{
  snd_state *ss = (snd_state *)context;
  update_all_usage_stats(ss);
  check_stats_window(ss, TRUE);
}

void update_stats(snd_state *ss)
{
  int chars;
  if (stats_form == NULL) return;
  chars = gtk_text_get_length(GTK_TEXT(stats_form));
  if (chars > 0) gtk_editable_delete_text(GTK_EDITABLE(stats_form), 0, -1);
  gtk_text_freeze(GTK_TEXT(stats_form));
  update_stats_with_widget(ss, stats_form);
  gtk_text_thaw(GTK_TEXT(stats_form));
}

void update_stats_display(snd_state *ss, int all)
{
  /* dismiss update help -- update forces recalc of all stats */
  GtkWidget *help_button, *dismiss_button, *update_button, *table, *hscrollbar, *vscrollbar;
  if (!stats_window)
    {
      stats_window = gtk_dialog_new();
      set_dialog_widget(STATS_DIALOG, stats_window);
      gtk_signal_connect(GTK_OBJECT(stats_window), "delete_event", GTK_SIGNAL_FUNC(stats_delete), (gpointer)ss);
      gtk_window_set_title(GTK_WINDOW(stats_window), STR_Disk_and_Memory_Usage);
      gtk_window_set_policy(GTK_WINDOW(stats_window), TRUE, TRUE, FALSE); /* allow shrink or grow */
      set_background(stats_window, (ss->sgx)->basic_color);
      gtk_container_set_border_width (GTK_CONTAINER(stats_window), 10);
      gtk_widget_set_usize(GTK_WIDGET(stats_window), 650, 250);
      gtk_widget_realize(stats_window);
      add_dialog(ss, stats_window);

      help_button = gtk_button_new_with_label(STR_Help);
      dismiss_button = gtk_button_new_with_label(STR_Dismiss);
      update_button = gtk_button_new_with_label(STR_Update);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(stats_window)->action_area), dismiss_button, TRUE, TRUE, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(stats_window)->action_area), update_button, TRUE, TRUE, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(stats_window)->action_area), help_button, TRUE, TRUE, 10);
      gtk_signal_connect(GTK_OBJECT(dismiss_button), "clicked", GTK_SIGNAL_FUNC(stats_dismiss), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(help_button), "clicked", GTK_SIGNAL_FUNC(stats_help), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(update_button), "clicked", GTK_SIGNAL_FUNC(stats_update), (gpointer)ss);
      set_pushed_button_colors(help_button, ss);
      set_pushed_button_colors(dismiss_button, ss);
      set_pushed_button_colors(update_button, ss);
      gtk_widget_show(dismiss_button);
      gtk_widget_show(update_button);
      gtk_widget_show(help_button);

      table = gtk_table_new(2, 2, FALSE);
      stats_form = gtk_text_new(NULL, NULL);
      gtk_table_attach (GTK_TABLE(table), stats_form, 0, 1, 0, 1, 
			(GtkAttachOptions)(GTK_FILL | GTK_EXPAND), 
			(GtkAttachOptions)(GTK_FILL | GTK_EXPAND | GTK_SHRINK), 
			0, 0);
      gtk_text_set_editable(GTK_TEXT(stats_form), FALSE);
      gtk_text_set_word_wrap(GTK_TEXT(stats_form), FALSE);
      gtk_widget_show(stats_form);

      hscrollbar = gtk_hscrollbar_new (GTK_TEXT(stats_form)->hadj);
      set_background(hscrollbar, (ss->sgx)->position_color);
      gtk_table_attach (GTK_TABLE (table), hscrollbar, 0, 1, 1, 2, 
			(GtkAttachOptions)(GTK_EXPAND | GTK_FILL), 
			(GtkAttachOptions)(GTK_FILL), 
			0, 0);
      gtk_widget_show (hscrollbar);

      vscrollbar = gtk_vscrollbar_new (GTK_TEXT (stats_form)->vadj);
      set_background(vscrollbar, (ss->sgx)->position_color);
      gtk_table_attach (GTK_TABLE (table), vscrollbar, 1, 2, 0, 1, 
			(GtkAttachOptions)(GTK_FILL), 
			(GtkAttachOptions)(GTK_EXPAND | GTK_FILL | GTK_SHRINK), 
			0, 0);
      gtk_widget_show (vscrollbar);

      gtk_container_add(GTK_CONTAINER(GTK_DIALOG(stats_window)->vbox), table);
      gtk_widget_show(table);
      gtk_widget_show(stats_window);
    }
  else raise_dialog(stats_window);
  if (all) update_all_usage_stats(ss);
  update_stats(ss);
}

void check_stats_window(snd_state *ss, int val)
{
  /* if val==0, close active display if any, if val==1, open and spin through all current chans setting/gathering */
  if (val == 0)
    {
      if ((stats_window) && (GTK_WIDGET_VISIBLE(stats_window)))
	gtk_widget_hide(stats_window);
    }
  else
    {
      update_stats_display(ss, TRUE);
    }
}

#if DEBUGGING
char *stats_window_state(void);
char *stats_window_state(void)
{
  if (stats_form)
    return(gtk_editable_get_chars(GTK_EDITABLE(stats_form), 0, -1));
  return(NULL);
}
#endif
