#include "snd.h"

static GtkTargetEntry target_table[] = {
  { "STRING",     0, 0},
  { "FILE_NAME",  0, 0},
  { "text/plain", 0, 0}
};

static void drag_data_received (GtkWidget *widget, GdkDragContext *context, gint x, gint y, GtkSelectionData *data, guint info, guint time)
{
  snd_info *sp;
  if ((data->length >= 0) && (data->format == 8))
    {
      sp = snd_open_file((char *)(data->data),get_global_state());
      if (sp) select_channel(sp,0);
      gtk_drag_finish (context,TRUE,FALSE,time);
      return;
    }
  gtk_drag_finish(context,FALSE,FALSE,time);
}

void InitializeDrop(snd_state *ss)
{
  /* called via startup func */
  gtk_drag_dest_set(MAIN_SHELL(ss),GTK_DEST_DEFAULT_DROP,target_table,3,GDK_ACTION_COPY);
  gtk_signal_connect(GTK_OBJECT(MAIN_SHELL(ss)),"drag_data_received",GTK_SIGNAL_FUNC(drag_data_received),NULL);
}


/* on the guile-gtk side, drag-and-drop is apparently not supported yet (version 0.19) */
