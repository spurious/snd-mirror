#include "snd.h"

static GtkTargetEntry target_table[] = {
  { "STRING",     0, 0},
  { "FILE_NAME",  0, 0},
  { "text/plain", 0, 0}
};

static XEN drop_hook;

static void drag_data_received (GtkWidget *widget, GdkDragContext *context, gint x, gint y, 
				GtkSelectionData *data, guint info, guint time)
{
  snd_info *sp = NULL;
  if ((data->length >= 0) && 
      (data->format == 8))
    {
      if ((!(XEN_HOOKED(drop_hook))) || 
	  (!(XEN_TRUE_P(run_or_hook(drop_hook,
				    XEN_LIST_1(C_TO_XEN_STRING((char *)(data->data))),
				    "drop")))))
	{
	  sp = snd_open_file((char *)(data->data), get_global_state(), FALSE);
	  if (sp) select_channel(sp, 0);
	}
      gtk_drag_finish (context, TRUE, FALSE, time);
      return;
    }
  gtk_drag_finish(context, FALSE, FALSE, time);
}

void add_drop(snd_state *ss, GtkWidget *w)
{
  /* called via startup func */
  gtk_drag_dest_set(w, GTK_DEST_DEFAULT_DROP, target_table, 3, GDK_ACTION_COPY);
  g_signal_connect_closure_by_id(GTK_OBJECT(w),
				 g_signal_lookup("drag_data_received", G_OBJECT_TYPE(GTK_OBJECT(w))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(drag_data_received), NULL, 0),
				 0);
}


void g_init_gxdrop(void)
{
  #define H_drop_hook S_drop_hook " (filename) is called whenever Snd receives a drag-and-drop \
event. If the returns #t, the file is not opened by Snd."

  XEN_DEFINE_HOOK(drop_hook, S_drop_hook, 1, H_drop_hook); /* arg = filename */
}
