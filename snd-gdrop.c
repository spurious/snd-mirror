/* TODO: drag and drop broken
 */

#include "snd.h"


/* -------- DROP -------- */

static GdkAtom STRING;

void intern_atoms (snd_state *ss)
{
  STRING = gdk_atom_intern("STRING",FALSE);
}

static GtkTargetEntry target_table[] = {
  { "STRING",     0, 0}
};

static void drag_data_received (GtkWidget *widget, GdkDragContext *context, gint x, gint y, GtkSelectionData *data, guint info, guint time)
{
  /* from examples/testdnd.c */
  snd_info *sp;
  if ((data->length >= 0) && (data->format == 8))
    {
      sp = snd_open_file((char *)(data->data),get_global_state());
      if (sp) select_channel(sp,0);
      /* value is the file name if dropped icon from filer */
      gtk_drag_finish (context,TRUE,FALSE,time);
      return;
    }
  gtk_drag_finish(context,FALSE,FALSE,time);
}

void InitializeDrop(snd_state *ss)
{
  /* called via startup func */
  GtkWidget *menu;
  menu = get_menubar();
  gtk_drag_dest_set(menu,GTK_DEST_DEFAULT_DROP,target_table,1,GDK_ACTION_COPY);
  gtk_signal_connect(GTK_OBJECT(menu),"drag_data_received",GTK_SIGNAL_FUNC(drag_data_received),NULL);
}


