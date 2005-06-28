#include "snd.h"

enum {TARGET_STRING, TARGET_UTF8};

static GtkTargetEntry target_table[] = {
  {"STRING",        0, TARGET_STRING},
  {"FILE_NAME",     0, TARGET_STRING},
  {"text/plain",    0, TARGET_STRING}, /* untested */
  {"COMPOUND_TEXT", 0, TARGET_STRING}, /* hmm... -- why does Motif have elaborate converters for this if it's just a string? */
  {"UTF8_STRING",   0, TARGET_UTF8}    /* untested */
};

static XEN drop_hook;

static void drag_data_received (GtkWidget *widget, GdkDragContext *context, gint x, gint y, 
				GtkSelectionData *data, guint info, guint time)
{
  /* data->target */
  if ((data->length >= 0) && 
      (data->format == 8))
    {
      if ((!(XEN_HOOKED(drop_hook))) || 
	  (!(XEN_TRUE_P(run_or_hook(drop_hook,
				    XEN_LIST_1(C_TO_XEN_STRING((char *)(data->data))),
				    "drop")))))
	{
	  gsize bread, bwritten;
	  GError *error;
	  char *filename;
	  if (info == TARGET_STRING)
	    filename = (char *)(data->data);
	  else filename = (char *)g_filename_from_utf8((gchar *)(data->data), data->length, &bread, &bwritten, &error);
	  if (GTK_IS_DRAWING_AREA(widget))
	    mix_at_x_y(get_user_int_data(G_OBJECT(widget)), filename, x, y);
	  else
	    {
	      snd_info *sp = NULL;
	      ss->open_requestor = FROM_DRAG_AND_DROP;
	      sp = snd_open_file(filename, FILE_READ_WRITE);
	      if (sp) select_channel(sp, 0);
	    }
	}
      gtk_drag_finish (context, true, false, time);
      return;
    }
  gtk_drag_finish(context, false, false, time);
}

static void report_mouse_position_as_seconds(GtkWidget *w, gint x, gint y)
{
  snd_info *sp;
  chan_info *cp;
  int data, snd, chn;
  float seconds;
  data = get_user_int_data(G_OBJECT(w));
  chn = UNPACK_CHANNEL(data);
  snd = UNPACK_SOUND(data);
  sp = ss->sounds[snd];
  cp = sp->chans[chn];
  if ((sp->nchans > 1) && (sp->channel_style == CHANNELS_COMBINED))
    cp = which_channel(sp, y);    
  seconds = (float)(ungrf_x(cp->axis, x));
  if (seconds < 0.0) seconds = 0.0;
  if (sp->nchans > 1)
    report_in_minibuffer(sp, "drop to mix file in chan %d at %.4f", cp->chan + 1, seconds);
  else report_in_minibuffer(sp, "drop to mix file at %.4f", seconds);
}

static void clear_minibuffer_of(GtkWidget *w)
{
  int snd, data;
  data = get_user_int_data(G_OBJECT(w));
  snd = UNPACK_SOUND(data);
  clear_minibuffer(ss->sounds[snd]);
}

static bool have_drag_title = false;
static void drag_leave(GtkWidget *widget, GdkDragContext *context, guint time)
{
  if (GTK_IS_DRAWING_AREA(widget))
    clear_minibuffer_of(widget);
  else 
    {
      reflect_file_change_in_title();
      have_drag_title = false;
    }
}

static gboolean drag_motion(GtkWidget *widget, GdkDragContext *context, gint x, gint y, guint time)
{
  if (GTK_IS_DRAWING_AREA(widget))
    report_mouse_position_as_seconds(widget, x, y);
  else
    {
      if (!have_drag_title)
	{
	  char *new_title;
	  new_title = (char *)CALLOC(64, sizeof(char));
	  sprintf(new_title, "%s: drop to open file", ss->startup_title);
	  gtk_window_set_title(GTK_WINDOW(MAIN_SHELL(ss)), new_title);
	  have_drag_title = true;
	  FREE(new_title);
	}
    }
  return(true); /* this is what the examples return in gtk/tests/testdnd.c -- don't know what it means, if anything */
}

void add_drop(GtkWidget *w)
{
  gtk_drag_dest_set(w, GTK_DEST_DEFAULT_ALL, target_table, 5, (GdkDragAction)(GDK_ACTION_COPY | GDK_ACTION_MOVE));
  /* this (the cast to GdkDragAction) is actually a bug in gtk -- they are OR'ing these together so the correct type is some flavor of int */
  SG_SIGNAL_CONNECT(w, "drag_data_received", drag_data_received, NULL);
  SG_SIGNAL_CONNECT(w, "drag_motion", drag_motion, NULL);
  SG_SIGNAL_CONNECT(w, "drag_leave", drag_leave, NULL);
}


void g_init_gxdrop(void)
{
  #define H_drop_hook S_drop_hook " (filename): called whenever Snd receives a drag-and-drop \
event. If it returns #t, the file is not opened by Snd."

  drop_hook = XEN_DEFINE_HOOK(S_drop_hook, 1, H_drop_hook); /* arg = filename */
}
