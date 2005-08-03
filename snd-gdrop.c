#include "snd.h"

typedef struct {
  void (*watcher)(GtkWidget *w, const char *message, int x, int y, void *data);
  GtkWidget *caller;
  void *context;
} drop_watcher;

static drop_watcher **drop_watchers = NULL;
static int drop_watchers_size = 0;

#define DROP_WATCHER_SIZE_INCREMENT 2

static int add_drop_watcher(GtkWidget *w, void (*watcher)(GtkWidget *w, const char *message, int x, int y, void *data), void *context)
{
  int loc = -1;
  if (!(drop_watchers))
    {
      loc = 0;
      drop_watchers_size = DROP_WATCHER_SIZE_INCREMENT;
      drop_watchers = (drop_watcher **)CALLOC(drop_watchers_size, sizeof(drop_watcher *));
    }
  else
    {
      int i;
      for (i = 0; i < drop_watchers_size; i++)
	if (!(drop_watchers[i]))
	  {
	    loc = i;
	    break;
	  }
      if (loc == -1)
	{
	  loc = drop_watchers_size;
	  drop_watchers_size += DROP_WATCHER_SIZE_INCREMENT;
	  drop_watchers = (drop_watcher **)REALLOC(drop_watchers, drop_watchers_size * sizeof(drop_watcher *));
	  for (i = loc; i < drop_watchers_size; i++) drop_watchers[i] = NULL;
	}
    }
  drop_watchers[loc] = (drop_watcher *)CALLOC(1, sizeof(drop_watcher));
  drop_watchers[loc]->watcher = watcher;
  drop_watchers[loc]->context = context;
  drop_watchers[loc]->caller = w;
  return(loc);
}


enum {TARGET_STRING, TARGET_UTF8, TARGET_URL};

static GtkTargetEntry target_table[] = {
  {"STRING",        0, TARGET_STRING},
  {"FILE_NAME",     0, TARGET_STRING},
  {"text/plain",    0, TARGET_STRING}, /* untested */
  {"COMPOUND_TEXT", 0, TARGET_STRING}, 
  {"UTF8_STRING",   0, TARGET_UTF8},    /* untested */
  {"text/uri-list", 0, TARGET_URL}
};

static XEN drop_hook;

static void drag_data_received(GtkWidget *caller, GdkDragContext *context, gint x, gint y, 
			       GtkSelectionData *data, guint info, guint time)
{
  /* data->target */
  if ((data->length >= 0) && 
      (data->format == 8))
    {
      gsize bread, bwritten;
      GError *error;
      char *filename;
      if (info == TARGET_STRING)
	filename = (char *)(data->data);
      else filename = (char *)g_filename_from_utf8((gchar *)(data->data), data->length, &bread, &bwritten, &error);
      if ((!(XEN_HOOKED(drop_hook))) || 
	  (!(XEN_TRUE_P(run_or_hook(drop_hook,
				    XEN_LIST_1(C_TO_XEN_STRING(filename)),
				    S_drop_hook)))))
	{
	  if (drop_watchers)
	    {
	      int i;
	      for (i = 0; i < drop_watchers_size; i++)
		{
		  if (drop_watchers[i])
		    {
		      drop_watcher *d;
		      d = drop_watchers[i];
		      if (d->caller == caller)
			(*(d->watcher))(caller, (const char *)filename, x, y, d->context);
		    }
		}
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

void add_drop(GtkWidget *w, void (*watcher)(GtkWidget *w, const char *message, int x, int y, void *data), void *context)
{
  gtk_drag_dest_set(w, GTK_DEST_DEFAULT_ALL, target_table, 6, (GdkDragAction)(GDK_ACTION_COPY | GDK_ACTION_MOVE | GDK_ACTION_LINK));
  /* this (the cast to GdkDragAction) is actually a bug in gtk -- they are OR'ing these together so the correct type is some flavor of int */
  SG_SIGNAL_CONNECT(w, "drag_data_received", drag_data_received, NULL);
  SG_SIGNAL_CONNECT(w, "drag_motion", drag_motion, NULL);
  SG_SIGNAL_CONNECT(w, "drag_leave", drag_leave, NULL);
  add_drop_watcher(w, watcher, context);
}


void g_init_gxdrop(void)
{
  #define H_drop_hook S_drop_hook " (filename): called whenever Snd receives a drag-and-drop \
event. If it returns #t, the file is not opened by Snd."

  drop_hook = XEN_DEFINE_HOOK(S_drop_hook, 1, H_drop_hook); /* arg = filename */
}
