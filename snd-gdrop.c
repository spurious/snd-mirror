#include "snd.h"

typedef struct {
  void (*drop_watcher)(GtkWidget *w, const char *message, int x, int y, void *data);
  void (*drag_watcher)(GtkWidget *w, const char *message, int x, int y, drag_style_t dtype, void *data);
  GtkWidget *caller;
  void *context;
} drop_watcher_t;

static drop_watcher_t **drop_watchers = NULL;
static int drop_watchers_size = 0;

#define DROP_WATCHER_SIZE_INCREMENT 2

static int add_drop_watcher(GtkWidget *w, 
			    void (*drop_watcher)(GtkWidget *w, const char *message, int x, int y, void *data), 
			    void (*drag_watcher)(GtkWidget *w, const char *message, int x, int y, drag_style_t dtype, void *data), 
			    void *context)
{
  int loc = -1;
  if (!(drop_watchers))
    {
      loc = 0;
      drop_watchers_size = DROP_WATCHER_SIZE_INCREMENT;
      drop_watchers = (drop_watcher_t **)CALLOC(drop_watchers_size, sizeof(drop_watcher_t *));
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
	  drop_watchers = (drop_watcher_t **)REALLOC(drop_watchers, drop_watchers_size * sizeof(drop_watcher_t *));
	  for (i = loc; i < drop_watchers_size; i++) drop_watchers[i] = NULL;
	}
    }
  drop_watchers[loc] = (drop_watcher_t *)CALLOC(1, sizeof(drop_watcher_t));
  drop_watchers[loc]->drop_watcher = drop_watcher;
  drop_watchers[loc]->drag_watcher = drag_watcher;
  drop_watchers[loc]->context = context;
  drop_watchers[loc]->caller = w;
  return(loc);
}

static drop_watcher_t *find_drop_watcher(GtkWidget *caller)
{
  if (drop_watchers)
    {
      int i;
      for (i = 0; i < drop_watchers_size; i++)
	{
	  if (drop_watchers[i])
	    {
	      drop_watcher_t *d;
	      d = drop_watchers[i];
	      if (d->caller == caller)
		return(d);
	    }
	}
    }
  return(NULL);
}


enum {TARGET_STRING, TARGET_UTF8, TARGET_URL};

static GtkTargetEntry target_table[] = {
  {"STRING",        0, TARGET_STRING},
  {"FILE_NAME",     0, TARGET_STRING},
  {"text/plain",    0, TARGET_STRING},
  {"COMPOUND_TEXT", 0, TARGET_STRING}, 
  {"UTF8_STRING",   0, TARGET_UTF8},    /* untested */
  {"text/uri-list", 0, TARGET_URL}
};

static XEN drop_hook;

static void drag_data_received(GtkWidget *caller, GdkDragContext *context, gint mx, gint my, 
			       GtkSelectionData *data, guint info, guint time)
{
  /* data->target */
  if ((data->length >= 0) && 
      (data->format == 8))
    {
      gsize bread, bwritten;
      GError *error;
      char *str;
      if (info == TARGET_STRING)
	str = (char *)(data->data);
      else str = (char *)g_filename_from_utf8((gchar *)(data->data), data->length, &bread, &bwritten, &error);
      if ((!(XEN_HOOKED(drop_hook))) || 
	  (!(XEN_TRUE_P(run_or_hook(drop_hook,
				    XEN_LIST_1(C_TO_XEN_STRING(str)),
				    S_drop_hook)))))
	{
	  drop_watcher_t *d;
	  d = find_drop_watcher(caller);
	  if (d)
	    {
	      /* loop through possible list of filenames, calling watcher on each */
	      char *filename;
	      int len = 0, i, j = 0;
	      len = snd_strlen(str);
	      filename = (char *)CALLOC(len, sizeof(char));
	      for (i = 0; i < len; i++)
		{
		  if (isspace(str[i]))
		    {
		      if (j > 0)
			{
			  filename[j] = '\0';
			  if (strncmp(filename, "file://", 7) == 0)
			    {
			      char *tmp;
			      tmp = (char *)(filename + 7);
			      (*(d->drop_watcher))(caller, (const char *)tmp, mx, my, d->context);
			    }
			  else (*(d->drop_watcher))(caller, (const char *)filename, mx, my, d->context);
			  j = 0;
			}
		      /* else ignore extra white space chars */
		    }
		  else
		    {
		      filename[j++] = str[i];
		    }
		}
	      FREE(filename);
	    }
	}
      gtk_drag_finish (context, true, false, time);
      return;
    }
  gtk_drag_finish(context, false, false, time);
}

static void drag_leave(GtkWidget *w, GdkDragContext *context, guint time)
{
  drop_watcher_t *d;
  d = find_drop_watcher(w);
  if ((d) && (d->drag_watcher))
    (*(d->drag_watcher))(w, NULL, 0, 0, DRAG_LEAVE, d->context);
}

static gboolean drag_motion(GtkWidget *w, GdkDragContext *context, gint x, gint y, guint time)
{
  drop_watcher_t *d;
  d = find_drop_watcher(w);
  if ((d) && (d->drag_watcher))
    (*(d->drag_watcher))(w, NULL, x, y, DRAG_MOTION, d->context);
  return(true); /* this is what the examples return in gtk/tests/testdnd.c -- don't know what it means, if anything */
}

void add_drag_and_drop(GtkWidget *w, 
		       void (*drop_watcher)(GtkWidget *w, const char *message, int x, int y, void *data), 
		       void (*drag_watcher)(GtkWidget *w, const char *message, int x, int y, drag_style_t dtype, void *data), 
		       void *context)
{
  gtk_drag_dest_set(w, GTK_DEST_DEFAULT_ALL, target_table, 6, (GdkDragAction)(GDK_ACTION_COPY | GDK_ACTION_MOVE | GDK_ACTION_LINK));
  SG_SIGNAL_CONNECT(w, "drag_data_received", drag_data_received, NULL);
  SG_SIGNAL_CONNECT(w, "drag_motion", drag_motion, NULL);
  SG_SIGNAL_CONNECT(w, "drag_leave", drag_leave, NULL);
  add_drop_watcher(w, drop_watcher, drag_watcher, context);
}

void add_drop(GtkWidget *w, 
	      void (*drop_watcher)(GtkWidget *w, const char *message, int x, int y, void *data), 
	      void *context)
{
  add_drag_and_drop(w, drop_watcher, NULL, context);
}


void g_init_gxdrop(void)
{
  #define H_drop_hook S_drop_hook " (filename): called whenever Snd receives a drag-and-drop \
event. If it returns " PROC_TRUE ", the file is not opened by Snd."

  drop_hook = XEN_DEFINE_HOOK(S_drop_hook, 1, H_drop_hook); /* arg = filename */
}
