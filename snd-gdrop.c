#include "snd.h"

static GtkTargetEntry target_table[] = {
  {"STRING",        0, 0},
  {"FILE_NAME",     0, 0},
  {"text/plain",    0, 0},
  {"COMPOUND_TEXT", 0, 0}
};

static XEN drop_hook;

static void drag_data_received (GtkWidget *widget, GdkDragContext *context, gint x, gint y, 
				GtkSelectionData *data, guint info, guint time)
{
  snd_state *ss;
  snd_info *sp = NULL;
  if ((data->length >= 0) && 
      (data->format == 8))
    {
      ss = get_global_state();
      if ((!(XEN_HOOKED(drop_hook))) || 
	  (!(XEN_TRUE_P(run_or_hook(drop_hook,
				    XEN_LIST_1(C_TO_XEN_STRING((char *)(data->data))),
				    "drop")))))
	{
	  if (GTK_IS_DRAWING_AREA(widget))
	    {
	      /* mix at mouse */
	      /* TODO: complete drop split */
	      int pdata, snd, chn;
	      char *origin;
	      pdata = get_user_int_data(G_OBJECT(widget));
	      chn = UNPACK_CHANNEL(pdata);
	      snd = UNPACK_SOUND(pdata);
	      if ((snd >= 0) &&
		  (snd < ss->max_sounds) && 
		  (snd_ok(ss->sounds[snd])) &&
		  (chn >= 0) &&
		  (chn < ss->sounds[snd]->nchans) &&
		  (mus_file_probe((char *)(data->data))))
		{
		  off_t sample;
		  char *fullname = NULL;
		  chan_info *cp;
		  sp = ss->sounds[snd];
		  cp = sp->chans[chn];
		  select_channel(sp, chn);
		  origin = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
		  sample = snd_round_off_t(ungrf_x(cp->axis, x) * (double)(SND_SRATE(sp)));
		  if (sample < 0) sample = 0;
		  mus_snprintf(origin, PRINT_BUFFER_SIZE, "drop mix %s " OFF_TD, (char *)(data->data), sample);
		  fullname = mus_expand_filename((char *)(data->data));
		  mix_complete_file(sp, sample, fullname, origin, with_mix_tags(ss));
		  if (fullname) FREE(fullname);
		  FREE(origin);
		}
	    }
	  else
	    {
	      sp = snd_open_file((char *)(data->data), ss, FALSE);
	      if (sp) select_channel(sp, 0);
	    }
	}
      gtk_drag_finish (context, TRUE, FALSE, time);
      return;
    }
  gtk_drag_finish(context, FALSE, FALSE, time);
}

static void report_mouse_position_as_seconds(GtkWidget *w, gint x)
{
  snd_state *ss;
  snd_info *sp;
  chan_info *cp;
  int data, snd, chn;
  float seconds;
  ss = get_global_state();
  data = get_user_int_data(G_OBJECT(w));
  chn = UNPACK_CHANNEL(data);
  snd = UNPACK_SOUND(data);
  sp = ss->sounds[snd];
  cp = sp->chans[chn];
  seconds = (float)(ungrf_x(cp->axis, x));
  if (seconds < 0.0) seconds = 0.0;
  if (sp->nchans > 1)
    report_in_minibuffer(sp, "drop to mix file in chan %d at %.4f", chn + 1, seconds);
  else report_in_minibuffer(sp, "drop to mix file at %.4f", seconds);
}

static void clear_minibuffer_of(GtkWidget *w)
{
  snd_state *ss;
  int snd, data;
  ss = get_global_state();
  data = get_user_int_data(G_OBJECT(w));
  snd = UNPACK_SOUND(data);
  clear_minibuffer(ss->sounds[snd]);
}

static int have_drag_title = FALSE;
void drag_leave(GtkWidget *widget, GdkDragContext *context, guint time)
{
  if (GTK_IS_DRAWING_AREA(widget))
    clear_minibuffer_of(widget);
  else 
    {
      reflect_file_change_in_title(get_global_state());
      have_drag_title = FALSE;
    }
}

gboolean drag_motion(GtkWidget *widget, GdkDragContext *context, gint x, gint y, guint time)
{
  if (GTK_IS_DRAWING_AREA(widget))
    report_mouse_position_as_seconds(widget, x);
  else
    {
      if (!have_drag_title)
	{
	  snd_state *ss;
	  char *new_title;
	  ss = get_global_state();
	  new_title = (char *)CALLOC(64, sizeof(char));
	  sprintf(new_title, "%s: drop to open file", ss->startup_title);
	  gtk_window_set_title(GTK_WINDOW(MAIN_SHELL(ss)), new_title);
	  have_drag_title = TRUE;
	  FREE(new_title);
	}
    }
  return(TRUE); /* this is what the examples return in gtk/tests/testdnd.c -- don't know what it means, if anything */
}

void add_drop(snd_state *ss, GtkWidget *w)
{
  gtk_drag_dest_set(w, GTK_DEST_DEFAULT_ALL, target_table, 4, (GdkDragAction)(GDK_ACTION_COPY | GDK_ACTION_MOVE));
  /* this (the cast to GdkDragAction) is actually a bug in gtk -- they are OR'ing these together so the correct type is some flavor of int */
  g_signal_connect_closure_by_id(GTK_OBJECT(w),
				 g_signal_lookup("drag_data_received", G_OBJECT_TYPE(GTK_OBJECT(w))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(drag_data_received), NULL, 0),
				 0);
  g_signal_connect(G_OBJECT(w), "drag_motion", G_CALLBACK(drag_motion), NULL);
  g_signal_connect(G_OBJECT(w), "drag_leave", G_CALLBACK(drag_leave), NULL);
}


void g_init_gxdrop(void)
{
  #define H_drop_hook S_drop_hook " (filename): called whenever Snd receives a drag-and-drop \
event. If it returns #t, the file is not opened by Snd."

  XEN_DEFINE_HOOK(drop_hook, S_drop_hook, 1, H_drop_hook); /* arg = filename */
}
