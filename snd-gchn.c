#include "snd.h"

enum {
    W_main_window,
    W_edhist, W_edscroll,
    W_graph_window,
    W_wf_buttons,
      W_f, W_w,
    W_zy, W_sy,
    W_bottom_scrollers,
      W_sx, W_zx,
    W_graph,
    W_gzy, W_gsy,
    W_up_ev, W_down_ev
};

enum {W_zy_adj, W_zx_adj, W_sy_adj, W_sx_adj, W_gzy_adj, W_gsy_adj};

#define NUM_CHAN_WIDGETS 17
#define NUM_CHAN_ADJS 6
#define DEFAULT_EDIT_HISTORY_WIDTH 1

GtkWidget *channel_graph(chan_info *cp)      {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_widgets[W_graph]);   else return(NULL);}
GtkWidget *channel_sx(chan_info *cp)         {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_widgets[W_sx]);      else return(NULL);}
GtkWidget *channel_sy(chan_info *cp)         {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_widgets[W_sy]);      else return(NULL);}
GtkWidget *channel_zx(chan_info *cp)         {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_widgets[W_zx]);      else return(NULL);}
GtkWidget *channel_zy(chan_info *cp)         {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_widgets[W_zy]);      else return(NULL);}
static GtkWidget *channel_gsy(chan_info *cp) {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_widgets[W_gsy]);     else return(NULL);}
static GtkWidget *channel_gzy(chan_info *cp) {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_widgets[W_gzy]);     else return(NULL);}
GtkWidget *channel_w(chan_info *cp)          {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_widgets[W_w]);       else return(NULL);}
GtkWidget *channel_f(chan_info *cp)          {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_widgets[W_f]);       else return(NULL);}
GtkWidget *channel_up_arrow(chan_info *cp)   {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_widgets[W_up_ev]);   else return(NULL);}
GtkWidget *channel_down_arrow(chan_info *cp) {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_widgets[W_down_ev]); else return(NULL);}

static GtkWidget *channel_main_pane(chan_info *cp) {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_widgets[W_main_window]); else return(NULL);}
static GtkObject *gsy_adj(chan_info *cp)           {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_adjs[W_gsy_adj]);        else return(NULL);}
static GtkObject *gzy_adj(chan_info *cp)           {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_adjs[W_gzy_adj]);        else return(NULL);}
static GtkObject *sy_adj(chan_info *cp)            {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_adjs[W_sy_adj]);         else return(NULL);}
static GtkObject *sx_adj(chan_info *cp)            {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_adjs[W_sx_adj]);         else return(NULL);}
static GtkObject *zy_adj(chan_info *cp)            {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_adjs[W_zy_adj]);         else return(NULL);}
static GtkObject *zx_adj(chan_info *cp)            {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_adjs[W_zx_adj]);         else return(NULL);}

static Float sqr(Float a) {return(a*a);}
static Float cube (Float a) {return(a*a*a);}

int channel_open_pane(chan_info *cp, void *ptr)
{
  gtk_widget_show(channel_main_pane(cp));
  return(0);
}

static void sy_changed(float value, chan_info *cp)
{
  axis_info *ap;
  ap = cp->axis;
  if (ap == NULL) return;
  ap->sy = value - ap->zy;
  if (ap->sy < 0.0) ap->sy = 0.0;
  apply_y_axis_change(ap, cp);
}

static void sx_changed(float value, chan_info *cp)
{
  axis_info *ap;
  snd_info *sp;
  ap = cp->axis;
  if (ap == NULL) return;
  sp = cp->sound;
  ap->sx = value;
  apply_x_axis_change(ap, cp, sp);
}

static void zy_changed(float value, chan_info *cp)
{ 
  axis_info *ap;
  Float old_zy;
  ap = cp->axis;
  if (ap == NULL) return;
  if (value < .01) value = .01;
  old_zy = ap->zy;
  ap->zy = sqr(value);
  ap->sy += (.5 * (old_zy - ap->zy)); /* try to keep wave centered */
  if (ap->sy < 0) ap->sy = 0;
  apply_y_axis_change(ap, cp);
  resize_sy(cp);
}

#define X_RANGE_CHANGEOVER 20.0

static void zx_changed(float value, chan_info *cp)
{ 
  axis_info *ap;
  snd_info *sp;
  snd_state *ss;
  sp = cp->sound;
  ss = cp->state;
  ap = cp->axis;
  if (ap == NULL) return;
  if (ap->xmax == 0.0) return;
  if (ap->xmax <= ap->xmin) 
    {
      ap->xmax = ap->xmin + .001;
      ap->x_ambit = .001;
    }
  if (value < .01) value = .01;
  if (ap->x_ambit < X_RANGE_CHANGEOVER)
    ap->zx = sqr(value);
  else ap->zx = cube(value);
  /* if cursor visible, focus on that, else selection, else mark, else left side */
  focus_x_axis_change(ap, cp, sp, zoom_focus_style(ss));
  resize_sx(cp);
}

void set_zx_scrollbar_value(chan_info *cp, Float value)
{
  GtkObject *adj;
  adj = zx_adj(cp);
  GTK_ADJUSTMENT(adj)->value = value;
  gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj));
}

static void set_scrollbar(GtkObject *adj, Float position, Float range) /* position and range 0 to 1.0 */
{
  GTK_ADJUSTMENT(adj)->page_size = range;
  GTK_ADJUSTMENT(adj)->value = position;
  gtk_adjustment_changed(GTK_ADJUSTMENT(adj));
}

static void gzy_changed(float value, chan_info *cp)
{
  if (cp->axis == NULL) return;
  cp->gzy = value;
  GTK_ADJUSTMENT(gsy_adj(cp))->page_size = value; 
  gtk_adjustment_changed(GTK_ADJUSTMENT(gsy_adj(cp)));
  map_over_sound_chans(cp->sound, update_graph, NULL);
}

static void gsy_changed(float value, chan_info *cp)
{
  if (cp->axis == NULL) return;
  cp->gsy = cp->gzy * value;
  map_over_sound_chans(cp->sound, update_graph, NULL);
}

Float gsy_value(chan_info *cp)
{
  return(1.0 - GTK_ADJUSTMENT(gsy_adj(cp))->value - GTK_ADJUSTMENT(gsy_adj(cp))->page_size);
}

Float gsy_size(chan_info *cp)
{
  return(GTK_ADJUSTMENT(gsy_adj(cp))->page_size);
}

void initialize_scrollbars(chan_info *cp)
{
  axis_info *ap;
  snd_info *sp;
  ap = cp->axis;
  sp = cp->sound;
  set_scrollbar(sx_adj(cp), ap->sx, ap->zx);
  set_scrollbar(sy_adj(cp), 1.0 - ap->sy, ap->zy);
  if (ap->x_ambit < X_RANGE_CHANGEOVER)
    set_scrollbar(zx_adj(cp), sqrt(ap->zx), .1);  /* assume size is 10% of scrollbar length */
  else set_scrollbar(zx_adj(cp), pow(ap->zx, .333), .1);
  set_scrollbar(zy_adj(cp), 1.0 - ap->zy, .1);          /* assume 1.0 here so sqrt/cube decision, if any, is not needed */
  if ((sp->nchans > 1) && (cp->chan == 0) && (gsy_adj(cp)))
    {
      set_scrollbar(gsy_adj(cp), 1.0 - cp->gsy, cp->gzy);
      set_scrollbar(gzy_adj(cp), 
		    1.0 - cp->gzy, 
		    1.0 / (Float)(sp->nchans));
    }
}

void resize_sy(chan_info *cp)
{
  /* something changed the y axis view, so the scale scroller needs to reflect that change (in size and position) */
  axis_info *ap;
  Float size;
  ap = cp->axis;
  if (ap == NULL) return;
  size = (ap->y1 - ap->y0) / ap->y_ambit;
  set_scrollbar(sy_adj(cp), 
		1.0 - ((ap->y0 - ap->ymin) / ap->y_ambit + size), 
		size);
}

void resize_sx(chan_info *cp)
{
  axis_info *ap;
  snd_info *sp;
  ap = cp->axis;
  if (ap == NULL) return;
  sp = cp->sound;
  set_scrollbar(sx_adj(cp),
		(ap->x0 - ap->xmin) / ap->x_ambit,
		(ap->x1 - ap->x0) / ap->x_ambit);
}

void resize_zx(chan_info *cp)
{
  axis_info *ap;
  ap = cp->axis;
  if (ap == NULL) return;
  if (ap->x_ambit < X_RANGE_CHANGEOVER)
    set_scrollbar(zx_adj(cp), sqrt(ap->zx), .1);
  else set_scrollbar(zx_adj(cp), pow(ap->zx, 1.0 / 3.0), .1);
}

void resize_zy(chan_info *cp)
{
  axis_info *ap;
  ap = cp->axis;
  if (ap == NULL) return;
  set_scrollbar(zy_adj(cp), 1.0 - sqrt(ap->zy), .1);
}

static void W_sy_ValueChanged_Callback(GtkAdjustment *adj, gpointer context)
{
  chan_info *cp = (chan_info *)context;
  if (cp->active)
    {
      START_JUST_TIME(cp);
      sy_changed(1.0 - adj->value, cp);
      END_JUST_TIME(cp);
    }
}

static void W_sx_ValueChanged_Callback(GtkAdjustment *adj, gpointer context)
{
  chan_info *cp = (chan_info *)context;
  if (cp->active)
    {
      START_JUST_TIME(cp);
      sx_changed(adj->value, cp);
      END_JUST_TIME(cp);
    }
}

static void W_zy_ValueChanged_Callback(GtkAdjustment *adj, gpointer context)
{
  chan_info *cp = (chan_info *)context;
  if (cp->active)
    {
      START_JUST_TIME(cp);
      zy_changed(1.0 - adj->value, cp);
      END_JUST_TIME(cp);
    }
}

static void W_zx_ValueChanged_Callback(GtkAdjustment *adj, gpointer context)
{
  chan_info *cp = (chan_info *)context;
  if (cp->active)
    {
      START_JUST_TIME(cp);
      zx_changed(adj->value, cp);
      END_JUST_TIME(cp);
    }
}

static void W_gzy_ValueChanged_Callback(GtkAdjustment *adj, gpointer context)
{
  chan_info *cp = (chan_info *)context;
  if (cp->active)
    {
      START_JUST_TIME(cp);
      gzy_changed(1.0 - adj->value, cp);
      END_JUST_TIME(cp);
    }
}

static void W_gsy_ValueChanged_Callback(GtkAdjustment *adj, gpointer context)
{
  chan_info *cp = (chan_info *)context;
  if (cp->active)
    {
      START_JUST_TIME(cp);
      gsy_changed(1.0 - adj->value, cp);
      END_JUST_TIME(cp);
    }
}

static gint F_Button_Callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{ 
  f_button_callback((chan_info *)data, 
		    !(GTK_TOGGLE_BUTTON(w)->active), 
		    (ev->state & snd_ControlMask));
  return(TRUE);
}

static gint W_Button_Callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  w_button_callback((chan_info *)data, 
		    !(GTK_TOGGLE_BUTTON(w)->active), 
		    (ev->state & snd_ControlMask));
  return(TRUE);
}

#define MIN_REGRAPH_X 12
#define MIN_REGRAPH_Y 7
#define MIN_MIX_REGRAPH_X 30
#define MIN_MIX_REGRAPH_Y 30

static void Channel_Expose_Callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  chan_info *cp;
  snd_info *sp;
  snd_state *ss;

  cp = (chan_info *)data;
  if ((cp == NULL) || (cp->active != 1) || (cp->sound == NULL)) return;
  ss = cp->state;

  if ((cp->mixes) && (mix_dragging())) return;
  if ((ev->area.height < MIN_REGRAPH_Y) || 
      (ev->area.width < MIN_REGRAPH_X)) 
    return;
    
  sp = cp->sound;
  if (sp->channel_style != CHANNELS_SEPARATE)
    map_over_sound_chans(sp, update_graph, NULL);
  else update_graph(cp, NULL);

  sound_check_control_panel(sp, widget_height(SOUND_PANE(ss)));
}

static void Channel_Resize_Callback(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  snd_info *sp;
  chan_info *cp;
  cp = (chan_info *)data;
  if ((cp == NULL) || (cp->active != 1) || (cp->sound == NULL)) return;
  sp = cp->sound;
  if (sp == NULL) return;
  if (sp->channel_style != CHANNELS_SEPARATE)
    map_over_sound_chans(sp, update_graph, NULL);
  else update_graph(cp, NULL);
}

static SCM mouse_enter_graph_hook, mouse_leave_graph_hook;

#define UNPACK_SOUND(a) (a >> 16)
#define UNPACK_CHANNEL(a) (a & 0xff)
#define PACK_SOUND_AND_CHANNEL(a, b) ((a << 16) | b)

static void graph_mouse_enter(GtkWidget *w, GdkEventCrossing *ev, gpointer data)
{
  /* how many args does this thing take?  does it return an int?  what does the int mean? */
  int pdata;
  pdata = (int)gtk_object_get_user_data(GTK_OBJECT(w));
  if (HOOKED(mouse_enter_graph_hook))
    g_c_run_progn_hook(mouse_enter_graph_hook,
		       SCM_LIST2(TO_SMALL_SCM_INT(UNPACK_SOUND(pdata)),
				 TO_SMALL_SCM_INT(UNPACK_CHANNEL(pdata))),
		       S_mouse_enter_graph_hook);
  gdk_window_set_cursor(w->window, (((snd_state *)data)->sgx)->graph_cursor);
}

static void graph_mouse_leave(GtkWidget *w, GdkEventCrossing *ev, gpointer data)
{
  int pdata;
  pdata = (int)gtk_object_get_user_data(GTK_OBJECT(w));
  if (HOOKED(mouse_leave_graph_hook))
    g_c_run_progn_hook(mouse_leave_graph_hook,
		       SCM_LIST2(TO_SMALL_SCM_INT(UNPACK_SOUND(pdata)),
				 TO_SMALL_SCM_INT(UNPACK_CHANNEL(pdata))),
		       S_mouse_leave_graph_hook);
  gdk_window_set_cursor(w->window, (((snd_state *)data)->sgx)->arrow_cursor);
}

static void edit_select_Callback(GtkWidget *w, gint row, gint column, GdkEventButton *event, gpointer context)
{
  /* undo/redo to reach selected position */
  chan_info *cp = (chan_info *)context;
#if DEBUGGING
  if (event == NULL) return;
#endif
  edit_select_callback(cp, row, (event->state & snd_ControlMask));
}

static void hide_gz_scrollbars(snd_info *sp)
{
  chan_info *cp;
  cp = sp->chans[0];
  if (channel_gsy(cp))
    {
      gtk_widget_hide(channel_gsy(cp));
      gtk_widget_hide(channel_gzy(cp));
    }
}

static void show_gz_scrollbars(snd_info *sp)
{
  chan_info *cp;
  cp = sp->chans[0];
  if (channel_gsy(cp))
    {
      gtk_widget_show(channel_gsy(cp));
      gtk_widget_show(channel_gzy(cp));
    }
}


/* edit history support */

void reflect_edit_history_change(chan_info *cp)
{
  /* new edit so it is added, and any trailing lines removed */
  chan_context *cx;
  GtkWidget *lst;
  snd_state *ss;
  snd_info *sp;
  int i, eds;
  char *str;
  ss = cp->state;
  cx = cp->cgx;
  if (cx)
    {
      lst = cx->chan_widgets[W_edhist];
      if (lst)
	{
	  eds = cp->edit_ctr;
	  while ((eds<(cp->edit_size-1)) && (cp->edits[eds + 1])) eds++;
	  if (eds >= 0)
	    {
	      gtk_clist_clear(GTK_CLIST(lst));
	      sp = cp->sound;
	      str = sp->filename;
	      gtk_clist_append(GTK_CLIST(lst), &str);
	      for (i = 1; i <= eds; i++) 
		{
		  str = edit_to_string(cp, i);
		  gtk_clist_append(GTK_CLIST(lst), &str);
		}
	      gtk_signal_handler_block_by_data(GTK_OBJECT(lst), (gpointer)cp);
	      gtk_clist_select_row(GTK_CLIST(lst), cp->edit_ctr, 0);
	      gtk_clist_moveto(GTK_CLIST(lst), cp->edit_ctr, 0, 0.5, 0.5);
	      gtk_signal_handler_unblock_by_data(GTK_OBJECT(lst), (gpointer)cp);
	      goto_graph(cp);
	    }
	}
    }
}

void reflect_save_as_in_edit_history(chan_info *cp, char *filename)
{
  chan_context *cx;
  GtkWidget *lst;
  char *new_line;
  if (cp->edit_ctr < 1) return; /* Sun segfaults if 0 here! (apparently the usual C library strlen null bug) */
  cx = cp->cgx;
  if (cx)
    {
      lst = cx->chan_widgets[W_edhist];
      if (lst)
	{
	  new_line = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
	  mus_snprintf(new_line, PRINT_BUFFER_SIZE,
		  "%s: (save-sound-as \"%s\")", 
		  edit_to_string(cp, cp->edit_ctr), 
		  filename);
	  gtk_clist_set_text(GTK_CLIST(lst), cp->edit_ctr, 0, new_line);
	  FREE(new_line);
	}
    }
}

void reflect_edit_counter_change(chan_info *cp)
{
  /* undo/redo/revert -- change which line is highlighted */
  chan_context *cx;
  GtkWidget *lst;
  snd_state *ss;
  ss = cp->state;
  cx = cp->cgx;
  if (cx)
    {
      lst = cx->chan_widgets[W_edhist];
      if (lst)
	{
	  gtk_signal_handler_block_by_data(GTK_OBJECT(lst), (gpointer)cp);
	  gtk_clist_select_row(GTK_CLIST(lst), cp->edit_ctr, 0);
	  gtk_clist_moveto(GTK_CLIST(lst), cp->edit_ctr, 0, 0.5, 0.5);
	  gtk_signal_handler_unblock_by_data(GTK_OBJECT(lst), (gpointer)cp);
	  goto_graph(cp);
	}
    }
}


/* for combined cases, the incoming chan_info pointer is always chan[0], 
 * but the actual channel depends on placement if mouse oriented.
 * virtual_selected_channel(cp) (snd-chn.c) retains the current selected channel
 */

static gint real_graph_key_press(GtkWidget *w, GdkEventKey *ev, gpointer data)
{
  chan_info *cp = (chan_info *)data;
  int keysym, theirs;
  int x, y;
  snd_state *ss;
  GdkModifierType key_state;
  gdk_window_get_pointer(ev->window, &x, &y, &key_state);
  key_state = (GdkModifierType)(ev->state);
  keysym = ev->keyval;
  ss = cp->state;
  /* fprintf(stderr, "grf: %s %d ", gdk_keyval_name(keysym), key_state); */
  theirs = key_press_callback(cp, x, y, ev->state, keysym);
  if (theirs) (ss->sgx)->graph_is_active = FALSE;
  gtk_signal_emit_stop_by_name(GTK_OBJECT(w), "key_press_event");
  return(TRUE);
}

gint graph_key_press(GtkWidget *w, GdkEventKey *ev, gpointer data)
{
  chan_info *cp = (chan_info *)data;
  int keysym, theirs;
  int x, y;
  snd_state *ss;
  GdkModifierType key_state;
  gdk_window_get_pointer(ev->window, &x, &y, &key_state);
  key_state = (GdkModifierType)(ev->state);
  keysym = ev->keyval;
  ss = cp->state;
  /* fprintf(stderr, "key: %s %d ", gdk_keyval_name(keysym), key_state); */
  theirs = key_press_callback(cp, x, y, ev->state, keysym);
  if (theirs) (ss->sgx)->graph_is_active = TRUE;
  return(TRUE);
}
 
static void graph_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  chan_info *cp = (chan_info *)data;
  snd_state *ss;
  /* fprintf(stderr, "graph press "); */
  ss = cp->state;
  if ((ev->type == GDK_BUTTON_PRESS) && (ev->button == 2))
    create_popup_menu(ss, ev->button, ev->time);
  else
    {
      (ss->sgx)->graph_is_active = TRUE;
      gtk_widget_grab_focus(w);
      ((cp->sound)->sgx)->mini_active = 0;
      graph_button_press_callback(cp, (int)(ev->x), (int)(ev->y), ev->state, ev->button, ev->time);
    }
}

static void graph_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  graph_button_release_callback((chan_info *)data, (int)(ev->x), (int)(ev->y), ev->state, ev->button);
}

static void graph_button_motion(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{ 
  int x, y;
  GdkModifierType state;
  if (ev->state & GDK_BUTTON1_MASK)
    {
      if (ev->is_hint)
	{
	  gdk_window_get_pointer(ev->window, &x, &y, &state);
	}
      else
	{
	  x = (int)(ev->x);
	  y = (int)(ev->y);
	}
      graph_button_motion_callback((chan_info *)data, x, y, ev->time, 200);
    }
}

void add_channel_window(snd_info *sp, int channel, snd_state *ss, int chan_y, int insertion, GtkWidget *main, int button_style)
{
  GtkWidget **cw;
  GtkObject **adjs;
  chan_info *cp;
  chan_context *cx;
  axis_context *cax;
  state_context *sx;
  int make_widgets, need_extra_scrollbars;
  make_widgets = ((sp->chans[channel]) == NULL);
  sp->chans[channel] = make_chan_info(sp->chans[channel], channel, sp, ss);
  cp = sp->chans[channel];
  cx = cp->cgx;
  if (cx->chan_widgets == NULL) 
    {
      cw = (GtkWidget **)CALLOC(NUM_CHAN_WIDGETS, sizeof(GtkWidget *));
      adjs = (GtkObject **)CALLOC(NUM_CHAN_ADJS, sizeof(GtkObject *));
      (cp->cgx)->chan_widgets = cw;
      (cp->cgx)->chan_adjs = adjs;
    }
  else
    {
      cw = cx->chan_widgets;
      adjs = cx->chan_adjs;
    }
  sx = ss->sgx;
  need_extra_scrollbars = ((!main) && (channel == 0));
  if (make_widgets)
    {
      if (!main)
	{
	  cw[W_main_window] = gtk_hpaned_new();
	  set_backgrounds(cw[W_main_window], (ss->sgx)->sash_color);
	  gtk_container_set_border_width(GTK_CONTAINER(cw[W_main_window]), 2);
	  gtk_paned_set_handle_size(GTK_PANED(cw[W_main_window]), 6);
	  /* gtk_paned_add1(GTK_PANED(w_snd_pane(sp)), cw[W_main_window]); */
	  gtk_box_pack_start(GTK_BOX(w_snd_pane_box(sp)), cw[W_main_window], TRUE, TRUE, 0);

	  cw[W_edhist] = gtk_clist_new(1);
	  gtk_clist_set_selection_mode(GTK_CLIST(cw[W_edhist]), GTK_SELECTION_SINGLE);
	  gtk_clist_set_shadow_type(GTK_CLIST(cw[W_edhist]), GTK_SHADOW_ETCHED_IN);
	  gtk_clist_column_titles_passive(GTK_CLIST(cw[W_edhist]));
	  gtk_signal_connect(GTK_OBJECT(cw[W_edhist]), "select_row", GTK_SIGNAL_FUNC(edit_select_Callback), (gpointer)cp);

	  cw[W_edscroll] = gtk_scrolled_window_new(NULL, NULL);
	  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(cw[W_edscroll]), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	  gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(cw[W_edscroll]), cw[W_edhist]);
	  gtk_paned_add1(GTK_PANED(cw[W_main_window]), cw[W_edscroll]);
	  gtk_widget_show(cw[W_edscroll]);
	  gtk_widget_show(cw[W_edhist]);

	  gtk_signal_connect(GTK_OBJECT(cw[W_edscroll]), "key_press_event", GTK_SIGNAL_FUNC(real_graph_key_press), (gpointer)cp);
	  gtk_signal_connect(GTK_OBJECT(cw[W_edhist]), "key_press_event", GTK_SIGNAL_FUNC(real_graph_key_press), (gpointer)cp);
	}
      else cw[W_main_window] = main;

      cw[W_graph_window] = gtk_table_new(3, 5, FALSE);
      gtk_paned_add2(GTK_PANED(cw[W_main_window]), cw[W_graph_window]);

      cw[W_graph] = gtk_drawing_area_new();
      gtk_object_set_user_data(GTK_OBJECT(cw[W_graph]), (gpointer)(PACK_SOUND_AND_CHANNEL(sp->index, cp->chan)));
      gtk_widget_set_events(cw[W_graph], GDK_ALL_EVENTS_MASK);
      gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_graph], 2, 3, 0, 2, 
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND), 
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND | GTK_SHRINK), 
		       0, 0);
      set_background(cw[W_graph], (ss->sgx)->graph_color);
      set_foreground(cw[W_graph], (ss->sgx)->data_color);
      gtk_widget_show(cw[W_graph]);
      if (button_style == WITH_FW_BUTTONS)
	{
	  gtk_signal_connect(GTK_OBJECT(cw[W_graph]), "expose_event", GTK_SIGNAL_FUNC(Channel_Expose_Callback), (gpointer)cp);
	  gtk_signal_connect(GTK_OBJECT(cw[W_graph]), "configure_event", GTK_SIGNAL_FUNC(Channel_Resize_Callback), (gpointer)cp);
	  gtk_signal_connect(GTK_OBJECT(cw[W_graph]), "enter_notify_event", GTK_SIGNAL_FUNC(graph_mouse_enter), (gpointer)ss);
	  gtk_signal_connect(GTK_OBJECT(cw[W_graph]), "leave_notify_event", GTK_SIGNAL_FUNC(graph_mouse_leave), (gpointer)ss);
	  gtk_signal_connect(GTK_OBJECT(cw[W_graph]), "key_press_event", GTK_SIGNAL_FUNC(real_graph_key_press), (gpointer)cp);
	  gtk_signal_connect(GTK_OBJECT(cw[W_graph]), "button_press_event", GTK_SIGNAL_FUNC(graph_button_press), (gpointer)cp);
	  gtk_signal_connect(GTK_OBJECT(cw[W_graph]), "button_release_event", GTK_SIGNAL_FUNC(graph_button_release), (gpointer)cp);
	  gtk_signal_connect(GTK_OBJECT(cw[W_graph]), "motion_notify_event", GTK_SIGNAL_FUNC(graph_button_motion), (gpointer)cp);
	}

      cw[W_bottom_scrollers] = gtk_vbox_new(TRUE, 0);
      gtk_box_set_spacing(GTK_BOX(cw[W_bottom_scrollers]), 0);
      gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_bottom_scrollers], 2, 3, 2, 3, 
		       (GtkAttachOptions)(GTK_EXPAND | GTK_FILL), 
		       (GtkAttachOptions)(GTK_FILL), 
		       0, 0);
      gtk_widget_show(cw[W_bottom_scrollers]);

      adjs[W_sx_adj] = gtk_adjustment_new(0.0, 0.0, 1.00, 0.001, 0.01, .01);
      cw[W_sx] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_sx_adj]));
      gtk_box_pack_start(GTK_BOX(cw[W_bottom_scrollers]), cw[W_sx], TRUE, TRUE, 0);
      set_background(cw[W_sx], (ss->sgx)->position_color);
      gtk_signal_connect(GTK_OBJECT(adjs[W_sx_adj]), "value_changed", GTK_SIGNAL_FUNC(W_sx_ValueChanged_Callback), (gpointer)cp);
      gtk_widget_show(cw[W_sx]);

      adjs[W_zx_adj] = gtk_adjustment_new(0.0, 0.0, 1.1, 0.001, 0.01, .1);
      cw[W_zx] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_zx_adj]));
      set_background(cw[W_zx], (ss->sgx)->zoom_color);
      gtk_box_pack_start(GTK_BOX(cw[W_bottom_scrollers]), cw[W_zx], TRUE, TRUE, 0);
      gtk_signal_connect(GTK_OBJECT(adjs[W_zx_adj]), "value_changed", GTK_SIGNAL_FUNC(W_zx_ValueChanged_Callback), (gpointer)cp);
      gtk_widget_show(cw[W_zx]);


      cw[W_wf_buttons] = gtk_vbox_new(TRUE, 0);
      gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_wf_buttons], 0, 2, 1, 3, GTK_SHRINK, GTK_SHRINK, 0, 0);
      gtk_widget_show(cw[W_wf_buttons]);
      
      if (button_style == WITH_FW_BUTTONS)
	{
	  cw[W_f] = gtk_check_button_new_with_label(STR_f);
	  gtk_box_pack_start(GTK_BOX(cw[W_wf_buttons]), cw[W_f], TRUE, TRUE, 0);
	  gtk_widget_show(cw[W_f]);
	  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(cw[W_f]), FALSE);
	  set_pushed_button_colors(cw[W_f], ss);
	  gtk_signal_connect(GTK_OBJECT(cw[W_f]), "button_press_event", GTK_SIGNAL_FUNC(F_Button_Callback), (gpointer)cp);
  
	  cw[W_w] = gtk_check_button_new_with_label(STR_w);
	  gtk_box_pack_start(GTK_BOX(cw[W_wf_buttons]), cw[W_w], TRUE, TRUE, 0);
	  gtk_widget_show(cw[W_w]);
	  set_pushed_button_colors(cw[W_w], ss);
	  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(cw[W_w]), TRUE);
	  gtk_signal_connect(GTK_OBJECT(cw[W_w]), "button_press_event", GTK_SIGNAL_FUNC(W_Button_Callback), (gpointer)cp);

	  /* these are needed to keep f/w buttons from flushing all keypress events after being pressed */
	  gtk_signal_connect(GTK_OBJECT(cw[W_f]), "key_press_event", GTK_SIGNAL_FUNC(real_graph_key_press), (gpointer)cp);
	  gtk_signal_connect(GTK_OBJECT(cw[W_w]), "key_press_event", GTK_SIGNAL_FUNC(real_graph_key_press), (gpointer)cp);
	}
      else
	{
	  cw[W_up_ev] = gtk_event_box_new();
	  gtk_box_pack_start(GTK_BOX(cw[W_wf_buttons]), cw[W_up_ev], TRUE, TRUE, 0);
	  gtk_widget_show(cw[W_up_ev]);

	  cw[W_f] = gtk_arrow_new(GTK_ARROW_UP, GTK_SHADOW_ETCHED_OUT);
	  gtk_container_add(GTK_CONTAINER(cw[W_up_ev]), cw[W_f]);
	  set_background(cw[W_f], (ss->sgx)->zoom_color);
	  gtk_widget_set_usize(cw[W_f], 14, 14);
	  gtk_widget_show(cw[W_f]);

	  cw[W_down_ev] = gtk_event_box_new();
	  gtk_box_pack_start(GTK_BOX(cw[W_wf_buttons]), cw[W_down_ev], TRUE, TRUE, 0);
	  gtk_widget_show(cw[W_down_ev]);

	  cw[W_w] = gtk_arrow_new(GTK_ARROW_DOWN, GTK_SHADOW_ETCHED_OUT);
	  gtk_container_add(GTK_CONTAINER(cw[W_down_ev]), cw[W_w]);
	  set_background(cw[W_w], (ss->sgx)->zoom_color);
	  gtk_widget_set_usize(cw[W_w], 14, 14);
	  gtk_widget_show(cw[W_w]);
	}

      adjs[W_zy_adj] = gtk_adjustment_new(0.0, 0.0, 1.1, 0.001, 0.01, .1);   /* 0 -> 1 (upside down) */
      cw[W_zy] = gtk_vscrollbar_new(GTK_ADJUSTMENT(adjs[W_zy_adj]));
      set_background(cw[W_zy], (ss->sgx)->zoom_color);
      gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_zy], 0, 1, 0, 1, 
		       (GtkAttachOptions)(GTK_FILL), 
		       (GtkAttachOptions)(GTK_EXPAND | GTK_FILL | GTK_SHRINK), 
		       0, 0);
      gtk_signal_connect(GTK_OBJECT(adjs[W_zy_adj]), "value_changed", GTK_SIGNAL_FUNC(W_zy_ValueChanged_Callback), (gpointer)cp);
      gtk_widget_show(cw[W_zy]);

      adjs[W_sy_adj] = gtk_adjustment_new(0.5, 0.0, 1.01, 0.001, 0.01, .01);
      cw[W_sy] = gtk_vscrollbar_new(GTK_ADJUSTMENT(adjs[W_sy_adj]));
      set_background(cw[W_sy], (ss->sgx)->position_color);
      gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_sy], 1, 2, 0, 1, 
		       (GtkAttachOptions)(GTK_FILL), 
		       (GtkAttachOptions)(GTK_EXPAND | GTK_FILL | GTK_SHRINK), 
		       0, 0);
      gtk_signal_connect(GTK_OBJECT(adjs[W_sy_adj]), "value_changed", GTK_SIGNAL_FUNC(W_sy_ValueChanged_Callback), (gpointer)cp);
      gtk_widget_show(cw[W_sy]);

      if (need_extra_scrollbars)
	{
	  adjs[W_gsy_adj] = gtk_adjustment_new(0.0, 0.0, 1.0, 0.001, 0.01, .01);
	  cw[W_gsy] = gtk_vscrollbar_new(GTK_ADJUSTMENT(adjs[W_gsy_adj]));
	  set_background(cw[W_gsy], (ss->sgx)->position_color);
	  gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_gsy], 3, 4, 0, 2, 
			   (GtkAttachOptions)(GTK_FILL), 
			   (GtkAttachOptions)(GTK_EXPAND | GTK_FILL | GTK_SHRINK), 
			   0, 0);
	  gtk_signal_connect(GTK_OBJECT(adjs[W_gsy_adj]), "value_changed", GTK_SIGNAL_FUNC(W_gsy_ValueChanged_Callback), (gpointer)cp);
	  gtk_widget_show(cw[W_gsy]);

	  adjs[W_gzy_adj] = gtk_adjustment_new(1.0, 0.0, 1.00, 0.001, 0.01, .01);
	  cw[W_gzy] = gtk_vscrollbar_new(GTK_ADJUSTMENT(adjs[W_gzy_adj]));
	  set_background(cw[W_gzy], (ss->sgx)->zoom_color);
	  gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_gzy], 4, 5, 0, 2, 
			   (GtkAttachOptions)(GTK_FILL), 
			   (GtkAttachOptions)(GTK_EXPAND | GTK_FILL | GTK_SHRINK), 
			   0, 0);
	  gtk_signal_connect(GTK_OBJECT(adjs[W_gzy_adj]), "value_changed", GTK_SIGNAL_FUNC(W_gzy_ValueChanged_Callback), (gpointer)cp);
	  gtk_widget_show(cw[W_gzy]);
	  
	  gtk_widget_hide(cw[W_gsy]);
	  gtk_widget_hide(cw[W_gzy]);
	}
      else
	{
	  cw[W_gsy] = NULL;
	  cw[W_gzy] = NULL;
	}

      gtk_paned_set_position(GTK_PANED(cw[W_main_window]), 1);
      gtk_widget_show(cw[W_graph_window]);

    }
  else recolor_graph(cp, FALSE); /* in case selection color left over from previous use */
  if ((sp->channel_style != CHANNELS_COMBINED) || (channel == 0))
    gtk_widget_show_all(cw[W_main_window]);

  if ((need_extra_scrollbars) && (sp->channel_style == CHANNELS_SEPARATE)) 
    hide_gz_scrollbars(sp); /* default is on in this case */  

  cax = cx->ax;
  cax->gc = sx->basic_gc;
  /* cax->wn has to wait until update_graph */
}

void set_peak_numbers_font(chan_info *cp)
{
  snd_state *ss;
  axis_context *ax;
  ss = cp->state;
  ax = (cp->cgx)->ax;
  ax->current_font = (ss->sgx)->button_fnt;
  gdk_gc_set_font(copy_GC(cp), (ss->sgx)->button_fnt);
}

void set_tiny_numbers_font(chan_info *cp)
{
  snd_state *ss;
  axis_context *ax;
  ss = cp->state;
  ax = (cp->cgx)->ax;
  ax->current_font = (ss->sgx)->tiny_fnt;
  gdk_gc_set_font(copy_GC(cp), (ss->sgx)->tiny_fnt);
}

void set_bold_peak_numbers_font(chan_info *cp)
{
  snd_state *ss;
  axis_context *ax;
  ss = cp->state;
  ax = (cp->cgx)->ax;
  ax->current_font = (ss->sgx)->bold_button_fnt;
  gdk_gc_set_font(copy_GC(cp), (ss->sgx)->bold_button_fnt);
}

COLOR_TYPE get_foreground_color(chan_info *cp, axis_context *ax)
{
  GdkGCValues gv;
  snd_state *ss;
  ss = cp->state;
  gdk_gc_get_values(ax->gc, &gv);
  return(gdk_color_copy(&(gv.foreground)));
}

void set_foreground_color(chan_info *cp, axis_context *ax, GdkColor *color)
{
  gdk_gc_set_foreground(ax->gc, color);
}

GdkGC *copy_GC(chan_info *cp)
{
  state_context *sx;
  sx = (cp->state)->sgx;
  if ((cp->cgx)->selected) return(sx->selected_basic_gc);
  return(sx->basic_gc);
}

GdkGC *erase_GC(chan_info *cp)
{
  state_context *sx;
  snd_info *sp;
  snd_state *ss;
  ss = cp->state;
  sp = cp->sound;
  sx = (cp->state)->sgx;
  if (((cp->cgx)->selected) ||
      ((sp) && (sp->channel_style == CHANNELS_SUPERIMPOSED) && (sp->index == ss->selected_sound)))
    return(sx->selected_erase_gc);
  return(sx->erase_gc);
}

void cleanup_cw(chan_info *cp)
{
  chan_context *cx;
  GtkWidget **cw;
  if ((cp) && (cp->cgx))
    {
      cx = cp->cgx;
      cx->selected = 0;
      cw = cx->chan_widgets;
      if (cw)
	{
	  if (cw[W_w])
	    {
	      set_toggle_button(cw[W_w], TRUE, FALSE, (void *)cp);
	      set_toggle_button(cw[W_f], FALSE, FALSE, (void *)cp);
	    }
	  gtk_widget_hide(channel_main_pane(cp));
	}
    }
}

void change_channel_style(snd_info *sp, int new_style)
{
  int i, old_style;
  snd_state *ss;
  chan_info *ncp, *cp, *pcp;
  int height[1];
  chan_context *mcgx;
  GtkWidget **cw;
  axis_info *ap;
  chan_context *cx;
  if ((sp) && (sp->nchans > 1))
    {
      ss = sp->state;
      old_style = sp->channel_style;
      if (new_style != old_style)
	{
	  sp->channel_style = new_style;
	  if (old_style == CHANNELS_COMBINED)
	    hide_gz_scrollbars(sp);
	  else 
	    {
	      if (new_style == CHANNELS_COMBINED)
		show_gz_scrollbars(sp);
	    }
	  if (old_style == CHANNELS_SUPERIMPOSED)
	    {
	      syncb(sp, FALSE);
	      /* set to blue? */
	    }
	  else
	    {
	      if (new_style == CHANNELS_SUPERIMPOSED)
		{
		  syncb(sp, TRUE);
		  /* set to green ? */
		  apply_y_axis_change((sp->chans[0])->axis, sp->chans[0]);
		  apply_x_axis_change((sp->chans[0])->axis, sp->chans[0], sp);
		}
	    }
	  height[0] = widget_height(w_snd_pane(sp)) - widget_height(w_snd_ctrls(sp)) - 16;
	  if (old_style == CHANNELS_SEPARATE)
	    {
	      ncp = sp->chans[0];
	      sound_lock_ctrls(sp, NULL);
	      /* channel_lock_pane(ncp, height); */
	      mcgx = ncp->cgx;
	      for (i = 1; i < sp->nchans; i++) 
		{
		  ncp = sp->chans[i];
		  cleanup_cw(ncp);
		  ncp->tcgx = mcgx;
		  reset_mix_graph_parent(ncp);
		}
	      channel_open_pane(sp->chans[0], NULL);
	      /* channel_unlock_pane(sp->chans[0], NULL); */
	      sound_unlock_ctrls(sp, NULL);
	      set_toggle_button(w_snd_combine(sp), TRUE, FALSE, (void *)sp);
	    }
	  else
	    {
	      if (new_style == CHANNELS_SEPARATE)
		{
		  /* height[0] = total space available */
		  height[0] /= sp->nchans;
		  sound_lock_ctrls(sp, NULL);
		  /* map_over_sound_chans(sp, channel_lock_pane, (void *)height); */
		  map_over_sound_chans(sp, channel_open_pane, NULL);
		  /* map_over_sound_chans(sp, channel_unlock_pane, NULL); */
		  sound_unlock_ctrls(sp, NULL);
		  for (i = 0; i < sp->nchans; i++) reset_mix_graph_parent(sp->chans[i]);
		  pcp = sp->chans[0];
		  ap = pcp->axis;
		  for (i = 1; i < sp->nchans; i++)
		    {
		      cp = sp->chans[i];
		      cp->tcgx = NULL;
		      cx = cp->cgx;
		      cw = cx->chan_widgets;
		      gtk_widget_show_all(cw[W_main_window]);
		      set_toggle_button(cw[W_f], cp->graph_transform_p, FALSE, (void *)cp);
		      set_toggle_button(cw[W_w], cp->graph_time_p, FALSE, (void *)cp);
		      /* these can get out of sync if changes are made in the unseparated case */
		      set_axes(cp, ap->x0, ap->x1, ap->y0, ap->y1);
		    }
		  set_toggle_button(w_snd_combine(sp), FALSE, FALSE, (void *)sp);
		}
	    }
	}
    }
}

int fixup_cp_cgx_ax_wn(chan_info *cp)
{
  GtkWidget *w; 
  axis_context *ax; 
  ax = (cp->cgx)->ax;
  if (cp->tcgx) 
    w = channel_graph((cp->sound)->chans[0]);
  else w = channel_graph(cp);
  ax->wn = w->window;
  return((int)(ax->wn));
}

int channel_unlock_pane(chan_info *cp, void *ptr) {return(0);}
/* static int channel_lock_pane(chan_info *cp, void *ptr) {return(0);} */

void g_init_gxchn(SCM local_doc)
{
  #define H_mouse_enter_graph_hook S_mouse_enter_graph_hook " (snd chn) is called when the mouse \
enters the drawing area (graph pane) of the given channel.\n\
  (add-hook! mouse-enter-graph-hook\n\
    (lambda (snd chn)\n\
      (focus-widget (car (channel-widgets snd chn)))))"

  #define H_mouse_leave_graph_hook S_mouse_leave_graph_hook " (snd chn) is called when the mouse \
leaves the drawing area (graph pane) of the given channel."

  mouse_enter_graph_hook = MAKE_HOOK(S_mouse_enter_graph_hook, 2, H_mouse_enter_graph_hook);    /* args = snd chn */
  mouse_leave_graph_hook = MAKE_HOOK(S_mouse_leave_graph_hook, 2, H_mouse_leave_graph_hook);    /* args = snd chn */
}
