#include "snd.h"

enum {
    W_main_window,
    W_graph_window,
    W_wf_buttons,
      W_f, W_w,
    W_zy, W_sy,
    W_bottom_scrollers,
      W_sx, W_zx,
    W_graph,
    W_gzy, W_gsy,
    W_up_ev, W_down_ev,
    NUM_CHAN_WIDGETS
};

enum {W_zy_adj, W_zx_adj, W_sy_adj, W_sx_adj, W_gzy_adj, W_gsy_adj, NUM_CHAN_ADJS};


GtkWidget *channel_graph(chan_info *cp)      {return(cp->chan_widgets[W_graph]);}
static GtkWidget *channel_sx(chan_info *cp)  {return(cp->chan_widgets[W_sx]);}
static GtkWidget *channel_sy(chan_info *cp)  {return(cp->chan_widgets[W_sy]);}
static GtkWidget *channel_zx(chan_info *cp)  {return(cp->chan_widgets[W_zx]);}
static GtkWidget *channel_zy(chan_info *cp)  {return(cp->chan_widgets[W_zy]);}
static GtkWidget *channel_gsy(chan_info *cp) {return(cp->chan_widgets[W_gsy]);}
static GtkWidget *channel_gzy(chan_info *cp) {return(cp->chan_widgets[W_gzy]);}
GtkWidget *channel_w(chan_info *cp)          {return(cp->chan_widgets[W_w]);}
GtkWidget *channel_f(chan_info *cp)          {return(cp->chan_widgets[W_f]);}
GtkWidget *channel_up_arrow(chan_info *cp)   {return(cp->chan_widgets[W_up_ev]);}
GtkWidget *channel_down_arrow(chan_info *cp) {return(cp->chan_widgets[W_down_ev]);}


#define EDIT_HISTORY_LIST(Cp) (Cp)->edhist_list
#if HAVE_GTK_3
  #define EDIT_HISTORY_CLOSED 2
#else
  #define EDIT_HISTORY_CLOSED 1
#endif
/* in gtk2, 0 is a no-op here, leaving the edit history pane open, 
 * but in gtk 3 if less than about 30, we get 
 * "Gtk-CRITICAL **: gtk_paint_slider: assertion `width >= 0' failed"
 *   (which means the damned thing has to be open always)
 * or if we hide the scrolled text widget, the pane separator goes away!
 * So, in gtk3 the scrolled window does not display a horizontal scrollbar
 */

static GtkWidget *channel_main_pane(chan_info *cp) {return(cp->chan_widgets[W_main_window]);}
static GtkAdjustment *gsy_adj(chan_info *cp)           {return(cp->chan_adjs[W_gsy_adj]);}
static GtkAdjustment *gzy_adj(chan_info *cp)           {return(cp->chan_adjs[W_gzy_adj]);}
static GtkAdjustment *sy_adj(chan_info *cp)            {return(cp->chan_adjs[W_sy_adj]);}
static GtkAdjustment *sx_adj(chan_info *cp)            {return(cp->chan_adjs[W_sx_adj]);}
static GtkAdjustment *zy_adj(chan_info *cp)            {return(cp->chan_adjs[W_zy_adj]);}
static GtkAdjustment *zx_adj(chan_info *cp)            {return(cp->chan_adjs[W_zx_adj]);}


static mus_float_t sqr(mus_float_t a) {return(a * a);}

static mus_float_t cube(mus_float_t a) {return(a * a * a);}


bool channel_graph_is_visible(chan_info *cp)
{
  return((cp) &&
	 (channel_graph(cp)) &&
	 (widget_is_active(channel_graph(cp))) &&
	 (cp->sound) &&
	 /* here we may have a sound wrapper for variable display in which case the sound widgets are not implemented */
	 (((cp->sound->inuse == SOUND_WRAPPER) || (cp->sound->inuse == SOUND_REGION)) ||
	  ((cp->sound->inuse == SOUND_NORMAL) &&
	   /* other choice: SOUND_IDLE -> no display */
	   (w_snd_pane(cp->sound)) &&
	   (widget_is_active(w_snd_pane(cp->sound))))));
}


void channel_open_pane(chan_info *cp)
{
  gtk_widget_show(channel_main_pane(cp));
}


static void sy_changed(float value, chan_info *cp)
{
  axis_info *ap;
  ap = cp->axis;
  ap->sy = value - ap->zy;
  if (ap->sy < 0.0) ap->sy = 0.0;
  apply_y_axis_change(cp);
}


static void sx_changed(float value, chan_info *cp)
{
  axis_info *ap;
  ap = cp->axis;
  ap->sx = value;
  apply_x_axis_change(cp);
}


static void zy_changed(float value, chan_info *cp)
{ 
  axis_info *ap;
  mus_float_t old_zy;
  ap = cp->axis;
  if (value < .01) value = .01;
  old_zy = ap->zy;
  ap->zy = sqr(value);
  if (ap->zy < 1e-5) ap->zy = 1e-5;
  ap->sy += (.5 * (old_zy - ap->zy)); /* try to keep wave centered */
  if (ap->sy < 0) ap->sy = 0;
  apply_y_axis_change(cp);
  resize_sy(cp);
}


#define X_RANGE_CHANGEOVER 20.0

static void zx_changed(float value, chan_info *cp)
{ 
  axis_info *ap;
  double old_zx = 0.0;
  ap = cp->axis;
  if (ap->xmax == 0.0) return;
  if (ap->xmax <= ap->xmin) 
    {
      ap->xmax = ap->xmin + .001;
      ap->x_ambit = .001;
    }
  if (value < .01) value = .01;
  old_zx = ap->zx;
  if (ap->x_ambit < X_RANGE_CHANGEOVER)
    ap->zx = sqr(value);
  else ap->zx = cube(value);
  if (fabs(old_zx - ap->zx) > .00001) /* click on zoom is moving the window */
    {
      /* if cursor visible, focus on that, else selection, else mark, else left side */
      focus_x_axis_change(cp, zoom_focus_style(ss));

      /* focus_x_axis_change has already displayed the new graph, but in gtk the scrollbar setting
       *   in resize_sx will try to display everything again. So try to squelch it...
       */
      if (!(cp->squelch_update))
	{
	  cp->squelch_update = true;
	  resize_sx(cp);
	  cp->squelch_update = false;
	  clear_minibuffer(cp->sound); /* erase the "(update squelched)" message */
	}
      else resize_sx(cp);
    }
}


static void set_scrollbar(GtkAdjustment *adj, mus_float_t position, mus_float_t range) /* position and range 0 to 1.0 */
{
  ADJUSTMENT_SET_PAGE_SIZE(adj, range);
  ADJUSTMENT_SET_VALUE(adj, position);
}


/* restore_axes_data (snd-file.c) assumes change_gzy also fixes gsy */

void change_gzy(mus_float_t val, chan_info *cp)
{
  /* from snd_update */
  ADJUSTMENT_SET_PAGE_SIZE(gsy_adj(cp), 1.0 - val); 
  ADJUSTMENT_SET_VALUE(gsy_adj(cp), cp->gsy);
  ADJUSTMENT_SET_VALUE(gzy_adj(cp), val);
}


mus_float_t gsy_value(chan_info *cp)
{
  return(1.0 - (cp->gsy + ADJUSTMENT_PAGE_SIZE(gsy_adj(cp))));
}


mus_float_t gsy_size(chan_info *cp)
{
  return(ADJUSTMENT_PAGE_SIZE(gsy_adj(cp)));
}


static void set_zx(chan_info *cp, axis_info *ap)
{
  if (ap->x_ambit < X_RANGE_CHANGEOVER)
    set_scrollbar(zx_adj(cp), sqrt(ap->zx), .1);  /* assume size is 10% of scrollbar length */
  else set_scrollbar(zx_adj(cp), pow(ap->zx, .333), .1);
}


void set_z_scrollbars(chan_info *cp, axis_info *ap)
{
  set_zx(cp, ap);
  set_scrollbar(zy_adj(cp), 1.0 - sqrt(ap->zy), .1);
}


void initialize_scrollbars(chan_info *cp)
{
  axis_info *ap;
  snd_info *sp;

  cp->gzy = 0.0;
  cp->gsy = 0.0;

  ap = cp->axis;
  sp = cp->sound;
  set_scrollbar(sx_adj(cp), ap->sx, ap->zx);
  set_scrollbar(sy_adj(cp), 1.0 - ap->sy, ap->zy);
  set_z_scrollbars(cp, ap);
  if ((sp->nchans > 1) && (cp->chan == 0) && (gsy_adj(cp)))
    {
      set_scrollbar(gsy_adj(cp), 0.0, 1.0);
      set_scrollbar(gzy_adj(cp), 0.0, 1.0 / (mus_float_t)(sp->nchans));
    }
}


void resize_sy(chan_info *cp)
{
  /* something changed the y axis view, so the scale scroller needs to reflect that change (in size and position) */
  axis_info *ap;
  ap = cp->axis;
  if (ap->y_ambit != 0.0)
    {
      mus_float_t size;
      size = (ap->y1 - ap->y0) / ap->y_ambit;
      set_scrollbar(sy_adj(cp), 
		    1.0 - ((ap->y0 - ap->ymin) / ap->y_ambit + size), 
		    size);
    }
}


void resize_sy_and_zy(chan_info *cp)
{
  resize_sy(cp);
  set_scrollbar(zy_adj(cp), 1.0 - sqrt(cp->axis->zy), .1);
}


void resize_sx(chan_info *cp)
{
  axis_info *ap;
  ap = cp->axis;
  if (ap->x_ambit != 0.0)
    set_scrollbar(sx_adj(cp),
		  (ap->x0 - ap->xmin) / ap->x_ambit,
		  (ap->x1 - ap->x0) / ap->x_ambit);
}


void resize_sx_and_zx(chan_info *cp)
{
  resize_sx(cp);
  set_zx(cp, cp->axis);
}


static void sy_valuechanged_callback(GtkAdjustment *adj, gpointer context)
{
  /* see note above -- context may be garbage!! -- this is a huge bug in gtk */
  static bool ignore_sy_valuechanged_callback = false;
  chan_info *cp;

  cp = (chan_info *)get_user_data(G_OBJECT(adj));
  if ((cp->active == CHANNEL_HAS_AXES) &&
      (!ignore_sy_valuechanged_callback))
    {
      ignore_sy_valuechanged_callback = true;
      sy_changed(1.0 - ADJUSTMENT_VALUE(adj), cp);
      ignore_sy_valuechanged_callback = false;
    }
}


static void sx_valuechanged_callback(GtkAdjustment *adj, gpointer context)
{
  static bool ignore_sx_valuechanged_callback = false;
  chan_info *cp;

  cp = (chan_info *)get_user_data(G_OBJECT(adj));
  if ((cp->active == CHANNEL_HAS_AXES) &&
      (!ignore_sx_valuechanged_callback))
    {
      ignore_sx_valuechanged_callback = true;
      sx_changed(ADJUSTMENT_VALUE(adj), cp);
      ignore_sx_valuechanged_callback = false;
    }
}


static void zy_valuechanged_callback(GtkAdjustment *adj, gpointer context)
{
  static bool ignore_zy_valuechanged_callback = false;
  chan_info *cp;

  cp = (chan_info *)get_user_data(G_OBJECT(adj));
  if ((cp->active == CHANNEL_HAS_AXES) &&
      (!ignore_zy_valuechanged_callback))
    {
      ignore_zy_valuechanged_callback = true;
      zy_changed(1.0 - ADJUSTMENT_VALUE(adj), cp);
      ignore_zy_valuechanged_callback = false;
    }
}

static void zx_valuechanged_callback(GtkAdjustment *adj, gpointer context)
{
  chan_info *cp;
  static bool ignore_zx_valuechanged_callback = false; /*see below */
  
  cp = (chan_info *)get_user_data(G_OBJECT(adj));
  if ((cp->active == CHANNEL_HAS_AXES) &&
      (!ignore_zx_valuechanged_callback))
    {
      ignore_zx_valuechanged_callback = true;
      zx_changed(ADJUSTMENT_VALUE(adj), cp);
      ignore_zx_valuechanged_callback = false;
    }

  /* it's easy to get infinite recursion here: 
   *    zx_changed -> focus_x_axis_change -> apply_x_axis_change -> update_xs -> reset_x_display -> resize_sx -> set_scrollbar ->
   *    sx_valuechanged_callback -> apply_x_axis_change -> update_xs -> reset_x_display -> resize_sx_and_zx -> resize_sx
   *    and we're looping!  This loop happens now because the gtk 3 changes (hiding the adjustment struct) mean that
   *    we're using gtk_adjustment_set_value which triggers the valuechanged callback, whereas earlier we were
   *    simply setting the field, and not triggering the callback.  Good Grief!
   *
   * I added the guards, but now the sliders seem glitchy -- certainly not smooth!
   */
}


static void gzy_valuechanged_callback(GtkAdjustment *adj, gpointer context)
{
  chan_info *cp;
  cp = (chan_info *)get_user_data(G_OBJECT(adj));
  cp->gzy = ADJUSTMENT_VALUE(adj);
  if (cp->active == CHANNEL_HAS_AXES)
    {
      ADJUSTMENT_SET_PAGE_SIZE(gsy_adj(cp), 1.0 - ADJUSTMENT_VALUE(adj)); 
      if (cp->gsy > cp->gzy)
	{
	  cp->gsy = ADJUSTMENT_VALUE(adj);
	  ADJUSTMENT_SET_VALUE(gsy_adj(cp), cp->gsy);
	}
      else gtk_adjustment_changed(GTK_ADJUSTMENT(gsy_adj(cp)));
      for_each_sound_chan(cp->sound, update_graph_or_warn);
    }
}


static void gsy_valuechanged_callback(GtkAdjustment *adj, gpointer context)
{
  chan_info *cp;
  cp = (chan_info *)get_user_data(G_OBJECT(adj));
  cp->gsy = ADJUSTMENT_VALUE(adj);
  if (cp->active == CHANNEL_HAS_AXES)
    for_each_sound_chan(cp->sound, update_graph_or_warn);
}


static int last_f_state = 0;

static gboolean f_toggle_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{ 
  last_f_state = EVENT_STATE(ev);
  return(false);
}


static void f_toggle_click_callback(GtkWidget *w, gpointer data)
{
  f_button_callback((chan_info *)data, 
		    TOGGLE_BUTTON_ACTIVE(w), 
		    (last_f_state & snd_ControlMask));
}


static int last_w_state = 0;

static gboolean w_toggle_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{ 
  last_w_state = EVENT_STATE(ev);
  return(false);
}


static void w_toggle_click_callback(GtkWidget *w, gpointer data)
{
  w_button_callback((chan_info *)data, 
		    TOGGLE_BUTTON_ACTIVE(w), 
		    (last_w_state & snd_ControlMask));
}


#define MIN_REGRAPH_X 12
#define MIN_REGRAPH_Y 7

static gboolean channel_expose_callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  chan_info *cp;
  snd_info *sp;
  cp = (chan_info *)data;
  if ((cp == NULL) || (cp->active < CHANNEL_HAS_AXES) || (cp->sound == NULL)) return(false);

#if (!HAVE_GTK_3)
  if ((EVENT_AREA_HEIGHT(ev) < MIN_REGRAPH_Y) || 
      (EVENT_AREA_WIDTH(ev) < MIN_REGRAPH_X)) 
    return(false);
  /* these are 0 in gtk 3 */
#endif

  sp = cp->sound;
  if (sp->channel_style != CHANNELS_SEPARATE)
    for_each_sound_chan(sp, update_graph_or_warn);
  else update_graph_or_warn(cp);
  return(false);
}


static gboolean channel_resize_callback(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  channel_resize((chan_info *)data);
  return(false);
}


static XEN mouse_enter_graph_hook;
static XEN mouse_leave_graph_hook;

static gboolean graph_mouse_enter(GtkWidget *w, GdkEventCrossing *ev, gpointer data)
{
  /* how many args does this thing take?  does it return an int?  what does the return value mean? */

  if (with_pointer_focus(ss))
    goto_window(w);

  if (XEN_HOOKED(mouse_enter_graph_hook))
    {
      int pdata;
      pdata = get_user_int_data(G_OBJECT(w));
      run_hook(mouse_enter_graph_hook,
	       XEN_LIST_2(C_INT_TO_XEN_SOUND(UNPACK_SOUND(pdata)),
			  C_TO_XEN_INT(UNPACK_CHANNEL(pdata))),
	       S_mouse_enter_graph_hook);
    }

  gdk_window_set_cursor(WIDGET_TO_WINDOW(w), ss->graph_cursor);
  return(false);
}


static gboolean graph_mouse_leave(GtkWidget *w, GdkEventCrossing *ev, gpointer data)
{
  if (XEN_HOOKED(mouse_leave_graph_hook))
    {
      int pdata;
      pdata = get_user_int_data(G_OBJECT(w));
      run_hook(mouse_leave_graph_hook,
	       XEN_LIST_2(C_INT_TO_XEN_SOUND(UNPACK_SOUND(pdata)),
			  C_TO_XEN_INT(UNPACK_CHANNEL(pdata))),
	       S_mouse_leave_graph_hook);
    }

  gdk_window_set_cursor(WIDGET_TO_WINDOW(w), ss->arrow_cursor);
  return(false);
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

static void history_select_callback(const char *name, int row, void *data)
{
  edit_history_select((chan_info *)data, row);
}


static void remake_edit_history(chan_info *cp)
{
  snd_info *sp;
  int i, eds;
  slist *lst;
  if ((!cp) || (cp->active < CHANNEL_HAS_AXES)) return;
  if (cp->squelch_update) return;
  lst = EDIT_HISTORY_LIST(cp);
  if (!lst) return;
  slist_clear(lst);
  sp = cp->sound;
  if (sp->channel_style != CHANNELS_SEPARATE)
    {
      int k, ed, filelen;
      char *title;
      chan_info *ncp;
      filelen = 16 + strlen(sp->filename);
      title = (char *)calloc(filelen, sizeof(char));
      for (k = 0, ed = 0; k < sp->nchans; k++)
	{
	  ncp = sp->chans[k];
	  if ((ncp) && (ncp->sound))
	    {
	      ncp->edhist_base = ed++;
	      snprintf(title, filelen, "chan %d: %s", k + 1, sp->filename);
	      slist_append(lst, title);
	      eds = ncp->edit_ctr;
	      while ((eds < (ncp->edit_size - 1)) && (ncp->edits[eds + 1])) eds++;
	      for (i = 1; i <= eds; i++, ed++)
		{
		  char *str;
		  str = edit_to_string(ncp, i);
		  slist_append(lst, str);
		  free(str);
		}
	      if (k < sp->nchans - 1)
		{
		  slist_append(lst, "______________________________"); 
		  ed++; 
		} 
	    }
	}
      if (sp->selected_channel == NO_SELECTION)
	slist_select(lst, cp->edhist_base + cp->edit_ctr);
      else slist_select(lst, sp->chans[sp->selected_channel]->edhist_base + sp->chans[sp->selected_channel]->edit_ctr);
      free(title);
    }
  else
    {
      eds = cp->edit_ctr;
      while ((eds < (cp->edit_size - 1)) && (cp->edits[eds + 1])) eds++;
      if (eds >= 0)
	{
	  slist_append(lst, sp->filename);
	  for (i = 1; i <= eds; i++) 
	    {
	      char *str;
	      str = edit_to_string(cp, i);
	      slist_append(lst, str);
	      free(str);
	    }
	}
      slist_select(lst, cp->edit_ctr);
    }
  goto_graph(cp);
}


void reflect_edit_history_change(chan_info *cp)
{
  /* new edit so it is added, and any trailing lines removed */
  snd_info *sp;
  if (cp->squelch_update) return;
  if (cp->in_as_one_edit > 0) return;
  sp = cp->sound;
  if ((cp->chan > 0) && (sp->channel_style != CHANNELS_SEPARATE))
    {
      chan_info *ncp;
      ncp = sp->chans[0];
      if ((ncp) && (ncp->sound))
	remake_edit_history(ncp);
    }
  else remake_edit_history(cp);
}


void reflect_edit_counter_change(chan_info *cp)
{
  /* undo/redo/revert -- change which line is highlighted */
  snd_info *sp;
  if (cp->squelch_update) return;
  sp = cp->sound;
  if ((cp->chan > 0) && (sp->channel_style != CHANNELS_SEPARATE))
    {
      chan_info *ncp;
      ncp = sp->chans[0];
      slist_select(EDIT_HISTORY_LIST(ncp), cp->edit_ctr + cp->edhist_base);
    }
  else
    {
      slist_select(EDIT_HISTORY_LIST(cp), cp->edit_ctr);
      slist_moveto(EDIT_HISTORY_LIST(cp), cp->edit_ctr);
      goto_graph(cp);
    }
}


/* for combined cases, the incoming chan_info pointer is always chan[0], 
 * but the actual channel depends on placement if mouse oriented.
 * virtual_selected_channel(cp) (snd-chn.c) retains the current selected channel
 */

static gboolean real_graph_key_press(GtkWidget *w, GdkEventKey *ev, gpointer data)
{
  chan_info *cp = (chan_info *)data;
  int keysym;
  bool theirs;
  int x, y;
  GdkModifierType key_state;

  gdk_window_get_pointer(EVENT_WINDOW(ev), &x, &y, &key_state);
  key_state = (GdkModifierType)(EVENT_STATE(ev));
  keysym = EVENT_KEYVAL(ev);
  theirs = key_press_callback(cp, x, y, EVENT_STATE(ev), keysym);
  if (theirs) ss->graph_is_active = false;
  g_signal_stop_emission((gpointer)w, g_signal_lookup("key_press_event", G_OBJECT_TYPE((gpointer)w)), 0);

  return(true);
}


gboolean graph_key_press(GtkWidget *w, GdkEventKey *ev, gpointer data)
{
  chan_info *cp = (chan_info *)data;
  int keysym;
  bool theirs;
  int x, y;
  GdkModifierType key_state;

  gdk_window_get_pointer(EVENT_WINDOW(ev), &x, &y, &key_state);
  key_state = (GdkModifierType)(EVENT_STATE(ev));
  keysym = EVENT_KEYVAL(ev);
  theirs = key_press_callback(cp, x, y, EVENT_STATE(ev), keysym);
  if (theirs) ss->graph_is_active = true;

  return(true);
}
 

static gboolean graph_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  chan_info *cp = (chan_info *)data;
  ss->graph_is_active = true;
  gtk_widget_grab_focus(w);
  if (cp->sound)
    cp->sound->mini_active = false;
  graph_button_press_callback(cp, (void *)ev, (int)(EVENT_X(ev)), (int)(EVENT_Y(ev)), EVENT_STATE(ev), EVENT_BUTTON(ev), EVENT_TIME(ev));
  return(false);
}


static gboolean graph_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  graph_button_release_callback((chan_info *)data, (int)(EVENT_X(ev)), (int)(EVENT_Y(ev)), EVENT_STATE(ev), EVENT_BUTTON(ev));
  return(false);
}


static gboolean graph_button_motion(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{ 
  int x, y;
  GdkModifierType state;

  if (EVENT_IS_HINT(ev))
    gdk_window_get_pointer(EVENT_WINDOW(ev), &x, &y, &state);
  else
    {
      x = (int)(EVENT_X(ev));
      y = (int)(EVENT_Y(ev));
    }

  if (BUTTON1_PRESSED(EVENT_STATE(ev)))
    graph_button_motion_callback((chan_info *)data, x, y, EVENT_TIME(ev));
  else check_cursor_shape((chan_info *)data, x, y);

  return(false);
}


static void channel_drop_watcher(GtkWidget *w, const char *filename, int x, int y, void *data)
{
  drag_and_drop_mix_at_x_y(get_user_int_data(G_OBJECT(w)), filename, x, y);
}


static void channel_drag_watcher(GtkWidget *w, const char *filename, int x, int y, drag_style_t dtype, void *context)
{
  int snd, chn, data;
  snd_info *sp;
  chan_info *cp;
  float seconds;

  data = get_user_int_data(G_OBJECT(w));
  chn = UNPACK_CHANNEL(data);
  snd = UNPACK_SOUND(data);
  sp = ss->sounds[snd];

  if (snd_ok(sp))
    {
      switch (dtype)
	{
	case DRAG_ENTER:
	case DRAG_MOTION:
	  cp = sp->chans[chn];
	  if ((sp->nchans > 1) && (sp->channel_style == CHANNELS_COMBINED))
	    cp = which_channel(sp, y);    
	  seconds = (float)(ungrf_x(cp->axis, x));
	  if (seconds < 0.0) seconds = 0.0;
	  if (sp->nchans > 1)
	    report_in_minibuffer(sp, "drop to mix file in chan %d at %.4f", cp->chan + 1, seconds);
	  else report_in_minibuffer(sp, "drop to mix file at %.4f", seconds);
	  break;

	case DRAG_LEAVE:
	  string_to_minibuffer(sp, " "); /* not clear_minibuffer here! => segfault */
	  break;
	}
    }
}


int add_channel_window(snd_info *sp, int channel, int chan_y, int insertion, GtkWidget *main, fw_button_t button_style, bool with_events)
{
  GtkWidget **cw;
  GtkAdjustment **adjs;
  chan_info *cp;
  graphics_context *cax;
  bool make_widgets, need_extra_scrollbars;

  make_widgets = ((sp->chans[channel]) == NULL);
  sp->chans[channel] = make_chan_info(sp->chans[channel], channel, sp);
  cp = sp->chans[channel];

  if (cp->chan_widgets == NULL) 
    {
      cw = (GtkWidget **)calloc(NUM_CHAN_WIDGETS, sizeof(GtkWidget *));
      adjs = (GtkAdjustment **)calloc(NUM_CHAN_ADJS, sizeof(GtkAdjustment *));
      cp->chan_widgets = cw;
      cp->chan_adjs = adjs;
    }
  else
    {
      cw = cp->chan_widgets;
      adjs = cp->chan_adjs;
    }

  need_extra_scrollbars = ((!main) && (channel == 0));
  if (make_widgets)
    {
      if (!main)
	{
	  cw[W_main_window] = gtk_hpaned_new();
	  gtk_container_set_border_width(GTK_CONTAINER(cw[W_main_window]), 2);
	  gtk_box_pack_start(GTK_BOX(w_snd_pane_box(sp)), cw[W_main_window], true, true, 0);
	  cp->edhist_list = slist_new(cw[W_main_window], NULL, 0, PANED_ADD1);
#if HAVE_GTK_3
	  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(cp->edhist_list->scroller), GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
#else
	  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(cp->edhist_list->scroller), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
#endif
	  cp->edhist_list->select_callback = history_select_callback;
	  cp->edhist_list->select_callback_data = (void *)cp;
	}
      else cw[W_main_window] = main;

      cw[W_graph_window] = gtk_table_new(3, 5, false);
      if ((GTK_IS_VPANED(cw[W_main_window])) || (GTK_IS_HPANED(cw[W_main_window])))
	gtk_paned_add2(GTK_PANED(cw[W_main_window]), cw[W_graph_window]);
      else
	{
	  if ((GTK_IS_VBOX(cw[W_main_window])) || (GTK_IS_HBOX(cw[W_main_window])))
	    gtk_box_pack_start(GTK_BOX(cw[W_main_window]), cw[W_graph_window], true, true, 4);
	  else gtk_container_add(GTK_CONTAINER(cw[W_main_window]), cw[W_graph_window]);
	}
      gtk_widget_set_size_request(cw[W_graph_window], -1, chan_y);

      cw[W_graph] = gtk_drawing_area_new();
      add_drag_and_drop(cw[W_graph], channel_drop_watcher, channel_drag_watcher, NULL);
      set_user_int_data(G_OBJECT(cw[W_graph]), PACK_SOUND_AND_CHANNEL(sp->index, cp->chan));
      gtk_widget_set_events(cw[W_graph], GDK_ALL_EVENTS_MASK);
      SET_CAN_FOCUS(cw[W_graph]);
      gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_graph], 2, 3, 0, 2, 
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND), 
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND | GTK_SHRINK), 
		       0, 0);
      gtk_widget_show(cw[W_graph]);
      if (with_events)
	{
	  SG_SIGNAL_CONNECT(cw[W_graph], DRAW_SIGNAL, channel_expose_callback, cp);
	  SG_SIGNAL_CONNECT(cw[W_graph], "configure_event", channel_resize_callback, cp);
	}
      SG_SIGNAL_CONNECT(cw[W_graph], "button_press_event", graph_button_press, cp);
      SG_SIGNAL_CONNECT(cw[W_graph], "button_release_event", graph_button_release, cp);
      SG_SIGNAL_CONNECT(cw[W_graph], "motion_notify_event", graph_button_motion, cp);
      if (main == NULL)
	{
	  SG_SIGNAL_CONNECT(cw[W_graph], "enter_notify_event", graph_mouse_enter, NULL);
	  SG_SIGNAL_CONNECT(cw[W_graph], "leave_notify_event", graph_mouse_leave, NULL);
	  SG_SIGNAL_CONNECT(cw[W_graph], "key_press_event", real_graph_key_press, cp);
	}

      cw[W_bottom_scrollers] = gtk_vbox_new(true, 0);
      gtk_box_set_spacing(GTK_BOX(cw[W_bottom_scrollers]), 0);
      gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_bottom_scrollers], 2, 3, 2, 3, 
		       (GtkAttachOptions)(GTK_EXPAND | GTK_FILL), 
		       (GtkAttachOptions)(GTK_FILL), 
		       0, 0);
      gtk_widget_show(cw[W_bottom_scrollers]);

      adjs[W_sx_adj] = (GtkAdjustment *)gtk_adjustment_new(0.0, 0.0, 1.00, 0.001, 0.01, .01);
      cw[W_sx] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_sx_adj]));
      gtk_box_pack_start(GTK_BOX(cw[W_bottom_scrollers]), cw[W_sx], true, true, 0);
      set_user_data(G_OBJECT(adjs[W_sx_adj]), (gpointer)cp);
      SG_SIGNAL_CONNECT(adjs[W_sx_adj], "value_changed", sx_valuechanged_callback, cp);
#if HAVE_GTK_3
      widget_modify_bg(cw[W_sx], GTK_STATE_NORMAL, ss->position_color);
#endif
      gtk_widget_show(cw[W_sx]);
      gtk_widget_set_name(cw[W_sx], "sx_slider");

      adjs[W_zx_adj] = (GtkAdjustment *)gtk_adjustment_new(0.0, 0.0, 1.1, 0.001, 0.01, .1);
      cw[W_zx] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_zx_adj]));
      gtk_box_pack_start(GTK_BOX(cw[W_bottom_scrollers]), cw[W_zx], true, true, 0);
      set_user_data(G_OBJECT(adjs[W_zx_adj]), (gpointer)cp);
      SG_SIGNAL_CONNECT(adjs[W_zx_adj], "value_changed", zx_valuechanged_callback, cp);
#if HAVE_GTK_3
      widget_modify_bg(cw[W_zx], GTK_STATE_NORMAL, ss->zoom_color);
#endif
      gtk_widget_set_name(cw[W_zx], "zx_slider");
      gtk_widget_show(cw[W_zx]);


      cw[W_wf_buttons] = gtk_vbox_new(true, 0);
      gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_wf_buttons], 0, 2, 1, 3, GTK_SHRINK, GTK_SHRINK, 0, 0);
      gtk_widget_show(cw[W_wf_buttons]);
      
      if (button_style == WITH_FW_BUTTONS)
	{
	  cw[W_f] = gtk_check_button_new_with_label("f");
	  gtk_box_pack_start(GTK_BOX(cw[W_wf_buttons]), cw[W_f], true, true, 0);
	  gtk_widget_show(cw[W_f]);
	  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(cw[W_f]), false);
	  SG_SIGNAL_CONNECT(cw[W_f], "button_press_event", f_toggle_callback, cp);
	  SG_SIGNAL_CONNECT(cw[W_f], "toggled", f_toggle_click_callback, cp);
  
	  cw[W_w] = gtk_check_button_new_with_label("w");
	  gtk_box_pack_start(GTK_BOX(cw[W_wf_buttons]), cw[W_w], true, true, 0);
	  gtk_widget_show(cw[W_w]);
	  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(cw[W_w]), true);
	  SG_SIGNAL_CONNECT(cw[W_w], "button_press_event", w_toggle_callback, cp);
	  SG_SIGNAL_CONNECT(cw[W_w], "toggled", w_toggle_click_callback, cp);
	}
      else
	{
	  cw[W_up_ev] = gtk_event_box_new();
	  gtk_box_pack_start(GTK_BOX(cw[W_wf_buttons]), cw[W_up_ev], true, true, 0);
	  gtk_widget_show(cw[W_up_ev]);

	  cw[W_f] = gtk_arrow_new(GTK_ARROW_UP, GTK_SHADOW_ETCHED_OUT);
	  gtk_container_add(GTK_CONTAINER(cw[W_up_ev]), cw[W_f]);
	  gtk_widget_show(cw[W_f]);

	  cw[W_down_ev] = gtk_event_box_new();
	  gtk_box_pack_start(GTK_BOX(cw[W_wf_buttons]), cw[W_down_ev], true, true, 0);
	  gtk_widget_show(cw[W_down_ev]);

	  cw[W_w] = gtk_arrow_new(GTK_ARROW_DOWN, GTK_SHADOW_ETCHED_OUT);
	  gtk_container_add(GTK_CONTAINER(cw[W_down_ev]), cw[W_w]);
	  gtk_widget_show(cw[W_w]);
	}

      adjs[W_zy_adj] = (GtkAdjustment *)gtk_adjustment_new(0.0, 0.0, 1.1, 0.001, 0.01, .1);   /* 0 -> 1 (upside down) */
      cw[W_zy] = gtk_vscrollbar_new(GTK_ADJUSTMENT(adjs[W_zy_adj]));
      gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_zy], 0, 1, 0, 1, 
		       (GtkAttachOptions)(GTK_FILL), 
		       (GtkAttachOptions)(GTK_EXPAND | GTK_FILL | GTK_SHRINK), 
		       2, 0);
      set_user_data(G_OBJECT(adjs[W_zy_adj]), (gpointer)cp);
      SG_SIGNAL_CONNECT(adjs[W_zy_adj], "value_changed", zy_valuechanged_callback, cp);
#if HAVE_GTK_3
      widget_modify_bg(cw[W_zy], GTK_STATE_NORMAL, ss->zoom_color);
#endif
      gtk_widget_show(cw[W_zy]);
      gtk_widget_set_name(cw[W_zy], "zy_slider");

      adjs[W_sy_adj] = (GtkAdjustment *)gtk_adjustment_new(0.5, 0.0, 1.01, 0.001, 0.01, .01);
      cw[W_sy] = gtk_vscrollbar_new(GTK_ADJUSTMENT(adjs[W_sy_adj]));
      gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_sy], 1, 2, 0, 1, 
		       (GtkAttachOptions)(GTK_FILL), 
		       (GtkAttachOptions)(GTK_EXPAND | GTK_FILL | GTK_SHRINK), 
		       2, 0);
      set_user_data(G_OBJECT(adjs[W_sy_adj]), (gpointer)cp);
      SG_SIGNAL_CONNECT(adjs[W_sy_adj], "value_changed", sy_valuechanged_callback, cp);
#if HAVE_GTK_3
      widget_modify_bg(cw[W_sy], GTK_STATE_NORMAL, ss->position_color);
#endif
      gtk_widget_show(cw[W_sy]);
      gtk_widget_set_name(cw[W_sy], "sy_slider");

      if (need_extra_scrollbars)
	{
	  adjs[W_gsy_adj] = (GtkAdjustment *)gtk_adjustment_new(0.0, 0.0, 1.0, 0.001, 0.01, .01);
	  cw[W_gsy] = gtk_vscrollbar_new(GTK_ADJUSTMENT(adjs[W_gsy_adj]));
	  gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_gsy], 3, 4, 0, 2, 
			   (GtkAttachOptions)(GTK_FILL), 
			   (GtkAttachOptions)(GTK_EXPAND | GTK_FILL | GTK_SHRINK), 
			   0, 0);
	  set_user_data(G_OBJECT(adjs[W_gsy_adj]), (gpointer)cp);
	  SG_SIGNAL_CONNECT(adjs[W_gsy_adj], "value_changed", gsy_valuechanged_callback, cp);
	  gtk_widget_show(cw[W_gsy]);
	  gtk_widget_set_name(cw[W_gsy], "gsy_slider");

	  adjs[W_gzy_adj] = (GtkAdjustment *)gtk_adjustment_new(1.0, 0.0, 1.00, 0.001, 0.01, .01);
	  cw[W_gzy] = gtk_vscrollbar_new(GTK_ADJUSTMENT(adjs[W_gzy_adj]));
	  gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_gzy], 4, 5, 0, 2, 
			   (GtkAttachOptions)(GTK_FILL), 
			   (GtkAttachOptions)(GTK_EXPAND | GTK_FILL | GTK_SHRINK), 
			   0, 0);
	  set_user_data(G_OBJECT(adjs[W_gzy_adj]), (gpointer)cp);
	  SG_SIGNAL_CONNECT(adjs[W_gzy_adj], "value_changed", gzy_valuechanged_callback, cp);
	  gtk_widget_show(cw[W_gzy]);
	  gtk_widget_set_name(cw[W_gzy], "gzy_slider");
	  
	  gtk_widget_hide(cw[W_gsy]);
	  gtk_widget_hide(cw[W_gzy]);
	}
      else
	{
	  cw[W_gsy] = NULL;
	  cw[W_gzy] = NULL;
	}

      if ((GTK_IS_VPANED(cw[W_main_window])) || (GTK_IS_HPANED(cw[W_main_window])))
	gtk_paned_set_position(GTK_PANED(cw[W_main_window]), EDIT_HISTORY_CLOSED);  
      gtk_widget_show(cw[W_graph_window]);

    }
  else recolor_graph(cp, false); /* in case selection color left over from previous use */
  if ((sp->channel_style != CHANNELS_COMBINED) || (channel == 0))
    gtk_widget_show_all(cw[W_main_window]);

  if ((need_extra_scrollbars) && (sp->channel_style != CHANNELS_COMBINED))
    hide_gz_scrollbars(sp); /* default is on in this case */  

  reflect_edit_history_change(cp);

  cax = cp->ax;
  cax->gc = ss->basic_gc;
  /* cax->wn has to wait until update_graph */

  {
    GtkWidget *w; 
    w = channel_to_widget(cp);
    cax->wn = WIDGET_TO_WINDOW(w);
    cax->w = w;
  }
  return(0);
}


static void set_graph_font(chan_info *cp, graphics_context *ax, PangoFontDescription *fnt)
{
  ax->current_font = fnt;
}


void set_peak_numbers_font(chan_info *cp, graphics_context *ax) {set_graph_font(cp, ax, PEAKS_FONT(ss));}

void set_tiny_numbers_font(chan_info *cp, graphics_context *ax) {set_graph_font(cp, ax, TINY_FONT(ss));}

void set_bold_peak_numbers_font(chan_info *cp, graphics_context *ax) {set_graph_font(cp, ax, BOLD_PEAKS_FONT(ss));}


color_t get_foreground_color(graphics_context *ax)
{
  return(ax->gc->fg_color);
}


void set_foreground_color(graphics_context *ax, color_info *color)
{
  gc_set_foreground(ax->gc, color);
}


gc_t *copy_GC(chan_info *cp)
{
  if (cp->selected) return(ss->selected_basic_gc);
  return(ss->basic_gc);
}


gc_t *erase_GC(chan_info *cp)
{
  snd_info *sp;
  sp = cp->sound;
  if ((cp->selected) ||
      ((sp) && (sp->channel_style == CHANNELS_SUPERIMPOSED) && (sp->index == ss->selected_sound)))
    return(ss->selected_erase_gc);
  return(ss->erase_gc);
}


void cleanup_cw(chan_info *cp)
{
  if (cp)
    {
      GtkWidget **cw;
      cp->progress_pct = -1.0;

      if (EDIT_HISTORY_LIST(cp)) 
	{
	  slist_clear(EDIT_HISTORY_LIST(cp));
	  gtk_paned_set_position(GTK_PANED(cp->chan_widgets[W_main_window]), EDIT_HISTORY_CLOSED);
	}

      cp->selected = false;
      cw = cp->chan_widgets;

      if (cw)
	{
	  if (cw[W_w])
	    {
	      set_toggle_button(cw[W_w], true, false, (void *)cp);
	      set_toggle_button(cw[W_f], false, false, (void *)cp);
	    }
	  gtk_widget_hide(channel_main_pane(cp));
	}
    }
}


void change_channel_style(snd_info *sp, channel_style_t new_style)
{
  if ((sp) && 
      (sp->nchans > 1))
    {
      channel_style_t old_style;
      chan_info *selected_cp;

      selected_cp = any_selected_channel(sp); /* chan 0 is none is selected */
      old_style = sp->channel_style;
      sp->channel_style = new_style;

      if (new_style != old_style)
	{
	  int i;
	  int height[1];
	  if ((new_style == CHANNELS_SEPARATE) || (old_style == CHANNELS_SEPARATE))
	    remake_edit_history(sp->chans[0]);

	  if (old_style == CHANNELS_COMBINED)
	    {
	      hide_gz_scrollbars(sp);
	      for (i = 1; i < sp->nchans; i++) 
		channel_set_mix_tags_erased(sp->chans[i]);
	    }
	  else 
	    {
	      if (new_style == CHANNELS_COMBINED)
		{
		  show_gz_scrollbars(sp);
		  for (i = 1; i < sp->nchans; i++) 
		    channel_set_mix_tags_erased(sp->chans[i]);
		}
	    }

	  if (old_style == CHANNELS_SUPERIMPOSED)
	    {
	      syncb(sp, sp->previous_sync);
	      /* set to blue? */
	    }
	  else
	    {
	      if (new_style == CHANNELS_SUPERIMPOSED)
		{
		  sp->previous_sync = sp->sync;
		  if (sp->sync == 0) syncb(sp, 1);

		  apply_y_axis_change(selected_cp);
		  apply_x_axis_change(selected_cp);

		  for (i = 0; i < sp->nchans; i++) 
		    {
		      if (i != selected_cp->chan)
			{
			  chan_info *ncp;
			  ncp = sp->chans[i];
			  CURSOR(ncp) = CURSOR(selected_cp);
			  if (selected_cp->graph_transform_p != ncp->graph_transform_p)
			    {
			      ncp->graph_transform_p = selected_cp->graph_transform_p;
			      set_toggle_button(channel_f(ncp), selected_cp->graph_transform_p, false, (void *)ncp);
			    }
			  if (selected_cp->graph_time_p != ncp->graph_time_p)
			    {
			      ncp->graph_time_p = selected_cp->graph_time_p;
			      set_toggle_button(channel_w(ncp), selected_cp->graph_time_p, false, (void *)ncp);
			    }
			}
		    }
		}
	    }

	  height[0] = widget_height(w_snd_pane(sp)) - control_panel_height(sp);
	  if (old_style == CHANNELS_SEPARATE)
	    {
	      axis_info *ap;
	      ap = selected_cp->axis;

	      for (i = 0; i < sp->nchans; i++) 
		{
		  if (i != selected_cp->chan)
		    set_axes(sp->chans[i], ap->x0, ap->x1, ap->y0, ap->y1);

		  if (i > 0)
		    {
		      chan_info *ncp;
		      ncp = sp->chans[i];
		      cleanup_cw(ncp);
		      ncp->ax->w = channel_to_widget(ncp);
		      ncp->ax->wn = WIDGET_TO_WINDOW(ncp->ax->w);
		    }
		}
	      channel_open_pane(sp->chans[0]);
	      set_toggle_button(unite_button(sp), true, false, (void *)sp);
	    }
	  else
	    {
	      if (new_style == CHANNELS_SEPARATE)
		{
		  GtkWidget **cw;
		  /* height[0] = total space available */
		  height[0] /= sp->nchans;

		  for_each_sound_chan(sp, channel_open_pane);
		  /* for (i = 0; i < sp->nchans; i++) reset_mix_graph_parent(sp->chans[i]); */

		  for (i = 1; i < sp->nchans; i++)
		    {
		      chan_info *cp;
		      cp = sp->chans[i];
		      cp->ax->w = channel_to_widget(cp);
		      cp->ax->wn = WIDGET_TO_WINDOW(cp->ax->w);
		      cw = cp->chan_widgets;
		      gtk_widget_show_all(cw[W_main_window]);
		    }
		  set_toggle_button(unite_button(sp), false, false, (void *)sp);
		  if (sp->selected_channel > 0) color_selected_channel(sp);
		}
	    }

	  if ((new_style == CHANNELS_COMBINED) && 
	      (sp->selected_channel > 0)) 
	    color_selected_channel(sp);
	}
    }
}



static XEN g_channel_widgets(XEN snd, XEN chn)
{
  #define H_channel_widgets "(" S_channel_widgets " :optional snd chn): a list of widgets: ((0)graph (1)w (2)f (3)sx (4)sy (5)zx (6)zy (7)\
edhist (8)gsy (9)gzy (10)main (11)sx_adj (12)sy_adj (13)zx_adj (14)zy_adj (15)gsy_adj (16)gzy_adj"

  #define XEN_WRAP_ADJ(Value) ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("GtkAdjustment_"), XEN_WRAP_C_POINTER(Value)) : XEN_FALSE)

  chan_info *cp;
  ASSERT_CHANNEL(S_channel_widgets, snd, chn, 1);
  cp = get_cp(snd, chn, S_channel_widgets);
  if (!cp) return(XEN_FALSE);
  return(XEN_CONS(XEN_WRAP_WIDGET(channel_graph(cp)),
	  XEN_CONS(XEN_WRAP_WIDGET(channel_w(cp)),
	   XEN_CONS(XEN_WRAP_WIDGET(channel_f(cp)),
	    XEN_CONS(XEN_WRAP_WIDGET(channel_sx(cp)),
	     XEN_CONS(XEN_WRAP_WIDGET(channel_sy(cp)),
	      XEN_CONS(XEN_WRAP_WIDGET(channel_zx(cp)),
	       XEN_CONS(XEN_WRAP_WIDGET(channel_zy(cp)),
		XEN_CONS((EDIT_HISTORY_LIST(cp)) ? XEN_WRAP_WIDGET(EDIT_HISTORY_LIST(cp)->topics) : XEN_FALSE,
		 XEN_CONS(XEN_WRAP_WIDGET(channel_gsy(cp)),
		  XEN_CONS(XEN_WRAP_WIDGET(channel_gzy(cp)),
		   XEN_CONS(XEN_WRAP_WIDGET(channel_main_pane(cp)),
		    XEN_CONS(XEN_WRAP_ADJ(sx_adj(cp)),
		     XEN_CONS(XEN_WRAP_ADJ(sy_adj(cp)),
		      XEN_CONS(XEN_WRAP_ADJ(zx_adj(cp)),
		       XEN_CONS(XEN_WRAP_ADJ(zy_adj(cp)),
		        XEN_CONS(XEN_WRAP_ADJ(gsy_adj(cp)),
			 XEN_CONS(XEN_WRAP_ADJ(gzy_adj(cp)),
                          XEN_EMPTY_LIST))))))))))))))))));
}


/* previous snd-gxen.c contents */

static gint timed_eval(gpointer in_code)
{
#if HAVE_EXTENSION_LANGUAGE
  /* #if needed on 64-bit machines */
  XEN lst = (XEN)in_code;
  XEN_CALL_0(XEN_CADR(lst), "timed callback func");
  snd_unprotect_at(XEN_TO_C_INT(XEN_CAR(lst)));
#endif
  return(0);
}


static XEN g_in(XEN ms, XEN code)
{
  #define H_in "(" S_in " msecs thunk): invoke thunk in msecs milliseconds (named call_in in Ruby)"

#if HAVE_EXTENSION_LANGUAGE
  XEN_ASSERT_TYPE(XEN_NUMBER_P(ms), ms, XEN_ARG_1, S_in, "a number");
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(code), code, XEN_ARG_2, S_in, "a procedure");
  if (XEN_REQUIRED_ARGS_OK(code, 0))
    {
      int secs;
      secs = XEN_TO_C_INT(ms);
      if (secs < 0) 
	XEN_OUT_OF_RANGE_ERROR(S_in, XEN_ARG_1, ms, "a positive integer");
      else
	{
	  XEN lst;
	  lst = XEN_LIST_2(XEN_FALSE, code);
	  XEN_LIST_SET(lst, 0, C_TO_XEN_INT(snd_protect(lst)));
	  g_timeout_add_full(0, (guint32)secs, timed_eval, (gpointer)lst, NULL);
	}
    }
  else XEN_BAD_ARITY_ERROR(S_in, 2, code, "should take no args");
#endif

  return(ms);
}


void color_unselected_graphs(color_t color)
{
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      int j;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse != SOUND_WRAPPER))
	for (j = 0; j < sp->allocated_chans; j++)
	  {
	    chan_info *cp;
	    cp = sp->chans[j];
	    update_graph(cp);
	  }
    }
}


void color_chan_components(color_t color, slider_choice_t which_component)
{
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      int j;
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse != SOUND_WRAPPER))
	for (j = 0; j < sp->allocated_chans; j++)
	  {
	    chan_info *cp;
	    cp = sp->chans[j];
	    if (cp)
	      {
		if (which_component == COLOR_POSITION)
		  {
		    /* in gtk3 this sets the slider color when dragged, and it looks bad
		     * in gtk2 it sets the background (the trough, not the slider)
		     */
#if (!HAVE_GTK_3)
		    widget_modify_bg(channel_sx(cp), GTK_STATE_ACTIVE, color);
		    widget_modify_bg(channel_sy(cp), GTK_STATE_ACTIVE, color);
#else
		    /* (gtk_widget_override_background_color ((channel-widgets) 3) GTK_STATE_NORMAL (GDK_RGBA (make-color 1 0 0))) ; sets entire scroller
		     *    the scrollbar is neither a bin nor a container, so how to get at the slider?
		    */
		    /* this sets both the trough and the slider color -- not really what I'd like */
		    widget_modify_bg(channel_sx(cp), GTK_STATE_NORMAL, color);  
		    widget_modify_bg(channel_sy(cp), GTK_STATE_NORMAL, color);  
		    /* TODO: gzx etc? also the slider should be a different color
		     *       this might work if we used scales rather than sliders
		     */
#endif
		  }
		else
		  {
#if (!HAVE_GTK_3)
		    widget_modify_bg(channel_zx(cp), GTK_STATE_ACTIVE, color);
		    widget_modify_bg(channel_zy(cp), GTK_STATE_ACTIVE, color);
#else
		    widget_modify_bg(channel_zx(cp), GTK_STATE_NORMAL, color);  
		    widget_modify_bg(channel_zy(cp), GTK_STATE_NORMAL, color);  
#endif
		  }
	      }
	  }
    }
}


static XEN g_graph_cursor(void)
{
  #define H_graph_cursor "(" S_graph_cursor "): current graph cursor shape"
  return(C_TO_XEN_INT(in_graph_cursor(ss)));
}


static XEN g_set_graph_cursor(XEN curs)
{
  int val;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(curs), curs, XEN_ONLY_ARG, S_setB S_graph_cursor, "an integer");
  val = XEN_TO_C_INT(curs);
  if ((val >= 0) && ((val & 1) == 0) && (val <= GDK_XTERM)) /* these are even numbers up to about 152 (gdkcursor.h) */
    {
      ss->Graph_Cursor = val;
      ss->graph_cursor = gdk_cursor_new((GdkCursorType)in_graph_cursor(ss));
      /* the gtk examples ignore g_object_ref|unref in this regard, so I will also */
    }
  else XEN_OUT_OF_RANGE_ERROR(S_setB S_graph_cursor, 1, curs, "~A: invalid cursor");
  return(curs);
}


#ifdef XEN_ARGIFY_1
XEN_NARGIFY_2(g_in_w, g_in)
XEN_NARGIFY_0(g_graph_cursor_w, g_graph_cursor)
XEN_NARGIFY_1(g_set_graph_cursor_w, g_set_graph_cursor)
XEN_ARGIFY_2(g_channel_widgets_w, g_channel_widgets)
#else
#define g_in_w g_in
#define g_graph_cursor_w g_graph_cursor
#define g_set_graph_cursor_w g_set_graph_cursor
#define g_channel_widgets_w g_channel_widgets
#endif


void g_init_gxchn(void)
{
  XEN_DEFINE_PROCEDURE(S_in,            g_in_w,             2, 0, 0, H_in);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_graph_cursor, g_graph_cursor_w, H_graph_cursor,
				   S_setB S_graph_cursor, g_set_graph_cursor_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE(S_channel_widgets, g_channel_widgets_w, 0, 2, 0, H_channel_widgets);

#if HAVE_SCHEME
  #define H_mouse_enter_graph_hook S_mouse_enter_graph_hook " (snd chn): called when the mouse \
enters the drawing area (graph pane) of the given channel.\n\
  (add-hook! " S_mouse_enter_graph_hook "\n\
    (lambda (snd chn)\n\
      (" S_focus_widget " (car (" S_channel_widgets " snd chn)))))"

  #define H_mouse_leave_graph_hook S_mouse_leave_graph_hook " (snd chn): called when the mouse \
leaves the drawing area (graph pane) of the given channel."

#endif
#if HAVE_RUBY

  #define H_mouse_enter_graph_hook S_mouse_enter_graph_hook " (snd chn): called when the mouse \
enters the drawing area (graph pane) of the given channel.\n\
  $mouse_enter_graph_hook.add-hook!(\"focus\") do |snd chn|\n\
    focus_widget(channel_widgets(snd, chn)[0])\n\
    end"

  #define H_mouse_leave_graph_hook S_mouse_leave_graph_hook " (snd chn): called when the mouse \
leaves the drawing area (graph pane) of the given channel."
#endif
#if HAVE_FORTH
  #define H_mouse_enter_graph_hook S_mouse_enter_graph_hook " (snd chn): called when the mouse \
enters the drawing area (graph pane) of the given channel.\n\
" S_mouse_enter_graph_hook " lambda: <{ snd chn }>\n\
  snd chn " S_channel_widgets " car " S_focus_widget "\n\
; add-hook!"

  #define H_mouse_leave_graph_hook S_mouse_leave_graph_hook " (snd chn): called when the mouse \
leaves the drawing area (graph pane) of the given channel."
#endif

  mouse_enter_graph_hook = XEN_DEFINE_HOOK(S_mouse_enter_graph_hook, 2, H_mouse_enter_graph_hook);    /* args = snd chn */
  mouse_leave_graph_hook = XEN_DEFINE_HOOK(S_mouse_leave_graph_hook, 2, H_mouse_leave_graph_hook);    /* args = snd chn */
}
