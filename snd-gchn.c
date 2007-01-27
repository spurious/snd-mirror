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

GtkWidget *channel_graph(chan_info *cp)      {return(cp->cgx->chan_widgets[W_graph]);}
GtkWidget *channel_sx(chan_info *cp)         {return(cp->cgx->chan_widgets[W_sx]);}
GtkWidget *channel_sy(chan_info *cp)         {return(cp->cgx->chan_widgets[W_sy]);}
GtkWidget *channel_zx(chan_info *cp)         {return(cp->cgx->chan_widgets[W_zx]);}
GtkWidget *channel_zy(chan_info *cp)         {return(cp->cgx->chan_widgets[W_zy]);}
static GtkWidget *channel_gsy(chan_info *cp) {return(cp->cgx->chan_widgets[W_gsy]);}
static GtkWidget *channel_gzy(chan_info *cp) {return(cp->cgx->chan_widgets[W_gzy]);}
GtkWidget *channel_w(chan_info *cp)          {return(cp->cgx->chan_widgets[W_w]);}
GtkWidget *channel_f(chan_info *cp)          {return(cp->cgx->chan_widgets[W_f]);}
GtkWidget *channel_up_arrow(chan_info *cp)   {return(cp->cgx->chan_widgets[W_up_ev]);}
GtkWidget *channel_down_arrow(chan_info *cp) {return(cp->cgx->chan_widgets[W_down_ev]);}

#define EDIT_HISTORY_LIST(Cp) (Cp->cgx)->edhist_list

static GtkWidget *channel_main_pane(chan_info *cp) {return(cp->cgx->chan_widgets[W_main_window]);}
static GtkObject *gsy_adj(chan_info *cp)           {return(cp->cgx->chan_adjs[W_gsy_adj]);}
static GtkObject *gzy_adj(chan_info *cp)           {return(cp->cgx->chan_adjs[W_gzy_adj]);}
static GtkObject *sy_adj(chan_info *cp)            {return(cp->cgx->chan_adjs[W_sy_adj]);}
static GtkObject *sx_adj(chan_info *cp)            {return(cp->cgx->chan_adjs[W_sx_adj]);}
static GtkObject *zy_adj(chan_info *cp)            {return(cp->cgx->chan_adjs[W_zy_adj]);}
static GtkObject *zx_adj(chan_info *cp)            {return(cp->cgx->chan_adjs[W_zx_adj]);}

static Float sqr(Float a) {return(a * a);}
static Float cube(Float a) {return(a * a * a);}

bool channel_graph_is_visible(chan_info *cp)
{
  return((cp) &&
	 (cp->cgx) &&
	 (channel_graph(cp)) &&
	 (GTK_WIDGET_VISIBLE(channel_graph(cp))) &&
	 (cp->sound) &&
	 /* here we may have a sound wrapper for variable display in which case the sound widgets are not implemented */
	 (((cp->sound->inuse == SOUND_WRAPPER) || (cp->sound->inuse == SOUND_REGION)) ||
	  ((cp->sound->inuse == SOUND_NORMAL) &&
	   /* other choice: SOUND_IDLE -> no display */
	   (cp->sound->sgx) &&
	   (w_snd_pane(cp->sound)) &&
	   (GTK_WIDGET_VISIBLE(w_snd_pane(cp->sound))))));
}

bool channel_open_pane(chan_info *cp)
{
  gtk_widget_show(channel_main_pane(cp));
  return(false);
}

static void sy_changed(float value, chan_info *cp)
{
  axis_info *ap;
  ap = cp->axis;
  ap->sy = value - ap->zy;
  if (ap->sy < 0.0) ap->sy = 0.0;
  apply_y_axis_change(ap, cp);
}

static void sx_changed(float value, chan_info *cp)
{
  axis_info *ap;
  ap = cp->axis;
  ap->sx = value;
  apply_x_axis_change(ap, cp);
}

static void zy_changed(float value, chan_info *cp)
{ 
  axis_info *ap;
  Float old_zy;
  ap = cp->axis;
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
  ap = cp->axis;
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
  focus_x_axis_change(ap, cp, zoom_focus_style(ss));
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
  cp->gzy = value;
  GTK_ADJUSTMENT(gsy_adj(cp))->page_size = value; 
  gtk_adjustment_changed(GTK_ADJUSTMENT(gsy_adj(cp)));
  for_each_sound_chan(cp->sound, update_graph_or_warn);
}

static void gsy_changed(float value, chan_info *cp)
{
  cp->gsy = cp->gzy * value;
  for_each_sound_chan(cp->sound, update_graph_or_warn);
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
  set_scrollbar(zy_adj(cp), 1.0 - sqrt(ap->zy), .1);
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
  ap = cp->axis;
  if (ap->y_ambit != 0.0)
    {
      Float size;
      size = (ap->y1 - ap->y0) / ap->y_ambit;
      set_scrollbar(sy_adj(cp), 
		    1.0 - ((ap->y0 - ap->ymin) / ap->y_ambit + size), 
		    size);
    }
}

void resize_sx(chan_info *cp)
{
  axis_info *ap;
  snd_info *sp;
  ap = cp->axis;
  sp = cp->sound;
  if (ap->x_ambit != 0.0)
    set_scrollbar(sx_adj(cp),
		  (ap->x0 - ap->xmin) / ap->x_ambit,
		  (ap->x1 - ap->x0) / ap->x_ambit);
}

void resize_zx(chan_info *cp)
{
  axis_info *ap;
  ap = cp->axis;
  if (ap->x_ambit < X_RANGE_CHANGEOVER)
    set_scrollbar(zx_adj(cp), sqrt(ap->zx), .1);
  else set_scrollbar(zx_adj(cp), pow(ap->zx, 1.0 / 3.0), .1);
}

void resize_zy(chan_info *cp)
{
  axis_info *ap;
  ap = cp->axis;
  set_scrollbar(zy_adj(cp), 1.0 - sqrt(ap->zy), .1);
}

static void sy_valuechanged_callback(GtkAdjustment *adj, gpointer context)
{
  /* see note above -- context may be garbage!! -- this is a huge bug in gtk */
  chan_info *cp;
  cp = (chan_info *)get_user_data(G_OBJECT(adj));
  if (cp->active)
    sy_changed(1.0 - adj->value, cp);
}

static void sx_valuechanged_callback(GtkAdjustment *adj, gpointer context)
{
  chan_info *cp;
  cp = (chan_info *)get_user_data(G_OBJECT(adj));
  if (cp->active)
    sx_changed(adj->value, cp);
}

static void zy_valuechanged_callback(GtkAdjustment *adj, gpointer context)
{
  chan_info *cp;
  cp = (chan_info *)get_user_data(G_OBJECT(adj));
  if (cp->active)
    zy_changed(1.0 - adj->value, cp);
}

static void zx_valuechanged_callback(GtkAdjustment *adj, gpointer context)
{
  chan_info *cp;
  cp = (chan_info *)get_user_data(G_OBJECT(adj));
  if (cp->active)
    zx_changed(adj->value, cp);
}

static void gzy_valuechanged_callback(GtkAdjustment *adj, gpointer context)
{
  chan_info *cp;
  cp = (chan_info *)get_user_data(G_OBJECT(adj));
  if (cp->active)
    gzy_changed(1.0 - adj->value, cp);
}

static void gsy_valuechanged_callback(GtkAdjustment *adj, gpointer context)
{
  chan_info *cp;
  cp = (chan_info *)get_user_data(G_OBJECT(adj));
  if (cp->active)
    gsy_changed(1.0 - adj->value, cp);
}

static int last_f_state = 0;
static gboolean f_toggle_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{ 
  last_f_state = ev->state;
  return(false);
}

static void f_toggle_click_callback(GtkWidget *w, gpointer data)
{
  f_button_callback((chan_info *)data, 
		    (GTK_TOGGLE_BUTTON(w)->active), 
		    (last_f_state & snd_ControlMask));
}

static int last_w_state = 0;
static gboolean w_toggle_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{ 
  last_w_state = ev->state;
  return(false);
}

static void w_toggle_click_callback(GtkWidget *w, gpointer data)
{
  w_button_callback((chan_info *)data, 
		    (GTK_TOGGLE_BUTTON(w)->active), 
		    (last_w_state & snd_ControlMask));
}

#define MIN_REGRAPH_X 12
#define MIN_REGRAPH_Y 7

static gboolean channel_expose_callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  chan_info *cp;
  snd_info *sp;
  cp = (chan_info *)data;
  if ((cp == NULL) || (!(cp->active)) || (cp->sound == NULL)) return(false);
  if ((ev->area.height < MIN_REGRAPH_Y) || 
      (ev->area.width < MIN_REGRAPH_X)) 
    return(false);
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
  int pdata;
  pdata = get_user_int_data(G_OBJECT(w));
  if (XEN_HOOKED(mouse_enter_graph_hook))
    run_hook(mouse_enter_graph_hook,
	     XEN_LIST_2(C_TO_XEN_INT(UNPACK_SOUND(pdata)),
			C_TO_XEN_INT(UNPACK_CHANNEL(pdata))),
	     S_mouse_enter_graph_hook);
  gdk_window_set_cursor(w->window, ss->sgx->graph_cursor);
  return(false);
}

static gboolean graph_mouse_leave(GtkWidget *w, GdkEventCrossing *ev, gpointer data)
{
  int pdata;
  pdata = get_user_int_data(G_OBJECT(w));
  if (XEN_HOOKED(mouse_leave_graph_hook))
    run_hook(mouse_leave_graph_hook,
	     XEN_LIST_2(C_TO_XEN_INT(UNPACK_SOUND(pdata)),
			C_TO_XEN_INT(UNPACK_CHANNEL(pdata))),
	     S_mouse_leave_graph_hook);
  gdk_window_set_cursor(w->window, ss->sgx->arrow_cursor);
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
  char *str;
  slist *lst;
  if ((!cp) || (!(cp->cgx)) || (!(cp->active))) return;
  if (cp->squelch_update) return;
  lst = EDIT_HISTORY_LIST(cp);
  if (!lst) return;
  slist_clear(lst);
  sp = cp->sound;
#if MUS_DEBUGGING
  if ((!sp) || (!(sp->active)) || (sp->inuse != SOUND_NORMAL))
    fprintf(stderr, "trouble in remake_edit_history: %p %d %d\n", sp, (sp) ? sp->active : -1, (sp) ? sp->inuse : -1);
#endif
  if (sp->channel_style != CHANNELS_SEPARATE)
    {
      int k, ed, filelen;
      char *title;
      chan_info *ncp;
      filelen = 16 + strlen(sp->filename);
      title = (char *)CALLOC(filelen, sizeof(char));
      for (k = 0, ed = 0; k < sp->nchans; k++)
	{
	  ncp = sp->chans[k];
	  if ((ncp) && (ncp->sound))
	    {
	      ncp->edhist_base = ed++;
	      sprintf(title, "chan %d: %s", k + 1, sp->filename);
	      slist_append(lst, title);
	      eds = ncp->edit_ctr;
	      while ((eds < (ncp->edit_size - 1)) && (ncp->edits[eds + 1])) eds++;
	      for (i = 1; i <= eds; i++, ed++) 
		slist_append(lst, str = edit_to_string(ncp, i));
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
      FREE(title);
    }
  else
    {
      eds = cp->edit_ctr;
      while ((eds < (cp->edit_size - 1)) && (cp->edits[eds + 1])) eds++;
      if (eds >= 0)
	{
	  str = sp->filename;
	  slist_append(lst, str);
	  for (i = 1; i <= eds; i++) 
	    slist_append(lst, str = edit_to_string(cp, i));
	}
      slist_select(lst, cp->edit_ctr);
    }
  goto_graph(cp);
}

void reflect_edit_history_change(chan_info *cp)
{
  /* new edit so it is added, and any trailing lines removed */
  snd_info *sp;
  if ((cp->in_as_one_edit > 0) || (cp->cgx == NULL)) return;
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
  if (cp->cgx == NULL) return;
  sp = cp->sound;
  if ((cp->edit_ctr == 0) &&
      (sp->watchers))
    call_sp_watchers(sp, SP_REVERT_WATCHER, SP_REVERTED);
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
  gdk_window_get_pointer(ev->window, &x, &y, &key_state);
  key_state = (GdkModifierType)(ev->state);
  keysym = ev->keyval;
  theirs = key_press_callback(cp, x, y, ev->state, keysym);
  if (theirs) ss->sgx->graph_is_active = false;
  g_signal_stop_emission(GTK_OBJECT(w), g_signal_lookup("key_press_event", G_OBJECT_TYPE(GTK_OBJECT(w))), 0);
  return(true);
}

gboolean graph_key_press(GtkWidget *w, GdkEventKey *ev, gpointer data)
{
  chan_info *cp = (chan_info *)data;
  int keysym;
  bool theirs;
  int x, y;
  GdkModifierType key_state;
  gdk_window_get_pointer(ev->window, &x, &y, &key_state);
  key_state = (GdkModifierType)(ev->state);
  keysym = ev->keyval;
  theirs = key_press_callback(cp, x, y, ev->state, keysym);
  if (theirs) ss->sgx->graph_is_active = true;
  return(true);
}
 
static gboolean graph_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  if ((ev->state == 0) && (ev->type == GDK_BUTTON_PRESS) && (ev->button == POPUP_BUTTON))
    {
      int pdata;
      pdata = get_user_int_data(G_OBJECT(w));
      popup_menu_from(w, ev, data, UNPACK_SOUND(pdata), UNPACK_CHANNEL(pdata));
      return(true);
    }
  else
    {
      chan_info *cp = (chan_info *)data;
      ss->sgx->graph_is_active = true;
      gtk_widget_grab_focus(w);
      if ((cp->sound) && (cp->sound->sgx))
	cp->sound->sgx->mini_active = false;
      graph_button_press_callback(cp, (int)(ev->x), (int)(ev->y), ev->state, ev->button, ev->time);
    }
  return(false);
}

static gboolean graph_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  graph_button_release_callback((chan_info *)data, (int)(ev->x), (int)(ev->y), ev->state, ev->button);
  return(false);
}

static gboolean graph_scroll(GtkWidget *w, GdkEventScroll *ev, gpointer data)
{
  /* ev->direction + 4 maps this into mouse-click-hook as button 4 or 5 (!) -- is this a good idea? */
  graph_button_release_callback((chan_info *)data, (int)(ev->x), (int)(ev->y), ev->state, ev->direction + 4);
  return(false);
}

static gboolean graph_button_motion(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{ 
  if (ev->state & GDK_BUTTON1_MASK)
    {
      int x, y;
      GdkModifierType state;
      if (ev->is_hint)
	gdk_window_get_pointer(ev->window, &x, &y, &state);
      else
	{
	  x = (int)(ev->x);
	  y = (int)(ev->y);
	}
      graph_button_motion_callback((chan_info *)data, x, y, ev->time);
    }
  return(false);
}

#if HAVE_GL
static const gint config_attributes[] = {
  GDK_GL_DOUBLEBUFFER,
  GDK_GL_RGBA,
  GDK_GL_RED_SIZE,        1,
  GDK_GL_GREEN_SIZE,      1,
  GDK_GL_BLUE_SIZE,       1,
  GDK_GL_DEPTH_SIZE,      12,
  GDK_GL_ATTRIB_LIST_NONE
};
#endif

static void channel_drop_watcher(GtkWidget *w, const char *filename, int x, int y, void *data)
{
  mix_at_x_y(get_user_int_data(G_OBJECT(w)), filename, x, y);
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
  GtkObject **adjs;
  chan_info *cp;
  chan_context *cx;
  axis_context *cax;
  state_context *sx;
  bool make_widgets, need_extra_scrollbars;
  make_widgets = ((sp->chans[channel]) == NULL);
  sp->chans[channel] = make_chan_info(sp->chans[channel], channel, sp);
  cp = sp->chans[channel];
  cx = cp->cgx;
  if (cx->chan_widgets == NULL) 
    {
      cw = (GtkWidget **)CALLOC(NUM_CHAN_WIDGETS, sizeof(GtkWidget *));
      adjs = (GtkObject **)CALLOC(NUM_CHAN_ADJS, sizeof(GtkObject *));
      cp->cgx->chan_widgets = cw;
      cp->cgx->chan_adjs = adjs;
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
	  gtk_container_set_border_width(GTK_CONTAINER(cw[W_main_window]), 2);
	  gtk_box_pack_start(GTK_BOX(w_snd_pane_box(sp)), cw[W_main_window], true, true, 0);
	  cp->cgx->edhist_list = slist_new(cw[W_main_window], NULL, 0, PANED_ADD1);
	  cp->cgx->edhist_list->select_callback = history_select_callback;
	  cp->cgx->edhist_list->select_callback_data = (void *)cp;
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
#if HAVE_GL
  #if HAVE_GDK_GL_CONTEXT_COPY
      gtk_widget_set_gl_capability(GTK_WIDGET(cw[W_graph]), gdk_gl_config_new(&config_attributes[0]), NULL, true, GDK_GL_RGBA_TYPE);
  #else
      gtk_widget_set_gl_capability(GTK_WIDGET(cw[W_graph]), gdk_gl_config_new(&config_attributes[0]), GDK_GL_RGBA_TYPE, NULL, true);
  #endif
#endif
      add_drag_and_drop(cw[W_graph], channel_drop_watcher, channel_drag_watcher, NULL);
      set_user_int_data(G_OBJECT(cw[W_graph]), PACK_SOUND_AND_CHANNEL(sp->index, cp->chan));
      gtk_widget_set_events(cw[W_graph], GDK_ALL_EVENTS_MASK);
      GTK_WIDGET_SET_FLAGS(cw[W_graph], GTK_CAN_FOCUS);
      gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_graph], 2, 3, 0, 2, 
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND), 
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND | GTK_SHRINK), 
		       0, 0);
      gtk_widget_show(cw[W_graph]);
      if (with_events)
	{
	  SG_SIGNAL_CONNECT(cw[W_graph], "expose_event", channel_expose_callback, cp);
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
 	  SG_SIGNAL_CONNECT(cw[W_graph], "scroll_event", graph_scroll, cp);
	}

      cw[W_bottom_scrollers] = gtk_vbox_new(true, 0);
      gtk_box_set_spacing(GTK_BOX(cw[W_bottom_scrollers]), 0);
      gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_bottom_scrollers], 2, 3, 2, 3, 
		       (GtkAttachOptions)(GTK_EXPAND | GTK_FILL), 
		       (GtkAttachOptions)(GTK_FILL), 
		       0, 0);
      gtk_widget_show(cw[W_bottom_scrollers]);

      adjs[W_sx_adj] = gtk_adjustment_new(0.0, 0.0, 1.00, 0.001, 0.01, .01);
      cw[W_sx] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_sx_adj]));
      gtk_box_pack_start(GTK_BOX(cw[W_bottom_scrollers]), cw[W_sx], true, true, 0);
      set_user_data(G_OBJECT(adjs[W_sx_adj]), (gpointer)cp);
      SG_SIGNAL_CONNECT(adjs[W_sx_adj], "value_changed", sx_valuechanged_callback, cp);
      gtk_widget_show(cw[W_sx]);
      gtk_widget_set_name(cw[W_sx], "sx_slider");

      adjs[W_zx_adj] = gtk_adjustment_new(0.0, 0.0, 1.1, 0.001, 0.01, .1);
      cw[W_zx] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_zx_adj]));
      gtk_box_pack_start(GTK_BOX(cw[W_bottom_scrollers]), cw[W_zx], true, true, 0);
      set_user_data(G_OBJECT(adjs[W_zx_adj]), (gpointer)cp);
      SG_SIGNAL_CONNECT(adjs[W_zx_adj], "value_changed", zx_valuechanged_callback, cp);
      gtk_widget_set_name(cw[W_zx], "zx_slider");
      gtk_widget_show(cw[W_zx]);


      cw[W_wf_buttons] = gtk_vbox_new(true, 0);
      gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_wf_buttons], 0, 2, 1, 3, GTK_SHRINK, GTK_SHRINK, 0, 0);
      gtk_widget_show(cw[W_wf_buttons]);
      
      if (button_style == WITH_FW_BUTTONS)
	{
	  cw[W_f] = gtk_check_button_new_with_label(_("f"));
	  gtk_box_pack_start(GTK_BOX(cw[W_wf_buttons]), cw[W_f], true, true, 0);
	  gtk_widget_show(cw[W_f]);
	  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(cw[W_f]), false);
	  SG_SIGNAL_CONNECT(cw[W_f], "button_press_event", f_toggle_callback, cp);
	  SG_SIGNAL_CONNECT(cw[W_f], "toggled", f_toggle_click_callback, cp);
  
	  cw[W_w] = gtk_check_button_new_with_label(_("w"));
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

      adjs[W_zy_adj] = gtk_adjustment_new(0.0, 0.0, 1.1, 0.001, 0.01, .1);   /* 0 -> 1 (upside down) */
      cw[W_zy] = gtk_vscrollbar_new(GTK_ADJUSTMENT(adjs[W_zy_adj]));
      gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_zy], 0, 1, 0, 1, 
		       (GtkAttachOptions)(GTK_FILL), 
		       (GtkAttachOptions)(GTK_EXPAND | GTK_FILL | GTK_SHRINK), 
		       0, 0);
      set_user_data(G_OBJECT(adjs[W_zy_adj]), (gpointer)cp);
      SG_SIGNAL_CONNECT(adjs[W_zy_adj], "value_changed", zy_valuechanged_callback, cp);
      gtk_widget_show(cw[W_zy]);
      gtk_widget_set_name(cw[W_zy], "zy_slider");

      adjs[W_sy_adj] = gtk_adjustment_new(0.5, 0.0, 1.01, 0.001, 0.01, .01);
      cw[W_sy] = gtk_vscrollbar_new(GTK_ADJUSTMENT(adjs[W_sy_adj]));
      gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_sy], 1, 2, 0, 1, 
		       (GtkAttachOptions)(GTK_FILL), 
		       (GtkAttachOptions)(GTK_EXPAND | GTK_FILL | GTK_SHRINK), 
		       0, 0);
      set_user_data(G_OBJECT(adjs[W_sy_adj]), (gpointer)cp);
      SG_SIGNAL_CONNECT(adjs[W_sy_adj], "value_changed", sy_valuechanged_callback, cp);
      gtk_widget_show(cw[W_sy]);
      gtk_widget_set_name(cw[W_sy], "sy_slider");

      if (need_extra_scrollbars)
	{
	  adjs[W_gsy_adj] = gtk_adjustment_new(0.0, 0.0, 1.0, 0.001, 0.01, .01);
	  cw[W_gsy] = gtk_vscrollbar_new(GTK_ADJUSTMENT(adjs[W_gsy_adj]));
	  gtk_table_attach(GTK_TABLE(cw[W_graph_window]), cw[W_gsy], 3, 4, 0, 2, 
			   (GtkAttachOptions)(GTK_FILL), 
			   (GtkAttachOptions)(GTK_EXPAND | GTK_FILL | GTK_SHRINK), 
			   0, 0);
	  set_user_data(G_OBJECT(adjs[W_gsy_adj]), (gpointer)cp);
	  SG_SIGNAL_CONNECT(adjs[W_gsy_adj], "value_changed", gsy_valuechanged_callback, cp);
	  gtk_widget_show(cw[W_gsy]);
	  gtk_widget_set_name(cw[W_gsy], "gsy_slider");

	  adjs[W_gzy_adj] = gtk_adjustment_new(1.0, 0.0, 1.00, 0.001, 0.01, .01);
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
	gtk_paned_set_position(GTK_PANED(cw[W_main_window]), 0);
      gtk_widget_show(cw[W_graph_window]);

    }
  else recolor_graph(cp, false); /* in case selection color left over from previous use */
  if ((sp->channel_style != CHANNELS_COMBINED) || (channel == 0))
    gtk_widget_show_all(cw[W_main_window]);

  if ((need_extra_scrollbars) && (sp->channel_style != CHANNELS_COMBINED))
    hide_gz_scrollbars(sp); /* default is on in this case */  

  reflect_edit_history_change(cp);

  cax = cx->ax;
  cax->gc = sx->basic_gc;
  /* cax->wn has to wait until update_graph */
  return(0);
}

static void set_graph_font(chan_info *cp, PangoFontDescription *fnt)
{
  chan_context *cx;
  cx = cp->tcgx;
  if (!cx) cx = cp->cgx;
  cx->ax->current_font = fnt;
}

void set_peak_numbers_font(chan_info *cp) {set_graph_font(cp, PEAKS_FONT(ss));}
void set_tiny_numbers_font(chan_info *cp) {set_graph_font(cp, TINY_FONT(ss));}
void set_bold_peak_numbers_font(chan_info *cp) {set_graph_font(cp, BOLD_PEAKS_FONT(ss));}


color_t get_foreground_color(axis_context *ax)
{
  static GdkGCValues gv;
  gdk_gc_get_values(ax->gc, &gv);
  return(&(gv.foreground));
}

color_t get_background_color(axis_context *ax)
{
  static GdkGCValues gv;
  gdk_gc_get_values(ax->gc, &gv);
  return(&(gv.background));
}

void set_foreground_color(axis_context *ax, GdkColor *color)
{
  gdk_gc_set_foreground(ax->gc, color);
}

GdkGC *copy_GC(chan_info *cp)
{
  state_context *sx;
  sx = ss->sgx;
  if (cp->cgx->selected) return(sx->selected_basic_gc);
  return(sx->basic_gc);
}

GdkGC *erase_GC(chan_info *cp)
{
  state_context *sx;
  snd_info *sp;
  sp = cp->sound;
  sx = ss->sgx;
  if ((cp->cgx->selected) ||
      ((sp) && (sp->channel_style == CHANNELS_SUPERIMPOSED) && (sp->index == ss->selected_sound)))
    return(sx->selected_erase_gc);
  return(sx->erase_gc);
}

void free_fft_pix(chan_info *cp)
{
  if ((cp->cgx->fft_pix) &&
      (channel_graph(cp)))
    g_object_unref(cp->cgx->fft_pix);
  cp->cgx->fft_pix = NULL;
  cp->cgx->fft_pix_ready = false;
}

bool restore_fft_pix(chan_info *cp, axis_context *ax)
{
#if HAVE_GDK_DRAW_PIXBUF
  gdk_draw_pixbuf(ax->wn,
		  copy_GC(cp),
		  cp->cgx->fft_pix,
		  0, 0,
		  cp->cgx->fft_pix_x0, cp->cgx->fft_pix_y0,
		  cp->cgx->fft_pix_width, cp->cgx->fft_pix_height,
		  GDK_RGB_DITHER_NONE, 0, 0); /* dithering */
  return(true);
#else
  return(false);
#endif
}

void save_fft_pix(chan_info *cp, axis_context *ax, int fwidth, int fheight, int x0, int y1)
{
  if ((fwidth == 0) || (fheight == 0)) return;
#if HAVE_GDK_DRAW_PIXBUF
  cp->cgx->fft_pix_width = fwidth;
  cp->cgx->fft_pix_height = fheight;
  cp->cgx->fft_pix_x0 = x0;
  cp->cgx->fft_pix_y0 = y1;
  cp->cgx->fft_pix = gdk_pixbuf_get_from_drawable(cp->cgx->fft_pix,
						  ax->wn,
						  gtk_widget_get_colormap(ax->w),
						  cp->cgx->fft_pix_x0, cp->cgx->fft_pix_y0,
						  0, 0,
						  cp->cgx->fft_pix_width, cp->cgx->fft_pix_height);
  cp->cgx->fft_pix_ready = true;
#endif
}

void cleanup_cw(chan_info *cp)
{
  if ((cp) && (cp->cgx))
    {
      chan_context *cx;
      GtkWidget **cw;
      free_fft_pix(cp);
      if (EDIT_HISTORY_LIST(cp)) 
	{
	  slist_clear(EDIT_HISTORY_LIST(cp));
	  gtk_paned_set_position(GTK_PANED(cp->cgx->chan_widgets[W_main_window]), 1);
	}
      cx = cp->cgx;
      cx->selected = false;
      cw = cx->chan_widgets;
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
  if ((sp) && (sp->nchans > 1))
    {
      channel_style_t old_style;
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
	      for (i = 1; i < sp->nchans; i++) clear_mix_y(sp->chans[i]);
	    }
	  else 
	    {
	      if (new_style == CHANNELS_COMBINED)
		{
		  show_gz_scrollbars(sp);
		  for (i = 1; i < sp->nchans; i++) clear_mix_y(sp->chans[i]);
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
		  /* set to green ? */
		  apply_y_axis_change((sp->chans[0])->axis, sp->chans[0]);
		  apply_x_axis_change((sp->chans[0])->axis, sp->chans[0]);
		  for (i = 1; i < sp->nchans; i++) CURSOR(sp->chans[i]) = CURSOR(sp->chans[0]);
		}
	    }
	  height[0] = widget_height(w_snd_pane(sp)) - control_panel_height(sp);
	  if (old_style == CHANNELS_SEPARATE)
	    {
	      chan_context *mcgx;
	      chan_info *ncp;
	      ncp = sp->chans[0];
	      mcgx = ncp->cgx;
	      for (i = 1; i < sp->nchans; i++) 
		{
		  ncp = sp->chans[i];
		  cleanup_cw(ncp);
		  ncp->tcgx = mcgx;
		  fixup_cp_cgx_ax_wn(ncp);
		  reset_mix_graph_parent(ncp);
		}
	      channel_open_pane(sp->chans[0]);
	      set_toggle_button(unite_button(sp), true, false, (void *)sp);
	    }
	  else
	    {
	      if (new_style == CHANNELS_SEPARATE)
		{
		  axis_info *ap;
		  chan_info* pcp;
		  GtkWidget **cw;
		  chan_context *cx;
		  /* height[0] = total space available */
		  height[0] /= sp->nchans;

		  map_over_sound_chans(sp, channel_open_pane);
		  for (i = 0; i < sp->nchans; i++) reset_mix_graph_parent(sp->chans[i]);
		  pcp = sp->chans[0];
		  ap = pcp->axis;
		  for (i = 1; i < sp->nchans; i++)
		    {
		      chan_info *cp;
		      cp = sp->chans[i];
		      cp->tcgx = NULL;
		      fixup_cp_cgx_ax_wn(cp);
		      cx = cp->cgx;
		      cw = cx->chan_widgets;
		      gtk_widget_show_all(cw[W_main_window]);
		      set_toggle_button(cw[W_f], cp->graph_transform_p, false, (void *)cp);
		      set_toggle_button(cw[W_w], cp->graph_time_p, false, (void *)cp);
		      /* these can get out of sync if changes are made in the unseparated case */
		      set_axes(cp, ap->x0, ap->x1, ap->y0, ap->y1);
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

bool fixup_cp_cgx_ax_wn(chan_info *cp)
{
  GtkWidget *w; 
  axis_context *ax; 
  ax = cp->cgx->ax;
  if (cp->tcgx) 
    w = channel_graph(cp->sound->chans[0]);
  else w = channel_graph(cp);
  ax->wn = w->window;
  ax->w = w;
  return(w->window != NULL);
}

static XEN g_channel_widgets(XEN snd, XEN chn)
{
  #define H_channel_widgets "(" S_channel_widgets " :optional snd chn): a list of widgets: ((0)graph (1)w (2)f (3)sx (4)sy (5)zx (6)zy (7)\
edhist (8)gsy (9)gzy (10)main (11)sx_adj (12)sy_adj (13)zx_adj (14)zy_adj (15)gsy_adj (16)gzy_adj"

  #define XEN_WRAP_ADJ(Value) ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("GtkAdjustment_"), C_TO_XEN_ULONG((unsigned long)Value)) : XEN_FALSE)

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

#ifdef XEN_ARGIFY_1
XEN_ARGIFY_2(g_channel_widgets_w, g_channel_widgets)
#else
#define g_channel_widgets_w g_channel_widgets
#endif

void g_init_gxchn(void)
{
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
