#include "snd.h"

enum {
    W_top, W_form,
    W_main_window,
    W_edhist,
    W_wf_buttons,
      W_f, W_w,
    W_left_scrollers,
      W_zy, W_sy,
    W_bottom_scrollers,
      W_sx, W_zx,
    W_graph,
      W_gzy, W_gsy
};
#define NUM_CHAN_WIDGETS 16
#define DEFAULT_EDIT_HISTORY_WIDTH 1

Widget channel_main_pane(chan_info *cp)
{
#if (XmVERSION > 1)
  if ((cp) && (cp->cgx)) return((cp->cgx)->chan_widgets[W_form]);
#else
  if ((cp) && (cp->cgx)) return((cp->cgx)->chan_widgets[W_main_window]);
#endif
  return(NULL);
}

Widget channel_graph(chan_info *cp)      {return((cp->cgx)->chan_widgets[W_graph]);}
Widget channel_sx(chan_info *cp)         {return((cp->cgx)->chan_widgets[W_sx]);}
Widget channel_sy(chan_info *cp)         {return((cp->cgx)->chan_widgets[W_sy]);}
Widget channel_zx(chan_info *cp)         {return((cp->cgx)->chan_widgets[W_zx]);}
Widget channel_zy(chan_info *cp)         {return((cp->cgx)->chan_widgets[W_zy]);}
static Widget channel_gsy(chan_info *cp) {return((cp->cgx)->chan_widgets[W_gsy]);}
static Widget channel_gzy(chan_info *cp) {return((cp->cgx)->chan_widgets[W_gzy]);}
Widget channel_w(chan_info *cp)          {return((cp->cgx)->chan_widgets[W_w]);}
Widget channel_f(chan_info *cp)          {return((cp->cgx)->chan_widgets[W_f]);}

#define EDIT_HISTORY_LIST(Cp) (Cp->cgx)->chan_widgets[W_edhist]

static Float sqr(Float a) {return(a * a);}
static Float cube (Float a) {return(a * a * a);}

static Float get_scrollbar(Widget w, int val, int scrollbar_max)
{
  int size;
  if (val == 0) return(0.0);
  XtVaGetValues(w, XmNsliderSize, &size, NULL);
  return((Float)val / (Float)(scrollbar_max - size));
}

static void sy_changed(int value, chan_info *cp)
{
  axis_info *ap;
  Float low;
  ap = cp->axis;
  low = get_scrollbar(channel_sy(cp), value, SCROLLBAR_MAX);
  ap->sy = (1.0 - ap->zy) * low;
  apply_y_axis_change(ap, cp);
}

#define SCROLLBAR_SX_MAX 2000

static void sx_changed(int value, chan_info *cp)
{
  /* treat as centered with non-slider trough as defining current bounds */
  axis_info *ap;
  snd_info *sp;
  double low;
  ap = cp->axis;
  sp = cp->sound;
  low = get_scrollbar(channel_sx(cp), value, SCROLLBAR_SX_MAX);
  ap->sx = low * (1.0 - ap->zx);
  apply_x_axis_change(ap, cp, sp);
}

static void zy_changed(int value, chan_info *cp)
{ 
  axis_info *ap;
  Float old_zy;
  ap = cp->axis;
  if (value < 1) value = 1;
  old_zy = ap->zy;
  ap->zy = sqr(get_scrollbar(channel_zy(cp), value, SCROLLBAR_MAX));
  ap->sy += (.5 * (old_zy - ap->zy)); /* try to keep wave centered */
  if (ap->sy < 0) ap->sy = 0;
  apply_y_axis_change(ap, cp);
  resize_sy(cp);
}

#define X_RANGE_CHANGEOVER 20.0

static void zx_changed(int value, chan_info *cp)
{ /* scrollbar change */
  axis_info *ap;
  snd_info *sp;
  snd_state *ss;
  sp = cp->sound;
  ss = cp->state;
  ap = cp->axis;
  if (ap->xmax == 0.0) return;
  if (ap->xmax <= ap->xmin) 
    {
      ap->xmax = ap->xmin + .001;
      ap->x_ambit = .001;
    }
  if (value < 1) value = 1;
  if (ap->x_ambit < X_RANGE_CHANGEOVER)
    ap->zx = sqr(get_scrollbar(channel_zx(cp), value, SCROLLBAR_MAX));
  else ap->zx = cube(get_scrollbar(channel_zx(cp), value, SCROLLBAR_MAX));
  /* if cursor visible, focus on that, else selection, else mark, else left side */
  focus_x_axis_change(ap, cp, sp, zoom_focus_style(ss));
  resize_sx(cp);
}

void set_zx_scrollbar_value(chan_info *cp, Float value)
{
  XtVaSetValues(channel_zx(cp), XmNvalue, (int)(value * SCROLLBAR_MAX), NULL);
}

static void set_scrollbar(Widget w, Float position, Float range, int scrollbar_max) /* position and range 0 to 1.0 */
{
  int size, val;
  size = (int)(scrollbar_max * range);
  if (size > scrollbar_max) 
    size = scrollbar_max; /* this can't happen!?! */
  if (size < 1) size = 1;
  val = (int)(scrollbar_max * position);
  if ((val + size) > scrollbar_max) val = scrollbar_max - size;
  if (val < 0) val = 0;
  XtVaSetValues(w,
		XmNsliderSize, size,
		XmNvalue, val,
		NULL);
}

static void gzy_changed(int value, chan_info *cp)
{
  Float chan_frac, new_gsy, new_size;
  cp->gzy = get_scrollbar(channel_gzy(cp), value, SCROLLBAR_MAX);
  chan_frac = 1.0 / ((Float)(((snd_info *)(cp->sound))->nchans));
  new_size = chan_frac + ((1.0 - chan_frac) * cp->gzy);
  if ((cp->gsy + new_size) > 1.0) 
    new_gsy = 1.0 - new_size; 
  else new_gsy = cp->gsy;
  if (new_gsy < 0.0) new_gsy = 0.0;
  set_scrollbar(channel_gsy(cp), new_gsy, new_size, SCROLLBAR_MAX);
  for_each_sound_chan(cp->sound, update_graph);
}

static void gsy_changed(int value, chan_info *cp)
{
  Float low;
  low = get_scrollbar(channel_gsy(cp), value, SCROLLBAR_MAX);
  cp->gsy = (1.0 - cp->gzy) * low;
  for_each_sound_chan(cp->sound, update_graph);
}

void fixup_gsy(chan_info *cp, Float low, Float high)
{
  Widget wcp;
  int ival;
  Float val, size;
  wcp = channel_gsy(cp);
  XtVaGetValues(wcp, XmNvalue, &ival, NULL);
  val = (Float)ival / (Float)(SCROLLBAR_MAX);
  XtVaGetValues(wcp, XmNsliderSize, &ival, NULL);
  size = (Float)ival / (Float)(SCROLLBAR_MAX);
  if ((val > low) || ((val + size) < high))
    {
      val = low;
      if ((val + size) > 1.0) val = 1.0 - size;
      ival = (int)(val * SCROLLBAR_MAX);
      XtVaSetValues(wcp, XmNvalue, ival, NULL);
      gsy_changed((int)(val * SCROLLBAR_MAX), cp);
    }
}

Float gsy_value(chan_info *cp)
{
  Widget wcp;
  int ival;
  wcp = channel_gsy(cp);
  XtVaGetValues(wcp, XmNvalue, &ival, NULL);
  return((Float)ival / (Float)(SCROLLBAR_MAX));
}

Float gsy_size(chan_info *cp)
{
  Widget wcp;
  int ival;
  wcp = channel_gsy(cp);
  XtVaGetValues(wcp, XmNsliderSize, &ival, NULL);
  return((Float)ival / (Float)(SCROLLBAR_MAX));
}

void initialize_scrollbars(chan_info *cp)
{
  axis_info *ap;
  snd_info *sp;
  ap = cp->axis;
  sp = cp->sound;
  set_scrollbar(channel_sx(cp), ap->sx, ap->zx, SCROLLBAR_SX_MAX);
  set_scrollbar(channel_sy(cp), ap->sy, ap->zy, SCROLLBAR_MAX);
  if (ap->x_ambit < X_RANGE_CHANGEOVER)
    set_scrollbar(channel_zx(cp), sqrt(ap->zx), .1, SCROLLBAR_MAX);  /* assume size is 10% of scrollbar length */
  else set_scrollbar(channel_zx(cp), pow(ap->zx, .333), .1, SCROLLBAR_MAX);
  set_scrollbar(channel_zy(cp), ap->zy, .1, SCROLLBAR_MAX);          /* assume 1.0 here so sqrt/cube decision, if any, is not needed */
  if ((sp->nchans > 1) && (cp->chan == 0) && (channel_gsy(cp)))
    {
      set_scrollbar(channel_gsy(cp), cp->gsy, cp->gzy, SCROLLBAR_MAX);
      set_scrollbar(channel_gzy(cp), cp->gzy, 1.0 / (Float)(sp->nchans), SCROLLBAR_MAX);
    }
}

void resize_sy(chan_info *cp)
{
  /* something changed the y axis view, so the scale scroller needs to reflect that change (in size and position) */
  axis_info *ap;
  ap = cp->axis;
  set_scrollbar(channel_sy(cp),
		(ap->y0 - ap->ymin) / ap->y_ambit,
		(ap->y1 - ap->y0) / ap->y_ambit,
		SCROLLBAR_MAX);
}

void resize_sx(chan_info *cp)
{
  axis_info *ap;
  snd_info *sp;
  ap = cp->axis;
  sp = cp->sound;
  set_scrollbar(channel_sx(cp),
		(ap->x0 - ap->xmin) / ap->x_ambit,
		(ap->x1 - ap->x0) / ap->x_ambit,
		SCROLLBAR_SX_MAX);
}

void resize_zx(chan_info *cp)
{
  axis_info *ap;
  ap = cp->axis;
  if (ap->x_ambit < X_RANGE_CHANGEOVER)
    set_scrollbar(channel_zx(cp), sqrt(ap->zx) * .9, .1, SCROLLBAR_MAX);
  else set_scrollbar(channel_zx(cp), pow(ap->zx * .9, 1.0 / 3.0), .1, SCROLLBAR_MAX);
}

void resize_zy(chan_info *cp)
{
  axis_info *ap;
  ap = cp->axis;
  set_scrollbar(channel_zy(cp), sqrt(ap->zy) * .9, .1, SCROLLBAR_MAX);
}


int channel_open_pane(chan_info *cp, void *ptr)
{
  XtManageChild(channel_main_pane(cp));
  return(0);
}

int channel_unlock_pane(chan_info *cp, void *ptr)
{
  XtVaSetValues(channel_main_pane(cp),
		XmNpaneMinimum, 5,
		XmNpaneMaximum, LOTSA_PIXELS,
		NULL);
  return(0);
}

int channel_lock_pane(chan_info *cp, void *ptr)
{
  int val;
  val = (*((int *)ptr));
  if (val < 6) val = 6;
  XtUnmanageChild(channel_main_pane(cp));
  XtVaSetValues(channel_main_pane(cp),
		XmNpaneMinimum, val - 5,
		XmNpaneMaximum, val + 5,
		NULL);
  return(0);
}

static void sy_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  chan_info *cp = (chan_info *)(context);
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (cp->active)
    {
      START_JUST_TIME(cp);
      sy_changed(((XmScrollBarCallbackStruct *)info)->value, cp);
      END_JUST_TIME(cp);
    }
}

static void sy_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  chan_info *cp = (chan_info *)(context);
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (cp->active)
    sy_changed(((XmScrollBarCallbackStruct *)info)->value, cp);
}

static void sx_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  chan_info *cp = (chan_info *)(context);
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (cp->active)
    {
      START_JUST_TIME(cp);
      sx_changed(((XmScrollBarCallbackStruct *)info)->value, cp);
      END_JUST_TIME(cp);
    }
}

static void sx_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  chan_info *cp = (chan_info *)(context);
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (cp->active)
    sx_changed(((XmScrollBarCallbackStruct *)info)->value, cp);
}

static void sx_increment_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* problem here is that in large files these increments, if determined via scrollbar values, are huge */
  /* so, move ahead one windowfull on each tick */
  chan_info *cp = (chan_info *)(context);
  if (cp->active)
    sx_incremented(cp, 1.0);
}

static void sx_decrement_callback(Widget w, XtPointer context, XtPointer info) 
{
  chan_info *cp = (chan_info *)(context);
  if (cp->active)
    sx_incremented(cp, -1.0);
}

static void zy_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  chan_info *cp = (chan_info *)(context);
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (cp->active)
    {
      START_JUST_TIME(cp);
      zy_changed(((XmScrollBarCallbackStruct *)info)->value, cp);
      END_JUST_TIME(cp);
    }
}

static void zy_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  chan_info *cp = (chan_info *)(context);
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (cp->active)
    zy_changed(((XmScrollBarCallbackStruct *)info)->value, cp);
}

static void zx_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  chan_info *cp = (chan_info *)(context);
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (cp->active)
    {
      START_JUST_TIME(cp);
      zx_changed(((XmScrollBarCallbackStruct *)info)->value, cp);
      END_JUST_TIME(cp);
    }
}

static void zx_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  chan_info *cp = (chan_info *)(context);
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (cp->active)
    zx_changed(((XmScrollBarCallbackStruct *)info)->value, cp);
}

static void gzy_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  chan_info *cp = (chan_info *)(context);
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (cp->active)
    {
      START_JUST_TIME(cp);
      gzy_changed(((XmScrollBarCallbackStruct *)info)->value, cp);
      END_JUST_TIME(cp);
    }
}

static void gzy_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  chan_info *cp = (chan_info *)(context);
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (cp->active)
    gzy_changed(((XmScrollBarCallbackStruct *)info)->value, cp);
}

static void gsy_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  chan_info *cp = (chan_info *)(context);
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (cp->active)
    {
      START_JUST_TIME(cp);
      gsy_changed(((XmScrollBarCallbackStruct *)info)->value, cp);
      END_JUST_TIME(cp);
    }
}

static void gsy_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  chan_info *cp = (chan_info *)(context);
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (cp->active)
    gsy_changed(((XmScrollBarCallbackStruct *)info)->value, cp);
}

/* anything special for increment?  XmNincrementCallback sx_increment_callback */


static void graph_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help_with_url_and_wrap((snd_state *)context,
			     "Graph", "#panelayout",
"This portion of the Snd display shows the sound data in the time and/or frequency domains. \
If you click on the time domain wave, you can edit it using emacs-like keyboard commands, as \
well as using mouse-click-and-drag to define the selection.  Once defined, the selected portion \
can be cut, deleted, or pasted elsewhere, the latter with the middle mouse button.");
}

#if (XmVERSION > 1)
static void history_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help_with_url_and_wrap((snd_state *)context,
			     "Edit History",
			     "#edithistory",
"The current state of the undo/redo list can be viewed as a scrolled list of strings in the pane \
on the left of the graph (in Motif 1, there's a 'Show Edit History' menu option).  If there are no \
current edits, it just lists the associated file name (i.e. the zero-edits state).  As you edit the \
sound, the operations appear in the edit list window.  Click on a member of the list to move to \
that point in the edit list (equivalent to some number of undo's or redo's).  To move to a given \
edit point and follow the sync chain (if any), use control-click.");
}
#endif

static void sx_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help_with_wrap((snd_state *)context,
		     "X axis scroll",
"This scrollbar controls the position of the x axis within the overall sound file. The arrows increment the view by one window.");
}

static void sy_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help_with_wrap((snd_state *)context,
		     "Y axis scroll",
"This (nearly useless) scrollbar controls the position of the y-axis within the current y axis limits.");
}

static void zx_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help_with_wrap((snd_state *)context,
		     "X axis zoom",
"This scrollbar zooms in (as you move it to the left) or out along the x axis.");
}

static void zy_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help_with_wrap((snd_state *)context,
		     "Y axis zoom",
"This scrollbar zooms in (as you move it down) or out along the y axis.");
}

static void gsy_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help_with_wrap((snd_state *)context,
		     "Graph position",
"This scrollbar controls the position in the overall combined graph of the portion visible in the sound pane.");
}

static void gzy_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help_with_wrap((snd_state *)context,
		     "Graph zoom",
"This scrollbar controls how much of the overall combined graph is visible in the sound pane.");
}

static void f_button_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help_with_wrap((snd_state *)context,
		     "fft button",
"This button controls whether an FFT is displayed alongside the waveform.  To affect all channels at once, use control-click.");
}

static void w_button_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help_with_wrap((snd_state *)context,
		     "time domain waveform button",
"This button determines whether the time domain waveform is displayed.  If both the 'w' and 'f' buttons are off, only the lisp \
graph (if any) is displayed.  To affect all channels at once, use control-click.");
}


static void f_toggle_callback(Widget w, XtPointer context, XtPointer info)
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  XButtonEvent *ev;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  ev = (XButtonEvent *)(cb->event);
  f_button_callback((chan_info *)context, cb->set, (ev->state & snd_ControlMask));
}

static void w_toggle_callback(Widget w, XtPointer context, XtPointer info)
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  XButtonEvent *ev;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  ev = (XButtonEvent *)(cb->event);
  w_button_callback((chan_info *)context, cb->set, (ev->state & snd_ControlMask));
}

#define GUI_CURRENT_TIME(ss) XtLastTimestampProcessed(MAIN_DISPLAY(ss))

static void channel_expose_callback(Widget w, XtPointer context, XtPointer info)
{
  static TIME_TYPE last_expose_event_time = 0;
  static chan_info *last_cp = NULL;
  snd_info *sp;
  chan_info *cp = (chan_info *)context;
  XmDrawingAreaCallbackStruct *cb = (XmDrawingAreaCallbackStruct *)info;
  XExposeEvent *ev;
  TIME_TYPE curtime;
  ASSERT_WIDGET_TYPE(XmIsDrawingArea(w), w);
  if ((cp == NULL) || (cp->active != 1) || (cp->sound == NULL)) return;
  ev = (XExposeEvent *)(cb->event);
  if (ev->count > 0) return;
  curtime = GUI_CURRENT_TIME(cp->state);
  if ((ev->width < 15) && (last_expose_event_time == curtime) && (cp == last_cp)) return;
  last_cp = cp;
  last_expose_event_time = curtime;
  sp = cp->sound;
  if (sp->channel_style != CHANNELS_SEPARATE)
    {
      if ((cp->chan == 0) && (ev->width > 10) && (ev->height > 10))
	for_each_sound_chan(sp, update_graph);
    }
  else update_graph(cp);
}

static void channel_resize_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_info *sp;
  chan_info *cp = (chan_info *)context;
  if ((cp == NULL) || (cp->active != 1) || (cp->sound == NULL)) return;
  sp = cp->sound;
  if (sp->channel_style != CHANNELS_SEPARATE)
    {
      if (cp->chan == 0)
	for_each_sound_chan(sp, update_graph);
    }
  else update_graph(cp);
}

static XEN mouse_enter_graph_hook;
static XEN mouse_leave_graph_hook;

#define UNPACK_SOUND(a) (a >> 16)
#define UNPACK_CHANNEL(a) (a & 0xff)
#define PACK_SOUND_AND_CHANNEL(a, b) ((a << 16) | b)

static void graph_mouse_enter(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  snd_state *ss = (snd_state *)context;
  int data;
  XtVaGetValues(w, XmNuserData, &data, NULL);
  if (XEN_HOOKED(mouse_enter_graph_hook))
    run_hook(mouse_enter_graph_hook,
	     XEN_LIST_2(C_TO_SMALL_XEN_INT(UNPACK_SOUND(data)),
			C_TO_SMALL_XEN_INT(UNPACK_CHANNEL(data))),
	     S_mouse_enter_graph_hook);
  XDefineCursor(XtDisplay(w), XtWindow(w), (ss->sgx)->graph_cursor);
}

static void graph_mouse_leave(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  int data;
  XtVaGetValues(w, XmNuserData, &data, NULL);
  if (XEN_HOOKED(mouse_leave_graph_hook))
    run_hook(mouse_leave_graph_hook,
	     XEN_LIST_2(C_TO_SMALL_XEN_INT(UNPACK_SOUND(data)),
			C_TO_SMALL_XEN_INT(UNPACK_CHANNEL(data))),
	     S_mouse_leave_graph_hook);
  XUndefineCursor(XtDisplay(w), XtWindow(w));
}

static void graph_button_press(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  XButtonEvent *ev = (XButtonEvent *)event;
  if ((ev->button == 3) && 
      (sound_style(get_global_state()) == SOUNDS_IN_SEPARATE_WINDOWS))
    post_popup(ev);
  else graph_button_press_callback((chan_info *)context, ev->x, ev->y, ev->state, ev->button, ev->time);
}

static void graph_button_release(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  XButtonEvent *ev = (XButtonEvent *)event;
  graph_button_release_callback((chan_info *)context, ev->x, ev->y, ev->state, ev->button);
}

static void graph_button_motion(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{ /* mouse drag */
  XMotionEvent *ev = (XMotionEvent *)event;
#if DEBUGGING
  if (ev->send_event == True)
    /* in this case, we're being driven by scheme-generated fake X events (event.scm) */
    graph_button_motion_callback((chan_info *)context, ev->x_root, ev->y_root, ev->time, 0);
#endif
  graph_button_motion_callback((chan_info *)context, ev->x, ev->y, ev->time, XtGetMultiClickTime(XtDisplay(w)));
}

static int no_padding(Arg *args, int n)
{
  XtSetArg(args[n], XmNmarginHeight, 0); n++;
  XtSetArg(args[n], XmNmarginWidth, 0); n++;
  XtSetArg(args[n], XmNmarginTop, 0); n++;
  XtSetArg(args[n], XmNmarginBottom, 0); n++;
  XtSetArg(args[n], XmNmarginLeft, 0); n++;
  XtSetArg(args[n], XmNmarginRight, 0); n++;
  return(n);
}

static void hide_gz_scrollbars(snd_info *sp)
{
  Widget w;
  w = channel_gsy(sp->chans[0]);
  if ((w) && (XtIsManaged(w))) XtUnmanageChild(w);
  w = channel_gzy(sp->chans[0]);
  if ((w) && (XtIsManaged(w))) XtUnmanageChild(w);
}

static void show_gz_scrollbars(snd_info *sp)
{
  Widget w;
  w = channel_gsy(sp->chans[0]);
  if ((w) && (!XtIsManaged(w))) XtManageChild(w);
  w = channel_gzy(sp->chans[0]);
  if ((w) && (!XtIsManaged(w))) XtManageChild(w);
}


/* edit history support */

#if (XmVERSION > 1)
static void history_select_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* undo/redo to reach selected position */
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  XButtonEvent *ev = NULL;
  ASSERT_WIDGET_TYPE(XmIsList(w), w);
  ev = (XButtonEvent *)(cbs->event);
  edit_select_callback((chan_info *)context, 
		       cbs->item_position - 1, 
		       (ev) ? (ev->state & snd_ControlMask) : 0); /* in auto-test sequences, button event is null */
}
#endif

void reflect_edit_history_change(chan_info *cp)
{
  /* new edit so it is added, and any trailing lines removed */
  chan_context *cx;
  Widget lst;
  snd_info *sp;
  int i, eds, items = 0;
  XmString *edits;
  XmString edit;
#if (XmVERSION == 1)
  if (0)
#endif
    {
      cx = cp->cgx;
      if (cx)
	{
	  lst = EDIT_HISTORY_LIST(cp);
	  if (lst)
	    {
	      eds = cp->edit_ctr;
	      while ((eds < (cp->edit_size - 1)) && (cp->edits[eds + 1])) eds++;
	      if (eds >= 0)
		{
		  if ((eds == cp->edit_ctr) && (eds > 1)) /* need to force 0 (1) case to start list with sound file name */
		    {
		      /* special common case -- we're appending a new edit description */
		      XtVaGetValues(lst, XmNitemCount, &items, NULL);
		      if (items > eds )
			XmListDeleteItemsPos(lst, cp->edit_size, eds + 1);
		      edit = XmStringCreate(edit_to_string(cp, eds), XmFONTLIST_DEFAULT_TAG);
		      XmListAddItemUnselected(lst, edit, eds + 1);
		      XmStringFree(edit);
		    }
		  else
		    {
		      sp = cp->sound;
		      edits = (XmString *)CALLOC(eds + 1, sizeof(XmString));
		      edits[0] = XmStringCreate(sp->filename, XmFONTLIST_DEFAULT_TAG);
		      for (i = 1; i <= eds; i++) 
			edits[i] = XmStringCreate(edit_to_string(cp, i), XmFONTLIST_DEFAULT_TAG);
		      XtVaSetValues(lst, 
				    XmNitems, edits, 
				    XmNitemCount, eds + 1, 
				    NULL);
		      for (i = 0; i <= eds; i++) 
			XmStringFree(edits[i]);
		      FREE(edits);
		    }
		  XmListSelectPos(lst, cp->edit_ctr + 1, FALSE);
		  XtVaGetValues(lst, XmNvisibleItemCount, &items, NULL);
		  if (items <= eds)
		    XtVaSetValues(lst, XmNtopItemPosition, eds - items + 2, NULL);
		  goto_graph(cp);
		}
	    }
	}
    }
}

void reflect_save_as_in_edit_history(chan_info *cp, char *filename)
{
  chan_context *cx;
  Widget lst;
  char *new_line;
  XmString str;
  int pos;
  if (cp->edit_ctr < 1) return; /* Sun segfaults if 0 here! (apparently the usual C library strlen null bug) */
  cx = cp->cgx;
#if (XmVERSION == 1)
  if (0)
#else
  if (cx)
#endif
    {
      lst = EDIT_HISTORY_LIST(cp);
      if (lst)
	{
	  new_line = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
	  mus_snprintf(new_line, PRINT_BUFFER_SIZE,
		       "%s: (save-sound-as \"%s\")", 
		       edit_to_string(cp, cp->edit_ctr), 
		       filename);
	  str = XmStringCreate(new_line, XmFONTLIST_DEFAULT_TAG);
	  pos = cp->edit_ctr + 1;
	  XmListReplacePositions(lst, &pos, &str, 1);
	  XmStringFree(str);
	  FREE(new_line);
	}
    }
}

void reflect_edit_counter_change(chan_info *cp)
{
  /* undo/redo/revert -- change which line is highlighted */
  chan_context *cx;
  Widget lst;
  int len, top;
#if (XmVERSION == 1)
  if (0)
#endif
    {
      cx = cp->cgx;
      if (cx)
	{
	  lst = EDIT_HISTORY_LIST(cp);
	  if (lst)
	    {
	      XmListSelectPos(lst, cp->edit_ctr + 1, FALSE);
	      XtVaGetValues(lst, 
			    XmNvisibleItemCount, &len, 
			    XmNtopItemPosition, &top, 
			    NULL);
	      if ((cp->edit_ctr + 1) < top) 
		XtVaSetValues(lst, XmNtopItemPosition, cp->edit_ctr + 1, NULL);
	      else
		if ((cp->edit_ctr + 1) >= (top + len))
		  XtVaSetValues(lst, XmNtopItemPosition, cp->edit_ctr, NULL);
	      goto_graph(cp);
	    }
	}
    }
}

static void add_drop(snd_state *ss, Widget drawer)
{
  /* called via startup func */
  int n;
  Atom FILE_NAME;
  Atom targets[2];
  Arg args[12];
  targets[0] = XA_STRING;
  FILE_NAME = XInternAtom(MAIN_DISPLAY(ss), "FILE_NAME", FALSE);
  targets[1] = FILE_NAME;
  n = 0;
  XtSetArg(args[n], XmNdropSiteOperations, XmDROP_COPY); n++;
  XtSetArg(args[n], XmNimportTargets, targets); n++;
  XtSetArg(args[n], XmNnumImportTargets, 2); n++;
  XtSetArg(args[n], XmNdropProc, handle_drop); n++;
  XmDropSiteRegister(drawer, args, n);
}

static void cp_graph_key_press(Widget w, XtPointer context, XEvent *event, Boolean *cont);

void add_channel_window(snd_info *sp, int channel, snd_state *ss, int chan_y, int insertion, Widget main, int button_style)
{
  Widget *cw;
  Widget left_widget = NULL;
  XtCallbackList n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14;
  chan_info *cp;
  chan_context *cx;
  axis_context *cax;
  state_context *sx;
  int make_widgets, i, n, need_colors, need_extra_scrollbars;
  Arg args[32];
  make_widgets = ((sp->chans[channel]) == NULL);
  sp->chans[channel] = make_chan_info(sp->chans[channel], channel, sp, ss);
  cp = sp->chans[channel];
  cx = cp->cgx;
  if (cx->chan_widgets == NULL) 
    cx->chan_widgets = (Widget *)CALLOC(NUM_CHAN_WIDGETS, sizeof(Widget));
  cw = cx->chan_widgets;
  sx = ss->sgx;
  need_extra_scrollbars = ((!main) && (channel == 0));

  if (make_widgets)
    {
      /* allocate the entire widget apparatus for this channel of this sound */
      need_colors = (!(ss->using_schemes));

      if (!main)
	{
	  n = 0;
	  if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
	  if (insertion) {XtSetArg(args[n], XmNpositionIndex, (short)channel); n++;}
	  XtSetArg(args[n], XmNpaneMinimum, chan_y); n++;
#if (XmVERSION > 1)
	  cw[W_form] = XtCreateManagedWidget("chn-form", xmFormWidgetClass, w_snd_pane(sp), args, n);
	  if ((sp->channel_style == CHANNELS_COMBINED) && (channel > 0)) XtUnmanageChild(cw[W_form]);

	  n = 0;
	  if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
	  n = no_padding(args, n);
	  n = attach_all_sides(args, n);
	  XtSetArg(args[n], XmNsashIndent, 2); n++;
	  XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
	  cw[W_top] = XtCreateManagedWidget("chn-main-window", xmPanedWindowWidgetClass, cw[W_form], args, n);
	  XtAddEventHandler(cw[W_top], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
#else
	  cw[W_main_window] = XtCreateManagedWidget("chn-main-window", xmFormWidgetClass, w_snd_pane(sp), args, n);
	  XtAddEventHandler(cw[W_main_window], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
#endif

#if (XmVERSION > 1)
	  n = 0;
	  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->white); n++;}
	  XtSetArg(args[n], XmNpaneMaximum, DEFAULT_EDIT_HISTORY_WIDTH); n++;
	  XtSetArg(args[n], XmNlistSizePolicy, XmCONSTANT); n++;
	  cw[W_edhist] = XmCreateScrolledList(cw[W_top], "chn-edhist", args, n);

	  n = 0;
	  if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
	  XtSetArg(args[n], XmNpaneMaximum, LOTSA_PIXELS); n++;
	  cw[W_main_window] = XtCreateManagedWidget("chn-main-window", xmFormWidgetClass, cw[W_top], args, n);
	  XtAddEventHandler(cw[W_main_window], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
	  /* left_widget = cw[W_main_window]; */
	  XtManageChild(cw[W_edhist]);

	  XtAddCallback(cw[W_edhist], XmNbrowseSelectionCallback, history_select_callback, cp);
	  XtAddCallback(cw[W_edhist], XmNhelpCallback, history_help_callback, ss);
	  XtAddEventHandler(cw[W_edhist], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
	  XtAddEventHandler(XtParent(cw[W_edhist]), KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
#endif
	}
      else cw[W_main_window] = main;

      n = 0;  
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      if (left_widget)
	{
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNleftWidget, left_widget); n++;
	}
      else
	{
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
	}
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      n = no_padding(args, n);
      XtSetArg(args[n], XmNpacking, XmPACK_COLUMN); n++;
      XtSetArg(args[n], XmNnumColumns, 1); n++;
      XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
      cw[W_wf_buttons] = XtCreateManagedWidget("chn-buttons", xmRowColumnWidgetClass, cw[W_main_window], args, n);	

      if (button_style == WITH_FW_BUTTONS)
	{
	  n = 0;
	  if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
	  XtSetArg(args[n], XM_FONT_RESOURCE, BUTTON_FONT(ss)); n++;
	  XtSetArg(args[n], XmNspacing, 1); n++;
	  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNselectColor, sx->pushed_button_color); n++;}
	  cw[W_f] = make_togglebutton_widget(STR_f, cw[W_wf_buttons], args, n);
	  XtAddCallback(cw[W_f], XmNvalueChangedCallback, f_toggle_callback, cp);
	  XtAddCallback(cw[W_f], XmNhelpCallback, f_button_help_callback, ss);
	  XtAddEventHandler(cw[W_f], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
	  
	  XtSetArg(args[n], XmNset, TRUE); n++;
	  cw[W_w] = make_togglebutton_widget(STR_w, cw[W_wf_buttons], args, n);
	  XtAddCallback(cw[W_w], XmNvalueChangedCallback, w_toggle_callback, cp);
	  XtAddCallback(cw[W_w], XmNhelpCallback, w_button_help_callback, ss);
	  XtAddEventHandler(cw[W_w], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
	}
      else
	{
	  n = 0;
	  if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
	  XtSetArg(args[n], XmNarrowDirection, XmARROW_UP); n++;
	  XtSetArg(args[n], XmNsensitive, FALSE); n++;
	  cw[W_f] = XtCreateManagedWidget("up", xmArrowButtonWidgetClass, cw[W_wf_buttons], args, n);

	  n = 0;
	  if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
	  XtSetArg(args[n], XmNarrowDirection, XmARROW_DOWN); n++;
	  XtSetArg(args[n], XmNsensitive, FALSE); n++;
	  cw[W_w] = XtCreateManagedWidget("down", xmArrowButtonWidgetClass, cw[W_wf_buttons], args, n);
	}

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, cw[W_wf_buttons]); n++;
      if (left_widget)
	{
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNleftWidget, left_widget); n++;
	}
      else
	{
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
	}
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNspacing, 0); n++;
      cw[W_left_scrollers] = XtCreateManagedWidget("chn-left", xmRowColumnWidgetClass, cw[W_main_window], args, n);
      XtAddEventHandler(cw[W_left_scrollers], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->zoom_color); n++;}
      XtSetArg(args[n], XmNwidth, ss->position_slider_width); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++; 
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++; 
      XtSetArg(args[n], XmNincrement, 1); n++;
      XtSetArg(args[n], XmNprocessingDirection, XmMAX_ON_TOP); n++;
      XtSetArg(args[n], XmNdragCallback, n1 = make_callback_list(zy_drag_callback, (XtPointer)cp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n2 = make_callback_list(zy_valuechanged_callback, (XtPointer)cp)); n++;
      cw[W_zy] = XtCreateManagedWidget("chn-zy", xmScrollBarWidgetClass, cw[W_left_scrollers], args, n);
      XtAddCallback(cw[W_zy], XmNhelpCallback, zy_help_callback, ss);
      XtAddEventHandler(cw[W_zy], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->position_color); n++;}
      XtSetArg(args[n], XmNwidth, ss->zoom_slider_width); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, cw[W_zy]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNincrement, 1); n++;
      XtSetArg(args[n], XmNprocessingDirection, XmMAX_ON_TOP); n++;
      XtSetArg(args[n], XmNdragCallback, n3 = make_callback_list(sy_drag_callback, (XtPointer)cp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n4 = make_callback_list(sy_valuechanged_callback, (XtPointer)cp)); n++;
      cw[W_sy] = XtCreateManagedWidget("chn-sy", xmScrollBarWidgetClass, cw[W_left_scrollers], args, n);
      XtAddCallback(cw[W_sy], XmNhelpCallback, sy_help_callback, ss);
      XtAddEventHandler(cw[W_sy], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, cw[W_wf_buttons]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNspacing, 0); n++;
      cw[W_bottom_scrollers] = XtCreateManagedWidget("chn-bottom", xmRowColumnWidgetClass, cw[W_main_window], args, n);
      XtAddEventHandler(cw[W_bottom_scrollers], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->position_color); n++;}
      XtSetArg(args[n], XmNheight, ss->position_slider_width); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_SX_MAX); n++;
      XtSetArg(args[n], XmNincrement, 1); n++;
      XtSetArg(args[n], XmNdragCallback, n5 = make_callback_list(sx_drag_callback, (XtPointer)cp)); n++;
      XtSetArg(args[n], XmNincrementCallback, n6 = make_callback_list(sx_increment_callback, (XtPointer)cp)); n++;
      XtSetArg(args[n], XmNdecrementCallback, n7 = make_callback_list(sx_decrement_callback, (XtPointer)cp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n8 = make_callback_list(sx_valuechanged_callback, (XtPointer)cp)); n++;
      cw[W_sx] = XtCreateManagedWidget("chn-sx", xmScrollBarWidgetClass, cw[W_bottom_scrollers], args, n);
      XtAddCallback(cw[W_sx], XmNhelpCallback, sx_help_callback, ss);
      XtAddEventHandler(cw[W_sx], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->zoom_color); n++;}
      XtSetArg(args[n], XmNheight, ss->zoom_slider_width + 2); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, cw[W_sx]); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNincrement, 1); n++;
      XtSetArg(args[n], XmNdragCallback, n9 = make_callback_list(zx_drag_callback, (XtPointer)cp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n10 = make_callback_list(zx_valuechanged_callback, (XtPointer)cp)); n++;
      cw[W_zx] = XtCreateManagedWidget("chn-zx", xmScrollBarWidgetClass, cw[W_bottom_scrollers], args, n);
      XtAddCallback(cw[W_zx], XmNhelpCallback, zx_help_callback, ss);
      XtAddEventHandler(cw[W_zx], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);

      n = 0;
      if (need_colors) 
	{
	  XtSetArg(args[n], XmNbackground, sx->graph_color); n++;
	  XtSetArg(args[n], XmNforeground, sx->data_color); n++;
	  /* XtSetArg(args[n], XmNbackgroundPixmap, XmGetPixmap(XtScreen(SOUND_PANE(ss)), "text.xpm", sx->basic_color, sx->graph_color)); n++; */
	  /* how to include these in the resource list?  can we use png files here? */
	  /* ./snd oboe.snd -xrm '*chn-graph*backgroundPixmap: text.xpm' */
	}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, cw[W_bottom_scrollers]); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, cw[W_left_scrollers]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNuserData, PACK_SOUND_AND_CHANNEL(sp->index, cp->chan)); n++;
      /* this collides with W_gzy below, but a consistent version came up with half a window blank */
      XtSetArg(args[n], XmNnavigationType, XmNONE); n++;
      cw[W_graph] = XtCreateManagedWidget("chn-graph", xmDrawingAreaWidgetClass, cw[W_main_window], args, n);

      if (!main)
	{
	  XtAddCallback(cw[W_graph], XmNhelpCallback, graph_help_callback, ss);
	  XtAddCallback(cw[W_graph], XmNresizeCallback, channel_resize_callback, (XtPointer)cp);
	  XtAddCallback(cw[W_graph], XmNexposeCallback, channel_expose_callback, (XtPointer)cp);
	  XtAddEventHandler(cw[W_graph], EnterWindowMask, FALSE, graph_mouse_enter, (XtPointer)ss);
	  XtAddEventHandler(cw[W_graph], LeaveWindowMask, FALSE, graph_mouse_leave, (XtPointer)cp);
	  XtAddEventHandler(cw[W_graph], ButtonPressMask, FALSE, graph_button_press, (XtPointer)cp);
	  XtAddEventHandler(cw[W_graph], ButtonMotionMask, FALSE, graph_button_motion, (XtPointer)cp);
	  XtAddEventHandler(cw[W_graph], ButtonReleaseMask, FALSE, graph_button_release, (XtPointer)cp);
	  XtAddEventHandler(cw[W_graph], KeyPressMask, FALSE, cp_graph_key_press, (XtPointer)cp);
	}
      FREE(n1);
      FREE(n2);
      FREE(n3);
      FREE(n4);
      FREE(n5);
      FREE(n6);
      FREE(n7);
      FREE(n8);
      FREE(n9);
      FREE(n10);
      add_drop(ss, cw[W_graph]);

      if (need_extra_scrollbars)
	{
	  /* that is: not region browser chan, might need combined graph, channel 0 is the controller in that case */
	  /* this is independent of sp->nchans because these structs are re-used and added to as needed */
	  n = 0;
	  if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->zoom_color); n++;}
	  XtSetArg(args[n], XmNwidth, ss->position_slider_width); n++;
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
	  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNbottomWidget, cw[W_bottom_scrollers]); n++;
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
	  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
	  XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
	  XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++; 
	  XtSetArg(args[n], XmNincrement, 1); n++;
	  XtSetArg(args[n], XmNprocessingDirection, XmMAX_ON_TOP); n++;
	  XtSetArg(args[n], XmNdragCallback, n11 = make_callback_list(gzy_drag_callback, (XtPointer)cp)); n++;
	  XtSetArg(args[n], XmNvalueChangedCallback, n12 = make_callback_list(gzy_valuechanged_callback, (XtPointer)cp)); n++;
	  cw[W_gzy] = XtCreateManagedWidget("chn-gzy", xmScrollBarWidgetClass, cw[W_main_window], args, n);
	  XtAddCallback(cw[W_gzy], XmNhelpCallback, gzy_help_callback, ss);
	  XtAddEventHandler(cw[W_gzy], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);

	  n = 0;
	  if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->position_color); n++;}
	  XtSetArg(args[n], XmNwidth, ss->position_slider_width); n++;
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
	  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNbottomWidget, cw[W_bottom_scrollers]); n++;
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
	  XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNrightWidget, cw[W_gzy]); n++;
	  XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
	  XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
	  XtSetArg(args[n], XmNincrement, 1); n++;
	  XtSetArg(args[n], XmNprocessingDirection, XmMAX_ON_TOP); n++;
	  XtSetArg(args[n], XmNdragCallback, n13 = make_callback_list(gsy_drag_callback, (XtPointer)cp)); n++;
	  XtSetArg(args[n], XmNvalueChangedCallback, n14 = make_callback_list(gsy_valuechanged_callback, (XtPointer)cp)); n++;
	  cw[W_gsy] = XtCreateManagedWidget("chn-gsy", xmScrollBarWidgetClass, cw[W_main_window], args, n);
	  XtAddCallback(cw[W_gsy], XmNhelpCallback, gsy_help_callback, ss);
	  XtAddEventHandler(cw[W_gsy], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
	  
	  FREE(n11);
	  FREE(n12);
	  FREE(n13);
	  FREE(n14);

	  n = 0;
	}
      else
	{
	  cw[W_gsy] = NULL;
	  cw[W_gzy] = NULL;
	}
      run_new_widget_hook(cw[W_main_window]);

      /* also position of current graph in overall sound as window */

    } /* alloc new chan */
  else
    { /* re-manage currently inactive chan */
      XtVaSetValues(cw[W_main_window], XmNpaneMinimum, chan_y, NULL);
#if (XmVERSION > 1)
      if (cw[W_edhist]) XtVaSetValues(XtParent(cw[W_edhist]), XmNpaneMaximum, 1, NULL);
#endif
      if ((sp->channel_style != CHANNELS_COMBINED) || (channel == 0))
	for (i = 0; i < NUM_CHAN_WIDGETS; i++)
	  if (cw[i])
	    if  (!XtIsManaged(cw[i])) 
	      XtManageChild(cw[i]);
      recolor_graph(cp, FALSE); /* in case selection color left over from previous use */
    }
#if (XmVERSION > 1)
  if (cw[W_edhist]) 
    XtVaSetValues(XtParent(cw[W_edhist]), XmNpaneMaximum, LOTSA_PIXELS, NULL);
#endif
  if ((need_extra_scrollbars) && (sp->channel_style == CHANNELS_SEPARATE)) 
    hide_gz_scrollbars(sp); /* default is on in this case */  
  cax = cx->ax;
  cax->wn = XtWindow(cw[W_graph]);
  cax->dp = XtDisplay(cw[W_graph]);
  cax->gc = sx->basic_gc;
}

static void set_graph_font(chan_info *cp, XFontStruct *bf)
{
  chan_context *cx;
  cx = cp->tcgx;
  if (!cx) cx = cp->cgx;
  cx->ax->current_font = bf->fid;  
  XSetFont(XtDisplay(cx->chan_widgets[W_graph]), copy_GC(cp), bf->fid);
}

void set_peak_numbers_font(chan_info *cp) {set_graph_font(cp, PEAK_NUMBERS_FONT(cp->state));}
void set_tiny_numbers_font(chan_info *cp) {set_graph_font(cp, TINY_NUMBERS_FONT(cp->state));}
void set_bold_peak_numbers_font(chan_info *cp) {set_graph_font(cp, BOLD_PEAK_NUMBERS_FONT(cp->state));}

COLOR_TYPE get_foreground_color(chan_info *cp, axis_context *ax)
{
  XGCValues gv;
  snd_state *ss;
  ss = cp->state;
  XGetGCValues(MAIN_DISPLAY(ss), ax->gc, GCForeground, &gv);
  return(gv.foreground);
}

COLOR_TYPE get_background_color(chan_info *cp, axis_context *ax)
{
  XGCValues gv;
  snd_state *ss;
  ss = cp->state;
  XGetGCValues(MAIN_DISPLAY(ss), ax->gc, GCBackground, &gv);
  return(gv.background);
}

void set_foreground_color(chan_info *cp, axis_context *ax, Pixel color)
{
  snd_state *ss;
  ss = cp->state;
  XSetForeground(MAIN_DISPLAY(ss), ax->gc, color);
}

GC copy_GC(chan_info *cp)
{
  state_context *sx;
  sx = (cp->state)->sgx;
  if ((cp->cgx)->selected) return(sx->selected_basic_gc);
  return(sx->basic_gc);
}

GC erase_GC(chan_info *cp)
{
  /* used only to clear partial bgs in chan graphs */
  state_context *sx;
  snd_info *sp;
  snd_state *ss;
  ss = cp->state;
  sp = cp->sound;
  sx = ss->sgx;
  if (((cp->cgx)->selected) ||
      ((sp) && 
       (sp->channel_style == CHANNELS_SUPERIMPOSED) && 
       (sp->index == ss->selected_sound)))
    return(sx->selected_erase_gc);
  return(sx->erase_gc);
}

/* for combined cases, the incoming chan_info pointer is always chan[0], 
 * but the actual channel depends on placement if mouse oriented.
 * virtual_selected_channel(cp) (snd-chn.c) retains the current selected channel
 */

void graph_key_press(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  XKeyEvent *ev = (XKeyEvent *)event;
  KeySym keysym;
  int key_state;
  snd_info *sp = (snd_info *)context;
  key_state = ev->state;
  keysym = XKeycodeToKeysym(XtDisplay(w),
			    (int)(ev->keycode),
			    (key_state & ShiftMask) ? 1 : 0);
  key_press_callback(any_selected_channel(sp), ev->x, ev->y, ev->state, keysym);
}
 
static void cp_graph_key_press(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  /* called by every key-intercepting widget in the entire sound pane */
  XKeyEvent *ev = (XKeyEvent *)event;
  KeySym keysym;
  int key_state;
  chan_info *cp = (chan_info *)context;
  if ((cp == NULL) || (cp->sound == NULL))
    {
#if DEBUGGING
      fprintf(stderr, "unconnected chan in key press?");
      abort();
#endif
      return;
    }
  key_state = ev->state;
  keysym = XKeycodeToKeysym(XtDisplay(w),
			    (int)(ev->keycode),
			    (key_state & ShiftMask) ? 1 : 0);
  key_press_callback(cp, ev->x, ev->y, ev->state, keysym);
}
 
void cleanup_cw(chan_info *cp)
{
  chan_context *cx;
  Widget *cw;
  if ((cp) && (cp->cgx))
    {
      cx = cp->cgx;
      cx->selected = 0;
      cw = cx->chan_widgets;
      if (cw)
	{
	  if (cw[W_w])
	    {
	      XtVaSetValues(cw[W_w], XmNset, TRUE, NULL);
	      XtVaSetValues(cw[W_f], XmNset, FALSE, NULL);
	    }
	  XtUnmanageChild(channel_main_pane(cp));
	}
    }
}

void change_channel_style(snd_info *sp, int new_style)
{
  int i, j, old_style;
  snd_state *ss;
  chan_info *ncp, *cp, *pcp;
  int height[1];
  chan_context *mcgx;
  Widget *cw;
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
	      syncb(sp, sp->previous_sync);
	      XtVaSetValues(unite_button(sp), XmNselectColor, (ss->sgx)->pushed_button_color, NULL);
	    }
	  else
	    {
	      if (new_style == CHANNELS_SUPERIMPOSED)
		{
		  sp->previous_sync = sp->sync;
		  if (sp->sync == 0) syncb(sp, 1);
		  XtVaSetValues(unite_button(sp), XmNselectColor, (ss->sgx)->green, NULL);
		  apply_y_axis_change((sp->chans[0])->axis, sp->chans[0]);
		  apply_x_axis_change((sp->chans[0])->axis, sp->chans[0], sp);
		}
	    }
	  height[0] = widget_height(w_snd_pane(sp)) - control_panel_height(sp) - 16;
	  if (old_style == CHANNELS_SEPARATE)
	    {
	      ncp = sp->chans[0];
	      sound_lock_control_panel(sp, NULL);
	      channel_lock_pane(ncp, height);
	      mcgx = ncp->cgx;
	      for (i = 1; i < sp->nchans; i++) 
		{
		  ncp = sp->chans[i];
		  cleanup_cw(ncp);
		  ncp->tcgx = mcgx;
		  reset_mix_graph_parent(ncp);
		}
	      channel_open_pane(sp->chans[0], NULL);
	      channel_unlock_pane(sp->chans[0], NULL);
	      sound_unlock_control_panel(sp, NULL);
	      XmToggleButtonSetState(unite_button(sp), TRUE, FALSE);
	    }
	  else
	    {
	      if (new_style == CHANNELS_SEPARATE)
		{
		  /* height[0] = total space available */
		  height[0] /= sp->nchans;
		  sound_lock_control_panel(sp, NULL);
		  map_over_sound_chans(sp, channel_lock_pane, (void *)height);
		  map_over_sound_chans(sp, channel_open_pane, NULL);
		  map_over_sound_chans(sp, channel_unlock_pane, NULL);
		  sound_unlock_control_panel(sp, NULL);
		  for (i = 0; i < sp->nchans; i++) 
		    reset_mix_graph_parent(sp->chans[i]);
		  pcp = sp->chans[0];
		  ap = pcp->axis;
		  for (i = 1; i < sp->nchans; i++)
		    {
		      cp = sp->chans[i];
		      cp->tcgx = NULL;
		      cx = cp->cgx;
		      cw = cx->chan_widgets;
#if (XmVERSION > 1)
		      if (cw[W_edhist]) 
			XtVaSetValues(XtParent(cw[W_edhist]), XmNpaneMaximum, DEFAULT_EDIT_HISTORY_WIDTH, NULL);
#endif
		      for (j = 0; j < NUM_CHAN_WIDGETS; j++)
			{
			  if ((cw[j]) && (!XtIsManaged(cw[j]))) 
			    XtManageChild(cw[j]);
			}
#if (XmVERSION > 1)
		      if (cw[W_edhist]) 
			XtVaSetValues(XtParent(cw[W_edhist]), XmNpaneMaximum, LOTSA_PIXELS, NULL);
#endif
		      XmToggleButtonSetState(cw[W_f], cp->graph_transform_p, FALSE);
		      XmToggleButtonSetState(cw[W_w], cp->graph_time_p, FALSE);
		      /* these can get out of sync if changes are made in the unseparated case */
		      set_axes(cp, ap->x0, ap->x1, ap->y0, ap->y1);
		    }
		  XmToggleButtonSetState(unite_button(sp), FALSE, FALSE);
		}
	    }
	}
    }
}


int fixup_cp_cgx_ax_wn(chan_info *cp) 
{
  ((cp->cgx)->ax)->wn = XtWindow((cp->cgx)->chan_widgets[W_graph]); 
  return(1);
}

static XEN g_channel_widgets(XEN snd, XEN chn)
{
  #define H_channel_widgets "(" S_channel_widgets " snd chn) -> list of widgets ((0)graph (1)w (2)f (3)sx (4)sy (5)zx (6)zy (7)edhist)"
  chan_info *cp;
  ASSERT_CHANNEL(S_channel_widgets, snd, chn, 1);
  cp = get_cp(snd, chn, S_channel_widgets);
  return(XEN_CONS(XEN_WRAP_WIDGET(channel_graph(cp)),
	   XEN_CONS(XEN_WRAP_WIDGET(channel_w(cp)),
	     XEN_CONS(XEN_WRAP_WIDGET(channel_f(cp)),
	       XEN_CONS(XEN_WRAP_WIDGET(channel_sx(cp)),
	         XEN_CONS(XEN_WRAP_WIDGET(channel_sy(cp)),
	           XEN_CONS(XEN_WRAP_WIDGET(channel_zx(cp)),
	             XEN_CONS(XEN_WRAP_WIDGET(channel_zy(cp)),
		       XEN_CONS(XEN_WRAP_WIDGET(EDIT_HISTORY_LIST(cp)),
			 XEN_CONS(XEN_WRAP_WIDGET(channel_gsy(cp)),
			   XEN_CONS(XEN_WRAP_WIDGET(channel_gzy(cp)),
			     XEN_CONS(XEN_WRAP_WIDGET(channel_main_pane(cp)),
	                       XEN_EMPTY_LIST))))))))))));
}


#ifdef XEN_ARGIFY_1
XEN_ARGIFY_2(g_channel_widgets_w, g_channel_widgets)
#else
#define g_channel_widgets_w g_channel_widgets
#endif

void g_init_gxchn(void)
{
  XEN_DEFINE_PROCEDURE(S_channel_widgets, g_channel_widgets_w, 0, 2, 0, H_channel_widgets);

  #define H_mouse_enter_graph_hook S_mouse_enter_graph_hook " (snd chn) is called when the mouse \
enters the drawing area (graph pane) of the given channel.\n\
  (add-hook! mouse-enter-graph-hook\n\
    (lambda (snd chn)\n\
      (focus-widget (car (channel-widgets snd chn)))))"

  #define H_mouse_leave_graph_hook S_mouse_leave_graph_hook " (snd chn) is called when the mouse \
leaves the drawing area (graph pane) of the given channel."

  XEN_DEFINE_HOOK(mouse_enter_graph_hook, S_mouse_enter_graph_hook, 2, H_mouse_enter_graph_hook);    /* args = snd chn */
  XEN_DEFINE_HOOK(mouse_leave_graph_hook, S_mouse_leave_graph_hook, 2, H_mouse_leave_graph_hook);    /* args = snd chn */
}
