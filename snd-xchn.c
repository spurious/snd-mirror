#include "snd.h"

enum {
    W_top,W_form,
    W_main_window,
    W_edhist,
    W_wf_buttons,
      W_f,W_w,
    W_left_scrollers,
      W_zy,W_sy,
    W_bottom_scrollers,
      W_sx,W_zx,
    W_graph,
      W_gzy,W_gsy
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

Widget channel_graph(chan_info *cp) {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_widgets[W_graph]); else return(NULL);}
Widget channel_sx(chan_info *cp) {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_widgets[W_sx]); else return(NULL);}
Widget channel_sy(chan_info *cp) {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_widgets[W_sy]); else return(NULL);}
Widget channel_zx(chan_info *cp) {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_widgets[W_zx]); else return(NULL);}
Widget channel_zy(chan_info *cp) {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_widgets[W_zy]); else return(NULL);}
static Widget channel_gsy(chan_info *cp) {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_widgets[W_gsy]); else return(NULL);}
static Widget channel_gzy(chan_info *cp) {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_widgets[W_gzy]); else return(NULL);}
Widget channel_w(chan_info *cp) {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_widgets[W_w]); else return(NULL);}
Widget channel_f(chan_info *cp) {if ((cp) && (cp->cgx)) return((cp->cgx)->chan_widgets[W_f]); else return(NULL);}

static Float get_scrollbar(Widget w,int val, int scrollbar_max)
{
  int size;
  if (val == 0) return(0.0);
  XtVaGetValues(w,XmNsliderSize,&size,NULL);
  return((Float)val/(Float)(scrollbar_max-size));
}

static void sy_changed(int value,chan_info *cp)
{
  axis_info *ap;
  Float low;
  ap = cp->axis;
  low = get_scrollbar(channel_sy(cp),value,SCROLLBAR_SY_MAX);
  ap->sy = (1.0-ap->zy)*low;
  apply_y_axis_change(ap,cp);
}

static void sx_changed(int value,chan_info *cp)
{
  /* treat as centered with non-slider trough as defining current bounds */
  axis_info *ap;
  snd_info *sp;
  Float low;
  ap = cp->axis;
  sp = cp->sound;
  low = get_scrollbar(channel_sx(cp),value,sp->sx_scroll_max);
  ap->sx = low*(1.0-ap->zx);
  apply_x_axis_change(ap,cp,sp);
}

static void zy_changed(int value,chan_info *cp)
{ 
  axis_info *ap;
  Float old_zy;
  ap = cp->axis;
  if (value < 1) value = 1;
  old_zy = ap->zy;
  ap->zy = sqr(get_scrollbar(channel_zy(cp),value,SCROLLBAR_MAX));
  ap->sy += (.5*(old_zy - ap->zy)); /* try to keep wave centered */
  if (ap->sy < 0) ap->sy = 0;
  apply_y_axis_change(ap,cp);
  resize_sy(cp);
}

#define X_RANGE_CHANGEOVER 20.0

static void zx_changed(int value,chan_info *cp)
{ /* scrollbar change */
  axis_info *ap;
  snd_info *sp;
  snd_state *ss;
  sp = cp->sound;
  ss = cp->state;
  ap = cp->axis;
  if (ap->xmax == 0.0) return;
  if (ap->xmax <= ap->xmin) ap->xmax = ap->xmin+.001;
  if (value < 1) value = 1;
  if ((ap->xmax-ap->xmin) < X_RANGE_CHANGEOVER)
    ap->zx = sqr(get_scrollbar(channel_zx(cp),value,SCROLLBAR_MAX));
  else ap->zx = cube(get_scrollbar(channel_zx(cp),value,SCROLLBAR_MAX));
  /* if cursor visible, focus on that, else selection, else mark, else left side */
  focus_x_axis_change(ap,cp,sp,zoom_focus_style(ss));
  resize_sx(cp);
}

void set_zx_scrollbar_value(chan_info *cp, Float value)
{
  XtVaSetValues(channel_zx(cp),XmNvalue,(int)(value*SCROLLBAR_MAX),NULL);
}

static void set_scrollbar(Widget w,Float position,Float range, int scrollbar_max) /* position and range 0 to 1.0 */
{
  int size,val;
  size = (int)(scrollbar_max * range);
  if (size > scrollbar_max) size = scrollbar_max; /* this can't happen!?! */
  if (size < 1) size = 1;
  val = (int)(scrollbar_max * position);
  if ((val+size)>scrollbar_max) val=scrollbar_max-size;
  if (val < 0) val = 0;
  /* snd_error("%s %f %f to %d %d\n",XtName(w),position,range,val,size); */
  /* in Linux this occasionally generates bogus complaints about slide size and value */
  /* since the values going in are completely legal, I'll not try to kludge around the bug */
  XtVaSetValues(w,XmNsliderSize,size,XmNvalue,val,NULL);
}

static void gzy_changed(int value,chan_info *cp)
{
  Float chan_frac,new_gsy,new_size;
  cp->gzy = get_scrollbar(channel_gzy(cp),value,SCROLLBAR_MAX);
  chan_frac = 1.0 / ((Float)(((snd_info *)(cp->sound))->nchans));
  new_size = chan_frac + ((1.0-chan_frac)*cp->gzy);
  if ((cp->gsy+new_size) > 1.0) new_gsy = 1.0-new_size; else new_gsy = cp->gsy;
  if (new_gsy < 0.0) new_gsy = 0.0;
  set_scrollbar(channel_gsy(cp),new_gsy,new_size,SCROLLBAR_MAX);
  map_over_sound_chans(cp->sound,update_graph,NULL);
}

static void gsy_changed(int value,chan_info *cp)
{
  Float low;
  low = get_scrollbar(channel_gsy(cp),value,SCROLLBAR_MAX);
  cp->gsy = (1.0-cp->gzy)*low;
  map_over_sound_chans(cp->sound,update_graph,NULL);
}

void fixup_gsy(chan_info *cp, Float low, Float high)
{
  Widget wcp;
  int ival;
  Float val,size;
  wcp = channel_gsy(cp);
  XtVaGetValues(wcp,XmNvalue,&ival,NULL);
  val = (Float)ival/(Float)(SCROLLBAR_MAX);
  XtVaGetValues(wcp,XmNsliderSize,&ival,NULL);
  size = (Float)ival/(Float)(SCROLLBAR_MAX);
  if ((val > low) || ((val+size) < high))
    {
      val = low;
      if ((val+size) > 1.0) val = 1.0 - size;
      ival = (int)(val*SCROLLBAR_MAX);
      XtVaSetValues(wcp,XmNvalue,ival,NULL);
      gsy_changed((int)(val*SCROLLBAR_MAX),cp);
    }
}

Float gsy_value(chan_info *cp)
{
  Widget wcp;
  int ival;
  wcp = channel_gsy(cp);
  XtVaGetValues(wcp,XmNvalue,&ival,NULL);
  return((Float)ival/(Float)(SCROLLBAR_MAX));
}

Float gsy_size(chan_info *cp)
{
  Widget wcp;
  int ival;
  wcp = channel_gsy(cp);
  XtVaGetValues(wcp,XmNsliderSize,&ival,NULL);
  return((Float)ival/(Float)(SCROLLBAR_MAX));
}

void initialize_scrollbars(chan_info *cp)
{
  axis_info *ap;
  snd_info *sp;
  ap = cp->axis;
  sp = cp->sound;
  set_scrollbar(channel_sx(cp),ap->sx,ap->zx,sp->sx_scroll_max);
  set_scrollbar(channel_sy(cp),ap->sy,ap->zy,SCROLLBAR_SY_MAX);
  if ((ap->xmax-ap->xmin) < X_RANGE_CHANGEOVER)
    set_scrollbar(channel_zx(cp),sqrt(ap->zx),.1,SCROLLBAR_MAX);  /* assume size is 10% of scrollbar length */
  else set_scrollbar(channel_zx(cp),pow(ap->zx,.333),.1,SCROLLBAR_MAX);
  set_scrollbar(channel_zy(cp),ap->zy,.1,SCROLLBAR_MAX);          /* assume 1.0 here so sqrt/cube decision, if any, is not needed */
  if ((sp->nchans > 1) && (cp->chan == 0) && (channel_gsy(cp)))
    {
      set_scrollbar(channel_gsy(cp),cp->gsy,cp->gzy,SCROLLBAR_MAX);
      set_scrollbar(channel_gzy(cp),cp->gzy,1.0/(Float)(sp->nchans),SCROLLBAR_MAX);
    }
}

void resize_sy(chan_info *cp)
{
  /* something changed the y axis view, so the scale scroller needs to reflect that change (in size and position) */
  axis_info *ap;
  ap = cp->axis;
  set_scrollbar(channel_sy(cp),(ap->y0-ap->ymin)/(ap->ymax-ap->ymin),(ap->y1-ap->y0)/(ap->ymax-ap->ymin),SCROLLBAR_SY_MAX);
}

void resize_sx(chan_info *cp)
{
  axis_info *ap;
  snd_info *sp;
  ap = cp->axis;
  sp = cp->sound;
  set_scrollbar(channel_sx(cp),
		(ap->x0-ap->xmin)/(ap->xmax-ap->xmin),
		(ap->x1-ap->x0)/(ap->xmax-ap->xmin),
		sp->sx_scroll_max);
}

void resize_zx(chan_info *cp)
{
  axis_info *ap;
  ap = cp->axis;
  if ((ap->xmax-ap->xmin) < X_RANGE_CHANGEOVER)
    set_scrollbar(channel_zx(cp),sqrt(ap->zx)*.9,.1,SCROLLBAR_MAX);
  else set_scrollbar(channel_zx(cp),pow(ap->zx*.9,1.0/3.0),.1,SCROLLBAR_MAX);
}

void resize_zy(chan_info *cp)
{
  axis_info *ap;
  ap = cp->axis;
  set_scrollbar(channel_zy(cp),sqrt(ap->zy)*.9,.1,SCROLLBAR_MAX);
}


int channel_open_pane(chan_info *cp, void *ptr)
{
  XtManageChild(channel_main_pane(cp));
  return(0);
}

int channel_unlock_pane(chan_info *cp, void *ptr)
{
  XtVaSetValues(channel_main_pane(cp),XmNpaneMinimum,5,XmNpaneMaximum,LOTSA_PIXELS,NULL);
  return(0);
}

int channel_lock_pane(chan_info *cp, void *ptr)
{
  int val;
  val = (*((int *)ptr));
  if (val < 6) val = 6;
  XtUnmanageChild(channel_main_pane(cp));
  XtVaSetValues(channel_main_pane(cp),XmNpaneMinimum,val-5,XmNpaneMaximum,val+5,NULL);
  return(0);
}

static void W_sy_Drag_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  sy_changed(((XmScrollBarCallbackStruct *)callData)->value,(chan_info *)(clientData));
}

static void W_sy_ValueChanged_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  sy_changed(((XmScrollBarCallbackStruct *)callData)->value,(chan_info *)(clientData));
}

static void W_sx_Drag_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  sx_changed(((XmScrollBarCallbackStruct *)callData)->value,(chan_info *)(clientData));
}

static void W_sx_ValueChanged_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  sx_changed(((XmScrollBarCallbackStruct *)callData)->value,(chan_info *)(clientData));
}

static void W_sx_Increment_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  /* problem here is that in large files these increments, if determined via scrollbar values, are huge */
  /* so, move ahead one windowfull on each tick */
  sx_incremented((chan_info *)clientData,1.0);
}

static void W_sx_Decrement_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  sx_incremented((chan_info *)clientData,-1.0);
}

static void W_zy_Drag_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  zy_changed(((XmScrollBarCallbackStruct *)callData)->value,(chan_info *)clientData);
}

static void W_zy_ValueChanged_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  zy_changed(((XmScrollBarCallbackStruct *)callData)->value,(chan_info *)clientData);
}

static void W_zx_Drag_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  zx_changed(((XmScrollBarCallbackStruct *)callData)->value,(chan_info *)clientData);
}

static void W_zx_ValueChanged_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  zx_changed(((XmScrollBarCallbackStruct *)callData)->value,(chan_info *)clientData);
}

static void W_gzy_Drag_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  gzy_changed(((XmScrollBarCallbackStruct *)callData)->value,(chan_info *)clientData);
}

static void W_gzy_ValueChanged_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  gzy_changed(((XmScrollBarCallbackStruct *)callData)->value,(chan_info *)clientData);
}

static void W_gsy_Drag_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  gsy_changed(((XmScrollBarCallbackStruct *)callData)->value,(chan_info *)clientData);
}

static void W_gsy_ValueChanged_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  gsy_changed(((XmScrollBarCallbackStruct *)callData)->value,(chan_info *)clientData);
}

/* anything special for increment?  XmNincrementCallback W_sx_Increment_Callback */


/* help callbacks (for mouse click help) -- all take snd_state as clientData */

static void W_graph_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  click_for_graph_help((snd_state *)clientData);
}

static void W_History_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  click_for_history_help((snd_state *)clientData);
}

static void W_sx_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "X axis scroll",
"This scrollbar controls the position of\n\
the x axis within the overall sound file.\n\
The arrows increment the view by one window.\n\
");
}

static void W_sy_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Y axis scroll",
"This (nearly useless) scrollbar controls the\n\
position of the y-axis within the current y axis\n\
limits.\n\
");
}

static void W_zx_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "X axis zoom",
"This scrollbar zooms in (as you move\n\
it to the left) or out along the x axis.\n\
");
}

static void W_zy_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Y axis zoom",
"This scrollbar zooms in (as you move\n\
it down) or out along the y axis.\n\
");
}

static void W_gsy_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Graph position",
"This scrollbar controls the position\n\
in the overall combined graph of the\n\
portion visible in the sound pane.\n\
");
}

static void W_gzy_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Graph zoom",
"This scrollbar controls how much of\n\
the overall combined graph is visible\n\
in the sound pane.\n\
");
}

static void F_button_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "fft button",
"This button controls whether an FFT is\n\
displayed alongside the waveform.  To affect\n\
all channels at once, use control-click.\n\
");
}

static void W_button_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "time domain waveform button",
"This button determines whether the time\n\
domain waveform is displayed.  If both the\n\
'w' and 'f' buttons are off, only the lisp\n\
graph (if any) is displayed.  To affect\n\
all channels at once, use control-click.\n\
");
}


static void F_button_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)callData;
  XButtonEvent *ev;
  ev = (XButtonEvent *)(cb->event);
  f_button_callback((chan_info *)clientData,cb->set,(ev->state & snd_ControlMask));
}

static void W_button_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)callData;
  XButtonEvent *ev;
  ev = (XButtonEvent *)(cb->event);
  w_button_callback((chan_info *)clientData,cb->set,(ev->state & snd_ControlMask));
}

#define MIN_REGRAPH_X 12
#define MIN_REGRAPH_Y 7
#define MIN_MIX_REGRAPH_X 25
#define MIN_MIX_REGRAPH_Y 12

static void Channel_Expose_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  snd_info *sp;
  chan_info *cp = (chan_info *)clientData;
  XmDrawingAreaCallbackStruct *cb = (XmDrawingAreaCallbackStruct *)callData;
  XExposeEvent *ev;

  ev = (XExposeEvent *)(cb->event);
  if ((ev->count > 0) || (mix_dragging())) return;

  /* try to flush tiny expose events caused (I think) by the mix consoles on top of the drawingarea */
  if ((ev->height < MIN_REGRAPH_Y) || (ev->width < MIN_REGRAPH_X)) return;
  if ((cp->mixes) && ((ev->height < MIN_MIX_REGRAPH_Y) || (ev->width < MIN_MIX_REGRAPH_X))) return;
  /* 
   *   mix consoles are normally aligned by 10 in the y direction and (if minimized) are
   *   rectangles around 25 x 30, so as new consoles are drawn over the main graph, we
   *   get a sequence of expose callbacks for a rectangle at a y multiple of 10 and
   *   about 26-28 x 30 - 34.  But cancelling these events didn't seem to speed up
   *   redisplay.  As a mix console is dragged, its md->beg field is reset, but the
   *   associated redisplay seems to be off-by-one.
   */
  sp = cp->sound;
  if (sp->combining != CHANNELS_SEPARATE)
    map_over_sound_chans(sp,update_graph,NULL);
  else update_graph(cp,NULL);
}

static void Channel_Resize_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  snd_info *sp;
  chan_info *cp = (chan_info *)clientData;
  sp = cp->sound;
  if (sp->combining != CHANNELS_SEPARATE)
    map_over_sound_chans(sp,update_graph,NULL);
  else update_graph(cp,NULL);
}

static void graph_mouse_enter(Widget w, XtPointer clientData, XEvent *event, Boolean *flag)
{
  snd_state *ss = (snd_state *)clientData;
  XDefineCursor(XtDisplay(w),XtWindow(w),(ss->sgx)->graph_cursor);
}

static void graph_mouse_leave(Widget w, XtPointer clientData, XEvent *event, Boolean *flag)
{
  XUndefineCursor(XtDisplay(w),XtWindow(w));
}

static void graph_button_press(Widget w,XtPointer clientData,XEvent *event,Boolean *cont) 
{
  XButtonEvent *ev = (XButtonEvent *)event;
  graph_button_press_callback((chan_info *)clientData,ev->x,ev->y,ev->state,ev->button,ev->time);
}

static void graph_button_release(Widget w,XtPointer clientData,XEvent *event,Boolean *cont) 
{
  XButtonEvent *ev = (XButtonEvent *)event;
  graph_button_release_callback((chan_info *)clientData,ev->x,ev->y,ev->state,ev->button,ev->time);
}

static void graph_button_motion(Widget w,XtPointer clientData,XEvent *event,Boolean *cont) 
{ /* mouse drag */
  XMotionEvent *ev = (XMotionEvent *)event;
  graph_button_motion_callback((chan_info *)clientData,ev->x,ev->y,ev->time,XtGetMultiClickTime(XtDisplay(w)));
}

static int no_padding(Arg *args, int n)
{
  XtSetArg(args[n],XmNmarginHeight,0); n++;
  XtSetArg(args[n],XmNmarginWidth,0); n++;
  XtSetArg(args[n],XmNmarginTop,0); n++;
  XtSetArg(args[n],XmNmarginBottom,0); n++;
  XtSetArg(args[n],XmNmarginLeft,0); n++;
  XtSetArg(args[n],XmNmarginRight,0); n++;
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

static void edit_select_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  /* undo/redo to reach selected position */
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)callData;
  XButtonEvent *ev;
  ev = (XButtonEvent *)(cbs->event);
  edit_select_callback((chan_info *)clientData,cbs->item_position-1,(ev->state & snd_ControlMask));
}

void reflect_edit_history_change(chan_info *cp)
{
  /* new edit so it is added, and any trailing lines removed */
  chan_context *cx;
  Widget lst;
  snd_state *ss;
  snd_info *sp;
  int i,eds;
  XmString *edits;
  ss = cp->state;
#if (XmVERSION == 1)
  if (show_edit_history(ss))
#endif
    {
      cx = cp->cgx;
      if (cx)
	{
	  lst = cx->chan_widgets[W_edhist];
	  if (lst)
	    {
	      eds = cp->edit_ctr;
	      while ((eds<(cp->edit_size-1)) && (cp->edits[eds+1])) eds++;
	      if (eds>=0)
		{
		  sp = cp->sound;
		  edits = (XmString *)CALLOC(eds+1,sizeof(XmString));
		  edits[0] = XmStringCreate(sp->fullname,XmFONTLIST_DEFAULT_TAG);
		  for (i=1;i<=eds;i++) edits[i] = XmStringCreate(edit_to_string(cp->edits[i]),XmFONTLIST_DEFAULT_TAG);
		  XtVaSetValues(lst,XmNitems,edits,XmNitemCount,eds+1,NULL);
		  XmListSelectPos(lst,cp->edit_ctr+1,FALSE);
		  for (i=0;i<=eds;i++) XmStringFree(edits[i]);
		  FREE(edits);
		  XtVaGetValues(lst,XmNvisibleItemCount,&i,NULL);
		  if (i <= eds) XtVaSetValues(lst,XmNtopItemPosition,eds-i+2,NULL);
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
  if (cx)
    {
      lst = cx->chan_widgets[W_edhist];
      if (lst)
	{
	  new_line = (char *)CALLOC(256,sizeof(char));
	  sprintf(new_line,"%s: (save-sound-as \"%s\")",edit_to_string(cp->edits[cp->edit_ctr]),filename);
	  str = XmStringCreate(new_line,XmFONTLIST_DEFAULT_TAG);
	  pos = cp->edit_ctr+1;
	  XmListReplacePositions(lst,&pos,&str,1);
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
  int len,top;
  snd_state *ss;
  ss = cp->state;
#if (XmVERSION == 1)
  if (show_edit_history(ss))
#endif
    {
      cx = cp->cgx;
      if (cx)
	{
	  lst = cx->chan_widgets[W_edhist];
	  if (lst)
	    {
	      XmListSelectPos(lst,cp->edit_ctr+1,FALSE);
	      XtVaGetValues(lst,XmNvisibleItemCount,&len,XmNtopItemPosition,&top,NULL);
	      if ((cp->edit_ctr+1) < top) 
		XtVaSetValues(lst,XmNtopItemPosition,cp->edit_ctr+1,NULL);
	      else
		if ((cp->edit_ctr+1) >= (top+len))
		  XtVaSetValues(lst,XmNtopItemPosition,cp->edit_ctr,NULL);
	      goto_graph(cp);
	    }
	}
    }
}

#if (XmVERSION == 1)
int open_edit_histories(chan_info *cp, void *ptr)
{
  chan_context *cx;
  snd_state *ss;
  Widget *cw;
  cx = cp->cgx;
  cw = cx->chan_widgets;
  ss = cp->state;
  if ((cx) && (cw[W_edhist]))
    {
      XtUnmanageChild(cw[W_main_window]);
      if (!(ss->using_schemes)) XtVaSetValues(cw[W_edhist],XmNbackground,(ss->sgx)->white,XmNforeground,(ss->sgx)->black,NULL);
      XtVaSetValues(XtParent(cw[W_edhist]),XmNwidth,edit_history_width(ss),NULL);
      XtVaSetValues(cw[W_edhist],XmNwidth,edit_history_width(ss),NULL);
      XtManageChild(cw[W_main_window]);
      reflect_edit_history_change(cp);
    }
  return(0);
}

int close_edit_histories(chan_info *cp, void *ptr)
{
  chan_context *cx;
  snd_state *ss;
  Widget *cw;
  cx = cp->cgx;
  cw = cx->chan_widgets;
  ss = cp->state;
  in_set_edit_history_width(ss,0); /* 1? */
  if ((cx) && (cw[W_edhist]))
    {
      if (!(ss->using_schemes)) XtVaSetValues(cw[W_edhist],XmNbackground,(ss->sgx)->basic_color,NULL);
      XtVaSetValues(XtParent(cw[W_edhist]),XmNwidth,edit_history_width(ss),NULL);
      XtVaSetValues(cw[W_edhist],XmNwidth,edit_history_width(ss),NULL);
    }
  return(0);
}

int set_edit_history_width(snd_state *ss, int width)
{
  in_set_edit_history_width(ss,width);
  if (show_edit_history(ss))
    map_over_chans(ss,open_edit_histories,NULL);
  return(width);
}
#endif

void add_channel_window(snd_info *sp, int channel, snd_state *ss, int chan_y, int insertion, Widget main, int button_style)
{
  Widget *cw;
  Widget left_widget = NULL;
  chan_info *cp;
  chan_context *cx;
  axis_context *cax;
  state_context *sx;
  int make_widgets,i,n,need_colors,need_extra_scrollbars;
  Arg args[32];
  if (sp == NULL) snd_error("null sound pointer passed to add_channel_window");
  make_widgets = ((sp->chans[channel]) == NULL);
  sp->chans[channel] = make_chan_info(sp->chans[channel],channel,sp,ss);
  cp = sp->chans[channel];
  cx = cp->cgx;
  if (cx->chan_widgets == NULL) cx->chan_widgets = (Widget *)CALLOC(NUM_CHAN_WIDGETS,sizeof(Widget));
  cw = cx->chan_widgets;
  
  sx = ss->sgx;
  need_extra_scrollbars = ((!main) && (channel == 0));

  if (make_widgets)
    {
      /* allocate the entire widget apparatus for this channel of this sound */

      need_colors = (!(ss->using_schemes));

      if (!main)
	{
	  n=0;
	  if (need_colors) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
	  if (insertion) {XtSetArg(args[n],XmNpositionIndex,(short)channel); n++;}
	  XtSetArg(args[n],XmNpaneMinimum,chan_y); n++;
#if (XmVERSION > 1)
	  cw[W_form] = sndCreateFormWidget("hiho",w_snd_pane(sp),args,n);

	  n=0;
	  if (need_colors) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
	  n = no_padding(args,n);
	  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
	  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
	  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
	  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
	  XtSetArg(args[n],XmNsashIndent,2); n++;
	  XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
	  cw[W_top] = sndCreatePanedWindowWidget("chn-main-window",cw[W_form],args,n);
	  XtAddEventHandler(cw[W_top],KeyPressMask,FALSE,graph_key_press,(XtPointer)sp);
#else
	  cw[W_main_window] = sndCreateFormWidget("chn-main-window",w_snd_pane(sp),args,n);
	  XtAddEventHandler(cw[W_main_window],KeyPressMask,FALSE,graph_key_press,(XtPointer)sp);
#endif

	  n=0;
#if defined(LESSTIF_VERSION) || (XmVERSION > 1)
	  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->white); n++;}
#else
	  if (!(ss->using_schemes)) 
	    {
#if (XmVERSION == 1)
	      if (show_edit_history(ss))
		{XtSetArg(args[n],XmNbackground,(ss->sgx)->white); n++;}
	      else {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
#else
	      XtSetArg(args[n],XmNbackground,(ss->sgx)->white); n++;
#endif
	    }
#endif
#if (XmVERSION > 1)
	  XtSetArg(args[n],XmNpaneMaximum,DEFAULT_EDIT_HISTORY_WIDTH); n++;
	  XtSetArg(args[n],XmNlistSizePolicy,XmCONSTANT); n++;
	  cw[W_edhist] = XmCreateScrolledList(cw[W_top],"edhist",args,n);

	  n=0;
	  if (need_colors) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
	  XtSetArg(args[n],XmNpaneMaximum,LOTSA_PIXELS); n++;
	  cw[W_main_window] = sndCreateFormWidget("chn-main-window",cw[W_top],args,n);
	  XtAddEventHandler(cw[W_main_window],KeyPressMask,FALSE,graph_key_press,(XtPointer)sp);
	  /* left_widget = cw[W_main_window]; */
#else
	  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
	  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
	  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
	  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
	  XtSetArg(args[n],XmNwidth,edit_history_width(ss)); n++;
	  XtSetArg(args[n],XmNlistSizePolicy,XmCONSTANT); n++;
	  cw[W_edhist] = XmCreateScrolledList(cw[W_main_window],"edhist",args,n);
	  left_widget = cw[W_edhist];
#endif
	  XtManageChild(cw[W_edhist]);
#if (XmVERSION == 1)
	  XtVaSetValues(XtParent(cw[W_edhist]),XmNwidth,edit_history_width(ss),NULL);
#endif
	  XtAddCallback(cw[W_edhist],XmNbrowseSelectionCallback,edit_select_Callback,cp);
	  XtAddCallback(cw[W_edhist],XmNhelpCallback,W_History_Help_Callback,ss);
	  XtAddEventHandler(cw[W_edhist],KeyPressMask,FALSE,graph_key_press,(XtPointer)sp);
	  XtAddEventHandler(XtParent(cw[W_edhist]),KeyPressMask,FALSE,graph_key_press,(XtPointer)sp);
	}
      else cw[W_main_window] = main;

      n=0;  
      if (need_colors) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      if (left_widget)
	{
	  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
#if defined(LESSTIF_VERSION) && (XmVERSION == 1)
	  XtSetArg(args[n],XmNleftWidget,XtParent(left_widget)); n++;
#else
	  XtSetArg(args[n],XmNleftWidget,left_widget); n++;
#endif
	}
      else
	{
	  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
	}
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
      n = no_padding(args,n);
      XtSetArg(args[n],XmNpacking,XmPACK_COLUMN); n++;
      XtSetArg(args[n],XmNnumColumns,1); n++;
      XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
      cw[W_wf_buttons] = sndCreateRowColumnWidget("chn-buttons",cw[W_main_window],args,n);	

      if (button_style == WITH_FW_BUTTONS)
	{
	  n=0;
	  if (need_colors) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
	  XtSetArg(args[n],XM_FONT_RESOURCE,BUTTON_FONT(ss)); n++;
	  XtSetArg(args[n],XmNspacing,1); n++;
	  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNselectColor,sx->pushed_button_color); n++;}
	  cw[W_f] = sndCreateToggleButtonWidget(STR_f,cw[W_wf_buttons],args,n);
	  XtAddCallback(cw[W_f],XmNvalueChangedCallback,F_button_Callback,cp);
	  XtAddCallback(cw[W_f],XmNhelpCallback,F_button_Help_Callback,ss);
	  XtAddEventHandler(cw[W_f],KeyPressMask,FALSE,graph_key_press,(XtPointer)sp);
	  
	  XtSetArg(args[n],XmNset,TRUE); n++;
	  cw[W_w] = sndCreateToggleButtonWidget(STR_w,cw[W_wf_buttons],args,n);
	  XtAddCallback(cw[W_w],XmNvalueChangedCallback,W_button_Callback,cp);
	  XtAddCallback(cw[W_w],XmNhelpCallback,W_button_Help_Callback,ss);
	  XtAddEventHandler(cw[W_w],KeyPressMask,FALSE,graph_key_press,(XtPointer)sp);
	}
      else
	{
	  n=0;
	  if (need_colors) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
	  XtSetArg(args[n],XmNarrowDirection,XmARROW_UP); n++;
	  XtSetArg(args[n],XmNsensitive,FALSE); n++;
	  cw[W_f] = XtCreateManagedWidget("up",xmArrowButtonWidgetClass,cw[W_wf_buttons],args,n);

	  n=0;
	  if (need_colors) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
	  XtSetArg(args[n],XmNarrowDirection,XmARROW_DOWN); n++;
	  XtSetArg(args[n],XmNsensitive,FALSE); n++;
	  cw[W_w] = XtCreateManagedWidget("down",xmArrowButtonWidgetClass,cw[W_wf_buttons],args,n);
	}

      n=0;
      if (need_colors) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNbottomWidget,cw[W_wf_buttons]); n++;
      if (left_widget)
	{
	  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
#if defined(LESSTIF_VERSION) && (XmVERSION == 1)
	  XtSetArg(args[n],XmNleftWidget,XtParent(left_widget)); n++;
#else
	  XtSetArg(args[n],XmNleftWidget,left_widget); n++;
#endif
	}
      else
	{
	  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
	}
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNspacing,0); n++;
      cw[W_left_scrollers] = sndCreateRowColumnWidget("chn-left",cw[W_main_window],args,n);
      XtAddEventHandler(cw[W_left_scrollers],KeyPressMask,FALSE,graph_key_press,(XtPointer)sp);

      n=0;
      if (need_colors) {XtSetArg(args[n],XmNbackground,(ss->sgx)->zoom_color); n++;}
      XtSetArg(args[n],XmNwidth,ss->position_slider_width); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++; 
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
      XtSetArg(args[n],XmNmaximum,SCROLLBAR_MAX); n++; 
      XtSetArg(args[n],XmNincrement,1); n++;
      XtSetArg(args[n],XmNprocessingDirection,XmMAX_ON_TOP); n++;
      XtSetArg(args[n],XmNdragCallback,make_callback_list(W_zy_Drag_Callback,(XtPointer)cp)); n++;
      XtSetArg(args[n],XmNvalueChangedCallback,make_callback_list(W_zy_ValueChanged_Callback,(XtPointer)cp)); n++;
      cw[W_zy] = XtCreateManagedWidget("chn-zy",xmScrollBarWidgetClass,cw[W_left_scrollers],args,n);
      XtAddCallback(cw[W_zy],XmNhelpCallback,W_zy_Help_Callback,ss);
      XtAddEventHandler(cw[W_zy],KeyPressMask,FALSE,graph_key_press,(XtPointer)sp);

      n=0;
      if (need_colors) {XtSetArg(args[n],XmNbackground,(ss->sgx)->position_color); n++;}
      XtSetArg(args[n],XmNwidth,ss->zoom_slider_width); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNleftWidget,cw[W_zy]); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
      XtSetArg(args[n],XmNmaximum,SCROLLBAR_SY_MAX); n++;
      XtSetArg(args[n],XmNincrement,1); n++;
      XtSetArg(args[n],XmNprocessingDirection,XmMAX_ON_TOP); n++;
      XtSetArg(args[n],XmNdragCallback,make_callback_list(W_sy_Drag_Callback,(XtPointer)cp)); n++;
      XtSetArg(args[n],XmNvalueChangedCallback,make_callback_list(W_sy_ValueChanged_Callback,(XtPointer)cp)); n++;
      cw[W_sy] = XtCreateManagedWidget("chn-sy",xmScrollBarWidgetClass,cw[W_left_scrollers],args,n);
      XtAddCallback(cw[W_sy],XmNhelpCallback,W_sy_Help_Callback,ss);
      XtAddEventHandler(cw[W_sy],KeyPressMask,FALSE,graph_key_press,(XtPointer)sp);

      n=0;
      if (need_colors) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNleftWidget,cw[W_wf_buttons]); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNspacing,0); n++;
      cw[W_bottom_scrollers] = sndCreateRowColumnWidget("chn-bottom",cw[W_main_window],args,n);
      XtAddEventHandler(cw[W_bottom_scrollers],KeyPressMask,FALSE,graph_key_press,(XtPointer)sp);

      n=0;
      if (need_colors) {XtSetArg(args[n],XmNbackground,(ss->sgx)->position_color); n++;}
      XtSetArg(args[n],XmNheight,ss->position_slider_width); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
      XtSetArg(args[n],XmNmaximum,sp->sx_scroll_max); n++;
      XtSetArg(args[n],XmNincrement,1); n++;
      XtSetArg(args[n],XmNdragCallback,make_callback_list(W_sx_Drag_Callback,(XtPointer)cp)); n++;
      XtSetArg(args[n],XmNincrementCallback,make_callback_list(W_sx_Increment_Callback,(XtPointer)cp)); n++;
      XtSetArg(args[n],XmNdecrementCallback,make_callback_list(W_sx_Decrement_Callback,(XtPointer)cp)); n++;
      XtSetArg(args[n],XmNvalueChangedCallback,make_callback_list(W_sx_ValueChanged_Callback,(XtPointer)cp)); n++;
      cw[W_sx] = XtCreateManagedWidget("chn-sx",xmScrollBarWidgetClass,cw[W_bottom_scrollers],args,n);
      XtAddCallback(cw[W_sx],XmNhelpCallback,W_sx_Help_Callback,ss);
      XtAddEventHandler(cw[W_sx],KeyPressMask,FALSE,graph_key_press,(XtPointer)sp);

      n=0;
      if (need_colors) {XtSetArg(args[n],XmNbackground,(ss->sgx)->zoom_color); n++;}
      XtSetArg(args[n],XmNheight,ss->zoom_slider_width+2); n++;
      XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNbottomWidget,cw[W_sx]); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNmaximum,SCROLLBAR_MAX); n++;
      XtSetArg(args[n],XmNincrement,1); n++;
      XtSetArg(args[n],XmNdragCallback,make_callback_list(W_zx_Drag_Callback,(XtPointer)cp)); n++;
      XtSetArg(args[n],XmNvalueChangedCallback,make_callback_list(W_zx_ValueChanged_Callback,(XtPointer)cp)); n++;
      cw[W_zx] = XtCreateManagedWidget("chn-zx",xmScrollBarWidgetClass,cw[W_bottom_scrollers],args,n);
      XtAddCallback(cw[W_zx],XmNhelpCallback,W_zx_Help_Callback,ss);
      XtAddEventHandler(cw[W_zx],KeyPressMask,FALSE,graph_key_press,(XtPointer)sp);

      n=0;
      if (need_colors) 
	{
	  XtSetArg(args[n],XmNbackground,sx->graph_color); n++;
	  XtSetArg(args[n],XmNforeground,sx->data_color); n++;
	  /* XtSetArg(args[n],XmNbackgroundPixmap,XmGetPixmap(XtScreen(SOUND_PANE(ss)),"text.xpm",sx->basic_color,sx->graph_color)); n++; */
	  /* how to include these in the resource list?  can we use png files here? */
	  /* ./snd oboe.snd -xrm '*chn-graph*backgroundPixmap: text.xpm' */
	}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNbottomWidget,cw[W_bottom_scrollers]); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNleftWidget,cw[W_left_scrollers]); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      /* this collides with W_gzy below, but a consistent version came up with half a window blank */
      XtSetArg(args[n],XmNnavigationType,XmNONE); n++;
      cw[W_graph] = sndCreateDrawingAreaWidget("chn-graph",cw[W_main_window],args,n);
      XtAddCallback(cw[W_graph],XmNhelpCallback,W_graph_Help_Callback,ss);
      XtAddCallback(cw[W_graph],XmNresizeCallback,Channel_Resize_Callback,(XtPointer)cp);
      XtAddCallback(cw[W_graph],XmNexposeCallback,Channel_Expose_Callback,(XtPointer)cp);
      XtAddEventHandler(cw[W_graph],EnterWindowMask,FALSE,graph_mouse_enter,(XtPointer)(cp->state));
      XtAddEventHandler(cw[W_graph],LeaveWindowMask,FALSE,graph_mouse_leave,(XtPointer)cp);
      XtAddEventHandler(cw[W_graph],ButtonPressMask,FALSE,graph_button_press,(XtPointer)cp);
      XtAddEventHandler(cw[W_graph],ButtonMotionMask,FALSE,graph_button_motion,(XtPointer)cp);
      XtAddEventHandler(cw[W_graph],ButtonReleaseMask,FALSE,graph_button_release,(XtPointer)cp);
      XtAddEventHandler(cw[W_graph],KeyPressMask,FALSE,graph_key_press,(XtPointer)cp);

      if (need_extra_scrollbars)
	{
	  /* that is: not region browser chan, might need combined graph, channel 0 is the controller in that case */
	  /* this is independent of sp->nchans because these structs are re-used and added to as needed */
	  n=0;
	  if (need_colors) {XtSetArg(args[n],XmNbackground,(ss->sgx)->zoom_color); n++;}
	  XtSetArg(args[n],XmNwidth,ss->position_slider_width); n++;
	  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
	  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
	  XtSetArg(args[n],XmNbottomWidget,cw[W_bottom_scrollers]); n++;
	  XtSetArg(args[n],XmNleftAttachment,XmATTACH_NONE); n++;
	  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
	  XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
	  XtSetArg(args[n],XmNmaximum,SCROLLBAR_MAX); n++; 
	  XtSetArg(args[n],XmNincrement,1); n++;
	  XtSetArg(args[n],XmNprocessingDirection,XmMAX_ON_TOP); n++;
	  XtSetArg(args[n],XmNdragCallback,make_callback_list(W_gzy_Drag_Callback,(XtPointer)cp)); n++;
	  XtSetArg(args[n],XmNvalueChangedCallback,make_callback_list(W_gzy_ValueChanged_Callback,(XtPointer)cp)); n++;
	  cw[W_gzy] = XtCreateManagedWidget("chn-gzy",xmScrollBarWidgetClass,cw[W_main_window],args,n);
	  XtAddCallback(cw[W_gzy],XmNhelpCallback,W_gzy_Help_Callback,ss);
	  XtAddEventHandler(cw[W_gzy],KeyPressMask,FALSE,graph_key_press,(XtPointer)sp);

	  n=0;
	  if (need_colors) {XtSetArg(args[n],XmNbackground,(ss->sgx)->position_color); n++;}
	  XtSetArg(args[n],XmNwidth,ss->position_slider_width); n++;
	  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
	  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
	  XtSetArg(args[n],XmNbottomWidget,cw[W_bottom_scrollers]); n++;
	  XtSetArg(args[n],XmNleftAttachment,XmATTACH_NONE); n++;
	  XtSetArg(args[n],XmNrightAttachment,XmATTACH_WIDGET); n++;
	  XtSetArg(args[n],XmNrightWidget,cw[W_gzy]); n++;
	  XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
	  XtSetArg(args[n],XmNmaximum,SCROLLBAR_MAX); n++;
	  XtSetArg(args[n],XmNincrement,1); n++;
	  XtSetArg(args[n],XmNprocessingDirection,XmMAX_ON_TOP); n++;
	  XtSetArg(args[n],XmNdragCallback,make_callback_list(W_gsy_Drag_Callback,(XtPointer)cp)); n++;
	  XtSetArg(args[n],XmNvalueChangedCallback,make_callback_list(W_gsy_ValueChanged_Callback,(XtPointer)cp)); n++;
	  cw[W_gsy] = XtCreateManagedWidget("chn-gsy",xmScrollBarWidgetClass,cw[W_main_window],args,n);
	  XtAddCallback(cw[W_gsy],XmNhelpCallback,W_gsy_Help_Callback,ss);
	  XtAddEventHandler(cw[W_gsy],KeyPressMask,FALSE,graph_key_press,(XtPointer)sp);

	  n=0;
	}
      else
	{
	  cw[W_gsy] = NULL;
	  cw[W_gzy] = NULL;
	}

#if (XmVERSION == 1)
      if ((!main) && (show_edit_history(ss)))
	{
	  XtVaSetValues(cw[W_edhist],XmNwidth,50,NULL);
	  XtManageChild(cw[W_edhist]);
	}
#endif

      /* also position of current graph in overall sound as window */

    } /* alloc new chan */
  else
    { /* re-manage currently inactive chan */
      XtVaSetValues(cw[W_main_window],XmNpaneMinimum,chan_y,NULL);
#if (XmVERSION > 1)
      if (cw[W_edhist]) XtVaSetValues(XtParent(cw[W_edhist]),XmNpaneMaximum,1,NULL);
#endif
      for (i=0;i<NUM_CHAN_WIDGETS;i++)
	{
	  if (cw[i])
	    {
	      if  (!XtIsManaged(cw[i])) XtManageChild(cw[i]);
	      if (i == W_sx) 
		{
		  int current_size;
		  XtVaSetValues(cw[i],XmNvalue,0,NULL);
		  XtVaGetValues(cw[i],XmNsliderSize,&current_size,NULL);
		  if (current_size > sp->sx_scroll_max) XtVaSetValues(cw[i],XmNsliderSize,sp->sx_scroll_max/2,NULL);
		  XtVaSetValues(cw[i],XmNmaximum,sp->sx_scroll_max,NULL);
		}
	    }
	}
    }
#if (XmVERSION > 1)
  if (cw[W_edhist]) XtVaSetValues(XtParent(cw[W_edhist]),XmNpaneMaximum,LOTSA_PIXELS,NULL);
#endif
  if ((need_extra_scrollbars) && (sp->combining == CHANNELS_SEPARATE)) 
    hide_gz_scrollbars(sp); /* default is on in this case */  
  cax = cx->ax;
  cax->wn = XtWindow(cw[W_graph]);
  cax->dp = XtDisplay(cw[W_graph]);
  cax->gc = sx->basic_gc;
}

int chan_fft_in_progress(chan_info *cp)
{
  return((cp->cgx)->fft_in_progress);
}

void set_chan_fft_in_progress(chan_info *cp, XtWorkProcId fp) 
{
  (cp->cgx)->fft_in_progress = fp;
}

int calculate_fft(chan_info *cp, void *ptr)
{
  Widget w;
  snd_state *ss;
  if (cp->ffting)
    {
      ss = cp->state;
      if (!(chan_fft_in_progress(cp)))
	{
	  w = channel_graph(cp);
	  if (fft_style(ss) == NORMAL_FFT)
	    {
	      if (fft_size(ss) >= 65536) start_progress_report(ss,cp->sound,NOT_FROM_ENVED);
	      set_chan_fft_in_progress(cp,XtAppAddWorkProc(XtWidgetToApplicationContext(w),safe_fft_in_slices,(XtPointer)make_fft_state(cp,1)));
	    }
	  else 
	    set_chan_fft_in_progress(cp,XtAppAddWorkProc(XtWidgetToApplicationContext(w),sonogram_in_slices,(XtPointer)make_sonogram_state(cp)));
	}
    }
  return(0);
}

void set_peak_numbers_font(chan_info *cp)
{
  XFontStruct *bf;
  chan_context *cx;
  snd_state *ss;
  ss = cp->state;
  cx = cp->tcgx;
  if (!cx) cx = cp->cgx;
  bf = PEAK_NUMBERS_FONT(ss);
  XSetFont(XtDisplay(cx->chan_widgets[W_graph]),copy_GC(cp),bf->fid);
}

void set_tiny_numbers_font(chan_info *cp)
{
  XFontStruct *bf;
  chan_context *cx;
  snd_state *ss;
  ss = cp->state;
  cx = cp->tcgx;
  if (!cx) cx = cp->cgx;
  bf = TINY_NUMBERS_FONT(ss);
  XSetFont(XtDisplay(cx->chan_widgets[W_graph]),copy_GC(cp),bf->fid);
}

void set_bold_peak_numbers_font(chan_info *cp)
{
  XFontStruct *bf;
  chan_context *cx;
  snd_state *ss;
  ss = cp->state;
  cx = cp->tcgx;
  if (!cx) cx = cp->cgx;
  bf = BOLD_PEAK_NUMBERS_FONT(ss);
  XSetFont(XtDisplay(cx->chan_widgets[W_graph]),copy_GC(cp),bf->fid);
}

#define CHAN_GC 0
#define CHAN_IGC 1
#define CHAN_SELGC 2
#define CHAN_CGC 3
#define CHAN_MGC 4
#define CHAN_MXGC 5
#define CHAN_TMPGC 6

static axis_context *set_context (chan_info *cp, int gc)
{
  axis_context *ax;
  state_context *sx;
  chan_context *cx;
  snd_state *ss;
  cx = cp->tcgx;
  ss = cp->state;
  if (!cx) cx = cp->cgx;
  ax = cx->ax;
  sx = ss->sgx;
  if ((cp->cgx)->selected)
    {
      switch (gc)
	{
	case CHAN_GC: ax->gc = sx->selected_basic_gc;        break;
	case CHAN_IGC: ax->gc = sx->selected_erase_gc;       break;
	case CHAN_SELGC: ax->gc = sx->selected_selection_gc; break;
	case CHAN_CGC: ax->gc = sx->selected_cursor_gc;      break;
	case CHAN_MGC: ax->gc = sx->selected_mark_gc;        break;
	case CHAN_MXGC: ax->gc = sx->mix_gc;                 break;
	case CHAN_TMPGC: ax->gc = sx->selected_basic_gc;     break;
	}
    }
  else
    {
      switch (gc)
	{
	case CHAN_GC: ax->gc = sx->basic_gc;        break;
	case CHAN_IGC: ax->gc = sx->erase_gc;       break;
	case CHAN_SELGC: ax->gc = sx->selection_gc; break;
	case CHAN_CGC: ax->gc = sx->cursor_gc;      break;
	case CHAN_MGC: ax->gc = sx->mark_gc;        break;
	case CHAN_MXGC: ax->gc = sx->mix_gc;        break;
	case CHAN_TMPGC: 
	  ax->gc = sx->combined_basic_gc;
	  /* if this changes, see snd-xprint.c ps_rgb */
	  switch (cp->chan % 4)
	    {
	    case 0: XSetForeground(MAIN_DISPLAY(ss),ax->gc,sx->black);      break;
	    case 1: XSetForeground(MAIN_DISPLAY(ss),ax->gc,sx->red);        break;
	    case 2: XSetForeground(MAIN_DISPLAY(ss),ax->gc,sx->green);      break;
	    case 3: XSetForeground(MAIN_DISPLAY(ss),ax->gc,sx->light_blue); break;
	    }
	  break;
	}
    }
  return(ax);
}

unsigned long get_foreground_color(chan_info *cp, axis_context *ax)
{
  XGCValues gv;
  snd_state *ss;
  ss = cp->state;
  XGetGCValues(MAIN_DISPLAY(ss),ax->gc,GCForeground,&gv);
  return(gv.foreground);
}

void set_foreground_color(chan_info *cp, axis_context *ax, Pixel color)
{
  snd_state *ss;
  ss = cp->state;
  XSetForeground(MAIN_DISPLAY(ss),ax->gc,color);
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
  state_context *sx;
  sx = (cp->state)->sgx;
  if ((cp->cgx)->selected) return(sx->selected_erase_gc);
  return(sx->erase_gc);
}

axis_context *copy_context (chan_info *cp)         {return(set_context(cp,CHAN_GC));}
axis_context *erase_context (chan_info *cp)        {return(set_context(cp,CHAN_IGC));}
axis_context *selection_context (chan_info *cp)    {return(set_context(cp,CHAN_SELGC));}
axis_context *cursor_context (chan_info *cp)       {return(set_context(cp,CHAN_CGC));}
axis_context *mark_context (chan_info *cp)         {return(set_context(cp,CHAN_MGC));}
axis_context *mix_waveform_context (chan_info *cp) {return(set_context(cp,CHAN_MXGC));}
axis_context *combined_context (chan_info *cp)     {return(set_context(cp,CHAN_TMPGC));}

/* for combined cases, the incoming chan_info pointer is always chan[0], 
 * but the actual channel depends on placement if mouse oriented.
 * virtual_selected_channel(cp) (snd-chn.c) retains the current selected channel
 */

void graph_key_press(Widget w,XtPointer clientData,XEvent *event,Boolean *cont) 
{
  /* called by every key-intercepting widget in the entire sound pane */
  XKeyEvent *ev = (XKeyEvent *)event;
  KeySym keysym;
  int key_state,caller_type;
  key_state = ev->state;
  keysym = XKeycodeToKeysym(XtDisplay(w),(int)(ev->keycode),(key_state & ShiftMask) ? 1 : 0);
  caller_type = ((snd_any *)(clientData))->s_type;
  key_press_callback((caller_type == SND_STATE) ? (snd_state *)clientData : NULL,
		     (caller_type == SND_INFO) ? (snd_info *)clientData : NULL,
		     (caller_type == CHAN_INFO) ? (chan_info *)clientData : NULL,
		     ev->x,ev->y,ev->state,keysym,
		     XKeysymToString(keysym));
}
 
void stop_amp_env(chan_info *cp)
{
  chan_context *cgx;
  cgx = cp->cgx;
  if ((cgx) && (cgx->amp_env_in_progress))
    {
      XtRemoveWorkProc(cgx->amp_env_in_progress);
      free_env_state(cp);
      cgx->amp_env_in_progress = 0; 
    }
}

static BACKGROUND_TYPE xget_amp_env (XtPointer cp)
{
  /* this extra step is needed to get around various X-isms */
  return(get_amp_env((chan_info *)cp));
}

void start_amp_env(chan_info *cp)
{
  chan_context *cgx;
  snd_state *ss;
  cgx = cp->cgx;
  if (cgx)
    {
      ss = cp->state;
      if (cgx->amp_env_in_progress) stop_amp_env(cp);
      cgx->amp_env_state = make_env_state(cp,current_ed_samples(cp));
      cgx->amp_env_in_progress = XtAppAddWorkProc(MAIN_APP(ss),xget_amp_env,(XtPointer)cp);
      reflect_amp_env_in_progress(cp->sound);
    }
}

static void cleanup_cw(chan_info *cp)
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
	      XtVaSetValues(cw[W_w],XmNset,TRUE,NULL);
	      XtVaSetValues(cw[W_f],XmNset,FALSE,NULL);
	    }
	  XtUnmanageChild(channel_main_pane(cp));
	}
    }
}

int stop_fft_in_progress(chan_info *cp, void *ptr)
{
  chan_context *cx;
  if ((cp) && (cp->cgx))
    {
      cx = cp->cgx;
      if (cx->fft_in_progress) 
	{
	  XtRemoveWorkProc(cx->fft_in_progress);
	  finish_progress_report(cp->state,cp->sound,NOT_FROM_ENVED);
	  cx->fft_in_progress = 0;
	}
    }
  return(0);
}

void chan_info_cleanup(chan_info *cp)
{
  chan_context *cx;
  if ((cp) && (cp->cgx))
    {
      cx = cp->cgx;
      cx->selected = 0;
      if (cx->fft_in_progress) 
	{
	  XtRemoveWorkProc(cx->fft_in_progress);
	  cx->fft_in_progress = 0;
	}
      stop_amp_env(cp);
      cleanup_cw(cp);
    }
}


static XtWorkProcId watch_mouse_button = 0;
static BACKGROUND_TYPE WatchMouse(XtPointer cp)
{
  if (watch_mouse_button)
    {
      move_mark_2((chan_info *)cp);
      return(BACKGROUND_CONTINUE);
    }
  else return(BACKGROUND_QUIT);
}

void StartMarkWatch(chan_info *cp)
{
  snd_state *ss;
  ss = cp->state;
  watch_mouse_button = XtAppAddWorkProc(XtWidgetToApplicationContext(MAIN_PANE(ss)),WatchMouse,(XtPointer)cp);
}

void CancelMarkWatch(void)
{
  if (watch_mouse_button) XtRemoveWorkProc(watch_mouse_button);
  watch_mouse_button = 0;
}

static void change_channel_style(snd_info *sp, int new_style)
{
  int i,j,old_style;
  snd_state *ss;
  chan_info *ncp,*cp,*pcp;
  int height[1];
  chan_context *mcgx;
  Widget *cw;
  axis_info *ap;
  chan_context *cx;
  if ((sp) && (sp->nchans > 1))
    {
      ss = sp->state;
      old_style = sp->combining;
      if (new_style != old_style)
	{
	  sp->combining = new_style;
	  if (old_style == CHANNELS_COMBINED)
	    hide_gz_scrollbars(sp);
	  else 
	    {
	      if (new_style == CHANNELS_COMBINED)
		show_gz_scrollbars(sp);
	    }
	  if (old_style == CHANNELS_SUPERIMPOSED)
	    {
	      syncb(sp,FALSE);
	      XtVaSetValues(w_snd_combine(sp),XmNselectColor,(ss->sgx)->pushed_button_color,NULL);
	    }
	  else
	    {
	      if (new_style == CHANNELS_SUPERIMPOSED)
		{
		  syncb(sp,TRUE);
		  XtVaSetValues(w_snd_combine(sp),XmNselectColor,(ss->sgx)->green,NULL);
		  apply_y_axis_change((sp->chans[0])->axis,sp->chans[0]);
		  apply_x_axis_change((sp->chans[0])->axis,sp->chans[0],sp);
		}
	    }
	  height[0] = widget_height(w_snd_pane(sp)) - widget_height(w_snd_ctrls(sp)) - 16;
	  if (old_style == CHANNELS_SEPARATE)
	    {
	      ncp = sp->chans[0];
	      sound_lock_ctrls(sp,NULL);
	      channel_lock_pane(ncp,height);
	      mcgx = ncp->cgx;
	      for (i=1;i<sp->nchans;i++) 
		{
		  ncp = sp->chans[i];
		  cleanup_cw(ncp);
		  ncp->tcgx = mcgx;
		  regraph_all_mixmarks(ncp);
		}
	      channel_open_pane(sp->chans[0],NULL);
	      channel_unlock_pane(sp->chans[0],NULL);
	      sound_unlock_ctrls(sp,NULL);
	      XmToggleButtonSetState(w_snd_combine(sp),TRUE,FALSE);
	    }
	  else
	    {
	      if (new_style == CHANNELS_SEPARATE)
		{
		  /* height[0] = total space available */
		  height[0] /= sp->nchans;
		  sound_lock_ctrls(sp,NULL);
		  map_over_sound_chans(sp,channel_lock_pane,(void *)height);
		  map_over_sound_chans(sp,channel_open_pane,NULL);
		  map_over_sound_chans(sp,channel_unlock_pane,NULL);
		  sound_unlock_ctrls(sp,NULL);
		  for (i=0;i<sp->nchans;i++) regraph_all_mixmarks(sp->chans[i]);
		  pcp = sp->chans[0];
		  ap = pcp->axis;
		  for (i=1;i<sp->nchans;i++)
		    {
		      cp = sp->chans[i];
		      cp->tcgx = NULL;
		      cx = cp->cgx;
		      cw = cx->chan_widgets;
#if (XmVERSION > 1)
		      if (cw[W_edhist]) XtVaSetValues(XtParent(cw[W_edhist]),XmNpaneMaximum,DEFAULT_EDIT_HISTORY_WIDTH,NULL);
#endif
		      for (j=0;j<NUM_CHAN_WIDGETS;j++)
			{
			  if ((cw[j]) && (!XtIsManaged(cw[j]))) XtManageChild(cw[j]);
			}
#if (XmVERSION > 1)
		      if (cw[W_edhist]) XtVaSetValues(XtParent(cw[W_edhist]),XmNpaneMaximum,LOTSA_PIXELS,NULL);
#endif
		      XmToggleButtonSetState(cw[W_f],cp->ffting,FALSE);
		      XmToggleButtonSetState(cw[W_w],cp->waving,FALSE);
		      /* these can get out of sync if changes are made in the unseparated case */
		      set_axes(cp,ap->x0,ap->x1,ap->y0,ap->y1);
		    }
		  XmToggleButtonSetState(w_snd_combine(sp),FALSE,FALSE);
		}
	    }
	}
    }
}

void combine_sound(snd_info *sp) {change_channel_style(sp,CHANNELS_COMBINED);}
void superimpose_sound(snd_info *sp) {change_channel_style(sp,CHANNELS_SUPERIMPOSED);}
void separate_sound(snd_info *sp) {change_channel_style(sp,CHANNELS_SEPARATE);}


int fixup_cp_cgx_ax_wn(chan_info *cp) {((cp->cgx)->ax)->wn = XtWindow((cp->cgx)->chan_widgets[W_graph]); return(1);}
