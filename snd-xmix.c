#include "snd.h"

static Widget mix_panel = NULL;
static bool dragging = false;
static void update_mix_panel(int mix_id);

/* ---------------- SPEED ---------------- */

static char speed_number_buffer[5] = {'1', STR_decimal, '0', '0', '\0'};

#define SPEED_SCROLLBAR_MID (0.45 * SCROLLBAR_MAX)
#define SPEED_SCROLLBAR_BREAK (0.15 * SCROLLBAR_MAX)
static Widget w_speed_number, w_speed_label, w_speed;
static Float current_speed = 1.0;

static void change_mix_speed(int mix_id, Float val)
{
  chan_info *cp;
  cp = mix_channel_from_id(mix_id);
  set_mix_speed_from_id(mix_id,
			srate_changed(val,
				      speed_number_buffer,
				      cp->sound->speed_control_style,
				      cp->sound->speed_control_tones),
			dragging);
  set_label(w_speed_number, speed_number_buffer);
}

static void speed_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  change_mix_speed(current_mix_id(), 1.0);
  XtVaSetValues(w_speed, XmNvalue, (int)SPEED_SCROLLBAR_MID, NULL);
}

static int mix_speed_to_int(Float uval, snd_info *sp)
{
  int ival;
  Float val;
  val = srate_changed(uval,
		      speed_number_buffer,
		      sp->speed_control_style,
		      sp->speed_control_tones);
  set_label(w_speed_number, speed_number_buffer);
  if (val > 0.0)
    {
      ival = snd_round(SPEED_SCROLLBAR_MID + SPEED_SCROLLBAR_BREAK * log(val));
      if (ival < SCROLLBAR_MAX)
	return(ival);
      else return(SCROLLBAR_MAX);
    }
  else return(0);
}

static void speed_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  int ival;
  ival = ((XmScrollBarCallbackStruct *)info)->value;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (!dragging) start_mix_drag(current_mix_id());
  dragging = true;
  change_mix_speed(current_mix_id(), exp((Float)(ival - SPEED_SCROLLBAR_MID) / SPEED_SCROLLBAR_BREAK));
}

static void speed_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  dragging = false;
  change_mix_speed(current_mix_id(), exp((Float)(cb->value - SPEED_SCROLLBAR_MID) / SPEED_SCROLLBAR_BREAK));
}


/* ---------------- AMP ---------------- */

static Widget *w_amp_numbers, *w_amp_labels, *w_amps;
static Float *current_amps;
#define CHANS_ALLOCATED 8
static char amp_number_buffer[5] = {'1', STR_decimal, '0', '0', '\0'};

static void change_mix_amp(int mix_id, int chan, Float val)
{
  char *sfs;
  set_mix_amp_from_id(mix_id, chan, val, dragging);
  sfs = prettyf(val, 2);
  fill_number(sfs, amp_number_buffer);
  set_label(w_amp_numbers[chan], amp_number_buffer);
  FREE(sfs);
}

static void amp_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  int chan;
  XtVaGetValues(w, XmNuserData, &chan, NULL);
  change_mix_amp(current_mix_id(), chan, 1.0);
  XtVaSetValues(w_amps[chan], XmNvalue, SCROLLBAR_MID, NULL);
}

static Float int_amp_to_Float(int amp)
{
  if (amp == 0)
    return(0.0);
  else
    {
      if (amp < SCROLLBAR_LINEAR_MAX)
	return((Float)amp * SCROLLBAR_LINEAR_MULT);
      else return(exp((Float)(amp - SCROLLBAR_MID) / ((Float)SCROLLBAR_MAX * .2)));
    }
}

static void amp_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  int ival, chan;
  XtVaGetValues(w, XmNuserData, &chan, NULL);
  ival = ((XmScrollBarCallbackStruct *)info)->value;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (!dragging) start_mix_drag(current_mix_id());
  dragging = true;
  change_mix_amp(current_mix_id(), chan, int_amp_to_Float(ival));
}

static void amp_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  int ival, chan;
  ival = ((XmScrollBarCallbackStruct *)info)->value;
  XtVaGetValues(w, XmNuserData, &chan, NULL);
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  dragging = false;
  change_mix_amp(current_mix_id(), chan, int_amp_to_Float(ival));
}

static int mix_amp_to_int(Float amp, int chan)
{
  char *sfs;
  int val;
  sfs = prettyf(amp, 2);
  fill_number(sfs, amp_number_buffer);
  set_label(w_amp_numbers[chan], amp_number_buffer);
  FREE(sfs);
  if (amp <= 0.0)
    return(0);
  else
    {
      val = (int)snd_round(amp / (Float)(SCROLLBAR_LINEAR_MULT));
      if (val > SCROLLBAR_LINEAR_MAX)
	{
	  val = (int)snd_round((log(amp) * ((Float)SCROLLBAR_MAX * .2)) + SCROLLBAR_MID);
	  if (val > SCROLLBAR_MAX) val = SCROLLBAR_MAX;
	}
    }
  return(val);
}


/* ---------------- AMP ENV ---------------- */

static Widget w_env_frame, w_env;
static axis_context *ax = NULL;
static GC cur_gc;
static void *spfs[8];
static int last_clicked_env_chan = 0;

static void mix_amp_env_resize(Widget w, XtPointer context, XtPointer info) 
{
  XGCValues gv;
  int chans, chan, mix_id;
  env **e;
  env *cur_env;
  if (ax == NULL)
    {
      gv.function = GXcopy;
      XtVaGetValues(w_env, XmNbackground, &gv.background, XmNforeground, &gv.foreground, NULL);
      cur_gc = XtGetGC(w_env, GCForeground | GCFunction, &gv);
      ax = (axis_context *)CALLOC(1, sizeof(axis_context));
      ax->wn = XtWindow(w_env);
      ax->dp = XtDisplay(w_env);
      ax->gc = cur_gc;
    }
  else clear_window(ax);
  mix_id = current_mix_id();
  chans = mix_input_chans_from_id(mix_id);
  e = mix_panel_envs(mix_id);
  for (chan = 0; chan < chans; chan++)
    {
      edp_display_graph(spfs[chan], _("mix env"), ax, 
			(int)(chan * widget_width(w) / chans), 0,
			widget_width(w) / chans, widget_height(w), 
			e[chan], false, true);
      cur_env = mix_amp_env_from_id(mix_id, chan);
      if (cur_env)
	{
	  XSetForeground(MAIN_DISPLAY(ss), ax->gc, (ss->sgx)->enved_waveform_color);
	  edp_display_graph(spfs[chan], _("mix env"), ax, 
			    (int)(chan * widget_width(w) / chans), 0,
			    widget_width(w) / chans, widget_height(w), 
			    cur_env, false, false);
	  XSetForeground(MAIN_DISPLAY(ss), ax->gc, (ss->sgx)->black);
	}
    }
}

#ifdef MAC_OSX
static int press_x, press_y;
#endif

static void mix_drawer_button_motion(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  XMotionEvent *ev = (XMotionEvent *)event;
  int mix_id, chans, chan;
  env *e;
  Float pos;
#ifdef MAC_OSX
  if ((press_x == ev->x) && (press_y == ev->y)) return;
#endif
  mix_id = current_mix_id();
  chans = mix_input_chans_from_id(mix_id);
  pos = (Float)(ev->x) / (Float)widget_width(w);
  chan = (int)(pos * chans);
  last_clicked_env_chan = chan;
  e = mix_panel_env(mix_id, chan);
  edp_handle_point(spfs[chan], ev->x, ev->y, ev->time, e, false, 1.0);
  mix_amp_env_resize(w, NULL, NULL);
}

static void mix_drawer_button_press(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  XButtonEvent *ev = (XButtonEvent *)event;
  int mix_id, chans, chan;
  env *e;
  Float pos;
#ifdef MAC_OSX
  press_x = ev->x;
  press_y = ev->y;
#endif
  mix_id = current_mix_id();
  chans = mix_input_chans_from_id(mix_id);
  pos = (Float)(ev->x) / (Float)widget_width(w);
  chan = (int)(pos * chans);
  last_clicked_env_chan = chan;
  e = mix_panel_env(mix_id, chan);
  if (edp_handle_press(spfs[chan], ev->x, ev->y, ev->time, e, false, 1.0))
    mix_amp_env_resize(w, NULL, NULL);
}

static void mix_drawer_button_release(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  XButtonEvent *ev = (XButtonEvent *)event;
  int mix_id, chans, chan;
  env *e;
  Float pos;
  mix_id = current_mix_id();
  chans = mix_input_chans_from_id(mix_id);
  pos = (Float)(ev->x) / (Float)widget_width(w);
  chan = (int)(pos * chans);
  last_clicked_env_chan = chan;
  e = mix_panel_env(mix_id, chan);
  edp_handle_release(spfs[chan], e);
  mix_amp_env_resize(w, NULL, NULL);
}



/* ---------------- MIX PANEL ---------------- */

static Widget w_id = NULL, w_beg = NULL, w_track = NULL, mix_play = NULL, track_play = NULL;

static void track_activated(void)
{
  char *val;
  val = XmTextGetString(w_track);
  if (val)
    {
      set_mix_track_from_id(current_mix_id(), string2int(val));
      XtFree(val);
    }
}

static void id_activated(void)
{
  char *val;
  int id;
  val = XmTextGetString(w_id);
  if (val)
    {
      id = string2int(val);
      if (mix_ok_and_unlocked(id))
	{
	  select_mix_from_id(id);
	  update_mix_panel(ss->selected_mix);
	}
      XtFree(val);
    }
}

static void beg_activated(void)
{
  char *val;
  chan_info *cp;
  int mix_id;
  val = XmTextGetString(w_beg);
  if (val)
    {
      mix_id = current_mix_id();
      cp = mix_channel_from_id(mix_id);
      set_mix_position(mix_id, (off_t)(string2Float(val) * SND_SRATE(cp->sound)));
      update_mix_panel(mix_id);
      XtFree(val);
    }
}

static void apply_mix_panel_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* set all mix amp envs, last one should remix */
  int i, chans, mix_id;
  env **envs;
  mix_id = current_mix_id();
  chans = mix_input_chans_from_id(mix_id);
  envs = mix_panel_envs(mix_id);
  for (i = 0; i < chans; i++)
    if (i != last_clicked_env_chan)
      set_mix_amp_env_without_edit(mix_id, i, envs[i]);
  set_mix_amp_env_from_gui(mix_id, last_clicked_env_chan, envs[last_clicked_env_chan]);
  mix_amp_env_resize(w_env, NULL, NULL);
}

static void dismiss_mix_panel_callback(Widget w, XtPointer context, XtPointer info) 
{
  state_context *sgx;
  XmAnyCallbackStruct *cb = (XmAnyCallbackStruct *)info;
  sgx = ss->sgx;
  if (cb->event != sgx->text_activate_event)
    XtUnmanageChild(mix_panel);
  else
    {
      if (sgx->text_widget == w_track)
	track_activated();
      else
	{
	  if (sgx->text_widget == w_id)
	    id_activated();
	  else
	    {
	      if (sgx->text_widget == w_beg)
		beg_activated();
	    }
	}
    }
}

static void help_mix_panel_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help("Mix Panel",
"This dialog provides various commonly-used controls on the currently \
selected mix.  At the top are the mix id, begin and end times, \
track number, and a play button.  Beneath that are various sliders \
controlling the speed (sampling rate) of the mix, and the amplitude of each \
input channel; and finally, an envelope editor for the mix's (input) channels. \
The current mix amp env is not actually changed until you click 'Apply Env'.\
The editor envelope is drawn in black with dots whereas the current \
mix amp env (if any) is drawn in blue.",
	   true);
}


static bool mix_playing = false;
bool mix_play_stopped(void) {return(!mix_playing);}

void reflect_mix_play_stop(void)
{
  if (mix_play) 
    {
      XmChangeColor(mix_play, (ss->sgx)->basic_color);
      XmChangeColor(track_play, (ss->sgx)->basic_color);
    }
  mix_playing = false;
}

static void mix_panel_play_callback(Widget w, XtPointer context, XtPointer info) 
{
  if (mix_playing)
    reflect_mix_play_stop();
  else
    {
      mix_playing = true;
      if (mix_play) XmChangeColor(mix_play, (ss->sgx)->pushed_button_color);
      mix_play_from_id(current_mix_id());
    }
}

static void track_panel_play_callback(Widget w, XtPointer context, XtPointer info) 
{
  if (mix_playing)
    reflect_mix_play_stop();
  else
    {
      mix_playing = true;
      if (track_play) XmChangeColor(track_play, (ss->sgx)->pushed_button_color);
      track_play_from_id(current_mix_id());
    }
}

static Widget nextb, previousb;

static void mix_next_callback(Widget w, XtPointer context, XtPointer info)
{
  int id;
  id = next_mix_id(current_mix_id());
  if (id != INVALID_MIX_ID)
    {
      select_mix_from_id(id);
      update_mix_panel(ss->selected_mix);
      if (next_mix_id(id) == INVALID_MIX_ID) 
	set_sensitive(nextb, false);
    }
}

static void mix_previous_callback(Widget w, XtPointer context, XtPointer info)
{
  int id;
  id = previous_mix_id(current_mix_id());
  if (id != INVALID_MIX_ID)
    {
      select_mix_from_id(id);
      update_mix_panel(ss->selected_mix);
      if (previous_mix_id(id) == INVALID_MIX_ID) 
	set_sensitive(previousb, false);
    }
}

#define p_speaker_width 12
#define p_speaker_height 12
static unsigned char p_speaker_bits[] = {
   0x00, 0x07, 0xc0, 0x04, 0x30, 0x04, 0x0e, 0x04, 0x06, 0x04, 0x06, 0x04,
   0x06, 0x04, 0x06, 0x04, 0x0e, 0x04, 0x30, 0x04, 0xc0, 0x04, 0x00, 0x07};

static Widget w_sep1;

Widget make_mix_panel(void) 
{
  Widget mainform, mix_row, track_row, last_label, last_number, mix_frame, track_frame, sep;
  Pixmap speaker_r;
  XmString xdismiss, xhelp, xtitle, s1, xapply;
  int n, i;
  Arg args[20];
  XtCallbackList n1, n2;
  GC gc;
  int depth;
  XGCValues v;
  char amplab[LABEL_BUFFER_SIZE];
  if (mix_panel == NULL)
    {
      xdismiss = XmStringCreate(_("Dismiss"), XmFONTLIST_DEFAULT_TAG);
      xapply = XmStringCreate(_("Apply Env"), XmFONTLIST_DEFAULT_TAG);
      xhelp = XmStringCreate(_("Help"), XmFONTLIST_DEFAULT_TAG);
      xtitle = XmStringCreate(_("Mix Panel"), XmFONTLIST_DEFAULT_TAG);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNokLabelString, xdismiss); n++;
      XtSetArg(args[n], XmNcancelLabelString, xapply); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNdialogTitle, xtitle); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNtransient, false); n++;
      mix_panel = XmCreateTemplateDialog(MAIN_SHELL(ss), _("Mix Panel"), args, n);

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;
	  XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;
	}
      previousb = XtCreateManagedWidget(_("Previous"), xmPushButtonGadgetClass, mix_panel, args, n);
      if (previous_mix_id(current_mix_id()) == INVALID_MIX_ID) 
	set_sensitive(previousb, false);
      XtAddCallback(previousb, XmNactivateCallback, mix_previous_callback, NULL);
      nextb = XtCreateManagedWidget(_("Next"), xmPushButtonGadgetClass, mix_panel, args, n);
      XtAddCallback(nextb, XmNactivateCallback, mix_next_callback, NULL);
      if (next_mix_id(current_mix_id()) == INVALID_MIX_ID) 
	set_sensitive(nextb, false);

      XtAddCallback(mix_panel, XmNokCallback, dismiss_mix_panel_callback, NULL);
      XtAddCallback(mix_panel, XmNcancelCallback, apply_mix_panel_callback, NULL);
      XtAddCallback(mix_panel, XmNhelpCallback, help_mix_panel_callback, NULL);

      XmStringFree(xhelp);
      XmStringFree(xapply);
      XmStringFree(xdismiss);
      XmStringFree(xtitle);

      if (!(ss->using_schemes))
	{
	  XtVaSetValues(XmMessageBoxGetChild(mix_panel, XmDIALOG_CANCEL_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(mix_panel, XmDIALOG_OK_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(mix_panel, XmDIALOG_HELP_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	}

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, XmMessageBoxGetChild(mix_panel, XmDIALOG_SEPARATOR)); n++;
      mainform = XtCreateManagedWidget("formd", xmFormWidgetClass, mix_panel, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 2); n++;
      mix_frame = XtCreateManagedWidget("mix-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      mix_row = XtCreateManagedWidget("mix-panel-row", xmRowColumnWidgetClass, mix_frame, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtCreateManagedWidget(_("mix:"), xmLabelWidgetClass, mix_row, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNresizeWidth, false); n++;
      XtSetArg(args[n], XmNcolumns, 3); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      w_id = make_textfield_widget("mix-id", mix_row, args, n, ACTIVATABLE, NO_COMPLETER);
      XmTextSetString(w_id, "0");

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      w_beg = make_textfield_widget("mix-times", mix_row, args, n, ACTIVATABLE, NO_COMPLETER);
      XmTextSetString(w_beg, "0.000 : 1.000");

      XtVaGetValues(mix_row, XmNforeground, &v.foreground, XmNbackground, &v.background, XmNdepth, &depth, NULL);
      gc = XtGetGC(mix_row, GCForeground | GCBackground, &v);
      speaker_r = make_pixmap(p_speaker_bits, p_speaker_width, p_speaker_height, depth, gc);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNlabelType, XmPIXMAP); n++;
      XtSetArg(args[n], XmNlabelPixmap, speaker_r); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;}
      mix_play = XtCreateManagedWidget("mix-play", xmPushButtonWidgetClass, mix_row, args, n);
      XtAddCallback(mix_play, XmNactivateCallback, mix_panel_play_callback, NULL);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 2); n++;
      track_frame = XtCreateManagedWidget("track-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      track_row = XtCreateManagedWidget("track-panel-row", xmRowColumnWidgetClass, track_frame, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtCreateManagedWidget(_("track:"), xmLabelWidgetClass, track_row, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNresizeWidth, false); n++;
      XtSetArg(args[n], XmNcolumns, 3); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      w_track = make_textfield_widget("mix-track", track_row, args, n, ACTIVATABLE, NO_COMPLETER);
      XmTextSetString(w_track, "0");

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNlabelType, XmPIXMAP); n++;
      XtSetArg(args[n], XmNlabelPixmap, speaker_r); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;}
      track_play = XtCreateManagedWidget("track-play", xmPushButtonWidgetClass, track_row, args, n);
      XtAddCallback(track_play, XmNactivateCallback, track_panel_play_callback, NULL);


      /* separator before sliders */
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, mix_row); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNheight, 10); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      sep = XtCreateManagedWidget("mix-panel-sep", xmSeparatorWidgetClass, mainform, args, n);

      /* SRATE */
      n = 0;
      s1 = XmStringCreate(_("speed:"), XmFONTLIST_DEFAULT_TAG);
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      w_speed_label = make_pushbutton_widget("speed-label", mainform, args, n);
      XtAddCallback(w_speed_label, XmNactivateCallback, speed_click_callback, NULL);
      XmStringFree(s1);

      n = 0;
      s1 = initial_speed_label();
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_speed_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, w_speed_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      w_speed_number = XtCreateManagedWidget("srate-number", xmLabelWidgetClass, mainform, args, n);
      XmStringFree(s1);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->position_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_speed_number); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, w_speed_number); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNvalue, SPEED_SCROLLBAR_MID); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNdragCallback, n1 = make_callback_list(speed_drag_callback, NULL)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n2 = make_callback_list(speed_valuechanged_callback, NULL)); n++;
      w_speed = XtCreateManagedWidget("speed", xmScrollBarWidgetClass, mainform, args, n);
  
      FREE(n1);
      FREE(n2);
      last_label = w_speed_label;
      last_number = w_speed_number;

      /* now amp scalers */

      w_amp_numbers = (Widget *)CALLOC(CHANS_ALLOCATED, sizeof(Widget));
      w_amp_labels = (Widget *)CALLOC(CHANS_ALLOCATED, sizeof(Widget));
      w_amps = (Widget *)CALLOC(CHANS_ALLOCATED, sizeof(Widget));
      current_amps = (Float *)CALLOC(CHANS_ALLOCATED, sizeof(Float));

      for (i = 0; i < CHANS_ALLOCATED; i++)
	{
	  n = 0;
	  mus_snprintf(amplab, LABEL_BUFFER_SIZE, _("amp %d:"), i);
	  s1 = XmStringCreate(amplab, XmFONTLIST_DEFAULT_TAG);
	  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
	  XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNtopWidget, last_label); n++;
	  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
	  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
	  XtSetArg(args[n], XmNlabelString, s1); n++;
	  XtSetArg(args[n], XmNrecomputeSize, false); n++;
	  XtSetArg(args[n], XmNshadowThickness, 0); n++;
	  XtSetArg(args[n], XmNhighlightThickness, 0); n++;
	  XtSetArg(args[n], XmNfillOnArm, false); n++;
	  XtSetArg(args[n], XmNuserData, i); n++;
	  w_amp_labels[i] = make_pushbutton_widget("amp-label", mainform, args, n);
	  XtAddCallback(w_amp_labels[i], XmNactivateCallback, amp_click_callback, NULL);
	  XmStringFree(s1);

	  n = 0;
	  s1 = XmStringCreate(amp_number_buffer, XmFONTLIST_DEFAULT_TAG);
	  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
	  XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNtopWidget, last_number); n++;
	  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNleftWidget, w_amp_labels[i]); n++;
	  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
	  XtSetArg(args[n], XmNlabelString, s1); n++;
	  XtSetArg(args[n], XmNrecomputeSize, false); n++;
	  w_amp_numbers[i] = XtCreateManagedWidget("amp-number", xmLabelWidgetClass, mainform, args, n);
	  XmStringFree(s1);

	  n = 0;      
	  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->position_color); n++;}
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
	  XtSetArg(args[n], XmNtopWidget, w_amp_numbers[i]); n++;
	  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNleftWidget, w_amp_numbers[i]); n++;
	  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
	  XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
	  XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
	  XtSetArg(args[n], XmNuserData, i); n++;
	  XtSetArg(args[n], XmNvalue, 0); n++;  /* fixed up later; current_amp[chan] initial value is 0.0 */
	  XtSetArg(args[n], XmNdragCallback, n1 = make_callback_list(amp_drag_callback, NULL)); n++;
	  XtSetArg(args[n], XmNvalueChangedCallback, n2 = make_callback_list(amp_valuechanged_callback, NULL)); n++;
	  w_amps[i] = XtCreateManagedWidget("amp", xmScrollBarWidgetClass, mainform, args, n);
	  FREE(n1);
	  FREE(n2);
	  last_label = w_amp_labels[i];
	  last_number = w_amp_numbers[i];
	}

      /* separator before envelopes */
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, last_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNheight, 8); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      w_sep1 = XtCreateManagedWidget("mix-panel-sep1", xmSeparatorWidgetClass, mainform, args, n);

      /* amp env */
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_sep1); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 4); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 98); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 4); n++;
      w_env_frame = XtCreateManagedWidget("amp-env-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      w_env = XtCreateManagedWidget("amp-env-window", xmDrawingAreaWidgetClass, w_env_frame, args, n);

      XtManageChild(mix_panel);

      XtAddCallback(w_env, XmNresizeCallback, mix_amp_env_resize, NULL);
      XtAddCallback(w_env, XmNexposeCallback, mix_amp_env_resize, NULL);

      for (i = 0; i < CHANS_ALLOCATED; i++) spfs[i] = new_env_editor();

      XtAddEventHandler(w_env, ButtonPressMask, false, mix_drawer_button_press, NULL);
      XtAddEventHandler(w_env, ButtonMotionMask, false, mix_drawer_button_motion, NULL);
      XtAddEventHandler(w_env, ButtonReleaseMask, false, mix_drawer_button_release, NULL);

      set_dialog_widget(MIX_PANEL_DIALOG, mix_panel);
      speed_number_buffer[1] = local_decimal_point();
      amp_number_buffer[1] = local_decimal_point();
    }
  else raise_dialog(mix_panel);
  if (!(XtIsManaged(mix_panel))) XtManageChild(mix_panel);

  update_mix_panel(current_mix_id());
  return(mix_panel);
}

static void update_mix_panel(int mix_id) 
{
  chan_info *cp;
  int i, chans;
  off_t beg, len;
  Float val;
  char lab[LABEL_BUFFER_SIZE];
  if (mix_id == INVALID_MIX_ID) return;
  if (mix_id == current_mix_id())
    {
      if (mix_panel == NULL) 
	make_mix_panel();
      else
	{
	  set_sensitive(nextb, (next_mix_id(mix_id) != INVALID_MIX_ID));
	  set_sensitive(previousb, (previous_mix_id(mix_id) != INVALID_MIX_ID));
	}
      /* now reflect current mix state in mix panel controls */
      cp = mix_channel_from_id(mix_id);

      val = mix_speed_from_id(mix_id);
      if (val != current_speed)
	{
	  XtVaSetValues(w_speed, XmNvalue, mix_speed_to_int(val, cp->sound), NULL);
	  current_speed = val;
	}

      mus_snprintf(lab, LABEL_BUFFER_SIZE, "%d", mix_track_from_id(mix_id));
      XmTextSetString(w_track, lab);

      mus_snprintf(lab, LABEL_BUFFER_SIZE, "%d", mix_id);
      XmTextSetString(w_id, lab);

      beg = mix_position_from_id(mix_id);
      len = mix_frames(mix_id);
      mus_snprintf(lab, LABEL_BUFFER_SIZE, "%.3f : %.3f",
		   (float)((double)beg / (float)SND_SRATE(cp->sound)),
		   (float)((double)(beg + len) / (float)SND_SRATE(cp->sound)));
      XmTextSetString(w_beg, lab);

      chans = mix_input_chans_from_id(mix_id);
      if (chans > 8) chans = 8; 

      for (i = 0; i < chans; i++)
	{
	  XmString s1;
	  char amplab[LABEL_BUFFER_SIZE];
	  if ((i == 0) && (chans == 1))
	    mus_snprintf(amplab, LABEL_BUFFER_SIZE, _("amp:"));
	  else mus_snprintf(amplab, LABEL_BUFFER_SIZE, _("amp %d:"), i);
	  s1 = XmStringCreate(amplab, XmFONTLIST_DEFAULT_TAG);
	  XtVaSetValues(w_amp_labels[i], XmNlabelString, s1, NULL);
	  XmStringFree(s1);
	  if (!(XtIsManaged(w_amp_labels[i]))) XtManageChild(w_amp_labels[i]);
	  if (!(XtIsManaged(w_amp_numbers[i]))) XtManageChild(w_amp_numbers[i]);
	  val = mix_amp_from_id(mix_id, i);
	  XtVaSetValues(w_amps[i], XmNvalue, mix_amp_to_int(val, i), NULL);
	  current_amps[i] = val;
	  if (!(XtIsManaged(w_amps[i]))) XtManageChild(w_amps[i]);
	}
      for (i = chans; i < CHANS_ALLOCATED; i++)
	{
	  if ((w_amp_labels[i]) && (XtIsManaged(w_amp_labels[i])))
	    {
	      XtUnmanageChild(w_amp_labels[i]);
	      XtUnmanageChild(w_amp_numbers[i]);
	      XtUnmanageChild(w_amps[i]);
	    }
	}
      XtVaSetValues(w_sep1, XmNtopWidget, w_amp_labels[chans - 1], NULL);
      mix_amp_env_resize(w_env, NULL, NULL);
    }
}

void reflect_mix_in_mix_panel(int mix_id)
{
  if ((mix_panel) && 
      (XtIsManaged(mix_panel)))
    {
      if (current_mix_id() == mix_id)
	update_mix_panel(mix_id);
      if (mix_id != INVALID_MIX_ID)
	{
	  set_sensitive(nextb, (next_mix_id(current_mix_id()) != INVALID_MIX_ID));
	  set_sensitive(previousb, (previous_mix_id(current_mix_id()) != INVALID_MIX_ID));
	}
    }
}

void reflect_no_mix_in_mix_panel(void)
{
  if ((mix_panel) &&
      (XtIsManaged(mix_panel)))
    XtUnmanageChild(mix_panel);
}
