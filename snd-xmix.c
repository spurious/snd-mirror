#include "snd.h"

#define NAME_COLUMNS 8
#define DIALOG_WIDTH 600
#define DIALOG_HEIGHT 350

/* ---------------- mix dialog ---------------- */

static Widget mix_dialog = NULL;
static bool mix_dialog_slider_dragging = false;
static void update_mix_dialog(int mix_id);
static int mix_dialog_id = INVALID_MIX_ID;


/* -------- speed -------- */
static Widget w_speed_number, w_speed_label, w_speed;

static int speed_to_scrollbar(Float minval, Float val, Float maxval)
{
  if (val <= minval) return(0);
  if (val >= maxval) return((int)(0.9 * SCROLLBAR_MAX));
  return(snd_round(0.9 * SCROLLBAR_MAX * ((log(val) - log(minval)) / (log(maxval) - log(minval)))));
}

static Float set_speed_label(Widget speed_number, int ival)
{
  char speed_number_buffer[6];
  Float speed;
  speed = speed_changed(exp((ival * (log(speed_control_max(ss)) - log(speed_control_min(ss))) / (0.9 * SCROLLBAR_MAX)) + log(speed_control_min(ss))),
			speed_number_buffer,
			mix_speed_style(mix_dialog_id),
			speed_control_tones(ss),
			6);
  set_label(speed_number, speed_number_buffer);
  return(speed);
}

static void speed_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  char speed_number_buffer[6];
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return;
  mix_dialog_set_mix_speed(mix_dialog_id, 
			   speed_changed(1.0,
					 speed_number_buffer,
					 mix_speed_style(mix_dialog_id),
					 speed_control_tones(ss),
					 6),
			   mix_dialog_slider_dragging);
  set_label(w_speed_number, speed_number_buffer);
  XtVaSetValues(w_speed, XmNvalue, speed_to_scrollbar(speed_control_min(ss), 1.0, speed_control_max(ss)), NULL);
}

static void speed_label_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  char speed_number_buffer[6];
  ASSERT_WIDGET_TYPE(XmIsPushButton(w), w);
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return;
  switch (mix_speed_style(mix_dialog_id))
    {
    default:
    case SPEED_CONTROL_AS_FLOAT:    set_mix_speed_style(mix_dialog_id, SPEED_CONTROL_AS_RATIO, true);    break;
    case SPEED_CONTROL_AS_RATIO:    set_mix_speed_style(mix_dialog_id, SPEED_CONTROL_AS_SEMITONE, true); break;
    case SPEED_CONTROL_AS_SEMITONE: set_mix_speed_style(mix_dialog_id, SPEED_CONTROL_AS_FLOAT, true);    break;
    }
  speed_changed(mix_dialog_mix_speed(mix_dialog_id),
		speed_number_buffer,
		mix_speed_style(mix_dialog_id),
		speed_control_tones(ss),
		6);
  set_label(w_speed_number, speed_number_buffer);
}

static void speed_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  int ival;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return;
  ival = ((XmScrollBarCallbackStruct *)info)->value;
  if (!mix_dialog_slider_dragging) mix_dialog_start_drag(mix_dialog_id);
  mix_dialog_slider_dragging = true;
  mix_dialog_set_mix_speed(mix_dialog_id, 
			   set_speed_label(w_speed_number, ival),
			   true);
}

static void speed_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return;
  mix_dialog_slider_dragging = false;
  mix_dialog_set_mix_speed(mix_dialog_id, 
			   set_speed_label(w_speed_number, cb->value),
			   false);
}


/* -------- amp -------- */
static Widget *w_amp_numbers, *w_amp_labels, *w_amps;
static Float *current_amps;
#define CHANS_ALLOCATED 8

static Float scrollbar_to_amp(int val)
{
  if (val <= 0) 
    return(amp_control_min(ss));
  if (val >= (0.9 * SCROLLBAR_MAX)) 
    return(amp_control_max(ss));
  if (val > (0.5 * 0.9 * SCROLLBAR_MAX))
    return((((val / (0.5 * 0.9 * SCROLLBAR_MAX)) - 1.0) * (amp_control_max(ss) - 1.0)) + 1.0);
  else return((val * (1.0 - amp_control_min(ss)) / (0.5 * 0.9 * SCROLLBAR_MAX)) + amp_control_min(ss));
}

static int amp_to_scrollbar(Widget amp_number, Float amp)
{
  char sfs[6];
  mus_snprintf(sfs, 6, "%.2f", amp);
  set_label(amp_number, sfs);
  return(amp_to_scroll(amp_control_min(ss), amp, amp_control_max(ss)));
}

static void change_mix_amp(int mix_id, int chan, Float val)
{
  char sfs[6];
  mix_dialog_set_mix_amp(mix_id, chan, val, mix_dialog_slider_dragging);
  mus_snprintf(sfs, 6, "%.2f", val);
  set_label(w_amp_numbers[chan], sfs);
}

static void amp_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  int chan;
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return;
  XtVaGetValues(w, XmNuserData, &chan, NULL);
  change_mix_amp(mix_dialog_id, chan, 1.0);
  XtVaSetValues(w_amps[chan], XmNvalue, amp_to_scroll(amp_control_min(ss), 1.0, amp_control_max(ss)), NULL);
}

static void amp_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  int ival, chan;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return;
  XtVaGetValues(w, XmNuserData, &chan, NULL);
  ival = ((XmScrollBarCallbackStruct *)info)->value;
  if (!mix_dialog_slider_dragging) mix_dialog_start_drag(mix_dialog_id);
  mix_dialog_slider_dragging = true;
  change_mix_amp(mix_dialog_id, chan, scrollbar_to_amp(ival));
}

static void amp_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  int ival, chan;
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return;
  ival = ((XmScrollBarCallbackStruct *)info)->value;
  XtVaGetValues(w, XmNuserData, &chan, NULL);
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  mix_dialog_slider_dragging = false;
  change_mix_amp(mix_dialog_id, chan, scrollbar_to_amp(ival));
}


/* -------- amp-envs -------- */
static Widget w_env_frame, w_env;
static axis_context *ax = NULL;
static GC cur_gc;
static env_editor *spfs[8];
static int last_clicked_env_chan = 0;
static bool with_mix_background_wave = false;

static void show_mix_background_wave(int mix_id, int chan)
{
  env_editor *e;
  int pts;
  bool two_sided = false;
  e = spfs[chan];
  if (e == NULL) return;
  pts = prepare_mix_id_waveform(mix_id, e->axis, &two_sided);
  if (pts > 0)
    {
      XSetForeground(MAIN_DISPLAY(ss), ax->gc, ss->sgx->enved_waveform_color);
      if (two_sided)
	draw_both_grf_points(1, ax, pts, GRAPH_LINES);
      else draw_grf_points(1, ax, pts, e->axis, ungrf_y(e->axis, 0.0), GRAPH_LINES);
      XSetForeground(MAIN_DISPLAY(ss), ax->gc, ss->sgx->black);
    }
}

static void mix_amp_env_resize(Widget w, XtPointer context, XtPointer info) 
{
  int chans, chan;
  env **e;
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return;
  if (ax == NULL)
    {
      XGCValues gv;
      gv.function = GXcopy;
      XtVaGetValues(w_env, XmNbackground, &gv.background, XmNforeground, &gv.foreground, NULL);
      cur_gc = XtGetGC(w_env, GCForeground | GCFunction, &gv);
      ax = (axis_context *)CALLOC(1, sizeof(axis_context));
      ax->wn = XtWindow(w_env);
      ax->dp = XtDisplay(w_env);
      ax->gc = cur_gc;
    }
  else clear_window(ax);
  chans = mix_dialog_mix_input_chans(mix_dialog_id);
  e = mix_dialog_envs(mix_dialog_id);
  for (chan = 0; chan < chans; chan++)
    {
      env *cur_env;
      spfs[chan]->with_dots = true;
      env_editor_display_env(spfs[chan], e[chan], ax, _("mix env"), (int)(chan * widget_width(w) / chans), 0,
			     widget_width(w) / chans, widget_height(w), NOT_PRINTING);
      cur_env = mix_dialog_mix_amp_env(mix_dialog_id, chan);
      if (cur_env)
	{
	  XSetForeground(MAIN_DISPLAY(ss), ax->gc, ss->sgx->enved_waveform_color);
	  spfs[chan]->with_dots = false;
	  env_editor_display_env(spfs[chan], cur_env, ax, _("mix env"), (int)(chan * widget_width(w) / chans), 0,
				 widget_width(w) / chans, widget_height(w), NOT_PRINTING);
	  XSetForeground(MAIN_DISPLAY(ss), ax->gc, ss->sgx->black);
	}
      if (with_mix_background_wave)
	show_mix_background_wave(mix_dialog_id, chan);
    }
}

#ifdef MUS_MAC_OSX
static int press_x, press_y;
#endif

static void mix_drawer_button_motion(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  XMotionEvent *ev = (XMotionEvent *)event;
  int chans, chan;
  env *e;
  Float pos;
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return;
#ifdef MUS_MAC_OSX
  if ((press_x == ev->x) && (press_y == ev->y)) return;
#endif
  chans = mix_dialog_mix_input_chans(mix_dialog_id);
  pos = (Float)(ev->x) / (Float)widget_width(w);
  chan = (int)(pos * chans);
  last_clicked_env_chan = chan;
  e = mix_dialog_env(mix_dialog_id, chan);
  env_editor_button_motion(spfs[chan], ev->x, ev->y, ev->time, e);
  mix_amp_env_resize(w, NULL, NULL);
}

static void mix_drawer_button_press(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  XButtonEvent *ev = (XButtonEvent *)event;
  int chans, chan;
  env *e;
  Float pos;
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return;
#ifdef MUS_MAC_OSX
  press_x = ev->x;
  press_y = ev->y;
#endif
  chans = mix_dialog_mix_input_chans(mix_dialog_id);
  pos = (Float)(ev->x) / (Float)widget_width(w);
  chan = (int)(pos * chans);
  last_clicked_env_chan = chan;
  e = mix_dialog_env(mix_dialog_id, chan);
  if (env_editor_button_press(spfs[chan], ev->x, ev->y, ev->time, e))
    mix_amp_env_resize(w, NULL, NULL);
}

static void mix_drawer_button_release(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  XButtonEvent *ev = (XButtonEvent *)event;
  int chans, chan;
  env *e;
  Float pos;
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return;
  chans = mix_dialog_mix_input_chans(mix_dialog_id);
  pos = (Float)(ev->x) / (Float)widget_width(w);
  chan = (int)(pos * chans);
  last_clicked_env_chan = chan;
  e = mix_dialog_env(mix_dialog_id, chan);
  env_editor_button_release(spfs[chan], e);
  mix_amp_env_resize(w, NULL, NULL);
}


/* -------- mix track -------- */
static Widget w_id = NULL, w_beg = NULL, w_track = NULL, mix_play = NULL, mix_track_play = NULL, w_mix_pan = NULL;

static bool track_changed = false;

static Widget error_frame = NULL, error_label = NULL;

static void clear_mix_error(void)
{
  if ((error_frame) && (XtIsManaged(error_frame)))
    XtUnmanageChild(error_frame);
}

static void unpost_mix_error(XtPointer data, XtIntervalId *id)
{
  clear_mix_error();
}

static void errors_to_mix_text(const char *msg, void *data)
{
  int lines = 0;
  XmString label;
  label = multi_line_label(msg, &lines);
  XtVaSetValues(error_label, 
		XmNlabelString, label, 
		XmNheight, lines * 20,
		NULL);
  XtVaSetValues(error_frame, XmNheight, lines * 20, NULL);
  XmStringFree(label);
  XtManageChild(error_frame);
  /* since the offending text is automatically overwritten, we can't depend on subsequent text modify callbacks
   *   to clear things, so we'll just use a timer
   */
  XtAppAddTimeOut(MAIN_APP(ss),
		  5000,
		  (XtTimerCallbackProc)unpost_mix_error,
		  NULL);
}

static void widget_track_to_text(Widget w, int trk)
{
  if (track_name(trk))
    XmTextFieldSetString(w, track_name(trk));
  else widget_int_to_text(w, trk);
}

static void widget_mix_to_text(Widget w, int id)
{
  if (mix_name(id))
    XmTextFieldSetString(w, mix_name(id));
  else widget_int_to_text(w, id);
}

static void track_activated(void)
{
  char *val;
  track_changed = false;
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return;
  val = XmTextGetString(w_track);
  if (val)
    {
      int trk;
      /* look for a track name first, then a number */
      trk = track_name_to_id(val);
      if (trk < 0)
	{
	  redirect_errors_to(errors_to_mix_text, NULL);
	  trk = string_to_int(val, 0, "track");
	  redirect_errors_to(NULL, NULL);
	}
      if (trk >= 0)
	mix_dialog_set_mix_track(mix_dialog_id, trk);
      else widget_track_to_text(w_track, mix_dialog_mix_track(mix_dialog_id));
      XtFree(val);
    }
}

static void track_modify_callback(Widget w, XtPointer context, XtPointer info) 
{
  track_changed = true;
}

static void track_check_callback(Widget w, XtPointer context, XtPointer info)
{
  /* if user changes id, but forgets <cr>, then moves mouse away, we update as if <cr> */
  if (track_changed) track_activated();
}


static bool id_changed = false;

static void id_activated(void)
{
  char *val;
  id_changed = false;
  val = XmTextGetString(w_id);
  if (val)
    {
      int id;
      /* look for a mix name first, then a number */
      id = mix_name_to_id(val);
      if (id < 0)
	{
	  redirect_errors_to(errors_to_mix_text, NULL);
	  id = string_to_int(val, 0, "id");
	  redirect_errors_to(NULL, NULL);
	}
      if (mix_ok_and_unlocked(id))
	{
	  mix_dialog_id = id;
	  update_mix_dialog(id);
	}
      XtFree(val);
    }
}

static void id_modify_callback(Widget w, XtPointer context, XtPointer info) 
{
  id_changed = true;
}

static void id_check_callback(Widget w, XtPointer context, XtPointer info)
{
  if (id_changed) id_activated();
}

static void beg_activated(void)
{
  char *val;
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return;
  val = XmTextGetString(w_beg);
  if (val)
    {
      chan_info *cp;
      char *up_to_colon;
      Float beg;
      cp = mix_dialog_mix_channel(mix_dialog_id);
      up_to_colon = string_to_colon(val);
      redirect_errors_to(errors_to_mix_text, NULL);
      beg = string_to_Float(up_to_colon, 0.0, "begin time");
      redirect_errors_to(NULL, NULL);
      if (beg >= 0.0)
	set_mix_position(mix_dialog_id, (off_t)(beg * SND_SRATE(cp->sound)));
      update_mix_dialog(mix_dialog_id);
      FREE(up_to_colon);
      XtFree(val);
    }
}

static void apply_mix_dialog_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* set all mix amp envs, last one should remix */
  int i, chans;
  env **envs;
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return;
  chans = mix_dialog_mix_input_chans(mix_dialog_id);
  envs = mix_dialog_envs(mix_dialog_id);
  for (i = 0; i < chans; i++)
    if (i != last_clicked_env_chan)
      mix_dialog_set_mix_amp_env_without_edit(mix_dialog_id, i, envs[i]);
  set_mix_amp_env(mix_dialog_id, last_clicked_env_chan, envs[last_clicked_env_chan]);
  mix_amp_env_resize(w_env, NULL, NULL);
}

static void dismiss_mix_dialog_callback(Widget w, XtPointer context, XtPointer info) 
{
  Widget active_widget;
  clear_mix_error();
  active_widget = XmGetFocusWidget(mix_dialog);
  if (active_widget == XmMessageBoxGetChild(mix_dialog, XmDIALOG_OK_BUTTON))
    XtUnmanageChild(mix_dialog);
  else
    {
      if (active_widget == w_track)
	track_activated();
      else
	{
	  if (active_widget == w_id)
	    id_activated();
	  else
	    {
	      if (active_widget == w_beg)
		beg_activated();
	    }
	}
    }
}

static void help_mix_dialog_callback(Widget w, XtPointer context, XtPointer info) 
{
  mix_dialog_help();
}


/* -------- mix play -------- */
static bool mix_playing = false;
bool mix_play_stopped(void) {return(!mix_playing);}

void reflect_mix_play_stop(void)
{
  if (mix_play)
    {
      XmChangeColor(mix_play, ss->sgx->basic_color);
      XmChangeColor(mix_track_play, ss->sgx->basic_color);
    }
  mix_playing = false;
}

static void mix_dialog_play_callback(Widget w, XtPointer context, XtPointer info) 
{
  if (mix_playing)
    reflect_mix_play_stop();
  else
    {
      if (!(mix_ok(mix_dialog_id))) return;
      mix_playing = true;
      if (mix_play) XmChangeColor(mix_play, ss->sgx->pushed_button_color);
      mix_dialog_mix_play(mix_dialog_id);
    }
}

static void mix_track_dialog_play_callback(Widget w, XtPointer context, XtPointer info) 
{
  if (mix_playing)
    reflect_mix_play_stop();
  else
    {
      if (!(mix_ok(mix_dialog_id))) return;
      mix_playing = true;
      if (mix_track_play) XmChangeColor(mix_track_play, ss->sgx->pushed_button_color);
      mix_dialog_track_play(mix_dialog_id);
    }
}

static void mix_dB_callback(Widget w, XtPointer context, XtPointer info)
{
  int i;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info; 
  for (i = 0; i < CHANS_ALLOCATED; i++)
    if (spfs[i])
      spfs[i]->in_dB = cb->set;
  mix_amp_env_resize(w_env, NULL, NULL);
}

static void mix_clip_callback(Widget w, XtPointer context, XtPointer info)
{
  int i;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info; 
  for (i = 0; i < CHANS_ALLOCATED; i++)
    if (spfs[i])
      spfs[i]->clip_p = cb->set;
  mix_amp_env_resize(w_env, NULL, NULL);
}

static void mix_wave_callback(Widget w, XtPointer context, XtPointer info)
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info; 
  with_mix_background_wave = cb->set;
  mix_amp_env_resize(w_env, NULL, NULL);
}

static Widget nextb, previousb;

static void mix_next_callback(Widget w, XtPointer context, XtPointer info)
{
  int id;
  clear_mix_error();
  id = next_mix_id(mix_dialog_id);
  if (id != INVALID_MIX_ID)
    {
      mix_dialog_id = id;
      update_mix_dialog(id);
      if (next_mix_id(id) == INVALID_MIX_ID) 
	set_sensitive(nextb, false);
    }
}

static void mix_previous_callback(Widget w, XtPointer context, XtPointer info)
{
  int id;
  clear_mix_error();
  id = previous_mix_id(mix_dialog_id);
  if (id != INVALID_MIX_ID)
    {
      mix_dialog_id = id;
      update_mix_dialog(id);
      if (previous_mix_id(id) == INVALID_MIX_ID) 
	set_sensitive(previousb, false);
    }
}

static void mix_dialog_pan_callback(Widget w, XtPointer context, XtPointer info) 
{
  bool inverted;
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return;
  inverted = (!(mix_dialog_mix_inverted(mix_dialog_id)));
  XmChangeColor(w_mix_pan, (inverted) ? ss->sgx->yellow : ss->sgx->highlight_color);
  mix_dialog_set_mix_inverted(mix_dialog_id, inverted);
}

#define pan_width 14
#define pan_height 12
static unsigned char pan_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0x3f, 0x7f, 0x30, 0x60, 0x30,
   0x60, 0x30, 0x60, 0x30, 0xe0, 0x3f, 0xe0, 0x3f, 0x00, 0x00, 0x00, 0x00};

#define p_speaker_width 12
#define p_speaker_height 12
static unsigned char p_speaker_bits[] = {
   0x00, 0x07, 0xc0, 0x04, 0x30, 0x04, 0x0e, 0x04, 0x06, 0x04, 0x06, 0x04,
   0x06, 0x04, 0x06, 0x04, 0x0e, 0x04, 0x30, 0x04, 0xc0, 0x04, 0x00, 0x07};

static int mixer_depth;
static GC gc;
static Pixmap speaker_r, pan_r;
void make_mixer_icons_transparent_again(Pixel old_color, Pixel new_color)
{
  if (mix_dialog)
    {
      XFreePixmap(XtDisplay(mix_dialog), speaker_r);
      XSetBackground(XtDisplay(mix_dialog), gc, new_color);
      speaker_r = make_pixmap(p_speaker_bits, p_speaker_width, p_speaker_height, mixer_depth, gc);
      pan_r = make_pixmap(pan_bits, pan_width, pan_height, mixer_depth, gc);
      XtVaSetValues(mix_play, XmNlabelPixmap, speaker_r, NULL);
      XtVaSetValues(mix_track_play, XmNlabelPixmap, speaker_r, NULL);
      XtVaSetValues(w_mix_pan, XmNlabelPixmap, pan_r, NULL);
    }
}

static Widget w_sep1;

Widget make_mix_dialog(void) 
{
  if (mix_dialog == NULL)
    {
      Widget mainform, mix_row, track_row, last_label, last_number, mix_frame, track_frame, sep;
      Widget w_dB_frame, w_dB, w_clip, w_wave, w_dB_row;
      XmString xdismiss, xhelp, xtitle, s1, xapply;
      int n, i;
      Arg args[20];
      XtCallbackList n1, n2;
      XGCValues v;
      char amplab[LABEL_BUFFER_SIZE];

      mix_dialog_id = any_mix_id();
      xdismiss = XmStringCreateLocalized(_("Dismiss"));
      xapply = XmStringCreateLocalized(_("Apply Env"));
      xhelp = XmStringCreateLocalized(_("Help"));
      xtitle = XmStringCreateLocalized(_("Mixes"));

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNokLabelString, xdismiss); n++;
      XtSetArg(args[n], XmNcancelLabelString, xapply); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNdialogTitle, xtitle); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNtransient, false); n++;
      XtSetArg(args[n], XmNwidth, DIALOG_WIDTH); n++;
      mix_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), _("Mixes"), args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->doit_again_button_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;
      previousb = XtCreateManagedWidget(_("Previous"), xmPushButtonGadgetClass, mix_dialog, args, n);
      if (previous_mix_id(mix_dialog_id) == INVALID_MIX_ID) 
	set_sensitive(previousb, false);
      XtAddCallback(previousb, XmNactivateCallback, mix_previous_callback, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->reset_button_color); n++;
      XtSetArg(args[n], XmNforeground, ss->sgx->black); n++;
      XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;
      nextb = XtCreateManagedWidget(_("Next"), xmPushButtonGadgetClass, mix_dialog, args, n);
      XtAddCallback(nextb, XmNactivateCallback, mix_next_callback, NULL);
      if (next_mix_id(mix_dialog_id) == INVALID_MIX_ID) 
	set_sensitive(nextb, false);

      XtAddCallback(mix_dialog, XmNokCallback, dismiss_mix_dialog_callback, NULL);
      XtAddCallback(mix_dialog, XmNcancelCallback, apply_mix_dialog_callback, NULL);
      XtAddCallback(mix_dialog, XmNhelpCallback, help_mix_dialog_callback, NULL);

      XmStringFree(xhelp);
      XmStringFree(xapply);
      XmStringFree(xdismiss);
      XmStringFree(xtitle);

      XtVaSetValues(XmMessageBoxGetChild(mix_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(mix_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(mix_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(mix_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->doit_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(mix_dialog, XmDIALOG_OK_BUTTON), XmNbackground, ss->sgx->quit_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(mix_dialog, XmDIALOG_HELP_BUTTON), XmNbackground, ss->sgx->help_button_color, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, XmMessageBoxGetChild(mix_dialog, XmDIALOG_SEPARATOR)); n++;
      mainform = XtCreateManagedWidget("formd", xmFormWidgetClass, mix_dialog, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 2); n++;
      mix_frame = XtCreateManagedWidget("mix-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      mix_row = XtCreateManagedWidget("mix-dialog-row", xmRowColumnWidgetClass, mix_frame, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtCreateManagedWidget(_("mix:"), xmLabelWidgetClass, mix_row, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNresizeWidth, false); n++;
      XtSetArg(args[n], XmNcolumns, NAME_COLUMNS); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      w_id = make_textfield_widget("mix-id", mix_row, args, n, ACTIVATABLE, NO_COMPLETER);
      XtAddCallback(w_id, XmNlosingFocusCallback, id_check_callback, NULL);
      XtAddCallback(w_id, XmNmodifyVerifyCallback, id_modify_callback, NULL);
      XmTextSetString(w_id, "0");

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      w_beg = make_textfield_widget("mix-times", mix_row, args, n, ACTIVATABLE, NO_COMPLETER);
      XmTextSetString(w_beg, "0.000 : 1.000");

      XtVaGetValues(mix_row, XmNforeground, &v.foreground, XmNbackground, &v.background, XmNdepth, &mixer_depth, NULL);
      gc = XtGetGC(mix_row, GCForeground | GCBackground, &v);
      speaker_r = make_pixmap(p_speaker_bits, p_speaker_width, p_speaker_height, mixer_depth, gc);
      pan_r = make_pixmap(pan_bits, pan_width, pan_height, mixer_depth, gc);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNlabelType, XmPIXMAP); n++;
      XtSetArg(args[n], XmNlabelPixmap, speaker_r); n++;
      XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;
      mix_play = XtCreateManagedWidget("mix-play", xmPushButtonWidgetClass, mix_row, args, n);
      XtAddCallback(mix_play, XmNactivateCallback, mix_dialog_play_callback, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 2); n++;
      track_frame = XtCreateManagedWidget("mix-track-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      track_row = XtCreateManagedWidget("mix-track-dialog-row", xmRowColumnWidgetClass, track_frame, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtCreateManagedWidget(_("track:"), xmLabelWidgetClass, track_row, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNresizeWidth, false); n++;
      XtSetArg(args[n], XmNcolumns, NAME_COLUMNS); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      w_track = make_textfield_widget("mix-track", track_row, args, n, ACTIVATABLE, NO_COMPLETER);
      XtAddCallback(w_track, XmNlosingFocusCallback, track_check_callback, NULL);
      XtAddCallback(w_track, XmNmodifyVerifyCallback, track_modify_callback, NULL);
      XmTextSetString(w_track, "0");

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNlabelType, XmPIXMAP); n++;
      XtSetArg(args[n], XmNlabelPixmap, speaker_r); n++;
      XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;
      mix_track_play = XtCreateManagedWidget("mix-track-play", xmPushButtonWidgetClass, track_row, args, n);
      XtAddCallback(mix_track_play, XmNactivateCallback, mix_track_dialog_play_callback, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNlabelType, XmPIXMAP); n++;
      XtSetArg(args[n], XmNlabelPixmap, pan_r); n++;
      XtSetArg(args[n], XmNarmColor, ss->sgx->yellow); n++;
      w_mix_pan = XtCreateManagedWidget("mix-pan", xmPushButtonWidgetClass, track_row, args, n);
      XtAddCallback(w_mix_pan, XmNactivateCallback, mix_dialog_pan_callback, NULL);

      /* separator before sliders */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, mix_row); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNheight, 10); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      sep = XtCreateManagedWidget("mix-dialog-sep", xmSeparatorWidgetClass, mainform, args, n);

      /* SPEED */
      n = 0;
      s1 = XmStringCreateLocalized(_("speed:"));
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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
      w_speed_label = make_pushbutton_widget("mix-speed-label", mainform, args, n);
      XtAddCallback(w_speed_label, XmNactivateCallback, speed_click_callback, NULL);
      XmStringFree(s1);

      n = 0;
      s1 = initial_speed_label(speed_control_style(ss));
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_speed_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, w_speed_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      w_speed_number = make_pushbutton_widget("mix-speed-number", mainform, args, n);
      XtAddCallback(w_speed_number, XmNactivateCallback, speed_label_click_callback, NULL);
      XmStringFree(s1);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->position_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_speed_number); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, w_speed_number); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNvalue, speed_to_scrollbar(speed_control_min(ss), 1.0, speed_control_max(ss))); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNdragCallback, n1 = make_callback_list(speed_drag_callback, NULL)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n2 = make_callback_list(speed_valuechanged_callback, NULL)); n++;
      w_speed = XtCreateManagedWidget("mix-speed", xmScrollBarWidgetClass, mainform, args, n);
  
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
	  s1 = XmStringCreateLocalized(amplab);
	  XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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
	  w_amp_labels[i] = make_pushbutton_widget("mix-amp-label", mainform, args, n);
	  XtAddCallback(w_amp_labels[i], XmNactivateCallback, amp_click_callback, NULL);
	  XmStringFree(s1);

	  n = 0;
	  s1 = XmStringCreateLocalized("1.00");
	  XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
	  XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNtopWidget, last_number); n++;
	  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNleftWidget, w_amp_labels[i]); n++;
	  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
	  XtSetArg(args[n], XmNlabelString, s1); n++;
	  XtSetArg(args[n], XmNrecomputeSize, false); n++;
	  w_amp_numbers[i] = XtCreateManagedWidget("mix-amp-number", xmLabelWidgetClass, mainform, args, n);
	  XmStringFree(s1);

	  n = 0;      
	  XtSetArg(args[n], XmNbackground, ss->sgx->position_color); n++;
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
	  w_amps[i] = XtCreateManagedWidget("mix-amp", xmScrollBarWidgetClass, mainform, args, n);
	  FREE(n1);
	  FREE(n2);
	  last_label = w_amp_labels[i];
	  last_number = w_amp_numbers[i];
	}

      /* separator before envelopes */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, last_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNheight, 8); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      w_sep1 = XtCreateManagedWidget("mix-dialog-sep1", xmSeparatorWidgetClass, mainform, args, n);

      /* button box for dB clip wave */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_sep1); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 4); n++;
      w_dB_frame = XtCreateManagedWidget("mix-dB-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      w_dB_row = XtCreateManagedWidget("mix-dB-row", xmRowColumnWidgetClass, w_dB_frame, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNselectColor, ss->sgx->pushed_button_color); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}

      w_clip = make_togglebutton_widget(_("clip"), w_dB_row, args, n);
      XtAddCallback(w_clip, XmNvalueChangedCallback, mix_clip_callback, NULL);
      XmToggleButtonSetState(w_clip, true, false);

      w_wave = make_togglebutton_widget(_("wave"), w_dB_row, args, n);
      XtAddCallback(w_wave, XmNvalueChangedCallback, mix_wave_callback, NULL);

      w_dB = make_togglebutton_widget(_("dB"), w_dB_row, args, n);
      XtAddCallback(w_dB, XmNvalueChangedCallback, mix_dB_callback, NULL);


      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 2); n++;
      error_frame = XtCreateManagedWidget("error-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      error_label = XtCreateManagedWidget("", xmLabelWidgetClass, error_frame, args, n);

      
      /* amp env */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_sep1); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 4); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, w_dB_frame); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 4); n++;
      w_env_frame = XtCreateManagedWidget("mix-amp-env-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      w_env = XtCreateManagedWidget("mix-amp-env-window", xmDrawingAreaWidgetClass, w_env_frame, args, n);

      XtManageChild(mix_dialog);

      XtAddCallback(w_env, XmNresizeCallback, mix_amp_env_resize, NULL);
      XtAddCallback(w_env, XmNexposeCallback, mix_amp_env_resize, NULL);

      for (i = 0; i < CHANS_ALLOCATED; i++) spfs[i] = new_env_editor();

      XtAddEventHandler(w_env, ButtonPressMask, false, mix_drawer_button_press, NULL);
      XtAddEventHandler(w_env, ButtonMotionMask, false, mix_drawer_button_motion, NULL);
      XtAddEventHandler(w_env, ButtonReleaseMask, false, mix_drawer_button_release, NULL);

      set_dialog_widget(MIX_DIALOG, mix_dialog);

      XtUnmanageChild(error_frame);
    }
  else 
    {
      if (!(XtIsManaged(mix_dialog))) XtManageChild(mix_dialog);
      raise_dialog(mix_dialog);
      if (mix_dialog_id != INVALID_MIX_ID) reflect_edit_in_mix_dialog_envs(mix_dialog_id);
    }
  update_mix_dialog(mix_dialog_id);
  return(mix_dialog);
}

static bool track_dialog_slider_dragging = false;

static void update_mix_dialog(int mix_id) 
{
  if (!(mix_ok(mix_dialog_id)))
    {
      mix_dialog_id = mix_id; /* close-sound kills current mix, for example */
      if (!(mix_ok(mix_dialog_id))) 
	{
	  mix_dialog_id = any_mix_id(); 
	  mix_id = mix_dialog_id;
	}
    }
  if ((mix_id == mix_dialog_id) || (mix_id == ANY_MIX_ID))
    {
      Float val;
      int chans;
      if (mix_dialog == NULL) 
	make_mix_dialog();
      else
	{
	  set_sensitive(nextb, (next_mix_id(mix_dialog_id) != INVALID_MIX_ID));
	  set_sensitive(previousb, (previous_mix_id(mix_dialog_id) != INVALID_MIX_ID));
	}
      /* now reflect current mix state in mix dialog controls */
      if (mix_ok(mix_dialog_id))
	{
	  chan_info *cp = NULL;
	  off_t beg, len;
	  char lab[LABEL_BUFFER_SIZE];

	  cp = mix_dialog_mix_channel(mix_dialog_id);
	  val = mix_dialog_mix_speed(mix_dialog_id);
	  XtVaSetValues(w_speed, XmNvalue, speed_to_scrollbar(speed_control_min(ss), val, speed_control_max(ss)), NULL);
	  speed_changed(val, lab, mix_speed_style(mix_dialog_id), speed_control_tones(ss), 6);
	  set_label(w_speed_number, lab);

	  widget_track_to_text(w_track, mix_dialog_mix_track(mix_dialog_id));
	  widget_mix_to_text(w_id, mix_dialog_id);

	  beg = mix_dialog_mix_position(mix_dialog_id);
	  len = mix_frames(mix_dialog_id);
	  mus_snprintf(lab, LABEL_BUFFER_SIZE, "%.3f : %.3f%s",
		       (float)((double)beg / (float)SND_SRATE(cp->sound)),
		       (float)((double)(beg + len) / (float)SND_SRATE(cp->sound)),
		       (mix_ok_and_unlocked(mix_dialog_id)) ? "" : " (locked)");
	  XmTextSetString(w_beg, lab);

	  chans = mix_dialog_mix_input_chans(mix_dialog_id);
	  if (chans == 0) return;
	  if (chans > 8) chans = 8; 
	  set_sensitive(XmMessageBoxGetChild(mix_dialog, XmDIALOG_CANCEL_BUTTON), true);

	  if (mix_dialog_mix_inverted(mix_dialog_id))
	    XmChangeColor(w_mix_pan, ss->sgx->yellow);
	  else XmChangeColor(w_mix_pan, ss->sgx->highlight_color);
	}
      else
	{
	  chans = 1;
	  XmTextSetString(w_track, "0");
	  XmTextSetString(w_id, "-1");
	  XmTextSetString(w_beg, _("no active mixes"));
	  set_sensitive(XmMessageBoxGetChild(mix_dialog, XmDIALOG_CANCEL_BUTTON), false);
	  XmChangeColor(w_mix_pan, ss->sgx->highlight_color);
	}
      if ((!mix_dialog_slider_dragging) && (!track_dialog_slider_dragging))
	{
	  int i;
	  for (i = 0; i < chans; i++)
	    {
	      XmString s1;
	      char amplab[LABEL_BUFFER_SIZE];
	      if ((i == 0) && (chans == 1))
		mus_snprintf(amplab, LABEL_BUFFER_SIZE, _("amp:"));
	      else mus_snprintf(amplab, LABEL_BUFFER_SIZE, _("amp %d:"), i);
	      s1 = XmStringCreateLocalized(amplab);
	      XtVaSetValues(w_amp_labels[i], XmNlabelString, s1, NULL);
	      XmStringFree(s1);
	      if (mix_ok_and_unlocked(mix_dialog_id))
		val = mix_dialog_mix_amp(mix_dialog_id, i);
	      else val = 1.0;
	      XtVaSetValues(w_amps[i], XmNvalue, amp_to_scrollbar(w_amp_numbers[i], val), NULL);
	      current_amps[i] = val;
	      if (!(XtIsManaged(w_amp_labels[i]))) XtManageChild(w_amp_labels[i]);
	      if (!(XtIsManaged(w_amp_numbers[i]))) XtManageChild(w_amp_numbers[i]);
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
	}
      mix_amp_env_resize(w_env, NULL, NULL);
    }
}



/* -------------------------------- track dialog -------------------------------- */

static void update_track_dialog(int track_id);

static Widget track_dialog = NULL;
static int track_dialog_id = INVALID_TRACK_ID;


/* -------- speed -------- */
static Widget w_track_speed_number, w_track_speed_label, w_track_speed;

static void track_speed_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  char sfs[6];
  if (!(track_p(track_dialog_id))) return;
  track_dialog_set_speed(track_dialog_id,
			 speed_changed(1.0,
				       sfs,
				       track_speed_style(track_dialog_id),
				       speed_control_tones(ss),
				       6));
  set_label(w_track_speed_number, sfs);
  XtVaSetValues(w_track_speed, XmNvalue, speed_to_scrollbar(speed_control_min(ss), 1.0, speed_control_max(ss)), NULL);
}

static void track_speed_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (!(track_p(track_dialog_id))) return;
  set_speed_label(w_track_speed_number, ((XmScrollBarCallbackStruct *)info)->value);
}

static void track_speed_label_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  char speed_number_buffer[6];
  ASSERT_WIDGET_TYPE(XmIsPushButton(w), w);
  switch (track_speed_style(track_dialog_id))
    {
    default:
    case SPEED_CONTROL_AS_FLOAT:    set_track_speed_style(track_dialog_id, SPEED_CONTROL_AS_RATIO, true);    break;
    case SPEED_CONTROL_AS_RATIO:    set_track_speed_style(track_dialog_id, SPEED_CONTROL_AS_SEMITONE, true); break;
    case SPEED_CONTROL_AS_SEMITONE: set_track_speed_style(track_dialog_id, SPEED_CONTROL_AS_FLOAT, true);    break;
    }
  speed_changed(track_dialog_track_speed(track_dialog_id),
		speed_number_buffer,
		track_speed_style(track_dialog_id),
		speed_control_tones(ss),
		6);
  set_label(w_track_speed_number, speed_number_buffer);
}

static void track_speed_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (!(track_p(track_dialog_id))) return;
  track_dialog_slider_dragging = false;
  track_dialog_set_speed(track_dialog_id, set_speed_label(w_track_speed_number, cb->value));
}


/* -------- tempo -------- */
static Widget w_track_tempo_number, w_track_tempo_label, w_track_tempo;

static int tempo_to_scrollbar(Float minval, Float val, Float maxval)
{
  if (val <= minval) return(0);
  if (val >= maxval) return((int)(0.9 * SCROLLBAR_MAX));
  if (val >= 1.0)
    return(snd_round(0.9 * 0.5 * SCROLLBAR_MAX * (1.0 + (val - 1.0) / (maxval - 1.0))));
  return(snd_round(0.9 * 0.5 * SCROLLBAR_MAX * ((val - minval) / (1.0 - minval))));
}

static Float scrollbar_to_tempo(int val)
{
  char tempo_number_buffer[5];
  Float tempo;
  if (val <= 0) 
    tempo = tempo_control_min(ss);
  else
    {
      if (val >= (0.9 * SCROLLBAR_MAX)) 
	tempo = tempo_control_max(ss);
      else
	{
	  if (val > (0.5 * 0.9 * SCROLLBAR_MAX))
	    tempo = (((val / (0.5 * 0.9 * SCROLLBAR_MAX)) - 1.0) * (tempo_control_max(ss) - 1.0)) + 1.0;
	  else tempo = (val * (1.0 - tempo_control_min(ss)) / (0.5 * 0.9 * SCROLLBAR_MAX)) + tempo_control_min(ss);
	}
    }
  mus_snprintf(tempo_number_buffer, 5, "%.2f", tempo);
  set_label(w_track_tempo_number, tempo_number_buffer);
  return(tempo);
}

static void track_tempo_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  if (!(track_p(track_dialog_id))) return;
  track_dialog_set_tempo(track_dialog_id, 1.0, false);
  set_label(w_track_tempo_number, "1.00");
  XtVaSetValues(w_track_tempo, XmNvalue, tempo_to_scrollbar(tempo_control_min(ss), 1.0, tempo_control_max(ss)), NULL);
}

static void track_tempo_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  int ival;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (!(track_p(track_dialog_id))) return;
  ival = ((XmScrollBarCallbackStruct *)info)->value;
  if (!track_dialog_slider_dragging) 
    {
      track_dialog_slider_dragging = true;
      track_dialog_start_slider_drag(track_dialog_id);
    }
  track_dialog_set_tempo(track_dialog_id, scrollbar_to_tempo(ival), true);
}

static void track_tempo_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  int ival;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (!(track_p(track_dialog_id))) return;
  ival = ((XmScrollBarCallbackStruct *)info)->value;
  track_dialog_slider_dragging = false;
  track_dialog_set_tempo(track_dialog_id, scrollbar_to_tempo(ival), false);  
}



/* -------- amp -------- */
static Widget w_track_amp_number, w_track_amp_label, w_track_amp;

static void change_track_amp_label(int track_id, Float val)
{
  char sfs[6];
  mus_snprintf(sfs, 6, "%.2f", val);
  set_label(w_track_amp_number, sfs);
}

static void track_amp_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  if (!(track_p(track_dialog_id))) return;
  change_track_amp_label(track_dialog_id, 1.0);
  track_dialog_set_amp(track_dialog_id, 1.0);
  XtVaSetValues(w_track_amp, XmNvalue, amp_to_scroll(amp_control_min(ss), 1.0, amp_control_max(ss)), NULL);
}

static void track_amp_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (!(track_p(track_dialog_id))) return;
  change_track_amp_label(track_dialog_id, scrollbar_to_amp(((XmScrollBarCallbackStruct *)info)->value));
}

static void track_amp_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  int ival;
  Float val;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  ival = ((XmScrollBarCallbackStruct *)info)->value;
  if (!(track_p(track_dialog_id))) return;
  track_dialog_slider_dragging = false;
  val = scrollbar_to_amp(ival);
  change_track_amp_label(track_dialog_id, val);
  track_dialog_set_amp(track_dialog_id, val);
}

static int track_amp_to_scroll(Float amp)
{
  return(amp_to_scrollbar(w_track_amp_number, amp));
}


/* -------- amp-env -------- */
static Widget w_track_env_frame, w_track_env;
static axis_context *track_ax = NULL;
static GC track_cur_gc;
static env_editor *track_spf;
static bool with_track_background_wave = false;

void show_track_background_wave(int pts, bool two_sided)
{
  XSetForeground(MAIN_DISPLAY(ss), track_ax->gc, ss->sgx->enved_waveform_color);
  if (two_sided)
    draw_both_grf_points(1, track_ax, pts, GRAPH_LINES);
  else draw_grf_points(1, track_ax, pts, track_spf->axis, ungrf_y(track_spf->axis, 0.0), GRAPH_LINES);
  XSetForeground(MAIN_DISPLAY(ss), track_ax->gc, ss->sgx->black);  
}

static void track_amp_env_resize(Widget w, XtPointer context, XtPointer info) 
{
  env *e;
  env *cur_env;
  if (!(track_p(track_dialog_id))) return;
  if (track_ax == NULL)
    {
      XGCValues gv;
      gv.function = GXcopy;
      XtVaGetValues(w_track_env, XmNbackground, &gv.background, XmNforeground, &gv.foreground, NULL);
      track_cur_gc = XtGetGC(w_track_env, GCForeground | GCFunction, &gv);
      track_ax = (axis_context *)CALLOC(1, sizeof(axis_context));
      track_ax->wn = XtWindow(w_track_env);
      track_ax->dp = XtDisplay(w_track_env);
      track_ax->gc = track_cur_gc;
    }
  else clear_window(track_ax);
  e = track_dialog_env(track_dialog_id);
  track_spf->with_dots = true;
  env_editor_display_env(track_spf, e, track_ax, _("track env"), 0, 0, widget_width(w), widget_height(w), NOT_PRINTING);
  cur_env = track_dialog_track_amp_env(track_dialog_id);
  if (cur_env)
    {
      XSetForeground(MAIN_DISPLAY(ss), track_ax->gc, ss->sgx->enved_waveform_color);
      track_spf->with_dots = false;
      env_editor_display_env(track_spf, cur_env, track_ax, _("track env"), 0, 0, widget_width(w), widget_height(w), NOT_PRINTING);
      XSetForeground(MAIN_DISPLAY(ss), track_ax->gc, ss->sgx->black);
    }
  if (with_track_background_wave)
    display_track_waveform(track_dialog_id, track_spf->axis);
}

#ifdef MUS_MAC_OSX
static int track_press_x, track_press_y;
#endif

static void track_drawer_button_motion(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  XMotionEvent *ev = (XMotionEvent *)event;
  env *e;
  if (!(track_p(track_dialog_id))) return;
#ifdef MUS_MAC_OSX
  if ((track_press_x == ev->x) && (track_press_y == ev->y)) return;
#endif
  e = track_dialog_env(track_dialog_id);
  env_editor_button_motion(track_spf, ev->x, ev->y, ev->time, e);
  track_amp_env_resize(w, NULL, NULL);
}

static void track_drawer_button_press(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  XButtonEvent *ev = (XButtonEvent *)event;
  env *e;
  if (!(track_p(track_dialog_id))) return;
#ifdef MUS_MAC_OSX
  track_press_x = ev->x;
  track_press_y = ev->y;
#endif
  e = track_dialog_env(track_dialog_id);
  if (env_editor_button_press(track_spf, ev->x, ev->y, ev->time, e))
    track_amp_env_resize(w, NULL, NULL);
}

static void track_drawer_button_release(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  env *e;
  if (!(track_p(track_dialog_id))) return;
  e = track_dialog_env(track_dialog_id);
  env_editor_button_release(track_spf, e);
  track_amp_env_resize(w, NULL, NULL);
}



static Widget w_track_id = NULL, w_track_beg = NULL, w_track_track = NULL, w_track_track_play;
static Widget w_track_text; /* error msgs, mix lists etc */

static bool track_track_changed = false;

static Widget track_error_frame = NULL, track_error_label = NULL;

static void clear_track_error(void)
{
  if ((track_error_frame) && (XtIsManaged(track_error_frame)))
    XtUnmanageChild(track_error_frame);
}

static void unpost_track_error(XtPointer data, XtIntervalId *id)
{
  clear_track_error();
}

static void errors_to_track_text(const char *msg, void *data)
{
  int lines = 0;
  XmString label;
  label = multi_line_label(msg, &lines);
  XtVaSetValues(track_error_label, 
		XmNlabelString, label, 
		XmNheight, lines * 20,
		NULL);
  XtVaSetValues(track_error_frame, XmNheight, lines * 20, NULL);
  XmStringFree(label);
  XtManageChild(track_error_frame);
  /* since the offending text is automatically overwritten, we can't depend on subsequent text modify callbacks
   *   to clear things, so we'll just use a timer
   */
  XtAppAddTimeOut(MAIN_APP(ss),
		  5000,
		  (XtTimerCallbackProc)unpost_track_error,
		  NULL);
}


static void track_track_activated(void)
{
  char *val;
  track_track_changed = false;
  if (!(track_p(track_dialog_id))) return;
  val = XmTextGetString(w_track_track);
  if (val)
    {
      int id;
      id = track_name_to_id(val);
      if (id < 0)
	{
	  redirect_errors_to(errors_to_track_text, NULL);
	  id = string_to_int(val, 0, "track");
	  redirect_errors_to(NULL, NULL);
	}
      if (id >= 0)
	{
	  if ((id == track_dialog_id) ||
	      (!(set_track_track(track_dialog_id, id))))
	    {
	      errors_to_track_text(_("circular track chain"), NULL);
	      widget_track_to_text(w_track_track, track_dialog_track_track(track_dialog_id));
	    }
	  else update_track_dialog(id);
	}
      else widget_track_to_text(w_track_track, track_dialog_track_track(track_dialog_id));
      XtFree(val);
    }
}

static void track_track_modify_callback(Widget w, XtPointer context, XtPointer info) 
{
  track_track_changed = true;
}

static void track_track_check_callback(Widget w, XtPointer context, XtPointer info)
{
  /* if user changes id, but forgets <cr>, then moves mouse away, we update as if <cr> */
  if (track_track_changed) track_track_activated();
}

static bool track_id_changed = false;

static void track_id_activated(void)
{
  char *val;
  track_id_changed = false;
  val = XmTextGetString(w_track_id);
  if (val)
    {
      int id;
      id = track_name_to_id(val);
      if (id < 0)
	{
	  redirect_errors_to(errors_to_track_text, NULL);
	  id = string_to_int(val, 0, "track");
	  redirect_errors_to(NULL, NULL);
	}
      if (id >= 0)
	{
	  if (track_p(id))
	    {
	      track_dialog_id = id;
	      update_track_dialog(id);
	    }
	  else errors_to_track_text(_("no such track"), NULL);
	}
      XtFree(val);
    }
}

static void track_id_modify_callback(Widget w, XtPointer context, XtPointer info) 
{
  track_id_changed = true;
}

static void track_id_check_callback(Widget w, XtPointer context, XtPointer info)
{
  if (track_id_changed) track_id_activated();
}


static void redisplay_track_bounds(void)
{
  chan_info *cp;
  cp = track_channel(track_dialog_id, 0); /* can be NULL */
  if (cp)
    {
      char lab[LABEL_BUFFER_SIZE];
      off_t beg, len;
      beg = track_position(track_dialog_id, -1);
      len = track_frames(track_dialog_id, -1);
      mus_snprintf(lab, LABEL_BUFFER_SIZE, "%.3f : %.3f",
		   (float)((double)beg / (float)SND_SRATE(cp->sound)),
		   (float)((double)(beg + len) / (float)SND_SRATE(cp->sound)));
      XmTextSetString(w_track_beg, lab);
    }
  else
    {
      XmTextSetString(w_track_beg, _("no mixes in track"));
    }
}

static void track_beg_activated(void)
{
  char *val;
  if (!(track_p(track_dialog_id))) return;
  val = XmTextGetString(w_track_beg);
  if (val)
    {
      chan_info *cp;
      cp = track_channel(track_dialog_id, 0);
      if (cp)
	{
	  Float beg;
	  char *up_to_colon;
	  up_to_colon = string_to_colon(val);
	  redirect_errors_to(errors_to_track_text, NULL);
	  beg = string_to_Float(up_to_colon, 0.0, "begin time");
	  redirect_errors_to(NULL, NULL);
	  FREE(up_to_colon);
	  if (beg >= 0.0)
	    {
	      set_track_position(track_dialog_id, (off_t)(beg * SND_SRATE(cp->sound)));
	      update_track_dialog(track_dialog_id);
	    }
	  else redisplay_track_bounds(); 
	}
      else 
	{
	  errors_to_track_text(_("no mixes in track, so begin time ignored"), NULL);
	  redisplay_track_bounds();
	}
      XtFree(val);
    }
}

/* graph buttons */
static void track_dB_callback(Widget w, XtPointer context, XtPointer info)
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info; 
  if (track_spf) track_spf->in_dB = cb->set;
  track_amp_env_resize(w_track_env, NULL, NULL);
}

static void track_clip_callback(Widget w, XtPointer context, XtPointer info)
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info; 
  if (track_spf) track_spf->clip_p = cb->set;
  track_amp_env_resize(w_track_env, NULL, NULL);
}

static void track_wave_callback(Widget w, XtPointer context, XtPointer info)
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info; 
  with_track_background_wave = cb->set;
  track_amp_env_resize(w_track_env, NULL, NULL);
}


static void apply_track_dialog_callback(Widget w, XtPointer context, XtPointer info) 
{
  env *e;
  if (!(track_p(track_dialog_id))) return;
  e = track_dialog_env(track_dialog_id);
  track_dialog_set_amp_env(track_dialog_id, e);
  track_amp_env_resize(w_track_env, NULL, NULL);
}

static void dismiss_track_dialog_callback(Widget w, XtPointer context, XtPointer info) 
{
  Widget active_widget;
  clear_track_error();
  active_widget = XmGetFocusWidget(track_dialog);
  if (active_widget == XmMessageBoxGetChild(track_dialog, XmDIALOG_OK_BUTTON))
    XtUnmanageChild(track_dialog);
  else
    {
      if (active_widget == w_track_track)
	track_track_activated();
      else
	{
	  if (active_widget == w_track_id)
	    track_id_activated();
	  else
	    {
	      if (active_widget == w_track_beg)
		track_beg_activated();
	    }
	}
    }
}

static void help_track_dialog_callback(Widget w, XtPointer context, XtPointer info) 
{
  track_dialog_help();
}


/* -------- play -------- */
static bool track_playing = false;

bool track_play_stopped(void) {return(!track_playing);}

void reflect_track_play_stop(void)
{
  if (w_track_track_play)
    XmChangeColor(w_track_track_play, ss->sgx->basic_color);
  track_playing = false;
}

static void track_dialog_play_callback(Widget w, XtPointer context, XtPointer info) 
{
  if (track_playing)
    reflect_track_play_stop();
  else
    {
      if (!(track_p(track_dialog_id))) return;
      track_playing = true;
      if (w_track_track_play) XmChangeColor(w_track_track_play, ss->sgx->pushed_button_color);
      track_dialog_play(track_dialog_id);
    }
}

#define p_track_speaker_width 12
#define p_track_speaker_height 12
static unsigned char p_track_speaker_bits[] = {
   0x00, 0x07, 0xc0, 0x04, 0x30, 0x04, 0x0e, 0x04, 0x06, 0x04, 0x06, 0x04,
   0x06, 0x04, 0x06, 0x04, 0x0e, 0x04, 0x30, 0x04, 0xc0, 0x04, 0x00, 0x07};

static int track_depth;
static GC track_gc;
static Pixmap track_speaker_r;
void make_track_icons_transparent_again(Pixel old_color, Pixel new_color);
void make_track_icons_transparent_again(Pixel old_color, Pixel new_color)
{
  if (track_dialog)
    {
      XFreePixmap(XtDisplay(track_dialog), track_speaker_r);
      XSetBackground(XtDisplay(track_dialog), track_gc, new_color);
      track_speaker_r = make_pixmap(p_track_speaker_bits, p_track_speaker_width, p_track_speaker_height, track_depth, track_gc);
      XtVaSetValues(w_track_track_play, XmNlabelPixmap, track_speaker_r, NULL);
    }
}

static Widget w_track_sep1;
static Widget track_nextb, track_previousb;

static void track_next_callback(Widget w, XtPointer context, XtPointer info)
{
  int id;
  clear_track_error();
  id = next_track_id(track_dialog_id);
  if (id != INVALID_TRACK_ID)
    {
      track_dialog_id = id;
      update_track_dialog(id);
      if (next_track_id(id) == INVALID_TRACK_ID) 
	set_sensitive(track_nextb, false);
    }
}

static void track_previous_callback(Widget w, XtPointer context, XtPointer info)
{
  int id;
  clear_track_error();
  id = previous_track_id(track_dialog_id);
  if (id != INVALID_TRACK_ID)
    {
      track_dialog_id = id;
      update_track_dialog(id);
      if (previous_track_id(id) == INVALID_TRACK_ID) 
	set_sensitive(track_previousb, false);
    }
}

static Widget w_track_frame = NULL;

Widget make_track_dialog(void) 
{
  if (track_dialog == NULL)
    {
      Widget mainform, track_row, track_track_row, track_track_frame, sep, sep2;
      Widget w_dB_frame, w_dB, w_clip, w_wave, w_dB_row;
      XmString xdismiss, xhelp, xtitle, s1, xapply;
      int n;
      Arg args[20];
      XtCallbackList n1, n2;
      XGCValues v;

      track_dialog_id = any_track_id();
      xdismiss = XmStringCreateLocalized(_("Dismiss"));
      xapply = XmStringCreateLocalized(_("Apply Env"));
      xhelp = XmStringCreateLocalized(_("Help"));
      xtitle = XmStringCreateLocalized(_("Tracks"));

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNokLabelString, xdismiss); n++;
      XtSetArg(args[n], XmNcancelLabelString, xapply); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNdialogTitle, xtitle); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNtransient, false); n++;
      XtSetArg(args[n], XmNwidth, DIALOG_WIDTH); n++;
      XtSetArg(args[n], XmNheight, DIALOG_HEIGHT); n++;
      track_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), _("Tracks"), args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->doit_again_button_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;

      track_previousb = XtCreateManagedWidget(_("Previous"), xmPushButtonGadgetClass, track_dialog, args, n);
      if (previous_track_id(track_dialog_id) == INVALID_TRACK_ID) 
	set_sensitive(track_previousb, false);
      XtAddCallback(track_previousb, XmNactivateCallback, track_previous_callback, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->reset_button_color); n++;
      XtSetArg(args[n], XmNforeground, ss->sgx->black); n++;
      XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;

      track_nextb = XtCreateManagedWidget(_("Next"), xmPushButtonGadgetClass, track_dialog, args, n);
      XtAddCallback(track_nextb, XmNactivateCallback, track_next_callback, NULL);
      if (next_track_id(track_dialog_id) == INVALID_TRACK_ID) 
	set_sensitive(track_nextb, false);

      XmStringFree(xhelp);
      XmStringFree(xapply);
      XmStringFree(xdismiss);
      XmStringFree(xtitle);

      XtAddCallback(track_dialog, XmNokCallback, dismiss_track_dialog_callback, NULL);
      XtAddCallback(track_dialog, XmNcancelCallback, apply_track_dialog_callback, NULL);
      XtAddCallback(track_dialog, XmNhelpCallback, help_track_dialog_callback, NULL);

      XtVaSetValues(XmMessageBoxGetChild(track_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(track_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(track_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(track_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->doit_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(track_dialog, XmDIALOG_OK_BUTTON), XmNbackground, ss->sgx->quit_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(track_dialog, XmDIALOG_HELP_BUTTON), XmNbackground, ss->sgx->help_button_color, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, XmMessageBoxGetChild(track_dialog, XmDIALOG_SEPARATOR)); n++;
      mainform = XtCreateManagedWidget("formd", xmFormWidgetClass, track_dialog, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 2); n++;
      w_track_frame = XtCreateManagedWidget("track-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      track_row = XtCreateManagedWidget("track-dialog-row", xmRowColumnWidgetClass, w_track_frame, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtCreateManagedWidget(_("track:"), xmLabelWidgetClass, track_row, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNresizeWidth, false); n++;
      XtSetArg(args[n], XmNcolumns, NAME_COLUMNS); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      w_track_id = make_textfield_widget("track-id", track_row, args, n, ACTIVATABLE, NO_COMPLETER);
      XtAddCallback(w_track_id, XmNlosingFocusCallback, track_id_check_callback, NULL);
      XtAddCallback(w_track_id, XmNmodifyVerifyCallback, track_id_modify_callback, NULL);
      XmTextSetString(w_track_id, "0");

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      w_track_beg = make_textfield_widget("track-times", track_row, args, n, ACTIVATABLE, NO_COMPLETER);
      XmTextSetString(w_track_beg, "0.000 : 1.000");

      XtVaGetValues(track_row, XmNforeground, &v.foreground, XmNbackground, &v.background, XmNdepth, &track_depth, NULL);
      track_gc = XtGetGC(track_row, GCForeground | GCBackground, &v);
      track_speaker_r = make_pixmap(p_track_speaker_bits, p_track_speaker_width, p_track_speaker_height, track_depth, track_gc);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNlabelType, XmPIXMAP); n++;
      XtSetArg(args[n], XmNlabelPixmap, track_speaker_r); n++;
      XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;
      w_track_track_play = XtCreateManagedWidget("track-play", xmPushButtonWidgetClass, track_row, args, n);
      XtAddCallback(w_track_track_play, XmNactivateCallback, track_dialog_play_callback, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 2); n++;
      track_track_frame = XtCreateManagedWidget("track-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      track_track_row = XtCreateManagedWidget("track-dialog-row", xmRowColumnWidgetClass, track_track_frame, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtCreateManagedWidget(_("track:"), xmLabelWidgetClass, track_track_row, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNresizeWidth, false); n++;
      XtSetArg(args[n], XmNcolumns, NAME_COLUMNS); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      w_track_track = make_textfield_widget("track-track", track_track_row, args, n, ACTIVATABLE, NO_COMPLETER);
      XtAddCallback(w_track_track, XmNlosingFocusCallback, track_track_check_callback, NULL);
      XtAddCallback(w_track_track, XmNmodifyVerifyCallback, track_track_modify_callback, NULL);
      XmTextSetString(w_track_track, "0");


      /* separator before sliders */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, track_track_row); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNheight, 6); n++; /* 10 */
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      sep2 = XtCreateManagedWidget("track-dialog-sep", xmSeparatorWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep2); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      w_track_text = XtCreateManagedWidget("track-text", xmLabelWidgetClass, mainform, args, n);
      set_label(w_track_text, "");
	
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_track_text); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNheight, 6); n++; /* 10 */
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      sep = XtCreateManagedWidget("track-dialog-sep2", xmSeparatorWidgetClass, mainform, args, n);


      /* SPEED */
      n = 0;
      s1 = XmStringCreateLocalized(_("speed:"));
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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
      w_track_speed_label = make_pushbutton_widget("track-speed-label", mainform, args, n);
      XtAddCallback(w_track_speed_label, XmNactivateCallback, track_speed_click_callback, NULL);
      XmStringFree(s1);

      n = 0;
      s1 = initial_speed_label(speed_control_style(ss));
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_track_speed_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, w_track_speed_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      w_track_speed_number = make_pushbutton_widget("track-speed-number", mainform, args, n);
      XtAddCallback(w_track_speed_number, XmNactivateCallback, track_speed_label_click_callback, NULL);
      XmStringFree(s1);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->position_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_track_speed_number); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, w_track_speed_number); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNvalue, speed_to_scrollbar(speed_control_min(ss), 1.0, speed_control_max(ss))); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNdragCallback, n1 = make_callback_list(track_speed_drag_callback, NULL)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n2 = make_callback_list(track_speed_valuechanged_callback, NULL)); n++;
      w_track_speed = XtCreateManagedWidget("track-speed", xmScrollBarWidgetClass, mainform, args, n);
  
      FREE(n1);
      FREE(n2);


      /* TEMPO */
      n = 0;
      s1 = XmStringCreateLocalized(_("tempo:"));
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_track_speed_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      w_track_tempo_label = make_pushbutton_widget("track-tempo-label", mainform, args, n);
      XtAddCallback(w_track_tempo_label, XmNactivateCallback, track_tempo_click_callback, NULL);
      XmStringFree(s1);
      
      n = 0;
      s1 = XmStringCreateLocalized("1.00");
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_track_speed_number); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, w_track_tempo_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      w_track_tempo_number = XtCreateManagedWidget("track-tempo-number", xmLabelWidgetClass, mainform, args, n);
      XmStringFree(s1);
      
      n = 0;      
      XtSetArg(args[n], XmNbackground, ss->sgx->position_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_track_tempo_number); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, w_track_tempo_number); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNvalue, tempo_to_scrollbar(tempo_control_min(ss), 1.0, tempo_control_max(ss))); n++;
      XtSetArg(args[n], XmNdragCallback, n1 = make_callback_list(track_tempo_drag_callback, NULL)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n2 = make_callback_list(track_tempo_valuechanged_callback, NULL)); n++;
      w_track_tempo = XtCreateManagedWidget("track-tempo", xmScrollBarWidgetClass, mainform, args, n);
      FREE(n1);
      FREE(n2);
      

      /* AMP */
      n = 0;
      s1 = XmStringCreateLocalized(_("amp:"));
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_track_tempo_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      w_track_amp_label = make_pushbutton_widget("track-amp-label", mainform, args, n);
      XtAddCallback(w_track_amp_label, XmNactivateCallback, track_amp_click_callback, NULL);
      XmStringFree(s1);
      
      n = 0;
      s1 = XmStringCreateLocalized("1.00");
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_track_tempo_number); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, w_track_amp_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      w_track_amp_number = XtCreateManagedWidget("track-amp-number", xmLabelWidgetClass, mainform, args, n);
      XmStringFree(s1);
      
      n = 0;      
      XtSetArg(args[n], XmNbackground, ss->sgx->position_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_track_amp_number); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, w_track_amp_number); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNvalue, amp_to_scroll(amp_control_min(ss), 1.0, amp_control_max(ss))); n++;
      XtSetArg(args[n], XmNdragCallback, n1 = make_callback_list(track_amp_drag_callback, NULL)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n2 = make_callback_list(track_amp_valuechanged_callback, NULL)); n++;
      w_track_amp = XtCreateManagedWidget("track-amp", xmScrollBarWidgetClass, mainform, args, n);
      FREE(n1);
      FREE(n2);

      
      /* separator before envelope */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_track_amp_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNheight, 8); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      w_track_sep1 = XtCreateManagedWidget("track-dialog-sep1", xmSeparatorWidgetClass, mainform, args, n);
      
      /* button box for dB clip wave */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_track_sep1); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 4); n++;
      w_dB_frame = XtCreateManagedWidget("track-dB-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      w_dB_row = XtCreateManagedWidget("track-dB-row", xmRowColumnWidgetClass, w_dB_frame, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNselectColor, ss->sgx->pushed_button_color); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}

      w_clip = make_togglebutton_widget(_("clip"), w_dB_row, args, n);
      XtAddCallback(w_clip, XmNvalueChangedCallback, track_clip_callback, NULL);
      XmToggleButtonSetState(w_clip, true, false);

      w_wave = make_togglebutton_widget(_("wave"), w_dB_row, args, n);
      XtAddCallback(w_wave, XmNvalueChangedCallback, track_wave_callback, NULL);

      w_dB = make_togglebutton_widget(_("dB"), w_dB_row, args, n);
      XtAddCallback(w_dB, XmNvalueChangedCallback, track_dB_callback, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 2); n++;
      track_error_frame = XtCreateManagedWidget("track-error-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      track_error_label = XtCreateManagedWidget("", xmLabelWidgetClass, track_error_frame, args, n);


      /* amp env */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_track_sep1); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 4); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, w_dB_frame); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 4); n++;
      w_track_env_frame = XtCreateManagedWidget("track-amp-env-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      w_track_env = XtCreateManagedWidget("track-amp-env-window", xmDrawingAreaWidgetClass, w_track_env_frame, args, n);

      XtManageChild(track_dialog);

      XtAddCallback(w_track_env, XmNresizeCallback, track_amp_env_resize, NULL);
      XtAddCallback(w_track_env, XmNexposeCallback, track_amp_env_resize, NULL);

      track_spf = new_env_editor();

      XtAddEventHandler(w_track_env, ButtonPressMask, false, track_drawer_button_press, NULL);
      XtAddEventHandler(w_track_env, ButtonMotionMask, false, track_drawer_button_motion, NULL);
      XtAddEventHandler(w_track_env, ButtonReleaseMask, false, track_drawer_button_release, NULL);

      set_dialog_widget(TRACK_DIALOG, track_dialog);

      XtUnmanageChild(track_error_frame);
    }
  else 
    {
      if (!(XtIsManaged(track_dialog))) XtManageChild(track_dialog);
      raise_dialog(track_dialog);
      reflect_edit_in_track_dialog_env(track_dialog_id);
    }
  update_track_dialog(track_dialog_id);
  return(track_dialog);
}

static void update_track_dialog(int track_id) 
{
  if (!(track_p(track_dialog_id))) track_dialog_id = track_id;
  if (!(track_p(track_dialog_id))) track_dialog_id = any_track_id();
  if (track_id == track_dialog_id)
    {
      if (track_dialog == NULL) 
	make_track_dialog();
      else
	{
	  set_sensitive(track_nextb, (next_track_id(track_dialog_id) != INVALID_TRACK_ID));
	  set_sensitive(track_previousb, (previous_track_id(track_dialog_id) != INVALID_TRACK_ID));
	}
      /* now reflect current track state in track dialog controls */
      if (track_p(track_dialog_id))
	{
	  chan_info *cp;
	  off_t beg, len;
	  Float val;
	  char lab[LABEL_BUFFER_SIZE];
	  char *temp = NULL;
	  val = track_dialog_track_speed(track_dialog_id);
	  XtVaSetValues(w_track_speed, XmNvalue, speed_to_scrollbar(speed_control_min(ss), val, speed_control_max(ss)), NULL);
	  speed_changed(val, lab, track_speed_style(track_dialog_id), speed_control_tones(ss), 6);
	  set_label(w_track_speed_number, lab);
	  widget_track_to_text(w_track_track, track_dialog_track_track(track_dialog_id));
	  widget_track_to_text(w_track_id, track_dialog_id);
	  val = track_dialog_track_tempo(track_dialog_id);
	  XtVaSetValues(w_track_tempo, XmNvalue, tempo_to_scrollbar(tempo_control_min(ss), val, tempo_control_max(ss)), NULL);
	  mus_snprintf(lab, 5, "%.2f", val);
	  set_label(w_track_tempo_number, lab);
	  val = track_dialog_track_amp(track_dialog_id);
	  XtVaSetValues(w_track_amp, XmNvalue, track_amp_to_scroll(val), NULL);
	  track_amp_env_resize(w_track_env, NULL, NULL);
	  cp = track_channel(track_dialog_id, 0); /* can be NULL */
	  if (cp)
	    {
	      beg = track_position(track_dialog_id, -1);
	      len = track_frames(track_dialog_id, -1);
	      mus_snprintf(lab, LABEL_BUFFER_SIZE, "%.3f : %.3f",
			   (float)((double)beg / (float)SND_SRATE(cp->sound)),
			   (float)((double)(beg + len) / (float)SND_SRATE(cp->sound)));
	      XmTextSetString(w_track_beg, lab);
	    }
	  else XmTextSetString(w_track_beg, _("no mixes in track"));
	  set_sensitive(XmMessageBoxGetChild(track_dialog, XmDIALOG_CANCEL_BUTTON), true);
	  set_label(w_track_text, temp = track_dialog_track_info(track_dialog_id));
	  if (temp) FREE(temp);
	}
      else
	{
	  XmTextSetString(w_track_track, "0");
	  XmTextSetString(w_track_id, "0");
	  XmTextSetString(w_track_beg, _("no active track"));
	  set_sensitive(XmMessageBoxGetChild(track_dialog, XmDIALOG_CANCEL_BUTTON), false);
	}
      if (track_dialog_track_color_set(track_dialog_id))
	{
	  XmChangeColor(w_track_frame, track_dialog_track_color(track_dialog_id));
	  XtVaSetValues(w_track_frame, 
			XmNshadowThickness, 4, 
			NULL);
	}
      else
	{
	  XmChangeColor(w_track_frame, ss->sgx->basic_color);
	  XtVaSetValues(w_track_frame, 
			XmNshadowThickness, 2, 
			NULL);
	}
    }
}



/* ---------------- reflection ---------------- */

void reflect_mix_or_track_change(int mix_id, int track_id, bool forced)
{
  if ((mix_dialog) && 
      (XtIsManaged(mix_dialog)))
    {
      if ((forced) && (mix_ok_and_unlocked(mix_id))) mix_dialog_id = mix_id;
      update_mix_dialog(mix_id);
      if (mix_id != INVALID_MIX_ID)
	{
	  set_sensitive(nextb, (next_mix_id(mix_dialog_id) != INVALID_MIX_ID));
	  set_sensitive(previousb, (previous_mix_id(mix_dialog_id) != INVALID_MIX_ID));
	}
    }
  if ((track_dialog) && 
      (XtIsManaged(track_dialog)))
    {
      if ((forced) && (track_p(track_id))) track_dialog_id = track_id;
      update_track_dialog(track_id);
      if (track_id != INVALID_TRACK_ID)
	{
	  set_sensitive(track_nextb, (next_track_id(track_dialog_id) != INVALID_TRACK_ID));
	  set_sensitive(track_previousb, (previous_track_id(track_dialog_id) != INVALID_TRACK_ID));
	}
    }
}


int mix_dialog_mix(void) {return(mix_dialog_id);}
void mix_dialog_set_mix(int id) {mix_dialog_id = id; update_mix_dialog(mix_dialog_id);}
int track_dialog_track(void) {return(track_dialog_id);}
void track_dialog_set_track(int id) {track_dialog_id = id; update_track_dialog(track_dialog_id);}


#if MUS_DEBUGGING && HAVE_SCHEME
static XEN g_internal_xmix_tests(void)
{
  if (w_env)
    {
      with_mix_background_wave = true;
      mix_amp_env_resize(w_env, NULL, NULL);
    }
  if (w_track_env)
    {
      with_track_background_wave = true;
      track_amp_env_resize(w_track_env, NULL, NULL);
    }
  return(XEN_FALSE);
}

void g_init_xmix(void);
void g_init_xmix(void)
{
  XEN_DEFINE_PROCEDURE("internal-xmix-tests", g_internal_xmix_tests, 0, 0, 0, "internal testing func");
}
#endif
