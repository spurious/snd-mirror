#include "snd.h"

#define NAME_COLUMNS 8

/* ---------------- mix dialog ---------------- */

static Widget mix_dialog = NULL;
static int mix_dialog_id = INVALID_MIX_ID, old_mix_dialog_id = INVALID_MIX_ID;
static env *dialog_env = NULL;

static bool dragging = false;
static int edpos_before_drag;
static with_hook_t hookable_before_drag;
static mus_long_t drag_beg = 0, drag_end = 0;

static void start_dragging(int mix_id) 
{
  chan_info *cp;
  cp = mix_chan_info_from_id(mix_id);
  edpos_before_drag = cp->edit_ctr;
  hookable_before_drag = cp->hookable;
  cp->hookable = WITHOUT_HOOK;
  dragging = true;
  drag_beg = mix_position_from_id(mix_id);
  drag_end = drag_beg + mix_length_from_id(mix_id);
  start_dragging_syncd_mixes(mix_id);
}


static void keep_dragging(int mix_id) 
{
  chan_info *cp;
  cp = mix_chan_info_from_id(mix_id);
  cp->edit_ctr = edpos_before_drag;
  keep_dragging_syncd_mixes(mix_id);
}


static void stop_dragging(int mix_id) 
{
  chan_info *cp;
  cp = mix_chan_info_from_id(mix_id);
  undo_edit(cp, 1);
  cp->hookable = hookable_before_drag;
  dragging = false;
  stop_dragging_syncd_mixes(mix_id);
}


/* -------- speed -------- */

static Widget w_speed_number, w_speed_label, w_speed;
static speed_style_t xmix_speed_control_style = SPEED_CONTROL_AS_FLOAT;

static int speed_to_scrollbar(mus_float_t minval, mus_float_t val, mus_float_t maxval)
{
  if (val <= minval) return(0);
  if (val >= maxval) return((int)(0.9 * SCROLLBAR_MAX));
  return(snd_round(0.9 * SCROLLBAR_MAX * ((log(val) - log(minval)) / (log(maxval) - log(minval)))));
}


static mus_float_t set_speed_label(Widget speed_number, int ival)
{
  char speed_number_buffer[6];
  mus_float_t speed;
  speed = speed_changed(exp((ival * (log(speed_control_max(ss)) - log(speed_control_min(ss))) / (0.9 * SCROLLBAR_MAX)) + log(speed_control_min(ss))),
			speed_number_buffer,
			xmix_speed_control_style,
			speed_control_tones(ss),
			6);
  set_label(speed_number, speed_number_buffer);
  return(speed);
}


static void mix_speed_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  char speed_number_buffer[6];
  mus_float_t speed;

  if (!(mix_is_active(mix_dialog_id))) return;
  speed = speed_changed(1.0,
			speed_number_buffer,
			xmix_speed_control_style,
			speed_control_tones(ss),
			6);

  drag_beg = mix_position_from_id(mix_dialog_id);
  drag_end = drag_beg + mix_length_from_id(mix_dialog_id);

  mix_set_speed_edit(mix_dialog_id, speed);
  syncd_mix_set_speed(mix_dialog_id, speed);
  after_mix_edit(mix_dialog_id);
  set_label(w_speed_number, speed_number_buffer);
  XtVaSetValues(w_speed, XmNvalue, speed_to_scrollbar(speed_control_min(ss), 1.0, speed_control_max(ss)), NULL);
}


static void mix_speed_label_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  char speed_number_buffer[6];
  if (!(mix_is_active(mix_dialog_id))) return;
  switch (xmix_speed_control_style)
    {
    default:
    case SPEED_CONTROL_AS_FLOAT:    xmix_speed_control_style = SPEED_CONTROL_AS_RATIO;    break;
    case SPEED_CONTROL_AS_RATIO:    xmix_speed_control_style = SPEED_CONTROL_AS_SEMITONE; break;
    case SPEED_CONTROL_AS_SEMITONE: xmix_speed_control_style = SPEED_CONTROL_AS_FLOAT;    break;
    }
  speed_changed(mix_speed_from_id(mix_dialog_id),
		speed_number_buffer,
		xmix_speed_control_style,
		speed_control_tones(ss),
		6);
  set_label(w_speed_number, speed_number_buffer);
}


static void mix_speed_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  int ival;
  mus_float_t speed;
  mus_long_t beg, end;

  if (!(mix_is_active(mix_dialog_id))) return;

  ival = ((XmScrollBarCallbackStruct *)info)->value;
  if (!dragging) 
    start_dragging(mix_dialog_id);
  else keep_dragging(mix_dialog_id);

  speed = set_speed_label(w_speed_number, ival);
  mix_set_speed_edit(mix_dialog_id, speed);

  beg = mix_position_from_id(mix_dialog_id);
  end = beg + mix_length_from_id(mix_dialog_id);
  if (drag_beg > beg) drag_beg = beg;
  if (drag_end < end) drag_end = end;

  mix_display_during_drag(mix_dialog_id, drag_beg, drag_end);
  syncd_mix_set_speed(mix_dialog_id, speed);
}


static void mix_speed_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  mus_float_t speed;

  if (!(mix_is_active(mix_dialog_id))) return;
  if (dragging)
    stop_dragging(mix_dialog_id);

  speed = set_speed_label(w_speed_number, cb->value);
  mix_set_speed_edit(mix_dialog_id, speed);
  syncd_mix_set_speed(mix_dialog_id, speed);
  after_mix_edit(mix_dialog_id);
  after_syncd_mix_edit(mix_dialog_id);
}


/* -------- amp -------- */

static Widget w_amp_number, w_amp_label, w_amp;

static mus_float_t scrollbar_to_amp(int val)
{
  if (val <= 0) 
    return(amp_control_min(ss));
  if (val >= (0.9 * SCROLLBAR_MAX)) 
    return(amp_control_max(ss));
  if (val > (0.5 * 0.9 * SCROLLBAR_MAX))
    return((((val / (0.5 * 0.9 * SCROLLBAR_MAX)) - 1.0) * (amp_control_max(ss) - 1.0)) + 1.0);
  else return((val * (1.0 - amp_control_min(ss)) / (0.5 * 0.9 * SCROLLBAR_MAX)) + amp_control_min(ss));
}


static int amp_to_scrollbar(Widget amp_number, mus_float_t amp)
{
  char sfs[6];
  mus_snprintf(sfs, 6, "%.2f", amp);
  set_label(amp_number, sfs);
  return(amp_to_scroll(amp_control_min(ss), amp, amp_control_max(ss)));
}


static void change_mix_amp(int mix_id, mus_float_t val)
{
  char sfs[6];
  mix_set_amp_edit(mix_id, val);
  syncd_mix_set_amp(mix_id, val);
  mus_snprintf(sfs, 6, "%.2f", val);
  set_label(w_amp_number, sfs);
}


static void mix_amp_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  if (!(mix_is_active(mix_dialog_id))) return;
  change_mix_amp(mix_dialog_id, 1.0);
  after_mix_edit(mix_dialog_id);
  XtVaSetValues(w_amp, XmNvalue, amp_to_scroll(amp_control_min(ss), 1.0, amp_control_max(ss)), NULL);
}


static void mix_amp_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  int ival;
  if (!(mix_is_active(mix_dialog_id))) return;
  ival = ((XmScrollBarCallbackStruct *)info)->value;
  if (!dragging) 
    start_dragging(mix_dialog_id);
  else keep_dragging(mix_dialog_id);
  change_mix_amp(mix_dialog_id, scrollbar_to_amp(ival));
  mix_display_during_drag(mix_dialog_id, drag_beg, drag_end);
}


static void mix_amp_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  int ival;
  if (!(mix_is_active(mix_dialog_id))) return;
  ival = ((XmScrollBarCallbackStruct *)info)->value;
  if (dragging)
    stop_dragging(mix_dialog_id);
  change_mix_amp(mix_dialog_id, scrollbar_to_amp(ival));
  after_mix_edit(mix_dialog_id);
  after_syncd_mix_edit(mix_dialog_id);
}


/* -------- amp-env -------- */

static Widget w_env_frame, w_env;
static graphics_context *ax = NULL;
static GC cur_gc;
static env_editor *spf = NULL;
static bool with_mix_background_wave = false;

static void show_mix_background_wave(int mix_id)
{
  int pts;
  bool two_sided = false;
  if (spf == NULL) return;
  pts = prepare_mix_dialog_waveform(mix_id, spf->axis, &two_sided);
  if (pts > 0)
    {
      XSetForeground(MAIN_DISPLAY(ss), ax->gc, ss->enved_waveform_color);
      if (two_sided)
	draw_both_grf_points(1, ax, pts, GRAPH_LINES);
      else draw_grf_points(1, ax, pts, spf->axis, ungrf_y(spf->axis, 0.0), GRAPH_LINES);
      XSetForeground(MAIN_DISPLAY(ss), ax->gc, ss->black);
    }
}


static void mix_amp_env_resize(Widget w, XtPointer context, XtPointer info) 
{
  env *cur_env;
  if (!(mix_is_active(mix_dialog_id))) return;

  if (ax == NULL)
    {
      XGCValues gv;
      gv.function = GXcopy;
      XtVaGetValues(w_env, XmNbackground, &gv.background, XmNforeground, &gv.foreground, NULL);
      cur_gc = XtGetGC(w_env, GCForeground | GCFunction, &gv);
      ax = (graphics_context *)calloc(1, sizeof(graphics_context));
      ax->wn = XtWindow(w_env);
      ax->dp = XtDisplay(w_env);
      ax->gc = cur_gc;
    }
  else clear_window(ax);

  cur_env = dialog_env;
  spf->with_dots = true;
  env_editor_display_env(spf, cur_env, ax, "mix env", 0, 0, widget_width(w), widget_height(w), NOT_PRINTING);
  if (with_mix_background_wave)
    show_mix_background_wave(mix_dialog_id);
}


#ifdef __APPLE__
static int press_x, press_y;
#endif

static void mix_drawer_button_motion(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  XMotionEvent *ev = (XMotionEvent *)event;
  if (!(mix_is_active(mix_dialog_id))) return;
#ifdef __APPLE__
  if ((press_x == ev->x) && (press_y == ev->y)) return;
#endif
  env_editor_button_motion(spf, ev->x, ev->y, ev->time, dialog_env);
  mix_amp_env_resize(w, NULL, NULL);
}


static void mix_drawer_button_press(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  XButtonEvent *ev = (XButtonEvent *)event;
  if (!(mix_is_active(mix_dialog_id))) return;
#ifdef __APPLE__
  press_x = ev->x;
  press_y = ev->y;
#endif
  if (env_editor_button_press(spf, ev->x, ev->y, ev->time, dialog_env))
    mix_amp_env_resize(w, NULL, NULL);
}


static void mix_drawer_button_release(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  if (!(mix_is_active(mix_dialog_id))) return;
  env_editor_button_release(spf, dialog_env);
  mix_amp_env_resize(w, NULL, NULL);
}


static Widget w_id = NULL, w_beg = NULL;
#if WITH_AUDIO
  static Widget mix_play = NULL;
#endif
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


static void widget_mix_to_text(Widget w, int id)
{
  if (mix_name(id))
    XmTextFieldSetString(w, (char *)mix_name(id));
  else widget_int_to_text(w, id);
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
      if (mix_is_active(id))
	{
	  mix_dialog_id = id;
	  reflect_mix_change(id);
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
  if (!(mix_is_active(mix_dialog_id))) return;
  val = XmTextGetString(w_beg);
  if (val)
    {
      chan_info *cp;
      char *up_to_colon;
      mus_float_t beg;
      cp = mix_chan_info_from_id(mix_dialog_id);
      up_to_colon = string_to_colon(val);
      redirect_errors_to(errors_to_mix_text, NULL);
      beg = string_to_mus_float_t(up_to_colon, 0.0, "begin time");
      redirect_errors_to(NULL, NULL);
      if (beg >= 0.0)
	{
	  mus_long_t pos, old_pos;
	  old_pos = mix_position_from_id(mix_dialog_id);
	  pos = (mus_long_t)(beg * SND_SRATE(cp->sound));
	  mix_set_position_edit(mix_dialog_id, pos);
	  syncd_mix_change_position(mix_dialog_id, pos - old_pos);
	}
      after_mix_edit(mix_dialog_id);
      free(up_to_colon);
      XtFree(val);
    }
}


static void apply_mix_dialog_callback(Widget w, XtPointer context, XtPointer info) 
{
  if (!(mix_is_active(mix_dialog_id))) return;
  if ((dialog_env) && 
      (!(default_env_p(dialog_env))))
    {
      mix_set_amp_env_edit(mix_dialog_id, dialog_env);
      syncd_mix_set_amp_env(mix_dialog_id, dialog_env);  
    }
  else 
    {
      mix_set_amp_env_edit(mix_dialog_id, NULL);
      syncd_mix_set_amp_env(mix_dialog_id, NULL);  
    }
  mix_amp_env_resize(w_env, NULL, NULL);
  after_mix_edit(mix_dialog_id);
}


static void copy_mix_dialog_callback(Widget w, XtPointer context, XtPointer info) 
{
  Widget active_widget;
  active_widget = XmGetFocusWidget(mix_dialog);
  if (active_widget == XmMessageBoxGetChild(mix_dialog, XmDIALOG_OK_BUTTON))
    {
      copy_mix(mix_dialog_id);
      after_mix_edit(mix_dialog_id);
    }
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


static void quit_mix_dialog_callback(Widget w, XtPointer context, XtPointer info) 
{
  clear_mix_error();
  XtUnmanageChild(mix_dialog);
}


static void help_mix_dialog_callback(Widget w, XtPointer context, XtPointer info) 
{
  mix_dialog_help();
}


/* -------- play -------- */

#if WITH_AUDIO
  static bool mix_playing = false;
#endif

void reflect_mix_play_stop(void)
{
#if WITH_AUDIO
  if (mix_play)
    XmChangeColor(mix_play, ss->basic_color);
  mix_playing = false;
#endif
}


#if WITH_AUDIO
static void mix_dialog_play_callback(Widget w, XtPointer context, XtPointer info) 
{
  if (mix_playing)
    reflect_mix_play_stop();
  else
    {
      if (!(mix_exists(mix_dialog_id))) return;
      if (mix_play)
	XmChangeColor(mix_play, ss->selection_color); /* this needs to happen before trying to play */
      syncd_mix_play(mix_dialog_id);
      mix_playing = true;                                      /* don't use the return value here */
      play_mix_from_id(mix_dialog_id);
    }
}
#endif


/* -------- dB -------- */

static void mix_dB_callback(Widget w, XtPointer context, XtPointer info)
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info; 
  spf->in_dB = cb->set;
  mix_amp_env_resize(w_env, NULL, NULL);
}


/* -------- sync -------- */

static void mix_sync_callback(Widget w, XtPointer context, XtPointer info)
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info; 
  if ((cb->set) &&
      (mix_sync_from_id(mix_dialog_id) == 0))
    {
      mix_set_sync_from_id(mix_dialog_id, GET_ORIGINAL_SYNC);  /* choose a new sync val or return to previous */
      /* check for resync */
      syncd_mix_set_color(mix_dialog_id, ss->red);
    }
  else
    {
      if ((!(cb->set)) &&
	  (mix_sync_from_id(mix_dialog_id) != 0))
	{
	  syncd_mix_unset_color(mix_dialog_id); /* unset colors of any syncd mixes */
	  mix_set_sync_from_id(mix_dialog_id, 0);
	}
    }
  for_each_normal_chan(display_channel_mixes);
}


/* -------- clip -------- */

static void mix_clip_callback(Widget w, XtPointer context, XtPointer info)
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info; 
  spf->clip_p = cb->set;
  mix_amp_env_resize(w_env, NULL, NULL);
}


/* -------- wave -------- */

static void mix_wave_callback(Widget w, XtPointer context, XtPointer info)
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info; 
  with_mix_background_wave = cb->set;
  mix_amp_env_resize(w_env, NULL, NULL);
}


/* -------- next/previous -------- */

static Widget nextb, previousb;

static void mix_next_callback(Widget w, XtPointer context, XtPointer info)
{
  int id;
  clear_mix_error();
  id = next_mix_id(mix_dialog_id);
  if (id != INVALID_MIX_ID)
    {
      mix_dialog_id = id;
      reflect_mix_change(id);
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
      reflect_mix_change(id);
      if (previous_mix_id(id) == INVALID_MIX_ID) 
	set_sensitive(previousb, false);
    }
}


static Pixmap make_pixmap(unsigned char *bits, int width, int height, int depth, GC gc)
{
  Pixmap rb, nr;
  rb = XCreateBitmapFromData(MAIN_DISPLAY(ss), 
			     RootWindowOfScreen(XtScreen(MAIN_PANE(ss))), 
			     (const char *)bits, 
			     width, height);
  nr = XCreatePixmap(MAIN_DISPLAY(ss), 
		     RootWindowOfScreen(XtScreen(MAIN_PANE(ss))), 
		     width, height, depth);
  XCopyPlane(MAIN_DISPLAY(ss), rb, nr, gc, 0, 0, width, height, 0, 0, 1);
  XFreePixmap(MAIN_DISPLAY(ss), rb);
  return(nr);
}



#define p_speaker_width 12
#define p_speaker_height 12
static unsigned char p_speaker_bits[] = {
   0x00, 0x07, 0xc0, 0x04, 0x30, 0x04, 0x0e, 0x04, 0x06, 0x04, 0x06, 0x04,
   0x06, 0x04, 0x06, 0x04, 0x0e, 0x04, 0x30, 0x04, 0xc0, 0x04, 0x00, 0x07};

static int mixer_depth;
static GC gc;
static Pixmap speaker_r;

void make_mixer_icons_transparent_again(Pixel old_color, Pixel new_color)
{
  if (mix_dialog)
    {
      XFreePixmap(XtDisplay(mix_dialog), speaker_r);
      XSetBackground(XtDisplay(mix_dialog), gc, new_color);
      speaker_r = make_pixmap(p_speaker_bits, p_speaker_width, p_speaker_height, mixer_depth, gc);
#if WITH_AUDIO
      XtVaSetValues(mix_play, XmNlabelPixmap, speaker_r, NULL);
#endif
    }
}

static Widget w_sync;

Widget make_mix_dialog(void) 
{
  if (mix_dialog == NULL)
    {
      Widget mainform, mix_row, mix_frame, sep, w_sep1;
      Widget w_dB_frame, w_dB, w_clip, w_wave, w_dB_row, env_button, copy_button;
      XmString xgo_away, xhelp, xtitle, s1, xcopy;
      int n;
      Arg args[20];
      XtCallbackList n1, n2;
      XGCValues v;
      char amplab[LABEL_BUFFER_SIZE];

      xmix_speed_control_style = speed_control_style(ss);

      mix_dialog_id = any_mix_id();
      xgo_away = XmStringCreateLocalized((char *)I_GO_AWAY);
      xcopy = XmStringCreateLocalized((char *)"Copy mix");
      xhelp = XmStringCreateLocalized((char *)I_HELP);
      xtitle = XmStringCreateLocalized((char *)"Mixes");

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNokLabelString, xcopy); n++;
      XtSetArg(args[n], XmNcancelLabelString, xgo_away); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNdialogTitle, xtitle); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNtransient, false); n++;
      mix_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), (char *)"Mixes", args, n);
      copy_button = XmMessageBoxGetChild(mix_dialog, XmDIALOG_OK_BUTTON);

      /* XtAddCallback(mix_dialog, XmNokCallback, copy_mix_dialog_callback, NULL); */
      XtAddCallback(copy_button, XmNactivateCallback, copy_mix_dialog_callback, NULL);
      XtAddCallback(mix_dialog, XmNcancelCallback, quit_mix_dialog_callback, NULL);
      XtAddCallback(mix_dialog, XmNhelpCallback, help_mix_dialog_callback, NULL);

      XmStringFree(xhelp);
      XmStringFree(xcopy);
      XmStringFree(xgo_away);
      XmStringFree(xtitle);

      XtVaSetValues(XmMessageBoxGetChild(mix_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, ss->selection_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(mix_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, ss->selection_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(mix_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, ss->selection_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(mix_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->highlight_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(mix_dialog, XmDIALOG_OK_BUTTON), XmNbackground, ss->highlight_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(mix_dialog, XmDIALOG_HELP_BUTTON), XmNbackground, ss->highlight_color, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->selection_color); n++;
      env_button = XtCreateManagedWidget("Apply env", xmPushButtonGadgetClass, mix_dialog, args, n);
      XtAddCallback(env_button, XmNactivateCallback, apply_mix_dialog_callback, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, XmMessageBoxGetChild(mix_dialog, XmDIALOG_SEPARATOR)); n++;
      mainform = XtCreateManagedWidget("formd", xmFormWidgetClass, mix_dialog, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 2); n++;
      mix_frame = XtCreateManagedWidget("mix-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      mix_row = XtCreateManagedWidget("mix-dialog-row", xmRowColumnWidgetClass, mix_frame, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtCreateManagedWidget("mix:", xmLabelWidgetClass, mix_row, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNresizeWidth, false); n++;
      XtSetArg(args[n], XmNcolumns, NAME_COLUMNS); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      w_id = make_textfield_widget("mix-id", mix_row, args, n, ACTIVATABLE, NO_COMPLETER);
      XtAddCallback(w_id, XmNlosingFocusCallback, id_check_callback, NULL);
      XtAddCallback(w_id, XmNmodifyVerifyCallback, id_modify_callback, NULL);
      XmTextSetString(w_id, (char *)"0");

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      w_beg = make_textfield_widget("mix-times", mix_row, args, n, ACTIVATABLE, NO_COMPLETER);
      XmTextSetString(w_beg, (char *)"0.000 : 1.000");

      XtVaGetValues(mix_row, XmNforeground, &v.foreground, XmNbackground, &v.background, XmNdepth, &mixer_depth, NULL);
      gc = XtGetGC(mix_row, GCForeground | GCBackground, &v);
      speaker_r = make_pixmap(p_speaker_bits, p_speaker_width, p_speaker_height, mixer_depth, gc);

#if WITH_AUDIO
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNlabelType, XmPIXMAP); n++;
      XtSetArg(args[n], XmNlabelPixmap, speaker_r); n++;
      XtSetArg(args[n], XmNarmColor, ss->selection_color); n++;
      mix_play = XtCreateManagedWidget("mix-play", xmPushButtonWidgetClass, mix_row, args, n);
      XtAddCallback(mix_play, XmNactivateCallback, mix_dialog_play_callback, NULL);
#endif

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->selection_color); n++;
      previousb = XtCreateManagedWidget(I_PREVIOUS, xmPushButtonWidgetClass, mix_row, args, n);
      if (previous_mix_id(mix_dialog_id) == INVALID_MIX_ID) 
	set_sensitive(previousb, false);
      XtAddCallback(previousb, XmNactivateCallback, mix_previous_callback, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->selection_color); n++;
      nextb = XtCreateManagedWidget(I_NEXT, xmPushButtonWidgetClass, mix_row, args, n);
      if (next_mix_id(mix_dialog_id) == INVALID_MIX_ID) 
	set_sensitive(nextb, false);
      XtAddCallback(nextb, XmNactivateCallback, mix_next_callback, NULL);



      /* separator before sliders */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
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
      s1 = XmStringCreateLocalized((char *)"speed:");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
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
      XtAddCallback(w_speed_label, XmNactivateCallback, mix_speed_click_callback, NULL);
      XmStringFree(s1);

      n = 0;
      s1 = initial_speed_label(xmix_speed_control_style);
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
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
      XtAddCallback(w_speed_number, XmNactivateCallback, mix_speed_label_click_callback, NULL);
      XmStringFree(s1);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->position_color); n++;
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
      XtSetArg(args[n], XmNdragCallback, n1 = make_callback_list(mix_speed_drag_callback, NULL)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n2 = make_callback_list(mix_speed_valuechanged_callback, NULL)); n++;
      w_speed = XtCreateManagedWidget("mix-speed", xmScrollBarWidgetClass, mainform, args, n);
  
      free(n1);
      free(n2);

      n = 0;
      mus_snprintf(amplab, LABEL_BUFFER_SIZE, "%s", "amp:");
      s1 = XmStringCreateLocalized(amplab);
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_speed_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      w_amp_label = make_pushbutton_widget("mix-amp-label", mainform, args, n);
      XtAddCallback(w_amp_label, XmNactivateCallback, mix_amp_click_callback, NULL);
      XmStringFree(s1);

      n = 0;
      s1 = XmStringCreateLocalized((char *)"1.00");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_speed_number); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, w_amp_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      w_amp_number = XtCreateManagedWidget("mix-amp-number", xmLabelWidgetClass, mainform, args, n);
      XmStringFree(s1);

      n = 0;      
      XtSetArg(args[n], XmNbackground, ss->position_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_amp_number); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, w_amp_number); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNvalue, 0); n++; 
      XtSetArg(args[n], XmNdragCallback, n1 = make_callback_list(mix_amp_drag_callback, NULL)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n2 = make_callback_list(mix_amp_valuechanged_callback, NULL)); n++;
      w_amp = XtCreateManagedWidget("mix-amp", xmScrollBarWidgetClass, mainform, args, n);
      free(n1);
      free(n2);

      /* separator before envelopes */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_amp_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNheight, 8); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      w_sep1 = XtCreateManagedWidget("mix-dialog-sep1", xmSeparatorWidgetClass, mainform, args, n);

      /* button box for dB clip wave sync */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_sep1); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 4); n++;
      w_dB_frame = XtCreateManagedWidget("mix-dB-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      w_dB_row = XtCreateManagedWidget("mix-dB-row", xmRowColumnWidgetClass, w_dB_frame, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNselectColor, ss->selection_color); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}

      w_clip = make_togglebutton_widget("clip", w_dB_row, args, n);
      XtAddCallback(w_clip, XmNvalueChangedCallback, mix_clip_callback, NULL);
      XmToggleButtonSetState(w_clip, true, false);

      w_wave = make_togglebutton_widget("wave", w_dB_row, args, n);
      XtAddCallback(w_wave, XmNvalueChangedCallback, mix_wave_callback, NULL);

      w_dB = make_togglebutton_widget("dB", w_dB_row, args, n);
      XtAddCallback(w_dB, XmNvalueChangedCallback, mix_dB_callback, NULL);

      if (mix_sync_from_id(mix_dialog_id) != 0)
	{XtSetArg(args[n], XmNset, true); n++;}
      w_sync = make_togglebutton_widget("sync", w_dB_row, args, n);
      XtAddCallback(w_sync, XmNvalueChangedCallback, mix_sync_callback, NULL);


      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 2); n++;
      error_frame = XtCreateManagedWidget("error-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      error_label = XtCreateManagedWidget("", xmLabelWidgetClass, error_frame, args, n);

      
      /* amp env */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      w_env = XtCreateManagedWidget("mix-amp-env-window", xmDrawingAreaWidgetClass, w_env_frame, args, n);

      XtManageChild(mix_dialog);

      XtAddCallback(w_env, XmNresizeCallback, mix_amp_env_resize, NULL);
      XtAddCallback(w_env, XmNexposeCallback, mix_amp_env_resize, NULL);

      spf = new_env_editor();

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
    }
  reflect_mix_change(mix_dialog_id);
  return(mix_dialog);
}


void reflect_mix_change(int mix_id)
{
  if ((mix_dialog) && 
      (XtIsManaged(mix_dialog)))
    {
      if (mix_id != ANY_MIX_ID)
	mix_dialog_id = mix_id;

      if (!(mix_exists(mix_dialog_id))) 
	{
	  mix_dialog_id = any_mix_id(); 
	  mix_id = mix_dialog_id;
	}

      if ((mix_id == mix_dialog_id) || (mix_id == ANY_MIX_ID))
	{
	  mus_float_t val;
	  set_sensitive(nextb, (next_mix_id(mix_dialog_id) != INVALID_MIX_ID));
	  set_sensitive(previousb, (previous_mix_id(mix_dialog_id) != INVALID_MIX_ID));

	  /* now reflect current mix state in mix dialog controls */
	  if (mix_exists(mix_dialog_id))
	    {
	      chan_info *cp = NULL;
	      mus_long_t beg, len;
	      char lab[LABEL_BUFFER_SIZE];
	      
	      /* syncd mixes have the same color (red) reverting to old color when sync changes */
	      cp = mix_chan_info_from_id(mix_dialog_id);
	      if (old_mix_dialog_id != INVALID_MIX_ID)
		{
		  mix_unset_color_from_id(old_mix_dialog_id);
		  syncd_mix_unset_color(old_mix_dialog_id);
		}
	      old_mix_dialog_id = mix_dialog_id;
	      mix_set_color_from_id(mix_dialog_id, ss->red);
	      syncd_mix_set_color(mix_dialog_id, ss->red);

	      for_each_normal_chan(display_channel_mixes);

	      if (!dragging)
		{
		  val = mix_speed_from_id(mix_dialog_id);
		  XtVaSetValues(w_speed, XmNvalue, speed_to_scrollbar(speed_control_min(ss), val, speed_control_max(ss)), NULL);
		  speed_changed(val, lab, xmix_speed_control_style, speed_control_tones(ss), 6);
		  set_label(w_speed_number, lab);
		}
	      widget_mix_to_text(w_id, mix_dialog_id);
	      
	      beg = mix_position_from_id(mix_dialog_id);
	      len = mix_length_from_id(mix_dialog_id);
	      mus_snprintf(lab, LABEL_BUFFER_SIZE, "%.3f : %.3f%s",
			   (float)((double)beg / (float)SND_SRATE(cp->sound)),
			   (float)((double)(beg + len) / (float)SND_SRATE(cp->sound)),
			   (mix_is_active(mix_dialog_id)) ? "" : " (locked)");
	      XmTextSetString(w_beg, lab);
	      
	      set_sensitive(XmMessageBoxGetChild(mix_dialog, XmDIALOG_OK_BUTTON), true);
	    }
	  else
	    {
	      XmTextSetString(w_id, (char *)"-1");
	      XmTextSetString(w_beg, (char *)"no active mixes");
	      set_sensitive(XmMessageBoxGetChild(mix_dialog, XmDIALOG_OK_BUTTON), false);
	    }
	  if (!dragging)
	    {
	      if (mix_is_active(mix_dialog_id))
		val = mix_amp_from_id(mix_dialog_id);
	      else val = 1.0;
	      XtVaSetValues(w_amp, XmNvalue, amp_to_scrollbar(w_amp_number, val), NULL);
	    }
	  
	  if (mix_amp_env_from_id(mix_dialog_id))
	    {
	      if (dialog_env) free_env(dialog_env);
	      dialog_env = copy_env(mix_amp_env_from_id(mix_dialog_id));
	    }
	  /* copy here else we're editing it directly afterwards (and we free old in mix_set_amp_env_edit) */
	  if (!dialog_env) 
	    dialog_env = default_env(1.0, 1.0);
	  mix_amp_env_resize(w_env, NULL, NULL);

	  set_toggle_button(w_sync, (mix_sync_from_id(mix_dialog_id) != 0), false, NULL);
	}
    }
}


int mix_dialog_mix(void) 
{
  return(mix_dialog_id);
}


void mix_dialog_set_mix(int id) 
{
  mix_dialog_id = id; 
  reflect_mix_change(mix_dialog_id);
}



