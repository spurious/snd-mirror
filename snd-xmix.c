#include "snd.h"

/* 
 * TODO: make amp env display editable -- each change would affect the original
 *        (not an edit history addition to the mixed sound), but would each be
 *        a separate edit of the underlying sound? (Then we'd need a stack of
 *        mix envs so we could back up).
 * TODO: add a sync button to affect all of track?  Also need a way to display all related mixes at once (panning etc)
 */

static Widget mix_panel = NULL;
static int dragging = 0;
static void update_mix_panel(int mix_id);


/* ---------------- SPEED ---------------- */

static char speed_number_buffer[5] ={'1', STR_decimal, '0', '0', '\0'};
#define SPEED_SCROLLBAR_MAX 1000
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
  snd_state *ss = (snd_state *)context;
  change_mix_speed(current_mix_id(ss), 1.0);
  XtVaSetValues(w_speed, XmNvalue, 450, NULL);
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
      ival = snd_round(450.0 + 150.0 * log(val));
      if (ival < SPEED_SCROLLBAR_MAX)
	return(ival);
      else return(SPEED_SCROLLBAR_MAX);
    }
  else return(0);
}

static void speed_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  int ival;
  snd_state *ss = (snd_state *)context;
  ival = ((XmScrollBarCallbackStruct *)info)->value;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (dragging == 0) start_mix_drag(current_mix_id(ss));
  dragging = 1;
  change_mix_speed(current_mix_id(ss), exp((Float)(ival - 450.0) / 150.0));
}

static void speed_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_state *ss = (snd_state *)context;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  dragging = 0;
  change_mix_speed(current_mix_id(ss), exp((Float)(cb->value - 450.0) / 150.0));
}


/* ---------------- AMP ---------------- */

static Widget *w_amp_numbers, *w_amp_labels, *w_amps;
static Float *current_amps;
static int chans_allocated = 0;
static char amp_number_buffer[5] ={'1', STR_decimal, '0', '0', '\0'};

static int allocate_amps(int chans)
{
  int i;
  if (chans > chans_allocated)
    {
      if (chans_allocated == 0)
	{
	  if (chans < 4)
	    chans_allocated = 4;
	  else chans_allocated = chans;
	  w_amp_numbers = (Widget *)CALLOC(chans_allocated, sizeof(Widget));
	  w_amp_labels = (Widget *)CALLOC(chans_allocated, sizeof(Widget));
	  w_amps = (Widget *)CALLOC(chans_allocated, sizeof(Widget));
	  current_amps = (Float *)CALLOC(chans_allocated, sizeof(Float));
	}
      else
	{
	  w_amp_numbers = (Widget *)REALLOC(w_amp_numbers, chans * sizeof(Widget));
	  w_amp_labels = (Widget *)REALLOC(w_amp_labels, chans * sizeof(Widget));
	  w_amps = (Widget *)REALLOC(w_amps, chans * sizeof(Widget));
	  current_amps = (Float *)REALLOC(current_amps, chans * sizeof(Float));
	  for (i = chans_allocated; i < chans; i++)
	    {
	      w_amp_numbers[i] = NULL;
	      w_amp_labels[i] = NULL;
	      w_amps[i] = NULL;
	      current_amps[i] = 0.0;
	    }
	  chans_allocated = chans;
	}
    }
  return(chans_allocated);
}

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
  snd_state *ss = (snd_state *)context;
  int chan;
  XtVaGetValues(w, XmNuserData, &chan, NULL);
  change_mix_amp(current_mix_id(ss), chan, 1.0);
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
  snd_state *ss = (snd_state *)context;
  XtVaGetValues(w, XmNuserData, &chan, NULL);
  ival = ((XmScrollBarCallbackStruct *)info)->value;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  if (dragging == 0) start_mix_drag(current_mix_id(ss));
  dragging = 1;
  change_mix_amp(current_mix_id(ss), chan, int_amp_to_Float(ival));
}

static void amp_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  int ival, chan;
  snd_state *ss = (snd_state *)context;
  ival = ((XmScrollBarCallbackStruct *)info)->value;
  XtVaGetValues(w, XmNuserData, &chan, NULL);
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  dragging = 0;
  change_mix_amp(current_mix_id(ss), chan, int_amp_to_Float(ival));
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
static chan_info *axis_cp = NULL;
static axis_context *ax = NULL;
static GC cur_gc;

static void mix_amp_env_resize(Widget w, XtPointer context, XtPointer info) 
{
  snd_state *ss = (snd_state *)context;
  XGCValues gv;

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
  if (axis_cp == NULL) 
    {
      axis_cp = new_env_axis(ss); /* just creates the axis struct and sets basic fields */
      fixup_axis_context(axis_cp->axis->ax, w_env, ax->gc);
    }
  display_mix_amp_envs(ss, axis_cp, ax, widget_width(w), widget_height(w));
}



/* ---------------- MIX PANEL ---------------- */

static Widget w_id = NULL, w_name = NULL, w_beg = NULL, w_track = NULL, w_play = NULL;

static void track_activated(snd_state *ss)
{
  char *val;
  val = XmTextGetString(w_track);
  if (val)
    {
      set_mix_track_from_id(current_mix_id(ss), string2int(val));
      XtFree(val);
    }
}

static void id_activated(snd_state *ss)
{
  char *val;
  int id;
  val = XmTextGetString(w_id);
  if (val)
    {
      id = string2int(val);
      if (mix_ok_and_unlocked(id))
	{
	  ss->selected_mix = id;
	  update_mix_panel(ss->selected_mix);
	}
      XtFree(val);
    }
}

static void beg_activated(snd_state *ss)
{
  char *val;
  chan_info *cp;
  int mix_id;
  val = XmTextGetString(w_beg);
  if (val)
    {
      mix_id = current_mix_id(ss);
      cp = mix_channel_from_id(mix_id);
      set_mix_position_from_id(mix_id, (int)(string2Float(val) * SND_SRATE(cp->sound)));
      update_mix_panel(mix_id);
      XtFree(val);
    }
}

static void name_activated(snd_state *ss)
{
  char *val;
  val = XmTextGetString(w_track);
  if (val)
    {
      /* look for mix by this name?? */
      set_mix_name_from_id(current_mix_id(ss), val);
      XtFree(val);
    }
}

static void dismiss_mix_panel_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_state *ss = (snd_state *)context;
  state_context *sgx;
  XmAnyCallbackStruct *cb = (XmAnyCallbackStruct *)info;
  sgx = ss->sgx;
  if (cb->event != sgx->text_activate_event)
    XtUnmanageChild(mix_panel);
  else
    {
      if (sgx->text_widget == w_track)
	track_activated(ss);
      else
	{
	  if (sgx->text_widget == w_id)
	    id_activated(ss);
	  else
	    {
	      if (sgx->text_widget == w_name)
		name_activated(ss);
	      else
		{
		  if (sgx->text_widget == w_beg)
		    beg_activated(ss);
		}
	    }
	}
    }
}

static void help_mix_panel_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help_with_wrap((snd_state *)context,
		     "Mix Panel",
"This dialog controls the currently selected mix");
}


static int mix_playing = 0;
int mix_play_stopped(void) {return(!mix_playing);}

void reflect_mix_play_stop(void)
{
  snd_state *ss;
  if (w_play) 
    {
      ss = get_global_state();
      XmChangeColor(w_play, (ss->sgx)->basic_color);
    }
  mix_playing = 0;
}

static void mix_panel_play_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_state *ss;
  if (mix_playing)
    {
      reflect_mix_play_stop();
    }
  else
    {
      ss = get_global_state();
      mix_playing = 1;
      if (w_play) XmChangeColor(w_play, (ss->sgx)->pushed_button_color);
      mix_play_from_id(current_mix_id(ss));
    }
}

#define p_speaker_width 12
#define p_speaker_height 12
static unsigned char p_speaker_bits[] = {
   0x00, 0x07, 0xc0, 0x04, 0x30, 0x04, 0x0e, 0x04, 0x06, 0x04, 0x06, 0x04,
   0x06, 0x04, 0x06, 0x04, 0x0e, 0x04, 0x30, 0x04, 0xc0, 0x04, 0x00, 0x07};

Widget make_mix_panel(snd_state *ss) 
{
  Widget mainform, w_row, last_label, last_number;
  Pixmap speaker_r;
  XmString xdismiss, xhelp, xtitle, s1;
  int n, chans, i;
  Arg args[20];
  XtCallbackList n1, n2;
  GC gc;
  int depth;
  XGCValues v;
  char amplab[LABEL_BUFFER_SIZE];
  if (mix_panel == NULL)
    {
      xdismiss = XmStringCreate(STR_Dismiss, XmFONTLIST_DEFAULT_TAG);
      xhelp = XmStringCreate(STR_Help, XmFONTLIST_DEFAULT_TAG);
      xtitle = XmStringCreate(STR_Mix_Panel, XmFONTLIST_DEFAULT_TAG);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNokLabelString, xdismiss); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNautoUnmanage, FALSE); n++;
      XtSetArg(args[n], XmNdialogTitle, xtitle); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, FALSE); n++;
      XtSetArg(args[n], XmNtransient, FALSE); n++;
      mix_panel = XmCreateTemplateDialog(MAIN_SHELL(ss), STR_Mix_Panel, args, n);

      XtAddCallback(mix_panel, XmNokCallback, dismiss_mix_panel_callback, ss);
      XtAddCallback(mix_panel, XmNhelpCallback, help_mix_panel_callback, ss);

      XmStringFree(xhelp);
      XmStringFree(xdismiss);
      XmStringFree(xtitle);

      if (!(ss->using_schemes))
	{
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
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      w_row = XtCreateManagedWidget("mix-panel-row", xmRowColumnWidgetClass, mainform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtCreateManagedWidget("mix id:", xmLabelWidgetClass, w_row, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNresizeWidth, FALSE); n++;
      XtSetArg(args[n], XmNcolumns, 3); n++;
      XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
      w_id = make_textfield_widget(ss, "mix-id", w_row, args, n, ACTIVATABLE, NO_COMPLETER);
      XmTextSetString(w_id, "0");

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      w_name = make_textfield_widget(ss, "mix-name", w_row, args, n, ACTIVATABLE, NO_COMPLETER);
      XmTextSetString(w_name, "mix");

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      w_beg = make_textfield_widget(ss, "mix-times", w_row, args, n, ACTIVATABLE, NO_COMPLETER);
      XmTextSetString(w_beg, "0.000 : 1.000");

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtCreateManagedWidget("track:", xmLabelWidgetClass, w_row, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNresizeWidth, FALSE); n++;
      XtSetArg(args[n], XmNcolumns, 3); n++;
      XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
      w_track = make_textfield_widget(ss, "mix-track", w_row, args, n, ACTIVATABLE, NO_COMPLETER);
      XmTextSetString(w_track, "0");


      XtVaGetValues(w_row, XmNforeground, &v.foreground, XmNbackground, &v.background, XmNdepth, &depth, NULL);
      gc = XtGetGC(w_row, GCForeground | GCBackground, &v);
      speaker_r = make_pixmap(ss, p_speaker_bits, p_speaker_width, p_speaker_height, depth, gc);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNlabelType, XmPIXMAP); n++;
      XtSetArg(args[n], XmNlabelPixmap, speaker_r); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;}
      w_play = XtCreateManagedWidget("play", xmPushButtonWidgetClass, w_row, args, n);
      XtAddCallback(w_play, XmNactivateCallback, mix_panel_play_callback, ss);

      /* SRATE */
      n = 0;
      s1 = XmStringCreate(STR_speed, XmFONTLIST_DEFAULT_TAG);
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_row); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, FALSE); n++;
      w_speed_label = make_pushbutton_widget("speed-label", mainform, args, n);
      XtAddCallback(w_speed_label, XmNactivateCallback, speed_click_callback, ss);
      XmStringFree(s1);

      n = 0;
      s1 = initial_speed_label(ss);
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, w_speed_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, w_speed_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
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
      XtSetArg(args[n], XmNmaximum, SPEED_SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNvalue, 450); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNdragCallback, n1 = make_callback_list(speed_drag_callback, (XtPointer)ss)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n2 = make_callback_list(speed_valuechanged_callback, (XtPointer)ss)); n++;
      w_speed = XtCreateManagedWidget("speed", xmScrollBarWidgetClass, mainform, args, n);
  
      FREE(n1);
      FREE(n2);
      last_label = w_speed_label;
      last_number = w_speed_number;

      /* now amp scalers */

      chans = allocate_amps(8);
      for (i = 0; i < chans; i++)
	{
	  n = 0;
	  mus_snprintf(amplab, LABEL_BUFFER_SIZE, "amp %d:", i);
	  s1 = XmStringCreate(amplab, XmFONTLIST_DEFAULT_TAG);
	  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
	  XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNtopWidget, last_label); n++;
	  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
	  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
	  XtSetArg(args[n], XmNlabelString, s1); n++;
	  XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
	  XtSetArg(args[n], XmNshadowThickness, 0); n++;
	  XtSetArg(args[n], XmNhighlightThickness, 0); n++;
	  XtSetArg(args[n], XmNfillOnArm, FALSE); n++;
	  XtSetArg(args[n], XmNuserData, i); n++;
	  w_amp_labels[i] = make_pushbutton_widget("amp-label", mainform, args, n);
	  XtAddCallback(w_amp_labels[i], XmNactivateCallback, amp_click_callback, ss);
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
	  XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
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
	  XtSetArg(args[n], XmNdragCallback, n1 = make_callback_list(amp_drag_callback, (XtPointer)ss)); n++;
	  XtSetArg(args[n], XmNvalueChangedCallback, n2 = make_callback_list(amp_valuechanged_callback, (XtPointer)ss)); n++;
	  w_amps[i] = XtCreateManagedWidget("amp", xmScrollBarWidgetClass, mainform, args, n);
	  FREE(n1);
	  FREE(n2);
	  last_label = w_amp_labels[i];
	  last_number = w_amp_numbers[i];
	}

      /* amp env */
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, last_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 4); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 98); n++;
      XtSetArg(args[n], XmNallowResize, TRUE); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 4); n++;
      w_env_frame = XtCreateManagedWidget("amp-env-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, TRUE); n++;
      w_env = XtCreateManagedWidget("amp-env-window", xmDrawingAreaWidgetClass, w_env_frame, args, n);

      XtManageChild(mix_panel);

      XtAddCallback(w_env, XmNresizeCallback, mix_amp_env_resize, ss);
      XtAddCallback(w_env, XmNexposeCallback, mix_amp_env_resize, ss);
      set_dialog_widget(ss, MIX_PANEL_DIALOG, mix_panel);
    }
  else raise_dialog(mix_panel);
  if (!(XtIsManaged(mix_panel))) XtManageChild(mix_panel);

  update_mix_panel(current_mix_id(ss));
  return(mix_panel);
}

static void update_mix_panel(int mix_id) 
{
  snd_state *ss;
  chan_info *cp;
  int i, chans, beg, len;
  Float val;
  char lab[LABEL_BUFFER_SIZE];
  ss = get_global_state();
  if (mix_id == INVALID_MIX_ID) return;
  if (mix_id == current_mix_id(ss))
    {
      if (mix_panel == NULL) 
	make_mix_panel(ss);
      
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

      XmTextSetString(w_name, mix_name_from_id(mix_id));

      beg = mix_position_from_id(mix_id);
      len = mix_length(mix_id);
      mus_snprintf(lab, LABEL_BUFFER_SIZE, "%.3f : %.3f",
	      (float)beg / (float)SND_SRATE(cp->sound),
	      (float)(beg + len) / (float)SND_SRATE(cp->sound));
      XmTextSetString(w_beg, lab);

      chans = mix_input_chans_from_id(mix_id);
      if (chans > 8) chans = 8; 

      for (i = 0; i < chans; i++)
	{
	  if (!(XtIsManaged(w_amp_labels[i]))) XtManageChild(w_amp_labels[i]);
	  if (!(XtIsManaged(w_amp_numbers[i]))) XtManageChild(w_amp_numbers[i]);
	  val = mix_amp_from_id(mix_id, i);
	  if (val != current_amps[i])
	    {
	      XtVaSetValues(w_amps[i], XmNvalue, mix_amp_to_int(val, i), NULL);
	      current_amps[i] = val;
	    }
	  if (!(XtIsManaged(w_amps[i]))) XtManageChild(w_amps[i]);
	}
      for (i = chans; i < chans_allocated; i++)
	{
	  if ((w_amp_labels[i]) && (XtIsManaged(w_amp_labels[i])))
	    {
	      XtUnmanageChild(w_amp_labels[i]);
	      XtUnmanageChild(w_amp_numbers[i]);
	      XtUnmanageChild(w_amps[i]);
	    }
	}
      XtVaSetValues(w_env_frame, XmNtopWidget, w_amp_labels[chans - 1], NULL);

      mix_amp_env_resize(w_env, (XtPointer)ss, NULL);

    }
}


void reflect_mix_in_mix_panel(int mix_id)
{
  if ((mix_panel) && 
      (XtIsManaged(mix_panel)) &&
      (current_mix_id(get_global_state()) == mix_id))
    update_mix_panel(mix_id);
}

void reflect_no_mix_in_mix_panel(void)
{
  if ((mix_panel) &&
      (XtIsManaged(mix_panel)))
    XtUnmanageChild(mix_panel);
}
