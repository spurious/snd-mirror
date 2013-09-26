#include "snd.h"
#include <X11/xpm.h>

#define TOGGLE_MARGIN 0

enum {W_pane,
      W_name_form, W_amp_form,
      W_amp, W_amp_label, W_amp_number, 
      W_speed, W_speed_label, W_speed_number, W_speed_arrow,
      W_expand, W_expand_label, W_expand_number, W_expand_button,
      W_contrast, W_contrast_label, W_contrast_number, W_contrast_button,
      W_revscl, W_revscl_label, W_revscl_number,
      W_revlen, W_revlen_label, W_revlen_number, W_reverb_button,
      W_filter_label, W_filter_order, W_filter_env, W_filter, W_filter_button, W_filter_dB, W_filter_hz, W_filter_frame,
      W_filter_order_down, W_filter_order_up,
      W_name, W_lock_or_bomb, W_stop_icon, W_info,
      W_play, W_sync, W_unite, W_close,
      NUM_SND_WIDGETS
};


Widget unite_button(snd_info *sp) {return(sp->snd_widgets[W_unite]);}
Widget w_snd_pane(snd_info *sp)   {return(sp->snd_widgets[W_pane]);}

#define SND_PANE(Sp)             Sp->snd_widgets[W_pane]
#define SND_NAME(Sp)             Sp->snd_widgets[W_name]

#define NAME_BOX(Sp)             Sp->snd_widgets[W_name_form]
#define LOCK_OR_BOMB(Sp)         Sp->snd_widgets[W_lock_or_bomb]
#define STOP_ICON(Sp)            Sp->snd_widgets[W_stop_icon]
#define NAME_LABEL(Sp)           Sp->snd_widgets[W_name]
#define STATUS_AREA(Sp)      Sp->snd_widgets[W_info]
#define SYNC_BUTTON(Sp)          Sp->snd_widgets[W_sync]
#define PLAY_BUTTON(Sp)          Sp->snd_widgets[W_play]
#define UNITE_BUTTON(Sp)         Sp->snd_widgets[W_unite]
#define CLOSE_BUTTON(Sp)         Sp->snd_widgets[W_close]

#define CONTROLS(Sp)             Sp->snd_widgets[W_amp_form]
#define AMP_SCROLLBAR(Sp)        Sp->snd_widgets[W_amp]
#define AMP_LABEL(Sp)            Sp->snd_widgets[W_amp_number]
#define AMP_BUTTON(Sp)           Sp->snd_widgets[W_amp_label]

#define SPEED_SCROLLBAR(Sp)      Sp->snd_widgets[W_speed]
#define SPEED_ARROW(Sp)          Sp->snd_widgets[W_speed_arrow]
#define SPEED_LABEL(Sp)          Sp->snd_widgets[W_speed_number]
#define SPEED_BUTTON(Sp)         Sp->snd_widgets[W_speed_label]

#define EXPAND_SCROLLBAR(Sp)     Sp->snd_widgets[W_expand]
#define EXPAND_LABEL(Sp)         Sp->snd_widgets[W_expand_number]
#define EXPAND_RIGHT_BUTTON(Sp)  Sp->snd_widgets[W_expand_button]
#define EXPAND_LEFT_BUTTON(Sp)   Sp->snd_widgets[W_expand_label]

#define CONTRAST_SCROLLBAR(Sp)   Sp->snd_widgets[W_contrast]
#define CONTRAST_LABEL(Sp)       Sp->snd_widgets[W_contrast_number]
#define CONTRAST_RIGHT_BUTTON(Sp) Sp->snd_widgets[W_contrast_button]
#define CONTRAST_LEFT_BUTTON(Sp) Sp->snd_widgets[W_contrast_label]

#define REVSCL_SCROLLBAR(Sp)     Sp->snd_widgets[W_revscl]
#define REVLEN_SCROLLBAR(Sp)     Sp->snd_widgets[W_revlen]
#define REVSCL_LABEL(Sp)         Sp->snd_widgets[W_revscl_number]
#define REVLEN_LABEL(Sp)         Sp->snd_widgets[W_revlen_number]
#define REVSCL_BUTTON(Sp)        Sp->snd_widgets[W_revscl_label]
#define REVLEN_BUTTON(Sp)        Sp->snd_widgets[W_revlen_label]
#define REVERB_BUTTON(Sp)        Sp->snd_widgets[W_reverb_button]

#define FILTER_ORDER_TEXT(Sp)    Sp->snd_widgets[W_filter_order]
#define FILTER_COEFFS_TEXT(Sp)   Sp->snd_widgets[W_filter]
#define FILTER_BUTTON(Sp)        Sp->snd_widgets[W_filter_button]
#define FILTER_DB_BUTTON(Sp)     Sp->snd_widgets[W_filter_dB]
#define FILTER_HZ_BUTTON(Sp)     Sp->snd_widgets[W_filter_hz]
#define FILTER_LABEL(Sp)         Sp->snd_widgets[W_filter_label]
#define FILTER_GRAPH(Sp)         Sp->snd_widgets[W_filter_env]
#define FILTER_ORDER_UP(Sp)      Sp->snd_widgets[W_filter_order_up]
#define FILTER_ORDER_DOWN(Sp)    Sp->snd_widgets[W_filter_order_down]
#define FILTER_FRAME(Sp)         Sp->snd_widgets[W_filter_frame]

#define PROGRESS_ICON(Cp)        (Cp)->sound->progress_widgets[(Cp)->chan]


int snd_pane_height(snd_info *sp)
{
  Dimension height;
  XtVaGetValues(SND_PANE(sp), XmNheight, &height, NULL);
  return((int)height);
}


void set_status(snd_info *sp, const char *str, bool update) 
{
  if ((sp->inuse != SOUND_NORMAL) || (!HAS_WIDGETS(sp))) return;
  XmTextSetString(STATUS_AREA(sp), (char *)str);
  /* updating clears the entire graph widget and triggers an expose event -- this is evil if we're currently displaying! */
  /* there's also a bug in libxcb (fixed, but not propagated yet) that causes a segfault here if more than
   *   one thread is affected by this global X queue flush.
   */

  if (update) XmUpdateDisplay(STATUS_AREA(sp));
}


static void name_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  char *str;
  snd_info *sp = (snd_info *)context;
  str = sp_name_click(sp);
  if (str)
    {
      status_report(sp, "%s", str);
      free(str);
    }
}


static void stop_sign_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_info *sp = (snd_info *)context;
  if ((ss->checking_explicitly) || (play_in_progress()))
    ss->stopped_explicitly = true; 
  stop_playing_all_sounds(PLAY_C_G);
  if (sp->applying) stop_applying(sp);
  for_each_sound_chan(sp, stop_fft_in_progress);
}


/* The 0.9 * SCROLLBAR_MAX reflects the fact that the slider is 10% of the trough, and the left edge of the slider is the readback value */

/* ---------------- AMP-CONTROL ---------------- */

int amp_to_scroll(mus_float_t minval, mus_float_t val, mus_float_t maxval)
{
  if (val <= minval) return(0);
  if (val >= maxval) return((int)(0.9 * SCROLLBAR_MAX));
  if (val >= 1.0)
    return(snd_round(0.9 * 0.5 * SCROLLBAR_MAX * (1.0 + (val - 1.0) / (maxval - 1.0))));
  return(snd_round(0.9 * 0.5 * SCROLLBAR_MAX * ((val - minval) / (1.0 - minval))));
}


static int scroll_to_amp(snd_info *sp, int val)
{
  char amp_number_buffer[6];
  if (val <= 0) 
    sp->amp_control = sp->amp_control_min;
  else
    {
      if (val >= (0.9 * SCROLLBAR_MAX)) 
	sp->amp_control = sp->amp_control_max;
      else
	{
	  if (val > (0.5 * 0.9 * SCROLLBAR_MAX))
	    sp->amp_control = (((val / (0.5 * 0.9 * SCROLLBAR_MAX)) - 1.0) * (sp->amp_control_max - 1.0)) + 1.0;
	  else sp->amp_control = (val * (1.0 - sp->amp_control_min) / (0.5 * 0.9 * SCROLLBAR_MAX)) + sp->amp_control_min;
	}
    }
  snprintf(amp_number_buffer, 6, "%.3f", sp->amp_control);
  set_label(AMP_LABEL(sp), amp_number_buffer);
  return(val);
}


void set_amp(snd_info *sp, mus_float_t val)
{
  if (!HAS_WIDGETS(sp))
    sp->amp_control = val;
  else XtVaSetValues(AMP_SCROLLBAR(sp),
		     XmNvalue,
		     scroll_to_amp(sp, amp_to_scroll(sp->amp_control_min, val, sp->amp_control_max)),
		     NULL);
}


static void amp_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  XButtonEvent *ev;
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    set_amp(sp, sp->last_amp_control);
  else set_amp(sp, 1.0);
}


static void amp_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  scroll_to_amp((snd_info *)context, ((XmScrollBarCallbackStruct *)info)->value);
}


static void amp_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  scroll_to_amp(sp, cb->value);
  sp->last_amp_control = sp->saved_amp_control;
  sp->saved_amp_control = sp->amp_control;
}


/* ---------------- SPEED-CONTROL ---------------- */

XmString initial_speed_label(speed_style_t style)
{
  /* used also in snd-xmix.c */
  switch (style)
    {
    case SPEED_CONTROL_AS_RATIO:    return(XmStringCreateLocalized((char *)"  1/1")); break;
    case SPEED_CONTROL_AS_SEMITONE: return(XmStringCreateLocalized((char *)"    0")); break;
    default:                        return(XmStringCreateLocalized((char *)" 1.00")); break;
    }
}


static int speed_to_scroll(mus_float_t minval, mus_float_t val, mus_float_t maxval)
{
  if (val <= minval) return(0);
  if (val >= maxval) return((int)(0.9 * SCROLLBAR_MAX));
  return(snd_round(0.9 * SCROLLBAR_MAX * ((log(val) - log(minval)) / (log(maxval) - log(minval)))));
}


static int scroll_to_speed(snd_info *sp, int ival)
{
  char speed_number_buffer[6];
  sp->speed_control = speed_changed(exp((ival * (log(sp->speed_control_max) - log(sp->speed_control_min)) / 
					 (0.9 * SCROLLBAR_MAX)) + 
					log(sp->speed_control_min)),
				    speed_number_buffer,
				    sp->speed_control_style,
				    sp->speed_control_tones,
				    6);
  set_label(SPEED_LABEL(sp), speed_number_buffer);
  /* set_label works with buttons or labels */
  return(ival);
}


void set_speed(snd_info *sp, mus_float_t val)
{
  if (!HAS_WIDGETS(sp))
    sp->speed_control = val;
  else XtVaSetValues(SPEED_SCROLLBAR(sp),
		     XmNvalue,
		     scroll_to_speed(sp, speed_to_scroll(sp->speed_control_min, val, sp->speed_control_max)),
		     NULL);
}


static void speed_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  XButtonEvent *ev;


  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    set_speed(sp, sp->last_speed_control);
  else set_speed(sp, 1.0);

#if XEN_HAVE_RATIOS
  if (sp->speed_control_style == SPEED_CONTROL_AS_RATIO)
    snd_rationalize(sp->speed_control, &(sp->speed_control_numerator), &(sp->speed_control_denominator));
#endif
}


static void speed_label_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_info *sp = (snd_info *)context;
  switch (sp->speed_control_style)
    {
    default:
    case SPEED_CONTROL_AS_FLOAT:    sp->speed_control_style = SPEED_CONTROL_AS_RATIO;    break;
    case SPEED_CONTROL_AS_RATIO:    sp->speed_control_style = SPEED_CONTROL_AS_SEMITONE; break;
    case SPEED_CONTROL_AS_SEMITONE: sp->speed_control_style = SPEED_CONTROL_AS_FLOAT;    break;
    }
  set_speed(sp, sp->speed_control);  /* remake label */
}


static void speed_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_info *sp = (snd_info *)context;
  scroll_to_speed(sp, ((XmScrollBarCallbackStruct *)info)->value);

#if XEN_HAVE_RATIOS
  if (sp->speed_control_style == SPEED_CONTROL_AS_RATIO)
    snd_rationalize(sp->speed_control, &(sp->speed_control_numerator), &(sp->speed_control_denominator));
#endif
}


static void speed_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  scroll_to_speed(sp, cb->value);

#if XEN_HAVE_RATIOS
  if (sp->speed_control_style == SPEED_CONTROL_AS_RATIO)
    snd_rationalize(sp->speed_control, &(sp->speed_control_numerator), &(sp->speed_control_denominator));
#endif
  sp->last_speed_control = sp->saved_speed_control;
  sp->saved_speed_control = sp->speed_control;
}


void toggle_direction_arrow(snd_info *sp, bool state)
{
  if (!HAS_WIDGETS(sp))
    sp->speed_control_direction = ((state) ? -1 : 1);
  else XmToggleButtonSetState(SPEED_ARROW(sp), (Boolean)state, true);
}


/* ---------------- EXPAND-CONTROL ---------------- */

static int expand_to_scroll(mus_float_t minval, mus_float_t val, mus_float_t maxval)
{
  if (val <= minval) return(0);
  if (val >= maxval) return((int)(0.9 * SCROLLBAR_MAX));
  return(snd_round(0.9 * SCROLLBAR_MAX * ((log(val) - log(minval)) / (log(maxval) - log(minval)))));
}


static int scroll_to_expand(snd_info *sp, int val)
{
  char expand_number_buffer[6];

  if (val <= 0) 
    sp->expand_control = sp->expand_control_min;
  else
    {
      if (val >= (0.9 * SCROLLBAR_MAX)) 
	sp->expand_control = sp->expand_control_max;
      else sp->expand_control = exp((val * (log(sp->expand_control_max) - log(sp->expand_control_min)) / (0.9 * SCROLLBAR_MAX)) + log(sp->expand_control_min));
    }

  if (sp->playing) dac_set_expand(sp, sp->expand_control);
  snprintf(expand_number_buffer, 6, "%.3f", sp->expand_control);
  set_label(EXPAND_LABEL(sp), expand_number_buffer);
  return(val);
}


void set_expand(snd_info *sp, mus_float_t val)
{
  if (!HAS_WIDGETS(sp))
    sp->expand_control = val;
  else XtVaSetValues(EXPAND_SCROLLBAR(sp),
		     XmNvalue,
		     scroll_to_expand(sp, expand_to_scroll(sp->expand_control_min, val, sp->expand_control_max)),
		     NULL);
}


static void expand_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  XButtonEvent *ev;
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask))
    set_expand(sp, sp->last_expand_control);
  else set_expand(sp, 1.0);
}


static void expand_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  scroll_to_expand((snd_info *)context, ((XmScrollBarCallbackStruct *)info)->value);
}


static void expand_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  scroll_to_expand(sp, cb->value);
  sp->last_expand_control = sp->saved_expand_control;
  sp->saved_expand_control = sp->expand_control;
}


static void expand_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info; 
  snd_info *sp = (snd_info *)context;


  sp->expand_control_p = cb->set;
  XmChangeColor(EXPAND_SCROLLBAR(sp), (Pixel)((sp->expand_control_p) ? (ss->position_color) : (ss->basic_color)));
}


void toggle_expand_button(snd_info *sp, bool state)
{
  if (!HAS_WIDGETS(sp))
    sp->expand_control_p = state;
  else XmToggleButtonSetState(EXPAND_RIGHT_BUTTON(sp), (Boolean)state, true);
}


/* ---------------- CONTRAST-CONTROL ---------------- */

static int contrast_to_scroll(mus_float_t minval, mus_float_t val, mus_float_t maxval)
{
  if (val <= minval) return(0);
  if (val >= maxval) return((int)(0.9 * SCROLLBAR_MAX));
  return(snd_round((val - minval) / (maxval - minval) * 0.9 * SCROLLBAR_MAX));
}


static int scroll_to_contrast(snd_info *sp, int val)
{
  char contrast_number_buffer[6];
  sp->contrast_control = sp->contrast_control_min + val * (sp->contrast_control_max - sp->contrast_control_min) / (0.9 * SCROLLBAR_MAX);
  snprintf(contrast_number_buffer, 6, "%.3f", sp->contrast_control);
  set_label(CONTRAST_LABEL(sp), contrast_number_buffer);
  return(val);
}


void set_contrast(snd_info *sp, mus_float_t val)
{
  if (!HAS_WIDGETS(sp))
    sp->contrast_control = val;
  else XtVaSetValues(CONTRAST_SCROLLBAR(sp),
		     XmNvalue,
		     scroll_to_contrast(sp, contrast_to_scroll(sp->contrast_control_min, val, sp->contrast_control_max)),
		     NULL);
}


static void contrast_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  XButtonEvent *ev;
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask))
    set_contrast(sp, sp->last_contrast_control);
  else set_contrast(sp, 0.0);
}


static void contrast_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  scroll_to_contrast((snd_info *)context, ((XmScrollBarCallbackStruct *)info)->value);
}


static void contrast_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  scroll_to_contrast(sp, cb->value);
  sp->last_contrast_control = sp->saved_contrast_control;
  sp->saved_contrast_control = sp->contrast_control;
}


static void contrast_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  sp->contrast_control_p = cb->set;
  XmChangeColor(CONTRAST_SCROLLBAR(sp), (Pixel)((sp->contrast_control_p) ? (ss->position_color) : (ss->basic_color)));
}


void toggle_contrast_button(snd_info *sp, bool state)
{
  if (!HAS_WIDGETS(sp))
    sp->contrast_control_p = state;
  else XmToggleButtonSetState(CONTRAST_RIGHT_BUTTON(sp), (Boolean)state, true);
}


/* ---------------- REVERB-CONTROL-SCALE ---------------- */

static int revscl_to_scroll(mus_float_t minval, mus_float_t val, mus_float_t maxval)
{
  if (val <= minval) return(0);
  if (val >= maxval) return((int)(0.9 * SCROLLBAR_MAX));
  return(snd_round(0.9 * SCROLLBAR_MAX * (pow(val, 0.333) - pow(minval, 0.333)) / (pow(maxval, 0.333) - pow(minval, 0.333))));
}


static mus_float_t cube(mus_float_t a) {return(a*a*a);}


static int scroll_to_revscl(snd_info *sp, int val)
{
  char revscl_number_buffer[7];

  if (val <= 0) 
    sp->reverb_control_scale = sp->reverb_control_scale_min;
  else
    {
      if (val >= (0.9 * SCROLLBAR_MAX)) 
	sp->reverb_control_scale = sp->reverb_control_scale_max;
      else sp->reverb_control_scale = cube((val * (pow(sp->reverb_control_scale_max, 0.333) - pow(sp->reverb_control_scale_min, 0.333)) / 
					    (0.9 * SCROLLBAR_MAX)) + 
					   pow(sp->reverb_control_scale_min, 0.333));
    }

  snprintf(revscl_number_buffer, 7, "%.4f", sp->reverb_control_scale);
  set_label(REVSCL_LABEL(sp), revscl_number_buffer);
  return(val);
}


void set_revscl(snd_info *sp, mus_float_t val)
{
  if (!HAS_WIDGETS(sp))
    sp->reverb_control_scale = val;
  else XtVaSetValues(REVSCL_SCROLLBAR(sp),
		     XmNvalue,
		     scroll_to_revscl(sp, revscl_to_scroll(sp->reverb_control_scale_min, val, sp->reverb_control_scale_max)),
		     NULL);
}


static void revscl_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  XButtonEvent *ev;
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask))
    set_revscl(sp, sp->last_reverb_control_scale);
  else set_revscl(sp, 0.0);
}


static void revscl_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  scroll_to_revscl((snd_info *)context, ((XmScrollBarCallbackStruct *)info)->value);
}


static void revscl_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  scroll_to_revscl(sp, cb->value);
  sp->last_reverb_control_scale = sp->saved_reverb_control_scale;
  sp->saved_reverb_control_scale = sp->reverb_control_scale;
}


/* ---------------- REVERB-CONTROL-LENGTH ---------------- */

static int revlen_to_scroll(mus_float_t minval, mus_float_t val, mus_float_t maxval)
{
  if (val <= minval) return(0);
  if (val >= maxval) return((int)(0.9 * SCROLLBAR_MAX));
  return(snd_round((val - minval) / (maxval - minval) * 0.9 * SCROLLBAR_MAX));
}


static int scroll_to_revlen(snd_info *sp, int val)
{
  char revlen_number_buffer[5];

  sp->reverb_control_length = sp->reverb_control_length_min + 
    (sp->reverb_control_length_max - sp->reverb_control_length_min) * (mus_float_t)val / (0.9 * SCROLLBAR_MAX);
  snprintf(revlen_number_buffer, 5, "%.2f", sp->reverb_control_length);
  set_label(REVLEN_LABEL(sp), revlen_number_buffer);
  return(val);
}


void set_revlen(snd_info *sp, mus_float_t val)
{
  if (!HAS_WIDGETS(sp))
    sp->reverb_control_length = val;
  else XtVaSetValues(REVLEN_SCROLLBAR(sp),
		     XmNvalue,
		     scroll_to_revlen(sp, revlen_to_scroll(sp->reverb_control_length_min, val, sp->reverb_control_length_max)),
		     NULL);
}


static void revlen_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  XButtonEvent *ev;
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    set_revlen(sp, sp->last_reverb_control_length);
  else set_revlen(sp, 1.0);
}


static void revlen_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  scroll_to_revlen((snd_info *)context, ((XmScrollBarCallbackStruct *)info)->value);
}


static void revlen_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  scroll_to_revlen(sp, cb->value);
  sp->last_reverb_control_length = sp->saved_reverb_control_length;
  sp->saved_reverb_control_length = sp->reverb_control_length;
}


static void reverb_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  sp->reverb_control_p = cb->set;
  XmChangeColor(REVLEN_SCROLLBAR(sp), (Pixel)((sp->reverb_control_p) ? (ss->position_color) : (ss->basic_color)));
  XmChangeColor(REVSCL_SCROLLBAR(sp), (Pixel)((sp->reverb_control_p) ? (ss->position_color) : (ss->basic_color)));
}


void toggle_reverb_button(snd_info *sp, bool state)
{
  if (!HAS_WIDGETS(sp))
    sp->reverb_control_p = state;
  else XmToggleButtonSetState(REVERB_BUTTON(sp), (Boolean)state, true);
}


/* ---------------- FILTER_CONTROL ---------------- */

static void filter_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  sp->filter_control_p = cb->set;
}


void toggle_filter_button(snd_info *sp, bool state)
{
  if (!HAS_WIDGETS(sp))
    sp->filter_control_p = state;
  else XmToggleButtonSetState(FILTER_BUTTON(sp), (Boolean)state, true);
}


static void filter_textfield_deactivate(snd_info *sp)
{
  chan_info *active_chan;
  active_chan = any_selected_channel(sp);
  if (active_chan)
    goto_window(channel_graph(active_chan));
}


#define MIN_FILTER_GRAPH_HEIGHT 20

void display_filter_env(snd_info *sp)
{
  graphics_context *ax;
  int height, width;
  Widget drawer;
  env_editor *edp;

  if (!(snd_ok(sp))) return; /* autotest close + lagging X updates */

  edp = sp->flt;
  drawer = FILTER_GRAPH(sp);
  height = widget_height(drawer);
  if (height < MIN_FILTER_GRAPH_HEIGHT) return;

  width = widget_width(drawer);
  ax = (graphics_context *)calloc(1, sizeof(graphics_context));
  ax->gc = ss->fltenv_basic_gc;
  ax->wn = XtWindow(drawer);
  ax->dp = XtDisplay(drawer);

  XClearWindow(ax->dp, ax->wn);
  edp->in_dB = sp->filter_control_in_dB;
  edp->with_dots = true;

  if (sp->filter_control_in_hz)
    sp->filter_control_xmax = (mus_float_t)(SND_SRATE(sp) / 2);
  else sp->filter_control_xmax = 1.0;

  if (sp->filter_control_envelope == NULL) 
    sp->filter_control_envelope = default_env(sp->filter_control_xmax, 1.0);

  env_editor_display_env(edp, sp->filter_control_envelope, ax, "frequency response", 0, 0, width, height, NOT_PRINTING);
  if (edp->edited)
    {
      ax->gc = ss->fltenv_data_gc;
      display_frequency_response(sp->filter_control_envelope, 
				 (SOUND_ENV_EDITOR(sp))->axis, ax, 
				 sp->filter_control_order, 
				 sp->filter_control_in_dB);
    }
  free(ax);
}


void set_filter_text(snd_info *sp, const char *str)
{
  if (HAS_WIDGETS(sp))
    XmTextSetString(FILTER_COEFFS_TEXT(sp), (char *)str);
}

#ifdef __APPLE__
static int press_x, press_y;
#endif


static void filter_drawer_button_motion(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  snd_info *sp = (snd_info *)context;
  XMotionEvent *ev = (XMotionEvent *)event;
  env_editor *edp;
#ifdef __APPLE__
  if ((press_x == ev->x) && (press_y == ev->y)) return;
#endif
  edp = sp->flt;
  edp->in_dB = sp->filter_control_in_dB;
  env_editor_button_motion(edp, ev->x, ev->y, ev->time, sp->filter_control_envelope);
  display_filter_env(sp);
  sp->filter_control_changed = true;
}


static void filter_drawer_button_press(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  snd_info *sp = (snd_info *)context;
  XButtonEvent *ev = (XButtonEvent *)event;
  env_editor *edp;
  if (!(sp->filter_control_envelope)) return;
#ifdef __APPLE__
  press_x = ev->x;
  press_y = ev->y;
#endif
  edp = sp->flt;
  edp->in_dB = sp->filter_control_in_dB;
  if (env_editor_button_press(edp, ev->x, ev->y, ev->time, sp->filter_control_envelope))
    display_filter_env(sp);
}


static void filter_drawer_button_release(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  char *tmpstr = NULL;
  snd_info *sp = (snd_info *)context;
  env_editor_button_release(SOUND_ENV_EDITOR(sp), sp->filter_control_envelope);
  display_filter_env(sp);
  set_filter_text(sp, tmpstr = env_to_string(sp->filter_control_envelope));
  if (tmpstr) free(tmpstr);
  sp->filter_control_changed = true;
}


static void filter_drawer_resize(Widget w, XtPointer context, XtPointer info) 
{
  snd_info *sp = (snd_info *)context;
  display_filter_env(sp);
}


static void filter_dB_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  sp->filter_control_in_dB = (cb->set);
  display_filter_env(sp);
}


void set_filter_in_dB(snd_info *sp, bool val)
{
  sp->filter_control_in_dB = val;
  if (HAS_WIDGETS(sp))
    {
      XmToggleButtonSetState(FILTER_DB_BUTTON(sp), (Boolean)val, false);
      display_filter_env(sp);
    }
}


static void new_in_hz(snd_info *sp, bool val)
{
  sp->filter_control_in_hz = val;
  if (val)
    sp->filter_control_xmax = (mus_float_t)(SND_SRATE(sp) / 2);
  else sp->filter_control_xmax = 1.0;
  if (sp->filter_control_envelope) free_env(sp->filter_control_envelope);
  sp->filter_control_envelope = default_env(sp->filter_control_xmax, 1.0);
}


static void filter_hz_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  new_in_hz(sp, cb->set);
  display_filter_env(sp);
}


void set_filter_in_hz(snd_info *sp, bool val)
{
  new_in_hz(sp, val);
  if (HAS_WIDGETS(sp))
    {
      XmToggleButtonSetState(FILTER_HZ_BUTTON(sp), (Boolean)val, false);
      display_filter_env(sp);
    }
}


void set_filter_order(snd_info *sp, int order)
{
  if (order & 1) order++;
  if (order <= 0) order = 2;
  sp->filter_control_order = order;
  if (HAS_WIDGETS(sp))
    {
      widget_int_to_text(FILTER_ORDER_TEXT(sp), order);
      display_filter_env(sp);
    }
  sp->filter_control_changed = true;
}


static void filter_order_up_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_info *sp = (snd_info *)context;
  set_filter_order(sp, sp->filter_control_order + 2);
}


static void filter_order_down_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_info *sp = (snd_info *)context;
  if (sp->filter_control_order > 2)
    set_filter_order(sp, sp->filter_control_order - 2);
}


static void get_filter_order(snd_info *sp, char *str)
{
  int order;
  redirect_errors_to(errors_to_status_area, (void *)sp);
  order = string_to_int(str, 1, "filter order");
  redirect_errors_to(NULL, NULL);
  if (order & 1) order++;
  if (order <= 0) order = 2;
  sp->filter_control_order = order;
}


static void filter_activate_callback(Widget w, XtPointer context, XtPointer info)
{
  /* make an envelope out of the data */
  snd_info *sp = (snd_info *)context;
  char *str = NULL;

  str = XmTextGetString(w);
  if (sp->filter_control_envelope) sp->filter_control_envelope = free_env(sp->filter_control_envelope);
  redirect_errors_to(errors_to_status_area, (void *)sp);
  sp->filter_control_envelope = string_to_env((const char *)str);
  redirect_errors_to(NULL, NULL);
  if (str) XtFree(str);

  if (!(sp->filter_control_envelope)) /* maybe user cleared text field? */
    sp->filter_control_envelope = default_env(sp->filter_control_xmax, 1.0);

  str = XmTextGetString(FILTER_ORDER_TEXT(sp));
  if ((str) && (*str))
    {
      get_filter_order(sp, str);
      XtFree(str);
    }
  (SOUND_ENV_EDITOR(sp))->edited = true;
  display_filter_env(sp);
  filter_textfield_deactivate(sp);
  sp->filter_control_changed = true;
}


static void filter_order_activate_callback(Widget w, XtPointer context, XtPointer info)
{
  char *str;
  snd_info *sp = (snd_info *)context;
  str = XmTextGetString(w);
  if ((str) && (*str))
    {
      get_filter_order(sp, str);
      sp->filter_control_changed = true;
      display_filter_env(sp);
      XtFree(str);
    }
  filter_textfield_deactivate(sp);
}


void filter_env_changed(snd_info *sp, env *e)
{
  /* turn e back into a string for textfield widget */
  if (HAS_WIDGETS(sp))
    {
      char *tmpstr = NULL;
      XmTextSetString(FILTER_COEFFS_TEXT(sp), tmpstr = env_to_string(e));
      if (tmpstr) free(tmpstr);
      (SOUND_ENV_EDITOR(sp))->edited = true;
      display_filter_env(sp);
    }
  sp->filter_control_changed = true;
}


/* ---------------- PLAY BUTTON ---------------- */

void set_play_button(snd_info *sp, bool val)
{
#if WITH_AUDIO
  if (HAS_WIDGETS(sp))
    XmToggleButtonSetState(PLAY_BUTTON(sp), (Boolean)val, false);
  set_open_file_play_button(val);
#endif
}


#if WITH_AUDIO
static void play_button_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_info *sp = (snd_info *)context;
  chan_info *cp;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  XButtonEvent *ev;

  ev = (XButtonEvent *)(cb->event);

  if (sp->playing) 
    stop_playing_sound(sp, PLAY_BUTTON_UNSET);

  ss->tracking = ((with_tracking_cursor(ss)) ||
		  ((cb->set) && 
		   (ev->state & (snd_ControlMask | snd_MetaMask))));

  cp = any_selected_channel(sp);
  goto_graph(cp);
  if (cb->set) 
    {
      XtVaSetValues(w, XmNselectColor, (ss->tracking) ? ss->green : ss->selection_color, NULL);
      play_sound(sp, 0, NO_END_SPECIFIED);
    }
}
#endif


typedef struct {bool pausing; } pause_data;

#if WITH_AUDIO
static void set_play_button_pause(snd_info *sp, void *ptr)
{
  if ((sp->playing) && (HAS_WIDGETS(sp)))
    {
      pause_data *pd = (pause_data *)ptr;
      Widget w;
      w = PLAY_BUTTON(sp);
      if (pd->pausing)
	XtVaSetValues(w, XmNselectColor, ss->red, NULL);
      else XtVaSetValues(w, XmNselectColor, (ss->tracking) ? ss->green : ss->selection_color, NULL);
    }
}
#endif


void play_button_pause(bool pausing)
{
#if WITH_AUDIO
  pause_data *pd;
  pd = (pause_data *)calloc(1, sizeof(pause_data));
  pd->pausing = pausing;
  for_each_sound_with_void(set_play_button_pause, (void *)pd);
  free(pd);
#endif
}


void set_control_panel_play_button(snd_info *sp)
{
#if WITH_AUDIO
  if (HAS_WIDGETS(sp))
    {
      set_toggle_button(PLAY_BUTTON(sp), false, false, sp);
      XtVaSetValues(PLAY_BUTTON(sp), XmNselectColor, ss->selection_color, NULL);
    }
#endif
}


static void play_arrow_callback(Widget w, XtPointer context, XtPointer info)
{
#if WITH_AUDIO
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  bool dir;
  dir = (bool)(cb->set);
  if (dir) sp->speed_control_direction = -1; else sp->speed_control_direction = 1;
#endif
}


/* ---------------- SYNC BUTTON ---------------- */

static void set_sync_color(snd_info *sp)
{
  Widget syb;
  syb = SYNC_BUTTON(sp);
  switch (sp->sync)
    {
    case 1: case 0: XtVaSetValues(syb, XmNselectColor, ss->selection_color, NULL); break;
    case 2:         XtVaSetValues(syb, XmNselectColor, ss->green, NULL);               break;
    case 3:         XtVaSetValues(syb, XmNselectColor, ss->yellow, NULL);              break;
    case 4:         XtVaSetValues(syb, XmNselectColor, ss->red, NULL);                 break;
    default:        XtVaSetValues(syb, XmNselectColor, ss->black, NULL);               break;
    }
}


void syncb(snd_info *sp, int on)
{
  sp->sync = on;
  if (on > ss->sound_sync_max) ss->sound_sync_max = on;
  if (HAS_WIDGETS(sp))
    {
      set_sync_color(sp);
      XmToggleButtonSetState(SYNC_BUTTON(sp), (on != 0), false); /* need actual bool here, not a cast! */
    }
}


static void sync_button_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  XButtonEvent *ev;


  ev = (XButtonEvent *)(cb->event);
  if (cb->set)
    if (ev->state & snd_ControlMask) 
      if (ev->state & snd_MetaMask)
	if (ev->state & snd_ShiftMask)
	  sp->sync = 4;
	else sp->sync = 3;
      else sp->sync = 2;
    else sp->sync = 1;
  else sp->sync = 0;

  set_sync_color(sp);

  if (sp->sync != 0) 
    {
      chan_info *cp;
      if (sp->sync > ss->sound_sync_max) ss->sound_sync_max = sp->sync;
      cp = sp->lacp;
      if (cp == NULL) cp = any_selected_channel(sp);
      goto_graph(cp);
      if (cp->cursor_on) sync_cursors(cp, CURSOR(cp));
      apply_x_axis_change(cp);
    }
}


/* ---------------- UNITE BUTTON ---------------- */

static void unite_button_callback(Widget w, XtPointer context, XtPointer info)
{
  /* click if set unsets, click if unset->combine, ctrl-click->superimpose */
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  XButtonEvent *ev;
  channel_style_t val;
  ev = (XButtonEvent *)(cb->event);

  if (cb->set)
    {
      if (ev->state & (snd_ControlMask | snd_MetaMask)) 
	val = CHANNELS_SUPERIMPOSED;
      else val = CHANNELS_COMBINED;
    }
  else val = CHANNELS_SEPARATE;

  set_sound_channel_style(sp, val);
}


/* ---------------- CLOSE BUTTON ---------------- */

static void close_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_close_file((snd_info *)context);
}



/* apply is only safe if the DAC is currently inactive and remains safe only
 * if all other apply buttons are locked out (and play).
 */

/* relative panes needs to notice overall window resize, but there's no way to do so in Motif, as far as I can tell */

#if WITH_RELATIVE_PANES
/* It would be nice if we could set a paned window to keep its children relative
 *   amounts the same upon outside resize, but the Paned Window widget doesn't
 *   have a resize callback, and no obvious way to advise the resize mechanism.
 *   An attempt to get the same effect by wrapping w_pane in a drawingarea widget
 *   ran into other troubles (the thing is seriously confused about its size).
 *   You'd naively think the Actions "Start" and "Commit" could be used, since
 *   XtActions are said to be a list of XtActionProcs, but I can't find a way to add
 *   my action without deactivating the built-in action of the same name --
 *   XtAugmentTranslations ignores new actions if the old exists, XtOverride
 *   replaces the old, etc. (And XtActions involve far more complexity than
 *   anyone should have to endure).
 *
 * so... drop down into the sashes...(using undocumented stuff throughout this code)
 */
#include <Xm/SashP.h>


static void sash_lock_control_panel(snd_info *sp)
{
  if (showing_controls(sp))
    {
      /* lock to its current size */
      int hgt;
      hgt = control_panel_height(sp);
      XtVaSetValues(CONTROLS(sp),
		    XmNpaneMinimum, hgt,
		    XmNpaneMaximum, hgt,
		    NULL);
    }
}


static void sash_unlock_control_panel(snd_info *sp)
{
  if (showing_controls(sp))
    {
      XtVaSetValues(CONTROLS(sp),
		    XmNpaneMinimum, 1,
		    XmNpaneMaximum, LOTSA_PIXELS, 
		    NULL);
    }
}


static int outer_panes = 0;
static int *inner_panes = NULL;
static Dimension *outer_sizes = NULL;
static Dimension **inner_sizes = NULL;

static void watch_sash(Widget w, XtPointer closure, XtPointer info)
{
  SashCallData call_data = (SashCallData)info;
  /* call_data->params[0]: Commit, Move, Key, Start (as strings) */

  if ((call_data->params) && 
      (call_data->params[0]) && 
      (with_relative_panes(ss)) &&
      (sound_style(ss) == SOUNDS_VERTICAL))
    {
      int i, k;
      snd_info *sp;
      if (mus_strcmp(call_data->params[0], "Start"))
	{
	  int outer_ctr = 0;
	  for (i = 0; i < ss->max_sounds; i++)
	    {
	      sp = ss->sounds[i];
	      if ((sp) &&
		  (sp->inuse == SOUND_NORMAL) &&
		  (sp->nchans > 1) &&
		  (sp->channel_style == CHANNELS_SEPARATE))
		outer_panes++;
	    }

	  for_each_sound(sash_lock_control_panel);

	  if (outer_panes > 0)
	    {
	      inner_panes = (int *)calloc(outer_panes, sizeof(int));
	      outer_sizes = (Dimension *)calloc(outer_panes, sizeof(Dimension));
	      inner_sizes = (Dimension **)calloc(outer_panes, sizeof(Dimension *));
	      outer_ctr = 0;

	      for (i = 0; i < ss->max_sounds; i++)
		{
		  sp = ss->sounds[i];
		  if ((sp) &&
		      (sp->inuse == SOUND_NORMAL) &&
		      (sp->nchans > 1) &&
		      (sp->channel_style == CHANNELS_SEPARATE))
		    {
		      Widget child;
		      child = SND_PANE(sp);
		      inner_panes[outer_ctr] = sp->nchans;
		      inner_sizes[outer_ctr] = (Dimension *)calloc(sp->nchans, sizeof(Dimension));
		      XtVaGetValues(child, XmNheight, &(outer_sizes[outer_ctr]), NULL);

		      for (k = 0; k < sp->nchans; k++)
			XtVaGetValues(channel_main_pane(sp->chans[k]), XmNheight, &(inner_sizes[outer_ctr][k]), NULL);

		      outer_ctr++;
		      if (outer_ctr >= outer_panes) break;
		    }
		}
	    }
	}
      else
	{
	  if (mus_strcmp(call_data->params[0], "Commit")) /* release sash */
	    {
	      if (outer_panes > 0)
		{
		  int outer_ctr = 0;
		  Dimension cur_outer_size = 0;
		  
		  for (i = 0; i < ss->max_sounds; i++)
		    {
		      sp = ss->sounds[i];
		      if ((sp) &&
			  (sp->inuse == SOUND_NORMAL) &&
			  (sp->nchans > 1) &&
			  (sp->channel_style == CHANNELS_SEPARATE))
			{
			  XtVaGetValues(SND_PANE(sp), XmNheight, &cur_outer_size, NULL);
			  
			  if ((cur_outer_size > 40) && 
			      (abs(cur_outer_size - outer_sizes[outer_ctr]) > (sp->nchans * 2)))
			    {
			      /* this pane has multiple chans and its size has changed enough to matter */
			      Dimension total_inner = 0, diff;
			      int size;
			      float ratio;
			      
			      for (k = 0; k < sp->nchans; k++)
				total_inner += inner_sizes[outer_ctr][k];
			      diff = outer_sizes[outer_ctr] - total_inner; /* this is non-channel stuff */
			      
			      for (k = 0; k < sp->nchans; k++)
				XtUnmanageChild(channel_main_pane(sp->chans[k]));
			      
			      ratio = (float)(cur_outer_size - diff) / (float)(outer_sizes[outer_ctr] - diff);
			      if (ratio > 0.0)
				{
				  for (k = 0; k < sp->nchans; k++)
				    {
				      size = (int)(ratio * inner_sizes[outer_ctr][k]);
				      XtVaSetValues(channel_main_pane(sp->chans[k]), 
						    XmNpaneMinimum, size - 1,
						    XmNpaneMaximum, size + 1, 
						    NULL);
				    }
				  for (k = 0; k < sp->nchans; k++)
				    XtManageChild(channel_main_pane(sp->chans[k]));
				  for (k = 0; k < sp->nchans; k++)
				    XtVaSetValues(channel_main_pane(sp->chans[k]), 
						  XmNpaneMinimum, 1,
						  XmNpaneMaximum, LOTSA_PIXELS, 
						  NULL);
				}
			    }
			  outer_ctr++;
			}
		    }

		  for (i = 0; i < outer_panes; i++)
		    if (inner_sizes[i])
		      free(inner_sizes[i]);
		  free(inner_panes);
		  free(inner_sizes);
		  free(outer_sizes);
		  outer_panes = 0;
		}

	      for_each_sound(sash_unlock_control_panel);
	    }
	}
    }
}

static Widget *sashes = NULL;
static int sashes_size = 0;

static void remember_sash(Widget w)
{
  /* add callback only once (means remembering which widgets already have our callback */
  int loc = -1;
  if (sashes_size == 0)
    {
      sashes = (Widget *)calloc(16, sizeof(Widget));
      sashes_size = 16;
      loc = 0;
    }
  else
    {
      int i;
      for (i = 0; i < sashes_size; i++)
	{
	  if (sashes[i] == w) return;
	  if (sashes[i] == NULL)
	    {
	      loc = i;
	      break;
	    }
	}
      if (loc == -1)
	{
	  sashes = (Widget *)realloc(sashes, sashes_size * 2 * sizeof(Widget));
	  for (i = sashes_size; i < sashes_size * 2; i++) sashes[i] = NULL;
	  loc = sashes_size;
	  sashes_size *= 2;
	}
    }
  sashes[loc] = w;
  XtAddCallback(w, XmNcallback, watch_sash, NULL);
}


static void add_sash_watchers(Widget w)
{
  /* if relative panes, add sash watchers to the outer paned window sashes (SOUND_PANE(ss)) */
  unsigned int i;
  CompositeWidget cw = (CompositeWidget)w;
  for (i = 0; i < cw->composite.num_children; i++) /* only outermost sashes count here */
    {
      Widget child;
      child = cw->composite.children[i];
      if ((XtIsWidget(child)) && 
	  (XtIsManaged(child)) && 
	  (XtIsSubclass(child, xmSashWidgetClass)))
	remember_sash(child);
    }
}

#endif


static bool cant_write(char *name)
{
#ifndef _MSC_VER
  return((access(name, W_OK)) != 0);
#else
  return(false);
#endif
}


/* bitmaps for the playback direction arrow */

static unsigned char speed_r_bits1[] = {
   0x00, 0x04, 0x10, 0x08, 0x00, 0x10, 0x04, 0x20, 0x00, 0x40, 0xa5, 0xbf,
   0x00, 0x40, 0x04, 0x20, 0x00, 0x10, 0x10, 0x08, 0x00, 0x04, 0x00, 0x00};
static unsigned char speed_l_bits1[] = {
   0x20, 0x00, 0x10, 0x08, 0x08, 0x00, 0x04, 0x20, 0x02, 0x00, 0xfd, 0xa5,
   0x02, 0x00, 0x04, 0x20, 0x08, 0x00, 0x10, 0x08, 0x20, 0x00, 0x00, 0x00};

#define NUM_BOMBS 15

static Pixmap mini_lock = 0;
static Pixmap close_icon = 0;
static Pixmap blank_pixmap = 0;
static bool mini_lock_allocated = false;
static Pixmap bombs[NUM_BOMBS];
static Pixmap hourglasses[NUM_HOURGLASSES];
static Pixmap stop_sign = 0;

void show_lock(snd_info *sp)
{
  if (!HAS_WIDGETS(sp)) return;
  if (mini_lock)
    XtVaSetValues(LOCK_OR_BOMB(sp), XmNlabelPixmap, mini_lock, NULL);
}


void hide_lock(snd_info *sp)
{
  if (!HAS_WIDGETS(sp)) return;
  if (mini_lock)
    XtVaSetValues(LOCK_OR_BOMB(sp), XmNlabelPixmap, blank_pixmap, NULL);
  /* these Pixmaps can be null if the colormap is screwed up */
}


static void show_stop_sign(snd_info *sp)
{
  if (!HAS_WIDGETS(sp)) return;
  if (stop_sign)
    XtVaSetValues(STOP_ICON(sp), XmNlabelPixmap, stop_sign, NULL);
}


static void hide_stop_sign(snd_info *sp)
{
  if (!HAS_WIDGETS(sp)) return;
  if (blank_pixmap)
    XtVaSetValues(STOP_ICON(sp), XmNlabelPixmap, blank_pixmap, NULL);
}


static void show_bomb(snd_info *sp)
{
  if (!HAS_WIDGETS(sp)) return;
  if (sp->bomb_ctr >= NUM_BOMBS) 
    sp->bomb_ctr = 0;
  if (bombs[sp->bomb_ctr])
    XtVaSetValues(LOCK_OR_BOMB(sp), XmNlabelPixmap, bombs[sp->bomb_ctr], NULL);
  sp->bomb_ctr++; 
}


static void hide_bomb(snd_info *sp)
{
  if (!HAS_WIDGETS(sp)) return;
  XtVaSetValues(LOCK_OR_BOMB(sp), XmNlabelPixmap, blank_pixmap, NULL);
  sp->bomb_ctr = 0;
}


#define BOMB_TIME 200

static void tick_bomb(XtPointer context, XtIntervalId *id)
{
  snd_info *sp = (snd_info *)context;
  if (!HAS_WIDGETS(sp)) return;
  if ((sp->need_update) || (sp->file_unreadable))
    {
      show_bomb(sp);
      XtAppAddTimeOut(MAIN_APP(ss),
		      (unsigned long)BOMB_TIME,
		      (XtTimerCallbackProc)tick_bomb,
		      context);
    }
  else 
    {
      hide_bomb(sp);
      sp->bomb_in_progress = false;
    }
}


void start_bomb(snd_info *sp)
{
  if (!HAS_WIDGETS(sp)) return;
  sp->bomb_ctr = 0;
  if (!(sp->bomb_in_progress))
    {
      sp->bomb_in_progress = true;
      XtAppAddTimeOut(MAIN_APP(ss),
		      (unsigned long)BOMB_TIME,
		      (XtTimerCallbackProc)tick_bomb,
		      (void *)sp);
    }
}


void stop_bomb(snd_info *sp)
{
  if (!HAS_WIDGETS(sp)) return;
  hide_bomb(sp);
  sp->bomb_in_progress = false;
}


static char *bits_to_string(const char **icon)
{
  /* show first few lines */
  char *buf;
  buf = (char *)calloc(128, sizeof(char));
  snprintf(buf, 128, "\n%s\n%s\n%s...", icon[0], icon[1], icon[2]);
  return(buf);
}


static void allocate_icons(Widget w)
{ 
  Pixmap shape1, shape2, shape3, shape4; 
  XpmAttributes attributes; 
  XpmColorSymbol symbols[1];
  int scr, k, pixerr = XpmSuccess;
  Display *dp;
  Drawable wn;

  dp = XtDisplay(w);
  wn = XtWindow(w);
  scr = DefaultScreen(dp);
  XtVaGetValues(w, XmNdepth, &attributes.depth, XmNcolormap, &attributes.colormap, NULL);
  attributes.visual = DefaultVisual(dp, scr);
  symbols[0].name = (char *)"basiccolor";
  symbols[0].value = NULL;
  symbols[0].pixel = ss->basic_color;
  attributes.colorsymbols = symbols;
  attributes.numsymbols = 1;
  attributes.valuemask = XpmColorSymbols | XpmDepth | XpmColormap | XpmVisual;

  pixerr = XpmCreatePixmapFromData(dp, wn, (char **)mini_lock_bits(), &mini_lock, &shape1, &attributes);
  if (pixerr != XpmSuccess)
    snd_error("lock pixmap trouble: %s from %s\n", XpmGetErrorString(pixerr), bits_to_string(mini_lock_bits()));

  pixerr = XpmCreatePixmapFromData(dp, wn, (char **)blank_bits(), &blank_pixmap, &shape1, &attributes);
  if (pixerr != XpmSuccess) 
    snd_error("blank pixmap trouble: %s from %s\n", XpmGetErrorString(pixerr), bits_to_string(blank_bits()));

  pixerr = XpmCreatePixmapFromData(dp, wn, (char **)stop_sign_bits(), &stop_sign, &shape4, &attributes);
  if (pixerr != XpmSuccess) 
    snd_error("stop sign pixmap trouble: %s from %s\n", XpmGetErrorString(pixerr), bits_to_string(stop_sign_bits()));

  pixerr = XpmCreatePixmapFromData(dp, wn, (char **)close_icon_bits(), &close_icon, &shape1, &attributes);
  if (pixerr != XpmSuccess) 
    snd_error("stop sign pixmap trouble: %s from %s\n", XpmGetErrorString(pixerr), bits_to_string(close_icon_bits()));

  for (k = 0; k < NUM_BOMBS; k++)
    {
      pixerr = XpmCreatePixmapFromData(dp, wn, (char **)mini_bomb_bits(k), &(bombs[k]), &shape2, &attributes);
      if (pixerr != XpmSuccess) 
	{
	  snd_error("bomb pixmap trouble: %s from %s\n", XpmGetErrorString(pixerr), bits_to_string(mini_bomb_bits(k)));
	  break;
	}
      pixerr = XpmCreatePixmapFromData(dp, wn, (char **)mini_glass_bits(k), &(hourglasses[k]), &shape3, &attributes);
      if (pixerr != XpmSuccess) 
	{
	  snd_error("glass pixmap trouble: %s from %s\n", XpmGetErrorString(pixerr), bits_to_string(mini_glass_bits(k))); 
	  break;
	}
    }
  mini_lock_allocated = true;
}


static void change_pixmap_background(Widget w, Pixmap orig, Pixel old_color, Pixel new_color, int width, int height)
{
  XImage *before;
  Display *dp;
  Drawable wn;
  Visual *vis;
  XGCValues v;
  GC draw_gc;
  int depth, depth_bytes, x, y;
  char *data;

  dp = XtDisplay(w);
  wn = XtWindow(w);
  vis = DefaultVisual(dp, DefaultScreen(dp));
  XtVaGetValues(w, XmNdepth, &depth, NULL);
  depth_bytes = (depth >> 3);

  data = (char *)calloc((width + 1) * (height + 1) * depth_bytes, sizeof(char)); /* not calloc since X will free this */
  /* there's overflow in X here, apparently -- the +1's fix it according to valgrind */
  /*   perhaps this is supposed to be rounded up to byte boundaries? */

  before = XCreateImage(dp, vis, depth, XYPixmap, 0, data, width, height, 8, 0);
  XGetSubImage(dp, orig, 0, 0, width, height, AllPlanes, XYPixmap, before, 0, 0);

  v.background = new_color;
  draw_gc = XCreateGC(dp, wn, GCBackground, &v);
  XSetBackground(dp, draw_gc, new_color); 

  for (x = 0; x < width; x++) 
    for (y = 0; y < height; y++) 
      if (XGetPixel(before, x, y) == old_color)
	XPutPixel(before, x, y, new_color);

  XPutImage(dp, orig, draw_gc, before, 0, 0, 0, 0, width, height);
  XDestroyImage(before);  /* frees data as well */
  XFreeGC(dp, draw_gc);
}


void make_sound_icons_transparent_again(Pixel old_color, Pixel new_color)
{
  int i;
  if (!mini_lock_allocated) allocate_icons(MAIN_SHELL(ss));
  change_pixmap_background(MAIN_SHELL(ss), mini_lock, old_color, new_color, 16, 14);
  change_pixmap_background(MAIN_SHELL(ss), blank_pixmap, old_color, new_color, 16, 14);
  change_pixmap_background(MAIN_SHELL(ss), close_icon, old_color, new_color, 16, 14);
  /* change_pixmap_background(MAIN_SHELL(ss), stop_sign, old_color, new_color, 17, 17); */
  /* memory corruption here! */
  for (i = 0; i < NUM_BOMBS; i++)
    change_pixmap_background(MAIN_SHELL(ss), bombs[i], old_color, new_color, 16, 14);
  for (i = 0; i < NUM_HOURGLASSES; i++)
    change_pixmap_background(MAIN_SHELL(ss), hourglasses[i], old_color, new_color, 16, 14);
}


static Pixmap spd_r, spd_l;
static bool spd_ok = false;

static void close_sound_dialog(Widget w, XtPointer context, XtPointer info) 
{
  snd_info *sp = (snd_info *)context;
  if (sp) snd_close_file(sp);
}


static void manage_sync_button(snd_info *sp)
{
  XtManageChild(SYNC_BUTTON(sp));
}


static void attach_status_area(snd_info *sp)
{
  XtUnmanageChild(STATUS_AREA(sp));
  XtVaSetValues(STATUS_AREA(sp),
		XmNrightAttachment, XmATTACH_WIDGET,
		XmNrightWidget, (XtIsManaged(UNITE_BUTTON(sp))) ? UNITE_BUTTON(sp) : ((XtIsManaged(SYNC_BUTTON(sp))) ? SYNC_BUTTON(sp) : PLAY_BUTTON(sp)),
		NULL);
  XtManageChild(STATUS_AREA(sp));
}


snd_info *add_sound_window(char *filename, read_only_t read_only, file_info *hdr)
{  
  snd_info *sp = NULL, *osp;
  Widget *sw;
  XmString s1;
  int snd_slot, nchans = 1, i, k, n, old_chans;
  bool make_widgets;
  Arg args[32];
  char *old_name = NULL, *title;
  Dimension app_dy, screen_y, chan_min_y;
  Position app_y;
  /* these dimensions are used to try to get a reasonable channel graph size without falling off the screen bottom */
  Pixmap rb, lb;
  int depth;
  bool free_filename = false;
  Widget form;
  XtCallbackList n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12;
  Atom sound_delete;

  if (ss->translated_filename) 
    {
      old_name = filename;
      filename = ss->translated_filename;
      free_filename = true;
      ss->translated_filename = NULL;
    }

  nchans = hdr->chans;
  if (nchans <= 0) nchans = 1;
  XtVaGetValues(MAIN_SHELL(ss),
		XmNy, &app_y,
		XmNheight, &app_dy,
		NULL);
  screen_y = DisplayHeight(MAIN_DISPLAY(ss),
			   DefaultScreen(MAIN_DISPLAY(ss)));
  app_dy = (screen_y - app_y - app_dy - 20 * nchans);
  chan_min_y = (Dimension)(app_dy / (Dimension)nchans);
  if (chan_min_y > (Dimension)(ss->channel_min_height)) 
    chan_min_y = ss->channel_min_height; 
  else 
    if (chan_min_y < 5) 
      chan_min_y = 5;

  snd_slot = find_free_sound_slot(nchans); /* expands sound list if needed */
  if (ss->sounds[snd_slot]) /* we're trying to re-use an old, inactive set of widgets and whatnot */
    {
      osp = ss->sounds[snd_slot];
      old_chans = osp->allocated_chans;
    }
  else old_chans = 0;
  make_widgets = (ss->sounds[snd_slot] == NULL);
  ss->sounds[snd_slot] = make_snd_info(ss->sounds[snd_slot], filename, hdr, snd_slot, read_only);
  sp = ss->sounds[snd_slot];
  sp->inuse = SOUND_NORMAL;
  sp->bomb_ctr = 0;
  sp->write_date = file_write_date(filename); /* needed early in this process by the peak-env handlers */

  if (sp->snd_widgets == NULL) 
    sp->snd_widgets = (Widget *)calloc(NUM_SND_WIDGETS, sizeof(Widget));
  sw = sp->snd_widgets;

  if ((!make_widgets) && (old_chans < nchans))
    {
      for (i = old_chans; i < nchans; i++) 
	add_channel_window(sp, i, chan_min_y, 1, NULL, WITH_FW_BUTTONS, WITH_EVENTS);
    }

  if (make_widgets)
    {
      if (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS)
	{
	  title = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
	  snprintf(title, PRINT_BUFFER_SIZE, "%d: %s", snd_slot, sp->short_filename);
	  if (sp->dialog == NULL)
	    {
	      n = 0;
	      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
	      XtSetArg(args[n], XmNautoUnmanage, false); n++;
	      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
	      XtSetArg(args[n], XmNnoResize, false); n++;
	      XtSetArg(args[n], XmNtransient, false); n++;
	      sp->dialog = XtCreatePopupShell(title, xmDialogShellWidgetClass, MAIN_SHELL(ss), args, n);
	      /* using popup shell here gets around the problem that the shell passes resize requests to all its children
	       * -- as a popup, it's not considered a child, but that means we don't inherit things like popup menus from 
	       * the main shell.
	       */
	      sound_delete = XmInternAtom(XtDisplay(sp->dialog), (char *)"WM_DELETE_WINDOW", false);
	      XmAddWMProtocolCallback(sp->dialog, sound_delete, close_sound_dialog, (XtPointer)sp);
	    }
	  else XtVaSetValues(sp->dialog, XmNtitle, title, NULL);
	  free(title);
	  if (!XtIsManaged(sp->dialog)) XtManageChild(sp->dialog);
	}

      n = 0;      
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      n = attach_all_sides(args, n);
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNsashIndent, ss->channel_sash_indent); n++;
      XtSetArg(args[n], XmNpaneMaximum, LOTSA_PIXELS); n++; /* Xm/Paned.c initializes this to 1000! */
      if (ss->channel_sash_size != 0)
	{
	  XtSetArg(args[n], XmNsashHeight, ss->channel_sash_size); n++;
	  XtSetArg(args[n], XmNsashWidth, ss->channel_sash_size); n++;
	}

      /* if (mumble_style(ss) == CHANNELS_HORIZONTAL) {XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;} */
      /* this doesn't work yet because the control panel is screwed up when trying to display itself horizontally */
      /* Perhaps another layer of panes? */

      if (sound_style(ss) == SOUNDS_VERTICAL)
	{
	  if (ss->toolbar)
	    XtSetArg(args[n], XmNpositionIndex, snd_slot + 1);
	  else XtSetArg(args[n], XmNpositionIndex, snd_slot);
	  n++;
	}
      XtSetArg(args[n], XmNuserData, sp->index); n++;

      if (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS)
	SND_PANE(sp) = XtCreateManagedWidget("snd-pane", xmPanedWindowWidgetClass, sp->dialog, args, n);
      else 
	{
	  unsigned int i;
	  CompositeWidget cw = (CompositeWidget)SOUND_PANE(ss);
	  SND_PANE(sp) = XtCreateManagedWidget("snd-pane", xmPanedWindowWidgetClass, SOUND_PANE(ss), args, n);

	  /* try to make the division between sounds more obvious */
	  for (i = 0; i < cw->composite.num_children; i++)
	    {
	      Widget child;
	      child = cw->composite.children[i];
	      if (((XtIsWidget(child))|| (XmIsGadget(child))) &&
		  (XtIsManaged(child)) && 
		  ((XmIsSeparator(child)) || (XmIsSeparatorGadget(child))))
		XtVaSetValues(child, XmNseparatorType, XmDOUBLE_LINE, 
			      XmNbackground, ss->white,
			      NULL);
	    }
	}

      XtAddEventHandler(SND_PANE(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);
      /* if user clicks in controls, then starts typing, try to send key events to current active channel */
      /* all widgets in the control-pane that would otherwise intercept the key events get this event handler */

      for (i = 0; i < nchans; i++)
	add_channel_window(sp, i, chan_min_y, 0, NULL, WITH_FW_BUTTONS, WITH_EVENTS); 
      /* creates channel (horizontal) paned window widget as child of w_snd_pane(sp) == SND_PANE(sp) */
      

      /* -------- sound file name, status area, various buttons -------- */

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNpaneMinimum, 20); n++;
      XtSetArg(args[n], XmNpaneMaximum, 20); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      NAME_BOX(sp) = XtCreateManagedWidget("snd-name-form", xmFormWidgetClass, SND_PANE(sp), args, n);
      XtAddEventHandler(NAME_BOX(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);

      if (!mini_lock_allocated) 
	allocate_icons(NAME_BOX(sp));

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNlabelType, XmPIXMAP); n++;
      XtSetArg(args[n], XmNlabelPixmap, close_icon); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNwidth, 32); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      CLOSE_BUTTON(sp) = XtCreateManagedWidget("close-button", xmPushButtonWidgetClass, NAME_BOX(sp), args, n);
      XtAddCallback(CLOSE_BUTTON(sp), XmNactivateCallback, close_button_callback, (XtPointer)sp);

      n = 0;      
      s1 = XmStringCreateLocalized(shortname_indexed(sp));
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      /* XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++; */

      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, CLOSE_BUTTON(sp)); n++;

      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      NAME_LABEL(sp) = XtCreateManagedWidget("snd-name", xmPushButtonWidgetClass, NAME_BOX(sp), args, n);
      XtAddEventHandler(NAME_LABEL(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(NAME_LABEL(sp), XmNactivateCallback, name_click_callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;      
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, NAME_LABEL(sp)); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;

      if (blank_pixmap)
	{
	  /* if xpm failed (blank_pixmap == 0), this can cause X to kill Snd! */
	  XtSetArg(args[n], XmNlabelType, XmPIXMAP); n++;
	  XtSetArg(args[n], XmNlabelPixmap, blank_pixmap); n++;
	}

      LOCK_OR_BOMB(sp) = XtCreateManagedWidget("", xmLabelWidgetClass, NAME_BOX(sp), args, n);

      {
	int i;
	Widget left_widget;

	left_widget = LOCK_OR_BOMB(sp);
	sp->progress_widgets = (Widget *)calloc(sp->nchans, sizeof(Widget));
	sp->num_progress_widgets = sp->nchans;
	/* when an unused sound is reopened in snd-data.c, it's possible for its channel number
	 *   to be increased.  If we then try to draw the clock icon in the new channel, its
	 *   widget will be unallocated -> segfault, so to keep things simple, we check this number.
	 */

	for (i = 0; i < sp->nchans; i++)
	  {
	    n = 0;      
	    XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
	    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
	    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
	    XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
	    XtSetArg(args[n], XmNleftWidget, left_widget); n++;
	    XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;

	    if (blank_pixmap)
	      {
		/* if xpm failed (blank_pixmap == 0), this can cause X to kill Snd! */
		XtSetArg(args[n], XmNlabelType, XmPIXMAP); n++;
		XtSetArg(args[n], XmNlabelPixmap, blank_pixmap); n++;
	      }

	    sp->progress_widgets[i] = XtCreateManagedWidget("", xmLabelWidgetClass, NAME_BOX(sp), args, n);
	    left_widget = sp->progress_widgets[i];
	  }

	n = 0;      
	XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n], XmNleftWidget, left_widget); n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;

	if (blank_pixmap)
	  {
	    XtSetArg(args[n], XmNlabelType, XmPIXMAP); n++;
	    XtSetArg(args[n], XmNlabelPixmap, blank_pixmap); n++;
	  }

	XtSetArg(args[n], XmNshadowThickness, 0); n++;
	XtSetArg(args[n], XmNhighlightThickness, 0); n++;
	XtSetArg(args[n], XmNfillOnArm, false); n++;
	STOP_ICON(sp) = XtCreateManagedWidget("", xmPushButtonWidgetClass, NAME_BOX(sp), args, n);
	XtAddCallback(STOP_ICON(sp), XmNactivateCallback, stop_sign_click_callback, (XtPointer)sp);
      }

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, STOP_ICON(sp)); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNresizeWidth, true); n++;
      XtSetArg(args[n], XmNmarginHeight, 1); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNeditable, false); n++;
      XtSetArg(args[n], XmNcursorPositionVisible, false); n++;
      STATUS_AREA(sp) = XtCreateManagedWidget("snd-info", xmTextFieldWidgetClass, NAME_BOX(sp), args, n);

#if WITH_AUDIO
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
#ifdef TOGGLE_MARGIN
      XtSetArg(args[n], XmNmarginHeight, TOGGLE_MARGIN); n++;
      XtSetArg(args[n], XmNmarginTop, TOGGLE_MARGIN); n++;
#endif
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      /* in Motif 2.2 this sets up a tooltip:
	XtSetArg(args[n], XmNtoolTipString, XmStringCreateLocalized((char *)"play this sound")); n++;
      */
      XtSetArg(args[n], XmNselectColor, ss->selection_color); n++;
      PLAY_BUTTON(sp) = make_togglebutton_widget("play", NAME_BOX(sp), args, n);
      XtAddCallback(PLAY_BUTTON(sp), XmNvalueChangedCallback, play_button_callback, (XtPointer)sp);
#endif

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
#ifdef TOGGLE_MARGIN
      XtSetArg(args[n], XmNmarginHeight, TOGGLE_MARGIN); n++;
      XtSetArg(args[n], XmNmarginTop, TOGGLE_MARGIN); n++;
#endif
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
#if WITH_AUDIO
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, PLAY_BUTTON(sp)); n++;
#else
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
#endif
      XtSetArg(args[n], XmNselectColor, ss->selection_color); n++;
      SYNC_BUTTON(sp) = make_togglebutton_widget("sync", NAME_BOX(sp), args, n);
      XtAddEventHandler(SYNC_BUTTON(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(SYNC_BUTTON(sp), XmNvalueChangedCallback, sync_button_callback, (XtPointer)sp);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, SYNC_BUTTON(sp)); n++;
#ifdef TOGGLE_MARGIN
      XtSetArg(args[n], XmNmarginHeight, TOGGLE_MARGIN); n++;
      XtSetArg(args[n], XmNmarginTop, TOGGLE_MARGIN); n++;
#endif
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, SYNC_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNselectColor, ss->selection_color); n++;
      UNITE_BUTTON(sp) = make_togglebutton_widget("unite", NAME_BOX(sp), args, n);
      XtAddEventHandler(UNITE_BUTTON(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(UNITE_BUTTON(sp), XmNvalueChangedCallback, unite_button_callback, (XtPointer)sp);


      /* ---------------- control panel ---------------- */
      n = 0;      
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      CONTROLS(sp) = XtCreateManagedWidget("snd-amp", xmFormWidgetClass, SND_PANE(sp), args, n);
      XtAddEventHandler(CONTROLS(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);

      n = 0;      
      /* AMP */
      s1 = XmStringCreateLocalized((char *)"amp:");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      AMP_BUTTON(sp) = make_pushbutton_widget("amp-label", CONTROLS(sp), args, n);
      XtAddEventHandler(AMP_BUTTON(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(AMP_BUTTON(sp), XmNactivateCallback, amp_click_callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;
      s1 = XmStringCreateLocalized((char *)"1.0   ");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, AMP_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, AMP_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginRight, 3); n++;
      AMP_LABEL(sp) = XtCreateManagedWidget("amp-number", xmLabelWidgetClass, CONTROLS(sp), args, n);
      XmStringFree(s1);

      n = 0;      
      XtSetArg(args[n], XmNbackground, ss->position_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, AMP_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, AMP_LABEL(sp)); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNvalue, amp_to_scroll(sp->amp_control_min, 1.0, sp->amp_control_max)); n++;
      XtSetArg(args[n], XmNdragCallback, n1 = make_callback_list(amp_drag_callback, (XtPointer)sp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n2 = make_callback_list(amp_valuechanged_callback, (XtPointer)sp)); n++;
      AMP_SCROLLBAR(sp) = XtCreateManagedWidget("amp", xmScrollBarWidgetClass, CONTROLS(sp), args, n);
      XtAddEventHandler(AMP_SCROLLBAR(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);

      n = 0;
      /* SPEED */
      s1 = XmStringCreateLocalized((char *)"speed:");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, AMP_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++; 
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      SPEED_BUTTON(sp) = make_pushbutton_widget("speed-label", CONTROLS(sp), args, n);
      XtAddEventHandler(SPEED_BUTTON(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(SPEED_BUTTON(sp), XmNactivateCallback, speed_click_callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;
      s1 = initial_speed_label(sp->speed_control_style);
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, SPEED_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, SPEED_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++; 
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNmarginRight, 3); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      SPEED_LABEL(sp) = make_pushbutton_widget("speed-number", CONTROLS(sp), args, n);
      XtAddEventHandler(SPEED_LABEL(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(SPEED_LABEL(sp), XmNactivateCallback, speed_label_click_callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, SPEED_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNindicatorOn, false); n++;
      XtSetArg(args[n], XmNlabelType, XmPIXMAP); n++;
      XtSetArg(args[n], XmNmarginHeight, 0); n++;
      XtSetArg(args[n], XmNmarginWidth, 0); n++;
      XtSetArg(args[n], XmNmarginTop, 0); n++;
      XtSetArg(args[n], XmNtopOffset, 0); n++;
      SPEED_ARROW(sp) = make_togglebutton_widget("dir", CONTROLS(sp), args, n);
      form = SPEED_ARROW(sp);
      if (!spd_ok)
	{
	  rb = XCreateBitmapFromData(XtDisplay(form), RootWindowOfScreen(XtScreen(form)), (const char *)speed_r_bits1, 16, 12);
	  lb = XCreateBitmapFromData(XtDisplay(form), RootWindowOfScreen(XtScreen(form)), (const char *)speed_l_bits1, 16, 12);
	  XtVaGetValues(form, XmNdepth, &depth, NULL);
	  spd_r = XCreatePixmap(XtDisplay(form), RootWindowOfScreen(XtScreen(form)), 16, 12, depth);
	  spd_l = XCreatePixmap(XtDisplay(form), RootWindowOfScreen(XtScreen(form)), 16, 12, depth);
	  XCopyPlane(XtDisplay(form), rb, spd_r, ss->fltenv_basic_gc, 0, 0, 16, 12, 0, 0, 1);
	  XCopyPlane(XtDisplay(form), lb, spd_l, ss->fltenv_basic_gc, 0, 0, 16, 12, 0, 0, 1);
	  XFreePixmap(XtDisplay(form), rb);
	  XFreePixmap(XtDisplay(form), lb);
	  spd_ok = true;
	}
      XtVaSetValues(form, XmNselectPixmap, spd_l, XmNlabelPixmap, spd_r, NULL);
      XtAddEventHandler(SPEED_ARROW(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(SPEED_ARROW(sp), XmNvalueChangedCallback, play_arrow_callback, (XtPointer)sp);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->position_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, SPEED_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, SPEED_LABEL(sp)); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, SPEED_ARROW(sp)); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNvalue, speed_to_scroll(sp->speed_control_min, 1.0, sp->speed_control_max)); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNdragCallback, n3 = make_callback_list(speed_drag_callback, (XtPointer)sp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n4 = make_callback_list(speed_valuechanged_callback, (XtPointer)sp)); n++;
      SPEED_SCROLLBAR(sp) = XtCreateManagedWidget("speed-scroll", xmScrollBarWidgetClass, CONTROLS(sp), args, n);
      XtAddEventHandler(SPEED_SCROLLBAR(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);

      n = 0;
      /* EXPAND */
      s1 = XmStringCreateLocalized((char *)"expand:");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, SPEED_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      EXPAND_LEFT_BUTTON(sp) = make_pushbutton_widget("expand-label", CONTROLS(sp), args, n);
      XtAddEventHandler(EXPAND_LEFT_BUTTON(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(EXPAND_LEFT_BUTTON(sp), XmNactivateCallback, expand_click_callback, (XtPointer)sp);
      XmStringFree(s1);
      
      n = 0;
      s1 = XmStringCreateLocalized((char *)"1.0   ");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, EXPAND_LEFT_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, EXPAND_LEFT_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginRight, 3); n++;
      EXPAND_LABEL(sp) = XtCreateManagedWidget("expand-number", xmLabelWidgetClass, CONTROLS(sp), args, n);
      XmStringFree(s1);

      n = 0;
      s1 = XmStringCreateLocalized((char *)"");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, EXPAND_LEFT_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNmarginWidth, 0); n++;
      XtSetArg(args[n], XmNtopOffset, 1); n++;
      XtSetArg(args[n], XmNspacing, 0); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
      XtSetArg(args[n], XmNselectColor, ss->selection_color); n++;
      EXPAND_RIGHT_BUTTON(sp) = make_togglebutton_widget("expoff", CONTROLS(sp), args, n);
      XtAddEventHandler(EXPAND_RIGHT_BUTTON(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(EXPAND_RIGHT_BUTTON(sp), XmNvalueChangedCallback, expand_button_callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, EXPAND_LEFT_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, EXPAND_LABEL(sp)); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, EXPAND_RIGHT_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNvalue, expand_to_scroll(sp->expand_control_min, 1.0, sp->expand_control_max)); n++; 
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNdragCallback, n5 = make_callback_list(expand_drag_callback, (XtPointer)sp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n6 = make_callback_list(expand_valuechanged_callback, (XtPointer)sp)); n++;
      EXPAND_SCROLLBAR(sp) = XtCreateManagedWidget("expand-scroll", xmScrollBarWidgetClass, CONTROLS(sp), args, n);
      XtAddEventHandler(EXPAND_SCROLLBAR(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);


      /* CONTRAST */
      n = 0;
      s1 = XmStringCreateLocalized((char *)"contrast:");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, EXPAND_LEFT_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      CONTRAST_LEFT_BUTTON(sp) = make_pushbutton_widget("contrast-label", CONTROLS(sp), args, n);
      XtAddEventHandler(CONTRAST_LEFT_BUTTON(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(CONTRAST_LEFT_BUTTON(sp), XmNactivateCallback, contrast_click_callback, (XtPointer)sp);
      XmStringFree(s1);
      
      n = 0;
      s1 = XmStringCreateLocalized((char *)"1.0   ");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, CONTRAST_LEFT_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, CONTRAST_LEFT_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginRight, 3); n++;
      CONTRAST_LABEL(sp) = XtCreateManagedWidget("contrast-number", xmLabelWidgetClass, CONTROLS(sp), args, n);
      XmStringFree(s1);
      
      n = 0;
      s1 = XmStringCreateLocalized((char *)"");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, CONTRAST_LEFT_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNmarginWidth, 0); n++;
      XtSetArg(args[n], XmNtopOffset, 1); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNspacing, 0); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
      XtSetArg(args[n], XmNselectColor, ss->selection_color); n++;
      CONTRAST_RIGHT_BUTTON(sp) = make_togglebutton_widget("conoff", CONTROLS(sp), args, n);
      XtAddEventHandler(CONTRAST_RIGHT_BUTTON(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(CONTRAST_RIGHT_BUTTON(sp), XmNvalueChangedCallback, contrast_button_callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, CONTRAST_LEFT_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, CONTRAST_LABEL(sp)); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, CONTRAST_RIGHT_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNvalue, 0); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNdragCallback, n7 = make_callback_list(contrast_drag_callback, (XtPointer)sp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n8 = make_callback_list(contrast_valuechanged_callback, (XtPointer)sp)); n++;
      CONTRAST_SCROLLBAR(sp) = XtCreateManagedWidget("contrast-scroll", xmScrollBarWidgetClass, CONTROLS(sp), args, n);
      XtAddEventHandler(CONTRAST_SCROLLBAR(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);

      /* REVERB */
      /* REVSCL */
      n = 0;
      s1 = XmStringCreateLocalized((char *)"reverb:");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, CONTRAST_LEFT_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      REVSCL_BUTTON(sp) = make_pushbutton_widget("revscl-label", CONTROLS(sp), args, n);
      XtAddEventHandler(REVSCL_BUTTON(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(REVSCL_BUTTON(sp), XmNactivateCallback, revscl_click_callback, (XtPointer)sp);
      XmStringFree(s1);
      
      n = 0;
      s1 = XmStringCreateLocalized((char *)"0.0     ");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, REVSCL_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, REVSCL_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNmarginRight, 3); n++;
      REVSCL_LABEL(sp) = XtCreateManagedWidget("revscl-number", xmLabelWidgetClass, CONTROLS(sp), args, n);
      XmStringFree(s1);
      
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, REVSCL_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, REVSCL_LABEL(sp)); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 60); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNvalue, 0); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNdragCallback, n9 = make_callback_list(revscl_drag_callback, (XtPointer)sp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n10 = make_callback_list(revscl_valuechanged_callback, (XtPointer)sp)); n++;
      REVSCL_SCROLLBAR(sp) = XtCreateManagedWidget("revscl-scroll", xmScrollBarWidgetClass, CONTROLS(sp), args, n);
      XtAddEventHandler(REVSCL_SCROLLBAR(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);

      /* REVOFF */
      n = 0;
      s1 = XmStringCreateLocalized((char *)"");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, REVSCL_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, CONTRAST_RIGHT_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNmarginWidth, 0); n++;
      XtSetArg(args[n], XmNtopOffset, 1); n++;
      XtSetArg(args[n], XmNspacing, 0); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
      XtSetArg(args[n], XmNselectColor, ss->selection_color); n++;
      REVERB_BUTTON(sp) = make_togglebutton_widget("revoff", CONTROLS(sp), args, n);
      XtAddEventHandler(REVERB_BUTTON(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(REVERB_BUTTON(sp), XmNvalueChangedCallback, reverb_button_callback, (XtPointer)sp);
      XmStringFree(s1);


      /* REVLEN */
      n = 0;
      s1 = XmStringCreateLocalized((char *)"len:");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, REVSCL_SCROLLBAR(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 60); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      REVLEN_BUTTON(sp) = make_pushbutton_widget("revlen-label", CONTROLS(sp), args, n);
      XtAddEventHandler(REVLEN_BUTTON(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(REVLEN_BUTTON(sp), XmNactivateCallback, revlen_click_callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;
      s1 = XmStringCreateLocalized((char *)"1.0 ");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, REVLEN_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, REVLEN_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNmarginRight, 3); n++;
      REVLEN_LABEL(sp) = XtCreateManagedWidget("revlen-number", xmLabelWidgetClass, CONTROLS(sp), args, n);
      XmStringFree(s1);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, REVLEN_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, REVLEN_LABEL(sp)); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, REVERB_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNvalue, revlen_to_scroll(sp->reverb_control_length_min, 1.0, sp->reverb_control_length_max)); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNdragCallback, n11 = make_callback_list(revlen_drag_callback, (XtPointer)sp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n12 = make_callback_list(revlen_valuechanged_callback, (XtPointer)sp)); n++;
      REVLEN_SCROLLBAR(sp) = XtCreateManagedWidget("revlen-scroll", xmScrollBarWidgetClass, CONTROLS(sp), args, n);
      XtAddEventHandler(REVLEN_SCROLLBAR(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);


      /* FILTER */
      n = 0;
      s1 = XmStringCreateLocalized((char *)"filter:");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, REVSCL_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      FILTER_LABEL(sp) = XtCreateManagedWidget("filter-label", xmLabelWidgetClass, CONTROLS(sp), args, n);
      XmStringFree(s1);

      /* filter order */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNresizeWidth, false); n++;
      XtSetArg(args[n], XmNcolumns, 3); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, FILTER_LABEL(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, FILTER_LABEL(sp)); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      FILTER_ORDER_TEXT(sp) = make_textfield_widget("filter-order", CONTROLS(sp), args, n, ACTIVATABLE, NO_COMPLETER);
      XmTextSetString(FILTER_ORDER_TEXT(sp), (char *)" 20");
      XtAddCallback(FILTER_ORDER_TEXT(sp), XmNactivateCallback, filter_order_activate_callback, (XtPointer)sp);

      #define ARROW_SIZE 12

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, FILTER_ORDER_TEXT(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, FILTER_ORDER_TEXT(sp)); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNheight, ARROW_SIZE); n++;
      XtSetArg(args[n], XmNwidth, ARROW_SIZE); n++;
      XtSetArg(args[n], XmNborderWidth, 0); n++;
      XtSetArg(args[n], XmNmarginWidth, 0); n++;
      XtSetArg(args[n], XmNarmColor, ss->selection_color); n++;
      FILTER_ORDER_DOWN(sp) = make_pushbutton_widget("", CONTROLS(sp), args, n);
      XtAddEventHandler(FILTER_ORDER_DOWN(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(FILTER_ORDER_DOWN(sp), XmNactivateCallback, filter_order_down_callback, (XtPointer)sp);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, FILTER_ORDER_DOWN(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, FILTER_ORDER_TEXT(sp)); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNheight, ARROW_SIZE); n++;
      XtSetArg(args[n], XmNwidth, ARROW_SIZE); n++;
      XtSetArg(args[n], XmNborderWidth, 0); n++;
      XtSetArg(args[n], XmNmarginWidth, 0); n++;
      XtSetArg(args[n], XmNarmColor, ss->selection_color); n++;
      FILTER_ORDER_UP(sp) = make_pushbutton_widget("", CONTROLS(sp), args, n);
      XtAddEventHandler(FILTER_ORDER_UP(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(FILTER_ORDER_UP(sp), XmNactivateCallback, filter_order_up_callback, (XtPointer)sp);

      n = 0;
      s1 = XmStringCreateLocalized((char *)"");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, REVERB_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNmarginWidth, 0); n++;
      XtSetArg(args[n], XmNtopOffset, 2); n++;
      XtSetArg(args[n], XmNspacing, 0); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++; 
      if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
      XtSetArg(args[n], XmNselectColor, ss->selection_color); n++;
      FILTER_BUTTON(sp) = make_togglebutton_widget("fltoff", CONTROLS(sp), args, n);
      XtAddEventHandler(FILTER_BUTTON(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(FILTER_BUTTON(sp), XmNvalueChangedCallback, filter_button_callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;
      s1 = XmStringCreateLocalized((char *)"hz");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, FILTER_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, FILTER_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++; 
      XtSetArg(args[n], XmNvalue, sp->filter_control_in_hz); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
      XtSetArg(args[n], XmNselectColor, ss->selection_color); n++;
      FILTER_HZ_BUTTON(sp) = make_togglebutton_widget("flthz", CONTROLS(sp), args, n);
      XtAddEventHandler(FILTER_HZ_BUTTON(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(FILTER_HZ_BUTTON(sp), XmNvalueChangedCallback, filter_hz_callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;
      s1 = XmStringCreateLocalized((char *)"dB");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, FILTER_HZ_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, FILTER_HZ_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++; 
      XtSetArg(args[n], XmNvalue, sp->filter_control_in_dB); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
      XtSetArg(args[n], XmNselectColor, ss->selection_color); n++;
      FILTER_DB_BUTTON(sp) = make_togglebutton_widget("fltdB", CONTROLS(sp), args, n);
      XtAddEventHandler(FILTER_DB_BUTTON(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(FILTER_DB_BUTTON(sp), XmNvalueChangedCallback, filter_dB_callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, FILTER_ORDER_DOWN(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, FILTER_ORDER_DOWN(sp)); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, FILTER_DB_BUTTON(sp)); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      FILTER_COEFFS_TEXT(sp) = make_textfield_widget("filter-text", CONTROLS(sp), args, n, ACTIVATABLE, add_completer_func(filename_completer, NULL));
      XtAddCallback(FILTER_COEFFS_TEXT(sp), XmNactivateCallback, filter_activate_callback, (XtPointer)sp);

      /* FILTER GRAPH */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, FILTER_COEFFS_TEXT(sp)); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 4); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 98); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 4); n++;
      FILTER_FRAME(sp) = XtCreateManagedWidget("filter-frame", xmFrameWidgetClass, CONTROLS(sp), args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      n = attach_all_sides(args, n);
      XtSetArg(args[n], XmNallowResize, true); n++;
      FILTER_GRAPH(sp) = XtCreateManagedWidget("filter-window", xmDrawingAreaWidgetClass, FILTER_FRAME(sp), args, n);
      XtAddCallback(FILTER_GRAPH(sp), XmNresizeCallback, filter_drawer_resize, (XtPointer)sp);
      XtAddCallback(FILTER_GRAPH(sp), XmNexposeCallback, filter_drawer_resize, (XtPointer)sp);

      sp->flt = new_env_editor();

      XtAddEventHandler(FILTER_GRAPH(sp), ButtonPressMask, false, filter_drawer_button_press, sp);
      XtAddEventHandler(FILTER_GRAPH(sp), ButtonMotionMask, false, filter_drawer_button_motion, sp);
      XtAddEventHandler(FILTER_GRAPH(sp), ButtonReleaseMask, false, filter_drawer_button_release, sp);
      XtAddEventHandler(FILTER_GRAPH(sp), KeyPressMask, false, graph_key_press, (XtPointer)sp);

      free(n1); free(n2); free(n3); free(n4); free(n5); free(n6);
      free(n7); free(n8); free(n9); free(n10); free(n11); free(n12);
      /* end if control-panel */
      if (sound_style(ss) == SOUNDS_IN_NOTEBOOK)
	{
	  char *name;
	  name = just_filename(sp->short_filename); /* copies */
	  if (strlen(name) > 8) name[8] = '\0';
	  n = 0;
	  XtSetArg(args[n], XmNbackground, ss->graph_color); n++;
	  XtSetArg(args[n], XmNnotebookChildType, XmMAJOR_TAB); n++;
	  XtSetArg(args[n], XmNuserData, sp->index); n++;
	  sp->tab = XtCreateManagedWidget(name, xmPushButtonWidgetClass, SOUND_PANE(ss), args, n);
	  free(name);
	}


      if (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS)
	run_new_widget_hook(SND_PANE(sp));
      else run_new_widget_hook(sp->dialog);

#if WITH_RELATIVE_PANES
      if (sound_style(ss) == SOUNDS_VERTICAL)
	add_sash_watchers(SOUND_PANE(ss)); /* add in any case since we might later change the sense of with_relative_panes */
#endif

    } /* new sound ss */
  else
    { /* re-manage currently inactive chan */
      if (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS)
	{
	  title = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
	  snprintf(title, PRINT_BUFFER_SIZE, "%d: %s", snd_slot, sp->short_filename);
	  XtVaSetValues(sp->dialog, XmNtitle, title, NULL);
	  free(title);
	  if (!XtIsManaged(sp->dialog)) XtManageChild(sp->dialog);
	}

      for (i = 0; i < NUM_SND_WIDGETS - 1; i++)
	if ((sw[i]) && 
	    (!XtIsManaged(sw[i])) &&
	    (in_show_controls(ss) || (i != W_amp_form)))
	  XtManageChild(sw[i]);

      for (k = 0; k < nchans; k++) 
	add_channel_window(sp, k, chan_min_y, 0, NULL, WITH_FW_BUTTONS, WITH_EVENTS);

      set_button_label(NAME_LABEL(sp), shortname_indexed(sp));
      XtVaSetValues(SND_PANE(sp), XmNuserData, sp->index, NULL);

      if (sound_style(ss) == SOUNDS_IN_NOTEBOOK)
	{
	  char *name;
	  name = just_filename(sp->short_filename);
	  set_label(sp->tab, name);
	  free(name);
	}
    }

  map_over_children(SOUND_PANE(ss), color_sashes);
  if (!(in_show_controls(ss)))
    hide_controls(sp);
  else show_controls(sp);

  if (sp->nchans == 1) 
    {
      XmToggleButtonSetState(UNITE_BUTTON(sp), false, false);
      XtUnmanageChild(UNITE_BUTTON(sp));

      if (ss->active_sounds == 0) /* ticked at the end in snd-file.c */
	{
	  XmToggleButtonSetState(SYNC_BUTTON(sp), false, false);
	  XtUnmanageChild(SYNC_BUTTON(sp));
	}
      else
	{
	  for_each_sound(manage_sync_button); 
	}
    }
  else
    {
      for_each_sound(manage_sync_button); 
    }
  attach_status_area(sp);

  add_sound_data(filename, sp, WITH_GRAPH);

  if (cant_write(sp->filename)) 
    sp->file_read_only = FILE_READ_ONLY;
  if ((sp->file_read_only == FILE_READ_ONLY) || 
      (sp->user_read_only == FILE_READ_ONLY)) 
    show_lock(sp);
  else hide_lock(sp);

  if (old_name)
    status_report(sp, "(translated %s)", old_name);

  if (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS)
    {
      reset_controls(sp);
    }
  else 
    {
      XtVaSetValues(sp->dialog,
		    XmNwidth, 100,
		    XmNheight, 100,
		    NULL);
      /* this is not redundant -- apparently they're trying to ignore size resets to the "current" */
      /* value, but forgot that unmanage/remanage does not return to the previous size */
      XtVaSetValues(sp->dialog,
		    XmNwidth, (Dimension)(widget_width(MAIN_SHELL(ss))),
		    XmNheight, (Dimension)(chan_min_y * nchans), /* bugfix thanks to Paul @pobox */
		    NULL);
    }

  after_open(sp);
  if (free_filename) free(filename);
  return(sp);
}


void snd_info_cleanup(snd_info *sp)
{
  if (HAS_WIDGETS(sp))
    {
      clear_status_area(sp);
      if (SYNC_BUTTON(sp))
	{
	  XtVaSetValues(SYNC_BUTTON(sp), XmNset, false, NULL);
	  XtVaSetValues(EXPAND_RIGHT_BUTTON(sp), XmNset, false, NULL);
	  XtVaSetValues(CONTRAST_RIGHT_BUTTON(sp), XmNset, false, NULL);
	  XtVaSetValues(SPEED_ARROW(sp), XmNset, false, NULL);
	  XtVaSetValues(FILTER_BUTTON(sp), XmNset, false, NULL);
	  XtVaSetValues(REVERB_BUTTON(sp), XmNset, false, NULL);
	  XmToggleButtonSetState(UNITE_BUTTON(sp), false, false);
	  sp->channel_style = CHANNELS_SEPARATE;
	  if (sound_style(ss) == SOUNDS_IN_NOTEBOOK)
	    {
	      set_label(sp->tab, "none");
	      XmChangeColor(sp->tab, ss->graph_color);
	    }
	  XtUnmanageChild(SND_PANE(sp));
	}
      if ((sp->dialog) && 
	  (XtIsManaged(sp->dialog))) 
	XtUnmanageChild(sp->dialog);
    }
}


static XEN reflect_file_close_in_sync(XEN hook_or_reason)
{
  int reason;
#if HAVE_SCHEME
  reason = XEN_TO_C_INT(s7_environment_ref(s7, hook_or_reason, s7_make_symbol(s7, "reason")));
#else
  reason = XEN_TO_C_INT(hook_or_reason);
#endif
  if ((reason == FILE_CLOSED) && /* snd-file.c */
      (ss->active_sounds == 1))
    {
      snd_info *sp;
      sp = any_selected_sound();
      if ((sp) && (sp->nchans == 1))
	{
	  XtUnmanageChild(SYNC_BUTTON(sp));
	  attach_status_area(sp);
	}
    }
  return(XEN_FALSE);
}

XEN_NARGIFY_1(reflect_file_close_in_sync_w, reflect_file_close_in_sync)


void set_sound_pane_file_label(snd_info *sp, const char *str)
{
  if (!(mus_strcmp(sp->name_string, str)))
    {
      if (sp->name_string) free(sp->name_string);
      sp->name_string = mus_strdup(str);
      set_button_label(SND_NAME(sp), str); /* this causes an expose event, so it's worth minimizing */
    }
}


void color_filter_waveform(Pixel color)
{
  int i;
  XSetForeground(MAIN_DISPLAY(ss), ss->fltenv_data_gc, color);
  ss->filter_control_waveform_color = color;
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	display_filter_env(sp);
    }
}


void show_controls(snd_info *sp)
{
  Dimension hgt;
  XtVaGetValues(FILTER_LABEL(sp),
		XmNy, &hgt,
		NULL);
  if (XtIsManaged(CONTROLS(sp)))
    XtUnmanageChild(CONTROLS(sp));
  XtVaSetValues(CONTROLS(sp),
		XmNpaneMinimum, hgt,
		XmNpaneMaximum, hgt,
		NULL);
  XtManageChild(CONTROLS(sp));
  XtVaSetValues(CONTROLS(sp),
		XmNpaneMinimum, 1,
		XmNpaneMaximum, LOTSA_PIXELS,
		NULL);
}


void hide_controls(snd_info *sp)
{
  XtUnmanageChild(CONTROLS(sp));
}


bool showing_controls(snd_info *sp)
{
  return((bool)(XtIsManaged(CONTROLS(sp))));
}


void show_all_controls(void)
{
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	show_controls(sp);
    }
}


void hide_all_controls(void)
{
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	hide_controls(sp);
    }
}


int control_panel_height(snd_info *sp)
{
  return(widget_height(CONTROLS(sp)));
}


/* -------- PROGRESS REPORT -------- */

void progress_report(chan_info *cp, mus_float_t pct)
{
  int which;
  snd_info *sp;
  sp = cp->sound;

  if ((!HAS_WIDGETS(sp)) || (sp->inuse != SOUND_NORMAL)) return;

  which = (int)(pct * NUM_HOURGLASSES);
  if (which >= NUM_HOURGLASSES) which = NUM_HOURGLASSES - 1;
  if (which < 0) which = 0;

  if ((hourglasses[which]) &&
      (cp->chan < sp->num_progress_widgets) &&
      ((cp->chan == 0) ||
       (sp->channel_style != CHANNELS_SUPERIMPOSED)))
    {
      XtVaSetValues(PROGRESS_ICON(cp), XmNlabelPixmap, hourglasses[which], NULL);
      XmUpdateDisplay(PROGRESS_ICON(cp));
    }

  check_for_event();
}

void finish_progress_report(chan_info *cp)
{
  snd_info *sp;
  sp = cp->sound;

  if ((!HAS_WIDGETS(sp)) || (sp->inuse != SOUND_NORMAL)) return;

  if ((cp->chan < sp->num_progress_widgets) &&
      ((cp->chan == 0) ||
       (sp->channel_style != CHANNELS_SUPERIMPOSED)))
    {
      XtVaSetValues(PROGRESS_ICON(cp), XmNlabelPixmap, blank_pixmap, NULL);
      XmUpdateDisplay(PROGRESS_ICON(cp));
      hide_stop_sign(sp);
    }
}


void start_progress_report(chan_info *cp)
{
  snd_info *sp;
  sp = cp->sound;

  if ((!HAS_WIDGETS(sp)) || (sp->inuse != SOUND_NORMAL)) return;

  if ((cp->chan < sp->num_progress_widgets) &&
      ((cp->chan == 0) ||
       (sp->channel_style != CHANNELS_SUPERIMPOSED)))
    {
      XtVaSetValues(PROGRESS_ICON(cp), XmNlabelPixmap, hourglasses[0], NULL);
      XmUpdateDisplay(PROGRESS_ICON(cp));
      show_stop_sign(sp);
    }
}



void reflect_sound_selection(snd_info *sp)
{
  /* sp is the newly selected sound, ss->selected_sound is the previous one */
  snd_info *osp = NULL;

  if (ss->selected_sound != NO_SELECTION) 
    osp = ss->sounds[ss->selected_sound];

  if ((osp) && 
      (sp != osp) && 
      (osp->inuse == SOUND_NORMAL)) 
    {
      XmChangeColor(SND_NAME(osp), ss->highlight_color);
      if (sound_style(ss) == SOUNDS_IN_NOTEBOOK) 
	XmChangeColor(osp->tab, ss->graph_color);
    }

  if (sp->selected_channel != NO_SELECTION) 
    {
      XmChangeColor(SND_NAME(sp), ss->white);
      if (sound_style(ss) == SOUNDS_IN_NOTEBOOK) 
	{
	  int page, current_page;
	  XmNotebookPageStatus status;
	  XmNotebookPageInfo info;
	  XmChangeColor(sp->tab, ss->selected_graph_color);
	  XtVaGetValues(SOUND_PANE(ss), XmNcurrentPageNumber, &current_page, NULL);
	  XtVaGetValues(sp->tab, XmNpageNumber, &page, NULL);
	  if (page != current_page)
	    {
	      status = XmNotebookGetPageInfo(SOUND_PANE(ss), page, &info);
	      if (status == XmPAGE_FOUND)
		{
		  XtVaSetValues(SOUND_PANE(ss), XmNcurrentPageNumber, page, NULL);
		}
	    }
	}
    }
}


/* -------- controls dialog -------- */

static Widget controls_dialog = NULL;
enum {EXPAND_HOP, EXPAND_LENGTH, EXPAND_RAMP, EXPAND_JITTER, CONTRAST_AMP, REVERB_LOWPASS, REVERB_FEEDBACK};
static Widget controls[7];

static void reset_all_sliders(void)
{
  expand_control_set_hop(DEFAULT_EXPAND_CONTROL_HOP);
  expand_control_set_length(DEFAULT_EXPAND_CONTROL_LENGTH);
  expand_control_set_ramp(DEFAULT_EXPAND_CONTROL_RAMP);
  expand_control_set_jitter(DEFAULT_EXPAND_CONTROL_JITTER);
  contrast_control_set_amp(DEFAULT_CONTRAST_CONTROL_AMP);
  reverb_control_set_lowpass(DEFAULT_REVERB_CONTROL_LOWPASS);
  reverb_control_set_feedback(DEFAULT_REVERB_CONTROL_FEEDBACK);

  XtVaSetValues(controls[EXPAND_HOP], XmNvalue, (int)(expand_control_hop(ss) * 1000), NULL);
  XtVaSetValues(controls[EXPAND_LENGTH], XmNvalue, (int)(expand_control_length(ss) * 1000), NULL);
  XtVaSetValues(controls[EXPAND_RAMP], XmNvalue, (int)(expand_control_ramp(ss) * 1000), NULL);
  XtVaSetValues(controls[EXPAND_JITTER], XmNvalue, (int)(expand_control_jitter(ss) * 1000), NULL);
  XtVaSetValues(controls[CONTRAST_AMP], XmNvalue, (int)(contrast_control_amp(ss) * 1000), NULL);
  XtVaSetValues(controls[REVERB_LOWPASS], XmNvalue, (int)(reverb_control_lowpass(ss) * 1000), NULL);
  XtVaSetValues(controls[REVERB_FEEDBACK], XmNvalue, (int)(reverb_control_feedback(ss) * 1000), NULL);
}

static void controls_reset_callback(Widget w, XtPointer context, XtPointer info) 
{
  if (XmGetFocusWidget(controls_dialog) == XmMessageBoxGetChild(controls_dialog, XmDIALOG_OK_BUTTON))
    reset_all_sliders();
}

static void controls_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help("More controls", 
"This dialog controls all the otherwise hidden control-panel variables.\n\
Expand-hop sets the time in seconds between successive grains.\n\
Expand-length sets the length of each grain.\n\
Expand-ramp sets the ramp-time in the grain envelope.\n\
Expand-jitter sets the grain timing jitter.\n\
Contrast-amp sets the prescaler for contrast-enhancement.\n\
Reverb-lowpass sets the feedback lowpass filter coeficient.\n\
Reverb-feedback sets the scaler on the feedback.",
	   WITHOUT_WORD_WRAP);
}

static void controls_quit_callback(Widget w, XtPointer context, XtPointer info) 
{
  XtUnmanageChild(controls_dialog);
}

static void expand_hop_callback(Widget w, XtPointer context, XtPointer info)
{
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  expand_control_set_hop(cbs->value * 0.001);
}

static void expand_length_callback(Widget w, XtPointer context, XtPointer info)
{
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  expand_control_set_length(cbs->value * 0.001);
}

static void expand_ramp_callback(Widget w, XtPointer context, XtPointer info)
{
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  expand_control_set_ramp(cbs->value * 0.001);
}

static void expand_jitter_callback(Widget w, XtPointer context, XtPointer info)
{
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  expand_control_set_jitter(cbs->value * 0.001);
}

static void contrast_amp_callback(Widget w, XtPointer context, XtPointer info)
{
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  contrast_control_set_amp(cbs->value * 0.001);
}

static void reverb_lowpass_callback(Widget w, XtPointer context, XtPointer info)
{
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  reverb_control_set_lowpass(cbs->value * 0.001);
}

static void reverb_feedback_callback(Widget w, XtPointer context, XtPointer info)
{
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  reverb_control_set_feedback(cbs->value * 0.001);
}

void make_controls_dialog(void)
{
  #define MSG_BOX(Dialog, Child) XmMessageBoxGetChild(Dialog, Child)
  if (!controls_dialog)
    {
      int n;
      Arg args[32];
      XmString go_away, xhelp, titlestr, xreset;
      Widget mainform, slider, reset_button;

      go_away = XmStringCreateLocalized((char *)I_GO_AWAY);
      xhelp = XmStringCreateLocalized((char *)I_HELP);
      titlestr = XmStringCreateLocalized((char *)"More controls");
      xreset = XmStringCreateLocalized((char *)"Reset");

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNokLabelString, xreset); n++;
      XtSetArg(args[n], XmNcancelLabelString, go_away); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNtransient, false); n++;
      XtSetArg(args[n], XmNwidth, 400); n++;
      controls_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), (char *)"More controls", args, n);
      reset_button = MSG_BOX(controls_dialog, XmDIALOG_OK_BUTTON);

      XtAddCallback(controls_dialog, XmNhelpCallback,   controls_help_callback,  NULL);
      /* XtAddCallback(controls_dialog, XmNokCallback,  controls_reset_callback, NULL); */
      XtAddCallback(reset_button, XmNactivateCallback,  controls_reset_callback, NULL);
      XtAddCallback(controls_dialog, XmNcancelCallback, controls_quit_callback,  NULL);

      XmStringFree(xhelp);
      XmStringFree(go_away);
      XmStringFree(titlestr);
      XmStringFree(xreset);

      XtVaSetValues(MSG_BOX(controls_dialog, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->selection_color, NULL);
      XtVaSetValues(MSG_BOX(controls_dialog, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->selection_color, NULL);
      XtVaSetValues(MSG_BOX(controls_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->selection_color, NULL);
      XtVaSetValues(MSG_BOX(controls_dialog, XmDIALOG_OK_BUTTON),     XmNbackground, ss->highlight_color, NULL);
      XtVaSetValues(MSG_BOX(controls_dialog, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->highlight_color, NULL);
      XtVaSetValues(MSG_BOX(controls_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->highlight_color, NULL);

      mainform = XtVaCreateManagedWidget("formd", xmRowColumnWidgetClass, controls_dialog,
					 XmNleftAttachment,   XmATTACH_FORM,
					 XmNrightAttachment,  XmATTACH_FORM,
					 XmNtopAttachment,    XmATTACH_FORM,
					 XmNbottomAttachment, XmATTACH_WIDGET,
					 XmNbottomWidget,     XmMessageBoxGetChild(controls_dialog, XmDIALOG_SEPARATOR),
					 XmNorientation,      XmVERTICAL, 
					 NULL);

      titlestr = XmStringCreateLocalized((char *)"expand hop");
      slider = XtVaCreateManagedWidget("expand-hop", xmScaleWidgetClass, mainform,
				       XmNorientation,   XmHORIZONTAL,
				       XmNshowValue,     true,
				       XmNminimum,       1,
				       XmNmaximum,       300,
				       XmNvalue,         (int)(expand_control_hop(ss) * 1000),
				       XmNdecimalPoints, 3,
				       XmNtitleString,   titlestr,
				       XmNborderWidth,   1,
				       XmNbackground,    ss->basic_color,
				       NULL);
      XmStringFree(titlestr);
      XtAddCallback(slider, XmNvalueChangedCallback, expand_hop_callback, NULL);
      XtAddCallback(slider, XmNdragCallback, expand_hop_callback, NULL);
      controls[EXPAND_HOP] = slider;

      titlestr = XmStringCreateLocalized((char *)"expand length");
      slider = XtVaCreateManagedWidget("expand-length", xmScaleWidgetClass, mainform,
				       XmNorientation,   XmHORIZONTAL,
				       XmNshowValue,     true,
				       XmNminimum,       10,
				       XmNmaximum,       500,
				       XmNvalue,         (int)(expand_control_length(ss) * 1000),
				       XmNdecimalPoints, 3,
				       XmNtitleString,   titlestr,
				       XmNborderWidth,   1,
				       XmNbackground,    ss->basic_color,
				       NULL);
      XmStringFree(titlestr);
      XtAddCallback(slider, XmNvalueChangedCallback, expand_length_callback, NULL);
      XtAddCallback(slider, XmNdragCallback, expand_length_callback, NULL);
      controls[EXPAND_LENGTH] = slider;

      titlestr = XmStringCreateLocalized((char *)"expand ramp");
      slider = XtVaCreateManagedWidget("expand-ramp", xmScaleWidgetClass, mainform,
				       XmNorientation,   XmHORIZONTAL,
				       XmNshowValue,     true,
				       XmNminimum,       10,
				       XmNmaximum,       500,
				       XmNvalue,         (int)(expand_control_ramp(ss) * 1000),
				       XmNdecimalPoints, 3,
				       XmNtitleString,   titlestr,
				       XmNborderWidth,   1,
				       XmNbackground,    ss->basic_color,
				       NULL);
      XmStringFree(titlestr);
      XtAddCallback(slider, XmNvalueChangedCallback, expand_ramp_callback, NULL);
      XtAddCallback(slider, XmNdragCallback, expand_ramp_callback, NULL);
      controls[EXPAND_RAMP] = slider;

      titlestr = XmStringCreateLocalized((char *)"expand jitter");
      slider = XtVaCreateManagedWidget("expand-hop", xmScaleWidgetClass, mainform,
				       XmNorientation,   XmHORIZONTAL,
				       XmNshowValue,     true,
				       XmNminimum,       0,
				       XmNmaximum,       200,
				       XmNvalue,         (int)(expand_control_jitter(ss) * 1000),
				       XmNdecimalPoints, 3,
				       XmNtitleString,   titlestr,
				       XmNborderWidth,   1,
				       XmNbackground,    ss->basic_color,
				       NULL);
      XmStringFree(titlestr);
      XtAddCallback(slider, XmNvalueChangedCallback, expand_jitter_callback, NULL);
      XtAddCallback(slider, XmNdragCallback, expand_jitter_callback, NULL);
      controls[EXPAND_JITTER] = slider;

      titlestr = XmStringCreateLocalized((char *)"contrast amp");
      slider = XtVaCreateManagedWidget("contrast-amp", xmScaleWidgetClass, mainform,
				       XmNorientation,   XmHORIZONTAL,
				       XmNshowValue,     true,
				       XmNminimum,       0,
				       XmNmaximum,       2000,
				       XmNvalue,         (int)(contrast_control_amp(ss) * 1000),
				       XmNdecimalPoints, 3,
				       XmNtitleString,   titlestr,
				       XmNborderWidth,   1,
				       XmNbackground,    ss->basic_color,
				       NULL);
      XmStringFree(titlestr);
      XtAddCallback(slider, XmNvalueChangedCallback, contrast_amp_callback, NULL);
      XtAddCallback(slider, XmNdragCallback, contrast_amp_callback, NULL);
      controls[CONTRAST_AMP] = slider;

      titlestr = XmStringCreateLocalized((char *)"reverb lowpass");
      slider = XtVaCreateManagedWidget("reverb-lowpass", xmScaleWidgetClass, mainform,
				       XmNorientation,   XmHORIZONTAL,
				       XmNshowValue,     true,
				       XmNminimum,       0,
				       XmNmaximum,       1000,
				       XmNvalue,         (int)(reverb_control_lowpass(ss) * 1000),
				       XmNdecimalPoints, 3,
				       XmNtitleString,   titlestr,
				       XmNborderWidth,   1,
				       XmNbackground,    ss->basic_color,
				       NULL);
      XmStringFree(titlestr);
      XtAddCallback(slider, XmNvalueChangedCallback, reverb_lowpass_callback, NULL);
      XtAddCallback(slider, XmNdragCallback, reverb_lowpass_callback, NULL);
      controls[REVERB_LOWPASS] = slider;

      titlestr = XmStringCreateLocalized((char *)"reverb feedback");
      slider = XtVaCreateManagedWidget("reverb-feedback", xmScaleWidgetClass, mainform,
				       XmNorientation,   XmHORIZONTAL,
				       XmNshowValue,     true,
				       XmNminimum,       0,
				       XmNmaximum,       1250,
				       XmNvalue,         (int)(reverb_control_feedback(ss) * 1000),
				       XmNdecimalPoints, 3,
				       XmNtitleString,   titlestr,
				       XmNborderWidth,   1,
				       XmNbackground,    ss->basic_color,
				       NULL);
      XmStringFree(titlestr);
      XtAddCallback(slider, XmNvalueChangedCallback, reverb_feedback_callback, NULL);
      XtAddCallback(slider, XmNdragCallback, reverb_feedback_callback, NULL);
      controls[REVERB_FEEDBACK] = slider;

      set_dialog_widget(CONTROLS_DIALOG, controls_dialog);
    }
  
  if (!XtIsManaged(controls_dialog))
    XtManageChild(controls_dialog);
}


/* ---------------------------------------- */

static XEN g_sound_widgets(XEN snd)
{
  #define H_sound_widgets "(" S_sound_widgets " :optional snd): a list of \
widgets: (0)pane (1)name (2)control-panel (3)status area (4)play-button (5)filter-env (6)unite-button (7)name-label (8)name-icon (9)sync-button"

  snd_info *sp;

  ASSERT_SOUND(S_sound_widgets, snd, 1);

  sp = get_sp(snd);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_sound_widgets, snd));
  if (!HAS_WIDGETS(sp))
    return(XEN_EMPTY_LIST);

  return(XEN_CONS(XEN_WRAP_WIDGET(SND_PANE(sp)),
	  XEN_CONS(XEN_WRAP_WIDGET(SND_NAME(sp)),
           XEN_CONS(XEN_WRAP_WIDGET(CONTROLS(sp)),
	    XEN_CONS(XEN_WRAP_WIDGET(STATUS_AREA(sp)),
#if WITH_AUDIO
	     XEN_CONS(XEN_WRAP_WIDGET(PLAY_BUTTON(sp)),
#else
	     XEN_CONS(XEN_FALSE,
#endif
	      XEN_CONS(XEN_WRAP_WIDGET(FILTER_GRAPH(sp)), /* this is the drawingarea widget */
	       XEN_CONS(XEN_WRAP_WIDGET(UNITE_BUTTON(sp)),
		XEN_CONS(XEN_FALSE,
	         XEN_CONS(XEN_WRAP_WIDGET(LOCK_OR_BOMB(sp)),
	          XEN_CONS(XEN_WRAP_WIDGET(SYNC_BUTTON(sp)),
	           XEN_EMPTY_LIST)))))))))));
}


XEN_ARGIFY_1(g_sound_widgets_w, g_sound_widgets)

void g_init_gxsnd(void)
{
  XEN_ADD_HOOK(ss->snd_open_file_hook, reflect_file_close_in_sync_w, "sync-open-file-watcher", "sound sync open-file-hook handler");
  XEN_DEFINE_PROCEDURE(S_sound_widgets,  g_sound_widgets_w,  0, 1, 0, H_sound_widgets);
}
