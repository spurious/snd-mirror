#include "snd.h"

#if HAVE_XPM
  #include <X11/xpm.h>
#endif

#ifndef SGI
  #define TOGGLE_MARGIN 0
#endif

enum {W_pane,
      W_name_form, W_amp_form,
      W_amp, W_amp_label, W_amp_number, W_amp_separator,
      W_speed, W_speed_label, W_speed_number, W_speed_arrow,
      W_expand, W_expand_label, W_expand_number, W_expand_button,
      W_contrast, W_contrast_label, W_contrast_number, W_contrast_button,
      W_revscl, W_revscl_label, W_revscl_number,
      W_revlen, W_revlen_label, W_revlen_number, W_reverb_button,
      W_filter_label, W_filter_order, W_filter_env, W_filter, W_filter_button, W_filter_dB, W_filter_hz, W_filter_frame,
      W_filter_order_down, W_filter_order_up,
      W_name, W_name_icon, W_info_label, W_info,
      W_info_sep,
      W_play, W_sync, W_unite,
      W_control_panel
};

#define NUM_SND_WIDGETS 45

Widget unite_button(snd_info *sp) {return((sp->sgx)->snd_widgets[W_unite]);}
Widget filter_graph(snd_info *sp) {return((sp->sgx)->snd_widgets[W_filter_env]);}

Widget w_snd_pane(snd_info *sp)   {return((sp->sgx)->snd_widgets[W_pane]);}
Widget w_snd_name(snd_info *sp)   {return((sp->sgx)->snd_widgets[W_name]);}

#define CONTROL_PANEL(Sp)        (Sp->sgx)->snd_widgets[W_control_panel]
#define PLAY_BUTTON(Sp)          (Sp->sgx)->snd_widgets[W_play]
#define NAME_ICON(Sp)            (Sp->sgx)->snd_widgets[W_name_icon]
#define AMP_SCROLLBAR(Sp)        (Sp->sgx)->snd_widgets[W_amp]
#define SPEED_SCROLLBAR(Sp)      (Sp->sgx)->snd_widgets[W_speed]
#define SPEED_ARROW(Sp)          (Sp->sgx)->snd_widgets[W_speed_arrow]
#define EXPAND_SCROLLBAR(Sp)     (Sp->sgx)->snd_widgets[W_expand]
#define CONTRAST_SCROLLBAR(Sp)   (Sp->sgx)->snd_widgets[W_contrast]
#define REVSCL_SCROLLBAR(Sp)     (Sp->sgx)->snd_widgets[W_revscl]
#define REVLEN_SCROLLBAR(Sp)     (Sp->sgx)->snd_widgets[W_revlen]
#define AMP_LABEL(Sp)            (Sp->sgx)->snd_widgets[W_amp_number]
#define SPEED_LABEL(Sp)          (Sp->sgx)->snd_widgets[W_speed_number]
#define EXPAND_LABEL(Sp)         (Sp->sgx)->snd_widgets[W_expand_number]
#define EXPAND_BUTTON(Sp)        (Sp->sgx)->snd_widgets[W_expand_button]
#define CONTRAST_LABEL(Sp)       (Sp->sgx)->snd_widgets[W_contrast_number]
#define CONTRAST_BUTTON(Sp)      (Sp->sgx)->snd_widgets[W_contrast_button]
#define REVSCL_LABEL(Sp)         (Sp->sgx)->snd_widgets[W_revscl_number]
#define REVLEN_LABEL(Sp)         (Sp->sgx)->snd_widgets[W_revlen_number]
#define REVERB_BUTTON(Sp)        (Sp->sgx)->snd_widgets[W_reverb_button]
#define FILTER_ORDER_TEXT(Sp)    (Sp->sgx)->snd_widgets[W_filter_order]
#define FILTER_COEFFS_TEXT(Sp)   (Sp->sgx)->snd_widgets[W_filter]
#define FILTER_BUTTON(Sp)        (Sp->sgx)->snd_widgets[W_filter_button]
#define FILTER_DB_BUTTON(Sp)     (Sp->sgx)->snd_widgets[W_filter_dB]
#define FILTER_HZ_BUTTON(Sp)     (Sp->sgx)->snd_widgets[W_filter_hz]
#define MINIBUFFER_SEPARATOR(Sp) (Sp->sgx)->snd_widgets[W_info_sep]
#define MINIBUFFER_LABEL(Sp)     (Sp->sgx)->snd_widgets[W_info_label]
#define MINIBUFFER_TEXT(Sp)      (Sp->sgx)->snd_widgets[W_info]
#define SYNC_BUTTON(Sp)          (Sp->sgx)->snd_widgets[W_sync]

void goto_minibuffer(snd_info *sp)
{
  if (sp) goto_window(MINIBUFFER_TEXT(sp));
}

void set_minibuffer_string(snd_info *sp, char *str, bool update) 
{
  if ((sp->inuse != SOUND_NORMAL) || (!(sp->sgx))) return;
  XmTextSetString(MINIBUFFER_TEXT(sp), str);
  if (update) XmUpdateDisplay(MINIBUFFER_TEXT(sp));
}

void set_minibuffer_cursor_position(snd_info *sp, int pos)
{
  if ((sp->inuse != SOUND_NORMAL) || (!(sp->sgx))) return;
  XmTextSetCursorPosition(MINIBUFFER_TEXT(sp), pos);
}

char *get_minibuffer_string(snd_info *sp) 
{
  if ((sp->inuse != SOUND_NORMAL) || (!(sp->sgx))) return(NULL);
  return(XmTextGetString(MINIBUFFER_TEXT(sp)));
}

void make_minibuffer_label(snd_info *sp , char *str)
{
  if ((sp->sgx) && (MINIBUFFER_LABEL(sp)))
    {
      XmString s1;
      s1 = XmStringCreate(str, XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(MINIBUFFER_LABEL(sp), XmNlabelString, s1, NULL);
      XmStringFree(s1);
    }
}

static Latus open_ctrls_height = 180; /* just a first guess */
static Latus ctrls_height = CLOSED_CTRLS_HEIGHT;

void sound_unlock_control_panel(snd_info *sp, void *ptr)
{
  XtManageChild(CONTROL_PANEL(sp));
  XtVaSetValues(CONTROL_PANEL(sp), XmNpaneMinimum, 1, NULL);
}

void sound_lock_control_panel(snd_info *sp, void *ptr)
{
  Dimension height;
  XtVaGetValues(CONTROL_PANEL(sp), XmNpaneMaximum, &height, NULL);  
  XtUnmanageChild(CONTROL_PANEL(sp));
  if (height > ctrls_height) height = ctrls_height;
  XtVaSetValues(CONTROL_PANEL(sp), XmNpaneMinimum, height, NULL);
}

static void name_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  sp_name_click((snd_info *)context);
}

/* The 0.9 * SCROLLBAR_MAX reflects the fact that the slider is 10% of the trough, and the left edge of the slider is the readback value */

/* ---------------- AMP-CONTROL ---------------- */

int amp_to_scroll(Float minval, Float val, Float maxval)
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
  mus_snprintf(amp_number_buffer, 6, "%.3f", sp->amp_control);
  set_label(AMP_LABEL(sp), amp_number_buffer);
  return(val);
}

void set_amp(snd_info *sp, Float val)
{
  if (IS_PLAYER(sp))
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
  ASSERT_WIDGET_TYPE(XmIsPushButton(w), w);
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    set_amp(sp, sp->last_amp_control);
  else set_amp(sp, 1.0);
}

static void amp_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  scroll_to_amp((snd_info *)context, ((XmScrollBarCallbackStruct *)info)->value);
}

static void amp_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
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
    case SPEED_CONTROL_AS_RATIO:    return(XmStringCreate("  1/1", XmFONTLIST_DEFAULT_TAG)); break;
    case SPEED_CONTROL_AS_SEMITONE: return(XmStringCreate("    0", XmFONTLIST_DEFAULT_TAG)); break;
    default:                        return(XmStringCreate(" 1.00", XmFONTLIST_DEFAULT_TAG)); break;
    }
}

static int speed_to_scroll(Float minval, Float val, Float maxval)
{
  if (val <= minval) return(0);
  if (val >= maxval) return((int)(0.9 * SCROLLBAR_MAX));
  return(snd_round(0.9 * SCROLLBAR_MAX * ((log(val) - log(minval)) / (log(maxval) - log(minval)))));
}

static int scroll_to_speed(snd_info *sp, int ival)
{
  char speed_number_buffer[6];
  sp->speed_control = speed_changed(exp((ival * (log(sp->speed_control_max) - log(sp->speed_control_min)) / (0.9 * SCROLLBAR_MAX)) + log(sp->speed_control_min)),
				    speed_number_buffer,
				    sp->speed_control_style,
				    sp->speed_control_tones,
				    6);
  set_label(SPEED_LABEL(sp), speed_number_buffer);
  return(ival);
}

void set_speed(snd_info *sp, Float val)
{
  if (IS_PLAYER(sp))
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
  ASSERT_WIDGET_TYPE(XmIsPushButton(w), w);
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    set_speed(sp, sp->last_speed_control);
  else set_speed(sp, 1.0);
#if (HAVE_SCM_MAKE_RATIO || HAVE_SCM_C_MAKE_RECTANGULAR)
  if (sp->speed_control_style == SPEED_CONTROL_AS_RATIO)
    snd_rationalize(sp->speed_control, &(sp->speed_control_numerator), &(sp->speed_control_denominator));
#endif
}

static void speed_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
#if (HAVE_SCM_MAKE_RATIO || HAVE_SCM_C_MAKE_RECTANGULAR)
  snd_info *sp = (snd_info *)context;
#endif
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  scroll_to_speed((snd_info *)context, ((XmScrollBarCallbackStruct *)info)->value);
#if (HAVE_SCM_MAKE_RATIO || HAVE_SCM_C_MAKE_RECTANGULAR)
  if (sp->speed_control_style == SPEED_CONTROL_AS_RATIO)
    snd_rationalize(sp->speed_control, &(sp->speed_control_numerator), &(sp->speed_control_denominator));
#endif
}

static void speed_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  scroll_to_speed(sp, cb->value);
#if (HAVE_SCM_MAKE_RATIO || HAVE_SCM_C_MAKE_RECTANGULAR)
  if (sp->speed_control_style == SPEED_CONTROL_AS_RATIO)
    snd_rationalize(sp->speed_control, &(sp->speed_control_numerator), &(sp->speed_control_denominator));
#endif
  sp->last_speed_control = sp->saved_speed_control;
  sp->saved_speed_control = sp->speed_control;
}

void toggle_direction_arrow(snd_info *sp, bool state)
{
  if (IS_PLAYER(sp))
    sp->speed_control_direction = ((state) ? -1 : 1);
  else XmToggleButtonSetState(SPEED_ARROW(sp), (Boolean)state, true);
}


/* ---------------- EXPAND-CONTROL ---------------- */

static int expand_to_scroll(Float minval, Float val, Float maxval)
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
  mus_snprintf(expand_number_buffer, 6, "%.3f", sp->expand_control);
  set_label(EXPAND_LABEL(sp), expand_number_buffer);
  return(val);
}

void set_expand(snd_info *sp, Float val)
{
  if (IS_PLAYER(sp))
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
  ASSERT_WIDGET_TYPE(XmIsPushButton(w), w);
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask))
    set_expand(sp, sp->last_expand_control);
  else set_expand(sp, 1.0);
}

static void expand_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  scroll_to_expand((snd_info *)context, ((XmScrollBarCallbackStruct *)info)->value);
}

static void expand_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  scroll_to_expand(sp, cb->value);
  sp->last_expand_control = sp->saved_expand_control;
  sp->saved_expand_control = sp->expand_control;
}

static void expand_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info; 
  snd_info *sp = (snd_info *)context;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  sp->expand_control_p = cb->set;
  if (!(ss->using_schemes)) 
    XmChangeColor(EXPAND_SCROLLBAR(sp), (Pixel)((sp->expand_control_p) ? ((ss->sgx)->position_color) : ((ss->sgx)->basic_color)));
}

void toggle_expand_button(snd_info *sp, bool state)
{
  if (IS_PLAYER(sp))
    sp->expand_control_p = state;
  else XmToggleButtonSetState(EXPAND_BUTTON(sp), (Boolean)state, true);
}


/* ---------------- CONTRAST-CONTROL ---------------- */

static int contrast_to_scroll(Float minval, Float val, Float maxval)
{
  if (val <= minval) return(0);
  if (val >= maxval) return((int)(0.9 * SCROLLBAR_MAX));
  return(snd_round((val - minval) / (maxval - minval) * 0.9 * SCROLLBAR_MAX));
}

static int scroll_to_contrast(snd_info *sp, int val)
{
  char contrast_number_buffer[6];
  sp->contrast_control = sp->contrast_control_min + val * (sp->contrast_control_max - sp->contrast_control_min) / (0.9 * SCROLLBAR_MAX);
  mus_snprintf(contrast_number_buffer, 6, "%.3f", sp->contrast_control);
  set_label(CONTRAST_LABEL(sp), contrast_number_buffer);
  return(val);
}

void set_contrast(snd_info *sp, Float val)
{
  if (IS_PLAYER(sp))
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
  ASSERT_WIDGET_TYPE(XmIsPushButton(w), w);
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask))
    set_contrast(sp, sp->last_contrast_control);
  else set_contrast(sp, 0.0);
}

static void contrast_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  scroll_to_contrast((snd_info *)context, ((XmScrollBarCallbackStruct *)info)->value);
}

static void contrast_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  scroll_to_contrast(sp, cb->value);
  sp->last_contrast_control = sp->saved_contrast_control;
  sp->saved_contrast_control = sp->contrast_control;
}

static void contrast_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  sp->contrast_control_p = cb->set;
  if (!(ss->using_schemes)) 
    XmChangeColor(CONTRAST_SCROLLBAR(sp), (Pixel)((sp->contrast_control_p) ? ((ss->sgx)->position_color) : ((ss->sgx)->basic_color)));
}

void toggle_contrast_button(snd_info *sp, bool state)
{
  if (IS_PLAYER(sp))
    sp->contrast_control_p = state;
  else XmToggleButtonSetState(CONTRAST_BUTTON(sp), (Boolean)state, true);
}


/* ---------------- REVERB-CONTROL-SCALE ---------------- */

static int revscl_to_scroll(Float minval, Float val, Float maxval)
{
  if (val <= minval) return(0);
  if (val >= maxval) return((int)(0.9 * SCROLLBAR_MAX));
  return(snd_round(0.9 * SCROLLBAR_MAX * (pow(val, 0.333) - pow(minval, 0.333)) / (pow(maxval, 0.333) - pow(minval, 0.333))));
}

static Float cube (Float a) {return(a*a*a);}

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
  mus_snprintf(revscl_number_buffer, 7, "%.4f", sp->reverb_control_scale);
  set_label(REVSCL_LABEL(sp), revscl_number_buffer);
  return(val);
}

void set_revscl(snd_info *sp, Float val)
{
  if (IS_PLAYER(sp))
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
  ASSERT_WIDGET_TYPE(XmIsPushButton(w), w);
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
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  scroll_to_revscl(sp, cb->value);
  sp->last_reverb_control_scale = sp->saved_reverb_control_scale;
  sp->saved_reverb_control_scale = sp->reverb_control_scale;
}


/* ---------------- REVERB-CONTROL-LENGTH ---------------- */

static int revlen_to_scroll(Float minval, Float val, Float maxval)
{
  if (val <= minval) return(0);
  if (val >= maxval) return((int)(0.9 * SCROLLBAR_MAX));
  return(snd_round((val - minval) / (maxval - minval) * 0.9 * SCROLLBAR_MAX));
}

static int scroll_to_revlen(snd_info *sp, int val)
{
  char revlen_number_buffer[5];
  sp->reverb_control_length = sp->reverb_control_length_min + 
    (sp->reverb_control_length_max - sp->reverb_control_length_min) * (Float)val / (0.9 * SCROLLBAR_MAX);
  mus_snprintf(revlen_number_buffer, 5, "%.2f", sp->reverb_control_length);
  set_label(REVLEN_LABEL(sp), revlen_number_buffer);
  return(val);
}

void set_revlen(snd_info *sp, Float val)
{
  if (IS_PLAYER(sp))
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
  ASSERT_WIDGET_TYPE(XmIsPushButton(w), w);
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    set_revlen(sp, sp->last_reverb_control_length);
  else set_revlen(sp, 1.0);
}

static void revlen_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  scroll_to_revlen((snd_info *)context, ((XmScrollBarCallbackStruct *)info)->value);
}

static void revlen_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  scroll_to_revlen(sp, cb->value);
  sp->last_reverb_control_length = sp->saved_reverb_control_length;
  sp->saved_reverb_control_length = sp->reverb_control_length;
}

static void reverb_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  sp->reverb_control_p = cb->set;
  if (!(ss->using_schemes))
    {
      XmChangeColor(REVLEN_SCROLLBAR(sp), (Pixel)((sp->reverb_control_p) ? ((ss->sgx)->position_color) : ((ss->sgx)->basic_color)));
      XmChangeColor(REVSCL_SCROLLBAR(sp), (Pixel)((sp->reverb_control_p) ? ((ss->sgx)->position_color) : ((ss->sgx)->basic_color)));
    }
}

void toggle_reverb_button(snd_info *sp, bool state)
{
  if (IS_PLAYER(sp))
    sp->reverb_control_p = state;
  else XmToggleButtonSetState(REVERB_BUTTON(sp), (Boolean)state, true);
}


/* ---------------- FILTER_CONTROL ---------------- */

static void filter_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  sp->filter_control_p = cb->set;
}

void toggle_filter_button(snd_info *sp, bool state)
{
  if (IS_PLAYER(sp))
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
  axis_context *ax;
  int height, width;
  Widget drawer;
  env_editor *edp;
  edp = (env_editor *)(sp->sgx->flt);
  drawer = filter_graph(sp);
  height = widget_height(drawer);
  if (height < MIN_FILTER_GRAPH_HEIGHT) return;
  width = widget_width(drawer);
  ax = (axis_context *)CALLOC(1, sizeof(axis_context));
  ax->gc = (ss->sgx)->fltenv_basic_gc;
  ax->wn = XtWindow(drawer);
  ax->dp = XtDisplay(drawer);
  XClearWindow(ax->dp, ax->wn);
  edp->in_dB = sp->filter_control_in_dB;
  edp->with_dots = true;
  if (sp->filter_control_in_hz)
    sp->filter_control_xmax = (Float)(SND_SRATE(sp) / 2);
  else sp->filter_control_xmax = 1.0;
  if (sp->filter_control_envelope == NULL) sp->filter_control_envelope = default_env(sp->filter_control_xmax, 1.0);
  env_editor_display_env(edp, sp->filter_control_envelope, ax, _("frequency response"), 0, 0, width, height, NOT_PRINTING);
  if (edp->edited)
    {
      ax->gc = (ss->sgx)->fltenv_data_gc;
      display_frequency_response(sp->filter_control_envelope, 
				 (SOUND_ENV_EDITOR(sp))->axis, ax, 
				 sp->filter_control_order, 
				 sp->filter_control_in_dB);
    }
  ax = free_axis_context(ax);
}

void set_filter_text(snd_info *sp, char *str)
{
  if (!(IS_PLAYER(sp)))
    XmTextSetString(FILTER_COEFFS_TEXT(sp), str);
}

#ifdef MAC_OSX
static int press_x, press_y;
#endif

static void filter_drawer_button_motion(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  snd_info *sp = (snd_info *)context;
  XMotionEvent *ev = (XMotionEvent *)event;
  env_editor *edp;
#ifdef MAC_OSX
  if ((press_x == ev->x) && (press_y == ev->y)) return;
#endif
  edp = (env_editor *)(sp->sgx->flt);
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
#ifdef MAC_OSX
  press_x = ev->x;
  press_y = ev->y;
#endif
  edp = (env_editor *)(sp->sgx->flt);
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
  if (tmpstr) FREE(tmpstr);
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
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  sp->filter_control_in_dB = (cb->set);
  display_filter_env(sp);
}

void set_filter_in_dB(snd_info *sp, bool val)
{
  sp->filter_control_in_dB = val;
  if (!(IS_PLAYER(sp)))
    {
      XmToggleButtonSetState(FILTER_DB_BUTTON(sp), (Boolean)val, false);
      display_filter_env(sp);
    }
}

static void new_in_hz(snd_info *sp, bool val)
{
  sp->filter_control_in_hz = val;
  if (val)
    sp->filter_control_xmax = (Float)(SND_SRATE(sp) / 2);
  else sp->filter_control_xmax = 1.0;
  if (sp->filter_control_envelope) free_env(sp->filter_control_envelope);
  sp->filter_control_envelope = default_env(sp->filter_control_xmax, 1.0);
}

static void filter_hz_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  new_in_hz(sp, cb->set);
  display_filter_env(sp);
}

void set_filter_in_hz(snd_info *sp, bool val)
{
  new_in_hz(sp, val);
  if (!(IS_PLAYER(sp)))
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
  if (!(IS_PLAYER(sp)))
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

static void filter_activate_callback(Widget w, XtPointer context, XtPointer info)
{
  /* make an envelope out of the data */
  snd_info *sp = (snd_info *)context;
  char *str = NULL;
  XmAnyCallbackStruct *cb = (XmAnyCallbackStruct *)info;
  XKeyEvent *ev;
  KeySym keysym;
  ev = (XKeyEvent *)(cb->event);
  keysym = XKeycodeToKeysym(XtDisplay(w),
			    (int)(ev->keycode),
			    (ev->state & snd_ShiftMask) ? 1 : 0);
  ss->mx_sp = sp; 
  if ((ev->state & snd_MetaMask) && 
      ((keysym == snd_K_p) || (keysym == snd_K_P) || (keysym == snd_K_n) || (keysym == snd_K_N)))
    {
      restore_filter_string(sp, (keysym == snd_K_p) || (keysym == snd_K_P));
      return;
    }
  str = XmTextGetString(w);
  if ((str) && (*str)) remember_filter_string(sp, str);

  if (sp->filter_control_envelope) sp->filter_control_envelope = free_env(sp->filter_control_envelope);
  sp->filter_control_envelope = string_to_env(str);
  if (str) XtFree(str);
  if (!(sp->filter_control_envelope)) /* maybe user cleared text field? */
    sp->filter_control_envelope = default_env(sp->filter_control_xmax, 1.0);
  str = XmTextGetString(FILTER_ORDER_TEXT(sp));
  if ((str) && (*str))
    {
      int order;
      order = string_to_int(str);
      if (order & 1) order++;
      if (order <= 0) order = 2;
      sp->filter_control_order = order;
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
      int order;
      order = string_to_int(str);
      if (order & 1) order++;
      if (order <= 0) order = 2;
      sp->filter_control_order = order;
      sp->filter_control_changed = true;
      display_filter_env(sp);
      XtFree(str);
    }
  filter_textfield_deactivate(sp);
}

void filter_env_changed(snd_info *sp, env *e)
{
  /* turn e back into a string for textfield widget */
  if (!(IS_PLAYER(sp)))
    {
      char *tmpstr = NULL;
      XmTextSetString(FILTER_COEFFS_TEXT(sp), tmpstr = env_to_string(e));
      if (tmpstr) FREE(tmpstr);
      (SOUND_ENV_EDITOR(sp))->edited = true;
      display_filter_env(sp);
    }
  sp->filter_control_changed = true;
}

/* ---------------- PLAY BUTTON ---------------- */
void set_play_button(snd_info *sp, bool val)
{
  if ((sp->sgx) && (!(IS_PLAYER(sp))))
    {
      XmToggleButtonSetState(PLAY_BUTTON(sp), (Boolean)val, false);
      set_file_browser_play_button(sp->short_filename, val);
      set_open_file_play_button(val);
    }
}

static void play_button_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_info *sp = (snd_info *)context;
  chan_info *cp;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  XButtonEvent *ev;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  ev = (XButtonEvent *)(cb->event);
  if (sp->playing) 
    stop_playing_sound(sp, PLAY_BUTTON_UNSET);
  if (sp->cursor_follows_play != FOLLOW_ALWAYS)         /* can be set in init file */
    {
      if ((cb->set) && (ev->state & (snd_ControlMask | snd_MetaMask)))
	sp->cursor_follows_play = FOLLOW_ONCE;
      else sp->cursor_follows_play = DONT_FOLLOW;
    }
  set_file_browser_play_button(sp->short_filename, cb->set);
  cp = any_selected_channel(sp);
  goto_graph(cp);
  if (cb->set) 
    {
      XtVaSetValues(w, XmNselectColor, ((sp->cursor_follows_play != DONT_FOLLOW) ? ((ss->sgx)->green) : ((ss->sgx)->pushed_button_color)), NULL);
      play_sound(sp, 0, NO_END_SPECIFIED, IN_BACKGROUND, AT_CURRENT_EDIT_POSITION);
    }
}

typedef struct {bool pausing; } pause_data;

static void set_play_button_pause(snd_info *sp, void *ptr)
{
  if ((sp->playing) && (!(IS_PLAYER(sp))))
    {
      pause_data *pd = (pause_data *)ptr;
      Widget w;
      w = PLAY_BUTTON(sp);
      if (pd->pausing)
	XtVaSetValues(w, XmNselectColor, (ss->sgx)->red, NULL);
      else XtVaSetValues(w, XmNselectColor, ((sp->cursor_follows_play != DONT_FOLLOW) ? ((ss->sgx)->green) : ((ss->sgx)->pushed_button_color)), NULL);
    }
}

void play_button_pause(bool pausing)
{
  pause_data *pd;
  pd = (pause_data *)CALLOC(1, sizeof(pause_data));
  pd->pausing = pausing;
  for_each_sound(set_play_button_pause, (void *)pd);
  FREE(pd);
}

void set_control_panel_play_button(snd_info *sp)
{
  if ((sp) && (sp->sgx) && (PLAY_BUTTON(sp)))
    {
      set_toggle_button(PLAY_BUTTON(sp), false, false, sp);
      XtVaSetValues(PLAY_BUTTON(sp), XmNselectColor, (ss->sgx)->pushed_button_color, NULL);
    }
}


static void play_arrow_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  bool dir;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  dir = (bool)(cb->set);
  if (dir) sp->speed_control_direction = -1; else sp->speed_control_direction = 1;
}


/* ---------------- SYNC BUTTON ---------------- */

static void set_sync_color(snd_info *sp)
{
  Widget syb;
  syb = SYNC_BUTTON(sp);
  switch (sp->sync)
    {
    case 1: case 0: XtVaSetValues(syb, XmNselectColor, (ss->sgx)->pushed_button_color, NULL); break;
    case 2:         XtVaSetValues(syb, XmNselectColor, (ss->sgx)->green, NULL);               break;
    case 3:         XtVaSetValues(syb, XmNselectColor, (ss->sgx)->yellow, NULL);              break;
    case 4:         XtVaSetValues(syb, XmNselectColor, (ss->sgx)->red, NULL);                 break;
    default:        XtVaSetValues(syb, XmNselectColor, (ss->sgx)->black, NULL);               break;
    }
}

void syncb(snd_info *sp, int on)
{
  sp->sync = on;
  if (!(IS_PLAYER(sp)))
    {
      set_sync_color(sp);
      XmToggleButtonSetState(SYNC_BUTTON(sp), (Boolean)on, false);
    }
}

static void sync_button_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  XButtonEvent *ev;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
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
  if (sp->sync != 0) 
    {
      chan_info *cp;
      set_sync_color(sp);
      cp = sp->lacp;
      if (cp == NULL) cp = any_selected_channel(sp);
      goto_graph(cp);
      if (cp->cursor_on) cursor_moveto(cp, CURSOR(cp));
      apply_x_axis_change(cp->axis, cp);
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
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
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


static void minibuffer_click_callback(Widget w, XtPointer context, XtPointer info)
{
  /* can be response to various things */
  snd_info *sp = (snd_info *)context;
  XmAnyCallbackStruct *cb = (XmAnyCallbackStruct *)info;
  XKeyEvent *ev;
  KeySym keysym;
  ev = (XKeyEvent *)(cb->event);
  keysym = XKeycodeToKeysym(XtDisplay(w),
			    (int)(ev->keycode),
			    (ev->state & ShiftMask) ? 1 : 0);
  ss->mx_sp = sp; 
  snd_minibuffer_activate(sp, keysym, (ev->state & snd_MetaMask));
}


/* apply is only safe if the DAC is currently inactive and remains safe only
 * if all other apply buttons are locked out (and play).
 */

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

static int outer_panes = 0;
static int *inner_panes = NULL;
static int *outer_sizes = NULL;
static int **inner_sizes = NULL;

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
      if (strcmp(call_data->params[0], "Start") == 0)
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
	  if (outer_panes > 0)
	    {
	      inner_panes = (int *)CALLOC(outer_panes, sizeof(int));
	      outer_sizes = (int *)CALLOC(outer_panes, sizeof(int));
	      inner_sizes = (int **)CALLOC(outer_panes, sizeof(int *));
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
		      child = w_snd_pane(sp);
		      inner_panes[outer_ctr] = sp->nchans;
		      inner_sizes[outer_ctr] = (int *)CALLOC(sp->nchans, sizeof(int));
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
	  if ((outer_panes > 0) && 
	      (strcmp(call_data->params[0], "Commit") == 0))
	    {
	      int outer_ctr = 0, cur_outer_size = 0;
	      for (i = 0; i < ss->max_sounds; i++)
		{
		  sp = ss->sounds[i];
		  if ((sp) &&
		      (sp->inuse == SOUND_NORMAL) &&
		      (sp->nchans > 1) &&
		      (sp->channel_style == CHANNELS_SEPARATE))
		    {
		      XtVaGetValues(w_snd_pane(sp), XmNheight, &cur_outer_size, NULL);
		      if ((cur_outer_size > 40) && 
			  (abs(cur_outer_size - outer_sizes[outer_ctr]) > (sp->nchans * 2)))
			{
			  /* this pane has multiple chans and its size has changed enough to matter */
			  int total_inner = 0, diff, size;
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
		  FREE(inner_sizes[i]);
	      FREE(inner_panes);
	      FREE(inner_sizes);
	      FREE(outer_sizes);
	      outer_panes = 0;
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
      sashes = (Widget *)CALLOC(16, sizeof(Widget));
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
	  sashes = (Widget *)REALLOC(sashes, sashes_size * 2 * sizeof(Widget));
	  for (i = sashes_size; i < sashes_size * 2; i++) sashes[i] = NULL;
	  loc = sashes_size;
	  sashes_size *= 2;
	}
    }
  sashes[loc] = w;
  XtAddCallback(w, XmNcallback, watch_sash, NULL);
}

static void add_watchers(Widget w)
{
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
#if HAVE_ACCESS
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

#if HAVE_XPM

static Pixmap mini_lock = 0;
static Pixmap blank_pixmap = 0;
static bool mini_lock_allocated = false;
static Pixmap mini_bombs[NUM_BOMBS];
static Pixmap mini_glasses[NUM_GLASSES];

void snd_file_lock_icon(snd_info *sp, bool on)
{
  if (mini_lock) 
    {
      snd_context *sx;
      sx = sp->sgx;
      if (on)
	sx->file_pix = mini_lock;
      else sx->file_pix = blank_pixmap;
      XtVaSetValues(NAME_ICON(sp), XmNlabelPixmap, sx->file_pix, NULL);
    }
  /* these Pixmaps can be null if the colormap is screwed up */
}

#define BOMB_TIME 200

static void show_bomb_icon(snd_info *sp, bool on)
{
  if (sp->bomb_ctr >= NUM_BOMBS) sp->bomb_ctr = 0;
  if (mini_bombs[sp->bomb_ctr]) 
    {
      snd_context *sx;
      sx = sp->sgx;
      if (sx)
	{
	  if (on)
	    sx->file_pix = mini_bombs[sp->bomb_ctr];
	  else sx->file_pix = blank_pixmap;
	  XtVaSetValues(NAME_ICON(sp), XmNlabelPixmap, sx->file_pix, NULL);
	}
    }
}

void x_bomb(snd_info *sp, bool on)
{
  show_bomb_icon(sp, on);
  if (on) 
    sp->bomb_ctr++; 
  else sp->bomb_ctr = 0;
}

static void inc_bomb(snd_info *sp, void *ptr)
{
  if (sp)
    {
      if (sp->need_update)
	{
	  int *buf;
	  buf = (int *)ptr;
	  buf[0]++;
	  show_bomb_icon(sp, sp->bomb_ctr);
	  sp->bomb_ctr++;
	}
    }
}

static bool bomb_in_progress = false;

static void bomb_check(XtPointer context, XtIntervalId *id)
{
  int incs[1];
  incs[0] = 0;
  for_each_sound(inc_bomb, (void *)incs);
  if (incs[0] > 0)
    XtAppAddTimeOut(MAIN_APP(ss),
		    (unsigned long)BOMB_TIME,
		    (XtTimerCallbackProc)bomb_check,
		    context);
  else bomb_in_progress = false;
}

void snd_file_bomb_icon(snd_info *sp, bool on)
{
  if ((on) && (!bomb_in_progress))
    {
      bomb_in_progress = true;
      XtAppAddTimeOut(MAIN_APP(ss),
		      (unsigned long)BOMB_TIME,
		      (XtTimerCallbackProc)bomb_check,
		      (void *)sp);
    }
}

static void snd_file_glasses_icon(snd_info *sp, bool on, int glass)
{
  Widget w;
  snd_context *sx;
  sx = sp->sgx;
  if (!sx) return;
  w = NAME_ICON(sp);
  if (on)
    {
      if (mini_glasses[glass])
	{
	  XtVaSetValues(w, XmNlabelPixmap, mini_glasses[glass], NULL);
	  XmUpdateDisplay(w);
	}
    }
  else
    {
      XtVaSetValues(w, XmNlabelPixmap, sx->file_pix, NULL);
      XmUpdateDisplay(w);
    }
}

#if HAVE_XPM
static char *bits_to_string(char **icon)
{
  /* show first few lines */
  char *buf;
  buf = (char *)CALLOC(128, sizeof(char));
  mus_snprintf(buf, 128, "\n%s\n%s\n%s...", icon[0], icon[1], icon[2]);
  return(buf);
}
#endif

static void allocate_icons(Widget w)
{ 
  Pixmap shape1, shape2, shape3; 
  XpmAttributes attributes; 
  XpmColorSymbol symbols[1];
  int scr, pixerr = XpmSuccess;
  Display *dp;
  Drawable wn;
  dp = XtDisplay(w);
  wn = XtWindow(w);
  scr = DefaultScreen(dp);
  XtVaGetValues(w, XmNdepth, &attributes.depth, XmNcolormap, &attributes.colormap, NULL);
  attributes.visual = DefaultVisual(dp, scr);
  symbols[0].name = "basiccolor";
  symbols[0].value = NULL;
  symbols[0].pixel = (ss->sgx)->basic_color;
  attributes.colorsymbols = symbols;
  attributes.numsymbols = 1;
  attributes.valuemask = XpmColorSymbols | XpmDepth | XpmColormap | XpmVisual;
  pixerr = XpmCreatePixmapFromData(dp, wn, mini_lock_bits(), &mini_lock, &shape1, &attributes);
  if (pixerr != XpmSuccess) 
    snd_error("lock pixmap trouble: %s from %s\n", XpmGetErrorString(pixerr), bits_to_string(mini_lock_bits()));
  else
    {
#if 0
      Pixmap bgpx = 0;
      XtVaGetValues(w, XmNbackgroundPixmap, &bgpx, NULL);
      if (bgpx) blank_pixmap = bgpx;
      /* this appears to work, but is not compatible with the pixmap changes when basic-color is set. */
#endif
      pixerr = XpmCreatePixmapFromData(dp, wn, blank_bits(), &blank_pixmap, &shape1, &attributes);
      if (pixerr != XpmSuccess) 
	snd_error("blank pixmap trouble: %s from %s\n", XpmGetErrorString(pixerr), bits_to_string(blank_bits()));
      else
	{
	  int k;
	  for (k = 0; k < NUM_BOMBS; k++)
	    {
	      pixerr = XpmCreatePixmapFromData(dp, wn, mini_bomb_bits(k), &(mini_bombs[k]), &shape2, &attributes);
	      if (pixerr != XpmSuccess) 
		{
		  snd_error("bomb pixmap trouble: %s from %s\n", XpmGetErrorString(pixerr), bits_to_string(mini_bomb_bits(k)));
		  break;
		}
	      pixerr = XpmCreatePixmapFromData(dp, wn, mini_glass_bits(k), &(mini_glasses[k]), &shape3, &attributes);
	      if (pixerr != XpmSuccess) 
		{
		  snd_error("glass pixmap trouble: %s from %s\n", XpmGetErrorString(pixerr), bits_to_string(mini_glass_bits(k))); 
		  break;
		}
	    }
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
  data = (char *)calloc((width + 1) * (height + 1) * depth_bytes, sizeof(char)); /* not CALLOC since X will free this */
  /* there's overflow in X here, apparently -- the +1's fix it according to valgrind */
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
  XDestroyImage(before);  /* frees data as well, or so claims the documentation */
  XFreeGC(dp, draw_gc);
}

void make_sound_icons_transparent_again(Pixel old_color, Pixel new_color)
{
  int i;
  if (!mini_lock_allocated) allocate_icons(MAIN_SHELL(ss));
  change_pixmap_background(MAIN_SHELL(ss), mini_lock, old_color, new_color, 16, 14);
  change_pixmap_background(MAIN_SHELL(ss), blank_pixmap, old_color, new_color, 16, 14);
  for (i = 0; i < NUM_BOMBS; i++)
    change_pixmap_background(MAIN_SHELL(ss), mini_bombs[i], old_color, new_color, 16, 14);
  for (i = 0; i < NUM_GLASSES; i++)
    change_pixmap_background(MAIN_SHELL(ss), mini_glasses[i], old_color, new_color, 16, 14);
}

#else
void make_icons_transparent_again(Pixel old_color, Pixel new_color) {}
void snd_file_lock_icon(snd_info *sp, bool on) {}
void snd_file_bomb_icon(snd_info *sp, bool on) 
{
  if (on)
    report_in_minibuffer(sp, _("%s has changed since we last read it!"), sp->short_filename);
}
/* static void snd_file_glasses_icon(snd_info *sp, bool on, int glass) {} */
void x_bomb(snd_info *sp, bool on) {}
#endif

static Pixmap spd_r, spd_l;
static bool spd_ok = false;

static void close_sound_dialog(Widget w, XtPointer context, XtPointer info) 
{
  snd_info *sp = (snd_info *)context;
  if (sp) snd_close_file(sp);
}

snd_info *add_sound_window(char *filename, bool read_only)
{  
  snd_info *sp = NULL, *osp;
  file_info *hdr = NULL;
  Widget *sw;
  XmString s1;
  int snd_slot, nchans = 1, i, k, n, old_chans;
  bool make_widgets, need_colors;
  Arg args[32];
  char *old_name = NULL, *title;
  Dimension app_y, app_dy, screen_y, chan_min_y;
  /* these dimensions are used to try to get a reasonable channel graph size without falling off the screen bottom */
  Pixmap rb, lb;
  int depth;
  bool free_filename = false;
  Widget form;
  XtCallbackList n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12;
  snd_context *sx;
  Atom sound_delete;
  static bool first_window = true;
  errno = 0;
  hdr = make_file_info(filename);
  if (!hdr) return(NULL);
  if (ss->pending_change) 
    {
      old_name = filename;
      filename = ss->pending_change;
      free_filename = true;
      ss->pending_change = NULL;
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
  sx = sp->sgx;
#if HAVE_XPM
  sx->file_pix = blank_pixmap;
#else
  sx->file_pix = (Pixmap)0;
#endif
  sp->bomb_ctr = 0;
  if (sx->snd_widgets == NULL) 
    sx->snd_widgets = (Widget *)CALLOC(NUM_SND_WIDGETS, sizeof(Widget));
  sw = sx->snd_widgets;
  if ((!make_widgets) && (old_chans < nchans))
    {
      for (i = old_chans; i < nchans; i++) 
	add_channel_window(sp, i, chan_min_y, 1, NULL, WITH_FW_BUTTONS, WITH_EVENTS);
    }

  if (make_widgets)
    {
      need_colors = (!(ss->using_schemes));

      if ((sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS))
	{
	  title = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
	  mus_snprintf(title, PRINT_BUFFER_SIZE, "%d: %s", snd_slot, sp->short_filename);
	  if (sx->dialog == NULL)
	    {
	      n = 0;
	      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
	      XtSetArg(args[n], XmNautoUnmanage, false); n++;
	      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
	      XtSetArg(args[n], XmNnoResize, false); n++;
	      XtSetArg(args[n], XmNtransient, false); n++;
	      sx->dialog = XtCreatePopupShell(title, xmDialogShellWidgetClass, MAIN_SHELL(ss), args, n);
	      /* using popup shell here gets around the problem that the shell passes resize requests to all its children
	       * -- as a popup, it's not considered a child, but that means we don't inherit things like popup menus from 
	       * the main shell.
	       */
	      sound_delete = XmInternAtom(XtDisplay(sx->dialog), "WM_DELETE_WINDOW", false);
	      XmAddWMProtocolCallback(sx->dialog, sound_delete, close_sound_dialog, (XtPointer)sp);
	    }
	  else XtVaSetValues(sx->dialog, XmNtitle, title, NULL);
	  FREE(title);
	  if (!XtIsManaged(sx->dialog)) XtManageChild(sx->dialog);
	}

      n = 0;      
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      n = attach_all_sides(args, n);
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNsashIndent, ss->channel_sash_indent); n++;
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
	  XtSetArg(args[n], XmNpositionIndex, snd_slot); n++;
	}
      XtSetArg(args[n], XmNuserData, sp->index); n++;

      if (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS)
	sw[W_pane] = XtCreateManagedWidget("snd-pane", xmPanedWindowWidgetClass, sx->dialog, args, n);
      else sw[W_pane] = XtCreateManagedWidget("snd-pane", xmPanedWindowWidgetClass, SOUND_PANE(ss), args, n);

      XtAddEventHandler(sw[W_pane], KeyPressMask, false, graph_key_press, (XtPointer)sp);
      /* if user clicks in controls, then starts typing, try to send key events to current active channel */
      /* all widgets in the control-pane that would otherwise intercept the key events get this event handler */

      for (i = 0; i < nchans; i++)
	add_channel_window(sp, i, chan_min_y, 0, NULL, WITH_FW_BUTTONS, WITH_EVENTS);
      
      n = 0;      
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNpaneMinimum, ctrls_height); n++;
      XtSetArg(args[n], XmNpaneMaximum, ctrls_height); n++;
      sw[W_control_panel] = XtCreateManagedWidget ("snd-ctrls", xmFormWidgetClass, sw[W_pane], args, n);
      XtAddEventHandler(sw[W_control_panel], KeyPressMask, false, graph_key_press, (XtPointer)sp);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      sw[W_name_form] = XtCreateManagedWidget("snd-name-form", xmFormWidgetClass, sw[W_control_panel], args, n);
      XtAddEventHandler(sw[W_name_form], KeyPressMask, false, graph_key_press, (XtPointer)sp);

      n = 0;      
      s1 = XmStringCreate(shortname_indexed(sp), XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      sw[W_name] = XtCreateManagedWidget ("snd-name", xmPushButtonWidgetClass, sw[W_name_form], args, n);
      XtAddEventHandler(sw[W_name], KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_name], XmNactivateCallback, name_click_callback, (XtPointer)sp);
      XmStringFree(s1);

#if HAVE_XPM
      if (!mini_lock_allocated) 
	{
	  allocate_icons(sw[W_name]);
	  if (ss->using_schemes) 
	    {
	      Pixel new_color;
	      XtVaGetValues(sw[W_name], XmNbackground, &new_color, NULL);
	      make_sound_icons_transparent_again(ss->sgx->basic_color, new_color);
	    }
	}
#endif
      n = 0;      
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_name]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
#if HAVE_XPM
      if (blank_pixmap)
	{
	  /* if xpm failed (blank_pixmap == 0), this can cause X to kill Snd! */
	  XtSetArg(args[n], XmNlabelType, XmPIXMAP); n++;
	  XtSetArg(args[n], XmNlabelPixmap, blank_pixmap); n++;
	}
#endif
      sw[W_name_icon] = XtCreateManagedWidget("", xmLabelWidgetClass, sw[W_name_form], args, n);

      n = 0;      
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, sw[W_name]); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_name_icon]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
      XtSetArg(args[n], XmNwidth, 20); n++; /* was 40 */
      XtSetArg(args[n], XmNseparatorType, XmSHADOW_ETCHED_IN); n++;
      sw[W_info_sep] = XtCreateManagedWidget ("snd-info-sep", xmSeparatorWidgetClass, sw[W_name_form], args, n);

      n = 0;
      s1 = XmStringCreate("     ", XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, sw[W_info_sep]); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_info_sep]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      sw[W_info_label] = XtCreateManagedWidget ("snd-info-label", xmLabelWidgetClass, sw[W_name_form], args, n);
      XmStringFree(s1);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_info_label]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNresizeWidth, true); n++;
      XtSetArg(args[n], XmNmarginHeight, 1); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNcolumns, 30); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      sw[W_info] = make_textfield_widget("snd-info", sw[W_name_form], args, n, ACTIVATABLE, add_completer_func(info_completer));
      XtAddCallback(sw[W_info], XmNactivateCallback, minibuffer_click_callback, (XtPointer)sp);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
#ifdef TOGGLE_MARGIN
      XtSetArg(args[n], XmNmarginHeight, TOGGLE_MARGIN); n++;
      XtSetArg(args[n], XmNmarginTop, TOGGLE_MARGIN); n++;
#endif
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      /* in Motif 2.2 this sets up a tooltip:
	XtSetArg(args[n], XmNtoolTipString, XmStringCreate("play this sound", XmFONTLIST_DEFAULT_TAG)); n++;
      */
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNselectColor, (ss->sgx)->pushed_button_color); n++;}
      sw[W_play] = make_togglebutton_widget(_("play"), sw[W_name_form], args, n);
      XtAddCallback(sw[W_play], XmNvalueChangedCallback, play_button_callback, (XtPointer)sp);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
#ifdef TOGGLE_MARGIN
      XtSetArg(args[n], XmNmarginHeight, TOGGLE_MARGIN); n++;
      XtSetArg(args[n], XmNmarginTop, TOGGLE_MARGIN); n++;
#endif
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, sw[W_play]); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNselectColor, (ss->sgx)->pushed_button_color); n++;}
      sw[W_sync] = make_togglebutton_widget(_("sync"), sw[W_name_form], args, n);
      XtAddEventHandler(sw[W_sync], KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_sync], XmNvalueChangedCallback, sync_button_callback, (XtPointer)sp);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, sw[W_sync]); n++;
#ifdef TOGGLE_MARGIN
      XtSetArg(args[n], XmNmarginHeight, TOGGLE_MARGIN); n++;
      XtSetArg(args[n], XmNmarginTop, TOGGLE_MARGIN); n++;
#endif
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, sw[W_sync]); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNselectColor, (ss->sgx)->pushed_button_color); n++;}
      sw[W_unite] = make_togglebutton_widget(_("unite"), sw[W_name_form], args, n);
      XtAddEventHandler(sw[W_unite], KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_unite], XmNvalueChangedCallback, unite_button_callback, (XtPointer)sp);

      n = 0;
      XtVaSetValues(sw[W_control_panel], XmNskipAdjust, true, NULL);

      /* tried a dial widget here, but it didn't seem to fit and was harder to manipulate and read than a scale */
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_name_form]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNmargin, LINE_MARGIN); n++;
      XtSetArg(args[n], XmNheight, LINE_MARGIN); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      sw[W_amp_separator] = XtCreateManagedWidget ("snd-amp-sep", xmSeparatorWidgetClass, sw[W_control_panel], args, n);
      
      /* if control-panel */
      n = 0;      
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_amp_separator]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      sw[W_amp_form] = XtCreateManagedWidget ("snd-amp", xmFormWidgetClass, sw[W_control_panel], args, n);
      XtAddEventHandler(sw[W_amp_form], KeyPressMask, false, graph_key_press, (XtPointer)sp);

      n = 0;      
      /* AMP */
      s1 = XmStringCreate(_("amp:"), XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
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
      sw[W_amp_label] = make_pushbutton_widget ("amp-label", sw[W_amp_form], args, n);
      XtAddEventHandler(sw[W_amp_label], KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_amp_label], XmNactivateCallback, amp_click_callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;
      s1 = XmStringCreate("1.0   ", XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_amp_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_amp_label]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginRight, 3); n++;
      sw[W_amp_number] = XtCreateManagedWidget ("amp-number", xmLabelWidgetClass, sw[W_amp_form], args, n);
      XmStringFree(s1);

      n = 0;      
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->position_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_amp_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_amp_number]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNvalue, amp_to_scroll(sp->amp_control_min, 1.0, sp->amp_control_max)); n++;
      XtSetArg(args[n], XmNdragCallback, n1 = make_callback_list(amp_drag_callback, (XtPointer)sp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n2 = make_callback_list(amp_valuechanged_callback, (XtPointer)sp)); n++;
      sw[W_amp] = XtCreateManagedWidget("amp", xmScrollBarWidgetClass, sw[W_amp_form], args, n);
      XtAddEventHandler(sw[W_amp], KeyPressMask, false, graph_key_press, (XtPointer)sp);

      n = 0;
      /* SPEED */
      s1 = XmStringCreate(_("speed:"), XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_amp_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++; 
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      sw[W_speed_label] = make_pushbutton_widget ("speed-label", sw[W_amp_form], args, n);
      XtAddEventHandler(sw[W_speed_label], KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_speed_label], XmNactivateCallback, speed_click_callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;
      s1 = initial_speed_label(sp->speed_control_style);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_speed_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_speed_label]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++; 
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNmarginRight, 3); n++;
      sw[W_speed_number] = XtCreateManagedWidget ("speed-number", xmLabelWidgetClass, sw[W_amp_form], args, n);
      XmStringFree(s1);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_speed_label]); n++;
      XtSetArg(args[n], XmNindicatorOn, false); n++;
      XtSetArg(args[n], XmNlabelType, XmPIXMAP); n++;
      XtSetArg(args[n], XmNmarginHeight, 0); n++;
      XtSetArg(args[n], XmNmarginWidth, 0); n++;
      XtSetArg(args[n], XmNmarginTop, 0); n++;
      XtSetArg(args[n], XmNtopOffset, 0); n++;
      sw[W_speed_arrow] = make_togglebutton_widget("dir", sw[W_amp_form], args, n);
      form = sw[W_speed_arrow];
      if (!spd_ok)
	{
	  rb = XCreateBitmapFromData(XtDisplay(form), RootWindowOfScreen(XtScreen(form)), (const char *)speed_r_bits1, 16, 12);
	  lb = XCreateBitmapFromData(XtDisplay(form), RootWindowOfScreen(XtScreen(form)), (const char *)speed_l_bits1, 16, 12);
	  XtVaGetValues(form, XmNdepth, &depth, NULL);
	  spd_r = XCreatePixmap(XtDisplay(form), RootWindowOfScreen(XtScreen(form)), 16, 12, depth);
	  spd_l = XCreatePixmap(XtDisplay(form), RootWindowOfScreen(XtScreen(form)), 16, 12, depth);
	  XCopyPlane(XtDisplay(form), rb, spd_r, (ss->sgx)->fltenv_basic_gc, 0, 0, 16, 12, 0, 0, 1);
	  XCopyPlane(XtDisplay(form), lb, spd_l, (ss->sgx)->fltenv_basic_gc, 0, 0, 16, 12, 0, 0, 1);
	  XFreePixmap(XtDisplay(form), rb);
	  XFreePixmap(XtDisplay(form), lb);
	  spd_ok = true;
	}
      XtVaSetValues(form, XmNselectPixmap, spd_l, XmNlabelPixmap, spd_r, NULL);
      XtAddEventHandler(sw[W_speed_arrow], KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_speed_arrow], XmNvalueChangedCallback, play_arrow_callback, (XtPointer)sp);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->position_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_speed_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_speed_number]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, sw[W_speed_arrow]); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNvalue, speed_to_scroll(sp->speed_control_min, 1.0, sp->speed_control_max)); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNdragCallback, n3 = make_callback_list(speed_drag_callback, (XtPointer)sp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n4 = make_callback_list(speed_valuechanged_callback, (XtPointer)sp)); n++;
      sw[W_speed] = XtCreateManagedWidget("speed-scroll", xmScrollBarWidgetClass, sw[W_amp_form], args, n);
      XtAddEventHandler(sw[W_speed], KeyPressMask, false, graph_key_press, (XtPointer)sp);

      n = 0;
      /* EXPAND */
      s1 = XmStringCreate(_("expand:"), XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_speed_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      sw[W_expand_label] = make_pushbutton_widget ("expand-label", sw[W_amp_form], args, n);
      XtAddEventHandler(sw[W_expand_label], KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_expand_label], XmNactivateCallback, expand_click_callback, (XtPointer)sp);
      XmStringFree(s1);
      
      n = 0;
      s1 = XmStringCreate("1.0   ", XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_expand_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_expand_label]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginRight, 3); n++;
      sw[W_expand_number] = XtCreateManagedWidget ("expand-number", xmLabelWidgetClass, sw[W_amp_form], args, n);
      XmStringFree(s1);

      n = 0;
      s1 = XmStringCreate("", XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_expand_label]); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNmarginWidth, 0); n++;
      XtSetArg(args[n], XmNtopOffset, 1); n++;
      XtSetArg(args[n], XmNspacing, 0); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNselectColor, (ss->sgx)->pushed_button_color); n++;}
      sw[W_expand_button] = make_togglebutton_widget("expoff", sw[W_amp_form], args, n);
      XtAddEventHandler(sw[W_expand_button], KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_expand_button], XmNvalueChangedCallback, expand_button_callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_expand_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_expand_number]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, sw[W_expand_button]); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNvalue, expand_to_scroll(sp->expand_control_min, 1.0, sp->expand_control_max)); n++; 
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNdragCallback, n5 = make_callback_list(expand_drag_callback, (XtPointer)sp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n6 = make_callback_list(expand_valuechanged_callback, (XtPointer)sp)); n++;
      sw[W_expand] = XtCreateManagedWidget("expand-scroll", xmScrollBarWidgetClass, sw[W_amp_form], args, n);
      XtAddEventHandler(sw[W_expand], KeyPressMask, false, graph_key_press, (XtPointer)sp);


      /* CONTRAST */
      n = 0;
      s1 = XmStringCreate(_("contrast:"), XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_expand_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      sw[W_contrast_label] = make_pushbutton_widget ("contrast-label", sw[W_amp_form], args, n);
      XtAddEventHandler(sw[W_contrast_label], KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_contrast_label], XmNactivateCallback, contrast_click_callback, (XtPointer)sp);
      XmStringFree(s1);
      
      n = 0;
      s1 = XmStringCreate("1.0   ", XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_contrast_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_contrast_label]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginRight, 3); n++;
      sw[W_contrast_number] = XtCreateManagedWidget ("contrast-number", xmLabelWidgetClass, sw[W_amp_form], args, n);
      XmStringFree(s1);
      
      n = 0;
      s1 = XmStringCreate("", XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_contrast_label]); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNmarginWidth, 0); n++;
      XtSetArg(args[n], XmNtopOffset, 1); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNspacing, 0); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNselectColor, (ss->sgx)->pushed_button_color); n++;}
      sw[W_contrast_button] = make_togglebutton_widget("conoff", sw[W_amp_form], args, n);
      XtAddEventHandler(sw[W_contrast_button], KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_contrast_button], XmNvalueChangedCallback, contrast_button_callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_contrast_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_contrast_number]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, sw[W_contrast_button]); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNvalue, 0); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNdragCallback, n7 = make_callback_list(contrast_drag_callback, (XtPointer)sp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n8 = make_callback_list(contrast_valuechanged_callback, (XtPointer)sp)); n++;
      sw[W_contrast] = XtCreateManagedWidget("contrast-scroll", xmScrollBarWidgetClass, sw[W_amp_form], args, n);
      XtAddEventHandler(sw[W_contrast], KeyPressMask, false, graph_key_press, (XtPointer)sp);

      /* REVERB */
      /* REVSCL */
      n = 0;
      s1 = XmStringCreate(_("reverb:"), XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_contrast_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      sw[W_revscl_label] = make_pushbutton_widget ("revscl-label", sw[W_amp_form], args, n);
      XtAddEventHandler(sw[W_revscl_label], KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_revscl_label], XmNactivateCallback, revscl_click_callback, (XtPointer)sp);
      XmStringFree(s1);
      
      n = 0;
      s1 = XmStringCreate("0.0     ", XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_revscl_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_revscl_label]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNmarginRight, 3); n++;
      sw[W_revscl_number] = XtCreateManagedWidget ("revscl-number", xmLabelWidgetClass, sw[W_amp_form], args, n);
      XmStringFree(s1);
      
      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_revscl_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_revscl_number]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 60); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNvalue, 0); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNdragCallback, n9 = make_callback_list(revscl_drag_callback, (XtPointer)sp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n10 = make_callback_list(revscl_valuechanged_callback, (XtPointer)sp)); n++;
      sw[W_revscl] = XtCreateManagedWidget("revscl-scroll", xmScrollBarWidgetClass, sw[W_amp_form], args, n);
      XtAddEventHandler(sw[W_revscl], KeyPressMask, false, graph_key_press, (XtPointer)sp);

      /* REVOFF */
      n = 0;
      s1 = XmStringCreate("", XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_revscl_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, sw[W_contrast_button]); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNmarginWidth, 0); n++;
      XtSetArg(args[n], XmNtopOffset, 1); n++;
      XtSetArg(args[n], XmNspacing, 0); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNselectColor, (ss->sgx)->pushed_button_color); n++;}
      sw[W_reverb_button] = make_togglebutton_widget("revoff", sw[W_amp_form], args, n);
      XtAddEventHandler(sw[W_reverb_button], KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_reverb_button], XmNvalueChangedCallback, reverb_button_callback, (XtPointer)sp);
      XmStringFree(s1);


      /* REVLEN */
      n = 0;
      s1 = XmStringCreate(_("len:"), XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_revscl]); n++;
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
      sw[W_revlen_label] = make_pushbutton_widget("revlen-label", sw[W_amp_form], args, n);
      XtAddEventHandler(sw[W_revlen_label], KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_revlen_label], XmNactivateCallback, revlen_click_callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;
      s1 = XmStringCreate("1.0 ", XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_revlen_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_revlen_label]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNmarginRight, 3); n++;
      sw[W_revlen_number] = XtCreateManagedWidget("revlen-number", xmLabelWidgetClass, sw[W_amp_form], args, n);
      XmStringFree(s1);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_revlen_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_revlen_number]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, sw[W_reverb_button]); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNvalue, revlen_to_scroll(sp->reverb_control_length_min, 1.0, sp->reverb_control_length_max)); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNdragCallback, n11 = make_callback_list(revlen_drag_callback, (XtPointer)sp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n12 = make_callback_list(revlen_valuechanged_callback, (XtPointer)sp)); n++;
      sw[W_revlen] = XtCreateManagedWidget("revlen-scroll", xmScrollBarWidgetClass, sw[W_amp_form], args, n);
      XtAddEventHandler(sw[W_revlen], KeyPressMask, false, graph_key_press, (XtPointer)sp);


      /* FILTER */
      n = 0;
      s1 = XmStringCreate(_("filter:"), XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_revscl_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
#ifdef SGI
      sw[W_filter_label] = XtCreateManagedWidget ("filter-label", xmPushButtonWidgetClass, sw[W_amp_form], args, n);
#else
      sw[W_filter_label] = XtCreateManagedWidget ("filter-label", xmLabelWidgetClass, sw[W_amp_form], args, n);
#endif
      XmStringFree(s1);

      /* filter order */
      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNresizeWidth, false); n++;
      XtSetArg(args[n], XmNcolumns, 3); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_filter_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_filter_label]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      sw[W_filter_order] = make_textfield_widget("filter-order", sw[W_amp_form], args, n, ACTIVATABLE, NO_COMPLETER);
      XmTextSetString(sw[W_filter_order], " 20");
      XtAddCallback(sw[W_filter_order], XmNactivateCallback, filter_order_activate_callback, (XtPointer)sp);

      #define ARROW_SIZE 12

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_filter_order]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_filter_order]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNheight, ARROW_SIZE); n++;
      XtSetArg(args[n], XmNwidth, ARROW_SIZE); n++;
      XtSetArg(args[n], XmNborderWidth, 0); n++;
      XtSetArg(args[n], XmNmarginWidth, 0); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;}
      sw[W_filter_order_down] = make_pushbutton_widget("", sw[W_amp_form], args, n);
      XtAddEventHandler(sw[W_filter_order_down], KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_filter_order_down], XmNactivateCallback, filter_order_down_callback, (XtPointer)sp);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_filter_order_down]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_filter_order]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNheight, ARROW_SIZE); n++;
      XtSetArg(args[n], XmNwidth, ARROW_SIZE); n++;
      XtSetArg(args[n], XmNborderWidth, 0); n++;
      XtSetArg(args[n], XmNmarginWidth, 0); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;}
      sw[W_filter_order_up] = make_pushbutton_widget("", sw[W_amp_form], args, n);
      XtAddEventHandler(sw[W_filter_order_up], KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_filter_order_up], XmNactivateCallback, filter_order_up_callback, (XtPointer)sp);

      n = 0;
      s1 = XmStringCreate("", XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_reverb_button]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNmarginWidth, 0); n++;
      XtSetArg(args[n], XmNtopOffset, 2); n++;
      XtSetArg(args[n], XmNspacing, 0); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++; 
      if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNselectColor, (ss->sgx)->pushed_button_color); n++;}
      sw[W_filter_button] = make_togglebutton_widget("fltoff", sw[W_amp_form], args, n);
      XtAddEventHandler(sw[W_filter_button], KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_filter_button], XmNvalueChangedCallback, filter_button_callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;
      s1 = XmStringCreate(_("hz"), XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_filter_button]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, sw[W_filter_button]); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++; 
      XtSetArg(args[n], XmNvalue, sp->filter_control_in_hz); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNselectColor, (ss->sgx)->pushed_button_color); n++;}
      sw[W_filter_hz] = make_togglebutton_widget("flthz", sw[W_amp_form], args, n);
      XtAddEventHandler(sw[W_filter_hz], KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_filter_hz], XmNvalueChangedCallback, filter_hz_callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;
      s1 = XmStringCreate(_("dB"), XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_filter_hz]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, sw[W_filter_hz]); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++; 
      XtSetArg(args[n], XmNvalue, sp->filter_control_in_dB); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNselectColor, (ss->sgx)->pushed_button_color); n++;}
      sw[W_filter_dB] = make_togglebutton_widget("fltdB", sw[W_amp_form], args, n);
      XtAddEventHandler(sw[W_filter_dB], KeyPressMask, false, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_filter_dB], XmNvalueChangedCallback, filter_dB_callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_filter_order_down]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_filter_order_down]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, sw[W_filter_dB]); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      sw[W_filter] = make_textfield_widget("filter-text", sw[W_amp_form], args, n, ACTIVATABLE, add_completer_func(filename_completer));
      XtAddCallback(sw[W_filter], XmNactivateCallback, filter_activate_callback, (XtPointer)sp);

      /* FILTER GRAPH */
      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_filter]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 4); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 98); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      /* if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;} */
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 4); n++;
      sw[W_filter_frame] = XtCreateManagedWidget("filter-frame", xmFrameWidgetClass, sw[W_amp_form], args, n);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;}
      n = attach_all_sides(args, n);
      XtSetArg(args[n], XmNallowResize, true); n++;
      sw[W_filter_env] = XtCreateManagedWidget("filter-window", xmDrawingAreaWidgetClass, sw[W_filter_frame], args, n);
      XtAddCallback(sw[W_filter_env], XmNresizeCallback, filter_drawer_resize, (XtPointer)sp);
      XtAddCallback(sw[W_filter_env], XmNexposeCallback, filter_drawer_resize, (XtPointer)sp);

      sp->sgx->flt = (void *)new_env_editor();

      XtAddEventHandler(sw[W_filter_env], ButtonPressMask, false, filter_drawer_button_press, sp);
      XtAddEventHandler(sw[W_filter_env], ButtonMotionMask, false, filter_drawer_button_motion, sp);
      XtAddEventHandler(sw[W_filter_env], ButtonReleaseMask, false, filter_drawer_button_release, sp);
      XtAddEventHandler(sw[W_filter_env], KeyPressMask, false, graph_key_press, (XtPointer)sp);
      FREE(n1); FREE(n2); FREE(n3); FREE(n4); FREE(n5); FREE(n6);
      FREE(n7); FREE(n8); FREE(n9); FREE(n10); FREE(n11); FREE(n12);
      /* end if control-panel */
#if (XmVERSION > 1)
      if (sound_style(ss) == SOUNDS_IN_NOTEBOOK)
	{
	  char* name;
	  name = just_filename(sp->short_filename); /* copies */
	  if (strlen(name) > 8) name[8] = '\0';
	  n = 0;
	  if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->graph_color); n++;}
	  XtSetArg(args[n], XmNnotebookChildType, XmMAJOR_TAB); n++;
	  XtSetArg(args[n], XmNuserData, sp->index); n++;
	  sx->tab = XtCreateManagedWidget(name, xmPushButtonWidgetClass, SOUND_PANE(ss), args, n);
	  FREE(name);
	}
#endif
      if (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS)
	run_new_widget_hook(sw[W_pane]);
      else run_new_widget_hook(sx->dialog);

#if WITH_RELATIVE_PANES
      if (sound_style(ss) == SOUNDS_VERTICAL)
	add_watchers(SOUND_PANE(ss)); /* add in any case since we might later change the sense of with_relative_panes */
#endif

    } /* new sound ss */
  else
    { /* re-manage currently inactive chan */
      if (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS)
	XtVaSetValues(sw[W_control_panel],
		      XmNpaneMinimum, ctrls_height,
		      XmNpaneMaximum, ctrls_height,
		      NULL);
      else 
	{
	  title = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
	  mus_snprintf(title, PRINT_BUFFER_SIZE, "%d: %s", snd_slot, sp->short_filename);
	  XtVaSetValues(sx->dialog, XmNtitle, title, NULL);
	  FREE(title);
	  if (!XtIsManaged(sx->dialog)) XtManageChild(sx->dialog);
	}
      for (i = 0; i < NUM_SND_WIDGETS; i++)
	if ((sw[i]) && (!XtIsManaged(sw[i]))) 
	  XtManageChild(sw[i]);
      for (k = 0; k < nchans; k++) 
	add_channel_window(sp, k, chan_min_y, 0, NULL, WITH_FW_BUTTONS, WITH_EVENTS);
      set_button_label(sw[W_name], shortname_indexed(sp));
      XtVaSetValues(sw[W_pane], XmNuserData, sp->index, NULL);
      if (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS)
	XtVaSetValues(sw[W_control_panel],
		      XmNpaneMinimum, 1,
		      XmNpaneMaximum, LOTSA_PIXELS,
		      NULL);
#if (XmVERSION > 1)
      if (sound_style(ss) == SOUNDS_IN_NOTEBOOK)
	{
	  char *name;
	  name = just_filename(sp->short_filename);
	  set_label(sx->tab, name);
	  FREE(name);
	}
#endif
    }
  if (sp->nchans == 1) 
    {
      XmToggleButtonSetState(unite_button(sp), false, false);
      XtUnmanageChild(unite_button(sp));
    }
  add_sound_data(filename, sp, WITH_GRAPH);
  if (cant_write(sp->filename)) sp->read_only = true;
  snd_file_lock_icon(sp, sp->read_only);
  if (old_name)
    report_in_minibuffer(sp, _("(translated %s)"), old_name);
  if (!(ss->using_schemes)) map_over_children(SOUND_PANE(ss), color_sashes, NULL);
  if (!(auto_resize(ss))) equalize_all_panes(); 

  if (first_window)
    {
      /* try to get the pane height that shows everything except the filter graph (hidden for my amusement) */
      /* this calculation assumes the window is built amp_form down, then record buttons up, then filter_frame */
      Position fey, cy;
      /* if control-panel */
      cy = widget_y(sw[W_amp_form]);
      fey = widget_y(sw[W_filter_frame]);
      /* end if control-panel */
      open_ctrls_height = fey + cy - 1;
      first_window = false;
    } 
  if (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS)
    {
      if (make_widgets) 
	XtVaSetValues(sw[W_control_panel],
		      XmNpaneMaximum, LOTSA_PIXELS,
		      NULL); /* locked above to force correct initial setup */
      reset_controls(sp);
      if ((sound_style(ss) == SOUNDS_HORIZONTAL) && (ss->active_sounds > 0)) /* active_sounds off-by-one here */
	equalize_all_panes();
    }
  else 
    {
      XtVaSetValues(sx->dialog,
		    XmNwidth, 100,
		    XmNheight, 100,
		    NULL);
      /* this is not redundant -- apparently they're trying to ignore size resets to the "current" */
      /* value, but forgot that unmanage/remanage does not return to the previous size */
      XtVaSetValues(sx->dialog,
		    XmNwidth, (Dimension)(widget_width(MAIN_SHELL(ss))),
		    XmNheight, (Dimension)(chan_min_y * nchans), /* bugfix thanks to Paul @pobox */
		    NULL);
      if (nchans > 1) equalize_all_panes();
    }
  after_open(sp->index);
  if (free_filename) FREE(filename);
  return(sp);
}

void snd_info_cleanup(snd_info *sp)
{
  if ((sp) && (sp->sgx))
    {
      snd_context *sx;
      sx = sp->sgx;
      if (SYNC_BUTTON(sp))
	{
	  XtVaSetValues(SYNC_BUTTON(sp), XmNset, false, NULL);
	  XtVaSetValues(EXPAND_BUTTON(sp), XmNset, false, NULL);
	  XtVaSetValues(CONTRAST_BUTTON(sp), XmNset, false, NULL);
	  XtVaSetValues(SPEED_ARROW(sp), XmNset, false, NULL);
	  XtVaSetValues(FILTER_BUTTON(sp), XmNset, false, NULL);
	  XtVaSetValues(REVERB_BUTTON(sp), XmNset, false, NULL);
	  XmToggleButtonSetState(unite_button(sp), false, false);
	  sp->channel_style = CHANNELS_SEPARATE;
#if (XmVERSION > 1)
	  if (sound_style(ss) == SOUNDS_IN_NOTEBOOK)
	    {
	      set_label((sp->sgx)->tab, _("none"));
	      XmChangeColor((sp->sgx)->tab, (ss->sgx)->graph_color);
	    }
#endif
	  XtUnmanageChild(w_snd_pane(sp));
	}
      if ((sx->dialog) && 
	  (XtIsManaged(sx->dialog))) 
	XtUnmanageChild(sx->dialog);
    }
}

void set_sound_pane_file_label(snd_info *sp, char *str)
{
  if ((sp->name_string == NULL) || (strcmp(sp->name_string, str) != 0))
    {
      if (sp->name_string) FREE(sp->name_string);
      sp->name_string = copy_string(str);
      set_button_label(w_snd_name(sp), str); /* this causes an expose event, so it's worth minimizing */
    }
}


/* ---------------- normalize sounds ---------------- */

static void even_channels(snd_info *sp, void *ptr)
{
  int chans;
  chans = sp->nchans;
  if (chans > 1)
    {
      int val, height, i;
      height = (*((int *)ptr));
      val = height / chans - 16;
      if (val < 6) val = 6;
      for (i = 0; i < chans; i++)
	{
	  chan_info *cp;
	  cp = sp->chans[i];
	  XtUnmanageChild(channel_main_pane(cp));
	  XtVaSetValues(channel_main_pane(cp),
			XmNpaneMinimum, val - 5,
			XmNpaneMaximum, val + 5,
			NULL);
	}
    }
}

static void even_sounds(snd_info *sp, void *ptr)
{
  int width;
  width = (*((int *)ptr));
  XtUnmanageChild(w_snd_pane(sp));
  XtVaSetValues(w_snd_pane(sp),
		XmNpaneMinimum, width - 5,
		XmNpaneMaximum, width + 5,
		NULL);
}

static void sound_open_pane(snd_info *sp, void *ptr)
{
  XtManageChild(w_snd_pane(sp));
}

static void sound_unlock_pane(snd_info *sp, void *ptr)
{
  XtVaSetValues(w_snd_pane(sp),
		XmNpaneMinimum, 5,
		XmNpaneMaximum, LOTSA_PIXELS,
		NULL);
}

void unlock_control_panel(snd_info *sp) 
{
  XtVaSetValues(CONTROL_PANEL(sp), XmNpaneMinimum, 1, NULL);
}

void equalize_sound_panes(snd_info *sp, chan_info *ncp, bool all_panes)
{
  /* make sp look ok, squeezing others if needed */
  /* if there's already enough (i.e. ss->channel_min_height), just return */
  /* this is used in goto_next_graph and goto_previous_graph (snd-chn.c) to open windows that are currently squeezed shut */
  Dimension chan_y, total = 0;
  int *wid;
  int i;
  chan_info *cp = NULL;
  if ((!sp) || (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS)) return;
  if (sound_style(ss) != SOUNDS_HORIZONTAL)
    {
      if ((all_panes) && (sp->nchans > 1) && (sp->channel_style == CHANNELS_SEPARATE))
	{
	  for (i = 0; i < sp->nchans; i++)
	    {
	      XtVaGetValues(channel_main_pane(sp->chans[i]), XmNheight, &chan_y, NULL);
	      total += chan_y;
	    }
	  total /= sp->nchans;
	  for (i = 0; i < sp->nchans; i++)
	    {
	      cp = sp->chans[i];
	      XtUnmanageChild(channel_main_pane(cp));
	      XtVaSetValues(channel_main_pane(cp),
			    XmNpaneMinimum, total - 5,
			    XmNpaneMaximum, total + 5,
			    NULL);
	    }
	  for (i = 0; i < sp->nchans; i++)
	    {
	      cp = sp->chans[i];
	      XtManageChild(channel_main_pane(cp));
	      XtVaSetValues(channel_main_pane(cp),
			    XmNpaneMinimum, 5,
			    XmNpaneMaximum, LOTSA_PIXELS,
			    NULL);
	    }
	}
      else
	{
	  /* several attempts to be fancy here just made a mess of the display */
	  if (sp->channel_style == CHANNELS_SEPARATE)
	    cp = ncp;
	  else cp = ncp->sound->chans[0];
	  XtVaGetValues(channel_main_pane(cp), XmNheight, &chan_y, NULL);
	  if (chan_y < (Dimension)(ss->channel_min_height >> 1)) 
	    {
	      wid = (int *)CALLOC(1, sizeof(int));
	      wid[0] = (ss->channel_min_height >> 1) + 10;
	      channel_lock_pane(cp, (void *)wid);
	      channel_open_pane(cp, NULL);
	      channel_unlock_pane(cp, NULL);
	      FREE(wid);
	      wid = NULL;
	    }
	}
    }
  else
    {
      chan_y = widget_width(channel_main_pane(ncp));
      if (chan_y < 200)
	{
	  XtUnmanageChild(channel_main_pane(ncp));
	  XtVaSetValues(channel_main_pane(ncp), XmNwidth, 200, NULL);
	  XtManageChild(channel_main_pane(ncp));
	}
    }
  if (sp->channel_style == CHANNELS_COMBINED)
    {
      Float low, high;
      cp = any_selected_channel(sp);
      high = (Float)(sp->nchans - cp->chan) / (Float)sp->nchans;
      low = high - 1.0 / (Float)sp->nchans;
      cp = sp->chans[0];
      fixup_gsy(cp, low, high);
    }
}


void color_filter_waveform(Pixel color)
{
  int i;
  XSetForeground(MAIN_DISPLAY(ss), (ss->sgx)->fltenv_data_gc, color);
  (ss->sgx)->filter_control_waveform_color = color;
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	display_filter_env(sp);
    }
}

void reflect_amp_env_completion(snd_info *sp)
{
  int i;
  /* a channel completed an amp env, check to see if all are complete */
  for (i = 0; i < sp->nchans; i++)
    {
      chan_info *cp;
      env_info *ep;
      cp = sp->chans[i];
      if (!(cp->amp_envs)) return;
      ep = cp->amp_envs[cp->edit_ctr];
      if (!ep) return;
      if (!(ep->completed)) return;
    }
  if (sp->sgx)
    {
      XtVaSetValues(MINIBUFFER_SEPARATOR(sp), XmNseparatorType, XmSHADOW_ETCHED_IN, NULL);
      alert_enved_amp_env(sp);
    }
}

void reflect_amp_env_in_progress(snd_info *sp)
{
  if (sp->sgx)
    XtVaSetValues(MINIBUFFER_SEPARATOR(sp), XmNseparatorType, XmNO_LINE, NULL);
}

void equalize_all_panes(void)
{
  /* normalize: get size, #chans, #snds, set pane minima, force remanage(?), unlock */
  int sounds = 0, chans, chan_y, height, width, screen_y, i;
  int wid[1];
  snd_info *nsp;
  if (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS)
    {
      for (i = 0; i < ss->max_sounds; i++)
	{
	  nsp = ss->sounds[i];
	  if ((snd_ok(nsp)) && (nsp->inuse == SOUND_NORMAL))
	    {
	      if (nsp->nchans > 1)
		{
		  height = widget_height(w_snd_pane(nsp));
		  even_channels(nsp, (void *)(&height));
		  map_over_sound_chans(nsp, channel_open_pane, NULL);
		  map_over_sound_chans(nsp, channel_unlock_pane, NULL);
		}
	    }
	}
      return;
    }
  for (i = 0; i < ss->max_sounds; i++) 
    if ((snd_ok(ss->sounds[i])) && (ss->sounds[i]->inuse == SOUND_NORMAL))
      sounds++;
  if (sound_style(ss) == SOUNDS_VERTICAL)
    {
      height = widget_height(SOUND_PANE(ss)) - listener_height();
      lock_listener_pane();
      /* all are lined up vertically, so we can just make all chans the same size */
      if (auto_resize(ss))
	{
	  screen_y = DisplayHeight(MAIN_DISPLAY(ss), DefaultScreen(MAIN_DISPLAY(ss)));
	  if (height > screen_y) height = screen_y;
	}
      else XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, true, NULL); /* need temporary resize to change pane sizes below */
      chans = active_channels(WITHOUT_VIRTUAL_CHANNELS);
      if (chans > 1)
	{
	  /* now we try to make room for the sound ctrl bar, each channel, perhaps the menu */
	  chan_y = (height - (sounds * ctrls_height)) / chans - 16;
	  /* probably can be 14 or 12 -- seems to be margin related or something */
	  wid[0] = chan_y;
	  for_each_sound(sound_lock_control_panel, NULL);
	  map_over_separate_chans(channel_lock_pane, (void *)wid);
	  map_over_separate_chans(channel_open_pane, NULL);
	  map_over_separate_chans(channel_unlock_pane, NULL);
	  for_each_sound(sound_unlock_control_panel, NULL);
	}
      unlock_listener_pane();
      if (!(auto_resize(ss))) XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, false, NULL);
    }
  else
    {
      if (sound_style(ss) == SOUNDS_HORIZONTAL)
	{
	  height = widget_height(SOUND_PANE(ss));
	  if (sounds > 1) 
	    {
	      width = widget_width(MAIN_PANE(ss));
	      width /= sounds;
	      for_each_sound(even_sounds, (void *)(&width));
	      for_each_sound(sound_open_pane, NULL);
	      for_each_sound(sound_unlock_pane, NULL);
	    }
	  for_each_sound(sound_lock_control_panel, NULL);
	  for_each_sound(even_channels, (void *)(&height));
	  map_over_separate_chans(channel_open_pane, NULL);   /* manage the channel widgets */
	  map_over_separate_chans(channel_unlock_pane, NULL); /* allow pane to be resized */
	  for_each_sound(sound_unlock_control_panel, NULL);
	}
    }
}

void sound_show_ctrls(snd_info *sp)
{
  XtUnmanageChild(CONTROL_PANEL(sp));
  XtVaSetValues(CONTROL_PANEL(sp),
		XmNpaneMinimum, open_ctrls_height,
		XmNpaneMaximum, open_ctrls_height,
		NULL);
  XtManageChild(CONTROL_PANEL(sp));
  XtVaSetValues(CONTROL_PANEL(sp),
		XmNpaneMinimum, 1,
		XmNpaneMaximum, LOTSA_PIXELS,
		NULL);
}

void sound_hide_ctrls(snd_info *sp)
{
  XtUnmanageChild(CONTROL_PANEL(sp));
  XtVaSetValues(CONTROL_PANEL(sp),
		XmNpaneMaximum, CLOSED_CTRLS_HEIGHT,
		XmNpaneMinimum, CLOSED_CTRLS_HEIGHT,
		NULL);
  XtManageChild(CONTROL_PANEL(sp));
  XtVaSetValues(CONTROL_PANEL(sp),
		XmNpaneMinimum, 1,
		XmNpaneMaximum, LOTSA_PIXELS,
		NULL);
}

bool control_panel_open(snd_info *sp)
{
  Dimension hgt;
  XtVaGetValues(CONTROL_PANEL(sp), XmNheight, &hgt, NULL);
  return(hgt > CLOSED_CTRLS_HEIGHT);
}

void show_controls(void)
{
  int i;
  ctrls_height = open_ctrls_height;
  set_view_ctrls_label(_("Hide controls"));
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	sound_show_ctrls(sp);
    }
}

void hide_controls(void)
{
  int i;
  ctrls_height = CLOSED_CTRLS_HEIGHT;
  set_view_ctrls_label(_("Show controls"));
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	sound_hide_ctrls(sp);
    }
}

int control_panel_height(snd_info *sp)
{
  return(widget_height(CONTROL_PANEL(sp)));
}


/* -------- PROGRESS REPORT -------- */
/*
 * if no xpm, send a string, else post an hourglass
 */

void progress_report(snd_info *sp, const char *funcname, int curchan, int chans, Float pct, enved_progress_t from_enved)
{
  int which;
#if HAVE_XPM
  char glass_num[8];
  char expr_str[8];
  if ((!sp) || (sp->inuse != SOUND_NORMAL)) return;
  which = (int)(pct * NUM_GLASSES);
  mus_snprintf(expr_str, 8, "%.2f", pct);
  if (which >= NUM_GLASSES) which = NUM_GLASSES - 1;
  if (which < 0) which = 0;
  if (from_enved == FROM_ENVED)
    display_enved_progress(expr_str, mini_glasses[which]);
  else 
    {
      report_in_minibuffer(sp, expr_str);
      snd_file_glasses_icon(sp, true, which);
    }
  if (chans > 1) 
    {
      mus_snprintf(glass_num, 8, "[%d]", curchan);
      make_minibuffer_label(sp, glass_num);
    }
#else
  char *expr_str;
  if (sp->inuse != SOUND_NORMAL) return;
  expr_str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  which = (int)(100.0 * pct);
  if (chans > 1)
    mus_snprintf(expr_str, PRINT_BUFFER_SIZE, "%s: (%d of %d) %d%%", funcname, curchan, chans, which);
  else mus_snprintf(expr_str, PRINT_BUFFER_SIZE, "%s: %d%%", funcname, which);
  if (from_enved == FROM_ENVED)
    display_enved_progress(expr_str, 0);
  else report_in_minibuffer(sp, expr_str);
  FREE(expr_str);
#endif
  check_for_event();
}

void finish_progress_report(snd_info *sp, enved_progress_t from_enved)
{
#if (!HAVE_XPM)
#endif
  if (sp->inuse != SOUND_NORMAL) return;
#if HAVE_XPM
  if (from_enved == FROM_ENVED)
    display_enved_progress(NULL, blank_pixmap);
  else snd_file_glasses_icon(sp, false, 0);
  clear_minibuffer_prompt(sp);
  if (!(ss->stopped_explicitly)) clear_minibuffer(sp);
#else
  if (from_enved == FROM_ENVED)
    display_enved_progress((ss->stopped_explicitly) ? _("stopped") : "", 0);
  else report_in_minibuffer(sp, (ss->stopped_explicitly) ? _("stopped") : "");
#endif
}

void start_progress_report(snd_info *sp, enved_progress_t from_enved)
{
  if (sp->inuse != SOUND_NORMAL) return;
#if HAVE_XPM
  if (from_enved == NOT_FROM_ENVED) 
    snd_file_glasses_icon(sp, true, 0);
#else
  if (from_enved == FROM_ENVED)
    display_enved_progress("", 0);
#endif
}

void reflect_sound_selection(snd_info *sp)
{
  if (!(ss->using_schemes))
    {
      snd_info *osp = NULL;
      if (ss->selected_sound != NO_SELECTION) osp = ss->sounds[ss->selected_sound];
      if ((osp) && (sp != osp) && (osp->inuse == SOUND_NORMAL)) 
	{
	  XmChangeColor(w_snd_name(osp), (ss->sgx)->highlight_color);
#if (XmVERSION > 1)
	  if (sound_style(ss) == SOUNDS_IN_NOTEBOOK) 
	    XmChangeColor((osp->sgx)->tab, (ss->sgx)->graph_color);
#endif
	}
      if (sp->selected_channel != NO_SELECTION) 
	{
	  XmChangeColor(w_snd_name(sp), (ss->sgx)->white);
#if (XmVERSION > 1)
	  if (sound_style(ss) == SOUNDS_IN_NOTEBOOK) 
	    {
	      int page, current_page;
	      XmNotebookPageStatus status;
	      XmNotebookPageInfo info;
	      XmChangeColor((sp->sgx)->tab, (ss->sgx)->selected_graph_color);
	      XtVaGetValues(SOUND_PANE(ss), XmNcurrentPageNumber, &current_page, NULL);
	      XtVaGetValues(sp->sgx->tab, XmNpageNumber, &page, NULL);
	      if (page != current_page)
		{
		  status = XmNotebookGetPageInfo(SOUND_PANE(ss), page, &info);
		  if (status == XmPAGE_FOUND)
		    {
		      XtVaSetValues(SOUND_PANE(ss), XmNcurrentPageNumber, page, NULL);
		      if (sp->nchans > 1)
			equalize_sound_panes(sp, sp->chans[0], false);
		    }
		}
	    }
#endif
	}
    }
}


static XEN g_sound_widgets(XEN snd)
{
  #define H_sound_widgets "(" S_sound_widgets " (snd #f)): a list of \
widgets: (0)pane (1)name (2)control-panel (3)minibuffer (4)play-button (5)filter-env (6)unite-button (7)name-label (8)name-icon (9)sync-button"
  snd_info *sp;
  ASSERT_SOUND(S_sound_widgets, snd, 1);
  sp = get_sp(snd, NO_PLAYERS);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_sound_widgets, snd));
  if (sp->sgx == NULL)
    return(XEN_EMPTY_LIST);
  return(XEN_CONS(XEN_WRAP_WIDGET(w_snd_pane(sp)),
	  XEN_CONS(XEN_WRAP_WIDGET(w_snd_name(sp)),
           XEN_CONS(XEN_WRAP_WIDGET(CONTROL_PANEL(sp)),
	    XEN_CONS(XEN_WRAP_WIDGET(MINIBUFFER_TEXT(sp)),
	     XEN_CONS(XEN_WRAP_WIDGET(PLAY_BUTTON(sp)),
	      XEN_CONS(XEN_WRAP_WIDGET(filter_graph(sp)), /* this is the drawingarea widget */
	       XEN_CONS(XEN_WRAP_WIDGET(unite_button(sp)),
	        XEN_CONS(XEN_WRAP_WIDGET(MINIBUFFER_LABEL(sp)),
	         XEN_CONS(XEN_WRAP_WIDGET(NAME_ICON(sp)),
	          XEN_CONS(XEN_WRAP_WIDGET(SYNC_BUTTON(sp)),
	           XEN_EMPTY_LIST)))))))))));
}

#if DEBUGGING && HAVE_GUILE && WITH_RELATIVE_PANES
static XEN g_sash(void)
{
  int i;
  XEN lst = XEN_EMPTY_LIST;
  for (i = 0; i < sashes_size; i++)
    if (sashes[i])
      lst = XEN_CONS(XEN_WRAP_WIDGET(sashes[i]), lst);
  return(lst);
}
#endif

#ifdef XEN_ARGIFY_1
  XEN_ARGIFY_1(g_sound_widgets_w, g_sound_widgets)
#else
  #define g_sound_widgets_w g_sound_widgets
#endif

void g_init_gxsnd(void)
{
  XEN_DEFINE_PROCEDURE(S_sound_widgets,  g_sound_widgets_w,  0, 1, 0, H_sound_widgets);
#if DEBUGGING && HAVE_GUILE && WITH_RELATIVE_PANES
  XEN_DEFINE_PROCEDURE("top-sash", g_sash, 0, 0, 0, "autotest func");
#endif
}

