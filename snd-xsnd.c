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
      W_srate, W_srate_label, W_srate_number, W_srate_arrow,
      W_expand, W_expand_label, W_expand_number, W_expand_button,
      W_contrast, W_contrast_label, W_contrast_number, W_contrast_button,
      W_revscl, W_revscl_label, W_revscl_number,
      W_revlen, W_revlen_label, W_revlen_number, W_reverb_button,
      W_remember, W_restore, W_apply, W_reset,
      W_filter_label, W_filter_order, W_filter_env, W_filter, W_filter_button, W_filter_dB, W_filter_frame,
      W_filter_order_down, W_filter_order_up,
      W_name, W_name_icon, W_info_label, W_info,
      W_info_sep,
      W_play, W_sync, W_unite,
      W_control_panel
};

#define NUM_SND_WIDGETS 48

Widget unite_button(snd_info *sp) {return((sp->sgx)->snd_widgets[W_unite]);}
Widget filter_graph(snd_info *sp) {return((sp->sgx)->snd_widgets[W_filter_env]);}

Widget w_snd_pane(snd_info *sp)   {return((sp->sgx)->snd_widgets[W_pane]);}
Widget w_snd_name(snd_info *sp)   {return((sp->sgx)->snd_widgets[W_name]);}

#define CONTROL_PANEL(Sp)        (Sp->sgx)->snd_widgets[W_control_panel]
#define PLAY_BUTTON(Sp)          (Sp->sgx)->snd_widgets[W_play]
#define NAME_ICON(Sp)            (Sp->sgx)->snd_widgets[W_name_icon]
#define AMP_SCROLLBAR(Sp)        (Sp->sgx)->snd_widgets[W_amp]
#define SRATE_SCROLLBAR(Sp)      (Sp->sgx)->snd_widgets[W_srate]
#define SRATE_ARROW(Sp)          (Sp->sgx)->snd_widgets[W_srate_arrow]
#define EXPAND_SCROLLBAR(Sp)     (Sp->sgx)->snd_widgets[W_expand]
#define CONTRAST_SCROLLBAR(Sp)   (Sp->sgx)->snd_widgets[W_contrast]
#define REVSCL_SCROLLBAR(Sp)     (Sp->sgx)->snd_widgets[W_revscl]
#define REVLEN_SCROLLBAR(Sp)     (Sp->sgx)->snd_widgets[W_revlen]
#define AMP_LABEL(Sp)            (Sp->sgx)->snd_widgets[W_amp_number]
#define SRATE_LABEL(Sp)          (Sp->sgx)->snd_widgets[W_srate_number]
#define EXPAND_LABEL(Sp)         (Sp->sgx)->snd_widgets[W_expand_number]
#define EXPAND_BUTTON(Sp)        (Sp->sgx)->snd_widgets[W_expand_button]
#define CONTRAST_LABEL(Sp)       (Sp->sgx)->snd_widgets[W_contrast_number]
#define CONTRAST_BUTTON(Sp)      (Sp->sgx)->snd_widgets[W_contrast_button]
#define REVSCL_LABEL(Sp)         (Sp->sgx)->snd_widgets[W_revscl_number]
#define REVLEN_LABEL(Sp)         (Sp->sgx)->snd_widgets[W_revlen_number]
#define REVERB_BUTTON(Sp)        (Sp->sgx)->snd_widgets[W_reverb_button]
#define APPLY_BUTTON(Sp)         (Sp->sgx)->snd_widgets[W_apply]
#define FILTER_ORDER_TEXT(Sp)    (Sp->sgx)->snd_widgets[W_filter_order]
#define FILTER_COEFFS_TEXT(Sp)   (Sp->sgx)->snd_widgets[W_filter]
#define FILTER_BUTTON(Sp)        (Sp->sgx)->snd_widgets[W_filter_button]
#define FILTER_DB_BUTTON(Sp)     (Sp->sgx)->snd_widgets[W_filter_dB]
#define MINIBUFFER_SEPARATOR(Sp) (Sp->sgx)->snd_widgets[W_info_sep]
#define MINIBUFFER_LABEL(Sp)     (Sp->sgx)->snd_widgets[W_info_label]
#define MINIBUFFER_TEXT(Sp)      (Sp->sgx)->snd_widgets[W_info]
#define SYNC_BUTTON(Sp)          (Sp->sgx)->snd_widgets[W_sync]

#define MAX_NOTEBOOK_TAB_LENGTH 5

static void info_help_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_help_with_url_and_wrap((snd_state *)context, 
			     "Minibuffer", "#panelayout",
"This is the 'minibuffer', to use Emacs jargon.  Although it looks inert and wasted,  \
there is in fact a text window lurking beneath that has access to the Lisp evaluator, not \
to mention much of the innards of the Snd program.");
}

static void play_help_callback(Widget w, XtPointer context, XtPointer info)
{  
  snd_help_with_url_and_wrap((snd_state *)context, 
			     "Play", "#play",
"Snd can play any number of sounds at once or should be able to anyway.  A sort of \
clumsy realtime mixer, although it was not intended to fill that role.");
}

static void info_sep_help_callback(Widget w, XtPointer context, XtPointer info)        
{  
  snd_help_with_wrap((snd_state *)context, 
		     "Name Separator", 
"When reading a very large file, Snd tries to keep an overview at hand of the channels so \
that you can move around quickly in very large data sets; when first read in, these overviews \
are set underway, and when they are finally ready for use, the line after the file name \
appears.  If you try to zoom out to a large view before the separator line appears, the graphics update process may be slow. ");
}

static void amp_help_callback(Widget w, XtPointer context, XtPointer info)             
{
  snd_help_with_wrap((snd_state *)context, 
		     "Amp", 
"This scrollbar controls the amplitude at which the sound is played.  Click the \
amp label to return to 1.0. Control-Click returns to the previous value.");
}

static void srate_help_callback(Widget w, XtPointer context, XtPointer info)           
{
  snd_help_with_url_and_wrap((snd_state *)context, 
			     "Srate", "#speed", 
"This scrollbar controls the sampling rate at which the sound is played.  The arrow \
controls the direction (forwards or backwards) of playback.  Label clicks behave as with amp.");
}

static void srate_arrow_help_callback(Widget w, XtPointer context, XtPointer info)     
{
  snd_help_with_wrap((snd_state *)context, 
		     "Srate Arrow",
"This button determines which direction the sound file is played.  When pointing \
to the right, the sound is played forwards;  to the left, backwards.");
}

static void expand_help_callback(Widget w, XtPointer context, XtPointer info)          
{  
  snd_help_with_url_and_wrap((snd_state *)context, 
			     "Expand", "#expand",
"This scrollbar controls the tempo at which the sound is played back, using granular \
synthesis. The expand button must be down to get any expansion. Label clicks as in amp.");
}

static void contrast_help_callback(Widget w, XtPointer context, XtPointer info)        
{  
  snd_help_with_url((snd_state *)context, 
		    "Contrast", "#contrast",
"This scrollbar controls the amount of 'contrast enhancement' applied during \
playback.  The contrast button must be down to get any effect.  Label clicks as in amp.");
}

static void revscl_help_callback(Widget w, XtPointer context, XtPointer info)          
{  
  snd_help_with_url_and_wrap((snd_state *)context, 
			     "Reverb amount", "#reverb",
"This scrollbar controls the amount of the sound that is fed into the reverberator. \
The reverb button must be down to get any reverb during playback.  Label clicks as in amp.");
}

static void revlen_help_callback(Widget w, XtPointer context, XtPointer info)          
{
  snd_help_with_url_and_wrap((snd_state *)context, 
			     "Reverb length", "#reverb", 
"This scrollbar controls the lengths of the various delay lines in the reverb. \
It only takes effect when the reverb is created, that is, only when the play \
operation starts from silence.  Label clicks as in amp.");
}

static void filter_help_callback(Widget w, XtPointer context, XtPointer info)          
{
  snd_help_with_wrap((snd_state *)context, 
		     "Filter", 
"The Snd filter is an FIR filter of arbitrary order.  You specify the filter you want by \
defining the frequency response as an envelope in the 'env' window; set the desired order in \
the 'order' window; then turn it on by pushing the filter button at the right.  The filter \
design algorithm uses frequency sampling. The higher the order, the closer the filter \
can approximate the envelope you draw. You can also specify the filter coefficients \
in a file of floats, then load them into the Snd filter by typing the file name in the \
filter envelope text window.");
}

static void filter_order_help_callback(Widget w, XtPointer context, XtPointer info)    
{
  snd_help_with_wrap((snd_state *)context, 
		     "Filter Order", 
"The filter order determines how closely the filter approximates the frequency response curve you drew in the 'env' window. ");
}

static void filter_envelope_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help_with_wrap((snd_state *)context, 
		     "Filter Envelope", 
"The filter envelope is a line-segment description of the frequency response \
you want.  It consists of a sequence of x, y pairs; normally the x axis goes \
from 0 to .5 or 0 to 1.0.  For example, a low-pass filter envelope could be: \
0.0 1.0 .25 1.0 .5 0.0 1.0 0.0");
}

static void name_help_callback(Widget w, XtPointer context, XtPointer info)            
{
  snd_help_with_wrap((snd_state *)context,
		     "Minibuffer",
"This portion of the snd display has several parts: the sound file name, with an asterisk if \
the file has unsaved edits; a minibuffer for various expression evaluations; a sync button \
that causes operations on one channel to be applied to all channels; and a play button \
that causes the sound to be played.  The lower portion of the pane, normally hidden, \
contains a variety of sound manipulation controls that can be applied while it is playing.");
}

static void expand_button_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help((snd_state *)context, "Expand Button", "This button turns on expansion (granular synthesis time scaling)\n");
}

static void contrast_button_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help((snd_state *)context, "Contrast Button", "This button turns on contrast enhancement (a form of companding)\n");
}

static void reverb_button_help_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_help((snd_state *)context, "Reverb Button", "This button turns on reverberation (Nrev)\n");
}

static void filter_button_help_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_help((snd_state *)context, "Filter Button", "This button turns on the filter\n");
}

static void filter_dB_help_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_help_with_wrap((snd_state *)context, 
		     "Filter dB",
"This button chooses between dB and linear y axis in the frequency response graph");
}

static void sync_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help_with_wrap((snd_state *)context,
		     "Sync Button",
"This button causes edit operations on one channel to be applied to all channels at the same time.");
}

static void unite_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help_with_wrap((snd_state *)context,
		     "Unite Button",
"This button causes all channels to be displayed in one window, sharing the various channel controls.  Two extra scrollbars on \
the right provide scroll and zoom for the overall set of channel graphs. The default multichannel display style can be set in \
the Snd initialization file by setting the variable " S_channel_style ".");
}


static void apply_help_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_help_with_wrap((snd_state *)context,
		     "Apply",
"The Apply button saves the last recorded run over the current file (see Record) as an edit of the current file.");
}

static void reset_help_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_help_with_wrap((snd_state *)context,
		     "Reset",
"The 'Reset' button clears the control panel settings to the no-change state.");
}

static void remember_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help_with_wrap((snd_state *)context,
		     "Remember",
"The 'Remember' button saves the current control panel state for a subsequent 'Restore'.");
}

static void restore_help_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_help_with_wrap((snd_state *)context,
		     "Restore",
"The 'Restore' button returns the control panel to the state at the time of the last 'Remember', or the initial state if there \
has been no 'Remember'.");
}

void goto_minibuffer(snd_info *sp)
{
  if (sp) goto_window(MINIBUFFER_TEXT(sp));
}

void set_minibuffer_string(snd_info *sp, char *str) 
{
  XmTextSetString(MINIBUFFER_TEXT(sp), str);
  XmUpdateDisplay(MINIBUFFER_TEXT(sp));
}

void set_minibuffer_cursor_position(snd_info *sp, int pos)
{
  XmTextSetCursorPosition(MINIBUFFER_TEXT(sp), pos);
}

char *get_minibuffer_string(snd_info *sp) 
{
  return(XmTextGetString(MINIBUFFER_TEXT(sp)));
}

void make_minibuffer_label(snd_info *sp , char *str)
{
  XmString s1;
  if ((sp->sgx) && (MINIBUFFER_LABEL(sp)))
    {
      s1 = XmStringCreate(str, "button_font");
      XtVaSetValues(MINIBUFFER_LABEL(sp), XmNlabelString, s1, NULL);
      XmStringFree(s1);
    }
}


void sound_unlock_control_panel(snd_info *sp, void *ptr)
{
  XtManageChild(CONTROL_PANEL(sp));
  XtVaSetValues(CONTROL_PANEL(sp), XmNpaneMinimum, 1, NULL);
}

void sound_lock_control_panel(snd_info *sp, void *ptr)
{
  snd_state *ss;
  ss = (snd_state *)(sp->state);
  XtUnmanageChild(CONTROL_PANEL(sp));
  XtVaSetValues(CONTROL_PANEL(sp), XmNpaneMinimum, ss->ctrls_height, NULL);
}

static void name_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  sp_name_click((snd_info *)context);
}

static char number_one[5] ={'1', STR_decimal, '0', '0', '\0'};
static char number_zero[5] ={'0', STR_decimal, '0', '0', '\0'};
static char semitone_one[5] ={' ', ' ', ' ', '0', '\0'};
static char ratio_one[5] ={' ', '1', '/', '1', '\0'};

static char amp_number_buffer[5] ={'1', STR_decimal, '0', '0', '\0'};

static int snd_amp_to_int(Float amp)
{
  int val;
  if (amp <= 0.0)
    val = 0;
  else
    {
      val = snd_round(amp / (Float)(SCROLLBAR_LINEAR_MULT));
      if (val > SCROLLBAR_LINEAR_MAX)
	{
	  val = snd_round((log(amp) * ((Float)SCROLLBAR_MAX * .2)) + SCROLLBAR_MID);
	  if (val > SCROLLBAR_MAX) val = SCROLLBAR_MAX;
	  /* in this and all similar cases, we should probably limit it to 90% of max (taking slider size into account) */
	}
    }
  return(val);
}

static int snd_amp_changed(snd_info *sp, int val)
{
  char *sfs;
  if (val == 0) 
    sp->amp_control = 0.0;
  else 
    {
      if (val < SCROLLBAR_LINEAR_MAX)
	sp->amp_control = (Float)val * SCROLLBAR_LINEAR_MULT;
      else sp->amp_control = exp((Float)(val - SCROLLBAR_MID) / ((Float)SCROLLBAR_MAX * .2));
    }
  sfs = prettyf(sp->amp_control, 2);
  fill_number(sfs, amp_number_buffer);
  set_label(AMP_LABEL(sp), amp_number_buffer);
  FREE(sfs);
  return(val);
}

void set_snd_amp(snd_info *sp, Float val)
{
  if (IS_PLAYER(sp))
    sp->amp_control = val;
  else XtVaSetValues(AMP_SCROLLBAR(sp),
		     XmNvalue,
		     snd_amp_changed(sp, snd_amp_to_int(mus_fclamp(0.0, val, 7.25))),
		     NULL);
}

static void amp_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  XButtonEvent *ev;
  int val;
  ASSERT_WIDGET_TYPE(XmIsPushButton(w), w);
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    val = snd_amp_to_int(sp->last_amp_control); 
  else val = SCROLLBAR_MID;
  snd_amp_changed(sp, val);
  XtVaSetValues(AMP_SCROLLBAR(sp), XmNvalue, val, NULL);
}

static void amp_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  snd_amp_changed((snd_info *)context, ((XmScrollBarCallbackStruct *)info)->value);
}

static void amp_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  snd_amp_changed(sp, cb->value);
  sp->last_amp_control = sp->saved_amp_control;
  sp->saved_amp_control = sp->amp_control;
}


#define SPEED_SCROLLBAR_MID (0.45 * SCROLLBAR_MAX)
#define SPEED_SCROLLBAR_BREAK (0.15 * SCROLLBAR_MAX)

static char srate_number_buffer[5] ={'1', STR_decimal, '0', '0', '\0'};

XmString initial_speed_label(snd_state *ss)
{
  switch (speed_control_style(ss))
    {
    case SPEED_CONTROL_AS_RATIO:    return(XmStringCreate(ratio_one, XmFONTLIST_DEFAULT_TAG));    break;
    case SPEED_CONTROL_AS_SEMITONE: return(XmStringCreate(semitone_one, XmFONTLIST_DEFAULT_TAG)); break;
    default:                        return(XmStringCreate(number_one, XmFONTLIST_DEFAULT_TAG));   break;
    }
}

static int snd_srate_to_int(Float val)
{
  int ival;
  if (val > 0.0)
    {
      ival = snd_round(SPEED_SCROLLBAR_MID + SPEED_SCROLLBAR_BREAK * log(val));
      if (ival < SCROLLBAR_MAX)
	return(ival);
      else return(SCROLLBAR_MAX);
    }
  else return(0);
}

static int snd_srate_changed(snd_info *sp, int ival)
{
  sp->speed_control = srate_changed(exp((Float)(ival - SPEED_SCROLLBAR_MID) / SPEED_SCROLLBAR_BREAK),
				    srate_number_buffer,
				    sp->speed_control_style,
				    sp->speed_control_tones);
  set_label(SRATE_LABEL(sp), srate_number_buffer);
  return(ival);
}

void set_snd_srate(snd_info *sp, Float val)
{
  if (IS_PLAYER(sp))
    sp->speed_control = val;
  else XtVaSetValues(SRATE_SCROLLBAR(sp),
		     XmNvalue,
		     snd_srate_changed(sp, snd_srate_to_int(mus_fclamp(-20.0, val, 20.0))),
		     NULL);
}

static void srate_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  XButtonEvent *ev;
  int val;
  ASSERT_WIDGET_TYPE(XmIsPushButton(w), w);
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    val = snd_srate_to_int(sp->last_speed_control); 
  else val = (int)SPEED_SCROLLBAR_MID;
  snd_srate_changed(sp, val);
  XtVaSetValues(SRATE_SCROLLBAR(sp), XmNvalue, val, NULL);
}

static void srate_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  snd_srate_changed((snd_info *)context, ((XmScrollBarCallbackStruct *)info)->value);
}

static void srate_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  snd_srate_changed(sp, cb->value);
  sp->last_speed_control = sp->saved_speed_control;
  sp->saved_speed_control = sp->speed_control;
}

void toggle_direction_arrow(snd_info *sp, int state)
{
  if (IS_PLAYER(sp))
    sp->speed_control_direction = ((state) ? -1 : 1);
  else XmToggleButtonSetState(SRATE_ARROW(sp), (Boolean)state, TRUE);
}


#define EXPAND_SCROLLBAR_MID (0.45 * SCROLLBAR_MAX)
#define EXPAND_SCROLLBAR_BREAK (0.15 * SCROLLBAR_MAX)
#define EXPAND_SCROLLBAR_MULT (.9697 / SCROLLBAR_MAX)
#define EXPAND_SCROLLBAR_SPLIT (0.1 * SCROLLBAR_MAX)

static char expand_number_buffer[5] ={'1', STR_decimal, '0', '0', '\0'};

static int snd_expand_to_int(Float ep)
{
  int val;
  val = (int)(ep / EXPAND_SCROLLBAR_MULT);
  if (val > EXPAND_SCROLLBAR_SPLIT) val = (int)(snd_round(EXPAND_SCROLLBAR_MID + EXPAND_SCROLLBAR_BREAK * log(ep)));
  if (val < SCROLLBAR_MAX)
    return(val);
  return(SCROLLBAR_MAX);
}

static int snd_expand_changed(snd_info *sp, int val)
{
  char *sfs;
  if (val < EXPAND_SCROLLBAR_SPLIT)
    sp->expand_control = (Float)val * EXPAND_SCROLLBAR_MULT;
  else sp->expand_control = exp((Float)(val - EXPAND_SCROLLBAR_MID) / EXPAND_SCROLLBAR_BREAK);
  if (sp->playing) dac_set_expand(sp, sp->expand_control);
  sfs = prettyf(sp->expand_control, 2);
  fill_number(sfs, expand_number_buffer);
  set_label(EXPAND_LABEL(sp), expand_number_buffer);
  FREE(sfs);
  return(val);
}

void set_snd_expand(snd_info *sp, Float val)
{
  if (IS_PLAYER(sp))
    sp->expand_control = val;
  else XtVaSetValues(EXPAND_SCROLLBAR(sp),
		     XmNvalue,
		     snd_expand_changed(sp, snd_expand_to_int(mus_fclamp(0.0, val, 20.0))),
		     NULL);
}

static void expand_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  XButtonEvent *ev;
  int val;
  ASSERT_WIDGET_TYPE(XmIsPushButton(w), w);
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    val = snd_expand_to_int(sp->last_expand_control); 
  else val = (int)EXPAND_SCROLLBAR_MID;
  snd_expand_changed(sp, val);
  XtVaSetValues(EXPAND_SCROLLBAR(sp), XmNvalue, val, NULL);
}

static void expand_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  snd_expand_changed((snd_info *)context, ((XmScrollBarCallbackStruct *)info)->value);
}

static void expand_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  snd_expand_changed(sp, cb->value);
  sp->last_expand_control = sp->saved_expand_control;
  sp->saved_expand_control = sp->expand_control;
}

static void expand_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_state *ss;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info; 
  snd_info *sp = (snd_info *)context;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  ss = sp->state;
  sp->expand_control_p = cb->set;
  if (!(ss->using_schemes)) 
    XmChangeColor(EXPAND_SCROLLBAR(sp), (Pixel)((sp->expand_control_p) ? ((ss->sgx)->position_color) : ((ss->sgx)->basic_color)));
}

void toggle_expand_button(snd_info *sp, int state)
{
  if (IS_PLAYER(sp))
    sp->expand_control_p = state;
  else XmToggleButtonSetState(EXPAND_BUTTON(sp), (Boolean)state, TRUE);
}


#define CONTRAST_SCROLLBAR_MULT (SCROLLBAR_MAX / 10)
static char contrast_number_buffer[5] ={'0', STR_decimal, '0', '0', '\0'};

static int snd_contrast_to_int(Float val)
{
  if (val < 10.0)
    return(snd_round(val * CONTRAST_SCROLLBAR_MULT));
  else return(SCROLLBAR_MAX);
}

static int snd_contrast_changed(snd_info *sp, int val)
{
  char *sfs;
  sp->contrast_control = (Float)val / CONTRAST_SCROLLBAR_MULT;
  sfs = prettyf(sp->contrast_control, 2);
  fill_number(sfs, contrast_number_buffer);
  set_label(CONTRAST_LABEL(sp), contrast_number_buffer);
  FREE(sfs);
  return(val);
}

void set_snd_contrast(snd_info *sp, Float val)
{
  if (IS_PLAYER(sp))
    sp->contrast_control = val;
  else XtVaSetValues(CONTRAST_SCROLLBAR(sp),
		     XmNvalue,
		     snd_contrast_changed(sp, snd_contrast_to_int(mus_fclamp(0.0, val, 9.0))),
		     NULL);
}

static void contrast_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  XButtonEvent *ev;
  int val;
  ASSERT_WIDGET_TYPE(XmIsPushButton(w), w);
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    val = snd_contrast_to_int(sp->last_contrast_control); 
  else val = 0;
  snd_contrast_changed(sp, val);
  XtVaSetValues(CONTRAST_SCROLLBAR(sp), XmNvalue, val, NULL);
}

static void contrast_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  snd_contrast_changed((snd_info *)context, ((XmScrollBarCallbackStruct *)info)->value);
}

static void contrast_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  snd_contrast_changed(sp, cb->value);
  sp->last_contrast_control = sp->saved_contrast_control;
  sp->saved_contrast_control = sp->contrast_control;
}

static void contrast_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_state *ss;
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  ss = sp->state;
  sp->contrast_control_p = cb->set;
  if (!(ss->using_schemes)) 
    XmChangeColor(CONTRAST_SCROLLBAR(sp), (Pixel)((sp->contrast_control_p) ? ((ss->sgx)->position_color) : ((ss->sgx)->basic_color)));
}

void toggle_contrast_button(snd_info *sp, int state)
{
  if (IS_PLAYER(sp))
    sp->contrast_control_p = state;
  else XmToggleButtonSetState(CONTRAST_BUTTON(sp), (Boolean)state, TRUE);
}


#define REVSCL_SCROLLBAR_MULT (0.60 * SCROLLBAR_MAX)

static char revscl_number_buffer[7] ={'0', STR_decimal, '0', '0', '0', '0', '\0'};
static char number_long_zero[7] ={'0', STR_decimal, '0', '0', '0', '0', '\0'};

static int snd_revscl_to_int(Float val)
{
  return(snd_round(pow(val, 0.333) * REVSCL_SCROLLBAR_MULT));
}

static Float cube (Float a) {return(a*a*a);}

static int snd_revscl_changed(snd_info *sp, int val)
{
  char *fs, *ps, *sfs;
  int i, j;
  sp->reverb_control_scale = cube((Float)val / REVSCL_SCROLLBAR_MULT);
  sfs = prettyf(sp->reverb_control_scale, 3);
  fs = sfs;
  ps = (char *)(revscl_number_buffer);
  j = strlen(fs);
  if (j > 6) j = 6;
  if (j < 6) 
    {
      revscl_number_buffer[5] ='0';
      revscl_number_buffer[4] ='0'; 
      revscl_number_buffer[3] ='0';
      revscl_number_buffer[2] ='0'; 
      revscl_number_buffer[1] = STR_decimal;
    }
  for (i = 0; i < j; i++) (*ps++) = (*fs++);
  set_label(REVSCL_LABEL(sp), revscl_number_buffer);
  FREE(sfs);
  return(val);
}

void set_snd_revscl(snd_info *sp, Float val)
{
  if (IS_PLAYER(sp))
    sp->reverb_control_scale = val;
  else XtVaSetValues(REVSCL_SCROLLBAR(sp),
		     XmNvalue,
		     snd_revscl_changed(sp, snd_revscl_to_int(mus_fclamp(0.0, val, 3.25))),
		     NULL);
}

static void revscl_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  XButtonEvent *ev;
  int val;
  ASSERT_WIDGET_TYPE(XmIsPushButton(w), w);
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    val = snd_revscl_to_int(sp->last_reverb_control_scale); 
  else val = 0;
  snd_revscl_changed(sp, val);
  XtVaSetValues(REVSCL_SCROLLBAR(sp), XmNvalue, val, NULL);
}


static void revscl_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_revscl_changed((snd_info *)context, ((XmScrollBarCallbackStruct *)info)->value);
}

static void revscl_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  snd_revscl_changed(sp, cb->value);
  sp->last_reverb_control_scale = sp->saved_reverb_control_scale;
  sp->saved_reverb_control_scale = sp->reverb_control_scale;
}



#define REVLEN_SCROLLBAR_MULT (SCROLLBAR_MAX / 5.0)

static char revlen_number_buffer[5] ={'1', STR_decimal, '0', '0', '\0'};

static int snd_revlen_to_int(Float val)
{
  return(snd_round(val * REVLEN_SCROLLBAR_MULT));
}

static int snd_revlen_changed(snd_info *sp, int val)
{
  char *sfs;
  sp->reverb_control_length = (Float)val / REVLEN_SCROLLBAR_MULT;
  sfs = prettyf(sp->reverb_control_length, 2);
  fill_number(sfs, revlen_number_buffer);
  set_label(REVLEN_LABEL(sp), revlen_number_buffer);
  FREE(sfs);
  return(val);
}

void set_snd_revlen(snd_info *sp, Float val)
{
  if (IS_PLAYER(sp))
    sp->reverb_control_length = val;
  else XtVaSetValues(REVLEN_SCROLLBAR(sp),
		     XmNvalue,
		     snd_revlen_changed(sp, snd_revlen_to_int(mus_fclamp(0.0, val, 4.5))),
		     NULL);
}

static void revlen_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  XButtonEvent *ev;
  int val;
  ASSERT_WIDGET_TYPE(XmIsPushButton(w), w);
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    val = snd_revlen_to_int(sp->last_reverb_control_length); 
  else val = (int)REVLEN_SCROLLBAR_MULT;
  snd_revlen_changed(sp, val);
  XtVaSetValues(REVLEN_SCROLLBAR(sp), XmNvalue, val, NULL);
}

static void revlen_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  snd_revlen_changed((snd_info *)context, ((XmScrollBarCallbackStruct *)info)->value);
}

static void revlen_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  snd_revlen_changed(sp, cb->value);
  sp->last_reverb_control_length = sp->saved_reverb_control_length;
  sp->saved_reverb_control_length = sp->reverb_control_length;
}



static void reverb_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_state *ss;
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  ss = sp->state;
  sp->reverb_control_p = cb->set;
  if (!(ss->using_schemes))
    {
      XmChangeColor(REVLEN_SCROLLBAR(sp), (Pixel)((sp->reverb_control_p) ? ((ss->sgx)->position_color) : ((ss->sgx)->basic_color)));
      XmChangeColor(REVSCL_SCROLLBAR(sp), (Pixel)((sp->reverb_control_p) ? ((ss->sgx)->position_color) : ((ss->sgx)->basic_color)));
    }
}

void toggle_reverb_button(snd_info *sp, int state)
{
  if (IS_PLAYER(sp))
    sp->reverb_control_p = state;
  else XmToggleButtonSetState(REVERB_BUTTON(sp), (Boolean)state, TRUE);
}

static void filter_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  sp->filter_control_p = cb->set;
}

void toggle_filter_button(snd_info *sp, int state)
{
  if (IS_PLAYER(sp))
    sp->filter_control_p = state;
  else XmToggleButtonSetState(FILTER_BUTTON(sp), (Boolean)state, TRUE);
}

static void filter_textfield_deactivate(snd_info *sp)
{
  chan_info *active_chan;
  Widget graph;
  active_chan = any_selected_channel(sp);
  if (active_chan)
    {
      graph = channel_graph(active_chan);
      if ((XmIsTraversable(graph)) && 
	  (XmGetVisibility(graph) != XmVISIBILITY_FULLY_OBSCURED))
	XmProcessTraversal(graph, XmTRAVERSE_CURRENT);
    }
}

#define MIN_FILTER_GRAPH_HEIGHT 20

static void display_filter_env(snd_info *sp)
{
  snd_state *ss;
  axis_context *ax;
  int height, width;
  Widget drawer;
  ss = sp->state;
  drawer = filter_graph(sp);
  height = widget_height(drawer);
  if (height < MIN_FILTER_GRAPH_HEIGHT) return;
  width = widget_width(drawer);
  ax = (axis_context *)CALLOC(1, sizeof(axis_context));
  ax->gc = (ss->sgx)->fltenv_basic_gc;
  ax->wn = XtWindow(drawer);
  ax->dp = XtDisplay(drawer);
  XClearWindow(ax->dp, ax->wn);
  if (edp_display_graph(ss, 
			sp->sgx->flt,
			"frequency response",
			ax, 0, 0, width, height, 
			sp->filter_control_env, 
			sp->filter_control_in_dB, TRUE))
    {
      ax->gc = (ss->sgx)->fltenv_data_gc;
      display_frequency_response(ss, 
				 sp->filter_control_env, 
				 edp_ap(sp->sgx->flt), ax, 
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

static void filter_drawer_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help_with_wrap((snd_state *)context, 
		     "Filter Frequency Response",
"This graph shows the current filter frequency response envelope, and the actual response (dependent on the filter order). \
See the envelope editor documentation for editing directions.");
}

#ifdef MAC_OSX
static int press_x, press_y;
#endif

static void filter_drawer_button_motion(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  snd_info *sp = (snd_info *)context;
  XMotionEvent *ev = (XMotionEvent *)event;
#ifdef MAC_OSX
  if ((press_x == ev->x) && (press_y == ev->y)) return;
#endif
  edp_handle_point(sp->state, 
		   sp->sgx->flt,
		   ev->x, ev->y, ev->time, 
		   sp->filter_control_env, 
		   sp->filter_control_in_dB,
		   sp->filter_control_env_xmax);
  display_filter_env(sp);
  sp->filter_control_changed = TRUE;
}

static void filter_drawer_button_press(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  snd_info *sp = (snd_info *)context;
  XButtonEvent *ev = (XButtonEvent *)event;
#ifdef MAC_OSX
  press_x = ev->x;
  press_y = ev->y;
#endif
  if (edp_handle_press(sp->state, 
		       sp->sgx->flt,
		       ev->x, ev->y, ev->time, 
		       sp->filter_control_env, 
		       sp->filter_control_in_dB,
		       sp->filter_control_env_xmax))
    display_filter_env(sp);
}

static void filter_drawer_button_release(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  char *tmpstr = NULL;
  snd_info *sp = (snd_info *)context;
  edp_handle_release(sp->sgx->flt, sp->filter_control_env);
  display_filter_env(sp);
  set_filter_text(sp, tmpstr = env_to_string(sp->filter_control_env));
  if (tmpstr) FREE(tmpstr);
  sp->filter_control_changed = TRUE;
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

void set_filter_in_dB(snd_info *sp, int val)
{
  sp->filter_control_in_dB = val;
  if (!(IS_PLAYER(sp)))
    {
      XmToggleButtonSetState(FILTER_DB_BUTTON(sp), (Boolean)val, FALSE);
      display_filter_env(sp);
    }
}

void set_snd_filter_order(snd_info *sp, int order)
{
  char *fltorder;
  if (order & 1) order++;
  if (order <= 0) order = 2;
  sp->filter_control_order = order;
  if (!(IS_PLAYER(sp)))
    {
      fltorder = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
      mus_snprintf(fltorder, LABEL_BUFFER_SIZE, "%d", order);
      XmTextSetString(FILTER_ORDER_TEXT(sp), fltorder);
      FREE(fltorder);
      display_filter_env(sp);
    }
  sp->filter_control_changed = TRUE;
}

static void filter_order_up_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_info *sp = (snd_info *)context;
  set_snd_filter_order(sp, sp->filter_control_order + 2);
}

static void filter_order_down_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_info *sp = (snd_info *)context;
  if (sp->filter_control_order > 2)
    set_snd_filter_order(sp, sp->filter_control_order - 2);
}

static void filter_order_up_help_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_help_with_wrap((snd_state *)context,
		     "Filter Order Increment Button",
"This button causes the filter order to be incremented");
}

static void filter_order_down_help_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_help_with_wrap((snd_state *)context,
		     "Filter Order Decrement Button",
"This button causes the filter order to be decremented");
}

static void filter_activate_callback(Widget w, XtPointer context, XtPointer info)
{
  /* make an envelope out of the data */
  snd_info *sp = (snd_info *)context;
  char *str = NULL;
  int order;
  snd_state *ss;
  XmAnyCallbackStruct *cb = (XmAnyCallbackStruct *)info;
  XKeyEvent *ev;
  KeySym keysym;
  ev = (XKeyEvent *)(cb->event);
  keysym = XKeycodeToKeysym(XtDisplay(w),
			    (int)(ev->keycode),
			    (ev->state & snd_ShiftMask) ? 1 : 0);
  ss = sp->state;
  ss->mx_sp = sp; 

  if ((ev->state & snd_MetaMask) && 
      ((keysym == snd_K_p) || (keysym == snd_K_P) || (keysym == snd_K_n) || (keysym == snd_K_N)))
    {
      restore_filter_string(sp, (keysym == snd_K_p) || (keysym == snd_K_P));
      return;
    }
  str = XmTextGetString(w);
  if ((str) && (*str)) remember_filter_string(sp, str);

  if (sp->filter_control_env) sp->filter_control_env = free_env(sp->filter_control_env);
  sp->filter_control_env = string2env(str);
  if (str) XtFree(str);
  if (!(sp->filter_control_env)) /* maybe user cleared text field? */
    sp->filter_control_env = default_env(sp->filter_control_env_xmax, 1.0);
  str = XmTextGetString(FILTER_ORDER_TEXT(sp));
  if ((str) && (*str))
    {
      order = string2int(str);
      if (order & 1) order++;
      if (order <= 0) order = 2;
      sp->filter_control_order = order;
      XtFree(str);
    }
  edp_edited(sp->sgx->flt);
  display_filter_env(sp);
  filter_textfield_deactivate(sp);
  sp->filter_control_changed = TRUE;
}

static void filter_order_activate_callback(Widget w, XtPointer context, XtPointer info)
{
  char *str;
  int order;
  snd_info *sp = (snd_info *)context;
  str = XmTextGetString(w);
  if ((str) && (*str))
    {
      order = string2int(str);
      if (order & 1) order++;
      if (order <= 0) order = 2;
      sp->filter_control_order = order;
      sp->filter_control_changed = TRUE;
      display_filter_env(sp);
      XtFree(str);
    }
  filter_textfield_deactivate(sp);
}

void filter_env_changed(snd_info *sp, env *e)
{
  /* turn e back into a string for textfield widget */
  char *tmpstr = NULL;
  if (!(IS_PLAYER(sp)))
    {
      XmTextSetString(FILTER_COEFFS_TEXT(sp), tmpstr = env_to_string(e));
      if (tmpstr) FREE(tmpstr);
      edp_edited(sp->sgx->flt);
      display_filter_env(sp);
    }
  sp->filter_control_changed = TRUE;
}

void set_play_button(snd_info *sp, int val)
{
  if ((sp->sgx) && (!(IS_PLAYER(sp))))
    {
      XmToggleButtonSetState(PLAY_BUTTON(sp), (Boolean)val, FALSE);
      set_file_browser_play_button(sp->short_filename, val);
      set_open_file_play_button(val);
    }
}

static void play_button_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_info *sp = (snd_info *)context;
  chan_info *cp;
  snd_state *ss;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  XButtonEvent *ev;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  ev = (XButtonEvent *)(cb->event);
  if (sp->playing) 
    stop_playing_sound(sp);
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
      ss = sp->state;
      XtVaSetValues(w, XmNselectColor, ((sp->cursor_follows_play != DONT_FOLLOW) ? ((ss->sgx)->green) : ((ss->sgx)->pushed_button_color)), NULL);
      play_sound(sp, 0, NO_END_SPECIFIED, IN_BACKGROUND, 
		 C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION),
		 "play_button", 0);
    }
}

typedef struct {int pausing; snd_state *ss;} pause_data;

static void set_play_button_pause(snd_info *sp, void *ptr)
{
  pause_data *pd = (pause_data *)ptr;
  snd_state *ss;
  Widget w;
  if ((sp->playing) && (!(IS_PLAYER(sp))))
    {
      ss = pd->ss;
      w = PLAY_BUTTON(sp);
      if (pd->pausing)
	XtVaSetValues(w, XmNselectColor, (ss->sgx)->red, NULL);
      else XtVaSetValues(w, XmNselectColor, ((sp->cursor_follows_play != DONT_FOLLOW) ? ((ss->sgx)->green) : ((ss->sgx)->pushed_button_color)), NULL);
    }
}

void play_button_pause(snd_state *ss, int pausing)
{
  pause_data *pd;
  pd = (pause_data *)CALLOC(1, sizeof(pause_data));
  pd->pausing = pausing;
  pd->ss = ss;
  for_each_sound(ss, set_play_button_pause, (void *)pd);
  FREE(pd);
}

void set_control_panel_play_button(snd_info *sp, int val)
{
  if ((sp) && (sp->sgx) && (PLAY_BUTTON(sp)))
    set_toggle_button(PLAY_BUTTON(sp), FALSE, FALSE, sp);
}


static void play_arrow_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  int dir;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  dir = cb->set;
  if (dir) sp->speed_control_direction = -1; else sp->speed_control_direction = 1;
}

static void set_sync_color(snd_info *sp)
{
  snd_state *ss;
  Widget syb;
  syb = SYNC_BUTTON(sp);
  ss = sp->state;
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
      XmToggleButtonSetState(SYNC_BUTTON(sp), (Boolean)on, FALSE);
    }
}

static void sync_button_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  chan_info *cp;
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
      set_sync_color(sp);
      cp = sp->lacp;
      if (cp == NULL) cp = any_selected_channel(sp);
      goto_graph(cp);
      if (cp->cursor_on) cursor_moveto(cp, CURSOR(cp));
      apply_x_axis_change(cp->axis, cp, sp);
    }
}

static void unite_button_callback(Widget w, XtPointer context, XtPointer info)
{
  /* click if set unsets, click if unset->combine, ctrl-click->superimpose */
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  XButtonEvent *ev;
  int val;
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
  snd_state *ss;
  XmAnyCallbackStruct *cb = (XmAnyCallbackStruct *)info;
  XKeyEvent *ev;
  KeySym keysym;
  ev = (XKeyEvent *)(cb->event);
  keysym = XKeycodeToKeysym(XtDisplay(w),
			    (int)(ev->keycode),
			    (ev->state & ShiftMask) ? 1 : 0);
  ss = sp->state;
  ss->mx_sp = sp; 
  snd_minibuffer_activate(sp, keysym, (ev->state & snd_MetaMask));
}

static void apply_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* create temp file of run over current file using the current (saved) ctrls state */
  snd_info *sp = (snd_info *)context;
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  XButtonEvent *ev;
  snd_state *ss;
  snd_context *sgx;
  ASSERT_WIDGET_TYPE(XmIsPushButton(w), w);
  sgx = sp->sgx;
  ss = sp->state;
  if (sp->applying) 
    {
      stop_applying(sp);
      if (!(ss->using_schemes)) 
	XmChangeColor(APPLY_BUTTON(sp), (Pixel)((ss->sgx)->basic_color));
      sp->applying = FALSE;
    }
  else
    {
      ss->apply_choice = APPLY_TO_SOUND;
      ev = (XButtonEvent *)(cb->event);
      if (ev->state & snd_ControlMask) 
	{
	  if (selection_is_active())
	    ss->apply_choice = APPLY_TO_SELECTION;
	  else ss->apply_choice = APPLY_TO_CHANNEL;
	}
      sp->applying = TRUE;
      if (!(ss->using_schemes)) 
	XmChangeColor(APPLY_BUTTON(sp), (Pixel)((ss->sgx)->pushed_button_color));
      sgx->apply_in_progress = BACKGROUND_ADD(ss, apply_controls, (Indicium)(make_apply_state_with_implied_beg_and_dur(sp)));
    }
}

/* apply is only safe if the DAC is currently inactive and remains safe only
 * if all other apply buttons are locked out (and play).
 */

static void lockapply(snd_info *sp, void *up) 
{
  if (sp != (snd_info *)up) set_sensitive(APPLY_BUTTON(sp), FALSE);
}

void lock_apply(snd_state *ss, snd_info *sp)
{
  /* if playing or applying, set other applys to insensitive */
  for_each_sound(ss, lockapply, (void *)sp);
}

static void unlockapply(snd_info *sp, void *up) 
{
  if (sp != (snd_info *)up) set_sensitive(APPLY_BUTTON(sp), TRUE);
}

void unlock_apply(snd_state *ss, snd_info *sp)
{
  for_each_sound(ss, unlockapply, (void *)sp);
  if ((sp) && (!(ss->using_schemes))) 
    XmChangeColor(APPLY_BUTTON(sp), (Pixel)((ss->sgx)->basic_color));
}

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
  int i, k;
  Widget child;
  snd_state *ss;
  snd_info *sp;
  /* call_data->params[0]: Commit, Move, Key, Start (as strings) */
  ss = get_global_state();
  if ((call_data->params) && 
      (call_data->params[0]) && 
      (with_relative_panes(ss)) &&
      (sound_style(ss) == SOUNDS_VERTICAL))
    {
      if (strcmp(call_data->params[0], "Start") == 0)
	{
	  int outer_ctr = 0;
	  for (i = 0; i < ss->max_sounds; i++)
	    if ((sp = ((snd_info *)(ss->sounds[i]))) &&
		(sp->inuse) &&
		(sp->nchans > 1) &&
		(sp->channel_style == CHANNELS_SEPARATE))
	      outer_panes++;
	  if (outer_panes > 0)
	    {
	      inner_panes = (int *)CALLOC(outer_panes, sizeof(int));
	      outer_sizes = (int *)CALLOC(outer_panes, sizeof(int));
	      inner_sizes = (int **)CALLOC(outer_panes, sizeof(int *));
	      outer_ctr = 0;
	      for (i = 0; i < ss->max_sounds; i++)
		if ((sp = ((snd_info *)(ss->sounds[i]))) &&
		    (sp->inuse) &&
		    (sp->nchans > 1) &&
		    (sp->channel_style == CHANNELS_SEPARATE))
		  {
		    child = w_snd_pane(sp);
		    inner_panes[outer_ctr] = sp->nchans;
		    inner_sizes[outer_ctr] = (int *)CALLOC(sp->nchans, sizeof(int));
		    XtVaGetValues(child, XmNheight, &(outer_sizes[outer_ctr]), NULL);
		    for (k = 0; k < sp->nchans; k++)
		      XtVaGetValues(channel_main_pane(sp->chans[k]), XmNheight, &(inner_sizes[outer_ctr][k]), NULL);
		    outer_ctr++;
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
		if ((sp = ((snd_info *)(ss->sounds[i]))) &&
		    (sp->inuse) &&
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
  int i, loc = -1;
  if (sashes_size == 0)
    {
      sashes = (Widget *)CALLOC(16, sizeof(Widget));
      sashes_size = 16;
      loc = 0;
    }
  else
    {
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
  Widget child;
  CompositeWidget cw = (CompositeWidget)w;
  for (i = 0; i < cw->composite.num_children; i++) /* only outermost sashes count here */
    {
      child = cw->composite.children[i];
      if ((XtIsWidget(child)) && 
	  (XtIsManaged(child)) && 
	  (XtIsSubclass(child, xmSashWidgetClass)))
	remember_sash(child);
    }
}

#endif

static int cant_write(char *name)
{
#if HAVE_ACCESS
  return((access(name, W_OK)) != 0);
#else
  return(0);
#endif
}

static void save_control_panel_callback(Widget w, XtPointer context, XtPointer info) {save_controls((snd_info *)context);}
static void restore_control_panel_callback(Widget w, XtPointer context, XtPointer info) {restore_controls((snd_info *)context);}
static void reset_control_panel_callback(Widget w, XtPointer context, XtPointer info) {reset_controls((snd_info *)context);}

/* bitmaps for the playback direction arrow */
static unsigned char speed_r_bits1[] = {
   0x00, 0x04, 0x10, 0x08, 0x00, 0x10, 0x04, 0x20, 0x00, 0x40, 0xa5, 0xbf,
   0x00, 0x40, 0x04, 0x20, 0x00, 0x10, 0x10, 0x08, 0x00, 0x04, 0x00, 0x00};
static unsigned char speed_l_bits1[] = {
   0x20, 0x00, 0x10, 0x08, 0x08, 0x00, 0x04, 0x20, 0x02, 0x00, 0xfd, 0xa5,
   0x02, 0x00, 0x04, 0x20, 0x08, 0x00, 0x10, 0x08, 0x20, 0x00, 0x00, 0x00};

#if HAVE_XPM

#define NUM_GLASSES 15
#define NUM_BOMBS 15

static Pixmap mini_lock = 0;
static Pixmap blank_pixmap = 0;
static int mini_lock_allocated = FALSE;
static Pixmap mini_bombs[NUM_BOMBS];
static Pixmap mini_glasses[NUM_GLASSES];

void snd_file_lock_icon(snd_info *sp, int on)
{
  snd_context *sx;
  if (mini_lock) 
    {
      sx = sp->sgx;
      if (on)
	sx->file_pix = mini_lock;
      else sx->file_pix = blank_pixmap;
      XtVaSetValues(NAME_ICON(sp), XmNlabelPixmap, sx->file_pix, NULL);
    }
  /* these Pixmaps can be null if the colormap is screwed up */
}

#define BOMB_TIME 200

static void show_bomb_icon(snd_info *sp, int on)
{
  snd_context *sx;
  if (sp->bomb_ctr >= NUM_BOMBS) sp->bomb_ctr = 0;
  if (mini_bombs[sp->bomb_ctr]) 
    {
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

void x_bomb(snd_info *sp, int on)
{
  show_bomb_icon(sp, on);
  if (on) 
    sp->bomb_ctr++; 
  else sp->bomb_ctr = 0;
}

static void inc_bomb(snd_info *sp, void *ptr)
{
  int *buf;
  if (sp)
    {
      if (sp->need_update)
	{
	  buf = (int *)ptr;
	  buf[0]++;
	  show_bomb_icon(sp, sp->bomb_ctr);
	  sp->bomb_ctr++;
	}
    }
}

static int bomb_in_progress = FALSE;

static void bomb_check(XtPointer context, XtIntervalId *id)
{
  snd_info *sp = (snd_info *)context;
  snd_state *ss;
  int incs[1];
  ss = sp->state;
  incs[0] = 0;
  for_each_sound(ss, inc_bomb, (void *)incs);
  if (incs[0] > 0)
    XtAppAddTimeOut(MAIN_APP(ss),
		    (unsigned long)BOMB_TIME,
		    (XtTimerCallbackProc)bomb_check,
		    context);
  else bomb_in_progress = FALSE;
}

void snd_file_bomb_icon(snd_info *sp, int on)
{
  snd_state *ss;
  if ((on) && (!bomb_in_progress))
    {
      ss = sp->state;
      bomb_in_progress = TRUE;
      XtAppAddTimeOut(MAIN_APP(ss),
		      (unsigned long)BOMB_TIME,
		      (XtTimerCallbackProc)bomb_check,
		      (void *)sp);
    }
}

static void snd_file_glasses_icon(snd_info *sp, int on, int glass)
{
  Widget w;
  snd_context *sx;
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
      sx = sp->sgx;
      XtVaSetValues(w, XmNlabelPixmap, sx->file_pix, NULL);
      XmUpdateDisplay(w);
    }
}

#else
void snd_file_lock_icon(snd_info *sp, int on) {}
void snd_file_bomb_icon(snd_info *sp, int on) 
{
  if (on)
    report_in_minibuffer(sp, "%s has changed since we last read it!", sp->short_filename);
}
static void snd_file_glasses_icon(snd_info *sp, int on, int glass) {}
void x_bomb(snd_info *sp, int on) {}
#endif

static void close_sound_dialog(Widget w, XtPointer context, XtPointer info) 
{
  snd_info *sp = (snd_info *)context;
  if (sp) snd_close_file(sp, sp->state);
}

static Pixmap spd_r, spd_l;
static int spd_ok = FALSE;

static snd_info *add_sound_window_with_parent (Widget parent, char *filename, snd_state *ss, int read_only)
{  
  snd_info *sp = NULL, *osp;
  file_info *hdr = NULL;
  Widget *sw;
  XmString s1;
  int snd_slot, nchans = 1, make_widgets, i, k, need_colors, n, old_chans;
  Arg args[32];
  char *old_name = NULL, *title;
  Dimension app_y, app_dy, screen_y, chan_min_y;
  /* these dimensions are used to try to get a reasonable channel graph size without falling off the screen bottom */
  Pixmap rb, lb;
  int depth, free_filename = FALSE;
  Widget form;
  XtCallbackList n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12;
  snd_context *sx;
  Atom sound_delete;
  static int first_window = TRUE;
  errno = 0;
  hdr = make_file_info(filename, ss);
  if (!hdr) return(NULL);
  if (ss->pending_change) 
    {
      old_name = filename;
      filename = ss->pending_change;
      free_filename = TRUE;
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

  snd_slot = find_free_sound_slot(ss, nchans); /* expands sound list if needed */
  if (ss->sounds[snd_slot]) /* we're trying to re-use an old, inactive set of widgets and whatnot */
    {
      osp = ss->sounds[snd_slot];
      old_chans = osp->allocated_chans;
    }
  else old_chans = 0;
  make_widgets = (ss->sounds[snd_slot] == NULL);
  ss->sounds[snd_slot] = make_snd_info(ss->sounds[snd_slot], ss, filename, hdr, snd_slot, read_only);
  sp = ss->sounds[snd_slot];
  sp->inuse = TRUE;
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
	add_channel_window(sp, i, ss, chan_min_y, 1, NULL, WITH_FW_BUTTONS);
    }

  if (make_widgets)
    {
      need_colors = (!(ss->using_schemes));

      if ((parent == NULL) && (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS))
	{
	  title = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
	  mus_snprintf(title, PRINT_BUFFER_SIZE, "%d: %s", snd_slot, sp->short_filename);
	  if (sx->dialog == NULL)
	    {
	      n = 0;
	      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
	      XtSetArg(args[n], XmNautoUnmanage, FALSE); n++;
	      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
	      XtSetArg(args[n], XmNnoResize, FALSE); n++;
	      XtSetArg(args[n], XmNtransient, FALSE); n++;
	      sx->dialog = XtCreatePopupShell(title, xmDialogShellWidgetClass, MAIN_SHELL(ss), args, n);
	      /* using popup shell here gets around the problem that the shell passes resize requests to all its children
	       * -- as a popup, it's not considered a child, but that means we don't inherit things like popup menus from 
	       * the main shell.
	       */
	      sound_delete = XmInternAtom(XtDisplay(sx->dialog), "WM_DELETE_WINDOW", FALSE);
	      XmAddWMProtocolCallback(sx->dialog, sound_delete, close_sound_dialog, (XtPointer)sp);
	    }
	  else XtVaSetValues(sx->dialog, XmNtitle, title, NULL);
	  FREE(title);
	  if (!XtIsManaged(sx->dialog)) XtManageChild(sx->dialog);
	}

      n = 0;      
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      n = attach_all_sides(args, n);
      XtSetArg(args[n], XmNallowResize, TRUE); n++;
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

      if (parent)
	sw[W_pane] = XtCreateManagedWidget("snd-pane", xmPanedWindowWidgetClass, parent, args, n);
      else
	{
	  if (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS)
	    sw[W_pane] = XtCreateManagedWidget("snd-pane", xmPanedWindowWidgetClass, sx->dialog, args, n);
	  else sw[W_pane] = XtCreateManagedWidget("snd-pane", xmPanedWindowWidgetClass, SOUND_PANE(ss), args, n);
	}

      XtAddCallback(sw[W_pane], XmNhelpCallback, name_help_callback, ss);
      XtAddEventHandler(sw[W_pane], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      /* if user clicks in controls, then starts typing, try to send key events to current active channel */
      /* all widgets in the control-pane that would otherwise intercept the key events get this event handler */

      for (i = 0; i < nchans; i++)
	add_channel_window(sp, i, ss, chan_min_y, 0, NULL, WITH_FW_BUTTONS);
      
      n = 0;      
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNpaneMinimum, ss->ctrls_height); n++;
      XtSetArg(args[n], XmNpaneMaximum, ss->ctrls_height); n++;
      sw[W_control_panel] = XtCreateManagedWidget ("snd-ctrls", xmFormWidgetClass, sw[W_pane], args, n);
      XtAddEventHandler(sw[W_control_panel], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      sw[W_name_form] = XtCreateManagedWidget("snd-name-form", xmFormWidgetClass, sw[W_control_panel], args, n);
      XtAddCallback(sw[W_name_form], XmNhelpCallback, name_help_callback, ss);
      XtAddEventHandler(sw[W_name_form], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);

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
      XtSetArg(args[n], XmNfillOnArm, FALSE); n++;
      sw[W_name] = XtCreateManagedWidget ("snd-name", xmPushButtonWidgetClass, sw[W_name_form], args, n);
      XtAddCallback(sw[W_name], XmNhelpCallback, name_help_callback, ss);
      XtAddEventHandler(sw[W_name], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_name], XmNactivateCallback, name_click_callback, (XtPointer)sp);
      XmStringFree(s1);

#if HAVE_XPM
      if (!mini_lock_allocated)
	{ 
	  Pixmap shape1, shape2, shape3; 
	  XpmAttributes attributes; 
	  XpmColorSymbol symbols[1];
	  int scr, pixerr, k;
	  Display *dp;
	  Drawable wn;
	  dp = XtDisplay(sw[W_name]);
	  wn = XtWindow(sw[W_name]);
	  scr = DefaultScreen(dp);
	  XtVaGetValues(sw[W_name], XmNdepth, &attributes.depth, XmNcolormap, &attributes.colormap, NULL);
	  attributes.visual = DefaultVisual(dp, scr);
	  symbols[0].name = "basiccolor";
	  symbols[0].value = NULL;
	  symbols[0].pixel = (ss->sgx)->basic_color;
	  attributes.colorsymbols = symbols;
	  attributes.numsymbols = 1;
	  attributes.valuemask = XpmColorSymbols | XpmDepth | XpmColormap | XpmVisual;
	  pixerr = XpmCreatePixmapFromData(dp, wn, mini_lock_bits(), &mini_lock, &shape1, &attributes);
	  if (pixerr != XpmSuccess) 
	    snd_error("lock pixmap woe: %d (%s)\n", pixerr, XpmGetErrorString(pixerr));
	  else
	    {
	      pixerr = XpmCreatePixmapFromData(dp, wn, blank_bits(), &blank_pixmap, &shape1, &attributes);
	      if (pixerr != XpmSuccess) 
		snd_error("blank pixmap woe: %d (%s)\n", pixerr, XpmGetErrorString(pixerr));
	      else
		{
		  for (k = 0; k < 15; k++)
		    {
		      pixerr = XpmCreatePixmapFromData(dp, wn, mini_bomb_bits(k), &(mini_bombs[k]), &shape2, &attributes);
		      if (pixerr != XpmSuccess) 
			{
			  snd_error("bomb pixmap woe: %d (%s)\n", pixerr, XpmGetErrorString(pixerr)); 
			  break;
			}
		      pixerr = XpmCreatePixmapFromData(dp, wn, mini_glass_bits(k), &(mini_glasses[k]), &shape3, &attributes);
		      if (pixerr != XpmSuccess) 
			{
			  snd_error("glass pixmap woe: %d (%s)\n", pixerr, XpmGetErrorString(pixerr)); 
			  break;
			}
		    }
		}
	    }
	  mini_lock_allocated = TRUE;
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
      XtSetArg(args[n], XmNlabelType, XmPIXMAP); n++;
      XtSetArg(args[n], XmNlabelPixmap, blank_pixmap); n++;
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
      XtAddCallback(sw[W_info_sep], XmNhelpCallback, info_sep_help_callback, ss);

      n = 0;
      s1 = XmStringCreate("     ", "button_font");
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, sw[W_info_sep]); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_info_sep]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XM_FONT_RESOURCE, BUTTON_FONT(ss)); n++;
      sw[W_info_label] = XtCreateManagedWidget ("snd-info-label", xmLabelWidgetClass, sw[W_name_form], args, n);
      XtAddCallback(sw[W_info_label], XmNhelpCallback, info_help_callback, ss);
      XmStringFree(s1);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_info_label]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XM_FONT_RESOURCE, BOLD_BUTTON_FONT(ss)); n++;
      XtSetArg(args[n], XmNresizeWidth, TRUE); n++;
      XtSetArg(args[n], XmNmarginHeight, 1); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNcolumns, 30); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      sw[W_info] = make_textfield_widget(ss, "snd-info", sw[W_name_form], args, n, ACTIVATABLE, add_completer_func(info_completer));
      XtAddCallback(sw[W_info], XmNhelpCallback, info_help_callback, ss);
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
      XtSetArg(args[n], XM_FONT_RESOURCE, BUTTON_FONT(ss)); n++;
      XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
      /* in Motif 2.2 this sets up a tooltip:
	XtSetArg(args[n], XmNtoolTipString, XmStringCreate("play this sound", XmFONTLIST_DEFAULT_TAG)); n++;
      */
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNselectColor, (ss->sgx)->pushed_button_color); n++;}
      sw[W_play] = make_togglebutton_widget("play", sw[W_name_form], args, n);
      XtAddCallback(sw[W_play], XmNhelpCallback, play_help_callback, ss);
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
      XtSetArg(args[n], XM_FONT_RESOURCE, BUTTON_FONT(ss)); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNselectColor, (ss->sgx)->pushed_button_color); n++;}
      sw[W_sync] = make_togglebutton_widget("sync", sw[W_name_form], args, n);
      XtAddCallback(sw[W_sync], XmNhelpCallback, sync_help_callback, ss);
      XtAddEventHandler(sw[W_sync], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
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
      XtSetArg(args[n], XM_FONT_RESOURCE, BUTTON_FONT(ss)); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNselectColor, (ss->sgx)->pushed_button_color); n++;}
      sw[W_unite] = make_togglebutton_widget("unite", sw[W_name_form], args, n);
      XtAddCallback(sw[W_unite], XmNhelpCallback, unite_help_callback, ss);
      XtAddEventHandler(sw[W_unite], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_unite], XmNvalueChangedCallback, unite_button_callback, (XtPointer)sp);

      n = 0;
      XtVaSetValues(sw[W_control_panel], XmNskipAdjust, TRUE, NULL);

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
      XtAddCallback(sw[W_amp_separator], XmNhelpCallback, amp_help_callback, ss);
      
      /* if control-panel */
      n = 0;      
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_amp_separator]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      sw[W_amp_form] = XtCreateManagedWidget ("snd-amp", xmFormWidgetClass, sw[W_control_panel], args, n);
      XtAddEventHandler(sw[W_amp_form], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);

      n = 0;      
      /* AMP */
      s1 = XmStringCreate("amp:", XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, FALSE); n++;
      sw[W_amp_label] = make_pushbutton_widget ("amp-label", sw[W_amp_form], args, n);
      XtAddCallback(sw[W_amp_label], XmNhelpCallback, amp_help_callback, ss);
      XtAddEventHandler(sw[W_amp_label], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_amp_label], XmNactivateCallback, amp_click_callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;
      s1 = XmStringCreate(number_one, XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_amp_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_amp_label]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
      sw[W_amp_number] = XtCreateManagedWidget ("amp-number", xmLabelWidgetClass, sw[W_amp_form], args, n);
      XtAddCallback(sw[W_amp_number], XmNhelpCallback, amp_help_callback, ss);
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
      XtSetArg(args[n], XmNvalue, (int)SCROLLBAR_MID); n++;
      XtSetArg(args[n], XmNdragCallback, n1 = make_callback_list(amp_drag_callback, (XtPointer)sp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n2 = make_callback_list(amp_valuechanged_callback, (XtPointer)sp)); n++;
      sw[W_amp] = XtCreateManagedWidget("amp", xmScrollBarWidgetClass, sw[W_amp_form], args, n);
      XtAddCallback(sw[W_amp], XmNhelpCallback, amp_help_callback, ss);
      XtAddEventHandler(sw[W_amp], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);

      n = 0;
      /* SRATE */
      s1 = XmStringCreate("speed:", XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_amp_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++; 
      XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, FALSE); n++;
      sw[W_srate_label] = make_pushbutton_widget ("srate-label", sw[W_amp_form], args, n);
      XtAddCallback(sw[W_srate_label], XmNhelpCallback, srate_help_callback, ss);
      XtAddEventHandler(sw[W_srate_label], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_srate_label], XmNactivateCallback, srate_click_callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;
      s1 = initial_speed_label(ss);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_srate_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_srate_label]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++; 
      XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
      sw[W_srate_number] = XtCreateManagedWidget ("srate-number", xmLabelWidgetClass, sw[W_amp_form], args, n);
      XtAddCallback(sw[W_srate_number], XmNhelpCallback, srate_help_callback, ss);
      XmStringFree(s1);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_srate_label]); n++;
      XtSetArg(args[n], XmNindicatorOn, FALSE); n++;
      XtSetArg(args[n], XmNlabelType, XmPIXMAP); n++;
      XtSetArg(args[n], XmNmarginHeight, 0); n++;
      XtSetArg(args[n], XmNmarginWidth, 0); n++;
      XtSetArg(args[n], XmNmarginTop, 0); n++;
      XtSetArg(args[n], XmNtopOffset, 0); n++;
      sw[W_srate_arrow] = make_togglebutton_widget("dir", sw[W_amp_form], args, n);
      form = sw[W_srate_arrow];
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
	  spd_ok = TRUE;
	}
      XtVaSetValues(form, XmNselectPixmap, spd_l, XmNlabelPixmap, spd_r, NULL);
      XtAddCallback(sw[W_srate_arrow], XmNhelpCallback, srate_arrow_help_callback, ss);
      XtAddEventHandler(sw[W_srate_arrow], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_srate_arrow], XmNvalueChangedCallback, play_arrow_callback, (XtPointer)sp);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->position_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_srate_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_srate_number]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, sw[W_srate_arrow]); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNvalue, (int)SPEED_SCROLLBAR_MID); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNdragCallback, n3 = make_callback_list(srate_drag_callback, (XtPointer)sp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n4 = make_callback_list(srate_valuechanged_callback, (XtPointer)sp)); n++;
      sw[W_srate] = XtCreateManagedWidget("speed-scroll", xmScrollBarWidgetClass, sw[W_amp_form], args, n);
      XtAddCallback(sw[W_srate], XmNhelpCallback, srate_help_callback, ss);
      XtAddEventHandler(sw[W_srate], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);

      n = 0;
      /* EXPAND */
      s1 = XmStringCreate("expand:", XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_srate_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, FALSE); n++;
      sw[W_expand_label] = make_pushbutton_widget ("expand-label", sw[W_amp_form], args, n);
      XtAddCallback(sw[W_expand_label], XmNhelpCallback, expand_help_callback, ss);
      XtAddEventHandler(sw[W_expand_label], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_expand_label], XmNactivateCallback, expand_click_callback, (XtPointer)sp);
      XmStringFree(s1);
      
      n = 0;
      s1 = XmStringCreate(number_one, XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_expand_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_expand_label]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
      sw[W_expand_number] = XtCreateManagedWidget ("expand-number", xmLabelWidgetClass, sw[W_amp_form], args, n);
      XtAddCallback(sw[W_expand_number], XmNhelpCallback, expand_help_callback, ss);
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
      XtAddCallback(sw[W_expand_button], XmNhelpCallback, expand_button_help_callback, ss);
      XtAddEventHandler(sw[W_expand_button], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
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
      XtSetArg(args[n], XmNvalue, (int)EXPAND_SCROLLBAR_MID); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNdragCallback, n5 = make_callback_list(expand_drag_callback, (XtPointer)sp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n6 = make_callback_list(expand_valuechanged_callback, (XtPointer)sp)); n++;
      sw[W_expand] = XtCreateManagedWidget("expand-scroll", xmScrollBarWidgetClass, sw[W_amp_form], args, n);
      XtAddCallback(sw[W_expand], XmNhelpCallback, expand_help_callback, ss);
      XtAddEventHandler(sw[W_expand], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);


      /* CONTRAST */
      n = 0;
      s1 = XmStringCreate("contrast:", XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_expand_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, FALSE); n++;
      sw[W_contrast_label] = make_pushbutton_widget ("contrast-label", sw[W_amp_form], args, n);
      XtAddCallback(sw[W_contrast_label], XmNhelpCallback, contrast_help_callback, ss);
      XtAddEventHandler(sw[W_contrast_label], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_contrast_label], XmNactivateCallback, contrast_click_callback, (XtPointer)sp);
      XmStringFree(s1);
      
      n = 0;
      s1 = XmStringCreate(number_zero, XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_contrast_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_contrast_label]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
      sw[W_contrast_number] = XtCreateManagedWidget ("contrast-number", xmLabelWidgetClass, sw[W_amp_form], args, n);
      XtAddCallback(sw[W_contrast_number], XmNhelpCallback, contrast_help_callback, ss);
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
      XtAddCallback(sw[W_contrast_button], XmNhelpCallback, contrast_button_help_callback, ss);
      XtAddEventHandler(sw[W_contrast_button], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
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
      XtAddCallback(sw[W_contrast], XmNhelpCallback, contrast_help_callback, ss);
      XtAddEventHandler(sw[W_contrast], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);

      /* REVERB */
      /* REVSCL */
      n = 0;
      s1 = XmStringCreate("reverb:", XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_contrast_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, FALSE); n++;
      sw[W_revscl_label] = make_pushbutton_widget ("revscl-label", sw[W_amp_form], args, n);
      XtAddCallback(sw[W_revscl_label], XmNhelpCallback, revscl_help_callback, ss);
      XtAddEventHandler(sw[W_revscl_label], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_revscl_label], XmNactivateCallback, revscl_click_callback, (XtPointer)sp);
      XmStringFree(s1);
      
      n = 0;
      s1 = XmStringCreate(number_long_zero, XmFONTLIST_DEFAULT_TAG);
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
      XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
      sw[W_revscl_number] = XtCreateManagedWidget ("revscl-number", xmLabelWidgetClass, sw[W_amp_form], args, n);
      XtAddCallback(sw[W_revscl_number], XmNhelpCallback, revscl_help_callback, ss);
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
      XtAddCallback(sw[W_revscl], XmNhelpCallback, revscl_help_callback, ss);
      XtAddEventHandler(sw[W_revscl], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);

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
      XtAddCallback(sw[W_reverb_button], XmNhelpCallback, reverb_button_help_callback, ss);
      XtAddEventHandler(sw[W_reverb_button], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_reverb_button], XmNvalueChangedCallback, reverb_button_callback, (XtPointer)sp);
      XmStringFree(s1);


      /* REVLEN */
      n = 0;
      s1 = XmStringCreate("len:", XmFONTLIST_DEFAULT_TAG);
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
      XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, FALSE); n++;
      sw[W_revlen_label] = make_pushbutton_widget("revlen-label", sw[W_amp_form], args, n);
      XtAddCallback(sw[W_revlen_label], XmNhelpCallback, revlen_help_callback, ss);
      XtAddEventHandler(sw[W_revlen_label], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_revlen_label], XmNactivateCallback, revlen_click_callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;
      s1 = XmStringCreate(number_one, XmFONTLIST_DEFAULT_TAG);
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
      XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
      sw[W_revlen_number] = XtCreateManagedWidget("revlen-number", xmLabelWidgetClass, sw[W_amp_form], args, n);
      XtAddCallback(sw[W_revlen_number], XmNhelpCallback, revlen_help_callback, ss);
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
      XtSetArg(args[n], XmNvalue, (int)REVLEN_SCROLLBAR_MULT); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNdragCallback, n11 = make_callback_list(revlen_drag_callback, (XtPointer)sp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n12 = make_callback_list(revlen_valuechanged_callback, (XtPointer)sp)); n++;
      sw[W_revlen] = XtCreateManagedWidget("revlen-scroll", xmScrollBarWidgetClass, sw[W_amp_form], args, n);
      XtAddCallback(sw[W_revlen], XmNhelpCallback, revlen_help_callback, ss);
      XtAddEventHandler(sw[W_revlen], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);


      /* FILTER */
      n = 0;
      s1 = XmStringCreate("filter:", XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_revscl_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, FALSE); n++;
#ifdef SGI
      sw[W_filter_label] = XtCreateManagedWidget ("filter-label", xmPushButtonWidgetClass, sw[W_amp_form], args, n);
#else
      sw[W_filter_label] = XtCreateManagedWidget ("filter-label", xmLabelWidgetClass, sw[W_amp_form], args, n);
#endif
      XtAddCallback(sw[W_filter_label], XmNhelpCallback, filter_help_callback, ss);
      XmStringFree(s1);

      /* filter order */
      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNresizeWidth, FALSE); n++;
      XtSetArg(args[n], XmNcolumns, 3); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_filter_label]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_filter_label]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
      sw[W_filter_order] = make_textfield_widget(ss, "filter-order", sw[W_amp_form], args, n, ACTIVATABLE, NO_COMPLETER);
      XmTextSetString(sw[W_filter_order], " 20");
      XtAddCallback(sw[W_filter_order], XmNhelpCallback, filter_order_help_callback, ss);
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
      XtAddEventHandler(sw[W_filter_order_down], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_filter_order_down], XmNhelpCallback, filter_order_down_help_callback, ss);
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
      XtAddEventHandler(sw[W_filter_order_up], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_filter_order_up], XmNhelpCallback, filter_order_up_help_callback, ss);
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
      XtAddCallback(sw[W_filter_button], XmNhelpCallback, filter_button_help_callback, ss);
      XtAddEventHandler(sw[W_filter_button], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_filter_button], XmNvalueChangedCallback, filter_button_callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;
      s1 = XmStringCreate("dB", XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_filter_button]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, sw[W_filter_button]); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++; 
      XtSetArg(args[n], XmNvalue, sp->filter_control_in_dB); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNselectColor, (ss->sgx)->pushed_button_color); n++;}
      sw[W_filter_dB] = make_togglebutton_widget("fltdB", sw[W_amp_form], args, n);
      XtAddCallback(sw[W_filter_dB], XmNhelpCallback, filter_dB_help_callback, ss);
      XtAddEventHandler(sw[W_filter_dB], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_filter_dB], XmNvalueChangedCallback, filter_dB_callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XM_FONT_RESOURCE, BOLD_BUTTON_FONT(ss)); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_filter_order_down]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sw[W_filter_order_down]); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, sw[W_filter_dB]); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      sw[W_filter] = make_textfield_widget(ss, "filter-window", sw[W_amp_form], args, n, ACTIVATABLE, add_completer_func(filename_completer));
      XtAddCallback(sw[W_filter], XmNhelpCallback, filter_envelope_help_callback, ss);
      XtAddCallback(sw[W_filter], XmNactivateCallback, filter_activate_callback, (XtPointer)sp);

      /* APPLY */
      n = 0;
      if (need_colors) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;
	  XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;
	  XtSetArg(args[n], XmNfillOnArm, TRUE); n++;
	}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 25); n++;
      sw[W_apply] = make_pushbutton_widget("Apply", sw[W_amp_form], args, n);
      XtAddCallback(sw[W_apply], XmNhelpCallback, apply_help_callback, ss);
      XtAddEventHandler(sw[W_apply], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_apply], XmNactivateCallback, apply_callback, (XtPointer)sp);

      /* SAVE */
      n = 0;
      if (need_colors) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;
	  XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;
	  XtSetArg(args[n], XmNfillOnArm, TRUE); n++;
	}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 25); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 50); n++;
      sw[W_remember] = make_pushbutton_widget("Remember", sw[W_amp_form], args, n);
      XtAddCallback(sw[W_remember], XmNhelpCallback, remember_help_callback, ss);
      XtAddEventHandler(sw[W_remember], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_remember], XmNactivateCallback, save_control_panel_callback, (XtPointer)sp);

      /* RESTORE */
      n = 0;
      if (need_colors) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;
	  XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;
	  XtSetArg(args[n], XmNfillOnArm, TRUE); n++;
	}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 50); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 75); n++;
      sw[W_restore] = make_pushbutton_widget("Restore", sw[W_amp_form], args, n);
      XtAddCallback(sw[W_restore], XmNhelpCallback, restore_help_callback, ss);
      XtAddEventHandler(sw[W_restore], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_restore], XmNactivateCallback, restore_control_panel_callback, (XtPointer)sp);

      /* RESET */
      n = 0;
      if (need_colors) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;
	  XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;
	  XtSetArg(args[n], XmNfillOnArm, TRUE); n++;
	}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 75); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      sw[W_reset] = XtCreateManagedWidget("Reset", xmPushButtonWidgetClass, sw[W_amp_form], args, n);
      XtAddCallback(sw[W_reset], XmNhelpCallback, reset_help_callback, ss);
      XtAddEventHandler(sw[W_reset], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_reset], XmNactivateCallback, reset_control_panel_callback, (XtPointer)sp);


      /* FILTER GRAPH */
      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_filter]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, sw[W_apply]); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 4); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 98); n++;
      XtSetArg(args[n], XmNallowResize, TRUE); n++;
      /* if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;} */
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 4); n++;
      sw[W_filter_frame] = XtCreateManagedWidget("filter-frame", xmFrameWidgetClass, sw[W_amp_form], args, n);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      n = attach_all_sides(args, n);
      XtSetArg(args[n], XmNallowResize, TRUE); n++;
      sw[W_filter_env] = XtCreateManagedWidget("filter-window", xmDrawingAreaWidgetClass, sw[W_filter_frame], args, n);
      XtAddCallback(sw[W_filter_env], XmNhelpCallback, filter_drawer_help_callback, ss);
      XtAddCallback(sw[W_filter_env], XmNresizeCallback, filter_drawer_resize, (XtPointer)sp);
      XtAddCallback(sw[W_filter_env], XmNexposeCallback, filter_drawer_resize, (XtPointer)sp);

      sp->sgx->flt = new_env_editor();

      XtAddEventHandler(sw[W_filter_env], ButtonPressMask, FALSE, filter_drawer_button_press, sp);
      XtAddEventHandler(sw[W_filter_env], ButtonMotionMask, FALSE, filter_drawer_button_motion, sp);
      XtAddEventHandler(sw[W_filter_env], ButtonReleaseMask, FALSE, filter_drawer_button_release, sp);
      XtAddEventHandler(sw[W_filter_env], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      FREE(n1); FREE(n2); FREE(n3); FREE(n4); FREE(n5); FREE(n6);
      FREE(n7); FREE(n8); FREE(n9); FREE(n10); FREE(n11); FREE(n12);
      /* end if control-panel */
#if (XmVERSION > 1)
      if (sound_style(ss) == SOUNDS_IN_NOTEBOOK)
	{
	  char name[MAX_NOTEBOOK_TAB_LENGTH + 11];
	  strncpy(name, just_filename(sp->short_filename), MAX_NOTEBOOK_TAB_LENGTH);
	  name[MAX_NOTEBOOK_TAB_LENGTH] ='\0';
	  n = 0;
	  if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->graph_color); n++;}
	  XtSetArg(args[n], XmNnotebookChildType, XmMAJOR_TAB); n++;
	  sx->tab = XtCreateManagedWidget(name, xmPushButtonWidgetClass, SOUND_PANE(ss), args, n);
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
		      XmNpaneMinimum, ss->ctrls_height,
		      XmNpaneMaximum, ss->ctrls_height,
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
	add_channel_window(sp, k, ss, chan_min_y, 0, NULL, WITH_FW_BUTTONS);
      set_button_label(sw[W_name], shortname_indexed(sp));
      if (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS)
	XtVaSetValues(sw[W_control_panel],
		      XmNpaneMinimum, 1,
		      XmNpaneMaximum, LOTSA_PIXELS,
		      NULL);
#if (XmVERSION > 1)
      if (sound_style(ss) == SOUNDS_IN_NOTEBOOK)
	{
	  set_label(sx->tab, just_filename(sp->short_filename));
	}
#endif
    }
  if (sp->nchans == 1) 
    {
      XmToggleButtonSetState(unite_button(sp), FALSE, FALSE);
      XtUnmanageChild(unite_button(sp));
    }
  add_sound_data(filename, sp, ss, WITH_GRAPH);
  snd_file_lock_icon(sp, (sp->read_only || (cant_write(sp->filename))));
  if (old_name)
    report_in_minibuffer(sp, "(translated %s)", old_name);
  if (!(ss->using_schemes)) map_over_children(SOUND_PANE(ss), color_sashes, (void *)ss);
  if (!(auto_resize(ss))) equalize_all_panes(ss);
  
  if (first_window)
    {
      /* try to get the pane height that shows everything except the filter graph (hidden for my amusement) */
      /* this calculation assumes the window is built amp_form down, then record buttons up, then filter_frame */
      Position fey, cy, rsy;
      /* if control-panel */
      XtVaGetValues(sw[W_amp_form], XmNy, &cy, NULL);
      XtVaGetValues(sw[W_filter_frame], XmNy, &fey, NULL);
      XtVaGetValues(sw[W_apply], XmNy, &rsy, NULL);
      /* end if control-panel */
      ss->open_ctrls_height = fey + ((rsy < 0) ? (-rsy) : rsy) + cy - 1;
      first_window = FALSE;
    } 
  if (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS)
    {
      /* if control-panel */
      if (make_widgets) 
	XtVaSetValues(sw[W_control_panel],
		      XmNpaneMaximum, LOTSA_PIXELS,
		      NULL); /* locked above to force correct initial setup */
      reset_controls(sp);
      /* end if control-panel */
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
      if (nchans > 1) equalize_all_panes(ss);
    }
  after_open(sp->index);
  if (free_filename) FREE(filename);
  return(sp);
}

snd_info *add_sound_window (char *filename, snd_state *ss, int read_only)
{
  return(add_sound_window_with_parent(NULL, filename, ss, read_only));
}

void snd_info_cleanup(snd_info *sp)
{
  snd_context *sx;
  snd_state *ss;
  if ((sp) && (sp->sgx))
    {
      sx = sp->sgx;
      ss = sp->state;
      if (SYNC_BUTTON(sp))
	{
	  XtVaSetValues(SYNC_BUTTON(sp), XmNset, FALSE, NULL);
	  XtVaSetValues(EXPAND_BUTTON(sp), XmNset, FALSE, NULL);
	  XtVaSetValues(CONTRAST_BUTTON(sp), XmNset, FALSE, NULL);
	  XtVaSetValues(SRATE_ARROW(sp), XmNset, FALSE, NULL);
	  XtVaSetValues(FILTER_BUTTON(sp), XmNset, FALSE, NULL);
	  XtVaSetValues(REVERB_BUTTON(sp), XmNset, FALSE, NULL);
	  XmToggleButtonSetState(unite_button(sp), FALSE, FALSE);
	  sp->channel_style = CHANNELS_SEPARATE;
#if (XmVERSION > 1)
	  if (sound_style(ss) == SOUNDS_IN_NOTEBOOK)
	    {
	      set_label((sp->sgx)->tab, "none");
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

void set_apply_button(snd_info *sp, int val) 
{
  XmToggleButtonSetState(APPLY_BUTTON(sp), (Boolean)val, FALSE);
}


/* ---------------- normalize sounds ---------------- */

static void even_channels(snd_info *sp, void *ptr)
{
  int val, height, chans, i;
  chan_info *cp;
  chans = sp->nchans;
  if (chans > 1)
    {
      height = (*((int *)ptr));
      val = height / chans - 16;
      if (val < 6) val = 6;
      for (i = 0; i < chans; i++)
	{
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

void equalize_sound_panes(snd_state *ss, snd_info *sp, chan_info *ncp, int all_panes)
{
  /* make sp look ok, squeezing others if needed */
  /* if there's already enough (i.e. ss->channel_min_height), just return */
  /* this is used in goto_next_graph and goto_previous_graph (snd-chn.c) to open windows that are currently squeezed shut */
  Float low, high;
  Dimension chan_y, total = 0;
  int *wid;
  int i;
  chan_info *cp = NULL;
  if ((!ss) || (!sp) || (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS)) return;
  if (sound_style(ss) != SOUNDS_HORIZONTAL)
    {
      if ((all_panes) && (sp->nchans > 1))
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
	  XtVaGetValues(channel_main_pane(ncp), XmNheight, &chan_y, NULL);
	  if (chan_y < (Dimension)(ss->channel_min_height >> 1)) 
	    {
	      wid = (int *)CALLOC(1, sizeof(int));
	      wid[0] = (ss->channel_min_height >> 1) + 10;
	      channel_lock_pane(ncp, (void *)wid);
	      channel_open_pane(ncp, NULL);
	      channel_unlock_pane(ncp, NULL);
	      FREE(wid);
	      wid = NULL;
	    }
	}
    }
  else
    {
      XtVaGetValues(channel_main_pane(ncp), XmNwidth, &chan_y, NULL);
      if (chan_y < 200)
	{
	  XtUnmanageChild(channel_main_pane(ncp));
	  XtVaSetValues(channel_main_pane(ncp), XmNwidth, 200, NULL);
	  XtManageChild(channel_main_pane(ncp));
	}
    }
  if (sp->channel_style == CHANNELS_COMBINED)
    {
      cp = any_selected_channel(sp);
      high = (Float)(sp->nchans - cp->chan) / (Float)sp->nchans;
      low = high - 1.0 / (Float)sp->nchans;
      cp = sp->chans[0];
      fixup_gsy(cp, low, high);
    }
}



void color_filter_waveform(snd_state *ss, Pixel color)
{
  int i;
  snd_info *sp;
  XSetForeground(MAIN_DISPLAY(ss), (ss->sgx)->fltenv_data_gc, color);
  (ss->sgx)->filter_waveform_color = color;
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse)) display_filter_env(sp);
    }
}

void reflect_amp_env_completion(snd_info *sp)
{
  chan_info *cp;
  env_info *ep;
  int i;
  /* a channel completed an amp env, check to see if all are complete */
  for (i = 0; i < sp->nchans; i++)
    {
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

void equalize_all_panes(snd_state *ss)
{
  /* normalize: get size, #chans, #snds, set pane minima, force remanage(?), unlock */
  int sounds = 0, chans, chan_y, height, width, screen_y, i;
  int wid[1];
  snd_info *nsp;
  if (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS)
    {
      for (i = 0; i < ss->max_sounds; i++)
	if (snd_ok(ss->sounds[i]))
	  {
	    nsp = ss->sounds[i];
	    if (nsp->nchans > 1)
	      {
		height = widget_height(w_snd_pane(nsp));
		even_channels(nsp, (void *)(&height));
		map_over_sound_chans(nsp, channel_open_pane, NULL);
		map_over_sound_chans(nsp, channel_unlock_pane, NULL);
	      }
	  }
      return;
    }
  for (i = 0; i < ss->max_sounds; i++) if (snd_ok(ss->sounds[i])) sounds++;
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
      else XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, TRUE, NULL); /* need temporary resize to change pane sizes below */
      chans = active_channels(ss, WITHOUT_VIRTUAL_CHANNELS);
      if (chans > 1)
	{
	  /* now we try to make room for the sound ctrl bar, each channel, perhaps the menu */
	  chan_y = (height - (sounds * ss->ctrls_height)) / chans - 16;
	  /* probably can be 14 or 12 -- seems to be margin related or something */
	  wid[0] = chan_y;
	  for_each_sound(ss, sound_lock_control_panel, NULL);
	  map_over_separate_chans(ss, channel_lock_pane, (void *)wid);
	  map_over_separate_chans(ss, channel_open_pane, NULL);
	  map_over_separate_chans(ss, channel_unlock_pane, NULL);
	  for_each_sound(ss, sound_unlock_control_panel, NULL);
	}
      unlock_listener_pane();
      if (!(auto_resize(ss))) XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, FALSE, NULL);
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
	      for_each_sound(ss, even_sounds, (void *)(&width));
	      for_each_sound(ss, sound_open_pane, NULL);
	      for_each_sound(ss, sound_unlock_pane, NULL);
	    }
	  for_each_sound(ss, sound_lock_control_panel, NULL);
	  for_each_sound(ss, even_channels, (void *)(&height));
	  map_over_separate_chans(ss, channel_open_pane, NULL);   /* manage the channel widgets */
	  map_over_separate_chans(ss, channel_unlock_pane, NULL); /* allow pane to be resized */
	  for_each_sound(ss, sound_unlock_control_panel, NULL);
	}
    }
}

void sound_show_ctrls(snd_info *sp)
{
  snd_state *ss;
  ss = sp->state;
  XtUnmanageChild(CONTROL_PANEL(sp));
  XtVaSetValues(CONTROL_PANEL(sp),
		XmNpaneMinimum, ss->open_ctrls_height,
		XmNpaneMaximum, ss->open_ctrls_height,
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

int control_panel_open(snd_info *sp)
{
  Dimension hgt;
  XtVaGetValues(CONTROL_PANEL(sp), XmNheight, &hgt, NULL);
  return(hgt > CLOSED_CTRLS_HEIGHT);
}

void show_controls(snd_state *ss)
{
  snd_info *sp;
  int i;
  ss->ctrls_height = ss->open_ctrls_height;
  set_view_ctrls_label("Hide controls");
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse)) 
	sound_show_ctrls(sp);
    }
}

void hide_controls(snd_state *ss)
{
  snd_info *sp;
  int i;
  ss->ctrls_height = CLOSED_CTRLS_HEIGHT;
  set_view_ctrls_label("Show controls");
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse)) 
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

void progress_report(snd_info *sp, const char *funcname, int curchan, int chans, Float pct, int from_enved)
{
  int which;
#if HAVE_XPM
  char glass_num[8];
  which = (int)(pct * NUM_GLASSES);
  if (which >= NUM_GLASSES) which = NUM_GLASSES - 1;
  if (which < 0) which = 0;
  if (from_enved)
    display_enved_progress(NULL, mini_glasses[which]);
  else snd_file_glasses_icon(sp, TRUE, which);
  if (chans > 1) 
    {
      mus_snprintf(glass_num, 8, "[%d]", curchan);
      make_minibuffer_label(sp, glass_num);
    }
#else
  char *expr_str;
  expr_str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  which = (int)(100.0 * pct);
  if (chans > 1)
    mus_snprintf(expr_str, PRINT_BUFFER_SIZE, "%s: (%d of %d) %d%%", funcname, curchan, chans, which);
  else mus_snprintf(expr_str, PRINT_BUFFER_SIZE, "%s: %d%%", funcname, which);
  if (from_enved)
    display_enved_progress(expr_str, 0);
  else report_in_minibuffer(sp, expr_str);
  FREE(expr_str);
#endif
  check_for_event(sp->state);
}

void finish_progress_report(snd_info *sp, int from_enved)
{
#if HAVE_XPM
  if (from_enved)
    display_enved_progress(NULL, blank_pixmap);
  else snd_file_glasses_icon(sp, FALSE, 0);
  clear_minibuffer_prompt(sp);
#else
  snd_state *ss;
  ss = get_global_state();
  if (from_enved)
    display_enved_progress((ss->stopped_explicitly) ? "stopped" : "", 0);
  else report_in_minibuffer(sp, (ss->stopped_explicitly) ? "stopped" : "");
#endif
}

void start_progress_report(snd_info *sp, int from_enved)
{
#if HAVE_XPM
  if (!(from_enved)) snd_file_glasses_icon(sp, TRUE, 0);
#else
  if (from_enved)
    display_enved_progress("", 0);
#endif
}

static XEN g_sound_widgets(XEN snd)
{
  #define H_sound_widgets "(" S_sound_widgets " snd) -> list of \
widgets: (0)pane (1)name (2)control-panel (3)minibuffer (4)play-button (5)filter-env (6)unite-button (7)name-label (8)name-icon (9)sync-button"
  snd_info *sp;
  ASSERT_SOUND(S_sound_widgets, snd, 1);
  sp = get_sp(snd);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_sound_widgets, snd));
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

#ifdef XEN_ARGIFY_1
  XEN_ARGIFY_1(g_sound_widgets_w, g_sound_widgets)
#else
  #define g_sound_widgets_w g_sound_widgets
#endif

void g_init_gxsnd(void)
{
  XEN_DEFINE_PROCEDURE(S_sound_widgets, g_sound_widgets_w, 0, 1, 0, H_sound_widgets);
}



