#include "snd.h"

/* TODO:   when quad file is opened, its panes are a mess (normalize?)
 * TODO    paned windows should use ratios (not doable currently -- need own panedwindow widget)
 * TODO    in -separate mode (and elsewhere?) need to save description (sizes) of window/channels etc 
 * TODO    amp slider for each chan (as in mix panel)
 * TODO    gmeteor tie-in directly to filter text widget
 */

#if HAVE_XPM
  #include <X11/xpm.h>
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
      W_play, W_sync, W_combine,
      W_ctrls
};

#define NUM_SND_WIDGETS 48

Widget w_snd_ctrls(snd_info *sp)                   {if ((sp) && (sp->sgx)) return(((snd_context *)(sp->sgx))->snd_widgets[W_ctrls]); else return(NULL);}
Widget w_snd_pane(snd_info *sp)                    {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_pane]);                   else return(NULL);}
Widget w_snd_name(snd_info *sp)                    {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_name]);                   else return(NULL);}
Widget w_snd_combine(snd_info *sp)                 {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_combine]);                else return(NULL);}
Widget w_snd_play(snd_info *sp)                    {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_play]);                   else return(NULL);}
Widget w_snd_filter_env(snd_info *sp)              {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_filter_env]);             else return(NULL);}

static Widget w_snd_amp(snd_info *sp)              {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_amp]);                    else return(NULL);}
static Widget w_snd_srate(snd_info *sp)            {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_srate]);                  else return(NULL);}
static Widget w_snd_srate_arrow(snd_info *sp)      {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_srate_arrow]);            else return(NULL);}
static Widget w_snd_expand(snd_info *sp)           {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_expand]);                 else return(NULL);}
static Widget w_snd_contrast(snd_info *sp)         {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_contrast]);               else return(NULL);}
static Widget w_snd_revscl(snd_info *sp)           {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_revscl]);                 else return(NULL);}
static Widget w_snd_revlen(snd_info *sp)           {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_revlen]);                 else return(NULL);}
static Widget w_snd_minibuffer(snd_info *sp)       {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_info]);                   else return(NULL);}
static Widget w_snd_minibuffer_label(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_info_label]);             else return(NULL);}
static Widget w_snd_amp_number(snd_info *sp)       {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_amp_number]);             else return(NULL);}
static Widget w_snd_srate_number(snd_info *sp)     {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_srate_number]);           else return(NULL);}
static Widget w_snd_expand_number(snd_info *sp)    {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_expand_number]);          else return(NULL);}
static Widget w_snd_expand_button(snd_info *sp)    {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_expand_button]);          else return(NULL);}
static Widget w_snd_contrast_number(snd_info *sp)  {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_contrast_number]);        else return(NULL);}
static Widget w_snd_contrast_button(snd_info *sp)  {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_contrast_button]);        else return(NULL);}
static Widget w_snd_revscl_number(snd_info *sp)    {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_revscl_number]);          else return(NULL);}
static Widget w_snd_revlen_number(snd_info *sp)    {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_revlen_number]);          else return(NULL);}
static Widget w_snd_reverb_button(snd_info *sp)    {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_reverb_button]);          else return(NULL);}
static Widget w_snd_apply(snd_info *sp)            {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_apply]);                  else return(NULL);}
static Widget w_snd_filter_order(snd_info *sp)     {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_filter_order]);           else return(NULL);}
static Widget w_snd_filter(snd_info *sp)           {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_filter]);                 else return(NULL);}
static Widget w_snd_filter_button(snd_info *sp)    {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_filter_button]);          else return(NULL);}
static Widget w_snd_filter_dB(snd_info *sp)        {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_filter_dB]);              else return(NULL);}
static Widget w_snd_name_icon(snd_info *sp)        {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_name_icon]);              else return(NULL);}
static Widget w_snd_minibuffer_sep(snd_info *sp)   {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_info_sep]);               else return(NULL);}
static Widget w_snd_sync(snd_info *sp)             {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_sync]);                   else return(NULL);}

#define MAX_NOTEBOOK_TAB_LENGTH 5

static void W_info_Help_Callback(Widget w, XtPointer context, XtPointer info)            {click_for_minibuffer_help((snd_state *)context);}
static void W_play_Help_Callback(Widget w, XtPointer context, XtPointer info)            {click_for_play_help((snd_state *)context);}
static void W_info_sep_Help_Callback(Widget w, XtPointer context, XtPointer info)        {click_for_name_separator_help((snd_state *)context);}
static void W_amp_Help_Callback(Widget w, XtPointer context, XtPointer info)             {click_for_amp_help((snd_state *)context);}
static void W_srate_Help_Callback(Widget w, XtPointer context, XtPointer info)           {click_for_speed_help((snd_state *)context);}
static void W_srate_arrow_Help_Callback(Widget w, XtPointer context, XtPointer info)     {click_for_srate_arrow_help((snd_state *)context);}
static void W_expand_Help_Callback(Widget w, XtPointer context, XtPointer info)          {click_for_expand_help((snd_state *)context);}
static void W_contrast_Help_Callback(Widget w, XtPointer context, XtPointer info)        {click_for_contrast_help((snd_state *)context);}
static void W_revscl_Help_Callback(Widget w, XtPointer context, XtPointer info)          {click_for_reverb_scale_help((snd_state *)context);}
static void W_revlen_Help_Callback(Widget w, XtPointer context, XtPointer info)          {click_for_reverb_length_help((snd_state *)context);}
static void W_filter_Help_Callback(Widget w, XtPointer context, XtPointer info)          {click_for_filter_help((snd_state *)context);} 
static void W_filter_order_Help_Callback(Widget w, XtPointer context, XtPointer info)    {click_for_filter_order_help((snd_state *)context);}
static void W_filter_envelope_Help_Callback(Widget w, XtPointer context, XtPointer info) {click_for_filter_envelope_help((snd_state *)context);}
static void W_name_Help_Callback(Widget w, XtPointer context, XtPointer info)            {click_for_sound_help((snd_state *)context);}

static void W_expand_button_Help_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help((snd_state *)context, "Expand Button", "This button turns on expansion\n");
}

static void W_contrast_button_Help_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help((snd_state *)context, "Contrast Button", "This button turns on contrast enhancement\n");
}

static void W_reverb_button_Help_Callback(Widget w, XtPointer context, XtPointer info)
{
  snd_help((snd_state *)context, "Reverb Button", "This button turns on reverberation\n");
}

static void W_filter_button_Help_Callback(Widget w, XtPointer context, XtPointer info)
{
  snd_help((snd_state *)context, "Filter Button", "This button turns on the filter\n");
}

static void W_filter_dB_Help_Callback(Widget w, XtPointer context, XtPointer info)
{
  snd_help((snd_state *)context, "Filter dB",
"This button chooses between dB and linear y axis\n\
in the frequency response graph\n");
}

static void W_sync_Help_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help((snd_state *)context,
	   "Sync Button",
"This button causes edit operations on one\n\
channel to be applied to all channels at the\n\
same time.\n\
");
}

static void W_combine_Help_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help((snd_state *)context,
	   "Combine Button",
"This button causes all channels to be\n\
displayed in one window, sharing the various\n\
channel controls.  Two extra scrollbars on\n\
the right provide scroll and zoom for the\n\
overall set of channel graphs. The default\n\
multichannel display style can be set in\n\
the Snd initialization file by setting\n\
the variable combine-channels.\n\
");
}

static void W_apply_Help_Callback(Widget w, XtPointer context, XtPointer info)
{
  snd_help((snd_state *)context,
	   STR_Apply,
"The Apply button saves the last recorded\n\
run over the current file (see Record) as\n\
an edit of the current file.\n\
");
}

static void W_reset_Help_Callback(Widget w, XtPointer context, XtPointer info)
{
  snd_help((snd_state *)context,
	   STR_Reset,
"The 'Reset' button clears the control panel\n\
settings to the no-change state.\n\
");
}

static void W_remember_Help_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help((snd_state *)context,
	   STR_Remember,
"The 'Remember' button saves the current control\n\
panel state for a subsequent 'Restore'.\n\
");
}

static void W_restore_Help_Callback(Widget w, XtPointer context, XtPointer info)
{
  snd_help((snd_state *)context,
	   STR_Restore,
"The 'Restore' button returns the control\n\
panel to the state at the time of the\n\
last 'Remember', or the initial state if there\n\
has been no 'Remember'.\n\
");
}

void goto_minibuffer(snd_info *sp)
{
  if (sp) goto_window(w_snd_minibuffer(sp));
}

void set_minibuffer_string(snd_info *sp, char *str) 
{
  XmTextSetString(w_snd_minibuffer(sp), str);
  XmUpdateDisplay(w_snd_minibuffer(sp));
}

void set_minibuffer_cursor_position(snd_info *sp, int pos)
{
  XmTextSetCursorPosition(w_snd_minibuffer(sp), pos);
}

char *get_minibuffer_string(snd_info *sp) 
{
  return(XmTextGetString(w_snd_minibuffer(sp)));
}

void make_minibuffer_label(snd_info *sp , char *str)
{
  XmString s1;
  s1 = XmStringCreate(str, "button_font");
  XtVaSetValues(w_snd_minibuffer_label(sp), XmNlabelString, s1, NULL);
  XmStringFree(s1);
}


int sound_unlock_ctrls(snd_info *sp, void *ptr)
{
  XtManageChild(w_snd_ctrls(sp));
  XtVaSetValues(w_snd_ctrls(sp), XmNpaneMinimum, 1, NULL);
  return(0);
}

int sound_lock_ctrls(snd_info *sp, void *ptr)
{
  snd_state *ss;
  ss = (snd_state *)(sp->state);
  XtUnmanageChild(w_snd_ctrls(sp));
  XtVaSetValues(w_snd_ctrls(sp), XmNpaneMinimum, ss->ctrls_height, NULL);
  return(0);
}

static void W_name_Click_Callback(Widget w, XtPointer context, XtPointer info) 
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
      val = round(amp / (Float)(SCROLLBAR_LINEAR_MULT));
      if (val > SCROLLBAR_LINEAR_MAX)
	{
	  val = round((log(amp) * ((Float)SCROLLBAR_MAX * .2)) + SCROLLBAR_MID);
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
    sp->amp = 0.0;
  else 
    {
      if (val < SCROLLBAR_LINEAR_MAX)
	sp->amp = (Float)val * SCROLLBAR_LINEAR_MULT;
      else sp->amp = exp((Float)(val-SCROLLBAR_MID) / ((Float)SCROLLBAR_MAX * .2));
    }
  sfs = prettyf(sp->amp, 2);
  fill_number(sfs, amp_number_buffer);
  set_label(w_snd_amp_number(sp), amp_number_buffer);
  FREE(sfs);
  return(val);
}

void set_snd_amp(snd_info *sp, Float val)
{
  if (IS_PLAYER(sp))
    sp->amp = val;
  else XtVaSetValues(w_snd_amp(sp),
		     XmNvalue,
		     snd_amp_changed(sp, snd_amp_to_int(fclamp(0.0, val, 7.25))),
		     NULL);
}

static void W_amp_Click_Callback(Widget w, XtPointer context, XtPointer info) 
{
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  XButtonEvent *ev;
  int val;
  snd_context *sx;
  sx = sp->sgx;
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    val = snd_amp_to_int(sp->last_amp); 
  else val = 50;
  snd_amp_changed(sp, val);
  XtVaSetValues(sx->snd_widgets[W_amp], XmNvalue, val, NULL);
}

static void W_amp_Drag_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_amp_changed((snd_info *)context, ((XmScrollBarCallbackStruct *)info)->value);
}

static void W_amp_ValueChanged_Callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  snd_amp_changed(sp, cb->value);
  sp->last_amp = sp->saved_amp;
  sp->saved_amp = sp->amp;
}


#define SPEED_SCROLLBAR_MAX 1000

static char srate_number_buffer[5] ={'1', STR_decimal, '0', '0', '\0'};

XmString initial_speed_label(snd_state *ss)
{
  switch (speed_style(ss))
    {
    case SPEED_AS_RATIO:    return(XmStringCreate(ratio_one, XmFONTLIST_DEFAULT_TAG));    break;
    case SPEED_AS_SEMITONE: return(XmStringCreate(semitone_one, XmFONTLIST_DEFAULT_TAG)); break;
    default:                return(XmStringCreate(number_one, XmFONTLIST_DEFAULT_TAG));   break;
    }
}

static int snd_srate_to_int(Float val)
{
  int ival;
  if (val > 0.0)
    {
      ival = round(450.0 + 150.0 * log(val));
      if (ival < SPEED_SCROLLBAR_MAX)
	return(ival);
      else return(SPEED_SCROLLBAR_MAX);
    }
  else return(0);
}

static int snd_srate_changed(snd_info *sp, int ival)
{
  sp->srate = srate_changed(exp((Float)(ival - 450) / 150.0),
			    srate_number_buffer,
			    sp->speed_style,
			    sp->speed_tones);
  set_label(w_snd_srate_number(sp), srate_number_buffer);
  return(ival);
}

void set_snd_srate(snd_info *sp, Float val)
{
  if (IS_PLAYER(sp))
    sp->srate = val;
  else XtVaSetValues(w_snd_srate(sp),
		     XmNvalue,
		     snd_srate_changed(sp, snd_srate_to_int(fclamp(-20.0, val, 20.0))),
		     NULL);
}

static void W_srate_Click_Callback(Widget w, XtPointer context, XtPointer info) 
{
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  XButtonEvent *ev;
  int val;
  snd_context *sx;
  sx = sp->sgx;
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    val = snd_srate_to_int(sp->last_srate); 
  else val = 450;
  snd_srate_changed(sp, val);
  XtVaSetValues(sx->snd_widgets[W_srate], XmNvalue, val, NULL);
}

static void W_srate_Drag_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_srate_changed((snd_info *)context, ((XmScrollBarCallbackStruct *)info)->value);
}

static void W_srate_ValueChanged_Callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  snd_srate_changed(sp, cb->value);
  sp->last_srate = sp->saved_srate;
  sp->saved_srate = sp->srate;
}

void toggle_direction_arrow(snd_info *sp, int state)
{
  if (IS_PLAYER(sp))
    sp->play_direction = ((state) ? -1 : 1);
  else XmToggleButtonSetState(w_snd_srate_arrow(sp), state, TRUE);
}


#define EXPAND_SCROLLBAR_MAX 1000

static char expand_number_buffer[5] ={'1', STR_decimal, '0', '0', '\0'};

static int snd_expand_to_int(Float ep)
{
  int val;
  val = (int)(ep / .0009697);
  if (val > 100) val = (int)(round(450 + 150 * log(ep)));
  if (val < EXPAND_SCROLLBAR_MAX)
    return(val);
  return(EXPAND_SCROLLBAR_MAX);
}

static int snd_expand_changed(snd_info *sp, int val)
{
  char *sfs;
  if (val < 100)
    sp->expand = (Float)val * .0009697;
  else sp->expand = exp((Float)(val - 450) / 150.0);
  if (sp->playing) dac_set_expand(sp, sp->expand);
  sfs = prettyf(sp->expand, 2);
  fill_number(sfs, expand_number_buffer);
  set_label(w_snd_expand_number(sp), expand_number_buffer);
  FREE(sfs);
  return(val);
}

void set_snd_expand(snd_info *sp, Float val)
{
  if (IS_PLAYER(sp))
    sp->expand = val;
  else XtVaSetValues(w_snd_expand(sp),
		     XmNvalue,
		     snd_expand_changed(sp, snd_expand_to_int(fclamp(0.0, val, 20.0))),
		     NULL);
}

static void W_expand_Click_Callback(Widget w, XtPointer context, XtPointer info) 
{
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  XButtonEvent *ev;
  int val;
  snd_context *sx;
  sx = sp->sgx;
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    val = snd_expand_to_int(sp->last_expand); 
  else val = 450;
  snd_expand_changed(sp, val);
  XtVaSetValues(sx->snd_widgets[W_expand], XmNvalue, val, NULL);
}

static void W_expand_Drag_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_expand_changed((snd_info *)context, ((XmScrollBarCallbackStruct *)info)->value);
}

static void W_expand_ValueChanged_Callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  snd_expand_changed(sp, cb->value);
  sp->last_expand = sp->saved_expand;
  sp->saved_expand = sp->expand;
}

static void Expand_button_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_state *ss;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info; 
  snd_info *sp = (snd_info *)context;
  ss = sp->state;
  sp->expanding = cb->set;
  if (!(ss->using_schemes)) 
    XmChangeColor(w_snd_expand(sp), (Pixel)((sp->expanding) ? ((ss->sgx)->position_color) : ((ss->sgx)->basic_color)));
}

void toggle_expand_button(snd_info *sp, int state)
{
  if (IS_PLAYER(sp))
    sp->expanding = state;
  else XmToggleButtonSetState(w_snd_expand_button(sp), state, TRUE);
}



static char contrast_number_buffer[5] ={'0', STR_decimal, '0', '0', '\0'};

static int snd_contrast_to_int(Float val)
{
  if (val < 10.0)
    return(round(val * 10));
  else return(100);
}

static int snd_contrast_changed(snd_info *sp, int val)
{
  char *sfs;
  sp->contrast = (Float)val / 10.0;
  sfs = prettyf(sp->contrast, 2);
  fill_number(sfs, contrast_number_buffer);
  set_label(w_snd_contrast_number(sp), contrast_number_buffer);
  FREE(sfs);
  return(val);
}

void set_snd_contrast(snd_info *sp, Float val)
{
  if (IS_PLAYER(sp))
    sp->contrast = val;
  else XtVaSetValues(w_snd_contrast(sp),
		     XmNvalue,
		     snd_contrast_changed(sp, snd_contrast_to_int(fclamp(0.0, val, 9.0))),
		     NULL);
}

static void W_contrast_Click_Callback(Widget w, XtPointer context, XtPointer info) 
{
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  XButtonEvent *ev;
  int val;
  snd_context *sx;
  sx = sp->sgx;
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    val = snd_contrast_to_int(sp->last_contrast); 
  else val = 0;
  snd_contrast_changed(sp, val);
  XtVaSetValues(sx->snd_widgets[W_contrast], XmNvalue, val, NULL);
}

static void W_contrast_Drag_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_contrast_changed((snd_info *)context, ((XmScrollBarCallbackStruct *)info)->value);
}

static void W_contrast_ValueChanged_Callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  snd_contrast_changed(sp, cb->value);
  sp->last_contrast = sp->saved_contrast;
  sp->saved_contrast = sp->contrast;
}

static void Contrast_button_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_state *ss;
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ss = sp->state;
  sp->contrasting = cb->set;
  if (!(ss->using_schemes)) 
    XmChangeColor(w_snd_contrast(sp), (Pixel)((sp->contrasting) ? ((ss->sgx)->position_color) : ((ss->sgx)->basic_color)));
}

void toggle_contrast_button(snd_info *sp, int state)
{
  if (IS_PLAYER(sp))
    sp->contrasting = state;
  else XmToggleButtonSetState(w_snd_contrast_button(sp), state, TRUE);
}


void set_reverb_labels(const char *new_label)
{
  snd_state *ss;
  int i;
  snd_info *sp;
  Widget lab;
  ss = get_global_state();
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->sgx) && (sp->sgx->snd_widgets))
	{
	  lab = sp->sgx->snd_widgets[W_revscl_label];
	  if (lab) set_label(lab, new_label);
	}
    }
}

static char revscl_number_buffer[7] ={'0', STR_decimal, '0', '0', '0', '0', '\0'};
static char number_long_zero[7] ={'0', STR_decimal, '0', '0', '0', '0', '\0'};

static int snd_revscl_to_int(Float val)
{
  return(round(pow(val, 0.333) * 60.0));
}

static inline Float cube (Float a) {return(a*a*a);}

static int snd_revscl_changed(snd_info *sp, int val)
{
  char *fs, *ps, *sfs;
  int i, j;
  sp->revscl = cube((Float)val / 60.0);
  sfs = prettyf(sp->revscl, 3);
  fs = sfs;
  ps=(char *)(revscl_number_buffer);
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
  set_label(w_snd_revscl_number(sp), revscl_number_buffer);
  FREE(sfs);
  return(val);
}

void set_snd_revscl(snd_info *sp, Float val)
{
  if (IS_PLAYER(sp))
    sp->revscl = val;
  else XtVaSetValues(w_snd_revscl(sp),
		     XmNvalue,
		     snd_revscl_changed(sp, snd_revscl_to_int(fclamp(0.0, val, 3.25))),
		     NULL);
}

static void W_revscl_Click_Callback(Widget w, XtPointer context, XtPointer info) 
{
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  XButtonEvent *ev;
  int val;
  snd_context *sx;
  sx = sp->sgx;
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    val = snd_revscl_to_int(sp->last_revscl); 
  else val = 0;
  snd_revscl_changed(sp, val);
  XtVaSetValues(sx->snd_widgets[W_revscl], XmNvalue, val, NULL);
}


static void W_revscl_Drag_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_revscl_changed((snd_info *)context, ((XmScrollBarCallbackStruct *)info)->value);
}

static void W_revscl_ValueChanged_Callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  snd_revscl_changed(sp, cb->value);
  sp->last_revscl = sp->saved_revscl;
  sp->saved_revscl = sp->revscl;
}



static char revlen_number_buffer[5] ={'1', STR_decimal, '0', '0', '\0'};

static int snd_revlen_to_int(Float val)
{
  return(round(val * 20.0));
}

static int snd_revlen_changed(snd_info *sp, int val)
{
  char *sfs;
  sp->revlen = (Float)val / 20.0;
  sfs = prettyf(sp->revlen, 2);
  fill_number(sfs, revlen_number_buffer);
  set_label(w_snd_revlen_number(sp), revlen_number_buffer);
  FREE(sfs);
  return(val);
}

void set_snd_revlen(snd_info *sp, Float val)
{
  if (IS_PLAYER(sp))
    sp->revlen = val;
  else XtVaSetValues(w_snd_revlen(sp),
		     XmNvalue,
		     snd_revlen_changed(sp, snd_revlen_to_int(fclamp(0.0, val, 4.5))),
		     NULL);
}

static void W_revlen_Click_Callback(Widget w, XtPointer context, XtPointer info) 
{
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  XButtonEvent *ev;
  int val;
  snd_context *sx;
  sx = sp->sgx;
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    val = snd_revlen_to_int(sp->last_revlen); 
  else val = 20;
  snd_revlen_changed(sp, val);
  XtVaSetValues(sx->snd_widgets[W_revlen], XmNvalue, val, NULL);
}

static void W_revlen_Drag_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_revlen_changed((snd_info *)context, ((XmScrollBarCallbackStruct *)info)->value);
}

static void W_revlen_ValueChanged_Callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  snd_info *sp = (snd_info *)context;
  snd_revlen_changed(sp, cb->value);
  sp->last_revlen = sp->saved_revlen;
  sp->saved_revlen = sp->revlen;
}



static void Reverb_button_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_state *ss;
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ss = sp->state;
  sp->reverbing = cb->set;
  if (!(ss->using_schemes))
    {
      XmChangeColor(w_snd_revlen(sp), (Pixel)((sp->reverbing) ? ((ss->sgx)->position_color) : ((ss->sgx)->basic_color)));
      XmChangeColor(w_snd_revscl(sp), (Pixel)((sp->reverbing) ? ((ss->sgx)->position_color) : ((ss->sgx)->basic_color)));
    }
}

void toggle_reverb_button(snd_info *sp, int state)
{
  if (IS_PLAYER(sp))
    sp->reverbing = state;
  else XmToggleButtonSetState(w_snd_reverb_button(sp), state, TRUE);
}

static void Filter_button_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  sp->filtering = cb->set;
}

void toggle_filter_button(snd_info *sp, int state)
{
  if (IS_PLAYER(sp))
    sp->filtering = state;
  else XmToggleButtonSetState(w_snd_filter_button(sp), state, TRUE);
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

void sp_display_env(snd_info *sp)
{
  snd_state *ss;
  axis_context *ax;
  int height, width;
  Widget drawer;
  ss = sp->state;
  drawer = w_snd_filter_env(sp);
  height = widget_height(drawer);
  if (height < MIN_FILTER_GRAPH_HEIGHT) return;
  width = widget_width(drawer);
  ax = (axis_context *)CALLOC(1, sizeof(axis_context));
  ax->gc = (ss->sgx)->fltenv_basic_gc;
  ax->wn = XtWindow(drawer);
  ax->dp = XtDisplay(drawer);
  XClearWindow(ax->dp, ax->wn);
  display_filter_graph(ss, sp, ax, width, height);
  FREE(ax);
}

void set_filter_text(snd_info *sp, char *str)
{
  if (!(IS_PLAYER(sp)))
    XmTextSetString(w_snd_filter(sp), str);
}

static void filter_drawer_help_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help((snd_state *)context, "Filter Frequency Response",
"This graph shows the current filter frequency response envelope, \n\
and the actual response (dependent on the filter order).\n\
See the envelope editor documentation for editing directions.\n");
}

static void filter_drawer_button_motion(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  snd_info *sp = (snd_info *)context;
  XMotionEvent *ev = (XMotionEvent *)event;
  handle_filter_point(sp->state, sp, ev->x, ev->y, ev->time);
}

static void filter_drawer_button_press(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  snd_info *sp = (snd_info *)context;
  XButtonEvent *ev = (XButtonEvent *)event;
  handle_filter_press(sp, ev->x, ev->y, ev->time);
}

static void filter_drawer_button_release(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  handle_filter_release((snd_info *)context);
}

static void filter_drawer_resize(Widget w, XtPointer context, XtPointer info) 
{
  snd_info *sp = (snd_info *)context;
  sp_display_env(sp);
}

static void filter_dB_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  sp->filter_dBing = (cb->set);
  sp_display_env(sp);
}

void set_filter_dBing(snd_info *sp, int val)
{
  sp->filter_dBing = val;
  if (!(IS_PLAYER(sp)))
    {
      XmToggleButtonSetState(w_snd_filter_dB(sp), val, FALSE);
      sp_display_env(sp);
    }
}

void set_snd_filter_order(snd_info *sp, int order)
{
  char *fltorder;
  if (order & 1) order++;
  if (order <= 0) order = 2;
  sp->filter_order = order;
  if (!(IS_PLAYER(sp)))
    {
      fltorder = (char *)CALLOC(8, sizeof(char));
      sprintf(fltorder, "%d", order);
      XmTextSetString(w_snd_filter_order(sp), fltorder);
      FREE(fltorder);
      sp_display_env(sp);
    }
  sp->filter_changed = 1;
}

static void filter_order_up_Callback(Widget w, XtPointer context, XtPointer info)
{
  snd_info *sp = (snd_info *)context;
  set_snd_filter_order(sp, sp->filter_order + 2);
}

static void filter_order_down_Callback(Widget w, XtPointer context, XtPointer info)
{
  snd_info *sp = (snd_info *)context;
  if (sp->filter_order > 2)
    set_snd_filter_order(sp, sp->filter_order - 2);
}

static void W_filter_order_up_Help_Callback(Widget w, XtPointer context, XtPointer info)
{
  snd_help((snd_state *)context,
	   "Filter Order Increment Button",
"This button causes the filter order to be incremented\n\
in case your keyboard is not working, or arabic numbers\n\
present an insuperable challenge.\n\
");
}

static void W_filter_order_down_Help_Callback(Widget w, XtPointer context, XtPointer info)
{
  snd_help((snd_state *)context,
	   "Filter Order Decrement Button",
"This button causes the filter order to be decremented\n\
in case your keyboard is not working, or arabic numbers\n\
present an insuperable challenge.\n\
");
}

static void Filter_activate_Callback(Widget w, XtPointer context, XtPointer info)
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

  if (sp->filter_env) free_env(sp->filter_env);
  sp->filter_env = string2env(str);
  if (str) XtFree(str);
  if (!(sp->filter_env)) /* maybe user cleared text field? */
    sp->filter_env = default_env(sp->filter_env_xmax, 1.0);
  str = XmTextGetString(w_snd_filter_order(sp));
  if ((str) && (*str))
    {
      order = string2int(str);
      if (order & 1) order++;
      if (order <= 0) order = 2;
      sp->filter_order = order;
      XtFree(str);
    }
  report_filter_edit(sp);
  sp_display_env(sp);
  filter_textfield_deactivate(sp);
  sp->filter_changed = 1;
}

static void Filter_Order_activate_Callback(Widget w, XtPointer context, XtPointer info)
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
      sp->filter_order = order;
      sp->filter_changed = 1;
      sp_display_env(sp);
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
      XmTextSetString(w_snd_filter(sp), tmpstr = env_to_string(e));
      if (tmpstr) FREE(tmpstr);
      report_filter_edit(sp);
      sp_display_env(sp);
      /* this is called also from snd-scm.c */
    }
  sp->filter_changed = 1;
}

void set_play_button(snd_info *sp, int val)
{
  if (!(IS_PLAYER(sp)))
    {
      XmToggleButtonSetState(w_snd_play(sp), val, FALSE);
      set_file_browser_play_button(sp->shortname, val);
    }
}

static void Play_button_Callback(Widget w, XtPointer context, XtPointer info)
{
  snd_info *sp = (snd_info *)context;
  chan_info *cp;
  snd_state *ss;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  int i;
  XButtonEvent *ev;
  ev = (XButtonEvent *)(cb->event);
  if (sp->playing) 
    {
      if (sp->cursor_follows_play != DONT_FOLLOW)
	{
	  for (i = 0; i < sp->nchans; i++)
	    {
	      cp = sp->chans[i];
	      cp->original_cursor = cp->cursor;
	    }
	}
      stop_playing_sound(sp);
    }
  if (sp->cursor_follows_play != FOLLOW_ALWAYS)         /* can be set in init file */
    {
      if ((cb->set) && (ev->state & (snd_ControlMask | snd_MetaMask)))
	sp->cursor_follows_play = FOLLOW_ONCE;
      else sp->cursor_follows_play = DONT_FOLLOW;
    }
  set_file_browser_play_button(sp->shortname, cb->set);
  cp = any_selected_channel(sp);
  goto_graph(cp);
  if ((!(cp->cursor_on)) && (sp->cursor_follows_play != DONT_FOLLOW))
    for (i = 0; i < sp->nchans; i++)
      {
	cp = sp->chans[i];
	cp->cursor_on = 1;
      }
  if (cb->set) 
    {
      ss = sp->state;
      XtVaSetValues(w,XmNselectColor, ((sp->cursor_follows_play != DONT_FOLLOW) ? ((ss->sgx)->green) : ((ss->sgx)->pushed_button_color)),NULL);
      play_sound(sp, 0, NO_END_SPECIFIED, IN_BACKGROUND);
    }
}

typedef struct {int pausing; snd_state *ss;} pause_data;

static int set_play_button_pause(snd_info *sp, void *ptr)
{
  pause_data *pd = (pause_data *)ptr;
  snd_state *ss;
  Widget w;
  if ((sp->playing) && (!(IS_PLAYER(sp))))
    {
      ss = pd->ss;
      w = w_snd_play(sp);
      if (pd->pausing)
	XtVaSetValues(w, XmNselectColor, (ss->sgx)->red, NULL);
      else XtVaSetValues(w, XmNselectColor, ((sp->cursor_follows_play != DONT_FOLLOW) ? ((ss->sgx)->green) : ((ss->sgx)->pushed_button_color)), NULL);
    }
  return(0);
}

void play_button_pause(snd_state *ss, int pausing)
{
  pause_data *pd;
  pd = (pause_data *)CALLOC(1, sizeof(pause_data));
  pd->pausing = pausing;
  pd->ss = ss;
  map_over_sounds(ss, set_play_button_pause, (void *)pd);
  FREE(pd);
}


static void Play_arrow_Callback(Widget w, XtPointer context, XtPointer info)
{
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  int dir;
  dir = cb->set;
  if (dir) sp->play_direction = -1; else sp->play_direction = 1;
}

static void set_sync_color(snd_info *sp)
{
  snd_state *ss;
  Widget syb;
  syb = w_snd_sync(sp);
  ss = sp->state;
  switch (sp->syncing)
    {
    case 1: case 0: XtVaSetValues(syb, XmNselectColor, (ss->sgx)->pushed_button_color, NULL); break;
    case 2: XtVaSetValues(syb, XmNselectColor, (ss->sgx)->green, NULL); break;
    case 3: XtVaSetValues(syb, XmNselectColor, (ss->sgx)->yellow, NULL); break;
    case 4: XtVaSetValues(syb, XmNselectColor, (ss->sgx)->red, NULL); break;
    default: XtVaSetValues(syb, XmNselectColor, (ss->sgx)->black, NULL); break;
    }
}

void syncb(snd_info *sp, int on)
{
  sp->syncing = on;
  if (!(IS_PLAYER(sp)))
    {
      set_sync_color(sp);
      XmToggleButtonSetState(w_snd_sync(sp), (on == 0) ? FALSE : TRUE, FALSE);
    }
}

static void Sync_button_Callback(Widget w, XtPointer context, XtPointer info)
{
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  chan_info *cp;
  XButtonEvent *ev;
  ev = (XButtonEvent *)(cb->event);
  if (cb->set)
    if (ev->state & snd_ControlMask) 
      if (ev->state & snd_MetaMask)
	if (ev->state & snd_ShiftMask)
	  sp->syncing = 4;
	else sp->syncing = 3;
      else sp->syncing = 2;
    else sp->syncing = 1;
  else sp->syncing = 0;
  if (sp->syncing != 0) 
    {
      set_sync_color(sp);
      cp = sp->lacp;
      if (cp == NULL) cp = any_selected_channel(sp);
      goto_graph(cp);
      if (cp->cursor_on) cursor_moveto(cp, cp->cursor);
      apply_x_axis_change(cp->axis, cp, sp);
    }
}

static void Combine_button_Callback(Widget w, XtPointer context, XtPointer info)
{
  /* click if set unsets, click if unset->combine, ctrl-click->superimpose */
  snd_info *sp = (snd_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  XButtonEvent *ev;
  int val;
  ev = (XButtonEvent *)(cb->event);
  if (cb->set)
    {
      if (ev->state & (snd_ControlMask | snd_MetaMask)) 
	val = CHANNELS_SUPERIMPOSED;
      else val = CHANNELS_COMBINED;
    }
  else val = CHANNELS_SEPARATE;
  combineb(sp, val);
}


static void minibuffer_click_Callback(Widget w, XtPointer context, XtPointer info)
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

static void Apply_Callback(Widget w, XtPointer context, XtPointer info) 
{
  /* create temp file of run over current file using the current (saved) ctrls state */
  snd_info *sp = (snd_info *)context;
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  XButtonEvent *ev;
  snd_state *ss;
  snd_context *sgx;
  sgx = sp->sgx;
  ss = sp->state;
  if (sp->applying) 
    {
      stop_applying(sp);
      if (!(ss->using_schemes)) 
	XmChangeColor(w_snd_apply(sp), (Pixel)((ss->sgx)->basic_color));
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
	XmChangeColor(w_snd_apply(sp), (Pixel)((ss->sgx)->pushed_button_color));
      sgx->apply_in_progress = BACKGROUND_ADD(ss, apply_controls, (GUI_POINTER)(make_apply_state(sp)));
    }
}

/* apply is only safe if the DAC is currently inactive and remains safe only
 * if all other apply buttons are locked out (and play).
 */

static int lockapply(snd_info *sp, void *up) 
{
  if (sp != up) set_sensitive(w_snd_apply(sp), FALSE);
  return(0);
}

void lock_apply(snd_state *ss, snd_info *sp)
{
  /* if playing or applying, set other applys to insensitive */
  map_over_sounds(ss, lockapply, (void *)sp);
}

static int unlockapply(snd_info *sp, void *up) 
{
  if (sp != up) set_sensitive(w_snd_apply(sp), TRUE);
  return(0);
}

void unlock_apply(snd_state *ss, snd_info *sp)
{
  map_over_sounds(ss, unlockapply, (void *)sp);
  if ((sp) && (!(ss->using_schemes))) 
    XmChangeColor(w_snd_apply(sp), (Pixel)((ss->sgx)->basic_color));
}

static int cant_write(char *name)
{
#if HAVE_ACCESS
  return((access(name, W_OK)) != 0);
#else
  return(0);
#endif
}

static void save_control_panel_Callback(Widget w, XtPointer context, XtPointer info) {save_control_panel((snd_info *)context);}
static void restore_control_panel_Callback(Widget w, XtPointer context, XtPointer info) {restore_control_panel((snd_info *)context);}
static void reset_control_panel_Callback(Widget w, XtPointer context, XtPointer info) {reset_control_panel((snd_info *)context);}

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
static int mini_lock_allocated = 0;
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
      XtVaSetValues(w_snd_name_icon(sp), XmNlabelPixmap, sx->file_pix, NULL);
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
      if (on)
	sx->file_pix = mini_bombs[sp->bomb_ctr];
      else sx->file_pix = blank_pixmap;
      XtVaSetValues(w_snd_name_icon(sp), XmNlabelPixmap, sx->file_pix, NULL);
    }
}

void x_bomb(snd_info *sp, int on)
{
  show_bomb_icon(sp, on);
  if (on) 
    sp->bomb_ctr++; 
  else sp->bomb_ctr = 0;
}

static int inc_bomb(snd_info *sp, void *ptr)
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
  return(0);
}

static int bomb_in_progress = 0;

static void bomb_check(XtPointer context, XtIntervalId *id)
{
  snd_info *sp = (snd_info *)context;
  snd_state *ss;
  int incs[1];
  ss = sp->state;
  incs[0] = 0;
  map_over_sounds(ss, inc_bomb, (void *)incs);
  if (incs[0] > 0)
    XtAppAddTimeOut(MAIN_APP(ss),
		    (unsigned long)BOMB_TIME,
		    (XtTimerCallbackProc)bomb_check,
		    context);
  else bomb_in_progress = 0;
}

void snd_file_bomb_icon(snd_info *sp, int on)
{
  snd_state *ss;
  if ((on) && (bomb_in_progress == 0))
    {
      ss = sp->state;
      bomb_in_progress = 1;
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
  w = w_snd_name_icon(sp);
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
    report_in_minibuffer(sp, "%s has changed since we last read it!", sp->shortname);
}
static void snd_file_glasses_icon(snd_info *sp, int on, int glass) {}
void x_bomb(snd_info *sp, int on) {}
#endif

static void Close_Sound_Dialog(Widget w, XtPointer context, XtPointer info) 
{
  snd_info *sp = (snd_info *)context;
  if (sp) snd_close_file(sp, sp->state);
} 

snd_info *add_sound_window (char *filename, snd_state *ss)
{  
  snd_info *sp = NULL, *osp;
  file_info *hdr = NULL;
  Widget *sw;
  XmString s1;
  int snd_slot, nchans, make_widgets, i, k, need_colors, n, old_chans;
  Arg args[32];
  char *old_name = NULL, *title;
  Dimension app_y, app_dy, screen_y, chan_min_y;
  /* these dimensions are used to try to get a reasonable channel graph size without falling off the screen bottom */
  Pixmap rb, lb;
  int depth;
  Widget form;
  XtCallbackList n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12;
  snd_context *sx;
  Atom sound_delete;
  static int first_window = 1;
  errno = 0;
  hdr = make_file_info(filename, ss);
  if (!hdr) return(NULL);
  if (ss->pending_change) 
    {
      old_name = filename;
      filename = ss->pending_change;
      ss->pending_change = NULL;
    }
  nchans = hdr->chans;
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
  ss->sounds[snd_slot] = make_snd_info(ss->sounds[snd_slot], ss, filename, hdr, snd_slot);
  sp = ss->sounds[snd_slot];
  sp->inuse = 1;
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

      if (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS)
	{
	  title = (char *)CALLOC(128, sizeof(char));
	  sprintf(title, "%d: %s", snd_slot, sp->shortname);
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
	       * -- as a popup, it's not considered a child
	       */
	      add_dialog(ss, sx->dialog);
	      sound_delete = XmInternAtom(XtDisplay(sx->dialog), "WM_DELETE_WINDOW", FALSE);
	      XmAddWMProtocolCallback(sx->dialog, sound_delete, Close_Sound_Dialog, (XtPointer)sp);
	      add_popup_handler(sx->dialog);
	    }
	  else XtVaSetValues(sx->dialog, XmNtitle, title, NULL);
	  FREE(title);
	  if (!XtIsManaged(sx->dialog)) XtManageChild(sx->dialog);
	}

      n = 0;      
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
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
	if (ss->listening != LISTENER_CLOSED) 
	  {
	    XtSetArg(args[n], XmNpositionIndex, snd_slot); n++;
	  }

      if (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS)
	sw[W_pane] = sndCreatePanedWindowWidget("snd-pane", sx->dialog, args, n);
      else sw[W_pane] = sndCreatePanedWindowWidget("snd-pane", SOUND_PANE(ss), args, n);
      /* it would be better if we could set a paned window to keep its children relative
       *   amounts the same upon outside resize, but the Paned Window widget doesn't
       *   have a resize callback, and no obvious way to advise the resize mechanism.
       *   An attempt to get the same effect by wrapping w_pane in a drawingarea widget
       *   ran into other troubles.
       */

      XtAddCallback(sw[W_pane], XmNhelpCallback, W_name_Help_Callback, ss);
      XtAddEventHandler(sw[W_pane], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      /* if user clicks in controls, then starts typing, try to send key events to current active channel */
      /* all widgets in the control-pane that would otherwise intercept the key events get this event handler */

      for (i = 0; i < nchans; i++)
	add_channel_window(sp, i, ss, chan_min_y, 0, NULL, WITH_FW_BUTTONS);
      
      n = 0;      
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNpaneMinimum, ss->ctrls_height); n++;
      XtSetArg(args[n], XmNpaneMaximum, ss->ctrls_height); n++;
      sw[W_ctrls] = sndCreateFormWidget ("snd-ctrls", sw[W_pane], args, n);
      XtAddEventHandler(sw[W_ctrls], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      sw[W_name_form] = sndCreateFormWidget("snd-name-form", sw[W_ctrls], args, n);
      XtAddCallback(sw[W_name_form], XmNhelpCallback, W_name_Help_Callback, ss);
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
      XtAddCallback(sw[W_name], XmNhelpCallback, W_name_Help_Callback, ss);
      XtAddEventHandler(sw[W_name], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_name], XmNactivateCallback, W_name_Click_Callback, (XtPointer)sp);
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
	  mini_lock_allocated = 1;
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
      XtAddCallback(sw[W_info_sep], XmNhelpCallback, W_info_sep_Help_Callback, ss);

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
      XtAddCallback(sw[W_info_label], XmNhelpCallback, W_info_Help_Callback, ss);
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
      sw[W_info] = sndCreateTextFieldWidget(ss, "snd-info", sw[W_name_form], args, n, ACTIVATABLE, add_completer_func(info_completer));
      XtAddCallback(sw[W_info], XmNhelpCallback, W_info_Help_Callback, ss);
      XtAddCallback(sw[W_info], XmNactivateCallback, minibuffer_click_Callback, (XtPointer)sp);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
#if NEED_TOGGLE_MARGIN
      XtSetArg(args[n], XmNmarginHeight, TOGGLE_MARGIN); n++;
      XtSetArg(args[n], XmNmarginTop, TOGGLE_MARGIN); n++;
#endif
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XM_FONT_RESOURCE, BUTTON_FONT(ss)); n++;
      XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNselectColor, (ss->sgx)->pushed_button_color); n++;}
      sw[W_play] = sndCreateToggleButtonWidget(STR_play, sw[W_name_form], args, n);
      XtAddCallback(sw[W_play], XmNhelpCallback, W_play_Help_Callback, ss);
      XtAddCallback(sw[W_play], XmNvalueChangedCallback, Play_button_Callback, (XtPointer)sp);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
#if NEED_TOGGLE_MARGIN
      XtSetArg(args[n], XmNmarginHeight, TOGGLE_MARGIN); n++;
      XtSetArg(args[n], XmNmarginTop, TOGGLE_MARGIN); n++;
#endif
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, sw[W_play]); n++;
      XtSetArg(args[n], XM_FONT_RESOURCE, BUTTON_FONT(ss)); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNselectColor, (ss->sgx)->pushed_button_color); n++;}
      sw[W_sync] = sndCreateToggleButtonWidget(STR_sync, sw[W_name_form], args, n);
      XtAddCallback(sw[W_sync], XmNhelpCallback, W_sync_Help_Callback, ss);
      XtAddEventHandler(sw[W_sync], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_sync], XmNvalueChangedCallback, Sync_button_Callback, (XtPointer)sp);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, sw[W_sync]); n++;
#if NEED_TOGGLE_MARGIN
      XtSetArg(args[n], XmNmarginHeight, TOGGLE_MARGIN); n++;
      XtSetArg(args[n], XmNmarginTop, TOGGLE_MARGIN); n++;
#endif
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, sw[W_sync]); n++;
      XtSetArg(args[n], XM_FONT_RESOURCE, BUTTON_FONT(ss)); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNselectColor, (ss->sgx)->pushed_button_color); n++;}
      sw[W_combine] = sndCreateToggleButtonWidget(STR_unite, sw[W_name_form], args, n);
      XtAddCallback(sw[W_combine], XmNhelpCallback, W_combine_Help_Callback, ss);
      XtAddEventHandler(sw[W_combine], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_combine], XmNvalueChangedCallback, Combine_button_Callback, (XtPointer)sp);

      n = 0;
      XtVaSetValues(sw[W_ctrls], XmNskipAdjust, TRUE, NULL);

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
      sw[W_amp_separator] = XtCreateManagedWidget ("snd-amp-sep", xmSeparatorWidgetClass, sw[W_ctrls], args, n);
      XtAddCallback(sw[W_amp_separator], XmNhelpCallback, W_amp_Help_Callback, ss);
      
      /* if control-panel */
      n = 0;      
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_amp_separator]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      sw[W_amp_form] = sndCreateFormWidget ("snd-amp", sw[W_ctrls], args, n);
      XtAddEventHandler(sw[W_amp_form], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);

      n = 0;      
      /* AMP */
      s1 = XmStringCreate(STR_amp_p, XmFONTLIST_DEFAULT_TAG);
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
      sw[W_amp_label] = sndCreatePushButtonWidget ("amp-label", sw[W_amp_form], args, n);
      XtAddCallback(sw[W_amp_label], XmNhelpCallback, W_amp_Help_Callback, ss);
      XtAddEventHandler(sw[W_amp_label], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_amp_label], XmNactivateCallback, W_amp_Click_Callback, (XtPointer)sp);
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
      XtAddCallback(sw[W_amp_number], XmNhelpCallback, W_amp_Help_Callback, ss);
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
      XtSetArg(args[n], XmNvalue, SCROLLBAR_MID); n++;
      XtSetArg(args[n], XmNdragCallback, n1 = make_callback_list(W_amp_Drag_Callback, (XtPointer)sp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n2 = make_callback_list(W_amp_ValueChanged_Callback, (XtPointer)sp)); n++;
      sw[W_amp] = XtCreateManagedWidget("amp", xmScrollBarWidgetClass, sw[W_amp_form], args, n);
      XtAddCallback(sw[W_amp], XmNhelpCallback, W_amp_Help_Callback, ss);
      XtAddEventHandler(sw[W_amp], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);

      n = 0;
      /* SRATE */
      s1 = XmStringCreate(STR_speed, XmFONTLIST_DEFAULT_TAG);
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
      sw[W_srate_label] = sndCreatePushButtonWidget ("srate-label", sw[W_amp_form], args, n);
      XtAddCallback(sw[W_srate_label], XmNhelpCallback, W_srate_Help_Callback, ss);
      XtAddEventHandler(sw[W_srate_label], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_srate_label], XmNactivateCallback, W_srate_Click_Callback, (XtPointer)sp);
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
      XtAddCallback(sw[W_srate_number], XmNhelpCallback, W_srate_Help_Callback, ss);
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
      sw[W_srate_arrow] = sndCreateToggleButtonWidget("dir", sw[W_amp_form], args, n);
      form = sw[W_srate_arrow];
      rb = XCreateBitmapFromData(XtDisplay(form), RootWindowOfScreen(XtScreen(form)), (const char *)speed_r_bits1, 16, 12);
      lb = XCreateBitmapFromData(XtDisplay(form), RootWindowOfScreen(XtScreen(form)), (const char *)speed_l_bits1, 16, 12);
      XtVaGetValues(form, XmNdepth, &depth, NULL);
      sx->speed_r = XCreatePixmap(XtDisplay(form), RootWindowOfScreen(XtScreen(form)), 16, 12, depth);
      sx->speed_l = XCreatePixmap(XtDisplay(form), RootWindowOfScreen(XtScreen(form)), 16, 12, depth);
      XCopyPlane(XtDisplay(form), rb, sx->speed_r, (ss->sgx)->speed_gc, 0, 0, 16, 12, 0, 0, 1);
      XCopyPlane(XtDisplay(form), lb, sx->speed_l, (ss->sgx)->speed_gc, 0, 0, 16, 12, 0, 0, 1);
      XFreePixmap(XtDisplay(form), rb);
      XFreePixmap(XtDisplay(form), lb);
      XtVaSetValues(form, XmNselectPixmap, sx->speed_l, XmNlabelPixmap, sx->speed_r, NULL);
      /* pretty damn tedious -- we can't use the bare pixmap because X dies sputtering incomprehensible jargon */
      XtAddCallback(sw[W_srate_arrow], XmNhelpCallback, W_srate_arrow_Help_Callback, ss);
      XtAddEventHandler(sw[W_srate_arrow], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_srate_arrow], XmNvalueChangedCallback, Play_arrow_Callback, (XtPointer)sp);

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
      XtSetArg(args[n], XmNmaximum, SPEED_SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNvalue, 450); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNdragCallback, n3 = make_callback_list(W_srate_Drag_Callback, (XtPointer)sp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n4 = make_callback_list(W_srate_ValueChanged_Callback, (XtPointer)sp)); n++;
      sw[W_srate] = XtCreateManagedWidget("srate", xmScrollBarWidgetClass, sw[W_amp_form], args, n);
      XtAddCallback(sw[W_srate], XmNhelpCallback, W_srate_Help_Callback, ss);
      XtAddEventHandler(sw[W_srate], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);

      n = 0;
      /* EXPAND */
      s1 = XmStringCreate(STR_expand, XmFONTLIST_DEFAULT_TAG);
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
      sw[W_expand_label] = sndCreatePushButtonWidget ("expand-label", sw[W_amp_form], args, n);
      XtAddCallback(sw[W_expand_label], XmNhelpCallback, W_expand_Help_Callback, ss);
      XtAddEventHandler(sw[W_expand_label], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_expand_label], XmNactivateCallback, W_expand_Click_Callback, (XtPointer)sp);
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
      XtAddCallback(sw[W_expand_number], XmNhelpCallback, W_expand_Help_Callback, ss);
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
      sw[W_expand_button] = sndCreateToggleButtonWidget("expoff", sw[W_amp_form], args, n);
      XtAddCallback(sw[W_expand_button], XmNhelpCallback, W_expand_button_Help_Callback, ss);
      XtAddEventHandler(sw[W_expand_button], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_expand_button], XmNvalueChangedCallback, Expand_button_Callback, (XtPointer)sp);
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
      XtSetArg(args[n], XmNmaximum, EXPAND_SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNvalue, 450); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNdragCallback, n5 = make_callback_list(W_expand_Drag_Callback, (XtPointer)sp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n6 = make_callback_list(W_expand_ValueChanged_Callback, (XtPointer)sp)); n++;
      sw[W_expand] = XtCreateManagedWidget("", xmScrollBarWidgetClass, sw[W_amp_form], args, n);
      XtAddCallback(sw[W_expand], XmNhelpCallback, W_expand_Help_Callback, ss);
      XtAddEventHandler(sw[W_expand], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);


      /* CONTRAST */
      n = 0;
      s1 = XmStringCreate(STR_contrast, XmFONTLIST_DEFAULT_TAG);
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
      sw[W_contrast_label] = sndCreatePushButtonWidget ("contrast-label", sw[W_amp_form], args, n);
      XtAddCallback(sw[W_contrast_label], XmNhelpCallback, W_contrast_Help_Callback, ss);
      XtAddEventHandler(sw[W_contrast_label], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_contrast_label], XmNactivateCallback, W_contrast_Click_Callback, (XtPointer)sp);
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
      XtAddCallback(sw[W_contrast_number], XmNhelpCallback, W_contrast_Help_Callback, ss);
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
      sw[W_contrast_button] = sndCreateToggleButtonWidget("conoff", sw[W_amp_form], args, n);
      XtAddCallback(sw[W_contrast_button], XmNhelpCallback, W_contrast_button_Help_Callback, ss);
      XtAddEventHandler(sw[W_contrast_button], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_contrast_button], XmNvalueChangedCallback, Contrast_button_Callback, (XtPointer)sp);
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
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNvalue, 0); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNdragCallback, n7 = make_callback_list(W_contrast_Drag_Callback, (XtPointer)sp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n8 = make_callback_list(W_contrast_ValueChanged_Callback, (XtPointer)sp)); n++;
      sw[W_contrast] = XtCreateManagedWidget("", xmScrollBarWidgetClass, sw[W_amp_form], args, n);
      XtAddCallback(sw[W_contrast], XmNhelpCallback, W_contrast_Help_Callback, ss);
      XtAddEventHandler(sw[W_contrast], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);

      /* REVERB */
      /* REVSCL */
      n = 0;
      s1 = XmStringCreate(reverb_name(), XmFONTLIST_DEFAULT_TAG);
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
      sw[W_revscl_label] = sndCreatePushButtonWidget ("revscl-label", sw[W_amp_form], args, n);
      XtAddCallback(sw[W_revscl_label], XmNhelpCallback, W_revscl_Help_Callback, ss);
      XtAddEventHandler(sw[W_revscl_label], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_revscl_label], XmNactivateCallback, W_revscl_Click_Callback, (XtPointer)sp);
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
      XtAddCallback(sw[W_revscl_number], XmNhelpCallback, W_revscl_Help_Callback, ss);
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
      XtSetArg(args[n], XmNvalue, 0); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNdragCallback, n9 = make_callback_list(W_revscl_Drag_Callback, (XtPointer)sp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n10 = make_callback_list(W_revscl_ValueChanged_Callback, (XtPointer)sp)); n++;
      sw[W_revscl] = XtCreateManagedWidget("", xmScrollBarWidgetClass, sw[W_amp_form], args, n);
      XtAddCallback(sw[W_revscl], XmNhelpCallback, W_revscl_Help_Callback, ss);
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
      sw[W_reverb_button] = sndCreateToggleButtonWidget("revoff", sw[W_amp_form], args, n);
      XtAddCallback(sw[W_reverb_button], XmNhelpCallback, W_reverb_button_Help_Callback, ss);
      XtAddEventHandler(sw[W_reverb_button], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_reverb_button], XmNvalueChangedCallback, Reverb_button_Callback, (XtPointer)sp);
      XmStringFree(s1);


      /* REVLEN */
      n = 0;
      s1 = XmStringCreate(STR_len, XmFONTLIST_DEFAULT_TAG);
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
      sw[W_revlen_label] = sndCreatePushButtonWidget("revlen-label", sw[W_amp_form], args, n);
      XtAddCallback(sw[W_revlen_label], XmNhelpCallback, W_revlen_Help_Callback, ss);
      XtAddEventHandler(sw[W_revlen_label], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_revlen_label], XmNactivateCallback, W_revlen_Click_Callback, (XtPointer)sp);
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
      XtAddCallback(sw[W_revlen_number], XmNhelpCallback, W_revlen_Help_Callback, ss);
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
      XtSetArg(args[n], XmNvalue, 20); n++;
      XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;
      XtSetArg(args[n], XmNdragCallback, n11 = make_callback_list(W_revlen_Drag_Callback, (XtPointer)sp)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n12 = make_callback_list(W_revlen_ValueChanged_Callback, (XtPointer)sp)); n++;
      sw[W_revlen] = XtCreateManagedWidget("", xmScrollBarWidgetClass, sw[W_amp_form], args, n);
      XtAddCallback(sw[W_revlen], XmNhelpCallback, W_revlen_Help_Callback, ss);
      XtAddEventHandler(sw[W_revlen], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);


      /* FILTER */
      n = 0;
      s1 = XmStringCreate(STR_filter, XmFONTLIST_DEFAULT_TAG);
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
      XtAddCallback(sw[W_filter_label], XmNhelpCallback, W_filter_Help_Callback, ss);
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
      sw[W_filter_order] = sndCreateTextFieldWidget(ss, "filter-order", sw[W_amp_form], args, n, ACTIVATABLE, NO_COMPLETER);
      XmTextSetString(sw[W_filter_order], " 20");
      XtAddCallback(sw[W_filter_order], XmNhelpCallback, W_filter_order_Help_Callback, ss);
      XtAddCallback(sw[W_filter_order], XmNactivateCallback, Filter_Order_activate_Callback, (XtPointer)sp);

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
      sw[W_filter_order_down] = sndCreatePushButtonWidget("", sw[W_amp_form], args, n);
      XtAddEventHandler(sw[W_filter_order_down], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_filter_order_down], XmNhelpCallback, W_filter_order_down_Help_Callback, ss);
      XtAddCallback(sw[W_filter_order_down], XmNactivateCallback, filter_order_down_Callback, (XtPointer)sp);

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
      sw[W_filter_order_up] = sndCreatePushButtonWidget("", sw[W_amp_form], args, n);
      XtAddEventHandler(sw[W_filter_order_up], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_filter_order_up], XmNhelpCallback, W_filter_order_up_Help_Callback, ss);
      XtAddCallback(sw[W_filter_order_up], XmNactivateCallback, filter_order_up_Callback, (XtPointer)sp);

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
      sw[W_filter_button] = sndCreateToggleButtonWidget("fltoff", sw[W_amp_form], args, n);
      XtAddCallback(sw[W_filter_button], XmNhelpCallback, W_filter_button_Help_Callback, ss);
      XtAddEventHandler(sw[W_filter_button], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_filter_button], XmNvalueChangedCallback, Filter_button_Callback, (XtPointer)sp);
      XmStringFree(s1);

      n = 0;
      s1 = XmStringCreate(STR_dB, XmFONTLIST_DEFAULT_TAG);
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sw[W_filter_button]); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, sw[W_filter_button]); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++; 
      XtSetArg(args[n], XmNvalue, sp->filter_dBing); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNselectColor, (ss->sgx)->pushed_button_color); n++;}
      sw[W_filter_dB] = sndCreateToggleButtonWidget("fltdB", sw[W_amp_form], args, n);
      XtAddCallback(sw[W_filter_dB], XmNhelpCallback, W_filter_dB_Help_Callback, ss);
      XtAddEventHandler(sw[W_filter_dB], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_filter_dB], XmNvalueChangedCallback, filter_dB_Callback, (XtPointer)sp);
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
      sw[W_filter] = sndCreateTextFieldWidget(ss, "filter-window", sw[W_amp_form], args, n, ACTIVATABLE, add_completer_func(filename_completer));
      XtAddCallback(sw[W_filter], XmNhelpCallback, W_filter_envelope_Help_Callback, ss);
      XtAddCallback(sw[W_filter], XmNactivateCallback, Filter_activate_Callback, (XtPointer)sp);

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
      sw[W_apply] = sndCreatePushButtonWidget(STR_Apply, sw[W_amp_form], args, n);
      XtAddCallback(sw[W_apply], XmNhelpCallback, W_apply_Help_Callback, ss);
      XtAddEventHandler(sw[W_apply], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_apply], XmNactivateCallback, Apply_Callback, (XtPointer)sp);

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
      sw[W_remember] = sndCreatePushButtonWidget(STR_Remember, sw[W_amp_form], args, n);
      XtAddCallback(sw[W_remember], XmNhelpCallback, W_remember_Help_Callback, ss);
      XtAddEventHandler(sw[W_remember], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_remember], XmNactivateCallback, save_control_panel_Callback, (XtPointer)sp);

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
      sw[W_restore] = sndCreatePushButtonWidget(STR_Restore, sw[W_amp_form], args, n);
      XtAddCallback(sw[W_restore], XmNhelpCallback, W_restore_Help_Callback, ss);
      XtAddEventHandler(sw[W_restore], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_restore], XmNactivateCallback, restore_control_panel_Callback, (XtPointer)sp);

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
      sw[W_reset] = XtCreateManagedWidget(STR_Reset, xmPushButtonWidgetClass, sw[W_amp_form], args, n);
      XtAddCallback(sw[W_reset], XmNhelpCallback, W_reset_Help_Callback, ss);
      XtAddEventHandler(sw[W_reset], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
      XtAddCallback(sw[W_reset], XmNactivateCallback, reset_control_panel_Callback, (XtPointer)sp);


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
      sw[W_filter_frame] = sndCreateFrameWidget("filter-frame", sw[W_amp_form], args, n);

      n = 0;
      if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, TRUE); n++;
      sw[W_filter_env] = sndCreateDrawingAreaWidget("filter-window", sw[W_filter_frame], args, n);
      XtAddCallback(sw[W_filter_env], XmNhelpCallback, filter_drawer_help_Callback, ss);
      XtAddCallback(sw[W_filter_env], XmNresizeCallback, filter_drawer_resize, (XtPointer)sp);
      XtAddCallback(sw[W_filter_env], XmNexposeCallback, filter_drawer_resize, (XtPointer)sp);

      new_flt(sp);

      XtAddEventHandler(sw[W_filter_env], ButtonPressMask, FALSE, filter_drawer_button_press, sp);
      XtAddEventHandler(sw[W_filter_env], ButtonMotionMask, FALSE, filter_drawer_button_motion, sp);
      XtAddEventHandler(sw[W_filter_env], ButtonReleaseMask, FALSE, filter_drawer_button_release, sp);
      XtAddEventHandler(sw[W_filter_env], KeyPressMask, FALSE, graph_key_press, (XtPointer)sp);
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
      FREE(n11);
      FREE(n12);
      /* end if control-panel */
#if (XmVERSION > 1)
      if (sound_style(ss) == SOUNDS_IN_NOTEBOOK)
	{
	  char name[MAX_NOTEBOOK_TAB_LENGTH+11];
	  strncpy(name, just_filename(sp->shortname), MAX_NOTEBOOK_TAB_LENGTH);
	  name[MAX_NOTEBOOK_TAB_LENGTH] ='\0';
	  n = 0;
	  if (need_colors) {XtSetArg(args[n], XmNbackground, (ss->sgx)->graph_color); n++;}
	  XtSetArg(args[n], XmNnotebookChildType, XmMAJOR_TAB); n++;
	  sx->tab = XtCreateManagedWidget(name, xmPushButtonWidgetClass, SOUND_PANE(ss), args, n);
	}
#endif

    } /* new sound ss */
  else
    { /* re-manage currently inactive chan */
      if (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS)
	XtVaSetValues(sw[W_ctrls],
		      XmNpaneMinimum, ss->ctrls_height,
		      XmNpaneMaximum, ss->ctrls_height,
		      NULL);
      else 
	{
	  title = (char *)CALLOC(128, sizeof(char));
	  sprintf(title, "%d: %s", snd_slot, sp->shortname);
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
      set_button_label(sw[W_revscl_label], reverb_name());
      if (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS)
	XtVaSetValues(sw[W_ctrls],
		      XmNpaneMinimum, 1,
		      XmNpaneMaximum, LOTSA_PIXELS,
		      NULL);
#if (XmVERSION > 1)
      if (sound_style(ss) == SOUNDS_IN_NOTEBOOK)
	{
	  set_label(sx->tab, just_filename(sp->shortname));
	}
#endif
    }
  if (sp->nchans == 1) 
    {
      XmToggleButtonSetState(w_snd_combine(sp), FALSE, FALSE);
      XtUnmanageChild(w_snd_combine(sp));
    }
  add_sound_data(filename, sp, ss);
  snd_file_lock_icon(sp, (ss->viewing || (cant_write(sp->fullname)))); /* sp->read_only not set yet */
  if (ss->pending_change)
    report_in_minibuffer(sp, "(translated %s)", old_name);
  if (!(ss->using_schemes)) map_over_children(SOUND_PANE(ss), color_sashes, (void *)ss);
  if (!(auto_resize(ss))) normalize_all_sounds(ss);
  
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
      first_window = 0;
    } 
  if (sound_style(ss) != SOUNDS_IN_SEPARATE_WINDOWS)
    {
      /* if control-panel */
      if (make_widgets) 
	XtVaSetValues(sw[W_ctrls],
		      XmNpaneMaximum, LOTSA_PIXELS,
		      NULL); /* locked above to force correct initial setup */
      reset_control_panel(sp);
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
		    XmNheight, (Dimension)(widget_height(MAIN_SHELL(ss)) / 2),
		    NULL);
    }
#if HAVE_HOOKS
  after_open(sp->index);
#endif
  return(sp);
}

void snd_info_cleanup(snd_info *sp)
{
  snd_context *sx;
  snd_state *ss;
  if ((sp) && (sp->sgx))
    {
      sx = sp->sgx;
      ss = sp->state;
      if (w_snd_sync(sp))
	{
	  XtVaSetValues(w_snd_sync(sp), XmNset, FALSE, NULL);
	  XtVaSetValues(w_snd_expand_button(sp), XmNset, FALSE, NULL);
	  XtVaSetValues(w_snd_contrast_button(sp), XmNset, FALSE, NULL);
	  XtVaSetValues(w_snd_srate_arrow(sp), XmNset, FALSE, NULL);
	  XtVaSetValues(w_snd_filter_button(sp), XmNset, FALSE, NULL);
	  XtVaSetValues(w_snd_reverb_button(sp), XmNset, FALSE, NULL);
	  XmToggleButtonSetState(w_snd_combine(sp), FALSE, FALSE);
	  sp->combining = CHANNELS_SEPARATE;
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
  set_button_label(w_snd_name(sp), str);
}

void unlock_ctrls(snd_info *sp) {XtVaSetValues(w_snd_ctrls(sp), XmNpaneMinimum, 1, NULL);}

void set_apply_button(snd_info *sp, int val) {XmToggleButtonSetState(w_snd_apply(sp), val, FALSE);}

void normalize_sound(snd_state *ss, snd_info *sp, chan_info *ncp)
{
  /* make sp look ok, squeezing others if needed; called only if normalize_on_open(ss) */
  /* if there's already enough (i.e. ss->channel_min_height), just return */
  /* this is used in goto_next_graph and goto_previous_graph (snd-chn.c) to open windows that are currently squeezed shut */
  Float low, high;
  Dimension chan_y;
  int *wid;
  chan_info *cp = NULL;
  if ((!ss) || (!sp) || (!(normalize_on_open(ss))) || (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS)) return;
  if (sound_style(ss) != SOUNDS_HORIZONTAL)
    {
      /* several attempts to be fancy here just made a mess of the display */
      XtVaGetValues(channel_main_pane(ncp), XmNheight, &chan_y, NULL);
      if (chan_y < (Dimension)(ss->channel_min_height>>1)) 
	{
	  wid = (int *)CALLOC(1, sizeof(int));
	  (*wid) = (ss->channel_min_height>>1) + 10;
	  channel_lock_pane(ncp, (void *)wid);
	  channel_open_pane(ncp, NULL);
	  channel_unlock_pane(ncp, NULL);
	  FREE(wid);
	  wid = NULL;
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
  if (sp->combining == CHANNELS_COMBINED)
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
      if ((sp) && (sp->inuse)) sp_display_env(sp);
    }
}

void reflect_amp_env_completion(snd_info *sp)
{
  chan_info *cp;
  env_info *ep;
  int i;
  Widget info_sep;
  /* a channel completed an amp env, check to see if all are complete */
  for (i = 0; i < sp->nchans; i++)
    {
      cp = sp->chans[i];
      if (!(cp->amp_envs)) return;
      ep = cp->amp_envs[cp->edit_ctr];
      if (!ep) return;
      if (!(ep->completed)) return;
    }
  info_sep = w_snd_minibuffer_sep(sp);
  if (info_sep) XtVaSetValues(info_sep, XmNseparatorType, XmSHADOW_ETCHED_IN, NULL);
#if DEBUGGING
  /* stop_timing(); */
#endif
  alert_enved_amp_env(sp);
}

void reflect_amp_env_in_progress(snd_info *sp)
{
  Widget info_sep;
  info_sep = w_snd_minibuffer_sep(sp);
  if (info_sep) XtVaSetValues(info_sep, XmNseparatorType, XmNO_LINE, NULL);
#if DEBUGGING
  /* start_timing(); */
#endif
}

/* ---------------- normalize sounds ---------------- */

static int even_channels(snd_info *sp, void *ptr)
{
  int val, height, chans, i;
  chan_info *cp;
  chans = sp->nchans;
  if (chans > 1)
    {
      height = (int)ptr;
      val = height/chans - 16;
      if (val < 6) val = 6;
      for (i = 0; i < chans; i++)
	{
	  cp = sp->chans[i];
	  XtUnmanageChild(channel_main_pane(cp));
	  XtVaSetValues(channel_main_pane(cp),
			XmNpaneMinimum, val-5,
			XmNpaneMaximum, val+5,
			NULL);
	}
    }
  return(0);
}

static int even_sounds(snd_info *sp, void *ptr)
{
  int width;
  width = (int)ptr;
  XtUnmanageChild(w_snd_pane(sp));
  XtVaSetValues(w_snd_pane(sp),
		XmNpaneMinimum, width-5,
		XmNpaneMaximum, width+5,
		NULL);
  return(0);
}

static int sound_open_pane(snd_info *sp, void *ptr)
{
  XtManageChild(w_snd_pane(sp));
  return(0);
}

static int sound_unlock_pane(snd_info *sp, void *ptr)
{
  XtVaSetValues(w_snd_pane(sp),
		XmNpaneMinimum, 5,
		XmNpaneMaximum, LOTSA_PIXELS,
		NULL);
  return(0);
}

void normalize_all_sounds(snd_state *ss)
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
		even_channels(nsp, (void *)height);
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
      /* if lisp listener, remove it from this calculation */
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
	  chan_y = (height-(sounds*ss->ctrls_height))/chans - 16;
	  /* probably can be 14 or 12 -- seems to be margin related or something */
	  wid[0] = chan_y;
	  map_over_sounds(ss, sound_lock_ctrls, NULL);
	  map_over_separate_chans(ss, channel_lock_pane, (void *)wid);
	  map_over_separate_chans(ss, channel_open_pane, NULL);
	  map_over_separate_chans(ss, channel_unlock_pane, NULL);
	  map_over_sounds(ss, sound_unlock_ctrls, NULL);
	}
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
	      map_over_sounds(ss, even_sounds, (void *)width);
	      map_over_sounds(ss, sound_open_pane, NULL);
	      map_over_sounds(ss, sound_unlock_pane, NULL);
	    }
	  map_over_sounds(ss, sound_lock_ctrls, NULL);
	  map_over_sounds(ss, even_channels, (void *)height);
	  map_over_separate_chans(ss, channel_open_pane, NULL);   /* manage the channel widgets */
	  map_over_separate_chans(ss, channel_unlock_pane, NULL); /* allow pane to be resized */
	  map_over_sounds(ss, sound_unlock_ctrls, NULL);
	}
    }
}

void sound_show_ctrls(snd_info *sp)
{
  snd_state *ss;
  ss = sp->state;
  XtUnmanageChild(w_snd_ctrls(sp));
  XtVaSetValues(w_snd_ctrls(sp),
		XmNpaneMinimum, ss->open_ctrls_height,
		XmNpaneMaximum, ss->open_ctrls_height,
		NULL);
  XtManageChild(w_snd_ctrls(sp));
  XtVaSetValues(w_snd_ctrls(sp),
		XmNpaneMinimum, 1,
		XmNpaneMaximum, LOTSA_PIXELS,
		NULL);
}

void sound_hide_ctrls(snd_info *sp)
{
  XtUnmanageChild(w_snd_ctrls(sp));
  XtVaSetValues(w_snd_ctrls(sp),
		XmNpaneMaximum, CLOSED_CTRLS_HEIGHT,
		XmNpaneMinimum, CLOSED_CTRLS_HEIGHT,
		NULL);
  XtManageChild(w_snd_ctrls(sp));
  XtVaSetValues(w_snd_ctrls(sp),
		XmNpaneMinimum, 1,
		XmNpaneMaximum, LOTSA_PIXELS,
		NULL);
}

int control_panel_open(snd_info *sp)
{
  Dimension hgt;
  XtVaGetValues(w_snd_ctrls(sp), XmNheight, &hgt, NULL);
  return(hgt > CLOSED_CTRLS_HEIGHT);
}

void show_controls(snd_state *ss)
{
  snd_info *sp;
  int i;
  ss->ctrls_height = ss->open_ctrls_height;
  set_view_ctrls_label(STR_Hide_controls);
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
  set_view_ctrls_label(STR_Show_controls);
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse)) 
	sound_hide_ctrls(sp);
    }
}


/* -------- PROGRESS REPORT -------- */
/*
 * if no xpm, send a string, else post an hourglass
 */

void progress_report(snd_info *sp, const char *funcname, int curchan, int chans, Float pct, int from_enved)
{
  int which;
#if HAVE_XPM
  which = (int)(pct * NUM_GLASSES);
  if (which >= NUM_GLASSES) which = NUM_GLASSES-1;
  if (which < 0) which = 0;
  if (from_enved)
    display_enved_progress(NULL, mini_glasses[which]);
  else snd_file_glasses_icon(sp, TRUE, which);
#else
  char *expr_str;
  expr_str = (char *)CALLOC(128, sizeof(char));
  which = (int)(100.0 * pct);
  if (chans > 1)
    sprintf(expr_str, "%s: (%d of %d) %d%%", funcname, curchan, chans, which);
  else sprintf(expr_str, "%s: %d%%", funcname, which);
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
#else
  char *expr_str;
  snd_state *ss;
  ss = get_global_state();
  expr_str = (char *)CALLOC(128, sizeof(char));
  if (ss->stopped_explicitly) 
    sprintf(expr_str, "stopped"); 
  else expr_str[0] ='\0';
  if (from_enved)
    display_enved_progress(expr_str, 0);
  else report_in_minibuffer(sp, expr_str);
  FREE(expr_str);
#endif
}

void start_progress_report(snd_info *sp, int from_enved)
{
#if HAVE_XPM
  if (!(from_enved)) snd_file_glasses_icon(sp, TRUE, 0);
#else
  char *expr_str;
  if (from_enved)
    {
      expr_str = (char *)CALLOC(4, sizeof(char));
      expr_str[0] ='\0';
      display_enved_progress(expr_str, 0);
      FREE(expr_str);
    }
#endif
}
