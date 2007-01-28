#include "snd.h"
#include "sndlib-strings.h"

/* preferences dialog; layout design taken from webmail
 */

/* should the exit key work in the listener as well? -- how to handle C-x cmds in xttranslation tables?

   abandoned:
       preset packages: dlp, ksm
       emacs setup
         how to tie into emacs?
       audio mixer settings? -> volume in some mode (snd6.scm has OSS version)
         "startup dac volume" -- but this will be confusing since we don't notice mute settings etc
       clm instruments? -- surely user should learn about the listener...
*/


static Widget preferences_dialog = NULL, load_path_text_widget = NULL;
static bool prefs_helping = false, prefs_unsaved = false;
static char *prefs_saved_filename = NULL;
static char *include_load_path = NULL;

#define MID_POSITION 40
#define COLOR_POSITION 50
#define FIRST_COLOR_POSITION 6
#define SECOND_COLOR_POSITION 30
#define THIRD_COLOR_POSITION 55
#define HELP_POSITION 80

#define MID_SPACE 16
#define INTER_TOPIC_SPACE 3
#define INTER_VARIABLE_SPACE 2

#define HELP_WAIT_TIME 500
#define POWER_WAIT_TIME 100
#define POWER_INITIAL_WAIT_TIME 500
#define ERROR_WAIT_TIME 5000

#define STARTUP_WIDTH 925
#define STARTUP_HEIGHT 800

typedef struct prefs_info {
  Widget label, text, arrow_up, arrow_down, arrow_right, error, toggle, scale, toggle2, toggle3;
  Widget color, rscl, gscl, bscl, rtxt, gtxt, btxt, list_menu, radio_button;
  Widget *radio_buttons;
  bool got_error;
  XtIntervalId help_id, power_id;
  const char *var_name, *saved_label;
  const char **values;
  int num_values, num_buttons;
  Float scale_max;
  void (*toggle_func)(struct prefs_info *prf);
  void (*toggle2_func)(struct prefs_info *prf);
  void (*scale_func)(struct prefs_info *prf);
  void (*arrow_up_func)(struct prefs_info *prf);
  void (*arrow_down_func)(struct prefs_info *prf);
  void (*text_func)(struct prefs_info *prf);
  void (*list_func)(struct prefs_info *prf, char *value);
  void (*color_func)(struct prefs_info *prf, float r, float g, float b);
  void (*reflect_func)(struct prefs_info *prf);
  void (*save_func)(struct prefs_info *prf, FILE *fd);
  void (*help_func)(struct prefs_info *prf);
  void (*clear_func)(struct prefs_info *prf);
  void (*revert_func)(struct prefs_info *prf);
} prefs_info;


static void prefs_set_dialog_title(const char *filename);
static void reflect_key(prefs_info *prf, const char *key_name);
static void save_key_binding(prefs_info *prf, FILE *fd, char *(*binder)(char *key, bool c, bool m, bool x));
static void key_bind(prefs_info *prf, char *(*binder)(char *key, bool c, bool m, bool x));
static void clear_prefs_dialog_error(void);
static void scale_set_color(prefs_info *prf, color_t pixel);
static color_t rgb_to_color(Float r, Float g, Float b);
static void post_prefs_error(const char *msg, prefs_info *data);
#ifdef __GNUC__
  static void va_post_prefs_error(const char *msg, prefs_info *data, ...) __attribute__ ((format (printf, 1, 0)));
#else
  static void va_post_prefs_error(const char *msg, prefs_info *data, ...);
#endif

#define GET_TOGGLE(Toggle)        (XmToggleButtonGetState(Toggle) == XmSET)
#define SET_TOGGLE(Toggle, Value) XmToggleButtonSetState(Toggle, Value, false)
#define GET_TEXT(Text)            XmTextFieldGetString(Text)
#define SET_TEXT(Text, Val)       XmTextFieldSetString(Text, Val)
#define FREE_TEXT(Val)            XtFree(Val)
#define TIMEOUT(Func)             XtAppAddTimeOut(MAIN_APP(ss), ERROR_WAIT_TIME, Func, (XtPointer)prf)
#define TIMEOUT_ARGS              XtPointer context, XtIntervalId *id
#define TIMEOUT_TYPE              void
#define TIMEOUT_RESULT            
#define SET_SCALE(Value)          XmScaleSetValue(prf->scale, (int)(100 * Value))
#define SET_SENSITIVE(Wid, Val)   XtSetSensitive(Wid, Val)
#define black_text(Prf)           XtVaSetValues(Prf->label, XmNforeground, ss->sgx->black, NULL)
#define red_text(Prf)             XtVaSetValues(Prf->label, XmNforeground, ss->sgx->red, NULL)


static int get_scale_1(Widget scale)
{
  int val = 0;
  XmScaleGetValue(scale, &val);
  return(val);
}

#define GET_SCALE()               (get_scale_1(prf->scale) * 0.01)

static void set_radio_button(prefs_info *prf, int which)
{
  if ((which >= 0) && (which < prf->num_buttons))
    {
      int i;
      for (i = 0; i < prf->num_buttons; i++)
	{
	  if ((prf->radio_buttons[i]) &&
	      (XmIsToggleButton(prf->radio_buttons[i])))
	    XmToggleButtonSetState(prf->radio_buttons[i], (i == which), false);
	}
      prf->radio_button = prf->radio_buttons[which];
    }
}

static int which_radio_button(prefs_info *prf)
{
  int which = 0;
  XtVaGetValues(prf->radio_button, XmNuserData, &which, NULL);
  return(which);
}


#include "snd-prefs.c"



/* ---------------- help strings ---------------- */

static void prefs_help_click_callback(Widget w, XtPointer context, XtPointer info)
{
  prefs_help((prefs_info *)context);
}

static void prefs_tooltip_help(XtPointer context, XtIntervalId *id)
{
  prefs_info *prf = (prefs_info *)context;
  if (help_dialog_is_active())
    prefs_help(prf);
  else prefs_helping = false;
  prf->help_id = 0;
}

static void mouse_enter_pref_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  prefs_info *prf = (prefs_info *)context;
  if (prefs_helping)
    prf->help_id = XtAppAddTimeOut(MAIN_APP(ss),
				   HELP_WAIT_TIME,
				   prefs_tooltip_help,
				   (XtPointer)prf);
}

static void mouse_leave_pref_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  prefs_info *prf = (prefs_info *)context;
  if (prf->help_id != 0)
    {
      XtRemoveTimeOut(prf->help_id);
      prf->help_id = 0;
    }
}

static void mouse_help_click_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  prefs_help((prefs_info *)context);
}

static bool prefs_dialog_error_is_posted = false;

static void post_prefs_dialog_error(const char *message, void *data)
{
  XmString title;
  title = XmStringCreateLocalized((char *)message);
  XtVaSetValues(preferences_dialog, 
		XmNmessageString, title, 
		NULL);
  XmStringFree(title);
  prefs_dialog_error_is_posted = (message != NULL);
}

static void clear_prefs_dialog_error(void)
{
  if (prefs_dialog_error_is_posted)
    {
      prefs_dialog_error_is_posted = false;
      post_prefs_dialog_error(NULL, NULL);
    }
}

static void prefs_change_callback(Widget w, XtPointer context, XtPointer info)
{
  prefs_unsaved = true;
  prefs_set_dialog_title(NULL);
  clear_prefs_dialog_error();
}



/* ---------------- row (main) label widget ---------------- */

static Widget make_row_label(prefs_info *prf, const char *label, Widget box, Widget top_widget)
{
  Widget w;
  Arg args[20];
  int n;

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
  XtSetArg(args[n], XmNrightPosition, MID_POSITION); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_END); n++;
  w = XtCreateManagedWidget(label, xmLabelWidgetClass, box, args, n);
  prf->saved_label = label;

  XtAddEventHandler(w, EnterWindowMask, false, mouse_enter_pref_callback, (XtPointer)prf);
  XtAddEventHandler(w, LeaveWindowMask, false, mouse_leave_pref_callback, (XtPointer)prf);
  XtAddEventHandler(w, ButtonPressMask, false, mouse_help_click_callback, (XtPointer)prf);

  return(w);
}

/* ---------------- row inner label widget ---------------- */

static Widget make_row_inner_label(prefs_info *prf, const char *label, Widget left_widget, Widget box, Widget top_widget)
{
  Widget w;
  Arg args[20];
  int n;

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, left_widget); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  w = XtCreateManagedWidget(label, xmLabelWidgetClass, box, args, n);

  XtAddEventHandler(w, EnterWindowMask, false, mouse_enter_pref_callback, (XtPointer)prf);
  XtAddEventHandler(w, LeaveWindowMask, false, mouse_leave_pref_callback, (XtPointer)prf);
  XtAddEventHandler(w, ButtonPressMask, false, mouse_help_click_callback, (XtPointer)prf);

  return(w);
}

/* ---------------- row middle separator widget ---------------- */

static Widget make_row_middle_separator(Widget label, Widget box, Widget top_widget)
{
  Arg args[20];
  int n;

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
  XtSetArg(args[n], XmNbottomWidget, label); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, label); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
  XtSetArg(args[n], XmNwidth, MID_SPACE); n++;
  XtSetArg(args[n], XmNseparatorType, XmSHADOW_ETCHED_OUT); n++;
  return(XtCreateManagedWidget("sep", xmSeparatorWidgetClass, box, args, n));
}

/* ---------------- row inner separator widget ---------------- */

static Widget make_row_inner_separator(int width, Widget left_widget, Widget box, Widget top_widget)
{
  Arg args[20];
  int n;

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, left_widget); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
  XtSetArg(args[n], XmNwidth, width); n++;
  XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
  return(XtCreateManagedWidget("sep1", xmSeparatorWidgetClass, box, args, n));
}

/* ---------------- row help widget ---------------- */

static Widget make_row_help(prefs_info *prf, const char *label, Widget box, Widget top_widget, Widget left_widget)
{
  Arg args[20];
  int n;
  XmString s1;
  Widget helper, spacer;

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
  XtSetArg(args[n], XmNbottomWidget, left_widget); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, left_widget); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
  XtSetArg(args[n], XmNrightPosition, HELP_POSITION + 1); n++;
  XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
  XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
  spacer = XtCreateManagedWidget("spacer", xmSeparatorWidgetClass, box, args, n);

  n = 0;
  s1 = XmStringCreateLocalized((char *)label);
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, spacer); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
  XtSetArg(args[n], XmNlabelString, s1); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNhighlightThickness, 0); n++;
  XtSetArg(args[n], XmNfillOnArm, false); n++;
  helper = XtCreateManagedWidget(label, xmPushButtonWidgetClass, box, args, n);
  XmStringFree(s1);

  XtAddCallback(helper, XmNactivateCallback, prefs_help_click_callback, (XtPointer)prf);

  XtAddEventHandler(helper, EnterWindowMask, false, mouse_enter_pref_callback, (XtPointer)prf);
  XtAddEventHandler(helper, LeaveWindowMask, false, mouse_leave_pref_callback, (XtPointer)prf);

  return(helper);
}

static Widget make_row_error(prefs_info *prf, Widget box, Widget left_widget, Widget top_widget)
{
  Arg args[20];
  int n;
  Widget w;

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, left_widget); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
  XtSetArg(args[n], XmNrightPosition, HELP_POSITION); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_END); n++;
  w = XtCreateManagedWidget("", xmLabelWidgetClass, box, args, n);

  XtAddEventHandler(w, EnterWindowMask, false, mouse_enter_pref_callback, (XtPointer)prf);
  XtAddEventHandler(w, LeaveWindowMask, false, mouse_leave_pref_callback, (XtPointer)prf);
  XtAddEventHandler(w, ButtonPressMask, false, mouse_help_click_callback, (XtPointer)prf);

  return(w);
}

/* ---------------- row toggle widget ---------------- */

static Widget make_row_toggle_with_label(prefs_info *prf, bool current_value, Widget left_widget, Widget box, Widget top_widget, const char *label)
{
  Widget w;
  Arg args[20];
  int n;

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, left_widget); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNset, (current_value) ? XmSET : XmUNSET); n++;
  XtSetArg(args[n], XmNborderWidth, 0); n++;
  XtSetArg(args[n], XmNborderColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNmarginHeight, 0); n++;
  XtSetArg(args[n], XmNindicatorOn, XmINDICATOR_FILL); n++;
  XtSetArg(args[n], XmNindicatorSize, 14); n++;
  w = XtCreateManagedWidget(label, xmToggleButtonWidgetClass, box, args, n);

  XtAddEventHandler(w, EnterWindowMask, false, mouse_enter_pref_callback, (XtPointer)prf);
  XtAddEventHandler(w, LeaveWindowMask, false, mouse_leave_pref_callback, (XtPointer)prf);
  XtAddCallback(w, XmNvalueChangedCallback, prefs_change_callback, NULL);

  return(w);
}


static Widget make_row_toggle(prefs_info *prf, bool current_value, Widget left_widget, Widget box, Widget top_widget)
{
  return(make_row_toggle_with_label(prf, current_value, left_widget, box, top_widget, " "));
}



/* ---------------- row arrows ---------------- */

static void remove_arrow_func(Widget w, XtPointer context, XtPointer info)
{
  prefs_info *prf = (prefs_info *)context;
  if (prf->power_id != 0)
    {
      XtRemoveTimeOut(prf->power_id);
      prf->power_id = 0;
    }
}

static void arrow_func_up(XtPointer context, XtIntervalId *id)
{
  prefs_info *prf = (prefs_info *)context;
  if (XtIsSensitive(prf->arrow_up))
    {
      if ((prf) && (prf->arrow_up_func))
	{
	  (*(prf->arrow_up_func))(prf);
	  prf->power_id = XtAppAddTimeOut(MAIN_APP(ss),
					  POWER_WAIT_TIME,
					  arrow_func_up,
					  (XtPointer)prf);
	}
      else prf->power_id = 0;
    }
}

static void arrow_func_down(XtPointer context, XtIntervalId *id)
{
  prefs_info *prf = (prefs_info *)context;
  if (XtIsSensitive(prf->arrow_down))
    {
      if ((prf) && (prf->arrow_down_func))
	{
	  (*(prf->arrow_down_func))(prf);
	  prf->power_id = XtAppAddTimeOut(MAIN_APP(ss),
					  POWER_WAIT_TIME,
					  arrow_func_down,
					  (XtPointer)prf);
	}
      else prf->power_id = 0;
    }
}

static void call_arrow_down_press(Widget w, XtPointer context, XtPointer info) 
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->arrow_down_func))
    {
      (*(prf->arrow_down_func))(prf);
      if (XtIsSensitive(w))
	prf->power_id = XtAppAddTimeOut(MAIN_APP(ss),
					POWER_INITIAL_WAIT_TIME,
					arrow_func_down,
					(XtPointer)prf);
      else prf->power_id = 0;
    }
}

static void call_arrow_up_press(Widget w, XtPointer context, XtPointer info) 
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->arrow_up_func))
    {
      (*(prf->arrow_up_func))(prf);
      if (XtIsSensitive(w))
	prf->power_id = XtAppAddTimeOut(MAIN_APP(ss),
					POWER_INITIAL_WAIT_TIME,
					arrow_func_up,
					(XtPointer)prf);
      else prf->power_id = 0;
    }
}

static Widget make_row_arrows(prefs_info *prf, Widget box, Widget left_widget, Widget top_widget)
{
  Arg args[20];
  int n;

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, left_widget); n++;
  XtSetArg(args[n], XmNarrowDirection, XmARROW_DOWN); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  prf->arrow_down = XtCreateManagedWidget("arrow-down", xmArrowButtonWidgetClass, box, args, n);
  
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, prf->arrow_down); n++;
  XtSetArg(args[n], XmNarrowDirection, XmARROW_UP); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  prf->arrow_up = XtCreateManagedWidget("arrow-up", xmArrowButtonWidgetClass, box, args, n);
  
  XtAddEventHandler(prf->arrow_up, EnterWindowMask, false, mouse_enter_pref_callback, (XtPointer)prf);
  XtAddEventHandler(prf->arrow_down, EnterWindowMask, false, mouse_enter_pref_callback, (XtPointer)prf);
  XtAddEventHandler(prf->arrow_up, LeaveWindowMask, false, mouse_leave_pref_callback, (XtPointer)prf);
  XtAddEventHandler(prf->arrow_down, LeaveWindowMask, false, mouse_leave_pref_callback, (XtPointer)prf);

  XtAddCallback(prf->arrow_down, XmNarmCallback, call_arrow_down_press, (XtPointer)prf);
  XtAddCallback(prf->arrow_down, XmNdisarmCallback, remove_arrow_func, (XtPointer)prf);
  XtAddCallback(prf->arrow_up, XmNarmCallback, call_arrow_up_press, (XtPointer)prf);
  XtAddCallback(prf->arrow_up, XmNdisarmCallback, remove_arrow_func, (XtPointer)prf);

  XtAddCallback(prf->arrow_up, XmNactivateCallback, prefs_change_callback, NULL);
  XtAddCallback(prf->arrow_down, XmNactivateCallback, prefs_change_callback, NULL);

  return(prf->arrow_up);
}

/* ---------------- bool row ---------------- */

static void call_toggle_func(Widget w, XtPointer context, XtPointer info)
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->toggle_func))
    (*(prf->toggle_func))(prf);
}

static prefs_info *prefs_row_with_toggle(const char *label, const char *varname, bool current_value,
					 Widget box, Widget top_widget, 
					 void (*toggle_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  Widget sep, help;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;

  prf->label = make_row_label(prf, label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  prf->toggle = make_row_toggle(prf, current_value, sep, box, top_widget);
  help = make_row_help(prf, varname, box, top_widget, prf->toggle);
  
  XtAddCallback(prf->toggle, XmNvalueChangedCallback, call_toggle_func, (XtPointer)prf);
  return(prf);
}


/* ---------------- two toggles ---------------- */

static void call_toggle2_func(Widget w, XtPointer context, XtPointer info)
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->toggle2_func))
    (*(prf->toggle2_func))(prf);
}

static prefs_info *prefs_row_with_two_toggles(const char *label, const char *varname, 
					      const char *label1, bool value1,
					      const char *label2, bool value2,
					      Widget box, Widget top_widget, 
					      void (*toggle_func)(prefs_info *prf),
					      void (*toggle2_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  Widget sep, help, sep1;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;
  prf->toggle2_func = toggle2_func;

  prf->label = make_row_label(prf, label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  prf->toggle = make_row_toggle_with_label(prf, value1, sep, box, top_widget, label1);
  sep1 = make_row_inner_separator(20, prf->toggle, box, top_widget);
  prf->toggle2 = make_row_toggle_with_label(prf, value2, sep1, box, top_widget, label2);
  help = make_row_help(prf, varname, box, top_widget, prf->toggle2);

  XtAddCallback(prf->toggle, XmNvalueChangedCallback, call_toggle_func, (XtPointer)prf);
  XtAddCallback(prf->toggle2, XmNvalueChangedCallback, call_toggle2_func, (XtPointer)prf);
  return(prf);
}


/* ---------------- toggle with text ---------------- */

static Widget make_row_text(prefs_info *prf, const char *text_value, int cols, Widget left_widget, Widget box, Widget top_widget)
{
  Widget w;
  int n;
  Arg args[20];

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, left_widget); n++;
  if (cols != 0)
    {
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNcolumns, cols); n++;
    }
  else
    {
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, HELP_POSITION); n++;
    }
  if (text_value)
    {
      XtSetArg(args[n], XmNvalue, text_value); n++;
    }
  XtSetArg(args[n], XmNmarginHeight, 1); n++;
  XtSetArg(args[n], XmNborderWidth, 0); n++;
  XtSetArg(args[n], XmNborderColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNbottomShadowColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNtopShadowColor, ss->sgx->white); n++;
  w = make_textfield_widget("text", box, args, n, ACTIVATABLE_BUT_NOT_FOCUSED, NO_COMPLETER);

  XtAddEventHandler(w, EnterWindowMask, false, mouse_enter_pref_callback, (XtPointer)prf);
  XtAddEventHandler(w, LeaveWindowMask, false, mouse_leave_pref_callback, (XtPointer)prf);

  XtAddCallback(w, XmNactivateCallback, prefs_change_callback, NULL);

  return(w);
}

static void call_text_func(Widget w, XtPointer context, XtPointer info) 
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->text_func))
    (*(prf->text_func))(prf);
}

static prefs_info *prefs_row_with_toggle_with_text(const char *label, const char *varname, bool current_value,
						   const char *text_label, const char *text_value, int cols,
						   Widget box, Widget top_widget, 
						   void (*toggle_func)(prefs_info *prf),
						   void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  Widget sep, sep1, lab1, help;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;
  prf->text_func = text_func;

  prf->label = make_row_label(prf, label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  prf->toggle = make_row_toggle(prf, current_value, sep, box, top_widget);
  sep1 = make_row_inner_separator(16, prf->toggle, box, top_widget);
  lab1 = make_row_inner_label(prf, text_label, sep1, box, top_widget);
  prf->text = make_row_text(prf, text_value, cols, lab1, box, top_widget);
  help = make_row_help(prf, varname, box, top_widget, prf->text);
  
  XtAddCallback(prf->toggle, XmNvalueChangedCallback, call_toggle_func, (XtPointer)prf);
  XtAddCallback(prf->text, XmNactivateCallback, call_text_func, (XtPointer)prf);

  return(prf);
}

static prefs_info *prefs_row_with_toggle_with_two_texts(const char *label, const char *varname, bool current_value,
							const char *label1, const char *text1, 
							const char *label2, const char *text2, int cols,
							Widget box, Widget top_widget,
							void (*toggle_func)(prefs_info *prf),
							void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  Widget sep, lab1, lab2, help, sep1;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;
  prf->text_func = text_func;

  prf->label = make_row_label(prf, label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  prf->toggle = make_row_toggle(prf, current_value, sep, box, top_widget);
  sep1 = make_row_inner_separator(16, prf->toggle, box, top_widget);
  lab1 = make_row_inner_label(prf, label1, sep1, box, top_widget);
  prf->text = make_row_text(prf, text1, cols, lab1, box, top_widget);
  lab2 = make_row_inner_label(prf, label2, prf->text, box, top_widget);  
  prf->rtxt = make_row_text(prf, text2, cols, lab2, box, top_widget);
  help = make_row_help(prf, varname, box, top_widget, prf->rtxt);

  XtAddCallback(prf->toggle, XmNvalueChangedCallback, call_toggle_func, (XtPointer)prf);
  XtAddCallback(prf->text, XmNactivateCallback, call_text_func, (XtPointer)prf);
  XtAddCallback(prf->rtxt, XmNactivateCallback, call_text_func, (XtPointer)prf);

  return(prf);
}


/* ---------------- text with toggle ---------------- */

static prefs_info *prefs_row_with_text_with_toggle(const char *label, const char *varname, bool current_value,
						   const char *toggle_label, const char *text_value, int cols,
						   Widget box, Widget top_widget, 
						   void (*toggle_func)(prefs_info *prf),
						   void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  Widget sep, sep1, lab1, help;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;
  prf->text_func = text_func;

  prf->label = make_row_label(prf, label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  prf->text = make_row_text(prf, text_value, cols, sep, box, top_widget);
  sep1 = make_row_inner_separator(8, prf->text, box, top_widget);
  lab1 = make_row_inner_label(prf, toggle_label, sep1, box, top_widget);
  prf->toggle = make_row_toggle(prf, current_value, lab1, box, top_widget);  
  help = make_row_help(prf, varname, box, top_widget, prf->toggle);
  
  XtAddCallback(prf->toggle, XmNvalueChangedCallback, call_toggle_func, (XtPointer)prf);
  XtAddCallback(prf->text, XmNactivateCallback, call_text_func, (XtPointer)prf);

  return(prf);
}


/* ---------------- text with toggle ---------------- */

static prefs_info *prefs_row_with_text_and_three_toggles(const char *label, const char *varname,
							 const char *text_label, int cols,
							 const char *toggle1_label, const char *toggle2_label, const char *toggle3_label,
							 const char *text_value, 
							 bool toggle1_value, bool toggle2_value, bool toggle3_value,
							 Widget box, Widget top_widget, 
							 void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  Widget sep, sep1, lab1, sep2, lab2, lab3, sep3, lab4, help;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->text_func = text_func;

  prf->label = make_row_label(prf, label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  lab3 = make_row_inner_label(prf, text_label, sep, box, top_widget);
  prf->text = make_row_text(prf, text_value, cols, lab3, box, top_widget);
  sep1 = make_row_inner_separator(12, prf->text, box, top_widget);
  lab1 = make_row_inner_label(prf, toggle1_label, sep1, box, top_widget);
  prf->toggle = make_row_toggle(prf, toggle1_value, lab1, box, top_widget);  
  sep2 = make_row_inner_separator(4, prf->toggle, box, top_widget);
  lab2 = make_row_inner_label(prf, toggle2_label, sep2, box, top_widget);
  prf->toggle2 = make_row_toggle(prf, toggle2_value, lab2, box, top_widget);
  sep3 = make_row_inner_separator(4, prf->toggle2, box, top_widget);
  lab4 = make_row_inner_label(prf, toggle3_label, sep3, box, top_widget);
  prf->toggle3 = make_row_toggle(prf, toggle3_value, lab4, box, top_widget);
  help = make_row_help(prf, varname, box, top_widget, prf->toggle3);
  
  XtAddCallback(prf->text, XmNactivateCallback, call_text_func, (XtPointer)prf);

  return(prf);
}


/* ---------------- radio row ---------------- */

static void call_radio_func(Widget w, XtPointer context, XtPointer info)
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->toggle_func))
    {
      prf->radio_button = w;
      (*(prf->toggle_func))(prf);
    }
}

static Widget make_row_radio_box(prefs_info *prf,
				 const char **labels, int num_labels, int current_value,
				 Widget box, Widget left_widget, Widget top_widget)
{
  Arg args[20];
  int i, n;
  Widget w;

  prf->radio_buttons = (Widget *)CALLOC(num_labels, sizeof(Widget));
  prf->num_buttons = num_labels;

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, left_widget); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNborderWidth, 0); n++;
  XtSetArg(args[n], XmNborderColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNmarginHeight, 0); n++;
  XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
  XtSetArg(args[n], XmNpacking, XmPACK_TIGHT); n++;
  w = XmCreateRadioBox(box, "radio-box", args, n);
  XtManageChild(w);

  for (i = 0; i < num_labels; i++)
    {
      Widget button;

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
      XtSetArg(args[n], XmNset, (i == current_value) ? XmSET : XmUNSET); n++;
      XtSetArg(args[n], XmNborderWidth, 0); n++;
      XtSetArg(args[n], XmNborderColor, ss->sgx->white); n++;
      XtSetArg(args[n], XmNmarginHeight, 0); n++;
      XtSetArg(args[n], XmNindicatorOn, XmINDICATOR_FILL); n++;
      XtSetArg(args[n], XmNindicatorSize, 14); n++;
      XtSetArg(args[n], XmNselectColor, ss->sgx->green); n++;
      XtSetArg(args[n], XmNuserData, i); n++;
      button = XtCreateManagedWidget(labels[i], xmToggleButtonWidgetClass, w, args, n);
      prf->radio_buttons[i] = button;

      XtAddCallback(button, XmNvalueChangedCallback, call_radio_func, (XtPointer)prf);
      XtAddCallback(button, XmNvalueChangedCallback, prefs_change_callback, NULL);
      XtAddEventHandler(button, EnterWindowMask, false, mouse_enter_pref_callback, (XtPointer)prf);
      XtAddEventHandler(button, LeaveWindowMask, false, mouse_leave_pref_callback, (XtPointer)prf);
    }
  return(w);
}

static prefs_info *prefs_row_with_radio_box(const char *label, const char *varname, 
					    const char **labels, int num_labels, int current_value,
					    Widget box, Widget top_widget, 
					    void (*toggle_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  Widget sep, help;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;
  prf->label = make_row_label(prf, label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  prf->toggle = make_row_radio_box(prf, labels, num_labels, current_value, box, sep, top_widget);
  help = make_row_help(prf, varname, box, top_widget, prf->toggle);
  return(prf);
}

/* ---------------- radio box + number ---------------- */

static prefs_info *prefs_row_with_radio_box_and_number(const char *label, const char *varname, 
						       const char **labels, int num_labels, int current_value,
						       int number, const char *text_value, int text_cols,
						       Widget box, Widget top_widget, 
						       void (*toggle_func)(prefs_info *prf),
						       void (*arrow_up_func)(prefs_info *prf), void (*arrow_down_func)(prefs_info *prf), 
						       void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  Widget sep, help;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;
  prf->text_func = text_func;
  prf->arrow_up_func = arrow_up_func;
  prf->arrow_down_func = arrow_down_func;
  prf->label = make_row_label(prf, label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  prf->toggle = make_row_radio_box(prf, labels, num_labels, current_value, box, sep, top_widget);
  prf->text = make_row_text(prf, text_value, text_cols, prf->toggle, box, top_widget);
  prf->arrow_up = make_row_arrows(prf, box, prf->text, top_widget);
  help = make_row_help(prf, varname, box, top_widget, prf->arrow_up);

  XtAddCallback(prf->text, XmNactivateCallback, call_text_func, (XtPointer)prf);
  return(prf);
}


/* ---------------- scale row ---------------- */

static void call_scale_func(Widget w, XtPointer context, XtPointer info)
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->scale_func))
    (*(prf->scale_func))(prf);
}

static void call_scale_text_func(Widget w, XtPointer context, XtPointer info)
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->text_func))
    (*(prf->text_func))(prf);
}

static void prefs_scale_callback(Widget w, XtPointer context, XtPointer info)
{
  prefs_info *prf = (prefs_info *)context;
  XmScaleCallbackStruct *cb = (XmScaleCallbackStruct *)info;
  float_to_textfield(prf->text, (cb->value * prf->scale_max) / 100.0);
}

static prefs_info *prefs_row_with_scale(const char *label, const char *varname, 
					Float max_val, Float current_value,
					Widget box, Widget top_widget, 
					void (*scale_func)(prefs_info *prf),
					void (*text_func)(prefs_info *prf))
{
  Arg args[20];
  int n;
  prefs_info *prf = NULL;
  Widget sep, help;
  XtCallbackList n1, n2;
  char *str;

  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->scale_max = max_val;

  prf->label = make_row_label(prf, label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  
  str = (char *)CALLOC(12, sizeof(char));
  mus_snprintf(str, 12, "%.3f", current_value);
  prf->text = make_row_text(prf, str, 6, sep, box, top_widget);
  FREE(str);
  
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, prf->text); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
  XtSetArg(args[n], XmNrightPosition, HELP_POSITION); n++;
  XtSetArg(args[n], XmNborderWidth, 0); n++;
  XtSetArg(args[n], XmNborderColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNmarginHeight, 0); n++;
  XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
  XtSetArg(args[n], XmNshowValue, XmNONE); n++;
  XtSetArg(args[n], XmNvalue, (int)(100 * current_value / max_val)); n++;
  XtSetArg(args[n], XmNdragCallback, n1 = make_callback_list(prefs_scale_callback, (XtPointer)prf)); n++;
  XtSetArg(args[n], XmNvalueChangedCallback, n2 = make_callback_list(prefs_scale_callback, (XtPointer)prf)); n++;
  prf->scale = XtCreateManagedWidget("", xmScaleWidgetClass, box, args, n);
  help = make_row_help(prf, varname, box, top_widget, prf->scale);

  prf->scale_func = scale_func;
  prf->text_func = text_func;

  XtAddCallback(prf->scale, XmNvalueChangedCallback, call_scale_func, (XtPointer)prf);
  XtAddCallback(prf->text, XmNactivateCallback, call_scale_text_func, (XtPointer)prf);
  XtAddCallback(prf->scale, XmNvalueChangedCallback, prefs_change_callback, NULL);

  XtAddEventHandler(prf->scale, EnterWindowMask, false, mouse_enter_pref_callback, (XtPointer)prf);
  XtAddEventHandler(prf->scale, LeaveWindowMask, false, mouse_leave_pref_callback, (XtPointer)prf);

  FREE(n1);
  FREE(n2);

  return(prf);
}


/* ---------------- text row ---------------- */

static prefs_info *prefs_row_with_text(const char *label, const char *varname, const char *value,
				       Widget box, Widget top_widget,
				       void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  Widget sep, help;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;

  prf->label = make_row_label(prf, label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  prf->text = make_row_text(prf, value, 0, sep, box, top_widget);
  help = make_row_help(prf, varname, box, top_widget, prf->text);

  prf->text_func = text_func;
  XtAddCallback(prf->text, XmNactivateCallback, call_text_func, (XtPointer)prf);
  return(prf);
}



/* ---------------- two texts in a row ---------------- */

static prefs_info *prefs_row_with_two_texts(const char *label, const char *varname,
					    const char *label1, const char *text1, const char *label2, const char *text2, int cols,
					    Widget box, Widget top_widget,
					    void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  Widget sep, lab1, lab2, help;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;

  prf->label = make_row_label(prf, label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  lab1 = make_row_inner_label(prf, label1, sep, box, top_widget);
  prf->text = make_row_text(prf, text1, cols, lab1, box, top_widget);
  lab2 = make_row_inner_label(prf, label2, prf->text, box, top_widget);  
  prf->rtxt = make_row_text(prf, text2, cols, lab2, box, top_widget);
  help = make_row_help(prf, varname, box, top_widget, prf->rtxt);

  prf->text_func = text_func;
  XtAddCallback(prf->text, XmNactivateCallback, call_text_func, (XtPointer)prf);
  XtAddCallback(prf->rtxt, XmNactivateCallback, call_text_func, (XtPointer)prf);

  return(prf);
}


/* ---------------- number row ---------------- */

static prefs_info *prefs_row_with_number(const char *label, const char *varname, const char *value, int cols,
					 Widget box, Widget top_widget,
 					 void (*arrow_up_func)(prefs_info *prf), void (*arrow_down_func)(prefs_info *prf), 
					 void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  Widget sep, help;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;

  prf->label = make_row_label(prf, label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  prf->text = make_row_text(prf, value, cols, sep, box, top_widget);
  prf->arrow_up = make_row_arrows(prf, box, prf->text, top_widget);
  prf->error = make_row_error(prf, box, prf->arrow_up, top_widget);
  help = make_row_help(prf, varname, box, top_widget, prf->error);

  prf->text_func = text_func;
  prf->arrow_up_func = arrow_up_func;
  prf->arrow_down_func = arrow_down_func;

  XtAddCallback(prf->text, XmNactivateCallback, call_text_func, (XtPointer)prf);

  return(prf);
}


/* ---------------- list row ---------------- */

typedef struct {
  prefs_info *prf;
  char *value;
} list_entry;

static list_entry *make_list_entry(prefs_info *prf, char *value)
{
  list_entry *le;
  le = (list_entry *)CALLOC(1, sizeof(list_entry));
  le->prf = prf;
  le->value = value;
  return(le);
}

static void prefs_list_callback(Widget w, XtPointer context, XtPointer info)
{
  list_entry *le = (list_entry *)context;
  if ((le) && (le->prf->list_func))
    (*(le->prf->list_func))(le->prf, le->value);
}

static prefs_info *prefs_row_with_list(const char *label, const char *varname, const char *value,
				       const char **values, int num_values,
				       Widget box, Widget top_widget,
				       void (*text_func)(prefs_info *prf),
				       char *(*completion_func)(char *text, void *context), void *completion_context,
				       void (*list_func)(prefs_info *prf, char *value))
{
  Arg args[20];
  int n, i, cols = 0;
  prefs_info *prf = NULL;
  Widget sep, sbar, help;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;

  prf->label = make_row_label(prf, label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);  
  
  /* get text widget size */
  for (i = 0; i < num_values; i++)
    if (values[i])
      {
	int len;
	len = strlen(values[i]);
	if (len > cols) cols = len;
      }

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, sep); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNcolumns, cols + 1); n++;
  XtSetArg(args[n], XmNvalue, value); n++;
  XtSetArg(args[n], XmNmarginHeight, 1); n++;
  XtSetArg(args[n], XmNborderWidth, 0); n++;
  XtSetArg(args[n], XmNborderColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNbottomShadowColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNtopShadowColor, ss->sgx->white); n++;
  if (completion_func)
    prf->text = make_textfield_widget("text", box, args, n, ACTIVATABLE_BUT_NOT_FOCUSED, add_completer_func(completion_func, completion_context));
  else prf->text = make_textfield_widget("text", box, args, n, ACTIVATABLE_BUT_NOT_FOCUSED, NO_COMPLETER);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, prf->text); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNhighlightThickness, 0); n++;
  XtSetArg(args[n], XmNmarginHeight, 0); n++;
  sbar = XmCreateMenuBar(box, "menuBar", args, n);
  XtManageChild(sbar);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
  prf->list_menu = XmCreatePulldownMenu(sbar, "sort-menu", args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
  XtSetArg(args[n], XmNsubMenuId, prf->list_menu); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNhighlightThickness, 0); n++;
  XtSetArg(args[n], XmNmarginHeight, 1); n++;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, prf->text); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  prf->arrow_right = XtCreateManagedWidget(">", xmCascadeButtonWidgetClass, sbar, args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  for (i = 0; i < num_values; i++)
    if (values[i])
      {
	Widget tmp;
	tmp = XtCreateManagedWidget(values[i],  xmPushButtonWidgetClass, prf->list_menu, args, n);
	XtAddCallback(tmp, XmNactivateCallback, prefs_list_callback, make_list_entry(prf, (char *)values[i]));
	XtAddCallback(tmp, XmNactivateCallback, prefs_change_callback, NULL);
      }

  prf->error = make_row_error(prf, box, prf->arrow_right, top_widget);
  help = make_row_help(prf, varname, box, top_widget, prf->error);
  prf->text_func = text_func;
  XtAddCallback(prf->text, XmNactivateCallback, call_text_func, (XtPointer)prf);
  XtAddCallback(prf->text, XmNactivateCallback, prefs_change_callback, NULL);
  XtAddCallback(prf->arrow_right, XmNactivateCallback, prefs_change_callback, NULL);

  prf->list_func = list_func;

  XtAddEventHandler(prf->text, EnterWindowMask, false, mouse_enter_pref_callback, (XtPointer)prf);
  XtAddEventHandler(prf->arrow_right, EnterWindowMask, false, mouse_enter_pref_callback, (XtPointer)prf);
  XtAddEventHandler(prf->text, LeaveWindowMask, false, mouse_leave_pref_callback, (XtPointer)prf);
  XtAddEventHandler(prf->arrow_right, LeaveWindowMask, false, mouse_leave_pref_callback, (XtPointer)prf);

  return(prf);
}


/* ---------------- color selector row(s) ---------------- */

static XColor *rgb_to_color_1(Float r, Float g, Float b)
{
  Display *dpy;
  XColor *new_color;
  new_color = (XColor *)CALLOC(1, sizeof(XColor));
  new_color->flags = DoRed | DoGreen | DoBlue;
  new_color->red = (unsigned short)(65535 * r);
  new_color->green = (unsigned short)(65535 * g);
  new_color->blue = (unsigned short)(65535 * b);
  dpy = MAIN_DISPLAY(ss);
  XAllocColor(dpy, DefaultColormap(dpy, DefaultScreen(dpy)), new_color);
  return(new_color);
}

static color_t rgb_to_color(Float r, Float g, Float b)
{
  color_t temp;
  XColor *color;
  color = rgb_to_color_1(r, g, b);
  temp = color->pixel;
  FREE(color);
  return(temp);
}

static void pixel_to_rgb(Pixel pix, float *r, float *g, float *b)
{
  XColor tmp_color;
  Display *dpy;
  dpy = XtDisplay(MAIN_SHELL(ss));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  tmp_color.pixel = pix;
  XQueryColor(dpy, DefaultColormap(dpy, DefaultScreen(dpy)), &tmp_color);
  (*r) = (float)tmp_color.red / 65535.0;
  (*g) = (float)tmp_color.green / 65535.0;
  (*b) = (float)tmp_color.blue / 65535.0;
}

static void reflect_color(prefs_info *prf)
{
  int ir = 0, ig = 0, ib = 0;
  Float r, g, b;
  XColor *current_color;
  Pixel pixel;

  XmScaleGetValue(prf->rscl, &ir);
  XmScaleGetValue(prf->gscl, &ig);
  XmScaleGetValue(prf->bscl, &ib);

  current_color = rgb_to_color_1(ir / 100.0, ig / 100.0, ib / 100.0);
  r = current_color->red / 65535.0;
  g = current_color->green / 65535.0;
  b = current_color->blue / 65535.0;

  pixel = current_color->pixel;
  FREE(current_color);
  current_color = NULL;

  XtVaSetValues(prf->color, XmNbackground, pixel, NULL);
  float_to_textfield(prf->rtxt, r);
  float_to_textfield(prf->gtxt, g);
  float_to_textfield(prf->btxt, b);
}

static void prefs_color_callback(Widget w, XtPointer context, XtPointer info)
{
  reflect_color((prefs_info *)context);
}

static void unpost_color_error(XtPointer data, XtIntervalId *id)
{
  prefs_info *prf = (prefs_info *)data;
  reflect_color(prf);
  prf->got_error = false;
  black_text(prf);
  set_label(prf->label, prf->saved_label);
}

static void errors_to_color_text(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  prf->got_error = true;
  red_text(prf);
  set_label(prf->label, msg);
  XtAppAddTimeOut(MAIN_APP(ss),
		  ERROR_WAIT_TIME,
		  (XtTimerCallbackProc)unpost_color_error,
		  data);
}

static void prefs_r_callback(Widget w, XtPointer context, XtPointer info)
{
  prefs_info *prf = (prefs_info *)context;
  char *str;
  float r = 0.0;
  str = XmTextFieldGetString(w);
  redirect_errors_to(errors_to_color_text, (void *)prf);
  r = (float)string_to_Float(str, 0.0, "red amount");
  redirect_errors_to(NULL, NULL);
  XmScaleSetValue(prf->rscl, mus_iclamp(0, (int)(100 * r), 100));
  if (!(prf->got_error)) reflect_color(prf);
  if (str) XtFree(str);
}

static void prefs_g_callback(Widget w, XtPointer context, XtPointer info)
{
  prefs_info *prf = (prefs_info *)context;
  char *str;
  float r = 0.0;
  str = XmTextFieldGetString(w);
  redirect_errors_to(errors_to_color_text, (void *)prf);
  r = (float)string_to_Float(str, 0.0, "green amount");
  redirect_errors_to(NULL, NULL);
  XmScaleSetValue(prf->gscl, mus_iclamp(0, (int)(100 * r), 100));
  if (!(prf->got_error)) reflect_color(prf);
  if (str) XtFree(str);
}

static void prefs_b_callback(Widget w, XtPointer context, XtPointer info)
{
  prefs_info *prf = (prefs_info *)context;
  char *str;
  float r = 0.0;
  str = XmTextFieldGetString(w);
  redirect_errors_to(errors_to_color_text, (void *)prf);
  r = (float)string_to_Float(str, 0.0, "blue amount");
  redirect_errors_to(NULL, NULL);
  XmScaleSetValue(prf->bscl, mus_iclamp(0, (int)(100 * r), 100));
  if (!(prf->got_error)) reflect_color(prf);
  if (str) XtFree(str);
}

static void prefs_call_color_func_callback(Widget w, XtPointer context, XtPointer info)
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->color_func))
    {
      int ir = 0, ig = 0, ib = 0;

      XmScaleGetValue(prf->rscl, &ir);
      XmScaleGetValue(prf->gscl, &ig);
      XmScaleGetValue(prf->bscl, &ib);

      (*(prf->color_func))(prf, (float)ir / 100.0, (float)ig / 100.0, (float)ib / 100.0);
    }
}

static void scale_set_color(prefs_info *prf, color_t pixel)
{
  float r = 0.0, g = 0.0, b = 0.0;
  pixel_to_rgb(pixel, &r, &g, &b);
  float_to_textfield(prf->rtxt, r);
  XmScaleSetValue(prf->rscl, (int)(100 * r));
  float_to_textfield(prf->gtxt, g);
  XmScaleSetValue(prf->gscl, (int)(100 * g));
  float_to_textfield(prf->btxt, b);
  XmScaleSetValue(prf->bscl, (int)(100 * b));
  XtVaSetValues(prf->color, XmNbackground, pixel, NULL);
}

static Pixel red, green, blue;

static prefs_info *prefs_color_selector_row(const char *label, const char *varname, 
					    Pixel current_pixel,
					    Widget box, Widget top_widget,
					    void (*color_func)(prefs_info *prf, float r, float g, float b))
{
  Arg args[20];
  int n;
  prefs_info *prf = NULL;
  Widget sep, sep1, frame, help;
  XtCallbackList n1;
  float r = 0.0, g = 0.0, b = 0.0;

  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  pixel_to_rgb(current_pixel, &r, &g, &b);

  prf->label = make_row_label(prf, label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);    

  n = 0;
  XtSetArg(args[n], XmNbackground, current_pixel); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
  XtSetArg(args[n], XmNbottomWidget, sep); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, sep); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
  XtSetArg(args[n], XmNrightPosition, COLOR_POSITION); n++;
  XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
  frame = XtCreateManagedWidget("frame", xmFrameWidgetClass, box, args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, current_pixel); n++;
  prf->color = XtCreateManagedWidget("", xmLabelWidgetClass, frame, args, n);

  sep1 = make_row_inner_separator(8, prf->color, box, top_widget);
  
  prf->rtxt = make_row_text(prf, NULL, 6, sep1, box, top_widget);
  float_to_textfield(prf->rtxt, r);

  prf->gtxt = make_row_text(prf, NULL, 6, prf->rtxt, box, top_widget);
  float_to_textfield(prf->gtxt, g);

  prf->btxt = make_row_text(prf, NULL, 6, prf->gtxt, box, top_widget);
  float_to_textfield(prf->btxt, b);

  help = make_row_help(prf, varname, box, top_widget, prf->btxt);

  /* 2nd row = 3 scales */
  n1 = make_callback_list(prefs_color_callback, (XtPointer)prf);
  
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, prf->label); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  if (FIRST_COLOR_POSITION == 0)
    {
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    }
  else
    {
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, FIRST_COLOR_POSITION); n++;
    }
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
  XtSetArg(args[n], XmNrightPosition, SECOND_COLOR_POSITION); n++;
  XtSetArg(args[n], XmNmarginHeight, 0); n++;
  XtSetArg(args[n], XmNborderWidth, 1); n++;
  XtSetArg(args[n], XmNborderColor, red); n++;
  XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
  XtSetArg(args[n], XmNshowValue, XmNONE); n++;
  XtSetArg(args[n], XmNvalue, (int)(100 * r)); n++;
  XtSetArg(args[n], XmNdragCallback, n1); n++;
  XtSetArg(args[n], XmNvalueChangedCallback, n1); n++;
  prf->rscl = XtCreateManagedWidget("", xmScaleWidgetClass, box, args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, prf->label); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, prf->rscl); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
  XtSetArg(args[n], XmNrightPosition, THIRD_COLOR_POSITION); n++;
  XtSetArg(args[n], XmNmarginHeight, 0); n++;
  XtSetArg(args[n], XmNborderWidth, 1); n++;
  XtSetArg(args[n], XmNborderColor, green); n++;
  XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
  XtSetArg(args[n], XmNshowValue, XmNONE); n++;
  XtSetArg(args[n], XmNvalue, (int)(100 * g)); n++;
  XtSetArg(args[n], XmNdragCallback, n1); n++;
  XtSetArg(args[n], XmNvalueChangedCallback, n1); n++;
  prf->gscl = XtCreateManagedWidget("", xmScaleWidgetClass, box, args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, prf->label); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, prf->gscl); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
  XtSetArg(args[n], XmNrightPosition, 80); n++;
  XtSetArg(args[n], XmNmarginHeight, 0); n++;
  XtSetArg(args[n], XmNborderWidth, 1); n++;
  XtSetArg(args[n], XmNborderColor, blue); n++;
  XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
  XtSetArg(args[n], XmNshowValue, XmNONE); n++;
  XtSetArg(args[n], XmNvalue, (int)(100 * b)); n++;
  XtSetArg(args[n], XmNdragCallback, n1); n++;
  XtSetArg(args[n], XmNvalueChangedCallback, n1); n++;
  prf->bscl = XtCreateManagedWidget("", xmScaleWidgetClass, box, args, n);

  XtAddCallback(prf->rtxt, XmNactivateCallback, prefs_r_callback, (XtPointer)prf);
  XtAddCallback(prf->gtxt, XmNactivateCallback, prefs_g_callback, (XtPointer)prf);
  XtAddCallback(prf->btxt, XmNactivateCallback, prefs_b_callback, (XtPointer)prf);

  XtAddCallback(prf->rscl, XmNvalueChangedCallback, prefs_call_color_func_callback, (XtPointer)prf);
  XtAddCallback(prf->gscl, XmNvalueChangedCallback, prefs_call_color_func_callback, (XtPointer)prf);
  XtAddCallback(prf->bscl, XmNvalueChangedCallback, prefs_call_color_func_callback, (XtPointer)prf);

  XtAddCallback(prf->rscl, XmNvalueChangedCallback, prefs_change_callback, NULL);
  XtAddCallback(prf->gscl, XmNvalueChangedCallback, prefs_change_callback, NULL);
  XtAddCallback(prf->bscl, XmNvalueChangedCallback, prefs_change_callback, NULL);

  XtAddEventHandler(prf->color, EnterWindowMask, false, mouse_enter_pref_callback, (XtPointer)prf);
  XtAddEventHandler(prf->color, LeaveWindowMask, false, mouse_leave_pref_callback, (XtPointer)prf);
  XtAddEventHandler(prf->rscl, EnterWindowMask, false, mouse_enter_pref_callback, (XtPointer)prf);
  XtAddEventHandler(prf->rscl, LeaveWindowMask, false, mouse_leave_pref_callback, (XtPointer)prf);
  XtAddEventHandler(prf->gscl, EnterWindowMask, false, mouse_enter_pref_callback, (XtPointer)prf);
  XtAddEventHandler(prf->gscl, LeaveWindowMask, false, mouse_leave_pref_callback, (XtPointer)prf);
  XtAddEventHandler(prf->bscl, EnterWindowMask, false, mouse_enter_pref_callback, (XtPointer)prf);
  XtAddEventHandler(prf->bscl, LeaveWindowMask, false, mouse_leave_pref_callback, (XtPointer)prf);

  prf->color_func = color_func;
  FREE(n1);
  return(prf);
}


/* ---------------- topic separator ---------------- */

static Widget make_inter_topic_separator(Widget topics)
{
  int n;
  Arg args[20];
  n = 0;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
  XtSetArg(args[n], XmNheight, INTER_TOPIC_SPACE); n++;
  XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
  return(XtCreateManagedWidget("sep", xmSeparatorWidgetClass, topics, args, n));
}

/* ---------------- variable separator ---------------- */

static Widget make_inter_variable_separator(Widget topics, Widget top_widget)
{
  int n;
  Arg args[20];
  n = 0;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
  XtSetArg(args[n], XmNheight, INTER_VARIABLE_SPACE); n++;
  XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
  return(XtCreateManagedWidget("sep", xmSeparatorWidgetClass, topics, args, n));
}

/* ---------------- top-level contents label ---------------- */

static Widget make_top_level_label(const char *label, Widget parent)
{
  int n;
  Arg args[20];
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->light_blue); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
  return(XtCreateManagedWidget(label, xmLabelWidgetClass, parent, args, n));
}

static Widget make_top_level_box(Widget topics)
{
  Widget frame;
  int n;
  Arg args[20];
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  frame = XtCreateManagedWidget("pref-frame", xmFrameWidgetClass, topics, args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  return(XtCreateManagedWidget("pref-box", xmFormWidgetClass, frame, args, n));
}

static Widget make_inner_label(const char *label, Widget parent, Widget top_widget)
{
  int n;
  Arg args[20];
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
  return(XtCreateManagedWidget(label, xmLabelWidgetClass, parent, args, n));
}


/* ---------------- base buttons ---------------- */

static void wm_delete_callback(Widget w, XtPointer context, XtPointer info) 
{
  clear_prefs_dialog_error();
  prefs_helping = false;
}

static void preferences_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  prefs_helping = true;
  snd_help("preferences",
	   "This dialog sets various global variables. 'Save' then writes the new values \
to ~/.snd_prefs_guile|ruby|forth|gauche so that they take effect the next time you start Snd.  'Revert' resets each variable either to \
its value when the Preferences dialog was started, or to the last saved value.  'Clear' resets each variable to its default value (its \
value when Snd starts, before loading initialization files). 'Help' starts this dialog, and as long as it's active, it will post helpful \
information if the mouse lingers over some variable -- sort of a tooltip that stays out of your way. \
You can also request help on a given topic by clicking the variable name on the far right.",
	   WITH_WORD_WRAP);
}

static void preferences_quit_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* if helping, should we unmanage the help dialog as well? */
  prefs_helping = false;
  clear_prefs_dialog_error();
  if (XmGetFocusWidget(preferences_dialog) == XmMessageBoxGetChild(preferences_dialog, XmDIALOG_OK_BUTTON))
    XtUnmanageChild(preferences_dialog);
}

static void prefs_set_dialog_title(const char *filename)
{
  XmString title;
  char *str;
  if (filename)
    {
      if (prefs_saved_filename) FREE(prefs_saved_filename);
      prefs_saved_filename = copy_string(filename);
    }
  if (prefs_saved_filename)
    str = mus_format("Preferences%s (saved in %s)\n",
		     (prefs_unsaved) ? "*" : "",
		     prefs_saved_filename);
  else str = mus_format("Preferences%s",
			(prefs_unsaved) ? "*" : "");
  title = XmStringCreateLocalized(str);
  FREE(str);
  XtVaSetValues(preferences_dialog, 
		XmNdialogTitle, title, 
		NULL);
  XmStringFree(title);
}

static void preferences_revert_callback(Widget w, XtPointer context, XtPointer info) 
{
  preferences_revert_or_clear(true);
}

static void preferences_clear_callback(Widget w, XtPointer context, XtPointer info) 
{
  preferences_revert_or_clear(false);
}

static void preferences_save_callback(Widget w, XtPointer context, XtPointer info) 
{
  clear_prefs_dialog_error();
  redirect_snd_error_to(post_prefs_dialog_error, NULL);
  redirect_snd_warning_to(post_prefs_dialog_error, NULL);
  save_prefs(save_options_in_prefs());
  redirect_snd_error_to(NULL, NULL);
  redirect_snd_warning_to(NULL, NULL);
}


/* ---------------- errors ---------------- */

static void clear_prefs_error(Widget w, XtPointer context, XtPointer info) 
{
  prefs_info *prf = (prefs_info *)context;
  ASSERT_WIDGET_TYPE(XmIsLabel(prf->error), prf->error);
  XtRemoveCallback(prf->text, XmNvalueChangedCallback, clear_prefs_error, context);
  set_label(prf->error, "");
}

static void post_prefs_error(const char *msg, prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsLabel(prf->error), prf->error);
  prf->got_error = true;
  set_label(prf->error, msg);
  XtAddCallback(prf->text, XmNvalueChangedCallback, clear_prefs_error, (XtPointer)prf);
}

static void va_post_prefs_error(const char *msg, prefs_info *data, ...)
{
  char *buf;
  va_list ap;
  va_start(ap, data);
  buf = vstr(msg, ap);
  va_end(ap);
  post_prefs_error(buf, data);
  FREE(buf);
}


/* ---------------- preferences dialog ---------------- */

widget_t start_preferences_dialog(void)
{
  Arg args[20];
  int n;
  Widget scroller, topics, current_sep;
  char *str;
  prefs_info *prf;

  if (preferences_dialog) 
    {
      /* I don't think this should reflect current state except when it is created */
      if (!(XtIsManaged(preferences_dialog)))
	XtManageChild(preferences_dialog);
      else raise_dialog(preferences_dialog);
      return(preferences_dialog);
    }

  /* -------- base buttons -------- */
  {
    XmString title, help, revert, clear, save, dismiss;
    Widget revert_button, clear_button;

    title = XmStringCreateLocalized(_("Preferences"));
    help = XmStringCreateLocalized(_("Help"));
    revert = XmStringCreateLocalized(_("Revert"));
    clear = XmStringCreateLocalized(_("Clear"));
    save = XmStringCreateLocalized(_("Save"));
    dismiss = XmStringCreateLocalized(_("Dismiss"));

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
    XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
    XtSetArg(args[n], XmNnoResize, false); n++;
    XtSetArg(args[n], XmNtransient, false); n++;
    XtSetArg(args[n], XmNcancelLabelString, save); n++;
    XtSetArg(args[n], XmNhelpLabelString, help); n++;
    XtSetArg(args[n], XmNokLabelString, dismiss); n++;
    XtSetArg(args[n], XmNdialogTitle, title); n++;
    XtSetArg(args[n], XmNallowShellResize, true); n++;
    XtSetArg(args[n], XmNautoUnmanage, false); n++;
    {
      Dimension width, height;
      width = XDisplayWidth(MAIN_DISPLAY(ss), DefaultScreen(MAIN_DISPLAY(ss)));
      height = XDisplayHeight(MAIN_DISPLAY(ss), DefaultScreen(MAIN_DISPLAY(ss)));
      XtSetArg(args[n], XmNwidth, (STARTUP_WIDTH < width) ? STARTUP_WIDTH : ((Dimension)(width - 50))); n++;
      XtSetArg(args[n], XmNheight, (STARTUP_HEIGHT < height) ? STARTUP_HEIGHT : ((Dimension)(height - 50))); n++;
    }
    preferences_dialog = XmCreateTemplateDialog(MAIN_PANE(ss), "preferences", args, n);

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->reset_button_color); n++;
    XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;
    revert_button = XtCreateManagedWidget(_("Revert"), xmPushButtonGadgetClass, preferences_dialog, args, n);

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->reset_button_color); n++;
    XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;
    clear_button = XtCreateManagedWidget(_("Clear"), xmPushButtonGadgetClass, preferences_dialog, args, n);

    XtAddCallback(preferences_dialog, XmNcancelCallback, preferences_save_callback, NULL);
    XtAddCallback(preferences_dialog, XmNhelpCallback, preferences_help_callback, NULL);
    XtAddCallback(preferences_dialog, XmNokCallback, preferences_quit_callback, NULL);
    XtAddCallback(revert_button, XmNactivateCallback, preferences_revert_callback, NULL);
    XtAddCallback(clear_button, XmNactivateCallback, preferences_clear_callback, NULL);
    
    XmStringFree(title);
    XmStringFree(help);
    XmStringFree(save);
    XmStringFree(dismiss);
    XmStringFree(revert);
    XmStringFree(clear);
    
    map_over_children(preferences_dialog, set_main_color_of_widget);
    XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->sgx->pushed_button_color, NULL);
    XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->sgx->pushed_button_color, NULL);
    XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->sgx->pushed_button_color, NULL);
    XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->doit_button_color,   NULL);
    XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_OK_BUTTON),     XmNbackground, ss->sgx->quit_button_color,   NULL);
    XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->sgx->help_button_color,   NULL);
    
    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNbottomWidget, XmMessageBoxGetChild(preferences_dialog, XmDIALOG_SEPARATOR)); n++;
    XtSetArg(args[n], XmNscrollingPolicy, XmAUTOMATIC); n++;
    XtSetArg(args[n], XmNscrollBarDisplayPolicy, XmSTATIC); n++;
    scroller = XmCreateScrolledWindow(preferences_dialog, "pref-scroller", args, n);
    XtManageChild(scroller);
    
    XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
    n = attach_all_sides(args, 0);
    XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
    topics = XtCreateManagedWidget("pref-topics", xmRowColumnWidgetClass, scroller, args, n);
    XtVaSetValues(scroller,
		  XmNworkWindow, topics, 
		  NULL);
  }

  red = rgb_to_color(1.0, 0.0, 0.0);
  green = rgb_to_color(0.0, 1.0, 0.0);
  blue = rgb_to_color(0.0, 0.0, 1.0);


  /* ---------------- overall behavior ---------------- */

  {
    Widget dpy_box, dpy_label, file_label, cursor_label, key_label;
    char *str1, *str2;

    /* ---------------- overall behavior ----------------*/

    dpy_box = make_top_level_box(topics);
    dpy_label = make_top_level_label("overall behavior choices", dpy_box);

    current_sep = dpy_label;
    str1 = mus_format("%d", ss->init_window_width);
    str2 = mus_format("%d", ss->init_window_height);
    rts_init_window_width = ss->init_window_width;
    rts_init_window_height = ss->init_window_height;
    prf = prefs_row_with_two_texts("start up size", S_window_width, 
				   "width:", str1, "height:", str2, 6,
				   dpy_box, current_sep,
				   startup_size_text);
    /* this is not reflected */
    remember_pref(prf, NULL, save_init_window_size, NULL, clear_init_window_size, revert_init_window_size); 
    FREE(str2);
    FREE(str1);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("ask before overwriting anything", S_ask_before_overwrite,
				rts_ask_before_overwrite = ask_before_overwrite(ss), 
				dpy_box, current_sep,
				overwrite_toggle);
    remember_pref(prf, reflect_ask_before_overwrite, save_ask_before_overwrite, NULL, NULL, revert_ask_before_overwrite);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("ask about unsaved edits before exiting", "check-for-unsaved-edits",
				rts_unsaved_edits = unsaved_edits(), 
				dpy_box, current_sep,
				unsaved_edits_toggle);
    remember_pref(prf, reflect_unsaved_edits, save_unsaved_edits, help_unsaved_edits, clear_unsaved_edits, revert_unsaved_edits);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("include thumbnail graph in upper right corner", "make-current-window-display",
				rts_current_window_display = current_window_display(),
				dpy_box, current_sep,
				current_window_display_toggle);
    remember_pref(prf, reflect_current_window_display, save_current_window_display, help_current_window, 
		  clear_current_window_display, revert_current_window_display);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("resize main window as sounds open and close", S_auto_resize,
				rts_auto_resize = auto_resize(ss), 
				dpy_box, current_sep, 
				resize_toggle);
    remember_pref(prf, reflect_auto_resize, save_auto_resize, NULL, NULL, revert_auto_resize);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("focus follows mouse", "focus-follows-mouse",
				rts_focus_follows_mouse = focus_follows_mouse(),
				dpy_box, current_sep,
				focus_follows_mouse_toggle);
    remember_pref(prf, reflect_focus_follows_mouse, save_focus_follows_mouse, help_focus_follows_mouse, clear_focus_follows_mouse, revert_focus_follows_mouse);
    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    rts_sync_choice = sync_choice();
    prf = prefs_row_with_two_toggles("operate on all channels together", S_sync,
				     "within each sound", rts_sync_choice == SYNC_WITHIN_EACH_SOUND,
				     "across all sounds", rts_sync_choice == SYNC_ACROSS_ALL_SOUNDS,
				     dpy_box, current_sep, 
				     sync1_choice, sync2_choice);
    remember_pref(prf, reflect_sync_choice, save_sync_choice, help_sync_choice, clear_sync_choice, revert_sync_choice);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    rts_remember_sound_state_choice = find_remember_sound_state_choice();
    prf = prefs_row_with_two_toggles("restore a sound's state if reopened later", "remember-sound-state",
				     "within one run", rts_remember_sound_state_choice & 1,
				     "across runs", rts_remember_sound_state_choice & 2,
				     dpy_box, current_sep, 
				     remember_sound_state_1_choice, remember_sound_state_2_choice);
    remember_pref(prf, reflect_remember_sound_state_choice, save_remember_sound_state_choice, help_remember_sound_state_choice, 
		  clear_remember_sound_state, revert_remember_sound_state);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("show the control panel upon opening a sound", S_show_controls,
				rts_show_controls = in_show_controls(ss), 
				dpy_box, current_sep, 
				controls_toggle);
    remember_pref(prf, reflect_show_controls, save_show_controls, NULL, NULL, revert_show_controls);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    include_peak_env_directory = copy_string(peak_env_directory());
    rts_peak_env_directory = copy_string(include_peak_env_directory);
    include_peak_envs = find_peak_envs();
    rts_peak_envs = include_peak_envs;
    prf = prefs_row_with_toggle_with_text("save peak envs to speed up initial display", "save-peak-env-info",
					  include_peak_envs,
					  "directory:", include_peak_env_directory, 25,
					  dpy_box, current_sep,
					  peak_envs_toggle, peak_envs_text);
    remember_pref(prf, reflect_peak_envs, save_peak_envs, help_peak_envs, clear_peak_envs, revert_peak_envs);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    str = mus_format("%d", rts_max_regions = max_regions(ss));
    prf = prefs_row_with_toggle_with_text("selection creates an associated region", S_selection_creates_region,
					  rts_selection_creates_region = selection_creates_region(ss),
					  "max regions:", str, 5,
					  dpy_box, current_sep,
					  selection_creates_region_toggle, max_regions_text);
    remember_pref(prf, reflect_selection_creates_region, save_selection_creates_region, NULL, NULL, revert_selection_creates_region);
    FREE(str);

    /* ---------------- file options ---------------- */

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    file_label = make_inner_label("  file options", dpy_box, current_sep);

    rts_load_path = find_sources();
    prf = prefs_row_with_text("directory containing Snd's " XEN_LANGUAGE_NAME " files", "load path", 
			      rts_load_path,
			      dpy_box, file_label,
			      load_path_text);
    remember_pref(prf, reflect_load_path, NULL, help_load_path, clear_load_path, revert_load_path);
    if (rts_load_path == NULL) red_text(prf);
    load_path_text_widget = prf->text;

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("display only sound files in various file lists", S_just_sounds,
				rts_just_sounds = just_sounds(ss), 
				dpy_box, current_sep, 
				just_sounds_toggle);
    remember_pref(prf, prefs_reflect_just_sounds, save_just_sounds, NULL, NULL, revert_just_sounds);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    rts_temp_dir = copy_string(temp_dir(ss));
    prf = prefs_row_with_text("directory for temporary files", S_temp_dir, 
			      temp_dir(ss), 
			      dpy_box, current_sep,
			      temp_dir_text);
    remember_pref(prf, reflect_temp_dir, save_temp_dir, NULL, NULL, revert_temp_dir);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    rts_save_dir = copy_string(save_dir(ss));
    prf = prefs_row_with_text("directory for save-state files", S_save_dir, 
			      save_dir(ss), 
			      dpy_box, current_sep,
			      save_dir_text);
    remember_pref(prf, reflect_save_dir, save_save_dir, NULL, NULL, revert_save_dir);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    rts_save_state_file = copy_string(save_state_file(ss));
    prf = prefs_row_with_text("default save-state filename", S_save_state_file, 
			      save_state_file(ss), 
			      dpy_box, current_sep,
			      save_state_file_text);
    remember_pref(prf, reflect_save_state_file, save_save_state_file, NULL, NULL, revert_save_state_file);

#if HAVE_LADSPA
    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    rts_ladspa_dir = copy_string(ladspa_dir(ss));
    prf = prefs_row_with_text("directory for ladspa plugins", S_ladspa_dir, 
			      ladspa_dir(ss), 
			      dpy_box, current_sep,
			      ladspa_dir_text);
    remember_pref(prf, reflect_ladspa_dir, save_ladspa_dir, NULL, NULL, revert_ladspa_dir);
#endif

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    rts_vf_directory = copy_string(view_files_find_any_directory());
    prf = prefs_row_with_text("directory for view-files dialog", S_add_directory_to_view_files_list,
			      rts_vf_directory,
			      dpy_box, current_sep,
			      view_files_directory_text);
    remember_pref(prf, reflect_view_files_directory, save_view_files_directory, NULL, NULL, revert_view_files_directory);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    rts_html_program = copy_string(html_program(ss));
    prf = prefs_row_with_text("external program to read HTML files via snd-help", S_html_program,
			      html_program(ss),
			      dpy_box, current_sep,
			      html_program_text);
    remember_pref(prf, reflect_html_program, save_html_program, NULL, NULL, revert_html_program);
    current_sep = make_inter_variable_separator(dpy_box, prf->label);

    rts_default_output_chans = default_output_chans(ss);
    prf = prefs_row_with_radio_box("default new sound attributes: chans", S_default_output_chans,
				   output_chan_choices, NUM_OUTPUT_CHAN_CHOICES, -1,
				   dpy_box, current_sep,
				   default_output_chans_choice);
    remember_pref(prf, reflect_default_output_chans, save_default_output_chans, NULL, NULL, revert_default_output_chans);
    reflect_default_output_chans(prf);

    rts_default_output_srate = default_output_srate(ss);
    prf = prefs_row_with_radio_box("srate", S_default_output_srate,
				   output_srate_choices, NUM_OUTPUT_SRATE_CHOICES, -1,
				   dpy_box, prf->label,
				   default_output_srate_choice);
    remember_pref(prf, reflect_default_output_srate, save_default_output_srate, NULL, NULL, revert_default_output_srate);
    reflect_default_output_srate(prf);

    rts_default_output_header_type = default_output_header_type(ss);
    prf = prefs_row_with_radio_box("header type", S_default_output_header_type,
				   output_type_choices, NUM_OUTPUT_TYPE_CHOICES, -1,
				   dpy_box, prf->label,
				   default_output_header_type_choice);
    output_header_type_prf = prf;
    remember_pref(prf, reflect_default_output_header_type, save_default_output_header_type, NULL, NULL, revert_default_output_header_type);

    rts_default_output_data_format = default_output_data_format(ss);
    prf = prefs_row_with_radio_box("data format", S_default_output_data_format,
				   output_format_choices, NUM_OUTPUT_FORMAT_CHOICES, -1,
				   dpy_box, prf->label,
				   default_output_data_format_choice);
    output_data_format_prf = prf;
    remember_pref(prf, reflect_default_output_data_format, save_default_output_data_format, NULL, NULL, revert_default_output_data_format);
    reflect_default_output_header_type(output_header_type_prf);
    reflect_default_output_data_format(output_data_format_prf);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    {
      int i, srate = 0, chans = 0, format = 0;
      mus_header_raw_defaults(&srate, &chans, &format);
      rts_raw_chans = chans;
      rts_raw_srate = srate;
      rts_raw_data_format = format;
      str = mus_format("%d", chans);
      str1 = mus_format("%d", srate);
      raw_data_format_choices = (char **)CALLOC(MUS_NUM_DATA_FORMATS - 1, sizeof(char *));
      for (i = 1; i < MUS_NUM_DATA_FORMATS; i++)
	raw_data_format_choices[i - 1] = raw_data_format_to_string(i); /* skip MUS_UNKNOWN */
      prf = prefs_row_with_text("default raw sound attributes: chans", S_mus_header_raw_defaults, str,
				dpy_box, current_sep,
				raw_chans_choice);
      remember_pref(prf, reflect_raw_chans, save_raw_chans, NULL, NULL, revert_raw_chans);

      prf = prefs_row_with_text("srate", S_mus_header_raw_defaults, str1,
				dpy_box, prf->label,
				raw_srate_choice);
      remember_pref(prf, reflect_raw_srate, save_raw_srate, NULL, NULL, revert_raw_srate);

      prf = prefs_row_with_list("data format", S_mus_header_raw_defaults, raw_data_format_choices[format - 1],
				(const char **)raw_data_format_choices, MUS_NUM_DATA_FORMATS - 1,
				dpy_box, prf->label,
				raw_data_format_from_text,
				NULL, NULL,
				raw_data_format_from_menu);
      remember_pref(prf, reflect_raw_data_format, save_raw_data_format, NULL, NULL, revert_raw_data_format);
      FREE(str);
      FREE(str1);
    }
    current_sep = make_inter_variable_separator(dpy_box, prf->label);

  /* ---------------- extra menus ---------------- */

#if HAVE_STATIC_XM
    cursor_label = make_inner_label("  extra menus", dpy_box, current_sep);
#else
    cursor_label = make_inner_label("  extra menus (these will need the xm module)", dpy_box, current_sep);
#endif

    prf = prefs_row_with_toggle("context-sensitive popup menu", "add-selection-popup",
				(include_context_sensitive_popup = find_context_sensitive_popup()),
				dpy_box, cursor_label, 
				context_sensitive_popup_toggle);
    remember_pref(prf, reflect_context_sensitive_popup, save_context_sensitive_popup, help_context_sensitive_popup, 
		  clear_context_sensitive_popup, revert_context_sensitive_popup);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("effects menu", 
#if HAVE_SCHEME
				"new-effects.scm",
#else
				"effects." XEN_FILE_EXTENSION,
#endif
				(include_effects_menu = find_effects_menu()),
				dpy_box, current_sep, 
				effects_menu_toggle);
    remember_pref(prf, reflect_effects_menu, save_effects_menu, help_effects_menu, clear_effects_menu, revert_effects_menu);

#if HAVE_SCHEME
    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("edit menu additions", "edit-menu.scm",
				(include_edit_menu = find_edit_menu()),
				dpy_box, current_sep, 
				edit_menu_toggle);
    remember_pref(prf, reflect_edit_menu, save_edit_menu, help_edit_menu, clear_edit_menu, revert_edit_menu);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("marks menu", "marks-menu.scm",
				(include_marks_menu = find_marks_menu()),
				dpy_box, current_sep, 
				marks_menu_toggle);
    remember_pref(prf, reflect_marks_menu, save_marks_menu, help_marks_menu, clear_marks_menu, revert_marks_menu);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("mix/track menu", "mix-menu.scm",
				(include_mix_menu = find_mix_menu()),
				dpy_box, current_sep, 
				mix_menu_toggle);
    remember_pref(prf, reflect_mix_menu, save_mix_menu, help_mix_menu, clear_mix_menu, revert_mix_menu);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("a toolbar", "toolbar.scm",
				(include_icon_box = find_icon_box()),
				dpy_box, current_sep, 
				icon_box_toggle);
    remember_pref(prf, reflect_icon_box, save_icon_box, help_icon_box, clear_icon_box, revert_icon_box);
#endif

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("reopen menu", "with-reopen-menu",
				(include_reopen_menu = find_reopen_menu()),
				dpy_box, current_sep, 
				reopen_menu_toggle);
    remember_pref(prf, reflect_reopen_menu, save_reopen_menu, help_reopen_menu, clear_reopen_menu, revert_reopen_menu);


    /* ---------------- additional key bindings ---------------- */

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    key_label = make_inner_label("  additional key bindings", dpy_box, current_sep);

    {
      key_info *ki;

      ki = find_prefs_key_binding("play-from-cursor");
      prf = prefs_row_with_text_and_three_toggles("play all chans from cursor", S_play, 
						  "key:", 8, "ctrl:", "meta:",  "C-x:",
						  ki->key, ki->c, ki->m, ki->x,
						  dpy_box, key_label,
						  bind_play_from_cursor);
      remember_pref(prf, reflect_play_from_cursor, save_pfc_binding, help_play_from_cursor, clear_play_from_cursor, NULL);
      FREE(ki);

      current_sep = make_inter_variable_separator(dpy_box, prf->label);
      ki = find_prefs_key_binding("show-all");
      prf = prefs_row_with_text_and_three_toggles("show entire sound", S_x_bounds, 
						  "key:", 8, "ctrl:", "meta:",  "C-x:",
						  ki->key, ki->c, ki->m, ki->x,
						  dpy_box, current_sep,
						  bind_show_all);
      remember_pref(prf, reflect_show_all, save_show_all_binding, help_show_all, clear_show_all, NULL);
      FREE(ki);

      current_sep = make_inter_variable_separator(dpy_box, prf->label);
      ki = find_prefs_key_binding("select-all");
      prf = prefs_row_with_text_and_three_toggles("select entire sound", S_select_all, 
						  "key:", 8, "ctrl:", "meta:",  "C-x:",
						  ki->key, ki->c, ki->m, ki->x,
						  dpy_box, current_sep,
						  bind_select_all);
      remember_pref(prf, reflect_select_all, save_select_all_binding, help_select_all, clear_select_all, NULL);
      FREE(ki);

      current_sep = make_inter_variable_separator(dpy_box, prf->label);
      ki = find_prefs_key_binding("show-selection");
      prf = prefs_row_with_text_and_three_toggles("show current selection", "show-selection", 
						  "key:", 8, "ctrl:", "meta:",  "C-x:",
						  ki->key, ki->c, ki->m, ki->x,
						  dpy_box, current_sep,
						  bind_show_selection);
      remember_pref(prf, reflect_show_selection, save_show_selection_binding, help_show_selection, clear_show_selection, NULL);
      FREE(ki);

      current_sep = make_inter_variable_separator(dpy_box, prf->label);
      ki = find_prefs_key_binding("revert-sound");
      prf = prefs_row_with_text_and_three_toggles("undo all edits (revert)", S_revert_sound, 
						  "key:", 8, "ctrl:", "meta:",  "C-x:",
						  ki->key, ki->c, ki->m, ki->x,
						  dpy_box, current_sep,
						  bind_revert);
      remember_pref(prf, reflect_revert, save_revert_binding, help_revert, clear_revert_sound, NULL);
      FREE(ki);

      current_sep = make_inter_variable_separator(dpy_box, prf->label);
      ki = find_prefs_key_binding("exit");
      prf = prefs_row_with_text_and_three_toggles("exit from Snd", S_exit, 
						  "key:", 8, "ctrl:", "meta:",  "C-x:",
						  ki->key, ki->c, ki->m, ki->x,
						  dpy_box, current_sep,
						  bind_exit);
      remember_pref(prf, reflect_exit, save_exit_binding, help_exit, clear_exit, NULL);
      FREE(ki);

      current_sep = make_inter_variable_separator(dpy_box, prf->label);
      ki = find_prefs_key_binding("goto-maxamp");
      prf = prefs_row_with_text_and_three_toggles("move cursor to channel's maximum sample", S_maxamp_position, 
						  "key:", 8, "ctrl:", "meta:",  "C-x:",
						  ki->key, ki->c, ki->m, ki->x,
						  dpy_box, current_sep,
						  bind_goto_maxamp);
      remember_pref(prf, reflect_goto_maxamp, save_goto_maxamp_binding, help_goto_maxamp, clear_goto_maxamp, NULL);
      FREE(ki);

    }


    /* ---------------- cursor options ---------------- */

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    cursor_label = make_inner_label("  cursor options", dpy_box, current_sep);

    prf = prefs_row_with_toggle("report cursor location as it moves", S_with_verbose_cursor,
				rts_verbose_cursor = verbose_cursor(ss), 
				dpy_box, cursor_label, 
				verbose_cursor_toggle);
    remember_pref(prf, reflect_verbose_cursor, save_verbose_cursor, NULL, NULL, revert_verbose_cursor);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    {
      char *str1;
      str = mus_format("%.2f", rts_cursor_update_interval = cursor_update_interval(ss));
      str1 = mus_format("%d", rts_cursor_location_offset = cursor_location_offset(ss));
      prf = prefs_row_with_toggle_with_two_texts("track current location while playing", S_with_tracking_cursor,
						 (rts_with_tracking_cursor = with_tracking_cursor(ss)), 
						 "update:", str,
						 "offset:", str1, 8, 
						 dpy_box, current_sep,
						 with_tracking_cursor_toggle,
						 cursor_location_text);
      remember_pref(prf, reflect_with_tracking_cursor, save_with_tracking_cursor, NULL, NULL, revert_with_tracking_cursor);
      FREE(str);
      FREE(str1);
    }

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    str = mus_format("%d", rts_cursor_size = cursor_size(ss));
    prf = prefs_row_with_number("size", S_cursor_size,
				str, 4, 
				dpy_box, current_sep,
				cursor_size_up, cursor_size_down, cursor_size_from_text);
    remember_pref(prf, reflect_cursor_size, save_cursor_size, NULL, NULL, revert_cursor_size);
    FREE(str);
    if (cursor_size(ss) <= 0) XtSetSensitive(prf->arrow_down, false);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_radio_box("shape", S_cursor_style,
				   cursor_styles, NUM_CURSOR_STYLES, 
				   rts_cursor_style = cursor_style(ss),
				   dpy_box, current_sep, 
				   cursor_style_choice);
    remember_pref(prf, reflect_cursor_style, save_cursor_style, NULL, NULL, revert_cursor_style);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_radio_box("tracking cursor shape", S_tracking_cursor_style,
				   cursor_styles, NUM_CURSOR_STYLES, 
				   rts_tracking_cursor_style = tracking_cursor_style(ss),
				   dpy_box, current_sep, 
				   tracking_cursor_style_choice);
    remember_pref(prf, reflect_tracking_cursor_style, save_tracking_cursor_style, NULL, NULL, revert_tracking_cursor_style);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    saved_cursor_color = ss->sgx->cursor_color;
    prf = prefs_color_selector_row("color", S_cursor_color, ss->sgx->cursor_color,
				   dpy_box, current_sep,
				   cursor_color_func);
    remember_pref(prf, NULL, save_cursor_color, NULL, clear_cursor_color, revert_cursor_color);


    /* ---------------- (overall) colors ---------------- */

    current_sep = make_inter_variable_separator(dpy_box, prf->rscl);
    cursor_label = make_inner_label("  colors", dpy_box, current_sep);
    
    saved_basic_color = ss->sgx->basic_color;
    prf = prefs_color_selector_row("main background color", S_basic_color, ss->sgx->basic_color,
				   dpy_box, cursor_label,
				   basic_color_func);
    remember_pref(prf, NULL, save_basic_color, NULL, clear_basic_color, revert_basic_color);

    current_sep = make_inter_variable_separator(dpy_box, prf->rscl);
    saved_highlight_color = ss->sgx->highlight_color;
    prf = prefs_color_selector_row("main highlight color", S_highlight_color, ss->sgx->highlight_color,
				   dpy_box, current_sep,
				   highlight_color_func);
    remember_pref(prf, NULL, save_highlight_color, NULL, clear_highlight_color, revert_highlight_color);

    current_sep = make_inter_variable_separator(dpy_box, prf->rscl);
    saved_position_color = ss->sgx->position_color;
    prf = prefs_color_selector_row("second highlight color", S_position_color, ss->sgx->position_color,
				   dpy_box, current_sep,
				   position_color_func);
    remember_pref(prf, NULL, save_position_color, NULL, clear_position_color, revert_position_color);

    current_sep = make_inter_variable_separator(dpy_box, prf->rscl);
    saved_zoom_color = ss->sgx->zoom_color;
    prf = prefs_color_selector_row("third highlight color", S_zoom_color, ss->sgx->zoom_color,
				   dpy_box, current_sep,
				   zoom_color_func);
    remember_pref(prf, NULL, save_zoom_color, NULL, clear_zoom_color, revert_zoom_color);
  }

  current_sep = make_inter_topic_separator(topics);

  /* -------- graphs -------- */
  {
    Widget grf_box, grf_label, colgrf_label;

    /* ---------------- graph options ---------------- */

    grf_box = make_top_level_box(topics);
    grf_label = make_top_level_label("graph options", grf_box);

    prf = prefs_row_with_radio_box("how to connect the dots", S_graph_style,
				   graph_styles, NUM_GRAPH_STYLES, 
				   rts_graph_style = graph_style(ss),
				   grf_box, grf_label,
				   graph_style_choice);
    remember_pref(prf, reflect_graph_style, save_graph_style, NULL, NULL, revert_graph_style);

    current_sep = make_inter_variable_separator(grf_box, prf->label);
    str = mus_format("%d", rts_dot_size = dot_size(ss));
    prf = prefs_row_with_number("dot size", S_dot_size,
				str, 4, 
				grf_box, current_sep,
				dot_size_up, dot_size_down, dot_size_from_text);
    remember_pref(prf, reflect_dot_size, save_dot_size, NULL, NULL, revert_dot_size);
    FREE(str);
    if (dot_size(ss) <= 0) XtSetSensitive(prf->arrow_down, false);

    current_sep = make_inter_variable_separator(grf_box, prf->label);
    rts_initial_beg = initial_beg();
    rts_initial_dur = initial_dur();
    str = mus_format("%.2f : %.2f", rts_initial_beg, rts_initial_dur);
    prf = prefs_row_with_text_with_toggle("initial graph x bounds", S_initial_graph_hook, 
					  (rts_full_duration = full_duration()),
					  "show full duration", str, 16,
					  grf_box, current_sep,
					  initial_bounds_toggle,
					  initial_bounds_text);
    FREE(str);
    remember_pref(prf, reflect_initial_bounds, save_initial_bounds, help_initial_bounds, clear_initial_bounds, revert_initial_bounds);

    current_sep = make_inter_variable_separator(grf_box, prf->label);
    prf = prefs_row_with_radio_box("how to layout multichannel graphs", S_channel_style,
				   channel_styles, NUM_CHANNEL_STYLES, 
				   rts_channel_style = channel_style(ss),
				   grf_box, current_sep,
				   channel_style_choice);
    remember_pref(prf, reflect_channel_style, save_channel_style, NULL, NULL, revert_channel_style);

    current_sep = make_inter_variable_separator(grf_box, prf->label);
    prf = prefs_row_with_toggle("layout wave and fft graphs horizontally", S_graphs_horizontal,
				rts_graphs_horizontal = graphs_horizontal(ss),
				grf_box, current_sep,
				graphs_horizontal_toggle);
    remember_pref(prf, reflect_graphs_horizontal, save_graphs_horizontal, NULL, NULL, revert_graphs_horizontal);

    current_sep = make_inter_variable_separator(grf_box, prf->label);
   prf = prefs_row_with_toggle("include y=0 line in sound graphs", S_show_y_zero,
				rts_show_y_zero = show_y_zero(ss),
				grf_box, current_sep,
				y_zero_toggle);
    remember_pref(prf, reflect_show_y_zero, save_show_y_zero, NULL, NULL, revert_show_y_zero);

    current_sep = make_inter_variable_separator(grf_box, prf->label);
    rts_show_grid = show_grid(ss);
    prf = prefs_row_with_toggle("include a grid in sound graphs", S_show_grid,
				rts_show_grid == WITH_GRID,
				grf_box, current_sep,
				grid_toggle);
    remember_pref(prf, reflect_show_grid, save_show_grid, NULL, NULL, revert_show_grid);

    current_sep = make_inter_variable_separator(grf_box, prf->label);
    prf = prefs_row_with_scale("grid density", S_grid_density, 
			       2.0, rts_grid_density = grid_density(ss),
			       grf_box, current_sep,
			       grid_density_scale_callback, grid_density_text_callback);
    remember_pref(prf, reflect_grid_density, save_grid_density, NULL, NULL, revert_grid_density);

    current_sep = make_inter_variable_separator(grf_box, prf->label);
    rts_show_axes = show_axes(ss);
    prf = prefs_row_with_list("what axes to display", S_show_axes, show_axes_choices[(int)rts_show_axes],
			      show_axes_choices, NUM_SHOW_AXES,
			      grf_box, current_sep,
			      show_axes_from_text,
			      NULL, NULL,
			      show_axes_from_menu);
    remember_pref(prf, reflect_show_axes, save_show_axes, NULL, clear_show_axes, revert_show_axes);

    current_sep = make_inter_variable_separator(grf_box, prf->label);
    rts_x_axis_style = x_axis_style(ss);
    prf = prefs_row_with_list("time division", S_x_axis_style, x_axis_styles[(int)rts_x_axis_style],
			      x_axis_styles, NUM_X_AXIS_STYLES,
			      grf_box, current_sep,
			      x_axis_style_from_text,
			      NULL, NULL,
			      x_axis_style_from_menu);
    remember_pref(prf, reflect_x_axis_style, save_x_axis_style, NULL, clear_x_axis_style, revert_x_axis_style);

    current_sep = make_inter_variable_separator(grf_box, prf->label);
    prf = prefs_row_with_toggle("include smpte info", "show-smpte-label",
				(include_smpte = find_smpte()),
				grf_box, current_sep,
				smpte_toggle);
    remember_pref(prf, reflect_smpte, save_smpte, help_smpte, clear_smpte, revert_smpte);

    /* ---------------- (graph) colors ---------------- */

    current_sep = make_inter_variable_separator(grf_box, prf->label); 
    colgrf_label = make_inner_label("  colors", grf_box, current_sep);

    saved_data_color = ss->sgx->data_color;    
    prf = prefs_color_selector_row("unselected data (waveform) color", S_data_color, ss->sgx->data_color,
				   grf_box, colgrf_label,
				   data_color_func);
    remember_pref(prf, NULL, save_data_color, NULL, clear_data_color, revert_data_color);

    current_sep = make_inter_variable_separator(grf_box, prf->rscl);
    saved_graph_color = ss->sgx->graph_color;
    prf = prefs_color_selector_row("unselected graph (background) color", S_graph_color, ss->sgx->graph_color,
				   grf_box, current_sep,
				   graph_color_func);
    remember_pref(prf, NULL, save_graph_color, NULL, clear_graph_color, revert_graph_color);

    current_sep = make_inter_variable_separator(grf_box, prf->rscl);
    saved_selected_data_color = ss->sgx->selected_data_color;
    prf = prefs_color_selector_row("selected channel data (waveform) color", S_selected_data_color, ss->sgx->selected_data_color,
				   grf_box, current_sep,
				   selected_data_color_func);
    remember_pref(prf, NULL, save_selected_data_color, NULL, clear_selected_data_color, revert_selected_data_color);

    current_sep = make_inter_variable_separator(grf_box, prf->rscl);
    saved_selected_graph_color = ss->sgx->selected_graph_color;
    prf = prefs_color_selector_row("selected channel graph (background) color", S_selected_graph_color, ss->sgx->selected_graph_color,
				   grf_box, current_sep,
				   selected_graph_color_func);
    remember_pref(prf, NULL, save_selected_graph_color, NULL, clear_selected_graph_color, revert_selected_graph_color);

    current_sep = make_inter_variable_separator(grf_box, prf->rscl);
    saved_selection_color = ss->sgx->selection_color;
    prf = prefs_color_selector_row("selection color", S_selection_color, ss->sgx->selection_color,
				   grf_box, current_sep,
				   selection_color_func);
    remember_pref(prf, NULL, save_selection_color, NULL, clear_selection_color, revert_selection_color);

    /* ---------------- (graph) fonts ---------------- */

    current_sep = make_inter_variable_separator(grf_box, prf->rscl);
    colgrf_label = make_inner_label("  fonts", grf_box, current_sep);

    rts_axis_label_font = copy_string(axis_label_font(ss));
    prf = prefs_row_with_text("axis label font", S_axis_label_font, 
			      axis_label_font(ss), 
			      grf_box, colgrf_label,
			      axis_label_font_text);
    remember_pref(prf, reflect_axis_label_font, save_axis_label_font, NULL, clear_axis_label_font, revert_axis_label_font);

    current_sep = make_inter_variable_separator(grf_box, prf->label);     
    rts_axis_numbers_font = copy_string(axis_numbers_font(ss));
    prf = prefs_row_with_text("axis number font", S_axis_numbers_font, 
			      axis_numbers_font(ss), 
			      grf_box, current_sep,
			      axis_numbers_font_text);
    remember_pref(prf, reflect_axis_numbers_font, save_axis_numbers_font, NULL, clear_axis_numbers_font, revert_axis_numbers_font);

    current_sep = make_inter_variable_separator(grf_box, prf->label);     
    rts_peaks_font = copy_string(peaks_font(ss));
    prf = prefs_row_with_text("fft peaks font", S_peaks_font, 
			      peaks_font(ss), 
			      grf_box, current_sep,
			      peaks_font_text);
    remember_pref(prf, reflect_peaks_font, save_peaks_font, NULL, clear_peaks_font, revert_peaks_font);

    current_sep = make_inter_variable_separator(grf_box, prf->label);     
    rts_bold_peaks_font = copy_string(bold_peaks_font(ss));
    prf = prefs_row_with_text("fft peaks bold font (for main peaks)", S_bold_peaks_font, 
			      bold_peaks_font(ss), 
			      grf_box, current_sep,
			      bold_peaks_font_text);
    remember_pref(prf, reflect_bold_peaks_font, save_bold_peaks_font, NULL, clear_bold_peaks_font, revert_bold_peaks_font);

    current_sep = make_inter_variable_separator(grf_box, prf->label);     
    rts_tiny_font = copy_string(tiny_font(ss));
    prf = prefs_row_with_text("tiny font (for various annotations)", S_peaks_font, 
			      tiny_font(ss), 
			      grf_box, current_sep,
			      tiny_font_text);
    remember_pref(prf, reflect_tiny_font, save_tiny_font, NULL, clear_tiny_font, revert_tiny_font);
  }

  current_sep = make_inter_topic_separator(topics);

  /* -------- transform -------- */
  {
    Widget fft_box, fft_label;

    /* ---------------- transform options ---------------- */

    fft_box = make_top_level_box(topics);
    fft_label = make_top_level_label("transform options", fft_box);

    str = mus_format("%d", rts_fft_size = transform_size(ss));
    prf = prefs_row_with_number("size", S_transform_size,
				str, 12, 
				fft_box, fft_label, 
				fft_size_up, fft_size_down, fft_size_from_text);
    remember_pref(prf, reflect_fft_size, save_fft_size, NULL, NULL, revert_fft_size);
    FREE(str);
    if (transform_size(ss) <= 2) XtSetSensitive(prf->arrow_down, false);

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    prf = prefs_row_with_radio_box("transform graph choice", S_transform_graph_type,
				   transform_graph_types, NUM_TRANSFORM_GRAPH_TYPES, 
				   rts_transform_graph_type = transform_graph_type(ss),
				   fft_box, current_sep,
				   transform_graph_type_choice);
    remember_pref(prf, reflect_transform_graph_type, save_transform_graph_type, NULL, NULL, revert_transform_graph_type);

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    rts_transform_type = transform_type(ss);
    prf = prefs_row_with_list("transform", S_transform_type, transform_types[rts_transform_type],
			      transform_types, NUM_BUILTIN_TRANSFORM_TYPES,
			      fft_box, current_sep,
			      transform_type_from_text,
			      transform_type_completer, NULL,
			      transform_type_from_menu);
    remember_pref(prf, reflect_transform_type, save_transform_type, NULL, clear_transform_type, revert_transform_type);

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    rts_fft_window = fft_window(ss);
    prf = prefs_row_with_list("data window", S_fft_window, fft_windows[(int)rts_fft_window],
			      fft_windows, MUS_NUM_WINDOWS,
			      fft_box, current_sep,
			      fft_window_from_text,
			      fft_window_completer, NULL,
			      fft_window_from_menu);
    remember_pref(prf, reflect_fft_window, save_fft_window, NULL, clear_fft_window, revert_fft_window);

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    prf = prefs_row_with_scale("data window family parameter", S_fft_window_beta, 
			       1.0, rts_fft_window_beta = fft_window_beta(ss),
			       fft_box, current_sep,
			       fft_window_beta_scale_callback, fft_window_beta_text_callback);
    remember_pref(prf, reflect_fft_window_beta, save_fft_window_beta, NULL, NULL, revert_fft_window_beta);

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    str = mus_format("%d", rts_max_transform_peaks = max_transform_peaks(ss));
    prf = prefs_row_with_toggle_with_text("show fft peak data", S_show_transform_peaks,
					  rts_show_transform_peaks = show_transform_peaks(ss),
					  "max peaks:", str, 5,
					  fft_box, current_sep,
					  transform_peaks_toggle, max_peaks_text);
    remember_pref(prf, reflect_transform_peaks, save_transform_peaks, NULL, NULL, revert_transform_peaks);
    FREE(str);

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    {
      const char **cmaps;
      int i, len;
      len = num_colormaps();
      cmaps = (const char **)CALLOC(len, sizeof(const char *));
      for (i = 0; i < len; i++)
	cmaps[i] = (const char *)colormap_name(i);
      rts_colormap = color_map(ss);
      prf = prefs_row_with_list("sonogram colormap", S_colormap, cmaps[rts_colormap],
				cmaps, len,
				fft_box, current_sep,
				colormap_from_text,
				colormap_completer, NULL,
				colormap_from_menu);
      remember_pref(prf, reflect_colormap, save_colormap, NULL, clear_colormap, revert_colormap);
      FREE(cmaps);
    }

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    prf = prefs_row_with_toggle("y axis as log magnitude (dB)", S_fft_log_magnitude,
				rts_fft_log_magnitude = fft_log_magnitude(ss),
				fft_box, current_sep,
				log_magnitude_toggle);
    remember_pref(prf, reflect_fft_log_magnitude, save_fft_log_magnitude, NULL, NULL, revert_fft_log_magnitude);

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    str = mus_format("%.1f", rts_min_dB = min_dB(ss));
    prf = prefs_row_with_text("minimum y-axis dB value", S_min_dB, str,
			      fft_box, current_sep,
			      min_dB_text);
    remember_pref(prf, reflect_min_dB, save_min_dB, NULL, NULL, revert_min_dB);
    FREE(str);

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    prf = prefs_row_with_toggle("x axis as log freq", S_fft_log_frequency,
				rts_fft_log_frequency = fft_log_frequency(ss),
				fft_box, current_sep,
				log_frequency_toggle);
    remember_pref(prf, reflect_fft_log_frequency, save_fft_log_frequency, NULL, NULL, revert_fft_log_frequency);

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    prf = prefs_row_with_radio_box("normalization", S_transform_normalization,
				   transform_normalizations, NUM_TRANSFORM_NORMALIZATIONS, 
				   rts_transform_normalization = transform_normalization(ss),
				   fft_box, current_sep,
				   transform_normalization_choice);
    remember_pref(prf, reflect_transform_normalization, save_transform_normalization, NULL, NULL, revert_transform_normalization);
  }

  current_sep = make_inter_topic_separator(topics);

  /* -------- marks, mixes, and regions -------- */
  {
    Widget mmr_box, mmr_label;
    char *str1, *str2;

    /* ---------------- marks and mixes ---------------- */

    mmr_box = make_top_level_box(topics);
    mmr_label = make_top_level_label("marks and mixes", mmr_box);

    saved_mark_color = ss->sgx->mark_color;
    prf = prefs_color_selector_row("mark and mix tag color", S_mark_color, ss->sgx->mark_color,
				   mmr_box, mmr_label,
				   mark_color_func);
    remember_pref(prf, NULL, save_mark_color, NULL, clear_mark_color, revert_mark_color);

    current_sep = make_inter_variable_separator(mmr_box, prf->rscl);

    str1 = mus_format("%d", rts_mark_tag_width = mark_tag_width(ss));
    str2 = mus_format("%d", rts_mark_tag_height = mark_tag_height(ss));
    prf = prefs_row_with_two_texts("mark tag size", S_mark_tag_width, 
				   "width:", str1, "height:", str2, 4,
				   mmr_box, current_sep,
				   mark_tag_size_text);
    remember_pref(prf, reflect_mark_tag_size, save_mark_tag_size, NULL, NULL, revert_mark_tag_size);
    FREE(str2);
    FREE(str1);

    current_sep = make_inter_variable_separator(mmr_box, prf->label);
    str1 = mus_format("%d", rts_mix_tag_width = mix_tag_width(ss));
    str2 = mus_format("%d", rts_mix_tag_height = mix_tag_height(ss));
    prf = prefs_row_with_two_texts("mix tag size", S_mix_tag_width, 
				   "width:", str1, "height:", str2, 4,
				   mmr_box, current_sep,
				   mix_tag_size_text);
    remember_pref(prf, reflect_mix_tag_size, save_mix_tag_size, NULL, NULL, revert_mix_tag_size);
    FREE(str2);
    FREE(str1);

    current_sep = make_inter_variable_separator(mmr_box, prf->label);
    saved_mix_color = ss->sgx->mix_color;
    prf = prefs_color_selector_row("mix waveform color", S_mix_color, ss->sgx->mix_color,
				   mmr_box, current_sep,
				   mix_color_func);
    remember_pref(prf, NULL, save_mix_color, NULL, clear_mix_color, revert_mix_color);

    current_sep = make_inter_variable_separator(mmr_box, prf->rscl);
    str = mus_format("%d", rts_mix_waveform_height = mix_waveform_height(ss));
    prf = prefs_row_with_toggle_with_text("show mix waveforms (attached to the mix tag)", S_show_mix_waveforms,
					  rts_show_mix_waveforms = show_mix_waveforms(ss),
					  "max waveform height:", str, 5,
					  mmr_box, current_sep,
					  show_mix_waveforms_toggle, mix_waveform_height_text);
    remember_pref(prf, reflect_mix_waveforms, save_mix_waveforms, NULL, NULL, revert_mix_waveforms);
    FREE(str);

    current_sep = make_inter_variable_separator(mmr_box, prf->label);
    prf = prefs_row_with_toggle("include mark pane", "mark-pane",
				(include_mark_pane = find_mark_pane()),
				mmr_box, current_sep,
				mark_pane_toggle);
    remember_pref(prf, reflect_mark_pane, save_mark_pane, help_mark_pane, clear_mark_pane, revert_mark_pane);
  }
  
  current_sep = make_inter_topic_separator(topics);

  /* -------- clm -------- */
  {
    Widget clm_box, clm_label;

    /* ---------------- clm options ---------------- */

    clm_box = make_top_level_box(topics);
    clm_label = make_top_level_label("clm", clm_box);

    rts_with_sound = with_sound_is_loaded();
    prf = prefs_row_with_toggle("include with-sound", "with-sound",
				rts_with_sound,
				clm_box, clm_label,
				with_sound_toggle);
    remember_pref(prf, reflect_with_sound, save_with_sound, help_with_sound, clear_with_sound, revert_with_sound);

    current_sep = make_inter_variable_separator(clm_box, prf->label);
    rts_speed_control_style = speed_control_style(ss);
    str = mus_format("%d", rts_speed_control_tones = speed_control_tones(ss));
    prf = prefs_row_with_radio_box_and_number("speed control choice", S_speed_control_style,
					      speed_control_styles, NUM_SPEED_CONTROL_STYLES, (int)speed_control_style(ss),
					      speed_control_tones(ss), str, 6,
					      clm_box, current_sep,
					      speed_control_choice, speed_control_up, speed_control_down, speed_control_text);
    XtSetSensitive(prf->arrow_down, (speed_control_tones(ss) > MIN_SPEED_CONTROL_SEMITONES));
    remember_pref(prf, reflect_speed_control, save_speed_control, NULL, NULL, revert_speed_control);
    FREE(str);

#if HAVE_SCHEME
    current_sep = make_inter_variable_separator(clm_box, prf->label);
    prf = prefs_row_with_toggle("include hidden controls dialog", "hidden-controls-dialog",
				(include_hidden_controls = find_hidden_controls()),
				clm_box, current_sep, 
				hidden_controls_toggle);
    remember_pref(prf, reflect_hidden_controls, save_hidden_controls, help_hidden_controls, clear_hidden_controls, revert_hidden_controls);
#endif

    current_sep = make_inter_variable_separator(clm_box, prf->label);
    str = mus_format("%d", rts_sinc_width = sinc_width(ss));
    prf = prefs_row_with_text("sinc interpolation width in srate converter", S_sinc_width, str,
			      clm_box, current_sep,
			      sinc_width_text);
    remember_pref(prf, reflect_sinc_width, save_sinc_width, NULL, NULL, revert_sinc_width);
    FREE(str);

    current_sep = make_inter_variable_separator(clm_box, prf->label);
    rts_clm_file_name = copy_string(find_clm_file_name());
    prf = prefs_row_with_text("with-sound default output file name", "*clm-file-name*", rts_clm_file_name,
			      clm_box, current_sep,
			      clm_file_name_text);
    remember_pref(prf, reflect_clm_file_name, save_clm_file_name, help_clm_file_name, clear_clm_file_name, revert_clm_file_name);

    current_sep = make_inter_variable_separator(clm_box, prf->label);
    rts_clm_table_size = find_clm_table_size();
    rts_clm_file_buffer_size = find_clm_file_buffer_size();
    prf = prefs_row_with_two_texts("sizes", "*clm-table-size*",
				   "wave table:", NULL, "file buffer:", NULL, 8,
				   clm_box, current_sep,
				   clm_sizes_text);
    reflect_clm_sizes(prf);
    remember_pref(prf, reflect_clm_sizes, save_clm_sizes, help_clm_sizes, clear_clm_sizes, revert_clm_sizes);
  }

  current_sep = make_inter_topic_separator(topics);

  /* -------- programming -------- */
  {
    Widget prg_box, prg_label;

    /* ---------------- listener options ---------------- */

    prg_box = make_top_level_box(topics);
    prg_label = make_top_level_label("listener options", prg_box);

    prf = prefs_row_with_toggle("show listener at start up", S_show_listener,
				rts_show_listener = listener_is_visible(),
				prg_box, prg_label,
				show_listener_toggle);
    remember_pref(prf, reflect_show_listener, save_show_listener, NULL, clear_show_listener, revert_show_listener);

#if WITH_RUN
    current_sep = make_inter_variable_separator(prg_box, prf->label);
    str = mus_format("%d", rts_optimization = optimization(ss));
    prf = prefs_row_with_number("optimization level", S_optimization,
				str, 3, 
				prg_box, current_sep, 
				optimization_up, optimization_down, optimization_from_text);
    remember_pref(prf, reflect_optimization, save_optimization, NULL, NULL, revert_optimization);
    FREE(str);
    if (optimization(ss) == 6) XtSetSensitive(prf->arrow_up, false);
    if (optimization(ss) == 0) XtSetSensitive(prf->arrow_down, false);
#endif

    current_sep = make_inter_variable_separator(prg_box, prf->label);
    rts_listener_prompt = copy_string(listener_prompt(ss));
    prf = prefs_row_with_text("prompt", S_listener_prompt, 
			      listener_prompt(ss), 
			      prg_box, current_sep,
			      listener_prompt_text);
    remember_pref(prf, reflect_listener_prompt, save_listener_prompt, NULL, NULL, revert_listener_prompt);

    current_sep = make_inter_variable_separator(prg_box, prf->label);
    prf = prefs_row_with_toggle("include backtrace in error report", S_show_backtrace,
				rts_show_backtrace = show_backtrace(ss),
				prg_box, current_sep,
				show_backtrace_toggle);
    remember_pref(prf, reflect_show_backtrace, save_show_backtrace, NULL, NULL, revert_show_backtrace);

#if HAVE_GUILE
    current_sep = make_inter_variable_separator(prg_box, prf->label);
    prf = prefs_row_with_toggle("include debugging aids", "snd-break",
				(include_debugging_aids = find_debugging_aids()),
				prg_box, current_sep,
				debugging_aids_toggle);
    remember_pref(prf, reflect_debugging_aids, save_debugging_aids, NULL, clear_debugging_aids, revert_debugging_aids);
    include_debugging_aids = find_debugging_aids();
#endif

    current_sep = make_inter_variable_separator(prg_box, prf->label);
    str = mus_format("%d", rts_print_length = print_length(ss));
    prf = prefs_row_with_text("number of vector elements to display", S_print_length, str,
			      prg_box, current_sep,
			      print_length_text);
    remember_pref(prf, reflect_print_length, save_print_length, NULL, NULL, revert_print_length);
    FREE(str);

    current_sep = make_inter_variable_separator(prg_box, prf->label);
    rts_listener_font = copy_string(listener_font(ss));
    prf = prefs_row_with_text("font", S_listener_font, 
			      listener_font(ss), 
			      prg_box, current_sep,
			      listener_font_text);
    remember_pref(prf, reflect_listener_font, save_listener_font, NULL, clear_listener_font, revert_listener_font);

    current_sep = make_inter_variable_separator(prg_box, prf->label);
    saved_listener_color = ss->sgx->listener_color;
    prf = prefs_color_selector_row("background color", S_listener_color, ss->sgx->listener_color,
				   prg_box, current_sep,
				   listener_color_func);
    remember_pref(prf, NULL, save_listener_color, NULL, clear_listener_color, revert_listener_color);

    current_sep = make_inter_variable_separator(prg_box, prf->rscl);
    saved_listener_text_color = ss->sgx->listener_text_color;
    prf = prefs_color_selector_row("text color", S_listener_text_color, ss->sgx->listener_text_color,
				   prg_box, current_sep,
				   listener_text_color_func);
    remember_pref(prf, NULL, save_listener_text_color, NULL, clear_listener_text_color, revert_listener_text_color);
  }

  current_sep = make_inter_topic_separator(topics);

  /* -------- audio -------- */
  {
    Widget aud_box, aud_label, recorder_label;

    /* ---------------- audio options ---------------- */

    aud_box = make_top_level_box(topics);
    aud_label = make_top_level_label("audio options", aud_box);

    str = mus_format("%d", rts_dac_size = dac_size(ss));
    prf = prefs_row_with_text("dac buffer size", S_dac_size, 
			      str,
			      aud_box, aud_label,
			      dac_size_text);
    remember_pref(prf, reflect_dac_size, save_dac_size, NULL, NULL, revert_dac_size);
    FREE(str);

    current_sep = make_inter_variable_separator(aud_box, prf->label);
    prf = prefs_row_with_toggle("fold in otherwise unplayable channels", S_dac_combines_channels,
				rts_dac_combines_channels = dac_combines_channels(ss),
				aud_box, current_sep,
				dac_combines_channels_toggle);
    remember_pref(prf, reflect_dac_combines_channels, save_dac_combines_channels, NULL, NULL, revert_dac_combines_channels);

    current_sep = make_inter_variable_separator(aud_box, prf->label);
    recorder_label = make_inner_label("  recorder options", aud_box, current_sep);

    rts_recorder_filename = copy_string(rec_filename());
    prf = prefs_row_with_text("recorder output file name", S_recorder_file, rec_filename(),
			      aud_box, recorder_label,
			      recorder_filename_text);
    remember_pref(prf, reflect_recorder_filename, save_recorder_filename, NULL, NULL, revert_recorder_filename);
    current_sep = make_inter_variable_separator(aud_box, prf->label);

    prf = prefs_row_with_toggle("automatically open the recorded sound", S_recorder_autoload,
				rts_recorder_autoload = rec_autoload(),
				aud_box, current_sep,
				recorder_autoload_toggle);
    remember_pref(prf, reflect_recorder_autoload, save_recorder_autoload, NULL, NULL, revert_recorder_autoload);
    current_sep = make_inter_variable_separator(aud_box, prf->label);

    str = mus_format("%d", rts_recorder_buffer_size = rec_buffer_size());
    prf = prefs_row_with_text("input buffer size", S_recorder_buffer_size, 
			      str,
			      aud_box, current_sep,
			      recorder_buffer_size_text);
    remember_pref(prf, reflect_recorder_buffer_size, save_recorder_buffer_size, NULL, NULL, revert_recorder_buffer_size);
    FREE(str);
    current_sep = make_inter_variable_separator(aud_box, prf->label);

    rts_recorder_output_chans = rec_output_chans();
    prf = prefs_row_with_radio_box("default recorder output sound attributes: chans", S_recorder_out_chans,
				   recorder_out_chans_choices, NUM_RECORDER_OUT_CHANS_CHOICES, -1,
				   aud_box, current_sep,
				   recorder_output_chans_choice);
    reflect_recorder_output_chans(prf);
    remember_pref(prf, reflect_recorder_output_chans, save_recorder_output_chans, NULL, NULL, revert_recorder_output_chans);

    rts_recorder_srate = rec_srate();
    prf = prefs_row_with_radio_box("srate", S_recorder_srate,
				   recorder_srate_choices, NUM_RECORDER_SRATE_CHOICES, -1,
				   aud_box, prf->label,
				   recorder_srate_choice);
    reflect_recorder_srate(prf);
    remember_pref(prf, reflect_recorder_srate, save_recorder_srate, NULL, NULL, revert_recorder_srate);

    rts_recorder_output_header_type = rec_output_header_type();
    prf = prefs_row_with_radio_box("header type", S_recorder_out_header_type,
				   recorder_out_type_choices, NUM_RECORDER_OUT_TYPE_CHOICES, -1,
				   aud_box, prf->label,
				   recorder_output_header_type_choice);
    recorder_output_header_type_prf = prf;
    remember_pref(prf, reflect_recorder_output_header_type, save_recorder_output_header_type, NULL, NULL, revert_recorder_output_header_type);

    rts_recorder_output_data_format = rec_output_data_format();
    prf = prefs_row_with_radio_box("data format", S_recorder_out_data_format,
				   recorder_out_format_choices, NUM_RECORDER_OUT_FORMAT_CHOICES, -1,
				   aud_box, prf->label,
				   recorder_output_data_format_choice);
    recorder_output_data_format_prf = prf;
    remember_pref(prf, reflect_recorder_output_data_format, save_recorder_output_data_format, NULL, NULL, revert_recorder_output_data_format);
    reflect_recorder_output_header_type(recorder_output_header_type_prf);
    reflect_recorder_output_data_format(recorder_output_data_format_prf);
  }

  {
    Atom wm_delete_window;
    wm_delete_window = XmInternAtom(MAIN_DISPLAY(ss), "WM_DELETE_WINDOW", false);
    XmAddWMProtocolCallback(XtParent(preferences_dialog), wm_delete_window, wm_delete_callback, NULL);
  }

  XtManageChild(preferences_dialog);
  set_dialog_widget(PREFERENCES_DIALOG, preferences_dialog);
  return(preferences_dialog);
}
