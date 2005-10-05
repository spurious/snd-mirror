#include "snd.h"

/* preferences dialog; layout design taken from webmail
 *
 * also add an html-style table of contents at top -- basically mimic a web-page
 */


/* start automatically if no .snd? -- create empty if not wanted?
 *  color choice via 4 sliders + extra line
 *  font choice via some spinners + text + pull-down menus and extra line if needed
 */


static Widget preferences_dialog = NULL;
static bool prefs_helping = false;

#define MID_POSITION 61
#define MID_SPACE 16
#define INTER_TOPIC_SPACE 2
#define INTER_VARIABLE_SPACE 2
#define HELP_WAIT_TIME 500
#define POWER_WAIT_TIME 100
#define POWER_INITIAL_WAIT_TIME 500


typedef struct prefs_info {
  Widget label, text, arrow_up, arrow_down, arrow_right, error, toggle, scale;
  bool got_error;
  XtIntervalId help_id, power_id;
  const char *var_name;
  const char **values;
  int num_values;
  void (*toggle_func)(struct prefs_info *prf);
  void (*scale_func)(struct prefs_info *prf);
  void (*arrow_up_func)(struct prefs_info *prf);
  void (*arrow_down_func)(struct prefs_info *prf);
  void (*text_func)(struct prefs_info *prf);
  void (*list_func)(struct prefs_info *prf, char *value);
} prefs_info;


/* ---------------- help strings ---------------- */

static void prefs_help(XtPointer context, XtIntervalId *id)
{
  prefs_info *prf = (prefs_info *)context;
  if (help_dialog_is_active())
    snd_help(prf->var_name, 
	     XEN_TO_C_STRING(XEN_OBJECT_HELP(C_STRING_TO_XEN_SYMBOL(prf->var_name))),
	     WITH_WORD_WRAP);
  else prefs_helping = false;
  prf->help_id = 0;
}

static void mouse_enter_pref_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  prefs_info *prf = (prefs_info *)context;
  if (prefs_helping)
    {
      prf->help_id = XtAppAddTimeOut(MAIN_APP(ss),
				     HELP_WAIT_TIME,
				     prefs_help,
				     (XtPointer)prf);
#if DEBUGGING
      if (prf->help_id == 0)
	fprintf(stderr, "help time out id is 0??");
#endif
    }
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
  Arg args[20];
  int n;
  prefs_info *prf = NULL;
  Widget sep;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
  XtSetArg(args[n], XmNrightPosition, MID_POSITION); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_END); n++;
  prf->label = XtCreateManagedWidget(label, xmLabelWidgetClass, box, args, n);
  
  /* ideally we'd use one of the Motif Combo|Spin boxes, but they are all execrable */
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
  XtSetArg(args[n], XmNbottomWidget, prf->label); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, prf->label); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
  XtSetArg(args[n], XmNwidth, MID_SPACE); n++;
  XtSetArg(args[n], XmNseparatorType, XmSHADOW_ETCHED_OUT); n++;
  sep = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, box, args, n);
  
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, sep); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNset, (current_value) ? XmSET : XmUNSET); n++;
  XtSetArg(args[n], XmNborderWidth, 0); n++;
  XtSetArg(args[n], XmNborderColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNmarginHeight, 0); n++;
  XtSetArg(args[n], XmNindicatorOn, XmINDICATOR_FILL); n++;
  XtSetArg(args[n], XmNindicatorSize, 14); n++;
  prf->toggle = XtCreateManagedWidget(" ", xmToggleButtonWidgetClass, box, args, n);
  
  /* no error possible -- right? */
  
  prf->toggle_func = toggle_func;
  XtAddCallback(prf->toggle, XmNvalueChangedCallback, call_toggle_func, (void *)prf);

  XtAddEventHandler(prf->label, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->toggle, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->label, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->toggle, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);

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
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = (char *)CALLOC(12, sizeof(char));
  mus_snprintf(str, 12, "%.3f", (Float)(cb->value) / 100.0);
  XmTextFieldSetString(prf->text, str);
  FREE(str);
}

static prefs_info *prefs_row_with_scale(const char *label, const char *varname, Float current_value,
					Widget box, Widget top_widget, 
					void (*scale_func)(prefs_info *prf),
					void (*text_func)(prefs_info *prf))
{
  Arg args[20];
  int n;
  prefs_info *prf = NULL;
  Widget sep;
  XtCallbackList n1, n2;
  char *str;

  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
  XtSetArg(args[n], XmNrightPosition, MID_POSITION); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_END); n++;
  prf->label = XtCreateManagedWidget(label, xmLabelWidgetClass, box, args, n);
  
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
  XtSetArg(args[n], XmNbottomWidget, prf->label); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, prf->label); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
  XtSetArg(args[n], XmNwidth, MID_SPACE); n++;
  XtSetArg(args[n], XmNseparatorType, XmSHADOW_ETCHED_OUT); n++;
  sep = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, box, args, n);
  

  str = (char *)CALLOC(12, sizeof(char));
  mus_snprintf(str, 12, "%.3f", current_value);
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, sep); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNeditable, true); n++;
  XtSetArg(args[n], XmNcolumns, 6); n++;
  XtSetArg(args[n], XmNvalue, str); n++;
  XtSetArg(args[n], XmNmarginHeight, 1); n++;
  XtSetArg(args[n], XmNborderWidth, 0); n++;
  XtSetArg(args[n], XmNborderColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNbottomShadowColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNtopShadowColor, ss->sgx->white); n++;
  prf->text = make_textfield_widget("text", box, args, n, ACTIVATABLE_BUT_NOT_FOCUSED, NO_COMPLETER);
  FREE(str);
  
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, prf->text); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNborderWidth, 0); n++;
  XtSetArg(args[n], XmNborderColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNmarginHeight, 0); n++;
  XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
  XtSetArg(args[n], XmNshowValue, XmNONE); n++;
  XtSetArg(args[n], XmNdecimalPoints, 2); n++;
  XtSetArg(args[n], XmNvalue, 100 * current_value); n++;
  XtSetArg(args[n], XmNdragCallback, n1 = make_callback_list(prefs_scale_callback, (XtPointer)prf)); n++;
  XtSetArg(args[n], XmNvalueChangedCallback, n2 = make_callback_list(prefs_scale_callback, (XtPointer)prf)); n++;
  prf->scale = XtCreateManagedWidget("", xmScaleWidgetClass, box, args, n);
  
  prf->scale_func = scale_func;
  prf->text_func = text_func;

  XtAddCallback(prf->scale, XmNvalueChangedCallback, call_scale_func, (XtPointer)prf);
  XtAddCallback(prf->text, XmNactivateCallback, call_scale_text_func, (XtPointer)prf);

  XtAddEventHandler(prf->label, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->scale, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->text, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->label, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->scale, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->text, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);

  FREE(n1);
  FREE(n2);

  return(prf);
}


/* ---------------- number row ---------------- */

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

static void call_text_func(Widget w, XtPointer context, XtPointer info) 
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->text_func))
    (*(prf->text_func))(prf);
}

static prefs_info *prefs_row_with_number(const char *label, const char *varname, const char *value, int cols,
					 Widget box, Widget top_widget,
 					 void (*arrow_up_func)(prefs_info *prf), void (*arrow_down_func)(prefs_info *prf), 
					 void (*text_func)(prefs_info *prf))
{
  Arg args[20];
  int n;
  prefs_info *prf = NULL;
  Widget sep;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
  XtSetArg(args[n], XmNrightPosition, MID_POSITION); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_END); n++;
  prf->label = XtCreateManagedWidget(label, xmLabelWidgetClass, box, args, n);
  
  /* ideally we'd use one of the Motif Combo|Spin boxes, but they are all execrable */
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
  XtSetArg(args[n], XmNbottomWidget, prf->label); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, prf->label); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
  XtSetArg(args[n], XmNwidth, MID_SPACE); n++;
  XtSetArg(args[n], XmNseparatorType, XmSHADOW_ETCHED_OUT); n++;
  sep = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, box, args, n);
  
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, sep); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNcolumns, cols); n++;
  XtSetArg(args[n], XmNvalue, value); n++;
  XtSetArg(args[n], XmNmarginHeight, 1); n++;
  XtSetArg(args[n], XmNborderWidth, 0); n++;
  XtSetArg(args[n], XmNborderColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNbottomShadowColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNtopShadowColor, ss->sgx->white); n++;
  prf->text = make_textfield_widget("text", box, args, n, ACTIVATABLE_BUT_NOT_FOCUSED, NO_COMPLETER);
  
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, prf->text); n++;
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
  
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, prf->arrow_up); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_END); n++;
  prf->error = XtCreateManagedWidget("", xmLabelWidgetClass, box, args, n);
  
  prf->text_func = text_func;
  prf->arrow_up_func = arrow_up_func;
  prf->arrow_down_func = arrow_down_func;

  XtAddCallback(prf->arrow_down, XmNarmCallback, call_arrow_down_press, (XtPointer)prf);
  XtAddCallback(prf->arrow_down, XmNdisarmCallback, remove_arrow_func, (XtPointer)prf);
  XtAddCallback(prf->arrow_up, XmNarmCallback, call_arrow_up_press, (XtPointer)prf);
  XtAddCallback(prf->arrow_up, XmNdisarmCallback, remove_arrow_func, (XtPointer)prf);

  XtAddCallback(prf->text, XmNactivateCallback, call_text_func, (XtPointer)prf);

  XtAddEventHandler(prf->label, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->text, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->arrow_up, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->arrow_down, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->error, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->label, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->text, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->arrow_up, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->arrow_down, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->error, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);

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

static prefs_info *prefs_row_with_completed_list(const char *label, const char *varname, const char *value,
						 const char **values, int num_values,
						 Widget box, Widget top_widget,
						 void (*text_func)(prefs_info *prf),
						 char *(*completion_func)(char *text, void *context), void *completion_context,
						 void (*list_func)(prefs_info *prf, char *value))
{
  Arg args[20];
  int n, i, cols = 0;
  prefs_info *prf = NULL;
  Widget sep, sbar, smenu;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
  XtSetArg(args[n], XmNrightPosition, MID_POSITION); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_END); n++;
  prf->label = XtCreateManagedWidget(label, xmLabelWidgetClass, box, args, n);
  
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
  XtSetArg(args[n], XmNbottomWidget, prf->label); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, prf->label); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
  XtSetArg(args[n], XmNwidth, MID_SPACE); n++;
  XtSetArg(args[n], XmNseparatorType, XmSHADOW_ETCHED_OUT); n++;
  sep = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, box, args, n);
  
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
  prf->text = make_textfield_widget("text", box, args, n, ACTIVATABLE_BUT_NOT_FOCUSED, add_completer_func(completion_func, completion_context));

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
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
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
  smenu = XmCreatePulldownMenu(sbar, "sort-menu", args, n);

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
  XtSetArg(args[n], XmNsubMenuId, smenu); n++;
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
  /* SOMEDAY: figure out how to get a pixmap of an arrow in this damned widget */
  /* SOMEDAY: center the pulldown menu on the text widget and on the current position */
      
  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;}
  for (i = 0; i < num_values; i++)
    if (values[i])
      {
	Widget tmp;
	tmp = XtCreateManagedWidget(values[i],  xmPushButtonWidgetClass, smenu, args, n);
	XtAddCallback(tmp, XmNactivateCallback, prefs_list_callback, make_list_entry(prf, (char *)values[i]));
      }
  /* TODO: if list grows, need to add menu entries -- keep smenu in prf? */

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, prf->arrow_right); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_END); n++;
  prf->error = XtCreateManagedWidget("", xmLabelWidgetClass, box, args, n);

  prf->text_func = text_func;
  XtAddCallback(prf->text, XmNactivateCallback, call_text_func, (XtPointer)prf);

  prf->list_func = list_func;

  XtAddEventHandler(prf->label, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->text, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->arrow_right, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->error, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->label, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->text, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->arrow_right, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->error, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);

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
  XtSetArg(args[n], XmNwidth, INTER_TOPIC_SPACE); n++;
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
  XtSetArg(args[n], XmNwidth, INTER_VARIABLE_SPACE); n++;
  XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
  return(XtCreateManagedWidget("sep", xmSeparatorWidgetClass, topics, args, n));
}




/* ---------------- base buttons ---------------- */

static void preferences_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  prefs_helping = true;

  snd_help("preferences",
	   "This dialog is under construction.  It sets various global variables, 'Save' then writes the new values \
to ~/.snd_prefs_guile|ruby so that they take effect the next time you start Snd.  'Reset' resets all variables to \
their default (initial) values. 'Help' starts this dialog, and as long as it is active, it will post helpful \
information if the mouse lingers over some variable -- sort of a tooltip that stays out of your way. ",
	   WITH_WORD_WRAP);
}

static void preferences_quit_callback(Widget w, XtPointer context, XtPointer info) 
{
  if (XmGetFocusWidget(preferences_dialog) == XmMessageBoxGetChild(preferences_dialog, XmDIALOG_OK_BUTTON))
    XtUnmanageChild(preferences_dialog);
}

static void preferences_reset_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_set_global_defaults(true); 
  /* need complete redisplay, what about snd/chn stuff? also max-regions et al need check */
  /* also what about reflection from elsewhere? */
  /* also there's a distinction between reset to original startup state and reset to complete default state */
}

static void preferences_save_callback(Widget w, XtPointer context, XtPointer info) 
{
  save_options_in_prefs(); /* returns filename, need redirect + msg */
}

/* ---------------- errors ---------------- */

static void clear_prefs_error(Widget w, XtPointer context, XtPointer info) 
{
  prefs_info *prf = (prefs_info *)context;
  ASSERT_WIDGET_TYPE(XmIsLabel(prf->error), prf->error);
  XtRemoveCallback(prf->text, XmNvalueChangedCallback, clear_prefs_error, context);
  set_label(prf->error, "");
}

static void post_prefs_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  ASSERT_WIDGET_TYPE(XmIsLabel(prf->error), prf->error);
  prf->got_error = true;
  set_label(prf->error, msg);
  XtAddCallback(prf->text, XmNvalueChangedCallback, clear_prefs_error, (void *)prf);
}

static void va_post_prefs_error(const char *msg, void *data, ...)
{
  char *buf;
  va_list ap;
  va_start(ap, data);
  buf = vstr(msg, ap);
  va_end(ap);
  post_prefs_error(buf, data);
  FREE(buf);
}


/* ---------------- auto-resize ---------------- */

static void resize_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  set_auto_resize(XmToggleButtonGetState(prf->toggle) == XmSET);
}

/* ---------------- show-y-zero ---------------- */

static void y_zero_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  in_set_show_y_zero(XmToggleButtonGetState(prf->toggle) == XmSET);
}

/* ---------------- show-grid ---------------- */

static void grid_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  in_set_show_grid((XmToggleButtonGetState(prf->toggle) == XmSET) ? WITH_GRID : NO_GRID);
}

/* ---------------- grid-density ---------------- */

static void grid_density_scale_callback(prefs_info *prf)
{
  int val = 0;
  ASSERT_WIDGET_TYPE(XmIsScale(prf->scale), prf->scale);
  XmScaleGetValue(prf->scale, &val);
  in_set_grid_density((Float)val / 100.0);
}

/* TODO: scale needs min/max handling */

static void grid_density_text_callback(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextGetString(prf->text);
  if ((str) && (*str))
    {
      float value = 0.0;
      sscanf(str, "%f", &value);
      if ((value >= 0.0) &&
	  (value <= 1.0))
	{
	  in_set_grid_density(value);
	  XmScaleSetValue(prf->scale, (int)(100 * value));
	}
      else XmTextSetString(prf->text, "error");
    }
}


/* ---------------- fft-size ---------------- */

#define MAX_TRANSFORM_SIZE 1073741824
#define MIN_TRANSFORM_SIZE 2

static void fft_size_up(prefs_info *prf)
{
  off_t size;
  char *new_size;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  size = transform_size(ss) * 2;
  if (size >= MAX_TRANSFORM_SIZE) XtSetSensitive(prf->arrow_up, false);
  if (size > MIN_TRANSFORM_SIZE) XtSetSensitive(prf->arrow_down, true);
  in_set_transform_size(size);
  new_size = mus_format(OFF_TD, transform_size(ss));
  XmTextSetString(prf->text, new_size);
  FREE(new_size);
}

static void fft_size_down(prefs_info *prf)
{
  off_t size;
  char *new_size;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  size = transform_size(ss) / 2;
  if (size <= MIN_TRANSFORM_SIZE) XtSetSensitive(prf->arrow_down, false);
  if (size < MAX_TRANSFORM_SIZE) XtSetSensitive(prf->arrow_up, true);
  in_set_transform_size(size);
  new_size = mus_format(OFF_TD, transform_size(ss));
  XmTextSetString(prf->text, new_size);
  FREE(new_size);
}

static void fft_size_from_text(prefs_info *prf)
{
  off_t size;
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((str) && (*str))
    {
      prf->got_error = false;
      redirect_errors_to(post_prefs_error, (void *)prf);
      size = string_to_off_t(str, 0, "size"); 
      redirect_errors_to(NULL, NULL);
      XtFree(str);
      if (!(prf->got_error))
	{
	  if (POWER_OF_2_P(size))
	    {
	      if (size >= MIN_TRANSFORM_SIZE)
		{
		  if (size <= MAX_TRANSFORM_SIZE)
		    in_set_transform_size(size);
		  else va_post_prefs_error("%s > %d?", (void *)prf, str, MAX_TRANSFORM_SIZE);
		}
	      else va_post_prefs_error("%s < %d?", (void *)prf, str, MIN_TRANSFORM_SIZE);
	    }
	  else post_prefs_error("size must be a power of 2", (void *)prf);
	}
      else prf->got_error = false;
    }
  else post_prefs_error("no size?", (void *)prf);
}


/* -------- fft-window -------- */

static const char *fft_windows[NUM_FFT_WINDOWS] = 
  {"Rectangular", "Hann", "Welch", "Parzen", "Bartlett", "Hamming", "Blackman2", "Blackman3", "Blackman4",
   "Exponential", "Riemann", "Kaiser", "Cauchy", "Poisson", "Gaussian", "Tukey", "Dolph-Chebyshev", "Hann-Poisson", "Connes"};

static list_completer_info *fft_window_completer_info = NULL;

static char *fft_window_completer(char *text, void *data)
{
  if (!fft_window_completer_info)
    {
      fft_window_completer_info = (list_completer_info *)CALLOC(1, sizeof(list_completer_info));
      fft_window_completer_info->exact_match = false;
      fft_window_completer_info->values = (char **)fft_windows;
      fft_window_completer_info->num_values = NUM_FFT_WINDOWS;
      fft_window_completer_info->values_size = NUM_FFT_WINDOWS;
    }
  return(list_completer(text, (void *)fft_window_completer_info));
}

static void fft_window_from_menu(prefs_info *prf, char *value)
{
  int i;
  for (i = 0; i < NUM_FFT_WINDOWS; i++)
    if (strcmp(value, fft_windows[i]) == 0)
      {
	in_set_fft_window(i);
	XmTextFieldSetString(prf->text, value);
      }
}

static void fft_window_from_text(prefs_info *prf)
{
  int i;
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((str) && (*str))
    {
      int len, j = 0;
      char *trimmed_str;
      len = strlen(str);
      trimmed_str = (char *)CALLOC(len + 1, sizeof(char));
      for (i = 0; i < len; i++)
	if (!(isspace(str[i])))
	  trimmed_str[j++] = str[i];
      if (j > 0)
	{
	  int curpos = -1;
	  for (i = 0; i < NUM_FFT_WINDOWS; i++)
	    if (STRCMP(trimmed_str, fft_windows[i]) == 0)
	      {
		curpos = i;
		break;
	      }
	  if (curpos >= 0)
	    in_set_fft_window((mus_fft_window_t)curpos);
	  else post_prefs_error("unknown window", (void *)prf);
	}
      else post_prefs_error("no window?", (void *)prf);
    }
  else post_prefs_error("no window?", (void *)prf);
}

/* ---------------- fft-window-beta ---------------- */

static void fft_window_beta_scale_callback(prefs_info *prf)
{
  int val = 0;
  ASSERT_WIDGET_TYPE(XmIsScale(prf->scale), prf->scale);
  XmScaleGetValue(prf->scale, &val);
  in_set_fft_window_beta((Float)val / 100.0);
}

static void fft_window_beta_text_callback(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextGetString(prf->text);
  if ((str) && (*str))
    {
      float value = 0.0;
      sscanf(str, "%f", &value);
      if ((value >= 0.0) &&
	  (value <= 1.0))
	{
	  in_set_fft_window_beta(value);
	  XmScaleSetValue(prf->scale, (int)(100 * value));
	}
      else XmTextSetString(prf->text, "error");
    }
}

/* ---------------- fft-log-magnitude ---------------- */

static void log_magnitude_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  in_set_fft_log_magnitude(XmToggleButtonGetState(prf->toggle) == XmSET);
}

/* ---------------- fft-log-frequency ---------------- */

static void log_frequency_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  in_set_fft_log_frequency(XmToggleButtonGetState(prf->toggle) == XmSET);
}




/* ---------------- preferences dialog ---------------- */

void start_preferences_dialog(void)
{
  Arg args[20];
  int n;
  Widget scroller, topics, current_sep;
  char *str;
  prefs_info *prf;

  if (preferences_dialog) return;

  /* -------- base buttons -------- */
  {
    XmString title, help, reset, save, dismiss;
    Widget reset_button;

    title = XmStringCreate(_("Preferences"), XmFONTLIST_DEFAULT_TAG);
    help = XmStringCreate(_("Help"), XmFONTLIST_DEFAULT_TAG);
    reset = XmStringCreate(_("Reset"), XmFONTLIST_DEFAULT_TAG);
    save = XmStringCreate(_("Save"), XmFONTLIST_DEFAULT_TAG);
    dismiss = XmStringCreate(_("Dismiss"), XmFONTLIST_DEFAULT_TAG);

    n = 0;
    if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
    XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
    XtSetArg(args[n], XmNnoResize, false); n++;
    XtSetArg(args[n], XmNtransient, false); n++;
    XtSetArg(args[n], XmNcancelLabelString, save); n++;
    XtSetArg(args[n], XmNhelpLabelString, help); n++;
    XtSetArg(args[n], XmNokLabelString, dismiss); n++;
    XtSetArg(args[n], XmNdialogTitle, title); n++;
    XtSetArg(args[n], XmNallowShellResize, true); n++;
    XtSetArg(args[n], XmNautoUnmanage, false); n++;
    preferences_dialog = XmCreateTemplateDialog(MAIN_PANE(ss), "preferences", args, n);

    n = 0;
    if (!(ss->using_schemes)) 
      {
	XtSetArg(args[n], XmNbackground, ss->sgx->reset_button_color); n++;
	XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;
      }
    reset_button = XtCreateManagedWidget(_("Reset"), xmPushButtonGadgetClass, preferences_dialog, args, n);

    XtAddCallback(preferences_dialog, XmNcancelCallback, preferences_save_callback, NULL);
    XtAddCallback(preferences_dialog, XmNhelpCallback, preferences_help_callback, NULL);
    XtAddCallback(preferences_dialog, XmNokCallback, preferences_quit_callback, NULL);
    XtAddCallback(reset_button, XmNactivateCallback, preferences_reset_callback, NULL);
    
    XmStringFree(title);
    XmStringFree(help);
    XmStringFree(save);
    XmStringFree(dismiss);
    XmStringFree(reset);
    
    if (!(ss->using_schemes))
      {
	map_over_children(preferences_dialog, set_main_color_of_widget, NULL);
	XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->sgx->pushed_button_color, NULL);
	XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->sgx->pushed_button_color, NULL);
	XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->sgx->pushed_button_color, NULL);
	XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->doit_button_color,   NULL);
	XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_OK_BUTTON),     XmNbackground, ss->sgx->quit_button_color,   NULL);
	XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->sgx->help_button_color,   NULL);
      }
    
    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNbottomWidget, XmMessageBoxGetChild(preferences_dialog, XmDIALOG_SEPARATOR)); n++;
    scroller = XmCreateScrolledWindow(preferences_dialog, "pref-scroller", args, n);
    
    XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
    n = attach_all_sides(args, 0);
    XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
    topics = XtCreateManagedWidget("pref-topics", xmRowColumnWidgetClass, scroller, args, n);
    XtVaSetValues(scroller,
		  XmNworkWindow, topics, 
		  NULL);
  }

  /* -------- display -------- */
  {
    Widget dpy_frame, dpy_box, dpy_label;

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    dpy_frame = XtCreateManagedWidget("pref-dpy-frame", xmFrameWidgetClass, topics, args, n);

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
    dpy_box = XtCreateManagedWidget("pref-dpy", xmFormWidgetClass, dpy_frame, args, n);

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->light_blue); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
    dpy_label = XtCreateManagedWidget("display", xmLabelWidgetClass, dpy_box, args, n);

    prf = prefs_row_with_toggle("resize main window as sounds open and close (" S_auto_resize "):", S_auto_resize,
				auto_resize(ss), 
				dpy_box, dpy_label, 
				resize_toggle);
    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("include y=0 line in sound graphs (" S_show_y_zero "):", S_show_y_zero,
				show_y_zero(ss),
				dpy_box, current_sep,
				y_zero_toggle);
    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("include a grid in sound graphs (" S_show_grid "):", S_show_grid,
				(show_grid(ss) == WITH_GRID),
				dpy_box, current_sep,
				grid_toggle);
    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_scale("grid density (" S_grid_density "):", S_grid_density, grid_density(ss),
			       dpy_box, current_sep,
			       grid_density_scale_callback, grid_density_text_callback);

  }

  current_sep = make_inter_topic_separator(topics);

  /* -------- transform -------- */
  {
    Widget fft_frame, fft_box, fft_label;

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    fft_frame = XtCreateManagedWidget("pref-fft-frame", xmFrameWidgetClass, topics, args, n);

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
    fft_box = XtCreateManagedWidget("pref-fft", xmFormWidgetClass, fft_frame, args, n);

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->light_blue); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
    fft_label = XtCreateManagedWidget("transforms", xmLabelWidgetClass, fft_box, args, n);

    str = mus_format(OFF_TD, transform_size(ss));
    prf = prefs_row_with_number("size (" S_transform_size "):", S_transform_size,
				str, 12, 
				fft_box, fft_label, 
				fft_size_up, fft_size_down, fft_size_from_text);
    FREE(str);
    current_sep = make_inter_variable_separator(fft_box, prf->label);
    prf = prefs_row_with_completed_list("data window (" S_fft_window "):", S_fft_window, fft_windows[(int)fft_window(ss)],
					fft_windows, NUM_FFT_WINDOWS,
					fft_box, current_sep,
					fft_window_from_text,
					fft_window_completer, NULL,
					fft_window_from_menu);
    current_sep = make_inter_variable_separator(fft_box, prf->label);
    prf = prefs_row_with_scale("data window family parameter (" S_fft_window_beta "):", S_fft_window_beta, fft_window_beta(ss),
			       fft_box, current_sep,
			       fft_window_beta_scale_callback, fft_window_beta_text_callback);
    current_sep = make_inter_variable_separator(fft_box, prf->label);
    prf = prefs_row_with_toggle("y axis as log magnitude (" S_fft_log_magnitude "):", S_fft_log_magnitude,
				fft_log_magnitude(ss),
				fft_box, current_sep,
				log_magnitude_toggle);
    current_sep = make_inter_variable_separator(fft_box, prf->label);
    prf = prefs_row_with_toggle("x axis as log freq (" S_fft_log_frequency "):", S_fft_log_frequency,
				fft_log_frequency(ss),
				fft_box, current_sep,
				log_frequency_toggle);

    
  }
  
  XtManageChild(scroller);
  XtManageChild(preferences_dialog);
  set_dialog_widget(PREFERENCES_DIALOG, preferences_dialog);

}

#if 0

hooks?
locale?

  ss->Minibuffer_History_Length = DEFAULT_MINIBUFFER_HISTORY_LENGTH;
  ss->Transform_Graph_Type = DEFAULT_TRANSFORM_GRAPH_TYPE;
  ss->Sinc_Width = DEFAULT_SINC_WIDTH;
  ss->Default_Output_Chans = DEFAULT_OUTPUT_CHANS;
  ss->Default_Output_Srate = DEFAULT_OUTPUT_SRATE;
  ss->Default_Output_Header_Type = DEFAULT_OUTPUT_HEADER_TYPE;
  ss->Default_Output_Data_Format = DEFAULT_OUTPUT_DATA_FORMAT;
  ss->Graphs_Horizontal = DEFAULT_GRAPHS_HORIZONTAL;
  ss->Color_Cutoff = DEFAULT_COLOR_CUTOFF;
  ss->Color_Scale = DEFAULT_COLOR_SCALE;
  ss->Color_Inverted = DEFAULT_COLOR_INVERTED;
  ss->Zero_Pad = DEFAULT_ZERO_PAD;
  ss->Ask_Before_Overwrite = DEFAULT_ASK_BEFORE_OVERWRITE;
  ss->X_Axis_Style = DEFAULT_X_AXIS_STYLE;
  ss->Beats_Per_Minute = DEFAULT_BEATS_PER_MINUTE;
  ss->Beats_Per_Measure = DEFAULT_BEATS_PER_MEASURE;
  ss->Time_Graph_Type = DEFAULT_TIME_GRAPH_TYPE;
  ss->Wavo_Hop = DEFAULT_WAVO_HOP;
  ss->Wavo_Trace = DEFAULT_WAVO_TRACE;
  ss->Spectro_Hop = DEFAULT_SPECTRO_HOP;
  ss->Spectro_X_Scale = DEFAULT_SPECTRO_X_SCALE;
  ss->Spectro_Y_Scale = DEFAULT_SPECTRO_Y_SCALE;
  ss->Spectro_Z_Scale = DEFAULT_SPECTRO_Z_SCALE;
  ss->Spectro_Z_Angle = DEFAULT_SPECTRO_Z_ANGLE;
  ss->Spectro_X_Angle = DEFAULT_SPECTRO_X_ANGLE;
  ss->Spectro_Y_Angle = DEFAULT_SPECTRO_Y_ANGLE;
  ss->Color_Map = DEFAULT_COLOR_MAP;
  ss->Color_Map_Size = DEFAULT_COLOR_MAP_SIZE;
  ss->Spectro_Cutoff = DEFAULT_SPECTRO_CUTOFF;
  ss->Spectro_Start = DEFAULT_SPECTRO_START;
  ss->Wavelet_Type = DEFAULT_WAVELET_TYPE;
  ss->Transform_Type = DEFAULT_TRANSFORM_TYPE;
  ss->Show_Selection_Transform = DEFAULT_SHOW_SELECTION_TRANSFORM;
  ss->With_Mix_Tags = DEFAULT_WITH_MIX_TAGS;
  ss->With_Relative_Panes = DEFAULT_WITH_RELATIVE_PANES;
  ss->With_GL = DEFAULT_WITH_GL;
  ss->With_Background_Processes = DEFAULT_WITH_BACKGROUND_PROCESSES;
  ss->Dot_Size = DEFAULT_DOT_SIZE;
  ss->Cursor_Size = DEFAULT_CURSOR_SIZE;
  ss->Cursor_Style = DEFAULT_CURSOR_STYLE;
  ss->cursor_proc = XEN_UNDEFINED;
  ss->cursor_proc_loc = NOT_A_GC_LOC;
  ss->Vu_Size = DEFAULT_VU_SIZE;
  ss->Vu_Font_Size = DEFAULT_VU_FONT_SIZE;
  if (DEFAULT_VU_FONT != (char *)NULL) ss->Vu_Font = copy_string(DEFAULT_VU_FONT); else ss->Vu_Font = NULL;
  ss->Selection_Creates_Region = DEFAULT_SELECTION_CREATES_REGION;
  ss->Transform_Normalization = DEFAULT_TRANSFORM_NORMALIZATION;
  ss->Zoom_Focus_Style = DEFAULT_ZOOM_FOCUS_STYLE;
  ss->zoom_focus_proc = XEN_UNDEFINED;
  ss->zoom_focus_proc_loc = NOT_A_GC_LOC;
  ss->Graph_Style = DEFAULT_GRAPH_STYLE;
  ss->Region_Graph_Style = DEFAULT_GRAPH_STYLE;
  ss->Enved_Filter_Order = DEFAULT_ENVED_FILTER_ORDER;
  ss->Verbose_Cursor = DEFAULT_VERBOSE_CURSOR;
  if (DEFAULT_TEMP_DIR != (char *)NULL) ss->Temp_Dir = copy_string(DEFAULT_TEMP_DIR); else ss->Temp_Dir = NULL;
  if (DEFAULT_SAVE_DIR != (char *)NULL) ss->Save_Dir = copy_string(DEFAULT_SAVE_DIR); else ss->Save_Dir = NULL;
  if (DEFAULT_LADSPA_DIR != (char *)NULL) ss->Ladspa_Dir = copy_string(DEFAULT_LADSPA_DIR); else ss->Ladspa_Dir = NULL;
  if (DEFAULT_EPS_FILE != (char *)NULL) ss->Eps_File = copy_string(DEFAULT_EPS_FILE); else ss->Eps_File = NULL;
  ss->Eps_Bottom_Margin = DEFAULT_EPS_BOTTOM_MARGIN;
  ss->Eps_Left_Margin = DEFAULT_EPS_LEFT_MARGIN;
  ss->Eps_Size = DEFAULT_EPS_SIZE;
  ss->Listener_Prompt = copy_string(DEFAULT_LISTENER_PROMPT);
  ss->Show_Transform_Peaks = DEFAULT_SHOW_TRANSFORM_PEAKS;
  ss->Show_Sonogram_Cursor = DEFAULT_SHOW_SONOGRAM_CURSOR;
  ss->Show_Axes = DEFAULT_SHOW_AXES;
  ss->Show_Marks = DEFAULT_SHOW_MARKS;
  ss->Show_Indices = DEFAULT_SHOW_INDICES;
  ss->Show_Backtrace = DEFAULT_SHOW_BACKTRACE;
  ss->Data_Clipped = DEFAULT_DATA_CLIPPED;
  ss->Channel_Style = DEFAULT_CHANNEL_STYLE;
  ss->Sound_Style = DEFAULT_SOUND_STYLE;
  ss->Audio_Input_Device = DEFAULT_AUDIO_INPUT_DEVICE;
  ss->Audio_Output_Device = DEFAULT_AUDIO_OUTPUT_DEVICE;
  ss->Optimization = DEFAULT_OPTIMIZATION;
  ss->Print_Length = DEFAULT_PRINT_LENGTH;
  ss->View_Files_Sort = DEFAULT_VIEW_FILES_SORT;
  ss->Show_Mix_Waveforms = DEFAULT_SHOW_MIX_WAVEFORMS;
  ss->Mix_Waveform_Height = DEFAULT_MIX_WAVEFORM_HEIGHT;
  ss->Mix_Tag_Width = DEFAULT_MIX_TAG_WIDTH;
  ss->Mix_Tag_Height = DEFAULT_MIX_TAG_HEIGHT;
  ss->Mark_Tag_Width = DEFAULT_MARK_TAG_WIDTH;
  ss->Mark_Tag_Height = DEFAULT_MARK_TAG_HEIGHT;
  if (DEFAULT_SAVE_STATE_FILE != (char *)NULL) ss->Save_State_File = copy_string(DEFAULT_SAVE_STATE_FILE); else ss->Save_State_File = NULL;
  ss->Enved_Base = DEFAULT_ENVED_BASE;
  ss->Enved_Power = DEFAULT_ENVED_POWER;
  ss->Enved_Wave_p = DEFAULT_ENVED_WAVE_P;
  ss->Enved_Style = DEFAULT_ENVED_STYLE;
  ss->Enved_Target = DEFAULT_ENVED_TARGET;
  ss->Dac_Size = DEFAULT_DAC_SIZE;
  ss->Dac_Combines_Channels = DEFAULT_DAC_COMBINES_CHANNELS;
  ss->Cursor_Update_Interval = DEFAULT_CURSOR_UPDATE_INTERVAL;
  ss->Cursor_Location_Offset = DEFAULT_CURSOR_LOCATION_OFFSET;
  ss->Max_Regions = DEFAULT_MAX_REGIONS;
  ss->Max_Transform_Peaks = DEFAULT_MAX_TRANSFORM_PEAKS;
  ss->HTML_Dir = NULL;
  ss->HTML_Program = copy_string(DEFAULT_HTML_PROGRAM);
  ss->Log_Freq_Start = DEFAULT_LOG_FREQ_START;
  ss->Min_dB = DEFAULT_MIN_DB;
  ss->lin_dB = pow(10.0, DEFAULT_MIN_DB * 0.05);

  ss->Expand_Control_Min = DEFAULT_EXPAND_CONTROL_MIN;
  ss->Expand_Control_Max = DEFAULT_EXPAND_CONTROL_MAX;
  ss->Amp_Control_Min = DEFAULT_AMP_CONTROL_MIN;
  ss->Amp_Control_Max = DEFAULT_AMP_CONTROL_MAX;
  ss->Speed_Control_Min = DEFAULT_SPEED_CONTROL_MIN;
  ss->Speed_Control_Max = DEFAULT_SPEED_CONTROL_MAX;
  ss->Contrast_Control_Min = DEFAULT_CONTRAST_CONTROL_MIN;
  ss->Contrast_Control_Max = DEFAULT_CONTRAST_CONTROL_MAX;
  ss->Contrast_Control_Amp = DEFAULT_CONTRAST_CONTROL_AMP;
  ss->Expand_Control_Length = DEFAULT_EXPAND_CONTROL_LENGTH;
  ss->Expand_Control_Ramp = DEFAULT_EXPAND_CONTROL_RAMP;
  ss->Expand_Control_Hop = DEFAULT_EXPAND_CONTROL_HOP;
  ss->Expand_Control_Jitter = DEFAULT_EXPAND_CONTROL_JITTER;
  ss->Reverb_Control_Feedback = DEFAULT_REVERB_CONTROL_FEEDBACK;
  ss->Reverb_Control_Lowpass = DEFAULT_REVERB_CONTROL_LOWPASS;
  ss->Reverb_Control_Scale_Min = DEFAULT_REVERB_CONTROL_SCALE_MIN;
  ss->Reverb_Control_Scale_Max = DEFAULT_REVERB_CONTROL_SCALE_MAX;
  ss->Reverb_Control_Decay = DEFAULT_REVERB_CONTROL_DECAY;
  ss->Speed_Control_Tones = DEFAULT_SPEED_CONTROL_TONES;
  ss->Speed_Control_Style = DEFAULT_SPEED_CONTROL_STYLE;
  ss->Reverb_Control_Length_Min = DEFAULT_REVERB_CONTROL_LENGTH_MIN;
  ss->Reverb_Control_Length_Max = DEFAULT_REVERB_CONTROL_LENGTH_MAX;
  ss->Filter_Control_Order = DEFAULT_FILTER_CONTROL_ORDER;
  ss->Filter_Control_In_Db = DEFAULT_FILTER_CONTROL_IN_DB;
  ss->Filter_Control_In_Hz = DEFAULT_FILTER_CONTROL_IN_HZ;
  ss->Tempo_Control_Min = DEFAULT_TEMPO_CONTROL_MIN;
  ss->Tempo_Control_Max = DEFAULT_TEMPO_CONTROL_MAX;
  ss->Show_Controls = DEFAULT_SHOW_CONTROLS;
  ss->Cursor_Follows_Play = DEFAULT_CURSOR_FOLLOWS_PLAY;
  ss->Just_Sounds = DEFAULT_JUST_SOUNDS;
#endif
