#include "snd.h"

/* preferences dialog; layout design taken from webmail
 *
 * also add an html-style table of contents at top -- basically mimic a web-page
 */


/* start automatically if no .snd? -- create empty if not wanted?
 *  color choice via 4 sliders + extra line
 *  font choice via some spinners + text + pull-down menus and extra line if needed
 */

/* reflection (no callbacks)
 */


static Widget preferences_dialog = NULL;
static bool prefs_helping = false;

#define MID_POSITION 61
#define COLOR_POSITION 75
#define RED_POSITION 83
#define GREEN_POSITION 91

#define MID_SPACE 16
#define INTER_TOPIC_SPACE 2
#define INTER_VARIABLE_SPACE 2

#define HELP_WAIT_TIME 500
#define POWER_WAIT_TIME 100
#define POWER_INITIAL_WAIT_TIME 500
#define ERROR_WAIT_TIME 1000

#define STARTUP_WIDTH 940
#define STARTUP_HEIGHT 600


typedef struct prefs_info {
  Widget label, text, arrow_up, arrow_down, arrow_right, error, toggle, scale, color, rscl, gscl, bscl, rtxt, gtxt, btxt, list_menu;
  bool got_error;
  XtIntervalId help_id, power_id;
  const char *var_name;
  const char **values;
  int num_values;
  Float scale_max;
  void (*toggle_func)(struct prefs_info *prf);
  void (*scale_func)(struct prefs_info *prf);
  void (*arrow_up_func)(struct prefs_info *prf);
  void (*arrow_down_func)(struct prefs_info *prf);
  void (*text_func)(struct prefs_info *prf);
  void (*list_func)(struct prefs_info *prf, char *value);
  void (*color_func)(struct prefs_info *prf, float r, float g, float b);
  void (*reflect_func)(struct prefs_info *prf);
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
					 void (*toggle_func)(prefs_info *prf),
					 void (*reflect_func)(prefs_info *prf))
{
  Arg args[20];
  int n;
  prefs_info *prf = NULL;
  Widget sep;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->reflect_func = reflect_func;

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

static void float_to_textfield(Widget w, Float val)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(w), w);
  str = (char *)CALLOC(12, sizeof(char));
  mus_snprintf(str, 12, "%.3f", val);
  XmTextFieldSetString(w, str);
  FREE(str);
}

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
					void (*text_func)(prefs_info *prf),
					void (*reflect_func)(prefs_info *prf))
{
  Arg args[20];
  int n;
  prefs_info *prf = NULL;
  Widget sep;
  XtCallbackList n1, n2;
  char *str;

  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->scale_max = max_val;
  prf->reflect_func = reflect_func;

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
  /* XtSetArg(args[n], XmNdecimalPoints, 2); n++; */
  XtSetArg(args[n], XmNvalue, (int)(100 * current_value / max_val)); n++;
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


/* ---------------- text row ---------------- */

static void call_text_func(Widget w, XtPointer context, XtPointer info) 
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->text_func))
    (*(prf->text_func))(prf);
}

static prefs_info *prefs_row_with_text(const char *label, const char *varname, const char *value,
				       Widget box, Widget top_widget,
				       void (*text_func)(prefs_info *prf),
				       void (*reflect_func)(prefs_info *prf))
{
  Arg args[20];
  int n;
  prefs_info *prf = NULL;
  Widget sep;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->reflect_func = reflect_func;

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
  
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, sep); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNvalue, value); n++;
  XtSetArg(args[n], XmNmarginHeight, 1); n++;
  XtSetArg(args[n], XmNborderWidth, 0); n++;
  XtSetArg(args[n], XmNborderColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNbottomShadowColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNtopShadowColor, ss->sgx->white); n++;
  prf->text = make_textfield_widget("text", box, args, n, ACTIVATABLE_BUT_NOT_FOCUSED, NO_COMPLETER);
  
  prf->text_func = text_func;
  XtAddCallback(prf->text, XmNactivateCallback, call_text_func, (XtPointer)prf);

  XtAddEventHandler(prf->label, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->text, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->label, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->text, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);

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

static prefs_info *prefs_row_with_number(const char *label, const char *varname, const char *value, int cols,
					 Widget box, Widget top_widget,
 					 void (*arrow_up_func)(prefs_info *prf), void (*arrow_down_func)(prefs_info *prf), 
					 void (*text_func)(prefs_info *prf),
					 void (*reflect_func)(prefs_info *prf))
{
  Arg args[20];
  int n;
  prefs_info *prf = NULL;
  Widget sep;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->reflect_func = reflect_func;

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

static void add_item_to_list_menu(prefs_info *prf, char *item)
{
  Widget tmp;
  int n;
  Arg args[20];
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  tmp = XtCreateManagedWidget(item,  xmPushButtonWidgetClass, prf->list_menu, args, n);
  XtAddCallback(tmp, XmNactivateCallback, prefs_list_callback, make_list_entry(prf, item));
}

static prefs_info *prefs_row_with_completed_list(const char *label, const char *varname, const char *value,
						 const char **values, int num_values,
						 Widget box, Widget top_widget,
						 void (*text_func)(prefs_info *prf),
						 char *(*completion_func)(char *text, void *context), void *completion_context,
						 void (*list_func)(prefs_info *prf, char *value),
						 void (*reflect_func)(prefs_info *prf))
{
  Arg args[20];
  int n, i, cols = 0;
  prefs_info *prf = NULL;
  Widget sep, sbar;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->reflect_func = reflect_func;

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

  /* SOMEDAY: figure out how to get a pixmap of an arrow in this damned widget */
  /* SOMEDAY: center the pulldown menu on the text widget and on the current position */
      
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  for (i = 0; i < num_values; i++)
    if (values[i])
      {
	Widget tmp;
	tmp = XtCreateManagedWidget(values[i],  xmPushButtonWidgetClass, prf->list_menu, args, n);
	XtAddCallback(tmp, XmNactivateCallback, prefs_list_callback, make_list_entry(prf, (char *)values[i]));
      }

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


/* ---------------- color selector row(s) ---------------- */

static XColor *rgb_to_color(Float r, Float g, Float b)
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

  current_color = rgb_to_color(ir / 100.0, ig / 100.0, ib / 100.0);
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

static void prefs_r_callback(Widget w, XtPointer context, XtPointer info)
{
  prefs_info *prf = (prefs_info *)context;
  char *str;
  float r = 0.0;
  str = XmTextFieldGetString(w);
  sscanf(str, "%f", &r);
  if ((r >= 0.0) &&
      (r <= 1.0))
    {
      XmScaleSetValue(prf->rscl, (int)(100 * r));
      reflect_color(prf);
    }
  else XmTextFieldSetString(w, "err");
  if (str) XtFree(str);
}

static void prefs_g_callback(Widget w, XtPointer context, XtPointer info)
{
  prefs_info *prf = (prefs_info *)context;
  char *str;
  float r = 0.0;
  str = XmTextFieldGetString(w);
  sscanf(str, "%f", &r);
  if ((r >= 0.0) &&
      (r <= 1.0))
    {
      XmScaleSetValue(prf->gscl, (int)(100 * r));
      reflect_color(prf);
    }
  else XmTextFieldSetString(w, "err");
  if (str) XtFree(str);
}

static void prefs_b_callback(Widget w, XtPointer context, XtPointer info)
{
  prefs_info *prf = (prefs_info *)context;
  char *str;
  float r = 0.0;
  str = XmTextFieldGetString(w);
  sscanf(str, "%f", &r);
  if ((r >= 0.0) &&
      (r <= 1.0))
    {
      XmScaleSetValue(prf->bscl, (int)(100 * r));
      reflect_color(prf);
    }
  else XmTextFieldSetString(w, "err");
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

static prefs_info *prefs_color_selector_row(const char *label, const char *varname, 
					    Pixel current_pixel,
					    Widget box, Widget top_widget,
					    void (*color_func)(prefs_info *prf, float r, float g, float b),
					    void (*reflect_func)(prefs_info *prf))
{
  Arg args[20];
  int n;
  prefs_info *prf = NULL;
  Widget sep, sep1, frame;
  XtCallbackList n1;
  Pixel red, green, blue;
  XColor *tmp;
  float r = 0.0, g = 0.0, b = 0.0;

  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->reflect_func = reflect_func;
  pixel_to_rgb(current_pixel, &r, &g, &b);
  tmp = rgb_to_color(1.0, 0.0, 0.0);
  red = tmp->pixel;
  FREE(tmp);
  tmp = rgb_to_color(0.0, 1.0, 0.0);
  green = tmp->pixel;
  FREE(tmp);
  tmp = rgb_to_color(0.0, 0.0, 1.0);
  blue = tmp->pixel;
  FREE(tmp);

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

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, prf->color); n++;
  XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
  XtSetArg(args[n], XmNwidth, 8); n++;
  XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
  sep1 = XtCreateManagedWidget("sep1", xmSeparatorWidgetClass, box, args, n);
  
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, sep1); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
  XtSetArg(args[n], XmNrightPosition, RED_POSITION); n++;
  XtSetArg(args[n], XmNmarginHeight, 1); n++;
  XtSetArg(args[n], XmNborderWidth, 0); n++;
  XtSetArg(args[n], XmNborderColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNbottomShadowColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNtopShadowColor, ss->sgx->white); n++;
  prf->rtxt = make_textfield_widget("text", box, args, n, ACTIVATABLE_BUT_NOT_FOCUSED, NO_COMPLETER);
  float_to_textfield(prf->rtxt, r);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, prf->rtxt); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
  XtSetArg(args[n], XmNrightPosition, GREEN_POSITION); n++;
  XtSetArg(args[n], XmNmarginHeight, 1); n++;
  XtSetArg(args[n], XmNborderWidth, 0); n++;
  XtSetArg(args[n], XmNborderColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNbottomShadowColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNtopShadowColor, ss->sgx->white); n++;
  prf->gtxt = make_textfield_widget("text", box, args, n, ACTIVATABLE_BUT_NOT_FOCUSED, NO_COMPLETER);
  float_to_textfield(prf->gtxt, g);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, prf->gtxt); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNmarginHeight, 1); n++;
  XtSetArg(args[n], XmNborderWidth, 0); n++;
  XtSetArg(args[n], XmNborderColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNbottomShadowColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNtopShadowColor, ss->sgx->white); n++;
  prf->btxt = make_textfield_widget("text", box, args, n, ACTIVATABLE_BUT_NOT_FOCUSED, NO_COMPLETER);
  float_to_textfield(prf->btxt, b);

  /* 2nd row = 3 scales */
  n1 = make_callback_list(prefs_color_callback, (XtPointer)prf);
  
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, prf->label); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
  XtSetArg(args[n], XmNrightPosition, 33); n++;
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
  XtSetArg(args[n], XmNrightPosition, 66); n++;
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
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
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

  XtAddEventHandler(prf->label, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->label, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->color, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->color, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->rtxt, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->rtxt, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->rscl, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->rscl, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->gtxt, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->gtxt, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->gscl, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->gscl, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->btxt, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->btxt, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->bscl, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->bscl, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);

  prf->color_func = color_func;
  FREE(n1);
  return(prf);
}

  /* TODO: font selector row
   */

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

/* ---------------- ask-before-overwrite ---------------- */

static void overwrite_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  set_ask_before_overwrite(XmToggleButtonGetState(prf->toggle) == XmSET);
}

/* ---------------- show-controls ---------------- */

static void controls_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  in_set_show_controls(ss, XmToggleButtonGetState(prf->toggle) == XmSET);
}

/* ---------------- verbose-cursor ---------------- */

static void verbose_cursor_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  in_set_verbose_cursor(XmToggleButtonGetState(prf->toggle) == XmSET);
}

/* ---------------- cursor-size ---------------- */

#define MIN_CURSOR_SIZE 1
#define MAX_CURSOR_SIZE 500

static void cursor_size_up(prefs_info *prf)
{
  int size;
  char *new_size;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  size = cursor_size(ss) + 1;
  if (size >= MAX_CURSOR_SIZE) XtSetSensitive(prf->arrow_up, false);
  if (size > MIN_CURSOR_SIZE) XtSetSensitive(prf->arrow_down, true);
  in_set_cursor_size(size);
  new_size = mus_format("%d", cursor_size(ss));
  XmTextSetString(prf->text, new_size);
  FREE(new_size);
}

static void cursor_size_down(prefs_info *prf)
{
  int size;
  char *new_size;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  size = cursor_size(ss) - 1;
  if (size <= MIN_CURSOR_SIZE) XtSetSensitive(prf->arrow_down, false);
  if (size < MAX_CURSOR_SIZE) XtSetSensitive(prf->arrow_up, true);
  in_set_cursor_size(size);
  new_size = mus_format("%d", cursor_size(ss));
  XmTextSetString(prf->text, new_size);
  FREE(new_size);
}

static void cursor_size_from_text(prefs_info *prf)
{
  int size;
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((str) && (*str))
    {
      prf->got_error = false;
      redirect_errors_to(post_prefs_error, (void *)prf);
      size = string_to_int(str, 0, "cursor size"); 
      redirect_errors_to(NULL, NULL);
      XtFree(str);
      if (!(prf->got_error))
	{
	  if (size >= MIN_CURSOR_SIZE)
	    {
	      if (size <= MAX_CURSOR_SIZE)
		in_set_cursor_size(size);
	      else va_post_prefs_error("%s > %d?", (void *)prf, str, MAX_CURSOR_SIZE);
	    }
	  else va_post_prefs_error("%s < %d?", (void *)prf, str, MIN_CURSOR_SIZE);
	}
      else prf->got_error = false;
    }
  else post_prefs_error("no size?", (void *)prf);
}



/* ---------------- temp-dir ---------------- */

static bool local_access(char *dir)
{
  int err;
  char *temp;
  temp = shorter_tempnam(dir, "snd_");
  err = mus_file_create(temp);
  if (err != -1)
    {
      snd_close(err, temp);
      snd_remove(temp, IGNORE_CACHE);
    }
  FREE(temp);
  return(err != -1);
}

static void temp_dir_error_erase_func(XtPointer context, XtIntervalId *id)
{
  prefs_info *prf = (prefs_info *)context;
  XmTextSetString(prf->text, temp_dir(ss));
}

static void temp_dir_text(prefs_info *prf)
{
  char *str, *dir = NULL;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((!str) || (!(*str))) 
    dir = DEFAULT_TEMP_DIR;
  else dir = str;
  if (local_access(dir))
    {
      if (temp_dir(ss)) FREE(temp_dir(ss));
      set_temp_dir(copy_string(dir));
    }
  else
    {
      XmTextSetString(prf->text, "can't access that directory");
      XtAppAddTimeOut(MAIN_APP(ss),
		      ERROR_WAIT_TIME,
		      temp_dir_error_erase_func,
		      (XtPointer)prf);
    }
  if (str) XtFree(str);
}

#if HAVE_EXTENSION_LANGUAGE
/* ---------------- save-dir ---------------- */

static void save_dir_error_erase_func(XtPointer context, XtIntervalId *id)
{
  prefs_info *prf = (prefs_info *)context;
  XmTextSetString(prf->text, save_dir(ss));
}

static void save_dir_text(prefs_info *prf)
{
  char *str, *dir = NULL;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((!str) || (!(*str))) 
    dir = DEFAULT_SAVE_DIR;
  else dir = str;
  if (local_access(dir))
    {
      if (save_dir(ss)) FREE(save_dir(ss));
      set_save_dir(copy_string(dir));
    }
  else
    {
      XmTextSetString(prf->text, "can't access that directory");
      XtAppAddTimeOut(MAIN_APP(ss),
		      ERROR_WAIT_TIME,
		      save_dir_error_erase_func,
		      (XtPointer)prf);
    }
  if (str) XtFree(str);
}
#endif


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
  in_set_grid_density(val * prf->scale_max / 100.0);
}

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
	  (value <= prf->scale_max))
	{
	  in_set_grid_density(value);
	  XmScaleSetValue(prf->scale, (int)(100 * value / prf->scale_max));
	}
      else XmTextSetString(prf->text, "error");
    }
}

/* ---------------- data-color ---------------- */

static void data_color_func(prefs_info *prf, float r, float g, float b)
{
  XColor *tmp;
  tmp = rgb_to_color(r, g, b);
  color_data(tmp->pixel);
  FREE(tmp);
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

static char *trim_string(const char *str)
{
  int i, len, j = 0;
  char *trimmed_str;
  len = strlen(str);
  trimmed_str = (char *)CALLOC(len + 1, sizeof(char));
  for (i = 0; i < len; i++)
    if (!(isspace(str[i])))
      trimmed_str[j++] = str[i];
  return(trimmed_str);
}

static void fft_window_from_text(prefs_info *prf)
{
  int i;
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((str) && (*str))
    {
      char *trimmed_str;
      trimmed_str = trim_string(str);
      XtFree(str);
      if (snd_strlen(trimmed_str) > 0)
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
      FREE(trimmed_str);
    }
  else post_prefs_error("no window?", (void *)prf);
}

/* ---------------- fft-window-beta ---------------- */

static void fft_window_beta_scale_callback(prefs_info *prf)
{
  int val = 0;
  ASSERT_WIDGET_TYPE(XmIsScale(prf->scale), prf->scale);
  XmScaleGetValue(prf->scale, &val);
  in_set_fft_window_beta(val * prf->scale_max / 100.0);
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
	  (value <= prf->scale_max))
	{
	  in_set_fft_window_beta(value);
	  XmScaleSetValue(prf->scale, (int)(100 * value / prf->scale_max));
	}
      else XmTextSetString(prf->text, "error");
    }
}

/* ---------------- show-transform-peaks ---------------- */

static void transform_peaks_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  in_set_show_transform_peaks(XmToggleButtonGetState(prf->toggle) == XmSET);
}


/* ---------------- colormap ---------------- */

static char *colormap_completer(char *text, void *data)
{
  list_completer_info *compinfo;
  char **cmaps;
  int i, len;
  char *result;
  len = num_colormaps();
  cmaps = (char **)CALLOC(len, sizeof(char *));
  for (i = 0; i < len; i++)
    cmaps[i] = colormap_name(i);
  compinfo = (list_completer_info *)CALLOC(1, sizeof(list_completer_info));
  compinfo->exact_match = false;
  compinfo->values = (char **)fft_windows;
  compinfo->num_values = len;
  compinfo->values_size = len;
  result = list_completer(text, (void *)compinfo);
  FREE(cmaps);
  return(result);
}

static void colormap_from_text(prefs_info *prf)
{
  int i;
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((str) && (*str))
    {
      char *trimmed_str;
      trimmed_str = trim_string(str);
      XtFree(str);
      if (snd_strlen(trimmed_str) > 0)
	{
	  int len, curpos = -1;
	  len = num_colormaps();
	  for (i = 0; i < len; i++)
	    if (STRCMP(trimmed_str, colormap_name(i)) == 0)
	      {
		curpos = i;
		break;
	      }
	  if (curpos >= 0)
	    in_set_color_map(curpos);
	  else post_prefs_error("unknown colormap", (void *)prf);
	}
      else post_prefs_error("no colormap?", (void *)prf);
      FREE(trimmed_str);
    }
  else post_prefs_error("no colormap?", (void *)prf);
}

static void colormap_from_menu(prefs_info *prf, char *value)
{
  int i, len;
  len = num_colormaps();
  for (i = 0; i < len; i++)
    if (strcmp(value, colormap_name(i)) == 0)
      {
	in_set_color_map(i);
	XmTextFieldSetString(prf->text, value);
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


/* ---------------- mark-color ---------------- */

static void mark_color_func(prefs_info *prf, float r, float g, float b)
{
  XColor *tmp;
  tmp = rgb_to_color(r, g, b);
  color_marks(tmp->pixel);
  FREE(tmp);
}


#define THIS_NEEDS_REFLECTION NULL


/* ---------------- preferences dialog ---------------- */

void start_preferences_dialog(void)
{
  Arg args[20];
  int n;
  Widget scroller, topics, current_sep;
  char *str;
  prefs_info *prf;

  if (preferences_dialog) return; /* TODO: else reflection, also no save if no extlang */

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
    XtSetArg(args[n], XmNwidth, STARTUP_WIDTH); n++;
    XtSetArg(args[n], XmNheight, STARTUP_HEIGHT); n++;
    preferences_dialog = XmCreateTemplateDialog(MAIN_PANE(ss), "preferences", args, n);

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->reset_button_color); n++;
    XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;
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
    
    map_over_children(preferences_dialog, set_main_color_of_widget, NULL);
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

  /* a table of contents at the start -- click to jump or perhaps a specialized popup menu for it or a pulldown menu */

  /* ---------------- overall behavior ---------------- */

  {
    Widget dpy_frame, dpy_box, dpy_label, file_label, cursor_label;
    char *str;

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
    dpy_label = XtCreateManagedWidget("overall behavior choices", xmLabelWidgetClass, dpy_box, args, n);

    prf = prefs_row_with_toggle("ask before overwriting anything (" S_ask_before_overwrite "):", S_ask_before_overwrite,
				ask_before_overwrite(ss), 
				dpy_box, dpy_label, 
				overwrite_toggle, THIS_NEEDS_REFLECTION);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("resize main window as sounds open and close (" S_auto_resize "):", S_auto_resize,
				auto_resize(ss), 
				dpy_box, current_sep, 
				resize_toggle, THIS_NEEDS_REFLECTION);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("show the control panel upon opening a sound (" S_show_controls "):", S_show_controls,
				in_show_controls(ss), 
				dpy_box, current_sep, 
				controls_toggle, THIS_NEEDS_REFLECTION);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);

    /* basic color, highlight */

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget, current_sep); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
    cursor_label = XtCreateManagedWidget("     cursor options", xmLabelWidgetClass, dpy_box, args, n);

    prf = prefs_row_with_toggle("report cursor location as it moves (" S_verbose_cursor "):", S_verbose_cursor,
				verbose_cursor(ss), 
				dpy_box, cursor_label, 
				verbose_cursor_toggle, THIS_NEEDS_REFLECTION);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    
    str = mus_format("%d", cursor_size(ss));
    prf = prefs_row_with_number("size (" S_cursor_size "):", S_cursor_size,
				str, 4, 
				dpy_box, current_sep,
				cursor_size_up, cursor_size_down, cursor_size_from_text, THIS_NEEDS_REFLECTION);
    FREE(str);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);


    /* cursor follows play, size, style, color? */
    /*
    ss->Cursor_Style = DEFAULT_CURSOR_STYLE;
    cursor-color
    ss->Cursor_Follows_Play = DEFAULT_CURSOR_FOLLOWS_PLAY;

    cursor as line during play? (check for other cursor stuff)
    */

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget, current_sep); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
    file_label = XtCreateManagedWidget("     file locations", xmLabelWidgetClass, dpy_box, args, n);

    prf = prefs_row_with_text("directory for temporary files (" S_temp_dir "):", S_temp_dir, 
			      temp_dir(ss), 
			      dpy_box, file_label,
			      temp_dir_text, THIS_NEEDS_REFLECTION);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
#if HAVE_EXTENSION_LANGUAGE
    prf = prefs_row_with_text("directory for save-state files (" S_save_dir "):", S_save_dir, 
			      save_dir(ss), 
			      dpy_box, current_sep,
			      save_dir_text, THIS_NEEDS_REFLECTION);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
#endif

    /*
    ss->Graphs_Horizontal = DEFAULT_GRAPHS_HORIZONTAL;
    ss->Channel_Style = DEFAULT_CHANNEL_STYLE;
    ss->Sound_Style = DEFAULT_SOUND_STYLE;
    peaks envs?
    mouse->focus?

    initial size

    ss->Show_Indices = DEFAULT_SHOW_INDICES;

    file options?
    ss->Just_Sounds = DEFAULT_JUST_SOUNDS;
    ladspa: if (DEFAULT_LADSPA_DIR != (char *)NULL) ss->Ladspa_Dir = copy_string(DEFAULT_LADSPA_DIR); else ss->Ladspa_Dir = NULL;
    ss->HTML_Dir = NULL;
    ss->HTML_Program = copy_string(DEFAULT_HTML_PROGRAM);

    inner label:
    ss->Default_Output_Chans = DEFAULT_OUTPUT_CHANS;
    ss->Default_Output_Srate = DEFAULT_OUTPUT_SRATE;
    ss->Default_Output_Header_Type = DEFAULT_OUTPUT_HEADER_TYPE;
    ss->Default_Output_Data_Format = DEFAULT_OUTPUT_DATA_FORMAT;


    inner: optional menus and menu items
    marks-menu
    oscope
    etc
    */

  /* general appearance */
  /*
    basic/selection colors
    fonts
    pixmaps for backgrounds?
  */

  }

  current_sep = make_inter_topic_separator(topics);

  /* -------- graphs -------- */
  {
    Widget grf_frame, grf_box, grf_label, colgrf_label;

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    grf_frame = XtCreateManagedWidget("pref-dpy-frame", xmFrameWidgetClass, topics, args, n);

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
    grf_box = XtCreateManagedWidget("pref-dpy", xmFormWidgetClass, grf_frame, args, n);

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->light_blue); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
    grf_label = XtCreateManagedWidget("graph options", xmLabelWidgetClass, grf_box, args, n);

    prf = prefs_row_with_toggle("include y=0 line in sound graphs (" S_show_y_zero "):", S_show_y_zero,
				show_y_zero(ss),
				grf_box, grf_label,
				y_zero_toggle, THIS_NEEDS_REFLECTION);

    current_sep = make_inter_variable_separator(grf_box, prf->label);
    prf = prefs_row_with_toggle("include a grid in sound graphs (" S_show_grid "):", S_show_grid,
				(show_grid(ss) == WITH_GRID),
				grf_box, current_sep,
				grid_toggle, THIS_NEEDS_REFLECTION);

    current_sep = make_inter_variable_separator(grf_box, prf->label);
    prf = prefs_row_with_scale("grid density (" S_grid_density "):", S_grid_density, 
			       2.0, grid_density(ss),
			       grf_box, current_sep,
			       grid_density_scale_callback, grid_density_text_callback, THIS_NEEDS_REFLECTION);

    current_sep = make_inter_variable_separator(grf_box, prf->label);

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget, current_sep); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
    colgrf_label = XtCreateManagedWidget("      graph colors", xmLabelWidgetClass, grf_box, args, n);
    
    prf = prefs_color_selector_row("data color (" S_data_color "):", S_data_color, ss->sgx->data_color,
				   grf_box, colgrf_label,
				   data_color_func, THIS_NEEDS_REFLECTION);


    /*
    ss->X_Axis_Style = DEFAULT_X_AXIS_STYLE;
    ss->With_GL = DEFAULT_WITH_GL;
    ss->Graph_Style = DEFAULT_GRAPH_STYLE;
    ss->Show_Axes = DEFAULT_SHOW_AXES;
    ss->Dot_Size = DEFAULT_DOT_SIZE;
    graph fonts
    graph colors
    cursor color?
    
    */
  }

  current_sep = make_inter_topic_separator(topics);

  /* -------- audio -------- */
  {
    Widget aud_frame, aud_box, aud_label;

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    aud_frame = XtCreateManagedWidget("pref-aud-frame", xmFrameWidgetClass, topics, args, n);

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
    aud_box = XtCreateManagedWidget("pref-aud", xmFormWidgetClass, aud_frame, args, n);

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->light_blue); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
    aud_label = XtCreateManagedWidget("audio options", xmLabelWidgetClass, aud_box, args, n);

    

  /*
    ss->Dac_Size = DEFAULT_DAC_SIZE;
    ss->Dac_Combines_Channels = DEFAULT_DAC_COMBINES_CHANNELS;

    audio output device? or sound card?
    line cursor during play?
  */
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
    fft_label = XtCreateManagedWidget("transform options", xmLabelWidgetClass, fft_box, args, n);

    str = mus_format(OFF_TD, transform_size(ss));
    prf = prefs_row_with_number("size (" S_transform_size "):", S_transform_size,
				str, 12, 
				fft_box, fft_label, 
				fft_size_up, fft_size_down, fft_size_from_text, THIS_NEEDS_REFLECTION);
    FREE(str);
    current_sep = make_inter_variable_separator(fft_box, prf->label);
    prf = prefs_row_with_completed_list("data window (" S_fft_window "):", S_fft_window, fft_windows[(int)fft_window(ss)],
					fft_windows, NUM_FFT_WINDOWS,
					fft_box, current_sep,
					fft_window_from_text,
					fft_window_completer, NULL,
					fft_window_from_menu, THIS_NEEDS_REFLECTION);

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    prf = prefs_row_with_scale("data window family parameter (" S_fft_window_beta "):", S_fft_window_beta, 
			       1.0, fft_window_beta(ss),
			       fft_box, current_sep,
			       fft_window_beta_scale_callback, fft_window_beta_text_callback, THIS_NEEDS_REFLECTION);

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    prf = prefs_row_with_toggle("show fft peak data (" S_show_transform_peaks "):", S_show_transform_peaks,
				show_transform_peaks(ss),
				fft_box, current_sep,
				transform_peaks_toggle, THIS_NEEDS_REFLECTION);
    /* PERHAPS: Include max-peaks on this line */

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    {
      const char **cmaps;
      int i, len;
      len = num_colormaps();
      cmaps = (const char **)CALLOC(len, sizeof(const char *));
      for (i = 0; i < len; i++)
	cmaps[i] = (const char *)colormap_name(i);
      prf = prefs_row_with_completed_list("sonogram colormap (" S_colormap "):", S_colormap, cmaps[color_map(ss)],
					  cmaps, len,
					  fft_box, current_sep,
					  colormap_from_text,
					  colormap_completer, NULL,
					  colormap_from_menu, THIS_NEEDS_REFLECTION);
      FREE(cmaps);
    }
    /* PERHAPS: a line with color-inverted and color-scaler */

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    prf = prefs_row_with_toggle("y axis as log magnitude (" S_fft_log_magnitude "):", S_fft_log_magnitude,
				fft_log_magnitude(ss),
				fft_box, current_sep,
				log_magnitude_toggle, THIS_NEEDS_REFLECTION);

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    prf = prefs_row_with_toggle("x axis as log freq (" S_fft_log_frequency "):", S_fft_log_frequency,
				fft_log_frequency(ss),
				fft_box, current_sep,
				log_frequency_toggle, THIS_NEEDS_REFLECTION);

    /*
    ss->Transform_Graph_Type = DEFAULT_TRANSFORM_GRAPH_TYPE;
    ss->Wavelet_Type = DEFAULT_WAVELET_TYPE;
    ss->Transform_Type = DEFAULT_TRANSFORM_TYPE;
    ss->Transform_Normalization = DEFAULT_TRANSFORM_NORMALIZATION;

    colormap options?

    peaks fonts
    ss->Sinc_Width = DEFAULT_SINC_WIDTH;

    */
    
  }

  current_sep = make_inter_topic_separator(topics);

  /* -------- marks, mixes, and regions -------- */
  {
    Widget mmr_frame, mmr_box, mmr_label;

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    mmr_frame = XtCreateManagedWidget("pref-mmr-frame", xmFrameWidgetClass, topics, args, n);

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
    mmr_box = XtCreateManagedWidget("pref-mmr", xmFormWidgetClass, mmr_frame, args, n);

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->light_blue); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
    mmr_label = XtCreateManagedWidget("marks, mixes, and regions", xmLabelWidgetClass, mmr_box, args, n);

    prf = prefs_color_selector_row("mark color (" S_mark_color "):", S_mark_color, ss->sgx->mark_color,
				   mmr_box, mmr_label,
				   mark_color_func, THIS_NEEDS_REFLECTION);

    /*
    ss->Show_Marks = DEFAULT_SHOW_MARKS;
    ss->Mark_Tag_Width = DEFAULT_MARK_TAG_WIDTH;
    ss->Mark_Tag_Height = DEFAULT_MARK_TAG_HEIGHT;
    ss->With_Mix_Tags = DEFAULT_WITH_MIX_TAGS;
    ss->Show_Mix_Waveforms = DEFAULT_SHOW_MIX_WAVEFORMS;
    ss->Mix_Waveform_Height = DEFAULT_MIX_WAVEFORM_HEIGHT;
    ss->Mix_Tag_Width = DEFAULT_MIX_TAG_WIDTH;
    ss->Mix_Tag_Height = DEFAULT_MIX_TAG_HEIGHT;
    ss->Selection_Creates_Region = DEFAULT_SELECTION_CREATES_REGION;
    colors
    marks menu?
    */
    
  }
  

  current_sep = make_inter_topic_separator(topics);

  /* -------- programming -------- */
  {
    Widget prg_frame, prg_box, prg_label;

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    prg_frame = XtCreateManagedWidget("pref-fft-frame", xmFrameWidgetClass, topics, args, n);

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
    prg_box = XtCreateManagedWidget("pref-fft", xmFormWidgetClass, prg_frame, args, n);

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->light_blue); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
    prg_label = XtCreateManagedWidget("programming options", xmLabelWidgetClass, prg_box, args, n);


    /* omit those that are extlang related if no extlang */

    /*
    extlang: ss->Listener_Prompt = copy_string(DEFAULT_LISTENER_PROMPT);
    guile: ss->Optimization = DEFAULT_OPTIMIZATION; 
    ss->Print_Length = DEFAULT_PRINT_LENGTH;
    extlang: ss->Show_Backtrace = DEFAULT_SHOW_BACKTRACE;
    colors
    listener font
    */
  }
  
  XtManageChild(preferences_dialog);
  set_dialog_widget(PREFERENCES_DIALOG, preferences_dialog);

}

/*
hooks -- peak env
locale?
package deals
optional menus/files/key bindings
*/

