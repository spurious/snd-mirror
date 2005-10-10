#include "snd.h"

/* preferences dialog; layout design taken from webmail
 *
 * also add an html-style table of contents at top -- basically mimic a web-page
 */

static Widget preferences_dialog = NULL;
static bool prefs_helping = false;

#define MID_POSITION 40
#define COLOR_POSITION 50
#define FIRST_COLOR_POSITION 6
#define SECOND_COLOR_POSITION 30
#define THIRD_COLOR_POSITION 55
/* was 0 27 54 */
#define HELP_POSITION 80

#define MID_SPACE 16
#define INTER_TOPIC_SPACE 3
#define INTER_VARIABLE_SPACE 2

#define HELP_WAIT_TIME 500
#define POWER_WAIT_TIME 100
#define POWER_INITIAL_WAIT_TIME 500
#define ERROR_WAIT_TIME 1000

#define STARTUP_WIDTH 925
#define STARTUP_HEIGHT 800

/* TODO: XEN_HOOK_MEMBER_P, XEN_HOOK_ADD, and XEN_HOOK_REMOVE
         XEN_FILE_LOADED_P
 */

#if HAVE_GUILE


/*
#define XEN_CLEAR_HOOK(Arg)           scm_reset_hook_x(Arg)
#define XEN_HOOKED(a)                 (XEN_NOT_NULL_P(SCM_HOOK_PROCEDURES(a)))
#define XEN_HOOK_PROCEDURES(a)        SCM_HOOK_PROCEDURES(a)
*/

#endif


typedef struct prefs_info {
  Widget label, text, arrow_up, arrow_down, arrow_right, error, toggle, scale;
  Widget color, rscl, gscl, bscl, rtxt, gtxt, btxt, list_menu, radio_button, helper;
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
  void (*save_func)(struct prefs_info *prf, FILE *fd);
} prefs_info;

static int prefs_size = 0, prefs_top = 0;
static prefs_info **prefs = NULL;

static void remember_pref(prefs_info *prf, 
			  void (*reflect_func)(struct prefs_info *prf),
			  void (*save_func)(struct prefs_info *prf, FILE *fd))
{
  if (prefs_size == 0)
    {
      prefs_size = 100;
      prefs = (prefs_info **)CALLOC(prefs_size, sizeof(prefs_info *));
    }
  else
    {
      if (prefs_top >= prefs_size)
	{
	  int i;
	  prefs_size += 100;
	  prefs = (prefs_info **)REALLOC(prefs, prefs_size * sizeof(prefs_info *));
	  for (i = prefs_top; i < prefs_size; i++) prefs[i] = NULL;
	}
    }
  prf->reflect_func = reflect_func;
  prf->save_func = save_func;
  prefs[prefs_top++] = prf;
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

#include <X11/IntrinsicP.h>

static Widget find_radio_button(Widget parent, const char *name)
{
  int i;
  CompositeWidget cw = (CompositeWidget)parent;
  for (i = 0; i < cw->composite.num_children; i++)
    {
      Widget child;
      child = cw->composite.children[i];
      if ((child) &&
	  (strcmp(XtName(child), name) == 0))
	return(child);
    }
  return(NULL);
}



/* ---------------- help strings ---------------- */

static void prefs_help(prefs_info *prf)
{
  if (prf->var_name)
    snd_help(prf->var_name, 
	     XEN_TO_C_STRING(XEN_OBJECT_HELP(C_STRING_TO_XEN_SYMBOL(prf->var_name))),
	     WITH_WORD_WRAP);
}

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
    {
      prf->help_id = XtAppAddTimeOut(MAIN_APP(ss),
				     HELP_WAIT_TIME,
				     prefs_tooltip_help,
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

/* ---------------- row label widget ---------------- */

static Widget make_row_label(const char *label, Widget box, Widget top_widget)
{
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
  return(XtCreateManagedWidget(label, xmLabelWidgetClass, box, args, n));
}

/* ---------------- row separator widget ---------------- */

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

/* ---------------- row help widget ---------------- */

static Widget make_row_help(const char *label, prefs_info *prf, Widget box, Widget top_widget, Widget left_widget)
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
  s1 = XmStringCreate((char *)label, XmFONTLIST_DEFAULT_TAG);
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
  XtAddCallback(helper, XmNactivateCallback, prefs_help_click_callback, (XtPointer)prf);
  XmStringFree(s1);
  return(helper);
}

/* ---------------- row toggle widget ---------------- */

static Widget make_row_toggle(bool current_value, Widget left_widget, Widget box, Widget top_widget)
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
  XtSetArg(args[n], XmNset, (current_value) ? XmSET : XmUNSET); n++;
  XtSetArg(args[n], XmNborderWidth, 0); n++;
  XtSetArg(args[n], XmNborderColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNmarginHeight, 0); n++;
  XtSetArg(args[n], XmNindicatorOn, XmINDICATOR_FILL); n++;
  XtSetArg(args[n], XmNindicatorSize, 14); n++;
  return(XtCreateManagedWidget(" ", xmToggleButtonWidgetClass, box, args, n));
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
  Widget sep;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;

  prf->label = make_row_label(label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  prf->toggle = make_row_toggle(current_value, sep, box, top_widget);
  make_row_help(varname, prf, box, top_widget, prf->toggle);
  
  XtAddCallback(prf->toggle, XmNvalueChangedCallback, call_toggle_func, (void *)prf);

  XtAddEventHandler(prf->label, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->toggle, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->label, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->toggle, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);

  return(prf);
}


/* ---------------- toggle with text ---------------- */

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
  Arg args[20];
  int n;
  prefs_info *prf = NULL;
  Widget sep, sep1, lab1;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;
  prf->text_func = text_func;

  prf->label = make_row_label(label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  prf->toggle = make_row_toggle(current_value, sep, box, top_widget);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, prf->toggle); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
  XtSetArg(args[n], XmNwidth, 16); n++;
  XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
  sep1 = XtCreateManagedWidget("sep1", xmSeparatorWidgetClass, box, args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, sep1); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  lab1 = XtCreateManagedWidget(text_label, xmLabelWidgetClass, box, args, n);
  
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, lab1); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNcolumns, cols); n++;
  XtSetArg(args[n], XmNvalue, text_value); n++;
  XtSetArg(args[n], XmNmarginHeight, 1); n++;
  XtSetArg(args[n], XmNborderWidth, 0); n++;
  XtSetArg(args[n], XmNborderColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNbottomShadowColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNtopShadowColor, ss->sgx->white); n++;
  prf->text = make_textfield_widget("text", box, args, n, ACTIVATABLE_BUT_NOT_FOCUSED, NO_COMPLETER);

  make_row_help(varname, prf, box, top_widget, prf->text);
  
  XtAddCallback(prf->toggle, XmNvalueChangedCallback, call_toggle_func, (void *)prf);
  XtAddCallback(prf->text, XmNactivateCallback, call_text_func, (void *)prf);

  XtAddEventHandler(prf->label, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->toggle, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->text, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->label, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->toggle, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->text, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);

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

static prefs_info *prefs_row_with_radio_box(const char *label, const char *varname, 
					    const char **labels, int num_labels, int current_value,
					    Widget box, Widget top_widget, 
					    void (*toggle_func)(prefs_info *prf))
{
  Arg args[20];
  int i, n;
  prefs_info *prf = NULL;
  Widget sep;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;

  prf->label = make_row_label(label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, sep); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNborderWidth, 0); n++;
  XtSetArg(args[n], XmNborderColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNmarginHeight, 0); n++;
  XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;

  XtSetArg(args[n], XmNpacking, XmPACK_TIGHT); n++;

  prf->toggle = XmCreateRadioBox(box, "radio-box", args, n);
  XtManageChild(prf->toggle);

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
      button = XtCreateManagedWidget(labels[i], xmToggleButtonWidgetClass, prf->toggle, args, n);

      XtAddCallback(button, XmNvalueChangedCallback, call_radio_func, (XtPointer)prf);
    }

  make_row_help(varname, prf, box, top_widget, prf->toggle);

  XtAddEventHandler(prf->label, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->label, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->toggle, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
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
  prf->scale_max = max_val;

  prf->label = make_row_label(label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  
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
  
  make_row_help(varname, prf, box, top_widget, prf->scale);

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

static prefs_info *prefs_row_with_text(const char *label, const char *varname, const char *value,
				       Widget box, Widget top_widget,
				       void (*text_func)(prefs_info *prf))
{
  Arg args[20];
  int n;
  prefs_info *prf = NULL;
  Widget sep;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;

  prf->label = make_row_label(label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, sep); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
  XtSetArg(args[n], XmNrightPosition, HELP_POSITION); n++;
  XtSetArg(args[n], XmNvalue, value); n++;
  XtSetArg(args[n], XmNmarginHeight, 1); n++;
  XtSetArg(args[n], XmNborderWidth, 0); n++;
  XtSetArg(args[n], XmNborderColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNbottomShadowColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNtopShadowColor, ss->sgx->white); n++;
  prf->text = make_textfield_widget("text", box, args, n, ACTIVATABLE_BUT_NOT_FOCUSED, NO_COMPLETER);
  
  make_row_help(varname, prf, box, top_widget, prf->text);

  prf->text_func = text_func;
  XtAddCallback(prf->text, XmNactivateCallback, call_text_func, (XtPointer)prf);

  XtAddEventHandler(prf->label, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->text, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->label, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->text, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);

  return(prf);
}


/* ---------------- two texts in a row ---------------- */

static prefs_info *prefs_row_with_two_texts(const char *label, const char *varname,
					    const char*label1, const char *text1, const char*label2, const char *text2, int cols,
					    Widget box, Widget top_widget,
					    void (*text_func)(prefs_info *prf))
{
  Arg args[20];
  int n;
  prefs_info *prf = NULL;
  Widget sep, lab1, lab2;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;

  prf->label = make_row_label(label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, sep); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  lab1 = XtCreateManagedWidget(label1, xmLabelWidgetClass, box, args, n);
  
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, lab1); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNcolumns, cols); n++;
  XtSetArg(args[n], XmNvalue, text1); n++;
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
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  lab2 = XtCreateManagedWidget(label2, xmLabelWidgetClass, box, args, n);
  
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, lab2); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNcolumns, cols); n++;
  XtSetArg(args[n], XmNvalue, text2); n++;
  XtSetArg(args[n], XmNmarginHeight, 1); n++;
  XtSetArg(args[n], XmNborderWidth, 0); n++;
  XtSetArg(args[n], XmNborderColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNbottomShadowColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNtopShadowColor, ss->sgx->white); n++;
  prf->rtxt = make_textfield_widget("rtxt", box, args, n, ACTIVATABLE_BUT_NOT_FOCUSED, NO_COMPLETER);
  
  make_row_help(varname, prf, box, top_widget, prf->rtxt);

  prf->text_func = text_func;
  XtAddCallback(prf->text, XmNactivateCallback, call_text_func, (XtPointer)prf);
  XtAddCallback(prf->rtxt, XmNactivateCallback, call_text_func, (XtPointer)prf);

  XtAddEventHandler(prf->label, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->text, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->rtxt, EnterWindowMask, false, mouse_enter_pref_callback, (void *)prf);
  XtAddEventHandler(prf->label, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->text, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);
  XtAddEventHandler(prf->rtxt, LeaveWindowMask, false, mouse_leave_pref_callback, (void *)prf);

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
					 void (*text_func)(prefs_info *prf))
{
  Arg args[20];
  int n;
  prefs_info *prf = NULL;
  Widget sep;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;

  prf->label = make_row_label(label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  
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
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
  XtSetArg(args[n], XmNrightPosition, HELP_POSITION); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_END); n++;
  prf->error = XtCreateManagedWidget("", xmLabelWidgetClass, box, args, n);
  
  make_row_help(varname, prf, box, top_widget, prf->error);

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
						 void (*list_func)(prefs_info *prf, char *value))
{
  Arg args[20];
  int n, i, cols = 0;
  prefs_info *prf = NULL;
  Widget sep, sbar;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;

  prf->label = make_row_label(label, box, top_widget);
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
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
  XtSetArg(args[n], XmNrightPosition, HELP_POSITION); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_END); n++;
  prf->error = XtCreateManagedWidget("", xmLabelWidgetClass, box, args, n);

  make_row_help(varname, prf, box, top_widget, prf->error);

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
					    void (*color_func)(prefs_info *prf, float r, float g, float b))
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

  prf->label = make_row_label(label, box, top_widget);
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
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNmarginHeight, 1); n++;
  XtSetArg(args[n], XmNborderWidth, 0); n++;
  XtSetArg(args[n], XmNborderColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNbottomShadowColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNcolumns, 6); n++;
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
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNmarginHeight, 1); n++;
  XtSetArg(args[n], XmNborderWidth, 0); n++;
  XtSetArg(args[n], XmNborderColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNbottomShadowColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNcolumns, 6); n++;
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
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNmarginHeight, 1); n++;
  XtSetArg(args[n], XmNborderWidth, 0); n++;
  XtSetArg(args[n], XmNborderColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNbottomShadowColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNcolumns, 6); n++;
  XtSetArg(args[n], XmNtopShadowColor, ss->sgx->white); n++;
  prf->btxt = make_textfield_widget("text", box, args, n, ACTIVATABLE_BUT_NOT_FOCUSED, NO_COMPLETER);
  float_to_textfield(prf->btxt, b);

  make_row_help(varname, prf, box, top_widget, prf->btxt);

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




/* ---------------- base buttons ---------------- */

static void preferences_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  prefs_helping = true;

  snd_help("preferences",
	   "This dialog is under construction.  It sets various global variables, 'Save' then writes the new values \
to ~/.snd_prefs_guile|ruby so that they take effect the next time you start Snd.  'Reset' resets all variables to \
their default (initial) values. 'Help' starts this dialog, and as long as it is active, it will post helpful \
information if the mouse lingers over some variable -- sort of a tooltip that stays out of your way. \
You can also request help on a given topic by clicking the variable name on the far right.",
	   WITH_WORD_WRAP);
}

static void preferences_quit_callback(Widget w, XtPointer context, XtPointer info) 
{
  if (XmGetFocusWidget(preferences_dialog) == XmMessageBoxGetChild(preferences_dialog, XmDIALOG_OK_BUTTON))
    XtUnmanageChild(preferences_dialog);
}

static void reflect_prefs(void)
{
  int i;
  for (i = 0; i < prefs_top; i++)
    {
      prefs_info *prf;
      prf = prefs[i];
      if ((prf) &&
	  (prf->reflect_func))
	(*(prf->reflect_func))(prf);
    }
}

static void preferences_reset_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_set_global_defaults(true); 
  reflect_prefs();
}

static void save_prefs(const char *filename)
{
  int i;
  char *fullname;
  FILE *fd;
  if (!filename) return; /* error earlier */
  fullname = mus_expand_filename(filename);
  fd = FOPEN(fullname, "a");
  if (fd)
    {
      for (i = 0; i < prefs_top; i++)
	{
	  prefs_info *prf;
	  prf = prefs[i];
	  if ((prf) &&
	      (prf->save_func))
	    (*(prf->save_func))(prf, fd);
	}
      snd_fclose(fd, filename);
    }
  else snd_error("can't save preferences: %s %s", filename, snd_io_strerror());
  FREE(fullname);
}

static void preferences_save_callback(Widget w, XtPointer context, XtPointer info) 
{
  save_prefs(save_options_in_prefs()); /* TODO: redirect (for save_options) + msg */
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


/* ---------------- start up size ---------------- */

static void int_to_textfield(Widget w, int val)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(w), w);
  str = (char *)CALLOC(16, sizeof(char));
  mus_snprintf(str, 16, "%d", val);
  XmTextFieldSetString(w, str);
  FREE(str);
}

static void startup_width_erase_func(XtPointer context, XtIntervalId *id)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->text, ss->init_window_width);
}

static void startup_height_erase_func(XtPointer context, XtIntervalId *id)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->rtxt, ss->init_window_height);
}

static void startup_width_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  XmTextSetString(prf->text, "right");
  XtAppAddTimeOut(MAIN_APP(ss),
		  ERROR_WAIT_TIME,
		  startup_width_erase_func,
		  (XtPointer)prf);
}

static void startup_height_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  XmTextSetString(prf->rtxt, "right");
  XtAppAddTimeOut(MAIN_APP(ss),
		  ERROR_WAIT_TIME,
		  startup_height_erase_func,
		  (XtPointer)prf);
}

static void startup_size_text(prefs_info *prf)
{
  char *str;
  str = XmTextGetString(prf->text);
  if ((str) && (*str))
    {
      int width = 0;
      redirect_errors_to(startup_width_error, (void *)prf);
      width = string_to_int(str, 1, "startup width");
      redirect_errors_to(NULL, NULL);
      if (width > 0) ss->init_window_width = width;
      XtFree(str);
      str = XmTextGetString(prf->rtxt);
      if ((str) && (*str))
	{
	  int height;
	  redirect_errors_to(startup_height_error, (void *)prf);
	  height = string_to_int(str, 1, "startup height");
	  redirect_errors_to(NULL, NULL);
	  if (height > 0) ss->init_window_height = height;
	  XtFree(str);
	}
    }
}


/* ---------------- auto-resize ---------------- */

static void reflect_auto_resize(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, auto_resize(ss), false);
}

static void resize_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  set_auto_resize(XmToggleButtonGetState(prf->toggle) == XmSET);
}

/* ---------------- ask-before-overwrite ---------------- */

static void reflect_ask_before_overwrite(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, ask_before_overwrite(ss), false);
}

static void overwrite_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  set_ask_before_overwrite(XmToggleButtonGetState(prf->toggle) == XmSET);
}

/* ---------------- current-window-display ---------------- */

static void save_current_window_display(prefs_info *prf, FILE *fd)
{
  
}

static void reflect_current_window_display(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, false, false);
}


/* TODO: XEN_HOOK_MEMBER_P, XEN_HOOK_ADD, and XEN_HOOK_REMOVE
   also is file (snd-draw.scm or rb case) loaded?
 */

static void current_window_display_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);

}

static bool find_current_window_display(void)
{
  return(false);
}


/* ---------------- show-controls ---------------- */

static void reflect_show_controls(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, in_show_controls(ss), false);
}

static void controls_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  in_set_show_controls(ss, XmToggleButtonGetState(prf->toggle) == XmSET);
}

/* ---------------- basic-color ---------------- */

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

static Pixel saved_basic_color;

static void reflect_basic_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_basic_color); 
  set_basic_color(saved_basic_color);
}

static void basic_color_func(prefs_info *prf, float r, float g, float b)
{
  XColor *tmp;
  tmp = rgb_to_color(r, g, b);
  set_basic_color(tmp->pixel);
  FREE(tmp);
}

/* ---------------- highlight-color ---------------- */

static Pixel saved_highlight_color;

static void reflect_highlight_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_highlight_color); 
  set_highlight_color(saved_highlight_color);
}

static void highlight_color_func(prefs_info *prf, float r, float g, float b)
{
  XColor *tmp;
  tmp = rgb_to_color(r, g, b);
  set_highlight_color(tmp->pixel);
  FREE(tmp);
}

/* ---------------- position-color ---------------- */

static Pixel saved_position_color;

static void reflect_position_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_position_color); 
  set_position_color(saved_position_color);
}

static void position_color_func(prefs_info *prf, float r, float g, float b)
{
  XColor *tmp;
  tmp = rgb_to_color(r, g, b);
  set_position_color(tmp->pixel);
  FREE(tmp);
}

/* ---------------- zoom-color ---------------- */

static Pixel saved_zoom_color;

static void reflect_zoom_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_zoom_color); 
  set_zoom_color(saved_zoom_color);
}

static void zoom_color_func(prefs_info *prf, float r, float g, float b)
{
  XColor *tmp;
  tmp = rgb_to_color(r, g, b);
  set_zoom_color(tmp->pixel);
  FREE(tmp);
}

/* ---------------- verbose-cursor ---------------- */

static void reflect_verbose_cursor(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, verbose_cursor(ss), false);
}

static void verbose_cursor_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  in_set_verbose_cursor(XmToggleButtonGetState(prf->toggle) == XmSET);
}

/* ---------------- cursor-follows-play ---------------- */

static void reflect_cursor_follows_play(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, cursor_follows_play(ss), false);
}

static void cursor_follows_play_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  in_set_cursor_follows_play(ss, (XmToggleButtonGetState(prf->toggle) == XmSET) ? FOLLOW_ALWAYS : DONT_FOLLOW);
}

/* ---------------- cursor-size ---------------- */

#define MIN_CURSOR_SIZE 1
#define MAX_CURSOR_SIZE 500

static void show_cursor_size(prefs_info *prf)
{
  int_to_textfield(prf->text, cursor_size(ss));
}

static void reflect_cursor_size(prefs_info *prf)
{
  show_cursor_size(prf);
  XtSetSensitive(prf->arrow_up, cursor_size(ss) < MAX_CURSOR_SIZE);
  XtSetSensitive(prf->arrow_down, cursor_size(ss) > MIN_CURSOR_SIZE);
}

static void cursor_size_up(prefs_info *prf)
{
  int size;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  size = cursor_size(ss) + 1;
  if (size >= MAX_CURSOR_SIZE) XtSetSensitive(prf->arrow_up, false);
  if (size > MIN_CURSOR_SIZE) XtSetSensitive(prf->arrow_down, true);
  in_set_cursor_size(size);
  show_cursor_size(prf);
}

static void cursor_size_down(prefs_info *prf)
{
  int size;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  size = cursor_size(ss) - 1;
  if (size <= MIN_CURSOR_SIZE) XtSetSensitive(prf->arrow_down, false);
  if (size < MAX_CURSOR_SIZE) XtSetSensitive(prf->arrow_up, true);
  in_set_cursor_size(size);
  show_cursor_size(prf);
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

/* ---------------- cursor-style ---------------- */

static const char *cursor_styles[2] = {"cross", "line"};

static void reflect_cursor_style(prefs_info *prf)
{
  Widget w;
  w = find_radio_button(prf->toggle, cursor_styles[cursor_style(ss)]);
  if (w)
    XmToggleButtonSetState(w, XmSET, false);
  else fprintf(stderr, "can't find %s\n", cursor_styles[cursor_style(ss)]);
  if ((prf->radio_button) &&
      (XmIsToggleButton(prf->radio_button)) &&
      (w != prf->radio_button))
    {
      /* motif docs are incorrect -- the set above does not unset the currently set radio button */
      XmToggleButtonSetState(prf->radio_button, XmUNSET, false);
      prf->radio_button = w;
    }
}

static void cursor_style_choice(prefs_info *prf)
{
  if (XmToggleButtonGetState(prf->radio_button) == XmSET)
    {
      if (strcmp(XtName(prf->radio_button), "line") == 0)
	in_set_cursor_style(CURSOR_LINE);
      else in_set_cursor_style(CURSOR_CROSS);
    }
}

/* ---------------- cursor-color ---------------- */

static Pixel saved_cursor_color;

static void reflect_cursor_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_cursor_color); 
  color_cursor(saved_cursor_color);
}

static void cursor_color_func(prefs_info *prf, float r, float g, float b)
{
  XColor *tmp;
  tmp = rgb_to_color(r, g, b);
  color_cursor(tmp->pixel);
  for_each_chan(update_graph);
  FREE(tmp);
}


/* ---------------- temp-dir ---------------- */

static void reflect_temp_dir(prefs_info *prf)
{
  XmTextSetString(prf->text, temp_dir(ss));
}

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

static void reflect_save_dir(prefs_info *prf)
{
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

#if HAVE_LADSPA
/* ---------------- ladspa-dir ---------------- */

static void reflect_ladspa_dir(prefs_info *prf)
{
  XmTextSetString(prf->text, ladspa_dir(ss));
}

static void ladspa_dir_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if (ladspa_dir(ss)) FREE(ladspa_dir(ss));
  if ((!str) || (!(*str)))
    {
      set_ladspa_dir(copy_string(str));
      XtFree(str);
    }
  else set_ladspa_dir(copy_string(DEFAULT_LADSPA_DIR));
}
#endif

/* ---------------- html-program ---------------- */

static void reflect_html_program(prefs_info *prf)
{
  XmTextSetString(prf->text, html_program(ss));
}

static void html_program_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if (html_program(ss)) FREE(html_program(ss));
  if ((!str) || (!(*str)))
    {
      set_html_program(copy_string(str));
      XtFree(str);
    }
  else set_html_program(copy_string(DEFAULT_HTML_PROGRAM));
}

/* ---------------- graph-style ---------------- */

static const char *graph_styles[5] = {"line", "dot", "filled", "dot+line", "lollipop"};

static void reflect_graph_style(prefs_info *prf)
{
  Widget w;
  w = find_radio_button(prf->toggle, graph_styles[graph_style(ss)]);
  if (w)
    XmToggleButtonSetState(w, XmSET, false);
  else fprintf(stderr, "can't find %s\n", graph_styles[graph_style(ss)]);
  if ((prf->radio_button) &&
      (XmIsToggleButton(prf->radio_button)) &&
      (w != prf->radio_button))
    {
      /* motif docs are incorrect -- the set above does not unset the currently set radio button */
      XmToggleButtonSetState(prf->radio_button, XmUNSET, false);
      prf->radio_button = w;
    }
}

static void graph_style_choice(prefs_info *prf)
{
  if (XmToggleButtonGetState(prf->radio_button) == XmSET)
    {
      if (strcmp(XtName(prf->radio_button), "line") == 0)
	in_set_graph_style(GRAPH_LINES);
      else
	{
	  if (strcmp(XtName(prf->radio_button), "dot") == 0)
	    in_set_graph_style(GRAPH_DOTS);
	  else
	    {
	      if (strcmp(XtName(prf->radio_button), "filled") == 0)
		in_set_graph_style(GRAPH_FILLED);
	      else
		{
		  if (strcmp(XtName(prf->radio_button), "dot+line") == 0)
		    in_set_graph_style(GRAPH_DOTS_AND_LINES);
		  else in_set_graph_style(GRAPH_LOLLIPOPS);
		}
	    }
	}
    }
}

/* ---------------- dot-size ---------------- */

#define MIN_DOT_SIZE 0
#define MAX_DOT_SIZE 100

static void show_dot_size(prefs_info *prf)
{
  int_to_textfield(prf->text, dot_size(ss));
}

static void reflect_dot_size(prefs_info *prf)
{
  show_dot_size(prf);
  XtSetSensitive(prf->arrow_up, dot_size(ss) < MAX_DOT_SIZE);
  XtSetSensitive(prf->arrow_down, dot_size(ss) > MIN_DOT_SIZE);
}

static void dot_size_up(prefs_info *prf)
{
  int size;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  size = dot_size(ss) + 1;
  if (size >= MAX_DOT_SIZE) XtSetSensitive(prf->arrow_up, false);
  if (size > MIN_DOT_SIZE) XtSetSensitive(prf->arrow_down, true);
  in_set_dot_size(size);
  show_dot_size(prf);
}

static void dot_size_down(prefs_info *prf)
{
  int size;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  size = dot_size(ss) - 1;
  if (size <= MIN_DOT_SIZE) XtSetSensitive(prf->arrow_down, false);
  if (size < MAX_DOT_SIZE) XtSetSensitive(prf->arrow_up, true);
  in_set_dot_size(size);
  show_dot_size(prf);
}

static void dot_size_from_text(prefs_info *prf)
{
  int size;
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((str) && (*str))
    {
      prf->got_error = false;
      redirect_errors_to(post_prefs_error, (void *)prf);
      size = string_to_int(str, 0, "dot size"); 
      redirect_errors_to(NULL, NULL);
      XtFree(str);
      if (!(prf->got_error))
	{
	  if (size >= MIN_DOT_SIZE)
	    {
	      if (size <= MAX_DOT_SIZE)
		in_set_dot_size(size);
	      else va_post_prefs_error("%s > %d?", (void *)prf, str, MAX_DOT_SIZE);
	    }
	  else va_post_prefs_error("%s < %d?", (void *)prf, str, MIN_DOT_SIZE);
	}
      else prf->got_error = false;
    }
  else post_prefs_error("no size?", (void *)prf);
}


/* ---------------- show-y-zero ---------------- */

static void reflect_show_y_zero(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, show_y_zero(ss), false);
}

static void y_zero_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  in_set_show_y_zero(XmToggleButtonGetState(prf->toggle) == XmSET);
}

/* ---------------- show-grid ---------------- */

static void reflect_show_grid(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, show_grid(ss), false);
}

static void grid_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  in_set_show_grid((XmToggleButtonGetState(prf->toggle) == XmSET) ? WITH_GRID : NO_GRID);
}

/* ---------------- grid-density ---------------- */

static void reflect_grid_density(prefs_info *prf)
{
  XmScaleSetValue(prf->scale, (int)(100 * grid_density(ss) / prf->scale_max));
  float_to_textfield(prf->text, grid_density(ss));
}

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
      else XmTextSetString(prf->text, "right");
      XtFree(str);
    }
}

/* ---------------- data-color ---------------- */

static Pixel saved_data_color;

static void reflect_data_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_data_color); 
  set_data_color(saved_data_color);
}

static void data_color_func(prefs_info *prf, float r, float g, float b)
{
  XColor *tmp;
  tmp = rgb_to_color(r, g, b);
  set_data_color(tmp->pixel);
  FREE(tmp);
}

/* ---------------- graph-color ---------------- */

static Pixel saved_graph_color;

static void reflect_graph_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_graph_color); 
  set_graph_color(saved_graph_color);
}

static void graph_color_func(prefs_info *prf, float r, float g, float b)
{
  XColor *tmp;
  tmp = rgb_to_color(r, g, b);
  set_graph_color(tmp->pixel);
  FREE(tmp);
}

/* ---------------- selected-data-color ---------------- */

static Pixel saved_selected_data_color;

static void reflect_selected_data_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_selected_data_color); 
  set_selected_data_color(saved_selected_data_color);
}

static void selected_data_color_func(prefs_info *prf, float r, float g, float b)
{
  XColor *tmp;
  tmp = rgb_to_color(r, g, b);
  set_selected_data_color(tmp->pixel);
  FREE(tmp);
}

/* ---------------- selected-graph-color ---------------- */

static Pixel saved_selected_graph_color;

static void reflect_selected_graph_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_selected_graph_color); 
  set_selected_graph_color(saved_selected_graph_color);
}

static void selected_graph_color_func(prefs_info *prf, float r, float g, float b)
{
  XColor *tmp;
  tmp = rgb_to_color(r, g, b);
  set_selected_graph_color(tmp->pixel);
  FREE(tmp);
}


/* ---------------- axis-label-font ---------------- */

static void axis_label_font_error_erase_func(XtPointer context, XtIntervalId *id)
{
  prefs_info *prf = (prefs_info *)context;
  XmTextSetString(prf->text, axis_label_font(ss));
}

static void reflect_axis_label_font(prefs_info *prf)
{
  XmTextSetString(prf->text, axis_label_font(ss));
}

static void axis_label_font_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((!str) || (!(*str)))
    {
      XmTextSetString(prf->text, axis_label_font(ss));
      return;
    }
  if (!(set_axis_label_font(str)))
    {
      XmTextSetString(prf->text, "can't find that font");
      XtAppAddTimeOut(MAIN_APP(ss),
		      ERROR_WAIT_TIME,
		      axis_label_font_error_erase_func,
		      (XtPointer)prf);
    }
  if (str) XtFree(str);
}

/* ---------------- axis-numbers-font ---------------- */

static void axis_numbers_font_error_erase_func(XtPointer context, XtIntervalId *id)
{
  prefs_info *prf = (prefs_info *)context;
  XmTextSetString(prf->text, axis_numbers_font(ss));
}

static void reflect_axis_numbers_font(prefs_info *prf)
{
  XmTextSetString(prf->text, axis_numbers_font(ss));
}

static void axis_numbers_font_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((!str) || (!(*str)))
    {
      XmTextSetString(prf->text, axis_numbers_font(ss));
      return;
    }
  if (!(set_axis_numbers_font(str)))
    {
      XmTextSetString(prf->text, "can't find that font");
      XtAppAddTimeOut(MAIN_APP(ss),
		      ERROR_WAIT_TIME,
		      axis_numbers_font_error_erase_func,
		      (XtPointer)prf);
    }
  if (str) XtFree(str);
}

/* ---------------- peaks-font ---------------- */

static void peaks_font_error_erase_func(XtPointer context, XtIntervalId *id)
{
  prefs_info *prf = (prefs_info *)context;
  XmTextSetString(prf->text, peaks_font(ss));
}

static void reflect_peaks_font(prefs_info *prf)
{
  XmTextSetString(prf->text, peaks_font(ss));
}

static void peaks_font_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((!str) || (!(*str)))
    {
      XmTextSetString(prf->text, peaks_font(ss));
      return;
    }
  if (!(set_peaks_font(str)))
    {
      XmTextSetString(prf->text, "can't find that font");
      XtAppAddTimeOut(MAIN_APP(ss),
		      ERROR_WAIT_TIME,
		      peaks_font_error_erase_func,
		      (XtPointer)prf);
    }
  if (str) XtFree(str);
}

/* ---------------- bold-peaks-font ---------------- */

static void bold_peaks_font_error_erase_func(XtPointer context, XtIntervalId *id)
{
  prefs_info *prf = (prefs_info *)context;
  XmTextSetString(prf->text, bold_peaks_font(ss));
}

static void reflect_bold_peaks_font(prefs_info *prf)
{
  XmTextSetString(prf->text, bold_peaks_font(ss));
}

static void bold_peaks_font_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((!str) || (!(*str)))
    {
      XmTextSetString(prf->text, bold_peaks_font(ss));
      return;
    }
  if (!(set_bold_peaks_font(str)))
    {
      XmTextSetString(prf->text, "can't find that font");
      XtAppAddTimeOut(MAIN_APP(ss),
		      ERROR_WAIT_TIME,
		      bold_peaks_font_error_erase_func,
		      (XtPointer)prf);
    }
  if (str) XtFree(str);
}

/* ---------------- tiny-font ---------------- */

static void tiny_font_error_erase_func(XtPointer context, XtIntervalId *id)
{
  prefs_info *prf = (prefs_info *)context;
  XmTextSetString(prf->text, tiny_font(ss));
}

static void reflect_tiny_font(prefs_info *prf)
{
  XmTextSetString(prf->text, tiny_font(ss));
}

static void tiny_font_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((!str) || (!(*str)))
    {
      XmTextSetString(prf->text, tiny_font(ss));
      return;
    }
  if (!(set_tiny_font(str)))
    {
      XmTextSetString(prf->text, "can't find that font");
      XtAppAddTimeOut(MAIN_APP(ss),
		      ERROR_WAIT_TIME,
		      tiny_font_error_erase_func,
		      (XtPointer)prf);
    }
  if (str) XtFree(str);
}



/* ---------------- fft-size ---------------- */

#define MAX_TRANSFORM_SIZE 1073741824
#define MIN_TRANSFORM_SIZE 2

static void fft_size_to_text(prefs_info *prf)
{
  char *new_size;
  new_size = mus_format(OFF_TD, transform_size(ss));
  XmTextSetString(prf->text, new_size);
  FREE(new_size);
}

static void reflect_fft_size(prefs_info *prf)
{
  fft_size_to_text(prf);
  XtSetSensitive(prf->arrow_up, transform_size(ss) < MAX_TRANSFORM_SIZE);
  XtSetSensitive(prf->arrow_down, transform_size(ss) > MIN_TRANSFORM_SIZE);
}

static void fft_size_up(prefs_info *prf)
{
  off_t size;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  size = transform_size(ss) * 2;
  if (size >= MAX_TRANSFORM_SIZE) XtSetSensitive(prf->arrow_up, false);
  if (size > MIN_TRANSFORM_SIZE) XtSetSensitive(prf->arrow_down, true);
  in_set_transform_size(size);
  fft_size_to_text(prf);
}

static void fft_size_down(prefs_info *prf)
{
  off_t size;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  size = transform_size(ss) / 2;
  if (size <= MIN_TRANSFORM_SIZE) XtSetSensitive(prf->arrow_down, false);
  if (size < MAX_TRANSFORM_SIZE) XtSetSensitive(prf->arrow_up, true);
  in_set_transform_size(size);
  fft_size_to_text(prf);
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
      size = string_to_off_t(str, MIN_TRANSFORM_SIZE, "size"); 
      redirect_errors_to(NULL, NULL);
      XtFree(str);
      if (!(prf->got_error))
	{
	  if (POWER_OF_2_P(size))
	    {
	      if (size <= MAX_TRANSFORM_SIZE)
		in_set_transform_size(size);
	      else va_post_prefs_error("%s > %d?", (void *)prf, str, MAX_TRANSFORM_SIZE);
	    }
	  else post_prefs_error("size must be a power of 2", (void *)prf);
	}
      else prf->got_error = false;
    }
}

/* ---------------- transform-graph-type ---------------- */

static const char *transform_graph_types[3] = {"normal", "sonogram", "spectrogram"};

static void reflect_transform_graph_type(prefs_info *prf)
{
  Widget w;
  w = find_radio_button(prf->toggle, transform_graph_types[transform_graph_type(ss)]);
  if (w)
    XmToggleButtonSetState(w, XmSET, false);
  else fprintf(stderr, "can't find %s\n", transform_graph_types[transform_graph_type(ss)]);
  if ((prf->radio_button) &&
      (XmIsToggleButton(prf->radio_button)) &&
      (w != prf->radio_button))
    {
      XmToggleButtonSetState(prf->radio_button, XmUNSET, false);
      prf->radio_button = w;
    }
}

static void transform_graph_type_choice(prefs_info *prf)
{
  if (XmToggleButtonGetState(prf->radio_button) == XmSET)
    {
      if (strcmp(XtName(prf->radio_button), "sonogram") == 0)
	in_set_transform_graph_type(GRAPH_AS_SONOGRAM);
      else
	{
	  if (strcmp(XtName(prf->radio_button), "spectrogram") == 0)
	    in_set_transform_graph_type(GRAPH_AS_SPECTROGRAM);
	  else in_set_transform_graph_type(GRAPH_ONCE);
	}
    }
}


/* ---------------- transform-type ---------------- */

#define NUM_TRANSFORM_TYPES 6

static const char *transform_types[NUM_TRANSFORM_TYPES] = {"Fourier", "Wavelet", "Walsh", "Autocorrelate", "Cepstrum", "Haar"};

static list_completer_info *transform_type_completer_info = NULL;

static void reflect_transform_type(prefs_info *prf)
{
  XmTextSetString(prf->text, (char *)transform_types[transform_type(ss)]);
}

static char *transform_type_completer(char *text, void *data)
{
  if (!transform_type_completer_info)
    {
      transform_type_completer_info = (list_completer_info *)CALLOC(1, sizeof(list_completer_info));
      transform_type_completer_info->exact_match = false;
      transform_type_completer_info->values = (char **)transform_types;
      transform_type_completer_info->num_values = NUM_TRANSFORM_TYPES;
      transform_type_completer_info->values_size = NUM_TRANSFORM_TYPES;
    }
  return(list_completer(text, (void *)transform_type_completer_info));
}

static void transform_type_from_menu(prefs_info *prf, char *value)
{
  int i;
  for (i = 0; i < NUM_TRANSFORM_TYPES; i++)
    if (strcmp(value, transform_types[i]) == 0)
      {
	in_set_transform_type(i);
	XmTextFieldSetString(prf->text, value);
      }
}

static void transform_type_from_text(prefs_info *prf)
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
	  for (i = 0; i < NUM_TRANSFORM_TYPES; i++)
	    if (STRCMP(trimmed_str, transform_types[i]) == 0)
	      {
		curpos = i;
		break;
	      }
	  if (curpos >= 0)
	    in_set_transform_type(curpos);
	  else post_prefs_error("unknown tranform", (void *)prf);
	}
      else post_prefs_error("no transform?", (void *)prf);
      FREE(trimmed_str);
    }
  else post_prefs_error("no transform?", (void *)prf);
}


/* -------- fft-window -------- */

static const char *fft_windows[NUM_FFT_WINDOWS] = 
  {"Rectangular", "Hann", "Welch", "Parzen", "Bartlett", "Hamming", "Blackman2", "Blackman3", "Blackman4",
   "Exponential", "Riemann", "Kaiser", "Cauchy", "Poisson", "Gaussian", "Tukey", "Dolph-Chebyshev", "Hann-Poisson", "Connes"};

static list_completer_info *fft_window_completer_info = NULL;

static void reflect_fft_window(prefs_info *prf)
{
  XmTextSetString(prf->text, (char *)fft_windows[(int)fft_window(ss)]);
}

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

static void reflect_fft_window_beta(prefs_info *prf)
{
  XmScaleSetValue(prf->scale, (int)(100 * fft_window_beta(ss) / prf->scale_max));
  float_to_textfield(prf->text, fft_window_beta(ss));
}

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
      else XmTextSetString(prf->text, "right");
      XtFree(str);
    }
}


/* ---------------- show-transform-peaks ---------------- */

static void reflect_show_transform_peaks(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, show_transform_peaks(ss), false);
}

static void transform_peaks_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  in_set_show_transform_peaks(XmToggleButtonGetState(prf->toggle) == XmSET);
}

static void transform_peaks_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextGetString(prf->text);
  if ((str) && (*str))
    {
      int value = 0;
      sscanf(str, "%d", &value);
      if (value >= 0)
	in_set_max_transform_peaks(value);
      else
	{
	  XtFree(str);
	  int_to_textfield(prf->text, max_transform_peaks(ss));
	}
    }
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

static void reflect_colormap(prefs_info *prf)
{
  XmTextFieldSetString(prf->text, colormap_name(color_map(ss)));
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

static void reflect_fft_log_magnitude(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, fft_log_magnitude(ss), false);
}

static void log_magnitude_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  in_set_fft_log_magnitude(XmToggleButtonGetState(prf->toggle) == XmSET);
}

/* ---------------- min-dB ---------------- */

static void reflect_min_dB(prefs_info *prf)
{
  float_to_textfield(prf->text, min_dB(ss));
}

static void min_dB_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextGetString(prf->text);
  if ((str) && (*str))
    {
      float value = 0.0;
      sscanf(str, "%f", &value);
      set_min_db(value); /* snd-chn.c -- redisplays */
    }
}


/* ---------------- fft-log-frequency ---------------- */

static void reflect_fft_log_frequency(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, fft_log_frequency(ss), false);
}

static void log_frequency_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  in_set_fft_log_frequency(XmToggleButtonGetState(prf->toggle) == XmSET);
}

/* ---------------- transform-normalization ---------------- */

static const char *transform_normalizations[4] = {"none", "by channel", "by sound", "global"};

static void reflect_transform_normalization(prefs_info *prf)
{
  Widget w;
  w = find_radio_button(prf->toggle, transform_normalizations[transform_normalization(ss)]);
  if (w)
    XmToggleButtonSetState(w, XmSET, false);
  else fprintf(stderr, "can't find %s\n", transform_normalizations[transform_normalization(ss)]);
  if ((prf->radio_button) &&
      (XmIsToggleButton(prf->radio_button)) &&
      (w != prf->radio_button))
    {
      XmToggleButtonSetState(prf->radio_button, XmUNSET, false);
      prf->radio_button = w;
    }
}

static void transform_normalization_choice(prefs_info *prf)
{
  if (XmToggleButtonGetState(prf->radio_button) == XmSET)
    {
      if (strcmp(XtName(prf->radio_button), "none") == 0)
	in_set_transform_normalization(DONT_NORMALIZE);
      else
	{
	  if (strcmp(XtName(prf->radio_button), "by channel") == 0)
	    in_set_transform_normalization(NORMALIZE_BY_CHANNEL);
	  else
	    {
	      if (strcmp(XtName(prf->radio_button), "by sound") == 0)
		in_set_transform_normalization(NORMALIZE_BY_SOUND);
	      else in_set_transform_normalization(NORMALIZE_GLOBALLY);
	    }
	}
    }
}


/* ---------------- mark-color ---------------- */

static Pixel saved_mark_color;

static void reflect_mark_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_mark_color);
  color_marks(saved_mark_color);
}

static void mark_color_func(prefs_info *prf, float r, float g, float b)
{
  XColor *tmp;
  tmp = rgb_to_color(r, g, b);
  color_marks(tmp->pixel);
  FREE(tmp);
}

/* ---------------- mark-tag size ---------------- */

static void reflect_mark_tag_size(prefs_info *prf)
{
  int_to_textfield(prf->text, mark_tag_width(ss));
  int_to_textfield(prf->rtxt, mark_tag_height(ss));
}

static void mark_tag_width_erase_func(XtPointer context, XtIntervalId *id)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->text, mark_tag_width(ss));
}

static void mark_tag_height_erase_func(XtPointer context, XtIntervalId *id)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->rtxt, mark_tag_height(ss));
}

static void mark_tag_width_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  XmTextSetString(prf->text, "right");
  XtAppAddTimeOut(MAIN_APP(ss),
		  ERROR_WAIT_TIME,
		  mark_tag_width_erase_func,
		  (XtPointer)prf);
}

static void mark_tag_height_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  XmTextSetString(prf->rtxt, "right");
  XtAppAddTimeOut(MAIN_APP(ss),
		  ERROR_WAIT_TIME,
		  mark_tag_height_erase_func,
		  (XtPointer)prf);
}

static void mark_tag_size_text(prefs_info *prf)
{
  char *str;
  str = XmTextGetString(prf->text);
  if ((str) && (*str))
    {
      int width = 0;
      redirect_errors_to(mark_tag_width_error, (void *)prf);
      width = string_to_int(str, 1, "mark tag width");
      redirect_errors_to(NULL, NULL);
      if (width > 0) set_mark_tag_width(width);
      XtFree(str);
      str = XmTextGetString(prf->rtxt);
      if ((str) && (*str))
	{
	  int height;
	  redirect_errors_to(mark_tag_height_error, (void *)prf);
	  height = string_to_int(str, 1, "mark tag height");
	  redirect_errors_to(NULL, NULL);
	  if (height > 0) set_mark_tag_height(height);
	  XtFree(str);
	}
    }
}


/* ---------------- mix-color (waveform) ---------------- */

static Pixel saved_mix_color;

static void reflect_mix_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_mix_color);
  color_mixes(saved_mix_color);
}

static void mix_color_func(prefs_info *prf, float r, float g, float b)
{
  XColor *tmp;
  tmp = rgb_to_color(r, g, b);
  color_mixes(tmp->pixel);
  FREE(tmp);
}

/* ---------------- mix-tag size ---------------- */

static void reflect_mix_tag_size(prefs_info *prf)
{
  int_to_textfield(prf->text, mix_tag_width(ss));
  int_to_textfield(prf->rtxt, mix_tag_height(ss));
}

static void mix_tag_width_erase_func(XtPointer context, XtIntervalId *id)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->text, mix_tag_width(ss));
}

static void mix_tag_height_erase_func(XtPointer context, XtIntervalId *id)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->rtxt, mix_tag_height(ss));
}

static void mix_tag_width_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  XmTextSetString(prf->text, "right");
  XtAppAddTimeOut(MAIN_APP(ss),
		  ERROR_WAIT_TIME,
		  mix_tag_width_erase_func,
		  (XtPointer)prf);
}

static void mix_tag_height_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  XmTextSetString(prf->rtxt, "right");
  XtAppAddTimeOut(MAIN_APP(ss),
		  ERROR_WAIT_TIME,
		  mix_tag_height_erase_func,
		  (XtPointer)prf);
}

static void mix_tag_size_text(prefs_info *prf)
{
  char *str;
  str = XmTextGetString(prf->text);
  if ((str) && (*str))
    {
      int width = 0;
      redirect_errors_to(mix_tag_width_error, (void *)prf);
      width = string_to_int(str, 1, "mix tag width");
      redirect_errors_to(NULL, NULL);
      if (width > 0) set_mix_tag_width(width);
      XtFree(str);
      str = XmTextGetString(prf->rtxt);
      if ((str) && (*str))
	{
	  int height;
	  redirect_errors_to(mix_tag_height_error, (void *)prf);
	  height = string_to_int(str, 1, "mix tag height");
	  redirect_errors_to(NULL, NULL);
	  if (height > 0) set_mix_tag_height(height);
	  XtFree(str);
	}
    }
}

/* ---------------- show-mix-waveforms ---------------- */

static void reflect_show_mix_waveforms(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, show_mix_waveforms(ss), false);
}

static void show_mix_waveforms_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  in_set_show_mix_waveforms(XmToggleButtonGetState(prf->toggle) == XmSET);
}

static void mix_waveform_height_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextGetString(prf->text);
  if ((str) && (*str))
    {
      int value = 0;
      sscanf(str, "%d", &value);
      if (value >= 0)
	in_set_mix_waveform_height(value);
      else
	{
	  XtFree(str);
	  int_to_textfield(prf->text, mix_waveform_height(ss));
	}
    }
}



/* ---------------- optimization ---------------- */

#define MAX_OPTIMIZATION 6
#define MIN_OPTIMIZATION 0

static void show_opt(prefs_info *prf)
{
  int_to_textfield(prf->text, optimization(ss));
}

static void reflect_optimization(prefs_info *prf)
{
  int_to_textfield(prf->text, optimization(ss));
  XtSetSensitive(prf->arrow_up, optimization(ss) < MAX_OPTIMIZATION);
  XtSetSensitive(prf->arrow_down, optimization(ss) > MIN_OPTIMIZATION);
}

static void optimization_up(prefs_info *prf)
{
  int val;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  val = optimization(ss) + 1;
  if (val >= MAX_OPTIMIZATION) XtSetSensitive(prf->arrow_up, false);
  if (val > MIN_OPTIMIZATION) XtSetSensitive(prf->arrow_down, true);
  set_optimization(val);
  show_opt(prf);
}

static void optimization_down(prefs_info *prf)
{
  int val;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  val = optimization(ss) - 1;
  if (val <= MIN_OPTIMIZATION) XtSetSensitive(prf->arrow_down, false);
  if (val < MAX_OPTIMIZATION) XtSetSensitive(prf->arrow_up, true);
  set_optimization(val);
  show_opt(prf);
}

static void optimization_from_text(prefs_info *prf)
{
  int opt;
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((str) && (*str))
    {
      prf->got_error = false;
      redirect_errors_to(post_prefs_error, (void *)prf);
      opt = string_to_int(str, MIN_OPTIMIZATION, "optimization"); 
      redirect_errors_to(NULL, NULL);
      XtFree(str);
      if (!(prf->got_error))
	{
	  if (opt <= MAX_OPTIMIZATION)
	    set_optimization(opt);		 
	  else va_post_prefs_error("%s > %d?", (void *)prf, str, MAX_OPTIMIZATION);
	}
      else prf->got_error = false;
    }
}

#if HAVE_EXTENSION_LANGUAGE
/* ---------------- listener-prompt ---------------- */

static void reflect_listener_prompt(prefs_info *prf)
{
  XmTextFieldSetString(prf->text, listener_prompt(ss));
}

static void listener_prompt_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if (str)
    {
      if (listener_prompt(ss)) FREE(listener_prompt(ss));
      set_listener_prompt(copy_string(str));
      XtFree(str);
    }
}

/* ---------------- show-backtrace ---------------- */

static void reflect_show_backtrace(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, show_backtrace(ss), false);
}

static void show_backtrace_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  set_show_backtrace(XmToggleButtonGetState(prf->toggle) == XmSET);
}

/* ---------------- listener-color ---------------- */

static Pixel saved_listener_color;

static void reflect_listener_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_listener_color);
  color_listener(saved_listener_color);
}

static void listener_color_func(prefs_info *prf, float r, float g, float b)
{
  XColor *tmp;
  tmp = rgb_to_color(r, g, b);
  color_listener(tmp->pixel);
  FREE(tmp);
}

/* ---------------- listener-text-color ---------------- */

static Pixel saved_listener_text_color;

static void reflect_listener_text_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_listener_text_color);
  color_listener_text(saved_listener_text_color);
}

static void listener_text_color_func(prefs_info *prf, float r, float g, float b)
{
  XColor *tmp;
  tmp = rgb_to_color(r, g, b);
  color_listener_text(tmp->pixel);
  FREE(tmp);
}

/* ---------------- listener-font ---------------- */

static void listener_font_error_erase_func(XtPointer context, XtIntervalId *id)
{
  prefs_info *prf = (prefs_info *)context;
  XmTextSetString(prf->text, listener_font(ss));
}

static void reflect_listener_font(prefs_info *prf)
{
  XmTextSetString(prf->text, listener_font(ss));
}

static void listener_font_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((!str) || (!(*str)))
    {
      XmTextSetString(prf->text, listener_font(ss));
      return;
    }
  if (!(set_listener_font(str)))
    {
      XmTextSetString(prf->text, "can't find that font");
      XtAppAddTimeOut(MAIN_APP(ss),
		      ERROR_WAIT_TIME,
		      listener_font_error_erase_func,
		      (XtPointer)prf);
    }
  if (str) XtFree(str);
}
#endif



/* ---------------- preferences dialog ---------------- */

void start_preferences_dialog(void)
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
      return;
    }

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
    char *str1, *str2;

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

    str1 = mus_format("%d", ss->init_window_width);
    str2 = mus_format("%d", ss->init_window_height);
    prf = prefs_row_with_two_texts("start up size", S_window_width, 
				   "width:", str1, "height:", str2, 6,
				   dpy_box, dpy_label,
				   startup_size_text);
    remember_pref(prf, NULL, NULL); /* this is not reflected, and is saved via window-width|height */
    FREE(str2);
    FREE(str1);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("ask before overwriting anything", S_ask_before_overwrite,
				ask_before_overwrite(ss), 
				dpy_box, current_sep,
				overwrite_toggle);
    remember_pref(prf, reflect_ask_before_overwrite, NULL);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("include thumbnail graph in upper right corner", "make-current-window-display",
				find_current_window_display(),
				dpy_box, current_sep,
				current_window_display_toggle);
    remember_pref(prf, reflect_current_window_display, NULL);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("resize main window as sounds open and close", S_auto_resize,
				auto_resize(ss), 
				dpy_box, current_sep, 
				resize_toggle);
    remember_pref(prf, reflect_auto_resize, NULL);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("show the control panel upon opening a sound", S_show_controls,
				in_show_controls(ss), 
				dpy_box, current_sep, 
				controls_toggle);
    remember_pref(prf, reflect_show_controls, NULL);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget, current_sep); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
    cursor_label = XtCreateManagedWidget("  colors", xmLabelWidgetClass, dpy_box, args, n);

    current_sep = make_inter_variable_separator(dpy_box, cursor_label);
    saved_basic_color = ss->sgx->basic_color;
    prf = prefs_color_selector_row("main background color", S_basic_color, ss->sgx->basic_color,
				   dpy_box, current_sep,
				   basic_color_func);
    remember_pref(prf, reflect_basic_color, NULL);

    current_sep = make_inter_variable_separator(dpy_box, prf->rscl);
    saved_highlight_color = ss->sgx->highlight_color;
    prf = prefs_color_selector_row("main highlight color", S_highlight_color, ss->sgx->highlight_color,
				   dpy_box, current_sep,
				   highlight_color_func);
    remember_pref(prf, reflect_highlight_color, NULL);

    current_sep = make_inter_variable_separator(dpy_box, prf->rscl);
    saved_position_color = ss->sgx->position_color;
    prf = prefs_color_selector_row("second highlight color", S_position_color, ss->sgx->position_color,
				   dpy_box, current_sep,
				   position_color_func);
    remember_pref(prf, reflect_position_color, NULL);

    current_sep = make_inter_variable_separator(dpy_box, prf->rscl);
    saved_zoom_color = ss->sgx->zoom_color;
    prf = prefs_color_selector_row("third highlight color", S_zoom_color, ss->sgx->zoom_color,
				   dpy_box, current_sep,
				   zoom_color_func);
    remember_pref(prf, reflect_zoom_color, NULL);

    current_sep = make_inter_variable_separator(dpy_box, prf->rscl);


    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget, current_sep); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
    cursor_label = XtCreateManagedWidget("  cursor options", xmLabelWidgetClass, dpy_box, args, n);

    prf = prefs_row_with_toggle("report cursor location as it moves", S_verbose_cursor,
				verbose_cursor(ss), 
				dpy_box, cursor_label, 
				verbose_cursor_toggle);
    remember_pref(prf, reflect_verbose_cursor, NULL);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("track current location while playing", S_cursor_follows_play,
				cursor_follows_play(ss), 
				dpy_box, current_sep,
				cursor_follows_play_toggle);
    remember_pref(prf, reflect_cursor_follows_play, NULL);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    
    str = mus_format("%d", cursor_size(ss));
    prf = prefs_row_with_number("size", S_cursor_size,
				str, 4, 
				dpy_box, current_sep,
				cursor_size_up, cursor_size_down, cursor_size_from_text);
    remember_pref(prf, reflect_cursor_size, NULL);
    FREE(str);
    if (cursor_size(ss) <= 0) XtSetSensitive(prf->arrow_down, false);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_radio_box("shape", S_cursor_style,
				   cursor_styles, 2, cursor_style(ss),
				   dpy_box, current_sep, 
				   cursor_style_choice);
    remember_pref(prf, reflect_cursor_style, NULL);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    saved_cursor_color = ss->sgx->cursor_color;
    prf = prefs_color_selector_row("color", S_cursor_color, ss->sgx->cursor_color,
				   dpy_box, current_sep,
				   cursor_color_func);
    remember_pref(prf, reflect_cursor_color, NULL);

    current_sep = make_inter_variable_separator(dpy_box, prf->rscl);


    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget, current_sep); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
    file_label = XtCreateManagedWidget("  file options", xmLabelWidgetClass, dpy_box, args, n);

    prf = prefs_row_with_text("directory for temporary files", S_temp_dir, 
			      temp_dir(ss), 
			      dpy_box, file_label,
			      temp_dir_text);
    remember_pref(prf, reflect_temp_dir, NULL);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
#if HAVE_EXTENSION_LANGUAGE
    prf = prefs_row_with_text("directory for save-state files", S_save_dir, 
			      save_dir(ss), 
			      dpy_box, current_sep,
			      save_dir_text);
    remember_pref(prf, reflect_save_dir, NULL);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
#endif
#if HAVE_LADSPA
    prf = prefs_row_with_text("directory for ladspa plugins", S_ladspa_dir, 
			      ladspa_dir(ss), 
			      dpy_box, current_sep,
			      ladspa_dir_text);
    remember_pref(prf, reflect_ladspa_dir, NULL);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
#endif
    prf = prefs_row_with_text("external program to read HTML files via snd-help", S_html_program,
			      html_program(ss),
			      dpy_box, current_sep,
			      html_program_text);
    remember_pref(prf, reflect_html_program, NULL);
    current_sep = make_inter_variable_separator(dpy_box, prf->label);

    /* new file defaults
    ss->Default_Output_Chans = DEFAULT_OUTPUT_CHANS;
    ss->Default_Output_Srate = DEFAULT_OUTPUT_SRATE;
    ss->Default_Output_Header_Type = DEFAULT_OUTPUT_HEADER_TYPE;
    ss->Default_Output_Data_Format = DEFAULT_OUTPUT_DATA_FORMAT;
       raw file defaults
       mus_header_raw_defaults
    */

    /*
    ss->Graphs_Horizontal = DEFAULT_GRAPHS_HORIZONTAL;
    ss->Channel_Style = DEFAULT_CHANNEL_STYLE;typedef enum {CHANNELS_SEPARATE, CHANNELS_COMBINED, CHANNELS_SUPERIMPOSED} channel_style_t;
    ss->Sound_Style = DEFAULT_SOUND_STYLE;typedef enum {SOUNDS_VERTICAL, SOUNDS_HORIZONTAL, SOUNDS_IN_NOTEBOOK, SOUNDS_IN_SEPARATE_WINDOWS} sound_style_t;
    peaks envs?
    mouse->focus? ->
(add-hook! mouse-enter-listener-hook 
  (lambda (widget) 
    (focus-widget widget)))
(add-hook! mouse-enter-graph-hook 
  (lambda (snd chn) 
    (if (sound? snd)
	(let ((wids (catch 'no-such-channel
	              (lambda () (channel-widgets snd chn))
		      (lambda args #f))))
          (if wids
              (focus-widget (car wids)))))))

(add-hook! mouse-enter-listener-hook 
  (lambda (widget) 
    (focus-widget widget)))


  current-window-display
  specialized popup 

    initial size

    ss->Show_Indices = DEFAULT_SHOW_INDICES;

    file options?
    ss->Just_Sounds = DEFAULT_JUST_SOUNDS;
    ss->HTML_Dir = NULL;
    ss->HTML_Program = copy_string(DEFAULT_HTML_PROGRAM);

    inner label:
    inner: optional menus and menu items
    marks-menu
    oscope
    etc
    */

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
    char *str;

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

    prf = prefs_row_with_radio_box("how to connect the dots", S_graph_style,
				   graph_styles, 5, graph_style(ss),
				   grf_box, grf_label,
				   graph_style_choice);
    remember_pref(prf, reflect_graph_style, NULL);

    current_sep = make_inter_variable_separator(grf_box, prf->label);
    str = mus_format("%d", dot_size(ss));
    prf = prefs_row_with_number("dot size", S_dot_size,
				str, 4, 
				grf_box, current_sep,
				dot_size_up, dot_size_down, dot_size_from_text);
    remember_pref(prf, reflect_dot_size, NULL);
    FREE(str);
    if (dot_size(ss) <= 0) XtSetSensitive(prf->arrow_down, false);

    current_sep = make_inter_variable_separator(grf_box, prf->label);
    prf = prefs_row_with_toggle("include y=0 line in sound graphs", S_show_y_zero,
				show_y_zero(ss),
				grf_box, current_sep,
				y_zero_toggle);
    remember_pref(prf, reflect_show_y_zero, NULL);

    current_sep = make_inter_variable_separator(grf_box, prf->label);
    prf = prefs_row_with_toggle("include a grid in sound graphs", S_show_grid,
				(show_grid(ss) == WITH_GRID),
				grf_box, current_sep,
				grid_toggle);
    remember_pref(prf, reflect_show_grid, NULL);

    current_sep = make_inter_variable_separator(grf_box, prf->label);
    prf = prefs_row_with_scale("grid density", S_grid_density, 
			       2.0, grid_density(ss),
			       grf_box, current_sep,
			       grid_density_scale_callback, grid_density_text_callback);
    remember_pref(prf, reflect_grid_density, NULL);

    current_sep = make_inter_variable_separator(grf_box, prf->label); 

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget, current_sep); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
    colgrf_label = XtCreateManagedWidget("  graph colors", xmLabelWidgetClass, grf_box, args, n);

    saved_data_color = ss->sgx->data_color;    
    prf = prefs_color_selector_row("unselected data (waveform) color", S_data_color, ss->sgx->data_color,
				   grf_box, colgrf_label,
				   data_color_func);
    remember_pref(prf, reflect_data_color, NULL);

    current_sep = make_inter_variable_separator(grf_box, prf->rscl);
    saved_graph_color = ss->sgx->graph_color;
    prf = prefs_color_selector_row("unselected graph (background) color", S_graph_color, ss->sgx->graph_color,
				   grf_box, current_sep,
				   graph_color_func);
    remember_pref(prf, reflect_graph_color, NULL);

    current_sep = make_inter_variable_separator(grf_box, prf->rscl);
    saved_selected_data_color = ss->sgx->selected_data_color;
    prf = prefs_color_selector_row("selected data (waveform) color", S_selected_data_color, ss->sgx->selected_data_color,
				   grf_box, current_sep,
				   selected_data_color_func);
    remember_pref(prf, reflect_selected_data_color, NULL);

    current_sep = make_inter_variable_separator(grf_box, prf->rscl);
    saved_selected_graph_color = ss->sgx->selected_graph_color;
    prf = prefs_color_selector_row("selected graph (background) color", S_selected_graph_color, ss->sgx->selected_graph_color,
				   grf_box, current_sep,
				   selected_graph_color_func);
    remember_pref(prf, reflect_selected_graph_color, NULL);

    current_sep = make_inter_variable_separator(grf_box, prf->rscl);
    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget, current_sep); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
    colgrf_label = XtCreateManagedWidget("  graph fonts", xmLabelWidgetClass, grf_box, args, n);

    prf = prefs_row_with_text("axis label font", S_axis_label_font, 
			      axis_label_font(ss), 
			      grf_box, colgrf_label,
			      axis_label_font_text);
    remember_pref(prf, reflect_axis_label_font, NULL);

    current_sep = make_inter_variable_separator(grf_box, prf->label);     
    prf = prefs_row_with_text("axis number font", S_axis_numbers_font, 
			      axis_numbers_font(ss), 
			      grf_box, current_sep,
			      axis_numbers_font_text);
    remember_pref(prf, reflect_axis_numbers_font, NULL);

    current_sep = make_inter_variable_separator(grf_box, prf->label);     
    prf = prefs_row_with_text("fft peaks font", S_peaks_font, 
			      peaks_font(ss), 
			      grf_box, current_sep,
			      peaks_font_text);
    remember_pref(prf, reflect_peaks_font, NULL);

    current_sep = make_inter_variable_separator(grf_box, prf->label);     
    prf = prefs_row_with_text("fft peaks bold font (for main peaks)", S_bold_peaks_font, 
			      bold_peaks_font(ss), 
			      grf_box, current_sep,
			      bold_peaks_font_text);
    remember_pref(prf, reflect_bold_peaks_font, NULL);

    current_sep = make_inter_variable_separator(grf_box, prf->label);     
    prf = prefs_row_with_text("tiny font (for various annotations)", S_peaks_font, 
			      tiny_font(ss), 
			      grf_box, current_sep,
			      tiny_font_text);
    remember_pref(prf, reflect_tiny_font, NULL);

    /*
    ss->X_Axis_Style = DEFAULT_X_AXIS_STYLE;typedef enum {X_AXIS_IN_SECONDS, X_AXIS_IN_SAMPLES, X_AXIS_AS_PERCENTAGE, X_AXIS_IN_BEATS, X_AXIS_IN_MEASURES} x_axis_style_t;
    ss->Show_Axes = DEFAULT_SHOW_AXES;typedef enum {SHOW_NO_AXES, SHOW_ALL_AXES, SHOW_X_AXIS, SHOW_ALL_AXES_UNLABELLED, SHOW_X_AXIS_UNLABELLED} show_axes_t;
    graph fonts (tiny?)
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

    recorder stuff?
    audio mixer settings?
  */
  }

  current_sep = make_inter_topic_separator(topics);


  /* -------- transform -------- */
  {
    Widget fft_frame, fft_box, fft_label;
    char *str;

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
    prf = prefs_row_with_number("size", S_transform_size,
				str, 12, 
				fft_box, fft_label, 
				fft_size_up, fft_size_down, fft_size_from_text);
    remember_pref(prf, reflect_fft_size, NULL);
    FREE(str);
    if (transform_size(ss) <= 2) XtSetSensitive(prf->arrow_down, false);

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    prf = prefs_row_with_radio_box("transform graph choice", S_transform_graph_type,
				   transform_graph_types, 3, transform_graph_type(ss),
				   fft_box, current_sep,
				   transform_graph_type_choice);
    remember_pref(prf, reflect_transform_graph_type, NULL);

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    prf = prefs_row_with_completed_list("transform", S_transform_type, transform_types[transform_type(ss)],
					transform_types, NUM_TRANSFORM_TYPES,
					fft_box, current_sep,
					transform_type_from_text,
					transform_type_completer, NULL,
					transform_type_from_menu);
    remember_pref(prf, reflect_transform_type, NULL);

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    prf = prefs_row_with_completed_list("data window", S_fft_window, fft_windows[(int)fft_window(ss)],
					fft_windows, NUM_FFT_WINDOWS,
					fft_box, current_sep,
					fft_window_from_text,
					fft_window_completer, NULL,
					fft_window_from_menu);
    remember_pref(prf, reflect_fft_window, NULL);

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    prf = prefs_row_with_scale("data window family parameter", S_fft_window_beta, 
			       1.0, fft_window_beta(ss),
			       fft_box, current_sep,
			       fft_window_beta_scale_callback, fft_window_beta_text_callback);
    remember_pref(prf, reflect_fft_window_beta, NULL);

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    str = mus_format("%d", max_transform_peaks(ss));
    prf = prefs_row_with_toggle_with_text("show fft peak data", S_show_transform_peaks,
					  show_transform_peaks(ss),
					  "max peaks:", str, 5,
					  fft_box, current_sep,
					  transform_peaks_toggle, transform_peaks_text);
    remember_pref(prf, reflect_show_transform_peaks, NULL);
    FREE(str);

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    {
      const char **cmaps;
      int i, len;
      len = num_colormaps();
      cmaps = (const char **)CALLOC(len, sizeof(const char *));
      for (i = 0; i < len; i++)
	cmaps[i] = (const char *)colormap_name(i);
      prf = prefs_row_with_completed_list("sonogram colormap", S_colormap, cmaps[color_map(ss)],
					  cmaps, len,
					  fft_box, current_sep,
					  colormap_from_text,
					  colormap_completer, NULL,
					  colormap_from_menu);
      remember_pref(prf, reflect_colormap, NULL);
      FREE(cmaps);
    }
    /* PERHAPS: a line with color-inverted and color-scaler and color-cutoff */


    /* here min-db as text */
    current_sep = make_inter_variable_separator(fft_box, prf->label);
    prf = prefs_row_with_toggle("y axis as log magnitude (dB)", S_fft_log_magnitude,
				fft_log_magnitude(ss),
				fft_box, current_sep,
				log_magnitude_toggle);
    remember_pref(prf, reflect_fft_log_magnitude, NULL);

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    str = mus_format("%.3f", min_dB(ss));
    prf = prefs_row_with_text("minimum y-axis dB value", S_min_dB, str,
			      fft_box, current_sep,
			      min_dB_text);
    remember_pref(prf, reflect_min_dB, NULL);
    FREE(str);

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    prf = prefs_row_with_toggle("x axis as log freq", S_fft_log_frequency,
				fft_log_frequency(ss),
				fft_box, current_sep,
				log_frequency_toggle);
    remember_pref(prf, reflect_fft_log_frequency, NULL);

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    prf = prefs_row_with_radio_box("normalization", S_transform_normalization,
				   transform_normalizations, 4, transform_normalization(ss),
				   fft_box, current_sep,
				   transform_normalization_choice);
    remember_pref(prf, reflect_transform_normalization, NULL);

    /*
    ss->Sinc_Width = DEFAULT_SINC_WIDTH;
    ss->With_GL = DEFAULT_WITH_GL;

    */
    
  }

  current_sep = make_inter_topic_separator(topics);

  /* -------- marks, mixes, and regions -------- */
  {
    Widget mmr_frame, mmr_box, mmr_label;
    char *str1, *str2;

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

    saved_mark_color = ss->sgx->mark_color;
    prf = prefs_color_selector_row("mark and mix tag color", S_mark_color, ss->sgx->mark_color,
				   mmr_box, mmr_label,
				   mark_color_func);
    remember_pref(prf, reflect_mark_color, NULL);

    current_sep = make_inter_variable_separator(mmr_box, prf->rscl);

    str1 = mus_format("%d", mark_tag_width(ss));
    str2 = mus_format("%d", mark_tag_height(ss));
    prf = prefs_row_with_two_texts("mark tag size", S_mark_tag_width, 
				   "width:", str1, "height:", str2, 4,
				   mmr_box, current_sep,
				   mark_tag_size_text);
    remember_pref(prf, reflect_mark_tag_size, NULL);
    FREE(str2);
    FREE(str1);

    current_sep = make_inter_variable_separator(mmr_box, prf->label);
    str1 = mus_format("%d", mix_tag_width(ss));
    str2 = mus_format("%d", mix_tag_height(ss));
    prf = prefs_row_with_two_texts("mix tag size", S_mix_tag_width, 
				   "width:", str1, "height:", str2, 4,
				   mmr_box, current_sep,
				   mix_tag_size_text);
    remember_pref(prf, reflect_mix_tag_size, NULL);
    FREE(str2);
    FREE(str1);

    current_sep = make_inter_variable_separator(mmr_box, prf->label);
    saved_mix_color = ss->sgx->mix_color;
    prf = prefs_color_selector_row("mix waveform color", S_mix_color, ss->sgx->mix_color,
				   mmr_box, current_sep,
				   mix_color_func);
    remember_pref(prf, reflect_mix_color, NULL);

    current_sep = make_inter_variable_separator(mmr_box, prf->rscl);
    str = mus_format("%d", mix_waveform_height(ss));
    prf = prefs_row_with_toggle_with_text("show mix waveforms (attached to the mix tag)", S_show_mix_waveforms,
					  show_mix_waveforms(ss),
					  "max waveform height:", str, 5,
					  mmr_box, current_sep,
					  show_mix_waveforms_toggle, mix_waveform_height_text);
    remember_pref(prf, reflect_show_mix_waveforms, NULL);
    FREE(str);


    /* current_sep = make_inter_variable_separator(mmr_box, prf->rscl); */


    /*
    ss->Selection_Creates_Region = DEFAULT_SELECTION_CREATES_REGION;
    marks menu?
    */
    
  }
  
#if HAVE_EXTENSION_LANGUAGE
  current_sep = make_inter_topic_separator(topics);

  /* -------- programming -------- */
  {
    Widget prg_frame, prg_box, prg_label;
    char *str;

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
    prg_label = XtCreateManagedWidget("listener options", xmLabelWidgetClass, prg_box, args, n);

#if HAVE_SCHEME
    str = mus_format("%d", optimization(ss));
    prf = prefs_row_with_number("optimization level", S_optimization,
				str, 3, 
				prg_box, prg_label, 
				optimization_up, optimization_down, optimization_from_text);
    remember_pref(prf, reflect_optimization, NULL);
    FREE(str);
    if (optimization(ss) == 6) XtSetSensitive(prf->arrow_up, false);
    if (optimization(ss) == 0) XtSetSensitive(prf->arrow_down, false);
    current_sep = make_inter_variable_separator(prg_box, prf->label);
#else
    current_sep = prg_label;
#endif

    prf = prefs_row_with_text("prompt", S_listener_prompt, 
			      listener_prompt(ss), 
			      prg_box, current_sep,
			      listener_prompt_text);
    remember_pref(prf, reflect_listener_prompt, NULL);

    current_sep = make_inter_variable_separator(prg_box, prf->label);
    prf = prefs_row_with_toggle("include backtrace in error report", S_show_backtrace,
				show_backtrace(ss),
				prg_box, current_sep,
				show_backtrace_toggle);
    remember_pref(prf, reflect_show_backtrace, NULL);

    current_sep = make_inter_variable_separator(prg_box, prf->label);
    saved_listener_color = ss->sgx->listener_color;
    prf = prefs_color_selector_row("background color", S_listener_color, ss->sgx->listener_color,
				   prg_box, current_sep,
				   listener_color_func);
    remember_pref(prf, reflect_listener_color, NULL);

    current_sep = make_inter_variable_separator(prg_box, prf->rscl);
    saved_listener_text_color = ss->sgx->listener_text_color;
    prf = prefs_color_selector_row("text color", S_listener_text_color, ss->sgx->listener_text_color,
				   prg_box, current_sep,
				   listener_text_color_func);
    remember_pref(prf, reflect_listener_text_color, NULL);

    current_sep = make_inter_variable_separator(prg_box, prf->rscl);     
    prf = prefs_row_with_text("font", S_listener_font, 
			      listener_font(ss), 
			      prg_box, current_sep,
			      listener_font_text);
    remember_pref(prf, reflect_listener_font, NULL);

    /*
    clm: ws.scm + any instruments

    autosave
    debugging aids

    edit-menu
    effects menu
    extensions
    nb.scm (buffer for it)

    snd-motif: hidden-controls-dialog
    disable-control-panel
    smpte level and disk-space
    */
  }
#endif  

#if DEBUGGING
  fprintf(stderr, "top: %d\n", prefs_top);
#endif

  XtManageChild(preferences_dialog);
  set_dialog_widget(PREFERENCES_DIALOG, preferences_dialog);
}

/*
hooks -- peak env
locale?
package deals
optional menus/files/key bindings
*/

