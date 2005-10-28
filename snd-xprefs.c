#include "snd.h"

/* preferences dialog; layout design taken from webmail
 */

/* things left to do:

    line cursor during play?
    preset packages
    snd-test somehow

   remember state for subsequent load (extensions.scm)
     (remember-sound-state) ;also needs turn-off code

   snd-motif: hidden-controls-dialog (make-hidden-controls-dialog)
              sync'd zoom slider, separate amp controls
   add sound file exts? -- would need display of existing exts
   graph-cursor? [drawing area + x cursor setters] [arrow, cross, beam, circle]
   mark pane?
   colormap: a line with color-inverted and color-scaler and color-cutoff
   check out gui.scm et al 
   various additional key bindings? move-one-pixel zoom-one-pixel [how to specify fancy keys?]
   nb.scm (buffer for it)

    audio section: (gtk side too)
    dac-combines-channels, cursor offset, smart line cursor, recorder setting, audio mixer settings
    ss->Dac_Combines_Channels = DEFAULT_DAC_COMBINES_CHANNELS;
    recorder stuff? -> buffer-size in-chans in-data-format in-device out-chans out-data-format out-header-type srate
    audio mixer settings? -> volume in some mode
    audio output device? or sound card?

    raw file defaults
    -> mus_header_raw_defaults [chans srate format in headers.c]

      clm ins, prc ins, v+jcrev+nrev, birds
      clm table size, file buffer size, default srate?
*/


static Widget preferences_dialog = NULL;
static bool prefs_helping = false, prefs_unsaved = false;
static char *prefs_saved_filename = NULL;
static char *prefs_time = NULL;

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
#define ERROR_WAIT_TIME 1000

#define STARTUP_WIDTH 925
#define STARTUP_HEIGHT 800

typedef struct prefs_info {
  Widget label, text, arrow_up, arrow_down, arrow_right, error, toggle, scale;
  Widget color, rscl, gscl, bscl, rtxt, gtxt, btxt, list_menu, radio_button;
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


/* ---------------- utilities ---------------- */

static char *trim_string(const char *str)
{
  int i = 0, len, j = 0, k, m;
  char *trimmed_str;
  len = strlen(str);
  trimmed_str = (char *)CALLOC(len + 1, sizeof(char));
  while ((i < len) && (isspace(str[i]))) i++;
  k = len - 1;
  while ((k > i) && (isspace(str[k]))) k--;
  for (m = i; m <= k; m++)
    trimmed_str[j++] = str[m];
  return(trimmed_str);
}

static void int_to_textfield(Widget w, int val)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(w), w);
  str = (char *)CALLOC(16, sizeof(char));
  mus_snprintf(str, 16, "%d", val);
  XmTextFieldSetString(w, str);
  FREE(str);
}

static void float_to_textfield(Widget w, Float val)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(w), w);
  str = (char *)CALLOC(12, sizeof(char));
  mus_snprintf(str, 12, "%.3f", val);
  XmTextFieldSetString(w, str);
  FREE(str);
}

static void float_1_to_textfield(Widget w, Float val)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(w), w);
  str = (char *)CALLOC(12, sizeof(char));
  mus_snprintf(str, 12, "%.1f", val);
  XmTextFieldSetString(w, str);
  FREE(str);
}


#include <X11/IntrinsicP.h>

static Widget find_radio_button(Widget parent, const char *name)
{
  unsigned int i;
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
    {
      XEN sym;
      sym = C_STRING_TO_XEN_SYMBOL((char *)(prf->var_name));
      if (XEN_SYMBOL_P(sym))
	{
	  XEN obj;
	  obj = XEN_OBJECT_HELP(sym);
	  if (XEN_STRING_P(obj))
	    {
	      prefs_helping = true;
	      snd_help(prf->var_name, 
		       XEN_TO_C_STRING(obj),
		       WITH_WORD_WRAP);
	    }
	}
    }
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

static bool prefs_dialog_error_is_posted = false;

static void post_prefs_dialog_error(const char *message, void *data)
{
  XmString title;
  title = XmStringCreate((char *)message, XmFONTLIST_DEFAULT_TAG);
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

static void prefs_set_dialog_title(const char *filename);
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

  XtAddEventHandler(w, EnterWindowMask, false, mouse_enter_pref_callback, (XtPointer)prf);
  XtAddEventHandler(w, LeaveWindowMask, false, mouse_leave_pref_callback, (XtPointer)prf);

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

  return(w);
}

/* ---------------- row toggle widget ---------------- */

static Widget make_row_toggle(prefs_info *prf, bool current_value, Widget left_widget, Widget box, Widget top_widget)
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
  w = XtCreateManagedWidget(" ", xmToggleButtonWidgetClass, box, args, n);

  XtAddEventHandler(w, EnterWindowMask, false, mouse_enter_pref_callback, (XtPointer)prf);
  XtAddEventHandler(w, LeaveWindowMask, false, mouse_leave_pref_callback, (XtPointer)prf);
  XtAddCallback(w, XmNvalueChangedCallback, prefs_change_callback, NULL);

  return(w);
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
  help = make_row_help(prf, varname, box, top_widget, prf->text);
  
  XtAddCallback(prf->toggle, XmNvalueChangedCallback, call_toggle_func, (XtPointer)prf);
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
      button = XtCreateManagedWidget(labels[i], xmToggleButtonWidgetClass, w, args, n);

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

#if 0
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
#endif

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

static void wm_delete_callback (Widget w, XtPointer context, XtPointer info) 
{
  clear_prefs_dialog_error();
  prefs_helping = false;
}

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
  prefs_helping = false;
  clear_prefs_dialog_error();
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
    {
      if (prefs_time)
	str = mus_format("Preferences%s (%s: saved in %s)\n",
			 (prefs_unsaved) ? "*" : "",
			 prefs_time,
			 prefs_saved_filename);
      else str = mus_format("Preferences%s (saved in %s)\n",
			    (prefs_unsaved) ? "*" : "",
			    prefs_saved_filename);
    }
  else str = mus_format("Preferences%s",
			(prefs_unsaved) ? "*" : "");
  title = XmStringCreate(str, XmFONTLIST_DEFAULT_TAG);
  FREE(str);
  XtVaSetValues(preferences_dialog, 
		XmNdialogTitle, title, 
		NULL);
  XmStringFree(title);
}

static void preferences_reset_callback(Widget w, XtPointer context, XtPointer info) 
{
  clear_prefs_dialog_error();
  snd_set_global_defaults(true); 
  reflect_prefs();
  prefs_unsaved = false;
  if (prefs_saved_filename) 
    {
      char *fullname;
      fullname = mus_expand_filename(prefs_saved_filename);
      if (mus_file_probe(fullname))
	snd_remove(fullname, IGNORE_CACHE);
      FREE(prefs_saved_filename);
      FREE(fullname);
      prefs_saved_filename = NULL;
      if (prefs_time)
	{
	  FREE(prefs_time);
	  prefs_time = NULL;
	}
    }
  prefs_set_dialog_title(NULL);
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
#if HAVE_STRFTIME
  {
    time_t ts;
    time(&ts);
    if (!prefs_time) prefs_time = (char *)CALLOC(TIME_STR_SIZE, sizeof(char));
    strftime(prefs_time, TIME_STR_SIZE, "%H:%M", localtime(&ts));
  }
#endif
  prefs_unsaved = false;
  prefs_set_dialog_title(filename);
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

static void post_prefs_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  ASSERT_WIDGET_TYPE(XmIsLabel(prf->error), prf->error);
  prf->got_error = true;
  set_label(prf->error, msg);
  XtAddCallback(prf->text, XmNvalueChangedCallback, clear_prefs_error, (XtPointer)prf);
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

#if 0
/* ---------------- customization choice ---------------- */

    prf = prefs_row_with_radio_box("preset customization packages", "customization",
				   customization_choices, 4, "none",
				   dpy_box, dpy_label,
				   customization_choice);
    remember_pref(prf, reflect_customization_choice, NULL);
#endif


/* ---------------- start up size ---------------- */

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
  XmTextFieldSetString(prf->text, "right");
  XtAppAddTimeOut(MAIN_APP(ss),
		  ERROR_WAIT_TIME,
		  startup_width_erase_func,
		  (XtPointer)prf);
}

static void startup_height_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  XmTextFieldSetString(prf->rtxt, "right");
  XtAppAddTimeOut(MAIN_APP(ss),
		  ERROR_WAIT_TIME,
		  startup_height_erase_func,
		  (XtPointer)prf);
}

static void startup_size_text(prefs_info *prf)
{
  char *str;
  str = XmTextFieldGetString(prf->text);
  if ((str) && (*str))
    {
      int width = 0;
      redirect_errors_to(startup_width_error, (void *)prf);
      width = string_to_int(str, 1, "startup width");
      redirect_errors_to(NULL, NULL);
      if (width > 0) ss->init_window_width = width;
      XtFree(str);
      str = XmTextFieldGetString(prf->rtxt);
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

static bool include_current_window_display = false;

static void save_current_window_display(prefs_info *prf, FILE *fd)
{
  if (include_current_window_display)
    {
#if HAVE_SCHEME
      fprintf(fd, "(if (not (provided? 'snd-draw.scm)) (load-from-path \"draw.scm\"))\n");
      fprintf(fd, "(make-current-window-display)\n");
#endif
#if HAVE_RUBY
      fprintf(fd, "require \"draw\"\n");
      fprintf(fd, "make_current_window_display\n");
#endif
    }
}

static void current_window_display_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  include_current_window_display = (XmToggleButtonGetState(prf->toggle) == XmSET);
}

static bool find_current_window_display(void)
{
#if HAVE_SCHEME
  /* there's no clean way to look for the functions on the hook lists, so I'll kludge up
   *   some variable...
   */
  return((XEN_DEFINED_P("current-window-display-is-running")) &&
	 (XEN_TRUE_P(XEN_NAME_AS_C_STRING_TO_VALUE("current-window-display-is-running"))));
#endif
#if HAVE_RUBY
  return(strcmp(XEN_AS_STRING(XEN_EVAL_C_STRING("defined? Current_window")), "constant") == 0);
#endif
}

static void reflect_current_window_display(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, find_current_window_display(), false);
}

/* ---------------- focus-follows-mouse ---------------- */

static bool focus_follows_mouse = false;

static bool focus_is_following_mouse(void)
{
  return((XEN_DEFINED_P("focus-is-following-mouse")) &&
	 (XEN_TRUE_P(XEN_NAME_AS_C_STRING_TO_VALUE("focus-is-following-mouse"))));
}

static void reflect_focus_follows_mouse(prefs_info *prf) 
{
  focus_follows_mouse = focus_is_following_mouse();
  XmToggleButtonSetState(prf->toggle, focus_follows_mouse, false);
}

static void focus_follows_mouse_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  focus_follows_mouse = (XmToggleButtonGetState(prf->toggle) == XmSET);
}

static void save_focus_follows_mouse(prefs_info *prf, FILE *fd) 
{
  if (focus_follows_mouse)
    {
#if HAVE_SCHEME
      fprintf(fd, "(if (not (provided? 'snd-extensions.scm)) (load-from-path \"extensions.scm\"))\n");
      fprintf(fd, "(focus-follows-mouse)\n");
#endif
#if HAVE_RUBY
      /* TODO: ruby side of focus-follows-mouse */
#endif
    }
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

/* ---------------- peak-envs ---------------- */

/*
  ruby: this is in env.rb
  also gtk
*/

static bool include_peak_envs = false;
static char *include_peak_env_directory = NULL;

static bool find_peak_envs(void)
{
#if HAVE_GUILE
  return((XEN_DEFINED_P("save-peak-env-info?")) &&
	 XEN_TO_C_BOOLEAN(XEN_NAME_AS_C_STRING_TO_VALUE("save-peak-env-info?")));
#endif
#if HAVE_RUBY
  return(XEN_DEFINED_P("install_save_peak_env");
#endif
}

static char *peak_env_directory(void)
{
  if (include_peak_env_directory)
    return(include_peak_env_directory);
#if HAVE_GUILE
  if (XEN_DEFINED_P("save-peak-env-info-directory"))
    return(XEN_TO_C_STRING(XEN_NAME_AS_C_STRING_TO_VALUE("save-peak-env-info-directory")));
#endif
#if HAVE_RUBY
  /* TODO: ruby side of peak env dir */
#endif
  return(NULL);
}

static void reflect_peak_envs(prefs_info *prf) 
{
  if (include_peak_env_directory) {FREE(include_peak_env_directory); include_peak_env_directory = NULL;}
  include_peak_env_directory = copy_string(peak_env_directory());
  include_peak_envs = find_peak_envs();
  XmToggleButtonSetState(prf->toggle, include_peak_envs, false);
  XmTextFieldSetString(prf->text, include_peak_env_directory);
}

static void peak_envs_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  include_peak_envs = (XmToggleButtonGetState(prf->toggle) == XmSET);
}

static void peak_envs_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((str) && (*str))
    {
      if (include_peak_env_directory) {FREE(include_peak_env_directory); include_peak_env_directory = NULL;}
      include_peak_env_directory = copy_string(str);
      XtFree(str);
    }
}

static void save_peak_envs(prefs_info *prf, FILE *fd)
{
  if (include_peak_envs)
    {
#if HAVE_GUILE
      fprintf(fd, "(if (not (provided? 'snd-peak-env.scm)) (load-from-path \"peak-env.scm\"))\n");
      if (include_peak_env_directory)
	fprintf(fd, "(set! save-peak-env-info-directory \"%s\")\n", include_peak_env_directory);
#endif
    }
}


/* ---------------- selection-creates-region, max-regions ---------------- */

static void reflect_selection_creates_region(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, selection_creates_region(ss), false);
  int_to_textfield(prf->text, max_regions(ss));
}

static void selection_creates_region_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  set_selection_creates_region(XmToggleButtonGetState(prf->toggle) == XmSET);
}

static void max_regions_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((str) && (*str))
    {
      int value = 0;
      sscanf(str, "%d", &value);
      if (value >= 0)
	in_set_max_regions(value);
      else int_to_textfield(prf->text, max_regions(ss));
      XtFree(str);
    }
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
  int_to_textfield(prf->rtxt, cursor_location_offset(ss));
  float_to_textfield(prf->text, cursor_update_interval(ss));
}

static void cursor_follows_play_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  in_set_cursor_follows_play(ss, (XmToggleButtonGetState(prf->toggle) == XmSET) ? FOLLOW_ALWAYS : DONT_FOLLOW);
}

static void cursor_location_text(prefs_info *prf)
{
  char *str;
  str = XmTextFieldGetString(prf->text);
  if ((str) && (*str))
    {
      Float interval = DEFAULT_CURSOR_UPDATE_INTERVAL;
      sscanf(str, "%f", &interval);
      if (interval >= 0.0)
	set_cursor_update_interval(interval);
      XtFree(str);
      str = XmTextFieldGetString(prf->rtxt);
      if ((str) && (*str))
	{
	  int loc = DEFAULT_CURSOR_LOCATION_OFFSET;
	  sscanf(str, "%d", &loc);
	  set_cursor_location_offset(loc);
	  XtFree(str);
	}
    }
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
  else post_prefs_error("no size?", (XtPointer)prf);
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


/* ---------------- just-sounds ---------------- */

static void prefs_reflect_just_sounds(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, just_sounds(ss), false);
}

static void just_sounds_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  set_just_sounds(XmToggleButtonGetState(prf->toggle) == XmSET);
}


/* ---------------- temp-dir ---------------- */

static void reflect_temp_dir(prefs_info *prf)
{
  XmTextFieldSetString(prf->text, temp_dir(ss));
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
  XmTextFieldSetString(prf->text, temp_dir(ss));
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
      XmTextFieldSetString(prf->text, "can't access that directory");
      XtAppAddTimeOut(MAIN_APP(ss),
		      ERROR_WAIT_TIME,
		      temp_dir_error_erase_func,
		      (XtPointer)prf);
    }
  if (str) XtFree(str);
}

/* ---------------- save-dir ---------------- */

static void save_dir_error_erase_func(XtPointer context, XtIntervalId *id)
{
  prefs_info *prf = (prefs_info *)context;
  XmTextFieldSetString(prf->text, save_dir(ss));
}

static void reflect_save_dir(prefs_info *prf)
{
  XmTextFieldSetString(prf->text, save_dir(ss));
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
      XmTextFieldSetString(prf->text, "can't access that directory");
      XtAppAddTimeOut(MAIN_APP(ss),
		      ERROR_WAIT_TIME,
		      save_dir_error_erase_func,
		      (XtPointer)prf);
    }
  if (str) XtFree(str);
}

/* ---------------- save-state-file ---------------- */

static void reflect_save_state_file(prefs_info *prf)
{
  XmTextFieldSetString(prf->text, save_state_file(ss));
}

static void save_state_file_text(prefs_info *prf)
{
  char *str, *file = NULL;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((!str) || (!(*str))) 
    file = DEFAULT_SAVE_STATE_FILE;
  else file = str;
  if (save_state_file(ss)) FREE(save_state_file(ss));
  in_set_save_state_file(copy_string(file));
  if (str) XtFree(str);
}


#if HAVE_LADSPA
/* ---------------- ladspa-dir ---------------- */

static void reflect_ladspa_dir(prefs_info *prf)
{
  XmTextFieldSetString(prf->text, ladspa_dir(ss));
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


/* ---------------- view-files directory ---------------- */

static char *include_vf_directory = NULL;

static void reflect_view_files_directory(prefs_info *prf)
{
  if (include_vf_directory) FREE(include_vf_directory);
  include_vf_directory = copy_string(view_files_find_any_directory());
  XmTextFieldSetString(prf->text, view_files_find_any_directory());
}

static void view_files_directory_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if (include_vf_directory) FREE(include_vf_directory);
  include_vf_directory = copy_string(str); /* could be null to cancel */
  if ((!str) || (!(*str)))
    {
      view_files_add_directory(NULL_WIDGET, (const char *)str);
      XtFree(str);
    }
}

static void save_view_files_directory(prefs_info *prf, FILE *fd)
{
  if (include_vf_directory)
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s %s)\n", S_add_directory_to_view_files_list, include_vf_directory);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(%s)\n", TO_PROC_NAME(S_add_directory_to_view_files_list), include_vf_directory);
#endif
    }
}


/* ---------------- html-program ---------------- */

static void reflect_html_program(prefs_info *prf)
{
  XmTextFieldSetString(prf->text, html_program(ss));
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

/* ---------------- default-output-chans etc ---------------- */

static const char *output_chan_choices[4] = {"1", "2", "4", "8"};
static const char *output_srate_choices[4] = {"8000", "22050", "44100", "48000"};

static void reflect_output_chans(prefs_info *prf)
{
  char *str;
  Widget w;
  str = (char *)CALLOC(6, sizeof(char));
  mus_snprintf(str, 6, "%d", default_output_chans(ss));
  w = find_radio_button(prf->toggle, str);
  if (w)
    XmToggleButtonSetState(w, XmSET, false);
  else fprintf(stderr, "can't find %s\n", str);
  FREE(str);
  if ((prf->radio_button) &&
      (XmIsToggleButton(prf->radio_button)) &&
      (w != prf->radio_button))
    {
      XmToggleButtonSetState(prf->radio_button, XmUNSET, false);
      prf->radio_button = w;
    }
}

static void reflect_output_srate(prefs_info *prf)
{
  char *str;
  Widget w;
  str = (char *)CALLOC(8, sizeof(char));
  mus_snprintf(str, 8, "%d", default_output_srate(ss));
  w = find_radio_button(prf->toggle, str);
  if (w)
    XmToggleButtonSetState(w, XmSET, false);
  else fprintf(stderr, "can't find %s\n", str);
  FREE(str);
  if ((prf->radio_button) &&
      (XmIsToggleButton(prf->radio_button)) &&
      (w != prf->radio_button))
    {
      XmToggleButtonSetState(prf->radio_button, XmUNSET, false);
      prf->radio_button = w;
    }
}

static void output_chans_choice(prefs_info *prf)
{
  if (XmToggleButtonGetState(prf->radio_button) == XmSET)
    {
      if (strcmp(XtName(prf->radio_button), "1") == 0)
	set_default_output_chans(1);
      else
	{
	  if (strcmp(XtName(prf->radio_button), "2") == 0)
	    set_default_output_chans(2);
	  else
	    {
	      if (strcmp(XtName(prf->radio_button), "4") == 0)
		set_default_output_chans(4);
	      else set_default_output_chans(8);
	    }
	}
    }
}

static void output_srate_choice(prefs_info *prf)
{
  if (XmToggleButtonGetState(prf->radio_button) == XmSET)
    {
      if (strcmp(XtName(prf->radio_button), "8000") == 0)
	set_default_output_srate(8000);
      else
	{
	  if (strcmp(XtName(prf->radio_button), "22050") == 0)
	    set_default_output_srate(22050);
	  else
	    {
	      if (strcmp(XtName(prf->radio_button), "44100") == 0)
		set_default_output_srate(44100);
	      else set_default_output_srate(48000);
	    }
	}
    }
}

static const char *output_type_choices[5] = {"aifc", "wave", "next/sun", "nist", "aiff"};
static const char *output_format_choices[4] = {"short", "int", "float", "double"};

static void reflect_output_type(prefs_info *prf)
{
  char *str = "not a choice";
  Widget w;
  switch (default_output_header_type(ss))
    {
    case MUS_AIFC: str = "aifc"; break;
    case MUS_AIFF: str = "aiff"; break;
    case MUS_RIFF: str = "wave"; break;
    case MUS_NEXT: str = "next/sun"; break;
    case MUS_NIST: str = "nist"; break;
    }
  w = find_radio_button(prf->toggle, str);
  if (w)
    XmToggleButtonSetState(w, XmSET, false);
  else fprintf(stderr, "can't find %s\n", str);
  if ((prf->radio_button) &&
      (XmIsToggleButton(prf->radio_button)) &&
      (w != prf->radio_button))
    {
      XmToggleButtonSetState(prf->radio_button, XmUNSET, false);
      prf->radio_button = w;
    }
}

static void reflect_output_format(prefs_info *prf)
{
  char *str = "not a choice";
  Widget w;
  switch (default_output_data_format(ss))
    {
    case MUS_LINT: case MUS_BINT: str = "int"; break;
    case MUS_LSHORT: case MUS_BSHORT: str = "short"; break;
    case MUS_LFLOAT: case MUS_BFLOAT: str = "float"; break;
    case MUS_LDOUBLE: case MUS_BDOUBLE: str = "double"; break;
    }
  w = find_radio_button(prf->toggle, str);
  if (w)
    XmToggleButtonSetState(w, XmSET, false);
  else fprintf(stderr, "can't find %s\n", str);
  if ((prf->radio_button) &&
      (XmIsToggleButton(prf->radio_button)) &&
      (w != prf->radio_button))
    {
      XmToggleButtonSetState(prf->radio_button, XmUNSET, false);
      prf->radio_button = w;
    }
}

static prefs_info *output_data_format_prf = NULL, *output_header_type_prf = NULL;

static void output_type_choice(prefs_info *prf)
{
  if (XmToggleButtonGetState(prf->radio_button) == XmSET)
    {
      if (strcmp(XtName(prf->radio_button), "aifc") == 0)
	set_default_output_header_type(MUS_AIFC);
      else
	{
	  if (strcmp(XtName(prf->radio_button), "wave") == 0)
	    set_default_output_header_type(MUS_RIFF);
	  else
	    {
	      if (strcmp(XtName(prf->radio_button), "next/sun") == 0)
		set_default_output_header_type(MUS_NEXT);
	      else 
		{
		  if (strcmp(XtName(prf->radio_button), "nist") == 0)
		    set_default_output_header_type(MUS_NIST);
		  else set_default_output_header_type(MUS_AIFF);
		}
	    }
	}

      /* nist -> short or int (lb)
	 aiff -> short or int (b)
	 aifc -> any (b)
	 next -> any (b)
	 wave -> any (l)
      */
      switch (default_output_header_type(ss))
	{
	case MUS_NEXT: case MUS_AIFC:
	  switch (default_output_data_format(ss))
	    {
	    case MUS_LSHORT: set_default_output_data_format(MUS_BSHORT); break;
	    case MUS_LINT: set_default_output_data_format(MUS_BINT); break;
	    case MUS_LFLOAT: set_default_output_data_format(MUS_BFLOAT); break;
	    case MUS_LDOUBLE: set_default_output_data_format(MUS_BDOUBLE); break;
	    }
	  break;
	case MUS_AIFF:
	  switch (default_output_data_format(ss))
	    {
	    case MUS_LSHORT: set_default_output_data_format(MUS_BSHORT); break;
	    case MUS_LINT: set_default_output_data_format(MUS_BINT); break;
	    case MUS_LFLOAT: case MUS_LDOUBLE: case MUS_BFLOAT: case MUS_BDOUBLE: set_default_output_data_format(MUS_BINT); break;
	    }
	  break;
	case MUS_NIST:
	  switch (default_output_data_format(ss))
	    {
	    case MUS_LFLOAT: case MUS_LDOUBLE: set_default_output_data_format(MUS_LINT); break;
	    case MUS_BFLOAT: case MUS_BDOUBLE: set_default_output_data_format(MUS_BINT); break;
	    }
	  break;
	case MUS_RIFF:
	  switch (default_output_data_format(ss))
	    {
	    case MUS_BSHORT: set_default_output_data_format(MUS_LSHORT); break;
	    case MUS_BINT: set_default_output_data_format(MUS_LINT); break;
	    case MUS_BFLOAT: set_default_output_data_format(MUS_LFLOAT); break;
	    case MUS_BDOUBLE: set_default_output_data_format(MUS_LDOUBLE); break;
	    }
	  break;
	}
      reflect_output_format(output_data_format_prf);
    }
}

static void output_format_choice(prefs_info *prf)
{
  if (XmToggleButtonGetState(prf->radio_button) == XmSET)
    {
      if (strcmp(XtName(prf->radio_button), "short") == 0)
	set_default_output_data_format(MUS_LSHORT);
      else
	{
	  if (strcmp(XtName(prf->radio_button), "int") == 0)
	    set_default_output_data_format(MUS_LINT);
	  else
	    {
	      if (strcmp(XtName(prf->radio_button), "float") == 0)
		set_default_output_data_format(MUS_LFLOAT);
	      else set_default_output_data_format(MUS_LDOUBLE);
	    }
	}

      switch (default_output_data_format(ss))
	{
	case MUS_LSHORT:
	  switch (default_output_header_type(ss))
	    {
	    case MUS_AIFC: case MUS_AIFF: case MUS_NEXT: 
	      set_default_output_data_format(MUS_BSHORT); 
	      break;
	    }
	  break;
	  
	case MUS_LINT:
	  switch (default_output_header_type(ss))
	    {
	    case MUS_AIFC: case MUS_AIFF: case MUS_NEXT: 
	      set_default_output_data_format(MUS_BINT); 
	      break;
	    }
	  break;
	case MUS_LFLOAT:
	  switch (default_output_header_type(ss))
	    {
	    case MUS_AIFC: case MUS_NEXT: 
	      set_default_output_data_format(MUS_BFLOAT); 
	      break;
	    case MUS_AIFF:
	      set_default_output_header_type(MUS_AIFC);
	      set_default_output_data_format(MUS_BFLOAT); 
	      break;
	    case MUS_NIST: 
	      set_default_output_header_type(MUS_RIFF); 
	      break;
	    }
	  break;
	case MUS_LDOUBLE:
	  switch (default_output_header_type(ss))
	    {
	    case MUS_AIFC: case MUS_NEXT: 
	      set_default_output_data_format(MUS_BDOUBLE); 
	      break;
	    case MUS_AIFF:
	      set_default_output_header_type(MUS_AIFC);
	      set_default_output_data_format(MUS_BDOUBLE); 
	      break;
	    case MUS_NIST: 
	      set_default_output_header_type(MUS_RIFF); 
	      break;
	    }
	  break;
	}
      reflect_output_type(output_header_type_prf);
    }
}



/* ---------------- context sensitive popup ---------------- */

static bool include_context_sensitive_popup = false;

static void save_context_sensitive_popup(prefs_info *prf, FILE *fd)
{
  if (include_context_sensitive_popup)
    {
#if HAVE_SCHEME
      fprintf(fd, "(if (provided? 'snd-motif)\n    (if (not (provided? 'snd-popup.scm))\n        (load-from-path \"popup.scm\"))\n    (if (not (provided? 'snd-gtk-popup.scm))\n	(load-from-path \"gtk-popup.scm\")))\n");
#endif
#if HAVE_RUBY
      fprintf(fd, "require \"popup\"\n");
#endif
    }
}

static void context_sensitive_popup_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  include_context_sensitive_popup = (XmToggleButtonGetState(prf->toggle) == XmSET);
}

static bool find_context_sensitive_popup(void)
{
#if HAVE_SCHEME
  return(XEN_DEFINED_P("edhist-help-edits")); /* defined in both cases */
#endif
#if HAVE_RUBY
  /* XEN_DEFINED_P always returns false in Xen/Ruby? */
  return(strcmp(XEN_AS_STRING(XEN_EVAL_C_STRING("defined? Snd_popup_menu")), "constant") == 0);
  /* it returns "nil" in the undefined case */
#endif
}

static void reflect_context_sensitive_popup(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, find_context_sensitive_popup(), false);
}

/* ---------------- effects menu ---------------- */

static bool include_effects_menu = false;

static void save_effects_menu(prefs_info *prf, FILE *fd)
{
  if (include_effects_menu)
    {
#if HAVE_SCHEME
      fprintf(fd, "(if (provided? 'snd-motif)\n    (if (not (provided? 'snd-new-effects.scm))\n        (load-from-path \"new-effects.scm\"))\n    (if (not (provided? 'snd-gtk-effects.scm))\n	(load-from-path \"gtk-effects.scm\")))\n");
#endif
#if HAVE_RUBY
      fprintf(fd, "require \"effects\"\n");
#endif
    }
}

static void effects_menu_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  include_effects_menu = (XmToggleButtonGetState(prf->toggle) == XmSET);
}

static bool find_effects_menu(void)
{
#if HAVE_SCHEME
  return(XEN_DEFINED_P("effects-menu"));
#endif
#if HAVE_RUBY
  return(strcmp(XEN_AS_STRING(XEN_EVAL_C_STRING("defined? Effects")), "constant") == 0);
#endif
}

static void reflect_effects_menu(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, find_effects_menu(), false);
}

#if HAVE_GUILE
/* ---------------- edit menu ---------------- */

static bool include_edit_menu = false;

static void save_edit_menu(prefs_info *prf, FILE *fd)
{
  if (include_edit_menu)
    fprintf(fd, "(if (not (provided? 'snd-edit-menu.scm)) (load-from-path \"edit-menu.scm\"))\n"); /* ok for either case */
}

static void edit_menu_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  include_edit_menu = (XmToggleButtonGetState(prf->toggle) == XmSET);
}

static bool find_edit_menu(void)
{
  return(XEN_DEFINED_P("make-stereofile")); /* a kludge... currently this is only defined in edit-menu.scm */
}

static void reflect_edit_menu(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, find_edit_menu(), false);
}

/* ---------------- marks menu ---------------- */

static bool include_marks_menu = false;

static void save_marks_menu(prefs_info *prf, FILE *fd)
{
  if (include_marks_menu)
    fprintf(fd, "(if (not (provided? 'snd-marks-menu.scm)) (load-from-path \"marks-menu.scm\"))\n");
}

static void marks_menu_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  include_marks_menu = (XmToggleButtonGetState(prf->toggle) == XmSET);
}

static bool find_marks_menu(void)
{
  return(XEN_DEFINED_P("marks-menu"));
}

static void reflect_marks_menu(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, find_marks_menu(), false);
}

/* ---------------- mix menu ---------------- */

static bool include_mix_menu = false;

static void save_mix_menu(prefs_info *prf, FILE *fd)
{
  if (include_mix_menu)
    fprintf(fd, "(if (not (provided? 'snd-mix-menu.scm)) (load-from-path \"mix-menu.scm\"))\n");
}

static void mix_menu_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  include_mix_menu = (XmToggleButtonGetState(prf->toggle) == XmSET);
}

static bool find_mix_menu(void)
{
  return(XEN_DEFINED_P("mix-menu"));
}

static void reflect_mix_menu(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, find_mix_menu(), false);
}
#endif


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
  else post_prefs_error("no size?", (XtPointer)prf);
}


/* ---------------- initial bounds ---------------- */

static bool use_full_duration(void)
{
#if HAVE_SCHEME
  return((XEN_DEFINED_P("prefs-show-full-duration")) &&
	 (XEN_TRUE_P(XEN_NAME_AS_C_STRING_TO_VALUE("prefs-show-full-duration"))));
#endif
#if HAVE_RUBY
  /* TODO: ruby side of initial bounds */
  return(false);
#endif
}

static char *initial_bounds_to_string(void)
{
#if HAVE_SCHEME
  if (XEN_DEFINED_P("prefs-initial-beg"))
    return(mus_format("%.2f : %.2f", 
		      XEN_TO_C_DOUBLE(XEN_NAME_AS_C_STRING_TO_VALUE("prefs-initial-beg")),
		      XEN_TO_C_DOUBLE(XEN_NAME_AS_C_STRING_TO_VALUE("prefs-initial-dur"))));
#endif
  return(copy_string("0.0 : 0.1"));
}

static void reflect_initial_bounds(prefs_info *prf)
{
  /* text has beg : dur, toggle true if full dur */
  char *str;
  str = initial_bounds_to_string();
  XmTextFieldSetString(prf->text, str);
  FREE(str);
  XmToggleButtonSetState(prf->toggle, use_full_duration(), false);
}

static void save_initial_bounds(prefs_info *prf, FILE *fd)
{
  if ((use_full_duration()) ||
      (!(snd_feq(XEN_TO_C_DOUBLE(XEN_NAME_AS_C_STRING_TO_VALUE("prefs-initial-beg")), 0.0))) ||
      (!(snd_feq(XEN_TO_C_DOUBLE(XEN_NAME_AS_C_STRING_TO_VALUE("prefs-initial-dur")), 0.1))))
    {
#if HAVE_SCHEME
      fprintf(fd, "(if (not (provided? 'snd-extensions.scm)) (load-from-path \"extensions.scm\"))\n");
      fprintf(fd, "(prefs-activate-initial-bounds %.2f %.2f %s)\n",
	      XEN_TO_C_DOUBLE(XEN_NAME_AS_C_STRING_TO_VALUE("prefs-initial-beg")),
	      XEN_TO_C_DOUBLE(XEN_NAME_AS_C_STRING_TO_VALUE("prefs-initial-dur")),
	      (XEN_TRUE_P(XEN_NAME_AS_C_STRING_TO_VALUE("prefs-show-full-duration"))) ? "#t" : "#f");
#endif
    }
}

static void initial_bounds_toggle(prefs_info *prf)
{
  bool use_full_duration = false;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  use_full_duration = (XmToggleButtonGetState(prf->toggle) == XmSET);
#if HAVE_SCHEME
  if (!(XEN_DEFINED_P("prefs-initial-beg")))
    XEN_LOAD_FILE_WITH_PATH("extensions.scm");
  XEN_VARIABLE_SET(XEN_NAME_AS_C_STRING_TO_VARIABLE("prefs-show-full-duration"), C_TO_XEN_BOOLEAN(use_full_duration));
#endif
}

static void initial_bounds_text(prefs_info *prf)
{
  float beg = 0.0, dur = 0.1;
  char *str;
  str = XmTextFieldGetString(prf->text);
  sscanf(str, "%f : %f", &beg, &dur);
  fprintf(stderr, "beg: %f, dur: %f\n", beg, dur);
#if HAVE_SCHEME
  if (!(XEN_DEFINED_P("prefs-initial-beg")))
    XEN_LOAD_FILE_WITH_PATH("extensions.scm");
  XEN_VARIABLE_SET(XEN_NAME_AS_C_STRING_TO_VARIABLE("prefs-initial-beg"), C_TO_XEN_DOUBLE(beg));
  XEN_VARIABLE_SET(XEN_NAME_AS_C_STRING_TO_VARIABLE("prefs-initial-dur"), C_TO_XEN_DOUBLE(dur));
#endif
  XtFree(str);
}


/* ---------------- channel-style ---------------- */

static const char *channel_styles[3] = {"separate", "combined", "superimposed"};

static void reflect_channel_style(prefs_info *prf)
{
  Widget w;
  w = find_radio_button(prf->toggle, channel_styles[(int)channel_style(ss)]);
  if (w)
    XmToggleButtonSetState(w, XmSET, false);
  else fprintf(stderr, "can't find %s\n", channel_styles[(int)channel_style(ss)]);
  if ((prf->radio_button) &&
      (XmIsToggleButton(prf->radio_button)) &&
      (w != prf->radio_button))
    {
      XmToggleButtonSetState(prf->radio_button, XmUNSET, false);
      prf->radio_button = w;
    }
}

static void channel_style_choice(prefs_info *prf)
{
  if (XmToggleButtonGetState(prf->radio_button) == XmSET)
    {
      if (strcmp(XtName(prf->radio_button), "separate") == 0)
	in_set_channel_style(CHANNELS_SEPARATE);
      else
	{
	  if (strcmp(XtName(prf->radio_button), "combined") == 0)
	    in_set_channel_style(CHANNELS_COMBINED);
	  else in_set_channel_style(CHANNELS_SUPERIMPOSED);
	}
    }
}

/* ---------------- graphs-horizontal ---------------- */

static void reflect_graphs_horizontal(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, graphs_horizontal(ss), false);
}

static void graphs_horizontal_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  in_set_graphs_horizontal(XmToggleButtonGetState(prf->toggle) == XmSET);
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
  str = XmTextFieldGetString(prf->text);
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
      else XmTextFieldSetString(prf->text, "right");
      XtFree(str);
    }
}

/* ---------------- show-axes ---------------- */

static const char *show_axes_choices[5] = {"none", "X and Y", "just X", "X and Y unlabelled", "just X unlabelled"};

static void reflect_show_axes(prefs_info *prf)
{
  XmTextFieldSetString(prf->text, (char *)show_axes_choices[(int)show_axes(ss)]);
}

static void show_axes_from_menu(prefs_info *prf, char *value)
{
  int i;
  for (i = 0; i < 5; i++)
    if (strcmp(value, show_axes_choices[i]) == 0)
      {
	in_set_show_axes((show_axes_t)i);
	XmTextFieldSetString(prf->text, value);
	return;
      }
}

static void show_axes_from_text(prefs_info *prf)
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
	  for (i = 0; i < 5; i++)
	    if (STRCMP(trimmed_str, show_axes_choices[i]) == 0)
	      {
		curpos = i;
		break;
	      }
	  if (curpos >= 0)
	    in_set_show_axes((show_axes_t)curpos);
	  else post_prefs_error("what?", (XtPointer)prf);
	}
      else post_prefs_error("right", (XtPointer)prf);
      FREE(trimmed_str);
    }
  else post_prefs_error("right", (XtPointer)prf);
}

/* ---------------- x-axis-style ---------------- */

static const char *x_axis_styles[5] = {"seconds", "samples", "% of total", "beats", "measures"};

static void reflect_x_axis_style(prefs_info *prf)
{
  XmTextFieldSetString(prf->text, (char *)x_axis_styles[(int)x_axis_style(ss)]);
}

static void x_axis_style_from_menu(prefs_info *prf, char *value)
{
  int i;
  for (i = 0; i < 5; i++)
    if (strcmp(value, x_axis_styles[i]) == 0)
      {
	in_set_x_axis_style((x_axis_style_t)i);
	XmTextFieldSetString(prf->text, value);
	return;
      }
}

static void x_axis_style_from_text(prefs_info *prf)
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
	  for (i = 0; i < 5; i++)
	    if (STRCMP(trimmed_str, x_axis_styles[i]) == 0)
	      {
		curpos = i;
		break;
	      }
	  if (curpos >= 0)
	    in_set_x_axis_style((x_axis_style_t)curpos);
	  else post_prefs_error("what?", (XtPointer)prf);
	}
      else post_prefs_error("right", (XtPointer)prf);
      FREE(trimmed_str);
    }
  else post_prefs_error("right", (XtPointer)prf);
}

/* ---------------- smpte ---------------- */

static bool include_smpte = false;

static bool find_smpte(void)
{
#if HAVE_SCHEME
  return((XEN_DEFINED_P("smpte-is-on")) &&
	 (!(XEN_FALSE_P(XEN_EVAL_C_STRING("(smpte-is-on)"))))); /* "member" of hook-list -> a list if successful */
#endif
#if HAVE_RUBY
  /* TODO: ruby side of smpte */
#endif
  return(false);
}

static void reflect_smpte(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, find_smpte(), false);
}

static void smpte_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  include_smpte = (XmToggleButtonGetState(prf->toggle) == XmSET);
}

static void save_smpte(prefs_info *prf, FILE *fd)
{
  if (include_smpte)
    {
#if HAVE_SCHEME
      fprintf(fd, "(if (provided? 'snd-motif)\n    (if (not (provided? 'snd-snd-motif.scm))\n        (load-from-path \"snd-motif.scm\"))\n    (if (not (provided? 'snd-snd-gtk.scm))\n        (load-from-path \"snd-gtk.scm\")))\n");
      fprintf(fd, "(show-smpte-label #t)\n");
#endif
#if HAVE_RUBY
#endif
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

/* ---------------- selection-color ---------------- */

static void set_selection_color(color_t color)
{
  color_selection(color);
  for_each_chan(update_graph);
}

static Pixel saved_selection_color;

static void reflect_selection_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_selection_color); 
  set_selection_color(saved_selection_color);
}

static void selection_color_func(prefs_info *prf, float r, float g, float b)
{
  XColor *tmp;
  tmp = rgb_to_color(r, g, b);
  set_selection_color(tmp->pixel);
  FREE(tmp);
}


/* ---------------- axis-label-font ---------------- */

static void axis_label_font_error_erase_func(XtPointer context, XtIntervalId *id)
{
  prefs_info *prf = (prefs_info *)context;
  XmTextFieldSetString(prf->text, axis_label_font(ss));
}

static void reflect_axis_label_font(prefs_info *prf)
{
  XmTextFieldSetString(prf->text, axis_label_font(ss));
}

static void axis_label_font_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((!str) || (!(*str)))
    {
      XmTextFieldSetString(prf->text, axis_label_font(ss));
      return;
    }
  if (!(set_axis_label_font(str)))
    {
      XmTextFieldSetString(prf->text, "can't find that font");
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
  XmTextFieldSetString(prf->text, axis_numbers_font(ss));
}

static void reflect_axis_numbers_font(prefs_info *prf)
{
  XmTextFieldSetString(prf->text, axis_numbers_font(ss));
}

static void axis_numbers_font_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((!str) || (!(*str)))
    {
      XmTextFieldSetString(prf->text, axis_numbers_font(ss));
      return;
    }
  if (!(set_axis_numbers_font(str)))
    {
      XmTextFieldSetString(prf->text, "can't find that font");
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
  XmTextFieldSetString(prf->text, peaks_font(ss));
}

static void reflect_peaks_font(prefs_info *prf)
{
  XmTextFieldSetString(prf->text, peaks_font(ss));
}

static void peaks_font_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((!str) || (!(*str)))
    {
      XmTextFieldSetString(prf->text, peaks_font(ss));
      return;
    }
  if (!(set_peaks_font(str)))
    {
      XmTextFieldSetString(prf->text, "can't find that font");
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
  XmTextFieldSetString(prf->text, bold_peaks_font(ss));
}

static void reflect_bold_peaks_font(prefs_info *prf)
{
  XmTextFieldSetString(prf->text, bold_peaks_font(ss));
}

static void bold_peaks_font_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((!str) || (!(*str)))
    {
      XmTextFieldSetString(prf->text, bold_peaks_font(ss));
      return;
    }
  if (!(set_bold_peaks_font(str)))
    {
      XmTextFieldSetString(prf->text, "can't find that font");
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
  XmTextFieldSetString(prf->text, tiny_font(ss));
}

static void reflect_tiny_font(prefs_info *prf)
{
  XmTextFieldSetString(prf->text, tiny_font(ss));
}

static void tiny_font_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((!str) || (!(*str)))
    {
      XmTextFieldSetString(prf->text, tiny_font(ss));
      return;
    }
  if (!(set_tiny_font(str)))
    {
      XmTextFieldSetString(prf->text, "can't find that font");
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
  XmTextFieldSetString(prf->text, new_size);
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
	  else post_prefs_error("size must be a power of 2", (XtPointer)prf);
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
  XmTextFieldSetString(prf->text, (char *)transform_types[mus_iclamp(0, transform_type(ss), NUM_TRANSFORM_TYPES - 1)]); 
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
	return;
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
	  else post_prefs_error("unknown tranform", (XtPointer)prf);
	}
      else post_prefs_error("no transform?", (XtPointer)prf);
      FREE(trimmed_str);
    }
  else post_prefs_error("no transform?", (XtPointer)prf);
}


/* -------- fft-window -------- */

static const char *fft_windows[NUM_FFT_WINDOWS] = 
  {"Rectangular", "Hann", "Welch", "Parzen", "Bartlett", "Hamming", "Blackman2", "Blackman3", "Blackman4",
   "Exponential", "Riemann", "Kaiser", "Cauchy", "Poisson", "Gaussian", "Tukey", "Dolph-Chebyshev", "Hann-Poisson", "Connes"};

static list_completer_info *fft_window_completer_info = NULL;

static void reflect_fft_window(prefs_info *prf)
{
  XmTextFieldSetString(prf->text, (char *)fft_windows[(int)fft_window(ss)]);
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
	in_set_fft_window((mus_fft_window_t)i);
	XmTextFieldSetString(prf->text, value);
	return;
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
	  else post_prefs_error("unknown window", (XtPointer)prf);
	}
      else post_prefs_error("no window?", (XtPointer)prf);
      FREE(trimmed_str);
    }
  else post_prefs_error("no window?", (XtPointer)prf);
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
  str = XmTextFieldGetString(prf->text);
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
      else XmTextFieldSetString(prf->text, "right");
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

static void max_peaks_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((str) && (*str))
    {
      int value = 0;
      sscanf(str, "%d", &value);
      if (value >= 0)
	in_set_max_transform_peaks(value);
      else int_to_textfield(prf->text, max_transform_peaks(ss));
      XtFree(str);
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
	  else post_prefs_error("unknown colormap", (XtPointer)prf);
	}
      else post_prefs_error("no colormap?", (XtPointer)prf);
      FREE(trimmed_str);
    }
  else post_prefs_error("no colormap?", (XtPointer)prf);
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
	return;
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
  float_1_to_textfield(prf->text, min_dB(ss));
}

static void min_dB_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((str) && (*str))
    {
      float value = 0.0;
      sscanf(str, "%f", &value);
      set_min_db(value); /* snd-chn.c -- redisplays */
      XtFree(str);
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
  XmTextFieldSetString(prf->text, "right");
  XtAppAddTimeOut(MAIN_APP(ss),
		  ERROR_WAIT_TIME,
		  mark_tag_width_erase_func,
		  (XtPointer)prf);
}

static void mark_tag_height_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  XmTextFieldSetString(prf->rtxt, "right");
  XtAppAddTimeOut(MAIN_APP(ss),
		  ERROR_WAIT_TIME,
		  mark_tag_height_erase_func,
		  (XtPointer)prf);
}

static void mark_tag_size_text(prefs_info *prf)
{
  char *str;
  str = XmTextFieldGetString(prf->text);
  if ((str) && (*str))
    {
      int width = 0;
      redirect_errors_to(mark_tag_width_error, (void *)prf);
      width = string_to_int(str, 1, "mark tag width");
      redirect_errors_to(NULL, NULL);
      if (width > 0) set_mark_tag_width(width);
      XtFree(str);
      str = XmTextFieldGetString(prf->rtxt);
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
  XmTextFieldSetString(prf->text, "right");
  XtAppAddTimeOut(MAIN_APP(ss),
		  ERROR_WAIT_TIME,
		  mix_tag_width_erase_func,
		  (XtPointer)prf);
}

static void mix_tag_height_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  XmTextFieldSetString(prf->rtxt, "right");
  XtAppAddTimeOut(MAIN_APP(ss),
		  ERROR_WAIT_TIME,
		  mix_tag_height_erase_func,
		  (XtPointer)prf);
}

static void mix_tag_size_text(prefs_info *prf)
{
  char *str;
  str = XmTextFieldGetString(prf->text);
  if ((str) && (*str))
    {
      int width = 0;
      redirect_errors_to(mix_tag_width_error, (void *)prf);
      width = string_to_int(str, 1, "mix tag width");
      redirect_errors_to(NULL, NULL);
      if (width > 0) set_mix_tag_width(width);
      XtFree(str);
      str = XmTextFieldGetString(prf->rtxt);
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
  str = XmTextFieldGetString(prf->text);
  if ((str) && (*str))
    {
      int value = 0;
      sscanf(str, "%d", &value);
      if (value >= 0)
	in_set_mix_waveform_height(value);
      else int_to_textfield(prf->text, mix_waveform_height(ss));
      XtFree(str);
    }
}


/* ---------------- include with-sound ---------------- */

static bool include_with_sound = false;
static char *include_clm_file_name = NULL;

static bool with_sound_is_loaded(void)
{
#if HAVE_SCHEME
  return(XEN_DEFINED_P("with-sound"));
#endif
#if HAVE_RUBY
  /* TODO: ruby side of with_sound_is_loaded (etc) */
#endif
  return(false);
}

static void reflect_with_sound(prefs_info *prf) 
{
  include_with_sound = with_sound_is_loaded();
  XmToggleButtonSetState(prf->toggle, include_with_sound, false);
}

static void with_sound_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  include_with_sound = (XmToggleButtonGetState(prf->toggle) == XmSET);
}

static void save_with_sound(prefs_info *prf, FILE *fd)
{
  if (include_with_sound)
    {
#if HAVE_SCHEME
      fprintf(fd, "(if (not (provided? 'snd-ws.scm)) (load-from-path \"ws.scm\"))\n");
      if (include_clm_file_name)
	fprintf(fd, "(set! *clm-file-name* %s)\n", include_clm_file_name);
#endif
#if HAVE_RUBY
#endif
    }
}

/* ---------------- speed control ---------------- */

#define NUM_SPEED_CONTROL_CHOICES 3
#define MIN_SPEED_CONTROL_SEMITONES 1

static const char *speed_control_styles[NUM_SPEED_CONTROL_CHOICES] = {"float", "ratio", "semitones:"};

static void show_speed_control_semitones(prefs_info *prf)
{
  int_to_textfield(prf->text, speed_control_tones(ss));
  XtSetSensitive(prf->arrow_down, (speed_control_tones(ss) > MIN_SPEED_CONTROL_SEMITONES));
}

static void speed_control_up(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  in_set_speed_control_tones(ss, speed_control_tones(ss) + 1);
  show_speed_control_semitones(prf);
}

static void speed_control_down(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  in_set_speed_control_tones(ss, speed_control_tones(ss) - 1);
  show_speed_control_semitones(prf);
}

static void speed_control_text(prefs_info *prf)
{
  int tones;
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((str) && (*str))
    {
      prf->got_error = false;
      redirect_errors_to(post_prefs_error, (void *)prf);
      tones = string_to_int(str, MIN_SPEED_CONTROL_SEMITONES, "semitones");
      redirect_errors_to(NULL, NULL);
      XtFree(str);
      if (!(prf->got_error))
	{
	  in_set_speed_control_tones(ss, tones);
	  XtSetSensitive(prf->arrow_down, (speed_control_tones(ss) > MIN_SPEED_CONTROL_SEMITONES));
	}
      else prf->got_error = false;
    }
}

static void reflect_speed_control(prefs_info *prf)
{
  Widget w;
  show_speed_control_semitones(prf);
  w = find_radio_button(prf->toggle, speed_control_styles[(int)speed_control_style(ss)]);
  if (w)
    XmToggleButtonSetState(w, XmSET, false);
  else fprintf(stderr, "can't find %s\n", speed_control_styles[(int)speed_control_style(ss)]);
  if ((prf->radio_button) &&
      (XmIsToggleButton(prf->radio_button)) &&
      (w != prf->radio_button))
    {
      XmToggleButtonSetState(prf->radio_button, XmUNSET, false);
      prf->radio_button = w;
    }
}

static void speed_control_choice(prefs_info *prf)
{
  if (XmToggleButtonGetState(prf->radio_button) == XmSET)
    {
      if (strcmp(XtName(prf->radio_button), "float") == 0)
	in_set_speed_control_style(ss, SPEED_CONTROL_AS_FLOAT);
      else
	{
	  if (strcmp(XtName(prf->radio_button), "ratio") == 0)
	    in_set_speed_control_style(ss, SPEED_CONTROL_AS_RATIO);
	  else in_set_speed_control_style(ss, SPEED_CONTROL_AS_SEMITONE);
	}
    }
}

/* ---------------- sinc width ---------------- */

static void sinc_width_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((str) && (*str))
    {
      int value = 0;
      sscanf(str, "%d", &value);
      if (value >= 0)
	set_sinc_width(value);
      else int_to_textfield(prf->text, sinc_width(ss));
      XtFree(str);
    }
}

static void reflect_sinc_width(prefs_info *prf)
{
  int_to_textfield(prf->text, sinc_width(ss));
}


/* ---------------- clm file name ---------------- */

static char *clm_file_name(void)
{
#if HAVE_SCHEME
  if (XEN_DEFINED_P("*clm-file-name*"))
    return(XEN_TO_C_STRING(XEN_NAME_AS_C_STRING_TO_VALUE("*clm-file-name*")));
#endif
#if HAVE_RUBY
  /* TODO: ruby side of clm_file_name etc */
#endif
  return(NULL);
}

static void set_clm_file_name(const char *str)
{
#if HAVE_SCHEME
  if (XEN_DEFINED_P("*clm-file-name*"))
    XEN_VARIABLE_SET(XEN_NAME_AS_C_STRING_TO_VARIABLE("*clm-file-name*"), C_TO_XEN_STRING(str));
#endif
#if HAVE_RUBY
#endif
}

static void clm_file_name_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((str) && (*str))
    {
      include_with_sound = true;
      if (include_clm_file_name) FREE(include_clm_file_name); /* save is done after we're sure with-sound is loaded */
      include_clm_file_name = copy_string(str);
      set_clm_file_name(str);
      XtFree(str);
    }
}

static void reflect_clm_file_name(prefs_info *prf)
{
  XmTextFieldSetString(prf->text, clm_file_name());
}


/* ---------------- show-listener ---------------- */

static bool include_listener = false;

static void reflect_show_listener(prefs_info *prf) 
{
  include_listener = listener_is_visible();
  XmToggleButtonSetState(prf->toggle, include_listener, false);
}

static void show_listener_toggle(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(XmIsToggleButton(prf->toggle), prf->toggle);
  include_listener = (XmToggleButtonGetState(prf->toggle) == XmSET);
}

static void save_show_listener(prefs_info *prf, FILE *fd)
{
  if (include_listener)
    {
#if HAVE_SCHEME
      /* show-listener is saved in save-state, but not save-options */
      fprintf(fd, "(show-listener)\n");
#endif
#if HAVE_RUBY
      fprintf(fd, "show_listener\n");
#endif
    }
}


#if HAVE_GUILE
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
#endif


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

#if HAVE_GUILE
/* ---------------- debugging aids ---------------- */

static bool include_debugging_aids = false;

static bool find_debugging_aids(void)
{
  return((XEN_DEFINED_P("snd-break")) && 
	 (XEN_DEFINED_P("untrace-stack")));
}

static void reflect_debugging_aids(prefs_info *prf) 
{
  XmToggleButtonSetState(prf->toggle, find_debugging_aids(), false);
}

static void debugging_aids_toggle(prefs_info *prf)
{
  include_debugging_aids = (XmToggleButtonGetState(prf->toggle) == XmSET);
}

static void save_debugging_aids(prefs_info *prf, FILE *fd)
{
  if (include_debugging_aids)
    {
      fprintf(fd, "(use-modules (ice-9 debug) (ice-9 session))\n");
#if HAVE_SCM_OBJECT_TO_STRING
      /* 1.6 or later -- not sure 1.4 etc can handle these things */
      fprintf(fd, "(debug-set! stack 0)\n");
      fprintf(fd, "(debug-enable 'debug 'backtrace)\n");
      fprintf(fd, "(read-enable 'positions)\n");
#endif
      fprintf(fd, "(if (not (provided? 'snd-debug.scm)) (load-from-path \"debug.scm\"))\n");
    }
}
#endif

/* ---------------- print-length ---------------- */

static void reflect_print_length(prefs_info *prf)
{
  int_to_textfield(prf->text, print_length(ss));
}

static void print_length_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((str) && (*str))
    {
      int value = 0;
      sscanf(str, "%d", &value);
      set_print_length(value); /* TODO: shouldn't this be reflected in ws.scm *clm-array-print-length*? */
      XtFree(str);
    }
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
  XmTextFieldSetString(prf->text, listener_font(ss));
}

static void reflect_listener_font(prefs_info *prf)
{
  XmTextFieldSetString(prf->text, listener_font(ss));
}

static void listener_font_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((!str) || (!(*str)))
    {
      XmTextFieldSetString(prf->text, listener_font(ss));
      return;
    }
  if (!(set_listener_font(str)))
    {
      XmTextFieldSetString(prf->text, "can't find that font");
      XtAppAddTimeOut(MAIN_APP(ss),
		      ERROR_WAIT_TIME,
		      listener_font_error_erase_func,
		      (XtPointer)prf);
    }
  if (str) XtFree(str);
}


/* ---------------- dac-size ---------------- */

static void reflect_dac_size(prefs_info *prf) 
{
  int_to_textfield(prf->text, dac_size(ss));
}

static void dac_size_text(prefs_info *prf)
{
  char *str;
  ASSERT_WIDGET_TYPE(XmIsTextField(prf->text), prf->text);
  str = XmTextFieldGetString(prf->text);
  if ((str) && (*str))
    {
      int value = 0;
      sscanf(str, "%d", &value);
      if (value > 0)
	set_dac_size(value);
      else int_to_textfield(prf->text, dac_size(ss));
      XtFree(str);
    }
}


/* ---------------- help-button-color ---------------- */

static Pixel saved_help_button_color;

static void reflect_help_button_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_help_button_color); 
  ss->sgx->help_button_color = saved_help_button_color;
}

static void help_button_color_func(prefs_info *prf, float r, float g, float b)
{
  XColor *tmp;
  tmp = rgb_to_color(r, g, b);
  ss->sgx->help_button_color = tmp->pixel;
  FREE(tmp);
}

/* ---------------- quit-button-color ---------------- */

static Pixel saved_quit_button_color;

static void reflect_quit_button_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_quit_button_color); 
  ss->sgx->quit_button_color = saved_quit_button_color;
}

static void quit_button_color_func(prefs_info *prf, float r, float g, float b)
{
  XColor *tmp;
  tmp = rgb_to_color(r, g, b);
  ss->sgx->quit_button_color = tmp->pixel;
  FREE(tmp);
}

/* ---------------- reset-button-color ---------------- */

static Pixel saved_reset_button_color;

static void reflect_reset_button_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_reset_button_color); 
  ss->sgx->reset_button_color = saved_reset_button_color;
}

static void reset_button_color_func(prefs_info *prf, float r, float g, float b)
{
  XColor *tmp;
  tmp = rgb_to_color(r, g, b);
  ss->sgx->reset_button_color = tmp->pixel;
  FREE(tmp);
}

/* ---------------- doit-button-color ---------------- */

static Pixel saved_doit_button_color;

static void reflect_doit_button_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_doit_button_color); 
  ss->sgx->doit_button_color = saved_doit_button_color;
}

static void doit_button_color_func(prefs_info *prf, float r, float g, float b)
{
  XColor *tmp;
  tmp = rgb_to_color(r, g, b);
  ss->sgx->doit_button_color = tmp->pixel;
  FREE(tmp);
}

/* ---------------- doit-again-button-color ---------------- */

static Pixel saved_doit_again_button_color;

static void reflect_doit_again_button_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_doit_again_button_color); 
  ss->sgx->doit_again_button_color = saved_doit_again_button_color;
}

static void doit_again_button_color_func(prefs_info *prf, float r, float g, float b)
{
  XColor *tmp;
  tmp = rgb_to_color(r, g, b);
  ss->sgx->doit_again_button_color = tmp->pixel;
  FREE(tmp);
}

/* ---------------- pushed-button-color ---------------- */

static Pixel saved_pushed_button_color;

static void reflect_pushed_button_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_pushed_button_color); 
  ss->sgx->pushed_button_color = saved_pushed_button_color;
}

static void pushed_button_color_func(prefs_info *prf, float r, float g, float b)
{
  XColor *tmp;
  tmp = rgb_to_color(r, g, b);
  ss->sgx->pushed_button_color = tmp->pixel;
  FREE(tmp);
}




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

  {
      XColor *tmp;
      tmp = rgb_to_color(1.0, 0.0, 0.0);
      red = tmp->pixel;
      FREE(tmp);
      tmp = rgb_to_color(0.0, 1.0, 0.0);
      green = tmp->pixel;
      FREE(tmp);
      tmp = rgb_to_color(0.0, 0.0, 1.0);
      blue = tmp->pixel;
      FREE(tmp);
  }

  /* ---------------- overall behavior ---------------- */

  {
    Widget dpy_box, dpy_label, file_label, cursor_label;
    char *str1, *str2;

    /* ---------------- overall behavior ----------------*/

    dpy_box = make_top_level_box(topics);
    dpy_label = make_top_level_label("overall behavior choices", dpy_box);

#if 0
    /* TODO: packages of presets */
    prf = prefs_row_with_radio_box("preset customization packages", "customization",
				   customization_choices, 4, "none",
				   dpy_box, dpy_label,
				   customization_choice);
    remember_pref(prf, reflect_customization_choice, NULL);
    current_sep = make_inter_variable_separator(dpy_box, prf->label);
#else
    current_sep = dpy_label;
#endif

    str1 = mus_format("%d", ss->init_window_width);
    str2 = mus_format("%d", ss->init_window_height);
    prf = prefs_row_with_two_texts("start up size", S_window_width, 
				   "width:", str1, "height:", str2, 6,
				   dpy_box, current_sep,
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
    remember_pref(prf, reflect_current_window_display, save_current_window_display);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("resize main window as sounds open and close", S_auto_resize,
				auto_resize(ss), 
				dpy_box, current_sep, 
				resize_toggle);
    remember_pref(prf, reflect_auto_resize, NULL);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    focus_follows_mouse = focus_is_following_mouse();
    prf = prefs_row_with_toggle("focus follows mouse", "focus-follows-mouse",
				focus_follows_mouse,
				dpy_box, current_sep,
				focus_follows_mouse_toggle);
    remember_pref(prf, reflect_focus_follows_mouse, save_focus_follows_mouse);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("show the control panel upon opening a sound", S_show_controls,
				in_show_controls(ss), 
				dpy_box, current_sep, 
				controls_toggle);
    remember_pref(prf, reflect_show_controls, NULL);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    include_peak_env_directory = copy_string(peak_env_directory());
    include_peak_envs = find_peak_envs();
    prf = prefs_row_with_toggle_with_text("save peak envs to speed up initial display", "save-peak-env-info",
					  include_peak_envs,
					  "directory:", include_peak_env_directory, 25,
					  dpy_box, current_sep,
					  peak_envs_toggle, peak_envs_text);
    remember_pref(prf, reflect_peak_envs, save_peak_envs);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    str = mus_format("%d", max_regions(ss));
    prf = prefs_row_with_toggle_with_text("selection creates an associated region", S_selection_creates_region,
					  selection_creates_region(ss),
					  "max regions:", str, 5,
					  dpy_box, current_sep,
					  selection_creates_region_toggle, max_regions_text);
    remember_pref(prf, reflect_selection_creates_region, NULL);
    FREE(str);

    /* ---------------- file options ---------------- */

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    file_label = make_inner_label("  file options", dpy_box, current_sep);

    prf = prefs_row_with_toggle("display only sound files in various file lists", S_just_sounds,
				just_sounds(ss), 
				dpy_box, file_label, 
				just_sounds_toggle);
    remember_pref(prf, prefs_reflect_just_sounds, NULL);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_text("directory for temporary files", S_temp_dir, 
			      temp_dir(ss), 
			      dpy_box, current_sep,
			      temp_dir_text);
    remember_pref(prf, reflect_temp_dir, NULL);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_text("directory for save-state files", S_save_dir, 
			      save_dir(ss), 
			      dpy_box, current_sep,
			      save_dir_text);
    remember_pref(prf, reflect_save_dir, NULL);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_text("default save-state filename", S_save_state_file, 
			      save_state_file(ss), 
			      dpy_box, current_sep,
			      save_state_file_text);
    remember_pref(prf, reflect_save_state_file, NULL);

#if HAVE_LADSPA
    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_text("directory for ladspa plugins", S_ladspa_dir, 
			      ladspa_dir(ss), 
			      dpy_box, current_sep,
			      ladspa_dir_text);
    remember_pref(prf, reflect_ladspa_dir, NULL);
#endif

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    include_vf_directory = copy_string(view_files_find_any_directory());
    prf = prefs_row_with_text("directory for view-files dialog", S_add_directory_to_view_files_list,
			      include_vf_directory,
			      dpy_box, current_sep,
			      view_files_directory_text);
    remember_pref(prf, reflect_view_files_directory, save_view_files_directory);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_text("external program to read HTML files via snd-help", S_html_program,
			      html_program(ss),
			      dpy_box, current_sep,
			      html_program_text);
    remember_pref(prf, reflect_html_program, NULL);
    current_sep = make_inter_variable_separator(dpy_box, prf->label);

    prf = prefs_row_with_radio_box("default new sound attributes: chans", S_default_output_chans,
				   output_chan_choices, 4, -1,
				   dpy_box, current_sep,
				   output_chans_choice);
    reflect_output_chans(prf);
    remember_pref(prf, reflect_output_chans, NULL);

    prf = prefs_row_with_radio_box("srate", S_default_output_srate,
				   output_srate_choices, 4, -1,
				   dpy_box, prf->label,
				   output_srate_choice);
    reflect_output_srate(prf);
    remember_pref(prf, reflect_output_srate, NULL);

    prf = prefs_row_with_radio_box("header type", S_default_output_header_type,
				   output_type_choices, 5, -1,
				   dpy_box, prf->label,
				   output_type_choice);
    output_header_type_prf = prf;
    remember_pref(prf, reflect_output_type, NULL);

    prf = prefs_row_with_radio_box("data format", S_default_output_data_format,
				   output_format_choices, 4, -1,
				   dpy_box, prf->label,
				   output_format_choice);
    output_data_format_prf = prf;
    remember_pref(prf, reflect_output_format, NULL);
    current_sep = make_inter_variable_separator(dpy_box, prf->label);

    reflect_output_type(output_header_type_prf);
    reflect_output_format(output_data_format_prf);


  /* ---------------- extra menus ---------------- */

#if HAVE_STATIC_XM
    cursor_label = make_inner_label("  extra menus", dpy_box, current_sep);
#else
    cursor_label = make_inner_label("  extra menus (these will need the xm module)", dpy_box, current_sep);
#endif

    prf = prefs_row_with_toggle("context-sensitive popup menu", "add-selection-popup",
				find_context_sensitive_popup(),
				dpy_box, cursor_label, 
				context_sensitive_popup_toggle);
    remember_pref(prf, reflect_context_sensitive_popup, save_context_sensitive_popup);
    current_sep = make_inter_variable_separator(dpy_box, prf->label);

    prf = prefs_row_with_toggle("effects menu", "new-effects.scm", /* TODO: help index for effects? */
				find_effects_menu(),
				dpy_box, current_sep, 
				effects_menu_toggle);
    remember_pref(prf, reflect_effects_menu, save_effects_menu);

#if HAVE_GUILE
    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("edit menu additions", "edit-menu.scm", /* TODO help index */
				find_edit_menu(),
				dpy_box, current_sep, 
				edit_menu_toggle);
    remember_pref(prf, reflect_edit_menu, save_edit_menu);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("marks menu", "marks-menu.scm",
				find_marks_menu(),
				dpy_box, current_sep, 
				marks_menu_toggle);
    remember_pref(prf, reflect_marks_menu, save_marks_menu);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    prf = prefs_row_with_toggle("mix/track menu", "mix-menu.scm",
				find_mix_menu(),
				dpy_box, current_sep, 
				mix_menu_toggle);
    remember_pref(prf, reflect_mix_menu, save_mix_menu);
#endif
    current_sep = make_inter_variable_separator(dpy_box, prf->label);

    /* ---------------- cursor options ---------------- */

    cursor_label = make_inner_label("  cursor options", dpy_box, current_sep);

    prf = prefs_row_with_toggle("report cursor location as it moves", S_verbose_cursor,
				verbose_cursor(ss), 
				dpy_box, cursor_label, 
				verbose_cursor_toggle);
    remember_pref(prf, reflect_verbose_cursor, NULL);

    current_sep = make_inter_variable_separator(dpy_box, prf->label);
    {
      char *str1;
      str = mus_format("%.2f", cursor_update_interval(ss));
      str1 = mus_format("%d", cursor_location_offset(ss));
      prf = prefs_row_with_toggle_with_two_texts("track current location while playing", S_cursor_follows_play,
						 cursor_follows_play(ss), 
						 "update:", str,
						 "offset:", str1, 8, 
						 dpy_box, current_sep,
						 cursor_follows_play_toggle,
						 cursor_location_text);
      remember_pref(prf, reflect_cursor_follows_play, NULL);
      FREE(str);
      FREE(str1);
    }

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

    /* ---------------- (overall) colors ---------------- */

    current_sep = make_inter_variable_separator(dpy_box, prf->rscl);
    cursor_label = make_inner_label("  colors", dpy_box, current_sep);
    
    saved_basic_color = ss->sgx->basic_color;
    prf = prefs_color_selector_row("main background color", S_basic_color, ss->sgx->basic_color,
				   dpy_box, cursor_label,
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
  }

  current_sep = make_inter_topic_separator(topics);

  /* -------- graphs -------- */
  {
    Widget grf_box, grf_label, colgrf_label;

    /* ---------------- graph options ---------------- */

    grf_box = make_top_level_box(topics);
    grf_label = make_top_level_label("graph options", grf_box);

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
    str = initial_bounds_to_string();
    prf = prefs_row_with_text_with_toggle("initial graph x bounds", S_initial_graph_hook, use_full_duration(),
					  "show full duration", str, 16,
					  grf_box, current_sep,
					  initial_bounds_toggle,
					  initial_bounds_text);
    FREE(str);
    remember_pref(prf, reflect_initial_bounds, save_initial_bounds);

    current_sep = make_inter_variable_separator(grf_box, prf->label);
    prf = prefs_row_with_radio_box("how to layout multichannel graphs", S_channel_style,
				   channel_styles, 3, channel_style(ss),
				   grf_box, current_sep,
				   channel_style_choice);
    remember_pref(prf, reflect_channel_style, NULL);

    current_sep = make_inter_variable_separator(grf_box, prf->label);
    prf = prefs_row_with_toggle("layout wave and fft graphs horizontally", S_graphs_horizontal,
				graphs_horizontal(ss),
				grf_box, current_sep,
				graphs_horizontal_toggle);
    remember_pref(prf, reflect_graphs_horizontal, NULL);

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
    prf = prefs_row_with_completed_list("what axes to display", S_show_axes, show_axes_choices[(int)show_axes(ss)],
					show_axes_choices, 5,
					grf_box, current_sep,
					show_axes_from_text,
					NULL, NULL,
					show_axes_from_menu);
    remember_pref(prf, reflect_show_axes, NULL);

    current_sep = make_inter_variable_separator(grf_box, prf->label);
    prf = prefs_row_with_completed_list("time division", S_x_axis_style, x_axis_styles[(int)x_axis_style(ss)],
					x_axis_styles, 5,
					grf_box, current_sep,
					x_axis_style_from_text,
					NULL, NULL,
					x_axis_style_from_menu);
    remember_pref(prf, reflect_x_axis_style, NULL);

    current_sep = make_inter_variable_separator(grf_box, prf->label);
    prf = prefs_row_with_toggle("include smpte info", "show-smpte-label", /* TODO: does this trigger help? */
				find_smpte(),
				grf_box, current_sep,
				smpte_toggle);
    remember_pref(prf, reflect_smpte, save_smpte);

    /* ---------------- (graph) colors ---------------- */

    current_sep = make_inter_variable_separator(grf_box, prf->label); 
    colgrf_label = make_inner_label("  colors", grf_box, current_sep);

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
    prf = prefs_color_selector_row("selected channel data (waveform) color", S_selected_data_color, ss->sgx->selected_data_color,
				   grf_box, current_sep,
				   selected_data_color_func);
    remember_pref(prf, reflect_selected_data_color, NULL);

    current_sep = make_inter_variable_separator(grf_box, prf->rscl);
    saved_selected_graph_color = ss->sgx->selected_graph_color;
    prf = prefs_color_selector_row("selected channel graph (background) color", S_selected_graph_color, ss->sgx->selected_graph_color,
				   grf_box, current_sep,
				   selected_graph_color_func);
    remember_pref(prf, reflect_selected_graph_color, NULL);

    current_sep = make_inter_variable_separator(grf_box, prf->rscl);
    saved_selection_color = ss->sgx->selection_color;
    prf = prefs_color_selector_row("selection color", S_selection_color, ss->sgx->selection_color,
				   grf_box, current_sep,
				   selection_color_func);
    remember_pref(prf, reflect_selection_color, NULL);

    /* ---------------- (graph) fonts ---------------- */

    current_sep = make_inter_variable_separator(grf_box, prf->rscl);
    colgrf_label = make_inner_label("  fonts", grf_box, current_sep);

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
  }

  current_sep = make_inter_topic_separator(topics);

#if 0    
  /* -------- audio -------- */
  {
    Widget aud_box, aud_label;

    aud_box = make_top_level_box(topics);
    aud_label = make_top_level_label("audio options", aud_box);
    
  }

  current_sep = make_inter_topic_separator(topics);
#endif

  /* -------- transform -------- */
  {
    Widget fft_box, fft_label;

    /* ---------------- transform options ---------------- */

    fft_box = make_top_level_box(topics);
    fft_label = make_top_level_label("transform options", fft_box);

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
					  transform_peaks_toggle, max_peaks_text);
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

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    prf = prefs_row_with_toggle("y axis as log magnitude (dB)", S_fft_log_magnitude,
				fft_log_magnitude(ss),
				fft_box, current_sep,
				log_magnitude_toggle);
    remember_pref(prf, reflect_fft_log_magnitude, NULL);

    current_sep = make_inter_variable_separator(fft_box, prf->label);
    str = mus_format("%.1f", min_dB(ss));
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
  }
  
  current_sep = make_inter_topic_separator(topics);

  /* -------- clm -------- */
  {
    Widget clm_box, clm_label;

    /* ---------------- clm options ---------------- */

    clm_box = make_top_level_box(topics);
    clm_label = make_top_level_label("clm", clm_box);

    include_with_sound = with_sound_is_loaded();
    prf = prefs_row_with_toggle("include with-sound", "with-sound",
				include_with_sound,
				clm_box, clm_label,
				with_sound_toggle);
    remember_pref(prf, reflect_with_sound, save_with_sound);

    current_sep = make_inter_variable_separator(clm_box, prf->label);
    str = mus_format("%d", speed_control_tones(ss));
    prf = prefs_row_with_radio_box_and_number("speed control choice", S_speed_control_style,
					      speed_control_styles, 3, (int)speed_control_style(ss),
					      speed_control_tones(ss), str, 6,
					      clm_box, current_sep,
					      speed_control_choice, speed_control_up, speed_control_down, speed_control_text);
    XtSetSensitive(prf->arrow_down, (speed_control_tones(ss) > MIN_SPEED_CONTROL_SEMITONES));
    remember_pref(prf, reflect_speed_control, NULL);
    FREE(str);

    current_sep = make_inter_variable_separator(clm_box, prf->label);
    str = mus_format("%d", sinc_width(ss));
    prf = prefs_row_with_text("sinc interpolation width in srate converter", S_sinc_width, str,
			      clm_box, current_sep,
			      sinc_width_text);
    remember_pref(prf, reflect_sinc_width, NULL);
    FREE(str);

    current_sep = make_inter_variable_separator(clm_box, prf->label);
    prf = prefs_row_with_text("with-sound default output file name", "*clm-file-name*", clm_file_name(),
			      clm_box, current_sep,
			      clm_file_name_text);
    remember_pref(prf, reflect_clm_file_name, NULL);

  }

  current_sep = make_inter_topic_separator(topics);

  /* -------- programming -------- */
  {
    Widget prg_box, prg_label;

    /* ---------------- listener options ---------------- */

    prg_box = make_top_level_box(topics);
    prg_label = make_top_level_label("listener options", prg_box);

    include_listener = listener_is_visible();
    prf = prefs_row_with_toggle("show listener at start up", S_show_listener,
				include_listener,
				prg_box, prg_label,
				show_listener_toggle);
    remember_pref(prf, reflect_show_listener, save_show_listener);

#if HAVE_SCHEME
    current_sep = make_inter_variable_separator(prg_box, prf->label);
    str = mus_format("%d", optimization(ss));
    prf = prefs_row_with_number("optimization level", S_optimization,
				str, 3, 
				prg_box, current_sep, 
				optimization_up, optimization_down, optimization_from_text);
    remember_pref(prf, reflect_optimization, NULL);
    FREE(str);
    if (optimization(ss) == 6) XtSetSensitive(prf->arrow_up, false);
    if (optimization(ss) == 0) XtSetSensitive(prf->arrow_down, false);
#endif

    current_sep = make_inter_variable_separator(prg_box, prf->label);
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

#if HAVE_SCHEME
    current_sep = make_inter_variable_separator(prg_box, prf->label);
    prf = prefs_row_with_toggle("include debugging aids", "snd-break",
				find_debugging_aids(),
				prg_box, current_sep,
				debugging_aids_toggle);
    remember_pref(prf, reflect_debugging_aids, save_debugging_aids);
    include_debugging_aids = find_debugging_aids();
#endif

    current_sep = make_inter_variable_separator(prg_box, prf->label);
    str = mus_format("%d", print_length(ss));
    prf = prefs_row_with_text("number of vector elements to display", S_print_length, str,
			      prg_box, current_sep,
			      print_length_text);
    remember_pref(prf, reflect_print_length, NULL);
    FREE(str);

    current_sep = make_inter_variable_separator(prg_box, prf->label);
    prf = prefs_row_with_text("font", S_listener_font, 
			      listener_font(ss), 
			      prg_box, current_sep,
			      listener_font_text);
    remember_pref(prf, reflect_listener_font, NULL);

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
  }

  current_sep = make_inter_topic_separator(topics);

  /* -------- audio -------- */
  {
    Widget aud_box, aud_label;

    /* ---------------- audio options ---------------- */

    aud_box = make_top_level_box(topics);
    aud_label = make_top_level_label("audio options", aud_box);

    str = mus_format("%d", dac_size(ss));
    prf = prefs_row_with_text("dac buffer size", S_dac_size, 
			      str,
			      aud_box, aud_label,
			      dac_size_text);
    remember_pref(prf, reflect_dac_size, NULL);
    FREE(str);

    current_sep = make_inter_variable_separator(aud_box, prf->label);

    /*
    audio section:
    dac-combines-channels, smart line cursor, recorder setting, audio mixer settings
    ss->Dac_Combines_Channels = DEFAULT_DAC_COMBINES_CHANNELS;
    recorder stuff? -> buffer-size in-chans in-data-format in-device out-chans out-data-format out-header-type srate
    audio mixer settings? -> volume in some mode
    audio output device? or sound card?
    */
  }

  current_sep = make_inter_topic_separator(topics);

  /* -------- silly stuff -------- */
  {
    Widget silly_box, silly_label;

    /* ---------------- silly options ---------------- */

    silly_box = make_top_level_box(topics);
    silly_label = make_top_level_label("silly stuff", silly_box);

    saved_help_button_color = ss->sgx->help_button_color;
    prf = prefs_color_selector_row("help button color", S_help_button_color, ss->sgx->help_button_color,
				   silly_box, silly_label,
				   help_button_color_func);
    remember_pref(prf, reflect_help_button_color, NULL);
    current_sep = make_inter_variable_separator(silly_box, prf->rscl);

    saved_reset_button_color = ss->sgx->reset_button_color;
    prf = prefs_color_selector_row("reset button color", S_reset_button_color, ss->sgx->reset_button_color,
				   silly_box, current_sep,
				   reset_button_color_func);
    remember_pref(prf, reflect_reset_button_color, NULL);
    current_sep = make_inter_variable_separator(silly_box, prf->rscl);

    saved_quit_button_color = ss->sgx->quit_button_color;
    prf = prefs_color_selector_row("quit button color", S_quit_button_color, ss->sgx->quit_button_color,
				   silly_box, current_sep,
				   quit_button_color_func);
    remember_pref(prf, reflect_quit_button_color, NULL);
    current_sep = make_inter_variable_separator(silly_box, prf->rscl);

    saved_doit_button_color = ss->sgx->doit_button_color;
    prf = prefs_color_selector_row("doit button color", S_doit_button_color, ss->sgx->doit_button_color,
				   silly_box, current_sep,
				   doit_button_color_func);
    remember_pref(prf, reflect_doit_button_color, NULL);
    current_sep = make_inter_variable_separator(silly_box, prf->rscl);

    saved_doit_again_button_color = ss->sgx->doit_again_button_color;
    prf = prefs_color_selector_row("doit-again button color", S_doit_again_button_color, ss->sgx->doit_again_button_color,
				   silly_box, current_sep,
				   doit_again_button_color_func);
    remember_pref(prf, reflect_doit_again_button_color, NULL);

    current_sep = make_inter_variable_separator(silly_box, prf->rscl);
    saved_pushed_button_color = ss->sgx->pushed_button_color;
    prf = prefs_color_selector_row("pushed-button color", S_pushed_button_color, ss->sgx->pushed_button_color,
				   silly_box, current_sep,
				   pushed_button_color_func);
    remember_pref(prf, reflect_pushed_button_color, NULL);

  }
#if DEBUGGING
  fprintf(stderr, "top: %d\n", prefs_top);
#endif

  {
    Atom wm_delete_window;
    wm_delete_window = XmInternAtom(MAIN_DISPLAY(ss), "WM_DELETE_WINDOW", false);
    XmAddWMProtocolCallback(XtParent(preferences_dialog), wm_delete_window, wm_delete_callback, NULL);
  }

  XtManageChild(preferences_dialog);
  set_dialog_widget(PREFERENCES_DIALOG, preferences_dialog);
}
