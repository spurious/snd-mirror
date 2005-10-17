#include "snd.h"

static GtkWidget *preferences_dialog = NULL;

#if DEBUGGING
/* whole file on this switch until I can get it into shape */

static bool prefs_helping = false;

#define MID_POSITION 40
#define COLOR_POSITION 50
#define FIRST_COLOR_POSITION 6
#define SECOND_COLOR_POSITION 30
#define THIRD_COLOR_POSITION 55
#define HELP_POSITION 80

#define MID_SPACE 16
#define INTER_TOPIC_SPACE 3
#define INTER_VARIABLE_SPACE 2

#define HELP_WAIT_TIME ((guint32)500)
#define POWER_WAIT_TIME ((guint32)100)
#define POWER_INITIAL_WAIT_TIME ((guint32)500)
#define ERROR_WAIT_TIME ((guint32)1000)

#define STARTUP_WIDTH 925
#define STARTUP_HEIGHT 800

typedef struct prefs_info {
  GtkWidget *label, *text, *arrow_up, *arrow_down, *arrow_right, *error, *toggle, *scale;
  GtkWidget *color, *rscl, *gscl, *bscl, *rtxt, *gtxt, *btxt, *list_menu, *radio_button, *helper;
  GtkObject *radj, *gadj, *badj;
  GtkWidget **radio_buttons;
  bool got_error;
  guint help_id, power_id;
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
  int i, len, j = 0;
  char *trimmed_str;
  len = strlen(str);
  trimmed_str = (char *)CALLOC(len + 1, sizeof(char));
  for (i = 0; i < len; i++)
    if (!(isspace(str[i])))
      trimmed_str[j++] = str[i];
  return(trimmed_str);
}

static void int_to_textfield(GtkWidget *w, int val)
{
  char *str;
  str = (char *)CALLOC(16, sizeof(char));
  mus_snprintf(str, 16, "%d", val);
  gtk_entry_set_text(GTK_ENTRY(w), str);
  FREE(str);
}

static void float_to_textfield(GtkWidget *w, Float val)
{
  char *str;
  str = (char *)CALLOC(12, sizeof(char));
  mus_snprintf(str, 12, "%.3f", val);
  gtk_entry_set_text(GTK_ENTRY(w), str);
  FREE(str);
}

static GtkWidget *find_radio_button(prefs_info *prf, int num)
{
  return(prf->radio_buttons[num]);
}



/* ---------------- help strings ---------------- */

static void prefs_help(prefs_info *prf)
{
  if (prf->var_name)
    snd_help(prf->var_name, 
	     XEN_TO_C_STRING(XEN_OBJECT_HELP(C_STRING_TO_XEN_SYMBOL((char *)(prf->var_name)))),
	     WITH_WORD_WRAP);
}

static void prefs_help_click_callback(GtkWidget *w, gpointer context)
{
  prefs_help((prefs_info *)context);
}

static gboolean prefs_tooltip_help(gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if (help_dialog_is_active())
    prefs_help(prf);
  else prefs_helping = false;
  prf->help_id = 0;
  return(false);
}

static gboolean mouse_enter_pref_callback(GtkWidget *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if (prefs_helping)
    prf->help_id = g_timeout_add_full(0,
				      HELP_WAIT_TIME,
				      prefs_tooltip_help,
				      (gpointer)prf, NULL);
  return(false);
}

static gboolean mouse_leave_pref_callback(GtkWidget *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if (prf->help_id != 0)
    {
      g_source_remove(prf->help_id);
      prf->help_id = 0;
    }
  return(false);
}


/* ---------------- row (main) label widget ---------------- */

static GtkWidget *make_row_label(const char *label, GtkWidget *box, GtkWidget *top_widget)
{
  GtkWidget *w;
  w = snd_gtk_label_new(label, ss->sgx->white);
  gtk_box_pack_start(GTK_BOX(box), w, false, false, 0);
  gtk_widget_show(w);
  return(w);
  /* needs to be attached on left to box, on right to mid position */
}

/* ---------------- row inner label widget ---------------- */

static GtkWidget *make_row_inner_label(const char *label, GtkWidget *left_widget, GtkWidget *box, GtkWidget *top_widget)
{
  GtkWidget *w;
  w = snd_gtk_label_new(label, ss->sgx->white);
  gtk_box_pack_start(GTK_BOX(box), w, false, false, 0);
  gtk_widget_show(w);
  return(w);
  /* needs to be attached on left to left_widget */
}

/* ---------------- row middle separator widget ---------------- */

static GtkWidget *make_row_middle_separator(GtkWidget *label, GtkWidget *box, GtkWidget *top_widget)
{
  GtkWidget *w;
  w = gtk_vseparator_new();
  gtk_box_pack_start(GTK_BOX(box), w, false, false, 0);
  gtk_widget_show(w);
  return(w);
  /* needs to be white, with etched in line, next to label (left), width = MID_SPACE */
}

/* ---------------- row inner separator widget ---------------- */

static GtkWidget *make_row_inner_separator(int width, GtkWidget *left_widget, GtkWidget *box, GtkWidget *top_widget)
{
  GtkWidget *w;
  w = gtk_hseparator_new();
  gtk_box_pack_start(GTK_BOX(box), w, false, false, 0);
  gtk_widget_show(w);
  return(w);
  /* white, no line, width = width, left_widget on left */
}

/* ---------------- row help widget ---------------- */

static GtkWidget *make_row_help(const char *label, prefs_info *prf, GtkWidget *box, GtkWidget *top_widget, GtkWidget *left_widget)
{
  GtkWidget *w;
  w = snd_gtk_label_new("NO HELP", ss->sgx->white); /* FIX */
  /* spacer to HELP_POSITION, then a pushbutton widget with no borders etc */
  gtk_box_pack_start(GTK_BOX(box), w, false, false, 0);
  gtk_widget_show(w);
  return(w);
}

/* ---------------- row toggle widget ---------------- */

static GtkWidget *make_row_toggle(bool current_value, GtkWidget *left_widget, GtkWidget *box, GtkWidget *top_widget)
{
  GtkWidget *w;
  /* toggle button */
  w = snd_gtk_label_new("NO HELP", ss->sgx->white); /* FIX */
  gtk_box_pack_start(GTK_BOX(box), w, false, false, 0);
  gtk_widget_show(w);
  return(w);
}


/* ---------------- bool row ---------------- */

static void call_toggle_func(GtkWidget *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->toggle_func))
    (*(prf->toggle_func))(prf);
}

static prefs_info *prefs_row_with_toggle(const char *label, const char *varname, bool current_value,
					 GtkWidget *box, GtkWidget *top_widget, 
					 void (*toggle_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *sep;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;

  prf->label = make_row_label(label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  prf->toggle = make_row_toggle(current_value, sep, box, top_widget);
  make_row_help(varname, prf, box, top_widget, prf->toggle);

  SG_SIGNAL_CONNECT(prf->toggle, "toggled", call_toggle_func, (gpointer)prf);

  SG_SIGNAL_CONNECT(prf->label, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->toggle, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->label, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->toggle, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);

  return(prf);
}


/* ---------------- toggle with text ---------------- */

static GtkWidget *make_row_text(const char *text_value, int cols, GtkWidget *left_widget, GtkWidget *box, GtkWidget *top_widget)
{
  GtkWidget *w;
  /* toggle button */
  w = snd_gtk_label_new("NO HELP", ss->sgx->white); /* FIX */
  gtk_box_pack_start(GTK_BOX(box), w, false, false, 0);
  gtk_widget_show(w);
  return(w);
}

static void call_text_func(GtkWidget *w, gpointer context) 
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->text_func))
    (*(prf->text_func))(prf);
}

static prefs_info *prefs_row_with_toggle_with_text(const char *label, const char *varname, bool current_value,
						   const char *text_label, const char *text_value, int cols,
						   GtkWidget *box, GtkWidget *top_widget, 
						   void (*toggle_func)(prefs_info *prf),
						   void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *sep, *sep1, *lab1;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;
  prf->text_func = text_func;

  prf->label = make_row_label(label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  prf->toggle = make_row_toggle(current_value, sep, box, top_widget);
  sep1 = make_row_inner_separator(16, prf->toggle, box, top_widget);
  lab1 = make_row_inner_label(text_label, sep1, box, top_widget);
  prf->text= make_row_text(text_value, cols, lab1, box, top_widget);
  make_row_help(varname, prf, box, top_widget, prf->text);

  SG_SIGNAL_CONNECT(prf->toggle, "toggled", call_toggle_func, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "activate", call_text_func, (gpointer)prf);

  SG_SIGNAL_CONNECT(prf->label, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->toggle, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->label, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->toggle, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);

  return(prf);
}


/* ---------------- text with toggle ---------------- */

static prefs_info *prefs_row_with_text_with_toggle(const char *label, const char *varname, bool current_value,
						   const char *toggle_label, const char *text_value, int cols,
						   GtkWidget *box, GtkWidget *top_widget, 
						   void (*toggle_func)(prefs_info *prf),
						   void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *sep, *sep1, *lab1;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;
  prf->text_func = text_func;

  prf->label = make_row_label(label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  prf->text = make_row_text(text_value, cols, sep, box, top_widget);
  sep1 = make_row_inner_separator(8, prf->text, box, top_widget);
  lab1 = make_row_inner_label(toggle_label, sep1, box, top_widget);
  prf->toggle = make_row_toggle(current_value, lab1, box, top_widget);  
  make_row_help(varname, prf, box, top_widget, prf->text);
  
  SG_SIGNAL_CONNECT(prf->toggle, "toggled", call_toggle_func, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "activate", call_text_func, (gpointer)prf);

  SG_SIGNAL_CONNECT(prf->label, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->toggle, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->label, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->toggle, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);

  return(prf);
}


/* ---------------- radio row ---------------- */

static void call_radio_func(GtkWidget *w, gpointer context)
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
					    GtkWidget *box, GtkWidget *top_widget, 
					    void (*toggle_func)(prefs_info *prf))
{
  int i, n;
  prefs_info *prf = NULL;
  GtkWidget *sep, *current_button;
  GSList *group = NULL;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;

  prf->label = make_row_label(label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);

  prf->toggle = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(box), prf->toggle, false, false, 0);
  gtk_widget_show(prf->toggle);

  prf->radio_buttons = (GtkWidget **)CALLOC(num_labels, sizeof(GtkWidget *));

  for (i = 0; i < num_labels; i++)
    {
      current_button = gtk_radio_button_new_with_label(group, labels[i]);
      prf->radio_buttons[i] = current_button;
      gtk_box_pack_start(GTK_BOX(prf->toggle), current_button, false, false, 0);
      set_user_int_data(G_OBJECT(current_button), i);
      gtk_widget_show(current_button);
      SG_SIGNAL_CONNECT(current_button, "clicked", call_radio_func, (gpointer)prf);
      
      if (i == 0)
	group = gtk_radio_button_get_group(GTK_RADIO_BUTTON(current_button));
    }

  make_row_help(varname, prf, box, top_widget, prf->toggle);

  SG_SIGNAL_CONNECT(prf->label, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->label, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->toggle, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->toggle, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);

  return(prf);
}


/* ---------------- scale row ---------------- */

static void call_scale_func(GtkWidget *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->scale_func))
    (*(prf->scale_func))(prf);
}

static void call_scale_text_func(GtkWidget *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->text_func))
    (*(prf->text_func))(prf);
}

static void prefs_scale_callback(GtkWidget *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  float value = 0.0;
  float_to_textfield(prf->text, (value * prf->scale_max) / 100.0);
}

static prefs_info *prefs_row_with_scale(const char *label, const char *varname, 
					Float max_val, Float current_value,
					GtkWidget *box, GtkWidget *top_widget, 
					void (*scale_func)(prefs_info *prf),
					void (*text_func)(prefs_info *prf))
{
  int i, n;
  prefs_info *prf = NULL;
  GtkWidget *sep;
  char *str;

  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->scale_max = max_val;

  prf->label = make_row_label(label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  
  str = (char *)CALLOC(12, sizeof(char));
  mus_snprintf(str, 12, "%.3f", current_value);
  prf->text = make_row_text(str, 6, sep, box, top_widget);
  FREE(str);

  /* scale */
  
  make_row_help(varname, prf, box, top_widget, prf->scale);

  prf->scale_func = scale_func;
  prf->text_func = text_func;
#if 0
  SG_SIGNAL_CONNECT(prf->scale, XmNvalueChangedCallback, call_scale_func, (gpointer)prf);
#endif
  SG_SIGNAL_CONNECT(prf->text, "activate", call_scale_text_func, (gpointer)prf);

  SG_SIGNAL_CONNECT(prf->label, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->scale, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->label, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->scale, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);

  return(prf);
}


/* ---------------- text row ---------------- */

static prefs_info *prefs_row_with_text(const char *label, const char *varname, const char *value,
				       GtkWidget *box, GtkWidget *top_widget,
				       void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *sep;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;

  prf->label = make_row_label(label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  prf->text = make_row_text(value, 0, sep, box, top_widget);
  
  make_row_help(varname, prf, box, top_widget, prf->text);

  prf->text_func = text_func;

  SG_SIGNAL_CONNECT(prf->text, "activate", call_text_func, (gpointer)prf);

  SG_SIGNAL_CONNECT(prf->label, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->label, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);

  return(prf);
}


/* ---------------- two texts in a row ---------------- */

static prefs_info *prefs_row_with_two_texts(const char *label, const char *varname,
					    const char*label1, const char *text1, const char*label2, const char *text2, int cols,
					    GtkWidget *box, GtkWidget *top_widget,
					    void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *sep, *lab1, *lab2;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;

  prf->label = make_row_label(label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  lab1 = make_row_inner_label(label1, sep, box, top_widget);
  prf->text = make_row_text(text1, cols, lab1, box, top_widget);
  lab2 = make_row_inner_label(label2, prf->text, box, top_widget);  
  prf->rtxt = make_row_text(text2, cols, lab2, box, top_widget);
  make_row_help(varname, prf, box, top_widget, prf->rtxt);

  prf->text_func = text_func;

  SG_SIGNAL_CONNECT(prf->text, "activate", call_text_func, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->rtxt, "activate", call_text_func, (gpointer)prf);

  SG_SIGNAL_CONNECT(prf->label, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->rtxt, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->label, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->rtxt, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);

  return(prf);
}


/* ---------------- number row ---------------- */

static void remove_arrow_func(GtkWidget *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if (prf->power_id != 0)
    {
      g_source_remove(prf->power_id);
      prf->power_id = 0;
    }
}

static gint arrow_func_up(gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if (GTK_WIDGET_IS_SENSITIVE(prf->arrow_up))
    {
      if ((prf) && (prf->arrow_up_func))
	{
	  (*(prf->arrow_up_func))(prf);
	  prf->power_id = g_timeout_add_full(0,
					  POWER_WAIT_TIME,
					  arrow_func_up,
					     (gpointer)prf, NULL);
	}
      else prf->power_id = 0;
    }
  return(0);
}

static gint arrow_func_down(gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if (GTK_WIDGET_IS_SENSITIVE(prf->arrow_down))
    {
      if ((prf) && (prf->arrow_down_func))
	{
	  (*(prf->arrow_down_func))(prf);
	  prf->power_id = g_timeout_add_full(0,
					  POWER_WAIT_TIME,
					  arrow_func_down,
					     (gpointer)prf, NULL);
	}
      else prf->power_id = 0;
    }
  return(0);
}

static void call_arrow_down_press(GtkWidget *w, gpointer context) 
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->arrow_down_func))
    {
      (*(prf->arrow_down_func))(prf);
      if (GTK_WIDGET_IS_SENSITIVE(w))
	prf->power_id = g_timeout_add_full(0,
					POWER_INITIAL_WAIT_TIME,
					arrow_func_down,
					   (gpointer)prf, NULL);
      else prf->power_id = 0;
    }
}

static void call_arrow_up_press(GtkWidget *w, gpointer context) 
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->arrow_up_func))
    {
      (*(prf->arrow_up_func))(prf);
      if (GTK_WIDGET_IS_SENSITIVE(w))
	prf->power_id = g_timeout_add_full(0,
					POWER_INITIAL_WAIT_TIME,
					arrow_func_up,
					   (gpointer)prf, NULL);
      else prf->power_id = 0;
    }
}

static prefs_info *prefs_row_with_number(const char *label, const char *varname, const char *value, int cols,
					 GtkWidget *box, GtkWidget *top_widget,
 					 void (*arrow_up_func)(prefs_info *prf), void (*arrow_down_func)(prefs_info *prf), 
					 void (*text_func)(prefs_info *prf))
{
  int n;
  prefs_info *prf = NULL;
  GtkWidget *sep;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;

  prf->label = make_row_label(label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);
  
  prf->text = make_row_text(value, cols, sep, box, top_widget);
  
  /* arrows error */
  make_row_help(varname, prf, box, top_widget, prf->error);

  prf->text_func = text_func;
  prf->arrow_up_func = arrow_up_func;
  prf->arrow_down_func = arrow_down_func;
#if 0
  SG_SIGNAL_CONNECT(prf->arrow_down, XmNarmCallback, call_arrow_down_press, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->arrow_down, XmNdisarmCallback, remove_arrow_func, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->arrow_up, XmNarmCallback, call_arrow_up_press, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->arrow_up, XmNdisarmCallback, remove_arrow_func, (gpointer)prf);
#endif

  SG_SIGNAL_CONNECT(prf->text, "activate", call_text_func, (gpointer)prf);

  SG_SIGNAL_CONNECT(prf->label, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->arrow_up, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->arrow_down, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->error, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->label, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->arrow_up, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->arrow_down, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->error, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);

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

static void prefs_list_callback(GtkWidget *w, gpointer context)
{
  list_entry *le = (list_entry *)context;
  if ((le) && (le->prf->list_func))
    (*(le->prf->list_func))(le->prf, le->value);
}

static prefs_info *prefs_row_with_completed_list(const char *label, const char *varname, const char *value,
						 const char **values, int num_values,
						 GtkWidget *box, GtkWidget *top_widget,
						 void (*text_func)(prefs_info *prf),
						 char *(*completion_func)(char *text, void *context), void *completion_context,
						 void (*list_func)(prefs_info *prf, char *value))
{
  int n, i, cols = 0;
  prefs_info *prf = NULL;
  GtkWidget *sep, *sbar;
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

  /* text + menu */

  make_row_help(varname, prf, box, top_widget, prf->error);

  prf->text_func = text_func;
  prf->list_func = list_func;

  SG_SIGNAL_CONNECT(prf->text, "activate", call_text_func, (gpointer)prf);

  SG_SIGNAL_CONNECT(prf->label, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->arrow_right, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->error, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->label, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->arrow_right, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->error, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);

  return(prf);
}


/* ---------------- color selector row(s) ---------------- */

static GdkColor *rgb_to_color(Float r, Float g, Float b)
{
  GdkColor *new_color;
#if 0
  new_color = (GdkColor *)CALLOC(1, sizeof(GdkColor));
  new_color->flags = DoRed | DoGreen | DoBlue;
  new_color->red = (unsigned short)(65535 * r);
  new_color->green = (unsigned short)(65535 * g);
  new_color->blue = (unsigned short)(65535 * b);
  XAllocColor(dpy, DefaultColormap(dpy, DefaultScreen(dpy)), new_color);
#endif
  return(new_color);
}

static void pixel_to_rgb(color_t pix, float *r, float *g, float *b)
{
  GdkColor tmp_color;
#if 0
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  tmp_color.pixel = pix;
  XQueryColor(dpy, DefaultColormap(dpy, DefaultScreen(dpy)), &tmp_color);
  (*r) = (float)tmp_color.red / 65535.0;
  (*g) = (float)tmp_color.green / 65535.0;
  (*b) = (float)tmp_color.blue / 65535.0;
#endif
}

static void reflect_color(prefs_info *prf)
{
  int ir = 0, ig = 0, ib = 0;
  Float r, g, b;
  GdkColor *current_color;
#if 0
  color_t pixel;

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
#endif
}

static void prefs_color_callback(GtkWidget *w, gpointer context)
{
  reflect_color((prefs_info *)context);
}

static void prefs_r_callback(GtkWidget *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  char *str;
  float r = 0.0;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(w));
  sscanf(str, "%f", &r);
  if ((r >= 0.0) &&
      (r <= 1.0))
    {
#if 0
      XmScaleSetValue(prf->rscl, (int)(100 * r));
#endif
      reflect_color(prf);
    }
  else gtk_entry_set_text(GTK_ENTRY(w), "err");

}

static void prefs_g_callback(GtkWidget *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  char *str;
  float r = 0.0;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(w));
  sscanf(str, "%f", &r);
  if ((r >= 0.0) &&
      (r <= 1.0))
    {
#if 0
      XmScaleSetValue(prf->gscl, (int)(100 * r));
#endif
      reflect_color(prf);
    }
  else gtk_entry_set_text(GTK_ENTRY(w), "err");

}

static void prefs_b_callback(GtkWidget *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  char *str;
  float r = 0.0;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(w));
  sscanf(str, "%f", &r);
  if ((r >= 0.0) &&
      (r <= 1.0))
    {
#if 0
      XmScaleSetValue(prf->bscl, (int)(100 * r));
#endif
      reflect_color(prf);
    }
  else gtk_entry_set_text(GTK_ENTRY(w), "err");

}

static void prefs_call_color_func_callback(GtkWidget *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->color_func))
    {
      int ir = 0, ig = 0, ib = 0;
#if 0
      XmScaleGetValue(prf->rscl, &ir);
      XmScaleGetValue(prf->gscl, &ig);
      XmScaleGetValue(prf->bscl, &ib);
#endif
      (*(prf->color_func))(prf, (float)ir / 100.0, (float)ig / 100.0, (float)ib / 100.0);
    }
}

static prefs_info *prefs_color_selector_row(const char *label, const char *varname, 
					    color_t current_pixel,
					    GtkWidget *box, GtkWidget *top_widget,
					    void (*color_func)(prefs_info *prf, float r, float g, float b))
{

  int n;
  prefs_info *prf = NULL;
  GtkWidget *sep, *sep1, *frame;

  GdkColor *tmp;
  float r = 0.0, g = 0.0, b = 0.0;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
#if 0
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
#endif
  prf->label = make_row_label(label, box, top_widget);
  sep = make_row_middle_separator(prf->label, box, top_widget);    

  /* frames etc */

  SG_SIGNAL_CONNECT(prf->rtxt, "activate", prefs_r_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->gtxt, "activate", prefs_g_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->btxt, "activate", prefs_b_callback, (gpointer)prf);
#if 0
  SG_SIGNAL_CONNECT(prf->rscl, XmNvalueChangedCallback, prefs_call_color_func_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->gscl, XmNvalueChangedCallback, prefs_call_color_func_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->bscl, XmNvalueChangedCallback, prefs_call_color_func_callback, (gpointer)prf);
#endif
  SG_SIGNAL_CONNECT(prf->label, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->label, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->color, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->color, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->rtxt, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->rtxt, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->rscl, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->rscl, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->gtxt, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->gtxt, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->gscl, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->gscl, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->btxt, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->btxt, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->bscl, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->bscl, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);

  prf->color_func = color_func;

  return(prf);
}


/* ---------------- topic separator ---------------- */

static GtkWidget *make_inter_topic_separator(GtkWidget *topics)
{
  GtkWidget *w;
  w = gtk_hseparator_new();
  gtk_box_pack_start(GTK_BOX(topics), w, false, false, 0);
  gtk_widget_show(w);
  return(w);
  /* height = INTER_TOPIC_SPACE no line */
}

/* ---------------- variable separator ---------------- */

static GtkWidget *make_inter_variable_separator(GtkWidget *topics, GtkWidget *top_widget)
{
  GtkWidget *w;
  w = gtk_hseparator_new();
  gtk_box_pack_start(GTK_BOX(topics), w, false, false, 0);
  gtk_widget_show(w);
  return(w);
  /* height = INTER_VARIABLE_SPACE no line */
}

/* ---------------- top-level contents label ---------------- */

static GtkWidget *make_top_level_label(const char *label, GtkWidget *parent)
{
  GtkWidget *w;
  w = snd_gtk_label_new(label, ss->sgx->light_blue);
  gtk_box_pack_start(GTK_BOX(parent), w, false, false, 0);
  gtk_widget_show(w);
  return(w);
}

static GtkWidget *make_top_level_box(GtkWidget *topics)
{
  GtkWidget *w;
  w = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(topics), w, false, false, 0);
  gtk_widget_show(w);
  return(w);
  /* frame etc */
  return(NULL);
}

static GtkWidget *make_inner_label(const char *label, GtkWidget *parent, GtkWidget *top_widget)
{
  GtkWidget *w;
  w = snd_gtk_label_new(label, ss->sgx->highlight_color);
  gtk_box_pack_start(GTK_BOX(parent), w, false, false, 0);
  gtk_widget_show(w);
  return(w);
}


/* ---------------- base buttons ---------------- */

static gint preferences_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(preferences_dialog);
  return(true);
}

static void preferences_dismiss_callback(GtkWidget *w, gpointer context) 
{
  gtk_widget_hide(preferences_dialog);
}

static void preferences_help_callback(GtkWidget *w, gpointer context) 
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

static void preferences_reset_callback(GtkWidget *w, gpointer context) 
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

static void preferences_save_callback(GtkWidget *w, gpointer context) 
{
  save_prefs(save_options_in_prefs()); /* TODO: redirect (for save_options) + msg -- need a global error site */
}





/* ---------------- errors ---------------- */

static void clear_prefs_error(GtkWidget *w, gpointer context) 
{
  prefs_info *prf = (prefs_info *)context;
#if 0
  XtRemoveCallback(prf->text, XmNvalueChangedCallback, clear_prefs_error, context);
#endif
  set_label(prf->error, "");
}

static void post_prefs_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  prf->got_error = true;
  set_label(prf->error, msg);
#if 0
  SG_SIGNAL_CONNECT(prf->text, XmNvalueChangedCallback, clear_prefs_error, (void *)prf);
#endif
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

static gint startup_width_erase_func(gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->text, ss->init_window_width);
  return(0);
}

static gint startup_height_erase_func(gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->rtxt, ss->init_window_height);
  return(0);
}

static void startup_width_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  gtk_entry_set_text(GTK_ENTRY(prf->text), "right");
  g_timeout_add_full(0,
		  ERROR_WAIT_TIME,
		  startup_width_erase_func,
		     (gpointer)prf, NULL);
}

static void startup_height_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  gtk_entry_set_text(GTK_ENTRY(prf->rtxt), "right");
  g_timeout_add_full(0,
		  ERROR_WAIT_TIME,
		  startup_height_erase_func,
		     (gpointer)prf, NULL);
}

static void startup_size_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      int width = 0;
      redirect_errors_to(startup_width_error, (void *)prf);
      width = string_to_int(str, 1, "startup width");
      redirect_errors_to(NULL, NULL);
      if (width > 0) ss->init_window_width = width;

      str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->rtxt));
      if ((str) && (*str))
	{
	  int height;
	  redirect_errors_to(startup_height_error, (void *)prf);
	  height = string_to_int(str, 1, "startup height");
	  redirect_errors_to(NULL, NULL);
	  if (height > 0) ss->init_window_height = height;

	}
    }
}

/* ---------------- auto-resize ---------------- */

static void reflect_auto_resize(prefs_info *prf) 
{
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle), auto_resize(ss));
}

static void resize_toggle(prefs_info *prf)
{
  set_auto_resize(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

/* ---------------- ask-before-overwrite ---------------- */

static void reflect_ask_before_overwrite(prefs_info *prf) 
{
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle), ask_before_overwrite(ss));
}

static void overwrite_toggle(prefs_info *prf)
{
  set_ask_before_overwrite(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
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
  include_current_window_display = (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
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
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle), find_current_window_display());
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
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle), focus_follows_mouse);
}

static void focus_follows_mouse_toggle(prefs_info *prf)
{
  focus_follows_mouse = (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

static void save_focus_follows_mouse(prefs_info *prf, FILE *fd) 
{
  if (focus_follows_mouse)
    {
#if HAVE_SCHEME
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
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle), in_show_controls(ss));
}

static void controls_toggle(prefs_info *prf)
{
  in_set_show_controls(ss, gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

/* ---------------- selection-creates-region ---------------- */

static void reflect_selection_creates_region(prefs_info *prf) 
{
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle), selection_creates_region(ss));
}

static void selection_creates_region_toggle(prefs_info *prf)
{
  set_selection_creates_region(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

/* ---------------- basic-color ---------------- */

static void scale_set_color(prefs_info *prf, color_t pixel)
{
#if 0
  float r = 0.0, g = 0.0, b = 0.0;
  pixel_to_rgb(pixel, &r, &g, &b);
  float_to_textfield(prf->rtxt, r);
  XmScaleSetValue(prf->rscl, (int)(100 * r));
  float_to_textfield(prf->gtxt, g);
  XmScaleSetValue(prf->gscl, (int)(100 * g));
  float_to_textfield(prf->btxt, b);
  XmScaleSetValue(prf->bscl, (int)(100 * b));
  XtVaSetValues(prf->color, XmNbackground, pixel, NULL);
#endif
}

static color_t saved_basic_color;

static void reflect_basic_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_basic_color); 
  set_basic_color(saved_basic_color);
}

static void basic_color_func(prefs_info *prf, float r, float g, float b)
{
  GdkColor *tmp;
  tmp = rgb_to_color(r, g, b);
  set_basic_color(tmp);
  FREE(tmp);
}

/* ---------------- highlight-color ---------------- */

static color_t saved_highlight_color;

static void reflect_highlight_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_highlight_color); 
  set_highlight_color(saved_highlight_color);
}

static void highlight_color_func(prefs_info *prf, float r, float g, float b)
{
  GdkColor *tmp;
  tmp = rgb_to_color(r, g, b);
  set_highlight_color(tmp);
  FREE(tmp);
}

/* ---------------- position-color ---------------- */

static color_t saved_position_color;

static void reflect_position_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_position_color); 
  set_position_color(saved_position_color);
}

static void position_color_func(prefs_info *prf, float r, float g, float b)
{
  GdkColor *tmp;
  tmp = rgb_to_color(r, g, b);
  set_position_color(tmp);
  FREE(tmp);
}

/* ---------------- zoom-color ---------------- */

static color_t saved_zoom_color;

static void reflect_zoom_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_zoom_color); 
  set_zoom_color(saved_zoom_color);
}

static void zoom_color_func(prefs_info *prf, float r, float g, float b)
{
  GdkColor *tmp;
  tmp = rgb_to_color(r, g, b);
  set_zoom_color(tmp);
  FREE(tmp);
}

/* ---------------- verbose-cursor ---------------- */

static void reflect_verbose_cursor(prefs_info *prf) 
{
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle), verbose_cursor(ss));
}

static void verbose_cursor_toggle(prefs_info *prf)
{
  in_set_verbose_cursor(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

/* ---------------- cursor-follows-play ---------------- */

static void reflect_cursor_follows_play(prefs_info *prf) 
{
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle), cursor_follows_play(ss));
}

static void cursor_follows_play_toggle(prefs_info *prf)
{
  in_set_cursor_follows_play(ss, (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle))) ? FOLLOW_ALWAYS : DONT_FOLLOW);
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
  gtk_widget_set_sensitive(prf->arrow_up, cursor_size(ss) < MAX_CURSOR_SIZE);
  gtk_widget_set_sensitive(prf->arrow_down, cursor_size(ss) > MIN_CURSOR_SIZE);
}

static void cursor_size_up(prefs_info *prf)
{
  int size;
  size = cursor_size(ss) + 1;
  if (size >= MAX_CURSOR_SIZE) gtk_widget_set_sensitive(prf->arrow_up, false);
  if (size > MIN_CURSOR_SIZE) gtk_widget_set_sensitive(prf->arrow_down, true);
  in_set_cursor_size(size);
  show_cursor_size(prf);
}

static void cursor_size_down(prefs_info *prf)
{
  int size;
  size = cursor_size(ss) - 1;
  if (size <= MIN_CURSOR_SIZE) gtk_widget_set_sensitive(prf->arrow_down, false);
  if (size < MAX_CURSOR_SIZE) gtk_widget_set_sensitive(prf->arrow_up, true);
  in_set_cursor_size(size);
  show_cursor_size(prf);
}

static void cursor_size_from_text(prefs_info *prf)
{
  int size;
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      prf->got_error = false;
      redirect_errors_to(post_prefs_error, (void *)prf);
      size = string_to_int(str, 0, "cursor size"); 
      redirect_errors_to(NULL, NULL);

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
  GtkWidget *w;
#if 0
  w = find_radio_button(prf, cursor_style(ss));
  if (w)
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(w), XmSET);
  else fprintf(stderr, "can't find %s\n", cursor_styles[cursor_style(ss)]);
  if ((prf->radio_button) &&
      (XmIsToggleButton(prf->radio_button)) &&
      (w != prf->radio_button))
    {
      /* motif docs are incorrect -- the set above does not unset the currently set radio button */
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->radio_button), XmUNSET);
      prf->radio_button = w;
    }
#endif
}

static void cursor_style_choice(prefs_info *prf)
{
#if 0
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
    {
      if (strcmp(XtName(prf->radio_button), "line") == 0)
	in_set_cursor_style(CURSOR_LINE);
      else in_set_cursor_style(CURSOR_CROSS);
    }
#endif
}

/* ---------------- cursor-color ---------------- */

static color_t saved_cursor_color;

static void reflect_cursor_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_cursor_color); 
  color_cursor(saved_cursor_color);
}

static void cursor_color_func(prefs_info *prf, float r, float g, float b)
{
  GdkColor *tmp;
  tmp = rgb_to_color(r, g, b);
  color_cursor(tmp);
  for_each_chan(update_graph);
  FREE(tmp);
}


/* ---------------- just-sounds ---------------- */

static void prefs_reflect_just_sounds(prefs_info *prf) 
{
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle), just_sounds(ss));
}

static void just_sounds_toggle(prefs_info *prf)
{
  set_just_sounds(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}


/* ---------------- temp-dir ---------------- */

static void reflect_temp_dir(prefs_info *prf)
{
  gtk_entry_set_text(GTK_ENTRY(prf->text), temp_dir(ss));
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

static gint temp_dir_error_erase_func(gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  gtk_entry_set_text(GTK_ENTRY(prf->text), temp_dir(ss));
  return(0);
}

static void temp_dir_text(prefs_info *prf)
{
  char *str, *dir = NULL;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
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
      gtk_entry_set_text(GTK_ENTRY(prf->text), "can't access that directory");
      g_timeout_add_full(0,
		      ERROR_WAIT_TIME,
		      temp_dir_error_erase_func,
			 (gpointer)prf, NULL);
    }

}

/* ---------------- save-dir ---------------- */

static gint save_dir_error_erase_func(gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  gtk_entry_set_text(GTK_ENTRY(prf->text), save_dir(ss));
  return(0);
}

static void reflect_save_dir(prefs_info *prf)
{
  gtk_entry_set_text(GTK_ENTRY(prf->text), save_dir(ss));
}

static void save_dir_text(prefs_info *prf)
{
  char *str, *dir = NULL;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
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
      gtk_entry_set_text(GTK_ENTRY(prf->text), "can't access that directory");
      g_timeout_add_full(0,
		      ERROR_WAIT_TIME,
		      save_dir_error_erase_func,
			 (gpointer)prf, NULL);
    }

}

/* ---------------- save-state-file ---------------- */

static void reflect_save_state_file(prefs_info *prf)
{
  gtk_entry_set_text(GTK_ENTRY(prf->text), save_state_file(ss));
}

static void save_state_file_text(prefs_info *prf)
{
  char *str, *file = NULL;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((!str) || (!(*str))) 
    file = DEFAULT_SAVE_STATE_FILE;
  else file = str;
  if (save_state_file(ss)) FREE(save_state_file(ss));
  in_set_save_state_file(copy_string(file));

}


#if HAVE_LADSPA
/* ---------------- ladspa-dir ---------------- */

static void reflect_ladspa_dir(prefs_info *prf)
{
  gtk_entry_set_text(GTK_ENTRY(prf->text), ladspa_dir(ss));
}

static void ladspa_dir_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if (ladspa_dir(ss)) FREE(ladspa_dir(ss));
  if ((!str) || (!(*str)))
    {
      set_ladspa_dir(copy_string(str));

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
  gtk_entry_set_text(GTK_ENTRY(prf->text), view_files_find_any_directory());
}

static void view_files_directory_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if (include_vf_directory) FREE(include_vf_directory);
  include_vf_directory = copy_string(str); /* could be null to cancel */
  if ((!str) || (!(*str)))
    {
      view_files_add_directory(NULL_WIDGET, (const char *)str);

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
  gtk_entry_set_text(GTK_ENTRY(prf->text), html_program(ss));
}

static void html_program_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if (html_program(ss)) FREE(html_program(ss));
  if ((!str) || (!(*str)))
    {
      set_html_program(copy_string(str));

    }
  else set_html_program(copy_string(DEFAULT_HTML_PROGRAM));
}

/* ---------------- default-output-chans etc ---------------- */

static const char *output_chan_choices[4] = {"1", "2", "4", "8"};
static const char *output_srate_choices[4] = {"8000", "22050", "44100", "48000"};

static void reflect_output_chans(prefs_info *prf)
{
  char *str;
  GtkWidget *w;
  str = (char *)CALLOC(6, sizeof(char));
  mus_snprintf(str, 6, "%d", default_output_chans(ss));
#if 0
  w = find_radio_button(prf, /* CHANS */ str);
  if (w)
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(w), XmSET);
  else fprintf(stderr, "can't find %s\n", str);
  FREE(str);
  if ((prf->radio_button) &&
      (XmIsToggleButton(prf->radio_button)) &&
      (w != prf->radio_button))
    {
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->radio_button), XmUNSET);
      prf->radio_button = w;
    }
#endif
}

static void reflect_output_srate(prefs_info *prf)
{
  char *str;
  GtkWidget *w;
  str = (char *)CALLOC(8, sizeof(char));
  mus_snprintf(str, 8, "%d", default_output_srate(ss));
#if 0
  w = find_radio_button(prf, /* SRATE */ str);
  if (w)
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(w), XmSET);
  else fprintf(stderr, "can't find %s\n", str);
  FREE(str);
  if ((prf->radio_button) &&
      (XmIsToggleButton(prf->radio_button)) &&
      (w != prf->radio_button))
    {
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->radio_button), XmUNSET);
      prf->radio_button = w;
    }
#endif
}

static void output_chans_choice(prefs_info *prf)
{
#if 0
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
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
#endif
}

static void output_srate_choice(prefs_info *prf)
{
#if 0
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
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
#endif
}

static const char *output_type_choices[5] = {"aifc", "wave", "next/sun", "nist", "aiff"};
static const char *output_format_choices[4] = {"short", "int", "float", "double"};

static void reflect_output_type(prefs_info *prf)
{
  char *str = "not a choice";
  GtkWidget *w;
  switch (default_output_header_type(ss))
    {
    case MUS_AIFC: str = "aifc"; break;
    case MUS_AIFF: str = "aiff"; break;
    case MUS_RIFF: str = "wave"; break;
    case MUS_NEXT: str = "next/sun"; break;
    case MUS_NIST: str = "nist"; break;
    }
#if 0
  w = find_radio_button(prf, /* TYPE */ str);
  if (w)
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(w), XmSET);
  else fprintf(stderr, "can't find %s\n", str);
  if ((prf->radio_button) &&
      (XmIsToggleButton(prf->radio_button)) &&
      (w != prf->radio_button))
    {
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->radio_button), XmUNSET);
      prf->radio_button = w;
    }
#endif
}

static void reflect_output_format(prefs_info *prf)
{
  char *str = "not a choice";
  GtkWidget *w;
  switch (default_output_data_format(ss))
    {
    case MUS_LINT: case MUS_BINT: str = "int"; break;
    case MUS_LSHORT: case MUS_BSHORT: str = "short"; break;
    case MUS_LFLOAT: case MUS_BFLOAT: str = "float"; break;
    case MUS_LDOUBLE: case MUS_BDOUBLE: str = "double"; break;
    }
#if 0
  w = find_radio_button(prf, /* FORMAT */ str);
  if (w)
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(w), XmSET);
  else fprintf(stderr, "can't find %s\n", str);
  if ((prf->radio_button) &&
      (XmIsToggleButton(prf->radio_button)) &&
      (w != prf->radio_button))
    {
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->radio_button), XmUNSET);
      prf->radio_button = w;
    }
#endif
}

static prefs_info *output_data_format_prf = NULL, *output_header_type_prf = NULL;

static void output_type_choice(prefs_info *prf)
{
#if 0
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
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
#endif
}

static void output_format_choice(prefs_info *prf)
{
#if 0
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
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
#endif
}



/* ---------------- context sensitive popup ---------------- */

static bool include_context_sensitive_popup = false;

static void save_context_sensitive_popup(prefs_info *prf, FILE *fd)
{
  if (include_context_sensitive_popup)
    {
#if HAVE_SCHEME
      fprintf(fd, "(if (not (provided? 'snd-popup.scm)) (load-from-path \"popup.scm\"))\n");
#endif
#if HAVE_RUBY
      fprintf(fd, "require \"popup\"\n");
#endif
    }
}

static void context_sensitive_popup_toggle(prefs_info *prf)
{
  include_context_sensitive_popup = (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

static bool find_context_sensitive_popup(void)
{
#if HAVE_SCHEME
  return(XEN_DEFINED_P("fft-popup-menu"));
#endif
#if HAVE_RUBY
  /* XEN_DEFINED_P always returns false in Xen/Ruby? */
  return(strcmp(XEN_AS_STRING(XEN_EVAL_C_STRING("defined? Snd_popup_menu")), "constant") == 0);
  /* it returns "nil" in the undefined case */
#endif
}

static void reflect_context_sensitive_popup(prefs_info *prf) 
{
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle), find_context_sensitive_popup());
}

/* ---------------- effects menu ---------------- */

static bool include_effects_menu = false;

static void save_effects_menu(prefs_info *prf, FILE *fd)
{
  if (include_effects_menu)
    {
#if HAVE_SCHEME
      fprintf(fd, "(if (not (provided? 'snd-new-effects.scm)) (load-from-path \"new-effects.scm\"))\n");
#endif
#if HAVE_RUBY
      fprintf(fd, "require \"effects\"\n");
#endif
    }
}

static void effects_menu_toggle(prefs_info *prf)
{
  include_effects_menu = (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
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
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle), find_effects_menu());
}

#if HAVE_GUILE
/* ---------------- edit menu ---------------- */

static bool include_edit_menu = false;

static void save_edit_menu(prefs_info *prf, FILE *fd)
{
  if (include_edit_menu)
    fprintf(fd, "(if (not (provided? 'snd-edit-menu.scm)) (load-from-path \"edit-menu.scm\"))\n");
}

static void edit_menu_toggle(prefs_info *prf)
{
  include_edit_menu = (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

static bool find_edit_menu(void)
{
  return(XEN_DEFINED_P("make-stereofile")); /* a kludge... currently this is only defined in edit-menu.scm */
}

static void reflect_edit_menu(prefs_info *prf) 
{
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle), find_edit_menu());
}
#endif


/* ---------------- graph-style ---------------- */

static const char *graph_styles[5] = {"line", "dot", "filled", "dot+line", "lollipop"};

static void reflect_graph_style(prefs_info *prf)
{
  GtkWidget *w;
#if 0
  w = find_radio_button(prf, graph_style(ss));
  if (w)
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(w), XmSET);
  else fprintf(stderr, "can't find %s\n", graph_styles[graph_style(ss)]);
  if ((prf->radio_button) &&
      (XmIsToggleButton(prf->radio_button)) &&
      (w != prf->radio_button))
    {
      /* motif docs are incorrect -- the set above does not unset the currently set radio button */
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->radio_button), XmUNSET);
      prf->radio_button = w;
    }
#endif
}

static void graph_style_choice(prefs_info *prf)
{
#if 0
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
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
#endif
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
  gtk_widget_set_sensitive(prf->arrow_up, dot_size(ss) < MAX_DOT_SIZE);
  gtk_widget_set_sensitive(prf->arrow_down, dot_size(ss) > MIN_DOT_SIZE);
}

static void dot_size_up(prefs_info *prf)
{
  int size;
  size = dot_size(ss) + 1;
  if (size >= MAX_DOT_SIZE) gtk_widget_set_sensitive(prf->arrow_up, false);
  if (size > MIN_DOT_SIZE) gtk_widget_set_sensitive(prf->arrow_down, true);
  in_set_dot_size(size);
  show_dot_size(prf);
}

static void dot_size_down(prefs_info *prf)
{
  int size;
  size = dot_size(ss) - 1;
  if (size <= MIN_DOT_SIZE) gtk_widget_set_sensitive(prf->arrow_down, false);
  if (size < MAX_DOT_SIZE) gtk_widget_set_sensitive(prf->arrow_up, true);
  in_set_dot_size(size);
  show_dot_size(prf);
}

static void dot_size_from_text(prefs_info *prf)
{
  int size;
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      prf->got_error = false;
      redirect_errors_to(post_prefs_error, (void *)prf);
      size = string_to_int(str, 0, "dot size"); 
      redirect_errors_to(NULL, NULL);

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


/* ---------------- initial bounds ---------------- */

/* TODO: initial bounds callbacks */
/* (add-hook! after-open-hook (lambda (snd)...) */
/* (set! (x-bounds) (list 0.0 (/ (frames) (srate)))) */

/* this could be a function that if defined could also return bounds if arg is :bounds -> #t if full */
/* then subsequent runs get the current bounds */


static void reflect_initial_bounds(prefs_info *prf)
{
}
static void save_initial_bounds(prefs_info *prf, FILE *fd)
{
}
static void initial_bounds_toggle(prefs_info *prf)
{
}
static void initial_bounds_text(prefs_info *prf)
{
}


/* ---------------- channel-style ---------------- */

static const char *channel_styles[3] = {"separate", "combined", "superimposed"};

static void reflect_channel_style(prefs_info *prf)
{
  GtkWidget *w;
#if 0
  w = find_radio_button(prf, (int)channel_style(ss));
  if (w)
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(w), XmSET);
  else fprintf(stderr, "can't find %s\n", channel_styles[(int)channel_style(ss)]);
  if ((prf->radio_button) &&
      (XmIsToggleButton(prf->radio_button)) &&
      (w != prf->radio_button))
    {
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->radio_button), XmUNSET);
      prf->radio_button = w;
    }
#endif
}

static void channel_style_choice(prefs_info *prf)
{
#if 0
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
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
#endif
}

/* ---------------- graphs-horizontal ---------------- */

static void reflect_graphs_horizontal(prefs_info *prf) 
{
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle), graphs_horizontal(ss));
}

static void graphs_horizontal_toggle(prefs_info *prf)
{
  in_set_graphs_horizontal(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

/* ---------------- show-y-zero ---------------- */

static void reflect_show_y_zero(prefs_info *prf) 
{
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle), show_y_zero(ss));
}

static void y_zero_toggle(prefs_info *prf)
{
  in_set_show_y_zero(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

/* ---------------- show-grid ---------------- */

static void reflect_show_grid(prefs_info *prf) 
{
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle), show_grid(ss));
}

static void grid_toggle(prefs_info *prf)
{
  in_set_show_grid((gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle))) ? WITH_GRID : NO_GRID);
}

/* ---------------- grid-density ---------------- */

static void reflect_grid_density(prefs_info *prf)
{
#if 0
  XmScaleSetValue(prf->scale, (int)(100 * grid_density(ss) / prf->scale_max));
#endif
  float_to_textfield(prf->text, grid_density(ss));
}

static void grid_density_scale_callback(prefs_info *prf)
{
  int val = 0;
#if 0
  XmScaleGetValue(prf->scale, &val);
#endif
  in_set_grid_density(val * prf->scale_max / 100.0);
}

static void grid_density_text_callback(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      float value = 0.0;
      sscanf(str, "%f", &value);
      if ((value >= 0.0) &&
	  (value <= prf->scale_max))
	{
	  in_set_grid_density(value);
#if 0
	  XmScaleSetValue(prf->scale, (int)(100 * value / prf->scale_max));
#endif
	}
      else gtk_entry_set_text(GTK_ENTRY(prf->text), "right");

    }
}

/* ---------------- show-axes ---------------- */

static const char *show_axes_choices[5] = {"none", "X and Y", "just X", "X and Y unlabelled", "just X unlabelled"};

static void reflect_show_axes(prefs_info *prf)
{
  gtk_entry_set_text(GTK_ENTRY(prf->text), (char *)show_axes_choices[(int)show_axes(ss)]);
}

static void show_axes_from_menu(prefs_info *prf, char *value)
{
  int i;
  for (i = 0; i < 5; i++)
    if (strcmp(value, show_axes_choices[i]) == 0)
      {
	in_set_show_axes((show_axes_t)i);
	gtk_entry_set_text(GTK_ENTRY(prf->text), value);
	return;
      }
}

static void show_axes_from_text(prefs_info *prf)
{
  int i;
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      char *trimmed_str;
      trimmed_str = trim_string(str);

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
	  else post_prefs_error("what?", (void *)prf);
	}
      else post_prefs_error("right", (void *)prf);
      FREE(trimmed_str);
    }
  else post_prefs_error("right", (void *)prf);
}

/* ---------------- x-axis-style ---------------- */

static const char *x_axis_styles[5] = {"seconds", "samples", "% of total", "beats", "measures"};

static void reflect_x_axis_style(prefs_info *prf)
{
  gtk_entry_set_text(GTK_ENTRY(prf->text), (char *)x_axis_styles[(int)x_axis_style(ss)]);
}

static void x_axis_style_from_menu(prefs_info *prf, char *value)
{
  int i;
  for (i = 0; i < 5; i++)
    if (strcmp(value, x_axis_styles[i]) == 0)
      {
	in_set_x_axis_style((x_axis_style_t)i);
	gtk_entry_set_text(GTK_ENTRY(prf->text), value);
	return;
      }
}

static void x_axis_style_from_text(prefs_info *prf)
{
  int i;
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      char *trimmed_str;
      trimmed_str = trim_string(str);

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
	  else post_prefs_error("what?", (void *)prf);
	}
      else post_prefs_error("right", (void *)prf);
      FREE(trimmed_str);
    }
  else post_prefs_error("right", (void *)prf);
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
}

static void reflect_smpte(prefs_info *prf) 
{
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle), find_smpte());
}

static void smpte_toggle(prefs_info *prf)
{
  include_smpte = (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

static void save_smpte(prefs_info *prf, FILE *fd)
{
  if (include_smpte)
    {
#if HAVE_SCHEME
      fprintf(fd, "(if (not (provided? 'snd-motif.scm)) (load-from-path \"snd-motif.scm\"))\n");
      fprintf(fd, "(show-smpte-label #t)\n");
#endif
#if HAVE_RUBY
#endif
    }
}


/* ---------------- data-color ---------------- */

static color_t saved_data_color;

static void reflect_data_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_data_color); 
  set_data_color(saved_data_color);
}

static void data_color_func(prefs_info *prf, float r, float g, float b)
{
  GdkColor *tmp;
  tmp = rgb_to_color(r, g, b);
  set_data_color(tmp);
  FREE(tmp);
}

/* ---------------- graph-color ---------------- */

static color_t saved_graph_color;

static void reflect_graph_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_graph_color); 
  set_graph_color(saved_graph_color);
}

static void graph_color_func(prefs_info *prf, float r, float g, float b)
{
  GdkColor *tmp;
  tmp = rgb_to_color(r, g, b);
  set_graph_color(tmp);
  FREE(tmp);
}

/* ---------------- selected-data-color ---------------- */

static color_t saved_selected_data_color;

static void reflect_selected_data_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_selected_data_color); 
  set_selected_data_color(saved_selected_data_color);
}

static void selected_data_color_func(prefs_info *prf, float r, float g, float b)
{
  GdkColor *tmp;
  tmp = rgb_to_color(r, g, b);
  set_selected_data_color(tmp);
  FREE(tmp);
}

/* ---------------- selected-graph-color ---------------- */

static color_t saved_selected_graph_color;

static void reflect_selected_graph_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_selected_graph_color); 
  set_selected_graph_color(saved_selected_graph_color);
}

static void selected_graph_color_func(prefs_info *prf, float r, float g, float b)
{
  GdkColor *tmp;
  tmp = rgb_to_color(r, g, b);
  set_selected_graph_color(tmp);
  FREE(tmp);
}

/* ---------------- selection-color ---------------- */

static void set_selection_color(color_t color)
{
  color_selection(color);
  for_each_chan(update_graph);
}

static color_t saved_selection_color;

static void reflect_selection_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_selection_color); 
  set_selection_color(saved_selection_color);
}

static void selection_color_func(prefs_info *prf, float r, float g, float b)
{
  GdkColor *tmp;
  tmp = rgb_to_color(r, g, b);
  set_selection_color(tmp);
  FREE(tmp);
}


/* ---------------- axis-label-font ---------------- */

static gint axis_label_font_error_erase_func(gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  gtk_entry_set_text(GTK_ENTRY(prf->text), axis_label_font(ss));
  return(0);
}

static void reflect_axis_label_font(prefs_info *prf)
{
  gtk_entry_set_text(GTK_ENTRY(prf->text), axis_label_font(ss));
}

static void axis_label_font_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((!str) || (!(*str)))
    {
      gtk_entry_set_text(GTK_ENTRY(prf->text), axis_label_font(ss));
      return;
    }
  if (!(set_axis_label_font(str)))
    {
      gtk_entry_set_text(GTK_ENTRY(prf->text), "can't find that font");
      g_timeout_add_full(0,
		      ERROR_WAIT_TIME,
		      axis_label_font_error_erase_func,
			 (gpointer)prf, NULL);
    }

}

/* ---------------- axis-numbers-font ---------------- */

static gint axis_numbers_font_error_erase_func(gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  gtk_entry_set_text(GTK_ENTRY(prf->text), axis_numbers_font(ss));
  return(0);
}

static void reflect_axis_numbers_font(prefs_info *prf)
{
  gtk_entry_set_text(GTK_ENTRY(prf->text), axis_numbers_font(ss));
}

static void axis_numbers_font_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((!str) || (!(*str)))
    {
      gtk_entry_set_text(GTK_ENTRY(prf->text), axis_numbers_font(ss));
      return;
    }
  if (!(set_axis_numbers_font(str)))
    {
      gtk_entry_set_text(GTK_ENTRY(prf->text), "can't find that font");
      g_timeout_add_full(0,
		      ERROR_WAIT_TIME,
		      axis_numbers_font_error_erase_func,
			 (gpointer)prf, NULL);
    }

}

/* ---------------- peaks-font ---------------- */

static gint peaks_font_error_erase_func(gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  gtk_entry_set_text(GTK_ENTRY(prf->text), peaks_font(ss));
  return(0);
}

static void reflect_peaks_font(prefs_info *prf)
{
  gtk_entry_set_text(GTK_ENTRY(prf->text), peaks_font(ss));
}

static void peaks_font_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((!str) || (!(*str)))
    {
      gtk_entry_set_text(GTK_ENTRY(prf->text), peaks_font(ss));
      return;
    }
  if (!(set_peaks_font(str)))
    {
      gtk_entry_set_text(GTK_ENTRY(prf->text), "can't find that font");
      g_timeout_add_full(0,
		      ERROR_WAIT_TIME,
		      peaks_font_error_erase_func,
			 (gpointer)prf, NULL);
    }

}

/* ---------------- bold-peaks-font ---------------- */

static gint bold_peaks_font_error_erase_func(gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  gtk_entry_set_text(GTK_ENTRY(prf->text), bold_peaks_font(ss));
  return(0);
}

static void reflect_bold_peaks_font(prefs_info *prf)
{
  gtk_entry_set_text(GTK_ENTRY(prf->text), bold_peaks_font(ss));
}

static void bold_peaks_font_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((!str) || (!(*str)))
    {
      gtk_entry_set_text(GTK_ENTRY(prf->text), bold_peaks_font(ss));
      return;
    }
  if (!(set_bold_peaks_font(str)))
    {
      gtk_entry_set_text(GTK_ENTRY(prf->text), "can't find that font");
      g_timeout_add_full(0,
		      ERROR_WAIT_TIME,
		      bold_peaks_font_error_erase_func,
			 (gpointer)prf, NULL);
    }
}

/* ---------------- tiny-font ---------------- */

static gint tiny_font_error_erase_func(gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  gtk_entry_set_text(GTK_ENTRY(prf->text), tiny_font(ss));
  return(0);
}

static void reflect_tiny_font(prefs_info *prf)
{
  gtk_entry_set_text(GTK_ENTRY(prf->text), tiny_font(ss));
}

static void tiny_font_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((!str) || (!(*str)))
    {
      gtk_entry_set_text(GTK_ENTRY(prf->text), tiny_font(ss));
      return;
    }
  if (!(set_tiny_font(str)))
    {
      gtk_entry_set_text(GTK_ENTRY(prf->text), "can't find that font");
      g_timeout_add_full(0,
		      ERROR_WAIT_TIME,
		      tiny_font_error_erase_func,
			 (gpointer)prf, NULL);
    }

}



/* ---------------- fft-size ---------------- */

#define MAX_TRANSFORM_SIZE 1073741824
#define MIN_TRANSFORM_SIZE 2

static void fft_size_to_text(prefs_info *prf)
{
  char *new_size;
  new_size = mus_format(OFF_TD, transform_size(ss));
  gtk_entry_set_text(GTK_ENTRY(prf->text), new_size);
  FREE(new_size);
}

static void reflect_fft_size(prefs_info *prf)
{
  fft_size_to_text(prf);
  gtk_widget_set_sensitive(prf->arrow_up, transform_size(ss) < MAX_TRANSFORM_SIZE);
  gtk_widget_set_sensitive(prf->arrow_down, transform_size(ss) > MIN_TRANSFORM_SIZE);
}

static void fft_size_up(prefs_info *prf)
{
  off_t size;
  size = transform_size(ss) * 2;
  if (size >= MAX_TRANSFORM_SIZE) gtk_widget_set_sensitive(prf->arrow_up, false);
  if (size > MIN_TRANSFORM_SIZE) gtk_widget_set_sensitive(prf->arrow_down, true);
  in_set_transform_size(size);
  fft_size_to_text(prf);
}

static void fft_size_down(prefs_info *prf)
{
  off_t size;
  size = transform_size(ss) / 2;
  if (size <= MIN_TRANSFORM_SIZE) gtk_widget_set_sensitive(prf->arrow_down, false);
  if (size < MAX_TRANSFORM_SIZE) gtk_widget_set_sensitive(prf->arrow_up, true);
  in_set_transform_size(size);
  fft_size_to_text(prf);
}

static void fft_size_from_text(prefs_info *prf)
{
  off_t size;
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      prf->got_error = false;
      redirect_errors_to(post_prefs_error, (void *)prf);
      size = string_to_off_t(str, MIN_TRANSFORM_SIZE, "size"); 
      redirect_errors_to(NULL, NULL);

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
  GtkWidget *w;
#if 0
  w = find_radio_button(prf, transform_graph_type(ss));
  if (w)
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(w), XmSET);
  else fprintf(stderr, "can't find %s\n", transform_graph_types[transform_graph_type(ss)]);
  if ((prf->radio_button) &&
      (XmIsToggleButton(prf->radio_button)) &&
      (w != prf->radio_button))
    {
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->radio_button), XmUNSET);
      prf->radio_button = w;
    }
#endif
}

static void transform_graph_type_choice(prefs_info *prf)
{
#if 0
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
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
#endif
}


/* ---------------- transform-type ---------------- */

#define NUM_TRANSFORM_TYPES 6

static const char *transform_types[NUM_TRANSFORM_TYPES] = {"Fourier", "Wavelet", "Walsh", "Autocorrelate", "Cepstrum", "Haar"};

static list_completer_info *transform_type_completer_info = NULL;

static void reflect_transform_type(prefs_info *prf)
{
  gtk_entry_set_text(GTK_ENTRY(prf->text), (char *)transform_types[mus_iclamp(0, transform_type(ss), NUM_TRANSFORM_TYPES - 1)]);
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
	gtk_entry_set_text(GTK_ENTRY(prf->text), value);
	return;
      }
}

static void transform_type_from_text(prefs_info *prf)
{
  int i;
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      char *trimmed_str;
      trimmed_str = trim_string(str);

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
  gtk_entry_set_text(GTK_ENTRY(prf->text), (char *)fft_windows[(int)fft_window(ss)]);
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
	gtk_entry_set_text(GTK_ENTRY(prf->text), value);
	return;
      }
}

static void fft_window_from_text(prefs_info *prf)
{
  int i;
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      char *trimmed_str;
      trimmed_str = trim_string(str);

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
#if 0
  XmScaleSetValue(prf->scale, (int)(100 * fft_window_beta(ss) / prf->scale_max));
#endif
  float_to_textfield(prf->text, fft_window_beta(ss));
}

static void fft_window_beta_scale_callback(prefs_info *prf)
{
  int val = 0;
#if 0
  XmScaleGetValue(prf->scale, &val);
#endif
  in_set_fft_window_beta(val * prf->scale_max / 100.0);
}

static void fft_window_beta_text_callback(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      float value = 0.0;
      sscanf(str, "%f", &value);
      if ((value >= 0.0) &&
	  (value <= prf->scale_max))
	{
	  in_set_fft_window_beta(value);
#if 0
	  XmScaleSetValue(prf->scale, (int)(100 * value / prf->scale_max));
#endif
	}
      else gtk_entry_set_text(GTK_ENTRY(prf->text), "right");

    }
}


/* ---------------- show-transform-peaks ---------------- */

static void reflect_show_transform_peaks(prefs_info *prf) 
{
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle), show_transform_peaks(ss));
}

static void transform_peaks_toggle(prefs_info *prf)
{
  in_set_show_transform_peaks(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

static void transform_peaks_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      int value = 0;
      sscanf(str, "%d", &value);
      if (value >= 0)
	in_set_max_transform_peaks(value);
      else
	{

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
  gtk_entry_set_text(GTK_ENTRY(prf->text), colormap_name(color_map(ss)));
}

static void colormap_from_text(prefs_info *prf)
{
  int i;
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      char *trimmed_str;
      trimmed_str = trim_string(str);

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
	gtk_entry_set_text(GTK_ENTRY(prf->text), value);
	return;
      }
}


/* ---------------- fft-log-magnitude ---------------- */

static void reflect_fft_log_magnitude(prefs_info *prf) 
{
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle), fft_log_magnitude(ss));
}

static void log_magnitude_toggle(prefs_info *prf)
{
  in_set_fft_log_magnitude(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

/* ---------------- min-dB ---------------- */

static void reflect_min_dB(prefs_info *prf)
{
  float_to_textfield(prf->text, min_dB(ss));
}

static void min_dB_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
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
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle), fft_log_frequency(ss));
}

static void log_frequency_toggle(prefs_info *prf)
{
  in_set_fft_log_frequency(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

/* ---------------- transform-normalization ---------------- */

static const char *transform_normalizations[4] = {"none", "by channel", "by sound", "global"};

static void reflect_transform_normalization(prefs_info *prf)
{
  GtkWidget *w;
#if 0
  w = find_radio_button(prf, transform_normalization(ss));
  if (w)
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(w), XmSET);
  else fprintf(stderr, "can't find %s\n", transform_normalizations[transform_normalization(ss)]);
  if ((prf->radio_button) &&
      (XmIsToggleButton(prf->radio_button)) &&
      (w != prf->radio_button))
    {
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->radio_button), XmUNSET);
      prf->radio_button = w;
    }
#endif
}

static void transform_normalization_choice(prefs_info *prf)
{
#if 0
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
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
#endif
}


/* ---------------- mark-color ---------------- */

static color_t saved_mark_color;

static void reflect_mark_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_mark_color);
  color_marks(saved_mark_color);
}

static void mark_color_func(prefs_info *prf, float r, float g, float b)
{
  GdkColor *tmp;
  tmp = rgb_to_color(r, g, b);
  color_marks(tmp);
  FREE(tmp);
}

/* ---------------- mark-tag size ---------------- */

static void reflect_mark_tag_size(prefs_info *prf)
{
  int_to_textfield(prf->text, mark_tag_width(ss));
  int_to_textfield(prf->rtxt, mark_tag_height(ss));
}

static gint mark_tag_width_erase_func(gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->text, mark_tag_width(ss));
  return(0);
}

static gint mark_tag_height_erase_func(gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->rtxt, mark_tag_height(ss));
  return(0);
}

static void mark_tag_width_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  gtk_entry_set_text(GTK_ENTRY(prf->text), "right");
  g_timeout_add_full(0,
		  ERROR_WAIT_TIME,
		  mark_tag_width_erase_func,
		     (gpointer)prf, NULL);
}

static void mark_tag_height_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  gtk_entry_set_text(GTK_ENTRY(prf->rtxt), "right");
  g_timeout_add_full(0,
		  ERROR_WAIT_TIME,
		  mark_tag_height_erase_func,
		     (gpointer)prf, NULL);
}

static void mark_tag_size_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      int width = 0;
      redirect_errors_to(mark_tag_width_error, (void *)prf);
      width = string_to_int(str, 1, "mark tag width");
      redirect_errors_to(NULL, NULL);
      if (width > 0) set_mark_tag_width(width);

      str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->rtxt));
      if ((str) && (*str))
	{
	  int height;
	  redirect_errors_to(mark_tag_height_error, (void *)prf);
	  height = string_to_int(str, 1, "mark tag height");
	  redirect_errors_to(NULL, NULL);
	  if (height > 0) set_mark_tag_height(height);

	}
    }
}


/* ---------------- mix-color (waveform) ---------------- */

static color_t saved_mix_color;

static void reflect_mix_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_mix_color);
  color_mixes(saved_mix_color);
}

static void mix_color_func(prefs_info *prf, float r, float g, float b)
{
  GdkColor *tmp;
  tmp = rgb_to_color(r, g, b);
  color_mixes(tmp);
  FREE(tmp);
}

/* ---------------- mix-tag size ---------------- */

static void reflect_mix_tag_size(prefs_info *prf)
{
  int_to_textfield(prf->text, mix_tag_width(ss));
  int_to_textfield(prf->rtxt, mix_tag_height(ss));
}

static gint mix_tag_width_erase_func(gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->text, mix_tag_width(ss));
  return(0);
}

static gint mix_tag_height_erase_func(gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->rtxt, mix_tag_height(ss));
  return(0);
}

static void mix_tag_width_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  gtk_entry_set_text(GTK_ENTRY(prf->text), "right");
  g_timeout_add_full(0,
		  ERROR_WAIT_TIME,
		  mix_tag_width_erase_func,
		     (gpointer)prf, NULL);
}

static void mix_tag_height_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  gtk_entry_set_text(GTK_ENTRY(prf->rtxt), "right");
  g_timeout_add_full(0,
		  ERROR_WAIT_TIME,
		  mix_tag_height_erase_func,
		     (gpointer)prf, NULL);
}

static void mix_tag_size_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      int width = 0;
      redirect_errors_to(mix_tag_width_error, (void *)prf);
      width = string_to_int(str, 1, "mix tag width");
      redirect_errors_to(NULL, NULL);
      if (width > 0) set_mix_tag_width(width);

      str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->rtxt));
      if ((str) && (*str))
	{
	  int height;
	  redirect_errors_to(mix_tag_height_error, (void *)prf);
	  height = string_to_int(str, 1, "mix tag height");
	  redirect_errors_to(NULL, NULL);
	  if (height > 0) set_mix_tag_height(height);

	}
    }
}

/* ---------------- show-mix-waveforms ---------------- */

static void reflect_show_mix_waveforms(prefs_info *prf) 
{
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle), show_mix_waveforms(ss));
}

static void show_mix_waveforms_toggle(prefs_info *prf)
{
  in_set_show_mix_waveforms(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

static void mix_waveform_height_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      int value = 0;
      sscanf(str, "%d", &value);
      if (value >= 0)
	in_set_mix_waveform_height(value);
      else
	{

	  int_to_textfield(prf->text, mix_waveform_height(ss));
	}
    }
}


/* ---------------- show-listener ---------------- */

static bool include_listener = false;

static void reflect_show_listener(prefs_info *prf) 
{
  include_listener = listener_is_visible();
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle), include_listener);
}

static void show_listener_toggle(prefs_info *prf)
{
  handle_listener(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
  include_listener = true;
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
  gtk_widget_set_sensitive(prf->arrow_up, optimization(ss) < MAX_OPTIMIZATION);
  gtk_widget_set_sensitive(prf->arrow_down, optimization(ss) > MIN_OPTIMIZATION);
}

static void optimization_up(prefs_info *prf)
{
  int val;
  val = optimization(ss) + 1;
  if (val >= MAX_OPTIMIZATION) gtk_widget_set_sensitive(prf->arrow_up, false);
  if (val > MIN_OPTIMIZATION) gtk_widget_set_sensitive(prf->arrow_down, true);
  set_optimization(val);
  show_opt(prf);
}

static void optimization_down(prefs_info *prf)
{
  int val;
  val = optimization(ss) - 1;
  if (val <= MIN_OPTIMIZATION) gtk_widget_set_sensitive(prf->arrow_down, false);
  if (val < MAX_OPTIMIZATION) gtk_widget_set_sensitive(prf->arrow_up, true);
  set_optimization(val);
  show_opt(prf);
}

static void optimization_from_text(prefs_info *prf)
{
  int opt;
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      prf->got_error = false;
      redirect_errors_to(post_prefs_error, (void *)prf);
      opt = string_to_int(str, MIN_OPTIMIZATION, "optimization"); 
      redirect_errors_to(NULL, NULL);

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
  gtk_entry_set_text(GTK_ENTRY(prf->text), listener_prompt(ss));
}

static void listener_prompt_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if (str)
    {
      if (listener_prompt(ss)) FREE(listener_prompt(ss));
      set_listener_prompt(copy_string(str));

    }
}

/* ---------------- show-backtrace ---------------- */

static void reflect_show_backtrace(prefs_info *prf) 
{
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle), show_backtrace(ss));
}

static void show_backtrace_toggle(prefs_info *prf)
{
  set_show_backtrace(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

/* ---------------- listener-color ---------------- */

static color_t saved_listener_color;

static void reflect_listener_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_listener_color);
  color_listener(saved_listener_color);
}

static void listener_color_func(prefs_info *prf, float r, float g, float b)
{
  GdkColor *tmp;
  tmp = rgb_to_color(r, g, b);
  color_listener(tmp);
  FREE(tmp);
}

/* ---------------- listener-text-color ---------------- */

static color_t saved_listener_text_color;

static void reflect_listener_text_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_listener_text_color);
  color_listener_text(saved_listener_text_color);
}

static void listener_text_color_func(prefs_info *prf, float r, float g, float b)
{
  GdkColor *tmp;
  tmp = rgb_to_color(r, g, b);
  color_listener_text(tmp);
  FREE(tmp);
}

/* ---------------- listener-font ---------------- */

static gint listener_font_error_erase_func(gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  gtk_entry_set_text(GTK_ENTRY(prf->text), listener_font(ss));
  return(0);
}

static void reflect_listener_font(prefs_info *prf)
{
  gtk_entry_set_text(GTK_ENTRY(prf->text), listener_font(ss));
}

static void listener_font_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((!str) || (!(*str)))
    {
      gtk_entry_set_text(GTK_ENTRY(prf->text), listener_font(ss));
      return;
    }
  if (!(set_listener_font(str)))
    {
      gtk_entry_set_text(GTK_ENTRY(prf->text), "can't find that font");
      g_timeout_add_full(0,
		      ERROR_WAIT_TIME,
		      listener_font_error_erase_func,
			 (gpointer)prf, NULL);
    }

}


/* ---------------- help-button-color ---------------- */

static color_t saved_help_button_color;

static void reflect_help_button_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_help_button_color); 
  ss->sgx->help_button_color = saved_help_button_color;
}

static void help_button_color_func(prefs_info *prf, float r, float g, float b)
{
  GdkColor *tmp;
  tmp = rgb_to_color(r, g, b);
  ss->sgx->help_button_color = tmp;
  FREE(tmp);
}

/* ---------------- quit-button-color ---------------- */

static color_t saved_quit_button_color;

static void reflect_quit_button_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_quit_button_color); 
  ss->sgx->quit_button_color = saved_quit_button_color;
}

static void quit_button_color_func(prefs_info *prf, float r, float g, float b)
{
  GdkColor *tmp;
  tmp = rgb_to_color(r, g, b);
  ss->sgx->quit_button_color = tmp;
  FREE(tmp);
}

/* ---------------- reset-button-color ---------------- */

static color_t saved_reset_button_color;

static void reflect_reset_button_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_reset_button_color); 
  ss->sgx->reset_button_color = saved_reset_button_color;
}

static void reset_button_color_func(prefs_info *prf, float r, float g, float b)
{
  GdkColor *tmp;
  tmp = rgb_to_color(r, g, b);
  ss->sgx->reset_button_color = tmp;
  FREE(tmp);
}

/* ---------------- doit-button-color ---------------- */

static color_t saved_doit_button_color;

static void reflect_doit_button_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_doit_button_color); 
  ss->sgx->doit_button_color = saved_doit_button_color;
}

static void doit_button_color_func(prefs_info *prf, float r, float g, float b)
{
  GdkColor *tmp;
  tmp = rgb_to_color(r, g, b);
  ss->sgx->doit_button_color = tmp;
  FREE(tmp);
}

/* ---------------- doit-again-button-color ---------------- */

static color_t saved_doit_again_button_color;

static void reflect_doit_again_button_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_doit_again_button_color); 
  ss->sgx->doit_again_button_color = saved_doit_again_button_color;
}

static void doit_again_button_color_func(prefs_info *prf, float r, float g, float b)
{
  GdkColor *tmp;
  tmp = rgb_to_color(r, g, b);
  ss->sgx->doit_again_button_color = tmp;
  FREE(tmp);
}

/* ---------------- pushed-button-color ---------------- */

static color_t saved_pushed_button_color;

static void reflect_pushed_button_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_pushed_button_color); 
  ss->sgx->pushed_button_color = saved_pushed_button_color;
}

static void pushed_button_color_func(prefs_info *prf, float r, float g, float b)
{
  GdkColor *tmp;
  tmp = rgb_to_color(r, g, b);
  ss->sgx->pushed_button_color = tmp;
  FREE(tmp);
}




/* ---------------- preferences dialog ---------------- */


void start_preferences_dialog(void)
{
  GtkWidget *saveB, *resetB, *helpB, *dismissB, *topics, *scroller, *current_sep;
  prefs_info *prf;
  char *str;

  if (preferences_dialog)
    {
      gtk_widget_show(preferences_dialog);
      return;
    }

  preferences_dialog = snd_gtk_dialog_new();
  gtk_window_set_title(GTK_WINDOW(preferences_dialog), _("Preferences"));
  sg_make_resizable(preferences_dialog);
  /* gtk_container_set_border_width (GTK_CONTAINER(preferences_dialog), 10); */
  /* gtk_window_resize(GTK_WINDOW(preferences_dialog), 400, 200); */
  gtk_widget_realize(preferences_dialog);

  helpB = gtk_button_new_from_stock(GTK_STOCK_HELP);
  gtk_widget_set_name(helpB, "help_button");

  saveB = gtk_button_new_from_stock(GTK_STOCK_SAVE);
  gtk_widget_set_name(saveB, "doit_button");

  resetB = gtk_button_new_from_stock(GTK_STOCK_CLEAR);
  gtk_widget_set_name(resetB, "reset_button");

  dismissB = gtk_button_new_from_stock(GTK_STOCK_QUIT);
  gtk_widget_set_name(dismissB, "quit_button");

  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(preferences_dialog)->action_area), dismissB, true, true, 10);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(preferences_dialog)->action_area), resetB, true, true, 10);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(preferences_dialog)->action_area), saveB, true, true, 10);
  gtk_box_pack_end(GTK_BOX(GTK_DIALOG(preferences_dialog)->action_area), helpB, true, true, 10);

  SG_SIGNAL_CONNECT(preferences_dialog, "delete_event", preferences_delete_callback, NULL);
  SG_SIGNAL_CONNECT(dismissB, "clicked", preferences_dismiss_callback, NULL);
  SG_SIGNAL_CONNECT(resetB, "clicked", preferences_reset_callback, NULL);
  SG_SIGNAL_CONNECT(saveB, "clicked", preferences_save_callback, NULL);
  SG_SIGNAL_CONNECT(helpB, "clicked", preferences_help_callback, NULL);

  gtk_widget_show(dismissB);
  gtk_widget_show(saveB);
  gtk_widget_show(resetB);
  gtk_widget_show(helpB);

  topics = gtk_vbox_new(false, 0);

  scroller = gtk_scrolled_window_new(NULL, NULL);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(preferences_dialog)->vbox), scroller, true, true, 0);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroller), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scroller), topics);

  gtk_widget_show(topics);
  gtk_widget_show(scroller);



  /* ---------------- overall behavior ---------------- */

  {
    GtkWidget *dpy_box, *dpy_label, *file_label, *cursor_label;
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
    prf = prefs_row_with_toggle("selection creates an associated region", S_selection_creates_region,
				selection_creates_region(ss),
				dpy_box, current_sep,
				selection_creates_region_toggle);
    remember_pref(prf, reflect_selection_creates_region, NULL);


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
    current_sep = make_inter_variable_separator(dpy_box, prf->label);

#if HAVE_GUILE
    prf = prefs_row_with_toggle("edit menu additions", "edit-menu.scm", /* TODO help index */
				find_edit_menu(),
				dpy_box, current_sep, 
				edit_menu_toggle);
    remember_pref(prf, reflect_edit_menu, save_edit_menu);
    current_sep = make_inter_variable_separator(dpy_box, prf->label);
#endif

#if 0
    /* TODO: untangle marks-menu */

    /* currently forces effects menu to load (add-sliders code) */
    prf = prefs_row_with_toggle("marks menu", "marks-menu.scm",
				find_marks_menu(),
				dpy_box, current_sep, 
				marks_menu_toggle);
    remember_pref(prf, reflect_marks_menu, save_marks_menu);
    current_sep = make_inter_variable_separator(dpy_box, prf->label);

#endif

    /* ---------------- cursor options ---------------- */

    cursor_label = make_inner_label("  cursor options", dpy_box, current_sep);

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
    if (cursor_size(ss) <= 0) gtk_widget_set_sensitive(prf->arrow_down, false);

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
    GtkWidget *grf_box, *grf_label, *colgrf_label;

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
    if (dot_size(ss) <= 0) gtk_widget_set_sensitive(prf->arrow_down, false);

    current_sep = make_inter_variable_separator(grf_box, prf->label);
    prf = prefs_row_with_text_with_toggle("initial graph x bounds", S_x_bounds, false, /* TODO: how to find this?? */
					  "show full duration", "0.0 : 0.1", 12,
					  grf_box, current_sep,
					  initial_bounds_toggle,
					  initial_bounds_text);
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
    GtkWidget *aud_box, *aud_label;

    aud_box = make_top_level_box(topics);
    aud_label = make_top_level_label("audio options", aud_box);
    
  }

  current_sep = make_inter_topic_separator(topics);
#endif

  /* -------- transform -------- */
  {
    GtkWidget *fft_box, *fft_label;

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
    if (transform_size(ss) <= 2) gtk_widget_set_sensitive(prf->arrow_down, false);

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
  }

  current_sep = make_inter_topic_separator(topics);

  /* -------- marks, mixes, and regions -------- */
  {
    GtkWidget *mmr_box, *mmr_label;
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

  /* -------- programming -------- */
  {
    GtkWidget *prg_box, *prg_label;

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
    if (optimization(ss) == 6) gtk_widget_set_sensitive(prf->arrow_up, false);
    if (optimization(ss) == 0) gtk_widget_set_sensitive(prf->arrow_down, false);
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

  /* -------- silly stuff -------- */
  {
    GtkWidget *silly_box, *silly_label;

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

  set_dialog_widget(PREFERENCES_DIALOG, preferences_dialog);
  gtk_widget_show(preferences_dialog);
}
#else

void start_preferences_dialog(void)
{
}

#endif
