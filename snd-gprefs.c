#include "snd.h"
#include "sndlib-strings.h"

static GtkWidget *preferences_dialog = NULL;

static bool prefs_helping = false, prefs_unsaved = false;
static char *prefs_saved_filename = NULL;
static char *include_load_path = NULL;

#define HELP_WAIT_TIME ((guint32)500)
#define POWER_WAIT_TIME ((guint32)100)
#define POWER_INITIAL_WAIT_TIME ((guint32)500)
#define ERROR_WAIT_TIME ((guint32)1000)

#define STARTUP_WIDTH 925
#define STARTUP_HEIGHT 800

typedef struct prefs_info {
  GtkWidget *label, *text, *arrow_up, *arrow_down, *arrow_right, *error, *toggle, *scale, *toggle2, *toggle3;
  GtkWidget *color, *rscl, *gscl, *bscl, *rtxt, *gtxt, *btxt, *list_menu, *radio_button, *helper;
  GtkObject *adj, *radj, *gadj, *badj;
  GtkWidget **radio_buttons;
  bool got_error;
  guint help_id, power_id, erase_id;
  const char *var_name;
  const char **values;
  int num_values;
  Float scale_max;
  GtkSizeGroup *color_texts, *color_scales;
  void (*toggle_func)(struct prefs_info *prf);
  void (*toggle2_func)(struct prefs_info *prf);
  void (*toggle3_func)(struct prefs_info *prf);
  void (*scale_func)(struct prefs_info *prf);
  void (*arrow_up_func)(struct prefs_info *prf);
  void (*arrow_down_func)(struct prefs_info *prf);
  void (*text_func)(struct prefs_info *prf);
  void (*color_func)(struct prefs_info *prf, float r, float g, float b);
  void (*reflect_func)(struct prefs_info *prf);
  void (*save_func)(struct prefs_info *prf, FILE *fd);
  void (*help_func)(struct prefs_info *prf);
} prefs_info;


static void prefs_set_dialog_title(const char *filename);
static void reflect_key(prefs_info *prf, const char *key_name);
static void save_key_binding(prefs_info *prf, FILE *fd, char *(*binder)(char *key, bool c, bool m, bool x));
static void key_bind(prefs_info *prf, char *(*binder)(char *key, bool c, bool m, bool x));
#include "snd-prefs.c"


/* ---------------- utilities ---------------- */

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

static void float_1_to_textfield(GtkWidget *w, Float val)
{
  char *str;
  str = (char *)CALLOC(12, sizeof(char));
  mus_snprintf(str, 12, "%.1f", val);
  gtk_entry_set_text(GTK_ENTRY(w), str);
  FREE(str);
}

static void sg_entry_set_text(GtkEntry* entry, const char *text)
{
  if (text)
    gtk_entry_set_text(entry, (gchar *)text);
  else gtk_entry_set_text(entry, " ");
}


/* ---------------- help strings ---------------- */

static gboolean prefs_help_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer context)
{
  prefs_help((prefs_info *)context);
  return(false);
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

static gboolean mouse_enter_pref_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if (prefs_helping)
    prf->help_id = g_timeout_add_full(0,
				      HELP_WAIT_TIME,
				      prefs_tooltip_help,
				      context, NULL);
  return(false);
}

static gboolean mouse_leave_pref_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if (prf->help_id != 0)
    {
      g_source_remove(prf->help_id);
      prf->help_id = 0;
    }
  return(false);
}

static bool prefs_dialog_error_is_posted = false;

static void post_prefs_dialog_error(const char *message, void *data)
{
  gtk_window_set_title(GTK_WINDOW(preferences_dialog), (char *)message);
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

static void prefs_change_callback(GtkWidget *w, gpointer context)
{
  prefs_unsaved = true;
  prefs_set_dialog_title(NULL);
  clear_prefs_dialog_error();
}


static GtkSizeGroup *label_group;
static GtkSizeGroup *help_group;
static GtkSizeGroup *widgets_group;

static GdkColor *rscl_color, *gscl_color, *bscl_color;

#define PACK_1 true
#define PACK_2 false


/* ---------------- row (main) label widget ---------------- */

static GtkWidget *make_row_label(prefs_info *prf, const char *label, GtkWidget *box)
{
  GtkWidget *w, *ev;

  ev = gtk_event_box_new();
  gtk_box_pack_start(GTK_BOX(box), ev, PACK_1, PACK_2, 0);
  gtk_widget_show(ev);

  w = gtk_label_new(label);
  gtk_misc_set_alignment(GTK_MISC(w), 1.0, 0.0);
  gtk_size_group_add_widget(label_group, w);
  gtk_container_add(GTK_CONTAINER(ev), w);
  gtk_widget_show(w);

  SG_SIGNAL_CONNECT(ev, "button_press_event", prefs_help_click_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(ev, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(ev, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);

  return(w);
}

/* ---------------- row inner label widget ---------------- */

static GtkWidget *make_row_inner_label(prefs_info *prf, const char *label, GtkWidget *box)
{
  GtkWidget *w, *ev;
  ASSERT_WIDGET_TYPE(GTK_IS_HBOX(box), box);

  ev = gtk_event_box_new();
  gtk_box_pack_start(GTK_BOX(box), ev, false, false, 4);
  gtk_widget_show(ev);

  w = gtk_label_new(label);
  gtk_container_add(GTK_CONTAINER(ev), w);
  gtk_widget_show(w);

  SG_SIGNAL_CONNECT(ev, "button_press_event", prefs_help_click_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(ev, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(ev, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);

  return(w);
}

/* ---------------- row middle separator widget ---------------- */

static GtkWidget *make_row_middle_separator(GtkWidget *box)
{
  GtkWidget *w;
  ASSERT_WIDGET_TYPE(GTK_IS_HBOX(box), box);
  w = gtk_vseparator_new();
  gtk_box_pack_start(GTK_BOX(box), w, false, false, 10);
  gtk_widget_show(w);
  return(w);
}

/* ---------------- row inner separator widget ---------------- */

static GtkWidget *make_row_inner_separator(int width, GtkWidget *box)
{
  GtkWidget *w;
  ASSERT_WIDGET_TYPE(GTK_IS_HBOX(box), box);
  w = gtk_hseparator_new();
  gtk_box_pack_start(GTK_BOX(box), w, false, false, width);
  gtk_widget_show(w);
  return(w);
}

/* ---------------- row help widget ---------------- */

static GtkWidget *make_row_help(prefs_info *prf, const char *label, GtkWidget *box)
{
  GtkWidget *w, *ev;

  ev = gtk_event_box_new();
  gtk_box_pack_end(GTK_BOX(box), ev, PACK_1, PACK_2, 0);
  gtk_widget_show(ev);

  w = gtk_label_new(label);
  gtk_misc_set_alignment(GTK_MISC(w), 1.0, 0.0);
  gtk_container_add(GTK_CONTAINER(ev), w);
  gtk_size_group_add_widget(help_group, w);

  gtk_widget_show(w);

  SG_SIGNAL_CONNECT(ev, "button_press_event", prefs_help_click_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(ev, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(ev, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);

  return(w);
}

/* ---------------- row toggle widget ---------------- */

static GtkWidget *make_row_toggle_with_label(prefs_info *prf, bool current_value, GtkWidget *box, const char *label)
{
  GtkWidget *w;
  ASSERT_WIDGET_TYPE(GTK_IS_HBOX(box), box);
  if (label)
    w = gtk_check_button_new_with_label(label);
  else w = gtk_check_button_new();
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(w), current_value);
  gtk_box_pack_start(GTK_BOX(box), w, false, false, 0); /* was 10 */
  gtk_widget_show(w);

  SG_SIGNAL_CONNECT(w, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(w, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(w, "toggled", prefs_change_callback, NULL);

  return(w);
}

static GtkWidget *make_row_toggle(prefs_info *prf, bool current_value, GtkWidget *box)
{
  return(make_row_toggle_with_label(prf, current_value, box, NULL));
}


/* ---------------- error widget ---------------- */

static GtkWidget *make_row_error(prefs_info *prf, GtkWidget *box)
{
  GtkWidget *w;
  ASSERT_WIDGET_TYPE(GTK_IS_HBOX(box), box);

  w = gtk_label_new("");
  gtk_box_pack_end(GTK_BOX(box), w, true, false, 0);
  gtk_widget_show(w);

  SG_SIGNAL_CONNECT(w, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(w, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  
  return(w);
}

/* ---------------- row arrows ---------------- */

static gboolean remove_arrow_func(GtkWidget *w, GdkEventButton *ev, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if (prf->power_id != 0)
    {
      g_source_remove(prf->power_id);
      prf->power_id = 0;
    }
  return(false);
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

static gboolean call_arrow_down_press(GtkWidget *w, GdkEventButton *ev, gpointer context)
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
  return(false);
}

static gboolean call_arrow_up_press(GtkWidget *w, GdkEventButton *ev, gpointer context) 
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
  return(false);
}

static GtkWidget *make_row_arrows(prefs_info *prf, GtkWidget *box)
{
  GtkWidget *ev_up, *ev_down, *up, *down;

  ev_down = gtk_event_box_new();
  gtk_box_pack_start(GTK_BOX(box), ev_down, false, false, 0);
  gtk_widget_show(ev_down);

  down = gtk_arrow_new(GTK_ARROW_DOWN, GTK_SHADOW_ETCHED_OUT);
  gtk_container_add(GTK_CONTAINER(ev_down), down);
  gtk_widget_show(down);

  ev_up = gtk_event_box_new();
  gtk_box_pack_start(GTK_BOX(box), ev_up, false, false, 0);
  gtk_widget_show(ev_up);

  up = gtk_arrow_new(GTK_ARROW_UP, GTK_SHADOW_ETCHED_OUT);
  gtk_container_add(GTK_CONTAINER(ev_up), up);
  gtk_widget_show(up);

  prf->arrow_up = up;
  prf->arrow_down = down;

  SG_SIGNAL_CONNECT(ev_down, "button_press_event", call_arrow_down_press, (gpointer)prf);
  SG_SIGNAL_CONNECT(ev_down, "button_release_event", remove_arrow_func, (gpointer)prf);
  SG_SIGNAL_CONNECT(ev_up, "button_press_event", call_arrow_up_press, (gpointer)prf);
  SG_SIGNAL_CONNECT(ev_up, "button_release_event", remove_arrow_func, (gpointer)prf);

  SG_SIGNAL_CONNECT(ev_up, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(ev_down, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(ev_up, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(ev_down, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);

  SG_SIGNAL_CONNECT(ev_down, "button_press_event", prefs_change_callback, NULL);
  SG_SIGNAL_CONNECT(ev_up, "button_press_event", prefs_change_callback, NULL);

  return(up);
}


/* ---------------- bool row ---------------- */

static void call_toggle_func(GtkWidget *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->toggle_func))
    (*(prf->toggle_func))(prf);
}

static prefs_info *prefs_row_with_toggle(const char *label, const char *varname, bool current_value,
					 GtkWidget *box,
					 void (*toggle_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *sep, *hb, *row, *help;

  ASSERT_WIDGET_TYPE(GTK_IS_VBOX(box), box);
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;

  row = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(box), row, false, false, 0);
  gtk_widget_show(row);

  prf->label = make_row_label(prf, label, row);
  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  gtk_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  sep = make_row_middle_separator(hb);
  prf->toggle = make_row_toggle(prf, current_value, hb);
  help = make_row_help(prf, varname, row);

  SG_SIGNAL_CONNECT(prf->toggle, "toggled", call_toggle_func, (gpointer)prf);

  return(prf);
}


/* ---------------- two toggles ---------------- */

static void call_toggle2_func(GtkWidget *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->toggle2_func))
    (*(prf->toggle2_func))(prf);
}

static prefs_info *prefs_row_with_two_toggles(const char *label, const char *varname, 
					      const char *label1, bool value1,
					      const char *label2, bool value2,
					      GtkWidget *box,
					      void (*toggle_func)(prefs_info *prf),
					      void (*toggle2_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *sep, *help, *sep1, *row, *hb;

  ASSERT_WIDGET_TYPE(GTK_IS_VBOX(box), box);
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;
  prf->toggle2_func = toggle2_func;

  row = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(box), row, false, false, 0);
  gtk_widget_show(row);

  prf->label = make_row_label(prf, label, row);
  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  gtk_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  sep = make_row_middle_separator(hb);
  prf->toggle = make_row_toggle_with_label(prf, value1, hb, label1);
  sep1 = make_row_inner_separator(20, hb);
  prf->toggle2 = make_row_toggle_with_label(prf, value2, hb, label2);
  help = make_row_help(prf, varname, row);

  SG_SIGNAL_CONNECT(prf->toggle, "toggled", call_toggle_func, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->toggle2, "toggled", call_toggle2_func, (gpointer)prf);

  return(prf);
}



/* ---------------- toggle with text ---------------- */

static GtkWidget *make_row_text(prefs_info *prf, const char *text_value, int cols, GtkWidget *box)
{
  GtkWidget *w;
  GtkSettings *settings;
  ASSERT_WIDGET_TYPE(GTK_IS_HBOX(box), box);

  w = gtk_entry_new();
  gtk_entry_set_has_frame(GTK_ENTRY(w), true);
  if (text_value) sg_entry_set_text(GTK_ENTRY(w), text_value);
  gtk_entry_set_has_frame(GTK_ENTRY(w), false);
  if (cols > 0)
    gtk_entry_set_width_chars(GTK_ENTRY(w), cols);
  gtk_editable_set_editable(GTK_EDITABLE(w), true);
  settings = gtk_widget_get_settings(w);
  g_object_set(settings, "gtk-entry-select-on-focus", false, NULL);
  gtk_box_pack_start(GTK_BOX(box), w, false, false, 0);
  gtk_widget_show(w);

  SG_SIGNAL_CONNECT(w, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(w, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(w, "activate", prefs_change_callback, NULL);

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
						   GtkWidget *box,
						   void (*toggle_func)(prefs_info *prf),
						   void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *sep, *sep1, *lab1, *hb, *row, *help;

  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;
  prf->text_func = text_func;

  row = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(box), row, false, false, 0);
  gtk_widget_show(row);

  prf->label = make_row_label(prf, label, row);
  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  gtk_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  sep = make_row_middle_separator(hb);
  prf->toggle = make_row_toggle(prf, current_value, hb);
  sep1 = make_row_inner_separator(16, hb);
  lab1 = make_row_inner_label(prf, text_label, hb);
  prf->text= make_row_text(prf, text_value, cols, hb);
  help = make_row_help(prf, varname, row);

  SG_SIGNAL_CONNECT(prf->toggle, "toggled", call_toggle_func, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "activate", call_text_func, (gpointer)prf);

  return(prf);
}

static prefs_info *prefs_row_with_toggle_with_two_texts(const char *label, const char *varname, bool current_value,
							const char *label1, const char *text1, 
							const char *label2, const char *text2, int cols,
							GtkWidget *box,
							void (*toggle_func)(prefs_info *prf),
							void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *sep, *sep1, *lab1, *lab2, *hb, *row, *help;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;
  prf->text_func = text_func;

  row = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(box), row, false, false, 0);
  gtk_widget_show(row);

  prf->label = make_row_label(prf, label, row);
  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  gtk_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  sep = make_row_middle_separator(hb);
  prf->toggle = make_row_toggle(prf, current_value, hb);
  sep1 = make_row_inner_separator(16, hb);
  lab1 = make_row_inner_label(prf, label1, hb);
  prf->text= make_row_text(prf, text1, cols, hb);
  lab2 = make_row_inner_label(prf, label2, hb);
  prf->rtxt= make_row_text(prf, text2, cols, hb);
  help = make_row_help(prf, varname, row);

  SG_SIGNAL_CONNECT(prf->toggle, "toggled", call_toggle_func, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "activate", call_text_func, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->rtxt, "activate", call_text_func, (gpointer)prf);

  return(prf);
}



/* ---------------- text with toggle ---------------- */

static prefs_info *prefs_row_with_text_with_toggle(const char *label, const char *varname, bool current_value,
						   const char *toggle_label, const char *text_value, int cols,
						   GtkWidget *box,
						   void (*toggle_func)(prefs_info *prf),
						   void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *sep, *sep1, *lab1, *hb, *row, *help;

  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;
  prf->text_func = text_func;

  row = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(box), row, false, false, 0);
  gtk_widget_show(row);

  prf->label = make_row_label(prf, label, row);
  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  gtk_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  sep = make_row_middle_separator(hb);
  prf->text = make_row_text(prf, text_value, cols, hb);
  sep1 = make_row_inner_separator(8, hb);
  lab1 = make_row_inner_label(prf, toggle_label, hb);
  prf->toggle = make_row_toggle(prf, current_value, hb);  
  help = make_row_help(prf, varname, row);
  
  SG_SIGNAL_CONNECT(prf->toggle, "toggled", call_toggle_func, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "activate", call_text_func, (gpointer)prf);

  return(prf);
}


/* ---------------- text with three toggle ---------------- */

static prefs_info *prefs_row_with_text_and_three_toggles(const char *label, const char *varname,
							 const char *text_label, int cols,
							 const char *toggle1_label, const char *toggle2_label, const char *toggle3_label,
							 const char *text_value, 
							 bool toggle1_value, bool toggle2_value, bool toggle3_value,
							 GtkWidget *box,
							 void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *sep, *sep1, *sep2, *sep3, *lab1, *lab2, *lab3, *lab4, *hb, *row, *help;

  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->text_func = text_func;

  row = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(box), row, false, false, 0);
  gtk_widget_show(row);

  prf->label = make_row_label(prf, label, row);
  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  gtk_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  sep = make_row_middle_separator(hb);
  lab4 = make_row_inner_label(prf, text_label, hb);
  prf->text = make_row_text(prf, text_value, cols, hb);
  sep1 = make_row_inner_separator(12, hb);
  lab1 = make_row_inner_label(prf, toggle1_label, hb);
  prf->toggle = make_row_toggle(prf, toggle1_value, hb);  
  sep2 = make_row_inner_separator(4, hb);
  lab2 = make_row_inner_label(prf, toggle2_label, hb);
  prf->toggle2 = make_row_toggle(prf, toggle2_value, hb);  
  sep3 = make_row_inner_separator(4, hb);
  lab3 = make_row_inner_label(prf, toggle3_label, hb);
  prf->toggle3 = make_row_toggle(prf, toggle3_value, hb);  
  help = make_row_help(prf, varname, row);
  
  SG_SIGNAL_CONNECT(prf->text, "activate", call_text_func, (gpointer)prf);
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

static GtkWidget *make_row_radio_box(prefs_info *prf,
				     const char **labels, int num_labels, int current_value,
				     GtkWidget *box)
{
  GtkWidget *w, *current_button;
  int i;

  w = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(box), w, false, false, 0);
  gtk_widget_show(w);

  prf->radio_buttons = (GtkWidget **)CALLOC(num_labels, sizeof(GtkWidget *));

  for (i = 0; i < num_labels; i++)
    {
      if (i == 0)
	current_button = gtk_radio_button_new_with_label(NULL, labels[i]);
      else current_button = gtk_radio_button_new_with_label(gtk_radio_button_get_group(GTK_RADIO_BUTTON(prf->radio_buttons[0])), labels[i]);
      prf->radio_buttons[i] = current_button;
      gtk_box_pack_start(GTK_BOX(w), current_button, false, false, 0);
      set_user_int_data(G_OBJECT(current_button), i);
      gtk_widget_show(current_button);
      SG_SIGNAL_CONNECT(current_button, "clicked", call_radio_func, (gpointer)prf);
      SG_SIGNAL_CONNECT(current_button, "clicked", prefs_change_callback, NULL);
      SG_SIGNAL_CONNECT(current_button, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
      SG_SIGNAL_CONNECT(current_button, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
    }

  if (current_value != -1)
    set_toggle_button(prf->radio_buttons[current_value], true, false, (void *)prf);

  return(w);
}
  

static prefs_info *prefs_row_with_radio_box(const char *label, const char *varname, 
					    const char **labels, int num_labels, int current_value,
					    GtkWidget *box,
					    void (*toggle_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *sep, *hb, *row, *help;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;

  row = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(box), row, false, false, 0);
  gtk_widget_show(row);

  prf->label = make_row_label(prf, label, row);

  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  gtk_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  sep = make_row_middle_separator(hb);
  prf->toggle = make_row_radio_box(prf, labels, num_labels, current_value, hb);
  help = make_row_help(prf, varname, row);

  return(prf);
}

static prefs_info *prefs_row_with_radio_box_and_number(const char *label, const char *varname, 
						       const char **labels, int num_labels, int current_value,
						       int number, const char *text_value, int text_cols,
						       GtkWidget *box,
						       void (*toggle_func)(prefs_info *prf),
						       void (*arrow_up_func)(prefs_info *prf), void (*arrow_down_func)(prefs_info *prf), 
						       void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *sep, *help, *row, *hb;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->toggle_func = toggle_func;
  prf->text_func = text_func;
  prf->arrow_up_func = arrow_up_func;
  prf->arrow_down_func = arrow_down_func;

  row = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(box), row, false, false, 0);
  gtk_widget_show(row);

  prf->label = make_row_label(prf, label, row);

  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  gtk_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  sep = make_row_middle_separator(hb);
  prf->toggle = make_row_radio_box(prf, labels, num_labels, current_value, hb);
  prf->text = make_row_text(prf, text_value, text_cols, hb);
  prf->arrow_up = make_row_arrows(prf, hb);
  help = make_row_help(prf, varname, row);

  SG_SIGNAL_CONNECT(prf->text, "activate", call_text_func, (gpointer)prf);

  return(prf);
}



/* ---------------- scale row ---------------- */

static void call_scale_func(GtkAdjustment *w, gpointer context)
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
  float_to_textfield(prf->text, GTK_ADJUSTMENT(prf->adj)->value * prf->scale_max);
}

static prefs_info *prefs_row_with_scale(const char *label, const char *varname, 
					Float max_val, Float current_value,
					GtkWidget *box,
					void (*scale_func)(prefs_info *prf),
					void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *sep, *hb, *row, *help;
  char *str;

  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  prf->scale_max = max_val;

  row = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(box), row, false, false, 0);
  gtk_widget_show(row);

  prf->label = make_row_label(prf, label, row);
  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  gtk_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  sep = make_row_middle_separator(hb);
  
  str = (char *)CALLOC(12, sizeof(char));
  mus_snprintf(str, 12, "%.3f", current_value);
  prf->text = make_row_text(prf, str, 6, hb);
  FREE(str);

  prf->adj = gtk_adjustment_new(current_value /max_val, 0.0, 1.01, 0.001, 0.01, .01);
  prf->scale = gtk_hscale_new(GTK_ADJUSTMENT(prf->adj));
  gtk_box_pack_start(GTK_BOX(hb), prf->scale, true, true, 4);
  gtk_widget_show(prf->scale);
  gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(prf->scale)), GTK_UPDATE_CONTINUOUS);
  gtk_scale_set_draw_value(GTK_SCALE(prf->scale), false);
  
  help = make_row_help(prf, varname, row);

  prf->scale_func = scale_func;
  prf->text_func = text_func;

  SG_SIGNAL_CONNECT(prf->scale, "value_changed", call_scale_func, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->scale, "value_changed", prefs_change_callback, NULL);
  SG_SIGNAL_CONNECT(prf->scale, "value_changed", prefs_scale_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "activate", call_scale_text_func, (gpointer)prf);

  SG_SIGNAL_CONNECT(prf->scale, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->scale, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);

  return(prf);
}


/* ---------------- text row ---------------- */

static prefs_info *prefs_row_with_text(const char *label, const char *varname, const char *value,
				       GtkWidget *box,
				       void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *sep, *hb, *row, *help;

  ASSERT_WIDGET_TYPE(GTK_IS_VBOX(box), box);
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;

  row = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(box), row, false, false, 0);
  gtk_widget_show(row);

  prf->label = make_row_label(prf, label, row);
  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  gtk_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  sep = make_row_middle_separator(hb);
  prf->text = make_row_text(prf, value, 0, hb);
  help = make_row_help(prf, varname, row);

  prf->text_func = text_func;
  SG_SIGNAL_CONNECT(prf->text, "activate", call_text_func, (gpointer)prf);

  return(prf);
}


/* ---------------- two texts in a row ---------------- */

static prefs_info *prefs_row_with_two_texts(const char *label, const char *varname,
					    const char *label1, const char *text1, const char *label2, const char *text2, int cols,
					    GtkWidget *box,
					    void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *sep, *lab1, *lab2, *hb, *row, *help;
  ASSERT_WIDGET_TYPE(GTK_IS_VBOX(box), box);
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;

  row = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(box), row, false, false, 0);
  gtk_widget_show(row);

  prf->label = make_row_label(prf, label, row);
  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  gtk_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  sep = make_row_middle_separator(hb);
  lab1 = make_row_inner_label(prf, label1, hb);
  prf->text = make_row_text(prf, text1, cols, hb);
  lab2 = make_row_inner_label(prf, label2, hb);  
  prf->rtxt = make_row_text(prf, text2, cols, hb);
  help = make_row_help(prf, varname, row);

  prf->text_func = text_func;

  SG_SIGNAL_CONNECT(prf->text, "activate", call_text_func, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->rtxt, "activate", call_text_func, (gpointer)prf);

  return(prf);
}

/* ---------------- number row ---------------- */

static prefs_info *prefs_row_with_number(const char *label, const char *varname, const char *value, int cols,
					 GtkWidget *box,
 					 void (*arrow_up_func)(prefs_info *prf), void (*arrow_down_func)(prefs_info *prf), 
					 void (*text_func)(prefs_info *prf))
{
  prefs_info *prf = NULL;
  GtkWidget *sep, *hb, *row, *help;

  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;

  row = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(box), row, false, false, 0);
  gtk_widget_show(row);

  prf->label = make_row_label(prf, label, row);

  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  gtk_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  sep = make_row_middle_separator(hb);
  prf->text = make_row_text(prf, value, cols, hb);
  prf->arrow_up = make_row_arrows(prf, hb);
  prf->error = make_row_error(prf, hb);
  help = make_row_help(prf, varname, row);

  prf->text_func = text_func;
  prf->arrow_up_func = arrow_up_func;
  prf->arrow_down_func = arrow_down_func;

  SG_SIGNAL_CONNECT(prf->text, "activate", call_text_func, (gpointer)prf);
  return(prf);
}


/* ---------------- list row ---------------- */

#if HAVE_GTK_COMBO_BOX_ENTRY_NEW_TEXT
static prefs_info *prefs_row_with_list(const char *label, const char *varname, const char *value,
				       const char **values, int num_values,
				       GtkWidget *box,
				       void (*text_func)(prefs_info *prf),
				       char *(*completion_func)(char *text, void *context), void *completion_context)
{
  int i;
  prefs_info *prf = NULL;
  GtkWidget *sep, *hb, *row, *help;

  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;

  row = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(box), row, false, false, 0);
  gtk_widget_show(row);

  prf->label = make_row_label(prf, label, row);
  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  gtk_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  sep = make_row_middle_separator(hb);  
  
  prf->text = gtk_combo_box_entry_new_text();
  for (i = 0; i < num_values; i++)
    gtk_combo_box_append_text(GTK_COMBO_BOX(prf->text), values[i]);
  sg_entry_set_text(GTK_ENTRY(GTK_BIN(prf->text)->child), value);
  gtk_box_pack_start(GTK_BOX(hb), prf->text, false, false, 4);
  gtk_widget_show(prf->text);

  prf->error = make_row_error(prf, hb);
  help = make_row_help(prf, varname, row);

  prf->text_func = text_func;
  SG_SIGNAL_CONNECT(prf->text, "changed", call_text_func, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "changed", prefs_change_callback, NULL);

  SG_SIGNAL_CONNECT(prf->text, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->text, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  return(prf);
}
#endif

/* ---------------- color selector row(s) ---------------- */

static GdkColor *rgb_to_color(Float r, Float g, Float b)
{
  GdkColor gcolor;
  GdkColor *ccolor;
  gcolor.red = (unsigned short)(65535 * r);
  gcolor.green = (unsigned short)(65535 * g);
  gcolor.blue = (unsigned short)(65535 * b);
  ccolor = gdk_color_copy(&gcolor);
  gdk_rgb_find_color(gdk_colormap_get_system(), ccolor);
  return(ccolor);
}

static void pixel_to_rgb(color_t pix, float *r, float *g, float *b)
{
  (*r) = (float)(pix->red) / 65535.0;
  (*g) = (float)(pix->green) / 65535.0;
  (*b) = (float)(pix->blue) / 65535.0;
}

static void reflect_color(prefs_info *prf)
{
  Float r, g, b;
  GdkColor *current_color;

  r = GTK_ADJUSTMENT(prf->radj)->value;
  g = GTK_ADJUSTMENT(prf->gadj)->value;
  b = GTK_ADJUSTMENT(prf->badj)->value;

  current_color = rgb_to_color(r, g, b);
  gtk_widget_modify_bg(prf->color, GTK_STATE_NORMAL, current_color);

  r = current_color->red / 65535.0;
  g = current_color->green / 65535.0;
  b = current_color->blue / 65535.0;

  float_to_textfield(prf->rtxt, r);
  float_to_textfield(prf->gtxt, g);
  float_to_textfield(prf->btxt, b);
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
      gtk_adjustment_set_value(GTK_ADJUSTMENT(prf->radj), r);
      reflect_color(prf);
    }
  else sg_entry_set_text(GTK_ENTRY(w), "err");

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
      gtk_adjustment_set_value(GTK_ADJUSTMENT(prf->gadj), r);
      reflect_color(prf);
    }
  else sg_entry_set_text(GTK_ENTRY(w), "err");

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
      gtk_adjustment_set_value(GTK_ADJUSTMENT(prf->badj), r);
      reflect_color(prf);
    }
  else sg_entry_set_text(GTK_ENTRY(w), "err");

}

static void prefs_call_color_func_callback(GtkWidget *w, gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  if ((prf) && (prf->color_func))
    {
      float r, g, b;
      r = GTK_ADJUSTMENT(prf->radj)->value;
      g = GTK_ADJUSTMENT(prf->gadj)->value;
      b = GTK_ADJUSTMENT(prf->badj)->value;
      (*(prf->color_func))(prf, r, g, b);
    }
}

static prefs_info *prefs_color_selector_row(const char *label, const char *varname, 
					    color_t current_pixel,
					    GtkWidget *box,
					    void (*color_func)(prefs_info *prf, float r, float g, float b))
{
  prefs_info *prf = NULL;
  GtkWidget *sep, *sep1, *hb, *row, *row2, *sep2, *sep3, *help;
  float r = 0.0, g = 0.0, b = 0.0;

  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));
  prf->var_name = varname;
  pixel_to_rgb(current_pixel, &r, &g, &b);

  /* first row */
  row = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(box), row, false, false, 0);
  gtk_widget_show(row);

  prf->label = make_row_label(prf, label, row);
  hb = gtk_hbox_new(false, 0);
  gtk_size_group_add_widget(widgets_group, hb);
  gtk_box_pack_start(GTK_BOX(row), hb, false, false, 0);
  gtk_widget_show(hb);

  sep = make_row_middle_separator(hb);    

  prf->color_texts = gtk_size_group_new(GTK_SIZE_GROUP_HORIZONTAL);
  prf->color = gtk_drawing_area_new();
  gtk_widget_set_events(prf->color, GDK_ENTER_NOTIFY_MASK | GDK_LEAVE_NOTIFY_MASK);
  gtk_box_pack_start(GTK_BOX(hb), prf->color, false, false, 4);
  gtk_size_group_add_widget(prf->color_texts, prf->color);
  gtk_widget_modify_bg(prf->color, GTK_STATE_NORMAL, current_pixel);
  gtk_widget_show(prf->color);
  
  sep1 = make_row_inner_separator(8, hb);
  
  prf->rtxt = make_row_text(prf, NULL, 6, hb);
  gtk_size_group_add_widget(prf->color_texts, prf->rtxt);
  float_to_textfield(prf->rtxt, r);

  prf->gtxt = make_row_text(prf, NULL, 6, hb);
  gtk_size_group_add_widget(prf->color_texts, prf->gtxt);
  float_to_textfield(prf->gtxt, g);

  prf->btxt = make_row_text(prf, NULL, 6, hb);
  gtk_size_group_add_widget(prf->color_texts, prf->btxt);
  float_to_textfield(prf->btxt, b);
  help = make_row_help(prf, varname, row);

  /* second row */

  row2 = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(box), row2, false, false, 0);
  gtk_widget_show(row2);

  sep2 = make_row_inner_separator(20, row2);

  prf->radj = gtk_adjustment_new(r, 0.0, 1.01, 0.001, 0.01, .01);
  prf->rscl = gtk_hscale_new(GTK_ADJUSTMENT(prf->radj));
  gtk_box_pack_start(GTK_BOX(row2), prf->rscl, true, true, 4);
  /* normal = slider, active = trough, selected unused */
  gtk_widget_modify_bg(prf->rscl, GTK_STATE_NORMAL, rscl_color);
  gtk_widget_modify_bg(prf->rscl, GTK_STATE_PRELIGHT, rscl_color);
  gtk_widget_show(prf->rscl);
  gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(prf->rscl)), GTK_UPDATE_CONTINUOUS);
  gtk_scale_set_draw_value(GTK_SCALE(prf->rscl), false);

  prf->gadj = gtk_adjustment_new(g, 0.0, 1.01, 0.001, 0.01, .01);
  prf->gscl = gtk_hscale_new(GTK_ADJUSTMENT(prf->gadj));
  gtk_box_pack_start(GTK_BOX(row2), prf->gscl, true, true, 4);
  gtk_widget_modify_bg(prf->gscl, GTK_STATE_NORMAL, gscl_color);
  gtk_widget_modify_bg(prf->gscl, GTK_STATE_PRELIGHT, gscl_color);
  gtk_widget_show(prf->gscl);
  gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(prf->gscl)), GTK_UPDATE_CONTINUOUS);
  gtk_scale_set_draw_value(GTK_SCALE(prf->gscl), false);

  prf->badj = gtk_adjustment_new(b, 0.0, 1.01, 0.001, 0.01, .01);
  prf->bscl = gtk_hscale_new(GTK_ADJUSTMENT(prf->badj));
  gtk_box_pack_start(GTK_BOX(row2), prf->bscl, true, true, 4);
  gtk_widget_modify_bg(prf->bscl, GTK_STATE_NORMAL, bscl_color);
  gtk_widget_modify_bg(prf->bscl, GTK_STATE_PRELIGHT, bscl_color);
  gtk_widget_show(prf->bscl);
  gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(prf->bscl)), GTK_UPDATE_CONTINUOUS);
  gtk_scale_set_draw_value(GTK_SCALE(prf->bscl), false);

  sep3 = gtk_hseparator_new();
  gtk_box_pack_end(GTK_BOX(row2), sep3, false, false, 20);
  gtk_widget_show(sep3);

  SG_SIGNAL_CONNECT(prf->rtxt, "activate", prefs_r_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->gtxt, "activate", prefs_g_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->btxt, "activate", prefs_b_callback, (gpointer)prf);

  SG_SIGNAL_CONNECT(prf->radj, "value_changed", prefs_call_color_func_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->gadj, "value_changed", prefs_call_color_func_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->badj, "value_changed", prefs_call_color_func_callback, (gpointer)prf);

  SG_SIGNAL_CONNECT(prf->radj, "value_changed", prefs_change_callback, NULL);
  SG_SIGNAL_CONNECT(prf->gadj, "value_changed", prefs_change_callback, NULL);
  SG_SIGNAL_CONNECT(prf->badj, "value_changed", prefs_change_callback, NULL);

  SG_SIGNAL_CONNECT(prf->radj, "value_changed", prefs_color_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->gadj, "value_changed", prefs_color_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->badj, "value_changed", prefs_color_callback, (gpointer)prf);

  SG_SIGNAL_CONNECT(prf->color, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->color, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->rscl, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->rscl, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->gscl, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->gscl, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->bscl, "enter_notify_event", mouse_enter_pref_callback, (gpointer)prf);
  SG_SIGNAL_CONNECT(prf->bscl, "leave_notify_event", mouse_leave_pref_callback, (gpointer)prf);

  prf->color_func = color_func;

  return(prf);
}


/* ---------------- topic separator ---------------- */

static GtkWidget *make_inter_topic_separator(GtkWidget *topics)
{
  GtkWidget *w;
  ASSERT_WIDGET_TYPE(GTK_IS_VBOX(topics), topics);
  w = gtk_hseparator_new();
  gtk_box_pack_start(GTK_BOX(topics), w, false, false, 0);
  gtk_widget_show(w);
  return(w);
  /* height = INTER_TOPIC_SPACE no line */
}

/* ---------------- variable separator ---------------- */

static GtkWidget *make_inter_variable_separator(GtkWidget *topics)
{
  GtkWidget *w;
  ASSERT_WIDGET_TYPE(GTK_IS_VBOX(topics), topics);
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
  ASSERT_WIDGET_TYPE(GTK_IS_VBOX(parent), parent);
#if HAVE_GTK_BUTTON_SET_ALIGNMENT
  w = snd_gtk_highlight_label_new(label);
  gtk_button_set_alignment(GTK_BUTTON(w), 0.01, 0.5);
  gtk_widget_modify_bg(w, GTK_STATE_NORMAL, ss->sgx->light_blue);
  gtk_widget_modify_bg(w, GTK_STATE_PRELIGHT, ss->sgx->light_blue);
#else
  w = snd_gtk_entry_label_new(label, ss->sgx->light_blue);
#endif
  gtk_box_pack_start(GTK_BOX(parent), w, false, false, 0);
  gtk_widget_show(w);
  return(w);
}

static GtkWidget *make_top_level_box(GtkWidget *topics)
{
  GtkWidget *w, *frame;
  ASSERT_WIDGET_TYPE(GTK_IS_VBOX(topics), topics);
  frame = gtk_frame_new(NULL);
  gtk_box_pack_start(GTK_BOX(topics), frame, true, true, 0);
  gtk_widget_show(frame);
  w = gtk_vbox_new(false, 0);
  gtk_container_add(GTK_CONTAINER(frame), w);
  gtk_widget_show(w);
  return(w);
}

static GtkWidget *make_inner_label(const char *label, GtkWidget *parent)
{
  GtkWidget *w;
  ASSERT_WIDGET_TYPE(GTK_IS_VBOX(parent), parent);
#if HAVE_GTK_BUTTON_SET_ALIGNMENT
  w = snd_gtk_highlight_label_new(label);
  gtk_button_set_alignment(GTK_BUTTON(w), 0.0, 0.5);
#else
  w = snd_gtk_entry_label_new(label, ss->sgx->highlight_color);
#endif
  gtk_box_pack_start(GTK_BOX(parent), w, false, false, 0);
  gtk_widget_show(w);
  return(w);
}


/* ---------------- base buttons ---------------- */

static gint preferences_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  prefs_helping = false;
  clear_prefs_dialog_error();
  gtk_widget_hide(preferences_dialog);
  return(true);
}

static void preferences_dismiss_callback(GtkWidget *w, gpointer context) 
{
  prefs_helping = false;
  clear_prefs_dialog_error();
  gtk_widget_hide(preferences_dialog);
}

static void preferences_help_callback(GtkWidget *w, gpointer context) 
{
  prefs_helping = true;
  snd_help("preferences",
	   "This dialog sets various global variables. 'Save' then writes the new values \
to ~/.snd_prefs_guile|ruby so that they take effect the next time you start Snd.  'Reset' resets all variables to \
their default (initial) values. 'Help' starts this dialog, and as long as it is active, it will post helpful \
information if the mouse lingers over some variable -- sort of a tooltip that stays out of your way. \
You can also request help on a given topic by clicking the variable name on the far right.",
	   WITH_WORD_WRAP);
}

static void prefs_set_dialog_title(const char *filename)
{
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
  gtk_window_set_title(GTK_WINDOW(preferences_dialog), str);
  FREE(str);
}

static void preferences_reset_callback(GtkWidget *w, gpointer context) 
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
    }
  prefs_set_dialog_title(NULL);
}

static void preferences_save_callback(GtkWidget *w, gpointer context) 
{
  clear_prefs_dialog_error();
  redirect_snd_error_to(post_prefs_dialog_error, NULL);
  redirect_snd_warning_to(post_prefs_dialog_error, NULL);
  save_prefs(save_options_in_prefs(), include_load_path);
  redirect_snd_error_to(NULL, NULL);
  redirect_snd_warning_to(NULL, NULL);
}



/* ---------------- errors ---------------- */

static void clear_prefs_error(GtkWidget *w, gpointer context) 
{
  prefs_info *prf = (prefs_info *)context;
  g_signal_handler_disconnect(prf->text, prf->erase_id);
  prf->erase_id = 0;
  set_label(prf->error, "");
}

static void post_prefs_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  prf->got_error = true;
  set_label(prf->error, msg);
  if (prf->erase_id != 0)
    g_signal_handler_disconnect(prf->text, prf->erase_id);
  prf->erase_id = SG_SIGNAL_CONNECT(prf->text, "changed", clear_prefs_error, (gpointer)prf);
}

#ifdef __GNUC__
static void va_post_prefs_error(const char *msg, void *data, ...) __attribute__ ((format (printf, 1, 0)));
#endif

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
  sg_entry_set_text(GTK_ENTRY(prf->text), "must be > 0");
  g_timeout_add_full(0,
		     ERROR_WAIT_TIME,
		     startup_width_erase_func,
		     (gpointer)prf, NULL);
}

static void startup_height_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  sg_entry_set_text(GTK_ENTRY(prf->rtxt), "must be > 0");
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
  set_toggle_button(prf->toggle, auto_resize(ss), false, (void *)prf);
}

static void resize_toggle(prefs_info *prf)
{
  set_auto_resize(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

/* ---------------- ask-before-overwrite ---------------- */

static void reflect_ask_before_overwrite(prefs_info *prf) 
{
  set_toggle_button(prf->toggle, ask_before_overwrite(ss), false, (void *)prf);
}

static void overwrite_toggle(prefs_info *prf)
{
  set_ask_before_overwrite(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

/* ---------------- check-for-unsaved-edits ---------------- */

static bool include_unsaved_edits = false;

static void reflect_unsaved_edits(prefs_info *prf) 
{
  include_unsaved_edits = unsaved_edits();
  set_toggle_button(prf->toggle, include_unsaved_edits, false, (void *)prf);
}

static void unsaved_edits_toggle(prefs_info *prf)
{
  include_unsaved_edits = (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

static void save_unsaved_edits(prefs_info *prf, FILE *fd)
{
  if (include_unsaved_edits) save_unsaved_edits_1(prf, fd);
}

/* ---------------- current-window-display ---------------- */

static bool include_current_window_display = false;

static void save_current_window_display(prefs_info *prf, FILE *fd)
{
  if (include_current_window_display) save_current_window_display_1(prf, fd);
}

static void current_window_display_toggle(prefs_info *prf)
{
  include_current_window_display = (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

static void reflect_current_window_display(prefs_info *prf) 
{
  set_toggle_button(prf->toggle, find_current_window_display(), false, (void *)prf);
}

/* ---------------- focus-follows-mouse ---------------- */

static bool focus_follows_mouse = false;

static void reflect_focus_follows_mouse(prefs_info *prf) 
{
  focus_follows_mouse = focus_is_following_mouse();
  set_toggle_button(prf->toggle, focus_follows_mouse, false, (void *)prf);
}

static void focus_follows_mouse_toggle(prefs_info *prf)
{
  focus_follows_mouse = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle));
}

static void save_focus_follows_mouse(prefs_info *prf, FILE *fd) 
{
  if (focus_follows_mouse) save_focus_follows_mouse_1(prf, fd);
}

/* ---------------- sync choice ---------------- */

static int global_sync_choice = 0;

static void reflect_sync_choice(prefs_info *prf)
{
  global_sync_choice = find_sync_choice();
  set_toggle_button(prf->toggle, global_sync_choice == 2, false, (void *)prf);
  set_toggle_button(prf->toggle2, global_sync_choice == 1, false, (void *)prf);
}

static void save_sync_choice(prefs_info *prf, FILE *fd)
{
  if (global_sync_choice != 0)
    save_sync_choice_1(prf, fd, global_sync_choice);
}

static void sync1_choice(prefs_info *prf)
{
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)))
    global_sync_choice = 2;
  else global_sync_choice = 0;
  set_toggle_button(prf->toggle2, false, false, (void *)prf);
}

static void sync2_choice(prefs_info *prf)
{
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle2)))
    global_sync_choice = 1;
  else global_sync_choice = 0;
  set_toggle_button(prf->toggle, false, false, (void *)prf);
}


/* ---------------- remember sound state ---------------- */

static int global_remember_sound_state_choice = 0; /* 0=none, 1=local, 2=global+not local, 3=local+global */

static void reflect_remember_sound_state_choice(prefs_info *prf)
{
  global_remember_sound_state_choice = find_remember_sound_state_choice();
  set_toggle_button(prf->toggle, global_remember_sound_state_choice & 1, false, (void *)prf);
  set_toggle_button(prf->toggle2, global_remember_sound_state_choice & 2, false, (void *)prf);
}

static void save_remember_sound_state_choice(prefs_info *prf, FILE *fd)
{
  if (global_remember_sound_state_choice != 0)
    save_remember_sound_state_choice_1(prf, fd, global_remember_sound_state_choice);
}

static void remember_sound_state_1_choice(prefs_info *prf)
{
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)))
    global_remember_sound_state_choice |= 1;
  else global_remember_sound_state_choice &= 2;
}

static void remember_sound_state_2_choice(prefs_info *prf)
{
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle2)))
    global_remember_sound_state_choice |= 2;
  else global_remember_sound_state_choice &= 1;
}


/* ---------------- show-controls ---------------- */

static void reflect_show_controls(prefs_info *prf) 
{
  set_toggle_button(prf->toggle, in_show_controls(ss), false, (void *)prf);
}

static void controls_toggle(prefs_info *prf)
{
  in_set_show_controls(ss, gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

/* ---------------- peak-envs ---------------- */

static bool include_peak_envs = false;
static char *include_peak_env_directory = NULL;

static char *peak_env_directory(void)
{
  if (include_peak_env_directory)
    return(include_peak_env_directory);
  if (XEN_DEFINED_P("save-peak-env-info-directory"))
    return(XEN_TO_C_STRING(XEN_NAME_AS_C_STRING_TO_VALUE("save-peak-env-info-directory")));
  return(NULL);
}

static void reflect_peak_envs(prefs_info *prf) 
{
  if (include_peak_env_directory) {FREE(include_peak_env_directory); include_peak_env_directory = NULL;}
  include_peak_env_directory = copy_string(peak_env_directory());
  include_peak_envs = find_peak_envs();
  set_toggle_button(prf->toggle, include_peak_envs, false, (void *)prf);
  sg_entry_set_text(GTK_ENTRY(prf->text), include_peak_env_directory);
}

static void peak_envs_toggle(prefs_info *prf)
{
  include_peak_envs = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle));
}

static void peak_envs_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      if (include_peak_env_directory) {FREE(include_peak_env_directory); include_peak_env_directory = NULL;}
      include_peak_env_directory = copy_string(str);
    }
}

static void save_peak_envs(prefs_info *prf, FILE *fd)
{
  if (include_peak_envs) save_peak_envs_1(prf, fd, include_peak_env_directory);
}


/* ---------------- selection-creates-region ---------------- */

static void reflect_selection_creates_region(prefs_info *prf) 
{
  set_toggle_button(prf->toggle, selection_creates_region(ss), false, (void *)prf);
  int_to_textfield(prf->text, max_regions(ss));
}

static void selection_creates_region_toggle(prefs_info *prf)
{
  set_selection_creates_region(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

static void max_regions_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      int value = 0;
      sscanf(str, "%d", &value);
      if (value >= 0)
	in_set_max_regions(value);
      else int_to_textfield(prf->text, max_regions(ss));
    }
}


/* ---------------- basic-color ---------------- */

static void scale_set_color(prefs_info *prf, color_t pixel)
{
  float r = 0.0, g = 0.0, b = 0.0;
  pixel_to_rgb(pixel, &r, &g, &b);
  float_to_textfield(prf->rtxt, r);
  gtk_adjustment_set_value(GTK_ADJUSTMENT(prf->radj), r);
  float_to_textfield(prf->gtxt, g);
  gtk_adjustment_set_value(GTK_ADJUSTMENT(prf->gadj), g);
  float_to_textfield(prf->btxt, b);
  gtk_adjustment_set_value(GTK_ADJUSTMENT(prf->badj), b);
  gtk_widget_modify_bg(prf->color, GTK_STATE_NORMAL, pixel);
}

static color_t saved_basic_color;

static void reflect_basic_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_basic_color); 
  set_basic_color(saved_basic_color);
}

static void basic_color_func(prefs_info *prf, float r, float g, float b)
{
  set_basic_color(rgb_to_color(r, g, b));
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
  set_highlight_color(rgb_to_color(r, g, b));
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
  set_position_color(rgb_to_color(r, g, b));
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
  set_zoom_color(rgb_to_color(r, g, b));
}

/* ---------------- keys ---------------- */

static void reflect_key(prefs_info *prf, const char *key_name)
{
  key_info *ki;
  ki = find_prefs_key_binding(key_name);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle), ki->c);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle2), ki->m);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(prf->toggle3), ki->x);
  sg_entry_set_text(GTK_ENTRY(prf->text), ki->key);
  FREE(ki);
}

static void save_key_binding(prefs_info *prf, FILE *fd, char *(*binder)(char *key, bool c, bool m, bool x))
{
  /* pick up possible binding even if no <cr> */
  char *key, *expr;
  key = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((key) && (*key))
    {
      expr = (*binder)(key, 
		       gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)),
		       gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle2)),
		       gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle3)));
      fprintf(fd, expr);
      FREE(expr);
    }
}

static void key_bind(prefs_info *prf, char *(*binder)(char *key, bool c, bool m, bool x))
{
  char *key, *expr;
  bool ctrl, meta, cx;
  key = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  ctrl = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle));
  meta = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle2));
  cx = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle3));
  if ((key) && (*key))
    expr = (*binder)(key, ctrl, meta, cx);
  else expr = mus_format("(unbind-key %s %d %s)",
			 possibly_quote(key), 
			 ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
			 (cx) ? "#t" : "#f");
  XEN_EVAL_C_STRING(expr);
  FREE(expr);
}



/* ---------------- verbose-cursor ---------------- */

static void reflect_verbose_cursor(prefs_info *prf) 
{
  set_toggle_button(prf->toggle, verbose_cursor(ss), false, (void *)prf);
}

static void verbose_cursor_toggle(prefs_info *prf)
{
  in_set_verbose_cursor(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

/* ---------------- with-tracking-cursor ---------------- */

static void reflect_with_tracking_cursor(prefs_info *prf) 
{
  set_toggle_button(prf->toggle, with_tracking_cursor(ss), false, (void *)prf);
  int_to_textfield(prf->rtxt, cursor_location_offset(ss));
  float_to_textfield(prf->text, cursor_update_interval(ss));
}

static void with_tracking_cursor_toggle(prefs_info *prf)
{
  in_set_with_tracking_cursor(ss, (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle))) ? ALWAYS_TRACK : DONT_TRACK);
}

static void cursor_location_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      float interval = DEFAULT_CURSOR_UPDATE_INTERVAL;
      sscanf(str, "%f", &interval);
      if (interval >= 0.0)
	set_cursor_update_interval(interval);
      str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->rtxt));
      if ((str) && (*str))
	{
	  int loc = DEFAULT_CURSOR_LOCATION_OFFSET;
	  sscanf(str, "%d", &loc);
	  set_cursor_location_offset(loc);
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
static cursor_style_t cursor_styles_i[2] = {CURSOR_CROSS, CURSOR_LINE};

static void reflect_cursor_style(prefs_info *prf)
{
  int which = -1;
  if (cursor_style(ss) == CURSOR_CROSS)
    which = 0;
  else 
    {
      if (cursor_style(ss) == CURSOR_LINE)
	which = 1;
    }
  if (which != -1)
    set_toggle_button(prf->radio_buttons[which], true, false, (void *)prf);
}

static void cursor_style_choice(prefs_info *prf)
{
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
    in_set_cursor_style(cursor_styles_i[get_user_int_data(G_OBJECT(prf->radio_button))]);
}

/* ---------------- tracking-cursor-style ---------------- */

static void reflect_tracking_cursor_style(prefs_info *prf)
{
  set_toggle_button(prf->radio_buttons[(tracking_cursor_style(ss) == CURSOR_CROSS) ? 0 : 1], true, false, (void *)prf);
}

static void tracking_cursor_style_choice(prefs_info *prf)
{
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
    in_set_tracking_cursor_style(cursor_styles_i[get_user_int_data(G_OBJECT(prf->radio_button))]);
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
  color_cursor(rgb_to_color(r, g, b));
  for_each_chan(update_graph);
}


/* ---------------- load path ---------------- */

static void reflect_load_path(prefs_info *prf)
{
  char *str;
  str = find_sources();
  sg_entry_set_text(GTK_ENTRY(prf->text), str);
  if (str) FREE(str);
}

static void load_path_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((!str) || (!(*str)))
    return;
  if (local_access(str))
    {
      if (include_load_path) FREE(include_load_path);
      include_load_path = copy_string(str);
#if HAVE_RUBY
      {
	extern VALUE rb_load_path;
	rb_ary_unshift(rb_load_path, rb_str_new2(str));
      }
#endif
#if HAVE_GUILE
      {
	char *buf;
	buf = mus_format("(set! %%load-path (cons \"%s\" %%load-path))", str);
	XEN_EVAL_C_STRING(buf);
	FREE(buf);
      }
#endif
    }
}


/* ---------------- just-sounds ---------------- */

static void prefs_reflect_just_sounds(prefs_info *prf) 
{
  set_toggle_button(prf->toggle, just_sounds(ss), false, (void *)prf);
}

static void just_sounds_toggle(prefs_info *prf)
{
  set_just_sounds(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}


/* ---------------- temp-dir ---------------- */

static void reflect_temp_dir(prefs_info *prf)
{
  ASSERT_WIDGET_TYPE(GTK_IS_ENTRY(prf->text), prf->text);
  sg_entry_set_text(GTK_ENTRY(prf->text), temp_dir(ss));
}

static gint temp_dir_error_erase_func(gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  sg_entry_set_text(GTK_ENTRY(prf->text), temp_dir(ss));
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
      sg_entry_set_text(GTK_ENTRY(prf->text), "can't access that directory");
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
  sg_entry_set_text(GTK_ENTRY(prf->text), save_dir(ss));
  return(0);
}

static void reflect_save_dir(prefs_info *prf)
{
  sg_entry_set_text(GTK_ENTRY(prf->text), save_dir(ss));
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
      sg_entry_set_text(GTK_ENTRY(prf->text), "can't access that directory");
      g_timeout_add_full(0,
			 ERROR_WAIT_TIME,
			 save_dir_error_erase_func,
			 (gpointer)prf, NULL);
    }

}

/* ---------------- save-state-file ---------------- */

static void reflect_save_state_file(prefs_info *prf)
{
  sg_entry_set_text(GTK_ENTRY(prf->text), save_state_file(ss));
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
  sg_entry_set_text(GTK_ENTRY(prf->text), ladspa_dir(ss));
}

static void ladspa_dir_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if (ladspa_dir(ss)) FREE(ladspa_dir(ss));
  if (str)
    set_ladspa_dir(copy_string(str));
  else set_ladspa_dir(copy_string(DEFAULT_LADSPA_DIR));
}
#endif


/* ---------------- view-files directory ---------------- */

static char *include_vf_directory = NULL;

static void reflect_view_files_directory(prefs_info *prf)
{
  if (include_vf_directory) FREE(include_vf_directory);
  include_vf_directory = copy_string(view_files_find_any_directory());
  sg_entry_set_text(GTK_ENTRY(prf->text), view_files_find_any_directory());
}

static void view_files_directory_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if (include_vf_directory) FREE(include_vf_directory);
  include_vf_directory = copy_string(str); /* could be null to cancel */
  if (str)
    view_files_add_directory(NULL_WIDGET, (const char *)str);
}

static void save_view_files_directory(prefs_info *prf, FILE *fd)
{
  if (include_vf_directory) save_view_files_directory_1(prf, fd, include_vf_directory);
}


/* ---------------- html-program ---------------- */

static void reflect_html_program(prefs_info *prf)
{
  sg_entry_set_text(GTK_ENTRY(prf->text), html_program(ss));
}

static void html_program_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if (html_program(ss)) FREE(html_program(ss));
  if (str)
    set_html_program(copy_string(str));
  else set_html_program(copy_string(DEFAULT_HTML_PROGRAM));
}

/* ---------------- default-output-chans etc ---------------- */

static const char *output_chan_choices[4] = {"1", "2", "4", "8"};
static int output_chans[4] = {1, 2, 4, 8};

static const char *output_srate_choices[4] = {"8000", "22050", "44100", "48000"};
static int output_srates[4] = {8000, 22050, 44100, 48000};

static void reflect_output_chans(prefs_info *prf)
{
  int which = -1;
  switch (default_output_chans(ss))
    {
    case 1: which = 0; break;
    case 2: which = 1; break;
    case 4: which = 2; break;
    case 8: which = 3; break;
    }
  if (which != -1)
    set_toggle_button(prf->radio_buttons[which], true, false, (void *)prf);
}

static void reflect_output_srate(prefs_info *prf)
{
  int which = -1, sr;
  sr = default_output_srate(ss);
  if (sr == 8000) which = 0; else
  if (sr == 22050) which = 1; else
  if (sr == 44100) which = 2; else
  if (sr == 48000) which = 3;
  if (which != -1)
    set_toggle_button(prf->radio_buttons[which], true, false, (void *)prf);
}

static void output_chans_choice(prefs_info *prf)
{
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
    {
      int which;
      which = get_user_int_data(G_OBJECT(prf->radio_button));
      set_default_output_chans(output_chans[which]);
    }
}

static void output_srate_choice(prefs_info *prf)
{
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
    {
      int which;
      which = get_user_int_data(G_OBJECT(prf->radio_button));
      set_default_output_srate(output_srates[which]);
    }
}

static const char *output_type_choices[5] = {"aifc", "wave", "next/sun", "nist", "aiff"};
static int output_types[5] = {MUS_AIFC, MUS_RIFF, MUS_NEXT, MUS_NIST, MUS_AIFF};

static const char *output_format_choices[4] = {"short", "int", "float", "double"};
static int output_formats[4] = {MUS_LSHORT, MUS_LINT, MUS_LFLOAT, MUS_LDOUBLE};

static void reflect_output_type(prefs_info *prf)
{
  int which = -1;
  switch (default_output_header_type(ss))
    {
    case MUS_AIFC: which = 0; break;
    case MUS_AIFF: which = 4; break;
    case MUS_RIFF: which = 1; break;
    case MUS_NEXT: which = 2; break;
    case MUS_NIST: which = 3; break;
    }
  if (which != -1)
    set_toggle_button(prf->radio_buttons[which], true, false, (void *)prf);
}

static void reflect_output_format(prefs_info *prf)
{
  int which = -1;
  switch (default_output_data_format(ss))
    {
    case MUS_LINT: case MUS_BINT: which = 1; break;
    case MUS_LSHORT: case MUS_BSHORT: which = 0; break;
    case MUS_LFLOAT: case MUS_BFLOAT: which = 2; break;
    case MUS_LDOUBLE: case MUS_BDOUBLE: which = 3; break;
    }
  if (which != -1)
    set_toggle_button(prf->radio_buttons[which], true, false, (void *)prf);
}

static prefs_info *output_data_format_prf = NULL, *output_header_type_prf = NULL;

static void output_type_choice(prefs_info *prf)
{
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
    {
      set_default_output_header_type(output_types[get_user_int_data(G_OBJECT(prf->radio_button))]);
      set_default_output_data_format(header_to_data(default_output_header_type(ss), default_output_data_format(ss)));
      reflect_output_format(output_data_format_prf);
    }
}

static void output_format_choice(prefs_info *prf)
{
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
    {
      int which = -1;
      which = get_user_int_data(G_OBJECT(prf->radio_button));
      set_default_output_data_format(output_formats[which]);

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

/* ---------------- raw sound defaults ---------------- */

static void reflect_raw_chans(prefs_info *prf)
{
  int srate = 0, chans = 0, format = 0;
  mus_header_raw_defaults(&srate, &chans, &format);
  int_to_textfield(prf->text, chans);
}

static void raw_chans_choice(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if (str)
    {
      int srate = 0, chans = 0, format = 0;
      mus_header_raw_defaults(&srate, &chans, &format);
      sscanf(str, "%d", &chans);
      if (chans > 0)
	mus_header_set_raw_defaults(srate, chans, format);
      else reflect_raw_chans(prf);
    }
}

static void reflect_raw_srate(prefs_info *prf)
{
  int srate = 0, chans = 0, format = 0;
  mus_header_raw_defaults(&srate, &chans, &format);
  int_to_textfield(prf->text, srate);
}

static void raw_srate_choice(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if (str)
    {
      int srate = 0, chans = 0, format = 0;
      mus_header_raw_defaults(&srate, &chans, &format);
      sscanf(str, "%d", &srate);
      if (srate > 0)
	mus_header_set_raw_defaults(srate, chans, format);
      else reflect_raw_srate(prf);
    }
}

static void reflect_raw_data_format(prefs_info *prf)
{
  int srate = 0, chans = 0, format = 0;
  char *str;
  mus_header_raw_defaults(&srate, &chans, &format);
  str = raw_data_format_to_string(format);
  sg_entry_set_text(GTK_ENTRY(GTK_BIN(prf->text)->child), str);
  FREE(str);
}

static char **raw_data_format_choices = NULL;

static void raw_data_format_from_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(GTK_BIN(prf->text)->child));
  if (str)
    {
      int i, srate = 0, chans = 0, format = 0;
      mus_header_raw_defaults(&srate, &chans, &format);
      for (i = 0; i < MUS_NUM_DATA_FORMATS - 1; i++)
	if (STRCMP(raw_data_format_choices[i], str) == 0)
	  {
	    mus_header_set_raw_defaults(srate, chans, i + 1); /* skipping MUS_UNKNOWN = 0 */
	    return;
	  }
    }
  reflect_raw_data_format(prf);
}


/* ---------------- context sensitive popup ---------------- */

static bool include_context_sensitive_popup = false;

static void save_context_sensitive_popup(prefs_info *prf, FILE *fd)
{
  if (include_context_sensitive_popup) save_context_sensitive_popup_1(prf, fd);
}

static void context_sensitive_popup_toggle(prefs_info *prf)
{
  include_context_sensitive_popup = (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

static void reflect_context_sensitive_popup(prefs_info *prf) 
{
  set_toggle_button(prf->toggle, find_context_sensitive_popup(), false, (void *)prf);
}

/* ---------------- effects menu ---------------- */

static bool include_effects_menu = false;

static void save_effects_menu(prefs_info *prf, FILE *fd)
{
  if (include_effects_menu) save_effects_menu_1(prf, fd);
}

static void effects_menu_toggle(prefs_info *prf)
{
  include_effects_menu = (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

static void reflect_effects_menu(prefs_info *prf) 
{
  set_toggle_button(prf->toggle, find_effects_menu(), false, (void *)prf);
}

#if HAVE_SCHEME
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

static void reflect_edit_menu(prefs_info *prf) 
{
  set_toggle_button(prf->toggle, find_edit_menu(), false, (void *)prf);
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
  include_marks_menu = (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

static void reflect_marks_menu(prefs_info *prf) 
{
  set_toggle_button(prf->toggle, find_marks_menu(), false, (void *)prf);
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
  include_mix_menu = (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

static void reflect_mix_menu(prefs_info *prf) 
{
  set_toggle_button(prf->toggle, find_mix_menu(), false, (void *)prf);
}
#endif

/* ---------------- reopen menu ---------------- */

static bool include_reopen_menu = false;

static void save_reopen_menu(prefs_info *prf, FILE *fd)
{
  if (include_reopen_menu) save_reopen_menu_1(prf, fd);
}

static void reopen_menu_toggle(prefs_info *prf)
{
  include_reopen_menu = (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

static void reflect_reopen_menu(prefs_info *prf) 
{
  set_toggle_button(prf->toggle, find_reopen_menu(), false, (void *)prf);
}


/* ---------------- graph-style ---------------- */

static const char *graph_styles[NUM_GRAPH_STYLES] = {"line", "dot", "filled", "dot+line", "lollipop"};

static void reflect_graph_style(prefs_info *prf)
{
  set_toggle_button(prf->radio_buttons[(int)graph_style(ss)], true, false, (void *)prf);
}

static void graph_style_choice(prefs_info *prf)
{
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
    in_set_graph_style((graph_style_t)get_user_int_data(G_OBJECT(prf->radio_button)));
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

static void reflect_initial_bounds(prefs_info *prf)
{
  /* text has beg : dur, toggle true if full dur */
  char *str;
  str = initial_bounds_to_string();
  sg_entry_set_text(GTK_ENTRY(prf->text), str);
  FREE(str);
  set_toggle_button(prf->toggle, use_full_duration(), false, (void *)prf);
}

static void initial_bounds_toggle(prefs_info *prf)
{
  bool use_full_duration = false;
  use_full_duration = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle));
#if HAVE_SCHEME
  if (!(XEN_DEFINED_P("prefs-initial-beg")))
    XEN_LOAD_FILE_WITH_PATH("extensions.scm");
  XEN_VARIABLE_SET(XEN_NAME_AS_C_STRING_TO_VARIABLE("prefs-show-full-duration"), C_TO_XEN_BOOLEAN(use_full_duration));
#endif
#if HAVE_RUBY
  if (!(XEN_DEFINED_P("prefs-initial-beg")))
    XEN_LOAD_FILE_WITH_PATH("extensions.rb");
  XEN_VARIABLE_SET("prefs-show-full-duration", C_TO_XEN_BOOLEAN(use_full_duration));
#endif
}

static void initial_bounds_text(prefs_info *prf)
{
  float beg = 0.0, dur = 0.1;
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  sscanf(str, "%f : %f", &beg, &dur);
  fprintf(stderr, "beg: %f, dur: %f\n", beg, dur);
#if HAVE_SCHEME
  if (!(XEN_DEFINED_P("prefs-initial-beg")))
    XEN_LOAD_FILE_WITH_PATH("extensions.scm");
  XEN_VARIABLE_SET(XEN_NAME_AS_C_STRING_TO_VARIABLE("prefs-initial-beg"), C_TO_XEN_DOUBLE(beg));
  XEN_VARIABLE_SET(XEN_NAME_AS_C_STRING_TO_VARIABLE("prefs-initial-dur"), C_TO_XEN_DOUBLE(dur));
#endif
#if HAVE_RUBY
   if (!(XEN_DEFINED_P("prefs-initial-beg")))
     XEN_LOAD_FILE_WITH_PATH("extensions.rb");
   XEN_VARIABLE_SET("prefs-initial-beg", C_TO_XEN_DOUBLE(beg));
   XEN_VARIABLE_SET("prefs-initial-dur", C_TO_XEN_DOUBLE(dur));
#endif
}


/* ---------------- channel-style ---------------- */

static const char *channel_styles[NUM_CHANNEL_STYLES] = {"separate", "combined", "superimposed"};

static void reflect_channel_style(prefs_info *prf)
{
  set_toggle_button(prf->radio_buttons[(int)channel_style(ss)], true, false, (void *)prf);
}

static void channel_style_choice(prefs_info *prf)
{
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
    in_set_channel_style((channel_style_t)get_user_int_data(G_OBJECT(prf->radio_button)));
}

/* ---------------- graphs-horizontal ---------------- */

static void reflect_graphs_horizontal(prefs_info *prf) 
{
  set_toggle_button(prf->toggle, graphs_horizontal(ss), false, (void *)prf);
}

static void graphs_horizontal_toggle(prefs_info *prf)
{
  in_set_graphs_horizontal(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

/* ---------------- show-y-zero ---------------- */

static void reflect_show_y_zero(prefs_info *prf) 
{
  set_toggle_button(prf->toggle, show_y_zero(ss), false, (void *)prf);
}

static void y_zero_toggle(prefs_info *prf)
{
  in_set_show_y_zero(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

/* ---------------- show-grid ---------------- */

static void reflect_show_grid(prefs_info *prf) 
{
  set_toggle_button(prf->toggle, show_grid(ss), false, (void *)prf);
}

static void grid_toggle(prefs_info *prf)
{
  in_set_show_grid((gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle))) ? WITH_GRID : NO_GRID);
}

/* ---------------- grid-density ---------------- */

static void reflect_grid_density(prefs_info *prf)
{
  gtk_adjustment_set_value(GTK_ADJUSTMENT(prf->adj), grid_density(ss) / prf->scale_max);
  float_to_textfield(prf->text, grid_density(ss));
}

static void grid_density_scale_callback(prefs_info *prf)
{
  in_set_grid_density(GTK_ADJUSTMENT(prf->adj)->value * prf->scale_max);
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
	  gtk_adjustment_set_value(GTK_ADJUSTMENT(prf->adj), value / prf->scale_max);
	}
      else sg_entry_set_text(GTK_ENTRY(prf->text), "must be >= 0.0");

    }
}

/* ---------------- show-axes ---------------- */

static const char *show_axes_choices[NUM_SHOW_AXES] = {"none", "X and Y", "just X", "X and Y unlabelled", "just X unlabelled"};

static void reflect_show_axes(prefs_info *prf)
{
  sg_entry_set_text(GTK_ENTRY(GTK_BIN(prf->text)->child), (char *)show_axes_choices[(int)show_axes(ss)]);
}

static void show_axes_from_text(prefs_info *prf)
{
  int i;
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(GTK_BIN(prf->text)->child));
  if ((str) && (*str))
    {
      char *trimmed_str;
      trimmed_str = trim_string(str);
      if (snd_strlen(trimmed_str) > 0)
	{
	  int curpos = -1;
	  for (i = 0; i < NUM_SHOW_AXES; i++)
	    if (STRCMP(trimmed_str, show_axes_choices[i]) == 0)
	      {
		curpos = i;
		break;
	      }
	  if (curpos >= 0)
	    in_set_show_axes((show_axes_t)curpos);
	  else post_prefs_error("unknown axis choice", (void *)prf);
	}
      else post_prefs_error("need an axis choice", (void *)prf);
      FREE(trimmed_str);
    }
  else post_prefs_error("need an axis choice", (void *)prf);
}

/* ---------------- x-axis-style ---------------- */

static const char *x_axis_styles[NUM_X_AXIS_STYLES] = {"seconds", "samples", "% of total", "beats", "measures", "clock"};

static void reflect_x_axis_style(prefs_info *prf)
{
  sg_entry_set_text(GTK_ENTRY(GTK_BIN(prf->text)->child), (char *)x_axis_styles[(int)x_axis_style(ss)]);
}

static void x_axis_style_from_text(prefs_info *prf)
{
  int i;
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(GTK_BIN(prf->text)->child));
  if ((str) && (*str))
    {
      char *trimmed_str;
      trimmed_str = trim_string(str);
      if (snd_strlen(trimmed_str) > 0)
	{
	  int curpos = -1;
	  for (i = 0; i < NUM_X_AXIS_STYLES; i++)
	    if (STRCMP(trimmed_str, x_axis_styles[i]) == 0)
	      {
		curpos = i;
		break;
	      }
	  if (curpos >= 0)
	    in_set_x_axis_style((x_axis_style_t)curpos);
	  else post_prefs_error("unknown axis style", (void *)prf);
	}
      else post_prefs_error("need an axis style", (void *)prf);
      FREE(trimmed_str);
    }
  else post_prefs_error("need an axis style", (void *)prf);
}

/* ---------------- smpte ---------------- */

static bool include_smpte = false;

static void reflect_smpte(prefs_info *prf) 
{
  set_toggle_button(prf->toggle, find_smpte(), false, (void *)prf);
}

static void smpte_toggle(prefs_info *prf)
{
  include_smpte = (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

static void save_smpte(prefs_info *prf, FILE *fd)
{
  if (include_smpte) save_smpte_1(prf, fd);
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
  set_data_color(rgb_to_color(r, g, b));
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
  set_graph_color(rgb_to_color(r, g, b));
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
  set_selected_data_color(rgb_to_color(r, g, b));
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
  set_selected_graph_color(rgb_to_color(r, g, b));
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
  set_selection_color(rgb_to_color(r, g, b));
}


/* ---------------- axis-label-font ---------------- */

static gint axis_label_font_error_erase_func(gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  sg_entry_set_text(GTK_ENTRY(prf->text), axis_label_font(ss));
  return(0);
}

static void reflect_axis_label_font(prefs_info *prf)
{
  sg_entry_set_text(GTK_ENTRY(prf->text), axis_label_font(ss));
}

static void axis_label_font_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((!str) || (!(*str)))
    {
      sg_entry_set_text(GTK_ENTRY(prf->text), axis_label_font(ss));
      return;
    }
  if (!(set_axis_label_font(str)))
    {
      sg_entry_set_text(GTK_ENTRY(prf->text), "can't find that font");
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
  sg_entry_set_text(GTK_ENTRY(prf->text), axis_numbers_font(ss));
  return(0);
}

static void reflect_axis_numbers_font(prefs_info *prf)
{
  sg_entry_set_text(GTK_ENTRY(prf->text), axis_numbers_font(ss));
}

static void axis_numbers_font_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((!str) || (!(*str)))
    {
      sg_entry_set_text(GTK_ENTRY(prf->text), axis_numbers_font(ss));
      return;
    }
  if (!(set_axis_numbers_font(str)))
    {
      sg_entry_set_text(GTK_ENTRY(prf->text), "can't find that font");
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
  sg_entry_set_text(GTK_ENTRY(prf->text), peaks_font(ss));
  return(0);
}

static void reflect_peaks_font(prefs_info *prf)
{
  sg_entry_set_text(GTK_ENTRY(prf->text), peaks_font(ss));
}

static void peaks_font_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((!str) || (!(*str)))
    {
      sg_entry_set_text(GTK_ENTRY(prf->text), peaks_font(ss));
      return;
    }
  if (!(set_peaks_font(str)))
    {
      sg_entry_set_text(GTK_ENTRY(prf->text), "can't find that font");
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
  sg_entry_set_text(GTK_ENTRY(prf->text), bold_peaks_font(ss));
  return(0);
}

static void reflect_bold_peaks_font(prefs_info *prf)
{
  sg_entry_set_text(GTK_ENTRY(prf->text), bold_peaks_font(ss));
}

static void bold_peaks_font_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((!str) || (!(*str)))
    {
      sg_entry_set_text(GTK_ENTRY(prf->text), bold_peaks_font(ss));
      return;
    }
  if (!(set_bold_peaks_font(str)))
    {
      sg_entry_set_text(GTK_ENTRY(prf->text), "can't find that font");
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
  sg_entry_set_text(GTK_ENTRY(prf->text), tiny_font(ss));
  return(0);
}

static void reflect_tiny_font(prefs_info *prf)
{
  sg_entry_set_text(GTK_ENTRY(prf->text), tiny_font(ss));
}

static void tiny_font_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((!str) || (!(*str)))
    {
      sg_entry_set_text(GTK_ENTRY(prf->text), tiny_font(ss));
      return;
    }
  if (!(set_tiny_font(str)))
    {
      sg_entry_set_text(GTK_ENTRY(prf->text), "can't find that font");
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
  sg_entry_set_text(GTK_ENTRY(prf->text), new_size);
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
  set_toggle_button(prf->radio_buttons[(int)transform_graph_type(ss)], true, false, (void *)prf);  
}

static void transform_graph_type_choice(prefs_info *prf)
{
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
    in_set_transform_graph_type((graph_type_t)get_user_int_data(G_OBJECT(prf->radio_button)));
}


/* ---------------- transform-type ---------------- */

static const char *transform_types[NUM_BUILTIN_TRANSFORM_TYPES] = {"Fourier", "Wavelet", "Walsh", "Autocorrelate", "Cepstrum", "Haar"};

static list_completer_info *transform_type_completer_info = NULL;

static void reflect_transform_type(prefs_info *prf)
{
  sg_entry_set_text(GTK_ENTRY(GTK_BIN(prf->text)->child), 
		     (char *)transform_types[mus_iclamp(0, transform_type(ss), NUM_BUILTIN_TRANSFORM_TYPES - 1)]);
}

static char *transform_type_completer(char *text, void *data)
{
  if (!transform_type_completer_info)
    {
      transform_type_completer_info = (list_completer_info *)CALLOC(1, sizeof(list_completer_info));
      transform_type_completer_info->exact_match = false;
      transform_type_completer_info->values = (char **)transform_types;
      transform_type_completer_info->num_values = NUM_BUILTIN_TRANSFORM_TYPES;
      transform_type_completer_info->values_size = NUM_BUILTIN_TRANSFORM_TYPES;
    }
  return(list_completer(text, (void *)transform_type_completer_info));
}

static void transform_type_from_text(prefs_info *prf)
{
  int i;
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(GTK_BIN(prf->text)->child));
  if ((str) && (*str))
    {
      char *trimmed_str;
      trimmed_str = trim_string(str);
      if (snd_strlen(trimmed_str) > 0)
	{
	  int curpos = -1;
	  for (i = 0; i < NUM_BUILTIN_TRANSFORM_TYPES; i++)
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

static const char *fft_windows[MUS_NUM_WINDOWS] = 
  {"Rectangular", "Hann", "Welch", "Parzen", "Bartlett", "Hamming", "Blackman2", "Blackman3", "Blackman4",
   "Exponential", "Riemann", "Kaiser", "Cauchy", "Poisson", "Gaussian", "Tukey", "Dolph-Chebyshev", "Hann-Poisson", "Connes",
   "Samaraki", "Ultraspherical"};

static list_completer_info *fft_window_completer_info = NULL;

static void reflect_fft_window(prefs_info *prf)
{
  sg_entry_set_text(GTK_ENTRY(GTK_BIN(prf->text)->child), 
		     (char *)fft_windows[(int)fft_window(ss)]);
}

static char *fft_window_completer(char *text, void *data)
{
  if (!fft_window_completer_info)
    {
      fft_window_completer_info = (list_completer_info *)CALLOC(1, sizeof(list_completer_info));
      fft_window_completer_info->exact_match = false;
      fft_window_completer_info->values = (char **)fft_windows;
      fft_window_completer_info->num_values = MUS_NUM_WINDOWS;
      fft_window_completer_info->values_size = MUS_NUM_WINDOWS;
    }
  return(list_completer(text, (void *)fft_window_completer_info));
}

static void fft_window_from_text(prefs_info *prf)
{
  int i;
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(GTK_BIN(prf->text)->child));
  if ((str) && (*str))
    {
      char *trimmed_str;
      trimmed_str = trim_string(str);
      if (snd_strlen(trimmed_str) > 0)
	{
	  int curpos = -1;
	  for (i = 0; i < MUS_NUM_WINDOWS; i++)
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
  gtk_adjustment_set_value(GTK_ADJUSTMENT(prf->adj), fft_window_beta(ss) / prf->scale_max);
  float_to_textfield(prf->text, fft_window_beta(ss));
}

static void fft_window_beta_scale_callback(prefs_info *prf)
{
  in_set_fft_window_beta(GTK_ADJUSTMENT(prf->adj)->value * prf->scale_max);
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
	  gtk_adjustment_set_value(GTK_ADJUSTMENT(prf->adj), value / prf->scale_max);
	}
      else sg_entry_set_text(GTK_ENTRY(prf->text), "must be >= 0.0");
    }
}


/* ---------------- show-transform-peaks ---------------- */

static void reflect_show_transform_peaks(prefs_info *prf) 
{
  set_toggle_button(prf->toggle, show_transform_peaks(ss), false, (void *)prf);
}

static void transform_peaks_toggle(prefs_info *prf)
{
  in_set_show_transform_peaks(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

static void max_peaks_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      int value = 0;
      sscanf(str, "%d", &value);
      if (value >= 0)
	in_set_max_transform_peaks(value);
      else int_to_textfield(prf->text, max_transform_peaks(ss));
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
  sg_entry_set_text(GTK_ENTRY(GTK_BIN(prf->text)->child), 
		    colormap_name(color_map(ss)));
}

static void colormap_from_text(prefs_info *prf)
{
  int i;
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(GTK_BIN(prf->text)->child));
  if ((str) && (*str))
    {
      char *trimmed_str;
      trimmed_str = trim_string(str);
      if (snd_strlen(trimmed_str) > 0)
	{
	  int len, curpos = -1;
	  len = num_colormaps();
	  for (i = 0; i < len; i++)
	    if ((colormap_name(i)) &&
		(STRCMP(trimmed_str, colormap_name(i)) == 0))
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


/* ---------------- fft-log-magnitude ---------------- */

static void reflect_fft_log_magnitude(prefs_info *prf) 
{
  set_toggle_button(prf->toggle, fft_log_magnitude(ss), false, (void *)prf);
}

static void log_magnitude_toggle(prefs_info *prf)
{
  in_set_fft_log_magnitude(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

/* ---------------- min-dB ---------------- */

static void reflect_min_dB(prefs_info *prf)
{
  float_1_to_textfield(prf->text, min_dB(ss));
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
  set_toggle_button(prf->toggle, fft_log_frequency(ss), false, (void *)prf);
}

static void log_frequency_toggle(prefs_info *prf)
{
  in_set_fft_log_frequency(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

/* ---------------- transform-normalization ---------------- */

static const char *transform_normalizations[4] = {"none", "by channel", "by sound", "global"};

static void reflect_transform_normalization(prefs_info *prf)
{
  set_toggle_button(prf->radio_buttons[(int)transform_normalization(ss)], true, false, (void *)prf);
}

static void transform_normalization_choice(prefs_info *prf)
{
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
    in_set_transform_normalization((fft_normalize_t)get_user_int_data(G_OBJECT(prf->radio_button)));
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
  color_marks(rgb_to_color(r, g, b));
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
  sg_entry_set_text(GTK_ENTRY(prf->text), "must be > 0");
  g_timeout_add_full(0,
		     ERROR_WAIT_TIME,
		     mark_tag_width_erase_func,
		     (gpointer)prf, NULL);
}

static void mark_tag_height_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  sg_entry_set_text(GTK_ENTRY(prf->rtxt), "must be > 0");
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
  color_mixes(rgb_to_color(r, g, b));
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
  sg_entry_set_text(GTK_ENTRY(prf->text), "must be > 0");
  g_timeout_add_full(0,
		  ERROR_WAIT_TIME,
		  mix_tag_width_erase_func,
		     (gpointer)prf, NULL);
}

static void mix_tag_height_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  sg_entry_set_text(GTK_ENTRY(prf->rtxt), "must be > 0");
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
  set_toggle_button(prf->toggle, show_mix_waveforms(ss), false, (void *)prf);
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
      else int_to_textfield(prf->text, mix_waveform_height(ss));
    }
}

/* ---------------- include with-sound ---------------- */

static bool include_with_sound = false;
static char *include_clm_file_name = NULL;
static int include_clm_file_buffer_size = 65536;
static int include_clm_table_size = 512;

static bool with_sound_is_loaded(void)
{
  return(XEN_DEFINED_P("with-sound"));
}

static void reflect_with_sound(prefs_info *prf) 
{
  include_with_sound = with_sound_is_loaded();
  set_toggle_button(prf->toggle, include_with_sound, false, (void *)prf);
}

static void with_sound_toggle(prefs_info *prf)
{
  include_with_sound = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle));
}

static void save_with_sound(prefs_info *prf, FILE *fd)
{
  if (include_with_sound)
    {
#if HAVE_SCHEME
      fprintf(fd, "(if (not (provided? 'snd-ws.scm)) (load-from-path \"ws.scm\"))\n");
      if (include_clm_file_name)
	fprintf(fd, "(set! *clm-file-name* \"%s\")\n", include_clm_file_name);
      if (include_clm_file_buffer_size != 65536)
	fprintf(fd, "(set! *clm-file-buffer-size* %d)\n", include_clm_file_buffer_size);
      if (include_clm_table_size != 512)
	fprintf(fd, "(set! *clm-table-size* %d)\n", include_clm_table_size);
#endif
#if HAVE_RUBY
      fprintf(fd, "require \"ws\"\n");
      if (include_clm_file_name)
	fprintf(fd, "$clm_file_name = \"%s\"\n", include_clm_file_name);
      if (include_clm_file_buffer_size != 65536)
	fprintf(fd, "$clm_file_buffer_size = %d\n", include_clm_file_buffer_size);
      if (include_clm_table_size != 512)
	fprintf(fd, "$clm_table_size = %d\n", include_clm_table_size);
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
  set_sensitive(prf->arrow_down, (speed_control_tones(ss) > MIN_SPEED_CONTROL_SEMITONES));
}

static void speed_control_up(prefs_info *prf)
{
  in_set_speed_control_tones(ss, speed_control_tones(ss) + 1);
  show_speed_control_semitones(prf);
}

static void speed_control_down(prefs_info *prf)
{
  in_set_speed_control_tones(ss, speed_control_tones(ss) - 1);
  show_speed_control_semitones(prf);
}

static void speed_control_text(prefs_info *prf)
{
  int tones;
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      prf->got_error = false;
      redirect_errors_to(post_prefs_error, (void *)prf);
      tones = string_to_int(str, MIN_SPEED_CONTROL_SEMITONES, "semitones");
      redirect_errors_to(NULL, NULL);
      if (!(prf->got_error))
	{
	  in_set_speed_control_tones(ss, tones);
	  set_sensitive(prf->arrow_down, (speed_control_tones(ss) > MIN_SPEED_CONTROL_SEMITONES));
	}
      else prf->got_error = false;
    }
}

static void reflect_speed_control(prefs_info *prf)
{
  set_toggle_button(prf->radio_buttons[(int)speed_control_style(ss)], true, false, (void *)prf);
}

static void speed_control_choice(prefs_info *prf)
{
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
    in_set_speed_control_style(ss, (speed_style_t)get_user_int_data(G_OBJECT(prf->radio_button)));
}

#if HAVE_SCHEME
/* ---------------- hidden controls dialog ---------------- */

static bool include_hidden_controls = false;

static void save_hidden_controls(prefs_info *prf, FILE *fd)
{
  if (include_hidden_controls)
    {
      fprintf(fd, "(if (not (provided? 'snd-snd-motif.scm)) (load-from-path \"snd-motif.scm\"))\n");
      fprintf(fd, "(make-hidden-controls-dialog)\n");
    }
}

static void hidden_controls_toggle(prefs_info *prf)
{
  include_hidden_controls = (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

static void reflect_hidden_controls(prefs_info *prf) 
{
  set_toggle_button(prf->toggle, find_hidden_controls(), false, (void *)prf);
}
#endif

/* ---------------- sinc width ---------------- */

static void sinc_width_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      int value = 0;
      sscanf(str, "%d", &value);
      if (value >= 0)
	set_sinc_width(value);
      else int_to_textfield(prf->text, sinc_width(ss));
    }
}

static void reflect_sinc_width(prefs_info *prf)
{
  int_to_textfield(prf->text, sinc_width(ss));
}


/* ---------------- clm file name ---------------- */

static void clm_file_name_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      include_with_sound = true;
      if (include_clm_file_name) FREE(include_clm_file_name); /* save is done after we're sure with-sound is loaded */
      include_clm_file_name = copy_string(str);
      set_clm_file_name(str);
    }
}

static void reflect_clm_file_name(prefs_info *prf)
{
  sg_entry_set_text(GTK_ENTRY(prf->text), clm_file_name());
}

/* ---------------- clm sizes ---------------- */

static void reflect_clm_sizes(prefs_info *prf)
{
  include_clm_table_size = clm_table_size();
  int_to_textfield(prf->text, include_clm_table_size);
  include_clm_file_buffer_size = clm_file_buffer_size();
  int_to_textfield(prf->rtxt, include_clm_file_buffer_size);
}

static void clm_sizes_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if (str)
    {
      int size = 0;
      include_with_sound = true;
      sscanf(str, "%d", &size);
      if (size > 0)
	include_clm_table_size = size;
      else int_to_textfield(prf->text, include_clm_table_size);
    }
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->rtxt));
  if ((str) && (*str))
    {
      int size = 0;
      include_with_sound = true;
      sscanf(str, "%d", &size);
      if (size > 0)
	include_clm_file_buffer_size = size;
      else int_to_textfield(prf->rtxt, include_clm_file_buffer_size);
    }
}

/* ---------------- show-listener ---------------- */

static bool include_listener = false;

static void reflect_show_listener(prefs_info *prf) 
{
  include_listener = listener_is_visible();
  set_toggle_button(prf->toggle, include_listener, false, (void *)prf);
}

static void show_listener_toggle(prefs_info *prf)
{
  include_listener = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle));
}

static void save_show_listener(prefs_info *prf, FILE *fd)
{
  if (include_listener) save_show_listener_1(prf, fd);
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
  sg_entry_set_text(GTK_ENTRY(prf->text), listener_prompt(ss));
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
  set_toggle_button(prf->toggle, show_backtrace(ss), false, (void *)prf);
}

static void show_backtrace_toggle(prefs_info *prf)
{
  set_show_backtrace(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

#if HAVE_GUILE
/* ---------------- debugging aids ---------------- */

static bool include_debugging_aids = false;

static void reflect_debugging_aids(prefs_info *prf) 
{
  set_toggle_button(prf->toggle, find_debugging_aids(), false, (void *)prf);
}

static void debugging_aids_toggle(prefs_info *prf)
{
  include_debugging_aids = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle));
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
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      int value = 0;
      sscanf(str, "%d", &value);
      set_print_length(value);
    }
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
  color_listener(rgb_to_color(r, g, b));
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
  color_listener_text(rgb_to_color(r, g, b));
}

/* ---------------- listener-font ---------------- */

static gint listener_font_error_erase_func(gpointer context)
{
  prefs_info *prf = (prefs_info *)context;
  sg_entry_set_text(GTK_ENTRY(prf->text), listener_font(ss));
  return(0);
}

static void reflect_listener_font(prefs_info *prf)
{
  sg_entry_set_text(GTK_ENTRY(prf->text), listener_font(ss));
}

static void listener_font_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((!str) || (!(*str)))
    {
      sg_entry_set_text(GTK_ENTRY(prf->text), listener_font(ss));
      return;
    }
  if (!(set_listener_font(str)))
    {
      sg_entry_set_text(GTK_ENTRY(prf->text), "can't find that font");
      g_timeout_add_full(0,
		      ERROR_WAIT_TIME,
		      listener_font_error_erase_func,
			 (gpointer)prf, NULL);
    }

}

/* ---------------- dac-size ---------------- */

static void reflect_dac_size(prefs_info *prf) 
{
  int_to_textfield(prf->text, dac_size(ss));
}

static void dac_size_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      int value = 0;
      sscanf(str, "%d", &value);
      if (value > 0)
	set_dac_size(value);
      else int_to_textfield(prf->text, dac_size(ss));
    }
}

/* ---------------- dac-combines-channels ---------------- */

static void reflect_dac_combines_channels(prefs_info *prf) 
{
  set_toggle_button(prf->toggle, dac_combines_channels(ss), false, (void *)prf);
}

static void dac_combines_channels_toggle(prefs_info *prf)
{
  set_dac_combines_channels(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

/* ---------------- recorder file name ---------------- */

static void recorder_filename_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    rec_set_filename(str);
}

static void reflect_recorder_filename(prefs_info *prf)
{
  sg_entry_set_text(GTK_ENTRY(prf->text), rec_filename());
}

/* ---------------- recorder-autoload ---------------- */

static void reflect_recorder_autoload(prefs_info *prf) 
{
  set_toggle_button(prf->toggle, rec_autoload(), false, (void *)prf);
}

static void recorder_autoload_toggle(prefs_info *prf)
{
  rec_set_autoload(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->toggle)));
}

/* ---------------- recorder-buffer-size ---------------- */

static void reflect_recorder_buffer_size(prefs_info *prf) 
{
  int_to_textfield(prf->text, rec_buffer_size());
}

static void recorder_buffer_size_text(prefs_info *prf)
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(prf->text));
  if ((str) && (*str))
    {
      int value = 0;
      sscanf(str, "%d", &value);
      if (value > 0)
	rec_set_buffer_size(value);
      else int_to_textfield(prf->text, rec_buffer_size());
    }
}

/* ---------------- recorder-out-chans etc ---------------- */

static const char *recorder_out_chans_choices[4] = {"1", "2", "4", "8"};
static const char *recorder_srate_choices[5] = {"8000", "22050", "44100", "48000", "96000"};

static void reflect_recorder_out_chans(prefs_info *prf)
{
  int which = -1;
  switch (rec_output_chans())
    {
    case 1: which = 0; break;
    case 2: which = 1; break;
    case 4: which = 2; break;
    case 8: which = 3; break;
    }
  if (which != -1)
    set_toggle_button(prf->radio_buttons[which], true, false, (void *)prf);
}

static void reflect_recorder_srate(prefs_info *prf)
{
  int which = -1, sr;
  sr = rec_srate();
  if (sr == 8000) which = 0; else
  if (sr == 22050) which = 1; else
  if (sr == 44100) which = 2; else
  if (sr == 48000) which = 3; else
  if (sr == 96000) which = 4;
  if (which != -1)
    set_toggle_button(prf->radio_buttons[which], true, false, (void *)prf);
}

static void recorder_out_chans_choice(prefs_info *prf)
{
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
    {
      int which;
      which = get_user_int_data(G_OBJECT(prf->radio_button));
      rec_set_output_chans(output_chans[which]);
    }
}

static void recorder_srate_choice(prefs_info *prf)
{
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
    {
      int which;
      which = get_user_int_data(G_OBJECT(prf->radio_button));
      rec_set_srate(output_srates[which]);
    }
}

static const char *recorder_out_type_choices[5] = {"aifc", "wave", "next/sun", "nist", "aiff"};
static int recorder_types[5] = {MUS_AIFC, MUS_RIFF, MUS_NEXT, MUS_NIST, MUS_AIFF};

static const char *recorder_out_format_choices[4] = {"short", "int", "float", "double"};
static int recorder_formats[4] = {MUS_LSHORT, MUS_LINT, MUS_LFLOAT, MUS_LDOUBLE};

static void reflect_recorder_out_type(prefs_info *prf)
{
  int which = -1;
  switch (rec_output_header_type())
    {
    case MUS_AIFC: which = 0; break;
    case MUS_AIFF: which = 4; break;
    case MUS_RIFF: which = 1; break;
    case MUS_NEXT: which = 2; break;
    case MUS_NIST: which = 3; break;
    }
  if (which != -1)
    set_toggle_button(prf->radio_buttons[which], true, false, (void *)prf);
}

static void reflect_recorder_out_format(prefs_info *prf)
{
  int which = -1;
  switch (rec_output_data_format())
    {
    case MUS_LINT: case MUS_BINT: which = 1; break;
    case MUS_LSHORT: case MUS_BSHORT: which = 0; break;
    case MUS_LFLOAT: case MUS_BFLOAT: which = 2; break;
    case MUS_LDOUBLE: case MUS_BDOUBLE: which = 3; break;
    }
  if (which != -1)
    set_toggle_button(prf->radio_buttons[which], true, false, (void *)prf);
}

static prefs_info *recorder_out_data_format_prf = NULL, *recorder_out_header_type_prf = NULL;

static void recorder_out_type_choice(prefs_info *prf)
{
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
    {
      rec_set_output_header_type(recorder_types[get_user_int_data(G_OBJECT(prf->radio_button))]);
      rec_set_output_data_format(header_to_data(rec_output_header_type(), rec_output_data_format()));
      reflect_recorder_out_format(recorder_out_data_format_prf);
    }
}

static void recorder_out_format_choice(prefs_info *prf)
{
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(prf->radio_button)))
    {
      int which = -1;
      which = get_user_int_data(G_OBJECT(prf->radio_button));
      rec_set_output_data_format(recorder_formats[which]);
      switch (rec_output_data_format())
	{
	case MUS_LSHORT:
	  switch (rec_output_header_type())
	    {
	    case MUS_AIFC: case MUS_AIFF: case MUS_NEXT: 
	      rec_set_output_data_format(MUS_BSHORT); 
	      break;
	    }
	  break;
	  
	case MUS_LINT:
	  switch (rec_output_header_type())
	    {
	    case MUS_AIFC: case MUS_AIFF: case MUS_NEXT: 
	      rec_set_output_data_format(MUS_BINT); 
	      break;
	    }
	  break;
	case MUS_LFLOAT:
	  switch (rec_output_header_type())
	    {
	    case MUS_AIFC: case MUS_NEXT: 
	      rec_set_output_data_format(MUS_BFLOAT); 
	      break;
	    case MUS_AIFF:
	      rec_set_output_header_type(MUS_AIFC);
	      rec_set_output_data_format(MUS_BFLOAT); 
	      break;
	    case MUS_NIST: 
	      rec_set_output_header_type(MUS_RIFF); 
	      break;
	    }
	  break;
	case MUS_LDOUBLE:
	  switch (rec_output_header_type())
	    {
	    case MUS_AIFC: case MUS_NEXT: 
	      rec_set_output_data_format(MUS_BDOUBLE); 
	      break;
	    case MUS_AIFF:
	      rec_set_output_header_type(MUS_AIFC);
	      rec_set_output_data_format(MUS_BDOUBLE); 
	      break;
	    case MUS_NIST: 
	      rec_set_output_header_type(MUS_RIFF); 
	      break;
	    }
	  break;
	}
      reflect_recorder_out_type(output_header_type_prf);
    }
}



/* ---------------- preferences dialog ---------------- */


widget_t start_preferences_dialog(void)
{
  GtkWidget *saveB, *resetB, *helpB, *dismissB, *topics, *scroller, *current_sep;
  prefs_info *prf;
  char *str;

  if (preferences_dialog)
    {
      gtk_widget_show(preferences_dialog);
      return(preferences_dialog);
    }

  preferences_dialog = snd_gtk_dialog_new();
  gtk_window_set_title(GTK_WINDOW(preferences_dialog), _("Preferences"));
  sg_make_resizable(preferences_dialog);
  /* gtk_container_set_border_width (GTK_CONTAINER(preferences_dialog), 10); */
  gtk_widget_realize(preferences_dialog);
  gtk_window_resize(GTK_WINDOW(preferences_dialog), STARTUP_WIDTH, STARTUP_HEIGHT);

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

  label_group = gtk_size_group_new(GTK_SIZE_GROUP_HORIZONTAL);
  widgets_group = gtk_size_group_new(GTK_SIZE_GROUP_HORIZONTAL);
  help_group = gtk_size_group_new(GTK_SIZE_GROUP_HORIZONTAL);

  rscl_color = rgb_to_color(1.0, 0.5, 0.5);
  gscl_color = rgb_to_color(0.5, 0.9, 0.5);
  bscl_color = rgb_to_color(0.7, 0.8, 0.9);


  /* ---------------- overall behavior ---------------- */

  {
    GtkWidget *dpy_box, *dpy_label, *file_label, *cursor_label, *key_label;
    char *str1, *str2;

    /* ---------------- overall behavior ----------------*/

    dpy_box = make_top_level_box(topics);
    dpy_label = make_top_level_label("overall behavior choices", dpy_box);

    str1 = mus_format("%d", ss->init_window_width);
    str2 = mus_format("%d", ss->init_window_height);
    prf = prefs_row_with_two_texts("start up size", S_window_width, 
				   "width:", str1, "height:", str2, 6,
				   dpy_box,
				   startup_size_text);
    remember_pref(prf, NULL, NULL); /* this is not reflected, and is saved via window-width|height */
    FREE(str2);
    FREE(str1);

    current_sep = make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_toggle("ask before overwriting anything", S_ask_before_overwrite,
				ask_before_overwrite(ss), 
				dpy_box,
				overwrite_toggle);
    remember_pref(prf, reflect_ask_before_overwrite, NULL);

    current_sep = make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_toggle("ask about unsaved edits before exiting", "check-for-unsaved-edits",
				unsaved_edits(), 
				dpy_box,
				unsaved_edits_toggle);
    remember_pref(prf, reflect_unsaved_edits, save_unsaved_edits);
    prf->help_func = unsaved_edits_help;

    current_sep = make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_toggle("include thumbnail graph in upper right corner", "make-current-window-display",
				find_current_window_display(),
				dpy_box,
				current_window_display_toggle);
    remember_pref(prf, reflect_current_window_display, save_current_window_display);
    prf->help_func = current_window_help;

    current_sep = make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_toggle("resize main window as sounds open and close", S_auto_resize,
				auto_resize(ss), 
				dpy_box, 
				resize_toggle);
    remember_pref(prf, reflect_auto_resize, NULL);

    current_sep = make_inter_variable_separator(dpy_box);
    focus_follows_mouse = focus_is_following_mouse();
    prf = prefs_row_with_toggle("focus follows mouse", "focus-follows-mouse",
				focus_follows_mouse,
				dpy_box,
				focus_follows_mouse_toggle);
    remember_pref(prf, reflect_focus_follows_mouse, save_focus_follows_mouse);
    prf->help_func = mouse_focus_help;

    current_sep = make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_two_toggles("operate on all channels together", S_sync,
				     "within each sound", find_sync_choice() == 1,
				     "across all sounds", find_sync_choice() == 2,
				     dpy_box,
				     sync1_choice, sync2_choice);
    remember_pref(prf, reflect_sync_choice, save_sync_choice);
    prf->help_func = sync_choice_help;

    current_sep = make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_two_toggles("restore a sound's state if reopened later", "remember-sound-state",
				     "within one run", find_remember_sound_state_choice() & 1,
				     "across runs", find_remember_sound_state_choice() & 2,
				     dpy_box,
				     remember_sound_state_1_choice, remember_sound_state_2_choice);
    remember_pref(prf, reflect_remember_sound_state_choice, save_remember_sound_state_choice);
    prf->help_func = remember_sound_state_choice_help;

    current_sep = make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_toggle("show the control panel upon opening a sound", S_show_controls,
				in_show_controls(ss), 
				dpy_box, 
				controls_toggle);
    remember_pref(prf, reflect_show_controls, NULL);

    current_sep = make_inter_variable_separator(dpy_box);
    include_peak_env_directory = copy_string(peak_env_directory());
    include_peak_envs = find_peak_envs();
    prf = prefs_row_with_toggle_with_text("save peak envs to speed up initial display", "save-peak-env-info",
					  include_peak_envs,
					  "directory:", include_peak_env_directory, 25,
					  dpy_box,
					  peak_envs_toggle, peak_envs_text);
    remember_pref(prf, reflect_peak_envs, save_peak_envs);
    prf->help_func = peak_env_help;

    current_sep = make_inter_variable_separator(dpy_box);
    str = mus_format("%d", max_regions(ss));
    prf = prefs_row_with_toggle_with_text("selection creates an associated region", S_selection_creates_region,
					  selection_creates_region(ss),
					  "max regions:", str, 5,
					  dpy_box,
					  selection_creates_region_toggle, max_regions_text);
    remember_pref(prf, reflect_selection_creates_region, NULL);
    FREE(str);


    /* ---------------- file options ---------------- */

    current_sep = make_inter_variable_separator(dpy_box);
    file_label = make_inner_label("  file options", dpy_box);

    str = find_sources();
    prf = prefs_row_with_text("directory containing Snd's " LANG_NAME " files", "load path", 
			      str,
			      dpy_box,
			      load_path_text);
    remember_pref(prf, reflect_load_path, NULL);
    prf->help_func = load_path_help;
    if (str) FREE(str);

    current_sep = make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_toggle("display only sound files in various file lists", S_just_sounds,
				just_sounds(ss), 
				dpy_box,
				just_sounds_toggle);
    remember_pref(prf, prefs_reflect_just_sounds, NULL);

    current_sep = make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_text("directory for temporary files", S_temp_dir, 
			      temp_dir(ss), 
			      dpy_box,
			      temp_dir_text);
    remember_pref(prf, reflect_temp_dir, NULL);

    current_sep = make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_text("directory for save-state files", S_save_dir, 
			      save_dir(ss), 
			      dpy_box,
			      save_dir_text);
    remember_pref(prf, reflect_save_dir, NULL);

    current_sep = make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_text("default save-state filename", S_save_state_file, 
			      save_state_file(ss), 
			      dpy_box,
			      save_state_file_text);
    remember_pref(prf, reflect_save_state_file, NULL);

#if HAVE_LADSPA
    current_sep = make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_text("directory for ladspa plugins", S_ladspa_dir, 
			      ladspa_dir(ss), 
			      dpy_box,
			      ladspa_dir_text);
    remember_pref(prf, reflect_ladspa_dir, NULL);
#endif

    current_sep = make_inter_variable_separator(dpy_box);
    include_vf_directory = copy_string(view_files_find_any_directory());
    prf = prefs_row_with_text("directory for view-files dialog", S_add_directory_to_view_files_list,
			      include_vf_directory,
			      dpy_box,
			      view_files_directory_text);
    remember_pref(prf, reflect_view_files_directory, save_view_files_directory);

    current_sep = make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_text("external program to read HTML files via snd-help", S_html_program,
			      html_program(ss),
			      dpy_box,
			      html_program_text);
    remember_pref(prf, reflect_html_program, NULL);
    current_sep = make_inter_variable_separator(dpy_box);

    prf = prefs_row_with_radio_box("default new sound attributes: chans", S_default_output_chans,
				   output_chan_choices, 4, -1,
				   dpy_box,
				   output_chans_choice);
    reflect_output_chans(prf);
    remember_pref(prf, reflect_output_chans, NULL);

    prf = prefs_row_with_radio_box("srate", S_default_output_srate,
				   output_srate_choices, 4, -1,
				   dpy_box,
				   output_srate_choice);
    reflect_output_srate(prf);
    remember_pref(prf, reflect_output_srate, NULL);

    prf = prefs_row_with_radio_box("header type", S_default_output_header_type,
				   output_type_choices, 5, -1,
				   dpy_box,
				   output_type_choice);
    output_header_type_prf = prf;
    remember_pref(prf, reflect_output_type, NULL);

    prf = prefs_row_with_radio_box("data format", S_default_output_data_format,
				   output_format_choices, 4, -1,
				   dpy_box,
				   output_format_choice);
    output_data_format_prf = prf;
    remember_pref(prf, reflect_output_format, NULL);
    reflect_output_type(output_header_type_prf);
    reflect_output_format(output_data_format_prf);

    current_sep = make_inter_variable_separator(dpy_box);
    {
      int i, srate = 0, chans = 0, format = 0;
      mus_header_raw_defaults(&srate, &chans, &format);
      str = mus_format("%d", chans);
      str1 = mus_format("%d", srate);
      raw_data_format_choices = (char **)CALLOC(MUS_NUM_DATA_FORMATS - 1, sizeof(char *));
      for (i = 1; i < MUS_NUM_DATA_FORMATS; i++)
	raw_data_format_choices[i - 1] = raw_data_format_to_string(i); /* skip MUS_UNKNOWN */
      prf = prefs_row_with_text("default raw sound attributes: chans", S_mus_header_raw_defaults, str,
				dpy_box, 
				raw_chans_choice);
      remember_pref(prf, reflect_raw_chans, NULL);

      prf = prefs_row_with_text("srate", S_mus_header_raw_defaults, str1,
				dpy_box, 
				raw_srate_choice);
      remember_pref(prf, reflect_raw_srate, NULL);
      FREE(str);
      FREE(str1);

#if HAVE_GTK_COMBO_BOX_ENTRY_NEW_TEXT
      prf = prefs_row_with_list("data format", S_mus_header_raw_defaults, raw_data_format_choices[format - 1],
				(const char **)raw_data_format_choices, MUS_NUM_DATA_FORMATS - 1,
				dpy_box, 
				raw_data_format_from_text,
				NULL, NULL);
      remember_pref(prf, reflect_raw_data_format, NULL);
#endif
    }
    current_sep = make_inter_variable_separator(dpy_box);


  /* ---------------- extra menus ---------------- */

#if HAVE_STATIC_XM
    cursor_label = make_inner_label("  extra menus", dpy_box);
#else
    cursor_label = make_inner_label("  extra menus (these will need the xm module)", dpy_box);
#endif

    prf = prefs_row_with_toggle("context-sensitive popup menu", "add-selection-popup",
				find_context_sensitive_popup(),
				dpy_box,
				context_sensitive_popup_toggle);
    remember_pref(prf, reflect_context_sensitive_popup, save_context_sensitive_popup);
    prf->help_func = context_sensitive_popup_help;

    current_sep = make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_toggle("effects menu", "new-effects.scm",
				find_effects_menu(),
				dpy_box, 
				effects_menu_toggle);
    remember_pref(prf, reflect_effects_menu, save_effects_menu);
    prf->help_func = effects_menu_help;

#if HAVE_SCHEME
    current_sep = make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_toggle("edit menu additions", "edit-menu.scm",
				find_edit_menu(),
				dpy_box, 
				edit_menu_toggle);
    remember_pref(prf, reflect_edit_menu, save_edit_menu);
    prf->help_func = edit_menu_help;

    current_sep = make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_toggle("marks menu", "marks-menu.scm",
				find_marks_menu(),
				dpy_box,
				marks_menu_toggle);
    remember_pref(prf, reflect_marks_menu, save_marks_menu);
    prf->help_func = marks_menu_help;

    current_sep = make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_toggle("mix/track menu", "mix-menu.scm",
				find_mix_menu(),
				dpy_box,
				mix_menu_toggle);
    remember_pref(prf, reflect_mix_menu, save_mix_menu);
    prf->help_func = mix_menu_help;
#endif

    current_sep = make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_toggle("reopen menu", "with-reopen-menu",
				find_reopen_menu(),
				dpy_box,
				reopen_menu_toggle);
    remember_pref(prf, reflect_reopen_menu, save_reopen_menu);
    prf->help_func = reopen_menu_help;

    current_sep = make_inter_variable_separator(dpy_box);


    /* ---------------- additional key bindings ---------------- */

    {
      key_info *ki;

      key_label = make_inner_label("  additional key bindings", dpy_box);

      ki = find_prefs_key_binding("play-from-cursor");
      prf = prefs_row_with_text_and_three_toggles("play all chans from cursor", S_play, 
						  "key:", 8, "ctrl:", "meta:",  "C-x:",
						  ki->key, ki->c, ki->m, ki->x,						
						  dpy_box,
						  bind_play_from_cursor);
      remember_pref(prf, reflect_play_from_cursor, save_pfc_binding);
      prf->help_func = play_from_cursor_help;
      FREE(ki);

      current_sep = make_inter_variable_separator(dpy_box);
      ki = find_prefs_key_binding("show-all");
      prf = prefs_row_with_text_and_three_toggles("show entire sound", S_x_bounds, 
						  "key:", 8, "ctrl:", "meta:",  "C-x:",
						  ki->key, ki->c, ki->m, ki->x,
						  dpy_box,
						  bind_show_all);
      remember_pref(prf, reflect_show_all, save_show_all_binding);
      prf->help_func = show_all_help;
      FREE(ki);

      current_sep = make_inter_variable_separator(dpy_box);
      ki = find_prefs_key_binding("select-all");
      prf = prefs_row_with_text_and_three_toggles("select entire sound", S_select_all, 
						  "key:", 8, "ctrl:", "meta:",  "C-x:",
						  ki->key, ki->c, ki->m, ki->x,
						  dpy_box,
						  bind_select_all);
      remember_pref(prf, reflect_select_all, save_select_all_binding);
      prf->help_func = select_all_help;
      FREE(ki);

      current_sep = make_inter_variable_separator(dpy_box);
      ki = find_prefs_key_binding("show-selection");
      prf = prefs_row_with_text_and_three_toggles("show current selection", "show-selection", 
						  "key:", 8, "ctrl:", "meta:",  "C-x:",
						  ki->key, ki->c, ki->m, ki->x,
						  dpy_box,
						  bind_show_selection);
      remember_pref(prf, reflect_show_selection, save_show_selection_binding);
      prf->help_func = show_selection_help;
      FREE(ki);

      current_sep = make_inter_variable_separator(dpy_box);
      ki = find_prefs_key_binding("revert-sound");
      prf = prefs_row_with_text_and_three_toggles("undo all edits (revert)", S_revert_sound, 
						  "key:", 8, "ctrl:", "meta:",  "C-x:",
						  ki->key, ki->c, ki->m, ki->x,
						  dpy_box,
						  bind_revert);
      remember_pref(prf, reflect_revert, save_revert_binding);
      prf->help_func = revert_help;
      FREE(ki);

      current_sep = make_inter_variable_separator(dpy_box);
      ki = find_prefs_key_binding("exit");
      prf = prefs_row_with_text_and_three_toggles("exit from Snd", S_exit, 
						  "key:", 8, "ctrl:", "meta:",  "C-x:",
						  ki->key, ki->c, ki->m, ki->x,
						  dpy_box,
						  bind_exit);
      remember_pref(prf, reflect_exit, save_exit_binding);
      prf->help_func = exit_help;
      FREE(ki);

      current_sep = make_inter_variable_separator(dpy_box);
      ki = find_prefs_key_binding("goto-maxamp");
      prf = prefs_row_with_text_and_three_toggles("move cursor to channel's maximum sample", S_maxamp_position, 
						  "key:", 8, "ctrl:", "meta:",  "C-x:",
						  ki->key, ki->c, ki->m, ki->x,
						  dpy_box,
						  bind_goto_maxamp);
      remember_pref(prf, reflect_goto_maxamp, save_goto_maxamp_binding);
      prf->help_func = goto_maxamp_help;
      FREE(ki);

    }

    /* ---------------- cursor options ---------------- */

    current_sep = make_inter_variable_separator(dpy_box);
    cursor_label = make_inner_label("  cursor options", dpy_box);

    prf = prefs_row_with_toggle("report cursor location as it moves", S_with_verbose_cursor,
				verbose_cursor(ss), 
				dpy_box,
				verbose_cursor_toggle);
    remember_pref(prf, reflect_verbose_cursor, NULL);

    current_sep = make_inter_variable_separator(dpy_box);
    {
      char *str1;
      str = mus_format("%.2f", cursor_update_interval(ss));
      str1 = mus_format("%d", cursor_location_offset(ss));
      prf = prefs_row_with_toggle_with_two_texts("track current location while playing", S_with_tracking_cursor,
						 with_tracking_cursor(ss), 
						 "update:", str,
						 "offset:", str1, 8, 
						 dpy_box,
						 with_tracking_cursor_toggle,
						 cursor_location_text);
      remember_pref(prf, reflect_with_tracking_cursor, NULL);
      FREE(str);
      FREE(str1);
    }

    current_sep = make_inter_variable_separator(dpy_box);
    str = mus_format("%d", cursor_size(ss));
    prf = prefs_row_with_number("size", S_cursor_size,
				str, 4, 
				dpy_box,
				cursor_size_up, cursor_size_down, cursor_size_from_text);
    remember_pref(prf, reflect_cursor_size, NULL);
    FREE(str);
    if (cursor_size(ss) <= 0) gtk_widget_set_sensitive(prf->arrow_down, false);

    current_sep = make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_radio_box("shape", S_cursor_style,
				   cursor_styles, 2, cursor_style(ss),
				   dpy_box, 
				   cursor_style_choice);
    remember_pref(prf, reflect_cursor_style, NULL);

    current_sep = make_inter_variable_separator(dpy_box);
    prf = prefs_row_with_radio_box("tracking cursor shape", S_tracking_cursor_style,
				   cursor_styles, 2, tracking_cursor_style(ss),
				   dpy_box, 
				   tracking_cursor_style_choice);
    remember_pref(prf, reflect_tracking_cursor_style, NULL);

    current_sep = make_inter_variable_separator(dpy_box);
    saved_cursor_color = ss->sgx->cursor_color;
    prf = prefs_color_selector_row("color", S_cursor_color, ss->sgx->cursor_color,
				   dpy_box,
				   cursor_color_func);
    remember_pref(prf, reflect_cursor_color, NULL);

    /* ---------------- (overall) colors ---------------- */

    current_sep = make_inter_variable_separator(dpy_box);
    cursor_label = make_inner_label("  colors", dpy_box);
    
    saved_basic_color = ss->sgx->basic_color;
    prf = prefs_color_selector_row("main background color", S_basic_color, ss->sgx->basic_color,
				   dpy_box,
				   basic_color_func);
    remember_pref(prf, reflect_basic_color, NULL);

    current_sep = make_inter_variable_separator(dpy_box);
    saved_highlight_color = ss->sgx->highlight_color;
    prf = prefs_color_selector_row("main highlight color", S_highlight_color, ss->sgx->highlight_color,
				   dpy_box,
				   highlight_color_func);
    remember_pref(prf, reflect_highlight_color, NULL);

    current_sep = make_inter_variable_separator(dpy_box);
    saved_position_color = ss->sgx->position_color;
    prf = prefs_color_selector_row("second highlight color", S_position_color, ss->sgx->position_color,
				   dpy_box,
				   position_color_func);
    remember_pref(prf, reflect_position_color, NULL);

    current_sep = make_inter_variable_separator(dpy_box);
    saved_zoom_color = ss->sgx->zoom_color;
    prf = prefs_color_selector_row("third highlight color", S_zoom_color, ss->sgx->zoom_color,
				   dpy_box,
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
				   graph_styles, NUM_GRAPH_STYLES, graph_style(ss),
				   grf_box,
				   graph_style_choice);
    remember_pref(prf, reflect_graph_style, NULL);

    current_sep = make_inter_variable_separator(grf_box);
    str = mus_format("%d", dot_size(ss));
    prf = prefs_row_with_number("dot size", S_dot_size,
				str, 4, 
				grf_box,
				dot_size_up, dot_size_down, dot_size_from_text);
    remember_pref(prf, reflect_dot_size, NULL);
    FREE(str);
    if (dot_size(ss) <= 0) gtk_widget_set_sensitive(prf->arrow_down, false);

    current_sep = make_inter_variable_separator(grf_box);
    str = initial_bounds_to_string();
    prf = prefs_row_with_text_with_toggle("initial graph x bounds", S_initial_graph_hook, use_full_duration(),
					  "show full duration", str, 16,
					  grf_box, 
					  initial_bounds_toggle,
					  initial_bounds_text);
    FREE(str);
    remember_pref(prf, reflect_initial_bounds, save_initial_bounds);
    prf->help_func = initial_bounds_help;

    current_sep = make_inter_variable_separator(grf_box);
    prf = prefs_row_with_radio_box("how to layout multichannel graphs", S_channel_style,
				   channel_styles, NUM_CHANNEL_STYLES, channel_style(ss),
				   grf_box,
				   channel_style_choice);
    remember_pref(prf, reflect_channel_style, NULL);

    current_sep = make_inter_variable_separator(grf_box);
    prf = prefs_row_with_toggle("layout wave and fft graphs horizontally", S_graphs_horizontal,
				graphs_horizontal(ss),
				grf_box,
				graphs_horizontal_toggle);
    remember_pref(prf, reflect_graphs_horizontal, NULL);

    current_sep = make_inter_variable_separator(grf_box);
    prf = prefs_row_with_toggle("include y=0 line in sound graphs", S_show_y_zero,
				show_y_zero(ss),
				grf_box,
				y_zero_toggle);
    remember_pref(prf, reflect_show_y_zero, NULL);

    current_sep = make_inter_variable_separator(grf_box);
    prf = prefs_row_with_toggle("include a grid in sound graphs", S_show_grid,
				(show_grid(ss) == WITH_GRID),
				grf_box,
				grid_toggle);
    remember_pref(prf, reflect_show_grid, NULL);

    current_sep = make_inter_variable_separator(grf_box);
    prf = prefs_row_with_scale("grid density", S_grid_density, 
			       2.0, grid_density(ss),
			       grf_box,
			       grid_density_scale_callback, grid_density_text_callback);
    remember_pref(prf, reflect_grid_density, NULL);

#if HAVE_GTK_COMBO_BOX_ENTRY_NEW_TEXT
    current_sep = make_inter_variable_separator(grf_box);
    prf = prefs_row_with_list("what axes to display", S_show_axes, show_axes_choices[(int)show_axes(ss)],
			      show_axes_choices, NUM_SHOW_AXES,
			      grf_box,
			      show_axes_from_text,
			      NULL, NULL);
    remember_pref(prf, reflect_show_axes, NULL);

    current_sep = make_inter_variable_separator(grf_box);
    prf = prefs_row_with_list("time division", S_x_axis_style, x_axis_styles[(int)x_axis_style(ss)],
			      x_axis_styles, NUM_X_AXIS_STYLES,
			      grf_box,
			      x_axis_style_from_text,
			      NULL, NULL);
    remember_pref(prf, reflect_x_axis_style, NULL);
#endif

    current_sep = make_inter_variable_separator(grf_box);
    prf = prefs_row_with_toggle("include smpte info", "show-smpte-label",
				find_smpte(),
				grf_box,
				smpte_toggle);
    remember_pref(prf, reflect_smpte, save_smpte);
    prf->help_func = smpte_label_help;

    /* ---------------- (graph) colors ---------------- */

    current_sep = make_inter_variable_separator(grf_box); 
    colgrf_label = make_inner_label("  colors", grf_box);

    saved_data_color = ss->sgx->data_color;    
    prf = prefs_color_selector_row("unselected data (waveform) color", S_data_color, ss->sgx->data_color,
				   grf_box, 
				   data_color_func);
    remember_pref(prf, reflect_data_color, NULL);

    current_sep = make_inter_variable_separator(grf_box);
    saved_graph_color = ss->sgx->graph_color;
    prf = prefs_color_selector_row("unselected graph (background) color", S_graph_color, ss->sgx->graph_color,
				   grf_box,
				   graph_color_func);
    remember_pref(prf, reflect_graph_color, NULL);

    current_sep = make_inter_variable_separator(grf_box);
    saved_selected_data_color = ss->sgx->selected_data_color;
    prf = prefs_color_selector_row("selected channel data (waveform) color", S_selected_data_color, ss->sgx->selected_data_color,
				   grf_box,
				   selected_data_color_func);
    remember_pref(prf, reflect_selected_data_color, NULL);

    current_sep = make_inter_variable_separator(grf_box);
    saved_selected_graph_color = ss->sgx->selected_graph_color;
    prf = prefs_color_selector_row("selected channel graph (background) color", S_selected_graph_color, ss->sgx->selected_graph_color,
				   grf_box,
				   selected_graph_color_func);
    remember_pref(prf, reflect_selected_graph_color, NULL);

    current_sep = make_inter_variable_separator(grf_box);
    saved_selection_color = ss->sgx->selection_color;
    prf = prefs_color_selector_row("selection color", S_selection_color, ss->sgx->selection_color,
				   grf_box,
				   selection_color_func);
    remember_pref(prf, reflect_selection_color, NULL);

    /* ---------------- (graph) fonts ---------------- */

    current_sep = make_inter_variable_separator(grf_box);
    colgrf_label = make_inner_label("  fonts", grf_box);

    prf = prefs_row_with_text("axis label font", S_axis_label_font, 
			      axis_label_font(ss), 
			      grf_box, 
			      axis_label_font_text);
    remember_pref(prf, reflect_axis_label_font, NULL);

    current_sep = make_inter_variable_separator(grf_box);     
    prf = prefs_row_with_text("axis number font", S_axis_numbers_font, 
			      axis_numbers_font(ss), 
			      grf_box,
			      axis_numbers_font_text);
    remember_pref(prf, reflect_axis_numbers_font, NULL);

    current_sep = make_inter_variable_separator(grf_box);     
    prf = prefs_row_with_text("fft peaks font", S_peaks_font, 
			      peaks_font(ss), 
			      grf_box,
			      peaks_font_text);
    remember_pref(prf, reflect_peaks_font, NULL);

    current_sep = make_inter_variable_separator(grf_box);     
    prf = prefs_row_with_text("fft peaks bold font (for main peaks)", S_bold_peaks_font, 
			      bold_peaks_font(ss), 
			      grf_box,
			      bold_peaks_font_text);
    remember_pref(prf, reflect_bold_peaks_font, NULL);

    current_sep = make_inter_variable_separator(grf_box);     
    prf = prefs_row_with_text("tiny font (for various annotations)", S_peaks_font, 
			      tiny_font(ss), 
			      grf_box,
			      tiny_font_text);
    remember_pref(prf, reflect_tiny_font, NULL);
  }

  current_sep = make_inter_topic_separator(topics);

  /* -------- transform -------- */
  {
    GtkWidget *fft_box, *fft_label;

    /* ---------------- transform options ---------------- */

    fft_box = make_top_level_box(topics);
    fft_label = make_top_level_label("transform options", fft_box);

    str = mus_format(OFF_TD, transform_size(ss));
    prf = prefs_row_with_number("size", S_transform_size,
				str, 12, 
				fft_box,
				fft_size_up, fft_size_down, fft_size_from_text);
    remember_pref(prf, reflect_fft_size, NULL);
    FREE(str);
    if (transform_size(ss) <= 2) gtk_widget_set_sensitive(prf->arrow_down, false);

    current_sep = make_inter_variable_separator(fft_box);
    prf = prefs_row_with_radio_box("transform graph choice", S_transform_graph_type,
				   transform_graph_types, 3, transform_graph_type(ss),
				   fft_box,
				   transform_graph_type_choice);
    remember_pref(prf, reflect_transform_graph_type, NULL);

#if HAVE_GTK_COMBO_BOX_ENTRY_NEW_TEXT
    current_sep = make_inter_variable_separator(fft_box);
    prf = prefs_row_with_list("transform", S_transform_type, transform_types[transform_type(ss)],
			      transform_types, NUM_BUILTIN_TRANSFORM_TYPES,
			      fft_box,
			      transform_type_from_text,
			      transform_type_completer, NULL);
    remember_pref(prf, reflect_transform_type, NULL);

    current_sep = make_inter_variable_separator(fft_box);
    prf = prefs_row_with_list("data window", S_fft_window, fft_windows[(int)fft_window(ss)],
			      fft_windows, MUS_NUM_WINDOWS,
			      fft_box,
			      fft_window_from_text,
			      fft_window_completer, NULL);
    remember_pref(prf, reflect_fft_window, NULL);
#endif

    current_sep = make_inter_variable_separator(fft_box);
    prf = prefs_row_with_scale("data window family parameter", S_fft_window_beta, 
			       1.0, fft_window_beta(ss),
			       fft_box,
			       fft_window_beta_scale_callback, fft_window_beta_text_callback);
    remember_pref(prf, reflect_fft_window_beta, NULL);

    current_sep = make_inter_variable_separator(fft_box);
    str = mus_format("%d", max_transform_peaks(ss));
    prf = prefs_row_with_toggle_with_text("show fft peak data", S_show_transform_peaks,
					  show_transform_peaks(ss),
					  "max peaks:", str, 5,
					  fft_box,
					  transform_peaks_toggle, max_peaks_text);
    remember_pref(prf, reflect_show_transform_peaks, NULL);
    FREE(str);

#if HAVE_GTK_COMBO_BOX_ENTRY_NEW_TEXT
    current_sep = make_inter_variable_separator(fft_box);
    {
      const char **cmaps;
      int i, len;
      len = num_colormaps();
      cmaps = (const char **)CALLOC(len, sizeof(const char *));
      for (i = 0; i < len; i++)
	cmaps[i] = (const char *)colormap_name(i);
      prf = prefs_row_with_list("sonogram colormap", S_colormap, cmaps[color_map(ss)],
				cmaps, len,
				fft_box,
				colormap_from_text,
				colormap_completer, NULL);
      remember_pref(prf, reflect_colormap, NULL);
      FREE(cmaps);
    }
#endif

    current_sep = make_inter_variable_separator(fft_box);
    prf = prefs_row_with_toggle("y axis as log magnitude (dB)", S_fft_log_magnitude,
				fft_log_magnitude(ss),
				fft_box,
				log_magnitude_toggle);
    remember_pref(prf, reflect_fft_log_magnitude, NULL);

    current_sep = make_inter_variable_separator(fft_box);
    str = mus_format("%.1f", min_dB(ss));
    prf = prefs_row_with_text("minimum y-axis dB value", S_min_dB, str,
			      fft_box,
			      min_dB_text);
    remember_pref(prf, reflect_min_dB, NULL);
    FREE(str);

    current_sep = make_inter_variable_separator(fft_box);
    prf = prefs_row_with_toggle("x axis as log freq", S_fft_log_frequency,
				fft_log_frequency(ss),
				fft_box,
				log_frequency_toggle);
    remember_pref(prf, reflect_fft_log_frequency, NULL);

    current_sep = make_inter_variable_separator(fft_box);
    prf = prefs_row_with_radio_box("normalization", S_transform_normalization,
				   transform_normalizations, 4, transform_normalization(ss),
				   fft_box,
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
				   mmr_box,
				   mark_color_func);
    remember_pref(prf, reflect_mark_color, NULL);

    current_sep = make_inter_variable_separator(mmr_box);

    str1 = mus_format("%d", mark_tag_width(ss));
    str2 = mus_format("%d", mark_tag_height(ss));
    prf = prefs_row_with_two_texts("mark tag size", S_mark_tag_width, 
				   "width:", str1, "height:", str2, 4,
				   mmr_box,
				   mark_tag_size_text);
    remember_pref(prf, reflect_mark_tag_size, NULL);
    FREE(str2);
    FREE(str1);

    current_sep = make_inter_variable_separator(mmr_box);
    str1 = mus_format("%d", mix_tag_width(ss));
    str2 = mus_format("%d", mix_tag_height(ss));
    prf = prefs_row_with_two_texts("mix tag size", S_mix_tag_width, 
				   "width:", str1, "height:", str2, 4,
				   mmr_box,
				   mix_tag_size_text);
    remember_pref(prf, reflect_mix_tag_size, NULL);
    FREE(str2);
    FREE(str1);

    current_sep = make_inter_variable_separator(mmr_box);
    saved_mix_color = ss->sgx->mix_color;
    prf = prefs_color_selector_row("mix waveform color", S_mix_color, ss->sgx->mix_color,
				   mmr_box,
				   mix_color_func);
    remember_pref(prf, reflect_mix_color, NULL);

    current_sep = make_inter_variable_separator(mmr_box);
    str = mus_format("%d", mix_waveform_height(ss));
    prf = prefs_row_with_toggle_with_text("show mix waveforms (attached to the mix tag)", S_show_mix_waveforms,
					  show_mix_waveforms(ss),
					  "max waveform height:", str, 5,
					  mmr_box,
					  show_mix_waveforms_toggle, mix_waveform_height_text);
    remember_pref(prf, reflect_show_mix_waveforms, NULL);
    FREE(str);
  }
  

  current_sep = make_inter_topic_separator(topics);

  /* -------- clm -------- */
  {
    GtkWidget *clm_box, *clm_label;

    /* ---------------- clm options ---------------- */

    clm_box = make_top_level_box(topics);
    clm_label = make_top_level_label("clm", clm_box);

    include_with_sound = with_sound_is_loaded();
    prf = prefs_row_with_toggle("include with-sound", "with-sound",
				include_with_sound,
				clm_box,
				with_sound_toggle);
    remember_pref(prf, reflect_with_sound, save_with_sound);
    prf->help_func = with_sound_help;

    current_sep = make_inter_variable_separator(clm_box);
    str = mus_format("%d", speed_control_tones(ss));
    prf = prefs_row_with_radio_box_and_number("speed control choice", S_speed_control_style,
					      speed_control_styles, 3, (int)speed_control_style(ss),
					      speed_control_tones(ss), str, 6,
					      clm_box,
					      speed_control_choice, speed_control_up, speed_control_down, speed_control_text);
    remember_pref(prf, reflect_speed_control, NULL);
    FREE(str);

#if HAVE_SCHEME
    current_sep = make_inter_variable_separator(clm_box);
    prf = prefs_row_with_toggle("include hidden controls dialog", "hidden-controls-dialog",
				find_hidden_controls(),
				clm_box,
				hidden_controls_toggle);
    remember_pref(prf, reflect_hidden_controls, save_hidden_controls);
    prf->help_func = hidden_controls_help;
#endif

    current_sep = make_inter_variable_separator(clm_box);
    str = mus_format("%d", sinc_width(ss));
    prf = prefs_row_with_text("sinc interpolation width in srate converter", S_sinc_width, str,
			      clm_box,
			      sinc_width_text);
    remember_pref(prf, reflect_sinc_width, NULL);
    FREE(str);

    current_sep = make_inter_variable_separator(clm_box);
    prf = prefs_row_with_text("with-sound default output file name", "*clm-file-name*", clm_file_name(),
			      clm_box,
			      clm_file_name_text);
    remember_pref(prf, reflect_clm_file_name, NULL);
    prf->help_func = clm_file_name_help;

    current_sep = make_inter_variable_separator(clm_box);
    prf = prefs_row_with_two_texts("sizes", "*clm-table-size*",
				   "wave table:", NULL, "file buffer:", NULL, 8,
				   clm_box,
				   clm_sizes_text);
    reflect_clm_sizes(prf);
    remember_pref(prf, reflect_clm_sizes, NULL);
    prf->help_func = clm_table_size_help;
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
				prg_box,
				show_listener_toggle);
    remember_pref(prf, reflect_show_listener, save_show_listener);

#if HAVE_GUILE
    current_sep = make_inter_variable_separator(prg_box);
    str = mus_format("%d", optimization(ss));
    prf = prefs_row_with_number("optimization level", S_optimization,
				str, 3, 
				prg_box,
				optimization_up, optimization_down, optimization_from_text);
    remember_pref(prf, reflect_optimization, NULL);
    FREE(str);
    if (optimization(ss) == 6) gtk_widget_set_sensitive(prf->arrow_up, false);
    if (optimization(ss) == 0) gtk_widget_set_sensitive(prf->arrow_down, false);
#endif

    current_sep = make_inter_variable_separator(prg_box);
    prf = prefs_row_with_text("prompt", S_listener_prompt, 
			      listener_prompt(ss), 
			      prg_box,
			      listener_prompt_text);
    remember_pref(prf, reflect_listener_prompt, NULL);

    current_sep = make_inter_variable_separator(prg_box);
    prf = prefs_row_with_toggle("include backtrace in error report", S_show_backtrace,
				show_backtrace(ss),
				prg_box,
				show_backtrace_toggle);
    remember_pref(prf, reflect_show_backtrace, NULL);

#if HAVE_GUILE
    current_sep = make_inter_variable_separator(prg_box);
    prf = prefs_row_with_toggle("include debugging aids", "snd-break",
				find_debugging_aids(),
				prg_box,
				debugging_aids_toggle);
    remember_pref(prf, reflect_debugging_aids, save_debugging_aids);
    include_debugging_aids = find_debugging_aids();
#endif

    current_sep = make_inter_variable_separator(prg_box);
    str = mus_format("%d", print_length(ss));
    prf = prefs_row_with_text("number of vector elements to display", S_print_length, str,
			      prg_box,
			      print_length_text);
    remember_pref(prf, reflect_print_length, NULL);
    FREE(str);

    current_sep = make_inter_variable_separator(prg_box);
    prf = prefs_row_with_text("font", S_listener_font, 
			      listener_font(ss), 
			      prg_box,
			      listener_font_text);
    remember_pref(prf, reflect_listener_font, NULL);

    current_sep = make_inter_variable_separator(prg_box);
    saved_listener_color = ss->sgx->listener_color;
    prf = prefs_color_selector_row("background color", S_listener_color, ss->sgx->listener_color,
				   prg_box,
				   listener_color_func);
    remember_pref(prf, reflect_listener_color, NULL);

    current_sep = make_inter_variable_separator(prg_box);
    saved_listener_text_color = ss->sgx->listener_text_color;
    prf = prefs_color_selector_row("text color", S_listener_text_color, ss->sgx->listener_text_color,
				   prg_box,
				   listener_text_color_func);
    remember_pref(prf, reflect_listener_text_color, NULL);
  }

  /* -------- audio -------- */
  {
    GtkWidget *aud_box, *aud_label;

    /* ---------------- audio options ---------------- */

    aud_box = make_top_level_box(topics);
    aud_label = make_top_level_label("audio options", aud_box);

    str = mus_format("%d", dac_size(ss));
    prf = prefs_row_with_text("dac buffer size", S_dac_size, 
			      str,
			      aud_box,
			      dac_size_text);
    remember_pref(prf, reflect_dac_size, NULL);
    FREE(str);

    current_sep = make_inter_variable_separator(aud_box);
    prf = prefs_row_with_toggle("fold in otherwise unplayable channels", S_dac_combines_channels,
				dac_combines_channels(ss),
				aud_box,
				dac_combines_channels_toggle);
    remember_pref(prf, reflect_dac_combines_channels, NULL);

    current_sep = make_inter_variable_separator(aud_box);
    make_inner_label("  recorder options", aud_box);

    prf = prefs_row_with_text("recorder output file name", S_recorder_file, rec_filename(),
			      aud_box, 
			      recorder_filename_text);
    remember_pref(prf, reflect_recorder_filename, NULL);
    current_sep = make_inter_variable_separator(aud_box);

    prf = prefs_row_with_toggle("automatically open the recorded sound", S_recorder_autoload,
				rec_autoload(),
				aud_box, 
				recorder_autoload_toggle);
    remember_pref(prf, reflect_recorder_autoload, NULL);
    current_sep = make_inter_variable_separator(aud_box);

    str = mus_format("%d", rec_buffer_size());
    prf = prefs_row_with_text("input buffer size", S_recorder_buffer_size, 
			      str,
			      aud_box,
			      recorder_buffer_size_text);
    remember_pref(prf, reflect_recorder_buffer_size, NULL);
    FREE(str);
    current_sep = make_inter_variable_separator(aud_box);

    prf = prefs_row_with_radio_box("default recorder output sound attributes: chans", S_recorder_out_chans,
				   recorder_out_chans_choices, 4, -1,
				   aud_box,
				   recorder_out_chans_choice);
    reflect_recorder_out_chans(prf);
    remember_pref(prf, reflect_recorder_out_chans, NULL);

    prf = prefs_row_with_radio_box("srate", S_recorder_srate,
				   recorder_srate_choices, 5, -1,
				   aud_box,
				   recorder_srate_choice);
    reflect_recorder_srate(prf);
    remember_pref(prf, reflect_recorder_srate, NULL);

    prf = prefs_row_with_radio_box("header type", S_recorder_out_header_type,
				   recorder_out_type_choices, 5, -1,
				   aud_box,
				   recorder_out_type_choice);
    recorder_out_header_type_prf = prf;
    remember_pref(prf, reflect_recorder_out_type, NULL);

    prf = prefs_row_with_radio_box("data format", S_recorder_out_data_format,
				   recorder_out_format_choices, 4, -1,
				   aud_box,
				   recorder_out_format_choice);
    recorder_out_data_format_prf = prf;
    remember_pref(prf, reflect_recorder_out_format, NULL);
    reflect_recorder_out_type(recorder_out_header_type_prf);
    reflect_recorder_out_format(recorder_out_data_format_prf);

  }

  set_dialog_widget(PREFERENCES_DIALOG, preferences_dialog);
  gtk_widget_show(preferences_dialog);
  prefs_unsaved = false;
  prefs_set_dialog_title(NULL);
  return(preferences_dialog);
}
