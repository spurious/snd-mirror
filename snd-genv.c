#include "snd.h"

/* envelope editor and viewer */

static GtkWidget *enved_dialog = NULL;
static GtkWidget *applyB, *apply2B, *cancelB, *drawer, *showB, *saveB, *resetB, *firB = NULL;
static GtkWidget *revertB, *undoB, *redoB, *printB, *brktxtL, *brkpixL, *graphB, *fltB, *ampB, *srcB, *rbrow, *clipB, *deleteB;
static GtkWidget *nameL, *textL, *dBB, *orderL;
static GtkWidget *expB, *linB, *lerow, *baseScale, *baseLabel, *baseValue, *selectionB, *selrow, *revrow, *unrow, *saverow;
static GtkObject *baseAdj, *orderAdj;
static GdkGC *gc, *rgc, *ggc, *hgc;
static slist *env_list = NULL;

static char *env_names[3] = {N_("amp env:"), N_("flt env:"), N_("src env:")};

static bool showing_all_envs = false; /* edit one env (0), or view all currently defined envs (1) */
static bool apply_to_selection = false;

static int env_window_width = 0;
static int env_window_height = 0;

static chan_info *active_channel = NULL, *last_active_channel = NULL;

static env* selected_env = NULL; /* if during view, one env is clicked, it is "selected" and can be pasted elsewhere */
static env* active_env = NULL;   /* env currently being edited */

static axis_info *axis = NULL;
static axis_info *gray_ap = NULL;
static bool FIR_p = true;
static bool old_clip_p = false;
static bool ignore_button_release = false;

axis_info *enved_make_axis(const char *name, axis_context *ax, 
			   int ex0, int ey0, int width, int height, 
			   Float xmin, Float xmax, Float ymin, Float ymax,
			   printing_t printing)
{
  /* conjure up minimal context for axis drawer in snd-axis.c */
  if (!axis) 
    {
      axis = (axis_info *)CALLOC(1, sizeof(axis_info));
      axis->ax = (axis_context *)CALLOC(1, sizeof(axis_context));
      fixup_axis_context(axis->ax, drawer, ax->gc);
    }
  if (!gray_ap) 
    {
      gray_ap = (axis_info *)CALLOC(1, sizeof(axis_info));
      gray_ap->ax = (axis_context *)CALLOC(1, sizeof(axis_context));
      gray_ap->graph_active = true;
      fixup_axis_context(gray_ap->ax, drawer, ggc);
    }
  init_env_axes(axis, name, ex0, ey0, width, height, xmin, xmax, ymin, ymax, printing);
  return(axis);
}

static void display_env(env *e, const char *name, GdkGC *cur_gc, int x0, int y0, int width, int height, bool dots, printing_t printing)
{
  axis_context *ax = NULL;  
  ax = (axis_context *)CALLOC(1, sizeof(axis_context));
  ax->wn = drawer->window;
  ax->w = drawer;
  ax->gc = cur_gc;
  ss->enved->with_dots = dots;
  env_editor_display_env(ss->enved, e, ax, name, x0, y0, width, height, printing);
  ax = free_axis_context(ax);
}

void display_enved_env_with_selection(env *e, const char *name, int x0, int y0, int width, int height, bool dots, printing_t printing)
{
  display_env(e, name, (selected_env == e) ? rgc : gc, x0, y0, width, height, dots, printing);
}

static void reflect_segment_state(void);

static void prepare_env_edit(env *new_env)
{
  prepare_enved_edit(new_env);
  if (new_env->base == 1.0)
    set_enved_style(ENVELOPE_LINEAR);
  else
    {
      set_enved_style(ENVELOPE_EXPONENTIAL);
      set_enved_base(new_env->base);
    }
  reflect_segment_state();
}

void set_enved_redo_sensitive(bool val) {set_sensitive(redoB, val);}
void set_enved_revert_sensitive(bool val) {set_sensitive(revertB, val);}
void set_enved_undo_sensitive(bool val) {set_sensitive(undoB, val);}
void set_enved_save_sensitive(bool val) {set_sensitive(saveB, val);}
void set_enved_show_sensitive(bool val) {set_sensitive(showB, val);}

void make_scrolled_env_list(void)
{
  int n, size;
  size = enved_all_envs_top();
  slist_clear(env_list);
  for (n = 0; n < size; n++) 
    slist_append(env_list, enved_all_names(n));
  gtk_widget_show(env_list->scroller);
}

void enved_reflect_amp_env_completion(snd_info *sp)
{
  if ((enved_dialog) && (active_channel) && (enved_wave_p(ss)))
    {
      if (active_channel->sound == sp) 
	env_redisplay();
    }
}

void new_active_channel_alert(void)
{
  if (enved_dialog)
    {
      /* if showing current active channel in gray, update */
      active_channel = current_channel();
      env_redisplay();
    }
}

static void dismiss_enved_callback(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(enved_dialog);
}

static gint delete_enved_dialog(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(enved_dialog);
  return(true);
}

static void help_enved_callback(GtkWidget *w, gpointer context)
{
  envelope_editor_dialog_help();
}

static bool within_selection_src = false;

static void apply_enved(void)
{
  char *origin = NULL, *estr = NULL;
  if (active_env)
    {
      active_channel = current_channel();
      if (active_channel)
	{
	  set_sensitive(applyB, false);
	  set_sensitive(apply2B, false);
	  set_stock_button_label(cancelB, _("Stop"));
	  force_update(cancelB);
	  switch (enved_target(ss))
	    {
	    case ENVED_AMPLITUDE:
#if HAVE_FORTH
	      origin = mus_format("%s%s %s drop", 
				  estr = env_to_string(active_env),
				  (apply_to_selection) ? "" : " 0 " PROC_FALSE,
				  (apply_to_selection) ? S_env_selection : S_env_channel);
#else
	      origin = mus_format("%s" PROC_OPEN "%s%s", 
				  TO_PROC_NAME((apply_to_selection) ? S_env_selection : S_env_channel),
				  estr = env_to_string(active_env),
				  (apply_to_selection) ? "" : PROC_SEP "0" PROC_SEP PROC_FALSE);
#endif
	      apply_env(active_channel, active_env, 0, 
			CURRENT_SAMPLES(active_channel),
			apply_to_selection, FROM_ENVED, 
			origin, NULL,
			C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0);
	      /* calls update_graph, I think, but in short files that doesn't update the amp-env */
	      if (enved_wave_p(ss)) env_redisplay();
	      if (estr) FREE(estr);
	      if (origin) FREE(origin);
	      break;
	    case ENVED_SPECTRUM: 
#if HAVE_FORTH
	      origin = mus_format("%s %d%s %s drop",
				  estr = env_to_string(active_env), 
				  enved_filter_order(ss),
				  (apply_to_selection) ? "" : " 0 " PROC_FALSE,
				  (apply_to_selection) ? S_filter_selection : S_filter_channel);
#else
	      origin = mus_format("%s" PROC_OPEN "%s" PROC_SEP "%d%s",
				  TO_PROC_NAME((apply_to_selection) ? S_filter_selection : S_filter_channel),
				  estr = env_to_string(active_env), 
				  enved_filter_order(ss),
				  (apply_to_selection) ? "" : PROC_SEP "0" PROC_SEP PROC_FALSE);
#endif
	      apply_filter(active_channel, 
			   (FIR_p) ? enved_filter_order(ss) : 0,
			   active_env, 
			   FROM_ENVED, origin, NULL,
			   apply_to_selection, NULL, NULL,
			   C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0, false);
	      if (estr) FREE(estr);
	      if (origin) FREE(origin);
	      break;
	    case ENVED_SRATE:
	      {
		int i, j;
		env *max_env = NULL;
		max_env = copy_env(active_env);
		for (i = 0, j = 1; i < max_env->pts; i++, j += 2)
		  if (max_env->data[j] < .01) max_env->data[j] = .01;
		within_selection_src = true;
		src_env_or_num(active_channel, max_env, 0.0, 
			       false, FROM_ENVED, "Enved: src", apply_to_selection, NULL,
			       C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0);
		within_selection_src = false;
		max_env = free_env(max_env);
		if (enved_wave_p(ss)) env_redisplay();
	      }
	      break;
	    }
	  set_sensitive(applyB, true);
	  set_sensitive(apply2B, true);
	  set_stock_button_label(cancelB, _("Dismiss"));
	}
    }
}

static void env_redisplay_1(printing_t printing)
{
  if (enved_dialog_is_active())
    {
      gdk_window_clear(drawer->window);
      if (showing_all_envs) 
	view_envs(env_window_width, env_window_height, printing);
      else 
	{
	  char *name = NULL;
	  name = (char *)gtk_entry_get_text(GTK_ENTRY(textL));
	  if (!name) name = _("noname");
	  display_env(active_env, name, gc, 0, 0, env_window_width, env_window_height, true, printing);
	  name = NULL;
	  if (enved_wave_p(ss))
	    {
	      if ((enved_target(ss) == ENVED_SPECTRUM) && (active_env) && (FIR_p) && (printing == NOT_PRINTING))
		display_frequency_response(active_env, axis, gray_ap->ax, enved_filter_order(ss), enved_in_dB(ss));
	      enved_show_background_waveform(axis, gray_ap, apply_to_selection, (enved_target(ss) == ENVED_SPECTRUM), printing);
	    }
	}
    }
}

void env_redisplay(void) {env_redisplay_1(NOT_PRINTING);}
void env_redisplay_with_print(void) {env_redisplay_1(PRINTING);}

static void enved_filter_order_callback(GtkWidget *w, gpointer data)
{
  set_enved_filter_order(gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(orderL)));
}

static void clear_point_label(void);
static void clear_genv_error(void)
{
  if (brktxtL) 
    clear_point_label();
}

static gint unpost_genv_error(gpointer data)
{
  clear_genv_error();
  return(0);
}

static void errors_to_genv_text(const char *msg, void *data)
{
  set_button_label(brktxtL, msg);
  g_timeout_add_full(0, (guint32)5000, unpost_genv_error, NULL, NULL);
}

static void text_field_activated(GtkWidget *w, gpointer context)
{ /* might be breakpoints to load or an envelope name (<cr> in enved text field) */
  char *str = NULL;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(w));
  if ((str) && (*str))
    {
      env *e = NULL;
      while (isspace((int)(*str))) str++;
      e = name_to_env(str);
      if (!e)
	{
	  if (isalpha((int)(str[0])))
	    {
	      alert_envelope_editor(str, copy_env(active_env));
	      add_or_edit_symbol(str, active_env);
	      set_sensitive(saveB, false);
	      env_redisplay(); /* updates label */
	    }
	  else 
	    {
	      redirect_errors_to(errors_to_genv_text, NULL);
	      e = string_to_env(str);
	      redirect_errors_to(NULL, NULL);
	    }
	}
      if (e) 
	{
	  if (active_env) active_env = free_env(active_env);
	  active_env = copy_env(e);
	  set_enved_env_list_top(0);
	  prepare_env_edit(active_env);
	  set_sensitive(saveB, true);
	  set_sensitive(undoB, false);
	  set_sensitive(revertB, false);
	  env_redisplay();
	  free_env(e);
	}
    }
}

static void save_button_pressed(GtkWidget *w, gpointer context)
{
  char *name = NULL;
  if (active_env == NULL) return;
  name = (char *)gtk_entry_get_text(GTK_ENTRY(textL));
  if ((!name) || (!(*name))) 
    name = _("unnamed");
  alert_envelope_editor(name, copy_env(active_env));
  add_or_edit_symbol(name, active_env);
  set_sensitive(saveB, false);
  env_redisplay();
}

static void apply_enved_callback(GtkWidget *w, gpointer context)
{
  /* apply current envs to currently sync'd channels */
  apply_enved();
  last_active_channel = active_channel;
}

static void undo_and_apply_enved_callback(GtkWidget *w, gpointer context)
{
  /* undo upto previous amp env, then apply */
  /* this blindly undoes the previous edit (assumed to be an envelope) -- if the user made some other change in the meantime, too bad */
  if ((active_channel) && (active_channel == last_active_channel))
    {
      active_channel->squelch_update = true;
      undo_edit_with_sync(active_channel, 1);
      active_channel->squelch_update = false;
      clear_minibuffer(active_channel->sound);
    }
  apply_enved();
  last_active_channel = active_channel;
}

static void reflect_segment_state(void)
{
  if (enved_dialog)
    {
      gtk_widget_modify_bg(expB, GTK_STATE_NORMAL, (enved_style(ss) == ENVELOPE_EXPONENTIAL) ? ss->sgx->yellow : ss->sgx->lighter_blue);
      gtk_widget_modify_bg(linB, GTK_STATE_NORMAL, (enved_style(ss) == ENVELOPE_LINEAR) ? ss->sgx->yellow : ss->sgx->lighter_blue);
      if ((active_env) && (!(showing_all_envs))) env_redisplay();
    }
}

static void select_or_edit_env(int pos)
{
  if (showing_all_envs)
    {
      showing_all_envs = false;
      set_button_label(showB, _("view envs"));
    }
  if (active_env) active_env = free_env(active_env);
  selected_env = position_to_env(pos);
  if (!selected_env) return;
  active_env = selected_env;
  gtk_entry_set_text(GTK_ENTRY(textL), enved_all_names(pos));
  set_enved_env_list_top(0);
  prepare_env_edit(active_env);
  set_sensitive(undoB, false);
  set_sensitive(revertB, false);
  set_sensitive(saveB, false);
  env_redisplay();
  set_sensitive(deleteB, true);
}

#define BLANK_LABEL "              "

static void clear_point_label(void)
{
  gdk_draw_rectangle(GDK_DRAWABLE(brkpixL->window), hgc, true, 0, 4, 24, 24);
  set_button_label(brktxtL, BLANK_LABEL);
}

static char brkpt_buf[LABEL_BUFFER_SIZE];

void enved_display_point_label(Float x, Float y)
{
  if ((enved_in_dB(ss)) && (min_dB(ss) < -60))
    mus_snprintf(brkpt_buf, LABEL_BUFFER_SIZE, "%.3f : %.5f", x, y);
  else mus_snprintf(brkpt_buf, LABEL_BUFFER_SIZE, "%.3f : %.3f", x, y);
  set_button_label(brktxtL, brkpt_buf);
}

void display_enved_progress(char *str, GdkPixmap *pix)
{
  if (pix)
    gdk_draw_drawable(GDK_DRAWABLE(brkpixL->window), hgc, pix, 0, 0, 0, 8, 16, 16);
  else gdk_draw_rectangle(GDK_DRAWABLE(brkpixL->window), hgc, true, 0, 4, 24, 24);
  if (str)
    set_button_label(brktxtL, str);
  else set_button_label(brktxtL, BLANK_LABEL);
}

static gboolean brkpixL_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  gdk_draw_rectangle(GDK_DRAWABLE(brkpixL->window), hgc, true, 0, 4, 24, 24);
  return(false);
}

static gboolean drawer_button_motion(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{
  int evx, evy;
  GdkModifierType state;
  Tempus motion_time = 0;
  ignore_button_release = false;
  if (ev->state & GDK_BUTTON1_MASK)
    {
      if (ev->is_hint)
	gdk_window_get_pointer(ev->window, &evx, &evy, &state);
      else
	{
	  evx = (int)(ev->x);
	  evy = (int)(ev->y);
	  motion_time = ev->time;
	  if ((motion_time - ss->enved->down_time) < 100) return(false);
	}
    }
  else return(false);
  if (!showing_all_envs)
    {
      env_editor_button_motion(ss->enved, evx, evy, motion_time, active_env);
      enved_display_point_label(ungrf_x(ss->enved->axis, evx), env_editor_ungrf_y_dB(ss->enved, evy));
      env_redisplay();
    }
  return(false);
}

static gboolean drawer_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  ss->enved->down_time = ev->time;
  ss->enved->env_dragged = false;
  if (showing_all_envs)
    {
      int pos;
      pos = hit_env((int)(ev->x), (int)(ev->y), env_window_width, env_window_height);
      slist_select(env_list, pos);
      if ((pos >= 0) && 
	  (pos < enved_all_envs_top())) 
	{
	  select_or_edit_env(pos);
	  ignore_button_release = true;
	}
    }
  else
    {
      if (!active_env)
	{
	  active_env = default_env(1.0, 0.0);
	  env_redisplay(); /* needed to get current_xs set up correctly */
	}
      if (env_editor_button_press(ss->enved, (int)(ev->x), (int)(ev->y), ev->time, active_env))
	env_redisplay();
      enved_display_point_label(ungrf_x(ss->enved->axis, ev->x), env_editor_ungrf_y_dB(ss->enved, (int)(ev->y)));
      set_sensitive(saveB, true);
      set_sensitive(undoB, true);
      set_sensitive(revertB, true);
    }
  return(false);
}

static gboolean drawer_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  if (ignore_button_release)
    ignore_button_release = false;
  else
    {
      if (!showing_all_envs)
	{
	  env_editor_button_release(ss->enved, active_env);
	  env_redisplay();
	  clear_point_label();
	}
    }
  return(false);
}

static gboolean drawer_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  env_window_width = widget_width(w);
  env_window_height = widget_height(w);
  env_redisplay();
  return(false);
}

static gboolean drawer_resize(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  /* update display, can be either view of all envs or sequence of current envs */
  env_window_width = widget_width(w);
  env_window_height = widget_height(w);
  env_redisplay();
  return(false);
}

static void show_button_pressed(GtkWidget *w, gpointer context)
{
  /* if show all (as opposed to show current), loop through loaded LV_LISTs */
  showing_all_envs = (!showing_all_envs);
  set_button_label(showB, (showing_all_envs) ? _("edit env") : _("view envs"));
  env_redisplay();
}

static void selection_button_pressed(GtkWidget *w, gpointer context)
{
  apply_to_selection = (!apply_to_selection);
  gtk_widget_modify_bg(selectionB, GTK_STATE_NORMAL, (apply_to_selection) ? ss->sgx->yellow : ss->sgx->lighter_blue);
  set_sensitive(apply2B, true);
  if ((enved_wave_p(ss)) && 
      (!showing_all_envs)) 
    env_redisplay();
}

static void delete_button_pressed(GtkWidget *w, gpointer context)
{
  if (selected_env)
    {
      int i, len;
      len = enved_all_envs_top();
      for (i = 0; i < len; i++)
	if (selected_env == enved_all_envs(i))
	  {
	    delete_envelope(enved_all_names(i));
	    if (enved_all_envs_top() == 0) 
	      set_sensitive(deleteB, false);
	    if (active_env) active_env = free_env(active_env);
	    selected_env = NULL;
	    env_redisplay();
	    break;
	  }
    }
}

static void revert_button_pressed(GtkWidget *w, gpointer context)
{
  revert_env_edit();
  if (active_env) active_env = free_env(active_env);
  active_env = enved_next_env();
  if (active_env == NULL)
    text_field_activated(textL, NULL);
  env_redisplay();
}

static void undo_button_pressed(GtkWidget *w, gpointer context)
{
  undo_env_edit();
  if (active_env) active_env = free_env(active_env);
  active_env = enved_next_env();
  env_redisplay();
}

static void redo_button_pressed(GtkWidget *w, gpointer context)
{
  redo_env_edit();
  if (active_env) active_env = free_env(active_env);
  active_env = enved_next_env();
  env_redisplay();
}

static void reflect_apply_state(void)
{
  gtk_label_set_text(GTK_LABEL(nameL), _(env_names[enved_target(ss)]));
  gtk_widget_modify_bg(ampB, GTK_STATE_NORMAL, (enved_target(ss) == ENVED_AMPLITUDE) ? ss->sgx->yellow : ss->sgx->lighter_blue);
  gtk_widget_modify_bg(fltB, GTK_STATE_NORMAL, (enved_target(ss) == ENVED_SPECTRUM) ? ss->sgx->yellow : ss->sgx->lighter_blue);
  gtk_widget_modify_bg(srcB, GTK_STATE_NORMAL, (enved_target(ss) == ENVED_SRATE) ? ss->sgx->yellow : ss->sgx->lighter_blue);
  if ((!showing_all_envs) && (enved_wave_p(ss))) env_redisplay();
}

static void flt_button_pressed(GtkWidget *w, gpointer context)
{
  in_set_enved_target(ENVED_SPECTRUM);
  old_clip_p = enved_clip_p(ss);
  set_enved_clip_p(true);
  reflect_apply_state();
}

static void amp_button_pressed(GtkWidget *w, gpointer context)
{
  if (enved_target(ss) == ENVED_SPECTRUM)
    set_enved_clip_p(old_clip_p);
  in_set_enved_target(ENVED_AMPLITUDE);
  reflect_apply_state();
}

static void src_button_pressed(GtkWidget *w, gpointer context)
{
  if (enved_target(ss) == ENVED_SPECTRUM)
    set_enved_clip_p(old_clip_p);
  in_set_enved_target(ENVED_SRATE);
  reflect_apply_state();
}

static void enved_reset(void)
{
  set_enved_clip_p(DEFAULT_ENVED_CLIP_P);
  set_enved_style(ENVELOPE_LINEAR);
  set_enved_power(DEFAULT_ENVED_POWER);
  set_enved_base(DEFAULT_ENVED_BASE);
  set_enved_target(DEFAULT_ENVED_TARGET);
  set_enved_wave_p(DEFAULT_ENVED_WAVE_P);
  set_enved_in_dB(DEFAULT_ENVED_IN_DB);
  set_enved_filter_order(DEFAULT_ENVED_FILTER_ORDER);
  if (active_env) active_env = free_env(active_env);
#if HAVE_SCHEME
  active_env = string_to_env("'(0 0 1 0)");
#endif
#if HAVE_FORTH
  active_env = string_to_env("'( 0 0 1 0 )");
#endif
#if HAVE_RUBY
  active_env = string_to_env("[0, 0, 1, 0]");
#endif
  set_enved_env_list_top(0);
  prepare_env_edit(active_env);
  set_sensitive(saveB, true);
  reflect_enved_style();
  env_redisplay();
}

static void reset_button_pressed(GtkWidget *w, gpointer context)
{
  enved_reset();
}

void enved_print(char *name)
{
  print_enved(name, env_window_height);
}

static void print_button_pressed(GtkWidget *w, gpointer context)
{
  ss->print_choice = PRINT_ENV;
  file_print_callback(w, context);
}

static void env_browse_callback(const char *name, int row, void *data)
{
  select_or_edit_env(row);
}

static void graph_button_callback(GtkWidget *w, gpointer context)
{
  in_set_enved_wave_p(GTK_TOGGLE_BUTTON(w)->active);
  env_redisplay();
}

static void dB_button_callback(GtkWidget *w, gpointer context)
{
  in_set_enved_in_dB(GTK_TOGGLE_BUTTON(w)->active);
  env_redisplay();
}

static void clip_button_callback(GtkWidget *w, gpointer context)
{
  in_set_enved_clip_p(GTK_TOGGLE_BUTTON(w)->active);
}

static void exp_button_pressed(GtkWidget *w, gpointer context)
{
  set_enved_style(ENVELOPE_EXPONENTIAL);
  if ((active_env) && (!(showing_all_envs)))
    {
      active_env->base = enved_base(ss);
      set_sensitive(saveB, true);
    }
  reflect_segment_state();
}

static void lin_button_pressed(GtkWidget *w, gpointer context)
{
  set_enved_style(ENVELOPE_LINEAR);
  if ((active_env) && (!(showing_all_envs)))
    {
      active_env->base = 1.0;
      set_sensitive(saveB, true);
    }
  reflect_segment_state();
}

static void make_base_label(Float bval)
{
  char *sfs, *buf;
  int i, len, scale_len;
  len = (int)(enved_power(ss) * 4);
  if (len < 32) len = 32;
  sfs = (char *)CALLOC(len, sizeof(char));
  mus_snprintf(sfs, len, "%.3f", bval);
  scale_len = (int)(enved_power(ss) + 3);
  if (scale_len < 32) scale_len = 32;
  buf = (char *)CALLOC(scale_len, sizeof(char));
  for (i = 0; i < scale_len - 1; i++) buf[i] = sfs[i];
  gtk_label_set_text(GTK_LABEL(baseValue), buf);
  FREE(sfs);
  FREE(buf);
  in_set_enved_base(bval);
  if ((active_env) && (!(showing_all_envs))) 
    {
      active_env->base = enved_base(ss);
      if (enved_style(ss) == ENVELOPE_EXPONENTIAL) env_redisplay();
    }
}

static void base_changed(Float val)
{
  Float bval;
  if (val == 0) 
    bval = 0.0;
  else 
    {
      if (val == 0.5)
	bval = 1.0;
      else
	{
	  if (val > 0.5)
	    bval = pow(1.0 + (10.0 * ((val - 0.5) * 2)), enved_power(ss));  
	  else 
	    bval = pow((val * 2), enved_power(ss) - 1.0);
	}
    }
  make_base_label(bval);
  if ((active_env) && (enved_style(ss) == ENVELOPE_EXPONENTIAL)) set_sensitive(saveB, true); /* what about undo/redo here? */
}

static void reflect_changed_base(Float val)
{
  Float ival;
  if (val <= 0.0) 
    ival = 0.0;
  else
    {
      if (val == 1.0)
	ival = 0.5;
      else
	{
	  if (val <= 1.0)
	    ival = pow(val, 1.0 / (enved_power(ss) - 1.0)) * 0.5;
	  else ival = (0.5 + ((0.5 * (pow(val, (1.0 / (enved_power(ss)))) - 1)) / 10.0));
	}
    }
  GTK_ADJUSTMENT(baseAdj)->value = ival;
  gtk_adjustment_value_changed(GTK_ADJUSTMENT(baseAdj));
  make_base_label(val);
}

static void base_changed_callback(GtkAdjustment *adj, gpointer context)
{
  base_changed(adj->value);
}

static void fir_button_pressed(GtkWidget *w, gpointer context)
{
  FIR_p = (!FIR_p);
  set_button_label(firB, (FIR_p) ? "fir" : "fft");
  if (enved_wave_p(ss)) env_redisplay();
}

static void reflect_sound_state(void)
{
  bool file_p;
  file_p = (bool)(any_selected_sound());
  set_sensitive(applyB, file_p);
  set_sensitive(apply2B, file_p);
}

static void reflect_file_in_enved(ss_watcher_reason_t reason, void *ignore)
{
  if (enved_dialog) reflect_sound_state();
}

static void enved_selection_watcher(selection_watcher_reason_t reason, void *data);
#define BB_MARGIN 3

GtkWidget *create_envelope_editor(void)
{
  if (!enved_dialog)
    {
      GtkWidget *mainform, *helpB, *leftbox, *bottombox, *leftframe, *toprow, *bottomrow;

      enved_dialog = gtk_dialog_new();
      SG_SIGNAL_CONNECT(enved_dialog, "delete_event", delete_enved_dialog, NULL);
      gtk_window_set_title(GTK_WINDOW(enved_dialog), _("Edit Envelope"));
      sg_make_resizable(enved_dialog);
      gtk_container_set_border_width(GTK_CONTAINER(enved_dialog), 4);
      gtk_widget_realize(enved_dialog);
      gtk_window_resize(GTK_WINDOW(enved_dialog), 500, 500);
      gtk_widget_modify_bg(enved_dialog, GTK_STATE_NORMAL, ss->sgx->highlight_color);

      gc = gdk_gc_new(MAIN_WINDOW(ss));
      gdk_gc_set_background(gc, ss->sgx->white);
      gdk_gc_set_foreground(gc, ss->sgx->black);

      rgc = gdk_gc_new(MAIN_WINDOW(ss));
      gdk_gc_set_background(rgc, ss->sgx->white);
      gdk_gc_set_foreground(rgc, ss->sgx->red);

      ggc = gdk_gc_new(MAIN_WINDOW(ss));
      gdk_gc_set_background(ggc, ss->sgx->white);
      gdk_gc_set_foreground(ggc, ss->sgx->enved_waveform_color);

      hgc = gdk_gc_new(MAIN_WINDOW(ss));
      gdk_gc_set_background(hgc, ss->sgx->highlight_color);
      gdk_gc_set_foreground(hgc, ss->sgx->highlight_color);

      helpB = gtk_button_new_from_stock(GTK_STOCK_HELP);
      gtk_widget_set_name(helpB, "help_button");

      cancelB = sg_button_new_from_stock_with_label("Dismiss", GTK_STOCK_QUIT);
      gtk_widget_set_name(cancelB, "quit_button");

      applyB = gtk_button_new_from_stock(GTK_STOCK_APPLY);
      gtk_widget_set_name(applyB, "doit_button");

      apply2B = sg_button_new_from_stock_with_label(_("Undo&Apply"), GTK_STOCK_UNDO);
      gtk_widget_set_name(apply2B, "doit_again_button");

      resetB = sg_button_new_from_stock_with_label(_("Reset"), GTK_STOCK_REFRESH);
      gtk_widget_set_name(resetB, "reset_button");

      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(enved_dialog)->action_area), applyB, false, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(enved_dialog)->action_area), apply2B, false, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(enved_dialog)->action_area), cancelB, false, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(enved_dialog)->action_area), resetB, false, true, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(enved_dialog)->action_area), helpB, false, true, 10);

      SG_SIGNAL_CONNECT(cancelB, "clicked", dismiss_enved_callback, NULL);
      SG_SIGNAL_CONNECT(applyB, "clicked", apply_enved_callback, NULL);
      SG_SIGNAL_CONNECT(apply2B, "clicked", undo_and_apply_enved_callback, NULL);
      SG_SIGNAL_CONNECT(resetB, "clicked", reset_button_pressed, NULL);
      SG_SIGNAL_CONNECT(helpB, "clicked", help_enved_callback, NULL);

      gtk_widget_show(cancelB);
      gtk_widget_show(applyB);
      gtk_widget_show(apply2B);
      gtk_widget_show(resetB);
      gtk_widget_show(helpB);

      mainform = gtk_hbox_new(false, 0); /* buttons + graph */
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(enved_dialog)->vbox), mainform, true, true, 0);

      leftframe = gtk_frame_new(NULL);
      gtk_box_pack_start(GTK_BOX(mainform), leftframe, false, false, 0);
      gtk_frame_set_shadow_type(GTK_FRAME(leftframe), GTK_SHADOW_ETCHED_IN);
      gtk_widget_show(leftframe);
      gtk_widget_modify_bg(leftframe, GTK_STATE_NORMAL, ss->sgx->black);

      leftbox = gtk_vbox_new(false, 0);
      gtk_container_add(GTK_CONTAINER(leftframe), leftbox);
      gtk_widget_show(leftbox);

      bottombox = gtk_vbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(enved_dialog)->vbox), bottombox, false, false, 0);
      gtk_widget_show(bottombox);

      drawer = gtk_drawing_area_new();
      gtk_box_pack_start(GTK_BOX(mainform), drawer, true, true, 0);
      gtk_widget_set_events(drawer, GDK_ALL_EVENTS_MASK);
      gtk_widget_modify_bg(drawer, GTK_STATE_NORMAL, ss->sgx->white);
      gtk_widget_modify_fg(drawer, GTK_STATE_NORMAL, ss->sgx->black);
      gtk_widget_show(drawer);

      showB = gtk_button_new_with_label(_("view envs"));
      gtk_widget_set_name(showB, "env_button");
      gtk_button_set_relief(GTK_BUTTON(showB), GTK_RELIEF_HALF);
      gtk_box_pack_start(GTK_BOX(leftbox), showB, false, false, BB_MARGIN);
      SG_SIGNAL_CONNECT(showB, "clicked", show_button_pressed, NULL);
      gtk_widget_show(showB);

      saverow = gtk_hbox_new(false, BB_MARGIN);
      gtk_box_pack_start(GTK_BOX(leftbox), saverow, false, false, BB_MARGIN);
      gtk_widget_show(saverow);

      saveB = gtk_button_new_with_label(_(" save "));
      gtk_widget_set_name(saveB, "env_button");
      gtk_button_set_relief(GTK_BUTTON(saveB), GTK_RELIEF_HALF);
      gtk_box_pack_start(GTK_BOX(saverow), saveB, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(saveB, "clicked", save_button_pressed, NULL);
      gtk_widget_show(saveB);

      printB = gtk_button_new_with_label(_(" print  "));
      gtk_widget_set_name(printB, "env_button");
      gtk_button_set_relief(GTK_BUTTON(printB), GTK_RELIEF_HALF);
      gtk_box_pack_start(GTK_BOX(saverow), printB, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(printB, "clicked", print_button_pressed, NULL);
      gtk_widget_show(printB);

      revrow = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(leftbox), revrow, false, false, BB_MARGIN);
      gtk_widget_show(revrow);

      revertB = gtk_button_new_with_label(_("revert "));
      gtk_widget_set_name(revertB, "env_button");
      gtk_button_set_relief(GTK_BUTTON(revertB), GTK_RELIEF_HALF);
      gtk_box_pack_start(GTK_BOX(revrow), revertB, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(revertB, "clicked", revert_button_pressed, NULL);
      gtk_widget_show(revertB);

      deleteB = gtk_button_new_with_label(_("delete"));
      gtk_widget_set_name(deleteB, "env_button");
      gtk_button_set_relief(GTK_BUTTON(deleteB), GTK_RELIEF_HALF);
      gtk_box_pack_start(GTK_BOX(revrow), deleteB, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(deleteB, "clicked", delete_button_pressed, NULL);
      gtk_widget_show(deleteB);

      unrow = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(leftbox), unrow, false, false, BB_MARGIN);
      gtk_widget_show(unrow);

      undoB = gtk_button_new_with_label(_(" undo "));
      gtk_widget_set_name(undoB, "env_button");
      gtk_button_set_relief(GTK_BUTTON(undoB), GTK_RELIEF_HALF);
      gtk_box_pack_start(GTK_BOX(unrow), undoB, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(undoB, "clicked", undo_button_pressed, NULL);
      gtk_widget_show(undoB);

      redoB = gtk_button_new_with_label(_(" redo "));
      gtk_widget_set_name(redoB, "env_button");
      gtk_button_set_relief(GTK_BUTTON(redoB), GTK_RELIEF_HALF);
      gtk_box_pack_start(GTK_BOX(unrow), redoB, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(redoB, "clicked", redo_button_pressed, NULL);
      gtk_widget_show(redoB);

      rbrow = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(leftbox), rbrow, false, false, BB_MARGIN);
      gtk_widget_show(rbrow);

      ampB = gtk_button_new_with_label(_("amp"));
      gtk_widget_set_name(ampB, "env_button");
      gtk_button_set_relief(GTK_BUTTON(ampB), GTK_RELIEF_HALF);
      gtk_box_pack_start(GTK_BOX(rbrow), ampB, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(ampB, "clicked", amp_button_pressed, NULL);
      gtk_widget_show(ampB);

      fltB = gtk_button_new_with_label(_("flt"));
      gtk_widget_set_name(fltB, "env_button");
      gtk_button_set_relief(GTK_BUTTON(fltB), GTK_RELIEF_HALF);
      gtk_box_pack_start(GTK_BOX(rbrow), fltB, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(fltB, "clicked", flt_button_pressed, NULL);
      gtk_widget_show(fltB);

      srcB = gtk_button_new_with_label(_("src"));
      gtk_widget_set_name(srcB, "env_button");
      gtk_button_set_relief(GTK_BUTTON(srcB), GTK_RELIEF_HALF);
      gtk_box_pack_start(GTK_BOX(rbrow), srcB, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(srcB, "clicked", src_button_pressed, NULL);
      gtk_widget_show(srcB);

      lerow = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(leftbox), lerow, false, false, BB_MARGIN);
      gtk_widget_show(lerow);

      linB = gtk_button_new_with_label(_("lin"));
      gtk_widget_set_name(linB, "env_button");
      gtk_button_set_relief(GTK_BUTTON(linB), GTK_RELIEF_HALF);
      gtk_box_pack_start(GTK_BOX(lerow), linB, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(linB, "clicked", lin_button_pressed, NULL);
      gtk_widget_show(linB);

      expB = gtk_button_new_with_label(_("exp"));
      gtk_widget_set_name(expB, "env_button");
      gtk_button_set_relief(GTK_BUTTON(expB), GTK_RELIEF_HALF);
      gtk_box_pack_start(GTK_BOX(lerow), expB, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(expB, "clicked", exp_button_pressed, NULL);
      gtk_widget_show(expB);

      selrow = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(leftbox), selrow, false, false, BB_MARGIN);
      gtk_widget_show(selrow);

      selectionB = gtk_button_new_with_label(_("selection"));
      gtk_widget_set_name(selectionB, "env_button");
      gtk_button_set_relief(GTK_BUTTON(selectionB), GTK_RELIEF_HALF);
      gtk_box_pack_start(GTK_BOX(selrow), selectionB, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(selectionB, "clicked", selection_button_pressed, NULL);
      gtk_widget_show(selectionB);

      env_list = slist_new_with_title(_("envs:"), leftbox, NULL, 0, BOX_PACK);
      env_list->select_callback = env_browse_callback;
      if (enved_all_envs_top() > 0) make_scrolled_env_list();

      toprow = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(bottombox), toprow, false, false, 0);
      gtk_widget_show(toprow);

      bottomrow = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(bottombox), bottomrow, false, false, 4);
      gtk_widget_show(bottomrow);

      nameL = gtk_label_new(_("amp env:"));
      gtk_box_pack_start(GTK_BOX(toprow), nameL, false, false, 0);
      gtk_widget_show(nameL);

      textL = snd_entry_new(toprow, WITH_WHITE_BACKGROUND);
      SG_SIGNAL_CONNECT(textL, "activate", text_field_activated, NULL);

      brkpixL = gtk_drawing_area_new();
      gtk_widget_modify_bg(brkpixL, GTK_STATE_NORMAL, ss->sgx->highlight_color);
      gtk_widget_set_events(brkpixL, GDK_EXPOSURE_MASK);
      gtk_widget_set_size_request(brkpixL, 16, 16);
      gtk_box_pack_start(GTK_BOX(toprow), brkpixL, false, false, 0);
      gtk_widget_show(brkpixL);
      SG_SIGNAL_CONNECT(brkpixL, "expose_event", brkpixL_expose, NULL);
      
      brktxtL = snd_gtk_highlight_label_new(BLANK_LABEL); /* not NULL!  gtk only creates the label child if not null */
      gtk_box_pack_start(GTK_BOX(toprow), brktxtL, false, false, 0);
      gtk_widget_show(brktxtL);

      clipB = gtk_check_button_new_with_label(_("clip"));
      SG_SIGNAL_CONNECT(clipB, "toggled", clip_button_callback, NULL);
      gtk_box_pack_start(GTK_BOX(toprow), clipB, false, false, 0);
      gtk_widget_show(clipB);

      graphB = gtk_check_button_new_with_label(_("wave"));
      SG_SIGNAL_CONNECT(graphB, "toggled", graph_button_callback, NULL);
      gtk_box_pack_start(GTK_BOX(toprow), graphB, false, false, 0);
      gtk_widget_show(graphB);

      dBB = gtk_check_button_new_with_label(_("dB"));
      SG_SIGNAL_CONNECT(dBB, "toggled", dB_button_callback, NULL);
      gtk_box_pack_start(GTK_BOX(toprow), dBB, false, false, 0);
      gtk_widget_show(dBB);

      baseLabel = gtk_label_new(_("exp:"));
      gtk_box_pack_start(GTK_BOX(bottomrow), baseLabel, false, false, 2);
      gtk_widget_show(baseLabel);

      baseValue = gtk_label_new("1.000");
      gtk_box_pack_start(GTK_BOX(bottomrow), baseValue, false, false, 2);
      gtk_widget_show(baseValue);


      baseAdj = gtk_adjustment_new(0.5, 0.0, 1.0, 0.001, 0.01, .1);
      baseScale = gtk_hscrollbar_new(GTK_ADJUSTMENT(baseAdj));
      gtk_widget_modify_bg(baseScale, GTK_STATE_NORMAL, ss->sgx->position_color);
      SG_SIGNAL_CONNECT(baseAdj, "value_changed", base_changed_callback, NULL);
      gtk_box_pack_start(GTK_BOX(bottomrow), baseScale, true, true, 4);
      gtk_widget_show(baseScale);

      orderAdj = gtk_adjustment_new(20, 2, 100000, 2, 10, 0);
      orderL = gtk_spin_button_new(GTK_ADJUSTMENT(orderAdj), 0.0, 0);
      gtk_box_pack_end(GTK_BOX(bottomrow), orderL, false, false, 4);
      gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(orderL), true);
      SG_SIGNAL_CONNECT(orderAdj, "value_changed", enved_filter_order_callback, NULL);
      SG_SIGNAL_CONNECT(orderL, "enter_notify_event", spin_button_focus_callback, NULL);
      SG_SIGNAL_CONNECT(orderL, "leave_notify_event", spin_button_unfocus_callback, NULL);
      gtk_widget_show(orderL);

      firB = gtk_button_new_with_label((FIR_p) ? "fir" : "fft");
      SG_SIGNAL_CONNECT(firB, "clicked", fir_button_pressed, NULL);
      gtk_box_pack_end(GTK_BOX(bottomrow), firB, false, false, 0);
      gtk_widget_show(firB);

      gtk_widget_show(mainform);
      gtk_widget_show(enved_dialog);

      SG_SIGNAL_CONNECT(drawer, "expose_event", drawer_expose, NULL);
      SG_SIGNAL_CONNECT(drawer, "configure_event", drawer_resize, NULL);
      SG_SIGNAL_CONNECT(drawer, "button_press_event", drawer_button_press, NULL);
      SG_SIGNAL_CONNECT(drawer, "button_release_event", drawer_button_release, NULL);
      SG_SIGNAL_CONNECT(drawer, "motion_notify_event", drawer_button_motion, NULL);

      if (enved_all_envs_top() == 0)
	{
	  set_sensitive(showB, false);
	}
      set_sensitive(revertB, false);
      set_sensitive(deleteB, false);
      set_sensitive(undoB, false);
      set_sensitive(redoB, false);
      set_sensitive(saveB, false);
      if (!(selection_is_active())) set_sensitive(selectionB, false);

      set_toggle_button(clipB, enved_clip_p(ss), false, NULL);
      set_toggle_button(graphB, enved_wave_p(ss), false, NULL);
      set_toggle_button(dBB, enved_in_dB(ss), false, NULL);

      reflect_apply_state();
      reflect_segment_state();
      reflect_sound_state();

      set_dialog_widget(ENVED_DIALOG, enved_dialog);
      add_ss_watcher(SS_FILE_OPEN_WATCHER, reflect_file_in_enved, NULL);
      add_selection_watcher(enved_selection_watcher, NULL);
    }
  else raise_dialog(enved_dialog);
  active_channel = current_channel();
  return(enved_dialog);
}

void set_enved_clip_p(bool val) 
{
  in_set_enved_clip_p(val); 
  if (enved_dialog) set_toggle_button(clipB, val, false, NULL);
}

void reflect_enved_style(void)
{
  reflect_segment_state();
}

void set_enved_target(enved_target_t val) 
{
  in_set_enved_target(val); 
  if (enved_dialog) reflect_apply_state();
}

void set_enved_wave_p(bool val) 
{
  in_set_enved_wave_p(val); 
  if (enved_dialog) set_toggle_button(graphB, val, false, NULL);
}

void set_enved_in_dB(bool val) 
{
  in_set_enved_in_dB(val); 
  if (enved_dialog) set_toggle_button(dBB, val, false, NULL);
}

void set_enved_base(Float val) 
{
  in_set_enved_base(val); 
  if (enved_dialog) reflect_changed_base(val);
}

bool enved_dialog_is_active(void)
{
  return((enved_dialog) && (GTK_WIDGET_VISIBLE(enved_dialog)));
}

void set_enved_filter_order(int order)
{
  if ((order > 0) && (order < 2000))
    {
      if (order & 1) 
	in_set_enved_filter_order(order + 1);
      else in_set_enved_filter_order(order);
      if (enved_dialog)
	{
	  widget_int_to_text(orderL, enved_filter_order(ss));
	  if ((enved_target(ss) == ENVED_SPECTRUM) && 
	      (enved_wave_p(ss)) && 
	      (!showing_all_envs)) 
	    env_redisplay();
	}
    }
}

static void enved_reflect_selection(bool on)
{
  if ((enved_dialog) && (!within_selection_src))
    {
      set_sensitive(selectionB, on);
      if ((apply_to_selection) && (!on))
	{
	  apply_to_selection = false;
	  gtk_widget_modify_bg(selectionB, GTK_STATE_NORMAL, ss->sgx->lighter_blue);
	}
      if ((enved_target(ss) != ENVED_SPECTRUM) && 
	  (enved_wave_p(ss)) && 
	  (!showing_all_envs)) 
	env_redisplay();
    }
}

static void enved_selection_watcher(selection_watcher_reason_t reason, void *data)
{
  switch (reason)
    {
    case SELECTION_INACTIVE: enved_reflect_selection(false);                 break;
    case SELECTION_ACTIVE:   enved_reflect_selection(true);                  break;
    default:                 enved_reflect_selection(selection_is_active()); break;
    }
}

void color_enved_waveform(GdkColor *pix)
{
  ss->sgx->enved_waveform_color = pix;
  if (enved_dialog)
    {
      gdk_gc_set_foreground(ggc, pix);
      if ((enved_wave_p(ss)) && (enved_dialog)) env_redisplay();
    }
}

static XEN g_enved_envelope(void)
{
  #define H_enved_envelope "(" S_enved_envelope "): current envelope editor displayed (active) envelope"
  return(env_to_xen(active_env));
}

static XEN g_set_enved_envelope(XEN e)
{
  XEN_ASSERT_TYPE(XEN_LIST_P(e) || XEN_STRING_P(e) || XEN_SYMBOL_P(e), e, XEN_ONLY_ARG, S_setB S_enved_envelope, "a list, symbol, or string");
  if (active_env) active_env = free_env(active_env);
  if ((XEN_STRING_P(e)) || (XEN_SYMBOL_P(e)))
    active_env = name_to_env((XEN_STRING_P(e)) ? XEN_TO_C_STRING(e) : XEN_SYMBOL_TO_C_STRING(e));
  else active_env = xen_to_env(e);
  if ((!active_env) && (!(XEN_LIST_P(e))))
    XEN_ERROR(NO_SUCH_ENVELOPE,
	      XEN_LIST_2(C_TO_XEN_STRING(S_setB S_enved_envelope),
			 e));
  if (enved_dialog) 
    env_redisplay();
  return(e);
}

static XEN g_enved_filter(void)
{
  #define H_enved_filter "(" S_enved_filter "): envelope editor FIR/FFT filter choice (#t: FIR)"
  return(C_TO_XEN_BOOLEAN(FIR_p));
}

static XEN g_set_enved_filter(XEN type)
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(type), type, XEN_ONLY_ARG, S_setB S_enved_filter, "boolean");
  FIR_p = XEN_TO_C_BOOLEAN(type);
  if (firB)
    set_button_label(firB, (FIR_p) ? "fir" : "fft");
  return(type);
}

#if MUS_DEBUGGING
static XEN g_enved_dialog_widgets(void)
{
  if (enved_dialog)
    return(XEN_CONS(XEN_WRAP_WIDGET(enved_dialog),
	    XEN_CONS(XEN_WRAP_WIDGET(drawer),
	     XEN_CONS(XEN_WRAP_WIDGET(orderL),
	      XEN_CONS(XEN_WRAP_WIDGET(textL),
	       XEN_CONS(XEN_WRAP_WIDGET(applyB),
		XEN_CONS(XEN_WRAP_WIDGET(apply2B),
		 XEN_CONS(XEN_WRAP_WIDGET(cancelB),
		  XEN_CONS(XEN_WRAP_WIDGET(showB),
		   XEN_CONS(XEN_WRAP_WIDGET(saveB),
		    XEN_CONS(XEN_WRAP_WIDGET(revertB),
		     XEN_CONS(XEN_WRAP_WIDGET(undoB),
		      XEN_CONS(XEN_WRAP_WIDGET(redoB),
		       XEN_CONS(XEN_WRAP_WIDGET(printB),
			XEN_CONS(XEN_WRAP_WIDGET(graphB),
			 XEN_CONS(XEN_WRAP_WIDGET(fltB),
			  XEN_CONS(XEN_WRAP_WIDGET(ampB),
			   XEN_CONS(XEN_WRAP_WIDGET(srcB),
			    XEN_CONS(XEN_WRAP_WIDGET(clipB),
			     XEN_CONS(XEN_WRAP_WIDGET(dBB),
			      XEN_CONS(XEN_WRAP_WIDGET(deleteB),
			       XEN_CONS(XEN_WRAP_WIDGET(expB),
				XEN_CONS(XEN_WRAP_WIDGET(linB),
				 XEN_CONS(XEN_WRAP_WIDGET(selectionB),
				  XEN_CONS(XEN_WRAP_WIDGET(resetB),
				   XEN_CONS(XEN_WRAP_WIDGET(env_list->topics),
				    XEN_CONS(XEN_WRAP_WIDGET(firB),
				     XEN_EMPTY_LIST)))))))))))))))))))))))))));
  return(XEN_EMPTY_LIST);
}

static XEN g_enved_axis_info(void)
{
  if (enved_dialog)
    {
      axis_info *ap;
      if (axis == NULL)
	enved_reset();
      ap = axis;
      return(XEN_CONS(C_TO_XEN_INT(ap->x_axis_x0),
               XEN_CONS(C_TO_XEN_INT(ap->y_axis_y0),
                 XEN_CONS(C_TO_XEN_INT(ap->x_axis_x1),
		   XEN_CONS(C_TO_XEN_INT(ap->y_axis_y1),
		     XEN_EMPTY_LIST)))));
    }
  return(XEN_EMPTY_LIST);
}
#endif


#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_enved_filter_w, g_enved_filter)
XEN_NARGIFY_1(g_set_enved_filter_w, g_set_enved_filter)
XEN_NARGIFY_0(g_enved_envelope_w, g_enved_envelope)
XEN_NARGIFY_1(g_set_enved_envelope_w, g_set_enved_envelope)
#if MUS_DEBUGGING
XEN_NARGIFY_0(g_enved_dialog_widgets_w, g_enved_dialog_widgets)
XEN_NARGIFY_0(g_enved_axis_info_w, g_enved_axis_info)
#endif
#else
#define g_enved_filter_w g_enved_filter
#define g_set_enved_filter_w g_set_enved_filter
#define g_enved_envelope_w g_enved_envelope
#define g_set_enved_envelope_w g_set_enved_envelope
#if MUS_DEBUGGING
#define g_enved_dialog_widgets_w g_enved_dialog_widgets
#define g_enved_axis_info_w g_enved_axis_info
#endif
#endif

void g_init_gxenv(void)
{
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_filter, g_enved_filter_w, H_enved_filter,
				   S_setB S_enved_filter, g_set_enved_filter_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_envelope, g_enved_envelope_w, H_enved_envelope,
				   S_setB S_enved_envelope, g_set_enved_envelope_w,  0, 0, 1, 0);

#if MUS_DEBUGGING
  XEN_DEFINE_PROCEDURE("enved-dialog-widgets", g_enved_dialog_widgets_w, 0, 0, 0, "");
  XEN_DEFINE_PROCEDURE("enved-axis-info",  g_enved_axis_info_w, 0, 0, 0, "");
#endif
}

