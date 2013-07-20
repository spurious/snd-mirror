#include "snd.h"

/* envelope editor and viewer */

static GtkWidget *enved_dialog = NULL;
static GtkWidget *applyB, *apply2B, *cancelB, *drawer, *showB, *saveB = NULL, *resetB, *firB = NULL;
static GtkWidget *revertB, *undoB, *redoB, *brktxtL, *graphB, *fltB, *ampB, *srcB, *rbrow, *clipB;
static GtkWidget *nameL, *textL, *dBB, *orderL;
static GtkWidget *linB, *lerow, *baseScale, *baseLabel, *baseValue, *selectionB, *selrow, *unrow;
static GtkAdjustment *baseAdj, *orderAdj;
static gc_t *gc, *rgc, *ggc;
static slist *env_list = NULL;

static const char *env_names[3] = {"amp env:", "flt env:", "src env:"};

static bool showing_all_envs = false; /* edit one env (0), or view all currently defined envs (1) */
static bool apply_to_selection = false, we_turned_selection_off = false;

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


axis_info *enved_make_axis(const char *name, graphics_context *ax, 
			   int ex0, int ey0, int width, int height, 
			   mus_float_t xmin, mus_float_t xmax, mus_float_t ymin, mus_float_t ymax,
			   printing_t printing)
{
  init_env_axes(axis, name, ex0, ey0, width, height, xmin, xmax, ymin, ymax, printing);
  return(axis);
}


static void display_env(env *e, const char *name, gc_t *cur_gc, int x0, int y0, int width, int height, bool dots, printing_t printing)
{
  ss->enved->with_dots = dots;
  axis->ax->gc = cur_gc;
  env_editor_display_env(ss->enved, e, axis->ax, name, x0, y0, width, height, printing);
}


void display_enved_env_with_selection(env *e, const char *name, int x0, int y0, int width, int height, bool dots, printing_t printing)
{
  /* called in a loop through all envs during view_envs (called only from env_redisplay_1 below) */
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


void enved_reflect_peak_env_completion(snd_info *sp)
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


static void force_update(GtkWidget *wid)
{
  if ((wid) && (WIDGET_TO_WINDOW(wid)))
    {
      gdk_window_invalidate_rect(GDK_WINDOW(WIDGET_TO_WINDOW(wid)), NULL, true);
      gdk_window_process_updates(GDK_WINDOW(WIDGET_TO_WINDOW(wid)), true);
    }
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
	  set_stock_button_label(cancelB, I_STOP);
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
			apply_to_selection, 
			origin, NULL,
			C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0);
	      /* calls update_graph, I think, but in short files that doesn't update the amp-env */
	      if (enved_wave_p(ss)) env_redisplay();
	      if (estr) free(estr);
	      if (origin) free(origin);
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
			   origin, NULL,
			   apply_to_selection, NULL, NULL,
			   C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0, false);
	      if (estr) free(estr);
	      if (origin) free(origin);
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
			       false, "Enved: src", apply_to_selection, NULL,
			       C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0);
		within_selection_src = false;
		max_env = free_env(max_env);
		if (enved_wave_p(ss)) env_redisplay();
	      }
	      break;
	    }
	  set_sensitive(applyB, true);
	  set_sensitive(apply2B, true);
	  set_stock_button_label(cancelB, I_GO_AWAY);
	}
    }
}


static void env_redisplay_1(printing_t printing)
{
  if (enved_dialog_is_active())
    {
      bool clear_cr = false;
      if ((printing == NOT_PRINTING) && 
	  (!(ss->cr)))
	{
	  /* we can get here from display_channel_data_with_size with an existing ss->cr */
	  ss->cr = make_cairo(WIDGET_TO_WINDOW(drawer));
	  clear_cr = true;
	}
      if (!(ss->cr)) return;
      
      cairo_push_group(ss->cr);
      cairo_set_source_rgba(ss->cr, gc->bg_color->red, gc->bg_color->green, gc->bg_color->blue, gc->bg_color->alpha);
      cairo_rectangle(ss->cr, 0, 0, env_window_width, env_window_height);
      cairo_fill(ss->cr);

      if (showing_all_envs) 
	{
	  int x0, x1, y0, y1;
	  x0 = axis->x_axis_x0;
	  x1 = axis->x_axis_x1;
	  y0 = axis->y_axis_y0;
	  y1 = axis->y_axis_y1;
	  view_envs(env_window_width, env_window_height, NOT_PRINTING); /* NOT_PRINTING because we're not using the eps stuff here */
	  axis->x_axis_x0 = x0;
	  axis->x_axis_x1 = x1;
	  axis->y_axis_y0 = y0;
	  axis->y_axis_y1 = y1;
	}
      else 
	{
	  char *name = NULL;
	  name = (char *)gtk_entry_get_text(GTK_ENTRY(textL));
	  if (!name) name = (char *)"noname";

	  if ((enved_wave_p(ss)) &&
	      (active_channel) &&
	      (!(active_channel->squelch_update)))
	    {
	      if ((enved_target(ss) == ENVED_SPECTRUM) && (active_env) && (FIR_p) && (printing == NOT_PRINTING))
		display_frequency_response(active_env, axis, gray_ap->ax, enved_filter_order(ss), enved_in_dB(ss));
	      enved_show_background_waveform(axis, gray_ap, apply_to_selection, (enved_target(ss) == ENVED_SPECTRUM), NOT_PRINTING);
	    }
	  display_env(active_env, name, gc, 0, 0, env_window_width, env_window_height, true, NOT_PRINTING);
	}

      cairo_pop_group_to_source(ss->cr);
      cairo_paint(ss->cr);
      if ((printing == NOT_PRINTING) &&
	  (clear_cr))
	{
	  free_cairo(ss->cr);
	  ss->cr = NULL;
	}
    }
}


void env_redisplay(void) 
{
  env_redisplay_1(NOT_PRINTING);
}


void env_redisplay_with_print(void) 
{
  env_redisplay_1(PRINTING);
}


void update_enved_background_waveform(chan_info *cp)
{
  if ((enved_dialog_is_active()) &&
      (enved_wave_p(ss)) &&
      (enved_target(ss) == ENVED_AMPLITUDE) &&
      (cp == active_channel) &&
      ((!apply_to_selection) || (selection_is_active_in_channel(cp))))
    env_redisplay();
}



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
  gtk_label_set_text(GTK_LABEL(brktxtL), msg);
  g_timeout_add_full(0, (guint32)5000, unpost_genv_error, NULL, NULL);
}


static void text_field_activated(GtkWidget *w, gpointer context)
{ 
  /* might be breakpoints to load or an envelope name (<cr> in enved text field) */
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
	  if (active_env)
	    {
	      #define ENVED_TEMP_NAME "enved-backup"
	      /* save current under a temp name!  -- user might have mistakenly reused a name */
	      alert_envelope_editor((char *)ENVED_TEMP_NAME, copy_env(active_env));
	      add_or_edit_symbol(ENVED_TEMP_NAME, active_env);
	      active_env = free_env(active_env);
	    }
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
    name = (char *)"unnamed";
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
      clear_status_area(active_channel->sound);
    }
  apply_enved();
  last_active_channel = active_channel;
}


static void reflect_segment_state(void)
{
  if ((enved_dialog) &&
      (active_env) && 
      (!(showing_all_envs)))
    env_redisplay();
}


static void select_or_edit_env(int pos)
{
  if (showing_all_envs)
    {
      showing_all_envs = false;
      set_button_label(showB, "view envs");
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
}


#define BLANK_LABEL "              "

static void clear_point_label(void)
{
  gtk_label_set_text(GTK_LABEL(brktxtL), BLANK_LABEL);
}


static char brkpt_buf[LABEL_BUFFER_SIZE];

static void enved_display_point_label(mus_float_t x, mus_float_t y)
{
  if ((enved_in_dB(ss)) && (min_dB(ss) < -60))
    snprintf(brkpt_buf, LABEL_BUFFER_SIZE, "%.3f : %.5f", x, y);
  else snprintf(brkpt_buf, LABEL_BUFFER_SIZE, "%.3f : %.3f", x, y);
  gtk_label_set_text(GTK_LABEL(brktxtL), brkpt_buf);
}


static gboolean drawer_button_motion(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{
  int evx, evy;
  GdkModifierType state;
  oclock_t motion_time = 0;
  ignore_button_release = false;

  if (BUTTON1_PRESSED(EVENT_STATE(ev)))
    {
      if (EVENT_IS_HINT(ev))
	window_get_pointer(ev, &evx, &evy, &state);
      else
	{
	  evx = (int)(EVENT_X(ev));
	  evy = (int)(EVENT_Y(ev));
	  motion_time = EVENT_TIME(ev);
	  if ((motion_time - ss->enved->down_time) < 100) return(false);
	}
    }
  else return(false);

  if (!showing_all_envs)
    {
      mus_float_t x, y;
      env_editor_button_motion_with_xy(ss->enved, evx, evy, motion_time, active_env, &x, &y);
      enved_display_point_label(x, y);
      env_redisplay();
    }

  return(false);
}


static gboolean drawer_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  ss->enved->down_time = EVENT_TIME(ev);
  ss->enved->env_dragged = false;

  if (showing_all_envs)
    {
      int pos;
      pos = hit_env((int)(EVENT_X(ev)), (int)(EVENT_Y(ev)), env_window_width, env_window_height);
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
	  active_env->base = enved_base(ss);
	  env_redisplay(); /* needed to get current_xs set up correctly */
	}
      if (env_editor_button_press(ss->enved, (int)(EVENT_X(ev)), (int)(EVENT_Y(ev)), EVENT_TIME(ev), active_env))
	env_redisplay();
      enved_display_point_label(ungrf_x(ss->enved->axis, EVENT_X(ev)), env_editor_ungrf_y_dB(ss->enved, (int)(EVENT_Y(ev))));
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
  set_button_label(showB, (showing_all_envs) ? "edit env" : "view envs");
  env_redisplay();
}


static void selection_button_pressed(GtkWidget *w, gpointer context)
{
  apply_to_selection = (!apply_to_selection);
  widget_modify_bg(selectionB, GTK_STATE_NORMAL, (apply_to_selection) ? ss->yellow : ss->basic_color);
  set_sensitive(apply2B, true);
  if ((enved_wave_p(ss)) && 
      (!showing_all_envs)) 
    env_redisplay();
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
  gtk_label_set_text(GTK_LABEL(nameL), env_names[enved_target(ss)]);
  widget_modify_bg(ampB, GTK_STATE_NORMAL, (enved_target(ss) == ENVED_AMPLITUDE) ? ss->yellow : ss->basic_color);
  widget_modify_bg(fltB, GTK_STATE_NORMAL, (enved_target(ss) == ENVED_SPECTRUM) ? ss->yellow : ss->basic_color);
  widget_modify_bg(srcB, GTK_STATE_NORMAL, (enved_target(ss) == ENVED_SRATE) ? ss->yellow : ss->basic_color);
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


static void env_browse_callback(const char *name, int row, void *data)
{
  select_or_edit_env(row);
}


static void graph_button_callback(GtkWidget *w, gpointer context)
{
  in_set_enved_wave_p(TOGGLE_BUTTON_ACTIVE(w));
  env_redisplay();
}


static void dB_button_callback(GtkWidget *w, gpointer context)
{
  in_set_enved_in_dB(TOGGLE_BUTTON_ACTIVE(w));
  env_redisplay();
}


static void clip_button_callback(GtkWidget *w, gpointer context)
{
  in_set_enved_clip_p(TOGGLE_BUTTON_ACTIVE(w));
}


static void make_base_label(mus_float_t bval)
{
  char *sfs, *buf;
  int i, len, scale_len;

  len = (int)(enved_power(ss) * 4);
  if (len < 32) len = 32;

  sfs = (char *)calloc(len, sizeof(char));
  snprintf(sfs, len, "%.3f", bval);

  scale_len = (int)(enved_power(ss) + 3);
  if (scale_len < 32) scale_len = 32;
  buf = (char *)calloc(scale_len, sizeof(char));

  for (i = 0; i < scale_len - 1; i++) buf[i] = sfs[i];
  gtk_label_set_text(GTK_LABEL(baseValue), buf);

  free(sfs);
  free(buf);

  in_set_enved_base(bval);

  if ((active_env) && 
      (!(showing_all_envs))) 
    {
      if (saveB) set_sensitive(saveB, true); /* what about undo/redo here? */

      active_env->base = enved_base(ss);
      if (active_env->base == 1.0)
	set_enved_style(ENVELOPE_LINEAR);
      else set_enved_style(ENVELOPE_EXPONENTIAL);

      env_redisplay();
    }
}


static void base_changed(mus_float_t val)
{
  mus_float_t bval;
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
}


static void reflect_changed_base(mus_float_t val)
{
  mus_float_t ival;
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
  if (baseAdj)
    {
      ADJUSTMENT_SET_VALUE(baseAdj, ival);
      make_base_label(val);
    }
}


static void make_linear(GtkWidget *w, gpointer context)
{
  reflect_changed_base(1.0);
  set_enved_style(ENVELOPE_LINEAR);
}


static void base_changed_callback(GtkAdjustment *adj, gpointer context)
{
  base_changed(ADJUSTMENT_VALUE(adj));
}


static gboolean fir_button_pressed(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  FIR_p = (!FIR_p);
  gtk_label_set_text(GTK_LABEL(firB), (FIR_p) ? "fir" : "fft");
  if (enved_wave_p(ss)) env_redisplay();
  return(false);
}


static void reflect_sound_state(void)
{
  bool file_p;
  file_p = (bool)(any_selected_sound());
  set_sensitive(applyB, file_p);
  set_sensitive(apply2B, file_p);
}


static XEN reflect_file_in_enved(XEN hook_or_reason)
{
  if (enved_dialog) reflect_sound_state();
  return(XEN_FALSE);
}

#ifdef XEN_ARGIFY_1
  XEN_NARGIFY_1(reflect_file_in_enved_w, reflect_file_in_enved)
#else
  #define reflect_file_in_enved_w reflect_file_in_enved
#endif



#define BB_MARGIN 3

GtkWidget *create_envelope_editor(void)
{
  if (!enved_dialog)
    {
      GtkWidget *mainform, *helpB, *leftbox, *bottombox, *leftframe, *toprow, *bottomrow;

      enved_dialog = gtk_dialog_new();
      add_dialog_style(enved_dialog);
      SG_SIGNAL_CONNECT(enved_dialog, "delete_event", delete_enved_dialog, NULL);
      gtk_window_set_title(GTK_WINDOW(enved_dialog), "Edit Envelope");
      sg_make_resizable(enved_dialog);
      gtk_container_set_border_width(GTK_CONTAINER(enved_dialog), 4);
      gtk_widget_realize(enved_dialog);
      gtk_window_resize(GTK_WINDOW(enved_dialog), 500, 500);
      widget_modify_bg(enved_dialog, GTK_STATE_NORMAL, ss->basic_color);

      gc = gc_new();
      gc_set_background(gc, ss->white);
      gc_set_foreground(gc, ss->black);

      rgc = gc_new();
      gc_set_background(rgc, ss->white);
      gc_set_foreground(rgc, ss->red);

      ggc = gc_new();
      gc_set_background(ggc, ss->white);
      gc_set_foreground(ggc, ss->enved_waveform_color);

      helpB = button_new_with_icon(ICON_HELP);
      gtk_widget_set_name(helpB, "dialog_button");

      cancelB = sg_button_new_with_label_and_icon(I_GO_AWAY, ICON_QUIT);
      gtk_widget_set_name(cancelB, "dialog_button");

      applyB = button_new_with_icon(ICON_APPLY);
      gtk_widget_set_name(applyB, "dialog_button");

      apply2B = sg_button_new_with_label_and_icon("Undo&Apply", ICON_UNDO);
      gtk_widget_set_name(apply2B, "dialog_button");

      resetB = sg_button_new_with_label_and_icon("Clear graph", ICON_REFRESH);
      gtk_widget_set_name(resetB, "dialog_button");

      gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(enved_dialog)), helpB, false, true, 10);
      gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(enved_dialog)), resetB, false, true, 10);
      gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(enved_dialog)), cancelB, false, true, 10);
      gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(enved_dialog)), apply2B, false, true, 10);
      gtk_box_pack_end(GTK_BOX(DIALOG_ACTION_AREA(enved_dialog)), applyB, false, true, 10);

      SG_SIGNAL_CONNECT(cancelB, "clicked", dismiss_enved_callback, NULL);
      SG_SIGNAL_CONNECT(resetB, "clicked", reset_button_pressed, NULL);
      SG_SIGNAL_CONNECT(helpB, "clicked", help_enved_callback, NULL);
      SG_SIGNAL_CONNECT(apply2B, "clicked", undo_and_apply_enved_callback, NULL);
      SG_SIGNAL_CONNECT(applyB, "clicked", apply_enved_callback, NULL);

#if HAVE_GTK_3
      add_highlight_button_style(cancelB);
      add_highlight_button_style(applyB);
      add_highlight_button_style(apply2B);
      add_highlight_button_style(resetB);
      add_highlight_button_style(helpB);
#endif
      gtk_widget_show(cancelB);
      gtk_widget_show(applyB);
      gtk_widget_show(apply2B);
      gtk_widget_show(resetB);
      gtk_widget_show(helpB);

      mainform = gtk_hbox_new(false, 0); /* buttons + graph */
      gtk_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(enved_dialog)), mainform, true, true, 0);

      leftframe = gtk_frame_new(NULL);
      gtk_box_pack_start(GTK_BOX(mainform), leftframe, false, false, 0);
      gtk_frame_set_shadow_type(GTK_FRAME(leftframe), GTK_SHADOW_ETCHED_IN);
      gtk_widget_show(leftframe);
      widget_modify_bg(leftframe, GTK_STATE_NORMAL, ss->black);

      leftbox = gtk_vbox_new(false, 0);
      gtk_container_add(GTK_CONTAINER(leftframe), leftbox);
      gtk_widget_show(leftbox);

      bottombox = gtk_vbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(enved_dialog)), bottombox, false, false, 4);
      gtk_widget_show(bottombox);


      drawer = gtk_drawing_area_new();
      gtk_box_pack_start(GTK_BOX(mainform), drawer, true, true, 0);
      gtk_widget_set_events(drawer, GDK_ALL_EVENTS_MASK);
      widget_modify_bg(drawer, GTK_STATE_NORMAL, ss->white);
      widget_modify_fg(drawer, GTK_STATE_NORMAL, ss->black);
      gtk_widget_show(drawer);

      showB = gtk_button_new_with_label("view envs");
      gtk_button_set_relief(GTK_BUTTON(showB), GTK_RELIEF_HALF);
      gtk_box_pack_start(GTK_BOX(leftbox), showB, false, false, BB_MARGIN);
      SG_SIGNAL_CONNECT(showB, "clicked", show_button_pressed, NULL);
#if HAVE_GTK_3
      add_highlight_button_style(showB);
#endif
      gtk_widget_show(showB);

      saveB = gtk_button_new_with_label("define it");
      gtk_button_set_relief(GTK_BUTTON(saveB), GTK_RELIEF_HALF);
      gtk_box_pack_start(GTK_BOX(leftbox), saveB, false, false, BB_MARGIN);
      SG_SIGNAL_CONNECT(saveB, "clicked", save_button_pressed, NULL);
#if HAVE_GTK_3
      add_highlight_button_style(saveB);
#endif
      gtk_widget_show(saveB);

      revertB = gtk_button_new_with_label("revert ");
      gtk_button_set_relief(GTK_BUTTON(revertB), GTK_RELIEF_HALF);
      gtk_box_pack_start(GTK_BOX(leftbox), revertB, false, false, BB_MARGIN);
      SG_SIGNAL_CONNECT(revertB, "clicked", revert_button_pressed, NULL);
#if HAVE_GTK_3
      add_highlight_button_style(revertB);
#endif
      gtk_widget_show(revertB);

      unrow = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(leftbox), unrow, false, false, BB_MARGIN);
      gtk_widget_show(unrow);

      undoB = gtk_button_new_with_label(" undo ");
      gtk_button_set_relief(GTK_BUTTON(undoB), GTK_RELIEF_HALF);
      gtk_box_pack_start(GTK_BOX(unrow), undoB, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(undoB, "clicked", undo_button_pressed, NULL);
#if HAVE_GTK_3
      add_highlight_button_style(undoB);
#endif
      gtk_widget_show(undoB);

      redoB = gtk_button_new_with_label(" redo ");
      gtk_button_set_relief(GTK_BUTTON(redoB), GTK_RELIEF_HALF);
      gtk_box_pack_start(GTK_BOX(unrow), redoB, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(redoB, "clicked", redo_button_pressed, NULL);
#if HAVE_GTK_3
      add_highlight_button_style(redoB);
#endif
      gtk_widget_show(redoB);

      rbrow = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(leftbox), rbrow, false, false, BB_MARGIN);
      gtk_widget_show(rbrow);

      ampB = gtk_button_new_with_label("amp");
      gtk_button_set_relief(GTK_BUTTON(ampB), GTK_RELIEF_HALF);
      gtk_box_pack_start(GTK_BOX(rbrow), ampB, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(ampB, "clicked", amp_button_pressed, NULL);
#if HAVE_GTK_3
      add_highlight_button_style(ampB);
#endif
      gtk_widget_show(ampB);

      fltB = gtk_button_new_with_label("flt");
      gtk_button_set_relief(GTK_BUTTON(fltB), GTK_RELIEF_HALF);
      gtk_box_pack_start(GTK_BOX(rbrow), fltB, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(fltB, "clicked", flt_button_pressed, NULL);
#if HAVE_GTK_3
      add_highlight_button_style(fltB);
#endif
      gtk_widget_show(fltB);

      srcB = gtk_button_new_with_label("src");
      gtk_button_set_relief(GTK_BUTTON(srcB), GTK_RELIEF_HALF);
      gtk_box_pack_start(GTK_BOX(rbrow), srcB, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(srcB, "clicked", src_button_pressed, NULL);
#if HAVE_GTK_3
      add_highlight_button_style(srcB);
#endif
      gtk_widget_show(srcB);


      selrow = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(leftbox), selrow, false, false, BB_MARGIN);
      gtk_widget_show(selrow);

      selectionB = gtk_button_new_with_label("selection");
      gtk_button_set_relief(GTK_BUTTON(selectionB), GTK_RELIEF_HALF);
      gtk_box_pack_start(GTK_BOX(selrow), selectionB, true, true, BB_MARGIN);
      SG_SIGNAL_CONNECT(selectionB, "clicked", selection_button_pressed, NULL);
#if HAVE_GTK_3
      add_highlight_button_style(selectionB);
#endif
      gtk_widget_show(selectionB);

      env_list = slist_new_with_title("envs:", leftbox, NULL, 0, BOX_PACK);
      env_list->select_callback = env_browse_callback;
      if (enved_all_envs_top() > 0) make_scrolled_env_list();


      toprow = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(bottombox), toprow, false, false, 0);
      gtk_widget_show(toprow);

      bottomrow = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(bottombox), bottomrow, false, false, 0);
      gtk_widget_show(bottomrow);

      {
	GtkWidget* sep;
	sep = gtk_hseparator_new();
	gtk_box_pack_end(GTK_BOX(DIALOG_CONTENT_AREA(enved_dialog)), sep, false, false, 8);
	gtk_widget_show(sep);
      }

      #define LEFT_MARGIN 8

      nameL = gtk_label_new("amp env:");
      widget_set_margin_left(nameL, LEFT_MARGIN);
      gtk_box_pack_start(GTK_BOX(toprow), nameL, false, false, 0);
      gtk_widget_show(nameL);

      textL = snd_entry_new_with_size(toprow, 28);
      SG_SIGNAL_CONNECT(textL, "activate", text_field_activated, NULL);

      brktxtL = gtk_label_new(BLANK_LABEL);
      gtk_box_pack_start(GTK_BOX(toprow), brktxtL, false, false, 6);
      gtk_widget_show(brktxtL);


      dBB = gtk_check_button_new_with_label("dB");
      SG_SIGNAL_CONNECT(dBB, "toggled", dB_button_callback, NULL);
      gtk_box_pack_end(GTK_BOX(toprow), dBB, false, false, 4);
      gtk_widget_show(dBB);

      graphB = gtk_check_button_new_with_label("wave");
      SG_SIGNAL_CONNECT(graphB, "toggled", graph_button_callback, NULL);
      gtk_box_pack_end(GTK_BOX(toprow), graphB, false, false, 4);
      gtk_widget_show(graphB);

      clipB = gtk_check_button_new_with_label("clip");
      SG_SIGNAL_CONNECT(clipB, "toggled", clip_button_callback, NULL);
      gtk_box_pack_end(GTK_BOX(toprow), clipB, false, false, 4);
      gtk_widget_show(clipB);


      baseLabel = gtk_label_new("exp:");
      widget_set_margin_left(baseLabel, LEFT_MARGIN);
      gtk_box_pack_start(GTK_BOX(bottomrow), baseLabel, false, false, 4);
      gtk_widget_show(baseLabel);

      baseValue = gtk_label_new("1.000");
      gtk_box_pack_start(GTK_BOX(bottomrow), baseValue, false, false, 4);
      gtk_widget_show(baseValue);


      lerow = gtk_vbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(bottomrow), lerow, true, true, 8);
      gtk_widget_show(lerow);

      {
	GtkWidget* sep;
	sep = gtk_vseparator_new();
	gtk_box_pack_start(GTK_BOX(lerow), sep, false, false, 8);
	gtk_widget_show(sep);
      }

      baseAdj = (GtkAdjustment *)gtk_adjustment_new(0.5, 0.0, 1.0, 0.001, 0.01, .1);
      baseScale = gtk_hscrollbar_new(GTK_ADJUSTMENT(baseAdj));
      widget_modify_bg(baseScale, GTK_STATE_NORMAL, ss->position_color);
      SG_SIGNAL_CONNECT(baseAdj, "value_changed", base_changed_callback, NULL);
      /* gtk_box_pack_start(GTK_BOX(bottomrow), baseScale, true, true, 4); */
      gtk_box_pack_start(GTK_BOX(lerow), baseScale, true, true, 0);
      gtk_widget_show(baseScale);


      {
	/* try to center the linear button */
	GtkWidget *hr, *rb, *lb;

	hr = gtk_hbox_new(false, 0);
	gtk_box_pack_start(GTK_BOX(lerow), hr, false, false, 4);
	gtk_widget_show(hr);

	rb = gtk_label_new("");
	gtk_box_pack_start(GTK_BOX(hr), rb, true, true, 2);
	gtk_widget_show(rb);

	lb = gtk_label_new("");
	gtk_box_pack_end(GTK_BOX(hr), lb, true, true, 2);
	gtk_widget_show(lb);

	linB = gtk_button_new_with_label("1.0");
	gtk_box_pack_start(GTK_BOX(hr), linB, false, false, 4);
	SG_SIGNAL_CONNECT(linB, "clicked", make_linear, NULL);
#if HAVE_GTK_3
	add_center_button_style(linB); 
#endif
	gtk_widget_show(linB);
      }

      orderAdj = (GtkAdjustment *)gtk_adjustment_new(20, 2, 100000, 2, 10, 0);
      orderL = gtk_spin_button_new(GTK_ADJUSTMENT(orderAdj), 0.0, 0);
      gtk_box_pack_end(GTK_BOX(bottomrow), orderL, false, false, 4);
      gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(orderL), true);
      SG_SIGNAL_CONNECT(orderAdj, "value_changed", enved_filter_order_callback, NULL);
      SG_SIGNAL_CONNECT(orderL, "enter_notify_event", spin_button_focus_callback, NULL);
      SG_SIGNAL_CONNECT(orderL, "leave_notify_event", spin_button_unfocus_callback, NULL);
      gtk_widget_show(orderL);

      /* firB = gtk_button_new_with_label((FIR_p) ? "fir" : "fft"); */
      /* SG_SIGNAL_CONNECT(eb, "clicked", fir_button_pressed, NULL); */
      {
	GtkWidget *eb;
	
	eb = gtk_event_box_new();
	gtk_box_pack_end(GTK_BOX(bottomrow), eb, false, false, 4);
	widget_set_margin_left(eb, 8);
	widget_modify_bg(eb, GTK_STATE_NORMAL, ss->basic_color);
	gtk_widget_set_events(eb, GDK_BUTTON_PRESS_MASK);
	SG_SIGNAL_CONNECT(eb, "button_press_event", fir_button_pressed, NULL);
	gtk_widget_show(eb);

	firB = gtk_label_new("fir");
	gtk_container_add(GTK_CONTAINER(eb), firB);
	gtk_widget_show(firB);
      }

      gtk_widget_show(mainform);
      gtk_widget_show(enved_dialog);

      axis = (axis_info *)calloc(1, sizeof(axis_info));
      axis->ax = (graphics_context *)calloc(1, sizeof(graphics_context));
      axis->ax->wn = WIDGET_TO_WINDOW(drawer);
      axis->ax->w = drawer;
      axis->ax->gc = gc;
      axis->ax->current_font = AXIS_NUMBERS_FONT(ss);

      gray_ap = (axis_info *)calloc(1, sizeof(axis_info));
      gray_ap->ax = (graphics_context *)calloc(1, sizeof(graphics_context));
      gray_ap->graph_active = true;
      gray_ap->ax->wn = WIDGET_TO_WINDOW(drawer);
      gray_ap->ax->w = drawer;
      gray_ap->ax->gc = ggc;
      gray_ap->ax->current_font = AXIS_NUMBERS_FONT(ss);

      SG_SIGNAL_CONNECT(drawer, DRAW_SIGNAL, drawer_expose, NULL);
      SG_SIGNAL_CONNECT(drawer, "configure_event", drawer_resize, NULL);
      SG_SIGNAL_CONNECT(drawer, "button_press_event", drawer_button_press, NULL);
      SG_SIGNAL_CONNECT(drawer, "button_release_event", drawer_button_release, NULL);
      SG_SIGNAL_CONNECT(drawer, "motion_notify_event", drawer_button_motion, NULL);

      if (enved_all_envs_top() == 0)
	set_sensitive(showB, false);
      set_sensitive(revertB, false);
      set_sensitive(undoB, false);
      set_sensitive(redoB, false);
      set_sensitive(saveB, false);
      if (!(selection_is_active())) 
	set_sensitive(selectionB, false);

      set_toggle_button(clipB, enved_clip_p(ss), false, NULL);
      set_toggle_button(graphB, enved_wave_p(ss), false, NULL);
      set_toggle_button(dBB, enved_in_dB(ss), false, NULL);

      reflect_apply_state();
      reflect_segment_state();
      reflect_sound_state();

      set_dialog_widget(ENVED_DIALOG, enved_dialog);

      XEN_ADD_HOOK(ss->snd_open_file_hook, reflect_file_in_enved_w, "enved-file-open-handler", "enved dialog's file-open-hook handler");
    }
  else raise_dialog(enved_dialog);

  env_window_width = widget_width(drawer);
  env_window_height = widget_height(drawer);
  active_channel = current_channel();
  env_redisplay();

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


void set_enved_base(mus_float_t val) 
{
  in_set_enved_base(val); 
  if (enved_dialog) reflect_changed_base(val);
}


bool enved_dialog_is_active(void)
{
  return((enved_dialog) && (widget_is_active(enved_dialog)));
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


void enved_reflect_selection(bool on)
{
  if ((enved_dialog) && (!within_selection_src))
    {
      set_sensitive(selectionB, on);
      if ((apply_to_selection) && (!on))
	{
	  apply_to_selection = false;
	  we_turned_selection_off = true;
	}
      if ((on) && (we_turned_selection_off))
	{
	  apply_to_selection = true;
	}
      widget_modify_bg(selectionB, GTK_STATE_NORMAL, (apply_to_selection) ? ss->yellow : ss->basic_color);
      if ((enved_target(ss) != ENVED_SPECTRUM) && 
	  (enved_wave_p(ss)) && 
	  (!showing_all_envs)) 
	env_redisplay();
    }
}


void color_enved_waveform(color_info *pix)
{
  ss->enved_waveform_color = pix;
  if (enved_dialog)
    {
      gc_set_foreground(ggc, pix);
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
	      XEN_LIST_2(C_TO_XEN_STRING(S_setB S_enved_envelope ": bad envelope arg: ~A"),
			 e));
  if (enved_dialog) 
    env_redisplay();
  return(e);
}


static XEN g_enved_filter(void)
{
  #define H_enved_filter "(" S_enved_filter "): envelope editor FIR/FFT filter choice (" PROC_TRUE ": FIR)"
  return(C_TO_XEN_BOOLEAN(FIR_p));
}


static XEN g_set_enved_filter(XEN type)
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(type), type, XEN_ONLY_ARG, S_setB S_enved_filter, "boolean");
  FIR_p = XEN_TO_C_BOOLEAN(type);
  if (firB)
    gtk_label_set_text(GTK_LABEL(firB), (FIR_p) ? "fir" : "fft");
  return(type);
}


#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_enved_filter_w, g_enved_filter)
XEN_NARGIFY_1(g_set_enved_filter_w, g_set_enved_filter)
XEN_NARGIFY_0(g_enved_envelope_w, g_enved_envelope)
XEN_NARGIFY_1(g_set_enved_envelope_w, g_set_enved_envelope)
#else
#define g_enved_filter_w g_enved_filter
#define g_set_enved_filter_w g_set_enved_filter
#define g_enved_envelope_w g_enved_envelope
#define g_set_enved_envelope_w g_set_enved_envelope
#endif

void g_init_gxenv(void)
{
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_filter, g_enved_filter_w, H_enved_filter,
				   S_setB S_enved_filter, g_set_enved_filter_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_envelope, g_enved_envelope_w, H_enved_envelope,
				   S_setB S_enved_envelope, g_set_enved_envelope_w,  0, 0, 1, 0);
}

