#include "snd.h"

/* envelope editor and viewer */

static GtkWidget *enved_dialog = NULL;
static GtkWidget *applyB, *apply2B, *cancelB, *drawer, *showB, *saveB, *resetB, *firB = NULL;
static GtkWidget *revertB, *undoB, *redoB, *printB, *brktxtL, *brkpixL, *graphB, *fltB, *ampB, *srcB, *rbrow, *clipB, *deleteB;
static GtkWidget *nameL, *textL, *env_list, *dBB, *orderL;
static GtkWidget *expB, *linB, *lerow, *baseScale, *baseLabel, *baseValue, *selectionB, *mixB, *selrow, *revrow, *unrow, *saverow;
static GtkObject *baseAdj, *orderAdj;
static GdkGC *gc, *rgc, *ggc;

static GdkPixmap *blank = NULL;

static char *env_names[3] = {N_("amp env:"), N_("flt env:"), N_("src env:")};

static bool showing_all_envs = false; /* edit one env (0), or view all currently defined envs (1) */
static bool apply_to_selection = false;
static bool apply_to_mix = false;
static Float active_env_base = 1.0;

static int env_window_width = 0;
static int env_window_height = 0;

static chan_info *active_channel = NULL, *last_active_channel = NULL;

static env* selected_env = NULL; /* if during view, one env is clicked, it is "selected" and can be pasted elsewhere */
static env* active_env = NULL;   /* env currently being edited */

static axis_info *axis = NULL;
static axis_info *gray_ap = NULL;
static bool FIR_p = true;
static bool old_clip_p = false;

axis_info *enved_make_axis(char *name, axis_context *ax, 
			   int ex0, int ey0, int width, int height, 
			   Float xmin, Float xmax, Float ymin, Float ymax,
			   bool printing)
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

static void display_env(snd_state *ss, env *e, char *name, GdkGC *cur_gc, int x0, int y0, int width, int height, int dots, Float base, bool printing)
{
  axis_context *ax = NULL;  
  ax = (axis_context *)CALLOC(1, sizeof(axis_context));
  ax->wn = drawer->window;
  ax->w = drawer;
  ax->gc = cur_gc;
  display_enved_env(ss, e, ax, name, x0, y0, width, height, dots, base, printing);
  ax = free_axis_context(ax);
}

void display_enved_env_with_selection(snd_state *ss, env *e, char *name, int x0, int y0, int width, int height, int dots, Float base, bool printing)
{
  display_env(ss, e, name, (selected_env == e) ? rgc : gc, x0, y0, width, height, dots, base, printing);
}

static void do_env_edit(env *new_env, int loading)
{
  do_enved_edit(new_env);
  if (!loading)
    {
      set_sensitive(undoB, true);
      set_sensitive(redoB, false);
      set_sensitive(saveB, true);
      set_sensitive(revertB, true);
    }
}

void set_enved_redo_sensitive(bool val) {set_sensitive(redoB, val);}
void set_enved_revert_sensitive(bool val) {set_sensitive(revertB, val);}
void set_enved_undo_sensitive(bool val) {set_sensitive(undoB, val);}
void set_enved_save_sensitive(bool val) {set_sensitive(saveB, val);}
void set_enved_show_sensitive(bool val) {set_sensitive(showB, val);}

void make_scrolled_env_list (snd_state *ss)
{
  int n, size;
  char *str;
  size = enved_all_envs_top();
  gtk_list_store_clear(GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(env_list))));
  set_background(env_list, (ss->sgx)->basic_color);
  for (n = 0; n < size; n++) 
    {
      str = enved_all_names(n);
      sg_list_append(env_list, str);
    }
}

void alert_enved_amp_env(snd_info *sp)
{
  snd_state *ss;
  ss = sp->state;
  if ((enved_dialog) && (active_channel) && (enved_wave_p(ss)))
    {
      if (active_channel->sound == sp) 
	env_redisplay(sp->state);
    }
}

void new_active_channel_alert(snd_state *ss)
{
  if (enved_dialog)
    {
      /* if showing current active channel in gray, update */
      active_channel = current_channel(ss);
      env_redisplay(ss);
    }
}

static void dismiss_enved_callback(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(enved_dialog);
}

static gboolean delete_enved_dialog(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(enved_dialog);
  return(false);
}

static void help_enved_callback(GtkWidget *w, gpointer context)
{
  envelope_editor_dialog_help((snd_state *)context);
}

static bool within_selection_src = false;

static void apply_enved(snd_state *ss)
{
  int mix_id = 0, i, j, chan;
  env *max_env = NULL;
  snd_info *sp;
  if (active_env)
    {
      if (apply_to_mix)
	{
	  if (ss->selected_mix != INVALID_MIX_ID)
	    mix_id = ss->selected_mix;
	  else mix_id = any_mix_id();
	  chan = mix_selected_channel(mix_id);
	  sp = make_mix_readable_from_id(mix_id);
	  if (sp)
	    active_channel = sp->chans[(chan != NO_SELECTION) ? chan : 0];
	}
      else active_channel = current_channel(ss);
      if (active_channel)
	{
	  set_sensitive(applyB, false);
	  set_sensitive(apply2B, false);
	  set_button_label(cancelB, _("Stop"));
	  force_update(cancelB);
	  switch (enved_target(ss))
	    {
	    case ENVED_AMPLITUDE:
	      if (apply_to_mix)
		{
		  set_mix_amp_env_from_gui(mix_id, NO_SELECTION, active_env); /* chan = NO_SELECTION: use selected chan if more than 1 */
		  active_channel = current_channel(ss);
		}
	      else apply_env(active_channel, active_env, 0, 
			     CURRENT_SAMPLES(active_channel),
			     apply_to_selection, FROM_ENVED, 
			     "Enved: amp", NULL,
			     C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0, (enved_exp_p(ss)) ? active_env_base : 1.0);
	      /* calls update_graph, I think, but in short files that doesn't update the amp-env */
	      if (enved_wave_p(ss)) env_redisplay(ss);
	      break;
	    case ENVED_SPECTRUM: 
	      apply_filter(active_channel, 
			   (FIR_p) ? enved_filter_order(ss) : 0,
			   active_env, 
			   FROM_ENVED, "Enved: flt", 
			   apply_to_selection, NULL, NULL,
			   C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0);
	      break;
	    case ENVED_SRATE:
	      max_env = copy_env(active_env);
	      for (i = 0, j = 1; i < max_env->pts; i++, j += 2)
		if (max_env->data[j] < .01) max_env->data[j] = .01;
	      within_selection_src = true;
	      src_env_or_num(ss, active_channel, max_env, 0.0, 
			     false, FROM_ENVED, "Enved: src", apply_to_selection, NULL,
			     C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0, (enved_exp_p(ss)) ? active_env_base : 1.0);
	      within_selection_src = false;
	      max_env = free_env(max_env);
	      if (enved_wave_p(ss)) env_redisplay(ss);
	      break;
	    }
	  set_sensitive(applyB, true);
	  if (!apply_to_mix) set_sensitive(apply2B, true);
	  set_button_label(cancelB, _("Dismiss"));
	}
    }
}

static void env_redisplay_1(snd_state *ss, int printing)
{
  char *name = NULL;
  if (enved_dialog_is_active())
    {
      gdk_window_clear(drawer->window);
      if (showing_all_envs) 
	view_envs(ss, env_window_width, env_window_height, printing);
      else 
	{
	  name = (char *)gtk_entry_get_text(GTK_ENTRY(textL));
	  if (!name) name = _("noname");
	  display_env(ss, active_env, name, gc, 0, 0, 
		      env_window_width, env_window_height, 1, 
		      (enved_exp_p(ss)) ? active_env_base : 1.0,
		      printing);
	  name = NULL;
	  if ((enved_wave_p(ss)) && (!apply_to_mix))
	    {
	      if ((enved_target(ss) == ENVED_SPECTRUM) && (active_env) && (!printing))
		display_frequency_response(ss, active_env, axis, gray_ap->ax, enved_filter_order(ss), enved_in_dB(ss));
	      enved_show_background_waveform(ss, axis, gray_ap, apply_to_selection, (enved_target(ss) == ENVED_SPECTRUM), printing);
	    }
	}
    }
}

void env_redisplay(snd_state *ss) {env_redisplay_1(ss, false);}
void env_redisplay_with_print(snd_state *ss) {env_redisplay_1(ss, true);}

void enved_fft_update(void)
{
  snd_state *ss;
  if ((enved_dialog_is_active()) &&
      (!(showing_all_envs)))
    {
      ss = get_global_state();
      if ((enved_wave_p(ss)) &&
	  (enved_target(ss) == ENVED_SPECTRUM) && 
	  (active_env))
	enved_show_background_waveform(ss, axis, gray_ap, apply_to_selection, true, false);
    }
}

static void enved_filter_order_callback(GtkWidget *w, gpointer data)
{
  set_enved_filter_order((snd_state *)data, gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(orderL)));
}

static void text_field_activated(GtkWidget *w, gpointer context)
{ /* might be breakpoints to load or an envelope name (<cr> in enved text field) */
  snd_state *ss = (snd_state *)context;
  char *str = NULL;
  env *e = NULL;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(w));
  if ((str) && (*str))
    {
      while (isspace((int)(*str))) str++;
      e = name_to_env(str);
      if (!e)
	{
	  if (isalpha((int)(str[0])))
	    {
	      alert_envelope_editor(ss, str, copy_env(active_env));
	      add_or_edit_symbol(str, active_env);
	      set_sensitive(saveB, false);
	      env_redisplay(ss); /* updates label */
	    }
	  else e = string2env(str);
	}
      if (e) 
	{
	  if (active_env) active_env = free_env(active_env);
	  active_env = copy_env(e);
	  set_enved_env_list_top(0);
	  do_env_edit(active_env, true);
	  set_sensitive(saveB, true);
	  env_redisplay(ss);
	  free_env(e);
	}
    }
}

static void save_button_pressed(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  char *name = NULL;
  name = (char *)gtk_entry_get_text(GTK_ENTRY(textL));
  if ((!name) || (!(*name))) 
    name = _("unnamed");
  alert_envelope_editor(ss, name, copy_env(active_env));
  add_or_edit_symbol(name, active_env);
  set_sensitive(saveB, false);
  env_redisplay(ss);
}

static void apply_enved_callback(GtkWidget *w, gpointer context)
{
  /* apply current envs to currently sync'd channels */
  apply_enved((snd_state *)context);
  last_active_channel = active_channel;
}

static void undo_and_apply_enved_callback(GtkWidget *w, gpointer context)
{
  /* undo upto previous amp env, then apply */
  /* this blindly undoes the previous edit (assumed to be an envelope) -- if the user made some other change in the meantime, too bad */
  snd_state *ss = (snd_state *)context;
  if ((active_channel) && (active_channel == last_active_channel))
    {
      active_channel->squelch_update = true;
      undo_edit_with_sync(active_channel, 1);
      active_channel->squelch_update = false;
    }
  apply_enved(ss);
  last_active_channel = active_channel;
}

static void reflect_segment_state (snd_state *ss)
{
  if (enved_dialog)
    {
      set_backgrounds(expB, (enved_exp_p(ss)) ? (ss->sgx)->yellow : (ss->sgx)->basic_color);
      set_backgrounds(linB, (enved_exp_p(ss)) ? (ss->sgx)->basic_color : (ss->sgx)->yellow);
      if ((active_env) && (!(showing_all_envs))) env_redisplay(ss);
    }
}

static void select_or_edit_env(snd_state *ss, int pos)
{
  if (showing_all_envs)
    {
      showing_all_envs = false;
      set_button_label(showB, _("view envs"));
    }
  if (active_env) active_env = free_env(active_env);
  selected_env = enved_all_envs(pos);
  active_env = copy_env(selected_env);
  gtk_entry_set_text(GTK_ENTRY(textL), enved_all_names(pos));
  set_enved_env_list_top(0);
  do_env_edit(active_env, true);
  set_sensitive(undoB, false);
  set_sensitive(revertB, false);
  set_sensitive(saveB, false);
  set_enved_exp_p(ss, (active_env_base != 1.0));
  set_enved_base(ss, active_env_base);
  env_redisplay(ss);
  set_sensitive(deleteB, true);
}

static void clear_point_label(void)
{
  snd_state *ss;
  ss = get_global_state();
  gdk_draw_drawable(GDK_DRAWABLE(brkpixL->window), ss->sgx->basic_gc, blank, 0, 0, 0, 4, 18, 16);
  gtk_label_set_text(GTK_LABEL(brktxtL), "");
}

static char brkpt_buf[LABEL_BUFFER_SIZE];

void enved_display_point_label(snd_state *ss, Float x, Float y)
{
  if ((enved_in_dB(ss)) && (ss->min_dB < -60))
    mus_snprintf(brkpt_buf, LABEL_BUFFER_SIZE, "%.3f : %.5f", x, y);
  else mus_snprintf(brkpt_buf, LABEL_BUFFER_SIZE, "%.3f : %.3f", x, y);
  gtk_label_set_text(GTK_LABEL(brktxtL), brkpt_buf);
}

void display_enved_progress(char *str, GdkPixmap *pix)
{
  snd_state *ss;
  ss = get_global_state();
  if (pix == NULL)
    gtk_label_set_text(GTK_LABEL(brktxtL), str);
  else gdk_draw_drawable(GDK_DRAWABLE(brkpixL->window), ss->sgx->basic_gc, pix, 0, 0, 0, 4, 18, 16);
}

static gboolean brkpixL_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  snd_state *ss;
  ss = get_global_state();
  gdk_draw_drawable(GDK_DRAWABLE(brkpixL->window), ss->sgx->basic_gc, blank, 0, 0, 0, 4, 16, 16);
  return(false);
}


static Tempus down_time;
static bool env_dragged = false;
static int env_pos = 0;
static bool click_to_delete = false;

static gboolean drawer_button_motion(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{
  snd_state *ss = (snd_state *)data;
  int evx, evy;
  GdkModifierType state;
  Tempus motion_time;
  axis_info *ap;
  Float x0, x1, x, y;
  if (ev->state & GDK_BUTTON1_MASK)
    {
      if (ev->is_hint)
	{
	  gdk_window_get_pointer(ev->window, &evx, &evy, &state);
	}
      else
	{
	  evx = (int)(ev->x);
	  evy = (int)(ev->y);
	  motion_time = ev->time;
	  if ((motion_time - down_time) < 100) return(false);
	}
    }
  else return(false);
  if (!showing_all_envs)
    {
      env_dragged = true;
      click_to_delete = false;
      ap = axis;
      x = ungrf_x(ap, evx);
      if (env_pos > 0) 
	x0 = active_env->data[env_pos * 2 - 2]; else 
	  x0 = 0.0;
      if (env_pos < active_env->pts) 
	x1 = active_env->data[env_pos * 2 + 2]; 
      else x1 = 1.0;
      if (x < x0) x = x0;
      if (x > x1) x = x1;
      if (env_pos == 0) x = active_env->data[0];
      if (env_pos == (active_env->pts - 1)) x = active_env->data[(active_env->pts - 1) * 2];
      y = ungrf_y(ap, evy);
      if ((enved_clip_p(ss)) || (enved_in_dB(ss)))
	{
	  if (y < ap->y0) y = ap->y0;
	  if (y > ap->y1) y = ap->y1;
	}
      if (enved_in_dB(ss)) y = un_dB(ss, y);
      if (check_enved_hook(active_env, env_pos, x, y, ENVED_MOVE_POINT) == 0)
	move_point(active_env, env_pos, x, y);
      enved_display_point_label(ss, x, y);
      env_redisplay(ss);
    }
  return(false);
}

static gboolean drawer_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_state *ss = (snd_state *)data;
  int pos;
  down_time = ev->time;
  env_dragged = false;
  if (showing_all_envs)
    {
      pos = hit_env((int)(ev->x), (int)(ev->y), env_window_width, env_window_height);
      sg_list_select(env_list, pos);
      if ((pos >= 0) && 
	  (pos < enved_all_envs_top())) 
	select_or_edit_env(ss, pos);
    }
  else
    {
      if (!active_env)
	{
	  active_env = default_env(1.0, 0.0);
	  active_env_base = 1.0;
	  env_redisplay(ss); /* needed to get current_xs set up correctly */
	}
      env_pos = enved_button_press_display(ss, axis, active_env, (int)(ev->x), (int)(ev->y));
    }
  return(false);
}

void set_enved_click_to_delete(bool n) {click_to_delete = n;}

static gboolean drawer_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  if (!showing_all_envs)
    {
      if ((click_to_delete) && (!env_dragged) && (env_pos != 0) && (env_pos != active_env->pts - 1))
	{
	  if (check_enved_hook(active_env, env_pos, 0, 0, ENVED_DELETE_POINT) == 0)
	    delete_point(active_env, env_pos);
	}
      do_env_edit(active_env, false);
      env_pos = 0;
      env_dragged = false;
      click_to_delete = false;
      env_redisplay((snd_state *)data);
      clear_point_label();
    }
  return(false);
}

static gboolean drawer_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  env_window_width = widget_width(w);
  env_window_height = widget_height(w);
  env_redisplay((snd_state *)data);
  return(false);
}

static gboolean drawer_resize(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  /* update display, can be either view of all envs or sequence of current envs */
  env_window_width = widget_width(w);
  env_window_height = widget_height(w);
  env_redisplay((snd_state *)data);
  return(false);
}

static void show_button_pressed(GtkWidget *w, gpointer context)
{
  /* if show all (as opposed to show current), loop through loaded LV_LISTs */
  showing_all_envs = (!showing_all_envs);
  set_button_label(showB, (showing_all_envs) ? _("edit env") : _("view envs"));
  env_redisplay((snd_state *)context);
}

static void selection_button_pressed(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  apply_to_selection = (!apply_to_selection);
  if (apply_to_selection) 
    {
      if (apply_to_mix) set_backgrounds(mixB, (ss->sgx)->basic_color);
      apply_to_mix = false;
    }
  set_backgrounds(selectionB, (apply_to_selection) ? (ss->sgx)->yellow : (ss->sgx)->basic_color);
  set_sensitive(apply2B, (!apply_to_mix));
  if ((enved_target(ss) != ENVED_SPECTRUM) && 
      (enved_wave_p(ss)) && 
      (!showing_all_envs)) 
    env_redisplay(ss);
}

static void mix_button_pressed(GtkWidget *w, gpointer data)
{
  snd_state *ss = (snd_state *)data;
  int chan = 0;
  int mxchan, mix_id = INVALID_MIX_ID;
  apply_to_mix = (!apply_to_mix);
  if (apply_to_mix) 
    {
      if (apply_to_selection) set_backgrounds(selectionB, (ss->sgx)->basic_color);
      apply_to_selection = false;
      if (ss->selected_mix != INVALID_MIX_ID) 
	mix_id = ss->selected_mix; 
      else
	{
	  mix_id = any_mix_id();
	  select_mix_from_id(mix_id);
	}
      if (mix_id != INVALID_MIX_ID)
	{
	  mxchan = mix_selected_channel(mix_id);
	  if (mxchan != NO_SELECTION) chan = mxchan;
	  if (mix_amp_env_from_id(mix_id, chan))
	    {
	      if (active_env) active_env = free_env(active_env);
	      active_env = copy_env(mix_amp_env_from_id(mix_id, chan));
	      set_enved_env_list_top(0);
	      do_env_edit(active_env, true);
	      set_sensitive(undoB, false);
	      set_sensitive(revertB, false);
	      set_sensitive(saveB, false);
	      env_redisplay(ss);
	    }
	}
    }
  set_backgrounds(mixB, (apply_to_mix) ? (ss->sgx)->yellow : (ss->sgx)->basic_color);
  set_sensitive(apply2B, (!apply_to_mix));
  if ((enved_target(ss) == ENVED_AMPLITUDE) && 
      (enved_wave_p(ss)) && 
      (!showing_all_envs)) 
    env_redisplay(ss);
}

static void delete_button_pressed(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  int i, len;
  if (selected_env)
    {
      len = enved_all_envs_top();
      for (i = 0; i < len; i++)
	if (selected_env == enved_all_envs(i))
	  {
	    delete_envelope(ss, enved_all_names(i));
	    if (enved_all_envs_top() == 0) 
	      set_sensitive(deleteB, false);
	    if (active_env) active_env = free_env(active_env);
	    selected_env = NULL;
	    env_redisplay(ss);
	    break;
	  }
    }
}

static void revert_button_pressed(GtkWidget *w, gpointer context)
{
  revert_env_edit();
  if (active_env) active_env = free_env(active_env);
  active_env = enved_next_env();
  env_redisplay((snd_state *)context);
}

static void undo_button_pressed(GtkWidget *w, gpointer context)
{
  undo_env_edit();
  if (active_env) active_env = free_env(active_env);
  active_env = enved_next_env();
  env_redisplay((snd_state *)context);
}

static void redo_button_pressed(GtkWidget *w, gpointer context)
{
  redo_env_edit();
  if (active_env) active_env = free_env(active_env);
  active_env = enved_next_env();
  env_redisplay((snd_state *)context);
}

static void reflect_apply_state (snd_state *ss)
{
  gtk_label_set_text(GTK_LABEL(nameL), _(env_names[enved_target(ss)]));
  set_backgrounds(ampB, (enved_target(ss) == ENVED_AMPLITUDE) ? (ss->sgx)->green : (ss->sgx)->basic_color);
  set_backgrounds(fltB, (enved_target(ss) == ENVED_SPECTRUM) ? (ss->sgx)->green : (ss->sgx)->basic_color);
  set_backgrounds(srcB, (enved_target(ss) == ENVED_SRATE) ? (ss->sgx)->green : (ss->sgx)->basic_color);
  if ((!showing_all_envs) && (enved_wave_p(ss))) env_redisplay(ss);
}

static void flt_button_pressed(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  in_set_enved_target(ss, ENVED_SPECTRUM);
  old_clip_p = enved_clip_p(ss);
  set_enved_clip_p(ss, true);
  reflect_apply_state(ss);
}

static void amp_button_pressed(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  if (enved_target(ss) == ENVED_SPECTRUM)
    set_enved_clip_p(ss, old_clip_p);
  in_set_enved_target(ss, ENVED_AMPLITUDE);
  reflect_apply_state(ss);
}

static void src_button_pressed(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  if (enved_target(ss) == ENVED_SPECTRUM)
    set_enved_clip_p(ss, old_clip_p);
  in_set_enved_target(ss, ENVED_SRATE);
  reflect_apply_state(ss);
}

static void enved_reset(void)
{
  snd_state *ss;
  ss = get_global_state();
  set_enved_clip_p(ss, DEFAULT_ENVED_CLIP_P);
  set_enved_exp_p(ss, DEFAULT_ENVED_EXP_P);
  set_enved_power(ss, DEFAULT_ENVED_POWER);
  set_enved_base(ss, DEFAULT_ENVED_BASE);
  set_enved_target(ss, DEFAULT_ENVED_TARGET);
  set_enved_wave_p(ss, DEFAULT_ENVED_WAVE_P);
  set_enved_in_dB(ss, DEFAULT_ENVED_IN_DB);
  set_enved_filter_order(ss, DEFAULT_ENVED_FILTER_ORDER);
  if (active_env) active_env = free_env(active_env);
#if HAVE_GUILE
  active_env = string2env("'(0 0 1 0)");
#else
  active_env = string2env("[0, 0, 1, 0]");
#endif
  set_enved_env_list_top(0);
  do_env_edit(active_env, true);
  set_sensitive(saveB, true);
  env_redisplay(ss);
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
  snd_state *ss = (snd_state *)context;
  ss->print_choice = PRINT_ENV;
  file_print_callback(w, context);
}

static void env_browse_callback(GtkTreeSelection *selection, gpointer *gp)
{
  GtkTreeIter iter;
  gchar *value;
  int size, n;
  char *str;
  GtkTreeModel *model;
  if (!(gtk_tree_selection_get_selected(selection, &model, &iter))) return;
  gtk_tree_model_get(model, &iter, 0, &value, -1);
  size = enved_all_envs_top();
  for (n = 0; n < size; n++) 
    {
      str = enved_all_names(n);
      if (strcmp(str, value) == 0)
	{
	  select_or_edit_env((snd_state *)gp, n);
	  return;
	}
    }
}

static void graph_button_callback(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context; 
  in_set_enved_wave_p(ss, GTK_TOGGLE_BUTTON(w)->active);
  env_redisplay(ss);
}

static void dB_button_callback(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context; 
  in_set_enved_in_dB(ss, GTK_TOGGLE_BUTTON(w)->active);
  env_redisplay(ss);
}

static void clip_button_callback(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context; 
  in_set_enved_clip_p(ss, GTK_TOGGLE_BUTTON(w)->active);
}

static void exp_button_pressed(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context; 
  in_set_enved_exp_p(ss, (!(enved_exp_p(ss))));
  if ((active_env) && (!(showing_all_envs)))
    {
      if (enved_exp_p(ss))
	active_env_base = enved_base(ss);
      else active_env_base = 1.0;
      set_sensitive(saveB, true);
    }
  reflect_segment_state(ss);
}

static void lin_button_pressed(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context; 
  in_set_enved_exp_p(ss, (!(enved_exp_p(ss))));
  if ((active_env) && (!(showing_all_envs)))
    {
      if (enved_exp_p(ss))
	active_env_base = enved_base(ss);
      else active_env_base = 1.0;
      set_sensitive(saveB, true);
    }
  reflect_segment_state(ss);
}


#define BASE_MAX 400
#define BASE_MID 200
/* these two just set the granularity of the scale widget, not the user-visible bounds */

static void make_base_label(snd_state *ss, Float bval)
{
  char *sfs, *buf;
  int i, len, scale_len;
  len = (int)(enved_power(ss) * 4);
  if (len < 32) len = 32;
  sfs = (char *)CALLOC(len, sizeof(char));
  mus_snprintf(sfs, len, "%f", bval);
  scale_len = (int)(enved_power(ss) + 3);
  if (scale_len < 32) scale_len = 32;
  buf = (char *)CALLOC(scale_len, sizeof(char));
  for (i = 0; i < scale_len - 1; i++) buf[i] = sfs[i];
  gtk_label_set_text(GTK_LABEL(baseValue), buf);
  FREE(sfs);
  FREE(buf);
  in_set_enved_base(ss, bval);
  if ((active_env) && (!(showing_all_envs))) 
    {
      active_env_base = enved_base(ss);
      if (enved_exp_p(ss)) env_redisplay(ss);
    }
}

static void base_changed(snd_state *ss, Float val)
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
  make_base_label(ss, bval);
  if ((active_env) && (enved_exp_p(ss))) set_sensitive(saveB, true); /* what about undo/redo here? */
}

static void reflect_changed_base(snd_state *ss, Float val)
{
  Float ival;
  if (val <= 0.0) 
    ival = 0;
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
  GTK_ADJUSTMENT(orderAdj)->value = ival;
  gtk_adjustment_value_changed(GTK_ADJUSTMENT(orderAdj));
  make_base_label(ss, val);
}

static void base_changed_callback(GtkAdjustment *adj, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  base_changed(ss, adj->value);
}

static void fir_button_pressed(GtkWidget *w, gpointer context)
{
  FIR_p = (!FIR_p);
  set_button_label(firB, (FIR_p) ? "fir" : "fft");
}

#define BB_MARGIN 2

GtkWidget *create_envelope_editor (snd_state *ss)
{
  GtkWidget *mainform, *helpB, *leftbox, *bottombox, *leftframe, *toprow, *bottomrow;
  if (!enved_dialog)
    {
      enved_dialog = gtk_dialog_new();
      g_signal_connect_closure_by_id(GTK_OBJECT(enved_dialog),
				     g_signal_lookup("delete_event", G_OBJECT_TYPE(GTK_OBJECT(enved_dialog))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(delete_enved_dialog), (gpointer)ss, 0),
				     0);
      gtk_window_set_title(GTK_WINDOW(enved_dialog), _("Edit Envelope"));
      sg_make_resizable(enved_dialog);
      set_background(enved_dialog, (ss->sgx)->basic_color);
      gtk_container_set_border_width(GTK_CONTAINER(enved_dialog), 4);
      gtk_widget_realize(enved_dialog);
      gtk_window_resize(GTK_WINDOW(enved_dialog), 500, 500);

      gc = gdk_gc_new(MAIN_WINDOW(ss));
      gdk_gc_set_background(gc, (ss->sgx)->white);
      gdk_gc_set_foreground(gc, (ss->sgx)->black);

      rgc = gdk_gc_new(MAIN_WINDOW(ss));
      gdk_gc_set_background(rgc, (ss->sgx)->white);
      gdk_gc_set_foreground(rgc, (ss->sgx)->red);

      ggc = gdk_gc_new(MAIN_WINDOW(ss));
      gdk_gc_set_background(ggc, (ss->sgx)->white);
      gdk_gc_set_foreground(ggc, (ss->sgx)->enved_waveform_color);

      helpB = gtk_button_new_with_label(_("Help"));
      cancelB = gtk_button_new_with_label(_("Dismiss"));
      applyB = gtk_button_new_with_label(_("Apply"));
      apply2B = gtk_button_new_with_label(_("Undo&Apply"));
      resetB = gtk_button_new_with_label(_("Reset"));
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(enved_dialog)->action_area), cancelB, false, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(enved_dialog)->action_area), applyB, false, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(enved_dialog)->action_area), apply2B, false, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(enved_dialog)->action_area), resetB, false, true, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(enved_dialog)->action_area), helpB, false, true, 10);
      g_signal_connect_closure_by_id(GTK_OBJECT(cancelB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(cancelB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(dismiss_enved_callback), (gpointer)ss, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(applyB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(applyB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(apply_enved_callback), (gpointer)ss, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(apply2B),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(apply2B))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(undo_and_apply_enved_callback), (gpointer)ss, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(resetB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(resetB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(reset_button_pressed), (gpointer)ss, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(helpB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(helpB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(help_enved_callback), (gpointer)ss, 0),
				     0);
      gtk_widget_show(cancelB);
      gtk_widget_show(applyB);
      gtk_widget_show(apply2B);
      gtk_widget_show(resetB);
      gtk_widget_show(helpB);

      mainform = gtk_hbox_new(false, 0); /* buttons + graph */
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(enved_dialog)->vbox), mainform, true, true, 0);
      set_background(mainform, (ss->sgx)->basic_color);

      leftframe = gtk_frame_new(NULL);
      gtk_box_pack_start(GTK_BOX(mainform), leftframe, false, false, 0);
      gtk_frame_set_shadow_type(GTK_FRAME(leftframe), GTK_SHADOW_ETCHED_IN);
      gtk_widget_show(leftframe);

      leftbox = gtk_vbox_new(false, 0);
      gtk_container_add(GTK_CONTAINER(leftframe), leftbox);
      gtk_widget_show(leftbox);
      
      bottombox = gtk_vbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(enved_dialog)->vbox), bottombox, false, false, 0);
      gtk_widget_show(bottombox);

      drawer = gtk_drawing_area_new();
      gtk_box_pack_start(GTK_BOX(mainform), drawer, true, true, 0);
      gtk_widget_set_events(drawer, GDK_ALL_EVENTS_MASK);
      set_background(drawer, (ss->sgx)->white);
      set_foreground(drawer, (ss->sgx)->black);
      gtk_widget_show(drawer);

      showB = gtk_button_new_with_label(_("view envs"));
      set_backgrounds(showB, (ss->sgx)->basic_color);
      gtk_box_pack_start(GTK_BOX(leftbox), showB, false, false, BB_MARGIN);
      g_signal_connect_closure_by_id(GTK_OBJECT(showB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(showB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(show_button_pressed), (gpointer)ss, 0),
				     0);
      gtk_widget_show(showB);

      saverow = gtk_hbox_new(false, BB_MARGIN);
      gtk_box_pack_start(GTK_BOX(leftbox), saverow, false, false, BB_MARGIN);
      gtk_widget_show(saverow);

      saveB = gtk_button_new_with_label(_(" save "));
      set_backgrounds(saveB, (ss->sgx)->basic_color);
      gtk_box_pack_start(GTK_BOX(saverow), saveB, true, true, BB_MARGIN);
      g_signal_connect_closure_by_id(GTK_OBJECT(saveB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(saveB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(save_button_pressed), (gpointer)ss, 0),
				     0);
      gtk_widget_show(saveB);

      printB = gtk_button_new_with_label(_(" print  "));
      set_backgrounds(printB, (ss->sgx)->basic_color);
      gtk_box_pack_start(GTK_BOX(saverow), printB, true, true, BB_MARGIN);
      g_signal_connect_closure_by_id(GTK_OBJECT(printB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(printB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(print_button_pressed), (gpointer)ss, 0),
				     0);
      gtk_widget_show(printB);

      revrow = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(leftbox), revrow, false, false, BB_MARGIN);
      gtk_widget_show(revrow);

      revertB = gtk_button_new_with_label(_("revert "));
      set_backgrounds(revertB, (ss->sgx)->basic_color);
      gtk_box_pack_start(GTK_BOX(revrow), revertB, true, true, BB_MARGIN);
      g_signal_connect_closure_by_id(GTK_OBJECT(revertB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(revertB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(revert_button_pressed), (gpointer)ss, 0),
				     0);
      gtk_widget_show(revertB);

      deleteB = gtk_button_new_with_label(_("delete"));
      set_backgrounds(deleteB, (ss->sgx)->basic_color);
      gtk_box_pack_start(GTK_BOX(revrow), deleteB, true, true, BB_MARGIN);
      g_signal_connect_closure_by_id(GTK_OBJECT(deleteB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(deleteB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(delete_button_pressed), (gpointer)ss, 0),
				     0);
      gtk_widget_show(deleteB);

      unrow = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(leftbox), unrow, false, false, BB_MARGIN);
      gtk_widget_show(unrow);

      undoB = gtk_button_new_with_label(_(" undo "));
      set_backgrounds(undoB, (ss->sgx)->basic_color);
      gtk_box_pack_start(GTK_BOX(unrow), undoB, true, true, BB_MARGIN);
      g_signal_connect_closure_by_id(GTK_OBJECT(undoB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(undoB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(undo_button_pressed), (gpointer)ss, 0),
				     0);
      gtk_widget_show(undoB);

      redoB = gtk_button_new_with_label(_(" redo "));
      set_backgrounds(redoB, (ss->sgx)->basic_color);
      gtk_box_pack_start(GTK_BOX(unrow), redoB, true, true, BB_MARGIN);
      g_signal_connect_closure_by_id(GTK_OBJECT(redoB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(redoB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(redo_button_pressed), (gpointer)ss, 0),
				     0);
      gtk_widget_show(redoB);

      rbrow = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(leftbox), rbrow, false, false, BB_MARGIN);
      gtk_widget_show(rbrow);

      ampB = gtk_button_new_with_label(_("amp"));
      set_backgrounds(ampB, (ss->sgx)->basic_color);
      gtk_box_pack_start(GTK_BOX(rbrow), ampB, true, true, BB_MARGIN);
      g_signal_connect_closure_by_id(GTK_OBJECT(ampB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(ampB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(amp_button_pressed), (gpointer)ss, 0),
				     0);
      gtk_widget_show(ampB);

      fltB = gtk_button_new_with_label(_("flt"));
      set_backgrounds(fltB, (ss->sgx)->basic_color);
      gtk_box_pack_start(GTK_BOX(rbrow), fltB, true, true, BB_MARGIN);
      g_signal_connect_closure_by_id(GTK_OBJECT(fltB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(fltB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(flt_button_pressed), (gpointer)ss, 0),
				     0);
      gtk_widget_show(fltB);

      srcB = gtk_button_new_with_label(_("src"));
      set_backgrounds(srcB, (ss->sgx)->basic_color);
      gtk_box_pack_start(GTK_BOX(rbrow), srcB, true, true, BB_MARGIN);
      g_signal_connect_closure_by_id(GTK_OBJECT(srcB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(srcB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(src_button_pressed), (gpointer)ss, 0),
				     0);
      gtk_widget_show(srcB);

      lerow = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(leftbox), lerow, false, false, BB_MARGIN);
      gtk_widget_show(lerow);

      linB = gtk_button_new_with_label(_("linear"));
      set_backgrounds(linB, (ss->sgx)->basic_color);
      gtk_box_pack_start(GTK_BOX(lerow), linB, true, true, BB_MARGIN);
      g_signal_connect_closure_by_id(GTK_OBJECT(linB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(linB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(lin_button_pressed), (gpointer)ss, 0),
				     0);
      gtk_widget_show(linB);

      expB = gtk_button_new_with_label(_("exp"));
      set_backgrounds(expB, (ss->sgx)->basic_color);
      gtk_box_pack_start(GTK_BOX(lerow), expB, true, true, BB_MARGIN);
      g_signal_connect_closure_by_id(GTK_OBJECT(expB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(expB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(exp_button_pressed), (gpointer)ss, 0),
				     0);
      gtk_widget_show(expB);

      selrow = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(leftbox), selrow, false, false, BB_MARGIN);
      gtk_widget_show(selrow);

      selectionB = gtk_button_new_with_label(_("selection"));
      set_backgrounds(selectionB, (ss->sgx)->basic_color);
      gtk_box_pack_start(GTK_BOX(selrow), selectionB, true, true, BB_MARGIN);
      g_signal_connect_closure_by_id(GTK_OBJECT(selectionB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(selectionB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(selection_button_pressed), (gpointer)ss, 0),
				     0);
      gtk_widget_show(selectionB);

      mixB = gtk_button_new_with_label(_("mix"));
      set_backgrounds(mixB, (ss->sgx)->basic_color);
      gtk_box_pack_start(GTK_BOX(selrow), mixB, true, true, BB_MARGIN);
      g_signal_connect_closure_by_id(GTK_OBJECT(mixB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(mixB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(mix_button_pressed), (gpointer)ss, 0),
				     0);
      gtk_widget_show(mixB);

      env_list = sg_make_list(_("envs:"), leftbox, BOX_PACK, (gpointer)ss, 0, NULL, GTK_SIGNAL_FUNC(env_browse_callback),0,0,0,0);
      if (enved_all_envs_top() > 0) make_scrolled_env_list(ss);
      gtk_widget_show(env_list);

      toprow = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(bottombox), toprow, false, false, 0);
      gtk_widget_show(toprow);

      bottomrow = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(bottombox), bottomrow, false, false, 4);
      gtk_widget_show(bottomrow);

      nameL = gtk_label_new(_("amp env:"));
      gtk_box_pack_start(GTK_BOX(toprow), nameL, false, false, 0);
      gtk_widget_show(nameL);

      textL = snd_entry_new(ss, toprow, true);
      g_signal_connect_closure_by_id(GTK_OBJECT(textL),
				     g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(textL))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(text_field_activated), (gpointer)ss, 0),
				     0);

      blank = gdk_pixmap_create_from_xpm_d(MAIN_WINDOW(ss), NULL, NULL, blank_bits());
      brkpixL = gtk_drawing_area_new();
      gtk_widget_set_events(brkpixL, GDK_EXPOSURE_MASK);
      set_background(brkpixL, (ss->sgx)->basic_color);
      gtk_widget_set_size_request(brkpixL, 16, 16);
      gtk_box_pack_start(GTK_BOX(toprow), brkpixL, false, false, 0);
      gtk_widget_show(brkpixL);
      g_signal_connect_closure_by_id(GTK_OBJECT(brkpixL),
				     g_signal_lookup("expose_event", G_OBJECT_TYPE(GTK_OBJECT(brkpixL))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(brkpixL_expose), (gpointer)ss, 0),
				     0);
      
      brktxtL = gtk_label_new(NULL);
      set_background(brktxtL, (ss->sgx)->basic_color);
      gtk_box_pack_start(GTK_BOX(toprow), brktxtL, false, false, 0);
      gtk_widget_show(brktxtL);

      clipB = gtk_check_button_new_with_label(_("clip"));
      g_signal_connect_closure_by_id(GTK_OBJECT(clipB),
				     g_signal_lookup("toggled", G_OBJECT_TYPE(GTK_OBJECT(clipB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(clip_button_callback), (gpointer)ss, 0),
				     0);
      gtk_box_pack_start(GTK_BOX(toprow), clipB, false, false, 0);
      gtk_widget_show(clipB);

      graphB = gtk_check_button_new_with_label(_("wave"));
      g_signal_connect_closure_by_id(GTK_OBJECT(graphB),
				     g_signal_lookup("toggled", G_OBJECT_TYPE(GTK_OBJECT(graphB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(graph_button_callback), (gpointer)ss, 0),
				     0);
      gtk_box_pack_start(GTK_BOX(toprow), graphB, false, false, 0);
      gtk_widget_show(graphB);

      dBB = gtk_check_button_new_with_label(_("dB"));
      g_signal_connect_closure_by_id(GTK_OBJECT(dBB),
				     g_signal_lookup("toggled", G_OBJECT_TYPE(GTK_OBJECT(dBB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(dB_button_callback), (gpointer)ss, 0),
				     0);
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
      set_background(baseScale, (ss->sgx)->position_color);
      g_signal_connect_closure_by_id(GTK_OBJECT(baseAdj),
				     g_signal_lookup("value_changed", G_OBJECT_TYPE(GTK_OBJECT(baseAdj))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(base_changed_callback), (gpointer)ss, 0),
				     0);
      gtk_box_pack_start(GTK_BOX(bottomrow), baseScale, true, true, 4);
      gtk_widget_show(baseScale);

      orderAdj = gtk_adjustment_new(20, 2, 100000, 2, 10, 0);
      orderL = gtk_spin_button_new(GTK_ADJUSTMENT(orderAdj), 0.0, 0);
      gtk_box_pack_end(GTK_BOX(bottomrow), orderL, false, false, 4);
      gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(orderL), true);
      g_signal_connect_closure_by_id(GTK_OBJECT(orderAdj),
				     g_signal_lookup("value_changed", G_OBJECT_TYPE(GTK_OBJECT(orderAdj))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(enved_filter_order_callback), (gpointer)ss, 0),
				     0);
      gtk_widget_show(orderL);

      firB = gtk_button_new_with_label((FIR_p) ? "fir" : "fft");
      g_signal_connect_closure_by_id(GTK_OBJECT(firB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(firB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(fir_button_pressed), (gpointer)ss, 0),
				     0);
      gtk_box_pack_end(GTK_BOX(bottomrow), firB, false, false, 0);
      gtk_widget_show(firB);

      gtk_widget_show(mainform);
      gtk_widget_show(enved_dialog);

      g_signal_connect_closure_by_id(GTK_OBJECT(drawer),
				     g_signal_lookup("expose_event", G_OBJECT_TYPE(GTK_OBJECT(drawer))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(drawer_expose), (gpointer)ss, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(drawer),
				     g_signal_lookup("configure_event", G_OBJECT_TYPE(GTK_OBJECT(drawer))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(drawer_resize), (gpointer)ss, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(drawer),
				     g_signal_lookup("button_press_event", G_OBJECT_TYPE(GTK_OBJECT(drawer))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(drawer_button_press), (gpointer)ss, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(drawer),
				     g_signal_lookup("button_release_event", G_OBJECT_TYPE(GTK_OBJECT(drawer))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(drawer_button_release), (gpointer)ss, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(drawer),
				     g_signal_lookup("motion_notify_event", G_OBJECT_TYPE(GTK_OBJECT(drawer))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(drawer_button_motion), (gpointer)ss, 0),
				     0);

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

      set_toggle_button(clipB, enved_clip_p(ss), false, (void *)ss);
      set_toggle_button(graphB, enved_wave_p(ss), false, (void *)ss);
      set_toggle_button(dBB, enved_in_dB(ss), false, (void *)ss);

      reflect_apply_state(ss);
      reflect_segment_state(ss);
      set_dialog_widget(ss, ENVED_DIALOG, enved_dialog);
    }
  else raise_dialog(enved_dialog);
  active_channel = current_channel(ss);
  set_sensitive(mixB, (any_mix_id() != INVALID_MIX_ID));
  return(enved_dialog);
}

void set_enved_clip_p(snd_state *ss, bool val) 
{
  in_set_enved_clip_p(ss, val); 
  if (enved_dialog) set_toggle_button(clipB, val, false, (void *)ss);
}

void set_enved_exp_p(snd_state *ss, bool val) 
{
  in_set_enved_exp_p(ss, val); 
  /* if (enved_dialog) set_toggle_button(expB, val, false, (void *)ss);  */ /* not a toggle button */
  reflect_segment_state(ss);
}

void set_enved_target(snd_state *ss, enved_target_t val) 
{
  in_set_enved_target(ss, val); 
  if (enved_dialog) reflect_apply_state(ss);
}

void set_enved_wave_p(snd_state *ss, bool val) 
{
  in_set_enved_wave_p(ss, val); 
  if (enved_dialog) set_toggle_button(graphB, val, false, (void *)ss);
}

void set_enved_in_dB(snd_state *ss, bool val) 
{
  in_set_enved_in_dB(ss, val); 
  if (enved_dialog) set_toggle_button(dBB, val, false, (void *)ss);
}

void set_enved_base(snd_state *ss, Float val) 
{
  in_set_enved_base(ss, val); 
  if (enved_dialog) reflect_changed_base(ss, val);
}

int enved_dialog_is_active(void)
{
  return((enved_dialog) && (GTK_WIDGET_VISIBLE(enved_dialog)));
}

void set_enved_filter_order(snd_state *ss, int order)
{
  char str[LABEL_BUFFER_SIZE];
  if ((order > 0) && (order < 2000))
    {
      if (order & 1) 
	in_set_enved_filter_order(ss, order + 1);
      else in_set_enved_filter_order(ss, order);
      if (enved_dialog)
	{
	  mus_snprintf(str, LABEL_BUFFER_SIZE, "%d", enved_filter_order(ss));
	  gtk_entry_set_text(GTK_ENTRY(orderL), str);
	  if ((enved_target(ss) == ENVED_SPECTRUM) && 
	      (enved_wave_p(ss)) && 
	      (!showing_all_envs)) 
	    env_redisplay(ss);
	}
    }
}

void enved_reflect_selection(bool on)
{
  snd_state *ss;
  if ((enved_dialog) && (!within_selection_src))
    {
      ss = get_global_state();
      set_sensitive(selectionB, on);
      if ((apply_to_selection) && (!on))
	{
	  apply_to_selection = false;
	  set_background(selectionB, (ss->sgx)->basic_color);
	}
      if ((enved_target(ss) != ENVED_SPECTRUM) && 
	  (enved_wave_p(ss)) && 
	  (!showing_all_envs)) 
	env_redisplay(ss);
    }
}

void color_enved_waveform(GdkColor *pix)
{
  snd_state *ss;
  ss = get_global_state();
  (ss->sgx)->enved_waveform_color = pix;
  if (enved_dialog)
    {
      gdk_gc_set_foreground(ggc, pix);
      if ((enved_wave_p(ss)) && (enved_dialog)) env_redisplay(ss);
    }
}

void reflect_mix_in_enved(void)
{
  if (enved_dialog)
    set_sensitive(mixB, (any_mix_id() != INVALID_MIX_ID));
}


static int find_named_env(XEN name)
{
  int pos;
  char *env_name;
  if (XEN_STRING_P(name))
    env_name = XEN_TO_C_STRING(name);
  else env_name = XEN_SYMBOL_TO_C_STRING(name);
  pos = find_env(env_name);
  if (pos == -1)
    XEN_ERROR(NO_SUCH_ENVELOPE, 
	      XEN_LIST_1(name));
  return(pos);
}

static XEN g_enved_active_env(void)
{
  #define H_enved_active_env "(" S_enved_active_env "): current envelope editor displayed (active) envelope"
  return(env_to_xen(active_env));
}

static XEN g_set_enved_active_env(XEN e)
{
  XEN_ASSERT_TYPE(XEN_LIST_P(e) || XEN_STRING_P(e) || XEN_SYMBOL_P(e), e, XEN_ONLY_ARG, S_setB S_enved_active_env, "a list, symbol, or string");
  if (active_env) active_env = free_env(active_env);
  if ((XEN_STRING_P(e)) || (XEN_SYMBOL_P(e)))
    active_env = copy_env(enved_all_envs(find_named_env(e)));
  else active_env = xen_to_env(e);
  if (enved_dialog) 
    env_redisplay(get_global_state());
  return(e);
}

static XEN g_enved_selected_env(void)
{
  #define H_enved_selected_env "(" S_enved_selected_env "): current envelope editor selected envelope"
  return(env_to_xen(selected_env));
}

static XEN g_set_enved_selected_env(XEN name)
{
  int pos;
  XEN_ASSERT_TYPE(XEN_STRING_P(name) || XEN_SYMBOL_P(name), name, XEN_ONLY_ARG, S_setB S_enved_selected_env, "a string or symbol");
  pos = find_named_env(name);
  if (pos >= 0)
    {
      selected_env = enved_all_envs(pos);
      if (enved_dialog)
	{
	  sg_list_select(env_list, pos);
	  sg_list_moveto(env_list, pos);
	}
    }
  else XEN_ERROR(NO_SUCH_ENVELOPE,
		 XEN_LIST_2(C_TO_XEN_STRING(S_setB S_enved_selected_env),
			    name));
  return(name);
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

#if DEBUGGING
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
				  XEN_CONS(XEN_WRAP_WIDGET(mixB),
				   XEN_CONS(XEN_WRAP_WIDGET(resetB),
				    XEN_CONS(XEN_WRAP_WIDGET(env_list),
				     XEN_CONS(XEN_WRAP_WIDGET(firB),
				      XEN_EMPTY_LIST))))))))))))))))))))))))))));
  return(XEN_EMPTY_LIST);
}

static XEN g_enved_axis_info(void)
{
  axis_info *ap;
  if (enved_dialog)
    {
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
XEN_NARGIFY_0(g_enved_active_env_w, g_enved_active_env)
XEN_NARGIFY_1(g_set_enved_active_env_w, g_set_enved_active_env)
XEN_NARGIFY_0(g_enved_selected_env_w, g_enved_selected_env)
XEN_NARGIFY_1(g_set_enved_selected_env_w, g_set_enved_selected_env)
#if DEBUGGING
XEN_NARGIFY_0(g_enved_dialog_widgets_w, g_enved_dialog_widgets)
XEN_NARGIFY_0(g_enved_axis_info_w, g_enved_axis_info)
#endif
#else
#define g_enved_filter_w g_enved_filter
#define g_set_enved_filter_w g_set_enved_filter
#define g_enved_active_env_w g_enved_active_env
#define g_set_enved_active_env_w g_set_enved_active_env
#define g_enved_selected_env_w g_enved_selected_env
#define g_set_enved_selected_env_w g_set_enved_selected_env
#if DEBUGGING
#define g_enved_dialog_widgets_w g_enved_dialog_widgets
#define g_enved_axis_info_w g_enved_axis_info
#endif
#endif

void g_init_gxenv(void)
{
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_filter, g_enved_filter_w, H_enved_filter,
				   S_setB S_enved_filter, g_set_enved_filter_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_active_env, g_enved_active_env_w, H_enved_active_env,
				   S_setB S_enved_active_env, g_set_enved_active_env_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_selected_env, g_enved_selected_env_w, H_enved_selected_env,
				   S_setB S_enved_selected_env, g_set_enved_selected_env_w,  0, 0, 1, 0);

#if DEBUGGING
  XEN_DEFINE_PROCEDURE("enved-dialog-widgets", g_enved_dialog_widgets_w, 0, 0, 0, "");
  XEN_DEFINE_PROCEDURE("enved-axis-info",  g_enved_axis_info_w, 0, 0, 0, "");
#endif
}

