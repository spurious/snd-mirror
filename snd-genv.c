#include "snd.h"

/* envelope editor and viewer */

static GtkWidget *enved_dialog = NULL;
static GtkWidget *applyB, *apply2B, *cancelB, *drawer, *showB, *saveB, *resetB;
static GtkWidget *revertB, *undoB, *redoB, *printB, *brktxtL, *brkpixL, *graphB, *fltB, *ampB, *srcB, *rbrow, *clipB, *deleteB;
static GtkWidget *nameL, *textL, *env_list, *env_list_frame, *env_list_scroller, *dBB, *orderL;
static GtkWidget *expB, *linB, *lerow, *baseScale, *baseLabel, *baseValue, *selectionB, *mixB, *selrow, *revrow, *unrow, *saverow;
static GtkObject *baseAdj, *orderAdj;
static GdkGC *gc, *rgc, *ggc;

static char *env_names[3] = {STR_amp_env_p, STR_flt_env_p, STR_src_env_p};

static int showing_all_envs = 0; /* edit one env (0), or view all currently defined envs (1) */
static int apply_to_selection = 0;
static int apply_to_mix = 0;

static int env_window_width = 0;
static int env_window_height = 0;

static chan_info *active_channel = NULL, *last_active_channel = NULL;

static env* selected_env = NULL; /* if during view, one env is clicked, it is "selected" and can be pasted elsewhere */
static env* active_env = NULL;   /* env currently being edited */

static chan_info *axis_cp = NULL;
static axis_info *gray_ap = NULL;

chan_info *enved_make_axis_cp(snd_state *ss, char *name, axis_context *ax, 
			      int ex0, int ey0, int width, int height, Float xmin, Float xmax, Float ymin, Float ymax)
{
  /* conjure up minimal context for axis drawer in snd-axis.c */
  if (!axis_cp) 
    {
      axis_cp = new_env_axis(ss);
      fixup_axis_context(axis_cp->axis->ax, drawer, ax->gc);
    }
  if (!gray_ap) 
    {
      gray_ap = new_wave_axis(ss);
      fixup_axis_context(gray_ap->ax, drawer, ggc);
    }
  init_env_axes(axis_cp, name, ex0, ey0, width, height, xmin, xmax, ymin, ymax);
  return(axis_cp);
}

static void display_env(snd_state *ss, env *e, char *name, GdkGC *cur_gc, int x0, int y0, int width, int height, int dots)
{
  axis_context *ax = NULL;  
  ax = (axis_context *)CALLOC(1, sizeof(axis_context));
  ax->wn = drawer->window;
  ax->gc = cur_gc;
  display_enved_env(ss, e, ax, axis_cp, name, x0, y0, width, height, dots);
  ax = free_axis_context(ax);
}

void display_enved_env_with_selection(snd_state *ss, env *e, char *name, int x0, int y0, int width, int height, int dots)
{
  display_env(ss, e, name, (selected_env == e) ? rgc : gc, x0, y0, width, height, dots);
}

static void do_env_edit(env *new_env, int loading)
{
  do_enved_edit(new_env);
  if (!loading)
    {
      set_sensitive(undoB, TRUE);
      set_sensitive(redoB, FALSE);
      set_sensitive(saveB, TRUE);
      set_sensitive(revertB, TRUE);
    }
}

void set_enved_redo_sensitive(int val) {set_sensitive(redoB, val);}
void set_enved_revert_sensitive(int val) {set_sensitive(revertB, val);}
void set_enved_undo_sensitive(int val) {set_sensitive(undoB, val);}
void set_enved_save_sensitive(int val) {set_sensitive(saveB, val);}
void set_enved_show_sensitive(int val) {set_sensitive(showB, val);}

void make_scrolled_env_list (snd_state *ss)
{
  int n, size;
  char *str;
  size = enved_all_envs_top();
  gtk_clist_clear(GTK_CLIST(env_list));
  set_background(env_list, (ss->sgx)->highlight_color);
  for (n = 0; n < size; n++) 
    {
      str = enved_all_names(n);
      gtk_clist_append(GTK_CLIST(env_list), &str);
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

static void Dismiss_Enved_Callback(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(enved_dialog);
}

static void delete_enved_dialog(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(enved_dialog);
}

static void Help_Enved_Callback(GtkWidget *w, gpointer context)
{
  envelope_editor_dialog_help((snd_state *)context);
}

static int within_selection_src = 0;

static void apply_enved(snd_state *ss)
{
  int mix_id = 0, i, j, chan;
  env *max_env = NULL;
  snd_info *sp;
  if (active_env)
    {
      if (apply_to_mix)
	{
	  if (ss->selected_mix != NO_SELECTION)
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
	  set_sensitive(applyB, FALSE);
	  set_sensitive(apply2B, FALSE);
	  set_button_label(cancelB, STR_Stop);
	  check_for_event(ss);
	  switch (enved_target(ss))
	    {
	    case ENVED_AMPLITUDE:
	      if (apply_to_mix)
		{
		  set_mix_amp_env(mix_id, NO_SELECTION, active_env); /* chan = NO_SELECTION: use selected chan if more than 1 */
		  active_channel = current_channel(ss);
		}
	      else apply_env(active_channel, active_env, 0, 
			     current_ed_samples(active_channel), 1.0, 
			     apply_to_selection, FROM_ENVED, 
			     "Enved: amp", NULL,
			     TO_SCM_INT(AT_CURRENT_EDIT_POSITION), 0); 
	      /* calls update_graph, I think, but in short files that doesn't update the amp-env */
	      if (enved_wave_p(ss)) env_redisplay(ss);
	      break;
	    case ENVED_SPECTRUM: 
	      apply_filter(active_channel, enved_filter_order(ss), active_env, 
			   FROM_ENVED, "Enved: flt", apply_to_selection, NULL, NULL,
			   TO_SCM_INT(AT_CURRENT_EDIT_POSITION), 0);
	      break;
	    case ENVED_SRATE:
	      max_env = copy_env(active_env);
	      for (i = 0, j = 1; i < max_env->pts; i++, j += 2)
		if (max_env->data[j] < .01) max_env->data[j] = .01;
	      within_selection_src = 1;
	      src_env_or_num(ss, active_channel, max_env, 0.0, 
			     FALSE, FROM_ENVED, "Enved: src", apply_to_selection, NULL,
			     TO_SCM_INT(AT_CURRENT_EDIT_POSITION), 0);
	      within_selection_src = 0;
	      max_env = free_env(max_env);
	      if (enved_wave_p(ss)) env_redisplay(ss);
	      break;
	    }
	  set_sensitive(applyB, TRUE);
	  set_sensitive(apply2B, TRUE);
	  set_button_label(cancelB, STR_Dismiss);
	}
    }
}

void env_redisplay(snd_state *ss)
{
  char *name = NULL;
  if (enved_dialog_is_active())
    {
      gdk_window_clear(drawer->window);
      if (showing_all_envs) 
	view_envs(ss, env_window_width, env_window_height);
      else 
	{
	  name = gtk_entry_get_text(GTK_ENTRY(textL));
	  if (!name) name = "noname";
	  display_env(ss, active_env, name, gc, 0, 0, env_window_width, env_window_height, 1);
	  if (enved_wave_p(ss))
	    {
	      if ((enved_target(ss) == ENVED_SPECTRUM) && (active_env))
		display_frequency_response(ss, active_env, axis_cp->axis, gray_ap->ax, enved_filter_order(ss), enved_in_dB(ss));
	      else enved_show_background_waveform(ss, axis_cp, gray_ap, apply_to_mix, apply_to_selection);
	    }
	}
    }
}

static void enved_filter_order_callback(GtkWidget *w, gpointer data)
{
  set_enved_filter_order((snd_state *)data, gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(orderL)));
}

static void text_field_activated(GtkWidget *w, gpointer context)
{ /* might be breakpoints to load or an envelope name (<cr> in enved text field) */
  snd_state *ss = (snd_state *)context;
  char *name = NULL, *str;
  env *e = NULL;
  name = gtk_entry_get_text(GTK_ENTRY(w));
  if ((name) && (*name))
    {
      str = name;
      while (isspace((int)(*str))) str++;
      e = name_to_env(str);
      if (!e)
	{
	  if (isalpha((int)(str[0])))
	    {
	      alert_envelope_editor(ss, str, copy_env(active_env));
	      add_or_edit_symbol(str, active_env);
	      set_sensitive(saveB, FALSE);
	      env_redisplay(ss); /* updates label */
	    }
	  else e = string2env(str);
	}
      if (e) 
	{
	  if (active_env) active_env = free_env(active_env);
	  active_env = copy_env(e);
	  set_enved_env_list_top(0);
	  do_env_edit(active_env, TRUE);
	  set_sensitive(saveB, TRUE);
	  env_redisplay(ss);
	  free_env(e);
	}
    }
}

static void save_button_pressed(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  char *name = NULL;
  name = gtk_entry_get_text(GTK_ENTRY(textL));
  if ((!name) || (!(*name))) 
    name = "unnamed";
  alert_envelope_editor(ss, name, copy_env(active_env));
  add_or_edit_symbol(name, active_env);
  set_sensitive(saveB, FALSE);
  env_redisplay(ss);
}

static void Apply_Enved_Callback(GtkWidget *w, gpointer context)
{
  /* apply current envs to currently sync'd channels */
  apply_enved((snd_state *)context);
  last_active_channel = active_channel;
}

static void Undo_and_Apply_Enved_Callback(GtkWidget *w, gpointer context)
{
  /* undo upto previous amp env, then apply */
  /* this blindly undoes the previous edit (assumed to be an envelope) -- if the user made some other change in the meantime, too bad */
  snd_state *ss = (snd_state *)context;
  if ((active_channel) && (active_channel == last_active_channel))
    {
      active_channel->squelch_update = 1;
      undo_edit_with_sync(active_channel, 1);
      active_channel->squelch_update = 0;
    }
  apply_enved(ss);
  last_active_channel = active_channel;
}

static void reflect_segment_state (snd_state *ss)
{
  if (enved_dialog)
    {
      set_backgrounds(expB, (enved_exp_p(ss)) ? (ss->sgx)->yellow : (ss->sgx)->highlight_color);
      set_backgrounds(linB, (enved_exp_p(ss)) ? (ss->sgx)->highlight_color : (ss->sgx)->yellow);
      if ((active_env) && (!(showing_all_envs))) env_redisplay(ss);
    }
}

static void select_or_edit_env(snd_state *ss, int pos)
{
  if (selected_env == enved_all_envs(pos)) 
    {
      if (showing_all_envs)
	{
	  showing_all_envs = 0;
	  set_button_label(showB, STR_view_envs);
	}
      if (active_env) active_env = free_env(active_env);
      active_env = copy_env(enved_all_envs(pos));
      gtk_entry_set_text(GTK_ENTRY(textL), enved_all_names(pos));
      set_enved_env_list_top(0);
      do_env_edit(active_env, TRUE);
      set_sensitive(undoB, FALSE);
      set_sensitive(revertB, FALSE);
      set_sensitive(saveB, FALSE);
      set_enved_exp_p(ss, (active_env->base != 1.0));
      set_enved_base(ss, active_env->base);
      env_redisplay(ss);
    }
  else
    {
      selected_env = enved_all_envs(pos);
      if (showing_all_envs) view_envs(ss, env_window_width, env_window_height);
    }
  set_sensitive(deleteB, TRUE);
}

static void clear_point_label(void)
{
  set_blank_pixmap(brkpixL);
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

void display_enved_progress(char *str, GdkPixmap *pix, GdkBitmap *mask)
{
  if (pix == NULL)
    gtk_label_set_text(GTK_LABEL(brktxtL), str);
  else gtk_pixmap_set(GTK_PIXMAP(brkpixL), pix, mask);
}

static TIME_TYPE down_time;
static int env_dragged = 0;
static int env_pos = 0;
static int click_to_delete = 0;

static void drawer_button_motion(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{
  snd_state *ss = (snd_state *)data;
  int evx, evy;
  GdkModifierType state;
  TIME_TYPE motion_time;
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
	  if ((motion_time - down_time) < 100) return;
	}
    }
  else return;
  if (!showing_all_envs)
    {
      env_dragged = 1;
      click_to_delete = 0;
      ap = axis_cp->axis;
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
}

static void drawer_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_state *ss = (snd_state *)data;
  int pos;
  down_time = ev->time;
  env_dragged = 0;
  if (showing_all_envs)
    {
      pos = hit_env((int)(ev->x), (int)(ev->y), env_window_width, env_window_height);
      gtk_clist_select_row(GTK_CLIST(env_list), pos, 0);
      if ((pos >= 0) && 
	  (pos < enved_all_envs_top())) 
	select_or_edit_env(ss, pos);
    }
  else
    {
      if (!active_env)
	{
	  active_env = default_env(1.0, 0.0);
	  active_env->base = 1.0;
	  env_redisplay(ss); /* needed to get current_xs set up correctly */
	}
      env_pos = enved_button_press_display(ss, axis_cp->axis, active_env, (int)(ev->x), (int)(ev->y));
    }
}

void set_enved_click_to_delete(int n) {click_to_delete = n;}

static void drawer_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  if (!showing_all_envs)
    {
      if ((click_to_delete) && 
	  (!env_dragged) && 
	  (env_pos != 0)) /* might want to protect last point also */
	{
	  if (check_enved_hook(active_env, env_pos, 0, 0, ENVED_DELETE_POINT) == 0)
	    delete_point(active_env, env_pos);
	}
      do_env_edit(active_env, FALSE);
      env_pos = 0;
      env_dragged = 0;
      click_to_delete = 0;
      env_redisplay((snd_state *)data);
      clear_point_label();
    }
}

static void drawer_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  env_window_width = widget_width(w);
  env_window_height = widget_height(w);
  env_redisplay((snd_state *)data);
}

static void drawer_resize(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  /* update display, can be either view of all envs or sequence of current envs */
  env_window_width = widget_width(w);
  env_window_height = widget_height(w);
  env_redisplay((snd_state *)data);
}

static void show_button_pressed(GtkWidget *w, gpointer context)
{
  /* if show all (as opposed to show current), loop through loaded LV_LISTs */
  showing_all_envs = (!showing_all_envs);
  set_button_label(showB, (showing_all_envs) ? STR_edit_env : STR_view_envs);
  env_redisplay((snd_state *)context);
}

static void selection_button_pressed(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  apply_to_selection = (!apply_to_selection);
  if (apply_to_selection) 
    {
      if (apply_to_mix) set_backgrounds(mixB, (ss->sgx)->highlight_color);
      apply_to_mix = 0;
    }
  set_backgrounds(selectionB, (apply_to_selection) ? (ss->sgx)->yellow : (ss->sgx)->highlight_color);
  if ((enved_target(ss) != ENVED_SPECTRUM) && (enved_wave_p(ss)) && (!showing_all_envs)) env_redisplay(ss);
}

static void mix_button_pressed(GtkWidget *w, gpointer data)
{
  snd_state *ss = (snd_state *)data;
  int chan = 0;
  int mxchan, mix_id = NO_SELECTION;
  apply_to_mix = (!apply_to_mix);
  if (apply_to_mix) 
    {
      if (apply_to_selection) set_backgrounds(selectionB, (ss->sgx)->highlight_color);
      apply_to_selection = 0;
      if (ss->selected_mix != NO_SELECTION) 
	mix_id = ss->selected_mix; 
      else
	{
	  mix_id = any_mix_id();
	  if (mix_id != NO_SELECTION) select_mix_from_id(mix_id);
	}
      if (mix_id != NO_SELECTION)
	{
	  mxchan = mix_selected_channel(mix_id);
	  if (mxchan != NO_SELECTION) chan = mxchan;
	  if (mix_amp_env_from_id(mix_id, chan))
	    {
	      if (active_env) active_env = free_env(active_env);
	      active_env = copy_env(mix_amp_env_from_id(mix_id, chan));
	      set_enved_env_list_top(0);
	      do_env_edit(active_env, TRUE);
	      set_sensitive(undoB, FALSE);
	      set_sensitive(revertB, FALSE);
	      set_sensitive(saveB, FALSE);
	      env_redisplay(ss);
	    }
	}
    }
  set_backgrounds(mixB, (apply_to_mix) ? (ss->sgx)->yellow : (ss->sgx)->highlight_color);
  if ((enved_target(ss) == ENVED_AMPLITUDE) && (enved_wave_p(ss)) && (!showing_all_envs)) env_redisplay(ss);
}

static void delete_button_pressed(GtkWidget *w, gpointer context)
{
  int i, len;
  if (selected_env)
    {
      len = enved_all_envs_top();
      for (i = 0; i < len; i++)
	if (selected_env == enved_all_envs(i))
	  {
	    delete_envelope((snd_state *)context, enved_all_names(i));
	    if (enved_all_envs_top() == 0) set_sensitive(deleteB, FALSE);
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
  gtk_label_set_text(GTK_LABEL(nameL), env_names[enved_target(ss)]);
  set_backgrounds(ampB, (enved_target(ss) == ENVED_AMPLITUDE) ? (ss->sgx)->green : (ss->sgx)->highlight_color);
  set_backgrounds(fltB, (enved_target(ss) == ENVED_SPECTRUM) ? (ss->sgx)->green : (ss->sgx)->highlight_color);
  set_backgrounds(srcB, (enved_target(ss) == ENVED_SRATE) ? (ss->sgx)->green : (ss->sgx)->highlight_color);
  if ((!showing_all_envs) && (enved_wave_p(ss))) env_redisplay(ss);
}

static void flt_button_pressed(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  in_set_enved_target(ss, ENVED_SPECTRUM);
  reflect_apply_state(ss);
}

static void amp_button_pressed(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  in_set_enved_target(ss, ENVED_AMPLITUDE);
  reflect_apply_state(ss);
}

static void src_button_pressed(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
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
  active_env = string2env("'(0 0 1 0)");
  set_enved_env_list_top(0);
  do_env_edit(active_env, TRUE);
  set_sensitive(saveB, TRUE);
  env_redisplay(ss);
}

static void reset_button_pressed(GtkWidget *w, gpointer context)
{
  enved_reset();
}

void enved_print(char *name)
{
  print_enved(name, axis_cp, env_window_height);
}

static void print_button_pressed(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  ss->print_choice = PRINT_ENV;
  File_Print_Callback(w, context);
}

static void env_browse_Callback(GtkWidget *w, gint row, gint column, GdkEventButton *event, gpointer context)
{
  select_or_edit_env((snd_state *)context, row);
}

static void Graph_Button_Callback(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context; 
  in_set_enved_wave_p(ss, GTK_TOGGLE_BUTTON(w)->active);
  env_redisplay(ss);
}

static void dB_Button_Callback(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context; 
  in_set_enved_in_dB(ss, GTK_TOGGLE_BUTTON(w)->active);
  env_redisplay(ss);
}

static void Clip_Button_Callback(GtkWidget *w, gpointer context)
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
	active_env->base = enved_base(ss);
      else active_env->base = 1.0;
      set_sensitive(saveB, TRUE);
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
	active_env->base = enved_base(ss);
      else active_env->base = 1.0;
      set_sensitive(saveB, TRUE);
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
      active_env->base = enved_base(ss);
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
  if ((active_env) && (enved_exp_p(ss))) set_sensitive(saveB, TRUE); /* what about undo/redo here? */
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

GtkWidget *create_envelope_editor (snd_state *ss)
{
  GtkWidget *mainform, *helpB, *leftbox, *bottombox, *leftframe, *toprow, *bottomrow, *brkbox;
  if (!enved_dialog)
    {
      enved_dialog = gtk_dialog_new();
      set_dialog_widget(ENVED_DIALOG, enved_dialog);
      gtk_signal_connect(GTK_OBJECT(enved_dialog), "delete_event", GTK_SIGNAL_FUNC(delete_enved_dialog), (gpointer)ss);
      gtk_window_set_title(GTK_WINDOW(enved_dialog), STR_Edit_Envelope);
      gtk_window_set_policy(GTK_WINDOW(enved_dialog), TRUE, TRUE, FALSE); /* allow shrink or grow */
      set_background(enved_dialog, (ss->sgx)->basic_color);
      gtk_container_set_border_width(GTK_CONTAINER(enved_dialog), 4);
      gtk_widget_realize(enved_dialog);
      add_dialog(ss, enved_dialog);
      gtk_widget_set_usize(GTK_WIDGET(enved_dialog), 500, 500);

      gc = gdk_gc_new(MAIN_WINDOW(ss));
      gdk_gc_set_background(gc, (ss->sgx)->white);
      gdk_gc_set_foreground(gc, (ss->sgx)->black);

      rgc = gdk_gc_new(MAIN_WINDOW(ss));
      gdk_gc_set_background(rgc, (ss->sgx)->white);
      gdk_gc_set_foreground(rgc, (ss->sgx)->red);

      ggc = gdk_gc_new(MAIN_WINDOW(ss));
      gdk_gc_set_background(ggc, (ss->sgx)->white);
      gdk_gc_set_foreground(ggc, (ss->sgx)->enved_waveform_color);

      helpB = gtk_button_new_with_label(STR_Help);
      cancelB = gtk_button_new_with_label(STR_Dismiss);
      applyB = gtk_button_new_with_label(STR_Apply);
      apply2B = gtk_button_new_with_label(STR_Undo_and_Apply);
      resetB = gtk_button_new_with_label(STR_Reset);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(enved_dialog)->action_area), cancelB, FALSE, TRUE, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(enved_dialog)->action_area), applyB, FALSE, TRUE, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(enved_dialog)->action_area), apply2B, FALSE, TRUE, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(enved_dialog)->action_area), resetB, FALSE, TRUE, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(enved_dialog)->action_area), helpB, FALSE, TRUE, 10);
      gtk_signal_connect(GTK_OBJECT(cancelB), "clicked", GTK_SIGNAL_FUNC(Dismiss_Enved_Callback), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(applyB), "clicked", GTK_SIGNAL_FUNC(Apply_Enved_Callback), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(apply2B), "clicked", GTK_SIGNAL_FUNC(Undo_and_Apply_Enved_Callback), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(resetB), "clicked", GTK_SIGNAL_FUNC(reset_button_pressed), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(helpB), "clicked", GTK_SIGNAL_FUNC(Help_Enved_Callback), (gpointer)ss);
      set_pushed_button_colors(helpB, ss);
      set_pushed_button_colors(cancelB, ss);
      set_pushed_button_colors(applyB, ss);
      set_pushed_button_colors(apply2B, ss);
      set_pushed_button_colors(resetB, ss);
      gtk_widget_show(cancelB);
      gtk_widget_show(applyB);
      gtk_widget_show(apply2B);
      gtk_widget_show(resetB);
      gtk_widget_show(helpB);

      mainform = gtk_hbox_new(FALSE, 0); /* buttons + graph */
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(enved_dialog)->vbox), mainform, TRUE, TRUE, 0);
      set_background(mainform, (ss->sgx)->basic_color);

      leftframe = gtk_frame_new(NULL);
      gtk_box_pack_start(GTK_BOX(mainform), leftframe, FALSE, FALSE, 0);
      gtk_frame_set_shadow_type(GTK_FRAME(leftframe), GTK_SHADOW_ETCHED_IN);
      gtk_widget_show(leftframe);

      leftbox = gtk_vbox_new(FALSE, 0);
      gtk_container_add(GTK_CONTAINER(leftframe), leftbox);
      set_background(leftbox, (ss->sgx)->highlight_color);
      gtk_widget_show(leftbox);
      
      bottombox = gtk_vbox_new(FALSE, 0);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(enved_dialog)->vbox), bottombox, FALSE, FALSE, 0);
      gtk_widget_show(bottombox);

      drawer = gtk_drawing_area_new();
      gtk_box_pack_start(GTK_BOX(mainform), drawer, TRUE, TRUE, 0);
      gtk_widget_set_events(drawer, GDK_ALL_EVENTS_MASK);
      set_background(drawer, (ss->sgx)->white);
      set_foreground(drawer, (ss->sgx)->black);
      gtk_widget_show(drawer);

      showB = gtk_button_new_with_label(STR_view_envs);
      set_backgrounds(showB, (ss->sgx)->highlight_color);
      gtk_box_pack_start(GTK_BOX(leftbox), showB, FALSE, FALSE, 0);
      gtk_signal_connect(GTK_OBJECT(showB), "clicked", GTK_SIGNAL_FUNC(show_button_pressed), (gpointer)ss);
      gtk_widget_show(showB);

      saverow = gtk_hbox_new(FALSE, 0);
      set_background(saverow, (ss->sgx)->highlight_color);
      gtk_box_pack_start(GTK_BOX(leftbox), saverow, FALSE, FALSE, 0);
      gtk_widget_show(saverow);

      saveB = gtk_button_new_with_label(" save ");
      set_backgrounds(saveB, (ss->sgx)->highlight_color);
      gtk_box_pack_start(GTK_BOX(saverow), saveB, FALSE, FALSE, 0);
      gtk_signal_connect(GTK_OBJECT(saveB), "clicked", GTK_SIGNAL_FUNC(save_button_pressed), (gpointer)ss);
      gtk_widget_show(saveB);

      printB = gtk_button_new_with_label(" print  ");
      set_backgrounds(printB, (ss->sgx)->highlight_color);
      gtk_box_pack_start(GTK_BOX(saverow), printB, FALSE, FALSE, 0);
      gtk_signal_connect(GTK_OBJECT(printB), "clicked", GTK_SIGNAL_FUNC(print_button_pressed), (gpointer)ss);
      gtk_widget_show(printB);

      revrow = gtk_hbox_new(FALSE, 0);
      set_background(revrow, (ss->sgx)->highlight_color);
      gtk_box_pack_start(GTK_BOX(leftbox), revrow, FALSE, FALSE, 0);
      gtk_widget_show(revrow);

      revertB = gtk_button_new_with_label("revert ");
      set_backgrounds(revertB, (ss->sgx)->highlight_color);
      gtk_box_pack_start(GTK_BOX(revrow), revertB, FALSE, FALSE, 0);
      gtk_signal_connect(GTK_OBJECT(revertB), "clicked", GTK_SIGNAL_FUNC(revert_button_pressed), (gpointer)ss);
      gtk_widget_show(revertB);

      deleteB = gtk_button_new_with_label("delete");
      set_backgrounds(deleteB, (ss->sgx)->highlight_color);
      gtk_box_pack_start(GTK_BOX(revrow), deleteB, FALSE, FALSE, 0);
      gtk_signal_connect(GTK_OBJECT(deleteB), "clicked", GTK_SIGNAL_FUNC(delete_button_pressed), (gpointer)ss);
      gtk_widget_show(deleteB);

      unrow = gtk_hbox_new(FALSE, 0);
      set_background(unrow, (ss->sgx)->highlight_color);
      gtk_box_pack_start(GTK_BOX(leftbox), unrow, FALSE, FALSE, 0);
      gtk_widget_show(unrow);

      undoB = gtk_button_new_with_label(" undo ");
      set_backgrounds(undoB, (ss->sgx)->highlight_color);
      gtk_box_pack_start(GTK_BOX(unrow), undoB, FALSE, FALSE, 0);
      gtk_signal_connect(GTK_OBJECT(undoB), "clicked", GTK_SIGNAL_FUNC(undo_button_pressed), (gpointer)ss);
      gtk_widget_show(undoB);

      redoB = gtk_button_new_with_label(" redo ");
      set_backgrounds(redoB, (ss->sgx)->highlight_color);
      gtk_box_pack_start(GTK_BOX(unrow), redoB, FALSE, FALSE, 0);
      gtk_signal_connect(GTK_OBJECT(redoB), "clicked", GTK_SIGNAL_FUNC(redo_button_pressed), (gpointer)ss);
      gtk_widget_show(redoB);

      rbrow = gtk_hbox_new(FALSE, 0);
      set_background(rbrow, (ss->sgx)->highlight_color);
      gtk_box_pack_start(GTK_BOX(leftbox), rbrow, FALSE, FALSE, 0);
      gtk_widget_show(rbrow);

      ampB = gtk_button_new_with_label(STR_amp);
      set_backgrounds(ampB, (ss->sgx)->highlight_color);
      gtk_box_pack_start(GTK_BOX(rbrow), ampB, TRUE, TRUE, 0);
      gtk_signal_connect(GTK_OBJECT(ampB), "clicked", GTK_SIGNAL_FUNC(amp_button_pressed), (gpointer)ss);
      gtk_widget_show(ampB);

      fltB = gtk_button_new_with_label(STR_flt);
      set_backgrounds(fltB, (ss->sgx)->highlight_color);
      gtk_box_pack_start(GTK_BOX(rbrow), fltB, TRUE, TRUE, 0);
      gtk_signal_connect(GTK_OBJECT(fltB), "clicked", GTK_SIGNAL_FUNC(flt_button_pressed), (gpointer)ss);
      gtk_widget_show(fltB);

      srcB = gtk_button_new_with_label(STR_src);
      set_backgrounds(srcB, (ss->sgx)->highlight_color);
      gtk_box_pack_start(GTK_BOX(rbrow), srcB, TRUE, TRUE, 0);
      gtk_signal_connect(GTK_OBJECT(srcB), "clicked", GTK_SIGNAL_FUNC(src_button_pressed), (gpointer)ss);
      gtk_widget_show(srcB);

      lerow = gtk_hbox_new(FALSE, 0);
      set_backgrounds(lerow, (ss->sgx)->highlight_color);
      gtk_box_pack_start(GTK_BOX(leftbox), lerow, FALSE, FALSE, 0);
      gtk_widget_show(lerow);

      linB = gtk_button_new_with_label(STR_linear);
      set_backgrounds(linB, (ss->sgx)->highlight_color);
      gtk_box_pack_start(GTK_BOX(lerow), linB, TRUE, TRUE, 0);
      gtk_signal_connect(GTK_OBJECT(linB), "clicked", GTK_SIGNAL_FUNC(lin_button_pressed), (gpointer)ss);
      gtk_widget_show(linB);

      expB = gtk_button_new_with_label(STR_exp);
      set_backgrounds(expB, (ss->sgx)->highlight_color);
      gtk_box_pack_start(GTK_BOX(lerow), expB, TRUE, TRUE, 0);
      gtk_signal_connect(GTK_OBJECT(expB), "clicked", GTK_SIGNAL_FUNC(exp_button_pressed), (gpointer)ss);
      gtk_widget_show(expB);

      selrow = gtk_hbox_new(FALSE, 0);
      set_background(selrow, (ss->sgx)->highlight_color);
      gtk_box_pack_start(GTK_BOX(leftbox), selrow, FALSE, FALSE, 0);
      gtk_widget_show(selrow);

      selectionB = gtk_button_new_with_label(STR_selection);
      set_backgrounds(selectionB, (ss->sgx)->highlight_color);
      gtk_box_pack_start(GTK_BOX(selrow), selectionB, TRUE, TRUE, 0);
      gtk_signal_connect(GTK_OBJECT(selectionB), "clicked", GTK_SIGNAL_FUNC(selection_button_pressed), (gpointer)ss);
      gtk_widget_show(selectionB);

      mixB = gtk_button_new_with_label("mix");
      set_backgrounds(mixB, (ss->sgx)->highlight_color);
      gtk_box_pack_start(GTK_BOX(selrow), mixB, TRUE, TRUE, 0);
      gtk_signal_connect(GTK_OBJECT(mixB), "clicked", GTK_SIGNAL_FUNC(mix_button_pressed), (gpointer)ss);
      gtk_widget_show(mixB);


      env_list_frame = gtk_frame_new(STR_envs_p);
      gtk_box_pack_start(GTK_BOX(leftbox), env_list_frame, TRUE, TRUE, 4);
      gtk_frame_set_label_align(GTK_FRAME(env_list_frame), 0.5, 0.0);
      gtk_frame_set_shadow_type(GTK_FRAME(env_list_frame), GTK_SHADOW_ETCHED_IN);

      env_list = gtk_clist_new(1); /* 1 => columns */
      gtk_clist_set_selection_mode(GTK_CLIST(env_list), GTK_SELECTION_SINGLE);
      gtk_clist_set_shadow_type(GTK_CLIST(env_list), GTK_SHADOW_ETCHED_IN);
      gtk_clist_column_titles_passive(GTK_CLIST(env_list));
      if (enved_all_envs_top() > 0) make_scrolled_env_list(ss);
      gtk_signal_connect(GTK_OBJECT(env_list), "select_row", GTK_SIGNAL_FUNC(env_browse_Callback), (gpointer)ss);

      env_list_scroller = gtk_scrolled_window_new(NULL, NULL);
      gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(env_list_scroller), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
      gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(env_list_scroller), env_list);
      gtk_container_add(GTK_CONTAINER(env_list_frame), env_list_scroller);

      gtk_widget_show(env_list_scroller);
      gtk_widget_show(env_list);
      gtk_widget_show(env_list_frame);

      toprow = gtk_hbox_new(FALSE, 0);
      gtk_box_pack_start(GTK_BOX(bottombox), toprow, FALSE, FALSE, 0);
      gtk_widget_show(toprow);

      bottomrow = gtk_hbox_new(FALSE, 0);
      gtk_box_pack_start(GTK_BOX(bottombox), bottomrow, FALSE, FALSE, 4);
      gtk_widget_show(bottomrow);

      nameL = gtk_label_new(STR_amp_env_p);
      gtk_box_pack_start(GTK_BOX(toprow), nameL, FALSE, FALSE, 0);
      gtk_widget_show(nameL);

      textL = snd_entry_new(ss, toprow, TRUE);
      gtk_signal_connect(GTK_OBJECT(textL), "activate", GTK_SIGNAL_FUNC(text_field_activated), (gpointer)ss);

      brkbox = gtk_button_new();
      gtk_box_pack_start(GTK_BOX(toprow), brkbox, FALSE, FALSE, 0);
      gtk_button_set_relief(GTK_BUTTON(brkbox), GTK_RELIEF_NONE);
      set_background(brkbox, (ss->sgx)->basic_color);
      gtk_widget_show(brkbox);
      
      brkpixL = get_blank_pixmap(ss);
      gtk_container_add(GTK_CONTAINER(brkbox), brkpixL);
      gtk_widget_show(brkpixL);
      set_blank_pixmap(brkpixL);
      
      brktxtL = gtk_label_new(NULL);
      set_background(brktxtL, (ss->sgx)->basic_color);
      gtk_box_pack_start(GTK_BOX(toprow), brktxtL, FALSE, FALSE, 0);
      gtk_widget_show(brktxtL);

      clipB = gtk_check_button_new_with_label(STR_clip);
      gtk_signal_connect(GTK_OBJECT(clipB), "toggled", GTK_SIGNAL_FUNC(Clip_Button_Callback), (gpointer)ss);
      gtk_box_pack_start(GTK_BOX(toprow), clipB, FALSE, FALSE, 0);
      gtk_widget_show(clipB);

      graphB = gtk_check_button_new_with_label(STR_wave);
      gtk_signal_connect(GTK_OBJECT(graphB), "toggled", GTK_SIGNAL_FUNC(Graph_Button_Callback), (gpointer)ss);
      gtk_box_pack_start(GTK_BOX(toprow), graphB, FALSE, FALSE, 0);
      gtk_widget_show(graphB);

      dBB = gtk_check_button_new_with_label(STR_dB);
      gtk_signal_connect(GTK_OBJECT(dBB), "toggled", GTK_SIGNAL_FUNC(dB_Button_Callback), (gpointer)ss);
      gtk_box_pack_start(GTK_BOX(toprow), dBB, FALSE, FALSE, 0);
      gtk_widget_show(dBB);

      baseLabel = gtk_label_new(STR_exp_base);
      gtk_box_pack_start(GTK_BOX(bottomrow), baseLabel, FALSE, FALSE, 2);
      gtk_widget_show(baseLabel);

      baseValue = gtk_label_new("1.000");
      gtk_box_pack_start(GTK_BOX(bottomrow), baseValue, FALSE, FALSE, 2);
      gtk_widget_show(baseValue);


      baseAdj = gtk_adjustment_new(0.5, 0.0, 1.0, 0.001, 0.01, .1);
      baseScale = gtk_hscrollbar_new(GTK_ADJUSTMENT(baseAdj));
      set_background(baseScale, (ss->sgx)->position_color);
      gtk_signal_connect(GTK_OBJECT(baseAdj), "value_changed", GTK_SIGNAL_FUNC(base_changed_callback), (gpointer)ss);
      gtk_box_pack_start(GTK_BOX(bottomrow), baseScale, TRUE, TRUE, 4);
      gtk_widget_show(baseScale);

      orderAdj = gtk_adjustment_new(20, 2, 100000, 2, 10, 0);
      orderL = gtk_spin_button_new(GTK_ADJUSTMENT(orderAdj), 0.0, 0);
      gtk_box_pack_end(GTK_BOX(bottomrow), orderL, FALSE, FALSE, 4);
      gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(orderL), TRUE);
      gtk_signal_connect(GTK_OBJECT(orderAdj), "value_changed", GTK_SIGNAL_FUNC(enved_filter_order_callback), (gpointer)ss);
      gtk_widget_show(orderL);

      gtk_widget_show(mainform);
      gtk_widget_show(enved_dialog);

      gtk_signal_connect(GTK_OBJECT(drawer), "expose_event", GTK_SIGNAL_FUNC(drawer_expose), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(drawer), "configure_event", GTK_SIGNAL_FUNC(drawer_resize), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(drawer), "button_press_event", GTK_SIGNAL_FUNC(drawer_button_press), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(drawer), "button_release_event", GTK_SIGNAL_FUNC(drawer_button_release), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(drawer), "motion_notify_event", GTK_SIGNAL_FUNC(drawer_button_motion), (gpointer)ss);

      if (enved_all_envs_top() == 0)
	{
	  set_sensitive(showB, FALSE);
	}
      set_sensitive(revertB, FALSE);
      set_sensitive(deleteB, FALSE);
      set_sensitive(undoB, FALSE);
      set_sensitive(redoB, FALSE);
      set_sensitive(saveB, FALSE);
      if (!(selection_is_active())) set_sensitive(selectionB, FALSE);

      set_toggle_button(clipB, enved_clip_p(ss), FALSE, (void *)ss);
      set_toggle_button(graphB, enved_wave_p(ss), FALSE, (void *)ss);
      set_toggle_button(dBB, enved_in_dB(ss), FALSE, (void *)ss);

      reflect_apply_state(ss);
      reflect_segment_state(ss);
    }
  else raise_dialog(enved_dialog);
  active_channel = current_channel(ss);
  set_sensitive(mixB, (mixes() > 0));
  return(enved_dialog);
}

void set_enved_clip_p(snd_state *ss, int val) 
{
  in_set_enved_clip_p(ss, val); 
  if (enved_dialog) set_toggle_button(clipB, val, FALSE, (void *)ss);
}

void set_enved_exp_p(snd_state *ss, int val) 
{
  in_set_enved_exp_p(ss, val); 
  /* if (enved_dialog) set_toggle_button(expB, val, FALSE, (void *)ss);  */ /* not a toggle button */
  reflect_segment_state(ss);
}

void set_enved_target(snd_state *ss, int val) 
{
  in_set_enved_target(ss, val); 
  if (enved_dialog) reflect_apply_state(ss);
}

void set_enved_wave_p(snd_state *ss, int val) 
{
  in_set_enved_wave_p(ss, val); 
  if (enved_dialog) set_toggle_button(graphB, val, FALSE, (void *)ss);
}

void set_enved_in_dB(snd_state *ss, int val) 
{
  in_set_enved_in_dB(ss, val); 
  if (enved_dialog) set_toggle_button(dBB, val, FALSE, (void *)ss);
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
	in_set_enved_filter_order(ss, order+1);
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

void enved_reflect_selection(int on)
{
  snd_state *ss;
  if ((enved_dialog) && (within_selection_src == 0))
    {
      ss = get_global_state();
      set_sensitive(selectionB, on);
      if ((apply_to_selection) && (!on))
	{
	  apply_to_selection = 0;
	  set_background(selectionB, (ss->sgx)->highlight_color);
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
    set_sensitive(mixB, TRUE);
}

static env *find_named_env(SCM name)
{
  char *env_name;
  int i, len;
  env_name = TO_NEW_C_STRING(name);
  len = enved_all_envs_top();
  for (i = 0; i < len; i++)
    if (strcmp(env_name, enved_all_names(i)) == 0)
      {
	free(env_name);
	return(enved_all_envs(i));
      }
  free(env_name);
  ERROR(NO_SUCH_ENVELOPE, SCM_LIST1(name));
  return(NULL);
}

static SCM g_enved_active_env(void)
{
  #define H_enved_active_env "(" S_enved_active_env ") -> current envelope editor env"
  return(env2scm(active_env));
}

static SCM g_set_enved_active_env(SCM e)
{
  ASSERT_TYPE(LIST_P(e) || STRING_P(e), e, SCM_ARGn, "set-" S_enved_active_env, "a list or string");
  if (active_env) active_env = free_env(active_env);
  if (STRING_P(e))
    active_env = copy_env(find_named_env(e));
  else active_env = scm2env(e);
  env_redisplay(get_global_state());
  return(e);
}

static SCM g_enved_selected_env(void)
{
  #define H_enved_selected_env "(" S_enved_selected_env ") -> current envelope editor selected env"
  return(env2scm(selected_env));
}

static SCM g_set_enved_selected_env(SCM name)
{
  ASSERT_TYPE(STRING_P(name), name, SCM_ARGn, "set-" S_enved_selected_env, "a string");
  selected_env = find_named_env(name);
  return(name);
}

void g_init_gxenv(SCM local_doc)
{
  define_procedure_with_setter(S_enved_active_env, SCM_FNC g_enved_active_env, H_enved_active_env,
			       "set-" S_enved_active_env, SCM_FNC g_set_enved_active_env, local_doc, 0, 0, 1, 0);
  define_procedure_with_setter(S_enved_selected_env, SCM_FNC g_enved_selected_env, H_enved_selected_env,
			       "set-" S_enved_selected_env, SCM_FNC g_set_enved_selected_env, local_doc, 0, 0, 1, 0);

}

