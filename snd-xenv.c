#include "snd.h"

/* TODO: if font is 6x10 or 5x8, formd goes berserk */

/* envelope editor and viewer */

static Widget enved_dialog = NULL;
static Widget applyB, apply2B, cancelB, drawer, showB, saveB, revertB, undoB, redoB;
static Widget printB, brkptL, graphB, fltB, ampB, srcB, clipB;
static Widget nameL, textL, screnvlst, dBB, orderL, deleteB, resetB, firB = NULL;
static Widget expB, linB, baseScale, baseValue, selectionB, mixB;
static GC gc, rgc, ggc;

static char *env_names[3] = {N_("amp env:"), N_("flt env:"), N_("src env:")};

static bool showing_all_envs = false; /* edit one env (0), or view all currently defined envs (1) */
static bool apply_to_selection = false;
static bool apply_to_mix = false;

static int env_window_width = 0;
static int env_window_height = 0;

static chan_info *active_channel = NULL, *last_active_channel = NULL;

static env* selected_env = NULL; /* if during view, one env is clicked, it is "selected" and can be pasted elsewhere */
static env* active_env = NULL;   /* env currently being edited */
static Float active_env_base = 1.0;

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

static void display_env(env *e, char *name, GC cur_gc, 
			int x0, int y0, int width, int height, bool dots, Float base, bool printing)
{
  axis_context *ax = NULL;  
  ax = (axis_context *)CALLOC(1, sizeof(axis_context));
  ax->wn = XtWindow(drawer);
  ax->dp = XtDisplay(drawer);
  ax->gc = cur_gc;
  display_enved_env(e, ax, name, x0, y0, width, height, dots, base, printing);
  ax = free_axis_context(ax);
}

void display_enved_env_with_selection(env *e, char *name, 
				      int x0, int y0, int width, int height, bool dots, Float base, bool printing)
{
  display_env(e, name, (selected_env == e) ? rgc : gc, x0, y0, width, height, dots, base, printing);
}

static void do_env_edit(env *new_env, bool loading)
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

void make_scrolled_env_list (void)
{
  XmString *strs;
  int n, size;
  size = enved_all_envs_top();
  if (!(ss->using_schemes)) 
    XtVaSetValues(screnvlst, XmNbackground, (ss->sgx)->highlight_color, NULL); 
  strs = (XmString *)CALLOC(size, sizeof(XmString)); 
  for (n = 0; n < size; n++) 
    strs[n] = XmStringCreate(enved_all_names(n), XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(screnvlst, 
		XmNitems, strs, 
		XmNitemCount, size, 
		NULL);
  for (n = 0; n < size; n++) 
    XmStringFree(strs[n]);
  FREE(strs);
}

void alert_enved_amp_env(snd_info *sp)
{
  if ((enved_dialog) && (active_channel) && (enved_wave_p(ss)))
    if (active_channel->sound == sp) 
      env_redisplay();
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

static void dismiss_enved_callback(Widget w, XtPointer context, XtPointer info) 
{
  if (ss->checking_explicitly)
    ss->stopped_explicitly = true;
  else XtUnmanageChild(enved_dialog);
}

static void help_enved_callback(Widget w, XtPointer context, XtPointer info) 
{
  envelope_editor_dialog_help();
}

static bool within_selection_src = false;

static void apply_enved(void)
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
      else active_channel = current_channel();
      if (active_channel)
	{
	  set_sensitive(applyB, false);
	  set_sensitive(apply2B, false);
	  set_button_label(cancelB, _("Stop"));
	  check_for_event();
	  switch (enved_target(ss))
	    {
	    case ENVED_AMPLITUDE:
	      if (apply_to_mix)
		{
		  set_mix_amp_env_from_gui(mix_id, NO_SELECTION, active_env); /* chan = NO_SELECTION: use selected chan if more than 1 */
		  active_channel = current_channel();
		}
	      else apply_env(active_channel, active_env, 0,
			     CURRENT_SAMPLES(active_channel), 
			     apply_to_selection, FROM_ENVED, 
			     "Enved: amp", NULL,
			     C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0, (enved_exp_p(ss)) ? active_env_base : 1.0);
	      /* calls update_graph, I think, but in short files that doesn't update the amp-env */
	      break;
	    case ENVED_SPECTRUM: 
	      apply_filter(active_channel,
			   (FIR_p) ? enved_filter_order(ss) : 0,
			   active_env, FROM_ENVED, 
			   "Enved: flt", apply_to_selection, 
			   NULL, NULL,
			   C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0);
	      break;
	    case ENVED_SRATE:
	      /* mus_src no longer protects against 0 srate */
	      max_env = copy_env(active_env);
	      for (i = 0, j = 1; i < max_env->pts; i++, j += 2)
		if (max_env->data[j] < .01) 
		  max_env->data[j] = .01;
	      within_selection_src = true;
	      src_env_or_num(active_channel, max_env, 0.0, 
			     false, FROM_ENVED, "Enved: src", 
			     apply_to_selection, NULL,
			     C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0, (enved_exp_p(ss)) ? active_env_base : 1.0);
	      within_selection_src = false;
	      max_env = free_env(max_env);
	      break;
	    }
	  if (enved_wave_p(ss)) env_redisplay();
	  set_sensitive(applyB, true);
	  if (!apply_to_mix) set_sensitive(apply2B, true);
	  set_button_label(cancelB, _("Dismiss"));
	}
    }
}

static void env_redisplay_1(bool printing)
{
  char *name = NULL;
  if (enved_dialog_is_active())
    {
      XClearWindow(XtDisplay(drawer), XtWindow(drawer));
      if (showing_all_envs) 
	view_envs(env_window_width, env_window_height, printing);
      else 
	{
	  name = XmTextGetString(textL);
	  if (!name) name = copy_string(_("noname"));
	  display_env(active_env, name, gc, 0, 0, 
		      env_window_width, env_window_height, true, 
		      (enved_exp_p(ss)) ? active_env_base : 1.0,
		      printing);
	  if (name) XtFree(name);
	  if ((enved_wave_p(ss)) && (!apply_to_mix))
	    {
	      if ((enved_target(ss) == ENVED_SPECTRUM) && (active_env) && (FIR_p) && (!printing))
		display_frequency_response(active_env, axis, gray_ap->ax, enved_filter_order(ss), enved_in_dB(ss));
	      enved_show_background_waveform(axis, gray_ap, apply_to_selection, (enved_target(ss) == ENVED_SPECTRUM), printing);
	    }
	}
    }
}

void env_redisplay(void) {env_redisplay_1(false);}
void env_redisplay_with_print(void) {env_redisplay_1(true);}

void enved_fft_update(void)
{
  if ((enved_dialog_is_active()) &&
      (!(showing_all_envs)))
    {
      if ((enved_wave_p(ss)) &&
	  (enved_target(ss) == ENVED_SPECTRUM) && 
	  (active_env))
	enved_show_background_waveform(axis, gray_ap, apply_to_selection, true, false);
    }
}

static void enved_reset(void)
{
  set_enved_clip_p(DEFAULT_ENVED_CLIP_P);
  set_enved_exp_p(DEFAULT_ENVED_EXP_P);
  set_enved_power(DEFAULT_ENVED_POWER);
  set_enved_base(DEFAULT_ENVED_BASE);
  set_enved_target(DEFAULT_ENVED_TARGET);
  set_enved_wave_p(DEFAULT_ENVED_WAVE_P);
  set_enved_in_dB(DEFAULT_ENVED_IN_DB);
  XmTextSetString(textL, NULL);
  set_enved_filter_order(DEFAULT_ENVED_FILTER_ORDER);
  if (active_env) active_env = free_env(active_env);
#if HAVE_GUILE
  active_env = string2env("'(0 0 1 0)");
#else
  active_env = string2env("[0, 0, 1, 0]");
#endif
  set_enved_env_list_top(0);
  do_env_edit(active_env, true);
  set_sensitive(saveB, true);
  env_redisplay();
}

static void order_field_activated(void)
{
  /* return in order text field */
  char *str = NULL;
  int order;
  str = XmTextGetString(orderL);
  if ((str) && (*str))
    {
      order = string2int(str);
      if (order & 1) order++;
      if ((order > 0) && 
	  (order < 2000)) 
	set_enved_filter_order(order);
    }
  if (str) XtFree(str);
}

static void text_field_activated(void)
{ /* might be breakpoints to load or an envelope name (<cr> in enved text field) */
  char *name = NULL, *str;
  env *e = NULL;
  name = XmTextGetString(textL);
  if ((name) && (*name))
    {
      str = name;
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
	  else e = string2env(str);
	}
      if (e) 
	{
	  if (active_env) active_env = free_env(active_env);
	  active_env = copy_env(e);
	  set_enved_env_list_top(0);
	  do_env_edit(active_env, true);
	  set_sensitive(saveB, true);
	  env_redisplay();
	  e = free_env(e);
	}
    }
  if (name) XtFree(name);
}

static void save_button_pressed(Widget w, XtPointer context, XtPointer info) 
{
  char *name = NULL;
  name = XmTextGetString(textL);
  if ((!name) || (!(*name))) 
    name = copy_string(_("unnamed"));
  alert_envelope_editor(name, copy_env(active_env));
  add_or_edit_symbol(name, active_env);
  set_sensitive(saveB, false);
  env_redisplay();
  if (name) XtFree(name);
}

static void apply_enved_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* apply current envs to currently sync'd channels */
  state_context *sgx;
  XmAnyCallbackStruct *cb = (XmAnyCallbackStruct *)info;
  sgx = ss->sgx;
  /* since this is the OK button from Motif's point of view, text activation (<cr> in the text field)
   * causes a bogus activation of this callback; we trap that by looking for text_activate_event.
   * cr in text => save under that name
   */
  if (cb->event != sgx->text_activate_event)
    {
      apply_enved();
      last_active_channel = active_channel;
    }
  else 
    {
      if (sgx->text_widget == textL)
	text_field_activated();
      else order_field_activated();
    }
}

static void undo_and_apply_enved_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* undo previous amp env, then apply */
  /* this blindly undoes the previous edit (assumed to be an envelope) -- if the user made some other change in the meantime, too bad */
  if ((active_channel) && (active_channel == last_active_channel))
    {
      active_channel->squelch_update = true;
      undo_edit_with_sync(active_channel, 1);
      active_channel->squelch_update = false;
    }
  apply_enved();
  last_active_channel = active_channel;
}

static void reflect_segment_state (void)
{
  if (enved_dialog)
    {
      if (!(ss->using_schemes))
	{
	  XmChangeColor(expB, 
			(enved_exp_p(ss)) ? ((Pixel)(ss->sgx)->yellow) : 
			                     ((Pixel)(ss->sgx)->highlight_color));
	  XmChangeColor(linB, 
			(enved_exp_p(ss)) ? ((Pixel)(ss->sgx)->highlight_color) : 
                                            ((Pixel)(ss->sgx)->yellow));
	}
      if ((active_env) && 
	  (!(showing_all_envs))) 
	env_redisplay();
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
  selected_env = enved_all_envs(pos);
  active_env = copy_env(selected_env);
  XmTextSetString(textL, enved_all_names(pos));
  set_enved_env_list_top(0);
  do_env_edit(active_env, true);
  set_sensitive(undoB, false);
  set_sensitive(revertB, false);
  set_sensitive(saveB, false);
  set_enved_exp_p((active_env_base != 1.0));
  set_enved_base(active_env_base);
  env_redisplay();
  set_sensitive(deleteB, true);
}

static void clear_point_label(void)
{
  XtVaSetValues(brkptL, XmNlabelType, XmSTRING, XmNlabelString, NULL, NULL);
}

void enved_display_point_label(Float x, Float y)
{
  char brkpt_buf[LABEL_BUFFER_SIZE];
  if ((enved_in_dB(ss)) && (ss->min_dB < -60))
    mus_snprintf(brkpt_buf, LABEL_BUFFER_SIZE, "%.3f : %.5f", x, y);
  else mus_snprintf(brkpt_buf, LABEL_BUFFER_SIZE, "%.3f : %.3f", x, y);
  set_button_label(brkptL, brkpt_buf);
}

void display_enved_progress(char *str, Pixmap pix)
{
  if (pix == 0)
    set_button_label(brkptL, str);
  else XtVaSetValues(brkptL, 
		     XmNlabelType, XmPIXMAP, 
		     XmNlabelPixmap, pix, 
		     NULL);
}

static Tempus down_time;
static bool env_dragged = false;
static int env_pos = 0;
static bool click_to_delete = false;

#ifdef MAC_OSX
static int press_x, press_y;
#endif

static void drawer_button_motion(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  XMotionEvent *ev = (XMotionEvent *)event;
  Tempus motion_time;
  axis_info *ap;
  Float x0, x1, x, y;
  if (!showing_all_envs)
    {
      motion_time = ev->time;
#ifdef MAC_OSX
      if ((motion_time - down_time) < XtGetMultiClickTime(XtDisplay(w))) return;
      if ((ev->x == press_x) && (ev->y == press_y)) return;
#else
      if ((motion_time - down_time) < (0.5 * XtGetMultiClickTime(XtDisplay(w)))) return;
#endif
      env_dragged = true;
      click_to_delete = false;
      ap = axis;
      x = ungrf_x(ap, ev->x);
      if (env_pos > 0) 
	x0 = active_env->data[env_pos * 2 - 2]; 
      else x0 = 0.0;
      if (env_pos < active_env->pts) 
	x1 = active_env->data[env_pos * 2 + 2]; 
      else x1 = 1.0;
      if (x < x0) x = x0;
      if (x > x1) x = x1;
      if (env_pos == 0) x = active_env->data[0];
      if (env_pos == (active_env->pts - 1)) 
	x = active_env->data[(active_env->pts - 1) * 2];
      y = ungrf_y(ap, ev->y);
      if ((enved_clip_p(ss)) || 
	  (enved_in_dB(ss)))
	{
	  if (y < ap->y0) y = ap->y0;
	  if (y > ap->y1) y = ap->y1;
	}
      if (enved_in_dB(ss)) y = un_dB(y);
      if (check_enved_hook(active_env, env_pos, x, y, ENVED_MOVE_POINT) == 0)
	move_point(active_env, env_pos, x, y);
      enved_display_point_label(x, y);
      env_redisplay();
    }
}

static void drawer_button_press(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  XButtonEvent *ev = (XButtonEvent *)event;
  int pos;
#ifdef MAC_OSX
  press_x = ev->x;
  press_y = ev->y;
#endif
  down_time = ev->time;
  env_dragged = false;
  if (showing_all_envs)
    {
      pos = hit_env(ev->x, ev->y, env_window_width, env_window_height);
      XmListSelectPos(screnvlst, pos + 1, false);
      if ((pos >= 0) && 
	  (pos < enved_all_envs_top())) 
	select_or_edit_env(pos);
    }
  else
    {
      if (!active_env)
	{
	  active_env = default_env(1.0, 0.0);
	  active_env_base = 1.0;
	  env_redisplay(); /* needed to get current_xs set up correctly */
	}
      env_pos = enved_button_press_display(axis, active_env, ev->x, ev->y);
    }
}

void set_enved_click_to_delete(bool n) {click_to_delete = n;}

static void drawer_button_release(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
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
      env_redisplay();
      clear_point_label();
    }
}

static void drawer_resize(Widget w, XtPointer context, XtPointer info) 
{
  /* update display, can be either view of all envs or sequence of current envs */
  env_window_width = widget_width(w);
  env_window_height = widget_height(w);
  env_redisplay();
}

static void show_button_pressed(Widget w, XtPointer context, XtPointer info) 
{
  /* if show all (as opposed to show current), loop through loaded LV_LISTs */
  showing_all_envs = (!showing_all_envs);
  set_button_label(showB, (showing_all_envs) ? _("edit env") : _("view envs"));
  env_redisplay();
}

static void selection_button_pressed(Widget s, XtPointer context, XtPointer info) 
{
  apply_to_selection = (!apply_to_selection);
  if (apply_to_selection) 
    {
      if (apply_to_mix) 
	{
	  if (!(ss->using_schemes))
	    XmChangeColor(mixB, ((Pixel)(ss->sgx)->highlight_color));
	}
      apply_to_mix = false;
    }
  if (!(ss->using_schemes))
    XmChangeColor(selectionB, 
		  (apply_to_selection) ? ((Pixel)(ss->sgx)->yellow) : ((Pixel)(ss->sgx)->highlight_color));
  set_sensitive(apply2B, (!apply_to_mix));
  if ((enved_target(ss) != ENVED_SPECTRUM) && 
      (enved_wave_p(ss)) && 
      (!showing_all_envs)) 
    env_redisplay();
}

static void mix_button_pressed(Widget w, XtPointer context, XtPointer info) 
{
  int chan = 0;
  int mxchan, mix_id = INVALID_MIX_ID;
  apply_to_mix = (!apply_to_mix);
  if (apply_to_mix) 
    {
      if (apply_to_selection) 
	{
	  if (!(ss->using_schemes))
	    XmChangeColor(selectionB, ((Pixel)(ss->sgx)->highlight_color));
	}
      apply_to_selection = false;
      if (ss->selected_mix != INVALID_MIX_ID) 
	mix_id = ss->selected_mix; 
      else
	{
	  mix_id = any_mix_id();
	  if (mix_id != INVALID_MIX_ID) 
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
	      env_redisplay();
	    }
	}
    }
  if (!(ss->using_schemes))
    XmChangeColor(mixB, 
		  (apply_to_mix) ? ((Pixel)(ss->sgx)->yellow) : ((Pixel)(ss->sgx)->highlight_color));
  set_sensitive(apply2B, (!apply_to_mix));
  if ((enved_target(ss) == ENVED_AMPLITUDE) && 
      (enved_wave_p(ss)) && 
      (!showing_all_envs)) 
    env_redisplay();
}

static void delete_button_pressed(Widget w, XtPointer context, XtPointer info) 
{
  int i, len;
  if (selected_env)
    {
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

static void revert_button_pressed(Widget w, XtPointer context, XtPointer info) 
{
  revert_env_edit();
  if (active_env) active_env = free_env(active_env);
  active_env = enved_next_env();
  env_redisplay();
}

static void undo_button_pressed(Widget w, XtPointer context, XtPointer info) 
{
  undo_env_edit();
  if (active_env) active_env = free_env(active_env);
  active_env = enved_next_env();
  env_redisplay();
}

static void redo_button_pressed(Widget w, XtPointer context, XtPointer info) 
{
  redo_env_edit();
  if (active_env) active_env = free_env(active_env);
  active_env = enved_next_env();
  env_redisplay();
}

static void reflect_apply_state (void)
{
  set_label(nameL, _(env_names[enved_target(ss)]));
  if (!(ss->using_schemes))
    {
      XmChangeColor(ampB, 
		    (enved_target(ss) == ENVED_AMPLITUDE) ? ((Pixel)(ss->sgx)->green) : ((Pixel)(ss->sgx)->highlight_color));
      XmChangeColor(fltB, 
		    (enved_target(ss) == ENVED_SPECTRUM) ? ((Pixel)(ss->sgx)->green) : ((Pixel)(ss->sgx)->highlight_color));
      XmChangeColor(srcB, 
		    (enved_target(ss) == ENVED_SRATE) ? ((Pixel)(ss->sgx)->green) : ((Pixel)(ss->sgx)->highlight_color));
    }
  if ((!showing_all_envs) && 
      (enved_wave_p(ss))) 
    env_redisplay();
}

static void freq_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  in_set_enved_target(ENVED_SPECTRUM);
  old_clip_p = enved_clip_p(ss);
  set_enved_clip_p(true);
  reflect_apply_state();
}

static void amp_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  if (enved_target(ss) == ENVED_SPECTRUM)
    set_enved_clip_p(old_clip_p);
  in_set_enved_target(ENVED_AMPLITUDE);
  reflect_apply_state();
}

static void src_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  if (enved_target(ss) == ENVED_SPECTRUM)
    set_enved_clip_p(old_clip_p);
  in_set_enved_target(ENVED_SRATE);
  reflect_apply_state();
}

static void reset_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  enved_reset();
}

void enved_print(char *name)
{
  print_enved(name, env_window_height);
}

static void print_button_pressed(Widget w, XtPointer context, XtPointer info) 
{
  ss->print_choice = PRINT_ENV;
  file_print_callback(w, context, info); /* eventually calls enved_print -> print_enved -> env_redisplay_with_print */
}

static void env_browse_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmListCallbackStruct *cb = (XmListCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsList(w), w);
  select_or_edit_env(cb->item_position - 1);
}

static void graph_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  in_set_enved_wave_p(cb->set);
  env_redisplay();
}

static void dB_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  in_set_enved_in_dB(cb->set);
  env_redisplay();
}

static void clip_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info; 
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  in_set_enved_clip_p(cb->set);
}

static void exp_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* a push button */
  in_set_enved_exp_p(true); 
  if ((active_env) && 
      (!(showing_all_envs)))
    {
      if (enved_exp_p(ss))
	active_env_base = enved_base(ss);
      else active_env_base = 1.0;
      set_sensitive(saveB, true);
    }
  reflect_segment_state();
}

static void lin_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* a push button */
  in_set_enved_exp_p(false);
  if ((active_env) && 
      (!(showing_all_envs)))
    {
      if (enved_exp_p(ss))
	active_env_base = enved_base(ss);
      else active_env_base = 1.0;
      set_sensitive(saveB, true);
    }
  reflect_segment_state();
}

#define BASE_MAX 400
#define BASE_MID 200
/* these two just set the granularity of the scale widget, not the user-visible bounds */

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
  for (i = 0; i < scale_len - 1; i++) 
    buf[i] = sfs[i];
  set_button_label(baseValue, buf);
  FREE(sfs);
  FREE(buf);
  in_set_enved_base(bval);
  if ((active_env) && 
      (!(showing_all_envs))) 
    {
      active_env_base = enved_base(ss);
      if (enved_exp_p(ss)) 
	env_redisplay();
    }
}

static void base_changed(int val)
{
  Float bval;
  if (val == 0) 
    bval = 0.0;
  else 
    {
      if (val == BASE_MID)
	bval = 1.0;
      else
	{
	  if (val > BASE_MID)
	    bval = pow(1.0 + (10.0 * ((Float)(val - BASE_MID) / (Float)BASE_MID)), enved_power(ss));  
	  else 
	    bval = pow(((Float)val / (Float)BASE_MID), enved_power(ss) - 1.0);
	}
    }
  make_base_label(bval);
  if ((active_env) && (enved_exp_p(ss))) 
    set_sensitive(saveB, true); /* what about undo/redo here? */
}

static void reflect_changed_base(Float val)
{
  int ival;
  if (val <= 0.0) 
    ival = 0;
  else
    {
      if (val == 1.0)
	ival = BASE_MID;
      else
	{
	  if (val <= 1.0)
	    ival = (int)(pow(val, 1.0 / (enved_power(ss) - 1.0)) * BASE_MID);
	  else ival = (int)(BASE_MID + ((BASE_MID * (pow(val, (1.0 / (enved_power(ss)))) - 1)) / 10.0));
	}
    }
  XtVaSetValues(baseScale, XmNvalue, mus_iclamp(0, ival, (int)(BASE_MAX * .9)), NULL);
  make_base_label(val);
}

static void base_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *sb = (XmScrollBarCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  base_changed(sb->value);
}

static int base_last_value = BASE_MID;

static void base_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *sb = (XmScrollBarCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsScrollBar(w), w);
  base_last_value = sb->value;
  base_changed(sb->value);
}

static void base_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  XButtonEvent *ev;
  int val;
  ASSERT_WIDGET_TYPE(XmIsPushButton(w), w);
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    val = base_last_value; 
  else val = BASE_MID;
  base_changed(val);
  XtVaSetValues(baseScale, XmNvalue, val, NULL);
}

static void FIR_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  ASSERT_WIDGET_TYPE(XmIsPushButton(w), w);
  FIR_p = (!FIR_p);
  set_label(w, (FIR_p) ? "fir" : "fft");
}

Widget create_envelope_editor(void)
{
  int n;
  Arg args[32];
  Widget colE, colD, colB, colF;
  Widget spacer, spacer1, aform, mainform, screnvname, baseSep, baseLabel;
  XmString xhelp, xdismiss, xapply, titlestr, s1;
  XGCValues gv;
  XtCallbackList n1, n2;
  char str[LABEL_BUFFER_SIZE];

  if (!enved_dialog)
    {

      /* -------- DIALOG -------- */
      xdismiss = XmStringCreate(_("Dismiss"), XmFONTLIST_DEFAULT_TAG);
      xhelp = XmStringCreate(_("Help"), XmFONTLIST_DEFAULT_TAG);
      titlestr = XmStringCreate(_("Edit Envelope"), XmFONTLIST_DEFAULT_TAG);
      xapply = XmStringCreate(_("Apply"), XmFONTLIST_DEFAULT_TAG);
      /* xreset = XmStringCreate(_("Reset"), XmFONTLIST_DEFAULT_TAG); */

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNcancelLabelString, xdismiss); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNokLabelString, xapply); n++;
      XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNtransient, false); n++;
      enved_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), _("envelope editor"), args, n);
  
      XtAddCallback(enved_dialog, XmNcancelCallback, dismiss_enved_callback, NULL);
      XtAddCallback(enved_dialog, XmNhelpCallback, help_enved_callback, NULL);
      XtAddCallback(enved_dialog, XmNokCallback, apply_enved_callback, NULL);

      XmStringFree(xhelp);
      XmStringFree(xdismiss);
      XmStringFree(titlestr);
      XmStringFree(xapply);

      if (!(ss->using_schemes))
	{
	  XtVaSetValues(XmMessageBoxGetChild(enved_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(enved_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(enved_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	}

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;
	  XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;
	}
      apply2B = XtCreateManagedWidget(_("Undo&Apply"), xmPushButtonGadgetClass, enved_dialog, args, n);
      XtAddCallback(apply2B, XmNactivateCallback, undo_and_apply_enved_callback, NULL);

      resetB = XtCreateManagedWidget(_("Reset"), xmPushButtonGadgetClass, enved_dialog, args, n);
      XtAddCallback(resetB, XmNactivateCallback, reset_button_callback, NULL);


      /* -------- MAIN WIDGET -------- */
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, XmMessageBoxGetChild(enved_dialog, XmDIALOG_SEPARATOR)); n++;
      mainform = XtCreateManagedWidget("formd", xmFormWidgetClass, enved_dialog, args, n);

      /* the order in which widgets are defined matters a lot here:
       * we need to build from the bottom up so that the graph portion expands
       * when the window is resized (if top-down, the slider at the bottom expands!)
       */

      /* -------- EXP SLIDER AT BOTTOM -------- */
      n = 0;      
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      baseLabel = make_pushbutton_widget (_("exp:"), mainform, args, n);
      XtAddCallback(baseLabel, XmNactivateCallback, base_click_callback, NULL);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      s1 = XmStringCreate("1.000", XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, baseLabel); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, baseLabel); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      /*      XtSetArg(args[n], XmNrecomputeSize, false); n++; */
      XtSetArg(args[n], XmNlabelString, s1); n++;
      baseValue = XtCreateManagedWidget ("base-label", xmLabelWidgetClass, mainform, args, n);
      XmStringFree(s1);

      /* -------- filter order -------- */
      n = 0;      
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNcolumns, 3); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
#ifndef SGI
      XtSetArg(args[n], XmNheight, 24); n++;
#endif
      XtSetArg(args[n], XmNresizeWidth, false); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNmarginHeight, 0); n++;
      XtSetArg(args[n], XmNmarginBottom, 0); n++;
      mus_snprintf(str, LABEL_BUFFER_SIZE, "%d", enved_filter_order(ss));
      XtSetArg(args[n], XmNvalue, str); n++;
      orderL = make_textfield_widget("orderL", mainform, args, n, ACTIVATABLE, NO_COMPLETER);

      /* -------- fft/fir choice -------- */
      n = 0;      
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, orderL); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      firB = make_pushbutton_widget((char *)((FIR_p) ? "fir" : "fft"), mainform, args, n);
      XtAddCallback(firB, XmNactivateCallback, FIR_click_callback, NULL);

      /* -------- exp base scale -------- */
      n = 0;      
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->position_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, baseLabel); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, baseValue); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, firB); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNmaximum, BASE_MAX); n++;
      XtSetArg(args[n], XmNvalue, BASE_MID); n++;
      XtSetArg(args[n], XmNincrement, 1); n++;
      XtSetArg(args[n], XmNpageIncrement, 1); n++;
      XtSetArg(args[n], XmNdragCallback, n1 = make_callback_list(base_drag_callback, NULL)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n2 = make_callback_list(base_valuechanged_callback, NULL)); n++;
      baseScale = XtCreateManagedWidget("expscl", xmScrollBarWidgetClass, mainform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, baseScale); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNmargin, LINE_MARGIN); n++;
      XtSetArg(args[n], XmNheight, LINE_MARGIN); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      XtSetArg(args[n], XmNheight, 5); n++;
      baseSep = XtCreateManagedWidget("snd-rec-sep", xmSeparatorWidgetClass, mainform, args, n);

      /* -------- AMP ENV NAME -------- */
      n = 0;
      s1 = XmStringCreate(_("amp env:"), XmFONTLIST_DEFAULT_TAG);
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, baseSep); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      nameL = XtCreateManagedWidget("nameL", xmLabelWidgetClass, mainform, args, n);
      XmStringFree(s1);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, baseSep); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, nameL); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      textL = make_textfield_widget("textL", mainform, args, n, ACTIVATABLE, NO_COMPLETER);

      /* -------- dB, GRAPH ('wave') AND CLIP BUTTONS -------- */
      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;
	  XtSetArg(args[n], XmNselectColor, (ss->sgx)->pushed_button_color); n++;
	}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, baseSep); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
      dBB = make_togglebutton_widget(_("dB"), mainform, args, n);
      XtAddCallback(dBB, XmNvalueChangedCallback, dB_button_callback, NULL);

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;
	  XtSetArg(args[n], XmNselectColor, (ss->sgx)->pushed_button_color); n++;
	}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, baseSep); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, dBB); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
      graphB = make_togglebutton_widget(_("wave"), mainform, args, n);
      XtAddCallback(graphB, XmNvalueChangedCallback, graph_button_callback, NULL);

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;
	  XtSetArg(args[n], XmNselectColor, (ss->sgx)->pushed_button_color); n++;
	}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, baseSep); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, graphB); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
      clipB = make_togglebutton_widget(_("clip"), mainform, args, n);
      XtAddCallback(clipB, XmNvalueChangedCallback, clip_button_callback, NULL);

      /* -------- BREAKPOINT DATA DISPLAY LABEL -------- */
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, baseSep); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, textL); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, clipB); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNlabelType, XmSTRING); n++;
      brkptL = XtCreateManagedWidget("         ", xmLabelWidgetClass, mainform, args, n);

      /* -------- SPACERS TO DIVIDE WINDOW IN TWO -------- */
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, textL); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNheight, 4); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      spacer = XtCreateManagedWidget("spacer", xmSeparatorWidgetClass, mainform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, spacer); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNseparatorType, XmSHADOW_ETCHED_IN); n++;
      spacer1 = XtCreateManagedWidget("spacer1", xmSeparatorWidgetClass, mainform, args, n);
      /* second separator needed because marginTop seems to be broken or non-existent for these widgets */

      /* -------- WINDOW LEFT WIDGET HOLDER -------- */
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, spacer1); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      aform = XtCreateManagedWidget("aform", xmFormWidgetClass, mainform, args, n);

      /* -------- BUTTON BOX AT TOP LEFT -------- */
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->zoom_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNshadowThickness, 4); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      colF = XtCreateManagedWidget("env-button-frame", xmFrameWidgetClass, aform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->lighter_blue); n++;}
      colB = XtCreateManagedWidget("env-button-holder", xmFormWidgetClass, colF, args, n);

      /* VIEW ENVS */
      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;
	  XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;
	}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      showB = XtCreateManagedWidget(_("view envs"), xmPushButtonWidgetClass, colB, args, n);
      XtAddCallback(showB, XmNactivateCallback, show_button_pressed, NULL);

      /* SAVE PRINT */
      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;
	  XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;
	}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, showB); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 50); n++;
      saveB = XtCreateManagedWidget(_("save"), xmPushButtonWidgetClass, colB, args, n);

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;
	  XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;
	}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, saveB); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, saveB); n++;
      printB = XtCreateManagedWidget(_("print"), xmPushButtonWidgetClass, colB, args, n);

      XtAddCallback(saveB, XmNactivateCallback, save_button_pressed, NULL);
      XtAddCallback(printB, XmNactivateCallback, print_button_pressed, NULL);

      /* UNDO REDO */
      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;
	  XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;
	}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, saveB); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 50); n++;
      undoB = XtCreateManagedWidget(_("undo"), xmPushButtonWidgetClass, colB, args, n);

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;
	  XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;
	}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, undoB); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, undoB); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      redoB = XtCreateManagedWidget(_("redo"), xmPushButtonWidgetClass, colB, args, n);

      XtAddCallback(undoB, XmNactivateCallback, undo_button_pressed, NULL);
      XtAddCallback(redoB, XmNactivateCallback, redo_button_pressed, NULL);

      /* REVERT DELETE */
      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;
	  XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;
	}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, undoB); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 50); n++;
      revertB = XtCreateManagedWidget(_("revert"), xmPushButtonWidgetClass, colB, args, n);

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;
	  XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;
	}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, revertB); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, revertB); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      deleteB = XtCreateManagedWidget(_("delete"), xmPushButtonWidgetClass, colB, args, n);

      XtAddCallback(revertB, XmNactivateCallback, revert_button_pressed, NULL);
      XtAddCallback(deleteB, XmNactivateCallback, delete_button_pressed, NULL);

      /* AMP FLT SRC */
      /* enved_function (target) choice (a row of three push buttons that acts like a "radio box") */
      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;
	  XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;
	}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, revertB); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 33); n++;
      ampB = XtCreateManagedWidget(_("amp"), xmPushButtonWidgetClass, colB, args, n);

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;
	  XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;
	}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, ampB); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, ampB); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 67); n++;
      fltB = XtCreateManagedWidget(_("flt"), xmPushButtonWidgetClass, colB, args, n);

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;
	  XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;
	}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, fltB); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, fltB); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      srcB = XtCreateManagedWidget(_("src"), xmPushButtonWidgetClass, colB, args, n);

      XtAddCallback(fltB, XmNactivateCallback, freq_button_callback, NULL);
      XtAddCallback(ampB, XmNactivateCallback, amp_button_callback, NULL);
      XtAddCallback(srcB, XmNactivateCallback, src_button_callback, NULL);

      /* LINEAR EXP */
      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;
	  XtSetArg(args[n], XmNarmColor, (ss->sgx)->yellow); n++;
	}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, ampB); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 50); n++;
      linB = XtCreateManagedWidget(_("linear"), xmPushButtonWidgetClass, colB, args, n);

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;
	  XtSetArg(args[n], XmNarmColor, (ss->sgx)->yellow); n++;
	}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, linB); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, linB); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      expB = XtCreateManagedWidget(_("exp"), xmPushButtonWidgetClass, colB, args, n);

      XtAddCallback(linB, XmNactivateCallback, lin_button_callback, NULL);
      XtAddCallback(expB, XmNactivateCallback, exp_button_callback, NULL);


      /* SELECTION MIX */
      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;
	  XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;
	}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, linB); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 70); n++;
      selectionB = make_pushbutton_widget(_("selection"), colB, args, n);


      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;
	  XtSetArg(args[n], XmNarmColor, (ss->sgx)->yellow); n++;
	}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, selectionB); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, selectionB); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      mixB = make_pushbutton_widget(_("mix"), colB, args, n);

      XtAddCallback(selectionB, XmNactivateCallback, selection_button_pressed, NULL);
      XtAddCallback(mixB, XmNactivateCallback, mix_button_pressed, NULL);


      /* -------- ENV LIST AT LEFT UNDER BUTTONS -------- */
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, colF); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNshadowThickness, 4); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      colE = XtCreateManagedWidget("env-list-frame", xmFrameWidgetClass, aform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;}
      colD = XtCreateManagedWidget("env-list-holder", xmFormWidgetClass, colE, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      screnvname = XtCreateManagedWidget(_("envs:"), xmLabelWidgetClass, colD, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, screnvname); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      screnvlst = XmCreateScrolledList(colD, "scrolled-env-list", args, n);
      XtManageChild(screnvlst); 
      XtAddCallback(screnvlst, XmNbrowseSelectionCallback, env_browse_callback, NULL);
      if (!(ss->using_schemes)) map_over_children(screnvlst, set_main_color_of_widget, NULL);
      if (enved_all_envs_top() > 0) make_scrolled_env_list();

      /* -------- MAIN GRAPH -------- */

      n = 0;
      if (!(ss->using_schemes))	{XtSetArg(args[n], XmNbackground, (ss->sgx)->graph_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, spacer1 /* textL */); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, aform); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNheight, 350); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      drawer = XtCreateManagedWidget("drawer", xmDrawingAreaWidgetClass, mainform, args, n);

      gv.function = GXcopy;
      XtVaGetValues(drawer, XmNbackground, &gv.background, XmNforeground, &gv.foreground, NULL);
      gc = XtGetGC(drawer, GCForeground | GCFunction, &gv);
      gv.foreground = (ss->sgx)->red;
      rgc = XtGetGC(drawer, GCBackground | GCForeground | GCFunction, &gv);
      gv.foreground = (ss->sgx)->enved_waveform_color;
      ggc = XtGetGC(drawer, GCBackground | GCForeground | GCFunction, &gv);

      XtManageChild(enved_dialog); /* needed so that window is valid when resize callback is invoked */
      applyB = XmMessageBoxGetChild(enved_dialog, XmDIALOG_OK_BUTTON);
      cancelB = XmMessageBoxGetChild(enved_dialog, XmDIALOG_CANCEL_BUTTON);

      XtAddCallback(drawer, XmNresizeCallback, drawer_resize, NULL);
      XtAddCallback(drawer, XmNexposeCallback, drawer_resize, NULL);

      XtAddEventHandler(drawer, ButtonPressMask, false, drawer_button_press, NULL);
      XtAddEventHandler(drawer, ButtonMotionMask, false, drawer_button_motion, NULL);
      XtAddEventHandler(drawer, ButtonReleaseMask, false, drawer_button_release, NULL);

      if (enved_all_envs_top() == 0)
	set_sensitive(showB, false);
      set_sensitive(revertB, false);
      set_sensitive(deleteB, false);
      set_sensitive(undoB, false);
      set_sensitive(redoB, false);
      set_sensitive(saveB, false);
      if (!(selection_is_active())) 
	set_sensitive(selectionB, false);

      XmToggleButtonSetState(clipB, (Boolean)(enved_clip_p(ss)), false);
      XmToggleButtonSetState(graphB, (Boolean)(enved_wave_p(ss)), false);
      XmToggleButtonSetState(dBB, (Boolean)(enved_in_dB(ss)), false);

      FREE(n1);
      FREE(n2);

      reflect_apply_state();
      reflect_segment_state();
      set_dialog_widget(ENVED_DIALOG, enved_dialog);
    }
  else raise_dialog(enved_dialog);
  if (!XtIsManaged(enved_dialog)) 
    XtManageChild(enved_dialog);
  active_channel = current_channel();
  set_sensitive(mixB, (any_mix_id() != INVALID_MIX_ID));
  return(enved_dialog);
}

void set_enved_clip_p(bool val) 
{
  in_set_enved_clip_p(val); 
  if (enved_dialog) 
    XmToggleButtonSetState(clipB, (Boolean)val, false);
}

void set_enved_exp_p(bool val) 
{
  in_set_enved_exp_p(val); 
  reflect_segment_state();
}

void set_enved_target(enved_target_t val) 
{
  in_set_enved_target(val); 
  if (enved_dialog) 
    reflect_apply_state();
}

void set_enved_wave_p(bool val) 
{
  in_set_enved_wave_p(val); 
  if (enved_dialog) 
    XmToggleButtonSetState(graphB, (Boolean)val, false);
}

void set_enved_in_dB(bool val) 
{
  in_set_enved_in_dB(val);
  if (enved_dialog) 
    XmToggleButtonSetState(dBB, (Boolean)val, false);
}

void set_enved_base(Float val) 
{
  in_set_enved_base(val); 
  if (enved_dialog) 
    reflect_changed_base(val);
}

bool enved_dialog_is_active(void)
{
  return((enved_dialog) && (XtIsManaged(enved_dialog)));
}

void set_enved_filter_order(int order)
{
  char str[LABEL_BUFFER_SIZE];
  if ((order > 0) && (order < 2000))
    {
      if (order & 1) 
	in_set_enved_filter_order(order + 1);
      else in_set_enved_filter_order(order);
      if (enved_dialog)
	{
	  mus_snprintf(str, LABEL_BUFFER_SIZE, "%d", enved_filter_order(ss));
	  XmTextFieldSetString(orderL, str);
	  if ((enved_dialog) && 
	      (enved_target(ss) == ENVED_SPECTRUM) && 
	      (enved_wave_p(ss)) && (!showing_all_envs)) 
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
	  if (!(ss->using_schemes))
	    XmChangeColor(selectionB, (Pixel)(ss->sgx)->highlight_color);
	}
      if ((enved_target(ss) != ENVED_SPECTRUM) && 
	  (enved_wave_p(ss)) && 
	  (!showing_all_envs)) 
	env_redisplay();
    }
}

void color_enved_waveform(Pixel pix)
{
  (ss->sgx)->enved_waveform_color = pix;
  if (enved_dialog)
    {
      XSetForeground(MAIN_DISPLAY(ss), ggc, pix);
      if ((enved_wave_p(ss)) && 
	  (enved_dialog)) 
	env_redisplay();
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
    env_redisplay();
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
	XmListSelectPos(screnvlst, pos + 1, false);
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
    set_label(firB, (FIR_p) ? "fir" : "fft");
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
				    XEN_CONS(XEN_WRAP_WIDGET(screnvlst),
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
  XEN_DEFINE_PROCEDURE("enved-dialog-widgets", g_enved_dialog_widgets_w, 0, 0, 0, "internal testing function");
  XEN_DEFINE_PROCEDURE("enved-axis-info",  g_enved_axis_info_w, 0, 0, 0, "internal testing function");
#endif
}
