#include "snd.h"

/* envelope editor and viewer */


static Widget enved_dialog = NULL;
static Widget applyB, apply2B, cancelB, drawer, showB, saveB, revertB, undoB, redoB;
static Widget brkptL, graphB, fltB, ampB, srcB, clipB;
static Widget nameL, textL, screnvlst, dBB, orderL, resetB, firB = NULL;
static Widget baseScale, baseValue, selectionB;
static GC gc, rgc, ggc;

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
static bool is_FIR = true;
static bool old_clipping = false;
static bool ignore_button_release = false;
static bool cancelling = true;


static void fixup_graphics_context(graphics_context *ax, Widget w, GC gc)
{
  ax->dp = XtDisplay(w);
  ax->wn = XtWindow(w);
  if (gc) ax->gc = gc;
}


axis_info *enved_make_axis(const char *name, graphics_context *ax, 
			   int ex0, int ey0, int width, int height, 
			   mus_float_t xmin, mus_float_t xmax, mus_float_t ymin, mus_float_t ymax,
			   printing_t printing)
{
  /* conjure up minimal context for axis drawer in snd-axis.c */
  if (!axis) 
    {
      axis = (axis_info *)calloc(1, sizeof(axis_info));
      axis->ax = (graphics_context *)calloc(1, sizeof(graphics_context));
      fixup_graphics_context(axis->ax, drawer, ax->gc);
    }
  if (!gray_ap) 
    {
      gray_ap = (axis_info *)calloc(1, sizeof(axis_info));
      gray_ap->ax = (graphics_context *)calloc(1, sizeof(graphics_context));
      gray_ap->graph_active = true;
      fixup_graphics_context(gray_ap->ax, drawer, ggc);
    }
  init_env_axes(axis, name, ex0, ey0, width, height, xmin, xmax, ymin, ymax, printing);
  return(axis);
}


static void display_env(env *e, const char *name, GC cur_gc, int x0, int y0, int width, int height, bool dots, printing_t printing)
{
  graphics_context *ax = NULL;  
  ax = (graphics_context *)calloc(1, sizeof(graphics_context));
  ax->wn = XtWindow(drawer);
  if (!(ax->wn)) return;
  ax->dp = XtDisplay(drawer);
  ax->gc = cur_gc;
  ss->enved->with_dots = dots;
  env_editor_display_env(ss->enved, e, ax, name, x0, y0, width, height, printing);
  free(ax);
}


void display_enved_env_with_selection(env *e, const char *name, int x0, int y0, int width, int height, bool dots, printing_t printing)
{
  display_env(e, name, (selected_env == e) ? rgc : gc, x0, y0, width, height, dots, printing);
}


static void reflect_segment_state(void)
{
  if ((enved_dialog) &&
      (active_env) && 
      (!(showing_all_envs)))
    env_redisplay();
}


static void prepare_env_edit(env *new_env)
{
  prepare_enved_edit(new_env);
  if (new_env->base == 1.0)
    {set_enved_style(ENVELOPE_LINEAR);}
  else
    {
      set_enved_base(new_env->base);
      set_enved_style(ENVELOPE_EXPONENTIAL);
    }
  reflect_segment_state();
}


void set_enved_redo_sensitive(bool val) {set_sensitive(redoB, val);}
void set_enved_revert_sensitive(bool val) {set_sensitive(revertB, val);}
void set_enved_undo_sensitive(bool val) {set_sensitive(undoB, val);}
void set_enved_save_sensitive(bool val) {set_sensitive(saveB, val);}
void set_enved_show_sensitive(bool val) {set_sensitive(showB, val);}


static bool use_listener_font = false;

void make_scrolled_env_list(void)
{
  XmString *strs;
  int n, size;
  size = enved_all_envs_top();
  XtVaSetValues(screnvlst, XmNbackground, ss->highlight_color, NULL); 
  strs = (XmString *)calloc(size, sizeof(XmString)); 
  for (n = 0; n < size; n++) 
    strs[n] = XmStringCreate(enved_all_names(n), (char *)((use_listener_font) ? "listener_font" : XmFONTLIST_DEFAULT_TAG));
  XtVaSetValues(screnvlst, 
		XmNitems, strs, 
		XmNitemCount, size, 
		NULL);
  for (n = 0; n < size; n++) 
    XmStringFree(strs[n]);
  free(strs);
}


void enved_reflect_peak_env_completion(snd_info *sp)
{
  if ((enved_dialog) && (active_channel) && (enved_with_wave(ss)))
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
  if (!cancelling)
    {
      if (ss->checking_explicitly)
	ss->stopped_explicitly = true;
    }
  else XtUnmanageChild(enved_dialog);
}


static void help_enved_callback(Widget w, XtPointer context, XtPointer info) 
{
  envelope_editor_dialog_help();
}


static bool within_selection_src = false;

static void apply_enved(void)
{
  char *origin = NULL, *estr = NULL;
  if (active_env)
    {
      int i, j;
      env *max_env = NULL;
      active_channel = current_channel();
      if (active_channel)
	{
	  set_sensitive(applyB, false);
	  set_sensitive(apply2B, false);
	  set_button_label(cancelB, I_STOP);
	  cancelling = false;
	  check_for_event();
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
				  to_proc_name((apply_to_selection) ? S_env_selection : S_env_channel),
				  estr = env_to_string(active_env),
				  (apply_to_selection) ? "" : PROC_SEP "0" PROC_SEP PROC_FALSE);
#endif
	      apply_env(active_channel, active_env, 0,
			current_samples(active_channel), 
			apply_to_selection, 
			origin, NULL,
			C_int_to_Xen_integer(AT_CURRENT_EDIT_POSITION), 0);
	      /* calls update_graph, I think, but in short files that doesn't update the amp-env */
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
				  to_proc_name((apply_to_selection) ? S_filter_selection : S_filter_channel),
				  estr = env_to_string(active_env), 
				  enved_filter_order(ss),
				  (apply_to_selection) ? "" : PROC_SEP "0" PROC_SEP PROC_FALSE);
#endif
	      apply_filter(active_channel,
			   (is_FIR) ? enved_filter_order(ss) : 0,
			   active_env, 
			   origin, NULL, apply_to_selection,
			   NULL, NULL,
			   C_int_to_Xen_integer(AT_CURRENT_EDIT_POSITION), 0, false);
	      if (estr) free(estr);
	      if (origin) free(origin);
	      break;
	    case ENVED_SRATE:
	      /* mus_src no longer protects against 0 srate */
	      max_env = copy_env(active_env);
	      for (i = 0, j = 1; i < max_env->pts; i++, j += 2)
		if (max_env->data[j] < .01) 
		  max_env->data[j] = .01;
	      within_selection_src = true;
	      src_env_or_num(active_channel, max_env, 0.0, 
			     false, "Enved: src", 
			     apply_to_selection, NULL,
			     C_int_to_Xen_integer(AT_CURRENT_EDIT_POSITION), 0);
	      within_selection_src = false;
	      max_env = free_env(max_env);
	      break;
	    }
	  if (enved_with_wave(ss)) env_redisplay();
	  set_sensitive(applyB, true);
	  set_sensitive(apply2B, true);
	  set_button_label(cancelB, I_GO_AWAY);
	  cancelling = true;
	}
    }
}


static void env_redisplay_1(printing_t printing)
{
  if (enved_dialog_is_active())
    {
      XClearWindow(XtDisplay(drawer), XtWindow(drawer));
      if (showing_all_envs) 
	view_envs(env_window_width, env_window_height, printing);
      else 
	{
	  char *name = NULL;
	  name = XmTextGetString(textL);
	  if (!name) name = mus_strdup("noname");
	  /* active_env can be null here if just showing axes (empty initial graph) */
	  
	  if ((enved_with_wave(ss)) &&
	      (active_channel) &&
	      (!(active_channel->squelch_update)))
	    {
	      if ((enved_target(ss) == ENVED_SPECTRUM) && (active_env) && (is_FIR) && (printing == NOT_PRINTING))
		display_frequency_response(active_env, axis, gray_ap->ax, enved_filter_order(ss), enved_in_dB(ss));
	      enved_show_background_waveform(axis, gray_ap, apply_to_selection, (enved_target(ss) == ENVED_SPECTRUM), printing);
	    }

	  display_env(active_env, name, gc, 0, 0, env_window_width, env_window_height, true, printing);
	  if (name) XtFree(name);
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
      (enved_with_wave(ss)) &&
      (enved_target(ss) == ENVED_AMPLITUDE) &&
      (cp == active_channel) &&
      ((!apply_to_selection) || (selection_is_active_in_channel(cp))))
    env_redisplay();
}


static void enved_reset(void)
{
  set_enved_clipping(DEFAULT_ENVED_CLIPPING);
  set_enved_style(ENVELOPE_LINEAR);
  set_enved_power(DEFAULT_ENVED_POWER);
  set_enved_base(DEFAULT_ENVED_BASE);
  set_enved_target(DEFAULT_ENVED_TARGET);
  set_enved_with_wave(DEFAULT_ENVED_WITH_WAVE);
  set_enved_in_dB(DEFAULT_ENVED_IN_DB);
  XmTextSetString(textL, NULL);
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


static void clear_point_label(void);

static void clear_xenv_error(void)
{
  if (brkptL) 
    clear_point_label();
}


static void unpost_xenv_error(XtPointer data, XtIntervalId *id)
{
  clear_xenv_error();
}


static void errors_to_xenv_text(const char *msg, void *data)
{
  set_button_label(brkptL, msg);
  XtAppAddTimeOut(MAIN_APP(ss),
		  5000,
		  (XtTimerCallbackProc)unpost_xenv_error,
		  NULL);
}


static void order_field_activated(void)
{
  /* return in order text field */
  char *str = NULL;
  str = XmTextGetString(orderL);
  if ((str) && (*str))
    {
      int order;
      redirect_errors_to(errors_to_xenv_text, NULL);
      order = string_to_int(str, 1, "order");
      redirect_errors_to(NULL, NULL);
      if (order & 1) order++;
      if ((order > 0) && 
	  (order < 2000)) 
	set_enved_filter_order(order);
      else widget_int_to_text(orderL, enved_filter_order(ss));
    }
  if (str) XtFree(str);
}


static void text_field_activated(void)
{ /* might be breakpoints to load or an envelope name (<cr> in enved text field) */
  char *name = NULL, *str;
  name = XmTextGetString(textL);
  if ((name) && (*name))
    {
      env *e = NULL;
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
	      /* e is null here */
	    }
	  else 
	    {
	      redirect_errors_to(errors_to_xenv_text, NULL);
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
	  e = free_env(e);
	}
    }
  if (name) XtFree(name);
}

static void text_activate_callback(Widget w, XtPointer context, XtPointer info) 
{
  text_field_activated();
}


static void order_activate_callback(Widget w, XtPointer context, XtPointer info) 
{
  order_field_activated();
}


static void save_button_pressed(Widget w, XtPointer context, XtPointer info) 
{
  char *name = NULL;
  if (active_env == NULL) return;
  name = XmTextGetString(textL);
  if ((!name) || (!(*name))) 
    name = mus_strdup("unnamed");
  alert_envelope_editor(name, copy_env(active_env));
  add_or_edit_symbol(name, active_env);
  set_sensitive(saveB, false);
  env_redisplay();
  if (name) XtFree(name);
}


static void apply_enved_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* apply current envs to currently sync'd channels */
  Widget active_widget;
  active_widget = XmGetFocusWidget(enved_dialog);
  if (active_widget == XmMessageBoxGetChild(enved_dialog, XmDIALOG_OK_BUTTON))
    {
      apply_enved();
      last_active_channel = active_channel;
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
      clear_status_area(active_channel->sound);
    }
  apply_enved();
  last_active_channel = active_channel;
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
  XmTextSetString(textL, enved_all_names(pos));
  set_enved_env_list_top(0);
  prepare_env_edit(active_env);
  set_sensitive(undoB, false);
  set_sensitive(revertB, false);
  set_sensitive(saveB, false);
  env_redisplay();
}


static void clear_point_label(void)
{
  XtVaSetValues(brkptL, XmNlabelType, XmSTRING, XmNlabelString, NULL, NULL);
}


static void enved_display_point_label(mus_float_t x, mus_float_t y)
{
  char brkpt_buf[LABEL_BUFFER_SIZE];
  if ((enved_in_dB(ss)) && (min_dB(ss) < -60))
    snprintf(brkpt_buf, LABEL_BUFFER_SIZE, "%.3f : %.5f", x, y);
  else snprintf(brkpt_buf, LABEL_BUFFER_SIZE, "%.3f : %.3f", x, y);
  set_button_label(brkptL, brkpt_buf);
}


#ifdef __APPLE__
static int press_x, press_y;
#endif

static void drawer_button_motion(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  XMotionEvent *ev = (XMotionEvent *)event;
  ignore_button_release = false;

  if (!showing_all_envs)
    {
      mus_float_t x, y;
#ifdef __APPLE__
      if ((ev->x == press_x) && (ev->y == press_y)) return;
#endif
      env_editor_button_motion_with_xy(ss->enved, ev->x, ev->y, ev->time, active_env, &x, &y);
      enved_display_point_label(x, y);
      env_redisplay();
    }
}


static void drawer_button_press(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  XButtonEvent *ev = (XButtonEvent *)event;
  int pos;
#ifdef __APPLE__
  press_x = ev->x;
  press_y = ev->y;
#endif
  ss->enved->down_time = ev->time;
  ss->enved->env_dragged = false;
  if (showing_all_envs)
    {
      pos = hit_env(ev->x, ev->y, env_window_width, env_window_height);
      XmListSelectPos(screnvlst, pos + 1, false);
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
      if (env_editor_button_press(ss->enved, ev->x, ev->y, ev->time, active_env))
	env_redisplay();
      enved_display_point_label(ungrf_x(ss->enved->axis, ev->x), env_editor_ungrf_y_dB(ss->enved, ev->y));
      set_sensitive(saveB, true);
      set_sensitive(undoB, true);
      set_sensitive(revertB, true);
    }
}


static void drawer_button_release(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  if (ignore_button_release)
    ignore_button_release = false;
  else
    {
      if ((active_env) && (!showing_all_envs))
	{
	  env_editor_button_release(ss->enved, active_env);
	  env_redisplay();
	  clear_point_label();
	}
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
  set_button_label(showB, (showing_all_envs) ? "edit env" : "view envs");
  env_redisplay();
}


static void selection_button_pressed(Widget s, XtPointer context, XtPointer info) 
{
  we_turned_selection_off = false;
  apply_to_selection = (!apply_to_selection);
  XmChangeColor(selectionB, (apply_to_selection) ? ((Pixel)ss->yellow) : ((Pixel)ss->highlight_color));
  set_sensitive(apply2B, true);
  if ((enved_with_wave(ss)) && 
      (!showing_all_envs))
    env_redisplay();
}


static void revert_button_pressed(Widget w, XtPointer context, XtPointer info) 
{
  revert_env_edit();
  if (active_env) active_env = free_env(active_env);
  active_env = enved_next_env();
  if (active_env == NULL)
    text_field_activated();
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


static void reflect_apply_state(void)
{
  set_label(nameL, env_names[enved_target(ss)]);
  XmChangeColor(ampB, (enved_target(ss) == ENVED_AMPLITUDE) ? ((Pixel)ss->yellow) : ((Pixel)ss->highlight_color));
  XmChangeColor(fltB, (enved_target(ss) == ENVED_SPECTRUM) ? ((Pixel)ss->yellow) : ((Pixel)ss->highlight_color));
  XmChangeColor(srcB, (enved_target(ss) == ENVED_SRATE) ? ((Pixel)ss->yellow) : ((Pixel)ss->highlight_color));
  if ((!showing_all_envs) && 
      (enved_with_wave(ss))) 
    env_redisplay();
}


static void freq_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  in_set_enved_target(ENVED_SPECTRUM);
  old_clipping = enved_clipping(ss);
  set_enved_clipping(true);
  reflect_apply_state();
}


static void amp_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  if (enved_target(ss) == ENVED_SPECTRUM)
    set_enved_clipping(old_clipping);
  in_set_enved_target(ENVED_AMPLITUDE);
  reflect_apply_state();
}


static void src_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  if (enved_target(ss) == ENVED_SPECTRUM)
    set_enved_clipping(old_clipping);
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


static void env_browse_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmListCallbackStruct *cb = (XmListCallbackStruct *)info;
  select_or_edit_env(cb->item_position - 1);
}


static void graph_button_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  in_set_enved_with_wave(cb->set);
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
  in_set_enved_clipping(cb->set);
}



#define BASE_MAX 400
#define BASE_MID 200
/* these two just set the granularity of the scale widget, not the user-visible bounds */

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
  for (i = 0; i < scale_len - 1; i++) 
    buf[i] = sfs[i];
  set_button_label(baseValue, buf);
  free(sfs);
  free(buf);
  in_set_enved_base(bval);
  if ((active_env) && 
      (!(showing_all_envs))) 
    {
      active_env->base = enved_base(ss);
      if (active_env->base == 1.0)
	{set_enved_style(ENVELOPE_LINEAR);}
      else {set_enved_style(ENVELOPE_EXPONENTIAL);}
      env_redisplay();
    }
}


static void base_changed(int val)
{
  mus_float_t bval;
  if (val == 0) 
    bval = 0.0;
  else 
    {
      if (val == BASE_MID)
	bval = 1.0;
      else
	{
	  if (val > BASE_MID)
	    bval = pow(1.0 + (10.0 * ((mus_float_t)(val - BASE_MID) / (mus_float_t)BASE_MID)), enved_power(ss));  
	  else 
	    bval = pow(((mus_float_t)val / (mus_float_t)BASE_MID), enved_power(ss) - 1.0);
	}
    }
  make_base_label(bval);
  if (active_env)
    set_sensitive(saveB, true); /* what about undo/redo here? */
}


static void reflect_changed_base(mus_float_t val)
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
  base_changed(sb->value);
}


static int base_last_value = BASE_MID;

static void base_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScrollBarCallbackStruct *sb = (XmScrollBarCallbackStruct *)info;
  base_last_value = sb->value;
  base_changed(sb->value);
}


static void base_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  XButtonEvent *ev;
  int val;
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    val = base_last_value; 
  else val = BASE_MID; /* this is supposedly 1.0 */
  base_changed(val);
  XtVaSetValues(baseScale, XmNvalue, val, NULL);
}


static void FIR_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  is_FIR = (!is_FIR);
  set_label(w, (is_FIR) ? "fir" : "fft");
  if (enved_with_wave(ss)) env_redisplay();
}


static void reflect_sound_state(void)
{
  bool file_on;
  file_on = (bool)(any_selected_sound());
  set_sensitive(applyB, file_on);
  set_sensitive(apply2B, file_on);
}


static Xen reflect_file_in_enved(Xen hook_or_reason)
{
  if (enved_dialog) reflect_sound_state();
  return(Xen_false);
}


Xen_wrap_1_arg(reflect_file_in_enved_w, reflect_file_in_enved)


Widget create_envelope_editor(void)
{
  if (!enved_dialog)
    {
      int n;
      Arg args[32];
      Widget colE, colD, colB, colF;
      Widget spacer, spacer1, aform, mainform, screnvname, baseSep, baseLabel;
      XmString xhelp, xdismiss, xapply, titlestr, s1;
      XGCValues gv;
      XtCallbackList n1, n2;
      char str[LABEL_BUFFER_SIZE];

      /* -------- DIALOG -------- */
      xdismiss = XmStringCreateLocalized((char *)I_GO_AWAY);
      xhelp = XmStringCreateLocalized((char *)I_HELP);
      titlestr = XmStringCreateLocalized((char *)"Edit Envelope");
      xapply = XmStringCreateLocalized((char *)"Apply");

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNcancelLabelString, xdismiss); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNokLabelString, xapply); n++;
      XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNtransient, false); n++;
      enved_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), (char *)"envelope editor", args, n);
  
      XtAddCallback(enved_dialog, XmNcancelCallback, dismiss_enved_callback, NULL);
      XtAddCallback(enved_dialog, XmNhelpCallback, help_enved_callback, NULL);
      XtAddCallback(enved_dialog, XmNokCallback, apply_enved_callback, NULL);

      XmStringFree(xhelp);
      XmStringFree(xdismiss);
      XmStringFree(titlestr);
      XmStringFree(xapply);

      XtVaSetValues(XmMessageBoxGetChild(enved_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, ss->selection_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(enved_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, ss->selection_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(enved_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, ss->selection_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(enved_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->highlight_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(enved_dialog, XmDIALOG_HELP_BUTTON), XmNbackground, ss->highlight_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(enved_dialog, XmDIALOG_OK_BUTTON), XmNbackground, ss->highlight_color, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->selection_color); n++;
      apply2B = XtCreateManagedWidget("Undo&Apply", xmPushButtonGadgetClass, enved_dialog, args, n);
      XtAddCallback(apply2B, XmNactivateCallback, undo_and_apply_enved_callback, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNforeground, ss->black); n++;
      XtSetArg(args[n], XmNarmColor, ss->selection_color); n++;
      resetB = XtCreateManagedWidget("Clear graph", xmPushButtonGadgetClass, enved_dialog, args, n);
      XtAddCallback(resetB, XmNactivateCallback, reset_button_callback, NULL);


      /* -------- MAIN WIDGET -------- */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      baseLabel = make_pushbutton_widget("exp:", mainform, args, n);
      XtAddCallback(baseLabel, XmNactivateCallback, base_click_callback, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      s1 = XmStringCreateLocalized((char *)"1.000");
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, baseLabel); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, baseLabel); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      /*      XtSetArg(args[n], XmNrecomputeSize, false); n++; */
      XtSetArg(args[n], XmNlabelString, s1); n++;
      baseValue = XtCreateManagedWidget("base-label", xmLabelWidgetClass, mainform, args, n);
      XmStringFree(s1);

      /* -------- filter order -------- */
      n = 0;      
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNcolumns, 3); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNheight, 24); n++;
      XtSetArg(args[n], XmNresizeWidth, false); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNmarginHeight, 0); n++;
      XtSetArg(args[n], XmNmarginBottom, 0); n++;
      snprintf(str, LABEL_BUFFER_SIZE, "%d", enved_filter_order(ss));
      XtSetArg(args[n], XmNvalue, str); n++;
      orderL = make_textfield_widget("orderL", mainform, args, n, ACTIVATABLE, NO_COMPLETER);
      XtAddCallback(orderL, XmNactivateCallback, order_activate_callback, NULL);

      /* -------- fft/fir choice -------- */
      n = 0;      
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, orderL); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      firB = make_pushbutton_widget((char *)((is_FIR) ? "fir" : "fft"), mainform, args, n);
      XtAddCallback(firB, XmNactivateCallback, FIR_click_callback, NULL);

      /* -------- exp base scale -------- */
      n = 0;      
      XtSetArg(args[n], XmNbackground, ss->position_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
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
      s1 = XmStringCreateLocalized((char *)"amp env:");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, baseSep); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, nameL); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      textL = make_textfield_widget("textL", mainform, args, n, ACTIVATABLE, add_completer_func(env_name_completer, NULL));
      XtAddCallback(textL, XmNactivateCallback, text_activate_callback, NULL);


      /* -------- dB, GRAPH ('wave') AND CLIP BUTTONS -------- */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNselectColor, ss->selection_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, baseSep); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
      dBB = make_togglebutton_widget("dB", mainform, args, n);
      XtAddCallback(dBB, XmNvalueChangedCallback, dB_button_callback, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNselectColor, ss->selection_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, baseSep); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, dBB); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
      graphB = make_togglebutton_widget("wave", mainform, args, n);
      XtAddCallback(graphB, XmNvalueChangedCallback, graph_button_callback, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNselectColor, ss->selection_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, baseSep); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, graphB); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
      clipB = make_togglebutton_widget("clip", mainform, args, n);
      XtAddCallback(clipB, XmNvalueChangedCallback, clip_button_callback, NULL);

      /* -------- BREAKPOINT DATA DISPLAY LABEL -------- */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, spacer1); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      aform = XtCreateManagedWidget("aform", xmFormWidgetClass, mainform, args, n);

      /* -------- BUTTON BOX AT TOP LEFT -------- */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->zoom_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNshadowThickness, 4); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      colF = XtCreateManagedWidget("env-button-frame", xmFrameWidgetClass, aform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;
      colB = XtCreateManagedWidget("env-button-holder", xmFormWidgetClass, colF, args, n);

      /* VIEW ENVS */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->selection_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      showB = XtCreateManagedWidget("view envs", xmPushButtonWidgetClass, colB, args, n);
      XtAddCallback(showB, XmNactivateCallback, show_button_pressed, NULL);

      /* SAVE PRINT */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->selection_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, showB); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      saveB = XtCreateManagedWidget("define it", xmPushButtonWidgetClass, colB, args, n);

      XtAddCallback(saveB, XmNactivateCallback, save_button_pressed, NULL);

      /* REVERT */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->selection_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, saveB); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      revertB = XtCreateManagedWidget("revert", xmPushButtonWidgetClass, colB, args, n);

      XtAddCallback(revertB, XmNactivateCallback, revert_button_pressed, NULL);


      /* UNDO REDO */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->selection_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, revertB); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 50); n++;
      undoB = XtCreateManagedWidget("undo", xmPushButtonWidgetClass, colB, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->selection_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, undoB); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, undoB); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      redoB = XtCreateManagedWidget("redo", xmPushButtonWidgetClass, colB, args, n);

      XtAddCallback(undoB, XmNactivateCallback, undo_button_pressed, NULL);
      XtAddCallback(redoB, XmNactivateCallback, redo_button_pressed, NULL);


      /* AMP FLT SRC */
      /* enved_function (target) choice (a row of three push buttons that acts like a "radio box") */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->yellow); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, undoB); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 33); n++;
      ampB = XtCreateManagedWidget("amp", xmPushButtonWidgetClass, colB, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->yellow); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, ampB); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, ampB); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 67); n++;
      fltB = XtCreateManagedWidget("flt", xmPushButtonWidgetClass, colB, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->yellow); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, fltB); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, fltB); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      srcB = XtCreateManagedWidget("src", xmPushButtonWidgetClass, colB, args, n);

      XtAddCallback(fltB, XmNactivateCallback, freq_button_callback, NULL);
      XtAddCallback(ampB, XmNactivateCallback, amp_button_callback, NULL);
      XtAddCallback(srcB, XmNactivateCallback, src_button_callback, NULL);

      /* SELECTION */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->selection_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, ampB); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      selectionB = make_pushbutton_widget("selection", colB, args, n);

      XtAddCallback(selectionB, XmNactivateCallback, selection_button_pressed, NULL);


      /* -------- ENV LIST AT LEFT UNDER BUTTONS -------- */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, colF); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNshadowThickness, 4); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      colE = XtCreateManagedWidget("env-list-frame", xmFrameWidgetClass, aform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      colD = XtCreateManagedWidget("env-list-holder", xmFormWidgetClass, colE, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      screnvname = XtCreateManagedWidget("envs:", xmLabelWidgetClass, colD, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, screnvname); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      if (ss->listener_fontlist) 
	{
	  XtSetArg(args[n], XmNfontList, 0); n++;
	  XtSetArg(args[n], XM_FONT_RESOURCE, ss->listener_fontlist); n++;
	  use_listener_font = true;
	}
      screnvlst = XmCreateScrolledList(colD, (char *)"scrolled-env-list", args, n);
      XtManageChild(screnvlst); 
      XtAddCallback(screnvlst, XmNbrowseSelectionCallback, env_browse_callback, NULL);
      map_over_children(screnvlst, set_main_color_of_widget);
      if (enved_all_envs_top() > 0) make_scrolled_env_list();

      /* -------- MAIN GRAPH -------- */

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->graph_color); n++;
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
      gv.foreground = ss->red;
      rgc = XtGetGC(drawer, GCBackground | GCForeground | GCFunction, &gv);
      gv.foreground = ss->enved_waveform_color;
      ggc = XtGetGC(drawer, GCBackground | GCForeground | GCFunction, &gv);

      XtManageChild(enved_dialog); /* needed so that window is valid when resize callback is invoked */
      applyB = XmMessageBoxGetChild(enved_dialog, XmDIALOG_OK_BUTTON);
      cancelB = XmMessageBoxGetChild(enved_dialog, XmDIALOG_CANCEL_BUTTON);
      cancelling = true;

      XtAddCallback(drawer, XmNresizeCallback, drawer_resize, NULL);
      XtAddCallback(drawer, XmNexposeCallback, drawer_resize, NULL);

      XtAddEventHandler(drawer, ButtonPressMask, false, drawer_button_press, NULL);
      XtAddEventHandler(drawer, ButtonMotionMask, false, drawer_button_motion, NULL);
      XtAddEventHandler(drawer, ButtonReleaseMask, false, drawer_button_release, NULL);

      if (enved_all_envs_top() == 0)
	set_sensitive(showB, false);
      set_sensitive(revertB, false);
      set_sensitive(undoB, false);
      set_sensitive(redoB, false);
      set_sensitive(saveB, false);
      if (!(selection_is_active())) 
	set_sensitive(selectionB, false);

      XmToggleButtonSetState(clipB, (Boolean)(enved_clipping(ss)), false);
      XmToggleButtonSetState(graphB, (Boolean)(enved_with_wave(ss)), false);
      XmToggleButtonSetState(dBB, (Boolean)(enved_in_dB(ss)), false);

      free(n1);
      free(n2);

      reflect_apply_state();
      reflect_segment_state();
      reflect_sound_state();

      set_dialog_widget(ENVED_DIALOG, enved_dialog);

      Xen_add_to_hook_list(ss->snd_open_file_hook, reflect_file_in_enved_w, "enved-file-open-handler", "enved dialog's file-open-hook handler");
    }
  else raise_dialog(enved_dialog);
  if (!XtIsManaged(enved_dialog)) 
    XtManageChild(enved_dialog);
  active_channel = current_channel();
  return(enved_dialog);
}


void set_enved_clipping(bool val) 
{
  in_set_enved_clipping(val); 
  if (enved_dialog) 
    XmToggleButtonSetState(clipB, (Boolean)val, false);
}


void reflect_enved_style(void)
{
  reflect_segment_state();
}


void set_enved_target(enved_target_t val) 
{
  in_set_enved_target(val); 
  if (enved_dialog) 
    reflect_apply_state();
}


void set_enved_with_wave(bool val) 
{
  in_set_enved_with_wave(val); 
  if (enved_dialog) 
    XmToggleButtonSetState(graphB, (Boolean)val, false);
}


void set_enved_in_dB(bool val) 
{
  in_set_enved_in_dB(val);
  if (enved_dialog) 
    XmToggleButtonSetState(dBB, (Boolean)val, false);
}


void set_enved_base(mus_float_t val) 
{
  in_set_enved_base(val); 
  if (enved_dialog) 
    reflect_changed_base(val);
}


bool enved_dialog_is_active(void)
{
  return((enved_dialog) && 
	 (XtIsManaged(enved_dialog)));
}


void set_enved_filter_order(int order)
{
  if ((order > 0) && (order < 2000))
    {
      if (order & 1) 
	{in_set_enved_filter_order(order + 1);}
      else {in_set_enved_filter_order(order);}
      if (enved_dialog)
	{
	  widget_int_to_text(orderL, enved_filter_order(ss));
	  if ((enved_dialog) && 
	      (enved_target(ss) == ENVED_SPECTRUM) && 
	      (enved_with_wave(ss)) && (!showing_all_envs)) 
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
      XmChangeColor(selectionB, (apply_to_selection) ? ((Pixel)ss->yellow) : ((Pixel)ss->highlight_color));
      if ((enved_target(ss) != ENVED_SPECTRUM) && 
	  (enved_with_wave(ss)) && 
	  (!showing_all_envs)) 
	env_redisplay();
    }
}



void color_enved_waveform(Pixel pix)
{
  if (enved_dialog)
    {
      XSetForeground(MAIN_DISPLAY(ss), ggc, pix);
      if ((enved_with_wave(ss)) && 
	  (enved_dialog)) 
	env_redisplay();
    }
}


static Xen g_enved_envelope(void)
{
  #define H_enved_envelope "(" S_enved_envelope "): current envelope editor displayed (active) envelope"
  return(env_to_xen(active_env));
}


static Xen g_set_enved_envelope(Xen e)
{
  Xen_check_type(Xen_is_list(e) || Xen_is_string(e) || Xen_is_symbol(e), e, 1, S_setB S_enved_envelope, "a list, symbol, or string");
  if (active_env) active_env = free_env(active_env);
  if ((Xen_is_string(e)) || (Xen_is_symbol(e)))
    active_env = name_to_env((Xen_is_string(e)) ? Xen_string_to_C_string(e) : Xen_symbol_to_C_string(e)); /* xen_to_env in name_to_env, so no copy */
  else active_env = xen_to_env(e);
  if ((!active_env) && (!(Xen_is_list(e))))
    Xen_error(NO_SUCH_ENVELOPE,
	      Xen_list_2(C_string_to_Xen_string(S_setB S_enved_envelope ": bad envelope arg: ~A"),
			 e));
  if (enved_dialog) 
    env_redisplay();
  return(e);
}


static Xen g_enved_filter(void)
{
  #define H_enved_filter "(" S_enved_filter "): envelope editor FIR/FFT filter choice (" PROC_TRUE ": FIR)"
  return(C_bool_to_Xen_boolean(is_FIR));
}


static Xen g_set_enved_filter(Xen type)
{
  Xen_check_type(Xen_is_boolean(type), type, 1, S_setB S_enved_filter, "boolean");
  is_FIR = Xen_boolean_to_C_bool(type);
  if (firB)
    set_label(firB, (is_FIR) ? "fir" : "fft");
  return(type);
}


Xen_wrap_no_args(g_enved_filter_w, g_enved_filter)
Xen_wrap_1_arg(g_set_enved_filter_w, g_set_enved_filter)
Xen_wrap_no_args(g_enved_envelope_w, g_enved_envelope)
Xen_wrap_1_arg(g_set_enved_envelope_w, g_set_enved_envelope)

void g_init_gxenv(void)
{
  Xen_define_procedure_with_setter(S_enved_filter, g_enved_filter_w, H_enved_filter,
				   S_setB S_enved_filter, g_set_enved_filter_w,  0, 0, 1, 0);
  Xen_define_procedure_with_setter(S_enved_envelope, g_enved_envelope_w, H_enved_envelope,
				   S_setB S_enved_envelope, g_set_enved_envelope_w,  0, 0, 1, 0);
}
