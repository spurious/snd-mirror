#include "snd.h"

/* envelope editor and viewer */

static Widget enved_dialog = NULL;
static Widget mainform,applyB,apply2B,cancelB,drawer,colB,colF,showB,saveB,revertB,undoB,redoB,printB,brkptL,graphB,fltB,ampB,srcB,rbrow,clipB;
static Widget nameL,textL,screnvlst,screnvname,dBB,orderL;
static Widget expB,linB,lerow,baseScale,baseLabel,baseValue,baseSep,selectionB,mixB,selrow;
static GC gc,rgc,ggc;

static char *env_names[3] = {STR_amp_env_p,STR_flt_env_p,STR_src_env_p};

static int showing_all_envs = 0; /* edit one env (0), or view all currently defined envs (1) */
static int apply_to_selection = 0;
static int apply_to_mix = 0;

static int env_window_width = 0;
static int env_window_height = 0;

static chan_info *active_channel = NULL;

static env* selected_env = NULL; /* if during view, one env is clicked, it is "selected" and can be pasted elsewhere */
static env* active_env = NULL;   /* env currently being edited */


static chan_info *axis_cp = NULL;
static axis_info *gray_ap = NULL;

void enved_make_axis_cp(snd_state *ss, char *name, axis_context *ax, int ex0, int ey0, int width, int height, Float xmin, Float xmax, Float ymin, Float ymax)
{
  /* conjure up minimal context for axis drawer in snd-axis.c */
  if (!axis_cp) 
    {
      axis_cp = new_env_axis(ss);
      fixup_axis_context(axis_cp->axis->ax,drawer,ax->gc);
    }
  if (!gray_ap) 
    {
      gray_ap = new_wave_axis(ss);
      fixup_axis_context(gray_ap->ax,drawer,ggc);
    }
  init_env_axes(axis_cp,name,ex0,ey0,width,height,xmin,xmax,ymin,ymax);
}

static void display_env(snd_state *ss, env *e, char *name, GC cur_gc, int x0, int y0, int width, int height, int dots)
{
  axis_context *ax=NULL;  
  ax = (axis_context *)CALLOC(1,sizeof(axis_context));
  ax->wn = XtWindow(drawer);
  ax->dp = XtDisplay(drawer);
  ax->gc = cur_gc;
  display_enved_env(ss,e,ax,axis_cp,name,x0,y0,width,height,dots);
}

void display_enved_env_with_selection(snd_state *ss, env *e, char *name, int x0, int y0, int width, int height, int dots)
{
  display_env(ss,e,name,(selected_env == e) ? rgc : gc,x0,y0,width,height,dots);
}

static void do_env_edit(env *new_env, int loading)
{
  do_enved_edit(new_env);
  if (!loading)
    {
      set_sensitive(undoB,TRUE);
      set_sensitive(redoB,FALSE);
      set_sensitive(saveB,TRUE);
      set_sensitive(revertB,TRUE);
    }
}

void set_enved_redo_sensitive(int val) {set_sensitive(redoB,val);}
void set_enved_revert_sensitive(int val) {set_sensitive(revertB,val);}
void set_enved_undo_sensitive(int val) {set_sensitive(undoB,val);}
void set_enved_save_sensitive(int val) {set_sensitive(saveB,val);}
void set_enved_show_sensitive(int val) {set_sensitive(showB,val);}

void make_scrolled_env_list (snd_state *ss)
{
  XmString *strs;
  int n,size;
  size = enved_all_envs_top();
  if (!(ss->using_schemes)) XtVaSetValues(screnvlst,XmNbackground,(ss->sgx)->highlight_color,NULL); 
  strs = (XmString *)CALLOC(size,sizeof(XmString)); 
  for (n=0;n<size;n++) strs[n] = XmStringCreate(enved_all_names(n),"button_font");
  XtVaSetValues(screnvlst,XmNitems,strs,XmNitemCount,size,NULL);
  for (n=0;n<size;n++) XmStringFree(strs[n]);
  FREE(strs);
}

void alert_enved_amp_env(snd_info *sp)
{
  snd_state *ss;
  ss = sp->state;
  if ((enved_dialog) && (active_channel) && (enved_waving(ss)) && (XtIsManaged(enved_dialog)))
    {
      if (active_channel->sound == sp) env_redisplay(sp->state);
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

static void Dismiss_Enved_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  if (ss->checking_explicitly)
    ss->stopped_explicitly = 1;
  else XtUnmanageChild(enved_dialog);
}

static void Help_Enved_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  envelope_editor_dialog_help((snd_state *)clientData);
}

static void apply_enved(snd_state *ss)
{
  int mix_id=0;
  mixdata *md;
  snd_info *sp;
  if (active_env)
    {
      if (apply_to_mix)
	{
	  if (ss->selected_mix != NO_SELECTION)
	    mix_id = ss->selected_mix;
	  else mix_id = any_mix_id();
	  md = md_from_int(mix_id);
	  if (md) 
	    {
	      sp = make_mix_readable(md);
	      active_channel = sp->chans[((md->selected_chan != NO_SELECTION) ? (md->selected_chan) : 0)];
	    }
	}
      else active_channel = current_channel((void *)ss);
      if (active_channel)
	{
	  set_sensitive(applyB,FALSE);
	  set_sensitive(apply2B,FALSE);
	  set_button_label(cancelB,STR_Stop);
	  check_for_event(ss);
	  switch (enved_target(ss))
	    {
	    case AMPLITUDE_ENV:
	      if (apply_to_mix)
		set_mix_amp_env(mix_id,NO_SELECTION,active_env); /* chan = NO_SELECTION: use selected chan if more than 1 */
	      else apply_env(active_channel,active_env,0,current_ed_samples(active_channel),1.0,apply_to_selection,TRUE,"Enved: amp"); 
	      /* calls update_graph, I think, but in short files that doesn't update the amp-env */
	      if (enved_waving(ss)) env_redisplay(ss);
	      break;
	    case SPECTRUM_ENV: 
	      apply_filter(active_channel,filter_env_order(ss),active_env,TRUE,"Enved: flt",apply_to_selection,NULL);
	      break;
	    case SRATE_ENV:
	      src_env_or_num(ss,active_channel,active_env,0.0,FALSE,TRUE,"Enved: src",apply_to_selection);
	      if (enved_waving(ss)) env_redisplay(ss);
	      break;
	    }
	  set_sensitive(applyB,TRUE);
	  set_sensitive(apply2B,TRUE);
	  set_button_label(cancelB,STR_Dismiss);
	}
    }
}

void env_redisplay(snd_state *ss)
{
  char *name = NULL;
  XClearWindow(XtDisplay(drawer),XtWindow(drawer));
  if (showing_all_envs) 
    view_envs(ss,env_window_width,env_window_height);
  else 
    {
      name = XmTextGetString(textL);
      if (!name) name = copy_string("noname");
      display_env(ss,active_env,name,gc,0,0,env_window_width,env_window_height,1);
      if (name) XtFree(name);
      if (enved_waving(ss))
	{
	  if ((enved_target(ss) == SPECTRUM_ENV) && (active_env))
	    display_frequency_response(ss,active_env,axis_cp->axis,gray_ap->ax,filter_env_order(ss),enved_dBing(ss));
	  else enved_show_background_waveform(ss,axis_cp,gray_ap,apply_to_mix,apply_to_selection);
	}
    }
}

static void order_field_activated(snd_state *ss)
{
  /* return in order text field */
  char *str=NULL;
  int order;
  str = XmTextGetString(orderL);
  if ((str) && (*str))
    {
      order = string2int(str);
      if (order&1) order++;
      if ((order>0) && (order<2000)) set_filter_env_order(ss,order);
    }
  if (str) XtFree(str);
}

static void text_field_activated(snd_state *ss)
{ /* might be breakpoints to load or an envelope name (<cr> in enved text field) */
  char *name = NULL,*str;
  env *e = NULL;
  int newenv = 0;
  name = XmTextGetString(textL);
  if ((name) && (*name))
    {
      str = name;
      while (isspace((int)(*str))) str++;
      e = name_to_env(str);
      if (!e)
	{
	  e = string2env(str);
	  if (e) newenv = 1;
	}
      if (e) 
	{
	  if (active_env) active_env = free_env(active_env);
	  active_env = copy_env(e);
	  set_enved_env_list_top(0);
	  do_env_edit(active_env,TRUE);
	  set_sensitive(saveB,newenv);
	  env_redisplay(ss);
	}
      /* else if str is alphabetic, rename active_env as str and save a copy in the list */
      if (isalpha((int)(str[0])))
	{
	  alert_envelope_editor(ss,str,copy_env(active_env));
	  add_or_edit_symbol(str,active_env);
	  set_sensitive(saveB,FALSE);
	  env_redisplay(ss); /* updates label */
	}
    }
  if (name) XtFree(name);
}

static void save_button_pressed(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  char *name = NULL;
  name = XmTextGetString(textL);
  if ((!name) || (!(*name))) 
    name = copy_string("unnamed");
  alert_envelope_editor(ss,name,copy_env(active_env));
  add_or_edit_symbol(name,active_env);
  set_sensitive(saveB,FALSE);
  env_redisplay(ss);
  if (name) XtFree(name);
}

static void save_button_help_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Save Current Envelope",
"The envelope in the editor is not saved until you\n\
press the 'save' button.  Once saved, it can be\n\
used in other portions of Snd (for example, as the\n\
argument to C-x C-a), or saved to a file via the\n\
Option menu Save State option.  Similarly, a file\n\
of CLM (lisp) envelope definitions can be loaded\n\
using load.\n\
");
}

static void Apply_Enved_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  /* apply current envs to currently sync'd channels */
  snd_state *ss = (snd_state *)clientData;
  state_context *sgx;
  XmAnyCallbackStruct *cb = (XmAnyCallbackStruct *)callData;
  sgx = ss->sgx;
  /* since this is the OK button from Motif's point of view, text activation (<cr> in the text field)
   * causes a bogus activation of this callback; we trap that by looking for text_activate_event.
   * cr in text => save under that name
   */
  if (cb->event != sgx->text_activate_event)
    apply_enved((snd_state *)clientData);
  else 
    {
      if (sgx->text_widget == textL)
	text_field_activated(ss);
      else order_field_activated(ss);
    }
}

static void Undo_and_Apply_Enved_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  /* undo upto previous amp env, then apply */
  /* this blindly undoes the previous edit (assumed to be an envelope) -- if the user made some other change in the meantime, too bad */
  snd_state *ss = (snd_state *)clientData;
  if ((active_channel) && (active_channel == current_channel((void *)ss)))
    {
      undo_EDIT((void *)ss,1);
    }
  apply_enved(ss);
}

static void Undo_and_Apply_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Undo and Apply",
"This button first tries to undo a previous apply\n\
then calls apply, the point obviously being to make\n\
it easy to retry an envelope after some minor change.\n\
");
}

static void reflect_segment_state (snd_state *ss)
{
  if (enved_dialog)
    {
      if (!(ss->using_schemes))
	{
	  XmChangeColor(expB,(enved_exping(ss)) ? ((Pixel)(ss->sgx)->yellow) : ((Pixel)(ss->sgx)->highlight_color));
	  XmChangeColor(linB,(enved_exping(ss)) ? ((Pixel)(ss->sgx)->highlight_color) : ((Pixel)(ss->sgx)->yellow));
	}
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
	  set_button_label_normal(showB,STR_view_envs);
	}
      if (active_env) active_env = free_env(active_env);
      active_env = copy_env(enved_all_envs(pos));
      XmTextSetString(textL,enved_all_names(pos));
      set_enved_env_list_top(0);
      do_env_edit(active_env,TRUE);
      set_sensitive(undoB,FALSE);
      set_sensitive(revertB,FALSE);
      set_sensitive(saveB,FALSE);
      set_enved_exping(ss,(active_env->base != 1.0));
      set_enved_base(ss,active_env->base);
      env_redisplay(ss);
    }
  else
    {
      selected_env = enved_all_envs(pos);
      if (showing_all_envs) view_envs(ss,env_window_width,env_window_height);
    }
}

static void clear_point_label(void)
{
  XtVaSetValues(brkptL,XmNlabelType,XmSTRING,XmNlabelString,NULL,NULL);
}

static char brkpt_buf[32];

void enved_display_point_label(snd_state *ss, Float x, Float y)
{
  if ((enved_dBing(ss)) && (ss->min_dB < -60))
    sprintf(brkpt_buf,"%.3f : %.5f",x,y);
  else sprintf(brkpt_buf,"%.3f : %.3f",x,y);
  set_button_label_normal(brkptL,brkpt_buf);
}

void display_enved_progress(snd_state *ss, char *str, Pixmap pix)
{
  if (pix == 0)
    set_button_label_normal(brkptL,str);
  else XtVaSetValues(brkptL,XmNlabelType,XmPIXMAP,XmNlabelPixmap,pix,NULL);
  check_for_event(ss);
}

static Time down_time;
static int env_dragged = 0;
static int env_pos = 0;
static int click_to_delete = 0;

static void drawer_button_motion(Widget w,XtPointer clientData,XEvent *event,Boolean *cont) 
{
  snd_state *ss = (snd_state *)clientData;
  XMotionEvent *ev = (XMotionEvent *)event;
  Time motion_time;
  axis_info *ap;
  Float x0,x1,x,y;
  if (!showing_all_envs)
    {
      motion_time = ev->time;
      if ((motion_time - down_time) < (0.5 * XtGetMultiClickTime(XtDisplay(w)))) return;
      env_dragged = 1;
      click_to_delete = 0;
      ap = axis_cp->axis;
      x = ungrf_x(ap,ev->x);
      if (env_pos > 0) x0 = active_env->data[env_pos*2-2]; else x0 = 0.0;
      if (env_pos < active_env->pts) x1 = active_env->data[env_pos*2+2]; else x1 = 1.0;
      if (x<x0) x=x0;
      if (x>x1) x=x1;
      if (env_pos == 0) x = active_env->data[0];
      if (env_pos == (active_env->pts-1)) x = active_env->data[(active_env->pts-1)*2];
      y = ungrf_y(ap,ev->y);
      if ((enved_clipping(ss)) || (enved_dBing(ss)))
	{
	  if (y<ap->y0) y=ap->y0;
	  if (y>ap->y1) y=ap->y1;
	}
      if (enved_dBing(ss)) y=un_dB(ss,y);
      move_point(active_env,env_pos,x,y);
      enved_display_point_label(ss,x,y);
      env_redisplay(ss);
    }
}

static void drawer_button_press(Widget w,XtPointer clientData,XEvent *event,Boolean *cont) 
{
  XButtonEvent *ev = (XButtonEvent *)event;
  snd_state *ss = (snd_state *)clientData;
  int pos;
  down_time = ev->time;
  env_dragged = 0;
  if (showing_all_envs)
    {
      pos = hit_env(ev->x,ev->y,env_window_width,env_window_height);
      XmListSelectPos(screnvlst,pos+1,FALSE);
      if ((pos >= 0) && (pos < enved_all_envs_top())) select_or_edit_env((snd_state *)clientData,pos);
    }
  else
    {
      if (!active_env)
	{
	  active_env = default_env(0.0);
	  active_env->base = 1.0;
	  env_redisplay(ss); /* needed to get current_xs set up correctly */
	}
      env_pos = enved_button_press_display(ss,axis_cp->axis,active_env,ev->x,ev->y);
    }
}

void set_enved_click_to_delete(int n) {click_to_delete = n;}

static void drawer_button_release(Widget w,XtPointer clientData,XEvent *event,Boolean *cont) 
{
  if (!showing_all_envs)
    {
      if ((click_to_delete) && (!env_dragged) && (env_pos != 0)) /* might want to protect last point also */
	{
	  delete_point(active_env,env_pos);
	}
      do_env_edit(active_env,FALSE);
      env_pos = 0;
      env_dragged = 0;
      click_to_delete = 0;
      env_redisplay((snd_state *)clientData);
      clear_point_label();
    }
}

static void drawer_resize(Widget w,XtPointer clientData,XtPointer callData) 
{
  /* update display, can be either view of all envs or sequence of current envs */
  env_window_width = widget_width(w);
  env_window_height = widget_height(w);
  env_redisplay((snd_state *)clientData);
}

static void Drawer_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Envelope Editor Display",
"This portion of the envelope editor displays either\n\
the envelope currently being edited, or all currently\n\
loaded envelopes.  In the latter case, click an envelope\n\
to select it, click the selected envelope to load it into\n\
the editor portion.  In the former case, click anywhere in\n\
the graph to place a new breakpoint at that point; click\n\
an existing point to delete it; drag an existing point to\n\
move it.  Put some envelope name in the text field and click\n\
the 'save' button to save the current envelope under the\n\
chosen name.  Unlimited undo/redo are available through the\n\
'undo' and 'redo' buttons.  Set the 'w' button to see the\n\
currently active sound under the envelope; set the 'f' button\n\
to apply the envelope to the sound's frequency, rather than\n\
amplitude; set the 'c' button to clip mouse motion to the current\n\
y axis bounds.\n\
");
}

static void Text_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Envelope Name",
"To load an exisiting envelope into the editor,\n\
type its name in this field; to give a name to\n\
the envelope as it is currently defined in the\n\
graph viewer, type its name in this field, then\n\
either push return or the 'save' button; to define\n\
a new envelope by its break point values, give the\n\
values in this field as though it were a defvar\n\
call in M-X -- that is, '(0 0 1 1)<cr> in this\n\
field fires up a ramp as a new envelope.\n\
");
}

static void Scrolled_List_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Loaded Envelope Names",
"This is the list of all currently loaded envelopes\n\
by name.  When a new envelope is saved, its name is\n\
added to the end of the list.  Click a name to select\n\
that envelope; click the selected envelope to load it\n\
into the editor.  The selected envelope is displayed in\n\
red.\n\
");
}

static void Brkpt_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Breakpoint Data",
"This portion ofthe envelope editor displays the\n\
current breakpoint values while the mouse is dragging\n\
a point\n\
");
}

static void show_button_pressed(Widget w,XtPointer clientData,XtPointer callData) 
{
  /* if show all (as opposed to show current), loop through loaded LV_LISTs */
  showing_all_envs = (!showing_all_envs);
  set_button_label_normal(showB,(showing_all_envs) ? STR_edit_env : STR_view_envs);
  env_redisplay((snd_state *)clientData);
}

static void show_button_help_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "View Envs or Edit Env",
"There are two sides to the Envelope Editor, the\n\
editor itself, and an envelope viewer.  This button\n\
chooses which is active.  In the envelope viewer,\n\
all currently loaded envelopes are displayed, and\n\
you can select one by clicking it; click the selected\n\
envelope to load it into the editor portion.  The\n\
other choice is the editor which displays the current\n\
state of the envelope being edited.\n\
");
}

static void selection_button_pressed(Widget s, XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  apply_to_selection = (!apply_to_selection);
  if (apply_to_selection) 
    {
      if (apply_to_mix) XmChangeColor(mixB,((Pixel)(ss->sgx)->highlight_color));
      apply_to_mix = 0;
    }
  XmChangeColor(selectionB,(apply_to_selection) ? ((Pixel)(ss->sgx)->yellow) : ((Pixel)(ss->sgx)->highlight_color));
  if ((enved_target(ss) != SPECTRUM_ENV) && (enved_waving(ss)) && (!showing_all_envs)) env_redisplay(ss);
}

static void mix_button_pressed(Widget s, XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)callData;
  XButtonEvent *ev;
  int chan = 0;
  mixdata *md = NULL;
  ev = (XButtonEvent *)(cb->event);
  apply_to_mix = (!apply_to_mix);
  if (apply_to_mix) 
    {
      if (apply_to_selection) XmChangeColor(selectionB,((Pixel)(ss->sgx)->highlight_color));
      apply_to_selection = 0;
      if (ev->state & snd_ControlMask) 
	{
	  if (ss->selected_mix != NO_SELECTION) 
	    md = md_from_int(ss->selected_mix); 
	  else
	    {
	      md = md_from_int(any_mix_id());
	      if (md) select_mix(ss,md);
	    }
	  if (md)
	    {
	      if (md->selected_chan != NO_SELECTION) chan = md->selected_chan;
	      if (mix_amp_env(md->id,chan))
		{
		  if (active_env) active_env = free_env(active_env);
		  active_env = copy_env(mix_amp_env(md->id,chan));
		  set_enved_env_list_top(0);
		  do_env_edit(active_env,TRUE);
		  set_sensitive(undoB,FALSE);
		  set_sensitive(revertB,FALSE);
		  set_sensitive(saveB,FALSE);
		  env_redisplay(ss);
		}
	    }
	}
    }
  XmChangeColor(mixB,(apply_to_mix) ? ((Pixel)(ss->sgx)->yellow) : ((Pixel)(ss->sgx)->highlight_color));
  if ((enved_target(ss) == AMPLITUDE_ENV) && (enved_waving(ss)) && (!showing_all_envs)) env_redisplay(ss);
}

static void selection_button_help_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Apply to Selection or Sound",
"The 'Apply' button can affect either the current selection,\n\
the current sound, or the currently selected mix.\n\
");
}

static void mix_button_help_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Apply to Mix",
"The 'Apply' button can affect either the currently selected\n\
mix, or the entire current sound, or the current selection.\n\
");
}

static void revert_button_pressed(Widget w,XtPointer clientData,XtPointer callData) 
{
  revert_env_edit();
  if (active_env) active_env = free_env(active_env);
  active_env = enved_next_env();
  env_redisplay((snd_state *)clientData);
}

static void revert_button_help_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Revert to Original Envelope",
"The revert button undoes all the edits back to\n\
the original state, and if clicked again at that\n\
point, clears the editor to a clean state (that\n\
is, the envelope '(0 0 1 0)).\n\
");
}

static void undo_button_pressed(Widget w,XtPointer clientData,XtPointer callData) 
{
  undo_env_edit();
  if (active_env) active_env = free_env(active_env);
  active_env = enved_next_env();
  env_redisplay((snd_state *)clientData);
}

static void undo_button_help_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Undo Edit",
"The undo button undoes the previous edit.  The\n\
list of edits is not cleared until you changed to\n\
a new envelope, so you can undo and redo every edit\n\
without limitation.\n\
");
}

static void redo_button_pressed(Widget w,XtPointer clientData,XtPointer callData) 
{
  redo_env_edit();
  if (active_env) active_env = free_env(active_env);
  active_env = enved_next_env();
  env_redisplay((snd_state *)clientData);
}

static void redo_button_help_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Redo Edit",
"The redo button redoes an edit that was previously\n\
undone.  There is no limit on the number of edits\n\
all_that can be undone and redone.\n\
");
}

static void reflect_apply_state (snd_state *ss)
{
  set_label(nameL,env_names[enved_target(ss)]);
  XmChangeColor(ampB,(enved_target(ss) == AMPLITUDE_ENV) ? ((Pixel)(ss->sgx)->green) : ((Pixel)(ss->sgx)->highlight_color));
  XmChangeColor(fltB,(enved_target(ss) == SPECTRUM_ENV) ? ((Pixel)(ss->sgx)->green) : ((Pixel)(ss->sgx)->highlight_color));
  XmChangeColor(srcB,(enved_target(ss) == SRATE_ENV) ? ((Pixel)(ss->sgx)->green) : ((Pixel)(ss->sgx)->highlight_color));
  if ((!showing_all_envs) && (enved_waving(ss))) env_redisplay(ss);
}

static void amp_button_pressed(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  in_set_enved_target(ss,enved_target(ss)+1);
  if (enved_target(ss) > SRATE_ENV) in_set_enved_target(ss,AMPLITUDE_ENV);
  reflect_apply_state(ss);
}

static void amp_button_help_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Env Label",
"If you click the envelope name label, it\n\
is equivalent to pushing the next button\n\
in the sequence amp, flt, src; that is,\n\
it chooses the next in the list of possible\n\
envelope applications.\n\
");
}

static void Freq_Button_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  in_set_enved_target(ss,SPECTRUM_ENV);
  reflect_apply_state(ss);
}

static void Freq_Button_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Apply Envelope to Spectrum",
"When the 'Apply' button is pressed, the envelope\n\
in the editor window is applied to the current sound\n\
and any sounds sync'd to it. If this button is set,\n\
the envelope is interpreted as a frequency response\n\
curve, and used to design an FIR filter that is then\n\
applied to the current sound. The order of the filter\n\
is determined by the variable " S_filter_env_order "\n\
which defaults to 40.\n\
");
}

static void Amp_Button_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  in_set_enved_target(ss,AMPLITUDE_ENV);
  reflect_apply_state(ss);
}

static void Amp_Button_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Apply Envelope to Amplitude",
"When the 'Apply' button is pressed, the envelope\n\
in the editor window is applied to the current sound\n\
and any sounds sync'd to it. If this button is set,\n\
the envelope is interpreted as a amplitude envelope.\n\
");
}

static void Src_Button_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  in_set_enved_target(ss,SRATE_ENV);
  reflect_apply_state(ss);
}

static void Src_Button_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Apply Envelope to Srate",
"When the 'Apply' button is pressed, the envelope\n\
in the editor window is applied to the current sound\n\
and any sounds sync'd to it. If this button is set,\n\
the envelope is applied to the sampling rate.\n\
");
}

void enved_print(snd_state *ss, char *name)
{
  print_enved(name,axis_cp,env_window_height);
}

static void print_button_pressed(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  ss->print_choice = PRINT_ENV;
  File_Print_Callback(w,clientData,callData);
}

static void print_button_help_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Print Envelopes",
"The print button causes the current envelope editor\n\
display (whether the envelope being edited, or all the\n\
envelopes currently loaded) to be saved in a postscript\n\
file named aaa.eps.  You can send this to a printer with\n\
lpr.\n\
");
}

static void env_browse_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  XmListCallbackStruct *cb = (XmListCallbackStruct *)callData;
  select_or_edit_env((snd_state *)clientData,cb->item_position - 1);
}

static void Graph_Button_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)callData;
  snd_state *ss = (snd_state *)clientData; 
  in_set_enved_waving(ss,cb->set);
  env_redisplay(ss);
}

static void Graph_Button_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Show Current Active Channel",
"This button, if set, causes a light-colored version\n\
of the current channel's overall data to be displayed\n\
in the envelope editor window, making it easier to\n\
design an envelope to fit the exact contours of the\n\
sound.  Since the envelope is always applied to the\n\
entire sound, the x-axis bounds in the envelope editor\n\
are arbitrary.\n\
");
}

static void dB_Button_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)callData;
  snd_state *ss = (snd_state *)clientData; 
  in_set_enved_dBing(ss,cb->set);
  env_redisplay(ss);
}

static void dB_Button_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Linear or Log Y Axis",
"This button, if set, causes the y axis to be in dB\n\
");
}

static void Clip_Button_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData; 
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)callData; 
  in_set_enved_clipping(ss,cb->set);
}

static void Clip_Button_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Y Axis Clipped",
"This button, if set, causes break point motion\n\
in the y direction to be clipped at the current\n\
axis bounds.  If unset, the bounds will try to\n\
change to accomodate the current mouse position.\n\
");
}

static void Exp_Button_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData; 
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)callData; 
  in_set_enved_exping(ss,cb->set);
  if ((active_env) && (!(showing_all_envs)))
    {
      if (enved_exping(ss))
	active_env->base = enved_base(ss);
      else active_env->base = 1.0;
      set_sensitive(saveB,TRUE);
    }
  reflect_segment_state(ss);
}

static void Exp_Button_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Linear or Exponential Segments",
"This button, if set, causes the segments to be\n\
drawn with an exponential curve\n\
");
}

static void Lin_Button_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData; 
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)callData; 
  in_set_enved_exping(ss,(!(cb->set)));
  if ((active_env) && (!(showing_all_envs)))
    {
      if (enved_exping(ss))
	active_env->base = enved_base(ss);
      else active_env->base = 1.0;
      set_sensitive(saveB,TRUE);
    }
  reflect_segment_state(ss);
}

static void Lin_Button_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Linear or Exponential Segments",
"This button, if set, causes the segments to be\n\
drawn with an straight line.\n\
");
}

static void Base_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Exponential Envelope Base",
"This slider sets the base of the exponential used to\n\
connect the envelope breakpoints.\n\
");
}

static void Order_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Filter Order",
"This text field sets the order of the FIR filter used by\n\
the 'flt' envelope.\n\
");
}

#define BASE_MAX 400
#define BASE_MID 200
/* these two just set the granularity of the scale widget, not the user-visible bounds */

static void make_base_label(snd_state *ss, Float bval)
{
  char *sfs,*buf;
  int i,len,scale_len;
  len = (int)(enved_power(ss) * 4);
  if (len < 32) len = 32;
  sfs = (char *)CALLOC(len,sizeof(char));
  sprintf(sfs,"%f",bval);
  scale_len = (int)(enved_power(ss) + 3);
  buf = (char *)CALLOC(scale_len,sizeof(char));
  for (i=0;i<scale_len-1;i++) buf[i] = sfs[i];
  set_button_label(baseValue,buf);
  FREE(sfs);
  FREE(buf);
  in_set_enved_base(ss,bval);
  if ((active_env) && (!(showing_all_envs))) 
    {
      active_env->base = enved_base(ss);
      if (enved_exping(ss)) env_redisplay(ss);
    }
}

static void base_changed(snd_state *ss, int val)
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
	  if (val>BASE_MID)
	    bval = pow(1.0 + (10.0 * ((Float)(val - BASE_MID)/(Float)BASE_MID)),enved_power(ss));  
	  else 
	    bval = pow(((Float)val / (Float)BASE_MID),enved_power(ss) - 1.0);
	}
    }
  make_base_label(ss,bval);
  if ((active_env) && (enved_exping(ss))) set_sensitive(saveB,TRUE); /* what about undo/redo here? */
}

static void reflect_changed_base(snd_state *ss, Float val)
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
	    ival = (int)(pow(val,1.0/(enved_power(ss)-1.0))*BASE_MID);
	  else ival = (int)(BASE_MID + ((BASE_MID * (pow(val,(1.0/(enved_power(ss)))) - 1)) / 10.0));
	}
    }
  XtVaSetValues(baseScale,XmNvalue,ival,NULL);
  make_base_label(ss,val);
}

static void Base_Drag_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  XmScrollBarCallbackStruct *sb = (XmScrollBarCallbackStruct *)callData;
  base_changed(ss,sb->value);
}

static int base_last_value = BASE_MID;

static void Base_ValueChanged_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  XmScrollBarCallbackStruct *sb = (XmScrollBarCallbackStruct *)callData;
  base_last_value = sb->value;
  base_changed(ss,sb->value);
}

static void Base_Click_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)callData;
  snd_state *ss = (snd_state *)clientData;
  XButtonEvent *ev;
  int val;
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask)) val = base_last_value; else val = BASE_MID;
  base_changed(ss,val);
  XtVaSetValues(baseScale,XmNvalue,val,NULL);
}

void create_envelope_editor (snd_state *ss)
{
  int n;
  Arg args[32];
  Widget spacer,spacer1,aform;
  XmString xhelp,xdismiss,xapply,titlestr,s1;
  XGCValues gv;
  char str[8];

  if (!enved_dialog)
    {

      /* -------- DIALOG -------- */
      xdismiss = XmStringCreate(STR_Dismiss,XmFONTLIST_DEFAULT_TAG);
      xhelp = XmStringCreate(STR_Help,XmFONTLIST_DEFAULT_TAG);
      titlestr = XmStringCreate(STR_Edit_Envelope,XmFONTLIST_DEFAULT_TAG);
      xapply = XmStringCreate(STR_Apply,XmFONTLIST_DEFAULT_TAG);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNautoUnmanage,FALSE); n++;
      XtSetArg(args[n],XmNcancelLabelString,xdismiss); n++;
      XtSetArg(args[n],XmNhelpLabelString,xhelp); n++;
      XtSetArg(args[n],XmNokLabelString,xapply); n++;
      XtSetArg(args[n],XmNdialogTitle,titlestr); n++;
      XtSetArg(args[n],XmNresizePolicy,XmRESIZE_GROW); n++;

#if RESIZE_DIALOG
      XtSetArg(args[n],XmNnoResize,FALSE); n++;
#endif
      XtSetArg(args[n],XmNtransient,FALSE); n++;
      enved_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss),STR_envelope_editor,args,n);
      add_dialog(ss,enved_dialog);
#if OVERRIDE_TOGGLE
      override_form_translation(enved_dialog);
#endif
  
      XtAddCallback(enved_dialog,XmNcancelCallback,Dismiss_Enved_Callback,ss);
      XtAddCallback(enved_dialog,XmNhelpCallback,Help_Enved_Callback,ss);
      XtAddCallback(enved_dialog,XmNokCallback,Apply_Enved_Callback,ss);

      XmStringFree(xhelp);
      XmStringFree(xdismiss);
      XmStringFree(titlestr);
      XmStringFree(xapply);

      if (!(ss->using_schemes))
	{
	  XtVaSetValues(XmMessageBoxGetChild(enved_dialog,XmDIALOG_CANCEL_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
	  XtVaSetValues(XmMessageBoxGetChild(enved_dialog,XmDIALOG_HELP_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
	  XtVaSetValues(XmMessageBoxGetChild(enved_dialog,XmDIALOG_OK_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
	}

      n=0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;
	  XtSetArg(args[n],XmNarmColor,(ss->sgx)->pushed_button_color); n++;
	}
      apply2B = XtCreateManagedWidget(STR_Undo_and_Apply,xmPushButtonWidgetClass,enved_dialog,args,n);
      XtAddCallback(apply2B,XmNactivateCallback,Undo_and_Apply_Enved_Callback,ss);
      XtAddCallback(apply2B,XmNhelpCallback,Undo_and_Apply_Help_Callback,ss);

      /* -------- MAIN WIDGET -------- */
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNbottomWidget,XmMessageBoxGetChild(enved_dialog,XmDIALOG_SEPARATOR)); n++;
      mainform = sndCreateFormWidget("formd",enved_dialog,args,n);

      /* the order in which widgets are defined matters a lot here:
       * we need to build from the bottom up so that the graph portion expands
       * when the window is resized (if top-down, the slider at the bottom expands!)
       */

      /* -------- EXP SLIDER AT BOTTOM -------- */
      n=0;      
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNrecomputeSize,FALSE); n++;
      XtSetArg(args[n],XmNshadowThickness,0); n++;
      XtSetArg(args[n],XmNhighlightThickness,0); n++;
      XtSetArg(args[n],XmNfillOnArm,FALSE); n++;
      baseLabel = sndCreatePushButtonWidget (STR_exp_base,mainform,args,n);
      XtAddCallback(baseLabel,XmNhelpCallback,Base_Help_Callback,ss);
      XtAddCallback(baseLabel,XmNactivateCallback,Base_Click_Callback,ss);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      s1 = XmStringCreate("1.000",XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,baseLabel); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNleftWidget,baseLabel); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
      /*      XtSetArg(args[n],XmNrecomputeSize,FALSE); n++; */
      XtSetArg(args[n],XmNlabelString,s1); n++;
      baseValue = XtCreateManagedWidget ("base-label",xmLabelWidgetClass,mainform,args,n);
      XtAddCallback(baseValue,XmNhelpCallback,Base_Help_Callback,ss);
      XmStringFree(s1);

      n=0;      
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNcolumns,3); n++;
      XtSetArg(args[n],XmNrecomputeSize,FALSE); n++;
#ifndef SGI
      XtSetArg(args[n],XmNheight,24); n++;
#endif
      XtSetArg(args[n],XmNresizeWidth,FALSE); n++;
      XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n],XmNmarginHeight,0); n++;
      XtSetArg(args[n],XmNmarginBottom,0); n++;
      sprintf(str,"%d",filter_env_order(ss));
      XtSetArg(args[n],XmNvalue,str); n++;
      orderL = sndCreateTextFieldWidget(ss,"orderL",mainform,args,n,ACTIVATABLE,NO_COMPLETER);
      XtAddCallback(orderL,XmNhelpCallback,Order_Help_Callback,ss);

      n=0;      
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->position_color); n++;}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,baseLabel); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNleftWidget,baseValue); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNrightWidget,orderL); n++;
      XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
      XtSetArg(args[n],XmNmaximum,BASE_MAX); n++;
      XtSetArg(args[n],XmNvalue,BASE_MID); n++;
      XtSetArg(args[n],XmNincrement,1); n++;
      XtSetArg(args[n],XmNpageIncrement,1); n++;
      XtSetArg(args[n],XmNdragCallback,make_callback_list(Base_Drag_Callback,(XtPointer)ss)); n++;
      XtSetArg(args[n],XmNvalueChangedCallback,make_callback_list(Base_ValueChanged_Callback,(XtPointer)ss)); n++;
      baseScale = XtCreateManagedWidget("expscl",xmScrollBarWidgetClass,mainform,args,n);
      XtAddCallback(baseScale,XmNhelpCallback,Base_Help_Callback,ss);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNbottomWidget,baseScale); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNmargin,LINE_MARGIN); n++;
      XtSetArg(args[n],XmNheight,LINE_MARGIN); n++;
      XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
      XtSetArg(args[n],XmNseparatorType,XmNO_LINE); n++;
      XtSetArg(args[n],XmNheight,5); n++;
      baseSep = XtCreateManagedWidget("snd-rec-sep",xmSeparatorWidgetClass,mainform,args,n);

      /* -------- AMP ENV NAME -------- */
      n=0;
      s1=XmStringCreate(STR_amp_env_p,XmFONTLIST_DEFAULT_TAG);
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNbottomWidget,baseSep); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNlabelString,s1); n++;
      XtSetArg(args[n],XmNshadowThickness,0); n++;
      XtSetArg(args[n],XmNhighlightThickness,0); n++;
      XtSetArg(args[n],XmNfillOnArm,FALSE); n++;
      nameL = sndCreatePushButtonWidget("nameL",mainform,args,n);
      XtAddCallback(nameL,XmNactivateCallback,amp_button_pressed,ss);
      XtAddCallback(nameL,XmNhelpCallback,amp_button_help_callback,ss);
      XmStringFree(s1);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNbottomWidget,baseSep); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNleftWidget,nameL); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
      textL = sndCreateTextFieldWidget(ss,"textL",mainform,args,n,ACTIVATABLE,NO_COMPLETER);
      XtAddCallback(textL,XmNhelpCallback,Text_Help_Callback,ss);

      /* -------- dB, GRAPH ('wave') AND CLIP BUTTONS -------- */
      n=0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;
	  XtSetArg(args[n],XmNselectColor,(ss->sgx)->pushed_button_color); n++;
	}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNbottomWidget,baseSep); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n],XmNindicatorSize,ss->toggle_size); n++;}
      dBB = sndCreateToggleButtonWidget(STR_dB,mainform,args,n);
      XtAddCallback(dBB,XmNvalueChangedCallback,dB_Button_Callback,ss);
      XtAddCallback(dBB,XmNhelpCallback,dB_Button_Help_Callback,ss);

      n=0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;
	  XtSetArg(args[n],XmNselectColor,(ss->sgx)->pushed_button_color); n++;
	}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNbottomWidget,baseSep); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNrightWidget,dBB); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n],XmNindicatorSize,ss->toggle_size); n++;}
      graphB = sndCreateToggleButtonWidget(STR_wave,mainform,args,n);
      XtAddCallback(graphB,XmNvalueChangedCallback,Graph_Button_Callback,ss);
      XtAddCallback(graphB,XmNhelpCallback,Graph_Button_Help_Callback,ss);

      n=0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;
	  XtSetArg(args[n],XmNselectColor,(ss->sgx)->pushed_button_color); n++;
	}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNbottomWidget,baseSep); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNrightWidget,graphB); n++;
      if (ss->toggle_size > 0) {XtSetArg(args[n],XmNindicatorSize,ss->toggle_size); n++;}
      clipB = sndCreateToggleButtonWidget(STR_clip,mainform,args,n);
      XtAddCallback(clipB,XmNvalueChangedCallback,Clip_Button_Callback,ss);
      XtAddCallback(clipB,XmNhelpCallback,Clip_Button_Help_Callback,ss);

      /* -------- BREAKPOINT DATA DISPLAY LABEL -------- */
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNbottomWidget,baseSep); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNleftWidget,textL); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNrightWidget,clipB); n++;
      XtSetArg(args[n],XmNrecomputeSize,FALSE); n++;
      XtSetArg(args[n],XM_FONT_RESOURCE,BUTTON_FONT(ss)); n++;
      XtSetArg(args[n],XmNlabelType,XmSTRING); n++;
      brkptL = XtCreateManagedWidget("         ",xmLabelWidgetClass,mainform,args,n);
      XtAddCallback(brkptL,XmNhelpCallback,Brkpt_Help_Callback,ss);

      /* -------- SPACERS TO DIVIDE WINDOW IN TWO -------- */
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNbottomWidget,textL); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
      XtSetArg(args[n],XmNheight,4); n++;
      XtSetArg(args[n],XmNseparatorType,XmNO_LINE); n++;
      spacer = XtCreateManagedWidget("spacer",xmSeparatorWidgetClass,mainform,args,n);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNbottomWidget,spacer); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
      XtSetArg(args[n],XmNseparatorType,XmSHADOW_ETCHED_IN); n++;
      spacer1 = XtCreateManagedWidget("spacer1",xmSeparatorWidgetClass,mainform,args,n);
      /* second separator needed because marginTop seems to be broken or non-existent for these widgets */

      /* -------- WINDOW LEFT WIDGET HOLDER -------- */
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNbottomWidget,spacer1); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
      aform = sndCreateFormWidget("aform",mainform,args,n);

      /* -------- BUTTON BOX AT TOP LEFT -------- */
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->zoom_color); n++;}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNshadowThickness,4); n++;
      colF = sndCreateFrameWidget("env-button-frame",aform,args,n);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
      XtSetArg(args[n],XmNspacing,0); n++;
      colB = sndCreateRowColumnWidget("env-button-holder",colF,args,n);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
      XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
      XtSetArg(args[n],XmNspacing,0); n++;
      XtSetArg(args[n],XmNmarginWidth,0); n++;
      selrow = sndCreateRowColumnWidget("selmix-holder",colB,args,n);  

      n=0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;
	  XtSetArg(args[n],XM_FONT_RESOURCE,BUTTON_FONT(ss)); n++;
	  XtSetArg(args[n],XmNarmColor,(ss->sgx)->pushed_button_color); n++;
	}
      XtSetArg(args[n],XmNshadowThickness,1); n++;
      selectionB = sndCreatePushButtonWidget("selection",selrow,args,n);
      mixB = sndCreatePushButtonWidget("mix",selrow,args,n);

      XtAddCallback(selectionB,XmNactivateCallback,selection_button_pressed,ss);
      XtAddCallback(selectionB,XmNhelpCallback,selection_button_help_callback,ss);
      XtAddCallback(mixB,XmNactivateCallback,mix_button_pressed,ss);
      XtAddCallback(mixB,XmNhelpCallback,mix_button_help_callback,ss);

      n=0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;
	  XtSetArg(args[n],XM_FONT_RESOURCE,BUTTON_FONT(ss)); n++;
	  XtSetArg(args[n],XmNarmColor,(ss->sgx)->pushed_button_color); n++;
	}
      showB = XtCreateManagedWidget(STR_view_envs,xmPushButtonWidgetClass,colB,args,n);
      saveB = XtCreateManagedWidget(STR_save,xmPushButtonWidgetClass,colB,args,n);
      revertB = XtCreateManagedWidget(STR_revert,xmPushButtonWidgetClass,colB,args,n);
      undoB = XtCreateManagedWidget(STR_undo,xmPushButtonWidgetClass,colB,args,n);
      redoB = XtCreateManagedWidget(STR_redo,xmPushButtonWidgetClass,colB,args,n);
      printB = XtCreateManagedWidget(STR_print,xmPushButtonWidgetClass,colB,args,n);

      XtAddCallback(showB,XmNactivateCallback,show_button_pressed,ss);
      XtAddCallback(saveB,XmNactivateCallback,save_button_pressed,ss);
      XtAddCallback(revertB,XmNactivateCallback,revert_button_pressed,ss);
      XtAddCallback(undoB,XmNactivateCallback,undo_button_pressed,ss);
      XtAddCallback(redoB,XmNactivateCallback,redo_button_pressed,ss);
      XtAddCallback(printB,XmNactivateCallback,print_button_pressed,ss);

      XtAddCallback(showB,XmNhelpCallback,show_button_help_callback,ss);
      XtAddCallback(saveB,XmNhelpCallback,save_button_help_callback,ss);
      XtAddCallback(revertB,XmNhelpCallback,revert_button_help_callback,ss);
      XtAddCallback(undoB,XmNhelpCallback,undo_button_help_callback,ss);
      XtAddCallback(redoB,XmNhelpCallback,redo_button_help_callback,ss);
      XtAddCallback(printB,XmNhelpCallback,print_button_help_callback,ss);

      /* enved_function (target) choice (a row of three push buttons that acts like a "radio box") */
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
      XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
      XtSetArg(args[n],XmNspacing,0); n++;
      XtSetArg(args[n],XmNmarginWidth,0); n++;
      rbrow = sndCreateRowColumnWidget("asf-holder",colB,args,n);  

      n=0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;
	  XtSetArg(args[n],XM_FONT_RESOURCE,BUTTON_FONT(ss)); n++;
	  XtSetArg(args[n],XmNarmColor,(ss->sgx)->pushed_button_color); n++;
	}
      /* XtSetArg(args[n],XmNmarginWidth,0); n++; */
      XtSetArg(args[n],XmNshadowThickness,1); n++;
      ampB = XtCreateManagedWidget(STR_amp,xmPushButtonWidgetClass,rbrow,args,n);
      fltB = XtCreateManagedWidget(STR_flt,xmPushButtonWidgetClass,rbrow,args,n);
      srcB = XtCreateManagedWidget(STR_src,xmPushButtonWidgetClass,rbrow,args,n);

      XtAddCallback(fltB,XmNactivateCallback,Freq_Button_Callback,ss);
      XtAddCallback(fltB,XmNhelpCallback,Freq_Button_Help_Callback,ss);
      XtAddCallback(ampB,XmNactivateCallback,Amp_Button_Callback,ss);
      XtAddCallback(ampB,XmNhelpCallback,Amp_Button_Help_Callback,ss);
      XtAddCallback(srcB,XmNactivateCallback,Src_Button_Callback,ss);
      XtAddCallback(srcB,XmNhelpCallback,Src_Button_Help_Callback,ss);

      /* similar secondary box for linear/exp buttons */
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      lerow = sndCreateFormWidget("le-holder",colB,args,n);  

      n=0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;
	  XtSetArg(args[n],XM_FONT_RESOURCE,BUTTON_FONT(ss)); n++;
	  XtSetArg(args[n],XmNarmColor,(ss->sgx)->yellow); n++;
	}
      /* XtSetArg(args[n],XmNmarginWidth,0); n++; */
      XtSetArg(args[n],XmNshadowThickness,1); n++;
      XtSetArg(args[n],XmNalignment,XmALIGNMENT_CENTER); n++;	
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_POSITION); n++;
      XtSetArg(args[n],XmNrightPosition,50); n++;
      linB = XtCreateManagedWidget(STR_linear,xmPushButtonWidgetClass,lerow,args,n);
      n-=3;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNleftWidget,linB); n++;
      expB = XtCreateManagedWidget(STR_exp,xmPushButtonWidgetClass,lerow,args,n);

      XtAddCallback(linB,XmNactivateCallback,Lin_Button_Callback,ss);
      XtAddCallback(linB,XmNhelpCallback,Lin_Button_Help_Callback,ss);
      XtAddCallback(expB,XmNactivateCallback,Exp_Button_Callback,ss);
      XtAddCallback(expB,XmNhelpCallback,Exp_Button_Help_Callback,ss);


      /* -------- ENV LIST AT LEFT UNDER BUTTONS -------- */
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,colF); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      screnvname = XtCreateManagedWidget(STR_envs_p,xmLabelWidgetClass,aform,args,n);
      XtAddCallback(screnvname,XmNhelpCallback,Scrolled_List_Help_Callback,ss);

      n=0;
#ifdef LESSTIF_VERSION
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->white); n++;}
#else
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
#endif
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,screnvname); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XM_FONT_RESOURCE,BUTTON_FONT(ss)); n++;
      screnvlst = XmCreateScrolledList(aform,"scrolled-env-list",args,n);
      XtManageChild(screnvlst); 
      XtAddCallback(screnvlst,XmNbrowseSelectionCallback,env_browse_Callback,ss);
      map_over_children(screnvlst,set_main_color_of_widget,(void *)ss);
      if (enved_all_envs_top() > 0) make_scrolled_env_list(ss);
      XtAddCallback(screnvlst,XmNhelpCallback,Scrolled_List_Help_Callback,ss);

      /* -------- MAIN GRAPH -------- */

      n=0;
      if (!(ss->using_schemes))	{XtSetArg(args[n],XmNbackground,(ss->sgx)->graph_color); n++;}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNbottomWidget,spacer1 /* textL */); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNleftWidget,aform); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNheight,350); n++;
      XtSetArg(args[n],XmNallowResize,TRUE); n++;
      drawer = sndCreateDrawingAreaWidget("drawer",mainform,args,n);
      XtAddCallback(drawer,XmNhelpCallback,Drawer_Help_Callback,ss);

      gv.function = GXcopy;
      XtVaGetValues(drawer,XmNbackground, &gv.background,XmNforeground, &gv.foreground,NULL);
      gc = XtGetGC(drawer, GCForeground | GCFunction, &gv);
      gv.foreground = (ss->sgx)->red;
      rgc = XtGetGC(drawer, GCBackground | GCForeground | GCFunction, &gv);
      gv.foreground = (ss->sgx)->enved_waveform_color;
      ggc = XtGetGC(drawer, GCBackground | GCForeground | GCFunction, &gv);

      XtManageChild(enved_dialog); /* needed so that window is valid when resize callback is invoked */
      applyB = XmMessageBoxGetChild(enved_dialog,XmDIALOG_OK_BUTTON);
      cancelB = XmMessageBoxGetChild(enved_dialog,XmDIALOG_CANCEL_BUTTON);

      XtAddCallback(drawer,XmNresizeCallback,drawer_resize,ss);
      XtAddCallback(drawer,XmNexposeCallback,drawer_resize,ss);

      XtAddEventHandler(drawer,ButtonPressMask,FALSE,drawer_button_press,ss);
      XtAddEventHandler(drawer,ButtonMotionMask,FALSE,drawer_button_motion,ss);
      XtAddEventHandler(drawer,ButtonReleaseMask,FALSE,drawer_button_release,ss);

      if (enved_all_envs_top() == 0)
	{
	  set_sensitive(showB,FALSE);
	}
      set_sensitive(revertB,FALSE);
      set_sensitive(undoB,FALSE);
      set_sensitive(redoB,FALSE);
      set_sensitive(saveB,FALSE);
      if (!(selection_is_current())) set_sensitive(selectionB,FALSE);

      XmToggleButtonSetState(clipB,enved_clipping(ss),FALSE);
      XmToggleButtonSetState(graphB,enved_waving(ss),FALSE);
      XmToggleButtonSetState(dBB,enved_dBing(ss),FALSE);

      reflect_apply_state(ss);
      reflect_segment_state(ss);
    }
  else raise_dialog(enved_dialog);
  if (!XtIsManaged(enved_dialog)) XtManageChild(enved_dialog);
  active_channel = current_channel(ss);
  set_sensitive(mixB,(mixes() > 0));
}

void set_enved_clipping(snd_state *ss, int val) {in_set_enved_clipping(ss,val); if (enved_dialog) XmToggleButtonSetState(clipB,val,FALSE);}
void set_enved_exping(snd_state *ss, int val) {in_set_enved_exping(ss,val); if (enved_dialog) XmToggleButtonSetState(expB,val,FALSE); reflect_segment_state(ss);}
void set_enved_target(snd_state *ss, int val) {in_set_enved_target(ss,val); if (enved_dialog) reflect_apply_state(ss);}
void set_enved_waving(snd_state *ss, int val) {in_set_enved_waving(ss,val); if (enved_dialog) XmToggleButtonSetState(graphB,val,FALSE);}
void set_enved_dBing(snd_state *ss, int val) {in_set_enved_dBing(ss,val); if (enved_dialog) XmToggleButtonSetState(dBB,val,FALSE);}
void set_enved_base(snd_state *ss, Float val) {in_set_enved_base(ss,val); if (enved_dialog) reflect_changed_base(ss,val);}

int enved_dialog_is_active(void)
{
  return((enved_dialog) && (XtIsManaged(enved_dialog)));
}

void set_filter_env_order(snd_state *ss, int order)
{
  char str[8];
  if ((order>0) && (order<2000))
    {
      if (order&1) 
	in_set_filter_env_order(ss,order+1);
      else in_set_filter_env_order(ss,order);
      if (enved_dialog_is_active())
	{
	  sprintf(str,"%d",filter_env_order(ss));
	  XmTextFieldSetString(orderL,str);
	  if ((enved_target(ss) == SPECTRUM_ENV) && (enved_waving(ss)) && (!showing_all_envs)) env_redisplay(ss);
	}
    }
}

void enved_reflect_selection(int on)
{
  snd_state *ss;
  if (enved_dialog_is_active())
    {
      ss = get_global_state();
      set_sensitive(selectionB,on);
      if ((apply_to_selection) && (!on))
	{
	  apply_to_selection = 0;
	  XmChangeColor(selectionB,(Pixel)(ss->sgx)->highlight_color);
	}
      if ((enved_target(ss) != SPECTRUM_ENV) && (enved_waving(ss)) && (!showing_all_envs)) env_redisplay(ss);
    }
}

void color_enved_waveform(Pixel pix)
{
  snd_state *ss;
  ss = get_global_state();
  (ss->sgx)->enved_waveform_color = pix;
  if (enved_dialog)
    {
      XSetForeground(MAIN_DISPLAY(ss),ggc,pix);
      if ((enved_waving(ss)) && (enved_dialog_is_active())) env_redisplay(ss);
    }
}

void reflect_mix_in_enved(void)
{
  if (enved_dialog_is_active())
    set_sensitive(mixB,TRUE);
}
