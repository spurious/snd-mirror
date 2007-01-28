#include "snd.h"

/* -------- region browser -------- */

typedef struct {
  Widget rw, nm, pl;
  int pos;
} regrow;

static Widget region_dialog = NULL, region_list, region_grf;
static regrow **region_rows = NULL;
static int region_rows_size = 0;
static snd_info *rsp = NULL;
static int current_region = -1;
static Widget reg_srtxt, reg_lentxt, reg_chntxt, reg_maxtxt;
static Widget region_ww = NULL;
static Widget mix_button = NULL, save_as_button = NULL, insert_button = NULL;
static regrow *region_row(int n);

static void set_current_region(int rg)
{
  bool reg_ok = false;
  current_region = rg;
  reflect_region_in_save_as_dialog();
  if (rg >= 0)
    reg_ok = region_ok(region_list_position_to_id(rg));
  if (save_as_button) XtSetSensitive(save_as_button, reg_ok);
  if (mix_button) XtSetSensitive(mix_button, reg_ok);
  if (insert_button) XtSetSensitive(insert_button, reg_ok);
}

void reflect_regions_in_region_browser(void)
{
  if (rsp)
    {
      int i;
      rsp->active = true;
      if (rsp->chans)
	for (i = 0; i < rsp->nchans; i++)
	  rsp->chans[i]->active = true;
    }
}

void reflect_no_regions_in_region_browser(void)
{
  if (rsp)
    {
      int i;
      rsp->active = false;
      if (rsp->chans)
	for (i = 0; i < rsp->nchans; i++)
	  rsp->chans[i]->active = false;
    }
}

static void region_update_graph(chan_info *cp)
{
  if (current_region == -1) return;
  rsp->nchans = region_chans(region_list_position_to_id(current_region));
  if (rsp->nchans == 0) return;
  update_graph(cp);
  rsp->nchans = 1;
}

void reflect_region_graph_style(void)
{
  if (current_region == -1) return;
  if ((rsp) &&
      (rsp->chans) &&
      (rsp->chans[0]) &&
      (region_dialog_is_active()))
    {
      rsp->chans[0]->time_graph_style = region_graph_style(ss);
      rsp->chans[0]->dot_size = dot_size(ss);
      /* update_graph(rsp->chans[0]); */
      update_region_browser(true);
    }
}

static void unhighlight_region(void)
{
  if (current_region != -1)
    {
      regrow *oldr;
      oldr = region_row(current_region);
      XtVaSetValues(oldr->rw, XmNbackground, ss->sgx->highlight_color, NULL);
      XtVaSetValues(oldr->nm, XmNbackground, ss->sgx->highlight_color, NULL);
    }
}

static void highlight_region(void)
{
  if (current_region != -1)
    {
      regrow *oldr;
      oldr = region_row(current_region);
      XtVaSetValues(oldr->rw, XmNbackground, ss->sgx->zoom_color, NULL);
      XtVaSetValues(oldr->nm, XmNbackground, ss->sgx->zoom_color, NULL);
    }
}

static void make_region_labels(file_info *hdr)
{
  char *str;
  if (hdr == NULL) return;
  str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  mus_snprintf(str, PRINT_BUFFER_SIZE, _("srate: %d"), hdr->srate);
  set_label(reg_srtxt, str);
  mus_snprintf(str, PRINT_BUFFER_SIZE, _("chans: %d"), hdr->chans);
  set_label(reg_chntxt, str);
  mus_snprintf(str, PRINT_BUFFER_SIZE, _("length: %.3f"), (float)((double)(hdr->samples) / (float)(hdr->chans * hdr->srate)));
  set_label(reg_lentxt, str);
  mus_snprintf(str, PRINT_BUFFER_SIZE, _("maxamp: %.3f"), region_maxamp(region_list_position_to_id(current_region)));
  set_label(reg_maxtxt, str);
  FREE(str);
}

void update_region_browser(bool grf_too)
{
  int i, len;
  region_state *rs;
  chan_info *cp;
  rs = region_report();
  len = rs->len;
  for (i = 0; i < len; i++)
    {
      regrow *r;
      r = region_row(i);
      ASSERT_WIDGET_TYPE(XmIsToggleButton(r->pl), r->pl);
      set_button_label(r->nm, rs->name[i]);
      XmToggleButtonSetState(r->pl, false, false);
      XtManageChild(r->rw);
    }
  for (i = len; i < max_regions(ss); i++) 
    if (region_rows[i])
      XtUnmanageChild(region_rows[i]->rw);
  free_region_state(rs);
  if (len == 0) return;
  XtManageChild(region_list);
  if (grf_too)
    {
      unhighlight_region();
      set_current_region(0);
      highlight_region();
      goto_window(region_rows[0]->nm);
      cp = rsp->chans[0];
      if (cp) 
	{
	  cp->sound = rsp;
	  cp->chan = 0;
	  set_sensitive(channel_f(cp), false);
	  set_sensitive(channel_w(cp), (region_chans(region_list_position_to_id(0)) > 1));
	  rsp->hdr = fixup_region_data(cp, 0, 0);
	  make_region_labels(rsp->hdr);
	  region_update_graph(cp);
	}
    }
}

static void region_ok_callback(Widget w, XtPointer context, XtPointer info) 
{
  XtUnmanageChild(region_dialog);
}

bool region_browser_is_active(void)
{
  return((region_dialog) && (XtIsRealized(region_dialog)));
}

static void region_resize_callback(Widget w, XtPointer context, XtPointer info)
{
  region_update_graph((chan_info *)context);
}

void delete_region_and_update_browser(int pos)
{
  int act;
  unhighlight_region();
  act = remove_region_from_list(pos);
  if (act == INVALID_REGION) return;
  if (region_dialog)
    {
      if (act != NO_REGIONS)
	{
	  set_current_region(0);
	  highlight_region();
	  goto_window(region_rows[0]->nm);
	}
      else set_current_region(-1);
      update_region_browser(1);
    }
}

static void region_unlist_callback(Widget w, XtPointer context, XtPointer info) 
{
  if (current_region != -1)
    delete_region_and_update_browser(current_region);
}

static void region_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  region_dialog_help();
}

static void region_insert_callback(Widget w, XtPointer context, XtPointer info) 
{
  if ((current_region != -1) &&
      (selected_channel()))
    paste_region(region_list_position_to_id(current_region), selected_channel());
}

static void region_mix_callback(Widget w, XtPointer context, XtPointer info) 
{
  if ((current_region != -1) &&
      (selected_channel()))
    add_region(region_list_position_to_id(current_region), selected_channel());
}

static void region_save_callback(Widget w, XtPointer context, XtPointer info) 
{
  if (current_region != -1)
    make_region_save_as_dialog(true);
}

static void region_up_arrow_callback(Widget w, XtPointer context, XtPointer info) 
{
  chan_info *cp;
  cp = rsp->chans[0];
  cp->sound = rsp;
  if (cp->chan > 0)
    {
      cp->chan--;
      set_sensitive(channel_f(cp), (cp->chan > 0));
      set_sensitive(channel_w(cp), true);
      fixup_region_data(cp, cp->chan, current_region);
      region_update_graph(cp);
    }
}

static void region_down_arrow_callback(Widget w, XtPointer context, XtPointer info) 
{
  chan_info *cp;
  cp = rsp->chans[0];
  cp->sound = rsp;
  if ((cp->chan + 1) < region_chans(region_list_position_to_id(current_region)))
    {
      cp->chan++;
      set_sensitive(channel_f(cp), true);
      set_sensitive(channel_w(cp), (region_chans(region_list_position_to_id(current_region)) > (cp->chan + 1)));
      fixup_region_data(cp, cp->chan, current_region);
      region_update_graph(cp);
    }
}

static void region_focus_callback(Widget w, XtPointer context, XtPointer info) 
{
  chan_info *cp;
  regrow *r = (regrow *)context;
  unhighlight_region();
  if (region_list_position_to_id(r->pos) == INVALID_REGION) return; /* needed by auto-tester */
  set_current_region(r->pos);
  cp = rsp->chans[0];
  cp->sound = rsp;
  cp->chan  = 0;
  highlight_region();
  set_sensitive(channel_f(cp), false);
  set_sensitive(channel_w(cp), (region_chans(region_list_position_to_id(current_region)) > 1));
  rsp->hdr = fixup_region_data(cp, 0, current_region);
  if (rsp->hdr == NULL) return;
  make_region_labels(rsp->hdr);
  region_update_graph(cp);
}


void reflect_play_region_stop(int n)
{
  if (region_rows)
    {
      regrow *rg;
      rg = region_row(region_id_to_list_position(n));
      if (rg) XmToggleButtonSetState(rg->pl, false, false);
    }
}

static void region_play_callback(Widget w, XtPointer context, XtPointer info) 
{
  regrow *r = (regrow *)context;
  if (XmToggleButtonGetState(r->pl))
    play_region(region_list_position_to_id(r->pos), IN_BACKGROUND);
  else stop_playing_region(region_list_position_to_id(r->pos), PLAY_BUTTON_UNSET);
}

static void region_print_callback(Widget w, XtPointer context, XtPointer info) 
{
  if (current_region != -1)
    region_print(eps_file(ss), _("region"), rsp->chans[0]);
}

static void region_edit_callback(Widget w, XtPointer context, XtPointer info) 
{
  if (current_region != -1) 
    region_edit(current_region);
}

static void reflect_file_in_region_browser(ss_watcher_reason_t reason, void *ignore)
{
  if (region_dialog)
    {
      bool file_p;
      file_p = (bool)(any_selected_sound());
      set_sensitive(mix_button, file_p);
      set_sensitive(insert_button, file_p);
    }
}

char *regrow_get_label(void *ur)
{
  regrow *r = (regrow *)ur;
  return(get_label(r->nm));
}

int regrow_get_pos(void *ur)
{
  regrow *r = (regrow *)ur;
  return(r->pos);
}

static void regrow_mouse_enter_label(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  mouse_enter_label(context, REGION_VIEWER);
}

static void regrow_mouse_leave_label(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  mouse_leave_label(context, REGION_VIEWER);
}

static regrow *make_regrow(Widget ww, Widget last_row, XtCallbackProc play_callback, XtCallbackProc name_callback)
{
  int n;
  Arg args[32];
  regrow *r;
  XmString s1;
  XtCallbackList n1, n3;

  s1 = XmStringCreateLocalized("");
  r = (regrow *)CALLOC(1, sizeof(regrow));

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNtopAttachment, (last_row) ? XmATTACH_WIDGET : XmATTACH_FORM); n++;
  if (last_row) {XtSetArg(args[n], XmNtopWidget, last_row); n++;}
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNheight, 18); n++; 
  r->rw = XtCreateWidget("rw", xmFormWidgetClass, ww, args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNselectColor, ss->sgx->pushed_button_color); n++;
  XtSetArg(args[n], XmNlabelString, s1); n++;
  XtSetArg(args[n], XmNvalueChangedCallback, n1 = make_callback_list(play_callback, (XtPointer)r)); n++;
  if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
  XtSetArg(args[n], XmNmarginWidth, 8); n++;
  r->pl = make_togglebutton_widget("pl", r->rw, args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, r->pl); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNhighlightThickness, 0); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
  XtSetArg(args[n], XmNfillOnArm, false); n++;
  XtSetArg(args[n], XmNrecomputeSize, false); n++;
  XtSetArg(args[n], XmNwidth, 300); n++;
  XtSetArg(args[n], XmNactivateCallback, n3 = make_callback_list(name_callback, (XtPointer)r)); n++;
  r->nm = XtCreateManagedWidget("nm", xmPushButtonWidgetClass, r->rw, args, n);
  XmStringFree(s1);

  XtAddEventHandler(r->nm, EnterWindowMask, false, regrow_mouse_enter_label, (XtPointer)r);
  XtAddEventHandler(r->nm, LeaveWindowMask, false, regrow_mouse_leave_label, (XtPointer)r);

  FREE(n1);
  FREE(n3);
  return(r);
}

static void make_region_dialog(void)
{
  int n, i, id;
  Arg args[32];
  Widget formw, last_row, infosep, fr ,rw;
  Widget prtb, editb, unlistb, plw, panes, toppane, sep1 = NULL;
  XmString xok, xinsert, xhelp, titlestr;
  regrow *r;
  chan_info *cp;

  xok = XmStringCreateLocalized(_("Dismiss"));
  xhelp = XmStringCreateLocalized(_("Help"));
  xinsert = XmStringCreateLocalized(_("Insert"));
  titlestr = XmStringCreateLocalized(_("Regions"));

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
  XtSetArg(args[n], XmNcancelLabelString, xinsert); n++;
  XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
  XtSetArg(args[n], XmNokLabelString, xok); n++;
  XtSetArg(args[n], XmNautoUnmanage, false); n++;
  XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
  XtSetArg(args[n], XmNnoResize, false); n++;
  XtSetArg(args[n], XmNtransient, false); n++;
  region_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), _("Regions"), args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->reset_button_color); n++;
  XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;
  save_as_button = XtCreateManagedWidget(_("Save as"), xmPushButtonGadgetClass, region_dialog, args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->doit_again_button_color); n++;
  XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;
  mix_button = XtCreateManagedWidget(_("Mix"), xmPushButtonGadgetClass, region_dialog, args, n);

  XtAddCallback(region_dialog,  XmNokCallback,       region_ok_callback,     NULL);
  XtAddCallback(region_dialog,  XmNcancelCallback,   region_insert_callback, NULL);
  XtAddCallback(region_dialog,  XmNhelpCallback,     region_help_callback,   NULL);
  XtAddCallback(mix_button,     XmNactivateCallback, region_mix_callback,    NULL);
  XtAddCallback(save_as_button, XmNactivateCallback, region_save_callback,   NULL);

  XmStringFree(xhelp);
  XmStringFree(xok);
  XmStringFree(xinsert);
  XmStringFree(titlestr);

  XtVaSetValues(XmMessageBoxGetChild(region_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
  XtVaSetValues(XmMessageBoxGetChild(region_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
  XtVaSetValues(XmMessageBoxGetChild(region_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
  XtVaSetValues(XmMessageBoxGetChild(region_dialog, XmDIALOG_OK_BUTTON), XmNbackground, ss->sgx->quit_button_color, NULL);
  XtVaSetValues(XmMessageBoxGetChild(region_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->doit_button_color, NULL);
  XtVaSetValues(XmMessageBoxGetChild(region_dialog, XmDIALOG_HELP_BUTTON), XmNbackground, ss->sgx->help_button_color, NULL);

  insert_button = XmMessageBoxGetChild(region_dialog, XmDIALOG_CANCEL_BUTTON);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNbottomWidget, XmMessageBoxGetChild(region_dialog, XmDIALOG_SEPARATOR)); n++;
  formw = XtCreateManagedWidget("formw", xmFormWidgetClass, region_dialog, args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, sep1); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNallowResize, true); n++;
  panes = XtCreateManagedWidget("panes", xmPanedWindowWidgetClass, formw, args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
  n = attach_all_sides(args, n);
  XtSetArg(args[n], XmNpaneMinimum, 40); n++;
  toppane = XtCreateManagedWidget("toppane", xmFormWidgetClass, panes, args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  plw = XtCreateManagedWidget(_("play"), xmLabelWidgetClass, toppane, args, n);
  
  n = 0;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
  XtSetArg(args[n], XmNrightPosition, 70); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, plw); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNscrollingPolicy, XmAUTOMATIC); n++;
  XtSetArg(args[n], XmNscrollBarDisplayPolicy, XmSTATIC); n++;
  region_list = XmCreateScrolledWindow(toppane, "reglist", args, n);

  n = attach_all_sides(args, 0);
  region_ww = XtCreateManagedWidget("ww", xmFormWidgetClass, region_list, args, n);
  XtVaSetValues(region_list, 
		XmNworkWindow, region_ww, 
		NULL);

  map_over_children(region_list, set_main_color_of_widget);
  last_row = NULL;
  
  region_rows = (regrow **)CALLOC(max_regions(ss), sizeof(regrow *));
  region_rows_size = max_regions(ss);
  for (i = 0; i < max_regions(ss); i++)
    {
      r = make_regrow(region_ww, last_row, region_play_callback, region_focus_callback);
      region_rows[i] = r;
      r->pos = i;
      last_row = r->rw;
    }

  update_region_browser(0);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, region_list); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
  XtSetArg(args[n], XmNseparatorType, XmSHADOW_ETCHED_IN); n++;
  infosep = XtCreateManagedWidget("infosep", xmSeparatorWidgetClass, toppane, args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, infosep); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
  reg_srtxt = XtCreateManagedWidget(_("srate:"), xmLabelWidgetClass, toppane, args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, infosep); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, reg_srtxt); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
  reg_chntxt = XtCreateManagedWidget(_("chans:"), xmLabelWidgetClass, toppane, args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, infosep); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, reg_chntxt); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
  reg_lentxt = XtCreateManagedWidget(_("length:"), xmLabelWidgetClass, toppane, args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, infosep); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, reg_lentxt); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
  reg_maxtxt = XtCreateManagedWidget(_("maxamp:"), xmLabelWidgetClass, toppane, args, n);


  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->zoom_color); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, reg_maxtxt); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, infosep); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
  XtSetArg(args[n], XmNshadowThickness, 1); n++;
  fr = XtCreateManagedWidget("reg-fr", xmFrameWidgetClass, toppane, args, n);
  
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
  rw = XtCreateManagedWidget("reg-rw", xmRowColumnWidgetClass, fr, args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
  XtSetArg(args[n], XmNarmColor, ss->sgx->red); n++;
  editb = XtCreateManagedWidget(_("edit"), xmPushButtonWidgetClass, rw, args, n);
  XtAddCallback(editb, XmNactivateCallback, region_edit_callback, NULL);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
  XtSetArg(args[n], XmNarmColor, ss->sgx->red); n++;
  prtb = XtCreateManagedWidget(_("print"), xmPushButtonWidgetClass, rw, args, n);
  XtAddCallback(prtb, XmNactivateCallback, region_print_callback, NULL);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
  XtSetArg(args[n], XmNarmColor, ss->sgx->red); n++;
  unlistb = XtCreateManagedWidget(_("unlist"), xmPushButtonWidgetClass, rw, args, n);
  XtAddCallback(unlistb, XmNactivateCallback, region_unlist_callback, NULL);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  n = attach_all_sides(args, n);
  XtSetArg(args[n], XmNpaneMinimum, 150); n++;
  region_grf = XtCreateManagedWidget("grf", xmFormWidgetClass, panes, args, n);

  XtManageChild(region_dialog);
  if (widget_width(region_dialog) < 400) set_widget_width(region_dialog, 400);

  id = region_list_position_to_id(0);
  rsp = make_simple_channel_display(region_srate(id), region_len(id), WITH_ARROWS, region_graph_style(ss), region_grf, WITHOUT_EVENTS);
  rsp->inuse = SOUND_REGION;
  set_current_region(0);
  cp = rsp->chans[0];
  XtVaSetValues(region_rows[0]->nm, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
  map_over_children(panes, color_sashes);
  XtVaSetValues(toppane, XmNpaneMinimum, 1, NULL);
  XtVaSetValues(region_grf, XmNpaneMinimum, 1, NULL);

  XtAddCallback(channel_graph(cp), XmNresizeCallback, region_resize_callback, (XtPointer)cp);
  XtAddCallback(channel_graph(cp), XmNexposeCallback, region_resize_callback, (XtPointer)cp);

  /* channel_f is up arrow, channel_w is down arrow */
  XtAddCallback(channel_f(cp), XmNactivateCallback, region_up_arrow_callback, NULL);
  XtAddCallback(channel_w(cp), XmNactivateCallback, region_down_arrow_callback, NULL);
  set_sensitive(channel_f(cp), false);
  if (region_chans(region_list_position_to_id(0)) > 1) set_sensitive(channel_w(cp), true);
  cp->chan = 0;
  rsp->hdr = fixup_region_data(cp, 0, 0);
  make_region_labels(rsp->hdr);
  highlight_region();
  region_update_graph(cp);
  add_ss_watcher(SS_FILE_OPEN_WATCHER, reflect_file_in_region_browser, NULL);
  set_dialog_widget(REGION_DIALOG, region_dialog);
}

void view_region_callback(Widget w, XtPointer context, XtPointer info)
{
  /* put up scrollable dialog describing/playing/editing the region list */
  if (region_dialog == NULL)
    make_region_dialog();
  else raise_dialog(region_dialog);
  if (!XtIsManaged(region_dialog)) 
    {
      set_current_region(0); 
      XtManageChild(region_dialog);
    }
}

bool region_dialog_is_active(void)
{
  return((region_dialog != NULL) && 
	 (XtIsManaged(region_dialog)));
}

void allocate_region_rows(int n)
{
  if ((region_dialog) && 
      (n > region_rows_size))
    {
      int i;
      region_rows = (regrow **)REALLOC(region_rows, n * sizeof(regrow *));
      for (i = region_rows_size; i < n; i++) region_rows[i] = NULL;
      region_rows_size = n;
    }
}

static regrow *region_row(int n)
{
  if (n < region_rows_size)
    {
      regrow *r;
      if (region_rows[n] == NULL)
	{
	  r = make_regrow(region_ww, 
			  (n > 0) ? (region_rows[n - 1]->rw) : NULL, 
			  region_play_callback, region_focus_callback);
	  region_rows[n] = r;
	  r->pos = n;
	}
      return(region_rows[n]);
    }
  return(NULL);
}

int region_dialog_region(void)
{
  return(region_list_position_to_id(current_region));
}

static XEN g_view_regions_dialog(void) 
{
  #define H_view_regions_dialog "(" S_view_regions_dialog "): start the region dialog"
  if (snd_regions() > 0) 
    view_region_callback(MAIN_PANE(ss), NULL, NULL);
  return(XEN_WRAP_WIDGET(region_dialog));
}

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_view_regions_dialog_w, g_view_regions_dialog)
#else
#define g_view_regions_dialog_w g_view_regions_dialog
#endif

void g_init_gxregion(void)
{
  XEN_DEFINE_PROCEDURE(S_view_regions_dialog, g_view_regions_dialog_w, 0, 0, 0,  H_view_regions_dialog);
}
