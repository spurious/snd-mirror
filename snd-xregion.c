#include "snd.h"

/* -------- region browser -------- */

static Widget region_dialog=NULL,region_list,region_grf;
static regrow **region_rows = NULL;
static snd_info *reg_sp = NULL;
static int current_region = -1;
static Widget selectw,reg_srtxt,reg_lentxt,reg_chntxt,reg_maxtxt;

static void region_update_graph(chan_info *cp)
{
  reg_sp->nchans = region_chans(current_region);
  update_graph(cp,NULL);
  reg_sp->nchans = 1;
}

static void make_region_element(region_state *rs, int i)
{
  regrow *r;
  r = region_rows[i];
  set_button_label_bold(r->nm,rs->name[i]);
  XmToggleButtonSetState(r->sv,rs->save[i],FALSE);
  XmToggleButtonSetState(r->pl,FALSE,FALSE);
  XtManageChild(r->rw);
}

static void unhighlight_region(snd_state *ss)
{
  regrow *oldr;
  if (current_region != -1)
    {
      oldr = region_rows[current_region];
      if (!(ss->using_schemes)) 
	{
	  XtVaSetValues(oldr->rw,XmNbackground,(ss->sgx)->highlight_color,NULL);
	  XtVaSetValues(oldr->nm,XmNbackground,(ss->sgx)->highlight_color,NULL);
	}
    }
}

static void unmake_region_labels (void)
{
  set_button_label_bold(reg_srtxt,STR_srate_p);
  set_button_label_bold(reg_chntxt,STR_chans_p);
  set_button_label_bold(reg_lentxt,STR_length_p);
  set_button_label_bold(reg_maxtxt,STR_maxamp_p);
}

static void highlight_region(snd_state *ss)
{
  regrow *oldr;
  if (current_region != -1)
    {
      oldr = region_rows[current_region];
      if (!(ss->using_schemes)) 
	{
	  XtVaSetValues(oldr->rw,XmNbackground,(ss->sgx)->zoom_color,NULL);
	  XtVaSetValues(oldr->nm,XmNbackground,(ss->sgx)->zoom_color,NULL);
	}
    }
}

static void make_region_labels(file_info *hdr)
{
  char *str;
  if (hdr == NULL) return;
  str = (char *)CALLOC(256,sizeof(char));
  sprintf(str,STR_srate,hdr->srate);
  set_button_label_bold(reg_srtxt,str);
  sprintf(str,STR_chans,hdr->chans);
  set_button_label_bold(reg_chntxt,str);
  sprintf(str,STR_length,(float)(hdr->samples)/(float)(hdr->chans * hdr->srate));
  set_button_label_bold(reg_lentxt,str);
  sprintf(str,STR_maxamp,region_maxamp(current_region));
  set_button_label_bold(reg_maxtxt,str);
  FREE(str);
}

void update_region_browser(snd_state *ss, int grf_too)
{
  int i,len;
  region_state *rs;
  chan_info *cp;
  rs = region_report();
  len = rs->len;
  for (i=0;i<len;i++) make_region_element(rs,i);
  for (i=len;i<max_regions(ss);i++) XtUnmanageChild(region_rows[i]->rw);
  free_region_state(rs);
  if (len == 0) return;
  XtManageChild(region_list);
  if (grf_too)
    {
      unhighlight_region(ss);
      current_region = 0;
      highlight_region(ss);
      goto_window(region_rows[0]->nm);
      cp = reg_sp->chans[0];
      if (cp) 
	{
	  cp->chan = 0;
	  set_sensitive(channel_f(cp),FALSE);
	  set_sensitive(channel_w(cp),(region_chans(0) > 1));
	  if (region_ok(0)) 
	    {
	      reg_sp->hdr = fixup_region_data(cp,0,0);
	      make_region_labels(reg_sp->hdr);
	      region_update_graph(cp);
	    }
	  else unmake_region_labels();
	}
    }
}

static void region_ok_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  XtUnmanageChild(region_dialog);
}

int region_browser_is_active(void)
{
  return((region_dialog) && (XtIsRealized(region_dialog)));
}

static void region_resize_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  region_update_graph((chan_info *)clientData);
}

static int region_dialog_active(void);

void delete_region_and_update_browser(snd_state *ss, int n)
{
  int act;
  act = delete_region(n);
  if (act == INVALID_REGION) return;
  if (region_dialog_active())
    {
      if (act != NO_REGIONS)
	{
	  current_region = 0;
	  highlight_region(ss);
	  goto_window(region_rows[0]->nm);
	}
      else 
	{
	  unhighlight_region(ss);
	  current_region = -1;
	}
      update_region_browser(ss,1);
    }
}

static void region_delete_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  if (current_region != -1)
    delete_region_and_update_browser(ss,current_region);
}

static void region_help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  region_dialog_help((snd_state *)clientData);
}

void select_region_and_update_browser(snd_state *ss, int n)
{
  deactivate_selection();  /* just in case there's a region being highlighted */
  if (region_dialog_active())
    {
      unhighlight_region(ss);
      select_region(n);
      current_region = 0;
      highlight_region(ss);
      goto_window(region_rows[0]->nm);
      set_sensitive(selectw,FALSE);
      update_region_browser(ss,0);
    }
}

static void region_select_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  if (current_region != -1)
    select_region_and_update_browser(ss,current_region);
}

static void region_up_arrow_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  chan_info *cp;
  cp = reg_sp->chans[0];
  if (cp->chan > 0)
    {
      cp->chan--;
      set_sensitive(channel_f(cp),(cp->chan > 0));
      set_sensitive(channel_w(cp),TRUE);
      fixup_region_data(cp,cp->chan,current_region);
      region_update_graph(cp);
    }
}

static void region_down_arrow_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  chan_info *cp;
  cp = reg_sp->chans[0];
  if ((cp->chan+1) < region_chans(current_region))
    {
      cp->chan++;
      set_sensitive(channel_f(cp),TRUE);
      set_sensitive(channel_w(cp),(region_chans(current_region) > (cp->chan+1)));
      fixup_region_data(cp,cp->chan,current_region);
      region_update_graph(cp);
    }
}

static void region_focus_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss;
  chan_info *cp;
  regrow *r = (regrow *)clientData;
  ss = r->ss;
  unhighlight_region(ss);
  current_region=r->pos;
  cp = reg_sp->chans[0];
  cp->chan  = 0;
  highlight_region(ss);
  set_sensitive(channel_f(cp),FALSE);
  set_sensitive(channel_w(cp),(region_chans(current_region) > 1));
  set_sensitive(selectw,(current_region != 0));
  reg_sp->hdr = fixup_region_data(cp,0,current_region);
  make_region_labels(reg_sp->hdr);
  region_update_graph(cp);
}


void reflect_play_region_stop(region_info *r)
{
  regrow *rg;
  rg = (regrow *)(r->rg);
  if (rg) XmToggleButtonSetState(rg->pl,FALSE,FALSE);
}

static void region_play_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  regrow *r = (regrow *)clientData;
  if (XmToggleButtonGetState(r->pl))
    play_region(r->ss,r->pos,(void *)r,FALSE);
  else stop_playing_region(r->pos);
}

static void region_save_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  regrow *r = (regrow *)clientData;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)callData;
  protect_region(r->pos,cb->set);
}

void set_region_protect(int reg, int protect)
{
  regrow *r;
  protect_region(reg,protect);
  if (region_rows)
    {
      r = region_rows[reg];
      if ((r) && (r->sv)) XmToggleButtonSetState(r->sv,protect,FALSE);
    }
}

static void region_print_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  if (current_region != -1)
    region_print(eps_file(ss),"region",reg_sp->chans[0]);
}

static void region_edit_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  if (current_region != -1) 
    region_edit((snd_state *)clientData,current_region);
}

static Widget region_ww;

static void make_region_dialog(snd_state *ss)
{
  int n,i;
  Arg args[32];
  Widget formw,last_row,ww,infosep,prtb,editb;
  XmString xok,xdelete,xhelp,titlestr;
  regrow *r;
  chan_info *cp;
  file_info *hdr;
  ww_info *wwl;

  xok = XmStringCreate(STR_Dismiss,XmFONTLIST_DEFAULT_TAG);
  xhelp = XmStringCreate(STR_Help,XmFONTLIST_DEFAULT_TAG);
  xdelete = XmStringCreate(STR_Delete,XmFONTLIST_DEFAULT_TAG);
  titlestr = XmStringCreate(STR_Regions,XmFONTLIST_DEFAULT_TAG);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNcancelLabelString,xdelete); n++;
  XtSetArg(args[n],XmNhelpLabelString,xhelp); n++;
  XtSetArg(args[n],XmNokLabelString,xok); n++;
  XtSetArg(args[n],XmNautoUnmanage,FALSE); n++;
  XtSetArg(args[n],XmNdialogTitle,titlestr); n++;
#if RESIZE_DIALOG
  XtSetArg(args[n],XmNresizePolicy,XmRESIZE_GROW); n++;
  XtSetArg(args[n],XmNnoResize,FALSE); n++;
#endif
  XtSetArg(args[n],XmNtransient,FALSE); n++;
  region_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss),STR_Regions,args,n);
  add_dialog(ss,region_dialog);
#if OVERRIDE_TOGGLE
  override_form_translation(region_dialog);
#endif

  XtAddCallback(region_dialog,XmNokCallback,region_ok_Callback,ss);
  XtAddCallback(region_dialog,XmNcancelCallback,region_delete_Callback,ss);
  XtAddCallback(region_dialog,XmNhelpCallback,region_help_Callback,ss);
  XmStringFree(xhelp);
  XmStringFree(xok);
  XmStringFree(xdelete);
  XmStringFree(titlestr);

  if (!(ss->using_schemes))
    {
      XtVaSetValues(XmMessageBoxGetChild(region_dialog,XmDIALOG_OK_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
      XtVaSetValues(XmMessageBoxGetChild(region_dialog,XmDIALOG_CANCEL_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
      XtVaSetValues(XmMessageBoxGetChild(region_dialog,XmDIALOG_HELP_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
    }

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  selectw = XtCreateManagedWidget(STR_Select,xmPushButtonWidgetClass,region_dialog,args,n);
  XtAddCallback(selectw,XmNactivateCallback,region_select_Callback,ss);
  set_sensitive(selectw,FALSE);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNbottomWidget,XmMessageBoxGetChild(region_dialog,XmDIALOG_SEPARATOR)); n++;
  formw = sndCreateFormWidget("formw",region_dialog,args,n);

  wwl = make_title_row(ss,formw,STR_save,STR_play,STR_regions,DONT_PAD_TITLE,WITHOUT_SORT_BUTTON,WITH_PANED_WINDOW);
  ww = wwl->ww;
  region_ww = ww;
  region_list = wwl->list;
  if (!(ss->using_schemes)) map_over_children(region_list,set_main_color_of_widget,(void *)ss);
  last_row = NULL;
  
  region_rows = (regrow **)CALLOC(max_regions(ss),sizeof(regrow *));
  for (i=0;i<max_regions(ss);i++)
    {
      r = make_regrow(ss,ww,last_row,region_save_Callback,region_play_Callback,region_focus_Callback);
      region_rows[i] = r;
      r->pos = i;
      r->ss = ss;
      last_row = r->rw;
    }

  update_region_browser(ss,0);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,region_list); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
  XtSetArg(args[n],XmNseparatorType,XmSHADOW_ETCHED_IN); n++;
  infosep = XtCreateManagedWidget("infosep",xmSeparatorWidgetClass,wwl->toppane,args,n);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,infosep); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XM_FONT_RESOURCE,BOLD_BUTTON_FONT(ss)); n++;
  XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;
  reg_srtxt = XtCreateManagedWidget(STR_srate_p,xmLabelWidgetClass,wwl->toppane,args,n);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,infosep); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,reg_srtxt); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XM_FONT_RESOURCE,BOLD_BUTTON_FONT(ss)); n++;
  XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;
  reg_chntxt = XtCreateManagedWidget(STR_chans_p,xmLabelWidgetClass,wwl->toppane,args,n);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,infosep); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,reg_chntxt); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XM_FONT_RESOURCE,BOLD_BUTTON_FONT(ss)); n++;
  XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;
  reg_lentxt = XtCreateManagedWidget(STR_length_p,xmLabelWidgetClass,wwl->toppane,args,n);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,infosep); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,reg_lentxt); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XM_FONT_RESOURCE,BOLD_BUTTON_FONT(ss)); n++;
  XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;
  reg_maxtxt = XtCreateManagedWidget(STR_maxamp_p,xmLabelWidgetClass,wwl->toppane,args,n);

  n=0;
  if (!(ss->using_schemes)) 
    {
      XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;
      XtSetArg(args[n],XmNarmColor,(ss->sgx)->pushed_button_color); n++;
    }
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,infosep); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,reg_maxtxt); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XM_FONT_RESOURCE,BOLD_BUTTON_FONT(ss)); n++;
  XtSetArg(args[n],XmNalignment,XmALIGNMENT_CENTER); n++;
  editb = XtCreateManagedWidget(STR_edit,xmPushButtonWidgetClass,wwl->toppane,args,n);
  XtAddCallback(editb,XmNactivateCallback,region_edit_Callback,(XtPointer)ss);

  n=0;
  if (!(ss->using_schemes)) 
    {
      XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;
      XtSetArg(args[n],XmNarmColor,(ss->sgx)->pushed_button_color); n++;
    }
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,infosep); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,editb); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XM_FONT_RESOURCE,BOLD_BUTTON_FONT(ss)); n++;
  XtSetArg(args[n],XmNalignment,XmALIGNMENT_CENTER); n++;
  prtb = XtCreateManagedWidget(STR_print,xmPushButtonWidgetClass,wwl->toppane,args,n);
  XtAddCallback(prtb,XmNactivateCallback,region_print_Callback,(XtPointer)ss);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->white); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNpaneMinimum,150); n++;
  region_grf = sndCreateFormWidget("grf",wwl->panes,args,n);

  XtManageChild(region_dialog);

  if (!reg_sp) 
    { /* just a place holder, I think -- see make_region_readable in snd-clip.c */
      reg_sp = (snd_info *)CALLOC(1,sizeof(snd_info));
      reg_sp->s_type = SND_INFO;
      reg_sp->nchans = 1;
      reg_sp->allocated_chans = 1;
      reg_sp->chans = (chan_info **)CALLOC(1,sizeof(chan_info *));
      reg_sp->sx_scroll_max = 100;
      reg_sp->hdr = (file_info *)CALLOC(1,sizeof(file_info));
      hdr = reg_sp->hdr;
      hdr->samples = region_len(0);
      hdr->srate = region_srate(0);
      hdr->comment = NULL;
      hdr->chans = 1;
      current_region = 0;
      add_channel_window(reg_sp,0,ss,0,0,region_grf,WITH_ARROWS);
      cp = reg_sp->chans[0];
      cp->edit_size = 1;
      cp->edit_ctr = 0;
      allocate_ed_list(cp);
      cp->samples = (int *)CALLOC(cp->edit_size,sizeof(int));
      cp->sound_size = 1;
      cp->sound_ctr = 0;
      cp->sounds = (snd_data **)CALLOC(cp->sound_size,sizeof(snd_data *));
      cp->samples[0] = region_len(0);
    }
  else 
    {
      add_channel_window(reg_sp,0,ss,0,0,region_grf,WITH_ARROWS);
      cp = reg_sp->chans[0];
    }

  cp->hookable = 0;
  if (!(ss->using_schemes)) 
    {
      XtVaSetValues(region_rows[0]->nm,XmNbackground,(ss->sgx)->white,XmNforeground,(ss->sgx)->black,NULL);
      map_over_children(wwl->panes,color_sashes,(void *)ss);
    }
  XtVaSetValues(wwl->toppane,XmNpaneMinimum,1,NULL);
  XtVaSetValues(region_grf,XmNpaneMinimum,1,NULL);

  XtAddCallback(channel_graph(cp),XmNresizeCallback,region_resize_Callback,(XtPointer)cp);
  XtAddCallback(channel_graph(cp),XmNexposeCallback,region_resize_Callback,(XtPointer)cp);

  /* channel_f is up arrow, channel_w is down arrow */
  XtAddCallback(channel_f(cp),XmNactivateCallback,region_up_arrow_Callback,(XtPointer)ss);
  XtAddCallback(channel_w(cp),XmNactivateCallback,region_down_arrow_Callback,(XtPointer)ss);
  set_sensitive(channel_f(cp),FALSE);
  if (region_chans(0) > 1) set_sensitive(channel_w(cp),TRUE);
  cp->chan = 0;
  reg_sp->hdr = fixup_region_data(cp,0,0);
  make_region_labels(reg_sp->hdr);
  highlight_region(ss);
  region_update_graph(cp);
  FREE(wwl); 
  wwl=NULL;
}

void View_Region_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  /* put up scrollable dialog describing/playing/editing the region list */
  snd_state *ss = (snd_state *)clientData;
  if (region_dialog == NULL)
    make_region_dialog(ss);
  else raise_dialog(region_dialog);
  if (!XtIsManaged(region_dialog)) {current_region = 0; XtManageChild(region_dialog);}
}

static int region_dialog_active(void) {return(region_dialog != NULL);}

int region_dialog_is_active(void)
{
  return((region_dialog != NULL) && (XtIsManaged(region_dialog)));
}

void allocate_region_rows(snd_state *ss, int n)
{
  Widget last_row;
  regrow *r;
  int i;
  if ((region_dialog) && (n > max_regions(ss)))
    {
      r = region_rows[max_regions(ss) - 1];
      last_row = r->rw;
      region_rows = (regrow **)REALLOC(region_rows,n * sizeof(regrow *));
      for (i=max_regions(ss);i<n;i++)
	{
	  r = make_regrow(ss,region_ww,last_row,region_save_Callback,region_play_Callback,region_focus_Callback);
	  region_rows[i] = r;
	  r->pos = i;
	  r->ss = ss;
	  last_row = r->rw;
	}
    }
}



