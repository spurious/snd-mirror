#include "snd.h"

/* -------- STATS WINDOW -------- */

#if HAVE_MALLINFO
  #include <malloc.h>
#endif

static Widget stats_window = NULL;

static void stats_help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  stats_dialog_help((snd_state *)clientData);
}

static void stats_dismiss_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  set_show_usage_stats((snd_state *)clientData,FALSE);
}

static void stats_update_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  update_all_usage_stats(ss);
  check_stats_window(ss,TRUE);
}

static Widget stats_form;


#ifdef DEBUG_MEMORY
  char *mem_stats(snd_state *ss,int ub);
#endif

void update_stats(snd_state *ss)
{
  int i,j,pos,regs,used_bytes=0;
  int vals[2];
  snd_info *sp;
  chan_info *cp;
  char *str,*r0 = NULL,*r1 = NULL;
  XmTextSetString(stats_form,"file chn: mem(#bufs), main, temp(#files), amp envs\n\n");
  for (i=0;i<ss->max_sounds;i++)
    {
      if ((sp=((snd_info *)(ss->sounds[i]))))
	{
	  if (sp->inuse)
	    {
	      for (j=0;j<(sp->nchans);j++)
		{
		  if ((cp=((chan_info *)(sp->chans[j]))))
		    {
		      if (cp->stats)
			{
			  pos = XmTextGetLastPosition(stats_form);
			  str = update_chan_stats(cp);
			  XmTextInsert(stats_form,pos,str);
			  FREE(str);
			}}}}}}
  regs = snd_regions();
  if (regs > 0)
    {
      region_stats(vals);
      str = (char *)CALLOC(256,sizeof(char));
      sprintf(str,"\nregions (%d): %s + %s\n",regs,r0=kmg(vals[0]),r1=kmg(vals[1]));
      pos = XmTextGetLastPosition(stats_form);
      XmTextInsert(stats_form,pos,str);
      if (r0) free(r0);
      if (r1) free(r1);
      FREE(str);
    }
#if HAVE_MALLINFO
  {
    struct mallinfo mall;
    char *m0=NULL,*m1=NULL,*m2=NULL,*m3=NULL;
    mall = mallinfo();
    str = (char *)CALLOC(256,sizeof(char));
    sprintf(str,"\nmalloc: %s + %s (in use: %s, freed: %s)\n",
	    m0=kmg(mall.arena),m1=kmg(mall.hblkhd),
	    m2=kmg(used_bytes=mall.uordblks),m3=kmg(mall.fordblks));
    pos = XmTextGetLastPosition(stats_form);
    XmTextInsert(stats_form,pos,str);
    if (m0) free(m0);
    if (m1) free(m1);
    if (m2) free(m2);
    if (m3) free(m3);
    FREE(str);
  }
#endif
#ifdef DEBUG_MEMORY
  str = mem_stats(ss,used_bytes);
  pos = XmTextGetLastPosition(stats_form);
  XmTextInsert(stats_form,pos,str);
  free(str);
#endif
}

void update_stats_display(snd_state *ss, int all)
{
  /* dismiss update help -- update forces recalc of all stats */
  XmString xstr1,xstr2,xstr3,titlestr;
  int n;
  Arg args[20];

  if (!stats_window)
    {
      n=0;
      xstr1 = XmStringCreate(STR_Dismiss,XmFONTLIST_DEFAULT_TAG);
      xstr2 = XmStringCreate(STR_Help,XmFONTLIST_DEFAULT_TAG);
      xstr3 = XmStringCreate(STR_Update,XmFONTLIST_DEFAULT_TAG);
      titlestr = XmStringCreate(STR_Disk_and_Memory_Usage,XmFONTLIST_DEFAULT_TAG);
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNcancelLabelString,xstr3); n++;
      XtSetArg(args[n],XmNhelpLabelString,xstr2); n++;
      XtSetArg(args[n],XmNokLabelString,xstr1); n++;
      XtSetArg(args[n],XmNdialogTitle,titlestr); n++;
      XtSetArg(args[n],XmNautoUnmanage,FALSE); n++;
#if RESIZE_DIALOG
      XtSetArg(args[n],XmNresizePolicy,XmRESIZE_GROW); n++;
      XtSetArg(args[n],XmNnoResize,FALSE); n++;
#endif
      XtSetArg(args[n],XmNtransient,FALSE); n++;
      stats_window = XmCreateTemplateDialog(MAIN_SHELL(ss),STR_Disk_and_Memory_Usage,args,n);
      add_dialog(ss,stats_window);
#if OVERRIDE_TOGGLE
      override_form_translation(stats_window);
#endif

      XtAddCallback(stats_window,XmNcancelCallback,stats_update_Callback,ss);
      XtAddCallback(stats_window,XmNhelpCallback,stats_help_Callback,ss);
      XtAddCallback(stats_window,XmNokCallback,stats_dismiss_Callback,ss);
      XmStringFree(xstr1);
      XmStringFree(xstr2);
      XmStringFree(xstr3);
      XmStringFree(titlestr);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNheight,60); n++;
      XtSetArg(args[n],XmNwidth,300); n++;
      stats_form = sndCreateTextWidget(ss,"stats",stats_window,args,n);

#if MANAGE_DIALOG
      XtManageChild(stats_window);
#endif
      if (!(ss->using_schemes)) 
	{
	  XtVaSetValues(XmMessageBoxGetChild(stats_window,XmDIALOG_OK_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
	  XtVaSetValues(XmMessageBoxGetChild(stats_window,XmDIALOG_CANCEL_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
	  XtVaSetValues(XmMessageBoxGetChild(stats_window,XmDIALOG_HELP_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
	  map_over_children(stats_window,set_main_color_of_widget,ss);
	  XtVaSetValues(stats_form,XmNbackground,(ss->sgx)->white,XmNforeground,(ss->sgx)->black,NULL);
	}
    }
  else raise_dialog(stats_window);
  if (all) update_all_usage_stats(ss);
  update_stats(ss);
  if (!(XtIsManaged(stats_window))) XtManageChild(stats_window);
}

void check_stats_window(snd_state *ss, int val)
{
  /* if val==0, close active display if any, if val==1, open and spin through all current chans setting/gathering */
  if (val == 0)
    {
      if ((stats_window) && (XtIsManaged(stats_window)))
	XtUnmanageChild(stats_window);
    }
  else
    {
      update_stats_display(ss,TRUE);
    }
}

