#include "snd.h"

static void massage_selection(Widget w,XtPointer clientData,Atom *selection,Atom *type,XtPointer value,unsigned long *length,int *format)
{
  unsigned long i;
  char *str;
  snd_info *sp;
  if (*type == XA_STRING)
    {
      str = (char *)CALLOC(*length + 1,sizeof(char));
      for (i=0;i<(*length);i++)
	{
	  if (((char *)value)[i] == ' ')
	    {
	      str[i] = '\0';
	      break;
	    }
	  else str[i] = ((char *)value)[i];
	}
      str[*length] = '\0';
      sp = snd_open_file(str,(snd_state *)clientData);
      if (sp) select_channel(sp,0);
      /* value is the file name if dropped icon from filer */
      FREE(str);
    }
}

static void HandleDrop(Widget w,XtPointer clientData,XtPointer callData) 
{
  /* this is called (see InitializeDrop) when a drop occurs */
  XmDropProcCallbackStruct *cb = (XmDropProcCallbackStruct *)callData;
  Arg args[12];
  int n,i,num_targets,k;
  Atom *targets;
  XmDropTransferEntryRec entries[2];
  XtPointer ss;
  XtVaGetValues(w,XmNuserData,&ss,NULL);
  if ((cb->dropAction != XmDROP) || (cb->operation != XmDROP_COPY))
    {
      cb->dropSiteStatus = XmINVALID_DROP_SITE;
      return;
    }
  k=-1;
  XtVaGetValues(cb->dragContext,XmNexportTargets,&targets,XmNnumExportTargets,&num_targets,NULL);
  for (i=0;i<num_targets;i++) 
    {
      if (targets[i] == XA_STRING)
	{
	  k=i; 
	  break;
	}
    }
  if (k == -1)
    {
      cb->dropSiteStatus = XmINVALID_DROP_SITE;
      cb->operation = XmDROP_NOOP;
      n=0;
      XtSetArg(args[n],XmNnumDropTransfers,0); n++;
      XtSetArg(args[n],XmNtransferStatus,XmTRANSFER_FAILURE); n++;
      XmDropTransferStart(cb->dragContext,args,n);
      return;
    }
  entries[0].target = targets[k];
  entries[0].client_data = ss;
  n=0;
  XtSetArg(args[n],XmNdropTransfers,entries); n++;
  XtSetArg(args[n],XmNnumDropTransfers,1); n++;
  XtSetArg(args[n],XmNtransferProc,massage_selection); n++;
  cb->operation = XmDROP_COPY;
  XmDropTransferStart(cb->dragContext,args,n);
}

void InitializeDrop(snd_state *ss)
{
  /* called via startup func */
  int n;
  Atom targets[1];
  Arg args[12];
  targets[0] = XA_STRING;
  n=0;
  XtSetArg(args[n],XmNdropSiteOperations,XmDROP_COPY); n++;
  XtSetArg(args[n],XmNimportTargets,targets); n++;
  XtSetArg(args[n],XmNnumImportTargets,1); n++;
  XtSetArg(args[n],XmNdropProc,HandleDrop); n++;
  XmDropSiteRegister(get_menubar(),args,n);
}

/* all this seems to be Motif-specific? (i.e. the dragger needs to be a Motif program) */
