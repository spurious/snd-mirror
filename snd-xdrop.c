#include "snd.h"

static Atom FILE_NAME; /* Sun uses this, SGI uses STRING */

static SCM drop_hook;

static void massage_selection(Widget w, XtPointer context, Atom *selection, Atom *type, XtPointer value, unsigned long *length, int *format)
{
  unsigned long i;
  char *str;
  snd_info *sp = NULL;
  if ((*type == XA_STRING) || (*type == FILE_NAME))
    {
      str = (char *)CALLOC(*length + 1, sizeof(char));
      for (i = 0; i<(*length); i++)
	{
	  if (((char *)value)[i] == ' ')
	    {
	      str[i] = '\0';
	      break;
	    }
	  else str[i] = ((char *)value)[i];
	}
      str[*length] = '\0';
      if ((!(HOOKED(drop_hook))) || 
	  (!(TRUE_P(g_c_run_or_hook(drop_hook,
					SCM_LIST1(TO_SCM_STRING(str)),
					"drop")))))
	{
	  sp = snd_open_file(str, (snd_state *)context, FALSE);
	  if (sp) select_channel(sp, 0);
	  /* value is the file name if dropped icon from filer */
	}
      FREE(str);
    }
}

static void HandleDrop(Widget w, XtPointer context, XtPointer info) 
{
  /* this is called (see InitializeDrop) when a drop occurs */
  XmDropProcCallbackStruct *cb = (XmDropProcCallbackStruct *)info;
  Arg args[12];
  int n, i, num_targets, k;
  Atom *targets;
  XmDropTransferEntryRec entries[2];
  XtPointer ss;
  XtVaGetValues(w, XmNuserData, &ss, NULL);
  if ((cb->dropAction != XmDROP) || 
      (cb->operation != XmDROP_COPY))
    {
      cb->dropSiteStatus = XmINVALID_DROP_SITE;
      return;
    }
  k = -1;
  XtVaGetValues(cb->dragContext, XmNexportTargets, &targets, XmNnumExportTargets, &num_targets, NULL);
  for (i = 0; i < num_targets; i++) 
    if ((targets[i] == XA_STRING) || 
	(targets[i] == FILE_NAME))
      {
	k = i; 
	break;
      }
  if (k == -1)
    {
#if DEBUGGING
      fprintf(stderr, "failed drop attempt:\n");
      for (i = 0; i < num_targets; i++) 
	fprintf(stderr, "  target %d = %s\n", i, 
		XGetAtomName(MAIN_DISPLAY(((snd_state *)ss)), 
			     targets[i]));
#endif
      cb->dropSiteStatus = XmINVALID_DROP_SITE;
      cb->operation = XmDROP_NOOP;
      n = 0;
      XtSetArg(args[n], XmNnumDropTransfers, 0); n++;
      XtSetArg(args[n], XmNtransferStatus, XmTRANSFER_FAILURE); n++;
      XmDropTransferStart(cb->dragContext, args, n);
      return;
    }
  entries[0].target = targets[k];
  entries[0].client_data = ss;
  n = 0;
  XtSetArg(args[n], XmNdropTransfers, entries); n++;
  XtSetArg(args[n], XmNnumDropTransfers, 1); n++;
  XtSetArg(args[n], XmNtransferProc, massage_selection); n++;
  cb->operation = XmDROP_COPY;
  XmDropTransferStart(cb->dragContext, args, n);
}

void InitializeDrop(snd_state *ss)
{
  /* called via startup func */
  int n;
  Atom targets[2];
  Arg args[12];
  targets[0] = XA_STRING;
  FILE_NAME = XInternAtom(MAIN_DISPLAY(ss), "FILE_NAME", FALSE);
  targets[1] = FILE_NAME;
  n = 0;
  XtSetArg(args[n], XmNdropSiteOperations, XmDROP_COPY); n++;
  XtSetArg(args[n], XmNimportTargets, targets); n++;
  XtSetArg(args[n], XmNnumImportTargets, 2); n++;
  XtSetArg(args[n], XmNdropProc, HandleDrop); n++;
  XmDropSiteRegister(get_menubar(), args, n); /* won't accept main-shell here, or main-pane! */
}

void g_init_gxdrop(SCM local_doc)
{
  #define H_drop_hook S_drop_hook " (filename) is called whenever Snd receives a drag-and-drop \
event. If the returns #t, the file is not opened by Snd."

  drop_hook = MAKE_HOOK(S_drop_hook, 1, H_drop_hook); /* arg = filename */
}
