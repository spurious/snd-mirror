#include "snd.h"

static Atom FILE_NAME; /* Sun uses this, SGI uses STRING */

static XEN drop_hook;
#define UNPACK_SOUND(a) (a >> 16)
#define UNPACK_CHANNEL(a) (a & 0xff)

static void massage_selection(Widget w, XtPointer context, Atom *selection, Atom *type, XtPointer value, unsigned long *length, int *format)
{
  unsigned long i;
  int data, snd, chn;
  char *str, *origin;
  snd_state *ss;
  snd_info *sp = NULL;
  Widget caller;
  if ((*type == XA_STRING) || (*type == FILE_NAME))
    {
      str = (char *)CALLOC(*length + 1, sizeof(char));
      for (i = 0; i < (*length); i++)
	{
	  if (((char *)value)[i] == ' ')
	    {
	      str[i] = '\0';
	      break;
	    }
	  else str[i] = ((char *)value)[i];
	}
      str[*length] = '\0';
      if ((!(XEN_HOOKED(drop_hook))) || 
	  (!(XEN_TRUE_P(run_or_hook(drop_hook,
				    XEN_LIST_1(C_TO_XEN_STRING(str)),
				    "drop")))))
	{
	  caller = (Widget)((XmDropTransferEntry)context)->client_data;
	  if (strcmp(XtName(caller), "menuBar") == 0)
	    {
	      sp = snd_open_file(str, get_global_state(), FALSE);
	      if (sp) select_channel(sp, 0);
	    }
	  else
	    {
	      XtVaGetValues(caller, XmNuserData, &data, NULL);
	      chn = UNPACK_CHANNEL(data);
	      snd = UNPACK_SOUND(data);
	      ss = get_global_state();
	      if ((snd >= 0) &&
		  (snd < ss->max_sounds) && 
		  (snd_ok(ss->sounds[snd])) &&
		  (chn >= 0) &&
		  (chn < ss->sounds[snd]->nchans) &&
		  (mus_file_probe(str)))
		{
		  sp = ss->sounds[snd];
		  select_channel(sp, chn);
		  origin = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
		  mus_snprintf(origin, PRINT_BUFFER_SIZE, "drop mix %s " OFF_TD, str, CURSOR(sp->chans[chn]));
		  mix_complete_file_at_cursor(sp, str, origin, with_mix_tags(ss)); 
		  FREE(origin);
		}
	    }
	  /* value is the file name if dropped icon from filer */
	}
      FREE(str);
    }
}

static void handle_drop(Widget w, XtPointer context, XtPointer info) 
{
  /* this is called (see add_drop) when a drop occurs */
  XmDropProcCallbackStruct *cb = (XmDropProcCallbackStruct *)info;
  Arg args[12];
  int n, i, num_targets, k;
  Atom *targets;
  XmDropTransferEntryRec entries[2];
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
#if 0
      fprintf(stderr, "failed drop attempt:\n");
      for (i = 0; i < num_targets; i++) 
	fprintf(stderr, "  target %d = %s\n", i, 
		XGetAtomName(MAIN_DISPLAY(get_global_state()),
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
  entries[0].client_data = (XtPointer)w;
  n = 0;
  XtSetArg(args[n], XmNdropTransfers, entries); n++;
  XtSetArg(args[n], XmNnumDropTransfers, 1); n++;
  XtSetArg(args[n], XmNtransferProc, massage_selection); n++;
  cb->operation = XmDROP_COPY;
  XmDropTransferStart(cb->dragContext, args, n);
}

void add_drop(snd_state *ss, Widget w)
{
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
  XtSetArg(args[n], XmNdropProc, handle_drop); n++;
  XmDropSiteRegister(w, args, n);
}

void g_init_gxdrop(void)
{
  #define H_drop_hook S_drop_hook " (filename): called whenever Snd receives a drag-and-drop \
event. If it returns #t, the file is not opened or mixed by Snd."

  XEN_DEFINE_HOOK(drop_hook, S_drop_hook, 1, H_drop_hook); /* arg = filename */
}
