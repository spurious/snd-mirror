#include "snd.h"

/* can't move axes if icon dragged to end of graph because the entire system freezes! */

/* TODO: multichan mix/target: autosync, autoscale, etc */
/* SOMEDAY: figure out how to include the incoming filename in the drag report */

static Atom FILE_NAME;               /* Sun uses this, SGI uses STRING */
static Atom COMPOUND_TEXT;           /* various Motif widgets use this and the next */
static Atom _MOTIF_COMPOUND_STRING;
static Atom text_plain;              /* gtk uses this */

static XEN drop_hook;

static char *atom_to_filename(Atom type, XtPointer value, unsigned long length)
{
  unsigned long i;
  char *str = NULL;
  if ((type == XA_STRING) || (type == FILE_NAME) || (type == text_plain))
    {
      str = (char *)CALLOC(length + 1, sizeof(char));
      for (i = 0; i < length; i++)
	{
	  if (((char *)value)[i] == ' ')
	    {
	      str[i] = '\0';
	      break;
	    }
	  else str[i] = ((char *)value)[i];
	}
      str[length] = '\0';
    }
  else
    {
      if ((type == COMPOUND_TEXT) || (type == _MOTIF_COMPOUND_STRING))
	{
	  char *temp;
	  XmString cvt;
	  if (type == _MOTIF_COMPOUND_STRING)
	    cvt = XmCvtByteStreamToXmString((unsigned char *)value);
	  else cvt = XmCvtCTToXmString((char *)value);
	  temp = (char *)XmStringUnparse(cvt, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
	  XmStringFree(cvt);
	  str = copy_string(temp);
	  XtFree(temp);
	}
    }
  return(str);
}

static Position mx, my;

static void massage_selection(Widget w, XtPointer context, Atom *selection, Atom *type, XtPointer value, unsigned long *length, int *format)
{
  char *str = NULL;
  snd_state *ss;
  snd_info *sp = NULL;
  Widget caller;
  str = atom_to_filename(*type, value, *length);
  if (str)
    {
      if ((!(XEN_HOOKED(drop_hook))) || 
	  (!(XEN_TRUE_P(run_or_hook(drop_hook,
				    XEN_LIST_1(C_TO_XEN_STRING(str)),
				    "drop")))))
	{
	  ss = get_global_state();
	  caller = (Widget)((XmDropTransferEntry)context)->client_data;
	  if (XmIsRowColumn(caller)) /* top menuBar widget or top level menu */
	    {
	      sp = snd_open_file(str, ss, FALSE);
	      if (sp) select_channel(sp, 0);
	    }
	  else
	    {
	      if (XmIsDrawingArea(caller)) /* channel graph */
		{
		  int data;
		  XtVaGetValues(caller, XmNuserData, &data, NULL);
		  mix_at_x(ss, data, str, mx);
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
	(targets[i] == FILE_NAME) ||
	(targets[i] == COMPOUND_TEXT) ||
	(targets[i] == _MOTIF_COMPOUND_STRING) ||
	(targets[i] == text_plain))
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
  mx = cb->x;
  my = cb->y;
  n = 0;
  XtSetArg(args[n], XmNdropTransfers, entries); n++;
  XtSetArg(args[n], XmNnumDropTransfers, 1); n++;
  XtSetArg(args[n], XmNtransferProc, massage_selection); n++;
  cb->operation = XmDROP_COPY;
  XmDropTransferStart(cb->dragContext, args, n);
}

static void report_mouse_position_as_seconds(Widget w, Position x)
{
  snd_state *ss;
  snd_info *sp;
  chan_info *cp;
  int data, snd, chn;
  float seconds;
  ss = get_global_state();
  XtVaGetValues(w, XmNuserData, &data, NULL);
  chn = UNPACK_CHANNEL(data);
  snd = UNPACK_SOUND(data);
  sp = ss->sounds[snd];
  cp = sp->chans[chn];
  seconds = (float)(ungrf_x(cp->axis, x));
  if (seconds < 0.0) seconds = 0.0;
  if (sp->nchans > 1)
    report_in_minibuffer(sp, "drop to mix file in chan %d at %.4f", chn + 1, seconds);
  else report_in_minibuffer(sp, "drop to mix file at %.4f", seconds);
}

static void clear_minibuffer_of(Widget w)
{
  snd_state *ss;
  int snd, data;
  ss = get_global_state();
  XtVaGetValues(w, XmNuserData, &data, NULL);
  snd = UNPACK_SOUND(data);
  clear_minibuffer(ss->sounds[snd]);
}

static void handle_drag(Widget w, XtPointer context, XtPointer info)
{
  XmDragProcCallbackStruct *cb = (XmDragProcCallbackStruct *)info;
  int is_menubar;
  snd_state *ss;
  is_menubar = XmIsRowColumn(w);
  ss = get_global_state();
  switch(cb->reason)
    { 
    case XmCR_DROP_SITE_MOTION_MESSAGE:
      if (!is_menubar)
	report_mouse_position_as_seconds(w, cb->x);
      break;
    case XmCR_DROP_SITE_ENTER_MESSAGE:
      if (is_menubar)
	{
	  char *new_title;
	  new_title = (char *)CALLOC(64, sizeof(char));
	  sprintf(new_title, "%s: drop to open file", ss->startup_title);
	  XtVaSetValues(MAIN_SHELL(ss), XmNtitle, (char*)new_title, NULL);
	  XmChangeColor(w, ss->sgx->pushed_button_color);
	  FREE(new_title);
	}
      else report_mouse_position_as_seconds(w, cb->x);
      break;
    case XmCR_DROP_SITE_LEAVE_MESSAGE: 
      if (is_menubar)
	{
	  reflect_file_change_in_title(ss);
	  XmChangeColor(w, ss->sgx->highlight_color);
	}
      else clear_minibuffer_of(w);
      break;
    }
}

void add_drop(snd_state *ss, Widget w)
{
  Display *dpy;
  int n;
  Atom targets[5];
  Arg args[12];
  dpy = MAIN_DISPLAY(ss);
  targets[0] = XA_STRING;
  FILE_NAME = XInternAtom(dpy, "FILE_NAME", FALSE);
  targets[1] = FILE_NAME;
  COMPOUND_TEXT = XInternAtom(dpy, "COMPOUND_TEXT", FALSE);
  targets[2] = COMPOUND_TEXT;
  _MOTIF_COMPOUND_STRING = XInternAtom(dpy, "_MOTIF_COMPOUND_STRING", FALSE);
  targets[3] = _MOTIF_COMPOUND_STRING;
  text_plain = XInternAtom(dpy, "text/plain", FALSE);
  targets[4] = text_plain;
  n = 0;
  XtSetArg(args[n], XmNdropSiteOperations, XmDROP_COPY); n++;
  XtSetArg(args[n], XmNimportTargets, targets); n++;
  XtSetArg(args[n], XmNnumImportTargets, 5); n++;
  XtSetArg(args[n], XmNdropProc, handle_drop); n++;
  XtSetArg(args[n], XmNdragProc, handle_drag); n++;
  XmDropSiteRegister(w, args, n);
}

void g_init_gxdrop(void)
{
  #define H_drop_hook S_drop_hook " (filename): called whenever Snd receives a drag-and-drop \
event. If it returns #t, the file is not opened or mixed by Snd."

  XEN_DEFINE_HOOK(drop_hook, S_drop_hook, 1, H_drop_hook); /* arg = filename */
}
