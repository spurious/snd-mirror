#include "snd.h"

/* can't move axes if icon dragged to end of graph because the entire system freezes! */

static Atom FILE_NAME;               /* Sun uses this, SGI uses STRING */
static Atom COMPOUND_TEXT;           /* various Motif widgets use this and the next */
static Atom _MOTIF_COMPOUND_STRING;
static Atom text_plain;              /* gtk uses this -- untested here */
static Atom TEXT;                    /* ditto */

#define WITH_DRAG_CONVERSION 0

#if WITH_DRAG_CONVERSION
static Atom _MOTIF_DROP;             /* to query in-coming drag for filename */
#endif

static XEN drop_hook;

static char *atom_to_filename(Atom type, XtPointer value, unsigned long length)
{
  unsigned long i;
  char *str = NULL;
  if ((type == XA_STRING) || (type == FILE_NAME) || (type == text_plain) || (type == TEXT))
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
	  XmString cvt, tmp;
	  XmParseTable parser = (XmParseTable)XtCalloc(1, sizeof(XmParseMapping));
	  int n;
	  Arg args[12];

	  /* create parse table to catch separator in XmString and insert "\n" in output */
	  /*   multiple file names are passed this way in Motif */
	  tmp = XmStringSeparatorCreate();
	  n = 0;
	  XtSetArg(args[n], XmNincludeStatus, XmINSERT); n++;
	  XtSetArg(args[n], XmNsubstitute, tmp); n++;
	  XtSetArg(args[n], XmNpattern, "\n"); n++;
	  parser[0] = XmParseMappingCreate(args, n);

	  if (type == _MOTIF_COMPOUND_STRING)
	    cvt = XmCvtByteStreamToXmString((unsigned char *)value);
	  else cvt = XmCvtCTToXmString((char *)value);
	  temp = (char *)XmStringUnparse(cvt, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, parser, 1, XmOUTPUT_ALL);

	  XmParseTableFree(parser, 1);
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
  str = atom_to_filename(*type, value, *length);
  /* str can contain more than one name (separated by cr) */
  if (str)
    {
      if ((!(XEN_HOOKED(drop_hook))) || 
	  (!(XEN_TRUE_P(run_or_hook(drop_hook,
				    XEN_LIST_1(C_TO_XEN_STRING(str)),
				    "drop")))))
	{
	  snd_info *sp = NULL;
	  Widget caller;
	  caller = (Widget)((XmDropTransferEntry)context)->client_data;
	  if (XmIsRowColumn(caller)) /* top menuBar widget or top level menu */
	    {
	      ss->open_requestor = FROM_DRAG_AND_DROP;
	      /* TODO: handle mult-file request her and below */
	      sp = snd_open_file(str, FILE_READ_WRITE);
	      if (sp) select_channel(sp, 0);
	    }
	  else
	    {
	      if (XmIsDrawingArea(caller)) /* channel graph */
		{
		  int data;
		  XtVaGetValues(caller, XmNuserData, &data, NULL);
		  mix_at_x_y(data, str, mx, my);
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
      ((cb->operation != XmDROP_COPY) &&
       (cb->operation != XmDROP_LINK)))
    {
      cb->dropSiteStatus = XmINVALID_DROP_SITE;
      return;
    }

  k = -1;
  XtVaGetValues(cb->dragContext, 
		XmNexportTargets, &targets, 
		XmNnumExportTargets, &num_targets, 
		NULL);

  for (i = 0; i < num_targets; i++) 
    if ((targets[i] == XA_STRING) || 
	(targets[i] == FILE_NAME) ||
	(targets[i] == COMPOUND_TEXT) ||
	(targets[i] == _MOTIF_COMPOUND_STRING) ||
	(targets[i] == TEXT) ||
	(targets[i] == text_plain)) /* gtk apparently, also UTF8_STRING */
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
		XGetAtomName(MAIN_DISPLAY(ss),
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
  /* cb->operation = XmDROP_COPY; */

  XmDropTransferStart(cb->dragContext, args, n);
}

static void report_mouse_position_as_seconds(Widget w, const char *file, Position x, Position y)
{
  snd_info *sp;
  chan_info *cp;
  int data, snd, chn;
  float seconds;
  XtVaGetValues(w, XmNuserData, &data, NULL);
  chn = UNPACK_CHANNEL(data);
  snd = UNPACK_SOUND(data);
  sp = ss->sounds[snd];
  cp = sp->chans[chn];
  if ((sp->nchans > 1) && (sp->channel_style == CHANNELS_COMBINED))
    cp = which_channel(sp, y);    
  seconds = (float)(ungrf_x(cp->axis, x));
  if (seconds < 0.0) seconds = 0.0;
  if (sp->nchans > 1)
    report_in_minibuffer(sp, "drop to mix %s in chan %d at %.4f", file, cp->chan + 1, seconds);
  else report_in_minibuffer(sp, "drop to mix %s at %.4f", file, seconds);
}

static void clear_minibuffer_of(Widget w)
{
  int snd, data;
  XtVaGetValues(w, XmNuserData, &data, NULL);
  snd = UNPACK_SOUND(data);
  clear_minibuffer(ss->sounds[snd]);
}

static char *current_file = NULL; /* used only in the broken WITH_DRAG_CONVERSION code */

static void handle_drag(Widget w, XtPointer context, XtPointer info)
{
  XmDragProcCallbackStruct *cb = (XmDragProcCallbackStruct *)info;
  bool is_menubar;
  is_menubar = XmIsRowColumn(w);
  switch(cb->reason)
    { 
    case XmCR_DROP_SITE_MOTION_MESSAGE:
      if (!is_menubar)
	report_mouse_position_as_seconds(w, (current_file) ? current_file : "file", cb->x, cb->y);
      break;
    case XmCR_DROP_SITE_ENTER_MESSAGE:
#if WITH_DRAG_CONVERSION
      /* this code does get the filename from the drag context, but after the first such transfer
       * it starts complaining 
       *
       *   Xt warning: xtGetSelectionRequest, notInConvertSelection: 
       *   XtGetSelectionRequest or XtGetSelectionParameters called for widget "dragContext" outside of ConvertSelection proc
       *
       * and eventually (after a half-dozen drags), the entire machine freezes.  In other cases,
       * we die with an immediate segfault in XtGetSelectionRequest
       *
       * See comment above as well -- I'd say drag-and-drop is not very robust in Xt!
       */
      {
	int i, num_targets, k, format = 0;
	Boolean ok;
	Atom *targets;
	XtPointer value;
	Atom type_returned;
	XtConvertSelectionIncrProc proc;
	unsigned long len = 0, maxlen = 0;
	if (cb->dragContext)
	  {
	    XtVaGetValues(cb->dragContext, XmNexportTargets, &targets, XmNnumExportTargets, &num_targets, NULL);
	    if (num_targets > 0)
	      {
		XtVaGetValues(cb->dragContext, XmNconvertProc, &proc, NULL);
		if (proc != NULL)
		  {
		    k = -1;
		    for (i = 0; i < num_targets; i++) 
		      if ((targets[i] == XA_STRING) || 
			  (targets[i] == FILE_NAME) ||
			  (targets[i] == COMPOUND_TEXT) ||
			  (targets[i] == TEXT) ||
			  (targets[i] == _MOTIF_COMPOUND_STRING) ||
			  (targets[i] == text_plain))
			{
			  k = i; 
			  break;
			}
		    if (k != -1)
		      {
			XtVaSetValues(cb->dragContext, XmNincremental, false, NULL); /* see Motif docs for XmDragContext */
			ok = (*proc)(cb->dragContext, &_MOTIF_DROP, &(targets[i]), &type_returned, &value, &len, &format, &maxlen, NULL, 0);
			if (ok)
			  current_file = just_filename(atom_to_filename(type_returned, value, len));
		      }
		}
	    }
	  }
      }
#endif
      if (is_menubar)
	{
	  char *new_title;
	  new_title = (char *)CALLOC(64, sizeof(char));
	  sprintf(new_title, "%s: drop to open %s", ss->startup_title, (current_file) ? current_file : "file");
	  XtVaSetValues(MAIN_SHELL(ss), XmNtitle, (char*)new_title, NULL);
	  if (!(ss->using_schemes)) XmChangeColor(w, ss->sgx->pushed_button_color);
	  FREE(new_title);
	}
      else report_mouse_position_as_seconds(w, (current_file) ? current_file : "file", cb->x, cb->y);
      break;
    case XmCR_DROP_SITE_LEAVE_MESSAGE: 
      if (is_menubar)
	{
	  reflect_file_change_in_title();
	  if (!(ss->using_schemes)) XmChangeColor(w, ss->sgx->highlight_color);
	}
      else clear_minibuffer_of(w);
      if (current_file)
	{
	  FREE(current_file);
	  current_file = NULL;
	}
      break;
    }
}

#define NUM_TARGETS 6
void add_drop(Widget w)
{
  Display *dpy;
  int n;
  Atom targets[NUM_TARGETS];
  Arg args[12];
  dpy = MAIN_DISPLAY(ss);
#if WITH_DRAG_CONVERSION
  _MOTIF_DROP = XInternAtom(dpy, "_MOTIF_DROP", false);
#endif
  targets[0] = XA_STRING;
  FILE_NAME = XInternAtom(dpy, "FILE_NAME", false);
  targets[1] = FILE_NAME;
  COMPOUND_TEXT = XInternAtom(dpy, "COMPOUND_TEXT", false);
  targets[2] = COMPOUND_TEXT;
  _MOTIF_COMPOUND_STRING = XInternAtom(dpy, "_MOTIF_COMPOUND_STRING", false);
  targets[3] = _MOTIF_COMPOUND_STRING;
  text_plain = XInternAtom(dpy, "text/plain", false);
  targets[4] = text_plain;
  TEXT = XInternAtom(dpy, "TEXT", false);
  targets[5] = TEXT;
  /* TODO: could we add audio/wav -- what are the audio types in this context? */
  /* TODO: add support for array of strings/filenames -- what is the type? */
  n = 0;
  XtSetArg(args[n], XmNdropSiteOperations, XmDROP_COPY); n++;
  XtSetArg(args[n], XmNimportTargets, targets); n++;
  XtSetArg(args[n], XmNnumImportTargets, NUM_TARGETS); n++;
  XtSetArg(args[n], XmNdropProc, handle_drop); n++;
  XtSetArg(args[n], XmNdragProc, handle_drag); n++;
  XmDropSiteRegister(w, args, n);
}

void g_init_gxdrop(void)
{
  #define H_drop_hook S_drop_hook " (filename): called whenever Snd receives a drag-and-drop \
event. If it returns #t, the file is not opened or mixed by Snd."

  drop_hook = XEN_DEFINE_HOOK(S_drop_hook, 1, H_drop_hook); /* arg = filename */
}
