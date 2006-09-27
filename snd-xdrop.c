#include "snd.h"

/* -------- drop watcher lists -------- */

/* the rigamarole for dealing with a drop is so messed up that I think it's
 *   worth the trouble of setting up a separate callback list -- hence the
 *   drop watchers
 */

typedef struct {
  void (*drop_watcher)(Widget w, const char *message, Position x, Position y, void *data);
  void (*drag_watcher)(Widget w, const char *message, Position x, Position y, drag_style_t dtype, void *data);
  Widget caller;
  void *context;
} drop_watcher_t;

static drop_watcher_t **drop_watchers = NULL;
static int drop_watchers_size = 0;

#define DROP_WATCHER_SIZE_INCREMENT 2

static int add_drop_watcher(Widget w, 
			    void (*drop_watcher)(Widget w, const char *message, Position x, Position y, void *data), 
			    void (*drag_watcher)(Widget w, const char *message, Position x, Position y, drag_style_t dtype, void *data), 
			    void *context)
{
  int loc = -1;
  if (!(drop_watchers))
    {
      loc = 0;
      drop_watchers_size = DROP_WATCHER_SIZE_INCREMENT;
      drop_watchers = (drop_watcher_t **)CALLOC(drop_watchers_size, sizeof(drop_watcher_t *));
    }
  else
    {
      int i;
      for (i = 0; i < drop_watchers_size; i++)
	if (!(drop_watchers[i]))
	  {
	    loc = i;
	    break;
	  }
      if (loc == -1)
	{
	  loc = drop_watchers_size;
	  drop_watchers_size += DROP_WATCHER_SIZE_INCREMENT;
	  drop_watchers = (drop_watcher_t **)REALLOC(drop_watchers, drop_watchers_size * sizeof(drop_watcher_t *));
	  for (i = loc; i < drop_watchers_size; i++) drop_watchers[i] = NULL;
	}
    }
  drop_watchers[loc] = (drop_watcher_t *)CALLOC(1, sizeof(drop_watcher_t));
  drop_watchers[loc]->drop_watcher = drop_watcher;
  drop_watchers[loc]->drag_watcher = drag_watcher;
  drop_watchers[loc]->context = context;
  drop_watchers[loc]->caller = w;
  return(loc);
}

#if 0
static bool remove_drop_watcher(int loc)
{
  if ((drop_watchers) &&
      (loc < drop_watchers_size) &&
      (loc >= 0) &&
      (drop_watchers[loc]))
    {
      FREE(drop_watchers[loc]);
      drop_watchers[loc] = NULL;
      return(true);
    }
  return(false);
}
#endif

static drop_watcher_t *find_drop_watcher(Widget caller)
{
  if (drop_watchers)
    {
      int i;
      for (i = 0; i < drop_watchers_size; i++)
	{
	  if (drop_watchers[i])
	    {
	      drop_watcher_t *d;
	      d = drop_watchers[i];
	      if (d->caller == caller)
		return(d);
	    }
	}
    }
  return(NULL);
}



/* can't move axes if icon dragged to end of graph because the entire system freezes! */

static Atom FILE_NAME;               /* Sun uses this, SGI uses STRING */
static Atom COMPOUND_TEXT;           /* various Motif widgets use this and the next */
static Atom _MOTIF_COMPOUND_STRING;
static Atom text_plain;              /* gtk uses this -- apparently a url */
static Atom uri_list;                /* rox uses this -- looks just like text/plain to me */
static Atom TEXT;                    /* ditto */

static XEN drop_hook;

static char *atom_to_string(Atom type, XtPointer value, unsigned long length)
{
  unsigned long i;
  char *str = NULL;
  if ((type == XA_STRING) || (type == FILE_NAME) || (type == text_plain) || (type == uri_list) || (type == TEXT))
    {
      str = (char *)CALLOC(length + 1, sizeof(char));
      for (i = 0; i < length; i++)
	str[i] = ((char *)value)[i];
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
  str = atom_to_string(*type, value, *length);
  /* str can contain more than one name (separated by cr) */
  if (str)
    {
      if ((!(XEN_HOOKED(drop_hook))) || 
	  (!(XEN_TRUE_P(run_or_hook(drop_hook,
				    XEN_LIST_1(C_TO_XEN_STRING(str)),
				    S_drop_hook)))))
	{
	  Widget caller; /* "w" above is the transfer control widget, not the drop-receiver */
	  drop_watcher_t *d;
	  caller = (Widget)((XmDropTransferEntry)context)->client_data;
	  d = find_drop_watcher(caller);
	  if (d)
	    {
	      /* loop through possible list of filenames, calling watcher on each */
	      char *filename;
	      int len = 0, i, j = 0;
	      len = snd_strlen(str);
	      filename = (char *)CALLOC(len, sizeof(char));
	      for (i = 0; i < len; i++)
		{
		  if ((str[i] == '\n') || (str[i] == '\r')) /* apparently the only space chars here are \n and \r? */
		    {
		      if (j > 0)
			{
			  filename[j] = '\0';
			  if (strncmp(filename, "file://", 7) == 0)
			    {
			      char *tmp;
			      tmp = (char *)(filename + 7);
			      (*(d->drop_watcher))(caller, (const char *)tmp, mx, my, d->context);
			    }
			  else (*(d->drop_watcher))(caller, (const char *)filename, mx, my, d->context);
			  j = 0;
			}
		      /* else ignore extra white space chars */
		    }
		  else
		    {
		      filename[j++] = str[i];
		    }
		}
	      FREE(filename);
	    }
	}
      FREE(str);
    }
}

static void handle_drop(Widget w, XtPointer context, XtPointer info) 
{
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
	(targets[i] == text_plain) ||
	(targets[i] == uri_list))
      {
	k = i; 
	break;
      }
  if (k == -1)
    {
#if MUS_DEBUGGING
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
  
  mx = cb->x;
  my = cb->y;
  entries[0].target = targets[k];
  entries[0].client_data = (XtPointer)w;
  n = 0;
  XtSetArg(args[n], XmNdropTransfers, entries); n++;
  XtSetArg(args[n], XmNnumDropTransfers, 1); n++;
  XtSetArg(args[n], XmNtransferProc, massage_selection); n++;
  /* cb->operation = XmDROP_COPY; */

  XmDropTransferStart(cb->dragContext, args, n);
}

static void handle_drag(Widget w, XtPointer context, XtPointer info)
{
  XmDragProcCallbackStruct *cb = (XmDragProcCallbackStruct *)info;
  drop_watcher_t *d;
  d = find_drop_watcher(w);
  if ((d) && (d->drag_watcher))
    {
      switch(cb->reason)
	{ 
	case XmCR_DROP_SITE_MOTION_MESSAGE:
	  (*(d->drag_watcher))(w, NULL, cb->x, cb->y, DRAG_MOTION, d->context);
	  break;
	case XmCR_DROP_SITE_ENTER_MESSAGE:
	  (*(d->drag_watcher))(w, NULL, cb->x, cb->y, DRAG_ENTER, d->context);
	  break;
	case XmCR_DROP_SITE_LEAVE_MESSAGE: 
	  (*(d->drag_watcher))(w, NULL, cb->x, cb->y, DRAG_LEAVE, d->context);
	  break;
	}
    }
}

#define NUM_TARGETS 7
void add_drag_and_drop(Widget w, 
		       void (*drop_watcher)(Widget w, const char *message, Position x, Position y, void *data), 
		       void (*drag_watcher)(Widget w, const char *message, Position x, Position y, drag_style_t dtype, void *data), 
		       void *context)
{
  Display *dpy;
  int n;
  Atom targets[NUM_TARGETS];
  Arg args[12];
  dpy = MAIN_DISPLAY(ss);
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
  uri_list = XInternAtom(dpy, "text/uri-list", false);
  targets[6] = uri_list;
  n = 0;
  XtSetArg(args[n], XmNdropSiteOperations, XmDROP_COPY | XmDROP_LINK); n++;
  XtSetArg(args[n], XmNimportTargets, targets); n++;
  XtSetArg(args[n], XmNnumImportTargets, NUM_TARGETS); n++;
  XtSetArg(args[n], XmNdropProc, handle_drop); n++;
  XtSetArg(args[n], XmNdragProc, handle_drag); n++;
  XmDropSiteRegister(w, args, n);
  add_drop_watcher(w, drop_watcher, drag_watcher, context);
}

void g_init_gxdrop(void)
{
  #define H_drop_hook S_drop_hook " (filename): called whenever Snd receives a drag-and-drop \
event. If it returns " PROC_TRUE ", the file is not opened or mixed by Snd."

  drop_hook = XEN_DEFINE_HOOK(S_drop_hook, 1, H_drop_hook); /* arg = filename */
}
