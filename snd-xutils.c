#include "snd.h"

#include <X11/IntrinsicP.h>

#if __GNUC__
#ifdef LESSTIF_VERSION
  /* moved the warning here so it only is displayed once */
  #warning You appear to be using Lesstif: this is not recommended!  Expect bugs...
#endif
#endif

#if (USE_RENDITIONS)
static XmRenderTable get_xm_font(XFontStruct *ignore, char *font, char *tag)
{
  XmRendition tmp;
  XmRenderTable tabl;
  int n;
  Arg args[12];
  n = 0;
  XtSetArg(args[n], XmNfontName, font); n++;
  XtSetArg(args[n], XmNfontType, XmFONT_IS_FONT); n++; 
  XtSetArg(args[n], XmNloadModel, XmLOAD_IMMEDIATE); n++;
  tmp = XmRenditionCreate(MAIN_SHELL(ss), tag, args, n);
  tabl = XmRenderTableAddRenditions(NULL, &tmp, 1, XmMERGE_NEW);
  XmRenditionFree(tmp);
  return(tabl);
}
#else
static XmFontList get_xm_font(XFontStruct *fs, char *font, char *tag)
{
  XmFontList fl = NULL;
  XmFontListEntry e1;
  e1 = XmFontListEntryCreate(tag, XmFONT_IS_FONT, (XtPointer)fs);
  fl = XmFontListAppendEntry(NULL, e1);
  XmFontListEntryFree(&e1);
  return(fl);
}
#endif

bool set_tiny_font(char *font)
{
  XFontStruct *fs = NULL;
  state_context *sgx;
  sgx = ss->sgx;
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), font);
  if (fs)
    {
      if (tiny_font(ss)) FREE(tiny_font(ss));
      in_set_tiny_font(copy_string(font));
      sgx->tiny_fontstruct = fs;
      if (sgx->tiny_fontlist) XM_FONT_FREE(sgx->tiny_fontlist);
      sgx->tiny_fontlist = get_xm_font(sgx->tiny_fontstruct, font, "tiny_font");
      return(true);
    }
  return(false);
}

bool set_listener_font(char *font)
{
  XFontStruct *fs = NULL;
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), font);
  if (fs)
    {
      if (listener_font(ss)) FREE(listener_font(ss));
      in_set_listener_font(copy_string(font));
      (ss->sgx)->listener_fontstruct = fs;
      if ((ss->sgx)->listener_fontlist) XM_FONT_FREE((ss->sgx)->listener_fontlist);
      (ss->sgx)->listener_fontlist = get_xm_font((ss->sgx)->listener_fontstruct, font, "listener_font");
      set_listener_text_font();
      return(true);
    }
  return(false);
}

bool set_peaks_font(char *font)
{
  XFontStruct *fs = NULL;
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), font);
  if (fs)
    {
      if (peaks_font(ss)) FREE(peaks_font(ss));
      in_set_peaks_font(copy_string(font));
      (ss->sgx)->peaks_fontstruct = fs;
      if ((ss->sgx)->peaks_fontlist) XM_FONT_FREE((ss->sgx)->peaks_fontlist);
      (ss->sgx)->peaks_fontlist = get_xm_font((ss->sgx)->peaks_fontstruct, font, "peaks_font");
      return(true);
    }
  return(false);
}

bool set_bold_peaks_font(char *font)
{
  XFontStruct *fs = NULL;
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), font);
  if (fs)
    {
      if (bold_peaks_font(ss)) FREE(bold_peaks_font(ss));
      in_set_bold_peaks_font(copy_string(font));
      (ss->sgx)->bold_peaks_fontstruct = fs;
      if ((ss->sgx)->bold_peaks_fontlist) XM_FONT_FREE((ss->sgx)->bold_peaks_fontlist);
      (ss->sgx)->bold_peaks_fontlist = get_xm_font((ss->sgx)->bold_peaks_fontstruct, font, "bold_peaks_font");
      return(true);
    }
  return(false);
}

bool set_axis_label_font(char *font)
{
  XFontStruct *fs = NULL;
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), font);
  if (fs)
    {
      if (axis_label_font(ss)) FREE(axis_label_font(ss));
      in_set_axis_label_font(copy_string(font));
      (ss->sgx)->axis_label_fontstruct = fs;
#if HAVE_GL
      reload_label_font();
#endif
      return(true);
    }
  return(false);
}

bool set_axis_numbers_font(char *font)
{
  XFontStruct *fs = NULL;
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), font);
  if (fs)
    {
      if (axis_numbers_font(ss)) FREE(axis_numbers_font(ss));
      in_set_axis_numbers_font(copy_string(font));
      (ss->sgx)->axis_numbers_fontstruct = fs;
#if HAVE_GL
      reload_number_font();
#endif
      return(true);
    }
  return(false);
}

int label_width(char *txt)
{
  if (txt)
    return(XTextWidth(AXIS_LABEL_FONT(ss), txt, strlen(txt)));
  else return(0);
}

int mark_name_width(char *txt)
{
  if (txt)
    return(XTextWidth(ss->sgx->peaks_fontstruct, txt, strlen(txt)));
  return(0);
}

int number_width(char *num)
{
  if (num)
    return(XTextWidth(AXIS_NUMBERS_FONT(ss), num, strlen(num)));
  return(0);
}

int number_height(void)
{
  XFontStruct *numbers_font;
  numbers_font = AXIS_NUMBERS_FONT(ss);
  return(numbers_font->ascent + numbers_font->descent);
}

int label_height(void)
{
  XFontStruct *label_font;
  label_font = AXIS_LABEL_FONT(ss);
  return(label_font->ascent + label_font->descent);
}

void clear_window(axis_context *ax)
{
  if (ax) XClearWindow(ax->dp, ax->wn);
}

void map_over_children (Widget w, void (*func)(Widget, void *), void *userptr)
{
  /* apply func to each child in entire tree beneath top widget */
  /* taken from Douglas Young, "Motif Debugging and Performance Tuning" Prentice-Hall 1995 */
  /* used mostly to get colors right in non-SGI environments with "convenience" widgets */
  unsigned int i;
  if (w)
    {
      (*func)(w, userptr);
      if (XtIsComposite(w))
	{
	  CompositeWidget cw = (CompositeWidget)w;
	  for (i = 0; i < cw->composite.num_children; i++)
	    map_over_children(cw->composite.children[i], func, userptr);
	}
      if (XtIsWidget(w))
	{
	  for (i = 0; i < w->core.num_popups; i++)
	    {
	      Widget child = w->core.popup_list[i];
	      map_over_children(child, func, userptr);
	    }
	}
    }
}

void raise_dialog(Widget w)
{
  /* since we're using non-transient message dialogs, the dialog window can become completely
   * hidden behind other windows, with no easy way to raise it back to the top, so...
   */
  Widget parent;
  if ((w) && (XtIsManaged(w)))
    {
      parent = XtParent(w);
      if ((parent) && 
	  (XtIsSubclass(parent, xmDialogShellWidgetClass)))
	XtPopup(parent, XtGrabNone);
      /* XtGrabNone means don't lock out events to rest of App (i.e. modeless dialog) */
    }
}

void set_main_color_of_widget (Widget w, void *userptr)
{
  if (XtIsWidget(w))
    {
      if (XmIsScrollBar(w)) 
	XmChangeColor(w, (Pixel)(ss->sgx)->position_color);
      else 
	{
	  Pixel cur_color;
	  XtVaGetValues(w, XmNbackground, &cur_color, NULL);
	  if ((cur_color != ss->sgx->quit_button_color) &&
	      (cur_color != ss->sgx->help_button_color) &&
	      (cur_color != ss->sgx->doit_button_color) &&
	      (cur_color != ss->sgx->doit_again_button_color) &&
	      (cur_color != ss->sgx->reset_button_color) &&
	      (cur_color != ss->sgx->highlight_color))
	    XmChangeColor(w, (Pixel)(ss->sgx)->basic_color);
	}
    }
}

void set_label(Widget label, const char *str)
{
  XmString s1;
  s1 = XmStringCreate((char *)str, XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(label, XmNlabelString, s1, NULL);
  XmStringFree(s1);
}

void set_button_label (Widget label, const char *str) {set_label(label, str);}

void set_title(const char *title)
{
  XtVaSetValues(MAIN_SHELL(ss), XmNtitle, (char*)title, NULL);
}

static bool complain_about_focus_policy = true;

void goto_window(Widget text)
{
  int err;
  if ((XmIsTraversable(text)) && (1))
    /*
      there's a major memory leak here
      (XmGetVisibility(text) != XmVISIBILITY_FULLY_OBSCURED))
    */
    {
      if (!(XmProcessTraversal(text, XmTRAVERSE_CURRENT)))
	{
	  if (complain_about_focus_policy)
	    {
	      XtVaGetValues(text, XmNkeyboardFocusPolicy, &err, NULL);
	      if (err == XmEXPLICIT)
		snd_error("goto_window: traverse to %s failed!", XtName(text));
	      else 
		{
		  snd_error("goto_window: keyboard focus policy is not explicit!");
		  complain_about_focus_policy = false;
		}
	    }
	}
    }
}

XtCallbackList make_callback_list(XtCallbackProc callback, XtPointer closure)
{
  XtCallbackList nlist;
  nlist = (XtCallbackList)CALLOC(2, sizeof(XtCallbackRec));
  nlist[0].callback = callback;
  nlist[0].closure = closure;
  nlist[1].callback = NULL;
  nlist[1].closure = NULL;
  return(nlist);
}

#include <Xm/SashP.h>
void color_sashes(Widget w, void *ptr)
{
  if ((XtIsWidget(w)) && 
      (XtIsManaged(w)) && 
      (XtIsSubclass(w, xmSashWidgetClass)))
    XmChangeColor(w, (Pixel)(ss->sgx)->sash_color);
}

void check_for_event(void)
{
  /* this is needed to force label updates and provide interrupts from long computations */
  XEvent event;
  XtInputMask msk = 0;
  XtAppContext app;
  if (ss->checking_explicitly) return;
  ss->checking_explicitly = true;
  app = MAIN_APP(ss);
  while (true)
    {
      msk = XtAppPending(app);
      if (msk & (XtIMXEvent | XtIMAlternateInput))
	{
	  XtAppNextEvent(app, &event);
	  XtDispatchEvent(&event);
	  /* widget = XtWindowToWidget(event.xany.display, event.xany.window); */
	}
      else break;
    }
  ss->checking_explicitly = false;
}

bool event_pending(void)
{
  XtInputMask msk = 0;
  XtAppContext app;
  app = MAIN_APP(ss);
  msk = XtAppPending(app);
  return((bool)(msk & XtIMXEvent));
}

void color_cursor(Pixel color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->cursor_color = color;
  XSetForeground(MAIN_DISPLAY(ss), sx->cursor_gc, (Pixel)(XOR(color, sx->graph_color)));
  XSetForeground(MAIN_DISPLAY(ss), sx->selected_cursor_gc, (Pixel)(XOR(color, sx->selected_graph_color)));
}

void color_marks(Pixel color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->mark_color = color;
  XSetForeground(MAIN_DISPLAY(ss), sx->mark_gc, (Pixel)(XOR(color, sx->graph_color)));
  XSetForeground(MAIN_DISPLAY(ss), sx->selected_mark_gc, (Pixel)(XOR(color, sx->selected_graph_color)));
}

void color_selection(Pixel color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->selection_color = color;
  XSetForeground(MAIN_DISPLAY(ss), sx->selection_gc, (Pixel)(XOR(color, sx->graph_color)));
  XSetForeground(MAIN_DISPLAY(ss), sx->selected_selection_gc, (Pixel)(XOR(color, sx->selected_graph_color)));
}

void color_graph(Pixel color)
{
  Display *dpy;
  state_context *sx;
  dpy = MAIN_DISPLAY(ss);
  sx = ss->sgx;
  sx->graph_color = color;
  XSetBackground(dpy, sx->basic_gc, color);
  XSetForeground(dpy, sx->erase_gc, color);
  XSetForeground(dpy, sx->selection_gc, (Pixel)(XOR(sx->selection_color, color)));
  XSetForeground(dpy, sx->cursor_gc, (Pixel)(XOR(sx->cursor_color, color)));
  XSetForeground(dpy, sx->mark_gc, (Pixel)(XOR(sx->mark_color, color)));
}

void color_selected_graph(Pixel color)
{
  Display *dpy;
  state_context *sx;
  dpy = MAIN_DISPLAY(ss);
  sx = ss->sgx;
  sx->selected_graph_color = color;
  XSetBackground(dpy, sx->selected_basic_gc, color);
  XSetForeground(dpy, sx->selected_erase_gc, color);
  XSetForeground(dpy, sx->selected_selection_gc, (Pixel)(XOR(sx->selection_color, color)));
  XSetForeground(dpy, sx->selected_cursor_gc, (Pixel)(XOR(sx->cursor_color, color)));
  XSetForeground(dpy, sx->selected_mark_gc, (Pixel)(XOR(sx->mark_color, color)));
}

void color_data(Pixel color)
{
  Display *dpy;
  state_context *sx;
  dpy = MAIN_DISPLAY(ss);
  sx = ss->sgx;
  sx->data_color = color;
  XSetForeground(dpy, sx->basic_gc, color);
  XSetBackground(dpy, sx->erase_gc, color);
}

void color_selected_data(Pixel color)
{
  Display *dpy;
  state_context *sx;
  dpy = MAIN_DISPLAY(ss);
  sx = ss->sgx;
  sx->selected_data_color = color;
  XSetForeground(dpy, sx->selected_basic_gc, color);
  XSetBackground(dpy, sx->selected_erase_gc, color);
}

void recolor_graph(chan_info *cp, bool selected)
{
  state_context *sx;
  sx = ss->sgx;
  XtVaSetValues(channel_graph(cp), XmNbackground, (selected) ? sx->selected_graph_color : sx->graph_color, NULL);
}

void set_mix_color(Pixel color)
{
  Display *dpy;
  state_context *sx;
  dpy = MAIN_DISPLAY(ss);
  sx = ss->sgx;
  sx->mix_color = color;
  XSetForeground(dpy, sx->mix_gc, color);
}

#if (!(HAVE_XPM_GET_ERROR_STRING))
char *XpmGetErrorString(int err);
char *XpmGetErrorString(int err) {return("");}
#endif

void set_sensitive(Widget wid, bool val) {if (wid) XtSetSensitive(wid, val);}
bool is_sensitive(Widget wid) {if (wid) return(XtIsSensitive(wid)); return(false);}
void set_toggle_button(Widget wid, bool val, bool passed, void *data) {XmToggleButtonSetState(wid, (Boolean)val, (Boolean)passed);}

Dimension widget_height(Widget w)
{
  Dimension height;
  XtVaGetValues(w, XmNheight, &height, NULL);
  return(height);
}

Dimension widget_width(Widget w)
{
  Dimension width;
  XtVaGetValues(w, XmNwidth, &width, NULL);
  return(width);
}

void set_widget_height(Widget w, Dimension height)
{
  XtVaSetValues(w, XmNheight, height, NULL);
}

void set_widget_width(Widget w, Dimension width)
{
  XtVaSetValues(w, XmNwidth, width, NULL);
}

void set_widget_size(Widget w, Dimension width, Dimension height)
{
  XtVaSetValues(w, XmNwidth, width, XmNheight, height, NULL);
}

Position widget_x(Widget w)
{
  Position x;
  XtVaGetValues(w, XmNx, &x, NULL);
  return(x);
}

Position widget_y(Widget w)
{
  Position y;
  XtVaGetValues(w, XmNy, &y, NULL);
  return(y);
}

void set_widget_x(Widget w, Position x)
{
  XtVaSetValues(w, XmNx, x, NULL);
}

void set_widget_y(Widget w, Position y)
{
  XtVaSetValues(w, XmNy, y, NULL);
}

void set_widget_position(Widget w, Position x, Position y)
{
  XtVaSetValues(w, XmNx, x, XmNy, y, NULL);
}

void fixup_axis_context(axis_context *ax, Widget w, GC gc)
{
  ax->dp = XtDisplay(w);
  ax->wn = XtWindow(w);
  if (gc) ax->gc = gc;
}

Pixmap make_pixmap(unsigned char *bits, int width, int height, int depth, GC gc)
{
  Pixmap rb, nr;
  rb = XCreateBitmapFromData(MAIN_DISPLAY(ss), 
			     RootWindowOfScreen(XtScreen(MAIN_PANE(ss))), 
			     (const char *)bits, 
			     width, height);
  nr = XCreatePixmap(MAIN_DISPLAY(ss), 
		     RootWindowOfScreen(XtScreen(MAIN_PANE(ss))), 
		     width, height, depth);
  XCopyPlane(MAIN_DISPLAY(ss), rb, nr, gc, 0, 0, width, height, 0, 0, 1);
  XFreePixmap(MAIN_DISPLAY(ss), rb);
  return(nr);
}

Cessator add_work_proc(XtWorkProc func, XtPointer data)
{
  /* during auto-testing I need to force the background procs to run to completion */
  if (with_background_processes(ss))
    return(XtAppAddWorkProc(MAIN_APP(ss), func, data));
  else
    {
      while (((*func)(data)) == BACKGROUND_CONTINUE);
      return(0);
    }
}

int attach_all_sides(Arg *args, int n)
{
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  return(n);
}
