#include "snd.h"

#include <X11/IntrinsicP.h>

#if __GNUC__
#ifdef LESSTIF_VERSION
  /* moved the warning here so it only is displayed once */
  #warning You appear to be using Lesstif: this is not recommended!  Expect bugs...
#endif
#endif

#if (USE_RENDITIONS)
static XmRenderTable get_xm_font(snd_state *ss, XFontStruct *fs, char *font, char *tag)
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
static XmFontList get_xm_font(snd_state *ss, XFontStruct *fs, char *font, char *tag)
{
  XmFontList fl = NULL;
  XmFontListEntry e1;
  e1 = XmFontListEntryCreate(tag, XmFONT_IS_FONT, (XtPointer)fs);
  fl = XmFontListAppendEntry(NULL, e1);
  XmFontListEntryFree(&e1);
  return(fl);
}
#endif

int set_help_text_font(snd_state *ss, char *font)
{
  XFontStruct *fs = NULL;
  state_context *sgx;
  sgx = ss->sgx;
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), font);
  if (fs)
    {
      if (help_text_font(ss)) FREE(help_text_font(ss));
      in_set_help_text_font(ss, copy_string(font)); /* may be temporary from caller's viewpoint */
      sgx->help_text_fontstruct = fs;
      if (sgx->help_text_fontlist) XM_FONT_FREE(sgx->help_text_fontlist);
      sgx->help_text_fontlist = get_xm_font(ss, sgx->help_text_fontstruct, font, "help_text_font");
      return(TRUE);
    }
  return(FALSE);
}

int set_tiny_font(snd_state *ss, char *font)
{
  XFontStruct *fs = NULL;
  state_context *sgx;
  sgx = ss->sgx;
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), font);
  if (fs)
    {
      if (tiny_font(ss)) FREE(tiny_font(ss));
      in_set_tiny_font(ss, copy_string(font));
      sgx->tiny_fontstruct = fs;
      if (sgx->tiny_fontlist) XM_FONT_FREE(sgx->tiny_fontlist);
      sgx->tiny_fontlist = get_xm_font(ss, sgx->tiny_fontstruct, font, "tiny_font");
      return(TRUE);
    }
  return(FALSE);
}

int set_listener_font(snd_state *ss, char *font)
{
  XFontStruct *fs = NULL;
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), font);
  if (fs)
    {
      if (listener_font(ss)) FREE(listener_font(ss));
      in_set_listener_font(ss, copy_string(font));
      (ss->sgx)->listener_fontstruct = fs;
      if ((ss->sgx)->listener_fontlist) XM_FONT_FREE((ss->sgx)->listener_fontlist);
      (ss->sgx)->listener_fontlist = get_xm_font(ss, (ss->sgx)->listener_fontstruct, font, "listener_font");
      return(TRUE);
    }
  return(FALSE);
}

int set_button_font(snd_state *ss, char *font)
{
  XFontStruct *fs = NULL;
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), font);
  if (fs)
    {
      if (button_font(ss)) FREE(button_font(ss));
      in_set_button_font(ss, copy_string(font));
      (ss->sgx)->button_fontstruct = fs;
      if ((ss->sgx)->button_fontlist) XM_FONT_FREE((ss->sgx)->button_fontlist);
      (ss->sgx)->button_fontlist = get_xm_font(ss, (ss->sgx)->button_fontstruct, font, "button_font");
      return(TRUE);
    }
  return(FALSE);
}

int set_bold_button_font(snd_state *ss, char *font)
{
  XFontStruct *fs = NULL;
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), font);
  if (fs)
    {
      if (bold_button_font(ss)) FREE(bold_button_font(ss));
      in_set_bold_button_font(ss, copy_string(font));
      (ss->sgx)->bold_button_fontstruct = fs;
      if ((ss->sgx)->bold_button_fontlist) XM_FONT_FREE((ss->sgx)->bold_button_fontlist);
      (ss->sgx)->bold_button_fontlist = get_xm_font(ss, (ss->sgx)->bold_button_fontstruct, font, "bold_button_font");
      return(TRUE);
    }
  return(FALSE);
}

int set_axis_label_font(snd_state *ss, char *font)
{
  XFontStruct *fs = NULL;
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), font);
  if (fs)
    {
      if (axis_label_font(ss)) FREE(axis_label_font(ss));
      in_set_axis_label_font(ss, copy_string(font));
      (ss->sgx)->axis_label_fontstruct = fs;
#if HAVE_GL
      reload_label_font(ss);
#endif
      return(TRUE);
    }
  return(FALSE);
}

int set_axis_numbers_font(snd_state *ss, char *font)
{
  XFontStruct *fs = NULL;
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), font);
  if (fs)
    {
      if (axis_numbers_font(ss)) FREE(axis_numbers_font(ss));
      in_set_axis_numbers_font(ss, copy_string(font));
      (ss->sgx)->axis_numbers_fontstruct = fs;
#if HAVE_GL
      reload_number_font(ss);
#endif
      return(TRUE);
    }
  return(FALSE);
}


void activate_numbers_font(axis_context *ax, snd_state *ss)
{
  ax->current_font = ((XFontStruct *)(AXIS_NUMBERS_FONT(ss)))->fid;
  XSetFont(ax->dp, ax->gc, ((XFontStruct *)(AXIS_NUMBERS_FONT(ss)))->fid);
}
   
void activate_button_font(axis_context *ax, snd_state *ss)
{
  state_context *sgx;
  sgx = ss->sgx;
  ax->current_font = (sgx->button_fontstruct)->fid;
  XSetFont(ax->dp, ax->gc, (sgx->button_fontstruct)->fid);
}

void activate_label_font(axis_context *ax, snd_state *ss)
{
  ax->current_font = ((XFontStruct *)(AXIS_LABEL_FONT(ss)))->fid;
  XSetFont(ax->dp, ax->gc, ((XFontStruct *)(AXIS_LABEL_FONT(ss)))->fid);
}

int label_width(snd_state *ss, char *txt)
{
  if (txt)
    return(XTextWidth(AXIS_LABEL_FONT(ss), txt, strlen(txt)));
  else return(0);
}

int mark_name_width(snd_state *ss, char *txt)
{
  state_context *sgx;
  if (txt)
    {
      sgx = ss->sgx;
      return(XTextWidth(sgx->button_fontstruct, txt, strlen(txt)));
    }
  return(0);
}

int number_width(snd_state *ss, char *num)
{
  if (num)
    return(XTextWidth(AXIS_NUMBERS_FONT(ss), num, strlen(num)));
  return(0);
}

int number_height(snd_state *ss)
{
  XFontStruct *numbers_font;
  numbers_font = AXIS_NUMBERS_FONT(ss);
  return(numbers_font->ascent + numbers_font->descent);
}

int label_height(snd_state *ss)
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
	XmChangeColor(w, (Pixel)(((snd_state *)userptr)->sgx)->position_color);
      else XmChangeColor(w, (Pixel)(((snd_state *)userptr)->sgx)->basic_color);
    }
}

void highlight_color(snd_state *ss, Widget w) {XmChangeColor(w, (ss->sgx)->highlight_color);}
void white_color(snd_state *ss, Widget w) {XmChangeColor(w, (ss->sgx)->white);}

void set_button_label_normal(Widget button, const char *str) 
{
  XmString s1;
  s1 = XmStringCreate((char *)str, "button_font");
#if (USE_RENDITIONS)
  XtVaSetValues(button, 
		XmNrenderTable, BUTTON_FONT(get_global_state()), 
		XmNlabelString, s1, 
		NULL);
#else
  XtVaSetValues(button, XmNlabelString, s1, NULL);
#endif
  XmStringFree(s1);
}

void set_button_label_bold(Widget button, const char *str)
{
  XmString s1;
  s1 = XmStringCreate((char *)str, "bold_button_font");
#if (USE_RENDITIONS)
  XtVaSetValues(button, 
		XmNrenderTable, BOLD_BUTTON_FONT(get_global_state()), 
		XmNlabelString, s1,
		NULL);
#else
  XtVaSetValues(button, XmNlabelString, s1, NULL);
#endif
  XmStringFree(s1);
}

void set_label(Widget label, const char *str)
{
  XmString s1;
  s1 = XmStringCreate((char *)str, XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(label, XmNlabelString, s1, NULL);
  XmStringFree(s1);
}

void set_button_label (Widget label, const char *str) {set_label(label, str);}


void set_title(snd_state *ss, const char *title)
{
  XtVaSetValues(MAIN_SHELL(ss), XmNtitle, (char*)title, NULL);
}

static int complain_about_focus_policy = 1;

void goto_window(Widget text)
{
  int err;
  if ((XmIsTraversable(text)) && 
      (XmGetVisibility(text) != XmVISIBILITY_FULLY_OBSCURED))
    {
      if (!(XmProcessTraversal(text, XmTRAVERSE_CURRENT)))
	{
	  if (complain_about_focus_policy)
	    {
	      XtVaGetValues(text, XmNkeyboardFocusPolicy, &err, NULL);
	      if (err == XmEXPLICIT)
		snd_error("%s[%d] %s: traverse to %s failed!", 
			  __FILE__, __LINE__, __FUNCTION__, 
			  XtName(text));
	      else 
		{
		  snd_error("%s[%d] %s: keyboard focus policy is not explicit!", 
			    __FILE__, __LINE__, __FUNCTION__);
		  complain_about_focus_policy = 0;
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
    XmChangeColor(w, (Pixel)(((snd_state *)ptr)->sgx)->sash_color);
}

void check_for_event(snd_state *ss)
{
  /* this is needed to force label updates and provide interrupts from long computations */
  XEvent event;
  XtInputMask msk = 0;
  XtAppContext app;
  if (ss->checking_explicitly) return;
  ss->checking_explicitly = 1;
  app = MAIN_APP(ss);
  while (1)
    {
      msk = XtAppPending(app);
      if (msk & (XtIMXEvent | XtIMAlternateInput))
	{
	  XtAppNextEvent(app, &event);
	  XtDispatchEvent(&event);
	}
      else break;
    }
  ss->checking_explicitly = 0;
}

int event_pending(snd_state *ss)
{
  XtInputMask msk = 0;
  XtAppContext app;
  app = MAIN_APP(ss);
  msk = XtAppPending(app);
  return(msk & XtIMXEvent);
}

void color_cursor(snd_state *ss, Pixel color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->cursor_color = color;
  XSetForeground(MAIN_DISPLAY(ss), sx->cursor_gc, (Pixel)(XOR(color, sx->graph_color)));
  XSetForeground(MAIN_DISPLAY(ss), sx->selected_cursor_gc, (Pixel)(XOR(color, sx->selected_graph_color)));
}

void color_marks(snd_state *ss, Pixel color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->mark_color = color;
  XSetForeground(MAIN_DISPLAY(ss), sx->mark_gc, (Pixel)(XOR(color, sx->graph_color)));
  XSetForeground(MAIN_DISPLAY(ss), sx->selected_mark_gc, (Pixel)(XOR(color, sx->selected_graph_color)));
}

void color_selection(snd_state *ss, Pixel color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->selection_color = color;
  XSetForeground(MAIN_DISPLAY(ss), sx->selection_gc, (Pixel)(XOR(color, sx->graph_color)));
  XSetForeground(MAIN_DISPLAY(ss), sx->selected_selection_gc, (Pixel)(XOR(color, sx->selected_graph_color)));
}

void color_graph(snd_state *ss, Pixel color)
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

void color_selected_graph(snd_state *ss, Pixel color)
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

void color_data(snd_state *ss, Pixel color)
{
  Display *dpy;
  state_context *sx;
  dpy = MAIN_DISPLAY(ss);
  sx = ss->sgx;
  sx->data_color = color;
  XSetForeground(dpy, sx->basic_gc, color);
  XSetBackground(dpy, sx->erase_gc, color);
}

void color_selected_data(snd_state *ss, Pixel color)
{
  Display *dpy;
  state_context *sx;
  dpy = MAIN_DISPLAY(ss);
  sx = ss->sgx;
  sx->selected_data_color = color;
  XSetForeground(dpy, sx->selected_basic_gc, color);
  XSetBackground(dpy, sx->selected_erase_gc, color);
}

void recolor_graph(chan_info *cp, int selected)
{
  snd_state *ss;
  state_context *sx;
  ss = cp->state;
  sx = ss->sgx;
  XtVaSetValues(channel_graph(cp), XmNbackground, (selected) ? sx->selected_graph_color : sx->graph_color, NULL);
}

void set_mix_color(snd_state *ss, Pixel color)
{
  Display *dpy;
  state_context *sx;
  dpy = MAIN_DISPLAY(ss);
  sx = ss->sgx;
  sx->mix_color = color;
  XSetForeground(dpy, sx->mix_gc, color);
}

void set_selected_mix_color(snd_state *ss, Pixel color)
{
  Display *dpy;
  state_context *sx;
  dpy = MAIN_DISPLAY(ss);
  sx = ss->sgx;
  sx->selected_mix_color = color;
  XSetForeground(dpy, sx->selected_mix_gc, color);
}

#if (!(HAVE_XPM_GET_ERROR_STRING))
char *XpmGetErrorString(int err);
char *XpmGetErrorString(int err) {return("");}
#endif

void reflect_resize(snd_state *ss)
{
  XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, auto_resize(ss), NULL);
}

void set_sensitive(Widget wid, int val) {if (wid) XtSetSensitive(wid, val);}
int is_sensitive(Widget wid) {if (wid) return(XtIsSensitive(wid)); return(0);}
void set_toggle_button(Widget wid, int val, int passed, void *data) {XmToggleButtonSetState(wid, val, passed);}


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

Pixmap make_pixmap(snd_state *ss, unsigned char *bits, int width, int height, int depth, GC gc)
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

BACKGROUND_FUNCTION_TYPE add_work_proc(snd_state *ss, XtWorkProc func, XtPointer data)
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
