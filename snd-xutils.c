#include "snd.h"

#include <X11/IntrinsicP.h>

axis_context *free_axis_context(axis_context *ax)
{
  if (ax) FREE(ax);
  return(NULL);
}

#if (USE_RENDITIONS)
static XmRenderTable get_xm_font(snd_state *ss, XFontStruct *fs, char *font, char *tag)
{
  XmRendition tmp;
  int n;
  Arg args[12];
  n = 0;
  XtSetArg(args[n], XmNfontName, font); n++;
  XtSetArg(args[n], XmNfontType, XmFONT_IS_FONT); n++; 
  XtSetArg(args[n], XmNloadModel, XmLOAD_DEFERRED); n++;
  tmp = XmRenditionCreate(MAIN_SHELL(ss), tag, args, n);
  return(XmRenderTableAddRenditions(NULL, &tmp, 1, XmMERGE_NEW));
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
      in_set_help_text_font(ss, font);
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
      in_set_tiny_font(ss, font);
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
      in_set_listener_font(ss, font);
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
      in_set_button_font(ss, font);
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
      in_set_bold_button_font(ss, font);
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
      in_set_axis_label_font(ss, font);
      (ss->sgx)->axis_label_fontstruct = fs;
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
      in_set_axis_numbers_font(ss, font);
      (ss->sgx)->axis_numbers_fontstruct = fs;
      return(TRUE);
    }
  return(FALSE);
}


void activate_numbers_font(axis_context *ax)
{
  ax->current_font = ((XFontStruct *)(AXIS_NUMBERS_FONT(ax->ss)))->fid;
  XSetFont(ax->dp, ax->gc, ((XFontStruct *)(AXIS_NUMBERS_FONT(ax->ss)))->fid);
}
   
void activate_button_font(axis_context *ax, snd_state *ss)
{
  state_context *sgx;
  sgx = ss->sgx;
  ax->current_font = (sgx->button_fontstruct)->fid;
  XSetFont(ax->dp, ax->gc, (sgx->button_fontstruct)->fid);
}

void activate_label_font(axis_context *ax)
{
  ax->current_font = ((XFontStruct *)(AXIS_LABEL_FONT(ax->ss)))->fid;
  XSetFont(ax->dp, ax->gc, ((XFontStruct *)(AXIS_LABEL_FONT(ax->ss)))->fid);
}

int label_width(axis_context *ax, char *txt)
{
  if (txt)
    return(XTextWidth(AXIS_LABEL_FONT(ax->ss), txt, strlen(txt)));
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

int number_width(axis_context *ax, char *num)
{
  if (num)
    return(XTextWidth(AXIS_NUMBERS_FONT(ax->ss), num, strlen(num)));
  return(0);
}

int number_height(axis_context *ax)
{
  XFontStruct *numbers_font;
  numbers_font = AXIS_NUMBERS_FONT(ax->ss);
  return(numbers_font->ascent + numbers_font->descent);
}

int label_height(axis_context *ax)
{
  XFontStruct *label_font;
  label_font = AXIS_LABEL_FONT(ax->ss);
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

#if 0
void raise_widget(Widget w)
{
  /* try to change stacking order (form widgets in drawingarea; overlap covers desired console) */
  XtWidgetGeometry *request;
  request = (XtWidgetGeometry *)CALLOC(1, sizeof(XtWidgetGeometry));
  request->request_mode = CWStackMode; /* (1 << 6) */
  request->stack_mode = 0; /* Above */
  XtMakeGeometryRequest(w, request, NULL);
}
#endif

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
		XmNlabelString, s1, 
		XmNrenderTable, BUTTON_FONT(get_global_state()), 
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
		XmNlabelString, s1,
		XmNrenderTable, BOLD_BUTTON_FONT(get_global_state()), 
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

#if NEED_XPM_GET_ERROR_STRING
char *XpmGetErrorString(off_err) {return("");}
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


#if HAVE_THEMES
/* background pixmap (can be filled with fancy gradients etc) */

static void repixmap(Widget w, void *usx)
{
  Pixel curcol;
  state_context *sx = (state_context *)usx;
  if (XtIsWidget(w))
    {
      XtVaGetValues(w, XmNbackground, &curcol, NULL);
      if (curcol == sx->basic_color)
	XtVaSetValues(w, XmNbackgroundPixmap, sx->backmap, NULL);
    }
}

static Pixel *bgs = NULL;
static int bgs_size = 0;
void make_bg(snd_state *ss, unsigned int width, unsigned int height);
void make_bg(snd_state *ss, unsigned int width, unsigned int height)
{
  unsigned int depth;
  Widget w;
  Display *dp;
  Drawable wn;
  XGCValues v;
  state_context *sx;
  Colormap cmap;
  int scr, i, j, err, wid_incr;
  GC draw_gc;
  XColor tmp_color;
  Float red_incr, blue_incr, green_incr, red_init, blue_init, green_init;
  sx = ss->sgx;
  w = MAIN_PANE(ss);
  dp = MAIN_DISPLAY(ss);
  wn = XtWindow(w);
  scr = DefaultScreen(dp);
  XtVaGetValues(w, XmNdepth, &depth, NULL);
  if (sx->backmap) XFreePixmap(dp, sx->backmap);
  cmap = DefaultColormap(dp, scr);
  if (bgs) 
    {
      XFreeColors(dp, cmap, bgs, bgs_size, 0);
      free(bgs);
    }
  bgs_size = width / 2;
  if (bgs_size > 200) bgs_size = 200;
  wid_incr = width / bgs_size;
  bgs = (Pixel *)calloc(bgs_size, sizeof(Pixel));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  sx->backmap = XCreatePixmap(dp, wn, width, height, depth);
  v.background = WhitePixel(dp, scr);
  v.foreground = WhitePixel(dp, scr);
  draw_gc = XCreateGC(dp, wn, GCForeground | GCBackground, &v);
  tmp_color.pixel = sx->highlight_color;
  XQueryColor(dp, cmap, &tmp_color);
  red_init = tmp_color.red;
  green_init = tmp_color.green;
  blue_init = tmp_color.blue;
  tmp_color.pixel = sx->basic_color;
  XQueryColor(dp, cmap, &tmp_color);
  red_incr = (tmp_color.red - red_init) / bgs_size;
  green_incr = (tmp_color.green - green_init) / bgs_size;
  blue_incr = (tmp_color.blue - blue_init) / bgs_size;
  /* this should probably be light at left top and shade toward lower right */
  for (i = 0, j = 0; i < (int)width; i += wid_incr, j++)
    {
      tmp_color.flags = DoRed | DoGreen | DoBlue;
      tmp_color.red = (unsigned short)red_init; red_init += red_incr;
      tmp_color.green = (unsigned short)green_init; green_init += green_incr;
      tmp_color.blue = (unsigned short)blue_init; blue_init += blue_incr;
      err = XAllocColor(dp, cmap, &tmp_color); 
      if (err == 0) fprintf(stderr, ".");
      bgs[j] = tmp_color.pixel;
      XSetForeground(dp, draw_gc, tmp_color.pixel);
      XFillRectangle(dp, sx->backmap, draw_gc, i, 0, wid_incr, height);
    }
  map_over_children(w, repixmap, (void *)sx);
}
#endif

