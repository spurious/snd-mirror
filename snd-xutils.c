#include "snd.h"

#include <X11/IntrinsicP.h>

#if __GNUC__
#ifdef LESSTIF_VERSION
  /* moved the warning here so it only is displayed once */
  #warning You appear to be using Lesstif: this is not recommended!  Expect bugs...
#endif
#if (XmVERSION == 1)
  #warning Motif 1 is no longer supported -- this has little chance of working...
#endif
#endif

static XmRenderTable get_xm_font(XFontStruct *ignore, const char *font, char *tag)
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
  /* XmRenditionFree(tmp); */ /* valgrind thinks this is a bad idea */
  return(tabl);
}

bool set_tiny_font(const char *font)
{
  XFontStruct *fs = NULL;
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), font);
  if (fs)
    {
      /* it's not clear to me whether this is safe -- what if two fontstructs are pointing to the same font? */
      if (TINY_FONT(ss)) XFreeFont(MAIN_DISPLAY(ss), TINY_FONT(ss));
      if (tiny_font(ss)) FREE(tiny_font(ss));
      in_set_tiny_font(copy_string(font));
      TINY_FONT(ss) = fs;
      if (ss->sgx->tiny_fontlist) XM_FONT_FREE(ss->sgx->tiny_fontlist);
      ss->sgx->tiny_fontlist = get_xm_font(TINY_FONT(ss), font, "tiny_font");
      return(true);
    }
  return(false);
}

bool set_listener_font(const char *font)
{
  XFontStruct *fs = NULL;
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), font);
  if (fs)
    {
      if (LISTENER_FONT(ss)) XFreeFont(MAIN_DISPLAY(ss), LISTENER_FONT(ss));
      if (listener_font(ss)) FREE(listener_font(ss));
      in_set_listener_font(copy_string(font));
      LISTENER_FONT(ss) = fs;
      if (ss->sgx->listener_fontlist) XM_FONT_FREE(ss->sgx->listener_fontlist);
      ss->sgx->listener_fontlist = get_xm_font(LISTENER_FONT(ss), font, "listener_font");
      set_listener_text_font();
      return(true);
    }
  return(false);
}

bool set_peaks_font(const char *font)
{
  XFontStruct *fs = NULL;
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), font);
  if (fs)
    {
      if (PEAKS_FONT(ss)) XFreeFont(MAIN_DISPLAY(ss), PEAKS_FONT(ss));
      if (peaks_font(ss)) FREE(peaks_font(ss));
      in_set_peaks_font(copy_string(font));
      PEAKS_FONT(ss) = fs;
      if (ss->sgx->peaks_fontlist) XM_FONT_FREE(ss->sgx->peaks_fontlist);
      ss->sgx->peaks_fontlist = get_xm_font(PEAKS_FONT(ss), font, "peaks_font");
      return(true);
    }
  return(false);
}

bool set_bold_peaks_font(const char *font)
{
  XFontStruct *fs = NULL;
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), font);
  if (fs)
    {
      if (BOLD_PEAKS_FONT(ss)) XFreeFont(MAIN_DISPLAY(ss), BOLD_PEAKS_FONT(ss));
      if (bold_peaks_font(ss)) FREE(bold_peaks_font(ss));
      in_set_bold_peaks_font(copy_string(font));
      BOLD_PEAKS_FONT(ss) = fs;
      if (ss->sgx->bold_peaks_fontlist) XM_FONT_FREE(ss->sgx->bold_peaks_fontlist);
      ss->sgx->bold_peaks_fontlist = get_xm_font(BOLD_PEAKS_FONT(ss), font, "bold_peaks_font");
      return(true);
    }
  return(false);
}

bool set_axis_label_font(const char *font)
{
  XFontStruct *fs = NULL;
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), font);
  if (fs)
    {
      if (AXIS_LABEL_FONT(ss)) XFreeFont(MAIN_DISPLAY(ss), AXIS_LABEL_FONT(ss));
      if (axis_label_font(ss)) FREE(axis_label_font(ss));
      in_set_axis_label_font(copy_string(font));
      AXIS_LABEL_FONT(ss) = fs;
#if HAVE_GL
      reload_label_font();
#endif
      return(true);
    }
  return(false);
}

bool set_axis_numbers_font(const char *font)
{
  XFontStruct *fs = NULL;
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), font);
  if (fs)
    {
      if (AXIS_NUMBERS_FONT(ss)) XFreeFont(MAIN_DISPLAY(ss), AXIS_NUMBERS_FONT(ss));
      if (axis_numbers_font(ss)) FREE(axis_numbers_font(ss));
      in_set_axis_numbers_font(copy_string(font));
      AXIS_NUMBERS_FONT(ss) = fs;
#if HAVE_GL
      reload_number_font();
#endif
      return(true);
    }
  return(false);
}

int mark_name_width(const char *txt)
{
  if (txt)
    return(XTextWidth(PEAKS_FONT(ss), txt, strlen(txt)));
  return(0);
}

int label_width(const char *txt, bool use_tiny_font)
{
  if (txt)
    return(XTextWidth((use_tiny_font) ? TINY_FONT(ss) : AXIS_LABEL_FONT(ss), txt, strlen(txt)));
  else return(0);
}

int number_width(const char *num, bool use_tiny_font)
{
  if (num)
    return(XTextWidth((use_tiny_font) ? TINY_FONT(ss) : AXIS_NUMBERS_FONT(ss), num, strlen(num)));
  return(0);
}

int number_height(bool use_tiny_font)
{
  XFontStruct *numbers_font;
  if (use_tiny_font)
    numbers_font = TINY_FONT(ss);
  else numbers_font = AXIS_NUMBERS_FONT(ss);
  return(numbers_font->ascent);
}

int label_height(bool use_tiny_font)
{
  XFontStruct *label_font;
  if (use_tiny_font)
    label_font = TINY_FONT(ss);
  else label_font = AXIS_LABEL_FONT(ss);
  return(label_font->ascent + label_font->descent);
}

void clear_window(axis_context *ax)
{
  if (ax) XClearWindow(ax->dp, ax->wn);
}

void map_over_children(Widget w, void (*func)(Widget uw))
{
  /* apply func to each child in entire tree beneath top widget */
  /* taken from Douglas Young, "Motif Debugging and Performance Tuning" Prentice-Hall 1995 */
  /* used mostly to get colors right in non-SGI environments with "convenience" widgets */
  if (w)
    {
      unsigned int i;
      (*func)(w);
      if (XtIsComposite(w))
	{
	  CompositeWidget cw = (CompositeWidget)w;
	  for (i = 0; i < cw->composite.num_children; i++)
	    map_over_children(cw->composite.children[i], func);
	}
      if (XtIsWidget(w))
	{
	  for (i = 0; i < w->core.num_popups; i++)
	    {
	      Widget child = w->core.popup_list[i];
	      map_over_children(child, func);
	    }
	}
    }
}

void map_over_children_with_color(Widget w, void (*func)(Widget uw, color_t color), color_t color)
{
  if (w)
    {
      unsigned int i;
      (*func)(w, color);
      if (XtIsComposite(w))
	{
	  CompositeWidget cw = (CompositeWidget)w;
	  for (i = 0; i < cw->composite.num_children; i++)
	    map_over_children_with_color(cw->composite.children[i], func, color);
	}
      if (XtIsWidget(w))
	{
	  for (i = 0; i < w->core.num_popups; i++)
	    {
	      Widget child = w->core.popup_list[i];
	      map_over_children_with_color(child, func, color);
	    }
	}
    }
}

void raise_dialog(Widget w)
{
  /* since we're using non-transient message dialogs, the dialog window can become completely
   * hidden behind other windows, with no easy way to raise it back to the top, so...
   */
  if ((w) && (XtIsManaged(w)))
    {
      Widget parent;
      parent = XtParent(w);
      if ((parent) && 
	  (XtIsSubclass(parent, xmDialogShellWidgetClass)))
	XtPopup(parent, XtGrabNone);
      /* XtGrabNone means don't lock out events to rest of App (i.e. modeless dialog) */
    }
}

void set_main_color_of_widget(Widget w)
{
  if (XtIsWidget(w))
    {
      if (XmIsScrollBar(w)) 
	XmChangeColor(w, (Pixel)ss->sgx->position_color);
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
	    XmChangeColor(w, (Pixel)ss->sgx->basic_color);
	}
    }
}

void set_label(Widget label, const char *str)
{
  XmString s1;
  s1 = XmStringCreateLocalized((char *)str);
  XtVaSetValues(label, XmNlabelString, s1, NULL);
  XmStringFree(s1);
}

char *get_label(Widget label)
{
  char *text;
  XmString str = NULL;
  XtVaGetValues(label, XmNlabelString, &str, NULL);
  if (XmStringEmpty(str)) return(NULL);
  text = (char *)XmStringUnparse(str, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
  XmStringFree(str);
  return(text);
}

void set_button_label(Widget label, const char *str) {set_label(label, str);}

void set_title(const char *title)
{
  XtVaSetValues(MAIN_SHELL(ss), XmNtitle, (char*)title, NULL);
}

void goto_window(Widget text)
{
  if (XmIsTraversable(text))
    XmProcessTraversal(text, XmTRAVERSE_CURRENT);
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
void color_sashes(Widget w)
{
  if ((XtIsWidget(w)) && 
      (XtIsSubclass(w, xmSashWidgetClass)))
    XmChangeColor(w, (Pixel)ss->sgx->sash_color);
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
      /* if (msk & (XtIMXEvent | XtIMAlternateInput)) */
      if (msk & XtIMXEvent)
	/* was also tracking alternate input events, but these are problematic if libfam is in use (even with check) */
	{
	  XtAppNextEvent(app, &event);
	  XtDispatchEvent(&event);
	  /* widget = XtWindowToWidget(event.xany.display, event.xany.window); */
	}
      else break;
    }
  ss->checking_explicitly = false;
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
  XtVaSetValues(channel_graph(cp), XmNbackground, (selected) ? ss->sgx->selected_graph_color : ss->sgx->graph_color, NULL);
}

void set_mix_color(Pixel color)
{
  /* called only from g_set_mix_color -- docs imply it changes existing colors */
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

void set_sensitive(Widget wid, bool val) 
{
  if (wid) XtSetSensitive(wid, val);
}

void set_toggle_button(Widget wid, bool val, bool passed, void *ignore) 
{
  XmToggleButtonSetState(wid, (Boolean)val, (Boolean)passed);
}

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

void widget_int_to_text(Widget w, int val)
{
  char *str;
  str = (char *)CALLOC(8, sizeof(char));
  mus_snprintf(str, 8, "%d", val);
  XmTextFieldSetString(w, str);
  FREE(str);
}

void widget_float_to_text(Widget w, Float val)
{
  char *str;
  str = (char *)CALLOC(16, sizeof(char));
  mus_snprintf(str, 16, "%.2f", val);
  XmTextFieldSetString(w, str);
  FREE(str);
}

void widget_off_t_to_text(Widget w, off_t val)
{
  char *str;
  str = (char *)CALLOC(32, sizeof(char));
  mus_snprintf(str, 32, OFF_TD, val);
  XmTextFieldSetString(w, str);
  FREE(str);
}

Pixmap rotate_text(Widget w, const char *str, XFontStruct *font, Float angle_in_degrees, int *nw, int *nh, Pixel bg, Pixel fg, GC d_gc)
{
  /* rotate clockwise by angle_in_degrees degrees (i.e. 45 points text south-east), 
   * new bounding box (text centered in it) returned in nw and nh
   * bg = background color, fg = foreground (text) color) 
   */
  Float matrix[4];
  Float angle_in_radians;
  XImage *before, *after;
  Pixmap pix, rotpix;
  unsigned int width, height, depth, nwidth, nheight, x, y, nx, ny, tx, ty, depth_bytes;
  char *data;
  unsigned long px;
  Display *dp;
  Drawable wn;
  Visual *vis;
  int scr;
  int bx0 = 0, bx1 = 0, by0 = 0, by1 = 0, b;
  if (str == NULL) return(BadPixmap);

  angle_in_radians = mus_degrees_to_radians(angle_in_degrees);
  matrix[0] = cos(angle_in_radians);
  matrix[1] = sin(angle_in_radians);
  matrix[2] = -sin(angle_in_radians);
  matrix[3] = cos(angle_in_radians);
  dp = XtDisplay(w);
  wn = XtWindow(w);
  scr = DefaultScreen(dp);
  vis = DefaultVisual(dp, scr);
  XtVaGetValues(w, XmNdepth, &depth, NULL);
  depth_bytes = (depth >> 3);
  if (depth_bytes == 0) depth_bytes = 1; /* unsigned so can't be negative */

  /* find extent of original text, expand out to byte boundaries */
  XSetFont(dp, d_gc, font->fid);
  width = XTextWidth(font, str, strlen(str)) + 8;
  height = (font->ascent + font->descent) + 8;
  if (width % 8) width = 8 * (1 + (int)(width / 8));
  if (height % 8) height = 8 * (1 + (int)(height / 8));

  /* get bounding box of rotated text (this could be simplfied -- used to involve scaling) */
  b = (int)(width * matrix[0]);
  if (b < 0) bx0 = b; else bx1 = b;
  b = (int)(height * matrix[2]);
  if (b < 0) bx0 += b; else bx1 += b;
  b = (int)(width * matrix[1]);
  if (b < 0) by0 = b; else by1 = b;
  b = (int)(height * matrix[3]);
  if (b < 0) by0 += b; else by1 += b;
  
  /* set translation vector so we're centered in the resultant pixmap */
  if (bx0 < 0) tx = -bx0; else tx = 0;
  if (by0 < 0) ty = -by0; else ty = 0;
  nx = bx1 - bx0;
  ny = by1 - by0;

  /* expand result bounds to byte boundaries */
  if (nx % 8) nwidth = 8 * (1 + (int)(nx / 8)); else nwidth = nx;
  if (ny % 8) nheight = 8 * (1 + (int)(ny / 8)); else nheight = ny;
  (*nw) = nwidth;
  (*nh) = nheight;

  XSetBackground(dp, d_gc, bg); 
  XSetForeground(dp, d_gc, bg); 

  /* create pixmaps, fill with background color, write string to pix */
  pix = XCreatePixmap(dp, wn, width, height, depth);
  rotpix= XCreatePixmap(dp, wn, nwidth, nheight, depth);
  XFillRectangle(dp, pix, d_gc, 0, 0, width, height);
  XFillRectangle(dp, rotpix, d_gc, 0, 0, nwidth, nheight);
#ifdef MUS_SUN
  XSync(dp, 0);
  /* needed to get the numbers drawn at all */
#endif
  XSetForeground(dp, d_gc, fg);
  XDrawImageString(dp, pix, d_gc, 4, height - 4, str, strlen(str));

  /* dump pixmap bits into an image; image data will be freed automatically later */
  data = (char *)calloc((width + 1) * (height + 1) * depth_bytes, sizeof(char)); /* not CALLOC since X will free this */
  before = XCreateImage(dp, vis, depth, XYPixmap, 0, data, width, height, 8, 0);
  XGetSubImage(dp, pix, 0, 0, width, height, AllPlanes, XYPixmap, before, 0, 0);
  data = (char *)calloc((nwidth + 1) * (nheight + 1) * depth_bytes, sizeof(char));
  after = XCreateImage(dp, vis, depth, XYPixmap, 0, data, nwidth, nheight, 8, 0);

  /* clear background of result image */
  for (x = 0; x < nwidth; x++) 
    for (y = 0; y < nheight; y++) 
      XPutPixel(after, x, y, bg);

  /* write rotated pixels to result image */
  for (x = 0; x < width; x++)
    for (y = 0; y < height; y++)
      {
	px = XGetPixel(before, x, y);
	if (px != bg)
	  XPutPixel(after, 
		    mus_iclamp(0, (int)snd_round(tx + x * matrix[0] + y * matrix[2]), nwidth - 1),
		    mus_iclamp(0, (int)snd_round(ty + x * matrix[1] + y * matrix[3]), nheight - 1),
		    px);
      }

  /* dump image into result pixmap (needed for later display) */
  XPutImage(dp, rotpix, d_gc, after, 0, 0, 0, 0, nwidth, nheight);

  /* cleanup */
  XDestroyImage(before);  /* frees data as well */
  XDestroyImage(after);
  XFreePixmap(dp, pix);
  return(rotpix);
}

void draw_rotated_axis_label(chan_info *cp, GC gc, const char *text, int x0, int y0)
{
  Pixmap pix;
  int h, w;
  XGCValues gv;
  Display *dp;
  Widget widget;
  if ((cp->chan > 0) && (cp->sound->channel_style == CHANNELS_COMBINED))
    widget = channel_graph(cp->sound->chans[0]);
  else widget = channel_graph(cp);
  dp = XtDisplay(widget);
  XGetGCValues(MAIN_DISPLAY(ss), gc, GCForeground | GCBackground, &gv);
  pix = rotate_text(widget, text, AXIS_LABEL_FONT(ss), -90.0, &w, &h, gv.background, gv.foreground, gc);
  XCopyArea(dp, pix, XtWindow(widget), gc, 0, 0, w, h, x0, y0); /* XtWindow?? */
  XFreePixmap(dp, pix);  
}

void ensure_list_row_visible(widget_t list, int pos)
{
  int top, visible, num_rows;
  if (pos >= 0)
    {
      XtVaGetValues(list,
		    XmNtopItemPosition, &top,
		    XmNvisibleItemCount, &visible,
		    XmNitemCount, &num_rows,
		    NULL);
      if (pos < top)
	XmListSetPos(list, pos);
      else
	{
	  if (pos >= (top + visible))
	    {
	      if ((pos + visible) > num_rows)
		XmListSetBottomPos(list, pos);
	      else XmListSetPos(list, pos);
	    }
	}
    }
}

void ensure_scrolled_window_row_visible(widget_t list, int row, int num_rows)
{
  int visible_size, minimum, maximum, value, size, new_value, increment, page_increment;
  Widget scrollbar, work_window;
  XtVaGetValues(list, 
		XmNverticalScrollBar, &scrollbar, 
		XmNworkWindow, &work_window,
		NULL);
  XtVaGetValues(work_window, XmNheight, &visible_size, NULL);
  XtVaGetValues(scrollbar, 
		XmNminimum, &minimum,
		XmNmaximum, &maximum,
		XmNvalue, &value,
		XmNsliderSize, &size,
		XmNincrement, &increment, /* needed for XmScrollBarSetValues which is needed to force list display update */
		XmNpageIncrement, &page_increment,
		NULL);
  maximum -= size;
  if (row == 0)
    new_value = 0;
  else
    {
      if (row >= (num_rows - 1))
	new_value = maximum;
      else new_value = (int)((row + 0.5) * ((float)(maximum - minimum) / (float)(num_rows - 1)));
    }
  XmScrollBarSetValues(scrollbar, new_value, size, increment, page_increment, true);
}

XmString multi_line_label(const char *s, int *lines)
{
  /* taken from the Motif FAQ */
  XmString xms1, xms2, line, separator;
  char *p, *tmp;
  (*lines) = 1;
  tmp = copy_string(s);
  separator = XmStringSeparatorCreate();
  p = strtok(tmp, "\n");
  xms1 = XmStringCreateLocalized(p);
  p = strtok(NULL, "\n");
  while (p)
    {
      (*lines)++;
      line = XmStringCreateLocalized(p);
      xms2 = XmStringConcat(xms1, separator);
      XmStringFree(xms1);
      xms1 = XmStringConcat(xms2, line);
      XmStringFree(xms2);
      XmStringFree(line);
      p = strtok(NULL, "\n");
    }
  XmStringFree(separator);
  FREE(tmp);
  return(xms1);
}

