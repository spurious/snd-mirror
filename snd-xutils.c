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


static XmRenderTable get_xm_font(XFontStruct *ignore, const char *font, const char *tag)
{
  XmRendition tmp;
  XmRenderTable tabl;
  int n;
  Arg args[12];

  n = 0;
  XtSetArg(args[n], XmNfontName, font); n++;
  XtSetArg(args[n], XmNfontType, XmFONT_IS_FONT); n++; 
  XtSetArg(args[n], XmNloadModel, XmLOAD_IMMEDIATE); n++;
  tmp = XmRenditionCreate(MAIN_SHELL(ss), (char *)tag, args, n);
  tabl = XmRenderTableAddRenditions(NULL, &tmp, 1, XmMERGE_NEW);

  /* XmRenditionFree(tmp); */ /* valgrind thinks this is a bad idea */
  return(tabl);
}


/* to see all fonts: (format #f "~{~A~%~}" (XListFonts (XtDisplay (cadr (main-widgets))) "*" 10000))
 */

bool set_tiny_font(const char *font)
{
  XFontStruct *fs = NULL;
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), font);
  if (fs)
    {
      /* it's not clear to me whether this is safe -- what if two fontstructs are pointing to the same font? */
      if (TINY_FONT(ss)) XFreeFont(MAIN_DISPLAY(ss), TINY_FONT(ss));
      if (tiny_font(ss)) free(tiny_font(ss));
      in_set_tiny_font(mus_strdup(font));
      TINY_FONT(ss) = fs;
      if (ss->tiny_fontlist) XM_FONT_FREE(ss->tiny_fontlist);
      ss->tiny_fontlist = get_xm_font(TINY_FONT(ss), font, "tiny_font");
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
      if (listener_font(ss)) free(listener_font(ss));
      in_set_listener_font(mus_strdup(font));
      LISTENER_FONT(ss) = fs;
      if (ss->listener_fontlist) XM_FONT_FREE(ss->listener_fontlist);
      ss->listener_fontlist = get_xm_font(LISTENER_FONT(ss), font, "listener_font");
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
      if (peaks_font(ss)) free(peaks_font(ss));
      in_set_peaks_font(mus_strdup(font));
      PEAKS_FONT(ss) = fs;
      if (ss->peaks_fontlist) XM_FONT_FREE(ss->peaks_fontlist);
      ss->peaks_fontlist = get_xm_font(PEAKS_FONT(ss), font, "peaks_font");
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
      if (bold_peaks_font(ss)) free(bold_peaks_font(ss));
      in_set_bold_peaks_font(mus_strdup(font));
      BOLD_PEAKS_FONT(ss) = fs;
      if (ss->bold_peaks_fontlist) XM_FONT_FREE(ss->bold_peaks_fontlist);
      ss->bold_peaks_fontlist = get_xm_font(BOLD_PEAKS_FONT(ss), font, "bold_peaks_font");
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
      if (axis_label_font(ss)) free(axis_label_font(ss));
      in_set_axis_label_font(mus_strdup(font));
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
      if (axis_numbers_font(ss)) free(axis_numbers_font(ss));
      in_set_axis_numbers_font(mus_strdup(font));
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


int number_height(XFontStruct *numbers_font)
{
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


void clear_window(graphics_context *ax)
{
  if ((ax) && (ax->dp) && (ax->wn))
    XClearWindow(ax->dp, ax->wn);
}


void map_over_children(Widget w, void (*func)(Widget uw))
{
  /* apply func to each child in entire tree beneath top widget */
  /* taken from Douglas Young, "Motif Debugging and Performance Tuning" Prentice-Hall 1995 */
  /* used mostly to get colors right in environments with "convenience" widgets */
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
	for (i = 0; i < w->core.num_popups; i++)
	  map_over_children(w->core.popup_list[i], func);
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
	for (i = 0; i < w->core.num_popups; i++)
	  map_over_children_with_color(w->core.popup_list[i], func, color);
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
	XmChangeColor(w, (Pixel)ss->position_color);
      else 
	{
	  Pixel cur_color;
	  XtVaGetValues(w, XmNbackground, &cur_color, NULL);
	  if ((cur_color != ss->highlight_color) &&
	      (cur_color != ss->white))
	    XmChangeColor(w, (Pixel)ss->basic_color);
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


void set_button_label(Widget label, const char *str) 
{
  set_label(label, str);
}


void set_title(const char *title)
{
  XtVaSetValues(MAIN_SHELL(ss), XmNtitle, (char *)title, NULL);
}


void goto_window(Widget text)
{
  if (XmIsTraversable(text))
    XmProcessTraversal(text, XmTRAVERSE_CURRENT);
}


XtCallbackList make_callback_list(XtCallbackProc callback, XtPointer closure)
{
  XtCallbackList nlist;
  nlist = (XtCallbackList)calloc(2, sizeof(XtCallbackRec));
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
    XmChangeColor(w, (Pixel)ss->sash_color);
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
  ss->cursor_color = color;
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->cursor_color_symbol, Xen_wrap_pixel(color));
#endif
  XSetForeground(MAIN_DISPLAY(ss), ss->cursor_gc, (Pixel)(XOR(color, ss->graph_color)));
  XSetForeground(MAIN_DISPLAY(ss), ss->selected_cursor_gc, (Pixel)(XOR(color, ss->selected_graph_color)));
}


void color_marks(Pixel color)
{
  ss->mark_color = color;
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->mark_color_symbol, Xen_wrap_pixel(color));
#endif
  XSetForeground(MAIN_DISPLAY(ss), ss->mark_gc, (Pixel)(XOR(color, ss->graph_color)));
  XSetForeground(MAIN_DISPLAY(ss), ss->selected_mark_gc, (Pixel)(XOR(color, ss->selected_graph_color)));
}


void color_selection(Pixel color)
{
  ss->selection_color = color;
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->selection_color_symbol, Xen_wrap_pixel(color));
#endif
  XSetForeground(MAIN_DISPLAY(ss), ss->selection_gc, (Pixel)(XOR(color, ss->graph_color)));
  XSetForeground(MAIN_DISPLAY(ss), ss->selected_selection_gc, (Pixel)(XOR(color, ss->selected_graph_color)));
}


void color_graph(Pixel color)
{
  Display *dpy;
  dpy = MAIN_DISPLAY(ss);
  XSetBackground(dpy, ss->basic_gc, color);
  XSetForeground(dpy, ss->erase_gc, color);
  XSetForeground(dpy, ss->selection_gc, (Pixel)(XOR(ss->selection_color, color)));
  XSetForeground(dpy, ss->cursor_gc, (Pixel)(XOR(ss->cursor_color, color)));
  XSetForeground(dpy, ss->mark_gc, (Pixel)(XOR(ss->mark_color, color)));
}


void color_selected_graph(Pixel color)
{
  Display *dpy;
  dpy = MAIN_DISPLAY(ss);
  XSetBackground(dpy, ss->selected_basic_gc, color);
  XSetForeground(dpy, ss->selected_erase_gc, color);
  XSetForeground(dpy, ss->selected_selection_gc, (Pixel)(XOR(ss->selection_color, color)));
  XSetForeground(dpy, ss->selected_cursor_gc, (Pixel)(XOR(ss->cursor_color, color)));
  XSetForeground(dpy, ss->selected_mark_gc, (Pixel)(XOR(ss->mark_color, color)));
}


void color_data(Pixel color)
{
  Display *dpy;
  dpy = MAIN_DISPLAY(ss);
  XSetForeground(dpy, ss->basic_gc, color);
  XSetBackground(dpy, ss->erase_gc, color);
}


void color_selected_data(Pixel color)
{
  Display *dpy;
  dpy = MAIN_DISPLAY(ss);
  XSetForeground(dpy, ss->selected_basic_gc, color);
  XSetBackground(dpy, ss->selected_erase_gc, color);
}


void recolor_graph(chan_info *cp, bool selected)
{
  XtVaSetValues(channel_graph(cp), XmNbackground, (selected) ? ss->selected_graph_color : ss->graph_color, NULL);
}


void set_mix_color(Pixel color)
{
  Display *dpy;
  dpy = MAIN_DISPLAY(ss);
  ss->mix_color = color;
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->mix_color_symbol, Xen_wrap_pixel(color));
#endif
  XSetForeground(dpy, ss->mix_gc, color);
  
}


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


idle_t add_work_proc(XtWorkProc func, XtPointer data)
{
  /* during auto-testing I need to force the background procs to run to completion */
  if (with_background_processes(ss))
    return(XtAppAddWorkProc(MAIN_APP(ss), func, data));
  else
    {
      while (((*func)(data)) == BACKGROUND_CONTINUE) ;
      return((idle_t)0);
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
  str = (char *)calloc(8, sizeof(char));
  snprintf(str, 8, "%d", val);
  XmTextFieldSetString(w, str);
  free(str);
}


void widget_mus_long_t_to_text(Widget w, mus_long_t val)
{
  char *str;
  str = (char *)calloc(32, sizeof(char));
  snprintf(str, 32, "%lld", val);
  XmTextFieldSetString(w, str);
  free(str);
}


static Pixmap rotate_text(Widget w, const char *str, XFontStruct *font, mus_float_t angle_in_degrees, int *nw, int *nh, Pixel bg, Pixel fg, GC d_gc)
{
  /* rotate clockwise by angle_in_degrees degrees (i.e. 45 points text south-east), 
   * new bounding box (text centered in it) returned in nw and nh
   * bg = background color, fg = foreground (text) color) 
   */
  mus_float_t matrix[4];
  mus_float_t angle_in_radians;
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
#if HAVE_SUN
  XSync(dp, 0);
  /* needed to get the numbers drawn at all */
#endif
  XSetForeground(dp, d_gc, fg);
  XDrawImageString(dp, pix, d_gc, 4, height - 4, str, strlen(str));

  /* dump pixmap bits into an image; image data will be freed automatically later */
  data = (char *)calloc((width + 1) * (height + 1) * depth_bytes, sizeof(char)); /* not calloc since X will free this */
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


void draw_rotated_axis_label(chan_info *cp, graphics_context *ax, const char *text, int x0, int y0)
{
  Pixmap pix;
  int h = 0, w = 0;
  XGCValues gv;
  Display *dp;
  Widget widget;

  if ((cp->chan > 0) && (cp->sound->channel_style == CHANNELS_COMBINED))
    widget = channel_graph(cp->sound->chans[0]);
  else widget = channel_graph(cp);
  dp = XtDisplay(widget);
  XGetGCValues(MAIN_DISPLAY(ss), ax->gc, GCForeground | GCBackground, &gv);

  pix = rotate_text(widget, text, AXIS_LABEL_FONT(ss), -90.0, &w, &h, gv.background, gv.foreground, ax->gc);

  XCopyArea(dp, pix, XtWindow(widget), ax->gc, 0, 0, w, h, x0, y0); /* XtWindow?? */
  XFreePixmap(dp, pix);  
}


void ensure_list_row_visible(widget_t list, int pos)
{
  if (pos >= 0)
    {
      int top, visible, num_rows;
      XtVaGetValues(list,
		    XmNtopItemPosition, &top,
		    XmNvisibleItemCount, &visible,
		    XmNitemCount, &num_rows,
		    NULL);
      if (pos <= top)
	XmListSetPos(list, pos); /* was pos+1?? (new file dialog data format list off by 1 in that case) */
      else
	{
	  if (pos >= (top + visible))
	    {
	      if ((pos + visible) > num_rows)
		XmListSetBottomPos(list, num_rows);
	      else XmListSetPos(list, pos);
	    }
	}
    }
}


void ensure_scrolled_window_row_visible(widget_t list, int row, int num_rows)
{
  int minimum, maximum, value, size, new_value, increment, page_increment;
  Widget scrollbar, work_window;

  XtVaGetValues(list, 
		XmNverticalScrollBar, &scrollbar, 
		XmNworkWindow, &work_window,
		NULL);

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
  tmp = mus_strdup(s);
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
  free(tmp);
  return(xms1);
}

