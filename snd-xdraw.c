#include "snd.h"

void draw_line(axis_context *ax, int x0, int y0, int x1, int y1) 
{
  XDrawLine(ax->dp, ax->wn, ax->gc, x0, y0, x1, y1);
}


void fill_rectangle(axis_context *ax, int x0, int y0, int width, int height)
{
  XFillRectangle(ax->dp, ax->wn, ax->gc, x0, y0, width, height);
}


void erase_rectangle(chan_info *cp, axis_context *ax, int x0, int y0, int width, int height)
{
  XFillRectangle(ax->dp, ax->wn, erase_GC(cp), x0, y0, width, height);
}


void draw_string(axis_context *ax, int x0, int y0, const char *str, int len)
{
  if ((str) && (*str))
    XDrawString(ax->dp, ax->wn, ax->gc, x0, y0, str, len);
}


void gtk_style_draw_string(axis_context *ax, int x0, int y0, const char *str, int len)
{
  /* for callers of Scheme-level draw-string, the Motif and Gtk versions should agree on where "y0" is */
  XGCValues gv;
  static XFontStruct *fs = NULL;
  XGetGCValues(MAIN_DISPLAY(ss), ax->gc, GCFont, &gv);
  /* now gv.font is the default font */
  if (fs) XFree(fs);
  fs = XQueryFont(MAIN_DISPLAY(ss), gv.font);
  if (fs)
    XDrawString(ax->dp, ax->wn, ax->gc, x0, y0 + fs->ascent, str, len);
  else XDrawString(ax->dp, ax->wn, ax->gc, x0, y0, str, len); /* not sure why this happens... */
  /* XFreeFont here is trouble, but handling it as above seems ok -- Font.c in xlib does allocate new space */
}


static void draw_polygon_va(axis_context *ax, bool filled, int points, va_list ap)
{
  int i;
  XPoint *pts;
  pts = (XPoint *)CALLOC(points, sizeof(XPoint));
  for (i = 0; i < points; i++)
    {
      pts[i].x = va_arg(ap, int);
      pts[i].y = va_arg(ap, int);
    }
  if (filled)
    XFillPolygon(ax->dp, ax->wn, ax->gc, pts, points, Convex, CoordModeOrigin);
  else XDrawLines(ax->dp, ax->wn, ax->gc, pts, points, CoordModeOrigin);
  FREE(pts);
}


void fill_polygon(axis_context *ax, int points, ...)
{ /* currently used only in snd-marks.c */
  va_list ap;
  if (points == 0) return;
  va_start(ap, points);
  draw_polygon_va(ax, true, points, ap);
  va_end(ap);
}


void draw_polygon(axis_context *ax, int points, ...)
{ /* currently used only in snd-marks.c */
  va_list ap;
  if (points == 0) return;
  va_start(ap, points);
  draw_polygon_va(ax, false, points, ap);
  va_end(ap);
}


void draw_lines(axis_context *ax, point_t *points, int num)
{
  if (num == 0) return;
  XDrawLines(ax->dp, ax->wn, ax->gc, points, num, CoordModeOrigin);
}


void draw_points(axis_context *ax, point_t *points, int num, int size)
{
  if (num == 0) return;
  if (size == 1)
    XDrawPoints(ax->dp, ax->wn, ax->gc, points, num, CoordModeOrigin);
  else
    {
      int i, size2;
      XArc *rs;
      /* create squares or whatever centered on each point */
      size2 = size / 2;
      rs = (XArc *)CALLOC(num, sizeof(XArc));
      for (i = 0; i < num; i++)
	{
	  rs[i].x = points[i].x - size2;
	  rs[i].y = points[i].y - size2;
	  rs[i].angle1 = 0;
	  rs[i].angle2 = 360 * 64;
	  rs[i].width = size;
	  rs[i].height = size;
	}
      XFillArcs(ax->dp, ax->wn, ax->gc, rs, num);
      FREE(rs);
    }
}


#if 0
void draw_point(axis_context *ax, point_t point, int size)
{
  if (size == 1)
    XDrawPoint(ax->dp, ax->wn, ax->gc, point.x, point.y);
  else
    XFillArc(ax->dp, ax->wn, ax->gc, 
	     point.x - size / 2, 
	     point.y - size / 2, 
	     size, size, 0, 
	     360 * 64);
}
#endif


void draw_dot(axis_context *ax, int x, int y, int size)
{
  XFillArc(ax->dp, ax->wn, ax->gc, 
	   x - size / 2, 
	   y - size / 2, 
	   size, size, 0, 
	   360 * 64);
}


void fill_polygons(axis_context *ax, point_t *points, int num, int y0)
{
  XPoint polypts[4];
  int i;
  for (i = 1; i < num; i++)
    {
      polypts[0].x = points[i - 1].x;
      polypts[0].y = points[i - 1].y;
      polypts[1].x = points[i].x;
      polypts[1].y = points[i].y;
      polypts[2].x = polypts[1].x;
      polypts[2].y = y0;
      polypts[3].x = points[i - 1].x;
      polypts[3].y = y0;
      XFillPolygon(ax->dp, ax->wn, ax->gc, polypts, 4, Convex, CoordModeOrigin);
    }
}


void fill_two_sided_polygons(axis_context *ax, point_t *points, point_t *points1, int num)
{
  XPoint polypts[4];
  int i;
  for (i = 1; i < num; i++)
    {
      polypts[0].x = points[i - 1].x;
      polypts[0].y = points[i - 1].y;
      polypts[1].x = points[i].x;
      polypts[1].y = points[i].y;
      polypts[2].x = points1[i].x;
      polypts[2].y = points1[i].y;
      polypts[3].x = points1[i - 1].x;
      polypts[3].y = points1[i - 1].y;
      XFillPolygon(ax->dp, ax->wn, ax->gc, polypts, 4, Convex, CoordModeOrigin);
    }
}


void setup_axis_context(chan_info *cp, axis_context *ax)
{
  Widget w;
  snd_info *sp;
  sp = cp->sound;
  if (cp->tcgx) 
    w = channel_graph(sp->chans[0]);
  else w = channel_graph(cp);
  ax->dp = XtDisplay(w);
  ax->gc = copy_GC(cp);
  ax->wn = XtWindow(w);
}


/* colormaps */

#define BLACK_AND_WHITE_COLORMAP 0
/* defined as enum member in snd-gxcolormaps.c */

static int sono_bins = 0;             /* tracks total_bins -- each sono_data[i] is an array of total_bins rectangles */
static Pixel *current_colors = NULL;
static int current_colors_size = 0;
static int current_colormap = BLACK_AND_WHITE_COLORMAP;
static XRectangle **sono_data = NULL; /* each entry in sono_data is an array of colormap_size arrays: sono_data[colormap_size][total_bins] */
static int sono_colors = 0;           /* tracks colormap_size */
static GC colormap_GC;


void check_colormap_sizes(int colors)
{
  int i, old_size;
  if (current_colors_size > 0)
    {
      if (current_colormap != BLACK_AND_WHITE_COLORMAP)
	{
	  int scr;
	  Colormap cmap;
	  Display *dpy;
	  dpy = XtDisplay(MAIN_SHELL(ss));
	  scr = DefaultScreen(dpy);
	  cmap = DefaultColormap(dpy, scr);
	  XFreeColors(dpy, cmap, current_colors, current_colors_size, 0);
	  current_colormap = BLACK_AND_WHITE_COLORMAP;
	}
      if ((current_colors) && (current_colors_size < colors))
	{
	  old_size = current_colors_size;
	  current_colors_size = colors;
	  current_colors = (Pixel *)REALLOC(current_colors, current_colors_size * sizeof(Pixel));
	  for (i = old_size; i < current_colors_size; i++) current_colors[i] = 0;
	}
    }
  if ((sono_data) && (sono_colors < colors) && (sono_bins > 0))
    {
      old_size = sono_colors;
      sono_colors = colors;
      sono_data = (XRectangle **)REALLOC(sono_data, sono_colors * sizeof(XRectangle *));
      for (i = old_size; i < sono_colors; i++) sono_data[i] = (XRectangle *)CALLOC(sono_bins, sizeof(XRectangle));
    }
}


void initialize_colormap(void)
{
  state_context *sx;
  XGCValues gv;
  sx = ss->sgx;
  gv.background = sx->white;
  gv.foreground = sx->data_color;
  colormap_GC = XCreateGC(MAIN_DISPLAY(ss), XtWindow(MAIN_SHELL(ss)), GCForeground | GCBackground, &gv);
  sono_colors = color_map_size(ss);
  sono_data = (XRectangle **)CALLOC(sono_colors, sizeof(XRectangle *));
  current_colors_size = color_map_size(ss);
  current_colors = (Pixel *)CALLOC(current_colors_size, sizeof(Pixel));
}


void draw_spectro_line(axis_context *ax, int color, int x0, int y0, int x1, int y1)
{
  XSetForeground(ax->dp, colormap_GC, current_colors[color]);
  XDrawLine(ax->dp, ax->wn, colormap_GC, x0, y0, x1, y1);
}


void draw_sono_rectangles(axis_context *ax, int color, int jmax)
{
  XSetForeground(ax->dp, colormap_GC, current_colors[color]);
  XFillRectangles(ax->dp, ax->wn, colormap_GC, sono_data[color], jmax); 
}


void set_sono_rectangle(int j, int color, int x, int y, int width, int height)
{
  XRectangle *r;
  r = sono_data[color];
  r[j].x = x;
  r[j].y = y;
  r[j].width = width;
  r[j].height = height;
}


void allocate_sono_rects(int bins)
{
  if (bins != sono_bins)
    {
      int i;
      for (i = 0; i < sono_colors; i++)
	{
	  if ((sono_bins > 0) && (sono_data[i]))
	    FREE(sono_data[i]); /* each is array of XRectangle structs, but it's the wrong size */
	  sono_data[i] = (XRectangle *)CALLOC(bins, sizeof(XRectangle));
	}
      sono_bins = bins;
    }
}


void allocate_color_map(int colormap)
{
  static bool warned_color = false;
  if (current_colormap != colormap)
    {
      int i;
      Colormap cmap;
      XColor tmp_color;
      Display *dpy;
      int scr;
      tmp_color.flags = DoRed | DoGreen | DoBlue;

      dpy = XtDisplay(MAIN_SHELL(ss));
      scr = DefaultScreen(dpy);
      cmap = DefaultColormap(dpy, scr);

      /* 8-bit color displays can't handle all these colors, apparently, so we have to check status */
      if (current_colormap != BLACK_AND_WHITE_COLORMAP) XFreeColors(dpy, cmap, current_colors, current_colors_size, 0);

      for (i = 0; i < current_colors_size; i++)
	{
	  get_current_color(colormap, i, &(tmp_color.red), &(tmp_color.green), &(tmp_color.blue));
	  if ((XAllocColor(dpy, cmap, &tmp_color)) == 0) /* 0 = failure -- try black as a fallback */
	    {
	      tmp_color.red = 0;
	      tmp_color.green = 0;
	      tmp_color.blue = 0;
	      if ((XAllocColor(dpy, cmap, &tmp_color)) == 0)
		{
		  if (!warned_color)
		    snd_error_without_format(_("can't even allocate black?!?"));
		  warned_color = true;
		}
	    }
	  current_colors[i] = tmp_color.pixel;
	}
      current_colormap = colormap;
    }
}



/* -------- color browser -------- */

static XEN color_hook;

static void check_color_hook(void)
{
  if (XEN_HOOKED(color_hook))
    run_hook(color_hook, XEN_EMPTY_LIST, S_color_hook);
}


typedef struct {
  Widget dialog;
  Widget list; 
  Widget scale; 
  Widget invert;
  Widget cutoff;
} color_chooser_info;

static color_chooser_info *ccd = NULL;


static void update_graph_setting_fft_changed(chan_info *cp)
{
  cp->fft_changed = FFT_CHANGE_LOCKED;
  update_graph(cp);
}


static void invert_color_callback(Widget w, XtPointer context, XtPointer info)
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  in_set_color_inverted(cb->set);
  check_color_hook();
  for_each_chan(update_graph_setting_fft_changed);
}


void set_color_inverted(bool val)
{
  in_set_color_inverted(val);
  if (ccd) 
    XmToggleButtonSetState(ccd->invert, (Boolean)val, false);
  check_color_hook();
  if (!(ss->graph_hook_active)) 
    for_each_chan(update_graph_setting_fft_changed);
}


static void scale_color_callback(Widget w, XtPointer context, XtPointer info)
{
  Float val;
  int scale_val;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsScale(w), w);
  scale_val = cbs->value;
  if (scale_val <= 50) 
    val = (Float)(scale_val + 1) / 51.0;
  else val = 1.0 + (Float)((scale_val - 50) * (scale_val - 50)) / 12.5;
  in_set_color_scale(val);
  check_color_hook();
  for_each_chan(update_graph_setting_fft_changed);
}


static void reflect_color_scale(Float val)
{
  if (val < 0.02)
    XmScaleSetValue(ccd->scale, 0);
  else
    {
      if (val <= 1.0) 
	XmScaleSetValue(ccd->scale, mus_iclamp(0, (int)(val * 51.0 - 1), 100));
      else XmScaleSetValue(ccd->scale, mus_iclamp(0, 50 + (int)sqrt((val - 1.0) * 12.5), 100));
    }
}


void set_color_scale(Float val)
{
  in_set_color_scale(val);
  if (ccd) 
    reflect_color_scale(color_scale(ss));
  if (!(ss->graph_hook_active)) 
    for_each_chan(update_graph_setting_fft_changed);
}


static void list_color_callback(Widget w, XtPointer context, XtPointer info)
{
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsList(w), w);
  if (is_colormap(cbs->item_position - 1))
    {
      in_set_color_map(cbs->item_position - 1);
      check_color_hook();
      for_each_chan(update_graph_setting_fft_changed);
    }
}


void set_color_map(int val)
{
  in_set_color_map(val);
  if ((ccd) && (val >= 0))
    XmListSelectPos(ccd->list, val + 1, false);
  check_color_hook();
  if (!(ss->graph_hook_active)) 
    for_each_chan(update_graph_setting_fft_changed);
}


static void cutoff_color_callback(Widget w, XtPointer context, XtPointer info) /* cutoff point */
{
  /* cutoff point for color chooser */
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsScale(w), w);
  in_set_color_cutoff((Float)(cbs->value) / 1000.0);
  check_color_hook();
  for_each_chan(update_graph_setting_fft_changed);
}


void set_color_cutoff(Float val)
{
  in_set_color_cutoff(val);
  if (ccd) 
    XmScaleSetValue(ccd->cutoff, (int)(val * 1000.0));
  if (!(ss->graph_hook_active)) 
    for_each_chan(update_graph_setting_fft_changed);
}


static void dismiss_color_callback(Widget w, XtPointer context, XtPointer info)
{
  color_chooser_info *cd = (color_chooser_info *)context;
  XtUnmanageChild(cd->dialog);
}


static void help_color_callback(Widget w, XtPointer context, XtPointer info)
{
  color_dialog_help();
}


void reflect_color_list(bool setup_time)
{
  if ((ccd) && (ccd->list))
    {
      int i, size;
      XmString *cmaps;
      size = num_colormaps();
      cmaps = (XmString *)CALLOC(size, sizeof(XmString));
      for (i = 0; i < size; i++)
	cmaps[i] = XmStringCreateLocalized(colormap_name(i));
      XtVaSetValues(ccd->list, 
		    XmNitems, cmaps, 
		    XmNitemCount, size,
		    NULL);
      if (setup_time)
	XtVaSetValues(ccd->list, 
		      XmNvisibleItemCount, 6,
		      NULL);
      for (i = 0; i < size; i++) XmStringFree(cmaps[i]);
      FREE(cmaps);
    }
}


/* I tried a scrolled window with each colormap name in an appropriate color, but it looked kinda dumb */

static void start_view_color_dialog(bool managed)
{
  if (!ccd)
    {
      Arg args[32];
      int n;
      XmString xhelp, xdismiss, xcutoff, xinvert, titlestr;
      Widget mainform, light_label, sep, sep1;

      /* create color chooser dialog window */
      ccd = (color_chooser_info *)CALLOC(1, sizeof(color_chooser_info));

      xdismiss = XmStringCreateLocalized(_("Go Away")); /* needed by template dialog */
      xhelp = XmStringCreateLocalized(_("Help"));
      titlestr = XmStringCreateLocalized(_("Color"));
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNcancelLabelString, xdismiss); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNtransient, false); n++;
      ccd->dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), _("Color"), args, n);

      XtAddCallback(ccd->dialog, XmNcancelCallback, dismiss_color_callback, ccd);
      XtAddCallback(ccd->dialog, XmNhelpCallback, help_color_callback, NULL);
      XmStringFree(xhelp);
      XmStringFree(xdismiss);
      XmStringFree(titlestr);

      XtVaSetValues(XmMessageBoxGetChild(ccd->dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(ccd->dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(ccd->dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->quit_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(ccd->dialog, XmDIALOG_HELP_BUTTON), XmNbackground, ss->sgx->help_button_color, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, XmMessageBoxGetChild(ccd->dialog, XmDIALOG_SEPARATOR)); n++;
      mainform = XtCreateManagedWidget("formd", xmFormWidgetClass, ccd->dialog, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 60); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNlistMarginWidth, 3); n++;
      ccd->list = XmCreateScrolledList(mainform, "colormap-list", args, n);
      XtVaSetValues(ccd->list, 
		    XmNbackground, ss->sgx->white, 
		    XmNforeground, ss->sgx->black, 
		    NULL);
      reflect_color_list(true);
      XtAddCallback(ccd->list, XmNbrowseSelectionCallback, list_color_callback, NULL);
      XtManageChild(ccd->list);
      XmListSelectPos(ccd->list, color_map(ss) + 1, false);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, ccd->list); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, XmMessageBoxGetChild(ccd->dialog, XmDIALOG_SEPARATOR)); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
      XtSetArg(args[n], XmNwidth, 10); n++;
      sep = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, mainform, args, n);

      /* this horizontal separator exists solely to keep the "light" label from clobbering the "dark" label! */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNwidth, 250); n++;
      XtSetArg(args[n], XmNheight, 10); n++;
      sep1 = XtCreateManagedWidget("sep1", xmSeparatorWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, sep); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep1); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, XmNEAR_SLIDER); n++;
      XtSetArg(args[n], XmNvalue, 50); n++;
      ccd->scale = XtCreateManagedWidget("ccdscl", xmScaleWidgetClass, mainform, args, n);
      XtAddCallback(ccd->scale, XmNvalueChangedCallback, scale_color_callback, NULL);
      XtAddCallback(ccd->scale, XmNdragCallback, scale_color_callback, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, ccd->scale); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      light_label = XtCreateManagedWidget(_("light"), xmLabelWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, sep); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, ccd->scale); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtCreateManagedWidget(_("dark"), xmLabelWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, sep); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, light_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, XmNEAR_SLIDER); n++;
      XtSetArg(args[n], XmNmaximum, 250); n++;
      XtSetArg(args[n], XmNdecimalPoints, 3); n++;
      xcutoff = XmStringCreateLocalized(_("data cutoff"));
      XtSetArg(args[n], XmNtitleString, xcutoff); n++;
      XtSetArg(args[n], XmNvalue, (int)(color_cutoff(ss) * 1000)); n++;
      ccd->cutoff = XtCreateManagedWidget("cutoff", xmScaleWidgetClass, mainform, args, n);
      XtAddCallback(ccd->cutoff, XmNvalueChangedCallback, cutoff_color_callback, NULL);
      XtAddCallback(ccd->cutoff, XmNdragCallback, cutoff_color_callback, NULL);
      XmStringFree(xcutoff);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNselectColor, ss->sgx->pushed_button_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, ccd->cutoff); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNset, color_inverted(ss)); n++;
      xinvert = XmStringCreateLocalized(_("invert"));
      XtSetArg(args[n], XmNlabelString, xinvert); n++;
      ccd->invert = make_togglebutton_widget("invert", mainform, args, n);
      XtAddCallback(ccd->invert, XmNvalueChangedCallback, invert_color_callback, NULL);
      XmStringFree(xinvert);
      if (color_scale(ss) != 1.0)
	reflect_color_scale(color_scale(ss));

      map_over_children(ccd->dialog, set_main_color_of_widget);
      set_dialog_widget(COLOR_DIALOG, ccd->dialog);
      if (managed) XtManageChild(ccd->dialog);
    }
  else 
    {
      if (managed)
	{
	  if (!XtIsManaged(ccd->dialog)) XtManageChild(ccd->dialog);
	  raise_dialog(ccd->dialog);
	}
    }
}


void view_color_callback(Widget w, XtPointer context, XtPointer info)
{
  start_view_color_dialog(true);
}


bool color_dialog_is_active(void)
{
  return((ccd) && (ccd->dialog) && (XtIsManaged(ccd->dialog)));
}


Widget start_color_dialog(bool managed)
{
  start_view_color_dialog(managed);
  return(ccd->dialog);
}



/* -------- orientation browser -------- */

static XEN orientation_hook;

static void check_orientation_hook(void)
{
  if (XEN_HOOKED(orientation_hook))
    run_hook(orientation_hook, XEN_EMPTY_LIST, S_orientation_hook);
}


typedef struct {
  Widget dialog;
  Widget ax, ay, az, sx, sy, sz, hop, cut, glbutton; 
} orientation_info;

#define HOP_MAX 20

static orientation_info *oid = NULL;


static XmString scale_label(const char *orig_label, int value, bool dec)
{
  XmString x;
  char *lab;
  if (!dec)
    lab = mus_format("%s: %d", orig_label, value);
  else lab = mus_format("%s: %.2f", orig_label, value * 0.01);
  x = XmStringCreateLocalized(lab);
  FREE(lab);
  return(x);
}


static void scale_set_label(const char *orig_label, Widget w, int value, bool dec)
{
  /* in new motif (after version 2.1), showValue not XmNONE clobbers XmScale title! 
   *   also XmNEAR_BORDER has no effect -- same as XmNEAR_SLIDER
   * so...
   *   we create the full label by hand here.
   */

  XmString x;
  char *lab;
  if (!dec)
    lab = mus_format("%s: %d", orig_label, value);
  else lab = mus_format("%s: %.2f", orig_label, value * 0.01);
  x = XmStringCreateLocalized(lab);
  XtVaSetValues(w, XmNtitleString, x, NULL);
  FREE(lab);
  XmStringFree(x);
}


static void ax_orientation_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsScale(w), w);
  scale_set_label("x angle", w, cbs->value, false);
  in_set_spectro_x_angle((Float)(cbs->value));
  chans_field(FCP_X_ANGLE, (Float)(cbs->value));
  check_orientation_hook();
  for_each_chan(update_graph);
}


void set_spectro_x_angle(Float val)
{
  if (val < 0.0) val += 360.0; else if (val >= 360.0) val = fmod(val, 360.0);
  in_set_spectro_x_angle(val);
  if (oid) 
    {
      XmScaleSetValue(oid->ax, (int)val);
      scale_set_label("x angle", oid->ax, (int)val, false);
    }
  chans_field(FCP_X_ANGLE, val);
  check_orientation_hook();
  if (!(ss->graph_hook_active)) 
    for_each_chan(update_graph);
}


static void ay_orientation_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsScale(w), w);
  scale_set_label("y angle", w, cbs->value, false);
  in_set_spectro_y_angle((Float)(cbs->value));
  chans_field(FCP_Y_ANGLE, (Float)(cbs->value));
  check_orientation_hook();
  for_each_chan(update_graph);
}


void set_spectro_y_angle(Float val)
{
  if (val < 0.0) val += 360.0; else if (val >= 360.0) val = fmod(val, 360.0);
  in_set_spectro_y_angle(val);
  if (oid) 
    {
      XmScaleSetValue(oid->ay, (int)val);
      scale_set_label("y angle", oid->ay, (int)val, false);
    }
  chans_field(FCP_Y_ANGLE, val);
  check_orientation_hook();
  if (!(ss->graph_hook_active)) 
    for_each_chan(update_graph);
}


static void az_orientation_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsScale(w), w);
  scale_set_label("z angle", w, cbs->value, false);
  in_set_spectro_z_angle((Float)(cbs->value));
  chans_field(FCP_Z_ANGLE, (Float)(cbs->value));
  check_orientation_hook();
  for_each_chan(update_graph);
}


void set_spectro_z_angle(Float val)
{
  if (val < 0.0) val += 360.0; else if (val >= 360.0) val = fmod(val, 360.0);
  in_set_spectro_z_angle(val);
  if (oid) 
    {
      XmScaleSetValue(oid->az, (int)val);
      scale_set_label("z angle", oid->az, (int)val, false);
    }
  chans_field(FCP_Z_ANGLE, val);
  check_orientation_hook();
  if (!(ss->graph_hook_active)) 
    for_each_chan(update_graph);
}


static void sx_orientation_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsScale(w), w);
  scale_set_label("x scale", w, cbs->value, true);
  in_set_spectro_x_scale((Float)(cbs->value) * 0.01);
  chans_field(FCP_X_SCALE, (Float)(cbs->value) * 0.01);
  check_orientation_hook();
  for_each_chan(update_graph);
}


void set_spectro_x_scale(Float val)
{
  in_set_spectro_x_scale(val);
  if (oid) 
    {
      int value;
      value = mus_iclamp(0, (int)(val * 100), (int)(100 * SPECTRO_X_SCALE_MAX));
      XmScaleSetValue(oid->sx, value);
      scale_set_label("x scale", oid->sx, value, true);
    }
  chans_field(FCP_X_SCALE, val);
  check_orientation_hook();
  if (!(ss->graph_hook_active)) 
    for_each_chan(update_graph);
}


static void sy_orientation_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsScale(w), w);
  scale_set_label("y scale", w, cbs->value, true);
  in_set_spectro_y_scale((Float)(cbs->value) * 0.01);
  chans_field(FCP_Y_SCALE, (Float)(cbs->value) * 0.01);
  check_orientation_hook();
  for_each_chan(update_graph);
}


void set_spectro_y_scale(Float val)
{
  in_set_spectro_y_scale(val);
  if (oid) 
    {
      int value;
      value = mus_iclamp(0, (int)(val * 100), (int)(100 * SPECTRO_Y_SCALE_MAX));
      XmScaleSetValue(oid->sy, value);
      scale_set_label("y scale", oid->sy, value, true);
    }
  chans_field(FCP_Y_SCALE, val);
  check_orientation_hook();
  if (!(ss->graph_hook_active)) 
    for_each_chan(update_graph);
}


static void sz_orientation_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsScale(w), w);
  scale_set_label("z scale", w, cbs->value, true);
  in_set_spectro_z_scale((Float)(cbs->value) * 0.01);
  chans_field(FCP_Z_SCALE, (Float)(cbs->value) * 0.01);
  check_orientation_hook();
  for_each_chan(update_graph);
}


void set_spectro_z_scale(Float val)
{
  in_set_spectro_z_scale(val);
  if (oid) 
    {
      int value;
      value = mus_iclamp(0, (int)(val * 100), (int)(100 * SPECTRO_Z_SCALE_MAX));
      XmScaleSetValue(oid->sz, value);
      scale_set_label("z scale", oid->sz, value, true);
    }
  chans_field(FCP_Z_SCALE, val);
  check_orientation_hook();
  if (!(ss->graph_hook_active)) 
    for_each_chan(update_graph);
}


static void chans_spectro_hop(chan_info *cp, int value)
{
  cp->spectro_hop = value;
}


static void hop_orientation_callback(Widget w, XtPointer context, XtPointer info) 
{
  int val;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsScale(w), w);
  scale_set_label("hop", w, cbs->value, false);
  val = mus_iclamp(1, cbs->value, HOP_MAX);
  in_set_spectro_hop(val);
  for_each_chan_with_int(chans_spectro_hop,val);
  check_orientation_hook();
  for_each_chan(update_graph);
}


void set_spectro_hop(int val)
{
  if (val > 0)
    {
      in_set_spectro_hop(val);
      if (oid) 
	{
	  int value;
	  value = mus_iclamp(1, val, HOP_MAX);
	  XmScaleSetValue(oid->hop, value);
	  scale_set_label("hop", oid->hop, value, false);
	}
      for_each_chan_with_int(chans_spectro_hop, val);
      check_orientation_hook();
      if (!(ss->graph_hook_active)) 
	for_each_chan(update_graph);
    }
}


static void chans_spectro_cut(chan_info *cp) {cp->fft_changed = FFT_CHANGE_LOCKED;}

static void cut_orientation_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* y axis limit */
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsScale(w), w);
  scale_set_label("% of spectrum", w, cbs->value, true);
  chans_field(FCP_CUTOFF, (Float)(cbs->value) * 0.01);
  for_each_chan(chans_spectro_cut);
  check_orientation_hook();
  set_spectro_cutoff_and_redisplay((Float)(cbs->value) * 0.01); /* calls in_set... */
} 


void set_spectro_cutoff(Float val)
{
  in_set_spectro_cutoff(val);
  if (oid) 
    {
      XmScaleSetValue(oid->cut, (int)(val * 100));
      scale_set_label("% of spectrum", oid->cut, (int)(val * 100), true);
    }
  chans_field(FCP_CUTOFF, val);
  check_orientation_hook();
  if (!(ss->graph_hook_active)) 
    for_each_chan(update_graph_setting_fft_changed);
}


static int fixup_angle(Float ang)
{
  int na;
  na = (int)ang;
  if (na < 0) na += 360;
  na = na % 360;
  return(na);
}


void reflect_spectro(void)
{
  /* set color/orientaton widget values */
  if (ccd) 
    {
      XmToggleButtonSetState(ccd->invert, (Boolean)(color_inverted(ss)), false);
      XtVaSetValues(ccd->cutoff, XmNvalue, (int)((color_cutoff(ss)) * 1000), NULL);
      reflect_color_scale(color_scale(ss));
    }
  if (oid) 
    {
      XtVaSetValues(oid->ax, XmNvalue, fixup_angle(spectro_x_angle(ss)), NULL);
      XtVaSetValues(oid->ay, XmNvalue, fixup_angle(spectro_y_angle(ss)), NULL);
      XtVaSetValues(oid->az, XmNvalue, fixup_angle(spectro_z_angle(ss)), NULL);
      XtVaSetValues(oid->sx, XmNvalue, mus_iclamp(0, (int)(spectro_x_scale(ss) * 100), 100), NULL);
      XtVaSetValues(oid->sy, XmNvalue, mus_iclamp(0, (int)(spectro_y_scale(ss) * 100), 100), NULL);
      XtVaSetValues(oid->sz, XmNvalue, mus_iclamp(0, (int)(spectro_z_scale(ss) * 100), 100), NULL);
      XtVaSetValues(oid->hop, XmNvalue, mus_iclamp(1, spectro_hop(ss), HOP_MAX), NULL);
      XtVaSetValues(oid->cut, XmNvalue, mus_iclamp(0, (int)(spectro_cutoff(ss) * 100), 100), NULL);
      check_orientation_hook();
    }
}


void set_with_gl(bool val)
{
#if HAVE_GL
  sgl_save_currents();
#endif
  in_set_with_gl(val);
#if HAVE_GL
  sgl_set_currents();
  if (oid) XmToggleButtonSetState(oid->glbutton, val, false);
  /* for_each_chan(update_graph); */
#endif
} 


#if HAVE_GL
static void with_gl_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  sgl_save_currents();
  in_set_with_gl(cb->set);
  sgl_set_currents();
  for_each_chan(update_graph);
}
#endif


static void help_orientation_callback(Widget w, XtPointer context, XtPointer info) 
{
  orientation_dialog_help();
}


static void dismiss_orientation_callback(Widget w, XtPointer context, XtPointer info) 
{
  orientation_info *od = (orientation_info *)context;
  XtUnmanageChild(od->dialog);
}


static void reset_orientation_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* put everything back the way it was at the start */
  reset_spectro();
  reflect_spectro();
  for_each_chan(update_graph);
}


static void start_view_orientation_dialog(bool managed)
{
  if (!oid)
    {
      Widget mainform;
      XmString xdismiss, xhelp, xstr, xreset, titlestr;
#if HAVE_GL
      XmString glstr;
#endif
      int n, initial_value;
      Arg args[32];

      /* create orientation window */
      oid = (orientation_info *)CALLOC(1, sizeof(orientation_info));

      xdismiss = XmStringCreateLocalized(_("Go Away"));
      xhelp = XmStringCreateLocalized(_("Help"));
      xreset = XmStringCreateLocalized(_("Reset"));
      titlestr = XmStringCreateLocalized(_("Spectrogram Orientation"));

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNcancelLabelString, xdismiss); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNokLabelString, xreset); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNtransient, false); n++;
      oid->dialog = XmCreateTemplateDialog(MAIN_PANE(ss), _("Orientation"), args, n);

      XtAddCallback(oid->dialog, XmNcancelCallback, dismiss_orientation_callback, oid);
      XtAddCallback(oid->dialog, XmNhelpCallback, help_orientation_callback, NULL);
      XtAddCallback(oid->dialog, XmNokCallback, reset_orientation_callback, NULL);
      XmStringFree(xhelp);
      XmStringFree(xdismiss);
      XmStringFree(titlestr);
      XmStringFree(xreset);

      XtVaSetValues(XmMessageBoxGetChild(oid->dialog, XmDIALOG_OK_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(oid->dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(oid->dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(oid->dialog, XmDIALOG_OK_BUTTON), XmNbackground, ss->sgx->reset_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(oid->dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->quit_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(oid->dialog, XmDIALOG_HELP_BUTTON), XmNbackground, ss->sgx->help_button_color, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, XmMessageBoxGetChild(oid->dialog, XmDIALOG_SEPARATOR)); n++;
      mainform = XtCreateManagedWidget("formd", xmFormWidgetClass, oid->dialog, args, n);
      
      n = 0;
      initial_value = fixup_angle(spectro_x_angle(ss));
      xstr = scale_label("x angle", initial_value, false);
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, XmNONE); n++;
      XtSetArg(args[n], XmNvalue, initial_value); n++;
      XtSetArg(args[n], XmNmaximum, 360); n++;
      XtSetArg(args[n], XmNtitleString, xstr); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 48); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNborderWidth, 10); n++;
      XtSetArg(args[n], XmNborderColor, ss->sgx->basic_color); n++;
      oid->ax = XtCreateManagedWidget("ax", xmScaleWidgetClass, mainform, args, n);
      XtAddCallback(oid->ax, XmNvalueChangedCallback, ax_orientation_callback, NULL);
      XtAddCallback(oid->ax, XmNdragCallback, ax_orientation_callback, NULL);
      XmStringFree(xstr);


      n = 0;
      initial_value = mus_iclamp(0, (int)(spectro_x_scale(ss) * 100), (int)(100 * SPECTRO_X_SCALE_MAX));
      xstr = scale_label("x scale", initial_value, true);
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, XmNONE); n++;
      XtSetArg(args[n], XmNmaximum, (int)(100 * SPECTRO_X_SCALE_MAX)); n++;
      XtSetArg(args[n], XmNvalue, initial_value); n++;
      XtSetArg(args[n], XmNtitleString, xstr); n++;
      XtSetArg(args[n], XmNdecimalPoints, 2); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 52); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNborderWidth, 10); n++;
      XtSetArg(args[n], XmNborderColor, ss->sgx->basic_color); n++;
      oid->sx = XtCreateManagedWidget("xs", xmScaleWidgetClass, mainform, args, n);
      XtAddCallback(oid->sx, XmNvalueChangedCallback, sx_orientation_callback, NULL);
      XtAddCallback(oid->sx, XmNdragCallback, sx_orientation_callback, NULL);
      XmStringFree(xstr);


      n = 0;
      initial_value = fixup_angle(spectro_y_angle(ss));
      xstr = scale_label("y angle", initial_value, false);
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, XmNONE); n++;
      XtSetArg(args[n], XmNvalue, initial_value); n++;
      XtSetArg(args[n], XmNmaximum, 360); n++;
      XtSetArg(args[n], XmNtitleString, xstr); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 48); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNtopPosition, 25); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNborderWidth, 10); n++;
      XtSetArg(args[n], XmNborderColor, ss->sgx->basic_color); n++;
      oid->ay = XtCreateManagedWidget("ay", xmScaleWidgetClass, mainform, args, n);
      XtAddCallback(oid->ay, XmNvalueChangedCallback, ay_orientation_callback, NULL);
      XtAddCallback(oid->ay, XmNdragCallback, ay_orientation_callback, NULL);
      XmStringFree(xstr);


      n = 0;
      initial_value = mus_iclamp(0, (int)(spectro_y_scale(ss) * 100), (int)(100 * SPECTRO_Y_SCALE_MAX));
      xstr = scale_label("y scale", initial_value, true);
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, XmNONE); n++;
      XtSetArg(args[n], XmNmaximum, (int)(100 * SPECTRO_Y_SCALE_MAX)); n++;
      XtSetArg(args[n], XmNvalue, initial_value); n++;
      XtSetArg(args[n], XmNtitleString, xstr); n++;
      XtSetArg(args[n], XmNdecimalPoints, 2); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 52); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNtopPosition, 25); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNborderWidth, 10); n++;
      XtSetArg(args[n], XmNborderColor, ss->sgx->basic_color); n++;
      oid->sy = XtCreateManagedWidget("ys", xmScaleWidgetClass, mainform, args, n);
      XtAddCallback(oid->sy, XmNvalueChangedCallback, sy_orientation_callback, NULL);
      XtAddCallback(oid->sy, XmNdragCallback, sy_orientation_callback, NULL);
      XmStringFree(xstr);


      n = 0;
      initial_value = fixup_angle(spectro_z_angle(ss));
      xstr = scale_label("z angle", initial_value, false);
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNtitleString, xstr); n++;
      XtSetArg(args[n], XmNshowValue, XmNONE); n++;
      XtSetArg(args[n], XmNvalue, initial_value); n++;
      XtSetArg(args[n], XmNmaximum, 360); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 48); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNtopPosition, 50); n++;
      XtSetArg(args[n], XmNborderWidth, 10); n++;
      XtSetArg(args[n], XmNborderColor, ss->sgx->basic_color); n++;
      oid->az = XtCreateManagedWidget("az", xmScaleWidgetClass, mainform, args, n);
      XtAddCallback(oid->az, XmNvalueChangedCallback, az_orientation_callback, NULL);
      XtAddCallback(oid->az, XmNdragCallback, az_orientation_callback, NULL);
      XmStringFree(xstr);


      n = 0;
      initial_value = mus_iclamp(0, (int)(spectro_z_scale(ss) * 100), (int)(100 * SPECTRO_Z_SCALE_MAX));
      xstr = scale_label("z scale", initial_value, true);
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, XmNONE); n++;
      XtSetArg(args[n], XmNdecimalPoints, 2); n++;
      XtSetArg(args[n], XmNmaximum, (int)(100 * SPECTRO_Z_SCALE_MAX)); n++;
      XtSetArg(args[n], XmNvalue, initial_value); n++;
      XtSetArg(args[n], XmNtitleString, xstr); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 52); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNtopPosition, 50); n++;
      XtSetArg(args[n], XmNborderWidth, 10); n++;
      XtSetArg(args[n], XmNborderColor, ss->sgx->basic_color); n++;
      oid->sz = XtCreateManagedWidget("zs", xmScaleWidgetClass, mainform, args, n);
      XtAddCallback(oid->sz, XmNvalueChangedCallback, sz_orientation_callback, NULL);
      XtAddCallback(oid->sz, XmNdragCallback, sz_orientation_callback, NULL);
      XmStringFree(xstr);


      n = 0;
      initial_value = mus_iclamp(1, spectro_hop(ss), HOP_MAX);
      xstr = scale_label("hop", initial_value, false);
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, XmNONE); n++;
      XtSetArg(args[n], XmNvalue, initial_value); n++;
      XtSetArg(args[n], XmNmaximum, HOP_MAX); n++;
      XtSetArg(args[n], XmNtitleString, xstr); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 48); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNtopPosition, 75); n++;
#if HAVE_GL
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
#else
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
#endif
      XtSetArg(args[n], XmNborderWidth, 10); n++;
      XtSetArg(args[n], XmNborderColor, ss->sgx->basic_color); n++;
      oid->hop = XtCreateManagedWidget("hop", xmScaleWidgetClass, mainform, args, n);
      XtAddCallback(oid->hop, XmNvalueChangedCallback, hop_orientation_callback, NULL);
      XtAddCallback(oid->hop, XmNdragCallback, hop_orientation_callback, NULL);
      XmStringFree(xstr);


      n = 0;
      initial_value = (int)(spectro_cutoff(ss) * 100);
      xstr = scale_label("% of spectrum", initial_value, true);
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, XmNONE); n++;
      XtSetArg(args[n], XmNvalue, initial_value); n++;
      XtSetArg(args[n], XmNtitleString, xstr); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 52); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNtopPosition, 75); n++;
#if HAVE_GL
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
#else
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
#endif
      XtSetArg(args[n], XmNborderWidth, 10); n++;
      XtSetArg(args[n], XmNborderColor, ss->sgx->basic_color); n++;
      oid->cut = XtCreateManagedWidget("cut", xmScaleWidgetClass, mainform, args, n);
      XtAddCallback(oid->cut, XmNvalueChangedCallback, cut_orientation_callback, NULL);
      XtAddCallback(oid->cut, XmNdragCallback, cut_orientation_callback, NULL);
      XmStringFree(xstr);


#if HAVE_GL
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNselectColor, ss->sgx->pushed_button_color); n++;
      XtSetArg(args[n], XmNset, with_gl(ss)); n++;
      glstr = XmStringCreateLocalized(_("use OpenGL"));
      XtSetArg(args[n], XmNlabelString, glstr); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, oid->hop); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      oid->glbutton = make_togglebutton_widget("use OpenGL", mainform, args, n);
      XtAddCallback(oid->glbutton, XmNvalueChangedCallback, with_gl_callback, NULL);
      XmStringFree(glstr);
#endif

      map_over_children(oid->dialog, set_main_color_of_widget);
      set_dialog_widget(ORIENTATION_DIALOG, oid->dialog);
      if (managed) XtManageChild(oid->dialog);
    }
  else 
    {
      if (managed)
	{
	  if (!XtIsManaged(oid->dialog)) XtManageChild(oid->dialog);
	  raise_dialog(oid->dialog);
	}
    }
}


void view_orientation_callback(Widget w, XtPointer context, XtPointer info)
{
  start_view_orientation_dialog(true);
}


bool orientation_dialog_is_active(void)
{
  return((oid) && (oid->dialog) && (XtIsManaged(oid->dialog)));
}


widget_t start_orientation_dialog(bool managed)
{
  start_view_orientation_dialog(managed);
  return(oid->dialog);
}


void g_init_gxdraw(void)
{
  #define H_orientation_hook S_orientation_hook " (): called whenever one of the variables associated with the \
orientation dialog changes"
  #define H_color_hook S_color_hook " (): called whenever one of the variables associated with the \
color dialog changes"

  orientation_hook = XEN_DEFINE_HOOK(S_orientation_hook, 0, H_orientation_hook);
  color_hook = XEN_DEFINE_HOOK(S_color_hook, 0, H_color_hook);
}
