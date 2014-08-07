#include "snd.h"

#include <Xm/ScaleP.h>
/* needed to set the scale title background */


void draw_line(graphics_context *ax, int x0, int y0, int x1, int y1) 
{
  XDrawLine(ax->dp, ax->wn, ax->gc, x0, y0, x1, y1);
}


void fill_rectangle(graphics_context *ax, int x0, int y0, int width, int height)
{
  XFillRectangle(ax->dp, ax->wn, ax->gc, x0, y0, width, height);
}


void erase_rectangle(chan_info *cp, graphics_context *ax, int x0, int y0, int width, int height)
{
  XFillRectangle(ax->dp, ax->wn, erase_GC(cp), x0, y0, width, height);
}


void draw_string(graphics_context *ax, int x0, int y0, const char *str, int len)
{
  if ((str) && (*str))
    XDrawString(ax->dp, ax->wn, ax->gc, x0, y0, str, len);
}


void gtk_style_draw_string(graphics_context *ax, int x0, int y0, const char *str, int len)
{
  /* for callers of Scheme-level draw-string, the Motif and Gtk versions should agree on where "y0" is */
  XGCValues gv;
  static XFontStruct *fs = NULL;

  XGetGCValues(MAIN_DISPLAY(ss), ax->gc, GCFont, &gv);

  /* now gv.font is the default font */
  if (fs) XFree(fs);
  /*  this doesn't free all the space */
  /* but this: */
  /* if (fs) XFreeFont(MAIN_DISPLAY(ss), fs); */
  /* gets:
     X Error of failed request:  BadFont (invalid Font parameter)
     Major opcode of failed request:  56 (X_ChangeGC)
     Resource id in failed request:  0x4e0035c
     Serial number of failed request:  8479111
     Current serial number in output stream:  8479240
  */

  fs = XQueryFont(MAIN_DISPLAY(ss), gv.font);
  if (fs)
    XDrawString(ax->dp, ax->wn, ax->gc, x0, y0 + fs->ascent, str, len);
  else XDrawString(ax->dp, ax->wn, ax->gc, x0, y0, str, len); /* not sure why this happens... */

  /* XFreeFont here is trouble, but handling it as above seems ok -- Font.c in xlib does allocate new space */
}


static void draw_polygon_va(graphics_context *ax, bool filled, int points, va_list ap)
{
  int i;
  XPoint *pts;
  pts = (XPoint *)calloc(points, sizeof(XPoint));
  for (i = 0; i < points; i++)
    {
      pts[i].x = va_arg(ap, int);
      pts[i].y = va_arg(ap, int);
    }
  if (filled)
    XFillPolygon(ax->dp, ax->wn, ax->gc, pts, points, Convex, CoordModeOrigin);
  else XDrawLines(ax->dp, ax->wn, ax->gc, pts, points, CoordModeOrigin);
  free(pts);
}


void fill_polygon(graphics_context *ax, int points, ...)
{ /* currently used only in snd-marks.c */
  va_list ap;
  if (points == 0) return;
  va_start(ap, points);
  draw_polygon_va(ax, true, points, ap);
  va_end(ap);
}

#if 0
void draw_polygon(graphics_context *ax, int points, ...)
{ 
  va_list ap;
  if (points == 0) return;
  va_start(ap, points);
  draw_polygon_va(ax, false, points, ap);
  va_end(ap);
}
#endif

void draw_lines(graphics_context *ax, point_t *points, int num)
{
  if (num == 0) return;
  XDrawLines(ax->dp, ax->wn, ax->gc, points, num, CoordModeOrigin);
}


void draw_points(graphics_context *ax, point_t *points, int num, int size)
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
      rs = (XArc *)calloc(num, sizeof(XArc));
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
      free(rs);
    }
}


#if 0
void draw_point(graphics_context *ax, point_t point, int size)
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


void draw_dot(graphics_context *ax, int x, int y, int size)
{
  XFillArc(ax->dp, ax->wn, ax->gc, 
	   x - size / 2, 
	   y - size / 2, 
	   size, size, 0, 
	   360 * 64);
}


void fill_polygons(graphics_context *ax, point_t *points, int num, int y0)
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


void fill_two_sided_polygons(graphics_context *ax, point_t *points, point_t *points1, int num)
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


void setup_graphics_context(chan_info *cp, graphics_context *ax)
{
  Widget w;
  w = channel_to_widget(cp);
  ax->dp = XtDisplay(w);
  ax->gc = copy_GC(cp);
  ax->wn = XtWindow(w);
}


/* colormaps */

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
	  current_colors = (Pixel *)realloc(current_colors, current_colors_size * sizeof(Pixel));
	  for (i = old_size; i < current_colors_size; i++) current_colors[i] = 0;
	}
    }
  if ((sono_data) && (sono_colors < colors) && (sono_bins > 0))
    {
      old_size = sono_colors;
      sono_colors = colors;
      sono_data = (XRectangle **)realloc(sono_data, sono_colors * sizeof(XRectangle *));
      for (i = old_size; i < sono_colors; i++) sono_data[i] = (XRectangle *)calloc(sono_bins, sizeof(XRectangle));
    }
}


void initialize_colormap(void)
{
  XGCValues gv;
  gv.background = ss->white;
  gv.foreground = ss->data_color;
  colormap_GC = XCreateGC(MAIN_DISPLAY(ss), XtWindow(MAIN_SHELL(ss)), GCForeground | GCBackground, &gv);
  sono_colors = color_map_size(ss);
  sono_data = (XRectangle **)calloc(sono_colors, sizeof(XRectangle *));
  current_colors_size = color_map_size(ss);
  current_colors = (Pixel *)calloc(current_colors_size, sizeof(Pixel));
}


void draw_spectro_line(graphics_context *ax, int color, int x0, int y0, int x1, int y1)
{
  XSetForeground(ax->dp, colormap_GC, current_colors[color]);
  XDrawLine(ax->dp, ax->wn, colormap_GC, x0, y0, x1, y1);
}


void draw_sono_rectangles(graphics_context *ax, int color, int jmax)
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
	    free(sono_data[i]); /* each is array of XRectangle structs, but it's the wrong size */
	  sono_data[i] = (XRectangle *)calloc(bins, sizeof(XRectangle));
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
      if (current_colormap != BLACK_AND_WHITE_COLORMAP) 
	XFreeColors(dpy, cmap, current_colors, current_colors_size, 0);

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
		    snd_error_without_format("can't even allocate black?!?");
		  warned_color = true;
		}
	    }
	  current_colors[i] = tmp_color.pixel;
	}
      current_colormap = colormap;
    }
}


void draw_colored_lines(chan_info *cp, graphics_context *ax, point_t *points, int num, int *colors, int axis_y0, color_t default_color)
{
  int i, x0, y0, y2 = 0, y00 = -1, cur, prev;
  color_t old_color;

  if (num <= 0) return;

  old_color = get_foreground_color(ax);

  x0 = points[0].x;
  y0 = points[0].y;

  if (abs(y0 - axis_y0) < 5)
    prev = -1;
  else prev = colors[0];

  set_foreground_color(ax, (prev == -1) ? default_color : current_colors[prev]);

  for (i = 1; i < num; i++)
    {
      int x1, y1;
      x1 = points[i].x;
      y1 = points[i].y;
      if (i < num - 1)
	y2 = points[i + 1].y;
      else y2 = y1;

      if ((abs(y0 - axis_y0) < 5) &&
	  (abs(y1 - axis_y0) < 5))
	cur = -1;
      else 
	{
	  if ((y00 > y0) &&
	      (y00 > y1) &&
	      (i > 1))
	    cur = colors[i - 2];
	  else
	    {
	      if ((y2 > y1) &&
		  (y2 > y0))
		cur = colors[i + 1];
	      else
		{
		  if (y0 > y1)
		    cur = colors[i];
		  else cur = colors[i - 1]; /* coords are upside down */
		}
	    }
	}

      if (cur != prev)
	{
	  set_foreground_color(ax, (cur == -1) ? default_color : current_colors[cur]);
	  prev = cur;
	}

      if (cp->transform_graph_style == GRAPH_DOTS)
	draw_dot(ax, x0, y0, cp->dot_size);
      else draw_line(ax, x0, y0, x1, y1);

      y00 = y0;
      x0 = x1;
      y0 = y1;
    }

  set_foreground_color(ax, old_color);
}



/* -------- color/orientation browser -------- */

static Xen color_hook;

static void check_color_hook(void)
{
  if (Xen_hook_has_list(color_hook))
    run_hook(color_hook, Xen_empty_list, S_color_hook);
}


static Widget ccd_dialog = NULL, ccd_list, ccd_scale, ccd_invert, ccd_cutoff;

static void update_graph_setting_fft_changed(chan_info *cp)
{
  cp->fft_changed = FFT_CHANGE_LOCKED;
  update_graph(cp);
}


static void invert_color_callback(Widget w, XtPointer context, XtPointer info)
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  in_set_color_inverted(cb->set);
  check_color_hook();
  for_each_chan(update_graph_setting_fft_changed);
}


void set_color_inverted(bool val)
{
  in_set_color_inverted(val);
  if (ccd_dialog) 
    XmToggleButtonSetState(ccd_invert, (Boolean)val, false);
  check_color_hook();
  if (!(ss->graph_hook_active)) 
    for_each_chan(update_graph_setting_fft_changed);
}


static void scale_color_callback(Widget w, XtPointer context, XtPointer info)
{
  mus_float_t val;
  int scale_val;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  scale_val = cbs->value;
  if (scale_val <= 50) 
    val = (mus_float_t)(scale_val + 1) / 51.0;
  else val = 1.0 + (mus_float_t)((scale_val - 50) * (scale_val - 50)) / 12.5;
  in_set_color_scale(val);
  check_color_hook();
  for_each_chan(update_graph_setting_fft_changed);
}


static void reflect_color_scale(mus_float_t val)
{
  if (val < 0.02)
    XmScaleSetValue(ccd_scale, 0);
  else
    {
      if (val <= 1.0) 
	XmScaleSetValue(ccd_scale, mus_iclamp(0, (int)(val * 51.0 - 1), 100));
      else XmScaleSetValue(ccd_scale, mus_iclamp(0, 50 + (int)sqrt((val - 1.0) * 12.5), 100));
    }
}


void set_color_scale(mus_float_t val)
{
  in_set_color_scale(val);
  if (ccd_dialog) 
    reflect_color_scale(color_scale(ss));
  if (!(ss->graph_hook_active)) 
    for_each_chan(update_graph_setting_fft_changed);
}


static void list_color_callback(Widget w, XtPointer context, XtPointer info)
{
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
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
  if ((ccd_dialog) && (val >= 0))
    XmListSelectPos(ccd_list, val + 1, false);
  check_color_hook();
  if (!(ss->graph_hook_active)) 
    for_each_chan(update_graph_setting_fft_changed);
}


static XmString fscale_label(const char *orig_label, mus_float_t value)
{
  XmString x;
  char *lab;
  lab = mus_format("%s: %.3f", orig_label, value);
  x = XmStringCreateLocalized(lab);
  free(lab);
  return(x);
}


static void fscale_set_label(const char *orig_label, Widget w, mus_float_t value)
{
  XmString x;
  char *lab;
  lab = mus_format("%s: %.3f", orig_label, value);
  x = XmStringCreateLocalized(lab);
  XtVaSetValues(w, XmNtitleString, x, NULL);
  free(lab);
  XmStringFree(x);
}


static void cutoff_color_callback(Widget w, XtPointer context, XtPointer info) /* cutoff point */
{
  /* cutoff point for color chooser */
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  in_set_color_cutoff((mus_float_t)(cbs->value) / 1000.0);
  fscale_set_label("data cutoff", w, color_cutoff(ss));
  check_color_hook();
  for_each_chan(update_graph_setting_fft_changed);
}


void set_color_cutoff(mus_float_t val)
{
  in_set_color_cutoff(val);
  if (ccd_dialog) 
    XmScaleSetValue(ccd_cutoff, (int)(val * 1000.0));
  if (!(ss->graph_hook_active)) 
    for_each_chan(update_graph_setting_fft_changed);
}


static void dismiss_color_orientation_callback(Widget w, XtPointer context, XtPointer info)
{
  XtUnmanageChild(ccd_dialog);
}


static void help_color_orientation_callback(Widget w, XtPointer context, XtPointer info)
{
  color_orientation_dialog_help();
}


void reflect_color_list(bool setup_time)
{
  if ((ccd_dialog) && (ccd_list))
    {
      int i, size;
      XmString *cmaps;
      size = num_colormaps();
      cmaps = (XmString *)calloc(size, sizeof(XmString));
      for (i = 0; i < size; i++)
	cmaps[i] = XmStringCreateLocalized(colormap_name(i));
      XtVaSetValues(ccd_list, 
		    XmNitems, cmaps, 
		    XmNitemCount, size,
		    NULL);
      if (setup_time)
	XtVaSetValues(ccd_list, 
		      XmNvisibleItemCount, 6,
		      NULL);
      for (i = 0; i < size; i++) XmStringFree(cmaps[i]);
      free(cmaps);
    }
}


static Xen orientation_hook;

static void check_orientation_hook(void)
{
  if (Xen_hook_has_list(orientation_hook))
    run_hook(orientation_hook, Xen_empty_list, S_orientation_hook);
}


static Widget oid_ax, oid_ay, oid_az, oid_sx, oid_sy, oid_sz, oid_hop;
#if HAVE_GL
  static Widget oid_glbutton; 
#endif

#define HOP_MAX 20

static XmString scale_label(const char *orig_label, int value, bool dec)
{
  XmString x;
  char *lab;
  if (!dec)
    lab = mus_format("%s: %d", orig_label, value);
  else lab = mus_format("%s: %.2f", orig_label, value * 0.01);
  x = XmStringCreateLocalized(lab);
  free(lab);
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
  free(lab);
  XmStringFree(x);
}


static void ax_orientation_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  scale_set_label("x angle", w, cbs->value, false);
  in_set_spectro_x_angle((mus_float_t)(cbs->value));
  chans_field(FCP_X_ANGLE, (mus_float_t)(cbs->value));
  check_orientation_hook();
  for_each_chan(update_graph);
}


void set_spectro_x_angle(mus_float_t val)
{
  if (val < 0.0) val += 360.0; else if (val >= 360.0) val = fmod(val, 360.0);
  in_set_spectro_x_angle(val);
  if (ccd_dialog) 
    {
      XmScaleSetValue(oid_ax, (int)val);
      scale_set_label("x angle", oid_ax, (int)val, false);
    }
  chans_field(FCP_X_ANGLE, val);
  check_orientation_hook();
  if (!(ss->graph_hook_active)) 
    for_each_chan(update_graph);
}


static void ay_orientation_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  scale_set_label("y angle", w, cbs->value, false);
  in_set_spectro_y_angle((mus_float_t)(cbs->value));
  chans_field(FCP_Y_ANGLE, (mus_float_t)(cbs->value));
  check_orientation_hook();
  for_each_chan(update_graph);
}


void set_spectro_y_angle(mus_float_t val)
{
  if (val < 0.0) val += 360.0; else if (val >= 360.0) val = fmod(val, 360.0);
  in_set_spectro_y_angle(val);
  if (ccd_dialog) 
    {
      XmScaleSetValue(oid_ay, (int)val);
      scale_set_label("y angle", oid_ay, (int)val, false);
    }
  chans_field(FCP_Y_ANGLE, val);
  check_orientation_hook();
  if (!(ss->graph_hook_active)) 
    for_each_chan(update_graph);
}


static void az_orientation_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  scale_set_label("z angle", w, cbs->value, false);
  in_set_spectro_z_angle((mus_float_t)(cbs->value));
  chans_field(FCP_Z_ANGLE, (mus_float_t)(cbs->value));
  check_orientation_hook();
  for_each_chan(update_graph);
}


void set_spectro_z_angle(mus_float_t val)
{
  if (val < 0.0) val += 360.0; else if (val >= 360.0) val = fmod(val, 360.0);
  in_set_spectro_z_angle(val);
  if (ccd_dialog) 
    {
      XmScaleSetValue(oid_az, (int)val);
      scale_set_label("z angle", oid_az, (int)val, false);
    }
  chans_field(FCP_Z_ANGLE, val);
  check_orientation_hook();
  if (!(ss->graph_hook_active)) 
    for_each_chan(update_graph);
}


static void sx_orientation_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  scale_set_label("x scale", w, cbs->value, true);
  in_set_spectro_x_scale((mus_float_t)(cbs->value) * 0.01);
  chans_field(FCP_X_SCALE, (mus_float_t)(cbs->value) * 0.01);
  check_orientation_hook();
  for_each_chan(update_graph);
}


void set_spectro_x_scale(mus_float_t val)
{
  in_set_spectro_x_scale(val);
  if (ccd_dialog) 
    {
      int value;
      value = mus_iclamp(0, (int)(val * 100), (int)(100 * SPECTRO_X_SCALE_MAX));
      XmScaleSetValue(oid_sx, value);
      scale_set_label("x scale", oid_sx, value, true);
    }
  chans_field(FCP_X_SCALE, val);
  check_orientation_hook();
  if (!(ss->graph_hook_active)) 
    for_each_chan(update_graph);
}


static void sy_orientation_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  scale_set_label("y scale", w, cbs->value, true);
  in_set_spectro_y_scale((mus_float_t)(cbs->value) * 0.01);
  chans_field(FCP_Y_SCALE, (mus_float_t)(cbs->value) * 0.01);
  check_orientation_hook();
  for_each_chan(update_graph);
}


void set_spectro_y_scale(mus_float_t val)
{
  in_set_spectro_y_scale(val);
  if (ccd_dialog) 
    {
      int value;
      value = mus_iclamp(0, (int)(val * 100), (int)(100 * SPECTRO_Y_SCALE_MAX));
      XmScaleSetValue(oid_sy, value);
      scale_set_label("y scale", oid_sy, value, true);
    }
  chans_field(FCP_Y_SCALE, val);
  check_orientation_hook();
  if (!(ss->graph_hook_active)) 
    for_each_chan(update_graph);
}


static void sz_orientation_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  scale_set_label("z scale", w, cbs->value, true);
  in_set_spectro_z_scale((mus_float_t)(cbs->value) * 0.01);
  chans_field(FCP_Z_SCALE, (mus_float_t)(cbs->value) * 0.01);
  check_orientation_hook();
  for_each_chan(update_graph);
}


void set_spectro_z_scale(mus_float_t val)
{
  in_set_spectro_z_scale(val);
  if (ccd_dialog) 
    {
      int value;
      value = mus_iclamp(0, (int)(val * 100), (int)(100 * SPECTRO_Z_SCALE_MAX));
      XmScaleSetValue(oid_sz, value);
      scale_set_label("z scale", oid_sz, value, true);
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
      if (ccd_dialog) 
	{
	  int value;
	  value = mus_iclamp(1, val, HOP_MAX);
	  XmScaleSetValue(oid_hop, value);
	  scale_set_label("hop", oid_hop, value, false);
	}
      for_each_chan_with_int(chans_spectro_hop, val);
      check_orientation_hook();
      if (!(ss->graph_hook_active)) 
	for_each_chan(update_graph);
    }
}


static int fixup_angle(mus_float_t ang)
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
  if (ccd_dialog) 
    {
      XmToggleButtonSetState(ccd_invert, (Boolean)(color_inverted(ss)), false);
      XtVaSetValues(ccd_cutoff, XmNvalue, (int)((color_cutoff(ss)) * 1000), NULL);
      reflect_color_scale(color_scale(ss));

      XtVaSetValues(oid_ax, XmNvalue, fixup_angle(spectro_x_angle(ss)), NULL);
      XtVaSetValues(oid_ay, XmNvalue, fixup_angle(spectro_y_angle(ss)), NULL);
      XtVaSetValues(oid_az, XmNvalue, fixup_angle(spectro_z_angle(ss)), NULL);
      XtVaSetValues(oid_sx, XmNvalue, mus_iclamp(0, (int)(spectro_x_scale(ss) * 100), 100), NULL);
      XtVaSetValues(oid_sy, XmNvalue, mus_iclamp(0, (int)(spectro_y_scale(ss) * 100), 100), NULL);
      XtVaSetValues(oid_sz, XmNvalue, mus_iclamp(0, (int)(spectro_z_scale(ss) * 100), 100), NULL);
      XtVaSetValues(oid_hop, XmNvalue, mus_iclamp(1, spectro_hop(ss), HOP_MAX), NULL);
      check_orientation_hook();
    }
}


void set_with_gl(bool val, bool with_dialogs)
{
#if HAVE_GL
  sgl_save_currents();
#endif
  in_set_with_gl(val);
#if HAVE_GL
  sgl_set_currents(with_dialogs);
  if ((ccd_dialog) && (with_dialogs))
    XmToggleButtonSetState(oid_glbutton, val, false);
#endif
} 


#if HAVE_GL
static void with_gl_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  sgl_save_currents();
  in_set_with_gl(cb->set);
  sgl_set_currents(true);
  /* this only sets the slider positions -- it doesn't update the labels! */
  /*   and  reflect_spectro() doesn't help! */
  if (ccd_dialog)
    {
      scale_set_label("x angle", oid_ax, spectro_x_angle(ss), false);
      scale_set_label("y angle", oid_ay, spectro_y_angle(ss), false);
      scale_set_label("z angle", oid_az, spectro_z_angle(ss), false);
      scale_set_label("x scale", oid_sx, spectro_x_scale(ss), false);
      scale_set_label("y scale", oid_sy, spectro_y_scale(ss), false);
      scale_set_label("z scale", oid_sz, spectro_z_scale(ss), false);
    }
  for_each_chan(update_graph);
}
#endif


static void reset_color_orientation_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* put everything back the way it was at the start.
   *     this sets everything to the startup defaults -- should they be the dialog startup values instead? 
   */
  set_color_cutoff(DEFAULT_COLOR_CUTOFF);
  set_color_inverted(DEFAULT_COLOR_INVERTED);
  set_color_scale(DEFAULT_COLOR_SCALE);
  set_color_map(DEFAULT_COLOR_MAP);

  reset_spectro(); 
  reflect_spectro();
  for_each_chan(update_graph);
}



/* I tried a scrolled window with each colormap name in an appropriate color, but it looked kinda dumb */

Widget make_color_orientation_dialog(bool managed)
{
  if (!ccd_dialog)
    {
      Arg args[32];
      int n, initial_value;
      XmString xhelp, xdismiss, xinvert, titlestr, xreset, xstr;
      Widget mainform, light_label, lsep, rsep, sep1, tsep, color_frame, orientation_frame, color_form, orientation_form;
      Widget color_title, orientation_title;
#if HAVE_GL
      XmString glstr;
#endif

      xdismiss = XmStringCreateLocalized((char *)I_GO_AWAY); /* needed by template dialog */
      xhelp = XmStringCreateLocalized((char *)I_HELP);
      xreset = XmStringCreateLocalized((char *)"Reset");
      titlestr = XmStringCreateLocalized((char *)"Color and Orientation");

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNcancelLabelString, xdismiss); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNokLabelString, xreset); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNtransient, false); n++;
      ccd_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), (char *)"Color and Orientation", args, n);

      XtAddCallback(ccd_dialog, XmNcancelCallback, dismiss_color_orientation_callback, NULL);
      XtAddCallback(ccd_dialog, XmNhelpCallback, help_color_orientation_callback, NULL);
      XtAddCallback(ccd_dialog, XmNokCallback, reset_color_orientation_callback, NULL);

      XmStringFree(xhelp);
      XmStringFree(xdismiss);
      XmStringFree(titlestr);
      XmStringFree(xreset);

      XtVaSetValues(XmMessageBoxGetChild(ccd_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, ss->selection_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(ccd_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, ss->selection_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(ccd_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->highlight_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(ccd_dialog, XmDIALOG_HELP_BUTTON), XmNbackground, ss->highlight_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(ccd_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, ss->selection_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(ccd_dialog, XmDIALOG_OK_BUTTON), XmNbackground, ss->highlight_color, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, XmMessageBoxGetChild(ccd_dialog, XmDIALOG_SEPARATOR)); n++;
      mainform = XtCreateManagedWidget("formd", xmFormWidgetClass, ccd_dialog, args, n);

      /* color section */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNborderWidth, 10); n++;
      XtSetArg(args[n], XmNborderColor, ss->basic_color); n++;
      color_frame = XtCreateManagedWidget("color", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      color_form = XtCreateManagedWidget("cform", xmFormWidgetClass, color_frame, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;
      color_title = XtCreateManagedWidget("colors", xmLabelWidgetClass, color_form, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 60); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, color_title); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNlistMarginWidth, 3); n++;
      ccd_list = XmCreateScrolledList(color_form, (char *)"colormap-list", args, n);

      XtVaSetValues(ccd_list, 
		    XmNbackground, ss->white, 
		    XmNforeground, ss->black, 
		    NULL);
      reflect_color_list(true);
      XtAddCallback(ccd_list, XmNbrowseSelectionCallback, list_color_callback, NULL);
      XtManageChild(ccd_list);
      XmListSelectPos(ccd_list, color_map(ss) + 1, false);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, color_title); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
      XtSetArg(args[n], XmNwidth, 10); n++;
      lsep = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, color_form, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, ccd_list); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, color_title); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
      XtSetArg(args[n], XmNwidth, 10); n++;
      rsep = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, color_form, args, n);

      /* this horizontal separator exists solely to keep the "light" label from clobbering the "dark" label! */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, lsep); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, rsep); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, color_title); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNwidth, 250); n++;
      XtSetArg(args[n], XmNheight, 10); n++;
      sep1 = XtCreateManagedWidget("sep1", xmSeparatorWidgetClass, color_form, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, lsep); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, rsep); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep1); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, XmNEAR_SLIDER); n++;
      XtSetArg(args[n], XmNvalue, 50); n++;
      ccd_scale = XtCreateManagedWidget("ccdscl", xmScaleWidgetClass, color_form, args, n);
      XtAddCallback(ccd_scale, XmNvalueChangedCallback, scale_color_callback, NULL);
      XtAddCallback(ccd_scale, XmNdragCallback, scale_color_callback, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, lsep); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, ccd_scale); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      light_label = XtCreateManagedWidget("light", xmLabelWidgetClass, color_form, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, rsep); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, ccd_scale); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtCreateManagedWidget("dark", xmLabelWidgetClass, color_form, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, lsep); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, rsep); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, light_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNwidth, 250); n++;
      XtSetArg(args[n], XmNheight, 10); n++;
      tsep = XtCreateManagedWidget("tsep", xmSeparatorWidgetClass, color_form, args, n);

      n = 0;
      xstr = fscale_label("data cutoff", color_cutoff(ss));
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, lsep); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, rsep); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, tsep); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, XmNONE); n++;
      XtSetArg(args[n], XmNmaximum, 250); n++;
      XtSetArg(args[n], XmNdecimalPoints, 3); n++;
      XtSetArg(args[n], XmNtitleString, xstr); n++;
      XtSetArg(args[n], XmNvalue, (int)(color_cutoff(ss) * 1000)); n++;
      ccd_cutoff = XtCreateManagedWidget("cutoff", xmScaleWidgetClass, color_form, args, n);
      XtAddCallback(ccd_cutoff, XmNvalueChangedCallback, cutoff_color_callback, NULL);
      XtAddCallback(ccd_cutoff, XmNdragCallback, cutoff_color_callback, NULL);
      XmStringFree(xstr);

      XtVaSetValues(((XmScaleWidget)ccd_cutoff)->composite.children[0], XmNbackground, ss->basic_color, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNselectColor, ss->selection_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, lsep); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, ccd_cutoff); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNset, color_inverted(ss)); n++;
      xinvert = XmStringCreateLocalized((char *)"invert");
      XtSetArg(args[n], XmNlabelString, xinvert); n++;
      ccd_invert = make_togglebutton_widget("invert", color_form, args, n);
      XtAddCallback(ccd_invert, XmNvalueChangedCallback, invert_color_callback, NULL);
      XmStringFree(xinvert);


      /* orientation section */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, color_frame); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNborderWidth, 10); n++;
      XtSetArg(args[n], XmNborderColor, ss->basic_color); n++;
      orientation_frame = XtCreateManagedWidget("color", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      orientation_form = XtCreateManagedWidget("oform", xmFormWidgetClass, orientation_frame, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;
      orientation_title = XtCreateManagedWidget("orientation", xmLabelWidgetClass, orientation_form, args, n);

      #define SCALE_BORDER_WIDTH 6

      n = 0;
      initial_value = fixup_angle(spectro_x_angle(ss));
      xstr = scale_label("x angle", initial_value, false);
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, XmNONE); n++;
      XtSetArg(args[n], XmNvalue, initial_value); n++;
      XtSetArg(args[n], XmNmaximum, 360); n++;
      XtSetArg(args[n], XmNtitleString, xstr); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 48); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, orientation_title); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNborderWidth, SCALE_BORDER_WIDTH); n++;
      XtSetArg(args[n], XmNborderColor, ss->basic_color); n++;
      oid_ax = XtCreateManagedWidget("ax", xmScaleWidgetClass, orientation_form, args, n);
      XtAddCallback(oid_ax, XmNvalueChangedCallback, ax_orientation_callback, NULL);
      XtAddCallback(oid_ax, XmNdragCallback, ax_orientation_callback, NULL);
      XmStringFree(xstr);

      XtVaSetValues(((XmScaleWidget)oid_ax)->composite.children[0], XmNbackground, ss->basic_color, NULL);

      n = 0;
      initial_value = mus_iclamp(0, (int)(spectro_x_scale(ss) * 100), (int)(100 * SPECTRO_X_SCALE_MAX));
      xstr = scale_label("x scale", initial_value, true);
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, XmNONE); n++;
      XtSetArg(args[n], XmNmaximum, (int)(100 * SPECTRO_X_SCALE_MAX)); n++;
      XtSetArg(args[n], XmNvalue, initial_value); n++;
      XtSetArg(args[n], XmNtitleString, xstr); n++;
      XtSetArg(args[n], XmNdecimalPoints, 2); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 52); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, oid_ax); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNborderWidth, SCALE_BORDER_WIDTH); n++;
      XtSetArg(args[n], XmNborderColor, ss->basic_color); n++;
      oid_sx = XtCreateManagedWidget("xs", xmScaleWidgetClass, orientation_form, args, n);
      XtAddCallback(oid_sx, XmNvalueChangedCallback, sx_orientation_callback, NULL);
      XtAddCallback(oid_sx, XmNdragCallback, sx_orientation_callback, NULL);
      XmStringFree(xstr);

      XtVaSetValues(((XmScaleWidget)oid_sx)->composite.children[0], XmNbackground, ss->basic_color, NULL);

      n = 0;
      initial_value = fixup_angle(spectro_y_angle(ss));
      xstr = scale_label("y angle", initial_value, false);
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, XmNONE); n++;
      XtSetArg(args[n], XmNvalue, initial_value); n++;
      XtSetArg(args[n], XmNmaximum, 360); n++;
      XtSetArg(args[n], XmNtitleString, xstr); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 48); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, oid_ax); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNborderWidth, SCALE_BORDER_WIDTH); n++;
      XtSetArg(args[n], XmNborderColor, ss->basic_color); n++;
      oid_ay = XtCreateManagedWidget("ay", xmScaleWidgetClass, orientation_form, args, n);
      XtAddCallback(oid_ay, XmNvalueChangedCallback, ay_orientation_callback, NULL);
      XtAddCallback(oid_ay, XmNdragCallback, ay_orientation_callback, NULL);
      XmStringFree(xstr);

      XtVaSetValues(((XmScaleWidget)oid_ay)->composite.children[0], XmNbackground, ss->basic_color, NULL);

      n = 0;
      initial_value = mus_iclamp(0, (int)(spectro_y_scale(ss) * 100), (int)(100 * SPECTRO_Y_SCALE_MAX));
      xstr = scale_label("y scale", initial_value, true);
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, XmNONE); n++;
      XtSetArg(args[n], XmNmaximum, (int)(100 * SPECTRO_Y_SCALE_MAX)); n++;
      XtSetArg(args[n], XmNvalue, initial_value); n++;
      XtSetArg(args[n], XmNtitleString, xstr); n++;
      XtSetArg(args[n], XmNdecimalPoints, 2); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 52); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, oid_sx); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNborderWidth, SCALE_BORDER_WIDTH); n++;
      XtSetArg(args[n], XmNborderColor, ss->basic_color); n++;
      oid_sy = XtCreateManagedWidget("ys", xmScaleWidgetClass, orientation_form, args, n);
      XtAddCallback(oid_sy, XmNvalueChangedCallback, sy_orientation_callback, NULL);
      XtAddCallback(oid_sy, XmNdragCallback, sy_orientation_callback, NULL);
      XmStringFree(xstr);

      XtVaSetValues(((XmScaleWidget)oid_sy)->composite.children[0], XmNbackground, ss->basic_color, NULL);

      n = 0;
      initial_value = fixup_angle(spectro_z_angle(ss));
      xstr = scale_label("z angle", initial_value, false);
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNtitleString, xstr); n++;
      XtSetArg(args[n], XmNshowValue, XmNONE); n++;
      XtSetArg(args[n], XmNvalue, initial_value); n++;
      XtSetArg(args[n], XmNmaximum, 360); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 48); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, oid_ay); n++;
      XtSetArg(args[n], XmNborderWidth, SCALE_BORDER_WIDTH); n++;
      XtSetArg(args[n], XmNborderColor, ss->basic_color); n++;
      oid_az = XtCreateManagedWidget("az", xmScaleWidgetClass, orientation_form, args, n);
      XtAddCallback(oid_az, XmNvalueChangedCallback, az_orientation_callback, NULL);
      XtAddCallback(oid_az, XmNdragCallback, az_orientation_callback, NULL);
      XmStringFree(xstr);

      XtVaSetValues(((XmScaleWidget)oid_az)->composite.children[0], XmNbackground, ss->basic_color, NULL);

      n = 0;
      initial_value = mus_iclamp(0, (int)(spectro_z_scale(ss) * 100), (int)(100 * SPECTRO_Z_SCALE_MAX));
      xstr = scale_label("z scale", initial_value, true);
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
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
      XtSetArg(args[n], XmNtopWidget, oid_sy); n++;
      XtSetArg(args[n], XmNborderWidth, SCALE_BORDER_WIDTH); n++;
      XtSetArg(args[n], XmNborderColor, ss->basic_color); n++;
      oid_sz = XtCreateManagedWidget("zs", xmScaleWidgetClass, orientation_form, args, n);
      XtAddCallback(oid_sz, XmNvalueChangedCallback, sz_orientation_callback, NULL);
      XtAddCallback(oid_sz, XmNdragCallback, sz_orientation_callback, NULL);
      XmStringFree(xstr);

      XtVaSetValues(((XmScaleWidget)oid_sz)->composite.children[0], XmNbackground, ss->basic_color, NULL);

      n = 0;
      initial_value = mus_iclamp(1, spectro_hop(ss), HOP_MAX);
      xstr = scale_label("hop", initial_value, false);
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, XmNONE); n++;
      XtSetArg(args[n], XmNvalue, initial_value); n++;
      XtSetArg(args[n], XmNmaximum, HOP_MAX); n++;
      XtSetArg(args[n], XmNtitleString, xstr); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 48); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, oid_az); n++;
#if HAVE_GL
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
#else
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
#endif
      XtSetArg(args[n], XmNborderWidth, SCALE_BORDER_WIDTH); n++;
      XtSetArg(args[n], XmNborderColor, ss->basic_color); n++;
      oid_hop = XtCreateManagedWidget("hop", xmScaleWidgetClass, orientation_form, args, n);
      XtAddCallback(oid_hop, XmNvalueChangedCallback, hop_orientation_callback, NULL);
      XtAddCallback(oid_hop, XmNdragCallback, hop_orientation_callback, NULL);
      XmStringFree(xstr);

      XtVaSetValues(((XmScaleWidget)oid_hop)->composite.children[0], XmNbackground, ss->basic_color, NULL);

#if HAVE_GL
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNselectColor, ss->selection_color); n++;
      XtSetArg(args[n], XmNset, with_gl(ss)); n++;
      glstr = XmStringCreateLocalized((char *)"use OpenGL");
      XtSetArg(args[n], XmNlabelString, glstr); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, oid_hop); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      oid_glbutton = make_togglebutton_widget("use OpenGL", orientation_form, args, n);
      XtAddCallback(oid_glbutton, XmNvalueChangedCallback, with_gl_callback, NULL);
      XmStringFree(glstr);
#endif

      if (color_scale(ss) != 1.0)
	reflect_color_scale(color_scale(ss));

      map_over_children(ccd_dialog, set_main_color_of_widget);
      set_dialog_widget(COLOR_ORIENTATION_DIALOG, ccd_dialog);
      if (managed) XtManageChild(ccd_dialog);
    }
  else 
    {
      if (managed)
	{
	  if (!XtIsManaged(ccd_dialog)) XtManageChild(ccd_dialog);
	  raise_dialog(ccd_dialog);
	}
    }
  return(ccd_dialog);
}


void view_color_orientation_callback(Widget w, XtPointer context, XtPointer info)
{
  make_color_orientation_dialog(true);
}


bool color_orientation_dialog_is_active(void)
{
  return((ccd_dialog) && (XtIsManaged(ccd_dialog)));
}



void g_init_gxdraw(void)
{
  #define H_orientation_hook S_orientation_hook " (): called whenever one of the variables associated with the \
orientation dialog changes"
  #define H_color_hook S_color_hook " (): called whenever one of the variables associated with the \
color dialog changes"

  orientation_hook = Xen_define_hook(S_orientation_hook, "(make-hook)", 0, H_orientation_hook);
  color_hook =       Xen_define_hook(S_color_hook,       "(make-hook)", 0, H_color_hook);
}
