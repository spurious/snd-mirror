#include "snd.h"

void draw_line (axis_context *ax, int x0, int y0, int x1, int y1) 
{
  XDrawLine(ax->dp, ax->wn, ax->gc, x0, y0, x1, y1);
}

void fill_rectangle (axis_context *ax, int x0, int y0, int width, int height)
{
  XFillRectangle(ax->dp, ax->wn, ax->gc, x0, y0, width, height);
}

void erase_rectangle (chan_info *cp, axis_context *ax, int x0, int y0, int width, int height)
{
  XFillRectangle(ax->dp, ax->wn, erase_GC(cp), x0, y0, width, height);
}

void draw_string (axis_context *ax, int x0, int y0, char *str, int len)
{
  XDrawString(ax->dp, ax->wn, ax->gc, x0, y0, str, len);
}

void fill_polygon(axis_context *ax, int points, ...)
{
  int i;
  XPoint *pts;
  va_list ap;
  if (points == 0) return;
  pts = (XPoint *)CALLOC(points, sizeof(XPoint));
  va_start(ap, points);
  for (i = 0; i < points; i++)
    {
      pts[i].x = va_arg(ap, int);
      pts[i].y = va_arg(ap, int);
    }
  va_end(ap);
  XFillPolygon(ax->dp, ax->wn, ax->gc, pts, points, Convex, CoordModeOrigin);
  FREE(pts);
}

void draw_polygon(axis_context *ax, int points, ...)
{
  int i;
  XPoint *pts;
  va_list ap;
  if (points == 0) return;
  pts = (XPoint *)CALLOC(points, sizeof(XPoint));
  va_start(ap, points);
  for (i = 0; i < points; i++)
    {
      pts[i].x = va_arg(ap, int);
      pts[i].y = va_arg(ap, int);
    }
  va_end(ap);
  XDrawLines(ax->dp, ax->wn, ax->gc, pts, points, CoordModeOrigin);
  FREE(pts);
}

void draw_lines (axis_context *ax, XPoint *points, int num)
{
  if (num == 0) return;
  XDrawLines(ax->dp, ax->wn, ax->gc, points, num, CoordModeOrigin);
}

void draw_points (axis_context *ax, XPoint *points, int num, int size)
{
  XArc *rs;
  int i, size2;
  if (num == 0) return;
  if (size == 1)
    XDrawPoints(ax->dp, ax->wn, ax->gc, points, num, CoordModeOrigin);
  else
    {
      /* create squares or whatever centered on each point */
      size2 = size/2;
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

static void draw_point (Display *dp, Drawable wn, GC gc, XPoint point, int size)
{
  if (size == 1)
    XDrawPoint(dp, wn, gc, point.x, point.y);
  else
    XFillArc(dp, wn, gc, 
	     point.x - size / 2, 
	     point.y - size / 2, 
	     size, size, 0, 
	     360 * 64);
}

void draw_arc(axis_context *ax, int x, int y, int size)
{
  XFillArc(ax->dp, ax->wn, ax->gc, 
	   x - size / 2, 
	   y - size / 2, 
	   size, size, 0, 
	   360 * 64);
}

static XPoint polypts[4];

static void fill_polygons (axis_context *ax, XPoint *points, int num, int y0)
{
  int i;
  for (i = 1; i < num; i++)
    {
      polypts[0].x = points[i-1].x;
      polypts[0].y = points[i-1].y;
      polypts[1].x = points[i].x;
      polypts[1].y = points[i].y;
      polypts[2].x = polypts[1].x;
      polypts[2].y = y0;
      polypts[3].x = points[i-1].x;
      polypts[3].y = y0;
      XFillPolygon(ax->dp, ax->wn, ax->gc, polypts, 4, Convex, CoordModeOrigin);
    }
}

static void fill_two_sided_polygons(axis_context *ax, XPoint *points, XPoint *points1, int num)
{
  int i;
  for (i = 1; i < num; i++)
    {
      polypts[0].x = points[i-1].x;
      polypts[0].y = points[i-1].y;
      polypts[1].x = points[i].x;
      polypts[1].y = points[i].y;
      polypts[2].x = points1[i].x;
      polypts[2].y = points1[i].y;
      polypts[3].x = points1[i-1].x;
      polypts[3].y = points1[i-1].y;
      XFillPolygon(ax->dp, ax->wn, ax->gc, polypts, 4, Convex, CoordModeOrigin);
    }
}

static XPoint *points = NULL;
static XPoint *points1 = NULL;

static XPoint *points_address(int which) {if (which == 0) return(points); else return(points1);}

void allocate_grf_points(void)
{
  if (!points) points = (XPoint *)CALLOC(POINT_BUFFER_SIZE, sizeof(XPoint));
  if (!points1) points1 = (XPoint *)CALLOC(POINT_BUFFER_SIZE, sizeof(XPoint));
}

void set_grf_points(int xi, int j, int ymin, int ymax)
{
  points[j].x = xi;
  points1[j].x = xi;
  points[j].y = ymax;
  points1[j].y = ymin;
}

void set_grf_point(int xi, int j, int yi)
{
  points[j].x = xi;
  points[j].y = yi;
}

void draw_both_grf_points(chan_info *cp, axis_context *ax, int j, int graph_style)
{
  int i, size8, size4;
  switch (graph_style)
    {
    case GRAPH_LINES:
      XDrawLines(ax->dp, ax->wn, ax->gc, points, j, CoordModeOrigin);
      XDrawLines(ax->dp, ax->wn, ax->gc, points1, j, CoordModeOrigin);
      break;
    case GRAPH_DOTS:
      draw_points(ax, points, j, cp->dot_size);
      draw_points(ax, points1, j, cp->dot_size);
      break;
    case GRAPH_FILLED:
      fill_two_sided_polygons(ax, points, points1, j);
      break;
    case GRAPH_DOTS_AND_LINES:
      if (cp->dot_size > 1)
	{
	  draw_points(ax, points, j, cp->dot_size);
	  draw_points(ax, points1, j, cp->dot_size);
	}
      XDrawLines(ax->dp, ax->wn, ax->gc, points, j, CoordModeOrigin);
      XDrawLines(ax->dp, ax->wn, ax->gc, points1, j, CoordModeOrigin);
      break;
    case GRAPH_LOLLIPOPS:
      if (cp->dot_size == 1)
	{
	  for (i = 0; i < j; i++)
	    XDrawLine(ax->dp, ax->wn, ax->gc, points[i].x, points[i].y, points1[i].x, points1[i].y);
	}
      else
	{
	  size8 = cp->dot_size/8;
	  size4 = cp->dot_size/4;
	  if (size4 < 1) size4 = 1;
	  draw_points(ax, points, j, cp->dot_size);
	  draw_points(ax, points1, j, cp->dot_size);
	  for (i = 0; i < j; i++)
	    XFillRectangle(ax->dp, ax->wn, ax->gc, 
			   points[i].x - size8, 
			   points[i].y, 
			   size4, 
			   points1[i].y - points[i].y);
	}
 
    }
}

void draw_grf_points(chan_info *cp, axis_context *ax, int j, axis_info *ap, Float y0, int graph_style)
{
  int i, gy0, size8, size4;
  switch (graph_style)
    {
    case GRAPH_LINES: 
      draw_lines(ax, points, j); 
      break;
    case GRAPH_DOTS: 
      draw_points(ax, points, j, cp->dot_size); 
      break;
    case GRAPH_FILLED: 
      fill_polygons(ax, points, j, grf_y(y0, ap)); 
      break;
    case GRAPH_DOTS_AND_LINES: 
      if (cp->dot_size > 1) draw_points(ax, points, j, cp->dot_size); 
      draw_lines(ax, points, j); 
      break;
    case GRAPH_LOLLIPOPS:
      gy0 = grf_y(y0, ap);
      if (cp->dot_size == 1)
	{
	  for (i = 0; i < j; i++)
	    XDrawLine(ax->dp, ax->wn, ax->gc, points[i].x, points[i].y, points[i].x, gy0);
	}
      else
	{
	  size8 = cp->dot_size / 8;
	  size4 = cp->dot_size / 4;
	  if (size4 < 1) size4 = 1;
	  draw_points(ax, points, j, cp->dot_size);
	  for (i = 0; i < j; i++)
	    if (points[i].y > gy0) /* unsigned int height */
	      XFillRectangle(ax->dp, ax->wn, ax->gc, points[i].x - size8, gy0, size4, points[i].y - gy0);
	    else XFillRectangle(ax->dp, ax->wn, ax->gc, points[i].x - size8, points[i].y, size4, gy0 - points[i].y);
	}
      break;
    }
}

void draw_both_grfs(axis_context *ax, int j) /* only for enved wave */
{
  XDrawLines(ax->dp, ax->wn, ax->gc, points, j, CoordModeOrigin);
  if (points1[0].x != -1) 
    XDrawLines(ax->dp, ax->wn, ax->gc, points1, j, CoordModeOrigin);
}

static void allocate_erase_grf_points(mix_context *ms)
{
  if (ms->p0 == NULL)
    {
      ms->p0 = (XPoint *)CALLOC(POINT_BUFFER_SIZE, sizeof(XPoint));
      ms->p1 = (XPoint *)CALLOC(POINT_BUFFER_SIZE, sizeof(XPoint));
    }
}

static void backup_erase_grf_points(mix_context *ms, int nj)
{
  int i;
  XPoint *points, *points1;
  points = points_address(0);
  points1 = points_address(1);
  ms->lastpj = nj;
  for (i = 0; i < nj; i++)
    {
      ms->p0[i] = points[i];
      ms->p1[i] = points1[i];
    }
}

void mix_save_graph(snd_state *ss, mix_context *ms, int j)
{
  if (movies(ss))
    {
      allocate_erase_grf_points(ms);
      backup_erase_grf_points(ms, j);
    }
}

void erase_and_draw_grf_points(mix_context *ms, chan_info *cp, int nj)
{
  int i, j, min, previous_j;
  chan_context *cx;
  axis_context *ax;
  Display *dpy;
  Drawable wn;
  GC draw_gc, undraw_gc;
  XPoint *points;
  points = points_address(0);
  previous_j = ms->lastpj;
  cx = cp->tcgx;
  if (!cx) cx = cp->cgx;
  ax = cx->ax;
  dpy = ax->dp;
  wn = ax->wn;
  draw_gc = copy_GC(cp);
  undraw_gc = erase_GC(cp);
  min = ((nj < previous_j) ? nj : previous_j);
  if (cp->graph_style == GRAPH_LINES)
    {
      for (i = 0, j = 1; i < min-1; i++, j++)
	{
	  XDrawLine(dpy, wn, undraw_gc, ms->p0[i].x, ms->p0[i].y, ms->p0[j].x, ms->p0[j].y);
	  XDrawLine(dpy, wn, draw_gc, points[i].x, points[i].y, points[j].x, points[j].y);
	}
      if (nj > previous_j)
	{
	  for (i = min-1; i < nj-1; i++) 
	    XDrawLine(dpy, wn, draw_gc, points[i].x, points[i].y, points[i+1].x, points[i+1].y);
	}
      else
	{
	  if (previous_j > nj)
	    {
	      for (i = min-1; i < previous_j-1; i++) 
		XDrawLine(dpy, wn, undraw_gc, ms->p0[i].x, ms->p0[i].y, ms->p0[i+1].x, ms->p0[i+1].y);
	    }
	}
    }
  else /* dots */
    {
      for (i = 0; i < min; i++)
	{
	  draw_point(dpy, wn, undraw_gc, ms->p0[i], cp->dot_size);
	  draw_point(dpy, wn, draw_gc, points[i], cp->dot_size);
	}
      if (nj > previous_j)
	{
	  for (i = min; i < nj; i++) 
	    draw_point(dpy, wn, draw_gc, points[i], cp->dot_size);
	}
      else
	{
	  if (previous_j > nj)
	    {
	      for (i = min; i < previous_j; i++) 
		draw_point(dpy, wn, undraw_gc, ms->p0[i], cp->dot_size);
	    }
	}
    }
  backup_erase_grf_points(ms, nj);
}

void erase_and_draw_both_grf_points(mix_context *ms, chan_info *cp, int nj)
{
  int i, j, min, previous_j;
  chan_context *cx;
  axis_context *ax;
  Display *dpy;
  Drawable wn;
  GC draw_gc, undraw_gc;
  XPoint *points, *points1;
  points = points_address(0);
  points1 = points_address(1);
  previous_j = ms->lastpj;
  cx = cp->tcgx;
  if (!cx) cx = cp->cgx;
  ax = cx->ax;
  dpy = ax->dp;
  wn = ax->wn;
  draw_gc = copy_GC(cp);
  undraw_gc = erase_GC(cp);
  min = ((nj < previous_j) ? nj : previous_j);
  if (cp->graph_style == GRAPH_LINES)
    {
      for (i = 0, j = 1; i < min-1; i++, j++)
	{
	  XDrawLine(dpy, wn, undraw_gc, ms->p0[i].x, ms->p0[i].y, ms->p0[j].x, ms->p0[j].y);
	  XDrawLine(dpy, wn, draw_gc, points[i].x, points[i].y, points[j].x, points[j].y);
	  XDrawLine(dpy, wn, undraw_gc, ms->p1[i].x, ms->p1[i].y, ms->p1[j].x, ms->p1[j].y);
	  XDrawLine(dpy, wn, draw_gc, points1[i].x, points1[i].y, points1[j].x, points1[j].y);
	}
      if (nj > previous_j)
	{
	  for (i = min-1; i < nj-1; i++) 
	    {
	      XDrawLine(dpy, wn, draw_gc, points[i].x, points[i].y, points[i+1].x, points[i+1].y);
	      XDrawLine(dpy, wn, draw_gc, points1[i].x, points1[i].y, points1[i+1].x, points1[i+1].y);
	    }
	}
      else
	if (previous_j > nj)
	  {
	    for (i = min-1; i < previous_j-1; i++) 
	      {
		XDrawLine(dpy, wn, undraw_gc, ms->p0[i].x, ms->p0[i].y, ms->p0[i+1].x, ms->p0[i+1].y);
		XDrawLine(dpy, wn, undraw_gc, ms->p1[i].x, ms->p1[i].y, ms->p1[i+1].x, ms->p1[i+1].y);
	      }
	  }
    }
  else /* dots */
    {
      for (i = 0; i < min; i++)
	{
	  draw_point(dpy, wn, undraw_gc, ms->p0[i], cp->dot_size);
	  draw_point(dpy, wn, draw_gc, points[i], cp->dot_size);
	  draw_point(dpy, wn, undraw_gc, ms->p1[i], cp->dot_size);
	  draw_point(dpy, wn, draw_gc, points1[i], cp->dot_size);
	}
      if (nj > previous_j)
	{
	  for (i = min; i < nj; i++) 
	    {
	      draw_point(dpy, wn, draw_gc, points[i], cp->dot_size);
	      draw_point(dpy, wn, draw_gc, points1[i], cp->dot_size);
	    }
	}
      else
	{
	  if (previous_j > nj)
	    {
	      for (i = min; i < previous_j; i++) 
		{
		  draw_point(dpy, wn, undraw_gc, ms->p0[i], cp->dot_size);
		  draw_point(dpy, wn, undraw_gc, ms->p1[i], cp->dot_size);
		}
	    }
	}

    }
  backup_erase_grf_points(ms, nj);
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

static int sono_size = -1;
static Pixel grays[GRAY_SCALES];
static int grays_allocated = -1;
static XRectangle *sono_data[GRAY_SCALES];
static GC colormap_GC;

void initialize_colormap(snd_state *ss)
{
  state_context *sx;
  XGCValues gv;
  sx = ss->sgx;
  gv.background = sx->white;
  gv.foreground = sx->data_color;
  colormap_GC = XCreateGC(MAIN_DISPLAY(ss), XtWindow(MAIN_SHELL(ss)), GCForeground | GCBackground, &gv);
}

void draw_sono_rectangles(axis_context *ax, int color, int jmax)
{
  XSetForeground(ax->dp, colormap_GC, grays[color]);
  XFillRectangles(ax->dp, ax->wn, colormap_GC, sono_data[color], jmax); 
}

void draw_spectro_line(axis_context *ax, int color, int x0, int y0, int x1, int y1)
{
  XSetForeground(ax->dp, colormap_GC, grays[color]);
  XDrawLine(ax->dp, ax->wn, colormap_GC, x0, y0, x1, y1);
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

void allocate_sono_rects(snd_state *ss, int size)
{
  int i;
  if (color_map(ss) != -1) 
    allocate_color_map(ss, color_map(ss));
  else allocate_color_map(ss, 0);
  if (size > sono_size)
    {
      for (i = 0; i < GRAY_SCALES; i++)
	{
	  if ((sono_size > 0) && (sono_data[i])) FREE(sono_data[i]); 
	  sono_data[i] = NULL;
	}
      for (i = 0; i < GRAY_SCALES; i++)
	{
	  sono_data[i] = (XRectangle *)CALLOC(size, sizeof(XRectangle));
	}
      sono_size = size;
    }
}

void allocate_color_map(snd_state *ss, int colormap)
{
  static int warned_color = 0;
  int i, j;
  Colormap cmap;
  XColor tmp_color;
  Display *dpy;
  int scr;
  unsigned short *curmap;
  if (grays_allocated != colormap)
    {
      curmap = snd_colormap(colormap);
      tmp_color.flags = DoRed | DoGreen | DoBlue;
      dpy = XtDisplay(MAIN_SHELL(ss));
      scr = DefaultScreen(dpy);
      cmap = DefaultColormap(dpy, scr);
      /* 8-bit color displays can't handle all these colors, apparently, so we have to check status */
      if (grays_allocated != -1) XFreeColors(dpy, cmap, grays, GRAY_SCALES, 0);
      j = 0;
      for (i = 0; i < GRAY_SCALES; i++)
	{
	  tmp_color.red = curmap[j++];
	  tmp_color.green = curmap[j++];
	  tmp_color.blue = curmap[j++];
	  if ((XAllocColor(dpy, cmap, &tmp_color)) == 0) /* 0 = failure -- try black as a fallback */
	    {
	      tmp_color.red = 0;
	      tmp_color.green = 0;
	      tmp_color.blue = 0;
	      if ((XAllocColor(dpy, cmap, &tmp_color)) == 0)
		{
		  if (warned_color == 0)
		    snd_error("can't even allocate black?!?");
		  warned_color = 1;
		}
	    }
	  grays[i] = tmp_color.pixel;
	}
      grays_allocated = colormap;
    }
}


#if HAVE_GUILE
void x_load_colormap(Pixel *colors)
{
  int i;
  for (i = 0; i < GRAY_SCALES; i++) 
    grays[i] = colors[i];
}
#endif

/* -------- color browser -------- */

typedef struct {
  Widget dialog;
  Widget list; 
  Widget scale; 
  Widget invert;
  Widget cutoff;
  snd_state *state;
} color_chooser_info;

static color_chooser_info *ccd = NULL;

static void Invert_Color_Callback(Widget w, XtPointer context, XtPointer info)
{
  snd_state *ss;
  color_chooser_info *cd = (color_chooser_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ss = cd->state;
  in_set_color_inverted(ss, cb->set);
  map_over_chans(ss, update_graph, NULL);
}

void set_color_inverted(snd_state *ss, int val)
{
  in_set_color_inverted(ss, val);
  if (ccd) 
    XmToggleButtonSetState(ccd->invert, val, FALSE);
  if (!(ss->graph_hook_active)) 
    map_over_chans(ss, update_graph, NULL);
}

static void Scale_Color_Callback(Widget w, XtPointer context, XtPointer info)
{
  snd_state *ss;
  Float val;
  int scale_val;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  color_chooser_info *cd = (color_chooser_info *)context;
  ss = cd->state;
  scale_val = cbs->value;
  if (scale_val <= 50) 
    val = (Float)(scale_val + 1) / 51.0;
  else val = 1.0 + (Float)(scale_val - 50) * 20.0;
  in_set_color_scale(ss, val);
  map_over_chans(ss, update_graph, NULL);
}

static void reflect_color_scale(Float val)
{
  if (val <= 1.0) 
    XmScaleSetValue(ccd->scale, (int)(val * 51.0 - 1));
  else XmScaleSetValue(ccd->scale, (int)((val - 1.0) / 20.0 + 50.0));
}

void set_color_scale(snd_state *ss, Float val)
{
  in_set_color_scale(ss, val);
  if (ccd) 
    reflect_color_scale(color_scale(ss));
  if (!(ss->graph_hook_active)) 
    map_over_chans(ss, update_graph, NULL);
}

static void List_Color_Callback(Widget w, XtPointer context, XtPointer info)
{
  snd_state *ss;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  color_chooser_info *cd = (color_chooser_info *)context;
  ss = cd->state;
  in_set_color_map(ss, (cbs->item_position - 1));
  map_over_chans(ss, update_graph, NULL);
}

void set_color_map(snd_state *ss, int val)
{
  in_set_color_map(ss, val);
  if (ccd) 
    XmListSelectPos(ccd->list, val + 1, FALSE);
  if (!(ss->graph_hook_active)) 
    map_over_chans(ss, update_graph, NULL);
}

static void Cutoff_Color_Callback(Widget w, XtPointer context, XtPointer info) /* cutoff point */
{
  /* cutoff point for color chooser */
  snd_state *ss;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  color_chooser_info *cd = (color_chooser_info *)context;
  ss = cd->state;
  in_set_color_cutoff(ss, (Float)(cbs->value) / 1000.0);
  map_over_chans(ss, update_graph, NULL);
}

void set_color_cutoff(snd_state *ss, Float val)
{
  in_set_color_cutoff(ss, val);
  if (ccd) 
    XmScaleSetValue(ccd->cutoff, (int)(val * 1000.0));
  if (!(ss->graph_hook_active)) 
    map_over_chans(ss, update_graph, NULL);
}


static void Dismiss_Color_Callback(Widget w, XtPointer context, XtPointer info)
{
  color_chooser_info *cd = (color_chooser_info *)context;
  XtUnmanageChild(cd->dialog);
}

static void Help_Color_Callback(Widget w, XtPointer context, XtPointer info)
{
  color_dialog_help((snd_state *)context);
}

/* I tried a scrolled window with each colormap name in an appropriate color, but it looked kinda dumb */

void View_Color_Callback(Widget w, XtPointer context, XtPointer info)
{
  Arg args[32];
  int n, i;
  XmString xhelp, xdismiss, xcutoff, xinvert, titlestr;
  XmString *cmaps;
  Widget mainform, list_label, light_label, sep, sep1;
  snd_state *ss = (snd_state *)context;
  if (!ccd)
    {
      /* create color chooser dialog window */
      ccd = (color_chooser_info *)CALLOC(1, sizeof(color_chooser_info));
      ccd->state = ss;

      xdismiss = XmStringCreate(STR_Dismiss, XmFONTLIST_DEFAULT_TAG); /* needed by template dialog */
      xhelp = XmStringCreate(STR_Help, XmFONTLIST_DEFAULT_TAG);
      titlestr = XmStringCreate(STR_Color_Editor, XmFONTLIST_DEFAULT_TAG);
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNcancelLabelString, xdismiss); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNautoUnmanage, FALSE); n++;
      XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, FALSE); n++;
      XtSetArg(args[n], XmNtransient, FALSE); n++;
      ccd->dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), STR_Color, args, n);
#if HAVE_GUILE
      set_dialog_widget(COLOR_DIALOG, ccd->dialog);
#endif
      add_dialog(ss, ccd->dialog);
#if OVERRIDE_TOGGLE
      override_form_translation(ccd->dialog);
#endif

      XtAddCallback(ccd->dialog, XmNcancelCallback, Dismiss_Color_Callback, ccd);
      XtAddCallback(ccd->dialog, XmNhelpCallback, Help_Color_Callback, ss);
      XmStringFree(xhelp);
      XmStringFree(xdismiss);
      XmStringFree(titlestr);

      if (!(ss->using_schemes))
	{
	  XtVaSetValues(XmMessageBoxGetChild(ccd->dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(ccd->dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	}

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, XmMessageBoxGetChild(ccd->dialog, XmDIALOG_SEPARATOR)); n++;
      mainform = sndCreateFormWidget("formd", ccd->dialog, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      list_label = XtCreateManagedWidget(S_colormap, xmLabelWidgetClass, mainform, args, n);
      
      n = 0;
      cmaps = (XmString *)CALLOC(NUM_COLORMAPS, sizeof(XmString));
      for (i = 0; i < NUM_COLORMAPS; i++) cmaps[i] = XmStringCreate(colormap_name(i), XmFONTLIST_DEFAULT_TAG);
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, list_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNlistMarginWidth, 3); n++;
      ccd->list = XmCreateScrolledList(mainform, "colormap-list", args, n);
      if (!(ss->using_schemes)) 
	XtVaSetValues(ccd->list, 
		      XmNbackground, (ss->sgx)->white, 
		      XmNforeground, (ss->sgx)->black, 
		      NULL);
      XtVaSetValues(ccd->list, 
		    XmNitems, cmaps, 
		    XmNitemCount, NUM_COLORMAPS, 
		    XmNvisibleItemCount, 6, 
		    NULL);
      XtAddCallback(ccd->list, XmNbrowseSelectionCallback, List_Color_Callback, ccd);
      for (i = 0; i < NUM_COLORMAPS; i++) XmStringFree(cmaps[i]);
      FREE(cmaps);
      XtManageChild(ccd->list);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
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
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
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
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, sep); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep1); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, TRUE); n++;
      XtSetArg(args[n], XmNvalue, 50); n++;
      ccd->scale = XtCreateManagedWidget("ccdscl", xmScaleWidgetClass, mainform, args, n);
      XtAddCallback(ccd->scale, XmNvalueChangedCallback, Scale_Color_Callback, ccd);
      XtAddCallback(ccd->scale, XmNdragCallback, Scale_Color_Callback, ccd);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, ccd->scale); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      light_label = XtCreateManagedWidget(STR_light, xmLabelWidgetClass, mainform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, sep); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, ccd->scale); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtCreateManagedWidget(STR_dark, xmLabelWidgetClass, mainform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, sep); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, light_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, TRUE); n++;
      XtSetArg(args[n], XmNmaximum, 250); n++;
      XtSetArg(args[n], XmNdecimalPoints, 3); n++;
      xcutoff = XmStringCreate(STR_cutoff, XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNtitleString, xcutoff); n++;
      XtSetArg(args[n], XmNvalue, (int)(color_cutoff(ss) * 1000)); n++;
      ccd->cutoff = XtCreateManagedWidget("cutoff", xmScaleWidgetClass, mainform, args, n);
      XtAddCallback(ccd->cutoff, XmNvalueChangedCallback, Cutoff_Color_Callback, ccd);
      XtAddCallback(ccd->cutoff, XmNdragCallback, Cutoff_Color_Callback, ccd);
      XmStringFree(xcutoff);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNselectColor, (ss->sgx)->pushed_button_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, ccd->cutoff); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNset, color_inverted(ss)); n++;
      xinvert = XmStringCreate(STR_invert, XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNlabelString, xinvert); n++;
      ccd->invert = sndCreateToggleButtonWidget(STR_invert, mainform, args, n);
      XtAddCallback(ccd->invert, XmNvalueChangedCallback, Invert_Color_Callback, ccd);
      XmStringFree(xinvert);
      if (color_scale(ss) != 1.0)
	reflect_color_scale(color_scale(ss));
    }
  else raise_dialog(ccd->dialog);
  if (!XtIsManaged(ccd->dialog)) 
    XtManageChild(ccd->dialog);
}

int color_dialog_is_active(void)
{
  return((ccd) && (ccd->dialog) && (XtIsManaged(ccd->dialog)));
}


/* -------- orientation browser -------- */

typedef struct {
  Widget dialog;
  Widget ax, ay, az, sx, sy, sz, hop, cut; 
  snd_state *state;
} orientation_info;

static orientation_info *oid = NULL;

static void AX_Orientation_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_state *ss;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  orientation_info *od = (orientation_info *)context;
  ss = od->state;
  in_set_spectro_x_angle(ss, (Float)(cbs->value));
  map_chans_field(ss, FCP_X_ANGLE, (Float)(cbs->value));
  map_over_chans(ss, update_graph, NULL);
}

void set_spectro_x_angle(snd_state *ss, Float val)
{
  in_set_spectro_x_angle(ss, val);
  if (oid) XmScaleSetValue(oid->ax, (int)val);
  map_chans_field(ss, FCP_X_ANGLE, val);
  if (!(ss->graph_hook_active)) 
    map_over_chans(ss, update_graph, NULL);
}

static void AX_Help_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help((snd_state *)context, 
	   "x angle slider", 
	   "This slider causes the graph to rotate\naround the x axis.\n");
}

static void AY_Orientation_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_state *ss;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  orientation_info *od = (orientation_info *)context;
  ss = od->state;
  in_set_spectro_y_angle(ss, (Float)(cbs->value));
  map_chans_field(ss, FCP_Y_ANGLE, (Float)(cbs->value));
  map_over_chans(ss, update_graph, NULL);
}

void set_spectro_y_angle(snd_state *ss, Float val)
{
  in_set_spectro_y_angle(ss, val);
  if (oid) XmScaleSetValue(oid->ay, (int)val);
  map_chans_field(ss, FCP_Y_ANGLE, val);
  if (!(ss->graph_hook_active)) 
    map_over_chans(ss, update_graph, NULL);
}

static void AY_Help_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help((snd_state *)context, 
	   "y angle slider", 
	   "This slider causes the graph to rotate\naround the y axis.\n");
}

static void AZ_Orientation_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_state *ss;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  orientation_info *od = (orientation_info *)context;
  ss = od->state;
  in_set_spectro_z_angle(ss, (Float)(cbs->value));
  map_chans_field(ss, FCP_Z_ANGLE, (Float)(cbs->value));
  map_over_chans(ss, update_graph, NULL);
}

void set_spectro_z_angle(snd_state *ss, Float val)
{
  in_set_spectro_z_angle(ss, val);
  if (oid) XmScaleSetValue(oid->az, (int)val);
  map_chans_field(ss, FCP_Z_ANGLE, val);
  if (!(ss->graph_hook_active)) 
    map_over_chans(ss, update_graph, NULL);
}

static void AZ_Help_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help((snd_state *)context, 
	   "z angle slider", 
	   "This slider causes the graph to rotate\naround the z axis.\n");
}

static void SX_Orientation_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_state *ss;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  orientation_info *od = (orientation_info *)context;
  ss = od->state;
  in_set_spectro_x_scale(ss, (Float)(cbs->value) * 0.01);
  map_chans_field(ss, FCP_X_SCALE, (Float)(cbs->value) * 0.01);
  map_over_chans(ss, update_graph, NULL);
}

void set_spectro_x_scale(snd_state *ss, Float val)
{
  in_set_spectro_x_scale(ss, val);
  if (oid) XmScaleSetValue(oid->sx, (int)(val * 100));
  map_chans_field(ss, FCP_X_SCALE, val);
  if (!(ss->graph_hook_active)) 
    map_over_chans(ss, update_graph, NULL);
}

static void SX_Help_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help((snd_state *)context, 
	   "x scale slider", 
	   "This slider causes the graph to expand or\ncontract along the x axis.\n");
}

static void SY_Orientation_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_state *ss;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  orientation_info *od = (orientation_info *)context;
  ss = od->state;
  in_set_spectro_y_scale(ss, (Float)(cbs->value) * 0.01);
  map_chans_field(ss, FCP_Y_SCALE, (Float)(cbs->value) * 0.01);
  map_over_chans(ss, update_graph, NULL);
}

void set_spectro_y_scale(snd_state *ss, Float val)
{
  in_set_spectro_y_scale(ss, val);
  if (oid) XmScaleSetValue(oid->sy, (int)(val * 100));
  map_chans_field(ss, FCP_Y_SCALE, val);
  if (!(ss->graph_hook_active)) 
    map_over_chans(ss, update_graph, NULL);
}

static void SY_Help_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help((snd_state *)context, 
	   "y scale slider", 
	   "This slider causes the graph to expand or\ncontract along the y axis.\n");
}

static void SZ_Orientation_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_state *ss;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  orientation_info *od = (orientation_info *)context;
  ss = od->state;
  in_set_spectro_z_scale(ss, (Float)(cbs->value) * 0.01);
  map_chans_field(ss, FCP_Z_SCALE, (Float)(cbs->value) * 0.01);
  map_over_chans(ss, update_graph, NULL);
}

void set_spectro_z_scale(snd_state *ss, Float val)
{
  in_set_spectro_z_scale(ss, val);
  if (oid) XmScaleSetValue(oid->sz, (int)(val * 100));
  map_chans_field(ss, FCP_Z_SCALE, val);
  if (!(ss->graph_hook_active)) 
    map_over_chans(ss, update_graph, NULL);
}

static void SZ_Help_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help((snd_state *)context, 
	   "z scale slider", 
	   "This slider causes the graph to expand or\ncontract along the z axis.\n");
}

static int map_chans_spectro_hop(chan_info *cp, void *ptr) {cp->spectro_hop = (int)ptr; return(0);}

static void Hop_Orientation_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_state *ss;
  int val;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  orientation_info *od = (orientation_info *)context;
  ss = od->state;
  val = iclamp(1, cbs->value, 20);
  in_set_spectro_hop(ss, val);
  map_over_chans(ss, map_chans_spectro_hop, (void *)val);
  map_over_chans(ss, update_graph, NULL);
}

void set_spectro_hop(snd_state *ss, int val)
{
  if (val > 0)
    {
      in_set_spectro_hop(ss, val);
      if (oid) XmScaleSetValue(oid->hop, val);
      map_over_chans(ss, map_chans_spectro_hop, (void *)val);
      if (!(ss->graph_hook_active)) 
	map_over_chans(ss, update_graph, NULL);
    }
}

static void Hop_Help_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help((snd_state *)context, 
	   "hop slider", 
	   "This slider changes the hop size.\n");
}

static void Cut_Orientation_Callback(Widget w, XtPointer context, XtPointer info) 
{
  /* y axis limit */
  snd_state *ss;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)info;
  orientation_info *od = (orientation_info *)context;
  ss = od->state;
  map_chans_field(ss, FCP_CUTOFF, (Float)(cbs->value) * 0.01);
  set_spectro_cutoff_and_redisplay(ss, (Float)(cbs->value) * 0.01); /* calls in_set... */
} 

void set_spectro_cutoff(snd_state *ss, Float val)
{
  in_set_spectro_cutoff(ss, val);
  if (oid) XmScaleSetValue(oid->cut, (int)(val * 100));
  map_chans_field(ss, FCP_CUTOFF, val);
  if (!(ss->graph_hook_active)) 
    map_over_chans(ss, update_graph, NULL);
}

static void Cut_Help_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help((snd_state *)context, 
	   "% of spectrum slider", 
	   "This slider determines how much of\nthe spectrum is displayed\n");
}

static void Help_Orientation_Callback(Widget w, XtPointer context, XtPointer info) 
{
  orientation_dialog_help((snd_state *)context);
}

static void Dismiss_Orientation_Callback(Widget w, XtPointer context, XtPointer info) 
{
  orientation_info *od = (orientation_info *)context;
  XtUnmanageChild(od->dialog);
}

static int fixup_angle(Float ang)
{
  int na;
  na = (int)ang;
  if (na < 0) na += 360;
  na = na % 360;
  return(na);
}

void reflect_spectro(snd_state *ss)
{
  /* set color/orientaton widget values */
  if (ccd) 
    {
      XmToggleButtonSetState(ccd->invert, color_inverted(ss), FALSE);
      XtVaSetValues(ccd->cutoff, XmNvalue, (int)((color_cutoff(ss)) * 1000), NULL);
      reflect_color_scale(color_scale(ss));
    }
  if (oid) 
    {
      XtVaSetValues(oid->ax, XmNvalue, fixup_angle(spectro_x_angle(ss)), NULL);
      XtVaSetValues(oid->ay, XmNvalue, fixup_angle(spectro_y_angle(ss)), NULL);
      XtVaSetValues(oid->az, XmNvalue, fixup_angle(spectro_z_angle(ss)), NULL);
      XtVaSetValues(oid->sx, XmNvalue, (int)(spectro_x_scale(ss) * 100), NULL);
      XtVaSetValues(oid->sy, XmNvalue, (int)(spectro_y_scale(ss) * 100), NULL);
      XtVaSetValues(oid->sz, XmNvalue, (int)(spectro_z_scale(ss) * 100), NULL);
      XtVaSetValues(oid->hop, XmNvalue, (spectro_hop(ss) > 100) ? 100 : (spectro_hop(ss)), NULL);
      XtVaSetValues(oid->cut, XmNvalue, (int)(spectro_cutoff(ss) * 100), NULL);
    }
}

static void Reset_Orientation_Callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_state *ss;
  orientation_info *od = (orientation_info *)context;
  /* put everything back the way it was at the start */
  ss = od->state;
  reset_spectro(ss);
  reflect_spectro(ss);
  map_over_chans(ss, update_graph, NULL);
}

void View_Orientation_Callback(Widget w, XtPointer context, XtPointer info)
{
  snd_state *ss = (snd_state *)context;
  Widget mainform, rightbox, leftbox;
  XmString xdismiss, xhelp, xstr, xreset, titlestr;
  int n;
  Arg args[20];
  if (!oid)
    {
      /* create orientation window */
      oid = (orientation_info *)CALLOC(1, sizeof(orientation_info));
      oid->state = ss;

      xdismiss = XmStringCreate(STR_Dismiss, XmFONTLIST_DEFAULT_TAG); /* needed by template dialog */
      xhelp = XmStringCreate(STR_Help, XmFONTLIST_DEFAULT_TAG);
      xreset = XmStringCreate(STR_Reset, XmFONTLIST_DEFAULT_TAG);
      titlestr = XmStringCreate(STR_Spectrogram_Orientation, XmFONTLIST_DEFAULT_TAG);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNcancelLabelString, xdismiss); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNokLabelString, xreset); n++;
      XtSetArg(args[n], XmNautoUnmanage, FALSE); n++;
      XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, FALSE); n++;
      XtSetArg(args[n], XmNtransient, FALSE); n++;
      oid->dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), STR_Orientation, args, n);
#if HAVE_GUILE
      set_dialog_widget(ORIENTATION_DIALOG, oid->dialog);
#endif
      add_dialog(ss, oid->dialog);
#if OVERRIDE_TOGGLE
      override_form_translation(oid->dialog);
#endif

      XtAddCallback(oid->dialog, XmNcancelCallback, Dismiss_Orientation_Callback, oid);
      XtAddCallback(oid->dialog, XmNhelpCallback, Help_Orientation_Callback, ss);
      XtAddCallback(oid->dialog, XmNokCallback, Reset_Orientation_Callback, oid);
      XmStringFree(xhelp);
      XmStringFree(xdismiss);
      XmStringFree(titlestr);
      XmStringFree(xreset);

      if (!(ss->using_schemes))
	{
	  XtVaSetValues(XmMessageBoxGetChild(oid->dialog, XmDIALOG_OK_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(oid->dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(oid->dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	}

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, XmMessageBoxGetChild(oid->dialog, XmDIALOG_SEPARATOR)); n++;
      mainform = sndCreateFormWidget("formd", oid->dialog, args, n);
      
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 50); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
      leftbox = sndCreateRowColumnWidget("leftb", mainform, args, n);
      
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, leftbox); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
      rightbox = sndCreateRowColumnWidget("rightb", mainform, args, n);
      
      /* left box */
      n = 0;
      xstr = XmStringCreate(STR_x_angle, XmFONTLIST_DEFAULT_TAG);
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, TRUE); n++;
      XtSetArg(args[n], XmNvalue, fixup_angle(spectro_x_angle(ss))); n++;
      XtSetArg(args[n], XmNmaximum, 360); n++;
      XtSetArg(args[n], XmNtitleString, xstr); n++;
      oid->ax = XtCreateManagedWidget("ax", xmScaleWidgetClass, leftbox, args, n);
      XtAddCallback(oid->ax, XmNvalueChangedCallback, AX_Orientation_Callback, oid);
      XtAddCallback(oid->ax, XmNdragCallback, AX_Orientation_Callback, oid);
      XtAddCallback(oid->ax, XmNhelpCallback, AX_Help_Callback, ss);
      XmStringFree(xstr);

      n = 0;
      xstr = XmStringCreate(STR_y_angle, XmFONTLIST_DEFAULT_TAG);
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, TRUE); n++;
      XtSetArg(args[n], XmNvalue, fixup_angle(spectro_y_angle(ss))); n++;
      XtSetArg(args[n], XmNmaximum, 360); n++;
      XtSetArg(args[n], XmNtitleString, xstr); n++;
      oid->ay = XtCreateManagedWidget("ay", xmScaleWidgetClass, leftbox, args, n);
      XtAddCallback(oid->ay, XmNvalueChangedCallback, AY_Orientation_Callback, oid);
      XtAddCallback(oid->ay, XmNdragCallback, AY_Orientation_Callback, oid);
      XtAddCallback(oid->ay, XmNhelpCallback, AY_Help_Callback, ss);
      XmStringFree(xstr);

      n = 0;
      xstr = XmStringCreate(STR_z_angle, XmFONTLIST_DEFAULT_TAG);
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, TRUE); n++;
      XtSetArg(args[n], XmNvalue, fixup_angle(spectro_z_angle(ss))); n++;
      XtSetArg(args[n], XmNmaximum, 360); n++;
      XtSetArg(args[n], XmNtitleString, xstr); n++;
      oid->az = XtCreateManagedWidget("az", xmScaleWidgetClass, leftbox, args, n);
      XtAddCallback(oid->az, XmNvalueChangedCallback, AZ_Orientation_Callback, oid);
      XtAddCallback(oid->az, XmNdragCallback, AZ_Orientation_Callback, oid);
      XtAddCallback(oid->az, XmNhelpCallback, AZ_Help_Callback, ss);
      XmStringFree(xstr);

      n = 0;
      xstr = XmStringCreate(STR_hop, XmFONTLIST_DEFAULT_TAG);
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, TRUE); n++;
      XtSetArg(args[n], XmNvalue, (spectro_hop(ss) > 20) ? 20 : (spectro_hop(ss))); n++;
      XtSetArg(args[n], XmNmaximum, 20); n++;
      XtSetArg(args[n], XmNtitleString, xstr); n++;
      oid->hop = XtCreateManagedWidget("hop", xmScaleWidgetClass, leftbox, args, n);
      XtAddCallback(oid->hop, XmNvalueChangedCallback, Hop_Orientation_Callback, oid);
      XtAddCallback(oid->hop, XmNdragCallback, Hop_Orientation_Callback, oid);
      XtAddCallback(oid->hop, XmNhelpCallback, Hop_Help_Callback, ss);
      XmStringFree(xstr);

      /* right box */
      n = 0;
      xstr = XmStringCreate(STR_x_scale, XmFONTLIST_DEFAULT_TAG);
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, TRUE); n++;
      XtSetArg(args[n], XmNmaximum, 200); n++;
      XtSetArg(args[n], XmNvalue, (int)(spectro_x_scale(ss) * 100)); n++;
      XtSetArg(args[n], XmNtitleString, xstr); n++;
      XtSetArg(args[n], XmNdecimalPoints, 2); n++;
      oid->sx = XtCreateManagedWidget("xs", xmScaleWidgetClass, rightbox, args, n);
      XtAddCallback(oid->sx, XmNvalueChangedCallback, SX_Orientation_Callback, oid);
      XtAddCallback(oid->sx, XmNdragCallback, SX_Orientation_Callback, oid);
      XtAddCallback(oid->sx, XmNhelpCallback, SX_Help_Callback, ss);
      XmStringFree(xstr);

      n = 0;
      xstr = XmStringCreate(STR_y_scale, XmFONTLIST_DEFAULT_TAG);
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, TRUE); n++;
      XtSetArg(args[n], XmNmaximum, 200); n++;
      XtSetArg(args[n], XmNvalue, (int)(spectro_y_scale(ss) * 100)); n++;
      XtSetArg(args[n], XmNtitleString, xstr); n++;
      XtSetArg(args[n], XmNdecimalPoints, 2); n++;
      oid->sy = XtCreateManagedWidget("ys", xmScaleWidgetClass, rightbox, args, n);
      XtAddCallback(oid->sy, XmNvalueChangedCallback, SY_Orientation_Callback, oid);
      XtAddCallback(oid->sy, XmNdragCallback, SY_Orientation_Callback, oid);
      XtAddCallback(oid->sy, XmNhelpCallback, SY_Help_Callback, ss);
      XmStringFree(xstr);

      n = 0;
      xstr = XmStringCreate(STR_z_scale, XmFONTLIST_DEFAULT_TAG);
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, TRUE); n++;
      XtSetArg(args[n], XmNdecimalPoints, 2); n++;
      XtSetArg(args[n], XmNvalue, (int)(spectro_z_scale(ss) * 100)); n++;
      XtSetArg(args[n], XmNtitleString, xstr); n++;
      oid->sz = XtCreateManagedWidget("zs", xmScaleWidgetClass, rightbox, args, n);
      XtAddCallback(oid->sz, XmNvalueChangedCallback, SZ_Orientation_Callback, oid);
      XtAddCallback(oid->sz, XmNdragCallback, SZ_Orientation_Callback, oid);
      XtAddCallback(oid->sz, XmNhelpCallback, SZ_Help_Callback, ss);
      XmStringFree(xstr);

      n = 0;
      xstr = XmStringCreate(STR_percent_of_spectrum, XmFONTLIST_DEFAULT_TAG);
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNshowValue, TRUE); n++;
      XtSetArg(args[n], XmNvalue, (int)(spectro_cutoff(ss) * 100)); n++;
      XtSetArg(args[n], XmNtitleString, xstr); n++;
      oid->cut = XtCreateManagedWidget("cut", xmScaleWidgetClass, rightbox, args, n);
      XtAddCallback(oid->cut, XmNvalueChangedCallback, Cut_Orientation_Callback, oid);
      XtAddCallback(oid->cut, XmNdragCallback, Cut_Orientation_Callback, oid);
      XtAddCallback(oid->cut, XmNhelpCallback, Cut_Help_Callback, ss);
      XmStringFree(xstr);
    }
  else raise_dialog(oid->dialog);
  if (!XtIsManaged(oid->dialog)) XtManageChild(oid->dialog);
}

int orientation_dialog_is_active(void)
{
  return((oid) && (oid->dialog) && (XtIsManaged(oid->dialog)));
}

void start_color_dialog(snd_state *ss, int width, int height)
{
  View_Color_Callback(NULL, (XtPointer)ss, NULL);
  if (width != 0) 
    XtVaSetValues(ccd->dialog, 
		  XmNwidth, (Dimension)width, 
		  XmNheight, (Dimension)height, 
		  NULL);
}

void start_orientation_dialog(snd_state *ss, int width, int height)
{
  View_Orientation_Callback(NULL, (XtPointer)ss, NULL);
  if (width != 0) 
    XtVaSetValues(oid->dialog, 
		  XmNwidth, (Dimension)width, 
		  XmNheight, (Dimension)height, 
		  NULL);
}

