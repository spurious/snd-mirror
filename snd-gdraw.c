/* TODO: use gdk rgb rather than colormaps? -- are they smart about rectangles?
 */

#include "snd.h"

void draw_line (axis_context *ax, int x0, int y0, int x1, int y1) 
{
  if (ax->wn == NULL) return;
  gdk_draw_line(ax->wn, ax->gc, (gint)x0, (gint)y0, (gint)x1, (gint)y1);
}

void fill_rectangle (axis_context *ax, int x0, int y0, int width, int height)
{
  if (ax->wn == NULL) return;
  gdk_draw_rectangle(ax->wn, ax->gc, TRUE, (gint)x0, (gint)y0, (gint)width, (gint)height);
}

void erase_rectangle (chan_info *cp, axis_context *ax, int x0, int y0, int width, int height)
{
  if (ax->wn == NULL) return;
  gdk_draw_rectangle(ax->wn, erase_GC(cp), TRUE, (gint)x0, (gint)y0, (gint)width, (gint)height);
}

void draw_string (axis_context *ax, int x0, int y0, char *str, int len)
{
  if ((ax->wn == NULL) || (ax->current_font == NULL)) return;
  gdk_draw_string(ax->wn, ax->current_font, ax->gc, (gint)x0, (gint)y0, (const gchar *)str);
}

void fill_polygon(axis_context *ax, int points, ...)
{
  int i;
  GdkPoint *pts;
  va_list ap;
  if (points == 0) return;
  pts = (GdkPoint *)CALLOC(points, sizeof(GdkPoint));
  va_start(ap, points);
  for (i = 0; i < points; i++)
    {
      pts[i].x = va_arg(ap, int);
      pts[i].y = va_arg(ap, int);
    }
  va_end(ap);
  gdk_draw_polygon(ax->wn, ax->gc, TRUE, pts, points);
  FREE(pts);
}

void draw_polygon(axis_context *ax, int points, ...)
{
  int i;
  GdkPoint *pts;
  va_list ap;
  if (points == 0) return;
  pts = (GdkPoint *)CALLOC(points, sizeof(GdkPoint));
  va_start(ap, points);
  for (i = 0; i < points; i++)
    {
      pts[i].x = va_arg(ap, int);
      pts[i].y = va_arg(ap, int);
    }
  va_end(ap);
  gdk_draw_lines(ax->wn, ax->gc, pts, points);
  FREE(pts);
}

void draw_lines (axis_context *ax, GdkPoint *points, int num)
{
  if (num == 0) return;
  gdk_draw_lines(ax->wn, ax->gc, points, num);
}

void draw_points (axis_context *ax, GdkPoint *points, int num, int size)
{
  int i, size2;
  if (num == 0) return;
  if (size == 1)
    gdk_draw_points(ax->wn, ax->gc, points, num);
  else
    {
      size2 = size / 2;
      for (i = 0; i < num; i++) 
	gdk_draw_arc(ax->wn, ax->gc, TRUE, points[i].x - size2, points[i].y - size2, size, size, 0, 360 * 64);
    }
}

static void draw_point (GdkDrawable *wn, GdkGC *gc, GdkPoint point, int size)
{
  if (size == 1)
    gdk_draw_point(wn, gc, point.x, point.y);
  else
    gdk_draw_arc(wn, gc, TRUE, point.x - size / 2, point.y - size / 2, size, size, 0, 360 * 64);
}

void draw_arc(axis_context *ax, int x, int y, int size)
{
  gdk_draw_arc(ax->wn, ax->gc, TRUE, x - size / 2, y - size / 2, size, size, 0, 360 * 64);
}

static GdkPoint polypts[4];

static void fill_polygons (axis_context *ax, GdkPoint *points, int num, axis_info *ap, int y0)
{
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
      gdk_draw_polygon(ax->wn, ax->gc, TRUE, polypts, 4);
    }
}

static void fill_two_sided_polygons(axis_context *ax, GdkPoint *points, GdkPoint *points1, int num)
{
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
      gdk_draw_polygon(ax->wn, ax->gc, TRUE, polypts, 4);
    }
}

static GdkPoint *points = NULL;
static GdkPoint *points1 = NULL;

static GdkPoint *points_address(int which) {if (which == 0) return(points); else return(points1);}

void allocate_grf_points(void)
{
  if (!points) points = (GdkPoint *)CALLOC(POINT_BUFFER_SIZE, sizeof(GdkPoint));
  if (!points1) points1 = (GdkPoint *)CALLOC(POINT_BUFFER_SIZE, sizeof(GdkPoint));
}

void set_grf_points(Locus xi, int j, Locus ymin, Locus ymax)
{
  points[j].x = xi;
  points1[j].x = xi;
  points[j].y = ymax;
  points1[j].y = ymin;
}

void set_grf_point(Locus xi, int j, Locus yi)
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
      gdk_draw_lines(ax->wn, ax->gc, points, j);
      gdk_draw_lines(ax->wn, ax->gc, points1, j);
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
      gdk_draw_lines(ax->wn, ax->gc, points, j);
      gdk_draw_lines(ax->wn, ax->gc, points1, j);
      break;
    case GRAPH_LOLLIPOPS:
      if (cp->dot_size == 1)
	{
	  for (i = 0; i < j; i++)
	    gdk_draw_line(ax->wn, ax->gc, points[i].x, points[i].y, points1[i].x, points1[i].y);
	}
      else
	{
	  size8 = cp->dot_size / 8;
	  size4 = cp->dot_size / 4;
	  if (size4 < 1) size4 = 1;
	  draw_points(ax, points, j, cp->dot_size);
	  draw_points(ax, points1, j, cp->dot_size);
	  for (i = 0; i < j; i++)
	    gdk_draw_rectangle(ax->wn, ax->gc, TRUE, points[i].x - size8, points[i].y, size4, points1[i].y - points[i].y);
	}
    }
}

void draw_grf_points(chan_info *cp, axis_context *ax, int j, axis_info *ap, Float y0, int graph_style)
{
  int i, gy0, size8, size4;
  switch (graph_style)
    {
    case GRAPH_LINES: draw_lines(ax, points, j); break;
    case GRAPH_DOTS: draw_points(ax, points, j, cp->dot_size); break;
    case GRAPH_FILLED: fill_polygons(ax, points, j, ap, grf_y(y0, ap)); break;
    case GRAPH_DOTS_AND_LINES: 
      if (cp->dot_size > 1) draw_points(ax, points, j, cp->dot_size); 
      draw_lines(ax, points, j); 
      break;
    case GRAPH_LOLLIPOPS:
      gy0 = grf_y(y0, ap);
      if (cp->dot_size == 1)
	{
	  for (i = 0; i < j; i++)
	    gdk_draw_line(ax->wn, ax->gc, points[i].x, points[i].y, points[i].x, gy0);
	}
      else
	{
	  size8 = cp->dot_size / 8;
	  size4 = cp->dot_size / 4;
	  if (size4 < 1) size4 = 1;
	  draw_points(ax, points, j, cp->dot_size);
	  for (i = 0; i < j; i++)
	    if (points[i].y > gy0) /* unsigned int height */
	      gdk_draw_rectangle(ax->wn, ax->gc, TRUE, points[i].x - size8, gy0, size4, points[i].y - gy0);
	    else gdk_draw_rectangle(ax->wn, ax->gc, TRUE, points[i].x - size8, points[i].y, size4, gy0 - points[i].y);
	}
      break;
    }
}

void draw_both_grfs(axis_context *ax, int j) /* only for enved wave */
{
  gdk_draw_lines(ax->wn, ax->gc, points, j);
  if (points1[0].x != -1) gdk_draw_lines(ax->wn, ax->gc, points1, j);
}

static void allocate_erase_grf_points(mix_context *ms)
{
  if (ms->p0 == NULL)
    {
      ms->p0 = (GdkPoint *)CALLOC(POINT_BUFFER_SIZE, sizeof(GdkPoint));
      ms->p1 = (GdkPoint *)CALLOC(POINT_BUFFER_SIZE, sizeof(GdkPoint));
    }
}

static void backup_erase_grf_points(mix_context *ms, int nj)
{
  int i;
  GdkPoint *points, *points1;
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
  GdkDrawable *wn;
  GdkGC *draw_gc, *undraw_gc;
  GdkPoint *points;
  points = points_address(0);
  previous_j = ms->lastpj;
  cx = cp->tcgx;
  if (!cx) cx = cp->cgx;
  ax = cx->ax;
  wn = ax->wn;
  draw_gc = copy_GC(cp);
  undraw_gc = erase_GC(cp);
  min = ((nj < previous_j) ? nj : previous_j);
  if (cp->graph_style == GRAPH_LINES)
    {
      if (min <= 0) min = 1;
      for (i = 0, j = 1; i < min - 1; i++, j++)
	{
	  gdk_draw_line(wn, undraw_gc, ms->p0[i].x, ms->p0[i].y, ms->p0[j].x, ms->p0[j].y);
	  gdk_draw_line(wn, draw_gc, points[i].x, points[i].y, points[j].x, points[j].y);
	}
      if (min > 0)
	{
	  if (nj > previous_j)
	    for (i = min - 1; i < nj - 1; i++) 
	      gdk_draw_line(wn, draw_gc, points[i].x, points[i].y, points[i + 1].x, points[i + 1].y);
	  else
	    {
	      if (previous_j > nj)
		for (i = min - 1; i < previous_j - 1; i++) 
		  gdk_draw_line(wn, undraw_gc, ms->p0[i].x, ms->p0[i].y, ms->p0[i + 1].x, ms->p0[i + 1].y);
	    }
	}
    }
  else /* dots */
    {
      for (i = 0; i < min; i++)
	{
	  draw_point(wn, undraw_gc, ms->p0[i], cp->dot_size);
	  draw_point(wn, draw_gc, points[i], cp->dot_size);
	}
      if (nj > previous_j)
	{
	  for (i = min; i < nj; i++) 
	    draw_point(wn, draw_gc, points[i], cp->dot_size);
	}
      else
	{
	  if (previous_j > nj)
	    {
	      for (i = min; i < previous_j; i++) 
		draw_point(wn, undraw_gc, ms->p0[i], cp->dot_size);
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
  GdkDrawable *wn;
  GdkGC *draw_gc, *undraw_gc;
  GdkPoint *points, *points1;
  points = points_address(0);
  points1 = points_address(1);
  previous_j = ms->lastpj;
  cx = cp->tcgx;
  if (!cx) cx = cp->cgx;
  ax = cx->ax;
  wn = ax->wn;
  draw_gc = copy_GC(cp);
  undraw_gc = erase_GC(cp);
  min = ((nj < previous_j) ? nj : previous_j);
  if (cp->graph_style == GRAPH_LINES)
    {
      for (i = 0, j = 1; i < min-1; i++, j++)
	{
	  gdk_draw_line(wn, undraw_gc, ms->p0[i].x, ms->p0[i].y, ms->p0[j].x, ms->p0[j].y);
	  gdk_draw_line(wn, draw_gc, points[i].x, points[i].y, points[j].x, points[j].y);
	  gdk_draw_line(wn, undraw_gc, ms->p1[i].x, ms->p1[i].y, ms->p1[j].x, ms->p1[j].y);
	  gdk_draw_line(wn, draw_gc, points1[i].x, points1[i].y, points1[j].x, points1[j].y);
	}
      if (nj > previous_j)
	{
	  for (i = min - 1; i < nj - 1; i++) 
	    {
	      gdk_draw_line(wn, draw_gc, points[i].x, points[i].y, points[i + 1].x, points[i + 1].y);
	      gdk_draw_line(wn, draw_gc, points1[i].x, points1[i].y, points1[i + 1].x, points1[i + 1].y);
	    }
	}
      else
	if (previous_j > nj)
	  {
	    for (i = min - 1; i < previous_j - 1; i++) 
	      {
		gdk_draw_line(wn, undraw_gc, ms->p0[i].x, ms->p0[i].y, ms->p0[i + 1].x, ms->p0[i + 1].y);
		gdk_draw_line(wn, undraw_gc, ms->p1[i].x, ms->p1[i].y, ms->p1[i + 1].x, ms->p1[i + 1].y);
	      }
	  }
    }
  else /* dots */
    {
      for (i = 0; i < min; i++)
	{
	  draw_point(wn, undraw_gc, ms->p0[i], cp->dot_size);
	  draw_point(wn, draw_gc, points[i], cp->dot_size);
	  draw_point(wn, undraw_gc, ms->p1[i], cp->dot_size);
	  draw_point(wn, draw_gc, points1[i], cp->dot_size);
	}
      if (nj > previous_j)
	{
	  for (i = min; i < nj; i++) 
	    {
	      draw_point(wn, draw_gc, points[i], cp->dot_size);
	      draw_point(wn, draw_gc, points1[i], cp->dot_size);
	    }
	}
      else
	{
	  if (previous_j > nj)
	    {
	      for (i = min; i < previous_j; i++) 
		{
		  draw_point(wn, undraw_gc, ms->p0[i], cp->dot_size);
		  draw_point(wn, undraw_gc, ms->p1[i], cp->dot_size);
		}
	    }
	}

    }
  backup_erase_grf_points(ms, nj);
}

void setup_axis_context(chan_info *cp, axis_context *ax)
{
  GtkWidget *w;
  snd_info *sp;
  sp = cp->sound;
  if (cp->tcgx) 
    w = channel_graph(sp->chans[0]);
  else w = channel_graph(cp);
  ax->gc = copy_GC(cp);
  ax->wn = w->window;
}


/* colormaps */
/* should I use the RGB stuff in gdk rather than colormaps? */

static int sono_size = -1;
static GdkColor *grays[COLORMAP_SIZE];
static int grays_allocated = -1;
static GdkRectangle *sono_data[COLORMAP_SIZE];
static GdkGC *colormap_GC;

void initialize_colormap(snd_state *ss)
{
  state_context *sx;
  sx = ss->sgx;
  colormap_GC = gdk_gc_new(MAIN_WINDOW(ss));
  gdk_gc_set_background(sx->basic_gc, sx->graph_color);
  gdk_gc_set_foreground(sx->basic_gc, sx->data_color);
}

void draw_sono_rectangles(axis_context *ax, int color, int jmax)
{
  int i;
  gdk_gc_set_foreground(colormap_GC, grays[color]);
  for (i = 0; i < jmax; i++)
    gdk_draw_rectangle(ax->wn, colormap_GC, TRUE, 
		       sono_data[color][i].x, 
		       sono_data[color][i].y, 
		       sono_data[color][i].width, 
		       sono_data[color][i].height);
}

void draw_spectro_line(axis_context *ax, int color, int x0, int y0, int x1, int y1)
{
  gdk_gc_set_foreground(colormap_GC, grays[color]);
  gdk_draw_line(ax->wn, colormap_GC, x0, y0, x1, y1);
}

void set_sono_rectangle(int j, int color, Locus x, Locus y, Latus width, Latus height)
{
  GdkRectangle *r;
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
      for (i = 0; i < COLORMAP_SIZE; i++)
	{
	  if ((sono_size > 0) && (sono_data[i])) 
	    FREE(sono_data[i]); 
	  sono_data[i] = NULL;
	}
      for (i = 0; i < COLORMAP_SIZE; i++)
	{
	  sono_data[i] = (GdkRectangle *)CALLOC(size, sizeof(GdkRectangle));
	}
      sono_size = size;
    }
}

void allocate_color_map(snd_state *ss, int colormap)
{
  int i, j;
  GdkColormap *cmap;
  GdkColor tmp_color;
  unsigned short *curmap;
  if (grays_allocated != colormap)
    {
      curmap = snd_colormap(colormap);
      cmap = gdk_colormap_get_system();
      if (grays_allocated != -1) 
	for (i = 0; i < COLORMAP_SIZE; i++) 
	  gdk_color_free(grays[i]);
      j = 0;
      for (i = 0; i < COLORMAP_SIZE; i++)
	{
	  tmp_color.red = curmap[j++];
	  tmp_color.green = curmap[j++];
	  tmp_color.blue = curmap[j++];
	  grays[i] = gdk_color_copy(&tmp_color);
	  gdk_color_alloc(cmap, grays[i]);
	}
      grays_allocated = colormap;
    }
}


/* -------- color browser -------- */

typedef struct {
  GtkWidget *dialog;
  GtkWidget *list; 
  GtkWidget *scale; 
  GtkObject *scale_adj;
  GtkWidget *invert;
  GtkWidget *cutoff;
  GtkObject *cutoff_adj;
  snd_state *state;
} color_chooser_info;

static color_chooser_info *ccd = NULL;

static void invert_color_callback(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  in_set_color_inverted(ss, GTK_TOGGLE_BUTTON(w)->active);
  map_over_chans(ss, update_graph, NULL);
}

void set_color_inverted(snd_state *ss, int val)
{
  in_set_color_inverted(ss, val);
  if (ccd) set_toggle_button(ccd->invert, FALSE, FALSE, (gpointer)ss);
  if (!(ss->graph_hook_active)) map_over_chans(ss, update_graph, NULL);
}

static void scale_color_callback(GtkAdjustment *adj, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  gfloat scale_val, val;
  scale_val = adj->value;
  if (scale_val <= 50) 
    val = (Float)(scale_val + 1) / 51.0;
  else val = 1.0 + (Float)(scale_val - 50) * 20.0;
  in_set_color_scale(ss, val);
  map_over_chans(ss, update_graph, NULL);
}

static void reflect_color_scale(Float val)
{
  gfloat new_val;
  if (val <= 1.0) 
    new_val = (val * 51.0 - 1);
  else new_val = (val - 1.0) / 20.0 + 50.0;
  if (ccd) gtk_adjustment_set_value(GTK_ADJUSTMENT(ccd->scale_adj), new_val);
}

void set_color_scale(snd_state *ss, Float val)
{
  in_set_color_scale(ss, val);
  if (ccd) reflect_color_scale(color_scale(ss));
  if (!(ss->graph_hook_active)) map_over_chans(ss, update_graph, NULL);
}

static void list_color_callback(GtkWidget *w, gint row, gint column, GdkEventButton *event, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  in_set_color_map(ss, row);
  map_over_chans(ss, update_graph, NULL);
}

void set_color_map(snd_state *ss, int val)
{
  in_set_color_map(ss, val);
  if (ccd) gtk_clist_select_row(GTK_CLIST(ccd->list), val, 0);
  if (!(ss->graph_hook_active)) map_over_chans(ss, update_graph, NULL);
}

static void cutoff_color_callback(GtkAdjustment *adj, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  in_set_color_cutoff(ss, adj->value);
  map_over_chans(ss, update_graph, NULL);
}

void set_color_cutoff(snd_state *ss, Float val)
{
  in_set_color_cutoff(ss, val);
  if (ccd) gtk_adjustment_set_value(GTK_ADJUSTMENT(ccd->cutoff_adj), val);
  if (!(ss->graph_hook_active)) map_over_chans(ss, update_graph, NULL);
}

static void dismiss_color_callback(GtkWidget *w, gpointer context)
{
  color_chooser_info *cd = (color_chooser_info *)context;
  gtk_widget_hide(cd->dialog);
}

static void help_color_callback(GtkWidget *w, gpointer context)
{
  color_dialog_help((snd_state *)context);
}

static void delete_color_dialog(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(ccd->dialog);
}

void view_color_callback(GtkWidget *w, gpointer context)
{
  GtkWidget *light_label, *dark_label, *help_button, *dismiss_button;
  GtkWidget *outer_table, *scale_box, *cutoff_box, *cutoff_label, *colormap_scroller, *maplabel, *colormap_box;
  int i;
  char *str;
  snd_state *ss = (snd_state *)context;
  if (!ccd)
    {
      /* create color chooser dialog window */
      ccd = (color_chooser_info *)CALLOC(1, sizeof(color_chooser_info));
      ccd->state = ss;
      ccd->dialog = gtk_dialog_new();
      gtk_signal_connect(GTK_OBJECT(ccd->dialog), "delete_event", GTK_SIGNAL_FUNC(delete_color_dialog), (gpointer)ss);
      gtk_window_set_title(GTK_WINDOW(ccd->dialog), STR_Color_Editor);
      ALLOW_SHRINK_GROW(GTK_WINDOW(ccd->dialog));
      set_background(ccd->dialog, (ss->sgx)->basic_color);
      gtk_container_set_border_width (GTK_CONTAINER(ccd->dialog), 4);
      gtk_widget_realize(ccd->dialog);
      SET_USIZE(GTK_WIDGET(ccd->dialog), 260, 200);

      help_button = gtk_button_new_with_label(STR_Help);
      dismiss_button = gtk_button_new_with_label(STR_Dismiss);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(ccd->dialog)->action_area), dismiss_button, FALSE, TRUE, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(ccd->dialog)->action_area), help_button, FALSE, TRUE, 10);
      gtk_signal_connect(GTK_OBJECT(dismiss_button), "clicked", GTK_SIGNAL_FUNC(dismiss_color_callback), (gpointer)ccd);
      gtk_signal_connect(GTK_OBJECT(help_button), "clicked", GTK_SIGNAL_FUNC(help_color_callback), (gpointer)ss);
      set_pushed_button_colors(help_button, ss);
      set_pushed_button_colors(dismiss_button, ss);
      gtk_widget_show(dismiss_button);
      gtk_widget_show(help_button);

      outer_table = gtk_table_new(3, 2, FALSE);
      gtk_container_add(GTK_CONTAINER(GTK_DIALOG(ccd->dialog)->vbox), outer_table);

      scale_box = gtk_table_new(2, 2, FALSE);
      gtk_table_attach_defaults(GTK_TABLE(outer_table), scale_box, 0, 1, 0, 1);
      
      ccd->scale_adj = gtk_adjustment_new(50.0, 0.0, 101.0, 0.1, 1.0, 1.0);
      ccd->scale = gtk_hscale_new(GTK_ADJUSTMENT(ccd->scale_adj));
      GTK_WIDGET_UNSET_FLAGS(ccd->scale, GTK_CAN_FOCUS);
      /* SET_USIZE (GTK_WIDGET(ccd->scale), 200, 30); */
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(ccd->scale)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_digits(GTK_SCALE(ccd->scale), 0);
      gtk_scale_set_value_pos(GTK_SCALE(ccd->scale), GTK_POS_TOP);
      gtk_scale_set_draw_value(GTK_SCALE(ccd->scale), TRUE);
      gtk_table_attach_defaults(GTK_TABLE(scale_box), ccd->scale, 0, 2, 0, 1);
      gtk_signal_connect(GTK_OBJECT(ccd->scale_adj), "value_changed", GTK_SIGNAL_FUNC(scale_color_callback), (gpointer)ss);

      light_label = gtk_label_new(STR_light);
      gtk_misc_set_alignment(GTK_MISC (light_label), 0.05, 0.0);
      gtk_table_attach_defaults(GTK_TABLE(scale_box), light_label, 0, 1, 1, 2);
      dark_label = gtk_label_new(STR_dark);
      gtk_misc_set_alignment(GTK_MISC(dark_label), 0.95, 0.0);
      gtk_table_attach_defaults(GTK_TABLE(scale_box), dark_label, 1, 2, 1, 2);

      gtk_widget_show(ccd->scale);
      gtk_widget_show(light_label);
      gtk_widget_show(dark_label);
      gtk_widget_show(scale_box);

      cutoff_box = gtk_table_new(2, 2, FALSE);
      gtk_table_attach_defaults(GTK_TABLE(outer_table), cutoff_box, 0, 1, 1, 2);

      ccd->cutoff_adj = gtk_adjustment_new(color_cutoff(ss), 0.0, 1.01, 0.001, 0.01, .01);
      ccd->cutoff = gtk_hscale_new(GTK_ADJUSTMENT(ccd->cutoff_adj));
      GTK_WIDGET_UNSET_FLAGS(ccd->cutoff, GTK_CAN_FOCUS);
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(ccd->cutoff)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_digits(GTK_SCALE(ccd->cutoff), 3);
      gtk_scale_set_value_pos(GTK_SCALE(ccd->cutoff), GTK_POS_TOP);
      gtk_scale_set_draw_value(GTK_SCALE(ccd->cutoff), TRUE);
      gtk_table_attach_defaults(GTK_TABLE(cutoff_box), ccd->cutoff, 0, 2, 0, 1);
      gtk_signal_connect(GTK_OBJECT(ccd->cutoff_adj), "value_changed", GTK_SIGNAL_FUNC(cutoff_color_callback), (gpointer)ss);

      cutoff_label = gtk_label_new(STR_cutoff);
      gtk_misc_set_alignment(GTK_MISC (cutoff_label), 0.1, 0.0);
      gtk_table_attach_defaults(GTK_TABLE(cutoff_box), cutoff_label, 0, 2, 1, 2);

      gtk_widget_show(ccd->cutoff);
      gtk_widget_show(cutoff_label);
      gtk_widget_show(cutoff_box);

      ccd->invert = gtk_check_button_new_with_label(STR_invert);
      gtk_table_attach(GTK_TABLE(outer_table), ccd->invert, 0, 1, 2, 3, (GtkAttachOptions)0, (GtkAttachOptions)0, 0, 4);
      gtk_signal_connect(GTK_OBJECT(ccd->invert), "toggled", GTK_SIGNAL_FUNC(invert_color_callback), (gpointer)ss);
      gtk_widget_show(ccd->invert);
      set_toggle_button(ccd->invert, color_inverted(ss), FALSE, (gpointer)ss);
      set_pushed_button_colors(ccd->invert, ss);

      colormap_box = gtk_vbox_new(FALSE, 0);
      gtk_table_attach(GTK_TABLE(outer_table), colormap_box, 1, 2, 0, 3,
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND),
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND), 
		       10, 4);

      maplabel = gtk_label_new(S_colormap);
      gtk_misc_set_alignment(GTK_MISC(maplabel), 0.5, 0.0);
      gtk_box_pack_start(GTK_BOX(colormap_box), maplabel, FALSE, FALSE, 0);
      gtk_widget_show(maplabel);

      ccd->list = gtk_clist_new(1);
      gtk_clist_set_selection_mode(GTK_CLIST(ccd->list), GTK_SELECTION_SINGLE);
      gtk_clist_set_shadow_type(GTK_CLIST(ccd->list), GTK_SHADOW_ETCHED_IN);
      gtk_clist_column_titles_passive(GTK_CLIST(ccd->list));
      for (i = 0; i < NUM_COLORMAPS; i++) 
	{
	  str = colormap_name(i);
	  gtk_clist_append(GTK_CLIST(ccd->list), &str);
	}
      gtk_signal_connect(GTK_OBJECT(ccd->list), "select_row", GTK_SIGNAL_FUNC(list_color_callback), (gpointer)ss);

      colormap_scroller = gtk_scrolled_window_new(NULL, NULL);
      gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(colormap_scroller), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
      gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(colormap_scroller), ccd->list);
      gtk_box_pack_start(GTK_BOX(colormap_box), colormap_scroller, TRUE, TRUE, 0);

      gtk_widget_show(ccd->list);
      gtk_widget_show(colormap_scroller);
      gtk_widget_show(colormap_box);

      gtk_widget_show(outer_table);
      set_dialog_widget(ss, COLOR_DIALOG, ccd->dialog);
    }
  else raise_dialog(ccd->dialog);
  gtk_widget_show(ccd->dialog);
}

int color_dialog_is_active(void)
{
  return((ccd) && (ccd->dialog) && (GTK_WIDGET_VISIBLE(ccd->dialog))); /* ismanaged ...? */
}

GtkWidget *start_color_dialog(snd_state *ss, int width, int height)
{
  view_color_callback(NULL, (gpointer)ss);
  if (width != 0) SET_USIZE(GTK_WIDGET(ccd->dialog), width, height);
  return(ccd->dialog);
}



/* -------- orientation browser -------- */

typedef struct {
  GtkWidget *dialog;
  GtkWidget *ax, *ay, *az, *sx, *sy, *sz, *hop, *cut; 
  GtkObject *ax_adj, *az_adj, *ay_adj, *sx_adj, *sz_adj, *sy_adj, *hop_adj, *cut_adj;
  snd_state *state;
} orientation_info;

static orientation_info *oid = NULL;

static void ax_orientation_callback(GtkAdjustment *adj, gpointer context) 
{
  snd_state *ss;
  orientation_info *od = (orientation_info *)context;
  ss = od->state;
  in_set_spectro_x_angle(ss, (Float)(adj->value));
  map_chans_field(ss, FCP_X_ANGLE, (Float)(adj->value));
  map_over_chans(ss, update_graph, NULL);
}

void set_spectro_x_angle(snd_state *ss, Float val)
{
  in_set_spectro_x_angle(ss, val);
  if (oid) gtk_adjustment_set_value(GTK_ADJUSTMENT(oid->ax_adj), val);
  map_chans_field(ss, FCP_X_ANGLE, val);
  if (!(ss->graph_hook_active)) map_over_chans(ss, update_graph, NULL);
}

static void ay_orientation_callback(GtkAdjustment *adj, gpointer context) 
{
  snd_state *ss;
  orientation_info *od = (orientation_info *)context;
  ss = od->state;
  in_set_spectro_y_angle(ss, (Float)(adj->value));
  map_chans_field(ss, FCP_Y_ANGLE, (Float)(adj->value));
  map_over_chans(ss, update_graph, NULL);
}

void set_spectro_y_angle(snd_state *ss, Float val)
{
  in_set_spectro_y_angle(ss, val);
  if (oid) gtk_adjustment_set_value(GTK_ADJUSTMENT(oid->ay_adj), val);
  map_chans_field(ss, FCP_Y_ANGLE, val);
  if (!(ss->graph_hook_active)) map_over_chans(ss, update_graph, NULL);
}

static void az_orientation_callback(GtkAdjustment *adj, gpointer context) 
{
  snd_state *ss;
  orientation_info *od = (orientation_info *)context;
  ss = od->state;
  in_set_spectro_z_angle(ss, (Float)(adj->value));
  map_chans_field(ss, FCP_Z_ANGLE, (Float)(adj->value));
  map_over_chans(ss, update_graph, NULL);
}

void set_spectro_z_angle(snd_state *ss, Float val)
{
  in_set_spectro_z_angle(ss, val);
  if (oid) gtk_adjustment_set_value(GTK_ADJUSTMENT(oid->az_adj), val);
  map_chans_field(ss, FCP_Z_ANGLE, val);
  if (!(ss->graph_hook_active)) map_over_chans(ss, update_graph, NULL);
}

static void sx_orientation_callback(GtkAdjustment *adj, gpointer context) 
{
  snd_state *ss;
  orientation_info *od = (orientation_info *)context;
  ss = od->state;
  in_set_spectro_x_scale(ss, (Float)(adj->value));
  map_chans_field(ss, FCP_X_SCALE, (Float)(adj->value));
  map_over_chans(ss, update_graph, NULL);
}

void set_spectro_x_scale(snd_state *ss, Float val)
{
  in_set_spectro_x_scale(ss, val);
  if (oid) gtk_adjustment_set_value(GTK_ADJUSTMENT(oid->sx_adj), val);
  map_chans_field(ss, FCP_X_SCALE, val);
  if (!(ss->graph_hook_active)) map_over_chans(ss, update_graph, NULL);
}

static void sy_orientation_callback(GtkAdjustment *adj, gpointer context) 
{
  snd_state *ss;
  orientation_info *od = (orientation_info *)context;
  ss = od->state;
  in_set_spectro_y_scale(ss, (Float)(adj->value));
  map_chans_field(ss, FCP_Y_SCALE, (Float)(adj->value));
  map_over_chans(ss, update_graph, NULL);
}

void set_spectro_y_scale(snd_state *ss, Float val)
{
  in_set_spectro_y_scale(ss, val);
  if (oid) gtk_adjustment_set_value(GTK_ADJUSTMENT(oid->sy_adj), val);
  map_chans_field(ss, FCP_Y_SCALE, val);
  if (!(ss->graph_hook_active)) map_over_chans(ss, update_graph, NULL);
}

static void sz_orientation_callback(GtkAdjustment *adj, gpointer context) 
{
  snd_state *ss;
  orientation_info *od = (orientation_info *)context;
  ss = od->state;
  in_set_spectro_z_scale(ss, (Float)(adj->value));
  map_chans_field(ss, FCP_Z_SCALE, (Float)(adj->value));
  map_over_chans(ss, update_graph, NULL);
}

void set_spectro_z_scale(snd_state *ss, Float val)
{
  in_set_spectro_z_scale(ss, val);
  if (oid) gtk_adjustment_set_value(GTK_ADJUSTMENT(oid->sz_adj), val);
  map_chans_field(ss, FCP_Z_SCALE, val);
  if (!(ss->graph_hook_active)) map_over_chans(ss, update_graph, NULL);
}

static int map_chans_spectro_hop(chan_info *cp, void *ptr) {cp->spectro_hop = (*((int *)ptr)); return(0);}

static void hop_orientation_callback(GtkAdjustment *adj, gpointer context) 
{
  snd_state *ss;
  int val;
  orientation_info *od = (orientation_info *)context;
  ss = od->state;
  val = mus_iclamp(1, (int)(adj->value), 20);
  in_set_spectro_hop(ss, val);
  map_over_chans(ss, map_chans_spectro_hop, (void *)(&val));
  map_over_chans(ss, update_graph, NULL);
}

void set_spectro_hop(snd_state *ss, int val)
{
  if (val > 0)
    {
      in_set_spectro_hop(ss, val);
      if (oid) gtk_adjustment_set_value(GTK_ADJUSTMENT(oid->hop_adj), val);
      map_over_chans(ss, map_chans_spectro_hop, (void *)(&val));
      if (!(ss->graph_hook_active)) map_over_chans(ss, update_graph, NULL);
    }
}

static void cut_orientation_callback(GtkAdjustment *adj, gpointer context) 
{
  /* y axis limit */
  snd_state *ss;
  orientation_info *od = (orientation_info *)context;
  ss = od->state;
  map_chans_field(ss, FCP_CUTOFF, (Float)(adj->value));
  set_spectro_cutoff_and_redisplay(ss, (Float)(adj->value)); /* calls in_set... */
} 

void set_spectro_cutoff(snd_state *ss, Float val)
{
  in_set_spectro_cutoff(ss, val);
  if (oid) gtk_adjustment_set_value(GTK_ADJUSTMENT(oid->cut_adj), val);
  map_chans_field(ss, FCP_CUTOFF, val);
  if (!(ss->graph_hook_active)) map_over_chans(ss, update_graph, NULL);
}

static void help_orientation_callback(GtkWidget *w, gpointer context)
{
  orientation_dialog_help((snd_state *)context);
}

static void dismiss_orientation_callback(GtkAdjustment *adj, gpointer context) 
{
  orientation_info *od = (orientation_info *)context;
  gtk_widget_hide(od->dialog);
}

static void delete_orientation_dialog(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(oid->dialog);
}

static int fixup_angle(Float ang)
{
  int na;
  na = (int)ang;
  na = na % 360;
  if (na < 0) na += 360;
  return(na);
}

void reflect_spectro(snd_state *ss)
{
  /* set color/orientaton widget values */
  if (ccd)
    {
      set_toggle_button(ccd->invert, color_inverted(ss), FALSE, (gpointer)ss);
      gtk_adjustment_set_value(GTK_ADJUSTMENT(ccd->cutoff_adj), color_cutoff(ss));
      reflect_color_scale(color_scale(ss));
    }
  if (oid) 
    {
      gtk_adjustment_set_value(GTK_ADJUSTMENT(oid->ax_adj), fixup_angle(spectro_x_angle(ss)));
      gtk_adjustment_set_value(GTK_ADJUSTMENT(oid->ay_adj), fixup_angle(spectro_y_angle(ss)));
      gtk_adjustment_set_value(GTK_ADJUSTMENT(oid->az_adj), fixup_angle(spectro_z_angle(ss)));
      gtk_adjustment_set_value(GTK_ADJUSTMENT(oid->sx_adj), spectro_x_scale(ss));
      gtk_adjustment_set_value(GTK_ADJUSTMENT(oid->sy_adj), spectro_y_scale(ss));
      gtk_adjustment_set_value(GTK_ADJUSTMENT(oid->sz_adj), spectro_z_scale(ss));
      gtk_adjustment_set_value(GTK_ADJUSTMENT(oid->hop_adj), (spectro_hop(ss) > 100) ? 100 : (spectro_hop(ss)));
      gtk_adjustment_set_value(GTK_ADJUSTMENT(oid->cut_adj), (spectro_cutoff(ss)));
    }
}

static void reset_orientation_callback(GtkWidget *w, gpointer context)
{
  snd_state *ss;
  orientation_info *od = (orientation_info *)context;
  /* put everything back the way it was at the start */
  ss = od->state;
  reset_spectro(ss);
  reflect_spectro(ss);
  map_over_chans(ss, update_graph, NULL);
}

void view_orientation_callback(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  GtkWidget *outer_table, *dismiss_button, *help_button, *reset_button;
  GtkWidget *ax_box, *ay_box, *az_box, *sx_box, *sy_box, *sz_box, *hop_box, *cut_box;
  GtkWidget *ax_label, *ay_label, *az_label, *sx_label, *sy_label, *sz_label, *hop_label, *cut_label;
  /* set up dialog with table 8 by 2 */
  if (!oid)
    {
      /* create orientation window */
      oid = (orientation_info *)CALLOC(1, sizeof(orientation_info));
      oid->state = ss;

      oid->dialog = gtk_dialog_new();
      gtk_signal_connect(GTK_OBJECT(oid->dialog), "delete_event", GTK_SIGNAL_FUNC(delete_orientation_dialog), (gpointer)ss);
      gtk_window_set_title(GTK_WINDOW(oid->dialog), STR_Spectrogram_Orientation);
      ALLOW_SHRINK_GROW(GTK_WINDOW(oid->dialog));
      set_background(oid->dialog, (ss->sgx)->basic_color);
      gtk_container_set_border_width (GTK_CONTAINER(oid->dialog), 4);
      gtk_widget_realize(oid->dialog);
      SET_USIZE(GTK_WIDGET(oid->dialog), 260, 300);

      help_button = gtk_button_new_with_label(STR_Help);
      dismiss_button = gtk_button_new_with_label(STR_Dismiss);
      reset_button = gtk_button_new_with_label(STR_Reset);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(oid->dialog)->action_area), reset_button, FALSE, TRUE, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(oid->dialog)->action_area), dismiss_button, FALSE, TRUE, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(oid->dialog)->action_area), help_button, FALSE, TRUE, 10);
      gtk_signal_connect(GTK_OBJECT(reset_button), "clicked", GTK_SIGNAL_FUNC(reset_orientation_callback), (gpointer)oid);
      gtk_signal_connect(GTK_OBJECT(dismiss_button), "clicked", GTK_SIGNAL_FUNC(dismiss_orientation_callback), (gpointer)oid);
      gtk_signal_connect(GTK_OBJECT(help_button), "clicked", GTK_SIGNAL_FUNC(help_orientation_callback), (gpointer)ss);
      set_pushed_button_colors(reset_button, ss);
      set_pushed_button_colors(help_button, ss);
      set_pushed_button_colors(dismiss_button, ss);
      gtk_widget_show(reset_button);
      gtk_widget_show(dismiss_button);
      gtk_widget_show(help_button);

      outer_table = gtk_table_new(4, 2, FALSE);
      gtk_container_add(GTK_CONTAINER(GTK_DIALOG(oid->dialog)->vbox), outer_table);

      /* AX */
      ax_box = gtk_table_new(2, 1, FALSE);
      gtk_table_attach_defaults(GTK_TABLE(outer_table), ax_box, 0, 1, 0, 1);

      oid->ax_adj = gtk_adjustment_new(spectro_x_angle(ss), 0.0, 361.0, 1.0, 10.0, 1.0);
      oid->ax = gtk_hscale_new(GTK_ADJUSTMENT(oid->ax_adj));
      GTK_WIDGET_UNSET_FLAGS(oid->ax, GTK_CAN_FOCUS);
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(oid->ax)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_digits(GTK_SCALE(oid->ax), 0);
      gtk_scale_set_value_pos(GTK_SCALE(oid->ax), GTK_POS_TOP);
      gtk_scale_set_draw_value(GTK_SCALE(oid->ax), TRUE);
      gtk_table_attach_defaults(GTK_TABLE(ax_box), oid->ax, 0, 1, 0, 1);
      gtk_signal_connect(GTK_OBJECT(oid->ax_adj), "value_changed", GTK_SIGNAL_FUNC(ax_orientation_callback), (gpointer)oid);

      ax_label = gtk_label_new(STR_x_angle);
      gtk_misc_set_alignment(GTK_MISC (ax_label), 0.1, 0.0);
      gtk_table_attach_defaults(GTK_TABLE(ax_box), ax_label, 0, 1, 1, 2);

      gtk_widget_show(oid->ax);
      gtk_widget_show(ax_label);
      gtk_widget_show(ax_box);

      /* AY */
      ay_box = gtk_table_new(2, 1, FALSE);
      gtk_table_attach_defaults(GTK_TABLE(outer_table), ay_box, 0, 1, 1, 2);

      oid->ay_adj = gtk_adjustment_new(spectro_y_angle(ss), 0.0, 361.0, 1.0, 10.0, 1.0);
      oid->ay = gtk_hscale_new(GTK_ADJUSTMENT(oid->ay_adj));
      GTK_WIDGET_UNSET_FLAGS(oid->ay, GTK_CAN_FOCUS);
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(oid->ay)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_digits(GTK_SCALE(oid->ay), 0);
      gtk_scale_set_value_pos(GTK_SCALE(oid->ay), GTK_POS_TOP);
      gtk_scale_set_draw_value(GTK_SCALE(oid->ay), TRUE);
      gtk_table_attach_defaults(GTK_TABLE(ay_box), oid->ay, 0, 1, 0, 1);
      gtk_signal_connect(GTK_OBJECT(oid->ay_adj), "value_changed", GTK_SIGNAL_FUNC(ay_orientation_callback), (gpointer)oid);

      ay_label = gtk_label_new(STR_y_angle);
      gtk_misc_set_alignment(GTK_MISC(ay_label), 0.1, 0.0);
      gtk_table_attach_defaults(GTK_TABLE(ay_box), ay_label, 0, 1, 1, 2);

      gtk_widget_show(oid->ay);
      gtk_widget_show(ay_label);
      gtk_widget_show(ay_box);

      /* AZ */
      az_box = gtk_table_new(2, 1, FALSE);
      gtk_table_attach_defaults(GTK_TABLE(outer_table), az_box, 0, 1, 2, 3);

      oid->az_adj = gtk_adjustment_new(spectro_z_angle(ss), 0.0, 361.0, 1.0, 10.0, 1.0);
      oid->az = gtk_hscale_new(GTK_ADJUSTMENT(oid->az_adj));
      GTK_WIDGET_UNSET_FLAGS(oid->az, GTK_CAN_FOCUS);
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(oid->az)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_digits(GTK_SCALE(oid->az), 0);
      gtk_scale_set_value_pos(GTK_SCALE(oid->az), GTK_POS_TOP);
      gtk_scale_set_draw_value(GTK_SCALE(oid->az), TRUE);
      gtk_table_attach_defaults(GTK_TABLE(az_box), oid->az, 0, 1, 0, 1);
      gtk_signal_connect(GTK_OBJECT(oid->az_adj), "value_changed", GTK_SIGNAL_FUNC(az_orientation_callback), (gpointer)oid);

      az_label = gtk_label_new(STR_z_angle);
      gtk_misc_set_alignment(GTK_MISC (az_label), 0.1, 0.0);
      gtk_table_attach_defaults(GTK_TABLE(az_box), az_label, 0, 1, 1, 2);

      gtk_widget_show(oid->az);
      gtk_widget_show(az_label);
      gtk_widget_show(az_box);

      /* HOP */
      hop_box = gtk_table_new(2, 1, FALSE);
      gtk_table_attach_defaults(GTK_TABLE(outer_table), hop_box, 0, 1, 3, 4);

      oid->hop_adj = gtk_adjustment_new((spectro_hop(ss) > 20) ? 20 : (spectro_hop(ss)), 0.0, 21.0, 0.1, 1.0, 1.0);
      oid->hop = gtk_hscale_new(GTK_ADJUSTMENT(oid->hop_adj));
      GTK_WIDGET_UNSET_FLAGS(oid->hop, GTK_CAN_FOCUS);
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(oid->hop)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_digits(GTK_SCALE(oid->hop), 0);
      gtk_scale_set_value_pos(GTK_SCALE(oid->hop), GTK_POS_TOP);
      gtk_scale_set_draw_value(GTK_SCALE(oid->hop), TRUE);
      gtk_table_attach_defaults(GTK_TABLE(hop_box), oid->hop, 0, 1, 0, 1);
      gtk_signal_connect(GTK_OBJECT(oid->hop_adj), "value_changed", GTK_SIGNAL_FUNC(hop_orientation_callback), (gpointer)oid);

      hop_label = gtk_label_new(STR_hop);
      gtk_misc_set_alignment(GTK_MISC (hop_label), 0.1, 0.0);
      gtk_table_attach_defaults(GTK_TABLE(hop_box), hop_label, 0, 1, 1, 2);

      gtk_widget_show(oid->hop);
      gtk_widget_show(hop_label);
      gtk_widget_show(hop_box);

      /* SX */
      sx_box = gtk_table_new(2, 1, FALSE);
      gtk_table_attach_defaults(GTK_TABLE(outer_table), sx_box, 1, 2, 0, 1);

      oid->sx_adj = gtk_adjustment_new(spectro_x_scale(ss), 0.0, 2.01, .01, .1, .01);
      oid->sx = gtk_hscale_new(GTK_ADJUSTMENT(oid->sx_adj));
      GTK_WIDGET_UNSET_FLAGS(oid->sx, GTK_CAN_FOCUS);
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(oid->sx)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_digits(GTK_SCALE(oid->sx), 2);
      gtk_scale_set_value_pos(GTK_SCALE(oid->sx), GTK_POS_TOP);
      gtk_scale_set_draw_value(GTK_SCALE(oid->sx), TRUE);
      gtk_table_attach_defaults(GTK_TABLE(sx_box), oid->sx, 0, 1, 0, 1);
      gtk_signal_connect(GTK_OBJECT(oid->sx_adj), "value_changed", GTK_SIGNAL_FUNC(sx_orientation_callback), (gpointer)oid);

      sx_label = gtk_label_new(STR_x_scale);
      gtk_misc_set_alignment(GTK_MISC (sx_label), 0.1, 0.0);
      gtk_table_attach_defaults(GTK_TABLE(sx_box), sx_label, 0, 1, 1, 2);

      gtk_widget_show(oid->sx);
      gtk_widget_show(sx_label);
      gtk_widget_show(sx_box);

      /* SY */
      sy_box = gtk_table_new(2, 1, FALSE);
      gtk_table_attach_defaults(GTK_TABLE(outer_table), sy_box, 1, 2, 1, 2);

      oid->sy_adj = gtk_adjustment_new(spectro_y_scale(ss), 0.0, 2.01, .01, .1, .01);
      oid->sy = gtk_hscale_new(GTK_ADJUSTMENT(oid->sy_adj));
      GTK_WIDGET_UNSET_FLAGS(oid->sy, GTK_CAN_FOCUS);
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(oid->sy)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_digits(GTK_SCALE(oid->sy), 2);
      gtk_scale_set_value_pos(GTK_SCALE(oid->sy), GTK_POS_TOP);
      gtk_scale_set_draw_value(GTK_SCALE(oid->sy), TRUE);
      gtk_table_attach_defaults(GTK_TABLE(sy_box), oid->sy, 0, 1, 0, 1);
      gtk_signal_connect(GTK_OBJECT(oid->sy_adj), "value_changed", GTK_SIGNAL_FUNC(sy_orientation_callback), (gpointer)oid);

      sy_label = gtk_label_new(STR_y_scale);
      gtk_misc_set_alignment(GTK_MISC (sy_label), 0.1, 0.0);
      gtk_table_attach_defaults(GTK_TABLE(sy_box), sy_label, 0, 1, 1, 2);

      gtk_widget_show(oid->sy);
      gtk_widget_show(sy_label);
      gtk_widget_show(sy_box);

      /* SZ */
      sz_box = gtk_table_new(2, 1, FALSE);
      gtk_table_attach_defaults(GTK_TABLE(outer_table), sz_box, 1, 2, 2, 3);

      oid->sz_adj = gtk_adjustment_new(spectro_z_scale(ss), 0.0, 2.01, .01, .1, .01);
      oid->sz = gtk_hscale_new(GTK_ADJUSTMENT(oid->sz_adj));
      GTK_WIDGET_UNSET_FLAGS(oid->sz, GTK_CAN_FOCUS);
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(oid->sz)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_digits(GTK_SCALE(oid->sz), 3);
      gtk_scale_set_value_pos(GTK_SCALE(oid->sz), GTK_POS_TOP);
      gtk_scale_set_draw_value(GTK_SCALE(oid->sz), TRUE);
      gtk_table_attach_defaults(GTK_TABLE(sz_box), oid->sz, 0, 1, 0, 1);
      gtk_signal_connect(GTK_OBJECT(oid->sz_adj), "value_changed", GTK_SIGNAL_FUNC(sz_orientation_callback), (gpointer)oid);

      sz_label = gtk_label_new(STR_z_scale);
      gtk_misc_set_alignment(GTK_MISC (sz_label), 0.1, 0.0);
      gtk_table_attach_defaults(GTK_TABLE(sz_box), sz_label, 0, 1, 1, 2);

      gtk_widget_show(oid->sz);
      gtk_widget_show(sz_label);
      gtk_widget_show(sz_box);

      /* CUT */
      cut_box = gtk_table_new(2, 1, FALSE);
      gtk_table_attach_defaults(GTK_TABLE(outer_table), cut_box, 1, 2, 3, 4);

      oid->cut_adj = gtk_adjustment_new(spectro_cutoff(ss), 0.0, 1.01, .01, .1, .01);
      oid->cut = gtk_hscale_new(GTK_ADJUSTMENT(oid->cut_adj));
      GTK_WIDGET_UNSET_FLAGS(oid->cut, GTK_CAN_FOCUS);
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(oid->cut)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_digits(GTK_SCALE(oid->cut), 2);
      gtk_scale_set_value_pos(GTK_SCALE(oid->cut), GTK_POS_TOP);
      gtk_scale_set_draw_value(GTK_SCALE(oid->cut), TRUE);
      gtk_table_attach_defaults(GTK_TABLE(cut_box), oid->cut, 0, 1, 0, 1);
      gtk_signal_connect(GTK_OBJECT(oid->cut_adj), "value_changed", GTK_SIGNAL_FUNC(cut_orientation_callback), (gpointer)oid);

      cut_label = gtk_label_new(STR_percent_of_spectrum);
      gtk_misc_set_alignment(GTK_MISC (cut_label), 0.1, 0.0);
      gtk_table_attach_defaults(GTK_TABLE(cut_box), cut_label, 0, 1, 1, 2);

      gtk_widget_show(oid->cut);
      gtk_widget_show(cut_label);
      gtk_widget_show(cut_box);

      gtk_widget_show(outer_table);
      set_dialog_widget(ss, ORIENTATION_DIALOG, oid->dialog);
    }
  else raise_dialog(oid->dialog);
  gtk_widget_show(oid->dialog);
}

int orientation_dialog_is_active(void)
{
  return((oid) && (oid->dialog) && (GTK_WIDGET_VISIBLE(oid->dialog)));
}

GtkWidget *start_orientation_dialog(snd_state *ss, int width, int height)
{
  view_orientation_callback(NULL, (gpointer)ss);
  if (width != 0) SET_USIZE(GTK_WIDGET(oid->dialog), width, height);
  return(oid->dialog);
}

