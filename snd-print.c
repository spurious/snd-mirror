#include "snd.h"

/* create Postscript version of graph */

static char *pbuf = NULL;
static int bbx = 0, bby = 0, bx0 = 0, by0 = 0;
static int ps_fd;

static char *nbuf = NULL;
static int nbuf_ctr = 0;
#define NBUF_SIZE 8192

static void ps_flush(int fd)
{
  if (nbuf_ctr > 0)
    {
      write(fd, nbuf, nbuf_ctr);
      nbuf_ctr = 0;
    }
}

static void ps_write(char *buf)
{
  /* sending tiny buffers over the net is a total loss -- grab a bunch at a time */
  if (!nbuf)
    {
      nbuf = (char *)CALLOC(NBUF_SIZE, sizeof(char));
      nbuf_ctr = 0;
    }
  while (*buf)
    {
      nbuf[nbuf_ctr] = (*buf);
      buf++;
      nbuf_ctr++;
      if (nbuf_ctr == NBUF_SIZE) ps_flush(ps_fd);
    }
}

static char *previous_locale = NULL;

static int start_ps_graph(char *output, char *title) 
{ 
  time_t ts;
  snd_state *ss;
  ps_fd = CREAT(output, 0666);
  if (ps_fd == -1) return(-1);
  if (!pbuf) pbuf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  bbx = 0;
  bby = 0;

#if HAVE_SETLOCALE
  previous_locale = copy_string(setlocale(LC_NUMERIC, "C")); /* must use decimal point in floats since PostScript assumes that format */
#endif

  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, 
	       "%%!PS-Adobe-2.0 EPSF-2.0\n%%%%Title: %s\n%%%%Creator: Snd: %s\n%%%%CreationDate: ", 
	       title, SND_VERSION);
  ps_write(pbuf);
#if HAVE_STRFTIME
  time(&ts);
  strftime(pbuf, PRINT_BUFFER_SIZE, STRFTIME_FORMAT, localtime(&ts));
  ps_write(pbuf);
#endif
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, 
	       "\n%%%%BoundingBox:(atend)\n%%%%EndComments\n%%%%EndProlog\n%%%%Page: 1 1\n");
  ps_write(pbuf);
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, 
	       "/LT {lineto} bind def\n/RF {rectfill} bind def\n/RG {setrgbcolor} bind def\n/NAF {newpath arc fill} bind def\n\n");
  ps_write(pbuf);
  ss = get_global_state();
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, 
	       "gsave [%.3f 0.0 0.0 %.3f %.3f %.3f] concat\n\n",
	       eps_size(ss), eps_size(ss), eps_left_margin(ss), eps_bottom_margin(ss));
  ps_write(pbuf);
  return(0);
}

static void ps_graph(chan_info *cp, int x0, int y0)
{
  cp->printing = TRUE;
  bx0 = x0;
  by0 = y0;
  display_channel_data(cp, cp->sound, cp->state);
  cp->printing = FALSE;
}

static void end_ps_graph(void)
{
  snd_state *ss;
  ss = get_global_state();
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, 
	       "%s\nshowpage\n%%%%Trailer\n%%%%BoundingBox: %d %d %d %d\n",
	       ((eps_left_margin(ss) != 0) || (eps_bottom_margin(ss) != 0)) ? "\ngrestore" : "",
	       0, 0,
	       (int)(bbx + 10 + eps_left_margin(ss)),
	       (int)(bby + 10 + eps_bottom_margin(ss)));
  ps_write(pbuf);
  ps_flush(ps_fd);
  snd_close(ps_fd, _("eps file"));
  if (previous_locale)
    {
#if HAVE_SETLOCALE
      setlocale(LC_NUMERIC, previous_locale);
#endif
      FREE(previous_locale);
      previous_locale = NULL;
    }
  if (nbuf)
    {
      FREE(nbuf);
      nbuf = NULL;
      nbuf_ctr = 0;
    }
}

/* the x and y values in the "points" are relative to grf_x/y:
 *
 *  x: ap->x_axis_x0 + (val - ap->x0) * ap->x_scale
 *  y: ap->y_axis_y0 + (val * MUS_FIX_TO_FLOAT - ap->y0) * ap->y_scale
 *
 * kept here in full precision since normally printers have much higher resolution than screens 
 */

static int reflect_y(axis_info *ap, int y)
{
  return(ap->height - y);
}

static Float *xpts = NULL;
static Float *ypts = NULL;
static Float *ypts1 = NULL;

void ps_allocate_grf_points(void)
{
  if (!xpts) xpts = (Float *)CALLOC(POINT_BUFFER_SIZE, sizeof(Float));
  if (!ypts) ypts = (Float *)CALLOC(POINT_BUFFER_SIZE, sizeof(Float));
  if (!ypts1) ypts1 = (Float *)CALLOC(POINT_BUFFER_SIZE, sizeof(Float));
}

void ps_set_grf_points(double x, int j, Float ymin, Float ymax) 
{
  xpts[j] = x;
  ypts[j] = ymin;
  ypts1[j] = ymax;
}

void ps_set_grf_point(double x, int j, Float y) 
{
  xpts[j] = x;
  ypts[j] = y;
}

static Float ps_grf_x(axis_info *ap, Float val)
{
  return(ap->x_axis_x0 + bx0 + (val - ap->x0) * ap->x_scale);
}

static Float ps_grf_y(axis_info *ap, Float val)
{
  return(by0 + ap->height - (ap->y_axis_y0 + (val - ap->y0) * ap->y_scale));
}

static void ps_draw_lines(axis_info *ap, int j, Float *xpts, Float *ypts)
{
  int i;
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f moveto\n", ps_grf_x(ap, xpts[0]), ps_grf_y(ap, ypts[0]));
  ps_write(pbuf);
  for (i = 1; i < j; i++)
    {
      mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f lineto\n", ps_grf_x(ap, xpts[i]), ps_grf_y(ap, ypts[i]));
      ps_write(pbuf);
    }
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " stroke\n");
  ps_write(pbuf);
}

static void ps_draw_dots(axis_info *ap, int j, Float *xpts, Float *ypts, int dot_size)
{
  int i;
  Float arc_size;
  arc_size = .5 * dot_size; /* radius here, diameter in X */
  for (i = 0; i < j; i++)
    {
      mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f %.2f 0 360 NAF\n", ps_grf_x(ap, xpts[i]), ps_grf_y(ap, ypts[i]), arc_size);
      ps_write(pbuf);
    }
}

static void ps_fill_polygons(axis_info *ap, int j, Float *xpts, Float *ypts, Float y0)
{
  int i;
  for (i = 1; i < j; i++)
    {
      mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f moveto\n", ps_grf_x(ap, xpts[i - 1]), ps_grf_y(ap, ypts[i - 1]));
      ps_write(pbuf);
      mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f lineto\n", ps_grf_x(ap, xpts[i]), ps_grf_y(ap, ypts[i]));
      ps_write(pbuf);
      mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f lineto\n", ps_grf_x(ap, xpts[i]), ps_grf_y(ap, y0));
      ps_write(pbuf);
      mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f lineto\n", ps_grf_x(ap, xpts[i - 1]), ps_grf_y(ap, y0));
      ps_write(pbuf);
      mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " closepath fill\n");
      ps_write(pbuf);
    }
}

void ps_draw_grf_points(axis_info *ap, int j, Float y0, int graph_style, int dot_size) 
{
  int i, gy0, size8, size4;
  switch (graph_style)
    {
    case GRAPH_LINES:
      ps_draw_lines(ap, j, xpts, ypts);
      break;
    case GRAPH_DOTS:
      ps_draw_dots(ap, j, xpts, ypts, dot_size);
      break;
    case GRAPH_FILLED:
      ps_fill_polygons(ap, j, xpts, ypts, y0);
      break;
    case GRAPH_DOTS_AND_LINES:
      ps_draw_lines(ap, j, xpts, ypts);
      if (dot_size > 1) ps_draw_dots(ap, j, xpts, ypts, dot_size);
      break;
    case GRAPH_LOLLIPOPS:
      if (dot_size > 1) ps_draw_dots(ap, j, xpts, ypts, dot_size);
      gy0 = (int)ps_grf_y(ap, y0);
      size8 = dot_size / 8;
      size4 = dot_size / 4;
      if (size4 < 1) size4 = 1;
      for (i = 0; i < j; i++)
	{
	  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f %.2f %.2f RF\n",
		       ps_grf_x(ap, xpts[i]) - size8,
		       (float)gy0,
		       (float)size4,
		       ps_grf_y(ap, ypts[i]) - gy0);
	  ps_write(pbuf);
	}
      break;
    }
}

void ps_draw_both_grf_points(axis_info *ap, int j, int graph_style, int dot_size) 
{
  int i, size8, size4;
  switch (graph_style)
    {
    case GRAPH_LINES:
      ps_draw_lines(ap, j, xpts, ypts);
      ps_draw_lines(ap, j, xpts, ypts1);
      break;
    case GRAPH_DOTS:
      ps_draw_dots(ap, j, xpts, ypts, dot_size);
      ps_draw_dots(ap, j, xpts, ypts1, dot_size);
      break;
    case GRAPH_FILLED:
      for (i = 1; i < j; i++)
	{
	  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f moveto\n", ps_grf_x(ap, xpts[i - 1]), ps_grf_y(ap, ypts[i - 1]));
	  ps_write(pbuf);
	  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f lineto\n", ps_grf_x(ap, xpts[i]), ps_grf_y(ap, ypts[i]));
	  ps_write(pbuf);
	  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f lineto\n", ps_grf_x(ap, xpts[i]), ps_grf_y(ap, ypts1[i]));
	  ps_write(pbuf);
	  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f lineto\n", ps_grf_x(ap, xpts[i - 1]), ps_grf_y(ap, ypts1[i - 1]));
	  ps_write(pbuf);
	  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " closepath fill\n");
	  ps_write(pbuf);
	}
      break;
    case GRAPH_DOTS_AND_LINES:
      if (dot_size > 1)
	{
	  ps_draw_dots(ap, j, xpts, ypts, dot_size);
	  ps_draw_dots(ap, j, xpts, ypts1, dot_size);
	}
      ps_draw_lines(ap, j, xpts, ypts);
      ps_draw_lines(ap, j, xpts, ypts1);
      break;
    case GRAPH_LOLLIPOPS:
      if (dot_size > 1)
	{
	  ps_draw_dots(ap, j, xpts, ypts, dot_size);
	  ps_draw_dots(ap, j, xpts, ypts1, dot_size);
	}
      size8 = dot_size / 8;
      size4 = dot_size / 4;
      if (size4 < 1) size4 = 1;
      for (i = 0; i < j; i++)
	{
	  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f %.2f %.2f RF\n",
		       ps_grf_x(ap, xpts[i]) - size8,
		       ps_grf_y(ap, ypts[i]),
		       (float)size4,
		       ps_grf_y(ap, ypts1[i]) - ps_grf_y(ap, ypts[i]));
	  ps_write(pbuf);
	}

      break;
    }
}

static int last_color = -1;

void ps_draw_sono_rectangle(axis_info *ap, int color, Float x, Float y, Float width, Float height)
{
  snd_state *ss;
  unsigned short r, g, b;
  ss = get_global_state();
  if (last_color != color)
    {
      get_current_color(color_map(ss), color, &r, &g, &b);
      last_color = color;
      mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f %.2f RG\n", (float)r / 65535.0, (float)g / 65535.0, (float)b / 65535.0);
      ps_write(pbuf);
    }
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.1f %.1f %.2f %.2f RF\n", ps_grf_x(ap, x) + 2, ps_grf_y(ap, y), width, height);
  ps_write(pbuf);
}

void ps_reset_color(void)
{
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " 0 setgray\n");
  ps_write(pbuf);
  last_color = -1;
}

static void ps_set_color(color_t color)
{
#if USE_MOTIF
  Colormap cmap;
  XColor tmp_color;
  Display *dpy;
  snd_state *ss;
  ss = get_global_state();
  dpy = XtDisplay(MAIN_SHELL(ss));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  tmp_color.pixel = color;
  XQueryColor(dpy, cmap, &tmp_color);
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f %.2f RG\n",
	       (float)tmp_color.red / 65535.0,
	       (float)tmp_color.green / 65535.0,
	       (float)tmp_color.blue / 65535.0);
  ps_write(pbuf);
#else
  #if USE_GTK
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f %.2f RG\n", 
	       (float)color->red / 65535.0, 
	       (float)color->green / 65535.0, 
	       (float)color->blue / 65535.0);
  ps_write(pbuf);
  #endif
#endif
  last_color = -1;
}

void ps_bg(axis_info *ap, axis_context *ax)
{
  /* get background color, fill graph, then set foreground for axis */
  ps_set_color(get_background_color(ap->cp, ax));
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %d %d %d %d RF\n",
	       ap->graph_x0 + bx0, ap->y_offset + by0, ap->width, ap->height);
  ps_write(pbuf);
  ps_fg(ap, ax);
}

void ps_fg(axis_info *ap, axis_context *ax)
{
  /* set foreground color for subsequent line drawing */
  ps_set_color(get_foreground_color(ap->cp, ax));
}

/* the rest are in real coordinates except upsidedown from PS point of view */
void ps_draw_line (axis_info *ap, int x0, int y0, int x1, int y1) 
{
  int py0, py1, px0, px1;
  px0 = x0 + bx0;
  px1 = x1 + bx0;
  py0 = reflect_y(ap, y0) + by0;
  py1 = reflect_y(ap, y1) + by0;
  if (px0 > bbx) bbx = px0;
  if (px1 > bbx) bbx = px1;
  if (py0 > bby) bby = py0;
  if (py1 > bby) bby = py1;
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " 0 setlinewidth %d %d moveto %d %d lineto stroke\n", px0, py0, px1, py1);
  ps_write(pbuf);
}

void ps_draw_spectro_line(axis_info *ap, int color, Float x0, Float y0, Float x1, Float y1)
{
  /* these are in local coords */
  snd_state *ss;
  unsigned short r, g, b;
  ss = get_global_state();
  if (last_color != color)
    {
      get_current_color(color_map(ss), color, &r, &g, &b);
      last_color = color;
      mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f %.2f RG\n", (float)r / 65535.0, (float)g / 65535.0, (float)b / 65535.0);
      ps_write(pbuf);
    }
  ps_draw_line(ap, (int)x0, (int)y0, (int)x1, (int)y1);
}

void ps_fill_rectangle (axis_info *ap, int x0, int y0, int width, int height) 
{
  int py0, py1, px0, px1;
  px0 = x0 + bx0;
  px1 = x0 + bx0 + width;
  py0 = reflect_y(ap, y0) + by0;
  py1 = reflect_y(ap, y0 + height) + by0;
  if (px0 > bbx) bbx = px0;
  if (px1 > bbx) bbx = px1;
  if (py0 > bby) bby = py0;
  if (py1 > bby) bby = py1;
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %d %d %d %d RF\n", px0, py0, width, -height);
  ps_write(pbuf);
}

void ps_draw_string (axis_info *ap, int x0, int y0, char *str) 
{
  int px0, py0;
  px0 = x0 + bx0;
  py0 = reflect_y(ap, y0) + by0;
  if (px0 > bbx) bbx = px0;
  if (py0 > bby) bby = py0;
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %d %d moveto (%s) show\n", px0, py0, str);
  ps_write(pbuf);
}

void ps_set_number_font(void) 
{
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " /Courier findfont 15 scalefont setfont\n");
  ps_write(pbuf);
}

void ps_set_label_font(void) 
{
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " /Times-Roman findfont 20 scalefont setfont\n");
  ps_write(pbuf);
}

void ps_set_bold_peak_numbers_font(void) 
{
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " /Times-Bold findfont 14 scalefont setfont\n");
  ps_write(pbuf);
}

void ps_set_peak_numbers_font(void) 
{
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " /Times-Roman findfont 14 scalefont setfont\n");
  ps_write(pbuf);
}

void ps_set_tiny_numbers_font(void) 
{
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " /Times-Roman findfont 12 scalefont setfont\n");
  ps_write(pbuf);
}


#define PRINTED_VERTICAL_SPACING 25 

static char *snd_print_or_error(snd_state *ss, char *output)
{
  int j, i, err;
  int *offsets = NULL;
  snd_info *sp;
  sync_info *si;
  chan_info *ccp;
  char *errstr = NULL;
  if ((output) && (*output))
    {
      ccp = current_channel(ss);
      if (ccp == NULL) 
	return(copy_string(_("nothing to print?")));
      si = sync_to_chan(ccp);
      offsets = (int *)CALLOC(si->chans, sizeof(int));
      for (j = 0, i = (si->chans - 1); i >= 0; i--)
	{
	  offsets[i] = j;
	  j += ((((axis_info *)((si->cps[i])->axis))->height) + PRINTED_VERTICAL_SPACING);
	}
      if (si->chans > 1)
	for (i = 0; i < si->chans; )
	  {
	    sp = (si->cps[i])->sound;
	    if (sp == NULL) break;
	    if (sp->channel_style == CHANNELS_COMBINED)
	      for (j = i + 1; (j < i + sp->nchans) && (j < si->chans); j++) 
		offsets[j] = offsets[i];
	    else
	      if (sp->channel_style == CHANNELS_SUPERIMPOSED)
		for (j = i; (j < i + sp->nchans - 1) && (j < si->chans); j++) 
		  offsets[j] = offsets[i + sp->nchans - 1];
	    i += sp->nchans;
	  }
      err = start_ps_graph(output, ((si->cps[0])->sound)->filename);
      if (err == 0)
	{
	  for (i = 0; i < si->chans; i++)
	    ps_graph(si->cps[i], 0, offsets[i]);
	  end_ps_graph();
	}
      else errstr = mus_format(_("print %s failed: %s"), output, strerror(errno));
      if (si) si = free_sync_info(si);
      if (offsets) FREE(offsets);
      return(errstr);
    }
  else return(copy_string(_("print sound: eps file name needed")));
}

void snd_print(snd_state *ss, char *output)
{
  char *error;
  error = snd_print_or_error(ss,output);
  if (error)
    {
      snd_error(error);
      FREE(error);
    }
}

void region_print(char *output, char* title, chan_info *cp)
{
  int err;
  if ((output) && (*output))
    {
      err = start_ps_graph(output, title);
      if (err == 0)
	{
	  ps_graph(cp, 0, 0);
	  end_ps_graph();
	}
      else snd_error(_("print region %s failed: %s"), output, strerror(errno));
    }
  else snd_error(_("print region: eps file name needed"));
}

void print_enved(char *output, int y0)
{
  int err;
  if ((output) && (*output))
    {
      err = start_ps_graph(output, "Envelope Editor");
      if (err == 0)
	{
	  bx0 = 0;
	  by0 = y0;
	  env_redisplay_with_print(get_global_state());
	  end_ps_graph();
	}
      else snd_error(_("print env %s failed: %s"), output, strerror(errno));
    }
  else snd_error(_("print envelope: eps file name needed"));
}

static XEN g_graph2ps(XEN filename)
{
  #define H_graph2ps "(" S_graph2ps " &optional filename) writes the current Snd displays to an EPS file"

  char *error,*file;
  XEN result;
  snd_state *ss;
  ss = get_global_state();
  if (XEN_STRING_P(filename))
    file = XEN_TO_C_STRING(filename);
  else file = eps_file(ss);
  error = snd_print_or_error(ss, file);
  if (error)
    {
      result = C_TO_XEN_STRING(error);
      FREE(error);
      XEN_ERROR(CANNOT_PRINT,
		XEN_LIST_3(C_TO_XEN_STRING(S_graph2ps),
			   C_TO_XEN_STRING(file),
			   result));
    }
  return(C_TO_XEN_STRING(file));
}

static XEN g_eps_file(void) {return(C_TO_XEN_STRING(eps_file(get_global_state())));}
static XEN g_set_eps_file(XEN val) 
{
  #define H_eps_file "(" S_eps_file ") -> current eps ('Print' command) file name (snd.eps)"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_eps_file, "a string"); 
  if (eps_file(ss)) FREE(eps_file(ss));
  set_eps_file(ss, copy_string(XEN_TO_C_STRING(val))); 
  return(C_TO_XEN_STRING(eps_file(ss)));
}

static XEN g_eps_left_margin(void) {return(C_TO_XEN_DOUBLE(eps_left_margin(get_global_state())));}
static XEN g_set_eps_left_margin(XEN val) 
{
  #define H_eps_left_margin "(" S_eps_left_margin ") -> current eps ('Print' command) left margin"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_eps_left_margin, "a number"); 
  set_eps_left_margin(ss, XEN_TO_C_DOUBLE(val));
  return(C_TO_XEN_DOUBLE(eps_left_margin(ss)));
}

static XEN g_eps_bottom_margin(void) {return(C_TO_XEN_DOUBLE(eps_bottom_margin(get_global_state())));}
static XEN g_set_eps_bottom_margin(XEN val) 
{
  #define H_eps_bottom_margin "(" S_eps_bottom_margin ") -> current eps ('Print' command) bottom margin"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_eps_bottom_margin, "a number"); 
  set_eps_bottom_margin(ss, XEN_TO_C_DOUBLE(val));
  return(C_TO_XEN_DOUBLE(eps_bottom_margin(ss)));
}

static XEN g_eps_size(void) {return(C_TO_XEN_DOUBLE(eps_size(get_global_state())));}
static XEN g_set_eps_size(XEN val) 
{
  #define H_eps_size "(" S_eps_size ") -> current eps ('Print' command) overall picture size scaler (1.0)"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_eps_size, "a number"); 
  set_eps_size(ss, XEN_TO_C_DOUBLE(val));
  return(C_TO_XEN_DOUBLE(eps_size(ss)));
}


#ifdef XEN_ARGIFY_1
XEN_ARGIFY_1(g_graph2ps_w, g_graph2ps)
XEN_NARGIFY_0(g_eps_file_w, g_eps_file)
XEN_NARGIFY_1(g_set_eps_file_w, g_set_eps_file)
XEN_NARGIFY_0(g_eps_left_margin_w, g_eps_left_margin)
XEN_ARGIFY_1(g_set_eps_left_margin_w, g_set_eps_left_margin)
XEN_NARGIFY_0(g_eps_size_w, g_eps_size)
XEN_ARGIFY_1(g_set_eps_size_w, g_set_eps_size)
XEN_NARGIFY_0(g_eps_bottom_margin_w, g_eps_bottom_margin)
XEN_ARGIFY_1(g_set_eps_bottom_margin_w, g_set_eps_bottom_margin)
#else
#define g_graph2ps_w g_graph2ps
#define g_eps_file_w g_eps_file
#define g_set_eps_file_w g_set_eps_file
#define g_eps_left_margin_w g_eps_left_margin
#define g_set_eps_left_margin_w g_set_eps_left_margin
#define g_eps_size_w g_eps_size
#define g_set_eps_size_w g_set_eps_size
#define g_eps_bottom_margin_w g_eps_bottom_margin
#define g_set_eps_bottom_margin_w g_set_eps_bottom_margin
#endif

void g_init_print(void)
{
  XEN_DEFINE_PROCEDURE(S_graph2ps, g_graph2ps_w, 0, 1, 0, H_graph2ps);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_eps_file, g_eps_file_w, H_eps_file,
				   "set-" S_eps_file, g_set_eps_file_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_eps_left_margin, g_eps_left_margin_w, H_eps_left_margin,
				   "set-" S_eps_left_margin, g_set_eps_left_margin_w,  0, 0, 0, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_eps_bottom_margin, g_eps_bottom_margin_w, H_eps_bottom_margin,
				   "set-" S_eps_bottom_margin, g_set_eps_bottom_margin_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_eps_size, g_eps_size_w, H_eps_size,
				   "set-" S_eps_size, g_set_eps_size_w,  0, 0, 0, 1);
}
