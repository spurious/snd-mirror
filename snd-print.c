#include "snd.h"

/* create Postscript version of graph */

static char *pbuf = NULL;
static int bbx, bby, bx0, by0;
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

static void ps_write(int fd, char *buf)
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
      if (nbuf_ctr == NBUF_SIZE) ps_flush(fd);
    }
}

static int start_ps_graph(char *output, char *title) 
{ 
  time_t ts;
  snd_state *ss;
  ps_fd = creat(output, 0666);
  if (ps_fd == -1) return(-1);
  if (!pbuf) pbuf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  bbx = 0;
  bby = 0;
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, "%%!PS-Adobe-2.0 EPSF-2.0\n%%%%Title: %s\n%%%%Creator: Snd: %s\n%%%%CreationDate: ", title, SND_VERSION);
  ps_write(ps_fd, pbuf);
#if HAVE_STRFTIME
  time(&ts);
  strftime(pbuf, PRINT_BUFFER_SIZE, STRFTIME_FORMAT, localtime(&ts));
  ps_write(ps_fd, pbuf);
#endif
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, "\n%%%%BoundingBox:(atend)\n%%%%EndComments\n%%%%EndProlog\n%%%%Page: 1 1\n");
  ps_write(ps_fd, pbuf);
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, "/LT {lineto} bind def\n/RF {rectfill} bind def\n/RG {setrgbcolor} bind def\n/NAF {newpath arc fill} bind def\n\n");
  ps_write(ps_fd, pbuf);
  ss = get_global_state();
  if ((eps_left_margin(ss) != 0) || (eps_bottom_margin(ss) != 0))
    {
      mus_snprintf(pbuf, PRINT_BUFFER_SIZE, "gsave [1.00 0.00 0.00 1.00 %.3f %.3f] concat\n\n",
	      eps_left_margin(ss), eps_bottom_margin(ss));
      ps_write(ps_fd, pbuf);
    }
  return(0);
}

static void ps_graph(chan_info *cp, int x0, int y0)
{
  cp->printing = 1;
  bx0 = x0;
  by0 = y0;
  display_channel_data(cp, cp->sound, cp->state);
  cp->printing = 0;
}

static void end_ps_graph(void)
{
  snd_state *ss;
  ss = get_global_state();
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, "%s\nshowpage\n%%%%Trailer\n%%%%BoundingBox: %d %d %d %d\n",
	       ((eps_left_margin(ss) != 0) || (eps_bottom_margin(ss) != 0)) ? "\ngrestore" : "",
	       0, 0,
	       (int)(bbx + 10 + eps_left_margin(ss)),
	       (int)(bby + 10 + eps_bottom_margin(ss)));
  ps_write(ps_fd, pbuf);
  ps_flush(ps_fd);
  close(ps_fd);
}

/* the x and y values in the "points" are relative to grf_x/y:
 *
 *  x: ap->x_axis_x0 + (val - ap->x0) * ap->x_scale
 *  y: ap->y_axis_y0 + (val * MUS_FIX_TO_FLOAT - ap->y0) * ap->y_scale
 *
 * kept here in full precision since normally printers have much higher resolution than screens 
 */

static int reflect_y(chan_info *cp, int y)
{
  return(((axis_info *)(cp->axis))->height - y);
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

static void ps_draw_lines(chan_info *cp, axis_info *ap, int j, Float *xpts, Float *ypts)
{
  int i;
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f moveto\n", ps_grf_x(ap, xpts[0]), ps_grf_y(ap, ypts[0]));
  ps_write(ps_fd, pbuf);
  for (i = 1; i < j; i++)
    {
      mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f lineto\n", ps_grf_x(ap, xpts[i]), ps_grf_y(ap, ypts[i]));
      ps_write(ps_fd, pbuf);
    }
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " stroke\n");
  ps_write(ps_fd, pbuf);
}

static void ps_draw_dots(chan_info *cp, axis_info *ap, int j, Float *xpts, Float *ypts)
{
  int i;
  Float arc_size;
  arc_size = .5 * cp->dot_size; /* radius here, diameter in X */
  for (i = 0; i < j; i++)
    {
      mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f %.2f 0 360 NAF\n", ps_grf_x(ap, xpts[i]), ps_grf_y(ap, ypts[i]), arc_size);
      ps_write(ps_fd, pbuf);
    }
}

static void ps_fill_polygons(chan_info *cp, axis_info *ap, int j, Float *xpts, Float *ypts, Float y0)
{
  int i;
  for (i = 1; i < j; i++)
    {
      mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f moveto\n", ps_grf_x(ap, xpts[i - 1]), ps_grf_y(ap, ypts[i - 1]));
      ps_write(ps_fd, pbuf);
      mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f lineto\n", ps_grf_x(ap, xpts[i]), ps_grf_y(ap, ypts[i]));
      ps_write(ps_fd, pbuf);
      mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f lineto\n", ps_grf_x(ap, xpts[i]), ps_grf_y(ap, y0));
      ps_write(ps_fd, pbuf);
      mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f lineto\n", ps_grf_x(ap, xpts[i - 1]), ps_grf_y(ap, y0));
      ps_write(ps_fd, pbuf);
      mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " closepath fill\n");
      ps_write(ps_fd, pbuf);
    }
}

void ps_draw_grf_points(chan_info *cp, axis_info *ap, int j, Float y0, int graph_style) 
{
  int i, gy0, size8, size4;
  switch (graph_style)
    {
    case GRAPH_LINES:
      ps_draw_lines(cp, ap, j, xpts, ypts);
      break;
    case GRAPH_DOTS:
      ps_draw_dots(cp, ap, j, xpts, ypts);
      break;
    case GRAPH_FILLED:
      ps_fill_polygons(cp, ap, j, xpts, ypts, y0);
      break;
    case GRAPH_DOTS_AND_LINES:
      ps_draw_lines(cp, ap, j, xpts, ypts);
      if (cp->dot_size > 1) ps_draw_dots(cp, ap, j, xpts, ypts);
      break;
    case GRAPH_LOLLIPOPS:
      if (cp->dot_size > 1) ps_draw_dots(cp, ap, j, xpts, ypts);
      gy0 = (int)ps_grf_y(ap, y0);
      size8 = cp->dot_size / 8;
      size4 = cp->dot_size / 4;
      if (size4 < 1) size4 = 1;
      for (i = 0; i < j; i++)
	{
	  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f %.2f %.2f RF\n",
		       ps_grf_x(ap, xpts[i]) - size8,
		       (float)gy0,
		       (float)size4,
		       ps_grf_y(ap, ypts[i]) - gy0);
	  ps_write(ps_fd, pbuf);
	}
      break;
    }
}

void ps_draw_both_grf_points(chan_info *cp, axis_info *ap, int j, int graph_style) 
{
  int i, size8, size4;
  switch (graph_style)
    {
    case GRAPH_LINES:
      ps_draw_lines(cp, ap, j, xpts, ypts);
      ps_draw_lines(cp, ap, j, xpts, ypts1);
      break;
    case GRAPH_DOTS:
      ps_draw_dots(cp, ap, j, xpts, ypts);
      ps_draw_dots(cp, ap, j, xpts, ypts1);
      break;
    case GRAPH_FILLED:
      for (i = 1; i < j; i++)
	{
	  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f moveto\n", ps_grf_x(ap, xpts[i - 1]), ps_grf_y(ap, ypts[i - 1]));
	  ps_write(ps_fd, pbuf);
	  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f lineto\n", ps_grf_x(ap, xpts[i]), ps_grf_y(ap, ypts[i]));
	  ps_write(ps_fd, pbuf);
	  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f lineto\n", ps_grf_x(ap, xpts[i]), ps_grf_y(ap, ypts1[i]));
	  ps_write(ps_fd, pbuf);
	  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f lineto\n", ps_grf_x(ap, xpts[i - 1]), ps_grf_y(ap, ypts1[i - 1]));
	  ps_write(ps_fd, pbuf);
	  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " closepath fill\n");
	  ps_write(ps_fd, pbuf);
	}
      break;
    case GRAPH_DOTS_AND_LINES:
      if (cp->dot_size > 1)
	{
	  ps_draw_dots(cp, ap, j, xpts, ypts);
	  ps_draw_dots(cp, ap, j, xpts, ypts1);
	}
      ps_draw_lines(cp, ap, j, xpts, ypts);
      ps_draw_lines(cp, ap, j, xpts, ypts1);
      break;
    case GRAPH_LOLLIPOPS:
      if (cp->dot_size > 1)
	{
	  ps_draw_dots(cp, ap, j, xpts, ypts);
	  ps_draw_dots(cp, ap, j, xpts, ypts1);
	}
      size8 = cp->dot_size / 8;
      size4 = cp->dot_size / 4;
      if (size4 < 1) size4 = 1;
      for (i = 0; i < j; i++)
	{
	  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f %.2f %.2f RF\n",
		       ps_grf_x(ap, xpts[i]) - size8,
		       ps_grf_y(ap, ypts[i]),
		       (float)size4,
		       ps_grf_y(ap, ypts1[i]) - ps_grf_y(ap, ypts[i]));
	  ps_write(ps_fd, pbuf);
	}

      break;
    }
}

static int last_color = -1;

void ps_draw_sono_rectangle(chan_info *cp, axis_info *ap, int color, Float x, Float y, Float width, Float height)
{
  snd_state *ss;
  unsigned short r, g, b;
  ss = cp->state;
  if (last_color != color)
    {
      get_current_color(color_map(ss), color, &r, &g, &b);
      last_color = color;
      mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f %.2f RG\n", (float)r / 65535.0, (float)g / 65535.0, (float)b / 65535.0);
      ps_write(ps_fd, pbuf);
    }
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.1f %.1f %.2f %.2f RF\n", ps_grf_x(ap, x), ps_grf_y(ap, y), width, height);
  ps_write(ps_fd, pbuf);
}

void ps_reset_color(chan_info *cp)
{
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " 0 setgray\n");
  ps_write(ps_fd, pbuf);
  last_color = -1;
}

void ps_recolor(chan_info *cp)
{
  char *rgb;
  rgb = ps_rgb(cp->state, cp->chan % 4);
  ps_write(ps_fd, rgb);
  FREE(rgb);
}

/* the rest are in real coordinates except upsidedown from PS point of view */
void ps_draw_line (chan_info *cp, int x0, int y0, int x1, int y1) 
{
  int py0, py1, px0, px1;
  px0 = x0 + bx0;
  px1 = x1 + bx0;
  py0 = reflect_y(cp, y0) + by0;
  py1 = reflect_y(cp, y1) + by0;
  if (px0 > bbx) bbx = px0;
  if (px1 > bbx) bbx = px1;
  if (py0 > bby) bby = py0;
  if (py1 > bby) bby = py1;
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " 0 setlinewidth %d %d moveto %d %d lineto stroke\n", px0, py0, px1, py1);
  ps_write(ps_fd, pbuf);
}

void ps_draw_spectro_line(chan_info *cp, int color, Float x0, Float y0, Float x1, Float y1)
{
  /* these are in local coords */
  snd_state *ss;
  unsigned short r, g, b;
  ss = cp->state;
  if (last_color != color)
    {
      get_current_color(color_map(ss), color, &r, &g, &b);
      last_color = color;
      mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %.2f %.2f %.2f RG\n", (float)r / 65535.0, (float)g / 65535.0, (float)b / 65535.0);
      ps_write(ps_fd, pbuf);
    }
  ps_draw_line(cp, (int)x0, (int)y0, (int)x1, (int)y1);
}

void ps_fill_rectangle (chan_info *cp, int x0, int y0, int width, int height) 
{
  int py0, py1, px0, px1;
  px0 = x0 + bx0;
  px1 = x0 + bx0 + width;
  py0 = reflect_y(cp, y0) + by0;
  py1 = reflect_y(cp, y0 + height) + by0;
  if (px0 > bbx) bbx = px0;
  if (px1 > bbx) bbx = px1;
  if (py0 > bby) bby = py0;
  if (py1 > bby) bby = py1;
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %d %d %d %d RF\n", px0, py0, width, -height);
  ps_write(ps_fd, pbuf);
}

void ps_draw_string (chan_info *cp, int x0, int y0, char *str) 
{
  int px0, py0;
  px0 = x0 + bx0;
  py0 = reflect_y(cp, y0) + by0;
  if (px0 > bbx) bbx = px0;
  if (py0 > bby) bby = py0;
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " %d %d moveto (%s) show\n", px0, py0, str);
  ps_write(ps_fd, pbuf);
}

void ps_set_number_font(chan_info *cp) 
{
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " /Courier findfont 15 scalefont setfont\n");
  ps_write(ps_fd, pbuf);
}

void ps_set_label_font(chan_info *cp) 
{
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " /Times-Roman findfont 20 scalefont setfont\n");
  ps_write(ps_fd, pbuf);
}

void ps_set_bold_peak_numbers_font(chan_info *cp) 
{
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " /Times-Bold findfont 14 scalefont setfont\n");
  ps_write(ps_fd, pbuf);
}

void ps_set_peak_numbers_font(chan_info *cp) 
{
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " /Times-Roman findfont 14 scalefont setfont\n");
  ps_write(ps_fd, pbuf);
}

void ps_set_tiny_numbers_font(chan_info *cp) 
{
  mus_snprintf(pbuf, PRINT_BUFFER_SIZE, " /Times-Roman findfont 12 scalefont setfont\n");
  ps_write(ps_fd, pbuf);
}


#define PRINTED_VERTICAL_SPACING 25 

static char *snd_print_or_error(snd_state *ss, char *output)
{
  int j, i, err;
  int *offsets = NULL;
  snd_info *sp;
  sync_info *si;
  chan_info *ccp;
  if ((output) && (*output))
    {
      ccp = current_channel(ss);
      if (ccp == NULL) 
	return(copy_string("nothing to print?"));
      si = sync_to_chan(ccp);
      if ((si == NULL) || (si->chans == 0))
	return(copy_string("no graphs to print?"));
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
      else return(mus_format("print %s failed: %s", output, strerror(errno)));
      si = free_sync_info(si);
      if (offsets) FREE(offsets);
      return(NULL);
    }
  else return(copy_string("print sound: eps file name needed"));
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
      else snd_error("print region %s failed: %s", output, strerror(errno));
    }
  else snd_error("print region: eps file name needed");
}

void print_enved(char *output, chan_info *cp, int y0)
{
  int err;
  if ((output) && (*output))
    {
      err = start_ps_graph(output, "Envelope Editor");
      if (err == 0)
	{
	  cp->printing = 1;
	  bx0 = 0;
	  by0 = y0;
	  env_redisplay(cp->state);
	  cp->printing = 0;
	  end_ps_graph();
	}
      else snd_error("print env %s failed: %s", output, strerror(errno));
    }
  else snd_error("print envelope: eps file name needed");
}

static SCM g_graph2ps(SCM filename)
{
  #define H_graph2ps "(" S_graph2ps " &optional filename) writes the current Snd displays to an EPS file"

  char *error,*file;
  SCM result;
  snd_state *ss;
  ss = get_global_state();
  if (STRING_P(filename))
    file = TO_C_STRING(filename);
  else file = eps_file(ss);
  error = snd_print_or_error(ss, file);
  if (error)
    {
      result = TO_SCM_STRING(error);
      FREE(error);
      ERROR(CANNOT_PRINT,
	    SCM_LIST3(TO_SCM_STRING(S_graph2ps),
		      TO_SCM_STRING(file),
		      result));
    }
  return(TO_SCM_STRING(file));
}

void g_init_print(SCM local_doc)
{
  DEFINE_PROC(S_graph2ps, g_graph2ps, 0, 1, 0, H_graph2ps);
}
