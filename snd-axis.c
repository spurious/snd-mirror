#include "snd.h"

/* changed to use doubles here 14-Nov-00: very small windows appear to get arithmetic problems otherwise */
/* removed y-label support 18-Dec-00 */

axis_context *free_axis_context(axis_context *ax)
{
  if (ax) FREE(ax);
  return(NULL);
}

typedef struct {
  double hi, lo; 
  int max_ticks;
  double flo, fhi, mlo, mhi, step, tenstep;
  int tens, min_label_width, max_label_width;
  Locus min_label_x, max_label_x;
  Latus maj_tick_len, min_tick_len;
  char *min_label, *max_label;
} tick_descriptor;

static tick_descriptor *free_tick_descriptor (tick_descriptor *td)
{
  if (td)
    {
      if (td->min_label) {FREE(td->min_label); td->min_label = NULL;}
      if (td->max_label) {FREE(td->max_label); td->max_label = NULL;}
      FREE(td);
    }
  return(NULL);
}

static tick_descriptor *describe_ticks(tick_descriptor *gd_td, double lo, double hi, int max_ticks)
{
  /* given absolute (unchangeable) axis bounds lo and hi, and abolute maximum number of ticks to use, find a "pretty" tick placement */
  /* much of the work here involves floating point rounding problems.  We assume the tick labeller will round as well */
  tick_descriptor *td;
  int ten, hib, lob, offset = 0;
  double flog10, plog10;
  double frac, ften, hilo_diff, eten, flt_ten, flt_ften;
  double inside, mfdiv, mten, mften;
  int mticks, mdiv;

  if (!gd_td)
    td = (tick_descriptor *)CALLOC(1, sizeof(tick_descriptor));
  else 
    {
      td = gd_td;
      if ((td->hi == hi) && 
	  (td->lo == lo) && 
	  (td->max_ticks == max_ticks)) 
	return(td);
    }
  td->hi = hi;
  td->lo = lo;
  hilo_diff = hi - lo;
  if (hilo_diff < .001) 
    {
      offset = (int)hi;
      hi -= offset;
      lo -= offset;
    }
  td->max_ticks = max_ticks;
  flt_ten = log10(hilo_diff);
  ten = (int)floor(flt_ten);
  frac = flt_ten - ten;
  if (frac > .9999) ten++;
  eten = pow(10, ten);
  hib = (int)floor(hi / eten);
  lob = (int)ceil(lo / eten);
  /* it's possible to wrap-around here and get negative numbers (so we keep the offset separate above) */
  if (lob != hib) 
    {
      td->mlo = (double)(lob * eten);
      td->mhi = (double)(hib * eten);
    }
  else
    {
      /* try next lower power of ten */
      ften = eten * .1;
      flt_ften = (hi / ften);
      hib = (int)floor(flt_ften);
      frac = flt_ften - hib;
      if (frac > .9999) hib++;
      lob = (int)ceil(lo / ften);
      td->mlo = (double)(lob * ften);
      td->mhi = (double)(hib * ften);
    }
  inside = (td->mhi - td->mlo) / hilo_diff;
  mticks = (int)floor(inside * max_ticks);
  if (mticks <= 1) mdiv = 1;
  if (mticks < 3) mdiv = mticks;
  else if (mticks == 3) mdiv = 2;
  else if (mticks < 6) mdiv = mticks;
  else if (mticks < 10) mdiv = 5;
  else mdiv = (int)(10 * floor(mticks / 10));
  mfdiv = (td->mhi - td->mlo) / mdiv;
  flog10 = floor(log10(mfdiv));
  plog10 = pow(10, flog10);
  td->tens = (int)fabs(flog10);
  mten = (double)(floor(4.0 * (.00001 + (mfdiv / plog10)))) / 4.0;
  if (mten < 1.0) mten = 1.0;
  if ((mten == 1.0) || (mten == 2.0) || (mten == 2.5) || (mten == 5.0)) ften = mten;
  else if (mten < 2.0) ften = 2.0;
  else if (mten < 2.5) ften = 2.5;
  else if (mten < 5.0) ften = 5.0;
  else ften = 10.0;
  td->tenstep = ften;
  mften = ften * plog10;
  td->step = mften;
  flt_ten = lo / mften;
  lob = (int)ceil(flt_ten);
  frac = lob - flt_ten;
  if (frac > .9999) lob--;
  td->flo = lob * mften;
  flt_ten = (hi / mften);
  hib = (int)floor(flt_ten);
  frac = flt_ten - hib;
  if (frac > .9999) hib++;
  td->fhi = hib * mften;
  if (hilo_diff < .001) 
    {
      td->mlo += offset;
      td->mhi += offset;
      td->flo += offset;
      td->fhi += offset;
    }
  return(td);
}

axis_info *free_axis_info(axis_info *ap)
{
  if (ap->x_ticks) ap->x_ticks = (void *)free_tick_descriptor((tick_descriptor *)(ap->x_ticks));
  if (ap->y_ticks) ap->y_ticks = (void *)free_tick_descriptor((tick_descriptor *)(ap->y_ticks));
  if (ap->ax) ap->ax = free_axis_context(ap->ax);
  if (ap->xlabel) 
    {
      FREE(ap->xlabel); 
      ap->xlabel = NULL;
    }
  FREE(ap);
  return(NULL);
}

Locus grf_x(double val, axis_info *ap)
{
  if (val >= ap->x1) return(ap->x_axis_x1);
  if (val <= ap->x0) return(ap->x_axis_x0);
  return((Locus)(ap->x_base + val * ap->x_scale));
}

Locus grf_y(Float val, axis_info *ap)
{
  if (val >= ap->y1) return(ap->y_axis_y1);
  if (val <= ap->y0) return(ap->y_axis_y0);
  return((Locus)(ap->y_base + val * ap->y_scale));
}

static Locus tick_grf_x(double val, axis_info *ap, int style, int srate)
{
  int res;
  switch (style)
    {
    case X_AXIS_IN_SECONDS: 
      res = (int)(ap->x_base + val * ap->x_scale); 
      break;
    case X_AXIS_IN_BEATS: 
      if (ap->cp)
	res = (int)(ap->x_base + val * ap->x_scale * 60.0 / ap->cp->beats_per_minute);
      else res = (int)(ap->x_base + val * ap->x_scale); 
      break;
    case X_AXIS_IN_SAMPLES: 
      res = (int)(ap->x_axis_x0 + (val - ap->x0 * srate) * ap->x_scale / srate); 
      break;
    case X_AXIS_AS_PERCENTAGE: 
      res = (int)(ap->x_axis_x0 + (val - ap->x0 / ap->xmax) * ap->x_scale * ap->xmax); 
      break;
    default: 
      res = (int)(ap->x_base + val * ap->x_scale);
      break;
    }
  if (res >= -32768) 
    {
      if (res < 32768) return((Locus)res);
      return(32767);
    }
  return(-32768);
}

#if HAVE_GL
static int gl_fonts_activated = FALSE;
static int label_base, number_base;
static void activate_gl_fonts(snd_state *ss)
{
#if USE_MOTIF
  XFontStruct *label, *number;
  if (!gl_fonts_activated)
    {
      label = (XFontStruct *)(AXIS_LABEL_FONT(ss));
      number = (XFontStruct *)(AXIS_NUMBERS_FONT(ss));
      label_base = glGenLists(128);
      number_base = glGenLists(128);
      glXUseXFont(label->fid, 32, 96, label_base + 32);
      glXUseXFont(number->fid, 32, 96, number_base + 32);
      gl_fonts_activated = TRUE;
    }
#else
  if (!gl_fonts_activated)
    {
      label_base = glGenLists(128);
      number_base = glGenLists(128);
#if HAVE_GDK_GL_FONT_USE_GDK_FONT
      gdk_gl_font_use_gdk_font(gdk_font_from_description(AXIS_LABEL_FONT(ss)), 32, 96, label_base + 32);
      gdk_gl_font_use_gdk_font(gdk_font_from_description(AXIS_NUMBERS_FONT(ss)), 32, 96, number_base + 32);
#else
      gdk_gl_font_use_pango_font(AXIS_LABEL_FONT(ss), 32, 96, label_base + 32);
      gdk_gl_font_use_pango_font(AXIS_NUMBERS_FONT(ss), 32, 96, number_base + 32);
#endif
      gl_fonts_activated = TRUE;
    }
#endif
}

void reload_label_font(snd_state *ss)
{
#if USE_MOTIF
  XFontStruct *label;
  if (gl_fonts_activated)
    {
      glDeleteLists(label_base, 128);
      label_base = glGenLists(128);
      label = (XFontStruct *)(AXIS_LABEL_FONT(ss));
      glXUseXFont(label->fid, 32, 96, label_base + 32);
    }
#else
  if (gl_fonts_activated)
    {
      glDeleteLists(label_base, 128);
      label_base = glGenLists(128);
#if HAVE_GDK_GL_FONT_USE_GDK_FONT
      gdk_gl_font_use_gdk_font(gdk_font_from_description(AXIS_LABEL_FONT(ss)), 32, 96, label_base + 32);
#else
      gdk_gl_font_use_pango_font(AXIS_LABEL_FONT(ss), 32, 96, label_base + 32);
#endif
    }
#endif
}

void reload_number_font(snd_state *ss)
{
#if USE_MOTIF
  XFontStruct *number;
  if (gl_fonts_activated)
    {
      glDeleteLists(number_base, 128);
      number_base = glGenLists(128);
      number = (XFontStruct *)(AXIS_NUMBERS_FONT(ss));
      glXUseXFont(number->fid, 32, 96, number_base + 32);
    }
#else
  if (gl_fonts_activated)
    {
      glDeleteLists(number_base, 128);
      number_base = glGenLists(128);
#if HAVE_GDK_GL_FONT_USE_GDK_FONT
      gdk_gl_font_use_gdk_font(gdk_font_from_description(AXIS_NUMBERS_FONT(ss)), 32, 96, number_base + 32);
#else
      gdk_gl_font_use_pango_font(AXIS_NUMBERS_FONT(ss), 32, 96, number_base + 32);
#endif
    }
#endif
}
#endif

void make_axes_1(axis_info *ap, int x_style, int srate, int axes, int printing, int show_x_axis)
{
  snd_state *ss;
  Latus width, height;
  double x_range, y_range, tens;
  Latus axis_thickness, left_border_width, bottom_border_width, top_border_width, right_border_width;
  Latus inner_border_width, tick_label_width;
  Latus major_tick_length, minor_tick_length, x_tick_spacing, y_tick_spacing;
  int include_x_label, include_x_ticks, include_x_tick_labels, include_y_ticks, include_y_tick_labels;
  Latus x_label_width, x_label_height, x_number_height;
  int num_ticks, majy, miny, majx, minx, x, y, tx, ty, x0, y0;
  double fy, fx;
  tick_descriptor *tdx = NULL, *tdy = NULL;
  Locus curx, cury, curdy;
  axis_context *ax;
#if HAVE_GL
  Float xthick, ythick, xmajorlen, xminorlen, ymajorlen, yminorlen;
#endif
  ss = get_global_state();
  ax = ap->ax;
  width = ap->width;
  height = ap->height;
  ap->graph_active = ((width > 4) || (height > 10));

  if ((axes == SHOW_NO_AXES) || (width < 40) || (height < 40))
    {
      /* ap->graph_active = 0; */
      /* leave it set up for bare graph */
      if (height > 100)
	{
	  ap->y_axis_y0 = ap->y_offset + height - 20;
	  ap->y_axis_y1 = ap->y_offset + 10;
	}
      else
	{
	  ap->y_axis_y0 = ap->y_offset + (int)(0.8 * height);
	  ap->y_axis_y1 = ap->y_offset + (int)(0.1 * height);
	}
      if (width > 100)
	{
	  ap->x_axis_x1 = ap->graph_x0 + width - 10;
	  ap->x_axis_x0 = ap->graph_x0 + 20;
	}
      else
	{
	  ap->x_axis_x1 = ap->graph_x0 + (int)(0.9 * width);
	  ap->x_axis_x0 = ap->graph_x0 + (int)(0.2 * width);
	}
      if ((ap->x_axis_x0 == ap->x_axis_x1) || (ap->x0 == ap->x1))
	ap->x_scale = 0.0;
      else ap->x_scale = ((double)(ap->x_axis_x1 - ap->x_axis_x0)) / ((double)(ap->x1 - ap->x0));
      ap->x_base = (double)(ap->x_axis_x0 - ap->x0 * ap->x_scale);

      if ((ap->y_axis_y0 == ap->y_axis_y1) || (ap->y0 == ap->y1))
	ap->y_scale = 0.0;
      else ap->y_scale = (ap->y_axis_y1 - ap->y_axis_y0) / (ap->y1 - ap->y0);
      ap->y_base = (Float)(ap->y_axis_y0 - ap->y0 * ap->y_scale);
      return;
    }

  left_border_width = 10;
  bottom_border_width = 14;
  top_border_width = 10;
  right_border_width = 14;
  inner_border_width = 5;
  x_tick_spacing = 20; 
  if (width < 250) x_tick_spacing = 10 + (width / 25);
  y_tick_spacing = 20; 
  if (height < 250) y_tick_spacing = 10 + (height / 25);
  major_tick_length = 9;
  minor_tick_length = 5;
  axis_thickness = 2;
#if HAVE_GL
  xthick = (Float)(2 * axis_thickness) / (Float)height;
  ythick = (Float)(2 * axis_thickness) / (Float)width;
  xmajorlen = (Float)(2 * major_tick_length) / (Float)height;
  xminorlen = (Float)(2 * minor_tick_length) / (Float)height;
  ymajorlen = (Float)(2 * major_tick_length) / (Float)width;
  yminorlen = (Float)(2 * minor_tick_length) / (Float)width;
#endif

  if (show_x_axis)
    {
      include_x_label = ((ap->xlabel) && ((height > 100) && (width > 100)));
      include_x_tick_labels = ((height > 60) && (width > 100));
      include_x_ticks = ((height > 40) && (width > 40));
    }
  else
    {
      include_x_label = 0;
      include_x_tick_labels = 0;
      include_x_ticks = 0;
    }
  if (axes != SHOW_X_AXIS)
    {
      include_y_tick_labels = ((width > 100) && (height > 60));
      include_y_ticks = ((width > 100) && (height > 40));
    }
  else
    {
      include_y_tick_labels = 0;
      include_y_ticks = 0;
    }

  curx = left_border_width;
  cury = height - bottom_border_width;
  
  x_number_height = number_height(ss);
  x_label_height = 0;
  x_label_width = 0;

  if (include_x_label)
    {
      x_label_width = label_width(ss, ap->xlabel);
      if ((x_label_width + curx + right_border_width) > width)
	{
	  include_x_label = 0;
	  x_label_width = 0;
	  x_label_height = 0;
	}
      else x_label_height = label_height(ss);
    }
  else
    if (include_x_ticks) 
      x_label_height = label_height(ss);

  curdy = cury;
  if (include_y_ticks)
    {
      /* figure out how long the longest tick label will be and make room for it */
      /* values go from ap->y0 to ap->y1 */
      /* basic tick spacing is tick_spacing pixels */
      num_ticks = curdy / y_tick_spacing;
      /* ticks start and stop at 10-based locations (i.e. sloppy axis bounds) */
      /* so, given the current min..max, find the pretty min..max for ticks */
      y_range = ap->y1 - ap->y0;
      if (y_range <= 0.0)
	{
	  if (ap->y0 != 0.0)
	    {
	      ap->y1 = ap->y0 * 1.25;
	      y_range = ap->y0 * .25;
	    }
	  else
	    {
	      ap->y1 = 1.0;
	      y_range = 1.0;
	    }
	}

      tdy = describe_ticks((tick_descriptor *)(ap->y_ticks), ap->y0, ap->y1, num_ticks);
      ap->y_ticks = tdy;
      if (include_y_tick_labels)
	{
	  if (tdy->min_label) 
	    {
	      FREE(tdy->min_label); 
	      tdy->min_label = NULL;
	    }
	  tdy->min_label = prettyf(tdy->mlo, tdy->tens);
	  tdy->min_label_width = number_width(ss, tdy->min_label);

	  if (tdy->max_label) 
	    {
	      FREE(tdy->max_label); 
	      tdy->max_label = NULL;
	    }
	  tdy->max_label = prettyf(tdy->mhi, tdy->tens);
	  tdy->max_label_width = number_width(ss, tdy->max_label);
	  tick_label_width = tdy->min_label_width;
	  if (tick_label_width < tdy->max_label_width) 
	    tick_label_width = tdy->max_label_width;
	  if (((curx + tick_label_width) > (int)(.61 * width)) || 
	      ((4 * x_number_height) > height))
	    include_y_tick_labels = 0;
	  else curx += tick_label_width;
	}
      
      curx += major_tick_length;
      tdy->maj_tick_len = major_tick_length;
      tdy->min_tick_len = minor_tick_length;
      ap->y_axis_y1 = top_border_width;
    }
  else ap->y_axis_y1 = 0;

  ap->x_axis_x1 = width - right_border_width;
  ap->x_axis_x0 = curx;
  x_range = ap->x1 - ap->x0;
  if (x_range <= 0)
    {
      if (ap->x0 != 0.0)
	{
	  ap->x1 = ap->x0 * 1.25;
	  x_range = ap->x0 * .25;
	}
      else
	{
	  ap->x1 = .1;
	  x_range = .1;
	}
    }
  if (include_x_ticks) 
    {
      num_ticks = (ap->x_axis_x1 - curx) / x_tick_spacing;
      switch (x_style)
	{
	case X_AXIS_IN_SECONDS: 
	  tdx = describe_ticks((tick_descriptor *)(ap->x_ticks), ap->x0, ap->x1, num_ticks); 
	  break;
	case X_AXIS_IN_SAMPLES: 
	  tdx = describe_ticks((tick_descriptor *)(ap->x_ticks), ap->x0 * srate, ap->x1 * srate, num_ticks); 
	  break;
	case X_AXIS_IN_BEATS: 
	  if (ap->cp)
	    tdx = describe_ticks((tick_descriptor *)(ap->x_ticks), 
				 (ap->x0 * ap->cp->beats_per_minute / 60.0), 
				 (ap->x1 * ap->cp->beats_per_minute / 60.0), 
				 num_ticks); 
	  else tdx = describe_ticks((tick_descriptor *)(ap->x_ticks), ap->x0, ap->x1, num_ticks); 
	  break;
	case X_AXIS_AS_PERCENTAGE: 
	  tdx = describe_ticks((tick_descriptor *)(ap->x_ticks), ap->x0 / ap->xmax, ap->x1 / ap->xmax, num_ticks); 
	  break;
	default: 
	  tdx = describe_ticks((tick_descriptor *)(ap->x_ticks), ap->x0, ap->x1, num_ticks); break;
	}
      ap->x_ticks = tdx;
      if (include_x_tick_labels)
	{
	  if (tdx->min_label) 
	    {
	      FREE(tdx->min_label); 
	      tdx->min_label = NULL;
	    }
	  tdx->min_label = prettyf(tdx->mlo, tdx->tens);
	  tdx->min_label_width = number_width(ss, tdx->min_label);
	  if (tdx->max_label) 
	    {
	      FREE(tdx->max_label); 
	      tdx->max_label = NULL;
	    }
	  tdx->max_label = prettyf(tdx->mhi, tdx->tens);
	  tdx->max_label_width = number_width(ss, tdx->max_label);
	  tick_label_width = tdx->min_label_width;
	  if (tick_label_width < tdx->max_label_width) 
	    tick_label_width = tdx->max_label_width;
	  if ((curx + 2 * tick_label_width) > (int)(.61 * width)) 
	    include_x_tick_labels = 0;
	}
      tdx->maj_tick_len = major_tick_length;
      tdx->min_tick_len = minor_tick_length;
    }

#if USE_GTK
  cury -= 12;
#endif

  if ((include_x_label) || (include_x_tick_labels))
    {
      ap->x_label_y = cury;
      ap->x_label_x = (curx + width - x_label_width) / 2;
      cury -= (x_label_height + inner_border_width);
    }
  ap->y_axis_y0 = cury;
  ap->y_axis_x0 = curx;
  ap->y_axis_x0 += ap->graph_x0;
  ap->x_axis_x0 += ap->graph_x0;
  ap->x_axis_x1 += ap->graph_x0;
  ap->x_label_x += ap->graph_x0;
  ap->x_axis_y0 = cury;
  ap->x_scale = ((double)(ap->x_axis_x1 - ap->x_axis_x0)) / ((double)(ap->x1 - ap->x0));
  ap->y_scale = (Float)(ap->y_axis_y1 - ap->y_axis_y0) / (ap->y1 - ap->y0);
  /* now if y_offset is in use, apply global shift in y direction */
  ap->x_axis_y0 += ap->y_offset;
  ap->y_axis_y0 += ap->y_offset;
  ap->y_axis_y1 += ap->y_offset;
  ap->x_label_y += ap->y_offset;
  ap->x_base = (double)(ap->x_axis_x0 - ap->x0 * ap->x_scale);
  ap->y_base = (Float)(ap->y_axis_y0 - ap->y0 * ap->y_scale);
  if ((printing) && (ap->cp) &&
      ((ap->cp->chan == 0) || (ap->cp->sound->channel_style != CHANNELS_SUPERIMPOSED)))
    ps_bg(ap, ax);
#if HAVE_GL
  if (ap->use_gl) activate_gl_fonts(ss);
  ap->used_gl = ap->use_gl;
#if DEBUGGING
  {
    GLenum errcode;
    errcode = glGetError();
    if (errcode != GL_NO_ERROR)
      fprintf(stderr, "GL fonts: %s\n", gluErrorString(errcode));
  }
#endif
#endif
  if (include_x_label)
    {
      activate_label_font(ax, ss);
#if HAVE_GL
      if (ap->use_gl)
	{
	  Float yl;
	  yl = -0.5 - xthick - ((Float)(4 * major_tick_length + x_label_height) / (Float)height);
	  glColor3f(0.0, 0.0, 0.0);
	  glRasterPos3f(-0.1, 0.0, yl);
	  glListBase(label_base);
	  glCallLists(snd_strlen(ap->xlabel), GL_UNSIGNED_BYTE, (GLubyte *)(ap->xlabel));
	}
      else
#endif
      draw_string(ax, ap->x_label_x, ap->x_label_y + 7, ap->xlabel, snd_strlen(ap->xlabel));
      if (printing) 
	{
	  ps_set_label_font();
	  ps_draw_string(ap, ap->x_label_x, ap->x_label_y + 7, ap->xlabel);
	}
    }

  if (show_x_axis)
    {
#if HAVE_GL
      if (ap->use_gl)
	{
	  glBegin(GL_POLYGON);
	  glColor3f(0.0, 0.0, 0.0);
	  glVertex3f(-0.501 - ythick, 0.0, -0.501 - xthick);
	  glVertex3f(0.50, 0.0, -0.501 - xthick);
	  glVertex3f(0.50, 0.0, -0.501);
	  glVertex3f(-0.501 - ythick, 0.0, -0.501);
	  glEnd();
	}
      else 
#endif
      fill_rectangle(ax, ap->x_axis_x0, ap->x_axis_y0, (unsigned int)(ap->x_axis_x1 - ap->x_axis_x0), axis_thickness);
    }
  if (axes != SHOW_X_AXIS)
    {
#if HAVE_GL
      if (ap->use_gl)
	{
	  glBegin(GL_POLYGON);
	  glColor3f(0.0, 0.0, 0.0);
	  glVertex3f(-0.501 - ythick, 0.0, -0.501 - xthick);
	  glVertex3f(-0.501, 0.0, -0.501 - xthick);
	  glVertex3f(-0.501, 0.0, 0.50);
	  glVertex3f(-0.501 - ythick, 0.0, 0.50);
	  glEnd();
	}
      else
#endif
      fill_rectangle(ax, ap->y_axis_x0, ap->y_axis_y1, axis_thickness, (unsigned int)(ap->y_axis_y0 - ap->y_axis_y1));
    }
  if ((include_y_tick_labels) || 
      (include_x_tick_labels))
    activate_numbers_font(ax, ss);
  if (printing) 
    {
      if (show_x_axis)
	ps_fill_rectangle(ap, ap->x_axis_x0, ap->x_axis_y0, ap->x_axis_x1 - ap->x_axis_x0, axis_thickness);
      if (axes != SHOW_X_AXIS)
	ps_fill_rectangle(ap, ap->y_axis_x0, ap->y_axis_y1, axis_thickness, ap->y_axis_y0 - ap->y_axis_y1);
      if ((include_y_tick_labels) || 
	  (include_x_tick_labels)) 
	ps_set_number_font();
    }

  if (include_y_tick_labels)
    {
#if HAVE_GL
      if (ap->use_gl)
	{
	  Float xl;
	  xl = -0.5 - ythick - ((Float)(3 * tdy->maj_tick_len + tdy->min_label_width + inner_border_width) / (Float)width);
	  glRasterPos3f(xl, 0.0, (tdy->mlo - ap->y0) / (ap->y1 - ap->y0) - 0.51);
	  glListBase(number_base);
	  glCallLists(snd_strlen(tdy->min_label), GL_UNSIGNED_BYTE, (GLubyte *)(tdy->min_label));

	  xl = -0.5 - ythick - ((Float)(3 * tdy->maj_tick_len + tdy->max_label_width + inner_border_width) / (Float)width);
	  glRasterPos3f(xl, 0.0, (tdy->mhi - ap->y0) / (ap->y1 - ap->y0) - 0.51);
	  glListBase(number_base);
	  glCallLists(snd_strlen(tdy->max_label), GL_UNSIGNED_BYTE, (GLubyte *)(tdy->max_label));
	}
      else
	{
#endif
      draw_string(ax,
		  ap->y_axis_x0 - tdy->maj_tick_len - tdy->min_label_width - inner_border_width,
#if USE_GTK
		  (int)(grf_y(tdy->mlo, ap) - .5 * x_number_height),
#else
		  (int)(grf_y(tdy->mlo, ap) + .25 * x_number_height),
#endif
		  tdy->min_label,
		  strlen(tdy->min_label));
      draw_string(ax,
		  ap->y_axis_x0 - tdy->maj_tick_len - tdy->max_label_width - inner_border_width,
#if USE_GTK
		  (int)(grf_y(tdy->mhi, ap) - .5 * x_number_height),
#else
		  (int)(grf_y(tdy->mhi, ap) + .5 * x_number_height),
#endif
		  tdy->max_label,
		  strlen(tdy->max_label));
#if HAVE_GL
	}
#endif
      if (printing) 
	{
	  ps_draw_string(ap,
		      ap->y_axis_x0 - tdy->maj_tick_len - tdy->min_label_width - inner_border_width,
		      (int)(grf_y(tdy->mlo, ap) + .25 * x_number_height),
		      tdy->min_label);
	  ps_draw_string(ap,
		      ap->y_axis_x0 - tdy->maj_tick_len - tdy->max_label_width - inner_border_width,
		      (int)(grf_y(tdy->mhi, ap) + .5 * x_number_height),
		      tdy->max_label);
	}
    }
  if (include_x_tick_labels)
    {
      int lx0, lx1, tx0, tx1;
      /* the label is at ap->x_label_x to that plus x_label_width */
      /* the number label widths are tdx->max|min_label_width */
      lx0 = ap->x_label_x;
      lx1 = lx0 + x_label_width;
      tx0 = (int)(tick_grf_x(tdx->mlo, ap, x_style, srate) - .45 * tdx->min_label_width);
      tx1 = tx0 + tdx->min_label_width;
      if ((lx0 > tx1) || (lx1 < tx0))
	{
#if HAVE_GL
	  if (ap->use_gl)
	    {
	      Float yl;
	      yl = -0.5 - xthick - ((Float)(3 * major_tick_length + x_number_height + inner_border_width) / (Float)height);
	      glRasterPos3f((tdx->mlo - ap->x0) / (ap->x1 - ap->x0) - 0.53, 0.0, yl);
	      glListBase(number_base);
	      glCallLists(snd_strlen(tdx->min_label), GL_UNSIGNED_BYTE, (GLubyte *)(tdx->min_label));
	    }
	  else
#endif
	  draw_string(ax,
		      tx0,
		      ap->x_label_y + 2,
		      tdx->min_label,
		      strlen(tdx->min_label));
	  if (printing) 
	    ps_draw_string(ap,
			   tx0,
			   ap->x_label_y + 2,
			   tdx->min_label);
	}
      tx0 = (int)(tick_grf_x(tdx->mhi, ap, x_style, srate) - (.45 * tdx->max_label_width)); /* try centered label first */
      if ((tx0 + tdx->max_label_width) > ap->x_axis_x1)
	tx0 = (int)(tick_grf_x(tdx->mhi, ap, x_style, srate) - tdx->max_label_width + .75 * right_border_width);
      tx1 = tx0 + tdx->max_label_width;
      if ((lx0 > tx1) || (lx1 < tx0))
	{
#if HAVE_GL
	  if (ap->use_gl)
	    {
	      Float yl;
	      yl = -0.5 - xthick - ((Float)(3 * major_tick_length + x_number_height + inner_border_width) / (Float)height);
	      glRasterPos3f((tdx->mhi - ap->x0) / (ap->x1 - ap->x0) - 0.53, 0.0, yl);
	      glListBase(number_base);
	      glCallLists(snd_strlen(tdx->max_label), GL_UNSIGNED_BYTE, (GLubyte *)(tdx->max_label));
	    }
	  else
#endif
	  draw_string(ax,
		      tx0,
		      ap->x_label_y + 2,
		      tdx->max_label,
		      strlen(tdx->max_label));
	  if (printing) 
	    ps_draw_string(ap,
			   tx0,
			   ap->x_label_y + 2,
			   tdx->max_label);
	}
    }
  if (include_y_ticks)
    {
      /* start ticks at flo, go to fhi by step, major ticks at mlo mhi and intervals of tenstep surrounding */
      x0 = ap->y_axis_x0;
      majx = x0 - tdy->maj_tick_len;
      minx = x0 - tdy->min_tick_len;
      fy = tdy->mlo;
      ty = grf_y(fy, ap);
#if HAVE_GL
      if (ap->use_gl)
	{
	  Float ypos;
	  ypos = (fy - ap->y0) / (ap->y1 - ap->y0) - 0.5;
	  glBegin(GL_LINES);
	  glVertex3f(-0.50 - ymajorlen, 0.0, ypos);
	  glVertex3f(-0.501, 0.0, ypos);
	  glEnd();
	}
      else
#endif
      draw_line(ax, majx, ty, x0, ty);
      if (printing) ps_draw_line(ap, majx, ty, x0, ty);
      tens = 0.0;
      fy -= tdy->step;
      while (fy >= tdy->flo)
	{
	  tens += tdy->tenstep;
	  if (tens == 10.0)
	    {
	      tens = 0.0;
	      x = majx;
	    }
	  else x = minx;
	  ty = grf_y(fy, ap);
#if HAVE_GL
	  if (ap->use_gl)
	    {
	      Float ypos;
	      ypos = (fy - ap->y0) / (ap->y1 - ap->y0) - 0.5;
	      glBegin(GL_LINES);
	      glVertex3f(-0.50 - ((x == majx) ? ymajorlen : yminorlen), 0.0, ypos);
	      glVertex3f(-0.501, 0.0, ypos);
	      glEnd();
	    }
	  else
#endif
	  draw_line(ax, x, ty, x0, ty);
	  if (printing) ps_draw_line(ap, x, ty, x0, ty);
	  fy -= tdy->step;
	}
      tens = 0.0;
      fy = tdy->mlo;
      fy += tdy->step;
      while (fy <= tdy->fhi)
	{
	  tens += tdy->tenstep;
	  if (tens == 10.0)
	    {
	      tens = 0.0;
	      x = majx;
	    }
	  else x = minx;
	  ty = grf_y(fy, ap);
#if HAVE_GL
	  if (ap->use_gl)
	    {
	      Float ypos;
	      ypos = (fy - ap->y0) / (ap->y1 - ap->y0) - 0.5;
	      glBegin(GL_LINES);
	      glVertex3f(-0.50 - ((x == majx) ? ymajorlen : yminorlen), 0.0, ypos);
	      glVertex3f(-0.501, 0.0, ypos);
	      glEnd();
	    }
	  else
#endif
	  draw_line(ax, x, ty, x0, ty);
	  if (printing) ps_draw_line(ap, x, ty, x0, ty);
	  fy += tdy->step;
	}
    }
  if (include_x_ticks)
    {
      /* start ticks at flo, go to fhi by step, major ticks at mlo mhi and intervals of tenstep surrounding */
      y0 = ap->x_axis_y0;
      majy = y0 + tdx->maj_tick_len;
      miny = y0 + tdx->min_tick_len;
      fx = tdx->mlo;
      tx = tick_grf_x(fx, ap, x_style, srate);
#if HAVE_GL
      if (ap->use_gl)
	{
	  Float xpos;
	  xpos = (fx - ap->x0) / (ap->x1 - ap->x0) - 0.5;
	  glBegin(GL_LINES);
	  glVertex3f(xpos, 0.0, -0.50 - xmajorlen);
	  glVertex3f(xpos, 0.0, -0.501);
	  glEnd();
	}
      else
#endif
      draw_line(ax, tx, majy, tx, y0);
      if (printing) ps_draw_line(ap, tx, majy, tx, y0);
      tens = 0.0;
      fx -= tdx->step;
      while (fx >= tdx->flo)
	{
	  tens += tdx->tenstep;
	  if (tens == 10.0)
	    {
	      tens = 0.0;
	      y = majy;
	    }
	  else y = miny;
	  tx = tick_grf_x(fx, ap, x_style, srate);
#if HAVE_GL
	  if (ap->use_gl)
	    {
	      Float xpos;
	      xpos = (fx - ap->x0) / (ap->x1 - ap->x0) - 0.5;
	      glBegin(GL_LINES);
	      glVertex3f(xpos, 0.0, -0.50 - ((y == majy) ? xmajorlen : xminorlen));
	      glVertex3f(xpos, 0.0, -0.501);
	      glEnd();
	    }
	  else
#endif
	  draw_line(ax, tx, y, tx, y0);
	  if (printing) ps_draw_line(ap, tx, y, tx, y0);
	  fx -= tdx->step;
	}
      tens = 0.0;
      fx = tdx->mlo;
      fx += tdx->step;
      while (fx <= tdx->fhi)
	{
	  tens += tdx->tenstep;
	  if (tens == 10.0)
	    {
 	      tens = 0.0;
	      y = majy;
	    }
	  else y = miny;
	  tx = tick_grf_x(fx, ap, x_style, srate);
#if HAVE_GL
	  if (ap->use_gl)
	    {
	      Float xpos;
	      xpos = (fx - ap->x0) / (ap->x1 - ap->x0) - 0.5;
	      glBegin(GL_LINES);
	      glVertex3f(xpos, 0.0, -0.50 - ((y == majy) ? xmajorlen : xminorlen));
	      glVertex3f(xpos, 0.0, -0.501);
	      glEnd();
	    }
	  else
#endif
	  draw_line(ax, tx, y, tx, y0);
	  if (printing) ps_draw_line(ap, tx, y, tx, y0);
	  fx += tdx->step;
	}
    }
}

axis_info *make_axis_info (chan_info *cp, double xmin, double xmax, Float ymin, Float ymax, 
			   char *xlabel, double x0, double x1, Float y0, Float y1, 
			   axis_info *old_ap)
{
  axis_info *ap;
  if (old_ap) 
    {
      ap = old_ap;
      if (ap->xlabel) 
	{
	  FREE(ap->xlabel); 
	  ap->xlabel = NULL;
	}
    }
  else
    {
      ap = (axis_info *)CALLOC(1, sizeof(axis_info));
      ap->cp = cp;
    }
  ap->xmin = xmin;
  ap->xmax = xmax;
  if (ap->xmin == ap->xmax) ap->xmax += .001;
  ap->ymin = ymin;
  ap->ymax = ymax;
  ap->xlabel = copy_string(xlabel);
  ap->x0 = x0;
  ap->x1 = x1;
  if (ap->x0 == ap->x1) ap->x1 += .001;
  if (ap->x1 > ap->xmax) ap->x1 = ap->xmax;
  ap->y0 = y0;
  ap->y1 = y1;
  ap->y_offset = 0;
  ap->y_ambit = ap->ymax - ap->ymin;
  ap->x_ambit = ap->xmax - ap->xmin;
  return(ap);
}

#if (!USE_NO_GUI)

#define TO_C_AXIS_INFO(Snd, Chn, Ap, Caller) \
  get_ap(get_cp(Snd, Chn, Caller), \
         XEN_TO_C_INT_OR_ELSE(Ap, TIME_AXIS_INFO), \
         Caller)


static XEN g_grf_x(XEN val, XEN snd, XEN chn, XEN ap)
{
  #define H_x2position "(" S_x2position " val snd chn ax) -> x pixel loc of val"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_x2position, "a number");
  ASSERT_CHANNEL(S_x2position, snd, chn, 2);
  return(C_TO_XEN_INT(grf_x(XEN_TO_C_DOUBLE(val),
			    TO_C_AXIS_INFO(snd, chn, ap, S_x2position))));
}

static XEN g_grf_y(XEN val, XEN snd, XEN chn, XEN ap)
{
#define H_y2position "(" S_y2position " val snd chn ax) -> y pixel loc of val"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_y2position, "a number");
  ASSERT_CHANNEL(S_y2position, snd, chn, 2);
  return(C_TO_XEN_INT(grf_y(XEN_TO_C_DOUBLE(val),
			    TO_C_AXIS_INFO(snd, chn, ap, S_y2position))));
}

static XEN g_ungrf_x(XEN val, XEN snd, XEN chn, XEN ap)
{
  #define H_position2x "(" S_position2x " val snd chn ax) -> x in axis of pixel val"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_1, S_position2x, "an integer");
  ASSERT_CHANNEL(S_position2x, snd, chn, 2);
  return(C_TO_XEN_DOUBLE(ungrf_x(TO_C_AXIS_INFO(snd, chn, ap, S_position2x),
				 XEN_TO_C_INT(val))));
}

static XEN g_ungrf_y(XEN val, XEN snd, XEN chn, XEN ap)
{
  #define H_position2y "(" S_position2y " val snd chn ax) -> y in axis of pixel val"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_1, S_position2y, "an integer");
  ASSERT_CHANNEL(S_position2y, snd, chn, 2);
  return(C_TO_XEN_DOUBLE(ungrf_y(TO_C_AXIS_INFO(snd, chn, ap, S_position2y),
				 XEN_TO_C_INT(val))));
}

static XEN g_axis_info(XEN snd, XEN chn, XEN ap_id)
{
  #define H_axis_info "(" S_axis_info " snd chn grf) -> (list losamp hisamp x0 y0 x1 y1 xmin ymin xmax ymax pix_x0 pix_y0 pix_x1 pix_y1 y_offset)"
  axis_info *ap;
  ap = TO_C_AXIS_INFO(snd, chn, ap_id, "axis-info");
  return(XEN_CONS(C_TO_XEN_OFF_T(ap->losamp),
	  XEN_CONS(C_TO_XEN_OFF_T(ap->hisamp),
	   XEN_CONS(C_TO_XEN_DOUBLE(ap->x0),
            XEN_CONS(C_TO_XEN_DOUBLE(ap->y0),
             XEN_CONS(C_TO_XEN_DOUBLE(ap->x1),
              XEN_CONS(C_TO_XEN_DOUBLE(ap->y1),
               XEN_CONS(C_TO_XEN_DOUBLE(ap->xmin),
                XEN_CONS(C_TO_XEN_DOUBLE(ap->ymin),
                 XEN_CONS(C_TO_XEN_DOUBLE(ap->xmax),
                  XEN_CONS(C_TO_XEN_DOUBLE(ap->ymax),
                   XEN_CONS(C_TO_XEN_INT(ap->x_axis_x0),
                    XEN_CONS(C_TO_XEN_INT(ap->y_axis_y0),
                     XEN_CONS(C_TO_XEN_INT(ap->x_axis_x1),
		      XEN_CONS(C_TO_XEN_INT(ap->y_axis_y1),
                       XEN_CONS(C_TO_XEN_INT(ap->y_offset),
			XEN_CONS(C_TO_XEN_DOUBLE(ap->x_scale),
			 XEN_CONS(C_TO_XEN_DOUBLE(ap->y_scale),
		          XEN_EMPTY_LIST))))))))))))))))));
}

#if USE_MOTIF
/* this is intended for use with the xm package */

static XEN g_draw_axes(XEN args)
{
  #define H_draw_axes "(draw-axes wid gc label x0 x1 y0 y1 style axes) draws axes \
in the widget 'wid', using the graphics context 'gc', with the x-axis label 'label' \
going from x0 to x1 (floats) along the x axis, y0 to y1 along the y axis, with x-axis-style \
'style' (x-axis-in-seconds etc); whether the axes are actually displayed or just implied \
depends on 'axes'. Returns actual (pixel) axis bounds.  Defaults are label time, \
x0 0.0, x1 1.0, y0 -1.0, y1 1.0, style x-axis-in-seconds, axes #t."
  XEN val;
  Widget w; GC gc; char *xlabel; 
  double x0 = 0.0; double x1 = 1.0; 
  Float y0 = -1.0; Float y1 = 1.0; 
  int x_style = X_AXIS_IN_SECONDS; int axes = 1;
  axis_context *ax;
  axis_info *ap;
  int len;
  len = XEN_LIST_LENGTH(args);
  XEN_ASSERT_TYPE((len >= 3) && (len < 10), args, XEN_ONLY_ARG, S_draw_axes, "3 required and 6 optional args");
  w = (Widget)(XEN_UNWRAP_WIDGET(XEN_LIST_REF(args, 0)));
  gc = (GC)(XEN_UNWRAP_GC(XEN_LIST_REF(args, 1)));
  xlabel = XEN_TO_C_STRING(XEN_LIST_REF(args, 2));
  if (len > 3) x0 = XEN_TO_C_DOUBLE(XEN_LIST_REF(args, 3));
  if (len > 4) x1 = XEN_TO_C_DOUBLE(XEN_LIST_REF(args, 4));
  if (len > 5) y0 = XEN_TO_C_DOUBLE(XEN_LIST_REF(args, 5));
  if (len > 6) y1 = XEN_TO_C_DOUBLE(XEN_LIST_REF(args, 6));
  if (len > 7) x_style = XEN_TO_C_INT(XEN_LIST_REF(args, 7));
  if (len > 8) axes = XEN_TO_C_BOOLEAN(XEN_LIST_REF(args, 8));
  ap = (axis_info *)CALLOC(1, sizeof(axis_info));
  ax = (axis_context *)CALLOC(1, sizeof(axis_context));
  ap->ax = ax;
  ax->dp = XtDisplay(w);
  ax->wn = XtWindow(w);
  ap->xmin = x0;
  ap->xmax = x1;
  ap->ymin = y0;
  ap->ymax = y1;
  ap->y_ambit = y1 - y0;
  ap->x_ambit = x1 - x0;
  ap->xlabel = copy_string(xlabel);
  ap->x0 = x0;
  ap->x1 = x1;
  ap->y0 = y0;
  ap->y1 = y1;
  ap->width = widget_width(w);
  ap->window_width = ap->width;
  ap->y_offset = 0;
  ap->height = widget_height(w);
  ap->graph_x0 = 0;
  clear_window(ax);
  ax->gc = gc;
  make_axes_1(ap, x_style, 1, axes, FALSE, TRUE);
  val = XEN_CONS(C_TO_XEN_INT(ap->x_axis_x0),
	 XEN_CONS(C_TO_XEN_INT(ap->y_axis_y0),
          XEN_CONS(C_TO_XEN_INT(ap->x_axis_x1),
	   XEN_CONS(C_TO_XEN_INT(ap->y_axis_y1),
	    XEN_EMPTY_LIST))));
  free_axis_info(ap);
  return(val);
}
#endif

#ifdef XEN_ARGIFY_1
XEN_ARGIFY_4(g_grf_x_w, g_grf_x)
XEN_ARGIFY_4(g_grf_y_w, g_grf_y)
XEN_ARGIFY_4(g_ungrf_x_w, g_ungrf_x)
XEN_ARGIFY_4(g_ungrf_y_w, g_ungrf_y)
XEN_ARGIFY_3(g_axis_info_w, g_axis_info)
#else
#define g_grf_x_w g_grf_x
#define g_grf_y_w g_grf_y
#define g_ungrf_x_w g_ungrf_x
#define g_ungrf_y_w g_ungrf_y
#define g_axis_info_w g_axis_info
#endif

void g_init_axis(void)
{
  XEN_DEFINE_PROCEDURE(S_x2position, g_grf_x_w, 1, 3, 0,     H_x2position);
  XEN_DEFINE_PROCEDURE(S_y2position, g_grf_y_w, 1, 3, 0,     H_y2position);
  XEN_DEFINE_PROCEDURE(S_position2x, g_ungrf_x_w, 1, 3, 0,   H_position2x);
  XEN_DEFINE_PROCEDURE(S_position2y, g_ungrf_y_w, 1, 3, 0,   H_position2y);

  XEN_DEFINE_PROCEDURE(S_axis_info,  g_axis_info_w, 0, 3, 0, H_axis_info);

#if USE_MOTIF
  XEN_DEFINE_PROCEDURE(S_draw_axes,  g_draw_axes, 0, 0, 1,   H_draw_axes);
#endif
}
#endif
