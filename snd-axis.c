#include "snd.h"

axis_context *free_axis_context(axis_context *ax)
{
  if (ax) FREE(ax);
  return(NULL);
}

typedef struct tick_descriptor {
  double hi, lo; 
  int max_ticks;
  double flo, fhi, mlo, mhi, step, tenstep;
  int tens, min_label_width, max_label_width;
  Locus min_label_x, max_label_x;
  Latus maj_tick_len, min_tick_len;
  char *min_label, *max_label;
  Float grid_scale;
} tick_descriptor;

static tick_descriptor *free_tick_descriptor(tick_descriptor *td)
{
  if (td)
    {
      if (td->min_label) {FREE(td->min_label); td->min_label = NULL;}
      if (td->max_label) {FREE(td->max_label); td->max_label = NULL;}
      FREE(td);
    }
  return(NULL);
}

static tick_descriptor *describe_ticks(tick_descriptor *gd_td, double lo, double hi, int max_ticks, Float grid_scale)
{
  /* given absolute (unchangeable) axis bounds lo and hi, and maximum number of ticks to use, find a "pretty" tick placement */
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
	  (td->max_ticks == max_ticks) &&
	  ((fabs(td->grid_scale - grid_scale)) < .01))
	return(td);
    }
  td->hi = hi;
  td->lo = lo;
  td->grid_scale = grid_scale;
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
  mten = grid_scale * (double)(floor(4.0 * (.00001 + (mfdiv / plog10)))) / 4.0;
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

static bool first_beat(chan_info *cp, double val)
{
  int measure, beat;
  double beat_frac;
  beat = (int)val;
  beat_frac = val - beat;
  measure = (int)(beat / cp->beats_per_measure);
  beat = beat - measure * cp->beats_per_measure;
  return((beat == 0) &&
	 (beat_frac < .001));
}

static char *measure_number(int bpm, double val)
{
  /* split out the measure number, change to beat count (1-based), add fraction, if any */
  /* "val" is in terms of beats */
  char *buf;
  int measure, beat;
  double beat_frac;
  beat = (int)val;
  beat_frac = val - beat;
  measure = (int)(beat / bpm);
  beat = beat - measure * bpm;
  buf = (char *)CALLOC(64, sizeof(char));
  if (beat_frac > .001)
    {
      char *frac_buf, *tmp; /* according to the C spec, there's no way to get %f to omit the leading "0" */
      frac_buf = (char *)CALLOC(32, sizeof(char));
      snprintf(frac_buf, 32, "%.3f", beat_frac);
      if (frac_buf[0] == '0')
	tmp = (frac_buf + 1);  /* omit the leading "0" */
      else tmp = frac_buf;
      snprintf(buf, 64, "%d(%d)%s", 1 + measure, 1 + beat, tmp);
      FREE(frac_buf);
    }
  else snprintf(buf, 64, "%d(%d)", 1 + measure, 1 + beat);
  return(buf);
}

static char *clock_number(double loc, int tens)
{
  /* DD:HH:MM:SS.ddd */
  int day, hour, minute, second;
  double frac_second;
  char *buf;
  second = (int)loc;
  frac_second = loc - second;
  minute = (int)floor(second / 60);
  hour = (int)floor(minute / 60);
  day = (int)floor(hour / 24);
  second %= 60;
  minute %= 60;
  hour %= 24;
  buf = (char *)CALLOC(64, sizeof(char));
  if (day > 0)
    sprintf(buf, "%02d:%02d:%02d:%02d.%0*d", day, hour, minute, second, tens, (int)(frac_second * pow(10.0, tens)));
  else
    {
      if (hour > 0)
	sprintf(buf, "%02d:%02d:%02d.%0*d", hour, minute, second, tens, (int)(frac_second * pow(10.0, tens)));
      else
	{
	  if (minute > 0)
	    sprintf(buf, "%02d:%02d.%0*d", minute, second, tens, (int)(frac_second * pow(10.0, tens)));
	  else
	    {
	      if (second > 0)
		sprintf(buf, "%d.%0*d", second, tens, (int)(frac_second * pow(10.0, tens)));
	      else sprintf(buf, "0.%0*d", tens, (int)(frac_second * pow(10.0, tens)));
	    }
	}
    }
  return(buf);
}

static char *location_to_string(double loc, int style, int bpm, int tens)
{
  if (tens == 0) tens = 1; /* in x axis we want the ".0" */
  switch (style)
    {
    case X_AXIS_AS_CLOCK:
      return(clock_number(loc, tens));
      break;
    case X_AXIS_IN_MEASURES:
      return(measure_number(bpm, loc));
      break;
    default:
      return(prettyf(loc, tens));
      break;
    }
  return(prettyf(loc, tens));
}

char *x_axis_location_to_string(chan_info *cp, double loc)
{
  if (cp)
    {
      axis_info *ap;
      ap = cp->axis; /* time graph */
      if (ap)
	{
	  tick_descriptor *tdx;
	  tdx = ap->x_ticks;
	  if (tdx)
	    return(location_to_string(loc, cp->x_axis_style, cp->beats_per_measure, tdx->tens));
	}
    }
  return(prettyf(loc, 2));
}

axis_info *free_axis_info(axis_info *ap)
{
  if (!ap) return(NULL);
  if (ap->x_ticks) ap->x_ticks = free_tick_descriptor(ap->x_ticks);
  if (ap->y_ticks) ap->y_ticks = free_tick_descriptor(ap->y_ticks);
  if (ap->ax) ap->ax = free_axis_context(ap->ax);
  if (ap->xlabel) 
    {
      FREE(ap->xlabel); 
      ap->xlabel = NULL;
    }
  if (ap->default_xlabel) 
    {
      FREE(ap->default_xlabel); 
      ap->default_xlabel = NULL;
    }
  if (ap->ylabel) 
    {
      FREE(ap->ylabel); 
      ap->ylabel = NULL;
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

void init_axis_scales(axis_info *ap)
{
  if ((ap->x_axis_x0 == ap->x_axis_x1) || (ap->x0 == ap->x1))
    ap->x_scale = 0.0;
  else ap->x_scale = ((double)(ap->x_axis_x1 - ap->x_axis_x0)) / ((double)(ap->x1 - ap->x0));
  ap->x_base = (double)(ap->x_axis_x0 - ap->x0 * ap->x_scale);
  if ((ap->y_axis_y0 == ap->y_axis_y1) || (ap->y0 == ap->y1))
    ap->y_scale = 0.0;
  else ap->y_scale = (Float)(ap->y_axis_y1 - ap->y_axis_y0) / (ap->y1 - ap->y0);
  ap->y_base = (Float)(ap->y_axis_y0 - ap->y0 * ap->y_scale);
}

static Locus tick_grf_x(double val, axis_info *ap, x_axis_style_t style, int srate)
{
  int res = 0;
  switch (style)
    {
    case X_AXIS_AS_CLOCK:
    case X_AXIS_IN_SECONDS: 
    default:
      res = (int)(ap->x_base + val * ap->x_scale); 
      break;
    case X_AXIS_IN_BEATS: 
    case X_AXIS_IN_MEASURES:
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
    }
  if (res >= -32768) 
    {
      if (res < 32768) return((Locus)res);
      return(32767);
    }
  return(-32768);
}

#if HAVE_GL
  #if MUS_WITH_GL2PS
    void gl2ps_text(const char *msg);
  #endif
static bool gl_fonts_activated = false;
static int label_base, number_base;
static void activate_gl_fonts(void)
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
      gl_fonts_activated = true;
    }
#else
  if (!gl_fonts_activated)
    {
      label_base = glGenLists(128);
      number_base = glGenLists(128);
      gdk_gl_font_use_pango_font(AXIS_LABEL_FONT(ss), 32, 96, label_base + 32);
      gdk_gl_font_use_pango_font(AXIS_NUMBERS_FONT(ss), 32, 96, number_base + 32);
      gl_fonts_activated = true;
    }
#endif
}

void reload_label_font(void)
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
      gdk_gl_font_use_pango_font(AXIS_LABEL_FONT(ss), 32, 96, label_base + 32);
    }
#endif
}

void reload_number_font(void)
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
      gdk_gl_font_use_pango_font(AXIS_NUMBERS_FONT(ss), 32, 96, number_base + 32);
    }
#endif
}
#endif

static void draw_horizontal_grid_line(int y, axis_info *ap, axis_context *ax)
{
  color_t old_color;
  old_color = get_foreground_color(ax);
  if (ap->cp->cgx->selected)
    set_foreground_color(ax, ss->sgx->selected_grid_color);
  else set_foreground_color(ax, ss->sgx->grid_color);
  draw_line(ax, ap->y_axis_x0, y, ap->x_axis_x1, y);
  set_foreground_color(ax, old_color);
}

static void draw_vertical_grid_line(int x, axis_info *ap, axis_context *ax)
{
  color_t old_color;
  old_color = get_foreground_color(ax);
  if (ap->cp->cgx->selected)
    set_foreground_color(ax, ss->sgx->selected_grid_color);
  else set_foreground_color(ax, ss->sgx->grid_color);
  draw_line(ax, x, ap->x_axis_y0, x, ap->y_axis_y1);
  set_foreground_color(ax, old_color);
}

static void draw_x_number(const char *label, int x, int y, int hgt, axis_info *ap, axis_context *ax, printing_t printing)
{
  /* from motif point of view, gtk is down by font height (ascent) in pixels */
#if USE_MOTIF
  y = y + hgt + 1;
#else
  y = y - 1;
#endif
  draw_string(ax, x, y, label, snd_strlen(label));
  if (printing) 
    ps_draw_string(ap, x, y, label);
}

static void draw_y_number(const char *label, int x, int y, int hgt, axis_info *ap, axis_context *ax, printing_t printing)
{
  /* from motif point of view, gtk is down by font height (ascent) in pixels */
#if USE_MOTIF
  y = y + (int)(hgt / 2);
#else
  y = y - (int)(hgt / 2);
#endif
  draw_string(ax, x, y, label, snd_strlen(label));
  if (printing) 
    ps_draw_string(ap, x, y, label);
}

static void draw_label(const char *label, int x, int y, int yoff, axis_info *ap, axis_context *ax, printing_t printing)
{
  /* from motif point of view, gtk is down by font height (ascent) in pixels */
#if USE_GTK
  y -= yoff;
#endif
  draw_string(ax, x, y, label, snd_strlen(label));
  if (printing) 
    ps_draw_string(ap, x, y, label);
}

static void draw_vertical_tick(int x, int y0, int y1, axis_info *ap, axis_context *ax, printing_t printing, bool include_grid)
{
  draw_line(ax, x, y1, x, y0);
  if (printing) ps_draw_line(ap, x, y1, x, y0);
  if (include_grid) draw_vertical_grid_line(x, ap, ax);
}

static void draw_horizontal_tick(int x0, int x1, int y, axis_info *ap, axis_context *ax, printing_t printing, bool include_grid)
{
  draw_line(ax, x0, y, x1, y);
  if (printing) ps_draw_line(ap, x0, y, x1, y);
  if (include_grid) draw_horizontal_grid_line(y, ap, ax);
}

static void draw_log_tick_label(const char *label, int logx, int y, int hgt, int x_label_width, int right_border_width, 
				axis_info *ap, axis_context *ax, printing_t printing, bool use_tiny_font)
{
  /* is there room for a label? */
  /* the main label is at ap->x_label_x to that plus x_label_width */
  int lx0, lx1, tx0, tx1, label_width;
  lx0 = ap->x_label_x;
  lx1 = lx0 + x_label_width;
  label_width = number_width(label, use_tiny_font);
  tx0 = (int)(logx - .45 * label_width);
  if ((tx0 + label_width) > ap->x_axis_x1)
    tx0 = (int)(logx - label_width + .75 * right_border_width);
  tx1 = tx0 + label_width;
  if ((lx0 > tx1) || (lx1 < tx0))
    draw_x_number(label, tx0, y, hgt, ap, ax, printing);
}

static void use_tiny(axis_context *ax, printing_t printing)
{
#if USE_MOTIF
  ax->current_font = ((XFontStruct *)(TINY_FONT(ss)))->fid;
  XSetFont(ax->dp, ax->gc, ((XFontStruct *)(TINY_FONT(ss)))->fid);
#else
#if USE_GTK
  ax->current_font = TINY_FONT(ss);
#endif
#endif
  if (printing) ps_set_tiny_numbers_font();
}

static void set_numbers_font(axis_context *ax, printing_t printing, bool use_tiny_font)
{
  if (use_tiny_font)
    use_tiny(ax, printing);
  else
    {
#if USE_MOTIF
      ax->current_font = ((XFontStruct *)(AXIS_NUMBERS_FONT(ss)))->fid;
      XSetFont(ax->dp, ax->gc, ((XFontStruct *)(AXIS_NUMBERS_FONT(ss)))->fid);
#else
#if USE_GTK
      ax->current_font = AXIS_NUMBERS_FONT(ss);
#endif
#endif
      if (printing) ps_set_number_font();
    }
}

static void set_labels_font(axis_context *ax, printing_t printing, bool use_tiny_font)
{
  if (use_tiny_font)
    use_tiny(ax, printing);
  else
    {
#if USE_MOTIF
      ax->current_font = ((XFontStruct *)(AXIS_LABEL_FONT(ss)))->fid;
      XSetFont(ax->dp, ax->gc, ((XFontStruct *)(AXIS_LABEL_FONT(ss)))->fid);
#else
  #if USE_GTK
      ax->current_font = AXIS_LABEL_FONT(ss);
  #endif
#endif
      if (printing) ps_set_label_font();
    }
}


void make_axes_1(axis_info *ap, x_axis_style_t x_style, int srate, show_axes_t axes, printing_t printing, 
		 with_x_axis_t show_x_axis, with_grid_t with_grid, log_axis_t log_axes, Float grid_scale)
{
  Latus width, height;
  Latus axis_thickness, left_border_width, bottom_border_width, top_border_width, right_border_width, inner_border_width;
  Latus major_tick_length, minor_tick_length, x_tick_spacing, y_tick_spacing;
  bool include_x_label, include_x_ticks, include_x_tick_labels, include_y_ticks, include_y_tick_labels, include_grid;
  bool y_axis_linear = true, x_axis_linear = true, use_tiny_font = false;
  Latus x_label_width, x_label_height, x_number_height;
  int num_ticks;
  tick_descriptor *tdx = NULL, *tdy = NULL;
  Locus curx, cury;
  axis_context *ax;
#if HAVE_GL
  Float xthick, ythick, xmajorlen, xminorlen, ymajorlen, yminorlen;
#endif

  ax = ap->ax;
  width = ap->width;
  height = ap->height;
  ap->graph_active = ((width > 4) || (height > 10));
  include_grid = ((ap->cp) && (with_grid));
  
  if ((axes == SHOW_NO_AXES) || (width < 40) || (height < 40) || (ap->xmax == 0.0))
    {
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
      init_axis_scales(ap);
      return;
    }
  left_border_width = 10;
#if USE_MOTIF
  bottom_border_width = 10;
#else
  bottom_border_width = 4;
#endif
  top_border_width = 10;
  right_border_width = 14;
  inner_border_width = 5;
  x_tick_spacing = 20; 
  if (width < 250) x_tick_spacing = 10 + (width / 25);
  y_tick_spacing = 20; 
  if (height < 250) y_tick_spacing = 10 + (height / 25);
  if ((height < 100) || (width < 100))
    {
      major_tick_length = 4;
      minor_tick_length = 2;
    }
  else
    {
      major_tick_length = 9;
      minor_tick_length = 5;
    }
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
      if (axes == SHOW_BARE_X_AXIS)
	{
	  include_x_label = false;
	  include_x_tick_labels = false;
	  include_x_ticks = false;
	}
      else
	{
	  if ((axes == SHOW_X_AXIS_UNLABELLED) || (axes == SHOW_ALL_AXES_UNLABELLED))
	    include_x_label = false;
	  else include_x_label = ((ap->xlabel) && ((height > 100) && (width > 100)));
	  include_x_tick_labels = ((height > 60) && (width > 100));
	  include_x_ticks = ((height > 40) && (width > 40));
	}
    }
  else
    {
      include_x_label = false;
      include_x_tick_labels = false;
      include_x_ticks = false;
    }
  if (log_axes == WITH_LOG_X_AXIS) x_axis_linear = false;
  
  if ((axes != SHOW_X_AXIS) && (axes != SHOW_X_AXIS_UNLABELLED))
    {
      include_y_tick_labels = ((width > 100) && (height > 60));
      include_y_ticks = ((width > 100) && (height > 40));
    }
  else
    {
      include_y_tick_labels = false;
      include_y_ticks = false;
    }
  if (log_axes == WITH_LOG_Y_AXIS) y_axis_linear = false;
  
  curx = left_border_width;
  cury = height - bottom_border_width;
  
  use_tiny_font = ((width < 250) || (height < 140));
  
  x_number_height = number_height(use_tiny_font);
  x_label_height = 0;
  x_label_width = 0;
  
  if (include_x_label)
    {
      x_label_width = label_width(ap->xlabel, use_tiny_font);
      if ((x_label_width + curx + right_border_width) > width)
	{
	  include_x_label = false;
	  x_label_width = 0;
	  x_label_height = 0;
	}
      else x_label_height = label_height(use_tiny_font);
    }
  else
    {
      if (include_x_ticks) 
	x_label_height = label_height(use_tiny_font);
      /* bare x axis case handled later */
    }
  
  if (y_axis_linear)
    {
      if (include_y_ticks)
	{
	  double y_range;
	  /* figure out how long the longest tick label will be and make room for it */
	  /* values go from ap->y0 to ap->y1 */
	  /* basic tick spacing is tick_spacing pixels */
	  num_ticks = cury / y_tick_spacing;
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
	  tdy = describe_ticks(ap->y_ticks, ap->y0, ap->y1, num_ticks, grid_scale);
	  ap->y_ticks = tdy;
	  if (include_y_tick_labels)
	    {
	      Latus tick_label_width;
	      if (tdy->min_label) 
		{
		  FREE(tdy->min_label); 
		  tdy->min_label = NULL;
		}
	      tdy->min_label = prettyf(tdy->mlo, tdy->tens);
	      tdy->min_label_width = number_width(tdy->min_label, use_tiny_font);
	      
	      if (tdy->max_label) 
		{
		  FREE(tdy->max_label); 
		  tdy->max_label = NULL;
		}
	      tdy->max_label = prettyf(tdy->mhi, tdy->tens);
	      tdy->max_label_width = number_width(tdy->max_label, use_tiny_font);
	      tick_label_width = tdy->min_label_width;
	      if (tick_label_width < tdy->max_label_width) 
		tick_label_width = tdy->max_label_width;
	      if (((curx + tick_label_width) > (int)(.61 * width)) || 
		  ((4 * x_number_height) > height))
		include_y_tick_labels = false;
	      else curx += tick_label_width;
	    }
	  
	  curx += major_tick_length;
	  tdy->maj_tick_len = major_tick_length;
	  tdy->min_tick_len = minor_tick_length;
	  ap->y_axis_y1 = top_border_width;
	}
      else ap->y_axis_y1 = 0;
    }
  else
    {
      /* log case */
      if (include_y_tick_labels)
	curx += number_width("10000", use_tiny_font);
      curx += major_tick_length;
    }
  
  ap->x_axis_x1 = width - right_border_width;
  ap->x_axis_x0 = curx;
  {
    double x_range;
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
  }
  if ((x_axis_linear) && (include_x_ticks))
    {
      num_ticks = (ap->x_axis_x1 - curx) / x_tick_spacing;
      switch (x_style)
	{
	case X_AXIS_AS_CLOCK:
	case X_AXIS_IN_SECONDS: 
	default:
	  tdx = describe_ticks(ap->x_ticks, ap->x0, ap->x1, num_ticks, grid_scale); 
	  break;
	case X_AXIS_IN_SAMPLES: 
	  tdx = describe_ticks(ap->x_ticks, ap->x0 * srate, ap->x1 * srate, num_ticks, grid_scale); 
	  break;
	case X_AXIS_IN_BEATS: 
	case X_AXIS_IN_MEASURES:
	  if (ap->cp) /* cp==null probably can't happen -- ap->cp is null (only?) if we're called from the envelope editor */
	    {
	      Float beats_per_second;
	      beats_per_second = ap->cp->beats_per_minute / 60.0;
	      tdx = describe_ticks(ap->x_ticks, 
				   ap->x0 * beats_per_second,
				   ap->x1 * beats_per_second,
				   num_ticks, 
				   grid_scale); 
	      /* here we are getting the intra-beat settings, if x-axis-in-measures */
	    }
	  else tdx = describe_ticks(ap->x_ticks, ap->x0, ap->x1, num_ticks, grid_scale); 
	  break;
	  
	  /* PERHAPS: measure-positions or some such list + user interface support => interpolate unset measures */
	  /*          e.g. grab measure number and drag it => drag (push) all others unset, click = set? */
	  
	case X_AXIS_AS_PERCENTAGE: 
	  tdx = describe_ticks(ap->x_ticks, ap->x0 / ap->xmax, ap->x1 / ap->xmax, num_ticks, grid_scale); 
	  break;
	}
      ap->x_ticks = tdx;
      if (include_x_tick_labels)
	{
	  Latus tick_label_width;
	  if (tdx->min_label) 
	    {
	      FREE(tdx->min_label); 
	      tdx->min_label = NULL;
	    }
	  tdx->min_label = location_to_string(tdx->mlo, x_style, (ap->cp) ? (ap->cp->beats_per_measure) : 1, tdx->tens);
	  tdx->min_label_width = number_width(tdx->min_label, use_tiny_font);
	  if (tdx->max_label) 
	    {
	      FREE(tdx->max_label); 
	      tdx->max_label = NULL;
	    }
	  tdx->max_label = location_to_string(tdx->mhi, x_style, (ap->cp) ? (ap->cp->beats_per_measure) : 1, tdx->tens);
	  tdx->max_label_width = number_width(tdx->max_label, use_tiny_font);
	  tick_label_width = tdx->min_label_width;
	  if (tick_label_width < tdx->max_label_width) 
	    tick_label_width = tdx->max_label_width;
	  if ((curx + 2 * tick_label_width) > (int)(.61 * width)) 
	    include_x_tick_labels = false;
	}
      tdx->maj_tick_len = major_tick_length;
      tdx->min_tick_len = minor_tick_length;
    }
  
  if ((include_x_label) || (include_x_tick_labels))
    {
      ap->x_label_y = cury;
      ap->x_label_x = (curx + width - x_label_width) / 2;
      cury -= (x_label_height + inner_border_width);
    }
  else
    {
      if (axes == SHOW_BARE_X_AXIS)
	cury -= (label_height(false) + inner_border_width);
    }
  ap->y_axis_y0 = cury;
  ap->y_axis_x0 = curx;
  ap->y_axis_x0 += ap->graph_x0;
  ap->x_axis_x0 += ap->graph_x0;
  ap->x_axis_x1 += ap->graph_x0;
  ap->x_label_x += ap->graph_x0;
  ap->x_axis_y0 = cury;
  /* now if y_offset is in use, apply global shift in y direction */
  ap->x_axis_y0 += ap->y_offset;
  ap->y_axis_y0 += ap->y_offset;
  ap->y_axis_y1 += ap->y_offset;
  ap->x_label_y += ap->y_offset;
  init_axis_scales(ap);
  if ((printing) && (ap->cp) &&
      ((ap->cp->chan == 0) || (ap->cp->sound->channel_style != CHANNELS_SUPERIMPOSED)))
    ps_bg(ap, ax);
#if HAVE_GL
  if (ap->use_gl) activate_gl_fonts();
  ap->used_gl = ap->use_gl;
#endif
  
  /* x axis label */
  if (include_x_label)
    {
      set_labels_font(ax, printing, use_tiny_font);
#if HAVE_GL
      if (ap->use_gl)
	{
	  Float yl;
	  yl = -0.5 - xthick - ((Float)(4 * major_tick_length + x_label_height) / (Float)height);
	  glColor3f(0.0, 0.0, 0.0);
	  glRasterPos3f(-0.1, 0.0, yl);
	  glListBase(label_base);
  #if MUS_WITH_GL2PS
	  if (ss->gl_printing) gl2ps_text(ap->xlabel);
  #endif
	  glCallLists(snd_strlen(ap->xlabel), GL_UNSIGNED_BYTE, (GLubyte *)(ap->xlabel));
	}
      else
#endif
	{
	  draw_label(ap->xlabel, ap->x_label_x, ap->x_label_y + inner_border_width, x_label_height, ap, ax, printing);
	}
    }
  
  /* x axis */
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
	{
	  fill_rectangle(ax, ap->x_axis_x0, ap->x_axis_y0, (unsigned int)(ap->x_axis_x1 - ap->x_axis_x0), axis_thickness);
	}
    }
  
  /* y axis */
  if ((axes != SHOW_X_AXIS) && (axes != SHOW_X_AXIS_UNLABELLED))
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

	  /* draw rotated text here doesn't look very good -- the code in Mesa/progs/xdemos/xrotfontdemo.c uses
	   *   Mesa/progs/xdemos/xuserotfont.c which first sets up a bitmap very much like rotate_text in snd-xutils.c
	   *   then code much like the x axis label drawer above.
	   */
	}
      else
#endif
	{
#if (!USE_NO_GUI)
	  if ((ap->cp) && (ap->ylabel) && (include_y_tick_labels))
	    {
	      int y_label_width = 0;
	      y_label_width = label_width(ap->ylabel, use_tiny_font);
	      if ((ap->y_axis_y0 - ap->y_axis_y1) > (y_label_width + 20))
		draw_rotated_axis_label(ap->cp,	ax->gc, ap->ylabel, 
					ap->y_axis_x0 - tdy->maj_tick_len - tdy->min_label_width - inner_border_width,
					(int)((ap->y_axis_y0 + ap->y_axis_y1 - y_label_width) * 0.5) - 8);
	    }
#endif
	  fill_rectangle(ax, ap->y_axis_x0, ap->y_axis_y1, axis_thickness, (unsigned int)(ap->y_axis_y0 - ap->y_axis_y1));
	}
    }
  
  if (printing) 
    {
      if (show_x_axis)
	ps_fill_rectangle(ap, ap->x_axis_x0, ap->x_axis_y0, ap->x_axis_x1 - ap->x_axis_x0, axis_thickness);
      if ((axes != SHOW_X_AXIS) && (axes != SHOW_X_AXIS_UNLABELLED))
	ps_fill_rectangle(ap, ap->y_axis_x0, ap->y_axis_y1, axis_thickness, ap->y_axis_y0 - ap->y_axis_y1);
    }
  
  /* linear axis ticks/labels */
  if ((include_y_tick_labels) || 
      (include_x_tick_labels))
    set_numbers_font(ax, printing, use_tiny_font);
  
  if ((y_axis_linear) && (include_y_tick_labels))
    {
#if HAVE_GL
      if (ap->use_gl)
	{
	  Float xl;
	  xl = -0.5 - ythick - ((Float)(3 * tdy->maj_tick_len + tdy->min_label_width + inner_border_width) / (Float)width);
	  glRasterPos3f(xl, 0.0, (tdy->mlo - ap->y0) / (ap->y1 - ap->y0) - 0.51);
	  glListBase(number_base);
  #if MUS_WITH_GL2PS
	  if (ss->gl_printing) gl2ps_text(tdy->min_label);
  #endif
	  glCallLists(snd_strlen(tdy->min_label), GL_UNSIGNED_BYTE, (GLubyte *)(tdy->min_label));
	  
	  xl = -0.5 - ythick - ((Float)(3 * tdy->maj_tick_len + tdy->max_label_width + inner_border_width) / (Float)width);
	  glRasterPos3f(xl, 0.0, (tdy->mhi - ap->y0) / (ap->y1 - ap->y0) - 0.51);
	  glListBase(number_base);
  #if MUS_WITH_GL2PS
	  if (ss->gl_printing) gl2ps_text(tdy->max_label);
  #endif
	  glCallLists(snd_strlen(tdy->max_label), GL_UNSIGNED_BYTE, (GLubyte *)(tdy->max_label));
	}
      else
#endif
	{
	  /* y axis numbers */
	  
	  draw_y_number(tdy->min_label,
			ap->y_axis_x0 - tdy->maj_tick_len - tdy->min_label_width - inner_border_width,
			grf_y(tdy->mlo, ap), x_number_height,
			ap, ax, printing);
	  draw_y_number(tdy->max_label,
			ap->y_axis_x0 - tdy->maj_tick_len - tdy->max_label_width - inner_border_width,
			grf_y(tdy->mhi, ap), x_number_height,
			ap, ax, printing);
	}
    }
  if ((x_axis_linear) && (include_x_tick_labels))
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
  #if MUS_WITH_GL2PS
	      if (ss->gl_printing) gl2ps_text(tdx->min_label);
  #endif
	      glCallLists(snd_strlen(tdx->min_label), GL_UNSIGNED_BYTE, (GLubyte *)(tdx->min_label));
	    }
	  else
#endif
	    {
	      /* x axis min label */
	      draw_x_number(tdx->min_label, tx0, ap->x_axis_y0 + major_tick_length, x_number_height, ap, ax, printing);
	    }
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
  #if MUS_WITH_GL2PS
	      if (ss->gl_printing) gl2ps_text(tdx->max_label);
  #endif
	      glCallLists(snd_strlen(tdx->max_label), GL_UNSIGNED_BYTE, (GLubyte *)(tdx->max_label));
	    }
	  else
#endif
	    {
	      /* x axis max label */
	      draw_x_number(tdx->max_label, tx0, ap->x_axis_y0 + major_tick_length, x_number_height, ap, ax, printing);
	    }
	}
    }
  if ((y_axis_linear) && (include_y_ticks))
    {
      double fy, tens;
      int ty, x0, majx, minx, x;
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
	{
	  draw_horizontal_tick(majx, x0, ty, ap, ax, printing, include_grid);
	}
      
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
	    {
	      draw_horizontal_tick(x, x0, ty, ap, ax, printing, include_grid);
	    }
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
	    {
	      draw_horizontal_tick(x, x0, ty, ap, ax, printing, include_grid);
	    }
	  fy += tdy->step;
	}
    }
  if ((x_axis_linear) && (include_x_ticks))
    {
      bool major_tick_is_less_than_measure = false;
      double fx, tens;
      int tx, y0, majy, miny, y;
      y0 = ap->x_axis_y0;
      majy = y0 + tdx->maj_tick_len;
      miny = y0 + tdx->min_tick_len;
      /* start at leftmost major tick and work toward y axis */
      fx = tdx->mlo;
      tx = tick_grf_x(fx, ap, x_style, srate);
      
      if ((ap->cp) && 
	  (x_style == X_AXIS_IN_MEASURES) &&
	  ((tdx->tenstep * tdx->step) <= ((60.0 * ap->cp->beats_per_measure) / (Float)(ap->cp->beats_per_minute))))
	major_tick_is_less_than_measure = true;
      
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
	{
	  if ((major_tick_is_less_than_measure) &&
	      (first_beat(ap->cp, fx)))
	    draw_vertical_tick(tx, y0 - major_tick_length, majy + minor_tick_length, ap, ax, printing, include_grid);
	  else draw_vertical_tick(tx, y0, majy, ap, ax, printing, include_grid);
	}
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
	    {
	      if ((major_tick_is_less_than_measure) &&
		  (first_beat(ap->cp, fx)))
		draw_vertical_tick(tx, y0 - major_tick_length, y + minor_tick_length, ap, ax, printing, include_grid);
	      else draw_vertical_tick(tx, y0, y, ap, ax, printing, include_grid);
	    }
	  fx -= tdx->step;
	}
      tens = 0.0;
      fx = tdx->mlo;
      /* now start at leftmost major tick and work towards right end */
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
	    {
	      if ((major_tick_is_less_than_measure) &&
		  (first_beat(ap->cp, fx)))
		draw_vertical_tick(tx, y0 - major_tick_length, y + minor_tick_length, ap, ax, printing, include_grid);
	      else draw_vertical_tick(tx, y0, y, ap, ax, printing, include_grid);
	    }
	  fx += tdx->step;
	}
    }
  
  /* All linear axis stuff has been taken care of. Check for log axes (assume fft context, not spectrogram so no GL) */
  /* x axis overall label has already gone out */
  
  if ((!x_axis_linear) && 
      (show_x_axis) && 
      (include_x_ticks))
    {
      double min_freq, max_freq;
      Float minlx = 0.0, maxlx, fap_range, log_range, lscale = 1.0, curlx;
      Locus logx;
      int y0, majy, miny, i;
      char *label = NULL;
      Float freq = 0.0, freq10 = 0.0;
      /* get min (log-freq or spectro-start), max, add major ticks and brief labels, then if room add log-style minor ticks (100's, 1000's) */
      y0 = ap->x_axis_y0;
      majy = y0 + major_tick_length;
      miny = y0 + minor_tick_length;
      min_freq = ap->x0;
      max_freq = ap->x1;
      if (include_x_tick_labels)
	set_numbers_font(ax, printing, use_tiny_font);
      fap_range = max_freq - min_freq;
      if (min_freq > 1.0) minlx = log(min_freq); else minlx = 0.0;
      maxlx = log(max_freq);
      log_range = (maxlx - minlx);
      lscale = fap_range / log_range;
      for (i = 0; i < 3; i++)
	{
	  switch (i)
	    {
	    case 0:
	      label = "100";
	      freq = 100.0;
	      break;
	    case 1:
	      label = "1000";
	      freq = 1000.0;
	      break;
	    case 2:
	      label = "10000";
	      freq = 10000.0;
	      break;
	    }
	  if ((min_freq <= freq) && (max_freq >= freq))
	    {
	      /* draw major tick at freq */
	      freq10 = freq / 10.0;
	      logx = grf_x(min_freq + lscale * (log(freq) - minlx), ap);
	      draw_vertical_tick(logx, y0, majy, ap, ax, printing, include_grid);	      
	      draw_log_tick_label(label, logx, ap->x_axis_y0 + major_tick_length, x_number_height, x_label_width, right_border_width, 
				  ap, ax, printing, use_tiny_font);
	      if (width > 200)
		{
		  curlx = snd_round(min_freq / freq10) * freq10;
		  for (; curlx < freq; curlx += freq10)
		    {
		      logx = grf_x(min_freq + lscale * (log(curlx) - minlx), ap);
		      draw_vertical_tick(logx, y0, miny, ap, ax, printing, include_grid);	      
		    }
		}
	    }
	}
    }
  
  if ((!y_axis_linear) && 
      (axes != SHOW_X_AXIS) && 
      (axes != SHOW_X_AXIS_UNLABELLED) &&
      (include_y_ticks))
    {
      double min_freq, max_freq;
      Float minlx = 0.0, maxlx, fap_range, log_range, lscale = 1.0, curly;
      Locus logy;
      int x0, majx, minx, i;
      char *label = NULL;
      Float freq = 0.0, freq10 = 0.0;
      /* get min (log-freq or spectro-start), max, add major ticks and brief labels, then if room add log-style minor ticks (100's, 1000's) */
      x0 = ap->y_axis_x0;
      majx = x0 - major_tick_length;
      minx = x0 - minor_tick_length;
      min_freq = ap->y0;
      max_freq = ap->y1;
      if (include_y_tick_labels)
	set_numbers_font(ax, printing, use_tiny_font);
      fap_range = max_freq - min_freq;
      if (min_freq > 1.0) minlx = log(min_freq); else minlx = 0.0;
      maxlx = log(max_freq);
      log_range = (maxlx - minlx);
      lscale = fap_range / log_range;
      for (i = 0; i < 3; i++)
	{
	  switch (i)
	    {
	    case 0:
	      label = "100";
	      freq = 100.0;
	      break;
	    case 1:
	      label = "1000";
	      freq = 1000.0;
	      break;
	    case 2:
	      label = "10000";
	      freq = 10000.0;
	      break;
	    }
	  if ((min_freq <= freq) && (max_freq >= freq))
	    {
	      /* draw major tick at freq */
	      freq10 = freq / 10.0;
	      logy = grf_y(min_freq + lscale * (log(freq) - minlx), ap);
	      draw_horizontal_tick(majx, x0, logy, ap, ax, printing, include_grid);	      
	      if (include_y_tick_labels)
		{
		  int label_width;
		  label_width = number_width(label, use_tiny_font);
		  /* y axis number */
		  draw_y_number(label,
				ap->y_axis_x0 - major_tick_length - label_width - inner_border_width,
				logy, x_number_height,
				ap, ax, printing);
		}
	      if (height > 200)
		{
		  curly = snd_round(min_freq / freq10) * freq10;
		  for (; curly < freq; curly += freq10)
		    {
		      logy = grf_y(min_freq + lscale * (log(curly) - minlx), ap);
		      draw_horizontal_tick(minx, x0, logy, ap, ax, printing, include_grid);	      
		    }
		}
	    }
	}
    }
}

axis_info *make_axis_info (chan_info *cp, double xmin, double xmax, Float ymin, Float ymax, 
			   char *xlabel, double x0, double x1, Float y0, Float y1, axis_info *old_ap)
{
  axis_info *ap;
  if (old_ap) 
    ap = old_ap;
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
  if ((xlabel) && 
      ((!(ap->xlabel)) || (strcmp(xlabel, ap->xlabel) != 0)))
    {
      /* this apparently should leave the default_xlabel and ylabels alone */
      if (ap->xlabel) FREE(ap->xlabel);
      ap->xlabel = copy_string(xlabel);
    }
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

#define TO_C_AXIS_INFO(Snd, Chn, Ap, Caller)	\
  get_ap(get_cp(Snd, Chn, Caller),				     \
         (axis_info_t)XEN_TO_C_INT_OR_ELSE(Ap, (int)TIME_AXIS_INFO), \
         Caller)

static XEN g_x_to_position(XEN val, XEN snd, XEN chn, XEN ap)
{
  #define H_x_to_position "(" S_x_to_position " val :optional snd chn (ax " S_time_graph ")): x pixel loc of val"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_x_to_position, "a number");
  ASSERT_CHANNEL(S_x_to_position, snd, chn, 2);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ap), ap, XEN_ARG_4, S_x_to_position, S_time_graph ", " S_transform_graph ", or " S_lisp_graph);
  return(C_TO_XEN_INT(grf_x(XEN_TO_C_DOUBLE(val),
			    TO_C_AXIS_INFO(snd, chn, ap, S_x_to_position))));
}

static XEN g_y_to_position(XEN val, XEN snd, XEN chn, XEN ap)
{
  #define H_y_to_position "(" S_y_to_position " val :optional snd chn (ax " S_time_graph ")): y pixel loc of val"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_y_to_position, "a number");
  ASSERT_CHANNEL(S_y_to_position, snd, chn, 2);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ap), ap, XEN_ARG_4, S_y_to_position, S_time_graph ", " S_transform_graph ", or " S_lisp_graph);
  return(C_TO_XEN_INT(grf_y(XEN_TO_C_DOUBLE(val),
			    TO_C_AXIS_INFO(snd, chn, ap, S_y_to_position))));
}

static XEN g_position_to_x(XEN val, XEN snd, XEN chn, XEN ap)
{
  #define H_position_to_x "(" S_position_to_x " val :optional snd chn (ax " S_time_graph ")): x axis value corresponding to pixel val"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_1, S_position_to_x, "an integer");
  ASSERT_CHANNEL(S_position_to_x, snd, chn, 2);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ap), ap, XEN_ARG_4, S_position_to_x, S_time_graph ", " S_transform_graph ", or " S_lisp_graph);
  return(C_TO_XEN_DOUBLE(ungrf_x(TO_C_AXIS_INFO(snd, chn, ap, S_position_to_x),
				 XEN_TO_C_INT(val))));
}

static XEN g_position_to_y(XEN val, XEN snd, XEN chn, XEN ap)
{
  #define H_position_to_y "(" S_position_to_y " val :optional snd chn (ax " S_time_graph ")): y axis value corresponding to pixel val"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_1, S_position_to_y, "an integer");
  ASSERT_CHANNEL(S_position_to_y, snd, chn, 2);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ap), ap, XEN_ARG_4, S_position_to_y, S_time_graph ", " S_transform_graph ", or " S_lisp_graph);
  return(C_TO_XEN_DOUBLE(ungrf_y(TO_C_AXIS_INFO(snd, chn, ap, S_position_to_y),
				 XEN_TO_C_INT(val))));
}

static XEN g_axis_info(XEN snd, XEN chn, XEN ap_id)
{
  #define H_axis_info "(" S_axis_info " :optional snd chn (ax " S_time_graph ")): info about axis: (list losamp hisamp \
x0 y0 x1 y1 xmin ymin xmax ymax pix_x0 pix_y0 pix_x1 pix_y1 y_offset xscale yscale xlabel ylabel new-peaks)"
  axis_info *ap;
  ASSERT_CHANNEL(S_axis_info, snd, chn, 1);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ap_id), ap_id, XEN_ARG_3, S_axis_info, S_time_graph ", " S_transform_graph ", or " S_lisp_graph);
  ap = TO_C_AXIS_INFO(snd, chn, ap_id, S_axis_info);
  if (ap == NULL) return(XEN_EMPTY_LIST);
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
			  XEN_CONS((ap->xlabel) ? C_TO_XEN_STRING(ap->xlabel) : XEN_FALSE,
			   XEN_CONS((ap->ylabel) ? C_TO_XEN_STRING(ap->ylabel) : XEN_FALSE,
			    XEN_CONS((ap->cp) ? C_TO_XEN_BOOLEAN(ap->cp->new_peaks) : XEN_FALSE,
			     XEN_EMPTY_LIST)))))))))))))))))))));
}

/* this is intended for use with the xm package */

#if USE_MOTIF
#define XEN_GC_P(Value) (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
			 (strcmp("GC", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))
#else
#define XEN_GC_P(Value) (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
			 (strcmp("GdkGC_", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))
#endif

#define AXIS_STYLE_OK(Id) (((Id) >= 0) && ((Id) < NUM_X_AXIS_STYLES))
#define SHOW_AXES_OK(Id) (((Id) >= 0) && ((Id) < NUM_SHOW_AXES))

static XEN g_draw_axes(XEN args)
{
  #define H_draw_axes "(" S_draw_axes " wid gc label (x0 0.0) (x1 1.0) (y0 -1.0) (y1 1.0) (style " S_x_axis_in_seconds ") (axes " S_show_all_axes ")): \
draws axes in the widget 'wid', using the graphics context 'gc', with the x-axis label 'label' \
going from x0 to x1 (floats) along the x axis, y0 to y1 along the y axis, with " S_x_axis_style " \
'style' (" S_x_axis_in_seconds " etc); the axes are actually displayed if 'axes' is " S_show_all_axes ". \
Returns actual (pixel) axis bounds -- a list (x0 y0 x1 y1)."
  XEN val, xwid, xgc, xx0, xx1, xy0, xy1, xstyle, xaxes;
#if USE_MOTIF
  Widget w; 
  GC gc; 
#else
  GtkWidget *w; 
  GdkGC *gc;
#endif
  XEN label_ref;
  double x0 = 0.0, x1 = 1.0; 
  Float y0 = -1.0, y1 = 1.0; 
  x_axis_style_t x_style = X_AXIS_IN_SECONDS;
  show_axes_t axes = SHOW_ALL_AXES;
  axis_context *ax;
  axis_info *ap;
  int len;
  len = XEN_LIST_LENGTH(args);
  XEN_ASSERT_TYPE((len >= 3) && (len < 10), args, XEN_ONLY_ARG, S_draw_axes, "3 required and 6 optional args");
  
  xwid = XEN_LIST_REF(args, 0);
  XEN_ASSERT_TYPE(XEN_WIDGET_P(xwid), xwid, XEN_ARG_1, S_draw_axes, "widget");
  xgc = XEN_LIST_REF(args, 1);
  XEN_ASSERT_TYPE(XEN_GC_P(xgc), xgc, XEN_ARG_2, S_draw_axes, "gc");
#if USE_MOTIF
  w = (Widget)(XEN_UNWRAP_WIDGET(xwid));
  gc = (GC)(XEN_UNWRAP_GC(xgc));
#else
  w = (GtkWidget *)(XEN_UNWRAP_WIDGET(xwid));
  gc = (GdkGC *)(XEN_UNWRAP_GC(xgc));
#endif
  label_ref = XEN_LIST_REF(args, 2);
  XEN_ASSERT_TYPE(XEN_STRING_P(label_ref) || XEN_FALSE_P(label_ref), label_ref, XEN_ARG_3, S_draw_axes, "a string");
  if (len > 3) 
    {
      xx0 = XEN_LIST_REF(args, 3);
      XEN_ASSERT_TYPE(XEN_NUMBER_P(xx0), xx0, XEN_ARG_4, S_draw_axes, "a number");
      x0 = XEN_TO_C_DOUBLE(xx0);
      if (len > 4) 
	{
	  xx1 = XEN_LIST_REF(args, 4);
	  XEN_ASSERT_TYPE(XEN_NUMBER_P(xx1), xx1, XEN_ARG_5, S_draw_axes, "a number");
	  x1 = XEN_TO_C_DOUBLE(xx1);
	  if (len > 5) 
	    {
	      xy0 = XEN_LIST_REF(args, 5);
	      XEN_ASSERT_TYPE(XEN_NUMBER_P(xy0), xy0, XEN_ARG_6, S_draw_axes, "a number");
	      y0 = XEN_TO_C_DOUBLE(xy0);
	      if (len > 6) 
		{
		  xy1 = XEN_LIST_REF(args, 6);
		  XEN_ASSERT_TYPE(XEN_NUMBER_P(xy1), xy1, XEN_ARG_7, S_draw_axes, "a number");
		  y1 = XEN_TO_C_DOUBLE(xy1);
		  if (len > 7) 
		    {
		      int tmp;
		      xstyle = XEN_LIST_REF(args, 7);
		      XEN_ASSERT_TYPE(XEN_INTEGER_P(xstyle), xstyle, XEN_ARG_8, S_draw_axes, "axis style");
		      tmp = XEN_TO_C_INT(xstyle);
		      if (!(AXIS_STYLE_OK(tmp)))
			XEN_OUT_OF_RANGE_ERROR(S_draw_axes, 7, xstyle, "axis style");
		      x_style = (x_axis_style_t)tmp;
		      if (len > 8) 
			{
			  xaxes = XEN_LIST_REF(args, 8);
			  XEN_ASSERT_TYPE(XEN_INTEGER_P(xaxes), xaxes, XEN_ARG_9, S_draw_axes, S_show_axes " choice");
			  tmp = XEN_TO_C_INT(xaxes);
			  if (!(SHOW_AXES_OK(tmp)))
			    XEN_OUT_OF_RANGE_ERROR(S_draw_axes, 8, xaxes, S_show_axes " choice");
			  axes = (show_axes_t)XEN_TO_C_INT(xaxes);
			}}}}}}
  ap = (axis_info *)CALLOC(1, sizeof(axis_info));
  ax = (axis_context *)CALLOC(1, sizeof(axis_context));
  ap->ax = ax;
#if USE_MOTIF
  ax->dp = XtDisplay(w);
  ax->wn = XtWindow(w);
#else
  ax->wn = w->window;
  ax->w = w;
#endif  
  ap->xmin = x0;
  ap->xmax = x1;
  ap->ymin = y0;
  ap->ymax = y1;
  ap->y_ambit = y1 - y0;
  ap->x_ambit = x1 - x0;
  if (XEN_STRING_P(label_ref))
    ap->xlabel = copy_string(XEN_TO_C_STRING(label_ref));
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
  make_axes_1(ap, x_style, 1, axes, NOT_PRINTING, WITH_X_AXIS, NO_GRID, WITH_LINEAR_AXES, grid_density(ss));
  val = XEN_CONS(C_TO_XEN_INT(ap->x_axis_x0),
	 XEN_CONS(C_TO_XEN_INT(ap->y_axis_y0),
	  XEN_CONS(C_TO_XEN_INT(ap->x_axis_x1),
	   XEN_CONS(C_TO_XEN_INT(ap->y_axis_y1),
	    XEN_EMPTY_LIST))));
  free_axis_info(ap);
  return(xen_return_first(val, args));
}

static XEN g_x_axis_label(XEN snd, XEN chn, XEN ax)
{
  #define H_x_axis_label "(" S_x_axis_label " :optional snd chn (ax " S_time_graph ")): current x axis label"
  axis_info *ap;
  ASSERT_CHANNEL(S_x_axis_label, snd, chn, 1);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ax), ax, XEN_ARG_3, S_x_axis_label, S_time_graph ", " S_transform_graph ", or " S_lisp_graph);
  ap = TO_C_AXIS_INFO(snd, chn, ax, S_x_axis_label);
  return(C_TO_XEN_STRING(ap->xlabel));
}

static XEN g_set_x_axis_label(XEN label, XEN snd, XEN chn, XEN ax)
{
  axis_info *ap;
  ASSERT_CHANNEL(S_setB S_x_axis_label, snd, chn, 2);
  XEN_ASSERT_TYPE(XEN_STRING_P(label) || XEN_FALSE_P(label), label, XEN_ARG_1, S_setB S_x_axis_label, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ax), ax, XEN_ARG_4, S_setB S_x_axis_label, S_time_graph ", " S_transform_graph ", or " S_lisp_graph);
  ap = TO_C_AXIS_INFO(snd, chn, ax, S_x_axis_label);
  if (ap->xlabel) FREE(ap->xlabel);
  if (ap->default_xlabel) FREE(ap->default_xlabel);
  if (XEN_FALSE_P(label))
    {
      ap->xlabel = NULL;
      ap->default_xlabel = NULL;
    }
  else
    {
      ap->xlabel = copy_string(XEN_TO_C_STRING(label));
      if ((XEN_INTEGER_P(ax)) && (XEN_TO_C_INT(ax) == (int)TRANSFORM_AXIS_INFO))
	set_fft_info_xlabel(ap->cp, ap->xlabel);
      ap->default_xlabel = copy_string(ap->xlabel);
    }
  update_graph(ap->cp);
  return(label);
}

WITH_FOUR_SETTER_ARGS(g_set_x_axis_label_reversed, g_set_x_axis_label)
  
static XEN g_y_axis_label(XEN snd, XEN chn, XEN ax)
{
  #define H_y_axis_label "(" S_y_axis_label " :optional snd chn (ax " S_time_graph ")): current y axis label"
  axis_info *ap;
  ASSERT_CHANNEL(S_y_axis_label, snd, chn, 1);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ax), ax, XEN_ARG_3, S_y_axis_label, S_time_graph ", " S_transform_graph ", or " S_lisp_graph);
  ap = TO_C_AXIS_INFO(snd, chn, ax, S_y_axis_label);
  return(C_TO_XEN_STRING(ap->ylabel));
}

static XEN g_set_y_axis_label(XEN label, XEN snd, XEN chn, XEN ax)
{
  axis_info *ap;
  ASSERT_CHANNEL(S_setB S_y_axis_label, snd, chn, 2);
  XEN_ASSERT_TYPE(XEN_STRING_P(label) || XEN_FALSE_P(label), label, XEN_ARG_1, S_setB S_y_axis_label, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ax), ax, XEN_ARG_4, S_setB S_y_axis_label, S_time_graph ", " S_transform_graph ", or " S_lisp_graph);
  ap = TO_C_AXIS_INFO(snd, chn, ax, S_y_axis_label);
  if (ap->ylabel) FREE(ap->ylabel);
  if (XEN_FALSE_P(label))
    ap->ylabel = NULL;
  else ap->ylabel = copy_string(XEN_TO_C_STRING(label));
  update_graph(ap->cp);
  return(label);
}

WITH_FOUR_SETTER_ARGS(g_set_y_axis_label_reversed, g_set_y_axis_label)
  
#ifdef XEN_ARGIFY_1
XEN_ARGIFY_4(g_x_to_position_w, g_x_to_position)
XEN_ARGIFY_4(g_y_to_position_w, g_y_to_position)
XEN_ARGIFY_4(g_position_to_x_w, g_position_to_x)
XEN_ARGIFY_4(g_position_to_y_w, g_position_to_y)
XEN_ARGIFY_3(g_axis_info_w, g_axis_info)
#if (!USE_NO_GUI)
XEN_VARGIFY(g_draw_axes_w, g_draw_axes)
#endif
XEN_ARGIFY_3(g_x_axis_label_w, g_x_axis_label)
XEN_ARGIFY_4(g_set_x_axis_label_w, g_set_x_axis_label)
XEN_ARGIFY_3(g_y_axis_label_w, g_y_axis_label)
XEN_ARGIFY_4(g_set_y_axis_label_w, g_set_y_axis_label)

#else
#define g_x_to_position_w g_x_to_position
#define g_y_to_position_w g_y_to_position
#define g_position_to_x_w g_position_to_x
#define g_position_to_y_w g_position_to_y
#define g_axis_info_w g_axis_info
#define g_draw_axes_w g_draw_axes
#define g_x_axis_label_w g_x_axis_label
#define g_set_x_axis_label_w g_set_x_axis_label
#define g_y_axis_label_w g_y_axis_label
#define g_set_y_axis_label_w g_set_y_axis_label
  
#endif
  
void g_init_axis(void)
{
  XEN_DEFINE_PROCEDURE(S_x_to_position, g_x_to_position_w,   1, 3, 0, H_x_to_position);
  XEN_DEFINE_PROCEDURE(S_y_to_position, g_y_to_position_w,   1, 3, 0, H_y_to_position);
  XEN_DEFINE_PROCEDURE(S_position_to_x, g_position_to_x_w,   1, 3, 0, H_position_to_x);
  XEN_DEFINE_PROCEDURE(S_position_to_y, g_position_to_y_w,   1, 3, 0, H_position_to_y);
  XEN_DEFINE_PROCEDURE(S_axis_info,     g_axis_info_w,       0, 3, 0, H_axis_info);
  XEN_DEFINE_PROCEDURE(S_draw_axes,     g_draw_axes_w,       0, 0, 1, H_draw_axes);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_x_axis_label, g_x_axis_label_w, H_x_axis_label,
					    S_setB S_x_axis_label, g_set_x_axis_label_w, g_set_x_axis_label_reversed, 0, 3, 1, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_y_axis_label, g_y_axis_label_w, H_y_axis_label,
					    S_setB S_y_axis_label, g_set_y_axis_label_w, g_set_y_axis_label_reversed, 0, 3, 1, 3);
  
  XEN_DEFINE_CONSTANT(S_time_graph,      TIME_AXIS_INFO,      "time domain graph axis info");
  XEN_DEFINE_CONSTANT(S_transform_graph, TRANSFORM_AXIS_INFO, "frequency domain graph axis info");
  XEN_DEFINE_CONSTANT(S_lisp_graph,      LISP_AXIS_INFO,      "lisp graph axis info");
}
#endif
/* end no gui (covers entire xen section) */

