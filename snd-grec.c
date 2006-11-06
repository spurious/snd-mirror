#include "snd.h"
#include "snd-rec.h"

/* if user changes vu-size after meters created, we need to make sure the pixmaps are big enough 
 *   (this is probably safe for now because the size can't change) 
 */

typedef struct {
  GtkWidget *meter, *label;
  GdkDrawable *wn;
  int on_off;
  int clipped;
  Float current_val, last_val;
  Float red_deg;
  Float size;
  Float max_val;
  bool dB;
  GdkPixmap *off_label;
  GdkPixmap *on_label;
  GdkPixmap *clip_label;
} vu_t;

typedef struct {
  GtkWidget *label, *number, *slider;
  GtkObject *adj;
  int last_amp, type, in, out;
  int device_in_chan;
} amp_t;

typedef struct {
  int in_chans, out_chans, meters_size, amps_size, active_size, on_buttons_size, in_chan_loc;
  vu_t **meters;         /* has meter widget, can assume frame is local to creator */
  amp_t **amps;          /* chans -- has label number slider amp which_amp -- software input gains to output */ 
  bool *active;         /* which (incoming or file_out outgoing) channels are actively shoveling samples */
  GtkWidget **on_buttons;
  GtkWidget *reset_button;
  GtkWidget *pane;
  GtkWidget *button_vertical_sep;
  int pane_size;
  int device, system;          /* audio.c style device descriptor */
  bool **active_sliders;
  GtkWidget ***matrix_buttons;
  int bx, by;
  int bw, bh;
  GtkWidget *slider_box;
} pane_t;

typedef struct Wdesc {
  int chan, field, device, system;
  int gain;
  pane_t *p;
  GtkWidget *wg;
  GtkObject *adj;
} Wdesc;

static GdkGC *draw_gc, *vu_gc;

static file_data *recdat;

static vu_t **rec_in_VU = NULL;       /* from rec in to associated meter */
static vu_t **rec_out_VU = NULL;      /* from rec out to associated meter */
static Wdesc **gain_sliders = NULL;  /* associated sliders and descriptors for write audio state */
static amp_t ***AMP_rec_ins = NULL;
static amp_t **AMP_rec_outs = NULL;

static GtkWidget *recorder = NULL;      /* the outer dialog shell */

static pane_t **all_panes = NULL;
static int out_file_pane;

static GtkWidget *rec_size_text, *trigger_scale, *trigger_label;
static GtkWidget *file_duration, *messages = NULL, *record_button, *reset_button, *file_text;
static GtkWidget **device_buttons;
static GtkObject *trigger_adj;
static int device_buttons_size = 0;
#if NEW_SGI_AL || MUS_SUN
  static int active_device_button = -1;
#endif
static int mixer_gains_posted[MAX_SOUNDCARDS];
static int tone_controls_posted[MAX_SOUNDCARDS];
static PangoFontDescription *small_font, *mid_font, *large_font;

static void record_report(GtkWidget *text, ...)
{
  va_list ap;
  char *nextstr;
  va_start(ap, text);
  while ((nextstr = va_arg(ap, char *)))
    sg_text_insert(text, nextstr);
  va_end(ap);
}

void recorder_error(const char *msg)
{
  if ((recorder) && (messages))
    record_report(messages, msg, NULL);
}



/* -------------------------------- ICONS -------------------------------- */

static GdkPixmap *speaker_pix, *line_in_pix, *mic_pix, *aes_pix, *adat_pix, *digital_in_pix, *cd_pix;

static void make_record_icons(GtkWidget *w)
{
  GdkWindow *wn;
  wn = MAIN_WINDOW(ss);
  speaker_pix = gdk_pixmap_create_from_xpm_d(wn, NULL, NULL, speaker_bits());
  mic_pix = gdk_pixmap_create_from_xpm_d(wn, NULL, NULL, mic_bits());
  line_in_pix = gdk_pixmap_create_from_xpm_d(wn, NULL, NULL, line_in_bits());
  cd_pix = gdk_pixmap_create_from_xpm_d(wn, NULL, NULL, cd_bits());
}

static GdkPixmap *device_pix(int device)
{
  switch (device)
    {
    case MUS_AUDIO_CD:         return(cd_pix); break;
    case MUS_AUDIO_DIGITAL_OUT:
    case MUS_AUDIO_LINE_OUT:
    case MUS_AUDIO_DEFAULT:
    case MUS_AUDIO_DAC_OUT:
    case MUS_AUDIO_DUPLEX_DEFAULT:
    case MUS_AUDIO_SPEAKERS:   return(speaker_pix); break;
    case MUS_AUDIO_ADAT_OUT:
    case MUS_AUDIO_ADAT_IN:    return(adat_pix); break;
    case MUS_AUDIO_SPDIF_IN:
    case MUS_AUDIO_SPDIF_OUT:
    case MUS_AUDIO_AES_OUT:  
    case MUS_AUDIO_AES_IN:     return(aes_pix); break;
    case MUS_AUDIO_LINE_IN:    return(line_in_pix); break;
    case MUS_AUDIO_DIGITAL_IN: return(digital_in_pix); break;
    case MUS_AUDIO_MICROPHONE: 
    default:                   return(mic_pix); break;
    }
  return(mic_pix); /* make compiler happy */
}

static gboolean recorder_noop_mouse_enter(GtkWidget *w, GdkEventCrossing *ev, gpointer data)
{
  g_signal_stop_emission(GTK_OBJECT(w), g_signal_lookup("enter_notify_event", G_OBJECT_TYPE(GTK_OBJECT(w))), 0);
  return(false);
}


/* -------------------------------- VU METER -------------------------------- */

#define SMALL_FONT "Monospace 8"
#define MID_FONT "Monospace 10"
#define LARGE_FONT "Monospace 12"

#define HEIGHT_OFFSET 16
#define METER_HEIGHT 80
#define METER_TOP 32
#define METER_WIDTH 120

#define VU_OFF 0
#define VU_ON 1
#define VU_CLIPPED 2
#define CLIPPED_TIME 10
#define CLIPPED_TRIGGER 0.99
#define VU_NEEDLE_SPEED 0.25
#define VU_BUBBLE_SPEED 0.025
#define VU_BUBBLE_SIZE (15 * 64)
#define VU_COLORS 11

static GdkColor *yellows[VU_COLORS];
static GdkColor *reds[VU_COLORS];
static bool vu_colors_allocated = false;
static int yellow_vals[] = {0, 16, 32, 64, 96, 128, 160, 175, 185, 200, 210, 220, 230, 240};

static GtkWidget *db_button = NULL;
static void remake_all_vu_meters(void);

void set_vu_in_dB(bool val)
{
  in_set_vu_in_dB(val);
  if (db_button)
    {
      set_toggle_button(db_button, val, false, NULL);   
      remake_all_vu_meters();
    }
}

static void db_button_callback(GtkWidget *w, gpointer context) 
{
  in_set_vu_in_dB(GTK_TOGGLE_BUTTON(w)->active);
  remake_all_vu_meters();
}

static void set_vu_max_label(vu_t *vu)
{
  char buf[64];
  if (!(vu_in_dB(ss)))
    mus_snprintf(buf, 64, "%.3f", vu->max_val);
  else 
    {
      Float dv = 0.0;
      dv = in_dB(min_dB(ss), ss->lin_dB, vu->max_val);
      mus_snprintf(buf, 64, "%ddB", (int)dv);
    }
  gtk_label_set_text(GTK_LABEL(vu->label), buf);
}

static void allocate_meter(vu_t *vu)
{
  GdkDrawable *wn;
  GdkColormap *cmap;
  GdkColor *white, *black, *red;
  Float size;
  int i, j, width, wid2, height, top;

  cmap = gdk_colormap_get_system();
  red = ss->sgx->red;
  wn = recorder->window;
  black = ss->sgx->black;
  white = ss->sgx->white;

  size = vu->size;
  width = (int)(size * METER_WIDTH * 2);
  wid2 = (int)(size * METER_WIDTH);
  height = (int)(size * METER_HEIGHT * 2);
  top = (int)(size * 100);

  /* create the lit-from-below effect in yellow and red */
  if (!vu_colors_allocated)
    {
      GdkColor tmp_color;
      vu_colors_allocated = true;
      tmp_color.red = (unsigned short)65535;
      for (i = 0; i < VU_COLORS; i++)
	{
	  tmp_color.blue = (unsigned short)(256 * yellow_vals[i]);
	  tmp_color.green = (unsigned short)(256 * 230 + 26 * yellow_vals[i]);
	  yellows[i] = gdk_color_copy(&tmp_color);
	  gdk_rgb_find_color(cmap, yellows[i]);
	}
      for (i = 0; i < VU_COLORS; i++)
	{
	  tmp_color.blue = (unsigned short)(128 * yellow_vals[i]);
	  tmp_color.green = (unsigned short)(128 * yellow_vals[i]);
	  reds[i] = gdk_color_copy(&tmp_color);
	  gdk_rgb_find_color(cmap, reds[i]);
	}
    }
  {
    int band, k;
    Float band_x, band_y;

    band_x = 2.75 * size;
    band_y = 3.25 * size;

    for (k = 0; k < 2; k++) 
      {
	GdkPoint pts[16];
	band = 1;
	if (k == 1)
	  {
	    if (!(vu->clip_label))
	      vu->clip_label = gdk_pixmap_new(wn, width, height, -1);
	    gdk_gc_set_foreground(draw_gc, reds[0]);	    
	    gdk_draw_rectangle(vu->clip_label, draw_gc, true, 0, 0, width, height);
	  }
	else 
	  {
	    if (!(vu->on_label))
	      vu->on_label = gdk_pixmap_new(wn, width, height, -1);
	    gdk_gc_set_foreground(draw_gc, yellows[2]);
	    gdk_draw_rectangle(vu->on_label, draw_gc, true, 0, 0, width, height);
	  }
	/* initialize the sequence of nested polygons */
	pts[0].x = (short)(wid2 - band_x);
	pts[0].y = (short)top;
	pts[1].x = pts[0].x;
	pts[1].y = (short)(pts[0].y - band_y + 1);
	pts[2].x = pts[1].x + 1;
	pts[2].y = pts[1].y - 1;
	pts[3].x = (short)(pts[2].x + band_x * 2 - 2);
	pts[3].y = pts[2].y;
	pts[4].x = pts[3].x + 2;
	pts[4].y = pts[3].y + 1;
	pts[5].x = pts[4].x;
	pts[5].y = pts[0].y;
	pts[6].x = pts[0].x;
	pts[6].y = pts[0].y;
	if (k == 1)
	  gdk_draw_polygon(vu->clip_label, draw_gc, true, pts, 7);
	else gdk_draw_polygon(vu->on_label, draw_gc, true, pts, 7);
	
	for (i = 1; i < VU_COLORS; i++)
	  {
	    band += i;
	    if (k == 1) 
	      gdk_gc_set_foreground(draw_gc, reds[i]); 
	    else 
	      {
		if (i < 2) 
		  gdk_gc_set_foreground(draw_gc, yellows[2]); 
		else gdk_gc_set_foreground(draw_gc, yellows[i]);
	      }
	    pts[6].x = (short)(wid2 + band * band_x);
	    pts[6].y = pts[5].y;
	    pts[7].x = pts[6].x;
	    pts[7].y = (short)(top - band * (band_y - 1));
	    pts[8].x = (short)(wid2 + band * (band_x - 1));
	    pts[8].y = (short)(top - band * band_y);
	    pts[9].x = (short)(wid2 - band * (band_x - 1));
	    pts[9].y = pts[8].y;
	    pts[10].x = (short)(wid2 - band * band_x);
	    pts[10].y = (short)(top - band * (band_y - 1));
	    pts[11].x = pts[10].x;
	    pts[11].y = pts[6].y;
	    pts[12].x = pts[0].x;
	    pts[12].y = pts[0].y;
	    if (k == 1)
	      gdk_draw_polygon(vu->clip_label, draw_gc, true, pts, 13);
	    else gdk_draw_polygon(vu->on_label, draw_gc, true, pts, 13);
	    for (j = 0; j < 6; j++) 
	      { 
		/* set up initial portion of next polygon */
		pts[j].x = pts[j + 6].x;
		pts[j].y = pts[j + 6].y;
	      }
	  }
      }
  }

  /* create the 3 labels, draw arcs and ticks */
  if (!(vu->off_label))
    vu->off_label = gdk_pixmap_new(wn, width, height, -1);
  /* not on, so just display a white background */
  gdk_gc_set_foreground(draw_gc, white);
  gdk_draw_rectangle(vu->off_label, draw_gc, true, 0, 0, width, height);
  gdk_gc_set_foreground(draw_gc, black);
  
  {
    int ang0, ang1, major_tick, minor_tick;
    Float sinr, cosr;
    int x0, y0, x1, y1;
    Float rdeg;

    ang0 = 45 * 64;
    ang1 = 90 * 64;
    top = (int)(size * METER_TOP);
    major_tick = (int)(width / 24);
    minor_tick = (int)((width * 0.6) / 24);

    /* x y = coords of upper left corner of the bounding rectangle, not the arc center! */

    gdk_draw_arc(vu->on_label, draw_gc, false, 0, top, width, width, ang0, ang1);
    gdk_draw_arc(vu->on_label, draw_gc, false, 1, top - 1, width - 2, width - 2, ang0, ang1);
    gdk_draw_arc(vu->on_label, draw_gc, false, 2, top - 2, width - 4, width - 4, ang0, ang1);
    gdk_draw_arc(vu->on_label, draw_gc, false, 4, top + 4, width - 8, width - 8, ang0, ang1);

    gdk_draw_arc(vu->off_label, draw_gc, false, 0, top, width, width, ang0, ang1);
    gdk_draw_arc(vu->off_label, draw_gc, false, 1, top - 1, width - 2, width - 2, ang0, ang1);
    gdk_draw_arc(vu->off_label, draw_gc, false, 2, top - 2, width - 4, width - 4, ang0, ang1);
    gdk_draw_arc(vu->off_label, draw_gc, false, 4, top + 4, width - 8, width - 8, ang0, ang1);

    gdk_draw_arc(vu->clip_label, draw_gc, false, 0, top, width, width, ang0, ang1);
    gdk_draw_arc(vu->clip_label, draw_gc, false, 1, top - 1, width - 2, width - 2, ang0, ang1);
    gdk_draw_arc(vu->clip_label, draw_gc, false, 2, top - 2, width - 4, width - 4, ang0, ang1);
    gdk_draw_arc(vu->clip_label, draw_gc, false, 4, top + 4, width - 8, width - 8, ang0, ang1);

    /* draw the axis ticks */
    {
      int major_ticks = 5, minor_ticks = 4;
      Float major_deg, minor_deg;
      if (vu_in_dB(ss))
	{
	  major_ticks = 7;
	  minor_ticks = 1;
	}
      major_deg = 90.0 / (major_ticks - 1); /* 22.5 for linear (one is boundary) */
      minor_deg = 90.0 / ((major_ticks - 1) * (minor_ticks + 1));

      for (i = 0; i < major_ticks; i++)
	{
	  rdeg = mus_degrees_to_radians(45.0 - (i * major_deg));
	  sinr = sin(rdeg);
	  cosr = cos(rdeg);
	  x0 = (int)(wid2 + wid2 * sinr);
	  y0 = (int)(wid2 + top - wid2 * cosr);
	  x1 = (int)(wid2 + (wid2 + major_tick) * sinr);
	  y1 = (int)(wid2 + top - (wid2 + major_tick) * cosr);
	  
	  gdk_draw_line(vu->on_label, draw_gc, x0, y0, x1, y1);
	  gdk_draw_line(vu->on_label, draw_gc, x0 + 1, y0, x1 + 1, y1);
	  gdk_draw_line(vu->off_label, draw_gc, x0, y0, x1, y1);
	  gdk_draw_line(vu->off_label, draw_gc, x0 + 1, y0, x1 + 1, y1);
	  gdk_draw_line(vu->clip_label, draw_gc, x0, y0, x1, y1);
	  gdk_draw_line(vu->clip_label, draw_gc, x0 + 1, y0, x1 + 1, y1);
	  
	  if (i < (major_ticks - 1))
	    for (j = 1; j <= minor_ticks; j++)
	      {
		rdeg = mus_degrees_to_radians(45.0 - (i * major_deg) - (j * minor_deg));
		sinr = sin(rdeg);
		cosr = cos(rdeg);
		x0 = (int)(wid2 + wid2 * sinr);
		y0 = (int)(wid2 + top - wid2 * cosr);
		x1 = (int)(wid2 + (wid2 + minor_tick) * sinr);
		y1 = (int)(wid2 + top - (wid2 + minor_tick) * cosr);
		gdk_draw_line(vu->on_label, draw_gc, x0, y0, x1, y1);
		gdk_draw_line(vu->off_label, draw_gc, x0, y0, x1, y1);
		gdk_draw_line(vu->clip_label, draw_gc, x0, y0, x1, y1);
	    }
	}
    }
  }
}

static void display_vu_meter(vu_t *vu)
{
  GdkPixmap *label = 0;
  recorder_info *rp;

  if (!vu) return;
  rp = get_recorder_info();

  if (vu->current_val > CLIPPED_TRIGGER) 
    {
      if (vu->on_off == VU_ON) 
	{
	  vu->on_off = VU_CLIPPED;
	  vu->clipped = (int)(CLIPPED_TIME * ((Float)(rp->srate) / 22050.0));
	  /* might also change with record buffer size (recorder_buffer_size below)
	   * at 4096, we're getting updated here every 1024 samps (4 chans in always)
	   * which is fast enough to look smooth, except perhaps at 8kHz?
	   * so clipped_time == 10 gives us about a .5 sec flash
	   */
	}
    }
  if (vu->current_val > 1.0) vu->current_val = 1.0;
  switch (vu->on_off)
    {
    case VU_OFF: 
      label = vu->off_label; 
      break;
    case VU_CLIPPED: 
      vu->clipped--; 
      if (vu->clipped <= 0) 
	{
	  label = vu->on_label; 
	  vu->on_off = VU_ON;
	} 
      else 
	{
	  label = vu->clip_label; 
	}
      break;
    case VU_ON: 
      label = vu->on_label; 
      break;
    }

  {
    Float size, rdeg, val;
    int redx, redy, height_offset;
    state_context *sx;
    int major_tick;
    Float sinr, cosr;
    int x0, y0, x1, y1;
    int width, wid2, height, top;

    size = vu->size;
    sx = ss->sgx;
    height_offset = (int)(HEIGHT_OFFSET * size);
    width = (int)(size * METER_WIDTH * 2);
    wid2 = (int)(size * METER_WIDTH);
    height = (int)(size * METER_HEIGHT * 2);
    top = (int)(size * METER_TOP);
    major_tick = (int)(width / 24);

    if (label) gdk_draw_drawable(vu->wn, vu_gc, label, 0, 0, 0, -height_offset, width, height);

    val = vu->current_val * VU_NEEDLE_SPEED + (vu->last_val * (1.0 - VU_NEEDLE_SPEED));
    vu->last_val = val;
    rdeg = mus_degrees_to_radians(val * 90.0 - 45.0);
    sinr = sin(rdeg);
    cosr = cos(rdeg);

    x0 = wid2;
    y0 = height;
    x1 = (int)(wid2 + (wid2 + major_tick) * sinr);
    y1 = (int)(wid2 + top - height_offset - (wid2 + major_tick) * cosr);

    gdk_gc_set_foreground(vu_gc, sx->black);
    gdk_draw_line(vu->wn, vu_gc, x0, y0, x1, y1);

    if (vu->on_off != VU_OFF)
      {
	if (vu->current_val > vu->red_deg) 
	  vu->red_deg = vu->current_val;
	else vu->red_deg = vu->current_val * VU_BUBBLE_SPEED + (vu->red_deg * (1.0 - VU_BUBBLE_SPEED));
	gdk_gc_set_foreground(vu_gc, sx->red);

	redx = (int)(vu->red_deg * 90 * 64);
	if (redx<(VU_BUBBLE_SIZE)) 
	  redy = redx; 
	else redy = VU_BUBBLE_SIZE;

	gdk_draw_arc(vu->wn, vu_gc, false, 3, top + 0 - height_offset, width - 6, width - 6, 135 * 64 - redx, redy);
	gdk_draw_arc(vu->wn, vu_gc, false, 3, top + 1 - height_offset, width - 6, width - 6, 135 * 64 - redx, redy);
	gdk_draw_arc(vu->wn, vu_gc, false, 3, top + 2 - height_offset, width - 6, width - 6, 135 * 64 - redx, redy);
	gdk_draw_arc(vu->wn, vu_gc, false, 3, top + 3 - height_offset, width - 6, width - 6, 135 * 64 - redx, redy);

	gdk_gc_set_foreground(vu_gc, sx->black);
      }
  }
}

static void remake_all_vu_meters(void)
{
  pane_t *p;
  vu_t *vu;
  recorder_info *rp;
  int i, k;
  rp = get_recorder_info();
  for (i = 0; i < rp->ordered_devices_size; i++)
    {
      p = all_panes[i];
      for (k = 0; k < p->meters_size; k++)
	{
	  vu = p->meters[k];
	  if (vu->dB != vu_in_dB(ss))
	    {
	      vu->dB = vu_in_dB(ss);
	      vu->max_val = 0.0; /* force button update */
#if 0
	      if (vu->on_label) {XFreePixmap(dp, vu->on_label); vu->on_label = None;}
	      if (vu->off_label) {XFreePixmap(dp, vu->off_label); vu->off_label = None;}
	      if (vu->clip_label) {XFreePixmap(dp, vu->clip_label); vu->clip_label = None;}
#endif
	      allocate_meter(vu);
	      display_vu_meter(vu);
	    }
	}
    }
}

static gboolean meter_expose_callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  display_vu_meter((vu_t *)data);
  return(false);
}

static gboolean meter_resize_callback(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  display_vu_meter((vu_t *)data);
  return(false);
}

static vu_t *make_vu_meter(GtkWidget *meter, GtkWidget *label, Float size)
{
  vu_t *vu;
  vu = (vu_t *)CALLOC(1, sizeof(vu_t));
  vu->meter = meter;
  vu->label = label;
  vu->size = size;
  vu->wn = meter->window;
  vu->on_off = VU_OFF;
  vu->current_val = 0.0;
  vu->last_val = 0.0;
  vu->max_val = 0.0;
  vu->clipped = 0;
  vu->dB = vu_in_dB(ss);
  allocate_meter(vu);
  return(vu);
}

static void set_vu_val(vu_t *vu, Float val) 
{
  if (!vu) return;
  vu->last_val = vu->current_val;
  if (!(vu_in_dB(ss)))
    vu->current_val = val;
  else
    {
      Float dv = 0.0;
      dv = in_dB(min_dB(ss), ss->lin_dB, val);
      vu->current_val = 1.0 +  ((dv < -30.0) ? -30.0 : dv) / 30.0;
    }
  display_vu_meter(vu);
  if (val > vu->max_val)
    {
      vu->max_val = val;
      set_vu_max_label(vu);
    }
}

void recorder_set_vu_in_val(int chan, mus_sample_t val) {set_vu_val(rec_in_VU[chan], MUS_SAMPLE_TO_FLOAT(val));}
void recorder_set_vu_out_val(int chan, mus_sample_t val) {set_vu_val(rec_out_VU[chan], MUS_SAMPLE_TO_FLOAT(val));}


/* -------------------------------- AMP SLIDER CALLBACKS -------------------------------- */

#define INPUT_AMP 0
#define OUTPUT_AMP 1

static Float scroll_to_amp(Float val)
{
  if (val <= 0.0) 
    return(amp_control_min(ss));
  if (val >= 0.9) 
    return(amp_control_max(ss));
  if (val > (0.5 * 0.9))
    return((((val / (0.5 * 0.9)) - 1.0) * (amp_control_max(ss) - 1.0)) + 1.0);
  else return((val * (1.0 - amp_control_min(ss)) / (0.5 * 0.9)) + amp_control_min(ss));
}

static void record_amp_changed(amp_t *ap, Float scrollval)
{
  #define NUMBER_SIZE 8
  char sfs[NUMBER_SIZE];
  Float amp;
  recorder_info *rp;
  rp = get_recorder_info();
  amp = scroll_to_amp(scrollval);
  mus_snprintf(sfs, NUMBER_SIZE, " %.2f ", amp);
  set_button_label(ap->number, sfs);
  if (ap->type == INPUT_AMP)
    rp->in_amps[ap->in][ap->out] = amp;
  else rp->out_amps[ap->out] = amp;
}

static Float amp_to_slider(Float amp) {return(amp_to_scroll(amp_control_min(ss), amp, amp_control_max(ss)));}

static Float global_amp(amp_t *a)
{
  recorder_info *rp;
  rp = get_recorder_info();
  if (a->type == INPUT_AMP)
    return(rp->in_amps[a->in][a->out]);
  else return(rp->out_amps[a->out]);
}

static bool ignore_callback = false;

static gboolean record_amp_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  amp_t *ap = (amp_t *)data;
  Float val;
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    val = ap->last_amp; 
  else val = amp_to_slider(1.0);
  record_amp_changed(ap, val);
  GTK_ADJUSTMENT(ap->adj)->value = val;
  gtk_adjustment_value_changed(GTK_ADJUSTMENT(ap->adj));
  return(false);
}

static void record_amp_drag_callback(GtkAdjustment *adj, gpointer data)
{
  if (ignore_callback) return;
  record_amp_changed((amp_t *)data, adj->value);
}


/* ---------------- FILE INFO PANE ---------------- */

static void help_record_callback(GtkWidget *w, gpointer context) 
{
  recording_help();
}

static void internal_trigger_set(Float val)
{
  recorder_info *rp;
  rp = get_recorder_info();
  rp->trigger = val;
  rp->triggering = (val > 0.0);
  rp->triggered = (!rp->triggering);
  if (!(rp->recording)) /* else wait for current session to end (via click) */
    set_stock_button_label(record_button, (rp->triggering) ? _("Triggered Record") : _("Record"));
}

static void change_trigger_callback(GtkAdjustment *adj, gpointer context)
{
  /* if val = 0, set record button normal label to 'Record', else 'Triggered Record' */
  recorder_info *rp;
  rp = get_recorder_info();
  rp->trigger = adj->value;
  internal_trigger_set(adj->value);
}

static void device_button_callback(GtkWidget *w, gpointer context) 
{
#if NEW_SGI_AL || MUS_SUN
  pane_t *p = (pane_t *)context;
#endif
  int button;
  bool on;
#if MUS_SGI || MUS_SUN
  int other_button, j, n, i;
  bool output;
  float val[2];
  recorder_info *rp;
  rp = get_recorder_info();
#endif

#if MUS_SGI || MUS_SUN
  output = 0;
#endif

  button = get_user_int_data(G_OBJECT(w));
  on = (bool)GTK_TOGGLE_BUTTON(w)->active;

#if OLD_SGI_AL
  /* on the older SGI's (and maybe newer Indy's?) digital input disables mic/line-in and vice versa */
  if (button == rp->digital_in_button)
    {
      set_line_source(on);
      if (on == (bool)(GTK_TOGGLE_BUTTON(device_buttons[rp->microphone_button])->active))
	set_toggle_button(device_buttons[rp->microphone_button], !on, true, (void *)(all_panes[rp->microphone_button])); 
      if (on == (bool)GTK_TOGGLE_BUTTON(device_buttons[rp->line_in_button])->active)
	set_toggle_button(device_buttons[rp->line_in_button], !on, true, (void *)(all_panes[rp->line_in_button])); 
    }
  else
    {
      if (button == rp->microphone_button)
	{
	  if (on == (bool)(GTK_TOGGLE_BUTTON(device_buttons[rp->digital_in_button])->active))
	    {
	      set_toggle_button(device_buttons[rp->digital_in_button], !on, true, (void *)(all_panes[rp->digital_in_button])); 
	      if (!(on == (bool)(GTK_TOGGLE_BUTTON(device_buttons[rp->line_in_button])->active)))
		set_toggle_button(device_buttons[rp->line_in_button], on, true, (void *)(all_panes[rp->line_in_button])); 
	    }
	}
    }
#endif
#if NEW_SGI_AL || MUS_SUN
  output = (!(recorder_input_device(p->device)));
  if (!output)
    {
      if (on)
	{
  	  if (active_device_button != -1)
	    {
	      p = all_panes[active_device_button];
	      for (i = p->in_chan_loc, j = 0; j < p->in_chans; j++, i++) rp->input_channel_active[i] = false;
	      if (active_device_button != button)
		set_toggle_button(device_buttons[active_device_button], false, false, (void *)(all_panes[active_device_button])); 
	      if (rp->taking_input) close_recorder_audio();
	    }
	  active_device_button = button;
	  p = all_panes[button];
	  for (i = p->in_chan_loc, j = 0; j < p->in_chans; j++, i++) rp->input_channel_active[i] = true;
	  rp->input_channels[0] = p->in_chans;

	  /* if digital in, get its srate (and reflect in srate text) */
	  if ((p->device == MUS_AUDIO_AES_IN) || (p->device == MUS_AUDIO_ADAT_IN))
	    {
	      mus_audio_mixer_read(p->device, MUS_AUDIO_SRATE, 0, val);
	      set_recorder_srate(rp, (int)val[0]);
	    }
	  rp->input_ports[0] = mus_audio_open_input(MUS_AUDIO_PACK_SYSTEM(0) | p->device,
						    rp->srate, rp->input_channels[0], rp->in_format, rp->buffer_size);
	  if (rp->input_ports[0] == -1)
	    record_report(messages, recorder_device_name(p->device), NULL);
	  else
	    {
	      rp->taking_input = true;
	      set_read_in_progress();
	    }
	}
  #ifndef MUS_SUN
      else
	{
	  if (on)
	    {
	      if (!rp->monitoring)
		{
		  rp->monitor_port = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_DAC_OUT,
							   rp->srate, rp->hd_audio_out_chans, rp->output_data_format, rp->buffer_size);
		  if (rp->monitor_port == -1)
		    {
		      record_report(messages, _("open output"), NULL);
		      rp->monitoring = false;
		    }
		  else rp->monitoring = true;
		}
	    }
	  else 
	    {
	      mus_audio_close(rp->monitor_port);
	      rp->monitoring = false;
	      rp->monitor_port = -1;
	    }
	}
  #endif
    }
#endif
}

static void autoload_file_callback(GtkWidget *w, gpointer context) 
{
  recorder_info *rp;
  rp = get_recorder_info();
  rp->autoload = GTK_TOGGLE_BUTTON(w)->active;
}

static void post_error_in_message_pane(const char *error_msg, void *data)
{
  recorder_error(error_msg);
  /* no need for clearing mechanism since the message pane is just a list of on-going messages */
}

static void rec_size_changed_callback(GtkWidget *w, gpointer context) 
{
  char *str;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(rec_size_text));  /* w here gets segfault!! */
  if (str) 
    {
      int n;
      recorder_info *rp;
      rp = get_recorder_info();
      redirect_snd_error_to(post_error_in_message_pane, NULL);
      n = string_to_int(str, 1, "buffer size");
      redirect_snd_error_to(NULL, NULL);
      if ((n > 0) && (n != rp->buffer_size)) set_record_size(n);
    }
}

static void make_file_info_pane(recorder_info *rp, GtkWidget *file_pane, int ndevs)
{
  int i;
  char *name;
  GtkWidget *file_label, *file_form, *duration_label, *rec_size_label, *ff_sep1, *ff_sep2, *autoload_file;
  GtkWidget *left_form, *right_form, *durbox, *triggerbox, *frame;
#if MUS_SGI || MUS_SUN
  float val[1];
  int err;
#endif

  /* file_pane is the outer frame widget */

  file_form = gtk_hbox_new(false, 0);
  gtk_container_add(GTK_CONTAINER(file_pane), file_form);
  gtk_widget_show(file_form);

  left_form = gtk_vbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(file_form), left_form, true, true, 0);
  gtk_widget_show(left_form);

  ff_sep1 = gtk_vseparator_new();
  gtk_box_pack_start(GTK_BOX(file_form), ff_sep1, false, false, 10);
  gtk_widget_show(ff_sep1);

  right_form = gtk_vbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(file_form), right_form, true, true, 0);
  gtk_widget_show(right_form);


  file_label = snd_gtk_highlight_label_new(_("output file:"));
  gtk_box_pack_start(GTK_BOX(left_form), file_label, false, false, 0);
  gtk_widget_show(file_label);

  file_text = snd_entry_new(left_form, WITH_WHITE_BACKGROUND);
  if (rp->output_file)
    gtk_entry_set_text(GTK_ENTRY(file_text), rp->output_file);


  recdat = make_file_data_panel(left_form, "data-form", 
				WITH_CHANNELS_FIELD, 
				rp->output_header_type, 
				rp->output_data_format, 
				WITHOUT_DATA_LOCATION_FIELD, 
				WITHOUT_SAMPLES_FIELD, 
				WITHOUT_ERROR_FIELD, 
				WITH_HEADER_TYPE_FIELD, 
				WITHOUT_COMMENT_FIELD,
				WITH_BUILTIN_HEADERS);

#if MUS_SGI
  err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_MICROPHONE, MUS_AUDIO_SRATE, 0, val);
  if (err == MUS_NO_ERROR) rp->srate = val[0];
#endif
  widget_int_to_text(recdat->srate_text, rp->srate);
  widget_int_to_text(recdat->chans_text, rp->out_chans);


  durbox = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(right_form), durbox, false, false, 0);
  gtk_widget_show(durbox);

  duration_label = gtk_label_new(_("duration:"));
  gtk_box_pack_start(GTK_BOX(durbox), duration_label, false, false, 8);
  gtk_widget_show(duration_label);

  file_duration = gtk_label_new("0.000");
  gtk_box_pack_start(GTK_BOX(durbox), file_duration, true, true, 0);
  gtk_widget_show(file_duration);
  
  rec_size_label = gtk_label_new(_("buf:"));
  gtk_box_pack_start(GTK_BOX(durbox), rec_size_label, false, false, 8);
  gtk_widget_show(rec_size_label);

  rec_size_text = snd_entry_new(durbox, WITH_WHITE_BACKGROUND);
  SG_SIGNAL_CONNECT(rec_size_text,  "activate", rec_size_changed_callback, NULL);
  widget_int_to_text(rec_size_text, rp->buffer_size);

  ff_sep2 = gtk_hseparator_new();
  gtk_box_pack_start(GTK_BOX(right_form), ff_sep2, false, false, 8);
  gtk_widget_show(ff_sep2);

  /* buttons */
  
  for (i = 0; i < ndevs; i++)
    {
      if ((rp->systems == 1) || (!(recorder_input_device(rp->ordered_devices[i]))))
	name = recorder_device_name(rp->ordered_devices[i]);
      else name = recorder_system_and_device_name(rp->ordered_systems[i], rp->ordered_devices[i]);
      device_buttons[i] = gtk_check_button_new_with_label(name);
      gtk_box_pack_start(GTK_BOX(right_form), device_buttons[i], false, false, 0);
      gtk_widget_show(device_buttons[i]);
      set_user_int_data(G_OBJECT(device_buttons[i]), i);
      SG_SIGNAL_CONNECT(device_buttons[i], "toggled", device_button_callback, all_panes[i]);
      set_toggle_button(device_buttons[i], true, false, (void *)(all_panes[i]));
    }

  autoload_file = gtk_check_button_new_with_label(_("Autoload Recording"));
  gtk_box_pack_start(GTK_BOX(right_form), autoload_file, false, false, 0);
  gtk_widget_show(autoload_file);
  device_buttons[ndevs] = autoload_file;
  /* we assume this is last in the device_buttons list in sensitize_control_buttons */
  rp->autoload_button = ndevs;
  SG_SIGNAL_CONNECT(autoload_file, "toggled", autoload_file_callback, NULL);
  set_toggle_button(autoload_file, rp->autoload, false, NULL); 

  db_button = gtk_check_button_new_with_label(_("VU meters in dB"));
  gtk_box_pack_start(GTK_BOX(right_form), db_button, false, false, 0);
  gtk_widget_show(db_button);
  SG_SIGNAL_CONNECT(db_button, "toggled", db_button_callback, NULL);
  set_toggle_button(db_button, vu_in_dB(ss), false, NULL); 

  triggerbox = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(right_form), triggerbox, true, true, 10);
  gtk_widget_show(triggerbox);

  trigger_label = gtk_label_new(_("trigger:"));
  gtk_box_pack_start(GTK_BOX(triggerbox), trigger_label, false, false, 4);
  gtk_widget_show(trigger_label);

  trigger_adj = gtk_adjustment_new(0.0, 0.0, 1.01, .01, .1, .01);
  trigger_scale = gtk_hscale_new(GTK_ADJUSTMENT(trigger_adj));
  gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(trigger_scale)), GTK_UPDATE_CONTINUOUS);
  gtk_scale_set_digits(GTK_SCALE(trigger_scale), 3);
  gtk_scale_set_draw_value(GTK_SCALE(trigger_scale), true);
  gtk_scale_set_value_pos(GTK_SCALE(trigger_scale), GTK_POS_LEFT);
  gtk_box_pack_start(GTK_BOX(triggerbox), trigger_scale, true, true, 0);
  SG_SIGNAL_CONNECT(trigger_adj, "value_changed", change_trigger_callback, NULL);
  gtk_widget_show(trigger_scale);

  frame = gtk_frame_new(NULL);
  gtk_box_pack_start(GTK_BOX(right_form), frame, true, true, 4);  
  gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_IN);
  gtk_widget_show(frame);
  messages = make_scrolled_text(frame, true, NULL, false);
}

void reflect_recorder_duration(Float new_dur)
{
  char buf[64];
  mus_snprintf(buf, 64, "%.2f", new_dur);
  set_label(file_duration, buf);
}

void lock_recording_audio(void)
{
  if ((recorder) && (GTK_WIDGET_VISIBLE(recorder)))
    {
      close_recorder_audio();
      set_sensitive(record_button, false);
      set_sensitive(reset_button, false);
    }
}

void unlock_recording_audio(void)
{
  if ((recorder) && (GTK_WIDGET_VISIBLE(recorder)))
    {
      set_sensitive(record_button, true);
      set_sensitive(reset_button, true);
      set_stock_button_label(reset_button, _("Restart"));
    }
}


/* -------------------------------- DEVICE PANE -------------------------------- */


static void vu_reset_callback(GtkWidget *w, gpointer context) 
{
  int i;
  pane_t *p = (pane_t *)context;
  for (i = 0; i < p->meters_size; i++)
    {
      vu_t *vu;
      vu = p->meters[i];
      vu->max_val = 0.0;
      set_vu_max_label(vu);
    }
}

static void meter_button_callback(GtkWidget *w, gpointer context) 
{
  Wdesc *wd = (Wdesc *)context;
  vu_t *vu;
  bool on;
  pane_t *p;
  recorder_info *rp;
  rp = get_recorder_info();
  p = wd->p;
  vu = p->meters[wd->chan];
  if (vu->on_off == VU_OFF)
    {
      gtk_widget_modify_bg(w, GTK_STATE_NORMAL, ss->sgx->red);
      vu->on_off = VU_ON;
      vu->red_deg = 0.0;
    }
  else 
    {
      gtk_widget_modify_bg(w, GTK_STATE_NORMAL, ss->sgx->basic_color);
      vu->on_off = VU_OFF;
    }
  display_vu_meter(vu);
  on = (vu->on_off == VU_ON);
  p->active[wd->chan] = on;
  if (recorder_output_device(p->device))
    {
      char *str;
      int val = 0;
      int i, n;
      rp->chan_out_active[wd->chan] = on;
      str = (char *)gtk_entry_get_text(GTK_ENTRY(recdat->chans_text)); 
      if (str) 
	{
	  redirect_snd_error_to(post_error_in_message_pane, NULL);
	  n = string_to_int(str, 1, "chans");
	  redirect_snd_error_to(NULL, NULL);
	}
      else n = 0;
      for (i = 0; i < p->active_size; i++) 
	if (p->active[i]) 
	  val++;
      if ((val > 0) && (val != n))
	widget_int_to_text(recdat->chans_text, val);
    }
  else rp->chan_in_active[wd->gain] = on;
}

static void volume_callback(GtkAdjustment *adj, gpointer context) 
{
  Wdesc *wd = (Wdesc *)context;
  set_mixer_gain(wd->system, wd->device, wd->chan, wd->gain, wd->field, 1.0 - adj->value);
}

/* ---- slider button matrix ---- */

static void make_recorder_slider(pane_t *p, amp_t *a, bool input)
{
  GtkWidget *hb;
  #define NUMBER_SIZE 8
  char numbuf[NUMBER_SIZE];

  hb = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(p->slider_box), hb, false, false, 0);
  gtk_widget_show(hb);

  /* the extra vertical padding is caused by these labels, not by the scrollbar */
  a->label = gtk_button_new_with_label(gain_channel_name(p->in_chans, p->out_chans, input, a->device_in_chan, a->out));
  gtk_button_set_relief(GTK_BUTTON(a->label), GTK_RELIEF_NONE);
  gtk_widget_set_name(a->label, "label_button");
  gtk_box_pack_start(GTK_BOX(hb), a->label, false, false, 0);

  gtk_widget_show(a->label);
  SG_SIGNAL_CONNECT(a->label, "button_press_event", record_amp_click_callback, a);
  SG_SIGNAL_CONNECT(a->label, "enter_notify_event", recorder_noop_mouse_enter, NULL);

  mus_snprintf(numbuf, NUMBER_SIZE, " %.2f ", global_amp(a));
  a->number = gtk_button_new_with_label(numbuf);
  gtk_button_set_relief(GTK_BUTTON(a->number), GTK_RELIEF_NONE);
  gtk_widget_set_name(a->number, "label_button");
  gtk_box_pack_start(GTK_BOX(hb), a->number, false, false, 0);
  gtk_widget_show(a->number);
  SG_SIGNAL_CONNECT(a->number, "button_press_event", record_amp_click_callback, a);
  SG_SIGNAL_CONNECT(a->number, "enter_notify_event", recorder_noop_mouse_enter, NULL);

  a->adj = gtk_adjustment_new(amp_to_slider(global_amp(a)), 0.0, 1.00, 0.001, 0.01, .1);
  a->slider = gtk_hscrollbar_new(GTK_ADJUSTMENT(a->adj));
  gtk_box_pack_start(GTK_BOX(hb), a->slider, true, true, 0); /* 6? */
  SG_SIGNAL_CONNECT(a->adj, "value_changed", record_amp_drag_callback, a);
  gtk_widget_show(a->slider);

}

static void handle_matrix_slider(GtkWidget *mb, pane_t *p, int bin, int bout, int curamp, bool remove)
{
  amp_t *a;
  a = p->amps[curamp];
  if (remove)
    {
      gtk_widget_modify_bg(mb, GTK_STATE_NORMAL, ss->sgx->basic_color);
      p->active_sliders[bin][bout] = false;
      gtk_widget_hide(a->label);
      gtk_widget_hide(a->number);
      gtk_widget_hide(a->slider);
    }
  else
    {
      gtk_widget_modify_bg(mb, GTK_STATE_NORMAL, ss->sgx->green);
      p->active_sliders[bin][bout] = true;
      if (a->label)
	{
	  gtk_widget_show(a->label);
	  gtk_widget_show(a->number);
	  gtk_widget_show(a->slider);
	}
      else
	make_recorder_slider(p, a, true);
    }
}

typedef struct {
  pane_t *p;
  amp_t *a;
  int in_chan, out_chan;
} slider_info;

static void matrix_button_callback(GtkWidget *mb, gpointer context) 
{
  slider_info *si = (slider_info *)context;
  pane_t *p;
  int curamp;
  p = si->p;
  curamp = si->out_chan * p->in_chans + si->in_chan;
  handle_matrix_slider(mb, p, si->in_chan, si->out_chan, curamp, p->active_sliders[si->in_chan][si->out_chan]);
}

static GtkWidget *make_button_matrix(recorder_info *rp, pane_t *p, char *name, GtkWidget *parent, Float meter_size)
{
  GtkWidget *outer_frame, *outer_vbox, *outer_hbox, *top_hbox, *left_vbox, *buttons;
  int ins = 2, outs = 2, row, col, vu_rows;
  int width, height;
  GtkWidget *outputs_label, *inputs_label0, *inputs_label1, *inputs_label2, *diag_button;
  bool **active_sliders;

  active_sliders = p->active_sliders;
  vu_rows = p->in_chans / 4;
  if (vu_rows == 0) vu_rows = 1;
  height = (int)(vu_rows*(3 * 2 + 100 * meter_size));
  width = height;

  outer_frame = gtk_frame_new(NULL);
  gtk_box_pack_start(GTK_BOX(parent), outer_frame, false, false, 0);
  gtk_widget_show(outer_frame);

  outer_vbox = gtk_vbox_new(false, 0);
  gtk_container_add(GTK_CONTAINER(outer_frame), outer_vbox);
  gtk_widget_show(outer_vbox);

  top_hbox = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(outer_vbox), top_hbox, false, false, 0);
  gtk_widget_show(top_hbox);

  outer_hbox = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(outer_vbox), outer_hbox, true, true, 0);
  gtk_widget_show(outer_hbox);

  left_vbox = gtk_vbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(outer_hbox), left_vbox, false, false, 0);
  gtk_widget_show(left_vbox);

  buttons = gtk_table_new(ins, outs, true);
  gtk_box_pack_start(GTK_BOX(outer_hbox), buttons, true, true, 0);
  gtk_widget_show(buttons);

  diag_button = gtk_label_new("/ ");
  gtk_box_pack_start(GTK_BOX(top_hbox), diag_button, false, false, 0);
  gtk_widget_show(diag_button);

  outputs_label = gtk_label_new(_("out"));
  gtk_box_pack_start(GTK_BOX(top_hbox), outputs_label, true, true, 0);
  gtk_widget_show(outputs_label);

  inputs_label0 = gtk_label_new(_("i"));
  gtk_box_pack_start(GTK_BOX(left_vbox), inputs_label0, false, false, 0);
  gtk_widget_show(inputs_label0);

  inputs_label1 = gtk_label_new(_("n"));
  gtk_box_pack_start(GTK_BOX(left_vbox), inputs_label1, false, false, 0);
  gtk_widget_show(inputs_label1);

  inputs_label2 = gtk_label_new(" ");
  gtk_box_pack_start(GTK_BOX(left_vbox), inputs_label2, false, false, 0);
  gtk_widget_show(inputs_label2);

  ins = p->in_chans;
  outs = p->out_chans;
  if ((outs > rp->out_chans) && (rp->out_chans > 0)) outs = rp->out_chans;
  p->matrix_buttons = (GtkWidget ***)CALLOC(ins, sizeof(GtkWidget **));
  for (row = 0; row < ins; row++) p->matrix_buttons[row] = (GtkWidget **)CALLOC(outs, sizeof(GtkWidget *));

  for (col = 0; col < outs; col++)
    for (row = 0; row < ins; row++)
      {
	slider_info *si;
	GtkWidget *mb;
	si = (slider_info *)CALLOC(1, sizeof(slider_info));
	si->p = p;
	si->in_chan = row;
	si->out_chan = col;

	mb = gtk_button_new();
	if (row == col)
	  gtk_widget_modify_bg(mb, GTK_STATE_NORMAL, ss->sgx->green);
	else gtk_widget_modify_bg(mb, GTK_STATE_NORMAL, ss->sgx->basic_color);
	gtk_table_attach_defaults(GTK_TABLE(buttons), mb, col, col + 1, row, row + 1);
	gtk_widget_show(mb);
	SG_SIGNAL_CONNECT(mb, "clicked", matrix_button_callback, si);
	p->matrix_buttons[row][col] = mb;
      }
  return(outer_frame);
}

/* -------- I/O pane -------- */

void recorder_fill_wd(struct Wdesc *wd, int chan, int field, int device)
{
  wd->chan = chan;
  wd->field = field;
  wd->device = device;
}

static void make_vu_meters(pane_t *p, int vu_meters,
			   int overall_input_ctr, Float meter_size, bool input, GtkWidget *vuh)
{
  int i, columns, row, rows;
  GtkWidget **hboxes, *vbox;
  vbox = gtk_vbox_new(true, 0);
  gtk_box_pack_start(GTK_BOX(vuh), vbox, input, input, 0);
  gtk_widget_show(vbox);
  /* need secondary set of boxes if vu_meters > 4 */
  columns = recorder_columns(vu_meters);
  rows = vu_meters / columns;
  hboxes = (GtkWidget **)CALLOC(rows, sizeof(GtkWidget *));
  for (i = 0; i < rows; i++) 
    {
      hboxes[i] = gtk_hbox_new(true, 0);
      gtk_box_pack_start(GTK_BOX(vbox), hboxes[i], true, true, 0);
      gtk_widget_show(hboxes[i]);
    }
  row = 0;
  for (i = 0; i < vu_meters; i++)
    {
      GtkWidget *frame, *label, *meter;
      vu_t *vu;

      label = gtk_label_new("0.000");
      if (vu_size(ss) <= 1.0)
	gtk_widget_modify_font(label, small_font);
      else
	{
	  if (vu_size(ss) <= 1.5)
	    gtk_widget_modify_font(label, mid_font);
	  else gtk_widget_modify_font(label, large_font);
	}

      frame = gtk_frame_new(NULL);
      gtk_widget_set_name(frame, "record_frame");

      gtk_box_pack_start(GTK_BOX(hboxes[row]), frame, false, false, 0);
      gtk_container_set_border_width(GTK_CONTAINER(frame), 2);
      gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_ETCHED_IN);
      gtk_widget_modify_bg(frame, GTK_STATE_NORMAL, ss->sgx->black);
      gtk_widget_set_size_request(frame, (int)(METER_WIDTH * 2 * meter_size), (int)(METER_HEIGHT * meter_size) + 12); /* 12 for meter frame */
      gtk_frame_set_label_widget(GTK_FRAME(frame), label);
      gtk_frame_set_label_align(GTK_FRAME(frame), 0.0, 1.0);
      gtk_widget_show(label);
      gtk_widget_show(frame);

      meter = gtk_drawing_area_new();
      gtk_container_add(GTK_CONTAINER(frame), meter);
      gtk_widget_show(meter);

      p->meters[i] = make_vu_meter(meter, label, meter_size);
      vu = p->meters[i];
      vu->max_val = 0.0;
      if (input)
	rec_in_VU[overall_input_ctr + i] = vu;
      else rec_out_VU[i] = vu;

      SG_SIGNAL_CONNECT(meter, "expose_event", meter_expose_callback, vu);
      SG_SIGNAL_CONNECT(meter, "configure_event", meter_resize_callback, vu);

      if ((i == (columns * (row + 1) - 1)) && 
	  (vu_meters > (i + 1))) 
	row++;

    }
  FREE(hboxes);
}

#if (HAVE_OSS || HAVE_ALSA)
static gboolean spix_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  Wdesc *wd = (Wdesc *)data;
  gdk_draw_drawable(GDK_DRAWABLE(w->window), ss->sgx->basic_gc, device_pix(wd->device), 0, 0, 2, 4, 12, 12);
  return(false);
}
#endif

static void make_vertical_gain_sliders(recorder_info *rp, pane_t *p, 
				       int num_gains, int gain_ctr, int *mixflds, bool input, GtkWidget *gv)
{
  int i, chan, this_device = 0, last_device = -1;
  for (i = 0, chan = num_gains - 1; i < num_gains; i++, chan--)
    {
      GtkWidget *sbox;
      Float vol;
      Wdesc *wd;
      wd = (Wdesc *)CALLOC(1, sizeof(Wdesc));
      wd->system = p->system;
      this_device = recorder_sort_mixer_device(wd, i, chan, input, p->device, mixflds);
      wd->p = p;
      wd->gain = gain_ctr + chan;
      if (wd->gain > rp->num_mixer_gains) 
	snd_error("gain (slider) number too high: %d > %d", 
		  wd->gain, rp->num_mixer_gains);
      gain_sliders[wd->gain] = wd;
      vol = mixer_gain(wd->system, wd->device, wd->chan, wd->gain, wd->field);
      if (vol < 0.0) vol = 0.0;
      if (vol > 1.0) vol = 1.0;
      
      sbox = gtk_vbox_new(false, 0);
      gtk_box_pack_end(GTK_BOX(gv), sbox, false, false, 0);
      gtk_widget_show(sbox);

#if (HAVE_OSS || HAVE_ALSA)
      if ((wd->device == MUS_AUDIO_MICROPHONE) ||
	  (wd->device == MUS_AUDIO_LINE_IN) ||
	  (wd->device == MUS_AUDIO_SPEAKERS) ||
	  (wd->device == MUS_AUDIO_DAC_OUT) ||
	  (wd->device == MUS_AUDIO_CD))
	{
	  GtkWidget *spix;
	  spix = gtk_drawing_area_new();
	  gtk_widget_set_events(spix, GDK_EXPOSURE_MASK);
	  gtk_widget_set_size_request(spix, 16, 16);
	  gtk_box_pack_start(GTK_BOX(sbox), spix, false, false, 0);
	  gtk_widget_show(spix);
	  SG_SIGNAL_CONNECT(spix, "expose_event", spix_expose, wd);
	}
      else
	{
	  GtkWidget *slabel;
	  if (last_device == this_device)
	    {
	      if ((!input) && (this_device == MUS_AUDIO_DAC_FILTER))
		slabel = gtk_label_new(_("ton"));
	      else slabel = gtk_label_new(recorder_field_abbreviation(this_device));
	      gtk_box_pack_start(GTK_BOX(sbox), slabel, false, false, 0);
	      gtk_widget_show(slabel);
	    }
	  else
	    {
	      slabel = gtk_label_new("---");
	      gtk_box_pack_start(GTK_BOX(sbox), slabel, false, false, 0);
	      gtk_widget_show(slabel);
	    }
	}
#endif
      wd->adj = gtk_adjustment_new(1.0 - vol, 0.0, 1.01, 0.001, 0.01, .01);
      wd->wg = gtk_vscale_new(GTK_ADJUSTMENT(wd->adj));
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(wd->wg)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_draw_value(GTK_SCALE(wd->wg), false);
      gtk_box_pack_start(GTK_BOX(sbox), wd->wg, true, true, 0);
      gtk_widget_show(wd->wg);
      SG_SIGNAL_CONNECT(wd->adj, "value_changed", volume_callback, wd);
#if (HAVE_OSS || HAVE_ALSA)
      last_device = this_device;
#endif
    }
}

static GtkWidget *make_button_box(recorder_info *rp, pane_t *p, Float meter_size,
			      bool input, int overall_input_ctr, int vu_meters, GtkWidget *vuh)
{
  int i, row, columns, button_size, rows;
  GtkWidget *btab, *button_label;
  GtkWidget **hboxes;

  btab = gtk_vbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(vuh), btab, true, true, 10);
  gtk_widget_show(btab);

  if ((rp->systems == 1) || (!input))
    button_label = gtk_label_new(recorder_device_name(p->device));
  else button_label = gtk_label_new(mus_audio_system_name(p->system));
  gtk_box_pack_start(GTK_BOX(btab), button_label, false, false, 0);
  gtk_widget_show(button_label);

  /* need secondary set of boxes if vu_meters > 4 */

  columns = recorder_columns(vu_meters);
  rows = vu_meters / columns;
  hboxes = (GtkWidget **)CALLOC(rows, sizeof(GtkWidget *));
  for (i = 0; i < rows; i++) 
    {
      hboxes[i] = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(btab), hboxes[i], true, true, 0);
      gtk_widget_show(hboxes[i]);
    }

  p->on_buttons = (GtkWidget **)CALLOC(vu_meters, sizeof(GtkWidget *));
  p->on_buttons_size = vu_meters;

  columns = recorder_columns(vu_meters);
  row = 0;
  button_size = 100 / columns;

  for (i = 0; i < vu_meters; i++)
    {
      Wdesc *wd;
      vu_t *vu;
      GtkWidget *chan_label, *bbox;

      p->on_buttons[i] = gtk_button_new();
      /* gtk_container_set_border_width(GTK_CONTAINER(p->on_buttons[i]), 6); */
      gtk_box_pack_start(GTK_BOX(hboxes[row]), p->on_buttons[i], true, true, 0);
      gtk_widget_show(p->on_buttons[i]);

      bbox = gtk_vbox_new(false, 0);
      gtk_container_add(GTK_CONTAINER(p->on_buttons[i]), bbox);
      gtk_widget_show(bbox);

      chan_label = gtk_label_new(channel_name(p->in_chans, p->out_chans, i));
      gtk_box_pack_start(GTK_BOX(bbox), chan_label, true, true, 1);
      gtk_widget_show(chan_label);

      wd = (Wdesc *)CALLOC(1, sizeof(Wdesc));
      wd->chan = i;
      wd->gain = i + overall_input_ctr;
      wd->p = p;
      wd->device = p->device;
      wd->system = p->system;
      wd->field = MUS_AUDIO_AMP;
      vu = p->meters[i];
      SG_SIGNAL_CONNECT(p->on_buttons[i], "clicked", meter_button_callback, wd);

      if ((i == (columns*(row + 1) - 1)) && (vu_meters > (i + 1))) 
	row++;
    }
  FREE(hboxes);
  return(btab);
}

static void make_reset_button(pane_t *p, GtkWidget *btab)
{
  p->reset_button = gtk_button_new_with_label(_("Reset"));
  gtk_box_pack_start(GTK_BOX(btab), p->reset_button, true, true, 0);
  gtk_widget_show(p->reset_button);
  SG_SIGNAL_CONNECT(p->reset_button, "clicked", vu_reset_callback, p);
}

static int make_amp_sliders(recorder_info *rp, pane_t *p, bool input, int overall_input_ctr)
{
  int i, amp_sliders, temp_out_chan, temp_in_chan;

  if (input) 
    amp_sliders = p->in_chans * p->out_chans;
  else amp_sliders = p->out_chans;
  p->amps = (amp_t **)CALLOC(amp_sliders, sizeof(amp_t *));
  p->amps_size = amp_sliders;
  
  /* input numbering starts at overall_input_ctr and goes up for p->in_chans */
  /* output numbering starts at 0 and goes for p->out_chans */
  /* if input, do direct cases first, then fill in rest of 1, rest of 2 etc */
  temp_out_chan = 0;
  temp_in_chan = 0;
  for (i = 0; i < amp_sliders; i++)
    {
      Wdesc *wd;
      int in_chan, out_chan;
      amp_t *a;

      in_chan = temp_in_chan;
      out_chan = temp_out_chan;
      wd = (Wdesc *)CALLOC(1, sizeof(Wdesc));
      wd->chan = i;
      wd->p = p;
      wd->device = p->device;
      wd->system = p->system;
      wd->field = MUS_AUDIO_AMP;
      p->amps[i] = (amp_t *)CALLOC(1, sizeof(amp_t));
      a = p->amps[i];
      if (input) 
	{
	  a->type = INPUT_AMP; 
	  a->in = temp_in_chan + overall_input_ctr;
	  a->device_in_chan = temp_in_chan;
	  a->out = temp_out_chan;

	  if (!(rp->in_amp_preset[a->in][a->out]))
	    {
	      if (temp_in_chan == temp_out_chan)
		rp->in_amps[a->in][a->out] = 1.0;
	      else rp->in_amps[a->in][a->out] = 0.0;
	    }

	  AMP_rec_ins[a->in][a->out] = p->amps[i];
	  temp_in_chan++;
	  if (temp_in_chan >= p->in_chans)
	    {
	      temp_in_chan = 0;
	      temp_out_chan++;
	    }
	}
      else 
	{
	  a->type = OUTPUT_AMP;
	  a->device_in_chan = 0;
	  a->out = i;
	  /* out_amps[i] set to 1.0 at initialization time */
	  AMP_rec_outs[i] = a;
	}
      if ((!input) || (p->active_sliders[in_chan][out_chan]))
	make_recorder_slider(p, a, input);
      else
	{
	  a->label = NULL;
	  a->number = NULL;
	  a->slider = NULL;
	}
    }
  return(0);
}

void sensitize_control_buttons(void)
{
  int i;
  for (i = 0; i < device_buttons_size - 1; i++) /* last button is autoload_file */
    {
      if (device_buttons[i])
	set_sensitive(device_buttons[i], true);
    }
}

void unsensitize_control_buttons(void)
{
  int i;
  for (i = 0; i < device_buttons_size - 1; i++)
    {
      if (device_buttons[i])
	set_sensitive(device_buttons[i], false);
    }
}

static void reset_record_callback(GtkWidget *w, gpointer context) 
{
  /* if recording, cancel and toss data, else reset various fields to default (ss) values */
  recorder_info *rp;
  rp = get_recorder_info();
  if (rp->recording)                  /* cancel */
    {
      char *str;
      rp->recording = false;
      rp->triggered = (!rp->triggering);
      sensitize_control_buttons();
      set_stock_button_label(reset_button, _("Reset"));
      gtk_widget_modify_bg(record_button, GTK_STATE_NORMAL, ss->sgx->basic_color);
      set_stock_button_label(record_button, (rp->triggering) ? _("Triggered Record") : _("Record"));
      mus_file_close(rp->output_file_descriptor);
      rp->output_file_descriptor = -1;
      str = just_filename(rp->output_file);
      record_report(messages, str, _(" recording cancelled"), NULL);
      FREE(str);
      snd_remove(rp->output_file, REMOVE_FROM_CACHE);
    }
  else                            /* reset or restart */
    { 
      pane_t *p;
      vu_t *vu;
      int i, k;
      for (i = 0; i < rp->ordered_devices_size; i++)
	{
	  p = all_panes[i];
	  for (k = 0; k < p->meters_size; k++)
	    {
	      vu = p->meters[k];
	      vu->max_val = 0.0;
	      set_vu_max_label(vu);
	    }
	}
      /* now if dac turned us off, turn everything back on */
      if (!(rp->taking_input))            /* restart */
	{
	  fire_up_recorder();
	  set_stock_button_label(reset_button, _("Reset"));
	}
    }
}

static void dismiss_record_callback(GtkWidget *w, gpointer context) 
{
  state_context *sgx;
  recorder_info *rp;
  rp = get_recorder_info();
  sgx = ss->sgx;
  if (rp->recording) reset_record_callback(w, context);
  gtk_widget_hide(recorder);
  close_recorder_audio();
}

static void errors_to_recorder(const char *msg, void *data)
{
  recorder_error((char *)msg);
}

void finish_recording(recorder_info *rp)
{
  char *str;
  Float duration;
  sensitize_control_buttons();
  gtk_widget_modify_bg(record_button, GTK_STATE_NORMAL, ss->sgx->basic_color);
  set_stock_button_label(reset_button, _("Reset"));
  set_stock_button_label(record_button, (rp->triggering) ? _("Triggered Record") : _("Record"));
  mus_file_close(rp->output_file_descriptor);
  mus_header_change_data_size(rp->output_file,
			      rp->output_header_type,
			      rp->total_output_frames * rp->out_chans * mus_bytes_per_sample(rp->output_data_format)); 
  rp->output_file_descriptor = -1;
  duration = (Float)((double)(rp->total_output_frames) / (Float)(rp->srate));
  reflect_recorder_duration(duration);
  str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  mus_snprintf(str, PRINT_BUFFER_SIZE, _("recorded %s:\n  duration: %.2f\n  srate: %d, chans: %d\n  type: %s, format: %s"),
	       rp->output_file, duration, rp->srate, rp->out_chans,
	       mus_header_type_name(rp->output_header_type), 
	       mus_data_format_name(rp->output_data_format));
  record_report(messages, str, NULL);
  FREE(str);
  if (rp->autoload)
    {
      snd_info *sp;
      ss->open_requestor = FROM_RECORDER;
      redirect_everything_to(errors_to_recorder, (void *)rp);
      if ((sp = find_sound(rp->output_file, 0)))
	snd_update(sp);
      else snd_open_file(rp->output_file, FILE_READ_WRITE);
      redirect_everything_to(NULL, NULL);
    }
}

static XEN recorder_file_hook;

static void record_button_callback(GtkWidget *w, gpointer context) 
{
  recorder_info *rp;
  rp = get_recorder_info();
  rp->recording = (!(rp->recording));
  if (rp->recording)
    {
      int i, old_srate, ofmt, rs, ochns;
      off_t oloc, samples;
      char *comment = NULL;
      char *str;
      bool got_name = false;

      if (!(rp->taking_input)) fire_up_recorder();
      old_srate = rp->srate;
      redirect_snd_error_to(post_error_in_message_pane, (void *)recdat);
      comment = get_file_dialog_sound_attributes(recdat, &rs, &ochns, &rp->output_header_type, &ofmt, &oloc, &samples, 1); 
      redirect_snd_error_to(NULL, NULL);
      rp->output_data_format = ofmt;
      if (rp->out_chans == 0)
	{
	  if ((all_panes[out_file_pane]) && 
	      (all_panes[out_file_pane]->on_buttons_size < ochns))
	    {
	      char *buf;
	      rp->out_chans = all_panes[out_file_pane]->on_buttons_size;
	      buf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
	      mus_snprintf(buf, PRINT_BUFFER_SIZE, 
			   "only %d out chan%s available",
			   rp->out_chans, (rp->out_chans > 1) ? "s" : "");
	      record_report(messages, buf, NULL);
	      FREE(buf);
	    }
	  else rp->out_chans = ochns;
	}
      if (rs != old_srate) 
	{
	  rp->srate = rs;
	  recorder_set_audio_srate(MUS_AUDIO_DEFAULT, rp->srate, 0, rp->taking_input);
	}
      if (rp->out_chans < 0)
	{
	  record_report(messages, _("can't record: you screwed up the output channel number!"), NULL);
	  rp->recording = false;
	  rp->triggered = (!rp->triggering);
	  if (comment) FREE(comment);
	  return;
	}
      reflect_recorder_duration(0.0);
      if (out_chans_active() != rp->out_chans)
	{
	  Wdesc *wd;
	  pane_t *p;
	  char *buf;
	  buf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
	  mus_snprintf(buf, PRINT_BUFFER_SIZE,
		       _("chans field (%d) doesn't match file out panel (%d channel%s active)"), 
		       rp->out_chans, out_chans_active(), (out_chans_active() > 1) ? "s" : "");
	  record_report(messages, buf, NULL);
	  FREE(buf);
	  wd = (Wdesc *)CALLOC(1, sizeof(Wdesc));
	  wd->p = all_panes[out_file_pane];
	  p = wd->p;
	  wd->field = MUS_AUDIO_AMP;
	  wd->device = p->device;
	  wd->system = 0;
	  for (i = 0; i < rp->out_chans; i++)
	    if (!(p->active[i]))
	      {
		wd->chan = i;
		meter_button_callback(p->on_buttons[i], (gpointer)wd);
	      }
	  FREE(wd);
	}
      if (in_chans_active() == 0)
	{
	  record_report(messages, _("can't record: no inputs enabled"), NULL);
	  rp->recording = false;
	  rp->triggered = (!rp->triggering);
	  if (comment) FREE(comment);
	  return;
	}
      str = (char *)gtk_entry_get_text(GTK_ENTRY(file_text));
      if (XEN_HOOKED(recorder_file_hook))
	{
	  int gloc;
	  XEN res;
	  res = run_progn_hook(recorder_file_hook,
			       XEN_LIST_1((str) ? C_TO_XEN_STRING(str) : XEN_FALSE),
			       S_recorder_file_hook);
	  gloc = snd_protect(res);
	  if (XEN_STRING_P(res))
	    {
	      if (rp->output_file) FREE(rp->output_file);
	      gtk_entry_set_text(GTK_ENTRY(file_text), XEN_TO_C_STRING(res));
	      rp->output_file = mus_expand_filename(XEN_TO_C_STRING(res));
	      got_name = true;
	    }
	  snd_unprotect_at(gloc);
	}
      if (!got_name)
	{
	  if ((str) && (*str))
	    {
	      if (rp->output_file) FREE(rp->output_file);
	      rp->output_file = mus_expand_filename(str);
	    }
	  else
	    {
	      record_report(messages, _("can't record: no output file name supplied"), NULL);
	      rp->recording = false;
	      rp->triggered = (!rp->triggering);
	      if (comment) FREE(comment);
	      return;
	    }
	}
      gtk_widget_modify_bg(record_button, GTK_STATE_NORMAL, ss->sgx->red);
      set_stock_button_label(reset_button, _("Cancel"));
      set_stock_button_label(record_button, _("Done"));
      recorder_start_output_file(comment);
      if (comment) FREE(comment);
    }
  else 
    finish_recording(rp);
}

static pane_t *make_pane(recorder_info *rp, GtkWidget *paned_window, int device, int system)
{
  /* VU meters (frame, then drawing area widget) */
  int i, k;
  pane_t *p;
  GtkWidget *matrix_frame;
  GtkWidget *vuh, *vuv, *gv, *btab;
  int vu_meters, true_inputs, num_gains;
  bool input;
  int pane_max;
  Float meter_size;
  state_context *sx;
  int mixflds[MAX_AUDIO_FIELD];
  static int gain_ctr = 0;
  static int overall_input_ctr = 0;
  sx = ss->sgx;
  p = (pane_t *)CALLOC(1, sizeof(pane_t));
  p->device = device;
  p->system = system;
  true_inputs = recorder_check_device(system, device, mixer_gains_posted, tone_controls_posted, mixflds, &num_gains, &input);

  if (input) 
    {
      if ((rp->in_chans > 0) && (true_inputs > rp->in_chans)) vu_meters = rp->in_chans; else vu_meters = true_inputs;
      p->in_chans = vu_meters;
      p->out_chans = rp->out_chans;
      /* this determines how many of the left-side buttons we get if chans > 4; if defaults to 2 (snd.c)
       * but probably should look at the output device's out chans when we start the recorder for the
       * first time, but not clobber user's setting (if any); perhaps if it's not the default, it can
       * be reset?  And if more than (say) 100 buttons, use pull-down menus?
       */
    }
  else 
    {
      vu_meters = true_inputs;
      if (vu_meters < rp->out_chans) 
	vu_meters = rp->out_chans;
      else
	{
	  if ((rp->out_chans > 0) && (vu_meters > rp->out_chans))
	    vu_meters = rp->out_chans;
	}
      p->out_chans = vu_meters;
      if (num_gains > vu_meters) num_gains = vu_meters;
      p->in_chans = 1;
    }

  p->meters = (vu_t **)CALLOC(vu_meters, sizeof(vu_t *));
  p->meters_size = vu_meters;
  p->active = (bool *)CALLOC(vu_meters, sizeof(bool));
  p->active_size = vu_meters;
  p->active_sliders = (bool **)CALLOC(p->in_chans, sizeof(bool *));
  for (i = 0; i < p->in_chans; i++) 
    p->active_sliders[i] = (bool *)CALLOC(p->out_chans, sizeof(bool));

  /* paned_window is a vbox = stack of panes, each pane is hbox = meters, sliders | gains */
  p->pane = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(paned_window), p->pane, false, false, 4); /* between-pane spacing */
  gtk_widget_show(p->pane);

  meter_size = vu_size(ss);
  if (vu_meters > 4) meter_size *= .6; else if (vu_meters > 2) meter_size *= .8;
  if ((vu_meters%5) == 0) meter_size *= 0.8;

  vuv = gtk_vbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(p->pane), vuv, true, true, 0);
  gtk_widget_show(vuv);
  p->slider_box = vuv;

  gv = gtk_hbox_new(false, 0);
  gtk_box_pack_end(GTK_BOX(p->pane), gv, false, false, 0);
  gtk_widget_show(gv);

  /* now left side = vuv divided vuh then sliders, right side = gv */

  vuh = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(vuv), vuh, false, false, 0);
  gtk_widget_show(vuh);

  if ((input) && ((p->in_chans*p->out_chans) > 8))
    {
      for (i = 0; i < p->in_chans; i++) 
	for (k = 0; k < p->out_chans; k++) 
	  if (i == k) 
	    p->active_sliders[i][k] = true;

      /* rather than default to posting 64 (or 256!) sliders, set up a channel matrix where desired sliders can be set */
      matrix_frame = make_button_matrix(rp, p, "channel-matrix", vuh, meter_size);
    }
  else 
    {
      for (i = 0; i < p->in_chans; i++) 
	for (k = 0; k < p->out_chans; k++) 
	  p->active_sliders[i][k] = true;
    }

  make_vu_meters(p, vu_meters, overall_input_ctr, meter_size, input, vuh);
  btab = make_button_box(rp, p, meter_size, input, overall_input_ctr, vu_meters, vuh);
  make_reset_button(p, btab);

  if (num_gains > 0)
    {
      make_vertical_gain_sliders(rp, p, num_gains, gain_ctr, mixflds, input, gv);
      gain_ctr += num_gains;
    }

  /* now the amp sliders across the bottom of the pane, with 'mixer' info on the right */
  pane_max = make_amp_sliders(rp, p, input, overall_input_ctr);

  p->in_chan_loc = overall_input_ctr;
  if (input) overall_input_ctr += true_inputs; /* p->in_chans; */
#if (HAVE_OSS || HAVE_ALSA)
  p->pane_size = pane_max + 20;
#else
  p->pane_size = pane_max + 50;
#endif

  return(p);
}

static void initialize_recorder(recorder_info *rp);
static GtkWidget *file_info_pane;

static gint recorder_delete(GtkWidget *w, GdkEvent *event, gpointer context)
{
  dismiss_record_callback(w, context);
  return(true);
}

widget_t snd_record_file(void)
{
  recorder_info *rp;
  rp = get_recorder_info();
  if (!recorder)
    {
      int n, i, input_devices, output_devices;
      GdkDrawable *wn;
      state_context *sx;
      GtkWidget *rec_panes_box, *help_button, *dismiss_button;

      input_devices = recorder_get_devices(rp, &output_devices);
      if (input_devices == -1) return(NULL);
      if (input_devices == 0)
	{
	  snd_warning("can't find any input devices!");
	  return(0);
	}

      recorder_characterize_devices(input_devices + 1, output_devices);
      if (rp->possible_input_chans <= 0)
	{
	  snd_warning("odd: found %d input device%s, but then %d input channels\n", 
		      input_devices, 
		      (input_devices != 1) ? "s" : "",
		      rp->possible_input_chans);
	  return(NULL);
	}

      sx = ss->sgx;
      wn = MAIN_WINDOW(ss);
      draw_gc = gdk_gc_new(wn);
      gdk_gc_set_background(draw_gc, sx->basic_color);
      gdk_gc_set_foreground(draw_gc, sx->black);
      gdk_gc_set_function(draw_gc, GDK_COPY);
      vu_gc = gdk_gc_new(wn);
      gdk_gc_set_background(vu_gc, sx->white);
      gdk_gc_set_foreground(vu_gc, sx->black);
      gdk_gc_set_function(vu_gc, GDK_COPY);

      all_panes = (pane_t **)CALLOC(input_devices + 1, sizeof(pane_t *));
      device_buttons_size = input_devices + 2; /* inputs, one output, autoload_file */
      device_buttons = (GtkWidget **)CALLOC(device_buttons_size, sizeof(GtkWidget *));
      if (rp->num_mixer_gains > 0)
	gain_sliders = (Wdesc **)CALLOC(rp->num_mixer_gains, sizeof(Wdesc *));
      /* out_file_pane will be the bottom (output) audio pane, not the file info pane */

      rec_in_VU = (vu_t **)CALLOC(rp->possible_input_chans, sizeof(vu_t *));
      rec_out_VU = (vu_t **)CALLOC(MAX_OUT_CHANS, sizeof(vu_t *));

      AMP_rec_ins = (amp_t ***)CALLOC(rp->possible_input_chans, sizeof(amp_t **));
      for (i = 0; i < rp->possible_input_chans; i++) 
	AMP_rec_ins[i] = (amp_t **)CALLOC(MAX_OUT_CHANS, sizeof(amp_t *));
      AMP_rec_outs = (amp_t **)CALLOC(MAX_OUT_CHANS, sizeof(amp_t *));

      /* for max label above VU meters */
      small_font = pango_font_description_from_string(SMALL_FONT);
      mid_font = pango_font_description_from_string(MID_FONT);
      large_font = pango_font_description_from_string(LARGE_FONT);

      recorder = snd_gtk_dialog_new();
      SG_SIGNAL_CONNECT(recorder, "delete_event", recorder_delete, NULL);
      gtk_window_set_title(GTK_WINDOW(recorder), _("Record"));
      sg_make_resizable(recorder);
      gtk_container_set_border_width (GTK_CONTAINER(recorder), 6); /* includes top border */
      gtk_widget_realize(recorder);

      help_button = gtk_button_new_from_stock(GTK_STOCK_HELP);
      gtk_widget_set_name(help_button, "help_button");

      dismiss_button = gtk_button_new_from_stock(GTK_STOCK_QUIT);
      gtk_widget_set_name(dismiss_button, "quit_button");

#ifdef GTK_STOCK_MEDIA_STOP
      reset_button = sg_button_new_from_stock_with_label(_("Reset"), GTK_STOCK_MEDIA_STOP);
#else
      reset_button = sg_button_new_from_stock_with_label(_("Reset"), GTK_STOCK_REFRESH);
#endif
      gtk_widget_set_name(reset_button, "reset_button");

#ifdef GTK_STOCK_MEDIA_RECORD
      record_button = sg_button_new_from_stock_with_label(_("Record"), GTK_STOCK_MEDIA_RECORD);
#else
      record_button = sg_button_new_from_stock_with_label(_("Record"), GTK_STOCK_EXECUTE);
#endif
      gtk_widget_set_name(record_button, "doit_button");

      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(recorder)->action_area), record_button, true, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(recorder)->action_area), dismiss_button, true, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(recorder)->action_area), reset_button, true, true, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(recorder)->action_area), help_button, true, true, 10);

      SG_SIGNAL_CONNECT(dismiss_button, "clicked", dismiss_record_callback, NULL);
      SG_SIGNAL_CONNECT(help_button, "clicked", help_record_callback, NULL);
      SG_SIGNAL_CONNECT(reset_button, "clicked", reset_record_callback, NULL);
      SG_SIGNAL_CONNECT(record_button, "clicked", record_button_callback, NULL);

      gtk_widget_show(dismiss_button);
      gtk_widget_show(reset_button);
      gtk_widget_show(record_button);
      gtk_widget_show(help_button);

      rec_panes_box = GTK_DIALOG(recorder)->vbox;

      gtk_widget_show(recorder);
      make_record_icons(recorder);

      /* open all audio devices and collect ins/out etc */
      out_file_pane = (rp->ordered_devices_size - 1);

      /* see note above (line 2809) about recorder out chans */
      n = device_channels(MUS_AUDIO_PACK_SYSTEM(rp->ordered_systems[out_file_pane]) |
			  rp->ordered_devices[out_file_pane]);
      if (rp->out_chans == 0)
	rp->out_chans = n;

      for (i = 0; i < rp->ordered_devices_size; i++)
	{
	  int device, system;
	  /* last one is output */
	  device = rp->ordered_devices[i];
	  system = rp->ordered_systems[i];
	  all_panes[i] = make_pane(rp, rec_panes_box, device, system);
	}

      file_info_pane = gtk_frame_new(NULL);
      gtk_widget_set_name(file_info_pane, "record_frame");
      gtk_frame_set_shadow_type(GTK_FRAME(file_info_pane), GTK_SHADOW_ETCHED_IN);
      gtk_widget_modify_bg(file_info_pane, GTK_STATE_NORMAL, ss->sgx->zoom_color);
      gtk_box_pack_start(GTK_BOX(rec_panes_box), file_info_pane, false, false, 0);
      gtk_widget_show(file_info_pane);
      make_file_info_pane(rp, file_info_pane, rp->ordered_devices_size);

      set_dialog_widget(RECORDER_DIALOG, recorder);
      initialize_recorder(rp);
    }
  gtk_widget_show(recorder);
  if (!(rp->taking_input)) fire_up_recorder();
  return(recorder);
}

void set_recorder_autoload(recorder_info *rp, bool val)
{
  rp->autoload = val;
  if (recorder) 
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(device_buttons[rp->autoload_button]), val);
}

void reflect_recorder_in_amp(int in, int out, Float val)
{
  recorder_info *rp;
  rp = get_recorder_info();
  if ((recorder) && (in < rp->possible_input_chans) && (out < MAX_OUT_CHANS) && (AMP_rec_ins[in][out]))
    {
      Float temp;
      amp_t *a;
      a = AMP_rec_ins[in][out];
      temp = amp_to_slider(val); 
      record_amp_changed(a, temp); 
      ignore_callback = true;
      GTK_ADJUSTMENT(a->adj)->value = temp;
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(a->adj));
      ignore_callback = false;
    }
}

void reflect_recorder_out_amp(int ind, Float val)
{
  if ((recorder) && (ind < MAX_OUT_CHANS) && (AMP_rec_outs[ind]))
    {
      Float temp;
      amp_t *a;
      a = AMP_rec_outs[ind];
      temp = amp_to_slider(val); 
      record_amp_changed(a, temp);
      ignore_callback = true;
      GTK_ADJUSTMENT(a->adj)->value = temp;
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(a->adj));
      ignore_callback = false;
    }
}

void reflect_amp_control_bounds_change_in_recorder(void)
{
  recorder_info *rp;
  rp = get_recorder_info();
  if (recorder)
    {
      int ic, oc;
      amp_t *a;
      ignore_callback = true;
      for (ic = 0; ic < rp->possible_input_chans; ic++)
	for (oc = 0; oc < MAX_OUT_CHANS; oc++)
	  if (AMP_rec_ins[ic][oc])
	    {
	      a = AMP_rec_ins[ic][oc];
	      if ((a->adj) && (rp->in_amps[ic][oc]))
		{
		  GTK_ADJUSTMENT(a->adj)->value = amp_to_slider(rp->in_amps[ic][oc]);
		  gtk_adjustment_value_changed(GTK_ADJUSTMENT(a->adj));
		}
	    }
      for (oc = 0; oc < MAX_OUT_CHANS; oc++)
	if (AMP_rec_outs[oc])
	  {
	    a = AMP_rec_outs[oc];
	    if (a->adj)
	      {
		GTK_ADJUSTMENT(a->adj)->value = amp_to_slider(rp->out_amps[oc]);
		gtk_adjustment_value_changed(GTK_ADJUSTMENT(a->adj));
	      }
	  }
      ignore_callback = false;
    }
}

void reflect_recorder_mixer_gain(int ind, Float val)
{
  recorder_info *rp;
  rp = get_recorder_info();
  if ((recorder) && (ind < rp->num_mixer_gains))
    {
      Wdesc *wd;
      wd = gain_sliders[ind];
      if (wd)
	{
	  GTK_ADJUSTMENT(wd->adj)->value = val;
	  gtk_adjustment_value_changed(GTK_ADJUSTMENT(wd->adj));
	  set_mixer_gain(wd->system, wd->device, wd->chan, wd->gain, wd->field, val);
	}
    }
}

static void initialize_recorder(recorder_info *rp)
{
  /* picked up initial (widget) values from globals vars */
#if NEW_SGI_AL || MUS_SUN
  int i;
#endif
  /* special case initializations */
#if OLD_SGI_AL
  /* in this case, digital in blocks out the others */
  long sb[2];
  int err;
  bool in_digital = false;
  sb[0] = AL_INPUT_SOURCE;
  err = ALgetparams(AL_DEFAULT_DEVICE, sb, 2);
  if ((!err) && (sb[1] == AL_INPUT_DIGITAL)) in_digital = true;
  set_toggle_button(device_buttons[rp->digital_in_button], 
		    in_digital, false, 
		    (void *)(all_panes[rp->digital_in_button]));
  device_button_callback(device_buttons[rp->digital_in_button], 
			 all_panes[rp->digital_in_button]);
#endif
#if NEW_SGI_AL || MUS_SUN
  /* for simplicity, and until someone complains, new SGI AL machines will just have one active input device */
  active_device_button = rp->microphone_button;
  for (i = 0; i < device_buttons_size - 1; i++)
    if ((device_buttons[i]) &&
	((i != rp->microphone_button) && (recorder_input_device(all_panes[i]->device))))
      set_toggle_button(device_buttons[i], false, true, (void *)(all_panes[i])); 
#endif
  if (rp->trigger != 0.0) set_recorder_trigger(rp, rp->trigger);
  if (rp->max_duration <= 0.0) rp->max_duration = 1000000.0;
}

void reflect_record_size(int size)
{
  if ((recorder) && (rec_size_text))
    widget_int_to_text(rec_size_text, size);
}

bool record_dialog_is_active(void)
{
  return((recorder) && (GTK_WIDGET_VISIBLE(recorder)));
}

void set_recorder_trigger(recorder_info *rp, Float val)
{
  rp->trigger = val;
  if (recorder)
    {
      GTK_ADJUSTMENT(trigger_adj)->value = val;
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(trigger_adj));
      internal_trigger_set(val);
    }
}

void set_recorder_srate(recorder_info *rp, int val)
{
  if (val < 1000) return;
  /* this just reflects the setting in the text field -- it doesn't actually set anything in the audio system */
  if (val > 0)
    {
      /* SGI AES In sometimes claims its srate is 0 */
      rp->srate = val;
      if (recorder) 
	widget_int_to_text(recdat->srate_text, rp->srate);
    }
}

void g_init_gxrec(void)
{
  #define H_recorder_file_hook S_recorder_file_hook " (filename): called when 'Record' is pressed \
in the recorder dialog.  The currently displayed output filename is passed in as the argument, \
or " PROC_FALSE " if no filename has been supplied.  The hook function should return the new filename."

  recorder_file_hook = XEN_DEFINE_HOOK(S_recorder_file_hook, 1, H_recorder_file_hook);     /* args = displayed output file name */
}
