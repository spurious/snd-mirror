/* TODO  buttons that choose which devices to display are no-ops
 * TODO  vertical slider labels aren't centered correctly
 */

#include "snd.h"
#include "snd-rec.h"

typedef struct {
  GtkWidget *meter;
  GtkWidget *max_button;
  GdkDrawable *wn;
  int on_off;
  int clipped;
  Float current_val, last_val;
  Float max_val;
  Float red_deg;
  Float size;
  int light_x, light_y, center_x, center_y;
  snd_state *ss;
  GdkPixmap *off_label;
  GdkPixmap *on_label;
  GdkPixmap *clip_label;
  GdkBitmap *off_label_mask;
  GdkBitmap *on_label_mask;
  GdkBitmap *clip_label_mask;
} VU;

typedef struct {
  GtkWidget *label, *number, *slider;
  GtkObject *adj;
  snd_state *ss;
  int last_amp, type, in, out;
  int device_in_chan;
  void *wd;
} AMP;

typedef struct {
  int in_chans, out_chans, meters_size, amps_size, active_size, on_buttons_size, in_chan_loc;
  VU **meters;         /* has meter widget, can assume frame is local to creator */
  AMP **amps;          /* chans -- has label number slider amp which_amp -- software input gains to output */ 
  int *active;         /* which (incoming or file_out outgoing) channels are actively shoveling samples */
  snd_state *ss;
  GtkWidget **on_buttons;
  GtkWidget *reset_button;
  GtkWidget *pane;
  GtkWidget *button_vertical_sep;
  int pane_size;
  int device, system;          /* audio.c style device descriptor */
  int **active_sliders;
  GtkWidget ***matrix_buttons;
  int bx, by;
  int bw, bh;
  GtkWidget *slider_box;
} PANE;

typedef struct {
  snd_state *ss;
  int chan, field, device, system;
  int gain;
  PANE *p;
  GtkWidget *wg;
  GtkObject *adj;
} Wdesc;

static GdkGC *draw_gc, *vu_gc;

static char timbuf[TIME_STR_SIZE];
static char *msgbuf = NULL;

static file_data *recdat;

static VU **rec_in_VU = NULL;       /* from rec in to associated meter */
static VU **rec_out_VU = NULL;      /* from rec out to associated meter */
static Wdesc **gain_sliders = NULL;  /* associated sliders and descriptors for write audio state */
static AMP ***AMP_rec_ins = NULL;
static AMP **AMP_rec_outs = NULL;

static GtkWidget *recorder = NULL;      /* the outer dialog shell */

static PANE **all_panes = NULL;
static int out_file_pane;

static GtkWidget *rec_size_text, *trigger_scale, *trigger_label;
static GtkWidget *file_duration, *messages = NULL, *record_button, *reset_button, *file_text;
static GtkWidget **device_buttons;
static GtkObject *trigger_adj;
static int device_buttons_size = 0;
#if NEW_SGI_AL || defined(SUN)
  static int active_device_button = -1;
#endif
static int mixer_gains_posted[MAX_SOUNDCARDS];
static int tone_controls_posted[MAX_SOUNDCARDS];
static GdkFont *small_font;

static vu_label **vu_labels = NULL;
static int vu_labels_size = 0;
static int current_vu_label = 0;

static char **pending_error = NULL;
static int pending_errors = 0;
static int pending_error_size = 0;

static void set_label_font(GtkWidget *w)
{
  GtkStyle *style;
  style = gtk_style_copy(gtk_widget_get_style(w));
  style->font = small_font;
  gtk_widget_set_style(w, style);
}

static void record_report(GtkWidget *text, ...)
{
  /* place time-stamped message in text window */
  time_t ts;
  va_list ap;
  char *nextstr;
  snd_state *ss;
  ss = get_global_state();
  if (msgbuf == NULL) msgbuf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
#if HAVE_STRFTIME
  time(&ts);
  strftime(timbuf, TIME_STR_SIZE, "%H:%M:%S", localtime(&ts));
  mus_snprintf(msgbuf, PRINT_BUFFER_SIZE, "\n[%s] ", timbuf);
#endif
  gtk_text_insert(GTK_TEXT(text), (ss->sgx)->help_text_fnt, (ss->sgx)->black, (ss->sgx)->light_blue, msgbuf, -1);
  va_start(ap, text);
  while ((nextstr = va_arg(ap, char *)))
    gtk_text_insert(GTK_TEXT(text), (ss->sgx)->help_text_fnt, (ss->sgx)->black, (ss->sgx)->light_blue, nextstr, -1);
  va_end(ap);
}

void recorder_error(char *msg)
{
  if ((recorder) && (messages))
    record_report(messages, msg, NULL);
}



/* -------------------------------- ICONS -------------------------------- */

static GdkPixmap *speaker_pix, *line_in_pix, *mic_pix, *aes_pix, *adat_pix, *digital_in_pix, *cd_pix;
static GdkBitmap *speaker_mask, *line_in_mask, *mic_mask, *aes_mask, *adat_mask, *digital_in_mask, *cd_mask;
static int icons_created = 0;

static void make_record_icons(GtkWidget *w, snd_state *ss)
{
  GdkWindow *wn;
  icons_created = 1;
  wn = MAIN_WINDOW(ss);
  speaker_pix = gdk_pixmap_create_from_xpm_d(wn, &speaker_mask, (ss->sgx)->basic_color, speaker_bits());
  mic_pix = gdk_pixmap_create_from_xpm_d(wn, &mic_mask, (ss->sgx)->basic_color, mic_bits());
  line_in_pix = gdk_pixmap_create_from_xpm_d(wn, &line_in_mask, (ss->sgx)->basic_color, line_in_bits());
  cd_pix = gdk_pixmap_create_from_xpm_d(wn, &cd_mask, (ss->sgx)->basic_color, cd_bits());
  /* TODO: digital_in, aes, adat */
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

}

static GdkBitmap *device_mask(int device)
{
  switch (device)
    {
    case MUS_AUDIO_CD:         return(cd_mask); break;
    case MUS_AUDIO_DIGITAL_OUT:
    case MUS_AUDIO_LINE_OUT:
    case MUS_AUDIO_DEFAULT:
    case MUS_AUDIO_DAC_OUT:
    case MUS_AUDIO_DUPLEX_DEFAULT:
    case MUS_AUDIO_SPEAKERS:   return(speaker_mask); break;
    case MUS_AUDIO_ADAT_OUT:
    case MUS_AUDIO_ADAT_IN:    return(adat_mask); break;
    case MUS_AUDIO_SPDIF_IN:
    case MUS_AUDIO_SPDIF_OUT:
    case MUS_AUDIO_AES_OUT:  
    case MUS_AUDIO_AES_IN:     return(aes_mask); break;
    case MUS_AUDIO_LINE_IN:    return(line_in_mask); break;
    case MUS_AUDIO_DIGITAL_IN: return(digital_in_mask); break;
    case MUS_AUDIO_MICROPHONE: 
    default:                   return(mic_mask); break;
    }

}


static void recorder_noop_mouse_enter(GtkWidget *w, GdkEventCrossing *ev, gpointer data)
{
  gtk_signal_emit_stop_by_name(GTK_OBJECT(w), "enter_notify_event");
}
/* gtk_signal_connect(GTK_OBJECT(w), "enter_notify_event", GTK_SIGNAL_FUNC(recorder_noop_mouse_enter), NULL); */


/* -------------------------------- VU METER -------------------------------- */

static void allocate_meter_2(GtkWidget *w, vu_label *vu)
{
  GdkDrawable *wn;
  wn = w->window;
  vu->off_label = gdk_pixmap_create_from_xpm_d(wn, &(vu->off_label_mask), NULL, offlabel_bits());
  vu->on_label = gdk_pixmap_create_from_xpm_d(wn, &(vu->on_label_mask), NULL, onlabel_bits());
  vu->clip_label = gdk_pixmap_create_from_xpm_d(wn, &(vu->clip_label_mask), NULL, cliplabel_bits());
}


#define VU_OFF 0
#define VU_ON 1
#define VU_CLIPPED 2
#define CLIPPED_TIME 10
#define CLIPPED_TRIGGER 0.99
#define VU_NEEDLE_SPEED 0.25
#define VU_BUBBLE_SPEED 0.025
#define VU_BUBBLE_SIZE (15 * 64)

#define LIGHT_X 120
#define LIGHT_Y 100
#define CENTER_X 120
#define CENTER_Y 160
#define VU_COLORS 11
#define VU_NUMBERS 15
/* these are for the highly optimized size = 1.0 case */

static GdkColor *yellows[VU_COLORS];
static GdkColor *reds[VU_COLORS];
static int vu_colors_allocated = 0;
static int yellow_vals[] = {0, 16, 32, 64, 96, 128, 160, 175, 185, 200, 210, 220, 230, 240};

static void allocate_meter_1(snd_state *ss, vu_label *vu)
{
  /* called only if size not allocated yet and size = 1.0 not available as pre-made pixmap */
  GdkDrawable *wn;
  GdkColor tmp_color;
  GdkColormap *cmap;
  int i, j, k;
  GdkColor *white, *black, *red;
  int band;
  GdkPoint pts[16];
  int x0, y0, x1, y1;
  Float rdeg;
  Float size;
  int xs[5], ys[5];

  Float BAND_X;
  Float BAND_Y;
  cmap = gdk_colormap_get_system();

  size = vu->size;
  BAND_X = 2.75 * size;
  BAND_Y = 3.25 * size;
  red = (ss->sgx)->red;
  wn = recorder->window;
  black = (ss->sgx)->black;
  white = (ss->sgx)->white;

  if (!vu_colors_allocated)
    {
      vu_colors_allocated = 1;
      tmp_color.red = (unsigned short)65535;
      for (i = 0; i < VU_COLORS; i++)
	{
	  tmp_color.blue = (unsigned short)(256 * yellow_vals[i]);
	  tmp_color.green = (unsigned short)(256 * 230 + 26 * yellow_vals[i]);
	  yellows[i] = gdk_color_copy(&tmp_color);
	  gdk_color_alloc(cmap, yellows[i]);
	}
      for (i = 0; i < VU_COLORS; i++)
	{
	  tmp_color.blue = (unsigned short)(128 * yellow_vals[i]);
	  tmp_color.green = (unsigned short)(128 * yellow_vals[i]);
	  reds[i] = gdk_color_copy(&tmp_color);
	  gdk_color_alloc(cmap, reds[i]);
	}
    }
  for (k = 0; k < 2; k++) 
    {
      band = 1;
      if (k == 1)
	{
	  vu->clip_label = gdk_pixmap_new(wn, 
					  (unsigned int)(CENTER_X * 2 * size), 
					  (unsigned int)(CENTER_Y * size), 
					  -1);
	  gdk_gc_set_foreground(draw_gc, reds[0]);	    
	  gdk_draw_rectangle(vu->clip_label, draw_gc, TRUE, 0, 0, 
			     (unsigned int)(CENTER_X * 2 * size), 
			     (unsigned int)(CENTER_Y * size));
	}
      else 
	{
	  vu->on_label = gdk_pixmap_new(wn, 
					(unsigned int)(CENTER_X * 2 * size), 
					(unsigned int)(CENTER_Y * size),
					-1);
	  gdk_gc_set_foreground(draw_gc, yellows[2]);
	  gdk_draw_rectangle(vu->on_label, draw_gc, TRUE, 0, 0, 
			     (unsigned int)(CENTER_X * 2 * size), 
			     (unsigned int)(CENTER_Y * size));
	}
      /* initialize the sequence of nested polygons */
      pts[0].x = (short)(LIGHT_X * size - BAND_X);
      pts[0].y = (short)(LIGHT_Y * size);
      pts[1].x = pts[0].x;
      pts[1].y = (short)(pts[0].y - BAND_Y + 1);
      pts[2].x = pts[1].x + 1;
      pts[2].y = pts[1].y - 1;
      pts[3].x = (short)(pts[2].x + BAND_X * 2 - 2);
      pts[3].y = pts[2].y;
      pts[4].x = pts[3].x + 2;
      pts[4].y = pts[3].y + 1;
      pts[5].x = pts[4].x;
      pts[5].y = pts[0].y;
      pts[6].x = pts[0].x;
      pts[6].y = pts[0].y;
      if (k == 1)
	gdk_draw_polygon(vu->clip_label, draw_gc, TRUE, pts, 7);
      else gdk_draw_polygon(vu->on_label, draw_gc, TRUE, pts, 7);

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
	  pts[6].x = (short)(LIGHT_X * size + band * BAND_X);
	  pts[6].y = pts[5].y;
	  pts[7].x = pts[6].x;
	  pts[7].y = (short)(LIGHT_Y * size - band * (BAND_Y - 1));
	  pts[8].x = (short)(LIGHT_X * size + band * (BAND_X - 1));
	  pts[8].y = (short)(LIGHT_Y * size - band * BAND_Y);
	  pts[9].x = (short)(LIGHT_X * size - band * (BAND_X - 1));
	  pts[9].y = pts[8].y;
	  pts[10].x = (short)(LIGHT_X * size - band * BAND_X);
	  pts[10].y = (short)(LIGHT_Y * size - band * (BAND_Y - 1));
	  pts[11].x = pts[10].x;
	  pts[11].y = pts[6].y;
	  pts[12].x = pts[0].x;
	  pts[12].y = pts[0].y;
	  if (k == 1)
	    gdk_draw_polygon(vu->clip_label, draw_gc, TRUE, pts, 13);
	  else gdk_draw_polygon(vu->on_label, draw_gc, TRUE, pts, 13);
	  for (j = 0; j < 6; j++) 
	    { 
	      /* set up initial portion of next polygon */
	      pts[j].x = pts[j+6].x;
	      pts[j].y = pts[j+6].y;
	    }
	}
    }

  vu->off_label = gdk_pixmap_new(wn, 
				 (unsigned int)(CENTER_X * 2 * size), 
				 (unsigned int)(CENTER_Y * size),
				 -1);
  /* not on, so just display a white background */
  gdk_gc_set_foreground(draw_gc, white);
  gdk_draw_rectangle(vu->off_label, draw_gc, TRUE, 0, 0, 
		     (unsigned int)(CENTER_X * 2 * size), 
		     (unsigned int)(CENTER_Y * size));

  gdk_gc_set_foreground(draw_gc, black);

  /* draw the arcs */
  xs[0] = (int)(size * (CENTER_X - 120));
  ys[0] = (int)(size * (CENTER_Y - 120));
  xs[1] = (int)(size * (CENTER_X - 119));
  ys[1] = (int)(size * (CENTER_Y - 120));
  xs[2] = (int)(size * (CENTER_X - 119));
  ys[2] = (int)(size * (CENTER_Y - 119));
  xs[3] = (int)(size * (CENTER_X - 116));
  ys[3] = (int)(size * (CENTER_Y - 116));

  gdk_draw_arc(vu->on_label, draw_gc, FALSE, xs[0], ys[0], (unsigned int)(size * (240)), (unsigned int)(size * (240)), 45 * 64, 90 * 64);
  gdk_draw_arc(vu->on_label, draw_gc, FALSE, xs[1], ys[1], (unsigned int)(size * (239)), (unsigned int)(size * (239)), 45 * 64, 89 * 64);
  gdk_draw_arc(vu->on_label, draw_gc, FALSE, xs[2], ys[2], (unsigned int)(size * (239)), (unsigned int)(size * (239)), 45 * 64, 89 * 64);
  gdk_draw_arc(vu->on_label, draw_gc, FALSE, xs[3], ys[3], (unsigned int)(size * (232)), (unsigned int)(size * (232)), 45 * 64, 90 * 64);

  gdk_draw_arc(vu->off_label, draw_gc, FALSE, xs[0], ys[0], (unsigned int)(size * (240)), (unsigned int)(size * (240)), 45 * 64, 90 * 64);
  gdk_draw_arc(vu->off_label, draw_gc, FALSE, xs[1], ys[1], (unsigned int)(size * (239)), (unsigned int)(size * (239)), 45 * 64, 89 * 64);
  gdk_draw_arc(vu->off_label, draw_gc, FALSE, xs[2], ys[2], (unsigned int)(size * (239)), (unsigned int)(size * (239)), 45 * 64, 89 * 64);
  gdk_draw_arc(vu->off_label, draw_gc, FALSE, xs[3], ys[3], (unsigned int)(size * (232)), (unsigned int)(size * (232)), 45 * 64, 90 * 64);

  gdk_draw_arc(vu->clip_label, draw_gc, FALSE, xs[0], ys[0], (unsigned int)(size * (240)), (unsigned int)(size * (240)), 45 * 64, 90 * 64);
  gdk_draw_arc(vu->clip_label, draw_gc, FALSE, xs[1], ys[1], (unsigned int)(size * (239)), (unsigned int)(size * (239)), 45 * 64, 89 * 64);
  gdk_draw_arc(vu->clip_label, draw_gc, FALSE, xs[2], ys[2], (unsigned int)(size * (239)), (unsigned int)(size * (239)), 45 * 64, 89 * 64);
  gdk_draw_arc(vu->clip_label, draw_gc, FALSE, xs[3], ys[3], (unsigned int)(size * (232)), (unsigned int)(size * (232)), 45 * 64, 90 * 64);

  /* draw the axis ticks */
  for (i = 0; i < 5; i++)
    {
      rdeg = mus_degrees2radians(45 - i * 22.5);
      x0 = (int)(CENTER_X * size + 120 * size * sin(rdeg));
      y0 = (int)(CENTER_Y * size - 120 * size * cos(rdeg));
      x1 = (int)(CENTER_X * size + 130 * size * sin(rdeg));
      y1 = (int)(CENTER_Y * size - 130 * size * cos(rdeg));
      gdk_draw_line(vu->on_label, draw_gc, x0, y0, x1, y1);
      gdk_draw_line(vu->on_label, draw_gc, x0 + 1, y0, x1 + 1, y1);
      gdk_draw_line(vu->off_label, draw_gc, x0, y0, x1, y1);
      gdk_draw_line(vu->off_label, draw_gc, x0 + 1, y0, x1 + 1, y1);
      gdk_draw_line(vu->clip_label, draw_gc, x0, y0, x1, y1);
      gdk_draw_line(vu->clip_label, draw_gc, x0 + 1, y0, x1 + 1, y1);
      if (i < 4)
	for (j = 1; j < 6; j++)
	  {
	    rdeg = mus_degrees2radians(45 - i * 22.5 - j * (90.0 / 20.0));
	    x0 = (int)(CENTER_X * size + 120 * size * sin(rdeg));
	    y0 = (int)(CENTER_Y * size - 120 * size * cos(rdeg));
	    x1 = (int)(CENTER_X * size + 126 * size * sin(rdeg));
	    y1 = (int)(CENTER_Y * size - 126 * size * cos(rdeg));
	    gdk_draw_line(vu->on_label, draw_gc, x0, y0, x1, y1);
	    gdk_draw_line(vu->off_label, draw_gc, x0, y0, x1, y1);
	    gdk_draw_line(vu->clip_label, draw_gc, x0, y0, x1, y1);
	  }
    }
}

static void display_vu_meter(VU *vu)
{
  Float deg, rdeg, val;
  int nx0, nx1, ny0, ny1, redx, redy, bub0, bub1, i, j;
  GdkPixmap *label = 0;
  GdkBitmap *mask;
  snd_state *ss;
  Float size;
  state_context *sx;
  recorder_info *rp;
  rp = get_recorder_info();
  ss = vu->ss;
  sx = ss->sgx;
  size = vu->size;
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
      mask = vu->off_label_mask; 
      break;
    case VU_CLIPPED: 
      vu->clipped--; 
      if (vu->clipped <= 0) 
	{
	  label = vu->on_label; 
	  mask = vu->on_label_mask;
	  vu->on_off = VU_ON;
	} 
      else 
	{
	  label = vu->clip_label; 
	  mask = vu->clip_label_mask;
	}
      break;
    case VU_ON: 
      label = vu->on_label; 
      mask = vu->on_label_mask;
      break;
    }

  if (label) gdk_draw_pixmap(vu->wn, vu_gc, label, 0, 0, 0, 0, vu->center_x * 2, vu->center_y);
  val = vu->current_val * VU_NEEDLE_SPEED + (vu->last_val * (1.0 - VU_NEEDLE_SPEED));
  vu->last_val = val;
  deg = -45.0 + val * 90.0;
  /* if (deg < -45.0) deg = -45.0; else if (deg > 45.0) deg = 45.0; */
  rdeg = mus_degrees2radians(deg);
  nx0 = vu->center_x - (int)((Float)(vu->center_y - vu->light_y) / tan(mus_degrees2radians(deg + 90)));
  ny0 = vu->light_y;
  nx1 = (int)(vu->center_x + 130 * size * sin(rdeg));
  ny1 = (int)(vu->center_y - 130 * size * cos(rdeg));
  gdk_gc_set_foreground(vu_gc, sx->black);
  gdk_draw_line(vu->wn, vu_gc, nx0, ny0, nx1, ny1);

  /* this shadow doesn't do much (and if +/-3 for depth, it looks kinda dumb) */
  if (deg != 0.0)
    {
      if (vu->on_off == VU_OFF)
	gdk_gc_set_foreground(vu_gc, sx->position_color);
      else
	if (vu->on_off == VU_CLIPPED)
	  gdk_gc_set_foreground(vu_gc, sx->pushed_button_color);
	else gdk_gc_set_foreground(vu_gc, sx->white);
      if (deg < 0.0)
	gdk_draw_line(vu->wn, vu_gc, nx0 - 1, ny0, nx1 - 1, ny1);
      else gdk_draw_line(vu->wn, vu_gc, nx0 + 1, ny0, nx1 + 1, ny1);
      gdk_gc_set_foreground(vu_gc, sx->black);
    }

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
      bub0 = (int)(size * 117);
      bub1 = (int)(size * 119);
      for (i = bub0, j = bub0 * 2; i <= bub1; i++, j += (int)(2 * size))
	gdk_draw_arc(vu->wn, vu_gc, FALSE, vu->center_x - i, vu->center_y - i, j, j, 135 * 64 - redx, redy);
      gdk_gc_set_foreground(vu_gc, sx->black);
    }
}

static void meter_expose_callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  display_vu_meter((VU *)data);
}

static void meter_resize_callback(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  display_vu_meter((VU *)data);
}

static VU *make_vu_meter(GtkWidget *meter, int light_x, int light_y, int center_x, int center_y, snd_state *ss, Float size)
{
  VU *vu;
  int i;
  vu_label *vl = NULL;
  vu = (VU *)CALLOC(1, sizeof(VU));
  vu->meter = meter;
  vu->size = size;
  vu->wn = meter->window;
  vu->on_off = VU_OFF;
  vu->current_val = 0.0;
  vu->last_val = 0.0;
  vu->clipped = 0;
  vu->max_val = 0.0;
  vu->light_x = (int)(light_x * size);
  vu->light_y = (int)(light_y * size);
  vu->center_x = (int)(center_x * size);
  vu->center_y = (int)(center_y * size);
  vu->ss = ss;

  for (i = 0; i < current_vu_label; i++)
    if (vu_labels[i]->size == size)
      {
	vl = vu_labels[i];
	break;
      }
  if (vl == NULL)
    {
      if (current_vu_label >= vu_labels_size)
	{
	  vu_labels_size += 8;
	  if (!vu_labels)
	    vu_labels = (vu_label **)CALLOC(vu_labels_size, sizeof(vu_label *));
	  else vu_labels = (vu_label **)REALLOC(vu_labels, vu_labels_size * sizeof(vu_label *));
	}
      vu_labels[current_vu_label] = (vu_label *)CALLOC(1, sizeof(vu_label));
      vl = vu_labels[current_vu_label];
      current_vu_label++;
      vl->label_font = get_vu_font(ss, size);
      vl->size = size;
      if ((vl->size == 1.0) || (vl->size > 4.0) || (vl->size < .25))
	allocate_meter_2(recorder, vl);
      else allocate_meter_1(ss, vl);
    }
  vu->on_label = vl->on_label;
  vu->off_label = vl->off_label;
  vu->clip_label = vl->clip_label;
  return(vu);
}

static void set_vu_val (VU *vu, Float val) 
{
  vu->last_val = vu->current_val;
  vu->current_val = val;
  display_vu_meter(vu);
  if (val > vu->max_val)
    {
      vu->max_val = val;
      mus_snprintf(timbuf, TIME_STR_SIZE, "%.3f", val);
      set_label(vu->max_button, timbuf);
    }
}

void recorder_set_vu_in_val(int chan, MUS_SAMPLE_TYPE val) {set_vu_val(rec_in_VU[chan], MUS_SAMPLE_TO_FLOAT(val));}
void recorder_set_vu_out_val(int chan, MUS_SAMPLE_TYPE val) {set_vu_val(rec_out_VU[chan], MUS_SAMPLE_TO_FLOAT(val));}


/* -------------------------------- AMP SLIDER CALLBACKS -------------------------------- */

#define INPUT_AMP 0
#define OUTPUT_AMP 1

static char record_one[5] = {'1', STR_decimal, '0', '0', '\0'};
static char record_zero[5] = {'0', STR_decimal, '0', '0', '\0'};
static char amp_number_buffer[5] = {'1', STR_decimal, '0', '0', '\0'};

static void record_amp_changed(AMP *ap, Float scrollval)
{
  char *sfs;
  Float amp;
  recorder_info *rp;
  rp = get_recorder_info();
  if (scrollval < .15)
    amp = (scrollval * 1.13);
  else amp = (exp((scrollval - 0.5) * 5.0));
  sfs = prettyf(amp, 2);
  fill_number(sfs, amp_number_buffer);
  set_button_label(ap->number, amp_number_buffer);
  FREE(sfs);
  if (ap->type == INPUT_AMP)
    rp->in_amps[ap->in][ap->out] = amp;
  else rp->out_amps[ap->out] = amp;
}

static Float amp_to_slider(Float amp)
{
  /* reverse calc above */
  if (amp <= 0.0)
    return(0.0);
  else
    {
      if (amp < .173)
	return(amp * .867);
      else return(log(amp) * 0.2 + 0.5);
    }
}

static Float global_amp(AMP *a)
{
  recorder_info *rp;
  rp = get_recorder_info();
  if (a->type == INPUT_AMP)
    return(rp->in_amps[a->in][a->out]);
  else return(rp->out_amps[a->out]);
}

static char *amp_to_string(Float val)
{
  char *sfs;
  if (val == 0.0) return(record_zero);
  else if (val == 1.0) return(record_one);
  sfs = prettyf(val, 2);
  fill_number(sfs, amp_number_buffer);
  FREE(sfs);
  return(amp_number_buffer);
}

static void record_amp_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  AMP *ap = (AMP *)data;
  Float val;
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    val = ap->last_amp; 
  else val = 0.5;
  record_amp_changed(ap, val);
  GTK_ADJUSTMENT(ap->adj)->value = val;
  gtk_adjustment_value_changed(GTK_ADJUSTMENT(ap->adj));
}

static void record_amp_drag_callback(GtkAdjustment *adj, gpointer data)
{
  record_amp_changed((AMP *)data, adj->value);
}


/* ---------------- FILE INFO PANE ---------------- */

static void help_record_callback(GtkWidget *w, gpointer context) 
{
  recording_help((snd_state *)context);
}

static void internal_trigger_set(Float val)
{
  recorder_info *rp;
  rp = get_recorder_info();
  rp->trigger = val;
  rp->triggering = (val > 0.0);
  rp->triggered = (!rp->triggering);
  if (!(rp->recording)) /* else wait for current session to end (via click) */
    set_button_label(record_button, (rp->triggering) ? STR_Triggered_Record : STR_Record);
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
  PANE *p = (PANE *)context;
  int on, button;
  snd_state *ss;
#if defined(SGI) || defined(SUN)
  int other_button, j, n, i, output;
  float val[2];
  recorder_info *rp;
  rp = get_recorder_info();
#endif

  ss = p->ss;

#if defined(SGI) || defined(SUN)
  output = 0;
#endif

  button = (int)get_user_data(GTK_OBJECT(w));
  on = GTK_TOGGLE_BUTTON(w)->active;

#if OLD_SGI_AL
  /* on the older SGI's (and maybe newer Indy's?) digital input disables mic/line-in and vice versa */
  if (button == rp->digital_in_button)
    {
      set_line_source(p->ss, on);
      if (on == (GTK_TOGGLE_BUTTON(device_buttons[rp->microphone_button])->active))
	set_toggle_button(device_buttons[rp->microphone_button], !on, TRUE, (void *)(all_panes[rp->microphone_button])); 
      if (on == GTK_TOGGLE_BUTTON(device_buttons[rp->line_in_button])->active)
	set_toggle_button(device_buttons[rp->line_in_button], !on, TRUE, (void *)(all_panes[rp->line_in_button])); 
    }
  else
    {
      if (button == rp->microphone_button)
	{
	  if (on == (GTK_TOGGLE_BUTTON(device_buttons[rp->digital_in_button])->active))
	    {
	      set_toggle_button(device_buttons[rp->digital_in_button], !on, TRUE, (void *)(all_panes[rp->digital_in_button])); 
	      if (!(on == (GTK_TOGGLE_BUTTON(device_buttons[rp->line_in_button])->active)))
		set_toggle_button(device_buttons[rp->line_in_button], on, TRUE, (void *)(all_panes[rp->line_in_button])); 
	    }
	}
    }
#endif
#if NEW_SGI_AL || defined(SUN)
  output = (!(recorder_input_device(p->device)));
  if (!output)
    {
      if (on)
	{
  	  if (active_device_button != -1)
	    {
	      p = all_panes[active_device_button];
	      for (i = p->in_chan_loc, j = 0; j < p->in_chans; j++, i++) rp->input_channel_active[i] = 0;
	      if (active_device_button != button)
		{
		  set_toggle_button(device_buttons[active_device_button], FALSE, FALSE, (void *)(all_panes[active_device_button])); 
		}
	      if (rp->taking_input) close_recorder_audio();
	    }
	  active_device_button = button;
	  p = all_panes[button];
	  for (i = p->in_chan_loc, j = 0; j < p->in_chans; j++, i++) rp->input_channel_active[i] = 1;
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
	      rp->taking_input = 1;
	      set_read_in_progress(ss);
	    }
	}
  #ifndef SUN
      else
	{
	  if (on)
	    {
	      if (!rp->monitoring)
		{
		  rp->monitor_port = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_DAC_OUT,
							   rp->srate, rp->monitor_chans, rp->out_format, rp->buffer_size);
		  if (rp->monitor_port == -1)
		    {
		      record_report(messages, "open output", NULL);
		      rp->monitoring = 0;
		    }
		  else rp->monitoring = 1;
		}
	    }
	  else 
	    {
	      mus_audio_close(rp->monitor_port);
	      rp->monitoring = 0;
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

#if (HAVE_OSS || HAVE_ALSA)
static void save_audio_settings_callback(GtkWidget *w, gpointer context) 
{
  snd_state *ss = (snd_state *)context;
  recorder_info *rp;
  rp = get_recorder_info();
  set_toggle_button(w, FALSE, FALSE, (void *)ss);
  rp->mixer_settings_saved = 1;
  mus_audio_mixer_save(AUDIO_STATE_FILE);
}
#endif

static void srate_changed_callback(GtkWidget *w, gpointer context) 
{
  char *str;
  int n;
  snd_state *ss = (snd_state *)context;
  recorder_info *rp;
  rp = get_recorder_info();
  str = (char *)gtk_entry_get_text(GTK_ENTRY(recdat->srate_text)); /* w here gets segfault!! */
  if (str) 
    {
      n = string2int(str);
      if ((n > 0) && (n != rp->srate))
	{
	  rp->srate = n;
	  recorder_set_audio_srate(ss, MUS_AUDIO_DEFAULT, rp->srate, 0, rp->taking_input);
	}
    }
}

static void rec_size_changed_callback(GtkWidget *w, gpointer context) 
{
  char *str;
  int n;
  recorder_info *rp;
  rp = get_recorder_info();
  str = (char *)gtk_entry_get_text(GTK_ENTRY(rec_size_text));  /* w here gets segfault!! */
  if (str) 
    {
      n = string2int(str);
      if ((n > 0) && (n != rp->buffer_size)) set_record_size(n);
    }
}

static void make_file_info_pane(snd_state *ss, recorder_info *rp, GtkWidget *file_pane, int ndevs)
{
  int i;
  char *name;
  GtkWidget *file_label, *file_form, *button_frame, *button_holder, *duration_label, *rec_size_label, *ff_sep1, *ff_sep2, *ff_sep3, *autoload_file;
  GtkWidget *left_form, *right_form, *filebox, *durbox, *triggerbox;
#if (HAVE_OSS || HAVE_ALSA)
  GtkWidget *save_audio_settings;
#endif
#if defined(SGI) || defined(SUN)
  float val[1];
  int err;
#endif

  /* file_pane is the outer frame widget */

  file_form = gtk_hbox_new(FALSE, 0);
  gtk_container_add(GTK_CONTAINER(file_pane), file_form);
  gtk_widget_show(file_form);

  left_form = gtk_vbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(file_form), left_form, TRUE, TRUE, 0);
  gtk_widget_show(left_form);

  ff_sep1 = gtk_vseparator_new();
  gtk_box_pack_start(GTK_BOX(file_form), ff_sep1, FALSE, FALSE, 2);
  gtk_widget_show(ff_sep1);

  right_form = gtk_vbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(file_form), right_form, TRUE, TRUE, 0);
  gtk_widget_show(right_form);

  filebox = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(left_form), filebox, FALSE, FALSE, 0);
  gtk_widget_show(filebox);

  file_label = gtk_label_new(STR_file_p);
  gtk_box_pack_start(GTK_BOX(filebox), file_label, FALSE, FALSE, 0);
  gtk_widget_show(file_label);

  file_text = snd_entry_new(ss, filebox, TRUE);

  ff_sep3 = gtk_hseparator_new();
  gtk_box_pack_start(GTK_BOX(left_form), ff_sep3, FALSE, FALSE, 8);
  gtk_widget_show(ff_sep3);

  recdat = make_file_data_panel(ss, left_form, "data-form", TRUE, rp->output_header_type, rp->out_format, FALSE, TRUE);
  gtk_signal_connect_object(GTK_OBJECT(recdat->srate_text), "activate", GTK_SIGNAL_FUNC(srate_changed_callback), (GtkObject *)ss);

#if defined(SGI)
  err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_MICROPHONE, MUS_AUDIO_SRATE, 0, val);
  if (!err) rp->srate = val[0];
#endif
  mus_snprintf(timbuf, TIME_STR_SIZE, "%d", rp->srate);
  gtk_entry_set_text(GTK_ENTRY(recdat->srate_text), copy_string(timbuf));
  mus_snprintf(timbuf, TIME_STR_SIZE, "%d", rp->out_chans);
  gtk_entry_set_text(GTK_ENTRY(recdat->chans_text), copy_string(timbuf));

  durbox = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(right_form), durbox, FALSE, FALSE, 0);
  gtk_widget_show(durbox);

  duration_label = gtk_label_new(STR_duration_p);
  gtk_box_pack_start(GTK_BOX(durbox), duration_label, FALSE, FALSE, 0);
  gtk_widget_show(duration_label);

  file_duration = gtk_label_new("0.000");
  gtk_box_pack_start(GTK_BOX(durbox), file_duration, TRUE, TRUE, 0);
  gtk_widget_show(file_duration);
  
  rec_size_label = gtk_label_new("buf:");
  gtk_box_pack_start(GTK_BOX(durbox), rec_size_label, FALSE, FALSE, 0);
  gtk_widget_show(rec_size_label);

  rec_size_text = snd_entry_new(ss, durbox, TRUE);
  gtk_signal_connect_object(GTK_OBJECT(rec_size_text), "activate", GTK_SIGNAL_FUNC(rec_size_changed_callback), (GtkObject *)ss);
  mus_snprintf(timbuf, TIME_STR_SIZE, "%d", rp->buffer_size);
  gtk_entry_set_text(GTK_ENTRY(rec_size_text), timbuf);

  ff_sep2 = gtk_hseparator_new();
  gtk_box_pack_start(GTK_BOX(right_form), ff_sep2, FALSE, FALSE, 0);
  gtk_widget_show(ff_sep2);

  triggerbox = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_end(GTK_BOX(right_form), triggerbox, FALSE, FALSE, 0);
  gtk_widget_show(triggerbox);

  trigger_label = gtk_label_new("trigger:");
  gtk_box_pack_start(GTK_BOX(triggerbox), trigger_label, FALSE, FALSE, 4);
  gtk_widget_show(trigger_label);

  trigger_adj = gtk_adjustment_new(0.0, 0.0, 1.01, .01, .1, .01);
  trigger_scale = gtk_hscale_new(GTK_ADJUSTMENT(trigger_adj));
  gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(trigger_scale)), GTK_UPDATE_CONTINUOUS);
  gtk_scale_set_digits(GTK_SCALE(trigger_scale), 3);
  gtk_scale_set_draw_value(GTK_SCALE(trigger_scale), TRUE);
  gtk_scale_set_value_pos(GTK_SCALE(trigger_scale), GTK_POS_LEFT);
  gtk_box_pack_start(GTK_BOX(triggerbox), trigger_scale, TRUE, TRUE, 0);
  gtk_signal_connect(GTK_OBJECT(trigger_adj), "value_changed", GTK_SIGNAL_FUNC(change_trigger_callback), (GtkObject *)ss);
  gtk_widget_show(trigger_scale);

  /* buttons */
  
  button_frame = gtk_frame_new(NULL);
  gtk_box_pack_start(GTK_BOX(right_form), button_frame, TRUE, TRUE, 0);
  gtk_widget_show(button_frame);

  button_holder = gtk_vbox_new(TRUE, 0);
  gtk_container_add(GTK_CONTAINER(button_frame), button_holder);
  gtk_widget_show(button_holder);

  for (i = 0; i < ndevs; i++)
    {
      if ((rp->systems == 1) || (!(recorder_input_device(rp->ordered_devices[i]))))
	name = recorder_device_name(rp->ordered_devices[i]);
      else name = recorder_system_and_device_name(rp->ordered_systems[i], rp->ordered_devices[i]);
      device_buttons[i] = gtk_check_button_new_with_label(name);
      gtk_box_pack_start(GTK_BOX(button_holder), device_buttons[i], TRUE, TRUE, 0);
      gtk_widget_show(device_buttons[i]);
      set_user_data(GTK_OBJECT(device_buttons[i]), (gpointer)i);
      gtk_signal_connect(GTK_OBJECT(device_buttons[i]), "toggled", GTK_SIGNAL_FUNC(device_button_callback), (gpointer)all_panes[i]);
      set_toggle_button(device_buttons[i], TRUE, FALSE, (void *)(all_panes[i]));
    }

  autoload_file = gtk_check_button_new_with_label(STR_Autoload_Recording);
  gtk_box_pack_start(GTK_BOX(button_holder), autoload_file, TRUE, TRUE, 0);
  gtk_widget_show(autoload_file);
  device_buttons[ndevs] = autoload_file;
  /* we assume this is last in the device_buttons list in sensitize_control_buttons */
  rp->autoload_button = ndevs;
  gtk_signal_connect(GTK_OBJECT(autoload_file), "toggled", GTK_SIGNAL_FUNC(autoload_file_callback), (gpointer)ss);
  set_toggle_button(autoload_file, rp->autoload, FALSE, (void *)ss); 
#if (HAVE_OSS || HAVE_ALSA)
  save_audio_settings = gtk_check_button_new_with_label(STR_Save_Audio_Settings);
  gtk_box_pack_start(GTK_BOX(button_holder), save_audio_settings, TRUE, TRUE, 0);
  gtk_widget_show(save_audio_settings);
  gtk_signal_connect(GTK_OBJECT(save_audio_settings), "toggled", GTK_SIGNAL_FUNC(save_audio_settings_callback), (gpointer)ss);
#endif
}

void reflect_recorder_duration(Float new_dur)
{
  mus_snprintf(timbuf, TIME_STR_SIZE, "%.2f", new_dur);
  set_label(file_duration, timbuf);
}

void lock_recording_audio(void)
{
  if ((recorder) && (GTK_WIDGET_VISIBLE(recorder)))
    {
      close_recorder_audio();
      set_sensitive(record_button, FALSE);
      set_sensitive(reset_button, FALSE);
    }
}

void unlock_recording_audio(void)
{
  if ((recorder) && (GTK_WIDGET_VISIBLE(recorder)))
    {
      set_sensitive(record_button, TRUE);
      set_sensitive(reset_button, TRUE);
      set_button_label(reset_button, STR_Restart);
    }
}


/* -------------------------------- DEVICE PANE -------------------------------- */


static void vu_reset_callback(GtkWidget *w, gpointer context) 
{
  /* set current maxes to 0.0 */
  int i;
  PANE *p = (PANE *)context;
  VU *vu;
  for (i = 0; i < p->meters_size; i++)
    {
      vu = p->meters[i];
      vu->max_val = 0.0;
      set_label(vu->max_button, "0.00");
    }
}

static void meter_button_callback(GtkWidget *w, gpointer context) 
{
  Wdesc *wd = (Wdesc *)context;
  VU *vu;
  snd_state *ss;
  int val, i, n;
  char *str;
  PANE *p;
  recorder_info *rp;
  rp = get_recorder_info();
  p = wd->p;
  vu = p->meters[wd->chan];
  ss = vu->ss;
  if (vu->on_off == VU_OFF)
    {
      set_backgrounds(w, (ss->sgx)->red);
      vu->on_off = VU_ON;
      vu->red_deg = 0.0;
    }
  else 
    {
      set_backgrounds(w, (ss->sgx)->basic_color);
      vu->on_off = VU_OFF;
    }
  display_vu_meter(vu);
  val = (vu->on_off == VU_ON);
  p->active[wd->chan] = val;
  if (recorder_output_device(p->device))
    {
      rp->chan_out_active[wd->chan] = val;
      str = (char *)gtk_entry_get_text(GTK_ENTRY(recdat->chans_text)); 
      if (str) 
	n = string2int(str);
      else n = 0;
      val = 0;
      for (i = 0; i < p->active_size; i++) 
	if (p->active[i]) 
	  val++;
      if ((val > 0) && (val != n))
	{
	  mus_snprintf(timbuf, TIME_STR_SIZE, "%d", val);
	  gtk_entry_set_text(GTK_ENTRY(recdat->chans_text), timbuf);
#ifdef HAVE_ALSA
	  /* FIXME: this apparently is not necessary, we cannot
	   * change the number of recorded channels on the fly
	   * (but we can activate or deactivate them in the gui?
	       rp->out_chans = val;
	   */
#endif
	}
    }
  else rp->chan_in_active[wd->gain] = val;
}

static void volume_callback(GtkAdjustment *adj, gpointer context) 
{
  Wdesc *wd = (Wdesc *)context;
  set_mixer_gain(wd->system, wd->device, wd->chan, wd->gain, wd->field, 1.0 - adj->value);
}

/* ---- slider button matrix ---- */

static void make_recorder_slider(snd_state *ss, PANE *p, AMP *a, int input)
{
  GtkWidget *hb;

  hb = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(p->slider_box), hb, FALSE, FALSE, 0);
  gtk_widget_show(hb);

  a->label = gtk_button_new_with_label(gain_channel_name(p->in_chans, p->out_chans, input, a->device_in_chan, a->out));
  gtk_button_set_relief(GTK_BUTTON(a->label), GTK_RELIEF_NONE);
  gtk_box_pack_start(GTK_BOX(hb), a->label, FALSE, FALSE, 0);
  gtk_widget_show(a->label);
  gtk_signal_connect(GTK_OBJECT(a->label), "button_press_event", GTK_SIGNAL_FUNC(record_amp_click_callback), (gpointer)a);
  gtk_signal_connect(GTK_OBJECT(a->label), "enter_notify_event", GTK_SIGNAL_FUNC(recorder_noop_mouse_enter), NULL);

  a->number = gtk_button_new_with_label(amp_to_string(global_amp(a)));
  gtk_button_set_relief(GTK_BUTTON(a->number), GTK_RELIEF_NONE);
  gtk_box_pack_start(GTK_BOX(hb), a->number, FALSE, FALSE, 0);
  gtk_widget_show(a->number);
  gtk_signal_connect(GTK_OBJECT(a->number), "button_press_event", GTK_SIGNAL_FUNC(record_amp_click_callback), (gpointer)a);
  gtk_signal_connect(GTK_OBJECT(a->number), "enter_notify_event", GTK_SIGNAL_FUNC(recorder_noop_mouse_enter), NULL);

  a->adj = gtk_adjustment_new(amp_to_slider(global_amp(a)), 0.0, 1.00, 0.001, 0.01, .1);
  a->slider = gtk_hscrollbar_new(GTK_ADJUSTMENT(a->adj));
  gtk_box_pack_start(GTK_BOX(hb), a->slider, TRUE, TRUE, 6);
  set_background(a->slider, (ss->sgx)->position_color);
  gtk_signal_connect(GTK_OBJECT(a->adj), "value_changed", GTK_SIGNAL_FUNC(record_amp_drag_callback), (gpointer)a);
  gtk_widget_show(a->slider);

}

static void handle_matrix_slider(GtkWidget *mb, PANE *p, int bin, int bout, int curamp, int remove)
{
  AMP *a;
  snd_state *ss;
  a = p->amps[curamp];
  ss = p->ss;

  if (remove)
    {
      set_backgrounds(mb, (ss->sgx)->basic_color);
      p->active_sliders[bin][bout] = 0;
      gtk_widget_hide(a->label);
      gtk_widget_hide(a->number);
      gtk_widget_hide(a->slider);
    }
  else
    {
      set_backgrounds(mb, (ss->sgx)->green);
      p->active_sliders[bin][bout] = 1;
      if (a->label)
	{
	  gtk_widget_show(a->label);
	  gtk_widget_show(a->number);
	  gtk_widget_show(a->slider);
	}
      else
	make_recorder_slider(ss, p, a, TRUE);
    }
}

typedef struct {
  PANE *p;
  AMP *a;
  int in_chan, out_chan;
} slider_info;

static void matrix_button_callback(GtkWidget *mb, gpointer context) 
{
  slider_info *si = (slider_info *)context;
  PANE *p;
  int curamp;
  p = si->p;
  curamp = si->out_chan * p->in_chans + si->in_chan;
  handle_matrix_slider(mb, p, si->in_chan, si->out_chan, curamp, (p->active_sliders[si->in_chan][si->out_chan]));
}

static GtkWidget *make_button_matrix(snd_state *ss, PANE *p, char *name, GtkWidget *parent, Float meter_size)
{
  GtkWidget *outer_frame, *outer_vbox, *outer_hbox, *top_hbox, *left_vbox, *buttons;
  int ins = 2, outs = 2, row, col, vu_rows;
  int width, height;
  GtkWidget *outputs_label, *inputs_label0, *inputs_label1, *inputs_label2, *diag_button, *mb;
  slider_info *si;
  int **active_sliders;

  active_sliders = p->active_sliders;
  vu_rows = p->in_chans / 4;
  if (vu_rows == 0) vu_rows = 1;
  height = (int)(vu_rows*(3 * 2 + LIGHT_Y * meter_size));
  width = height;

  outer_frame = gtk_frame_new(NULL);
  gtk_box_pack_start(GTK_BOX(parent), outer_frame, FALSE, FALSE, 0);
  gtk_widget_show(outer_frame);

  outer_vbox = gtk_vbox_new(FALSE, 0);
  gtk_container_add(GTK_CONTAINER(outer_frame), outer_vbox);
  gtk_widget_show(outer_vbox);

  top_hbox = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(outer_vbox), top_hbox, FALSE, FALSE, 0);
  gtk_widget_show(top_hbox);

  outer_hbox = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(outer_vbox), outer_hbox, TRUE, TRUE, 0);
  gtk_widget_show(outer_hbox);

  left_vbox = gtk_vbox_new(FALSE, 0);
  set_widget_width(left_vbox, 10);
  gtk_box_pack_start(GTK_BOX(outer_hbox), left_vbox, FALSE, FALSE, 0);
  gtk_widget_show(left_vbox);

  buttons = gtk_table_new(ins, outs, TRUE);
  gtk_box_pack_start(GTK_BOX(outer_hbox), buttons, TRUE, TRUE, 0);
  set_widget_width(buttons, outs*30);
  gtk_widget_show(buttons);

  diag_button = gtk_label_new("/ ");
  gtk_box_pack_start(GTK_BOX(top_hbox), diag_button, FALSE, FALSE, 0);
  gtk_widget_show(diag_button);

  outputs_label = gtk_label_new("out");
  gtk_box_pack_start(GTK_BOX(top_hbox), outputs_label, TRUE, TRUE, 0);
  gtk_widget_show(outputs_label);

  inputs_label0 = gtk_label_new("i");
  gtk_box_pack_start(GTK_BOX(left_vbox), inputs_label0, FALSE, FALSE, 0);
  gtk_widget_show(inputs_label0);

  inputs_label1 = gtk_label_new("n");
  gtk_box_pack_start(GTK_BOX(left_vbox), inputs_label1, FALSE, FALSE, 0);
  gtk_widget_show(inputs_label1);

  inputs_label2 = gtk_label_new("s");
  gtk_box_pack_start(GTK_BOX(left_vbox), inputs_label2, FALSE, FALSE, 0);
  gtk_widget_show(inputs_label2);

  ins = p->in_chans;
  outs = p->out_chans;
  p->matrix_buttons = (GtkWidget ***)CALLOC(ins, sizeof(GtkWidget **));
  for (row = 0; row < ins; row++) p->matrix_buttons[row] = (GtkWidget **)CALLOC(outs, sizeof(GtkWidget *));

  for (col = 0; col < outs; col++)
    for (row = 0; row < ins; row++)
      {
	si = (slider_info *)CALLOC(1, sizeof(slider_info));
	si->p = p;
	si->in_chan = row;
	si->out_chan = col;

	mb = gtk_button_new();
	if (row == col)
	  set_backgrounds(mb, (ss->sgx)->green);
	else set_backgrounds(mb, (ss->sgx)->basic_color);
	gtk_table_attach_defaults(GTK_TABLE(buttons), mb, col, col + 1, row, row + 1);
	gtk_widget_show(mb);
	gtk_signal_connect(GTK_OBJECT(mb), "clicked", GTK_SIGNAL_FUNC(matrix_button_callback), (gpointer)si);
	p->matrix_buttons[row][col] = mb;
      }
  return(outer_frame);
}

/* -------- I/O pane -------- */

/* these functions are used only by make_pane */
static int make_amp_sliders(snd_state *ss, recorder_info *rp, PANE *p, int input, int overall_input_ctr);
static void make_reset_button(snd_state *ss, PANE *p, GtkWidget *btab);
static GtkWidget *make_button_box(snd_state *ss, recorder_info *rp, PANE *p, Float meter_size,
			      int input, int overall_input_ctr, int vu_meters, GtkWidget *vuh);
static void make_vertical_gain_sliders(snd_state *ss, recorder_info *rp, PANE *p, 
					     int num_gains, int gain_ctr, int *mixflds, int input, GtkWidget *gv);
static void make_vu_meters(snd_state *ss, PANE *p, int vu_meters,
			   int overall_input_ctr, Float meter_size, int input, GtkWidget *vuh);

void recorder_fill_wd(void *uwd, int chan, int field, int device)
{
  Wdesc *wd = (Wdesc *)uwd;
  wd->chan = chan;
  wd->field = field;
  wd->device = device;
}

static PANE *make_pane(snd_state *ss, recorder_info *rp, GtkWidget *paned_window, int device, int system)
{
  /* VU meters (frame, then drawing area widget) */
  int i, k;
  PANE *p;
  GtkWidget *matrix_frame;
  GtkWidget *vuh, *vuv, *gv, *btab;

  int vu_meters, num_gains, input;
  int pane_max;
  Float meter_size;
  state_context *sx;
  int mixflds[MAX_AUDIO_FIELD];
  static int gain_ctr = 0;
  static int overall_input_ctr = 0;

  sx = ss->sgx;
  p = (PANE *)CALLOC(1, sizeof(PANE));
  p->device = device;
  p->system = system;
  p->ss = ss;
  vu_meters = recorder_check_device(system, device, mixer_gains_posted, tone_controls_posted, mixflds, &num_gains, &input);

  if (input) 
    {
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
      if (vu_meters < rp->out_chans) vu_meters = rp->out_chans;
      p->out_chans = vu_meters;
      p->in_chans = 1;
    }

  p->meters = (VU **)CALLOC(vu_meters, sizeof(VU *));
  p->meters_size = vu_meters;
  p->active = (int *)CALLOC(vu_meters, sizeof(int));
  p->active_size = vu_meters;
  p->active_sliders = (int **)CALLOC(p->in_chans, sizeof(int *));
  for (i = 0; i < p->in_chans; i++) p->active_sliders[i] = (int *)CALLOC(p->out_chans, sizeof(int));

  /* paned_window is a vbox = stack of panes, each pane is hbox = meters, sliders | gains */
  p->pane = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(paned_window), p->pane, TRUE, TRUE, 6);
  gtk_widget_show(p->pane);

  meter_size = vu_size(ss);
  if (vu_meters > 4) meter_size *= .6; else if (vu_meters > 2) meter_size *= .8;
  if ((vu_meters%5) == 0) meter_size *= 0.8;

  vuv = gtk_vbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(p->pane), vuv, TRUE, TRUE, 0);
  gtk_widget_show(vuv);
  p->slider_box = vuv;

  gv = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_end(GTK_BOX(p->pane), gv, FALSE, FALSE, 0);
  gtk_widget_show(gv);

  /* now left side = vuv divided vuh then sliders, right side = gv */

  vuh = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vuv), vuh, FALSE, FALSE, 0);
  gtk_widget_show(vuh);

  if ((input) && ((p->in_chans*p->out_chans) > 8))
    {
      for (i = 0; i < p->in_chans; i++) 
	for (k = 0; k < p->out_chans; k++) 
	  if (i == k) 
	    p->active_sliders[i][k] = 1;

      /* rather than default to posting 64 (or 256!) sliders, set up a channel matrix where desired sliders can be set */
      matrix_frame = make_button_matrix(ss, p, "channel-matrix", vuh, meter_size);
    }
  else 
    {
      for (i = 0; i < p->in_chans; i++) 
	for (k = 0; k < p->out_chans; k++) 
	  p->active_sliders[i][k] = 1;
    }

  make_vu_meters(ss, p, vu_meters, overall_input_ctr, meter_size, input, vuh);

  btab = make_button_box(ss, rp, p, meter_size, input, overall_input_ctr, vu_meters, vuh);

  make_reset_button(ss, p, btab);

  if (num_gains > 0)
    {
      make_vertical_gain_sliders(ss, rp, p, num_gains, gain_ctr, mixflds, input, gv);
      gain_ctr += num_gains;
    }

  /* now the amp sliders across the bottom of the pane, with 'mixer' info on the right */
  pane_max = make_amp_sliders(ss, rp, p, input, overall_input_ctr);

  p->in_chan_loc = overall_input_ctr;
  if (input) overall_input_ctr += p->in_chans;
#if (HAVE_OSS || HAVE_ALSA)
  p->pane_size = pane_max+20;
#else
  p->pane_size = pane_max+50;
#endif

  return(p);
}

static void make_vu_meters(snd_state *ss, PANE *p, int vu_meters,
			   int overall_input_ctr, Float meter_size, int input, GtkWidget *vuh)
{
  int i, columns, row, rows;
  GtkWidget *meter;
  VU *vu;
  GtkWidget **hboxes, *vbox, *frame;

  vbox = gtk_vbox_new(TRUE, 0);
  gtk_box_pack_start(GTK_BOX(vuh), vbox, input, input, 0);
  gtk_widget_show(vbox);

  /* need secondary set of boxes if vu_meters > 4 */

  columns = recorder_columns(vu_meters);
  rows = vu_meters / columns;
  hboxes = (GtkWidget **)CALLOC(rows, sizeof(GtkWidget *));
  for (i = 0; i < rows; i++) 
    {
      hboxes[i] = gtk_hbox_new(TRUE, 0);
      gtk_box_pack_start(GTK_BOX(vbox), hboxes[i], TRUE, TRUE, 0);
      gtk_widget_show(hboxes[i]);
    }

  row = 0;

  for (i = 0; i < vu_meters; i++)
    {

      frame = gtk_frame_new(NULL);

      gtk_box_pack_start(GTK_BOX(hboxes[row]), frame, TRUE, TRUE, 0);
      gtk_container_set_border_width(GTK_CONTAINER(frame), 4);
      gtk_widget_show(frame);

      meter = gtk_drawing_area_new();
      gtk_container_add(GTK_CONTAINER(frame), meter);
      SG_SET_DRAWING_AREA_SIZE(GTK_DRAWING_AREA(meter), (int)(240 * meter_size), (int)(100 * meter_size));
      gtk_widget_show(meter);

      p->meters[i] = make_vu_meter(meter, LIGHT_X, LIGHT_Y, CENTER_X, CENTER_Y, ss, meter_size);
      vu = p->meters[i];
      if (input)
	rec_in_VU[overall_input_ctr + i] = vu;
      else rec_out_VU[i] = vu;

      gtk_signal_connect(GTK_OBJECT(meter), "expose_event", GTK_SIGNAL_FUNC(meter_expose_callback), (gpointer)vu);
      gtk_signal_connect(GTK_OBJECT(meter), "configure_event", GTK_SIGNAL_FUNC(meter_resize_callback), (gpointer)vu);

      if ((i == (columns*(row + 1) - 1)) && 
	  (vu_meters > (i + 1))) 
	row++;
    }
}

static void make_vertical_gain_sliders(snd_state *ss, recorder_info *rp, PANE *p, 
				       int num_gains, int gain_ctr, int *mixflds, int input, GtkWidget *gv)
{
  int i, chan, this_device = 0, last_device = 0;
  GtkWidget *sbox, *slabel, *spix;
  Float vol;
  Wdesc *wd;

  last_device = -1;

  for (i = 0, chan = num_gains-1; i < num_gains; i++, chan--)
    {
      wd = (Wdesc *)CALLOC(1, sizeof(Wdesc));
      wd->system = p->system;
      this_device = recorder_sort_mixer_device((void *)wd, i, chan, input, p->device, mixflds);
      wd->ss = ss;
      wd->p = p;
      wd->gain = gain_ctr+chan;
      if (wd->gain > rp->num_mixer_gains) 
	snd_error("%s[%d] %s: overflow %d > %d", 
		  __FILE__, __LINE__, __FUNCTION__, 
		  wd->gain, rp->num_mixer_gains);
      gain_sliders[wd->gain] = wd;
      vol = mixer_gain(wd->system, wd->device, wd->chan, wd->gain, wd->field);
      if (vol < 0.0) vol = 0.0;
      if (vol > 1.0) vol = 1.0;
      
      sbox = gtk_vbox_new(FALSE, 0);
      gtk_box_pack_end(GTK_BOX(gv), sbox, FALSE, FALSE, 0);
      gtk_widget_show(sbox);

#if (HAVE_OSS || HAVE_ALSA)
      if (last_device != this_device)
	{
	  if ((wd->device == MUS_AUDIO_MICROPHONE) ||
	      (wd->device == MUS_AUDIO_LINE_IN) ||
	      (wd->device == MUS_AUDIO_SPEAKERS) ||
	      (wd->device == MUS_AUDIO_DAC_OUT) ||
	      (wd->device == MUS_AUDIO_CD))
	    {
	      slabel = gtk_button_new();
	      gtk_button_set_relief(GTK_BUTTON(slabel), GTK_RELIEF_NONE);
	      set_background(slabel, (ss->sgx)->basic_color);
	      gtk_box_pack_start(GTK_BOX(sbox), slabel, FALSE, FALSE, 0);
	      gtk_widget_show(slabel);
	      
	      spix = gtk_pixmap_new(device_pix(wd->device), device_mask(wd->device));
	      gtk_container_add (GTK_CONTAINER(slabel), spix);
	      gtk_widget_show(spix);
	    }
	  else
	    {
	      if ((!input) && (this_device == MUS_AUDIO_DAC_FILTER))
		slabel = gtk_label_new("ton");
	      else slabel = gtk_label_new(recorder_field_abbreviation(this_device));
	      gtk_box_pack_start(GTK_BOX(sbox), slabel, FALSE, FALSE, 0);
	      set_label_font(slabel);
	      gtk_widget_show(slabel);
	    }
	}
      else
	{
	  slabel = gtk_label_new(" ");
	  gtk_box_pack_start(GTK_BOX(sbox), slabel, FALSE, FALSE, 0);
	  set_label_font(slabel);
	  gtk_widget_show(slabel);
	}
#endif
      wd->adj = gtk_adjustment_new(1.0 - vol, 0.0, 1.01, 0.001, 0.01, .01);
      wd->wg = gtk_vscale_new(GTK_ADJUSTMENT(wd->adj));
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(wd->wg)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_draw_value(GTK_SCALE(wd->wg), FALSE);
      gtk_box_pack_start(GTK_BOX(sbox), wd->wg, TRUE, TRUE, 0);
      set_backgrounds(wd->wg, (ss->sgx)->zoom_color);
      gtk_widget_show(wd->wg);
      gtk_signal_connect(GTK_OBJECT(wd->adj), "value_changed", GTK_SIGNAL_FUNC(volume_callback), (gpointer)wd);
#if (HAVE_OSS || HAVE_ALSA)
      last_device = this_device;
#endif
    }
}

static GtkWidget *make_button_box(snd_state *ss, recorder_info *rp, PANE *p, Float meter_size,
			      int input, int overall_input_ctr, int vu_meters, GtkWidget *vuh)
{
  int i, row, columns, button_size, rows;
  GtkWidget *btab, *button_label, *max_label, *chan_label, *bbox, *hsep;
  Wdesc *wd;
  VU *vu;
  GtkWidget **hboxes;

  btab = gtk_vbox_new(FALSE, 0);
  /* gtk_box_pack_start(GTK_BOX(vuh), btab, !input, !input, 10); */
  gtk_box_pack_start(GTK_BOX(vuh), btab, TRUE, TRUE, 10);
  gtk_widget_show(btab);

  if ((rp->systems == 1) || (!input))
    button_label = gtk_label_new(recorder_device_name(p->device));
  else button_label = gtk_label_new(mus_audio_system_name(p->system));
  gtk_box_pack_start(GTK_BOX(btab), button_label, FALSE, FALSE, 0);
  gtk_widget_show(button_label);

  /* need secondary set of boxes if vu_meters > 4 */

  columns = recorder_columns(vu_meters);
  rows = vu_meters / columns;
  hboxes = (GtkWidget **)CALLOC(rows, sizeof(GtkWidget *));
  for (i = 0; i < rows; i++) 
    {
      hboxes[i] = gtk_hbox_new(FALSE, 0);
      gtk_box_pack_start(GTK_BOX(btab), hboxes[i], TRUE, TRUE, 0);
      gtk_widget_show(hboxes[i]);
    }

  p->on_buttons = (GtkWidget **)CALLOC(vu_meters, sizeof(GtkWidget *));
  p->on_buttons_size = vu_meters;

  columns = recorder_columns(vu_meters);
  row = 0;
  button_size = 100 / columns;

  for (i = 0; i < vu_meters; i++)
    {
      p->on_buttons[i] = gtk_button_new();
      /* gtk_container_set_border_width(GTK_CONTAINER(p->on_buttons[i]), 6); */
      set_background(p->on_buttons[i], (ss->sgx)->basic_color);
      gtk_box_pack_start(GTK_BOX(hboxes[row]), p->on_buttons[i], TRUE, TRUE, 0);
      /* set_widget_width(p->on_buttons[i], 30); */
      gtk_widget_show(p->on_buttons[i]);

      bbox = gtk_vbox_new(FALSE, 0);
      gtk_container_add(GTK_CONTAINER(p->on_buttons[i]), bbox);
      gtk_widget_show(bbox);

      chan_label = gtk_label_new(channel_name(p->in_chans, p->out_chans, i));
      gtk_box_pack_start(GTK_BOX(bbox), chan_label, TRUE, TRUE, 1);
      gtk_widget_show(chan_label);

      hsep = gtk_hseparator_new();
      gtk_box_pack_start(GTK_BOX(bbox), hsep, FALSE, FALSE, 2);
      gtk_widget_show(hsep);

      max_label = gtk_label_new("0.000");
      gtk_box_pack_start(GTK_BOX(bbox), max_label, TRUE, TRUE, 1);
      gtk_widget_show(max_label);

      wd = (Wdesc *)CALLOC(1, sizeof(Wdesc));
      wd->chan = i;
      wd->gain = i + overall_input_ctr;
      wd->ss = ss;
      wd->p = p;
      wd->device = p->device;
      wd->system = p->system;
      wd->field = MUS_AUDIO_AMP;
      vu = p->meters[i];
      vu->max_button = max_label;
      vu->max_val = 0.0;
      gtk_signal_connect(GTK_OBJECT(p->on_buttons[i]), "clicked", GTK_SIGNAL_FUNC(meter_button_callback), (gpointer)wd);

      if ((i == (columns*(row + 1) - 1)) && (vu_meters > (i + 1))) 
	row++;
    }
  return(btab);
}

static void make_reset_button(snd_state *ss, PANE *p, GtkWidget *btab)
{
  p->reset_button = gtk_button_new_with_label(STR_Reset);
  set_background(p->reset_button, (ss->sgx)->basic_color);
  gtk_box_pack_start(GTK_BOX(btab), p->reset_button, TRUE, TRUE, 0);
  gtk_widget_show(p->reset_button);
  gtk_signal_connect(GTK_OBJECT(p->reset_button), "clicked", GTK_SIGNAL_FUNC(vu_reset_callback), (gpointer)p);
}

static int make_amp_sliders(snd_state *ss, recorder_info *rp, PANE *p, int input, int overall_input_ctr)
{
  Wdesc *wd;
  int i, amp_sliders, temp_out_chan, temp_in_chan, in_chan, out_chan;
  AMP *a;
  GtkWidget *amp_sep;

  amp_sep = gtk_hseparator_new();
  gtk_box_pack_start(GTK_BOX(p->slider_box), amp_sep, FALSE, FALSE, 6);
  gtk_widget_show(amp_sep);

  if (input) 
    amp_sliders = p->in_chans * p->out_chans;
  else amp_sliders = p->out_chans;
  p->amps = (AMP **)CALLOC(amp_sliders, sizeof(AMP *));
  p->amps_size = amp_sliders;
  
  /* input numbering starts at overall_input_ctr and goes up for p->in_chans */
  /* output numbering starts at 0 and goes for p->out_chans */
  /* if input, do direct cases first, then fill in rest of 1, rest of 2 etc */
  temp_out_chan = 0;
  temp_in_chan = 0;
  for (i = 0; i < amp_sliders; i++)
    {
      in_chan = temp_in_chan;
      out_chan = temp_out_chan;
      wd = (Wdesc *)CALLOC(1, sizeof(Wdesc));
      wd->chan = i;
      wd->ss = ss;
      wd->p = p;
      wd->device = p->device;
      wd->system = p->system;
      wd->field = MUS_AUDIO_AMP;
      p->amps[i] = (AMP *)CALLOC(1, sizeof(AMP));
      a = p->amps[i];
      if (input) 
	{
	  a->type = INPUT_AMP; 
	  a->in = temp_in_chan + overall_input_ctr;
	  a->device_in_chan = temp_in_chan;
	  a->out = temp_out_chan;
	  if (temp_in_chan == temp_out_chan)
	    rp->in_amps[a->in][a->out] = 1.0;
	  else rp->in_amps[a->in][a->out] = 0.0;
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
	  rp->out_amps[i] = 1.0;
	  AMP_rec_outs[i] = a;
	}

      a->wd = wd;
      if ((!input) || (p->active_sliders[in_chan][out_chan]))
	make_recorder_slider(ss, p, a, input);
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
	set_sensitive(device_buttons[i], TRUE);
    }
}

void unsensitize_control_buttons(void)
{
  int i;
  for (i = 0; i < device_buttons_size - 1; i++)
    {
      if (device_buttons[i])
	set_sensitive(device_buttons[i], FALSE);
    }
}

static void reset_record_callback(GtkWidget *w, gpointer context) 
{
  /* if recording, cancel and toss data, else reset various fields to default (ss) values */
  snd_state *ss = (snd_state *)context;
  char *str;
  PANE *p;
  VU *vu;
  int i, k;
  recorder_info *rp;
  rp = get_recorder_info();
  if (rp->recording)                  /* cancel */
    {
      rp->recording = 0;
      rp->triggered = (!rp->triggering);
      sensitize_control_buttons();
      set_button_label(reset_button, STR_Reset);
      set_backgrounds(record_button, (ss->sgx)->basic_color);
      set_button_label(record_button, (rp->triggering) ? STR_Triggered_Record : STR_Record);
      mus_file_close(rp->output_file_descriptor);
      rp->output_file_descriptor = -1;
      str = just_filename(rp->output_file);
      record_report(messages, str, " recording cancelled", NULL);
      FREE(str);
      snd_remove(rp->output_file);
    }
  else                            /* reset or restart */
    { 
      for (i = 0; i < rp->ordered_devices_size; i++)
	{
	  p = all_panes[i];
	  for (k = 0; k < p->meters_size; k++)
	    {
	      vu = p->meters[k];
	      vu->max_val = 0.0;
	      set_label(vu->max_button, "0.00");
	    }
	}
      /* now if dac turned us off, turn everything back on */
      if (!(rp->taking_input))            /* restart */
	{
	  fire_up_recorder(ss);
	  set_button_label(reset_button, STR_Reset);
	}
    }
}

static void dismiss_record_callback(GtkWidget *w, gpointer context) 
{
  snd_state *ss = (snd_state *)context;
  state_context *sgx;
  recorder_info *rp;
  rp = get_recorder_info();
  sgx = ss->sgx;
  if (rp->recording) reset_record_callback(w, context);
  gtk_widget_hide(recorder);
  close_recorder_audio();
#if (!(HAVE_OSS || HAVE_ALSA))
  mus_audio_restore();
#endif
}

void finish_recording(snd_state *ss, recorder_info *rp)
{
  char *str;
  snd_info *sp;
  Float duration;
  sensitize_control_buttons();
  set_backgrounds(record_button, (ss->sgx)->basic_color);
  set_button_label(reset_button, STR_Reset);
  set_button_label(record_button, (rp->triggering) ? STR_Triggered_Record : STR_Record);
  mus_file_close(rp->output_file_descriptor);
  rp->output_file_descriptor = mus_file_reopen_write(rp->output_file);
  mus_header_update_with_fd(rp->output_file_descriptor,
			    rp->output_header_type,
			    rp->total_output_frames * rp->out_chans * mus_data_format_to_bytes_per_sample(rp->out_format)); 
  close(rp->output_file_descriptor);
  rp->output_file_descriptor = -1;
  duration = (Float)rp->total_output_frames / (Float)(rp->srate);
  reflect_recorder_duration(duration);
  str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  mus_snprintf(str, PRINT_BUFFER_SIZE, "recorded %s:\n  duration: %.2f\n  srate: %d, chans: %d\n  type: %s, format: %s",
	       rp->output_file, duration, rp->srate, rp->out_chans,
	       mus_header_type_name(rp->output_header_type), 
	       mus_data_format_name(rp->out_format));
  record_report(messages, str, NULL);
  FREE(str);
  if (rp->autoload)
    {
      if ((sp = find_sound(ss, rp->output_file)))
	snd_update(ss, sp);
      else snd_open_file(rp->output_file, ss, FALSE);
    }
}

static void record_button_callback(GtkWidget *w, gpointer context) 
{
  snd_state *ss = (snd_state *)context;
  Wdesc *wd;
  int i, old_srate, ofmt, rs, ochns, oloc;
  static char *comment;
  char *str;
  PANE *p;
  recorder_info *rp;
  rp = get_recorder_info();
  rp->recording = (!(rp->recording));
  if (rp->recording)
    {
      if (!(rp->taking_input)) fire_up_recorder(ss);
      str = (char *)gtk_entry_get_text(GTK_ENTRY(file_text));
      if ((str) && (*str))
	{
	  if (rp->output_file) FREE(rp->output_file);
	  rp->output_file = mus_expand_filename(str);
	  str = NULL;
	  old_srate = rp->srate;
	  str = read_file_data_choices(recdat, &rs, &ochns, &rp->output_header_type, &ofmt, &oloc); 
	  if (str) FREE(str);
	  str = NULL;
	  rp->out_format = ofmt;
	  rp->out_chans = ochns;
	  if (rs != old_srate) 
	    {
	      rp->srate = rs;
	      recorder_set_audio_srate(ss, MUS_AUDIO_DEFAULT, rp->srate, 0, rp->taking_input);
	    }

	  if (rp->out_chans <= 0)
	    {
	      record_report(messages, "can't record: you screwed up the output channel number!", NULL);
	      rp->recording = 0;
	      rp->triggered = (!rp->triggering);
	      return;
	    }

	  comment = gtk_editable_get_chars(GTK_EDITABLE(recdat->comment_text), 0, -1);
	  reflect_recorder_duration(0.0);
	  
	  if (out_chans_active() != rp->out_chans)
	    {
	      if (msgbuf == NULL) msgbuf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
	      mus_snprintf(msgbuf, PRINT_BUFFER_SIZE,
			   "chans field (%d) doesn't match file out panel (%d channels active)", 
			   rp->out_chans, out_chans_active());
	      record_report(messages, msgbuf, NULL);
	      wd = (Wdesc *)CALLOC(1, sizeof(Wdesc));
	      wd->ss = ss;

	      wd->p = all_panes[out_file_pane];
	      p = wd->p;
	      wd->field = MUS_AUDIO_AMP;
	      wd->device = p->device;
	      wd->system = 0;
	      for (i = 0; i < rp->out_chans; i++)
		if (!(p->active[i]))
		  {
		    wd->chan = i;
		    meter_button_callback(p->on_buttons[i], (gpointer)wd); /* callData not used */
		  }
	      FREE(wd);
	    }
	  if (in_chans_active() == 0)
	    {
	      record_report(messages, "can't record: no inputs enabled", NULL);
	      rp->recording = 0;
	      rp->triggered = (!rp->triggering);
	      return;
	    }
	  set_backgrounds(record_button, (ss->sgx)->red);
	  set_button_label(reset_button, STR_Cancel);
	  set_button_label(record_button, STR_Done);

	  if (recorder_start_output_file(ss, comment)) return; /* true = error */
	}
      else
	{
	  record_report(messages, "can't record: no output file name supplied", NULL);
	  rp->recording = 0;
	  rp->triggered = (!rp->triggering);
	  return;
	}
    }
  else 
    finish_recording(ss, rp);
}

static void initialize_recorder(recorder_info *rp);
static GtkWidget *rec_panes, *file_info_pane;

#define AUDVAL_SIZE 64

static void recorder_delete(GtkWidget *w, GdkEvent *event, gpointer context)
{
  dismiss_record_callback(w, context);
}

void snd_record_file(snd_state *ss)
{
  int n, i, device, input_devices, output_devices, system;
  GdkDrawable *wn;
  state_context *sx;
  GtkWidget *rec_panes_box, *help_button, *dismiss_button;

  recorder_info *rp;
  rp = get_recorder_info();

  if (!recorder)
    {
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

      input_devices = recorder_get_devices(rp, &output_devices);
      if (input_devices == -1) return;

      all_panes = (PANE **)CALLOC(input_devices + 1, sizeof(PANE *));
      device_buttons_size = input_devices + 2; /* inputs, one output, autoload_file */
      device_buttons = (GtkWidget **)CALLOC(device_buttons_size, sizeof(GtkWidget *));
      gain_sliders = (Wdesc **)CALLOC(rp->num_mixer_gains, sizeof(Wdesc *));
      /* out_file_pane will be the bottom (output) audio pane, not the file info pane */

      recorder_characterize_devices(input_devices + 1, output_devices);

      rec_in_VU = (VU **)CALLOC(rp->possible_input_chans, sizeof(VU *));
      rec_out_VU = (VU **)CALLOC(MAX_OUT_CHANS, sizeof(VU *));

      AMP_rec_ins = (AMP ***)CALLOC(rp->possible_input_chans, sizeof(AMP **));
      for (i = 0; i < rp->possible_input_chans; i++) 
	AMP_rec_ins[i] = (AMP **)CALLOC(MAX_OUT_CHANS, sizeof(AMP *));
      AMP_rec_outs = (AMP **)CALLOC(MAX_OUT_CHANS, sizeof(AMP *));
      small_font = gdk_font_load((vu_size(ss) < SMALLER_FONT_CUTOFF) ? SMALLER_FONT : SMALL_FONT);

      recorder = gtk_dialog_new();
      gtk_signal_connect(GTK_OBJECT(recorder), "delete_event", GTK_SIGNAL_FUNC(recorder_delete), (gpointer)ss);
      gtk_window_set_title(GTK_WINDOW(recorder), STR_Record);
      SG_MAKE_RESIZABLE(recorder);
      set_background(recorder, (ss->sgx)->basic_color);
      gtk_container_set_border_width (GTK_CONTAINER(recorder), 10);
      gtk_widget_realize(recorder);

      help_button = gtk_button_new_with_label(STR_Help);
      dismiss_button = gtk_button_new_with_label(STR_Dismiss);
      reset_button = gtk_button_new_with_label(STR_Reset);
      record_button = gtk_button_new_with_label(STR_Record);
      set_backgrounds(record_button, (ss->sgx)->basic_color);

      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(recorder)->action_area), dismiss_button, TRUE, TRUE, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(recorder)->action_area), reset_button, TRUE, TRUE, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(recorder)->action_area), record_button, TRUE, TRUE, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(recorder)->action_area), help_button, TRUE, TRUE, 10);

      gtk_signal_connect(GTK_OBJECT(dismiss_button), "clicked", GTK_SIGNAL_FUNC(dismiss_record_callback), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(help_button), "clicked", GTK_SIGNAL_FUNC(help_record_callback), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(reset_button), "clicked", GTK_SIGNAL_FUNC(reset_record_callback), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(record_button), "clicked", GTK_SIGNAL_FUNC(record_button_callback), (gpointer)ss);
      set_pushed_button_colors(help_button, ss);
      set_pushed_button_colors(dismiss_button, ss);
      set_pushed_button_colors(reset_button, ss);
      gtk_widget_show(dismiss_button);
      gtk_widget_show(reset_button);
      gtk_widget_show(record_button);
      gtk_widget_show(help_button);

      rec_panes = gtk_vpaned_new();
      set_backgrounds(rec_panes, (ss->sgx)->sash_color);
      gtk_container_add(GTK_CONTAINER(GTK_DIALOG(recorder)->vbox), rec_panes);
      gtk_widget_show(rec_panes);

      rec_panes_box = gtk_vbox_new(FALSE, 0);
      gtk_paned_add1(GTK_PANED(rec_panes), rec_panes_box);
      gtk_widget_show(rec_panes_box);

      gtk_widget_show(recorder);
      make_record_icons(recorder, ss);

      /* open all audio devices and collect ins/out etc */
      out_file_pane = (rp->ordered_devices_size - 1);

      /* see note above (line 2809) about recorder out chans */
      n = device_channels(MUS_AUDIO_PACK_SYSTEM(rp->ordered_systems[out_file_pane]) |
			  rp->ordered_devices[out_file_pane]);
      if ((rp->out_chans == DEFAULT_RECORDER_OUT_CHANS) && (n > 2))
	rp->out_chans = n;

      for (i = 0; i < rp->ordered_devices_size; i++)
	{
	  /* last one is output */
	  device = rp->ordered_devices[i];
	  system = rp->ordered_systems[i];
	  all_panes[i] = make_pane(ss, rp, rec_panes_box, device, system);
	}

      /* then make file_info_pane and messages at the bottom */
      file_info_pane = gtk_frame_new(NULL);
      gtk_box_pack_end(GTK_BOX(rec_panes_box), file_info_pane, FALSE, FALSE, 0);
      gtk_widget_show(file_info_pane);

      make_file_info_pane(ss, rp, file_info_pane, rp->ordered_devices_size);
      messages = make_scrolled_text(ss, NULL, FALSE, NULL, rec_panes);
      set_dialog_widget(ss, RECORDER_DIALOG, recorder);
      initialize_recorder(rp);
    }
  gtk_widget_show(recorder);

  if (pending_errors > 0)
    {
      for (i = 0; i < pending_errors; i++)
	{
	  record_report(messages, pending_error[i], NULL);
	  FREE(pending_error[i]);
	}
      pending_errors = 0;
      FREE(pending_error);
      pending_error_size = 0;
    }
  
  if (!(rp->taking_input)) fire_up_recorder(ss);
}

void set_recorder_autoload(recorder_info *rp, int val)
{
  rp->autoload = val;
  if (recorder) 
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(device_buttons[rp->autoload_button]), val);
}

void reflect_recorder_in_amp(int in, int out, Float val)
{
  Float temp;
  AMP *a;
  if (recorder)
    {
      a = AMP_rec_ins[in][out];
      temp = amp_to_slider(val); 
      record_amp_changed(a, temp); 
      GTK_ADJUSTMENT(a->adj)->value = val;
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(a->adj));
    }
}

void reflect_recorder_out_amp(int ind, Float val)
{
  Float temp;
  AMP *a;
  if (recorder)
    {
      a = AMP_rec_outs[ind];
      temp = amp_to_slider(val); 
      record_amp_changed(a, temp);
      GTK_ADJUSTMENT(a->adj)->value = val;
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(a->adj));
    }
}

void reflect_recorder_mixer_gain(int ind, Float val)
{
  Wdesc *wd;
  if (recorder)
    {
      wd = gain_sliders[ind];
      GTK_ADJUSTMENT(wd->adj)->value = val;
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(wd->adj));
      set_mixer_gain(wd->system, wd->device, wd->chan, wd->gain, wd->field, val);
    }
}

static void initialize_recorder(recorder_info *rp)
{
  /* picked up initial (widget) values from globals vars */
  int i;
  /* special case initializations */
#if OLD_SGI_AL
  /* in this case, digital in blocks out the others */
  long sb[2];
  int err;
  int in_digital = 0;
#endif
  for (i = 0; i < MAX_SOUNDCARDS; i++) rp->raw_input_bufs[i] = NULL;
#if OLD_SGI_AL
  sb[0] = AL_INPUT_SOURCE;
  err = ALgetparams(AL_DEFAULT_DEVICE, sb, 2);
  if ((!err) && (sb[1] == AL_INPUT_DIGITAL)) in_digital = 1;
  set_toggle_button(device_buttons[rp->digital_in_button], 
		    in_digital, FALSE, 
		    (void *)(all_panes[rp->digital_in_button]));
  device_button_callback(device_buttons[rp->digital_in_button], 
			 all_panes[rp->digital_in_button]);
#endif
#if NEW_SGI_AL || defined(SUN)
  /* for simplicity, and until someone complains, new SGI AL machines will just have one active input device */
  active_device_button = rp->microphone_button;
  for (i = 0; i < device_buttons_size - 1; i++)
    if ((device_buttons[i]) &&
	((i != rp->microphone_button) && (recorder_input_device(all_panes[i]->device))))
      set_toggle_button(device_buttons[i], FALSE, TRUE, (void *)(all_panes[i])); 
#endif
  if (rp->trigger != 0.0) set_recorder_trigger(rp, rp->trigger);
  if (rp->max_duration <= 0.0) rp->max_duration = 1000000.0;
}

void reflect_record_size(int size)
{
  if ((recorder) && (rec_size_text)) 
    {
      mus_snprintf(timbuf, TIME_STR_SIZE, "%d", size);
      gtk_entry_set_text(GTK_ENTRY(rec_size_text), timbuf);
    }
}

int record_dialog_is_active(void)
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
  char sbuf[LABEL_BUFFER_SIZE];
  if (val < 1000) return;
  /* this just reflects the setting in the text field -- it doesn't actually set anything in the audio system */
  if (val > 0)
    {
      /* SGI AES In sometimes claims its srate is 0 */
      rp->srate = val;
      if (recorder) 
	{
	  mus_snprintf(sbuf, LABEL_BUFFER_SIZE, "%d", rp->srate);
	  gtk_entry_set_text(GTK_ENTRY(recdat->srate_text), sbuf);
	}
    }
}

