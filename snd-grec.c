/* TODO: colors need work
 *       file data pane is squashed
 *       chans>2 cases are completely broken (button matrix is squashed, meters stay in one (clobbered) row, etc)
 * merge from snd-xrec is not to be trusted (probably counting wrong direction, and one major block omitted in button block)
 */

#include "snd.h"

#ifdef SGI
  #include <audio.h>
  #ifdef AL_RESOURCE
    #define NEW_SGI_AL 1
    #define OLD_SGI_AL 0
  #else
    #define NEW_SGI_AL 0
    #define OLD_SGI_AL 1
  #endif
#else
  #define NEW_SGI_AL 0
  #define OLD_SGI_AL 0
#endif

#define MAX_OUT_CHANS 32
#define MAX_SOUNDCARDS 8

typedef struct {
  GtkWidget *meter;
  GtkWidget *max_button;
  GdkDrawable *wn;
  int on_off;
  int clipped;
  Float current_val,last_val;
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
  GtkWidget *label, *number, *slider, *top;
  GtkObject *adj;
  snd_state *ss;
  int last_amp,type,in,out;
  int device_in_chan;
  void *wd;
} AMP;

typedef struct {
  int in_chans,out_chans,meters_size,amps_size,active_size,on_buttons_size,in_chan_loc;
  VU **meters;         /* has meter widget, can assume frame is local to creator */
  AMP **amps;          /* chans -- has label number slider amp which_amp -- software input gains to output */ 
  int *active;         /* which (incoming or file_out outgoing) channels are actively shoveling samples */
  snd_state *ss;
  GtkWidget **on_buttons;
  GtkWidget *reset_button;
  GtkWidget *pane;
  GtkWidget *button_vertical_sep;
  int pane_size;
  int device,system;          /* audio.c style device descriptor */
  int **active_sliders;
  GtkWidget ***matrix_buttons;
  int bx,by;
  int bw,bh;
} PANE;

typedef struct {
  snd_state *ss;
  int chan,field,device,system;
  int gain;
  PANE *p;
  GtkWidget *wg;
  GtkObject *adj;
} Wdesc;

static GdkGC *draw_gc,*vu_gc;

#define SMALL_FONT_CUTOFF .85
#define SMALLER_FONT_CUTOFF .7
#define SMALL_FONT "6x10"
#define SMALLER_FONT "5x7"

static char timbuf[64];
static char *msgbuf = NULL;

static int systems = 1;             /* multi-card setups */
static int audio_open = 0;          /* input active */
static int monitor_open = 0;        /* speakers active */
static int record_fd[MAX_SOUNDCARDS]; /* input (audio hardware) channel */
static int monitor_fd = -1;         /* output (file) channel */

static gint ever_read = 0;
static file_data *recdat;

static int output_fd = -1;
static int audio_out_chans = 2;
#ifdef LINUX
  static int out_type = MUS_RIFF;
#else
  static int out_type = MUS_AIFC;
#endif

/* on the SGI 1024 is very prone to clicks */
static char *record_buf[MAX_SOUNDCARDS];           /* incoming data has not yet been converted to sndlib representation */
#ifdef HAVE_ALSA
static int monitor_out_format;
static char *monitor_buf;
#endif
static MUS_SAMPLE_TYPE *fbuf = NULL;
static MUS_SAMPLE_TYPE *ffbuf = NULL;
static int fbuf_size = 0;
static MUS_SAMPLE_TYPE **obufs = NULL;          /* formatted non-interleaved output */
static int total_out_samps,duration_samps;
static Float max_duration;
static int overall_in_chans;
static int input_channels[MAX_SOUNDCARDS];

#ifdef HAVE_ALSA
  static int input_srate[MAX_SOUNDCARDS];
  static int input_format[MAX_SOUNDCARDS];
  static int input_buffer_size[MAX_SOUNDCARDS];
#endif

static int *rec_in_active = NULL;   /* overall_in_chans */
static int *rec_out_active = NULL;  /* (file)_out_chans */
static VU **rec_in_VU = NULL;       /* from rec in to associated meter */
static VU **rec_out_VU = NULL;      /* from rec out to associated meter */
static MUS_SAMPLE_TYPE *outvals = NULL;       /* out_chans */
static MUS_SAMPLE_TYPE *in_max = NULL;        /* overall_in_chans (for block-oriented VU meter update) */
static MUS_SAMPLE_TYPE *out_max = NULL;       /* same on output */
static int *in_device_on = NULL;    /* is this input channel receiving input */
static int *in_device_chan = NULL;  /* which actual (audio) input channel is associated with which (virtual) recorder device */

static Float **rec_in_amps = NULL;  /* overall_in_chans X out_chans */
static Float *rec_out_amps = NULL;  /* out_chans (independent of file write: monitor vol) */
static Float *audio_gains = NULL;   /* audio gain values (widgets are per pane) */
static Wdesc **audio_GAINS = NULL;  /* associated sliders and descriptors for write audio state */
static int audio_gains_size = 0;
static AMP ***rec_in_AMPS = NULL;
static AMP **rec_out_AMPS = NULL;

static GtkWidget *recorder = NULL;      /* the outer dialog shell */
static int recording = 0;

static PANE **all_panes = NULL;
static int all_panes_size = 0;
static int out_file_pane;
static int autoload_button = 0;
static int digital_in_button = 0;
static int microphone_button = 0;
static int line_in_button = 0;
static int triggering = 0;
static Float trigger = 0.0;
static int triggered = 1;

static GtkWidget *rec_size_text,*trigger_scale,*trigger_label;
static GtkWidget *file_duration,*messages,*record_button,*reset_button,*file_text;
static GtkWidget **device_buttons;
static GtkObject *trigger_adj;
static int device_buttons_size = 0;
#if NEW_SGI_AL
  static int active_device_button = -1;
#endif
#if SUN
  static int active_device_button = -1;
#endif
static int gain_ctr = 0;
static int overall_input_ctr = 0;
#if (HAVE_OSS || HAVE_ALSA)
  static int mixer_gains_posted[MAX_SOUNDCARDS];
  static int tone_controls_posted[MAX_SOUNDCARDS];
  static int settings_saved = 0;
#endif
static GdkFont *small_font;

static vu_label **vu_labels = NULL;
static int vu_labels_size = 0;
static int current_vu_label = 0;

static char **pending_error = NULL;
static int pending_errors = 0;
static int pending_error_size = 0;

static void set_read_in_progress (snd_state *ss);

static void set_label_font(GtkWidget *w)
{
  GtkStyle *style;
  style = gtk_style_copy(gtk_widget_get_style(w));
  style->font = small_font;
  gtk_widget_set_style(w,style);
}

static Float degrees_to_radians(Float x) {return(x * 3.14159265 / 180.0);}

static Float get_audio_gain(Wdesc *wd)
{
  /* read and set local audio_gains value as well (for snd-clm connection) */
  float g[1];
  mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(wd->system) | (wd->device),wd->field,wd->chan,g);
  if (wd->gain > audio_gains_size) snd_error("%s[%d] %s: overflow %d > %d",__FILE__,__LINE__,__FUNCTION__,wd->gain,audio_gains_size);
  audio_gains[wd->gain]=g[0];
  return(g[0]);
}

static void set_audio_gain(Wdesc *wd, Float amp) 
{
  float g[1];
  g[0] = amp;
  if (wd->device == MUS_AUDIO_DAC_FILTER) /* bass or treble control affects both channels at once */
    {
      mus_audio_mixer_write(MUS_AUDIO_PACK_SYSTEM(wd->system) | (wd->device),wd->field,0,g);
      mus_audio_mixer_write(MUS_AUDIO_PACK_SYSTEM(wd->system) | (wd->device),wd->field,1,g);
    }
  else 
    mus_audio_mixer_write(MUS_AUDIO_PACK_SYSTEM(wd->system) | (wd->device),wd->field,wd->chan,g);
  if (wd->gain > audio_gains_size) snd_error("%s[%d] %s: overflow %d > %d",__FILE__,__LINE__,__FUNCTION__,wd->gain,audio_gains_size);
  audio_gains[wd->gain] = amp;
}

void close_recorder_audio(void) 
{
  int i;
  if (audio_open)
    {
      for (i=0;i<systems;i++)
	if (record_fd[i] != -1)
	  {
	    mus_audio_close(record_fd[i]);
	    record_fd[i] = -1;
	  }
      audio_open = 0;
    }
  if (ever_read) 
    {
      gtk_idle_remove(ever_read);
      ever_read = 0;
    }
  if (monitor_open)
    {
      mus_audio_close(monitor_fd);
      monitor_open = 0;
    }
}

static void record_report(snd_state *ss, GtkWidget *text, ...)
{
  /* place time-stamped message in text window */
  time_t ts;
  va_list ap;
  char *nextstr;
  if (msgbuf == NULL) msgbuf = (char *)CALLOC(512,sizeof(char));
#if (!defined(HAVE_CONFIG_H)) || defined(HAVE_STRFTIME)
  time(&ts);
  strftime(timbuf,64,"%H:%M:%S",localtime(&ts));
  sprintf(msgbuf,"\n[%s] ",timbuf);
#endif
  gtk_text_insert(GTK_TEXT(text),(ss->sgx)->help_text_fnt,(ss->sgx)->black,(ss->sgx)->light_blue,msgbuf,-1);
  va_start(ap,text);
  while ((nextstr = va_arg(ap,char *)))
    gtk_text_insert(GTK_TEXT(text),(ss->sgx)->help_text_fnt,(ss->sgx)->black,(ss->sgx)->light_blue,nextstr,-1);
  va_end(ap);
}

static int fire_up_recorder(snd_state *ss);

#if OLD_SGI_AL
static void set_line_source(snd_state *ss, int in_digital)
{
  int aud,err;
  aud = audio_open;
  if (aud) close_recorder_audio();
  input_channels[0] = ((in_digital) ? 2 : 4);
  audio_out_chans = input_channels[0];
  err = mus_audio_mixer_write(MUS_AUDIO_DEFAULT,MUS_AUDIO_PORT,((in_digital) ? MUS_AUDIO_DIGITAL_IN : MUS_AUDIO_MICROPHONE),NULL);
  if (err == -1) 
    {
      snd_error("err: %s",mus_audio_error_name(mus_audio_error()));
      if (recorder) record_report(ss,messages,"set input source: ",mus_audio_error_name(mus_audio_error()),NULL);
    }
  in_device_on[0] = (!in_digital); 
  in_device_on[1] = (!in_digital);
  in_device_on[2] = (!in_digital); 
  in_device_on[3] = (!in_digital);
  in_device_on[4] = in_digital; 
  in_device_on[5] = in_digital;
  if (aud) fire_up_recorder(ss);
}
#endif

static void set_audio_srate(snd_state *ss, int device, int srate, int system)
{
  float g[1];
  int aud;
#if (!NEW_SGI_AL)
  aud = audio_open;
  if (aud) close_recorder_audio();
  g[0] = (Float)srate;
  mus_audio_mixer_write(MUS_AUDIO_PACK_SYSTEM(system) | device,MUS_AUDIO_SRATE,0,g);
  if (aud) fire_up_recorder(ss);
#endif
}


/* -------------------------------- ICONS -------------------------------- */

static GdkPixmap *speaker_pix,*line_in_pix,*mic_pix,*aes_pix,*adat_pix,*digital_in_pix,*cd_pix;
static GdkBitmap *speaker_mask,*line_in_mask,*mic_mask,*aes_mask,*adat_mask,*digital_in_mask,*cd_mask;
static int icons_created = 0;

static void make_record_icons(GtkWidget *w, snd_state *ss)
{
  GdkWindow *wn;
  icons_created = 1;
  wn = MAIN_WINDOW(ss);
  speaker_pix = gdk_pixmap_create_from_xpm_d(wn,&speaker_mask,(ss->sgx)->basic_color,speaker_bits());
  mic_pix = gdk_pixmap_create_from_xpm_d(wn,&mic_mask,(ss->sgx)->basic_color,mic_bits());
  line_in_pix = gdk_pixmap_create_from_xpm_d(wn,&line_in_mask,(ss->sgx)->basic_color,line_in_bits());
  cd_pix = gdk_pixmap_create_from_xpm_d(wn,&cd_mask,(ss->sgx)->basic_color,cd_bits());
  /* TODO: digital_in, aes, adat */
}

static GdkPixmap *device_pix(int device)
{
  switch (device)
    {
    case MUS_AUDIO_CD:      return(cd_pix); break;
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
    default:                       return(mic_pix); break;
    }

}

static GdkBitmap *device_mask(int device)
{
  switch (device)
    {
    case MUS_AUDIO_CD:      return(cd_mask); break;
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
    default:                       return(mic_mask); break;
    }

}


/* -------------------------------- VU METER -------------------------------- */

static GdkFont *get_vu_font(snd_state *ss, Float size)
{
  char font_name[64];
  int font_size;
  char *vu_font_name;
  GdkFont *label_font;
  font_size = (int)(size*12*vu_font_size(ss));
  if (font_size < 5) font_size = 5;
  vu_font_name = vu_font(ss);
  if (!vu_font_name)
    {
      if (size < 0.75) 
#ifndef SGI
	vu_font_name = "fixed";
#else
        vu_font_name = "courier";
#endif
      else
	{
	  if (size < 1.25) 
	    vu_font_name = "courier";
	  else vu_font_name = "times";
	}
    }
  sprintf(font_name,"-*-%s-%s-r-*-*-%d-*-*-*-*-*-*",
	  vu_font_name,
	  (font_size > 10) ? "bold" : "*",
	  font_size);
  
  label_font = gdk_font_load(font_name);
  if (!(label_font))
    {
      sprintf(font_name,"-*-%s-*-*-*-*-%d-*-*-*-*-*-*",vu_font_name,font_size);
      label_font = gdk_font_load(font_name);
      if (!(label_font))
	{
	  sprintf(font_name,"-*-courier-*-*-*-*-%d-*-*-*-*-*-*",font_size);
	  label_font = gdk_font_load(font_name);
	  while (!(label_font))
	    {
	      sprintf(font_name,"-*-*-*-*-*-*-%d-*-*-*-*-*-*",font_size);
	      label_font = gdk_font_load(font_name);
	      font_size++;
	      if (font_size>60) {label_font=gdk_font_load("-*-*-*-*-*-*-*-*-*-*-*-*-*"); break;}
	    }
	}
    }
  return(label_font);
}

static void allocate_meter_2(GtkWidget *w, vu_label *vu)
{
  GdkDrawable *wn;
  wn = w->window;
  vu->off_label = gdk_pixmap_create_from_xpm_d(wn,&(vu->off_label_mask),NULL,offlabel_bits());
  vu->on_label = gdk_pixmap_create_from_xpm_d(wn,&(vu->on_label_mask),NULL,onlabel_bits());
  vu->clip_label = gdk_pixmap_create_from_xpm_d(wn,&(vu->clip_label_mask),NULL,cliplabel_bits());
}


#define VU_OFF 0
#define VU_ON 1
#define VU_CLIPPED 2
#define CLIPPED_TIME 10
#define CLIPPED_TRIGGER 0.99
#define VU_NEEDLE_SPEED 0.25
#define VU_BUBBLE_SPEED 0.025
#define VU_BUBBLE_SIZE (15*64)

#define LIGHT_X 120
#define LIGHT_Y 100
#define CENTER_X 120
#define CENTER_Y 160
#define VU_COLORS 11
#define VU_NUMBERS 15
/* these are for the highly optimized size=1.0 case */

static GdkColor *yellows[VU_COLORS];
static GdkColor *reds[VU_COLORS];
static int vu_colors_allocated = 0;
static int yellow_vals[] = {0,16,32,64,96,128,160,175,185,200,210,220,230,240};

static void allocate_meter_1(snd_state *ss, vu_label *vu)
{
  /* called only if size not allocated yet and size=1.0 not available as pre-made pixmap */
  GdkDrawable *wn;
  GdkColor tmp_color;
  GdkColormap *cmap;
  int i,j,k;
  GdkColor *white,*black,*red;
  int band;
  GdkPoint pts[16];
  int x0,y0,x1,y1;
  Float rdeg;
  Float size;
  int xs[5],ys[5];

  Float BAND_X;
  Float BAND_Y;
  cmap = gdk_colormap_get_system();

  size = vu->size;
  BAND_X = 2.75*size;
  BAND_Y = 3.25*size;
  red = (ss->sgx)->red;
  wn = recorder->window;
  black = (ss->sgx)->black;
  white = (ss->sgx)->white;

  if (!vu_colors_allocated)
    {
      vu_colors_allocated = 1;
      tmp_color.red = (unsigned short)65535;
      for (i=0;i<VU_COLORS;i++)
	{
	  tmp_color.blue = (unsigned short)(256*yellow_vals[i]);
	  tmp_color.green = (unsigned short)(256*230 + 26*yellow_vals[i]);
	  yellows[i] = gdk_color_copy(&tmp_color);
	  gdk_color_alloc(cmap,yellows[i]);
	}
      for (i=0;i<VU_COLORS;i++)
	{
	  tmp_color.blue = (unsigned short)(128*yellow_vals[i]);
	  tmp_color.green = (unsigned short)(128*yellow_vals[i]);
	  reds[i] = gdk_color_copy(&tmp_color);
	  gdk_color_alloc(cmap,reds[i]);
	}
    }
  for (k=0;k<2;k++) 
    {
      band = 1;
      if (k == 1)
	{
	  vu->clip_label = gdk_pixmap_new(wn,(unsigned int)(CENTER_X*2*size),(unsigned int)(CENTER_Y*size),-1);
	  gdk_gc_set_foreground(draw_gc,reds[0]);	    
	  gdk_draw_rectangle(vu->clip_label,draw_gc,TRUE,0,0,(unsigned int)(CENTER_X*2*size),(unsigned int)(CENTER_Y*size));
	}
      else 
	{
	  vu->on_label = gdk_pixmap_new(wn,(unsigned int)(CENTER_X*2*size),(unsigned int)(CENTER_Y*size),-1);
	  gdk_gc_set_foreground(draw_gc,yellows[2]);
	  gdk_draw_rectangle(vu->on_label,draw_gc,TRUE,0,0,(unsigned int)(CENTER_X*2*size),(unsigned int)(CENTER_Y*size));
	}
      /* initialize the sequence of nested polygons */
      pts[0].x = (short)(LIGHT_X*size - BAND_X);
      pts[0].y = (short)(LIGHT_Y*size);
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
	gdk_draw_polygon(vu->clip_label,draw_gc,TRUE,pts,7);
      else gdk_draw_polygon(vu->on_label,draw_gc,TRUE,pts,7);

      for (i=1;i<VU_COLORS;i++)
	{
	  band += i;
	  if (k == 1) 
	    gdk_gc_set_foreground(draw_gc,reds[i]); 
	  else 
	    {
	      if (i<2) gdk_gc_set_foreground(draw_gc,yellows[2]); else gdk_gc_set_foreground(draw_gc,yellows[i]);
	    }
	  pts[6].x = (short)(LIGHT_X*size + band*BAND_X);
	  pts[6].y = pts[5].y;
	  pts[7].x = pts[6].x;
	  pts[7].y = (short)(LIGHT_Y*size - band*(BAND_Y - 1));
	  pts[8].x = (short)(LIGHT_X*size + band*(BAND_X - 1));
	  pts[8].y = (short)(LIGHT_Y*size - band*BAND_Y);
	  pts[9].x = (short)(LIGHT_X*size - band*(BAND_X - 1));
	  pts[9].y = pts[8].y;
	  pts[10].x = (short)(LIGHT_X*size - band*BAND_X);
	  pts[10].y = (short)(LIGHT_Y*size - band*(BAND_Y - 1));
	  pts[11].x = pts[10].x;
	  pts[11].y = pts[6].y;
	  pts[12].x = pts[0].x;
	  pts[12].y = pts[0].y;
	  if (k == 1)
	    gdk_draw_polygon(vu->clip_label,draw_gc,TRUE,pts,13);
	  else gdk_draw_polygon(vu->on_label,draw_gc,TRUE,pts,13);
	  for (j=0;j<6;j++) 
	    { 
	      /* set up initial portion of next polygon */
	      pts[j].x = pts[j+6].x;
	      pts[j].y = pts[j+6].y;
	    }
	}
    }

  vu->off_label = gdk_pixmap_new(wn,(unsigned int)(CENTER_X*2*size),(unsigned int)(CENTER_Y*size),-1);
  /* not on, so just display a white background */
  gdk_gc_set_foreground(draw_gc,white);
  gdk_draw_rectangle(vu->off_label,draw_gc,TRUE,0,0,(unsigned int)(CENTER_X*2*size),(unsigned int)(CENTER_Y*size));

  gdk_gc_set_foreground(draw_gc,black);

  /* draw the arcs */
  xs[0] = (int)(size*(CENTER_X - 120));
  ys[0] = (int)(size*(CENTER_Y - 120));
  xs[1] = (int)(size*(CENTER_X - 119));
  ys[1] = (int)(size*(CENTER_Y - 120));
  xs[2] = (int)(size*(CENTER_X - 119));
  ys[2] = (int)(size*(CENTER_Y - 119));
  xs[3] = (int)(size*(CENTER_X - 116));
  ys[3] = (int)(size*(CENTER_Y - 116));

  gdk_draw_arc(vu->on_label,draw_gc,FALSE,xs[0],ys[0],(unsigned int)(size*(240)),(unsigned int)(size*(240)),45*64,90*64);
  gdk_draw_arc(vu->on_label,draw_gc,FALSE,xs[1],ys[1],(unsigned int)(size*(239)),(unsigned int)(size*(239)),45*64,89*64);
  gdk_draw_arc(vu->on_label,draw_gc,FALSE,xs[2],ys[2],(unsigned int)(size*(239)),(unsigned int)(size*(239)),45*64,89*64);
  gdk_draw_arc(vu->on_label,draw_gc,FALSE,xs[3],ys[3],(unsigned int)(size*(232)),(unsigned int)(size*(232)),45*64,90*64);

  gdk_draw_arc(vu->off_label,draw_gc,FALSE,xs[0],ys[0],(unsigned int)(size*(240)),(unsigned int)(size*(240)),45*64,90*64);
  gdk_draw_arc(vu->off_label,draw_gc,FALSE,xs[1],ys[1],(unsigned int)(size*(239)),(unsigned int)(size*(239)),45*64,89*64);
  gdk_draw_arc(vu->off_label,draw_gc,FALSE,xs[2],ys[2],(unsigned int)(size*(239)),(unsigned int)(size*(239)),45*64,89*64);
  gdk_draw_arc(vu->off_label,draw_gc,FALSE,xs[3],ys[3],(unsigned int)(size*(232)),(unsigned int)(size*(232)),45*64,90*64);

  gdk_draw_arc(vu->clip_label,draw_gc,FALSE,xs[0],ys[0],(unsigned int)(size*(240)),(unsigned int)(size*(240)),45*64,90*64);
  gdk_draw_arc(vu->clip_label,draw_gc,FALSE,xs[1],ys[1],(unsigned int)(size*(239)),(unsigned int)(size*(239)),45*64,89*64);
  gdk_draw_arc(vu->clip_label,draw_gc,FALSE,xs[2],ys[2],(unsigned int)(size*(239)),(unsigned int)(size*(239)),45*64,89*64);
  gdk_draw_arc(vu->clip_label,draw_gc,FALSE,xs[3],ys[3],(unsigned int)(size*(232)),(unsigned int)(size*(232)),45*64,90*64);

  /* draw the axis ticks */
  for (i=0;i<5;i++)
    {
      rdeg = degrees_to_radians(45-i*22.5);
      x0 = (int)(CENTER_X*size+120*size*sin(rdeg));
      y0 = (int)(CENTER_Y*size-120*size*cos(rdeg));
      x1 = (int)(CENTER_X*size+130*size*sin(rdeg));
      y1 = (int)(CENTER_Y*size-130*size*cos(rdeg));
      gdk_draw_line(vu->on_label,draw_gc,x0,y0,x1,y1);
      gdk_draw_line(vu->on_label,draw_gc,x0+1,y0,x1+1,y1);
      gdk_draw_line(vu->off_label,draw_gc,x0,y0,x1,y1);
      gdk_draw_line(vu->off_label,draw_gc,x0+1,y0,x1+1,y1);
      gdk_draw_line(vu->clip_label,draw_gc,x0,y0,x1,y1);
      gdk_draw_line(vu->clip_label,draw_gc,x0+1,y0,x1+1,y1);
      if (i<4)
	{
	  for (j=1;j<6;j++)
	    {
	      rdeg = degrees_to_radians(45-i*22.5-j*(90.0/20.0));
	      x0 = (int)(CENTER_X*size+120*size*sin(rdeg));
	      y0 = (int)(CENTER_Y*size-120*size*cos(rdeg));
	      x1 = (int)(CENTER_X*size+126*size*sin(rdeg));
	      y1 = (int)(CENTER_Y*size-126*size*cos(rdeg));
	      gdk_draw_line(vu->on_label,draw_gc,x0,y0,x1,y1);
	      gdk_draw_line(vu->off_label,draw_gc,x0,y0,x1,y1);
	      gdk_draw_line(vu->clip_label,draw_gc,x0,y0,x1,y1);
	    }
	}
    }
}

static void allocate_meter(snd_state *ss, vu_label *vu)
{
  if ((vu->size == 1.0) || (vu->size > 4.0) || (vu->size < .25))
     allocate_meter_2(recorder,vu);
  else allocate_meter_1(ss,vu);
}

static vu_label *get_vu_label(snd_state *ss, Float size)
{
  int i;
  vu_label *vu;
  for (i=0;i<current_vu_label;i++)
    {
      if (vu_labels[i]->size == size) return(vu_labels[i]);
    }
  if (current_vu_label >= vu_labels_size)
    {
      vu_labels_size += 8;
      if (!vu_labels)
	vu_labels = (vu_label **)CALLOC(vu_labels_size,sizeof(vu_label *));
      else vu_labels = (vu_label **)REALLOC(vu_labels,vu_labels_size * sizeof(vu_label *));
    }
  vu_labels[current_vu_label] = (vu_label *)CALLOC(1,sizeof(vu_label));
  vu = vu_labels[current_vu_label];
  vu->label_font = get_vu_font(ss,size);
  vu->size = size;
  current_vu_label++;
  allocate_meter(ss,vu);
  return(vu);
}

static void display_vu_meter(VU *vu)
{
  Float deg,rdeg,val;
  int nx0,nx1,ny0,ny1,redx,redy,bub0,bub1,i,j;
  GdkPixmap *label = 0;
  GdkBitmap *mask;
  snd_state *ss;
  Float size;
  state_context *sx;
  ss = vu->ss;
  sx = ss->sgx;
  size = vu->size;
  if (vu->current_val > CLIPPED_TRIGGER) 
    {
      if (vu->on_off == VU_ON) 
	{
	  vu->on_off = VU_CLIPPED;
	  vu->clipped = (int)(CLIPPED_TIME * ((Float)(recorder_srate(ss)) / 22050.0));
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

  if (label) gdk_draw_pixmap(vu->wn,vu_gc,label,0,0,0,0,vu->center_x*2,vu->center_y);

  val = vu->current_val*VU_NEEDLE_SPEED + (vu->last_val*(1.0-VU_NEEDLE_SPEED));
  vu->last_val = val;
  deg = -45.0 + val*90.0;
  /* if (deg < -45.0) deg = -45.0; else if (deg > 45.0) deg = 45.0; */
  rdeg = degrees_to_radians(deg);
  nx0 = vu->center_x - (int)((Float)(vu->center_y - vu->light_y) / tan(degrees_to_radians(deg+90)));
  ny0 = vu->light_y;
  nx1 = (int)(vu->center_x + 130*size*sin(rdeg));
  ny1 = (int)(vu->center_y - 130*size*cos(rdeg));
  gdk_gc_set_foreground(vu_gc,sx->black);
  gdk_draw_line(vu->wn,vu_gc,nx0,ny0,nx1,ny1);

  /* this shadow doesn't do much (and if +/-3 for depth, it looks kinda dumb) */
  if (deg != 0.0)
    {
      if (vu->on_off == VU_OFF)
	gdk_gc_set_foreground(vu_gc,sx->position_color);
      else
	if (vu->on_off == VU_CLIPPED)
	  gdk_gc_set_foreground(vu_gc,sx->pushed_button_color);
	else gdk_gc_set_foreground(vu_gc,sx->white);
      if (deg < 0.0)
	gdk_draw_line(vu->wn,vu_gc,nx0-1,ny0,nx1-1,ny1);
      else gdk_draw_line(vu->wn,vu_gc,nx0+1,ny0,nx1+1,ny1);
      gdk_gc_set_foreground(vu_gc,sx->black);
    }

  if (vu->on_off != VU_OFF)
    {
      if (vu->current_val > vu->red_deg) 
	vu->red_deg = vu->current_val;
      else vu->red_deg = vu->current_val*VU_BUBBLE_SPEED + (vu->red_deg*(1.0 - VU_BUBBLE_SPEED));
      gdk_gc_set_foreground(vu_gc,sx->red);
      redx = (int)(vu->red_deg * 90*64);
      if (redx<(VU_BUBBLE_SIZE)) redy=redx; else redy=VU_BUBBLE_SIZE;
      bub0 = (int)(size*117);
      bub1 = (int)(size*119);
      for (i=bub0,j=bub0*2;i<=bub1;i++,j+=(int)(2*size))
	gdk_draw_arc(vu->wn,vu_gc,FALSE,vu->center_x - i,vu->center_y - i,j,j,135*64 - redx,redy);
      gdk_gc_set_foreground(vu_gc,sx->black);
    }
}

static void Meter_Expose_Callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  display_vu_meter((VU *)data);
}

static void Meter_Resize_Callback(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  display_vu_meter((VU *)data);
}

static VU *make_vu_meter(GtkWidget *meter, int light_x, int light_y, int center_x, int center_y, GdkColor *needle_color, snd_state *ss, Float size)
{
  VU *vu;
  vu_label *vl;
  vu = (VU *)CALLOC(1,sizeof(VU));
  vu->meter = meter;
  vu->size = size;
  vu->wn = meter->window;
  vu->on_off = VU_OFF;
  vu->current_val = 0.0;
  vu->last_val = 0.0;
  vu->clipped = 0;
  vu->max_val = 0.0;
  vu->light_x = (int)(light_x*size);
  vu->light_y = (int)(light_y*size);
  vu->center_x = (int)(center_x*size);
  vu->center_y = (int)(center_y*size);
  vu->ss = ss;
  vl = get_vu_label(ss,size);
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
      sprintf(timbuf,"%.3f",val);
      set_label(vu->max_button,timbuf);
    }
}


/* -------------------------------- AMP SLIDER CALLBACKS -------------------------------- */

#define INPUT_AMP 0
#define OUTPUT_AMP 1

static char record_one[5]={'1',STR_decimal,'0','0','\0'};
static char record_zero[5]={'0',STR_decimal,'0','0','\0'};
static char amp_number_buffer[5]={'1',STR_decimal,'0','0','\0'};

static void record_amp_changed(AMP *ap, Float scrollval)
{
  char *sfs;
  snd_state *ss;
  Float amp;
  ss = ap->ss;
  if (scrollval < .15)
    amp = (scrollval * 1.13);
  else amp = (exp((scrollval - 0.5) * 5.0));
  sfs=prettyf(amp,2);
  fill_number(sfs,amp_number_buffer);
  set_button_label(ap->number,amp_number_buffer);
  FREE(sfs);
  if (ap->type == INPUT_AMP)
    rec_in_amps[ap->in][ap->out] = amp;
  else rec_out_amps[ap->out] = amp;
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
      else return(log(amp)*0.2 + 0.5);
    }
}

static Float global_amp(AMP *a)
{
  if (a->type == INPUT_AMP)
    return(rec_in_amps[a->in][a->out]);
  else return(rec_out_amps[a->out]);
}

static char *amp_to_string(Float val)
{
  char *sfs;
  if (val == 0.0) return(record_zero);
  else if (val == 1.0) return(record_one);
  sfs=prettyf(val,2);
  fill_number(sfs,amp_number_buffer);
  FREE(sfs);
  return(amp_number_buffer);
}

static void Record_Amp_Click_Callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  AMP *ap = (AMP *)data;
  Float val;
  if (ev->state & (snd_ControlMask | snd_MetaMask)) val = ap->last_amp; else val = 0.5;
  record_amp_changed(ap,val);
  GTK_ADJUSTMENT(ap->adj)->value = val;
  gtk_adjustment_value_changed(GTK_ADJUSTMENT(ap->adj));
}

static void Record_Amp_Drag_Callback(GtkAdjustment *adj, gpointer data)
{
  record_amp_changed((AMP *)data,adj->value);
}

static void Record_Amp_ValueChanged_Callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
}



/* ---------------- MESSAGE PANE ---------------- */

static GtkWidget *make_message_pane(snd_state *ss)
{
  GtkWidget *table,*text;
  GtkWidget *hscrollbar;
  GtkWidget *vscrollbar;
  /* basically copied from the tutorial */
  table = gtk_table_new (2, 2, FALSE);
  text = gtk_text_new (NULL, NULL);
  set_text_background(text,(ss->sgx)->light_blue);
  messages = text;
  gtk_table_attach (GTK_TABLE (table), text, 0, 1, 0, 1, GTK_FILL | GTK_EXPAND, GTK_FILL | GTK_EXPAND | GTK_SHRINK, 0, 0);
  gtk_text_set_editable(GTK_TEXT(text),FALSE);
  gtk_text_set_word_wrap(GTK_TEXT(text),FALSE);
  gtk_widget_show (text);
  hscrollbar = gtk_hscrollbar_new (GTK_TEXT (text)->hadj);
  set_background(hscrollbar,(ss->sgx)->position_color);
  gtk_table_attach (GTK_TABLE (table), hscrollbar, 0, 1, 1, 2, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show (hscrollbar);
  vscrollbar = gtk_vscrollbar_new (GTK_TEXT (text)->vadj);
  set_background(vscrollbar,(ss->sgx)->position_color);
  gtk_table_attach (GTK_TABLE (table), vscrollbar, 1, 2, 0, 1, GTK_FILL, GTK_EXPAND | GTK_FILL | GTK_SHRINK, 0, 0);
  gtk_widget_show (vscrollbar);
  return(table);
}


/* ---------------- FILE INFO PANE ---------------- */

static char *Device_Name(int dev)
{
  /* format label at top of pane */
  switch (dev)
    {
    case MUS_AUDIO_DIGITAL_OUT: return(STR_Digital_Out); break;
    case MUS_AUDIO_LINE_OUT:    return(STR_Line_Out); break;
    case MUS_AUDIO_DEFAULT: 
    case MUS_AUDIO_DAC_OUT:     return(STR_Output); break;
    case MUS_AUDIO_DUPLEX_DEFAULT: 
    case MUS_AUDIO_SPEAKERS:    return(STR_Speakers); break;
    case MUS_AUDIO_ADAT_IN:     return(STR_Adat_In); break;
    case MUS_AUDIO_AES_IN:      return(STR_Aes_In); break;
#if (HAVE_OSS || HAVE_ALSA)
    case MUS_AUDIO_LINE_IN:     return(STR_Analog_In); break;
#else
    case MUS_AUDIO_LINE_IN:     return(STR_Line_In); break;
#endif
    case MUS_AUDIO_MICROPHONE:  return(STR_Microphone); break;
    case MUS_AUDIO_DIGITAL_IN:  return(STR_Digital_In); break;
    case MUS_AUDIO_ADAT_OUT:    return(STR_Adat_Out); break;
    case MUS_AUDIO_AES_OUT:     return(STR_Aes_Out); break;
    case MUS_AUDIO_DAC_FILTER:  return("Tone"); break;
    case MUS_AUDIO_MIXER:       return("Mixer"); break;
    case MUS_AUDIO_AUX_INPUT:   return("Aux Input"); break;
    case MUS_AUDIO_CD:       return("CD"); break;
    case MUS_AUDIO_AUX_OUTPUT:  return("Aux Output"); break;
    case MUS_AUDIO_SPDIF_IN:    return("S/PDIF In"); break;
    case MUS_AUDIO_SPDIF_OUT:   return("S/PDIF Out"); break;
    default: snd_error("%s[%d] %s: unknown device: %d",__FILE__,__LINE__,__FUNCTION__,dev); return(STR_Input); break;
    }
}

static char sysdevstr[32];
static char *System_and_Device_Name(int sys, int dev)
{
  if (strcmp("OSS",mus_audio_system_name(sys)) == 0) return(Device_Name(dev));
  sprintf(sysdevstr,"%s: %s",mus_audio_system_name(sys),Device_Name(dev));
  return(sysdevstr);
}

static int input_device(int dev)
{
  switch (dev)
    {
    case MUS_AUDIO_DIGITAL_OUT:
    case MUS_AUDIO_LINE_OUT:
    case MUS_AUDIO_DEFAULT:
    case MUS_AUDIO_ADAT_OUT:
    case MUS_AUDIO_AES_OUT:
    case MUS_AUDIO_SPDIF_OUT:
    case MUS_AUDIO_SPEAKERS:
    case MUS_AUDIO_MIXER:
    case MUS_AUDIO_DAC_FILTER:
    case MUS_AUDIO_DAC_OUT: return(0); break;
    case MUS_AUDIO_DUPLEX_DEFAULT: 
    case MUS_AUDIO_ADAT_IN: 
    case MUS_AUDIO_AES_IN:
    case MUS_AUDIO_SPDIF_IN:
    case MUS_AUDIO_LINE_IN: 
    case MUS_AUDIO_MICROPHONE: 
    case MUS_AUDIO_DIGITAL_IN: 
    case MUS_AUDIO_CD:
    default: return(1); break;
    }
  snd_error("%s[%d] %s: uncategorized device: %d",__FILE__,__LINE__,__FUNCTION__,dev);
  return(0);
}

static int output_device(int dev)
{
  return((dev != MUS_AUDIO_DAC_FILTER) && (dev != MUS_AUDIO_MIXER) && (!(input_device(dev))));
}


static void Help_Record_Callback(GtkWidget *w,gpointer clientData) 
{
  record_dialog_help((snd_state *)clientData);
}

static void internal_trigger_set(Float val)
{
  trigger = val;
  triggering = (val > 0.0);
  triggered = (!triggering);
  if (!recording) /* else wait for current session to end (via click) */
    {
      set_button_label(record_button,(triggering) ? STR_Triggered_Record : STR_Record);
    }
}

static void change_trigger_Callback(GtkAdjustment *adj, gpointer clientData)
{
  snd_state *ss = (snd_state *)clientData;
  /* if val=0, set record button normal label to 'Record', else 'Triggered Record' */
  in_set_recorder_trigger(ss,adj->value);
  internal_trigger_set(adj->value);
}

static void device_button_callback(GtkWidget *w,gpointer clientData) 
{
  PANE *p = (PANE *)clientData;
  int on,button;
  snd_state *ss;
#if defined(SGI) || defined(SUN)
  int other_button,j,n,i,output;
  float val[2];
#endif

  ss = p->ss;

#if defined(SGI) || defined(SUN)
  output = 0;
#endif

  button = (int)gtk_object_get_user_data(GTK_OBJECT(w));
  on = GTK_TOGGLE_BUTTON(w)->active;

#if OLD_SGI_AL
  /* on the older SGI's (and maybe newer Indy's?) digital input disables mic/line-in and vice versa */
  if (button == digital_in_button)
    {
      set_line_source(p->ss,on);
      if (on == (GTK_TOGGLE_BUTTON(device_buttons[microphone_button])->active))
	set_toggle_button(device_buttons[microphone_button],!on,TRUE,(void *)(all_panes[microphone_button])); 
      if (on == GTK_TOGGLE_BUTTON(device_buttons[line_in_button])->active)
	set_toggle_button(device_buttons[line_in_button],!on,TRUE,(void *)(all_panes[line_in_button])); 
    }
  else
    {
      if (button == microphone_button)
	{
	  if (on == (GTK_TOGGLE_BUTTON(device_buttons[digital_in_button])->active))
	    {
	      set_toggle_button(device_buttons[digital_in_button],!on,TRUE,(void *)(all_panes[digital_in_button])); 
	      if (!(on == (GTK_TOGGLE_BUTTON(device_buttons[line_in_button])->active)))
		set_toggle_button(device_buttons[line_in_button],on,TRUE,(void *)(all_panes[line_in_button])); 
	    }
	}
    }
#endif
#if NEW_SGI_AL || defined(SUN)
  output = (!(input_device(p->device)));
  if (!output)
    {
      if (on)
	{
  	  if (active_device_button != -1)
	    {
	      p = all_panes[active_device_button];
	      for (i=p->in_chan_loc,j=0;j<p->in_chans;j++,i++) in_device_on[i]=0;
	      if (active_device_button != button)
		{
		  set_toggle_button(device_buttons[active_device_button],FALSE,FALSE,(void *)(all_panes[active_device_button])); 
		}
	      if (audio_open) close_recorder_audio();
	    }
	  active_device_button = button;
	  p = all_panes[button];
	  for (i=p->in_chan_loc,j=0;j<p->in_chans;j++,i++) in_device_on[i]=1;
	  input_channels[0] = p->in_chans;

	  /* if digital in, get its srate (and reflect in srate text) */
	  if ((p->device == MUS_AUDIO_AES_IN) || (p->device == MUS_AUDIO_ADAT_IN))
	    {
	      mus_audio_mixer_read(p->device,MUS_AUDIO_SRATE,0,val);
	      set_recorder_srate(ss,(int)val[0]);
	    }
	  record_fd[0] = mus_audio_open_input(MUS_AUDIO_PACK_SYSTEM(0) | p->device,
					  recorder_srate(ss),input_channels[0],recorder_in_format(ss),recorder_buffer_size(ss));
	  if (record_fd[0] == -1)
	    record_report(ss,messages,Device_Name(p->device),": ",mus_audio_error_name(mus_audio_error()),NULL);
	  else
	    {
	      audio_open = 1;
	      set_read_in_progress(ss);
	    }
	}
  #ifndef SUN
      else
	{
	  if (on)
	    {
	      if (!monitor_open)
		{
		  monitor_fd = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_DAC_OUT,
						 recorder_srate(ss),audio_out_chans,recorder_out_format(ss),recorder_buffer_size(ss));
		  if (monitor_fd == -1)
		    {
		      record_report(ss,messages,"open output: ",mus_audio_error_name(mus_audio_error()),NULL);
		      monitor_open = 0;
		    }
		  else monitor_open = 1;
		}
	    }
	  else 
	    {
	      mus_audio_close(monitor_fd);
	      monitor_open = 0;
	      monitor_fd = -1;
	    }
	}
  #endif
    }
#endif
}

static void autoload_file_callback(GtkWidget *w,gpointer clientData) 
{
  snd_state *ss = (snd_state *)clientData;
  in_set_recorder_autoload(ss,GTK_TOGGLE_BUTTON(w)->active);
}

#if (HAVE_OSS || HAVE_ALSA)
static void save_audio_settings_callback(GtkWidget *w,gpointer clientData) 
{
  snd_state *ss = (snd_state *)clientData;
  set_toggle_button(w,FALSE,FALSE,(void *)ss);
  settings_saved = 1;
  mus_audio_mixer_save(AUDIO_STATE_FILE);
}
#endif

static void Srate_Changed_Callback(GtkWidget *w,gpointer clientData) 
{
  char *str;
  int n;
  snd_state *ss = (snd_state *)clientData;
  str = copy_string(gtk_entry_get_text(GTK_ENTRY(w))); 
  if (str) 
    {
      n = string2int(str);
      if ((n>0) && (n != recorder_srate(ss)))
	{
	  in_set_recorder_srate(ss,n);
	  set_audio_srate(ss,MUS_AUDIO_DEFAULT,recorder_srate(ss),0);
	}
      FREE(str);
    }
}

static void set_record_size (snd_state *ss, int new_size);

static void Rec_Size_Changed_Callback(GtkWidget *w,gpointer clientData) 
{
  snd_state *ss = (snd_state *)clientData;
  char *str;
  int n;
  str = copy_string(gtk_entry_get_text(GTK_ENTRY(w))); 
  if (str) 
    {
      n = string2int(str);
      if ((n>0) && (n != recorder_buffer_size(ss))) set_record_size(ss,n);
      FREE(str);
    }
}

static void make_file_info_pane(snd_state *ss, GtkWidget *file_pane, int *ordered_devices, int ndevs, int *ordered_systems)
{
  int i;
  char *name;
  GtkWidget *file_label,*file_form,*button_frame,*button_holder,*duration_label,*rec_size_label,*ff_sep1,*ff_sep2,*ff_sep3,*autoload_file;
  GtkWidget *left_form,*right_form,*filebox,*durbox,*triggerbox;
#if (HAVE_OSS || HAVE_ALSA)
  GtkWidget *save_audio_settings;
#endif
#if defined(SGI) || defined(SUN)
  float val[1];
  int err;
#endif

  /* file_pane is the outer frame widget */

  file_form = gtk_hbox_new(FALSE,0);
  gtk_container_add(GTK_CONTAINER(file_pane),file_form);
  gtk_widget_show(file_form);

  left_form = gtk_vbox_new(FALSE,0);
  gtk_box_pack_start(GTK_BOX(file_form),left_form,TRUE,TRUE,0);
  gtk_widget_show(left_form);

  ff_sep1 = gtk_vseparator_new();
  gtk_box_pack_start(GTK_BOX(file_form),ff_sep1,FALSE,FALSE,2);
  gtk_widget_show(ff_sep1);

  right_form = gtk_vbox_new(FALSE,0);
  gtk_box_pack_start(GTK_BOX(file_form),right_form,TRUE,TRUE,0);
  gtk_widget_show(right_form);

  filebox = gtk_hbox_new(FALSE,0);
  gtk_box_pack_start(GTK_BOX(left_form),filebox,FALSE,FALSE,0);
  gtk_widget_show(filebox);

  file_label = gtk_label_new(STR_file_p);
  gtk_box_pack_start(GTK_BOX(filebox),file_label,FALSE,FALSE,0);
  gtk_widget_show(file_label);

  file_text = gtk_entry_new();
  gtk_entry_set_editable(GTK_ENTRY(file_text),TRUE);
  gtk_box_pack_start(GTK_BOX(filebox),file_text,TRUE,TRUE,2);
  set_background(file_text,(ss->sgx)->white);
  gtk_widget_show(file_text);

  ff_sep3 = gtk_hseparator_new();
  gtk_box_pack_start(GTK_BOX(left_form),ff_sep3,FALSE,FALSE,8);
  gtk_widget_show(ff_sep3);

  recdat = sndCreateFileDataForm(ss,left_form,"data-form",TRUE,out_type,recorder_out_format(ss),FALSE);
  gtk_signal_connect_object(GTK_OBJECT(recdat->srate_text),"activate",GTK_SIGNAL_FUNC(Srate_Changed_Callback),(gpointer)ss);

#if defined(SGI)
  err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_MICROPHONE,MUS_AUDIO_SRATE,0,val);
  if (!err) in_set_recorder_srate(ss,val[0]);
#endif
  sprintf(timbuf,"%d",recorder_srate(ss));
  gtk_entry_set_text(GTK_ENTRY(recdat->srate_text),copy_string(timbuf));
  sprintf(timbuf,"%d",recorder_out_chans(ss));
  gtk_entry_set_text(GTK_ENTRY(recdat->chans_text),copy_string(timbuf));

  durbox = gtk_hbox_new(FALSE,0);
  gtk_box_pack_start(GTK_BOX(right_form),durbox,FALSE,FALSE,0);
  gtk_widget_show(durbox);

  duration_label = gtk_label_new(STR_duration_p);
  gtk_box_pack_start(GTK_BOX(durbox),duration_label,FALSE,FALSE,0);
  gtk_widget_show(duration_label);

  file_duration = gtk_label_new("0.000");
  gtk_box_pack_start(GTK_BOX(durbox),file_duration,TRUE,TRUE,0);
  gtk_widget_show(file_duration);
  
  rec_size_label = gtk_label_new("buf:");
  gtk_box_pack_start(GTK_BOX(durbox),rec_size_label,FALSE,FALSE,0);
  gtk_widget_show(rec_size_label);

  rec_size_text = gtk_entry_new();
  gtk_entry_set_editable(GTK_ENTRY(rec_size_text),TRUE);
  gtk_box_pack_start(GTK_BOX(durbox),rec_size_text,TRUE,TRUE,2);
  set_background(rec_size_text,(ss->sgx)->white);
  gtk_widget_show(rec_size_text);

  /* XtAddCallback(rec_size_text,XmNactivateCallback,Rec_Size_Changed_Callback,(void *)ss); */
  sprintf(timbuf,"%d",recorder_buffer_size(ss));
  gtk_entry_set_text(GTK_ENTRY(rec_size_text),timbuf);

  ff_sep2 = gtk_hseparator_new();
  gtk_box_pack_start(GTK_BOX(right_form),ff_sep2,FALSE,FALSE,0);
  gtk_widget_show(ff_sep2);

  triggerbox = gtk_hbox_new(FALSE,0);
  gtk_box_pack_end(GTK_BOX(right_form),triggerbox,FALSE,FALSE,0);
  gtk_widget_show(triggerbox);

  trigger_label = gtk_label_new(STR_trigger_p);
  gtk_box_pack_start(GTK_BOX(triggerbox),trigger_label,FALSE,FALSE,4);
  gtk_widget_show(trigger_label);

  trigger_adj = gtk_adjustment_new(0.0,0.0,1.01,.01,.1,.01);
  trigger_scale = gtk_hscale_new(GTK_ADJUSTMENT(trigger_adj));
  gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(trigger_scale)),GTK_UPDATE_CONTINUOUS);
  gtk_scale_set_digits(GTK_SCALE(trigger_scale),3);
  gtk_scale_set_draw_value(GTK_SCALE(trigger_scale),TRUE);
  gtk_scale_set_value_pos(GTK_SCALE(trigger_scale),GTK_POS_LEFT);
  gtk_box_pack_start(GTK_BOX(triggerbox),trigger_scale,TRUE,TRUE,0);
  gtk_signal_connect(GTK_OBJECT(trigger_adj),"value_changed",GTK_SIGNAL_FUNC(change_trigger_Callback),(gpointer)ss);
  gtk_widget_show(trigger_scale);

  /* buttons */
  
  button_frame = gtk_frame_new("");
  gtk_box_pack_start(GTK_BOX(right_form),button_frame,TRUE,TRUE,0);
  gtk_widget_show(button_frame);

  button_holder = gtk_vbox_new(TRUE,0);
  gtk_container_add(GTK_CONTAINER(button_frame),button_holder);
  gtk_widget_show(button_holder);

  for (i=0;i<ndevs;i++)
    {
      if ((systems == 1) || (!(input_device(ordered_devices[i]))))
	name = Device_Name(ordered_devices[i]);
      else name = System_and_Device_Name(ordered_systems[i],ordered_devices[i]);
      device_buttons[i] = gtk_check_button_new_with_label(name);
      gtk_box_pack_start(GTK_BOX(button_holder),device_buttons[i],TRUE,TRUE,0);
      gtk_widget_show(device_buttons[i]);
      gtk_object_set_user_data(GTK_OBJECT(device_buttons[i]),(gpointer)i);
      gtk_signal_connect(GTK_OBJECT(device_buttons[i]),"toggled",GTK_SIGNAL_FUNC(device_button_callback),(gpointer)all_panes[i]);
      set_toggle_button(device_buttons[i],TRUE,FALSE,(void *)(all_panes[i]));
    }

  autoload_file = gtk_check_button_new_with_label(STR_Autoload_Recording);
  gtk_box_pack_start(GTK_BOX(button_holder),autoload_file,TRUE,TRUE,0);
  gtk_widget_show(autoload_file);
  device_buttons[ndevs] = autoload_file;
  /* we assume this is last in the device_buttons list in sensitize_control_buttons */
  autoload_button = ndevs;
  gtk_signal_connect(GTK_OBJECT(autoload_file),"toggled",GTK_SIGNAL_FUNC(autoload_file_callback),(gpointer)ss);
  set_toggle_button(autoload_file,recorder_autoload(ss),FALSE,(void *)ss); 
#if (HAVE_OSS || HAVE_ALSA)
  save_audio_settings = gtk_check_button_new_with_label(STR_Save_Audio_Settings);
  gtk_box_pack_start(GTK_BOX(button_holder),save_audio_settings,TRUE,TRUE,0);
  gtk_widget_show(save_audio_settings);
  gtk_signal_connect(GTK_OBJECT(save_audio_settings),"toggled",GTK_SIGNAL_FUNC(save_audio_settings_callback),(gpointer)ss);
#endif
}

static void update_duration(Float new_dur)
{
  sprintf(timbuf,"%.2f",new_dur);
  set_label(file_duration,timbuf);
}

int record_in_progress(void)
{
  return((recorder) && (recording));
}

void lock_recording_audio(void)
{
  if ((recorder) && (GTK_WIDGET_VISIBLE(recorder)))
    {
      close_recorder_audio();
      set_sensitive(record_button,FALSE);
      set_sensitive(reset_button,FALSE);
    }
}

void unlock_recording_audio(void)
{
  if ((recorder) && (GTK_WIDGET_VISIBLE(recorder)))
    {
      set_sensitive(record_button,TRUE);
      set_sensitive(reset_button,TRUE);
      set_button_label(reset_button,STR_Restart);
    }
}


/* -------------------------------- DEVICE PANE -------------------------------- */


static char numbuf[8];
static char *channel_name(PANE *p, int chan)
{
  int use_numbers;
  use_numbers = ((p->out_chans>4) || (p->in_chans>4));
  if (use_numbers)
    sprintf(numbuf,"%d",chan+1);
  else sprintf(numbuf,"%c",(char)('A' + chan));
  return(numbuf);
}

static char *out_channel_name(snd_state *ss, int chan)
{
  int use_numbers;
  use_numbers = (recorder_out_chans(ss)>4);
  if (use_numbers)
    sprintf(numbuf,"%d",chan+1);
  else sprintf(numbuf,"%c",(char)('A' + chan));
  return(numbuf);
}

static char *gain_channel_name(PANE *p, int input, int dev_in, int out)
{
  int use_numbers;
  if (input)
    {
      use_numbers = ((p->out_chans>4) || (p->in_chans>4));
      if (use_numbers)
	sprintf(numbuf,"%d->%d:",dev_in+1,out+1);
      else sprintf(numbuf,"%c->%c:",(char)('A' + dev_in),(char)('A'+out));
    }
  else
    {
      use_numbers = (p->out_chans > 4);
      if (use_numbers)
	sprintf(numbuf,"%d:",out+1);
      else sprintf(numbuf,"%c:",(char)('A'+out));
    }
  return(numbuf);
}

#if (HAVE_OSS || HAVE_ALSA)
static char *field_abbreviation(int fld)
{
  switch (fld)
    {
    case MUS_AUDIO_IMIX:   return("imx"); break;
    case MUS_AUDIO_IGAIN:  return("ign"); break;
    case MUS_AUDIO_RECLEV: return("rec"); break;
    case MUS_AUDIO_PCM:    return("pcm"); break;
    case MUS_AUDIO_PCM2:   return("pc2"); break;
    case MUS_AUDIO_OGAIN:  return("ogn"); break;
    case MUS_AUDIO_LINE:   return("lin"); break;
    case MUS_AUDIO_MICROPHONE:    return("mic"); break;
    case MUS_AUDIO_LINE1:  return("l1");  break;
    case MUS_AUDIO_LINE2:  return("l2");  break;
    case MUS_AUDIO_LINE3:  return("l3");  break;
    case MUS_AUDIO_SYNTH:  return("syn"); break;
    case MUS_AUDIO_BASS:   return("ton"); break;
    case MUS_AUDIO_TREBLE: return("ton"); break;
    case MUS_AUDIO_CD:     return("cd"); break;
    }
  return("oops");
}
#endif

static void VU_Reset_Callback(GtkWidget *w,gpointer clientData) 
{
  /* set current maxes to 0.0 */
  int i;
  PANE *p = (PANE *)clientData;
  VU *vu;
  for (i=0;i<p->meters_size;i++)
    {
      vu = p->meters[i];
      vu->max_val = 0.0;
      set_label(vu->max_button,"0.00");
    }
}

static void Meter_Button_Callback(GtkWidget *w,gpointer clientData) 
{
  Wdesc *wd = (Wdesc *)clientData;
  VU *vu;
  snd_state *ss;
  int val,i,n;
  char *str;
  PANE *p;
  p = wd->p;
  vu = p->meters[wd->chan];
  ss = vu->ss;
  if (vu->on_off == VU_OFF)
    {
      set_background(w,(ss->sgx)->red);
      vu->on_off = VU_ON;
      vu->red_deg = 0.0;
    }
  else 
    {
      set_background(w,(ss->sgx)->basic_color);
      vu->on_off = VU_OFF;
    }
  display_vu_meter(vu);
  val = (vu->on_off == VU_ON);
  p->active[wd->chan] = val;
  if (output_device(p->device))
    {
      rec_out_active[wd->chan] = val;
      str = copy_string(gtk_entry_get_text(GTK_ENTRY(recdat->chans_text))); 
      if (str) 
	{
	  n = string2int(str);
	  FREE(str);
	}
      else n=0;
      val = 0;
      for (i=0;i<p->active_size;i++) {if (p->active[i]) val++;}
      if ((val>0) && (val != n))
	{
	  sprintf(timbuf,"%d",val);
	  gtk_entry_set_text(GTK_ENTRY(recdat->chans_text),timbuf);
#ifdef HAVE_ALSA
	  /* FIXME: this apparently is not necessary, we cannot
	   * change the number of recorded channels on the fly
	   * (but we can activate or deactivate them in the gui?
	       in_set_recorder_out_chans(ss,val);
	   */
#endif
	}
    }
  else rec_in_active[wd->gain] = val;
}

static void volume_callback(GtkAdjustment *adj, gpointer clientData) 
{
  Wdesc *wd = (Wdesc *)clientData;
  set_audio_gain(wd,1.0 - adj->value);
}

/* ---- slider button matrix ---- */

static void sndCreateRecorderSlider(snd_state *ss, PANE *p, AMP *a, GtkWidget *last_slider, int input)
{
  GtkWidget *hb;

  hb = gtk_hbox_new(FALSE,0);
  gtk_box_pack_start(GTK_BOX(last_slider),hb,FALSE,FALSE,0);
  gtk_widget_show(hb);

  a->label = gtk_button_new_with_label(gain_channel_name(p,input,a->device_in_chan,a->out));
  gtk_button_set_relief(GTK_BUTTON(a->label),GTK_RELIEF_NONE);
  gtk_box_pack_start(GTK_BOX(hb),a->label,FALSE,FALSE,0);
  gtk_widget_show(a->label);
  gtk_signal_connect(GTK_OBJECT(a->label),"button_press_event",GTK_SIGNAL_FUNC(Record_Amp_Click_Callback),(gpointer)a);

  a->number = gtk_button_new_with_label(amp_to_string(global_amp(a)));
  gtk_button_set_relief(GTK_BUTTON(a->number),GTK_RELIEF_NONE);
  gtk_box_pack_start(GTK_BOX(hb),a->number,FALSE,FALSE,0);
  gtk_widget_show(a->number);
  gtk_signal_connect(GTK_OBJECT(a->number),"button_press_event",GTK_SIGNAL_FUNC(Record_Amp_Click_Callback),(gpointer)a);

  a->adj = gtk_adjustment_new(amp_to_slider(global_amp(a)),0.0,1.00,0.001,0.01,.1);
  a->slider = gtk_hscrollbar_new(GTK_ADJUSTMENT(a->adj));
  gtk_box_pack_start(GTK_BOX(hb),a->slider,TRUE,TRUE,6);
  set_background(a->slider,(ss->sgx)->position_color);
  gtk_signal_connect(GTK_OBJECT(a->adj),"value_changed",GTK_SIGNAL_FUNC(Record_Amp_Drag_Callback),(gpointer)a);
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
      set_background(mb,(ss->sgx)->basic_color);
      p->active_sliders[bin][bout] = 0;
      gtk_widget_hide(a->label);
      gtk_widget_hide(a->number);
      gtk_widget_hide(a->slider);
    }
  else
    {
      set_background(mb,(ss->sgx)->green);
      p->active_sliders[bin][bout] = 1;
      if (a->label)
	{
	  gtk_widget_show(a->label);
	  gtk_widget_show(a->number);
	  gtk_widget_show(a->slider);
	}
      else
	sndCreateRecorderSlider(ss,p,a,a->top,TRUE);
    }
}

typedef struct {
  PANE *p;
  AMP *a;
  int in_chan,out_chan;
} slider_info;

static void Matrix_Button_Callback(GtkWidget *mb, gpointer clientData) 
{
  slider_info *si = (slider_info *)clientData;
  PANE *p;
  int curamp;
  p = si->p;
  curamp = si->out_chan * p->in_chans + si->in_chan;
  handle_matrix_slider(mb,p,si->in_chan,si->out_chan,curamp,(p->active_sliders[si->in_chan][si->out_chan]));
}

static int button_matrix_button = 0;
static int button_matrix_state = 0;
static int initial_button = 0;
static GtkWidget *active_button = NULL;

enum {CORNER_BUTTON,OUTPUT_BUTTON,INPUT_BUTTON};

static void button_matrix_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  button_matrix_button = ev->button;
  button_matrix_state = ev->state; 
  active_button = NULL;
  initial_button = (int)gtk_object_get_user_data(GTK_OBJECT(w));
}

static void button_matrix_button_motion(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{
  PANE *p = (PANE *)data;
  GtkWidget *current_button = NULL;
  int x,y;
  int bin=0,bout=0;
  x = widget_x(w);
  y = widget_y(w);
  x += (int)(ev->x - p->bx);
  y += (int)(ev->y - p->by);
  if ((x > (p->bw)) || (y > (p->bh)) || (x<0) || (y<0)) return;
  bin = (int)y * p->in_chans / p->bh;
  bout = (int)x * p->out_chans / p->bw;
  current_button = p->matrix_buttons[bin][bout];
  if ((active_button != current_button) && 
      (((p->active_sliders[bin][bout]) && (button_matrix_button == 2)) ||
       ((!(p->active_sliders[bin][bout])) && (button_matrix_button == 1))))
    {
      handle_matrix_slider(current_button,p,bin,bout,bout * p->in_chans + bin,(p->active_sliders[bin][bout]));
      active_button = current_button;
    }
}

static void button_matrix_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  PANE *p = (PANE *)data;
  int x,y;
  int row,col,on;
  int bin=0,bout=0;
  if (!active_button)
    {
      switch (initial_button)
	{
	case CORNER_BUTTON:
	  for (row=0;row<p->in_chans;row++)
	    for (col=0;col<p->out_chans;col++)
	      {
		if (button_matrix_state & snd_ControlMask)
		  on = (row == col);
		else on = (button_matrix_button == 1);
		if (on != p->active_sliders[row][col])
		  handle_matrix_slider(p->matrix_buttons[row][col],p,row,col,col * p->in_chans + row,!on);
	      }
	  break;
	case OUTPUT_BUTTON:
	  x = widget_x(w);
	  x += (int)(ev->x - p->bx);
	  bout = x*p->out_chans/p->bw;
	  on = (button_matrix_button == 1);
	  for (row=0;row<p->in_chans;row++)
	    if (on != p->active_sliders[row][bout])
	      handle_matrix_slider(p->matrix_buttons[row][bout],p,row,bout,bout * p->in_chans + row,!on);
	  break;
	case INPUT_BUTTON:
	  y = widget_y(w);
	  y += (int)(ev->y - p->by);
	  bin = y*p->in_chans/p->bh;
	  on = (button_matrix_button == 1);
	  for (col=0;col<p->out_chans;col++)
	    if (on != p->active_sliders[bin][col])
	      handle_matrix_slider(p->matrix_buttons[bin][col],p,bin,col,col * p->in_chans + bin,!on);
	  break;
	}
    }
}

static GtkWidget *sndCreateButtonMatrix(snd_state *ss, PANE *p, char *name, GtkWidget *parent, Float meter_size)
{
  GtkWidget *outer_frame,*outer_vbox,*outer_hbox,*top_hbox,*left_vbox,*buttons;
  int ins=2,outs=2,row,col,vu_rows;
  int width,height;
  GtkWidget *outputs_label,*inputs_label0,*inputs_label1,*inputs_label2,*diag_button,*mb;
  slider_info *si;
  int **active_sliders;

  active_sliders = p->active_sliders;
  vu_rows = p->in_chans / 4;
  if (vu_rows == 0) vu_rows = 1;
  height = (int)(vu_rows*(3*2 + LIGHT_Y*meter_size));
  width = (int)(LIGHT_Y*meter_size);

  outer_frame = gtk_frame_new("");
  gtk_box_pack_start(GTK_BOX(parent),outer_frame,FALSE,FALSE,0);
  gtk_widget_show(outer_frame);

  outer_vbox = gtk_vbox_new(FALSE,0);
  gtk_container_add(GTK_CONTAINER(outer_frame),outer_vbox);
  gtk_widget_show(outer_vbox);

  top_hbox = gtk_hbox_new(FALSE,0);
  gtk_box_pack_start(GTK_BOX(outer_vbox),top_hbox,FALSE,FALSE,0);
  gtk_widget_show(top_hbox);

  outer_hbox = gtk_hbox_new(FALSE,0);
  gtk_box_pack_start(GTK_BOX(outer_vbox),outer_hbox,TRUE,TRUE,0);
  gtk_widget_show(outer_hbox);

  left_vbox = gtk_vbox_new(FALSE,0);
  gtk_box_pack_start(GTK_BOX(outer_hbox),left_vbox,FALSE,FALSE,0);
  gtk_widget_show(left_vbox);

  buttons = gtk_table_new(ins,outs,TRUE);
  gtk_box_pack_start(GTK_BOX(outer_hbox),buttons,TRUE,TRUE,0);
  gtk_widget_show(buttons);

  diag_button = gtk_label_new("/ ");
  gtk_box_pack_start(GTK_BOX(top_hbox),diag_button,FALSE,FALSE,0);
  gtk_widget_show(diag_button);

  outputs_label = gtk_label_new("out");
  gtk_box_pack_start(GTK_BOX(top_hbox),outputs_label,TRUE,TRUE,0);
  gtk_widget_show(outputs_label);

  inputs_label0 = gtk_label_new("i");
  gtk_box_pack_start(GTK_BOX(left_vbox),inputs_label0,FALSE,FALSE,0);
  gtk_widget_show(inputs_label0);

  inputs_label1 = gtk_label_new("n");
  gtk_box_pack_start(GTK_BOX(left_vbox),inputs_label1,FALSE,FALSE,0);
  gtk_widget_show(inputs_label1);

  inputs_label2 = gtk_label_new("s");
  gtk_box_pack_start(GTK_BOX(left_vbox),inputs_label2,FALSE,FALSE,0);
  gtk_widget_show(inputs_label2);

  ins = p->in_chans;
  outs = p->out_chans;
  p->matrix_buttons = (GtkWidget ***)CALLOC(ins,sizeof(GtkWidget **));
  for (row=0;row<ins;row++) p->matrix_buttons[row] = (GtkWidget **)CALLOC(outs,sizeof(GtkWidget *));

  for (col=0;col<outs;col++)
    for (row=0;row<ins;row++)
      {
	si = (slider_info *)CALLOC(1,sizeof(slider_info));
	si->p = p;
	si->in_chan = row;
	si->out_chan = col;

	mb = gtk_button_new();
	gtk_table_attach_defaults(GTK_TABLE(buttons),mb,col,col+1,row,row+1);
	gtk_widget_show(mb);
	gtk_signal_connect(GTK_OBJECT(mb),"clicked",GTK_SIGNAL_FUNC(Matrix_Button_Callback),(gpointer)si);
	p->matrix_buttons[row][col] = mb;

#if 0
  /* this on all -- also green=active */
  XtAddEventHandler(outer_frame,ButtonPressMask,FALSE,button_matrix_button_press,(gpointer)p);
  XtAddEventHandler(outer_frame,ButtonMotionMask,FALSE,button_matrix_button_motion,(gpointer)p);
  XtAddEventHandler(outer_frame,ButtonReleaseMask,FALSE,button_matrix_button_release,(gpointer)p);
#endif

      }
  return(outer_frame);
}

/* -------- I/O pane -------- */

int device_channels(int dev);  /* audio.c */
/* for testing, it's convenient to fake up a device here */
/* static int device_channels(int dev) {return(12);} */
int device_gains(int dev);

static PANE *make_pane(snd_state *ss, GtkWidget *paned_window, int device, int system)
{
  /* VU meters (frame, then drawing area widget) */
  /* Linux OSS complication -- the top input panel also has all the "mixer" input volume controls and the output pane has the tone controls, if any */
  Wdesc *wd;
  int i,k,chan,amp_sliders,temp_out_chan,temp_in_chan,in_chan,out_chan;
  AMP *a;
  PANE *p;
  GtkWidget **frames = NULL;
  GtkWidget *frame,*meter,*last_frame,*last_slider,*max_label,*matrix_frame,
    *button_label,*button_box,*first_frame,*left_frame,*spix;
  GtkWidget *vuh,*vuv,*gv,*sbox,*btab,*slabel,*amp_sep;

  VU *vu;
  int vu_meters,num_audio_gains,input,special_cases = 0;
  int row,columns;
  int pane_max;
  Float meter_size,vol;
  state_context *sx;
#if (HAVE_OSS || HAVE_ALSA)
  float mixer_field_chans[32];
  int mixflds[32];
  int last_device,this_device=0;
#endif

  sx = ss->sgx;
  p = (PANE *)CALLOC(1,sizeof(PANE));
  p->device = device;
#ifdef HAVE_ALSA
  p->system = system;
#endif
  p->ss = ss;
  vu_meters = device_channels(MUS_AUDIO_PACK_SYSTEM(system) | device);
  input = (input_device(device));
  num_audio_gains = device_gains(MUS_AUDIO_PACK_SYSTEM(system) | device);
#if (HAVE_OSS || HAVE_ALSA)
  last_device = -1;
  if (num_audio_gains == 0)
    {
      mixer_gains_posted[system] = 0;
      tone_controls_posted[system] = 0;
      last_device = -1;
    }
  else
    {
      if ((input) && (!mixer_gains_posted[system]))
	{
	  for (k=0;k<32;k++) mixer_field_chans[k] = 0.0;
	  mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(system) | MUS_AUDIO_MIXER,MUS_AUDIO_FORMAT,32,mixer_field_chans);
	  for (k=0;k<32;k++) mixflds[k] = (int)mixer_field_chans[k]; /* simplify life later */
	  mixer_gains_posted[system] = device_gains(MUS_AUDIO_PACK_SYSTEM(system) | MUS_AUDIO_MIXER);
	  num_audio_gains = mixer_gains_posted[system]; /* includes the MUS_AUDIO_LINE_IN gains */
	  special_cases = mixer_gains_posted[system];
	}
      if ((!input) && (!tone_controls_posted[system]))
	{
	  tone_controls_posted[system] = device_gains(MUS_AUDIO_PACK_SYSTEM(system) | MUS_AUDIO_DAC_FILTER);
	  num_audio_gains += tone_controls_posted[system];
	  special_cases = tone_controls_posted[system];
	}
    }
  last_device = MUS_AUDIO_MICROPHONE;
#endif

  if (input) 
    {
      p->in_chans = vu_meters;
      p->out_chans = recorder_out_chans(ss);
      /* this determines how many of the left-side buttons we get if chans>4; if defaults to 2 (snd.c)
       * but probably should look at the output device's out chans when we start the recorder for the
       * first time, but not clobber user's setting (if any); perhaps if it's not the default, it can
       * be reset?  And if more than (say) 100 buttons, use pull-down menus?
       */
    }
  else 
    {
      if (vu_meters < recorder_out_chans(ss)) vu_meters = recorder_out_chans(ss);
      p->out_chans = vu_meters;
      p->in_chans = 1;
    }

  p->meters = (VU **)CALLOC(vu_meters,sizeof(VU *));
  p->meters_size = vu_meters;
  p->active = (int *)CALLOC(vu_meters,sizeof(int));
  p->active_size = vu_meters;
  p->active_sliders = (int **)CALLOC(p->in_chans,sizeof(int *));
  for (i=0;i<p->in_chans;i++) p->active_sliders[i] = (int *)CALLOC(p->out_chans,sizeof(int));
  frames = (GtkWidget **)CALLOC(vu_meters,sizeof(GtkWidget *));

  /* paned_window is a vbox = stack of panes, each pane is hbox = meters,sliders | gains */
  p->pane = gtk_hbox_new(FALSE,0);
  gtk_box_pack_start(GTK_BOX(paned_window),p->pane,TRUE,TRUE,6);
  gtk_widget_show(p->pane);

  last_frame = NULL;
  first_frame = NULL;
  left_frame = NULL;
  meter_size = vu_size(ss);
  if (vu_meters > 4) meter_size *= .6; else if (vu_meters > 2) meter_size *= .8;
  if ((vu_meters%5) == 0) meter_size *= 0.8;

  vuv = gtk_vbox_new(FALSE,0);
  gtk_box_pack_start(GTK_BOX(p->pane),vuv,TRUE,TRUE,0);
  gtk_widget_show(vuv);

  gv = gtk_hbox_new(FALSE,0);
  gtk_box_pack_end(GTK_BOX(p->pane),gv,FALSE,FALSE,0);
  gtk_widget_show(gv);

  /* now left side = vuv divided vuh then sliders, right side = gv */

  vuh = gtk_hbox_new(FALSE,0);
  gtk_box_pack_start(GTK_BOX(vuv),vuh,FALSE,FALSE,0);
  gtk_widget_show(vuh);

  if ((input) && ((p->in_chans*p->out_chans) > 8))
    {
      for (i=0;i<p->in_chans;i++) 
	for (k=0;k<p->out_chans;k++) 
	  if (i == k) p->active_sliders[i][k] = 1;
      /* PICKUP DEFAULTS HERE */

      /* rather than default to posting 64 (or 256!) sliders, set up a channel matrix where desired sliders can be set */
#if 0
      matrix_frame = gtk_vbox_new(FALSE,0);
      gtk_box_pack_start(GTK_BOX(vuh),matrix_frame,FALSE,FALSE,0);
      gtk_widget_show(matrix_frame);
#endif
      matrix_frame = sndCreateButtonMatrix(ss,p,"channel-matrix",vuh,meter_size);

      last_frame = matrix_frame;
      left_frame = matrix_frame;
    }
  else 
    {
      for (i=0;i<p->in_chans;i++) 
	for (k=0;k<p->out_chans;k++) 
	  p->active_sliders[i][k]=1;
      /* DEFAULTS?? */
    }

  if ((vu_meters%4) == 0)
    columns = 4;
  else 
    {
      if ((vu_meters%5) == 0)
	columns = 5;
      else columns = vu_meters;
    }
  row = 0;

  for (i=0;i<vu_meters;i++)
    {

      frames[i] = gtk_frame_new("");
      if (row == 0)
	{
	  /* this is the top row of meters, attached to the top of the pane */
	}
      else
	{
	  /* this is a subsequent row, attached to the previous row's frames */
	}
      gtk_box_pack_start(GTK_BOX(vuh),frames[i],FALSE,FALSE,0);
      gtk_container_set_border_width(GTK_CONTAINER(frames[i]),0);
      gtk_widget_show(frames[i]);

      meter = gtk_drawing_area_new();
      gtk_container_add(GTK_CONTAINER(frames[i]),meter);
      gtk_drawing_area_size(GTK_DRAWING_AREA(meter),(int)(240*meter_size),(int)(100*meter_size));
      gtk_widget_show(meter);

      p->meters[i] = make_vu_meter(meter,LIGHT_X,LIGHT_Y,CENTER_X,CENTER_Y,sx->black,ss,meter_size);
      vu = p->meters[i];
      if (input)
	rec_in_VU[overall_input_ctr+i] = vu;
      else rec_out_VU[i] = vu;

      gtk_signal_connect(GTK_OBJECT(meter),"expose_event",GTK_SIGNAL_FUNC(Meter_Expose_Callback),(gpointer)vu);
      gtk_signal_connect(GTK_OBJECT(meter),"configure_event",GTK_SIGNAL_FUNC(Meter_Resize_Callback),(gpointer)vu);

      wd = (Wdesc *)CALLOC(1,sizeof(Wdesc));
      wd->chan = i;
      wd->ss = ss;
      wd->p = p;
      wd->field = MUS_AUDIO_AMP;
      wd->device = device;
      wd->system = system;
      last_frame = frame;
      if ((i == (columns*(row+1) - 1)) && (vu_meters > (i+1))) 
	{
	  last_frame = NULL;
	  first_frame = NULL;
	  row++;
	}
    }
#if 0
  p->button_vertical_sep = gtk_vseparator_new();
  gtk_box_pack_start(GTK_BOX(vuh),p->button_vertical_sep,FALSE,FALSE,0);
  gtk_widget_show(p->button_vertical_sep);
#endif

  btab = gtk_vbox_new(FALSE,0);
  /* controls buttons with label */
  gtk_box_pack_start(GTK_BOX(vuh),btab,TRUE,TRUE,10);
  gtk_widget_show(btab);

  if ((systems == 1) || (!input))
    button_label = gtk_label_new(Device_Name(device));
  else button_label = gtk_label_new(mus_audio_system_name(system));
  gtk_box_pack_start(GTK_BOX(btab),button_label,FALSE,FALSE,0);
  gtk_widget_show(button_label);

  p->on_buttons = (GtkWidget **)CALLOC(vu_meters,sizeof(GtkWidget *));
  p->on_buttons_size = vu_meters;

  for (i=0;i<vu_meters;i++)
    {
      if ((i == 0) || (i == 4))
	{
	  button_box = gtk_hbox_new(TRUE,0);
	  gtk_box_pack_start(GTK_BOX(btab),button_box,FALSE,FALSE,0);
	  gtk_widget_show(button_box);
	}

      p->on_buttons[i] = gtk_button_new_with_label(channel_name(p,i));
      gtk_box_pack_start(GTK_BOX(button_box),p->on_buttons[i],TRUE,TRUE,0);
      gtk_widget_show(p->on_buttons[i]);
    }

  for (i=0;i<vu_meters;i++)
    {
      if ((i == 0) || (i == 4))
	{
	  button_box = gtk_hbox_new(TRUE,0);
	  gtk_box_pack_start(GTK_BOX(btab),button_box,FALSE,FALSE,0);
	  gtk_widget_show(button_box);
	}

      frames[i] = gtk_frame_new("");
      gtk_box_pack_start(GTK_BOX(button_box),frames[i],TRUE,TRUE,2);
      gtk_widget_show(frames[i]);
      
      max_label = gtk_label_new("0.000");
      gtk_container_add(GTK_CONTAINER(frames[i]),max_label);
      gtk_widget_show(max_label);

      wd = (Wdesc *)CALLOC(1,sizeof(Wdesc));
      wd->chan = i;
      wd->ss = ss;
      wd->p = p;
      wd->device = p->device;
      wd->system = system;
      wd->field = MUS_AUDIO_AMP;
      vu = p->meters[i];
      vu->max_button = max_label;
      vu->max_val = 0.0;

      gtk_signal_connect(GTK_OBJECT(p->on_buttons[i]),"clicked",GTK_SIGNAL_FUNC(Meter_Button_Callback),(gpointer)wd);
    }

  p->reset_button = gtk_button_new_with_label(STR_Reset);
  gtk_box_pack_start(GTK_BOX(btab),p->reset_button,TRUE,TRUE,0);
  gtk_widget_show(p->reset_button);
  gtk_signal_connect(GTK_OBJECT(p->reset_button),"clicked",GTK_SIGNAL_FUNC(VU_Reset_Callback),(gpointer)p);

  /* if no audio (hardware) gains, we have the vu separator and the control buttons */
  if (num_audio_gains > 0)
    {
      for (i=0,chan=num_audio_gains-1;i<num_audio_gains;i++,chan--)
	{
	  wd = (Wdesc *)CALLOC(1,sizeof(Wdesc));
	  wd->system = system;
#if (HAVE_OSS || HAVE_ALSA)

	  if (!input)
	    {
#ifdef HAVE_ALSA
	      /* the existing code assumes there are tone controls and that
	       * a certain device is the output (MUS_AUDIO_DAC_OUT), for now
	       * no tone controls at all */
	      wd->chan = 1;
	      wd->field = MUS_AUDIO_AMP;
	      wd->device = device;
	      this_device = device;
#else

	      switch (i)
		{
		case 3: 
		  wd->chan = 1;
		  wd->field = MUS_AUDIO_AMP;
		  wd->device = MUS_AUDIO_DAC_OUT;
		  break;
		case 2: 
		  wd->chan = 0;
		  wd->field = MUS_AUDIO_AMP;
		  wd->device = MUS_AUDIO_DAC_OUT;
		  break;
		case 1: 
		  wd->chan = 0;
		  wd->field = MUS_AUDIO_TREBLE;
		  wd->device = MUS_AUDIO_DAC_FILTER;
		  break;
		case 0: 
		  wd->chan =0;
		  wd->field = MUS_AUDIO_BASS;
		  wd->device = MUS_AUDIO_DAC_FILTER;
		  break;
		}
	      if (i>1) this_device = MUS_AUDIO_DAC_FILTER; else this_device = MUS_AUDIO_DAC_OUT;
#endif
	    }
	  else
	    {
	      /* we want speaker/line-in gains on the far right, then whatever else */
	      if (mixflds[MUS_AUDIO_MICROPHONE] > 0)
		{
		  wd->chan = mixflds[MUS_AUDIO_MICROPHONE]-1;
		  wd->field = MUS_AUDIO_AMP;
		  wd->device = MUS_AUDIO_MICROPHONE;
		  mixflds[MUS_AUDIO_MICROPHONE]--;
		  this_device = MUS_AUDIO_MICROPHONE;
		}
	      else
		{
		  if (mixflds[MUS_AUDIO_LINE] > 0)
		    {
		      wd->chan = mixflds[MUS_AUDIO_LINE]-1;
		      wd->field = MUS_AUDIO_AMP;
		      wd->device = MUS_AUDIO_LINE_IN;
		      mixflds[MUS_AUDIO_LINE]--;
		      this_device = MUS_AUDIO_LINE;
		    }
		  else
		    {
		      wd->chan = 0;
		      for (k=0;k<32;k++) /* MIXER_SIZE used for 32 in audio.c */
			{
			  if (mixflds[k] > 0)
			    {
			      wd->chan = mixflds[k]-1;
			      wd->field = k;
			      wd->device = MUS_AUDIO_MIXER;
			      mixflds[k]--;
			      this_device = k;
			      break;
			    }
			}
		    }
		}
	    }
#else
	  wd->chan = chan;
	  wd->field = MUS_AUDIO_AMP;
	  wd->device = p->device;
#endif
	  wd->ss = ss;
	  wd->p = p;
	  wd->gain = gain_ctr+chan;
	  if (wd->gain > audio_gains_size) 
	    snd_error("%s[%d] %s: overflow %d > %d",__FILE__,__LINE__,__FUNCTION__,wd->gain,audio_gains_size);
	  audio_GAINS[wd->gain] = wd;
	  vol = get_audio_gain(wd);

	  sbox = gtk_vbox_new(FALSE,0);
	  gtk_box_pack_end(GTK_BOX(gv),sbox,FALSE,FALSE,0);
	  gtk_widget_show(sbox);
#if (HAVE_OSS || HAVE_ALSA)
	  if (last_device != this_device)
	    {
	      if ((wd->device == MUS_AUDIO_MICROPHONE) ||
		  (wd->device == MUS_AUDIO_LINE_IN) ||
		  (wd->device == MUS_AUDIO_SPEAKERS) ||
		  (wd->device == MUS_AUDIO_CD))
		{
		  slabel = gtk_button_new();
		  gtk_button_set_relief(GTK_BUTTON(slabel),GTK_RELIEF_NONE);
		  set_background(slabel,(ss->sgx)->basic_color);
		  gtk_box_pack_start(GTK_BOX(sbox),slabel,FALSE,FALSE,0);
		  gtk_widget_show(slabel);
		  
		  spix = gtk_pixmap_new(device_pix(wd->device),device_mask(wd->device));
		  gtk_container_add (GTK_CONTAINER(slabel),spix);
		  gtk_widget_show(spix);
		}
	      else
		{
		  slabel = gtk_label_new(field_abbreviation(this_device));
		  gtk_box_pack_start(GTK_BOX(sbox),slabel,FALSE,FALSE,0);
		  set_label_font(slabel);
		  gtk_widget_show(slabel);
		}
	    }
	  else
	    {
	      slabel = gtk_label_new(" ");
	      gtk_box_pack_start(GTK_BOX(sbox),slabel,FALSE,FALSE,0);
	      set_label_font(slabel);
	      gtk_widget_show(slabel);
	    }
#endif
	  if (vol < 0.0) vol = 0.0;
	  if (vol > 1.0) vol = 1.0;
	  wd->adj = gtk_adjustment_new(1.0 - vol,0.0,1.01,0.001,0.01,.01);
	  wd->wg = gtk_vscale_new(GTK_ADJUSTMENT(wd->adj));
	  gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(wd->wg)),GTK_UPDATE_CONTINUOUS);
	  gtk_scale_set_draw_value(GTK_SCALE(wd->wg),FALSE);
	  gtk_box_pack_start(GTK_BOX(sbox),wd->wg,TRUE,TRUE,0);
	  set_backgrounds(wd->wg,(ss->sgx)->zoom_color);
	  gtk_widget_show(wd->wg);
	  gtk_signal_connect(GTK_OBJECT(wd->adj),"value_changed",GTK_SIGNAL_FUNC(volume_callback),(gpointer)wd);
#if (HAVE_OSS || HAVE_ALSA)
	  last_device = this_device;
#endif
	}
      gain_ctr += num_audio_gains;
    }

  /* now the amp sliders across the bottom of the pane, with 'mixer' info on the right */

  amp_sep = gtk_hseparator_new();
  gtk_box_pack_start(GTK_BOX(vuv),amp_sep,FALSE,FALSE,6);
  gtk_widget_show(amp_sep);

  if (input) 
    amp_sliders = p->in_chans * p->out_chans;
  else amp_sliders = p->out_chans;
  p->amps = (AMP **)CALLOC(amp_sliders,sizeof(AMP *));
  p->amps_size = amp_sliders;
  
  /* input numbering starts at overall_input_ctr and goes up for p->in_chans */
  /* output numbering starts at 0 and goes for p->out_chans */
  /* if input, do direct cases first, then fill in rest of 1, rest of 2 etc */
  temp_out_chan = 0;
  temp_in_chan = 0;
  for (i=0;i<amp_sliders;i++)
    {
      in_chan = temp_in_chan;
      out_chan = temp_out_chan;
      wd = (Wdesc *)CALLOC(1,sizeof(Wdesc));
      wd->chan = i;
      wd->ss = ss;
      wd->p = p;
      wd->device = p->device;
      wd->system = system;
      wd->field = MUS_AUDIO_AMP;
      p->amps[i] = (AMP *)CALLOC(1,sizeof(AMP));
      a = p->amps[i];
      if (input) 
	{
	  a->type = INPUT_AMP; 
	  a->in = temp_in_chan + overall_input_ctr;
	  a->device_in_chan = temp_in_chan;
	  a->out = temp_out_chan;
	  if (temp_in_chan == temp_out_chan)  /* CHECK ACTIVE_SLIDERS HERE OR SIMILAR TABLE (REC_IN_AMPS??) */
	    rec_in_amps[a->in][a->out] = 1.0;
	  else rec_in_amps[a->in][a->out] = 0.0;
	  rec_in_AMPS[a->in][a->out] = p->amps[i];
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
	  rec_out_amps[i] = 1.0;
	  rec_out_AMPS[i] = a;
	}

      a->top = last_slider;
      a->wd = wd;
      if ((!input) || (p->active_sliders[in_chan][out_chan]))
	{
	  sndCreateRecorderSlider(ss,p,a,vuv,input);
	}
      else
	{
	  a->label = NULL;
	  a->number = NULL;
	  a->slider = NULL;
	}
    }
  p->in_chan_loc = overall_input_ctr;
  if (input) overall_input_ctr += p->in_chans;
#if (HAVE_OSS || HAVE_ALSA)
  p->pane_size = pane_max+20;
#else
  p->pane_size = pane_max+50;
#endif

  return(p);
}

#ifdef HAVE_ALSA

static BACKGROUND_TYPE read_adc(snd_state *ss) 
{
  /* assume all in-coming channels are interleaved, (that we know where each is coming from!),
   * rec_amps[...][...] is in this order by first index
   * and second index points to associated output channel
   * value itself is always in-sync with the associated AMP struct's amp field (explicit copy rather than pointer ref)
   * rec_active[...] parallels the rec_amps telling whether the given input is writing to the output
   * rec_in_chans and rec_out_chans guide the array refs
   * vu meter positioning (guided via overall_in_chan field) does not need the max business -- do it direct
   */
  int in_chan,out_chan,i,k,m,n,out_samp,mon_samp,diff,inchn,offset,active_in_chans,ochns,sr,buffer_size,in_datum_size;
  MUS_SAMPLE_TYPE val;
  MUS_SAMPLE_TYPE *fbufs[1];
  if (ever_read == 0) return(BACKGROUND_QUIT); /* should not happen, but ... */
  out_samp = 0;
  mon_samp = 0;
  ochns = recorder_out_chans(ss);
  sr = recorder_srate(ss);
  if (systems == 1)
    {
      active_in_chans = input_channels[0];
      buffer_size = input_buffer_size[0]*input_channels[0];
      if (ochns > active_in_chans) buffer_size /= ochns;
      if (fbuf_size < buffer_size)
	{
	  if (fbuf) FREE(fbuf);
	  fbuf_size = buffer_size;
	  fbuf = (MUS_SAMPLE_TYPE *)CALLOC(fbuf_size,sizeof(MUS_SAMPLE_TYPE));
	}
      in_datum_size = mus_data_format_to_bytes_per_sample(input_format[0]);
      mus_audio_read(record_fd[0],record_buf[0],input_buffer_size[0]*input_channels[0]*in_datum_size);
      fbufs[0] = fbuf;
      mus_file_read_buffer(input_format[0],0,1,input_buffer_size[0]*input_channels[0],fbufs,record_buf[0]);
    }
  else
    {
      active_in_chans = 0;
      /* overall_in_chans is a count of all possible input channels, some of which may be incompatible */
      /* input_channels[i] is how many of these channels can be active at once on a given system */
      for (i=0;i<systems;i++) active_in_chans += input_channels[i];
      offset = 0;
      buffer_size = 0;
      for (i=0;i<systems;i++) 
	buffer_size += input_buffer_size[i]*input_channels[i];
      if (ochns > active_in_chans) buffer_size /= ochns;
      if (fbuf_size < buffer_size)
	{
	  fbuf_size = buffer_size;
	  if (fbuf) FREE(fbuf);
	  fbuf = (MUS_SAMPLE_TYPE *)CALLOC(fbuf_size,sizeof(MUS_SAMPLE_TYPE));
	  if (ffbuf) FREE(ffbuf);
	  ffbuf = (MUS_SAMPLE_TYPE *)CALLOC(fbuf_size,sizeof(MUS_SAMPLE_TYPE));
	}
      fbufs[0] = ffbuf;
      for (i=0;i<systems;i++)
	{
	  in_datum_size = mus_data_format_to_bytes_per_sample(input_format[i]);
	  mus_audio_read(record_fd[i],record_buf[i],input_buffer_size[i]*input_channels[i]*in_datum_size);
	  mus_file_read_buffer(input_format[i],0,1,input_buffer_size[i]*input_channels[i],fbufs,record_buf[i]);
	  for (k=0,m=offset;m<buffer_size;m+=active_in_chans) 
	    for (n=0;n<input_channels[i];n++) 
	      fbuf[m+n] = ffbuf[k++];
	  offset += input_channels[i];
	}
    }
  for (i=0;i<overall_in_chans;i++) {in_max[i] = MUS_SAMPLE_0;}
  for (i=0;i<ochns;i++) {out_max[i] = MUS_SAMPLE_0;}

  /* run through input devices looking for any that are currently turned on */
  /* for each channel currently on, get its associated input channel */

  diff = audio_out_chans - ochns;
  for (i=0,out_samp=0;i<buffer_size;i+=active_in_chans,out_samp++)
    {
      for (out_chan=0;out_chan<ochns;out_chan++) {outvals[out_chan] = MUS_SAMPLE_0;}
      inchn = 0;
      for (in_chan=0;in_chan<overall_in_chans;in_chan++)
	{
	  if (in_device_on[in_chan])
	    {
	      /* inchn = in_device_chan[in_chan]; */
	      val = fbuf[i+inchn];
	      if (rec_in_active[in_chan])
		{
		  for (out_chan=0;out_chan<ochns;out_chan++)
		    {
		      outvals[out_chan] += (MUS_SAMPLE_TYPE)(rec_in_amps[in_chan][out_chan] * val);
		    }
		}
	      if (val<MUS_SAMPLE_0) val=-val; 
	      if (val>in_max[in_chan]) in_max[in_chan]=val;
	      inchn++;
	    }
	}
      for (out_chan=0;out_chan<ochns;out_chan++)
	{
	  val = (MUS_SAMPLE_TYPE)(outvals[out_chan]*rec_out_amps[out_chan]);
	  /* FIXME: originally this code only executes when recording... */
	  // if ((recording) && (rec_out_active[out_chan])) obufs[out_chan][out_samp] = val;
	  if ((rec_out_active[out_chan])) obufs[out_chan][out_samp] = val;
	  if (val<MUS_SAMPLE_0) val=-val;
	  if (val>out_max[out_chan]) out_max[out_chan]=val;
	}
    }
  for (in_chan=0;in_chan<overall_in_chans;in_chan++)
    {
      if (in_device_on[in_chan])
	set_vu_val(rec_in_VU[in_chan],MUS_SAMPLE_TO_FLOAT(in_max[in_chan]));
    }
  for (out_chan=0;out_chan<ochns;out_chan++)
    {
      set_vu_val(rec_out_VU[out_chan],MUS_SAMPLE_TO_FLOAT(out_max[out_chan]));
      if ((!triggered) && (MUS_SAMPLE_TO_FLOAT(out_max[out_chan])>trigger)) triggered=1;
    }
  if ((monitor_open) && (ochns <= audio_out_chans))
    {
      /* opened in recorder_out_format and audio_out_chans */
      mus_file_write_buffer(monitor_out_format,0,out_samp-1,audio_out_chans,obufs,monitor_buf,data_clipped(ss));
      mus_audio_write(monitor_fd,monitor_buf,recorder_buffer_size(ss)*audio_out_chans*mus_data_format_to_bytes_per_sample(monitor_out_format));
    }
  if ((recording) && (triggered))
    {
      mus_file_write(output_fd,0,out_samp-1,ochns,obufs);
      total_out_samps += out_samp;
      if (total_out_samps > duration_samps)
	{
	  update_duration((Float)total_out_samps/(Float)sr);
	  duration_samps += (sr / 4);
	}
    }
  return(((total_out_samps/sr) >= max_duration) ? BACKGROUND_QUIT : BACKGROUND_CONTINUE);
}

#else

static BACKGROUND_TYPE read_adc(snd_state *ss) 
{
  /* assume all in-coming channels are interleaved, (that we know where each is coming from!),
   * rec_amps[...][...] is in this order by first index
   * and second index points to associated output channel
   * value itself is always in-sync with the associated AMP struct's amp field (explicit copy rather than pointer ref)
   * rec_active[...] parallels the rec_amps telling whether the given input is writing to the output
   * rec_in_chans and rec_out_chans guide the array refs
   * vu meter positioning (guided via overall_in_chan field) does not need the max business -- do it direct
   */
  int in_chan,out_chan,i,k,m,n,out_samp,mon_samp,diff,inchn,offset,active_in_chans,cur_size,ochns,sr,sz,ifmt,in_datum_size;
  MUS_SAMPLE_TYPE val;
  MUS_SAMPLE_TYPE *fbufs[1];
  if (ever_read == 0) return(BACKGROUND_QUIT); /* should not happen, but ... */
  fbufs[0] = fbuf;
  out_samp = 0;
  mon_samp = 0;
  ifmt = recorder_in_format(ss);
  ochns = recorder_out_chans(ss);
  sr = recorder_srate(ss);
  sz = recorder_buffer_size(ss);
  in_datum_size = mus_data_format_to_bytes_per_sample(ifmt);
  if (systems == 1)
    {
      active_in_chans = input_channels[0];
      if (ochns > active_in_chans) sz /= ochns;
      mus_audio_read(record_fd[0],record_buf[0],sz*in_datum_size);
      mus_file_read_buffer(ifmt,0,1,sz,fbufs,record_buf[0]);
    }
  else
    {
      active_in_chans = 0;
      /* overall_in_chans is a count of all possible input channels, some of which may be incompatible */
      /* input_channels[i] is how many of these channels can be active at once on a given system */
      for (i=0;i<systems;i++) active_in_chans += input_channels[i];
      offset = 0;
      sz = (int)(((Float)sz/(Float)active_in_chans)+.5);
      sz *= active_in_chans; /* size has to be a multiple of incoming channels */
      if (ochns > active_in_chans) sz /= ochns;
      if (fbuf_size < sz)
	{
	  if (fbuf) FREE(fbuf);
	  fbuf_size = sz;
	  fbuf = (MUS_SAMPLE_TYPE *)CALLOC(fbuf_size,sizeof(MUS_SAMPLE_TYPE));
	}
      if (!ffbuf) ffbuf = (MUS_SAMPLE_TYPE *)CALLOC(sz,sizeof(MUS_SAMPLE_TYPE));
      fbufs[0] = ffbuf;
      for (i=0;i<systems;i++)
	{
	  cur_size = sz * input_channels[i] / active_in_chans;
	  mus_audio_read(record_fd[i],record_buf[i],cur_size*in_datum_size);
	  mus_file_read_buffer(ifmt,0,1,sz,fbufs,record_buf[i]);
	  for (k=0,m=offset;m<sz;m+=active_in_chans) {for (n=0;n<input_channels[i];n++) fbuf[m+n] = ffbuf[k++];}
	  offset += input_channels[i];
	}
    }
  for (i=0;i<overall_in_chans;i++) {in_max[i] = MUS_SAMPLE_0;}
  for (i=0;i<ochns;i++) {out_max[i] = MUS_SAMPLE_0;}

  /* run through input devices looking for any that are currently turned on */
  /* for each channel currently on, get its associated input channel */

  diff = audio_out_chans - ochns;
  for (i=0,out_samp=0;i<sz;i+=active_in_chans,out_samp++)
    {
      for (out_chan=0;out_chan<ochns;out_chan++) {outvals[out_chan] = MUS_SAMPLE_0;}
      inchn = 0;
      for (in_chan=0;in_chan<overall_in_chans;in_chan++)
	{
	  if (in_device_on[in_chan])
	    {
	      /* inchn = in_device_chan[in_chan]; */
	      val = fbuf[i+inchn];
	      if (rec_in_active[in_chan])
		{
		  for (out_chan=0;out_chan<ochns;out_chan++)
		    {
		      outvals[out_chan] += (MUS_SAMPLE_TYPE)(rec_in_amps[in_chan][out_chan] * val);
		    }
		}
	      if (val<MUS_SAMPLE_0) val=-val; 
	      if (val>in_max[in_chan]) in_max[in_chan]=val;
	      inchn++;
	    }
	}
      for (out_chan=0;out_chan<ochns;out_chan++)
	{
	  val = (MUS_SAMPLE_TYPE)(outvals[out_chan]*rec_out_amps[out_chan]);
	  if ((recording) && (rec_out_active[out_chan])) obufs[out_chan][out_samp] = val;
	  if (val<MUS_SAMPLE_0) val=-val;
	  if (val>out_max[out_chan]) out_max[out_chan]=val;
	}
    }
  for (in_chan=0;in_chan<overall_in_chans;in_chan++)
    {
      if (in_device_on[in_chan])
	set_vu_val(rec_in_VU[in_chan],MUS_SAMPLE_TO_FLOAT(in_max[in_chan]));
    }
  for (out_chan=0;out_chan<ochns;out_chan++)
    {
      set_vu_val(rec_out_VU[out_chan],MUS_SAMPLE_TO_FLOAT(out_max[out_chan]));
      if ((!triggered) && (MUS_SAMPLE_TO_FLOAT(out_max[out_chan])>trigger)) triggered=1;
    }

  if ((monitor_open) && (ochns == audio_out_chans))
    {
      /* opened in recorder_out_format and audio_out_chans */
      mus_file_write_buffer(recorder_out_format(ss),0,out_samp-1,audio_out_chans,obufs,record_buf[0],data_clipped(ss));
      mus_audio_write(monitor_fd,record_buf[0],out_samp*mus_data_format_to_bytes_per_sample(recorder_out_format(ss)));
    }
  if ((recording) && (triggered))
    {
      mus_file_write(output_fd,0,out_samp-1,ochns,obufs);
      total_out_samps += out_samp;
      if (total_out_samps > duration_samps)
	{
	  update_duration((Float)total_out_samps/(Float)sr);
	  duration_samps += (sr / 4);
	}
    }
  return(((total_out_samps/sr) >= max_duration) ? BACKGROUND_QUIT : BACKGROUND_CONTINUE);
}
#endif

static void finish_recording(snd_state *ss);

static BACKGROUND_TYPE run_adc(gpointer ss)  /* X wrapper for read_adc */
{
  BACKGROUND_TYPE val;
  val = read_adc((snd_state *)ss);
  if (val == BACKGROUND_QUIT) 
    {
      recording = 0;
      finish_recording((snd_state *)ss);
    }
  return(val);
}

static void set_read_in_progress (snd_state *ss)
{
#ifdef DEBUGGING
  int in_chan;
  char *str;
  str = (char *)CALLOC(512,sizeof(char));
  sprintf(str,"open: srate: %d, format: %s, size: %d, in_chans: %d %d, record_fds: %d %d",
	  recorder_srate(ss),
	  mus_data_format_name(recorder_in_format(ss)),
	  recorder_buffer_size(ss),
	  input_channels[0],input_channels[1],
	  record_fd[0],record_fd[1]);
  record_report(ss,messages,str,NULL);
  for (in_chan=0;in_chan<overall_in_chans;in_chan++)
    {
      sprintf(str,"in[%d, %d] %s (%s): %.3f -> (VU *)%p",
	      in_chan,in_device_chan[in_chan],
	      (in_device_on[in_chan]) ? "on" : "off",
	      (rec_in_active[in_chan]) ? "active" : "idle",
	      MUS_SAMPLE_TO_FLOAT(in_max[in_chan]),
	      rec_in_VU[in_chan]);
      record_report(ss,messages,str,NULL);
    }
  FREE(str);
#endif
  ever_read = gtk_idle_add(run_adc,(gpointer)ss);
  /* ever_read will be explicitly stopped if the recorder is closed */
}

static void sensitize_control_buttons(void)
{
  int i;
  for (i=0;i<device_buttons_size-1;i++) /* last button is autoload_file */
    {
      if (device_buttons[i])
	set_sensitive(device_buttons[i],TRUE);
    }
}

static void unsensitize_control_buttons(void)
{
  int i;
  for (i=0;i<device_buttons_size-1;i++)
    {
      if (device_buttons[i])
	set_sensitive(device_buttons[i],FALSE);
    }
}

void cleanup_recording (void)
{
  if (audio_open) close_recorder_audio();
#if (!(HAVE_OSS || HAVE_ALSA))
  if (recorder) mus_audio_restore();
#endif
  if ((recorder) && (recording) && (output_fd > 0)) 
    {
      recording = 0;
      triggered = (!triggering);
      sensitize_control_buttons();
      snd_close(output_fd);
    }
}

static void Reset_Record_Callback(GtkWidget *w,gpointer clientData) 
{
  /* if recording, cancel and toss data, else reset various fields to default (ss) values */
  snd_state *ss = (snd_state *)clientData;
  char *str;
  PANE *p;
  VU *vu;
  int i,k;
  if (recording)                  /* cancel */
    {
      recording = 0;
      triggered = (!triggering);
      sensitize_control_buttons();
      set_button_label(reset_button,STR_Reset);
      set_background(record_button,(ss->sgx)->basic_color);
      set_button_label(record_button,(triggering) ? STR_Triggered_Record : STR_Record);
      snd_close(output_fd);
      output_fd = -1;
      str = just_filename(recorder_file(ss));
      record_report(ss,messages,str," recording cancelled",NULL);
      FREE(str);
      remove(recorder_file(ss));
    }
  else                            /* reset or restart */
    { 
      for (i=0;i<all_panes_size;i++)
	{
	  p = all_panes[i];
	  for (k=0;k<p->meters_size;k++)
	    {
	      vu = p->meters[k];
	      vu->max_val = 0.0;
	      set_label(vu->max_button,"0.00");
	    }
	}
      /* now if dac turned us off, turn everything back on */
      if (!audio_open)            /* restart */
	{
	  fire_up_recorder(ss);
	  set_button_label(reset_button,STR_Reset);
	}
    }
}

static void Dismiss_Record_Callback(GtkWidget *w, gpointer clientData) 
{
  snd_state *ss = (snd_state *)clientData;
  state_context *sgx;
  sgx = ss->sgx;
  if (recording) Reset_Record_Callback(w,clientData);
  gtk_widget_hide(recorder);
  close_recorder_audio();
#if (!(HAVE_OSS || HAVE_ALSA))
  mus_audio_restore();
#endif
}

static int in_chans_active(void)
{
  PANE *p;
  int val = 0;
  int i,k;
  for (k=0;k<all_panes_size;k++)
    {
      p = all_panes[k];
      if ((p) && (input_device(p->device)))
	{
	  for (i=0;i<p->active_size;i++) {if (p->active[i]) val++;}
	}
    }
  return(val);
}

static int out_chans_active(void)
{
  PANE *p;
  int val = 0;
  int i,k;
  for (k=0;k<all_panes_size;k++)
    {
      p = all_panes[k];
      if ((p) && (!(input_device(p->device))))
	{
	  for (i=0;i<p->active_size;i++) {if (p->active[i]) val++;}
	}
    }
  return(val);
}

static void finish_recording(snd_state *ss)
{
  char *str;
  snd_info *sp;
  Float duration;
  /* total_out_samps -> true duration here for header update */
  sensitize_control_buttons();
  set_background(record_button,(ss->sgx)->basic_color);
  set_button_label(reset_button,STR_Reset);
  set_button_label(record_button,(triggering) ? STR_Triggered_Record : STR_Record);
  snd_close(output_fd);
  output_fd = mus_file_reopen_write(recorder_file(ss));
  mus_header_update_with_fd(output_fd,out_type,total_out_samps*mus_data_format_to_bytes_per_sample(recorder_out_format(ss))); 
  close(output_fd);
  output_fd = -1;
  duration = (Float)total_out_samps / (Float)(recorder_out_chans(ss) * recorder_srate(ss));
  update_duration(duration);
  str = (char *)CALLOC(256,sizeof(char));
  sprintf(str,"recorded %s:\n  duration: %.2f\n  srate: %d, chans: %d\n  type: %s, format: %s",
	  recorder_file(ss),duration,recorder_srate(ss),recorder_out_chans(ss),
	  mus_header_type_name(out_type),mus_data_format_name(recorder_out_format(ss)));
  record_report(ss,messages,str,NULL);
  FREE(str);
  if (recorder_autoload(ss))
    {
      if ((sp = find_sound(ss,recorder_file(ss))))
	snd_update(ss,sp);
      else snd_open_file(recorder_file(ss),ss);
    }
}

static void Record_Button_Callback(GtkWidget *w,gpointer clientData) 
{
  snd_state *ss = (snd_state *)clientData;
  Wdesc *wd;
  int err,i,comlen,old_srate,ofmt,rs,ochns,oloc;
  static char *comment;
  char *str;
  PANE *p;
  recording = (!recording);
  if (recording)
    {
      if (!audio_open) fire_up_recorder(ss);
      str = copy_string(gtk_entry_get_text(GTK_ENTRY(file_text)));
      if ((str) && (*str))
	{
	  in_set_recorder_file(ss,mus_file_full_name(str));
	  FREE(str);
	  str=NULL;
	  old_srate = recorder_srate(ss);
	  read_file_data_choices(recdat,&rs,&ochns,&out_type,&ofmt,&oloc); 
	  in_set_recorder_out_format(ss,ofmt);
	  in_set_recorder_out_chans(ss,ochns);
	  if (rs != old_srate) 
	    {
	      in_set_recorder_srate(ss,rs);
	      set_audio_srate(ss,MUS_AUDIO_DEFAULT,recorder_srate(ss),0);
	    }

	  if (recorder_out_chans(ss) <= 0)
	    {
	      record_report(ss,messages,STR_cant_record_screwed_up_chans,NULL);
	      recording = 0;
	      triggered = (!triggering);
	      return;
	    }

	  if (gtk_text_get_length(GTK_TEXT(recdat->comment_text)) > 0)
	    comment = copy_string(gtk_editable_get_chars(GTK_EDITABLE(recdat->comment_text),0,-1));
	  update_duration(0.0);
	  
	  if (out_chans_active() != recorder_out_chans(ss))
	    {
	      if (msgbuf == NULL) msgbuf = (char *)CALLOC(512,sizeof(char));
	      sprintf(msgbuf,"chans field (%d) doesn't match file out panel (%d channels active)",recorder_out_chans(ss),out_chans_active());
	      record_report(ss,messages,msgbuf,NULL);
	      wd = (Wdesc *)CALLOC(1,sizeof(Wdesc));
	      wd->ss = ss;

	      wd->p = all_panes[out_file_pane];
	      p = wd->p;
	      wd->field = MUS_AUDIO_AMP;
	      wd->device = p->device;
	      wd->system = 0;
	      for (i=0;i<recorder_out_chans(ss);i++)
		{
		  if (!(p->active[i]))
		    {
		      wd->chan = i;
		      Meter_Button_Callback(p->on_buttons[i],(gpointer)wd); /* callData not used */
		    }
		}
	      FREE(wd);
	    }
	  if (in_chans_active() == 0)
	    {
	      record_report(ss,messages,STR_cant_record_no_inputs,NULL);
	      recording = 0;
	      triggered = (!triggering);
	      return;
	    }
	  set_background(w,(ss->sgx)->red);
	  set_button_label(reset_button,STR_Cancel);
	  set_button_label(record_button,STR_Done);
	  comlen = (int)(snd_strlen(comment) + 3)/4;
	  comlen *= 4;
	  err = snd_write_header(ss,recorder_file(ss),out_type,recorder_srate(ss),recorder_out_chans(ss),28+comlen,0,
				 recorder_out_format(ss),comment,snd_strlen(comment),NULL);
	  if (err)
	    {
	      record_report(ss,messages,recorder_file(ss),":\n  ",strerror(errno),"\n  (",snd_error_name(snd_IO_error()),")",NULL);
	      recording = 0;
	      triggered = (!triggering);
	      return;
	    }

	  unsensitize_control_buttons();

	  output_fd = snd_reopen_write(ss,recorder_file(ss));
	  mus_header_read_with_fd(output_fd);
	  mus_file_open_descriptors(output_fd,recorder_out_format(ss),mus_data_format_to_bytes_per_sample(recorder_out_format(ss)),mus_header_data_location());
	  mus_file_set_data_clipped(output_fd,data_clipped(ss));
	  total_out_samps = 0;
	  duration_samps = recorder_srate(ss)/4;
	  max_duration = recorder_max_duration(ss);
	  if (!obufs)
	    obufs = (MUS_SAMPLE_TYPE **)CALLOC(MAX_OUT_CHANS,sizeof(MUS_SAMPLE_TYPE *));
	  for (i=0;i<recorder_out_chans(ss);i++) 
	    {
	      if (!obufs[i])
		obufs[i] = (MUS_SAMPLE_TYPE *)CALLOC(recorder_buffer_size(ss),sizeof(MUS_SAMPLE_TYPE));
	    }
	}
      else
	{
	  record_report(ss,messages,STR_cant_record_no_output_file,NULL);
	  recording = 0;
	  triggered = (!triggering);
	  return;
	}
    }
  else 
    finish_recording(ss);
}

static void initialize_recorder(snd_state *ss);
static GtkWidget *rec_panes,*file_info_pane;

#define AUDVAL_SIZE 64

static void recorder_delete(GtkWidget *w,GdkEvent *event,gpointer clientData)
{
  Dismiss_Record_Callback(w,clientData);
}

void snd_record_file(snd_state *ss)
{
  int n,i,k,device,all_devices,input_devices,output_devices,def_out,system,cur_devices,err;
  int *ordered_devices,*ordered_systems;
  float audval[AUDVAL_SIZE];
  GdkDrawable *wn;
  state_context *sx;
  GtkWidget *rec_panes_box,*help_button,*dismiss_button,*messagetab;

  if (!recorder)
    {
      sx = ss->sgx;
      wn = MAIN_WINDOW(ss);
      draw_gc = gdk_gc_new(wn);
      gdk_gc_set_background(draw_gc,sx->basic_color);
      gdk_gc_set_foreground(draw_gc,sx->black);
      gdk_gc_set_function(draw_gc,GDK_COPY);
      vu_gc = gdk_gc_new(wn);
      gdk_gc_set_background(vu_gc,sx->white);
      gdk_gc_set_foreground(vu_gc,sx->black);
      gdk_gc_set_function(vu_gc,GDK_COPY);

      systems = mus_audio_systems();
      input_devices = 0;
      output_devices = 0;
      all_devices = 0;
#if (HAVE_OSS || HAVE_ALSA)
      mus_audio_mixer_restore(AUDIO_STATE_FILE);
#endif
      for (system=0;system<systems;system++)
	{
#if (HAVE_OSS || HAVE_ALSA)
	  mixer_gains_posted[system] = 0;
	  tone_controls_posted[system] = 0;
#endif
	  /* look for audio input devices -- if none, report problem and quit */
	  err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(system) | MUS_AUDIO_DEFAULT,MUS_AUDIO_PORT,AUDVAL_SIZE,audval);
	  if (err != 0) snd_error("%s[%d] %s: read audio state: %s ",__FILE__,__LINE__,__FUNCTION__,mus_audio_error_name(mus_audio_error()));
	  cur_devices = (int)(audval[0]);
	  if (cur_devices == 0) {snd_error("no audio devices available"); return;}
	  for (i=0;i<cur_devices;i++) 
	    {
	      device = (int)(audval[i+1]);
	      if (input_device(device))
		input_devices++;
	      else 
		{
		  if ((system == 0) && (output_device(device)))
		    output_devices++;
		}
	      audio_gains_size += device_gains(MUS_AUDIO_PACK_SYSTEM(system) | device);
	    }
	  all_devices += cur_devices;
	}
      if (input_devices == 0) {snd_error("no audio input devices available"); return;}
      all_panes_size = input_devices + 1;  /* one for all output devices -- maybe there should be more */

      /* or perhaps the output pane could have all outputs openable with meters but only one set of master sliders? */

      ordered_devices = (int *)CALLOC(all_panes_size,sizeof(int));
      ordered_systems = (int *)CALLOC(all_panes_size,sizeof(int));
      all_panes = (PANE **)CALLOC(all_panes_size,sizeof(PANE *));
      device_buttons_size = input_devices + 2; /* inputs, one output, autoload_file */
      device_buttons = (GtkWidget **)CALLOC(device_buttons_size,sizeof(GtkWidget *));
      audio_gains = (Float *)CALLOC(audio_gains_size,sizeof(Float));
      audio_GAINS = (Wdesc **)CALLOC(audio_gains_size,sizeof(Wdesc *));
      /* out_file_pane will be the bottom (output) audio pane, not the file info pane */
      overall_in_chans = 0;
      def_out = 2;
      k=0;
      for (system=0;system<systems;system++)
	{
	  mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(system) | MUS_AUDIO_DEFAULT,MUS_AUDIO_PORT,AUDVAL_SIZE,audval);
	  cur_devices = (int)(audval[0]);
	  for (i=0;i<cur_devices;i++)
	    {
#ifdef HAVE_ALSA
	      float direction = 0.0;
#endif
	      device = (int)audval[i+1];
#ifdef HAVE_ALSA
	      if ((err=mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(system)|device,MUS_AUDIO_DIRECTION,0,&direction))==0 &&
		  (int)direction == 1)
#else
	      if (input_device(device))
#endif
		{
		  n = device_channels(MUS_AUDIO_PACK_SYSTEM(system) | device);
		  overall_in_chans += n;
		  if (n > def_out) def_out = n;
		  ordered_devices[k] = device;
		  ordered_systems[k] = system;
		  if (device == MUS_AUDIO_DIGITAL_IN) digital_in_button = k;
		  else if (device == MUS_AUDIO_MICROPHONE) microphone_button = k;
		  else if (device == MUS_AUDIO_LINE_IN) line_in_button = k;
		  k++;
		}
	    }
	}
#ifdef HAVE_ALSA
      /* search for output devices, first one wins, previous code
       * was assuming one particular device existed */
      for (system=0;system<systems;system++)
	{
	  mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(system)|MUS_AUDIO_DEFAULT,MUS_AUDIO_PORT,AUDVAL_SIZE,audval);
	  cur_devices = (int)(audval[0]);
	  for (i=0;i<cur_devices;i++)
	    {
	      float direction=0.0;
	      device = (int)audval[i+1];
	      if ((err=mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(system)|device,MUS_AUDIO_DIRECTION,0,&direction))==0) 
		{
		  if ((int)direction == 0)
		    {
		      ordered_devices[k] = device;
		      ordered_systems[k] = system;
		      goto done;
		    }
		}
	    }
	}
    done:
#else
      if (output_devices) ordered_devices[k] = MUS_AUDIO_DAC_OUT;
#endif
      rec_in_active = (int *)CALLOC(overall_in_chans,sizeof(int));
      rec_out_active = (int *)CALLOC(MAX_OUT_CHANS,sizeof(int));
      rec_in_VU = (VU **)CALLOC(overall_in_chans,sizeof(VU *));
      rec_out_VU = (VU **)CALLOC(MAX_OUT_CHANS,sizeof(VU *));
      outvals = (MUS_SAMPLE_TYPE *)CALLOC(MAX_OUT_CHANS,sizeof(MUS_SAMPLE_TYPE));
      out_max = (MUS_SAMPLE_TYPE *)CALLOC(MAX_OUT_CHANS,sizeof(MUS_SAMPLE_TYPE));
      in_max = (MUS_SAMPLE_TYPE *)CALLOC(overall_in_chans,sizeof(MUS_SAMPLE_TYPE));
      in_device_on = (int *)CALLOC(overall_in_chans,sizeof(int));
      in_device_chan = (int *)CALLOC(overall_in_chans,sizeof(int));
      rec_in_amps = (Float **)CALLOC(overall_in_chans,sizeof(Float *));
      rec_in_AMPS = (AMP ***)CALLOC(overall_in_chans,sizeof(AMP **));
      for (i=0;i<overall_in_chans;i++) 
	{
	  rec_in_amps[i] = (Float *)CALLOC(MAX_OUT_CHANS,sizeof(Float));
	  rec_in_AMPS[i] = (AMP **)CALLOC(MAX_OUT_CHANS,sizeof(AMP *));
	}
      rec_out_amps = (Float *)CALLOC(MAX_OUT_CHANS,sizeof(Float));  /* monitor (speaker) vol, not file-output */
      rec_out_AMPS = (AMP **)CALLOC(MAX_OUT_CHANS,sizeof(AMP *));
      /* out chans defaults to def_out = parallel to the maximal input choice or 2 whichever is more */
      if (def_out < 2) def_out = 2;
      small_font = gdk_font_load((vu_size(ss) < SMALLER_FONT_CUTOFF) ? SMALLER_FONT : SMALL_FONT);

      recorder = gtk_dialog_new();
      gtk_signal_connect(GTK_OBJECT(recorder),"delete_event",GTK_SIGNAL_FUNC(recorder_delete),(gpointer)ss);
      gtk_window_set_title(GTK_WINDOW(recorder),STR_Record);
      gtk_window_set_policy(GTK_WINDOW(recorder),TRUE,TRUE,FALSE); /* allow shrink or grow */
      set_background(recorder,(ss->sgx)->basic_color);
      gtk_container_set_border_width (GTK_CONTAINER(recorder), 10);
      gtk_widget_set_usize(GTK_WIDGET(recorder),840,820);
      gtk_widget_realize(recorder);
      add_dialog(ss,recorder);

      help_button = gtk_button_new_with_label(STR_Help);
      dismiss_button = gtk_button_new_with_label(STR_Dismiss);
      reset_button = gtk_button_new_with_label(STR_Reset);
      record_button = gtk_button_new_with_label(STR_Record);

      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(recorder)->action_area),dismiss_button,TRUE,TRUE,10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(recorder)->action_area),reset_button,TRUE,TRUE,10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(recorder)->action_area),record_button,TRUE,TRUE,10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(recorder)->action_area),help_button,TRUE,TRUE,10);

      gtk_signal_connect(GTK_OBJECT(dismiss_button),"clicked",GTK_SIGNAL_FUNC(Dismiss_Record_Callback),(gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(help_button),"clicked",GTK_SIGNAL_FUNC(Help_Record_Callback),(gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(reset_button),"clicked",GTK_SIGNAL_FUNC(Reset_Record_Callback),(gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(record_button),"clicked",GTK_SIGNAL_FUNC(Record_Button_Callback),(gpointer)ss);
      set_pushed_button_colors(help_button,ss);
      set_pushed_button_colors(dismiss_button,ss);
      set_pushed_button_colors(reset_button,ss);
      set_active_color(record_button,(ss->sgx)->red);
      gtk_widget_show(dismiss_button);
      gtk_widget_show(reset_button);
      gtk_widget_show(record_button);
      gtk_widget_show(help_button);

      rec_panes = gtk_vpaned_new();
      gtk_container_add(GTK_CONTAINER(GTK_DIALOG(recorder)->vbox),rec_panes);
      gtk_widget_show(rec_panes);

      rec_panes_box = gtk_vbox_new(FALSE,0);
      gtk_paned_add1(GTK_PANED(rec_panes),rec_panes_box);
      gtk_widget_show(rec_panes_box);

      gtk_widget_show(recorder);
      make_record_icons(recorder,ss);

      /* open all audio devices and collect ins/out etc */
      out_file_pane = (all_panes_size - 1);

      /* see note above (line 2809) about recorder out chans */
      n = device_channels(MUS_AUDIO_PACK_SYSTEM(ordered_systems[out_file_pane]) | ordered_devices[out_file_pane]);
      if ((recorder_out_chans(ss) == DEFAULT_RECORDER_OUT_CHANS) && (n > 2))
	in_set_recorder_out_chans(ss,n);

      for (i=0;i<all_panes_size;i++)
	{
	  /* last one is output */
	  device = ordered_devices[i];
	  system = ordered_systems[i];
	  all_panes[i] = make_pane(ss,rec_panes_box,device,system);
	}

      /* then make file_info_pane and messages at the bottom */
      file_info_pane = gtk_frame_new("");
      gtk_box_pack_end(GTK_BOX(rec_panes_box),file_info_pane,FALSE,FALSE,0);
      gtk_widget_show(file_info_pane);

      make_file_info_pane(ss,file_info_pane,ordered_devices,all_panes_size,ordered_systems);

      messagetab = make_message_pane(ss);
      gtk_paned_add2(GTK_PANED(rec_panes),messagetab);
      gtk_widget_show(messagetab);

      initialize_recorder(ss);
    }
  gtk_widget_show(recorder);

  if (pending_errors>0)
    {
      for (i=0;i<pending_errors;i++)
	{
	  record_report(ss,messages,pending_error[i],NULL);
	  FREE(pending_error[i]);
	}
      pending_errors = 0;
      FREE(pending_error);
      pending_error_size = 0;
    }
  
  if (!audio_open) fire_up_recorder(ss);
}

typedef struct {
  int which,vali,valj;
  Float valf;
} recorder_setf;

static recorder_setf **setfs = NULL;
static int setfs_size = 0;
static int current_setf = 0;

static void add_pending_setf(int which, int vali, int valj, Float valf)
{
  if (current_setf >= setfs_size)
    {
      setfs_size += 16;
      if (setfs) 
	setfs = (recorder_setf **)REALLOC(setfs,setfs_size * sizeof(recorder_setf *));
      else
	setfs = (recorder_setf **)CALLOC(setfs_size,sizeof(recorder_setf *));
    }
  setfs[current_setf] = (recorder_setf *)CALLOC(1,sizeof(recorder_setf));
  setfs[current_setf]->which = which;
  setfs[current_setf]->vali = vali;
  setfs[current_setf]->valj = valj;
  setfs[current_setf]->valf = valf;
  current_setf++;
}

static Float scan_pending_setfs(int which, int vali, int valj)
{
  int i;
  Float val;
  recorder_setf *rs;
  val = 0.0;
  for (i=0;i<current_setf;i++)
    {
      rs = setfs[i];
      if (which == rs->which)
	{
	  switch (which)
	    {
	    case REC_IN_AMPS: if ((vali == rs->vali) && (valj == rs->valj)) val = rs->valf; break;
	    case REC_OUT_AMPS:        
	    case AUDIO_GAINS: if (vali == rs->vali) val = rs->valf; break;
	    }
	}
    }
  return(val);
}

void set_autoload(snd_state *ss, int val)
{
  in_set_recorder_autoload(ss,val);
  if (recorder) set_toggle_button(device_buttons[autoload_button],val,FALSE,(void *)(all_panes[autoload_button])); 
}

Float read_record_state(int which, int vali, int valj)
{
  if (!recorder) /* check list of pending setfs */
    return(scan_pending_setfs(which,vali,valj));
  else
    {
      switch (which)
	{
	case REC_IN_AMPS:  return(rec_in_amps[vali][valj]); break;
	case REC_OUT_AMPS: return(rec_out_amps[vali]); break;
	case AUDIO_GAINS:  return(audio_gains[vali]); break;
	}
    }
  return(0.0);
}

void write_record_state(int which, int vali, int valj, Float valf)
{
  /* snd-clm input port-hole */
  Float temp;
  Wdesc *wd;
  AMP *a;
  switch (which)
    {
    case REC_IN_AMPS:     
      if (recorder)
	{
	  a = rec_in_AMPS[vali][valj];
	  temp = amp_to_slider(valf); 
	  record_amp_changed(a,temp); 
	  GTK_ADJUSTMENT(a->adj)->value = valf;
	  gtk_adjustment_value_changed(GTK_ADJUSTMENT(a->adj));
	}
      else add_pending_setf(which,vali,valj,valf);
      break;
    case REC_OUT_AMPS:
      if (recorder)
	{
	  a = rec_out_AMPS[vali];
	  temp = amp_to_slider(valf); 
	  record_amp_changed(a,temp);
	  GTK_ADJUSTMENT(a->adj)->value = valf;
	  gtk_adjustment_value_changed(GTK_ADJUSTMENT(a->adj));
	}
      else add_pending_setf(which,vali,valj,valf);
      break;
    case AUDIO_GAINS:
      if (recorder)
	{
	  wd = audio_GAINS[vali];
	  GTK_ADJUSTMENT(wd->adj)->value = valf;
	  gtk_adjustment_value_changed(GTK_ADJUSTMENT(wd->adj));
	  set_audio_gain(wd,valf);
	}
      else add_pending_setf(which,vali,valj,valf);
      break;
    }
}

static void initialize_recorder(snd_state *ss)
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
  for (i=0;i<MAX_SOUNDCARDS;i++) record_buf[i] = NULL;
#if OLD_SGI_AL
  sb[0] = AL_INPUT_SOURCE;
  err = ALgetparams(AL_DEFAULT_DEVICE,sb,2);
  if ((!err) && (sb[1] == AL_INPUT_DIGITAL)) in_digital = 1;
  set_toggle_button(device_buttons[digital_in_button],in_digital,FALSE,(void *)(all_panes[digital_in_button]));
  device_button_callback(device_buttons[digital_in_button],all_panes[digital_in_button]);
#endif
#if NEW_SGI_AL || defined(SUN)
  /* for simplicity, and until someone complains, new SGI AL machines will just have one active input device */
  active_device_button = microphone_button;
  for (i=0;i<device_buttons_size-1;i++)
    {
      if (device_buttons[i])
	{
	  if ((i != microphone_button) && (input_device(all_panes[i]->device)))
	    {
	      set_toggle_button(device_buttons[i],FALSE,TRUE,(void *)(all_panes[i])); 
	    }
	}
    }
#endif
  if (recorder_trigger(ss) != 0.0) set_recorder_trigger(ss,recorder_trigger(ss));
  max_duration = recorder_max_duration(ss);
  if (max_duration<=0.0) max_duration = 1000000.0;
  for (i=0;i<current_setf;i++)
    write_record_state(setfs[i]->which,setfs[i]->vali,setfs[i]->valj,setfs[i]->valf);
}


#if HAVE_ALSA

static int fire_up_recorder(snd_state *ss)
{
  int i, j;
  PANE *p;
  float val[64];
  float direction=0.0;
  int size,sys,dev,sysdev,in_count;
  int err,new_srate = 0;
  if (!fbuf) 
    {
      fbuf_size = recorder_buffer_size(ss);
      fbuf = (MUS_SAMPLE_TYPE *)CALLOC(fbuf_size,sizeof(MUS_SAMPLE_TYPE));
    }
  for (i=0;i<systems;i++) 
    record_fd[i] = -1;
  if (settings_saved) 
    {
      mus_audio_restore(); 
    } 
  else 
    {
      mus_audio_save();
    }
  /* the recorder_srate sometimes depends purely on external devices */
  if (recorder_srate(ss) <= 0) 
    in_set_recorder_srate(ss,22050);
  err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_DEFAULT,MUS_AUDIO_SRATE,0,val);
  if (!err) 
    {
      new_srate = (int)val[0];
      if ((new_srate > 0) && (recorder_srate(ss) != new_srate)) 
	set_recorder_srate(ss,new_srate);
    }
  audio_out_chans = 2;
  for (i=0;i<overall_in_chans;i++) 
    {
      in_device_on[i] = 1; 
      in_device_chan[i] = i;
    }

  /* Select first input device for each system */

  /* FIXME: there is one srate, format and so on for each system. The current code
   * is not checking for them being compatible. At least srate and buffer length
   * have to be shared by all enabled recording devices. The code should check for
   * that and somehow disable devices that are incompatible. The previous global
   * state variables are being set with the results of the first system scanned */

  in_count = 0;
  for (i=0;i<all_panes_size;i++)
    {
      p = all_panes[i];
      sys = p->system;
      dev = p->device;
      sysdev = MUS_AUDIO_PACK_SYSTEM(sys)|dev;
      if ((err=mus_audio_mixer_read(sysdev,MUS_AUDIO_DIRECTION,0,&direction))==0) 
	{
	  if (input_channels[sys]==0 && (int)direction==1)
	    {
	      if ((err=mus_audio_mixer_read(sysdev,MUS_AUDIO_CHANNEL,2,val))==0) 
		input_channels[sys]=(int)val[0];
	      if ((err=mus_audio_mixer_read(sysdev,MUS_AUDIO_SRATE,2,val))==0)
		{
		  input_srate[sys]=(int)val[0];
		  if (i == 0) set_recorder_srate(ss,input_srate[sys]);
		}
	      input_format[sys]=mus_audio_compatible_format(sysdev);
	      if (i == 0) in_set_recorder_in_format(ss,input_format[sys]);
	      if ((err=mus_audio_mixer_read(sysdev,MUS_AUDIO_SAMPLES_PER_CHANNEL,0,val))==0)
		{
		  input_buffer_size[sys]=(int)(val[0]);
		  if (i == 0) 
		    {
		      in_set_recorder_buffer_size(ss,input_buffer_size[sys]);
		      if ((recorder) && (rec_size_text))
			{
			  sprintf(timbuf,"%d",recorder_buffer_size(ss));
			  XmTextSetString(rec_size_text,timbuf);
			}
		    }
		  if (!(record_buf[sys]))
		    record_buf[sys] = (char *)CALLOC(input_buffer_size[sys]*input_channels[sys],sizeof(int));
		}
	    }
	  if (input_channels[sys]>0)
	    in_count += input_channels[sys];
	}
    }
  if (in_count == 0) 
    {
      record_report(ss,messages,"no inputs?: ",mus_audio_error_name(mus_audio_error()),NULL);
      return(-1);
    }

  /* open all input devices */
  for (i=0;i<all_panes_size;i++) 
    {
      p = all_panes[i];
      sys = p->system;
      dev = p->device;
      sysdev = MUS_AUDIO_PACK_SYSTEM(sys)|dev;
      if ((err=mus_audio_mixer_read(sysdev,MUS_AUDIO_DIRECTION,0,&direction))==0) 
	{
	  if ((int)direction == 1)
	    {
	      size = mus_data_format_to_bytes_per_sample(input_format[sys]);
	      record_fd[sys] = mus_audio_open_input(sysdev,recorder_srate(ss),input_channels[sys],input_format[sys],
						    input_buffer_size[sys]*input_channels[sys]*size);
	      if (record_fd[sys] == -1)
		{
		  record_report(ss,messages,"open device[%d:%d]: ",mus_audio_error_name(mus_audio_error()),NULL);
		  for (j=0;j<all_panes_size;j++)
		    {
		      sys = all_panes[j]->system;
		      if (record_fd[sys]!=-1)
			{
			  mus_audio_close(record_fd[sys]);
			  record_fd[sys] = -1;
			}
		    }
		  return(-1);
		}
	    }
	}
    }
  audio_open = 1;

  /* search for output devices for monitoring, first one wins */

  for (i=0;i<all_panes_size;i++) 
    {
      p = all_panes[i];
      sys = p->system;
      dev = p->device;
      sysdev = MUS_AUDIO_PACK_SYSTEM(sys)|dev;
      if ((err=mus_audio_mixer_read(sysdev,MUS_AUDIO_DIRECTION,0,&direction))==0) 
	{
	  if ((int)direction == 0)
	    {
	      /* found the first pane that has an output device (must be the only one) */
	      if ((err=mus_audio_mixer_read(sysdev,MUS_AUDIO_CHANNEL,2,val))==0) 
		{
		  audio_out_chans=(int)(val[0]);
		  /* FIXME: what would be the proper value for this? 
		   * the computer wedges if I don't initialize it, why? */
		  in_set_recorder_out_chans(ss,audio_out_chans);
		}
	      monitor_out_format = mus_audio_compatible_format(sysdev);
	      size = mus_data_format_to_bytes_per_sample(monitor_out_format);
	      monitor_fd = mus_audio_open_output(sysdev,recorder_srate(ss),audio_out_chans,monitor_out_format,
						 recorder_buffer_size(ss)*audio_out_chans*size);
		if (monitor_fd != -1) 
		  {
		    if (!obufs)
		      obufs = (MUS_SAMPLE_TYPE **)CALLOC(MAX_OUT_CHANS,sizeof(MUS_SAMPLE_TYPE *));
		    for (i=0;i<audio_out_chans;i++) 
		      {
			if (obufs[i]) FREE(obufs[i]);
			obufs[i] = (MUS_SAMPLE_TYPE *)CALLOC(recorder_buffer_size(ss),sizeof(MUS_SAMPLE_TYPE));
		      }
		    if (!monitor_buf)
		      monitor_buf = (char *)CALLOC(recorder_buffer_size(ss)*audio_out_chans*size,1);
		    monitor_open = monitor_fd;
		  }
		else
		  {
		    record_report(ss,messages,"open output: ",mus_audio_error_name(mus_audio_error()),NULL);
		    monitor_open = 0;
		  }
		break;
	    }
	}
    }

  set_read_in_progress(ss);
  return(0);
}

#else /* not ALSA */

static int fire_up_recorder(snd_state *ss)
{
  int i;
#if NEW_SGI_AL
  int j,n;
  PANE *p;
#endif
  float val[8];
  int err,new_srate = 0;
#ifdef SGI
  int cur_dev;
  long sb[8];
#endif
  for (i=0;i<systems;i++)
    if (!(record_buf[i]))
      record_buf[i] = (char *)CALLOC(recorder_buffer_size(ss),sizeof(int));
  if (!fbuf) 
    {
      fbuf_size = recorder_buffer_size(ss);
      fbuf = (MUS_SAMPLE_TYPE *)CALLOC(fbuf_size,sizeof(MUS_SAMPLE_TYPE));
    }
  for (i=0;i<systems;i++) record_fd[i] = -1;
#if HAVE_OSS
  if (settings_saved) mus_audio_restore(); else mus_audio_save();
#else
  mus_audio_save();
#endif

  /* the recorder_srate sometimes depends purely on external devices */
  
#ifdef SGI
  cur_dev = MUS_AUDIO_MICROPHONE;
  #if OLD_SGI_AL
    sb[0] = AL_INPUT_SOURCE;
    err = ALgetparams(AL_DEFAULT_DEVICE,sb,2);
    if (!err)
      {
	if (sb[1] == AL_INPUT_LINE)
	  cur_dev = MUS_AUDIO_LINE_IN;
	else
	  if (sb[1] == AL_INPUT_DIGITAL)
	    cur_dev = MUS_AUDIO_DIGITAL_IN;
	if (cur_dev == MUS_AUDIO_DIGITAL_IN)
	  sb[0] = AL_DIGITAL_INPUT_RATE;
	else sb[0] = AL_INPUT_RATE;
	err = ALgetparams(AL_DEFAULT_DEVICE,sb,2);
	if (!err)
	  new_srate = sb[1];
	if (cur_dev == MUS_AUDIO_DIGITAL_IN) 
	  input_channels[0] = 2;
	else input_channels[0] = 4;
      }
    else
      {
	err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_MICROPHONE,MUS_AUDIO_SRATE,0,val);
	if (!err) in_set_recorder_srate(ss,(int)val[0]);
      }
    if ((new_srate > 0) && (new_srate != recorder_srate(ss))) set_recorder_srate(ss,new_srate);
    audio_out_chans = input_channels[0];
    for (i=0;i<4;i++) 
      {
        in_device_on[i] = (cur_dev != MUS_AUDIO_DIGITAL_IN);
	in_device_chan[i] = i;
      }
    in_device_on[4] = (cur_dev == MUS_AUDIO_DIGITAL_IN);
    in_device_chan[4] = 0; 
    in_device_on[5] = (cur_dev == MUS_AUDIO_DIGITAL_IN);
    in_device_chan[5]=1;
    if (cur_dev != MUS_AUDIO_DIGITAL_IN) mus_audio_mixer_write(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_MICROPHONE,MUS_AUDIO_CHANNEL,2,NULL);
  #endif
  #if NEW_SGI_AL
    err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_MICROPHONE,MUS_AUDIO_SRATE,0,val);
    new_srate = (int)val[0];
    if (!err) 
      if ((new_srate > 0) && (recorder_srate(ss) != new_srate)) set_recorder_srate(ss,new_srate);
    audio_out_chans = 2;
    for (i=0;i<all_panes_size;i++)
      {
	p = all_panes[i];
	if (input_device(p->device))
	  {
	    for (j=p->in_chan_loc,n=0;n<p->in_chans;n++,j++) 
	      {
		in_device_on[j]=(p->device == MUS_AUDIO_MICROPHONE);
		in_device_chan[j] = n;
	      }
	  }
      }
  #endif
#else /* not SGI */
    if (recorder_srate(ss) <= 0) in_set_recorder_srate(ss,22050);
  #ifdef SUN
    /* turn on "monitor" */
    val[0] = 1.0;
    mus_audio_mixer_write(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_MICROPHONE,MUS_AUDIO_IGAIN,0,val);
    in_device_on[0] = 1;
    in_device_chan[0] = 0;
    in_device_on[1] = 0;
    in_device_chan[1] = 1;
  #else
    err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_DEFAULT,MUS_AUDIO_SRATE,0,val);
    if (!err) 
      {
	new_srate = (int)val[0];
	if ((new_srate > 0) && (recorder_srate(ss) != new_srate)) set_recorder_srate(ss,new_srate);
      }
    audio_out_chans = 2;
    for (i=0;i<overall_in_chans;i++) 
      {
	in_device_on[i] = 1; 
	in_device_chan[i] = i;
      }
  #endif
#endif

  for (i=0;i<systems;i++)
    {
      if (input_channels[i] == 0)
	{
#if OLD_SGI_AL
	  input_channels[i] = ((cur_dev == MUS_AUDIO_DIGITAL_IN) ? 2 : 4);
	  audio_out_chans = input_channels[i];
#else
  #ifdef SUN
	  input_channels[i] = device_channels(MUS_AUDIO_PACK_SYSTEM(i) | MUS_AUDIO_MICROPHONE);
  #else
	  input_channels[i] = device_channels(MUS_AUDIO_PACK_SYSTEM(i) | MUS_AUDIO_DEFAULT);
	  if (input_channels[i] == 0)
	    {
	      input_channels[i] = device_channels(MUS_AUDIO_PACK_SYSTEM(i) | MUS_AUDIO_MICROPHONE);
	      if (input_channels[i] == 0)
		{
		  input_channels[i] = device_channels(MUS_AUDIO_PACK_SYSTEM(i) | MUS_AUDIO_LINE_IN);
		  if (input_channels[i] == 0)
		    {
		      input_channels[i] = device_channels(MUS_AUDIO_PACK_SYSTEM(i) | MUS_AUDIO_ADAT_IN);
		    }
		}
	    }
  #endif
#endif
	}
    }

  err = 1;
  for (i=0;i<systems;i++) if (input_channels[i] > 0) {err = 0; break;}
  if (err)
    {
      record_report(ss,messages,"no inputs?: ",mus_audio_error_name(mus_audio_error()),NULL);
      return(-1);
    }

#if NEW_SGI_AL
  record_fd[0] = mus_audio_open_input(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_MICROPHONE,recorder_srate(ss),input_channels[0],
				  recorder_in_format(ss),recorder_buffer_size(ss));
#else
  #if OLD_SGI_AL
    record_fd[0] = mus_audio_open_input(MUS_AUDIO_PACK_SYSTEM(0) | cur_dev,recorder_srate(ss),input_channels[0],
				    recorder_in_format(ss),recorder_buffer_size(ss));
  #else
    #ifdef SUN
    record_fd[0] = mus_audio_open_input(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_MICROPHONE,recorder_srate(ss),input_channels[0],
				    recorder_in_format(ss),recorder_buffer_size(ss));
    #else
    /* if adat, aes etc, make choices about default on/off state, open monitor separately (and write) */

    for (i=0;i<systems;i++)
      {
	record_fd[i] = mus_audio_open_input(MUS_AUDIO_PACK_SYSTEM(i) | MUS_AUDIO_DUPLEX_DEFAULT,recorder_srate(ss),input_channels[i],
					recorder_in_format(ss),recorder_buffer_size(ss));
	if (record_fd[i] == -1) /* perhaps not full-duplex */
	  {
	    record_fd[i] = mus_audio_open_input(MUS_AUDIO_PACK_SYSTEM(i) | MUS_AUDIO_DEFAULT,recorder_srate(ss),input_channels[i],
					    recorder_in_format(ss),recorder_buffer_size(ss));
	  }
      }
    #endif
  #endif
#endif
  if (record_fd[0] == -1)
    {
      record_report(ss,messages,"open device: ",mus_audio_error_name(mus_audio_error()),NULL);
      return(-1);
    }
  audio_open = 1;
#if ((HAVE_OSS) || defined(SUN))
  monitor_fd = 0;

  /*
   * if (full_duplex(0))
   *   monitor_fd = mus_audio_open_output(MUS_AUDIO_DUPLEX_DEFAULT,recorder_srate(ss),audio_out_chans,recorder_out_format(ss),recorder_buffer_size(ss));
   * else record_report(ss,messages,"Simultaneous input and output is not enabled on this card.",NULL);
   */

#else
  monitor_fd = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_DAC_OUT,recorder_srate(ss),audio_out_chans,
				 recorder_out_format(ss),recorder_buffer_size(ss));
#endif
  if (monitor_fd == -1)
    {
      record_report(ss,messages,"open output: ",mus_audio_error_name(mus_audio_error()),NULL);
      monitor_open = 0;
    }
#if ((HAVE_OSS) || defined(SUN))
  else monitor_open = monitor_fd;
#else
  else monitor_open = 1;
#endif
  set_read_in_progress(ss);
  return(0);
}
#endif

static void set_record_size (snd_state *ss, int new_size)
{
  int i;
  lock_recording_audio();
  if (new_size > recorder_buffer_size(ss))
    {
      in_set_recorder_buffer_size(ss,new_size);
      if (ffbuf)
	{
	  FREE(ffbuf);
	  ffbuf = (MUS_SAMPLE_TYPE *)CALLOC(new_size,sizeof(MUS_SAMPLE_TYPE));
	}
      if (obufs)
	{
	  for (i=0;i<recorder_out_chans(ss);i++) 
	    {
	      if (obufs[i]) 
		{
		  FREE(obufs[i]);
		  obufs[i] = (MUS_SAMPLE_TYPE *)CALLOC(new_size,sizeof(MUS_SAMPLE_TYPE));
		}
	    }
	}
      for (i=0;i<systems;i++)
	{
	  if (record_buf[i]) 
	    {
	      FREE(record_buf[i]);
	      record_buf[i] = (char *)CALLOC(new_size,sizeof(int));
	    }
	}
      if (fbuf) 
	{
	  FREE(fbuf);
	  fbuf_size = new_size;
	  fbuf = (MUS_SAMPLE_TYPE *)CALLOC(fbuf_size,sizeof(MUS_SAMPLE_TYPE));
	}
    }
  else in_set_recorder_buffer_size(ss,new_size);
  if ((recorder) && (rec_size_text)) 
    {
      sprintf(timbuf,"%d",recorder_buffer_size(ss));
      gtk_entry_set_text(GTK_ENTRY(rec_size_text),timbuf);
    }
  unlock_recording_audio();
}

int record_dialog_is_active(void)
{
  return((recorder) && (GTK_WIDGET_VISIBLE(recorder)));
}

void set_recorder_trigger(snd_state *ss, Float val)
{
  in_set_recorder_trigger(ss,val);
  if (recorder)
    {
      GTK_ADJUSTMENT(trigger_adj)->value = val;
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(trigger_adj));
      internal_trigger_set(val);
    }
}

void set_recorder_max_duration(snd_state *ss, Float val)
{
  in_set_recorder_max_duration(ss,val);
  max_duration = val;
}

void set_recorder_srate(snd_state *ss, int val)
{
  char sbuf[8];
  if (val < 1000) return; /* TODO figure out where "2" is coming from! */
  /* this just reflects the setting in the text field -- it doesn't actually set anything in the audio system */
  if (val > 0)
    {
      /* SGI AES In sometimes claims its srate is 0 */
      in_set_recorder_srate(ss,val);
      if (recorder) 
	{
	  sprintf(sbuf,"%d",recorder_srate(ss));
	  gtk_entry_set_text(GTK_ENTRY(recdat->srate_text),sbuf);
	}
    }
}


#if HAVE_GUILE_GTK
#include <guile-gtk.h>
#include "sg.h"

#define Sg_recorder_dialog_widget  "sg-recorder-dialog-widget"

static SCM sg_recorder_dialog_widget(void) 
{
  #define H_recorder_dialog_widget "HELP"
  return(sgtk_wrap_gtkobj((GtkObject *)(recorder)));
}

void init_recorder_widgets(SCM local_doc)
{
  DEFINE_PROC(gh_new_procedure0_0(Sg_recorder_dialog_widget,sg_recorder_dialog_widget),H_recorder_dialog_widget);
}

#endif
