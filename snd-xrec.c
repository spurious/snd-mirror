/* TODO: split out stuff that is not widget-dependent into snd-rec.c
 *         (there's a trap here -- record_report has different args in g/x)
 *       re-merge with snd-grec.c
 *       multiple inputs in new sgi al on properly equipped machines (how do Indys behave in this case?)
 *       smoothed amp sliders(?)
 *       clicks upon swap or whatever (set process priority?)
 *       on-going output sonogram (this is just an fft+running display)?
 *       CD input if supported (i.e. on the Sun)
 *       continuous record (flushing output until some indication)
 *       output panel should support max chans found in all output devices, and have some way to send output anywhere
 *       recorder-defaults device {function} -> layout, chans, whether included and active, etc (using sndlib device numbers)
 *       dB in VUs
 *       syncd sliders ("master" volume control basically)
 *       in kde, recorder not redrawn upon desktop switch
 */

#include "snd.h"

#if HAVE_XPM
  #include <X11/xpm.h>
#endif


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
  Widget meter;
  Widget max_button;
  Display *dp;
  Drawable wn;
  int scr;
  int on_off;
  int clipped;
  Float current_val,last_val;
  Float max_val;
  Float red_deg;
  Float size;
  int light_x, light_y, center_x, center_y;
  snd_state *ss;
  Pixmap off_label;
  Pixmap on_label;
  Pixmap clip_label;
} VU;

typedef struct {
  Widget label, number, slider, top;
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
  Widget *on_buttons;
  Widget reset_button;
  Widget pane;
  Widget button_vertical_sep;
  Dimension pane_size;
  int device,system;          /* audio.c style device descriptor */
  int **active_sliders;
  Widget **matrix_buttons;
  Position bx,by;
  Dimension bw,bh;
} PANE;

typedef struct {
  snd_state *ss;
  int chan,field,device,system;
  int gain;
  PANE *p;
  Widget wg;
} Wdesc;



#define SMALL_FONT_CUTOFF .85
#define SMALLER_FONT_CUTOFF .7
#define SMALL_FONT "6x10"
#define SMALLER_FONT "5x7"

static char timbuf[64];
static char *msgbuf = NULL;

static int systems = 1;             /* multi-card setups */
static int audio_open = 0;          /* input active */
static int monitor_open = 0;        /* speakers active (monitor_fd open) */

static int record_fd[MAX_SOUNDCARDS]; /* input (audio hardware) channel (mus_audio_read from this) */
static int monitor_fd = -1;         /* output ("monitor" not recorded file) port -- mus_audio_write to this */
static int output_fd = -1;          /* output file descriptor (mus_file_write to this) */

static int audio_out_chans = 2;     /* number of channels being "monitored" -- i.e. output chans sent to the "output" pane
				     *   not chans being sent to output file; used in conjunction with monitor_fd.
				     * for example, on some SGI's you can have 4 incoming chans, 
				     *   any number of recorded chans, but only 2 speaker chans
				     *   and on some Linux setups, you can have 2 incoming chans,
				     *   but no ("full duplex") speaker chans.
				     */
static XtWorkProcId ever_read = 0;  /* X work proc for recorder (defaults to being a background process) */
static file_data *recdat;

#ifdef LINUX
  static int out_type = MUS_RIFF;
#else
  static int out_type = MUS_AIFC;
#endif

/* on the SGI 1024 is very prone to clicks */
static char *record_buf[MAX_SOUNDCARDS];           /* incoming data has not yet been converted to sndlib representation */
#if (HAVE_ALSA || HAVE_OSS)
static int monitor_out_format;
static char *monitor_buf;
#endif
static MUS_SAMPLE_TYPE *fbuf = NULL;
static MUS_SAMPLE_TYPE *ffbuf = NULL;
static int fbuf_size = 0;
static MUS_SAMPLE_TYPE **obufs = NULL;            /* formatted non-interleaved output */
static int total_out_frames,duration_frames;      /* used to be "_samps" but it's actually counting frames, causing confusion for multi-channel takes */
static Float max_duration;
static int overall_in_chans;
static int input_channels[MAX_SOUNDCARDS];

#if (HAVE_ALSA || HAVE_OSS)
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

static Widget recorder = NULL;      /* the outer dialog shell */
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

static Widget rec_size_text,trigger_scale,trigger_label;
static Widget file_duration,messages,record_button,reset_button,file_text;
static Widget *device_buttons;
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
static XM_FONT_TYPE small_fontlist;

static vu_label **vu_labels = NULL;
static int vu_labels_size = 0;
static int current_vu_label = 0;

static char **pending_error = NULL;
static int pending_errors = 0;
static int pending_error_size = 0;

static void set_read_in_progress (snd_state *ss);

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
      XtRemoveWorkProc(ever_read);
      ever_read = 0;
    }
  if (monitor_open)
    {
      mus_audio_close(monitor_fd);
      monitor_open = 0;
    }
}

static void record_report(Widget text, ...)
{
  /* place time-stamped message in text window */
  time_t ts;
  int pos;
  va_list ap;
  char *nextstr;
  int textpos = 0;
  if (msgbuf == NULL) msgbuf = (char *)CALLOC(512,sizeof(char));
#if (!defined(HAVE_CONFIG_H)) || defined(HAVE_STRFTIME)
  time(&ts);
  strftime(timbuf,64,"%H:%M:%S",localtime(&ts));
  sprintf(msgbuf,"\n[%s] ",timbuf);
#endif
  pos = XmTextGetLastPosition(text);
  if (pos == 0) 
    XmTextSetString(text,msgbuf);
  else XmTextInsert(text,pos,msgbuf);
  va_start(ap,text);
  while ((nextstr = va_arg(ap,char *)))
    {
      textpos = XmTextGetLastPosition(text);
      XmTextInsert(text,textpos,nextstr);
    }
  XmTextShowPosition(text,XmTextGetLastPosition(text)-1);
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
      if (recorder) record_report(messages,"set input source: ",mus_audio_error_name(mus_audio_error()),NULL);
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

#if 0
static int full_duplex(int system)
{
  float val[1];
  mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(system) | MUS_AUDIO_DUPLEX_DEFAULT,MUS_AUDIO_CHANNEL,0,val);
  return((int)(val[0]));
}
#endif

#if HAVE_XPM
static void attach_error(char *msg)
{
  /* need to report errors that crop up during recorder initialization (when the message window is unready) */
  if (pending_errors == pending_error_size)
    {
      pending_error_size += 8;
      if (pending_errors == 0)
	pending_error = (char **)CALLOC(pending_error_size,sizeof(char *));
      else pending_error = (char **)REALLOC(pending_error,pending_error_size * sizeof(char *));
    }
  pending_error[pending_errors] = copy_string(msg);
  pending_errors++;
}
#endif


/* -------------------------------- ROTATE TEXT -------------------------------- */
/* rotate and scale text */

static Float degrees_to_radians(Float x) {return(x * 3.14159265 / 180.0);}

static GC draw_gc,vu_gc;

static Pixmap transform_text (Widget w, char *str, XFontStruct *font, Float angle_in_degrees, 
			      Float xscl, Float yscl, int *nw, int *nh, Pixel bg, Pixel fg)
{
  /* scale str in font font by xscl and yscl, then rotate clockwise by angle_in_degrees degrees */
  /* (i.e. 45 points text south-east), new bounding box (text centered in it) returned in nw and nh */
  /* returned pixmap must be freed (if at all) by the caller */
  /* bg = background color, fg = foreground (text) color) */
  Float matrix[4];
  Float angle_in_radians;
  XImage *before,*after;
  Pixmap pix,rotpix;
  unsigned int width,height,depth,nwidth,nheight,x,y,nx,ny,tx,ty,depth_bytes;
  int inx,iny;
  char *data;
  unsigned long px;
  Display *dp;
  Drawable wn;
  Visual *vis;
  int scr;
  int bx0,bx1,by0,by1,b;
  int i,j;
  if (str == NULL) return(0);
  /* set up transformation matrix */
  angle_in_radians = degrees_to_radians(angle_in_degrees);
  matrix[0] = cos(angle_in_radians) * xscl;
  matrix[1] = sin(angle_in_radians) * xscl;
  matrix[2] = -sin(angle_in_radians) * yscl;
  matrix[3] = cos(angle_in_radians) * yscl;
  
  /* get the usual X info */
  dp = XtDisplay(w);
  wn = XtWindow(w);
  scr = DefaultScreen(dp);
  vis = DefaultVisual(dp,scr);

  XtVaGetValues(w,XmNdepth,&depth,NULL);
  depth_bytes = (depth>>3);
  if (depth_bytes == 0) depth_bytes=1; /* unsigned so can't be negative */

  /* find extent of original text, expand out to byte boundaries */
  XSetFont(dp,draw_gc,font->fid);
  width = XTextWidth(font,str,strlen(str));
  height = (font->ascent+font->descent);
  if (width%8) width = 8*(1 + (int)(width/8));
  if (height%8) height = 8*(1+(int)(height/8));

  /* get bounding box of transformed text */
  bx0=0; bx1=0; by0=0; by1=0;
  b=(int)(width*matrix[0]);
  if (b<0) bx0=b; else bx1=b;
  b=(int)(height*matrix[2]);
  if (b<0) bx0+=b; else bx1+=b;
  b=(int)(width*matrix[1]);
  if (b<0) by0=b; else by1=b;
  b=(int)(height*matrix[3]);
  if (b<0) by0+=b; else by1+=b;
  
  /* set translation vector so we're centered in the resultant pixmap */
  if (bx0<0) tx=-bx0; else tx=0;
  if (by0<0) ty=-by0; else ty=0;
  nx = bx1-bx0;
  ny = by1-by0;

  /* expand result bounds to byte boundaries */
  if (nx%8) nwidth = 8*(1+(int)(nx/8)); else nwidth = nx;
  if (ny%8) nheight = 8*(1+(int)(ny/8)); else nheight = ny;
  (*nw) = nwidth;
  (*nh) = nheight;

  XSetBackground(dp,draw_gc,bg); 
  XSetForeground(dp,draw_gc,bg); 

  /* create pixmaps, fill with background color, write string to pix */
  pix = XCreatePixmap(dp,wn,width,height,depth);
  rotpix= XCreatePixmap(dp,wn,nwidth,nheight,depth);
  XFillRectangle(dp,pix,draw_gc,0,0,width,height);
  XFillRectangle(dp,rotpix,draw_gc,0,0,nwidth,nheight);
#ifdef SUN
  XSync(dp,0);
  /* needed to get the numbers drawn at all */
#endif
  XSetForeground(dp,draw_gc,fg);
  XDrawImageString(dp,pix,draw_gc,0,height,str,strlen(str));

  /* dump pixmap bits into an "image", image data will be freed automatically later */
  data = (char *)CALLOC((width+1)*(height+1)*depth_bytes,sizeof(char));
  before = XCreateImage(dp,vis,depth,XYPixmap,0,data,width,height,8,0);
  XGetSubImage(dp,pix,0,0,width,height,AllPlanes,XYPixmap,before,0,0);
  data = (char *)CALLOC((nwidth+1)*(nheight+1)*depth_bytes,sizeof(char));
  after = XCreateImage(dp,vis,depth,XYPixmap,0,data,nwidth,nheight,8,0);

  /* clear background of result image */
  for (x=0;x<nwidth;x++) {for (y=0;y<nheight;y++) XPutPixel(after,x,y,bg);}

  /* write transformed pixels to result image */
  for (x=0;x<width;x++)
    {
      for (y=0;y<height;y++)
	{
	  px = XGetPixel(before,x,y);
	  if (px != bg)
	    {
	      bx0 = (int)xscl; if (bx0 == 0) bx0=1;  /* draw full lines if possible (i.e. fill in scaled gaps) */
	      by0 = (int)yscl; if (by0 == 0) by0=1;
	      for (i=0;i<bx0;i++)
		{
		  for (j=0;j<by0;j++)
		    {
		      inx = tx + round((x+(Float)i/xscl)*matrix[0] + (y+(Float)j/yscl)*matrix[2]);  
		      if (inx<0) inx=0; 
		      if (inx>=(int)nwidth) inx=nwidth-1;
		      iny = ty + round((x+(Float)i/xscl)*matrix[1] + (y+(Float)j/yscl)*matrix[3]); 
		      if (iny<0) iny=0; 
		      if (iny>=(int)nheight) iny=nheight-1;
		      XPutPixel(after,inx,iny,px);
		    }
		}
	    }
	}
    }

  /* dump image into result pixmap (needed for later display) */
  XPutImage(dp,rotpix,draw_gc,after,0,0,0,0,nwidth,nheight);

  /* cleanup */
  XDestroyImage(before);  /* frees data as well, or so claims the documentation */
  XDestroyImage(after);
  XFreePixmap(dp,pix);
  return(rotpix);
}

/* -------------------------------- ICONS -------------------------------- */

static unsigned char speaker_bits[] = {
   0x00, 0x07, 0xc0, 0x04, 0x30, 0x04, 0x0e, 0x04, 0x06, 0x04, 0x06, 0x04,
   0x06, 0x04, 0x06, 0x04, 0x0e, 0x04, 0x30, 0x04, 0xc0, 0x04, 0x00, 0x07};

static unsigned char mic_bits[] = {
   0xf0, 0x00, 0x58, 0x01, 0xa8, 0x01, 0xf8, 0x01, 0x08, 0x01, 0x0f, 0x0f,
   0x09, 0x09, 0x09, 0x09, 0xf9, 0x09, 0xf1, 0x08, 0x61, 0x08, 0xff, 0x0f};

#ifdef SGI
/* SGI form */
static unsigned char line_in_bits[] = {
   0x00, 0x04, 0x40, 0x02, 0x20, 0x02, 0x24, 0x01, 0x12, 0x01, 0xff, 0x0f,
   0x12, 0x01, 0x24, 0x01, 0x20, 0x02, 0x40, 0x02, 0x00, 0x04, 0x00, 0x00};
#else
/* Sun form */
static unsigned char line_in_bits[] = {
   0x20, 0x00, 0x20, 0x00, 0x20, 0x00, 0x70, 0x00, 0xa8, 0x00, 0x24, 0x01,
   0xaa, 0x02, 0x72, 0x02, 0x22, 0x02, 0x04, 0x01, 0x88, 0x00, 0x70, 0x00};
#endif

static unsigned char aes_bits[] = {
   0xe6, 0x3d, 0x29, 0x04, 0x29, 0x04, 0x29, 0x04, 0xef, 0x3c, 0x29, 0x20,
   0x29, 0x20, 0x29, 0x20, 0xe9, 0x3d};

static unsigned char digin_bits[] = {
   0x03, 0x0c, 0x03, 0x0c, 0x03, 0x0c, 0xff, 0x0f, 0xff, 0x0f, 0xab, 0x0e,
   0x57, 0x0d, 0xff, 0x0f, 0xff, 0x0f, 0x03, 0x0c, 0x03, 0x0c, 0x03, 0x0c};

static unsigned char adat_bits[] = {
   0x40, 0x10, 0x40, 0x10, 0x40, 0x38, 0x77, 0x17, 0x55, 0x15, 0x55, 0x15,
   0x55, 0x15, 0x7f, 0x1f};

static unsigned char cd_bits[] = {
   0xf8, 0x00, 0x06, 0x03, 0x72, 0x02, 0x89, 0x04, 0x75, 0x05, 0x55, 0x05,
   0x75, 0x05, 0x89, 0x04, 0x72, 0x02, 0x06, 0x03, 0xf8, 0x00, 0x00, 0x00};

static Pixmap speaker_icon,line_in_icon,mic_icon,aes_icon,adat_icon,digital_in_icon,cd_icon;

static void make_record_icons(Widget w, snd_state *ss)
{
  int depth;
  XtVaGetValues(w,XmNdepth,&depth,NULL);
  speaker_icon = make_pixmap(ss,speaker_bits,12,12,depth,draw_gc);
  mic_icon = make_pixmap(ss,mic_bits,12,12,depth,draw_gc);
  line_in_icon = make_pixmap(ss,line_in_bits,12,12,depth,draw_gc);
  digital_in_icon = make_pixmap(ss,digin_bits,12,12,depth,draw_gc);
  aes_icon = make_pixmap(ss,aes_bits,14,9,depth,draw_gc);
  adat_icon = make_pixmap(ss,adat_bits,14,8,depth,draw_gc);
  cd_icon = make_pixmap(ss,cd_bits,12,12,depth,draw_gc);
}

static Pixmap device_icon(int device)
{
  switch (device)
    {
    case MUS_AUDIO_CD:      return(cd_icon); break;
    case MUS_AUDIO_DIGITAL_OUT:
    case MUS_AUDIO_LINE_OUT:
    case MUS_AUDIO_DEFAULT:
    case MUS_AUDIO_DAC_OUT:
    case MUS_AUDIO_DUPLEX_DEFAULT:
    case MUS_AUDIO_SPEAKERS:   return(speaker_icon); break;
    case MUS_AUDIO_ADAT_OUT:
    case MUS_AUDIO_ADAT_IN:    return(adat_icon); break;
    case MUS_AUDIO_SPDIF_IN:
    case MUS_AUDIO_SPDIF_OUT:
    case MUS_AUDIO_AES_OUT:  
    case MUS_AUDIO_AES_IN:     return(aes_icon); break;
    case MUS_AUDIO_LINE_IN:    return(line_in_icon); break;
    case MUS_AUDIO_DIGITAL_IN: return(digital_in_icon); break;
    case MUS_AUDIO_MICROPHONE: 
    default:                return(mic_icon); break;
    }

}


/* -------------------------------- VU METER -------------------------------- */

static XFontStruct *get_vu_font(snd_state *ss, Float size)
{
  char font_name[64];
  int font_size;
  char *vu_font_name;
  XFontStruct *label_font;
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
  
  label_font = XLoadQueryFont(MAIN_DISPLAY(ss),font_name);
  if (!(label_font))
    {
      sprintf(font_name,"-*-%s-*-*-*-*-%d-*-*-*-*-*-*",vu_font_name,font_size);
      label_font=XLoadQueryFont(MAIN_DISPLAY(ss),font_name);
      if (!(label_font))
	{
	  sprintf(font_name,"-*-courier-*-*-*-*-%d-*-*-*-*-*-*",font_size);
	  label_font=XLoadQueryFont(MAIN_DISPLAY(ss),font_name);
	  while (!(label_font))
	    {
	      sprintf(font_name,"-*-*-*-*-*-*-%d-*-*-*-*-*-*",font_size);
	      label_font=XLoadQueryFont(MAIN_DISPLAY(ss),font_name);
	      font_size++;
	      if (font_size>60) {label_font=XLoadQueryFont(MAIN_DISPLAY(ss),"-*-*-*-*-*-*-*-*-*-*-*-*-*"); break;}
	    }
	}
    }
  return(label_font);
}

#if HAVE_XPM
static int allocate_meter_2(Widget w, vu_label *vu)
{
  int off_err,err;
  Pixmap shape;
  Display *dp;
  Drawable wn;
  char *buf;
  dp = XtDisplay(w);
  wn = XtWindow(w);
  off_err = XpmCreatePixmapFromData(dp,wn,offlabel_bits(),&(vu->off_label),&shape,NULL);   
  if (off_err != XpmSuccess) 
    {
      buf = (char *)CALLOC(128,sizeof(char));
      sprintf(buf,"can't create VU meter's label: %s\n",XpmGetErrorString(off_err));
      attach_error(buf);
      FREE(buf);
      return(off_err);
    }
  err = XpmCreatePixmapFromData(dp,wn,onlabel_bits(),&(vu->on_label),&shape,NULL);     
  if (err != XpmSuccess) 
    {
      buf = (char *)CALLOC(128,sizeof(char));
      sprintf(buf,"trouble with VU meter's 'on label' (%s)\nwill use the colorless 'off label'\n",XpmGetErrorString(err));
      attach_error(buf);
      FREE(buf);
      vu->on_label = vu->off_label;
    }
  err = XpmCreatePixmapFromData(dp,wn,cliplabel_bits(),&(vu->clip_label),&shape,NULL); 
  if (err != XpmSuccess) 
    {
      buf = (char *)CALLOC(128,sizeof(char));
      sprintf(buf,"trouble with VU meter's 'clip label' (%s)\nwill use the colorless 'off label'\n",XpmGetErrorString(err));
      attach_error(buf);
      FREE(buf);
      vu->clip_label = vu->off_label;
    }
  return(XpmSuccess);
}
#endif

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

static Pixel yellows[VU_COLORS];
static Pixel reds[VU_COLORS];
static int vu_colors_allocated = 0;
static int yellow_vals[] = {0,16,32,64,96,128,160,175,185,200,210,220,230,240};

static void allocate_meter_1(snd_state *ss, vu_label *vu)
{
  /* called only if size not allocated yet and size=1.0 not available as pre-made pixmap */
  int scr;
  Display *dp;
  Drawable wn;
  Colormap cmap;
  XColor tmp_color;
  int i,j,k;
  Pixel white,black,red;
  unsigned int depth;
  int band;
  XPoint pts[16];
  int x0,y0,x1,y1;
  Float rdeg;
  Float size;
  Pixmap numbers[VU_NUMBERS];
  int wids[VU_NUMBERS],hgts[VU_NUMBERS];
  int xs[5],ys[5];

  Float BAND_X;
  Float BAND_Y;

  size = vu->size;
  BAND_X = 2.75*size;
  BAND_Y = 3.25*size;
  red = (ss->sgx)->red;
  dp = XtDisplay(recorder);
  wn = XtWindow(recorder);
  scr = DefaultScreen(dp);
  cmap = DefaultColormap(dp,scr);
  black = BlackPixel(dp,scr);
  white = WhitePixel(dp,scr);
  XtVaGetValues(recorder,XmNdepth,&depth,NULL);
  if (!vu_colors_allocated)
    {
      vu_colors_allocated = 1;
      tmp_color.flags = DoRed | DoGreen | DoBlue;
      tmp_color.red = (unsigned short)65535;
      for (i=0;i<VU_COLORS;i++)
	{
	  tmp_color.blue = (unsigned short)(256*yellow_vals[i]);
	  tmp_color.green = (unsigned short)(256*230 + 26*yellow_vals[i]);
	  XAllocColor(dp,cmap,&tmp_color);
	  yellows[i] = tmp_color.pixel;
	}
      for (i=0;i<VU_COLORS;i++)
	{
	  tmp_color.blue = (unsigned short)(128*yellow_vals[i]);
	  tmp_color.green = (unsigned short)(128*yellow_vals[i]);
	  XAllocColor(dp,cmap,&tmp_color);
	  reds[i] = tmp_color.pixel;
	}
    }
  /* need two versions of these, one with white bg and black fg for "off" state, then... */
  j=0;
  numbers[j] = transform_text(recorder,"0.0",vu->label_font,-40.0,1.0,1.0,&wids[j],&hgts[j],yellows[9],red); j++;
  numbers[j] = transform_text(recorder,"0.25",vu->label_font,-25.0,1.0,1.0,&wids[j],&hgts[j],yellows[7],red); j++;
  numbers[j] = transform_text(recorder,"0.5",vu->label_font,0.0,1.0,1.0,&wids[j],&hgts[j],yellows[7],red); j++;
  numbers[j] = transform_text(recorder,"0.75",vu->label_font,23.0,1.0,1.0,&wids[j],&hgts[j],yellows[7],red); j++;
  numbers[j] = transform_text(recorder,"1.0",vu->label_font,40.0,1.0,1.0,&wids[j],&hgts[j],yellows[9],red); j++;
  numbers[j] = transform_text(recorder,"0.0",vu->label_font,-40.0,1.0,1.0,&wids[j],&hgts[j],white,black); j++;
  numbers[j] = transform_text(recorder,"0.25",vu->label_font,-25.0,1.0,1.0,&wids[j],&hgts[j],white,black); j++;
  numbers[j] = transform_text(recorder,"0.5",vu->label_font,0.0,1.0,1.0,&wids[j],&hgts[j],white,black); j++;
  numbers[j] = transform_text(recorder,"0.75",vu->label_font,23.0,1.0,1.0,&wids[j],&hgts[j],white,black); j++;
  numbers[j] = transform_text(recorder,"1.0",vu->label_font,40.0,1.0,1.0,&wids[j],&hgts[j],white,black); j++;
  numbers[j] = transform_text(recorder,"0.0",vu->label_font,-40.0,1.0,1.0,&wids[j],&hgts[j],reds[9],black); j++;
  numbers[j] = transform_text(recorder,"0.25",vu->label_font,-25.0,1.0,1.0,&wids[j],&hgts[j],reds[7],black); j++;
  numbers[j] = transform_text(recorder,"0.5",vu->label_font,0.0,1.0,1.0,&wids[j],&hgts[j],reds[7],black); j++;
  numbers[j] = transform_text(recorder,"0.75",vu->label_font,23.0,1.0,1.0,&wids[j],&hgts[j],reds[7],black); j++;
  numbers[j] = transform_text(recorder,"1.0",vu->label_font,40.0,1.0,1.0,&wids[j],&hgts[j],reds[9],black); j++;
      
  for (k=0;k<2;k++) 
    {
      band = 1;
      if (k == 1)
	{
	  vu->clip_label = XCreatePixmap(dp,wn,(unsigned int)(CENTER_X*2*size),(unsigned int)(CENTER_Y*size),depth);
	  XSetForeground(dp,draw_gc,reds[0]);	    
	  XFillRectangle(dp,vu->clip_label,draw_gc,0,0,(unsigned int)(CENTER_X*2*size),(unsigned int)(CENTER_Y*size));
	}
      else 
	{
	  vu->on_label = XCreatePixmap(dp,wn,(unsigned int)(CENTER_X*2*size),(unsigned int)(CENTER_Y*size),depth);
	  XSetForeground(dp,draw_gc,yellows[2]);
	  XFillRectangle(dp,vu->on_label,draw_gc,0,0,(unsigned int)(CENTER_X*2*size),(unsigned int)(CENTER_Y*size));
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
	XFillPolygon(dp,vu->clip_label,draw_gc,pts,7,Convex,CoordModeOrigin);
      else XFillPolygon(dp,vu->on_label,draw_gc,pts,7,Convex,CoordModeOrigin);

      for (i=1;i<VU_COLORS;i++)
	{
	  band += i;
	  if (k == 1) 
	    XSetForeground(dp,draw_gc,reds[i]); 
	  else 
	    {
	      if (i<2) XSetForeground(dp,draw_gc,yellows[2]); else XSetForeground(dp,draw_gc,yellows[i]);
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
	    XFillPolygon(dp,vu->clip_label,draw_gc,pts,13,Complex,CoordModeOrigin);
	  else XFillPolygon(dp,vu->on_label,draw_gc,pts,13,Complex,CoordModeOrigin);
	  for (j=0;j<6;j++) 
	    { 
	      /* set up initial portion of next polygon */
	      pts[j].x = pts[j+6].x;
	      pts[j].y = pts[j+6].y;
	    }
	}
    }

  vu->off_label = XCreatePixmap(dp,wn,(unsigned int)(CENTER_X*2*size),(unsigned int)(CENTER_Y*size),depth);
  /* not on, so just display a white background */
  XSetForeground(dp,draw_gc,white);
  XFillRectangle(dp,vu->off_label,draw_gc,0,0,(unsigned int)(CENTER_X*2*size),(unsigned int)(CENTER_Y*size));

  XSetForeground(dp,draw_gc,black);

  /* draw the numbers */
  xs[0] = (int)(((size >= 1.0) ? (size*(CENTER_X - 114)) : (size*(CENTER_X - 116))));
  ys[0] = (int)(((size >= 1.0) ? (size*(CENTER_Y - 116)) : (size*(CENTER_Y - 119))));
  xs[1] = (int)(size*(CENTER_X - 71));
  ys[1] = (int)(size*(CENTER_Y - 147));
  xs[2] = (int)(size*(CENTER_X - 11));
  ys[2] = (int)(size*(CENTER_Y - 153));
  xs[3] = (int)(size*(CENTER_X + 42));
  ys[3] = (int)(size*(CENTER_Y - 145));
  xs[4] = (int)(size*(CENTER_X + 88));
  ys[4] = (int)(size*(CENTER_Y - 116));

  j=0;
  XCopyArea(dp,numbers[j],vu->on_label,draw_gc,0,0,wids[j],hgts[j],xs[0],ys[0]); j++;
  XCopyArea(dp,numbers[j],vu->on_label,draw_gc,0,0,wids[j],hgts[j],xs[1],ys[1]); j++;
  XCopyArea(dp,numbers[j],vu->on_label,draw_gc,0,0,wids[j],hgts[j],xs[2],ys[2]); j++;
  XCopyArea(dp,numbers[j],vu->on_label,draw_gc,0,0,wids[j],hgts[j],xs[3],ys[3]); j++;
  XCopyArea(dp,numbers[j],vu->on_label,draw_gc,0,0,wids[j],hgts[j],xs[4],ys[4]); j++;
  
  j=5;
  XCopyArea(dp,numbers[j],vu->off_label,draw_gc,0,0,wids[j],hgts[j],xs[0],ys[0]); j++;
  XCopyArea(dp,numbers[j],vu->off_label,draw_gc,0,0,wids[j],hgts[j],xs[1],ys[1]); j++;
  XCopyArea(dp,numbers[j],vu->off_label,draw_gc,0,0,wids[j],hgts[j],xs[2],ys[2]); j++;
  XCopyArea(dp,numbers[j],vu->off_label,draw_gc,0,0,wids[j],hgts[j],xs[3],ys[3]); j++;
  XCopyArea(dp,numbers[j],vu->off_label,draw_gc,0,0,wids[j],hgts[j],xs[4],ys[4]);

  j=10;
  XCopyArea(dp,numbers[j],vu->clip_label,draw_gc,0,0,wids[j],hgts[j],xs[0],ys[0]); j++;
  XCopyArea(dp,numbers[j],vu->clip_label,draw_gc,0,0,wids[j],hgts[j],xs[1],ys[1]); j++;
  XCopyArea(dp,numbers[j],vu->clip_label,draw_gc,0,0,wids[j],hgts[j],xs[2],ys[2]); j++;
  XCopyArea(dp,numbers[j],vu->clip_label,draw_gc,0,0,wids[j],hgts[j],xs[3],ys[3]); j++;
  XCopyArea(dp,numbers[j],vu->clip_label,draw_gc,0,0,wids[j],hgts[j],xs[4],ys[4]);

  /* draw the arcs */
  xs[0] = (int)(size*(CENTER_X - 120));
  ys[0] = (int)(size*(CENTER_Y - 120));
  xs[1] = (int)(size*(CENTER_X - 119));
  ys[1] = (int)(size*(CENTER_Y - 120));
  xs[2] = (int)(size*(CENTER_X - 119));
  ys[2] = (int)(size*(CENTER_Y - 119));
  xs[3] = (int)(size*(CENTER_X - 116));
  ys[3] = (int)(size*(CENTER_Y - 116));

  XDrawArc(dp,vu->on_label,draw_gc,xs[0],ys[0],(unsigned int)(size*(240)),(unsigned int)(size*(240)),45*64,90*64);
  XDrawArc(dp,vu->on_label,draw_gc,xs[1],ys[1],(unsigned int)(size*(239)),(unsigned int)(size*(239)),45*64,89*64);
  XDrawArc(dp,vu->on_label,draw_gc,xs[2],ys[2],(unsigned int)(size*(239)),(unsigned int)(size*(239)),45*64,89*64);
  XDrawArc(dp,vu->on_label,draw_gc,xs[3],ys[3],(unsigned int)(size*(232)),(unsigned int)(size*(232)),45*64,90*64);

  XDrawArc(dp,vu->off_label,draw_gc,xs[0],ys[0],(unsigned int)(size*(240)),(unsigned int)(size*(240)),45*64,90*64);
  XDrawArc(dp,vu->off_label,draw_gc,xs[1],ys[1],(unsigned int)(size*(239)),(unsigned int)(size*(239)),45*64,89*64);
  XDrawArc(dp,vu->off_label,draw_gc,xs[2],ys[2],(unsigned int)(size*(239)),(unsigned int)(size*(239)),45*64,89*64);
  XDrawArc(dp,vu->off_label,draw_gc,xs[3],ys[3],(unsigned int)(size*(232)),(unsigned int)(size*(232)),45*64,90*64);

  XDrawArc(dp,vu->clip_label,draw_gc,xs[0],ys[0],(unsigned int)(size*(240)),(unsigned int)(size*(240)),45*64,90*64);
  XDrawArc(dp,vu->clip_label,draw_gc,xs[1],ys[1],(unsigned int)(size*(239)),(unsigned int)(size*(239)),45*64,89*64);
  XDrawArc(dp,vu->clip_label,draw_gc,xs[2],ys[2],(unsigned int)(size*(239)),(unsigned int)(size*(239)),45*64,89*64);
  XDrawArc(dp,vu->clip_label,draw_gc,xs[3],ys[3],(unsigned int)(size*(232)),(unsigned int)(size*(232)),45*64,90*64);

  /* draw the axis ticks */
  for (i=0;i<5;i++)
    {
      rdeg = degrees_to_radians(45-i*22.5);
      x0 = (int)(CENTER_X*size+120*size*sin(rdeg));
      y0 = (int)(CENTER_Y*size-120*size*cos(rdeg));
      x1 = (int)(CENTER_X*size+130*size*sin(rdeg));
      y1 = (int)(CENTER_Y*size-130*size*cos(rdeg));
      XDrawLine(dp,vu->on_label,draw_gc,x0,y0,x1,y1);
      XDrawLine(dp,vu->on_label,draw_gc,x0+1,y0,x1+1,y1);
      XDrawLine(dp,vu->off_label,draw_gc,x0,y0,x1,y1);
      XDrawLine(dp,vu->off_label,draw_gc,x0+1,y0,x1+1,y1);
      XDrawLine(dp,vu->clip_label,draw_gc,x0,y0,x1,y1);
      XDrawLine(dp,vu->clip_label,draw_gc,x0+1,y0,x1+1,y1);
      if (i<4)
	{
	  for (j=1;j<6;j++)
	    {
	      rdeg = degrees_to_radians(45-i*22.5-j*(90.0/20.0));
	      x0 = (int)(CENTER_X*size+120*size*sin(rdeg));
	      y0 = (int)(CENTER_Y*size-120*size*cos(rdeg));
	      x1 = (int)(CENTER_X*size+126*size*sin(rdeg));
	      y1 = (int)(CENTER_Y*size-126*size*cos(rdeg));
	      XDrawLine(dp,vu->on_label,draw_gc,x0,y0,x1,y1);
	      XDrawLine(dp,vu->off_label,draw_gc,x0,y0,x1,y1);
	      XDrawLine(dp,vu->clip_label,draw_gc,x0,y0,x1,y1);
	    }
	}
    }
  for (i=0;i<VU_NUMBERS;i++) XFreePixmap(dp,numbers[i]);
}

#if 0
  /* here's how the label xpm files were created */
  /* the data was then transferred to this file and cleaned up by hand */
  XpmWriteFileFromPixmap(dp,"onlabel.xpm",vu->on_label,NULL,NULL);  
  XpmWriteFileFromPixmap(dp,"offlabel.xpm",vu->off_label,NULL,NULL);  
  XpmWriteFileFromPixmap(dp,"cliplabel.xpm",vu->clip_label,NULL,NULL);
#endif

static void allocate_meter(snd_state *ss, vu_label *vu)
{
#if HAVE_XPM  
  int err = XpmSuccess;
  if ((vu->size == 1.0) || (vu->size > 4.0) || (vu->size < .25))
    err = allocate_meter_2(recorder,vu);
  else allocate_meter_1(ss,vu);
  if (err != XpmSuccess) {vu->on_label = 0; vu->off_label = 0; vu->clip_label = 0;}
#else
  allocate_meter_1(ss,vu);
#endif
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
  Pixmap label = 0;
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
    case VU_OFF: label = vu->off_label; break;
    case VU_CLIPPED: 
      vu->clipped--; 
      if (vu->clipped <= 0) 
	{
	  label = vu->on_label; 
	  vu->on_off = VU_ON;
	} 
      else label = vu->clip_label; 
      break;
    case VU_ON: label = vu->on_label; break;
    }
  if (label) XCopyArea(vu->dp,label,vu->wn,vu_gc,0,0,vu->center_x*2,vu->center_y,0,0);
  val = vu->current_val*VU_NEEDLE_SPEED + (vu->last_val*(1.0-VU_NEEDLE_SPEED));
  vu->last_val = val;
  deg = -45.0 + val*90.0;
  /* if (deg < -45.0) deg = -45.0; else if (deg > 45.0) deg = 45.0; */
  rdeg = degrees_to_radians(deg);
  nx0 = vu->center_x - (int)((Float)(vu->center_y - vu->light_y) / tan(degrees_to_radians(deg+90)));
  ny0 = vu->light_y;
  nx1 = (int)(vu->center_x + 130*size*sin(rdeg));
  ny1 = (int)(vu->center_y - 130*size*cos(rdeg));
  XSetForeground(vu->dp,vu_gc,sx->black);
  XDrawLine(vu->dp,vu->wn,vu_gc,nx0,ny0,nx1,ny1);

  /* this shadow doesn't do much (and if +/-3 for depth, it looks kinda dumb) */
  if (deg != 0.0)
    {
      if (vu->on_off == VU_OFF)
	XSetForeground(vu->dp,vu_gc,sx->position_color);
      else
	if (vu->on_off == VU_CLIPPED)
	  XSetForeground(vu->dp,vu_gc,sx->pushed_button_color);
	else XSetForeground(vu->dp,vu_gc,sx->white);
      if (deg < 0.0)
	XDrawLine(vu->dp,vu->wn,vu_gc,nx0-1,ny0,nx1-1,ny1);
      else XDrawLine(vu->dp,vu->wn,vu_gc,nx0+1,ny0,nx1+1,ny1);
      XSetForeground(vu->dp,vu_gc,sx->black);
    }

  if (vu->on_off != VU_OFF)
    {
      if (vu->current_val > vu->red_deg) 
	vu->red_deg = vu->current_val;
      else vu->red_deg = vu->current_val*VU_BUBBLE_SPEED + (vu->red_deg*(1.0 - VU_BUBBLE_SPEED));
      XSetForeground(vu->dp,vu_gc,sx->red);
      redx = (int)(vu->red_deg * 90*64);
      if (redx<(VU_BUBBLE_SIZE)) redy=redx; else redy=VU_BUBBLE_SIZE;
      bub0 = (int)(size*117);
      bub1 = (int)(size*119);
      for (i=bub0,j=bub0*2;i<=bub1;i++,j+=(int)(2*size))
	XDrawArc(vu->dp,vu->wn,vu_gc,vu->center_x - i,vu->center_y - i,j,j,135*64 - redx,redy);
      XSetForeground(vu->dp,vu_gc,sx->black);
    }
}

static void Meter_Display_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  display_vu_meter((VU *)clientData);
}

static VU *make_vu_meter(Widget meter, int light_x, int light_y, int center_x, int center_y, Pixel needle_color, snd_state *ss, Float size)
{
  VU *vu;
  vu_label *vl;
  vu = (VU *)CALLOC(1,sizeof(VU));
  vu->meter = meter;
  vu->size = size;
  vu->dp = XtDisplay(meter);
  vu->wn = XtWindow(meter);
  vu->scr = DefaultScreen(vu->dp);
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

#define RECORD_SCROLLBAR_MAX 300
#define RECORD_SCROLLBAR_MID 150
#define RECORD_SCROLLBAR_LINEAR_MAX 50
#define RECORD_SCROLLBAR_LINEAR_MULT 0.00365368
#define RECORD_SCROLLBAR_CHANGEOVER_VALUE 0.188
/* final low end portion is linear -- multiplier is (exp-value-at-linear-max) / linear_max */

static void record_amp_changed(AMP *ap, int val)
{
  char *sfs;
  snd_state *ss;
  Float amp;
  ss = ap->ss;
  if (val == 0) 
    amp = 0.0;
  else 
    {
      if (val < RECORD_SCROLLBAR_LINEAR_MAX)
	amp = (Float)val * RECORD_SCROLLBAR_LINEAR_MULT;
      else amp = exp((Float)(val-RECORD_SCROLLBAR_MID)/((Float)RECORD_SCROLLBAR_MAX*.2));
    }
  sfs=prettyf(amp,2);
  fill_number(sfs,amp_number_buffer);
  set_button_label(ap->number,amp_number_buffer);
  FREE(sfs);
  if (ap->type == INPUT_AMP)
    rec_in_amps[ap->in][ap->out] = amp;
  else rec_out_amps[ap->out] = amp;
}

static int amp_to_slider(Float val)
{
  /* reverse calc above */
  if (val <= 0.0) 
    return(0);
  else
    {
      if (val <= RECORD_SCROLLBAR_CHANGEOVER_VALUE)
	return((int)(val / RECORD_SCROLLBAR_LINEAR_MULT));
      else return((int)(RECORD_SCROLLBAR_MID + ((RECORD_SCROLLBAR_MAX * .2) * log(val))));
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

static void Record_Amp_Click_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)callData;
  AMP *ap = (AMP *)clientData;
  XButtonEvent *ev;
  int val;
  ev = (XButtonEvent *)(cb->event);
  if (ev->state & (snd_ControlMask | snd_MetaMask)) val = ap->last_amp; else val = RECORD_SCROLLBAR_MID;
  record_amp_changed(ap,val);
  XtVaSetValues(ap->slider,XmNvalue,val,NULL);
}

static void Record_Amp_Drag_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  record_amp_changed((AMP *)clientData,((XmScrollBarCallbackStruct *)callData)->value);
}

static void Record_Amp_ValueChanged_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)callData;
  AMP *ap = (AMP *)clientData;
  ap->last_amp = cb->value;
  record_amp_changed(ap,cb->value);
}



/* ---------------- MESSAGE PANE ---------------- */

static void Messages_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  snd_help(ss,
	   "Message Pane",
"This pane contains any messages the recorder\n\
generates.  Each is time stamped.  Many are merely\n\
informational or mild gripes.\n\
");
}

static Widget make_message_pane(snd_state *ss, Widget message_pane)
{
  int n;
  Arg args[32];
  Widget msg;
  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNeditMode,XmMULTI_LINE_EDIT); n++;
  XtSetArg(args[n],XmNscrollBarDisplayPolicy,XmAS_NEEDED); n++;
  XtSetArg(args[n],XmNeditable,FALSE); n++;
  XtSetArg(args[n],XmNautoShowCursorPosition,FALSE); n++;
  XtSetArg(args[n],XmNcursorPositionVisible,FALSE); n++;
  XtSetArg(args[n],XmNscrollingPolicy,XmAUTOMATIC); n++;
  msg = XmCreateScrolledText(message_pane,"scrolled-text",args,n);
  XtManageChild(msg);
  if (!(ss->using_schemes)) 
    {
      map_over_children(XtParent(msg),set_main_color_of_widget,(void *)ss);
      XtVaSetValues(msg,XmNbackground,(ss->sgx)->light_blue,XmNforeground,(ss->sgx)->black,NULL);
    }
  XtAddCallback(msg,XmNhelpCallback,Messages_Help_Callback,ss);
  return(msg);
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
    case MUS_AUDIO_DAC_OUT:     return(STR_Output); break; /* default here means that linuxppc reports "Output" as analog-in pane name */
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
    case MUS_AUDIO_CD:          return("CD"); break;
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
    case MUS_AUDIO_AUX_OUTPUT:
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


static void file_label_help_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  snd_help(ss,
	   "Output File Name",
"This field sets the name of the output file.\n\
");	   
}

static void duration_label_help_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  snd_help(ss,
	   "Output File Duration",
"This field shows the duration of the current\n\
or previous recording.\n\
");	   
}

static void button_holder_help_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  snd_help(ss,
	   "Record Options",
"These buttons configure the appearance of the recorder;\n\
'Autoload Recording', if set, causes the recorded output to be\n\
loaded automatically into Snd.\n\
");
}

static void autoload_file_help_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  snd_help(ss,
       "Autoload File",
"If this button is set, the recorded file\n\
is automatically loaded into Snd.\n\
");	   
}

static void Help_Record_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  record_dialog_help((snd_state *)clientData);
}

static void VU_Reset_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  snd_help(ss,
	   "Reset Button",
"This button resets the fields above it that\n\
indicate the on-going max amp encountered\n\
since the last reset\n\
");
}

static void trigger_help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  snd_help(ss,
	   "Record Trigger",
"This scale sets the auto-record trigger\n\
value.  If it is non-zero, when you push the\n\
'Triggered Record' button, Snd waits until it\n\
receives data above that value before starting\n\
the actual recording.\n\
");
}

static void make_trigger_label(Float val)
{
  XmString s1;
  char *buf;
  if (trigger_label)
    {
      buf = (char *)CALLOC(32,sizeof(char));
      sprintf(buf,"%s %.3f",STR_trigger_p,val);
      s1 = XmStringCreate(buf,XmFONTLIST_DEFAULT_TAG);
      FREE(buf);
      XtVaSetValues(trigger_label,XmNlabelString,s1,NULL);
      XmStringFree(s1);
    }
}

static void internal_trigger_set(Float val)
{
  XmString s1;
  trigger = val;
  triggering = (val > 0.0);
  triggered = (!triggering);
  make_trigger_label(val);
  if (!recording) /* else wait for current session to end (via click) */
    {
      s1 = XmStringCreate((triggering) ? STR_Triggered_Record : STR_Record,XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(record_button,XmNlabelString,s1,NULL);
      XmStringFree(s1);
    }
}

static void change_trigger_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  /* if val=0, set record button normal label to 'Record', else 'Triggered Record' */
  XmScaleCallbackStruct *cb = (XmScaleCallbackStruct *)callData;
  in_set_recorder_trigger(ss,(Float)(cb->value)/100.0);
  internal_trigger_set((Float)(cb->value)/100.0);
}

static void drag_trigger_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  XmScaleCallbackStruct *cb = (XmScaleCallbackStruct *)callData;
  make_trigger_label((Float)(cb->value)/100.0);
}

static void device_button_callback(Widget w,XtPointer clientData,XtPointer callData) 
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

  XtVaGetValues(w,XmNuserData,&button,NULL);
  on = XmToggleButtonGetState(w);
  if (on)
    XtVaSetValues(p->pane,XmNpaneMinimum,p->pane_size,NULL);
  else 
    {
      XtVaGetValues(p->pane,XmNheight,&(p->pane_size),NULL);
      XtVaSetValues(p->pane,XmNpaneMaximum,1,NULL);
    }
  XtVaSetValues(p->pane,XmNpaneMinimum,1,XmNpaneMaximum,LOTSA_PIXELS,NULL);

#if OLD_SGI_AL
  /* on the older SGI's (and maybe newer Indy's?) digital input disables mic/line-in and vice versa */
  if (button == digital_in_button)
    {
      set_line_source(p->ss,on);
      if (on == (XmToggleButtonGetState(device_buttons[microphone_button])))
	XmToggleButtonSetState(device_buttons[microphone_button],!on,TRUE); 
      if (on == (XmToggleButtonGetState(device_buttons[line_in_button])))
	XmToggleButtonSetState(device_buttons[line_in_button],!on,TRUE); 
    }
  else
    {
      if (button == microphone_button)
	{
	  if (on == (XmToggleButtonGetState(device_buttons[digital_in_button])))
	    {
	      XmToggleButtonSetState(device_buttons[digital_in_button],!on,TRUE); 
	      if (!(on == (XmToggleButtonGetState(device_buttons[line_in_button]))))
		XmToggleButtonSetState(device_buttons[line_in_button],on,TRUE); 
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
		  /* XtVaGetValues(p->pane,XmNheight,&(p->pane_size),NULL); */
		  XtVaSetValues(p->pane,XmNpaneMaximum,1,NULL);
		  XtVaSetValues(p->pane,XmNpaneMinimum,1,XmNpaneMaximum,LOTSA_PIXELS,NULL);
		  XmToggleButtonSetState(device_buttons[active_device_button],FALSE,FALSE); 
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
	    record_report(messages,Device_Name(p->device),": ",mus_audio_error_name(mus_audio_error()),NULL);
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
		      record_report(messages,"open output: ",mus_audio_error_name(mus_audio_error()),NULL);
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

static void autoload_file_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  in_set_recorder_autoload(ss,XmToggleButtonGetState(w));
}

#if (HAVE_OSS || HAVE_ALSA)
static void save_audio_settings_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  XmToggleButtonSetState(w,FALSE,FALSE);
  settings_saved = 1;
  mus_audio_mixer_save(AUDIO_STATE_FILE);
}

static void save_audio_settings_help_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  snd_help(ss,
	   "Save Audio Settings",
	   "Normally, Snd saves the state of the audio hardware\n\
(the 'mixer' in Linux jargon) before opening the\n\
recorder window, then restores it upon closing that\n\
window.  This means that any changes you make via\n\
the sliders will be erased upon exit.  To save the\n\
current state, press this button.  The mixer state\n\
will be written to the file .snd-mixer which can be\n\
deleted to cancel the save.  This file will also be\n\
read by CLM, someday.\n\
");	   
}
#endif

static void Srate_Changed_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  char *str;
  int n;
  snd_state *ss = (snd_state *)clientData;
  str = XmTextGetString(w); 
  if (str) 
    {
      n = string2int(str);
      if ((n>0) && (n != recorder_srate(ss)))
	{
	  in_set_recorder_srate(ss,n);
	  set_audio_srate(ss,MUS_AUDIO_DEFAULT,recorder_srate(ss),0);
	}
      XtFree(str);
    }
}

static void set_record_size (snd_state *ss, int new_size);

static void Rec_Size_Changed_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  char *str;
  int n;
  str = XmTextGetString(w); 
  if (str) 
    {
      n = string2int(str);
      if ((n>0) && (n != recorder_buffer_size(ss))) set_record_size(ss,n);
      XtFree(str);
    }
}

static void rec_size_help_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  snd_help(ss,
	   "Record Input Buffer Size",
"This field sets the size of the recorder's input\n\
buffers.  If you are getting clicks, and have already\n\
tried everything else (you're writing to a local disk,\n\
in the host's native data format), try goofing around\n\
with this number.\n\
");
}

static int make_file_info_pane(snd_state *ss, Widget file_pane, int *ordered_devices, int ndevs, int *ordered_systems)
{
  int i,n,init_n;
  Position pane_max;
  Arg args[32];
  char *name;
  Widget file_label,file_form,button_frame,button_holder,duration_label,rec_size_label,
    ff_form,ff_sep1,ff_sep2,ff_sep3,ff_sep4,autoload_file;
#if (HAVE_OSS || HAVE_ALSA)
  Widget save_audio_settings;
#endif
#if defined(SGI) || defined(SUN)
  float val[1];
  int err;
#endif

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  file_form = sndCreateFormWidget("file-data",file_pane,args,n);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->black); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
  XtSetArg(args[n],XmNseparatorType,XmDOUBLE_LINE); n++;
  XtSetArg(args[n],XmNheight,15); n++;
  ff_sep4 = XtCreateManagedWidget("ff-sep4",xmSeparatorWidgetClass,file_form,args,n);
      
  /* file data */
  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,ff_sep4); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  ff_form = sndCreateFormWidget("ff-form",file_form,args,n);

  n=0;
  /* if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->red); n++;} */
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  /* XtSetArg(args[n],XmNheight,34); n++; */
  XtSetArg(args[n],XmNmarginTop,8); n++;
  XtSetArg(args[n],XmNmarginBottom,8); n++;
  file_label = XtCreateManagedWidget(STR_file_p,xmLabelWidgetClass,ff_form,args,n);
  XtAddCallback(file_label,XmNhelpCallback,file_label_help_callback,ss);

  n=0;
  /* if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;} */
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_OPPOSITE_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,file_label); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,file_label); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  if (snd_strlen(recorder_file(ss)) > 0) {XtSetArg(args[n],XmNvalue,recorder_file(ss)); n++;}
  file_text = sndCreateTextFieldWidget(ss,"text",ff_form,args,n,NOT_ACTIVATABLE,NO_COMPLETER);
  XtAddCallback(file_text,XmNhelpCallback,file_label_help_callback,ss);

  n=0;
  /* if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;} */
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,file_text); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
  XtSetArg(args[n],XmNheight,10); n++;
  ff_sep3 = XtCreateManagedWidget("ff-sep3",xmSeparatorWidgetClass,ff_form,args,n);      

  n=0;
  /* if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;} */
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,ff_sep3); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  recdat = sndCreateFileDataForm(ss,ff_form,"data-form",args,n,TRUE,out_type,recorder_out_format(ss),FALSE);
  XtVaGetValues(recdat->comment_text,XmNy,&pane_max,NULL);
  XtAddCallback(recdat->srate_text,XmNactivateCallback,Srate_Changed_Callback,(void *)ss);
#if defined(SGI)
  err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_MICROPHONE,MUS_AUDIO_SRATE,0,val);
  if (!err) in_set_recorder_srate(ss,val[0]);
#endif
  sprintf(timbuf,"%d",recorder_srate(ss));
  XmTextSetString(recdat->srate_text,timbuf);
  sprintf(timbuf,"%d",recorder_out_chans(ss));
  XmTextSetString(recdat->chans_text,timbuf);
  if (!(ss->using_schemes))
    {
      map_over_children(ff_form,set_main_color_of_widget,ss);
      XtVaSetValues(recdat->header_list,XmNbackground,(ss->sgx)->white,XmNforeground,(ss->sgx)->black,NULL);
      XtVaSetValues(recdat->format_list,XmNbackground,(ss->sgx)->white,XmNforeground,(ss->sgx)->black,NULL);
      XtVaSetValues(file_label,XmNbackground,(ss->sgx)->highlight_color,NULL);
    }

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,ff_sep4); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,ff_form); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
  XtSetArg(args[n],XmNwidth,15); n++;
  ff_sep1 = XtCreateManagedWidget("ff-sep1",xmSeparatorWidgetClass,file_form,args,n);      

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,ff_sep4); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,ff_sep1); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNmarginTop,8); n++;
  XtSetArg(args[n],XmNmarginBottom,8); n++;
  duration_label = XtCreateManagedWidget(STR_duration_p,xmLabelWidgetClass,file_form,args,n);
  XtAddCallback(duration_label,XmNhelpCallback,duration_label_help_callback,ss);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,ff_sep4); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrecomputeSize,FALSE); n++;
  XtSetArg(args[n],XmNcolumns,6); n++;
  rec_size_text = sndCreateTextFieldWidget(ss,"rectext",file_form,args,n,NOT_ACTIVATABLE,NO_COMPLETER);
  XtAddCallback(rec_size_text,XmNhelpCallback,rec_size_help_callback,ss);
  XtAddCallback(rec_size_text,XmNactivateCallback,Rec_Size_Changed_Callback,(void *)ss);
  sprintf(timbuf,"%d",recorder_buffer_size(ss));
  XmTextSetString(rec_size_text,timbuf);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,ff_sep4); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNrightWidget,rec_size_text); n++;
  XtSetArg(args[n],XmNmarginTop,8); n++;
  XtSetArg(args[n],XmNmarginBottom,8); n++;
  rec_size_label = XtCreateManagedWidget("buf:",xmLabelWidgetClass,file_form,args,n);
  XtAddCallback(rec_size_label,XmNhelpCallback,rec_size_help_callback,ss);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->light_blue); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,ff_sep4); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,duration_label); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNrightWidget,rec_size_label); n++;
  XtSetArg(args[n],XmNrecomputeSize,FALSE); n++;
  XtSetArg(args[n],XmNmarginTop,8); n++;
  XtSetArg(args[n],XmNmarginBottom,8); n++;
  file_duration = XtCreateManagedWidget("  0.0 ",xmLabelWidgetClass,file_form,args,n);
  XtAddCallback(file_duration,XmNhelpCallback,duration_label_help_callback,ss);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,rec_size_text); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,ff_sep1); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
  XtSetArg(args[n],XmNheight,10); n++;
  ff_sep2 = XtCreateManagedWidget("ff-sep2",xmSeparatorWidgetClass,file_form,args,n);      


  /* Auto-trigger scale */
  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,ff_sep1); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  trigger_label = XtCreateManagedWidget(STR_trigger_p,xmLabelWidgetClass,file_form,args,n);
  XtAddCallback(trigger_label,XmNhelpCallback,trigger_help_Callback,ss);
  make_trigger_label(recorder_trigger(ss));

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,trigger_label); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_OPPOSITE_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,trigger_label); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNvalue,(int)(100*recorder_trigger(ss))); n++;
  XtSetArg(args[n],XmNdragCallback,make_callback_list(drag_trigger_Callback,(XtPointer)ss)); n++;
  XtSetArg(args[n],XmNvalueChangedCallback,make_callback_list(change_trigger_Callback,(XtPointer)ss)); n++;
  trigger_scale = XtCreateManagedWidget("scale",xmScaleWidgetClass,file_form,args,n);
  XtAddCallback(trigger_scale,XmNhelpCallback,trigger_help_Callback,ss);

  /* buttons */
  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->zoom_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,ff_sep2); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNbottomWidget,trigger_scale); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,ff_sep1); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNshadowThickness,4); n++;
  button_frame = sndCreateFrameWidget("button-frame",file_form,args,n);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
  XtSetArg(args[n],XmNspacing,0); n++;
  button_holder = sndCreateRowColumnWidget("button-holder",button_frame,args,n);
  XtAddCallback(button_holder,XmNhelpCallback,button_holder_help_callback,ss);

  /* load up the box of panel on-buttons and various other settings (autoload for now) */
  n=0;
  if (!(ss->using_schemes)) 
    {
      XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;
      XtSetArg(args[n],XmNselectColor,(ss->sgx)->red); n++;
    }
  init_n = n;
  for (i=0;i<ndevs;i++)
    {
      n = init_n;
      XtSetArg(args[n],XmNuserData,i); n++;
#if OLD_SGI_AL
      if ((ordered_devices[i] == MUS_AUDIO_DIGITAL_IN) || (ordered_devices[i] == MUS_AUDIO_MICROPHONE))
	{XtSetArg(args[n],XmNindicatorType,XmONE_OF_MANY); n++;}
      else {XtSetArg(args[n],XmNindicatorType,XmN_OF_MANY); n++;}
#endif
#if NEW_SGI_AL || defined(SUN)
      if (input_device(ordered_devices[i]))
	{XtSetArg(args[n],XmNindicatorType,XmONE_OF_MANY); n++;}
      else {XtSetArg(args[n],XmNindicatorType,XmN_OF_MANY); n++;}
#endif
      if ((systems == 1) || (!(input_device(ordered_devices[i]))))
	name = Device_Name(ordered_devices[i]);
      else name = System_and_Device_Name(ordered_systems[i],ordered_devices[i]);
      device_buttons[i] = sndCreateToggleButtonWidget(name,button_holder,args,n);
      XtAddCallback(device_buttons[i],XmNhelpCallback,button_holder_help_callback,ss);
      XtAddCallback(device_buttons[i],XmNvalueChangedCallback,device_button_callback,(XtPointer)all_panes[i]);
      XmToggleButtonSetState(device_buttons[i],TRUE,FALSE); 
    }
  autoload_file = sndCreateToggleButtonWidget(STR_Autoload_Recording,button_holder,args,init_n);
  device_buttons[ndevs] = autoload_file;
  /* we assume this is last in the device_buttons list in sensitize_control_buttons */
  autoload_button = ndevs;
  XtAddCallback(autoload_file,XmNhelpCallback,autoload_file_help_callback,ss);
  XtAddCallback(autoload_file,XmNvalueChangedCallback,autoload_file_callback,ss);
  XmToggleButtonSetState(autoload_file,recorder_autoload(ss),FALSE); 
#if (HAVE_OSS || HAVE_ALSA)
  save_audio_settings = sndCreateToggleButtonWidget(STR_Save_Audio_Settings,button_holder,args,n);
  XtAddCallback(save_audio_settings,XmNvalueChangedCallback,save_audio_settings_callback,NULL);
  XtAddCallback(save_audio_settings,XmNhelpCallback,save_audio_settings_help_callback,ss);
#endif

  return(pane_max + 50);
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
  if ((recorder) && (XtIsManaged(recorder)))
    {
      close_recorder_audio();
      set_sensitive(record_button,FALSE);
      set_sensitive(reset_button,FALSE);
    }
}

void unlock_recording_audio(void)
{
  XmString s1;
  if ((recorder) && (XtIsManaged(recorder)))
    {
      set_sensitive(record_button,TRUE);
      set_sensitive(reset_button,TRUE);
      XmProcessTraversal(reset_button,XmTRAVERSE_CURRENT);
      s1 = XmStringCreate(STR_Restart,XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(reset_button,XmNlabelString,s1,NULL);
      XmStringFree(s1);
    }
}


/* -------------------------------- DEVICE PANE -------------------------------- */


static char *device_name(PANE *p)
{
  /* informal aw shucks reference in help window */
  switch (p->device)
    {
    case MUS_AUDIO_DIGITAL_OUT:
    case MUS_AUDIO_LINE_OUT:
    case MUS_AUDIO_DEFAULT:
    case MUS_AUDIO_DAC_OUT:
    case MUS_AUDIO_DUPLEX_DEFAULT: return("the output");                break;
    case MUS_AUDIO_SPEAKERS:   return("the speakers");                  break;
    case MUS_AUDIO_ADAT_OUT: 
    case MUS_AUDIO_ADAT_IN:    return("the Adat");                      break;
    case MUS_AUDIO_AES_OUT: 
    case MUS_AUDIO_AES_IN:     return("the Aes");                       break;
    case MUS_AUDIO_SPDIF_IN:
    case MUS_AUDIO_SPDIF_OUT:  return("the S/PDIF");                    break;
    case MUS_AUDIO_LINE_IN:    return("line in");                       break;
    case MUS_AUDIO_MICROPHONE: return("the microphone");                break;
    case MUS_AUDIO_DIGITAL_IN: return("digital in");                    break;
    case MUS_AUDIO_DAC_FILTER: return("the analog tone control");       break;
    case MUS_AUDIO_MIXER:      return("various analog volume controls");break;
    case MUS_AUDIO_CD:         return("the internal CD");               break;
    default:                   return("the input");                     break;
    }
}

#if (!(HAVE_OSS || HAVE_ALSA))
static char *channel_function(PANE *p)
{
  if (input_device(p->device)) return("gain"); else return("volume");
}

static char funbuf[16];
static char *device_function(PANE *p)
{
  sprintf(funbuf,"%s %s",Device_Name(p->device),channel_function(p));
  return(funbuf);
}
#endif

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

static void VU_Max_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  Wdesc *wd = (Wdesc *)clientData;
  ssnd_help(wd->ss,
	   "Max Amp Indicator",
	    "This number indicates the max amp encountered\n\
since the last reset in ",
	    device_name(wd->p),"'s",
	    " channel ",
	    channel_name(wd->p,wd->chan),
	    ".\n\
The reset button sets it back to 0.0.\n",
	    NULL);
}

static void VU_On_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  Wdesc *wd = (Wdesc *)clientData;
  ssnd_help(wd->ss,
	    "On/Off Button",
	    "This button causes ",
	    device_name(wd->p),"'s",
	    " channel ",
	    channel_name(wd->p,wd->chan),
	    " to be included\n\
in or removed from the recording. The button\n\
is red when the signal is active.\n",
	    NULL);
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

static char *field_name(int fld)
{
  switch (fld)
    {
    case MUS_AUDIO_IMIX:   return("imix"); break;
    case MUS_AUDIO_IGAIN:  return("igain"); break;
    case MUS_AUDIO_RECLEV: return("reclev"); break;
    case MUS_AUDIO_PCM:    return("pcm"); break;
    case MUS_AUDIO_PCM2:   return("pcm2"); break;
    case MUS_AUDIO_OGAIN:  return("ogain"); break;
    case MUS_AUDIO_LINE:   return("line-in"); break;
    case MUS_AUDIO_MICROPHONE:    return("mic"); break;
    case MUS_AUDIO_LINE1:  return("line1"); break;
    case MUS_AUDIO_LINE2:  return("line2"); break; 
    case MUS_AUDIO_LINE3:  return("line3"); break;
    case MUS_AUDIO_SYNTH:  return("synth"); break;
    case MUS_AUDIO_CD:     return("cd"); break;
    default: return("?"); break;
    }
}

static char *field_function(int fld)
{
  switch (fld)
    {
    case MUS_AUDIO_IMIX:   return("the pre-adc mix of mic and line-in"); break;
    case MUS_AUDIO_IGAIN:  return("input gain"); break;
    case MUS_AUDIO_RECLEV: return("recording level"); break;
    case MUS_AUDIO_PCM:    return("the speaker level, perhaps"); break;
    case MUS_AUDIO_PCM2:   return("nothing in particular"); break;
    case MUS_AUDIO_OGAIN:  return("output gain"); break;
    case MUS_AUDIO_LINE:   return("analog line-in"); break;
    case MUS_AUDIO_MICROPHONE:    return("the microphone"); break;
    case MUS_AUDIO_LINE1:  
    case MUS_AUDIO_LINE2:  
    case MUS_AUDIO_LINE3:  return("extra line inputs"); break;
    case MUS_AUDIO_SYNTH:  return("the on-card synthesizer, if any"); break;
    case MUS_AUDIO_CD:     return("the cd gain"); break;
    default: return("?"); break;
    }
}
#endif

static void volume_help_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  Wdesc *wd = (Wdesc *)clientData;
#if (HAVE_OSS || HAVE_ALSA)
  int device,channel,field;
  device = wd->device;
  field = wd->field;
  channel = wd->chan;
  if (device == MUS_AUDIO_DAC_OUT)
    ssnd_help(wd->ss,
	      "Analog Out",
	      "This slider scales channel ",
	      out_channel_name(wd->ss,channel),
	      "'s volume\n\
at the speaker.  It has no effect on the output\n\
file's amplitude.\n",
	      NULL);
  else
    {
      if (device == MUS_AUDIO_DAC_FILTER)
	ssnd_help(wd->ss,
		  "Tone Controls",
		  "This slider sets the mixer's ",
		  (field == MUS_AUDIO_TREBLE) ? "treble" : "bass",
		  " tone control.\n",
		  NULL);
      else
	{
	  if (device == MUS_AUDIO_MICROPHONE)
	    ssnd_help(wd->ss,
		      "Microphone Gain",
"This slider controls the input gain of the microphone.\n",
		      NULL);
	  else
	    {
	      if (device == MUS_AUDIO_LINE_IN)
		ssnd_help(wd->ss,
			  "Line In Gain",
"This slider controls the volume of the analog\n\
line-in channel ",
			  channel_name(wd->p,channel),
			  ".\n",
			  NULL);
	      else
		ssnd_help(wd->ss,
			  "Linux Mixer Settings",
"This bank of sliders affects the OSS 'mixer'; this\n\
particular slider claims to control channel ",
			  channel_name(wd->p,channel),
			  "'s\n",
			  field_name(field),
" field, which I believe has something to do with\n",
			  field_function(field),
			  ".\n",
			  NULL);
	    }
	}
    }
#else
  ssnd_help(wd->ss,
	    device_function(wd->p),
	    "This slider sets the ",
	    channel_function(wd->p),
	    " of channel ",
	    channel_name(wd->p,wd->chan),
	    " of ",
	    device_name(wd->p),
	    "\n",
	    (input_device(((PANE *)(wd->p))->device)) ? "This scales the in-coming signal before it\n\
is reflected in the meters or the audio data.\n" : "",
	    NULL);
#endif
}

static void amp_slider_help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  Wdesc *wd = (Wdesc *)clientData;
  PANE *p;
  AMP *a;
  p = wd->p;
  a = p->amps[wd->chan];
  ssnd_help(wd->ss,
	    "Volume Slider",
	    "This slider scales ",
	    (input_device(((PANE *)(wd->p))->device)) ? "the contribution of " : "",
	    device_name(wd->p),"'s\n",
	    "channel ",
	    channel_name(wd->p,a->in),
	    (input_device(((PANE *)(wd->p))->device)) ? " to the output file's channel " : ".",
	    (input_device(((PANE *)(wd->p))->device)) ? (out_channel_name(wd->ss,a->out)) : "",
	    NULL);
}

static void Meter_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  Wdesc *wd = (Wdesc *)clientData;
  ssnd_help(wd->ss,
	    "VU Meter",
	    "This meter shows the current volume of ",
	    device_name(wd->p),"'s",
	    " channel ",
	    channel_name(wd->p,wd->chan),
	    ".\n\
It is yellow if active, red if clipping.\n",
	    NULL);
}

static void VU_Reset_Callback(Widget w,XtPointer clientData,XtPointer callData) 
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

static void Meter_Button_Callback(Widget w,XtPointer clientData,XtPointer callData) 
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
      XmChangeColor(w,(Pixel)(ss->sgx)->red);
      vu->on_off = VU_ON;
      vu->red_deg = 0.0;
    }
  else 
    {
      XmChangeColor(w,(Pixel)(ss->sgx)->basic_color);
      vu->on_off = VU_OFF;
    }
  display_vu_meter(vu);
  val = (vu->on_off == VU_ON);
  p->active[wd->chan] = val;
  if (output_device(p->device))
    {
      rec_out_active[wd->chan] = val;
      str = XmTextGetString(recdat->chans_text); 
      if (str) 
	{
	  n = string2int(str);
	  XtFree(str);
	}
      else n=0;
      val = 0;
      for (i=0;i<p->active_size;i++) {if (p->active[i]) val++;}
      if ((val>0) && (val != n))
	{
	  sprintf(timbuf,"%d",val);
	  XmTextSetString(recdat->chans_text,timbuf);
#if (HAVE_ALSA || HAVE_OSS)
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

static void volume_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  Wdesc *wd = (Wdesc *)clientData;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)callData;
  set_audio_gain(wd,(Float)cbs->value/100.0);
}

/* ---- slider button matrix ---- */

static void Button_Matrix_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  snd_help(ss,"Channel Matrix",
"This set of buttons controls which amplitude sliders\n\
are visible in the pane.  To conserve screen space, only\n\
the most commonly used sliders are visible by default, but\n\
you can press the button corresponding to the slider you want\n\
at any time.  It is also possible to drag the mouse through\n\
a row or column, setting (button 1) or unsetting (button 2)\n\
buttons as you go.  Click in the corner (the '/' label) to\n\
set all (button 1) or unset all (button 2). Similarly, click\n\
above a column or to the left of a row to set (button 1) or\n\
unset (button 2) the entire column or row.  Control-click in\n\
the corner to return to the default settings.\n\
");
}

static Widget sndCreateRecorderSlider(snd_state *ss, PANE *p, AMP *a, Widget last_slider, int input)
{
  int n;
  Arg args[32];
  XmString s1;

  n=0;      
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;	
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,last_slider); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNmarginHeight,CONTROLS_MARGIN); n++;
  XtSetArg(args[n],XmNrecomputeSize,FALSE); n++;
  XtSetArg(args[n],XmNshadowThickness,0); n++;
  XtSetArg(args[n],XmNhighlightThickness,0); n++;
  XtSetArg(args[n],XmNfillOnArm,FALSE); n++;
#if 0
  /* smaller (in height) sliders need font changes here and below */
  if (amp_sliders>4)
    {
      XtSetArg(args[n],XM_FONT_RESOURCE,BUTTON_FONT(ss)); n++;
    }
  s1=XmStringCreate("     ","button_font");
  XtSetArg(args[n],XmNlabelString,s1); n++;
#endif
  a->label = sndCreatePushButtonWidget(gain_channel_name(p,input,a->device_in_chan,a->out),p->pane,args,n);
  XtAddCallback(a->label,XmNactivateCallback,Record_Amp_Click_Callback,a);
  XtAddCallback(a->label,XmNhelpCallback,amp_slider_help_Callback,a->wd);
  
  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
#if 0
  if (amp_sliders>4)
    {
      s1=XmStringCreate(amp_to_string(global_amp(a)),"button_font");
      XtSetArg(args[n],XM_FONT_RESOURCE,BUTTON_FONT(state)); n++;
    }
#endif
  s1=XmStringCreate(amp_to_string(global_amp(a)),XmFONTLIST_DEFAULT_TAG);
  XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;	
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_OPPOSITE_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,a->label); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,a->label); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNlabelString,s1); n++;
  XtSetArg(args[n],XmNmarginHeight,CONTROLS_MARGIN); n++;
  XtSetArg(args[n],XmNrecomputeSize,FALSE); n++;
  XtSetArg(args[n],XmNshadowThickness,0); n++;
  XtSetArg(args[n],XmNhighlightThickness,0); n++;
  XtSetArg(args[n],XmNfillOnArm,FALSE); n++;
  a->number = sndCreatePushButtonWidget ("amp-number",p->pane,args,n);
  /* this could be the snd-xsnd control panel case as well */
  XtAddCallback(a->number,XmNactivateCallback,Record_Amp_Click_Callback,a);
  XtAddCallback(a->number,XmNhelpCallback,amp_slider_help_Callback,a->wd);
  XmStringFree(s1);
	  
  n=0;      
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->position_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_OPPOSITE_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,a->number); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
#if 0
  XtSetArg(args[n],XmNheight,(amp_sliders>4) ? 12 : 16); n++;
#endif
  XtSetArg(args[n],XmNheight,16); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,a->number); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNrightWidget,p->button_vertical_sep); n++;
  XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
  XtSetArg(args[n],XmNmaximum,RECORD_SCROLLBAR_MAX); n++;
  XtSetArg(args[n],XmNvalue,amp_to_slider(global_amp(a))); n++;
  XtSetArg(args[n],XmNdragCallback,make_callback_list(Record_Amp_Drag_Callback,(XtPointer)a)); n++;
  XtSetArg(args[n],XmNvalueChangedCallback,make_callback_list(Record_Amp_ValueChanged_Callback,(XtPointer)a)); n++;
  a->slider = XtCreateManagedWidget("amp",xmScrollBarWidgetClass,p->pane,args,n);
  XtAddCallback(a->slider,XmNhelpCallback,amp_slider_help_Callback,a->wd);
  return(a->slider);
}

static void handle_matrix_slider(Widget mb, PANE *p, int bin, int bout, int curamp, int remove)
{
  AMP *a;
  snd_state *ss;
  Widget new_top;
  int i;
  a = p->amps[curamp];
  ss = p->ss;

  if (remove)
    {
      XmChangeColor(mb,(Pixel)((ss->sgx)->basic_color));
      p->active_sliders[bin][bout] = 0;
      XtUnmanageChild(a->label);
      XtUnmanageChild(a->number);
      XtUnmanageChild(a->slider);
      new_top = a->top;
    }
  else
    {
      XmChangeColor(mb,(Pixel)((ss->sgx)->green));
      p->active_sliders[bin][bout] = 1;
      if (a->label)
	{
	  XtVaSetValues(a->label,XmNtopWidget,a->top,NULL);
	  XtManageChild(a->label);
	  XtManageChild(a->number);
	  XtManageChild(a->slider);
	  new_top = a->slider;
	}
      else
	{
	  new_top = sndCreateRecorderSlider(ss,p,a,a->top,TRUE);
	}
    }
  for (i=curamp+1;i<p->amps_size;i++)
    {
      a = p->amps[i];
      if (a)
	{
	  a->top = new_top;
	  if ((a->label) && (XtIsManaged(a->label)))
	    {
	      XtVaSetValues(a->label,XmNtopWidget,new_top,NULL);
	      break;
	    }
	}
    }
}

typedef struct {
  PANE *p;
  AMP *a;
  int in_chan,out_chan;
} slider_info;

static void Matrix_Button_Callback(Widget mb,XtPointer clientData,XtPointer callData) 
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
static Widget active_button = NULL;

enum {CORNER_BUTTON,OUTPUT_BUTTON,INPUT_BUTTON};

static void button_matrix_button_press(Widget w, XtPointer clientData, XEvent *event, Boolean *flag) 
{
  XButtonEvent *ev = (XButtonEvent *)event;
  button_matrix_button = ev->button;
  button_matrix_state = ev->state; 
  active_button = NULL;
  XtVaGetValues(w,XmNuserData,&initial_button,NULL);
}

static void button_matrix_button_motion(Widget w, XtPointer clientData, XEvent *event, Boolean *flag) 
{
  PANE *p = (PANE *)clientData;
  XButtonEvent *ev = (XButtonEvent *)event;
  Widget current_button = NULL;
  Position x,y;
  int bin=0,bout=0;
  XtVaGetValues(w,XmNx,&x,XmNy,&y,NULL);
  x += (ev->x - p->bx);
  y += (ev->y - p->by);
  if ((x > (Position)(p->bw)) || (y > (Position)(p->bh)) || (x<0) || (y<0)) return;
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

static void button_matrix_button_release(Widget w, XtPointer clientData, XEvent *event, Boolean *flag) 
{
  PANE *p = (PANE *)clientData;
  XButtonEvent *ev = (XButtonEvent *)event;
  Position x,y;
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
	  XtVaGetValues(w,XmNx,&x,NULL);
	  x += (ev->x - p->bx);
	  bout = x*p->out_chans/p->bw;
	  on = (button_matrix_button == 1);
	  for (row=0;row<p->in_chans;row++)
	    if (on != p->active_sliders[row][bout])
	      handle_matrix_slider(p->matrix_buttons[row][bout],p,row,bout,bout * p->in_chans + row,!on);
	  break;
	case INPUT_BUTTON:
	  XtVaGetValues(w,XmNy,&y,NULL);
	  y += (ev->y - p->by);
	  bin = y*p->in_chans/p->bh;
	  on = (button_matrix_button == 1);
	  for (col=0;col<p->out_chans;col++)
	    if (on != p->active_sliders[bin][col])
	      handle_matrix_slider(p->matrix_buttons[bin][col],p,bin,col,col * p->in_chans + bin,!on);
	  break;
	}
    }
}

static Widget sndCreateButtonMatrix(snd_state *ss, PANE *p, char *name, Widget parent, Arg *in_args, int in_n, Float meter_size)
{
  Widget outer_form,outer_frame;
  Arg args[32];
  int n,ins,outs,row,col,vu_rows;
  int width,height;
  Widget outputs_label,inputs_label0,inputs_label1,inputs_label2,inner_frame,inner_form,diag_button,mb;
  slider_info *si;
  int **active_sliders;

  active_sliders = p->active_sliders;
  vu_rows = p->in_chans / 4;
  if (vu_rows == 0) vu_rows = 1;
  height = (int)(vu_rows*(3*2 + LIGHT_Y*meter_size));
  width = height;
  XtSetArg(in_args[in_n],XmNshadowType,XmSHADOW_ETCHED_IN); in_n++;
  outer_frame = sndCreateFrameWidget(name,parent,in_args,in_n);
  XtAddEventHandler(outer_frame,ButtonPressMask,FALSE,button_matrix_button_press,(XtPointer)p);
  XtAddEventHandler(outer_frame,ButtonMotionMask,FALSE,button_matrix_button_motion,(XtPointer)p);
  XtAddEventHandler(outer_frame,ButtonReleaseMask,FALSE,button_matrix_button_release,(XtPointer)p);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNheight,(Dimension)height); n++;
  XtSetArg(args[n],XmNwidth,(Dimension)width); n++;
  XtSetArg(args[n],XmNresizePolicy,XmRESIZE_NONE); n++;
  outer_form = sndCreateFormWidget("outer-form",outer_frame,args,n);
  XtAddCallback(outer_form,XmNhelpCallback,Button_Matrix_Help_Callback,ss);
  XtAddEventHandler(outer_form,ButtonPressMask,FALSE,button_matrix_button_press,(XtPointer)p);
  XtAddEventHandler(outer_form,ButtonMotionMask,FALSE,button_matrix_button_motion,(XtPointer)p);
  XtAddEventHandler(outer_form,ButtonReleaseMask,FALSE,button_matrix_button_release,(XtPointer)p);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNuserData,CORNER_BUTTON); n++;
  diag_button = XtCreateManagedWidget("/",xmLabelWidgetClass,outer_form,args,n);
  XtAddEventHandler(diag_button,ButtonPressMask,FALSE,button_matrix_button_press,(XtPointer)p);
  XtAddEventHandler(diag_button,ButtonMotionMask,FALSE,button_matrix_button_motion,(XtPointer)p);
  XtAddEventHandler(diag_button,ButtonReleaseMask,FALSE,button_matrix_button_release,(XtPointer)p);
  XtUninstallTranslations(diag_button);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,diag_button); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;
  XtSetArg(args[n],XmNuserData,OUTPUT_BUTTON); n++;
  outputs_label = XtCreateManagedWidget("out",xmLabelWidgetClass,outer_form,args,n);
  XtAddEventHandler(outputs_label,ButtonPressMask,FALSE,button_matrix_button_press,(XtPointer)p);
  XtAddEventHandler(outputs_label,ButtonMotionMask,FALSE,button_matrix_button_motion,(XtPointer)p);
  XtAddEventHandler(outputs_label,ButtonReleaseMask,FALSE,button_matrix_button_release,(XtPointer)p);
  XtUninstallTranslations(outputs_label);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,diag_button); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNmarginHeight,0); n++;
  XtSetArg(args[n],XmNmarginWidth,0); n++;
  XtSetArg(args[n],XmNuserData,INPUT_BUTTON); n++;
  inputs_label0 = XtCreateManagedWidget("i",xmLabelWidgetClass,outer_form,args,n);
  XtAddEventHandler(inputs_label0,ButtonPressMask,FALSE,button_matrix_button_press,(XtPointer)p);
  XtAddEventHandler(inputs_label0,ButtonMotionMask,FALSE,button_matrix_button_motion,(XtPointer)p);
  XtAddEventHandler(inputs_label0,ButtonReleaseMask,FALSE,button_matrix_button_release,(XtPointer)p);
  XtUninstallTranslations(inputs_label0);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,inputs_label0); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNmarginHeight,0); n++;
  XtSetArg(args[n],XmNmarginWidth,0); n++;
  XtSetArg(args[n],XmNuserData,INPUT_BUTTON); n++;
  inputs_label1 = XtCreateManagedWidget("n",xmLabelWidgetClass,outer_form,args,n);
  XtAddEventHandler(inputs_label1,ButtonPressMask,FALSE,button_matrix_button_press,(XtPointer)p);
  XtAddEventHandler(inputs_label1,ButtonMotionMask,FALSE,button_matrix_button_motion,(XtPointer)p);
  XtAddEventHandler(inputs_label1,ButtonReleaseMask,FALSE,button_matrix_button_release,(XtPointer)p);
  XtUninstallTranslations(inputs_label1);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,inputs_label1); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNuserData,INPUT_BUTTON); n++;
  inputs_label2 = XtCreateManagedWidget(" ",xmLabelWidgetClass,outer_form,args,n);
  XtAddEventHandler(inputs_label2,ButtonPressMask,FALSE,button_matrix_button_press,(XtPointer)p);
  XtAddEventHandler(inputs_label2,ButtonMotionMask,FALSE,button_matrix_button_motion,(XtPointer)p);
  XtAddEventHandler(inputs_label2,ButtonReleaseMask,FALSE,button_matrix_button_release,(XtPointer)p);
  XtUninstallTranslations(inputs_label2);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,inputs_label0); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,outputs_label); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNshadowType,XmSHADOW_ETCHED_OUT); n++;
  inner_frame = sndCreateFrameWidget("inner-frame",outer_form,args,n);

  ins = p->in_chans;
  outs = p->out_chans;
  p->matrix_buttons = (Widget **)CALLOC(ins,sizeof(Widget *));
  for (row=0;row<ins;row++) p->matrix_buttons[row] = (Widget *)CALLOC(outs,sizeof(Widget));

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNfractionBase,ins*outs); n++;
  inner_form = sndCreateFormWidget("inner-form",inner_frame,args,n);

  for (col=0;col<outs;col++)
    for (row=0;row<ins;row++)
      {
	si = (slider_info *)CALLOC(1,sizeof(slider_info));
	si->p = p;
	si->in_chan = row;
	si->out_chan = col;

	n=0;
	if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
	XtSetArg(args[n],XmNleftAttachment,XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition,col*ins); n++;
	XtSetArg(args[n],XmNrightAttachment,XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition,(col+1)*ins); n++;
	XtSetArg(args[n],XmNtopAttachment,XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,row*outs); n++;
	XtSetArg(args[n],XmNbottomAttachment,XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,(row+1)*outs); n++;
	XtSetArg(args[n],XmNborderWidth,0); n++;
	XtSetArg(args[n],XmNhighlightColor,(ss->sgx)->basic_color); n++;
	XtSetArg(args[n],XmNmarginHeight,0); n++;
	XtSetArg(args[n],XmNmarginWidth,0); n++;
	XtSetArg(args[n],XmNshadowThickness,1); n++;
	XtSetArg(args[n],XmNarmColor,(ss->sgx)->green); n++;
	XtSetArg(args[n],XmNfillOnArm,TRUE); n++;
	mb = sndCreatePushButtonWidget(" ",inner_form,args,n);
	XtAddCallback(mb,XmNactivateCallback,Matrix_Button_Callback,si);
	if (active_sliders[row][col]) XmChangeColor(mb,(Pixel)((ss->sgx)->green));
	p->matrix_buttons[row][col] = mb;
      }
  XtVaGetValues(inner_frame,XmNx,&(p->bx),XmNy,&(p->by),XmNwidth,&(p->bw),XmNheight,&(p->bh),NULL);
  return(outer_frame);
}

/* -------- I/O pane -------- */

int device_channels(int dev); /* audio.c */
/* for testing, it's convenient to fake up a device here */
/* static int device_channels(int dev) {return(8);} */
int device_gains(int dev);

static PANE *make_pane(snd_state *ss, Widget paned_window, int device, int system)
{
  /* VU meters (frame, then drawing area widget) */
  /* Linux OSS complication -- the top input panel also has all the "mixer" input volume controls and the output pane has the tone controls, if any */
  Wdesc *wd;
  int n,i,k,chan,amp_sliders,temp_out_chan,temp_in_chan,in_chan,out_chan;
  Arg args[32];
  AMP *a;
  PANE *p;
  Widget *frames = NULL;
  Widget frame,meter,last_frame,vu_vertical_sep,icon_label,last_slider=NULL,max_label,matrix_frame,
    button_label,button_box,button1_label,last_button,slider_sep,first_frame,last_max,left_frame;
  VU *vu;
  int vu_meters,num_audio_gains,button_size,input,special_cases = 0;
  int row,columns;
  XmString labelstr;
  Position pane_max;
  Float meter_size,vol;
  state_context *sx;
#if (HAVE_OSS || HAVE_ALSA)
  float mixer_field_chans[32];
  int mixflds[32];
  int last_device,this_device=0;
  XmString slabel = NULL;
#endif

  sx = ss->sgx;
  p = (PANE *)CALLOC(1,sizeof(PANE));
  p->device = device;
#if (HAVE_ALSA || HAVE_OSS)
  p->system = system;
#endif
  p->ss = ss;
  vu_meters = device_channels(MUS_AUDIO_PACK_SYSTEM(system) | device);
  input = (input_device(device));
  num_audio_gains = device_gains(MUS_AUDIO_PACK_SYSTEM(system) | device);
#if (HAVE_OSS || HAVE_ALSA)
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
  frames = (Widget *)CALLOC(vu_meters,sizeof(Widget));

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNallowResize,TRUE); n++;
  p->pane = sndCreateFormWidget("pane",paned_window,args,n);
  
  last_frame = NULL;
  first_frame = NULL;
  left_frame = NULL;
  meter_size = vu_size(ss);
  if (vu_meters > 4) meter_size *= .6; else if (vu_meters > 2) meter_size *= .8;
  if ((vu_meters%5) == 0) meter_size *= 0.8;

  if ((input) && ((p->in_chans*p->out_chans) > 8))
    {
      for (i=0;i<p->in_chans;i++) 
	for (k=0;k<p->out_chans;k++) 
	  if (i == k) p->active_sliders[i][k] = 1;
      /* PICKUP DEFAULTS HERE */

      /* rather than default to posting 64 (or 256!) sliders, set up a channel matrix where desired sliders can be set */
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      matrix_frame = sndCreateButtonMatrix(ss,p,"channel-matrix",p->pane,args,n,meter_size);
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
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,sx->black); n++;}
      if (row == 0)
	{
	  /* this is the top row of meters, attached to the top of the pane */
	  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
	}
      else
	{
	  /* this is a subsequent row, attached to the previous row's frames */
	  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
	  XtSetArg(args[n],XmNtopWidget,frames[i-columns]); n++;
	}
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      if (!last_frame) 
	{
	  if (left_frame)
	    {
	      XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
	      XtSetArg(args[n],XmNleftWidget,left_frame); n++;
	    }
	  else
	    {
	      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
	    }
	}
      else 
	{
	  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
	  XtSetArg(args[n],XmNleftWidget,last_frame); n++;
	}
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNshadowType,XmSHADOW_ETCHED_IN); n++;
      if (vu_meters < 4)
	{XtSetArg(args[n],XmNshadowThickness,6); n++;}
      else {XtSetArg(args[n],XmNshadowThickness,3); n++;}
      frame = sndCreateFrameWidget("frame",p->pane,args,n);
      frames[i] = frame;
      if (!first_frame) first_frame = frame;

      n=0;
      XtSetArg(args[n],XmNbackground,sx->white); n++;
      XtSetArg(args[n],XmNforeground,sx->black); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNwidth,CENTER_X*2*meter_size); n++;
      XtSetArg(args[n],XmNheight,LIGHT_Y*meter_size); n++;
      XtSetArg(args[n],XmNallowResize,FALSE); n++;
      meter = sndCreateDrawingAreaWidget("vu",frame,args,n);
      p->meters[i] = make_vu_meter(meter,LIGHT_X,LIGHT_Y,CENTER_X,CENTER_Y,sx->black,ss,meter_size);
      vu = p->meters[i];

      if (input)
	rec_in_VU[overall_input_ctr+i] = vu;
      else rec_out_VU[i] = vu;
      XtAddCallback(meter,XmNexposeCallback,Meter_Display_Callback,vu);
      XtAddCallback(meter,XmNresizeCallback,Meter_Display_Callback,vu);
      wd = (Wdesc *)CALLOC(1,sizeof(Wdesc));
      wd->chan = i;
      wd->ss = ss;
      wd->p = p;
      wd->field = MUS_AUDIO_AMP;
      wd->device = device;
      wd->system = system;
      XtAddCallback(meter,XmNhelpCallback,Meter_Help_Callback,wd);
      last_frame = frame;
      if ((i == (columns*(row+1) - 1)) && (vu_meters > (i+1))) 
	{
	  last_frame = NULL;
	  first_frame = NULL;
	  row++;
	}
    }
  
  /* if no audio (hardware) gains, we have the vu separator and the control buttons */

  /* vertical scalers on the right (with icon) */
  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_OPPOSITE_WIDGET); n++;
  XtSetArg(args[n],XmNbottomWidget,last_frame); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,last_frame); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNseparatorType,XmNO_LINE); n++;
  XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
  if (vu_meters < 4)
    {XtSetArg(args[n],XmNwidth,10); n++;}
  else {XtSetArg(args[n],XmNwidth,4); n++;}
  vu_vertical_sep = XtCreateManagedWidget("sep",xmSeparatorWidgetClass,p->pane,args,n);

  if (num_audio_gains > 0)
    {
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNlabelType,XmPIXMAP); n++;
#if (HAVE_OSS || HAVE_ALSA)
      if (input)
	{XtSetArg(args[n],XmNlabelPixmap,device_icon((mixflds[MUS_AUDIO_MICROPHONE]>0) ? MUS_AUDIO_MICROPHONE : MUS_AUDIO_LINE_IN)); n++;}
      else {XtSetArg(args[n],XmNlabelPixmap,device_icon(device)); n++;}
      if (input)
	{
	  if (mixflds[MUS_AUDIO_MICROPHONE]<=0)
	    last_device = MUS_AUDIO_LINE;
	}
      else last_device = MUS_AUDIO_DAC_OUT;
#else
      XtSetArg(args[n],XmNlabelPixmap,device_icon(device)); n++;
#endif
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      if (num_audio_gains > 1)
	{XtSetArg(args[n],XmNalignment,XmALIGNMENT_CENTER); n++;}
      else {XtSetArg(args[n],XmNalignment,XmALIGNMENT_END); n++;}
#if (HAVE_OSS || HAVE_ALSA)
      if (input)
	{XtSetArg(args[n],XmNwidth,(mixflds[last_device] == 2) ? 30 : 15); n++;}
      else {XtSetArg(args[n],XmNwidth,30); n++;}
#else
      XtSetArg(args[n],XmNwidth,30); n++;
#endif
      icon_label = XtCreateManagedWidget("icon",xmLabelWidgetClass,p->pane,args,n);
      
      last_slider = NULL;
      for (i=0,chan=num_audio_gains-1;i<num_audio_gains;i++,chan--)
	{
	  /* we're moving right to left here, so the first slider represents the highest channel */
	  /* in the Linux case, as each new device pops up, we need some indication above it */
	  wd = (Wdesc *)CALLOC(1,sizeof(Wdesc));
	  wd->system = system;
#if (HAVE_OSS || HAVE_ALSA)
	  /* we're moving right to left here, chan is counting down, we need to fill out MIXER|MUS_AUDIO_DAC_FILTER fields and channels */
	  /* and also handle whatever else comes along */
	  /* treble and bass are actually stereo -- we'll do both at once */
	  if (!input)
	    {
	      if (mus_audio_api() == ALSA_API) 
		{
		  /* the existing code assumes there are tone controls and that
		   * a certain device is the output (MUS_AUDIO_DAC_OUT), for now
		   * no tone controls at all */
		  wd->chan = 1;
		  wd->field = MUS_AUDIO_AMP;
		  wd->device = device;
		  this_device = device;
		}
	      else
		{
		  /* count back from speaker 1 0 treble bass */
		  switch (i)
		    {
		    case 0: 
		      wd->chan = 1;
		      wd->field = MUS_AUDIO_AMP;
		      wd->device = MUS_AUDIO_DAC_OUT;
		      break;
		    case 1: 
		      wd->chan = 0;
		      wd->field = MUS_AUDIO_AMP;
		      wd->device = MUS_AUDIO_DAC_OUT;
		      break;
		    case 2: 
		      wd->chan = 0;
		      wd->field = MUS_AUDIO_TREBLE;
		      wd->device = MUS_AUDIO_DAC_FILTER;
		      break;
		    case 3: 
		      wd->chan =0;
		      wd->field = MUS_AUDIO_BASS;
		      wd->device = MUS_AUDIO_DAC_FILTER;
		      break;
		    }
		  if (i>1) this_device = MUS_AUDIO_DAC_FILTER; else this_device = MUS_AUDIO_DAC_OUT;
		}
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
#if (HAVE_OSS || HAVE_ALSA)
	  if (last_device != this_device)
	    {
	      n=0;
	      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
	      if (this_device == MUS_AUDIO_LINE)
		{
		  XtSetArg(args[n],XmNlabelType,XmPIXMAP); n++;
		  XtSetArg(args[n],XmNlabelPixmap,device_icon(MUS_AUDIO_LINE_IN)); n++;
		}
	      else
		{
		  if ((!input) && (this_device == MUS_AUDIO_DAC_FILTER))
		    slabel = XmStringCreate("ton","small_font");
		  else slabel = XmStringCreate(field_abbreviation(this_device),"small_font");
		  XtSetArg(args[n],XmNlabelString,slabel); n++;
		}
	      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
	      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
	      XtSetArg(args[n],XmNleftAttachment,XmATTACH_NONE); n++;
	      XtSetArg(args[n],XmNrightAttachment,XmATTACH_WIDGET); n++;
	      XtSetArg(args[n],XmNrightWidget,icon_label); n++;
	      XtSetArg(args[n],XM_FONT_RESOURCE,small_fontlist); n++;
	      XtSetArg(args[n],XmNalignment,XmALIGNMENT_CENTER); n++;
	      XtSetArg(args[n],XmNwidth,30); n++;
	      if (input)
		{XtSetArg(args[n],XmNwidth,(mixflds[this_device] == 1) ? 30 : 15); n++;} /* 1 because we subtracted one already above */
	      else {XtSetArg(args[n],XmNwidth,30); n++;}
	      icon_label = XtCreateManagedWidget("icon",xmLabelWidgetClass,p->pane,args,n);
	      if ((this_device != MUS_AUDIO_LINE) && (slabel)) {XmStringFree(slabel); slabel=NULL;}
	    }
#endif
	  n=0;
	  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->zoom_color); n++;}
	  if (last_slider)
	    {
	      XtSetArg(args[n],XmNtopAttachment,XmATTACH_OPPOSITE_WIDGET); n++;
	      XtSetArg(args[n],XmNtopWidget,last_slider); n++;
	      XtSetArg(args[n],XmNrightAttachment,XmATTACH_WIDGET); n++;
	      XtSetArg(args[n],XmNrightWidget,last_slider); n++;
	    }
	  else
	    {
	      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
	      XtSetArg(args[n],XmNtopWidget,icon_label); n++;
	      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
	    }
	  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
	  XtSetArg(args[n],XmNleftAttachment,XmATTACH_NONE); n++;
	  XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
#ifdef LESSTIF_VERSION
	  XtSetArg(args[n],XmNscaleWidth,15); n++;
#else
	  XtSetArg(args[n],XmNwidth,15); n++;
#endif
	  if (vol < 0.0) vol = 0.0;
	  if (vol > 1.0) vol = 1.0;
	  XtSetArg(args[n],XmNvalue,(int)(vol*100)); n++;
	  XtSetArg(args[n],XmNshowValue,FALSE); n++;
	  wd->wg = XtCreateManagedWidget("mon",xmScaleWidgetClass,p->pane,args,n);
	  last_slider = wd->wg;
	  XtAddCallback(last_slider,XmNvalueChangedCallback,volume_callback,wd);
	  XtAddCallback(last_slider,XmNdragCallback,volume_callback,wd);
	  XtAddCallback(last_slider,XmNhelpCallback,volume_help_callback,wd);
#if (HAVE_OSS || HAVE_ALSA)
	  last_device = this_device;
#endif
	}
      gain_ctr += num_audio_gains;
    }

  /* separator between vertical sliders and buttons */
  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_NONE); n++;
  if (num_audio_gains > 0)
    {
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNrightWidget,last_slider); n++;
    }
  else
    {
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
    }
  XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
  if (vu_meters < 4)
    {XtSetArg(args[n],XmNwidth,10); n++;}
  else {XtSetArg(args[n],XmNwidth,4); n++;}
  XtSetArg(args[n],XmNseparatorType,XmNO_LINE); n++;
  p->button_vertical_sep = XtCreateManagedWidget("ff-sep5",xmSeparatorWidgetClass,p->pane,args,n);      
  
  /* controls buttons with label */
  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,vu_vertical_sep); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNrightWidget,p->button_vertical_sep); n++;
  if (meter_size<SMALL_FONT_CUTOFF) {XtSetArg(args[n],XM_FONT_RESOURCE,small_fontlist); n++;}
  if ((systems == 1) || (!input))
    button_label = XtCreateManagedWidget(Device_Name(device),xmLabelWidgetClass,p->pane,args,n);
  else 
    {
      button1_label = XtCreateManagedWidget(mus_audio_system_name(system),xmLabelWidgetClass,p->pane,args,n);
      /* using 2 labels here because there is no way to get a multiline label (at least in Metroworks Motif) */
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,button1_label); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNleftWidget,vu_vertical_sep); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNrightWidget,p->button_vertical_sep); n++;
      if (meter_size<SMALL_FONT_CUTOFF) {XtSetArg(args[n],XM_FONT_RESOURCE,small_fontlist); n++;}
      button_label = XtCreateManagedWidget(Device_Name(device),xmLabelWidgetClass,p->pane,args,n);
    }
  
  /* all the buttons and labels except the top device name are contained in a separate box (form widget) */
  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,button_label); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,vu_vertical_sep); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNrightWidget,p->button_vertical_sep); n++;
  button_box = sndCreateFormWidget("data",p->pane,args,n);
  
  p->on_buttons = (Widget *)CALLOC(vu_meters,sizeof(Widget));
  p->on_buttons_size = vu_meters;
  
  last_button = NULL;
  last_max = NULL;

  if ((vu_meters%4) == 0)
    columns = 4;
  else 
    {
      if ((vu_meters%5) == 0)
	columns = 5;
      else columns = vu_meters;
    }
  row = 0;
  button_size = 100/columns;

  for (i=0;i<vu_meters;i++)
    {
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      if (row == 0)
	{XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;}
      else 
	{
	  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
	  XtSetArg(args[n],XmNtopWidget,frames[i-columns]); n++;
	}
      if (last_button)
	{
	  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_OPPOSITE_WIDGET); n++;
	  XtSetArg(args[n],XmNbottomWidget,last_button); n++;
	  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
	  XtSetArg(args[n],XmNleftWidget,last_button); n++;
	}
      else
	{
	  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
	  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
	}
      if (meter_size<SMALL_FONT_CUTOFF) {XtSetArg(args[n],XM_FONT_RESOURCE,small_fontlist); n++;}
      if (((i+1)%columns) != 0)
	{
	  /* these are the right sides of the buttons before the rightmost one */
	  XtSetArg(args[n],XmNrightAttachment,XmATTACH_POSITION); n++;
	  XtSetArg(args[n],XmNrightPosition,((i+1)%columns)*button_size); n++;
	}
      else 
	{
	  /* this is the rightmost button in a given row */
	  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
	}
      p->on_buttons[i] = XtCreateManagedWidget(channel_name(p,i),xmPushButtonWidgetClass,button_box,args,n);
      last_button = p->on_buttons[i];
      wd = (Wdesc *)CALLOC(1,sizeof(Wdesc));
      wd->chan = i;
      wd->gain = i + overall_input_ctr;
      wd->ss = ss;
      wd->p = p;
      wd->device = p->device;
      wd->system = system;
      wd->field = MUS_AUDIO_AMP;
      XtAddCallback(last_button,XmNactivateCallback,Meter_Button_Callback,wd);
      XtAddCallback(last_button,XmNhelpCallback,VU_On_Help_Callback,wd);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,last_button); n++;
      if (i >= (vu_meters - columns))
	{XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;}
      else {XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;}
      if (last_max)
	{
	  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
	  XtSetArg(args[n],XmNleftWidget,last_max); n++;
	}
      else
	{
	  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
	}
      if (((i+1)%columns) != 0)
	{
	  XtSetArg(args[n],XmNrightAttachment,XmATTACH_POSITION); n++;
	  XtSetArg(args[n],XmNrightPosition,button_size*((i+1)%columns)); n++;
	}
      else
	{
	  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
	}
      XtSetArg(args[n],XmNshadowType,XmSHADOW_ETCHED_OUT); n++;
      last_max = sndCreateFrameWidget("max-frame1",button_box,args,n);
      frames[i] = last_max;
      
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNrecomputeSize,FALSE); n++;
      if (meter_size<SMALL_FONT_CUTOFF) {XtSetArg(args[n],XM_FONT_RESOURCE,small_fontlist); n++;}
      max_label = XtCreateManagedWidget("0.000",xmLabelWidgetClass,last_max,args,n);
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
      XtAddCallback(max_label,XmNhelpCallback,VU_Max_Help_Callback,wd);

      if ((i == (columns*(row+1) - 1)) && (vu_meters > (i+1))) 
	{
	  last_button = NULL; 
	  last_max = NULL;
	  row++;
	}
    }
  if (frames) {FREE(frames); frames = NULL;}

  if (meter_size<SMALL_FONT_CUTOFF)
    labelstr = XmStringCreate(STR_Reset,"small_font");
  else labelstr = XmStringCreate(STR_Reset,XmFONTLIST_DEFAULT_TAG);
  n=0;
  if (!(ss->using_schemes)) 
    {
      XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;
      XtSetArg(args[n],XmNarmColor,sx->pushed_button_color); n++;
    }
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,button_box); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_OPPOSITE_WIDGET); n++;
  XtSetArg(args[n],XmNbottomWidget,vu_vertical_sep); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,vu_vertical_sep); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNrightWidget,p->button_vertical_sep); n++;
  if (meter_size<SMALL_FONT_CUTOFF) {XtSetArg(args[n],XM_FONT_RESOURCE,small_fontlist); n++;}
  XtSetArg(args[n],XmNlabelString,labelstr); n++;
  p->reset_button = XtCreateManagedWidget("reset",xmPushButtonWidgetClass,p->pane,args,n);
  XtAddCallback(p->reset_button,XmNactivateCallback,VU_Reset_Callback,p);
  XtAddCallback(p->reset_button,XmNhelpCallback,VU_Reset_Help_Callback,ss);
  XmStringFree(labelstr);

  /* now the amp sliders across the bottom of the pane, with 'mixer' info on the right */
  
  last_slider = NULL;
  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,first_frame); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNrightWidget,p->button_vertical_sep); n++;
  XtSetArg(args[n],XmNseparatorType,XmNO_LINE); n++;
  XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
  XtSetArg(args[n],XmNheight,10); n++;
  slider_sep = XtCreateManagedWidget("amp-sep",xmSeparatorWidgetClass,p->pane,args,n);
  last_slider = slider_sep;
#if 0
  if (all_panes_size > 4) 
    {
      XtVaGetValues(slider_sep,XmNy,&pane_max,NULL);
      p->pane_size = pane_max;
    }
#endif  
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
	  last_slider = sndCreateRecorderSlider(ss,p,a,last_slider,input);
	  XtVaGetValues(last_slider,XmNy,&pane_max,NULL);
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

#if (HAVE_ALSA || HAVE_OSS)

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
  int in_chan,out_chan,i,k,m,n,out_frame,mon_samp,diff,inchn,offset,active_in_chans,ochns,sr,buffer_size,in_datum_size;
  MUS_SAMPLE_TYPE val;
  MUS_SAMPLE_TYPE *fbufs[1];
  if (ever_read == 0) return(BACKGROUND_QUIT); /* should not happen, but ... */
  out_frame = 0;
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
      mus_audio_read(record_fd[0],record_buf[0],buffer_size*in_datum_size);
      fbufs[0] = fbuf;
      mus_file_read_buffer(input_format[0],0,1,buffer_size,fbufs,record_buf[0]);
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
  for (i=0,out_frame=0;i<buffer_size;i+=active_in_chans,out_frame++)
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
	  if ((recording) && (rec_out_active[out_chan])) obufs[out_chan][out_frame] = val;
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
  if ((monitor_open) && (obufs) && (ochns <= audio_out_chans))
    {
      /* opened in recorder_out_format and audio_out_chans */
      mus_file_write_buffer(monitor_out_format,0,out_frame-1,audio_out_chans,obufs,monitor_buf,data_clipped(ss));
      mus_audio_write(monitor_fd,monitor_buf,recorder_buffer_size(ss)*audio_out_chans*mus_data_format_to_bytes_per_sample(monitor_out_format));
    }
  if ((recording) && (triggered))
    {
      mus_file_write(output_fd,0,out_frame-1,ochns,obufs);
      total_out_frames += out_frame;
      if (total_out_frames > duration_frames)
	{
	  update_duration((Float)total_out_frames/(Float)sr);
	  duration_frames += (sr / 4);
	}
    }
  return(((total_out_frames/sr) >= max_duration) ? BACKGROUND_QUIT : BACKGROUND_CONTINUE);
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
  int in_chan,out_chan,i,k,m,n,out_frame,mon_samp,diff,inchn,offset,active_in_chans,cur_size,ochns,sr,sz,ifmt,in_datum_size;
  MUS_SAMPLE_TYPE val;
  MUS_SAMPLE_TYPE *fbufs[1];
  if (ever_read == 0) return(BACKGROUND_QUIT); /* should not happen, but ... */
  fbufs[0] = fbuf;
  out_frame = 0;
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
  for (i=0,out_frame=0;i<sz;i+=active_in_chans,out_frame++)
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
	  if ((recording) && (rec_out_active[out_chan])) obufs[out_chan][out_frame] = val;
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

  if ((monitor_open) && (obufs) && (ochns == audio_out_chans))
    {
      /* opened in recorder_out_format and audio_out_chans */
      mus_file_write_buffer(recorder_out_format(ss),0,out_frame-1,audio_out_chans,obufs,record_buf[0],data_clipped(ss));
      mus_audio_write(monitor_fd,record_buf[0],out_frame*audio_out_chans*mus_data_format_to_bytes_per_sample(recorder_out_format(ss)));
    }
  if ((recording) && (triggered))
    {
      mus_file_write(output_fd,0,out_frame-1,ochns,obufs);
      total_out_frames += out_frame;
      if (total_out_frames > duration_frames)
	{
	  update_duration((Float)total_out_frames/(Float)sr);
	  duration_frames += (sr / 4);
	}
    }
  return(((total_out_frames/sr) >= max_duration) ? BACKGROUND_QUIT : BACKGROUND_CONTINUE);
}
#endif

static void finish_recording(snd_state *ss);

static BACKGROUND_TYPE run_adc(XtPointer ss)  /* X wrapper for read_adc */
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
  record_report(messages,str,NULL);
  for (in_chan=0;in_chan<overall_in_chans;in_chan++)
    {
      sprintf(str,"in[%d, %d] %s (%s): %.3f -> (VU *)%p",
	      in_chan,in_device_chan[in_chan],
	      (in_device_on[in_chan]) ? "on" : "off",
	      (rec_in_active[in_chan]) ? "active" : "idle",
	      MUS_SAMPLE_TO_FLOAT(in_max[in_chan]),
	      rec_in_VU[in_chan]);
      record_report(messages,str,NULL);
    }
  FREE(str);
#endif
  ever_read = XtAppAddWorkProc((ss->sgx)->mainapp,run_adc,(XtPointer)ss);
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

static void RecordCleanupCB(Widget w,XtPointer clientData,XtPointer callData)
{
  cleanup_recording(); 
}

static void Reset_Record_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  /* if recording, cancel and toss data, else reset various fields to default (ss) values */
  snd_state *ss = (snd_state *)clientData;
  char *str;
  XmString s1;
  PANE *p;
  VU *vu;
  int i,k;
  if (recording)                  /* cancel */
    {
      recording = 0;
      triggered = (!triggering);
      sensitize_control_buttons();
      XmChangeColor(record_button,(Pixel)(ss->sgx)->basic_color);
      s1 = XmStringCreate(STR_Reset,XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(reset_button,XmNlabelString,s1,NULL);
      XmStringFree(s1);
      s1 = XmStringCreate((triggering) ? STR_Triggered_Record : STR_Record,XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(record_button,XmNlabelString,s1,NULL);
      XmStringFree(s1);
      snd_close(output_fd);
      output_fd = -1;
      str = just_filename(recorder_file(ss));
      record_report(messages,str," recording cancelled",NULL);
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
	  s1 = XmStringCreate(STR_Reset,XmFONTLIST_DEFAULT_TAG);
	  XtVaSetValues(reset_button,XmNlabelString,s1,NULL);
	  XmStringFree(s1);
	}
    }
}

static void Dismiss_Record_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  XmAnyCallbackStruct *cb = (XmAnyCallbackStruct *)callData;
  state_context *sgx;
  sgx = ss->sgx;
  if (cb->event != sgx->text_activate_event)  /* passed up from textfield widget after <cr> typed */
    {
      if (recording) Reset_Record_Callback(w,clientData,callData);
      XtUnmanageChild(recorder);
      close_recorder_audio();
#if (!(HAVE_OSS || HAVE_ALSA))
      mus_audio_restore();
#endif
    }
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
  XmString s1 = NULL,s2 = NULL;
  char *str;
  snd_info *sp;
  Float duration;
  sensitize_control_buttons();
  XmChangeColor(record_button,(Pixel)(ss->sgx)->basic_color);
  s1 = XmStringCreate(STR_Reset,XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(reset_button,XmNlabelString,s1,NULL);
  XmStringFree(s1);
  s2 = XmStringCreate((triggering) ? STR_Triggered_Record : STR_Record,XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(record_button,XmNlabelString,s2,NULL);
  XmStringFree(s2);
  snd_close(output_fd);
  output_fd = mus_file_reopen_write(recorder_file(ss));
  mus_header_update_with_fd(output_fd,out_type,total_out_frames*recorder_out_chans(ss)*mus_data_format_to_bytes_per_sample(recorder_out_format(ss)));
  close(output_fd);
  output_fd = -1;
  duration = (Float)total_out_frames / (Float)(recorder_srate(ss));
  /* 25-Jun-00: this used to divide by chans, but total_out_frames is in terms of frames (it was named total_out_samps) */
  update_duration(duration);
  str = (char *)CALLOC(256,sizeof(char));
  sprintf(str,"recorded %s:\n  duration: %.2f\n  srate: %d, chans: %d\n  type: %s, format: %s",
	  recorder_file(ss),duration,recorder_srate(ss),recorder_out_chans(ss),
	  mus_header_type_name(out_type),mus_data_format_name(recorder_out_format(ss)));
  record_report(messages,str,NULL);
  FREE(str);
  if (recorder_autoload(ss))
    {
      if ((sp = find_sound(ss,recorder_file(ss))))
	snd_update(ss,sp);
      else snd_open_file(recorder_file(ss),ss);
    }
}

static void Record_Button_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  XmString s1 = NULL,s2 = NULL;
  Wdesc *wd;
  int err,i,comlen,old_srate,ofmt,rs,ochns,oloc;
  static char *comment;
  char *str;
  PANE *p;
  recording = (!recording);
  if (recording)
    {
      if (!audio_open) fire_up_recorder(ss);
      str = XmTextGetString(file_text);
      if ((str) && (*str))
	{
	  in_set_recorder_file(ss,mus_file_full_name(str));
	  XtFree(str);
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
	      record_report(messages,STR_cant_record_screwed_up_chans,NULL);
	      recording = 0;
	      triggered = (!triggering);
	      return;
	    }
	  comment = XmTextGetString(recdat->comment_text);
	  update_duration(0.0);
	  
	  if (out_chans_active() != recorder_out_chans(ss))
	    {
	      if (msgbuf == NULL) msgbuf = (char *)CALLOC(512,sizeof(char));
	      sprintf(msgbuf,"chans field (%d) doesn't match file out panel (%d channels active)",recorder_out_chans(ss),out_chans_active());
	      record_report(messages,msgbuf,NULL);
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
		      Meter_Button_Callback(p->on_buttons[i],(XtPointer)wd,NULL); /* callData not used */
		    }
		}
	      FREE(wd);
	    }
	  if (in_chans_active() == 0)
	    {
	      record_report(messages,STR_cant_record_no_inputs,NULL);
	      recording = 0;
	      triggered = (!triggering);
	      return;
	    }
	  XmChangeColor(w,(Pixel)(ss->sgx)->red);
	  s1 = XmStringCreate(STR_Cancel,XmFONTLIST_DEFAULT_TAG);
	  XtVaSetValues(reset_button,XmNlabelString,s1,NULL);
	  XmStringFree(s1);
	  s2 = XmStringCreate(STR_Done,XmFONTLIST_DEFAULT_TAG);
	  XtVaSetValues(record_button,XmNlabelString,s2,NULL);
	  XmStringFree(s2);
	  comlen = (int)(snd_strlen(comment) + 3)/4;
	  comlen *= 4;
	  err = snd_write_header(ss,recorder_file(ss),out_type,recorder_srate(ss),recorder_out_chans(ss),28+comlen,0,
				 recorder_out_format(ss),comment,snd_strlen(comment),NULL);
	  if (err)
	    {
	      record_report(messages,recorder_file(ss),":\n  ",strerror(errno),NULL);
	      recording = 0;
	      triggered = (!triggering);
	      return;
	    }

	  unsensitize_control_buttons();

	  output_fd = snd_reopen_write(ss,recorder_file(ss));
	  mus_header_read_with_fd(output_fd);
	  mus_file_set_descriptors(output_fd,recorder_file(ss),
				   recorder_out_format(ss),mus_data_format_to_bytes_per_sample(recorder_out_format(ss)),mus_header_data_location(),
				   recorder_out_chans(ss),out_type);
	  mus_file_set_data_clipped(output_fd,data_clipped(ss));
	  total_out_frames = 0;
	  duration_frames = recorder_srate(ss)/4;
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
	  record_report(messages,STR_cant_record_no_output_file,NULL);
	  recording = 0;
	  triggered = (!triggering);
	  return;
	}
    }
  else 
    finish_recording(ss);
}

static void initialize_recorder(snd_state *ss);
static Widget rec_panes,message_pane,file_info_pane;

#define AUDVAL_SIZE 64

void snd_record_file(snd_state *ss)
{
  Arg args[32];
  int n,i,k,device,all_devices,input_devices,output_devices,def_out,system,cur_devices,err;
  int *ordered_devices,*ordered_systems;
  XmString xdismiss,xhelp,xreset,titlestr;
  XFontStruct *small_fontstruct;
  float audval[AUDVAL_SIZE];
  Atom wm_delete;
  XGCValues v;
  Display *dpy;
  Drawable wn;
  state_context *sx;
  PANE *p;

  if (!recorder)
    {
      sx = ss->sgx;
      dpy = sx->mdpy;
      wn = XtWindow(sx->mainpane);
      v.background = sx->basic_color;
      v.foreground = sx->black;
      draw_gc = XCreateGC(dpy,wn,GCForeground | GCBackground,&v);
      v.background = sx->white;
      v.foreground = sx->black;
      vu_gc = XCreateGC(dpy,wn,GCForeground | GCBackground,&v);

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
      device_buttons = (Widget *)CALLOC(device_buttons_size,sizeof(Widget));
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
#if (HAVE_ALSA || HAVE_OSS)
	      float direction = 0.0;
	      device = (int)audval[i+1];
	      /* FIXME: have not looked to see if oss sndlib supports MUS_AUDIO_DIRECTION */
	      if ((mus_audio_api() == ALSA_API && 
		   (err=mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(system)|device,MUS_AUDIO_DIRECTION,0,&direction))==0 &&
		   (int)direction == 1) 
		  ||
		  (mus_audio_api() == OSS_API &&
		   input_device(device)))
#else
	      device = (int)audval[i+1];
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
#if (HAVE_ALSA || HAVE_OSS)
      if (mus_audio_api() == ALSA_API) 
	{
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
	}
      else
	{
	  if (output_devices) ordered_devices[k] = MUS_AUDIO_DAC_OUT;
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

      /* now create recording dialog using the info gathered above */
      small_fontstruct = XLoadQueryFont(MAIN_DISPLAY(ss),(vu_size(ss) < SMALLER_FONT_CUTOFF) ? SMALLER_FONT : SMALL_FONT);
#if (USE_RENDITIONS)
      {
	XmRendition rend;
	n=0;
	if (small_fontstruct)
	  XtSetArg(args[n],XmNfontName,(vu_size(ss) < SMALLER_FONT_CUTOFF) ? SMALLER_FONT : SMALL_FONT);
	else XtSetArg(args[n],XmNfontName,button_font(ss));
	n++;
	XtSetArg(args[n],XmNfontType,XmFONT_IS_FONT); n++;
	XtSetArg(args[n],XmNloadModel,XmLOAD_DEFERRED); n++;
	rend = XmRenditionCreate(MAIN_SHELL(ss),"small_font",args,n);
	small_fontlist = XmRenderTableAddRenditions(NULL,&rend,1,XmMERGE_NEW);
      }
#else
      if (small_fontstruct)
	{
	  small_fontlist = XmFontListCreate(small_fontstruct,"smallfont");
	  XmFontListEntryCreate("small_font",XmFONT_IS_FONT,(XtPointer)small_fontstruct);
	}
      else
	{
	  small_fontlist = BUTTON_FONT(ss);
	  XmFontListEntryCreate("small_font",XmFONT_IS_FONT,(XtPointer)PEAK_NUMBERS_FONT(ss));
	}
#endif
      xdismiss = XmStringCreate(STR_Dismiss,XmFONTLIST_DEFAULT_TAG);
      xhelp = XmStringCreate(STR_Help,XmFONTLIST_DEFAULT_TAG);
      xreset = XmStringCreate(STR_Reset,XmFONTLIST_DEFAULT_TAG);
      titlestr = XmStringCreate(STR_Record,XmFONTLIST_DEFAULT_TAG);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNcancelLabelString,xreset); n++;
      XtSetArg(args[n],XmNhelpLabelString,xhelp); n++;
      XtSetArg(args[n],XmNokLabelString,xdismiss); n++;
      XtSetArg(args[n],XmNautoUnmanage,FALSE); n++;
      XtSetArg(args[n],XmNdialogTitle,titlestr); n++;
#if RESIZE_DIALOG
      XtSetArg(args[n],XmNallowResize,TRUE); n++;
      XtSetArg(args[n],XmNresizePolicy,XmRESIZE_ANY); n++;
      XtSetArg(args[n],XmNnoResize,FALSE); n++;
#endif
      XtSetArg(args[n],XmNtransient,FALSE); n++;
      recorder = XmCreateTemplateDialog(MAIN_SHELL(ss),STR_Record,args,n);
      add_dialog(ss,recorder);
#if OVERRIDE_TOGGLE
      override_form_translation(recorder);
#endif

      XtAddCallback(recorder,XmNcancelCallback,Reset_Record_Callback,ss);
      XtAddCallback(recorder,XmNhelpCallback,Help_Record_Callback,ss);
      XtAddCallback(recorder,XmNokCallback,Dismiss_Record_Callback,ss);
      XmStringFree(xhelp);
      XmStringFree(xdismiss);
      XmStringFree(xreset);

#ifndef LESSTIF_VERSION
      /* now add the RECORD button to the others at the bottom of the window */
      /* this is a push button from Motif's point of view, but its behaviour is like a toggle button */
      /* in Lesstif, this causes a segmentation fault, so we move the record button elsewhere */
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNlabelString,titlestr); n++;
      record_button = XtCreateManagedWidget("record-button",xmPushButtonWidgetClass,recorder,args,n);
      XtAddCallback(record_button,XmNactivateCallback,Record_Button_Callback,ss);
#endif
      XmStringFree(titlestr);

#ifndef LESSTIF_VERSION
      reset_button = XtNameToWidget(recorder,"Cancel");
#else
      reset_button = XmMessageBoxGetChild(recorder,XmDIALOG_CANCEL_BUTTON);
#endif

      if (!(ss->using_schemes))
	{
	  XtVaSetValues(XmMessageBoxGetChild(recorder,XmDIALOG_CANCEL_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
	  XtVaSetValues(XmMessageBoxGetChild(recorder,XmDIALOG_HELP_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
	  XtVaSetValues(XmMessageBoxGetChild(recorder,XmDIALOG_OK_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
	}

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
#ifdef LESSTIF_VERSION
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
#else
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNbottomWidget,XmMessageBoxGetChild(recorder,XmDIALOG_SEPARATOR)); n++;
#endif
      XtSetArg(args[n],XmNallowResize,TRUE); n++;
      XtSetArg(args[n],XmNsashHeight,ss->sash_size); n++;
      XtSetArg(args[n],XmNsashWidth,ss->sash_size); n++;
      XtSetArg(args[n],XmNsashIndent,-1); n++; /* need room for icons -- default is -10 */
      rec_panes = sndCreatePanedWindowWidget("rec-panes",recorder,args,n);

      XtManageChild(recorder);
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
	  all_panes[i] = make_pane(ss,rec_panes,device,system);
	}

      /* then make file_info_pane and message_pane at the bottom */
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNallowResize,TRUE); n++;

      file_info_pane = sndCreateFormWidget("file-pane",rec_panes,args,n);
      message_pane = sndCreateFormWidget("msg-pane",rec_panes,args,n);

      make_file_info_pane(ss,file_info_pane,ordered_devices,all_panes_size,ordered_systems);
      messages = make_message_pane(ss,message_pane);
      if (!(ss->using_schemes)) map_over_children(rec_panes,color_sashes,(void *)ss);

      /* loop through all panes reading p->pane_size and */
      for (i=0;i<all_panes_size;i++)
	{
	  p = all_panes[i];
	  XtVaSetValues(p->pane,XmNpaneMaximum,p->pane_size,NULL);
#ifdef LESSTIF_VERSION
	  XtVaSetValues(p->pane,XmNheight,p->pane_size,NULL);
#endif
	}

#ifdef LESSTIF_VERSION
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      record_button = XtCreateManagedWidget(STR_Record,xmPushButtonWidgetClass,recorder,args,n);
      XtAddCallback(record_button,XmNactivateCallback,Record_Button_Callback,ss);
#endif

      /* in case caller closes (via window menu) dialog: */
      wm_delete = XmInternAtom(XtDisplay(recorder),"WM_DELETE_WINDOW",FALSE);
      XmAddWMProtocolCallback(XtParent(recorder),wm_delete,RecordCleanupCB,(XtPointer)ss);

      initialize_recorder(ss);
    }
  else 
    {
      raise_dialog(recorder);
    }
  if (!XtIsManaged(recorder)) XtManageChild(recorder);
  XtVaSetValues(message_pane,XmNpaneMinimum,1,NULL);
  for (i=0;i<all_panes_size;i++)
    {
      p = all_panes[i];
      XtVaSetValues(p->pane,XmNpaneMaximum,LOTSA_PIXELS,NULL); /* release max once we're set up so user can do what he wants */
    }

  if (pending_errors>0)
    {
      for (i=0;i<pending_errors;i++)
	{
	  record_report(messages,pending_error[i],NULL);
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
  if (recorder) XmToggleButtonSetState(device_buttons[autoload_button],val,FALSE); 
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
  int temp;
  Wdesc *wd;
  switch (which)
    {
    case REC_IN_AMPS:     
      if (recorder)
	{
	  temp = amp_to_slider(valf); 
	  record_amp_changed(rec_in_AMPS[vali][valj],temp); 
	  XtVaSetValues(rec_in_AMPS[vali][valj]->slider,XmNvalue,temp,NULL); 
	}
      else add_pending_setf(which,vali,valj,valf);
      break;
    case REC_OUT_AMPS:
      if (recorder)
	{
	  temp = amp_to_slider(valf); 
	  record_amp_changed(rec_out_AMPS[vali],temp); 
	  XtVaSetValues(rec_out_AMPS[vali]->slider,XmNvalue,temp,NULL); 
	}
      else add_pending_setf(which,vali,valj,valf);
      break;
    case AUDIO_GAINS:
      if (recorder)
	{
	  wd = audio_GAINS[vali];
	  XtVaSetValues(wd->wg,XmNvalue,(int)(valf * 100),NULL);
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
  XmToggleButtonSetState(device_buttons[digital_in_button],in_digital,FALSE);
  device_button_callback(device_buttons[digital_in_button],(XtPointer)all_panes[digital_in_button],NULL);
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
	      XmToggleButtonSetState(device_buttons[i],FALSE,TRUE); 
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


#if (HAVE_ALSA || HAVE_OSS)

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

  if (mus_audio_api() == ALSA_API) 
    {
      /* ALSA_API: Select first input device for each system */

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
	  record_report(messages,"no inputs?: ",mus_audio_error_name(mus_audio_error()),NULL);
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
		      record_report(messages,"open device[%d:%d]: ",mus_audio_error_name(mus_audio_error()),NULL);
		      for (j=0;j<all_panes_size;j++)
			{
			  sys = all_panes[j]->system;
			  if (record_fd[sys] != -1)
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
		      record_report(messages,"open output: ",mus_audio_error_name(mus_audio_error()),NULL);
		      monitor_open = 0;
		    }
		  break;
		}
	    }
	}
    }
  else
    {
      /* OSS_API */
      for (i=0;i<systems;i++)
	{
	  if (!(record_buf[i]))
	    record_buf[i] = (char *)CALLOC(recorder_buffer_size(ss),sizeof(int)); /* 4 bytes per sample is probably enough?? */
	  input_format[i] = recorder_in_format(ss);
	  input_buffer_size[i] = recorder_buffer_size(ss) / recorder_out_chans(ss);
	}
      for (i=0;i<systems;i++)
	{
	  if (input_channels[i] == 0)
	    {
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
	    }
	}
      err = 1;
      for (i=0;i<systems;i++) if (input_channels[i] > 0) {err = 0; break;}
      if (err)
	{
	  record_report(messages,"no inputs?: ",mus_audio_error_name(mus_audio_error()),NULL);
	  return(-1);
	}
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
      if (record_fd[0] == -1)
	{
	  record_report(messages,"open device: ",mus_audio_error_name(mus_audio_error()),NULL);
	  return(-1);
	}
      audio_open = 1;
      monitor_fd = 0;

      /*
       * if (full_duplex(0))
       *   monitor_fd = mus_audio_open_output(MUS_AUDIO_DUPLEX_DEFAULT,recorder_srate(ss),audio_out_chans,recorder_out_format(ss),recorder_buffer_size(ss));
       * else record_report(messages,"Simultaneous input and output is not enabled on this card.",NULL);
       */

      if (monitor_fd == -1)
	{
	  record_report(messages,"open output: ",mus_audio_error_name(mus_audio_error()),NULL);
	  monitor_open = 0;
	}
      else monitor_open = monitor_fd;
    }
  set_read_in_progress(ss);
  return(0);
}

#else /* not ALSA or OSS */

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
      record_buf[i] = (char *)CALLOC(recorder_buffer_size(ss),sizeof(int)); /* 4 bytes per sample is probably enough?? */
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
      record_report(messages,"no inputs?: ",mus_audio_error_name(mus_audio_error()),NULL);
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
      record_report(messages,"open device: ",mus_audio_error_name(mus_audio_error()),NULL);
      return(-1);
    }
  audio_open = 1;
#if ((HAVE_OSS) || defined(SUN))
  monitor_fd = 0;

  /*
   * if (full_duplex(0))
   *   monitor_fd = mus_audio_open_output(MUS_AUDIO_DUPLEX_DEFAULT,recorder_srate(ss),audio_out_chans,recorder_out_format(ss),recorder_buffer_size(ss));
   * else record_report(messages,"Simultaneous input and output is not enabled on this card.",NULL);
   */

#else
  monitor_fd = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_DAC_OUT,recorder_srate(ss),audio_out_chans,
				 recorder_out_format(ss),recorder_buffer_size(ss));
#endif
  if (monitor_fd == -1)
    {
      record_report(messages,"open output: ",mus_audio_error_name(mus_audio_error()),NULL);
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
      XmTextSetString(rec_size_text,timbuf);
    }
  unlock_recording_audio();
}

int record_dialog_is_active(void)
{
  return((recorder) && (XtIsManaged(recorder)));
}


#if 0
/* to ensure alReadBuffers: -- this might be handy when multiple active inputs are supported */
  pv.param = AL_VERSION; alGetParams(AL_SYSTEM,&pv,1); if (pv.sizeOut < 0 || pv.value.i < 6) {/* feature not present */}
  ALport p;
  short buf[8][1000];
  void *bufs[8];
  int i,j;
  ALconfig c;
  c = alNewConfig();
  alSetChannels(c, 8);
  p = alOpenPort("alReadBuffers example","r",c);
  for (i = 0; i < 8; i++) {bufs[i] = buf[i];}
  alReadBuffers(p, bufs, 0, 1000);          /* read 1000 8-channel frames */
#endif


void set_recorder_trigger(snd_state *ss, Float val)
{
  in_set_recorder_trigger(ss,val);
  if (recorder)
    {
      XmScaleSetValue(trigger_scale,(int)(100*val));
      internal_trigger_set(val);
    }
}

void set_recorder_max_duration(snd_state *ss, Float val)
{
  in_set_recorder_max_duration(ss,val);
  max_duration = val;
}

#if 0
void start_recorder(snd_state *ss)
{
  if (!recorder) snd_record_file(ss);
  Record_Button_Callback(record_button,(XtPointer)ss,NULL); /* call again to stop */
}
#endif

void set_recorder_srate(snd_state *ss, int val)
{
  char sbuf[8];
  /* this just reflects the setting in the text field -- it doesn't actually set anything in the audio system */
  if (val > 0)
    {
      /* SGI AES In sometimes claims its srate is 0 */
      in_set_recorder_srate(ss,val);
      if (recorder) 
	{
	  sprintf(sbuf,"%d",recorder_srate(ss));
	  XmTextSetString(recdat->srate_text,sbuf);
	}
    }
}
