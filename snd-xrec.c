/* TODO  multiple inputs in new sgi al on properly equipped machines (how do Indys behave in this case?)
 * TODO  smoothed amp sliders(?)
 * TODO  clicks upon swap or whatever (set process priority?)
 * TODO  on-going output sonogram (this is just an fft+running display)?
 * TODO  CD input if supported (i.e. on the Sun)
 * TODO  continuous record (flushing output until some indication)
 * TODO  output panel should support max chans found in all output devices, and have some way to send output anywhere
 * TODO  recorder-defaults device {function} -> layout, chans, whether included and active, etc (using sndlib device numbers)
 * TODO  dB in VUs
 * TODO  syncd sliders ("master" volume control basically)
 * TODO  in kde, recorder not re-drawn upon desktop switch
 * TODO  if mono microphone, both chans of analog in are seeing input?
 */

#include "snd.h"
#include "snd-rec.h"

#if HAVE_XPM
  #include <X11/xpm.h>
#endif

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

static char timbuf[TIME_STR_SIZE];
static char *msgbuf = NULL;

static file_data *recdat;

static VU **rec_in_VU = NULL;       /* from rec in to associated meter */
static VU **rec_out_VU = NULL;      /* from rec out to associated meter */
static Wdesc **gain_sliders = NULL;  /* associated sliders and descriptors for write audio state */
static AMP ***AMP_rec_ins = NULL;
static AMP **AMP_rec_outs = NULL;

static Widget recorder = NULL;      /* the outer dialog shell */

static PANE **all_panes = NULL;
static int out_file_pane;

static Widget rec_size_text,trigger_scale,trigger_label;
static Widget file_duration,messages,record_button,reset_button,file_text;
static Widget *device_buttons;
static int device_buttons_size = 0;
#if NEW_SGI_AL || defined(SUN)
  static int active_device_button = -1;
#endif
static int mixer_gains_posted[MAX_SOUNDCARDS];
static int tone_controls_posted[MAX_SOUNDCARDS];
static XM_FONT_TYPE small_fontlist;

static vu_label **vu_labels = NULL;
static int vu_labels_size = 0;
static int current_vu_label = 0;

static char **pending_error = NULL;
static int pending_errors = 0;
static int pending_error_size = 0;

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
  strftime(timbuf,TIME_STR_SIZE,"%H:%M:%S",localtime(&ts));
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

void recorder_error(char *msg)
{
  if (recorder)
    record_report(messages,msg,NULL);
}

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

static GC draw_gc,vu_gc;

/* -------------------------------- ROTATE TEXT -------------------------------- */
/* rotate and scale text */

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
  angle_in_radians = mus_degrees2radians(angle_in_degrees);
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
      rdeg = mus_degrees2radians(45-i*22.5);
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
	      rdeg = mus_degrees2radians(45-i*22.5-j*(90.0/20.0));
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

static void display_vu_meter(VU *vu)
{
  Float deg,rdeg,val;
  int nx0,nx1,ny0,ny1,redx,redy,bub0,bub1,i,j;
  Pixmap label = 0;
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
  rdeg = mus_degrees2radians(deg);
  nx0 = vu->center_x - (int)((Float)(vu->center_y - vu->light_y) / tan(mus_degrees2radians(deg+90)));
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

static VU *make_vu_meter(Widget meter, int light_x, int light_y, int center_x, int center_y, snd_state *ss, Float size)
{
  VU *vu;
  int i;
  vu_label *vl = NULL;
#if HAVE_XPM  
  int err = XpmSuccess;
#endif
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
  for (i=0;i<current_vu_label;i++)
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
	    vu_labels = (vu_label **)CALLOC(vu_labels_size,sizeof(vu_label *));
	  else vu_labels = (vu_label **)REALLOC(vu_labels,vu_labels_size * sizeof(vu_label *));
	}
      vu_labels[current_vu_label] = (vu_label *)CALLOC(1,sizeof(vu_label));
      vl = vu_labels[current_vu_label];
      current_vu_label++;
      vl->label_font = get_vu_font(ss,size);
      vl->size = size;
#if HAVE_XPM  
      if ((vl->size == 1.0) || (vl->size > 4.0) || (vl->size < .25))
	err = allocate_meter_2(recorder,vl);
      else allocate_meter_1(ss,vl);
      if (err != XpmSuccess) {vl->on_label = 0; vl->off_label = 0; vl->clip_label = 0;}
#else
      allocate_meter_1(ss,vl);
#endif
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
      sprintf(timbuf,"%.3f",val);
      set_label(vu->max_button,timbuf);
    }
}

void recorder_set_vu_in_val(int chan, MUS_SAMPLE_TYPE val) {set_vu_val(rec_in_VU[chan],MUS_SAMPLE_TO_FLOAT(val));}
void recorder_set_vu_out_val(int chan, MUS_SAMPLE_TYPE val) {set_vu_val(rec_out_VU[chan],MUS_SAMPLE_TO_FLOAT(val));}


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
  Float amp;
  recorder_info *rp;
  rp = get_recorder_info();
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
    rp->in_amps[ap->in][ap->out] = amp;
  else rp->out_amps[ap->out] = amp;
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
'Rp->Triggered Record' button, Snd waits until it\n\
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
      sprintf(buf,"%s %.3f","trigger:",val);
      s1 = XmStringCreate(buf,XmFONTLIST_DEFAULT_TAG);
      FREE(buf);
      XtVaSetValues(trigger_label,XmNlabelString,s1,NULL);
      XmStringFree(s1);
    }
}

static void internal_trigger_set(Float val)
{
  XmString s1;
  recorder_info *rp;
  rp = get_recorder_info();
  rp->trigger = val;
  rp->triggering = (val > 0.0);
  rp->triggered = (!rp->triggering);
  make_trigger_label(val);
  if (!(rp->recording)) /* else wait for current session to end (via click) */
    {
      s1 = XmStringCreate((char *)((rp->triggering) ? STR_Triggered_Record : STR_Record),XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(record_button,XmNlabelString,s1,NULL);
      XmStringFree(s1);
    }
}

static void change_trigger_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  /* if val=0, set record button normal label to 'Record', else 'Triggered Record' */
  XmScaleCallbackStruct *cb = (XmScaleCallbackStruct *)callData;
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

  recorder_info *rp;
  rp = get_recorder_info();
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
  if (button == rp->digital_in_button)
    {
      set_line_source(ss,on);
      if (on == (XmToggleButtonGetState(device_buttons[rp->microphone_button])))
	XmToggleButtonSetState(device_buttons[rp->microphone_button],!on,TRUE); 
      if (on == (XmToggleButtonGetState(device_buttons[rp->line_in_button])))
	XmToggleButtonSetState(device_buttons[rp->line_in_button],!on,TRUE); 
    }
  else
    {
      if (button == rp->microphone_button)
	{
	  if (on == (XmToggleButtonGetState(device_buttons[rp->digital_in_button])))
	    {
	      XmToggleButtonSetState(device_buttons[rp->digital_in_button],!on,TRUE); 
	      if (!(on == (XmToggleButtonGetState(device_buttons[rp->line_in_button]))))
		XmToggleButtonSetState(device_buttons[rp->line_in_button],on,TRUE); 
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
	      for (i=p->in_chan_loc,j=0;j<p->in_chans;j++,i++) rp->input_channel_active[i]=0;
	      if (active_device_button != button)
		{
		  /* XtVaGetValues(p->pane,XmNheight,&(p->pane_size),NULL); */
		  XtVaSetValues(p->pane,XmNpaneMaximum,1,NULL);
		  XtVaSetValues(p->pane,XmNpaneMinimum,1,XmNpaneMaximum,LOTSA_PIXELS,NULL);
		  XmToggleButtonSetState(device_buttons[active_device_button],FALSE,FALSE); 
		}
	      if (rp->taking_input) close_recorder_audio();
	    }
	  active_device_button = button;
	  p = all_panes[button];
	  for (i=p->in_chan_loc,j=0;j<p->in_chans;j++,i++) rp->input_channel_active[i]=1;
	  rp->input_channels[0] = p->in_chans;

	  /* if digital in, get its srate (and reflect in srate text) */
	  if ((p->device == MUS_AUDIO_AES_IN) || (p->device == MUS_AUDIO_ADAT_IN))
	    {
	      mus_audio_mixer_read(p->device,MUS_AUDIO_SRATE,0,val);
	      set_recorder_srate(rp,(int)val[0]);
	    }
	  rp->input_ports[0] = mus_audio_open_input(MUS_AUDIO_PACK_SYSTEM(0) | p->device,
					  rp->srate,rp->input_channels[0],rp->in_format,rp->buffer_size);
	  if (rp->input_ports[0] == -1)
	    record_report(messages,recorder_device_name(p->device),NULL);
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
						 rp->srate,rp->monitor_chans,rp->out_format,rp->buffer_size);
		  if (rp->monitor_port == -1)
		    {
		      record_report(messages,"open output",NULL);
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

static void autoload_file_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  recorder_info *rp;
  rp = get_recorder_info();
  rp->autoload = XmToggleButtonGetState(w);
}

#if (HAVE_OSS || HAVE_ALSA)
static void save_audio_settings_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  recorder_info *rp;
  rp = get_recorder_info();
  XmToggleButtonSetState(w,FALSE,FALSE);
  rp->mixer_settings_saved = 1;
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
  recorder_info *rp;
  rp = get_recorder_info();
  str = XmTextGetString(w); 
  if (str) 
    {
      n = string2int(str);
      if ((n>0) && (n != rp->srate))
	{
	  rp->srate = n;
	  recorder_set_audio_srate(get_global_state(),MUS_AUDIO_DEFAULT,rp->srate,0,rp->taking_input);
	}
      XtFree(str);
    }
}

static void Rec_Size_Changed_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  char *str;
  int n;
  recorder_info *rp;
  rp = get_recorder_info();
  str = XmTextGetString(w); 
  if (str) 
    {
      n = string2int(str);
      if ((n>0) && (n != rp->buffer_size)) set_record_size(n);
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

static void make_file_info_pane(snd_state *ss, recorder_info *rp, Widget file_pane, int ndevs)
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
  if (snd_strlen(rp->output_file) > 0) {XtSetArg(args[n],XmNvalue,rp->output_file); n++;}
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
  recdat = sndCreateFileDataForm(ss,ff_form,"data-form",args,n,TRUE,rp->output_header_type,rp->out_format,FALSE);
  XtVaGetValues(recdat->comment_text,XmNy,&pane_max,NULL);
  XtAddCallback(recdat->srate_text,XmNactivateCallback,Srate_Changed_Callback,(void *)ss);
#if defined(SGI)
  err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_MICROPHONE,MUS_AUDIO_SRATE,0,val);
  if (!err) rp->srate = val[0];
#endif
  sprintf(timbuf,"%d",rp->srate);
  XmTextSetString(recdat->srate_text,timbuf);
  sprintf(timbuf,"%d",rp->out_chans);
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
  sprintf(timbuf,"%d",rp->buffer_size);
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
  trigger_label = XtCreateManagedWidget("trigger:",xmLabelWidgetClass,file_form,args,n);
  XtAddCallback(trigger_label,XmNhelpCallback,trigger_help_Callback,ss);
  make_trigger_label(rp->trigger);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,trigger_label); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_OPPOSITE_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,trigger_label); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNvalue,(int)(100*(rp->trigger))); n++;
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
      if ((rp->ordered_devices[i] == MUS_AUDIO_DIGITAL_IN) || (rp->ordered_devices[i] == MUS_AUDIO_MICROPHONE))
	{XtSetArg(args[n],XmNindicatorType,XmONE_OF_MANY); n++;}
      else {XtSetArg(args[n],XmNindicatorType,XmN_OF_MANY); n++;}
#endif
#if NEW_SGI_AL || defined(SUN)
      if (recorder_input_device(rp->ordered_devices[i]))
	{XtSetArg(args[n],XmNindicatorType,XmONE_OF_MANY); n++;}
      else {XtSetArg(args[n],XmNindicatorType,XmN_OF_MANY); n++;}
#endif
      if ((rp->systems == 1) || (!(recorder_input_device(rp->ordered_devices[i]))))
	name = recorder_device_name(rp->ordered_devices[i]);
      else name = recorder_system_and_device_name(rp->ordered_systems[i],rp->ordered_devices[i]);
      device_buttons[i] = sndCreateToggleButtonWidget(name,button_holder,args,n);
      XtAddCallback(device_buttons[i],XmNhelpCallback,button_holder_help_callback,ss);
      XtAddCallback(device_buttons[i],XmNvalueChangedCallback,device_button_callback,(XtPointer)all_panes[i]);
      XmToggleButtonSetState(device_buttons[i],TRUE,FALSE); 
    }
  autoload_file = sndCreateToggleButtonWidget(STR_Autoload_Recording,button_holder,args,init_n);
  device_buttons[ndevs] = autoload_file;
  /* we assume this is last in the device_buttons list in sensitize_control_buttons */
  rp->autoload_button = ndevs;
  XtAddCallback(autoload_file,XmNhelpCallback,autoload_file_help_callback,ss);
  XtAddCallback(autoload_file,XmNvalueChangedCallback,autoload_file_callback,ss);
  XmToggleButtonSetState(autoload_file,rp->autoload,FALSE); 
#if (HAVE_OSS || HAVE_ALSA)
  save_audio_settings = sndCreateToggleButtonWidget(STR_Save_Audio_Settings,button_holder,args,n);
  XtAddCallback(save_audio_settings,XmNvalueChangedCallback,save_audio_settings_callback,NULL);
  XtAddCallback(save_audio_settings,XmNhelpCallback,save_audio_settings_help_callback,ss);
#endif
}

void reflect_recorder_duration(Float new_dur)
{
  sprintf(timbuf,"%.2f",new_dur);
  set_label(file_duration,timbuf);
}

void lock_recording_audio(void)
{
  if (record_dialog_is_active())
    {
      close_recorder_audio();
      set_sensitive(record_button,FALSE);
      set_sensitive(reset_button,FALSE);
    }
}

void unlock_recording_audio(void)
{
  XmString s1;
  if (record_dialog_is_active())
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



static void VU_Max_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  Wdesc *wd = (Wdesc *)clientData;
  ssnd_help(wd->ss,
	   "Max Amp Indicator",
	    "This number indicates the max amp encountered\n\
since the last reset in ",
	    pane_device_name(wd->p->device),"'s",
	    " channel ",
	    channel_name(wd->p->in_chans,wd->p->out_chans,wd->chan),
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
	    pane_device_name(wd->p->device),"'s",
	    " channel ",
	    channel_name(wd->p->in_chans,wd->p->out_chans,wd->chan),
	    " to be included\n\
in or removed from the recording. The button\n\
is red when the signal is active.\n",
	    NULL);
}

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
	      out_channel_name(channel),
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
			  channel_name(wd->p->in_chans,wd->p->out_chans,channel),
			  ".\n",
			  NULL);
	      else
		ssnd_help(wd->ss,
			  "Linux Mixer Settings",
"This bank of sliders affects the OSS 'mixer'; this\n\
particular slider claims to control channel ",
			  channel_name(wd->p->in_chans,wd->p->out_chans,channel),
			  "'s\n",
			  recorder_field_name(field),
" field, which I believe has something to do with\n",
			  recorder_field_function(field),
			  ".\n",
			  NULL);
	    }
	}
    }
#else
  ssnd_help(wd->ss,
	    recorder_device_name(wd->p->device),
	    "This slider sets the ",
	    (recorder_input_device(wd->p->device)) ? "gain" : "volume",
	    " of channel ",
	    channel_name(wd->p->in_chans,wd->p->out_chans,wd->chan),
	    " of ",
	    pane_device_name(wd->p->device),
	    "\n",
	    (recorder_input_device(((PANE *)(wd->p))->device)) ? "This scales the in-coming signal before it\n\
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
	    (recorder_input_device(((PANE *)(wd->p))->device)) ? "the contribution of " : "",
	    pane_device_name(wd->p->device),"'s\n",
	    "channel ",
	    channel_name(wd->p->in_chans,wd->p->out_chans,a->in),
	    (recorder_input_device(((PANE *)(wd->p))->device)) ? " to the output file's channel " : ".",
	    (recorder_input_device(((PANE *)(wd->p))->device)) ? (out_channel_name(a->out)) : "",
	    NULL);
}

static void Meter_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  Wdesc *wd = (Wdesc *)clientData;
  ssnd_help(wd->ss,
	    "VU Meter",
	    "This meter shows the current volume of ",
	    pane_device_name(wd->p->device),"'s",
	    " channel ",
	    channel_name(wd->p->in_chans,wd->p->out_chans,wd->chan),
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
  recorder_info *rp;
  rp = get_recorder_info();
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
  if (recorder_output_device(p->device))
    {
      rp->chan_out_active[wd->chan] = val;
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
	   rp->out_chans = val;
	   */
#endif
	}
    }
  else rp->chan_in_active[wd->gain] = val;
}

static void volume_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  Wdesc *wd = (Wdesc *)clientData;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)callData;
  set_mixer_gain(wd->system,wd->device,wd->chan,wd->gain,wd->field,(Float)cbs->value/100.0);
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
  a->label = sndCreatePushButtonWidget(gain_channel_name(p->in_chans,p->out_chans,input,a->device_in_chan,a->out),p->pane,args,n);
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

/* these functions are used only by make_pane */
static Position make_amp_sliders(snd_state *ss, recorder_info *rp, PANE *p, Widget first_frame, int input, int overall_input_ctr);
static void make_reset_button(snd_state *ss, PANE *p, Float meter_size, Widget button_box, Widget vu_vertical_sep);
static Widget make_button_box(snd_state *ss, recorder_info *rp, PANE *p, Float meter_size,
			      int input, int overall_input_ctr, int vu_meters, Widget vu_vertical_sep, Widget *frames);
static void make_gain_separator(snd_state *ss, PANE *p, int num_gains, int vu_meters, Widget last_slider);
static Widget make_vertical_gain_sliders(snd_state *ss, recorder_info *rp, PANE *p, 
					 int num_gains, int gain_ctr, int *mixflds, int input);
static Widget make_vertical_gain_separator(snd_state *ss, PANE *p, int vu_meters, Widget last_frame);
static Widget make_vu_meters(snd_state *ss, PANE *p, int vu_meters, Widget *frames, Widget in_last_frame, Widget in_left_frame,
			   int overall_input_ctr, Float meter_size, int input, Widget *out_frame);

static PANE *make_pane(snd_state *ss, recorder_info *rp, Widget paned_window, int device, int system)
{
  /* VU meters (frame, then drawing area widget) */
  /* Linux OSS complication -- the top input panel also has all the "mixer" input volume controls and the output pane has the tone controls, if any */
  int n,i,k;
  Arg args[32];
  PANE *p;
  Widget *frames = NULL;
  Widget last_frame,vu_vertical_sep,last_slider=NULL,matrix_frame,
    button_box,left_frame;
  Widget first_frame[1];
  int vu_meters,num_gains,input;
  Position pane_max;
  Float meter_size;
  int mixflds[MAX_AUDIO_FIELD];
  static int overall_input_ctr = 0;
  static int gain_ctr = 0;

  p = (PANE *)CALLOC(1,sizeof(PANE));
  p->device = device;
  p->system = system;
  p->ss = ss;
  vu_meters = recorder_check_device(system,device,mixer_gains_posted,tone_controls_posted,mixflds,&num_gains,&input);

  if (input) 
    {
      p->in_chans = vu_meters;
      p->out_chans = rp->out_chans;
      /* this determines how many of the left-side buttons we get if chans>4; if defaults to 2 (snd.c)
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
  left_frame = NULL;
  meter_size = vu_size(ss);
  if (vu_meters > 4) meter_size *= .6; else if (vu_meters > 2) meter_size *= .8;
  if ((vu_meters%5) == 0) meter_size *= 0.8;

  if ((input) && ((p->in_chans*p->out_chans) > 8))
    {
      for (i=0;i<p->in_chans;i++) 
	for (k=0;k<p->out_chans;k++) 
	  if (i == k) p->active_sliders[i][k] = 1;

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
    }

  last_frame = make_vu_meters(ss,p,vu_meters,frames,last_frame,left_frame,overall_input_ctr,meter_size,input,first_frame);
  
  /* if no audio (hardware) gains, we have the vu separator and the control buttons */
  vu_vertical_sep = make_vertical_gain_separator(ss,p,vu_meters,last_frame);
  if (num_gains > 0)
    {
      last_slider = make_vertical_gain_sliders(ss,rp,p,num_gains,gain_ctr,mixflds,input);
      gain_ctr += num_gains;
    }

  /* separator between vertical sliders and buttons */
  make_gain_separator(ss,p,num_gains,vu_meters,last_slider);
  
  /* control buttons with label */
  button_box = make_button_box(ss,rp,p,meter_size,input,overall_input_ctr,vu_meters,vu_vertical_sep,frames);
  if (frames) {FREE(frames); frames = NULL;}
  make_reset_button(ss,p,meter_size,button_box,vu_vertical_sep);
  
  /* now the amp sliders across the bottom of the pane, with 'mixer' info on the right */
  pane_max = make_amp_sliders(ss,rp,p,first_frame[0],input,overall_input_ctr);

  p->in_chan_loc = overall_input_ctr;

#if NEW_SGI_AL
  if (p->device == MUS_AUDIO_MICROPHONE)
    for (i=p->in_chan_loc,k=0;k<p->in_chans;i++,k++) 
      rp->input_channel_active[i] = 1;
#endif

  if (input) overall_input_ctr += p->in_chans;
#if (HAVE_OSS || HAVE_ALSA)
  p->pane_size = pane_max+20;
#else
  p->pane_size = pane_max+50;
#endif

  return(p);
}

static Widget make_vu_meters(snd_state *ss, PANE *p, int vu_meters, Widget *frames, Widget in_last_frame, Widget in_left_frame,
			   int overall_input_ctr, Float meter_size, int input, Widget *out_frame)
{
  int i,n,columns,row;
  Widget first_frame = NULL,frame,meter,last_frame,left_frame;
  Wdesc *wd;
  VU *vu;
  Arg args[32];

  last_frame = in_last_frame;
  left_frame = in_left_frame;

  columns = recorder_columns(vu_meters);
  row = 0;

  for (i=0;i<vu_meters;i++)
    {
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->black); n++;}
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
      XtSetArg(args[n],XmNbackground,(ss->sgx)->white); n++;
      XtSetArg(args[n],XmNforeground,(ss->sgx)->black); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNwidth,CENTER_X*2*meter_size); n++;
      XtSetArg(args[n],XmNheight,LIGHT_Y*meter_size); n++;
      XtSetArg(args[n],XmNallowResize,FALSE); n++;
      meter = sndCreateDrawingAreaWidget("vu",frame,args,n);
      p->meters[i] = make_vu_meter(meter,LIGHT_X,LIGHT_Y,CENTER_X,CENTER_Y,ss,meter_size);
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
      wd->device = p->device;
      wd->system = p->system;
      XtAddCallback(meter,XmNhelpCallback,Meter_Help_Callback,wd);
      last_frame = frame;
      if ((i == (columns*(row+1) - 1)) && (vu_meters > (i+1))) 
	{
	  last_frame = NULL;
	  first_frame = NULL;
	  row++;
	}
    }
  out_frame[0] = first_frame;
  return(last_frame);
}

static Widget make_vertical_gain_separator(snd_state *ss, PANE *p, int vu_meters, Widget last_frame)
{
  int n;
  Arg args[32];

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
  return(XtCreateManagedWidget("sep",xmSeparatorWidgetClass,p->pane,args,n));
}

void recorder_fill_wd(void *uwd, int chan, int field, int device)
{
  Wdesc *wd = (Wdesc *)uwd;
  wd->chan = chan;
  wd->field = field;
  wd->device = device;
}

static Widget make_vertical_gain_sliders(snd_state *ss, recorder_info *rp, PANE *p, 
					 int num_gains, int gain_ctr, int *mixflds, int input)
{
  /* vertical scalers on the right (with icon) */
  int n,i,chan,this_device=0,last_device=-1;
  Arg args[32];
  Widget icon_label, last_slider;
  Wdesc *wd;
  XmString slabel = NULL;
  Float vol;

#if (HAVE_OSS || HAVE_ALSA)
  last_device = MUS_AUDIO_MICROPHONE;
#endif

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNlabelType,XmPIXMAP); n++;
#if (HAVE_OSS || HAVE_ALSA)
  if (input)
    {XtSetArg(args[n],XmNlabelPixmap,device_icon((mixflds[MUS_AUDIO_MICROPHONE]>0) ? MUS_AUDIO_MICROPHONE : MUS_AUDIO_LINE_IN)); n++;}
  else {XtSetArg(args[n],XmNlabelPixmap,device_icon(p->device)); n++;}
  if (input)
    {
      if (mixflds[MUS_AUDIO_MICROPHONE]<=0)
	last_device = MUS_AUDIO_LINE;
    }
  else last_device = MUS_AUDIO_DAC_OUT;
#else
  XtSetArg(args[n],XmNlabelPixmap,device_icon(p->device)); n++;
#endif
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  if (num_gains > 1)
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
  for (i=0,chan=num_gains-1;i<num_gains;i++,chan--)
    {
      /* we're moving right to left here, so the first slider represents the highest channel */
      /* in the Linux case, as each new device pops up, we need some indication above it */
      wd = (Wdesc *)CALLOC(1,sizeof(Wdesc));
      wd->system = p->system;
      this_device = recorder_sort_mixer_device((void *)wd,i,chan,input,p->device,mixflds);
      wd->ss = ss;
      wd->p = p;
      wd->gain = gain_ctr+chan;
      if (wd->gain > rp->num_mixer_gains) 
	snd_error("%s[%d] %s: overflow %d > %d",__FILE__,__LINE__,__FUNCTION__,wd->gain,rp->num_mixer_gains);
      gain_sliders[wd->gain] = wd;
      vol = mixer_gain(wd->system,wd->device,wd->chan,wd->gain,wd->field);
      if (vol < 0.0) vol = 0.0;
      if (vol > 1.0) vol = 1.0;

#if (HAVE_OSS || HAVE_ALSA)
      if (last_device != this_device)
	{
	  n=0;
	  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
	  if (this_device == MUS_AUDIO_LINE_IN)
	    {
	      XtSetArg(args[n],XmNlabelType,XmPIXMAP); n++;
	      XtSetArg(args[n],XmNlabelPixmap,device_icon(MUS_AUDIO_LINE_IN)); n++;
	    }
	  else
	    {
	      if ((!input) && (this_device == MUS_AUDIO_DAC_FILTER))
		slabel = XmStringCreate("ton","small_font");
	      else slabel = XmStringCreate(recorder_field_abbreviation(this_device),"small_font");
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
	  if ((this_device != MUS_AUDIO_LINE_IN) && (slabel)) {XmStringFree(slabel); slabel=NULL;}
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
      XtSetArg(args[n],XmNwidth,15); n++;

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
  return(last_slider);
}

static void make_gain_separator(snd_state *ss, PANE *p, int num_gains, int vu_meters, Widget last_slider)
{
  int n;
  Arg args[32];

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_NONE); n++;
  if (num_gains > 0)
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
}

static Widget make_button_box(snd_state *ss, recorder_info *rp, PANE *p, Float meter_size,
			      int input, int overall_input_ctr, int vu_meters, Widget vu_vertical_sep, Widget *frames)
{
  int i,n,row,columns,button_size;
  Arg args[32];
  Widget button_label,button1_label,last_button,last_max,max_label,button_box;
  Wdesc *wd;
  VU *vu;

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,vu_vertical_sep); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNrightWidget,p->button_vertical_sep); n++;
  if (meter_size<SMALL_FONT_CUTOFF) {XtSetArg(args[n],XM_FONT_RESOURCE,small_fontlist); n++;}
  if ((rp->systems == 1) || (!input))
    button_label = XtCreateManagedWidget(recorder_device_name(p->device),xmLabelWidgetClass,p->pane,args,n);
  else 
    {
      button1_label = XtCreateManagedWidget(mus_audio_system_name(p->system),xmLabelWidgetClass,p->pane,args,n);
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
      button_label = XtCreateManagedWidget(recorder_device_name(p->device),xmLabelWidgetClass,p->pane,args,n);
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

  columns = recorder_columns(vu_meters);

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
      p->on_buttons[i] = XtCreateManagedWidget(channel_name(p->in_chans,p->out_chans,i),xmPushButtonWidgetClass,button_box,args,n);
      last_button = p->on_buttons[i];
      wd = (Wdesc *)CALLOC(1,sizeof(Wdesc));
      wd->chan = i;
      wd->gain = i + overall_input_ctr;
      wd->ss = ss;
      wd->p = p;
      wd->device = p->device;
      wd->system = p->system;
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
      wd->system = p->system;
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
  return(button_box);
}

static void make_reset_button(snd_state *ss, PANE *p, Float meter_size, Widget button_box, Widget vu_vertical_sep)
{
  XmString labelstr;
  int n;
  Arg args[32];

  if (meter_size<SMALL_FONT_CUTOFF)
    labelstr = XmStringCreate(STR_Reset,"small_font");
  else labelstr = XmStringCreate(STR_Reset,XmFONTLIST_DEFAULT_TAG);
  n=0;
  if (!(ss->using_schemes)) 
    {
      XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;
      XtSetArg(args[n],XmNarmColor,(ss->sgx)->pushed_button_color); n++;
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
}

static Position make_amp_sliders(snd_state *ss, recorder_info *rp, PANE *p, Widget first_frame, int input, int overall_input_ctr)
{
  Widget last_slider,slider_sep;
  int i,n,amp_sliders,temp_out_chan,temp_in_chan,out_chan,in_chan,system;
  Position pane_max;
  Arg args[32];
  AMP *a;
  Wdesc *wd;
  system = p->system;
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
  return(pane_max);
}

void sensitize_control_buttons(void)
{
  int i;
  for (i=0;i<device_buttons_size-1;i++) /* last button is autoload_file */
    {
      if (device_buttons[i])
	set_sensitive(device_buttons[i],TRUE);
    }
}

void unsensitize_control_buttons(void)
{
  int i;
  for (i=0;i<device_buttons_size-1;i++)
    {
      if (device_buttons[i])
	set_sensitive(device_buttons[i],FALSE);
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
  recorder_info *rp;
  rp = get_recorder_info();
  if (rp->recording)                  /* cancel */
    {
      rp->recording = 0;
      rp->triggered = (!rp->triggering);
      sensitize_control_buttons();
      XmChangeColor(record_button,(Pixel)(ss->sgx)->basic_color);
      s1 = XmStringCreate(STR_Reset,XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(reset_button,XmNlabelString,s1,NULL);
      XmStringFree(s1);
      s1 = XmStringCreate((char *)((rp->triggering) ? STR_Triggered_Record : STR_Record),XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(record_button,XmNlabelString,s1,NULL);
      XmStringFree(s1);
      snd_close(rp->output_file_descriptor);
      rp->output_file_descriptor = -1;
      str = just_filename(rp->output_file);
      record_report(messages,str," recording cancelled",NULL);
      FREE(str);
      remove(rp->output_file);
    }
  else                            /* reset or restart */
    { 
      for (i=0;i<rp->ordered_devices_size;i++)
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
      if (!(rp->taking_input))            /* restart */
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
  recorder_info *rp;
  rp = get_recorder_info();
  sgx = ss->sgx;
  if (cb->event != sgx->text_activate_event)  /* passed up from textfield widget after <cr> typed */
    {
      if (rp->recording) Reset_Record_Callback(w,clientData,callData);
      XtUnmanageChild(recorder);
      close_recorder_audio();
#if (!(HAVE_OSS || HAVE_ALSA))
      mus_audio_restore();
#endif
    }
}

void finish_recording(snd_state *ss, recorder_info *rp)
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
  s2 = XmStringCreate((char *)((rp->triggering) ? STR_Triggered_Record : STR_Record),XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(record_button,XmNlabelString,s2,NULL);
  XmStringFree(s2);
  snd_close(rp->output_file_descriptor);
  rp->output_file_descriptor = mus_file_reopen_write(rp->output_file);
  mus_header_update_with_fd(rp->output_file_descriptor,
			    rp->output_header_type,
			    rp->total_output_frames*rp->out_chans*mus_data_format_to_bytes_per_sample(rp->out_format));
  close(rp->output_file_descriptor);
  rp->output_file_descriptor = -1;
  duration = (Float)rp->total_output_frames / (Float)(rp->srate);
  /* 25-Jun-00: this used to divide by chans, but rp->total_output_frames is in terms of frames (it was named total_out_samps) */
  reflect_recorder_duration(duration);
  str = (char *)CALLOC(256,sizeof(char));
  sprintf(str,"recorded %s:\n  duration: %.2f\n  srate: %d, chans: %d\n  type: %s, format: %s",
	  rp->output_file,duration,rp->srate,rp->out_chans,
	  mus_header_type_name(rp->output_header_type),mus_data_format_name(rp->out_format));
  record_report(messages,str,NULL);
  FREE(str);
  if (rp->autoload)
    {
      if ((sp = find_sound(ss,rp->output_file)))
	snd_update(ss,sp);
      else snd_open_file(rp->output_file,ss);
    }
}

static void Record_Button_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  XmString s1 = NULL,s2 = NULL;
  Wdesc *wd;
  int i,old_srate,ofmt,rs,ochns,oloc;
  static char *comment;
  char *str;
  PANE *p;
  recorder_info *rp;
  rp = get_recorder_info();
  rp->recording = (!(rp->recording));
  if (rp->recording)
    {
      if (!(rp->taking_input)) fire_up_recorder(ss);
      str = XmTextGetString(file_text);
      if ((str) && (*str))
	{
	  rp->output_file = mus_file_full_name(str);
	  XtFree(str);
	  str=NULL;
	  old_srate = rp->srate;
	  read_file_data_choices(recdat,&rs,&ochns,&rp->output_header_type,&ofmt,&oloc); 
	  rp->out_format = ofmt;
	  rp->out_chans = ochns;
	  if (rs != old_srate) 
	    {
	      rp->srate = rs;
	      recorder_set_audio_srate(ss,MUS_AUDIO_DEFAULT,rp->srate,0,rp->taking_input);
	    }
	  if (rp->out_chans <= 0)
	    {
	      record_report(messages,"can't record: you screwed up the output channel number!",NULL);
	      rp->recording = 0;
	      rp->triggered = (!rp->triggering);
	      return;
	    }
	  comment = XmTextGetString(recdat->comment_text);
	  reflect_recorder_duration(0.0);
	  
	  if (out_chans_active() != rp->out_chans)
	    {
	      if (msgbuf == NULL) msgbuf = (char *)CALLOC(512,sizeof(char));
	      sprintf(msgbuf,"chans field (%d) doesn't match file out panel (%d channels active)",rp->out_chans,out_chans_active());
	      record_report(messages,msgbuf,NULL);
	      wd = (Wdesc *)CALLOC(1,sizeof(Wdesc));
	      wd->ss = ss;

	      wd->p = all_panes[out_file_pane];
	      p = wd->p;
	      wd->field = MUS_AUDIO_AMP;
	      wd->device = p->device;
	      wd->system = 0;
	      for (i=0;i<rp->out_chans;i++)
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
	      record_report(messages,"can't record: no inputs enabled",NULL);
	      rp->recording = 0;
	      rp->triggered = (!rp->triggering);
	      return;
	    }
	  XmChangeColor(w,(Pixel)(ss->sgx)->red);
	  s1 = XmStringCreate(STR_Cancel,XmFONTLIST_DEFAULT_TAG);
	  XtVaSetValues(reset_button,XmNlabelString,s1,NULL);
	  XmStringFree(s1);
	  s2 = XmStringCreate(STR_Done,XmFONTLIST_DEFAULT_TAG);
	  XtVaSetValues(record_button,XmNlabelString,s2,NULL);
	  XmStringFree(s2);

	  if (recorder_start_output_file(ss,comment)) return; /* true = error */
	}
      else
	{
	  record_report(messages,"can't record: no output file name supplied",NULL);
	  rp->recording = 0;
	  rp->triggered = (!rp->triggering);
	  return;
	}
    }
  else 
    finish_recording(ss,rp);
}

static void initialize_recorder(recorder_info *rp);
static Widget rec_panes,message_pane,file_info_pane;

void snd_record_file(snd_state *ss)
{
  Arg args[32];
  int n,i,device,input_devices,output_devices,system;
  XmString xdismiss,xhelp,xreset,titlestr;
  XFontStruct *small_fontstruct;
  Atom wm_delete;
  XGCValues v;
  Drawable wn;
  state_context *sx;
  PANE *p;
  recorder_info *rp;
  rp = get_recorder_info();

  if (!recorder)
    {
      sx = ss->sgx;
      wn = XtWindow(MAIN_PANE(ss));
      v.background = sx->basic_color;
      v.foreground = sx->black;
      draw_gc = XCreateGC(MAIN_DISPLAY(ss),wn,GCForeground | GCBackground,&v);
      v.background = sx->white;
      v.foreground = sx->black;
      vu_gc = XCreateGC(MAIN_DISPLAY(ss),wn,GCForeground | GCBackground,&v);

      input_devices = recorder_get_devices(rp,&output_devices);
      if (input_devices == -1) return;

      all_panes = (PANE **)CALLOC(input_devices+1,sizeof(PANE *));
      device_buttons_size = input_devices + 2; /* inputs, one output, autoload_file */
      device_buttons = (Widget *)CALLOC(device_buttons_size,sizeof(Widget));
      gain_sliders = (Wdesc **)CALLOC(rp->num_mixer_gains,sizeof(Wdesc *));
      /* out_file_pane will be the bottom (output) audio pane, not the file info pane */

      recorder_characterize_devices(input_devices+1,output_devices);

      rec_in_VU = (VU **)CALLOC(rp->possible_input_chans,sizeof(VU *));
      rec_out_VU = (VU **)CALLOC(MAX_OUT_CHANS,sizeof(VU *));

      AMP_rec_ins = (AMP ***)CALLOC(rp->possible_input_chans,sizeof(AMP **));
      for (i=0;i<rp->possible_input_chans;i++) 
	AMP_rec_ins[i] = (AMP **)CALLOC(MAX_OUT_CHANS,sizeof(AMP *));
      AMP_rec_outs = (AMP **)CALLOC(MAX_OUT_CHANS,sizeof(AMP *));

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

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNlabelString,titlestr); n++;
      record_button = XtCreateManagedWidget("record-button",xmPushButtonWidgetClass,recorder,args,n);
      XtAddCallback(record_button,XmNactivateCallback,Record_Button_Callback,ss);
      XmStringFree(titlestr);

      reset_button = XtNameToWidget(recorder,"Cancel");

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
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNbottomWidget,XmMessageBoxGetChild(recorder,XmDIALOG_SEPARATOR)); n++;
      XtSetArg(args[n],XmNallowResize,TRUE); n++;
      XtSetArg(args[n],XmNsashHeight,ss->sash_size); n++;
      XtSetArg(args[n],XmNsashWidth,ss->sash_size); n++;
      XtSetArg(args[n],XmNsashIndent,-1); n++; /* need room for icons -- default is -10 */
      rec_panes = sndCreatePanedWindowWidget("rec-panes",recorder,args,n);

      XtManageChild(recorder);
      make_record_icons(recorder,ss);

      /* open all audio devices and collect ins/out etc */
      out_file_pane = (rp->ordered_devices_size - 1);

      /* see note above (line 2809) about recorder out chans */
      n = device_channels(MUS_AUDIO_PACK_SYSTEM(rp->ordered_systems[out_file_pane]) | rp->ordered_devices[out_file_pane]);
      if ((rp->out_chans == DEFAULT_RECORDER_OUT_CHANS) && (n > 2))
	rp->out_chans = n;

      for (i=0;i<rp->ordered_devices_size;i++)
	{
	  /* last one is output */
	  device = rp->ordered_devices[i];
	  system = rp->ordered_systems[i];
	  all_panes[i] = make_pane(ss,rp,rec_panes,device,system);
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

      make_file_info_pane(ss,rp,file_info_pane,rp->ordered_devices_size);
      messages = make_message_pane(ss,message_pane);
      if (!(ss->using_schemes)) map_over_children(rec_panes,color_sashes,(void *)ss);

      /* loop through all panes reading p->pane_size and */
      for (i=0;i<rp->ordered_devices_size;i++)
	{
	  p = all_panes[i];
	  XtVaSetValues(p->pane,XmNpaneMaximum,p->pane_size,NULL);
	}

      /* in case caller closes (via window menu) dialog: */
      wm_delete = XmInternAtom(XtDisplay(recorder),"WM_DELETE_WINDOW",FALSE);
      XmAddWMProtocolCallback(XtParent(recorder),wm_delete,RecordCleanupCB,(XtPointer)ss);

      initialize_recorder(rp);
    }
  else 
    {
      raise_dialog(recorder);
    }
  if (!XtIsManaged(recorder)) XtManageChild(recorder);
  XtVaSetValues(message_pane,XmNpaneMinimum,1,NULL);
  for (i=0;i<rp->ordered_devices_size;i++)
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
  
  if (!(rp->taking_input)) fire_up_recorder(ss);
}

void set_recorder_autoload(recorder_info *rp, int val)
{
  rp->autoload = val;
  if (recorder) XmToggleButtonSetState(device_buttons[rp->autoload_button],val,FALSE); 
}

void reflect_recorder_in_amp(int in, int out, Float val)
{
  int temp;
  if (recorder)
    {
      temp = amp_to_slider(val); 
      record_amp_changed(AMP_rec_ins[in][out],temp); 
      XtVaSetValues(AMP_rec_ins[in][out]->slider,XmNvalue,temp,NULL); 
    }
}

void reflect_recorder_out_amp(int ind, Float val)
{
  int temp;
  if (recorder)
    {
      temp = amp_to_slider(val); 
      record_amp_changed(AMP_rec_outs[ind],temp); 
      XtVaSetValues(AMP_rec_outs[ind]->slider,XmNvalue,temp,NULL); 
    }
}

void reflect_recorder_mixer_gain(int ind, Float val)
{
  Wdesc *wd;
  if (recorder)
    {
      wd = gain_sliders[ind];
      XtVaSetValues(wd->wg,XmNvalue,(int)(val * 100),NULL);
      set_mixer_gain(wd->system,wd->device,wd->chan,wd->gain,wd->field,val);
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
  for (i=0;i<MAX_SOUNDCARDS;i++)
    {
      rp->raw_input_bufs[i] = NULL;
#if (HAVE_OSS || HAVE_ALSA)
      mixer_gains_posted[i] = 0;
      tone_controls_posted[i] = 0;
#endif
    }
#if OLD_SGI_AL
  sb[0] = AL_INPUT_SOURCE;
  err = ALgetparams(AL_DEFAULT_DEVICE,sb,2);
  if ((!err) && (sb[1] == AL_INPUT_DIGITAL)) in_digital = 1;
  XmToggleButtonSetState(device_buttons[rp->digital_in_button],in_digital,FALSE);
  device_button_callback(device_buttons[rp->digital_in_button],(XtPointer)all_panes[rp->digital_in_button],NULL);
#endif
#if NEW_SGI_AL || defined(SUN)
  /* for simplicity, and until someone complains, new SGI AL machines will just have one active input device */
  active_device_button = rp->microphone_button;
  for (i=0;i<device_buttons_size-1;i++)
    {
      if (device_buttons[i])
	{
	  if ((i != rp->microphone_button) && (recorder_input_device(all_panes[i]->device)))
	    {
	      XmToggleButtonSetState(device_buttons[i],FALSE,TRUE); 
	    }
	}
    }
#endif
  if (rp->trigger != 0.0) set_recorder_trigger(rp,rp->trigger);
  if (rp->max_duration<=0.0) rp->max_duration = 1000000.0;
}


void reflect_record_size(int size)
{
  if ((recorder) && (rec_size_text)) 
    {
      sprintf(timbuf,"%d",size);
      XmTextSetString(rec_size_text,timbuf);
    }
}

int record_dialog_is_active(void)
{
  return((recorder) && (XtIsManaged(recorder)));
}


void set_recorder_trigger(recorder_info *rp, Float val)
{
  rp->trigger = val;
  if (recorder)
    {
      XmScaleSetValue(trigger_scale,(int)(100*val));
      internal_trigger_set(val);
    }
}

void set_recorder_srate(recorder_info *rp, int val)
{
  char sbuf[8];
  /* this just reflects the setting in the text field -- it doesn't actually set anything in the audio system */
  if (val > 0)
    {
      /* SGI AES In sometimes claims its srate is 0 */
      rp->srate = val;
      if (recorder) 
	{
	  sprintf(sbuf,"%d",rp->srate);
	  XmTextSetString(recdat->srate_text,sbuf);
	}
    }
}
