/* transform settings dialog */

#include "snd.h"

static Widget transform_dialog = NULL; /* main dialog shell */
static Widget type_list,
              size_list,
              db_button,peaks_button,logfreq_button,sono_button,spectro_button,normo_button,normalize_button,selection_button,
              window_list,window_beta_scale,
              wavelet_list,
              graph_label,graph_drawer;
static GC gc,fgc;
static int fft_window_choice;

#define GRAPH_SIZE 128
static Float current_graph_data[GRAPH_SIZE]; /* fft window graph in transform options dialog */
static Float current_graph_fftr[GRAPH_SIZE*2];
static Float current_graph_ffti[GRAPH_SIZE*2];

#define NUM_FFT_SIZES 14
static char *FFT_SIZES[NUM_FFT_SIZES] = {"16","32","64","128","256","512","1024","2048","4096","8192","16384","65536","262144","1048576    "};
static int fft_sizes[NUM_FFT_SIZES] = {16,32,64,128,256,512,1024,2048,4096,8192,16384,65536,262144,1048576};

static char *FFT_WINDOWS[NUM_FFT_WINDOWS] = 
     {"rectangular","hanning","welch","parzen","bartlett","hamming","blackman2","blackman3","blackman4",
      "exponential","riemann","kaiser","cauchy","poisson","gaussian","tukey"};

static char *WAVELETS[NUM_WAVELETS]={
  "daub4","daub6","daub8","daub10","daub12","daub14","daub16","daub18","daub20",
  "battle_lemarie","burt_adelson","beylkin","coif2","coif4","coif6",
  "sym2","sym3","sym4","sym5","sym6"};

#define NUM_TRANSFORM_TYPES 8
static char *TRANSFORM_TYPES[NUM_TRANSFORM_TYPES]={"Fourier","Wavelet","Hankel","Walsh","Autocorrelate","Chebychev","Cepstrum","Hadamard"};
static int num_transform_types = NUM_TRANSFORM_TYPES;

#if HAVE_GUILE
static char *TRANSFORM_TYPE_CONSTANTS[NUM_TRANSFORM_TYPES]={
  S_fourier_transform,S_wavelet_transform,S_hankel_transform,S_walsh_transform,
  S_autocorrelation,S_chebyshev_transform,S_cepstrum,S_hadamard_transform};

char *transform_type_name(snd_state *ss) 
{
  if (transform_type(ss) < NUM_TRANSFORM_TYPES)
    return(TRANSFORM_TYPE_CONSTANTS[transform_type(ss)]);
  else return(added_transform_name(transform_type(ss)));
}

int max_transform_type(void) {return(num_transform_types - 1);}
#endif

static int set_ffts_size (chan_info *cp, void *p_size)
{
  fft_info *fp;
  int flen;
  if (cp->fft) 
    {
      flen = (*((int *)p_size));
      fp=cp->fft;
      if (fp->size < flen) fp->ok = 0; /* "dirty" flag for fft data array = needs REALLOCation */
      fp->size = flen;
    }
  return(0);
}

static int set_ffts_window (chan_info *cp, void *fd)
{
  if (cp->fft) (cp->fft)->window = fft_window_choice;
  return(0);
}

static int force_fft_clear(chan_info *cp, void *ptr)
{
  if ((cp->cgx) && ((cp->cgx)->fft_in_progress))
    {
      XtRemoveWorkProc((cp->cgx)->fft_in_progress);
      finish_progress_report(cp->state,cp->sound,NOT_FROM_ENVED);
      (cp->cgx)->fft_in_progress = 0;
    }
  if (cp->fft) cp->fft = free_fft_info(cp->fft);
  if (cp->fft_data) {FREE(cp->fft_data); cp->fft_data = NULL;}
  /* this may leave ->wp window unfreed? -- see snd-fft.c free_fft_state */
  return(0);
}

static chan_info *axis_cp = NULL;
static axis_context *make_axis_cp(snd_state *ss, Widget w)
{
  /* conjure up minimal context for axis drawer in snd-axis.c */
  /* almost the same as the same-named function in snd-xenv.c, but doesn't seem worth the trouble of making one function for both */
  axis_info *ap;
  axis_context *ax;
  if (!axis_cp)
    {
      /* axis_cp is just a dummy chan_info struct to hold the axis_info pointer */
      axis_cp = (chan_info *)CALLOC(1,sizeof(chan_info));
      axis_cp->s_type = CHAN_INFO;
      axis_cp->printing = 0;
      axis_cp->drawing = 1;
      axis_cp->state = ss;
      ap = (axis_info *)CALLOC(1,sizeof(axis_info));
      axis_cp->axis = ap;
      ax = (axis_context *)CALLOC(1,sizeof(axis_context));
      ap->ax = ax;
      ap->ss = ss;
      ap->cp = axis_cp;
      ax->ss = ss;
      ax->dp = XtDisplay(graph_drawer);
      ax->wn = XtWindow(graph_drawer);
    }
  else
    {
      ap = axis_cp->axis;
      ax = ap->ax;
      if (ap->xlabel) FREE(ap->xlabel);
    }
  ax->gc = gc;
  ap->xmin = 0.0;
  ap->xmax = 1.0;
  ap->ymin = 0.0;
  ap->ymax = 1.0;
  ap->xlabel = NULL;
  ap->ylabel = NULL;
  ap->x0 = 0.0;
  ap->x1 = 1.0;
  ap->y0 = 0.0;
  ap->y1 = 1.0;
  ap->width = widget_width(w);
  ap->window_width = ap->width;
  ap->y_offset = 0;
  ap->height = widget_height(w);
  ap->graph_x0 = 0;
  clear_window(ax);
  make_axes_1(axis_cp, ap, X_IN_SECONDS, 1);
  return(ax);
}

static void graph_redisplay(snd_state *ss)
{
  Display *dp;
  Drawable wn;
  int ix0,iy0,ix1,iy1,i;
  Float xincr,x;
  axis_context *ax;

  ax = make_axis_cp(ss,graph_drawer);
  dp = XtDisplay(graph_drawer);
  wn = XtWindow(graph_drawer);

  ax->gc = gc;
  ix1 = grf_x(0.0,axis_cp->axis);
  iy1 = grf_y(current_graph_data[0],axis_cp->axis);
  xincr = 1.0 / (Float)GRAPH_SIZE;

  for (i=1,x=xincr;i<GRAPH_SIZE;i++,x+=xincr)
    {
      ix0 = ix1;
      iy0 = iy1;
      ix1 = grf_x(x,axis_cp->axis);
      iy1 = grf_y(current_graph_data[i],axis_cp->axis);
      XDrawLine(dp,wn,gc,ix0,iy0,ix1,iy1);
    }

  ax->gc = fgc;
  ix1 = grf_x(0.0,axis_cp->axis);
  iy1 = grf_y(current_graph_fftr[0],axis_cp->axis);
  xincr = 1.0 / (Float)GRAPH_SIZE;

  for (i=1,x=xincr;i<GRAPH_SIZE;i++,x+=xincr)
    {
      ix0 = ix1;
      iy0 = iy1;
      ix1 = grf_x(x,axis_cp->axis);
      iy1 = grf_y(current_graph_fftr[i],axis_cp->axis);
      XDrawLine(dp,wn,fgc,ix0,iy0,ix1,iy1);
    }
}


static void size_help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "FFT Size",
"Any size FFT can be accomodated here, as long\n\
as it will fit in memory somehow.  The larger\n\
the FFT, the longer it takes to compute.\n\
The size must be a power of 2.\n\
");
}

static void get_fft_window_data(snd_state *ss)
{
  int i;
  make_fft_window_1(current_graph_data,GRAPH_SIZE,fft_window(ss),fft_beta(ss),FALSE);
  for (i=0;i<GRAPH_SIZE*2;i++)
    {
      current_graph_fftr[i] = 0.0;
      current_graph_ffti[i] = 0.0;
    }
  for (i=0;i<GRAPH_SIZE;i++)
    current_graph_fftr[i] = current_graph_data[i];
  mus_spectrum(current_graph_fftr,current_graph_ffti,NULL,GRAPH_SIZE*2,0);
  for (i=0;i<GRAPH_SIZE;i++)
    current_graph_fftr[i] = (current_graph_fftr[i] + 80.0)/80.0;
}

static void size_browse_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  int size;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)callData;
  in_set_fft_size(ss,fft_sizes[cbs->item_position - 1]);
  size = fft_size(ss);
  map_over_chans(ss,set_ffts_size,(void *)(&size));
  map_over_chans(ss,calculate_fft,NULL);
  set_label(graph_label,FFT_WINDOWS[fft_window(ss)]);
  get_fft_window_data(ss);
  graph_redisplay(ss);
}

static int map_chans_wavelet_type(chan_info *cp, void *ptr) {cp->wavelet_type = (int)ptr; return(0);}

static void wavelet_browse_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  int val;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)callData;
  in_set_wavelet_type(ss,val = (cbs->item_position - 1)); /* make these numbers 0-based as in mus.lisp */
  map_over_chans(ss,map_chans_wavelet_type,(void *)val);
  if (transform_type(ss) == WAVELET)
    map_over_chans(ss,calculate_fft,NULL);
}

static void wavelet_help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Wavelet Choice",
"These names refer to various standard wavelets.\n\
The actual coefficients are in snd-fft.c.\n\
");
}


static void window_browse_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)callData;
  snd_state *ss = (snd_state *)clientData;
  fft_window_choice = (cbs->item_position - 1); /* make these numbers 0-based as in mus.lisp */
  in_set_fft_window(ss,fft_window_choice);
  map_over_chans(ss,set_ffts_window,NULL);
  map_over_chans(ss,calculate_fft,NULL);
  set_label(graph_label,FFT_WINDOWS[fft_window(ss)]);
  get_fft_window_data(ss);
  graph_redisplay(ss);
  if (!(ss->using_schemes))
    {
      if (fft_window_beta_in_use(fft_window(ss)))
	XtVaSetValues(window_beta_scale,XmNbackground,(ss->sgx)->highlight_color,NULL);
      else XtVaSetValues(window_beta_scale,XmNbackground,(ss->sgx)->basic_color,NULL);
    }
}

static void window_help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "FFT Window",
"The FFT window names are taken from the article\n\
of Harris that discusses most FFT windows\n\
at great length.  The default window\n\
is a second order Blackman window.\n\
");
}



static void transform_type_browse_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)callData;
  map_over_chans(ss,force_fft_clear,NULL);
  in_set_transform_type(ss,cbs->item_position - 1);
  map_over_chans(ss,calculate_fft,NULL);
}

static void transform_type_help_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  snd_state *ss = (snd_state *)clientData;
  snd_help(ss,
       "Transform Type",
"This list presents the various transforms\n\
that are available.\n\
");	   
}


static void normal_fft_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  snd_state *ss = (snd_state *)clientData;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)callData;
  if (cb->set)
    {
      XmToggleButtonSetState(sono_button,FALSE,FALSE);
      XmToggleButtonSetState(spectro_button,FALSE,FALSE);
      in_set_fft_style(ss,NORMAL_FFT);
    }
  else
    {
      XmToggleButtonSetState(sono_button,TRUE,FALSE);
      in_set_fft_style(ss,SONOGRAM);
    }
  map_over_chans(ss,calculate_fft,NULL);
}

static void sonogram_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  snd_state *ss = (snd_state *)clientData;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)callData;
  if (cb->set)
    {
      XmToggleButtonSetState(normo_button,FALSE,FALSE);
      XmToggleButtonSetState(spectro_button,FALSE,FALSE);
      in_set_fft_style(ss,SONOGRAM);
    }
  else
    {
      XmToggleButtonSetState(normo_button,TRUE,FALSE);
      in_set_fft_style(ss,NORMAL_FFT);
    }
  map_over_chans(ss,calculate_fft,NULL);
}

static void spectrogram_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  snd_state *ss = (snd_state *)clientData;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)callData;
  if (cb->set)
    {
      XmToggleButtonSetState(normo_button,FALSE,FALSE);
      XmToggleButtonSetState(sono_button,FALSE,FALSE);
      in_set_fft_style(ss,SPECTROGRAM);
    }
  else
    {
      XmToggleButtonSetState(normo_button,TRUE,FALSE);
      in_set_fft_style(ss,NORMAL_FFT);
    }
  map_over_chans(ss,calculate_fft,NULL);
}

static int map_show_fft_peaks(chan_info *cp, void *ptr) {cp->show_fft_peaks = (int)ptr; return(0);}

static void peaks_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  snd_state *ss = (snd_state *)clientData;
  int val = 0;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)callData;
  in_set_show_fft_peaks(ss,val = (cb->set));
  map_over_chans(ss,map_show_fft_peaks,(void *)val);
  map_over_chans(ss,calculate_fft,NULL);
}

static int map_chans_fft_log_magnitude(chan_info *cp, void *ptr) {cp->fft_log_magnitude = (int)ptr; return(0);}

static void db_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  snd_state *ss = (snd_state *)clientData;
  int val;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)callData;
  in_set_fft_log_magnitude(ss,val = cb->set);
  map_over_chans(ss,map_chans_fft_log_magnitude,(void *)val);
  map_over_chans(ss,calculate_fft,NULL);
}

static int map_chans_fft_log_frequency(chan_info *cp, void *ptr) {cp->fft_log_frequency = (int)ptr; return(0);}

static void logfreq_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  snd_state *ss = (snd_state *)clientData;
  int val;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)callData;
  in_set_fft_log_frequency(ss,val = cb->set);
  map_over_chans(ss,map_chans_fft_log_frequency,(void *)val);
  map_over_chans(ss,calculate_fft,NULL);
}

static void normalize_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  snd_state *ss = (snd_state *)clientData;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)callData;
  in_set_normalize_fft(ss,(cb->set) ? NORMALIZE_BY_CHANNEL : DONT_NORMALIZE);
  map_over_chans(ss,calculate_fft,NULL);
}

static void selection_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  snd_state *ss = (snd_state *)clientData;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)callData;
  in_set_show_selection_transform(ss,cb->set);
  map_over_chans(ss,calculate_fft,NULL);
}


static void beta_help_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  snd_help((snd_state *)clientData,
	   "FFT Window Parameter",
"This scale sets the FFT window parameter\n\
sometimes known as alpha or beta.  The\n\
scale tries to provide access to 'normal'\n\
values, given the current FFT window.\n\
");
} 

static void beta_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  snd_state *ss = (snd_state *)clientData;
  XmScaleCallbackStruct *cb = (XmScaleCallbackStruct *)callData;
  in_set_fft_beta(ss,(Float)cb->value/100.0);
  if (fft_window_beta_in_use(fft_window(ss)))
    {
      get_fft_window_data(ss);
      graph_redisplay(ss);
      if (transform_type(ss) == FOURIER) map_over_chans(ss,calculate_fft,NULL);
    }
} 

static void graph_help_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  snd_help((snd_state *)clientData,
	   "Window Graph",
"This shows a graph of the current fft window and the spectrum\n\
thereof -- the spectrum is in dB with the min at -80dB.\n\
");
}

static void graph_resize_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  graph_redisplay((snd_state *)clientData);
}

static void Dismiss_Transform_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  XtUnmanageChild(transform_dialog);
}

static void Help_Transform_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  transform_dialog_help((snd_state *)clientData);
}

void fire_up_transform_dialog(snd_state *ss)
{
  XmString xhelp,xdismiss,xtitle,bstr;
  Arg args[32];
  XmString sizes[NUM_FFT_SIZES];
  XmString *types;
  XmString wavelets[NUM_WAVELETS];
  XmString windows[NUM_FFT_WINDOWS];
  XGCValues gv;
  int size_pos = 1;
  int n,i,need_callback = 0;
  Widget mainform,type_frame,type_form,type_label,
                  size_frame,size_form,size_label,
                  display_frame,display_form,display_label,
                  window_frame,window_form,window_label,
                  wavelet_frame,wavelet_form,wavelet_label,
                  graph_frame,graph_form;
    
  if (!transform_dialog)
    {
      types = (XmString *)CALLOC(num_transform_types,sizeof(XmString));
      for (i=0;i<NUM_FFT_SIZES;i++)
	if (fft_sizes[i] == fft_size(ss))
	  {
	    size_pos = i+1;
	    break;
	  }
      xdismiss = XmStringCreate(STR_Dismiss,XmFONTLIST_DEFAULT_TAG); /* needed by template dialog */
      xhelp = XmStringCreate(STR_Help,XmFONTLIST_DEFAULT_TAG);
      xtitle = XmStringCreate(STR_Transform_Options,XmFONTLIST_DEFAULT_TAG);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNcancelLabelString,xdismiss); n++;
      XtSetArg(args[n],XmNhelpLabelString,xhelp); n++;
      XtSetArg(args[n],XmNautoUnmanage,FALSE); n++;
      XtSetArg(args[n],XmNdialogTitle,xtitle); n++;
#if RESIZE_DIALOG
      XtSetArg(args[n],XmNresizePolicy,XmRESIZE_GROW); n++;
      XtSetArg(args[n],XmNnoResize,FALSE); n++;
#endif
      XtSetArg(args[n],XmNtransient,FALSE); n++;
      transform_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss),STR_Transform_Options,args,n);
      add_dialog(ss,transform_dialog);
#if OVERRIDE_TOGGLE
      override_form_translation(transform_dialog);
#endif

      XtAddCallback(transform_dialog,XmNcancelCallback,Dismiss_Transform_Callback,ss);
      XtAddCallback(transform_dialog,XmNhelpCallback,Help_Transform_Callback,ss);
      XmStringFree(xhelp);
      XmStringFree(xdismiss);
      XmStringFree(xtitle);
      if (!(ss->using_schemes))
	{
	  XtVaSetValues(XmMessageBoxGetChild(transform_dialog,XmDIALOG_CANCEL_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
	  XtVaSetValues(XmMessageBoxGetChild(transform_dialog,XmDIALOG_HELP_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
	}

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNbottomWidget,XmMessageBoxGetChild(transform_dialog,XmDIALOG_SEPARATOR)); n++;
      mainform = sndCreateFormWidget("mainform",transform_dialog,args,n);


      /* now 6 boxes within the main box:
	 
	 type (list)    |  size (list)        |  display (button column)
	 wavelet (list) |  window (list+beta) |  graph (fft?) of current window
	 
	 each box has a frame, label, and contents
      */

      /* TYPE */
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_POSITION); n++;
      XtSetArg(args[n],XmNbottomPosition,50); n++;
      XtSetArg(args[n],XmNborderWidth,4); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNborderColor,(ss->sgx)->basic_color); n++;}
      type_frame = sndCreateFrameWidget("type-frame",mainform,args,n);
      XtAddCallback(type_frame,XmNhelpCallback,transform_type_help_Callback,ss);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      type_form = sndCreateFormWidget("type-form",type_frame,args,n);
      /* needed because XmFrame only accepts one child */
      XtAddCallback(type_form,XmNhelpCallback,transform_type_help_Callback,ss);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNalignment,XmALIGNMENT_CENTER); n++;
      type_label = XtCreateManagedWidget(STR_type,xmLabelWidgetClass,type_form,args,n);
      XtAddCallback(type_label,XmNhelpCallback,transform_type_help_Callback,ss);

      n=0;
#ifdef LESSTIF_VERSION
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->white); n++;}
#else
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
#endif
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,type_label); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      type_list = XmCreateScrolledList(type_form,"type-list",args,n);
      if (!(ss->using_schemes)) XtVaSetValues(type_list,XmNbackground,(ss->sgx)->white,XmNforeground,(ss->sgx)->black,NULL);
      for (i=0;i<num_transform_types;i++) 
	{
	  if (i < NUM_TRANSFORM_TYPES)
	    types[i] = XmStringCreate(TRANSFORM_TYPES[i],XmFONTLIST_DEFAULT_TAG);
	  else types[i] = XmStringCreate(added_transform_name(i),XmFONTLIST_DEFAULT_TAG);
	}
      XtVaSetValues(type_list,XmNitems,types,XmNitemCount,num_transform_types,XmNvisibleItemCount,6,NULL);
      for (i=0;i<num_transform_types;i++) XmStringFree(types[i]);
      XtManageChild(type_list); 
      XtAddCallback(type_list,XmNbrowseSelectionCallback,transform_type_browse_Callback,ss);
      XtAddCallback(type_list,XmNhelpCallback,transform_type_help_Callback,ss);
      FREE(types);

      /* SIZE */
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNleftWidget,type_frame); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n],XmNbottomWidget,type_frame); n++;
      XtSetArg(args[n],XmNborderWidth,4); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNborderColor,(ss->sgx)->basic_color); n++;}
      size_frame = sndCreateFrameWidget("size-frame",mainform,args,n);
      XtAddCallback(size_frame,XmNhelpCallback,size_help_Callback,ss);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      size_form = sndCreateFormWidget("size-form",size_frame,args,n);
      XtAddCallback(size_form,XmNhelpCallback,size_help_Callback,ss);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNalignment,XmALIGNMENT_CENTER); n++;
      size_label = XtCreateManagedWidget(STR_size,xmLabelWidgetClass,size_form,args,n);
      XtAddCallback(size_label,XmNhelpCallback,size_help_Callback,ss);

      n=0;
#ifdef LESSTIF_VERSION
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->white); n++;}
#else
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
#endif
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,size_label); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopItemPosition,(size_pos>2) ? (size_pos-2) : size_pos); n++;
      size_list = XmCreateScrolledList(size_form,"size-list",args,n);
      if (!(ss->using_schemes)) XtVaSetValues(size_list,XmNbackground,(ss->sgx)->white,XmNforeground,(ss->sgx)->black,NULL);
      for (i=0;i<NUM_FFT_SIZES;i++) sizes[i] = XmStringCreate(FFT_SIZES[i],XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(size_list,XmNitems,sizes,XmNitemCount,NUM_FFT_SIZES,XmNvisibleItemCount,6,NULL);
      for (i=0;i<NUM_FFT_SIZES;i++) XmStringFree(sizes[i]);
      XtManageChild(size_list); 
      XtAddCallback(size_list,XmNbrowseSelectionCallback,size_browse_Callback,ss);
      XtAddCallback(size_list,XmNhelpCallback,size_help_Callback,ss);


      /* DISPLAY */
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNleftWidget,size_frame); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNborderWidth,4); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNborderColor,(ss->sgx)->basic_color); n++;}
      display_frame = sndCreateFrameWidget("display-frame",mainform,args,n);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->zoom_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      display_form = sndCreateFormWidget("display-form",display_frame,args,n);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNalignment,XmALIGNMENT_CENTER); n++;
      display_label = XtCreateManagedWidget(STR_display,xmLabelWidgetClass,display_form,args,n);

      n=0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n],XmNbackground,(ss->sgx)->lighter_blue); n++;
	  XtSetArg(args[n],XmNselectColor,(ss->sgx)->red); n++;
	}
      bstr = XmStringCreate(STR_normal_fft,XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,display_label); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNlabelString,bstr); n++;
      XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;
      XtSetArg(args[n],XmNindicatorType,XmONE_OF_MANY); n++;
      normo_button = sndCreateToggleButtonWidget("normo-button",display_form,args,n);
      XtAddCallback(normo_button,XmNvalueChangedCallback,normal_fft_Callback,ss);
      XmStringFree(bstr);

      n=0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n],XmNbackground,(ss->sgx)->lighter_blue); n++;
	  XtSetArg(args[n],XmNselectColor,(ss->sgx)->red); n++;
	}
      bstr = XmStringCreate(STR_sonogram,XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,normo_button); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNlabelString,bstr); n++;
      XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;
      XtSetArg(args[n],XmNindicatorType,XmONE_OF_MANY); n++;
      sono_button = sndCreateToggleButtonWidget("sono-button",display_form,args,n);
      XtAddCallback(sono_button,XmNvalueChangedCallback,sonogram_Callback,ss);
      XmStringFree(bstr);

      n=0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n],XmNbackground,(ss->sgx)->lighter_blue); n++;
	  XtSetArg(args[n],XmNselectColor,(ss->sgx)->red); n++;
	}
      bstr = XmStringCreate(STR_spectrogram,XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,sono_button); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNlabelString,bstr); n++;
      XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;
      XtSetArg(args[n],XmNindicatorType,XmONE_OF_MANY); n++;
      spectro_button = sndCreateToggleButtonWidget("spectro-button",display_form,args,n);
      XtAddCallback(spectro_button,XmNvalueChangedCallback,spectrogram_Callback,ss);
      XmStringFree(bstr);

      n=0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n],XmNbackground,(ss->sgx)->lighter_blue); n++;
	  XtSetArg(args[n],XmNselectColor,(ss->sgx)->red); n++;
	}
      bstr = XmStringCreate(STR_peaks,XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,spectro_button); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNlabelString,bstr); n++;
      XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;
      peaks_button = sndCreateToggleButtonWidget("peaks-button",display_form,args,n);
      XtAddCallback(peaks_button,XmNvalueChangedCallback,peaks_Callback,ss);
      XmStringFree(bstr);

      n=0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n],XmNbackground,(ss->sgx)->lighter_blue); n++;
	  XtSetArg(args[n],XmNselectColor,(ss->sgx)->red); n++;
	}
      bstr = XmStringCreate(STR_dB,XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,peaks_button); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNlabelString,bstr); n++;
      XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;
      db_button = sndCreateToggleButtonWidget("db-button",display_form,args,n);
      XtAddCallback(db_button,XmNvalueChangedCallback,db_Callback,ss);
      XmStringFree(bstr);

      n=0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n],XmNbackground,(ss->sgx)->lighter_blue); n++;
	  XtSetArg(args[n],XmNselectColor,(ss->sgx)->red); n++;
	}
      bstr = XmStringCreate(STR_log_freq,XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,db_button); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNlabelString,bstr); n++;
      XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;
      logfreq_button = sndCreateToggleButtonWidget("logfreq-button",display_form,args,n);
      XtAddCallback(logfreq_button,XmNvalueChangedCallback,logfreq_Callback,ss);
      XmStringFree(bstr);

      n=0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n],XmNbackground,(ss->sgx)->lighter_blue); n++;
	  XtSetArg(args[n],XmNselectColor,(ss->sgx)->red); n++;
	}
      bstr = XmStringCreate(STR_normalize,XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,logfreq_button); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNlabelString,bstr); n++;
      XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;
      normalize_button = sndCreateToggleButtonWidget("normalize-button",display_form,args,n);
      XtAddCallback(normalize_button,XmNvalueChangedCallback,normalize_Callback,ss);
      XmStringFree(bstr);

      n=0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n],XmNbackground,(ss->sgx)->lighter_blue); n++;
	  XtSetArg(args[n],XmNselectColor,(ss->sgx)->red); n++;
	}
      bstr = XmStringCreate(STR_selection,XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,normalize_button); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNlabelString,bstr); n++;
      XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;
      selection_button = sndCreateToggleButtonWidget("selection-button",display_form,args,n);
      XtAddCallback(selection_button,XmNvalueChangedCallback,selection_Callback,ss);
      XmStringFree(bstr);


      
      /* WAVELET */
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n],XmNrightWidget,type_frame); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,type_frame); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNborderWidth,4); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNborderColor,(ss->sgx)->basic_color); n++;}
      wavelet_frame = sndCreateFrameWidget("wavelet-frame",mainform,args,n);
      XtAddCallback(wavelet_frame,XmNhelpCallback,wavelet_help_Callback,ss);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      wavelet_form = sndCreateFormWidget("wavelet-form",wavelet_frame,args,n);
      XtAddCallback(wavelet_form,XmNhelpCallback,wavelet_help_Callback,ss);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNalignment,XmALIGNMENT_CENTER); n++;
      wavelet_label = XtCreateManagedWidget(STR_wavelet,xmLabelWidgetClass,wavelet_form,args,n);
      XtAddCallback(wavelet_label,XmNhelpCallback,wavelet_help_Callback,ss);

      n=0;
#ifdef LESSTIF_VERSION
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->white); n++;}
#else
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
#endif
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,wavelet_label); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      wavelet_list = XmCreateScrolledList(wavelet_form,"wavelet-list",args,n);
      if (!(ss->using_schemes)) XtVaSetValues(wavelet_list,XmNbackground,(ss->sgx)->white,XmNforeground,(ss->sgx)->black,NULL);
      for (i=0;i<NUM_WAVELETS;i++) wavelets[i] = XmStringCreate(WAVELETS[i],XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(wavelet_list,XmNitems,wavelets,XmNitemCount,NUM_WAVELETS,XmNvisibleItemCount,5,NULL);
      for (i=0;i<NUM_WAVELETS;i++) XmStringFree(wavelets[i]);
      XtManageChild(wavelet_list); 
      XtAddCallback(wavelet_list,XmNbrowseSelectionCallback,wavelet_browse_Callback,ss);
      XtAddCallback(wavelet_list,XmNhelpCallback,wavelet_help_Callback,ss);


      /* WINDOW */
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNleftWidget,wavelet_frame); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n],XmNrightWidget,size_frame); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,size_frame); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNborderWidth,4); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNborderColor,(ss->sgx)->basic_color); n++;}
      window_frame = sndCreateFrameWidget("window-frame",mainform,args,n);
      XtAddCallback(window_frame,XmNhelpCallback,window_help_Callback,ss);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      window_form = sndCreateFormWidget("window-form",window_frame,args,n);
      XtAddCallback(window_form,XmNhelpCallback,window_help_Callback,ss);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNshowValue,TRUE); n++;
#ifdef LESSTIF_VERSION
      XtSetArg(args[n],XmNdecimalPoints,2); n++;
#endif
      XtSetArg(args[n],XmNvalue,100 * fft_beta(ss)); n++;
      XtSetArg(args[n],XmNdragCallback,make_callback_list(beta_Callback,(XtPointer)ss)); n++;
      XtSetArg(args[n],XmNvalueChangedCallback,make_callback_list(beta_Callback,(XtPointer)ss)); n++;
      window_beta_scale = XtCreateManagedWidget("scale",xmScaleWidgetClass,window_form,args,n);
      XtAddCallback(window_beta_scale,XmNhelpCallback,beta_help_Callback,ss);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNalignment,XmALIGNMENT_CENTER); n++;
      window_label = XtCreateManagedWidget(STR_window,xmLabelWidgetClass,window_form,args,n);
      XtAddCallback(window_label,XmNhelpCallback,window_help_Callback,ss);

      n=0;
#ifdef LESSTIF_VERSION
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->white); n++;}
#else
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
#endif
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNbottomWidget,window_beta_scale); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,window_label); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopItemPosition,(fft_window(ss)>2) ? (fft_window(ss)-1) : (fft_window(ss)+1)); n++;
      window_list = XmCreateScrolledList(window_form,"window-list",args,n);
      if (!(ss->using_schemes)) XtVaSetValues(window_list,XmNbackground,(ss->sgx)->white,XmNforeground,(ss->sgx)->black,NULL);
      for (i=0;i<NUM_FFT_WINDOWS;i++) windows[i] = XmStringCreate(FFT_WINDOWS[i],XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(window_list,XmNitems,windows,XmNitemCount,NUM_FFT_WINDOWS,XmNvisibleItemCount,5,NULL);
      for (i=0;i<NUM_FFT_WINDOWS;i++) XmStringFree(windows[i]);
      XtManageChild(window_list); 
      XtAddCallback(window_list,XmNbrowseSelectionCallback,window_browse_Callback,ss);
      XtAddCallback(window_list,XmNhelpCallback,window_help_Callback,ss);



      /* GRAPH */
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNleftWidget,window_frame); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,display_frame); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNborderWidth,4); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNborderColor,(ss->sgx)->basic_color); n++;}
      graph_frame = sndCreateFrameWidget("graph-frame",mainform,args,n);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      graph_form = sndCreateFormWidget("graph-form",graph_frame,args,n);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNalignment,XmALIGNMENT_CENTER); n++;
      graph_label = XtCreateManagedWidget(STR_window,xmLabelWidgetClass,graph_form,args,n);
      /* label should change according to what is being displayed */

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->graph_color); n++;}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,graph_label); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNallowResize,TRUE); n++;
      graph_drawer = sndCreateDrawingAreaWidget("graph-drawer",graph_form,args,n);
      XtAddCallback(graph_drawer,XmNhelpCallback,graph_help_Callback,ss);

      gv.function = GXcopy;
      XtVaGetValues(graph_drawer,XmNbackground, &gv.background,XmNforeground, &gv.foreground,NULL);
      gc = XtGetGC(graph_drawer, GCForeground | GCFunction, &gv);

      gv.foreground = (ss->sgx)->enved_waveform_color;
      fgc = XtGetGC(graph_drawer, GCForeground | GCFunction, &gv);

      XmToggleButtonSetState(normo_button,fft_style(ss) == NORMAL_FFT,FALSE);
      XmToggleButtonSetState(sono_button,fft_style(ss) == SONOGRAM,FALSE);
      XmToggleButtonSetState(spectro_button,fft_style(ss) == SPECTROGRAM,FALSE);
      XmToggleButtonSetState(peaks_button,show_fft_peaks(ss),FALSE);
      XmToggleButtonSetState(db_button,fft_log_magnitude(ss),FALSE);
      XmToggleButtonSetState(logfreq_button,fft_log_frequency(ss),FALSE);
      XmToggleButtonSetState(normalize_button,normalize_fft(ss),FALSE);
      XmToggleButtonSetState(selection_button,show_selection_transform(ss),FALSE);

      /* select current list choices */
      /* display current windowing choice unless wavelet in force */

      XmListSelectPos(type_list,transform_type(ss)+1,FALSE);
      XmListSelectPos(wavelet_list,wavelet_type(ss)+1,FALSE);
      XmListSelectPos(size_list,size_pos,FALSE);
      XmListSelectPos(window_list,fft_window(ss)+1,FALSE);
      need_callback = 1;
      if (fft_window_beta_in_use(fft_window(ss))) XtVaSetValues(window_beta_scale,XmNbackground,(ss->sgx)->highlight_color,NULL);
    }
  else
    {
      raise_dialog(transform_dialog);
    }
  if (!XtIsManaged(transform_dialog)) XtManageChild(transform_dialog);
  if (need_callback)
    {
      set_label(graph_label,FFT_WINDOWS[fft_window(ss)]);
      get_fft_window_data(ss);
      XtAddCallback(graph_drawer,XmNresizeCallback,graph_resize_Callback,ss);
      XtAddCallback(graph_drawer,XmNexposeCallback,graph_resize_Callback,ss);
      need_callback = 0;
    }
}

int transform_dialog_is_active(void)
{
  return((transform_dialog) && (XtIsManaged(transform_dialog)));
}

/* various set- cases need to be reflected in the transform dialog */
void set_fft_beta(snd_state *ss, Float val)
{
  in_set_fft_beta(ss,val);
  if (transform_dialog) 
    {
      XmScaleSetValue(window_beta_scale,(int)(100*val));
      get_fft_window_data(ss);
      graph_redisplay(ss);
    }
  if (!(ss->graph_hook_active)) map_over_chans(ss,calculate_fft,NULL);
}

void set_fft_style(snd_state *ss, int val)
{
  in_set_fft_style(ss,val);
  if (transform_dialog)
    switch (val)
      {
      case NORMAL_FFT:
	XmToggleButtonSetState(normo_button,TRUE,FALSE);
	XmToggleButtonSetState(spectro_button,FALSE,FALSE);
	XmToggleButtonSetState(sono_button,FALSE,FALSE);
	break;
      case SONOGRAM:
	XmToggleButtonSetState(normo_button,FALSE,FALSE);
	XmToggleButtonSetState(spectro_button,FALSE,FALSE);
	XmToggleButtonSetState(sono_button,TRUE,FALSE);
	break;
      case SPECTROGRAM:
	XmToggleButtonSetState(normo_button,FALSE,FALSE);
	XmToggleButtonSetState(spectro_button,TRUE,FALSE);
	XmToggleButtonSetState(sono_button,FALSE,FALSE);
	break;
      }
  if (!(ss->graph_hook_active)) map_over_chans(ss,calculate_fft,NULL);
}

void set_fft_size(snd_state *ss, int val)
{
  int i;
  in_set_fft_size(ss,val);
  if (transform_dialog)
    {
      for (i=0;i<NUM_FFT_SIZES;i++)
	if (fft_sizes[i] == val)
	  {
	    XmListSelectPos(size_list,i+1,FALSE);
	    break;
	  }
    }
  if (!(ss->graph_hook_active)) map_over_chans(ss,calculate_fft,NULL);
}

void set_fft_window(snd_state *ss, int val)
{
  in_set_fft_window(ss,val);
  fft_window_choice = val;
  if (!(ss->graph_hook_active)) 
    {
      map_over_chans(ss,set_ffts_window,NULL);
      map_over_chans(ss,calculate_fft,NULL);
    }
  if (transform_dialog)
    {
      XmListSelectPos(window_list,val+1,FALSE);
      set_label(graph_label,FFT_WINDOWS[val]);
      get_fft_window_data(ss);
      graph_redisplay(ss);
      if (fft_window_beta_in_use(val))
	XtVaSetValues(window_beta_scale,XmNbackground,(ss->sgx)->highlight_color,NULL);
      else XtVaSetValues(window_beta_scale,XmNbackground,(ss->sgx)->basic_color,NULL);
    }
}
  
void set_transform_type(snd_state *ss, int val)
{
  if (!(ss->graph_hook_active)) map_over_chans(ss,force_fft_clear,NULL);
  in_set_transform_type(ss,val);
  if (!(ss->graph_hook_active)) map_over_chans(ss,calculate_fft,NULL);
  if (transform_dialog) XmListSelectPos(type_list,val+1,FALSE);
}

void set_wavelet_type(snd_state *ss, int val)
{
  if (transform_dialog) XmListSelectPos(wavelet_list,val,FALSE);
  in_set_wavelet_type(ss,val);
  map_over_chans(ss,map_chans_wavelet_type,(void *)val);
  if ((transform_type(ss) == WAVELET) && (!(ss->graph_hook_active))) map_over_chans(ss,calculate_fft,NULL);
}

/* various set- cases need to be reflected in the transform dialog */
void set_show_fft_peaks(snd_state *ss, int val)
{
  in_set_show_fft_peaks(ss,val);
  map_over_chans(ss,map_show_fft_peaks,(void *)val);
  if (transform_dialog) 
    set_toggle_button(peaks_button,val,FALSE,(void *)ss);
  if (!(ss->graph_hook_active)) map_over_chans(ss,calculate_fft,NULL);
}

void set_fft_log_frequency(snd_state *ss, int val)
{
  in_set_fft_log_frequency(ss,val);
  map_over_chans(ss,map_chans_fft_log_frequency,(void *)val);
  if (transform_dialog)
    set_toggle_button(logfreq_button,val,FALSE,(void *)ss);
  if (!(ss->graph_hook_active)) map_over_chans(ss,calculate_fft,NULL);
}

void set_fft_log_magnitude(snd_state *ss, int val)
{
  in_set_fft_log_magnitude(ss,val);
  map_over_chans(ss,map_chans_fft_log_magnitude,(void *)val);
  if (transform_dialog) 
    set_toggle_button(db_button,val,FALSE,(void *)ss);
  if (!(ss->graph_hook_active)) map_over_chans(ss,calculate_fft,NULL);
}

void set_normalize_fft(snd_state *ss, int val)
{
  in_set_normalize_fft(ss,val);
  if (transform_dialog) 
    set_toggle_button(normalize_button,val,FALSE,(void *)ss);
  if (!(ss->graph_hook_active)) map_over_chans(ss,calculate_fft,NULL);
}

void set_show_selection_transform(snd_state *ss, int show)
{
  in_set_show_selection_transform(ss,show);
  if (transform_dialog)
    set_toggle_button(selection_button,show,FALSE,(void *)ss); 
  if (!(ss->graph_hook_active)) map_over_chans(ss,calculate_fft,NULL);
}

#if HAVE_GUILE
int add_transform_to_list(char *name)
{
  /* put at end of list and return associated browse callback row */
  XmString str;
  if (transform_dialog)
    {
      str = XmStringCreate(name,XmFONTLIST_DEFAULT_TAG);
      XmListAddItem(type_list,str,0);
      XmStringFree(str);
    }
  return(num_transform_types++);
}
#endif
