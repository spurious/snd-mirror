#include "snd.h"

/* handling of "beta" changed drastically 28-June-98 
 * it is now a number between 0 and 1 from ss point of view,
 * and is scaled by the window max before being applied 
 *
 * convolution from fft.c merged into this file 12-Nov-99.
 * clm fft window stuff replaces local funcs 6-May-00.
 * started replacing local transforms with those in gsl 6-May-00.
 */

#define NUM_CACHED_FFT_WINDOWS 8
#define NUM_FFT_WINDOWS 16

static Float beta_maxes[NUM_FFT_WINDOWS] = {1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
					    1.0,1.0,15.0,10.0,10.0,10.0,1.0};

int default_fft_window(snd_state *ss) {if (ss) return(fft_window(ss)); else return(MUS_BLACKMAN2_WINDOW);}

typedef struct {
  int type;
  int inuse;
  int size;
  int pad_zero;
  Float beta;
  Float *window;
} fft_window_state;

typedef struct {
  int n,nn,mmax,istep,m,i,size,wintype,t_type,old_style;
  double wr,c,wi,s,angle;
  int slice,inner,outer;
  void *chan;
  fft_window_state *wp;
  Float *data;
  Float *hwin;
  Float beta;
  int fw_slot,hwin_size,wavelet_choice;
  int beg,databeg,datalen;
  int losamp,edit_ctr,dBing,lfreq;
  int pad_zero;
  Float cutoff;
  snd_state *ss;
} fft_state;

#if HAVE_GUILE

#include "vct.h"
#include "sg.h"

typedef struct {
  char *name,*xlabel;
  Float lo,hi;
  SCM proc;
  int type;
} added_transform;

static added_transform **added_transforms = NULL;
static int added_transforms_size = 0;
static int added_transforms_top = 0;

static added_transform *new_added_transform(void)
{
  int i;
  if (added_transforms == NULL)
    {
      added_transforms_size = 4;
      added_transforms = (added_transform **)CALLOC(added_transforms_size,sizeof(added_transform *));
    }
  else
    {
      if (added_transforms_top == added_transforms_size)
	{
	  added_transforms_size += 4;
	  added_transforms = (added_transform **)REALLOC(added_transforms,added_transforms_size * sizeof(added_transform *));
	  for (i=added_transforms_top;i<added_transforms_size;i++) added_transforms[i] = NULL;
	}
    }
  added_transforms[added_transforms_top] = (added_transform *)CALLOC(1,sizeof(added_transform));
  return(added_transforms[added_transforms_top++]);
}

static int add_transform(char *name, char *xlabel, Float lo, Float hi, SCM proc)
{
  added_transform *af;
  snd_protect(proc);
  af = new_added_transform();
  af->name = copy_string(name);
  af->xlabel = copy_string(xlabel);
  af->lo = lo;
  af->hi = hi;
  af->proc = proc;
  af->type = add_transform_to_list(name);
  return(af->type);
}

char *added_transform_name(int type)
{
  int i;
  for (i=0;i<added_transforms_top;i++)
    if (added_transforms[i]->type == type)
      return(added_transforms[i]->name);
  return("unknown");
}

static char *added_transform_xlabel(int type)
{
  int i;
  for (i=0;i<added_transforms_top;i++)
    if (added_transforms[i]->type == type)
      return(added_transforms[i]->xlabel);
  return("unknown");
}

static Float added_transform_lo(int type)
{
  int i;
  for (i=0;i<added_transforms_top;i++)
    if (added_transforms[i]->type == type)
      return(added_transforms[i]->lo);
  return(0.0);
}

static Float added_transform_hi(int type)
{
  int i;
  for (i=0;i<added_transforms_top;i++)
    if (added_transforms[i]->type == type)
      return(added_transforms[i]->hi);
  return(1.0);
}

static SCM added_transform_proc(int type)
{
  int i;
  for (i=0;i<added_transforms_top;i++)
    if (added_transforms[i]->type == type)
      return(added_transforms[i]->proc);
  return(SCM_BOOL_F);
}

#else
char *added_transform_name(int type) {return("unknown");}
#endif

static fft_window_state *fft_windows[NUM_CACHED_FFT_WINDOWS];


/* -------------------------------- HANKEL TRANSFORM -------------------------------- */

#if HAVE_GSL
  /* cc -o besi0 besi0.c -I/usr/local/include/gsl /usr/local/lib/gsl/libgsldht.a /usr/local/lib/gsl/libgslspecfunc.a /usr/local/lib/gsl/libgslerr.a */
#include <gsl/gsl_dht.h>

static void hankel_transform(int size, Float *input, Float *output)
{
  /* args size, bessel limit, xmax */
  gsl_dht_transform *t;
  double *in1,*out1;
  int i;
  t = gsl_dht_transform_new(size/2, 0.0, (double)(size/2));
  if (sizeof(Float) == sizeof(double))
    gsl_dht_transform_apply(t,(double *)input,(double *)output); /* the casts exist only to squelch dumb compiler complaints */
  else
    {
      in1 = (double *)CALLOC(size,sizeof(double));
      out1 = (double *)CALLOC(size,sizeof(double));
      for (i=0;i<size;i++) in1[i] = (double)input[i];
      gsl_dht_transform_apply(t,in1,out1);
      for (i=0;i<size;i++) output[i] = (Float)out1[i];
      FREE(in1);
      FREE(out1);
    }
  gsl_dht_transform_free(t);
}

#else
/*
 * Abel transform followed by fft
 *
 * taken (with modifications) from cwplib abel.c and hankel.c by
 *   Dave Hale and Lydia Deng, Colorado School of Mines, 06/01/90 
 *   that code: Copyright (c) Colorado School of Mines, 1995. All rights reserved.
 * 
 * Original reference:
 *   Hansen, E. W., 1985, Fast Hankel transform algorithm:  IEEE Trans. on
 *   Acoustics, Speech and Signal Processing, v. ASSP-33, n. 3, p. 666-671.
 */

#define NSE 9
static Float h[NSE] = {1.000000000000000000,0.610926299405048390,0.895089852938535935,1.34082948787002865,2.02532848558443890,
		       3.18110895533701843,5.90898360396353794,77.6000213494180286,528.221800846070892};    
static Float lambda[NSE] = {0.000000000000000000,-2.08424632126539366,-5.78928630565552371,-14.6268676854951032,
			    -35.0617158334443104,-83.3258406398958158,-210.358805421311445,-6673.64911325382036,-34897.7050244132261};

typedef struct abeltStruct {int n; Float **a,**b0,**b1;} abelt;
static abelt *at = NULL;

static void make_abel_transformer(int n)
{
  int i,j,nse=NSE;
  Float **a,**b0,**b1,fi,hj,lambdaj,scale,temp;
  if ((!at) || (at->n != n))
    {
      if (at) {for (i=0;i<at->n;i++) {FREE(at->a[i]); FREE(at->b0[i]); FREE(at->b1[i]);} FREE(at->a); FREE(at->b0); FREE(at->b1);}
      else at = (abelt *)CALLOC(1,sizeof(abelt));
      a = (Float **)CALLOC(n,sizeof(Float *));
      b0 = (Float **)CALLOC(n,sizeof(Float *));
      b1 = (Float **)CALLOC(n,sizeof(Float *));
      for (i=0;i<n;i++) 
	{
	  a[i] = (Float *)CALLOC(nse,sizeof(Float));
	  b0[i] = (Float *)CALLOC(nse,sizeof(Float));
	  b1[i] = (Float *)CALLOC(nse,sizeof(Float));
	}
      for (i=1; i<n; ++i) 
	{
	  fi = (Float)i+1.0;
	  for (j=0; j<nse; ++j) 
	    {
	      hj = h[j];
	      lambdaj = lambda[j];
	      a[i][j] = temp = pow(fi/(fi-1.0),lambdaj);
	      temp *= fi/(fi-1.0);
	      scale = 2.0*hj*(fi-1.0) / ((lambdaj+1.0)*(lambdaj+2.0));				
	      b0[i][j] = scale * (fi-1.0+(lambdaj+2.0-fi)*temp);
	      b1[i][j] = -scale * (lambdaj+1.0+fi-fi*temp);
	    }
	}
      at->n = n;
      at->a = a;
      at->b0 = b0;
      at->b1 = b1;
    }
}

static void abel (Float *f, Float *g)
{
  int i,j,n,nse=NSE;
  Float **a,**b0,**b1,xi[NSE],sum,fi,fip1;
  n = at->n;
  a = at->a;
  b0 = at->b0;
  b1 = at->b1;
  fi = f[n-1];
  g[0] = 0.5*f[0]+fi;
  for (j=0,sum=0.0; j<nse; ++j)
    {
      xi[j] = b1[n-1][j]*fi;
      sum += xi[j];
    }
  g[n-1] = sum;
  for (i=n-2; i>0; --i) 
    {
      fip1 = fi;
      fi = f[i];
      g[0] += fi;
      for (j=0,sum=0.0; j<nse; ++j) 
	{
	  xi[j] = a[i][j]*xi[j] + b0[i][j]*fip1 + b1[i][j]*fi;
	  sum += xi[j];
	}
      g[i] = sum;
    }
  g[0] *= 2.0;
}

/*
;;; test cases:

(with-sound (:output "j") ;sum of bessel funcs
  (loop for i from 0 below 1024 and r from 0.0 by (/ 1.0 1024) do 
    (outa i (* .5 (+ (bes-jn 0 (* 500 pi r)) 
                     (bes-jn 0 (* 150 pi r)))))))

(with-sound (:output "jinc") ;jinc
  (loop for i from 0 below 1024 and r from 0.0 by (/ 1.0 1024) do
    (if (= r 0.0)
	(outa i .999)
      (outa i (/ (bes-j1 (* 40 pi r)) (* r 20 pi))))))
*/
#endif



/* -------------------------------- WAVELET TRANSFORM -------------------------------- */
/* 
 * taken from wavelets.cl in clm which is taken from
 * M. J. Shensa Naval Ocean Systems Center,
 * Wickerhauser "Adapted Wavelet Analysis", 
 * and the UBC Imager Wavelet Package by Bob Lewis
 */

static int data1_size = 0;
static Float *data1 = NULL;

static void wavelet_transform(Float *data, int num, Float *cc, int cc_size)
{
  int i,jf,i1,k,i2,sig;
  int n;
  Float val;
  if (num<4) return;
  for (n=num;n>=4;n>>=1)
    {
      if (n > data1_size)
	{
	  if (data1) FREE(data1);
	  data1 = (Float *)CALLOC(n,sizeof(Float));
	  data1_size = n;
	}
      else for (i=0;i<n;i++) data1[i]=0.0;
      jf = (int)(cc_size*(n-0.5));
      for (i1=0,i=1,i2=(n>>1);i<=n;i+=2,i1++,i2++)
	{
	  sig=-1;
	  for (k=0;k<cc_size;k++)
	    {
	      val = data[(i+jf+k-1) % n];
	      data1[i1] += (cc[k] * val);
	      data1[i2] += (sig * cc[cc_size-k-1] * val);
	      sig = -sig;
	    }
	}
      for (i=0;i<n;i++) data[i] = data1[i];
    }
}

static Float daub4[4] = {0.4829629131445341, 0.8365163037378079, 0.2241438680420134, -0.1294095225512604};
static Float daub6[6] = {0.332670552950, 0.806891509311, 0.459877502118, -0.135011020010, -0.085441273882, 0.035226291886};
static Float daub8[8] = {0.230377813309, 0.714846570553, 0.630880767930, -0.027983769417, -0.187034811719, 0.030841381836, 
			 0.032883011667, -0.010597401785};
static Float daub10[10] = {0.160102397974, 0.603829269797, 0.724308528438, 0.138428145901, -0.242294887066, -0.032244869585,
			   0.077571493840, -0.006241490213, -0.012580751999, 0.003335725285};
static Float daub12[12] = {0.111540743350, 0.494623890398, 0.751133908021, 0.315250351709, -0.226264693965, -0.129766867567,
			   0.097501605587, 0.027522865530, -0.031582039317, 0.000553842201, 0.004777257511, -0.001077301085};
static Float daub14[14] = {0.077852054085, 0.396539319482, 0.729132090846, 0.469782287405, -0.143906003929, -0.224036184994,
			   0.071309219267, 0.080612609151, -0.038029936935, -0.016574541631, 0.012550998556, 0.000429577973,
			   -0.001801640704, 0.000353713800};
static Float daub16[16] = {0.054415842243, 0.312871590914, 0.675630736297, 0.585354683654, -0.015829105256, -0.284015542962,
			   0.000472484574, 0.128747426620, -0.017369301002, -0.044088253931, 0.013981027917, 0.008746094047,
			   -0.004870352993, -0.000391740373, 0.000675449406, -0.000117476784};
static Float daub18[18] = {0.038077947364, 0.243834674613, 0.604823123690, 0.657288078051, 0.133197385825, -0.293273783279,
			   -0.096840783223, 0.148540749338, 0.030725681479, -0.067632829061, 0.000250947115, 0.022361662124,
			   -0.004723204758, -0.004281503682, 0.001847646883, 0.000230385764, -0.000251963189, 0.000039347320};
static Float daub20[20] = {0.026670057901, 0.188176800077, 0.527201188931, 0.688459039453, 0.281172343661, -0.249846424327,
			   -0.195946274377, 0.127369340336, 0.093057364604, -0.071394147166, -0.029457536822, 0.033212674059,
			   0.003606553567, -0.010733175483, 0.001395351747, 0.001992405295, -0.000685856695, -0.000116466855,
			   0.000093588670, -0.000013264203};
static Float Battle_Lemarie[24] = {-0.0028284274, -0.004242641, 0.008485282, 0.008485282, -0.018384777, -0.016970564, 0.042426407, 
				   0.032526914, -0.11030867, -0.049497478, 0.4341636, 0.7665038, 0.4341636, -0.049497478, -0.11030867, 
				   0.032526914, 0.042426407, -0.016970564, -0.018384777, 0.008485282, 0.008485282, -0.004242641, -0.0028284274, 0.0};
static Float Burt_Adelson[6] = {-0.07071068, 0.3535534, 0.8485282, 0.3535534, -0.07071068, 0.0};
static Float Beylkin[18] = {0.099305765374353, 0.424215360812961, 0.699825214056600, 0.449718251149468,
			    -.110927598348234, -.264497231446384, 0.026900308803690, 0.155538731877093,
			    -.017520746266529, -.088543630622924, 0.019679866044322, 0.042916387274192,
			    -.017460408696028, -.014365807968852, 0.010040411844631, .0014842347824723,
			    -.002736031626258, .0006404853285212};
static Float coif2[6] = {0.038580775, -0.12696913, -0.07716155, 0.6074917, 0.74568766, 0.2265843};
static Float coif4[12] = {0.0011945726958388, -0.01284557955324, 0.024804330519353, 0.050023519962135, -0.15535722285996,
			  -0.071638282295294, 0.57046500145033, 0.75033630585287, 0.28061165190244, -0.0074103835186718,
			  -0.014611552521451, -0.0013587990591632};
static Float coif6[18] = {-0.0016918510194918, -0.00348787621998426, 0.019191160680044, 0.021671094636352, -0.098507213321468,
			  -0.056997424478478, 0.45678712217269, 0.78931940900416, 0.38055713085151, -0.070438748794943,
			  -0.056514193868065, 0.036409962612716, 0.0087601307091635, -0.011194759273835, -0.0019213354141368,
			  0.0020413809772660, 0.00044583039753204, -0.00021625727664696};
static Float sym2[5] = {-0.1767767, 0.3535534, 1.0606602, 0.3535534, -0.1767767};
static Float sym3[4]= {0.1767767, 0.5303301, 0.5303301, 0.1767767};
static Float sym4[10] = {0.033145633, -0.066291265, -0.1767767, 0.4198447, 0.994369, 0.4198447, -0.1767767, -0.066291265, 0.033145633, 0.0};
static Float sym5[8] = {0.066291265, -0.19887379, -0.15467963, 0.994369, 0.994369, -0.15467963, -0.19887379, 0.066291265};
static Float sym6[16] = {-0.0030210863, -0.009063259, -0.016831767, 0.07466399, 0.03133298, -0.30115914, -0.026499243, 
			 0.9516422, 0.9516422, -0.026499243, -0.30115914, 0.03133298, 0.07466399, -0.016831767, -0.009063259, -0.0030210863};

static char *wavelet_names[]={"daub4","daub6","daub8","daub10","daub12","daub14","daub16","daub18","daub20",
			      "battle_lemarie","burt_adelson","beylkin","coif2","coif4","coif6",
			      "sym2","sym3","sym4","sym5","sym6"};
static int wavelet_sizes[]={4,6,8,10,12,14,16,18,20,
			    24,6,18,6,12,18,
			    5,4,10,8,16};
static Float *wavelet_data[]={daub4,daub6,daub8,daub10,daub12,daub14,daub16,daub18,daub20,
			      Battle_Lemarie,Burt_Adelson,Beylkin,coif2,coif4,coif6,
			      sym2,sym3,sym4,sym5,sym6};


/* tested with pulse train and oboe.snd */

/* -------------------------------- CHEBYSHEV TRANSFORM -------------------------------- */

/* saving the polynomials speeds up the transform by a factor of 2 (added 15-Oct-98) */
#define MAX_PN_SIZE 1024
static double **saved_Pn = NULL;
static double *saved_wx = NULL;
static int saved_Pn_size = 0;

static void chebyshev_polynomials(double x, double *f, int n)
{
  int k;
  double r,s;
  r = x;
  s = 1.0;
  f[0] = 1.0;
  for (k=1;k<n;k++)
    {
      f[k] = r;
      r = 2*r*x - s;
      s = f[k];
    }
}

static void build_Pn(int n)
{
  int k;
  double x,rate,ln2;
  if (n != saved_Pn_size)
    {
      if (saved_Pn_size > 0)
	{
	  for (k=0;k<saved_Pn_size;k++) if (saved_Pn[k]) FREE(saved_Pn[k]);
	  if (saved_Pn) FREE(saved_Pn);
	  if (saved_wx) FREE(saved_wx);
	}
      saved_Pn = (double **)CALLOC(n,sizeof(double *));
      saved_wx = (double *)CALLOC(n,sizeof(double));
      for (k=0;k<n;k++) saved_Pn[k] = (double *)CALLOC(n,sizeof(double));
      saved_Pn_size = n;
      ln2 = log(n)/log(2);
      rate = 2.0/(Float)n;
      for (k=0,x=-1.0;k<n;k++,x+=rate) 
	{
	  chebyshev_polynomials(x,saved_Pn[k],n-1);
	  if ((x == 1.0) || (x == -1.0))
	    saved_wx[k] = ln2;
	  else saved_wx[k] = 1.0/sqrt(1.0 - x*x);
	}
    }
}

static void chebyshev_transform(Float *data, int n)
{
  double *An,*Pn=NULL;
  int i,k;
  double x,rate,wx,wfx,ln2;
  rate = 2.0/(Float)n;
  ln2 = log(n)/log(2);
  An = (double *)CALLOC(n,sizeof(double));
  if (n > MAX_PN_SIZE) 
    Pn = (double *)CALLOC(n,sizeof(double));
  else build_Pn(n);
  x = -1.0;
  for (k=0;k<n;k++)
    {
      if (n > MAX_PN_SIZE)
	{
	  if ((x == 1.0) || (x == -1.0))
	    wx = ln2;
	  else wx = 1.0/sqrt(1.0 - x*x);
	  wfx = wx * data[k];
	  chebyshev_polynomials(x,Pn,n-1);
	  x += rate;
	}
      else 
	{
	  Pn = saved_Pn[k];
	  wfx = data[k] * saved_wx[k];
	}
      for (i=0;i<n;i++) An[i] += (wfx * Pn[i]);
    }
  for (i=0;i<n;i++) data[i] = An[i];
  FREE(An);
  if (n > MAX_PN_SIZE) FREE(Pn);
}

/* 
;;; test case (bessel.lisp):

   (with-sound (:output "wave") 
     (let ((a (make-array 8 :initial-contents '(0 0 0 .25 0 0 0 0.5))))
       (loop for i from 0 below 256 do (outa i (* .5 (chebyshev-polynomial a (- (/ i 128) 1.0)))))))
*/


/* -------------------------------- WALSH TRANSFORM -------------------------------- */

/* borrowed from walsh/walshdit2.cc in the fxt package fxt970929.tgz written by (and copyright) Joerg Arndt
 * arndt@spektracom.de, arndt@jjj.de, http://www.spektracom.de/~arndt/joerg.html, http://www.jjj.de/fxt/
 * fxt.doc appears to say I can use it here (Snd is freeware and I've modified the original to some extent).
 */

static void walsh_transform(Float *data, int n)
{
  int i,j,k,m,m2,i1,i2,ipow,n2;
  Float f1,f2;
  n2 = n>>1;
  ipow = (int)(log(n)/log(2));
  for (i=1,j=0;i<n-1;i++)
    {
      for (k=n2;(!((j^=k)&k)); k>>=1);
      if (j>i) {f1=data[i]; data[i]=data[j]; data[j]=f1;}
    }
  for (i=1;i<=ipow;i++) /* for (i=ipow;i>0;i--) */
    {
      m = (1<<i);
      m2 = m>>1;
      for (j=0;j<m2;j++)
	{
	  for (k=0;k<n;k+=m)
	    {
	      i1 = k+j;
	      i2 = i1+m2;
	      f1 = data[i1];
	      f2 = data[i2];
	      data[i1] = f1+f2;
	      data[i2] = f1-f2;
	    }
	}
    }
}

/*
(with-sound () 
  (loop for i from 0 to 63 do (outa i .15)) 
  (loop for i from 64 to 127 do (outa i -.15)) 
  (loop for i from 128 to 191 do (outa i .15)) 
  (loop for i from 192 to 256 do (outa i -.15)) 
  (loop for i from 0 to 256 do (outa i .05)))
  */



/* -------------------------------- AUTOCORRELATION -------------------------------- */

static void autocorrelation(Float *data, int n)
{
  Float *rl,*im;
  Float fscl;
  int i;
  fscl = 2.0/(Float)n;
  rl = (Float *)CALLOC(n,sizeof(Float));
  im = (Float *)CALLOC(n,sizeof(Float));
  for (i=0;i<n;i++) rl[i] = data[i];
  mus_fft(rl,im,n,1);
  for (i=0;i<n;i++)
    {
      rl[i] = rl[i]*rl[i] + im[i]*im[i];
      im[i] = 0.0;
    }
  mus_fft(rl,im,n,-1);
  for (i=0;i<n/2;i++) data[i] = fscl*rl[i];
  FREE(rl);
  FREE(im);
}



/* -------------------------------- CEPSTRUM -------------------------------- */
/* is this useful? correct? */

static void cepstrum(Float *data, int n)
{
  Float *rl,*im;
  Float fscl=0.0,lowest;
  int i;
  lowest=0.00000001;
  fscl = 2.0/(Float)n;
  rl = (Float *)CALLOC(n,sizeof(Float));
  im = (Float *)CALLOC(n,sizeof(Float));
  for (i=0;i<n;i++) rl[i] = data[i];
  mus_fft(rl,im,n,1);
  for (i=0;i<n;i++)
    {
      rl[i] = rl[i]*rl[i] + im[i]*im[i];
      if (rl[i] < lowest)
	rl[i] = log(lowest);
      else rl[i] = log(sqrt(rl[i]));
      im[i] = 0.0;
    }
  mus_fft(rl,im,n,-1);
  for (i=0;i<n;i++) if (fabs(rl[i]) > fscl) fscl = fabs(rl[i]);
  if (fscl > 0.0)
    for (i=0;i<n;i++) data[i] = rl[i]/fscl;
  FREE(rl);
  FREE(im);
}


/* -------------------------------- HADAMARD TRANSFORM -------------------------------- */

/* 
 * Fast Hadamard-Walsh-Rademacher Transform: 
 *
 * Reference:
 * "Hadamard Transform Image Coding"
 * by William K. Pratt, Julius Kane, and Harry C. Andrews
 * Proceedings of the IEEE, Vol. 57, No.1, Jan. 1969 
 *
 * Implementation by C.C.Gumas 1/13/93 (slightly reformatted by bil for snd 30-Jan-99)
 */

static void fast_hwt_first_stage (int local_half_size, Float *out, Float *in) 
{
  Float tmp0, tmp1;
  int k, j, i;
  for (i=0, j=local_half_size, k=0; i<local_half_size; ) 
    {
      tmp0 = in[k++]; 
      tmp1 = in[k++];
      out[i++] = tmp0 + tmp1;
      out[j++] = tmp0 - tmp1;
    }
}

static void fast_hwt_stage (int n, int local_size, int local_half_size, Float *out, Float *in) 
{
  Float tmp0, tmp1;
  int k, j, i;
  for (i=0, j=local_half_size, k=0; i<local_half_size; ) 
    {
      tmp0 = in[k++]; 
      tmp1 = in[k++];
      out[i++] = tmp0 + tmp1;
      out[j++] = tmp0 - tmp1;
      tmp0 = in[k++]; 
      tmp1 = in[k++];   /* k gets incremented by 2 each time thru */
      out[i++] = tmp0 + tmp1;
      out[j++] = tmp1 - tmp0;
    }
  local_size = local_half_size;
  local_half_size >>= 1;
  if (n>2) 
    {             
      n -= 1;
      fast_hwt_stage (n, local_size, local_half_size, in, out);
      fast_hwt_stage (n, local_size, local_half_size, &in[local_size], &out[local_size]);  
    }
  else
    {
      if (n == 2) 
	{
	  fast_hwt_first_stage (local_half_size, in, out);
	  fast_hwt_first_stage (local_half_size, &in[local_size], &out[local_size]);  
	}
    }
}

static void fast_hwt (Float *out, Float *in, int n)
{
  int size;
  int need_to_switch_on_output = 0;
  size = (0x0001 << n);
  if ((n % 2) == 0) need_to_switch_on_output = 1;
  fast_hwt_stage (n, size, (size>>1), out, in);
  if (need_to_switch_on_output) memcpy (out, in, size*sizeof(in[0])); 
}



/* -------------------------------- FFT DATA WINDOW -------------------------------- */

int make_fft_window_1(Float *window, int size, int type, Float pre_beta, int scaled)
{
  int i;
  mus_make_fft_window_with_window(type,size,pre_beta * beta_maxes[type],window);
  if (scaled) for (i=0;i<size;i++) {window[i] *= MUS_FIX_TO_FLOAT;}
  return(1);
}

static int compare_peaks(const void *pk1, const void *pk2)
{
  if (((fft_peak *)pk1)->freq > ((fft_peak *)pk2)->freq) return(1);
  else if (((fft_peak *)pk1)->freq == ((fft_peak *)pk2)->freq) return(0);
  return(-1);
}

int find_and_sort_peaks(Float *buf, fft_peak *found, int num_peaks, int size)
{ /* in the fft peak finder below we assume data between 0 and 1 */
  int i,j,pks,minpk;
  Float minval,la,ra,ca;
  Float *peaks;
  int *inds;
  if (num_peaks <= 0) return(0);
  peaks = (Float *)CALLOC(num_peaks,sizeof(Float));
  inds = (int *)CALLOC(num_peaks,sizeof(int));
  pks = 0;
  la = 0.0;
  ca = 0.0;
  ra = 0.0;
  minval = 0.00001;
  for (i=0;i<size;i++)
    {
      la = ca;
      ca = ra;
      ra = buf[i];
      if ((ca > minval) && (ca > ra) && (ca > la))
	{
	  if (pks < num_peaks)
	    {
	      inds[pks] = i-1;
	      peaks[pks++] = ca;
	    }
	  else
	    {
	      minval = peaks[0];
	      minpk = 0;
	      for (j=1;j<num_peaks;j++)
		{
		  if (peaks[j] < minval) 
		    {
		      minval = peaks[j];
		      minpk = j;
		    }
		}
	      if (ca > minval)
		{
		  inds[minpk] = i-1;
		  peaks[minpk] = ca;
		}
	    }
	}
    }
  for (i=0;i<pks;i++)
    {
      j = inds[i];
      ca = buf[j];
      found[i].amp = buf[j];
      found[i].freq = j;
    }
  if (pks > 0) qsort((void *)found,pks,sizeof(fft_peak),compare_peaks);
  FREE(peaks);
  FREE(inds);
  return(pks);
}

int find_and_sort_fft_peaks(Float *buf, fft_peak *found, int num_peaks, int fftsize2, int srate, Float samps_per_pixel, Float fft_scale)
{
  /* we want to reflect the graph as displayed, so each "bin" is samps_per_pixel wide */
  int i,j,k,pks,minpk,hop,pkj,oldpkj;
  Float minval,la,ra,ca,logca,logra,logla,offset,fscl,ascl;
  Float *peaks;
  int *inds;
  peaks = (Float *)CALLOC(num_peaks,sizeof(Float));
  inds = (int *)CALLOC(num_peaks,sizeof(int));
  fscl = (Float)srate/(Float)fftsize2;
  hop = (int)(samps_per_pixel+0.5);
  if (hop < 1) hop=1;
  pks = 0;
  la = 0.0;
  ca = 0.0;
  ra = 0.0;
  minval = (Float)fftsize2/100000.0;
  ascl = 0.0;
  pkj = 0;
  for (i=0;i<fftsize2-hop;i+=hop)
    {
      la = ca;
      ca = ra;
      oldpkj = pkj;
      ra = 0.0;
      for (k=0;k<hop;k++) 
	{
	  if (buf[i+k]>ra) {pkj = i+k; ra=buf[pkj];} /* reflect user's view of the graph */
	}
      if ((ca > minval) && (ca > ra) && (ca > la))
	{
          if (ascl < ca) ascl = ca;
	  if (pks < num_peaks)
	    {
	      inds[pks] = oldpkj;
	      peaks[pks] = ca;
	      pks++;
	    }
	  else
	    {
	      minval = peaks[0];
	      minpk = 0;
	      for (j=1;j<num_peaks;j++)
		{
		  if (peaks[j] < minval) 
		    {
		      minval = peaks[j];
		      minpk = j;
		    }
		}
	      if (ca > minval)
		{
		  inds[minpk] = oldpkj;
		  peaks[minpk] = ca;
		}
	    }
	}
    }
  /* now we have the peaks; turn these into interpolated peaks/amps, and sort */
  if (fft_scale > 0.0) ascl = fft_scale; else {if (ascl > 0.0) ascl = 1.0/ascl; else ascl = 1.0;}
  for (i=0,k=0;i<pks;i++)
    {
      j = inds[i];
      ca = buf[j]*ascl;
      if (j>0) la = buf[j-1]*ascl; else la = ca;
      ra = buf[j+1]*ascl; 
      if (la<.00001) la=.00001;
      if (ra<.00001) ra=.00001;
      logla = log10(la);
      logca = log10(ca);
      logra = log10(ra);
      offset = (0.5*(logla-logra))/(logla+logra-2*logca); /* this assumes amps<1.0 (from XJS sms code) */
      found[k].amp = pow(10.0,logca-0.25*offset*(logla-logra));
      found[k].freq = fscl*(j+offset);
#ifdef DEBUGGING
      if ((isnan(found[k].amp)) || (isnan(found[k].freq)))
	{
	  snd_error("%s[%d] %s: peak[%d] NaN: %f %f\n",__FILE__,__LINE__,__FUNCTION__,i,found[k].amp,found[k].freq);
	  abort();
	}
#endif
      if (found[k].freq < 0.0) found[k].freq = 0.0;
      if (found[k].amp > .0005) k++; /* this is normalized to 1.0 via ascl above */
    }
  qsort((void *)found,pks,sizeof(fft_peak),compare_peaks);
  FREE(peaks);
  FREE(inds);
  return(k);
}


static int scramble_fft_state(fft_state *fs)
{
  int i,j,m;
  Float vr;
  Float *data;
  data = fs->data;
  if (!data) {snd_error("no fft data!"); return(1);}
  fs->n=(fs->nn*2);
  j=1;
  for (i=1;i<fs->n;i+=2) 
    {
      if (j > i) 
	{
	  vr=data[j];
	  data[j]=data[i];
	  data[i]=vr;
	  vr=data[j+1];
	  data[j+1]=data[i+1];
	  data[i+1]=vr;
	}
      m=fs->n >> 1;
      while (m >= 2 && j > m) 
	{
	  j -= m;
	  m >>= 1;
	}
      j += m;
    }
  fs->mmax=2;
  fs->outer = 1;
  return(1);
}

static int snd_fft(fft_state *fs)
{
  double wtemp;
  Float vr,vi;
  Float *data;
  int k,j;
  data = fs->data;
  if (fs->n == fs->mmax) return(1);
  if (fs->outer)
    {
      fs->outer = 0;
      fs->istep=2*fs->mmax;
      fs->angle=6.28318530717959/fs->mmax;
      wtemp=sin(0.5*fs->angle);
      fs->c = -2.0*wtemp*wtemp;
      fs->s=sin(fs->angle);
      fs->wr=1.0;
      fs->wi=0.0;
      fs->m = 1;
      fs->inner = 1;
    }
  if (fs->inner)
    {
      fs->i = fs->m;
      fs->inner = 0;
    }
  k=0;
LOOP:
  j=fs->i+fs->mmax;
  vr=fs->wr*data[j]-fs->wi*data[j+1];
  vi=fs->wr*data[j+1]+fs->wi*data[j];
  data[j]=data[fs->i]-vr;
  data[j+1]=data[fs->i+1]-vi;
  data[fs->i] += vr;
  data[fs->i+1] += vi;
  k++;
  fs->i += fs->istep;
  if (fs->i > fs->n)
    {
      fs->inner = 1;
      fs->wr=(wtemp=fs->wr)*fs->c-fs->wi*fs->s+fs->wr;
      fs->wi=fs->wi*fs->c+wtemp*fs->s+fs->wi;
      fs->m += 2;
      if (fs->m >= fs->mmax)
	{
	  fs->outer = 1;
	  fs->mmax = fs->istep;
	}
    }
  else 
    if (k < 100) goto LOOP;
  return(0);
}

static int snd_fft_cleanup(fft_state *fs)
{
  double wtemp;
  int n2p3,i,i1,i2,i3,i4;
  Float h1r,h1i,h2r,h2i;
  Float *data;
  data = fs->data;
  fs->angle=3.141592653589793/((double)fs->nn);
  wtemp=sin(0.5*fs->angle);
  fs->c = -2.0*wtemp*wtemp;
  fs->s=sin(fs->angle);
  fs->wr=1.0+fs->c;
  fs->wi=fs->s;
  n2p3=2*fs->nn+3;
  for (i=2;i<=(fs->nn/2);i++) 
    {
      i1=i+i-1;
      i2=i1+1;
      i3=n2p3-i2;
      i4=1+i3;
      h1r=0.5*(data[i1]+data[i3]);
      h1i=0.5*(data[i2]-data[i4]);
      h2r=0.5*(data[i2]+data[i4]);
      h2i=(-0.5)*(data[i1]-data[i3]);
      data[i1]=h1r+fs->wr*h2r-fs->wi*h2i;
      data[i2]=h1i+fs->wr*h2i+fs->wi*h2r;
      data[i3]=h1r-fs->wr*h2r+fs->wi*h2i;
      data[i4] = -h1i+fs->wr*h2i+fs->wi*h2r;
      fs->wr=(wtemp=fs->wr)*fs->c-fs->wi*fs->s+fs->wr;
      fs->wi=fs->wi*fs->c+wtemp*fs->s+fs->wi;
    }
  h1r=data[1];
  data[1] = h1r+data[2];
  data[2] = h1r-data[2];
  return(1);
}

static int snd_fft_to_spectrum (fft_state *fs)
{
  int i,j;
  Float val = 0.0;
  Float *fft_data;
  snd_state *ss;
  fft_data = fs->data;
  ss = fs->ss;
  if (transform_type(ss) == HANKEL) /* we only get here if not HAVE_GSL */
    {
      for (j=0;j<fs->size;j+=2)
	{
	  if (fft_data[j]>val) 
	    val = fft_data[j];
	  else
	    if ((-fft_data[j] ) > val)
	      val = (-fft_data[j]);
	}
      for (i=0,j=0;i<fs->size;j++,i+=2)
	{
	  fft_data[j] = fft_data[i] * val;
	}
    }
  else
    {
      if (fft_data[1] < 0.0001) fft_data[0] = 0.0; else fft_data[0] = fft_data[1];
      if (fft_data[2] < 0.0001) fft_data[fs->size-1] = 0.0; else fft_data[fs->size-1] = fft_data[2];
      for (i=3,j=1;i<fs->size-3;i+=2,j++)
	{
	  fft_data[j] = hypot(fft_data[i],fft_data[i+1]);
	}
      /* if fft_data[i]==0 pi/2 else atan2(fft_data[i],fft_data[i+1]) */
    }
  return(1);
}

static int make_fft_window(fft_state *fs)
{
  /* build fft window, taking int->Float transition into account */
  fft_window_state *wp;
  int toploc;
  snd_state *ss;
  ss = fs->ss;
  switch (transform_type(ss))
    {
    case FOURIER:
      wp = (fft_window_state *)(fs->wp);
      toploc = (int)(fs->size / (Float)(1 + fs->pad_zero));
      return(make_fft_window_1(wp->window,toploc,wp->type,wp->beta,TRUE));
      break;
    case HANKEL: 
      if (fs->size != fs->hwin_size)
	{
	  fs->hwin_size = fs->size;
	  if (fs->hwin) FREE(fs->hwin);
	  fs->hwin = (Float *)CALLOC(fs->size,sizeof(Float));
	}
      break;
    }
  return(1);
}

static void free_fft_window(int i)
{
  if (fft_windows[i])
    {
      if (((fft_window_state *)fft_windows[i])->window) FREE(((fft_window_state *)fft_windows[i])->window);
      FREE(fft_windows[i]);
      fft_windows[i]=NULL;
    }
}

static fft_state *free_fft_state(fft_state *fs)
{
  if (fs) 
    {
      if (fs->fw_slot == -1) FREE(fs->wp); /* free window only if it's not in the cache */
      if (fs->hwin) FREE(fs->hwin);
      FREE(fs); 
    }
  return(NULL);
}

static void decrement_fft_window_use(fft_state *fs)
{
  if (fs->fw_slot != -1)
    fft_windows[fs->fw_slot]->inuse--;
}

static int set_up_fft_window(fft_state *fs)
{
  int i,empty,ok,unused;
  fft_window_state *wp;
  snd_state *ss;
  ss = fs->ss;
  if (transform_type(ss) != FOURIER) return(1);
  /* first look to see if it already exists */
  empty = -1;
  ok = -1;
  unused = -1;
  for (i=0;i<NUM_CACHED_FFT_WINDOWS;i++)
    {
      wp = fft_windows[i];
      if (!wp) 
	{
	  if (empty == -1) empty = i;
	}
      else
	{
	  if (!wp->inuse) unused = i;
	  if (wp->size == fs->size) 
	    {

	      if ((wp->type == fs->wintype) && (wp->beta == fft_beta(ss)) && (wp->pad_zero == fs->pad_zero))
		{
		  fs->wp = wp;
		  fs->fw_slot = i;
		  wp->inuse++;
		  return(2);  /* skip making window */
		}
	      if (ok == -1) ok = i;
	    }
	}
    }
  if (empty == -1) empty = ok;
  if (empty == -1) empty = unused;
  if (empty == -1) 
    {
      wp = (fft_window_state *)CALLOC(1,sizeof(fft_window_state));
      fs->fw_slot = -1;
    }
  else
    {
      if (empty == unused) free_fft_window(empty);
      if (!fft_windows[empty])
	{
	  fft_windows[empty] = (fft_window_state *)CALLOC(1,sizeof(fft_window_state));
	}
      wp = fft_windows[empty];
      fs->fw_slot = empty;
    }
  fs->wp = wp;
  wp->size = fs->size;
  wp->pad_zero = fs->pad_zero;
  wp->type = fs->wintype;
  wp->beta = fft_beta(ss);
  fs->beta = wp->beta;
  wp->inuse++;
  if (!wp->window) wp->window = (Float *)CALLOC(fs->size,sizeof(Float));
  return(1);
}

 
/*-------------------------------- FFT_INFO -------------------------------- */

static fft_info *make_fft_info(int size, int window, Float beta)
{
  fft_info *fp;
  fp = (fft_info *)CALLOC(1,sizeof(fft_info));
  fp->size = size;
  fp->window = window;
  fp->beta = beta;
  fp->ok = 1;
  fp->data = (Float *)CALLOC(size+1,sizeof(Float)); /* +1 for complex storage or starts at 1 or something */
  return(fp);
}

fft_info *free_fft_info(fft_info *fp)
{
  fp->chan = NULL;
  if (fp->data) FREE(fp->data);
  if (fp->axis) free_axis_info(fp->axis);
  FREE(fp);
  return(NULL);
}



/* -------------------------------- FFT_WINDOW_STATE, FFT_STATE -------------------------------- 
 *
 * FFT as work proc, using the "realft" version, and splitting out as many steps as possible.
 * the basic fft here is taken from Xavier Serra's SMS program (CLM's is based on two 0-based arrays,
 * which in this case is not so useful).  Number of splits depends on the FFT size.
 */

static void make_sonogram_axes(chan_info *cp);

static char *spectro_xlabel(snd_state *ss)
{
  switch (transform_type(ss))
    {
    case FOURIER: 
      if (fft_log_frequency(ss))
	return(STR_log_freq);
      else return(STR_frequency);
      break;
    case WAVELET:         return(wavelet_names[wavelet_type(ss)]); break;
    case HANKEL:          return("Hankel spectrum"); break;
    case CHEBYSHEV:       return("Chebyshev spectrum"); break;
    case CEPSTRUM:        return("cepstrum"); break;
    case WALSH:           return("Sequency"); break;
    case HADAMARD:        return("Sequency"); break;
    case AUTOCORRELATION: return("Lag time"); break;
#if HAVE_GUILE
    default:             return(added_transform_xlabel(transform_type(ss))); break;
#endif
    }
  return(NULL);
}

static int snd_fft_set_up(fft_state *fs)
{
  /* allocate arrays if needed */
  fft_info *fp;
  chan_info *cp;
  cp = (chan_info *)(fs->chan);
  fp = cp->fft;
  if (!fp)                              /* associated channel hasn't done any ffts yet, so there's no struct */
    {
      cp->fft = make_fft_info(fs->size,fs->wintype,0.0);
      fp = cp->fft;
    }
  else
    {
      if ((!fp->ok) || (!fp->data) || (fs->size > fp->size))
	{
	  fp->size = fs->size;
	  if (fp->data) FREE(fp->data);
	  fp->data = (Float *)CALLOC(fp->size+1,sizeof(Float));
	  fp->ok = 1;
	}
    }
  fp->current_size = fs->size; /* protect against parallel size change via fft size menu */
  fs->data = fp->data;
  return(1);
}

static void make_sonogram_axes(chan_info *cp)
{
  snd_state *ss;
  fft_info *fp;
  axis_info *ap;
  Float max_freq,min_freq,yang;
  char *xlabel;
  fp = cp->fft;
  if (fp)
    {
      ap = cp->axis;
      ss = cp->state;
      if (transform_type(ss) == FOURIER)
	{
	  if ((fft_log_frequency(ss)) || (fft_style(ss) == SPECTROGRAM))
	    {
	      max_freq = spectro_cutoff(ss);
	      min_freq = spectro_start(ss);
	    }
	  else 
	    {
	      max_freq = spectro_cutoff(ss) * (Float)SND_SRATE(cp->sound) * 0.5;
	      min_freq = spectro_start(ss) * (Float)SND_SRATE(cp->sound) * 0.5;
	    }
	}
      else 
	{
	  if (transform_type(ss) == AUTOCORRELATION)
	    {
	      max_freq = fp->current_size * spectro_cutoff(ss) / 2;
	      min_freq = fp->current_size * spectro_start(ss) / 2;
	    }
	  else 
	    {
	      max_freq = fp->current_size * spectro_cutoff(ss);
	      min_freq = fp->current_size * spectro_start(ss);
	    }
	}
      yang = fmod(spectro_y_angle(ss),360.0);
      if (yang < 0.0) yang += 360.0;
      if (fft_style(ss) == SPECTROGRAM)
	{
	  if (transform_type(ss) == FOURIER)
	    {
	      if (yang < 45.0) xlabel = "frequency";
	      else if (yang < 135.0) xlabel = "time";
	      else if (yang < 225.0) xlabel = "ycneuqerf";
	      else if (yang < 315.0) xlabel = "emit";
	      else xlabel = "frequency";
	    }
	  else xlabel = spectro_xlabel(ss);
	}
      else xlabel = STR_time;
      fp->axis = make_axis_info(cp,ap->x0,ap->x1,min_freq,max_freq,xlabel,ap->x0,ap->x1,min_freq,max_freq,fp->axis);
    }
}

static int apply_fft_window(fft_state *fs)
{
  /* apply the window, reading data if necessary, resetting IO blocks to former state */
  int i,ind0,result = 5;
  Float *window,*fft_data;
  int data_len,pad = 0;
  MUS_SAMPLE_TYPE val;
  snd_fd *sf;
  chan_info *cp;
  snd_state *ss;
  ss = fs->ss;
  cp = (chan_info *)(fs->chan);
  fft_data = fs->data;
  
  if (transform_type(ss) == FOURIER) pad = fs->pad_zero;
  data_len = (int)(fs->size / (1+pad));
  if ((show_selection_transform(ss)) && (selection_is_current_in_channel(cp)))
    {
      ind0 = fs->databeg;
      if (fft_style(ss) == NORMAL_FFT) data_len = fs->datalen;
    }
  else 
    ind0 = (cp->axis)->losamp + fs->beg;
  sf = init_sample_read(ind0,cp,READ_FORWARD);
  switch (transform_type(ss))
    {
    case FOURIER:
      window = (Float *)((fft_window_state *)(fs->wp))->window;
      for (i=0;i<data_len;i++) 
	{
	  NEXT_SAMPLE(val,sf);
	  fft_data[i] = window[i] * val;
	}
      if (data_len < fs->size) for (i=data_len;i<fs->size;i++) fft_data[i] = 0.0;
      decrement_fft_window_use(fs);
      result = 1;
      break;
    case HANKEL:
      for (i=0;i<data_len;i++) fs->hwin[i] = next_sample(sf);
      if (data_len < fs->size) for (i=data_len;i<fs->size;i++) fs->hwin[i] = 0.0;
#if HAVE_GSL
      hankel_transform(data_len,fs->hwin,fft_data);
      result = 5;
#else
      make_abel_transformer(data_len);
      abel(fs->hwin,fft_data);
      result = 1;
#endif
      break;
    case WAVELET:
      for (i=0;i<data_len;i++) fft_data[i] = next_sample(sf);
      if (data_len < fs->size) for (i=data_len;i<fs->size;i++) fft_data[i] = 0.0;
      wavelet_transform(fft_data,fs->size,wavelet_data[wavelet_type(ss)],wavelet_sizes[wavelet_type(ss)]);
      break;
    case CHEBYSHEV:
      for (i=0;i<data_len;i++) fft_data[i] = next_sample(sf);
      if (data_len < fs->size) for (i=data_len;i<fs->size;i++) fft_data[i] = 0.0;
      chebyshev_transform(fft_data,fs->size);
      break;
    case CEPSTRUM:
      for (i=0;i<data_len;i++) fft_data[i] = next_sample(sf);
      if (data_len < fs->size) for (i=data_len;i<fs->size;i++) fft_data[i] = 0.0;
      cepstrum(fft_data,fs->size);
      break;
    case HADAMARD:
      window = (Float *)CALLOC(fs->size,sizeof(Float));
      for (i=0;i<data_len;i++) fft_data[i] = next_sample(sf);
      if (data_len < fs->size) for (i=data_len;i<fs->size;i++) fft_data[i] = 0.0;
      fast_hwt(window,fft_data,(int)(log((Float)(fs->size+1))/log(2.0)));
      for (i=0;i<fs->size;i++) fft_data[i] = window[i];
      FREE(window);
      break;
    case WALSH:
      for (i=0;i<data_len;i++) fft_data[i] = next_sample(sf);
      if (data_len < fs->size) for (i=data_len;i<fs->size;i++) fft_data[i] = 0.0;
      walsh_transform(fft_data,fs->size);
      break;
    case AUTOCORRELATION:
      for (i=0;i<data_len;i++) fft_data[i] = next_sample(sf);
      if (data_len < fs->size) for (i=data_len;i<fs->size;i++) fft_data[i] = 0.0;
      autocorrelation(fft_data,fs->size);
      break;
#if HAVE_GUILE
    default:
      {
	SCM res,sfd;
	vct *v;
	int len,i;
	sfd = g_c_make_sample_reader(sf);
	res = g_call2(added_transform_proc(transform_type(ss)),gh_int2scm(data_len),sfd);
	snd_protect(res);
	if (vct_p(res))
	  {
	    v = get_vct(res);
	    len = v->length;
	    for (i=0;i<len;i++) fft_data[i] = v->data[i];
	  }
	GH_SET_VALUE_OF(sfd,(SCM)NULL); /* don't let guile's gc mess with it */
	snd_unprotect(res);
      }
      break;
#endif      
    }
  free_snd_fd(sf);
  return(result);
}

static int display_snd_fft(fft_state *fs)
{
  fft_info *fp;
  chan_info *cp;
  snd_state *ss;
  axis_info *fap;
  Float max_freq=0.0,min_freq=0.0,max_val,min_val;
  char *xlabel;
  cp = (chan_info *)(fs->chan);
  ss = cp->state;
  if (fft_style(ss) == NORMAL_FFT)
    {
      fp = cp->fft;
      xlabel = spectro_xlabel(ss);
      switch (transform_type(ss))
	{
	case FOURIER: 
	  if (fft_log_frequency(ss))
	    {
	      max_freq = 1.0;
	      min_freq = 0.0;
	    }
	  else
	    {
	      max_freq = ((Float)(SND_SRATE(cp->sound)) * 0.5 * spectro_cutoff(ss));
	      min_freq = ((Float)(SND_SRATE(cp->sound)) * 0.5 * spectro_start(ss));
	    }
	  break;
	case WAVELET: case HANKEL: case CHEBYSHEV: case HADAMARD: case WALSH:
	  max_freq = fs->size * spectro_cutoff(ss); 
	  min_freq = fs->size * spectro_start(ss); 
	  break;
	case AUTOCORRELATION: case CEPSTRUM:
	  max_freq = fs->size * spectro_cutoff(ss) / 2; 
	  min_freq = fs->size * spectro_start(ss) / 2; 
	  break;
#if HAVE_GUILE
	default:
	  min_freq = added_transform_lo(transform_type(ss)) * fs->size * spectro_cutoff(ss); 
	  max_freq = added_transform_hi(transform_type(ss)) * fs->size * spectro_cutoff(ss); 
	  break;
#endif
	}
      fap = fp->axis;
      if (fft_log_magnitude(ss)) 
	{
	  max_val = 0.0; 
	  min_val = ss->min_dB;
	}
      else 
	{
	  if ((!(normalize_fft(ss))) && (fap) && ((fap->ymin > ss->min_dB) || (fap->ymax <= 1.0)))
	    {
	      max_val = fap->ymax; 
	      min_val = fap->ymin;
	    }
	  else
	    {
	      max_val = 1.0;
	      if (transform_type(ss) == FOURIER) min_val = 0.0; else min_val = -1.0;
	    }
	}
      fp->axis = make_axis_info(cp,
				min_freq,max_freq,
				min_val,max_val,
				xlabel,
				min_freq,max_freq,
				min_val,max_val,
				fap);
    }
  return(-1);
}

int fft_window_beta_in_use(int win) {return(win >= MUS_KAISER_WINDOW);}

void *make_fft_state(chan_info *cp, int simple)
{
  /* in simple fft case, (snd-xchn.c) calls this and passes it to fft_in_slices */
  /* we can cause the current fft to be re-used by setting slice to 8 */
  fft_state *fs = NULL;
  snd_state *ss;
  axis_info *ap;
  int reuse_old = 0,fftsize,dbeg=0,dlen=0;
  ss = cp->state;
  ap = cp->axis;
  
  if ((show_selection_transform(ss)) && (fft_style(ss) == NORMAL_FFT) && (selection_is_current_in_channel(cp)))
    {
      /* override fft_size(ss) in this case (sonograms cover selection but use preset size) */
      dbeg = selection_beg(cp);
      dlen = region_len(0);
      /* these need to be handled at the same time, and not re-examined until the next call */
      /* if we're sweeping the mouse defining the selection, by the time we get to apply_fft_window, region_len(0) can change */
      fftsize = (int)pow(2.0,(int)(ceil(log((Float)(dlen*(1+zero_pad(ss))))/log(2.0))));
      if (fftsize < 2) fftsize = 2;
      cp->selection_transform_size = fftsize;
    }
  else 
    {
      fftsize = fft_size(ss);
      cp->selection_transform_size = 0;
    }

  if ((simple) && (cp->fft_data) && (cp->selection_transform_size == 0))
    {
      fs = (fft_state *)(cp->fft_data);
      if ((fs->losamp == ap->losamp) && 
	  (fs->size == fftsize) &&
	  (fs->wintype == fft_window(ss)) &&
	  ((!(fft_window_beta_in_use(fs->wintype))) || (fs->beta == fft_beta(ss))) &&
	  (fs->dBing == fft_log_magnitude(ss)) &&
	  (fs->lfreq == fft_log_frequency(ss)) &&
	  (fs->pad_zero == zero_pad(ss)) &&
	  (fs->cutoff == spectro_cutoff(ss)) &&
	  (fs->t_type == transform_type(ss)) &&
	  (fs->old_style == fft_style(ss)) &&
	  (fs->wavelet_choice == wavelet_type(ss)) &&
	  (fs->edit_ctr == cp->edit_ctr))
	reuse_old = 1;
    }
  if (reuse_old)
    fs->slice = 8;
  else
    {
      if (cp->fft_data) cp->fft_data = free_fft_state((fft_state *)(cp->fft_data));
      fs = (fft_state *)CALLOC(1,sizeof(fft_state));
      fs->slice = 0;
      fs->chan = cp;
      fs->cutoff = spectro_cutoff(ss);
      fs->size = fftsize;
      fs->pad_zero = zero_pad(ss);
      fs->wintype = fft_window(ss);
      fs->dBing = fft_log_magnitude(ss);
      fs->lfreq = fft_log_frequency(ss);
      fs->wp = NULL;
      fs->losamp = ap->losamp;
      fs->edit_ctr = cp->edit_ctr;
      fs->ss = ss;
      fs->hwin_size = 0;
      fs->hwin = NULL;
      fs->wavelet_choice = wavelet_type(ss);
      fs->t_type = transform_type(ss);
      fs->old_style = fft_style(ss);
    }
  fs->nn = fs->size/2;
  fs->beg = 0;
  fs->databeg = dbeg;
  fs->datalen = dlen;
  if (simple) cp->fft_data = fs; else cp->fft_data = NULL;
  return((void *)fs);
}

static BACKGROUND_TYPE fft_in_slices(void *fftData)
{
  /* return true when done */
  /* slices are: 
   *    create arrays if needed
   *    window/load data
   *    scramble 
   *    step n times through the fft (100 to 200 per iteration) -- wait for +1 here
   *    return true 
   *
   * since we can be running multiple FFTs at once, not to mention other work procedures,
   * all FFT state needs to be in clientData.
   * 
   * Each slice function returns 0 => call me again, 1 => go to next slice, -1 => quit work proc altogether
   */
  fft_state *fs;
  int res = 0;
  fs = (fft_state *)fftData;
  switch (fs->slice)
    {
    case 0: res = snd_fft_set_up(fs);            break;
    case 1: res = set_up_fft_window(fs);         break;
    case 2: res = make_fft_window(fs);           break;
    case 3: res = apply_fft_window(fs);          break; /* in most non-Fourier cases this returns 5 causing us to skip to display_snd_fft */
    case 4: res = scramble_fft_state(fs);        break;
    case 5: res = snd_fft(fs);                   break;
    case 6: res = snd_fft_cleanup(fs);           break;
    case 7: res = snd_fft_to_spectrum(fs);       break;
    case 8: res = display_snd_fft(fs);           break;
    default: snd_error("impossible fft slice! %d",fs->slice); break;
    }
  if (res == -1) 
    {
      return(BACKGROUND_QUIT);
    }
  fs->slice += res;
  return(BACKGROUND_CONTINUE);
}

BACKGROUND_TYPE safe_fft_in_slices(void *fftData)
{
  BACKGROUND_TYPE res;
  chan_info *cp;
  snd_info *sp;
  snd_state *ss;
  fft_state *fs;
  fs = (fft_state *)fftData;
  cp = (chan_info *)(fs->chan);
  if (!(cp->ffting)) return(BACKGROUND_QUIT);
  res = fft_in_slices(fftData);
  if (res == BACKGROUND_QUIT)
    {
      ss = cp->state;
      sp = cp->sound;
      set_chan_fft_in_progress(cp,0);
      if (fft_size(ss) >= 65536) finish_progress_report(ss,sp,NOT_FROM_ENVED);
      display_channel_data(cp,sp,ss);
    }
  return(res);
}


/* -------------------------------- SONOGRAM -------------------------------- */
/*
 * calls calculate_fft for each slice, each bin being a gray-scaled rectangle in the display
 */

/* as we run the ffts, we need to save the fft data for subsequent redisplay/printing etc */
/* many of these can be running in parallel so the pointers can't be global */
/* display_channel_data above needs to be smart about updates here -- just new data */

/* this work proc calls a loop by pixels (hop depends on pixel/samps decision)
   each pixel(group) sets up the fft_state pointer with beg reflecting hop
   then loops, each time called, calling fft_in_slices until it returns true.
   then grab that data, update the channel display, look to see if we're
   behind the times, if so cleanup and exit, else jump back to outer loop.
   */

typedef struct {
  int slice;
  int outlim,outer;
  fft_state *fs;
  chan_info *cp;
  int spectrum_size;
  sono_info *scp;
  int beg,hop,losamp,hisamp;
  int done;
  int window;
  int msg_ctr;
  int edit_ctr;
  Float old_scale;
  int old_style,old_logxing,t_type,w_choice;
  int minibuffer_needs_to_be_cleared;
} sonogram_state;

void *make_sonogram_state(chan_info *cp)
{
  sonogram_state *sg;
  fft_state *fs;
  snd_state *ss;
  ss = cp->state;
  sg = (sonogram_state *)CALLOC(1,sizeof(sonogram_state));
  sg->cp = cp;
  sg->done = 0;
  fs = (fft_state *)make_fft_state(cp,0); /* 0=>not a simple one-shot fft */
  sg->fs = fs;
  sg->msg_ctr = 8;
  sg->t_type = transform_type(ss);
  sg->w_choice = wavelet_type(ss);
  sg->minibuffer_needs_to_be_cleared = 0;
  return((void *)sg);
}

void free_sono_info (chan_info *cp)
{
  int i;
  sono_info *si;
  si = (sono_info *)(cp->sonogram_data);
  if (si)
    {
      if (si->begs) FREE(si->begs);
      if (si->data)
	{
	  for (i=0;i<si->total_slices;i++)
	    {
	      if (si->data[i]) FREE(si->data[i]);
	    }
	  FREE(si->data);
	}
      FREE(si);
      cp->sonogram_data = NULL;
    }
}

static int set_up_sonogram(sonogram_state *sg)
{
  /* return 1 to go on, 2 to quit early */
  sono_info *si;
  axis_info *ap;
  chan_info *cp;
  snd_state *ss;
  sonogram_state *lsg = NULL;
  int i,tempsize,dpys=1;
  cp = sg->cp;
  if (cp->ffting == 0) return(2);
  ss = cp->state;
  ap = cp->axis;
  sg->slice = 0;
  sg->outer = 0;
  sg->beg = ap->losamp;
  sg->losamp = ap->losamp;
  sg->hisamp = ap->hisamp;
  sg->window = fft_window(ss);
  sg->minibuffer_needs_to_be_cleared = 0;
  if (cp->waving) dpys++; 
  if (cp->lisp_graphing) dpys++; 
  if (fft_style(ss) == SPECTROGRAM)
    sg->outlim = ap->height / spectro_hop(ss); /* this was window_width?? */
  else sg->outlim = ap->window_width/dpys;
  if (sg->outlim <= 1) return(2);
  sg->hop = (int)(ceil((Float)(ap->hisamp - ap->losamp+1)/(Float)(sg->outlim)));
  /* if fewer samps than pixels, draw rectangles */
  if ((transform_type(ss) == FOURIER) || (transform_type(ss) == AUTOCORRELATION))
    sg->spectrum_size = (fft_size(ss))/2;
  else sg->spectrum_size = fft_size(ss);
  sg->edit_ctr = cp->edit_ctr;
  si = (sono_info *)(cp->sonogram_data);
  if (!si)
    {
      si = (sono_info *)CALLOC(1,sizeof(sono_info));
      cp->sonogram_data = si;
      si->total_bins = sg->spectrum_size;
      si->total_slices = (int)(pow(2.0,ceil(log(sg->outlim)/log(2.0))));
      si->begs = (int *)CALLOC(si->total_slices,sizeof(int));
      si->data = (Float **)CALLOC(si->total_slices,sizeof(Float *));
      for (i=0;i<si->total_slices;i++) si->data[i]=(Float *)CALLOC(si->total_bins,sizeof(Float));
    }
  else
    if ((si->total_slices < sg->outlim) || (si->total_bins < sg->spectrum_size))
      {
	for (i=0;i<si->total_slices;i++) if (si->data[i]) {FREE(si->data[i]); si->data[i] = NULL;}
	tempsize = (int)(pow(2.0,ceil(log(sg->outlim)/log(2.0))));
	if (si->total_slices < tempsize) 
	  {
	    FREE(si->data);
	    si->total_slices = tempsize;
	    si->begs = (int *)REALLOC(si->begs,si->total_slices*sizeof(int));
	    si->data = (Float **)CALLOC(si->total_slices,sizeof(Float *));
	  }
	if (si->total_bins < sg->spectrum_size) si->total_bins = sg->spectrum_size;
	for (i=0;i<si->total_slices;i++) si->data[i]=(Float *)CALLOC(si->total_bins,sizeof(Float));
      }
  sg->scp = si;
  si->target_bins = sg->spectrum_size;
  si->active_slices = 0;
  si->target_slices = sg->outlim;
  si->scale = 0.0;
  allocate_sono_rects(ss,si->total_bins);              /* was total_slices by mistake (21-Mar-97) */
  if (cp->last_sonogram)                               /* there was a previous run */
    {
      lsg = (sonogram_state *)(cp->last_sonogram);
      if ((lsg->done) &&                               /* it completed all ffts */
	  (lsg->outlim == sg->outlim) &&               /* the number of ffts is the same */
	  (lsg->spectrum_size == sg->spectrum_size) && /* ditto fft sizes */
	  (lsg->losamp == sg->losamp) &&               /* begins are same */
	  (lsg->hisamp == sg->hisamp) &&               /* ends are same */
	  (lsg->window == sg->window) &&               /* data windows are same */
	  (lsg->t_type == sg->t_type) &&               /* transform types are the same */
	  (lsg->w_choice == sg->w_choice) &&           /* wavelets are the same */
	  (lsg->edit_ctr == sg->edit_ctr))             /* underlying data is the same */
	{
	  sg->outer = sg->outlim;                      /* fake up the run */
	  si->active_slices = si->target_slices;
	  sg->old_scale = lsg->old_scale;
	  si->scale = sg->old_scale;
	  if ((lsg->old_style != fft_style(ss)) ||
	      (lsg->old_logxing != fft_log_frequency(ss)))
	    make_sonogram_axes(cp);                    /* may need to fixup frequency axis labels */
	  sg->old_style = fft_style(ss);
	  sg->old_logxing = fft_log_frequency(ss);
	  return(2);                                   /* so skip the ffts! */
	}
    }
  start_progress_report(ss,cp->sound,NOT_FROM_ENVED);
  return(1);
}

static int run_all_ffts(sonogram_state *sg)
{
  BACKGROUND_TYPE res;
  fft_state *fs;
  sono_info *si;
  chan_info *cp;
  axis_info *ap;
  snd_state *ss;
  Float val;
  int i;
  /* return 0 until done with all ffts, then 1 -- 1 causes cleanup whether done or not */
  /* check for losamp/hisamp change? */
  res = fft_in_slices(sg->fs);
  if (res == BACKGROUND_QUIT)
    {
      /* slice is done -- store it and prepare to start the next slice */
      fs = sg->fs;
      cp = sg->cp;
      ss = cp->state;
      si = (sono_info *)(cp->sonogram_data);
      si->begs[si->active_slices] = sg->beg + fs->beg;
      sg->msg_ctr--;
      if (sg->msg_ctr == 0)
	{
	  progress_report(ss,cp->sound,(fft_style(ss) == SONOGRAM) ? S_sonogram : S_spectrogram,0,0,
			  ((Float)(si->active_slices)/(Float)(si->target_slices)),NOT_FROM_ENVED);
	  sg->minibuffer_needs_to_be_cleared = 1;
	  sg->msg_ctr = 8;
	  if (cp->ffting == 0) return(1);
	}
      if (transform_type(ss) == FOURIER)
	{
	  for (i=0;i<sg->spectrum_size;i++) 
	    {
	      val = fs->data[i];
	      if (val > si->scale) si->scale = val;
	      si->data[si->active_slices][i] = val;
	    }
	}
      else
	{
	  for (i=0;i<sg->spectrum_size;i++) 
	    {
	      val = fs->data[i];
	      if (val < 0.0) val = -val;  /* kinda dubious but I can't think of a good alternative */
	      if (val > si->scale) si->scale = val;
	      si->data[si->active_slices][i] = val;
	    }
	}
      si->active_slices++;
      sg->outer++;
      if ((sg->outer == sg->outlim) || (cp->ffting == 0) || (fft_style(ss) == NORMAL_FFT)) return(1);
      fs->beg += sg->hop;
      fs->slice = 0;
      ap = cp->axis;
      if ((sg->losamp != ap->losamp) || (sg->hisamp != ap->hisamp)) 
	{
	  fs->beg = 0;
	  return(-1);
	}
    }
  return(0);
}

static int cleanup_sonogram(sonogram_state *sg)
{
  chan_info *cp;
  /* data has already been placed on cp->sonogram_state, so we need only clear the fft_state struct */
  if (sg)
    {
      cp = sg->cp;
      if ((cp->ffting == 0) || (sg->scp == NULL) || (sg->outlim <= 1)) return(1);
      make_sonogram_axes(cp);
      if (sg->fs) sg->fs = free_fft_state(sg->fs);
      cp->fft_data = NULL;
      set_chan_fft_in_progress(cp,0);
      display_channel_data(cp,cp->sound,cp->state);
      if (cp->last_sonogram) FREE(cp->last_sonogram);
      if (sg->outer == sg->outlim) sg->done = 1;
      sg->old_scale = (sg->scp)->scale;
      cp->last_sonogram = sg;
      if (sg->minibuffer_needs_to_be_cleared)
	{
	  finish_progress_report(cp->state,cp->sound,NOT_FROM_ENVED);
	  sg->minibuffer_needs_to_be_cleared = 0;
	}
    }
  return(1);
}

BACKGROUND_TYPE sonogram_in_slices(void *sono)
{
  sonogram_state *sg = (sonogram_state *)sono;
  chan_info *cp;
  int res = 0;
  cp = sg->cp;
  if (cp->ffting == 0) return(BACKGROUND_QUIT);
  switch (sg->slice)
    {
    case 0: res = set_up_sonogram(sg); break; /* return 1 to go on, 2 to quit early */
    case 1: res = run_all_ffts(sg);    break; /* return 0 until done with all ffts, then 1 -- 1 causes cleanup whether done or not */
    case 2: res = cleanup_sonogram(sg); return(BACKGROUND_QUIT); break;
    default: snd_error("runaway sonogram? (%d) ",sg->slice); cleanup_sonogram(sg); return(BACKGROUND_QUIT); break;
    }
  sg->slice += res;
  return(BACKGROUND_CONTINUE);
}

int sono_update(chan_info *cp, void *ignore)
{
  make_sonogram_axes(cp);
  update_graph(cp,NULL);
  return(0);
}

void set_spectro_cutoff_and_redisplay(snd_state *ss,Float val)
{
  in_set_spectro_cutoff(ss,val); 
  if (fft_style(ss) != NORMAL_FFT)
    map_over_chans(ss,sono_update,NULL);
  else map_over_chans(ss,update_graph,NULL);
}

static void spectral_multiply (Float* rl1, Float* rl2, int n)
{
  int j,n2,nn2;
  Float rem,rep,aim,aip,invn;
  n2=(int)(n*0.5);
  invn = 0.25/n;
  rl1[0] = ((rl1[0]*rl2[0])/n);
  rl2[0] = 0.0;
  for (j=1;j<=n2;j++)
    {
      nn2 = n-j;
      rep = (rl1[j]+rl1[nn2]);
      rem = (rl1[j]-rl1[nn2]);
      aip = (rl2[j]+rl2[nn2]);
      aim = (rl2[j]-rl2[nn2]);
      rl1[j] = invn*(rep*aip + aim*rem);
      rl1[nn2] = rl1[j];
      rl2[j] = invn*(aim*aip - rep*rem);
      rl2[nn2] = -rl2[j];
    }
}

void c_convolve (snd_state *ss, char *fname, Float amp, int filec, int filehdr, int filterc, int filterhdr, int filtersize,
           int fftsize, int filter_chans, int filter_chan, int data_size, snd_info *gsp, int from_enved, int ip, int total_chans)
{
  Float *rl0 = NULL,*rl1 = NULL, *rl2 = NULL;
  MUS_SAMPLE_TYPE **pbuffer = NULL,**fbuffer = NULL;
  MUS_SAMPLE_TYPE *cm = NULL,*fcm = NULL, *pbuf = NULL;
  int i;
  Float scl;
  int tempfile;

  /* need file to hold convolution output */
  tempfile=mus_file_create(fname);
  if (tempfile != -1)
    {
      /* get to start point in the two sound files */
      mus_file_seek(filec,filehdr,SEEK_SET);
      mus_file_seek(filterc,filterhdr,SEEK_SET);

      rl0 = (Float *)CALLOC(fftsize,sizeof(Float));
      if (rl0) rl1 = (Float *)CALLOC(fftsize,sizeof(Float));
      if (rl1) pbuffer = (MUS_SAMPLE_TYPE **)CALLOC(1,sizeof(MUS_SAMPLE_TYPE *));
      if (pbuffer) pbuffer[0] = (MUS_SAMPLE_TYPE *)CALLOC(data_size,sizeof(MUS_SAMPLE_TYPE));
      if (pbuffer[0]) cm = (MUS_SAMPLE_TYPE *)CALLOC(1,sizeof(MUS_SAMPLE_TYPE));
      fbuffer = (MUS_SAMPLE_TYPE **)CALLOC(filter_chans,sizeof(MUS_SAMPLE_TYPE *));
      if (fbuffer) fbuffer[filter_chan] = (MUS_SAMPLE_TYPE *)CALLOC(filtersize,sizeof(MUS_SAMPLE_TYPE));
      if (fbuffer[filter_chan]) fcm = (MUS_SAMPLE_TYPE *)CALLOC(filter_chans,sizeof(MUS_SAMPLE_TYPE));
      if ((rl0 == NULL) || (rl1 == NULL) || 
	  (pbuffer == NULL) || (pbuffer[0] == NULL) || (cm == NULL) ||
	  (fbuffer == NULL) || (fbuffer[filter_chan] == NULL) || (fcm == NULL))
	{
	  snd_error("not enough memory for convolve");
	}
      else
	{
	  cm[0] = (MUS_SAMPLE_TYPE)1;
	  fcm[filter_chan] = (MUS_SAMPLE_TYPE)1;
	  pbuf = pbuffer[0];

	  /* read in the "impulse response" */
	  mus_file_read_any(filterc,0,filter_chans,filtersize-1,fbuffer,fcm);
	  for (i=0;i<filtersize;i++) 
	    rl1[i] = MUS_SAMPLE_TO_FLOAT(fbuffer[filter_chan][i]);
	  progress_report(ss,gsp,"convolve",ip+1,total_chans,.1,from_enved);
	  mus_header_write_next_header(tempfile,22050,1,28,data_size*4,MUS_BINT,NULL,0);
	  mus_file_set_descriptors(tempfile,fname,MUS_BINT,4,28,1,MUS_NEXT);
	  /* get the convolution data */
	  mus_file_read_any(filec,0,1,data_size-1,pbuffer,cm);
	  for (i=0;i<data_size;i++) rl0[i] = MUS_SAMPLE_TO_FLOAT(pbuf[i]);

	  progress_report(ss,gsp,"convolve",ip+1,total_chans,.3,from_enved);
	  mus_fft(rl0,rl1,fftsize,1);
	  progress_report(ss,gsp,"convolve",ip+1,total_chans,.5,from_enved);
	  spectral_multiply(rl0,rl1,fftsize);
	  progress_report(ss,gsp,"convolve",ip+1,total_chans,.6,from_enved);
	  mus_fft(rl0,rl1,fftsize,-1);
	  progress_report(ss,gsp,"convolve",ip+1,total_chans,.8,from_enved);

	  if (amp != 0.0)
	    {
	      /* normalize the results */
	      scl = 0.0;
	      for (i=0;i<data_size;i++) 
		{
		  if (rl0[i] > scl) scl=rl0[i];
		  else if (rl0[i] < -scl) scl=(-rl0[i]);
		}
	      if (scl != 0.0) scl = amp / scl;
	      for (i=0;i<data_size;i++) pbuf[i] = MUS_FLOAT_TO_SAMPLE(scl * rl0[i]);
	    }
	  progress_report(ss,gsp,"convolve",ip+1,total_chans,.9,from_enved);
	  /* and save as temp file */
	  mus_file_write(tempfile,0,data_size-1,1,&(pbuf));
	  mus_file_close(tempfile);
	}
      if (rl0) FREE(rl0);
      if (rl1) FREE(rl1);
      if (rl2) FREE(rl2);
      if (pbuffer) 
	{
	  if (pbuf) FREE(pbuf);
	  FREE(pbuffer);
	}
      if (cm) FREE(cm);
      if (fbuffer) 
	{
	  if (fbuffer[filter_chan]) FREE(fbuffer[filter_chan]);
	  FREE(fbuffer);
	}
      if (fcm) FREE(fcm);
    }
}

#if HAVE_GUILE
#include "sg.h"

static SCM g_autocorrelate(SCM reals)
{
  #define H_autocorrelate "(" S_autocorrelate " data) returns (in place) the autocorrelation of data (vector or vct)"
  /* assumes length is power of 2 */
  vct *v1 = NULL;
  int n,i;
  Float *rl;
  ERRV1(reals,S_autocorrelate);
  if (vct_p(reals))
    {
      v1 = (vct *)GH_VALUE_OF(reals);
      rl = v1->data;
      n = v1->length;
    }
  else
    {
      n = gh_vector_length(reals);
      rl = (Float *)CALLOC(n,sizeof(Float));
      for (i=0;i<n;i++) rl[i] = gh_scm2double(gh_vector_ref(reals,gh_int2scm(i)));
    }
  autocorrelation(rl,n);
  if (v1 == NULL) 
    {
      for (i=0;i<n;i++) gh_vector_set_x(reals,gh_int2scm(i),gh_double2scm(rl[i]));
      FREE(rl);
    }
  return(reals);
}

static SCM g_add_transform(SCM name, SCM xlabel, SCM lo, SCM hi, SCM proc)
{
  #define H_add_transform "(" S_add_transform " name x-label low high func) adds the transform func\n\
   to the transform lists; func should be a function of two arguments, the length of the transform\n\
   and a sample-reader to get the data, and should return a vct object containing the transform results.\n\
   'name' is the transform's name, x-label is its x-axis label, and the relevant returned data\n\
   to be displayed goes from low to high (normally 0.0 to 1.0)"

  char *str1=NULL,*str2=NULL;
  SCM res=SCM_BOOL_F;
  ERRS1(name,S_add_transform);
  ERRS2(xlabel,S_add_transform);
  ERRN3(lo,S_add_transform);
  ERRN4(hi,S_add_transform);
  str1 = gh_scm2newstr(name,NULL);
  str2 = gh_scm2newstr(xlabel,NULL);
  SCM_ASSERT(gh_procedure_p(proc),proc,SCM_ARG5,S_add_transform);
  if (procedure_ok(proc,2,0,S_add_transform,"func",5))
    res = gh_int2scm(add_transform(str1,str2,gh_scm2double(lo),gh_scm2double(hi),proc));
  if (str1) free(str1);
  if (str2) free(str2);
  return(res);
}


void g_init_fft(SCM local_doc)
{
  gh_define(S_fourier_transform,gh_int2scm(FOURIER));
  gh_define(S_wavelet_transform,gh_int2scm(WAVELET));
  gh_define(S_hankel_transform,gh_int2scm(HANKEL));
  gh_define(S_chebyshev_transform,gh_int2scm(CHEBYSHEV));
  gh_define(S_cepstrum,gh_int2scm(CEPSTRUM));
  gh_define(S_hadamard_transform,gh_int2scm(HADAMARD));
  gh_define(S_walsh_transform,gh_int2scm(WALSH));
  gh_define(S_autocorrelation,gh_int2scm(AUTOCORRELATION));

  DEFINE_PROC(gh_new_procedure1_0(S_autocorrelate,g_autocorrelate),H_autocorrelate);
  DEFINE_PROC(gh_new_procedure5_0(S_add_transform,g_add_transform),H_add_transform);
}

#endif
