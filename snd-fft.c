#include "snd.h"

/* handling of "beta" changed drastically 28-June-98 
 *   it is now a number between 0 and 1 from ss point of view, and is scaled by the window max before being applied 
 * returned to old wavelet code 18-Apr-01
 * added fftw support and removed fht 6-June-02
 * greatly simplified 8-June-02: no more cached windows, single fft background functions etc, zero pad made sensible
 */

#if HAVE_FFTW && WITH_SHARED_SNDLIB
#include <rfftw.h>
/* save old plans both ways */
static fftw_real *rdata = NULL, *idata = NULL;
static rfftw_plan rplan, iplan;
static int last_fft_size = 0;

void mus_fftw(Float *rl, int n, int dir)
{
  int i;
  if (n != last_fft_size)
    {
      if (rdata) {FREE(rdata); FREE(idata); rfftw_destroy_plan(rplan); rfftw_destroy_plan(iplan);}
      rplan = rfftw_create_plan(n, FFTW_REAL_TO_COMPLEX, FFTW_ESTIMATE); /* I didn't see any improvement here from using FFTW_MEASURE */
      iplan = rfftw_create_plan(n, FFTW_COMPLEX_TO_REAL, FFTW_ESTIMATE);
      last_fft_size = n;
      rdata = (fftw_real *)CALLOC(n, sizeof(fftw_real));
      idata = (fftw_real *)CALLOC(n, sizeof(fftw_real));
    }
  memset((void *)idata, 0, n * sizeof(fftw_real));
  /* if Float (default float) == fftw_real (default double) we could forego the data copy */
  for (i = 0; i < n; i++) {rdata[i] = rl[i];}
  if (dir != -1)
    rfftw_one(rplan, rdata, idata);
  else rfftw_one(iplan, rdata, idata);
  for (i = 0; i < n; i++) rl[i] = idata[i];
}
#endif


/* -------------------------------- WAVELET TRANSFORM -------------------------------- */
/* 
 * taken from wavelets.cl in clm which is taken from
 * M. J. Shensa Naval Ocean Systems Center,
 * Wickerhauser "Adapted Wavelet Analysis", 
 * and the UBC Imager Wavelet Package by Bob Lewis
 */

static void wavelet_transform(Float *data, int num, Float *cc, int cc_size)
{
  Float *data1 = NULL;
  Float sig = -1.0;
  Float *cr = NULL;
  int i, j, n, n1, nmod, nh, joff, ii, ni, k, jf;
  cr = (Float *)CALLOC(cc_size, sizeof(Float));
  for (i = 0, j = cc_size - 1; i < cc_size; i++, j--)
    {
      cr[j] = sig * cc[i];
      sig = -sig;
    }
  for (n = num; n >= 4; n /= 2)
    {
      if (data1) FREE(data1);
      data1 = (Float *)CALLOC(n, sizeof(Float));
      n1 = n - 1;
      nmod = cc_size * n;
      nh = n >> 1;
      joff = -(cc_size >> 1);
      for (ii = 0, i = 1; i <= n; i += 2, ii++)
	{
	  ni = i + nmod + joff;
	  for (k = 0; k < cc_size; k++)
	    {
	      jf = n1 & (ni + k + 1);
	      data1[ii] += cc[k] * data[jf];
	      data1[ii + nh] += cr[k] * data[jf];
	    }
	}
      for (i = 0; i < n; i++)
	data[i] = data1[i];
    }
  if (data1) FREE(data1);
  if (cr) FREE(cr);
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
static Float sym3[4] = {0.1767767, 0.5303301, 0.5303301, 0.1767767};
static Float sym4[10] = {0.033145633, -0.066291265, -0.1767767, 0.4198447, 0.994369, 0.4198447, -0.1767767, -0.066291265, 0.033145633, 0.0};
static Float sym5[8] = {0.066291265, -0.19887379, -0.15467963, 0.994369, 0.994369, -0.15467963, -0.19887379, 0.066291265};
static Float sym6[16] = {-0.0030210863, -0.009063259, -0.016831767, 0.07466399, 0.03133298, -0.30115914, -0.026499243, 
			 0.9516422, 0.9516422, -0.026499243, -0.30115914, 0.03133298, 0.07466399, -0.016831767, -0.009063259, -0.0030210863};

static char *wavelet_names[NUM_WAVELETS] =
  {"daub4", "daub6", "daub8", "daub10", "daub12", "daub14", "daub16", "daub18", "daub20",
   "battle_lemarie", "burt_adelson", "beylkin", "coif2", "coif4", "coif6",
   "sym2", "sym3", "sym4", "sym5", "sym6"};
static int wavelet_sizes[NUM_WAVELETS] =
  {4, 6, 8, 10, 12, 14, 16, 18, 20,
   24, 6, 18, 6, 12, 18,
   5, 4, 10, 8, 16};
static Float *wavelet_data[NUM_WAVELETS] =
  {daub4, daub6, daub8, daub10, daub12, daub14, daub16, daub18, daub20,
   Battle_Lemarie, Burt_Adelson, Beylkin, coif2, coif4, coif6,
   sym2, sym3, sym4, sym5, sym6};



/* -------------------------------- HAAR TRANSFORM -------------------------------- */
/*
 * from fxt/haar/haar.cc
 */

static void haar_transform(Float *f, int n)
{
  int m, mh, i, j, k;
  Float s2;
  Float v = 1.0;
  Float x, y;
  Float *g;
  s2 = sqrt(0.5);
  g = (Float *)CALLOC(n, sizeof(Float));
  for (m = n; m > 1; m >>= 1)
    {
      v *= s2;
      mh = (m >> 1);
      for (j = 0, k = 0; j < m; j += 2, k++)
        {
	  x = f[j];
	  y = f[j + 1];
	  g[k] = x + y;
	  g[mh + k] = (x - y) * v;
        }
      for (i = m - 1; i >= 0; i--) f[i] = g[i];
    }
  f[0] *= v;
  FREE(g);
}


/* -------------------------------- WALSH TRANSFORM -------------------------------- */

/* borrowed from walsh/walshdit2.cc in the fxt package fxt970929.tgz written by (and copyright) Joerg Arndt
 * arndt@spektracom.de, arndt@jjj.de, http://www.spektracom.de/~arndt/joerg.html, http://www.jjj.de/fxt/
 * fxt.doc appears to say I can use it here (Snd is freeware and I've modified the original to some extent).
 */

static void walsh_transform(Float *data, int n)
{
  int i, j, m, ipow;
  int r, t1, t2, mh;
  Float u, v;
  ipow = (int)((log(n) / log(2)) + .0001); /* added fudge factor 21-Sep-01 -- (int)3.0000 = 2 on PC */
  for (i = ipow; i >= 1; --i)
    {
      m = (1 << i);
      mh = (m >> 1);
      for (r = 0; r < n; r += m)
        {
	  for (j = 0, t1 = r, t2 = r + mh; j < mh; ++j, ++t1, ++t2)
            {
	      u = data[t1];
	      v = data[t2];
	      data[t1] = u + v;
	      data[t2] = u - v;
            }
        }
    }
}



/* -------------------------------- AUTOCORRELATION -------------------------------- */

void autocorrelation(Float *data, int n)
{
#if HAVE_FFTW
  Float *rl;
#else
  Float *rl, *im;
#endif
  Float fscl;
  int i, j, n2;
  n2 = n / 2;
  rl = (Float *)MALLOC(n * sizeof(Float));
  memcpy((void *)rl, (void *)data, n * sizeof(Float));
  /* for (i = 0; i < n; i++) rl[i] = data[i]; */
  fscl = 1.0 / (Float)n;
#if HAVE_FFTW
  mus_fftw(rl, n, 1);
  rl[0] *= rl[0];
  rl[n2] *= rl[n2];
  for (i = 1, j = n - 1; i < n2; i++, j--) 
    {
      rl[i] = rl[i] * rl[i] + rl[j] * rl[j];
      rl[j] = 0.0;
    }
  mus_fftw(rl, n, -1);
#else
  im = (Float *)CALLOC(n, sizeof(Float));
  mus_fft(rl, im, n, 1);
  rl[0] *= rl[0];
  rl[n2] *= rl[n2];
  for (i = 1, j = n - 1; i < n2; i++, j--)
    {
      rl[i] = rl[i] * rl[i] + im[i] * im[i];
      rl[j] = rl[i];
    }
  memset((void *)im, 0, n * sizeof(Float));
  mus_fft(rl, im, n, -1);
  FREE(im);
#endif
  for (i = 0; i <= n2; i++) data[i] = fscl * rl[i];
  FREE(rl);
}



/* -------------------------------- CEPSTRUM -------------------------------- */
/* is this useful? correct? */

static void cepstrum(Float *data, int n)
{
  Float *rl, *im;
  Float fscl = 0.0, lowest;
  int i, j, n2;
  lowest = 0.00000001;
  fscl = 2.0 / (Float)n;
  rl = (Float *)MALLOC(n * sizeof(Float));
  im = (Float *)CALLOC(n, sizeof(Float));
  memcpy((void *)rl, (void *)data, n * sizeof(Float));
  /* for (i = 0; i < n; i++) rl[i] = data[i]; */
  mus_fft(rl, im, n, 1);
  rl[0] *= rl[0];
  n2 = n / 2;
  rl[n2] *= rl[n2];
  for (i = 1, j = n - 1; i < n2; i++, j--)
    {
      rl[i] = rl[i] * rl[i] + im[i] * im[i];
      if (rl[i] < lowest)
	rl[i] = -10.0;
      else rl[i] = log(sqrt(rl[i]));
      rl[j] = rl[i];
    }
  memset((void *)im, 0, n * sizeof(Float));
  mus_fft(rl, im, n, -1);
  for (i = 0; i < n; i++)
    if (fabs(rl[i]) > fscl) 
      fscl = fabs(rl[i]);
  if (fscl > 0.0)
    for (i = 0; i < n; i++) 
      data[i] = rl[i] / fscl;
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

static void fast_hwt_first_stage(int local_half_size, Float *out, Float *in) 
{
  Float tmp0, tmp1;
  int k, j, i;
  for (i = 0, j = local_half_size, k = 0; i < local_half_size; ) 
    {
      tmp0 = in[k++]; 
      tmp1 = in[k++];
      out[i++] = tmp0 + tmp1;
      out[j++] = tmp0 - tmp1;
    }
}

static void fast_hwt_stage(int n, int local_size, int local_half_size, Float *out, Float *in) 
{
  Float tmp0, tmp1;
  int k, j, i;
  for (i = 0, j = local_half_size, k = 0; i < local_half_size; ) 
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
  if (n > 2) 
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

static void fast_hwt(Float *out, Float *in, int n)
{
  int size;
  int need_to_switch_on_output = FALSE;
  size = (0x0001 << n);
  if ((n % 2) == 0) need_to_switch_on_output = TRUE;
  fast_hwt_stage (n, size, (size >> 1), out, in);
  if (need_to_switch_on_output) memcpy (out, in, size * sizeof(in[0])); 
}


static int compare_peaks(const void *pk1, const void *pk2)
{
  if (((fft_peak *)pk1)->freq > ((fft_peak *)pk2)->freq) return(1);
  else if (((fft_peak *)pk1)->freq == ((fft_peak *)pk2)->freq) return(0);
  return(-1);
}

int find_and_sort_peaks(Float *buf, fft_peak *found, int num_peaks, int size)
{ /* in the fft peak finder below we assume data between 0 and 1 */
  /* this procedure is for the list graph -- see below for fft */
  int i, j, pks, minpk;
  Float minval, la, ra, ca;
  Float *peaks;
  int *inds;
  if (num_peaks <= 0) return(0);
  peaks = (Float *)CALLOC(num_peaks, sizeof(Float));
  inds = (int *)CALLOC(num_peaks, sizeof(int));
  pks = 0;
  la = 0.0;
  ca = 0.0;
  ra = 0.0;
  minval = 0.00001;
  for (i = 0; i < size; i++)
    {
      la = ca;
      ca = ra;
      ra = buf[i];
      if ((ca > minval) && (ca > ra) && (ca > la))
	{
	  if (pks < num_peaks)
	    {
	      inds[pks] = i - 1;
	      peaks[pks++] = ca;
	    }
	  else
	    {
	      minval = peaks[0];
	      minpk = 0;
	      for (j = 1; j < num_peaks; j++)
		if (peaks[j] < minval) 
		  {
		    minval = peaks[j];
		    minpk = j;
		  }
	      if (ca > minval)
		{
		  inds[minpk] = i - 1;
		  peaks[minpk] = ca;
		}
	    }
	}
    }
  for (i = 0; i < pks; i++)
    {
      j = inds[i];
      ca = buf[j];
      found[i].amp = buf[j];
      found[i].freq = (Float)j;
    }
  if (pks > 0) qsort((void *)found, pks, sizeof(fft_peak), compare_peaks);
  FREE(peaks);
  FREE(inds);
  return(pks);
}

#define MIN_CHECK 0.000001

int find_and_sort_transform_peaks(Float *buf, fft_peak *found, int num_peaks, int fftsize2, int srate, Float samps_per_pixel, Float fft_scale)
{
  /* we want to reflect the graph as displayed, so each "bin" is samps_per_pixel wide */
  int i, j, k, pks, minpk, hop, pkj, oldpkj;
  Float minval, la, ra, ca, logca, logra, logla, offset, fscl, ascl, bscl;
  Float *peaks;
  int *inds;
  peaks = (Float *)CALLOC(num_peaks, sizeof(Float));
  inds = (int *)CALLOC(num_peaks, sizeof(int));
  fscl = (Float)srate / (Float)fftsize2;
  hop = (int)(samps_per_pixel + 0.5);
  if (hop < 1) hop = 1;
  pks = 0;
  la = 0.0;
  ca = 0.0;
  ra = 0.0;
  minval = 0.0; /* (Float)fftsize2/100000.0; */
  ascl = 0.0;
  pkj = 0;
  for (i = 0; i < fftsize2 - hop; i += hop)
    {
      la = ca;
      ca = ra;
      oldpkj = pkj;
      ra = 0.0;
      for (k = 0; k < hop; k++) 
	if (buf[i + k] > ra) 
	  {
	    pkj = i + k; 
	    ra = buf[pkj]; /* reflect user's view of the graph */
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
	      for (j = 1; j < num_peaks; j++)
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
  if (ascl > 0.0) ascl = 1.0 / ascl; else ascl = 1.0;
  if (fft_scale > 0.0) bscl = fft_scale / ascl; else bscl = 1.0;
  for (i = 0, k = 0; i < pks; i++)
    {
      j = inds[i];
      ca = buf[j] * ascl;
      if (j > 0) la = buf[j - 1] * ascl; else la = ca;
      ra = buf[j + 1] * ascl; 
      if ((la < MIN_CHECK) || (ra < MIN_CHECK))
	{
	  found[k].amp = bscl * ca;
	  found[k].freq = fscl * j;
	}
      else
	{
	  logla = log10(la);
	  logca = log10(ca);
	  logra = log10(ra);
	  offset = (0.5 * (logla - logra)) / (logla + logra - 2 * logca); /* this assumes amps < 1.0 (from XJS sms code) */
	  found[k].amp = bscl * pow(10.0, logca - 0.25 * offset * (logla - logra));
	  if ((found[k].amp > 1.0) && 
	      (fft_scale > 0.0)) 
	    found[k].amp = 1.0;
	  found[k].freq = fscl * (j + offset);
	}
      if (found[k].freq < 0.0) found[k].freq = 0.0;
      if (found[k].amp > 0.0) k++;
    }
  for (i = k; i < num_peaks; i++) found[i].freq = 1.0; /* move blank case to end of sorted list */
  qsort((void *)found, pks, sizeof(fft_peak), compare_peaks);
  FREE(peaks);
  FREE(inds);
  return(k);
}

static Float beta_maxes[NUM_FFT_WINDOWS] = {1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
					    1.0, 1.0, 15.0, 10.0, 10.0, 10.0, 1.0, 18.0};
Float fft_beta_max(int win) {return(beta_maxes[win]);}

typedef struct {
  char *name, *xlabel;
  Float lo, hi;
  XEN proc;
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
      added_transforms = (added_transform **)CALLOC(added_transforms_size, sizeof(added_transform *));
    }
  else
    {
      if (added_transforms_top == added_transforms_size)
	{
	  added_transforms_size += 4;
	  added_transforms = (added_transform **)REALLOC(added_transforms, added_transforms_size * sizeof(added_transform *));
	  for (i = added_transforms_top; i < added_transforms_size; i++) added_transforms[i] = NULL;
	}
    }
  added_transforms[added_transforms_top] = (added_transform *)CALLOC(1, sizeof(added_transform));
  return(added_transforms[added_transforms_top++]);
}

static int add_transform(char *name, char *xlabel, Float lo, Float hi, XEN proc)
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
  for (i = 0; i < added_transforms_top; i++)
    if (added_transforms[i]->type == type)
      return(added_transforms[i]->name);
  return("unknown");
}

static char *added_transform_xlabel(int type)
{
  int i;
  for (i = 0; i < added_transforms_top; i++)
    if (added_transforms[i]->type == type)
      return(added_transforms[i]->xlabel);
  return("unknown");
}

static Float added_transform_lo(int type)
{
  int i;
  for (i = 0; i < added_transforms_top; i++)
    if (added_transforms[i]->type == type)
      return(added_transforms[i]->lo);
  return(0.0);
}

static Float added_transform_hi(int type)
{
  int i;
  for (i = 0; i < added_transforms_top; i++)
    if (added_transforms[i]->type == type)
      return(added_transforms[i]->hi);
  return(1.0);
}

static XEN added_transform_proc(int type)
{
  int i;
  for (i = 0; i < added_transforms_top; i++)
    if (added_transforms[i]->type == type)
      return(added_transforms[i]->proc);
  return(XEN_FALSE);
}

static XEN before_transform_hook;


static char *spectro_xlabel(chan_info *cp)
{
  switch (cp->transform_type)
    {
    case FOURIER: 
      if (cp->fft_log_frequency)
	return("log freq");
      else return("frequency");
      break;
    case WAVELET:         return(wavelet_names[cp->wavelet_type]); break;
    case HAAR:            return(_("Haar spectrum"));              break;
    case CEPSTRUM:        return("cepstrum");                      break;
    case WALSH:           return("Sequency");                      break;
    case HADAMARD:        return("Sequency");                      break;
    case AUTOCORRELATION: return(_("Lag time"));                   break;
    default:              return(added_transform_xlabel(cp->transform_type)); break;
    }
  return(NULL);
}

static void make_sonogram_axes(chan_info *cp)
{
  fft_info *fp;
  axis_info *ap;
  Float max_freq, min_freq, yang;
  char *xlabel;
  fp = cp->fft;
  if (fp)
    {
      ap = cp->axis;
      if (cp->transform_type == FOURIER)
	{
	  if ((cp->fft_log_frequency) || (cp->transform_graph_type == GRAPH_AS_SPECTROGRAM))
	    {
	      max_freq = cp->spectro_cutoff;
	      min_freq = cp->spectro_start;
	    }
	  else 
	    {
	      max_freq = cp->spectro_cutoff * (Float)SND_SRATE(cp->sound) * 0.5;
	      min_freq = cp->spectro_start * (Float)SND_SRATE(cp->sound) * 0.5;
	    }
	}
      else 
	{
	  if (cp->transform_type == AUTOCORRELATION)
	    {
	      max_freq = fp->current_size * cp->spectro_cutoff / 2;
	      min_freq = fp->current_size * cp->spectro_start / 2;
	    }
	  else 
	    {
	      max_freq = fp->current_size * cp->spectro_cutoff;
	      min_freq = fp->current_size * cp->spectro_start;
	    }
	}
      yang = fmod(cp->spectro_y_angle, 360.0);
      if (yang < 0.0) yang += 360.0;
      if (cp->transform_graph_type == GRAPH_AS_SPECTROGRAM)
	{
	  if (cp->transform_type == FOURIER)
	    {
	      if (yang < 45.0) xlabel = _("frequency");
	      else if (yang < 135.0) xlabel = _("time");
	      else if (yang < 225.0) xlabel = _("ycneuqerf"); /* a joke... */
	      else if (yang < 315.0) xlabel = _("emit");
	      else xlabel = "frequency";
	    }
	  else xlabel = spectro_xlabel(cp);
	  fp->axis = make_axis_info(cp,
				    min_freq, max_freq,
				    ap->x0, ap->x1,
				    xlabel,
				    min_freq, max_freq,
				    ap->x0, ap->x1,
				    fp->axis);
	}
      else 
	fp->axis = make_axis_info(cp,
				  ap->x0, ap->x1,
				  min_freq, max_freq,
				  "time",
				  ap->x0, ap->x1,
				  min_freq, max_freq,
				  fp->axis);
    }
}

typedef struct {
  int size, wintype, old_style, done;
  void *chan;
  Float *window;
  Float *data;
  Float beta;
  int wavelet_choice, transform_type;
  off_t beg, databeg, datalen;
  off_t losamp;
  int edit_ctr, dBing, lfreq;
  int pad_zero;
  Float cutoff;
  snd_state *ss;
} fft_state;

static void apply_fft(fft_state *fs)
{
  int i;
  off_t ind0;
  Float *window, *fft_data;
  int data_len;
  snd_fd *sf;
  chan_info *cp;
  snd_state *ss;
  ss = fs->ss;
  cp = (chan_info *)(fs->chan);
  fft_data = fs->data;
  data_len = cp->transform_size;
  if ((show_selection_transform(ss)) && 
      (selection_is_active_in_channel(cp)) && 
      (fs->datalen > 0))
    {
      ind0 = fs->databeg;
      if (cp->transform_graph_type == GRAPH_ONCE) data_len = (int)(fs->datalen);
    }
  else 
    {
      XEN res;
      if (XEN_HOOKED(before_transform_hook))
	{
	  res = run_progn_hook(before_transform_hook, 
			       XEN_LIST_2(C_TO_SMALL_XEN_INT(cp->sound->index), 
					  C_TO_SMALL_XEN_INT(cp->chan)),
			       S_before_transform_hook);
	  if (XEN_NUMBER_P(res))
	    ind0 = XEN_TO_C_OFF_T_OR_ELSE(res, 0) + fs->beg;
	  else ind0 = (cp->axis)->losamp + fs->beg;
	}
      else
	ind0 = (cp->axis)->losamp + fs->beg;
    }
  sf = init_sample_read(ind0, cp, READ_FORWARD);
  if (sf == NULL) return;
  switch (cp->transform_type)
    {
    case FOURIER:
      window = fs->window;
      for (i = 0; i < data_len; i++)
	fft_data[i] = window[i] * read_sample_to_float(sf);
      if (data_len < fs->size) 
	for (i = data_len; i < fs->size; i++) 
	  fft_data[i] = 0.0;
#if HAVE_FFTW
      {
	int j;
	mus_fftw(fft_data, fs->size, 1);
	fft_data[0] = fabs(fft_data[0]);
	fft_data[fs->size / 2] = fabs(fft_data[fs->size / 2]);
	for (i = 1, j = fs->size - 1; i < fs->size / 2; i++, j--) 
	  fft_data[i] = hypot(fft_data[i], fft_data[j]);
      }
#else
      {
	Float *idata;
	idata = (Float *)CALLOC(fs->size, sizeof(Float));
	mus_fft(fft_data, idata, fs->size, 1);
	for (i = 0; i < fs->size; i++) 
	  fft_data[i] = hypot(fft_data[i], idata[i]);
	FREE(idata);
      }
#endif
      break;
    case WAVELET:
      for (i = 0; i < data_len; i++) fft_data[i] = read_sample_to_float(sf);
      if (data_len < fs->size) 
	for (i = data_len; i < fs->size; i++) 
	  fft_data[i] = 0.0;
      wavelet_transform(fft_data, fs->size, wavelet_data[cp->wavelet_type], wavelet_sizes[cp->wavelet_type]);
      break;
    case HAAR:
      for (i = 0; i < data_len; i++) fft_data[i] = read_sample_to_float(sf);
      if (data_len < fs->size) 
	for (i = data_len; i < fs->size; i++) 
	  fft_data[i] = 0.0;
      haar_transform(fft_data, fs->size);
      break;
    case CEPSTRUM:
      for (i = 0; i < data_len; i++) fft_data[i] = read_sample_to_float(sf);
      if (data_len < fs->size) 
	for (i = data_len; i < fs->size; i++) 
	  fft_data[i] = 0.0;
      cepstrum(fft_data, fs->size);
      break;
    case HADAMARD:
      if (data_len >= 4)
	{
	  window = (Float *)CALLOC(fs->size, sizeof(Float));
	  for (i = 0; i < data_len; i++) fft_data[i] = read_sample_to_float(sf);
	  if (data_len < fs->size) 
	    for (i = data_len; i < fs->size; i++) 
	      fft_data[i] = 0.0;
	  fast_hwt(window, fft_data, (int)(log((Float)(fs->size + 1)) / log(2.0)));
	  for (i = 0; i < fs->size; i++) fft_data[i] = window[i];
	  FREE(window);
	}
      break;
    case WALSH:
      for (i = 0; i < data_len; i++) fft_data[i] = read_sample_to_float(sf);
      if (data_len < fs->size) 
	for (i = data_len; i < fs->size; i++) 
	  fft_data[i] = 0.0;
      walsh_transform(fft_data, fs->size);
      break;
    case AUTOCORRELATION:
      for (i = 0; i < data_len; i++) fft_data[i] = read_sample_to_float(sf);
      if (data_len < fs->size) 
	for (i = data_len; i < fs->size; i++) 
	  fft_data[i] = 0.0;
      autocorrelation(fft_data, fs->size);
      break;
    default:
      {
	XEN res = XEN_FALSE; XEN sfd;
	vct *v;
	int len;
	sfd = g_c_make_sample_reader(sf);
	snd_protect(sfd);
	res = XEN_CALL_2(added_transform_proc(cp->transform_type), 
			 C_TO_XEN_INT(data_len), 
			 sfd,
			 "added transform func");
	snd_protect(res);
	if (VCT_P(res))
	  {
	    v = TO_VCT(res);
	    len = v->length;
	    for (i = 0; i < len; i++) fft_data[i] = v->data[i];
	  }
	snd_unprotect(res);
	snd_unprotect(sfd);
	free_snd_fd_almost(sf);
	return;
      }
      break;
    }
  free_snd_fd(sf);
}

static void display_fft(fft_state *fs)
{
  fft_info *fp;
  chan_info *cp;
  int di;
  Float max_freq = 0.0, min_freq = 0.0, max_val, min_val, data_max = 0.0, scale = 1.0;
  char *xlabel;
  fft_info *nfp;
  Float *data, *tdata;
  chan_info *ncp;
  snd_info *sp;
  int i, j, lo, hi;

  cp = (chan_info *)(fs->chan);
  if ((cp == NULL) || (!(cp->active))) return;
  fp = cp->fft;
  if (fp == NULL) return; /* can happen if selection transform set, but no selection */
  data = fp->data;
  if (data == NULL) return;
  sp = cp->sound;
  if (cp->transform_graph_type == GRAPH_ONCE)
    {
      xlabel = spectro_xlabel(cp);
      switch (cp->transform_type)
	{
	case FOURIER: 
	  if (cp->fft_log_frequency)
	    {
	      max_freq = cp->spectro_cutoff;
	      min_freq = cp->spectro_start;
	    }
	  else
	    {
	      max_freq = ((Float)(SND_SRATE(sp)) * 0.5 * cp->spectro_cutoff);
	      min_freq = ((Float)(SND_SRATE(sp)) * 0.5 * cp->spectro_start);
	    }
	  break;
	case WAVELET: case HADAMARD: case WALSH: case HAAR:
	  max_freq = fs->size * cp->spectro_cutoff; 
	  min_freq = fs->size * cp->spectro_start; 
	  break;
	case AUTOCORRELATION: case CEPSTRUM:
	  max_freq = fs->size * cp->spectro_cutoff / 2; 
	  min_freq = fs->size * cp->spectro_start / 2; 
	  break;
	default:
	  min_freq = added_transform_lo(cp->transform_type) * fs->size * cp->spectro_cutoff; 
	  max_freq = added_transform_hi(cp->transform_type) * fs->size * cp->spectro_cutoff; 
	  break;
	}

      if (cp->transform_normalization == DONT_NORMALIZE)
	{
	  lo = 0;
	  hi = (int)(fp->current_size / 2);
	}
      else
	{
	  if (cp->transform_type == FOURIER)
	    {
	      hi = (int)(fs->size * cp->spectro_cutoff / 2);
	      lo = (int)(fs->size * cp->spectro_start / 2);
	    }
	  else
	    {
	      hi = (int)(fs->size * cp->spectro_cutoff);
	      lo = (int)(fs->size * cp->spectro_start);
	    }
	}

      data_max = 0.0;
      if ((cp->transform_normalization == NORMALIZE_BY_SOUND) ||
	  ((cp->transform_normalization == DONT_NORMALIZE) && 
	   (sp->nchans > 1) && 
	   (sp->channel_style == CHANNELS_SUPERIMPOSED)))
	{
	  for (j = 0; j < sp->nchans; j++)
	    {
	      ncp = sp->chans[j];
	      if ((ncp->graph_transform_p) && (ncp->fft)) /* normalize-by-sound but not ffting all chans? */
		{
		  nfp = ncp->fft;
		  tdata = nfp->data;
		  for (i = lo; i < hi; i++) 
		    if (tdata[i] > data_max) 
		      data_max = tdata[i];
		}
	    }
	}
      else
	{
	  if (cp->transform_type == FOURIER)
	    {
	      for (i = lo; i < hi; i++) 
		if (data[i] > data_max) 
		  data_max = data[i];
	    }
	  else
	    {
	      for (i = lo; i < hi; i++)
		{
		  if (data[i] > data_max) 
		    data_max = data[i];
		  else
		    if (data[i] < -data_max) 
		      data_max = -data[i];
		}
	    }
	}

      if (data_max == 0.0) data_max = 1.0;
      if (cp->transform_normalization != DONT_NORMALIZE)
	scale = 1.0 / data_max;
      else 
	{
	  if (cp->transform_type == FOURIER)
	    {
	      scale = 2.0 / (Float)(fs->size);
	      di = (int)(10 * data_max * scale + 1);
	      if (di == 1)
		{
		  di = (int)(100 * data_max * scale + 1);
		  if (di == 1)
		    {
		      di = (int)(1000 * data_max * scale + 1);
		      data_max = (Float)di / 1000.0;
		    }
		  else data_max = (Float)di / 100.0;
		}
	      else data_max = (Float)di / 10.0;
	    }
	  else 
	    {
	      scale = 1.0;
	    }
	}

      if (cp->fft_log_magnitude) 
	{
	  if (cp->transform_normalization == DONT_NORMALIZE)
	    max_val = in_dB(cp->min_dB, cp->lin_dB, data_max);
	  else max_val = 0.0;
	  min_val = cp->min_dB;
	}
      else 
	{
	  if (cp->transform_normalization == DONT_NORMALIZE)
	    {
	      if (cp->transform_type == FOURIER)
		min_val = 0.0;
	      else min_val = -data_max;
	      max_val = data_max;
	    }
	  else
	    {
	      if (cp->transform_type == FOURIER)
		min_val = 0.0;
	      else min_val = -1.0;
	      max_val = 1.0;
	    }
	}
      fp->scale = scale;
      fp->axis = make_axis_info(cp,
				min_freq, max_freq,
				min_val, max_val,
				xlabel,
				min_freq, max_freq,
				min_val, max_val,
				fp->axis);
    }
}

static fft_state *free_fft_state(fft_state *fs)
{
  if (fs) 
    {
      if (fs->window) {FREE(fs->window); fs->window = NULL;}
      FREE(fs); 
    }
  return(NULL);
}

void cp_free_fft_state(chan_info *cp)
{
  if (cp->fft_data)
    cp->fft_data = free_fft_state((fft_state *)(cp->fft_data));
}

int fft_window_beta_in_use(int win) {return(win >= MUS_KAISER_WINDOW);}

static fft_state *make_fft_state(chan_info *cp, int simple)
{
  fft_state *fs = NULL;
  snd_state *ss;
  axis_info *ap;
  int reuse_old = FALSE, fftsize;
  off_t dbeg = 0, dlen = 0;
  ss = cp->state;
  ap = cp->axis;
  if ((show_selection_transform(ss)) && 
      (cp->transform_graph_type == GRAPH_ONCE) && 
      (selection_is_active_in_channel(cp)))
    {
      /* override transform_size(ss) in this case (sonograms cover selection but use preset size) */
      dbeg = selection_beg(cp);
      dlen = selection_len();
      /* these need to be handled at the same time, and not re-examined until the next call */
      /* if we're sweeping the mouse defining the selection, by the time we get to apply_fft, selection_len() can change */
      fftsize = snd_2pow2(dlen * (1 + cp->zero_pad));
      if (fftsize < 2) fftsize = 2;
      cp->selection_transform_size = fftsize;
    }
  else 
    {
      if ((cp->zero_pad == 0) && (POWER_OF_2_P(cp->transform_size)))
	fftsize = cp->transform_size;
      else fftsize = snd_ipow2((int)((log((Float)(cp->transform_size * (1 + cp->zero_pad))) / log(2.0)) + .001));
      if (fftsize < 2) fftsize = 2;
      cp->selection_transform_size = 0;
    }
  if ((simple) && (cp->fft_data) && (cp->selection_transform_size == 0))
    {
      fs = (fft_state *)(cp->fft_data);
      if ((fs->losamp == ap->losamp) && 
	  (!(XEN_HOOKED(before_transform_hook))) &&
	  (fs->size == fftsize) &&
	  (fs->transform_type == cp->transform_type) &&
	  (fs->wintype == cp->fft_window) &&
	  ((!(fft_window_beta_in_use(fs->wintype))) || (fs->beta == cp->fft_window_beta)) &&
	  (fs->dBing == cp->fft_log_magnitude) &&
	  (fs->lfreq == cp->fft_log_frequency) &&
	  (fs->pad_zero == cp->zero_pad) &&
	  (fs->cutoff == cp->spectro_cutoff) &&
	  (fs->old_style == cp->transform_graph_type) &&
	  (fs->wavelet_choice == cp->wavelet_type) &&
	  (fs->edit_ctr == cp->edit_ctr))
	reuse_old = TRUE;
    }
  if (!reuse_old)
    {
      cp_free_fft_state(cp);
      fs = (fft_state *)CALLOC(1, sizeof(fft_state));
      fs->chan = cp;
      fs->cutoff = cp->spectro_cutoff;
      fs->size = fftsize;
      fs->pad_zero = cp->zero_pad;
      fs->wintype = cp->fft_window;
      fs->dBing = cp->fft_log_magnitude;
      fs->lfreq = cp->fft_log_frequency;
      fs->window = NULL;
      fs->losamp = ap->losamp;
      fs->edit_ctr = cp->edit_ctr;
      fs->ss = ss;
      fs->wavelet_choice = cp->wavelet_type;
      fs->transform_type = cp->transform_type;
      fs->old_style = cp->transform_graph_type;
      fs->beta = cp->fft_window_beta;
    }
  fs->done = reuse_old;
  fs->beg = 0;
  fs->databeg = dbeg;
  fs->datalen = dlen;
  if (simple) cp->fft_data = fs; else cp->fft_data = NULL;
  return(fs);
}

static int last_size = 0, last_wintype = -1, last_zero = 0;
static Float last_beta = 0.0;
static Float *last_window = NULL;

static fft_info *make_fft_info(int size, int window, Float beta)
{
  fft_info *fp;
  fp = (fft_info *)CALLOC(1, sizeof(fft_info));
  fp->size = size;
  fp->window = window;
  fp->beta = beta;
  fp->ok = TRUE;
  fp->data = (Float *)CALLOC(size + 1, sizeof(Float)); /*  + 1 for complex storage or starts at 1 or something */
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

static void one_fft(fft_state *fs)
{
  if (!fs->done)
    {
      /* allocate arrays if needed */
      fft_info *fp;
      chan_info *cp;
      cp = (chan_info *)(fs->chan);
      fp = cp->fft;
      if (!fp)                              /* associated channel hasn't done any ffts yet, so there's no struct */
	{
	  cp->fft = make_fft_info(fs->size, fs->wintype, 0.0);
	  fp = cp->fft;
	}
      else
	{
	  if ((!fp->ok) || (!fp->data) || (fs->size > fp->size))
	    {
	      fp->size = fs->size;
	      if (fp->data) FREE(fp->data);
	      fp->data = (Float *)CALLOC(fp->size + 1, sizeof(Float));
	      fp->ok = TRUE;
	    }
	}
      fp->current_size = fs->size; /* protect against parallel size change via fft size menu */
      fs->data = fp->data;
      if (fs->window == NULL)
	{
	  fs->window = (Float *)CALLOC(fs->size, sizeof(Float));
	  if ((fs->wintype != last_wintype) ||
	      (fs->size != last_size) ||
	      (fs->beta != last_beta) ||
	      (fs->pad_zero != last_zero))
	    {
	      if (last_window) FREE(last_window);
	      last_window = (Float *)CALLOC(fs->size, sizeof(Float));
	      if (cp->selection_transform_size > 0)
		mus_make_fft_window_with_window(fs->wintype, cp->selection_transform_size, fs->beta * beta_maxes[fs->wintype], last_window);
	      else mus_make_fft_window_with_window(fs->wintype, cp->transform_size, fs->beta * beta_maxes[fs->wintype], last_window);
	      last_size = fs->size;
	      last_beta = fs->beta;
	      last_wintype = fs->wintype;
	      last_zero = fs->pad_zero;
	    }
	  memcpy(fs->window, (void *)last_window, fs->size * sizeof(Float));
	}
      apply_fft(fs);
    }
  display_fft(fs);
}

void single_fft(chan_info *cp, int dpy)
{
  if (cp->transform_size < 2) return;
  one_fft(make_fft_state(cp, TRUE));
  if (dpy == 0) display_channel_fft_data(cp, cp->sound, cp->state);
  enved_fft_update();
}


/* -------------------------------- GRAPH_AS_SONOGRAM -------------------------------- */
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
  int outlim, outer;
  fft_state *fs;
  chan_info *cp;
  int spectrum_size;
  sono_info *scp;
  off_t beg, losamp, hisamp;
  int done, hop;
  int window;
  int msg_ctr;
  int edit_ctr;
  Float old_scale;
  int old_style, old_logxing, transform_type, w_choice;
  int minibuffer_needs_to_be_cleared;
} sonogram_state;

void clear_transform_edit_ctrs(chan_info *cp)
{
  fft_state *fs;
  sonogram_state *lsg;
  if (cp->fft_data)
    {
      fs = (fft_state *)(cp->fft_data);
      fs->edit_ctr = -1;
    }
  if (cp->last_sonogram)
    {
      lsg = (sonogram_state *)(cp->last_sonogram);
      lsg->edit_ctr = -1;
    }
}

void *make_sonogram_state(chan_info *cp)
{
  sonogram_state *sg, *temp_sg;
  fft_state *fs;
  sg = (sonogram_state *)CALLOC(1, sizeof(sonogram_state));
  sg->cp = cp;
  sg->done = FALSE;
  fs = make_fft_state(cp, FALSE); /* 0=>not a simple one-shot fft */
  sg->fs = fs;
  sg->msg_ctr = 8;
  sg->transform_type = cp->transform_type;
  sg->w_choice = cp->wavelet_type;
  sg->minibuffer_needs_to_be_cleared = FALSE;
  if (cp->temp_sonogram)
    {
      /* we must have restarted fft process without letting the previous run at all */
      temp_sg = (sonogram_state *)(cp->temp_sonogram);
      if (temp_sg->fs) temp_sg->fs = free_fft_state(temp_sg->fs);
      FREE(temp_sg);
      /* cp->last_sonogram = NULL; */
    }
  cp->temp_sonogram = sg; /* background process may never run, so we need a way to find this pointer at cleanup time */
  return((void *)sg);
}

void free_sonogram_fft_state(void *ptr)
{
  sonogram_state *sg = (sonogram_state *)ptr;
  if (sg->fs) free_fft_state(sg->fs);
  sg->fs = NULL;
}

void free_sono_info(chan_info *cp)
{
  int i;
  sono_info *si;
  si = (sono_info *)(cp->sonogram_data);
  if (si)
    {
      if (si->begs) FREE(si->begs);
      if (si->data)
	{
	  for (i = 0; i < si->total_slices; i++)
	    if (si->data[i]) 
	      FREE(si->data[i]);
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
  int i, tempsize, dpys = 1;
  cp = sg->cp;
  if (cp->fft_changed != FFT_CHANGE_LOCKED)
    cp->fft_changed = FFT_UNCHANGED;
  else cp->fft_changed = FFT_CHANGED;
  if ((cp->graph_transform_p == 0) || (cp->transform_size <= 1)) return(2);
  ss = cp->state;
  ap = cp->axis;
  sg->slice = 0;
  sg->outer = 0;
  sg->beg = ap->losamp;
  sg->losamp = ap->losamp;
  sg->hisamp = ap->hisamp;
  sg->window = cp->fft_window;
  sg->minibuffer_needs_to_be_cleared = FALSE;
  if (cp->graph_time_p) dpys++; 
  if (cp->graph_lisp_p) dpys++; 
  if (cp->transform_graph_type == GRAPH_AS_SPECTROGRAM)
    sg->outlim = ap->height / cp->spectro_hop;
  else sg->outlim = ap->window_width / dpys;
  if (sg->outlim <= 1) return(2);
  sg->hop = (int)(ceil((double)(ap->hisamp - ap->losamp + 1) / (double)(sg->outlim)));
  /* if fewer samps than pixels, draw rectangles */
  if ((cp->transform_type == FOURIER) || 
      (cp->transform_type == AUTOCORRELATION))
    sg->spectrum_size = (cp->transform_size) / 2;
  else sg->spectrum_size = cp->transform_size;
  if (sg->spectrum_size <= 0) return(2);
  sg->edit_ctr = cp->edit_ctr;
  si = (sono_info *)(cp->sonogram_data);
  if (!si)
    {
      si = (sono_info *)CALLOC(1, sizeof(sono_info));
      cp->sonogram_data = si;
      si->total_bins = sg->spectrum_size;
      si->total_slices = snd_2pow2(sg->outlim);
      si->begs = (off_t *)CALLOC(si->total_slices, sizeof(off_t));
      si->data = (Float **)CALLOC(si->total_slices, sizeof(Float *));
      for (i = 0; i < si->total_slices; i++) si->data[i] = (Float *)CALLOC(si->total_bins, sizeof(Float));
    }
  else
    if ((si->total_slices < sg->outlim) || 
	(si->total_bins < sg->spectrum_size))
      {
	for (i = 0; i < si->total_slices; i++) 
	  if (si->data[i]) 
	    {
	      FREE(si->data[i]); 
	      si->data[i] = NULL;
	    }
	tempsize = snd_2pow2(sg->outlim);
	if (si->total_slices < tempsize) 
	  {
	    FREE(si->data);
	    si->total_slices = tempsize;
	    si->begs = (off_t *)REALLOC(si->begs, si->total_slices * sizeof(off_t));
	    si->data = (Float **)CALLOC(si->total_slices, sizeof(Float *));
	  }
	if (si->total_bins < sg->spectrum_size) si->total_bins = sg->spectrum_size;
	for (i = 0; i < si->total_slices; i++) si->data[i] = (Float *)CALLOC(si->total_bins, sizeof(Float));
      }
  sg->scp = si;
  si->target_bins = sg->spectrum_size;
  si->active_slices = 0;
  si->target_slices = sg->outlim;
  si->scale = 0.0;
  if (cp->last_sonogram)                               /* there was a previous run */
    {
      lsg = (sonogram_state *)(cp->last_sonogram);
      if ((lsg->done) &&                               /* it completed all ffts */
	  (lsg->outlim == sg->outlim) &&               /* the number of ffts is the same */
	  (lsg->spectrum_size == sg->spectrum_size) && /* ditto fft sizes */
	  (lsg->losamp == sg->losamp) &&               /* begins are same */
	  (lsg->hisamp == sg->hisamp) &&               /* ends are same */
	  (lsg->window == sg->window) &&               /* data windows are same */
	  (lsg->transform_type == sg->transform_type) && /* transform types are the same */
	  (lsg->w_choice == sg->w_choice) &&           /* wavelets are the same */
	  (lsg->edit_ctr == sg->edit_ctr))             /* underlying data is the same */
	{
	  sg->outer = sg->outlim;                      /* fake up the run */
	  si->active_slices = si->target_slices;
	  sg->old_scale = lsg->old_scale;
	  si->scale = sg->old_scale;
	  if ((lsg->old_style != cp->transform_graph_type) ||
	      (lsg->old_logxing != cp->fft_log_frequency))
	    make_sonogram_axes(cp);                    /* may need to fixup frequency axis labels */
	  sg->old_style = cp->transform_graph_type;
	  sg->old_logxing = cp->fft_log_frequency;
	  return(2);                                   /* so skip the ffts! */
	}
    }
  cp->fft_changed = FFT_CHANGED;
  start_progress_report(cp->sound, NOT_FROM_ENVED);
  return(1);
}

static int run_all_ffts(sonogram_state *sg)
{
  fft_state *fs;
  sono_info *si;
  chan_info *cp;
  axis_info *ap;
  Float val;
  int i;
  /* return 0 until done with all ffts, then 1 -- 1 causes cleanup whether done or not */
  /* check for losamp/hisamp change? */
  one_fft((fft_state *)(sg->fs));
  fs = sg->fs;
  cp = sg->cp;
  si = (sono_info *)(cp->sonogram_data);
  if (si->active_slices < si->total_slices) 
    si->begs[si->active_slices] = sg->beg + fs->beg;
  sg->msg_ctr--;
  if (sg->msg_ctr == 0)
    {
      progress_report(cp->sound, 
		      (cp->transform_graph_type == GRAPH_AS_SONOGRAM) ? S_graph_as_sonogram : S_graph_as_spectrogram, 
		      0, 0,
		      ((Float)(si->active_slices) / (Float)(si->target_slices)), 
		      NOT_FROM_ENVED);
      sg->minibuffer_needs_to_be_cleared = TRUE;
      sg->msg_ctr = 8;
      if (cp->graph_transform_p == 0) return(1);
    }
  if (si->active_slices < si->total_slices)
    {
      if (cp->transform_type == FOURIER)
	{
	  for (i = 0; i < sg->spectrum_size; i++) 
	    {
	      val = fs->data[i];
	      if (val > si->scale) si->scale = val;
	      si->data[si->active_slices][i] = val;
	    }
	}
      else
	{
	  for (i = 0; i < sg->spectrum_size; i++) 
	    {
	      val = fs->data[i];
	      if (val < 0.0) val = -val;  /* kinda dubious but I can't think of a good alternative */
	      if (val > si->scale) si->scale = val;
	      si->data[si->active_slices][i] = val;
	    }
	}
      si->active_slices++;
    }
  sg->outer++;
  if ((sg->outer == sg->outlim) || (cp->graph_transform_p == 0) || (cp->transform_graph_type == GRAPH_ONCE)) return(1);
  fs->beg += sg->hop;
  
  ap = cp->axis;
  if ((sg->losamp != ap->losamp) || (sg->hisamp != ap->hisamp)) 
    {
      fs->beg = 0;
      return(-1);
    }
  return(0);
}

static int cleanup_sonogram(sonogram_state *sg)
{
  chan_info *cp;
  if (sg)
    {
      cp = sg->cp;
      if (cp->graph_transform_p == 0)
	{
	  if (sg->fs) sg->fs = free_fft_state(sg->fs);
	  return(1);
	}
      if ((sg->scp != NULL) && (sg->outlim > 1))
	make_sonogram_axes(cp);
      if (sg->fs) sg->fs = free_fft_state(sg->fs);
      cp->fft_data = NULL;
      set_chan_fft_in_progress(cp, 0); /* i.e. clear it */
      if ((sg->scp != NULL) && (sg->outlim > 1))
	{
	  display_channel_fft_data(cp, cp->sound, cp->state);
	  if (sg->outer == sg->outlim) sg->done = TRUE;
	  sg->old_scale = (sg->scp)->scale;
	}
      else sg->done = TRUE;
      if ((cp->last_sonogram) && (cp->last_sonogram != sg)) FREE(cp->last_sonogram);
      cp->last_sonogram = sg;
      if (sg->minibuffer_needs_to_be_cleared)
	{
	  finish_progress_report(cp->sound, NOT_FROM_ENVED);
	  sg->minibuffer_needs_to_be_cleared = FALSE;
	}
    }
  return(1);
}

Cessate sonogram_in_slices(void *sono)
{
  sonogram_state *sg = (sonogram_state *)sono;
  chan_info *cp;
  int res = 0;
  cp = sg->cp;
  cp->temp_sonogram = NULL;
  if (cp->graph_transform_p == 0) 
    {
      if (sg) cleanup_sonogram(sg);
      return(BACKGROUND_QUIT);
    }
  switch (sg->slice)
    {
    case 0: res = set_up_sonogram(sg); break; /* return 1 to go on, 2 to quit early */
    case 1: res = run_all_ffts(sg);    break; /* return 0 until done with all ffts, then 1 -- 1 causes cleanup whether done or not */
    case 2: res = cleanup_sonogram(sg); return(BACKGROUND_QUIT); break;
    default: 
      snd_error("runaway sonogram? (%d) ", sg->slice); 
      cleanup_sonogram(sg); 
      return(BACKGROUND_QUIT); 
      break;
    }
  sg->slice += res;
  return(BACKGROUND_CONTINUE);
}

void sono_update(chan_info *cp)
{
  if (cp->transform_graph_type != GRAPH_ONCE) make_sonogram_axes(cp);
  update_graph(cp);
}

void set_spectro_cutoff_and_redisplay(snd_state *ss, Float val)
{
  in_set_spectro_cutoff(ss, val); 
  for_each_chan(ss, sono_update);
}

static void spectral_multiply (Float* rl1, Float* rl2, int n)
{
  int j, n2, nn2;
  Float rem, rep, aim, aip, invn;
  n2 = (int)(n * 0.5);
  invn = 0.25 / n;
  rl1[0] = ((rl1[0] * rl2[0]) / n);
  rl2[0] = 0.0;
  for (j = 1; j <= n2; j++)
    {
      nn2 = n - j;
      rep = (rl1[j] + rl1[nn2]);
      rem = (rl1[j] - rl1[nn2]);
      aip = (rl2[j] + rl2[nn2]);
      aim = (rl2[j] - rl2[nn2]);
      rl1[j] = invn * (rep * aip + aim * rem);
      rl1[nn2] = rl1[j];
      rl2[j] = invn * (aim * aip - rep * rem);
      rl2[nn2] = -rl2[j];
    }
}

void c_convolve(char *fname, Float amp, int filec, off_t filehdr, int filterc, off_t filterhdr, int filtersize,
		int fftsize, int filter_chans, int filter_chan, int data_size, snd_info *gsp, int from_enved, int ip, int total_chans)
{
  Float *rl0 = NULL, *rl1 = NULL, *rl2 = NULL;
  mus_sample_t **pbuffer = NULL, **fbuffer = NULL;
  mus_sample_t *cm = NULL, *fcm = NULL, *pbuf = NULL;
  int i;
  Float scl;
  int tempfile;

  /* need file to hold convolution output */
  tempfile = mus_file_create(fname);
  if (tempfile != -1)
    {
      /* get to start point in the two sound files */
      lseek(filec, filehdr, SEEK_SET);
      lseek(filterc, filterhdr, SEEK_SET);

      rl0 = (Float *)CALLOC(fftsize, sizeof(Float));
      if (rl0) rl1 = (Float *)CALLOC(fftsize, sizeof(Float));
      if (rl1) pbuffer = (mus_sample_t **)CALLOC(1, sizeof(mus_sample_t *));
      if (pbuffer) pbuffer[0] = (mus_sample_t *)CALLOC(data_size, sizeof(mus_sample_t));
      if (pbuffer[0]) cm = (mus_sample_t *)CALLOC(1, sizeof(mus_sample_t));
      fbuffer = (mus_sample_t **)CALLOC(filter_chans, sizeof(mus_sample_t *));
      if (fbuffer) fbuffer[filter_chan] = (mus_sample_t *)CALLOC(filtersize, sizeof(mus_sample_t));
      if (fbuffer[filter_chan]) fcm = (mus_sample_t *)CALLOC(filter_chans, sizeof(mus_sample_t));
      if ((rl0 == NULL) || (rl1 == NULL) || 
	  (pbuffer == NULL) || (pbuffer[0] == NULL) || (cm == NULL) ||
	  (fbuffer == NULL) || (fbuffer[filter_chan] == NULL) || (fcm == NULL))
	{
	  snd_error(_("not enough memory for convolve of %s (filter size: %d, fft size: %d)"), 
		    fname, filtersize, fftsize);
	}
      else
	{
	  cm[0] = (mus_sample_t)1;
	  fcm[filter_chan] = (mus_sample_t)1;
	  pbuf = pbuffer[0];

	  /* read in the "impulse response" */
	  mus_file_read_any(filterc, 0, filter_chans, filtersize - 1, fbuffer, fcm);
	  for (i = 0; i < filtersize; i++) 
	    rl1[i] = MUS_SAMPLE_TO_FLOAT(fbuffer[filter_chan][i]);
	  progress_report(gsp, "convolve", ip + 1, total_chans, .1, from_enved);
	  mus_header_write_next_header(tempfile, 22050, 1, 28, data_size * 4, MUS_BINT, NULL, 0);
	  mus_file_open_descriptors(tempfile, fname, MUS_BINT, 4, 28, 1, MUS_NEXT);
	  /* get the convolution data */
	  mus_file_read_any(filec, 0, 1, data_size - 1, pbuffer, cm);
	  for (i = 0; i < data_size; i++) rl0[i] = MUS_SAMPLE_TO_FLOAT(pbuf[i]);

	  progress_report(gsp, "convolve", ip + 1, total_chans, .3, from_enved);
	  mus_fft(rl0, rl1, fftsize, 1);
	  progress_report(gsp, "convolve", ip + 1, total_chans, .5, from_enved);
	  spectral_multiply(rl0, rl1, fftsize);
	  progress_report(gsp, "convolve", ip + 1, total_chans, .6, from_enved);
	  mus_fft(rl0, rl1, fftsize, -1);
	  progress_report(gsp, "convolve", ip + 1, total_chans, .8, from_enved);

	  if (amp != 0.0)
	    {
	      /* normalize the results */
	      scl = 0.0;
	      for (i = 0; i < data_size; i++) 
		{
		  Float val;
		  val = fabs(rl0[i]);
		  if (val > scl) scl = val;
		}
	      if (scl != 0.0) scl = amp / scl;
	      for (i = 0; i < data_size; i++) 
		pbuf[i] = MUS_FLOAT_TO_SAMPLE(scl * rl0[i]);
	    }
	  else 
	    {
	      /* amp == 0.0 means un-normalized output */
	      for (i = 0; i < data_size; i++) 
		pbuf[i] = MUS_FLOAT_TO_SAMPLE(rl0[i]);
	    }
	  progress_report(gsp, "convolve", ip + 1, total_chans, .9, from_enved);
	  /* and save as temp file */
	  mus_file_write(tempfile, 0, data_size - 1, 1, &(pbuf));
	  if (mus_file_close(tempfile) != 0)
	    snd_error(_("convolve: can't close temp file %s!"), fname);
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

static XEN g_autocorrelate(XEN reals)
{
  #define H_autocorrelate "(" S_autocorrelate " data) returns (in place) the autocorrelation of data (vct)"
  /* assumes length is power of 2 */
  vct *v1 = NULL;
  XEN_ASSERT_TYPE(VCT_P(reals), reals, XEN_ONLY_ARG, S_autocorrelate, "a vct");
  v1 = (vct *)XEN_OBJECT_REF(reals);
  autocorrelation(v1->data, v1->length);
  return(reals);
}

static XEN g_add_transform(XEN name, XEN xlabel, XEN lo, XEN hi, XEN proc)
{
  #define H_add_transform "(" S_add_transform " name x-label low high func) adds the transform func \
to the transform lists; func should be a function of two arguments, the length of the transform \
and a sample-reader to get the data, and should return a vct object containing the transform results. \
'name' is the transform's name, x-label is its x-axis label, and the relevant returned data \
to be displayed goes from low to high (normally 0.0 to 1.0)"

  char *errmsg;
  XEN errstr;
  errmsg = procedure_ok(proc, 2, S_add_transform, "transform", 5);
  if (errmsg)
    {
      errstr = C_TO_XEN_STRING(errmsg);
      FREE(errmsg);
      return(snd_bad_arity_error(S_add_transform, errstr, proc));
    }
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_1, S_add_transform, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_P(xlabel), xlabel, XEN_ARG_2, S_add_transform, "a string");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(lo), lo, XEN_ARG_3, S_add_transform, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(hi), hi, XEN_ARG_4, S_add_transform, "a number");
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(proc), proc, XEN_ARG_5, S_add_transform, "a procedure");
  return(C_TO_SMALL_XEN_INT(add_transform(XEN_TO_C_STRING(name),
					  XEN_TO_C_STRING(xlabel),
					  XEN_TO_C_DOUBLE(lo),
					  XEN_TO_C_DOUBLE(hi),
					  proc)));
}

static XEN g_transform_samples_size(XEN snd, XEN chn)
{
  #define H_transform_samples_size "(" S_transform_samples_size " &optional snd chn)\n\
returns a description of transform graph data in snd's channel chn, based on " S_transform_graph_type ".\
If no transform graph, returns 0; if " S_graph_once ", returns " S_transform_size ",\
and otherwise returns a list (total-size active-bins active-slices)"

  chan_info *cp;
  sono_info *si;
  ASSERT_CHANNEL(S_transform_samples_size, snd, chn, 1);
  cp = get_cp(snd, chn, S_transform_samples_size);
  if (!(cp->graph_transform_p)) 
    return(XEN_ZERO);
  if (cp->transform_graph_type == GRAPH_ONCE)
    return(C_TO_SMALL_XEN_INT(cp->transform_size));
  si = (sono_info *)(cp->sonogram_data);
  if (si) return(XEN_LIST_3(C_TO_XEN_DOUBLE(cp->spectro_cutoff),
			    C_TO_SMALL_XEN_INT(si->active_slices),
			    C_TO_SMALL_XEN_INT(si->target_bins)));
  return(XEN_ZERO);
}

static XEN g_transform_sample(XEN bin, XEN slice, XEN snd_n, XEN chn_n)
{
  #define H_transform_sample "(" S_transform_sample " &optional (bin 0) (slice 0) snd chn)\n\
returns the current transform sample at bin and slice in snd channel chn (assuming sonogram or spectrogram)"

  chan_info *cp;
  fft_info *fp;
  sono_info *si;
  int fbin, fslice;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(bin), bin, XEN_ARG_1, S_transform_sample, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(slice), slice, XEN_ARG_2, S_transform_sample, "an integer");
  ASSERT_CHANNEL(S_transform_sample, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_transform_sample);
  if (cp->graph_transform_p)
    {
      fbin = XEN_TO_C_INT_OR_ELSE(bin, 0);
      fp = cp->fft;
      if (fp)
	{
	  if (fbin < fp->current_size)
	    {
	      if (cp->transform_graph_type == GRAPH_ONCE)
		return(C_TO_XEN_DOUBLE(fp->data[fbin]));
	      else 
		{
		  fslice = XEN_TO_C_INT_OR_ELSE(slice, 0);
		  si = (sono_info *)(cp->sonogram_data);
		  if ((si) && 
		      (fbin < si->target_bins) && 
		      (fslice < si->active_slices))
		    return(C_TO_XEN_DOUBLE(si->data[fslice][fbin]));
		  else XEN_ERROR(NO_SUCH_SAMPLE,
				 XEN_LIST_8(C_TO_XEN_STRING(S_transform_sample),
					    bin, slice,
					    snd_n, chn_n,
					    C_TO_XEN_STRING("max bin, max slice:"),
					    C_TO_XEN_INT((si) ? si->target_bins : 0),
					    C_TO_XEN_INT((si) ? si->active_slices : 0)));
		}
	    }
	  else XEN_ERROR(NO_SUCH_SAMPLE,
			 XEN_LIST_6(C_TO_XEN_STRING(S_transform_sample),
				    bin,
				    snd_n, chn_n,
				    C_TO_XEN_STRING("max bin:"),
				    C_TO_XEN_INT(fp->current_size)));
	}
    }
  return(XEN_FALSE);
}  

static XEN transform_samples2vct(XEN snd_n, XEN chn_n, XEN v)
{
  #define H_transform_samples2vct "(" S_transform_samples2vct " &optional snd chn vct-obj)\n\
returns a vct object (vct-obj if passed), with the current transform data from snd's channel chn"

  chan_info *cp;
  fft_info *fp;
  sono_info *si;
  int i, j, k, len, bins, slices;
  Float *fvals;
  vct *v1 = get_vct(v);
  ASSERT_CHANNEL(S_transform_samples2vct, snd_n, chn_n, 1);
  cp = get_cp(snd_n, chn_n, S_transform_samples2vct);
  if ((cp->graph_transform_p) && (cp->fft))
    {
      if (cp->transform_graph_type == GRAPH_ONCE)
	{
	  fp = cp->fft;
	  len = fp->current_size;
	  if (v1)
	    fvals = v1->data;
	  else fvals = (Float *)MALLOC(len * sizeof(Float));
	  memcpy((void *)fvals, (void *)(fp->data), len * sizeof(Float));
	  if (v1)
	    return(v);
	  else return(make_vct(len, fvals));
	}
      else
	{
	  si = (sono_info *)(cp->sonogram_data);
	  if (si)
	    {
	      slices = si->active_slices;
	      bins = si->target_bins;
	      len = bins * slices;
	      if (v1)
		fvals = v1->data;
	      else fvals = (Float *)CALLOC(len, sizeof(Float));
	      for (i = 0, k = 0; i < slices; i++)
		for (j = 0; j < bins; j++, k++)
		  fvals[k] = si->data[i][j];
	      if (v1)
		return(v);
	      else return(make_vct(len, fvals));
	    }
	}
    }
  return(XEN_FALSE);
}  

static XEN g_transform_samples(XEN snd_n, XEN chn_n)
{
  #define H_transform_samples "(" S_transform_samples " &optional snd chn) -> current transform data for snd channel chn"
  XEN val;
  val = transform_samples2vct(snd_n, chn_n, XEN_FALSE);
  if (VCT_P(val))
    return(vct2vector(val));
  return(XEN_FALSE);
}

static XEN g_snd_transform(XEN type, XEN data, XEN hint)
{
  int trf, i, j, hnt, n2;
  vct *v;
  Float *dat;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(type), type, XEN_ARG_1, "snd-transform", "an integer");
  XEN_ASSERT_TYPE(VCT_P(data), data, XEN_ARG_2, "snd-transform", "a vct");
  trf = XEN_TO_SMALL_C_INT(type);
  if ((trf < 0) || (trf > HAAR))
    mus_misc_error("snd-transform", _("invalid transform choice"), type);
  v = TO_VCT(data);
  switch (trf)
    {
    case FOURIER: 
      n2 = v->length / 2;
#if HAVE_FFTW
      if ((XEN_BOUND_P(hint)) && (XEN_TRUE_P(hint)))
	{
	  mus_fftw(v->data, v->length, 1);
	  v->data[0] *= v->data[0];
	  v->data[n2] *= v->data[n2];
	  for (i = 1, j = v->length - 1; i < n2; i++, j--) 
	    v->data[i] = v->data[i] * v->data[i] + v->data[j] * v->data[j];
	}
      else
	{
#endif
	  dat = (Float *)CALLOC(v->length, sizeof(Float));
	  mus_fft(v->data, dat, v->length, 1);
	  v->data[0] *= v->data[0];
	  v->data[n2] *= v->data[n2];
	  for (i = 1, j = v->length - 1; i < n2; i++, j--)
	    {
	      v->data[i] = v->data[i] * v->data[i] + dat[i] * dat[i];
	      v->data[j] = v->data[i];
	    }
	  FREE(dat);
#if HAVE_FFTW
	}
#endif
      break;
    case WAVELET:
      hnt = XEN_TO_SMALL_C_INT(hint);
      if (hnt < NUM_WAVELETS)
	wavelet_transform(v->data, v->length, wavelet_data[hnt], wavelet_sizes[hnt]);
      break;
    case HAAR:
      haar_transform(v->data, v->length);
      break;
    case CEPSTRUM:
      cepstrum(v->data, v->length);
      break;
    case WALSH:
      walsh_transform(v->data, v->length);
      break;
    case AUTOCORRELATION:
      autocorrelation(v->data, v->length);
      break;
    case HADAMARD:
      dat = (Float *)CALLOC(v->length, sizeof(Float));
      fast_hwt(dat, v->data, (int)(log((Float)(v->length + 1)) / log(2.0)));
      memcpy((void *)(v->data), (void *)dat, (v->length * sizeof(Float)));
      FREE(dat);
      break;
    }
  return(data);
}

#ifdef XEN_ARGIFY_1
XEN_ARGIFY_2(g_transform_samples_size_w, g_transform_samples_size)
XEN_ARGIFY_2(g_transform_samples_w, g_transform_samples)
XEN_ARGIFY_4(g_transform_sample_w, g_transform_sample)
XEN_ARGIFY_3(transform_samples2vct_w, transform_samples2vct)
XEN_NARGIFY_1(g_autocorrelate_w, g_autocorrelate)
XEN_NARGIFY_5(g_add_transform_w, g_add_transform)
XEN_ARGIFY_3(g_snd_transform_w, g_snd_transform)
#else
#define g_transform_samples_size_w g_transform_samples_size
#define g_transform_samples_w g_transform_samples
#define g_transform_sample_w g_transform_sample
#define transform_samples2vct_w transform_samples2vct
#define g_autocorrelate_w g_autocorrelate
#define g_add_transform_w g_add_transform
#define g_snd_transform_w g_snd_transform
#endif

void g_init_fft(void)
{
  #define H_before_transform_hook S_before_transform_hook " (snd chn) is called just before a transform is calculated.  If it returns \
an integer, it is used as the starting point of the transform.  The following \
somewhat brute-force code shows a way to have the transform reflect the position \
of a moving mark:\n\
  (define transform-position #f)\n\
  (add-hook! before-transform-hook \n\
    (lambda (snd chn) transform-position))\n\
  (add-hook! mark-drag-hook \n\
    (lambda (id)\n\
      (set! transform-position (mark-sample id))\n\
      (update-transform)))"

  XEN_DEFINE_HOOK(before_transform_hook, S_before_transform_hook, 2, H_before_transform_hook);  /* args = snd chn */

  #define H_fourier_transform   S_transform_type " value for Fourier transform (sinusoid basis)"
  #define H_wavelet_transform   S_transform_type " value for wavelet transform (" S_wavelet_type " chooses wavelet)"
  #define H_haar_transform      S_transform_type " value for Haar transform"
  #define H_cepstrum            S_transform_type " value for cepstrum (log of power spectrum)"
  #define H_hadamard_transform  S_transform_type " value for Hadamard transform"
  #define H_walsh_transform     S_transform_type " value for Walsh transform (step function basis)"
  #define H_autocorrelation     S_transform_type " value for autocorrelation (ifft of spectrum)"

  XEN_DEFINE_CONSTANT(S_fourier_transform,   FOURIER,         H_fourier_transform);
  XEN_DEFINE_CONSTANT(S_wavelet_transform,   WAVELET,         H_wavelet_transform);
  XEN_DEFINE_CONSTANT(S_haar_transform,      HAAR,            H_haar_transform);
  XEN_DEFINE_CONSTANT(S_cepstrum,            CEPSTRUM,        H_cepstrum);
  XEN_DEFINE_CONSTANT(S_hadamard_transform,  HADAMARD,        H_hadamard_transform);
  XEN_DEFINE_CONSTANT(S_walsh_transform,     WALSH,           H_walsh_transform);
  XEN_DEFINE_CONSTANT(S_autocorrelation,     AUTOCORRELATION, H_autocorrelation);

  #define H_dont_normalize "The value for " S_transform_normalization " that causes the transform to display raw data"
  #define H_normalize_by_channel "The value for " S_transform_normalization " that causes the transform to be normalized in each channel independently"
  #define H_normalize_by_sound "The value for " S_transform_normalization " that causes the transform to be normalized across a sound's channels"
  #define H_normalize_globally "The value for " S_transform_normalization " that causes the transform to be normalized across all sounds"

  XEN_DEFINE_CONSTANT(S_dont_normalize,        DONT_NORMALIZE,       H_dont_normalize);
  XEN_DEFINE_CONSTANT(S_normalize_by_channel,  NORMALIZE_BY_CHANNEL, H_normalize_by_channel);
  XEN_DEFINE_CONSTANT(S_normalize_by_sound,    NORMALIZE_BY_SOUND,   H_normalize_by_sound);
  XEN_DEFINE_CONSTANT(S_normalize_globally,    NORMALIZE_GLOBALLY,   H_normalize_globally);

  XEN_DEFINE_PROCEDURE(S_transform_samples_size,  g_transform_samples_size_w, 0, 2, 0,H_transform_samples_size);
  XEN_DEFINE_PROCEDURE(S_transform_samples,     g_transform_samples_w, 0, 2, 0,   H_transform_samples);
  XEN_DEFINE_PROCEDURE(S_transform_sample,      g_transform_sample_w, 0, 4, 0,    H_transform_sample);
  XEN_DEFINE_PROCEDURE(S_transform_samples2vct, transform_samples2vct_w, 0, 3, 0, H_transform_samples2vct);
  XEN_DEFINE_PROCEDURE(S_autocorrelate,         g_autocorrelate_w, 1, 0, 0,       H_autocorrelate);
  XEN_DEFINE_PROCEDURE(S_add_transform,         g_add_transform_w, 5, 0, 0,       H_add_transform);

  XEN_DEFINE_PROCEDURE("snd-transform",         g_snd_transform_w, 2, 1, 0,       "call transform code directly");
}

