/* CLM (Music V) implementation */

/* many of the generators have *_0 to *_2 versions; oscil_0 for example, alongside oscil.
 *   these represent various (minor) optimizations that are used by the run macros in
 *   snd-run.c and run.lisp.  In some cases (reverb), the overall speed-up can be around
 *   a factor of 2.
 */

#include <mus-config.h>

#if USE_SND
  #include "snd.h"
#endif

#include <stddef.h>
#include <math.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#if (defined(HAVE_LIBC_H) && (!defined(HAVE_UNISTD_H)))
  #include <libc.h>
#else
  #ifndef _MSC_VER
    #include <unistd.h>
  #endif
#endif

#include "_sndlib.h"
#include "clm.h"
#include "clm-strings.h"

#if HAVE_GSL
  #include <gsl/gsl_complex.h>
  #include <gsl/gsl_complex_math.h>
#endif

#if HAVE_FFTW3
  #include <fftw3.h>
#else
  #if HAVE_FFTW
    #include <rfftw.h>
  #endif
#endif

#if HAVE_COMPLEX_TRIG
  #include <complex.h>
#endif

#ifndef TWO_PI
  #define TWO_PI (2.0 * M_PI)
#endif

#if (!HAVE_MEMMOVE)
/* from libit */
static void *memmove (char *dest, const char *source, unsigned int length)
{
  char *d0 = dest;
  if (source < dest)
    /* Moving from low mem to hi mem; start at end.  */
    for (source += length, dest += length; length; --length)
      *--dest = *--source;
  else 
    if (source != dest)
      {
	/* Moving from hi mem to low mem; start at beginning.  */
	for (; length; --length)
	  *dest++ = *source++;
      }
  return (void *) d0;
}
#endif


enum {MUS_OSCIL, MUS_SUM_OF_COSINES, MUS_DELAY, MUS_COMB, MUS_NOTCH, MUS_ALL_PASS,
      MUS_TABLE_LOOKUP, MUS_SQUARE_WAVE, MUS_SAWTOOTH_WAVE, MUS_TRIANGLE_WAVE, MUS_PULSE_TRAIN,
      MUS_RAND, MUS_RAND_INTERP, MUS_ASYMMETRIC_FM, MUS_ONE_ZERO, MUS_ONE_POLE, MUS_TWO_ZERO, MUS_TWO_POLE, MUS_FORMANT,
      MUS_WAVESHAPE, MUS_SRC, MUS_GRANULATE, MUS_SINE_SUMMATION, MUS_WAVE_TRAIN, 
      MUS_FILTER, MUS_FIR_FILTER, MUS_IIR_FILTER, MUS_CONVOLVE, MUS_ENV, MUS_LOCSIG,
      MUS_FRAME, MUS_READIN, MUS_FILE_TO_SAMPLE, MUS_FILE_TO_FRAME,
      MUS_SAMPLE_TO_FILE, MUS_FRAME_TO_FILE, MUS_MIXER, MUS_PHASE_VOCODER,
      MUS_MOVING_AVERAGE, MUS_SUM_OF_SINES, MUS_SSB_AM, MUS_POLYSHAPE, MUS_FILTERED_COMB,
      MUS_MOVE_SOUND,
      MUS_INITIAL_GEN_TAG};

static char *interp_name[] = {"step", "linear", "sinusoidal", "all-pass", "lagrange", "bezier", "hermite"};


static int mus_class_tag = MUS_INITIAL_GEN_TAG;
int mus_make_class_tag(void) {return(mus_class_tag++);}


static Float sampling_rate = MUS_DEFAULT_SAMPLING_RATE;
static Float w_rate = (TWO_PI / MUS_DEFAULT_SAMPLING_RATE);


static Float float_equal_fudge_factor = 0.0000001;
Float mus_float_equal_fudge_factor(void) {return(float_equal_fudge_factor);}
Float mus_set_float_equal_fudge_factor(Float val) 
{
  Float prev; 
  prev = float_equal_fudge_factor; 
  float_equal_fudge_factor = val; 
  return(prev);
}


static int array_print_length = MUS_DEFAULT_ARRAY_PRINT_LENGTH;
int mus_array_print_length(void) {return(array_print_length);}
int mus_set_array_print_length(int val) 
{
  int prev; 
  prev = array_print_length; 
  if (val >= 0) array_print_length = val; 
  return(prev);
}


static int clm_file_buffer_size = MUS_DEFAULT_FILE_BUFFER_SIZE;
int mus_file_buffer_size(void) {return(clm_file_buffer_size);}
int mus_set_file_buffer_size(int size) 
{
  int prev; 
  prev = clm_file_buffer_size; 
  clm_file_buffer_size = size; 
  return(prev);
}

#define DESCRIBE_BUFFER_SIZE 2048
static char describe_buffer[DESCRIBE_BUFFER_SIZE];
#define STR_SIZE 128


#define clm_calloc(Num, Size, What) CALLOC(Num, Size)

static bool check_gen(mus_any *ptr, const char *name)
{
  if (ptr == NULL)
    {
      mus_error(MUS_NO_GEN, "null gen passed to %s", name);
      return(false);
    }
  return(true);
}

char *mus_name(mus_any *ptr) 
{
  if (ptr == NULL)
    return("null");
  return(ptr->core->name);
}


Float mus_radians_to_hz(Float rads) {return(rads / w_rate);}
Float mus_hz_to_radians(Float hz) {return(hz * w_rate);}

Float mus_degrees_to_radians(Float degree) {return(degree * TWO_PI / 360.0);}
Float mus_radians_to_degrees(Float rads) {return(rads * 360.0 / TWO_PI);}

Float mus_db_to_linear(Float x) {return(pow(10.0, x / 20.0));}
Float mus_linear_to_db(Float x) {if (x > 0.0) return(20.0 * log10(x)); return(-100.0);}

Float mus_srate(void) {return(sampling_rate);}
Float mus_set_srate(Float val) 
{
  Float prev; 
  prev = sampling_rate; 
  if (val > 0.0)
    {
      sampling_rate = val; 
      w_rate = (TWO_PI / sampling_rate); 
    }
  return(prev);
}

off_t mus_seconds_to_samples(Float secs) {return((off_t)(secs * sampling_rate));}
Float mus_samples_to_seconds(off_t samps) {return((Float)((double)samps / (double)sampling_rate));}


static char *float_array_to_string(Float *arr, int len, int loc)
{
  #define MAX_NUM_SIZE 32
  char *base, *str;
  int i, lim, k, size = 256;
  if (arr == NULL) 
    {
      str = (char *)CALLOC(4, sizeof(char));
      sprintf(str, "nil");
      return(str);
    }
  lim = (array_print_length + 4) * MAX_NUM_SIZE; /* 4 for possible bounds below */
  if (lim > size) size = lim;
  base = (char *)CALLOC(size, sizeof(char));
  str = (char *)CALLOC(STR_SIZE, sizeof(char));
  sprintf(base, "[");
  lim = len;
  if (lim > array_print_length) lim = array_print_length;
  k = loc;
  for (i = 0; i < lim - 1; i++)
    {
      mus_snprintf(str, STR_SIZE, "%.3f ", arr[k]);
      strcat(base, str);
      if ((int)(strlen(base) + MAX_NUM_SIZE) > size)
	{
	  base = (char *)REALLOC(base, size * 2 * sizeof(char));
	  base[size] = 0;
	  size *= 2;
	}
      k++;
      if (k >= len) k = 0;
    }
  mus_snprintf(str, STR_SIZE, "%.3f%s", arr[k], (len > lim) ? "..." : "]");
  strcat(base, str);
  if (len > lim)
    {
      /* print ranges */
      int min_loc = 0, max_loc = 0;
      Float min_val, max_val;
      min_val = arr[0];
      max_val = arr[0];
      for (i = 1; i < len; i++)
	{
	  if (arr[i] < min_val) {min_val = arr[i]; min_loc = i;}
	  if (arr[i] > max_val) {max_val = arr[i]; max_loc = i;}
	}
      mus_snprintf(str, STR_SIZE, "(%d: %.3f, %d: %.3f)]", min_loc, min_val, max_loc, max_val);
      strcat(base, str);
    }
  FREE(str);
  return(base);
}

static char *clm_array_to_string(mus_any **gens, int num_gens, char *name, char *indent)
{
  char *descr = NULL;
  if ((gens) && (num_gens > 0))
    {
      int i, len = 0;
      char **descrs;
      descrs = (char **)CALLOC(num_gens, sizeof(char *));
      for (i = 0; i < num_gens; i++)
	{
	  if (gens[i])
	    descrs[i] = mus_format("\n%s[%d]: %s", indent, i, mus_describe(gens[i]));
	  else descrs[i] = mus_format("\n%s[%d]: nil", indent, i);
	  len += strlen(descrs[i]);
	}
      len += (64 + strlen(name));
      descr = (char *)CALLOC(len, sizeof(char));
      mus_snprintf(descr, len, "%s[%d]:", name, num_gens);
      for (i = 0; i < num_gens; i++)
	{
	  strcat(descr, descrs[i]);
	  FREE(descrs[i]);
	}
      FREE(descrs);
    }
  else
    {
      descr = (char *)CALLOC(128, sizeof(char));
      mus_snprintf(descr, 128, "%s: nil", name);
    }
  return(descr);
}

static char *int_array_to_string(int *arr, int num_ints, char *name)
{
  #define MAX_INT_SIZE 32
  char *descr = NULL;
  if ((arr) && (num_ints > 0))
    {
      int i, len;
      char *intstr;
      len = num_ints * MAX_INT_SIZE + 64;
      descr = (char *)CALLOC(len, sizeof(char));
      intstr = (char *)CALLOC(MAX_INT_SIZE, sizeof(char));
      mus_snprintf(descr, len, "%s[%d]: (", name, num_ints);      
      for (i = 0; i < num_ints - 1; i++)
	{
	  mus_snprintf(intstr, MAX_INT_SIZE, "%d ", arr[i]);
	  strcat(descr, intstr);
	}
      mus_snprintf(intstr, MAX_INT_SIZE, "%d)", arr[num_ints - 1]);
      strcat(descr, intstr);
      FREE(intstr);
    }
  else
    {
      descr = (char *)CALLOC(128, sizeof(char));
      mus_snprintf(descr, 128, "%s: nil", name);
    }
  return(descr);
}


/* ---------------- generic functions ---------------- */

int mus_free(mus_any *gen)
{
  if ((check_gen(gen, "mus-free")) &&
      (gen->core->release))
    return((*(gen->core->release))(gen));
  return(mus_error(MUS_NO_FREE, "can't free %s", mus_name(gen)));
}

char *mus_describe(mus_any *gen)
{
  if (gen == NULL)
    return("null");
  if ((gen->core) && (gen->core->describe))
    return((*(gen->core->describe))(gen));
  else mus_error(MUS_NO_DESCRIBE, "can't describe %s", mus_name(gen));
  return(NULL);
}

bool mus_equalp(mus_any *p1, mus_any *p2)
{
  if ((p1) && (p2))
    {
      if ((p1->core)->equalp)
	return((*((p1->core)->equalp))(p1, p2));
      else return(p1 == p2);
    }
  return(true); /* (eq nil nil) */
}

void mus_reset(mus_any *gen)
{
  if ((check_gen(gen, S_mus_reset)) &&
      (gen->core->reset))
    (*(gen->core->reset))(gen);
  else mus_error(MUS_NO_RESET, "can't reset %s", mus_name(gen));
}

Float mus_frequency(mus_any *gen)
{
  if ((check_gen(gen, S_mus_frequency)) &&
      (gen->core->frequency))
    return((*(gen->core->frequency))(gen));
  return((Float)mus_error(MUS_NO_FREQUENCY, "can't get %s's frequency", mus_name(gen)));
}

Float mus_set_frequency(mus_any *gen, Float val)
{
  if ((check_gen(gen, S_setB S_mus_frequency)) &&
      (gen->core->set_frequency))
    return((*(gen->core->set_frequency))(gen, val));
  return((Float)mus_error(MUS_NO_FREQUENCY, "can't set %s's frequency", mus_name(gen)));
}

Float mus_phase(mus_any *gen)
{
  if ((check_gen(gen, S_mus_phase)) &&
      (gen->core->phase))
    return((*(gen->core->phase))(gen));
  return((Float)mus_error(MUS_NO_PHASE, "can't get %s's phase", mus_name(gen)));
}

Float mus_set_phase(mus_any *gen, Float val)
{
  if ((check_gen(gen, S_setB S_mus_phase)) &&
      (gen->core->set_phase))
    return((*(gen->core->set_phase))(gen, val));
  return((Float)mus_error(MUS_NO_PHASE, "can't set %s's phase", mus_name(gen)));
}

Float mus_scaler(mus_any *gen)
{
  if ((check_gen(gen, S_mus_scaler)) &&
      (gen->core->scaler))
    return((*(gen->core->scaler))(gen));
  return((Float)mus_error(MUS_NO_SCALER, "can't get %s's scaler", mus_name(gen)));
}

Float mus_set_scaler(mus_any *gen, Float val)
{
  if ((check_gen(gen, S_setB S_mus_scaler)) &&
      (gen->core->set_scaler))
    return((*(gen->core->set_scaler))(gen, val));
  return((Float)mus_error(MUS_NO_SCALER, "can't set %s's scaler", mus_name(gen)));
}

Float mus_offset(mus_any *gen)
{
  if ((check_gen(gen, S_mus_offset)) &&
      (gen->core->offset))
    return((*(gen->core->offset))(gen));
  return((Float)mus_error(MUS_NO_OFFSET, "can't get %s's offset", mus_name(gen)));
}

Float mus_set_offset(mus_any *gen, Float val)
{
  if ((check_gen(gen, S_setB S_mus_offset)) &&
      (gen->core->set_offset))
    return((*(gen->core->set_offset))(gen, val));
  return((Float)mus_error(MUS_NO_OFFSET, "can't set %s's offset", mus_name(gen)));
}

Float mus_width(mus_any *gen)
{
  if ((check_gen(gen, S_mus_width)) &&
      (gen->core->width))
    return((*(gen->core->width))(gen));
  return((Float)mus_error(MUS_NO_WIDTH, "can't get %s's width", mus_name(gen)));
}

Float mus_set_width(mus_any *gen, Float val)
{
  if ((check_gen(gen, S_setB S_mus_width)) &&
      (gen->core->set_width))
    return((*(gen->core->set_width))(gen, val));
  return((Float)mus_error(MUS_NO_WIDTH, "can't set %s's width", mus_name(gen)));
}

Float mus_increment(mus_any *gen)
{
  if ((check_gen(gen, S_mus_increment)) &&
      (gen->core->increment))
    return((*(gen->core->increment))(gen));
  return((Float)mus_error(MUS_NO_INCREMENT, "can't get %s's increment", mus_name(gen)));
}

Float mus_set_increment(mus_any *gen, Float val)
{
  if ((check_gen(gen, S_setB S_mus_increment)) &&
      (gen->core->set_increment))
    return((*(gen->core->set_increment))(gen, val));
  return((Float)mus_error(MUS_NO_INCREMENT, "can't set %s's increment", mus_name(gen)));
}

void *mus_environ(mus_any *gen)
{
  if (check_gen(gen, "mus-environ"))
    return((*(gen->core->closure))(gen));
  return(NULL);
}

void *mus_set_environ(mus_any *gen, void *e)
{
  if (check_gen(gen, S_setB "mus-environ")) 
    return((*(gen->core->set_closure))(gen, e));
  return(NULL);
}

Float mus_run(mus_any *gen, Float arg1, Float arg2)
{
  if ((check_gen(gen, "mus-run")) &&
      (gen->core->run))
    return((*(gen->core->run))(gen, arg1, arg2));
  return((Float)mus_error(MUS_NO_RUN, "can't run %s", mus_name(gen)));
}

off_t mus_length(mus_any *gen)
{
  if ((check_gen(gen, S_mus_length)) &&
      (gen->core->length))
    return((*(gen->core->length))(gen));
  return(mus_error(MUS_NO_LENGTH, "can't get %s's length", mus_name(gen)));
}

off_t mus_set_length(mus_any *gen, off_t len)
{
  if ((check_gen(gen, S_setB S_mus_length)) &&
      (gen->core->set_length))
    return((*(gen->core->set_length))(gen, len));
  return(mus_error(MUS_NO_LENGTH, "can't set %s's length", mus_name(gen)));
}

int mus_channels(mus_any *gen)
{
  if ((check_gen(gen, S_mus_channels)) &&
      (gen->core->channels))
    return((*(gen->core->channels))(gen));
  return(mus_error(MUS_NO_CHANNELS, "can't get %s's channels", mus_name(gen)));
}

int mus_channel(mus_any *gen)
{
  if ((check_gen(gen, S_mus_channel)) &&
      (gen->core->channel))
    return(((*gen->core->channel))(gen));
  return(mus_error(MUS_NO_CHANNEL, "can't get %s's channel", mus_name(gen)));
}

off_t mus_hop(mus_any *gen)
{
  if ((check_gen(gen, S_mus_hop)) &&
      (gen->core->hop))
    return((*(gen->core->hop))(gen));
  return(mus_error(MUS_NO_HOP, "can't get %s's hop value", mus_name(gen)));
}

off_t mus_set_hop(mus_any *gen, off_t len)
{
  if ((check_gen(gen, S_setB S_mus_hop)) &&
      (gen->core->set_hop))
    return((*(gen->core->set_hop))(gen, len));
  return(mus_error(MUS_NO_HOP, "can't set %s's hop value", mus_name(gen)));
}

off_t mus_ramp(mus_any *gen)
{
  if ((check_gen(gen, S_mus_ramp)) &&
      (gen->core->ramp))
    return((*(gen->core->ramp))(gen));
  return(mus_error(MUS_NO_RAMP, "can't get %s's ramp value", mus_name(gen)));
}

off_t mus_set_ramp(mus_any *gen, off_t len)
{
  if ((check_gen(gen, S_setB S_mus_ramp)) &&
      (gen->core->set_ramp))
    return((*(gen->core->set_ramp))(gen, len));
  return(mus_error(MUS_NO_RAMP, "can't set %s's ramp value", mus_name(gen)));
}

Float *mus_data(mus_any *gen)
{
  if ((check_gen(gen, S_mus_data)) &&
      (gen->core->data))
    return((*(gen->core->data))(gen));
  mus_error(MUS_NO_DATA, "can't get %s's data", mus_name(gen));
  return(NULL);
}

/* every case that implements the data or set data functions needs to include
 * a var-allocated flag, since all such memory has to be handled via vcts
 * in guile; a subsequent free by the enclosing object could leave a dangling
 * pointer in guile -- see clm2xen.c
 */

Float *mus_set_data(mus_any *gen, Float *new_data)
{
  if (check_gen(gen, S_setB S_mus_data))
    {
      if (gen->core->set_data)
	{
	  (*(gen->core->set_data))(gen, new_data);
	  return(new_data);
	}
      else mus_error(MUS_NO_DATA, "can't set %s's data", mus_name(gen));
    }
  return(new_data);
}

Float *mus_xcoeffs(mus_any *gen)
{
  if ((check_gen(gen, S_mus_xcoeffs)) &&
      (gen->core->xcoeffs))
    return((*(gen->core->xcoeffs))(gen));
  mus_error(MUS_NO_XCOEFFS, "can't get %s's xcoeffs", mus_name(gen));
  return(NULL);
}

Float *mus_ycoeffs(mus_any *gen)
{
  if ((check_gen(gen, S_mus_ycoeffs)) &&
      (gen->core->ycoeffs))
    return((*(gen->core->ycoeffs))(gen));
  mus_error(MUS_NO_YCOEFFS, "can't get %s's ycoeffs", mus_name(gen));
  return(NULL);
}

Float mus_xcoeff(mus_any *gen, int index)
{
  if ((check_gen(gen, S_mus_xcoeff)) &&
      (gen->core->xcoeff))
    return((*(gen->core->xcoeff))(gen, index));
  return(mus_error(MUS_NO_XCOEFF, "can't get %s's xcoeff[%d] value", mus_name(gen), index));
}

Float mus_set_xcoeff(mus_any *gen, int index, Float val)
{
  if ((check_gen(gen, S_setB S_mus_xcoeff)) &&
      (gen->core->set_xcoeff))
    return((*(gen->core->set_xcoeff))(gen, index, val));
  return(mus_error(MUS_NO_XCOEFF, "can't set %s's xcoeff[%d] value", mus_name(gen), index));
}

Float mus_ycoeff(mus_any *gen, int index)
{
  if ((check_gen(gen, S_mus_ycoeff)) &&
      (gen->core->ycoeff))
    return((*(gen->core->ycoeff))(gen, index));
  return(mus_error(MUS_NO_YCOEFF, "can't get %s's ycoeff[%d] value", mus_name(gen), index));
}

Float mus_set_ycoeff(mus_any *gen, int index, Float val)
{
  if ((check_gen(gen, S_setB S_mus_ycoeff)) &&
      (gen->core->set_ycoeff))
    return((*(gen->core->set_ycoeff))(gen, index, val));
  return(mus_error(MUS_NO_YCOEFF, "can't set %s's ycoeff[%d] value", mus_name(gen), index));
}

off_t mus_location(mus_any *gen)
{
  if ((check_gen(gen, S_mus_location)) &&
      (gen->core->location))
    return(((*gen->core->location))(gen));
  return((off_t)mus_error(MUS_NO_LOCATION, "can't get %s's location", mus_name(gen)));
}


/* ---------------- AM etc ---------------- */

Float mus_ring_modulate(Float sig1, Float sig2) {return(sig1 * sig2);}

Float mus_amplitude_modulate(Float carrier, Float sig1, Float sig2) {return(sig1 * (carrier + sig2));}

Float mus_contrast_enhancement(Float sig, Float index) {return(sin((sig * M_PI_2) + (index * sin(sig * TWO_PI))));}

void mus_clear_array(Float *arr, int size) {memset((void *)arr, 0, size * sizeof(Float));}

bool mus_arrays_are_equal(Float *arr1, Float *arr2, Float fudge, int len)
{
  int i;
  if (fudge == 0.0)
    {
      for (i = 0; i < len; i++)
	if (arr1[i] != arr2[i])
	  return(false);
    }
  else
    {
      for (i = 0; i < len; i++)
	if (fabs(arr1[i] - arr2[i]) > fudge)
	  return(false);
    }
  return(true);
}

static bool clm_arrays_are_equal(Float *arr1, Float *arr2, int len)
{
  return(mus_arrays_are_equal(arr1, arr2, float_equal_fudge_factor, len));
}


Float mus_dot_product(Float *data1, Float *data2, int size)
{
  int i;
  Float sum = 0.0;
  for (i = 0; i < size; i++) 
    sum += (data1[i] * data2[i]);
  return(sum);
}

#if HAVE_COMPLEX_TRIG
#if HAVE_FORTH 
  #include "xen.h" 
#endif 
complex double mus_edot_product(complex double freq, complex double *data, int size)
{
  int i;
  complex double sum = 0.0;
  for (i = 0; i < size; i++) 
    sum += (cexp(i * freq) * data[i]);
  return(sum);
}
#endif

Float mus_polynomial(Float *coeffs, Float x, int ncoeffs)
{
  Float sum;
  int i;
  if (ncoeffs <= 0) return(x);
  sum = coeffs[ncoeffs - 1];
  if (ncoeffs == 1) return(sum * x);
  for (i = ncoeffs - 2; i >= 0; i--) 
    sum = (sum * x) + coeffs[i];
  return(sum);
}

void mus_multiply_arrays(Float *data, Float *window, int len)
{
  int i;
  for (i = 0; i < len; i++) 
    data[i] *= window[i];
}

void mus_rectangular_to_polar(Float *rl, Float *im, int size) 
{
  int i; 
  for (i = 0; i < size; i++)
    {
      Float temp; /* apparently floating underflows in sqrt are bringing us to a halt */
      temp = rl[i] * rl[i] + im[i] * im[i];
      im[i] = -atan2(im[i], rl[i]); /* "-" here so that clockwise is positive? */
      if (temp < .00000001) 
	rl[i] = 0.0;
      else rl[i] = sqrt(temp);
    }
}

void mus_polar_to_rectangular(Float *rl, Float *im, int size) 
{
  int i; 
  for (i = 0; i < size; i++)
    {
      Float temp;
      temp = rl[i] * sin(-im[i]); /* minus to match sense of above */
      rl[i] *= cos(-im[i]);
      im[i] = temp;
    }
}

static Float *array_normalize(Float *table, int table_size)
{
  Float amp = 0.0;
  int i;
  for (i = 0; i < table_size; i++) 
    if (amp < (fabs(table[i]))) 
      amp = fabs(table[i]);
  if ((amp > 0.0) && (amp != 1.0))
    for (i = 0; i < table_size; i++) 
      table[i] /= amp;
  return(table);
}


/* ---------------- interpolation ---------------- */

Float mus_array_interp(Float *wave, Float phase, int size)
{
  /* changed 26-Sep-00 to be closer to mus.lisp */
  int int_part;
  Float frac_part;
  if ((phase < 0.0) || (phase > size))
    {
      /* 28-Mar-01 changed to fmod; I was hoping to avoid this... */
      phase = fmod((double)phase, (double)size);
      if (phase < 0.0) phase += size;
    }
  int_part = (int)floor(phase);
  frac_part = phase - int_part;
  if (int_part == size) int_part = 0;
  if (frac_part == 0.0) 
    return(wave[int_part]);
  else
    {
      int inx;
      inx = int_part + 1;
      if (inx >= size) inx = 0;
      return(wave[int_part] + (frac_part * (wave[inx] - wave[int_part])));
    }
}

static Float mus_array_all_pass_interp(Float *wave, Float phase, int size, Float yn1)
{
  /* from Perry Cook */
  int int_part, inx;
  Float frac_part;
  if ((phase < 0.0) || (phase > size))
    {
      phase = fmod((double)phase, (double)size);
      if (phase < 0.0) phase += size;
    }
  int_part = (int)floor(phase);
  frac_part = phase - int_part;
  if (int_part == size) int_part = 0;
  inx = int_part + 1;
  if (inx >= size) inx -= size;
  if (frac_part == 0.0) 
    return(wave[int_part] + wave[inx] - yn1);
  else return(wave[int_part] + ((1.0 - frac_part) / (1 + frac_part)) * (wave[inx] - yn1));
}

static Float mus_array_lagrange_interp(Float *wave, Float x, int size)
{
  /* Abramovitz and Stegun 25.2.11 -- everyone badmouths this poor formula */
  /* x assumed to be in the middle, between second and third vals */
  int x0, xp1, xm1;
  Float p, pp;
  if ((x < 0.0) || (x > size))
    {
      x = fmod((double)x, (double)size);
      if (x < 0.0) x += size;
    }
  x0 = (int)floor(x);
  p = x - x0;
  if (x0 >= size) x0 -= size;
  if (p == 0.0) return(wave[x0]);
  xp1 = x0 + 1;
  if (xp1 >= size) xp1 -= size;
  xm1 = x0 - 1;
  if (xm1 < 0) xm1 = size - 1;
  pp = p * p;
  return((wave[xm1] * 0.5 * (pp - p)) + 
	 (wave[x0] * (1.0 - pp)) + 
	 (wave[xp1] * 0.5 * (p + pp)));
}

static Float mus_array_hermite_interp(Float *wave, Float x, int size)
{
  /* from James McCartney */
  int x0, x1, x2, x3;
  Float p, c0, c1, c2, c3, y0, y1, y2, y3;
  if ((x < 0.0) || (x > size))
    {
      x = fmod((double)x, (double)size);
      if (x < 0.0) x += size;
    }
  x1 = (int)floor(x); 
  p = x - x1;
  if (x1 == size) x1 = 0;
  if (p == 0.0) return(wave[x1]);
  x2 = x1 + 1;
  if (x2 == size) x2 = 0;
  x3 = x2 + 1;
  if (x3 == size) x3 = 0;
  x0 = x1 - 1;
  if (x0 < 0) x0 = size - 1;
  y0 = wave[x0];
  y1 = wave[x1];
  y2 = wave[x2];
  y3 = wave[x3];
  c0 = y1;
  c1 = 0.5 * (y2 - y0);
  c3 = 1.5 * (y1 - y2) + 0.5 * (y3 - y0);
  c2 = y0 - y1 + c1 - c3;
  return(((c3 * p + c2) * p + c1) * p + c0);
}

static Float mus_array_bezier_interp(Float *wave, Float x, int size)
{
  int x0, x1, x2, x3;
  Float p, y0, y1, y2, y3, ay, by, cy;
  if ((x < 0.0) || (x > size))
    {
      x = fmod((double)x, (double)size);
      if (x < 0.0) x += size;
    }
  x1 = (int)floor(x); 
  p = ((x - x1) + 1.0) / 3.0;
  if (x1 == size) x1 = 0;
  x2 = x1 + 1;
  if (x2 == size) x2 = 0;
  x3 = x2 + 1;
  if (x3 == size) x3 = 0;
  x0 = x1 - 1;
  if (x0 < 0) x0 = size - 1;
  y0 = wave[x0];
  y1 = wave[x1];
  y2 = wave[x2];
  y3 = wave[x3];
  cy = 3 * (y1 - y0);
  by = 3 * (y2 - y1) - cy;
  ay = y3 - y0 - cy - by;
  return(y0 + p * (cy + (p * (by + (p * ay)))));
}

Float mus_interpolate(mus_interp_t type, Float x, Float *table, int table_size, Float y)
{
  switch (type)
    {
    case MUS_INTERP_NONE:
      {
	int x0;
	x0 = ((int)x) % table_size;
	if (x0 < 0) x0 += table_size;
	return(table[x0]);
      }
      break;
    case MUS_INTERP_LAGRANGE:
      return(mus_array_lagrange_interp(table, x, table_size));
      break;
    case MUS_INTERP_HERMITE:
      return(mus_array_hermite_interp(table, x, table_size));
      break;
    case MUS_INTERP_LINEAR:
      return(mus_array_interp(table, x, table_size));
      break;
    case MUS_INTERP_ALL_PASS:
      return(mus_array_all_pass_interp(table, x, table_size, y));
      break;
    case MUS_INTERP_BEZIER:
      return(mus_array_bezier_interp(table, x, table_size));
      break;
    default:
      mus_error(MUS_ARG_OUT_OF_RANGE, "unknown interpolation type: %d", type);
      break;
    }
  return(0.0);
}



/* ---------------- oscil ---------------- */

typedef struct {
  mus_any_class *core;
  double phase, freq;
} osc;

Float mus_oscil(mus_any *ptr, Float fm, Float pm)
{
  Float result;
  osc *gen = (osc *)ptr;
  result = sin(gen->phase + pm);
  gen->phase += (gen->freq + fm);
  return(result);
}

Float mus_oscil_0(mus_any *ptr)
{
  Float result;
  osc *gen = (osc *)ptr;
  result = sin(gen->phase);
  gen->phase += gen->freq;
  return(result);
}

Float mus_oscil_1(mus_any *ptr, Float fm)
{
  Float result;
  osc *gen = (osc *)ptr;
  result = sin(gen->phase);
  gen->phase += (gen->freq + fm);
  return(result);
}

Float mus_sine_bank(Float *amps, Float *phases, int size)
{
  int i;
  Float sum = 0.0;
  for (i = 0; i < size; i++) 
    if (amps[i] != 0.0)
      sum += (amps[i] * sin(phases[i]));
  return(sum);
}

bool mus_oscil_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_OSCIL));}
static int free_oscil(mus_any *ptr) {if (ptr) FREE(ptr); return(0);}

static Float oscil_freq(mus_any *ptr) {return(mus_radians_to_hz(((osc *)ptr)->freq));}
static Float set_oscil_freq(mus_any *ptr, Float val) {((osc *)ptr)->freq = mus_hz_to_radians(val); return(val);}

static Float oscil_phase(mus_any *ptr) {return(fmod(((osc *)ptr)->phase, TWO_PI));}
static Float set_oscil_phase(mus_any *ptr, Float val) {((osc *)ptr)->phase = val; return(val);}

static off_t oscil_cosines(mus_any *ptr) {return(1);}
static void oscil_reset(mus_any *ptr) {((osc *)ptr)->phase = 0.0;}

static bool oscil_equalp(mus_any *p1, mus_any *p2)
{
  return((p1 == p2) ||
	 ((mus_oscil_p((mus_any *)p1)) && 
	  (mus_oscil_p((mus_any *)p2)) &&
	  ((((osc *)p1)->freq) == (((osc *)p2)->freq)) &&
	  ((((osc *)p1)->phase) == (((osc *)p2)->phase))));
}

static char *describe_oscil(mus_any *ptr)
{
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, S_oscil " freq: %.3fHz, phase: %.3f", 
	       mus_frequency(ptr), 
	       mus_phase(ptr));
  return(describe_buffer);
}

static mus_any_class OSCIL_CLASS = {
  MUS_OSCIL,
  S_oscil,
  &free_oscil,
  &describe_oscil,
  &oscil_equalp,
  0, 0, 0, 0, /* data length */
  &oscil_freq,
  &set_oscil_freq,
  &oscil_phase,
  &set_oscil_phase,
  0, 0, 0, 0,
  &mus_oscil,
  MUS_NOT_SPECIAL, 
  NULL,
  0, 
  0, 0, 0, 0, 0, 0, &oscil_cosines, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &oscil_reset,
  0
};

mus_any *mus_make_oscil(Float freq, Float phase)
{
  osc *gen;
  gen = (osc *)clm_calloc(1, sizeof(osc), S_make_oscil);
  gen->core = &OSCIL_CLASS;
  gen->freq = mus_hz_to_radians(freq);
  gen->phase = phase;
  return((mus_any *)gen);
}

/* decided against feedback-oscil (as in cellon) because it's not clear how to handle the index,
 *   and there are many options for the filtering -- since this part of the signal path
 *   is not hidden, there's no reason to bring it out explicitly (as in filtered-comb)
 */



/* ---------------- sum-of-cosines ---------------- */

typedef struct {
  mus_any_class *core;
  int cosines;
  Float scaler, cos5;
  double phase, freq;
} cosp;

Float mus_sum_of_cosines(mus_any *ptr, Float fm)
{
  /* changed 25-Apr-04: use less stupid formula */
  /*   (/ (- (/ (sin (* (+ n 0.5) angle)) (* 2 (sin (* 0.5 angle)))) 0.5) n) */
  Float val, den;
  cosp *gen = (cosp *)ptr;
  den = sin(gen->phase * 0.5);
  if (den == 0.0)
    val = 1.0;
  else 
    {
      val = (gen->scaler * (((sin(gen->phase * gen->cos5)) / (2.0 * den)) - 0.5));
      if (val > 1.0) val = 1.0;
    }
  gen->phase += (gen->freq + fm);
  return(val);
}

bool mus_sum_of_cosines_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_SUM_OF_COSINES));}

static int free_sum_of_cosines(mus_any *ptr) {if (ptr) FREE(ptr); return(0);}
static void sum_of_cosines_reset(mus_any *ptr) {((cosp *)ptr)->phase = 0.0;}

static Float sum_of_cosines_freq(mus_any *ptr) {return(mus_radians_to_hz(((cosp *)ptr)->freq));}
static Float set_sum_of_cosines_freq(mus_any *ptr, Float val) {((cosp *)ptr)->freq = mus_hz_to_radians(val); return(val);}

static Float sum_of_cosines_phase(mus_any *ptr) {return(fmod(((cosp *)ptr)->phase, TWO_PI));}
static Float set_sum_of_cosines_phase(mus_any *ptr, Float val) {((cosp *)ptr)->phase = val; return(val);}

static Float sum_of_cosines_scaler(mus_any *ptr) {return(((cosp *)ptr)->scaler);}
static Float set_sum_of_cosines_scaler(mus_any *ptr, Float val) {((cosp *)ptr)->scaler = val; return(val);}

static off_t sum_of_cosines_cosines(mus_any *ptr) {return(((cosp *)ptr)->cosines);}
static off_t set_sum_of_cosines_cosines(mus_any *ptr, off_t val) 
{
  cosp *gen = (cosp *)ptr;
  if (val > 0)
    {
      gen->cosines = (int)val;
      gen->cos5 = val + 0.5;
      gen->scaler = 1.0 / (Float)val; 
    }
  return(val);
}

static Float run_sum_of_cosines(mus_any *ptr, Float fm, Float unused) {return(mus_sum_of_cosines(ptr, fm));}

static bool sum_of_cosines_equalp(mus_any *p1, mus_any *p2)
{
  return((p1 == p2) ||
	 ((mus_sum_of_cosines_p((mus_any *)p1)) && (mus_sum_of_cosines_p((mus_any *)p2)) &&
	  ((((cosp *)p1)->freq) == (((cosp *)p2)->freq)) &&
	  ((((cosp *)p1)->phase) == (((cosp *)p2)->phase)) &&
	  ((((cosp *)p1)->cosines) == (((cosp *)p2)->cosines)) &&
	  ((((cosp *)p1)->scaler) == (((cosp *)p2)->scaler))));
}

static char *describe_sum_of_cosines(mus_any *ptr)
{
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, S_sum_of_cosines " freq: %.3fHz, phase: %.3f, cosines: %d",
	       mus_frequency(ptr),
	       mus_phase(ptr),
	       (int)mus_order(ptr));
  return(describe_buffer);
}

static mus_any_class SUM_OF_COSINES_CLASS = {
  MUS_SUM_OF_COSINES,
  S_sum_of_cosines,
  &free_sum_of_cosines,
  &describe_sum_of_cosines,
  &sum_of_cosines_equalp,
  0, 0, /* data */
  &sum_of_cosines_cosines,
  &set_sum_of_cosines_cosines,
  &sum_of_cosines_freq,
  &set_sum_of_cosines_freq,
  &sum_of_cosines_phase,
  &set_sum_of_cosines_phase,
  &sum_of_cosines_scaler,
  &set_sum_of_cosines_scaler,
  0, 0,
  &run_sum_of_cosines,
  MUS_NOT_SPECIAL, 
  NULL,
  0,
  0, 0, 0, 0, 0, 0, 
  &sum_of_cosines_cosines, &set_sum_of_cosines_cosines, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &sum_of_cosines_reset,
  0
};

mus_any *mus_make_sum_of_cosines(int cosines, Float freq, Float phase)
{
  cosp *gen;
  gen = (cosp *)clm_calloc(1, sizeof(cosp), S_make_sum_of_cosines);
  gen->core = &SUM_OF_COSINES_CLASS;
  if (cosines == 0) cosines = 1;
  gen->scaler = 1.0 / (Float)cosines;
  gen->cosines = cosines;
  gen->cos5 = cosines + 0.5;
  gen->freq = mus_hz_to_radians(freq);
  gen->phase = phase;
  return((mus_any *)gen);
}


/* ---------------- sum-of-sines ---------------- */

static bool sum_of_sines_equalp(mus_any *p1, mus_any *p2)
{
  return((p1 == p2) ||
	 ((mus_sum_of_sines_p((mus_any *)p1)) && (mus_sum_of_sines_p((mus_any *)p2)) &&
	  ((((cosp *)p1)->freq) == (((cosp *)p2)->freq)) &&
	  ((((cosp *)p1)->phase) == (((cosp *)p2)->phase)) &&
	  ((((cosp *)p1)->cosines) == (((cosp *)p2)->cosines)) &&
	  ((((cosp *)p1)->scaler) == (((cosp *)p2)->scaler))));
}

static char *describe_sum_of_sines(mus_any *ptr)
{
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, S_sum_of_sines " freq: %.3fHz, phase: %.3f, sines: %d",
	       mus_frequency(ptr),
	       mus_phase(ptr),
	       (int)mus_order(ptr));
  return(describe_buffer);
}

bool mus_sum_of_sines_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_SUM_OF_SINES));}


static Float sum_of_sines_maxamps[] = {1.0, 1.0, 1.761, 2.5, 3.24, 3.97, 4.7, 5.42, 6.15, 6.88,
				       7.6, 8.33, 9.05, 9.78, 10.51, 11.23, 11.96, 12.68, 13.41, 14.13};

static Float sum_of_sines_50 = .743;
static Float sum_of_sines_100 = .733;

static Float sum_of_sines_scaler(int sines)
{
  /* doesn't this normalization assume initial-phase = 0? */
  if (sines < 20)
    return(1.0 / sum_of_sines_maxamps[sines]);
  if (sines < 50)
    return(1.0 / (sines * sum_of_sines_50));
  return(1.0 / (sines * sum_of_sines_100));
}

static off_t set_sum_of_sines_sines(mus_any *ptr, off_t val) 
{
  cosp *gen = (cosp *)ptr;
  gen->cosines = (int)val;
  gen->cos5 = val + 1.0;
  gen->scaler = sum_of_sines_scaler((int)val);
  return(val);
}

Float mus_sum_of_sines(mus_any *ptr, Float fm)
{
  /* (let* ((a2 (* angle 0.5))
	    (den (sin a2)))
       (if (= den 0.0)
	   0.0
	   (/ (* (sin (* n a2)) (sin (* (1+ n) a2))) den)))
  */
  Float val, den, a2;
  cosp *gen = (cosp *)ptr;
  a2 = gen->phase * 0.5;
  den = sin(a2);
  if (den == 0.0)
    val = 0.0;
  else val = gen->scaler * sin(gen->cosines * a2) * sin(a2 * gen->cos5) / den;
  gen->phase += (gen->freq + fm);
  return(val);
}

static Float run_sum_of_sines(mus_any *ptr, Float fm, Float unused) {return(mus_sum_of_sines(ptr, fm));}

static mus_any_class SUM_OF_SINES_CLASS = {
  MUS_SUM_OF_SINES,
  S_sum_of_sines,
  &free_sum_of_cosines,
  &describe_sum_of_sines,
  &sum_of_sines_equalp,
  0, 0, /* data */
  &sum_of_cosines_cosines,
  &set_sum_of_sines_sines,
  &sum_of_cosines_freq,
  &set_sum_of_cosines_freq,
  &sum_of_cosines_phase,
  &set_sum_of_cosines_phase,
  &sum_of_cosines_scaler,
  &set_sum_of_cosines_scaler,
  0, 0,
  &run_sum_of_sines,
  MUS_NOT_SPECIAL, 
  NULL,
  0,
  0, 0, 0, 0, 0, 0, 
  &sum_of_cosines_cosines, &set_sum_of_sines_sines, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &sum_of_cosines_reset,
  0
};

mus_any *mus_make_sum_of_sines(int sines, Float freq, Float phase)
{
  cosp *gen;
  gen = (cosp *)mus_make_sum_of_cosines(sines, freq, phase);
  gen->core = &SUM_OF_SINES_CLASS;
  gen->scaler = sum_of_sines_scaler(sines);
  gen->cos5 = gen->cosines + 1.0;
  return((mus_any *)gen);
}


/* ---------------- asymmetric-fm ---------------- */

typedef struct {
  mus_any_class *core;
  Float r;
  double freq, phase;
  Float ratio;
  Float cosr;
  Float sinr;
} asyfm;

static int free_asymmetric_fm(mus_any *ptr) {if (ptr) FREE(ptr); return(0);}
static void asyfm_reset(mus_any *ptr) {((asyfm *)ptr)->phase = 0.0;}

static Float asyfm_freq(mus_any *ptr) {return(mus_radians_to_hz(((asyfm *)ptr)->freq));}
static Float set_asyfm_freq(mus_any *ptr, Float val) {((asyfm *)ptr)->freq = mus_hz_to_radians(val); return(val);}

static Float asyfm_phase(mus_any *ptr) {return(fmod(((asyfm *)ptr)->phase, TWO_PI));}
static Float set_asyfm_phase(mus_any *ptr, Float val) {((asyfm *)ptr)->phase = val; return(val);}

static Float asyfm_ratio(mus_any *ptr) {return(((asyfm *)ptr)->ratio);}
static Float asyfm_r(mus_any *ptr) {return(((asyfm *)ptr)->r);}
static Float set_asyfm_r(mus_any *ptr, Float val) 
{
  asyfm *gen = (asyfm *)ptr;
  if (val != 0.0)
    {
      gen->r = val; 
      gen->cosr = 0.5 * (val - (1.0 / val));
      gen->sinr = 0.5 * (val + (1.0 / val));
    }
  return(val);
}

static bool asyfm_equalp(mus_any *p1, mus_any *p2)
{
  return((p1 == p2) ||
	 (((p1->core)->type == (p2->core)->type) &&
	  ((((asyfm *)p1)->freq) == (((asyfm *)p2)->freq)) && 
	  ((((asyfm *)p1)->phase) == (((asyfm *)p2)->phase)) &&
	  ((((asyfm *)p1)->ratio) == (((asyfm *)p2)->ratio)) &&
	  ((((asyfm *)p1)->r) == (((asyfm *)p2)->r))));
}

static char *describe_asyfm(mus_any *ptr)
{
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, S_asymmetric_fm " freq: %.3fHz, phase: %.3f, ratio: %.3f, r: %.3f",
	       mus_frequency(ptr),
	       mus_phase(ptr),
	       ((asyfm *)ptr)->ratio, 
	       asyfm_r(ptr));
  return(describe_buffer);
}

bool mus_asymmetric_fm_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_ASYMMETRIC_FM));}

Float mus_asymmetric_fm(mus_any *ptr, Float index, Float fm)
{
  asyfm *gen = (asyfm *)ptr;
  Float result, mth;
  mth = gen->ratio * gen->phase;
  result = exp(index * gen->cosr * cos(mth)) * sin(gen->phase + index * gen->sinr * sin(mth));
  /* second index factor added 4-Mar-02 */
  gen->phase += (gen->freq + fm);
  return(result);
}

Float mus_asymmetric_fm_1(mus_any *ptr, Float index)
{
  asyfm *gen = (asyfm *)ptr;
  Float result, mth;
  mth = gen->ratio * gen->phase;
  result = exp(index * gen->cosr * cos(mth)) * sin(gen->phase + index * gen->sinr * sin(mth));
  /* second index factor added 4-Mar-02 */
  gen->phase += gen->freq;
  return(result);
}

Float mus_asymmetric_fm_0(mus_any *ptr)
{
  asyfm *gen = (asyfm *)ptr;
  Float result;
  result = sin(gen->phase);
  gen->phase += gen->freq;
  return(result);
}

static mus_any_class ASYMMETRIC_FM_CLASS = {
  MUS_ASYMMETRIC_FM,
  S_asymmetric_fm,
  &free_asymmetric_fm,
  &describe_asyfm,
  &asyfm_equalp,
  0, 0, 0, 0,
  &asyfm_freq,
  &set_asyfm_freq,
  &asyfm_phase,
  &set_asyfm_phase,
  &asyfm_r,
  &set_asyfm_r,
  &asyfm_ratio, 0,
  &mus_asymmetric_fm,
  MUS_NOT_SPECIAL, 
  NULL, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &asyfm_reset,
  0
};

mus_any *mus_make_asymmetric_fm(Float freq, Float phase, Float r, Float ratio) /* r default 1.0, ratio 1.0 */
{
 asyfm *gen = NULL;
 if (r == 0.0)
   mus_error(MUS_ARG_OUT_OF_RANGE, "r can't be 0.0");
 else
   {
     gen = (asyfm *)clm_calloc(1, sizeof(asyfm), S_make_asymmetric_fm);
     gen->core = &ASYMMETRIC_FM_CLASS;
     gen->freq = mus_hz_to_radians(freq);
     gen->phase = phase;
     gen->r = r;
     gen->ratio = ratio;
     gen->cosr = 0.5 * (r - (1.0 / r)); /* 0.5 factor for I/2 */
     gen->sinr = 0.5 * (r + (1.0 / r));
   }
 return((mus_any *)gen);
}



/*---------------- sine-summation ---------------- */

typedef struct {
  mus_any_class *core;
  double freq, phase;
  Float a, b, an, a2;
  int n;
} sss;

static int free_sss(mus_any *ptr) {if (ptr) FREE(ptr); return(0);}
static void sss_reset(mus_any *ptr) {((sss *)ptr)->phase = 0.0;}

static Float sss_freq(mus_any *ptr) {return(mus_radians_to_hz(((sss *)ptr)->freq));}
static Float set_sss_freq(mus_any *ptr, Float val) {((sss *)ptr)->freq = mus_hz_to_radians(val); return(val);}

static Float sss_phase(mus_any *ptr) {return(fmod(((sss *)ptr)->phase, TWO_PI));}
static Float set_sss_phase(mus_any *ptr, Float val) {((sss *)ptr)->phase = val; return(val);}

static off_t sss_n(mus_any *ptr) {return((off_t)(((sss *)ptr)->n));}
static Float sss_b(mus_any *ptr) {return(((sss *)ptr)->b);}

static Float sss_a(mus_any *ptr) {return(((sss *)ptr)->a);}
static Float set_sss_a(mus_any *ptr, Float val) 
{
  sss *gen = (sss *)ptr;
  gen->a = val;
  gen->a2 = 1.0 + val * val;
  gen->an = pow(val, gen->n + 1);
  return(val);
}

static bool sss_equalp(mus_any *p1, mus_any *p2)
{
  sss *g1 = (sss *)p1;
  sss *g2 = (sss *)p2;
  return((p1 == p2) ||
	 (((g1->core)->type == (g2->core)->type) &&
	  (g1->freq == g2->freq) &&
	  (g1->phase == g2->phase) &&
	  (g1->n == g2->n) &&
	  (g1->a == g2->a) &&
	  (g1->b == g2->b)));
}

bool mus_sine_summation_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_SINE_SUMMATION));}

static char *describe_sss(mus_any *ptr)
{
  sss *gen = (sss *)ptr;
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, S_sine_summation ": frequency: %.3f, phase: %.3f, n: %d, a: %.3f, ratio: %.3f",
	       mus_frequency(ptr),
	       mus_phase(ptr),
	       gen->n, 
	       sss_a(ptr),
	       gen->b);
  return(describe_buffer);
}

Float mus_sine_summation(mus_any *ptr, Float fm)
{
  sss *gen = (sss *)ptr;
  Float B, thB, result, divisor;
  B = gen->b * gen->phase;
  thB = gen->phase - B;
  divisor = (gen->a2 - (2 * gen->a * cos(B)));
  if (divisor == 0.0) 
    result = 0.0;
  /* if a=1.0, the formula given by Moorer is extremely unstable anywhere near phase=0.0 
   *   (map-channel (let ((gen (make-sine-summation 100.0 0.0 1 1 1.0))) (lambda (y) (sine-summation gen))))
   * or even worse:
   *   (map-channel (let ((gen (make-sine-summation 100.0 0.0 0 1 1.0))) (lambda (y) (sine-summation gen))))
   * which should be a sine wave! 
   * I wonder if that formula is incorrect...
   */
  else result = (sin(gen->phase) - (gen->a * sin(thB)) - 
		 (gen->an * (sin(gen->phase + (B * (gen->n + 1))) - 
			     (gen->a * sin(gen->phase + (B * gen->n)))))) / divisor;
  gen->phase += (gen->freq + fm);
  /* gen->phase = fmod(gen->phase, TWO_PI); */
  return(result);
}

static Float run_sine_summation(mus_any *ptr, Float fm, Float unused) {return(mus_sine_summation(ptr, fm));}

static mus_any_class SINE_SUMMATION_CLASS = {
  MUS_SINE_SUMMATION,
  S_sine_summation,
  &free_sss,
  &describe_sss,
  &sss_equalp,
  0, 0, 0, 0,
  &sss_freq,
  &set_sss_freq,
  &sss_phase,
  &set_sss_phase,
  &sss_a,
  &set_sss_a,
  &sss_b, 0,
  &run_sine_summation,
  MUS_NOT_SPECIAL, 
  NULL, 0,
  0, 0, 0, 0, 0, 0, 
  &sss_n, /* mus-cosines */
  0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &sss_reset,
  0
};

mus_any *mus_make_sine_summation(Float frequency, Float phase, int n, Float a, Float b_ratio)
{
  sss *gen;
  gen = (sss *)clm_calloc(1, sizeof(sss), S_make_sine_summation);
  gen->core = &SINE_SUMMATION_CLASS;
  gen->freq = mus_hz_to_radians(frequency);
  gen->phase = phase;
  gen->an = pow(a, n + 1);
  gen->a2 = 1.0 + a * a;
  gen->a = a;
  gen->n = n;
  gen->b = b_ratio;
  return((mus_any *)gen);
}


/* ---------------- table lookup ---------------- */

typedef struct {
  mus_any_class *core;
  Float freq;
  Float internal_mag;
  Float phase;
  Float *table;
  int table_size;
  mus_interp_t type;
  bool table_allocated;
  Float yn1;
} tbl;

Float *mus_partials_to_wave(Float *partial_data, int partials, Float *table, int table_size, bool normalize)
{
  int partial, k;
  mus_clear_array(table, table_size);
  for (partial = 0, k = 1; partial < partials; partial++, k += 2)
    {
      Float amp;
      amp = partial_data[k];
      if (amp != 0.0)
	{
	  int i;
	  Float freq, angle;
	  freq = (partial_data[partial * 2] * TWO_PI) / (Float)table_size;
	  for (i = 0, angle = 0.0; i < table_size; i++, angle += freq) 
	    table[i] += amp * sin(angle);
	}
    }
  if (normalize) 
    return(array_normalize(table, table_size));
  return(table);
}

Float *mus_phase_partials_to_wave(Float *partial_data, int partials, Float *table, int table_size, bool normalize)
{
  int partial, k, n;
  mus_clear_array(table, table_size);
  for (partial = 0, k = 1, n = 2; partial < partials; partial++, k += 3, n += 3)
    {
      Float amp;
      amp = partial_data[k];
      if (amp != 0.0)
	{
	  int i;
	  Float freq, angle;
	  freq = (partial_data[partial * 3] * TWO_PI) / (Float)table_size;
	  for (i = 0, angle = partial_data[n]; i < table_size; i++, angle += freq) 
	    table[i] += amp * sin(angle);
	}
    }
  if (normalize) 
    return(array_normalize(table, table_size));
  return(table);
}

Float mus_table_lookup(mus_any *ptr, Float fm)
{
  tbl *gen = (tbl *)ptr;
  gen->yn1 = mus_interpolate(gen->type, gen->phase, gen->table, gen->table_size, gen->yn1);
  gen->phase += (gen->freq + (fm * gen->internal_mag));
  if ((gen->phase >= gen->table_size) || (gen->phase < 0.0))
    {
      gen->phase = fmod(gen->phase, (double)(gen->table_size));
      if (gen->phase < 0.0) gen->phase += gen->table_size;
    }
  return(gen->yn1);
}

Float mus_table_lookup_1(mus_any *ptr)
{
  tbl *gen = (tbl *)ptr;
  gen->yn1 = mus_interpolate(gen->type, gen->phase, gen->table, gen->table_size, gen->yn1);
  gen->phase += gen->freq;
  if ((gen->phase >= gen->table_size) || (gen->phase < 0.0))
    {
      gen->phase = fmod(gen->phase, (double)(gen->table_size));
      if (gen->phase < 0.0) gen->phase += gen->table_size;
    }
  return(gen->yn1);
}

static Float run_table_lookup(mus_any *ptr, Float fm, Float unused) {return(mus_table_lookup(ptr, fm));}
bool mus_table_lookup_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_TABLE_LOOKUP));}
static off_t table_lookup_length(mus_any *ptr) {return(((tbl *)ptr)->table_size);}
static Float *table_lookup_data(mus_any *ptr) {return(((tbl *)ptr)->table);}

static Float table_lookup_freq(mus_any *ptr) {return((((tbl *)ptr)->freq * sampling_rate) / (Float)(((tbl *)ptr)->table_size));}
static Float set_table_lookup_freq(mus_any *ptr, Float val) {((tbl *)ptr)->freq = (val * ((tbl *)ptr)->table_size) / sampling_rate; return(val);}

static Float table_lookup_phase(mus_any *ptr) {return(fmod(((TWO_PI * ((tbl *)ptr)->phase) / ((tbl *)ptr)->table_size), TWO_PI));}
static Float set_table_lookup_phase(mus_any *ptr, Float val) {((tbl *)ptr)->phase = (val * ((tbl *)ptr)->table_size) / TWO_PI; return(val);}

static int table_lookup_interp_type(mus_any *ptr) {return((int)(((tbl *)ptr)->type));}
static void table_lookup_reset(mus_any *ptr) {((tbl *)ptr)->phase = 0.0;}

static char *describe_table_lookup(mus_any *ptr)
{
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, S_table_lookup ": freq: %.3fHz, phase: %.3f, length: %d, interp: %s",
	       mus_frequency(ptr),
	       mus_phase(ptr),
	       (int)mus_length(ptr),
	       interp_name[table_lookup_interp_type(ptr)]);
  return(describe_buffer);
}

static bool table_lookup_equalp(mus_any *p1, mus_any *p2)
{
  tbl *t1 = (tbl *)p1;
  tbl *t2 = (tbl *)p2;
  if (p1 == p2) return(true);
  return((t1) && (t2) &&
	 (t1->core->type == t2->core->type) &&
	 (t1->table_size == t2->table_size) &&
	 (t1->freq == t2->freq) &&
	 (t1->phase == t2->phase) &&
	 (t1->type == t2->type) &&
	 (t1->internal_mag == t2->internal_mag) &&
	 (clm_arrays_are_equal(t1->table, t2->table, t1->table_size)));
}

static int free_table_lookup(mus_any *ptr) 
{
  tbl *gen = (tbl *)ptr;
  if (gen)
    {
      if ((gen->table) && (gen->table_allocated)) FREE(gen->table); 
      FREE(gen); 
    }
  return(0);
}

static Float *table_set_data(mus_any *ptr, Float *val) 
{
  tbl *gen = (tbl *)ptr;
  if (gen->table_allocated) {FREE(gen->table); gen->table_allocated = false;}
  gen->table = val; 
  return(val);
}

static mus_any_class TABLE_LOOKUP_CLASS = {
  MUS_TABLE_LOOKUP,
  S_table_lookup,
  &free_table_lookup,
  &describe_table_lookup,
  &table_lookup_equalp,
  &table_lookup_data,
  &table_set_data,
  &table_lookup_length,
  0,
  &table_lookup_freq,
  &set_table_lookup_freq,
  &table_lookup_phase,
  &set_table_lookup_phase,
  0, 0,
  0, 0,
  &run_table_lookup,
  MUS_NOT_SPECIAL, 
  NULL,
  &table_lookup_interp_type,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &table_lookup_reset,
  0
};

mus_any *mus_make_table_lookup(Float freq, Float phase, Float *table, int table_size, mus_interp_t type)
{
  tbl *gen;
  gen = (tbl *)clm_calloc(1, sizeof(tbl), S_make_table_lookup);
  gen->core = &TABLE_LOOKUP_CLASS;
  gen->table_size = table_size;
  gen->internal_mag = (Float)table_size / TWO_PI;
  gen->freq = (freq * table_size) / sampling_rate;
  gen->phase = (fmod(phase, TWO_PI) * table_size) / TWO_PI;
  gen->type = type;
  gen->yn1 = 0.0;
  if (table)
    {
      gen->table = table;
      gen->table_allocated = false;
    }
  else
    {
      gen->table = (Float *)clm_calloc(table_size, sizeof(Float), "table lookup table");
      gen->table_allocated = true;
    }
  return((mus_any *)gen);
}


/* ---------------- waveshape ---------------- */

typedef struct {
  mus_any_class *core;
  mus_any *o;
  Float *table;
  int table_size;
  Float offset;
  bool table_allocated;
} ws;

static int free_ws(mus_any *pt) 
{
  ws *ptr = (ws *)pt;
  if (ptr) 
    {
      mus_free(ptr->o);
      if ((ptr->table) && (ptr->table_allocated)) FREE(ptr->table);
      FREE(ptr); 
    }
  return(0);
}

static Float ws_freq(mus_any *ptr) {return(mus_frequency(((ws *)ptr)->o));}
static Float set_ws_freq(mus_any *ptr, Float val) {return(mus_set_frequency(((ws *)ptr)->o, val));}

static Float ws_phase(mus_any *ptr) {return(mus_phase(((ws *)ptr)->o));}
static Float set_ws_phase(mus_any *ptr, Float val) {return(mus_set_phase(((ws *)ptr)->o, val));}

static off_t ws_size(mus_any *ptr) {return(((ws *)ptr)->table_size);}
static off_t set_ws_size(mus_any *ptr, off_t val) {((ws *)ptr)->table_size = (int)val; return(val);}

static Float *ws_data(mus_any *ptr) {return(((ws *)ptr)->table);}

static void ws_reset(mus_any *ptr)
{
  ws *gen = (ws *)ptr;
  oscil_reset(gen->o);
  mus_clear_array(gen->table, gen->table_size);
}

static bool ws_equalp(mus_any *p1, mus_any *p2)
{
  ws *w1 = (ws *)p1;
  ws *w2 = (ws *)p2;
  if (p1 == p2) return(true);
  return((w1) && (w2) &&
	 (w1->core->type == w2->core->type) &&
	 (mus_equalp(w1->o, w2->o)) &&
	 (w1->table_size == w2->table_size) &&
	 (w1->offset == w2->offset) &&
	 (clm_arrays_are_equal(w1->table, w2->table, w1->table_size)));
}

static Float *set_ws_data(mus_any *ptr, Float *val) 
{
  ws *gen = (ws *)ptr;
  if (gen->table_allocated) {FREE(gen->table); gen->table_allocated = false;}
  gen->table = val; 
  return(val);
}

static char *describe_waveshape(mus_any *ptr)
{
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, S_waveshape " freq: %.3fHz, phase: %.3f, size: %d",
	       mus_frequency(ptr),
	       mus_phase(ptr),
	       (int)ws_size(ptr));
  return(describe_buffer);
}

static mus_any_class WAVESHAPE_CLASS = {
  MUS_WAVESHAPE,
  S_waveshape,
  &free_ws,
  &describe_waveshape,
  &ws_equalp,
  &ws_data,
  &set_ws_data,
  &ws_size,
  &set_ws_size,
  &ws_freq,
  &set_ws_freq,
  &ws_phase,
  &set_ws_phase,
  0, 0,
  0, 0,
  &mus_waveshape,
  MUS_NOT_SPECIAL, 
  NULL, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &ws_reset,
  0
};

bool mus_waveshape_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_WAVESHAPE));}

mus_any *mus_make_waveshape(Float frequency, Float phase, Float *table, int size)
{
  ws *gen;
  gen = (ws *)clm_calloc(1, sizeof(ws), S_make_waveshape);
  gen->core = &WAVESHAPE_CLASS;
  gen->o = mus_make_oscil(frequency, phase);
  if (table)
    {
      gen->table = table;
      gen->table_allocated = false;
    }
  else
    {
      gen->table = (Float *)clm_calloc(size, sizeof(Float), "waveshape table");
      gen->table_allocated = true;
    }
  gen->table_size = size;
  gen->offset = (Float)(size - 1) / 2.0;
  return((mus_any *)gen);
}

Float mus_waveshape(mus_any *ptr, Float index, Float fm)
{
  ws *gen = (ws *)ptr;
  Float table_index;
  table_index = gen->offset * (1.0 + (mus_oscil_1(gen->o, fm) * index));
  return(mus_array_interp(gen->table, table_index, gen->table_size));
}

Float mus_waveshape_2(mus_any *ptr, Float fm)
{
  ws *gen = (ws *)ptr;
  Float table_index;
  table_index = gen->offset * (1.0 + mus_oscil_1(gen->o, fm));
  return(mus_array_interp(gen->table, table_index, gen->table_size));
}

Float mus_waveshape_1(mus_any *ptr, Float index)
{
  ws *gen = (ws *)ptr;
  Float table_index;
  table_index = gen->offset * (1.0 + (mus_oscil_0(gen->o) * index));
  return(mus_array_interp(gen->table, table_index, gen->table_size));
}

Float mus_waveshape_0(mus_any *ptr) /* default index is 1.0 */
{
  ws *gen = (ws *)ptr;
  Float table_index;
  table_index = gen->offset * (1.0 + mus_oscil_0(gen->o));
  return(mus_array_interp(gen->table, table_index, gen->table_size));
}

Float *mus_partials_to_waveshape(int npartials, Float *partials, int size, Float *table)
{
  /* partials incoming is a list of partials amps indexed by partial number */
  /* #<0.0, 1.0, 0.0> = 2nd partial 1.0, rest 0. */
  int i;
  Float maxI2, x;
  Float *data;
  if (partials == NULL) return(NULL);
  if (table == NULL)
    data = (Float *)clm_calloc(size, sizeof(Float), "waveshape table");
  else data = table;
  if (data == NULL) return(NULL);
  maxI2 = 2.0 / (Float)(size - 1); /* was size, but mus.lisp was correct?!? */
  for (i = 0, x = -1.0; i < size; i++, x += maxI2)
    {
      Float temp, Tn, Tn1, sum;
      int hnum;
      sum = 0.0;
      temp = 0.0;
      Tn = 1.0;
      Tn1 = x;
      for (hnum = 0; hnum < npartials; hnum++)
	{
	  sum += (Tn * partials[hnum]);
	  temp = Tn1;
	  Tn1 = (2.0 * Tn1 * x) - Tn;
	  Tn = temp;
	}
      data[i] = sum;
    }
  return(array_normalize(data, size));
}

Float *mus_partials_to_polynomial(int npartials, Float *partials, mus_polynomial_t kind)
{
  /* coeffs returned in partials */
  int i;
  int *T0, *T1, *Tn;
  Float *Cc1;
  T0 = (int *)clm_calloc(npartials + 1, sizeof(int), "partials_to_polynomial t0");
  T1 = (int *)clm_calloc(npartials + 1, sizeof(int), "partials_to_polynomial t1");
  Tn = (int *)clm_calloc(npartials + 1, sizeof(int), "partials_to_polynomial tn");
  Cc1 = (Float *)clm_calloc(npartials + 1, sizeof(Float), "partials_to_polynomial cc1");
  if (kind == MUS_CHEBYSHEV_FIRST_KIND)
    T0[0] = 1;
  else T0[0] = 0;
  T1[1] = 1;

  for (i = 1; i < npartials; i++)
    {
      int k;
      Float amp;
      amp = partials[i];
      if (amp != 0.0)
	{
	  if (kind == MUS_CHEBYSHEV_FIRST_KIND)
	    for (k = 0; k <= i; k++) 
	      Cc1[k] += (amp * T1[k]);
	  else
	    for (k = 1; k <= i; k++) 
	      Cc1[k - 1] += (amp * T1[k]);
	}
      for (k = i + 1; k > 0; k--) 
	Tn[k] = (2 * T1[k - 1]) - T0[k];
      Tn[0] = -T0[0];
      for (k = i + 1; k >= 0; k--)
	{
	  T0[k] = T1[k];
	  T1[k] = Tn[k];
	}
    }

  for (i = 0; i < npartials; i++) 
    partials[i] = Cc1[i];
  FREE(T0);
  FREE(T1);
  FREE(Tn);
  FREE(Cc1);
  return(partials);
}


/* ---------------- polyshape ---------------- */

static void poly_reset(mus_any *ptr)
{
  ws *gen = (ws *)ptr;
  oscil_reset(gen->o);
}

static char *describe_polyshape(mus_any *ptr)
{
  ws *gen = (ws *)ptr;
  char *str;
  str = float_array_to_string(gen->table, gen->table_size, 0);
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, S_polyshape " freq: %.3fHz, phase: %.3f, coeffs[%d]: %s",
	       mus_frequency(ptr),
	       mus_phase(ptr),
	       gen->table_size,
	       str);
  FREE(str);
  return(describe_buffer);
}

Float mus_polyshape(mus_any *ptr, Float index, Float fm)
{
  ws *gen = (ws *)ptr;
  return(mus_polynomial(gen->table,
			index * mus_oscil_1(gen->o, fm), 
			gen->table_size));
}

Float mus_polyshape_2(mus_any *ptr, Float fm)
{
  ws *gen = (ws *)ptr;
  return(mus_polynomial(gen->table,
			mus_oscil_1(gen->o, fm), 
			gen->table_size));
}

Float mus_polyshape_1(mus_any *ptr, Float index)
{
  ws *gen = (ws *)ptr;
  return(mus_polynomial(gen->table,
			index * mus_oscil_0(gen->o), 
			gen->table_size));
}

Float mus_polyshape_0(mus_any *ptr)
{
  ws *gen = (ws *)ptr;
  return(mus_polynomial(gen->table,
			mus_oscil_0(gen->o), 
			gen->table_size));
}

static mus_any_class POLYSHAPE_CLASS = {
  MUS_POLYSHAPE,
  S_polyshape,
  &free_ws,
  &describe_polyshape,
  &ws_equalp,
  &ws_data,
  &set_ws_data,
  &ws_size,
  &set_ws_size,
  &ws_freq,
  &set_ws_freq,
  &ws_phase,
  &set_ws_phase,
  0, 0,
  0, 0,
  &mus_polyshape,
  MUS_NOT_SPECIAL, 
  NULL, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &poly_reset,
  0
};

mus_any *mus_make_polyshape(Float frequency, Float phase, Float *coeffs, int size)
{
  ws *gen;
  gen = (ws *)clm_calloc(1, sizeof(ws), S_make_polyshape);
  gen->core = &POLYSHAPE_CLASS;
  gen->o = mus_make_oscil(frequency, phase);
  gen->table = coeffs;
  gen->table_allocated = false;
  gen->table_size = size;
  return((mus_any *)gen);
}

bool mus_polyshape_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_POLYSHAPE));}



/* ---------------- wave-train ---------------- */

typedef struct {
  mus_any_class *core;
  Float freq;
  Float phase;
  Float *wave;        /* passed in from caller */
  int wave_size;
  Float *out_data;
  int out_data_size;
  mus_interp_t interp_type; /* "type" field exists in core -- avoid confusion */
  Float next_wave_time;
  int out_pos;
  bool first_time;
  Float yn1;
} wt;

static Float wt_freq(mus_any *ptr) {return(((wt *)ptr)->freq);}
static Float set_wt_freq(mus_any *ptr, Float val) {((wt *)ptr)->freq = val; return(val);}

static Float wt_phase(mus_any *ptr) {return(fmod(((TWO_PI * ((wt *)ptr)->phase) / ((Float)((wt *)ptr)->wave_size)), TWO_PI));}
static Float set_wt_phase(mus_any *ptr, Float val) {((wt *)ptr)->phase = (fmod(val, TWO_PI) * ((wt *)ptr)->wave_size) / TWO_PI; return(val);}

static off_t wt_length(mus_any *ptr) {return(((wt *)ptr)->wave_size);}
static off_t wt_set_length(mus_any *ptr, off_t val) {if (val > 0) ((wt *)ptr)->wave_size = (int)val; return((off_t)(((wt *)ptr)->wave_size));}

static int wt_interp_type(mus_any *ptr) {return((int)(((wt *)ptr)->interp_type));}

static Float *wt_data(mus_any *ptr) {return(((wt *)ptr)->wave);}
static Float *wt_set_data(mus_any *ptr, Float *data) {((wt *)ptr)->wave = data; return(data);}

static bool wt_equalp(mus_any *p1, mus_any *p2)
{
  wt *w1 = (wt *)p1;
  wt *w2 = (wt *)p2;
  if (p1 == p2) return(true);
  return((w1) && (w2) &&
	 (w1->core->type == w2->core->type) &&
	 (w1->freq == w2->freq) &&
	 (w1->phase == w2->phase) &&
	 (w1->interp_type == w2->interp_type) &&
	 (w1->wave_size == w2->wave_size) &&
	 (w1->out_data_size == w2->out_data_size) &&
	 (w1->out_pos == w2->out_pos) &&
	 (clm_arrays_are_equal(w1->wave, w2->wave, w1->wave_size)) &&
	 (clm_arrays_are_equal(w1->out_data, w2->out_data, w1->out_data_size)));
}

static char *describe_wt(mus_any *ptr)
{
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, S_wave_train " freq: %.3fHz, phase: %.3f, size: " OFF_TD ", interp: %s",
	       mus_frequency(ptr), 
	       mus_phase(ptr), 
	       mus_length(ptr), 
	       interp_name[wt_interp_type(ptr)]);
  return(describe_buffer);
}

static Float mus_wave_train_any(mus_any *ptr, Float fm) 
{
  wt *gen = (wt *)ptr;
  Float result = 0.0;
  if (gen->out_pos < gen->out_data_size)
    result = gen->out_data[gen->out_pos];
  gen->out_pos++;
  if (gen->out_pos >= gen->next_wave_time)
    {
      int i;
      if (gen->out_pos < gen->out_data_size)
	{
	  int good_samps;
	  good_samps = gen->out_data_size - gen->out_pos;
	  memmove((void *)(gen->out_data), (void *)(gen->out_data + gen->out_pos), good_samps * sizeof(Float));
	  memset((void *)(gen->out_data + good_samps), 0, gen->out_pos * sizeof(Float));
	}
      else mus_clear_array(gen->out_data, gen->out_data_size);
      for (i = 0; i < gen->wave_size; i++)
	{
	  gen->yn1 = mus_interpolate(gen->interp_type, gen->phase + i, gen->wave, gen->wave_size, gen->yn1);
	  gen->out_data[i] += gen->yn1;
	}
      if (gen->first_time)
	{
	  gen->first_time = false;
	  gen->out_pos = (int)(gen->phase); /* initial phase, but as an integer in terms of wave table size (gad...) */
	  if (gen->out_pos >= gen->wave_size)
	    gen->out_pos = gen->out_pos % gen->wave_size;
	  result = gen->out_data[gen->out_pos++];
	  gen->next_wave_time = ((Float)sampling_rate / (gen->freq + fm));
	}
      else 
	{
	  gen->next_wave_time += (((Float)sampling_rate / (gen->freq + fm)) - gen->out_pos);
	  gen->out_pos = 0;
	}
    }
  return(result);
}

Float mus_wave_train(mus_any *ptr, Float fm) {return(mus_wave_train_any(ptr, fm / w_rate));}
Float mus_wave_train_1(mus_any *ptr) {return(mus_wave_train(ptr, 0.0));}
static Float run_wave_train(mus_any *ptr, Float fm, Float unused) {return(mus_wave_train_any(ptr, fm / w_rate));}

static int free_wt(mus_any *p) 
{
  wt *ptr = (wt *)p;
  if (ptr) 
    {
      if (ptr->out_data) {FREE(ptr->out_data); ptr->out_data = NULL;}
      FREE(ptr);
    }
  return(0);
}

static void wt_reset(mus_any *ptr)
{
  wt *gen = (wt *)ptr;
  gen->phase = 0.0;
  mus_clear_array(gen->out_data, gen->out_data_size);
  gen->out_pos = gen->out_data_size;
  gen->next_wave_time = 0.0;
  gen->first_time = true;
}

static mus_any_class WAVE_TRAIN_CLASS = {
  MUS_WAVE_TRAIN,
  S_wave_train,
  &free_wt,
  &describe_wt,
  &wt_equalp,
  &wt_data,
  &wt_set_data,
  &wt_length,
  &wt_set_length,
  &wt_freq,
  &set_wt_freq,
  &wt_phase,
  &set_wt_phase,
  0, 0,
  0, 0,
  &run_wave_train,
  MUS_NOT_SPECIAL, 
  NULL,
  &wt_interp_type,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &wt_reset,
  0
};

mus_any *mus_make_wave_train(Float freq, Float phase, Float *wave, int wave_size, mus_interp_t type)
{
  wt *gen;
  gen = (wt *)clm_calloc(1, sizeof(wt), S_make_wave_train);
  gen->core = &WAVE_TRAIN_CLASS;
  gen->freq = freq;
  gen->phase = (wave_size * fmod(phase, TWO_PI)) / TWO_PI;
  gen->wave = wave;
  gen->wave_size = wave_size;
  gen->interp_type = type;
  gen->out_data_size = wave_size + 2;
  gen->out_data = (Float *)clm_calloc(gen->out_data_size, sizeof(Float), "wave train out data");
  gen->out_pos = gen->out_data_size;
  gen->next_wave_time = 0.0;
  gen->first_time = true;
  gen->yn1 = 0.0;
  return((mus_any *)gen);
}

bool mus_wave_train_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_WAVE_TRAIN));}




/* ---------------- delay, comb, notch, all-pass, moving-average ---------------- */

typedef struct {
  mus_any_class *core;
  int loc, size;
  bool zdly, line_allocated;
  Float *line;
  int zloc, zsize;
  Float xscl, yscl, yn1;
  mus_interp_t type;
  mus_any *filt;
} dly;


Float mus_delay_tick(mus_any *ptr, Float input)
{
  dly *gen = (dly *)ptr;
  gen->line[gen->loc] = input;
  gen->loc++;
  if (gen->zdly)
    {
      if (gen->loc >= gen->zsize) gen->loc = 0;
      gen->zloc++;
      if (gen->zloc >= gen->zsize) gen->zloc = 0;
    }
  else
    {
      if (gen->loc >= gen->size) gen->loc = 0;
    }
  return(input);
}

Float mus_delay(mus_any *ptr, Float input, Float pm)
{
  Float result;
  dly *gen = (dly *)ptr;
  if ((gen->size == 0) && (pm < 1.0))
    return(pm * gen->line[0] + (1.0 - pm) * input);
  result = mus_tap(ptr, pm);
  mus_delay_tick(ptr, input);
  return(result);
}

Float mus_delay_1(mus_any *ptr, Float input)
{
  dly *gen = (dly *)ptr;
  Float result;
  if (gen->zdly) 
    {
      if (gen->size == 0) return(input); /* no point in tick in this case */
      result = gen->line[gen->zloc];
    }
  else result = gen->line[gen->loc];
  mus_delay_tick(ptr, input);
  return(result);
}

Float mus_tap(mus_any *ptr, Float loc)
{
  dly *gen = (dly *)ptr;
  int taploc;
  if (gen->zdly)
    {
      gen->yn1 = mus_interpolate(gen->type, gen->zloc - loc, gen->line, gen->zsize, gen->yn1);
      return(gen->yn1);
    }
  else
    {
      if (gen->size == 0) return(gen->line[0]);
      if ((int)loc == 0) return(gen->line[gen->loc]);
      taploc = (int)(gen->loc - (int)loc) % gen->size;
      if (taploc < 0) taploc += gen->size;
      return(gen->line[taploc]);
    }
}

Float mus_tap_1(mus_any *ptr)
{
  dly *gen = (dly *)ptr;
  return(gen->line[gen->loc]);
}

static int free_delay(mus_any *gen) 
{
  dly *ptr = (dly *)gen;
  if (ptr) 
    {
      if ((ptr->line) && (ptr->line_allocated)) FREE(ptr->line);
      FREE(ptr);
    }
  return(0);
}

static char *describe_delay(mus_any *ptr)
{
  char *str = NULL;
  dly *gen = (dly *)ptr;
  if (gen->zdly)
    mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, 
		 S_delay ": line[%d,%d, %s]: %s", 
		 gen->size, gen->zsize, 
		 interp_name[gen->type],
		 str = float_array_to_string(gen->line, gen->size, gen->zloc));
  else mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, 
		    S_delay ": line[%d, %s]: %s", 
		    gen->size, interp_name[gen->type], str = float_array_to_string(gen->line, gen->size, gen->loc));
  if (str) FREE(str);
  return(describe_buffer);
}

static bool delay_equalp(mus_any *p1, mus_any *p2)
{
  dly *d1 = (dly *)p1;
  dly *d2 = (dly *)p2;
  if (p1 == p2) return(true);
  return((d1) && (d2) &&
	 (d1->core->type == d2->core->type) &&
	 (d1->size == d2->size) &&
	 (d1->loc == d2->loc) &&
	 (d1->zdly == d2->zdly) &&
	 (d1->zloc == d2->zloc) &&
	 (d1->zsize == d2->zsize) &&
	 (d1->xscl == d2->xscl) &&
	 (d1->yscl == d2->yscl) &&
	 (d1->yn1 == d2->yn1) &&
	 (d1->type == d2->type) &&
	 (clm_arrays_are_equal(d1->line, d2->line, d1->size)));
}

static off_t delay_length(mus_any *ptr) 
{
  dly *d = (dly *)ptr;
  if (d->size > 0) /* this is possible (not sure it's a good idea...) */
    return(d->size);
  return(d->zsize); /* maybe always use this? */
}

static Float delay_scaler(mus_any *ptr) {return(((dly *)ptr)->xscl);}
static Float set_delay_scaler(mus_any *ptr, Float val) {((dly *)ptr)->xscl = val; return(val);}

static Float delay_fb(mus_any *ptr) {return(((dly *)ptr)->yscl);}
static Float set_delay_fb(mus_any *ptr, Float val) {((dly *)ptr)->yscl = val; return(val);}

static int delay_interp_type(mus_any *ptr) {return((int)(((dly *)ptr)->type));}

static Float *delay_data(mus_any *ptr) {return(((dly *)ptr)->line);}
static Float *delay_set_data(mus_any *ptr, Float *val) 
{
  dly *gen = (dly *)ptr;
  if (gen->line_allocated) {FREE(gen->line); gen->line_allocated = false;}
  gen->line = val; 
  return(val);
}

static off_t set_delay_length(mus_any *ptr, off_t val) 
{
  dly *gen = (dly *)ptr;  
  if (val > 0) 
    {
      int old_size;
      old_size = gen->size;
      gen->size = (int)val; 
      if (gen->size < old_size)
	{
	  if (gen->loc > gen->size) gen->loc = 0;
	  gen->zdly = false; /* otherwise too many ways to screw up */
	}
    }
  return((off_t)(gen->size));
}

bool mus_delay_line_p(mus_any *gen)
{
  return((gen) && (gen->core->extended_type == MUS_DELAY_LINE));
}

static void delay_reset(mus_any *ptr)
{
  dly *gen = (dly *)ptr;
  gen->loc = 0;
  gen->zloc = 0;
  gen->yn1 = 0.0;
  mus_clear_array(gen->line, gen->zsize);
}

static mus_any_class DELAY_CLASS = {
  MUS_DELAY,
  S_delay,
  &free_delay,
  &describe_delay,
  &delay_equalp,
  &delay_data,
  &delay_set_data,
  &delay_length,
  &set_delay_length,
  0, 0, 0, 0, /* freq phase */
  &delay_scaler,
  &set_delay_scaler,
  &delay_fb,
  &set_delay_fb,
  &mus_delay,
  MUS_DELAY_LINE,
  NULL, 
  &delay_interp_type,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &delay_reset,
  0
};

mus_any *mus_make_delay(int size, Float *preloaded_line, int line_size, mus_interp_t type) 
{
  /* if preloaded_line null, allocated locally */
  /* if size == line_size, normal (non-interpolating) delay */
  dly *gen;
  gen = (dly *)clm_calloc(1, sizeof(dly), S_make_delay);
  gen->core = &DELAY_CLASS;
  gen->loc = 0;
  gen->size = size;
  gen->zsize = line_size;
  gen->zdly = ((line_size != size) || (type != MUS_INTERP_NONE));
  gen->type = type;
  if (preloaded_line)
    {
      gen->line = preloaded_line;
      gen->line_allocated = false;
    }
  else 
    {
      gen->line = (Float *)clm_calloc(line_size, sizeof(Float), "delay line");
      gen->line_allocated = true;
    }
  gen->zloc = line_size - size;
  return((mus_any *)gen);
}

bool mus_delay_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_DELAY));}

Float mus_comb(mus_any *ptr, Float input, Float pm) 
{
  dly *gen = (dly *)ptr;
  if (gen->zdly)
    return(mus_delay(ptr, input + (gen->yscl * mus_tap(ptr, pm)), pm)); 
  /* mus.lisp has 0 in place of the final pm -- the question is whether the delay
     should interpolate as well as the tap.  There is a subtle difference in
     output (the pm case is low-passed by the interpolation ("average"),
     but I don't know if there's a standard here, or what people expect.
     We're doing the outer-level interpolation in notch and all-pass.
     Should mus.lisp be changed?
  */
  else return(mus_delay(ptr, input + (gen->line[gen->loc] * gen->yscl), 0.0));
}

Float mus_comb_1(mus_any *ptr, Float input) 
{
  dly *gen = (dly *)ptr;
  return(mus_delay_1(ptr, input + (gen->line[gen->loc] * gen->yscl)));
}

static char *describe_comb(mus_any *ptr)
{
  char *str = NULL;
  dly *gen = (dly *)ptr;
  if (gen->zdly)
    mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, 
		 S_comb ": scaler: %.3f, line[%d,%d, %s]: %s", 
		 gen->yscl, gen->size, gen->zsize, 
		 interp_name[gen->type],
		 str = float_array_to_string(gen->line, gen->size, gen->zloc));
  else mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, 
		    S_comb ": scaler: %.3f, line[%d, %s]: %s", 
		    gen->yscl, gen->size, 
		    interp_name[gen->type],
		    str = float_array_to_string(gen->line, gen->size, gen->loc));
  if (str) FREE(str);
  return(describe_buffer);
}

static mus_any_class COMB_CLASS = {
  MUS_COMB,
  S_comb,
  &free_delay,
  &describe_comb,
  &delay_equalp,
  &delay_data,
  &delay_set_data,
  &delay_length,
  &set_delay_length,
  0, 0, 0, 0, /* freq phase */
  &delay_scaler,
  &set_delay_scaler,
  &delay_fb,
  &set_delay_fb,
  &mus_comb,
  MUS_DELAY_LINE,
  NULL,
  &delay_interp_type,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &delay_reset,
  0
};

mus_any *mus_make_comb(Float scaler, int size, Float *line, int line_size, mus_interp_t type)
{
  dly *gen;
  gen = (dly *)mus_make_delay(size, line, line_size, type);
  if (gen)
    {
      gen->core = &COMB_CLASS;
      gen->yscl = scaler;
      return((mus_any *)gen);
    }
  return(NULL);
}

bool mus_comb_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_COMB));}

static char *describe_notch(mus_any *ptr)
{
  char *str = NULL;
  dly *gen = (dly *)ptr;
  if (gen->zdly)
    mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, 
		 S_notch ": scaler: %.3f, line[%d,%d, %s]: %s", 
		 gen->xscl, gen->size, gen->zsize, 
		 interp_name[gen->type],
		 str = float_array_to_string(gen->line, gen->size, gen->zloc));
  else mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, 
		    S_notch ": scaler: %.3f, line[%d, %s]: %s", 
		    gen->xscl, gen->size, 
		    interp_name[gen->type],
		    str = float_array_to_string(gen->line, gen->size, gen->loc));
  if (str) FREE(str);
  return(describe_buffer);
}

static mus_any_class NOTCH_CLASS = {
  MUS_NOTCH,
  S_notch,
  &free_delay,
  &describe_notch,
  &delay_equalp,
  &delay_data,
  &delay_set_data,
  &delay_length,
  &set_delay_length,
  0, 0, 0, 0, /* freq phase */
  &delay_scaler,
  &set_delay_scaler,
  0, 0,
  &mus_notch,
  MUS_DELAY_LINE,
  NULL,
  &delay_interp_type,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &delay_reset,
  0
};

Float mus_notch(mus_any *ptr, Float input, Float pm) 
{
  dly *gen = (dly *)ptr;
  return((input * gen->xscl) + mus_delay(ptr, input, pm));
}

Float mus_notch_1(mus_any *ptr, Float input) 
{
  return((input * ((dly *)ptr)->xscl) + mus_delay_1(ptr, input));
}

bool mus_notch_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_NOTCH));}

mus_any *mus_make_notch(Float scaler, int size, Float *line, int line_size, mus_interp_t type)
{
  dly *gen;
  gen = (dly *)mus_make_delay(size, line, line_size, type);
  if (gen)
    {
      gen->core = &NOTCH_CLASS;
      gen->xscl = scaler;
      return((mus_any *)gen);
    }
  return(NULL);
}

Float mus_all_pass(mus_any *ptr, Float input, Float pm)
{
  Float din;
  dly *gen = (dly *)ptr;
  if (gen->zdly)
    din = input + (gen->yscl * mus_tap(ptr, pm));
  else din = input + (gen->yscl * gen->line[gen->loc]);
  return(mus_delay(ptr, din, pm) + (gen->xscl * din));
}

Float mus_all_pass_1(mus_any *ptr, Float input)
{
  Float din;
  dly *gen = (dly *)ptr;
  din = input + (gen->yscl * gen->line[gen->loc]);
  return(mus_delay_1(ptr, din) + (gen->xscl * din));
}

bool mus_all_pass_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_ALL_PASS));}

static char *describe_all_pass(mus_any *ptr)
{
  char *str = NULL;
  dly *gen = (dly *)ptr;
  if (gen->zdly)
    mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, 
		 S_all_pass ": feedback: %.3f, feedforward: %.3f, line[%d,%d, %s]:%s",
		 gen->yscl, gen->xscl, gen->size, gen->zsize, 
		 interp_name[gen->type],
		 str = float_array_to_string(gen->line, gen->size, gen->zloc));
  else mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, 
		    S_all_pass ": feedback: %.3f, feedforward: %.3f, line[%d, %s]:%s",
		    gen->yscl, gen->xscl, gen->size, 
		    interp_name[gen->type],
		    str = float_array_to_string(gen->line, gen->size, gen->loc));
  if (str) FREE(str);
  return(describe_buffer);
}

static mus_any_class ALL_PASS_CLASS = {
  MUS_ALL_PASS,
  S_all_pass,
  &free_delay,
  &describe_all_pass,
  &delay_equalp,
  &delay_data,
  &delay_set_data,
  &delay_length,
  &set_delay_length,
  0, 0, 0, 0, /* freq phase */
  &delay_scaler,
  &set_delay_scaler,
  &delay_fb,
  &set_delay_fb,
  &mus_all_pass,
  MUS_DELAY_LINE,
  NULL,
  &delay_interp_type,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &delay_reset,
  0
};

mus_any *mus_make_all_pass(Float backward, Float forward, int size, Float *line, int line_size, mus_interp_t type)
{
  dly *gen;
  gen = (dly *)mus_make_delay(size, line, line_size, type);
  if (gen)
    {
      gen->core = &ALL_PASS_CLASS;
      gen->xscl = forward;
      gen->yscl = backward;
      return((mus_any *)gen);
    }
  return(NULL);
}


bool mus_moving_average_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_MOVING_AVERAGE));}

Float mus_moving_average(mus_any *ptr, Float input)
{
  dly *gen = (dly *)ptr;
  Float output;
  output = mus_delay_1(ptr, input);
  gen->xscl += (input - output);
  return(gen->xscl * gen->yscl); /* xscl=sum, yscl=1/n */
}

static Float run_mus_moving_average(mus_any *ptr, Float input, Float unused) {return(mus_moving_average(ptr, input));}

static void moving_average_reset(mus_any *ptr)
{
  dly *gen = (dly *)ptr;
  delay_reset(ptr);
  gen->xscl = 0.0;
}

static char *describe_moving_average(mus_any *ptr)
{
  char *str = NULL;
  dly *gen = (dly *)ptr;
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, 
	       S_moving_average ": %.3f, line[%d]:%s",
	       gen->xscl * gen->xscl, gen->size, 
	       str = float_array_to_string(gen->line, gen->size, gen->loc));
  if (str) FREE(str);
  return(describe_buffer);
}

static mus_any_class MOVING_AVERAGE_CLASS = {
  MUS_MOVING_AVERAGE,
  S_moving_average,
  &free_delay,
  &describe_moving_average,
  &delay_equalp,
  &delay_data,
  &delay_set_data,
  &delay_length,
  &set_delay_length,
  0, 0, 0, 0, /* freq phase */
  &delay_scaler,
  &set_delay_scaler,
  &delay_fb,
  &set_delay_fb,
  &run_mus_moving_average,
  MUS_DELAY_LINE,
  NULL, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &moving_average_reset,
  0
};

mus_any *mus_make_moving_average(int size, Float *line)
{
  dly *gen;
  gen = (dly *)mus_make_delay(size, line, size, MUS_INTERP_NONE);
  if (gen)
    {
      int i;
      gen->core = &MOVING_AVERAGE_CLASS;
      gen->xscl = 0.0;
      for (i = 0; i < size; i++) 
	gen->xscl += gen->line[i];
      gen->yscl = 1.0 / (Float)size;
      return((mus_any *)gen);
    }
  return(NULL);
}



/* ---------------------------------------- filtered-comb ---------------------------------------- */

static void filtered_comb_reset(mus_any *ptr)
{
  dly *fc = (dly *)ptr;
  delay_reset(ptr);
  mus_reset(fc->filt);
}

static bool filtered_comb_equalp(mus_any *p1, mus_any *p2)
{
  return((delay_equalp(p1, p2)) &&
	 (mus_equalp(((dly *)p1)->filt, 
		     ((dly *)p2)->filt)));
}

static char *describe_filtered_comb(mus_any *ptr)
{
  char *comb_str, *filter_str;
  static char *res = NULL;
  int len;
  if (res) FREE(res); /* left over from before (mus_describe result isn't freed by caller) */
  comb_str = strdup(describe_comb(ptr));
  filter_str = strdup(mus_describe(((dly *)ptr)->filt));
  len = strlen(comb_str) + strlen(filter_str) + 64;
  res = (char *)CALLOC(len, sizeof(char));
  mus_snprintf(res, len, "%s: [%s], [%s]", S_filtered_comb, comb_str, filter_str);
  free(comb_str);
  free(filter_str);
  return(res);
}

bool mus_filtered_comb_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_FILTERED_COMB));}

Float mus_filtered_comb(mus_any *ptr, Float input, Float pm)
{
  dly *fc = (dly *)ptr;
  if (fc->zdly)
    return(mus_delay(ptr,
		     input + (fc->yscl * 
			      mus_run(fc->filt, 
				      mus_tap(ptr, pm), 
				      0.0)), 
		     pm)); 
  else return(mus_delay(ptr,
			input + (fc->yscl * 
				 mus_run(fc->filt, 
					 fc->line[fc->loc], 
					 0.0)), 
			0.0));
}

Float mus_filtered_comb_1(mus_any *ptr, Float input)
{
  dly *fc = (dly *)ptr;
  return(mus_delay_1(ptr,
		     input + (fc->yscl * 
			      mus_run(fc->filt, 
				      fc->line[fc->loc], 
				      0.0))));
}

static mus_any_class FILTERED_COMB_CLASS = {
  MUS_FILTERED_COMB,
  S_filtered_comb,
  &free_delay,
  &describe_filtered_comb,
  &filtered_comb_equalp,
  &delay_data,
  &delay_set_data,
  &delay_length,
  &set_delay_length,
  0, 0, 0, 0, /* freq phase */
  &delay_scaler,
  &set_delay_scaler,
  &delay_fb,
  &set_delay_fb,
  &mus_filtered_comb,
  MUS_DELAY_LINE,
  NULL,
  &delay_interp_type,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &filtered_comb_reset,
  0
};

mus_any *mus_make_filtered_comb(Float scaler, int size, Float *line, int line_size, mus_interp_t type, mus_any *filt)
{
  if (filt)
    {
      dly *fc;
      fc = (dly *)mus_make_comb(scaler, size, line, line_size, type);
      if (fc)
	{
	  fc->core = &FILTERED_COMB_CLASS;
	  fc->filt = filt;
	  return((mus_any *)fc);
	}
      else return(NULL);
    }
  return(mus_make_comb(scaler, size, line, line_size, type));
}



/* ---------------- sawtooth et al ---------------- */

typedef struct {
  mus_any_class *core;
  Float current_value;
  Float freq;
  Float phase;
  Float base;
  Float width;
} sw;

static int free_sw(mus_any *ptr) {if (ptr) FREE(ptr); return(0);}

Float mus_sawtooth_wave(mus_any *ptr, Float fm)
{
  sw *gen = (sw *)ptr;
  Float result;
  result = gen->current_value;
  gen->phase += (gen->freq + fm);
  if ((gen->phase >= TWO_PI) || (gen->phase < 0.0))
    {
      gen->phase = fmod(gen->phase, TWO_PI);
      if (gen->phase < 0.0) gen->phase += TWO_PI;
    }
  gen->current_value = gen->base * (gen->phase - M_PI);
  return(result);
}

static Float run_sawtooth_wave(mus_any *ptr, Float fm, Float unused) {return(mus_sawtooth_wave(ptr, fm));}
bool mus_sawtooth_wave_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_SAWTOOTH_WAVE));}

static Float sw_freq(mus_any *ptr) {return(mus_radians_to_hz(((sw *)ptr)->freq));}
static Float set_sw_freq(mus_any *ptr, Float val) {((sw *)ptr)->freq = mus_hz_to_radians(val); return(val);}

static Float sw_phase(mus_any *ptr) {return(fmod(((sw *)ptr)->phase, TWO_PI));}
static Float set_sw_phase(mus_any *ptr, Float val) {((sw *)ptr)->phase = val; return(val);}

static Float sw_width(mus_any *ptr) {return((((sw *)ptr)->width) / ( 2 * M_PI));}
static Float sw_set_width(mus_any *ptr, Float val) {((sw *)ptr)->width = (2 * M_PI * val); return(val);}

static Float sawtooth_scaler(mus_any *ptr) {return(((sw *)ptr)->base * M_PI);}
static Float set_sawtooth_scaler(mus_any *ptr, Float val) {((sw *)ptr)->base = val / M_PI; return(val);}

static bool sw_equalp(mus_any *p1, mus_any *p2)
{
  sw *s1, *s2;
  s1 = (sw *)p1;
  s2 = (sw *)p2;
  return((p1 == p2) ||
	 ((s1) && (s2) &&
	  (s1->core->type == s2->core->type) &&
	  (s1->freq == s2->freq) &&
	  (s1->phase == s2->phase) &&
	  (s1->base == s2->base) &&
	  (s1->current_value == s2->current_value)));
}

static char *describe_sw(mus_any *ptr)
{
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, "%s freq: %.3fHz, phase: %.3f, amp: %.3f",
	       mus_name(ptr),
	       mus_frequency(ptr),
	       mus_phase(ptr),
	       mus_scaler(ptr));
  return(describe_buffer);
}

static void sawtooth_reset(mus_any *ptr)
{
  sw *gen = (sw *)ptr;
  gen->phase = M_PI;
  gen->current_value = 0.0;
}

static mus_any_class SAWTOOTH_WAVE_CLASS = {
  MUS_SAWTOOTH_WAVE,
  S_sawtooth_wave,
  &free_sw,
  &describe_sw,
  &sw_equalp,
  0, 0, 0, 0,
  &sw_freq,
  &set_sw_freq,
  &sw_phase,
  &set_sw_phase,
  &sawtooth_scaler,
  &set_sawtooth_scaler,
  0, 0,
  &run_sawtooth_wave,
  MUS_NOT_SPECIAL, 
  NULL, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &sawtooth_reset,
  0
};

mus_any *mus_make_sawtooth_wave(Float freq, Float amp, Float phase) /* M_PI as initial phase, normally */
{
  sw *gen;
  gen = (sw *)clm_calloc(1, sizeof(sw), S_make_sawtooth_wave);
  gen->core = &SAWTOOTH_WAVE_CLASS;
  gen->freq = mus_hz_to_radians(freq);
  gen->base = (amp / M_PI);
  gen->phase = phase;
  gen->current_value = gen->base * (gen->phase - M_PI);
  return((mus_any *)gen);
}

Float mus_square_wave(mus_any *ptr, Float fm)
{
  sw *gen = (sw *)ptr;
  Float result;
  result = gen->current_value;
  gen->phase += (gen->freq + fm);
  if ((gen->phase >= TWO_PI) || (gen->phase < 0.0))
    {
      gen->phase = fmod(gen->phase, TWO_PI);
      if (gen->phase < 0.0) gen->phase += TWO_PI;
    }
  if (gen->phase < gen->width) gen->current_value = gen->base; else gen->current_value = 0.0;
  return(result);
}

bool mus_square_wave_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_SQUARE_WAVE));}

static Float run_square_wave(mus_any *ptr, Float fm, Float unused) {return(mus_square_wave(ptr, fm));}

static Float square_wave_scaler(mus_any *ptr) {return(((sw *)ptr)->base);}
static Float set_square_wave_scaler(mus_any *ptr, Float val) {((sw *)ptr)->base = val; return(val);}

static void square_wave_reset(mus_any *ptr)
{
  sw *gen = (sw *)ptr;
  gen->phase = 0.0;
  gen->current_value = gen->base;
}

static mus_any_class SQUARE_WAVE_CLASS = {
  MUS_SQUARE_WAVE,
  S_square_wave,
  &free_sw,
  &describe_sw,
  &sw_equalp,
  0, 0, 0, 0,
  &sw_freq,
  &set_sw_freq,
  &sw_phase,
  &set_sw_phase,
  &square_wave_scaler,
  &set_square_wave_scaler,
  0, 0,
  &run_square_wave,
  MUS_NOT_SPECIAL, 
  NULL, 0,
  0, 0, 
  &sw_width, &sw_set_width, 
  0, 0, 
  0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &square_wave_reset,
  0
};

mus_any *mus_make_square_wave(Float freq, Float amp, Float phase)
{
  sw *gen;
  gen = (sw *)clm_calloc(1, sizeof(sw), S_make_square_wave);
  gen->core = &SQUARE_WAVE_CLASS;
  gen->freq = mus_hz_to_radians(freq);
  gen->base = amp;
  gen->phase = phase;
  gen->width = M_PI;
  if (gen->phase < gen->width) 
    gen->current_value = gen->base; 
  else gen->current_value = 0.0;
  return((mus_any *)gen);
}

Float mus_triangle_wave(mus_any *ptr, Float fm)
{
  sw *gen = (sw *)ptr;
  Float result;
  result = gen->current_value;
  gen->phase += (gen->freq + fm);
  if ((gen->phase >= TWO_PI) || (gen->phase < 0.0))
    {
      gen->phase = fmod(gen->phase, TWO_PI);
      if (gen->phase < 0.0) gen->phase += TWO_PI;
    }
  if (gen->phase < (M_PI / 2.0)) 
    gen->current_value = gen->base * gen->phase;
  else
    if (gen->phase < (M_PI * 1.5)) 
      gen->current_value = gen->base * (M_PI - gen->phase);
    else gen->current_value = gen->base * (gen->phase - TWO_PI);
  return(result);
}

bool mus_triangle_wave_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_TRIANGLE_WAVE));}

static Float run_triangle_wave(mus_any *ptr, Float fm, Float unused) {return(mus_triangle_wave(ptr, fm));}

static Float triangle_wave_scaler(mus_any *ptr) {return(((sw *)ptr)->base * M_PI_2);}
static Float set_triangle_wave_scaler(mus_any *ptr, Float val) {((sw *)ptr)->base = (val * 2.0 / M_PI); return(val);}

static void triangle_wave_reset(mus_any *ptr)
{
  sw *gen = (sw *)ptr;
  gen->phase = 0.0;
  gen->current_value = 0.0;
}

static mus_any_class TRIANGLE_WAVE_CLASS = {
  MUS_TRIANGLE_WAVE,
  S_triangle_wave,
  &free_sw,
  &describe_sw,
  &sw_equalp,
  0, 0, 0, 0,
  &sw_freq,
  &set_sw_freq,
  &sw_phase,
  &set_sw_phase,
  &triangle_wave_scaler,
  &set_triangle_wave_scaler,
  0, 0,
  &run_triangle_wave,
  MUS_NOT_SPECIAL, 
  NULL, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &triangle_wave_reset,
  0
};

mus_any *mus_make_triangle_wave(Float freq, Float amp, Float phase)
{
  sw *gen;
  gen = (sw *)clm_calloc(1, sizeof(sw), S_make_triangle_wave);
  gen->core = &TRIANGLE_WAVE_CLASS;
  gen->freq = mus_hz_to_radians(freq);
  gen->base = (2.0 * amp / M_PI);
  gen->phase = phase;
  if (gen->phase < M_PI_2) 
    gen->current_value = gen->base * gen->phase;
  else
    if (gen->phase < (M_PI * 1.5)) 
      gen->current_value = gen->base * (M_PI - gen->phase);
    else gen->current_value = gen->base * (gen->phase - TWO_PI);
  return((mus_any *)gen);
}

Float mus_pulse_train(mus_any *ptr, Float fm)
{
  sw *gen = (sw *)ptr;
  if ((gen->phase >= TWO_PI) || (gen->phase < 0.0))
    {
      gen->phase = fmod(gen->phase, TWO_PI);
      if (gen->phase < 0.0) gen->phase += TWO_PI;
      gen->current_value = gen->base;
    }
  else gen->current_value = 0.0;
  gen->phase += (gen->freq + fm);
  return(gen->current_value);
}

bool mus_pulse_train_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_PULSE_TRAIN));}

static Float run_pulse_train(mus_any *ptr, Float fm, Float unused) {return(mus_pulse_train(ptr, fm));}

static Float pulse_train_scaler(mus_any *ptr) {return(((sw *)ptr)->base);}
static Float set_pulse_train_scaler(mus_any *ptr, Float val) {((sw *)ptr)->base = val; return(val);}

static void pulse_train_reset(mus_any *ptr)
{
  sw *gen = (sw *)ptr;
  gen->phase = TWO_PI;
  gen->current_value = 0.0;
}

static mus_any_class PULSE_TRAIN_CLASS = {
  MUS_PULSE_TRAIN,
  S_pulse_train,
  &free_sw,
  &describe_sw,
  &sw_equalp,
  0, 0, 0, 0,
  &sw_freq,
  &set_sw_freq,
  &sw_phase,
  &set_sw_phase,
  &pulse_train_scaler,
  &set_pulse_train_scaler,
  0, 0,
  &run_pulse_train,
  MUS_NOT_SPECIAL, 
  NULL, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &pulse_train_reset,
  0
};

mus_any *mus_make_pulse_train(Float freq, Float amp, Float phase) /* TWO_PI initial phase, normally */
{
  sw *gen;
  gen = (sw *)clm_calloc(1, sizeof(sw), S_make_pulse_train);
  gen->core = &PULSE_TRAIN_CLASS;
  gen->freq = mus_hz_to_radians(freq);
  gen->base = amp;
  gen->phase = phase;
  gen->current_value = 0.0;
  return((mus_any *)gen);
}



/* ---------------- rand, rand_interp ---------------- */

typedef struct {
  mus_any_class *core;
  Float freq;
  Float base;
  Float phase;
  Float output;
  Float incr;
  Float *distribution;
  int distribution_size;
} noi;

/* rand taken from the ANSI C standard (essentially the same as the Cmix form used earlier) */
static unsigned long randx = 1;
#define INVERSE_MAX_RAND  0.0000610351563
#define INVERSE_MAX_RAND2 0.000030517579

void mus_set_rand_seed(unsigned long val) {randx = val;}
unsigned long mus_rand_seed(void) {return(randx);}

static Float next_random(void)
{
  randx = randx * 1103515245 + 12345;
  return((Float)((unsigned int)(randx >> 16) & 32767));
}

Float mus_random(Float amp) /* -amp to amp as Float */
{
  return(amp * (next_random() * INVERSE_MAX_RAND - 1.0));
}

Float mus_random_1(void) /* -1.0 to 1.0 as Float */
{
  return(next_random() * INVERSE_MAX_RAND - 1.0);
}

Float mus_frandom(Float amp) /* 0.0 to amp as Float */
{
  return(amp * next_random() * INVERSE_MAX_RAND2);
}

Float mus_frandom_1(void) /* 0.0 to 1.0 as Float */
{
  return(next_random() * INVERSE_MAX_RAND2);
}

int mus_irandom(int amp)
{
  return((int)(amp * next_random() * INVERSE_MAX_RAND2));
}

static Float random_any(noi *gen) /* -amp to amp possibly through distribution */
{
  if (gen->distribution)
    return(gen->base * mus_array_interp(gen->distribution, 
					next_random() * INVERSE_MAX_RAND2 * gen->distribution_size, 
					gen->distribution_size));
  return(gen->base * (next_random() * INVERSE_MAX_RAND - 1.0));
}

Float mus_rand(mus_any *ptr, Float fm)
{
  noi *gen = (noi *)ptr;
  if ((gen->phase >= TWO_PI) || (gen->phase < 0.0))
    {
      gen->phase = fmod(gen->phase, TWO_PI);
      if (gen->phase < 0.0) gen->phase += TWO_PI;
      gen->output = random_any(gen);
    }
  gen->phase += (gen->freq + fm);
  return(gen->output);
}

Float mus_rand_interp(mus_any *ptr, Float fm)
{
  /* fm can change the increment step during a ramp */
  noi *gen = (noi *)ptr;
  gen->output += gen->incr;
  if (gen->output > gen->base) 
    gen->output = gen->base;
  else 
    {
      if (gen->output < -gen->base)
	gen->output = -gen->base;
    }
  if ((gen->phase >= TWO_PI) || (gen->phase < 0.0))
    {
      gen->phase = fmod(gen->phase, TWO_PI);
      if (gen->phase < 0.0) gen->phase += TWO_PI;
      gen->incr = (random_any(gen) - gen->output) / (ceil(TWO_PI / (gen->freq + fm)));
    }
  gen->phase += (gen->freq + fm);
  return(gen->output);
}

static Float run_rand(mus_any *ptr, Float fm, Float unused) {return(mus_rand(ptr, fm));}
static Float run_rand_interp(mus_any *ptr, Float fm, Float unused) {return(mus_rand_interp(ptr, fm));}
bool mus_rand_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_RAND));}
bool mus_rand_interp_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_RAND_INTERP));}

static int free_noi(mus_any *ptr) {if (ptr) FREE(ptr); return(0);}

static Float noi_freq(mus_any *ptr) {return(mus_radians_to_hz(((noi *)ptr)->freq));}
static Float set_noi_freq(mus_any *ptr, Float val) {((noi *)ptr)->freq = mus_hz_to_radians(val); return(val);}

static Float noi_phase(mus_any *ptr) {return(fmod(((noi *)ptr)->phase, TWO_PI));}
static Float set_noi_phase(mus_any *ptr, Float val) {((noi *)ptr)->phase = val; return(val);}

static Float noi_scaler(mus_any *ptr) {return(((noi *)ptr)->base);}
static Float set_noi_scaler(mus_any *ptr, Float val) {((noi *)ptr)->base = val; return(val);}

static Float *noi_data(mus_any *ptr) {return(((noi *)ptr)->distribution);}
static off_t noi_length(mus_any *ptr) {return(((noi *)ptr)->distribution_size);}

static void noi_reset(mus_any *ptr)
{
  noi *gen = (noi *)ptr;
  gen->phase = 0.0;
  gen->output = 0.0;
}

static bool noi_equalp(mus_any *p1, mus_any *p2)
{
  noi *g1 = (noi *)p1;
  noi *g2 = (noi *)p2;
  return((p1 == p2) ||
	 ((g1) && (g2) &&
	  (g1->core->type == g2->core->type) &&
	  (g1->freq == g2->freq) &&
	  (g1->phase == g2->phase) &&
	  (g1->output == g2->output) &&
	  (g1->incr == g2->incr) &&
	  (g1->base == g2->base) &&
	  (g1->distribution_size == g2->distribution_size) &&
	  (g1->distribution == g2->distribution)));
}

static char *describe_noi(mus_any *ptr)
{
  noi *gen = (noi *)ptr;
  if (mus_rand_p(ptr))
    mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, S_rand " freq: %.3fHz, phase: %.3f, amp: %.3f%s",
		 mus_frequency(ptr),
		 mus_phase(ptr),
		 mus_scaler(ptr),
		 (gen->distribution) ? ", with distribution envelope" : "");
  else
    mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, S_rand_interp " freq: %.3fHz, phase: %.3f, amp: %.3f, incr: %.3f, curval: %.3f%s",
		 mus_frequency(ptr),
		 mus_phase(ptr),
		 mus_scaler(ptr),
		 gen->incr,
		 gen->output,
		 (gen->distribution) ? ", with distribution envelope" : "");
  return(describe_buffer);
}

static mus_any_class RAND_INTERP_CLASS = {
  MUS_RAND_INTERP,
  S_rand_interp,
  &free_noi,
  &describe_noi,
  &noi_equalp,
  &noi_data, 0, 
  &noi_length, 0,
  &noi_freq,
  &set_noi_freq,
  &noi_phase,
  &set_noi_phase,
  &noi_scaler,
  &set_noi_scaler,
  0, 0,
  &run_rand_interp,
  MUS_NOT_SPECIAL, 
  NULL, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &noi_reset,
  0
};

static mus_any_class RAND_CLASS = {
  MUS_RAND,
  S_rand,
  &free_noi,
  &describe_noi,
  &noi_equalp,
  &noi_data, 0, 
  &noi_length, 0,
  &noi_freq,
  &set_noi_freq,
  &noi_phase,
  &set_noi_phase,
  &noi_scaler,
  &set_noi_scaler,
  0, 0,
  &run_rand,
  MUS_NOT_SPECIAL, 
  NULL, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &noi_reset,
  0
};

mus_any *mus_make_rand(Float freq, Float base)
{
  noi *gen;
  gen = (noi *)clm_calloc(1, sizeof(noi), S_make_rand);
  gen->core = &RAND_CLASS;
  gen->freq = mus_hz_to_radians(freq);
  gen->base = base;
  gen->incr = 0.0;
  gen->output = random_any(gen); /* this was always starting at 0.0 (changed 23-Dec-06) */
  return((mus_any *)gen);
}

mus_any *mus_make_rand_interp(Float freq, Float base)
{
  noi *gen;
  gen = (noi *)clm_calloc(1, sizeof(noi), S_make_rand_interp);
  gen->core = &RAND_INTERP_CLASS;
  gen->freq = mus_hz_to_radians(freq);
  gen->base = base;
  gen->incr =  mus_random(base) * freq / sampling_rate;
  gen->output = 0.0;
  return((mus_any *)gen);
}

mus_any *mus_make_rand_with_distribution(Float freq, Float base, Float *distribution, int distribution_size)
{
  noi *gen;
  gen = (noi *)mus_make_rand(freq, base);
  gen->distribution = distribution;
  gen->distribution_size = distribution_size;
  gen->output = random_any(gen);
  return((mus_any *)gen);
}

mus_any *mus_make_rand_interp_with_distribution(Float freq, Float base, Float *distribution, int distribution_size)
{
  noi *gen;
  gen = (noi *)mus_make_rand_interp(freq, base);
  gen->distribution = distribution;
  gen->distribution_size = distribution_size;
  return((mus_any *)gen);
}



/* ---------------- simple filters ---------------- */

/* eventually this class/struct could be replaced by flt/filter below */
typedef struct {
  mus_any_class *core;
  Float xs[3];
  Float ys[3];
  Float x1, x2, y1, y2;
  Float gain, radius, frequency;
} smpflt;

static int free_smpflt(mus_any *ptr) {if (ptr) FREE(ptr); return(0);}

static bool smpflt_equalp(mus_any *p1, mus_any *p2)
{
  smpflt *g1 = (smpflt *)p1;
  smpflt *g2 = (smpflt *)p2;
  return((p1 == p2) ||
	 ((g1->core->type == g2->core->type) &&
	  (g1->xs[0] == g2->xs[0]) &&
	  (g1->xs[1] == g2->xs[1]) &&
	  (g1->xs[2] == g2->xs[2]) &&
	  (g1->ys[1] == g2->ys[1]) &&
	  (g1->ys[2] == g2->ys[2]) &&
	  (g1->x1 == g2->x1) &&
	  (g1->x2 == g2->x2) &&
	  (g1->y1 == g2->y1) &&
	  (g1->y2 == g2->y2)));
}

static char *describe_smpflt(mus_any *ptr)
{
  smpflt *gen = (smpflt *)ptr;
  switch (gen->core->type)
    {
    case MUS_ONE_ZERO: 
      mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, S_one_zero ": a0: %.3f, a1: %.3f, x1: %.3f", 
		   gen->xs[0], gen->xs[1], gen->x1); 
      break;
    case MUS_ONE_POLE: 
      mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, S_one_pole ": a0: %.3f, b1: %.3f, y1: %.3f", 
		   gen->xs[0], gen->ys[1], gen->y1); 
      break;
    case MUS_TWO_ZERO: 
      mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, S_two_zero ": a0: %.3f, a1: %.3f, a2: %.3f, x1: %.3f, x2: %.3f",
		   gen->xs[0], gen->xs[1], gen->xs[2], gen->x1, gen->x2); 
      break;
    case MUS_TWO_POLE: 
      mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, S_two_pole ": a0: %.3f, b1: %.3f, b2: %.3f, y1: %.3f, y2: %.3f",
		   gen->xs[0], gen->ys[1], gen->ys[2], gen->y1, gen->y2); 
      break;
    }
  return(describe_buffer);
}

Float mus_one_zero(mus_any *ptr, Float input)
{
  smpflt *gen = (smpflt *)ptr;
  Float result;
  result = (gen->xs[0] * input) + (gen->xs[1] * gen->x1);
  gen->x1 = input;
  return(result);
}

static Float run_one_zero(mus_any *ptr, Float input, Float unused) {return(mus_one_zero(ptr, input));}
static off_t one_length(mus_any *ptr) {return(1);}
static off_t two_length(mus_any *ptr) {return(2);}

static Float smp_xcoeff(mus_any *ptr, int index) {return(((smpflt *)ptr)->xs[index]);}
static Float smp_set_xcoeff(mus_any *ptr, int index, Float val) {((smpflt *)ptr)->xs[index] = val; return(val);}

static Float smp_ycoeff(mus_any *ptr, int index) {return(((smpflt *)ptr)->ys[index]);}
static Float smp_set_ycoeff(mus_any *ptr, int index, Float val) {((smpflt *)ptr)->ys[index] = val; return(val);}

static Float *smp_xcoeffs(mus_any *ptr) {return(((smpflt *)ptr)->xs);}
static Float *smp_ycoeffs(mus_any *ptr) {return(((smpflt *)ptr)->ys);}

static void smpflt_reset(mus_any *ptr)
{
  smpflt *gen = (smpflt *)ptr;
  gen->x1 = 0.0;
  gen->x2 = 0.0;
  gen->y1 = 0.0;
  gen->y2 = 0.0;
}

static mus_any_class ONE_ZERO_CLASS = {
  MUS_ONE_ZERO,
  S_one_zero,
  &free_smpflt,
  &describe_smpflt,
  &smpflt_equalp,
  0, 0,
  &one_length, 0,
  0, 0, 0, 0,
  0, 0,
  0, 0,
  &run_one_zero,
  MUS_SIMPLE_FILTER, 
  NULL, 0,
  0, 0, 
  0, 0,
  &smp_xcoeff, &smp_set_xcoeff,
  0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 
  &smp_xcoeffs, &smp_ycoeffs, 0,
  &smpflt_reset,
  0
};

mus_any *mus_make_one_zero(Float a0, Float a1)
{
  smpflt *gen;
  gen = (smpflt *)clm_calloc(1, sizeof(smpflt), S_make_one_zero);
  gen->core = &ONE_ZERO_CLASS;
  gen->xs[0] = a0;
  gen->xs[1] = a1;
  return((mus_any *)gen);
}

bool mus_one_zero_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_ONE_ZERO));}

Float mus_one_pole(mus_any *ptr, Float input)
{
  smpflt *gen = (smpflt *)ptr;
  gen->y1 = (gen->xs[0] * input) - (gen->ys[1] * gen->y1);
  return(gen->y1);
}

static Float run_one_pole(mus_any *ptr, Float input, Float unused) {return(mus_one_pole(ptr, input));}

static mus_any_class ONE_POLE_CLASS = {
  MUS_ONE_POLE,
  S_one_pole,
  &free_smpflt,
  &describe_smpflt,
  &smpflt_equalp,
  0, 0,
  &one_length, 0,
  0, 0, 0, 0,
  0, 0, 0, 0,
  &run_one_pole,
  MUS_SIMPLE_FILTER, 
  NULL, 0,
  0, 0, 0, 0, 
  &smp_xcoeff, &smp_set_xcoeff, 
  0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  &smp_ycoeff, &smp_set_ycoeff, 
  &smp_xcoeffs, &smp_ycoeffs, 0,
  &smpflt_reset,
  0
};

mus_any *mus_make_one_pole(Float a0, Float b1)
{
  smpflt *gen;
  gen = (smpflt *)clm_calloc(1, sizeof(smpflt), S_make_one_pole);
  gen->core = &ONE_POLE_CLASS;
  gen->xs[0] = a0;
  gen->ys[1] = b1;
  return((mus_any *)gen);
}

bool mus_one_pole_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_ONE_POLE));}

Float mus_two_zero(mus_any *ptr, Float input)
{
  smpflt *gen = (smpflt *)ptr;
  Float result;
  result = (gen->xs[0] * input) + (gen->xs[1] * gen->x1) + (gen->xs[2] * gen->x2);
  gen->x2 = gen->x1;
  gen->x1 = input;
  return(result);
}

static Float run_two_zero(mus_any *ptr, Float input, Float unused) {return(mus_two_zero(ptr, input));}

static Float two_zero_radius(mus_any *ptr) 
{
  smpflt *gen = (smpflt *)ptr; 
  return(sqrt(gen->xs[2]));
}

static Float set_two_zero_radius(mus_any *ptr, Float new_radius)
{
  smpflt *gen = (smpflt *)ptr; 
  gen->xs[1] = -2.0 * new_radius * cos(mus_hz_to_radians(mus_frequency(ptr)));
  gen->xs[2] = new_radius * new_radius;
  return(new_radius);
}

static Float two_zero_frequency(mus_any *ptr)
{
  smpflt *gen = (smpflt *)ptr;
  return(mus_radians_to_hz(acos(gen->xs[1] / (-2.0 * two_zero_radius(ptr)))));
}

static Float set_two_zero_frequency(mus_any *ptr, Float new_freq)
{
  smpflt *gen = (smpflt *)ptr; 
  gen->xs[1] = -2.0 * mus_scaler(ptr) * cos(mus_hz_to_radians(new_freq));
  return(new_freq);
}

static mus_any_class TWO_ZERO_CLASS = {
  MUS_TWO_ZERO,
  S_two_zero,
  &free_smpflt,
  &describe_smpflt,
  &smpflt_equalp,
  0, 0,
  &two_length, 0,
  &two_zero_frequency, &set_two_zero_frequency, 
  0, 0,
  &two_zero_radius, &set_two_zero_radius, 
  0, 0,
  &run_two_zero,
  MUS_SIMPLE_FILTER, 
  NULL, 0,
  0, 0, 0, 0,
  &smp_xcoeff, &smp_set_xcoeff,
  0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0,
  &smp_xcoeffs, &smp_ycoeffs, 0,
  &smpflt_reset,
  0
};

mus_any *mus_make_two_zero(Float a0, Float a1, Float a2)
{
  smpflt *gen;
  gen = (smpflt *)clm_calloc(1, sizeof(smpflt), S_make_two_zero);
  gen->core = &TWO_ZERO_CLASS;
  gen->xs[0] = a0;
  gen->xs[1] = a1;
  gen->xs[2] = a2;
  return((mus_any *)gen);
}

bool mus_two_zero_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_TWO_ZERO));}

mus_any *mus_make_two_zero_from_radius_and_frequency(Float radius, Float frequency)
{
  return(mus_make_two_zero(1.0, -2.0 * radius * cos(mus_hz_to_radians(frequency)), radius * radius));
}

Float mus_two_pole(mus_any *ptr, Float input)
{
  smpflt *gen = (smpflt *)ptr;
  Float result;
  result = (gen->xs[0] * input) - (gen->ys[1] * gen->y1) - (gen->ys[2] * gen->y2);
  gen->y2 = gen->y1;
  gen->y1 = result;
  return(result);
}

static Float run_two_pole(mus_any *ptr, Float input, Float unused) {return(mus_two_pole(ptr, input));}

static Float two_pole_radius(mus_any *ptr) 
{
  smpflt *gen = (smpflt *)ptr; 
  return(sqrt(gen->ys[2]));
}

static Float set_two_pole_radius(mus_any *ptr, Float new_radius)
{
  smpflt *gen = (smpflt *)ptr; 
  gen->ys[1] = -2.0 * new_radius * cos(mus_hz_to_radians(mus_frequency(ptr)));
  gen->ys[2] = new_radius * new_radius;
  return(new_radius);
}

static Float two_pole_frequency(mus_any *ptr)
{
  smpflt *gen = (smpflt *)ptr;
  return(mus_radians_to_hz(acos(gen->ys[1] / (-2.0 * two_pole_radius(ptr)))));
}

static Float set_two_pole_frequency(mus_any *ptr, Float new_freq)
{
  smpflt *gen = (smpflt *)ptr; 
  gen->ys[1] = -2.0 * mus_scaler(ptr) * cos(mus_hz_to_radians(new_freq));
  return(new_freq);
}

static mus_any_class TWO_POLE_CLASS = {
  MUS_TWO_POLE,
  S_two_pole,
  &free_smpflt,
  &describe_smpflt,
  &smpflt_equalp,
  0, 0,
  &two_length, 0,
  &two_pole_frequency, &set_two_pole_frequency, 
  0, 0,
  &two_pole_radius, &set_two_pole_radius, 
  0, 0,
  &run_two_pole,
  MUS_SIMPLE_FILTER, 
  NULL, 0,
  0, 0, 0, 0,
  &smp_xcoeff, &smp_set_xcoeff, 
  0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  &smp_ycoeff, &smp_set_ycoeff, 
  &smp_xcoeffs, &smp_ycoeffs, 0,
  &smpflt_reset,
  0
};

mus_any *mus_make_two_pole(Float a0, Float b1, Float b2)
{
 if (fabs(b1) >= 2.0) 
   mus_error(MUS_UNSTABLE_TWO_POLE_ERROR, S_make_two_pole ": b1 = %.3f", b1);
 else
   {
     if (fabs(b2) >= 1.0) 
       mus_error(MUS_UNSTABLE_TWO_POLE_ERROR, S_make_two_pole ": b2 = %.3f", b2);
     else
       {
	 if ( ((b1 * b1) - (b2 * 4.0) >= 0.0) &&
	      ( ((b1 + b2) >= 1.0) || 
		((b2 - b1) >= 1.0)))
	   mus_error(MUS_UNSTABLE_TWO_POLE_ERROR, S_make_two_pole ": b1 = %.3f, b2 = %.3f", b1, b2);
	 else
	   {
	     smpflt *gen;
	     gen = (smpflt *)clm_calloc(1, sizeof(smpflt), S_make_two_pole);
	     gen->core = &TWO_POLE_CLASS;
	     gen->xs[0] = a0;
	     gen->ys[1] = b1;
	     gen->ys[2] = b2;
	     return((mus_any *)gen);
	    }
	}
    }
  return(NULL);
}

bool mus_two_pole_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_TWO_POLE));}

mus_any *mus_make_two_pole_from_radius_and_frequency(Float radius, Float frequency)
{
  return(mus_make_two_pole(1.0, -2.0 * radius * cos(mus_hz_to_radians(frequency)), radius * radius));
}



/* ---------------- formant ---------------- */

bool mus_formant_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_FORMANT));}

static char *describe_formant(mus_any *ptr)
{
  smpflt *gen = (smpflt *)ptr;
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, 
	       S_formant ": radius: %.3f, frequency: %.3f, (gain: %.3f)",
	       gen->radius, gen->frequency, gen->gain);
  return(describe_buffer);
}

Float mus_formant(mus_any *ptr, Float input) 
{
  smpflt *gen = (smpflt *)ptr;
  Float inval, tpinval, output;
  inval = gen->xs[0] * input;
  tpinval = inval + (gen->xs[2] * gen->x2);
  output = tpinval - (gen->ys[1] * gen->y1) - (gen->ys[2] * gen->y2);
  gen->y2 = gen->y1;
  gen->y1 = output;
  gen->x2 = gen->x1;
  gen->x1 = inval;
  return(output);
}

static Float run_formant(mus_any *ptr, Float input, Float unused) {return(mus_formant(ptr, input));}

Float mus_formant_bank(Float *amps, mus_any **formants, Float inval, int size)
{
  int i;
  Float sum = 0.0;
  for (i = 0; i < size; i++) 
    sum += (amps[i] * mus_formant(formants[i], inval));
  return(sum);
}

void mus_set_formant_radius_and_frequency(mus_any *ptr, Float radius, Float frequency)
{
  Float fw;
  smpflt *gen = (smpflt *)ptr;
  fw = mus_hz_to_radians(frequency);
  gen->radius = radius;
  gen->frequency = frequency;
  gen->ys[2] = radius * radius;
  gen->xs[0] = gen->gain * sin(fw) * (1.0 - gen->ys[2]);
  gen->xs[2] = -radius;
  gen->ys[1] = -2.0 * radius * cos(fw);
}

static Float formant_frequency(mus_any *ptr)
{
  smpflt *gen = (smpflt *)ptr;
  return(gen->frequency);
}

static Float set_formant_frequency(mus_any *ptr, Float val)
{
  smpflt *gen = (smpflt *)ptr;
  mus_set_formant_radius_and_frequency(ptr, gen->radius, val);
  return(val);
}

static Float f_radius(mus_any *ptr) {return(((smpflt *)ptr)->radius);}
static Float f_set_radius(mus_any *ptr, Float val) {mus_set_formant_radius_and_frequency(ptr, val, ((smpflt *)ptr)->frequency); return(val);}

static Float f_gain(mus_any *ptr) {return(((smpflt *)ptr)->gain);}
static Float f_set_gain(mus_any *ptr, Float val)
{
  smpflt *gen = (smpflt *)ptr;
  if (gen->gain != 0.0)
    gen->xs[0] *= (val / gen->gain);
  else gen->xs[0] = val * sin(mus_hz_to_radians(gen->frequency)) * (1.0 - gen->ys[2]);
  gen->gain = val;
  return(val);
}

static mus_any_class FORMANT_CLASS = {
  MUS_FORMANT,
  S_formant,
  &free_smpflt,
  &describe_formant,
  &smpflt_equalp,
  0, 0,
  &two_length, 0,
  &formant_frequency,
  &set_formant_frequency,
  &f_radius,
  &f_set_radius,
  &f_gain, &f_set_gain,
  0, 0,
  &run_formant,
  MUS_SIMPLE_FILTER, 
  NULL, 0,
  0, 0, 0, 0,
  &smp_xcoeff, &smp_set_xcoeff, 
  0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  &smp_ycoeff, &smp_set_ycoeff, 
  &smp_xcoeffs, &smp_ycoeffs, 0,
  &smpflt_reset,
  0
};

mus_any *mus_make_formant(Float radius, Float frequency, Float gain)
{
  smpflt *gen;
  gen = (smpflt *)clm_calloc(1, sizeof(smpflt), S_make_formant);
  gen->core = &FORMANT_CLASS;
  gen->gain = gain;
  gen->xs[1] = gain; /* for backwards compatibility */
  mus_set_formant_radius_and_frequency((mus_any *)gen, radius, frequency);
  return((mus_any *)gen);
}


/* ---------------- filter ---------------- */

typedef struct {
  mus_any_class *core;
  int order, allocated_size;
  bool state_allocated;
  Float *x, *y, *state;
} flt;

Float mus_filter(mus_any *ptr, Float input)
{
  flt *gen = (flt *)ptr;
  Float xout = 0.0;
  int j;
  if (!(gen->y)) return(mus_fir_filter(ptr, input));  
  if (!(gen->x)) return(mus_iir_filter(ptr, input));
  gen->state[0] = input;
  for (j = gen->order - 1; j >= 1; j--) 
    {
      xout += gen->state[j] * gen->x[j];
      gen->state[0] -= gen->y[j] * gen->state[j];
      gen->state[j] = gen->state[j - 1];
    }
  return(xout + (gen->state[0] * gen->x[0]));
}

/* in the common low-order symmetrical coeffs case, we can speed fir_filter
   up by about 40% by gathering the same-coeff cases before the multiply,
   but in the current context, the coeffs can change at any time,
   and I'd rather not slow down the basic case (via a check in the make
   function and a (*run) specialization) to try to automate it;
   the extra cases could be handled explicitly by the user, but that
   goes against all the other such cases, where the optimizations
   are handled internally (snd-run for example).
*/
Float mus_fir_filter(mus_any *ptr, Float input)
{
  Float xout = 0.0;
  int j;
  flt *gen = (flt *)ptr;
  gen->state[0] = input;
  for (j = gen->order - 1; j >= 1; j--) 
    {
      xout += gen->state[j] * gen->x[j]; /* if fma: xout = fma(gen->state[j], gen->x[j], xout) */
      gen->state[j] = gen->state[j - 1];
    }
  return(xout + (gen->state[0] * gen->x[0]));
}

Float mus_iir_filter(mus_any *ptr, Float input)
{
  int j;
  flt *gen = (flt *)ptr;
  gen->state[0] = input;
  for (j = gen->order - 1; j >= 1; j--) 
    {
      gen->state[0] -= gen->y[j] * gen->state[j];
      gen->state[j] = gen->state[j - 1];
    }
  return(gen->state[0]);
}

static Float run_filter(mus_any *ptr, Float input, Float unused) {return(mus_filter(ptr, input));}
static Float run_fir_filter(mus_any *ptr, Float input, Float unused) {return(mus_fir_filter(ptr, input));}
static Float run_iir_filter(mus_any *ptr, Float input, Float unused) {return(mus_iir_filter(ptr, input));}

bool mus_filter_p(mus_any *ptr) 
{
  return((ptr) && 
	 ((ptr->core->type == MUS_FILTER) || 
	  (ptr->core->type == MUS_FIR_FILTER) ||
	  (ptr->core->type == MUS_IIR_FILTER)));
}

bool mus_fir_filter_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_FIR_FILTER));}
bool mus_iir_filter_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_IIR_FILTER));}

static Float *filter_data(mus_any *ptr) {return(((flt *)ptr)->state);}
static off_t filter_length(mus_any *ptr) {return(((flt *)ptr)->order);}

static Float *filter_xcoeffs(mus_any *ptr) {return(((flt *)ptr)->x);}
static Float *filter_ycoeffs(mus_any *ptr) {return(((flt *)ptr)->y);}

Float *mus_filter_set_xcoeffs(mus_any *ptr, Float *new_data)
{
  /* needed by Snd if filter order increased during play */
  flt *gen = (flt *)ptr;
  Float *old_data;
  old_data = gen->x;
  gen->x = new_data;
  return(old_data);
}

Float *mus_filter_set_ycoeffs(mus_any *ptr, Float *new_data)
{
  flt *gen = (flt *)ptr;
  Float *old_data;
  old_data = gen->y;
  gen->y = new_data;
  return(old_data);
}

static off_t set_filter_length(mus_any *ptr, off_t val) 
{
  /* just resets order if order < allocated size */
  flt *gen = (flt *)ptr;
  if ((val > 0) && (val <= gen->allocated_size))
    gen->order = (int)val;
  return((off_t)(gen->order));
}

int mus_filter_set_order(mus_any *ptr, int order)
{
  /* resets order and fixes state array if needed (coeffs arrays should be handled separately by set_x|ycoeffs above) */
  /*   returns either old order or -1 if state array can't be reallocated */
  flt *gen = (flt *)ptr;
  int old_order;
  if ((order > gen->allocated_size) &&
      (!(gen->state_allocated)))
    return(-1);
  old_order = gen->order;
  gen->order = order;
  if (order > gen->allocated_size)
    {
      int i;
      gen->allocated_size = order;
      gen->state = (Float *)REALLOC(gen->state, order * sizeof(Float));
      for (i = old_order; i < order; i++)
	gen->state[i] = 0.0; /* try to minimize click */
    }
  return(old_order);
}

static Float filter_xcoeff(mus_any *ptr, int index) 
{
  flt *gen = (flt *)ptr;
  if (!(gen->x)) return((Float)mus_error(MUS_NO_XCOEFFS, "no xcoeffs"));
  if ((index >= 0) && (index < gen->order))
    return(gen->x[index]);
  return((Float)mus_error(MUS_ARG_OUT_OF_RANGE, S_mus_xcoeff ": invalid index %d, order = %d?", index, gen->order));
}

static Float filter_set_xcoeff(mus_any *ptr, int index, Float val) 
{
  flt *gen = (flt *)ptr;
  if (!(gen->x)) return((Float)mus_error(MUS_NO_XCOEFFS, "no xcoeffs"));
  if ((index >= 0) && (index < gen->order))
    {
      gen->x[index] = val;
      return(val);
    }
  return((Float)mus_error(MUS_ARG_OUT_OF_RANGE, S_setB S_mus_xcoeff ": invalid index %d, order = %d?", index, gen->order));
}

static Float filter_ycoeff(mus_any *ptr, int index) 
{
  flt *gen = (flt *)ptr;
  if (!(gen->y)) return((Float)mus_error(MUS_NO_YCOEFFS, "no ycoeffs"));
  if ((index >= 0) && (index < gen->order))
    return(gen->y[index]);
  return((Float)mus_error(MUS_ARG_OUT_OF_RANGE, S_mus_ycoeff ": invalid index %d, order = %d?", index, gen->order));
}

static Float filter_set_ycoeff(mus_any *ptr, int index, Float val) 
{
  flt *gen = (flt *)ptr;
  if (!(gen->y)) return((Float)mus_error(MUS_NO_YCOEFFS, "no ycoeffs"));
  if ((index >= 0) && (index < gen->order))
    {
      gen->y[index] = val;
      return(val);
    }
  return((Float)mus_error(MUS_ARG_OUT_OF_RANGE, S_setB S_mus_ycoeff ": invalid index %d, order = %d?", index, gen->order));
}

static int free_filter(mus_any *ptr)
{
  flt *gen = (flt *)ptr;
  if (gen)
    {
      if ((gen->state) && (gen->state_allocated)) FREE(gen->state);
      FREE(gen);
    }
  return(0);
}

static bool filter_equalp(mus_any *p1, mus_any *p2) 
{
  flt *f1, *f2;
  f1 = (flt *)p1;
  f2 = (flt *)p2;
  if (p1 == p2) return(true);
  return(((p1->core)->type == (p2->core)->type) &&
	 ((mus_filter_p(p1)) || (mus_fir_filter_p(p1)) || (mus_iir_filter_p(p1))) &&
	 (f1->order == f2->order) &&
	 ((!(f1->x)) || (!(f2->x)) || (clm_arrays_are_equal(f1->x, f2->x, f1->order))) &&
	 ((!(f1->y)) || (!(f2->y)) || (clm_arrays_are_equal(f1->y, f2->y, f1->order))) &&
	 (clm_arrays_are_equal(f1->state, f2->state, f1->order)));
}

static char *describe_filter(mus_any *ptr)
{
  flt *gen = (flt *)ptr;
  char *xstr = NULL, *ystr = NULL;
  xstr = float_array_to_string(gen->x, gen->order, 0);
  ystr = float_array_to_string(gen->y, gen->order, 0);
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, "%s: order: %d, xs: %s, ys: %s", 
	       gen->core->name, 
	       gen->order,
	       xstr, ystr);
  if (xstr) FREE(xstr);
  if (ystr) FREE(ystr);
  return(describe_buffer);
}

static char *describe_fir_filter(mus_any *ptr)
{
  flt *gen = (flt *)ptr;
  char *xstr = NULL;
  xstr = float_array_to_string(gen->x, gen->order, 0);
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, "%s: order: %d, xs: %s", 
	       gen->core->name, 
	       gen->order,
	       xstr);
  if (xstr) FREE(xstr);
  return(describe_buffer);
}

static char *describe_iir_filter(mus_any *ptr)
{
  flt *gen = (flt *)ptr;
  char *ystr = NULL;
  ystr = float_array_to_string(gen->y, gen->order, 0);
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, "%s: order: %d, ys: %s", 
	       gen->core->name, 
	       gen->order,
	       ystr);
  if (ystr) FREE(ystr);
  return(describe_buffer);
}

static void filter_reset(mus_any *ptr)
{
  flt *gen = (flt *)ptr;
  mus_clear_array(gen->state, gen->allocated_size);
}

static mus_any_class FILTER_CLASS = {
  MUS_FILTER,
  S_filter,
  &free_filter,
  &describe_filter,
  &filter_equalp,
  &filter_data, 0,
  &filter_length,
  &set_filter_length,
  0, 0, 0, 0,
  0, 0,
  0, 0,
  &run_filter,
  MUS_FULL_FILTER, 
  NULL, 0,
  0, 0, 0, 0, 
  &filter_xcoeff, &filter_set_xcoeff, 
  0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  &filter_ycoeff, &filter_set_ycoeff, &filter_xcoeffs, &filter_ycoeffs, 0,
  &filter_reset,
  0
};

static mus_any_class FIR_FILTER_CLASS = {
  MUS_FIR_FILTER,
  S_fir_filter,
  &free_filter,
  &describe_fir_filter,
  &filter_equalp,
  &filter_data, 0,
  &filter_length,
  &set_filter_length,
  0, 0, 0, 0,
  0, 0,
  0, 0,
  &run_fir_filter,
  MUS_FULL_FILTER, 
  NULL, 0,
  0, 0, 0, 0, 
  &filter_xcoeff, &filter_set_xcoeff, 
  0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, &filter_xcoeffs, 0, 0,
  &filter_reset,
  0
};

static mus_any_class IIR_FILTER_CLASS = {
  MUS_IIR_FILTER,
  S_iir_filter,
  &free_filter,
  &describe_iir_filter,
  &filter_equalp,
  &filter_data, 0,
  &filter_length,
  &set_filter_length,
  0, 0, 0, 0,
  0, 0,
  0, 0,
  &run_iir_filter,
  MUS_FULL_FILTER, 
  NULL, 0,
  0, 0, 0, 0, 
  0, 0,
  0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  &filter_ycoeff, &filter_set_ycoeff, 0, &filter_ycoeffs, 0,
  &filter_reset,
  0
};

static mus_any *make_filter(mus_any_class *cls, const char *name, int order, Float *xcoeffs, Float *ycoeffs, Float *state) /* if state null, allocated locally */
{
  if (order <= 0)
    mus_error(MUS_ARG_OUT_OF_RANGE, "%s order = %d?", name, order);
  else
    {
      flt *gen;
      gen = (flt *)clm_calloc(1, sizeof(flt), name);
      if (state)
	gen->state = state;
      else 
	{
	  gen->state = (Float *)clm_calloc(order, sizeof(Float), "filter state space");
	  gen->state_allocated = true;
	}
      gen->core = cls;
      gen->order = order;
      gen->allocated_size = order;
      gen->x = xcoeffs;
      gen->y = ycoeffs;
      return((mus_any *)gen);
    }
  return(NULL);
}

mus_any *mus_make_filter(int order, Float *xcoeffs, Float *ycoeffs, Float *state)
{
  return(make_filter(&FILTER_CLASS, S_make_filter, order, xcoeffs, ycoeffs, state));
}

mus_any *mus_make_fir_filter(int order, Float *xcoeffs, Float *state)
{
  return(make_filter(&FIR_FILTER_CLASS, S_make_fir_filter, order, xcoeffs, NULL, state));
}

mus_any *mus_make_iir_filter(int order, Float *ycoeffs, Float *state)
{
  return(make_filter(&IIR_FILTER_CLASS, S_make_iir_filter, order, NULL, ycoeffs, state));
}

Float *mus_make_fir_coeffs(int order, Float *envl, Float *aa)
{
  /* envl = evenly sampled freq response, has order samples */
  int n, i, j, jj;
  Float scl;
  Float *a;
  n = order;
  if (n <= 0) return(aa);
  if (aa) 
    a = aa;
  else a = (Float *)clm_calloc(order, sizeof(Float), "coeff space");
  if (!a) return(NULL);
  if (!(POWER_OF_2_P(order)))
    {
      int m;
      Float am, q, xt = 0.0, xt0, qj, x;
      m = (n + 1) / 2;
      am = 0.5 * (n + 1);
      scl = 2.0 / (Float)n;
      q = TWO_PI / (Float)n;
      xt0 = envl[0] * 0.5;
      for (j = 0, jj = n - 1; j < m; j++, jj--)
	{
	  xt = xt0;
	  qj = q * (am - j - 1);
	  for (i = 1, x = qj; i < m; i++, x += qj)
	    xt += (envl[i] * cos(x));
	  a[j] = xt * scl;
	  a[jj] = a[j];
	}
    }
  else /* use fft if it's easy to match -- there must be a way to handle odd orders here */
    {
      Float *rl, *im;
      int fsize, lim;
      Float offset;
      fsize = 2 * order; /* checked power of 2 above */
      rl = (Float *)CALLOC(fsize, sizeof(Float));
      im = (Float *)CALLOC(fsize, sizeof(Float));
      lim = order / 2;
      memcpy((void *)rl, (void *)envl, lim * sizeof(Float));
      /* for (i = 0; i < lim; i++) rl[i] = envl[i]; */
      mus_fft(rl, im, fsize, 1);
      scl = 4.0 / fsize;
      offset = -2.0 * envl[0] / fsize;
      for (i = 0; i < fsize; i++) 
	rl[i] = rl[i] * scl + offset;
      for (i = 1, j = lim - 1, jj = lim; i < order; i += 2, j--, jj++) 
	{
	  a[j] = rl[i]; 
	  a[jj] = rl[i];
	}
      FREE(rl);
      FREE(im);
    }
  return(a);
}


/* ---------------- env ---------------- */

/* although a pain, this way of doing env is 5 times faster than a table lookup,
 * in the linear segment and step cases.  In the exponential case, it is
 * only slightly slower (but more accurate).
 */

/* user defined env: 
 *  new-env-type (remove env_style_t->int, export presets, perhaps add sinusoid and power env to presets)
 *    needs init func (to package up particular call), run func -- split out all preset cases here?
 *    needs interp-func, set location func, description, free, equalp, restart
 *  make-env :type arg
 *  for Snd, would need ptree-channel settings
 */

typedef enum {ENV_SEG, ENV_STEP, ENV_EXP} env_style_t;

typedef struct {
  mus_any_class *core;
  double rate, current_value, base, offset, scaler, power, init_y, init_power, original_scaler, original_offset;
  off_t pass, end;
  env_style_t style;
  int index, size;
  bool data_allocated;
  Float *original_data;
  double *rates;
  off_t *passes;
} seg;

/* what about breakpoint triples for per-segment exp envs? */

bool mus_env_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_ENV));}
bool mus_env_linear_p(mus_any *ptr) {return(mus_env_p(ptr) && (((seg *)ptr)->style == ENV_SEG));}

Float mus_env(mus_any *ptr)
{
  seg *gen = (seg *)ptr;
  Float val;
  val = gen->current_value;
  if ((gen->index < gen->size) && (gen->pass >= gen->passes[gen->index]))
    {
      gen->index++;
      gen->rate = gen->rates[gen->index];
    }
  switch (gen->style)
    {
    case ENV_SEG: gen->current_value += gen->rate; break;
    case ENV_STEP: gen->current_value = gen->rate; break;
    case ENV_EXP:
      if (gen->rate != 0.0)
	{
	  gen->power += gen->rate;
	  gen->current_value = gen->offset + (gen->scaler * exp(gen->power));
	}
      break;
    }
  gen->pass++;
  return(val);
}

Float mus_env_linear(mus_any *ptr)
{
  seg *gen = (seg *)ptr;
  Float val;
  val = gen->current_value;
  if ((gen->index < gen->size) && (gen->pass >= gen->passes[gen->index]))
    {
      gen->index++;
      gen->rate = gen->rates[gen->index];
    }
  gen->current_value += gen->rate;
  gen->pass++;
  return(val);
}

static Float run_env(mus_any *ptr, Float unused1, Float unused2) {return(mus_env(ptr));}

static void dmagify_env(seg *e, Float *data, int pts, off_t dur, Float scaler)
{ 
  int i, j;
  off_t passes;
  double curx, curpass = 0.0;
  double x0, y0, x1 = 0.0, y1 = 0.0, xmag = 1.0;
  e->size = pts;
  if (pts > 1)
    {
      x1 = data[0];
      if (data[pts * 2 - 2] != x1)
	xmag = (double)(dur - 1) / (double)(data[pts * 2 - 2] - x1); /* was dur, 7-Apr-02 */
      y1 = data[1];
    }
  e->rates = (double *)clm_calloc(pts, sizeof(double), "env rates");
  e->passes = (off_t *)clm_calloc(pts, sizeof(off_t), "env passes");
  for (j = 0, i = 2; i < pts * 2; i += 2, j++)
    {
      x0 = x1;
      x1 = data[i];
      y0 = y1;
      y1 = data[i + 1];
      curx = xmag * (x1 - x0);
      if (curx < 1.0) curx = 1.0;
      curpass += curx;
      if (e->style == ENV_STEP)
	e->passes[j] = (off_t)curpass; /* this is the change boundary (confusing...) */
      else e->passes[j] = (off_t)(curpass + 0.5);
      if (j == 0) 
	passes = e->passes[0]; 
      else passes = e->passes[j] - e->passes[j - 1];
      if (e->style == ENV_STEP)
	e->rates[j] = e->offset + (scaler * y0);
      else
	{
	  if ((y0 == y1) || (passes == 0))
	    e->rates[j] = 0.0;
	  else e->rates[j] = scaler * (y1 - y0) / (double)passes;
	}
    }
  if ((pts > 1) && (e->passes[pts - 2] != e->end))
    e->passes[pts - 2] = e->end;
  if ((pts > 1) && (e->style == ENV_STEP))
    e->rates[pts - 1] = e->rates[pts - 2]; /* stick at last value, which in this case is the value (not 0 as increment) */
  e->passes[pts - 1] = 100000000;
}

static Float *fixup_exp_env(seg *e, Float *data, int pts, Float offset, Float scaler, Float base)
{
  Float min_y, max_y, val = 0.0, tmp = 0.0, b1;
  int len, i;
  bool flat;
  Float *result = NULL;
  if ((base <= 0.0) || (base == 1.0)) return(NULL);
  min_y = offset + scaler * data[1];
  max_y = min_y;
  b1 = base - 1.0;
  len = pts * 2;
  result = (Float *)clm_calloc(len, sizeof(Float), "env data");
  result[0] = data[0];
  result[1] = min_y;
  for (i = 2; i < len; i += 2)
    {
      tmp = offset + scaler * data[i + 1];
      result[i] = data[i];
      result[i + 1] = tmp;
      if (tmp < min_y) min_y = tmp;
      if (tmp > max_y) max_y = tmp;
    }
  flat = (min_y == max_y);
  if (!flat) val = 1.0 / (max_y - min_y);
  for (i = 1; i < len; i += 2)
    {
      if (flat) 
	tmp = 1.0;
      else tmp = val * (result[i] - min_y);
      result[i] = log(1.0 + (tmp * b1));
    }
  e->scaler = (max_y - min_y) / b1;
  e->offset = min_y;
  return(result);
}

static bool env_equalp(mus_any *p1, mus_any *p2)
{
  seg *e1 = (seg *)p1;
  seg *e2 = (seg *)p2;
  if (p1 == p2) return(true);
  return((e1) && (e2) &&
	 (e1->core->type == e2->core->type) &&
	 (e1->pass == e2->pass) &&
	 (e1->end == e2->end) &&
	 (e1->style == e2->style) &&
	 (e1->index == e2->index) &&
	 (e1->size == e2->size) &&

	 (e1->rate == e2->rate) &&
	 (e1->base == e2->base) &&
	 (e1->power == e2->power) &&
	 (e1->current_value == e2->current_value) &&
	 (e1->scaler == e2->scaler) &&
	 (e1->offset == e2->offset) &&
	 (e1->init_y == e2->init_y) &&
	 (e1->init_power == e2->init_power) &&
	 (clm_arrays_are_equal(e1->original_data, e2->original_data, e1->size * 2)));
}

static char *describe_env(mus_any *ptr)
{
  char *str = NULL;
  seg *e = (seg *)ptr;
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE,
	       S_env ": %s, pass: " OFF_TD " (dur: " OFF_TD "), index: %d, scaler: %.4f, offset: %.4f, data: %s",
	       ((e->style == ENV_SEG) ? "linear" : ((e->style == ENV_EXP) ? "exponential" : "step")),
	       e->pass, e->end + 1, e->index,
	       e->original_scaler, e->original_offset,
	       str = float_array_to_string(e->original_data, e->size * 2, 0));
  if (str) FREE(str);
  return(describe_buffer);
}

static int free_env_gen(mus_any *pt) 
{
  seg *ptr = (seg *)pt;
  if (ptr) 
    {
      if (ptr->passes) FREE(ptr->passes);
      if (ptr->rates) FREE(ptr->rates);
      if ((ptr->original_data) && (ptr->data_allocated)) FREE(ptr->original_data);
      FREE(ptr); 
    }
  return(0);
}

static Float *env_data(mus_any *ptr) {return(((seg *)ptr)->original_data);} /* mus-data */
static Float env_scaler(mus_any *ptr) {return(((seg *)ptr)->original_scaler);}

static Float env_offset(mus_any *ptr) {return(((seg *)ptr)->original_offset);}
static Float set_env_offset(mus_any *ptr, Float val) {((seg *)ptr)->original_offset = val; return(val);}

int mus_env_breakpoints(mus_any *ptr) {return(((seg *)ptr)->size);}
static off_t env_length(mus_any *ptr) {return((((seg *)ptr)->end));}
static Float env_current_value(mus_any *ptr) {return(((seg *)ptr)->current_value);}

off_t *mus_env_passes(mus_any *gen) {return(((seg *)gen)->passes);}
double *mus_env_rates(mus_any *gen) {return(((seg *)gen)->rates);}

static int env_position(mus_any *ptr) {return(((seg *)ptr)->index);}
double mus_env_offset(mus_any *gen) {return(((seg *)gen)->offset);}
double mus_env_scaler(mus_any *gen) {return(((seg *)gen)->scaler);}
double mus_env_initial_power(mus_any *gen) {return(((seg *)gen)->init_power);}

static off_t seg_pass(mus_any *ptr) {return(((seg *)ptr)->pass);}
static void set_env_location(mus_any *ptr, off_t val);
static off_t seg_set_pass(mus_any *ptr, off_t val) {set_env_location(ptr, val); return(val);}

static Float env_increment(mus_any *rd)
{
  if (((seg *)rd)->style == ENV_STEP)
    return(0.0);
  return(((seg *)rd)->base);
}

static void env_reset(mus_any *ptr)
{
  seg *gen = (seg *)ptr;
  gen->current_value = gen->init_y;
  gen->pass = 0;
  gen->index = 0;
  gen->rate = gen->rates[0];
  gen->power = gen->init_power;
}

static mus_any_class ENV_CLASS = {
  MUS_ENV,
  S_env,
  &free_env_gen,
  &describe_env,
  &env_equalp,
  &env_data, /* mus-data -> original breakpoints */
  0,
  &env_length,
  0,
  0, 0, 
  &env_current_value, 0,
  &env_scaler,
  0,
  &env_increment,
  0,
  &run_env,
  MUS_NOT_SPECIAL, 
  NULL,
  &env_position,
  &env_offset, &set_env_offset, 
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 
  &seg_pass, &seg_set_pass,
  0,
  0, 0, 0, 0, 0,
  &env_reset,
  0
};


/* make-env (C, scm, lisp) should use :dur, not :end, and should dispense with :start
 *   perhaps #define mus_make_env_with_dur(Brkpts, Pts, Scaler, Offset, Base, Dur) mus_make_env(Brkpts, Pts, Scaler, Offset, Base, 0.0, 0, Dur - 1, NULL)
 */

mus_any *mus_make_env(Float *brkpts, int npts, Float scaler, Float offset, Float base, Float duration, off_t start, off_t end, Float *odata)
{
  int i;
  off_t dur_in_samples;
  Float *edata;
  seg *e = NULL;
  for (i = 2; i < npts * 2; i += 2)
    if (brkpts[i - 2] > brkpts[i])
      {
	mus_error(MUS_BAD_ENVELOPE, S_make_env ": env at breakpoint %d: x axis value %f > %f", i / 2, brkpts[i - 2], brkpts[i]);
	return(NULL);
      }
  e = (seg *)clm_calloc(1, sizeof(seg), S_make_env);
  e->core = &ENV_CLASS;
  if (duration != 0.0)
    dur_in_samples = (off_t)(duration * sampling_rate);
  else dur_in_samples = (end - start + 1);
  e->init_y = offset + scaler * brkpts[1];
  e->current_value = e->init_y;
  e->rate = 0.0;
  e->offset = offset;
  e->scaler = scaler;
  e->original_offset = offset;
  e->original_scaler = scaler;
  e->base = base;
  e->end = (dur_in_samples - 1);
  e->pass = 0;
  e->index = 0; /* ? */
  if (odata)
    e->original_data = odata;
  else
    {
      e->original_data = (Float *)clm_calloc(npts * 2, sizeof(Float), "env original data");
      e->data_allocated = true;
    }
  if (e->original_data != brkpts)
    {
      memcpy((void *)(e->original_data), (void *)brkpts, npts * 2 *sizeof(Float));
      /* for (i = 0; i < npts * 2; i++) e->original_data[i] = brkpts[i]; */
    }
  if (base == 0.0)
    {
      e->style = ENV_STEP;
      dmagify_env(e, brkpts, npts, dur_in_samples, scaler);
    }
  else
    {
      if (base == 1.0)
	{
	  e->style = ENV_SEG;
	  dmagify_env(e, brkpts, npts, dur_in_samples, scaler);
	}
      else
	{
	  e->style = ENV_EXP;
	  edata = fixup_exp_env(e, brkpts, npts, offset, scaler, base);
	  if (edata == NULL)
	    {
	      if ((e->original_data) && (e->data_allocated)) FREE(e->original_data);
	      FREE(e);
	      return(NULL);
	    }
	  dmagify_env(e, edata, npts, dur_in_samples, 1.0);
	  e->power = edata[1];
	  e->init_power = e->power;
	  FREE(edata);
	  e->offset -= e->scaler;
	}
    }
  e->rate = e->rates[0];
  return((mus_any *)e);
}

static void set_env_location(mus_any *ptr, off_t val)
{
  /* doesn't this ignore the original notion of a "start" time? */
  seg *gen = (seg *)ptr;
  off_t ctr = 0;
  if (gen->pass == val) return;
  if (gen->pass > val)
    mus_reset(ptr);
  else ctr = gen->pass;
  gen->pass = val;
  while ((gen->index < gen->size) && 
	 (ctr < val))
    {
      off_t passes;
      if (val > gen->passes[gen->index])
	passes = gen->passes[gen->index] - ctr;
      else passes = val - ctr;
      switch (gen->style)
	{
	case ENV_SEG: 
	  gen->current_value += (passes * gen->rate);
	  break;
	case ENV_STEP: 
	  gen->current_value = gen->rate; 
	  break;
	case ENV_EXP: 
	  gen->power += (passes * gen->rate); 
	  gen->current_value = gen->offset + (gen->scaler * exp(gen->power));
	  break;
	}
      ctr += passes;
      if (ctr < val)
	{
	  gen->index++;
	  if (gen->index < gen->size)
	    gen->rate = gen->rates[gen->index];
	}
    }
}

Float mus_env_interp(Float x, mus_any *ptr)
{
  /* the accuracy depends on the duration here -- more samples = more accurate */
  seg *gen = (seg *)ptr;
  set_env_location(ptr, (off_t)((x * (gen->end + 1)) / (gen->original_data[gen->size * 2 - 2])));
  return(gen->current_value);
}


/* ---------------- frame ---------------- */

/* frame = vector, mixer = (square) matrix, but "vector" is in use already, and "matrix" sounds too techy */

typedef struct {
  mus_any_class *core;
  int chans;
  Float *vals;
  bool data_allocated;
} mus_frame;

static int free_frame(mus_any *pt)
{
  mus_frame *ptr = (mus_frame *)pt;
  if (ptr)
    {
      if ((ptr->vals) && (ptr->data_allocated)) FREE(ptr->vals);
      FREE(ptr);
    }
  return(0);
}

#define S_frame "frame"
static char *describe_frame(mus_any *ptr)
{
  mus_frame *gen = (mus_frame *)ptr;
  char *str = NULL;
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, 
	       S_frame "[%d]: %s", 
	       gen->chans,
	       str = float_array_to_string(gen->vals, gen->chans, 0));
  if (str) FREE(str);
  return(describe_buffer);
}

bool mus_frame_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_FRAME));}

static bool equalp_frame(mus_any *p1, mus_any *p2)
{
  mus_frame *g1, *g2;
  if (p1 == p2) return(true);
  g1 = (mus_frame *)p1;
  g2 = (mus_frame *)p2;
  return(((g1->core)->type == (g2->core)->type) &&
	 (g1->chans == g2->chans) &&
	 (clm_arrays_are_equal(g1->vals, g2->vals, g1->chans)));
}

static Float run_frame(mus_any *ptr, Float arg1, Float arg2) {return(mus_frame_ref(ptr, (int)arg1));}
static Float *frame_data(mus_any *ptr) {return(((mus_frame *)ptr)->vals);}
static off_t frame_length(mus_any *ptr) {return(((mus_frame *)ptr)->chans);}
static int frame_channels(mus_any *ptr) {return(((mus_frame *)ptr)->chans);}

static void frame_reset(mus_any *ptr) 
{
  mus_frame *gen = (mus_frame *)ptr;
  mus_clear_array(gen->vals, gen->chans);
}

static mus_any_class FRAME_CLASS = {
  MUS_FRAME,
  S_frame,
  &free_frame,
  &describe_frame,
  &equalp_frame,
  &frame_data, 0,
  &frame_length, 0,
  0, 0, 0, 0,
  0, 0,
  0, 0,
  &run_frame,
  MUS_NOT_SPECIAL, 
  NULL,
  &frame_channels,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &frame_reset,
  0
};

mus_any *mus_make_empty_frame(int chans)
{
  mus_frame *nf;
  if (chans <= 0) return(NULL);
  nf = (mus_frame *)clm_calloc(1, sizeof(mus_frame), S_make_frame);
  nf->core = &FRAME_CLASS;
  nf->chans = chans;
  nf->vals = (Float *)clm_calloc(chans, sizeof(Float), "frame data");
  nf->data_allocated = true;
  return((mus_any *)nf);
}

mus_any *mus_make_frame_with_data(int chans, Float *data)
{
  /* for CLM */
  mus_frame *nf;
  if (chans <= 0) return(NULL);
  nf = (mus_frame *)clm_calloc(1, sizeof(mus_frame), S_make_frame);
  nf->core = &FRAME_CLASS;
  nf->chans = chans;
  nf->vals = data;
  nf->data_allocated = false;
  return((mus_any *)nf);
}

mus_any *mus_make_frame(int chans, ...)
{
  if (chans <= 0)
    mus_error(MUS_ARG_OUT_OF_RANGE, S_make_frame ": chans: %d", chans);
  else
    {
      mus_frame *nf = NULL;
      nf = (mus_frame *)mus_make_empty_frame(chans);
      if (nf)
	{
	  int i;
	  va_list ap;
	  va_start(ap, chans);
	  for (i = 0; i < chans; i++)
	    nf->vals[i] = (Float)(va_arg(ap, double)); /* float not safe here apparently */
	  va_end(ap);
	  return((mus_any *)nf);
	}
    }
  return(NULL);
}

mus_any *mus_frame_add(mus_any *uf1, mus_any *uf2, mus_any *ures)
{
  int chans, i;
  mus_frame *f1 = (mus_frame *)uf1;
  mus_frame *f2 = (mus_frame *)uf2;
  mus_frame *res = (mus_frame *)ures;
  chans = f1->chans;
  if (f2->chans < chans) chans = f2->chans;
  if (res)
    {
      if (res->chans < chans) chans = res->chans;
    }
  else res = (mus_frame *)mus_make_empty_frame(chans);
  for (i = 0; i < chans; i++) 
    res->vals[i] = f1->vals[i] + f2->vals[i];
  return((mus_any *)res);
}

mus_any *mus_frame_multiply(mus_any *uf1, mus_any *uf2, mus_any *ures)
{
  int chans, i;
  mus_frame *f1 = (mus_frame *)uf1;
  mus_frame *f2 = (mus_frame *)uf2;
  mus_frame *res = (mus_frame *)ures;
  chans = f1->chans;
  if (f2->chans < chans) chans = f2->chans;
  if (res)
    {
      if (res->chans < chans) 
	chans = res->chans;
    }
  else res = (mus_frame *)mus_make_empty_frame(chans);
  for (i = 0; i < chans; i++) 
    res->vals[i] = f1->vals[i] * f2->vals[i];
  return((mus_any *)res);
}

mus_any *mus_frame_scale(mus_any *uf1, Float scl, mus_any *ures)
{
  int chans, i;
  mus_frame *f1 = (mus_frame *)uf1;
  mus_frame *res = (mus_frame *)ures;
  chans = f1->chans;
  if (res)
    {
      if (res->chans < chans) 
	chans = res->chans;
    }
  else res = (mus_frame *)mus_make_empty_frame(chans);
  for (i = 0; i < chans; i++) 
    res->vals[i] = f1->vals[i] * scl;
  return((mus_any *)res);
}

mus_any *mus_frame_offset(mus_any *uf1, Float offset, mus_any *ures)
{
  int chans, i;
  mus_frame *f1 = (mus_frame *)uf1;
  mus_frame *res = (mus_frame *)ures;
  chans = f1->chans;
  if (res)
    {
      if (res->chans < chans) chans = res->chans;
    }
  else res = (mus_frame *)mus_make_empty_frame(chans);
  for (i = 0; i < chans; i++) 
    res->vals[i] = f1->vals[i] + offset;
  return((mus_any *)res);
}

Float mus_frame_ref(mus_any *uf, int chan) 
{
  mus_frame *f = (mus_frame *)uf;
  if ((chan >= 0) && (chan < f->chans))
    return(f->vals[chan]);
  return((Float)mus_error(MUS_ARG_OUT_OF_RANGE, 
			  S_frame_ref ": invalid chan: %d (frame has %d chan%s)",
			  chan, f->chans, (f->chans == 1) ? "" : "s"));
}

Float mus_frame_set(mus_any *uf, int chan, Float val) 
{
  mus_frame *f = (mus_frame *)uf;
  if ((chan >= 0) && (chan < f->chans))
    f->vals[chan] = val; 
  else mus_error(MUS_ARG_OUT_OF_RANGE, 
		 S_frame_set ": invalid chan: %d (frame has %d chan%s)",
		 chan, f->chans, (f->chans == 1) ? "" : "s");
  return(val);
}


/* ---------------- mixer ---------------- */

typedef struct {
  mus_any_class *core;
  int chans;
  Float **vals;
  bool data_allocated;
} mus_mixer;

static int free_mixer(mus_any *pt)
{
  mus_mixer *ptr = (mus_mixer *)pt;
  if (ptr)
    {
      if (ptr->vals)
	{
	  int i;
	  if (ptr->data_allocated)
	    for (i = 0; i < ptr->chans; i++) 
	      FREE(ptr->vals[i]);
	  FREE(ptr->vals);
	}
      FREE(ptr);
    }
  return(0);
}

static void mixer_reset(mus_any *ptr) 
{
  int i;
  mus_mixer *gen = (mus_mixer *)ptr;
  for (i = 0; i < gen->chans; i++) 
    mus_clear_array(gen->vals[i], gen->chans);
}

#define S_mixer "mixer"
static char *describe_mixer(mus_any *ptr)
{
  mus_mixer *gen = (mus_mixer *)ptr;
  char *str;
  int i, j, lim;
  lim = mus_array_print_length();
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, S_mixer ": chans: %d, [\n ", gen->chans);
  str = (char *)CALLOC(64, sizeof(char));
  if (gen->chans < lim) lim = gen->chans;
  for (i = 0; i < lim; i++)
    for (j = 0; j < lim; j++)
      {
	mus_snprintf(str, 64, "%.3f%s%s%s",
		     gen->vals[i][j],
		     ((j == (lim - 1)) && (lim < gen->chans)) ? "..." : "",
		     (j == (lim - 1)) ? "\n" : "",
		     ((i == (lim - 1)) && (j == (lim - 1))) ? "]" : " ");
	if ((strlen(describe_buffer) + strlen(str)) < (DESCRIBE_BUFFER_SIZE - 1))
	  strcat(describe_buffer, str);
	else break;
      }
  FREE(str);
  return(describe_buffer);
}

bool mus_mixer_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_MIXER));}

static bool equalp_mixer(mus_any *p1, mus_any *p2)
{
  int i;
  mus_mixer *g1, *g2;
  if (p1 == p2) return(true);
  if ((p1 == NULL) || (p2 == NULL)) return(false); /* is this needed? */
  g1 = (mus_mixer *)p1;
  g2 = (mus_mixer *)p2;
  if (((g1->core)->type != (g2->core)->type) ||
      (g1->chans != g2->chans))
    return(false);
  for (i = 0; i < g1->chans; i++)
    if (!(clm_arrays_are_equal(g1->vals[i], g2->vals[i], g1->chans)))
      return(false);
  return(true);
}

static Float run_mixer(mus_any *ptr, Float arg1, Float arg2) {return(mus_mixer_ref(ptr, (int)arg1, (int)arg2));}
static off_t mixer_length(mus_any *ptr) {return(((mus_mixer *)ptr)->chans);}
static Float *mixer_data(mus_any *ptr) {return((Float *)(((mus_mixer *)ptr)->vals));}
static int mixer_channels(mus_any *ptr) {return(((mus_mixer *)ptr)->chans);}

static mus_any_class MIXER_CLASS = {
  MUS_MIXER,
  S_mixer,
  &free_mixer,
  &describe_mixer,
  &equalp_mixer,
  &mixer_data, 0,
  &mixer_length,
  0,
  0, 0, 0, 0,
  0, 0,
  0, 0,
  &run_mixer,
  MUS_NOT_SPECIAL, 
  NULL,
  &mixer_channels,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &mixer_reset,
  0
};

mus_any *mus_make_mixer_with_data(int chans, Float *data)
{
  /* for CLM */
  mus_mixer *nf;
  int i;
  if (chans <= 0) return(NULL);
  nf = (mus_mixer *)clm_calloc(1, sizeof(mus_mixer), S_make_mixer);
  nf->core = &MIXER_CLASS;
  nf->chans = chans;
  nf->vals = (Float **)clm_calloc(chans, sizeof(Float *), "mixer data");
  for (i = 0; i < chans; i++)
    nf->vals[i] = (Float *)(data + (i * chans));
  nf->data_allocated = false;
  return((mus_any *)nf);
}

mus_any *mus_make_empty_mixer(int chans)
{
  mus_mixer *nf = NULL;
  int i;
  nf = (mus_mixer *)clm_calloc(1, sizeof(mus_mixer), S_make_mixer);
  nf->core = &MIXER_CLASS;
  nf->chans = chans;
  nf->vals = (Float **)clm_calloc(chans, sizeof(Float *), "mixer data");
  for (i = 0; i < chans; i++)
    nf->vals[i] = (Float *)clm_calloc(chans, sizeof(Float), "mixer data");
  nf->data_allocated = true;
  return((mus_any *)nf);
}

mus_any *mus_make_scalar_mixer(int chans, Float scalar)
{
  mus_mixer *mx = NULL;
  if (chans <= 0)
    mus_error(MUS_ARG_OUT_OF_RANGE, S_make_scalar_mixer ": chans: %d", chans);
  else
    {
      int i;
      mx = (mus_mixer *)mus_make_empty_mixer(chans);
      if (mx) for (i = 0; i < chans; i++) mx->vals[i][i] = scalar;
    }
  return((mus_any *)mx);
}

mus_any *mus_make_identity_mixer(int chans)
{
  return(mus_make_scalar_mixer(chans, 1.0));
}

mus_any *mus_make_mixer(int chans, ...)
{
  mus_mixer *mx = NULL;
  if (chans <= 0) 
    mus_error(MUS_ARG_OUT_OF_RANGE, S_make_mixer ": chans: %d", chans);
  else
    {
      mx = (mus_mixer *)mus_make_empty_mixer(chans);
      if (mx) 
	{
	  int i, j;
	  va_list ap;
	  va_start(ap, chans);
	  for (i = 0; i < chans; i++)
	    for (j = 0; j < chans; j++)
	      mx->vals[i][j] = (Float)(va_arg(ap, double));
	  va_end(ap);
	}
    }
  return((mus_any *)mx);
}

Float mus_mixer_ref(mus_any *uf, int in, int out) 
{
  mus_mixer *f = (mus_mixer *)uf;
  if ((in >= 0) && (in < f->chans) &&
      (out >= 0) && (out < f->chans))
    return(f->vals[in][out]);
  mus_error(MUS_ARG_OUT_OF_RANGE, 
	    S_mixer_ref ": invalid chan: %d (mixer has %d chan%s)",
	    ((in < 0) || (in >= f->chans)) ? in : out,
	    f->chans,
	    (f->chans == 1) ? "" : "s");
  return(0.0);
}

Float mus_mixer_set(mus_any *uf, int in, int out, Float val) 
{
  mus_mixer *f = (mus_mixer *)uf;
  if ((in >= 0) && (in < f->chans) &&
      (out >= 0) && (out < f->chans))
    f->vals[in][out] = val; 
  else mus_error(MUS_ARG_OUT_OF_RANGE, 
		 S_mixer_set ": invalid chan: %d (mixer has %d chan%s)",
		 ((in < 0) || (in >= f->chans)) ? in : out,
		 f->chans,
		 (f->chans == 1) ? "" : "s");
  return(val);
}

static mus_any *frame_to_frame_right(mus_any *arg1, mus_any *arg2, mus_any *arg_out)
{
  /* (frame->frame frame mixer frame) = frame * mixer -> frame -- this is the original form */
  mus_mixer *mix = (mus_mixer *)arg2;
  mus_frame *frame = (mus_frame *)arg1;
  mus_frame *out = (mus_frame *)arg_out;
  int i, in_chans, out_chans;
  in_chans = frame->chans;
  if (in_chans > mix->chans) 
    in_chans = mix->chans;
  out_chans = mix->chans;
  if (out)
    {
      if (out->chans < out_chans) 
	out_chans = out->chans;
    }
  else out = (mus_frame *)mus_make_empty_frame(out_chans);
  for (i = 0; i < out_chans; i++)
    {
      int j;
      out->vals[i] = 0.0;
      for (j = 0; j < in_chans; j++)
	out->vals[i] += (frame->vals[j] * mix->vals[j][i]);
    }
  return((mus_any *)out);
}

static mus_any *frame_to_frame_left(mus_any *arg1, mus_any *arg2, mus_any *arg_out)
{
  /* (frame->frame mixer frame frame) = mixer * frame -> frame */
  mus_mixer *mix = (mus_mixer *)arg1;
  mus_frame *frame = (mus_frame *)arg2;
  mus_frame *out = (mus_frame *)arg_out;
  int i, in_chans, out_chans;
  in_chans = frame->chans;
  if (in_chans > mix->chans) 
    in_chans = mix->chans;
  out_chans = mix->chans;
  if (out)
    {
      if (out->chans < out_chans) 
	out_chans = out->chans;
    }
  else out = (mus_frame *)mus_make_empty_frame(out_chans);
  for (i = 0; i < out_chans; i++)
    {
      int j;
      out->vals[i] = 0.0;
      for (j = 0; j < in_chans; j++)
	out->vals[i] += (mix->vals[i][j] * frame->vals[j]);
    }
  return((mus_any *)out);
}

mus_any *mus_frame_to_frame(mus_any *arg1, mus_any *arg2, mus_any *arg_out)
{
  if (mus_mixer_p(arg2))
    return(frame_to_frame_right(arg1, arg2, arg_out));
  return(frame_to_frame_left(arg1, arg2, arg_out));
}

mus_any *mus_sample_to_frame(mus_any *f, Float in, mus_any *uout)
{
  int i, chans;
  mus_frame *out = (mus_frame *)uout;
  if (mus_frame_p(f))
    {
      mus_frame *fr;
      fr = (mus_frame *)f;
      chans = fr->chans;
      if (out)
	{
	  if (out->chans < chans) 
	    chans = out->chans;
	}
      else out = (mus_frame *)mus_make_empty_frame(chans);
      for (i = 0; i < chans; i++)
	out->vals[i] = (in * fr->vals[i]);
      /* was += here and below? */
    }
  else
    {
      if (mus_mixer_p(f))
	{
	  mus_mixer *mx;
	  mx = (mus_mixer *)f;
	  chans = mx->chans;
	  if (out)
	    {
	      if (out->chans < chans) 
		chans = out->chans;
	    }
	  else out = (mus_frame *)mus_make_empty_frame(chans);
	  for (i = 0; i < chans; i++)
	    out->vals[i] = (in * mx->vals[0][i]);
	}
      else mus_error(MUS_ARG_OUT_OF_RANGE, S_sample_to_frame ": gen not frame or mixer");
    }
  return((mus_any *)out);
}

Float mus_frame_to_sample(mus_any *f, mus_any *uin)
{
  int i, chans;
  mus_frame *in = (mus_frame *)uin;
  Float val = 0.0;
  if (mus_frame_p(f))
    {
      mus_frame *fr;
      fr = (mus_frame *)f;
      chans = in->chans;
      if (fr->chans < chans) 
	chans = fr->chans;
      for (i = 0; i < chans; i++)
	val += (in->vals[i] * fr->vals[i]); 
    }
  else
    {
      if (mus_mixer_p(f))
	{
	  mus_mixer *mx;
	  mx = (mus_mixer *)f;
	  chans = in->chans;
	  if (mx->chans < chans) 
	    chans = mx->chans;
	  for (i = 0; i < chans; i++)
	    val += (in->vals[i] * mx->vals[i][0]);
	}
      else mus_error(MUS_ARG_OUT_OF_RANGE, S_frame_to_sample ": gen not frame or mixer");
    }
  return(val);
}

mus_any *mus_mixer_add(mus_any *uf1, mus_any *uf2, mus_any *ures)
{
  int i, j, chans;
  mus_mixer *f1 = (mus_mixer *)uf1;
  mus_mixer *f2 = (mus_mixer *)uf2;
  mus_mixer *res = (mus_mixer *)ures;
  chans = f1->chans;
  if (f2->chans < chans) 
    chans = f2->chans;
  if (res)
    {
      if (res->chans < chans) 
	chans = res->chans;
    }
  else res = (mus_mixer *)mus_make_empty_mixer(chans);
  for (i = 0; i < chans; i++)
    for (j = 0; j < chans; j++)
      res->vals[i][j] = f1->vals[i][j] + f2->vals[i][j];
  return((mus_any *)res);
}

mus_any *mus_mixer_multiply(mus_any *uf1, mus_any *uf2, mus_any *ures)
{
  int i, j, k, chans;
  mus_mixer *f1 = (mus_mixer *)uf1;
  mus_mixer *f2 = (mus_mixer *)uf2;
  mus_mixer *res = (mus_mixer *)ures;
  chans = f1->chans;
  if (f2->chans < chans) 
    chans = f2->chans;
  if (res)
    {
      if (res->chans < chans) 
	chans = res->chans;
    }
  else res = (mus_mixer *)mus_make_empty_mixer(chans);
  for (i = 0; i < chans; i++)
    for (j = 0; j < chans; j++)
      {
	res->vals[i][j] = 0.0;
	for (k = 0; k < chans; k++) 
	  res->vals[i][j] += (f1->vals[i][k] * f2->vals[k][j]);
      }
  return((mus_any *)res);
}

mus_any *mus_mixer_scale(mus_any *uf1, Float scaler, mus_any *ures)
{
  int i, j, chans;
  mus_mixer *f1 = (mus_mixer *)uf1;
  mus_mixer *res = (mus_mixer *)ures;
  chans = f1->chans;
  if (res)
    {
      if (res->chans < chans) 
	chans = res->chans;
    }
  else res = (mus_mixer *)mus_make_empty_mixer(chans);
  for (i = 0; i < chans; i++)
    for (j = 0; j < chans; j++)
      res->vals[i][j] = f1->vals[i][j] * scaler;
  return((mus_any *)res);
}

mus_any *mus_mixer_offset(mus_any *uf1, Float offset, mus_any *ures)
{
  int i, j, chans;
  mus_mixer *f1 = (mus_mixer *)uf1;
  mus_mixer *res = (mus_mixer *)ures;
  chans = f1->chans;
  if (res)
    {
      if (res->chans < chans) 
	chans = res->chans;
    }
  else res = (mus_mixer *)mus_make_empty_mixer(chans);
  for (i = 0; i < chans; i++)
    for (j = 0; j < chans; j++)
      res->vals[i][j] = f1->vals[i][j] + offset;
  return((mus_any *)res);
}



/* ---------------- input/output ---------------- */

static Float mus_read_sample(mus_any *fd, off_t frame, int chan) 
{
  if ((check_gen(fd, "mus-read-sample")) &&
      ((fd->core)->read_sample))
    return(((*(fd->core)->read_sample))(fd, frame, chan));
  return((Float)mus_error(MUS_NO_SAMPLE_INPUT, 
			  "can't find %s's sample input function", 
			  mus_name(fd)));
}

static Float mus_write_sample(mus_any *fd, off_t frame, int chan, Float samp) 
{
  if ((check_gen(fd, "mus-write-sample")) &&
      ((fd->core)->write_sample))
    return(((*(fd->core)->write_sample))(fd, frame, chan, samp));
  return((Float)mus_error(MUS_NO_SAMPLE_OUTPUT, 
			  "can't find %s's sample output function", 
			  mus_name(fd)));
}

char *mus_file_name(mus_any *gen)
{
  if ((check_gen(gen, S_mus_file_name)) &&
      (gen->core->file_name))
    return((*(gen->core->file_name))(gen));
  else mus_error(MUS_NO_FILE_NAME, "can't get %s's file name", mus_name(gen));
  return(NULL);
}


bool mus_input_p(mus_any *gen) 
{
  return((gen) && 
	 (gen->core->extended_type == MUS_INPUT));
}

bool mus_output_p(mus_any *gen) 
{
  return((gen) && 
	 (gen->core->extended_type == MUS_OUTPUT));
}



/* ---------------- file->sample ---------------- */

typedef struct {
  mus_any_class *core;
  int chan;
  int dir;
  off_t loc;
  char *file_name;
  int chans;
  mus_sample_t **ibufs;
  off_t data_start, data_end, file_end;
  int file_buffer_size;
} rdin;

static char *describe_file_to_sample(mus_any *ptr)
{
  rdin *gen = (rdin *)ptr;
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, 
	       S_file_to_sample ": %s", 
	       gen->file_name);
  return(describe_buffer);
}

static bool rdin_equalp(mus_any *p1, mus_any *p2) 
{
  rdin *r1 = (rdin *)p1;
  rdin *r2 = (rdin *)p2;
  return ((p1 == p2) ||
	  ((r1) && (r2) &&
	   (r1->core->type == r2->core->type) &&
	   (r1->chan == r2->chan) &&
	   (r1->loc == r2->loc) &&
	   (r1->dir == r2->dir) &&
	   (r1->file_name) &&
	   (r2->file_name) &&
	   (strcmp(r1->file_name, r2->file_name) == 0)));
}

static int free_file_to_sample(mus_any *p) 
{
  rdin *ptr = (rdin *)p;
  if (ptr) 
    {
      if (ptr->core->end) ((*ptr->core->end))(p);
      FREE(ptr->file_name);
      FREE(ptr);
    }
  return(0);
}

static off_t file_to_sample_length(mus_any *ptr) {return((((rdin *)ptr)->file_end));}
static int file_to_sample_channels(mus_any *ptr) {return((int)(((rdin *)ptr)->chans));}

static Float file_to_sample_increment(mus_any *rd) {return((Float)(((rdin *)rd)->dir));}
static Float file_to_sample_set_increment(mus_any *rd, Float val) {((rdin *)rd)->dir = (int)val; return(val);}

static char *file_to_sample_file_name(mus_any *ptr) {return(((rdin *)ptr)->file_name);}
static void no_reset(mus_any *ptr) {}
static Float file_sample(mus_any *ptr, off_t samp, int chan);
static int file_to_sample_end(mus_any *ptr);
static Float run_file_to_sample(mus_any *ptr, Float arg1, Float arg2) {return(file_sample(ptr, (int)arg1, (int)arg2));} /* mus_read_sample here? */

static mus_any_class FILE_TO_SAMPLE_CLASS = {
  MUS_FILE_TO_SAMPLE,
  S_file_to_sample,
  &free_file_to_sample,
  &describe_file_to_sample,
  &rdin_equalp,
  0, 0, 
  &file_to_sample_length, 0,
  0, 0, 0, 0,
  0, 0,
  &file_to_sample_increment, 
  &file_to_sample_set_increment,
  &run_file_to_sample,
  MUS_INPUT,
  NULL,
  &file_to_sample_channels,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  &file_sample,
  0,
  &file_to_sample_file_name,
  &file_to_sample_end,
  0, /* location */
  0, /* set_location */
  0, /* channel */
  0, 0, 0, 0, 0,
  &no_reset,
  0
};

static Float file_sample(mus_any *ptr, off_t samp, int chan)
{
  /* check in-core buffer bounds,
   * if needed read new buffer (taking into account dir)
   * return Float at samp (frame) 
   */
  rdin *gen = (rdin *)ptr;
  if ((samp < 0) || 
      (samp >= gen->file_end) ||
      (chan >= gen->chans))
    return(0.0);

  if ((samp > gen->data_end) || 
      (samp < gen->data_start))
    {
      int fd, i;
      off_t newloc;
      /* read in first buffer start either at samp (dir > 0) or samp-bufsize (dir < 0) */
      if (gen->dir >= 0) 
	newloc = samp; 
      else newloc = (int)(samp - (gen->file_buffer_size * .75));
      /* The .75 in the backwards read is trying to avoid reading the full buffer on 
       * nearly every sample when we're oscillating around the
       * nominal buffer start/end (in src driven by an oscil for example)
       */
      if (newloc < 0) newloc = 0;
      gen->data_start = newloc;
      gen->data_end = newloc + gen->file_buffer_size - 1;
      fd = mus_sound_open_input(gen->file_name);
      if (fd == -1)
	return((Float)mus_error(MUS_CANT_OPEN_FILE, 
				"open(%s) -> %s", 
				gen->file_name, STRERROR(errno)));
      else
	{ 
	  if (gen->ibufs == NULL) 
	    {
	      gen->ibufs = (mus_sample_t **)clm_calloc(gen->chans, sizeof(mus_sample_t *), "input buffers");
	      for (i = 0; i < gen->chans; i++)
		gen->ibufs[i] = (mus_sample_t *)clm_calloc(gen->file_buffer_size, sizeof(mus_sample_t), "input buffer");
	    }
	  mus_file_seek_frame(fd, gen->data_start);

	  if ((gen->data_start + gen->file_buffer_size) >= gen->file_end)
	    mus_file_read_chans(fd, 0, gen->file_end - gen->data_start - 1, gen->chans, gen->ibufs, gen->ibufs);
	  else mus_file_read_chans(fd, 0, gen->file_buffer_size - 1, gen->chans, gen->ibufs, gen->ibufs);

	  /* we have to check file_end here because chunked files can have trailing chunks containing
	   *   comments or whatever.  io.c (mus_file_read_*) merely calls read, and translates bytes --
	   *   if it gets fewer than requested, it zeros from the point where the incoming file data stopped,
	   *   but that can be far beyond the actual end of the sample data!  It is at this level that
	   *   we know how much data is actually supposed to be in the file. 
	   *
	   * Also, file_end is the number of frames, so we should not read samp # file_end (see above).
	   */

	  mus_sound_close_input(fd);
	  if (gen->data_end > gen->file_end) gen->data_end = gen->file_end;
	}
    }
  return((Float)MUS_SAMPLE_TO_FLOAT(gen->ibufs[chan][samp - gen->data_start]));
}

static int file_to_sample_end(mus_any *ptr)
{
  rdin *gen = (rdin *)ptr;
  if (gen)
    {
      if (gen->ibufs)
	{
	  int i;
	  for (i = 0; i < gen->chans; i++)
	    if (gen->ibufs[i]) 
	      FREE(gen->ibufs[i]);
	  FREE(gen->ibufs);
	  gen->ibufs = NULL;
	}
    }
  return(0);
}

bool mus_file_to_sample_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_FILE_TO_SAMPLE));}

mus_any *mus_make_file_to_sample_with_buffer_size(const char *filename, int buffer_size)
{
  rdin *gen;
  if (filename == NULL)
    mus_error(MUS_NO_FILE_NAME_PROVIDED, S_make_file_to_sample " requires a file name");
  else
    {
      gen = (rdin *)clm_calloc(1, sizeof(rdin), S_make_file_to_sample);
      gen->core = &FILE_TO_SAMPLE_CLASS;
      gen->file_buffer_size = buffer_size;
      gen->file_name = (char *)clm_calloc(strlen(filename) + 1, sizeof(char), S_file_to_sample " filename");
      strcpy(gen->file_name, filename);
      gen->data_end = -1; /* force initial read */
      gen->chans = mus_sound_chans(gen->file_name);
      if (gen->chans <= 0) mus_error(MUS_NO_CHANNELS, "%s chans: %d", filename, gen->chans);
      gen->file_end = mus_sound_frames(gen->file_name);
      if (gen->file_end < 0) mus_error(MUS_NO_LENGTH, "%s frames: " OFF_TD, filename, gen->file_end);
      return((mus_any *)gen);
    }
  return(NULL);
}

mus_any *mus_make_file_to_sample(const char *filename)
{
  return(mus_make_file_to_sample_with_buffer_size(filename, clm_file_buffer_size));
}

Float mus_file_to_sample(mus_any *ptr, off_t samp, int chan)
{
  return(mus_read_sample(ptr, samp, chan));
}



/* ---------------- readin ---------------- */

/* readin reads only the desired channel and increments the location by the direction
 *   it inherits from and specializes the file_to_sample class 
 */

static char *describe_readin(mus_any *ptr)
{
  rdin *gen = (rdin *)ptr;
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, 
	       S_readin ": %s[chan %d], loc: " OFF_TD ", dir: %d", 
	       gen->file_name, gen->chan, gen->loc, gen->dir);
  return(describe_buffer);
}

static int free_readin(mus_any *p) 
{
  rdin *ptr = (rdin *)p;
  if (ptr) 
    {
      if (ptr->core->end) ((*ptr->core->end))(p);
      FREE(ptr->file_name);
      FREE(ptr);
    }
  return(0);
}

static Float run_readin(mus_any *ptr, Float unused1, Float unused2) {return(mus_readin(ptr));}

static Float rd_increment(mus_any *ptr) {return((Float)(((rdin *)ptr)->dir));}
static Float rd_set_increment(mus_any *ptr, Float val) {((rdin *)ptr)->dir = (int)val; return(val);}

static off_t rd_location(mus_any *rd) {return(((rdin *)rd)->loc);}
static off_t rd_set_location(mus_any *rd, off_t loc) {((rdin *)rd)->loc = loc; return(loc);}

static int rd_channel(mus_any *rd) {return(((rdin *)rd)->chan);}

static mus_any_class READIN_CLASS = {
  MUS_READIN,
  S_readin,
  &free_readin,
  &describe_readin,
  &rdin_equalp,
  0, 0, 
  &file_to_sample_length, 0,
  0, 0, 0, 0,
  0, 0,
  &rd_increment,
  &rd_set_increment,
  &run_readin,
  MUS_INPUT,
  NULL,
  &file_to_sample_channels,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  &file_sample,
  0,
  &file_to_sample_file_name,
  &file_to_sample_end,
  &rd_location,
  &rd_set_location,
  &rd_channel,
  0, 0, 0, 0, 0,
  &no_reset,
  0
};

bool mus_readin_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_READIN));}

mus_any *mus_make_readin_with_buffer_size(const char *filename, int chan, off_t start, int direction, int buffer_size)
{
  rdin *gen;
  gen = (rdin *)mus_make_file_to_sample(filename);
  if (gen)
    {
      gen->core = &READIN_CLASS;
      gen->loc = start;
      gen->dir = direction;
      gen->chan = chan;
      gen->file_buffer_size = buffer_size;
      gen->ibufs = (mus_sample_t **)clm_calloc(gen->chans, sizeof(mus_sample_t *), "readin buffers");
      gen->ibufs[chan] = (mus_sample_t *)clm_calloc(gen->file_buffer_size, sizeof(mus_sample_t), "readin buffer");
      return((mus_any *)gen);
    }
  return(NULL);
}

Float mus_readin(mus_any *ptr)
{
  Float res;
  rdin *rd = (rdin *)ptr;
  res = mus_file_to_sample(ptr, rd->loc, rd->chan);
  rd->loc += rd->dir;
  return(res);
}

off_t mus_set_location(mus_any *gen, off_t loc)
{
  if ((check_gen(gen, S_setB S_mus_location)) &&
      (gen->core->set_location))
    return((*(gen->core->set_location))(gen, loc));
  return((off_t)mus_error(MUS_NO_LOCATION, "can't set %s's location", mus_name(gen)));
}



/* ---------------- in-any ---------------- */

Float mus_in_any(off_t samp, int chan, mus_any *IO)
{
  if (IO) return(mus_file_to_sample(IO, samp, chan));
  return(0.0);
}


/* ---------------- file->frame ---------------- */

/* also built on file->sample */

static char *describe_file_to_frame(mus_any *ptr)
{
  rdin *gen = (rdin *)ptr;
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, 
	       S_file_to_frame ": %s", 
	       gen->file_name);
  return(describe_buffer);
}

static Float run_file_to_frame(mus_any *ptr, Float arg1, Float arg2) {mus_error(MUS_NO_RUN, "no run method for file->frame"); return(0.0);}

static mus_any_class FILE_TO_FRAME_CLASS = {
  MUS_FILE_TO_FRAME,
  S_file_to_frame,
  &free_file_to_sample,
  &describe_file_to_frame,
  &rdin_equalp,
  0, 0, 
  &file_to_sample_length, 0,
  0, 0, 0, 0,
  0, 0,
  0, 0,
  &run_file_to_frame,
  MUS_INPUT,
  NULL,
  &file_to_sample_channels,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  &file_sample,
  0,
  &file_to_sample_file_name,
  &file_to_sample_end,
  0, /* location */
  0, /* set_location */
  0, /* channel */
  0, 0, 0, 0, 0,
  &no_reset,
  0
};

mus_any *mus_make_file_to_frame_with_buffer_size(const char *filename, int buffer_size)
{
  rdin *gen;
  gen = (rdin *)mus_make_file_to_sample_with_buffer_size(filename, buffer_size);
  if (gen) 
    {
      gen->core = &FILE_TO_FRAME_CLASS;
      return((mus_any *)gen);
    }
  return(NULL);
}

mus_any *mus_make_file_to_frame(const char *filename)
{
  return(mus_make_file_to_frame_with_buffer_size(filename, clm_file_buffer_size));
}

bool mus_file_to_frame_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_FILE_TO_FRAME));}

mus_any *mus_file_to_frame(mus_any *ptr, off_t samp, mus_any *uf)
{
  mus_frame *f;
  rdin *gen = (rdin *)ptr;
  int i;
  if (uf == NULL) 
    f = (mus_frame *)mus_make_empty_frame(gen->chans); 
  else f = (mus_frame *)uf;
  for (i = 0; i < gen->chans; i++) 
    f->vals[i] = mus_file_to_sample(ptr, samp, i);
  return((mus_any *)f);
}


/* ---------------- sample->file ---------------- */

/* in all output functions, the assumption is that we're adding to whatever already exists */
/* also, the "end" methods need to flush the output buffer */

typedef struct {
  mus_any_class *core;
  int chan;
  off_t loc;
  char *file_name;
  int chans;
  mus_sample_t **obufs;
  off_t data_start, data_end;
  off_t out_end;
  int output_data_format;
  int output_header_type;
} rdout;

static char *describe_sample_to_file(mus_any *ptr)
{
  rdout *gen = (rdout *)ptr;
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, 
	       S_sample_to_file ": %s", 
	       gen->file_name);
  return(describe_buffer);
}

static bool sample_to_file_equalp(mus_any *p1, mus_any *p2) {return(p1 == p2);}

static int free_sample_to_file(mus_any *p) 
{
  rdout *ptr = (rdout *)p;
  if (ptr) 
    {
      if (ptr->core->end) ((*ptr->core->end))(p);
      FREE(ptr->file_name);
      FREE(ptr);
    }
  return(0);
}

static int sample_to_file_channels(mus_any *ptr) {return((int)(((rdout *)ptr)->chans));}

static off_t bufferlen(mus_any *ptr) {return(clm_file_buffer_size);}
static off_t set_bufferlen(mus_any *ptr, off_t len) {clm_file_buffer_size = (int)len; return(len);}
static char *sample_to_file_file_name(mus_any *ptr) {return(((rdout *)ptr)->file_name);}

static Float sample_file(mus_any *ptr, off_t samp, int chan, Float val);
static int sample_to_file_end(mus_any *ptr);

static Float run_sample_to_file(mus_any *ptr, Float arg1, Float arg2) {mus_error(MUS_NO_RUN, "no run method for sample->file"); return(0.0);}

static mus_any_class SAMPLE_TO_FILE_CLASS = {
  MUS_SAMPLE_TO_FILE,
  S_sample_to_file,
  &free_sample_to_file,
  &describe_sample_to_file,
  &sample_to_file_equalp,
  0, 0, 
  &bufferlen, &set_bufferlen, /* does this have any effect on the current gen? */
  0, 0, 0, 0,
  0, 0,
  0, 0,
  &run_sample_to_file,
  MUS_OUTPUT,
  NULL,
  &sample_to_file_channels,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0,
  &sample_file,
  &sample_to_file_file_name,
  &sample_to_file_end,
  0, 0, 0,
  0, 0, 0, 0, 0,
  &no_reset,
  0
};

static int *data_format_zero = NULL;

static void flush_buffers(rdout *gen)
{
  int fd;
  if ((gen->obufs == NULL) || (mus_file_probe(gen->file_name) == 0))
    return; /* can happen if output abandoned, then later mus_free called via GC sweep */
  fd = mus_sound_open_input(gen->file_name);
  if (fd == -1)
    {
      fd = mus_sound_open_output(gen->file_name, 
				 (int)sampling_rate, 
				 gen->chans, 
				 gen->output_data_format,
				 gen->output_header_type, 
				 NULL);
      if (fd == -1)
	mus_error(MUS_CANT_OPEN_FILE, 
		  "open(%s) -> %s", 
		  gen->file_name, STRERROR(errno));
      else
	{
	  mus_file_write(fd, 0, gen->out_end, gen->chans, gen->obufs);
	  mus_sound_close_output(fd, 
				 (gen->out_end + 1) * gen->chans * mus_bytes_per_sample(mus_sound_data_format(gen->file_name)));	  
	}
    }
  else
    {
      mus_sample_t **addbufs;
      int i, j, hdrfrm, hdrtyp;
      off_t size, hdrend, num, last;
      hdrend = mus_sound_data_location(gen->file_name);
      hdrfrm = mus_sound_data_format(gen->file_name);
      hdrtyp = mus_sound_header_type(gen->file_name);
      size = mus_sound_frames(gen->file_name);
      addbufs = (mus_sample_t **)clm_calloc(gen->chans, sizeof(mus_sample_t *), "output buffers");
      for (i = 0; i < gen->chans; i++) 
	addbufs[i] = (mus_sample_t *)clm_calloc(clm_file_buffer_size, sizeof(mus_sample_t), "output buffer");
      mus_file_seek_frame(fd, gen->data_start);
      num = gen->out_end - gen->data_start;
      if (num >= clm_file_buffer_size) 
	num = clm_file_buffer_size - 1;
      mus_file_read(fd, 0, num, gen->chans, addbufs);
      mus_sound_close_input(fd);
      fd = mus_sound_reopen_output(gen->file_name, gen->chans, hdrfrm, hdrtyp, hdrend);

      if ((size < gen->data_start) &&
	  (data_format_zero[hdrfrm] != 0))
	{
	  /* we're about to create a gap in the output file.  mus_file_seek_frame calls lseek which (man lseek):
	   *
           *    "The lseek function allows the file offset to be set beyond the  end  of
           *    the existing end-of-file of the file (but this does not change the size
           *    of the file).  If data is later written at this point, subsequent reads
           *    of  the  data  in the gap return bytes of zeros (until data is actually
           *    written into the gap)."
	   *
           * but 0 bytes in a file are not interpreted as sound samples of 0 in several data formats.
	   *  for example, mus-mulaw 0 => -.98, whereas sound sample 0 is a byte of 255.
	   *  see the table at the end of this file (data_format_zero) for the other cases.
	   *
	   * So, we need to write explicit data-format 0 values in those cases where machine 0's
	   *  won't be data format 0.  data_format_zero[format] != 0 signals we have such a
	   *  case, and returns the nominal zero value.  For unsigned shorts, we also need to
	   *  take endianess into account.
	   *
	   * Since addbufs is empty here, and is of type mus_sample_t, I'll take the slightly
	   *  slower but simpler path of calling mus_file_write to handle all the translations.
	   */

	  off_t filler, current_samps;
	  filler = gen->data_start - size; /* this is in terms of frames */
	  mus_file_seek_frame(fd, size);
	  while (filler > 0)
	    {
	      if (filler > clm_file_buffer_size)
		current_samps = clm_file_buffer_size;
	      else current_samps = filler;
	      mus_file_write(fd, 0, current_samps - 1, gen->chans, addbufs);
	      filler -= current_samps;
	    }
	}

      /* fill/write output buffers with current data added to saved data (if any) */
      last = gen->out_end - gen->data_start;
      for (j = 0; j < gen->chans; j++)
	for (i = 0; i <= last; i++)
	  addbufs[j][i] += gen->obufs[j][i];
      mus_file_seek_frame(fd, gen->data_start);
      mus_file_write(fd, 0, last, gen->chans, addbufs);
      if (size <= gen->out_end) size = gen->out_end + 1;
      mus_sound_close_output(fd, size * gen->chans * mus_bytes_per_sample(hdrfrm));
      for (i = 0; i < gen->chans; i++) 
	FREE(addbufs[i]);
      FREE(addbufs);
    }
}

static Float sample_file(mus_any *ptr, off_t samp, int chan, Float val)
{
  rdout *gen = (rdout *)ptr;
  if (chan < gen->chans)
    {
      if ((samp > gen->data_end) || 
	  (samp < gen->data_start))
	{
	  int j;
	  flush_buffers(gen);
	  for (j = 0; j < gen->chans; j++)
	    memset((void *)(gen->obufs[j]), 0, clm_file_buffer_size * sizeof(mus_sample_t));
	  gen->data_start = samp;
	  gen->data_end = samp + clm_file_buffer_size - 1;
	  gen->out_end = samp;
	}
      gen->obufs[chan][samp - gen->data_start] += MUS_FLOAT_TO_SAMPLE(val);
      if (samp > gen->out_end) 
	gen->out_end = samp;
    }
  return(val);
}

#if 0
Float mus_sample_to_file_current_value(mus_any *ptr, off_t samp, int chan)
{
  /* this is only safe just after calling sample_file (mus_write_sample) */
  rdout *gen = (rdout *)ptr;
  return(MUS_SAMPLE_TO_FLOAT(gen->obufs[chan][samp - gen->data_start]));
}
#endif

static int sample_to_file_end(mus_any *ptr)
{
  rdout *gen = (rdout *)ptr;
  if ((gen) && (gen->obufs))
    {
      int i;
      flush_buffers(gen);
      for (i = 0; i < gen->chans; i++)
	if (gen->obufs[i]) 
	  FREE(gen->obufs[i]);
      FREE(gen->obufs);
      gen->obufs = NULL;
    }
  return(0);
}

bool mus_sample_to_file_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_SAMPLE_TO_FILE));}

static mus_any *mus_make_sample_to_file_with_comment_1(const char *filename, int out_chans, int out_format, int out_type, const char *comment, bool reopen)
{
  int fd;
  if (filename == NULL)
    mus_error(MUS_NO_FILE_NAME_PROVIDED, S_make_sample_to_file " requires a file name");
  else
    {
      if (reopen)
	fd = mus_sound_reopen_output(filename, out_chans, out_format, out_type, mus_sound_data_location(filename));
      else fd = mus_sound_open_output(filename, (int)sampling_rate, out_chans, out_format, out_type, comment);
      if (fd == -1)
	mus_error(MUS_CANT_OPEN_FILE, 
		  "open(%s) -> %s", 
		  filename, STRERROR(errno));
      else
	{
	  rdout *gen;
	  int i;
	  gen = (rdout *)clm_calloc(1, sizeof(rdout), "output");
	  gen->core = &SAMPLE_TO_FILE_CLASS;
	  gen->file_name = (char *)clm_calloc(strlen(filename) + 1, sizeof(char), "output filename");
	  strcpy(gen->file_name, filename);
	  gen->data_start = 0;
	  gen->data_end = clm_file_buffer_size - 1;
	  gen->out_end = 0;
	  gen->chans = out_chans;
	  gen->output_data_format = out_format;
	  gen->output_header_type = out_type;
	  gen->obufs = (mus_sample_t **)clm_calloc(gen->chans, sizeof(mus_sample_t *), "output buffers");
	  for (i = 0; i < gen->chans; i++) 
	    gen->obufs[i] = (mus_sample_t *)clm_calloc(clm_file_buffer_size, sizeof(mus_sample_t), "output buffer");
	  /* clear previous, if any */
	  if (mus_file_close(fd) != 0)
	    mus_error(MUS_CANT_CLOSE_FILE, 
		      "close(%d, %s) -> %s", 
		      fd, gen->file_name, STRERROR(errno));
	  return((mus_any *)gen);
	}
    }
  return(NULL);
}

mus_any *mus_continue_sample_to_file(const char *filename)
{
  return(mus_make_sample_to_file_with_comment_1(filename,
						mus_sound_chans(filename),
						mus_sound_data_format(filename),
						mus_sound_header_type(filename),
						NULL,
						true));
}

mus_any *mus_make_sample_to_file_with_comment(const char *filename, int out_chans, int out_format, int out_type, const char *comment)
{
  return(mus_make_sample_to_file_with_comment_1(filename, out_chans, out_format, out_type, comment, false));
}

/* the unchecked version of this would be sample_file(ptr, samp, chan, val) */

Float mus_sample_to_file(mus_any *ptr, off_t samp, int chan, Float val)
{
  return(mus_write_sample(ptr, samp, chan, val));
}

int mus_close_file(mus_any *ptr)
{
  rdout *gen = (rdout *)ptr;
  if ((mus_output_p(ptr)) && (gen->obufs)) sample_to_file_end(ptr);
  return(0);
}


/* ---------------- out-any ---------------- */

Float mus_out_any(off_t samp, Float val, int chan, mus_any *IO)
{
  if (IO) 
    return(mus_sample_to_file(IO, samp, chan, val));
  return(val);
}



/* ---------------- frame->file ---------------- */

static char *describe_frame_to_file(mus_any *ptr)
{
  rdout *gen = (rdout *)ptr;
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, 
	       S_frame_to_file ": %s", 
	       gen->file_name);
  return(describe_buffer);
}

static Float run_frame_to_file(mus_any *ptr, Float arg1, Float arg2) {mus_error(MUS_NO_RUN, "no run method for frame->file"); return(0.0);}

static mus_any_class FRAME_TO_FILE_CLASS = {
  MUS_FRAME_TO_FILE,
  S_frame_to_file,
  &free_sample_to_file,
  &describe_frame_to_file,
  &sample_to_file_equalp,
  0, 0,
  &bufferlen, &set_bufferlen,
  0, 0, 0, 0,
  0, 0,
  0, 0,
  &run_frame_to_file,
  MUS_OUTPUT,
  NULL,
  &sample_to_file_channels,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0,
  &sample_file,
  &sample_to_file_file_name,
  &sample_to_file_end,
  0, 0, 0,
  0, 0, 0, 0, 0,
  &no_reset,
  0
};

mus_any *mus_make_frame_to_file_with_comment(const char *filename, int chans, int out_format, int out_type, const char *comment)
{
  rdout *gen = NULL;
  gen = (rdout *)mus_make_sample_to_file_with_comment(filename, chans, out_format, out_type, comment);
  if (gen) gen->core = &FRAME_TO_FILE_CLASS;
  return((mus_any *)gen);
}

bool mus_frame_to_file_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_FRAME_TO_FILE));}

mus_any *mus_frame_to_file(mus_any *ptr, off_t samp, mus_any *udata)
{
  rdout *gen = (rdout *)ptr;
  mus_frame *data = (mus_frame *)udata;
  if (data) 
    {
      if (data->chans == 1)
	mus_sample_to_file(ptr, samp, 0, data->vals[0]);
      else
	{
	  int i, chans;
	  chans = data->chans;
	  if (gen->chans < chans) 
	    chans = gen->chans;
	  for (i = 0; i < chans; i++) 
	    mus_sample_to_file(ptr, samp, i, data->vals[i]);
	}
    }
  return((mus_any *)data);
}

mus_any *mus_continue_frame_to_file(const char *filename)
{
  rdout *gen = NULL;
  gen = (rdout *)mus_continue_sample_to_file(filename);
  if (gen) gen->core = &FRAME_TO_FILE_CLASS;
  return((mus_any *)gen);
}


/* ---------------- locsig ---------------- */

typedef struct {
  mus_any_class *core;
  mus_any *outn_writer;
  mus_any *revn_writer;
  mus_frame *outf, *revf;
  Float *outn;
  Float *revn;
  int chans, rev_chans;
  mus_interp_t type;
  Float reverb;
  void *closure;
} locs;

static bool locsig_equalp(mus_any *p1, mus_any *p2) 
{
  locs *g1 = (locs *)p1;
  locs *g2 = (locs *)p2;
  if (p1 == p2) return(true);
  return((g1) && (g2) &&
	 (g1->core->type == g2->core->type) &&
	 (g1->chans == g2->chans) &&
	 (clm_arrays_are_equal(g1->outn, g2->outn, g1->chans)) &&
	 (((bool)(g1->revn)) == ((bool)(g2->revn))) &&
	 ((!(g1->revn)) || (clm_arrays_are_equal(g1->revn, g2->revn, g1->rev_chans))));
}

static char *describe_locsig(mus_any *ptr)
{
  char *str;
  int i, lim = 16;
  locs *gen = (locs *)ptr;
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE,
	       S_locsig ": chans %d, outn: [", 
	       gen->chans);
  str = (char *)CALLOC(STR_SIZE, sizeof(char));
  if (gen->chans - 1 < lim) lim = gen->chans - 1;
  for (i = 0; i < lim; i++)
    {
      mus_snprintf(str, STR_SIZE, "%.3f ", gen->outn[i]);
      if ((strlen(describe_buffer) + strlen(str)) < (DESCRIBE_BUFFER_SIZE - 16))
	strcat(describe_buffer, str);
      else break;
    }
  if (gen->chans - 1 > lim) strcat(describe_buffer, "...");
  mus_snprintf(str, STR_SIZE, "%.3f]", gen->outn[gen->chans - 1]);
  strcat(describe_buffer, str);
  if (gen->rev_chans > 0)
    {
      strcat(describe_buffer, ", revn: [");
      lim = 16;
      if (gen->rev_chans - 1 < lim) lim = gen->rev_chans - 1;
      for (i = 0; i < lim; i++)
	{
	  mus_snprintf(str, STR_SIZE, "%.3f ", gen->revn[i]);
	  if ((strlen(describe_buffer) + strlen(str)) < (DESCRIBE_BUFFER_SIZE - 16))
	    strcat(describe_buffer, str);
	  else break;
	}
      if (gen->rev_chans - 1 > lim) strcat(describe_buffer, "...");
      mus_snprintf(str, STR_SIZE, "%.3f]", gen->revn[gen->rev_chans - 1]);
      strcat(describe_buffer, str);
    }
  mus_snprintf(str, STR_SIZE, ", interp: %s", interp_name[gen->type]);
  strcat(describe_buffer, str);
  FREE(str);
  return(describe_buffer);
}

static int free_locsig(mus_any *p) 
{
  locs *ptr = (locs *)p;
  if (ptr) 
    {
      if (ptr->outn) FREE(ptr->outn);
      if (ptr->revn) FREE(ptr->revn);
      mus_free((mus_any *)(ptr->outf));
      if (ptr->revf) mus_free((mus_any *)(ptr->revf));
      FREE(ptr);
    }
  return(0);
}

static off_t locsig_length(mus_any *ptr) {return(((locs *)ptr)->chans);}
static Float *locsig_data(mus_any *ptr) {return(((locs *)ptr)->outn);}
static int locsig_channels(mus_any *ptr) {return(((locs *)ptr)->chans);}
static Float *locsig_xcoeffs(mus_any *ptr) {return(((locs *)ptr)->revn);}

mus_any *mus_locsig_outf(mus_any *ptr) {return((mus_any *)(((locs *)ptr)->outf));}  /* clm2xen.c */
mus_any *mus_locsig_revf(mus_any *ptr) {return((mus_any *)(((locs *)ptr)->revf));}

void *mus_locsig_closure(mus_any *ptr) {return(((locs *)ptr)->closure);}            /* snd-run.c */
static void *locsig_set_closure(mus_any *ptr, void *e) {((locs *)ptr)->closure = e; return(e);}

static void locsig_reset(mus_any *ptr)
{
  locs *gen = (locs *)ptr;
  if (gen->outn) mus_clear_array(gen->outn, gen->chans);
  if (gen->revn) mus_clear_array(gen->revn, gen->rev_chans);
}

static Float locsig_xcoeff(mus_any *ptr, int index) 
{
  locs *gen = (locs *)ptr;
  if (gen->revn)
    return(gen->revn[index]);
  return(0.0);
}
static Float locsig_set_xcoeff(mus_any *ptr, int index, Float val) 
{
  locs *gen = (locs *)ptr;
  if (gen->revn)
    gen->revn[index] = val; 
  return(val);
}

Float mus_locsig_ref(mus_any *ptr, int chan) 
{
  locs *gen = (locs *)ptr;
  if ((ptr) && (mus_locsig_p(ptr))) 
    {
      if ((chan >= 0) && 
	  (chan < gen->chans))
	return(gen->outn[chan]);
      else mus_error(MUS_NO_SUCH_CHANNEL, 
		     S_locsig_ref " chan %d >= %d", 
		     chan, gen->chans);
    }
  return(0.0);
}

Float mus_locsig_set(mus_any *ptr, int chan, Float val) 
{
  locs *gen = (locs *)ptr;
  if ((ptr) && (mus_locsig_p(ptr))) 
    {
      if ((chan >= 0) && 
	  (chan < gen->chans))
	gen->outn[chan] = val;
      else mus_error(MUS_NO_SUCH_CHANNEL, 
		     S_locsig_set " chan %d >= %d", 
		     chan, gen->chans);
    }
  return(val);
}

Float mus_locsig_reverb_ref(mus_any *ptr, int chan) 
{
  locs *gen = (locs *)ptr;
  if ((ptr) && (mus_locsig_p(ptr))) 
    {
      if ((chan >= 0) && 
	  (chan < gen->rev_chans))
	return(gen->revn[chan]);
      else mus_error(MUS_NO_SUCH_CHANNEL, 
		     S_locsig_reverb_ref " chan %d, but this locsig has %d reverb chans", 
		     chan, gen->rev_chans);
    }
  return(0.0);
}

Float mus_locsig_reverb_set(mus_any *ptr, int chan, Float val) 
{
  locs *gen = (locs *)ptr;
  if ((ptr) && (mus_locsig_p(ptr))) 
    {
      if ((chan >= 0) && 
	  (chan < gen->rev_chans))
	gen->revn[chan] = val;
      else mus_error(MUS_NO_SUCH_CHANNEL, 
		     S_locsig_reverb_set " chan %d >= %d", 
		     chan, gen->rev_chans);
    }
  return(val);
}

static Float run_locsig(mus_any *ptr, Float arg1, Float arg2) {mus_locsig(ptr, (off_t)arg1, arg2); return(arg2);}

static mus_any_class LOCSIG_CLASS = {
  MUS_LOCSIG,
  S_locsig,
  &free_locsig,
  &describe_locsig,
  &locsig_equalp,
  &locsig_data, 0,
  &locsig_length,
  0,
  0, 0, 0, 0,
  0, 0,
  0, 0,
  &run_locsig,
  MUS_OUTPUT,
  &mus_locsig_closure,
  &locsig_channels,
  0, 0, 0, 0,
  &locsig_xcoeff, &locsig_set_xcoeff, 
  0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 
  &locsig_xcoeffs, 0, 0,
  &locsig_reset,
  &locsig_set_closure
};

bool mus_locsig_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_LOCSIG));}

void mus_fill_locsig(Float *arr, int chans, Float degree, Float scaler, mus_interp_t type)
{
  if (chans == 1)
    arr[0] = scaler;
  else
    {
      Float deg, pos, frac, degs_per_chan, ldeg, c, s;
      int left, right;
      if (degree < 0.0)
	{
	  /* sigh -- hope for the best... */
	  int m;
	  m = (int)ceil(degree / -360.0);
	  degree += (360 * m);
	}
      if (chans == 2)
	{
	  if (degree > 90.0)
	    deg = 90.0;
	  else
	    {
	      if (degree < 0.0)
		deg = 0.0;
	      else deg = degree;
	    }
	  degs_per_chan = 90.0;
	}
      else 
	{
	  deg = fmod(degree, 360.0);
	  degs_per_chan = 360.0 / chans;
	}
      pos = deg / degs_per_chan;
      left = (int)floor(pos);
      right = left + 1;
      if (right == chans) right = 0;
      frac = pos - left;
      if (type == MUS_INTERP_LINEAR)
	{
	  arr[left] = scaler * (1.0 - frac);
	  arr[right] = scaler * frac;
	}
      else
	{
	  ldeg = M_PI_2 * (0.5 - frac);
	  scaler *= sqrt(2.0) / 2.0;
	  c = cos(ldeg);
	  s = sin(ldeg);
	  arr[left] = scaler * (c + s);
	  arr[right] = scaler * (c - s);
	}
    }
}

mus_any *mus_make_locsig(Float degree, Float distance, Float reverb, 
			 int chans, mus_any *output,     /* direct signal output */
			 int rev_chans, mus_any *revput,  /* reverb output */
			 mus_interp_t type)
{
  locs *gen;
  Float dist;
  if (chans <= 0)
    {
      mus_error(MUS_ARG_OUT_OF_RANGE, "chans: %d", chans);
      return(NULL);
    }
  gen = (locs *)clm_calloc(1, sizeof(locs), S_make_locsig);
  gen->core = &LOCSIG_CLASS;
  gen->outf = (mus_frame *)mus_make_empty_frame(chans);

  gen->type = type;
  gen->reverb = reverb;
  if (distance > 1.0)
    dist = 1.0 / distance;
  else dist = 1.0;

  if (mus_output_p(output)) 
    gen->outn_writer = output;
  gen->chans = chans;
  gen->outn = (Float *)clm_calloc(gen->chans, sizeof(Float), "locsig frame");
  mus_fill_locsig(gen->outn, gen->chans, degree, dist, type);

  if (mus_output_p(revput))
    gen->revn_writer = revput;
  gen->rev_chans = rev_chans;
  if (gen->rev_chans > 0)
    {
      gen->revn = (Float *)clm_calloc(gen->rev_chans, sizeof(Float), "locsig reverb frame");
      gen->revf = (mus_frame *)mus_make_empty_frame(gen->rev_chans);
      mus_fill_locsig(gen->revn, gen->rev_chans, degree, (reverb * sqrt(dist)), type);
    }

  return((mus_any *)gen);
}

Float mus_locsig(mus_any *ptr, off_t loc, Float val)
{
  locs *gen = (locs *)ptr;
  int i;
  for (i = 0; i < gen->chans; i++)
    (gen->outf)->vals[i] = val * gen->outn[i];
  for (i = 0; i < gen->rev_chans; i++)
    (gen->revf)->vals[i] = val * gen->revn[i];
  if (gen->revn_writer)
    mus_frame_to_file(gen->revn_writer, loc, (mus_any *)(gen->revf));
  if (gen->outn_writer)
    mus_frame_to_file(gen->outn_writer, loc, (mus_any *)(gen->outf));
  return(val);
}

void mus_move_locsig(mus_any *ptr, Float degree, Float distance)
{
  locs *gen = (locs *)ptr;
  Float dist;
  mus_reset(ptr); /* clear old state, if any */
  if (distance > 1.0)
    dist = 1.0 / distance;
  else dist = 1.0;
  if (gen->rev_chans > 0)
    mus_fill_locsig(gen->revn, gen->rev_chans, degree, (gen->reverb * sqrt(dist)), gen->type);
  mus_fill_locsig(gen->outn, gen->chans, degree, dist, gen->type);
}



/* ---------------- move-sound ---------------- */

/* TODO: move-sound rb: update dlocsig.rb (787, l788)
 */

typedef struct {
  mus_any_class *core;
  mus_any *outn_writer;
  mus_any *revn_writer;
  mus_frame *outf, *revf;
  int out_channels, rev_channels;
  off_t start, end;
  mus_any *doppler_delay, *doppler_env, *rev_env;
  mus_any **out_delays, **out_envs, **rev_envs;
  int *out_map;
  bool free_arrays, free_gens;
  void *closure;
} dloc;

static bool move_sound_equalp(mus_any *p1, mus_any *p2) {return(p1 == p2);}
static int move_sound_channels(mus_any *ptr) {return(((dloc *)ptr)->out_channels);}
static off_t move_sound_length(mus_any *ptr) {return(((dloc *)ptr)->out_channels);} /* need both because return types differ */
static void move_sound_reset(mus_any *ptr) {}

mus_any *mus_move_sound_outf(mus_any *ptr) {return((mus_any *)(((dloc *)ptr)->outf));}
mus_any *mus_move_sound_revf(mus_any *ptr) {return((mus_any *)(((dloc *)ptr)->revf));}

void *mus_move_sound_closure(mus_any *ptr) {return(((dloc *)ptr)->closure);}
static void *move_sound_set_closure(mus_any *ptr, void *e) {((dloc *)ptr)->closure = e; return(e);}


static char *describe_move_sound(mus_any *ptr)
{
  dloc *gen = (dloc *)ptr;
  char *dopdly = NULL, *dopenv = NULL, *revenv = NULL;
  char *outdlys = NULL, *outenvs = NULL, *revenvs = NULL;
  char *outmap = NULL;
  char *starts = NULL;
  static char *allstr = NULL;
  int len;

  if (allstr) {FREE(allstr); allstr = NULL;}

  starts = mus_format(S_move_sound ": start: " OFF_TD ", end: " OFF_TD ", out chans %d, rev chans: %d",
		      gen->start, gen->end, gen->out_channels, gen->rev_channels);
  dopdly = mus_format("doppler %s", mus_describe(gen->doppler_delay));
  dopenv = mus_format("doppler %s", mus_describe(gen->doppler_env));
  revenv = mus_format("global reverb %s", mus_describe(gen->rev_env));
  outdlys = clm_array_to_string(gen->out_delays, gen->out_channels, "out_delays", "    ");
  outenvs = clm_array_to_string(gen->out_envs, gen->out_channels, "out_envs", "    ");
  revenvs = clm_array_to_string(gen->rev_envs, gen->rev_channels, "rev_envs", "    ");
  outmap = int_array_to_string(gen->out_map, gen->out_channels, "out_map");

  len = 64 + strlen(starts) + strlen(dopdly) + strlen(dopenv) + strlen(revenv) + 
    strlen(outdlys) + strlen(outenvs) + strlen(revenvs) + strlen(outmap);
  allstr = (char *)CALLOC(len, sizeof(char));
  mus_snprintf(allstr, len, "%s\n  %s\n  %s\n  %s\n  %s\n  %s\n  %s\n  %s\n  free: arrays: %s, gens: %s\n",
		      starts, dopdly, dopenv, revenv, outdlys, outenvs, revenvs, outmap,
		      (gen->free_arrays) ? "true" : "false",
		      (gen->free_gens) ? "true" : "false");
  FREE(starts); 
  FREE(dopdly); 
  FREE(dopenv); 
  FREE(revenv); 
  FREE(outdlys); 
  FREE(outenvs); 
  FREE(revenvs); 
  FREE(outmap);
  return(allstr);
}

static int free_move_sound(mus_any *p) 
{
  dloc *ptr = (dloc *)p;
  if (ptr) 
    {
      if (ptr->free_gens)
	{
	  int i;
	  /* free everything except outer arrays and IO stuff */
	  if (ptr->doppler_delay) mus_free(ptr->doppler_delay);
	  if (ptr->doppler_env) mus_free(ptr->doppler_env);
	  if (ptr->rev_env) mus_free(ptr->rev_env);
	  if (ptr->out_delays)
	    for (i = 0; i < ptr->out_channels; i++)
	      if (ptr->out_delays[i]) mus_free(ptr->out_delays[i]);
	  if (ptr->out_envs)
	    for (i = 0; i < ptr->out_channels; i++)
	      if (ptr->out_envs[i]) mus_free(ptr->out_envs[i]);
	  if (ptr->rev_envs)
	    for (i = 0; i < ptr->rev_channels; i++)
	      if (ptr->rev_envs[i]) mus_free(ptr->rev_envs[i]);
	}

      if (ptr->free_arrays)
	{
	  /* free outer arrays */
	  if (ptr->out_envs) {FREE(ptr->out_envs); ptr->out_envs = NULL;}
	  if (ptr->rev_envs) {FREE(ptr->rev_envs); ptr->rev_envs = NULL;}
	  if (ptr->out_delays) {FREE(ptr->out_delays); ptr->out_delays = NULL;}
	  if (ptr->out_map) FREE(ptr->out_map);
	}

      /* we created these in make_move_sound, so it should always be safe to free them */
      if (ptr->outf) mus_free((mus_any *)(ptr->outf));
      if (ptr->revf) mus_free((mus_any *)(ptr->revf));
      FREE(ptr);
    }
  return(0);
}

bool mus_move_sound_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_MOVE_SOUND));}

Float mus_move_sound(mus_any *ptr, off_t loc, Float uval)
{
  dloc *gen = (dloc *)ptr;
  Float val;
  int chan;

  if (loc > gen->end) val = 0.0; else val = uval;

  /* initial silence */
  if (loc < gen->start)
    {
      mus_delay_1(gen->doppler_delay, val);
      /* original calls out_any here with 0.0 -- a no-op */
      return(val);
    }

  /* doppler */
  if (gen->doppler_delay)
    val = mus_delay(gen->doppler_delay, val, mus_env(gen->doppler_env));

  /* direct signal */
  for (chan = 0; chan < gen->out_channels; chan++)
    {
      Float sample;
      sample = val * mus_env(gen->out_envs[chan]);
      if (gen->out_delays[chan])
	sample = mus_delay_1(gen->out_delays[chan], sample);
      gen->outf->vals[gen->out_map[chan]] = sample;
    }

  /* reverb */
  if (gen->revn_writer)
    {
      val *= mus_env(gen->rev_env);
      if (gen->rev_envs)
	{
	  if (gen->rev_channels == 1)
	    gen->revf->vals[0] = val * mus_env(gen->rev_envs[0]);
	  else
	    {
	      for (chan = 0; chan < gen->rev_channels; chan++)
		gen->revf->vals[gen->out_map[chan]] = val * mus_env(gen->rev_envs[chan]);
	    }
	}
      else gen->revf->vals[0] = val;
      mus_frame_to_file(gen->revn_writer, loc, (mus_any *)(gen->revf));
    }

  /* file output */
  if (gen->outn_writer)
    mus_frame_to_file(gen->outn_writer, loc, (mus_any *)(gen->outf));

  return(uval);
}

static Float run_move_sound(mus_any *ptr, Float arg1, Float arg2) {mus_move_sound(ptr, (off_t)arg1, arg2); return(arg2);}

static mus_any_class MOVE_SOUND_CLASS = {
  MUS_MOVE_SOUND,
  S_move_sound,
  &free_move_sound,
  &describe_move_sound,
  &move_sound_equalp,
  0, 0,
  &move_sound_length,
  0,
  0, 0, 0, 0,
  0, 0,
  0, 0,
  &run_move_sound,
  MUS_OUTPUT,
  &mus_move_sound_closure,
  &move_sound_channels,
  0, 0, 0, 0,
  0, 0,
  0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 
  0, 0, 0,
  &move_sound_reset,
  &move_sound_set_closure
};

mus_any *mus_make_move_sound(off_t start, off_t end, int out_channels, int rev_channels,
			     mus_any *doppler_delay, mus_any *doppler_env, mus_any *rev_env,
			     mus_any **out_delays, mus_any **out_envs, mus_any **rev_envs,
			     int *out_map, mus_any *output, mus_any *revput, bool free_arrays, bool free_gens)
{
  /* most of these args come to us in a list at the lisp/xen level ("dlocs" struct is actually a list) 
   *   so the make-move-sound function in lisp/xen is (make-move-sound dloc-list output revout)
   *   where the trailing args mimic locsig.
   */
  dloc *gen;
  if (out_channels <= 0)
    {
      mus_error(MUS_ARG_OUT_OF_RANGE, "move-sound: out chans: %d", out_channels);
      return(NULL);
    }
  gen = (dloc *)clm_calloc(1, sizeof(dloc), S_make_move_sound);
  gen->core = &MOVE_SOUND_CLASS;

  gen->start = start;
  gen->end = end;
  gen->out_channels = out_channels;
  gen->rev_channels = rev_channels;
  gen->doppler_delay = doppler_delay;
  gen->doppler_env = doppler_env;
  gen->rev_env = rev_env;
  gen->out_delays = out_delays;
  gen->out_envs = out_envs;
  gen->rev_envs = rev_envs;
  gen->out_map = out_map;

  /* default is to free only what we make ourselves */
  gen->free_gens = free_gens;
  gen->free_arrays = free_arrays;

  gen->outf = (mus_frame *)mus_make_empty_frame(out_channels);
  if (mus_output_p(output)) 
    gen->outn_writer = output;

  if (rev_channels > 0)
    {
      if (mus_output_p(revput))
	gen->revn_writer = revput;
      gen->revf = (mus_frame *)mus_make_empty_frame(rev_channels);
    }

  return((mus_any *)gen);
}



/* ---------------- src ---------------- */

/* sampling rate conversion */
/* taken from sweep_srate.c of Perry Cook.  To quote Perry:
 *
 * 'The conversion is performed by sinc interpolation.
 *    J. O. Smith and P. Gossett, "A Flexible Sampling-Rate Conversion Method," 
 *    Proc. of the IEEE Conference on Acoustics, Speech, and Signal Processing, San Diego, CA, March, 1984.
 * There are essentially two cases, one where the conversion factor
 * is less than one, and the sinc table is used as is yielding a sound
 * which is band limited to the 1/2 the new sampling rate (we don't
 * want to create bandwidth where there was none).  The other case
 * is where the conversion factor is greater than one and we 'warp'
 * the sinc table to make the final cutoff equal to the original sampling
 * rate /2.  Warping the sinc table is based on the similarity theorem
 * of the time and frequency domain, stretching the time domain (sinc
 * table) causes shrinking in the frequency domain.'
 *
 * we also scale the amplitude if interpolating to take into account the broadened sinc 
 *   this means that isolated pulses get scaled by 1/src, but that's a dumb special case
 */

typedef struct {
  mus_any_class *core;
  Float (*feeder)(void *arg, int direction);
  Float x;
  Float incr, width_1;
  int width, lim;
  int len;
  Float *data, *sinc_table;
  void *closure;
} sr;

#define SRC_SINC_DENSITY 1000
#define SRC_SINC_WIDTH 10

static Float **sinc_tables = NULL;
static int *sinc_widths = NULL;
static int sincs = 0;

void mus_clear_sinc_tables(void)
{
  if (sincs)
    {
      int i;
      for (i = 0; i < sincs; i++) 
	if (sinc_tables[i]) 
	  FREE(sinc_tables[i]);
      FREE(sinc_tables);
      sinc_tables = NULL;
      FREE(sinc_widths);
      sinc_widths = NULL;
      sincs = 0;
    }
}

static Float *init_sinc_table(int width)
{
  int i, size, padded_size, loc;
  Float sinc_freq, win_freq, sinc_phase, win_phase;
  for (i = 0; i < sincs; i++)
    if (sinc_widths[i] == width)
      return(sinc_tables[i]);
  if (sincs == 0)
    {
      sinc_tables = (Float **)clm_calloc(8, sizeof(Float *), "sinc tables");
      sinc_widths = (int *)clm_calloc(8, sizeof(int), "sinc tables");
      sincs = 8;
      loc = 0;
    }
  else
    {
      loc = -1;
      for (i = 0; i < sincs; i++)
	if (sinc_widths[i] == 0)
	  {
	    loc = i;
	    break;
	  }
      if (loc == -1)
	{
	  sinc_tables = (Float **)REALLOC(sinc_tables, (sincs + 8) * sizeof(Float *));
	  sinc_widths = (int *)REALLOC(sinc_widths, (sincs + 8) * sizeof(int));
	  for (i = sincs; i < (sincs + 8); i++) 
	    {
	      sinc_widths[i] = 0; 
	      sinc_tables[i] = NULL;
	    }
	  loc = sincs;
	  sincs += 8;
	}
    }
  sinc_widths[loc] = width;
  size = width * SRC_SINC_DENSITY;
  padded_size = size + 4;
  sinc_freq = M_PI / (Float)SRC_SINC_DENSITY;
  win_freq = M_PI / (Float)size;
  sinc_tables[loc] = (Float *)clm_calloc(padded_size, sizeof(Float), "sinc table");
  sinc_tables[loc][0] = 1.0;
  for (i = 1, sinc_phase = sinc_freq, win_phase = win_freq; i < padded_size; i++, sinc_phase += sinc_freq, win_phase += win_freq)
    sinc_tables[loc][i] = sin(sinc_phase) * (0.5 + 0.5 * cos(win_phase)) / sinc_phase;
  return(sinc_tables[loc]);
}

bool mus_src_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_SRC));}

static int free_src_gen(mus_any *srptr)
{
  sr *srp = (sr *)srptr;
  if (srp)
    {
      if (srp->data) FREE(srp->data);
      FREE(srp);
    }
  return(0);
}

static bool src_equalp(mus_any *p1, mus_any *p2) {return(p1 == p2);}

static char *describe_src(mus_any *ptr)
{
  sr *gen = (sr *)ptr;
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE,
	       S_src ": width: %d, x: %.3f, incr: %.3f, sinc table len: %d",
	       gen->width, gen->x, gen->incr, gen->len);
  return(describe_buffer);
}

static off_t src_length(mus_any *ptr) {return(((sr *)ptr)->width);}
static Float run_src_gen(mus_any *srptr, Float sr_change, Float unused) {return(mus_src(srptr, sr_change, NULL));}

static void *src_closure(mus_any *rd) {return(((sr *)rd)->closure);}
static void *src_set_closure(mus_any *rd, void *e) {((sr *)rd)->closure = e; return(e);}

static Float src_increment(mus_any *rd) {return(((sr *)rd)->incr);}
static Float src_set_increment(mus_any *rd, Float val) {((sr *)rd)->incr = val; return(val);}

static Float *src_sinc_table(mus_any *rd) {return(((sr *)rd)->sinc_table);}

static void src_reset(mus_any *ptr)
{
  sr *gen = (sr *)ptr;
  mus_clear_array(gen->data, gen->lim + 1);
  gen->x = 0.0;
  /* center the data if possible */
  if (gen->feeder)
    {
      int i, dir = 1;
      if (gen->incr < 0.0) dir = -1;
      for (i = gen->width - 1; i < gen->lim; i++) 
	gen->data[i] = (*(gen->feeder))(gen->closure, dir);
    }
}

static mus_any_class SRC_CLASS = {
  MUS_SRC,
  S_src,
  &free_src_gen,
  &describe_src,
  &src_equalp,
  &src_sinc_table, 0,
  &src_length,  /* sinc width actually */
  0,
  0, 0, 0, 0,
  0, 0,
  &src_increment,
  &src_set_increment,
  &run_src_gen,
  MUS_NOT_SPECIAL,
  &src_closure,
  0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &src_reset,
  &src_set_closure
};

mus_any *mus_make_src(Float (*input)(void *arg, int direction), Float srate, int width, void *closure)
{
  if (fabs(srate) > MUS_MAX_CLM_SRC)
    mus_error(MUS_ARG_OUT_OF_RANGE, S_make_src " srate arg invalid: %f", srate);
  else
    {
      if ((width < 0) || (width > MUS_MAX_CLM_SINC_WIDTH))
	mus_error(MUS_ARG_OUT_OF_RANGE, S_make_src " width arg invalid: %d", width);
      else
	{
	  sr *srp;
	  int wid;
	  srp = (sr *)clm_calloc(1, sizeof(sr), S_make_src);
	  if (width <= 0) width = SRC_SINC_WIDTH;
	  if (width < (int)(fabs(srate) * 2))
	    wid = (int)(ceil(fabs(srate)) * 2); 
	  else wid = width;
	  srp->core = &SRC_CLASS;
	  srp->x = 0.0;
	  srp->feeder = input;
	  srp->closure = closure;
	  srp->incr = srate;
	  srp->width = wid;
	  srp->lim = 2 * wid;
	  srp->len = wid * SRC_SINC_DENSITY;
	  srp->data = (Float *)clm_calloc(srp->lim + 1, sizeof(Float), "src table");
	  srp->sinc_table = init_sinc_table(wid);
	  if (input)
	    {
	      int i, dir = 1;
	      if (srate < 0.0) dir = -1;
	      for (i = wid - 1; i < srp->lim; i++) 
		srp->data[i] = (*input)(closure, dir);
	      /* was i = 0 here but we want the incoming data centered */
	    }
	  srp->width_1 = 1.0 - wid;
	  return((mus_any *)srp);
	}
    }
  return(NULL);
}

Float mus_src(mus_any *srptr, Float sr_change, Float (*input)(void *arg, int direction))
{
  sr *srp = (sr *)srptr;
  Float sum = 0.0, x, zf, srx, factor;
  int fsx, lim, i, k, loc;
  int xi, xs;
  bool int_ok = false;
  lim = srp->lim;
#if HAVE_DECL_ISNAN && HAVE_DECL_ISINF
  if ((isnan(sr_change)) || (isinf(sr_change))) sr_change = 0.0;
#endif
  if (sr_change > MUS_MAX_CLM_SRC) 
    sr_change = MUS_MAX_CLM_SRC;
  else
    {
      if (sr_change < -MUS_MAX_CLM_SRC) 
	sr_change = -MUS_MAX_CLM_SRC;
    }
  srx = srp->incr + sr_change;
  if (srp->x >= 1.0)
    {
      Float (*sr_input)(void *arg, int direction) = input;
      if (sr_input == NULL) sr_input = srp->feeder;
      fsx = (int)(srp->x);
      srp->x -= fsx;
      /* realign data, reset srp->x */
      if (fsx > lim)
	{
	  int dir = 1;
	  if (srx < 0.0) dir = -1;
	  /* if sr_change is so extreme that the new index falls outside the data table, we need to
	   *   read forward until we reach the new data bounds
	   */
	  for (i = lim; i < fsx; i++)
	    (*sr_input)(srp->closure, dir);
	  fsx = lim;
	}
      loc = lim - fsx;
      if (loc > 0)
	memmove((void *)(srp->data), (void *)(srp->data + fsx), sizeof(Float) * loc);
      for (i = loc; i < lim; i++) 
	srp->data[i] = (*sr_input)(srp->closure, (srx >= 0.0) ? 1 : -1);
    }
  /* if (srx == 0.0) srx = 0.01; */ /* can't decide about this ... */
  if (srx < 0.0) srx = -srx;
  /* tedious timing tests indicate that precalculating this block in the sr_change=0 case saves no time at all */
  if (srx > 1.0) 
    {
      factor = 1.0 / srx;
      /* this is not exact since we're sampling the sinc and so on, but it's close over a wide range */
      zf = factor * (Float)SRC_SINC_DENSITY; 
      xi = (int)zf;
      int_ok = ((zf - xi) < .001);
    }
  else 
    {
      factor = 1.0;
      zf = (Float)SRC_SINC_DENSITY;
      xi = SRC_SINC_DENSITY;
      int_ok = true;
    }

  if (int_ok)
    {
      xs = (int)(zf * (srp->width_1 - srp->x));
      i = 0;
      if (xs < 0)
	for (; (i < lim) && (xs < 0); i++, xs += xi)
	  sum += (srp->data[i] * srp->sinc_table[-xs]); /* fma? */
      for (; i < lim; i++, xs += xi)
	sum += (srp->data[i] * srp->sinc_table[xs]);
    }
  else
    {
      /* this form twice as slow because of float->int conversions */
      for (i = 0, x = zf * (srp->width_1 - srp->x); i < lim; i++, x += zf)
	{
	  /* we're moving backwards in the data array, so the sr->x field has to mimic that (hence the '1.0 - srp->x') */
	  if (x < 0) k = (int)(-x); else k = (int)x;
	  sum += (srp->data[i] * srp->sinc_table[k]);
	  /* rather than do a bounds check here, we just padded the sinc_table above with 2 extra 0's */
	}
    }
  srp->x += srx;
  return(sum * factor);
}


/* it was a cold, rainy day... */
Float mus_src_20(mus_any *srptr, Float (*input)(void *arg, int direction))
{
  sr *srp = (sr *)srptr;
  Float sum;
  int lim, i, loc;
  int xi, xs;
  lim = srp->lim;
  if (srp->x > 0.0)
    {
      /* realign data, reset srp->x */
      Float (*sr_input)(void *arg, int direction) = input;
      if (sr_input == NULL) sr_input = srp->feeder;
      loc = lim - 2;
      memmove((void *)(srp->data), (void *)(srp->data + 2), sizeof(Float) * loc);
      for (i = loc; i < lim; i++) 
	srp->data[i] = (*sr_input)(srp->closure, 1);
    }
  else srp->x = 2.0;
  xi = (int)(SRC_SINC_DENSITY / 2);
  xs = xi * (1 - srp->width);
  xi *= 2;
  sum = srp->data[srp->width - 1];
  for (i = 0; (i < lim) && (xs < 0); i += 2, xs += xi)
    sum += (srp->data[i] * srp->sinc_table[-xs]);
  for (; i < lim; i += 2, xs += xi)
    sum += (srp->data[i] * srp->sinc_table[xs]);
  return(sum * 0.5);
}

Float mus_src_05(mus_any *srptr, Float (*input)(void *arg, int direction))
{
  sr *srp = (sr *)srptr;
  Float sum;
  int lim, i, loc;
  int xs;
  lim = srp->lim;
  if (srp->x >= 1.0)
    {
      Float (*sr_input)(void *arg, int direction) = input;
      if (sr_input == NULL) sr_input = srp->feeder;
      loc = lim - 1;
      memmove((void *)(srp->data), (void *)(srp->data + 1), sizeof(Float) * loc);
      for (i = loc; i < lim; i++) 
	srp->data[i] = (*sr_input)(srp->closure, 1);
      srp->x = 0.0;
    }
  if (srp->x == 0.0)
    {
      srp->x = 0.5;
      return(srp->data[srp->width - 1]);
    }
  xs = (int)(SRC_SINC_DENSITY * (srp->width_1 - 0.5));
  for (i = 0, sum = 0.0; (i < lim) && (xs < 0); i++, xs += SRC_SINC_DENSITY)
    sum += (srp->data[i] * srp->sinc_table[-xs]);
  for (; i < lim; i++, xs += SRC_SINC_DENSITY)
    sum += (srp->data[i] * srp->sinc_table[xs]);
  srp->x += 0.5;
  return(sum);
}




/* ---------------- granulate ---------------- */

typedef struct {
  mus_any_class *core;
  Float (*rd)(void *arg, int direction);
  int s20;
  int s50;
  int rmp;
  Float amp;
  int cur_out;
  int input_hop;
  int ctr;
  int output_hop;
  Float *out_data;     /* output buffer */
  int out_data_len;
  Float *in_data;      /* input buffer */
  int in_data_len;
  void *closure;
  int (*edit)(void *closure);
  Float *grain;        /* grain data */
  int grain_len;
  bool first_samp;
  unsigned long randx; /* gen-local random number seed */
} grn_info;

bool mus_granulate_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_GRANULATE));}

static bool granulate_equalp(mus_any *p1, mus_any *p2) {return(p1 == p2);}

static char *describe_granulate(mus_any *ptr)
{
  grn_info *gen = (grn_info *)ptr;
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE,
	       S_granulate ": expansion: %.3f (%d/%d), scaler: %.3f, length: %.3f secs (%d samps), ramp: %.3f",
	       (Float)(gen->output_hop) / (Float)(gen->input_hop),
	       gen->input_hop, gen->output_hop,
	       gen->amp,
	       (Float)(gen->grain_len) / (Float)sampling_rate, gen->grain_len,
	       (Float)(gen->rmp) / (Float)sampling_rate);
  return(describe_buffer);
}

static int free_granulate(mus_any *ptr)
{
  grn_info *gen = (grn_info *)ptr;
  if (gen)
    {
      if (gen->out_data) FREE(gen->out_data);
      if (gen->in_data) FREE(gen->in_data);
      if (gen->grain) FREE(gen->grain);
      FREE(gen);
    }
  return(0);
}

static off_t grn_length(mus_any *ptr) {return(((grn_info *)ptr)->grain_len);}
static off_t grn_set_length(mus_any *ptr, off_t val) 
{
  grn_info *gen = ((grn_info *)ptr);
  if ((val > 0) && (val < gen->out_data_len)) 
    gen->grain_len = (int)val;                /* larger -> segfault */
  return(gen->grain_len);
}
static Float grn_scaler(mus_any *ptr) {return(((grn_info *)ptr)->amp);}
static Float grn_set_scaler(mus_any *ptr, Float val) {((grn_info *)ptr)->amp = val; return(val);}

static Float grn_frequency(mus_any *ptr) {return(((Float)((grn_info *)ptr)->output_hop) / (Float)sampling_rate);}
static Float grn_set_frequency(mus_any *ptr, Float val) {((grn_info *)ptr)->output_hop = (int)((Float)sampling_rate * val); return(val);}

static void *grn_closure(mus_any *rd) {return(((grn_info *)rd)->closure);}
static void *grn_set_closure(mus_any *rd, void *e) {((grn_info *)rd)->closure = e; return(e);}

static Float grn_increment(mus_any *ptr) 
{
  grn_info *gen = ((grn_info *)ptr);
  return(((Float)(gen->output_hop)) / ((Float)(gen->input_hop)));
}
static Float grn_set_increment(mus_any *ptr, Float val) 
{
  grn_info *gen = ((grn_info *)ptr);
  if (val != 0.0) 
    gen->input_hop = (int)(gen->output_hop / val); 
  return(val);
}

static off_t grn_hop(mus_any *ptr) {return(((grn_info *)ptr)->output_hop);}
static off_t grn_set_hop(mus_any *ptr, off_t val) {((grn_info *)ptr)->output_hop = (int)val; return(val);}

static off_t grn_ramp(mus_any *ptr) {return(((grn_info *)ptr)->rmp);}
static off_t grn_set_ramp(mus_any *ptr, off_t val)
{
  grn_info *gen = (grn_info *)ptr; 
  if (val < (gen->grain_len * .5))
    gen->rmp = (int)val;
  return(val);
}

static Float *granulate_data(mus_any *ptr) {return(((grn_info *)ptr)->grain);}
int mus_granulate_grain_max_length(mus_any *ptr) {return(((grn_info *)ptr)->in_data_len);}

static off_t grn_location(mus_any *ptr) {return((off_t)(((grn_info *)ptr)->randx));}
static off_t grn_set_location(mus_any *ptr, off_t val) {((grn_info *)ptr)->randx = (unsigned long)val; return(val);}

static Float run_granulate(mus_any *ptr, Float unused1, Float unused2) {return(mus_granulate(ptr, NULL));}

static void grn_reset(mus_any *ptr)
{
  grn_info *gen = (grn_info *)ptr; 
  gen->cur_out = 0;
  gen->ctr = 0;
  mus_clear_array(gen->out_data, gen->out_data_len);
  mus_clear_array(gen->in_data, gen->in_data_len);
  mus_clear_array(gen->grain, gen->in_data_len);
  gen->first_samp = true;
}

static int grn_irandom(grn_info *spd, int amp)
{
  /* gen-local next_random */
  spd->randx = spd->randx * 1103515245 + 12345;
  return((int)(amp * INVERSE_MAX_RAND2 * ((Float)((unsigned int)(spd->randx >> 16) & 32767))));
}

static mus_any_class GRANULATE_CLASS = {
  MUS_GRANULATE,
  S_granulate,
  &free_granulate,
  &describe_granulate,
  &granulate_equalp,
  &granulate_data, 0,
  &grn_length,    /* segment-length */
  &grn_set_length,
  &grn_frequency, /* spd-out */
  &grn_set_frequency,
  0, 0,
  &grn_scaler,    /* segment-scaler */
  &grn_set_scaler,
  &grn_increment,
  &grn_set_increment,
  &run_granulate,
  MUS_NOT_SPECIAL,
  &grn_closure,
  0,
  0, 0, 0, 0, 0, 0, 
  &grn_hop, &grn_set_hop, 
  &grn_ramp, &grn_set_ramp,
  0, 0, 0, 0, 
  &grn_location, &grn_set_location, /* local randx */
  0, 0, 0, 0, 0, 0,
  &grn_reset,
  &grn_set_closure
};

mus_any *mus_make_granulate(Float (*input)(void *arg, int direction), 
			    Float expansion, Float length, Float scaler, 
			    Float hop, Float ramp, Float jitter, int max_size, 
			    int (*edit)(void *closure),
			    void *closure)
{
  grn_info *spd;
  int outlen;
  outlen = (int)(sampling_rate * (hop + length));
  if (max_size > outlen) outlen = max_size;
  if (expansion <= 0.0)
    {
      mus_error(MUS_ARG_OUT_OF_RANGE, S_make_granulate " expansion must be > 0.0: %f", expansion);
      return(NULL);
    }
  if (outlen <= 0) 
    {
      mus_error(MUS_NO_LENGTH, S_make_granulate " size is %d (hop: %f, segment-length: %f)?", outlen, hop, length);
      return(NULL);
    }
  if ((hop * sampling_rate) < expansion)
    {
      mus_error(MUS_ARG_OUT_OF_RANGE, S_make_granulate " expansion (%f) must be < hop * srate (%f)", expansion, hop * sampling_rate);
      return(NULL);
    }
  spd = (grn_info *)clm_calloc(1, sizeof(grn_info), S_make_granulate);
  spd->core = &GRANULATE_CLASS;
  spd->cur_out = 0;
  spd->ctr = 0;
  spd->grain_len = (int)(ceil(length * sampling_rate));
  spd->rmp = (int)(ramp * spd->grain_len);
  spd->amp = scaler;
  spd->output_hop = (int)(hop * sampling_rate);
  spd->input_hop = (int)((Float)(spd->output_hop) / expansion);
  spd->s20 = 2 * (int)(jitter * sampling_rate * hop); /* was *.05 here and *.02 below */
   /* added "2 *" 21-Mar-05 and replaced irandom with (grn)mus_irandom below */
  spd->s50 = (int)(jitter * sampling_rate * hop * 0.4);
  spd->out_data_len = outlen;
  spd->out_data = (Float *)clm_calloc(spd->out_data_len, sizeof(Float), "granulate out data");
  spd->in_data_len = outlen + spd->s20 + 1;
  spd->in_data = (Float *)clm_calloc(spd->in_data_len, sizeof(Float), "granulate in data");
  spd->rd = input;
  spd->closure = closure;
  spd->edit = edit;
  spd->grain = (Float *)clm_calloc(spd->in_data_len, sizeof(Float), "granulate grain");
  spd->first_samp = true;
  spd->randx = mus_rand_seed(); /* caller can override this via the mus_location method */
  next_random();
  return((mus_any *)spd);
}

void mus_granulate_set_edit_function(mus_any *ptr, int (*edit)(void *closure))
{
  grn_info *gen = (grn_info *)ptr;
  if (!(gen->grain))
    gen->grain = (Float *)clm_calloc(gen->in_data_len, sizeof(Float), "granulate grain");
  gen->edit = edit;
}

Float mus_granulate_with_editor(mus_any *ptr, Float (*input)(void *arg, int direction), int (*edit)(void *closure))
{ 
  /* in_data_len is the max grain size (:maxsize arg), not the current grain size
   * out_data_len is the size of the output buffer
   * grain_len is the current grain size
   * cur_out is the out_data buffer location where we need to add in the next grain
   * ctr is where we are now in out_data
   */
  grn_info *spd = (grn_info *)ptr;
  Float result = 0.0;

  if (spd->ctr < spd->out_data_len)
    result = spd->out_data[spd->ctr]; /* else return 0.0 */
  spd->ctr++;

  if (spd->ctr >= spd->cur_out)       /* time for next grain */
    {
      int i;

      /* set up edit/input functions and possible outside-accessible grain array */
      Float (*spd_input)(void *arg, int direction) = input;
      int (*spd_edit)(void *closure) = edit;
      if (spd_input == NULL) spd_input = spd->rd;
      if (spd_edit == NULL) spd_edit = spd->edit;

      if (spd->first_samp)
	{
	  /* fill up in_data, out_data is already cleared */
	  for (i = 0; i < spd->in_data_len; i++)
	    spd->in_data[i] = (*spd_input)(spd->closure, 1);
	}

      else
	{

	  /* align output buffer to flush the data we've already output, and zero out new trailing portion */
	  if (spd->cur_out >= spd->out_data_len)
	    {
	      /* entire buffer has been output, and in fact we've been sending 0's for awhile to fill out hop */
	      mus_clear_array(spd->out_data, spd->out_data_len); /* so zero the entire thing (it's all old) */
	    }
	  else 
	    {
	      /* move yet-un-output data to 0, zero trailers */
	      int good_samps;
	      good_samps = (spd->out_data_len - spd->cur_out);
	      memmove((void *)(spd->out_data), (void *)(spd->out_data + spd->cur_out), good_samps * sizeof(Float));
	      memset((void *)(spd->out_data + good_samps), 0, spd->cur_out * sizeof(Float)); /* must be cur_out trailing samples to 0 */
	    }

	  /* align input buffer */
	  if (spd->input_hop > spd->in_data_len)
	    {
	      /* need to flush enough samples to accommodate the fact that the hop is bigger than our data buffer */
	      for (i = spd->in_data_len; i < spd->input_hop; i++) (*spd_input)(spd->closure, 1);
	      /* then get a full input buffer */
	      for (i = 0; i < spd->in_data_len; i++)
		spd->in_data[i] = (*spd_input)(spd->closure, 1);
	    }
	  else
	    {
	      /* align input buffer with current input hop location */
	      int good_samps;
	      good_samps = (spd->in_data_len - spd->input_hop);
	      memmove((void *)(spd->in_data), (void *)(spd->in_data + spd->input_hop), good_samps * sizeof(Float));
	      for (i = good_samps; i < spd->in_data_len; i++)
		spd->in_data[i] = (*spd_input)(spd->closure, 1);
	    }
	}

      /* create current grain */
      {
	int lim, steady_end, curstart, j;
	lim = spd->grain_len;
	curstart = grn_irandom(spd, spd->s20); /* start location in input buffer */
	if ((curstart + spd->grain_len) > spd->in_data_len)
	  lim = (spd->in_data_len - curstart);
	if (lim > spd->grain_len)
	  lim = spd->grain_len;
	mus_clear_array(spd->grain, spd->grain_len);
	if (spd->rmp > 0)
	  {
	    Float amp = 0.0, incr;
	    steady_end = (spd->grain_len - spd->rmp);
	    incr = (Float)(spd->amp) / (Float)(spd->rmp);
	    for (i = 0, j = curstart; i < lim; i++, j++)
	      {
		spd->grain[i] = (amp * spd->in_data[j]);
		if (i < spd->rmp) 
		  amp += incr; 
		else 
		  if (i >= steady_end) /* was >, but that truncates the envelope */
		    amp -= incr;
	      }
	  }
	else
	  {
	    /* ramp is 0.0, so just scale the input buffer by the current amp */
	    if (spd->amp == 1.0)
	      memcpy((void *)(spd->grain), (void *)(spd->in_data + curstart), lim * sizeof(Float));
	    else
	      {
		for (i = 0, j = curstart; i < lim; i++, j++)
		  spd->grain[i] = (spd->amp * spd->in_data[j]);
	      }
	  }
      }

      /* add new grain into output buffer */
      {
	int new_len;
	if (spd_edit)
	  {
	    new_len = (*spd_edit)(spd->closure);
	    if (new_len <= 0)
	      new_len = spd->grain_len;
	    else
	      {
		if (new_len > spd->out_data_len)
		  new_len = spd->out_data_len;
	      }
	  }
	else new_len = spd->grain_len;
	if (new_len > spd->out_data_len) /* can be off-by-one here if hop is just barely greater then 0.0 (user is screwing around...) */
	  new_len = spd->out_data_len;
	for (i = 0; i < new_len; i++)
	  spd->out_data[i] += spd->grain[i];
      }
      
      /* set location of next grain calculation */
      spd->ctr = 0;
      spd->cur_out = spd->output_hop + grn_irandom(spd, 2 * spd->s50) - (spd->s50 >> 1); 
      /* this form suggested by Marc Lehmann */
      /* "2 *" added 21-Mar-05 and irandom replaced with mus_irandom, grn_irandom 28-Feb-06 */
      /* use of gen-local random sequence suggested by Kjetil Matheussen (to keep multi-channel grns in sync) */
      if (spd->cur_out < 0) spd->cur_out = 0;

      if (spd->first_samp)
	{
	  spd->first_samp = false;
	  spd->ctr = 1;
	  return(spd->out_data[0]);
	}
    }
  return(result);
}

Float mus_granulate(mus_any *ptr, Float (*input)(void *arg, int direction))
{
  return(mus_granulate_with_editor(ptr, input, NULL));
}


/* ---------------- convolve ---------------- */

/* fft and convolution of Float data in zero-based arrays
 */

#if HAVE_FFTW3
static double *rdata = NULL, *idata = NULL;
static fftw_plan rplan, iplan;
static int last_fft_size = 0;

void mus_fftw(Float *rl, int n, int dir)
{
  int i;
  if (n != last_fft_size)
    {
      if (rdata) {fftw_free(rdata); fftw_free(idata); fftw_destroy_plan(rplan); fftw_destroy_plan(iplan);}
      rdata = (double *)fftw_malloc(n * sizeof(double));
      idata = (double *)fftw_malloc(n * sizeof(double));
      rplan = fftw_plan_r2r_1d(n, rdata, idata, FFTW_R2HC, FFTW_ESTIMATE); 
      iplan = fftw_plan_r2r_1d(n, rdata, idata, FFTW_HC2R, FFTW_ESTIMATE);
      last_fft_size = n;
    }
  memset((void *)idata, 0, n * sizeof(double));
  for (i = 0; i < n; i++) rdata[i] = rl[i];
  if (dir != -1)
    fftw_execute(rplan);
  else fftw_execute(iplan);
  for (i = 0; i < n; i++) rl[i] = idata[i];
}
#else
#if HAVE_FFTW
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
  for (i = 0; i < n; i++) rdata[i] = rl[i];
  if (dir != -1)
    rfftw_one(rplan, rdata, idata);
  else rfftw_one(iplan, rdata, idata);
  for (i = 0; i < n; i++) rl[i] = idata[i];
}
#endif
#endif

static void mus_scramble(Float* rl, Float* im, int n)
{
  /* bit reversal */

  int i, m, j;
  Float vr, vi;
  j = 0;
  for (i = 0; i < n; i++)
    {
      if (j > i)
	{
	  vr = rl[j];
	  vi = im[j];
	  rl[j] = rl[i];
	  im[j] = im[i];
	  rl[i] = vr;
	  im[i] = vi;
	}
      m = n >> 1;
      while ((m >= 2) && (j >= m))
	{
	  j -= m;
	  m = m >> 1;
	}
      j += m;
    }
}

void mus_fft(Float *rl, Float *im, int n, int is)
{
  /* standard fft: real part in rl, imaginary in im,
   * rl and im are zero-based.
   * see fxt/simplfft/fft.c (Joerg Arndt) 
   */
  int m, j, mh, ldm, lg, i, i2, j2, imh;
  double ur, ui, u, vr, vi, angle, c, s;
  imh = (int)(log(n + 1) / log(2.0));
  mus_scramble(rl, im, n);
  m = 2;
  ldm = 1;
  mh = n >> 1;
  angle = (M_PI * is);
  for (lg = 0; lg < imh; lg++)
    {
      c = cos(angle);
      s = sin(angle);
      ur = 1.0;
      ui = 0.0;
      for (i2 = 0; i2 < ldm; i2++)
	{
	  i = i2;
	  j = i2 + ldm;
	  for (j2 = 0; j2 < mh; j2++)
	    {
	      vr = ur * rl[j] - ui * im[j];
	      vi = ur * im[j] + ui * rl[j];
	      rl[j] = rl[i] - vr;
	      im[j] = im[i] - vi;
	      rl[i] += vr;
	      im[i] += vi;
	      i += m;
	      j += m;
	    }
	  u = ur;
	  ur = (ur * c) - (ui * s);
	  ui = (ui * c) + (u * s);
	}
      mh >>= 1;
      ldm = m;
      angle *= 0.5;
      m <<= 1;
    }
}

#if HAVE_GSL
#include <gsl/gsl_sf_bessel.h>
double mus_bessi0(Float x)
{
  gsl_sf_result res;
  gsl_sf_bessel_I0_e(x, &res);
  return(res.val);
}
#else
double mus_bessi0(Float x)
{ 
  if (x == 0.0) return(1.0);
  if (fabs(x) <= 15.0) 
    {
      Float z, denominator, numerator;
      z = x * x;
      numerator=(z * (z * (z * (z * (z * (z * (z * (z * (z * (z * (z * (z * (z * (z * 
										  0.210580722890567e-22 + 0.380715242345326e-19) +
									     0.479440257548300e-16) + 0.435125971262668e-13) +
								   0.300931127112960e-10) + 0.160224679395361e-7) +
							 0.654858370096785e-5) + 0.202591084143397e-2) +
					       0.463076284721000e0) + 0.754337328948189e2) +
				     0.830792541809429e4) + 0.571661130563785e6) +
			   0.216415572361227e8) + 0.356644482244025e9) +
		 0.144048298227235e10);
      denominator=(z * (z * (z - 0.307646912682801e4) +
			0.347626332405882e7) - 0.144048298227235e10);
      return(-numerator / denominator);
    } 
  return(1.0);
}
#endif

#if HAVE_COMPLEX_TRIG || HAVE_GSL
static Float ultraspherical(int n, Float x, Float lambda)
{
  /* this is also the algorithm used in gsl gegenbauer.c -- slow but not as bad as using the binomials! */
  Float fn1, fn2 = 1.0, fn = 1.0;
  int k;
  if (n == 0) return(1.0);
  if (lambda == 0.0)
    fn1 = 2.0 * x;
  else fn1 = 2.0 * x * lambda;
  if (n == 1) return(fn1);
  for (k = 2; k <= n; k++)
    {
      fn = ((2.0 * x * (k + lambda - 1.0) * fn1) - ((k + (2.0 * lambda) - 2.0) * fn2)) / (Float)k;
      fn2 = fn1;
      fn1 = fn;
    }
  return(fn);
}
#endif

static Float sqr(Float x) {return(x * x);}

Float *mus_make_fft_window_with_window(mus_fft_window_t type, int size, Float beta, Float mu, Float *window)
{
  /* mostly taken from
   *    Fredric J. Harris, "On the Use of Windows for Harmonic Analysis with the
   *    Discrete Fourier Transform," Proceedings of the IEEE, Vol. 66, No. 1,
   *    January 1978.
   *    Albert H. Nuttall, "Some Windows with Very Good Sidelobe Behaviour", 
   *    IEEE Transactions of Acoustics, Speech, and Signal Processing, Vol. ASSP-29,
   *    No. 1, February 1981, pp 84-91
   *
   * JOS had slightly different numbers for the Blackman-Harris windows.
   */
  int i, j, midn, midp1;
  double freq, rate, angle = 0.0, cx;
  if (window == NULL) return(NULL);

  midn = size >> 1;
  midp1 = (size + 1) / 2;
  freq = TWO_PI / (Float)size;
  rate = 1.0 / (Float)midn;

  switch (type)
    {
    case MUS_RECTANGULAR_WINDOW:
      for (i = 0; i < size; i++) 
	window[i] = 1.0;
      break; 

    case MUS_HANN_WINDOW:
      for (i = 0, j = size - 1, angle = 0.0; i <= midn; i++, j--, angle += freq) 
	window[j] = (window[i] = 0.5 - 0.5 * cos(angle));
      break; 

    case MUS_WELCH_WINDOW:
      for (i = 0, j = size - 1; i <= midn; i++, j--) 
	window[j] = (window[i] = 1.0 - sqr((Float)(i - midn) / (Float)midp1));
      break; 

    case MUS_CONNES_WINDOW:
      for (i = 0, j = size - 1; i <= midn; i++, j--) 
	window[j] = (window[i] = sqr(1.0 - sqr((Float)(i - midn) / (Float)midp1)));
      break; 

    case MUS_PARZEN_WINDOW:
      for (i = 0, j = size - 1; i <= midn; i++, j--) 
	window[j] = (window[i] = 1.0 - fabs((Float)(i - midn) / (Float)midp1));
      break; 

    case MUS_BARTLETT_WINDOW:
      for (i = 0, j = size - 1, angle = 0.0; i <= midn; i++, j--, angle += rate) 
	window[j] = (window[i] = angle);
      break; 

    case MUS_HAMMING_WINDOW:
      for (i = 0, j = size - 1, angle = 0.0; i <= midn; i++, j--, angle += freq) 
	window[j] = (window[i] = 0.54 - 0.46 * cos(angle));
      break; 

    case MUS_BLACKMAN2_WINDOW: /* using Chebyshev polynomial equivalents here (this is also given as .42 .5 .08) */
      for (i = 0, j = size - 1, angle = 0.0; i <= midn; i++, j--, angle += freq) 
	{              /* (+ 0.42323 (* -0.49755 (cos a)) (* 0.07922 (cos (* a 2)))) */
	  cx = cos(angle);
	  window[j] = (window[i] = (.34401 + (cx * (-.49755 + (cx * .15844)))));
	}
      break; 

    case MUS_BLACKMAN3_WINDOW:
      for (i = 0, j = size - 1, angle = 0.0; i <= midn; i++, j--, angle += freq) 
	{              /* (+ 0.35875 (* -0.48829 (cos a)) (* 0.14128 (cos (* a 2))) (* -0.01168 (cos (* a 3)))) */
	               /* (+ 0.36336 (*  0.48918 (cos a)) (* 0.13660 (cos (* a 2))) (*  0.01064 (cos (* a 3)))) is "Nuttall" window? */

	  cx = cos(angle);
	  window[j] = (window[i] = (.21747 + (cx * (-.45325 + (cx * (.28256 - (cx * .04672)))))));
	}
      break; 

    case MUS_BLACKMAN4_WINDOW:
      for (i = 0, j = size - 1, angle = 0.0; i <= midn; i++, j--, angle += freq) 
	{             /* (+ 0.287333 (* -0.44716 (cos a)) (* 0.20844 (cos (* a 2))) (* -0.05190 (cos (* a 3))) (* 0.005149 (cos (* a 4)))) */
	  cx = cos(angle);
	  window[j] = (window[i] = (.084037 + (cx * (-.29145 + (cx * (.375696 + (cx * (-.20762 + (cx * .041194)))))))));
	}
      break; 

    case MUS_EXPONENTIAL_WINDOW:
      {
	Float expn, expsum = 1.0;
	expn = log(2) / (Float)midn + 1.0;
	for (i = 0, j = size - 1; i <= midn; i++, j--) 
	  {
	    window[j] = (window[i] = expsum - 1.0); 
	    expsum *= expn;
	  }
      }
      break;

    case MUS_KAISER_WINDOW:
      {
	Float I0beta;
	I0beta = mus_bessi0(beta); /* Harris multiplies beta by pi */
	for (i = 0, j = size - 1, angle = 1.0; i <= midn; i++, j--, angle -= rate) 
	  window[j] = (window[i] = mus_bessi0(beta * sqrt(1.0 - sqr(angle))) / I0beta);
      }
      break;

    case MUS_CAUCHY_WINDOW:
      for (i = 0, j = size - 1, angle = 1.0; i <= midn; i++, j--, angle -= rate) 
	window[j] = (window[i] = 1.0 / (1.0 + sqr(beta * angle)));
      break;

    case MUS_POISSON_WINDOW:
      for (i = 0, j = size - 1, angle = 1.0; i <= midn; i++, j--, angle -= rate) 
	window[j] = (window[i] = exp((-beta) * angle));
      break;

    case MUS_HANN_POISSON_WINDOW:
      /* Hann * Poisson -- from JOS */
      {
	Float angle1;
	for (i = 0, j = size - 1, angle = 1.0, angle1 = 0.0; i <= midn; i++, j--, angle -= rate, angle1 += freq) 
	  window[j] = (window[i] = (exp((-beta) * angle) * (0.5 - 0.5 * cos(angle1))));
      }
      break;

    case MUS_RIEMANN_WINDOW:
      {
	Float sr1;
	sr1 = TWO_PI / (Float)size;
	for (i = 0, j = size - 1; i <= midn; i++, j--) 
	  {
	    if (i == midn) 
	      window[j] = (window[i] = 1.0);
	    else 
	      {
		cx = sr1 * (midn - i);
		window[j] = (window[i] = sin(cx) / cx);
	      }
	  }
      }
      break;

    case MUS_GAUSSIAN_WINDOW:
      for (i = 0, j = size - 1, angle = 1.0; i <= midn; i++, j--, angle -= rate) 
	window[j] = (window[i] = exp(-.5 * sqr(beta * angle)));
      break;

    case MUS_TUKEY_WINDOW:
      cx = midn * (1.0 - beta);
      for (i = 0, j = size - 1; i <= midn; i++, j--) 
	{
	  if (i >= cx) 
	    window[j] = (window[i] = 1.0);
	  else window[j] = (window[i] = .5 * (1.0 - cos(M_PI * i / cx)));
	}
      break;

    case MUS_ULTRASPHERICAL_WINDOW:
    case MUS_SAMARAKI_WINDOW:
    case MUS_DOLPH_CHEBYSHEV_WINDOW:
      /* "Design of Ultraspherical Window Functions with Prescribed Spectral Characteristics", Bergen and Antoniou, EURASIP JASP 2004 */
      if (type == MUS_ULTRASPHERICAL_WINDOW)
	{
	  if (mu == 0.0)
	    type = MUS_DOLPH_CHEBYSHEV_WINDOW;
	  else
	    {
	      if (mu == 1.0)
		type = MUS_SAMARAKI_WINDOW;
	    }
	}

#if HAVE_COMPLEX_TRIG
      {
	Float *rl, *im;
	Float pk = 0.0;
	double alpha;
	freq = M_PI / (Float)size;
	if (beta < 0.2) beta = 0.2;
	alpha = ccosh(cacosh(pow(10.0, beta)) / (Float)size);
	rl = (Float *)clm_calloc(size, sizeof(Float), "ifft window buffer");
	im = (Float *)clm_calloc(size, sizeof(Float), "ifft window buffer");
	for (i = 0, angle = 0.0; i < size; i++, angle += freq)
	  {
	    switch (type)
	      {
	      case MUS_DOLPH_CHEBYSHEV_WINDOW:
		rl[i] = creal(ccos(cacos(alpha * cos(angle)) * size)); /* here is Tn (Chebyshev polynomial 1st kind) */
		break;
	      case MUS_SAMARAKI_WINDOW:
		/* Samaraki window uses Un instead */
		rl[i] = creal(csin(cacos(alpha * cos(angle)) * (size + 1.0)) / csin(cacos(alpha * cos(angle))));
		break;
	      case MUS_ULTRASPHERICAL_WINDOW:
		/* Cn here */
		rl[i] = ultraspherical(size, alpha * cos(angle), mu);
		break;
	      default: 
		break;
	      }
	  }
	mus_fft(rl, im, size, -1);    /* can be 1 here */
	pk = 0.0;
	for (i = 0; i < size; i++) 
	  if (pk < rl[i]) 
	    pk = rl[i];
	if ((pk != 0.0) && (pk != 1.0))
	  {
	    for (i = 0, j = size / 2; i < size; i++) 
	      {
		window[i] = rl[j++] / pk;
		if (j == size) j = 0;
	      }
	  }
	else
	  {
	    memcpy((void *)window, (void *)rl, size * sizeof(Float));
	  }

	FREE(rl);
	FREE(im);
      }
#else
#if HAVE_GSL
      {
	Float *rl, *im;
	Float pk;
	double alpha;
	freq = M_PI / (Float)size;
	if (beta < 0.2) beta = 0.2;
	alpha = GSL_REAL(gsl_complex_cosh(
			   gsl_complex_mul_real(
			     gsl_complex_arccosh_real(pow(10.0, beta)),
			     (double)(1.0 / (Float)size))));
	rl = (Float *)clm_calloc(size, sizeof(Float), "ifft window buffer");
	im = (Float *)clm_calloc(size, sizeof(Float), "ifft window buffer");
	for (i = 0, angle = 0.0; i < size; i++, angle += freq)
	  {
	    switch (type)
	      {
	      case MUS_DOLPH_CHEBYSHEV_WINDOW:
		rl[i] = GSL_REAL(gsl_complex_cos(
			           gsl_complex_mul_real(
			             gsl_complex_arccos_real(alpha * cos(angle)),
				     (double)size)));
		break;
	      case MUS_SAMARAKI_WINDOW:
		rl[i] = GSL_REAL(gsl_complex_div(
		                   gsl_complex_sin(
			             gsl_complex_mul_real(
			               gsl_complex_arccos_real(alpha * cos(angle)),
				       (double)(size + 1.0))),
				   gsl_complex_sin(
				     gsl_complex_arccos_real(alpha * cos(angle)))));
		break;
	      case MUS_ULTRASPHERICAL_WINDOW:
		rl[i] = ultraspherical(size, alpha * cos(angle), mu);
		break;
	      default: 
		break;
	      }

	  }
	mus_fft(rl, im, size, -1);    /* can be 1 here */
	pk = 0.0;
	for (i = 0; i < size; i++) 
	  if (pk < rl[i]) 
	    pk = rl[i];
	if ((pk != 0.0) && (pk != 1.0))
	  {
	    for (i = 0, j = size / 2; i < size; i++) 
	      {
		window[i] = rl[j++] / pk;
		if (j == size) j = 0;
	      }
	  }
	else
	  {
	    memcpy((void *)window, (void *)rl, size * sizeof(Float));
	  }
	FREE(rl);
	FREE(im);
      }
#else
      mus_error(MUS_NO_SUCH_FFT_WINDOW, "Dolph-Chebyshev, Samaraki, and Ultraspherical windows need complex trig support");
#endif
#endif
      break;
    default: 
      mus_error(MUS_NO_SUCH_FFT_WINDOW, "unknown fft data window: %d", (int)type); 
      break;
    }
  return(window);
}

Float *mus_make_fft_window(mus_fft_window_t type, int size, Float beta)
{
  return(mus_make_fft_window_with_window(type, size, beta, 0.0, (Float *)clm_calloc(size, sizeof(Float), S_make_fft_window)));
}

Float *mus_spectrum(Float *rdat, Float *idat, Float *window, int n, int type)
{
  int i;
  Float maxa, todb, lowest;
  double val;
  if (window) mus_multiply_arrays(rdat, window, n);
  mus_clear_array(idat, n);
  mus_fft(rdat, idat, n, 1);
  lowest = 0.000001;
  maxa = 0.0;
  n = (int)(n * 0.5);
  for (i = 0; i < n; i++)
    {
#if (__linux__ && __PPC__)
      if (rdat[i] < lowest) rdat[i] = 0.0;
      if (idat[i] < lowest) idat[i] = 0.0;
#endif
      val = rdat[i] * rdat[i] + idat[i] * idat[i];
      if (val < lowest)
	rdat[i] = 0.001;
      else 
	{
	  rdat[i] = sqrt(val);
	  if (rdat[i] > maxa) maxa = rdat[i];
	}
    }
  if (maxa > 0.0)
    {
      maxa = 1.0 / maxa;
      if (type == 0) /* dB */
	{
	  todb = 20.0 / log(10.0);
	  for (i = 0; i < n; i++) 
	    rdat[i] = todb * log(rdat[i] * maxa);
	}
      else
	{
	  if (type == 1) /* linear, normalized */
	    for (i = 0; i < n; i++) 
	      rdat[i] *= maxa;
	}
    }
  return(rdat);
}

Float *mus_convolution(Float* rl1, Float* rl2, int n)
{
  /* convolves two real arrays.                                           */
  /* rl1 and rl2 are assumed to be set up correctly for the convolution   */
  /* (that is, rl1 (the "signal") is zero-padded by length of             */
  /* (non-zero part of) rl2 and rl2 is stored in wrap-around order)       */
  /* We treat rl2 as the imaginary part of the first fft, then do         */
  /* the split, scaling, and (complex) spectral multiply in one step.     */
  /* result in rl1                                                        */

  int j, n2;
  Float invn;

  mus_fft(rl1, rl2, n, 1);
  
  n2 = n >> 1;
  invn = 0.25 / (Float)n;
  rl1[0] = ((rl1[0] * rl2[0]) / (Float)n);
  rl2[0] = 0.0;

  for (j = 1; j <= n2; j++)
    {
      int nn2;
      Float rem, rep, aim, aip;
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
  
  mus_fft(rl1, rl2, n, -1);
  return(rl1);
}


typedef struct {
  mus_any_class *core;
  Float (*feeder)(void *arg, int direction);
  int fftsize, fftsize2, ctr, filtersize;
  Float *rl1, *rl2, *buf, *filter; 
  void *closure;
} conv;

static bool convolve_equalp(mus_any *p1, mus_any *p2) {return(p1 == p2);}

static char *describe_convolve(mus_any *ptr)
{
  conv *gen = (conv *)ptr;
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, 
	       S_convolve ": size: %d", 
	       gen->fftsize);
  return(describe_buffer);
}

static int free_convolve(mus_any *ptr)
{
  conv *gen = (conv *)ptr;
  if (gen)
    {
      if (gen->rl1) FREE(gen->rl1);
      if (gen->rl2) FREE(gen->rl2);
      if (gen->buf) FREE(gen->buf);
      FREE(gen);
    }
  return(0);
}

static off_t conv_length(mus_any *ptr) {return(((conv *)ptr)->fftsize);}
static Float run_convolve(mus_any *ptr, Float unused1, Float unused2) {return(mus_convolve(ptr, NULL));}

static void *conv_closure(mus_any *rd) {return(((conv *)rd)->closure);}
static void *conv_set_closure(mus_any *rd, void *e) {((conv *)rd)->closure = e; return(e);}

static void convolve_reset(mus_any *ptr)
{
  conv *gen = (conv *)ptr;
  gen->ctr = gen->fftsize2;
  mus_clear_array(gen->rl1, gen->fftsize);
  mus_clear_array(gen->rl2, gen->fftsize);
  mus_clear_array(gen->buf, gen->fftsize);
}

static mus_any_class CONVOLVE_CLASS = {
  MUS_CONVOLVE,
  S_convolve,
  &free_convolve,
  &describe_convolve,
  &convolve_equalp,
  0, 0,
  &conv_length,
  0,
  0, 0, 0, 0,
  0, 0,
  0, 0,
  &run_convolve,
  MUS_NOT_SPECIAL,
  &conv_closure,
  0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &convolve_reset,
  &conv_set_closure
};

Float mus_convolve(mus_any *ptr, Float (*input)(void *arg, int direction))
{
  conv *gen = (conv *)ptr;
  Float result;
  if (gen->ctr >= gen->fftsize2)
    {
      int i, j;
      Float (*conv_input)(void *arg, int direction) = input;
      if (conv_input == NULL) conv_input = gen->feeder;
      for (i = 0, j = gen->fftsize2; i < gen->fftsize2; i++, j++) 
	{
	  gen->buf[i] = gen->buf[j]; 
	  gen->buf[j] = 0.0;
	  gen->rl1[i] = (*conv_input)(gen->closure, 1);
	  gen->rl1[j] = 0.0;
	  gen->rl2[i] = 0.0;
	  gen->rl2[j] = 0.0;
	}
      memcpy((void *)(gen->rl2), (void *)(gen->filter), gen->filtersize * sizeof(Float));
      /* for (i = 0; i < gen->filtersize; i++) gen->rl2[i] = gen->filter[i]; */
      mus_convolution(gen->rl1, gen->rl2, gen->fftsize);
      for (i = 0, j = gen->fftsize2; i < gen->fftsize2; i++, j++) 
	{
	  gen->buf[i] += gen->rl1[i];
	  gen->buf[j] = gen->rl1[j];
	}
      gen->ctr = 0;
    }
  result = gen->buf[gen->ctr];
  gen->ctr++;
  return(result);
}

bool mus_convolve_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_CONVOLVE));}

mus_any *mus_make_convolve(Float (*input)(void *arg, int direction), Float *filter, int fftsize, int filtersize, void *closure)
{
  conv *gen = NULL;
  gen = (conv *)clm_calloc(1, sizeof(conv), S_make_convolve);
  gen->core = &CONVOLVE_CLASS;
  gen->feeder = input;
  gen->closure = closure;
  gen->filter = filter;
  if (filter)
    {
      int i;
      bool all_zero = true;
      for (i = 0; i < filtersize; i++)
	if (fabs(filter[i]) != 0.0) /* I'm getting -0.000 != 0.000 */
	  {
	    all_zero = false;
	    break;
	  }
      if (all_zero)
	mus_print("make_convolve: filter contains only 0.0.");
    }
  gen->filtersize = filtersize;
  gen->fftsize = fftsize;
  gen->fftsize2 = gen->fftsize / 2;
  gen->ctr = gen->fftsize2;
  gen->rl1 = (Float *)clm_calloc(fftsize, sizeof(Float), "convolve fft data");
  gen->rl2 = (Float *)clm_calloc(fftsize, sizeof(Float), "convolve fft data");
  gen->buf = (Float *)clm_calloc(fftsize, sizeof(Float), "convolve fft data");
  return((mus_any *)gen);
}

void mus_convolve_files(const char *file1, const char *file2, Float maxamp, const char *output_file)
{
  off_t file1_len, file2_len, outlen, totallen;
  int fftlen, file1_chans, file2_chans, output_chans;
  Float *data1, *data2;
  mus_sample_t *samps;
  char *errmsg = NULL;
  Float maxval = 0.0;
  int i;
  file1_len = mus_sound_frames(file1);
  file2_len = mus_sound_frames(file2);
  if ((file1_len <= 0) || (file2_len <= 0)) return;
  file1_chans = mus_sound_chans(file1);
  if (file1_chans <= 0) mus_error(MUS_NO_CHANNELS, "%s chans: %d", file1, file1_chans);
  file2_chans = mus_sound_chans(file2);
  if (file2_chans <= 0) mus_error(MUS_NO_CHANNELS, "%s chans: %d", file2, file2_chans);
  output_chans = file1_chans; 
  if (file2_chans > output_chans) output_chans = file2_chans;
  fftlen = (int)(pow(2.0, (int)ceil(log(file1_len + file2_len + 1) / log(2.0))));
  outlen = file1_len + file2_len + 1;
  totallen = outlen * output_chans;
  data1 = (Float *)clm_calloc(fftlen, sizeof(Float), "convolve_files data");
  data2 = (Float *)clm_calloc(fftlen, sizeof(Float), "convolve_files data");
  if (output_chans == 1)
    {
      samps = (mus_sample_t *)clm_calloc(fftlen, sizeof(mus_sample_t), "convolve_files data");
      mus_file_to_array(file1, 0, 0, file1_len, samps); 
      for (i = 0; i < file1_len; i++) data1[i] = MUS_SAMPLE_TO_DOUBLE(samps[i]);
      mus_file_to_array(file2, 0, 0, file2_len, samps);
      for (i = 0; i < file2_len; i++) data2[i] = MUS_SAMPLE_TO_DOUBLE(samps[i]);
      mus_convolution(data1, data2, fftlen);
      for (i = 0; i < outlen; i++) 
	if (maxval < fabs(data1[i])) 
	  maxval = fabs(data1[i]);
      if (maxval > 0.0)
	{
	  maxval = maxamp / maxval;
	  for (i = 0; i < outlen; i++) data1[i] *= maxval;
	}
      for (i = 0; i < outlen; i++) samps[i] = MUS_DOUBLE_TO_SAMPLE(data1[i]);
      errmsg = mus_array_to_file_with_error(output_file, samps, outlen, mus_sound_srate(file1), 1);
      FREE(samps);
    }
  else
    {
      Float *outdat;
      int c1, c2;
      samps = (mus_sample_t *)clm_calloc(totallen, sizeof(mus_sample_t), "convolve_files data");
      outdat = (Float *)clm_calloc(totallen, sizeof(Float), "convolve_files data");
      c1 = 0; 
      c2 = 0;
      for (i = 0; i < output_chans; i++)
	{
	  int j, k;
	  mus_file_to_array(file1, c1, 0, file1_len, samps);
	  for (i = 0; i < file1_len; i++) data1[i] = MUS_SAMPLE_TO_DOUBLE(samps[i]);
	  mus_file_to_array(file2, c2, 0, file2_len, samps);
	  for (i = 0; i < file2_len; i++) data2[i] = MUS_SAMPLE_TO_DOUBLE(samps[i]);
	  mus_convolution(data1, data2, fftlen);
	  for (j = i, k = 0; j < totallen; j += output_chans, k++) outdat[j] = data1[k];
	  c1++; 
	  if (c1 >= file1_chans) c1 = 0;
	  c2++; 
	  if (c2 >= file2_chans) c2 = 0;
	  mus_clear_array(data1, fftlen);
	  mus_clear_array(data2, fftlen);
	}
      for (i = 0; i < totallen; i++) 
	if (maxval < fabs(outdat[i])) 
	  maxval = fabs(outdat[i]);
      if (maxval > 0.0)
	{
	  maxval = maxamp / maxval;
	  for (i = 0; i < totallen; i++) outdat[i] *= maxval;
	}
      for (i = 0; i < totallen; i++) samps[i] = MUS_DOUBLE_TO_SAMPLE(outdat[i]);
      errmsg = mus_array_to_file_with_error(output_file, samps, totallen, mus_sound_srate(file1), output_chans);
      FREE(samps);
      FREE(outdat);
    }
  FREE(data1);
  FREE(data2);
  if (errmsg)
    mus_error(MUS_CANT_OPEN_FILE, errmsg);
}



/* ---------------- phase-vocoder ---------------- */

typedef struct {
  mus_any_class *core;
  Float pitch;
  Float (*input)(void *arg, int direction);
  void *closure;
  bool (*analyze)(void *arg, Float (*input)(void *arg1, int direction));
  int (*edit)(void *arg);
  Float (*synthesize)(void *arg);
  int outctr, interp, filptr, N, D;
  Float *win, *ampinc, *amps, *freqs, *phases, *phaseinc, *lastphase, *in_data;
} pv_info;

bool mus_phase_vocoder_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_PHASE_VOCODER));}

static bool phase_vocoder_equalp(mus_any *p1, mus_any *p2) {return(p1 == p2);}

static char *describe_phase_vocoder(mus_any *ptr)
{
  char *arr = NULL;
  pv_info *gen = (pv_info *)ptr;
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE,
	       S_phase_vocoder ": outctr: %d, interp: %d, filptr: %d, N: %d, D: %d, in_data: %s",
	       gen->outctr, gen->interp, gen->filptr, gen->N, gen->D,
	       arr = float_array_to_string(gen->in_data, gen->N, 0));
  if (arr) FREE(arr);
  return(describe_buffer);
}

static int free_phase_vocoder(mus_any *ptr)
{
  pv_info *gen = (pv_info *)ptr;
  if (gen)
    {
      if (gen->in_data) FREE(gen->in_data);
      if (gen->amps) FREE(gen->amps);
      if (gen->freqs) FREE(gen->freqs);
      if (gen->phases) FREE(gen->phases);
      if (gen->win) FREE(gen->win);
      if (gen->phaseinc) FREE(gen->phaseinc);
      if (gen->lastphase) FREE(gen->lastphase);
      if (gen->ampinc) FREE(gen->ampinc);
      FREE(gen);
    }
  return(0);
}

static off_t pv_length(mus_any *ptr) {return(((pv_info *)ptr)->N);}

static off_t pv_hop(mus_any *ptr) {return(((pv_info *)ptr)->D);}
static off_t pv_set_hop(mus_any *ptr, off_t val) {((pv_info *)ptr)->D = (int)val; return(val);}

static Float pv_frequency(mus_any *ptr) {return(((pv_info *)ptr)->pitch);}
static Float pv_set_frequency(mus_any *ptr, Float val) {((pv_info *)ptr)->pitch = val; return(val);}

static void *pv_closure(mus_any *rd) {return(((pv_info *)rd)->closure);}
static void *pv_set_closure(mus_any *rd, void *e) {((pv_info *)rd)->closure = e; return(e);}

Float *mus_phase_vocoder_amp_increments(mus_any *ptr) {return(((pv_info *)ptr)->ampinc);}
Float *mus_phase_vocoder_amps(mus_any *ptr) {return(((pv_info *)ptr)->amps);}

Float *mus_phase_vocoder_freqs(mus_any *ptr) {return(((pv_info *)ptr)->freqs);}
Float *mus_phase_vocoder_phases(mus_any *ptr) {return(((pv_info *)ptr)->phases);}
Float *mus_phase_vocoder_phase_increments(mus_any *ptr) {return(((pv_info *)ptr)->phaseinc);}

int mus_phase_vocoder_outctr(mus_any *ptr) {return(((pv_info *)ptr)->outctr);}
int mus_phase_vocoder_set_outctr(mus_any *ptr, int val) {((pv_info *)ptr)->outctr = val; return(val);}

static Float run_phase_vocoder(mus_any *ptr, Float unused1, Float unused2) {return(mus_phase_vocoder(ptr, NULL));}

static Float pv_increment(mus_any *rd) {return((Float)(((pv_info *)rd)->interp));}
static Float pv_set_increment(mus_any *rd, Float val) {((pv_info *)rd)->interp = (int)val; return(val);}

static void pv_reset(mus_any *ptr)
{
  pv_info *gen = (pv_info *)ptr;
  if (gen->in_data) FREE(gen->in_data);
  gen->in_data = NULL;
  gen->outctr = gen->interp;
  gen->filptr = 0;
  mus_clear_array(gen->ampinc, gen->N);
  mus_clear_array(gen->freqs, gen->N);
  mus_clear_array(gen->amps, gen->N / 2);
  mus_clear_array(gen->phases, gen->N / 2);
  mus_clear_array(gen->lastphase, gen->N / 2);
  mus_clear_array(gen->phaseinc, gen->N / 2);
}

static mus_any_class PHASE_VOCODER_CLASS = {
  MUS_PHASE_VOCODER,
  S_phase_vocoder,
  &free_phase_vocoder,
  &describe_phase_vocoder,
  &phase_vocoder_equalp,
  0, 0,
  &pv_length, 0,
  &pv_frequency,
  &pv_set_frequency,
  0, 0,
  0, 0,
  &pv_increment,
  &pv_set_increment,
  &run_phase_vocoder,
  MUS_NOT_SPECIAL,
  &pv_closure,
  0,
  0, 0, 0, 0, 0, 0, 
  &pv_hop, &pv_set_hop, 
  0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  &pv_reset,
  &pv_set_closure
};

mus_any *mus_make_phase_vocoder(Float (*input)(void *arg, int direction), 
				int fftsize, int overlap, int interp,
				Float pitch,
				bool (*analyze)(void *arg, Float (*input)(void *arg1, int direction)),
				int (*edit)(void *arg), 
				Float (*synthesize)(void *arg), 
				void *closure)
{
  /* order of args is trying to match src, granulate etc
   *   the inclusion of pitch and interp provides built-in time/pitch scaling which is 99% of phase-vocoder use
   */
  pv_info *pv;
  int N2, D, i;
  Float scl;
  N2 = (int)(fftsize / 2);
  if (N2 == 0) return(NULL);
  D = fftsize / overlap;
  if (D == 0) return(NULL);
  pv = (pv_info *)clm_calloc(1, sizeof(pv_info), S_make_phase_vocoder);
  pv->core = &PHASE_VOCODER_CLASS;
  pv->N = fftsize;
  pv->D = D;
  pv->interp = interp;
  pv->outctr = interp;
  pv->filptr = 0;
  pv->pitch = pitch;
  pv->ampinc = (Float *)clm_calloc(fftsize, sizeof(Float), "pvoc ampinc");
  pv->freqs = (Float *)clm_calloc(fftsize, sizeof(Float), "pvoc freqs");
  pv->amps = (Float *)clm_calloc(N2, sizeof(Float), "pvoc amps");
  pv->phases = (Float *)clm_calloc(N2, sizeof(Float), "pvoc phases");
  pv->lastphase = (Float *)clm_calloc(N2, sizeof(Float), "pvoc lastphase");
  pv->phaseinc = (Float *)clm_calloc(N2, sizeof(Float), "pvoc phaseinc");
  pv->in_data = NULL;
  pv->input = input;
  pv->closure = closure;
  pv->analyze = analyze;
  pv->edit = edit;
  pv->synthesize = synthesize;
  pv->win = mus_make_fft_window(MUS_HAMMING_WINDOW, fftsize, 0.0);
  scl = 2.0 / (0.54 * (Float)fftsize);
  if (pv->win) /* clm2xen traps errors for later reporting (to clean up local allocation),
		*   so clm_calloc might return NULL in all the cases here
		*/
    for (i = 0; i < fftsize; i++) 
      pv->win[i] *= scl;
  return((mus_any *)pv);
}

Float mus_phase_vocoder_with_editors(mus_any *ptr, 
				     Float (*input)(void *arg, int direction),
				     bool (*analyze)(void *arg, Float (*input)(void *arg1, int direction)),
				     int (*edit)(void *arg), 
				     Float (*synthesize)(void *arg))
{
  pv_info *pv = (pv_info *)ptr;
  int N2, i;
  Float (*pv_synthesize)(void *arg) = synthesize;
  if (pv_synthesize == NULL) pv_synthesize = pv->synthesize;
  N2 = pv->N / 2;
  if (pv->outctr >= pv->interp)
    {
      Float scl;
      Float (*pv_input)(void *arg, int direction) = input;
      bool (*pv_analyze)(void *arg, Float (*input)(void *arg1, int direction)) = analyze;
      int (*pv_edit)(void *arg) = edit;
      if (pv_input == NULL) 
	{
	  pv_input = pv->input;
	  if (pv_input == NULL)
	    mus_error(MUS_NO_SAMPLE_INPUT, "%s has no input function!", mus_describe(ptr));
	}
      if (pv_analyze == NULL) pv_analyze = pv->analyze;
      if (pv_edit == NULL) pv_edit = pv->edit;

      pv->outctr = 0;
      if ((pv_analyze == NULL) || ((*pv_analyze)(pv->closure, pv_input)))
	{
	  int j, buf;
	  mus_clear_array(pv->freqs, pv->N);
	  if (pv->in_data == NULL)
	    {
	      pv->in_data = (Float *)clm_calloc(pv->N, sizeof(Float), "pvoc indata");
	      for (i = 0; i < pv->N; i++) pv->in_data[i] = (*pv_input)(pv->closure, 1);
	    }
	  else
	    {
	      for (i = 0, j = pv->D; j < pv->N; i++, j++) pv->in_data[i] = pv->in_data[j];
	      for (i = pv->N - pv->D; i < pv->N; i++) pv->in_data[i] = (*pv_input)(pv->closure, 1);
	    }
	  buf = pv->filptr % pv->N;
	  for (i = 0; i < pv->N; i++)
	    {
	      pv->ampinc[buf++] = pv->win[i] * pv->in_data[i];
	      if (buf >= pv->N) buf = 0;
	    }
	  pv->filptr += pv->D;
	  mus_fft(pv->ampinc, pv->freqs, pv->N, 1);
	  mus_rectangular_to_polar(pv->ampinc, pv->freqs, N2);
	}
      
      if ((pv_edit == NULL) || ((*pv_edit)(pv->closure)))
	{
	  Float pscl, kscl, ks;
	  pscl = 1.0 / (Float)(pv->D);
	  kscl = TWO_PI / (Float)(pv->N);
	  for (i = 0, ks = 0.0; i < N2; i++, ks += kscl)
	    {
	      Float diff;
	      diff = pv->freqs[i] - pv->lastphase[i];
	      pv->lastphase[i] = pv->freqs[i];
	      while (diff > M_PI) diff -= TWO_PI;
	      while (diff < -M_PI) diff += TWO_PI;
	      pv->freqs[i] = pv->pitch * (diff * pscl + ks);
	    }
	}
      
      scl = 1.0 / (Float)(pv->interp);
      for (i = 0; i < N2; i++)
	{
	  pv->ampinc[i] = scl * (pv->ampinc[i] - pv->amps[i]);
	  pv->freqs[i] = scl * (pv->freqs[i] - pv->phaseinc[i]);
	}
    }
  
  pv->outctr++;
  if (pv_synthesize)
    return((*pv_synthesize)(pv->closure));
  for (i = 0; i < N2; i++)
    {
      pv->amps[i] += pv->ampinc[i];
      pv->phaseinc[i] += pv->freqs[i];
      pv->phases[i] += pv->phaseinc[i];
    }
  return(mus_sine_bank(pv->amps, pv->phases, N2));
}

Float mus_phase_vocoder(mus_any *ptr, Float (*input)(void *arg, int direction))
{
  return(mus_phase_vocoder_with_editors(ptr, input, NULL, NULL, NULL));
}



/* ---------------- single sideband "suppressed carrier" amplitude modulation (ssb-am) ---------------- */

typedef struct {
  mus_any_class *core;
  bool shift_up;
  Float *coeffs;
  mus_any *sin_osc, *cos_osc, *hilbert, *dly;
} ssbam;

bool mus_ssb_am_p(mus_any *ptr) {return((ptr) && (ptr->core->type == MUS_SSB_AM));}

static Float run_hilbert(flt *g, Float insig)
{
  /* every odd-numbered entry in the coeffs array is 0 in this filter
   *   but we have to run the loop twice to skip the 0 mults --
   *   not very elaborate timing tests indicate all this silliness saves 10% compute time
   */
  int i, len;
  Float val = 0.0;
  len = g->order;
  g->state[0] = insig;
  for (i = 0; i < len; i += 2) val += (g->x[i] * g->state[i]);
  for (i = len - 1; i >= 1; i--) g->state[i] = g->state[i - 1];
  return(val);
}

Float mus_ssb_am_1(mus_any *ptr, Float insig)
{
  ssbam *gen = (ssbam *)ptr;
  return((mus_oscil_0(gen->cos_osc) * mus_delay_1(gen->dly, insig)) +
	 (mus_oscil_0(gen->sin_osc) * run_hilbert((flt *)(gen->hilbert), insig)));
}

Float mus_ssb_am(mus_any *ptr, Float insig, Float fm)
{
  ssbam *gen = (ssbam *)ptr;
  return((mus_oscil_1(gen->cos_osc, fm) * mus_delay_1(gen->dly, insig)) +
	 (mus_oscil_1(gen->sin_osc, fm) * run_hilbert((flt *)(gen->hilbert), insig)));
}

static int free_ssb_am(mus_any *ptr) 
{
  if (ptr) 
    {
      ssbam *gen = (ssbam *)ptr;
      mus_free(gen->dly);
      mus_free(gen->hilbert);
      mus_free(gen->cos_osc);
      mus_free(gen->sin_osc);
      if (gen->coeffs) {FREE(gen->coeffs); gen->coeffs = NULL;}
      FREE(ptr); 
    }
  return(0);
}

static Float ssb_am_freq(mus_any *ptr) 
{
  return(mus_radians_to_hz(((osc *)((ssbam *)ptr)->sin_osc)->freq));
}

static Float set_ssb_am_freq(mus_any *ptr, Float val) 
{
  ssbam *gen = (ssbam *)ptr;
  Float rads;
  rads = mus_hz_to_radians(val);
  ((osc *)(gen->sin_osc))->freq = rads;
  ((osc *)(gen->cos_osc))->freq = rads;
  return(val);
}

static Float ssb_am_phase(mus_any *ptr) 
{
  return(fmod(((osc *)((ssbam *)ptr)->cos_osc)->phase - 0.5 * M_PI, TWO_PI));
}

static Float set_ssb_am_phase(mus_any *ptr, Float val) 
{
  ssbam *gen = (ssbam *)ptr;
  if (gen->shift_up)
    ((osc *)(gen->sin_osc))->phase = val + M_PI;
  else ((osc *)(gen->sin_osc))->phase = val; 
  ((osc *)(gen->cos_osc))->phase = val + 0.5 * M_PI;
  return(val);
}

static off_t ssb_am_cosines(mus_any *ptr) {return(1);}
static off_t ssb_am_order(mus_any *ptr) {return(mus_order(((ssbam *)ptr)->dly));}
static int ssb_am_interp_type(mus_any *ptr) {return(delay_interp_type(((ssbam *)ptr)->dly));}

static Float *ssb_am_data(mus_any *ptr) {return(filter_data(((ssbam *)ptr)->hilbert));}
static Float ssb_am_run(mus_any *ptr, Float insig, Float fm) {return(mus_ssb_am(ptr, insig, fm));}

static Float *ssb_am_xcoeffs(mus_any *ptr) {return(mus_xcoeffs(((ssbam *)ptr)->hilbert));}
static Float ssb_am_xcoeff(mus_any *ptr, int index) {return(mus_xcoeff(((ssbam *)ptr)->hilbert, index));}
static Float ssb_am_set_xcoeff(mus_any *ptr, int index, Float val) {return(mus_set_xcoeff(((ssbam *)ptr)->hilbert, index, val));}

static bool ssb_am_equalp(mus_any *p1, mus_any *p2)
{
  return((p1 == p2) ||
	 ((mus_ssb_am_p((mus_any *)p1)) && 
	  (mus_ssb_am_p((mus_any *)p2)) &&
	  (((ssbam *)p1)->shift_up == ((ssbam *)p2)->shift_up) &&
	  (mus_equalp(((ssbam *)p1)->sin_osc, ((ssbam *)p2)->sin_osc)) &&
	  (mus_equalp(((ssbam *)p1)->cos_osc, ((ssbam *)p2)->cos_osc)) &&
	  (mus_equalp(((ssbam *)p1)->dly, ((ssbam *)p2)->dly)) &&
	  (mus_equalp(((ssbam *)p1)->hilbert, ((ssbam *)p2)->hilbert))));
}

static char *describe_ssb_am(mus_any *ptr)
{
  ssbam *gen = (ssbam *)ptr;
  mus_snprintf(describe_buffer, DESCRIBE_BUFFER_SIZE, S_ssb_am ": shift: %s, sin/cos: %f Hz (%f radians), order: %d",
	       (gen->shift_up) ? "up" : "down",
	       mus_frequency(ptr),
	       mus_phase(ptr),
	       (int)mus_order(ptr));
  return(describe_buffer);
}

static void ssb_reset(mus_any *ptr)
{
  ssbam *gen = (ssbam *)ptr;
  set_ssb_am_phase(ptr, 0.0);
  mus_reset(gen->dly);
  mus_reset(gen->hilbert);
}

static mus_any_class SSB_AM_CLASS = {
  MUS_SSB_AM,
  S_ssb_am,
  &free_ssb_am,
  &describe_ssb_am,
  &ssb_am_equalp,
  &ssb_am_data, 0,
  &ssb_am_order, 0,
  &ssb_am_freq,
  &set_ssb_am_freq,
  &ssb_am_phase,
  &set_ssb_am_phase,
  0, 0, 0, 0,
  &ssb_am_run,
  MUS_NOT_SPECIAL, 
  NULL,
  &ssb_am_interp_type,
  0, 0, 0, 0,
  &ssb_am_xcoeff, &ssb_am_set_xcoeff, 
  &ssb_am_cosines, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 
  &ssb_am_xcoeffs, 0, 0,
  &ssb_reset,
  0
};

mus_any *mus_make_ssb_am(Float freq, int order)
{
  ssbam *gen;
  int i, k, len;
  if ((order & 1) == 0) order++; /* if order is even, the first Hilbert coeff is 0.0 */
  gen = (ssbam *)clm_calloc(1, sizeof(ssbam), S_make_ssb_am);
  gen->core = &SSB_AM_CLASS;
  if (freq > 0)
    gen->shift_up = true;
  else gen->shift_up = false;
  gen->sin_osc = mus_make_oscil(fabs(freq), (gen->shift_up) ? M_PI : 0.0);
  gen->cos_osc = mus_make_oscil(fabs(freq), M_PI * 0.5);
  gen->dly = mus_make_delay(order, NULL, order, MUS_INTERP_NONE);
  len = order * 2 + 1;
  gen->coeffs = (Float *)CALLOC(len, sizeof(Float));
  for (i = -order, k = 0; i <= order; i++, k++)
    {
      Float denom, num;
      denom = i * M_PI;
      num = 1.0 - cos(denom);
      if (i == 0)
	gen->coeffs[k] = 0.0;
      else gen->coeffs[k] = (num / denom) * (0.54 + (0.46 * cos(denom / order)));
    }
  gen->hilbert = mus_make_fir_filter(len, gen->coeffs, NULL);
  return((mus_any *)gen);
}



/* ---------------- mix files ---------------- */

/* a mixing "instrument" along the lines of the mix function in clm */
/* this is a very commonly used function, so it's worth picking out the special cases for optimization */

#define IDENTITY_MIX 0
#define IDENTITY_MONO_MIX 1
#define SCALED_MONO_MIX 2
#define SCALED_MIX 3
#define ENVELOPED_MONO_MIX 4
#define ENVELOPED_MIX 5
#define ALL_MIX 6

static int mix_type(int out_chans, int in_chans, mus_any *umx, mus_any ***envs)
{
  mus_mixer *mx = (mus_mixer *)umx;
  if (envs)
    {
      if ((in_chans == 1) && (out_chans == 1)) 
	return(ENVELOPED_MONO_MIX);
      else 
	{
	  if (mx)
	    return(ALL_MIX);
	  return(ENVELOPED_MIX); 
	}
    }
  if (mx)
    {
      int i, j;
      if ((in_chans == 1) && (out_chans == 1)) 
	{
	  if (mx->vals[0][0] == 1.0)
	    return(IDENTITY_MONO_MIX); 
	  return(SCALED_MONO_MIX);
	}
      for (i = 0; i < mx->chans; i++)
	for (j = 0; j < mx->chans; j++)
	  if (((i == j) && (mx->vals[i][i] != 1.0)) ||
	      ((i != j) && (mx->vals[i][j] != 0.0)))
	    return(SCALED_MIX);
    }
  if ((in_chans == 1) && (out_chans == 1)) 
    return(IDENTITY_MONO_MIX);
  return(IDENTITY_MIX);
}

void mus_mix_with_reader_and_writer(mus_any *outf, mus_any *inf, off_t out_start, off_t out_frames, off_t in_start, mus_any *umx, mus_any ***envs)
{
  int in_chans, out_chans, mix_chans, mixtype;
  mus_mixer *mx = (mus_mixer *)umx;
  off_t inc, outc, offi;
  mus_frame *frin, *frthru = NULL;
  out_chans = mus_channels(outf);
  if (out_chans <= 0) mus_error(MUS_NO_CHANNELS, "%s chans: %d", mus_describe(outf), out_chans);
  in_chans = mus_channels(inf);
  if (in_chans <= 0) mus_error(MUS_NO_CHANNELS, "%s chans: %d", mus_describe(inf), in_chans);
  if (out_chans > in_chans) mix_chans = out_chans; else mix_chans = in_chans;
  mixtype = mix_type(out_chans, in_chans, umx, envs);
  frin = (mus_frame *)mus_make_empty_frame(mix_chans);
  frthru = (mus_frame *)mus_make_empty_frame(mix_chans);
  switch (mixtype)
    {
    case ENVELOPED_MONO_MIX:
    case ENVELOPED_MIX:
      if (umx == NULL) mx = (mus_mixer *)mus_make_identity_mixer(mix_chans); /* fall through */
    case ALL_MIX:
      /* the general case -- possible envs/scalers on every mixer cell */
      for (offi = 0, inc = in_start, outc = out_start; offi < out_frames; offi++, inc++, outc++)
	{
	  int j, k;
	  for (j = 0; j < in_chans; j++)
	    for (k = 0; k < out_chans; k++)
	      if (envs[j][k])
		mx->vals[j][k] = mus_env(envs[j][k]);
	  mus_frame_to_file(outf, 
			    outc, 
			    mus_frame_to_frame(mus_file_to_frame(inf, inc, (mus_any *)frin), 
					       (mus_any *)mx, 
					       (mus_any *)frthru));
	}
      if (umx == NULL) mus_free((mus_any *)mx);
      break;
    case IDENTITY_MIX:
    case IDENTITY_MONO_MIX:
      for (offi = 0, inc = in_start, outc = out_start; offi < out_frames; offi++, inc++, outc++)
	mus_frame_to_file(outf, outc, mus_file_to_frame(inf, inc, (mus_any *)frin));
      break;
    case SCALED_MONO_MIX:
    case SCALED_MIX:
      for (offi = 0, inc = in_start, outc = out_start; offi < out_frames; offi++, inc++, outc++)
	mus_frame_to_file(outf, 
			  outc, 
			  mus_frame_to_frame(mus_file_to_frame(inf, inc, (mus_any *)frin), 
					     (mus_any *)mx, 
					     (mus_any *)frthru));
      break;

    }
  mus_free((mus_any *)frin);
  mus_free((mus_any *)frthru);
}


void mus_mix(const char *outfile, const char *infile, off_t out_start, off_t out_frames, off_t in_start, mus_any *umx, mus_any ***envs)
{
  int in_chans, out_chans, min_chans, mixtype;
  out_chans = mus_sound_chans(outfile);
  if (out_chans <= 0) mus_error(MUS_NO_CHANNELS, "%s chans: %d", outfile, out_chans);
  in_chans = mus_sound_chans(infile);
  if (in_chans <= 0) mus_error(MUS_NO_CHANNELS, "%s chans: %d", infile, in_chans);
  if (out_chans > in_chans) min_chans = in_chans; else min_chans = out_chans;
  mixtype = mix_type(out_chans, in_chans, umx, envs);
  if (mixtype == ALL_MIX)
    {
      mus_any *inf, *outf;
      /* the general case -- possible envs/scalers on every mixer cell */
      outf = mus_continue_sample_to_file(outfile);
      inf = mus_make_file_to_frame(infile);
      mus_mix_with_reader_and_writer(outf, inf, out_start, out_frames, in_start, umx, envs);
      mus_free((mus_any *)inf);
      mus_free((mus_any *)outf);
    }
  else
    {
      mus_mixer *mx = (mus_mixer *)umx;
      int i, j = 0, m, ofd, ifd;
      Float scaler;
      mus_any *e;
      mus_sample_t **obufs, **ibufs;
      off_t offk, curoutframes;
      /* highly optimizable cases */
      obufs = (mus_sample_t **)clm_calloc(out_chans, sizeof(mus_sample_t *), "mix output");
      for (i = 0; i < out_chans; i++) 
	obufs[i] = (mus_sample_t *)clm_calloc(clm_file_buffer_size, sizeof(mus_sample_t), "mix output buffers");
      ibufs = (mus_sample_t **)clm_calloc(in_chans, sizeof(mus_sample_t *), "mix input");
      for (i = 0; i < in_chans; i++) 
	ibufs[i] = (mus_sample_t *)clm_calloc(clm_file_buffer_size, sizeof(mus_sample_t), "mix input buffers");
      ifd = mus_sound_open_input(infile);
      mus_file_seek_frame(ifd, in_start);
      mus_file_read(ifd, 0, clm_file_buffer_size - 1, in_chans, ibufs);
      ofd = mus_sound_reopen_output(outfile, 
				    out_chans, 
				    mus_sound_data_format(outfile), 
				    mus_sound_header_type(outfile), 
				    mus_sound_data_location(outfile));
      curoutframes = mus_sound_frames(outfile);
      mus_file_seek_frame(ofd, out_start);
      mus_file_read(ofd, 0, clm_file_buffer_size - 1, out_chans, obufs);
      mus_file_seek_frame(ofd, out_start);
      switch (mixtype)
	{
	case IDENTITY_MONO_MIX:
	  for (offk = 0, j = 0; offk < out_frames; offk++, j++)
	    {
	      if (j == clm_file_buffer_size)
		{
		  mus_file_write(ofd, 0, j - 1, out_chans, obufs);
		  j = 0;
		  mus_file_seek_frame(ofd, out_start + offk);
		  mus_file_read(ofd, 0, clm_file_buffer_size - 1, out_chans, obufs);
		  mus_file_seek_frame(ofd, out_start + offk);
		  mus_file_read(ifd, 0, clm_file_buffer_size - 1, in_chans, ibufs);
		}
	      obufs[0][j] += ibufs[0][j];
	    }
	  break;

	case IDENTITY_MIX:
	  for (offk = 0, j = 0; offk < out_frames; offk++, j++)
	    {
	      if (j == clm_file_buffer_size)
		{
		  mus_file_write(ofd, 0, j - 1, out_chans, obufs);
		  j = 0;
		  mus_file_seek_frame(ofd, out_start + offk);
		  mus_file_read(ofd, 0, clm_file_buffer_size - 1, out_chans, obufs);
		  mus_file_seek_frame(ofd, out_start + offk);
		  mus_file_read(ifd, 0, clm_file_buffer_size - 1, in_chans, ibufs);
		}
	      for (i = 0; i < min_chans; i++)
		obufs[i][j] += ibufs[i][j];
	    }
	  break;

	case SCALED_MONO_MIX:
	  scaler = mx->vals[0][0];
	  for (offk = 0, j = 0; offk < out_frames; offk++, j++)
	    {
	      if (j == clm_file_buffer_size)
		{
		  mus_file_write(ofd, 0, j - 1, out_chans, obufs);
		  j = 0;
		  mus_file_seek_frame(ofd, out_start + offk);
		  mus_file_read(ofd, 0, clm_file_buffer_size - 1, out_chans, obufs);
		  mus_file_seek_frame(ofd, out_start + offk);
		  mus_file_read(ifd, 0, clm_file_buffer_size - 1, in_chans, ibufs);
		}
	      obufs[0][j] += (mus_sample_t)(scaler * ibufs[0][j]);
	    }
	  break;

	case SCALED_MIX:
	  for (offk = 0, j = 0; offk < out_frames; offk++, j++)
	    {
	      if (j == clm_file_buffer_size)
		{
		  mus_file_write(ofd, 0, j - 1, out_chans, obufs);
		  j = 0;
		  mus_file_seek_frame(ofd, out_start + offk);
		  mus_file_read(ofd, 0, clm_file_buffer_size- 1 , out_chans, obufs);
		  mus_file_seek_frame(ofd, out_start + offk);
		  mus_file_read(ifd, 0, clm_file_buffer_size - 1, in_chans, ibufs);
		}
	      for (i = 0; i < min_chans; i++)
		for (m = 0; m < in_chans; m++)
		  obufs[i][j] += (mus_sample_t)(ibufs[m][j] * mx->vals[m][i]);
	    }
	  break;

	case ENVELOPED_MONO_MIX:
	  e = envs[0][0];
	  for (offk = 0, j = 0; offk < out_frames; offk++, j++)
	    {
	      if (j == clm_file_buffer_size)
		{
		  mus_file_write(ofd, 0, j - 1, out_chans, obufs);
		  j = 0;
		  mus_file_seek_frame(ofd, out_start + offk);
		  mus_file_read(ofd, 0, clm_file_buffer_size - 1, out_chans, obufs);
		  mus_file_seek_frame(ofd, out_start + offk);
		  mus_file_read(ifd, 0, clm_file_buffer_size - 1, in_chans, ibufs);
		}
	      obufs[0][j] += (mus_sample_t)(mus_env(e) * ibufs[0][j]);
	    }
	  break;

	case ENVELOPED_MIX:
	  e = envs[0][0];
	  for (offk = 0, j = 0; offk < out_frames; offk++, j++)
	    {
	      if (j == clm_file_buffer_size)
		{
		  mus_file_write(ofd, 0, j - 1, out_chans, obufs);
		  j = 0;
		  mus_file_seek_frame(ofd, out_start + offk);
		  mus_file_read(ofd, 0, clm_file_buffer_size - 1, out_chans, obufs);
		  mus_file_seek_frame(ofd, out_start + offk);
		  mus_file_read(ifd, 0, clm_file_buffer_size - 1, in_chans, ibufs);
		}
	      scaler = mus_env(e);
	      for (i = 0; i < min_chans; i++)
		obufs[i][j] += (mus_sample_t)(scaler * ibufs[i][j]);
	    }
	  break;

	}
      if (j > 0) 
	mus_file_write(ofd, 0, j - 1, out_chans, obufs);
      if (curoutframes < (out_frames + out_start)) 
	curoutframes = out_frames + out_start;
      mus_sound_close_output(ofd, 
			     curoutframes * out_chans * mus_bytes_per_sample(mus_sound_data_format(outfile)));
      mus_sound_close_input(ifd);
      for (i = 0; i < in_chans; i++) FREE(ibufs[i]);
      FREE(ibufs);
      for (i = 0; i < out_chans; i++) FREE(obufs[i]);
      FREE(obufs);
    }
}



/* ---------------- mus-apply ---------------- */

Float mus_apply(mus_any *gen, Float f1, Float f2)
{
  /* what about non-gen funcs such as polynomial, ring_modulate etc? */
  if ((gen) && (MUS_RUN_P(gen)))
    return(MUS_RUN(gen, f1, f2));
  return(0.0);
}


void init_mus_module(void)
{
  #define MULAW_ZERO 255
  #define ALAW_ZERO 213
  #define UBYTE_ZERO 128
  #define USHORT_ZERO 32768

  mus_class_tag = MUS_INITIAL_GEN_TAG;
  sampling_rate = MUS_DEFAULT_SAMPLING_RATE;
  w_rate = (TWO_PI / MUS_DEFAULT_SAMPLING_RATE);
  array_print_length = MUS_DEFAULT_ARRAY_PRINT_LENGTH;
  clm_file_buffer_size = MUS_DEFAULT_FILE_BUFFER_SIZE;
#if HAVE_FFTW3 || HAVE_FFTW
  last_fft_size = 0;
#endif
  sum_of_sines_50 = .743;
  sum_of_sines_100 = .733;
  sincs = 0;

  data_format_zero = (int *)CALLOC(MUS_NUM_DATA_FORMATS, sizeof(int));
  data_format_zero[MUS_MULAW] = MULAW_ZERO;
  data_format_zero[MUS_ALAW] = ALAW_ZERO;
  data_format_zero[MUS_UBYTE] = UBYTE_ZERO;
  data_format_zero[MUS_UBSHORT] = USHORT_ZERO;
  data_format_zero[MUS_ULSHORT] = USHORT_ZERO;
}

