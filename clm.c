/* CLM (music V) implementation as a C module */
/*
 * in gdb, p mus_describe(arg) will show the user-view of arg
 *         p mus_inspect(arg) will show every internal field of arg
 */

/* TODO: mus_bank (gens scalers *args...) -> loop through gens accumulating scaler * gen(args)
 */

#if defined(HAVE_CONFIG_H)
  #include "config.h"
#else
  #define HAVE_VPRINTF 1
#endif

#include <ctype.h>
#include <stddef.h>
#include <math.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#if (defined(NEXT) || (defined(HAVE_LIBC_H) && (!defined(HAVE_UNISTD_H))))
  #include <libc.h>
#else
  #ifndef _MSC_VER
    #include <unistd.h>
  #endif
#endif

#ifndef HAVE_SNDLIB
  #define HAVE_SNDLIB 1
#endif

#include "clm.h"

static int mus_class_tag = MUS_INITIAL_GEN_TAG;
int mus_make_class_tag(void) {return(mus_class_tag++);}

#if (!HAVE_SNDLIB)

#define MUS_SAMPLE_TYPE int
#define MUS_SAMPLE_BITS 24

static int mus_error_tag = MUS_INITIAL_ERROR_TAG;
int mus_error_make_tag(void) {return(mus_error_tag++);}
static void (*mus_error_handler)(int err_type, char *err_msg);
void mus_error_set_handler(void (*new_error_handler)(int err_type, char *err_msg)) {mus_error_handler = new_error_handler;}
static char *mus_error_buffer = NULL;

void mus_error(int error, const char *format, ...)
{
#if HAVE_VPRINTF
  va_list ap;
  va_start(ap,format);
  vsprintf(mus_error_buffer,format,ap);
  va_end(ap);
  if (mus_error_handler)
    (*mus_error_handler)(error,mus_error_buffer);
  else 
    {
      fprintf(stderr,mus_error_buffer);
      if (error != MUS_AUDIO_NO_ERROR) fputc('\n',stderr);
    }
#else
  fprintf(stderr,"error: %d",error);
  if (error != MUS_AUDIO_NO_ERROR) fputc('\n',stderr);
#endif
}

void mus_fwrite(int fd, const char *format, ...)
{
#if HAVE_VPRINTF
  va_list ap;
  va_start(ap,format);
  vsprintf(mus_error_buffer,format,ap);
  va_end(ap);
  write(fd,mus_error_buffer,strlen(mus_error_buffer));
#else
  write(fd,"error...",9);
#endif
}
#endif

static Float sampling_rate = 22050.0;
static int array_print_length = 8;

#if HAVE_SNDLIB
  static int mus_file_buffer_size = 8192;
  int mus_set_file_buffer_size(int size) {mus_file_buffer_size = size; return(size);}
#endif

#define DESCRIBE_BUFFER_SIZE 256
static int describe_buffer_size = DESCRIBE_BUFFER_SIZE; /* needs to be a variable to protect against large array_print_lengths */

static char *make_desc_buf_1(void *ptr, int descr, int size)
{
  char *desc = NULL;
  mus_any *gen = (mus_any *)ptr;
  desc = (char *)CALLOC(size,sizeof(char));
  if (desc == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate %d bytes for %s_%s!",size,(descr) ? "describe" : "inspect",(gen->core)->name);
  return(desc);
}

static char *make_desc_buf(void *ptr, int descr) {return(make_desc_buf_1(ptr,descr,describe_buffer_size));}
static char *make_big_desc_buf(void *ptr, int descr) {return(make_desc_buf_1(ptr,descr,describe_buffer_size * 4));}

#ifndef WITH_SINE_TABLE
  #define WITH_SINE_TABLE 1
#endif

#if WITH_SINE_TABLE

  /* use of "sin" rather than table lookup of a stored sine wave costs us about 
   * 1.5 (23/16) in compute time on a Pentium, using all doubles is slightly faster (~10%)
   * 2.5 (14/6) on an SGI
   * 4.5 (86/18) on a PPC
   * 4 (88/23) on a Sun
   * 5 (31/6) on a NeXT
   */

#define SINE_SIZE 8192
static Float *sine_table = NULL;

static void init_sine (void)
{
  int i;
  Float phase,incr;
  if (sine_table == NULL)
    {
      sine_table = (Float *)CALLOC(SINE_SIZE+1,sizeof(Float));
      if (sine_table == NULL) 
	mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate sine_table");
      else
	{
	  incr = TWO_PI/(Float)SINE_SIZE;
	  for (i=0,phase=0.0;i<SINE_SIZE+1;i++,phase+=incr)
	    sine_table[i]=(Float)sin(phase);
	}
    }
}

Float mus_sin(Float phase)
{
  Float mag_phase,frac;
  int index;
  if (phase < 0.0)
    {
      index=(int)(phase/TWO_PI);
      phase+=(Float)(((1-index)*(TWO_PI)));
    }
  mag_phase = phase*SINE_SIZE/TWO_PI;
  index = (int)mag_phase;
  frac = mag_phase - index;
  index = index%SINE_SIZE;
  if (frac == 0.0)
    return(sine_table[index]);
  else return(sine_table[index] + (frac * (sine_table[index+1] - sine_table[index])));
}

#else
  Float mus_sin(Float phase) {return(sin(phase));}
#endif

int mus_type(mus_any *ptr) {return((ptr->core)->type);}
char *mus_name(mus_any *ptr) {return((ptr->core)->name);}

static void describe_bad_gen(void *ptr, char *desc, char *gen_name, char *an)
{
  if (!ptr)
    sprintf(desc,"arg to describe_%s is null",gen_name);
  else
    {
      if ((((mus_any *)ptr)->core)->type < mus_class_tag)
	sprintf(desc,"arg to describe_%s appears to be %s %s",gen_name,an,mus_name((mus_any *)ptr));
      else sprintf(desc,"arg to describe_%s (%d) is not %s %s",gen_name,(int)ptr,an,gen_name);
    }
}

Float mus_radians2hz(Float rads) {return(rads * sampling_rate / TWO_PI);}
Float mus_hz2radians(Float hz) {return(hz * TWO_PI / sampling_rate);}
Float mus_degrees2radians(Float degree) {return(degree * TWO_PI / 360.0);}
Float mus_radians2degrees(Float rads) {return(rads * 360.0 / TWO_PI);}
Float mus_db2linear(Float x) {return(pow(10.0,x/20.0));}
Float mus_linear2db(Float x) {return(20.0 * log10(x));}

Float mus_srate(void) {return(sampling_rate);}
Float mus_set_srate(Float val) {sampling_rate = val; return(val);}

int mus_array_print_length(void) {return(array_print_length);}
int mus_set_array_print_length(int val) 
{
  array_print_length = val; 
  if ((val*16) > describe_buffer_size) describe_buffer_size = (val*16)+256;
  return(val);
}

static char *print_array(Float *arr, int len, int loc)
{
  char *base,*str;
  int i,lim,k,size = 128;
  if (arr == NULL) 
    {
      str = (char *)CALLOC(4,sizeof(char));
      sprintf(str,"nil");
      return(str);
    }
  if ((array_print_length*12) > size) size = array_print_length*12;
  base = (char *)CALLOC(size,sizeof(char));
  str = (char *)CALLOC(32,sizeof(char));
  sprintf(base,"[");
  lim = len;
  if (lim > array_print_length) lim = array_print_length;
  k = loc;
  for (i=0;i<lim-1;i++)
    {
      sprintf(str,"%.3f ",arr[k]);
      strcat(base,str);
      k++;
      if (k >= len) k=0;
    }
  sprintf(str,"%.3f%s]",arr[k],(len > lim) ? "..." : "");
  strcat(base,str);
  FREE(str);
  return(base);
}

static char *print_double_array(double *arr, int len, int loc)
{
  char *base,*str;
  int i,lim,k,size=128;
  if (arr == NULL) 
    {
      str = (char *)CALLOC(4,sizeof(char));
      sprintf(str,"nil");
      return(str);
    }
  if ((array_print_length*16) > size) size = array_print_length*16;
  base = (char *)CALLOC(size,sizeof(char));
  str = (char *)CALLOC(32,sizeof(char));
  sprintf(base,"[");
  lim = len;
  if (lim > array_print_length) lim = array_print_length;
  k = loc;
  for (i=0;i<lim-1;i++)
    {
      sprintf(str,"%.3f ",arr[k]);
      strcat(base,str);
      k++;
      if (k >= len) k=0;
    }
  sprintf(str,"%.3f%s]",arr[k],(len > lim) ? "..." : "");
  strcat(base,str);
  FREE(str);
  return(base);
}

static char *print_int_array(int *arr, int len, int loc)
{
  char *base,*str;
  int i,lim,k,size=128;
  if (arr == NULL) 
    {
      str = (char *)CALLOC(4,sizeof(char));
      sprintf(str,"nil");
      return(str);
    }
  if ((array_print_length*8) > size) size = array_print_length*8;
  base = (char *)CALLOC(size,sizeof(char));
  str = (char *)CALLOC(32,sizeof(char));
  sprintf(base,"[");
  lim = len;
  if (lim > array_print_length) lim = array_print_length;
  k = loc;
  for (i=0;i<lim-1;i++)
    {
      sprintf(str,"%d ",arr[k]);
      strcat(base,str);
      k++;
      if (k >= len) k=0;
    }
  sprintf(str,"%d%s]",arr[k],(len > lim) ? "..." : "");
  strcat(base,str);
  FREE(str);
  return(base);
}


/* ---------------- generic functions ---------------- */

int mus_free(mus_any *gen)
{
  if (gen)
    {
      if ((gen->core)->release)
	return((*((gen->core)->release))(gen));
      mus_error(MUS_NO_FREE,"can't free %s",mus_name(gen));
    }
  else mus_error(MUS_NO_GEN,"null gen passed to mus_free");
  return(0);
  
}

char *mus_describe(mus_any *gen)
{
  if (gen)
    {
      if ((gen->core)->describe)
	return((*((gen->core)->describe))(gen));
      else mus_error(MUS_NO_DESCRIBE,"can't describe %s",mus_name(gen));
    }
  else mus_error(MUS_NO_GEN,"null gen passed to mus_describe");
  return(NULL);
}

char *mus_inspect(mus_any *gen)
{
  if (gen)
    {
      if ((gen->core)->inspect)
	return((*((gen->core)->inspect))(gen));
      else mus_error(MUS_NO_DESCRIBE,"can't inspect %s",mus_name(gen));
    }
  else mus_error(MUS_NO_GEN,"null gen passed to inspect");
  return(NULL);
}

int mus_equalp(mus_any *p1, mus_any *p2)
{
  if ((p1) && (p2))
    {
      if ((p1->core)->equalp)
	return((*((p1->core)->equalp))(p1,p2));
      else return(p1 == p2);
    }
  return(TRUE); /* (eq nil nil) */
}

Float mus_frequency(mus_any *gen)
{
  if (gen)
    {
      if ((gen->core)->frequency)
	return((*((gen->core)->frequency))(gen));
      mus_error(MUS_NO_FREQUENCY,"can't get %s's frequency",mus_name(gen));
    }
  else mus_error(MUS_NO_GEN,"null gen passed to mus_frequency");
  return(0.0);
}

Float mus_set_frequency(mus_any *gen,Float val)
{
  if (gen)
    {
      if ((gen->core)->frequency)
	return((*((gen->core)->set_frequency))(gen,val));
      mus_error(MUS_NO_FREQUENCY,"can't set %s's frequency",mus_name(gen));
    }
  else mus_error(MUS_NO_GEN,"null gen passed to mus_set_frequency");
  return(0.0);
}

Float mus_phase(mus_any *gen)
{
  if (gen)
    {
      if ((gen->core)->phase)
	return((*((gen->core)->phase))(gen));
      mus_error(MUS_NO_PHASE,"can't get %s's phase",mus_name(gen));
    }
  return(0.0);
}

Float mus_set_phase(mus_any *gen, Float val)
{
  if (gen)
    {
      if ((gen->core)->phase)
	return((*((gen->core)->set_phase))(gen,val));
      mus_error(MUS_NO_PHASE,"can't set %s's phase",mus_name(gen));
    }
  return(0.0);
}

Float mus_scaler(mus_any *gen)
{
  if (gen)
    {
      if ((gen->core)->scaler)
	return((*((gen->core)->scaler))(gen));
      mus_error(MUS_NO_SCALER,"can't get %s's scaler",mus_name(gen));
    }
  return(0.0);
}

Float mus_set_scaler(mus_any *gen, Float val)
{
  if (gen)
    {
      if ((gen->core)->scaler)
	return((*((gen->core)->set_scaler))(gen,val));
      mus_error(MUS_NO_SCALER,"can't set %s's scaler",mus_name(gen));
    }
  return(0.0);
}

int mus_length(mus_any *gen)
{
  if (gen)
    {
      if ((gen->core)->length)
	return((*((((mus_any *)gen)->core)->length))(gen));
      else mus_error(MUS_NO_LENGTH,"can't get %s's length",mus_name(gen));
    }
  else mus_error(MUS_NO_GEN,"null gen passed to mus_length");
  return(0);
}

int mus_set_length(mus_any *gen, int len)
{
  if (gen)
    {
      if ((gen->core)->length)
	{
	  (*((gen->core)->set_length))(gen,len);
	  return(len);
	}
      else mus_error(MUS_NO_LENGTH,"can't set %s's length",mus_name(gen));
    }
  else mus_error(MUS_NO_GEN,"null gen passed to mus_set_length");
  return(0);
}

Float *mus_data(mus_any *gen)
{
  if (gen)
    {
      if ((gen->core)->data)
	return((*((gen->core)->data))(gen));
      else mus_error(MUS_NO_DATA,"can't get %s's data",mus_name(gen));
    }
  else mus_error(MUS_NO_GEN,"null gen passed to mus_data");
  return(NULL);
}

/* every case that implements the data or set data functions needs to include
 * a var-allocated flag, since all such memory has to be handled via vct objects
 * in guile; a subsequent free by the enclosing object could leave a dangling
 * pointer in guile -- see clm2scm.c
 */

Float *mus_set_data(mus_any *gen, Float *new_data)
{
  if (gen)
    {
      if ((gen->core)->set_data)
	{
	  (*((gen->core)->set_data))(gen,new_data);
	  return(new_data);
	}
      else mus_error(MUS_NO_DATA,"can't set %s's data",mus_name(gen));
    }
  else mus_error(MUS_NO_GEN,"null gen passed to mus_set_data");
  return(new_data);
}


/* ---------------- AM etc ---------------- */

Float mus_ring_modulate(Float sig1, Float sig2) {return(sig1 * sig2);}
Float mus_amplitude_modulate(Float carrier, Float sig1, Float sig2) {return(sig1 * (carrier + sig2));}
Float mus_contrast_enhancement(Float sig, Float index) {return(mus_sin((sig*M_PI_2) + (index*mus_sin(sig*TWO_PI))));}
void mus_clear_array(Float *arr, int size) {int i; for (i=0;i<size;i++) arr[i] = 0.0;}

Float mus_dot_product(Float *data1, Float *data2, int size)
{
  int i;
  Float sum = 0.0;
  for (i=0;i<size;i++) sum += (data1[i]*data2[i]);
  return(sum);
}

Float mus_polynomial(Float *coeffs, Float x, int ncoeffs)
{
  Float sum;
  int i;
  if (ncoeffs <= 0) return(x);
  sum = coeffs[ncoeffs-1];
  for (i=ncoeffs-2;i>=0;i--) sum = (sum * x) + coeffs[i];
  return(sum);
}

void mus_multiply_arrays(Float *data, Float *window, int len)
{
  int i;
  for (i=0;i<len;i++) data[i] *= window[i];
}

void mus_rectangular2polar(Float *rl, Float *im, int size) 
{
  int i; 
  Float temp; /* apparently floating underflows in sqrt are bringing us to a halt */
  for (i=0;i<size;i++)
    {
      temp = rl[i]*rl[i]+im[i]*im[i];
      if (temp < .0000001) 
	rl[i] = 0.0;
      else rl[i] = sqrt(temp);
    }
}

Float mus_array_interp(Float *wave, Float phase, int size)
{
  int int_part,inx,index;
  Float frac_part;
  if (phase < 0.0)
    {
      index = (int)(phase/size);
      phase += ((1-index)*size);
    }
  int_part = (int)floor(phase);
  frac_part = phase - int_part;
  if (int_part > size) int_part = (int_part%size);
  if (frac_part == 0.0) 
    return(wave[int_part]);
  else
    {
      inx = int_part+1;
      if (inx >= size) inx = 0;
      return(wave[int_part] + (frac_part * (wave[inx] - wave[int_part])));
    }
}

static Float *array_normalize(Float *table, int table_size)
{
  Float amp = 0.0;
  int i;
  for (i=0;i<table_size;i++) if (amp < (fabs(table[i]))) amp = fabs(table[i]);
  if ((amp > 0.0) && (amp != 1.0))
    for (i=0;i<table_size;i++) table[i] /= amp;
  return(table);
}



/* ---------------- oscil ---------------- */

typedef struct {
  mus_any_class *core;
  Float phase,freq;
} osc;

Float mus_oscil(mus_any *ptr, Float fm, Float pm)
{
  Float result;
  osc *gen = (osc *)ptr;
  result = mus_sin(gen->phase + pm);
  gen->phase += (gen->freq + fm);
  if ((gen->phase > 100.0) || (gen->phase < -100.0)) gen->phase = fmod(gen->phase,TWO_PI);
  return(result);
}

Float mus_oscil_bank(Float *amps, mus_any **oscils, Float *inputs, int size)
{
  int i;
  Float sum = 0.0;
  for (i=0;i<size;i++) 
    sum += (amps[i] * mus_oscil(oscils[i],inputs[i],0.0));
  return(sum);
}

int mus_oscil_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_OSCIL));}
static int free_oscil(void *ptr) {if (ptr) FREE(ptr); return(0);}
static Float oscil_freq(void *ptr) {return(mus_radians2hz(((osc *)ptr)->freq));}
static Float set_oscil_freq(void *ptr, Float val) {((osc *)ptr)->freq = mus_hz2radians(val); return(val);}
static Float oscil_phase(void *ptr) {return(fmod(((osc *)ptr)->phase,TWO_PI));}
static Float set_oscil_phase(void *ptr, Float val) {((osc *)ptr)->phase = val; return(val);}

static int oscil_equalp(void *p1, void *p2)
{
  return((p1 == p2) ||
	 ((mus_oscil_p((mus_any *)p1)) && 
	  (mus_oscil_p((mus_any *)p2)) &&
	  ((((osc *)p1)->freq) == (((osc *)p2)->freq)) &&
	  ((((osc *)p1)->phase) == (((osc *)p2)->phase))));
}

static char *describe_oscil(void *ptr)
{
  char *desc;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_oscil_p((mus_any *)ptr))
	sprintf(desc,"oscil freq: %.3fHz, phase: %.3f",mus_radians2hz(((osc *)ptr)->freq),((osc *)ptr)->phase);
      else describe_bad_gen(ptr,desc,"oscil","an");
    }
  return(desc);
}

static char *inspect_oscil(void *ptr)
{
  osc *gen = (osc *)ptr;
  char *desc;
  desc = make_desc_buf(ptr,FALSE);
  if (desc) sprintf(desc,"osc freq: %f, phase: %f",gen->freq,gen->phase);
  return(desc);
}

static mus_any_class OSCIL_CLASS = {
  MUS_OSCIL,
  "oscil",
  &free_oscil,
  &describe_oscil,
  &inspect_oscil,
  &oscil_equalp,
  0,0,0,0, /* data length */
  &oscil_freq,
  &set_oscil_freq,
  &oscil_phase,
  &set_oscil_phase,
  0,0
};

mus_any *mus_make_oscil(Float freq, Float phase)
{
  osc *gen;
  gen = (osc *)CALLOC(1,sizeof(osc));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_oscil!");
  else
    {
      gen->core = &OSCIL_CLASS;
      gen->freq = mus_hz2radians(freq);
      gen->phase = phase;
      return((mus_any *)gen);
    }
  return(NULL);
}

/* ---------------- sum-of-cosines ---------------- */

typedef struct {
  mus_any_class *core;
  int cosines;
  Float scaler;
  Float phase;
  Float freq;
} cosp;

Float mus_sum_of_cosines(mus_any *ptr, Float fm)
{
  Float val;
  cosp *gen = (cosp *)ptr;
  if ((gen->phase == 0.0) || (gen->phase == TWO_PI))
    val = 1.0;
  else 
    {
      val = (gen->scaler * mus_sin(gen->phase * (gen->cosines + 0.5))) / mus_sin(gen->phase * 0.5);
      if (val > 1.0) val = 1.0;
    }
  gen->phase += (gen->freq + fm);
  while (gen->phase > TWO_PI) gen->phase -= TWO_PI;
  while (gen->phase < 0.0) gen->phase += TWO_PI;
  return(val);
}

int mus_sum_of_cosines_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_SUM_OF_COSINES));}

static int free_sum_of_cosines(void *ptr) {if (ptr) FREE(ptr); return(0);}
static Float sum_of_cosines_freq(void *ptr) {return(mus_radians2hz(((cosp *)ptr)->freq));}
static Float set_sum_of_cosines_freq(void *ptr, Float val) {((cosp *)ptr)->freq = mus_hz2radians(val); return(val);}
static Float sum_of_cosines_phase(void *ptr) {return(fmod(((cosp *)ptr)->phase,TWO_PI));}
static Float set_sum_of_cosines_phase(void *ptr, Float val) {((cosp *)ptr)->phase = val; return(val);}
static Float sum_of_cosines_scaler(void *ptr) {return(((cosp *)ptr)->scaler);}
static Float set_sum_of_cosines_scaler(void *ptr, Float val) {((cosp *)ptr)->scaler = val; return(val);}
static int sum_of_cosines_cosines(void *ptr) {return(((cosp *)ptr)->cosines);}

int mus_cosines(mus_any *ptr) 
{
  if (mus_sum_of_cosines_p(ptr)) 
    return(sum_of_cosines_cosines(ptr)); 
  else 
    {
      if (mus_oscil_p(ptr)) 
	return(1);
      else return(0);
    }
}

static int sum_of_cosines_equalp(void *p1, void *p2)
{
  return((p1 == p2) ||
	 ((mus_sum_of_cosines_p((mus_any *)p1)) && (mus_sum_of_cosines_p((mus_any *)p2)) &&
	  ((((cosp *)p1)->freq) == (((cosp *)p2)->freq)) &&
	  ((((cosp *)p1)->phase) == (((cosp *)p2)->phase)) &&
	  ((((cosp *)p1)->cosines) == (((cosp *)p2)->cosines)) &&
	  ((((cosp *)p1)->scaler) == (((cosp *)p2)->scaler))));
}

static char *describe_sum_of_cosines(void *ptr)
{
  char *desc;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_sum_of_cosines_p((mus_any *)ptr))
	sprintf(desc,"sum_of_cosines freq: %.3fHz, phase: %.3f, cosines: %d",
		mus_radians2hz(((cosp *)ptr)->freq),((cosp *)ptr)->phase,((cosp *)ptr)->cosines);
      else describe_bad_gen(ptr,desc,"sum_of_cosines","a");
    }
  return(desc);
}

static char *inspect_sum_of_cosines(void *ptr)
{
  cosp *gen = (cosp *)ptr;
  char *desc;
  desc = make_desc_buf(ptr,FALSE);
  if (desc) sprintf(desc,"cosp freq: %f, phase: %f, cosines: %d, scaler: %f",gen->freq,gen->phase,gen->cosines,gen->scaler);
  return(desc);
}

static mus_any_class SUM_OF_COSINES_CLASS = {
  MUS_SUM_OF_COSINES,
  "sum_of_cosines",
  &free_sum_of_cosines,
  &describe_sum_of_cosines,
  &inspect_sum_of_cosines,
  &sum_of_cosines_equalp,
  0,0, /* data */
  &sum_of_cosines_cosines,
  0,
  &sum_of_cosines_freq,
  &set_sum_of_cosines_freq,
  &sum_of_cosines_phase,
  &set_sum_of_cosines_phase,
  &sum_of_cosines_scaler,
  &set_sum_of_cosines_scaler
};

mus_any *mus_make_sum_of_cosines(int cosines, Float freq, Float phase)
{
  cosp *gen;
  gen = (cosp *)CALLOC(1,sizeof(cosp));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_sum_of_cosines!");
  else
    {
      gen->core = &SUM_OF_COSINES_CLASS;
      if (cosines == 0) cosines = 1;
      gen->scaler = 1.0/(Float)(1+2*cosines);
      gen->cosines = cosines;
      gen->freq = mus_hz2radians(freq);
      gen->phase = phase;
      return((mus_any *)gen);
    }
  return(NULL);
}


/* ---------------- delay, comb, notch, all-pass ---------------- */

typedef struct {
  mus_any_class *core;
  int loc,size,zdly,line_allocated;
  Float *line;
  int zloc,zsize;
  Float xscl,yscl;
} dly;


Float mus_delay(mus_any *ptr, Float input, Float pm)
{
  dly *gen = (dly *)ptr;
  Float result;
  if (gen->zdly)
    {
      result = mus_array_interp(gen->line,gen->zloc - pm,gen->zsize);
      gen->line[gen->loc] = input;
      gen->loc++;
      if (gen->loc >= gen->zsize) gen->loc = 0;
      gen->zloc++;
      if (gen->zloc >= gen->zsize) gen->zloc = 0;
    }
  else
    {
      result = gen->line[gen->loc];
      gen->line[gen->loc] = input;
      gen->loc++;
      if (gen->loc >= gen->size) gen->loc = 0;
    }
  return(result);
}

Float mus_tap(mus_any *ptr, Float loc)
{
  dly *gen = (dly *)ptr;
  int taploc;
  if (gen->zdly)
    {
      if (loc == 0.0) 
	return(gen->line[gen->zloc]);
      else return(mus_array_interp(gen->line,gen->zloc - loc,gen->zsize));
    }
  else
    {
      if ((int)loc == 0) 
	return(gen->line[gen->loc]);
      else
	{
	  taploc = (int)(gen->loc - (int)loc) % gen->size;
	  if (taploc < 0) taploc += gen->size;
	  return(gen->line[taploc]);
	}
    }
}

static int free_delay(void *gen) 
{
  dly *ptr = (dly *)gen;
  if (ptr) 
    {
      if ((ptr->line) && (ptr->line_allocated)) FREE(ptr->line);
      FREE(ptr);
    }
  return(0);
}

static char *inspect_delay(void *ptr)
{
  dly *gen = (dly *)ptr;
  char *desc;
  desc = make_big_desc_buf(ptr,FALSE);
  if (desc) 
    sprintf(desc,"dly line[%d,%d at %d,%d (%s)]: %s, xscl: %f, yscl: %f",
	  gen->size,
	  gen->zsize,
	  gen->loc,
	  gen->zloc,
	  (gen->line_allocated) ? "local" : "external",
	  print_array(gen->line,gen->size,(gen->zdly) ? gen->zloc : gen->loc),
	  gen->xscl,
	  gen->yscl);
  return(desc);
}

static char *describe_delay(void *ptr)
{
  char *desc,*str = NULL;
  dly *gen = (dly *)ptr;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_delay_p((mus_any *)ptr))
	{
	  if (gen->zdly)
	    sprintf(desc,"delay: line[%d,%d]: %s",gen->size,gen->zsize,str = print_array(gen->line,gen->size,gen->zloc));
	  else sprintf(desc,"delay: line[%d]: %s",gen->size,str = print_array(gen->line,gen->size,gen->loc));
	  if (str) FREE(str);
	}
      else describe_bad_gen(ptr,desc,"delay","a");
    }
  return(desc);
}

static char *describe_comb(void *ptr)
{
  char *desc,*str = NULL;
  dly *gen = (dly *)ptr;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_comb_p((mus_any *)ptr))
	{
	  if (gen->zdly)
	    sprintf(desc,"comb: scaler: %.3f, line[%d,%d]: %s",gen->xscl,gen->size,gen->zsize,str = print_array(gen->line,gen->size,gen->zloc));
	  else sprintf(desc,"comb: scaler: %.3f, line[%d]: %s",gen->xscl,gen->size,str = print_array(gen->line,gen->size,gen->loc));
	  if (str) FREE(str);
	}
      else describe_bad_gen(ptr,desc,"comb","a");
    }
  return(desc);
}

static char *describe_notch(void *ptr)
{
  char *desc,*str = NULL;
  dly *gen = (dly *)ptr;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_notch_p((mus_any *)ptr))
	{
	  if (gen->zdly)
	    sprintf(desc,"notch: scaler: %.3f, line[%d,%d]: %s",gen->xscl,gen->size,gen->zsize,str = print_array(gen->line,gen->size,gen->zloc));
	  else sprintf(desc,"notch: scaler: %.3f, line[%d]: %s",gen->xscl,gen->size,str = print_array(gen->line,gen->size,gen->loc));
	  if (str) FREE(str);
	}
      else describe_bad_gen(ptr,desc,"notch","a");
    }
  return(desc);
}

static char *describe_all_pass(void *ptr)
{
  char *desc,*str = NULL;
  dly *gen = (dly *)ptr;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_all_pass_p((mus_any *)ptr))
	{
	  if (gen->zdly)
	    sprintf(desc,"all_pass: feedback: %.3f, feedforward: %.3f, line[%d,%d]:%s",
		    gen->xscl,gen->yscl,gen->size,gen->zsize,str = print_array(gen->line,gen->size,gen->zloc));
	  else sprintf(desc,"all_pass: feedback: %.3f, feedforward: %.3f, line[%d]:%s",
		       gen->xscl,gen->yscl,gen->size,str = print_array(gen->line,gen->size,gen->loc));
	  if (str) FREE(str);
	}
      else describe_bad_gen(ptr,desc,"all_pass","an");
    }
  return(desc);
}

static int delay_equalp(void *p1, void *p2)
{
  return((p1 == p2)); /* should we actually check the delay lines?? */
}

static int delay_length(void *ptr) {return(((dly *)ptr)->size);}
static Float *delay_data(void *ptr) {return(((dly *)ptr)->line);}
static Float delay_scaler(void *ptr) {return(((dly *)ptr)->xscl);}
static Float set_delay_scaler(void *ptr, Float val) {((dly *)ptr)->xscl = val; return(val);}

static mus_any_class DELAY_CLASS = {
  MUS_DELAY,
  "delay",
  &free_delay,
  &describe_delay,
  &inspect_delay,
  &delay_equalp,
  &delay_data,
  0,
  &delay_length,
  0,
  0,0,0,0,0,0 /* freq phase scaler */
};

mus_any *mus_make_delay(int size, Float *preloaded_line, int line_size) 
{
  /* if preloaded_line null, allocated locally */
  /* if size == line_size, normal (non-interpolating) delay */
  dly *gen;
  gen = (dly *)CALLOC(1,sizeof(dly));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_delay!");
  else
    {
      gen->core = &DELAY_CLASS;
      gen->loc = 0;
      gen->size = size;
      gen->zsize = line_size;
      gen->zdly = (line_size != size);
      if (preloaded_line)
	{
	  gen->line = preloaded_line;
	  gen->line_allocated = 0;
	}
      else 
	{
	  gen->line = (Float *)CALLOC(line_size,sizeof(Float));
	  if (gen->line == NULL) 
	    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate %d bytes for delay line in mus_make_delay!",(int)(line_size * sizeof(Float)));
	  else gen->line_allocated = 1;
	}
      gen->zloc = line_size - size;
      return((mus_any *)gen);
    }
  return(NULL);
}

int mus_delay_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_DELAY));}

Float mus_comb (mus_any *ptr, Float input, Float pm) 
{
  dly *gen = (dly *)ptr;
  if (gen->zdly)
    return(mus_delay(ptr,input + (gen->xscl * mus_tap(ptr,pm)),pm));
  else return(mus_delay(ptr,input + (gen->line[gen->loc] * gen->xscl),0.0));
}

static mus_any_class COMB_CLASS = {
  MUS_COMB,
  "comb",
  &free_delay,
  &describe_comb,
  &inspect_delay,
  &delay_equalp,
  &delay_data,
  0,
  &delay_length,
  0,
  0,0,0,0, /* freq phase */
  &delay_scaler,
  &set_delay_scaler
};

mus_any *mus_make_comb (Float scaler, int size, Float *line, int line_size)
{
  dly *gen;
  gen = (dly *)mus_make_delay(size,line,line_size);
  if (gen)
    {
      gen->core = &COMB_CLASS;
      gen->xscl = scaler;
      return((mus_any *)gen);
    }
  return(NULL);
}

int mus_comb_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_COMB));}

static mus_any_class NOTCH_CLASS = {
  MUS_NOTCH,
  "notch",
  &free_delay,
  &describe_notch,
  &inspect_delay,
  &delay_equalp,
  &delay_data,
  0,
  &delay_length,
  0,
  0,0,0,0, /* freq phase */
  &delay_scaler,
  &set_delay_scaler
};

Float mus_notch (mus_any *ptr, Float input, Float pm) 
{
  dly *gen = (dly *)ptr;
  return((input * gen->xscl) + mus_delay(ptr,input,pm));
}

int mus_notch_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_NOTCH));}

mus_any *mus_make_notch (Float scaler, int size, Float *line, int line_size)
{
  dly *gen;
  gen = (dly *)mus_make_delay(size,line,line_size);
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
    din = mus_delay(ptr,input + (gen->yscl * mus_tap(ptr,pm)),pm);
  else din = input + (gen->yscl * gen->line[gen->loc]);
  return(mus_delay(ptr,din,pm) + (gen->xscl * din));
}

int mus_all_pass_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_ALL_PASS));}

static mus_any_class ALL_PASS_CLASS = {
  MUS_ALL_PASS,
  "all_pass",
  &free_delay,
  &describe_all_pass,
  &inspect_delay,
  &delay_equalp,
  &delay_data,
  0,
  &delay_length,
  0,
  0,0,0,0, /* freq phase */
  &delay_scaler,
  &set_delay_scaler
};

mus_any *mus_make_all_pass (Float backward, Float forward, int size, Float *line, int line_size)
{
  dly *gen;
  gen = (dly *)mus_make_delay(size,line,line_size);
  if (gen)
    {
      gen->core = &ALL_PASS_CLASS;
      gen->xscl = forward;
      gen->yscl = backward;
      return((mus_any *)gen);
    }
  return(NULL);
}

Float mus_feedback(mus_any *ptr)
{
  if (mus_comb_p(ptr))
    return(((dly *)ptr)->xscl);
  else
    {
      if (mus_all_pass_p(ptr))
	return(((dly *)ptr)->yscl);
      else return(0.0);
    }
}

Float mus_set_feedback(mus_any *ptr, Float val)
{
  if (mus_comb_p(ptr))
    ((dly *)ptr)->xscl = val;
  else
    {
      if (mus_all_pass_p(ptr))
	((dly *)ptr)->yscl = val;
    }
  return(val);
}

Float mus_feedforward(mus_any *ptr)
{
  if ((mus_notch_p(ptr)) || (mus_all_pass_p(ptr)))
    return(((dly *)ptr)->xscl);
  else return(0.0);
}

Float mus_set_feedforward(mus_any *ptr, Float val)
{
  if ((mus_notch_p(ptr)) || (mus_all_pass_p(ptr)))
    ((dly *)ptr)->xscl = val;
  return(val);
}



/* ---------------- table lookup ---------------- */

typedef struct {
  mus_any_class *core;
  Float freq;
  Float internal_mag;
  Float phase;
  Float *table;
  int table_size,table_allocated;
} tbl;

Float *mus_partials2wave(Float *partial_data, int partials, Float *table, int table_size, int normalize)
{
  int partial,i,k;
  Float amp,freq,angle;
  for (i=0;i<table_size;i++) table[i] = 0.0;
  for (partial=0,k=1;partial<partials;partial++,k+=2)
    {
      amp = partial_data[k];
      if (amp != 0.0)
	{
	  freq = (partial_data[partial*2] * TWO_PI) / (Float)table_size;
	  for (i=0,angle=0.0;i<table_size;i++,angle+=freq) table[i] += amp * mus_sin(angle);
	}
    }
  if (normalize) 
    return(array_normalize(table,table_size));
  return(table);
}

Float *mus_phasepartials2wave(Float *partial_data, int partials, Float *table, int table_size, int normalize)
{
  int partial,i,k,n;
  Float amp,freq,angle;
  for (i=0;i<table_size;i++) table[i] = 0.0;
  for (partial=0,k=1,n=2;partial<partials;partial++,k+=3,n+=3)
    {
      amp = partial_data[k];
      if (amp != 0.0)
	{
	  freq = (partial_data[partial*3] * TWO_PI) / (Float)table_size;
	  for (i=0,angle=partial_data[n];i<table_size;i++,angle+=freq) table[i] += amp * mus_sin(angle);
	}
    }
  if (normalize) 
    return(array_normalize(table,table_size));
  return(table);
}

Float mus_table_lookup(mus_any *ptr, Float fm)
{
  tbl *gen = (tbl *)ptr;
  Float result;
  result = mus_array_interp(gen->table,gen->phase,gen->table_size);
  gen->phase += (gen->freq + (fm * gen->internal_mag));
  while (gen->phase >= gen->table_size) gen->phase -= gen->table_size;
  while (gen->phase < 0.0) gen->phase += gen->table_size;
  return(result);
}

int mus_table_lookup_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_TABLE_LOOKUP));}
static int table_lookup_length(void *ptr) {return(((tbl *)ptr)->table_size);}
static Float *table_lookup_data(void *ptr) {return(((tbl *)ptr)->table);}
static Float table_lookup_freq(void *ptr) {return((((tbl *)ptr)->freq * sampling_rate) / (Float)(((tbl *)ptr)->table_size));}
static Float set_table_lookup_freq(void *ptr, Float val) {((tbl *)ptr)->freq = (val * ((tbl *)ptr)->table_size) / sampling_rate; return(val);}
static Float table_lookup_phase(void *ptr) {return(fmod(((TWO_PI * ((tbl *)ptr)->phase)/((tbl *)ptr)->table_size),TWO_PI));}
static Float set_table_lookup_phase(void *ptr, Float val) {((tbl *)ptr)->phase = (val * ((tbl *)ptr)->table_size) / TWO_PI; return(val);}

static char *describe_table_lookup(void *ptr)
{
  char *desc;
  tbl *gen = (tbl *)ptr;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_table_lookup_p((mus_any *)ptr))
	sprintf(desc,"table_lookup: freq: %.3fHz, phase: %.3f, length: %d",
		gen->freq * sampling_rate / gen->table_size,gen->phase,gen->table_size);
      else describe_bad_gen(ptr,desc,"table_lookup","a");
    }
  return(desc);
}

static char *inspect_table_lookup(void *ptr)
{
  tbl *gen = (tbl *)ptr;
  char *desc;
  desc = make_desc_buf(ptr,FALSE);
  if (desc) sprintf(desc,"tbl freq: %f, phase: %f, length: %d, mag: %f, table: %d (%s)",
		    gen->freq,gen->phase,gen->table_size,gen->internal_mag,(int)(gen->table),
		    (gen->table_allocated) ? "local" : "external");
  return(desc);
}

static int table_lookup_equalp(void *p1, void *p2)
{
  return(p1 == p2); /* check entire tables?? */
}

static int free_table_lookup(void *ptr) 
{
  tbl *gen = (tbl *)ptr;
  if (gen)
    {
      if ((gen->table) && (gen->table_allocated)) FREE(gen->table); 
      FREE(gen); 
    }
  return(0);
}

static mus_any_class TABLE_LOOKUP_CLASS = {
  MUS_TABLE_LOOKUP,
  "table_lookup",
  &free_table_lookup,
  &describe_table_lookup,
  &inspect_table_lookup,
  &table_lookup_equalp,
  &table_lookup_data,
  0,
  &table_lookup_length,
  0,
  &table_lookup_freq,
  &set_table_lookup_freq,
  &table_lookup_phase,
  &set_table_lookup_phase,
  0,0
};

mus_any *mus_make_table_lookup (Float freq, Float phase, Float *table, int table_size)
{
  tbl *gen;
  gen = (tbl *)CALLOC(1,sizeof(tbl));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_table_lookup!");
  else
    {
      gen->core = &TABLE_LOOKUP_CLASS;
      gen->table_size = table_size;
      gen->internal_mag = (Float)table_size / TWO_PI;
      gen->freq = (freq * table_size) / sampling_rate;
      gen->phase = (phase * table_size) / TWO_PI;
      if (table)
	{
	  gen->table = table;
	  gen->table_allocated = 0;
	}
      else
	{
	  gen->table = (Float *)CALLOC(table_size,sizeof(Float));
	  if (gen->table == NULL)
	    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate %d bytes for table in mus_make_table_lookup!",(int)(table_size * sizeof(Float)));
	  else gen->table_allocated = 1;
	}
      return((mus_any *)gen);
    }
  return(NULL);
}



/* ---------------- sawtooth et al ---------------- */

typedef struct {
  mus_any_class *core;
  Float current_value;
  Float freq;
  Float phase;
  Float base;
} sw;

static char *inspect_sw(void *ptr)
{
  sw *gen = (sw *)ptr;
  char *desc;
  desc = make_desc_buf(ptr,FALSE);
  if (desc) sprintf(desc,"sw current_value: %f, freq: %f, phase: %f, base: %f",gen->current_value,gen->freq,gen->phase,gen->base);
  return(desc);
}

static int free_sw(void *ptr) 
{
  sw *gen = (sw *)ptr;
  if (gen) FREE(gen);
  return(0);
}

Float mus_sawtooth_wave(mus_any *ptr, Float fm)
{
  sw *gen = (sw *)ptr;
  Float result;
  result = gen->current_value;
  gen->phase += (gen->freq + fm);
  while (gen->phase >= TWO_PI) gen->phase -= TWO_PI;
  while (gen->phase < 0.0) gen->phase += TWO_PI;
  gen->current_value = gen->base * (gen->phase - M_PI);
  return(result);
}

int mus_sawtooth_wave_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_SAWTOOTH_WAVE));}
static Float sw_freq(void *ptr) {return(mus_radians2hz(((sw *)ptr)->freq));}
static Float sw_phase(void *ptr) {return(fmod(((sw *)ptr)->phase,TWO_PI));}
static Float set_sw_freq(void *ptr, Float val) {((sw *)ptr)->freq = mus_hz2radians(val); return(val);}
static Float set_sw_phase(void *ptr, Float val) {((sw *)ptr)->phase = val; return(val);}

static Float sawtooth_scaler(void *ptr) {return(((sw *)ptr)->base * M_PI);}
static Float set_sawtooth_scaler(void *ptr, Float val) {((sw *)ptr)->base = val / M_PI; return(val);}

static int sw_equalp(void *p1, void *p2)
{
  return((p1 == p2) ||
	 (((((sw *)p1)->core)->type == (((sw *)p2)->core)->type) &&
	  ((((sw *)p1)->freq) == (((sw *)p2)->freq)) &&
	  ((((sw *)p1)->phase) == (((sw *)p2)->phase)) &&
	  ((((sw *)p1)->base) == (((sw *)p2)->base)) &&
	  ((((sw *)p1)->current_value) == (((sw *)p2)->current_value))));
}

static char *describe_sawtooth(void *ptr)
{
  char *desc;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_sawtooth_wave_p((mus_any *)ptr))
	sprintf(desc,"sawtooth freq: %.3fHz, phase: %.3f, amp: %.3f",
		mus_radians2hz(((sw *)ptr)->freq),((sw *)ptr)->phase,((sw *)ptr)->base * M_PI);
      else describe_bad_gen(ptr,desc,"sawtooth_wave","a");
    }
  return(desc);
}

static mus_any_class SAWTOOTH_WAVE_CLASS = {
  MUS_SAWTOOTH_WAVE,
  "sawtooth_wave",
  &free_sw,
  &describe_sawtooth,
  &inspect_sw,
  &sw_equalp,
  0,0,0,0,
  &sw_freq,
  &set_sw_freq,
  &sw_phase,
  &set_sw_phase,
  &sawtooth_scaler,
  &set_sawtooth_scaler
};

mus_any *mus_make_sawtooth_wave(Float freq, Float amp, Float phase) /* M_PI as initial phase, normally */
{
  sw *gen;
  gen = (sw *)CALLOC(1,sizeof(sw));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_sawtooth_wave!");
  else
    {
      gen->core = &SAWTOOTH_WAVE_CLASS;
      gen->freq = mus_hz2radians(freq);
      gen->base = (amp / M_PI);
      gen->phase = phase;
      gen->current_value = gen->base * (gen->phase - M_PI);
      return((mus_any *)gen);
    }
  return(NULL);
}

Float mus_square_wave(mus_any *ptr, Float fm)
{
  sw *gen = (sw *)ptr;
  Float result;
  result = gen->current_value;
  gen->phase += (gen->freq + fm);
  while (gen->phase >= TWO_PI) gen->phase -= TWO_PI;
  while (gen->phase < 0.0) gen->phase += TWO_PI;
  if (gen->phase < M_PI) gen->current_value = gen->base; else gen->current_value = 0.0;
  return(result);
}

int mus_square_wave_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_SQUARE_WAVE));}

static char *describe_square_wave(void *ptr)
{
  char *desc;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_square_wave_p((mus_any *)ptr))
	sprintf(desc,"square_wave freq: %.3fHz, phase: %.3f, amp: %.3f",
		mus_radians2hz(((sw *)ptr)->freq),((sw *)ptr)->phase,((sw *)ptr)->base);
      else describe_bad_gen(ptr,desc,"square_wave","a");
    }
  return(desc);
}

static Float square_wave_scaler(void *ptr) {return(((sw *)ptr)->base);}
static Float set_square_wave_scaler(void *ptr, Float val) {((sw *)ptr)->base = val; return(val);}

static mus_any_class SQUARE_WAVE_CLASS = {
  MUS_SQUARE_WAVE,
  "square_wave",
  &free_sw,
  &describe_square_wave,
  &inspect_sw,
  &sw_equalp,
  0,0,0,0,
  &sw_freq,
  &set_sw_freq,
  &sw_phase,
  &set_sw_phase,
  &square_wave_scaler,
  &set_square_wave_scaler
};

mus_any *mus_make_square_wave(Float freq, Float amp, Float phase)
{
  sw *gen;
  gen = (sw *)CALLOC(1,sizeof(sw));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_square_wave!");
  else
    {
      gen->core = &SQUARE_WAVE_CLASS;
      gen->freq = mus_hz2radians(freq);
      gen->base = amp;
      gen->phase = phase;
      if (gen->phase < M_PI) gen->current_value = gen->base; else gen->current_value = 0.0;
      return((mus_any *)gen);
    }
  return(NULL);
}

Float mus_triangle_wave(mus_any *ptr, Float fm)
{
  sw *gen = (sw *)ptr;
  Float result;
  result = gen->current_value;
  gen->phase += (gen->freq + fm);
  while (gen->phase >= TWO_PI) gen->phase -= TWO_PI;
  while (gen->phase < 0.0) gen->phase += TWO_PI;
  if (gen->phase < (M_PI/2.0)) 
    gen->current_value = gen->base * gen->phase;
  else
    if (gen->phase < (M_PI*1.5)) 
      gen->current_value = gen->base * (M_PI - gen->phase);
    else gen->current_value = gen->base * (gen->phase - TWO_PI);
  return(result);
}

int mus_triangle_wave_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_TRIANGLE_WAVE));}

static Float triangle_wave_scaler(void *ptr) {return(((sw *)ptr)->base * M_PI_2);}
static Float set_triangle_wave_scaler(void *ptr, Float val) {((sw *)ptr)->base = (val * 2.0/M_PI); return(val);}

static char *describe_triangle_wave(void *ptr)
{
  char *desc;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_triangle_wave_p((mus_any *)ptr))
	sprintf(desc,"triangle_wave freq: %.3fHz, phase: %.3f, amp: %.3f",
		mus_radians2hz(((sw *)ptr)->freq),((sw *)ptr)->phase,((sw *)ptr)->base * M_PI_2);
      else describe_bad_gen(ptr,desc,"triangle_wave","a");
    }
  return(desc);
}

static mus_any_class TRIANGLE_WAVE_CLASS = {
  MUS_TRIANGLE_WAVE,
  "triangle_wave",
  &free_sw,
  &describe_triangle_wave,
  &inspect_sw,
  &sw_equalp,
  0,0,0,0,
  &sw_freq,
  &set_sw_freq,
  &sw_phase,
  &set_sw_phase,
  &triangle_wave_scaler,
  &set_triangle_wave_scaler
};

mus_any *mus_make_triangle_wave(Float freq, Float amp, Float phase)
{
  sw *gen;
  gen = (sw *)CALLOC(1,sizeof(sw));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_triangle_wave!");
  else
    {
      gen->core = &TRIANGLE_WAVE_CLASS;
      gen->freq = mus_hz2radians(freq);
      gen->base = (2.0*amp / M_PI);
      gen->phase = phase;
      if (gen->phase < M_PI_2) 
	gen->current_value = gen->base * gen->phase;
      else
	if (gen->phase < (M_PI*1.5)) 
	  gen->current_value = gen->base * (M_PI - gen->phase);
	else gen->current_value = gen->base * (gen->phase - TWO_PI);
      return((mus_any *)gen);
    }
  return(NULL);
}

Float mus_pulse_train(mus_any *ptr, Float fm)
{
  sw *gen = (sw *)ptr;
  if (fabs(gen->phase) >= TWO_PI)
    {
      while (gen->phase >= TWO_PI) gen->phase -= TWO_PI;
      while (gen->phase < 0.0) gen->phase += TWO_PI;
      gen->current_value = gen->base;
    }
  else gen->current_value = 0.0;
  gen->phase += (gen->freq + fm);
  return(gen->current_value);
}

int mus_pulse_train_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_PULSE_TRAIN));}

static Float pulse_train_scaler(void *ptr) {return(((sw *)ptr)->base);}
static Float set_pulse_train_scaler(void *ptr, Float val) {((sw *)ptr)->base = val; return(val);}

static char *describe_pulse_train(void *ptr)
{
  char *desc;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_pulse_train_p((mus_any *)ptr))
	sprintf(desc,"pulse_train freq: %.3fHz, phase: %.3f, amp: %.3f",
		mus_radians2hz(((sw *)ptr)->freq),((sw *)ptr)->phase,((sw *)ptr)->base);
      else describe_bad_gen(ptr,desc,"pulse_train","a");
    }
  return(desc);
}

static mus_any_class PULSE_TRAIN_CLASS = {
  MUS_PULSE_TRAIN,
  "pulse_train",
  &free_sw,
  &describe_pulse_train,
  &inspect_sw,
  &sw_equalp,
  0,0,0,0,
  &sw_freq,
  &set_sw_freq,
  &sw_phase,
  &set_sw_phase,
  &pulse_train_scaler,
  &set_pulse_train_scaler
};

mus_any *mus_make_pulse_train(Float freq, Float amp, Float phase) /* TWO_PI initial phase, normally */
{
  sw *gen;
  gen = (sw *)CALLOC(1,sizeof(sw));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_pulse_train!");
  else
    {
      gen->core = &PULSE_TRAIN_CLASS;
      gen->freq = mus_hz2radians(freq);
      gen->base = amp;
      gen->phase = phase;
      gen->current_value = 0.0;
      return((mus_any *)gen);
    }
  return(NULL);
}



/* ---------------- rand, rand_interp ---------------- */

typedef struct {
  mus_any_class *core;
  Float freq;
  Float base;
  Float phase;
  Float output;
  Float incr;
} noi;

static char *inspect_noi(void *ptr)
{
  noi *gen = (noi *)ptr;
  char *desc;
  desc = make_desc_buf(ptr,FALSE);
  if (desc) sprintf(desc,"noi freq: %f, phase: %f, base: %f, output: %f, incr: %f",
		    gen->freq,gen->phase,gen->base,gen->output,gen->incr);
  return(desc);
}

/* rand taken from the ANSI C standard (essentially the same as the Cmix form used earlier) */
static unsigned long randx = 1;
#define INVERSE_MAX_RAND  0.0000610351563
#define INVERSE_MAX_RAND2 0.000030517579

void mus_set_rand_seed(int val) {randx = val;}

Float mus_random(Float amp) /* -amp to amp as Float */
{
  int val;
  randx=randx*1103515245 + 12345;
  val=(unsigned int)(randx >> 16) & 32767;
  return(amp * (((Float)val)*INVERSE_MAX_RAND - 1.0));
}

Float mus_rand(mus_any *ptr, Float fm)
{
  noi *gen = (noi *)ptr;
  if (gen->phase >= TWO_PI)
    {
      while (gen->phase >= TWO_PI) gen->phase -= TWO_PI;
      gen->output = mus_random(gen->base);
    }
  gen->phase += (gen->freq + fm);
  while (gen->phase < 0.0) gen->phase += TWO_PI;
  return(gen->output);
}

Float mus_rand_interp(mus_any *ptr, Float fm)
{
  noi *gen = (noi *)ptr;
  gen->output += gen->incr;
  if (gen->phase >= TWO_PI)
    {
      while (gen->phase >= TWO_PI) gen->phase -= TWO_PI;
      gen->incr = (mus_random(gen->base) - gen->output) * (gen->freq + fm) / TWO_PI;
    }
  gen->phase += (gen->freq + fm);
  while (gen->phase < 0.0) gen->phase += TWO_PI;
  return(gen->output);
}

int mus_rand_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_RAND));}
int mus_rand_interp_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_RAND_INTERP));}


static int free_noi(void *ptr) {if (ptr) FREE(ptr); return(0);}
static Float noi_freq(void *ptr) {return(mus_radians2hz(((noi *)ptr)->freq));}
static Float set_noi_freq(void *ptr, Float val) {((noi *)ptr)->freq = mus_hz2radians(val); return(val);}
static Float noi_phase(void *ptr) {return(fmod(((noi *)ptr)->phase,TWO_PI));}
static Float set_noi_phase(void *ptr, Float val) {((noi *)ptr)->phase = val; return(val);}
static Float noi_scaler(void *ptr) {return(((noi *)ptr)->base);}
static Float set_noi_scaler(void *ptr, Float val) {((noi *)ptr)->base = val; return(val);}

static int noi_equalp(void *p1, void *p2)
{
  noi *g1 = (noi *)p1;
  noi *g2 = (noi *)p2;
  return((p1 == p2) ||
	 (((g1->core)->type == (g2->core)->type) &&
	  (g1->freq == g2->freq) &&
	  (g1->phase == g2->phase) &&
	  (g1->output == g2->output) &&
	  (g1->incr == g2->incr) &&
	  (g1->base == g2->base)));
}

static char *describe_noi(void *ptr)
{
  char *desc;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_rand_p((mus_any *)ptr))
	sprintf(desc,"rand freq: %.3fHz, phase: %.3f, amp: %.3f",
		mus_radians2hz(((noi *)ptr)->freq),((noi *)ptr)->phase,((noi *)ptr)->base);
      else
	{
	  if (mus_rand_interp_p((mus_any *)ptr))
	    sprintf(desc,"rand_interp freq: %.3fHz, phase: %.3f, base: %.3f, incr: %.3f",
		mus_radians2hz(((noi *)ptr)->freq),((noi *)ptr)->phase,((noi *)ptr)->base,((noi *)ptr)->incr);
	  else describe_bad_gen(ptr,desc,"rand","a");
	}
    }
  return(desc);
}

static mus_any_class RAND_INTERP_CLASS = {
  MUS_RAND_INTERP,
  "rand_interp",
  &free_noi,
  &describe_noi,
  &inspect_noi,
  &noi_equalp,
  0,0,0,0,
  &noi_freq,
  &set_noi_freq,
  &noi_phase,
  &set_noi_phase,
  &noi_scaler,
  &set_noi_scaler
};

mus_any *mus_make_rand_interp(Float freq, Float base)
{
  noi *gen;
  gen = (noi *)CALLOC(1,sizeof(noi));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_rand_interp!");
  else
    {
      gen->core = &RAND_INTERP_CLASS;
      gen->freq = mus_hz2radians(freq);
      gen->base = base;
      gen->output = 0.0;
      gen->incr =  mus_random(base) * freq / sampling_rate;
      return((mus_any *)gen);
    }
  return(NULL);
}

static mus_any_class RAND_CLASS = {
  MUS_RAND,
  "rand",
  &free_noi,
  &describe_noi,
  &inspect_noi,
  &noi_equalp,
  0,0,0,0,
  &noi_freq,
  &set_noi_freq,
  &noi_phase,
  &set_noi_phase,
  &noi_scaler,
  &set_noi_scaler
};

mus_any *mus_make_rand(Float freq, Float base)
{
  noi *gen;
  gen = (noi *)CALLOC(1,sizeof(noi));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_rand!");
  else
    {
      gen->core = &RAND_CLASS;
      gen->freq = mus_hz2radians(freq);
      gen->base = base;
      gen->incr = 0.0;
      gen->output = 0.0;
      return((mus_any *)gen);
    }
  return(NULL);
}


/* ---------------- asymmetric-fm ---------------- */

typedef struct {
  mus_any_class *core;
  Float r;
  Float freq;
  Float ratio;
  Float phase;
  Float cosr;
  Float sinr;
} asyfm;

static char *inspect_asyfm (void *ptr)
{
  asyfm *gen = (asyfm *)ptr;
  char *desc;
  desc = make_desc_buf(ptr,FALSE);
  if (desc) sprintf(desc,"asyfm r: %f, freq: %f, phase: %f, ratio: %f, cosr: %f, sinr: %f",
		    gen->r,gen->freq,gen->phase,gen->ratio,gen->cosr,gen->sinr);
  return(desc);
}

static int free_asymmetric_fm(void *ptr) {if (ptr) FREE(ptr); return(0);}
static Float asyfm_freq(void *ptr) {return(mus_radians2hz(((asyfm *)ptr)->freq));}
static Float set_asyfm_freq(void *ptr, Float val) {((asyfm *)ptr)->freq = mus_hz2radians(val); return(val);}
static Float asyfm_phase(void *ptr) {return(fmod(((asyfm *)ptr)->phase,TWO_PI));}
static Float set_asyfm_phase(void *ptr, Float val) {((asyfm *)ptr)->phase = val; return(val);}

static int asyfm_equalp(void *p1, void *p2)
{
  return((p1 == p2) ||
	 (((((mus_any *)p1)->core)->type == (((mus_any *)p2)->core)->type) &&
	  ((((asyfm *)p1)->freq) == (((asyfm *)p2)->freq)) && 
	  ((((asyfm *)p1)->phase) == (((asyfm *)p2)->phase)) &&
	  ((((asyfm *)p1)->ratio) == (((asyfm *)p2)->ratio)) &&
	  ((((asyfm *)p1)->r) == (((asyfm *)p2)->r))));
}

static char *describe_asyfm(void *ptr)
{
  char *desc;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_asymmetric_fm_p((mus_any *)ptr))
	sprintf(desc,"asymmetric-fm freq: %.3fHz, phase: %.3f, ratio: %.3f, r: %.3f",
		mus_radians2hz(((asyfm *)ptr)->freq),((asyfm *)ptr)->phase,((asyfm *)ptr)->ratio,((asyfm *)ptr)->r);
      else describe_bad_gen(ptr,desc,"asymmetric_fm","an");
    }
  return(desc);
}

int mus_asymmetric_fm_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_ASYMMETRIC_FM));}

Float mus_asymmetric_fm(mus_any *ptr, Float index, Float fm)
{
  asyfm *gen = (asyfm *)ptr;
  Float result,mth;
  mth = gen->ratio * gen->phase;
  result = exp(index * gen->cosr * cos(mth)) * mus_sin(gen->phase + gen->sinr * mus_sin(mth));
  gen->phase += (gen->freq + fm);
  if ((gen->phase > 100.0) || (gen->phase < -100.0)) gen->phase = fmod(gen->phase,TWO_PI);
  return(result);
}

static mus_any_class ASYMMETRIC_FM_CLASS = {
  MUS_ASYMMETRIC_FM,
  "asymmetric_fm",
  &free_asymmetric_fm,
  &describe_asyfm,
  &inspect_asyfm,
  &asyfm_equalp,
  0,0,0,0,
  &asyfm_freq,
  &set_asyfm_freq,
  &asyfm_phase,
  &set_asyfm_phase,
  0,0
};

mus_any *mus_make_asymmetric_fm(Float freq, Float phase, Float r, Float ratio) /* r default 1.0, ratio 1.0 */
{
 asyfm *gen;
 gen = (asyfm *)CALLOC(1,sizeof(asyfm));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_asymmetric_fm!");
  else
    {
      gen->core = &ASYMMETRIC_FM_CLASS;
      gen->freq = mus_hz2radians(freq);
      gen->phase = phase;
      gen->r = r;
      gen->ratio = ratio;
      gen->cosr = 0.5 * (r - 1.0/r);
      gen->sinr = 0.5 * (r + 1.0/r);
      return((mus_any *)gen);
    }
  return(NULL);
}


/* ---------------- simple filters ---------------- */

typedef struct {
  mus_any_class *core;
  Float a0;
  Float a1;
  Float a2;
  Float b1;
  Float b2;
  Float x1;
  Float x2;
  Float y1;
  Float y2;
} smpflt;

static char *inspect_smpflt(void *ptr)
{
  smpflt *gen = (smpflt *)ptr;
  char *desc;
  desc = make_desc_buf(ptr,FALSE);
  if (desc) sprintf(desc,"smpflt a0: %f, a1: %f, a2: %f, b1: %f, b2: %f, x1: %f, x2: %f, y1: %f, y2: %f",
		    gen->a0,gen->a1,gen->a2,gen->b1,gen->b2,gen->x1,gen->x2,gen->y1,gen->y2);
  return(desc);
}

static int free_smpflt(void *ptr) {if (ptr) FREE(ptr); return(0);}

static int smpflt_equalp(void *p1, void *p2)
{
  smpflt *g1 = (smpflt *)p1;
  smpflt *g2 = (smpflt *)p2;
  return((p1 == p2) ||
	 (((g1->core)->type == (g2->core)->type) &&
	  (g1->a0 == g2->a0) &&
	  (g1->a1 == g2->a1) &&
	  (g1->a2 == g2->a2) &&
	  (g1->b1 == g2->b1) &&
	  (g1->b2 == g2->b2) &&
	  (g1->x1 == g2->x1) &&
	  (g1->x2 == g2->x2) &&
	  (g1->y1 == g2->y1) &&
	  (g1->y2 == g2->y2)));
}

static char *describe_smpflt(void *ptr)
{
  char *desc;
  smpflt *gen = (smpflt *)ptr;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      switch ((gen->core)->type)
	{
	case MUS_ONE_ZERO: sprintf(desc,"one_zero: a0: %.3f, a1: %.3f, x1: %.3f",gen->a0,gen->a1,gen->x1); break;
	case MUS_ONE_POLE: sprintf(desc,"one_pole: a0: %.3f, b1: %.3f, y1: %.3f",gen->a0,gen->b1,gen->y1); break;
	case MUS_TWO_ZERO: 
	  sprintf(desc,"two_zero: a0: %.3f, a1: %.3f, a2: %.3f, x1: %.3f, x2: %.3f",
		  gen->a0,gen->a1,gen->a2,gen->x1,gen->x2); 
	  break;
	case MUS_TWO_POLE: 
	  sprintf(desc,"two_pole: a0: %.3f, b1: %.3f, b2: %.3f, y1: %.3f, y2: %.3f",
		  gen->a0,gen->b1,gen->b2,gen->y1,gen->y2); 
	  break;
	default: describe_bad_gen(ptr,desc,"simple filter","a");
	}
    }
  return(desc);
}

Float mus_one_zero(mus_any *ptr, Float input)
{
  smpflt *gen = (smpflt *)ptr;
  Float result;
  result = (gen->a0 * input) + (gen->a1 * gen->x1);
  gen->x1 = input;
  return(result);
}

static mus_any_class ONE_ZERO_CLASS = {
  MUS_ONE_ZERO,
  "one_zero",
  &free_smpflt,
  &describe_smpflt,
  &inspect_smpflt,
  &smpflt_equalp,
  0,0,0,0,
  0,0,0,0,
  0,0
};

mus_any *mus_make_one_zero(Float a0, Float a1)
{
  smpflt *gen;
  gen = (smpflt *)CALLOC(1,sizeof(smpflt));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_one_zero!");
  else
    {
      gen->core = &ONE_ZERO_CLASS;
      gen->a0 = a0;
      gen->a1 = a1;
      return((mus_any *)gen);
    }
  return(NULL);
}

int mus_one_zero_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_ONE_ZERO));}

Float mus_one_pole(mus_any *ptr, Float input)
{
  smpflt *gen = (smpflt *)ptr;
  gen->y1 = (gen->a0 * input) - (gen->b1 * gen->y1);
  return(gen->y1);
}

static mus_any_class ONE_POLE_CLASS = {
  MUS_ONE_POLE,
  "one_pole",
  &free_smpflt,
  &describe_smpflt,
  &inspect_smpflt,
  &smpflt_equalp,
  0,0,0,0,
  0,0,0,0,
  0,0
};

mus_any *mus_make_one_pole(Float a0, Float b1)
{
  smpflt *gen;
  gen = (smpflt *)CALLOC(1,sizeof(smpflt));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_one_pole!");
  else
    {
      gen->core = &ONE_POLE_CLASS;
      gen->a0 = a0;
      gen->b1 = b1;
      return((mus_any *)gen);
    }
  return(NULL);
}

int mus_one_pole_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_ONE_POLE));}

Float mus_two_zero(mus_any *ptr, Float input)
{
  smpflt *gen = (smpflt *)ptr;
  Float result;
  result = (gen->a0 * input) + (gen->a1 * gen->x1) + (gen->a2 * gen->x2);
  gen->x2 = gen->x1;
  gen->x1 = input;
  return(result);
}

static mus_any_class TWO_ZERO_CLASS = {
  MUS_TWO_ZERO,
  "two_zero",
  &free_smpflt,
  &describe_smpflt,
  &inspect_smpflt,
  &smpflt_equalp,
  0,0,0,0,
  0,0,0,0,
  0,0
};

mus_any *mus_make_two_zero(Float a0, Float a1, Float a2)
{
 smpflt *gen;
 gen = (smpflt *)CALLOC(1,sizeof(smpflt));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_two_zero!");
  else
    {
      gen->core = &TWO_ZERO_CLASS;
      gen->a0 = a0;
      gen->a1 = a1;
      gen->a2 = a2;
      return((mus_any *)gen);
    }
  return(NULL);
}

int mus_two_zero_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_TWO_ZERO));}

mus_any *mus_make_zpolar(Float radius, Float frequency)
{
  return(mus_make_two_zero(1.0,-2.0 * radius * cos(mus_hz2radians(frequency)),radius * radius));
}

Float mus_two_pole(mus_any *ptr, Float input)
{
  smpflt *gen = (smpflt *)ptr;
  Float result;
  result = (gen->a0 * input) - (gen->b1 * gen->y1) - (gen->b2 * gen->y2);
  gen->y2 = gen->y1;
  gen->y1 = result;
  return(result);
}

static mus_any_class TWO_POLE_CLASS = {
  MUS_TWO_POLE,
  "two_pole",
  &free_smpflt,
  &describe_smpflt,
  &inspect_smpflt,
  &smpflt_equalp,
  0,0,0,0,
  0,0,0,0,
  0,0
};

mus_any *mus_make_two_pole(Float a0, Float b1, Float b2)
{
 smpflt *gen;
 gen = (smpflt *)CALLOC(1,sizeof(smpflt));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_two_pole!");
  else
    {
      gen->core = &TWO_POLE_CLASS;
      gen->a0 = a0;
      gen->b1 = b1;
      gen->b2 = b2;
      if (fabs(b1) >= 2.0) mus_error(MUS_UNSTABLE_TWO_POLE_ERROR,"make_two_pole: b1 = %.3f",b1);
      if (fabs(b2) >= 1.0) mus_error(MUS_UNSTABLE_TWO_POLE_ERROR,"make_two_pole: b2 = %.3f",b2);
      if ( ((b1*b1)-(b2*4.0) >= 0.0) &&
	   ( ((b1+b2) >= 1.0) || 
	     ((b2-b1) >= 1.0)))
	mus_error(MUS_UNSTABLE_TWO_POLE_ERROR,"make_two_pole: b1 = %.3f, b2 = %.3f",b1,b2);
      return((mus_any *)gen);
    }
  return(NULL);
}

int mus_two_pole_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_TWO_POLE));}

mus_any *mus_make_ppolar(Float radius, Float frequency)
{
  return(mus_make_two_pole(1.0,-2.0 * radius * cos(mus_hz2radians(frequency)),radius * radius));
}

Float mus_a0(mus_any *ptr)
{
  smpflt *gen = (smpflt *)ptr;
  if ((gen) && 
      (((gen->core)->type == MUS_ONE_ZERO) || ((gen->core)->type == MUS_ONE_POLE) ||
       ((gen->core)->type == MUS_TWO_ZERO) || ((gen->core)->type == MUS_TWO_POLE) ||
       ((gen->core)->type == MUS_FORMANT)))
    return(gen->a0);
  return(0.0);
}

Float mus_set_a0(mus_any *ptr, Float val)
{
  smpflt *gen = (smpflt *)ptr;
  if ((gen) && 
      (((gen->core)->type == MUS_ONE_ZERO) || ((gen->core)->type == MUS_ONE_POLE) ||
       ((gen->core)->type == MUS_TWO_ZERO) || ((gen->core)->type == MUS_TWO_POLE) ||
       ((gen->core)->type == MUS_FORMANT)))
    gen->a0 = val;
  return(val);
}

Float mus_a1(mus_any *ptr)
{
  smpflt *gen = (smpflt *)ptr;
  if ((gen) && (((gen->core)->type == MUS_ONE_ZERO) || ((gen->core)->type == MUS_TWO_ZERO) || ((gen->core)->type == MUS_FORMANT)))
      return(gen->a1);
  return(0.0);
}

Float mus_set_a1(mus_any *ptr, Float val)
{
  smpflt *gen = (smpflt *)ptr;
  if ((gen) && (((gen->core)->type == MUS_ONE_ZERO) || ((gen->core)->type == MUS_TWO_ZERO) || ((gen->core)->type == MUS_FORMANT)))
    gen->a1 = val;
  return(val);
}

Float mus_a2(mus_any *ptr)
{
  smpflt *gen = (smpflt *)ptr;
  if ((gen) && (((gen->core)->type == MUS_TWO_ZERO) || ((gen->core)->type == MUS_FORMANT)))
    return(gen->a2);
  return(0.0);
}

Float mus_set_a2(mus_any *ptr, Float val)
{
  smpflt *gen = (smpflt *)ptr;
  if ((gen) && (((gen->core)->type == MUS_TWO_ZERO) || ((gen->core)->type == MUS_FORMANT)))
    gen->a2 = val;
  return(val);
}

Float mus_b1(mus_any *ptr)
{
  smpflt *gen = (smpflt *)ptr;
  if ((gen) && (((gen->core)->type == MUS_ONE_POLE) || ((gen->core)->type == MUS_TWO_POLE) || ((gen->core)->type == MUS_FORMANT))) 
    return(gen->b1);
  return(0.0);
}

Float mus_set_b1(mus_any *ptr, Float val)
{
  smpflt *gen = (smpflt *)ptr;
  if ((gen) && (((gen->core)->type == MUS_ONE_POLE) || ((gen->core)->type == MUS_TWO_POLE) || ((gen->core)->type == MUS_FORMANT))) 
    gen->b1 = val;
  return(val);
}

Float mus_b2(mus_any *ptr)
{
  smpflt *gen = (smpflt *)ptr;
  if ((gen) && (((gen->core)->type == MUS_TWO_POLE) || ((gen->core)->type == MUS_FORMANT)))
    return(gen->b2);
  return(0.0);
}

Float mus_set_b2(mus_any *ptr, Float val)
{
  smpflt *gen = (smpflt *)ptr;
  if ((gen) && (((gen->core)->type == MUS_TWO_POLE) || ((gen->core)->type == MUS_FORMANT)))
    gen->b2 = val;
  return(val);
}



/* ---------------- formant ---------------- */

int mus_formant_p (mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_FORMANT));}

static char *describe_formant(void *ptr)
{
  char *desc;
  smpflt *gen = (smpflt *)ptr;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_formant_p((mus_any *)ptr))
	sprintf(desc,"formant: radius: %.3f, frequency: %.3f, (gain: %.3f)",
		-(gen->a2),mus_radians2hz(acos(gen->b1 / (2.0 * gen->a2))),gen->a1);
      else describe_bad_gen(ptr,desc,"formant","a");
    }
  return(desc);
}

static Float formant_frequency(void *ptr)
{
  smpflt *gen = (smpflt *)ptr;
  return(mus_radians2hz(acos(gen->b1 / (2.0 * gen->a2))));
}

static Float set_formant_frequency(void *ptr, Float val)
{
  smpflt *gen = (smpflt *)ptr;
  Float radius,gain;
  radius = -gen->a2;
  gain = gen->a1;
  gen->a0 = gain * sin(mus_hz2radians(val)) * (1.0 - (radius*radius));
  gen->b1 = -2.0 * radius * cos(mus_hz2radians(val));
  return(val);
}

Float mus_formant(mus_any *ptr, Float input) 
{
  smpflt *gen = (smpflt *)ptr;
  Float inval,tpinval,output;
  inval = gen->a0 * input;
  tpinval = inval + (gen->a2 * gen->x2);
  output = tpinval - (gen->b1 * gen->y1) - (gen->b2 * gen->y2);
  gen->y2 = gen->y1;
  gen->y1 = output;
  gen->x2 = gen->x1;
  gen->x1 = inval;
  return(output);
}

Float mus_formant_bank(Float *amps, mus_any **formants, Float *inputs, int size)
{
  int i;
  Float sum = 0.0;
  for (i=0;i<size;i++) 
    sum += (amps[i] * mus_formant(formants[i],inputs[i]));
  return(sum);
}

static mus_any_class FORMANT_CLASS = {
  MUS_FORMANT,
  "formant",
  &free_smpflt,
  &describe_formant,
  &inspect_smpflt,
  &smpflt_equalp,
  0,0,0,0,
  &formant_frequency,
  &set_formant_frequency,
  0,0,0,0
};

mus_any *mus_make_formant(Float radius, Float frequency, Float gain)
{
  smpflt *gen;
  gen = (smpflt *)CALLOC(1,sizeof(smpflt));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_formant!");
  else
    {
      gen->core = &FORMANT_CLASS;
      gen->a0 = gain * sin(mus_hz2radians(frequency)) * (1.0 - (radius*radius));
      gen->a2 = -radius;
      gen->b1 = -2.0 * radius * cos(mus_hz2radians(frequency));
      gen->b2 = radius * radius;
      gen->a1 = gain;  /* for frequency calc etc */

      return((mus_any *)gen);
    }
  return(NULL);
}

Float mus_formant_radius(mus_any *ptr)
{
  smpflt *gen = (smpflt *)ptr;
  if (mus_formant_p(ptr)) return(-(gen->a2));
  return(0.0);
}

Float mus_set_formant_radius(mus_any *ptr, Float val)
{
  smpflt *gen = (smpflt *)ptr;
  Float oldR;
  oldR = -(gen->a2);
  if (val >= 1.0) val = 0.9999999;
  gen->a2 = -val;
  gen->b2 = val * val;
  gen->b1 *= val / oldR;
  gen->a0 *= (1.0 - (val * val)) / (1.0 - (oldR * oldR));
  return(val);
}



/*---------------- sine-summation ---------------- */

typedef struct {
  mus_any_class *core;
  Float freq;
  Float phase;
  Float a,b,an,a2;
  int n;
} sss;

static char *inspect_sss(void *ptr)
{
  sss *gen = (sss *)ptr;
  char *desc;
  desc = make_desc_buf(ptr,FALSE);
  if (desc) sprintf(desc,"sss freq: %f, phase: %f, a: %f, b: %f, an: %f, a2: %f, n: %d",
		    gen->freq,gen->phase,gen->a,gen->b,gen->an,gen->a2,gen->n);
  return(desc);
}

static int free_sss(void *ptr) {if (ptr) FREE(ptr); return(0);}
static Float sss_freq(void *ptr) {return(mus_radians2hz(((sss *)ptr)->freq));}
static Float set_sss_freq(void *ptr, Float val) {((sss *)ptr)->freq = mus_hz2radians(val); return(val);}
static Float sss_phase(void *ptr) {return(fmod(((sss *)ptr)->phase,TWO_PI));}
static Float set_sss_phase(void *ptr, Float val) {((sss *)ptr)->phase = val; return(val);}

static int sss_equalp(void *p1, void *p2)
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

int mus_sine_summation_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_SINE_SUMMATION));}

static char *describe_sss(void *ptr)
{
  char *desc;
  sss *gen = (sss *)ptr;
  desc = make_desc_buf(ptr,TRUE);
  if (desc) 
    {
      if (mus_sine_summation_p((mus_any *)ptr))
	sprintf(desc,"sine_summation: frequency: %.3f, phase: %.3f, n: %d, a: %.3f, ratio: %.3f",
		mus_radians2hz(gen->freq),gen->phase,gen->n,gen->a,gen->b);
      else describe_bad_gen(ptr,desc,"sine_summation","a");
    }
  return(desc);
}

Float mus_sine_summation(mus_any *ptr, Float fm)
{
  sss *gen = (sss *)ptr;
  Float B,thB,result;
  B = gen->b * gen->phase;
  thB = gen->phase - B;
  if (gen->n == 0)
    result = (mus_sin(gen->phase) - (gen->a * mus_sin(thB))) / (gen->a2 - (2 * gen->a * cos(B)));
  result = (mus_sin(gen->phase) - 
	       (gen->a * mus_sin(thB)) - 
	       (gen->an * (mus_sin(gen->phase + (B * (gen->n + 1))) - 
                             (gen->a * mus_sin(gen->phase + (B * gen->n)))))) / 
             (gen->a2 - (2 * gen->a * cos(B)));
  gen->phase += (gen->freq + fm);
  gen->phase = fmod(gen->phase,TWO_PI);
  return(result);
}

static mus_any_class SINE_SUMMATION_CLASS = {
  MUS_SINE_SUMMATION,
  "sine_summation",
  &free_sss,
  &describe_sss,
  &inspect_sss,
  &sss_equalp,
  0,0,0,0,
  &sss_freq,
  &set_sss_freq,
  &sss_phase,
  &set_sss_phase,
  0,0
};

mus_any *mus_make_sine_summation(Float frequency, Float phase, int n, Float a, Float b_ratio)
{
  sss *gen;
  gen = (sss *)CALLOC(1,sizeof(sss));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_sine_summation!");
  else
    {
      gen->core = &SINE_SUMMATION_CLASS;
      gen->freq = mus_hz2radians(frequency);
      gen->phase = phase;
      if (n) gen->an = pow(a,n+1); else gen->an = 0.0;
      gen->a2 = 1.0 + a*a;
      gen->a = a;
      gen->n = n;
      gen->b = b_ratio;
      return((mus_any *)gen);
    }
  return(NULL);
}



/* ---------------- filter ---------------- */

typedef struct {
  mus_any_class *core;
  int order,state_allocated;
  Float *x,*y,*state;
} flt;

static char *inspect_flt(void *ptr)
{
  flt *gen = (flt *)ptr;
  char *desc;
  desc = make_big_desc_buf(ptr,FALSE);
  if (desc) sprintf(desc,"flt order: %d, state (%s): %s, x: %s, y: %s",
		    gen->order,
		    (gen->state_allocated) ? "local" : "external",
		    print_array(gen->state,gen->order,0),
		    print_array(gen->x,gen->order,0),
		    print_array(gen->y,gen->order,0));
  return(desc);
}

Float mus_filter (mus_any *ptr, Float input)
{
  flt *gen = (flt *)ptr;
  Float xout;
  int j;
  xout = 0.0;
  gen->state[0] = input;
  for (j=gen->order-1;j>=1;j--) 
    {
      xout += gen->state[j]*gen->x[j];
      gen->state[0] -= gen->y[j]*gen->state[j];
      gen->state[j] = gen->state[j-1];
    }
  return(xout+(gen->state[0]*gen->x[0]));
}

Float mus_fir_filter (mus_any *ptr, Float input)
{
  Float xout;
  int j;
  flt *gen = (flt *)ptr;
  xout = 0.0;
  gen->state[0] = input;
  for (j=gen->order-1;j>=1;j--) 
    {
      xout += gen->state[j]*gen->x[j];
      gen->state[j] = gen->state[j-1];
    }
  return(xout+(gen->state[0]*gen->x[0]));
}

Float mus_iir_filter (mus_any *ptr, Float input)
{
  int j;
  flt *gen = (flt *)ptr;
  gen->state[0] = input;
  for (j=gen->order-1;j>=1;j--) 
    {
      gen->state[0] -= gen->y[j]*gen->state[j];
      gen->state[j] = gen->state[j-1];
    }
  return(gen->state[0]);
}

int mus_filter_p (mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_FILTER));}
int mus_fir_filter_p (mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_FIR_FILTER));}
int mus_iir_filter_p (mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_IIR_FILTER));}

static int free_filter(void *ptr)
{
  flt *gen = (flt *)ptr;
  if (gen)
    {
      if ((gen->state) && (gen->state_allocated)) FREE(gen->state);
      FREE(gen);
    }
  return(0);
}

static int filter_length(void *ptr) {return(((flt *)ptr)->order);}
static int filter_equalp(void *p1, void *p2) {return(p1 == p2);}

static char *describe_filter(void *ptr)
{
  char *desc;
  flt *gen = (flt *)ptr;
  desc = make_big_desc_buf(ptr,TRUE);
  if (desc) sprintf(desc,"%s: order: %d",(gen->core)->name,gen->order);
  return(desc);
}

static mus_any_class FILTER_CLASS = {
  MUS_FILTER,
  "filter",
  &free_filter,
  &describe_filter,
  &inspect_flt,
  &filter_equalp,
  0,0,
  &filter_length,
  0,
  0,0,0,0,
  0,0
};

static mus_any_class FIR_FILTER_CLASS = {
  MUS_FIR_FILTER,
  "fir_filter",
  &free_filter,
  &describe_filter,
  &inspect_flt,
  &filter_equalp,
  0,0,
  &filter_length,
  0,
  0,0,0,0,
  0,0
};

static mus_any_class IIR_FILTER_CLASS = {
  MUS_IIR_FILTER,
  "iir_filter",
  &free_filter,
  &describe_filter,
  &inspect_flt,
  &filter_equalp,
  0,0,
  &filter_length,
  0,
  0,0,0,0,
  0,0
};

static mus_any *make_filter(mus_any_class *cls, const char *name, int order, Float *xcoeffs, Float *ycoeffs, Float *state) /* if state null, allocated locally */
{
  flt *gen;
  gen = (flt *)CALLOC(1,sizeof(dly));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_%s!",name);
  else
    {
      if (state)
	gen->state = state;
      else 
	{
	  gen->state = (Float *)CALLOC(order,sizeof(Float));
	  if (gen->state == NULL) 
	    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate %d bytes for state in mus_make_%s!",(int)(order * sizeof(Float)),name);
	  else gen->state_allocated = 1;
	}
      gen->core = cls;
      gen->order = order;
      gen->x = xcoeffs;
      gen->y = ycoeffs;
      return((mus_any *)gen);
    }
  return(NULL);
}

mus_any *mus_make_filter(int order, Float *xcoeffs, Float *ycoeffs, Float *state)
{
  return(make_filter(&FILTER_CLASS,"filter",order,xcoeffs,ycoeffs,state));
}

mus_any *mus_make_fir_filter(int order, Float *xcoeffs, Float *state)
{
  return(make_filter(&FIR_FILTER_CLASS,"fir_filter",order,xcoeffs,NULL,state));
}

mus_any *mus_make_iir_filter(int order, Float *ycoeffs, Float *state)
{
  return(make_filter(&IIR_FILTER_CLASS,"iir_filter",order,NULL,ycoeffs,state));
}

Float *mus_xcoeffs(mus_any *ptr)
{
  flt *gen = (flt *)ptr;
  if ((gen) && (((gen->core)->type == MUS_FILTER) || ((gen->core)->type == MUS_FIR_FILTER))) return(gen->x);
  return(NULL);
}

Float *mus_ycoeffs(mus_any *ptr)
{
  flt *gen = (flt *)ptr;
  if ((gen) && (((gen->core)->type == MUS_FILTER) || ((gen->core)->type == MUS_IIR_FILTER))) return(gen->y);
  return(NULL);
}

int mus_order(mus_any *ptr)
{
  if (ptr)
    {
      switch ((ptr->core)->type)
	{
	case MUS_FILTER: case MUS_FIR_FILTER: case MUS_IIR_FILTER: return(((flt *)ptr)->order); break;
	case MUS_ONE_POLE: case MUS_ONE_ZERO: return(1); break;
	case MUS_TWO_POLE: case MUS_TWO_ZERO: case MUS_FORMANT: return(2); break;
	case MUS_NOTCH: case MUS_COMB: case MUS_ALL_PASS: return(((dly *)ptr)->size); break;
	}
    }
  return(0);
}

#if 0
Float *mus_make_fir_coeffs(int order, Float *env_brkpts, int pts)
{
  /* or maybe use env here */
  /* get_filter_coeffs in snd-chn.c to make_filter in snd-dac.c */
}
#endif



/* ---------------- waveshape ---------------- */

typedef struct {
  mus_any_class *core;
  Float freq;
  Float phase;
  Float *table;
  int table_size;
  Float offset;
  int table_allocated;
} ws;

static char *inspect_ws(void *ptr)
{
  ws *gen = (ws *)ptr;
  char *desc;
  desc = make_big_desc_buf(ptr,FALSE);
  if (desc) sprintf(desc,"ws freq: %f, phase: %f, offset: %f, table[%d (%s)]: %s",
		    gen->freq,gen->phase,gen->offset,gen->table_size,
		    (gen->table_allocated) ? "local" : "external",
		    print_array(gen->table,gen->table_size,0));
  return(desc);
}

static int free_ws(void *pt) 
{
  ws *ptr = (ws *)pt;
  if (ptr) 
    {
      if ((ptr->table) && (ptr->table_allocated)) FREE(ptr->table);
      FREE(ptr); 
    }
  return(0);
}

static Float ws_freq(void *ptr) {return(mus_radians2hz(((ws *)ptr)->freq));}
static Float set_ws_freq(void *ptr, Float val) {((ws *)ptr)->freq = mus_hz2radians(val); return(val);}
static Float ws_phase(void *ptr) {return(fmod(((ws *)ptr)->phase,TWO_PI));}
static Float set_ws_phase(void *ptr, Float val) {((ws *)ptr)->phase = val; return(val);}
static int ws_size(void *ptr) {return(((ws *)ptr)->table_size);}
static int set_ws_size(void *ptr, int val) {((ws *)ptr)->table_size = val; return(val);}
static Float *ws_data(void *ptr) {return(((ws *)ptr)->table);}
static int ws_equalp(void *p1, void *p2) {return(p1 == p2);}

static Float *set_ws_data(void *ptr, Float *val) 
{
  ws *gen = (ws *)ptr;
  if (gen->table_allocated) {FREE(gen->table); gen->table_allocated = 0;}
  gen->table = val; 
  return(val);
}

static char *describe_waveshape(void *ptr)
{
  char *desc;
  desc = make_big_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_waveshape_p((mus_any *)ptr))
	sprintf(desc,"waveshape freq: %.3fHz, phase: %.3f, size: %d",
		mus_radians2hz(((ws *)ptr)->freq),((ws *)ptr)->phase,((ws *)ptr)->table_size);
      else describe_bad_gen(ptr,desc,"waveshape","a");
    }
  return(desc);
}

static mus_any_class WAVESHAPE_CLASS = {
  MUS_WAVESHAPE,
  "waveshape",
  &free_ws,
  &describe_waveshape,
  &inspect_ws,
  &ws_equalp,
  &ws_data,
  &set_ws_data,
  &ws_size,
  &set_ws_size,
  &ws_freq,
  &set_ws_freq,
  &ws_phase,
  &set_ws_phase,
  0,0
};

int mus_waveshape_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_WAVESHAPE));}

mus_any *mus_make_waveshape(Float frequency, Float phase, Float *table, int size)
{
  ws *gen;
  gen = (ws *)CALLOC(1,sizeof(ws));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_waveshape!");
  else
    {
      gen->core = &WAVESHAPE_CLASS;
      gen->freq = mus_hz2radians(frequency);
      gen->phase = phase;
      if (table)
	{
	  gen->table = table;
	  gen->table_allocated = 0;
	}
      else
	{
	  gen->table = (Float *)CALLOC(size,sizeof(Float));
	  if (gen->table == NULL) 
	    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate %d bytes for table in mus_make_waveshape!",(int)(size * sizeof(Float)));
	  else gen->table_allocated = 1;
	}
      gen->table_size = size;
      gen->offset = (Float)size / 2.0;
      return((mus_any *)gen);
    }
  return(NULL);
}

Float mus_waveshape(mus_any *ptr, Float index, Float fm)
{
  ws *gen = (ws *)ptr;
  Float oscval;
  oscval = mus_sin(gen->phase);
  gen->phase += (gen->freq + fm);
  if ((gen->phase > 100.0) || (gen->phase < -100.0)) gen->phase = fmod(gen->phase,TWO_PI);
  return(mus_array_interp(gen->table,gen->offset * (1.0 + (oscval * index)),gen->table_size));
}

Float *mus_partials2waveshape(int npartials, Float *partials, int size, Float *table)
{
  /* partials incoming is a list of partials amps indexed by partial number */
  /* #<0.0,1.0,0.0> = 2nd partial 1.0, rest 0. */
  int i,hnum;
  Float sum = 0.0,maxI2,temp,x,Tn,Tn1;
  Float *data;
  for (i=0;i<npartials;i++) sum += partials[i];
  if (sum != 0.0) for (i=0;i<npartials;i++) partials[i] /= sum;
  for (i=2;i<npartials;i+=4)
    {
      partials[i] = (-partials[i]);
      if (npartials > (i+1)) partials[i+1] = (-partials[i+1]);
    }
  if (table == NULL)
    {
      data = (Float *)CALLOC(size,sizeof(Float));
      if (data == NULL)
	{
	  mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't get %d bytes for table in mus_make_waveshape_table!",(int)(size * sizeof(Float)));
	  return(NULL);
	}
    }
  else data = table;
  maxI2 = 2.0 / (Float)size;
  for (i=0,x=-1.0;i<size;i++,x+=maxI2)
    {
      sum = 0.0;
      temp = 0.0;
      Tn = 1.0;
      Tn1 = x;
      for (hnum=0;hnum<npartials;hnum++)
	{
	  sum += (Tn * partials[hnum]);
	  temp = Tn1;
	  Tn1 = (2.0 * Tn1 * x) - Tn;
	  Tn = temp;
	}
      data[i] = sum;
    }
  return(array_normalize(data,size));
}

Float *mus_partials2polynomial(int npartials, Float *partials, int kind)
{
  /* coeffs returned in partials */
  int i,k,bytes;
  Float amp = 0.0;
  int *T0,*T1,*Tn;
  Float *Cc1;
  bytes = (npartials + 1) * sizeof(int);
  T0 = (int *)CALLOC(npartials+1,sizeof(int));
  if (T0 == NULL) {mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't get %d bytes for T0 in mus_partials2polynomial!",bytes); return(NULL);}
  T1 = (int *)CALLOC(npartials+1,sizeof(int));
  if (T1 == NULL) {mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't get %d bytes for T1 in mus_partials2polynomial!",bytes); FREE(T0); return(NULL);}
  Tn = (int *)CALLOC(npartials+1,sizeof(int));
  if (Tn == NULL) {mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't get %d bytes for Tn in mus_partials2polynomial!",bytes); FREE(T0); FREE(T1); return(NULL);}
  Cc1 = (Float *)CALLOC(npartials+1,sizeof(Float));
  if (Cc1 == NULL) 
    {
      mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't get %d bytes for Cc1 in mus_partials2polynomial!",npartials * sizeof(Float));
      FREE(T0); 
      FREE(T1); 
      FREE(Tn); 
      return(NULL);
    }
  T0[0] = kind;
  T1[1] = 1;
  for (i=1;i<npartials;i++)
    {
      amp = partials[i];
      if (amp != 0.0)
	{
	  if (kind == 1)
	    {
	      for (k=0;k<=i;k++) Cc1[k] += (amp * T1[k]);
	    }
	  else
	    {
	      for (k=1;k<=i;k++) Cc1[k-1] += (amp * T1[k]);
	    }
	}
      for (k=i+1;k>0;k--) Tn[k] = (2 * T1[k-1]) - T0[k];
      Tn[0] = -T0[0];
      for (k=i+1;k>=0;k--)
	{
	  T0[k] = T1[k];
	  T1[k] = Tn[k];
	}
    }
  for (i=0;i<npartials;i++) partials[i] = Cc1[i];
  FREE(T0);
  FREE(T1);
  FREE(Tn);
  FREE(Cc1);
  return(partials);
}



/* ---------------- env ---------------- */

/* although a pain, this way of doing env is 5 times faster than a table lookup,
 * in the linear segment and step cases.  In the exponential case, it is
 * only slightly slower (but more accurate).
 */

typedef struct {
  mus_any_class *core;
  double rate,current_value,base,offset,scaler,power,init_y,init_power,b1;
  int pass,end,style,index,size,data_allocated;
  Float *original_data;
  double *rates;
  int *passes;
} seg;

enum {ENV_SEG,ENV_STEP,ENV_EXP};

static char *inspect_seg(void *ptr)
{
  seg *gen = (seg *)ptr;
  char *desc;
  desc = make_big_desc_buf(ptr,FALSE);
  if (desc) sprintf(desc,"seg rate: %f, current_value: %f, base: %f, offset: %f, scaler: %f, power: %f, init_y: %f, init_power: %f, b1: %f, pass: %d, end: %d, style: %d, index: %d, size: %d, original_data[%d]: %s, rates[%d]: %s, passes[%d]: %s",
		    gen->rate,gen->current_value,gen->base,gen->offset,gen->scaler,gen->power,gen->init_y,gen->init_power,gen->b1,
		    gen->pass,gen->end,gen->style,gen->index,gen->size,
		    gen->size * 2,print_array(gen->original_data,gen->size*2,0),
		    gen->size,print_double_array(gen->rates,gen->size,0),
		    gen->size,print_int_array(gen->passes,gen->size,0));
  return(desc);
}

/* what about breakpoint triples for per-segment exp envs? */

int mus_env_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_ENV));}

Float mus_env (mus_any *ptr)
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
	  gen->current_value = gen->offset + (gen->scaler * (exp(gen->base * gen->power) - 1.0));
	}
      break;
    }
  gen->pass++;
  return(val);
}

static void dmagify_env(seg *e, Float *data, int pts, int dur, Float scaler)
{ 
  int i,j,passes;
  double curx,curpass = 0.0;
  double x0,y0,x1,y1,xmag;
  e->size = pts;
  x1 = data[0];
  xmag = (double)dur/(double)(data[pts*2 - 2] - x1);
  y1 = data[1];
  e->rates = (double *)CALLOC(pts,sizeof(double));
  e->passes = (int *)CALLOC(pts,sizeof(int));
  for (j=0,i=2;i<pts*2;i+=2,j++)
    {
      x0 = x1;
      x1 = data[i];
      y0 = y1;
      y1 = data[i+1];
      curx = xmag * (x1-x0);
      if (curx < 1.0) curx = 1.0;
      curpass += curx;
      e->passes[j] = (int)curpass;
      if (j==0) passes = e->passes[0]; else passes = e->passes[j] - e->passes[j-1];
      if (e->style == ENV_STEP)
	e->rates[j] = e->offset + (scaler * y0);
      else
	{
	  if (y0 == y1) 
	    e->rates[j]=0.0;
	  else e->rates[j] = scaler*(y1-y0)/(double)passes;
	}
    }
  e->passes[pts - 1] = 100000000;
}

static Float *fixup_exp_env(seg *e, Float *data, int pts, Float offset, Float scaler, Float base)
{
  Float min_y,max_y,val = 0.0,tmp = 0.0,b;
  int flat,len,i;
  Float *result = NULL;
  if ((base <= 0.0) || (base == 1.0)) return(NULL);
  min_y = offset + scaler * data[1];
  max_y = min_y;
  b = 1.0 / log(base);
  e->b1 = base-1.0;
  len = pts*2;
  result = (Float *)CALLOC(len,sizeof(Float));
  result[0] = data[0];
  result[1] = min_y;
  for (i=2;i<len;i+=2)
    {
      tmp = offset + scaler * data[i+1];
      result[i] = data[i];
      result[i+1] = tmp;
      if (tmp<min_y) min_y = tmp;
      if (tmp>max_y) max_y = tmp;
    }
  flat = (min_y == max_y);
  if (!flat) val = 1.0 / (max_y - min_y);
  for (i=1;i<len;i+=2)
    {
      if (flat) 
	tmp = 1.0;
      else tmp = val * (result[i] - min_y);
      result[i] = log(1.0+(tmp*e->b1)) * b;
    }
  e->scaler = (max_y - min_y) / e->b1;
  e->offset = min_y;
  return(result);
}

static int env_equalp(void *p1, void *p2) {return(p1 == p2);}

static char *describe_env(void *ptr)
{
  char *desc,*str = NULL;
  seg *e = (seg *)ptr;
  desc = make_big_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_env_p((mus_any *)ptr))
	sprintf(desc,"env: %s, pass: %d (dur: %d), index: %d, data: %s",
		((e->style == ENV_SEG) ? "linear" : ((e->style == ENV_EXP) ? "exponential" : "step")),
		e->pass,e->end+1,e->index,
		str = print_array(e->original_data,e->size*2,0));
      else describe_bad_gen(ptr,desc,"env","an");
      if (str) FREE(str);
    }
  return(desc);
}

static int free_env(void *pt) 
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

static Float *env_data(void *ptr) {return(((seg *)ptr)->original_data);}
static Float env_scaler(void *ptr) {return(((seg *)ptr)->scaler);}
static int env_size(void *ptr) {return(((seg *)ptr)->size);}

static mus_any_class ENV_CLASS = {
  MUS_ENV,
  "env",
  &free_env,
  &describe_env,
  &inspect_seg,
  &env_equalp,
  &env_data,
  0,
  &env_size,
  0,
  0,0,0,0,
  &env_scaler,
  0
};

mus_any *mus_make_env(Float *brkpts, int npts, Float scaler, Float offset, Float base, Float duration, int start, int end, Float *odata)
{
  int i,dur_in_samples;
  Float *edata;
  seg *e = NULL;
  e = (seg *)CALLOC(1,sizeof(seg));
  if (e == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate env struct!");
  else
    {
      e->core = &ENV_CLASS;
      if (duration != 0.0)
	dur_in_samples = (int)(duration * sampling_rate);
      else dur_in_samples = (end-start+1);
      e->init_y = offset + scaler * brkpts[1];
      e->current_value = e->init_y;
      e->rate = 0.0;
      e->offset = offset;
      e->scaler = scaler;
      if (base > 0.0) e->base = log(base); /* used expt in lisp, fixup here for c's exp */
      e->end = (dur_in_samples - 1);
      e->pass = 0;
      e->index = 0; /* ? */
      if (odata)
	e->original_data = odata;
      else
	{
	  e->original_data = (Float *)CALLOC(npts*2,sizeof(Float));
	  if (e->original_data == NULL) 
	    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate env original_data array!");
	  e->data_allocated = 1;
	}
      for (i=0;i<npts*2;i++) e->original_data[i] = brkpts[i];
      if (base == 0.0)
	{
	  e->style = ENV_STEP;
	  dmagify_env(e,brkpts,npts,dur_in_samples,scaler);
	}
      else
	{
	  if (base == 1.0)
	    {
	      e->style = ENV_SEG;
	      dmagify_env(e,brkpts,npts,dur_in_samples,scaler);
	    }
	  else
	    {
	      e->style = ENV_EXP;
	      edata = fixup_exp_env(e,brkpts,npts,offset,scaler,base);
	      dmagify_env(e,edata,npts,dur_in_samples,1.0);
	      e->power = edata[1];
	      e->init_power = e->power;
	      if (edata) FREE(edata);
	    }
	}
      e->rate = e->rates[0];
      return((mus_any *)e);
    }
  return(NULL);
}

void mus_restart_env (mus_any *ptr)
{
  seg *gen = (seg *)ptr;
  gen->current_value = gen->init_y;
  gen->pass = 0;
  gen->index = 0;
  gen->rate = gen->rates[0];
  gen->power = gen->init_power;
}

static Float set_env_location(mus_any *ptr, int val)
{
  seg *gen = (seg *)ptr;
  int ctr=0,passes;
  mus_restart_env(ptr);
  gen->pass = val;
  while ((gen->index < gen->size) && (ctr < val))
    {
      if (val > gen->passes[gen->index])
	passes = gen->passes[gen->index] - ctr;
      else passes = val - ctr;
      switch (gen->style)
	{
	case ENV_SEG: gen->current_value += (passes * gen->rate); break;
	case ENV_STEP: gen->current_value = gen->rate; break;
	case ENV_EXP: 
	  gen->power += (passes * gen->rate); 
	  gen->current_value = gen->offset + (gen->scaler * (exp(gen->base * gen->power) - 1.0));
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
  return(gen->current_value);
}

static Float mus_env_interp_1(Float x, mus_any *ptr)
{
  seg *gen = (seg *)ptr;
  Float *data;
  int i;
  if (gen)
    {
      data = gen->original_data;
      if (data)
	{
	  for (i=0;i<gen->size*2-2;i+=2)
	    {
	      if (x <= data[i]) return(data[i+1]);
	      if (x < data[i+2])
		{
		  if (data[i+1] == data[i+3]) return(data[i+1]);
		  switch (gen->style)
		    {
		    case ENV_STEP: 
		      return(data[i+1]); 
		      break;
		    case ENV_SEG:
		      return(data[i+1] + ((x - data[i]) / (data[i+2] - data[i])) * (data[i+3] - data[i+1]));
		      break;
		    case ENV_EXP:
		      return(data[i+1] + ((data[i+3] - data[i+1]) / gen->b1) * (exp(gen->base * (x - data[i]) / (data[i+2] - data[i])) - 1.0));
		      break;
		    }
		}
	    }
	  return(data[gen->size*2 - 1]);
	}
    }
  return(0.0);
}

Float mus_env_interp(Float x, mus_any *ptr)
{
  seg *gen = (seg *)ptr;
  return(gen->offset + (gen->scaler * mus_env_interp_1(x,ptr)));
}



/* ---------------- frame ---------------- */

static int free_frame(void *pt)
{
  mus_frame *ptr = (mus_frame *)pt;
  if (ptr)
    {
      if (ptr->vals) FREE(ptr->vals);
      FREE(ptr);
    }
  return(0);
}

static char *describe_frame(void *ptr)
{
  mus_frame *gen;
  char *desc,*str = NULL;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_frame_p((mus_any *)ptr))
	{
	  gen = (mus_frame *)ptr;
	  sprintf(desc,"frame[%d]: %s",gen->chans,str = print_array(gen->vals,gen->chans,0));
	  if (str) FREE(str);
	}
      else describe_bad_gen(ptr,desc,"frame","a");
    }
  return(desc);
}

static char *inspect_frame(void *ptr)
{
  mus_frame *gen = (mus_frame *)ptr;
  char *desc;
  desc = make_desc_buf(ptr,FALSE);
  if (desc) sprintf(desc,"frame[%d]: %s",gen->chans,print_array(gen->vals,gen->chans,0));
  return(desc);
}

int mus_frame_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_FRAME));}

static int equalp_frame(void *p1, void *p2)
{
  mus_frame *g1,*g2;
  int i;
  if (p1 == p2) return(TRUE);
  g1 = (mus_frame *)p1;
  g2 = (mus_frame *)p2;
  if (((g1->core)->type != (g2->core)->type) ||
      (g1->chans != g2->chans))
    return(FALSE);
  for (i=0;i<g1->chans;i++)
    if (g1->vals[i] != g2->vals[i])
      return(FALSE);
  return(TRUE);
}

static Float *frame_data(void *ptr) {return(((mus_frame *)ptr)->vals);}
static Float *set_frame_data(void *ptr, Float *new_data) {((mus_frame *)ptr)->vals = new_data; return(new_data);}
static int frame_length(void *ptr) {return(((mus_frame *)ptr)->chans);}
static int set_frame_length(void *ptr, int new_len) {((mus_frame *)ptr)->chans = new_len; return(new_len);}

static mus_any_class FRAME_CLASS = {
  MUS_FRAME,
  "frame",
  &free_frame,
  &describe_frame,
  &inspect_frame,
  &equalp_frame,
  &frame_data,
  &set_frame_data,
  &frame_length,
  &set_frame_length,
  0,0,0,0,
  0,0
};

mus_frame *mus_make_empty_frame(int chans)
{
  mus_frame *nf;
  nf = (mus_frame *)CALLOC(1,sizeof(mus_frame));
  if (nf == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate mus_frame struct!");
  else
    {
      nf->core = &FRAME_CLASS;
      nf->chans = chans;
      nf->vals = (Float *)CALLOC(chans,sizeof(Float));
      if (nf->vals == NULL) 
	mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate frame in mus_make_empty_frame");
      return(nf);
    }
  return(NULL);
}

mus_frame *mus_make_frame(int chans, ...)
{
  mus_frame *nf;
  va_list ap;
  int i;
  nf = mus_make_empty_frame(chans);
  if (nf)
    {
      va_start(ap,chans);
      for (i=0;i<chans;i++)
	nf->vals[i] = (Float)(va_arg(ap,double)); /* float not safe here apparently */
      va_end(ap);
      return(nf);
    }
  return(NULL);
}

mus_frame *mus_frame_add(mus_frame *f1, mus_frame *f2, mus_frame *res)
{
  int chans,i;
  chans = f1->chans;
  if (f2->chans < chans) chans = f2->chans;
  if (res)
    {
      if (res->chans < chans) chans = res->chans;
    }
  else res = mus_make_empty_frame(chans);
  for (i=0;i<chans;i++) res->vals[i] = f1->vals[i] + f2->vals[i];
  return(res);
}

mus_frame *mus_frame_multiply(mus_frame *f1, mus_frame *f2, mus_frame *res)
{
  int chans,i;
  chans = f1->chans;
  if (f2->chans < chans) chans = f2->chans;
  if (res)
    {
      if (res->chans < chans) chans = res->chans;
    }
  else res = mus_make_empty_frame(chans);
  for (i=0;i<chans;i++) res->vals[i] = f1->vals[i] * f2->vals[i];
  return(res);
}

Float mus_frame_ref(mus_frame *f, int chan) {return(f->vals[chan]);}
Float mus_frame_set(mus_frame *f, int chan, Float val) {f->vals[chan] = val; return(val);}
Float *mus_frame_data(mus_frame *f) {return(f->vals);}



/* ---------------- mixer ---------------- */

static int free_mixer(void *pt)
{
  int i;
  mus_mixer *ptr = (mus_mixer *)pt;
  if (ptr)
    {
      if (ptr->vals) 
	{
	  for (i=0;i<ptr->chans;i++) FREE(ptr->vals[i]);
	  FREE(ptr->vals);
	}
      FREE(ptr);
    }
  return(0);
}

static char *describe_mixer(void *ptr)
{
  mus_mixer *gen;
  char *desc,*str;
  int i,j;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_mixer_p((mus_any *)ptr))
	{
	  gen = (mus_mixer *)ptr;
	  sprintf(desc,"mixer: chans: %d, vals: [",gen->chans);
	  str = (char *)CALLOC(16,sizeof(char));
	  for (i=0;i<gen->chans;i++)
	    {
	      for (j=0;j<gen->chans;j++)
		{
		  sprintf(str,"%s%.3f%s%s",
			  (j == 0) ? "(" : "",
			  gen->vals[i][j],
			  (j == (gen->chans - 1)) ? ")" : "",
			  ((i == (gen->chans - 1)) && (j == (gen->chans - 1))) ? "]" : " ");
		  strcat(desc,str);
		}
	    }
	  FREE(str);
	}
      else describe_bad_gen(ptr,desc,"mixer","a");
    }
  return(desc);
}

static char *inspect_mixer(void *ptr)
{
  mus_mixer *gen = (mus_mixer *)ptr;
  int i,j;
  char *desc;
  desc = make_desc_buf(ptr,FALSE);
  if (desc) 
    {
      sprintf(desc,"mixer: chans: %d, vals: [",gen->chans);
      for (i=0;i<gen->chans;i++)
	for (j=0;j<gen->chans;j++)
	  fprintf(stderr,"%s%.3f%s%s",
		  (j == 0) ? "(" : "",gen->vals[i][j],(j == (gen->chans - 1)) ? ")" : "",
		  ((i == (gen->chans - 1)) && (j == (gen->chans - 1))) ? "]" : " ");
    }
  return(desc);
}

int mus_mixer_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_MIXER));}

static int equalp_mixer(void *p1, void *p2)
{
  mus_mixer *g1,*g2;
  int i,j;
  if (p1 == p2) return(TRUE);
  if ((p1 == NULL) || (p2 == NULL)) return(FALSE);
  g1 = (mus_mixer *)p1;
  g2 = (mus_mixer *)p2;
  if (((g1->core)->type != (g2->core)->type) ||
      (g1->chans != g2->chans))
    return(FALSE);
  for (i=0;i<g1->chans;i++)
    for (j=0;j<g1->chans;j++)
      if (g1->vals[i][j] != g2->vals[i][j])
	return(FALSE);
  return(TRUE);
}

static int mixer_length(void *ptr) {return(((mus_mixer *)ptr)->chans);}

static mus_any_class MIXER_CLASS = {
  MUS_MIXER,
  "mixer",
  &free_mixer,
  &describe_mixer,
  &inspect_mixer,
  &equalp_mixer,
  0,0,
  &mixer_length,
  0,
  0,0,0,0,
  0,0
};

mus_mixer *mus_make_empty_mixer(int chans)
{
  mus_mixer *nf;
  int i;
  nf = (mus_mixer *)CALLOC(1,sizeof(mus_mixer));
  if (nf == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate mus_mixer struct!");
  else
    {
      nf->core = &MIXER_CLASS;
      nf->chans = chans;
      nf->vals = (Float **)CALLOC(chans,sizeof(Float));
      if (nf->vals == NULL) 
	mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate mixer in mus_make_empty_mixer");
      for (i=0;i<chans;i++)
	{
	  nf->vals[i] = (Float *)CALLOC(chans,sizeof(Float));
	  if (nf->vals[i] == NULL) 
	    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate mixer column in mus_make_empty_mixer");
	}
      return(nf);
    }
  return(NULL);
}

mus_mixer *mus_make_identity_mixer(int chans)
{
  mus_mixer *mx;
  int i;
  mx = mus_make_empty_mixer(chans);
  if (mx) for (i=0;i<chans;i++) mx->vals[i][i] = 1.0;
  return(mx);
}

mus_mixer *mus_make_mixer(int chans, ...)
{
  mus_mixer *mx;
  int i,j;
  va_list ap;
  mx = mus_make_empty_mixer(chans);
  if (mx) 
    {
      va_start(ap,chans);
      for (i=0;i<chans;i++)
	for (j=0;j<chans;j++)
	  mx->vals[i][j] = (Float)(va_arg(ap,double));
      va_end(ap);
    }
  return(mx);
}

Float **mus_mixer_data(mus_mixer *f) {return(f->vals);}
Float mus_mixer_ref(mus_mixer *f, int in, int out) {return(f->vals[in][out]);}
Float mus_mixer_set(mus_mixer *f, int in, int out, Float val) {f->vals[in][out] = val; return(val);}

mus_frame *mus_frame2frame(mus_mixer *f, mus_frame *in, mus_frame *out)
{
  int i,j,in_chans,out_chans;
  in_chans = in->chans;
  if (in_chans > f->chans) in_chans = f->chans;
  out_chans = f->chans;
  if (out)
    {
      if (out->chans < out_chans) out_chans = out->chans;
    }
  else out = mus_make_empty_frame(out_chans);
  for (i=0;i<out_chans;i++)
    {
      out->vals[i] = 0.0;
      for (j=0;j<in_chans;j++)
	out->vals[i] += (in->vals[j] * f->vals[j][i]);
    }
  return(out);
}

mus_frame *mus_sample2frame(mus_any *f, Float in, mus_frame *out)
{
  int i,chans;
  mus_mixer *mx;
  mus_frame *fr;
  if (mus_frame_p(f))
    {
      fr = (mus_frame *)f;
      chans = fr->chans;
      if (out)
	{
	  if (out->chans < chans) chans = out->chans;
	}
      else out = mus_make_empty_frame(chans);
      for (i=0;i<chans;i++)
	out->vals[i] += (in * fr->vals[i]);
    }
  else
    {
      mx = (mus_mixer *)f;
      chans = mx->chans;
      if (out)
	{
	  if (out->chans < chans) chans = out->chans;
	}
      else out = mus_make_empty_frame(chans);
      for (i=0;i<chans;i++)
	out->vals[i] += (in * mx->vals[0][i]);
    }
  return(out);
}

Float mus_frame2sample(mus_any *f, mus_frame *in)
{
  int i,chans;
  Float val = 0.0;
  mus_mixer *mx;
  mus_frame *fr;
  if (mus_frame_p(f))
    {
      fr = (mus_frame *)f;
      chans = in->chans;
      if (fr->chans < chans) chans = fr->chans;
      for (i=0;i<chans;i++)
	val += (in->vals[i] * fr->vals[i]); 
    }
  else
    {
      mx = (mus_mixer *)f;
      chans = in->chans;
      if (mx->chans < chans) chans = mx->chans;
      for (i=0;i<chans;i++)
	val += (in->vals[i] * mx->vals[i][0]);
    }
  return(val);
}

mus_mixer *mus_mixer_multiply(mus_mixer *f1, mus_mixer *f2, mus_mixer *res)
{
  int i,j,k,chans;
  chans = f1->chans;
  if (f2->chans < chans) chans = f2->chans;
  if (res)
    {
      if (res->chans < chans) chans = res->chans;
    }
  else res = mus_make_empty_mixer(chans);
  for (i=0;i<chans;i++)
    for (j=0;j<chans;j++)
      {
	res->vals[i][j] = 0.0;
	for (k=0;k<chans;k++) 
	  res->vals[i][j] += (f1->vals[i][k] * f2->vals[k][j]);
      }
  return(res);
}



/* ---------------- buffer ---------------- */

/* is it worthwhile to extend this to be a queue of anything (i.e. mus_any **buffer)? */

typedef struct {
  mus_any_class *core;
  Float *buf;
  int size;
  int loc;
  Float fill_time;
  int empty;
  int buf_allocated;
} rblk;

static char *inspect_rblk(void *ptr)
{
  rblk *gen = (rblk *)ptr;
  char *desc;
  desc = make_big_desc_buf(ptr,FALSE);
  if (desc) sprintf(desc,"rblk buf[%d (%s)]: %s, loc: %d, fill_time: %f, empty: %d",
		    gen->size,(gen->buf_allocated) ? "local" : "external",
		    print_array(gen->buf,gen->size,0),
		    gen->loc,gen->fill_time,gen->empty);
  return(desc);
}

static int mus_free_buffer(void *gen) 
{
  rblk *ptr = (rblk *)gen;
  if (ptr)
    {
      if ((ptr->buf) && (ptr->buf_allocated)) FREE(ptr->buf);
      FREE(ptr);
    }
  return(0);
}

static Float *rblk_data(void *ptr) {return(((rblk *)ptr)->buf);}
static int rblk_length(void *ptr) {return(((rblk *)ptr)->size);}
static int rblk_set_length(void *ptr, int new_size) {((rblk *)ptr)->size = new_size; return(new_size);}
static int rblk_equalp(void *p1, void *p2) {return(p1 == p2);}

static Float *rblk_set_data(void *ptr, Float *new_data) 
{
  rblk *rb = (rblk *)ptr;
  if (rb->buf_allocated) {FREE(rb->buf); rb->buf_allocated = 0;}
  rb->buf = new_data; 
  return(new_data);
}

static char *describe_buffer(void *ptr)
{
  char *desc;
  rblk *gen = (rblk *)ptr;
  desc = make_big_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_buffer_p((mus_any *)gen))
	sprintf(desc,"buffer: length: %d, loc: %d, fill: %.3f",
		gen->size,gen->loc,gen->fill_time);
      else describe_bad_gen(ptr,desc,"buffer","a");
    }
  return(desc);
}

Float mus_buffer2sample(mus_any *ptr)
{
  rblk *gen = (rblk *)ptr;
  int loc,i,j;
  Float val;
  loc = gen->loc;
  if (loc < gen->size) 
    val = gen->buf[loc];
  else val = 0.0;
  loc++;
  if ((gen->empty == FALSE) && ((Float)loc >= gen->fill_time))
    {
      i=0;
      for (j=loc;j<gen->size;i++,j++) gen->buf[i]=gen->buf[j];
      for (;i<gen->size;i++) gen->buf[i] = 0.0;
      gen->fill_time -= (Float)loc;
      gen->loc = 0;
      gen->empty = TRUE; 
    }
  else gen->loc = loc;
  return(val);
}

Float mus_sample2buffer(mus_any *ptr, Float val)
{
  /* place val at fill_time and increment */
  int i,j,loc;
#ifdef MACOS
  Float *tmp;
#endif
  rblk *gen = (rblk *)ptr;
  if (gen->fill_time >= gen->size)
    {
      if (gen->loc == 0)
	{
	  /* have to make more space */
	  gen->size += 256;
#ifdef MACOS
	  /* gotta do realloc by hand */
	  tmp = (Float *)CALLOC(gen->size,sizeof(Float));
	  for (i=0;i<gen->size-256;i++) tmp[i] = gen->buf[i];
	  FREE(gen->buf);
	  gen->buf = tmp;
#else
	  gen->buf = (Float *)REALLOC(gen->buf,gen->size * sizeof(Float));
#endif
	}
      else
	{
	  i=0;
	  loc = gen->loc;
	  for (j=loc;j<gen->size;i++,j++) gen->buf[i]=gen->buf[j];
	  for (;i<gen->size;i++) gen->buf[i] = 0.0;
	  gen->fill_time -= (Float)loc;
	  gen->loc = 0;
	}
    }
  gen->buf[(int)(gen->fill_time)] = val;
  gen->fill_time += 1.0;
  return(val);
}

static mus_any_class BUFFER_CLASS = {
  MUS_BUFFER,
  "buffer",
  &mus_free_buffer,
  &describe_buffer,
  &inspect_rblk,
  &rblk_equalp,
  &rblk_data,
  &rblk_set_data,
  &rblk_length,
  &rblk_set_length,
  0,0,0,0,
  0,0
};

mus_any *mus_make_buffer(Float *preloaded_buffer, int size, Float current_fill_time)
{
  rblk *gen;
  gen = (rblk *)CALLOC(1,sizeof(rblk));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_open_buffer2sample!");
  else
    {
      gen->core = &BUFFER_CLASS;
      if (size <= 0) size = 512;
      gen->size = size;
      gen->loc = 0;
      gen->fill_time = current_fill_time;
      if (preloaded_buffer)
	{
	  gen->buf = preloaded_buffer;
	  gen->buf_allocated = 0;
	}
      else
	{
	  gen->buf = (Float *)CALLOC(size,sizeof(Float));
	  if (gen->buf == NULL) 
	    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate %d bytes for buffer2sample buffer",(int)(size * sizeof(Float)));
	  else 
	    gen->buf_allocated = 1;
	}
      if (current_fill_time == 0) gen->empty = TRUE; else gen->empty = FALSE;
      return((mus_any *)gen);
    }
  return(NULL);
}

int mus_buffer_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_BUFFER));}

int mus_buffer_empty_p(mus_any *ptr) {return(((rblk *)ptr)->empty);}

int mus_buffer_full_p(mus_any *ptr)
{
  rblk *gen = (rblk *)ptr;
  return((gen->fill_time >= gen->size) && (gen->loc == 0));
}

mus_any *mus_frame2buffer(mus_any *rb, mus_any *fr)
{
  mus_frame *frm = (mus_frame *)fr;
  int i;
  for (i=0;i<frm->chans;i++) mus_sample2buffer(rb,frm->vals[i]);
  return(fr);
}

mus_any *mus_buffer2frame(mus_any *rb, mus_any *fr)
{
  mus_frame *frm = (mus_frame *)fr;
  int i;
  for (i=0;i<frm->chans;i++) frm->vals[i] = mus_buffer2sample(rb);
  return(fr);
}



/* ---------------- wave-train ---------------- */

typedef struct {
  mus_any_class *core;
  Float freq;
  Float phase;
  Float *wave;
  int wsize;
  rblk *b;
  int wave_allocated;
} wt;

static char *inspect_wt(void *ptr)
{
  wt *gen = (wt *)ptr;
  char *desc,*rdesc;
  desc = make_big_desc_buf(ptr,FALSE);
  if (desc) 
    {
      sprintf(desc,"wt freq: %f, phase: %f, wave[%d (%s)]: %s, b:",
	      gen->freq,gen->phase,gen->wsize,(gen->wave_allocated) ? "local" : "external",
	      print_array(gen->wave,gen->wsize,0));
      rdesc = inspect_rblk((void *)(gen->b));
      strcat(desc,rdesc);
      FREE(rdesc);
    }
  return(desc);
}

static Float wt_freq(void *ptr) {return(((wt *)ptr)->freq);}
static Float set_wt_freq(void *ptr, Float val) {((wt *)ptr)->freq = val; return(val);}
static Float wt_phase(void *ptr) {return(fmod(((TWO_PI * ((wt *)ptr)->phase) / ((Float)((wt *)ptr)->wsize)),TWO_PI));}
static Float set_wt_phase(void *ptr, Float val) {((wt *)ptr)->phase = (val * ((wt *)ptr)->phase) / TWO_PI; return(val);}
static int wt_length(void *ptr) {return(((wt *)ptr)->wsize);}
static int wt_set_length(void *ptr, int val) {((wt *)ptr)->wsize = val; return(val);}
static Float *wt_data(void *ptr) {return(((wt *)ptr)->wave);}
static int wt_equalp(void *p1, void *p2) {return(p1 == p2);}

static Float *wt_set_data(void *ptr, Float *data) 
{
  wt *gen = (wt *)ptr;
  if (gen->wave_allocated) {FREE(gen->wave); gen->wave_allocated = 0;}
  gen->wave = data; 
  return(data);
}

static char *describe_wt(void *ptr)
{
  char *desc;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_wave_train_p((mus_any *)ptr))
	sprintf(desc,"wave_train freq: %.3fHz, phase: %.3f, size: %d",
		wt_freq(ptr),wt_phase(ptr),wt_length(ptr));
      else describe_bad_gen(ptr,desc,"wave_train","a");
    }
  return(desc);
}

Float mus_wave_train(mus_any *ptr, Float fm) 
{
  wt *gen = (wt *)ptr;
  rblk *b;
  int i;
  b = gen->b;
  if (b->empty)
    {
      for (i=0;i<b->size;i++) b->buf[i] += mus_array_interp(gen->wave,gen->phase + i,gen->wsize);
      b->fill_time += ((Float)sampling_rate/(gen->freq + (fm * sampling_rate / TWO_PI)));
      b->empty = FALSE;
    }
  return(mus_buffer2sample((mus_any *)(gen->b)));
}

static int free_wt(void *p) 
{
  wt *ptr = (wt *)p;
  if (ptr) 
    {
      if ((ptr->wave) && (ptr->wave_allocated)) FREE(ptr->wave);
      if (ptr->b) mus_free_buffer((void *)(ptr->b));
      FREE(ptr);
    }
  return(0);
}

static mus_any_class WAVE_TRAIN_CLASS = {
  MUS_WAVE_TRAIN,
  "wave_train",
  &free_wt,
  &describe_wt,
  &inspect_wt,
  &wt_equalp,
  &wt_data,
  &wt_set_data,
  &wt_length,
  &wt_set_length,
  &wt_freq,
  &set_wt_freq,
  &wt_phase,
  &set_wt_phase,
  0,0
};

mus_any *mus_make_wave_train(Float freq, Float phase, Float *wave, int wsize)
{
 wt *gen;
 gen = (wt *)CALLOC(1,sizeof(wt));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_wave_train!");
  else
    {
      gen->core = &WAVE_TRAIN_CLASS;
      gen->freq = freq;
      gen->phase = (wsize * phase) / TWO_PI;
      gen->wave = wave;
      gen->wsize = wsize;
      gen->b = (rblk *)mus_make_buffer(NULL,wsize,0.0);
      return((mus_any *)gen);
    }
  return(NULL);
}

int mus_wave_train_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_WAVE_TRAIN));}



/* ---------------- input/output ---------------- */

static Float mus_read_sample(mus_input *fd, int frame, int chan) 
{
  if (fd)
    {
      if ((fd->base)->sample)
	return(((*(fd->base)->sample))((void *)fd,frame,chan));
      mus_error(MUS_NO_SAMPLE_INPUT,"can't find %s's sample input function",mus_name((mus_any *)fd));
    }
  else mus_error(MUS_NO_GEN,"null ptr passed to mus_read_sample");
  return(0.0);
}

static Float mus_write_sample(mus_output *fd, int frame, int chan, Float samp) 
{
  if (fd)
    {
      if ((fd->base)->sample)
	return(((*(fd->base)->sample))((void *)fd,frame,chan,samp));
      mus_error(MUS_NO_SAMPLE_OUTPUT,"can't find %s's sample output function",mus_name((mus_any *)fd));
    }
  else mus_error(MUS_NO_GEN,"null ptr passed to mus_write_sample");
  return(0.0);
}

int mus_input_p(void *gen) {return((gen) && ((((mus_input *)gen)->base)->type == MUS_INPUT));}
int mus_output_p(void *gen) {return((gen) && ((((mus_output *)gen)->base)->type == MUS_OUTPUT));}



/* ---------------- file->sample ---------------- */

typedef struct {
  mus_any_class *core;
  mus_input_class *base;
  int chan;
  int dir;
  int loc;
  char *file_name;
  int chans;
  MUS_SAMPLE_TYPE **ibufs;
  int data_start,data_end,file_end;
} rdin;

static char *inspect_rdin(void *ptr)
{
  rdin *gen = (rdin *)ptr;
  char *desc;
  desc = make_big_desc_buf(ptr,FALSE);
  if (desc) sprintf(desc,"rdin chan: %d, dir: %d, loc: %d, chans: %d, data_start: %d, data_end: %d, file_end: %d, file_name: %s",
		    gen->chan, gen->dir,gen->loc,gen->chans,gen->data_start,gen->data_end,gen->file_end,gen->file_name);
  return(desc);
}

static char *describe_file2sample(void *ptr)
{
  char *desc;
  rdin *gen = (rdin *)ptr;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_file2sample_p((mus_any *)ptr))
	sprintf(desc,"file2sample: %s",gen->file_name);
      else describe_bad_gen(ptr,desc,"file2sample","a");
    }
  return(desc);
}

static int file2sample_equalp(void *p1, void *p2) {return(p1 == p2);}

static int free_file2sample(void *p) 
{
  rdin *ptr = (rdin *)p;
  if (ptr) 
    {
      if ((ptr->base)->end) ((*(ptr->base)->end))(p);
      FREE(ptr->file_name);
      FREE(ptr);
    }
  return(0);
}

static mus_any_class FILE2SAMPLE_CLASS = {
  MUS_FILE2SAMPLE,
  "file2sample",
  &free_file2sample,
  &describe_file2sample,
  &inspect_rdin,
  &file2sample_equalp,
  0,0,0,0,
  0,0,0,0,
  0,0
};

/* if sndlib is loaded, we use it for the sample method,
 * otherwise, the caller has to provide this method
 *   Float sample(void *ptr, int samp, int chan)
 * where ptr -> a mus_input compatible struct (like rdin)
 * the caller can also set the end method to handle deallocation;
 * it is called in free_file2sample.  So, the intended non-sndlib use
 * is something like:
 *   ptr = mus_make_file2sample (args)
 *   ptr->base->end = deallocation_method
 *   ptr->base->sample = read_method -- or the other methods if they'll be used 
 *   ...
 *   mus_file2sample(ptr);
 */

#if HAVE_SNDLIB
static Float file_sample(void *ptr, int samp, int chan)
{
  /* check in-core buffer bounds,
   * if needed read new buffer (taking into account dir)
   * return Float at samp (frame) 
   */
  rdin *gen = (rdin *)ptr;
  int fd,newloc,i;
  if ((samp < 0) || (samp > gen->file_end)) return(0.0);
  if ((samp > gen->data_end) || (samp < gen->data_start))
    {
      /* read in first buffer start either at samp (dir>0) or samp-bufsize (dir<0) */
      if (gen->dir >= 0) newloc = samp; else newloc = (int)(samp - (mus_file_buffer_size * .75));
      /* The .75 in the backwards read is trying to avoid reading the full buffer on 
       * nearly every sample when we're oscillating around the
       * nominal buffer start/end (in src driven by and oscil for example)
       */
      if (newloc < 0) newloc = 0;
      gen->data_start = newloc;
      gen->data_end = newloc + mus_file_buffer_size - 1;
      fd = mus_sound_open_input(gen->file_name);
      if (fd == -1)
	mus_error(MUS_CANT_OPEN_FILE,"open(%s) -> %s",gen->file_name,strerror(errno));
      else
	{ 
	  if (gen->ibufs == NULL) 
	    {
	      gen->ibufs = (MUS_SAMPLE_TYPE **)CALLOC(gen->chans,sizeof(MUS_SAMPLE_TYPE *));
	      for (i=0;i<gen->chans;i++)
		gen->ibufs[i] = (MUS_SAMPLE_TYPE *)CALLOC(mus_file_buffer_size,sizeof(MUS_SAMPLE_TYPE));
	    }
	  mus_sound_seek_frame(fd,gen->data_start);
	  mus_file_read_chans(fd,0,mus_file_buffer_size-1,gen->chans,gen->ibufs,(MUS_SAMPLE_TYPE *)(gen->ibufs));
	  mus_sound_close_input(fd);
	  if (gen->data_end > gen->file_end) gen->data_end = gen->file_end;
	}
    }
  return((Float)MUS_SAMPLE_TO_FLOAT(gen->ibufs[chan][samp - gen->data_start]));
}

static int file2sample_end(void *ptr)
{
  rdin *gen = (rdin *)ptr;
  int i;
  if (gen)
    {
      if (gen->ibufs)
	{
	  for (i=0;i<gen->chans;i++)
	    if (gen->ibufs[i]) FREE(gen->ibufs[i]);
	  FREE(gen->ibufs);
	}
    }
  return(0);
}
#endif

static mus_input_class FILE2SAMPLE_INPUT_CLASS = {
  MUS_INPUT,
  0, /* file_sample */
  0, /* location */
  0, /* set_location */
  0, /* channel */
  0, /* begin */
  0  /* end */
};

int mus_file2sample_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_FILE2SAMPLE));}

mus_any *mus_make_file2sample(const char *filename)
{
  rdin *gen;
  gen = (rdin *)CALLOC(1,sizeof(rdin));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_file2sample!");
  else
    {
      if (filename == NULL)
	mus_error(MUS_NO_FILE_NAME_PROVIDED,"mus_make_file2sample requires a file name");
      else
	{
	  gen->core = &FILE2SAMPLE_CLASS;
	  gen->base = &FILE2SAMPLE_INPUT_CLASS;
	  gen->file_name = (char *)CALLOC(strlen(filename)+1,sizeof(char));
	  if (gen->file_name == NULL) 
	    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate file name space in mus_make_file2sample!");
	  else
	    strcpy(gen->file_name,filename);
#if HAVE_SNDLIB
	  (gen->base)->sample = &file_sample;
	  (gen->base)->end = &file2sample_end;
	  gen->data_end = -1; /* force initial read */
	  gen->chans = mus_sound_chans(gen->file_name);
	  gen->file_end = mus_sound_frames(gen->file_name);
#endif
	  return((mus_any *)gen);
	}
    }
  return(NULL);
}

Float mus_file2sample(mus_any *ptr, int samp, int chan)
{
  return(mus_read_sample((mus_input *)ptr,samp,chan));
}



/* ---------------- readin ---------------- */

/* readin reads only the desired channel and increments the location by the direction
 *   it inherits from and specializes the file2sample class 
 */

static char *describe_readin(void *ptr)
{
  char *desc;
  rdin *gen = (rdin *)ptr;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_readin_p((mus_any *)ptr))
	sprintf(desc,"readin: %s[chan %d], loc: %d, dir: %d",gen->file_name,gen->chan,gen->loc,gen->dir);
      else describe_bad_gen(ptr,desc,"readin","a");
    }
  return(desc);
}

static int readin_equalp(void *p1, void *p2) {return(p1 == p2);}

static int free_readin(void *p) 
{
  rdin *ptr = (rdin *)p;
  if (ptr) 
    {
      if ((ptr->base)->end) ((*(ptr->base)->end))(p);
      FREE(ptr->file_name);
      FREE(ptr);
    }
  return(0);
}

static mus_any_class READIN_CLASS = {
  MUS_READIN,
  "readin",
  &free_readin,
  &describe_readin,
  &inspect_rdin,
  &readin_equalp,
  0,0,0,0,
  0,0,0,0,
  0,0
};

int mus_readin_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_READIN));}

static int rd_location(void *rd) {return(((rdin *)rd)->loc);}
static int rd_set_location(void *rd, int loc) {((rdin *)rd)->loc = loc; return(loc);}
static int rd_channel(void *rd) {return(((rdin *)rd)->chan);}

static mus_input_class READIN_INPUT_CLASS = {
  MUS_INPUT,
  0, /* file_sample */
  &rd_location,
  &rd_set_location,
  &rd_channel,
  0, /* begin */
  0  /* end */
};

mus_any *mus_make_readin(const char *filename, int chan, int start, int direction)
{
  rdin *gen;
  gen = (rdin *)mus_make_file2sample(filename);
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_readin!");
  else
    {
      gen->core = &READIN_CLASS;
      gen->base = &READIN_INPUT_CLASS;
      gen->loc = start;
      gen->dir = direction;
      gen->chan = chan;
#if HAVE_SNDLIB
      gen->ibufs = (MUS_SAMPLE_TYPE **)CALLOC(gen->chans,sizeof(MUS_SAMPLE_TYPE *));
      gen->ibufs[chan] = (MUS_SAMPLE_TYPE *)CALLOC(mus_file_buffer_size,sizeof(MUS_SAMPLE_TYPE));
      /* i.e. read only the specified channel */
      (gen->base)->sample = &file_sample;
      (gen->base)->end = &file2sample_end;
#endif
      return((mus_any *)gen);
    }
  return(NULL);
}

Float mus_readin(mus_any *ptr)
{
  Float res;
  rdin *rd = (rdin *)ptr;
  res = mus_file2sample((mus_any *)rd,rd->loc,rd->chan);
  rd->loc += rd->dir;
  return(res);
}

int mus_location(mus_any *gen)
{
  mus_input *ingen;
  seg *envgen;
  if (gen)
    {
      if (mus_env_p(gen))
	{
	  envgen = (seg *)gen;
	  return(envgen->pass);
	}
      else
	{
	  if (mus_input_p(gen))
	    {
	      ingen = (mus_input *)gen;
	      if ((ingen->base)->location)
		return(((*(ingen->base)->location))((void *)gen));
	      else mus_error(MUS_NO_LOCATION,"can't get %s's location",mus_name(gen));
	    }
	  else mus_error(MUS_NO_LOCATION,"can't get %s's location",mus_name(gen));
	}
    }
  else mus_error(MUS_NO_GEN,"null gen passed to mus_location");
  return(0);
}

int mus_set_location(mus_any *gen, int loc)
{
  mus_input *ingen;
  if (gen)
    {
      if (mus_env_p(gen))
	{
	  set_env_location(gen,loc);
	  return(loc);
	}
      else
	{
	  if (mus_input_p(gen))
	    {
	      ingen = (mus_input *)gen;
	      if ((ingen->base)->set_location)
		{
		  (*((ingen->base)->set_location))((void *)gen,loc);
		  return(loc);
		}
	    }
	}
      mus_error(MUS_NO_LOCATION,"can't set %s's location",mus_name(gen));
    }
  else mus_error(MUS_NO_GEN,"null gen passed to mus_set_location");
  return(0);
}

int mus_channel(mus_input *gen)
{
  if (gen)
    {
      if ((gen->base)->channel)
	return(((*(gen->base)->channel))((void *)gen));
      else mus_error(MUS_NO_CHANNEL,"can't get %s's channel",mus_name((mus_any *)gen));
    }
  else mus_error(MUS_NO_GEN,"null gen passed to mus_channel");
  return(0);
}




/* ---------------- in-any ---------------- */

Float mus_in_any(int frame, int chan, mus_input *IO)
{
  if (IO) return(mus_file2sample((mus_any *)IO,frame,chan));
  return(0.0);
}

Float mus_ina(int frame, mus_input *inp) {return(mus_in_any(frame,0,inp));}
Float mus_inb(int frame, mus_input *inp) {return(mus_in_any(frame,1,inp));}



/* ---------------- file->frame ---------------- */

/* also built on file->sample */

static char *describe_file2frame(void *ptr)
{
  char *desc;
  rdin *gen = (rdin *)ptr;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_file2frame_p((mus_any *)ptr))
	sprintf(desc,"file2frame: %s",gen->file_name);
      else describe_bad_gen(ptr,desc,"file2frame","a");
    }
  return(desc);
}

static mus_any_class FILE2FRAME_CLASS = {
  MUS_FILE2FRAME,
  "file2frame",
  &free_file2sample,
  &describe_file2frame,
  &inspect_rdin,
  &file2sample_equalp,
  0,0,0,0,
  0,0,0,0,
  0,0
};

mus_any *mus_make_file2frame(const char *filename)
{
  rdin *gen;
  gen = (rdin *)mus_make_file2sample(filename);
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_file2frame!");
  else
    {
      gen->core = &FILE2FRAME_CLASS;
      return((mus_any *)gen);
    }
  return(NULL);
}

int mus_file2frame_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_FILE2FRAME));}

mus_frame *mus_file2frame(mus_any *ptr, int samp, mus_frame *uf)
{
  mus_frame *f;
  rdin *gen = (rdin *)ptr;
  int i;
  if (uf == NULL) f = mus_make_empty_frame(gen->chans); else f = uf;
  for (i=0;i<gen->chans;i++) f->vals[i] = mus_file2sample((mus_any *)gen,samp,i);
  return(f);
}


/* ---------------- sample->file ---------------- */

/* in all output functions, the assumption is that we're adding to whatever already exists */
/* also, the "end" methods need to flush the output buffer */

typedef struct {
  mus_any_class *core;
  mus_output_class *base;
  int chan;
  int loc;
  char *file_name;
  int chans;
  MUS_SAMPLE_TYPE **obufs;
  int data_start,data_end;
  int out_end;
  int output_data_format;
  int output_header_type;
} rdout;

static char *inspect_rdout(void *ptr)
{
  rdout *gen = (rdout *)ptr;
  char *desc;
  desc = make_big_desc_buf(ptr,FALSE);
  if (desc) sprintf(desc,"rdout chan: %d, loc: %d, file_name: %s, chans: %d, data_start: %d, data_end: %d, out_end: %d",
		    gen->chan,gen->loc,gen->file_name,gen->chans,gen->data_start,gen->data_end,gen->out_end);
  return(desc);
}

static char *describe_sample2file(void *ptr)
{
  char *desc;
  rdout *gen = (rdout *)ptr;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_sample2file_p((mus_any *)ptr))
	sprintf(desc,"sample2file: %s",gen->file_name);
      else describe_bad_gen(ptr,desc,"sample2file","a");
    }
  return(desc);
}

static int sample2file_equalp(void *p1, void *p2) {return(p1 == p2);}

static int free_sample2file(void *p) 
{
  rdout *ptr = (rdout *)p;
  if (ptr) 
    {
      if ((ptr->base)->end) ((*(ptr->base)->end))(p);
      FREE(ptr->file_name);
      FREE(ptr);
    }
  return(0);
}

static mus_any_class SAMPLE2FILE_CLASS = {
  MUS_SAMPLE2FILE,
  "sample2file",
  &free_sample2file,
  &describe_sample2file,
  &inspect_rdout,
  &sample2file_equalp,
  0,0,0,0,
  0,0,0,0,
  0,0
};

#if HAVE_SNDLIB
static int bufferlen(void *ptr) {return(mus_file_buffer_size);}
static int set_bufferlen(void *ptr, int len) {mus_file_buffer_size = len; return(len);}

static void flush_buffers(rdout *gen)
{
  int fd,i,j,last,size,hdrend,hdrfrm,hdrtyp;
  MUS_SAMPLE_TYPE **addbufs;
  fd = mus_sound_open_input(gen->file_name);
  if (fd == -1)
    {
      fd = mus_sound_open_output(gen->file_name,(int)sampling_rate,gen->chans,gen->output_data_format,gen->output_header_type,NULL);
      if (fd == -1)
	mus_error(MUS_CANT_OPEN_FILE,"open(%s) -> %s",gen->file_name,strerror(errno));
      else
	{
	  mus_sound_write(fd,0,gen->out_end,gen->chans,gen->obufs);
	  mus_sound_close_output(fd,(gen->out_end+1) * gen->chans * mus_data_format_to_bytes_per_sample(mus_sound_data_format(gen->file_name)));	  
	}
    }
  else
    {
      hdrend = mus_sound_data_location(gen->file_name);
      hdrfrm = mus_sound_data_format(gen->file_name);
      hdrtyp = mus_sound_header_type(gen->file_name);
      size = mus_sound_frames(gen->file_name);
      addbufs = (MUS_SAMPLE_TYPE **)CALLOC(gen->chans,sizeof(MUS_SAMPLE_TYPE *));
      for (i=0;i<gen->chans;i++) addbufs[i] = (MUS_SAMPLE_TYPE *)CALLOC(mus_file_buffer_size, sizeof(MUS_SAMPLE_TYPE));
      mus_sound_seek_frame(fd,gen->data_start);
      mus_sound_read(fd,0,gen->out_end - gen->data_start + 1,gen->chans,addbufs);
      mus_sound_close_input(fd);
      fd = mus_sound_reopen_output(gen->file_name,gen->chans,hdrfrm,hdrtyp,hdrend);
      last = gen->out_end - gen->data_start;
      for (j=0;j<gen->chans;j++)
	for (i=0;i<=last;i++)
	  addbufs[j][i] += gen->obufs[j][i];
      mus_sound_seek_frame(fd,gen->data_start);
      mus_sound_write(fd,0,last,gen->chans,addbufs);
      if (size <= gen->out_end) size = gen->out_end+1;
      mus_sound_close_output(fd,size * gen->chans * mus_data_format_to_bytes_per_sample(hdrfrm));
      for (i=0;i<gen->chans;i++) FREE(addbufs[i]);
      FREE(addbufs);
    }
}

static Float sample_file(void *ptr, int samp, int chan, Float val)
{
  rdout *gen = (rdout *)ptr;
  int i,j;
  if (chan < gen->chans)
    {
      if ((samp > gen->data_end) || (samp < gen->data_start))
	{
	  flush_buffers(gen);
	  for (j=0;j<gen->chans;j++)
	    for (i=0;i<mus_file_buffer_size;i++)
	      gen->obufs[j][i] = MUS_SAMPLE_0;
	  gen->data_start = samp;
	  gen->data_end = samp + mus_file_buffer_size - 1;
	  gen->out_end = samp;
	}
      gen->obufs[chan][samp - gen->data_start] += MUS_FLOAT_TO_SAMPLE(val);
      if (samp > gen->out_end) gen->out_end = samp;
    }
  return(val);
}

static int sample2file_end(void *ptr)
{
  rdout *gen = (rdout *)ptr;
  int i;
  if (gen)
    {
      if (gen->obufs)
	{
	  flush_buffers(gen);
	  for (i=0;i<gen->chans;i++)
	    if (gen->obufs[i]) FREE(gen->obufs[i]);
	  FREE(gen->obufs);
	}
    }
  return(0);
}
#endif

static mus_output_class SAMPLE2FILE_OUTPUT_CLASS = {
  MUS_OUTPUT,
  0, /* sample_file */
  0, /* begin */
  0  /* end */
};

int mus_sample2file_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_SAMPLE2FILE));}

mus_any *mus_make_sample2file(const char *filename, int out_chans, int out_format, int out_type)
{
  rdout *gen;
#if HAVE_SNDLIB
  int i,fd;
#endif
  gen = (rdout *)CALLOC(1,sizeof(rdout));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_sample2file!");
  else
    {
      if (filename == NULL)
	mus_error(MUS_NO_FILE_NAME_PROVIDED,"mus_make_sample2file requires a file name");
      else
	{
	  gen->core = &SAMPLE2FILE_CLASS;
	  gen->base = &SAMPLE2FILE_OUTPUT_CLASS;
	  gen->file_name = (char *)CALLOC(strlen(filename)+1,sizeof(char));
	  if (gen->file_name == NULL) 
	    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate file name space in mus_make_sample2file!");
	  else
	    strcpy(gen->file_name,filename);
#if HAVE_SNDLIB
	  (gen->base)->sample = &sample_file;
	  (gen->base)->end = &sample2file_end;
	  (gen->core)->length = &bufferlen;
	  (gen->core)->set_length = &set_bufferlen;
	  gen->data_start = 0;
	  gen->data_end = mus_file_buffer_size - 1;
	  gen->out_end = 0;
	  gen->chans = out_chans;
	  gen->output_data_format = out_format;
	  gen->output_header_type = out_type;
	  gen->obufs = (MUS_SAMPLE_TYPE **)CALLOC(gen->chans,sizeof(MUS_SAMPLE_TYPE *));
	  for (i=0;i<gen->chans;i++) gen->obufs[i] = (MUS_SAMPLE_TYPE *)CALLOC(mus_file_buffer_size,sizeof(MUS_SAMPLE_TYPE));
	  fd = mus_sound_open_output(gen->file_name,(int)sampling_rate,gen->chans,gen->output_data_format,gen->output_header_type,NULL);
	  /* clear previous, if any */
	  if (fd == -1)
	    mus_error(MUS_CANT_OPEN_FILE,"open(%s) -> %s",gen->file_name,strerror(errno));
	  else close(fd);
#endif
	  return((mus_any *)gen);
	}
    }
  return(NULL);
}

Float mus_sample2file(mus_any *ptr, int samp, int chan, Float val)
{
  return(mus_write_sample((mus_output *)ptr,samp,chan,val));
}

int mus_close_file(mus_any *ptr)
{
  rdout *gen = (rdout *)ptr;
  if ((gen) && (gen->obufs)) flush_buffers(gen);
  return(0);
}


/* ---------------- out-any ---------------- */

Float mus_out_any(int frame, Float val, int chan, mus_output *IO)
{
  if (IO) return(mus_sample2file((mus_any *)IO,frame,chan,val));
  return(0.0);
}

Float mus_outa(int frame, Float val, mus_output *IO) {return(mus_out_any(frame,val,0,IO));}
Float mus_outb(int frame, Float val, mus_output *IO) {return(mus_out_any(frame,val,1,IO));}
Float mus_outc(int frame, Float val, mus_output *IO) {return(mus_out_any(frame,val,2,IO));}
Float mus_outd(int frame, Float val, mus_output *IO) {return(mus_out_any(frame,val,3,IO));}



/* ---------------- frame->file ---------------- */

static char *describe_frame2file(void *ptr)
{
  char *desc;
  rdout *gen = (rdout *)ptr;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_frame2file_p((mus_any *)ptr))
	sprintf(desc,"frame2file: %s",gen->file_name);
      else describe_bad_gen(ptr,desc,"frame2file","a");
    }
  return(desc);
}

static mus_any_class FRAME2FILE_CLASS = {
  MUS_FRAME2FILE,
  "frame2file",
  &free_sample2file,
  &describe_frame2file,
  &inspect_rdout,
  &sample2file_equalp,
  0,0,0,0,
  0,0,0,0,
  0,0
};

mus_any *mus_make_frame2file(const char *filename, int chans, int out_format, int out_type)
{
  rdout *gen;
  gen = (rdout *)mus_make_sample2file(filename, chans, out_format, out_type);
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_frame2file!");
  else
    {
      gen->core = &FRAME2FILE_CLASS;
      return((mus_any *)gen);
    }
  return(NULL);
}

int mus_frame2file_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_FRAME2FILE));}

mus_frame *mus_frame2file(mus_any *ptr, int samp, mus_frame *data)
{
  rdout *gen = (rdout *)ptr;
  int i,chans;
  chans = data->chans;
  if (gen->chans < chans) chans = gen->chans;
  for (i=0;i<chans;i++) mus_sample2file(ptr,samp,i,data->vals[i]);
  return(data);
}



/* ---------------- locsig ---------------- */

/* always return a frame */

typedef struct {
  mus_any_class *core;
  mus_output *outn_writer;
  mus_output *revn_writer;
  mus_frame *outf,*revf;
  Float *outn;
  Float *revn;
  int chans,rev_chans;
} locs;

static char *inspect_locs(void *ptr)
{
  locs *gen = (locs *)ptr;
  char *desc;
  desc = make_desc_buf(ptr,FALSE);
  if (desc) sprintf(desc,"locs outn[%d]: %s, revn[%d]: %s",
		    gen->chans,print_array(gen->outn,gen->chans,0),
		    gen->rev_chans,print_array(gen->revn,gen->rev_chans,0));
  return(desc);
}

static int locsig_equalp(void *p1, void *p2) 
{
  locs *g1 = (locs *)p1;
  locs *g2 = (locs *)p2;
  int i;
  if (p1 != p2) return(FALSE);
  if ((g1->core)->type != (g2->core)->type) return(FALSE);
  if (g1->chans != g2->chans) return(FALSE);
  for (i=0;i<g1->chans;i++) 
    if (g1->outn[i] != g2->outn[i]) return(FALSE);
  if (g1->revn)
    {
      if (!(g2->revn)) return(FALSE);
      if (g1->revn[0] != g2->revn[0]) return(FALSE);
    }
  else if (g2->revn) return(FALSE);
  return(TRUE);
}

static char *describe_locsig(void *ptr)
{
  char *desc,*str;
  int i;
  locs *gen = (locs *)ptr;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_locsig_p((mus_any *)ptr))
	{
	  sprintf(desc,"locsig: chans %d, outn: [",gen->chans);
	  str = (char *)CALLOC(32,sizeof(char));
	  for (i=0;i<gen->chans-1;i++)
	    {
	      sprintf(str,"%.3f ",gen->outn[i]);
	      strcat(desc,str);
	    }
	  sprintf(str,"%.3f]",gen->outn[gen->chans-1]);
	  strcat(desc,str);
	  FREE(str);
	}
      else describe_bad_gen(ptr,desc,"locsig","a");
    }
  return(desc);
}

static int free_locsig(void *p) 
{
  locs *ptr = (locs *)p;
  if (ptr) 
    {
      if (ptr->outn) FREE(ptr->outn);
      if (ptr->revn) FREE(ptr->revn);
      mus_free((mus_any *)(ptr->outf));
      FREE(ptr);
    }
  return(0);
}

static int locsig_length(void *ptr) {return(((locs *)ptr)->chans);}

Float mus_locsig_ref (mus_any *ptr, int chan) 
{
  locs *gen = (locs *)ptr;
  if ((ptr) && (mus_locsig_p(ptr))) 
    {
      if ((chan >= 0) && (chan < gen->chans))
	return(gen->outn[chan]);
      else mus_error(MUS_NO_SUCH_CHANNEL,"locsig_ref chan %d >= %d",chan,gen->chans);
    }
  return(0.0);
}

Float mus_locsig_set (mus_any *ptr, int chan, Float val) 
{
  locs *gen = (locs *)ptr;
  if ((ptr) && (mus_locsig_p(ptr))) 
    {
      if ((chan >= 0) && (chan < gen->chans))
	gen->outn[chan] = val;
      else mus_error(MUS_NO_SUCH_CHANNEL,"locsig_set chan %d >= %d",chan,gen->chans);
    }
  return(val);
}

Float mus_locsig_reverb_ref (mus_any *ptr, int chan) 
{
  locs *gen = (locs *)ptr;
  if ((ptr) && (mus_locsig_p(ptr))) 
    {
      if ((chan >= 0) && (chan < gen->rev_chans))
	return(gen->revn[chan]);
      else mus_error(MUS_NO_SUCH_CHANNEL,"locsig_reverb_ref chan %d >= %d",chan,gen->rev_chans);
    }
  return(0.0);
}

Float mus_locsig_reverb_set (mus_any *ptr, int chan, Float val) 
{
  locs *gen = (locs *)ptr;
  if ((ptr) && (mus_locsig_p(ptr))) 
    {
      if ((chan >= 0) && (chan < gen->rev_chans))
	gen->revn[chan] = val;
      else mus_error(MUS_NO_SUCH_CHANNEL,"locsig_reverb_set chan %d >= %d",chan,gen->rev_chans);
    }
  return(val);
}

static mus_any_class LOCSIG_CLASS = {
  MUS_LOCSIG,
  "locsig",
  &free_locsig,
  &describe_locsig,
  &inspect_locs,
  &locsig_equalp,
  0,0,
  &locsig_length,
  0,
  0,0,0,0,
  0,0
};

int mus_locsig_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_LOCSIG));}

mus_any *mus_make_locsig(Float degree, Float distance, Float reverb, int chans, mus_output *output, mus_output *revput)
{
  locs *gen;
  Float dist,frac;
  int i;
  gen = (locs *)CALLOC(1,sizeof(locs));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_locsig!");
  else
    {
      gen->core = &LOCSIG_CLASS;
      gen->outf = mus_make_empty_frame(chans);
      gen->chans = chans;
      if (distance > 1.0)
	dist = 1.0 / distance;
      else dist = 1.0;
      if (output) gen->outn_writer = output;
      if (revput) 
	{
	  gen->revn_writer = revput;
	  gen->rev_chans = mus_channels((mus_any *)revput);
	  if (gen->rev_chans > 0)
	    {
	      gen->revn = (Float *)CALLOC(gen->rev_chans,sizeof(Float));
	      for (i=0;i<gen->rev_chans;i++) gen->revn[i] = reverb / sqrt(dist);
	      gen->revf = mus_make_empty_frame(gen->rev_chans);
	    }
	}
      else gen->rev_chans = 0;
      gen->outn = (Float *)CALLOC(chans,sizeof(Float));
      if (gen->outn == NULL) 
	mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate outn array for mus_make_locsig!");
      if (chans == 1)
	gen->outn[0] = dist;
      else
	{
	  if (chans == 2)
	    {
	      if (degree < 0.0) degree = 0.0; else if (degree > 90.0) degree = 90.0;
	      frac = degree / 90.0;
	      gen->outn[0] = dist * (1.0 - frac);
	      gen->outn[1] = dist * frac;
	    }
	  else
	    {
	      if ((degree >= 0.0) && (degree < 90.0))
		{
		  gen->outn[0] = dist * (90.0 - degree) / 90.0;
		  gen->outn[1] = dist * degree / 90.0;
		}
	      else
		{
		  if ((degree >= 90.0) && (degree < 180.0))
		    {
		      gen->outn[1] = dist * (180.0 - degree) / 90.0;
		      gen->outn[2] = dist * (degree - 90.0) / 90.0;
		    }
		  else
		    {
		      if ((degree >= 180.0) && (degree < 270.0))
			{
			  gen->outn[2] = dist * (270.0 - degree) / 90.0;
			  if (gen->chans > 3) gen->outn[3] = dist * (degree - 180.0) / 90.0;
			}
		      else
			{
			  if (gen->chans > 3) gen->outn[3] = dist * (360.0 - degree) / 90.0;
			  gen->outn[0] = dist * (degree - 270.0) / 90.0;
			}
		    }
		}
	    }
	}
      return((mus_any *)gen);
    }
  return(NULL);
}

mus_frame *mus_locsig(mus_any *ptr, int loc, Float val)
{
  locs *gen = (locs *)ptr;
  int i;
  for (i=0;i<gen->chans;i++)
    {
      (gen->outf)->vals[i] = val * gen->outn[i];
    }
  for (i=0;i<gen->rev_chans;i++)
    {
      (gen->revf)->vals[i] = val * gen->revn[i];
    }
  if (gen->revn_writer)
    mus_frame2file((mus_any *)(gen->revn_writer),loc,gen->revf);
  if (gen->outn_writer)
    return(mus_frame2file((mus_any *)(gen->outn_writer),loc,gen->outf));
  else return((mus_frame *)(gen->outf));
}

int mus_channels(mus_any *ptr)
{
  if ((mus_readin_p(ptr)) || (mus_file2sample_p(ptr)) || (mus_file2frame_p(ptr))) return(((rdin *)ptr)->chans);
  if (mus_locsig_p(ptr)) return(((locs *)ptr)->chans);
  if ((mus_sample2file_p(ptr)) || (mus_frame2file_p(ptr))) return(((rdout *)ptr)->chans);
  if (mus_frame_p(ptr)) return(((mus_frame *)ptr)->chans);
  if (mus_mixer_p(ptr)) return(((mus_mixer *)ptr)->chans);
  return(0);
}



/* ---------------- src ---------------- */

/* taken largely from snd-chn.c */

typedef struct {
  mus_any_class *core;
  Float (*feeder)(void *arg, int direction);
  Float x;
  Float incr;
  int width;
  int len;
  Float *data,*sinc_table;
  void *environ;
} sr;

static char *inspect_sr(void *ptr)
{
  sr *gen = (sr *)ptr;
  char *desc;
  desc = make_big_desc_buf(ptr,FALSE);
  if (desc) sprintf(desc,"sr x: %f, incr: %f, width: %d, len: %d, data[%d]: %s",
		    gen->x,gen->incr,gen->width,gen->len,gen->width * 2 + 1,
		    print_array(gen->data,gen->width * 2 + 1,0));
  return(desc);
}

#define SRC_SINC_DENSITY 1000
#define SRC_SINC_WIDTH 10

static Float **sinc_tables = NULL;
static int *sinc_widths = NULL;
static int sincs = 0;

static Float *init_sinc_table(int width)
{
#ifdef MACOS
  Float **ftmp;
  int *itmp;
#endif
  int i,size,loc;
  Float sinc_freq,win_freq,sinc_phase,win_phase;
  for (i=0;i<sincs;i++)
    if (sinc_widths[i] == width)
      return(sinc_tables[i]);
  if (sincs == 0)
    {
      sinc_tables = (Float **)CALLOC(8,sizeof(Float *));
      sinc_widths = (int *)CALLOC(8,sizeof(int));
      sincs = 8;
      loc = 0;
    }
  else
    {
      loc = -1;
      for (i=0;i<sincs;i++)
	if (sinc_widths[i] == 0)
	  {
	    loc = i;
	    break;
	  }
      if (loc == -1)
	{
#ifdef MACOS
	  ftmp = sinc_tables;
	  itmp = sinc_widths;
	  sinc_tables = (Float **)CALLOC((sincs+8),sizeof(Float *));
	  sinc_widths = (int *)CALLOC((sincs+8),sizeof(int));
	  for (i=0;i<sincs;i++)
	    {
	      sinc_tables[i] = ftmp[i];
	      sinc_widths[i] = itmp[i];
	    }
	  FREE(ftmp);
	  FREE(itmp);
#else
	  sinc_tables = (Float **)REALLOC(sinc_tables,(sincs+8) * sizeof(Float *));
	  sinc_widths = (int *)REALLOC(sinc_widths,(sincs+8) * sizeof(int));
#endif
	  for (i=sincs;i<(sincs+8);i++) {sinc_widths[i]=0; sinc_tables[i]=NULL;}
	  loc = sincs;
	  sincs+=8;
	}
    }
  sinc_tables[loc] = (Float *)CALLOC(width * SRC_SINC_DENSITY + 1,sizeof(Float));
  sinc_widths[loc] = width;
  size = width * SRC_SINC_DENSITY;
  sinc_freq = M_PI / (Float)SRC_SINC_DENSITY;
  win_freq = M_PI / (Float)size;
  sinc_tables[loc][0] = 1.0;
  for (i=1,sinc_phase=sinc_freq,win_phase=win_freq;i<size;i++,sinc_phase+=sinc_freq,win_phase+=win_freq)
    sinc_tables[loc][i] = sin(sinc_phase) * (0.5+0.5*cos(win_phase)) / sinc_phase;
  return(sinc_tables[loc]);
}

int mus_src_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_SRC));}

static int free_src(void *srptr)
{
  sr *srp = (sr *)srptr;
  if (srp)
    {
      if (srp->data) FREE(srp->data);
      FREE(srp);
    }
  return(0);
}

static int src_equalp(void *p1, void *p2) {return(p1 == p2);}

static char *describe_src(void *ptr)
{
  char *desc;
  sr *gen = (sr *)ptr;
  desc = make_big_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_src_p((mus_any *)ptr))
	{
	  sprintf(desc,"src: width: %d, x: %.3f, incr: %.3f, len: %d",
		  gen->width,gen->x,gen->incr,gen->len);
	}
      else describe_bad_gen(ptr,desc,"src","an");
    }
  return(desc);
}

static int src_length(void *ptr) {return(((sr *)ptr)->width);}

static mus_any_class SRC_CLASS = {
  MUS_SRC,
  "src",
  &free_src,
  &describe_src,
  &inspect_sr,
  &src_equalp,
  0,0,
  &src_length,  /* sinc width actually */
  0,
  0,0,0,0,
  0,0
};

mus_any *mus_make_src(Float (*input)(void *arg, int direction), Float srate, int width, void *environ)
{
  sr *srp;
  int i,lim,wid;
  srp = (sr *)CALLOC(1,sizeof(sr));
  if (srp == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_src!");
  else
    {
      if (width == 0) width = SRC_SINC_WIDTH;
      if (width < (srate * 2)) wid = (int)(ceil(srate) * 2); else wid = width;
      srp->core = &SRC_CLASS;
      srp->x = 0.0;
      srp->feeder = input;
      srp->environ = environ;
      srp->incr = srate;
      srp->width = wid;
      lim = 2*wid;
      srp->len = wid * SRC_SINC_DENSITY;
      srp->data = (Float *)CALLOC(lim+1,sizeof(Float));
      srp->sinc_table = init_sinc_table(wid);
      if (srp->data == NULL) 
	mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate src data array in mus_make_src!");
      else
	for (i=wid;i<lim;i++) srp->data[i] = (*input)(environ,(srate > 0.0) ? 1 : -1);
      /* was i=0 here but we want the incoming data centered */
      return((mus_any *)srp);
    }
  return(NULL);
}

Float mus_src(mus_any *srptr, Float sr_change, Float (*input)(void *arg, int direction))
{
  sr *srp = (sr *)srptr;
  Float sum,x,zf,srx,factor;
  int fsx,lim,i,k,loc;
  sum = 0.0;
  fsx = (int)(srp->x);
  lim = 2*srp->width;
  srx = srp->incr + sr_change;
  if (fsx > 0)
    {
      /* realign data, reset srp->x */
      for (i=fsx,loc=0;i<lim;i++,loc++) srp->data[loc] = srp->data[i];
      for (i=loc;i<lim;i++) 
	{
	  if (input)
	    srp->data[i] = (*input)(srp->environ,(srx > 0.0) ? 1 : -1);
	  else srp->data[i] = (*(srp->feeder))(srp->environ,(srx > 0.0) ? 1 : -1);
	}
      srp->x -= fsx;
    }
  if (srx == 0.0) srx = 0.01;
  if (srx < 0.0) srx = -srx;
  if (srx>1.0) factor=1.0/srx; else factor=1.0;
  zf = factor * (Float)SRC_SINC_DENSITY;
  for (i=0,x=zf*(1.0 - srp->x - srp->width);i<lim;i++,x+=zf)
    {
      /* we're moving backwards in the data array, so the sr->x field has to mimic that (hence the '1.0 - srp->x') */
      if (x<0) k = (int)(-x); else k = (int)x;
      if (k < srp->len) sum += (srp->data[i] * srp->sinc_table[k]);
    }
  srp->x += srx;
  return(sum*factor);
}




/* ---------------- granulate ---------------- */

typedef struct {
  mus_any_class *core;
  Float (*rd)(void *arg, int direction);
  int s20;
  int s50;
  int rmp;
  Float amp;
  int len;
  int cur_out;
  int cur_in;
  int input_hop;
  int ctr;
  int output_hop;
  Float *data;
  Float *in_data;
  int block_len;
  int in_data_len;
  int in_data_start;
  void *environ;
} spd_info;

static char *inspect_spd_info(void *ptr)
{
  spd_info *gen = (spd_info *)ptr;
  char *desc;
  desc = make_big_desc_buf(ptr,FALSE);
  if (desc) sprintf(desc,"spd_info s20: %d, s50: %d, rmp: %d, amp: %f, len: %d, cur_out: %d, cur_in: %d, input_hop: %d, ctr: %d, output_hop: %d, in_data_start: %d, in_data[%d]: %s, data[%d]: %s",
		    gen->s20,gen->s50,gen->rmp,gen->amp,gen->len,gen->cur_out,gen->cur_in,gen->input_hop,
		    gen->ctr,gen->output_hop,gen->in_data_start,
		    gen->in_data_len,print_array(gen->in_data,gen->in_data_len,0),
		    gen->block_len,print_array(gen->data,gen->block_len,0));
  return(desc);
}

int mus_granulate_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_GRANULATE));}

static int granulate_equalp(void *p1, void *p2) {return(p1 == p2);}

static char *describe_granulate(void *ptr)
{
  char *desc;
  spd_info *gen = (spd_info *)ptr;
  desc = make_big_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_granulate_p((mus_any *)ptr))
	{
	  sprintf(desc,"granulate: expansion: %.3f (%d/%d), scaler: %.3f, length: %.3f secs (%d samps), ramp: %.3f",
		  (Float)(gen->input_hop) / (Float)(gen->output_hop),
		  gen->input_hop,gen->output_hop,
		  gen->amp,
		  (Float)(gen->len) / (Float)sampling_rate,gen->len,
		  (Float)(gen->rmp) / (Float)sampling_rate);
	}
      else describe_bad_gen(ptr,desc,"granulate","a");
    }
  return(desc);
}

static int free_granulate(void *ptr)
{
  spd_info *gen = (spd_info *)ptr;
  if (gen)
    {
      if (gen->data) FREE(gen->data);
      if (gen->in_data) FREE(gen->in_data);
      FREE(gen);
    }
  return(0);
}

static int spd_length(void *ptr) {return(((spd_info *)ptr)->len);}
static int spd_set_length(void *ptr, int val) {((spd_info *)ptr)->len = val; return(val);}
static Float spd_scaler(void *ptr) {return(((spd_info *)ptr)->amp);}
static Float spd_set_scaler(void *ptr, Float val) {((spd_info *)ptr)->amp = val; return(val);}
static Float spd_frequency(void *ptr) {return(((Float)((spd_info *)ptr)->output_hop) / (Float)sampling_rate);}
static Float spd_set_frequency(void *ptr, Float val) {((spd_info *)ptr)->output_hop = (int)((Float)sampling_rate / val); return(val);}

int mus_ramp(mus_any *ptr) 
{
  spd_info *gen = (spd_info *)ptr; 
  if ((gen) && ((gen->core)->type == MUS_GRANULATE)) return(gen->rmp);
  return(0);
}

int mus_set_ramp(mus_any *ptr, int val) 
{
  spd_info *gen = (spd_info *)ptr; 
  if ((gen) && ((gen->core)->type == MUS_GRANULATE)) gen->rmp = val;
  return(val);
}

int mus_hop(mus_any *ptr) 
{
  spd_info *gen = (spd_info *)ptr; 
  if ((gen) && ((gen->core)->type == MUS_GRANULATE)) return(gen->output_hop);
  return(0);
}

int mus_set_hop(mus_any *ptr, int val) 
{
  spd_info *gen = (spd_info *)ptr; 
  if ((gen) && ((gen->core)->type == MUS_GRANULATE)) gen->output_hop = val;
  return(val);
}

Float mus_increment(mus_any *rd)
{
  if (mus_readin_p(rd)) return(((rdin *)rd)->dir);
  if (mus_file2sample_p(rd)) return(((rdin *)rd)->dir);
  if (mus_src_p(rd)) return(((sr *)rd)->incr);
  if (mus_buffer_p(rd)) return(((rblk *)rd)->fill_time);
  if (mus_granulate_p(rd)) return(((Float)(((spd_info *)rd)->output_hop))/((Float)((spd_info *)rd)->input_hop));
  return(0);
}

Float mus_set_increment(mus_any *rd, Float dir)
{
  if (mus_readin_p(rd)) ((rdin *)rd)->dir = (int)dir; 
  if (mus_file2sample_p(rd)) ((rdin *)rd)->dir = (int)dir; 
  if (mus_src_p(rd)) ((sr *)rd)->incr = dir;
  if (mus_buffer_p(rd)) {((rblk *)rd)->fill_time = dir; ((rblk *)rd)->empty = (dir == 0.0);}
  if (mus_granulate_p(rd)) {((spd_info *)rd)->input_hop = (int)(((spd_info *)rd)->output_hop / dir);}
 return(dir);
}

static mus_any_class GRANULATE_CLASS = {
  MUS_GRANULATE,
  "granulate",
  &free_granulate,
  &describe_granulate,
  &inspect_spd_info,
  &granulate_equalp,
  0,0,
  &spd_length,  /* segment-length */
  &spd_set_length,
  &spd_frequency, /* spd-out */
  &spd_set_frequency,
  0,0,
  &spd_scaler, /* segment-scaler */
  &spd_set_scaler
};

mus_any *mus_make_granulate(Float (*input)(void *arg,int direction), 
			    Float expansion, Float length, Float scaler, 
			    Float hop, Float ramp, Float jitter, int max_size, void *environ)
{
  spd_info *spd;
  int outlen;
  spd = (spd_info *)CALLOC(1,sizeof(spd_info));
  if (spd == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_granulate!");
  else
    {
      spd->core = &GRANULATE_CLASS;
      spd->cur_out = 0;
      spd->cur_in = 0;
      spd->len = (int)(ceil(length * sampling_rate));
      spd->rmp = (int)(ramp * spd->len);
      spd->amp = scaler;
      spd->output_hop = (int)(hop * sampling_rate);
      spd->input_hop = (int)((Float)(spd->output_hop) / expansion);
      spd->s20 = (int)(jitter * sampling_rate/20);
      spd->s50 = (int)(jitter * sampling_rate/50);
      spd->ctr = 0;
      outlen = (int)(sampling_rate * (hop + length));
      if (max_size > outlen) outlen = max_size;
      spd->block_len = outlen;
      spd->data = (Float *)CALLOC(outlen,sizeof(Float));
      spd->in_data_len = outlen + spd->s20 + 1;
      spd->in_data = (Float *)CALLOC(spd->in_data_len,sizeof(Float));
      spd->in_data_start = spd->in_data_len;
      spd->rd = input;
      spd->environ = environ;
      return((mus_any *)spd);
    }
  return(NULL);
}

static int irandom(int amp)
{
  int val;
  randx=randx*1103515245 + 12345;
  val=(unsigned int)(randx >> 16) & 32767;
  return((int)(amp * (((Float)val)*INVERSE_MAX_RAND)));
}

Float mus_granulate(mus_any *ptr, Float (*input)(void *arg, int direction))
{ 
  spd_info *spd = (spd_info *)ptr;
  int start,end,len,extra,i,j,k,steady_end,curstart;
  Float amp,incr,result;
  result = spd->data[spd->ctr];
  spd->ctr++;
  if (spd->ctr >= spd->cur_out)
    {
      start = spd->cur_out;
      end = spd->len - start;
      if (end < 0) end = 0;
      if (end > 0) for (i=0,j=start;i<end;i++,j++) spd->data[i] = spd->data[j];
      for (i=end;i<spd->block_len;i++) spd->data[i] = 0.0;

      start = spd->in_data_start;
      len = spd->in_data_len;
      if (start > len)
	{
	  extra = start - len;
	  if (input)
	    for (i=0;i<extra;i++) (*input)(spd->environ,1);
	  else for (i=0;i<extra;i++) (*(spd->rd))(spd->environ,1);
	  start = len;
	}
      if (start < len)
	{
	  for (i=0,k=start;k<len;i++,k++)
	    spd->in_data[i] = spd->in_data[k];
	}
      if (input)
	for (i=(len-start);i<len;i++) spd->in_data[i] = (*input)(spd->environ,1);
      else for (i=(len-start);i<len;i++) spd->in_data[i] = (*(spd->rd))(spd->environ,1);
      spd->in_data_start = spd->input_hop;

      amp = 0.0;
      incr = (Float)(spd->amp) / (Float)(spd->rmp);
      steady_end = (spd->len - spd->rmp);
      curstart = irandom(spd->s20);
      for (i=0,j=curstart;i<spd->len;i++,j++)
	{
	  spd->data[i] += (amp * spd->in_data[j]);
	  if (i < spd->rmp) amp += incr; else if (i > steady_end) amp -= incr;
	}
      
      spd->ctr -= spd->cur_out;
      /* spd->cur_out = spd->output_hop + irandom(spd->s50); */             /* this was the original form */
      spd->cur_out = spd->output_hop + irandom(spd->s50) - (spd->s50 >> 1); /* this form suggested by Marc Lehmann */
      if (spd->cur_out < 0) spd->cur_out = 0;
    }
  return(result);
}


/* ---------------- convolve ---------------- */

/* fft and convolution of real data in zero-based arrays
 */

static void mus_scramble (Float* rl, Float* im, int n)
{
  /* bit reversal */

  int i,m,j;
  Float vr,vi;
  j=0;
  for (i=0;i<n;i++)
    {
      if (j>i)
	{
	  vr = rl[j];
	  vi = im[j];
	  rl[j] = rl[i];
	  im[j] = im[i];
	  rl[i] = vr;
	  im[i] = vi;
	}
      m = n>>1;
      while ((m>=2) && (j>=m))
	{
	  j -= m;
	  m = m>>1;
	}
      j += m;
    }
}

void mus_fft (Float *rl, Float *im, int n, int is)
{
  /* standard fft: real part in rl, imaginary in im,
   * rl and im are zero-based.
   * see fxt/simplfft/fft.c (Joerg Arndt) 
   */
  int m,j,mh,ldm,lg,i,i2,j2,imh;
  double ur,ui,u,vr,vi,angle,c,s;
  imh = (int)(ceil(log(n)/log(2.0)));
  mus_scramble(rl,im,n);
  m = 2;
  ldm = 1;
  mh = n>>1;
  angle = (M_PI*is);
  for (lg=0;lg<imh;lg++)
    {
      c = cos(angle);
      s = sin(angle);
      ur = 1.0;
      ui = 0.0;
      for (i2=0;i2<ldm;i2++)
	{
#ifdef LINUX 
	  if (isnan(ui)) ui=0.0;
#endif
	  i = i2;
	  j = i2 + ldm;
	  for (j2=0;j2<mh;j2++)
	    {
	      vr = ur*rl[j] - ui*im[j];
	      vi = ur*im[j] + ui*rl[j];
	      rl[j] = rl[i] - vr;
	      im[j] = im[i] - vi;
	      rl[i] += vr;
	      im[i] += vi;
	      i += m;
	      j += m;
	    }
	  u = ur;
	  ur = (ur*c) - (ui*s);
	  ui = (ui*c) + (u*s);
	}
      mh >>= 1;
      ldm = m;
      angle *= 0.5;
      m <<= 1;
    }
}

#if HAVE_GSL
#include <gsl/gsl_sf_bessel.h>
static Float mus_bessi0(Float x)
{
  gsl_sf_result res;
  gsl_sf_bessel_I0_e(x,&res);
  return(res.val);
}
#else
static Float mus_bessi0(Float x)
{ 
  Float z,denominator,numerator;
  if (x == 0.0) return(1.0);
  if (fabs(x) <= 15.0) 
    {
      z=x*x;
      numerator=(z*(z*(z*(z*(z*(z*(z*(z*(z*(z*(z*(z*(z*(z*
							0.210580722890567e-22+0.380715242345326e-19)+
						     0.479440257548300e-16)+0.435125971262668e-13)+
					       0.300931127112960e-10)+0.160224679395361e-7)+
					 0.654858370096785e-5)+0.202591084143397e-2)+
				   0.463076284721000e0)+0.754337328948189e2)+
			     0.830792541809429e4)+0.571661130563785e6)+
		       0.216415572361227e8)+0.356644482244025e9)+
		 0.144048298227235e10);
      denominator=(z*(z*(z-0.307646912682801e4)+
		      0.347626332405882e7)-0.144048298227235e10);
      return(-numerator/denominator);
    } 
  return(1.0);
}
#endif

static Float sqr(Float x) {return(x*x);}

Float *mus_make_fft_window_with_window(int type, int size, Float beta, Float *window)
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
  int i,j,midn,midp1,midm1;
  Float freq,rate,sr1,angle,expn,expsum,I0beta,cx;
  I0beta = 0.0;
  midn = size >> 1;
  midp1 = (size+1)/2;
  midm1 = (size-1)/2;
  freq = TWO_PI/(Float)size;
  rate = 1.0/(Float)midn;
  angle = 0.0;
  expn = log(2)/(Float)midn+1.0;
  expsum = 1.0;
  if (type == MUS_KAISER_WINDOW) I0beta = mus_bessi0(beta); /* Harris multiplies beta by pi */
  switch (type)
    {
    case MUS_RECTANGULAR_WINDOW:
      for (i=0;i<size;i++) {window[i]=1.0;}
      break; 
    case MUS_HANNING_WINDOW: /* Hann would be more accurate */
      for (i=0,j=size-1,angle=0.0;i<=midn;i++,j--,angle+=freq) {window[j]=(window[i]=0.5-0.5*cos(angle));}
      break; 
    case MUS_WELCH_WINDOW:
      for (i=0,j=size-1;i<=midn;i++,j--) {window[j]=(window[i]=1.0-sqr((Float)(i-midm1)/(Float)midp1));}
      break; 
    case MUS_PARZEN_WINDOW:
      for (i=0,j=size-1;i<=midn;i++,j--) {window[j]=(window[i]=1.0-fabs((Float)(i-midm1)/(Float)midp1));}
      break; 
    case MUS_BARTLETT_WINDOW:
      for (i=0,j=size-1,angle=0.0;i<=midn;i++,j--,angle+=rate) {window[j]=(window[i]=angle);}
      break; 
    case MUS_HAMMING_WINDOW:
      for (i=0,j=size-1,angle=0.0;i<=midn;i++,j--,angle+=freq) {window[j]=(window[i]=0.54-0.46*cos(angle));} 
      break; 
    case MUS_BLACKMAN2_WINDOW: /* using Chebyshev polynomial equivalents here */
      for (i=0,j=size-1,angle=0.0;i<=midn;i++,j--,angle+=freq) 
	{              /* (+ 0.42323 (* -0.49755 (cos a)) (* 0.07922 (cos (* a 2)))) */
	  cx = cos(angle);
	  window[j]=(window[i]=(.34401+(cx*(-.49755+(cx*.15844)))));
	}
      break; 
    case MUS_BLACKMAN3_WINDOW:
      for (i=0,j=size-1,angle=0.0;i<=midn;i++,j--,angle+=freq) 
	{              /* (+ 0.35875 (* -0.48829 (cos a)) (* 0.14128 (cos (* a 2))) (* -0.01168 (cos (* a 3)))) */
	  cx = cos(angle);
	  window[j]=(window[i]=(.21747+(cx*(-.45325+(cx*(.28256-(cx*.04672)))))));
	}
      break; 
    case MUS_BLACKMAN4_WINDOW:
      for (i=0,j=size-1,angle=0.0;i<=midn;i++,j--,angle+=freq) 
	{             /* (+ 0.287333 (* -0.44716 (cos a)) (* 0.20844 (cos (* a 2))) (* -0.05190 (cos (* a 3))) (* 0.005149 (cos (* a 4)))) */
	  cx = cos(angle);
	  window[j]=(window[i]=(.084037+(cx*(-.29145+(cx*(.375696+(cx*(-.20762+(cx*.041194)))))))));
	}
      break; 
    case MUS_EXPONENTIAL_WINDOW:
      for (i=0,j=size-1;i<=midn;i++,j--) {window[j]=(window[i]=expsum-1.0); expsum *= expn;}
      break;
    case MUS_KAISER_WINDOW:
      for (i=0,j=size-1,angle=1.0;i<=midn;i++,j--,angle-=rate) {window[j]=(window[i]=mus_bessi0(beta*sqrt(1.0-sqr(angle)))/I0beta);}
      break;
    case MUS_CAUCHY_WINDOW:
      for (i=0,j=size-1,angle=1.0;i<=midn;i++,j--,angle-=rate) {window[j]=(window[i]=1.0/(1.0+sqr(beta*angle)));}
      break;
    case MUS_POISSON_WINDOW:
      for (i=0,j=size-1,angle=1.0;i<=midn;i++,j--,angle-=rate) {window[j]=(window[i]=exp((-beta)*angle));}
      break;
    case MUS_RIEMANN_WINDOW:
      sr1 = TWO_PI/(Float)size;
      for (i=0,j=size-1;i<=midn;i++,j--) 
	{
	  if (i == midn) window[j]=(window[i]=1.0);
	  else 
	    {
	      cx = sr1 * (midn - i); /* split out because NeXT C compiler can't handle the full expression */
	      window[i]=sin(cx) / cx;
	      window[j]=window[i];
	    }
	}
      break;
    case MUS_GAUSSIAN_WINDOW:
      for (i=0,j=size-1,angle=1.0;i<=midn;i++,j--,angle-=rate) {window[j]=(window[i]=exp(-.5*sqr(beta*angle)));}
      break;
    case MUS_TUKEY_WINDOW:
      cx = midn*(1.0-beta);
      for (i=0,j=size-1;i<=midn;i++,j--) 
	{
	  if (i >= cx) window[j]=(window[i]=1.0);
	  else window[j]=(window[i]=.5*(1.0-cos(M_PI*i/cx)));
	}
      break;
    default: 
      mus_error(MUS_NO_SUCH_FFT_WINDOW,"unknown fft data window: %d",type); 
      break;
    }
  return(window);
}

Float *mus_make_fft_window(int type, int size, Float beta)
{
  Float *window = NULL;
  window = (Float *)CALLOC(size,sizeof(Float));
  if (window == NULL)
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate fft window!");
  else mus_make_fft_window_with_window(type,size,beta,window);
  return(window);
}

void mus_spectrum(Float *rdat, Float *idat, Float *window, int n, int type)
{
  int i;
  Float maxa,todb,lowest,val;
  if (window) mus_multiply_arrays(rdat,window,n);
  mus_clear_array(idat,n);
  mus_fft(rdat,idat,n,1);
  lowest=0.00000001;
  maxa=0.0;
  n = (int)(n*0.5);
  for (i=0;i<n;i++)
    {
#if (__linux__ && __PPC__)
      if (rdat[i] < lowest) rdat[i] = 0.0;
      if (idat[i] < lowest) idat[i] = 0.0;
#endif
      val = rdat[i]*rdat[i]+idat[i]*idat[i];
      if (val < lowest)
	rdat[i] = .0001;
      else 
	{
	  rdat[i]=sqrt(val);
	  if (rdat[i] > maxa) maxa=rdat[i];
	}
    }
  if (maxa>0.0)
    {
      maxa=1.0/maxa;
      if (type == 0) /* dB */
	{
	  todb=20.0/log(10.0);
	  for (i=0;i<n;i++) rdat[i]=todb*log(rdat[i]*maxa);
	}
      else
	{
	  if (type == 1) /* linear, normalized */
	    {
	      for (i=0;i<n;i++) rdat[i] *= maxa;
	    }
	}
    }
}

void mus_convolution (Float* rl1, Float* rl2, int n)
{
  /* convolves two real arrays.                                           */
  /* rl1 and rl2 are assumed to be set up correctly for the convolution   */
  /* (that is, rl1 (the "signal") is zero-padded by length of             */
  /* (non-zero part of) rl2 and rl2 is stored in wrap-around order)       */
  /* We treat rl2 as the imaginary part of the first fft, then do         */
  /* the split, scaling, and (complex) spectral multiply in one step.     */
  /* result in rl1                                                        */

  int j,n2,nn2;
  Float rem,rep,aim,aip,invn;

  mus_fft(rl1,rl2,n,1);
  
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
  
  mus_fft(rl1,rl2,n,-1);
}


typedef struct {
  mus_any_class *core;
  Float (*feeder)(void *arg, int direction);
  int fftsize, fftsize2, ctr, filtersize;
  Float *rl1,*rl2,*buf,*filter; 
  void *environ;
} conv;

static char *inspect_conv(void *ptr)
{
  conv *gen = (conv *)ptr;
  char *desc;
  desc = make_big_desc_buf(ptr,FALSE);
  if (desc) sprintf(desc,"conv fftsize: %d, fftsize2: %d, filtersize: %d, ctr: %d, rl1: %s, rl2: %s, buf: %s, filter: %s",
		    gen->fftsize,gen->fftsize2,gen->filtersize,gen->ctr,
		    print_array(gen->rl1,gen->fftsize,0),
		    print_array(gen->rl2,gen->fftsize,0),
		    print_array(gen->buf,gen->fftsize,0),
		    print_array(gen->filter,gen->filtersize,0));
  return(desc);
}

static int convolve_equalp(void *p1, void *p2) {return(p1 == p2);}

static char *describe_convolve(void *ptr)
{
  char *desc;
  conv *gen = (conv *)ptr;
  desc = make_desc_buf(ptr,TRUE);
  if (desc)
    {
      if (mus_convolve_p((mus_any *)ptr))
	{
	  sprintf(desc,"convolve: size: %d",gen->fftsize);
	}
      else describe_bad_gen(ptr,desc,"convolve","a");
    }
  return(desc);
}

static int free_convolve(void *ptr)
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

static int conv_length(void *ptr) {return(((conv *)ptr)->fftsize);}

static mus_any_class CONVOLVE_CLASS = {
  MUS_CONVOLVE,
  "convolve",
  &free_convolve,
  &describe_convolve,
  &inspect_conv,
  &convolve_equalp,
  0,0,
  &conv_length,
  0,
  0,0,0,0,
  0,0
};

Float mus_convolve(mus_any *ptr, Float (*input)(void *arg, int direction))
{
  conv *gen = (conv *)ptr;
  Float result;
  int i,j;
  if (gen->ctr >= gen->fftsize2)
    {
      for (i=0,j=gen->fftsize2;i<gen->fftsize2;i++,j++) 
	{
	  gen->buf[i] = gen->buf[j]; 
	  gen->buf[j] = 0.0;
	  if (input)
	    gen->rl1[i] = (*input)(gen->environ,1);
	  else gen->rl1[i] = (*(gen->feeder))(gen->environ,1);
	  gen->rl1[j] = 0.0;
	  gen->rl2[i] = 0.0;
	  gen->rl2[j] = 0.0;
	}
      for (i=0;i<gen->filtersize;i++) gen->rl2[i] = gen->filter[i];
      mus_convolution(gen->rl1,gen->rl2,gen->fftsize);
      for (i=0,j=gen->fftsize2;i<gen->fftsize2;i++,j++) 
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

int mus_convolve_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_CONVOLVE));}

mus_any *mus_make_convolve(Float (*input)(void *arg, int direction), Float *filter, int fftsize, int filtersize, void *environ)
{
  conv *gen = NULL;
  gen = (conv *)CALLOC(1,sizeof(conv));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate struct for mus_make_convolve!");
  else
    {
      gen->core = &CONVOLVE_CLASS;
      gen->feeder = input;
      gen->environ = environ;
      gen->filter = filter;
      gen->filtersize = filtersize;
      gen->fftsize = fftsize;
      gen->fftsize2 = gen->fftsize/2;
      gen->ctr = gen->fftsize2;
      gen->rl1 = (Float *)CALLOC(fftsize,sizeof(Float));
      if (gen->rl1 == NULL) 
	mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate rl1 array for mus_make_convolve!");
      else
	{
	  gen->rl2 = (Float *)CALLOC(fftsize,sizeof(Float));
	  if (gen->rl2 == NULL) 
	    mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate rl2 array for mus_make_convolve!");
	  else
	    {
	      gen->buf = (Float *)CALLOC(fftsize,sizeof(Float));
	      if (gen->buf == NULL) 
		mus_error(MUS_MEMORY_ALLOCATION_FAILED,"can't allocate buf array for mus_make_convolve!");
	      else
		return((mus_any *)gen);
	    }
	}
      if (gen)
	{
	  if (gen->rl1) FREE(gen->rl1);
	  if (gen->rl2) FREE(gen->rl2);
	  if (gen->buf) FREE(gen->buf);
	}
    }
  return(NULL);
}

void mus_convolve_files(const char *file1, const char *file2, Float maxamp, const char *output_file)
{
  int file1_len,file2_len,fftlen,outlen,totallen,file1_chans,file2_chans,output_chans,c1,c2;
  Float *data1,*data2,*outdat;
  MUS_SAMPLE_TYPE *samps;
  Float maxval = 0.0;
  int i,j,k;
  file1_len = mus_sound_frames(file1);
  file2_len = mus_sound_frames(file2);
  file1_chans = mus_sound_chans(file1);
  file2_chans = mus_sound_chans(file2);
  output_chans = file1_chans; if (file2_chans > output_chans) output_chans = file2_chans;
  fftlen = (int)(pow(2.0,(int)ceil(log(file1_len+file2_len+1)/log(2.0))));
  outlen = file1_len+file2_len+1;
  totallen = outlen * output_chans;
  data1 = (Float *)CALLOC(fftlen,sizeof(Float));
  data2 = (Float *)CALLOC(fftlen,sizeof(Float));
  if (output_chans == 1)
    {
      samps = (MUS_SAMPLE_TYPE *)CALLOC(fftlen,sizeof(MUS_SAMPLE_TYPE));
      mus_file_to_array(file1,0,0,file1_len,samps); 
      for (i=0;i<file1_len;i++) data1[i] = MUS_SAMPLE_TO_DOUBLE(samps[i]);
      mus_file_to_array(file2,0,0,file2_len,samps);
      for (i=0;i<file2_len;i++) data2[i] = MUS_SAMPLE_TO_DOUBLE(samps[i]);
      mus_convolution(data1,data2,fftlen);
      for (i=0;i<outlen;i++) if (maxval < fabs(data1[i])) maxval = fabs(data1[i]);
      if (maxval > 0.0)
	{
	  maxval = maxamp / maxval;
	  for (i=0;i<outlen;i++) data1[i] *= maxval;
	}
      for (i=0;i<outlen;i++) samps[i] = MUS_DOUBLE_TO_SAMPLE(data1[i]);
      mus_array_to_file(output_file,samps,outlen,mus_sound_srate(file1),1);
      FREE(samps);
    }
  else
    {
      samps = (MUS_SAMPLE_TYPE *)CALLOC(totallen,sizeof(MUS_SAMPLE_TYPE));
      outdat = (Float *)CALLOC(totallen,sizeof(Float));
      c1 = 0; c2 = 0;
      for (i=0;i<output_chans;i++)
	{
	  mus_file_to_array(file1,c1,0,file1_len,samps);
	  for (i=0;i<file1_len;i++) data1[i] = MUS_SAMPLE_TO_DOUBLE(samps[i]);
	  mus_file_to_array(file2,c2,0,file2_len,samps);
	  for (i=0;i<file2_len;i++) data2[i] = MUS_SAMPLE_TO_DOUBLE(samps[i]);
	  mus_convolution(data1,data2,fftlen);
	  for (j=i,k=0;j<totallen;j+=output_chans,k++) outdat[j] = data1[k];
	  c1++; if (c1 >= file1_chans) c1 = 0;
	  c2++; if (c2 >= file2_chans) c2 = 0;
	  for (i=0;i<fftlen;i++) {data1[i] = 0.0; data2[i] = 0.0;}
	}
      for (i=0;i<totallen;i++) if (maxval < fabs(outdat[i])) maxval = fabs(outdat[i]);
      if (maxval > 0.0)
	{
	  maxval = maxamp / maxval;
	  for (i=0;i<totallen;i++) outdat[i] *= maxval;
	}
      for (i=0;i<totallen;i++) samps[i] = MUS_DOUBLE_TO_SAMPLE(outdat[i]);
      mus_array_to_file(output_file,samps,totallen,mus_sound_srate(file1),output_chans);
      FREE(samps);
      FREE(outdat);
    }
  FREE(data1);
  FREE(data2);
}


void init_mus_module(void)
{
#if (!HAVE_SNDLIB)
  mus_error_buffer = (char *)CALLOC(1024,sizeof(char));
#endif
#if WITH_SINE_TABLE
  init_sine();
#endif
}

/* a mixing "instrument" along the lines of the mix function in clm */
/* this is a very commonly used function, so it's worth picking out the special cases for optimization */

#define IDENTITY_MIX 0
#define IDENTITY_MONO_MIX 1
#define SCALED_MONO_MIX 2
#define SCALED_MIX 3
#define ENVELOPED_MONO_MIX 4
#define ENVELOPED_MIX 5

void mus_mix(const char *outfile, const char *infile, int out_start, int out_frames, int in_start, mus_mixer *mx, mus_any ***envs)
{
#if HAVE_SNDLIB
  int i,j=0,k,m,ofd,ifd,inc,outc,in_chans,out_chans,mix_chans,min_chans,curoutframes,mixtype;
  MUS_SAMPLE_TYPE **obufs,**ibufs;
  mus_frame *frin,*frthru = NULL;
  mus_any *inf,*outf;
  Float scaler;
  mus_any *e;
  out_chans = mus_sound_chans(outfile);
  in_chans = mus_sound_chans(infile);
  if (out_chans > in_chans) mix_chans = out_chans; else mix_chans = in_chans;
  if (out_chans > in_chans) min_chans = in_chans; else min_chans = out_chans;
  if ((mx) && (envs) && ((in_chans > 1) || (out_chans > 1)))
    {
      /* the general case -- possible envs/scalers on every mixer cell */
      outf = mus_make_frame2file(outfile,out_chans,mus_sound_data_format(outfile),mus_sound_header_type(outfile));
      inf = mus_make_file2frame(infile);
      frin = mus_make_empty_frame(mix_chans);
      frthru = mus_make_empty_frame(mix_chans);
      for (i=0,inc=in_start,outc=out_start;i<out_frames;i++,inc++,outc++)
	{
	  for (j=0;j<in_chans;j++)
	    {
	      for (k=0;k<out_chans;k++)
		{
		  if (envs[j][k])
		    mx->vals[j][k] = mus_env(envs[j][k]);
		}
	    }
	  mus_frame2file(outf,outc,mus_frame2frame(mx,mus_file2frame(inf,inc,frin),frthru));
	}
      mus_free((mus_any *)inf);
      mus_free((mus_any *)outf);
      mus_free((mus_any *)frin);
      if (frthru) mus_free((mus_any *)frthru);
    }
  else
    {
      /* highly optimizable cases */
      if (envs)
	{
	  if ((in_chans == 1) && (out_chans == 1)) 
	    mixtype = ENVELOPED_MONO_MIX;
	  else mixtype = ENVELOPED_MIX; /* i.e., mx nil, you get one global env */
	}
      else
	{
	  if (mx)
	    {
	      if ((in_chans == 1) && (out_chans == 1)) 
		{
		  if (mx->vals[0][0] == 1.0)
		    mixtype = IDENTITY_MONO_MIX; 
		  else mixtype = SCALED_MONO_MIX;
		}
	      else
		{
		  mixtype = IDENTITY_MIX;
		  for (i=0;i<mx->chans;i++)
		    for (j=0;j<mx->chans;j++)
		      if (((i == j) && (mx->vals[i][i] != 1.0)) ||
			  ((i != j) && (mx->vals[i][j] != 0.0)))
		      {
			mixtype = SCALED_MIX;
			break;
		      }
		}
	    }
	  else
	    {
	      if ((in_chans == 1) && (out_chans == 1)) 
		mixtype = IDENTITY_MONO_MIX;
	      else mixtype = IDENTITY_MIX;
	    }
	}
      obufs = (MUS_SAMPLE_TYPE **)CALLOC(out_chans,sizeof(MUS_SAMPLE_TYPE *));
      for (i=0;i<out_chans;i++) obufs[i] = (MUS_SAMPLE_TYPE *)CALLOC(mus_file_buffer_size,sizeof(MUS_SAMPLE_TYPE));
      ibufs = (MUS_SAMPLE_TYPE **)CALLOC(in_chans,sizeof(MUS_SAMPLE_TYPE *));
      for (i=0;i<in_chans;i++) ibufs[i] = (MUS_SAMPLE_TYPE *)CALLOC(mus_file_buffer_size,sizeof(MUS_SAMPLE_TYPE));
      ifd = mus_sound_open_input(infile);
      mus_sound_seek_frame(ifd,in_start);
      mus_sound_read(ifd,0,mus_file_buffer_size-1,in_chans,ibufs);
      ofd = mus_sound_reopen_output(outfile,out_chans,mus_sound_data_format(outfile),mus_sound_header_type(outfile),mus_sound_data_location(outfile));
      curoutframes = mus_sound_frames(outfile);
      mus_sound_seek_frame(ofd,out_start);
      mus_sound_read(ofd,0,mus_file_buffer_size-1,out_chans,obufs);
      mus_sound_seek_frame(ofd,out_start);
      switch (mixtype)
	{
	case IDENTITY_MONO_MIX:
	  for (k=0,j=0;k<out_frames;k++,j++)
	    {
	      if (j == mus_file_buffer_size)
		{
		  mus_sound_write(ofd,0,j-1,out_chans,obufs);
		  j = 0;
		  mus_sound_seek_frame(ofd,out_start+k);
		  mus_sound_read(ofd,0,mus_file_buffer_size-1,out_chans,obufs);
		  mus_sound_seek_frame(ofd,out_start+k);
		  mus_sound_read(ifd,0,mus_file_buffer_size-1,in_chans,ibufs);
		}
	      obufs[0][j] += ibufs[0][j];
	    }
	  break;
	case IDENTITY_MIX:
	  for (k=0,j=0;k<out_frames;k++,j++)
	    {
	      if (j == mus_file_buffer_size)
		{
		  mus_sound_write(ofd,0,j-1,out_chans,obufs);
		  j = 0;
		  mus_sound_seek_frame(ofd,out_start+k);
		  mus_sound_read(ofd,0,mus_file_buffer_size-1,out_chans,obufs);
		  mus_sound_seek_frame(ofd,out_start+k);
		  mus_sound_read(ifd,0,mus_file_buffer_size-1,in_chans,ibufs);
		}
	      for (i=0;i<min_chans;i++)
		obufs[i][j] += ibufs[i][j];
	    }
	  break;
	case SCALED_MONO_MIX:
	  scaler = mx->vals[0][0];
	  for (k=0,j=0;k<out_frames;k++,j++)
	    {
	      if (j == mus_file_buffer_size)
		{
		  mus_sound_write(ofd,0,j-1,out_chans,obufs);
		  j = 0;
		  mus_sound_seek_frame(ofd,out_start+k);
		  mus_sound_read(ofd,0,mus_file_buffer_size-1,out_chans,obufs);
		  mus_sound_seek_frame(ofd,out_start+k);
		  mus_sound_read(ifd,0,mus_file_buffer_size-1,in_chans,ibufs);
		}
	      obufs[0][j] += (MUS_SAMPLE_TYPE)(scaler * ibufs[0][j]);
	    }
	  break;
	case SCALED_MIX:
	  for (k=0,j=0;k<out_frames;k++,j++)
	    {
	      if (j == mus_file_buffer_size)
		{
		  mus_sound_write(ofd,0,j-1,out_chans,obufs);
		  j = 0;
		  mus_sound_seek_frame(ofd,out_start+k);
		  mus_sound_read(ofd,0,mus_file_buffer_size-1,out_chans,obufs);
		  mus_sound_seek_frame(ofd,out_start+k);
		  mus_sound_read(ifd,0,mus_file_buffer_size-1,in_chans,ibufs);
		}
	      for (i=0;i<min_chans;i++)
		for (m=0;m<in_chans;m++)
		  obufs[i][j] += (MUS_SAMPLE_TYPE)(ibufs[m][j] * mx->vals[m][i]);
	    }
	  break;
	case ENVELOPED_MONO_MIX:
	  e = envs[0][0];
	  for (k=0,j=0;k<out_frames;k++,j++)
	    {
	      if (j == mus_file_buffer_size)
		{
		  mus_sound_write(ofd,0,j-1,out_chans,obufs);
		  j = 0;
		  mus_sound_seek_frame(ofd,out_start+k);
		  mus_sound_read(ofd,0,mus_file_buffer_size-1,out_chans,obufs);
		  mus_sound_seek_frame(ofd,out_start+k);
		  mus_sound_read(ifd,0,mus_file_buffer_size-1,in_chans,ibufs);
		}
	      obufs[0][j] += (MUS_SAMPLE_TYPE)(mus_env(e) * ibufs[0][j]);
	    }
	  break;
	case ENVELOPED_MIX:
	  e = envs[0][0];
	  for (k=0,j=0;k<out_frames;k++,j++)
	    {
	      if (j == mus_file_buffer_size)
		{
		  mus_sound_write(ofd,0,j-1,out_chans,obufs);
		  j = 0;
		  mus_sound_seek_frame(ofd,out_start+k);
		  mus_sound_read(ofd,0,mus_file_buffer_size-1,out_chans,obufs);
		  mus_sound_seek_frame(ofd,out_start+k);
		  mus_sound_read(ifd,0,mus_file_buffer_size-1,in_chans,ibufs);
		}
	      scaler = mus_env(e);
	      for (i=0;i<min_chans;i++)
		obufs[i][j] += (MUS_SAMPLE_TYPE)(scaler * ibufs[i][j]);
	    }
	  break;

	}
      if (j > 0) mus_sound_write(ofd,0,j-1,out_chans,obufs);
      if (curoutframes < (out_frames+out_start)) curoutframes = out_frames + out_start;
      mus_sound_close_output(ofd,curoutframes * out_chans * mus_data_format_to_bytes_per_sample(mus_sound_data_format(outfile)));
      mus_sound_close_input(ifd);
      for (i=0;i<in_chans;i++) FREE(ibufs[i]);
      FREE(ibufs);
      for (i=0;i<out_chans;i++) FREE(obufs[i]);
      FREE(obufs);
    }
#endif
}

#if (!HAVE_SNDLIB)
  #define MUS_FLOAT_TO_FIX 32768.0
  #define MUS_FIX_TO_FLOAT 0.000030517578
#endif

int mus_file2fltarray(const char *filename, int chan, int start, int samples, Float *array)
{
  MUS_SAMPLE_TYPE *idata;
  int i,len;
  idata = (MUS_SAMPLE_TYPE *)CALLOC(samples,sizeof(MUS_SAMPLE_TYPE));
  len = mus_file_to_array(filename,chan,start,samples,idata);
  if (len != -1) for (i=0;i<samples;i++) array[i] = MUS_SAMPLE_TO_FLOAT(idata[i]);
  FREE(idata);
  return(len);
}

int mus_fltarray2file(const char *filename, Float *ddata, int len, int srate, int channels)
{
  MUS_SAMPLE_TYPE *idata;
  int i,err;
  idata = (MUS_SAMPLE_TYPE *)CALLOC(len,sizeof(MUS_SAMPLE_TYPE));
  for (i=0;i<len;i++) idata[i] = MUS_FLOAT_TO_SAMPLE(ddata[i]);
  err = mus_array_to_file(filename,idata,len,srate,channels);
  FREE(idata);
  return(err);
}

Float mus_apply(mus_any *gen, ...)
{
  /* what about non-gen funcs such as polynomial, ring_modulate etc? */
  #define NEXT_ARG (Float)(va_arg(ap,double))
  va_list ap;
  Float f1,f2; /* force order of evaluation */
  int i1,i2;
  if (gen)
    {
      va_start(ap,gen);
      switch ((gen->core)->type)
	{
	case MUS_OSCIL:          f1 = NEXT_ARG; f2 = NEXT_ARG; return(mus_oscil(gen,f1,f2));         break;
	case MUS_SUM_OF_COSINES: f1 = NEXT_ARG;                return(mus_sum_of_cosines(gen,f1));   break;
	case MUS_DELAY:          f1 = NEXT_ARG; f2 = NEXT_ARG; return(mus_delay(gen,f1,f2));         break;
	case MUS_COMB:           f1 = NEXT_ARG; f2 = NEXT_ARG; return(mus_comb(gen,f1,f2));          break;
	case MUS_NOTCH:          f1 = NEXT_ARG; f2 = NEXT_ARG; return(mus_notch(gen,f1,f2));         break;
	case MUS_ALL_PASS:       f1 = NEXT_ARG; f2 = NEXT_ARG; return(mus_all_pass(gen,f1,f2));      break;
	case MUS_TABLE_LOOKUP:   f1 = NEXT_ARG;                return(mus_table_lookup(gen,f1));     break;
	case MUS_SQUARE_WAVE:    f1 = NEXT_ARG;                return(mus_square_wave(gen,f1));      break;
	case MUS_SAWTOOTH_WAVE:  f1 = NEXT_ARG;                return(mus_sawtooth_wave(gen,f1));    break;
	case MUS_TRIANGLE_WAVE:  f1 = NEXT_ARG;                return(mus_triangle_wave(gen,f1));    break;
	case MUS_PULSE_TRAIN:    f1 = NEXT_ARG;                return(mus_pulse_train(gen,f1));      break;
	case MUS_RAND:           f1 = NEXT_ARG;                return(mus_rand(gen,f1));             break;
	case MUS_RAND_INTERP:    f1 = NEXT_ARG;                return(mus_rand_interp(gen,f1));      break;
	case MUS_ASYMMETRIC_FM:  f1 = NEXT_ARG; f2 = NEXT_ARG; return(mus_asymmetric_fm(gen,f1,f2)); break;
	case MUS_ONE_ZERO:       f1 = NEXT_ARG;                return(mus_one_zero(gen,f1));         break;
	case MUS_ONE_POLE:       f1 = NEXT_ARG;                return(mus_one_pole(gen,f1));         break;
	case MUS_TWO_ZERO:       f1 = NEXT_ARG;                return(mus_two_zero(gen,f1));         break;
	case MUS_TWO_POLE:       f1 = NEXT_ARG;                return(mus_two_pole(gen,f1));         break;
	case MUS_FORMANT:        f1 = NEXT_ARG;                return(mus_formant(gen,f1));          break;
	case MUS_WAVESHAPE:      f1 = NEXT_ARG; f2 = NEXT_ARG; return(mus_waveshape(gen,f1,f2));     break;
	case MUS_SRC:            f1 = NEXT_ARG;                return(mus_src(gen,f1,NULL));         break; /* how to pass procedure arg here? */
	case MUS_SINE_SUMMATION: f1 = NEXT_ARG;                return(mus_sine_summation(gen,f1));   break;
	case MUS_WAVE_TRAIN:     f1 = NEXT_ARG;                return(mus_wave_train(gen,f1));       break;
	case MUS_FILTER:         f1 = NEXT_ARG;                return(mus_filter(gen,f1));           break;
	case MUS_FIR_FILTER:     f1 = NEXT_ARG;                return(mus_fir_filter(gen,f1));       break;
	case MUS_IIR_FILTER:     f1 = NEXT_ARG;                return(mus_iir_filter(gen,f1));       break;
	case MUS_READIN:                                       return(mus_readin(gen));              break;
	case MUS_CONVOLVE:                                     return(mus_convolve(gen,NULL));       break;
	case MUS_ENV:                                          return(mus_env(gen));                 break;
	case MUS_BUFFER:                                       return(mus_buffer2sample(gen));       break;  /* actually ambiguous -- buffer2frame */
	case MUS_GRANULATE:                                    return(mus_granulate(gen,NULL));      break;
	case MUS_LOCSIG:         i1 = (int)(va_arg(ap,long)); 
                                                f2 = NEXT_ARG; mus_locsig(gen,i1,f2);                break; /* returns a frame -- should we call frame_ref? */
	case MUS_FILE2SAMPLE:    i1 = (int)(va_arg(ap,long)); 
                                 i2 = (int)(va_arg(ap,long));  return(mus_file2sample(gen,i1,i2));   break;
	  /* user defined gens? */
 	  /* originally I was going to set up a core->run pointer for every generator */
	}
      va_end(ap);
    }
  return(0.0);
}
