#include <mus-config.h>

#if USE_SND
  #include "snd.h"
#endif

#include <math.h>
#include <stdio.h>
#if HAVE_FCNTL_H
  #include <fcntl.h>
#endif
#if HAVE_LIMITS_H
  #include <limits.h>
#endif
#include <errno.h>
#include <stdlib.h>

#if (defined(HAVE_LIBC_H) && (!defined(HAVE_UNISTD_H)))
  #include <libc.h>
#else
  #if (!(defined(_MSC_VER)))
    #include <unistd.h>
  #endif
#endif
#if HAVE_STRING_H
  #include <string.h>
#endif
#include <stdarg.h>
#if HAVE_PTHREAD_H
  #include <pthread.h>
#endif

#include <sys/stat.h>

#include "_sndlib.h"


/* ---------------------------------------- */
/* in io.c but not in _sndlib.h (these are now internal to sndlib, but I don't like the C-oid "_" prefix): */
void mus_bint_to_char(unsigned char *j, int x);
int mus_char_to_bint(const unsigned char *inp);
void mus_lint_to_char(unsigned char *j, int x);
int mus_char_to_lint(const unsigned char *inp);
mus_long_t mus_char_to_llong(const unsigned char *inp);
mus_long_t mus_char_to_blong(const unsigned char *inp);
int mus_char_to_uninterpreted_int(const unsigned char *inp);
void mus_bfloat_to_char(unsigned char *j, float x);
float mus_char_to_bfloat(const unsigned char *inp);
void mus_lfloat_to_char(unsigned char *j, float x);
float mus_char_to_lfloat(const unsigned char *inp);
void mus_bshort_to_char(unsigned char *j, short x);
short mus_char_to_bshort(const unsigned char *inp);
void mus_lshort_to_char(unsigned char *j, short x);
short mus_char_to_lshort(const unsigned char *inp);
unsigned short mus_char_to_ubshort(const unsigned char *inp);
unsigned short mus_char_to_ulshort(const unsigned char *inp);
double mus_char_to_ldouble(const unsigned char *inp);
double mus_char_to_bdouble(const unsigned char *inp);
void mus_bdouble_to_char(unsigned char *j, double x);
void mus_blong_to_char(unsigned char *j, mus_long_t x);
void mus_llong_to_char(unsigned char *j, mus_long_t x);
unsigned int mus_char_to_ubint(const unsigned char *inp);
unsigned int mus_char_to_ulint(const unsigned char *inp);
/* ---------------------------------------- */



static mus_long_t mus_maximum_malloc = MUS_MAX_MALLOC_DEFAULT;

mus_long_t mus_max_malloc(void)
{
  return(mus_maximum_malloc);
}

mus_long_t mus_set_max_malloc(mus_long_t new_max)
{
  mus_maximum_malloc = new_max;
  return(new_max);
}



static mus_long_t mus_maximum_table_size = MUS_MAX_TABLE_SIZE_DEFAULT;

mus_long_t mus_max_table_size(void)
{
  return(mus_maximum_table_size);
}

mus_long_t mus_set_max_table_size(mus_long_t new_max)
{
  mus_maximum_table_size = new_max;
  return(new_max);
}



void mus_bint_to_char(unsigned char *j, int x)
{
  unsigned char *ox = (unsigned char *)&x;
#if MUS_LITTLE_ENDIAN
  j[0] = ox[3]; j[1] = ox[2]; j[2] = ox[1]; j[3] = ox[0];
#else
  memcpy((void *)j, (void *)ox, 4);
#endif
}


int mus_char_to_bint(const unsigned char *inp)
{
  int o;
  unsigned char *outp = (unsigned char *)&o;
#if MUS_LITTLE_ENDIAN
  outp[0] = inp[3]; outp[1] = inp[2]; outp[2] = inp[1]; outp[3] = inp[0];
#else
  memcpy((void *)outp, (void *)inp, 4);
#endif
  return(o);
}


void mus_lint_to_char(unsigned char *j, int x)
{
  unsigned char *ox = (unsigned char *)&x;
#if (!MUS_LITTLE_ENDIAN)
  j[0] = ox[3]; j[1] = ox[2]; j[2] = ox[1]; j[3] = ox[0];
#else
  memcpy((void *)j, (void *)ox, 4);
#endif
}


int mus_char_to_lint(const unsigned char *inp)
{
  int o;
  unsigned char *outp = (unsigned char *)&o;
#if (!MUS_LITTLE_ENDIAN)
  outp[0] = inp[3]; outp[1] = inp[2]; outp[2] = inp[1]; outp[3] = inp[0];
#else
  memcpy((void *)outp, (void *)inp, 4);
#endif
  return(o);
}


void mus_blong_to_char(unsigned char *j, mus_long_t x)
{
  unsigned char *ox = (unsigned char *)&x;
#if (!MUS_LITTLE_ENDIAN)
  memcpy((void *)j, (void *)ox, 8);
#else
  j[0] = ox[7]; j[1] = ox[6]; j[2] = ox[5]; j[3] = ox[4]; j[4] = ox[3]; j[5] = ox[2]; j[6] = ox[1]; j[7] = ox[0];
#endif
}


mus_long_t mus_char_to_blong(const unsigned char *inp)
{
  mus_long_t o;
  unsigned char *outp = (unsigned char *)&o;
#if MUS_LITTLE_ENDIAN
  outp[0] = inp[7]; outp[1] = inp[6]; outp[2] = inp[5]; outp[3] = inp[4]; outp[4] = inp[3]; outp[5] = inp[2]; outp[6] = inp[1]; outp[7] = inp[0];
#else
  memcpy((void *)outp, (void *)inp, 8);
#endif
  return(o);
}


void mus_llong_to_char(unsigned char *j, mus_long_t x)
{
  unsigned char *ox = (unsigned char *)&x;
#if (MUS_LITTLE_ENDIAN)
  memcpy((void *)j, (void *)ox, 8);
#else
  j[0] = ox[7]; j[1] = ox[6]; j[2] = ox[5]; j[3] = ox[4]; j[4] = ox[3]; j[5] = ox[2]; j[6] = ox[1]; j[7] = ox[0];
#endif
}


mus_long_t mus_char_to_llong(const unsigned char *inp)
{
  mus_long_t o;
  unsigned char *outp = (unsigned char *)&o;
#if (!MUS_LITTLE_ENDIAN)
  outp[0] = inp[7]; outp[1] = inp[6]; outp[2] = inp[5]; outp[3] = inp[4]; outp[4] = inp[3]; outp[5] = inp[2]; outp[6] = inp[1]; outp[7] = inp[0];
#else
  memcpy((void *)outp, (void *)inp, 8);
#endif
  return(o);
}


void mus_bfloat_to_char(unsigned char *j, float x)
{
  unsigned char *ox = (unsigned char *)&x;
#if MUS_LITTLE_ENDIAN
  j[0] = ox[3]; j[1] = ox[2]; j[2] = ox[1]; j[3] = ox[0];
#else
  memcpy((void *)j, (void *)ox, 4);
#endif
}


float mus_char_to_bfloat(const unsigned char *inp)
{
  float o;
  unsigned char *outp = (unsigned char *)&o;
#if MUS_LITTLE_ENDIAN
  outp[0] = inp[3]; outp[1] = inp[2]; outp[2] = inp[1]; outp[3] = inp[0];
#else
  memcpy((void *)outp, (void *)inp, 4);
#endif
  return(o);
}


void mus_lfloat_to_char(unsigned char *j, float x)
{
  unsigned char *ox = (unsigned char *)&x;
#if (!MUS_LITTLE_ENDIAN)
  j[0] = ox[3]; j[1] = ox[2]; j[2] = ox[1]; j[3] = ox[0];
#else
  memcpy((void *)j, (void *)ox, 4);
#endif
}


float mus_char_to_lfloat(const unsigned char *inp)
{
  float o;
  unsigned char *outp = (unsigned char *)&o;
#if (!MUS_LITTLE_ENDIAN)
  outp[0] = inp[3]; outp[1] = inp[2]; outp[2] = inp[1]; outp[3] = inp[0];
#else
  memcpy((void *)outp, (void *)inp, 4);
#endif
  return(o);
}


void mus_bshort_to_char(unsigned char *j, short x)
{
  unsigned char *ox = (unsigned char *)&x;
#if MUS_LITTLE_ENDIAN
  j[0] = ox[1]; j[1] = ox[0];
#else
  memcpy((void *)j, (void *)ox, 2); /* I wonder if this is faster */
#endif
}


short mus_char_to_bshort(const unsigned char *inp)
{
  short o;
  unsigned char *outp = (unsigned char *)&o;
#if MUS_LITTLE_ENDIAN
  outp[0] = inp[1]; outp[1] = inp[0];
#else
  memcpy((void *)outp, (void *)inp, 2);
#endif
  return(o);
}


void mus_lshort_to_char(unsigned char *j, short x)
{
  unsigned char *ox = (unsigned char *)&x;
#if (!MUS_LITTLE_ENDIAN)
  j[0] = ox[1]; j[1] = ox[0];
#else
  memcpy((void *)j, (void *)ox, 2);
#endif
}


short mus_char_to_lshort(const unsigned char *inp)
{
  short o;
  unsigned char *outp = (unsigned char *)&o;
#if (!MUS_LITTLE_ENDIAN)
  outp[0] = inp[1]; outp[1] = inp[0];
#else
  memcpy((void *)outp, (void *)inp, 2);
#endif
  return(o);
}


#if ((MUS_LITTLE_ENDIAN) && (!HAVE_BYTESWAP_H)) || ((!MUS_LITTLE_ENDIAN) && (MUS_SUN))
static void mus_ubshort_to_char(unsigned char *j, unsigned short x)
{
  unsigned char *ox = (unsigned char *)&x;
#if MUS_LITTLE_ENDIAN
  j[0] = ox[1]; j[1] = ox[0];
#else
  memcpy((void *)j, (void *)ox, 2);
#endif
}
#endif


unsigned short mus_char_to_ubshort(const unsigned char *inp)
{
  unsigned short o;
  unsigned char *outp = (unsigned char *)&o;
#if MUS_LITTLE_ENDIAN
  outp[0] = inp[1]; outp[1] = inp[0];
#else
  memcpy((void *)outp, (void *)inp, 2);
#endif
  return(o);
}


#if (!MUS_LITTLE_ENDIAN) && (!HAVE_BYTESWAP_H)
static void mus_ulshort_to_char(unsigned char *j, unsigned short x)
{
  unsigned char *ox = (unsigned char *)&x;
#if (!MUS_LITTLE_ENDIAN)
  j[0] = ox[1]; j[1] = ox[0];
#else
  memcpy((void *)j, (void *)ox, 2);
#endif
}
#endif


unsigned short mus_char_to_ulshort(const unsigned char *inp)
{
  unsigned short o;
  unsigned char *outp = (unsigned char *)&o;
#if (!MUS_LITTLE_ENDIAN)
  outp[0] = inp[1]; outp[1] = inp[0];
#else
  memcpy((void *)outp, (void *)inp, 2);
#endif
  return(o);
}


void mus_bdouble_to_char(unsigned char *j, double x)
{
  unsigned char *ox = (unsigned char *)&x;
#if (MUS_LITTLE_ENDIAN)
  j[0] = ox[7]; j[1] = ox[6]; j[2] = ox[5]; j[3] = ox[4]; j[4] = ox[3]; j[5] = ox[2]; j[6] = ox[1]; j[7] = ox[0];
#else
  memcpy((void *)j, (void *)ox, 8);
#endif
}


double mus_char_to_ldouble(const unsigned char *inp)
{
  double o;
  unsigned char *outp = (unsigned char *)&o;
#if (MUS_LITTLE_ENDIAN)
  memcpy((void *)outp, (void *)inp, 8);
#else
  outp[0] = inp[7]; outp[1] = inp[6]; outp[2] = inp[5]; outp[3] = inp[4]; outp[4] = inp[3]; outp[5] = inp[2]; outp[6] = inp[1]; outp[7] = inp[0];
#endif
  return(o);
}


#if (!MUS_LITTLE_ENDIAN)
static void mus_ldouble_to_char(unsigned char *j, double x)
{
  unsigned char *ox = (unsigned char *)&x;
#if (MUS_LITTLE_ENDIAN)
  memcpy((void *)j, (void *)ox, 8);
#else
  j[0] = ox[7]; j[1] = ox[6]; j[2] = ox[5]; j[3] = ox[4]; j[4] = ox[3]; j[5] = ox[2]; j[6] = ox[1]; j[7] = ox[0];
#endif
}
#endif


double mus_char_to_bdouble(const unsigned char *inp)
{
  double o;
  unsigned char *outp = (unsigned char *)&o;
#if (MUS_LITTLE_ENDIAN)
  outp[0] = inp[7]; outp[1] = inp[6]; outp[2] = inp[5]; outp[3] = inp[4]; outp[4] = inp[3]; outp[5] = inp[2]; outp[6] = inp[1]; outp[7] = inp[0];
#else
  memcpy((void *)outp, (void *)inp, 8);
#endif
  return(o);
}


int mus_char_to_uninterpreted_int(const unsigned char *inp)
{
  int o;
  unsigned char *outp = (unsigned char *)&o;
  memcpy((void *)outp, (void *)inp, 4);
  return(o);
}


unsigned int mus_char_to_ubint(const unsigned char *inp)
{
  unsigned int o;
  unsigned char *outp = (unsigned char *)&o;
#if MUS_LITTLE_ENDIAN
  outp[0] = inp[3]; outp[1] = inp[2]; outp[2] = inp[1]; outp[3] = inp[0];
#else
  memcpy((void *)outp, (void *)inp, 4);
#endif
  return(o);
}


unsigned int mus_char_to_ulint(const unsigned char *inp)
{
  unsigned int o;
  unsigned char *outp = (unsigned char *)&o;
#if (!MUS_LITTLE_ENDIAN)
  outp[0] = inp[3]; outp[1] = inp[2]; outp[2] = inp[1]; outp[3] = inp[0];
#else
  memcpy((void *)outp, (void *)inp, 4);
#endif
  return(o);
}


#if HAVE_BYTESWAP_H
  #include <byteswap.h>
#endif

#if MUS_LITTLE_ENDIAN

  #if HAVE_BYTESWAP_H
    #define big_endian_short(n)                  ((short)(bswap_16((*((unsigned short *)n)))))
    #define big_endian_int(n)                    ((int)(bswap_32((*((unsigned int *)n)))))
    #define big_endian_unsigned_short(n)         ((unsigned short)(bswap_16((*((unsigned short *)n)))))
  #else
    #define big_endian_short(n)                  (mus_char_to_bshort(n))
    #define big_endian_int(n)                    (mus_char_to_bint(n))
    #define big_endian_unsigned_short(n)         (mus_char_to_ubshort(n))
  #endif

  #define big_endian_float(n)                    (mus_char_to_bfloat(n))
  #define big_endian_double(n)                   (mus_char_to_bdouble(n))

  #define little_endian_short(n)                 (*((short *)n))
  #define little_endian_int(n)                   (*((int *)n))
  #define little_endian_float(n)                 (*((float *)n))
  #define little_endian_double(n)                (*((double *)n))
  #define little_endian_unsigned_short(n)        (*((unsigned short *)n))

  #if HAVE_BYTESWAP_H
    #define set_big_endian_short(n, x)           (*((short *)n)) = ((short)(bswap_16(x)))
    #define set_big_endian_int(n, x)             (*((int *)n)) = ((int)(bswap_32(x)))
    #define set_big_endian_unsigned_short(n, x)  (*((unsigned short *)n)) = ((unsigned short)(bswap_16(x)))
  #else
    #define set_big_endian_short(n, x)           mus_bshort_to_char(n, x)
    #define set_big_endian_int(n, x)             mus_bint_to_char(n, x)
    #define set_big_endian_unsigned_short(n, x)  mus_ubshort_to_char(n, x)
  #endif

  #define set_big_endian_float(n, x)             mus_bfloat_to_char(n, x)
  #define set_big_endian_double(n, x)            mus_bdouble_to_char(n, x)

  #define set_little_endian_short(n, x)          (*((short *)n)) = x
  #define set_little_endian_int(n, x)            (*((int *)n)) = x
  #define set_little_endian_float(n, x)          (*((float *)n)) = x
  #define set_little_endian_double(n, x)         (*((double *)n)) = x
  #define set_little_endian_unsigned_short(n, x) (*((unsigned short *)n)) = x

#else

#if (!MUS_SUN)
    #define big_endian_short(n)                  (*((short *)n))
    #define big_endian_int(n)                    (*((int *)n))
    #define big_endian_float(n)                  (*((float *)n))
    #define big_endian_double(n)                 (*((double *)n))
    #define big_endian_unsigned_short(n)         (*((unsigned short *)n))

    #define set_big_endian_short(n, x)           (*((short *)n)) = x
    #define set_big_endian_int(n, x)             (*((int *)n)) = x
    #define set_big_endian_float(n, x)           (*((float *)n)) = x
    #define set_big_endian_double(n, x)          (*((double *)n)) = x
    #define set_big_endian_unsigned_short(n, x)  (*((unsigned short *)n)) = x
  #else
    #define big_endian_short(n)                  (mus_char_to_bshort(n))
    #define big_endian_int(n)                    (mus_char_to_bint(n))
    #define big_endian_float(n)                  (mus_char_to_bfloat(n))
    #define big_endian_double(n)                 (mus_char_to_bdouble(n))
    #define big_endian_unsigned_short(n)         (mus_char_to_ubshort(n))

    #define set_big_endian_short(n, x)           mus_bshort_to_char(n, x)
    #define set_big_endian_int(n, x)             mus_bint_to_char(n, x)
    #define set_big_endian_float(n, x)           mus_bfloat_to_char(n, x)
    #define set_big_endian_double(n, x)          mus_bdouble_to_char(n, x)
    #define set_big_endian_unsigned_short(n, x)  mus_ubshort_to_char(n, x)
  #endif

  #if HAVE_BYTESWAP_H
    #define little_endian_short(n)               ((short)(bswap_16((*((unsigned short *)n)))))
    #define little_endian_int(n)                 ((int)(bswap_32((*((unsigned int *)n)))))
    #define little_endian_unsigned_short(n)      ((unsigned short)(bswap_16((*((unsigned short *)n)))))
  #else
    #define little_endian_short(n)               (mus_char_to_lshort(n))
    #define little_endian_int(n)                 (mus_char_to_lint(n))
    #define little_endian_unsigned_short(n)      (mus_char_to_ulshort(n))
  #endif
  #define little_endian_float(n)                 (mus_char_to_lfloat(n))
  #define little_endian_double(n)                (mus_char_to_ldouble(n))

  #if HAVE_BYTESWAP_H
    #define set_little_endian_short(n, x)        (*((short *)n)) = ((short)(bswap_16(x)))
    #define set_little_endian_int(n, x)          (*((int *)n)) = ((int)(bswap_32(x)))
    #define set_little_endian_unsigned_short(n, x) (*((unsigned short *)n)) = ((unsigned short)(bswap_16(x)))
  #else
    #define set_little_endian_short(n, x)        mus_lshort_to_char(n, x)
    #define set_little_endian_int(n, x)          mus_lint_to_char(n, x)
    #define set_little_endian_unsigned_short(n, x) mus_ulshort_to_char(n, x)
  #endif
  #define set_little_endian_float(n, x)          mus_lfloat_to_char(n, x)
  #define set_little_endian_double(n, x)         mus_ldouble_to_char(n, x)

#endif


/* ---------------- file descriptors ---------------- */

static bool clipping_default = false;
bool mus_clipping(void) {return(clipping_default);}
bool mus_set_clipping(bool new_value) {clipping_default = new_value; return(new_value);}


static mus_float_t prescaler_default = 1.0;
mus_float_t mus_prescaler(void) {return(prescaler_default);}
mus_float_t mus_set_prescaler(mus_float_t new_value) {prescaler_default = new_value; return(new_value);}


typedef struct {
  char *name;
  int data_format, bytes_per_sample, chans, header_type;
  bool clipping;
  mus_long_t data_location;
  mus_float_t prescaler;
} io_fd;

static int io_fd_size = 0;
static io_fd **io_fds = NULL;
#define IO_FD_ALLOC_SIZE 8

#if HAVE_PTHREADS
  static mus_lock_t io_table_lock = MUS_LOCK_INITIALIZER;
#endif


int mus_file_open_descriptors(int tfd, const char *name, int format, int size /* datum size */, mus_long_t location, int chans, int type)
{
  int err = MUS_NO_ERROR;

  MUS_LOCK(&io_table_lock);

  if (io_fd_size == 0)
    {
      io_fd_size = tfd + IO_FD_ALLOC_SIZE;
      io_fds = (io_fd **)calloc(io_fd_size, sizeof(io_fd *));
    }

  if (io_fds)
    {
      if (io_fd_size <= tfd)
	{
	  int i, lim;
	  lim = io_fd_size;
	  io_fd_size = tfd + IO_FD_ALLOC_SIZE;
	  io_fds = (io_fd **)realloc(io_fds, io_fd_size * sizeof(io_fd *));
	  for (i = lim; i < io_fd_size; i++) io_fds[i] = NULL;
	}

      if (io_fds[tfd] == NULL)
	io_fds[tfd] = (io_fd *)calloc(1, sizeof(io_fd));

      if (io_fds[tfd])
	{
	  io_fd *fd;
	  fd = io_fds[tfd];
	  fd->data_format = format;
	  fd->bytes_per_sample = size;
	  fd->data_location = location;
	  fd->clipping = clipping_default;
	  fd->prescaler = prescaler_default;
	  fd->header_type = type;
	  fd->chans = chans;
	  if (name)
	    {
	      fd->name = (char *)calloc(strlen(name) + 1, sizeof(char));
	      strcpy(fd->name, name);
	    }
	}
      else err = MUS_MEMORY_ALLOCATION_FAILED;
    }
  else err = MUS_MEMORY_ALLOCATION_FAILED;

  MUS_UNLOCK(&io_table_lock);

  return(err);
}


bool mus_file_clipping(int tfd)
{
  io_fd *fd;
  if ((io_fds == NULL) || (tfd >= io_fd_size) || (tfd < 0) || (io_fds[tfd] == NULL)) return(false);
  fd = io_fds[tfd];
  return(fd->clipping);
}


int mus_file_set_clipping(int tfd, bool clipped)
{
  io_fd *fd;
  if ((io_fds == NULL) || (tfd >= io_fd_size) || (tfd < 0) || (io_fds[tfd] == NULL)) return(MUS_FILE_DESCRIPTORS_NOT_INITIALIZED);
  fd = io_fds[tfd];
  fd->clipping = clipped;
  return(MUS_NO_ERROR);
}


int mus_file_set_header_type(int tfd, int type)
{
  io_fd *fd;
  if ((io_fds == NULL) || (tfd >= io_fd_size) || (tfd < 0) || (io_fds[tfd] == NULL)) return(MUS_FILE_DESCRIPTORS_NOT_INITIALIZED);
  fd = io_fds[tfd];
  fd->header_type = type;
  return(MUS_NO_ERROR);
}


int mus_file_header_type(int tfd)
{
  io_fd *fd;
  if ((io_fds == NULL) || (tfd >= io_fd_size) || (tfd < 0) || (io_fds[tfd] == NULL)) return(MUS_FILE_DESCRIPTORS_NOT_INITIALIZED);
  fd = io_fds[tfd];
  return(fd->header_type);
}


mus_float_t mus_file_prescaler(int tfd) 
{
  io_fd *fd;
  if ((io_fds == NULL) || (tfd >= io_fd_size) || (tfd < 0) || (io_fds[tfd] == NULL)) return(0.0);
  fd = io_fds[tfd];
  return(fd->prescaler);
}


mus_float_t mus_file_set_prescaler(int tfd, mus_float_t val) 
{
  io_fd *fd;
  if ((io_fds == NULL) || (tfd >= io_fd_size) || (tfd < 0) || (io_fds[tfd] == NULL)) return(0.0);
  fd = io_fds[tfd];
  fd->prescaler = val; 
  return(val);
}


char *mus_file_fd_name(int tfd)
{
  io_fd *fd;
  if ((io_fds == NULL) || (tfd >= io_fd_size) || (tfd < 0) || (io_fds[tfd] == NULL)) return(NULL);
  fd = io_fds[tfd];
  return(fd->name);
}


int mus_file_set_chans(int tfd, int chans)
{
  io_fd *fd;
  if ((io_fds == NULL) || (tfd >= io_fd_size) || (tfd < 0) || (io_fds[tfd] == NULL)) return(MUS_FILE_DESCRIPTORS_NOT_INITIALIZED);
  fd = io_fds[tfd];
  fd->chans = chans;
  return(MUS_NO_ERROR);
}


/* ---------------- open, creat, close ---------------- */

int mus_file_open_read(const char *arg) 
{
  int fd;
#ifdef MUS_WINDOZE
  fd = OPEN(arg, O_RDONLY | O_BINARY, 0);
#else
  fd = OPEN(arg, O_RDONLY, 0);
#endif
  return(fd);
}


bool mus_file_probe(const char *arg) 
{
#if HAVE_ACCESS
  return(access(arg, F_OK) == 0);
#else
  int fd;
#ifdef O_NONBLOCK
  fd = OPEN(arg, O_RDONLY, O_NONBLOCK);
#else
  fd = OPEN(arg, O_RDONLY, 0);
#endif
  if (fd == -1) return(false);
  CLOSE(fd, arg);
  return(true);
#endif
}


int mus_file_open_write(const char *arg)
{
  int fd;
#ifdef MUS_WINDOZE
  if ((fd = OPEN(arg, O_RDWR | O_BINARY, 0)) == -1)
    fd = CREAT(arg, S_IREAD | S_IWRITE);               /* 0x0100 | 0x0080 */
#else
  if ((fd = OPEN(arg, O_RDWR, 0)) == -1)
    fd = CREAT(arg, 0666);  /* equivalent to the new open(arg, O_RDWR | O_CREAT | O_TRUNC, 0666) */
#endif
  else lseek(fd, 0L, SEEK_END);
  return(fd);
}


int mus_file_create(const char *arg) 
{ 
#ifdef MUS_WINDOZE 
  return(CREAT(arg, S_IREAD | S_IWRITE)); 
#else 
  return(CREAT(arg, 0666)); 
#endif 
} 


int mus_file_reopen_write(const char *arg)
{
  int fd;
#ifdef MUS_WINDOZE
  fd = OPEN(arg, O_RDWR | O_BINARY, 0);
#else
  fd = OPEN(arg, O_RDWR, 0);
#endif
  return(fd);
}


int mus_file_close(int fd)
{
  io_fd *fdp;
  int close_result = 0;

  if ((io_fds == NULL) || (fd >= io_fd_size) || (fd < 0) || (io_fds[fd] == NULL)) return(MUS_FILE_DESCRIPTORS_NOT_INITIALIZED);

  MUS_LOCK(&io_table_lock);

  fdp = io_fds[fd];

#if USE_SND
  CLOSE(fd, fdp->name);
#else
  close_result = close(fd);
#endif

  if (fdp->name) {free(fdp->name); fdp->name = NULL;}
  free(fdp);
  io_fds[fd] = NULL;

  MUS_UNLOCK(&io_table_lock);

  if (close_result < 0)
    return(MUS_CANT_CLOSE_FILE);
  return(MUS_NO_ERROR);
}



/* ---------------- seek ---------------- */

mus_long_t mus_file_seek_frame(int tfd, mus_long_t frame)
{
  io_fd *fd;
  if (io_fds == NULL) 
    return(mus_error(MUS_FILE_DESCRIPTORS_NOT_INITIALIZED, "mus_file_seek_frame: no file descriptors!"));

  if (tfd >= io_fd_size)
    return(mus_error(MUS_FILE_DESCRIPTORS_NOT_INITIALIZED,
		     "mus_file_seek_frame: file descriptors not realloc'd? (tfd: %d, io_fd_size: %d)", tfd, io_fd_size));

  if ((tfd < 0) || 
      (io_fds[tfd] == NULL))
    return(mus_error(MUS_FILE_DESCRIPTORS_NOT_INITIALIZED, "mus_file_seek_frame: file descriptor = %d?", tfd));

  fd = io_fds[tfd];
  if (fd->data_format == MUS_UNKNOWN) 
    return(mus_error(MUS_NOT_A_SOUND_FILE, "mus_file_seek_frame: invalid data format for %s", fd->name));

  return(lseek(tfd, fd->data_location + (fd->chans * frame * fd->bytes_per_sample), SEEK_SET));
}



/* ---------------- mulaw/alaw conversions ----------------
 *
 *      x : input signal with max value 32767
 *     mu : compression parameter (mu = 255 used for telephony)
 *     y = (32767/log(1+mu))*log(1+mu*abs(x)/32767)*sign(x); -- this isn't right -- typo?
 */

/* from sox g711.c */
#define	QUANT_MASK	(0xf)		/* Quantization field mask. */
#define	SEG_SHIFT	(4)		/* Left shift for segment number. */

static short seg_end[8] = {0xFF, 0x1FF, 0x3FF, 0x7FF,  0xFFF, 0x1FFF, 0x3FFF, 0x7FFF};

static int search(int val, short *table, int size)
{
  int i;
  for (i = 0; i < size; i++) {if (val <= *table++) return(i);}
  return(size);
}

static unsigned char to_alaw(int pcm_val)
{
  int mask, seg;
  if (pcm_val >= 0) mask = 0xD5; else {mask = 0x55; pcm_val = -pcm_val - 8;}
  seg = search(pcm_val, seg_end, 8);
  if (seg >= 8)	return(0x7F ^ mask);
  else 
    {
      unsigned char aval;
      aval = seg << SEG_SHIFT;
      if (seg < 2) aval |= (pcm_val >> 4) & QUANT_MASK; else aval |= (pcm_val >> (seg + 3)) & QUANT_MASK;
      return(aval ^ mask);
    }
}

static const int alaw[256] = {
 -5504, -5248, -6016, -5760, -4480, -4224, -4992, -4736, -7552, -7296, -8064, -7808, -6528, -6272, -7040, -6784, 
 -2752, -2624, -3008, -2880, -2240, -2112, -2496, -2368, -3776, -3648, -4032, -3904, -3264, -3136, -3520, -3392, 
 -22016, -20992, -24064, -23040, -17920, -16896, -19968, -18944, -30208, -29184, -32256, -31232, -26112, -25088, -28160, -27136, 
 -11008, -10496, -12032, -11520, -8960, -8448, -9984, -9472, -15104, -14592, -16128, -15616, -13056, -12544, -14080, -13568, 
 -344, -328, -376, -360, -280, -264, -312, -296, -472, -456, -504, -488, -408, -392, -440, -424, 
 -88, -72, -120, -104, -24, -8, -56, -40, -216, -200, -248, -232, -152, -136, -184, -168, 
 -1376, -1312, -1504, -1440, -1120, -1056, -1248, -1184, -1888, -1824, -2016, -1952, -1632, -1568, -1760, -1696, 
 -688, -656, -752, -720, -560, -528, -624, -592, -944, -912, -1008, -976, -816, -784, -880, -848, 
 5504, 5248, 6016, 5760, 4480, 4224, 4992, 4736, 7552, 7296, 8064, 7808, 6528, 6272, 7040, 6784, 
 2752, 2624, 3008, 2880, 2240, 2112, 2496, 2368, 3776, 3648, 4032, 3904, 3264, 3136, 3520, 3392, 
 22016, 20992, 24064, 23040, 17920, 16896, 19968, 18944, 30208, 29184, 32256, 31232, 26112, 25088, 28160, 27136, 
 11008, 10496, 12032, 11520, 8960, 8448, 9984, 9472, 15104, 14592, 16128, 15616, 13056, 12544, 14080, 13568, 
 344, 328, 376, 360, 280, 264, 312, 296, 472, 456, 504, 488, 408, 392, 440, 424, 
 88, 72, 120, 104, 24, 8, 56, 40, 216, 200, 248, 232, 152, 136, 184, 168, 
 1376, 1312, 1504, 1440, 1120, 1056, 1248, 1184, 1888, 1824, 2016, 1952, 1632, 1568, 1760, 1696, 
 688, 656, 752, 720, 560, 528, 624, 592, 944, 912, 1008, 976, 816, 784, 880, 848
};

#define	BIAS		(0x84)		/* Bias for linear code. */

static unsigned char to_mulaw(int pcm_val)
{
  int mask;
  int seg;
  if (pcm_val < 0) {pcm_val = BIAS - pcm_val; mask = 0x7F;} else {pcm_val += BIAS; mask = 0xFF;}
  seg = search(pcm_val, seg_end, 8);
  if (seg >= 8) return(0x7F ^ mask);
  else 
    {
      unsigned char uval;
      uval = (seg << 4) | ((pcm_val >> (seg + 3)) & 0xF);
      return(uval ^ mask);
    }
}

/* generated by SNDiMulaw on a NeXT */
static const int mulaw[256] = {
  -32124, -31100, -30076, -29052, -28028, -27004, -25980, -24956, -23932, -22908, -21884, -20860, 
  -19836, -18812, -17788, -16764, -15996, -15484, -14972, -14460, -13948, -13436, -12924, -12412, 
  -11900, -11388, -10876, -10364, -9852, -9340, -8828, -8316, -7932, -7676, -7420, -7164, -6908, 
  -6652, -6396, -6140, -5884, -5628, -5372, -5116, -4860, -4604, -4348, -4092, -3900, -3772, -3644, 
  -3516, -3388, -3260, -3132, -3004, -2876, -2748, -2620, -2492, -2364, -2236, -2108, -1980, -1884, 
  -1820, -1756, -1692, -1628, -1564, -1500, -1436, -1372, -1308, -1244, -1180, -1116, -1052, -988, 
  -924, -876, -844, -812, -780, -748, -716, -684, -652, -620, -588, -556, -524, -492, -460, -428, 
  -396, -372, -356, -340, -324, -308, -292, -276, -260, -244, -228, -212, -196, -180, -164, -148, 
  -132, -120, -112, -104, -96, -88, -80, -72, -64, -56, -48, -40, -32, -24, -16, -8, 0, 32124, 31100, 
  30076, 29052, 28028, 27004, 25980, 24956, 23932, 22908, 21884, 20860, 19836, 18812, 17788, 16764, 
  15996, 15484, 14972, 14460, 13948, 13436, 12924, 12412, 11900, 11388, 10876, 10364, 9852, 9340, 
  8828, 8316, 7932, 7676, 7420, 7164, 6908, 6652, 6396, 6140, 5884, 5628, 5372, 5116, 4860, 4604, 
  4348, 4092, 3900, 3772, 3644, 3516, 3388, 3260, 3132, 3004, 2876, 2748, 2620, 2492, 2364, 2236, 
  2108, 1980, 1884, 1820, 1756, 1692, 1628, 1564, 1500, 1436, 1372, 1308, 1244, 1180, 1116, 1052, 
  988, 924, 876, 844, 812, 780, 748, 716, 684, 652, 620, 588, 556, 524, 492, 460, 428, 396, 372, 
  356, 340, 324, 308, 292, 276, 260, 244, 228, 212, 196, 180, 164, 148, 132, 120, 112, 104, 96, 
  88, 80, 72, 64, 56, 48, 40, 32, 24, 16, 8, 0};



/* ---------------- read ---------------- */

#define BUFLIM (64 * 1024)
#define UBYTE_ZERO 128
#define USHORT_ZERO 32768

#if SNDLIB_USE_FLOATS
  #define MUS_SAMPLE_UNSCALED(n) ((n) / 32768.0)
  /* see note in _sndlib.h" values are "unscaled" from the DAC's point of view */
#else
  #define MUS_SAMPLE_UNSCALED(n) ((n) * (1 << (MUS_SAMPLE_BITS - 16)))
#endif


static mus_long_t mus_read_any_1(int tfd, mus_long_t beg, int chans, mus_long_t nints, mus_sample_t **bufs, mus_sample_t **cm, char *inbuf)
{
  int format, siz, siz_chans;
  mus_long_t bytes, j, lim, leftover, total_read, k, loc, oldloc, buflim;
  unsigned char *jchar;
  char *charbuf = NULL;
  mus_sample_t *buffer;
  float prescaling;

  if (nints <= 0) return(0);

  if (!inbuf)
    {
      io_fd *fd;

      if ((io_fds == NULL) || (tfd >= io_fd_size) || (tfd < 0) || (io_fds[tfd] == NULL))
	return(mus_error(MUS_FILE_DESCRIPTORS_NOT_INITIALIZED, "mus_read: no file descriptors!"));

      fd = io_fds[tfd];
      if (fd->data_format == MUS_UNKNOWN) 
	return(mus_error(MUS_FILE_CLOSED, "mus_read: invalid data format for %s", fd->name));

      format = fd->data_format;
      siz = fd->bytes_per_sample;
      if ((format == MUS_OUT_FORMAT) && 
	  (chans == 1) && 
	  (beg == 0)
#if SNDLIB_USE_FLOATS 
	  && (fd->prescaler == 1.0)
#endif
	  )
	{
	  ssize_t total;

	  bytes = nints * siz;
	  total = read(tfd, (char *)(bufs[0]), bytes);
	  if (total != bytes)
	    {
	      if (total <= 0)
		memset((void *)(bufs[0]), 0, bytes);
	      else
		{
		  int i, last;
		  last = beg + nints;
		  for (i = total / siz; i < last; i++)
		    bufs[0][i] = MUS_SAMPLE_0;
		}
	    }
	  return(total / siz);
	}

      prescaling = (float)(fd->prescaler * MUS_FLOAT_TO_SAMPLE(1.0));
      /* not MUS_FLOAT_TO_SAMPLE(fd->prescaler) here because there's a possible cast to int which can overflow */

      charbuf = (char *)calloc(BUFLIM, sizeof(char)); 
      if (charbuf == NULL) 
	return(mus_error(MUS_MEMORY_ALLOCATION_FAILED, "mus_read: IO buffer allocation failed"));
    }
  else
    {
      charbuf = inbuf;
      siz = mus_bytes_per_sample(tfd);
      prescaling = (float)(MUS_FLOAT_TO_SAMPLE(1.0));
      format = tfd;
    }

  siz_chans = siz * chans;
  leftover = (nints * siz_chans);
  k = (BUFLIM) % siz_chans;
  if (k != 0) /* for example, 3 channel output of 1-byte (mulaw) samples will need a mod 3 buffer */
    buflim = (BUFLIM) - k;
  else buflim = BUFLIM;
  total_read = 0;
  loc = beg;

  while (leftover > 0)
    {
      bytes = leftover;
      if (bytes > buflim) 
	{
	  leftover = (bytes - buflim); 
	  bytes = buflim;
	} 
      else leftover = 0;
      if (!inbuf)
	{
	  ssize_t total;

	  total = read(tfd, charbuf, bytes); 
	  if (total <= 0) 
	    {
	      /* zero out trailing section (some callers don't check the returned value) -- this added 9-May-99 */

	      lim = beg + nints;
	      if (loc < lim)
		for (k = 0; k < chans; k++)
		  if ((cm == NULL) || (cm[k]))
		    {
		      if (loc == 0)
			memset((void *)(bufs[k]), 0, lim * sizeof(mus_sample_t));
		      else
			for (j = loc; j < lim; j++) 
			  bufs[k][j] = MUS_SAMPLE_0;
		    }
	      free(charbuf);
	      return(total_read);
	    }
	  lim = (int) (total / siz_chans);  /* this divide must be exact (hence the buflim calc above) */
	}
      else
	{
	  lim = nints; /* frames in this case */
	  leftover = 0;
	}
      total_read += lim;
      oldloc = loc;

      for (k = 0; k < chans; k++)
	{
	  if ((cm == NULL) || (cm[k]))
	    {
	      buffer = (mus_sample_t *)(bufs[k]);
	      if (buffer)
		{
		  mus_long_t loclim;

		  loc = oldloc;
		  loclim = loc + lim;
		  jchar = (unsigned char *)charbuf;
		  jchar += (k * siz);
		  switch (format)
		    {
		    case MUS_BSHORT:               
		      for (; loc < loclim; loc++, jchar += siz_chans) 
			buffer[loc] = MUS_SHORT_TO_SAMPLE(big_endian_short(jchar)); 
		      break;

		    case MUS_LSHORT: 
		      for (; loc < loclim; loc++, jchar += siz_chans) 
			buffer[loc] = MUS_SHORT_TO_SAMPLE(little_endian_short(jchar)); 
		      break;

		    case MUS_BINT:              
		      for (; loc < loclim; loc++, jchar += siz_chans) 
			buffer[loc] = MUS_INT_TO_SAMPLE(big_endian_int(jchar)); 
		      break;

		    case MUS_LINT: 
		      for (; loc < loclim; loc++, jchar += siz_chans) 
			buffer[loc] = MUS_INT_TO_SAMPLE(little_endian_int(jchar)); 
		      break;

		    case MUS_BINTN:              
		      for (; loc < loclim; loc++, jchar += siz_chans) 
			buffer[loc] = MUS_INT_TO_SAMPLE((big_endian_int(jchar) >> (32 - MUS_SAMPLE_BITS)));
		      break;

		    case MUS_LINTN: 
		      for (; loc < loclim; loc++, jchar += siz_chans) 
			buffer[loc] = MUS_INT_TO_SAMPLE((little_endian_int(jchar) >> (32 - MUS_SAMPLE_BITS)));
		      break;

		    case MUS_MULAW:  	              
		      for (; loc < loclim; loc++, jchar += siz_chans) 
			buffer[loc] = MUS_SHORT_TO_SAMPLE(mulaw[*jchar]); 
		      break;

		    case MUS_ALAW:                  
		      for (; loc < loclim; loc++, jchar += siz_chans) 
			buffer[loc] = MUS_SHORT_TO_SAMPLE(alaw[*jchar]); 
		      break;

		    case MUS_BYTE:                
		      for (; loc < loclim; loc++, jchar += siz_chans)
			buffer[loc] = MUS_BYTE_TO_SAMPLE((signed char)(*jchar));
		      break;

		    case MUS_UBYTE:     	      
		      for (; loc < loclim; loc++, jchar += siz_chans) 
			buffer[loc] = MUS_BYTE_TO_SAMPLE((int)(*jchar) - UBYTE_ZERO);
		      break;

		    case MUS_BFLOAT:
		      if (prescaling == 1.0)
			{
			  for (; loc < loclim; loc++, jchar += siz_chans) 
			    buffer[loc] = (mus_sample_t) (big_endian_float(jchar));
			}
		      else
			{
			  for (; loc < loclim; loc++, jchar += siz_chans) 
			    buffer[loc] = (mus_sample_t) (prescaling * (big_endian_float(jchar)));
			}
		      break;

		    case MUS_BFLOAT_UNSCALED:
		      for (; loc < loclim; loc++, jchar += siz_chans) 
			buffer[loc] = (mus_sample_t) (MUS_SAMPLE_UNSCALED(big_endian_float(jchar)));
		      break;

		    case MUS_BDOUBLE:   
		      for (; loc < loclim; loc++, jchar += siz_chans)
			buffer[loc] = (mus_sample_t) (prescaling * (big_endian_double(jchar)));
		      break;

		    case MUS_BDOUBLE_UNSCALED:   
		      for (; loc < loclim; loc++, jchar += siz_chans)
			buffer[loc] = (mus_sample_t) (MUS_SAMPLE_UNSCALED(big_endian_double(jchar)));
		      break;

		    case MUS_LFLOAT:
		      if (prescaling == 1.0)
			{
			  for (; loc < loclim; loc++, jchar += siz_chans) 
			    buffer[loc] = (mus_sample_t) (little_endian_float(jchar));
			}
		      else
			{
			  for (; loc < loclim; loc++, jchar += siz_chans) 
			    buffer[loc] = (mus_sample_t) (prescaling * (little_endian_float(jchar)));
			}
		      break;

		    case MUS_LFLOAT_UNSCALED:    
		      for (; loc < loclim; loc++, jchar += siz_chans) 
			buffer[loc] = (mus_sample_t) (MUS_SAMPLE_UNSCALED(little_endian_float(jchar)));
		      break;

		    case MUS_LDOUBLE:   
		      for (; loc < loclim; loc++, jchar += siz_chans) 
			buffer[loc] = (mus_sample_t) (prescaling * (little_endian_double(jchar)));
		      break;

		    case MUS_LDOUBLE_UNSCALED:   
		      for (; loc < loclim; loc++, jchar += siz_chans) 
			buffer[loc] = (mus_sample_t) (MUS_SAMPLE_UNSCALED(little_endian_double(jchar)));
		      break;

		    case MUS_UBSHORT:   
		      for (; loc < loclim; loc++, jchar += siz_chans) 
			buffer[loc] = MUS_SHORT_TO_SAMPLE((int)(big_endian_unsigned_short(jchar)) - USHORT_ZERO);
		      break;

		    case MUS_ULSHORT:   
		      for (; loc < loclim; loc++, jchar += siz_chans) 
			buffer[loc] = MUS_SHORT_TO_SAMPLE((int)(little_endian_unsigned_short(jchar)) - USHORT_ZERO);
		      break;

		    case MUS_B24INT:
		      for (; loc < loclim; loc++, jchar += siz_chans) 
			buffer[loc] = MUS_INT24_TO_SAMPLE((int)(((jchar[0] << 24) + 
								 (jchar[1] << 16) + 
								 (jchar[2] << 8)) >> 8));
		      break;

		    case MUS_L24INT:   
		      for (; loc < loclim; loc++, jchar += siz_chans) 
			buffer[loc] = MUS_INT24_TO_SAMPLE((int)(((jchar[2] << 24) + 
								 (jchar[1] << 16) + 
								 (jchar[0] << 8)) >> 8));
		      break;
		    }
		}
	    }
	}
    }
  if (!inbuf) free(charbuf);
  return(total_read);
}


mus_long_t mus_file_read_any(int tfd, mus_long_t beg, int chans, mus_long_t nints, mus_sample_t **bufs, mus_sample_t **cm)
{
  return(mus_read_any_1(tfd, beg, chans, nints, bufs, cm, NULL));
}


mus_long_t mus_file_read_file(int tfd, mus_long_t beg, int chans, mus_long_t nints, mus_sample_t **bufs)
{
  return(mus_read_any_1(tfd, beg, chans, nints, bufs, NULL, NULL));
}


mus_long_t mus_file_read_buffer(int charbuf_data_format, mus_long_t beg, int chans, mus_long_t nints, mus_sample_t **bufs, char *charbuf)
{
  return(mus_read_any_1(charbuf_data_format, beg, chans, nints, bufs, NULL, charbuf)); 
}


mus_long_t mus_file_read(int tfd, mus_long_t beg, mus_long_t end, int chans, mus_sample_t **bufs)
{
  mus_long_t num, rtn, k;
  num = (end - beg + 1);
  rtn = mus_read_any_1(tfd, beg, chans, num, bufs, NULL, NULL);
  if (rtn == MUS_ERROR) return(MUS_ERROR);
  if (rtn < num) 
    /* this zeroing can be fooled if the file is chunked and has trailing, non-data chunks */
    for (k = 0; k < chans; k++)
      {
	mus_long_t i;
	mus_sample_t *buffer;
	buffer = bufs[k];
	i = rtn + beg;
	/* this happens routinely in mus_outa + initial write (reads ahead in effect) */
	memset((void *)(buffer + i), 0, (end - i + 1) * sizeof(mus_sample_t));
      }
  return(num);
}


mus_long_t mus_file_read_chans(int tfd, mus_long_t beg, mus_long_t end, int chans, mus_sample_t **bufs, mus_sample_t **cm)
{
  /* an optimization of mus_file_read -- just reads the desired channels */
  mus_long_t num, rtn, k;
  num = (end - beg + 1);
  rtn = mus_read_any_1(tfd, beg, chans, num, bufs, cm, NULL);
  if (rtn == MUS_ERROR) return(MUS_ERROR);
  if (rtn < num) 
    for (k = 0; k < chans; k++)
      if ((cm == NULL) || (cm[k]))
	{
	  mus_long_t i;
	  mus_sample_t *buffer;
	  buffer = bufs[k];
	  i = rtn + beg;
	  memset((void *)(buffer + i), 0, (end - i + 1) * sizeof(mus_sample_t));
	}
  return(num);
}


/* ---------------- write ---------------- */

static int checked_write(int tfd, char *buf, mus_long_t chars)
{
  ssize_t bytes;
  bytes = write(tfd, buf, chars);
  if (bytes != chars) 
    {
      io_fd *fd;
      if ((io_fds == NULL) || (tfd >= io_fd_size) || (tfd < 0) || (io_fds[tfd] == NULL))
	return(mus_error(MUS_FILE_DESCRIPTORS_NOT_INITIALIZED, "mus_write: no file descriptors!"));
      fd = io_fds[tfd];
      if (fd->data_format == MUS_UNKNOWN) 
	return(mus_error(MUS_FILE_CLOSED,
			 "attempt to write closed file %s",
			 fd->name));
      else
	return(mus_error(MUS_WRITE_ERROR,
			 "mus_write: write error for %s%s%s: only " SSIZE_TD " of " OFF_TD " bytes written",
			 fd->name, (errno) ? ": " : "", (errno) ? STRERROR(errno) : "",
			 bytes, chars));
    }
  return(MUS_NO_ERROR);
}


static mus_clip_handler_t *mus_clip_handler = NULL;

mus_clip_handler_t *mus_clip_set_handler(mus_clip_handler_t *new_clip_handler) 
{
  mus_clip_handler_t *old_handler;
  old_handler = mus_clip_handler;
  mus_clip_handler = new_clip_handler;
  return(old_handler);
}


static int mus_write_1(int tfd, mus_long_t beg, mus_long_t end, int chans, mus_sample_t **bufs, char *inbuf, bool clipped)
{
  int err, siz, siz_chans, data_format, val;
  mus_long_t bytes, j, k, lim, leftover, loc, oldloc, buflim, cliploc;
  bool clipping = false;
  unsigned char *jchar;
  char *charbuf = NULL;
  mus_sample_t *buffer;

  if (chans <= 0) return(0);

  if (!inbuf)
    {
      io_fd *fd;
      if ((io_fds == NULL) || 
	  (tfd >= io_fd_size) || 
	  (tfd < 0) || 
	  (io_fds[tfd] == NULL))
	return(mus_error(MUS_FILE_DESCRIPTORS_NOT_INITIALIZED, "mus_write: no file descriptors!"));

      fd = io_fds[tfd];
      if (fd->data_format == MUS_UNKNOWN) 
	return(mus_error(MUS_FILE_CLOSED, "mus_write: invalid data format for %s", fd->name));

      siz = fd->bytes_per_sample;
      data_format = fd->data_format;
      clipping = fd->clipping;

      if ((data_format == MUS_OUT_FORMAT) && 
	  (chans == 1) && 
	  (!clipping) && 
	  (beg == 0))
	{
	  bytes = (end + 1) * siz;
	  return(checked_write(tfd, (char *)(bufs[0]), bytes));
	}
    }
  else
    {
      siz = mus_bytes_per_sample(tfd);
      data_format = tfd; /* in this case, tfd is the data format (see mus_file_write_buffer below) -- this should be changed! */
      clipping = clipped;
    }

  lim = (end - beg + 1);
  siz_chans = siz * chans;
  leftover = lim * siz_chans;
  k = (BUFLIM) % siz_chans;
  if (k != 0) 
    buflim = (BUFLIM) - k;
  else buflim = BUFLIM;
  loc = beg;

  if (inbuf)
    charbuf = inbuf;

  while (leftover > 0)
    {
      bytes = leftover;
      if (bytes > buflim) 
	{
	  leftover = (bytes - buflim); 
	  bytes = buflim;
	} 
      else leftover = 0;
      lim = (int)(bytes / siz_chans); /* see note above */
      oldloc = loc;

      for (k = 0; k < chans; k++)
	{
	  mus_long_t loclim;

	  if (bufs[k] == NULL) continue;
	  loc = oldloc;
	  buffer = (mus_sample_t *)(bufs[k]);
	  if (clipping)
	    {
	      cliploc = oldloc;
	      for (j = 0; j < lim; j++, cliploc++)
		if (buffer[cliploc] > MUS_SAMPLE_MAX)
		  {
		    if (mus_clip_handler)
		      buffer[cliploc] = (*mus_clip_handler)(buffer[cliploc]);
		    else buffer[cliploc] = MUS_SAMPLE_MAX;
		  }
		else
		  if (buffer[cliploc] < MUS_SAMPLE_MIN)
		    {
		      if (mus_clip_handler)
			buffer[cliploc] = (*mus_clip_handler)(buffer[cliploc]);
		      else buffer[cliploc] = MUS_SAMPLE_MIN;
		    }

	      if ((data_format == MUS_OUT_FORMAT) && 
		  (chans == 1) && 
		  (beg == 0))
		{
		  bytes = (end + 1) * siz;
		  return(checked_write(tfd, (char *)(bufs[0]), bytes));
		}
	    }
	  loclim = loc + lim;
	  if (!charbuf)
	    {
	      charbuf = (char *)calloc(BUFLIM, sizeof(char)); 
	      if (charbuf == NULL) 
		return(mus_error(MUS_MEMORY_ALLOCATION_FAILED, "mus_write: IO buffer allocation failed"));
	    }

	  jchar = (unsigned char *)charbuf; /* if to_buffer we should add the loop offset here, or never loop */
	  jchar += (k * siz); 
	  switch (data_format)
	    {
	    case MUS_BSHORT: 
	      for (; loc < loclim; loc++, jchar += siz_chans) 
		set_big_endian_short(jchar, MUS_SAMPLE_TO_SHORT(buffer[loc]));
	      break;

	    case MUS_LSHORT:   
	      for (; loc < loclim; loc++, jchar += siz_chans) 
		set_little_endian_short(jchar, MUS_SAMPLE_TO_SHORT(buffer[loc]));
	      break;

	    case MUS_BINT:   
	      for (; loc < loclim; loc++, jchar += siz_chans) 
		set_big_endian_int(jchar, MUS_SAMPLE_TO_INT(buffer[loc]));
	      break;

	    case MUS_LINT:   
	      for (; loc < loclim; loc++, jchar += siz_chans) 
		set_little_endian_int(jchar, MUS_SAMPLE_TO_INT(buffer[loc]));
	      break;

	    case MUS_BINTN:   
	      for (; loc < loclim; loc++, jchar += siz_chans) 
		set_big_endian_int(jchar, MUS_SAMPLE_TO_INT(buffer[loc]) << (32 - MUS_SAMPLE_BITS));
	      break;

	    case MUS_LINTN:   
	      for (; loc < loclim; loc++, jchar += siz_chans) 
		set_little_endian_int(jchar, MUS_SAMPLE_TO_INT(buffer[loc]) << (32 - MUS_SAMPLE_BITS));
	      break;

	    case MUS_MULAW:     
	      for (; loc < loclim; loc++, jchar += siz_chans) 
		(*jchar) = to_mulaw(MUS_SAMPLE_TO_SHORT(buffer[loc]));
	      break;

	    case MUS_ALAW:      
	      for (; loc < loclim; loc++, jchar += siz_chans) 
		(*jchar) = to_alaw(MUS_SAMPLE_TO_SHORT(buffer[loc]));
	      break;

	    case MUS_BYTE:    
	      for (; loc < loclim; loc++, jchar += siz_chans) 
		(*((signed char *)jchar)) = MUS_SAMPLE_TO_BYTE(buffer[loc]);
	      break;

	    case MUS_UBYTE:  
	      for (; loc < loclim; loc++, jchar += siz_chans) 
		(*jchar) = MUS_SAMPLE_TO_BYTE(buffer[loc]) + UBYTE_ZERO;
	      break;

	    case MUS_BFLOAT:    
	      for (; loc < loclim; loc++, jchar += siz_chans) 
		set_big_endian_float(jchar, MUS_SAMPLE_TO_FLOAT(buffer[loc]));
	      break;

	    case MUS_LFLOAT:    
	      for (; loc < loclim; loc++, jchar += siz_chans) 
		set_little_endian_float(jchar, MUS_SAMPLE_TO_FLOAT(buffer[loc]));
	      break;

	    case MUS_BDOUBLE:
	      for (; loc < loclim; loc++, jchar += siz_chans) 
		set_big_endian_double(jchar, MUS_SAMPLE_TO_DOUBLE(buffer[loc]));
	      break;

	    case MUS_LDOUBLE:   
	      for (; loc < loclim; loc++, jchar += siz_chans) 
		set_little_endian_double(jchar, MUS_SAMPLE_TO_DOUBLE(buffer[loc]));
	      break;

	    case MUS_BFLOAT_UNSCALED:    
	      for (; loc < loclim; loc++, jchar += siz_chans) 
		set_big_endian_float(jchar, 32768.0 * MUS_SAMPLE_TO_FLOAT(buffer[loc]));
	      break;

	    case MUS_LFLOAT_UNSCALED:    
	      for (; loc < loclim; loc++, jchar += siz_chans) 
		set_little_endian_float(jchar, 32768.0 * MUS_SAMPLE_TO_FLOAT(buffer[loc]));
	      break;

	    case MUS_BDOUBLE_UNSCALED:
	      for (; loc < loclim; loc++, jchar += siz_chans) 
		set_big_endian_double(jchar, 32768.0 * MUS_SAMPLE_TO_DOUBLE(buffer[loc]));
	      break;

	    case MUS_LDOUBLE_UNSCALED:   
	      for (; loc < loclim; loc++, jchar += siz_chans) 
		set_little_endian_double(jchar, 32768.0 * MUS_SAMPLE_TO_DOUBLE(buffer[loc]));
	      break;

	    case MUS_UBSHORT: 
	      for (; loc < loclim; loc++, jchar += siz_chans) 
		set_big_endian_unsigned_short(jchar, (unsigned short)(MUS_SAMPLE_TO_SHORT(buffer[loc]) + USHORT_ZERO));
	      break;

	    case MUS_ULSHORT: 
	      for (; loc < loclim; loc++, jchar += siz_chans) 
		set_little_endian_unsigned_short(jchar, (unsigned short)(MUS_SAMPLE_TO_SHORT(buffer[loc]) + USHORT_ZERO));
	      break;

	    case MUS_B24INT: 
	      {
		int c3;
		mus_long_t bk;
		bk = (k * 3);
		c3 = chans * 3;
		for (; loc < loclim; loc++, bk += c3) 
		  {
		    val = MUS_SAMPLE_TO_INT24(buffer[loc]);
		    charbuf[bk] = (val >> 16); 
		    charbuf[bk + 1] = (val >> 8); 
		    charbuf[bk + 2] = (val & 0xFF); 
		  }
	      }
	      break;

	    case MUS_L24INT:   
	      {
		int c3;
		mus_long_t bk;
		bk = (k * 3);
		c3 = chans * 3;
		for (; loc < loclim; loc++, bk += c3)
		  {
		    val = MUS_SAMPLE_TO_INT24(buffer[loc]);
		    charbuf[bk + 2] = (val >> 16); 
		    charbuf[bk + 1] = (val >> 8); 
		    charbuf[bk] = (val & 0xFF); 
		  }
	      }
	      break;
	    }
	}
      if (!inbuf)
	{
	  err = checked_write(tfd, charbuf, bytes);
	  if (err == MUS_ERROR) 
	    {
	      free(charbuf); 
	      return(MUS_ERROR);
	    }
	}
    }
  if (!inbuf) free(charbuf);
  return(MUS_NO_ERROR);
}


int mus_file_write(int tfd, mus_long_t beg, mus_long_t end, int chans, mus_sample_t **bufs)
{
  return(mus_write_1(tfd, beg, end, chans, bufs, NULL, false));
}


int mus_file_write_file(int tfd, mus_long_t beg, mus_long_t end, int chans, mus_sample_t **bufs)
{
  return(mus_write_1(tfd, beg, end, chans, bufs, NULL, false));
}


int mus_file_write_buffer(int charbuf_data_format, mus_long_t beg, mus_long_t end, int chans, mus_sample_t **bufs, char *charbuf, bool clipped)
{
  return(mus_write_1(charbuf_data_format, beg, end, chans, bufs, charbuf, clipped));
}


/* for CLM */
void mus_reset_io_c(void) 
{
  io_fd_size = 0;
  io_fds = NULL;
  clipping_default = false;
  prescaler_default = 1.0;
  mus_clip_set_handler(NULL);
}


#if !(defined(MUS_WINDOZE) && (!(defined(__CYGWIN__))))
static int sndlib_strlen(const char *str)
{
  /* strlen(NULL) -> seg fault! */
  if ((str) && (*str)) return(strlen(str));
  return(0);
}
#endif


char *mus_getcwd(void)
{
  int i, path_max = 0;
  char *pwd = NULL, *res = NULL;
#if HAVE_PATHCONF
  path_max = pathconf("/", _PC_PATH_MAX);
#endif
  if (path_max < 1024)
    {
#if defined(PATH_MAX)
      path_max = PATH_MAX;
#endif
      if (path_max < 1024) 
	path_max = 1024;
    }
#if HAVE_GETCWD
  for (i = path_max;; i *= 2)
    {
      if (pwd) free(pwd);
      pwd = (char *)calloc(i, sizeof(char));
      res = getcwd(pwd, i);
      if (res) break;    /* NULL is returned if failure, but what about success? should I check errno=ERANGE? */
    }
#else
#if HAVE_GETWD
  pwd = (char *)calloc(path_max, sizeof(char));
  getwd(pwd);
#endif
#endif
  return(pwd);
}


char *mus_expand_filename(const char *filename)
{
  /* fill out under-specified library pathnames etc */
#if defined(MUS_WINDOZE) && (!(defined(__CYGWIN__)))
  return(mus_strdup(filename));
#else
  char *file_name_buf = NULL;
  char *tok = NULL, *orig = NULL;
  int i, j = 0, len = 0;

  if ((filename) && (*filename)) 
    len = strlen(filename); 
  else return(NULL);
  if (len == 0) return(NULL);

  orig = mus_strdup(filename);
  tok = orig;
  /* get rid of "//" */
  for (i = 0; i < len - 1; i++)
    {
      if ((tok[i] == '/') && 
	  (tok[i + 1] == '/')) 
	j = i + 1;
    }
  if (j > 0)
    {
      for (i = 0; j < len; i++, j++) 
	tok[i] = tok[j];
      tok[i] ='\0';
    }
  /* get rid of "~/" at start */
  if (tok[0] != '/')
    {
      char *home = NULL;
      if ((tok[0] == '~') && (home = getenv("HOME")))
	{
	  file_name_buf = (char *)calloc(len + sndlib_strlen(home) + 8, sizeof(char));
	  strcpy(file_name_buf, home);
	  strcat(file_name_buf, ++tok);
	}
      else
	{
	  char *pwd;
	  pwd = mus_getcwd();
	  file_name_buf = (char *)calloc(len + sndlib_strlen(pwd) + 8, sizeof(char));
	  strcpy(file_name_buf, pwd);
	  free(pwd);
	  strcat(file_name_buf, "/");
	  if (tok[0])
	    strcat(file_name_buf, tok);
	}
    }
  else 
    {
      file_name_buf = (char *)calloc(len + 8, sizeof(char));
      strcpy(file_name_buf, tok);
    }
  /* get rid of "/../" and "/./" also "/." at end */
  {
    int slash_at = -1;
    bool found_one = true;
    while (found_one)
      {
	found_one = false;
	len = strlen(file_name_buf);
	for (i = 0; i < len - 3; i++)
	  if (file_name_buf[i] == '/')
	    {
	      if ((file_name_buf[i + 1] == '.') &&
		  (file_name_buf[i + 2] == '.') &&
		  (file_name_buf[i + 3] == '/'))
		{
		  i += 4;
		  for (j = slash_at + 1; i < len; i++, j++)
		    file_name_buf[j] = file_name_buf[i];
		  file_name_buf[j] = '\0';
		  found_one = true;
		  break;
		}
	      else
		{
		  if ((file_name_buf[i + 1] == '.') &&
		      (file_name_buf[i + 2] == '/'))
		    {
		      for (j = i + 3, i = i + 1; j < len; i++, j++)
			file_name_buf[i] = file_name_buf[j];
		      file_name_buf[i] = '\0';
		      found_one = true;
		    }
		  else slash_at = i;
		}
	    }
      }
    len = strlen(file_name_buf);
    if ((len > 1) &&
	(file_name_buf[len - 1] == '.') &&
	(file_name_buf[len - 2] == '/'))
      file_name_buf[len - 1] = '\0';
  }
  free(orig);
  return(file_name_buf);
#endif
}


int mus_snprintf(char *buffer, int buffer_len, const char *format, ...)
{
  int bytes_needed = 0;
  va_list ap;
  va_start(ap, format);
#if HAVE_VSNPRINTF
  bytes_needed = vsnprintf(buffer, buffer_len, format, ap);
#else
  bytes_needed = vsprintf(buffer, format, ap);
#endif
  va_end(ap);
  return(bytes_needed);
}


#define MUS_FORMAT_STRING_MAX 1024

char *mus_format(const char *format, ...)
{
  /* caller should free result */
  char *buf = NULL, *rtn = NULL;
  int needed_bytes = 0;
  va_list ap;
  buf = (char *)calloc(MUS_FORMAT_STRING_MAX, sizeof(char));
  va_start(ap, format);
#if HAVE_VSNPRINTF
  needed_bytes = vsnprintf(buf, MUS_FORMAT_STRING_MAX, format, ap);
#else
  needed_bytes = vsprintf(buf, format, ap);
#endif
  va_end(ap);
  if (needed_bytes > MUS_FORMAT_STRING_MAX)
    {
      free(buf);
      buf = (char *)calloc(needed_bytes + 1, sizeof(char));
      va_start(ap, format);
#if HAVE_VSNPRINTF
      vsnprintf(buf, needed_bytes + 1, format, ap);
#else
      vsprintf(buf, format, ap); /* argh -- we already clobbered memory, I presume */
#endif
      va_end(ap);
    }
  rtn = mus_strdup(buf);
  free(buf);
  return(rtn);
}


mus_float_t mus_fclamp(mus_float_t lo, mus_float_t val, mus_float_t hi) 
{
  if (val > hi) 
    return(hi); 
  else 
    if (val < lo) 
      return(lo); 
    else return(val);
}


int mus_iclamp(int lo, int val, int hi) 
{
  if (val > hi) 
    return(hi); 
  else 
    if (val < lo) 
      return(lo); 
    else return(val);
}


mus_long_t mus_oclamp(mus_long_t lo, mus_long_t val, mus_long_t hi) 
{
  if (val > hi) 
    return(hi); 
  else 
    if (val < lo) 
      return(lo); 
    else return(val);
}



/* raw sample data peak value (without per-sample conversion) */


/* ---------------- short ---------------- */

#define SHORT_BYTES 2

static void min_max_shorts(unsigned char *data, int bytes, int chan, int chans, mus_float_t *min_samp, mus_float_t *max_samp)
{
  short cur_min, cur_max;
  short *sbuf;
  int i, len;

  sbuf = (short *)data;
  len = bytes / SHORT_BYTES;

  cur_min = sbuf[chan];
  cur_max = cur_min;

  for (i = (chans + chan); i < len; i += chans)
    {
      if (sbuf[i] < cur_min) cur_min = sbuf[i];
      else if (sbuf[i] > cur_max) cur_max = sbuf[i];
    }

  (*min_samp) = (mus_float_t)cur_min / (mus_float_t)(1 << 15);
  (*max_samp) = (mus_float_t)cur_max / (mus_float_t)(1 << 15);
}


static void min_max_switch_shorts(unsigned char *data, int bytes, int chan, int chans, mus_float_t *min_samp, mus_float_t *max_samp)
{
  short cur_min, cur_max;
  /* frame based */
  unsigned char *samp, *eod;
  int bytes_per_frame;

  bytes_per_frame = chans * SHORT_BYTES;
  eod = (unsigned char *)(data + bytes);

#if MUS_LITTLE_ENDIAN
  cur_min = big_endian_short((unsigned char *)(data + (chan * SHORT_BYTES)));
#else
  cur_min = little_endian_short((unsigned char *)(data + (chan * SHORT_BYTES)));
#endif
  cur_max = cur_min;

  for (samp = (unsigned char *)(data + (chan * SHORT_BYTES) + bytes_per_frame); samp < eod; samp += bytes_per_frame)
    {
      short val;
#if MUS_LITTLE_ENDIAN
      val = big_endian_short(samp);
#else
      val = little_endian_short(samp);
#endif
      if (val < cur_min) cur_min = val;
      else if (val > cur_max) cur_max = val;
    }

  (*min_samp) = (mus_float_t)cur_min / (mus_float_t)(1 << 15);
  (*max_samp) = (mus_float_t)cur_max / (mus_float_t)(1 << 15);
}


/* ---------------- unsigned short ---------------- */

static void min_max_ushorts(unsigned char *data, int bytes, int chan, int chans, mus_float_t *min_samp, mus_float_t *max_samp)
{
  unsigned short cur_min, cur_max;
  unsigned short *sbuf;
  int i, len;

  sbuf = (unsigned short *)data;
  len = bytes / SHORT_BYTES;

  cur_min = sbuf[chan];
  cur_max = cur_min;

  for (i = (chans + chan); i < len; i += chans)
    {
      if (sbuf[i] < cur_min) cur_min = sbuf[i];
      else if (sbuf[i] > cur_max) cur_max = sbuf[i];
    }

  (*min_samp) = (mus_float_t)(cur_min - USHORT_ZERO) / (mus_float_t)(1 << 15);
  (*max_samp) = (mus_float_t)(cur_max - USHORT_ZERO) / (mus_float_t)(1 << 15);
}


static void min_max_switch_ushorts(unsigned char *data, int bytes, int chan, int chans, mus_float_t *min_samp, mus_float_t *max_samp)
{
  unsigned short cur_min, cur_max;
  /* frame based */
  unsigned char *samp, *eod;
  int bytes_per_frame;

  bytes_per_frame = chans * SHORT_BYTES;
  eod = (unsigned char *)(data + bytes);

#if MUS_LITTLE_ENDIAN
  cur_min = big_endian_unsigned_short((unsigned char *)(data + (chan * SHORT_BYTES)));
#else
  cur_min = little_endian_unsigned_short((unsigned char *)(data + (chan * SHORT_BYTES)));
#endif
  cur_max = cur_min;

  for (samp = (unsigned char *)(data + (chan * SHORT_BYTES) + bytes_per_frame); samp < eod; samp += bytes_per_frame)
    {
      unsigned short val;
#if MUS_LITTLE_ENDIAN
      val = big_endian_unsigned_short(samp);
#else
      val = little_endian_unsigned_short(samp);
#endif
      if (val < cur_min) cur_min = val;
      else if (val > cur_max) cur_max = val;
    }

  (*min_samp) = (mus_float_t)(cur_min - USHORT_ZERO) / (mus_float_t)(1 << 15);
  (*max_samp) = (mus_float_t)(cur_max - USHORT_ZERO) / (mus_float_t)(1 << 15);
}


/* ---------------- int ---------------- */

#define INT_BYTES 4

static void min_max_ints(unsigned char *data, int bytes, int chan, int chans, mus_float_t *min_samp, mus_float_t *max_samp, bool standard)
{
  int cur_min, cur_max;
  int *sbuf;
  int i, len;

  sbuf = (int *)data;
  len = bytes / INT_BYTES;

  cur_min = sbuf[chan];
  cur_max = cur_min;

  for (i = (chans + chan); i < len; i += chans)
    {
      if (sbuf[i] < cur_min) cur_min = sbuf[i];
      else if (sbuf[i] > cur_max) cur_max = sbuf[i];
    }
  
  (*min_samp) = (mus_float_t)cur_min / (mus_float_t)(1 << 23);
  (*max_samp) = (mus_float_t)cur_max / (mus_float_t)(1 << 23);

  if (!standard)
    {
      (*min_samp) = (mus_float_t)(*min_samp) / (mus_float_t)(1 << 8);
      (*max_samp) = (mus_float_t)(*max_samp) / (mus_float_t)(1 << 8);
    }
}

/* (with-sound (:data-format mus-lintn :statistics #t) (fm-violin 0 1 440 .1)) */


static void min_max_switch_ints(unsigned char *data, int bytes, int chan, int chans, mus_float_t *min_samp, mus_float_t *max_samp, bool standard)
{
  int cur_min, cur_max;
  /* frame based */
  unsigned char *samp, *eod;
  int bytes_per_frame;

  bytes_per_frame = chans * INT_BYTES;
  eod = (unsigned char *)(data + bytes);

#if MUS_LITTLE_ENDIAN
  cur_min = big_endian_int((unsigned char *)(data + (chan * INT_BYTES)));
#else
  cur_min = little_endian_int((unsigned char *)(data + (chan * INT_BYTES)));
#endif
  cur_max = cur_min;

  for (samp = (unsigned char *)(data + (chan * INT_BYTES) + bytes_per_frame); samp < eod; samp += bytes_per_frame)
    {
      int val;
#if MUS_LITTLE_ENDIAN
      val = big_endian_int(samp);
#else
      val = little_endian_int(samp);
#endif
      if (val < cur_min) cur_min = val;
      else if (val > cur_max) cur_max = val;
    }

  (*min_samp) = (mus_float_t)cur_min / (mus_float_t)(1 << 23);
  (*max_samp) = (mus_float_t)cur_max / (mus_float_t)(1 << 23);

  if (!standard)
    {
      (*min_samp) = (mus_float_t)(*min_samp) / (mus_float_t)(1 << 8);
      (*max_samp) = (mus_float_t)(*max_samp) / (mus_float_t)(1 << 8);
    }
}



/* ---------------- float ---------------- */

#define FLOAT_BYTES 4

static void min_max_floats(unsigned char *data, int bytes, int chan, int chans, mus_float_t *min_samp, mus_float_t *max_samp, bool unscaled)
{
  float cur_min, cur_max;
  float *sbuf;
  int i, len;

  sbuf = (float *)data;
  len = bytes / FLOAT_BYTES;

  cur_min = sbuf[chan];
  cur_max = cur_min;

  for (i = (chans + chan); i < len; i += chans)
    {
      if (sbuf[i] < cur_min) cur_min = sbuf[i];
      else if (sbuf[i] > cur_max) cur_max = sbuf[i];
    }

  if (unscaled)
    {
      (*min_samp) = cur_min / 32768.0;
      (*max_samp) = cur_max / 32768.0;
    }
  else
    {
      (*min_samp) = cur_min;
      (*max_samp) = cur_max;
    }
}


static void min_max_switch_floats(unsigned char *data, int bytes, int chan, int chans, mus_float_t *min_samp, mus_float_t *max_samp, bool unscaled)
{
  float cur_min, cur_max;
  /* frame based */
  unsigned char *samp, *eod;
  int bytes_per_frame;

  bytes_per_frame = chans * FLOAT_BYTES;
  eod = (unsigned char *)(data + bytes);

#if MUS_LITTLE_ENDIAN
  cur_min = big_endian_float((unsigned char *)(data + (chan * FLOAT_BYTES)));
#else
  cur_min = little_endian_float((unsigned char *)(data + (chan * FLOAT_BYTES)));
#endif
  cur_max = cur_min;

  for (samp = (unsigned char *)(data + (chan * FLOAT_BYTES) + bytes_per_frame); samp < eod; samp += bytes_per_frame)
    {
      float val;
#if MUS_LITTLE_ENDIAN
      val = big_endian_float(samp);
#else
      val = little_endian_float(samp);
#endif
      if (val < cur_min) cur_min = val;
      else if (val > cur_max) cur_max = val;
    }

  if (unscaled)
    {
      (*min_samp) = cur_min / 32768.0;
      (*max_samp) = cur_max / 32768.0;
    }
  else
    {
      (*min_samp) = cur_min;
      (*max_samp) = cur_max;
    }
}



/* ---------------- double ---------------- */

#define DOUBLE_BYTES 8

static void min_max_doubles(unsigned char *data, int bytes, int chan, int chans, mus_float_t *min_samp, mus_float_t *max_samp, bool unscaled)
{
  double cur_min, cur_max;
  double *sbuf;
  int i, len;

  sbuf = (double *)data;
  len = bytes / DOUBLE_BYTES;

  cur_min = sbuf[chan];
  cur_max = cur_min;

  for (i = (chans + chan); i < len; i += chans)
    {
      if (sbuf[i] < cur_min) cur_min = sbuf[i];
      else if (sbuf[i] > cur_max) cur_max = sbuf[i];
    }

  if (unscaled)
    {
      (*min_samp) = cur_min / 32768.0;
      (*max_samp) = cur_max / 32768.0;
    }
  else
    {
      (*min_samp) = cur_min;
      (*max_samp) = cur_max;
    }
}


static void min_max_switch_doubles(unsigned char *data, int bytes, int chan, int chans, mus_float_t *min_samp, mus_float_t *max_samp, bool unscaled)
{
  double cur_min, cur_max;
  /* frame based */
  unsigned char *samp, *eod;
  int bytes_per_frame;

  bytes_per_frame = chans * DOUBLE_BYTES;
  eod = (unsigned char *)(data + bytes);

#if MUS_LITTLE_ENDIAN
  cur_min = big_endian_double((unsigned char *)(data + (chan * DOUBLE_BYTES)));
#else
  cur_min = little_endian_double((unsigned char *)(data + (chan * DOUBLE_BYTES)));
#endif
  cur_max = cur_min;

  for (samp = (unsigned char *)(data + (chan * DOUBLE_BYTES) + bytes_per_frame); samp < eod; samp += bytes_per_frame)
    {
      double val;
#if MUS_LITTLE_ENDIAN
      val = big_endian_double(samp);
#else
      val = little_endian_double(samp);
#endif
      if (val < cur_min) cur_min = val;
      else if (val > cur_max) cur_max = val;
    }

  if (unscaled)
    {
      (*min_samp) = cur_min / 32768.0;
      (*max_samp) = cur_max / 32768.0;
    }
  else
    {
      (*min_samp) = cur_min;
      (*max_samp) = cur_max;
    }
}



/* ---------------- 3-byte samples ---------------- */

#define THREE_BYTES 3

static int three_bytes(unsigned char *data, int loc, bool big_endian)
{
  if (big_endian)
    return((int)(((data[loc + 0] << 24) + 
		  (data[loc + 1] << 16) + 
		  (data[loc + 2] << 8)) >> 8));
  return((int)(((data[loc + 2] << 24) + 
		(data[loc + 1] << 16) + 
		(data[loc + 0] << 8)) >> 8));
}


static void min_max_24s(unsigned char *data, int bytes, int chan, int chans, mus_float_t *min_samp, mus_float_t *max_samp, bool big_endian)
{
  int cur_min, cur_max;
  int i, bytes_per_frame, len, offset;

  bytes_per_frame = chans * THREE_BYTES;
  len = bytes / bytes_per_frame;
  offset = chan * THREE_BYTES;

  cur_min = three_bytes(data, offset, big_endian);
  cur_max = cur_min;

  for (i = 1; i < len; i++)
    {
      int val;
      val = three_bytes(data, i * bytes_per_frame + offset, big_endian);

      if (val < cur_min) cur_min = val;
      else if (val > cur_max) cur_max = val;
    }

  (*min_samp) = (mus_float_t)cur_min / (mus_float_t)(1 << 23);
  (*max_samp) = (mus_float_t)cur_max / (mus_float_t)(1 << 23);
}



/* ---------------- mulaw, alaw, byte ---------------- */

static void min_max_mulaw(unsigned char *data, int bytes, int chan, int chans, mus_float_t *min_samp, mus_float_t *max_samp)
{
  int cur_min, cur_max;
  int i;

  cur_min = mulaw[(int)data[chan]];
  cur_max = cur_min;

  for (i = (chans + chan); i < bytes; i += chans)
    {
      int val;
      val = mulaw[(int)data[i]];
      if (val < cur_min) cur_min = val;
      else if (val > cur_max) cur_max = val;
    }

  (*min_samp) = (mus_float_t)cur_min / 32768.0;
  (*max_samp) = (mus_float_t)cur_max / 32768.0;
}


static void min_max_alaw(unsigned char *data, int bytes, int chan, int chans, mus_float_t *min_samp, mus_float_t *max_samp)
{
  int cur_min, cur_max;
  int i;

  cur_min = alaw[(int)data[chan]];
  cur_max = cur_min;

  for (i = (chans + chan); i < bytes; i += chans)
    {
      int val;
      val = alaw[(int)data[i]];
      if (val < cur_min) cur_min = val;
      else if (val > cur_max) cur_max = val;
    }

  (*min_samp) = (mus_float_t)cur_min / 32768.0;
  (*max_samp) = (mus_float_t)cur_max / 32768.0;
}


static void min_max_bytes(unsigned char *data, int bytes, int chan, int chans, mus_float_t *min_samp, mus_float_t *max_samp)
{
  signed char cur_min, cur_max;
  int i;

  cur_min = (signed char)(data[chan]);
  cur_max = cur_min;

  for (i = (chans + chan); i < bytes; i += chans)
    {
      signed char val;
      val = (signed char)(data[i]);
      if (val < cur_min) cur_min = val;
      else if (val > cur_max) cur_max = val;
    }

  (*min_samp) = (mus_float_t)cur_min / (mus_float_t)(1 << 7);
  (*max_samp) = (mus_float_t)cur_max / (mus_float_t)(1 << 7);
}


static void min_max_ubytes(unsigned char *data, int bytes, int chan, int chans, mus_float_t *min_samp, mus_float_t *max_samp)
{
  unsigned char cur_min, cur_max;
  int i;

  cur_min = data[chan];
  cur_max = cur_min;

  for (i = (chans + chan); i < bytes; i += chans)
    {
      if (data[i] < cur_min) cur_min = data[i];
      else if (data[i] > cur_max) cur_max = data[i];
    }

  (*min_samp) = (mus_float_t)(cur_min - UBYTE_ZERO) / (mus_float_t)(1 << 7);
  (*max_samp) = (mus_float_t)(cur_max - UBYTE_ZERO) / (mus_float_t)(1 << 7);
}


int mus_samples_bounds(unsigned char *data, int bytes, int chan, int chans, int format, mus_float_t *min_samp, mus_float_t *max_samp)
{
  switch (format)
    {
    case MUS_MULAW:
      min_max_mulaw(data, bytes, chan, chans, min_samp, max_samp);
      break;

    case MUS_ALAW:
      min_max_alaw(data, bytes, chan, chans, min_samp, max_samp);
      break;

    case MUS_BYTE:
      min_max_bytes(data, bytes, chan, chans, min_samp, max_samp);
      break;

    case MUS_UBYTE:
      min_max_ubytes(data, bytes, chan, chans, min_samp, max_samp);
      break;

    case MUS_L24INT:
      min_max_24s(data, bytes, chan, chans, min_samp, max_samp, false);
      break;

    case MUS_B24INT:
      min_max_24s(data, bytes, chan, chans, min_samp, max_samp, true);
      break;

#if MUS_LITTLE_ENDIAN

    case MUS_LSHORT:
      min_max_shorts(data, bytes, chan, chans, min_samp, max_samp);
      break;

    case MUS_BSHORT:
      min_max_switch_shorts(data, bytes, chan, chans, min_samp, max_samp);
      break;

    case MUS_ULSHORT:
      min_max_ushorts(data, bytes, chan, chans, min_samp, max_samp);
      break;

    case MUS_UBSHORT:
      min_max_switch_ushorts(data, bytes, chan, chans, min_samp, max_samp);
      break;

    case MUS_LINT:
    case MUS_LINTN:
      min_max_ints(data, bytes, chan, chans, min_samp, max_samp, format == MUS_LINT);
      break;

    case MUS_BINT:
    case MUS_BINTN:
      min_max_switch_ints(data, bytes, chan, chans, min_samp, max_samp, format == MUS_BINT);
      break;

    case MUS_LFLOAT:
    case MUS_LFLOAT_UNSCALED:
      /* prescaler is known to be 1.0 here */
      min_max_floats(data, bytes, chan, chans, min_samp, max_samp, format == MUS_LFLOAT_UNSCALED);
      break;

    case MUS_BFLOAT:
    case MUS_BFLOAT_UNSCALED:
      min_max_switch_floats(data, bytes, chan, chans, min_samp, max_samp, format == MUS_BFLOAT_UNSCALED);
      break;

    case MUS_LDOUBLE:
    case MUS_LDOUBLE_UNSCALED:
      min_max_doubles(data, bytes, chan, chans, min_samp, max_samp, format == MUS_LDOUBLE_UNSCALED);
      break;

    case MUS_BDOUBLE:
    case MUS_BDOUBLE_UNSCALED:
      min_max_switch_doubles(data, bytes, chan, chans, min_samp, max_samp, format == MUS_BDOUBLE_UNSCALED);
      break;

#else /* big endian */

    case MUS_LSHORT:
      min_max_switch_shorts(data, bytes, chan, chans, min_samp, max_samp);
      break;

    case MUS_BSHORT:
      min_max_shorts(data, bytes, chan, chans, min_samp, max_samp);
      break;
    case MUS_ULSHORT:
      min_max_switch_ushorts(data, bytes, chan, chans, min_samp, max_samp);
      break;

    case MUS_UBSHORT:
      min_max_ushorts(data, bytes, chan, chans, min_samp, max_samp);
      break;

    case MUS_LINT:
    case MUS_LINTN:
      min_max_switch_ints(data, bytes, chan, chans, min_samp, max_samp, format == MUS_LINT);
      break;

    case MUS_BINT:
    case MUS_BINTN:
      min_max_ints(data, bytes, chan, chans, min_samp, max_samp, format == MUS_BINT);
      break;

    case MUS_LFLOAT:
    case MUS_LFLOAT_UNSCALED:
      min_max_switch_floats(data, bytes, chan, chans, min_samp, max_samp, format == MUS_LFLOAT_UNSCALED);
      break;

    case MUS_BFLOAT:
    case MUS_BFLOAT_UNSCALED:
      min_max_floats(data, bytes, chan, chans, min_samp, max_samp, format == MUS_BFLOAT_UNSCALED);
      break;

    case MUS_LDOUBLE:
    case MUS_LDOUBLE_UNSCALED:
      min_max_switch_doubles(data, bytes, chan, chans, min_samp, max_samp, format == MUS_LDOUBLE_UNSCALED);
      break;

    case MUS_BDOUBLE:
    case MUS_BDOUBLE_UNSCALED:
      min_max_doubles(data, bytes, chan, chans, min_samp, max_samp, format == MUS_BDOUBLE_UNSCALED);
      break;

#endif

    default:
      return(MUS_ERROR);
      break;
    }

  return(MUS_NO_ERROR);
}


int mus_samples_peak(unsigned char *data, int bytes, int chans, int format, mus_float_t *maxes)
{
  int chan;
  for (chan = 0; chan < chans; chan++)
    {
      mus_float_t cur_min, cur_max;
      mus_samples_bounds(data, bytes, chan, chans, format, &cur_min, &cur_max);
      if (-cur_min > cur_max)
	maxes[chan] = -cur_min;
      else maxes[chan] = cur_max;
    }
  return(MUS_NO_ERROR);
}


char *mus_strdup(const char *str)
{
  char *newstr = NULL;
  if ((!str) || (!(*str))) return(NULL);
  newstr = (char *)malloc(strlen(str) + 1);
  if (newstr) strcpy(newstr, str);
  return(newstr);
}


int mus_strlen(const char *str)
{
  /* strlen(NULL) -> seg fault! */
  if ((str) && (*str)) return(strlen(str));
  return(0);
}


bool mus_strcmp(const char *str1, const char *str2)
{
  return((str1 == str2) ||
	 ((str1) && (str2) &&
	  (strcmp(str1, str2) == 0)));
}


char *mus_strcat(char *errmsg, const char *str, int *size)
{
  int new_len, err_size;
  new_len = (mus_strlen(str) + mus_strlen(errmsg));
  err_size = size[0];
  if (new_len >= err_size)
    {
      if ((err_size * 2) > new_len)
	err_size = err_size * 2;
      else err_size = new_len * 2;
      errmsg = (char *)realloc(errmsg, err_size * sizeof(char));
      size[0] = err_size;
    }
  strcat(errmsg, str);
  return(errmsg);
}


