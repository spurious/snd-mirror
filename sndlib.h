#ifndef SNDLIB_H
#define SNDLIB_H

/* taken from libtool's demo/foo.h to try to protect us from C++ and ancient C's */
#undef __BEGIN_DECLS
#undef __END_DECLS
#ifdef __cplusplus
# define __BEGIN_DECLS extern "C" {
# define __END_DECLS }
#else
# define __BEGIN_DECLS /* empty */
# define __END_DECLS /* empty */
#endif

#undef __P
#if defined (__STDC__) || defined (_AIX) || (defined (__mips) && defined (_SYSTYPE_SVR4)) || defined(WIN32) || defined(__cplusplus)
# define __P(protos) protos
#else
# define __P(protos) ()
#endif


#define SNDLIB_VERSION 10
#define SNDLIB_REVISION 16
#define SNDLIB_DATE "8-Jun-00"

#ifndef HAVE_SNDLIB
  #define HAVE_SNDLIB 1
#endif

/* try to figure out what type of machine (and in worst case, what OS) we're running on */
/* gcc has various compile-time macros like #cpu, but we're hoping to run in Metroworks C, Watcom C, MSC, CodeWarrior, MPW, etc */

#if defined(HAVE_CONFIG_H)
  #include "config.h"
  #if (!defined(WORDS_BIGENDIAN))
     #define MUS_LITTLE_ENDIAN 1
  #endif
  #if (SIZEOF_INT_P != SIZEOF_INT)
     #define LONG_INT_P 1
  #else 
     #define LONG_INT_P 0
  #endif
#else
  #if defined(ALPHA) || defined(__alpha__)
     #define LONG_INT_P 1
  #else 
     #define LONG_INT_P 0
  #endif
  #define RETSIGTYPE void
  #ifdef __LITTLE_ENDIAN__
    /* NeXTStep on Intel */
    #define MUS_LITTLE_ENDIAN 1
  #else
    #ifdef BYTE_ORDER
      #if (BYTE_ORDER == LITTLE_ENDIAN)
        /* SGI: /usr/include/sys/endian.h, Linux: /usr/include/bytesex.h and endian.h, BeOS: /boot/develop/headers/posix/endian.h */
        /* Alpha is apparently /usr/include/alpha/endian.h */
        #define MUS_LITTLE_ENDIAN 1
      #else
        #if __INTEL__
          #define MUS_LITTLE_ENDIAN 1
        #endif
      #endif
    #endif
  #endif
#endif

#ifndef __GNUC__
  #ifndef __FUNCTION__
    #define __FUNCTION__ ""
  #endif
#endif

#if defined(ALPHA) || defined(WINDOZE) || defined(__alpha__)
  #define MUS_LITTLE_ENDIAN 1
#endif

#if (!(defined(MACOS))) && (defined(MPW_C) || defined(macintosh) || defined(__MRC__))
  #define MACOS 1
#endif

/* due to project builder stupidity, we can't always depend on -D flags here (maybe we need a SNDLIB_OS macro?) */
/* these wouldn't work with autoconf anyway, so we'll do it by hand */

#if (!defined(SGI)) && (!defined(NEXT)) && (!defined(LINUX)) && (!defined(MACOS)) && (!defined(BEOS)) && (!defined(SUN)) && (!defined(UW2)) && (!defined(SCO5)) && (!defined(ALPHA)) && (!defined(WINDOZE))
  #if defined(__BEOS__)
    #define BEOS 1
  #else
    #if defined(__dest_os)
      /* we're in Metrowerks Land */
      #if (__dest_os == __be_os)
        #define BEOS 1
      #else
        #if (__dest_os == __mac_os)
          #define MACOS 1
        #endif
      #endif
    #else
      #if macintosh
        #define MACOS 1
      #else
        #if (__WINDOWS__) || (__NT__) || (_WIN32) || (__CYGWIN__)
          #define WINDOZE 1
          #define MUS_LITTLE_ENDIAN 1
        #else
          #ifdef __alpha__
            #define ALPHA 1
            #define MUS_LITTLE_ENDIAN 1
          #endif
        #endif
      #endif
    #endif
  #endif
#endif  

/* others apparently are __QNX__ __bsdi__ __FreeBSD__ */

#ifndef HAVE_OSS
#ifndef HAVE_ALSA
  #if defined(LINUX) || defined(SCO5) || defined(UW2) || defined(HAVE_SOUNDCARD_H) || defined(HAVE_SYS_SOUNDCARD_H) || defined(HAVE_MACHINE_SOUNDCARD_H) || defined(USR_LIB_OSS) || defined(USR_LOCAL_LIB_OSS) || defined(OPT_OSS)
    #define HAVE_OSS 1
  #else
    #define HAVE_OSS 0
  #endif
#else
  #define HAVE_OSS 0
#endif
#endif

#if (!defined(M_PI))
  #define M_PI 3.14159265358979323846264338327
  #define M_PI_2 (M_PI/2.0)
#endif

#define TWO_PI (2.0*M_PI)

#ifndef SEEK_SET
  #define SEEK_SET 0
#endif

#ifndef SEEK_CUR
  #define SEEK_CUR 1
#endif

#ifndef SEEK_END
  #define SEEK_END 2
#endif

#if (!SNDLIB_USE_FLOATS)
  #define MUS_SAMPLE_TYPE int
  #ifndef MUS_SAMPLE_BITS
    #define MUS_SAMPLE_BITS 24
  #endif
  #define MUS_SAMPLE_0 0
  #define MUS_BYTE_TO_SAMPLE(n) ((n) << (MUS_SAMPLE_BITS - 8))
  #define MUS_SAMPLE_TO_BYTE(n) ((n) >> (MUS_SAMPLE_BITS - 8))
  #define MUS_SHORT_TO_SAMPLE(n) ((n) << (MUS_SAMPLE_BITS - 16))
  #define MUS_SAMPLE_TO_SHORT(n) ((n) >> (MUS_SAMPLE_BITS - 16))
  #if (MUS_SAMPLE_BITS < 24)
    #define MUS_INT24_TO_SAMPLE(n) ((n) >> (24 - MUS_SAMPLE_BITS))
    #define MUS_SAMPLE_TO_INT24(n) ((n) << (24 - MUS_SAMPLE_BITS))
  #else
    #define MUS_INT24_TO_SAMPLE(n) ((n) << (MUS_SAMPLE_BITS - 24))
    #define MUS_SAMPLE_TO_INT24(n) ((n) >> (MUS_SAMPLE_BITS - 24))
  #endif
  #define MUS_INT_TO_SAMPLE(n) (n)
  #define MUS_SAMPLE_TO_INT(n) (n)
  #define MUS_FLOAT_TO_FIX (1 << (MUS_SAMPLE_BITS - 1))
  #define MUS_FIX_TO_FLOAT (1.0 / (float)(MUS_FLOAT_TO_FIX))
  #define MUS_FLOAT_TO_SAMPLE(n) ((int)((n) * MUS_FLOAT_TO_FIX))
  #define MUS_SAMPLE_TO_FLOAT(n) ((float)((n) * MUS_FIX_TO_FLOAT))
  #define MUS_DOUBLE_TO_SAMPLE(n) ((int)((n) * MUS_FLOAT_TO_FIX))
  #define MUS_SAMPLE_TO_DOUBLE(n) ((double)((n) * MUS_FIX_TO_FLOAT))
  #define MUS_SAMPLE_MAX (MUS_FLOAT_TO_FIX - 1)
  #define MUS_SAMPLE_MIN (-(MUS_FLOAT_TO_FIX))
  #define MUS_MIX_MAX (1 << 30)
  #define MUS_MIX_MIN (-(MUS_MIX_MAX))
#else
  /* this could use Float throughout and reflect the Float=double choice elsewhere */
  #define MUS_SAMPLE_TYPE float
  #ifndef MUS_SAMPLE_BITS
    #define MUS_SAMPLE_BITS 24
  #endif
  #define MUS_SAMPLE_0 0.0
  #define MUS_BYTE_TO_SAMPLE(n) ((float)(n) / (float)(1 << 7))
  #define MUS_SHORT_TO_SAMPLE(n) ((float)(n) / (float)(1 << 15))
  #define MUS_INT_TO_SAMPLE(n) ((float)(n) / (float)(1 << (MUS_SAMPLE_BITS-1)))
  #define MUS_INT24_TO_SAMPLE(n) ((float)(n) / (float)(1 << 23))
  #define MUS_FLOAT_TO_FIX 1.0
  #define MUS_FIX_TO_FLOAT 1.0
  #define MUS_FLOAT_TO_SAMPLE(n) (n)
  #define MUS_DOUBLE_TO_SAMPLE(n) (n)
  #define MUS_SAMPLE_TO_FLOAT(n) (n)
  #define MUS_SAMPLE_TO_DOUBLE(n) (n)
  #define MUS_SAMPLE_TO_INT(n) ((int)((n) * (1 << (MUS_SAMPLE_BITS-1))))
  #define MUS_SAMPLE_TO_INT24(n) ((int)((n) * (1 << 23)))
  #define MUS_SAMPLE_TO_SHORT(n) ((short)((n) * (1 << 15)))
  #define MUS_SAMPLE_TO_BYTE(n) ((char)((n) * (1 << 7)))
  #define MUS_SAMPLE_MAX 0.99999
  #define MUS_SAMPLE_MIN (-1.0)
  #define MUS_MIX_MAX 100.0
  #define MUS_MIX_MIN (-100.0)
#endif

#define MUS_DAC_CHANNEL 252525
#define MUS_DAC_REVERB 252520

#define MUS_UNSUPPORTED -1
enum {MUS_NEXT,MUS_AIFC,MUS_RIFF,MUS_BICSF,MUS_NIST,MUS_INRS,MUS_ESPS,MUS_SVX,MUS_VOC,MUS_SNDT,MUS_RAW,
      MUS_SMP,MUS_SD2,MUS_AVR,MUS_IRCAM,MUS_SD1,MUS_SPPACK,MUS_MUS10,MUS_HCOM,MUS_PSION,MUS_MAUD,
      MUS_IEEE,MUS_DESKMATE,MUS_DESKMATE_2500,MUS_MATLAB,MUS_ADC,MUS_SOUND_EDIT,MUS_SOUND_EDIT_16,
      MUS_DVSM,MUS_MIDI,MUS_ESIGNAL,MUS_SOUNDFONT,MUS_GRAVIS,MUS_COMDISCO,MUS_GOLDWAVE,MUS_SRFS,
      MUS_MIDI_SAMPLE_DUMP,MUS_DIAMONDWARE,MUS_REALAUDIO,MUS_ADF,MUS_SBSTUDIOII,MUS_DELUSION,
      MUS_FARANDOLE,MUS_SAMPLE_DUMP,MUS_ULTRATRACKER,MUS_YAMAHA_SY85,MUS_YAMAHA_TX16,MUS_DIGIPLAYER,
      MUS_COVOX,MUS_SPL,MUS_AVI,MUS_OMF,MUS_QUICKTIME,MUS_ASF,MUS_YAMAHA_SY99,MUS_KURZWEIL_2000,
      MUS_AIFF,MUS_PAF,MUS_CSL,MUS_FILE_SAMP};

#define MUS_HEADER_TYPE_OK(n) (((n) > MUS_UNSUPPORTED) && ((n) <= MUS_FILE_SAMP))

enum {MUS_UNKNOWN,MUS_BSHORT,MUS_MULAW,MUS_BYTE,MUS_BFLOAT,MUS_BINT,MUS_ALAW,MUS_UBYTE,MUS_B24INT,
      MUS_BDOUBLE,MUS_LSHORT,MUS_LINT,MUS_LFLOAT,MUS_LDOUBLE,MUS_UBSHORT,MUS_ULSHORT,MUS_L24INT,
      MUS_BINTN,MUS_LINTN,MUS_L12INT};

/* MUS_LINTN and MUS_BINTN refer to 32 bit ints with 31 bits of "fraction" -- the data is "left justified" */

#define MUS_DATA_FORMAT_OK(n) (((n) > MUS_UNKNOWN) && ((n) <= MUS_L12INT))

#ifdef MUS_LITTLE_ENDIAN
  #define MUS_COMPATIBLE_FORMAT MUS_LSHORT
#else
  #define MUS_COMPATIBLE_FORMAT MUS_BSHORT
#endif

#ifdef MUS_LITTLE_ENDIAN
  #if SNDLIB_USE_FLOATS
    #define MUS_OUT_FORMAT MUS_LFLOAT
  #else
    #define MUS_OUT_FORMAT MUS_LINT
  #endif
#else
  #if SNDLIB_USE_FLOATS
    #define MUS_OUT_FORMAT MUS_BFLOAT
  #else
    #define MUS_OUT_FORMAT MUS_BINT
  #endif
#endif


#define MUS_NIST_SHORTPACK 2
#define MUS_AIFF_IMA_ADPCM 99

enum {MUS_AUDIO_NO_ERROR,MUS_AUDIO_CHANNELS_NOT_AVAILABLE,MUS_AUDIO_SRATE_NOT_AVAILABLE,MUS_AUDIO_FORMAT_NOT_AVAILABLE,
      MUS_AUDIO_NO_INPUT_AVAILABLE,MUS_AUDIO_NO_OUTPUT_AVAILABLE,MUS_AUDIO_INPUT_BUSY,MUS_AUDIO_OUTPUT_BUSY,
      MUS_AUDIO_CONFIGURATION_NOT_AVAILABLE,MUS_AUDIO_INPUT_CLOSED,MUS_AUDIO_OUTPUT_CLOSED,MUS_AUDIO_IO_INTERRUPTED,
      MUS_AUDIO_NO_LINES_AVAILABLE,MUS_AUDIO_WRITE_ERROR,MUS_AUDIO_SIZE_NOT_AVAILABLE,MUS_AUDIO_DEVICE_NOT_AVAILABLE,
      MUS_AUDIO_CANT_CLOSE,MUS_AUDIO_CANT_OPEN,MUS_AUDIO_READ_ERROR,MUS_AUDIO_AMP_NOT_AVAILABLE,MUS_AUDIO_NO_OP,
      MUS_AUDIO_CANT_WRITE,MUS_AUDIO_CANT_READ,MUS_AUDIO_NO_READ_PERMISSION};

#define MUS_AUDIO_PACK_SYSTEM(n) ((n)<<16)
#define MUS_AUDIO_SYSTEM(n) (((n)>>16)&0xffff)
#define MUS_AUDIO_DEVICE(n) ((n)&0xffff)

enum {MUS_AUDIO_DEFAULT,MUS_AUDIO_DUPLEX_DEFAULT,MUS_AUDIO_ADAT_IN,MUS_AUDIO_AES_IN,MUS_AUDIO_LINE_OUT,
      MUS_AUDIO_LINE_IN,MUS_AUDIO_MICROPHONE,MUS_AUDIO_SPEAKERS,MUS_AUDIO_DIGITAL_IN,MUS_AUDIO_DIGITAL_OUT,
      MUS_AUDIO_DAC_OUT,MUS_AUDIO_ADAT_OUT,MUS_AUDIO_AES_OUT,MUS_AUDIO_DAC_FILTER,MUS_AUDIO_MIXER,
      MUS_AUDIO_LINE1,MUS_AUDIO_LINE2,MUS_AUDIO_LINE3,MUS_AUDIO_AUX_INPUT,MUS_AUDIO_CD,
      MUS_AUDIO_AUX_OUTPUT,MUS_AUDIO_SPDIF_IN,MUS_AUDIO_SPDIF_OUT,MUS_AUDIO_AMP,MUS_AUDIO_SRATE,
      MUS_AUDIO_CHANNEL,MUS_AUDIO_FORMAT,MUS_AUDIO_IMIX,MUS_AUDIO_IGAIN,MUS_AUDIO_RECLEV,
      MUS_AUDIO_PCM,MUS_AUDIO_PCM2,MUS_AUDIO_OGAIN,MUS_AUDIO_LINE,MUS_AUDIO_SYNTH,
      MUS_AUDIO_BASS,MUS_AUDIO_TREBLE,MUS_AUDIO_PORT,MUS_AUDIO_SAMPLES_PER_CHANNEL,
      MUS_AUDIO_DIRECTION
};

#define MUS_AUDIO_DEVICE_OK(a) (((a) >= MUS_AUDIO_DEFAULT) && ((a) <= MUS_AUDIO_DIRECTION))

enum {MUS_NO_ERROR,MUS_NO_FREQUENCY,MUS_NO_PHASE,MUS_NO_GEN,MUS_NO_LENGTH,
      MUS_NO_FREE,MUS_NO_DESCRIBE,MUS_NO_EQUALP,MUS_NO_DATA,MUS_NO_SCALER,
      MUS_MEMORY_ALLOCATION_FAILED,MUS_UNSTABLE_TWO_POLE_ERROR,
      MUS_INVALID_CHANNEL_FOR_FRAME,MUS_CANT_OPEN_FILE,MUS_NO_SAMPLE_INPUT,
      MUS_NO_SAMPLE_OUTPUT,MUS_NO_FRAME_INPUT,MUS_NO_FRAME_OUTPUT,
      MUS_NO_SUCH_CHANNEL,MUS_NO_FILE_NAME_PROVIDED,MUS_NO_LOCATION,MUS_NO_CHANNEL,
      MUS_NO_SUCH_FFT_WINDOW,MUS_UNSUPPORTED_DATA_FORMAT,MUS_HEADER_READ_FAILED,
      MUS_HEADER_TOO_MANY_AUXILIARY_COMMENTS,MUS_UNSUPPORTED_HEADER_TYPE,
      MUS_FILE_DESCRIPTORS_NOT_INITIALIZED,MUS_NOT_A_SOUND_FILE,MUS_FILE_CLOSED,MUS_WRITE_ERROR,
      MUS_BOGUS_FREE,MUS_BUFFER_OVERFLOW,MUS_BUFFER_UNDERFLOW,MUS_FILE_OVERFLOW,MUS_EXPONENT_OVERFLOW,
      MUS_INITIAL_ERROR_TAG};

#ifdef MACOS
  /* C's calloc/free are incompatible with Mac's SndDisposeChannel (which we can't avoid using) */
  /* realloc is enough of a mess that I'll handle each case individually */
  #define CALLOC(a,b)  NewPtrClear((a) * (b))
  #define MALLOC(a)    NewPtr((a))
  #define FREE(a)      DisposePtr((Ptr)(a))
#else
  #ifdef DEBUG_MEMORY
    #define CALLOC(a,b)  mem_calloc(a,b,__FUNCTION__,__FILE__,__LINE__)
    #define MALLOC(a)    mem_malloc(a,__FUNCTION__,__FILE__,__LINE__)
    #define FREE(a)      mem_free(a,__FUNCTION__,__FILE__,__LINE__)
    #define REALLOC(a,b) mem_realloc(a,b,__FUNCTION__,__FILE__,__LINE__)
  #else
    #define CALLOC(a,b)  calloc(a,b)
    #define MALLOC(a)    malloc(a)
    #define FREE(a)      free(a)
    #define REALLOC(a,b) realloc(a,b)
  #endif
#endif 

#define MUS_MAX_FILE_NAME 256

__BEGIN_DECLS

/* -------- sound.c -------- */

#ifdef __GNUC__
  void mus_error(int error, const char *format, ...) __attribute__ ((format (printf, 2, 3)));
  void mus_fwrite(int fd, const char *format, ...) __attribute__ ((format (printf, 2, 3)));
#else
  void mus_error __P((int error, const char *format, ...));
  void mus_fwrite __P((int fd, const char *format, ...));
#endif
void mus_error_set_handler __P((void (*new_error_handler)(int err_type, char *err_msg)));
int mus_error_make_tag __P((void));

int mus_sound_samples __P((const char *arg));
int mus_sound_frames __P((const char *arg));
int mus_sound_datum_size __P((const char *arg));
int mus_sound_data_location __P((const char *arg));
int mus_sound_chans __P((const char *arg));
int mus_sound_srate __P((const char *arg));
int mus_sound_header_type __P((const char *arg));
int mus_sound_data_format __P((const char *arg));
int mus_sound_original_format __P((const char *arg));
int mus_sound_comment_start __P((const char *arg));
int mus_sound_comment_end __P((const char *arg));
int mus_sound_length __P((const char *arg));
int mus_sound_fact_samples __P((const char *arg));
int mus_sound_distributed __P((const char *arg));
int mus_sound_write_date __P((const char *arg));
int mus_sound_type_specifier __P((const char *arg));
int mus_sound_align __P((const char *arg));
int mus_sound_bits_per_sample __P((const char *arg));
char *mus_header_type_name __P((int type));
char *mus_data_format_name __P((int format));
char *mus_sound_comment __P((const char *name));
int mus_data_format_to_bytes_per_sample __P((int format));
float mus_sound_duration __P((const char *arg));
int mus_sound_initialize __P((void));
void mus_sound_finalize __P((void));
int mus_sample_bits __P((void));
int mus_sound_override_header __P((const char *arg, int srate, int chans, int format, int type, int location, int size));
int mus_sound_forget __P((const char *name));
void mus_sound_print_cache __P((void));
int mus_sound_aiff_p __P((const char *arg));
int *mus_sound_loop_info __P((const char *arg));
void mus_sound_set_loop_info __P((const char *arg, int *loop));

int mus_sound_open_input __P((const char *arg));
int mus_sound_open_output __P((const char *arg, int srate, int chans, int data_format, int header_type, const char *comment));
int mus_sound_reopen_output __P((const char *arg, int chans, int format, int type, int data_loc));
int mus_sound_close_input __P((int fd));
int mus_sound_close_output __P((int fd, int bytes_of_data));
int mus_sound_read __P((int fd, int beg, int end, int chans, MUS_SAMPLE_TYPE **bufs));
int mus_sound_write __P((int tfd, int beg, int end, int chans, MUS_SAMPLE_TYPE **bufs));
int mus_sound_seek __P((int tfd, long offset, int origin));
int mus_sound_seek_frame __P((int tfd, int frame));
int mus_sound_max_amp __P((const char *ifile, MUS_SAMPLE_TYPE *vals));
int mus_file_to_array __P((const char *filename, int chan, int start, int samples, MUS_SAMPLE_TYPE *array));
int mus_array_to_file __P((const char *filename, MUS_SAMPLE_TYPE *ddata, int len, int srate, int channels));


/* -------- audio.c -------- */

#if (HAVE_OSS || HAVE_ALSA)
  #define ALSA_API 0
  #define OSS_API 1
#endif

void mus_audio_describe __P((void));
char *mus_audio_report __P((void));
int mus_audio_open_output __P((int dev, int srate, int chans, int format, int size));
int mus_audio_open_input __P((int dev, int srate, int chans, int format, int size));
int mus_audio_write __P((int line, char *buf, int bytes));
int mus_audio_close __P((int line));
int mus_audio_read __P((int line, char *buf, int bytes));
int mus_audio_mixer_read __P((int dev, int field, int chan, float *val));
int mus_audio_mixer_write __P((int dev, int field, int chan, float *val));
void mus_audio_save __P((void));
void mus_audio_restore __P((void));
int mus_audio_error __P((void));
int mus_audio_initialize __P((void));
char *mus_audio_error_name __P((int err));
void mus_audio_set_error __P((int err));
int mus_audio_systems __P((void));
char *mus_audio_system_name __P((int system));
char *mus_audio_moniker __P((void));

#if HAVE_OSS
  void mus_audio_set_dsp_devices __P((int cards, int *dsps, int *mixers));
  void mus_audio_dsp_devices __P((int cards, int *dsps, int *mixers));
  void mus_audio_clear_soundcard_inputs __P((void));
#endif
#if (HAVE_OSS || HAVE_ALSA)
  void mus_audio_set_oss_buffers __P((int num,int size));
  int mus_audio_api __P((void));
#endif

void mus_audio_mixer_save __P((const char *file));
void mus_audio_mixer_restore __P((const char *file));

#ifdef SUN
  void mus_audio_sun_outputs __P((int speakers, int headphones, int line_out));
#endif

#if (defined(HAVE_CONFIG_H)) && (!defined(HAVE_STRERROR))
  char *strerror __P((int errnum));
#endif



/* -------- io.c -------- */

int mus_file_open_descriptors __P((int tfd, int df, int ds, int dl));
int mus_file_set_descriptors __P((int tfd, const char *arg, int df, int ds, int dl, int dc, int dt));
int mus_file_close_descriptors __P((int tfd));
int mus_file_cleanup_descriptors __P((void));
int mus_file_open_read __P((const char *arg));
int mus_file_probe __P((const char *arg));
int mus_file_open_write __P((const char *arg));
int mus_file_create __P((const char *arg));
int mus_file_reopen_write __P((const char *arg));
int mus_file_close __P((int fd));
long mus_file_seek __P((int tfd, long offset, int origin));
int mus_file_seek_frame __P((int tfd, int frame));
int mus_file_read __P((int fd, int beg, int end, int chans, MUS_SAMPLE_TYPE **bufs));
int mus_file_read_chans __P((int fd, int beg, int end, int chans, MUS_SAMPLE_TYPE **bufs, MUS_SAMPLE_TYPE *cm));
int mus_file_write_zeros __P((int tfd, int num));
int mus_file_write __P((int tfd, int beg, int end, int chans, MUS_SAMPLE_TYPE **bufs));
int mus_file_read_any __P((int tfd, int beg, int chans, int nints, MUS_SAMPLE_TYPE **bufs, MUS_SAMPLE_TYPE *cm));
int mus_file_read_file __P((int tfd, int beg, int chans, int nints, MUS_SAMPLE_TYPE **bufs));
int mus_file_read_buffer __P((int charbuf_data_format, int beg, int chans, int nints, MUS_SAMPLE_TYPE **bufs, char *charbuf));
int mus_file_write_file __P((int tfd, int beg, int end, int chans, MUS_SAMPLE_TYPE **bufs));
int mus_file_write_buffer __P((int charbuf_data_format, int beg, int end, int chans, MUS_SAMPLE_TYPE **bufs, char *charbuf, int clipped));
char *mus_file_full_name __P((char *tok));
int mus_file_set_data_clipped __P((int tfd, int clipped));
int mus_file_set_header_type __P((int tfd, int type));
int mus_file_header_type __P((int tfd));
char *mus_file_fd_name __P((int tfd));
int mus_file_set_chans __P((int tfd, int chans));
float mus_file_prescaler __P((int tfd));
float mus_file_set_prescaler __P((int tfd, float val));

void mus_bint_to_char __P((unsigned char *j, int x));
int mus_char_to_bint __P((const unsigned char *inp));
void mus_lint_to_char __P((unsigned char *j, int x));
int mus_char_to_lint __P((const unsigned char *inp));
int mus_char_to_uninterpreted_int __P((const unsigned char *inp));
void mus_bfloat_to_char __P((unsigned char *j, float x));
float mus_char_to_bfloat __P((const unsigned char *inp));
void mus_lfloat_to_char __P((unsigned char *j, float x));
float mus_char_to_lfloat __P((const unsigned char *inp));
void mus_bshort_to_char __P((unsigned char *j, short x));
short mus_char_to_bshort __P((const unsigned char *inp));
void mus_lshort_to_char __P((unsigned char *j, short x));
short mus_char_to_lshort __P((const unsigned char *inp));
void mus_ubshort_to_char __P((unsigned char *j, unsigned short x));
unsigned short mus_char_to_ubshort __P((const unsigned char *inp));
void mus_ulshort_to_char __P((unsigned char *j, unsigned short x));
unsigned short mus_char_to_ulshort __P((const unsigned char *inp));
double mus_char_to_ldouble __P((const unsigned char *inp));
double mus_char_to_bdouble __P((const unsigned char *inp));
void mus_bdouble_to_char __P((unsigned char *j, double x));
void mus_ldouble_to_char __P((unsigned char *j, double x));
unsigned int mus_char_to_ubint __P((const unsigned char *inp));
unsigned int mus_char_to_ulint __P((const unsigned char *inp));

#if LONG_INT_P
  MUS_SAMPLE_TYPE *mus_table2ptr __P((int arr));
  int mus_ptr2table __P((MUS_SAMPLE_TYPE *arr));
  void mus_untableptr __P((int ip_1));
  #define MUS_SAMPLE_ARRAY(n) mus_table2ptr((int)(n))
  #define MUS_MAKE_SAMPLE_ARRAY(size) mus_ptr2table((MUS_SAMPLE_TYPE *)CALLOC((size),sizeof(MUS_SAMPLE_TYPE)))
  #define MUS_FREE_SAMPLE_ARRAY(p) mus_untableptr((int)(p))
#else
  #define MUS_SAMPLE_ARRAY(n) ((MUS_SAMPLE_TYPE *)(n))
  #define MUS_MAKE_SAMPLE_ARRAY(size) ((MUS_SAMPLE_TYPE *)CALLOC((size),sizeof(MUS_SAMPLE_TYPE)))
  #define MUS_FREE_SAMPLE_ARRAY(p) FREE((void *)(p))
#endif

#ifdef CLM
  /* these are needed to clear a saved lisp image to the just-initialized state */
  void reset_io_c __P((void));
  void reset_headers_c __P((void));
  void reset_audio_c __P((void));
  void set_rt_audio_p __P((int rt));
#endif



/* -------- headers.c -------- */

int mus_header_samples __P((void));
int mus_header_data_location __P((void));
int mus_header_chans __P((void));
int mus_header_srate __P((void));
int mus_header_type __P((void));
int mus_header_format __P((void));
int mus_header_distributed __P((void));
int mus_header_comment_start __P((void));
int mus_header_comment_end __P((void));
int mus_header_type_specifier __P((void));
int mus_header_bits_per_sample __P((void));
int mus_header_fact_samples __P((void));
int mus_header_block_align __P((void));
int mus_header_loop_mode __P((int which));
int mus_header_loop_start __P((int which));
int mus_header_loop_end __P((int which));
int mus_header_mark_position __P((int id));
int mus_header_base_note __P((void));
int mus_header_base_detune __P((void));
void mus_header_set_raw_defaults __P((int sr, int chn, int frm));
int mus_header_true_length __P((void));
int mus_header_original_format __P((void));
int mus_header_data_format_to_bytes_per_sample __P((void));
int mus_samples_to_bytes __P((int format, int size));
int mus_bytes_to_samples __P((int format, int size));
int mus_header_write_next_header __P((int chan, int srate, int chans, int loc, int siz, int format, const char *comment, int len));
int mus_header_read_with_fd __P((int chan));
int mus_header_read __P((const char *name));
int mus_header_write __P((const char *name, int type, int srate, int chans, int loc, int size, int format, const char *comment, int len));
int mus_header_write_with_fd __P((int chan, int type, int in_srate, int in_chans, int loc, int size, int format, const char *comment, int len));
int mus_header_update_with_fd __P((int chan, int type, int siz));
int mus_header_update __P((const char *name, int type, int size, int srate, int format, int chans, int loc));
int mus_header_aux_comment_start __P((int n));
int mus_header_aux_comment_end __P((int n));
int mus_header_update_comment __P((const char *name, int loc, const char *comment, int len, int typ));
int mus_header_initialize __P((void));
void mus_header_snd_set_header __P((int in_srate, int in_chans, int in_format));
int mus_header_aiff_p __P((void));
int mus_header_writable __P((int type, int format));
void mus_header_set_aiff_loop_info __P((int *data));
int mus_header_sf2_entries __P((void));
char *mus_header_sf2_name __P((int n));
int mus_header_sf2_start __P((int n));
int mus_header_sf2_end __P((int n));
int mus_header_sf2_loop_start __P((int n));
int mus_header_sf2_loop_end __P((int n));

void mus_header_set_aifc __P((int val)); /* backwards compatibility, sort of */

/* -------- sndlib2scm.c -------- */

void mus_sndlib2scm_initialize __P((void));



#ifdef DEBUG_MEMORY
  /* snd-utils.c (only used in conjunction with Snd's memory tracking functions) */
  void *mem_calloc __P((size_t len, size_t size, char *func, char *file, int line));
  void *mem_malloc __P((size_t len, char *func, char *file, int line));
  void mem_free __P((void *ptr, char *func, char *file, int line));
  void *mem_realloc __P((void *ptr, size_t size, char *func, char *file, int line));
#endif

__END_DECLS

#endif
