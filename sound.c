/* sound.c */

/* TODO: make this thread-safe by wrapping locks around the header/data base references */
/*       (if using gdbm, this would also need to handle the gdbm file pointer differently) */
/*       if not using gdbm, it might be better to use a hash table in place of the sound_file array */

#if defined(HAVE_CONFIG_H)
  #include "config.h"
#else
  #define HAVE_VPRINTF 1
#endif

#include <math.h>
#include <stdio.h>
#if (!defined(HAVE_CONFIG_H)) || (defined(HAVE_FCNTL_H))
  #include <fcntl.h>
#endif
#include <signal.h>
#if (!defined(HAVE_CONFIG_H)) || (defined(HAVE_LIMITS_H))
  #include <limits.h>
#endif
#include <errno.h>
#include <stdlib.h>

#if (defined(NEXT) || (defined(HAVE_LIBC_H) && (!defined(HAVE_UNISTD_H))))
  #include <libc.h>
#else
  #if (!(defined(_MSC_VER))) && (!(defined(MPW_C)))
    #include <unistd.h>
  #endif
  #if (!defined(HAVE_CONFIG_H)) || (defined(HAVE_STRING_H))
    #include <string.h>
  #endif
#endif

#include <ctype.h>
#include <stddef.h>

#include "sndlib.h"

#if MACOS
  #if (!defined(MPW_C))
    #include <time.h>
    #include <stat.h>
  #endif
#else
  #include <sys/types.h>
  #include <sys/stat.h>
  #include <time.h>
#endif

#include <stdarg.h>

#if HAVE_GDBM

#include <gdbm.h>
#define SNDLIB_GDBM_FILENAME "sndlib.gdbm"
/* if gdbm was requested, the sound data base is saved in this file 
 *   in this case, the actual structs use constant-size arrays to simplify
 *   references to the data base, so we need to know in advance the max
 *   number of channels in a sound file; this will affect only the max-amp
 *   field, which is often not used anyway.
 */
#define SNDLIB_GDBM_MAX_CHANS 32
static datum our_key;
static datum gdbm_key(char *name) 
{
  our_key.dptr = (char *)name; 
  our_key.dsize = strlen(name) + 1; 
  return(our_key);
}

static void gdbm_errmsg(char *msg) {mus_error(MUS_CANT_OPEN_FILE,msg);}
static GDBM_FILE gdbmf = NULL;
static GDBM_FILE gdbm_fd(void) 
{
  if (gdbmf == NULL) 
    gdbmf = gdbm_open(SNDLIB_GDBM_FILENAME,0,GDBM_WRCREAT,0666,gdbm_errmsg);
  return(gdbmf);
}

#endif


static mus_error_handler_t *mus_error_handler = NULL;

mus_error_handler_t *mus_error_set_handler(mus_error_handler_t *new_error_handler) 
{
  mus_error_handler_t *old_handler;
  old_handler = mus_error_handler;
  mus_error_handler = new_error_handler;
  return(old_handler);
}

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
      fputc('\n',stderr);
    }
#else
  fprintf(stderr,"error: %d",error);
  fputc('\n',stderr);
#endif
}

static mus_print_handler_t *mus_print_handler = NULL;

mus_print_handler_t *mus_print_set_handler(mus_print_handler_t *new_print_handler) 
{
  mus_print_handler_t *old_handler;
  old_handler = mus_print_handler;
  mus_print_handler = new_print_handler;
  return(old_handler);
}

void mus_print(const char *format, ...)
{
#if HAVE_VPRINTF
  va_list ap;
  if (mus_print_handler)
    {
      va_start(ap,format);
      vsprintf(mus_error_buffer,format,ap);
      va_end(ap);
      (*mus_print_handler)(mus_error_buffer);
    }
  else
    {
      va_start(ap,format);
      vfprintf(stdout,format,ap);
      va_end(ap);
    }
#endif
}

static const char *mus_initial_error_names[] = {
  "no error","no frequency method","no phase method","no gen method","no length method",
  "no free method","no describe method","no data method","no scaler method",
  "memory allocation failed","unstable two pole error",
  "can't open file","no sample input","no sample output",
  "no such channel","no file name provided","no location method","no channel method",
  "no such fft window","unsupported data format","header read failed",
  "header has too many auxiliary comments","unsupported header type",
  "file descriptors not initialized","not a sound file","file closed","write error",
  "bogus free","buffer overflow","buffer underflow","file overflow","exponent overflow",
  "header write failed","cant open temp file","interrupted",

  "audio channels not available","audio srate not available","audio format not available",
  "no audio input available","no audio output available","audio input busy","audio output busy",
  "audio configuration not available","audio input closed","audio output closed","audio io interrupted",
  "no audio lines available","audio write error","audio size not available","audio device not available",
  "can't close audio","can't open audio","audio read error","audio amp not available","audio no op",
  "can't write audio","can't read audio","no audio read permission","can't close file"};

static char **mus_error_names = NULL;
static int mus_error_names_size = 0;

static int mus_error_tag = MUS_INITIAL_ERROR_TAG;
int mus_make_error(char *error_name) 
{
  int new_error,err,len,i;
  new_error = mus_error_tag++;
  err = new_error - MUS_INITIAL_ERROR_TAG;
  if (error_name)
    {
      if (err >= mus_error_names_size)
	{
	  if (mus_error_names_size == 0)
	    {
	      mus_error_names_size = 8;
	      mus_error_names = (char **)CALLOC(mus_error_names_size,sizeof(char *));
	    }
	  else
	    {
#ifndef MACOS
	      len = mus_error_names_size;
	      mus_error_names_size += 8;
	      mus_error_names = (char **)REALLOC(mus_error_names,mus_error_names_size * sizeof(char *));
	      for (i=len;i<mus_error_names_size;i++) mus_error_names[i] = NULL;
#endif
	    }
	}
      len = strlen(error_name);
      mus_error_names[err] = (char *)CALLOC(len+1,sizeof(char));
      strcpy(mus_error_names[err],error_name);
    }
  return(new_error);
}

const char *mus_error_to_string(int err)
{
  if (err < MUS_INITIAL_ERROR_TAG)
    return(mus_initial_error_names[err]);
  else
    {
      err -= MUS_INITIAL_ERROR_TAG;
      if ((mus_error_names) && (err < mus_error_names_size))
	return(mus_error_names[err]);
      else return("unknown mus error");
    }
}

/* sound.c local error wrappers */
static char *sound_err_buf = NULL;
static const char *local_filename = NULL,*local_func = NULL;
static int local_line = 0;
static mus_error_handler_t *old_sound_handler = NULL;

static void sound_mus_error(int type, char *msg)
{
  if (old_sound_handler)
    {
      if (local_filename)
	{
	  if (sound_err_buf == NULL) sound_err_buf = (char *)CALLOC(512,sizeof(char));
	  sprintf(sound_err_buf,"%s\n  [sound.c[%d] %s: %s]",
		  msg,local_line,local_func,local_filename);
	  (*old_sound_handler)(type,sound_err_buf);
	}
      else (*old_sound_handler)(type,msg);
    }
  else fprintf(stderr,sound_err_buf); /* ?? */
}

static void set_sound_error(const char *lfile, int line, const char *sfunc)
{
  local_filename = lfile;
  local_line = line;
  local_func = sfunc;
  if (old_sound_handler != sound_mus_error)
    old_sound_handler = mus_error_set_handler(sound_mus_error);
}

static void unset_sound_error(void)
{
  if (old_sound_handler == sound_mus_error)
    mus_error_set_handler(NULL);
  else mus_error_set_handler(old_sound_handler);
}



#ifndef MPW_C
static time_t file_write_date(const char *filename)
{
  struct stat statbuf;
  int err;
  err = stat(filename,&statbuf);
  if (err < 0) return(err);
  return((time_t)(statbuf.st_mtime));
}
#else
static int file_write_date(const char *filename) {return(1);}
#endif

static int sndlib_initialized = 0;

int mus_sound_initialize(void)
{
  int err = MUS_NO_ERROR;
  if (!sndlib_initialized)
    {
      sndlib_initialized = 1;
      mus_error_buffer = (char *)CALLOC(256,sizeof(char));
      if (mus_error_buffer == NULL) return(MUS_ERROR);
      err = mus_header_initialize();
      if (err == MUS_NO_ERROR) err = mus_audio_initialize();
      if (err == MUS_ERROR) {FREE(mus_error_buffer); return(MUS_ERROR);}
    }
  return(MUS_NO_ERROR);
}

void mus_sound_finalize(void)
{
#if HAVE_GDBM
  if (gdbmf) gdbm_close(gdbmf);
#endif
}

int mus_sample_bits(void)
{
  /* this to check for inconsistent loads */
#if SNDLIB_USE_FLOATS
  return(0);
#else
  return(MUS_SAMPLE_BITS);
#endif
}

#if HAVE_GDBM

typedef struct {
  int samples, datum_size, data_location, srate, chans, header_type, data_format, original_sound_format, true_file_length;
  int comment_start, comment_end, header_distributed, type_specifier, bits_per_sample, fact_samples, block_align;
  int write_date;
  int markers,base_detune,base_note;
  int aux_comment_start[4],aux_comment_end[4];
  int loop_modes[2],loop_starts[2],loop_ends[2];
  int marker_ids[4],marker_positions[4];
  MUS_SAMPLE_TYPE max_amps[SNDLIB_GDBM_MAX_CHANS*2];
  int max_amps_ok;
} sound_file;

static datum our_contents;
static datum gdbm_contents(sound_file *sf)
{
  our_contents.dptr = (void *)sf; 
  our_contents.dsize = sizeof(sound_file);
  return(our_contents);
}

#else

typedef struct {
  char *file_name;  /* full path -- everything is keyed to this name */
  int table_pos;
  int *aux_comment_start,*aux_comment_end;
  int *loop_modes,*loop_starts,*loop_ends;
  int markers,base_detune,base_note;
  int *marker_ids,*marker_positions;
  int samples, datum_size, data_location, srate, chans, header_type, data_format, original_sound_format, true_file_length;
  int comment_start, comment_end, header_distributed, type_specifier, bits_per_sample, fact_samples, block_align;
  int write_date;
  MUS_SAMPLE_TYPE *max_amps;
} sound_file;

static int sound_table_size = 0;
static sound_file **sound_table = NULL;

#endif

static void free_sound_file(sound_file *sf)
{
  if (sf)
#if HAVE_GDBM
    free(sf);
#else
    {
      sound_table[sf->table_pos] = NULL;
      if (sf->aux_comment_start) FREE(sf->aux_comment_start);
      if (sf->aux_comment_end) FREE(sf->aux_comment_end);
      if (sf->file_name) FREE(sf->file_name);
      if (sf->loop_modes) FREE(sf->loop_modes);
      if (sf->loop_starts) FREE(sf->loop_starts);
      if (sf->loop_ends) FREE(sf->loop_ends);
      if (sf->marker_ids) FREE(sf->marker_ids);
      if (sf->marker_positions) FREE(sf->marker_positions);
      if (sf->max_amps) FREE(sf->max_amps);
      FREE(sf);
    }
#endif
}

static sound_file *add_to_sound_table(const char *name)
{
#if HAVE_GDBM
  return((sound_file *)CALLOC(1,sizeof(sound_file)));
#else
  int i,pos;
#ifdef MACOS
  sound_file **ptr;
#endif
  pos = -1;
  for (i=0;i<sound_table_size;i++)
    if (sound_table[i] == NULL) 
      {
	pos = i;
	break;
      }
  if (pos == -1)
    {
      pos = sound_table_size;
      sound_table_size += 16;
      if (sound_table == NULL)
	sound_table = (sound_file **)CALLOC(sound_table_size,sizeof(sound_file *));
      else 
	{
#ifdef MACOS
	  ptr = (sound_file **)CALLOC(sound_table_size,sizeof(sound_file *));
	  for (i=0;i<pos;i++) ptr[i] = sound_table[i];
	  FREE(sound_table);
	  sound_table = ptr;
#else
	  sound_table = (sound_file **)REALLOC(sound_table,sound_table_size * sizeof(sound_file *));
#endif
	  for (i=pos;i<sound_table_size;i++) sound_table[i] = NULL;
	}
    }
  sound_table[pos] = (sound_file *)CALLOC(1,sizeof(sound_file));
  sound_table[pos]->table_pos = pos;
  sound_table[pos]->file_name = (char *)CALLOC(strlen(name)+1,sizeof(char));
  strcpy(sound_table[pos]->file_name,name);
  return(sound_table[pos]);
#endif
}

int mus_sound_forget(const char *name)
{
#if HAVE_GDBM
  return(gdbm_delete(gdbm_fd(),gdbm_key((char *)name)));
#else
  int i;
  if (name)
    {
      for (i=0;i<sound_table_size;i++)
	{
	  if (sound_table[i])
	    {
	      if (strcmp(name,sound_table[i]->file_name) == 0)
		{
		  free_sound_file(sound_table[i]);
		  return(MUS_NO_ERROR);
		}
	    }
	}
    }
  return(MUS_ERROR);
#endif
}

static sound_file *check_write_date(const char *name, sound_file *sf)
{
  int chan,data_size,date;
  if (sf)
    {
      date = file_write_date(name);
      if (date == sf->write_date)
	return(sf);
      else 
	{
	  if (sf->header_type == MUS_RAW)
	    {
	      /* sound has changed since we last read it, but it has no header, so
	       * the only sensible thing to check is the new length (i.e. caller
	       * has set other fields by hand)
	       */
	      sf->write_date = date;
	      chan = mus_file_open_read(name);
	      data_size = lseek(chan,0L,SEEK_END);
	      sf->true_file_length = data_size;
	      sf->samples = mus_bytes_to_samples(sf->data_format,data_size);
	      close(chan);  
	      return(sf);
	    }
	  /* otherwise our data base is out-of-date, so clear it out */
	  free_sound_file(sf);
	}
    }
  return(NULL);
}

static sound_file *find_sound_file(const char *name)
{
#if HAVE_GDBM
  datum contents;
  contents = gdbm_fetch(gdbm_fd(),gdbm_key((char *)name));
  return(check_write_date(name,(sound_file *)(contents.dptr)));
#else
  int i;
  if (name)
    {
      for (i=0;i<sound_table_size;i++)
	{
	  if (sound_table[i])
	    {
	      if (strcmp(name,sound_table[i]->file_name) == 0)
		return(check_write_date(name,sound_table[i]));
	    }
	}
    }
  return(NULL);
#endif
}

static void display_sound_file_entry(const char *name, sound_file *sf)
{
  int i,lim;
#ifndef MPW_C
  time_t date;
#endif
  char timestr[64];
#ifndef MPW_C
  date = sf->write_date;
  if (date != 0)
    {
#if (!defined(HAVE_CONFIG_H)) || defined(HAVE_STRFTIME)
      strftime(timestr,64,"%a %d-%b-%Y %H:%M:%S",localtime(&date));
#else
      sprintf(timestr,"%d",date);
#endif
    }
  else sprintf(timestr,"(date cleared)");
#else
  sprintf(timestr,"(date unknown)");
#endif
  fprintf(stdout,"  %s: %s, chans: %d, srate: %d, type: %s, format: %s, samps: %d",
	  name,
	  timestr,
	  sf->chans,
	  sf->srate,
	  mus_header_type_name(sf->header_type),
	  mus_data_format_name(sf->data_format),
	  sf->samples);
  if (sf->loop_modes)
    {
      if (sf->loop_modes[0] != 0)
	fprintf(stdout,", loop: %d to %d",sf->loop_starts[0],sf->loop_ends[0]);
      if (sf->loop_modes[1] != 0)
	fprintf(stdout,", loop: %d to %d,",sf->loop_starts[1],sf->loop_ends[1]);
      fprintf(stdout,", base: %d, detune: %d",sf->base_note,sf->base_detune);
    }
#if HAVE_GDBM
  if (sf->max_amps_ok)
#else
  if (sf->max_amps)
#endif
    {
      lim = sf->chans;
      if (lim > 0)
	{
#if HAVE_GDBM
	  if (lim > SNDLIB_GDBM_MAX_CHANS) lim = SNDLIB_GDBM_MAX_CHANS;
#else
	  if (lim > 64) lim = 64;
#endif
	  fprintf(stdout,", max amps:");
	  for (i=0;i<lim;i++)
	    {
	      if (i>1) fprintf(stdout,",");
	      fprintf(stdout," %.3f at %.3f ",
		      MUS_SAMPLE_TO_FLOAT(sf->max_amps[i+1]),
		      (float)(sf->max_amps[i])/(float)(sf->srate));
	    }
	}
    }
  fprintf(stdout,"\n");
}

void mus_sound_print_cache(void)
{
  sound_file *sf;
#if HAVE_GDBM
  GDBM_FILE dbf;
  datum key;
  datum content;
  dbf = gdbm_fd();
  if (dbf)
    {
      fprintf(stdout,"sound table:\n");
      key = gdbm_firstkey(dbf);
      while (key.dptr) 
	{
	  content = gdbm_fetch(dbf,key);
	  sf = (sound_file *)(content.dptr);
	  if (sf) display_sound_file_entry((char *)(key.dptr),sf);
	  free(sf);
	  key = gdbm_nextkey(dbf,key);
        }
      fprintf(stdout,"\n"); 
      fflush(stdout);
    }
#else
  int i;
  fprintf(stdout,"sound table:\n");
  for (i=0;i<sound_table_size;i++)
    {
      sf = sound_table[i];
      if (sf) display_sound_file_entry(sf->file_name,sound_table[i]);
    }
  fprintf(stdout,"\n"); 
  fflush(stdout);
#endif
}


static void fill_sf_record(const char *name, sound_file *sf)
{
  int i;
#if HAVE_GDBM
  int err;
#endif
  sf->data_location = mus_header_data_location();
  sf->samples = mus_header_samples();
  if (sf->samples < 0) sf->samples = 0;
  sf->data_format = mus_header_format();
  sf->srate = mus_header_srate();
  if (sf->srate < 0) sf->srate = 0;
  sf->chans = mus_header_chans();
  if (sf->chans < 0) sf->chans = 0;
  sf->datum_size = mus_header_data_format_to_bytes_per_sample();
  sf->header_type = mus_header_type();
  sf->original_sound_format = mus_header_original_format();
  sf->true_file_length = mus_header_true_length();
  sf->comment_start = mus_header_comment_start();
  sf->comment_end = mus_header_comment_end();
  if ((sf->header_type == MUS_AIFC) || 
      (sf->header_type == MUS_AIFF) || 
      (sf->header_type == MUS_RIFF))

    {
#if (!HAVE_GDBM)
      sf->aux_comment_start = (int *)CALLOC(4,sizeof(int));
      sf->aux_comment_end = (int *)CALLOC(4,sizeof(int));
#endif
      for (i=0;i<4;i++)
	{
	  sf->aux_comment_start[i] = mus_header_aux_comment_start(i);
	  sf->aux_comment_end[i] = mus_header_aux_comment_end(i);
	}
    }
  sf->header_distributed = mus_header_distributed();
  sf->type_specifier = mus_header_type_specifier();
  sf->bits_per_sample = mus_header_bits_per_sample();
  sf->fact_samples = mus_header_fact_samples();
  sf->block_align = mus_header_block_align();
  sf->write_date = file_write_date(name);
  if (mus_header_loop_mode(0) != 0)
    {
#if (!HAVE_GDBM)
      sf->loop_modes = (int *)CALLOC(2,sizeof(int));
      sf->loop_starts = (int *)CALLOC(2,sizeof(int));
      sf->loop_ends = (int *)CALLOC(2,sizeof(int));
#endif
      for (i=0;i<2;i++)
	{
	  sf->loop_modes[i] = mus_header_loop_mode(i);
	  if ((sf->header_type == MUS_AIFF) || (sf->header_type == MUS_AIFC))
	    {
	      sf->loop_starts[i] = mus_header_mark_position(mus_header_loop_start(i)); 
	      sf->loop_ends[i] = mus_header_mark_position(mus_header_loop_end(i));
	    }
	  else
	    {
	      sf->loop_starts[i] = mus_header_loop_start(i); 
	      sf->loop_ends[i] = mus_header_loop_end(i);
	    }
	}
      sf->base_detune = mus_header_base_detune();
      sf->base_note = mus_header_base_note();
    }
  /* aux comments */
#if HAVE_GDBM
  sf->max_amps_ok = 0;
  err = gdbm_store(gdbm_fd(),gdbm_key((char *)name),gdbm_contents(sf),GDBM_REPLACE);
  if (err != 0) 
    mus_error(MUS_CANT_OPEN_FILE,"%s %d = %s \n  [%s[%d] %s]",
	      name,i,gdbm_strerror(gdbm_errno),
	      __FILE__,__LINE__,__FUNCTION__,i);
#endif
}

static sound_file *read_sound_file_header_with_fd(int fd, const char *arg)
{
  int err=0;
  sound_file *sf = NULL;
  mus_sound_initialize();
  err = mus_header_read_with_fd(fd);
  if (err == MUS_ERROR) return(NULL);
  sf = add_to_sound_table(arg);
  fill_sf_record(arg,sf);
  return(sf);
}

static sound_file *read_sound_file_header_with_name(const char *name)
{
  sound_file *sf = NULL;
  mus_sound_initialize();
  if (mus_header_read(name) != MUS_ERROR)
    {
      sf = add_to_sound_table(name);
      fill_sf_record(name,sf);
    }
  return(sf);
}

static sound_file *getsf(const char *arg) 
{
  sound_file *sf = NULL;
  if (arg == NULL) return(NULL);
  if ((sf = find_sound_file(arg)) == NULL)
    return(read_sound_file_header_with_name(arg));
  return(sf);
}

#if HAVE_GDBM
  #define gdbm_sf_free(arg) free(arg)
#else
  #define gdbm_sf_free(arg)
#endif

#define MUS_SF(Filename,Expression) \
  int val = MUS_ERROR; \
  sound_file *sf; \
  set_sound_error(Filename,__LINE__,__FUNCTION__); \
  sf = getsf(Filename); \
  unset_sound_error(); \
  if (sf) \
    { \
      val = Expression; \
      gdbm_sf_free(sf); \
    } \
  return(val)

int mus_sound_samples (const char *arg)         {MUS_SF(arg,sf->samples);}
int mus_sound_frames (const char *arg)          {MUS_SF(arg,(sf->samples / sf->chans));}
int mus_sound_datum_size (const char *arg)      {MUS_SF(arg,sf->datum_size);}
int mus_sound_data_location (const char *arg)   {MUS_SF(arg,sf->data_location);}
int mus_sound_chans (const char *arg)           {MUS_SF(arg,sf->chans);}
int mus_sound_srate (const char *arg)           {MUS_SF(arg,sf->srate);}
int mus_sound_header_type (const char *arg)     {MUS_SF(arg,sf->header_type);}
int mus_sound_data_format (const char *arg)     {MUS_SF(arg,sf->data_format);}
int mus_sound_original_format (const char *arg) {MUS_SF(arg,sf->original_sound_format);}
int mus_sound_comment_start (const char *arg)   {MUS_SF(arg,sf->comment_start);}
int mus_sound_comment_end (const char *arg)     {MUS_SF(arg,sf->comment_end);}
int mus_sound_length (const char *arg)          {MUS_SF(arg,sf->true_file_length);}
int mus_sound_fact_samples (const char *arg)    {MUS_SF(arg,sf->fact_samples);}
int mus_sound_distributed (const char *arg)     {MUS_SF(arg,sf->header_distributed);}
int mus_sound_write_date (const char *arg)      {MUS_SF(arg,sf->write_date);}
int mus_sound_type_specifier (const char *arg)  {MUS_SF(arg,sf->type_specifier);}
int mus_sound_align (const char *arg)           {MUS_SF(arg,sf->block_align);}
int mus_sound_bits_per_sample (const char *arg) {MUS_SF(arg,sf->bits_per_sample);}

float mus_sound_duration(const char *arg) 
{
  float val = -1.0;
  sound_file *sf; 
  set_sound_error(arg,__LINE__,__FUNCTION__);
  sf = getsf(arg); 
  unset_sound_error();
  if (sf) 
    {
      val = (float)(sf->samples) / ((float)(sf->chans) * (float)(sf->srate));
#if HAVE_FINITE 
      if (!(finite(val)))
	{
	  val = (float)(((double)(sf->samples) / (double)(sf->chans)) / (double)(sf->srate)); 
	  if (!(finite(val))) val = 0.0;
	}
#endif
#if HAVE_GDBM
      free(sf);
#endif
    }
  return(val);
}

int *mus_sound_loop_info(const char *arg)
{
  sound_file *sf; 
  int *info;
  sf = getsf(arg); 
#if HAVE_GDBM
  if ((sf) && (sf->loop_modes[0] != 0))
#else
  if ((sf) && (sf->loop_modes))
#endif
    {
      info = (int *)CALLOC(6,sizeof(int));
      if (sf->loop_modes[0] != 0)
	{
	  info[0] = sf->loop_starts[0];
	  info[1] = sf->loop_ends[0];
	}
      if (sf->loop_modes[1] != 0)
	{
	  info[2] = sf->loop_starts[1];
	  info[3] = sf->loop_ends[1];
	}
      info[4] = sf->base_note;
      info[5] = sf->base_detune;
#if HAVE_GDBM
      free(sf);
#endif
      return(info);
    }
  else return(NULL);
}

void mus_sound_set_loop_info(const char *arg, int *loop)
{
  sound_file *sf; 
  sf = getsf(arg); 
  if (sf)
    {
#if (!HAVE_GDBM)
      if (sf->loop_modes == NULL)
	{
	  sf->loop_modes = (int *)CALLOC(2,sizeof(int));
	  sf->loop_starts = (int *)CALLOC(2,sizeof(int));
	  sf->loop_ends = (int *)CALLOC(2,sizeof(int));
	}
#endif
      if ((loop[0] != 0) || (loop[1] != 0))
	{
	  sf->loop_modes[0] = 1;
	  sf->loop_starts[0] = loop[0];
	  sf->loop_ends[0] = loop[1];
	}
      if ((loop[2] != 0) || (loop[3] != 0))
	{
	  sf->loop_modes[1] = 1;
	  sf->loop_starts[1] = loop[2];
	  sf->loop_ends[1] = loop[3];
	}
      if (loop[4] != 0) sf->base_note = loop[4];
      sf->base_detune = loop[5];
#if HAVE_GDBM
      if (gdbm_store(gdbm_fd(),gdbm_key((char *)arg),gdbm_contents(sf),GDBM_REPLACE) != 0)
	mus_error(MUS_CANT_OPEN_FILE,"%s: %s \n  [%s[%d] %s]",
		  arg,gdbm_strerror(gdbm_errno),
		  __FILE__,__LINE__,__FUNCTION__);
      free(sf);
#endif      
    }
}

int mus_sound_aiff_p(const char *arg) 
{
  return(mus_sound_header_type(arg) == MUS_AIFF);
}

char *mus_sound_comment(const char *name)
{
  int start,end,fd,len,full_len;
  char *sc = NULL,*auxcom;
  sound_file *sf = NULL;
  set_sound_error(name,__LINE__,__FUNCTION__);
  sf = getsf(name); 
  unset_sound_error();
  if (sf == NULL) return(NULL);
  start = mus_sound_comment_start(name);
  end = mus_sound_comment_end(name);
  if (end == 0) 
    {
      if (mus_sound_header_type(name) == MUS_RIFF) 
	return(mus_header_riff_aux_comment(name,sf->aux_comment_start,sf->aux_comment_end));
      if ((mus_sound_header_type(name) == MUS_AIFF) || (mus_sound_header_type(name) == MUS_AIFC)) 
	return(mus_header_aiff_aux_comment(name,sf->aux_comment_start,sf->aux_comment_end));
      return(NULL);
    }
  len = end-start+1;
  if (len > 0)
    {
      /* open and get the comment */
      fd = mus_file_open_read(name);
      if (fd == -1) return(NULL);
      lseek(fd,start,SEEK_SET);
      sc = (char *)CALLOC(len+1,sizeof(char)); /* len+1 calloc'd => we'll always have a trailing null */
      read(fd,sc,len);
      close(fd);
#ifndef MACOS
      if ((mus_sound_header_type(name) == MUS_AIFF) || (mus_sound_header_type(name) == MUS_AIFC)) 
	{
	  auxcom = mus_header_aiff_aux_comment(name,sf->aux_comment_start,sf->aux_comment_end);
	  if (auxcom)
	    {
	      full_len = strlen(auxcom) + strlen(sc) + 2;
	      sc = (char *)REALLOC(sc,full_len * sizeof(char));
	      strcat(sc,"\n");
	      strcat(sc,auxcom);
	    }
	}
#endif
    }
  return(sc);
}

int mus_sound_open_input (const char *arg) 
{
  int fd;
  sound_file *sf = NULL;
  mus_sound_initialize();
  fd = mus_file_open_read(arg);
  if (fd != MUS_ERROR)
    {
      if ((sf = find_sound_file(arg)) == NULL)
	sf = read_sound_file_header_with_fd(fd,arg);
    }
  if (sf) 
    {
      mus_file_set_descriptors(fd,arg,sf->data_format,sf->datum_size,sf->data_location,sf->chans,sf->header_type);
      mus_file_seek(fd,sf->data_location,SEEK_SET);
#if HAVE_GDBM
      free(sf);
#endif
    }
  else mus_error(MUS_CANT_OPEN_FILE,"can't open %s: %s\n  [%s[%d] %s]",
		 arg,strerror(errno),
		 __FILE__,__LINE__,__FUNCTION__);
  return(fd);
}

int mus_sound_open_output (const char *arg, int srate, int chans, int data_format, int header_type, const char *comment)
{
  int fd = MUS_ERROR,err,comlen = 0;
  mus_sound_forget(arg);
  if (comment) comlen = strlen(comment);
  mus_sound_initialize();
  set_sound_error(arg,__LINE__,__FUNCTION__);
  err = mus_header_write(arg,header_type,srate,chans,0,0,data_format,comment,comlen);
  unset_sound_error();
  if (err != MUS_ERROR)
    {
      fd = mus_file_open_write(arg);
      mus_file_set_descriptors(fd,
			       arg,
			       data_format,
			       mus_data_format_to_bytes_per_sample(data_format),
			       mus_header_data_location(),
			       chans,
			       header_type);
    }
  return(fd);
}

int mus_sound_reopen_output(const char *arg, int chans, int format, int type, int data_loc)
{
  int fd;
  mus_sound_initialize();
  set_sound_error(arg,__LINE__,__FUNCTION__);
  fd = mus_file_reopen_write(arg);
  mus_file_set_descriptors(fd,
			   arg,
			   format,
			   mus_data_format_to_bytes_per_sample(format),
			   data_loc,
			   chans,
			   type);
  unset_sound_error();
  return(fd);
}

int mus_sound_close_input (int fd) 
{
  return(mus_file_close(fd)); /* this closes the clm file descriptors */
}

int mus_sound_close_output (int fd, int bytes_of_data) 
{
  mus_sound_forget(mus_file_fd_name(fd));
  mus_header_update_with_fd(fd,mus_file_header_type(fd),bytes_of_data);
  return(mus_file_close(fd));
}

int mus_sound_read (int fd, int beg, int end, int chans, MUS_SAMPLE_TYPE **bufs) 
{
  return(mus_file_read(fd,beg,end,chans,bufs));
}

int mus_sound_write (int tfd, int beg, int end, int chans, MUS_SAMPLE_TYPE **bufs) 
{
  return(mus_file_write(tfd,beg,end,chans,bufs));
}

int mus_sound_seek (int tfd, long offset, int origin) 
{
  return(mus_file_seek(tfd,offset,origin));
}

int mus_sound_seek_frame(int tfd, int frame)
{
  return(mus_file_seek_frame(tfd,frame));
}

int mus_sound_override_header(const char *arg, int srate, int chans, int format, int type, int location, int size)
{
  sound_file *sf; 
  /* perhaps once a header has been over-ridden, we should not reset the relevant fields upon re-read? */
  sf = getsf(arg); 
  if (sf)
    {
      if (location != -1) sf->data_location = location;
      if (size != -1) sf->samples = size;
      if (format != -1) 
	{
	  sf->data_format = format;
	  sf->datum_size = mus_data_format_to_bytes_per_sample(format);
	}
      if (srate != -1) sf->srate = srate;
      if (chans != -1) sf->chans = chans;
      if (type != -1) sf->header_type = type;
#if HAVE_GDBM
      if (gdbm_store(gdbm_fd(),gdbm_key((char *)arg),gdbm_contents(sf),GDBM_REPLACE) != 0)
	mus_error(MUS_CANT_OPEN_FILE,"gdbm_store: %s ",gdbm_strerror(gdbm_errno));
      free(sf);
#endif      
      return(MUS_NO_ERROR);
    }
  return(MUS_ERROR);
}

int mus_sound_max_amp(const char *ifile, MUS_SAMPLE_TYPE *vals)
{
  int ifd,ichans,bufnum,n,curframes,i,frames,chn;
  MUS_SAMPLE_TYPE fc;
  MUS_SAMPLE_TYPE *buffer,*time,*samp;
  MUS_SAMPLE_TYPE **ibufs;
  sound_file *sf; 
  sf = getsf(ifile); 
#if HAVE_GDBM
  if ((sf) && (sf->max_amps_ok))
#else
  if ((sf) && (sf->max_amps))
#endif
    {
      for (chn=0;chn<sf->chans;chn++)
	{
	  vals[chn*2] = sf->max_amps[chn*2];
	  vals[chn*2+1] = sf->max_amps[chn*2+1];
	}
#if HAVE_GDBM
      free(sf);
#endif
      return(sf->samples / sf->chans);
    }
  ifd = mus_sound_open_input(ifile);
  if (ifd == MUS_ERROR) return(MUS_ERROR);
  /* sf = getsf(ifile); */
  ichans = mus_sound_chans(ifile);
  frames = mus_sound_frames(ifile);
  if (frames <= 0) {mus_sound_close_input(ifd); return(0);}
  mus_sound_seek_frame(ifd,0);
  ibufs = (MUS_SAMPLE_TYPE **)CALLOC(ichans,sizeof(MUS_SAMPLE_TYPE *));
  bufnum = 8192;
  for (i=0;i<ichans;i++) ibufs[i] = (MUS_SAMPLE_TYPE *)CALLOC(bufnum,sizeof(MUS_SAMPLE_TYPE));
  time = (MUS_SAMPLE_TYPE *)CALLOC(ichans,sizeof(MUS_SAMPLE_TYPE));
  samp = (MUS_SAMPLE_TYPE *)CALLOC(ichans,sizeof(MUS_SAMPLE_TYPE));
  for (n=0;n<frames;n+=bufnum)
    {
      if ((n+bufnum)<frames) curframes = bufnum; else curframes = (frames-n);
      mus_sound_read(ifd,0,curframes-1,ichans,ibufs);
      for (chn=0;chn<ichans;chn++)
	{
	  buffer = (MUS_SAMPLE_TYPE *)(ibufs[chn]);
	  fc = samp[chn];
	  for (i=0;i<curframes;i++) 
	    {
	      if ((buffer[i]>fc) || (fc < -buffer[i])) 
		{
		  time[chn]=i+n; 
		  samp[chn]=buffer[i]; 
		  if (samp[chn]<0) samp[chn] = -samp[chn];
		  fc = samp[chn];
		}
	    }
	}
    }
  mus_sound_close_input(ifd);
#if (!HAVE_GDBM)
  if (sf->max_amps == NULL) sf->max_amps = (MUS_SAMPLE_TYPE *)CALLOC(ichans*2,sizeof(MUS_SAMPLE_TYPE));
#endif
  for (chn=0,i=0;chn<ichans;chn++,i+=2)
    {
      vals[i]=time[chn];
      vals[i+1]=samp[chn];
      sf->max_amps[i] = vals[i];
      sf->max_amps[i+1] = vals[i+1];
    }
#if HAVE_GDBM
  sf->max_amps_ok = 1;
  if (gdbm_store(gdbm_fd(),gdbm_key((char *)ifile),gdbm_contents(sf),GDBM_REPLACE) != 0)
    mus_error(MUS_CANT_OPEN_FILE,"gdbm_store: %s ",gdbm_strerror(gdbm_errno));
  free(sf);
#endif      
  FREE(time);
  FREE(samp);
  for (i=0;i<ichans;i++) FREE(ibufs[i]);
  FREE(ibufs);
  return(frames);
}


int mus_file_to_array(const char *filename, int chan, int start, int samples, MUS_SAMPLE_TYPE *array)
{
  int ifd,chans,total_read;
  MUS_SAMPLE_TYPE **bufs;
  ifd = mus_sound_open_input(filename);
  if (ifd == MUS_ERROR) return(MUS_ERROR);
  chans = mus_sound_chans(filename);
  if (chan >= chans) 
    {
      mus_sound_close_input(ifd);      
      mus_error(MUS_NO_SUCH_CHANNEL,"can't read %s channel %d to array (file has %d chans)\n [%s[%d] %s]",
		filename,chan,chans,
		__FILE__,__LINE__,__FUNCTION__);
      return(MUS_ERROR);
    }
  bufs = (MUS_SAMPLE_TYPE **)CALLOC(chans,sizeof(MUS_SAMPLE_TYPE *));
  bufs[chan] = array;
  set_sound_error(filename,__LINE__,__FUNCTION__);
  mus_sound_seek_frame(ifd,start);
  total_read = mus_file_read_any(ifd,0,chans,samples,bufs,(MUS_SAMPLE_TYPE *)bufs);
  mus_sound_close_input(ifd);
  unset_sound_error();
  FREE(bufs);
  return(total_read);
}

int mus_array_to_file(const char *filename, MUS_SAMPLE_TYPE *ddata, int len, int srate, int channels)
{
  /* put ddata into a sound file, taking byte order into account */
  /* assume ddata is interleaved already if more than one channel */
  int fd,err=MUS_NO_ERROR;
  MUS_SAMPLE_TYPE *bufs[1];
  mus_sound_forget(filename);
  fd = mus_file_create(filename);
  if (fd == -1) 
    {
      mus_error(MUS_CANT_OPEN_FILE,"can't create %s: %s\n  [%s[%d] %s]",
		filename,strerror(errno),
		__FILE__,__LINE__,__FUNCTION__);
      return(MUS_ERROR);
    }
  set_sound_error(filename,__LINE__,__FUNCTION__);
  err = mus_file_set_descriptors(fd,filename,
				 MUS_OUT_FORMAT,
				 mus_data_format_to_bytes_per_sample(MUS_OUT_FORMAT),
				 28,channels,MUS_NEXT);
  if (err != MUS_ERROR)
    {
      err = mus_header_write_next_header(fd,srate,channels,28,len*sizeof(MUS_SAMPLE_TYPE),MUS_OUT_FORMAT,NULL,0);
      if (err != MUS_ERROR)
	{
	  bufs[0] = ddata;
	  err = mus_file_write(fd,0,len-1,1,bufs);
	}
    }
  mus_file_close(fd);
  unset_sound_error();
  return(err);
}

#include "sndlib-strings.h"

#define NUM_SNDLIB_NAMES 112
static const char *sndlib_names[] = {
S_make_sound_data,S_mus_aifc,S_mus_aiff,S_mus_alaw,
S_mus_audio_adat_in,S_mus_audio_adat_out,S_mus_audio_aes_in,S_mus_audio_aes_out,
S_mus_audio_amp,S_mus_audio_aux_input,S_mus_audio_aux_output,
S_mus_audio_bass,S_mus_audio_cd,S_mus_audio_channel,S_mus_audio_close,
S_mus_audio_dac_filter,S_mus_audio_dac_out,S_mus_audio_default,
S_mus_audio_digital_in,S_mus_audio_digital_out,S_mus_audio_direction,S_mus_audio_duplex_default,
S_mus_audio_format,S_mus_audio_igain,S_mus_audio_imix,S_mus_audio_line,
S_mus_audio_line_in,S_mus_audio_line_out,S_mus_audio_line1,
S_mus_audio_line2,S_mus_audio_line3,
S_mus_audio_microphone,S_mus_audio_mixer,S_mus_audio_mixer_read,
S_mus_audio_mixer_write,S_mus_audio_ogain,S_mus_audio_open_input,
S_mus_audio_open_output,S_mus_audio_pcm,S_mus_audio_pcm2,
S_mus_audio_port,S_mus_audio_read,S_mus_audio_reclev,
S_mus_audio_report,S_mus_audio_restore,S_mus_audio_samples_per_channel,S_mus_audio_save,
S_mus_audio_spdif_in,S_mus_audio_spdif_out,S_mus_audio_speakers,
S_mus_audio_srate,S_mus_audio_sun_outputs,S_mus_audio_synth,
S_mus_audio_systems,S_mus_audio_treble,S_mus_audio_write,
S_mus_b24int,S_mus_bdouble,S_mus_bfloat,S_mus_bint,S_mus_bshort,
S_mus_byte,S_mus_data_format_bytes_per_sample,S_mus_data_format_name,
S_mus_file_prescaler,S_mus_file_set_data_clipped,
S_mus_file_set_prescaler,S_mus_header_type_name,S_mus_ircam,
S_mus_l24int,S_mus_ldouble,S_mus_lfloat,S_mus_lint,S_mus_lshort,
S_mus_mulaw,S_mus_next,S_mus_nist,S_mus_raw,S_mus_riff,
S_mus_sound_chans,S_mus_sound_close_input,S_mus_sound_close_output,
S_mus_sound_comment,S_mus_sound_data_format,S_mus_sound_data_location,
S_mus_sound_datum_size,S_mus_sound_duration,S_mus_sound_frames,
S_mus_sound_header_type,S_mus_sound_length,S_mus_sound_loop_info,
S_mus_sound_max_amp,S_mus_sound_open_input,S_mus_sound_open_output,
S_mus_sound_read,S_mus_sound_reopen_output,S_mus_sound_samples,
S_mus_sound_seek,S_mus_sound_seek_frame,S_mus_sound_srate,
S_mus_sound_type_specifier,S_mus_sound_write,S_mus_ubshort,
S_mus_ubyte,S_mus_ulshort,S_sound_data2vct,S_sound_data_chans,
S_sound_data_length,S_sound_data_ref,S_sound_data_setB,S_sound_data_p,
S_vct2sound_data
};

int sndlib_num_commands(void);
const char **sndlib_commands(void);

int sndlib_num_commands(void) {return(NUM_SNDLIB_NAMES);}
const char **sndlib_commands(void) {return(sndlib_names);}

