/* sound.c */

/* TODO: make this thread-safe by wrapping locks around the header/data base references */
/*       (if using gdbm, this would also need to handle the gdbm file pointer differently) */

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

static void gdbm_errmsg(char *msg) {mus_error(MUS_AUDIO_CANT_OPEN,msg);}
static GDBM_FILE gdbmf = NULL;
static GDBM_FILE gdbm_fd(void) 
{
  if (gdbmf == NULL) 
    gdbmf = gdbm_open(SNDLIB_GDBM_FILENAME,0,GDBM_WRCREAT,0666,gdbm_errmsg);
  return(gdbmf);
}

#endif

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
  int err = 0;
  if (!sndlib_initialized)
    {
      sndlib_initialized = 1;
      mus_error_buffer = (char *)CALLOC(256,sizeof(char));
      if (mus_error_buffer == NULL) return(-1);
      err = mus_header_initialize();
      if (err == 0) err = mus_audio_initialize();
      if (err == -1) {FREE(mus_error_buffer); return(-1);}
    }
  return(0);
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
		  return(0);
		}
	    }
	}
    }
  return(-1);
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
	      fprintf(stdout," %.3f at %.3f ",MUS_SAMPLE_TO_FLOAT(sf->max_amps[i+1]),(float)(sf->max_amps[i])/(float)(sf->srate));
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
  sf->data_location = mus_header_data_location();
  sf->samples = mus_header_samples();
  sf->data_format = mus_header_format();
  sf->srate = mus_header_srate();
  sf->chans = mus_header_chans();
  sf->datum_size = mus_header_data_format_to_bytes_per_sample();
  sf->header_type = mus_header_type();
  sf->original_sound_format = mus_header_original_format();
  sf->true_file_length = mus_header_true_length();
  sf->comment_start = mus_header_comment_start();
  sf->comment_end = mus_header_comment_end();
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
  i= gdbm_store(gdbm_fd(),gdbm_key((char *)name),gdbm_contents(sf),GDBM_REPLACE);
  if (i != 0) mus_error(MUS_AUDIO_CANT_OPEN,"gdbm_store: %d = %s ",i,gdbm_strerror(gdbm_errno));
#endif
}

static sound_file *read_sound_file_header_with_fd(int fd, const char *arg)
{
  int err=0;
  sound_file *sf = NULL;
  mus_sound_initialize();
  err = mus_header_read_with_fd(fd);
  if (err == -1) return(NULL);
  sf = add_to_sound_table(arg);
  fill_sf_record(arg,sf);
  return(sf);
}

static sound_file *read_sound_file_header_with_name(const char *name)
{
  sound_file *sf = NULL;
  mus_sound_initialize();
  if (mus_header_read(name) != -1)
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
    {
      sf = read_sound_file_header_with_name(arg);
      if (sf == NULL) mus_audio_set_error(MUS_AUDIO_CANT_OPEN);
    }
  return(sf);
}

#if HAVE_GDBM
  #define FSF(val) free(val)
#else
  #define FSF(val)
#endif
int mus_sound_samples (const char *arg) {int val=-1; sound_file *sf; sf = getsf(arg); if (sf) {val = (sf->samples); FSF(sf);} return(val);}
int mus_sound_frames (const char *arg) {int val=-1; sound_file *sf; sf = getsf(arg); if (sf) {val = (sf->samples / sf->chans); FSF(sf);} return(val);}
int mus_sound_datum_size (const char *arg) {int val=-1; sound_file *sf; sf = getsf(arg); if (sf) {val = (sf->datum_size); FSF(sf);} return(val);}
int mus_sound_data_location (const char *arg) {int val=-1; sound_file *sf; sf = getsf(arg); if (sf) {val = (sf->data_location); FSF(sf);} return(val);}
int mus_sound_chans (const char *arg) {int val=-1; sound_file *sf; sf = getsf(arg); if (sf) {val = (sf->chans); FSF(sf);} return(val);}
int mus_sound_srate (const char *arg) {int val=-1; sound_file *sf; sf = getsf(arg); if (sf) {val = (sf->srate); FSF(sf);} return(val);}
int mus_sound_header_type (const char *arg) {int val=-1; sound_file *sf; sf = getsf(arg); if (sf) {val = (sf->header_type); FSF(sf);} return(val);}
int mus_sound_data_format (const char *arg) {int val=-1; sound_file *sf; sf = getsf(arg); if (sf) {val = (sf->data_format); FSF(sf);} return(val);}
int mus_sound_original_format (const char *arg) {int val=-1; sound_file *sf; sf = getsf(arg); if (sf) {val = (sf->original_sound_format); FSF(sf);} return(val);}
int mus_sound_comment_start (const char *arg) {int val=-1; sound_file *sf; sf = getsf(arg); if (sf) {val = (sf->comment_start); FSF(sf);} return(val);}
int mus_sound_comment_end (const char *arg) {int val=-1; sound_file *sf; sf = getsf(arg); if (sf) {val = (sf->comment_end); FSF(sf);} return(val);}
int mus_sound_length (const char *arg) {int val=-1; sound_file *sf; sf = getsf(arg); if (sf) {val = (sf->true_file_length); FSF(sf);} return(val);}
int mus_sound_fact_samples (const char *arg) {int val=-1; sound_file *sf; sf = getsf(arg); if (sf) {val = (sf->fact_samples); FSF(sf);} return(val);}
int mus_sound_distributed (const char *arg) {int val=-1; sound_file *sf; sf = getsf(arg); if (sf) {val = (sf->header_distributed); FSF(sf);} return(val);}
int mus_sound_write_date (const char *arg) {int val=-1; sound_file *sf; sf = getsf(arg); if (sf) {val = (sf->write_date); FSF(sf);} return(val);}
int mus_sound_type_specifier (const char *arg) {int val=-1; sound_file *sf; sf = getsf(arg); if (sf) {val = (sf->type_specifier); FSF(sf);} return(val);}
int mus_sound_align (const char *arg) {int val=-1; sound_file *sf; sf = getsf(arg); if (sf) {val = (sf->block_align); FSF(sf);} return(val);}
int mus_sound_bits_per_sample (const char *arg) {int val=-1; sound_file *sf; sf = getsf(arg); if (sf) {val = (sf->bits_per_sample); FSF(sf);} return(val);}
float mus_sound_duration(const char *arg) {return((float)mus_sound_frames(arg) / (float)mus_sound_srate(arg));}

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
	mus_error(MUS_AUDIO_CANT_OPEN,"gdbm_store: %s ",gdbm_strerror(gdbm_errno));
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
  int start,end,fd,len;
  char *sc = NULL;
  start = mus_sound_comment_start(name);
  end = mus_sound_comment_end(name);
  if (end == 0) return(NULL);
  len = end-start+1;
  if (len>0)
    {
      /* open and get the comment */
      sc = (char *)CALLOC(len+1,sizeof(char)); /* len+1 calloc'd => we'll always have a trailing null */
#if MACOS
      fd = open(name,O_RDONLY);
#else
  #ifdef WINDOZE
      fd = open(name,O_RDONLY | O_BINARY);
  #else
      fd = open(name,O_RDONLY,0);
  #endif
#endif
      lseek(fd,start,SEEK_SET);
      read(fd,sc,len);
      close(fd);
      return(sc);
    }
  else return(NULL);
}

int mus_sound_open_input (const char *arg) 
{
  int fd;
  sound_file *sf = NULL;
  mus_audio_set_error(MUS_AUDIO_NO_ERROR);
  mus_sound_initialize();
  fd = mus_file_open_read(arg);
  if (fd != -1)
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
  else mus_audio_set_error(MUS_AUDIO_CANT_OPEN); 
  return(fd);
}

int mus_sound_open_output (const char *arg, int srate, int chans, int data_format, int header_type, const char *comment)
{
  int fd = 0,err,comlen = 0;
  mus_sound_forget(arg);
  if (comment) comlen = strlen(comment);
  mus_audio_set_error(MUS_AUDIO_NO_ERROR);
  mus_sound_initialize();
  err = mus_header_write(arg,header_type,srate,chans,0,0,data_format,comment,comlen);
  if (err != -1)
    {
      fd = mus_file_open_write(arg);
      mus_file_set_descriptors(fd,arg,data_format,mus_data_format_to_bytes_per_sample(data_format),mus_header_data_location(),chans,header_type);
    }
  else mus_audio_set_error(MUS_AUDIO_CANT_OPEN); 
  return(fd);
}

int mus_sound_reopen_output(const char *arg, int chans, int format, int type, int data_loc)
{
  int fd;
  mus_audio_set_error(MUS_AUDIO_NO_ERROR);
  mus_sound_initialize();
  fd = mus_file_reopen_write(arg);
  mus_file_set_descriptors(fd,arg,format,mus_data_format_to_bytes_per_sample(format),data_loc,chans,type);
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
	mus_error(MUS_AUDIO_CANT_OPEN,"gdbm_store: %s ",gdbm_strerror(gdbm_errno));
      free(sf);
#endif      
      return(0);
    }
  else return(-1);
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
  if (ifd == -1) return(-1);
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
    mus_error(MUS_AUDIO_CANT_OPEN,"gdbm_store: %s ",gdbm_strerror(gdbm_errno));
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
  if (ifd == -1) return(-1);
  chans = mus_sound_chans(filename);
  if (chan >= chans) return(-1);
  bufs = (MUS_SAMPLE_TYPE **)CALLOC(chans,sizeof(MUS_SAMPLE_TYPE *));
  bufs[chan] = array;
  mus_sound_seek_frame(ifd,start);
  total_read = mus_file_read_any(ifd,0,chans,samples,bufs,(MUS_SAMPLE_TYPE *)bufs);
  mus_sound_close_input(ifd);
  FREE(bufs);
  return(total_read);
}

int mus_array_to_file(const char *filename, MUS_SAMPLE_TYPE *ddata, int len, int srate, int channels)
{
  /* put ddata into a sound file, taking byte order into account */
  /* assume ddata is interleaved already if more than one channel */
  int fd;
  MUS_SAMPLE_TYPE *bufs[1];
  mus_sound_forget(filename);
  fd = mus_file_create(filename);
  if (fd == -1) return(-1);
  mus_file_open_descriptors(fd,MUS_OUT_FORMAT,mus_data_format_to_bytes_per_sample(MUS_OUT_FORMAT),28);
  mus_header_write_next_header(fd,srate,channels,28,len*sizeof(MUS_SAMPLE_TYPE),MUS_OUT_FORMAT,NULL,0);
  bufs[0] = ddata;
  mus_file_write(fd,0,len-1,1,bufs);
  mus_file_close(fd);
  return(0);
}

#include "sndlib-strings.h"

#define NUM_SNDLIB_NAMES 114
static const char *sndlib_names[] = {
S_make_sound_data,S_mus_aifc,S_mus_aiff,S_mus_alaw,
S_mus_audio_adat_in,S_mus_audio_adat_out,S_mus_audio_aes_in,S_mus_audio_aes_out,
S_mus_audio_amp,S_mus_audio_aux_input,S_mus_audio_aux_output,
S_mus_audio_bass,S_mus_audio_cd,S_mus_audio_channel,S_mus_audio_close,
S_mus_audio_dac_filter,S_mus_audio_dac_out,S_mus_audio_default,
S_mus_audio_digital_in,S_mus_audio_digital_out,S_mus_audio_direction,S_mus_audio_duplex_default,
S_mus_audio_error,S_mus_audio_error_name,S_mus_audio_format,
S_mus_audio_igain,S_mus_audio_imix,S_mus_audio_line,
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

