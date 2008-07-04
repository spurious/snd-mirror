#include <mus-config.h>

#if USE_SND
  #include "snd.h"
#endif

#include <math.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <stddef.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <stdarg.h>

#if (defined(HAVE_LIBC_H) && (!defined(HAVE_UNISTD_H)))
  #include <libc.h>
#else
  #if (!(defined(_MSC_VER)))
    #include <unistd.h>
  #endif
  #if HAVE_STRING_H
    #include <string.h>
  #endif
#endif
#if HAVE_PTHREAD_H
  #include <pthread.h>
#endif

#include "_sndlib.h"
#include "sndlib-strings.h"



/* ---------------------------------------- pthread debugging ---------------------------------------- */
#if HAVE_PTHREADS && MUS_DEBUGGING

typedef struct {
  const char *file;
  int line;
  const char *func;
  mus_lock_t *lock;
} lock_info;

typedef struct {
  lock_info *lock_list;
  int top_lock;
} thread_locks;

static pthread_key_t mus_thread_locks;


typedef struct {
  mus_lock_t *lock;
  const char *name;
} lname;

static lname **lnames = NULL;
static int lnames_size = 0;

static const char *lock_name(mus_lock_t *lock)
{
  int i;
  for (i = 0; i < lnames_size; i++)
    if (lock == lnames[i]->lock)
      return(lnames[i]->name);
  return("unknown");
}


void mus_lock_set_name(mus_lock_t *lock, const char *name)
{
  int i, pos = -1;
  if (lnames_size == 0)
    {
      lnames_size = 32;
      lnames = (lname **)calloc(lnames_size, sizeof(lname *));
      pos = 0;
    }
  else
    {
      for (i = 0; i < lnames_size; i++)
	if (lnames[i] == NULL)
	  {
	    pos = i;
	    break;
	  }
    }
  if (pos < 0)
    {
      pos = lnames_size;
      lnames_size += 32;
      lnames = (lname **)realloc(lnames, lnames_size * sizeof(lname *));
      for (i = pos; i < lnames_size; i++) lnames[i] = NULL;
    }
  lnames[pos] = (lname *)calloc(1, sizeof(lname));
  lnames[pos]->lock = lock;
  lnames[pos]->name = name;
}


void mus_lock_unset_name(mus_lock_t *lock)
{
  int i;
  for (i = 0; i < lnames_size; i++)
    if ((lnames[i]) &&
	(lock == lnames[i]->lock))
      {
	free(lnames[i]);
	lnames[i] = NULL;
	return;
      }
}


static char lock_buffer[128];

static char *describe_lock(mus_lock_t *lock)
{
  mus_snprintf(lock_buffer, 128, "%p: %s", lock, lock_name(lock));
  return(lock_buffer);
}


static void describe_locks(thread_locks *locks)
{
  int i;
  if (locks->top_lock < 0)
    fprintf(stderr, "no current locks\n");
  else
    {
      fprintf(stderr, "\nCurrent locks:\n");
      for (i = 0; i <= locks->top_lock; i++)
	fprintf(stderr, "    %s: %s[%d] %s\n", 
		describe_lock(locks->lock_list[i].lock),
		locks->lock_list[i].file,
		locks->lock_list[i].line,
		locks->lock_list[i].func);
      fprintf(stderr, "\n");
    }
}

int mus_lock(mus_lock_t *lock, const char *file, int line, const char *func)
{
  int result;
  thread_locks *locks;

  locks = (thread_locks *)pthread_getspecific(mus_thread_locks);
  if (locks)
    {
      int i;
      for (i = 0; i < locks->top_lock; i++)
	if (lock == locks->lock_list[i].lock)
	  {
	    fprintf(stderr, "about to try to get a lock we already own: %s[%d]:%s, lock: %s\n", file, line, func, describe_lock(lock));
	    describe_locks(locks);
	  }
    }

  if (!locks)
    {
      locks = (thread_locks *)malloc(sizeof(thread_locks));
      locks->lock_list = (lock_info *)malloc(8 * sizeof(lock_info));
      locks->top_lock = -1;
      pthread_setspecific(mus_thread_locks, (void *)locks);
    }

  result = pthread_mutex_lock(lock);
  
  locks->top_lock++;
  if (locks->top_lock >= 8)
    {
      fprintf(stderr, "got too many locks!!");
      describe_locks(locks);
      locks->top_lock = 7;
    }
  
  locks->lock_list[locks->top_lock].file = file;
  locks->lock_list[locks->top_lock].line = line;
  locks->lock_list[locks->top_lock].func = func;
  locks->lock_list[locks->top_lock].lock = lock;

  return(result);
}


int mus_unlock(mus_lock_t *lock, const char *file, int line, const char *func)
{
  int result;
  thread_locks *locks;

  result = pthread_mutex_unlock(lock);

  locks = (thread_locks *)pthread_getspecific(mus_thread_locks);
  if (!locks)
    {
      fprintf(stderr, "just released lock %s that we didn't own? %s[%d]:%s\n", describe_lock(lock), file, line, func);
      describe_locks(locks);
    }
  else
    {
      if (locks->top_lock < 0)
	{
	  fprintf(stderr, "redundant unlock of %s? %s[%d]:%s\n", describe_lock(lock), file, line, func);
	}
      else
	{
	  if (lock != locks->lock_list[locks->top_lock].lock)
	    {
	      fprintf(stderr, "just released lock %s out of order? %s[%d]:%s\n", describe_lock(lock), file, line, func);
	      describe_locks(locks);
	    }
	  locks->top_lock--;
	}
    }
  return(result);
}


static void free_locks(void *ulocks)
{
  thread_locks *locks = (thread_locks *)ulocks;
  if (locks->lock_list)
    free(locks->lock_list);
  free(locks);
}
#endif

/* -------------------------------------------------------------------------------- */


/* mus_error handling is tricky enough without threads!  We have to keep the previous handlers
 *   intact within each thread, then in mus_error itself, ask the thread what its current
 *   error handler is.
 */

#if HAVE_PTHREADS
static pthread_key_t mus_thread_error_handler;
static pthread_key_t mus_thread_previous_error_handler;
#else
static mus_error_handler_t *mus_error_handler = NULL;
#endif

mus_error_handler_t *mus_error_set_handler(mus_error_handler_t *new_error_handler)
{
  mus_error_handler_t *old_handler;
#if HAVE_PTHREADS
  old_handler = (mus_error_handler_t *)pthread_getspecific(mus_thread_error_handler);
  pthread_setspecific(mus_thread_error_handler, (void *)new_error_handler);
  pthread_setspecific(mus_thread_previous_error_handler, (void *)old_handler);
#else
  old_handler = mus_error_handler;
  mus_error_handler = new_error_handler;
#endif
  return(old_handler);
}


#if HAVE_PTHREADS

void mus_thread_restore_error_handler(void)
{
  pthread_setspecific(mus_thread_error_handler, (void *)mus_error_set_handler((mus_error_handler_t *)pthread_getspecific(mus_thread_previous_error_handler)));
}


mus_error_handler_t *mus_thread_get_previous_error_handler(void)
{
  return((mus_error_handler_t *)pthread_getspecific(mus_thread_previous_error_handler));
}
#endif

static char *mus_error_buffer = NULL;
static int mus_error_buffer_size = 1024;

#if HAVE_PTHREADS
  static mus_lock_t sound_error_lock = MUS_LOCK_INITIALIZER;
#endif

int mus_error(int error, const char *format, ...)
{
  int bytes_needed = 0;
  va_list ap;

  if (format == NULL) 
    return(MUS_ERROR); /* else bus error in Mac OSX */

  MUS_LOCK(&sound_error_lock);

  if (mus_error_buffer == NULL)
    mus_error_buffer = (char *)CALLOC(mus_error_buffer_size, sizeof(char));

  va_start(ap, format);
#if HAVE_VSNPRINTF
  bytes_needed = vsnprintf(mus_error_buffer, mus_error_buffer_size, format, ap);
#else
  bytes_needed = vsprintf(mus_error_buffer, format, ap);
#endif
  va_end(ap);

  if (bytes_needed > mus_error_buffer_size)
    {
      mus_error_buffer_size = bytes_needed * 2;
      FREE(mus_error_buffer);
      mus_error_buffer = (char *)CALLOC(mus_error_buffer_size, sizeof(char));
      va_start(ap, format);
#if HAVE_VSNPRINTF
      vsnprintf(mus_error_buffer, mus_error_buffer_size, format, ap);
#else
      vsprintf(mus_error_buffer, format, ap);
#endif
      va_end(ap);
    }

#if HAVE_PTHREADS
  MUS_UNLOCK(&sound_error_lock);
  {
    mus_error_handler_t *mus_error_handler;
    mus_error_handler = (mus_error_handler_t *)pthread_getspecific(mus_thread_error_handler);
#endif

  if (mus_error_handler)
    (*mus_error_handler)(error, mus_error_buffer);
  else 
    {
      fprintf(stderr, mus_error_buffer);
      fputc('\n', stderr);
    }

#if HAVE_PTHREADS
  }
#endif

  return(MUS_ERROR);
}


static mus_print_handler_t *mus_print_handler = NULL;

mus_print_handler_t *mus_print_set_handler(mus_print_handler_t *new_print_handler) 
{
  mus_print_handler_t *old_handler;
  old_handler = mus_print_handler;
  mus_print_handler = new_print_handler;
  return(old_handler);
}


#if HAVE_PTHREADS
  static mus_lock_t sound_print_lock = MUS_LOCK_INITIALIZER;
#endif


void mus_print(const char *format, ...)
{
  va_list ap;

  if (mus_print_handler)
    {
      int bytes_needed = 0;

      MUS_LOCK(&sound_print_lock);

      if (mus_error_buffer == NULL)
	mus_error_buffer = (char *)CALLOC(mus_error_buffer_size, sizeof(char));

      va_start(ap, format);
#if HAVE_VSNPRINTF
      bytes_needed = vsnprintf(mus_error_buffer, mus_error_buffer_size, format, ap);
#else
      bytes_needed = vsprintf(mus_error_buffer, format, ap);
#endif
      va_end(ap);
      if (bytes_needed > mus_error_buffer_size)
	{
	  mus_error_buffer_size = bytes_needed * 2;
	  FREE(mus_error_buffer);
	  mus_error_buffer = (char *)CALLOC(mus_error_buffer_size, sizeof(char));
	  va_start(ap, format);
#if HAVE_VSNPRINTF
	  vsnprintf(mus_error_buffer, mus_error_buffer_size, format, ap);
#else
	  vsprintf(mus_error_buffer, format, ap);
#endif
	  va_end(ap);
	}

      MUS_UNLOCK(&sound_print_lock);

      (*mus_print_handler)(mus_error_buffer);
    }
  else
    {
      va_start(ap, format);
      vfprintf(stdout, format, ap);
      va_end(ap);
    }
}


static const char *mus_initial_error_names[MUS_INITIAL_ERROR_TAG] = {
  "no error", "no frequency method", "no phase method", "null gen arg to method", "no length method",
  "no free method", "no describe method", "no data method", "no scaler method",
  "memory allocation failed", "unstable two pole error",
  "can't open file", "no sample input", "no sample output",
  "no such channel", "no file name provided", "no location method", "no channel method",
  "no such fft window", "unsupported data format", "header read failed",
  "unsupported header type", "file descriptors not initialized", "not a sound file", "file closed", "write error",
  "header write failed", "can't open temp file", "interrupted", "bad envelope",

  "audio channels not available", "audio srate not available", "audio format not available",
  "no audio input available", "audio configuration not available", 
  "no audio lines available", "audio write error", "audio size not available", "audio device not available",
  "can't close audio", "can't open audio", "audio read error", "audio amp not available",
  "can't write audio", "can't read audio", "no audio read permission", 
  "can't close file", "arg out of range",

  "midi open error", "midi read error", "midi write error", "midi close error", "midi init error", "midi misc error",

  "no channels method", "no hop method", "no width method", "no file-name method", "no ramp method", "no run method",
  "no increment method", "no offset method",
  "no xcoeff method", "no ycoeff method", "no xcoeffs method", "no ycoeffs method", "no reset", "bad size", "can't convert",
  "read error", "no safety method"
};

static char **mus_error_names = NULL;
static int mus_error_names_size = 0;
static int mus_error_tag = MUS_INITIAL_ERROR_TAG;

int mus_make_error(const char *error_name) 
{
  int new_error, err;
  new_error = mus_error_tag++;
  err = new_error - MUS_INITIAL_ERROR_TAG;
  if (error_name)
    {
      int len, i;
      if (err >= mus_error_names_size)
	{
	  if (mus_error_names_size == 0)
	    {
	      mus_error_names_size = 8;
	      mus_error_names = (char **)CALLOC(mus_error_names_size, sizeof(char *));
	    }
	  else
	    {
	      len = mus_error_names_size;
	      mus_error_names_size += 8;
	      mus_error_names = (char **)REALLOC(mus_error_names, mus_error_names_size * sizeof(char *));
	      for (i = len; i < mus_error_names_size; i++) mus_error_names[i] = NULL;
	    }
	}
      len = strlen(error_name);
      mus_error_names[err] = (char *)CALLOC(len + 1, sizeof(char));
      MUS_SET_PRINTABLE(PRINT_CHAR);
      strcpy(mus_error_names[err], error_name);
    }
  return(new_error);
}


const char *mus_error_type_to_string(int err)
{
  if (err >= 0)
    {
      if (err < MUS_INITIAL_ERROR_TAG)
	return(mus_initial_error_names[err]);
      else
	{
	  err -= MUS_INITIAL_ERROR_TAG;
	  if ((mus_error_names) && (err < mus_error_names_size))
	    return(mus_error_names[err]);
	}
    }
  return("unknown mus error");
}


static void default_mus_error(int ignore, char *msg)
{
  /* default error handler simply prints the error message */
  fprintf(stderr, msg);
}


static time_t local_file_write_date(const char *filename)
{
  struct stat statbuf;
  int err;
  err = stat(filename, &statbuf);
  if (err < 0) return((time_t)0);
  return((time_t)(statbuf.st_mtime));
}


int mus_sample_bits(void)
{
  /* this to check for inconsistent loads */
#if SNDLIB_USE_FLOATS
  return(sizeof(Float));
#else
  return(MUS_SAMPLE_BITS);
#endif
}


/* -------- sound file table -------- */

typedef struct {
  char *file_name;  /* full path -- everything is keyed to this name */
  int table_pos;
  off_t *aux_comment_start, *aux_comment_end;
  int *loop_modes, *loop_starts, *loop_ends;
  int markers, base_detune, base_note;
  int *marker_ids, *marker_positions;
  off_t samples, true_file_length;
  off_t data_location;
  int srate, chans, header_type, data_format, original_sound_format, datum_size; 
  off_t comment_start, comment_end;
  int type_specifier, bits_per_sample, block_align, fact_samples;
  time_t write_date;
  mus_sample_t *maxamps;
  off_t *maxtimes;
} sound_file;

static int sound_table_size = 0;
static sound_file **sound_table = NULL;
#if HAVE_PTHREADS
  static mus_lock_t sound_table_lock = MUS_LOCK_INITIALIZER;
#endif


static void free_sound_file(sound_file *sf)
{
  if (sf)
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
      if (sf->maxamps) FREE(sf->maxamps);
      if (sf->maxtimes) FREE(sf->maxtimes);
      FREE(sf);
    }
}

static sound_file *add_to_sound_table(const char *name)
{
  int i, pos = -1;

  /* this is already within sound_table_lock */

  for (i = 0; i < sound_table_size; i++)
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
	sound_table = (sound_file **)CALLOC(sound_table_size, sizeof(sound_file *));
      else 
	{
	  sound_table = (sound_file **)REALLOC(sound_table, sound_table_size * sizeof(sound_file *));
	  for (i = pos; i < sound_table_size; i++) sound_table[i] = NULL;
	}
    }

  sound_table[pos] = (sound_file *)CALLOC(1, sizeof(sound_file));
  sound_table[pos]->table_pos = pos;
  sound_table[pos]->file_name = (char *)CALLOC(strlen(name) + 1, sizeof(char));
  MUS_SET_PRINTABLE(PRINT_CHAR);
  strcpy(sound_table[pos]->file_name, name);

  return(sound_table[pos]);
}


int mus_sound_prune(void)
{
  int i, pruned = 0;

  MUS_LOCK(&sound_table_lock);

  for (i = 0; i < sound_table_size; i++)
    if ((sound_table[i]) && 
	(!(mus_file_probe(sound_table[i]->file_name))))
      {
	free_sound_file(sound_table[i]);
	sound_table[i] = NULL;
	pruned++;
      }

  MUS_UNLOCK(&sound_table_lock);

  return(pruned);
}


int mus_sound_forget(const char *name)
{
  int i, len;
  bool free_name = false;
  char *short_name = NULL;
  if (name == NULL) return(MUS_ERROR);
  if (name[0] == '/')
    {
      len = strlen(name);
      for (i = 0; i < len; i++)
	if (name[i] == '/')
	  short_name = (char *)(name + i + 1);
    }
  else
    {
      short_name = mus_expand_filename(name);
      free_name = true;
    }

  if (name)
    {
      MUS_LOCK(&sound_table_lock);

      for (i = 0; i < sound_table_size; i++)
	if ((sound_table[i]) &&
	    ((strcmp(name, sound_table[i]->file_name) == 0) ||
	     ((short_name) && 
	      (strcmp(short_name, sound_table[i]->file_name) == 0))))
	  {
	    free_sound_file(sound_table[i]);
	    sound_table[i] = NULL;
	  }

      MUS_UNLOCK(&sound_table_lock);
    }
  
  if (free_name) FREE(short_name);
  return(MUS_NO_ERROR);
}


static sound_file *check_write_date(const char *name, sound_file *sf)
{
  if (sf)
    {
      time_t date;
      date = local_file_write_date(name);

      if (date == sf->write_date)
	return(sf);
      else 
	{
	  if ((sf->header_type == MUS_RAW) && (mus_header_no_header(name)))
	    {
	      int chan;
	      off_t data_size;
	      /* sound has changed since we last read it, but it has no header, so
	       * the only sensible thing to check is the new length (i.e. caller
	       * has set other fields by hand)
	       */
	      sf->write_date = date;
	      chan = mus_file_open_read(name);
	      data_size = lseek(chan, 0L, SEEK_END);
	      sf->true_file_length = data_size;
	      sf->samples = mus_bytes_to_samples(sf->data_format, data_size);
	      CLOSE(chan, name);  
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
  int i;

  if (!name) return(NULL);

  for (i = 0; i < sound_table_size; i++)
    if ((sound_table[i]) &&
	(strcmp(name, sound_table[i]->file_name) == 0))
      {
	check_write_date(name, sound_table[i]);
	return(sound_table[i]);
      }

  return(NULL);
}


static void display_sound_file_entry(FILE *fp, const char *name, sound_file *sf)
{
  int i, lim;
  time_t date;
  char timestr[64];
  char *comment;

  date = sf->write_date;
  if (date != 0)
    {
#if HAVE_STRFTIME
      strftime(timestr, 64, "%a %d-%b-%Y %H:%M:%S", localtime(&date));
#else
      sprintf(timestr, "%d", (int)date);
#endif
    }
  else sprintf(timestr, "(date cleared)");

  fprintf(fp, "  %s: %s, chans: %d, srate: %d, type: %s, format: %s, samps: " OFF_TD,
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
	fprintf(fp, ", loop mode %d: %d to %d", sf->loop_modes[0], sf->loop_starts[0], sf->loop_ends[0]);
      if (sf->loop_modes[1] != 0)
	fprintf(fp, ", loop mode %d: %d to %d, ", sf->loop_modes[1], sf->loop_starts[1], sf->loop_ends[1]);
      fprintf(fp, ", base: %d, detune: %d", sf->base_note, sf->base_detune);
    }

  if (sf->maxamps)
    {
      lim = sf->chans;
      if (lim > 0)
	{
	  if (lim > 64) 
	    lim = 64;
	  for (i = 0; i < lim; i++)
	    {
	      if (i > 1) fprintf(fp, ", ");
	      fprintf(fp, " %.3f at %.3f ",
		      MUS_SAMPLE_TO_FLOAT(sf->maxamps[i]),
		      (sf->srate > 0) ? (float)((double)(sf->maxtimes[i]) / (double)(sf->srate)) : (float)(sf->maxtimes[i]));
	    }
	}
    }

  if (mus_file_probe(name))
    {
      comment = mus_sound_comment(name);
      if (comment)
	{
	  fprintf(fp, "\n      comment: %s", comment);
	  FREE(comment);
	}
    }
  else fprintf(fp, " [defunct]");
  fprintf(fp, "\n");
}


void mus_sound_report_cache(FILE *fp)
{
  sound_file *sf;
  int entries = 0;
  int i;
  fprintf(fp, "sound table:\n");
  for (i = 0; i < sound_table_size; i++)
    {
      sf = sound_table[i];
      if (sf) 
	{
	  display_sound_file_entry(fp, sf->file_name, sf);
	  entries++;
	}
    }
  fprintf(fp, "\nentries: %d\n", entries); 
  fflush(fp);
}


static sound_file *fill_sf_record(const char *name, sound_file *sf)
{
  int i;

  /* already locked */

  sf->data_location = mus_header_data_location();
  sf->samples = mus_header_samples();
  sf->data_format = mus_header_format();
  sf->srate = mus_header_srate();
  /* if (sf->srate < 0) sf->srate = 0; */
  sf->chans = mus_header_chans();
  /* if (sf->chans < 0) sf->chans = 0; */
  sf->datum_size = mus_bytes_per_sample(sf->data_format);
  sf->header_type = mus_header_type();
  sf->original_sound_format = mus_header_original_format();
  sf->true_file_length = mus_header_true_length();
  sf->comment_start = mus_header_comment_start();
  sf->comment_end = mus_header_comment_end();
  if (((sf->header_type == MUS_AIFC) || 
       (sf->header_type == MUS_AIFF) || 
       (sf->header_type == MUS_RF64) || 
       (sf->header_type == MUS_RIFF)) &&
      (mus_header_aux_comment_start(0) != 0))

    {
      sf->aux_comment_start = (off_t *)CALLOC(4, sizeof(off_t));
      sf->aux_comment_end = (off_t *)CALLOC(4, sizeof(off_t));
      for (i = 0; i < 4; i++)
	{
	  sf->aux_comment_start[i] = mus_header_aux_comment_start(i);
	  sf->aux_comment_end[i] = mus_header_aux_comment_end(i);
	}
    }
  sf->type_specifier = mus_header_type_specifier();
  sf->bits_per_sample = mus_header_bits_per_sample();
  sf->fact_samples = mus_header_fact_samples();
  sf->block_align = mus_header_block_align();
  sf->write_date = local_file_write_date(name);
  if ((sf->header_type == MUS_AIFF) || (sf->header_type == MUS_AIFC))
    {
      int *marker_ids, *marker_positions;
      sf->markers = mus_header_mark_info(&marker_ids, &marker_positions);
      if (sf->markers > 0)
	{
	  sf->marker_ids = (int *)MALLOC(sf->markers * sizeof(int));
	  sf->marker_positions = (int *)MALLOC(sf->markers * sizeof(int));
	  memcpy((void *)(sf->marker_ids), (void *)marker_ids, sizeof(int) * sf->markers);
	  memcpy((void *)(sf->marker_positions), (void *)marker_positions, sizeof(int) * sf->markers);
	}
    }
  if (mus_header_loop_mode(0) > 0)
    {
      sf->loop_modes = (int *)CALLOC(2, sizeof(int));
      sf->loop_starts = (int *)CALLOC(2, sizeof(int));
      sf->loop_ends = (int *)CALLOC(2, sizeof(int));
      for (i = 0; i < 2; i++)
	{
	  sf->loop_modes[i] = mus_header_loop_mode(i);
	  if ((sf->header_type == MUS_AIFF) || 
	      (sf->header_type == MUS_AIFC))
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

  return(sf);
}


#if HAVE_PTHREADS
static mus_error_handler_t *old_header_read_error_handler; /* this should be safe -- only one thread can hold sound_table_lock */
static mus_error_handler_t *old_previous_header_read_error_handler; 

static void sound_table_lock_error_handler(int type, char *msg)
{
  /* hit error during header read, so reset current error handler to the one we started with, unlock sound_table_lock, pass error to original handler */

  /* fprintf(stderr, "hit header error: %d (%s) %s\n", type, mus_error_type_to_string(type), msg); */

  pthread_setspecific(mus_thread_error_handler, (void *)old_header_read_error_handler);
  pthread_setspecific(mus_thread_previous_error_handler, (void *)old_previous_header_read_error_handler);
  MUS_UNLOCK(&sound_table_lock);
  mus_error(type, msg);
}
#endif


static sound_file *read_sound_file_header(const char *name) /* 2 calls on this: mus_sound_open_input and get_sf */
{
  int result;
  mus_sound_initialize();

  /* if threads, sound_table_lock is set at this point, and the header read can throw an error, so
   *    we need to unlock while spinning back up the error handler stack.  But there are only two
   *    levels of error handler saved in the thread-specific data, so we need to make sure to
   *    save and restore the current handler by hand, while not messing with the previous handler.
   */
  
#if HAVE_PTHREADS
  /* save current error handler, reset to sound_table_unlock case... (save both just in case) */
  old_header_read_error_handler = (mus_error_handler_t *)pthread_getspecific(mus_thread_error_handler);
  old_previous_header_read_error_handler = (mus_error_handler_t *)pthread_getspecific(mus_thread_previous_error_handler);
  pthread_setspecific(mus_thread_error_handler, (void *)sound_table_lock_error_handler);
#endif

  result = mus_header_read(name);

#if HAVE_PTHREADS
  /* no error, reset current error handler to the one we started with */
  pthread_setspecific(mus_thread_error_handler, (void *)old_header_read_error_handler);
  pthread_setspecific(mus_thread_previous_error_handler, (void *)old_previous_header_read_error_handler);
#endif

  /* this portion won't trigger mus_error */
  if (result != MUS_ERROR)
    return(fill_sf_record(name, add_to_sound_table(name))); /* only call on fill_sf_record and add_to_sound_table */
  return(NULL);
}


static sound_file *get_sf(const char *arg) 
{
  sound_file *sf = NULL;
  if (arg == NULL) return(NULL);
  sf = find_sound_file(arg);
  if (sf) return(sf);
  return(read_sound_file_header(arg));
}


off_t mus_sound_samples(const char *arg)       
{
  sound_file *sf;
  off_t result = MUS_ERROR;
  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg);
  if (sf) result = sf->samples;
  MUS_UNLOCK(&sound_table_lock);
  return(result);
}

off_t mus_sound_frames(const char *arg)        
{
  sound_file *sf;
  off_t result = MUS_ERROR;
  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg);
  if (sf) result = (sf->chans > 0) ? (sf->samples / sf->chans) : 0;
  MUS_UNLOCK(&sound_table_lock);
  return(result);
}

int mus_sound_datum_size(const char *arg)      
{
  sound_file *sf;
  int result = MUS_ERROR;
  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg);
  if (sf) result = sf->datum_size;
  MUS_UNLOCK(&sound_table_lock);
  return(result);
}

off_t mus_sound_data_location(const char *arg) 
{
  sound_file *sf;
  off_t result = MUS_ERROR;
  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg);
  if (sf) result = sf->data_location;
  MUS_UNLOCK(&sound_table_lock);
  return(result);
}

int mus_sound_chans(const char *arg)           
{
  sound_file *sf;
  int result = MUS_ERROR;
  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg);
  if (sf) result = sf->chans;
  MUS_UNLOCK(&sound_table_lock);
  return(result);
}

int mus_sound_srate(const char *arg)           
{
  sound_file *sf;
  int result = MUS_ERROR;
  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg);
  if (sf) result = sf->srate;
  MUS_UNLOCK(&sound_table_lock);
  return(result);
}

int mus_sound_header_type(const char *arg)     
{
  sound_file *sf;
  int result = MUS_ERROR;
  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg);
  if (sf) result = sf->header_type;
  MUS_UNLOCK(&sound_table_lock);
  return(result);
}

int mus_sound_data_format(const char *arg)     
{
  sound_file *sf;
  int result = MUS_ERROR;
  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg);
  if (sf) result = sf->data_format;
  MUS_UNLOCK(&sound_table_lock);
  return(result);
}

int mus_sound_original_format(const char *arg) 
{
  sound_file *sf;
  int result = MUS_ERROR;
  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg);
  if (sf) result = sf->original_sound_format;
  MUS_UNLOCK(&sound_table_lock);
  return(result);
}

off_t mus_sound_comment_start(const char *arg) 
{
  sound_file *sf;
  off_t result = MUS_ERROR;
  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg);
  if (sf) result = sf->comment_start;
  MUS_UNLOCK(&sound_table_lock);
  return(result);
}

off_t mus_sound_comment_end(const char *arg)   
{
  sound_file *sf;
  off_t result = MUS_ERROR;
  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg);
  if (sf) result = sf->comment_end;
  MUS_UNLOCK(&sound_table_lock);
  return(result);
}

off_t mus_sound_length(const char *arg)        
{
  sound_file *sf;
  off_t result = MUS_ERROR;
  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg);
  if (sf) result = sf->true_file_length;
  MUS_UNLOCK(&sound_table_lock);
  return(result);
}

int mus_sound_fact_samples(const char *arg)    
{
  sound_file *sf;
  int result = MUS_ERROR;
  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg);
  if (sf) result = sf->fact_samples;
  MUS_UNLOCK(&sound_table_lock);
  return(result);
}

time_t mus_sound_write_date(const char *arg)   
{
  sound_file *sf;
  time_t result = (time_t)(MUS_ERROR);
  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg);
  if (sf) result = sf->write_date;
  MUS_UNLOCK(&sound_table_lock);
  return(result);
}

int mus_sound_type_specifier(const char *arg)  
{
  sound_file *sf;
  int result = MUS_ERROR;
  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg);
  if (sf) result = sf->type_specifier;
  MUS_UNLOCK(&sound_table_lock);
  return(result);
}

int mus_sound_block_align(const char *arg)     
{
  sound_file *sf;
  int result = MUS_ERROR;
  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg);
  if (sf) result = sf->block_align;
  MUS_UNLOCK(&sound_table_lock);
  return(result);
}

int mus_sound_bits_per_sample(const char *arg) 
{
  sound_file *sf;
  int result = MUS_ERROR;
  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg);
  if (sf) result = sf->bits_per_sample;
  MUS_UNLOCK(&sound_table_lock);
  return(result);
}



float mus_sound_duration(const char *arg) 
{
  float val = -1.0;
  sound_file *sf; 
  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg); 
  if (sf) 
    {
      if ((sf->chans > 0) && (sf->srate > 0))
	val = (float)((double)(sf->samples) / ((float)(sf->chans) * (float)(sf->srate)));
      else val = 0.0;
    }
  MUS_UNLOCK(&sound_table_lock);
  return(val);
}


int *mus_sound_loop_info(const char *arg)
{
  sound_file *sf; 
  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg); 
  if ((sf) && (sf->loop_modes))
    {
      int *info;
      info = (int *)CALLOC(MUS_LOOP_INFO_SIZE, sizeof(int));
      if (sf->loop_modes[0] != 0)
	{
	  info[0] = sf->loop_starts[0];
	  info[1] = sf->loop_ends[0];
	  info[6] = sf->loop_modes[0];
	}
      if (sf->loop_modes[1] != 0)
	{
	  info[2] = sf->loop_starts[1];
	  info[3] = sf->loop_ends[1];
	  info[7] = sf->loop_modes[1];
	}
      info[4] = sf->base_note;
      info[5] = sf->base_detune;
      MUS_UNLOCK(&sound_table_lock);
      return(info);
    }
  MUS_UNLOCK(&sound_table_lock);
  return(NULL);
}


void mus_sound_set_loop_info(const char *arg, int *loop)
{
  sound_file *sf; 
  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg); 
  if (sf)
    {
      if (sf->loop_modes == NULL)
	{
	  sf->loop_modes = (int *)CALLOC(2, sizeof(int));
	  sf->loop_starts = (int *)CALLOC(2, sizeof(int));
	  sf->loop_ends = (int *)CALLOC(2, sizeof(int));
	}
      sf->loop_modes[0] = loop[6]; 
      if (loop[6] != 0)
	{
	  sf->loop_starts[0] = loop[0];
	  sf->loop_ends[0] = loop[1];
	}
      else
	{
	  sf->loop_starts[0] = 0;
	  sf->loop_ends[0] = 0;
	}
      sf->loop_modes[1] = loop[7];
      if (loop[7] != 0)
	{
	  sf->loop_starts[1] = loop[2];
	  sf->loop_ends[1] = loop[3];
	}
      else
	{
	  sf->loop_starts[1] = 0;
	  sf->loop_ends[1] = 0;
	}
      sf->base_note = loop[4];
      sf->base_detune = loop[5];
    }
  MUS_UNLOCK(&sound_table_lock);
}


int mus_sound_mark_info(const char *arg, int **mark_ids, int **mark_positions)
{
  sound_file *sf; 
  int result = 0;
  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg); 
  if (sf)
    {
      (*mark_ids) = sf->marker_ids;
      (*mark_positions) = sf->marker_positions;
      result = sf->markers;
    }
  MUS_UNLOCK(&sound_table_lock);
  return(result);
}


char *mus_sound_comment(const char *name)
{
  off_t start, end, len;
  char *sc = NULL;
  sound_file *sf = NULL;
  MUS_LOCK(&sound_table_lock);
  sf = get_sf(name); 
  if (sf)
    {
      start = sf->comment_start;
      end = sf->comment_end;
      if (end == 0) 
	{
	  if (sf->aux_comment_start)
	    {
	      if ((sf->header_type == MUS_RIFF) ||
		  (sf->header_type == MUS_RF64))
		sc = mus_header_riff_aux_comment(name, 
						 sf->aux_comment_start, 
						 sf->aux_comment_end);
	      if ((sf->header_type == MUS_AIFF) || 
		  (sf->header_type == MUS_AIFC)) 
		sc = mus_header_aiff_aux_comment(name, 
						 sf->aux_comment_start, 
						 sf->aux_comment_end);
	    }
	}
      else
	{
	  if (end <= sf->true_file_length)
	    {
	      len = end - start + 1;
	      if (len > 0)
		{
		  /* open and get the comment */
		  ssize_t bytes;
		  int fd;
		  char *auxcom;
		  fd = mus_file_open_read(name);
		  if (fd == -1) return(NULL);
		  lseek(fd, start, SEEK_SET);
		  sc = (char *)CALLOC(len + 1, sizeof(char));
		  bytes = read(fd, sc, len);
		  CLOSE(fd, name);
		  if (((sf->header_type == MUS_AIFF) || 
		       (sf->header_type == MUS_AIFC)) &&
		      (sf->aux_comment_start) &&
		      (bytes != 0))
		    {
		      auxcom = mus_header_aiff_aux_comment(name, 
							   sf->aux_comment_start, 
							   sf->aux_comment_end);
		      if (auxcom)
			{
			  size_t full_len;
			  full_len = strlen(auxcom) + strlen(sc) + 2;
			  sc = (char *)REALLOC(sc, full_len * sizeof(char));
			  strcat(sc, "\n");
			  strcat(sc, auxcom);
			}
		    }
		}
	    }
	}
    }
  MUS_UNLOCK(&sound_table_lock);
  return(sc);
}


int mus_sound_open_input(const char *arg) 
{
  int fd = -1;
  if (!(mus_file_probe(arg)))
    mus_error(MUS_CANT_OPEN_FILE, S_mus_sound_open_input " can't open %s: %s", arg, STRERROR(errno));
  else
    {
      sound_file *sf = NULL;
      mus_sound_initialize();
      MUS_LOCK(&sound_table_lock);
      sf = get_sf(arg);
      if (sf) 
	{
	  fd = mus_file_open_read(arg);
	  mus_file_open_descriptors(fd, arg, sf->data_format, sf->datum_size, sf->data_location, sf->chans, sf->header_type);
	  lseek(fd, sf->data_location, SEEK_SET);
	}
      MUS_UNLOCK(&sound_table_lock);
    }
  return(fd);
}


int mus_sound_open_output(const char *arg, int srate, int chans, int data_format, int header_type, const char *comment)
{
  int fd = MUS_ERROR, err;
  mus_sound_initialize();
  mus_sound_forget(arg);
  err = mus_write_header(arg, header_type, srate, chans, 0, data_format, comment);
  if (err != MUS_ERROR)
    {
      fd = mus_file_open_write(arg);
      if (fd != -1)
	mus_file_open_descriptors(fd,
				  arg,
				  data_format,
				  mus_bytes_per_sample(data_format),
				  mus_header_data_location(),
				  chans,
				  header_type);
    }
  return(fd);
}


int mus_sound_reopen_output(const char *arg, int chans, int format, int type, off_t data_loc)
{
  int fd;
  mus_sound_initialize();
  fd = mus_file_reopen_write(arg);
  if (fd != -1)
    mus_file_open_descriptors(fd, arg, format, mus_bytes_per_sample(format), data_loc, chans, type);
  return(fd);
}


int mus_sound_close_input(int fd) 
{
  return(mus_file_close(fd)); /* this closes the clm file descriptors */
}


int mus_sound_close_output(int fd, off_t bytes_of_data) 
{
  char *name;
  name = mus_file_fd_name(fd);
  if (name)
    {
      int err = MUS_ERROR, old_type;
      char *fname;
      fname = strdup(name); /* strdup defined, if necessary, in io.c */
      old_type = mus_file_header_type(fd);
      err = mus_file_close(fd);        /* this frees the original fd->name, so we copied above */
      /* fd is NULL now */
      mus_sound_forget(fname);
      mus_header_change_data_size(fname, old_type, bytes_of_data);
      free(fname);
      return(err);
    }
  return(MUS_ERROR);
}


typedef enum {SF_CHANS, SF_SRATE, SF_TYPE, SF_FORMAT, SF_LOCATION, SF_SIZE} sf_field_t;

static int mus_sound_set_field(const char *arg, sf_field_t field, int val)
{
  sound_file *sf;
  int result = MUS_NO_ERROR;

  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg); 
  if (sf) 
    {
      switch (field)
	{
	case SF_CHANS:    
	  sf->chans = val;       
	  break;
	case SF_SRATE:    
	  sf->srate = val;       
	  break;
	case SF_TYPE:     
	  sf->header_type = val; 
	  break;
	case SF_FORMAT:   
	  sf->data_format = val; 
	  sf->datum_size = mus_bytes_per_sample(val); 
	  break;
	default: 
	  result = MUS_ERROR; 
	  break;
	}
    }
  else result = MUS_ERROR;
  MUS_UNLOCK(&sound_table_lock);
  return(result);
}


static int mus_sound_set_off_t_field(const char *arg, sf_field_t field, off_t val)
{
  sound_file *sf; 
  int result = MUS_NO_ERROR;

  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg); 
  if (sf) 
    {
      switch (field)
	{
	case SF_SIZE:     
	  sf->samples = val; 
	  break;
	case SF_LOCATION: 
	  sf->data_location = val; 
	  break;
	default: 
	  result = MUS_ERROR;
	  break;
	}
    }
  else result = MUS_ERROR;
  MUS_UNLOCK(&sound_table_lock);

  return(result);
}


int mus_sound_set_chans(const char *arg, int val)           {return(mus_sound_set_field(arg,       SF_CHANS,    val));}
int mus_sound_set_srate(const char *arg, int val)           {return(mus_sound_set_field(arg,       SF_SRATE,    val));}
int mus_sound_set_header_type(const char *arg, int val)     {return(mus_sound_set_field(arg,       SF_TYPE,     val));}
int mus_sound_set_data_format(const char *arg, int val)     {return(mus_sound_set_field(arg,       SF_FORMAT,   val));}
int mus_sound_set_data_location(const char *arg, off_t val) {return(mus_sound_set_off_t_field(arg, SF_LOCATION, val));}
int mus_sound_set_samples(const char *arg, off_t val)       {return(mus_sound_set_off_t_field(arg, SF_SIZE,     val));}


int mus_sound_override_header(const char *arg, int srate, int chans, int format, int type, off_t location, off_t size)
{
  sound_file *sf; 
  int result = MUS_NO_ERROR;
  /* perhaps once a header has been over-ridden, we should not reset the relevant fields upon re-read? */

  MUS_LOCK(&sound_table_lock);
  sf = get_sf(arg); 
  if (sf)
    {
      if (location != -1) sf->data_location = location;
      if (size != -1) sf->samples = size;
      if (format != -1) 
	{
	  sf->data_format = format;
	  sf->datum_size = mus_bytes_per_sample(format);
	}
      if (srate != -1) sf->srate = srate;
      if (chans != -1) sf->chans = chans;
      if (type != -1) sf->header_type = type;
    }
  else result = MUS_ERROR;
  MUS_UNLOCK(&sound_table_lock);

  return(result);
}


bool mus_sound_maxamp_exists(const char *ifile)
{
  sound_file *sf; 
  bool val = false;

  MUS_LOCK(&sound_table_lock);
  sf = get_sf(ifile); 
  val = ((sf) && (sf->maxamps));
  MUS_UNLOCK(&sound_table_lock);

  return(val);
}


off_t mus_sound_maxamps(const char *ifile, int chans, mus_sample_t *vals, off_t *times)
{
  off_t frames;
  int i, ichans, chn;

  MUS_LOCK(&sound_table_lock);

  {
    sound_file *sf; 

    sf = get_sf(ifile); 
    if (sf->chans <= 0)
      {
	MUS_UNLOCK(&sound_table_lock);
	return(MUS_ERROR);
      }
  
    if ((sf) && (sf->maxamps))
      {
	if (chans > sf->chans) 
	  ichans = sf->chans; 
	else ichans = chans;
	for (chn = 0; chn < ichans; chn++)
	  {
	    times[chn] = sf->maxtimes[chn];
	    vals[chn] = sf->maxamps[chn];
	  }
	frames = sf->samples / sf->chans;
	MUS_UNLOCK(&sound_table_lock);
	return(frames);
      }
  }
  MUS_UNLOCK(&sound_table_lock);

  {
    int j, bufnum, ifd;
    off_t n, curframes;
    mus_sample_t *buffer, *samp;
    off_t *time;
    mus_sample_t **ibufs;
    ifd = mus_sound_open_input(ifile);
    if (ifd == MUS_ERROR) return(MUS_ERROR);
    ichans = mus_sound_chans(ifile);
    frames = mus_sound_frames(ifile);
    if (frames == 0) 
      {
	mus_sound_close_input(ifd);
	return(0);
      }

    mus_file_seek_frame(ifd, 0);
    ibufs = (mus_sample_t **)CALLOC(ichans, sizeof(mus_sample_t *));
    bufnum = 8192;
    for (j = 0; j < ichans; j++) 
      ibufs[j] = (mus_sample_t *)CALLOC(bufnum, sizeof(mus_sample_t));
    time = (off_t *)CALLOC(ichans, sizeof(off_t));
    samp = (mus_sample_t *)CALLOC(ichans, sizeof(mus_sample_t));
    for (n = 0; n < frames; n += bufnum)
      {
	if ((n + bufnum) < frames) 
	  curframes = bufnum; 
	else curframes = (frames - n);
	mus_file_read(ifd, 0, curframes - 1, ichans, ibufs);
	for (chn = 0; chn < ichans; chn++)
	  {
	    buffer = (mus_sample_t *)(ibufs[chn]);
	    for (i = 0; i < curframes; i++) 
	      {
		mus_sample_t abs_samp;
		abs_samp = mus_sample_abs(buffer[i]);
		if (abs_samp > samp[chn])
		  {
		    time[chn] = i + n; 
		    samp[chn] = abs_samp;
		  }
	      }
	  }
      }
    mus_sound_close_input(ifd);
    mus_sound_set_maxamps(ifile, ichans, samp, time); /* save the complete set */
    if (ichans > chans) ichans = chans;
    for (chn = 0; chn < ichans; chn++)
      {
	times[chn] = time[chn];
	vals[chn] = samp[chn];
      }
    FREE(time);
    FREE(samp);
    for (j = 0; j < ichans; j++) FREE(ibufs[j]);
    FREE(ibufs);
    return(frames);
  }
}


int mus_sound_set_maxamps(const char *ifile, int chans, mus_sample_t *vals, off_t *times)
{
  sound_file *sf; 
  int result = MUS_NO_ERROR;

  MUS_LOCK(&sound_table_lock);

  sf = get_sf(ifile); 
  if (sf)
    {
      int i, ichans = 0;
      if (sf->maxamps)
	{
	  if (chans > sf->chans) 
	    ichans = sf->chans; 
	  else ichans = chans;
	  for (i = 0; i < ichans; i++)
	    {
	      sf->maxtimes[i] = times[i];
	      sf->maxamps[i] = vals[i];
	    }
	}
      else
	{
	  ichans = sf->chans; /* mus_sound_chans(ifile) */
	  if (sf->maxamps == NULL) 
	    {
	      /* here we need to use the max, since the caller may be confused */
	      int max_chans;
	      max_chans = ichans;
	      if (max_chans < chans)
		max_chans = chans;

	      sf->maxamps = (mus_sample_t *)CALLOC(max_chans, sizeof(mus_sample_t));
	      sf->maxtimes = (off_t *)CALLOC(max_chans, sizeof(off_t));
	    }

	  if (ichans > chans) 
	    ichans = chans;
	  for (i = 0; i < ichans; i++)
	    {
	      sf->maxtimes[i] = times[i];
	      sf->maxamps[i] = vals[i];
	    }
	}
    }
  else result = MUS_ERROR;
  MUS_UNLOCK(&sound_table_lock);

  return(result);
}


off_t mus_file_to_array(const char *filename, int chan, off_t start, off_t samples, mus_sample_t *array)
{
  int ifd, chans;
  off_t total_read;
  mus_sample_t **bufs;
  ifd = mus_sound_open_input(filename);
  if (ifd == MUS_ERROR) return(MUS_ERROR);
  chans = mus_sound_chans(filename);
  if (chan >= chans) 
    {
      mus_sound_close_input(ifd);      
      return(mus_error(MUS_NO_SUCH_CHANNEL, "mus_file_to_array can't read %s channel %d (file has %d chans)", filename, chan, chans));
    }
  bufs = (mus_sample_t **)CALLOC(chans, sizeof(mus_sample_t *));
  bufs[chan] = array;
  mus_file_seek_frame(ifd, start);
  total_read = mus_file_read_any(ifd, 0, chans, samples, bufs, bufs);
  mus_sound_close_input(ifd);
  FREE(bufs);
  return(total_read);
}


const char *mus_array_to_file_with_error(const char *filename, mus_sample_t *ddata, off_t len, int srate, int channels)
{
  /* put ddata into a sound file, taking byte order into account */
  /* assume ddata is interleaved already if more than one channel */
  int fd, err = MUS_NO_ERROR;
  off_t oloc;
  mus_sample_t *bufs[1];
  mus_sound_forget(filename);

  err = mus_write_header(filename, MUS_NEXT, srate, channels, len * channels, MUS_OUT_FORMAT, NULL);
  if (err != MUS_NO_ERROR)
    return("mus_array_to_file can't create output file");
  oloc = mus_header_data_location();
  fd = mus_file_reopen_write(filename);
  lseek(fd, oloc, SEEK_SET);
  err = mus_file_open_descriptors(fd, filename,
				  MUS_OUT_FORMAT,
				  mus_bytes_per_sample(MUS_OUT_FORMAT),
				  oloc, channels, MUS_NEXT);
  if (err != MUS_ERROR)
    {
      bufs[0] = ddata;
      err = mus_file_write(fd, 0, len - 1, 1, bufs); /* 1 = chans?? */
    }
  mus_file_close(fd);
  if (err == MUS_ERROR)
    return("mus_array_to_file write error");
  return(NULL);
}

int mus_array_to_file(const char *filename, mus_sample_t *ddata, off_t len, int srate, int channels)
{
  const char *errmsg;
  errmsg = mus_array_to_file_with_error(filename, ddata, len, srate, channels);
  if (errmsg)
    return(mus_error(MUS_CANT_OPEN_FILE, errmsg));
  return(MUS_NO_ERROR);
}


off_t mus_file_to_float_array(const char *filename, int chan, off_t start, off_t samples, Float *array)
{
#if SNDLIB_USE_FLOATS
  return(mus_file_to_array(filename, chan, start, samples, array));
#else
  mus_sample_t *idata;
  off_t i, len;

  idata = (mus_sample_t *)CALLOC(samples, sizeof(mus_sample_t));
  len = mus_file_to_array(filename, chan, start, samples, idata);
  if (len != -1) 
    for (i = 0; i < samples; i++)
      array[i] = MUS_SAMPLE_TO_FLOAT(idata[i]);
  FREE(idata);

  return(len);
#endif
}


int mus_float_array_to_file(const char *filename, Float *ddata, off_t len, int srate, int channels)
{
  const char *errmsg;
#if SNDLIB_USE_FLOATS
  errmsg = mus_array_to_file_with_error(filename, ddata, len, srate, channels);
#else
  mus_sample_t *idata;
  off_t i;

  idata = (mus_sample_t *)CALLOC(len, sizeof(mus_sample_t));
  for (i = 0; i < len; i++) 
    idata[i] = MUS_FLOAT_TO_SAMPLE(ddata[i]);
  errmsg = mus_array_to_file_with_error(filename, idata, len, srate, channels);
  FREE(idata);
#endif
  if (errmsg)
    return(mus_error(MUS_CANT_OPEN_FILE, errmsg));

  return(MUS_NO_ERROR);
}



#if HAVE_PTHREADS && MUS_DEBUGGING
  void io_set_table_lock_name(void);
  void utils_set_pointer_lock_name(void);
  void xen_set_gc_lock_name(void);
  void xen_set_string_lock_name(void);
#endif

static bool sndlib_initialized = false;

int mus_sound_initialize(void)
{
  if (!sndlib_initialized)
    {
      int err = MUS_NO_ERROR;
      sndlib_initialized = true;
#if HAVE_PTHREADS
      pthread_key_create(&mus_thread_previous_error_handler, NULL);
      pthread_key_create(&mus_thread_error_handler, NULL);
      pthread_setspecific(mus_thread_error_handler, (void *)default_mus_error);
#else
      mus_error_handler = default_mus_error;
#endif

#if HAVE_PTHREADS && MUS_DEBUGGING
      pthread_key_create(&mus_thread_locks, (void *)free_locks);
      mus_lock_set_name(&sound_error_lock, "mus_error");
      mus_lock_set_name(&sound_print_lock, "mus_print");
      mus_lock_set_name(&sound_table_lock, "sound_table");
      io_set_table_lock_name();
      xen_set_gc_lock_name();
      utils_set_pointer_lock_name();
      xen_set_string_lock_name();
#endif

      err = mus_header_initialize();
      if (err == MUS_NO_ERROR) 
	err = mus_audio_initialize();
      return(err);
    }
  return(MUS_NO_ERROR);
}


