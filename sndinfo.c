/* sndinfo describes sounds */

#if defined(HAVE_CONFIG_H)
  #include "config.h"
#endif

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#if (defined(NEXT) || (defined(HAVE_LIBC_H) && (!defined(HAVE_UNISTD_H))))
  #include <libc.h>
#else
  #if (!(defined(_MSC_VER))) && (!(defined(MPW_C)))
    #include <unistd.h>
  #endif
  #include <string.h>
#endif
#include <errno.h>
#include <time.h>

#include "sndlib.h"

#if MACOS
  #include <console.h>
#endif

#ifdef DEBUG_MEMORY
void *mem_calloc(size_t len, size_t size, const char *func, const char *file, int line) {return(calloc(len,size));}
void *mem_malloc(size_t len, const char *func, const char *file, int line) {return(malloc(len));}
void mem_free(void *ptr, const char *func, const char *file, int line) {free(ptr);}
void *mem_realloc(void *ptr, size_t size, const char *func, const char *file, int line) {return(realloc(ptr,size));}
#endif

int main(int argc, char *argv[])
{
  int chans,srate,samples,format,type;
  float length;
  time_t date;
  int *loops = NULL;
  char *comment,*header_name;
  char *format_info,*format_name;
  char timestr[64];
#if MACOS
  argc = ccommand(&argv);
#endif
#if HAVE_GDBM
  if (argc == 1) {mus_sound_print_cache(); exit(0);}
#else
  if (argc == 1) {printf("usage: sndinfo file\n"); exit(0);}
#endif
  mus_sound_initialize();
  if (mus_file_probe(argv[1])) /* see if it exists */
    {
      date = mus_sound_write_date(argv[1]);
      srate = mus_sound_srate(argv[1]);
      chans = mus_sound_chans(argv[1]);
      samples = mus_sound_samples(argv[1]);
      comment = mus_sound_comment(argv[1]); 
      length = (float)samples / (float)(chans * srate);
      loops = mus_sound_loop_info(argv[1]);
      type = mus_sound_header_type(argv[1]);
      header_name = mus_header_type_name(type);
      format = mus_sound_data_format(argv[1]);
      if (format != MUS_UNSUPPORTED)
	format_info = mus_data_format_name(format);
      else
	{
	  format_info = (char *)calloc(64,sizeof(char));
	  format = mus_sound_original_format(argv[1]);
	  format_name = mus_header_original_format_name(format,type);
	  if (format_name)
	    sprintf(format_info,"%d (%s)",format,format_name);
	  else sprintf(format_info,"%d",format);
	}
#if (!defined(HAVE_CONFIG_H)) || defined(HAVE_STRFTIME)
      strftime(timestr,64,"%a %d-%b-%Y %H:%M %Z",localtime(&date));
#else
      sprintf(timestr,"who knows?");
#endif
      fprintf(stdout,"%s:\n  srate: %d\n  chans: %d\n  length: %f\n",
	      argv[1],srate,chans,length);
      fprintf(stdout,"  type: %s\n  format: %s\n  written: %s\n  comment: %s\n",
	      header_name,
	      format_info,
	      timestr,(comment) ? comment : "");
      if (loops)
	{
	  if (loops[0] != 0)
	    fprintf(stdout,"  loop: %d to %d\n",loops[1],loops[2]);
	  if (loops[3] != 0)
	    fprintf(stdout,"  loop: %d to %d\n",loops[4],loops[5]);
	  if (loops[0] != 0)
	    fprintf(stdout,"    base: %d, detune: %d\n",loops[7],loops[6]);
	}
    }
  else
    fprintf(stderr,"%s: %s\n",argv[1],strerror(errno));
  return(0);
}
