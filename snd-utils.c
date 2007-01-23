#include "snd.h"

int snd_round(double x) /* needs to be double here (not Float) for x axis calcs */
{
  int i;
  i = (int)x;
  if ((x - i) > 0.5) return(i + 1);
  return(i);
}

off_t snd_round_off_t(double x)
{
  off_t i;
  i = (off_t)x;
  if ((x - i) > 0.5) return(i + 1);
  return(i);
}

off_t snd_abs_off_t(off_t val)
{
  /* div is also limited to int */
  return((val < 0) ? -val : val);
}

#define POW2_SIZE 31
static int ipow2s[POW2_SIZE] = {1, 2, 4, 8, 16, 32, 64, 128, 256, 
				512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 
				131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608, 16777216, 
				33554432, 67108864, 134217728, 268435456, 536870912, 1073741824};
int snd_int_pow2(int n)
{
  return(ipow2s[n]);
}

int snd_to_int_pow2(int n)
{
  /* round up to next power of 2 */
  int i;
  for (i = 0; i < POW2_SIZE; i++)
    if (ipow2s[i] >= n)
      return(ipow2s[i]);
  return(0);
}

int snd_int_log2(int n)
{
  /* round down */
  int i;
  for (i = 1; i < POW2_SIZE; i++)
    if (ipow2s[i] > n)
      return(i - 1);
  return(0);
}

/* removed off_t power-of-2 stuff for huge fft's etc: convolution_filter in snd-sig.c, make_fft_state in snd-fft.c 
 *   see note in snd-chn -- this involves a lot of work, and either changes in fftw or fallbacks for it
 */

#define MAX_FLOAT_DIFF_FOR_EQUAL 0.0000001
bool snd_feq(Float val1, Float val2)
{
  /* if some float can be affected by a widget, we can easily get float inaccuracies that confuse "==" (in gtk in particular) */
  if (val1 == val2) return(true);
  if (fabs(val1 - val2) < MAX_FLOAT_DIFF_FOR_EQUAL) return(true);
  return(false);
}

#if !(defined(__GNUC__) && (!(defined(__cplusplus))))
Float in_dB(Float min_dB, Float lin_dB, Float val)
{
  return((val <= lin_dB) ? min_dB : (20.0 * log10(val)));
}
#endif

#if MUS_DEBUGGING
char *copy_string_1(const char *str, const char *func, const char *file, int line)
{
  char *newstr = NULL;
  if (str)
    {
      newstr = (char *)mem_malloc((strlen(str) + 1) * sizeof(char), func, file, line);
      set_printable(PRINT_CHAR);
      strcpy(newstr, str);
    }
  return(newstr);
}
#else
char *copy_string(const char *str)
{
  if (str)
    return(strdup(str));
  return(NULL);
}
#endif

int snd_strlen(const char *str)
{
  /* strlen(NULL) -> seg fault! */
  if ((str) && (*str)) return(strlen(str));
  return(0);
}

char *snd_strcat(char *errmsg, const char *str, int *size)
{
  int new_len, err_size;
  new_len = (snd_strlen(str) + snd_strlen(errmsg));
  err_size = size[0];
  if (new_len >= err_size)
    {
      if ((err_size * 2) > new_len)
	err_size = err_size * 2;
      else err_size = new_len;
      errmsg = (char *)REALLOC(errmsg, err_size * sizeof(char));
      size[0] = err_size;
    }
  strcat(errmsg, str);
  return(errmsg);
}

#if HAVE_STRFTIME
#define TIME_STR_SIZE 64
static char time_buf[TIME_STR_SIZE];
#endif

char *snd_local_time(void)
{
#if HAVE_STRFTIME
  time_t ts;
  time(&ts);
  strftime(time_buf, TIME_STR_SIZE, STRFTIME_FORMAT, localtime(&ts));
  return(time_buf);
#else
  return(" ");
#endif
}

char *snd_strftime(const char *format, time_t date)
{
#if HAVE_STRFTIME
  strftime(time_buf, TIME_STR_SIZE, format, localtime(&date));
  return(time_buf);
#else
  return(" ");
#endif
}

char *snd_io_strerror(void) /* "snd_strerror" is exported by ALSA! */
{
  if (ss->local_errno != 0)
    return(strerror(ss->local_errno));
  if (errno != 0)
    return(strerror(errno));
  return(NULL);
}

char *snd_open_strerror(void)
{
  if (ss->local_open_errno != 0)
    return(strerror(ss->local_open_errno));
  return(snd_io_strerror());
}

char *string_to_colon(char *val)
{
  char *up_to_colon;
  int i, len;
  up_to_colon = (char *)CALLOC(strlen(val) + 1, sizeof(char));
  len = strlen(val);
  for (i = 0; i < len; i++)
    {
      if ((val[i] == ':') || (val[i] == ' '))
	{
	  up_to_colon[i] = 0;
	  return(up_to_colon);
	}
      up_to_colon[i] = val[i];
    }
  return(up_to_colon);
}

char *filename_without_directory(const char *name)
{
  /* since I don't want to mess with freeing these guys, I'll just return a pointer into the name */
  int i, len, last_slash;
  last_slash = 0;
  len = strlen(name);
  for (i = 0; i < len - 1; i++) 
    if (name[i] == '/') 
      last_slash = i + 1;
  return((char *)(name + last_slash));
}

char *just_filename(char *name)
{
  char *nodir;
  int i, len;
  nodir = copy_string(filename_without_directory(name));
  len = strlen(nodir);
  for (i = 0; i < len; i++) 
    if (nodir[i] == '.') 
      {
	nodir[i] = '\0'; 
	break;
      }
  return(nodir);
}

char *just_directory(const char *name)
{
  int i, len, last_slash = 0;
  char *dirname = NULL;
  len = strlen(name);
  dirname = (char *)CALLOC(len + 1, sizeof(char));
  for (i = 0; i < len - 1; i++) 
    if (name[i] == '/') 
      last_slash = i + 1;
  if (last_slash > 0)
    strncpy(dirname, name, last_slash);
  return(dirname);
}

bool directory_exists(char *name)
{
  char temp;
  bool result;
  int i, len, last_slash = -1;
  len = strlen(name);
  for (i = 0; i < len; i++) 
    if (name[i] == '/') 
      last_slash = i;
  if (last_slash <= 0)
    return(true);
  if (last_slash >= len - 1) /* can't be > */
    return(directory_p(name));
  temp = name[last_slash + 1];
  name[last_slash + 1] = '\0';
  result = directory_p(name);
  name[last_slash + 1] = temp;
  return(result);
}

char *file_to_string(const char *filename)
{ 
  FILE *file;
  long size;
  char *content = NULL;
  file = fopen(filename, "r");
  if (!file) return(NULL);
  fseek(file, 0, SEEK_END);
  size = ftell(file);
  if (size > 0)
    {
      size_t bytes;
      rewind(file);
      content = (char *)CALLOC(size + 1, sizeof(char));
      bytes = fread(content, sizeof(char), size, file);
      if (bytes == 0)
	fprintf(stderr, "file->string read error");
    }
  fclose(file);
  return(content);
}

char *vstr(const char *format, va_list ap)
{
  char *buf;
  int len;
  len = snd_strlen(format) + PRINT_BUFFER_SIZE;
  buf = (char *)CALLOC(len, sizeof(char));
 #if HAVE_VSNPRINTF
  vsnprintf(buf, len, format, ap);
 #else
  vsprintf(buf, format, ap);
 #endif
  return(buf);
}

disk_space_t disk_space_p(off_t bytes, const char *filename)
{
  off_t kfree, kneeded;
  kfree = disk_kspace(filename);
  if (kfree < 0) 
    {
      snd_error(_("can't access %s: %s"), filename, snd_io_strerror()); 
      return(NO_DISK_SPACE);
    }
  kneeded = bytes >> 10;
  if (kfree < kneeded)
    {
      snd_error(_("not enough space left on disk: only " OFF_TD " kbytes available"), kfree);
      return(NOT_ENOUGH_DISK_SPACE);
    }
  return(DISK_SPACE_OK);
}


const char *short_data_format_name(int sndlib_format, const char *filename)
{
  if (MUS_DATA_FORMAT_OK(sndlib_format))
    return(mus_data_format_short_name(sndlib_format));
  else return(mus_header_original_format_name(mus_sound_original_format(filename),
					      mus_sound_header_type(filename)));
}


#if HAVE_LANGINFO_DECIMAL_POINT || HAVE_LANGINFO_RADIXCHAR
#if NL_TYPES_H_DEFINES_MALLOC
  /* SGI complication */
  #undef MALLOC
  #include <langinfo.h>
  #undef MALLOC
  #define MALLOC malloc
#else
  #include <langinfo.h>
#endif
#endif

static char decimal_pt;
static char local_decimal_point(void)
{
#if HAVE_LANGINFO_DECIMAL_POINT
  return(nl_langinfo(DECIMAL_POINT)[0]);
#endif
#if HAVE_LANGINFO_RADIXCHAR
  return(nl_langinfo(RADIXCHAR)[0]);
#endif
  return('.');
}

char *prettyf(double num, int tens)
{ 
  /* try to prettify float display -- if tens <= 0, return int */
  static char prtbuf[256];

  if (tens <= 0)
    sprintf(prtbuf, "%d", (int)snd_round(num));
  else 
    {
      int i, len;
      sprintf(prtbuf, "%.*f", tens, num); /* %f assumes double arg */
      /* look for trailing 0's beyond the ddd.0 case */
      len = strlen(prtbuf);
      for (i = len - 1; (i > 0) && (prtbuf[i] == '0') && (prtbuf[i - 1] != '.'); i--)
	prtbuf[i] = '\0';
    }
  return(copy_string(prtbuf));
}

static char *get_tmpdir(void)
{
  char *tmpdir = NULL;
  int len;
  tmpdir = copy_string(getenv("TMPDIR"));
  if ((tmpdir == NULL) && (MUS_DEFAULT_TEMP_DIR)) tmpdir = copy_string(MUS_DEFAULT_TEMP_DIR);
#ifdef P_tmpdir
  if (tmpdir == NULL) tmpdir = copy_string(P_tmpdir); /* /usr/include/stdio.h */
#else
  if (tmpdir == NULL) return(copy_string("/tmp"));
#endif
  if (tmpdir == NULL) return(copy_string("."));
  len = strlen(tmpdir);
  if (tmpdir[len - 1] == '/') tmpdir[len - 1] = 0; /* this is what forces us to copy the string above (Sun segfaults otherwise) */
  return(tmpdir);
}

static int sect_ctr = 0;
char *shorter_tempnam(const char *udir, const char *prefix)
{
  /* tempnam turns out names that are inconveniently long (in this case the filename is user-visible) */
  char *str, *tmpdir = NULL;
  str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  if ((udir == NULL) || (snd_strlen(udir) == 0)) 
    tmpdir = get_tmpdir(); /* incoming dir could be "" */
  else tmpdir = copy_string(udir);
  mus_snprintf(str, PRINT_BUFFER_SIZE, "%s/%s%d_%d.snd", tmpdir, (prefix) ? prefix : "snd_", (int)getpid(), sect_ctr++);
  if (tmpdir) FREE(tmpdir);
  tmpdir = copy_string(str);
  FREE(str);
  return(tmpdir);
}

char *snd_tempnam(void)
{
  /* problem here is that NULL passed back from Guile becomes "" which is not NULL from tempnam's point of view */
  char *udir;
  udir = temp_dir(ss);
  if ((udir) && (*udir))
    return(shorter_tempnam(udir, "snd_"));
  return(shorter_tempnam(NULL, "snd_"));
}

void snd_exit(int val)
{
#ifndef SND_AS_WIDGET
  exit(val);
#endif
}


/* ---------------- file alteration monitor (fam or gamin) ---------------- */
#if HAVE_FAM
#define MUS_DEBUGGING_FAM 0

/* one confusing thing: if user deletes the file we're monitoring, apparently Linux doesn't
 *   actually delete it, though a directory monitor reports the deletion; the file itself
 *   hangs around, I guess because we have it open(?) -- can't decide how to deal with this.
 *
 * The main open is in snd_chn add_channel_data, and the file channel is held open unless
 *   too_many_files forces us to close as many as possible.
 */

static bool fam_already_warned = false;

char *fam_event_name(int code)
{
  switch (code)
    {
    case FAMChanged:        return("Changed");        break;
    case FAMDeleted:        return("Deleted");        break;
    case FAMCreated:        return("Created");        break;
    case FAMMoved:          return("Moved");          break;
    case FAMStartExecuting: return("StartExecuting"); break;
    case FAMStopExecuting:  return("StopExecuting");  break;
    case FAMAcknowledge:    return("Acknowledge");    break;
    case FAMExists:         return("Exists");         break;
    case FAMEndExist:       return("EndExist");       break;
    }
  return("unknown");
}

static const char *fam_error_to_string(int err)
{
  if (err > 0)
    return(FamErrlist[err]); /* not sure this actually does anything anymore */
  return("0");
}

static void fam_check(void)
{
  FAMEvent fe; 
  if (!(ss->fam_ok)) return;
  while (FAMPending(ss->fam_connection) > 0)
    {
      if (FAMNextEvent(ss->fam_connection, &fe) < 0) 
	snd_error("fam error: %s\n", fam_error_to_string(FAMErrno));
      else
	{
	  if (!(ss->checking_explicitly)) 
	    /* not in check_for_event -- we can't just ignore these because gamin hangs */
	    {
	      switch (fe.code)
		{
		case FAMStartExecuting:
		case FAMStopExecuting:
		case FAMAcknowledge:
		case FAMExists:
		case FAMEndExist:
		  /* ignore these (fp pointer can be a dangling reference here) */
		  break;
		default:
		  {
		    fam_info *fp = (fam_info *)(fe.userdata);
		    if ((!fp) || (fp->action == fp->data))
		      fprintf(stderr, "no fam user data!");
		    else 
		      {
#if MUS_DEBUGGING_FAM
			fprintf(stderr, "fam %s: %d (%s): %p\n", fe.filename, fe.code, fam_event_name(fe.code), fp);
#endif
			(*(fp->action))(fp, &fe);
		      }
		  }
		  break;
		}
	    }
	}
    }
}


#if USE_MOTIF
static void fam_reader(XtPointer context, int *fd, XtInputId *id)
{
  fam_check();
}
#else
static gboolean fam_reader(GIOChannel *source, GIOCondition condition, gpointer data)
{
  fam_check();
  return(true);
}
#endif


static fam_info *make_fam_info(FAMRequest *rp, void *data, void (*action)(struct fam_info *fp, FAMEvent *fe))
{
  fam_info *fp;
  fp = (fam_info *)CALLOC(1, sizeof(fam_info));
#if MUS_DEBUGGING
  set_printable(PRINT_FAM_INFO);
#endif
  fp->data = data;
  fp->action = action;
  fp->rp = rp;
  return(fp);
}
  
static FAMRequest *fam_monitor(void)
{
  if (!fam_already_warned)
    {
      if (!(ss->fam_connection))
	{
	  int err;
	  ss->fam_connection = (FAMConnection *)CALLOC(1, sizeof(FAMConnection));
	  err = FAMOpen(ss->fam_connection);
	  if (err < 0)
	    {
	      fam_already_warned = true;
	      snd_warning("can't start file alteration monitor: %s\n", fam_error_to_string(FAMErrno));
	      ss->fam_ok = false;
	      return(NULL);
	    }
	  else
	    {
	      int fd;
	      ss->fam_ok = true;
	      fd = FAMCONNECTION_GETFD(ss->fam_connection);
#if USE_MOTIF
	      ss->sgx->fam_port = XtAppAddInput(MAIN_APP(ss),
						fd,
						(XtPointer)XtInputReadMask,
						fam_reader,
						NULL);
#endif
#if USE_GTK
	      {
		GIOChannel *channel;
		channel = g_io_channel_unix_new(fd);
		ss->sgx->fam_port = g_io_add_watch_full(channel, 
							G_PRIORITY_DEFAULT, 
							(GIOCondition)(G_IO_IN | G_IO_HUP | G_IO_ERR), 
							fam_reader, NULL, NULL);
		g_io_channel_unref(channel);
	      }
#endif
	    }
	}
      return((FAMRequest *)CALLOC(1, sizeof(FAMRequest)));
    }
  return(NULL);
}

fam_info *fam_monitor_file(const char *filename, 
			   void *data, 
			   void (*action)(struct fam_info *fp, FAMEvent *fe))
{
  fam_info *fp = NULL;
  FAMRequest *rp = NULL;
  int err;
  if (!(with_file_monitor(ss))) return(NULL);
  rp = fam_monitor();
  if (rp)
    {
#if MUS_DEBUGGING_FAM
      fprintf(stderr, "monitor %s\n", filename);
#endif
      fp = make_fam_info(rp, data, action);
#if MUS_DEBUGGING
      fp->filename = copy_string(filename);
#endif
      err = FAMMonitorFile(ss->fam_connection, filename, rp, (void *)fp);
      if (err < 0)
	{
	  snd_warning("can't monitor %s: %s (free %p %p)\n", filename, fam_error_to_string(FAMErrno), fp, rp);
	  FREE(rp);
	  rp = NULL;
	  FREE(fp);
	  fp = NULL;
	}
      else return(fp);
    }
  else 
    {
      if (!fam_already_warned)
	snd_warning("can't get fam request for %s: %s\n", filename, fam_error_to_string(FAMErrno));
    }
  return(NULL);
}

fam_info *fam_monitor_directory(const char *dir_name,
				void *data, 
				void (*action)(struct fam_info *fp, FAMEvent *fe))
{
  fam_info *fp = NULL;
  FAMRequest *rp = NULL;
  int err;
  if (!(with_file_monitor(ss))) return(NULL);
  rp = fam_monitor();
  if (rp)
    {
      fp = make_fam_info(rp, data, action);
#if MUS_DEBUGGING
      fp->filename = copy_string(dir_name);
#endif
      err = FAMMonitorDirectory(ss->fam_connection, dir_name, rp, (void *)fp);
      if (err < 0)
	{
	  snd_warning("can't monitor %s: %s\n", dir_name, fam_error_to_string(FAMErrno));
	  FREE(rp);
	  rp = NULL;
	  FREE(fp);
	  fp = NULL;
	}
      else return(fp);
    }
  else 
    {
      if (!fam_already_warned)
	snd_warning("can't get fam request for %s: %s\n", dir_name, fam_error_to_string(FAMErrno));
    }
  return(NULL);
}

fam_info *fam_unmonitor_file(const char *filename, fam_info *fp)
{
  int err;
  if (fp)
    {
#if MUS_DEBUGGING_FAM
      fprintf(stderr, "unmonitor %s: %p %p\n", filename, fp, fp->rp);
#endif
#if MUS_DEBUGGING
      if (fp->filename) {FREE(fp->filename); fp->filename = NULL;}
#endif
      if (fp->rp)
	{
	  err = FAMCancelMonitor(ss->fam_connection, fp->rp);
	  if (err < 0)
	    snd_warning("can't unmonitor %s: %s\n", filename, fam_error_to_string(FAMErrno));
	  FREE(fp->rp);
	  /* /usr/include/fam.h implies that cancel frees this, but valgrind seems to disagree */
	  /* as far as I can see, gamin (libgamin/gam_api.c) does not free it */
	  /*   nor does fam (fam/fam.c++ in 2.6.10 does not, and their test case assumes it won't) */
	  fp->rp = NULL;
	  /* FREE(fp) below in debugging case will fill this with 'X', 0x858585... */
	}
      fp->action = NULL;
      fp->data = NULL;
      FREE(fp);
    }
  return(NULL);
}

#else
/* no fam */

fam_info *fam_monitor_file(const char *filename, 
			   void *data, 
			   void (*action)(struct fam_info *fp, FAMEvent *fe))
{
  return(NULL);
}

fam_info *fam_monitor_directory(const char *dir_name,
				void *data, 
				void (*action)(struct fam_info *fp, FAMEvent *fe))
{
  return(NULL);
}

fam_info *fam_unmonitor_file(const char *filename, fam_info *fp)
{
  if (fp) free(fp);
  return(NULL);
}

char *fam_event_name(int code)
{
  return("no fam!");
}
#endif

fam_info *fam_unmonitor_directory(const char *filename, fam_info *fp)
{
  return(fam_unmonitor_file(filename, fp));
}


/* ---------------- memory tracking etc ---------------- */
#if MUS_DEBUGGING

/* mtrace-style malloc hooks are not very useful here since I don't care
 * about X allocations (of which there are millions), and I need readable
 * backtrace info for leaks.  Doing it by hand makes it easy to sort
 * output and whatnot. All of Sndlib and Snd use the macros CALLOC,
 * MALLOC, REALLOC, and FREE.
 */

static char *kmg(size_t num)
{
  /* return number 0..1024, then in terms of K, M, G */
  char *str;
  str = (char *)calloc(LABEL_BUFFER_SIZE, sizeof(char));
  if (num > 1024)
    {
      if (num > (1024 * 1024))
	{
	  if (num > (1024 * 1024 * 1024))
	    mus_snprintf(str, LABEL_BUFFER_SIZE, "%.5fG", (float)num / (float)(1024 * 1024 * 1024));
	  else mus_snprintf(str, LABEL_BUFFER_SIZE, "%.4fM", (float)num / (float)(1024 * 1024));
	}
      else mus_snprintf(str, LABEL_BUFFER_SIZE, "%.3fK", (float)num / 1024.0);
    }
  else mus_snprintf(str, LABEL_BUFFER_SIZE, "%Zu", num); /* %Z, rather than %z -- the latter confuses old versions of gcc */
  return(str);
}

#define MEM_PAD_SIZE 32
static int mem_size = 0, max_mem_size = 0;
static size_t *sizes = NULL;
static int *locations = NULL;
static void **pointers = NULL, **true_pointers = NULL;
static char **functions = NULL, **files = NULL;
static int *printable = NULL;
static int *lines = NULL;
static int mem_location = -1;
static int mem_locations = 0;
static int *lines_hit = NULL;

static int find_mem_location(const char *func, const char *file, int line)
{
  int i;
  if ((lines_hit) && 
      (lines_hit[line] > 0) &&
      (strcmp(file, files[lines_hit[line]]) == 0))
    return(lines_hit[line]);
  for (i = 0; i <= mem_location; i++)
    if ((line == lines[i]) &&
	(strcmp(file, files[i]) == 0))
      return(i);
  mem_location++;
  if (mem_location >= mem_locations)
    {
      if (mem_locations == 0)
	{
	  functions = (char **)calloc(2048, sizeof(char *));
	  files = (char **)calloc(2048, sizeof(char *));
	  printable = (int *)calloc(2048, sizeof(int));
	  lines = (int *)calloc(2048, sizeof(int));
	  lines_hit = (int *)calloc(65536, sizeof(int));
	  mem_locations = 2048;
	}
      else
	{
	  functions = (char **)realloc(functions, (mem_locations + 1024) * sizeof(char *));
	  files = (char **)realloc(files, (mem_locations + 1024) * sizeof(char *));
	  printable = (int *)realloc(printable, (mem_locations + 1024) * sizeof(int));
	  lines = (int *)realloc(lines, (mem_location + 1024) * sizeof(int));
	  for (i = 0; i < 1024; i++) 
	    {
	      functions[i + mem_locations] = NULL;
	      files[i + mem_locations] = NULL;
	      printable[i + mem_locations] = 0;
	      lines[i + mem_locations] = 0;
	    }
	  mem_locations += 1024;
	}
    }
  /* NOT copy_string HERE!! */
  functions[mem_location] = (char *)calloc(strlen(func) + 1, sizeof(char));
  strcpy(functions[mem_location], func);
  files[mem_location] = (char *)calloc(strlen(file) + 1, sizeof(char));
  strcpy(files[mem_location], file);
  lines[mem_location] = line;

  if (line < 65536)
    {
      if (lines_hit[line] == 0) 
	lines_hit[line] = mem_location;
      else lines_hit[line] = -1;
    }
  return(mem_location);
}

static void fdescribe_pointer(FILE *fp, void *p)
{
  int loc;
  char *p3 = (char *)p;
  loc = (*((int *)(p3 - 4)));
  fprintf(fp, "%s[%d]:%s, len:%Zu", files[loc], lines[loc], functions[loc], sizes[loc]);
  if (printable[loc] == PRINT_CHAR)
    fprintf(fp, ": %s", p3);
}

static void describe_pointer(void *p)
{
  fdescribe_pointer(stderr, p);
}

static char pad[MEM_PAD_SIZE] = {'W','I','L','L','I','A','M',' ','G','A','R','D','N','E','R',' ','S','C','H','O','T','T','S','T','A','E','D','T',' ','D','M','A'};
static void set_padding(void *p1, void *p2, size_t len, int loc)
{
  char *p3 = (char *)p1;
  char *ip2;
  ip2 = (char *)p2;
  memcpy((void *)ip2, (void *)pad, MEM_PAD_SIZE);
  memcpy((void *)(ip2 + len + MEM_PAD_SIZE), (void *)pad, MEM_PAD_SIZE);

  (*((int *)(p3 - 4))) = loc;
}

static void check_padding(void *p1, void *p2, size_t len, bool refill)
{
  int i;
  size_t j;
  char *ip2;
  if (!p2)
    {
      fprintf(stderr, "history pointer null??");
      describe_pointer(p1);
      fprintf(stderr,")\n");
      abort();
    }
  ip2 = (char *)p2;
  for (i = 0; i < MEM_PAD_SIZE - 4; i++) 
    if (ip2[i] != pad[i])
      {
	fprintf(stderr, "memory clobbered %d bytes before %p (", MEM_PAD_SIZE - i, p1);
	describe_pointer(p1);
	fprintf(stderr,")\n");
	abort();
      }
  if (refill)
    memset((void *)(ip2 + MEM_PAD_SIZE), 'X', len);
  for (i = 0, j = len + MEM_PAD_SIZE; i < MEM_PAD_SIZE; i++, j++) 
    if (ip2[j] != pad[i])
      {
	fprintf(stderr, "memory clobbered %d bytes after %p (", i, p1);
	describe_pointer(p1);
	fprintf(stderr,")\n");
	abort();
      }
}

static int last_loc = -1;
#define FREED_POINTER 0x99999999

void set_printable(int val)
{
  printable[last_loc] = val;
}

void check_pointer(void *ptr)
{
  int loc;
  char *p3 = (char *)ptr;
  void *rtp;

  if (ptr == NULL) return;
  if (ptr == (void *)FREED_POINTER) {fprintf(stderr," pointer has been freed"); abort();}

  loc = (*((int *)(p3 - 4)));
  if ((loc < 0) || (loc > mem_size)) {fprintf(stderr, "loc clobbered: %p %d (%d)\n", ptr, loc, mem_size); abort();}

  rtp = true_pointers[loc];
  check_padding(ptr, rtp, sizes[loc], false);
}

static int *freed = NULL;
static int freed_out = 0, freed_in = 0;

static void *forget_pointer(void *ptr, const char *func, const char *file, int line, bool refill)
{
  int loc;
  void *rtp;
  char *p3 = (char *)ptr;

  if (ptr == NULL) {fprintf(stderr, "attempt to free NULL"); mem_report(); abort();}
  if (ptr == (void *)FREED_POINTER) {fprintf(stderr," attempt to free pointer twice"); abort();}

  loc = (*((int *)(p3 - 4)));
  if ((loc < 0) || (loc > mem_size))
    {
      fprintf(stderr, "loc clobbered: %p %d (%d)\n", ptr, loc, mem_size);
      abort();
    }
  freed[freed_in++] = loc;
  if (freed_in >= mem_size) freed_in = 0;

  rtp = true_pointers[loc];
  check_padding(ptr, rtp, sizes[loc], refill);
  pointers[loc] = 0;
  true_pointers[loc] = 0;
  return(rtp);
}

static int remember_pointer(void *ptr, void *true_ptr, size_t len, const char *func, const char *file, int line)
{
  int i, loc, temp;
  if (mem_size == 0)
    {
      mem_size = 65536 * 128;
      pointers = (void **)calloc(mem_size, sizeof(void *));
      true_pointers = (void **)calloc(mem_size, sizeof(void *));
      sizes = (size_t *)calloc(mem_size, sizeof(size_t));
      locations = (int *)calloc(mem_size, sizeof(int));
      freed = (int *)calloc(mem_size, sizeof(int));
      for (i = 0; i < mem_size; i++) freed[i] = i;
    }

  loc = freed[freed_out++];
  if (freed_out >= mem_size) freed_out = 0;
  temp = freed_out - freed_in;
  if (temp == 0) fprintf(stderr, "ran out of space ");
  if (temp < 0) temp += mem_size;
  if (temp > max_mem_size) max_mem_size = temp;

  pointers[loc] = ptr;
  true_pointers[loc] = true_ptr;
  set_padding(ptr, true_ptr, len, loc);
  sizes[loc] = len;
  locations[loc] = find_mem_location(func, file, line);
  last_loc = locations[loc];
  return(loc);
}

#define MAX_MALLOC (1 << 30)

void *mem_calloc(size_t len, size_t size, const char *func, const char *file, int line)
{
  char *ptr, *true_ptr;
  if ((len == 0) || ((len * size) > MAX_MALLOC) || (size == 0))
    {
      fprintf(stderr, "%s:%s[%d] attempt to calloc %Zu bytes", func, file, line, len * size);
      mem_report(); 
      abort();
    }
  true_ptr = (char *)malloc(len * size + 2 * MEM_PAD_SIZE);
  if (!true_ptr)
    {
      fprintf(stderr, "can't calloc %Zu bytes!!", len * size + 2 * MEM_PAD_SIZE);
      mem_report();
      abort();
    }
  memset(true_ptr, 0, len * size + 2 * MEM_PAD_SIZE);
  ptr = (char *)(true_ptr + MEM_PAD_SIZE);
  if (ptr == NULL) {fprintf(stderr,"calloc->null"); abort();}
  remember_pointer((void *)ptr, (void *)true_ptr, len * size, func, file, line);
  return((void *)ptr);
}

void *mem_malloc(size_t len, const char *func, const char *file, int line)
{
  char *ptr, *true_ptr;
  if ((len == 0) || (len > MAX_MALLOC))
    {
      fprintf(stderr, "%s:%s[%d] attempt to malloc %Zu bytes", func, file, line, len);
      mem_report(); abort();
    }
  true_ptr = (char *)malloc(len + 2 * MEM_PAD_SIZE);
  if (!true_ptr)
    {
      fprintf(stderr, "can't malloc %Zu bytes!!", len + 2 * MEM_PAD_SIZE);
      mem_report();
      abort();
    }
  ptr = (char *)(true_ptr + MEM_PAD_SIZE);
  if (ptr == NULL) {fprintf(stderr,"malloc->null"); abort();}
  remember_pointer((void *)ptr, (void *)true_ptr, len, func, file, line);
  return((void *)ptr);
}

void *mem_free(void *ptr, const char *func, const char *file, int line)
{
  void *true_ptr;
  /* fprintf(stderr,"free %s %s[%d]: %p\n", func, file, line, ptr); */
  true_ptr = forget_pointer(ptr, func, file, line, true);
  free(true_ptr);
  return((void *)FREED_POINTER);
}

void *mem_realloc(void *ptr, size_t size, const char *func, const char *file, int line)
{
  char *new_ptr, *true_ptr, *new_true_ptr;
  if ((size == 0) || (size > MAX_MALLOC))
    {
      fprintf(stderr, "%s:%s[%d] attempt to realloc %Zu bytes", func, file, line, size);
      mem_report(); abort();
    }
  true_ptr = (char *)forget_pointer(ptr, func, file, line, false);
  new_true_ptr = (char *)realloc((void *)true_ptr, size + 2 * MEM_PAD_SIZE);
  new_ptr = (char *)(new_true_ptr + MEM_PAD_SIZE);
  if (new_ptr == NULL) {fprintf(stderr,"realloc->null"); abort();}
  remember_pointer((void *)new_ptr, (void *)new_true_ptr, size, func, file, line);
  return((void *)new_ptr);
}

static char *mem_stats(int ub)
{
  int i, k, ptrs = 0, snds = 0, chns = 0, trees = 0;
  size_t sum = 0;
  snd_info *sp;
  char *result, *ksum = NULL, *kptrs = NULL, *kpers = NULL;
  for (i = 0; i < mem_size; i++)
    if (pointers[i])
      {
	ptrs++;
	sum += sizes[i];
      }
  result = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if (sp)
	{
	  snds++;
	  chns += sp->allocated_chans;
	  for (k = 0; k < sp->allocated_chans; k++)
	    {
	      chan_info *cp;
	      cp = sp->chans[k];
	      if ((cp) && (cp->ptrees))
		{
		  int j;
		  trees++;
		  fprintf(stderr, "chan(%d %d): %d [", k, sp->nchans, cp->ptree_size);
		  for (j = 0; j < cp->ptree_size; j++)
		    if (cp->ptrees[j])
		      fprintf(stderr, "%p ", cp->ptrees[j]);
		  fprintf(stderr, "\n");
		}
	    }
	}
    }
  mus_snprintf(result, PRINT_BUFFER_SIZE, "snd mem: %s (%s ptrs, size: %d, max: %d), %d sounds, %d chans (%s, %d)\n",
	       ksum = kmg(sum),
	       kptrs = kmg(ptrs),
	       mem_size,
	       max_mem_size,
	       snds, chns,
	       (chns > 0) ? (kpers = kmg(ub / chns)) : "", trees);
  if (ksum) free(ksum);
  if (kptrs) free(kptrs);
  if (kpers) free(kpers);
  return(result);
}

void dump_protection(FILE *Fp);
void io_fds_in_use(int *open, int *closed, int *top);
void describe_region(FILE *fd, void *ur);
void describe_sync(FILE *fp, void *ptr);

typedef struct {int ptrs, loc, refsize; size_t sum; int *refs;} sumloc;

static int sloc_bigger(const void *a, const void *b)
{
  sumloc d1 = *(sumloc *)a;
  sumloc d2 = *(sumloc *)b;
  return(d1.sum < d2.sum);
}

void mem_report(void)
{
  int loc, i, j;
  sumloc *slocs;
  FILE *Fp;
  if (ss->search_tree)
    {
      free_ptree(ss->search_tree);
      ss->search_tree = NULL;
    }
  slocs = (sumloc *)calloc(mem_location + 1, sizeof(sumloc));

  for (i = 0; i < mem_size; i++)
    if (pointers[i])
      {
	loc = locations[i];
	if ((loc < 0) || (loc > mem_location))
	  fprintf(stderr, "locations[%d] == %d??", i, loc);
	else
	  {
	    slocs[loc].sum += sizes[i];
	    if (slocs[loc].refs == NULL)
	      {
		slocs[loc].refs = (int *)calloc(1, sizeof(int));
		slocs[loc].refsize = 1;
	      }
	    else
	      {
		if (slocs[loc].refsize == slocs[loc].ptrs)
		  {
		    slocs[loc].refsize *= 2;
		    slocs[loc].refs = (int *)realloc(slocs[loc].refs, sizeof(int) * slocs[loc].refsize);
		  }
	      }
	  }
	slocs[loc].refs[slocs[loc].ptrs] = i;
	slocs[loc].ptrs++;
	slocs[loc].loc = loc; /* save for sort */
      }

  Fp = fopen("memlog", "w");
  if (Fp == NULL) return;

  {
    char *str;
    str = mem_stats(0);
    fprintf(Fp, "memlog: %s: %s\n\n", snd_local_time(), str);
    free(str);
  }

  qsort((void *)slocs, mem_location + 1, sizeof(sumloc), sloc_bigger);

  for (i = 0; i <= mem_location; i++)
    {
      int ptrs;
      size_t sum;
      sum = slocs[i].sum;
      ptrs = slocs[i].ptrs;
      loc = slocs[i].loc;

      if (sum > 0)
	{
	  if ((loc < 0) || (loc > mem_location))
	    {
	      fprintf(Fp, "impossible loc %d:  %Zu (%d)\n", loc, sum, ptrs);
	    }
	  else
	    {
	      fprintf(Fp, "%s[%d]:%s:  %Zu (%d)\n", files[loc], lines[loc], functions[loc], sum, ptrs);
	      if (printable[loc] > 0)
		{
		  fprintf(Fp, "        ");
		  for (j = 0; j < ptrs; j++)
		    {
		      int orig_i;
		      orig_i = slocs[i].refs[j];
		      switch (printable[loc])
			{
			case PRINT_CHAR:
			  fprintf(Fp, "[%s] ", (char *)(pointers[orig_i]));
			  break;
			case PRINT_CLM:
			  /* don't call mus_describe here!  It can call either free or calloc, and thereby screw up everything else */
			  fprintf(Fp, "[%p: %s]\n   ", pointers[orig_i], mus_name((mus_any *)(pointers[orig_i])));
			  break;
			case PRINT_REGION:
			  fprintf(Fp, "[%p: ", pointers[orig_i]); describe_region(Fp, pointers[orig_i]); fprintf(Fp, "]\n  ");
			  break;
			case PRINT_SYNC:
			  fprintf(Fp, "[%p: ", pointers[orig_i]); describe_sync(Fp, pointers[orig_i]); fprintf(Fp, "]\n  ");
			  break;
			case PRINT_SND_FD:
			  {
			    snd_fd *sf = (snd_fd *)(pointers[orig_i]);
			    fprintf(Fp, "[%p, loc: %d, beg: " OFF_TD ", eof: %d, sp: %p]\n  ",
				    sf, sf->dangling_loc, sf->initial_samp, (int)(sf->at_eof), sf->local_sp);
			  }
			  break;
			case PRINT_FAM_INFO:
			  {
			    fam_info *fp = (fam_info *)(pointers[orig_i]);
			    fprintf(Fp, "[%p, %s, rp: %p, data: %p]\n  ", fp, fp->filename, fp->rp, fp->data);
			  }
			  break;
			case PRINT_SOUND_DATA:
			  {
			    sound_data *sd = (sound_data *)(pointers[orig_i]);
			    fprintf(Fp, "[%p: %d %d]\n  ", sd, sd->chans, sd->length);
			  }
			  break;
			}
		      /* other printable cases that would be nice: snd_info chn_info io_fd env_state ladspa sound_data mix|track */
		    }
		  fprintf(Fp, "\n\n");
		}
	    }
	}
    }
  {
    int open = 0, closed = 0, top = 0;
    io_fds_in_use(&open, &closed, &top);
    fprintf(Fp, "ios: open: %d, closed: %d, top: %d\n", open, closed, top);
  }
  for (i = 0; i < 512; i++)
    if (mus_file_fd_name(i))
      fprintf(Fp, "[%d]: %s\n", i, mus_file_fd_name(i));
  fprintf(Fp, "\n\n");
  save_listener_text(Fp);
  dump_protection(Fp);
  for (i = 0; i <= mem_location; i++)
    if ((slocs[i].refs) && (slocs[i].refsize > 0)) free(slocs[i].refs);
  free(slocs);
  fclose(Fp);
}

#endif

#if (MUS_DEBUGGING) && (HAVE_CLOCK)

#if HAVE_SYS_TIME_H
  #include <sys/time.h>
#endif

static clock_t start;
void start_timing(void) {start = clock();}
void stop_timing(void) {fprintf(stderr, "time: %d ",(int)((clock() - start) * 1000.0 / CLOCKS_PER_SEC));}

#endif


#if HAVE_SCHEME
#define S_file_to_string "file->string"
static XEN g_file_to_string(XEN name)
{ 
  char *contents;
  XEN val = XEN_FALSE;
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ONLY_ARG, S_file_to_string, "a string");
  contents = file_to_string(XEN_TO_C_STRING(name));
  val = C_TO_XEN_STRING(contents);
  FREE(contents);
  return(val);
}
#endif


#if MUS_DEBUGGING
static XEN g_mem_report(void) 
{
  mem_report(); 
  return(XEN_FALSE);
}
#endif


#ifdef XEN_ARGIFY_1
#if MUS_DEBUGGING
  XEN_NARGIFY_0(g_mem_report_w, g_mem_report)
#endif
#if HAVE_SCHEME
  XEN_NARGIFY_1(g_file_to_string_w, g_file_to_string)
#endif
#else
#if MUS_DEBUGGING
  #define g_mem_report_w g_mem_report
#endif
#if HAVE_SCHEME
  #define g_file_to_string_w g_file_to_string
#endif
#endif


void g_init_utils(void)
{
  decimal_pt = local_decimal_point();
#if HAVE_SCHEME
  XEN_DEFINE_PROCEDURE(S_file_to_string, g_file_to_string_w, 1, 0, 0, "file contents as string");
#endif

#if MUS_DEBUGGING
  XEN_DEFINE_PROCEDURE("mem-report",   g_mem_report_w, 0, 0, 0, "(mem-report) writes memory usage stats to memlog");
#endif
}
