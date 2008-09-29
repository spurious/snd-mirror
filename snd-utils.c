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


off_t snd_off_t_pow2(int n)
{
  if ((n < 0) || (n > 46)) return(0);
  if (n < POW2_SIZE)
    return((off_t)ipow2s[n]);
  return((off_t)ipow2s[16] * (off_t)ipow2s[n - 16]);  /* can't store an array as above -- compiler complaints */
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


char *copy_string(const char *str)
{
  if (str)
    return(strdup(str));
  return(NULL);
}


int snd_strlen(const char *str)
{
  /* strlen(NULL) -> seg fault! */
  if ((str) && (*str)) return(strlen(str));
  return(0);
}


bool snd_strcmp(const char *str1, const char *str2)
{
  return((str1 == str2) ||
	 ((str1) && (str2) &&
	  (strcmp(str1, str2) == 0)));
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
      else err_size = new_len * 2;
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


int snd_mkdir(const char *filename)
{
#ifdef __MINGW32__ 
  return(mkdir(filename));
#else 
  return(mkdir(filename, 0777));
#endif 
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

#if MUS_DEBUGGING_FAM
static char *fam_event_name(int code)
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
#endif


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


fam_info *fam_monitor_file(const char *filename, void *data, void (*action)(struct fam_info *fp, FAMEvent *fe))
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


fam_info *fam_monitor_directory(const char *dir_name, void *data, void (*action)(struct fam_info *fp, FAMEvent *fe))
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

fam_info *fam_monitor_file(const char *filename, void *data, void (*action)(struct fam_info *fp, FAMEvent *fe))
{
  return(NULL);
}


fam_info *fam_monitor_directory(const char *dir_name, void *data, void (*action)(struct fam_info *fp, FAMEvent *fe))
{
  return(NULL);
}


fam_info *fam_unmonitor_file(const char *filename, fam_info *fp)
{
  if (fp) free(fp);
  return(NULL);
}

#if MUS_DEBUGGING_FAM
static char *fam_event_name(int code)
{
  return("no fam!");
}
#endif
#endif


fam_info *fam_unmonitor_directory(const char *filename, fam_info *fp)
{
  return(fam_unmonitor_file(filename, fp));
}


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


#ifdef XEN_ARGIFY_1
#if HAVE_SCHEME
  XEN_NARGIFY_1(g_file_to_string_w, g_file_to_string)
#endif
#else
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
}
