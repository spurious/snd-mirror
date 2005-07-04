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
static int pow2s[POW2_SIZE] = {1, 2, 4, 8, 16, 32, 64, 128, 256, 
			       512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 
			       131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608, 16777216, 
			       33554432, 67108864, 134217728, 268435456, 536870912, 1073741824};
int snd_ipow2(int n)
{
  return(pow2s[n]);
}

int snd_2pow2(int n)
{
  /* round up to next power of 2 */
  int i;
  for (i = 0; i < POW2_SIZE; i++)
    if (pow2s[i] >= n)
      return(pow2s[i]);
  return(0);
}

int snd_ilog2(int n)
{
  /* round down */
  int i;
  for (i = 1; i < POW2_SIZE; i++)
    if (pow2s[i] > n)
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

Float in_dB(Float min_dB, Float lin_dB, Float py)
{
  return((py <= lin_dB) ? min_dB : (20.0 * log10(py)));
}

char *copy_string(const char *str)
{
#if DEBUGGING
  char *newstr = NULL;
  if (str)
    {
      newstr = (char *)MALLOC((strlen(str) + 1) * sizeof(char));
      strcpy(newstr, str);
    }
  return(newstr);
#else
  if (str)
    return(strdup(str));
  return(NULL);
#endif
}

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

char *filename_without_home_directory(const char *name)
{
  /* since we don't want to mess with freeing these guys, I'll just return a pointer into the name */
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
  nodir = copy_string(filename_without_home_directory(name));
  len = strlen(nodir);
  for (i = 0; i < len; i++) 
    if (nodir[i] == '.') 
      {
	nodir[i] = '\0'; 
	break;
      }
  return(nodir);
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
      rewind(file);
      content = (char *)CALLOC(size + 1, sizeof(char));
      fread(content, sizeof(char), size, file);
    }
  fclose(file);
  return(content);
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

static char prtbuf[256];

char *prettyf(Float num, int tens)
{ /* try to prettify float display -- if tens < 0, return int */
  int fullf, len, i, lim;
  Float rounder;
  char *newval, *sp, *sn, *zp;
  zp = NULL;
  if (tens > 9) tens = 9;
  if (num < 0.0) rounder = -.49; else rounder = .49;
  if (tens < 0)
    {
      fullf = (int)(num + rounder);
      sprintf(prtbuf, "%d", fullf);
      return(copy_string(prtbuf));
    }
  fullf = (int)num;
  if ((num - fullf) == 0.0) 
    {
      if (num < 100.0)
	sprintf(prtbuf, "%d%c0", fullf, decimal_pt);
      else sprintf(prtbuf, "%d", fullf);
      return(copy_string(prtbuf));
    }
  if (num > 1000)
    {
      sprintf(prtbuf, "%d%c%d", fullf, decimal_pt, (int)((num - fullf) * pow(10.0, tens)));
      return(copy_string(prtbuf));
    }
  fullf = (int)(num * pow(10.0, tens + 1) + rounder);
  if (fullf == 0) 
    {
      /* will be freed later, so can't return a constant */
      newval = (char *)MALLOC(2 * sizeof(char));
      newval[0] = '0';
      newval[1] = '\0';
      return(newval);
    }
  sprintf(prtbuf, "%d", fullf);
  len = strlen(prtbuf);
  newval = (char *)CALLOC(len + tens + 10, sizeof(char));
  sn = newval;
  sp = prtbuf;
  if ((*sp) == '-') 
    {
      (*sn) = (*sp); 
      sn++; 
      sp++; 
      len--;
    }
  if (len >= (tens + 1))
    {
      for (i = 0; i < len - tens - 1; i++) 
	{
	  (*sn) = (*sp); 
	  sn++; 
	  sp++;
	}
      (*sn) = decimal_pt; 
      sn++;
    }
  else
    {
      (*sn) = '0';
      sn++;
      (*sn) = decimal_pt;
      sn++;
      lim = abs(len - tens - 1);
      for (i = 0; i < lim; i++) 
	{
	  (*sn) = '0';
	  sn++;
	}
    }
  if (tens > 5) tens = 5;
  for (i = 0; i <= tens; i++) 
    {
      if (!(*sp)) break;
      (*sn) = (*sp); 
      if ((!zp) || ((*sn) != '0')) zp = sn;
      sn++; 
      sp++;
    }
  if (zp) sn = zp; else (*sn) = '0';
  sn++;
  (*sn) = '\0';
  return(newval);
}

static char *get_tmpdir(void)
{
  char *tmpdir = NULL;
  int len;
  tmpdir = copy_string(getenv("TMPDIR"));
  if ((tmpdir == NULL) && (DEFAULT_TEMP_DIR)) tmpdir = copy_string(DEFAULT_TEMP_DIR);
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
#define DEBUGGING_FAM 0

static bool fam_already_warned = false;

#if USE_MOTIF
static void fam_reader(XtPointer context, int *fd, XtInputId *id)
#else
static gboolean fam_reader(GIOChannel *source, GIOCondition condition, gpointer data)
#endif
{
  FAMEvent fe; 
  if (FAMPending(ss->fam_connection) > 0)
    {
      if (FAMNextEvent(ss->fam_connection, &fe) < 0) 
	fprintf(stderr,"fam error: %d\n", FAMErrno);
      else
	{
	  snd_info *sp;
	  sp = (snd_info *)(fe.userdata);
#if DEBUGGING_FAM
	  fprintf(stderr, "fam %s: %d\n", fe.filename, fe.code);
#endif
	  switch (fe.code)
	    {
	    case FAMChanged:
	      /* TODO: check what changes might matter here (write-protection -> put up lock?) */
#if DEBUGGING_FAM
	      fprintf(stderr, "%s changed\n", fe.filename);
#endif	  
	      break;

	    case FAMDeleted:
	    case FAMCreated:
	    case FAMMoved:
#if DEBUGGING_FAM
	      fprintf(stderr, "%s moved etc\n", fe.filename);
#endif	  
	      if (!(sp->writing))
		{
		  sp->need_update = true;
		  if (auto_update(ss))
		    snd_update(sp);
		  else snd_file_bomb_icon(sp, true);
		}
	      break;

	    case FAMStartExecuting:
	    case FAMStopExecuting:
	    case FAMAcknowledge:
	    case FAMExists:
	    case FAMEndExist:
	      break;
	      /* these don't matter */
	    default:
#if DEBUGGING_FAM
	      fprintf(stderr, "%s: unknown fam code: %d!\n", fe.filename, fe.code);
#endif
	      break;
	    }
	}
    }
#if USE_GTK
  return(true);
#endif
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
	      snd_error("can't start file alteration monitor: %d\n", FAMErrno);
	      return(NULL);
	    }
	  else
	    {
	      int fd;
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

FAMRequest *fam_monitor_file(const char *filename, void *data)
{
  FAMRequest *rp;
  int err;
  rp = fam_monitor();
  if (rp)
    {
#if DEBUGGING_FAM
      fprintf(stderr, "monitor %s\n", filename);
#endif
      err = FAMMonitorFile(ss->fam_connection, filename, rp, data);
      if (err < 0)
	{
	  snd_error("can't monitor %s: %d\n", filename, FAMErrno);
	  FREE(rp);
	  return(NULL);
	}
    }
  else 
    {
      if (!fam_already_warned)
	snd_error("can't get fam request for %s: %d\n", filename, FAMErrno);
    }
  return(rp);
}

FAMRequest *fam_monitor_directory(const char *dir_name, void *data)
{
  FAMRequest *rp;
  int err;
  rp = fam_monitor();
  if (rp)
    {
      err = FAMMonitorDirectory(ss->fam_connection, dir_name, rp, data);
      if (err < 0)
	{
	  snd_error("can't monitor %s: %d\n", dir_name, FAMErrno);
	  FREE(rp);
	  return(NULL);
	}
    }
  else 
    {
      if (!fam_already_warned)
	snd_error("can't get fam request for %s: %d\n", dir_name, FAMErrno);
    }
  return(rp);
}

FAMRequest *fam_unmonitor_file(const char *filename, FAMRequest *rp)
{
  int err;
#if DEBUGGING_FAM
  fprintf(stderr, "unmonitor %s\n", filename);
#endif
  if (rp)
    {
      err = FAMCancelMonitor(ss->fam_connection, rp);
      if (err < 0)
	snd_error("can't unmonitor %s: %d\n", filename, FAMErrno);
      FREE(rp);
    }
  return(NULL);
}
#endif


#if DEBUGGING

/* mtrace-style malloc hooks are not very useful here since I don't care
 * about X allocations (of which there are millions), and I need readable
 * backtrace info for leaks.  Doing it by hand makes it easy to sort
 * output and whatnot. All of Sndlib and Snd use the macros CALLOC,
 * MALLOC, REALLOC, and FREE.
 */

static char *kmg (int num)
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
  else mus_snprintf(str, LABEL_BUFFER_SIZE, "%d", num);
  return(str);
}

#define MEM_PAD_SIZE 32
static int mem_size = 0, mem_top = -1;
static int *pointers = NULL, *true_pointers = NULL, *sizes = NULL, *locations = NULL;
static char **functions = NULL, **files = NULL;
static int *lines = NULL;
static int mem_location = -1;
static int mem_locations = 0;
static char **stacks = NULL;

static char *encloser = NULL;
void set_encloser(char *name) 
{
  if (name) encloser = name;
  else encloser = NULL;
}

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
	  lines = (int *)calloc(2048, sizeof(int));

	  lines_hit = (int *)calloc(65536, sizeof(int));

	  mem_locations = 2048;
	}
      else
	{
	  functions = (char **)realloc(functions, (mem_locations + 1024) * sizeof(char *));
	  files = (char **)realloc(files, (mem_locations + 1024) * sizeof(char *));
	  lines = (int *)realloc(lines, (mem_location + 1024) * sizeof(int));
	  for (i = 0; i < 1024; i++) 
	    {
	      functions[i + mem_locations] = NULL;
	      files[i + mem_locations] = NULL;
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

static void describe_pointer(void *p)
{
  int i, loc;
  for (i = mem_top; i >= 0; i--)
    if (pointers[i] == (int)p)
      {
	loc = locations[i];
	fprintf(stderr, "%s[%d]:%s, len:%d", files[loc], lines[loc], functions[loc], sizes[loc]);
	return;
      }
  fprintf(stderr, "pointer lost??");
}

static char pad[MEM_PAD_SIZE] = {'W','I','L','L','I','A','M',' ','G','A','R','D','N','E','R',' ','S','C','H','O','T','T','S','T','A','E','D','T',' ','D','M','A'};
static void set_padding(void *p1, void *p2, int len)
{
  char *ip2;
  ip2 = (char *)p2;
  memcpy((void *)ip2, (void *)pad, MEM_PAD_SIZE);
  memcpy((void *)(ip2 + len + MEM_PAD_SIZE), (void *)pad, MEM_PAD_SIZE);
}

static void check_padding(void *p1, void *p2, int len, bool refill)
{
  int i, j;
  char *ip2;
  ip2 = (char *)p2;
  for (i = 0; i < MEM_PAD_SIZE; i++) 
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

#define PTR_HISTORY_SIZE 128
static int ptr_history[PTR_HISTORY_SIZE], ptr_history_loc[PTR_HISTORY_SIZE];
static int ptr_history_ptr = 0;

void mem_report(void);
static int last_forgotten = -1;
#define FREED_POINTER 0x99999999

static void *forget_pointer(void *ptr, const char *func, const char *file, int line, bool refill)
{
  int i;
  void *rtp;
  if (ptr == NULL) {fprintf(stderr, "attempt to free NULL"); mem_report(); abort();}
  if (ptr == (void *)FREED_POINTER) {fprintf(stderr," attempt to free pointer twice"); abort();}
  for (i = 0; i < PTR_HISTORY_SIZE; i++)
    if (ptr_history[i] == (int)ptr)
      {
	int last_remembered_ptr, last_remembered;
	last_remembered_ptr = ptr_history[i];
	last_remembered = ptr_history_loc[i];
	ptr_history[i] = 0;
	ptr_history_loc[i] = -1;
	rtp = (void *)true_pointers[last_remembered];
	check_padding(ptr, rtp, sizes[last_remembered], refill);
	pointers[last_remembered] = 0;
	true_pointers[last_remembered] = 0;
	stacks[last_remembered] = NULL;
	last_forgotten = last_remembered;
	return(rtp);
      }
  for (i = mem_top; i >= 0; i--)
    if (pointers[i] == (int)ptr)
      {
	rtp = (void *)true_pointers[i];
	check_padding(ptr, rtp, sizes[i], refill);
	pointers[i] = 0;
	true_pointers[i] = 0;
	stacks[i] = NULL;
	last_forgotten = i;
	while ((mem_top >= 0) && (pointers[mem_top] == 0)) mem_top--;
	if (mem_top < last_forgotten) last_forgotten = mem_top + 1;
	return(rtp);
      }
  fprintf(stderr, "forget %p ", ptr); mem_report(); abort();
  return(NULL);
}

static void remember_pointer(void *ptr, void *true_ptr, size_t len, const char *func, const char *file, int line)
{
  int i, loc = 0;
  if (last_forgotten == -1)
    {
      if (mem_size == 0)
	{
	  mem_size = 8192;
	  pointers = (int *)calloc(mem_size, sizeof(int));
	  true_pointers = (int *)calloc(mem_size, sizeof(int));
	  sizes = (int *)calloc(mem_size, sizeof(int));
	  locations = (int *)calloc(mem_size, sizeof(int));
	  stacks = (char **)calloc(mem_size, sizeof(char *));
	  loc = 0;
	  goto GOT_ONE;
	}
      for (i = 0; i < mem_size; i++)
	if (pointers[i] == 0) 
	  {
	    loc = i;
	    goto GOT_ONE;
	  }
      loc = mem_size;
      mem_size += 4096;
      pointers = (int *)realloc(pointers, mem_size * sizeof(int));
      true_pointers = (int *)realloc(true_pointers, mem_size * sizeof(int));
      sizes = (int *)realloc(sizes, mem_size * sizeof(int));
      locations = (int *)realloc(locations, mem_size * sizeof(int));
      stacks = (char **)realloc(stacks, mem_size * sizeof(char *));
      for (i = loc; i < mem_size; i++)
	{
	  pointers[i] = 0;
	  true_pointers[i] = 0;
	  sizes[i] = 0;
	  locations[i] = 0;
	  stacks[i] = NULL;
	}
    }
  else
    {
      loc = last_forgotten;
      last_forgotten = -1;
    }
 GOT_ONE:
  pointers[loc] = (int)ptr;
  true_pointers[loc] = (int)true_ptr;
  set_padding(ptr, true_ptr, (int)len);
  sizes[loc] = (int)len;
  locations[loc] = find_mem_location(func, file, line);
  if (encloser) stacks[loc] = encloser;
  ptr_history[ptr_history_ptr] = (int)ptr;
  ptr_history_loc[ptr_history_ptr++] = loc;
  if (ptr_history_ptr >= PTR_HISTORY_SIZE) ptr_history_ptr = 0;
  if (mem_top < loc) mem_top = loc;
}

#define MAX_MALLOC (1 << 28)

void *mem_calloc(int len, int size, const char *func, const char *file, int line)
{
  char *ptr, *true_ptr;
  if ((len <= 0) || ((len * size) > MAX_MALLOC) || (size <= 0))
    {
      fprintf(stderr, "%s:%s[%d] attempt to calloc %d bytes", func, file, line, len * size);
      mem_report(); abort();
    }
  true_ptr = (char *)malloc(len * size + 2 * MEM_PAD_SIZE);
  memset(true_ptr, 0, len * size + 2 * MEM_PAD_SIZE);
  ptr = (char *)(true_ptr + MEM_PAD_SIZE);
  if (ptr == NULL) {fprintf(stderr,"calloc->null"); abort();}
  remember_pointer((void *)ptr, (void *)true_ptr, len * size, func, file, line);
  return((void *)ptr);
}

void *mem_malloc(int len, const char *func, const char *file, int line)
{
  char *ptr, *true_ptr;
  if ((len <= 0) || (len > MAX_MALLOC))
    {
      fprintf(stderr, "%s:%s[%d] attempt to malloc %d bytes", func, file, line, len);
      mem_report(); abort();
    }
  true_ptr = (char *)malloc(len + 2 * MEM_PAD_SIZE);
  ptr = (char *)(true_ptr + MEM_PAD_SIZE);
  if (ptr == NULL) {fprintf(stderr,"malloc->null"); abort();}
  remember_pointer((void *)ptr, (void *)true_ptr, len, func, file, line);
  return((void *)ptr);
}

void mem_free(void *ptr, const char *func, const char *file, int line)
{
  void *true_ptr;
  /* fprintf(stderr,"free %s %s[%d]: %p\n", func, file, line, ptr); */
  true_ptr = forget_pointer(ptr, func, file, line, true);
  free(true_ptr);
  ptr = (void *)FREED_POINTER;
}

void *mem_realloc(void *ptr, int size, const char *func, const char *file, int line)
{
  char *new_ptr, *true_ptr, *new_true_ptr;
  if ((size <= 0) || (size > MAX_MALLOC))
    {
      fprintf(stderr, "%s:%s[%d] attempt to realloc %d bytes", func, file, line, size);
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
  int i, ptrs = 0, sum = 0, snds = 0, chns = 0;
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
	}
    }
  mus_snprintf(result, PRINT_BUFFER_SIZE, "snd mem: %s (%s ptrs), %d sounds, %d chans (%s)\n",
	  ksum = kmg(sum),
	  kptrs = kmg(ptrs),
	  snds, chns,
	  (chns > 0) ? (kpers = kmg(ub / chns)) : "");
  if (ksum) free(ksum);
  if (kptrs) free(kptrs);
  if (kpers) free(kpers);
  return(result);
}


/* file bookkeepping */
static char **open_files = NULL, **file_funcs = NULL, **file_files = NULL;
static int *file_lines = NULL, *file_fds = NULL;
static FILE **file_fls = NULL;
static int file_size = 0;

static void io_add(const char *pathname, const char *func, const char *file, int line, int fd, FILE *fl)
{
  int i, loc = -1;
  if (file_size == 0)
    {
      file_size = 128;
      open_files = (char **)calloc(file_size, sizeof(char *));
      file_funcs = (char **)calloc(file_size, sizeof(char *));
      file_files = (char **)calloc(file_size, sizeof(char *));
      file_lines = (int *)calloc(file_size, sizeof(int));
      file_fds = (int *)calloc(file_size, sizeof(int));
      for (i = 0; i < file_size; i++) file_fds[i] = -1;
      file_fls = (FILE **)calloc(file_size, sizeof(FILE *));
      open_files[0] = copy_string(pathname);
      file_funcs[0] = copy_string(func);
      file_files[0] = copy_string(file);
      file_lines[0] = line;
      file_fds[0] = fd;
      file_fls[0] = fl;
    }
  else
    {
      for (i = 0; i < file_size; i++)
	if (open_files[i] == NULL)
	  {
	    loc = i;
	    break;
	  }
      if (loc == -1)
	{
	  loc = file_size;
	  file_size += 128;
	  open_files = (char **)realloc(open_files, file_size * sizeof(char *));
	  file_funcs = (char **)realloc(file_funcs, file_size * sizeof(char *));
	  file_files = (char **)realloc(file_files, file_size * sizeof(char *));
	  file_lines = (int *)realloc(file_lines, file_size * sizeof(int));
	  file_fds = (int *)realloc(file_fds, file_size * sizeof(int));
	  for (i = loc; i < file_size; i++) 
	    {
	      file_fds[i] = -1;
	      open_files[i] = NULL;
	      file_files[i] = NULL;
	      file_funcs[i] = NULL;
	    }
	  file_fls = (FILE **)realloc(file_fls, file_size * sizeof(FILE *));
	}
      open_files[loc] = copy_string(pathname);
      file_funcs[loc] = copy_string(func);
      file_files[loc] = copy_string(file);
      file_lines[loc] = line;
      file_fds[loc] = fd;
      file_fls[loc] = fl;
    }
}

static void io_subtract(int fd, FILE *fl, const char *func, const char *file, int line)
{
  int i;
  for (i = 0; i < file_size; i++)
    if (((fd >= 0) && (file_fds[i] == fd)) ||
	((fl) && (file_fls[i] == fl)))
      {
	if (((file_fls[i]) && (fd >= 0)) ||
	    ((fl) && (file_fds[i] >= 0)))
	  fprintf(stderr,"%s: %s[%d] (%s) %d %p?\n", file_files[i], file_funcs[i], file_lines[i], open_files[i], file_fds[i], file_fls[i]);
	FREE(file_files[i]); file_files[i] = NULL;
	FREE(file_funcs[i]); file_funcs[i] = NULL;
	FREE(open_files[i]); open_files[i] = NULL;
	file_fds[i] = -1;
	file_fls[i] = NULL;
	return;
      }
  fprintf(stderr, "%s: %s[%d] can't find %d %p?\n", file, func, line, fd, fl);
}

int io_open(const char *pathname, int flags, mode_t mode, const char *func, const char *file, int line)
{
  int fd;
  fd = snd_io_open(pathname, flags, mode);
  if (fd != -1) io_add(pathname, func, file, line, fd, NULL);
  return(fd);
}

int io_creat(const char *pathname, mode_t mode, const char *func, const char *file, int line)
{
  int fd;
  fd = snd_io_creat(pathname, mode);
  if (fd != -1) io_add(pathname, func, file, line, fd, NULL);
  return(fd);
}

int io_close(int fd, const char *func, const char *file, int line)
{
  io_subtract(fd, NULL, func, file, line);
  return(snd_io_close(fd));
}

FILE *io_fopen(const char *path, const char *mode, const char *func, const char *file, int line)
{
  FILE *fp;
  fp = snd_io_fopen(path, mode);
  if (fp) io_add(path, func, file, line, -1, fp);
  return(fp);
}

int io_fclose(FILE *stream, const char *func, const char *file, int line)
{
  io_subtract(-1, stream, func, file, line);
  return(snd_io_fclose(stream));
}

void dump_protection(FILE *Fp);

void mem_report(void)
{
  int loc, i, j, sum, ptr = 0;
  bool have_stacks = false;
  int *sums, *ptrs;
  FILE *Fp;
  time_t ts;
  char time_buf[TIME_STR_SIZE];
  if (ss->search_tree)
    {
      free_ptree(ss->search_tree);
      ss->search_tree = NULL;
    }
  for (i = 0; i < mem_size; i++)
    if (stacks[i])
      {
	have_stacks = true;
	break;
      }
  sums = (int *)calloc(mem_location + 1, sizeof(int));
  ptrs = (int *)calloc(mem_location + 1, sizeof(int));
  for (loc = 0; loc <= mem_location; loc++)
    {
      sum = 0;
      ptr = 0;
      for (i = 0; i < mem_size; i++)
	if ((pointers[i]) && (locations[i] == loc))
	  {
	    sum += sizes[i];
	    ptr++;
	  }
      sums[loc] = sum;
      ptrs[loc] = ptr;
    }
  Fp = fopen("memlog", "w");
  if (Fp == NULL) return;

  time(&ts);
  strftime(time_buf, TIME_STR_SIZE, STRFTIME_FORMAT, localtime(&ts));
  {
    char *str;
    str = mem_stats(0);
    fprintf(Fp, "memlog: %s: %s\n\n", time_buf, str);
    free(str);
  }

  for (i = 0; i <= mem_location; i++)
    {
      sum = 0;
      for (loc = 0; loc <= mem_location; loc++)
	if (sums[loc] > sum)
	  {
	    ptr = loc;
	    sum = sums[loc];
	  }
      if (sum > 0)
	{
	  fprintf(Fp, "%s[%d]:%s:  %d (%d)\n", files[ptr], lines[ptr], functions[ptr], sums[ptr], ptrs[ptr]);
	  sums[ptr] = 0;
	  if (have_stacks)
	    for (j = 0; j < mem_size; j++)
	      if ((stacks[j]) && (locations[j] == ptr) && (pointers[j]))
		fprintf(Fp, "    %s    %p\n", stacks[j], (void *)(pointers[j]));
	  if ((strcmp("mus_format", functions[ptr]) == 0) ||
	      (strcmp("copy_string", functions[ptr]) == 0))
	    {
	      if (have_stacks)
		fprintf(Fp, "                          ");
	      for (j = 0; j < mem_size; j++)
		if ((locations[j] == ptr) && (pointers[j]))
		  fprintf(Fp, "[%s] ", (char *)(pointers[j]));
	      fprintf(Fp, "\n");
	    }
	}
    }
  for (i = 0; i < 512; i++)
    if (mus_file_fd_name(i))
      fprintf(Fp, "[%d]: %s\n", i, mus_file_fd_name(i));
  fprintf(Fp, "\n\nprevlist: %d %d\n", get_prevfile_end(), get_max_prevfile_end());
  fprintf(Fp, "\n\n");
  save_listener_text(Fp);
  for (i = 0; i < file_size; i++)
    if (open_files[i])
      fprintf(Fp, "%s: %s[%d] (%s) %d %p?\n", file_files[i], file_funcs[i], file_lines[i], open_files[i], file_fds[i], file_fls[i]);
  dump_protection(Fp);
  free(sums);
  free(ptrs);
  fclose(Fp);
}

#endif

#if (DEBUGGING) && (HAVE_CLOCK)

#if HAVE_SYS_TIME_H
  #include <sys/time.h>
#endif

static clock_t start;
void start_timing(void) {start = clock();}
void stop_timing(void) {fprintf(stderr, "time: %d ",(int)((clock() - start) * 1000.0 / CLOCKS_PER_SEC));}

#endif

#if HAVE_GUILE
#define S_file2string "file->string"
static XEN g_file_to_string(XEN name)
{ 
  char *contents;
  XEN val = XEN_FALSE;
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ONLY_ARG, S_file2string, "a string");
  contents = file_to_string(XEN_TO_C_STRING(name));
  val = C_TO_XEN_STRING(contents);
  FREE(contents);
  return(val);
}
#endif

#if DEBUGGING
static XEN g_mem_report(void) 
{
  mem_report(); 
  return(XEN_FALSE);
}
#endif

#if DEBUGGING
#ifdef XEN_ARGIFY_1
  XEN_NARGIFY_0(g_mem_report_w, g_mem_report)
#else
  #define g_mem_report_w g_mem_report
#endif
#endif

void g_init_utils(void)
{
  decimal_pt = local_decimal_point();
#if HAVE_GUILE
  XEN_DEFINE_PROCEDURE(S_file2string, g_file_to_string, 1, 0, 0, "file contents as string");
#endif

#if DEBUGGING
  XEN_DEFINE_PROCEDURE("mem-report",   g_mem_report_w, 0, 0, 0, "(mem-report) writes memory usage stats to memlog");
#endif
}
