#include "snd.h"

int snd_round(Float x)
{
  int i;
  i = (int)x;
  if ((x - i) > 0.5) return(i + 1);
  return(i);
}

char *copy_string(const char *str)
{
#if DEBUG_MEMORY || (!HAVE_STRDUP)
  char *newstr = NULL;
  if (str)
    {
      newstr = (char *)MALLOC((strlen(str) + 1) * sizeof(char));
      strcpy(newstr, str);
    }
  return(newstr);
#else
  return(snd_strdup(str));
#endif
}

char *snd_strdup(const char *str)
{
#if HAVE_STRDUP
  if (str) return(strdup(str));
  return(NULL);
#else
  return(copy_string(str));
#endif
}


int snd_strlen(char *str)
{
  /* strlen(NULL) -> seg fault! */
  if ((str) && (*str)) return(strlen(str));
  return(0);
}

char *filename_without_home_directory(char *name)
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
	sprintf(prtbuf, "%d%c0", fullf, STR_decimal);
      else sprintf(prtbuf, "%d", fullf);
      return(copy_string(prtbuf));
    }
  if (num > 1000)
    {
      sprintf(prtbuf, "%d%c%d", fullf, STR_decimal, (int)((num - fullf) * pow(10.0, tens)));
      return(copy_string(prtbuf));
    }
  fullf = (int)(num * pow(10.0, tens + 1) + rounder);
  if (fullf == 0) 
    {
      /* will be freed later, so can't return a constant */
      newval=(char *)MALLOC(2 * sizeof(char));
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
	  (*sn) =(*sp); 
	  sn++; 
	  sp++;
	}
      (*sn) = STR_decimal; 
      sn++;
    }
  else
    {
      (*sn) = '0';
      sn++;
      (*sn) = STR_decimal;
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

void fill_number(char *fs, char *ps)
{
  int i, j;
  j = snd_strlen(fs);
  if (j > 4) j = 4;
  if (j < 4) 
    {
      ps[4] = '\0'; 
      ps[3] = '0'; 
      ps[2] = '0'; 
      ps[1] = STR_decimal;
    }
  if ((*fs) == STR_decimal) 
    {
      *ps++ = '0'; 
      if (j == 4) j = 3;
    }
  for (i = 0; i < j; i++) 
    (*ps++) = (*fs++);
}

static char *get_tmpdir(void)
{
  char *tmpdir = NULL;
  int len;
  tmpdir = getenv("TMPDIR");
#ifdef CCRMA
  if (tmpdir == NULL) tmpdir = "/zap";
#else
  #ifdef P_tmpdir
    if (tmpdir == NULL) tmpdir = P_tmpdir; /* /usr/include/stdio.h */
  #else
    if (tmpdir == NULL) tmpdir = "/tmp";
  #endif
#endif
  if (tmpdir == NULL) tmpdir = ".";
  len = strlen(tmpdir);
  if (tmpdir[len - 1] == '/') tmpdir[len - 1] = 0; /* sgi... */
  return(tmpdir);
}

static int sect_ctr = 0;
char *shorter_tempnam(char *udir, char *prefix)
{
  /* tempnam turns out names that are inconveniently long (in this case the filename is user-visible) */
  char *str;
  str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  if ((udir == NULL) || (snd_strlen(udir) == 0)) udir = get_tmpdir(); /* incoming dir could be "" */
  mus_snprintf(str, PRINT_BUFFER_SIZE, "%s/%s%d_%d.snd", udir, (prefix) ? prefix : "snd_", getpid(), sect_ctr++);
  return(str);
}

char *snd_tempnam(snd_state *ss)
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

char *kmg (int num)
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

#ifdef DEBUG_MEMORY

/* mtrace-style malloc hooks are not very useful here since I don't care
 * about X allocations (of which there are millions), and I need readable
 * backtrace info for leaks.  Doing it by hand makes it easy to sort
 * output and whatnot. All of Sndlib and Snd use the macros CALLOC,
 * MALLOC, REALLOC, and FREE.
 */

static int mem_size = 0;

#if 0
/* from glibc/debug/backtrace-tst.c -- doesn't decode local (Snd) function names) */
/*   so, to get this to work, we need to build Snd as a shared object, then load it */
#include <execinfo.h>
#include <inttypes.h>
void show_stack(void);
void show_stack(void)
{
  void *ba[20];
  int n = backtrace (ba, sizeof (ba) / sizeof (ba[0]));
  if (n != 0)
    {
      char **names = backtrace_symbols (ba, n);
      if (names != NULL)
	{
	  int i;
	  printf ("called from %s\n", names[0]);
	  for (i = 1; i < n; ++i)
	    printf ("            %s\n", names[i]);
	  free (names);
	}
    }
}
#endif

static const char *encloser = NULL;
void set_encloser(const char *name) 
{
  encloser = name;
} /* for exposing call chains */

static int *pointers = NULL, *sizes = NULL, *locations = NULL;
static char **functions = NULL, **files = NULL;
static int *lines = NULL;
static int mem_location = -1;
static int mem_locations = 0;
static int last_mem_location = -1;

static int find_mem_location(const char *ur_func, const char *file, int line)
{
  int i;
  char *func = NULL;
  if (encloser)
    {
      func = (char *)calloc(strlen(encloser) + strlen(ur_func) + 4, sizeof(char));
      sprintf(func, "%s->%s", encloser, ur_func);
    }
  else func = (char *)ur_func;
  if ((last_mem_location >= 0) &&
      (line == lines[last_mem_location]) &&
      (strcmp(func, functions[last_mem_location]) == 0) &&
      (strcmp(file, files[last_mem_location]) == 0))
    return(last_mem_location);
  for (i = 0; i <= mem_location; i++)
    if ((line == lines[i]) &&
	(strcmp(func, functions[i]) == 0) &&
	(strcmp(file, files[i]) == 0))
      return(i);
  mem_location++;
  if (mem_location >= mem_locations)
    {
      if (mem_locations == 0)
	{
	  functions = (char **)calloc(1024, sizeof(char *));
	  files = (char **)calloc(1024, sizeof(char *));
	  lines = (int *)calloc(1024, sizeof(int));
	  mem_locations = 1024;
	}
      else
	{
	  functions = (char **)realloc(functions, (mem_locations + 1024)*sizeof(char *));
	  files = (char **)realloc(files, (mem_locations + 1024)*sizeof(char *));
	  lines = (int *)realloc(lines, (mem_location + 1024)*sizeof(int));
	  for (i = 0; i < 1024; i++) 
	    {
	      functions[i+mem_locations] = NULL;
	      files[i+mem_locations] = NULL;
	      lines[i+mem_locations] = 0;
	    }
	  mem_locations += 1024;
	}
    }
  /* NOT copy_string HERE!! */
  last_mem_location = mem_location;
  functions[mem_location] = (char *)calloc(strlen(func) + 1, sizeof(char));
  strcpy(functions[mem_location], func);
  files[mem_location] = (char *)calloc(strlen(file) + 1, sizeof(char));
  strcpy(files[mem_location], file);
  lines[mem_location] = line;
  return(mem_location);
}

void mem_report(void);
static int last_forgotten = -1;
static int last_remembered = -1, last_remembered_ptr = -1;
static void forget_pointer(void *ptr, const char *func, const char *file, int line)
{
  int i;
  if (ptr == NULL) {fprintf(stderr, "attempt to free NULL"); mem_report(); abort();}
  if (last_remembered_ptr == (int)ptr)
    {
      pointers[last_remembered] = 0;
      last_forgotten = last_remembered;
      last_remembered_ptr = -1;
      return;
    }
  for (i = 0; i < mem_size; i++)
    if (pointers[i] == (int)ptr)
      {
	pointers[i] = 0;
	last_forgotten = i;
	return;
      }
  fprintf(stderr, "forget %p ", ptr); mem_report(); abort();
}

static void remember_pointer(void *ptr, size_t len, const char *func, const char *file, int line)
{
  int i, loc = 0;
  if (last_forgotten == -1)
    {
      if (mem_size == 0)
	{
	  mem_size = 8192;
	  pointers = (int *)calloc(mem_size, sizeof(int));
	  sizes = (int *)calloc(mem_size, sizeof(int));
	  locations = (int *)calloc(mem_size, sizeof(int));
	  loc = 0;
	  goto GOT_ONE;
	}
      if (last_remembered != -1)
	for (i = last_remembered + 1; i < mem_size; i++)
	  if (pointers[i] == 0) 
	    {
	      loc = i;
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
      sizes = (int *)realloc(sizes, mem_size * sizeof(int));
      locations = (int *)realloc(locations, mem_size * sizeof(int));
      for (i = loc; i < mem_size; i++)
	{
	  pointers[i] = 0;
	  sizes[i] = 0;
	  locations[i] = 0;
	}
    }
  else
    {
      loc = last_forgotten;
      last_forgotten = -1;
    }
 GOT_ONE:
  pointers[loc] = (int)ptr;
  sizes[loc] = (int)len;
  locations[loc] = find_mem_location(func, file, line);
  last_remembered = loc;
  last_remembered_ptr = (int)ptr;
}

static void find_ptr_location(int ptr)
{
  int i;
  for (i = 0; i < mem_size; i++)
    if (pointers[i] == ptr)
      {
	fprintf(stderr,"found it at %d, size: %d, loc: %d, %s[%s: %d]\n",
		i, sizes[i], locations[i], 
		functions[locations[i]], files[locations[i]], lines[locations[i]]);
	return;
      }
  fprintf(stderr,"can't find it");
  if (last_forgotten != -1)
    fprintf(stderr,"last freed: at %d, size: %d, loc: %d, %s[%s: %d]\n",
	    last_forgotten, sizes[last_forgotten], locations[last_forgotten], 
	    functions[locations[last_forgotten]], files[locations[last_forgotten]], lines[locations[last_forgotten]]);
}

#define MAX_MALLOC (1 << 24)

void *mem_calloc(size_t len, size_t size, const char *func, const char *file, int line)
{
  void *ptr;
#if DEBUGGING
  if ((len <= 0) || (len > MAX_MALLOC))
    {
      fprintf(stderr, "%s:%s[%d] attempt to calloc %d bytes", func, file, line, len*size);
      mem_report(); abort();
    }
#endif
  ptr = calloc(len, size);
  remember_pointer(ptr, len*size, func, file, line);
  return(ptr);
}

void *mem_malloc(size_t len, const char *func, const char *file, int line)
{
  void *ptr;
#if DEBUGGING
  if ((len <= 0) || (len > MAX_MALLOC))
    {
      fprintf(stderr, "%s:%s[%d] attempt to malloc %d bytes", func, file, line, len);
      mem_report(); abort();
    }
#endif
  ptr = malloc(len);
  remember_pointer(ptr, len, func, file, line);
  return(ptr);
}

void mem_free(void *ptr, const char *func, const char *file, int line)
{
  forget_pointer(ptr, func, file, line);
  free(ptr);
}

void *mem_realloc(void *ptr, size_t size, const char *func, const char *file, int line)
{
  void *new_ptr;
#if DEBUGGING
  if ((size <= 0) || (size > MAX_MALLOC))
    {
      fprintf(stderr, "%s:%s[%d] attempt to realloc %d bytes", func, file, line, size);
      mem_report(); abort();
    }
#endif
  forget_pointer(ptr, func, file, line);
  new_ptr = realloc(ptr, size);
  remember_pointer(new_ptr, size, func, file, line);
  return(new_ptr);
}

char *mem_stats(snd_state *ss, int ub);
char *mem_stats(snd_state *ss, int ub)
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
    if ((sp=((snd_info *)(ss->sounds[i]))))
      {
	snds++;
	chns += sp->allocated_chans;
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

#if DEBUGGING && (!USE_NO_GUI)
char *stats_window_state(void);
#endif
#if TIMING
static void report_times_1(FILE *std);
#endif

void mem_report(void)
{
  int loc, i, sum, ptr = 0;
  int *sums, *ptrs;
  FILE *Fp;
  time_t ts;
  char time_buf[TIME_STR_SIZE];

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

  time(&ts);
  strftime(time_buf, TIME_STR_SIZE, STRFTIME_FORMAT, localtime(&ts));
  fprintf(Fp, "memlog: %s: %s\n\n", time_buf, mem_stats(get_global_state(), 0));

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
	}
    
}
#if DEBUGGING
  for (loc = 0; loc <= mem_location; loc++)
    if (strcmp(functions[loc], "init_sample_read_any_1") == 0)
      {
	for (i = 0; i < mem_size; i++)
	  if ((pointers[i]) && (locations[i] == loc))
	    fprintf(Fp, "    %s %s (%d : %d)\n",
		    ((snd_fd *)(pointers[i]))->caller,
		    ((snd_fd *)(pointers[i]))->filename,
		    ((snd_fd *)(pointers[i]))->beg,
		    ((snd_fd *)(pointers[i]))->end);

	break;
      }
  for (loc = 0; loc <= mem_location; loc++)
    if (strcmp(functions[loc], "make_snd_data_buffer") == 0)
      {
	for (i = 0; i < mem_size; i++)
	  if ((pointers[i]) && (locations[i] == loc))
	    fprintf(Fp, "    %s %s [%d %p %p %d %d %d %d %d %d]\n",
		    ((snd_data *)(pointers[i]))->caller, 
		    ((snd_data *)(pointers[i]))->filename,
		    ((snd_data *)(pointers[i]))->copy,
		    ((snd_data *)(pointers[i]))->io,
		    ((snd_data *)(pointers[i]))->hdr,
		    ((snd_data *)(pointers[i]))->edit_ctr,
		    ((snd_data *)(pointers[i]))->open,
		    ((snd_data *)(pointers[i]))->inuse,
		    ((snd_data *)(pointers[i]))->chan,
		    ((snd_data *)(pointers[i]))->len,
		    ((snd_data *)(pointers[i]))->just_zeros);
	break;
      }
#endif

  for (i = 0; i < 512; i++)
    if (mus_file_fd_name(i))
      fprintf(Fp, "[%d]: %s\n", i, mus_file_fd_name(i));
#if DEBUGGING && (!USE_NO_GUI)
  fprintf(Fp, stats_window_state());
#endif
#if TIMING
  fprintf(Fp,"\n\n--------------------------------\n");
  report_times_1(Fp);
#endif
  fprintf(Fp, "\n\nprevlist: %d %d\n", get_prevfile_end(), get_max_prevfile_end());
  fclose(Fp);
}

#endif

#if DEBUGGING

#if HAVE_CLOCK
#if HAVE_SYS_TIME_H
  #include <sys/time.h>
#endif

static clock_t start;
void start_timing(void) {start = clock();}
void stop_timing(void) {fprintf(stderr, "time: %d ",(int)((clock() - start) * 1000.0 / CLOCKS_PER_SEC));}

#if TIMING

/* gprof is completely confused by guile, so... */
typedef struct {
  long long total;
  long start;
  int in_calls, out_calls;
  char *name;
} tdat;

static tdat *times = NULL;
static int num_times = 0;
static int timer = 0;

int new_time(char *name) 
{
  timer++;
  if (num_times <= timer)
    {
      if (times == NULL)
	{
	  times = (tdat *)CALLOC(1024, sizeof(tdat));
	  num_times = 1024;
	}
      else 
	{
	  times = (tdat *)REALLOC(times, timer * 2 * sizeof(tdat));
	  num_times =  timer * 2;
	}
    }
  times[timer].in_calls = 0;
  times[timer].out_calls = 0;
  times[timer].start = 0;
  times[timer].total = 0;
  times[timer].name = copy_string(name);
  return(timer);
}

static SCM start_time(SCM utag)
{
  int tag;
  tag = TO_C_INT(utag);
  times[tag].start = clock();
  times[tag].in_calls++;
  return(FALSE_VALUE);
}

static SCM stop_time(SCM utag)
{
  int tag;
  long clk;
  tag = TO_C_INT(utag);
  if (times[tag].start <= 0) return(FALSE_VALUE);
  clk = clock();
  if (clk >= times[tag].start)
    times[tag].total += (clk - times[tag].start); /* clock() can wrap around! */
  else times[tag].total += clk; /* semi-bogus... */
  times[tag].start = 0;
  times[tag].out_calls++;
  return(FALSE_VALUE);
}

static int compare_time(const void *a, const void *b)
{
  tdat t1, t2;
  t1 = *((tdat *)a);
  t2 = *((tdat *)b);
  if (t1.out_calls > 0)
    {
      if (t2.out_calls > 0)
	{
#if 1
	  if (((Float)(t1.total) / (Float)(t1.out_calls)) == ((Float)(t2.total) / (Float)(t2.out_calls)))
	    return(0);
	  if (((Float)(t1.total) / (Float)(t1.out_calls)) > ((Float)(t2.total) / (Float)(t2.out_calls)))
	    return(-1);
#else
	  if (t1.total == t2.total) return(0);
	  if (t1.total > t2.total) return(-1);
#endif
	  return(1);
	}
      else return(-1);
    }
  if (t2.out_calls == 0)
    return(0);
  return(1);
}

static void report_times_1(FILE *std)
{
  int i, j = 0, len = 0;
  tdat *ltimes;
  for (i = 0; i < num_times; i++)
    if (times[i].name)
      len++;
  ltimes = (tdat *)CALLOC(len, sizeof(tdat));
  for (i = 0; i < num_times; i++)
    if (times[i].name)
      {
	ltimes[j].total = times[i].total;
	ltimes[j].out_calls = times[i].out_calls;
	ltimes[j].in_calls = times[i].in_calls;
	ltimes[j].name = times[i].name;
	j++;
      }
  qsort((void *)ltimes, len, sizeof(tdat), compare_time);
  for (i = 0; i < len; i++)
    if (ltimes[i].name)
      {
	if (ltimes[i].in_calls == 0)
	  fprintf(std, "%s never called\n",
		  ltimes[i].name);
	else
	  {
	    if (ltimes[i].in_calls == ltimes[i].out_calls)
	      fprintf(std,"%s[%d]: %f, %f\n",
		      ltimes[i].name,
		      ltimes[i].in_calls,
		      (float)(ltimes[i].total) / 1000000.0,
		      (float)(ltimes[i].total) / (ltimes[i].in_calls * 1000000.0));
	    else
	      {
		if (ltimes[i].out_calls == 0)
		  fprintf(std,"%s[%d but never returned]\n",
			  ltimes[i].name,
			  ltimes[i].in_calls);
		else
		  fprintf(std,"%s[%d -> %d]: %f, %f\n",
			  ltimes[i].name,
			  ltimes[i].in_calls, ltimes[i].out_calls,
			  (float)(ltimes[i].total) / 1000000.0,
			  (float)(ltimes[i].total) / (ltimes[i].out_calls * 1000000.0));
	      }
	  }
      }
  FREE(ltimes);
}

static SCM report_times(void)
{
  report_times_1(stderr);
  return(FALSE_VALUE);
}

void g_init_timing(SCM local_doc)
{
#if HAVE_GUILE
  /* not DEFINE_PROC here! */
  NEW_PROCEDURE("start-time", start_time, 1, 0, 0);
  NEW_PROCEDURE("stop-time", stop_time, 1, 0, 0);
  NEW_PROCEDURE("report-times", report_times, 0, 0, 0);
#endif
}
#endif
#endif
#endif
