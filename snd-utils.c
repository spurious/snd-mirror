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

Float in_dB(Float min_dB, Float lin_dB, Float py)
{
  return((py <= lin_dB) ? min_dB : (20.0 * (log10(py))));
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
  if (str)
    return(strdup(str));
  return(NULL);
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
char local_decimal_point(void)
{
#if HAVE_LANGINFO_DECIMAL_POINT
  return(nl_langinfo(DECIMAL_POINT)[0]);
#endif
#if HAVE_LANGINFO_RADIXCHAR
  return(nl_langinfo(RADIXCHAR)[0]);
#endif
  return(STR_decimal);
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
      ps[1] = decimal_pt;
    }
  if ((*fs) == decimal_pt)
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
char *shorter_tempnam(char *udir, char *prefix)
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

#ifdef DEBUG_MEMORY

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

static int mem_size = 0, mem_top = -1;
static int *pointers = NULL, *sizes = NULL, *locations = NULL;
static char **functions = NULL, **files = NULL;
static int *lines = NULL;
static int mem_location = -1;
static int mem_locations = 0;
static int last_mem_location = -1;
static char **stacks = NULL;

static char *encloser = NULL;
void set_encloser(char *name) 
{
  if (name) encloser = name;
  else encloser = NULL;
}

static int find_mem_location(const char *func, const char *file, int line)
{
  int i;
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
      stacks[last_remembered] = NULL;
      last_forgotten = last_remembered;
      last_remembered_ptr = -1;
      return;
    }
  for (i = mem_top; i >= 0; i--)
    if (pointers[i] == (int)ptr)
      {
	pointers[i] = 0;
	stacks[i] = NULL;
	last_forgotten = i;
	if (i == mem_top)
	  while ((mem_top >= 0) && (pointers[mem_top] == 0)) mem_top--;
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
	  stacks = (char **)calloc(mem_size, sizeof(char *));
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
      stacks = (char **)realloc(stacks, mem_size * sizeof(char *));
      for (i = loc; i < mem_size; i++)
	{
	  pointers[i] = 0;
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
  sizes[loc] = (int)len;
  locations[loc] = find_mem_location(func, file, line);
  if (encloser) stacks[loc] = encloser;
  last_remembered = loc;
  last_remembered_ptr = (int)ptr;
  if (mem_top < loc) mem_top = loc;
}

#define MAX_MALLOC (1 << 28)

#if WITH_EFENCE
void *ef_calloc(size_t a, size_t b);
void *ef_malloc(size_t a);
void *ef_free(void *a);
void *ef_realloc(void *a, size_t b);
#endif

void *mem_calloc(size_t len, size_t size, const char *func, const char *file, int line)
{
  void *ptr;
  if ((len == 0) || (len > MAX_MALLOC))
    {
      fprintf(stderr, "%s:%s[%d] attempt to calloc %d bytes", func, file, line, len * size);
      mem_report(); abort();
    }
#if WITH_EFENCE
  ptr = ef_calloc(len, size);
#else
  ptr = calloc(len, size);
#endif
  if (ptr == NULL) {fprintf(stderr,"calloc->null"); abort();}
  remember_pointer(ptr, len * size, func, file, line);
  return(ptr);
}

void *mem_malloc(size_t len, const char *func, const char *file, int line)
{
  void *ptr;
  if ((len == 0) || (len > MAX_MALLOC))
    {
      fprintf(stderr, "%s:%s[%d] attempt to malloc %d bytes", func, file, line, len);
      mem_report(); abort();
    }
#if WITH_EFENCE
  ptr = ef_malloc(len);
#else
  ptr = malloc(len);
#endif
  if (ptr == NULL) {fprintf(stderr,"malloc->null"); abort();}
  remember_pointer(ptr, len, func, file, line);
  return(ptr);
}

void mem_free(void *ptr, const char *func, const char *file, int line)
{
  /* fprintf(stderr,"free %s %s[%d]: %p\n", func, file, line, ptr); */
  forget_pointer(ptr, func, file, line);
#if WITH_EFENCE
  ef_free(ptr);
#else
  free(ptr);
#endif
}

void *mem_realloc(void *ptr, size_t size, const char *func, const char *file, int line)
{
  void *new_ptr;
  if ((size == 0) || (size > MAX_MALLOC))
    {
      fprintf(stderr, "%s:%s[%d] attempt to realloc %d bytes", func, file, line, size);
      mem_report(); abort();
    }
  forget_pointer(ptr, func, file, line);
#if WITH_EFENCE
  new_ptr = ef_realloc(ptr, size);
#else
  new_ptr = realloc(ptr, size);
#endif
  if (new_ptr == NULL) {fprintf(stderr,"realloc->null"); abort();}
  remember_pointer(new_ptr, size, func, file, line);
  return(new_ptr);
}

static char *mem_stats(snd_state *ss, int ub)
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
  fd = open(pathname, flags, mode);
  if (fd != -1) io_add(pathname, func, file, line, fd, NULL);
  return(fd);
}

int io_creat(const char *pathname, mode_t mode, const char *func, const char *file, int line)
{
  int fd;
  fd = creat(pathname, mode);
  if (fd != -1) io_add(pathname, func, file, line, fd, NULL);
  return(fd);
}

int io_close(int fd, const char *func, const char *file, int line)
{
  io_subtract(fd, NULL, func, file, line);
  return(close(fd));
}

FILE *io_fopen(const char *path, const char *mode, const char *func, const char *file, int line)
{
  FILE *fp;
  fp = fopen(path, mode);
  if (fp) io_add(path, func, file, line, -1, fp);
  return(fp);
}

int io_fclose(FILE *stream, const char *func, const char *file, int line)
{
  io_subtract(-1, stream, func, file, line);
  return(fclose(stream));
}

void dump_protection(FILE *Fp);
#if DEBUGGING
  void report_dangling_readers(FILE *fp);
#endif

void mem_report(void)
{
  int loc, i, j, sum, ptr = 0, have_stacks = FALSE;
  int *sums, *ptrs;
  FILE *Fp;
  time_t ts;
  char time_buf[TIME_STR_SIZE];
  snd_state *ss;
  ss = get_global_state();
  if (ss->search_tree)
    ss->search_tree = free_ptree(ss->search_tree);
  for (i = 0; i < mem_size; i++)
    if (stacks[i])
      {
	have_stacks = TRUE;
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
    str = mem_stats(ss, 0);
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
	  fprintf(Fp, "%s[%d]:%s:  %d (%d)", files[ptr], lines[ptr], functions[ptr], sums[ptr], ptrs[ptr]);
	  if (0)
	    {
	      int fd, i, line, bytes, happy = TRUE;
	      char buf[8192];
	      fd = open(files[ptr], O_RDONLY, 0);
	      line = 1;
	      while (happy)
		{
		  bytes = read(fd, buf, 8192);
		  if (bytes <= 0) 
		    {
		      fprintf(Fp, "where is %s[%d]?\n", files[ptr], lines[ptr]);
		      happy = FALSE;
		      break;
		    }
		  for (i = 0; i < bytes; i++)
		    {
		      if (buf[i] == '\n')
			{
			  line++;
			  if (line == lines[ptr]) fprintf(Fp, ":   ");
			  if (line > lines[ptr]) 
			    {
			      fprintf(Fp, "\n");
			      happy = FALSE;
			      break;
			    }
			}
		      else
			{
			  if (line == lines[ptr]) fprintf(Fp, "%c", buf[i]);
			}
		    }
		}
	      close(fd);
	    }
	  else fprintf(Fp, "\n");
	  sums[ptr] = 0;
	  if (have_stacks)
	    for (j = 0; j < mem_size; j++)
	      if ((stacks[j]) && (locations[j] == ptr) && (pointers[j]))
		fprintf(Fp, "    %s    %p\n", stacks[j], (void *)(pointers[j]));
	  if ((strcmp("mus_format", functions[ptr]) == 0) ||
	      (strcmp("copy_string", functions[ptr]) == 0))
	    {
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
  /* mus_sound_report_cache(Fp); */
#if DEBUGGING
  report_dangling_readers(Fp);
#endif

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

void g_init_utils(void)
{
  decimal_pt = local_decimal_point();
}
