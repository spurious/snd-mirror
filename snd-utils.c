#include "snd.h"

int round(Float x)
{
  int i;
  i = (int)x;
  if ((x-i) > 0.5) return(i+1);
  return(i);
}

char *copy_string (char *str)
{
  char *newstr = NULL;
  if (str)
    {
      newstr = (char *)CALLOC(strlen(str)+1,sizeof(char));
      strcpy(newstr,str);
    }
  return(newstr);
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
  int i,len,last_slash;
  last_slash = 0;
  len = strlen(name);
  for (i=0;i<len-1;i++) if (name[i] == '/') last_slash = i+1;
  return((char *)(name+last_slash));
}

char *file_extension(char *arg)
{
  char *dot = NULL,*sp;
  if (arg) for (sp=arg;(*sp) != '\0';sp++) if ((*sp) == '.') dot=(++sp);
  return(dot);
}

char *just_filename(char *name)
{
  char *nodir;
  int i,len;
  nodir = copy_string(filename_without_home_directory(name));
  len = strlen(nodir);
  for (i=0;i<len;i++) if (nodir[i] == '.') {nodir[i] = '\0'; break;}
  return(nodir);
}

#ifndef sqr
Float sqr(Float a) {return(a*a);}
#endif

Float cube (Float a) {return(a*a*a);}

static char prtbuf[256];

char *prettyf(Float num, int tens)
{ /* try to prettify float display -- if tens<0, return int */
  int fullf,len,i;
  Float rounder;
  char *newval,*sp,*sn,*zp;
  zp=NULL;
  if (tens>9) tens=9;
  if (num < 0.0) rounder = -.49; else rounder = .49;
  if (tens < 0)
    {
      fullf=(int)(num+rounder);
      sprintf(prtbuf,"%d",fullf);
      return(copy_string(prtbuf));
    }
  fullf = (int)num;
  if ((num-fullf) == 0.0) 
    {
      if (num<100.0)
	sprintf(prtbuf,"%d%c0",fullf,STR_decimal);
      else sprintf(prtbuf,"%d",fullf);
      return(copy_string(prtbuf));
    }
  if (num > 1000)
    {
      sprintf(prtbuf,"%d%c%d",fullf,STR_decimal,(int)((num-fullf)*pow(10.0,tens)));
      return(copy_string(prtbuf));
    }
  fullf = (int)(num*pow(10.0,tens+1)+rounder);
  if (fullf == 0) 
    {
      /* will be freed later, so can't return a constant */
      newval=(char *)CALLOC(2,sizeof(char));
      newval[0] = '0';
      newval[1] = '\0';
      return(newval);
    }
  sprintf(prtbuf,"%d",fullf);
  len=strlen(prtbuf);
  newval=(char *)CALLOC(len+tens+10,sizeof(char));
  sn = newval;
  sp = prtbuf;
  if ((*sp) == '-') {(*sn) = (*sp); sn++; sp++; len--;}
  if (len >= (tens+1))
    {
      for (i=0;i<len-tens-1;i++) {(*sn)=(*sp); sn++; sp++;}
      (*sn)=STR_decimal; sn++;
    }
  else
    {
      (*sn) = '0';
      sn++;
      (*sn) = STR_decimal;
      sn++;
      for (i=0;i<abs(len-tens-1);i++) 
	{
	  (*sn) = '0';
	  sn++;
	}
    }
  if (tens > 5) tens = 5;
  for (i=0;i<=tens;i++) 
    {
      if (!(*sp)) break;
      (*sn)=(*sp); 
      if ((!zp) || ((*sn) != '0')) zp = sn;
      sn++; 
      sp++;
    }
  if (zp) sn=zp; else (*sn)='0';
  sn++;
  (*sn) = '\0';
  return(newval);
}

int disk_space_p(snd_info *sp, int fd, int bytes, int other_bytes)
{
  int kfree,kneeded,kother,go_on;
  kfree = disk_kspace(fd);
  if (kfree < 0) {sprintf(prtbuf,strerror(errno)); report_in_minibuffer(sp,prtbuf); return(NO_PROBLEM);}
  kneeded = bytes >> 10;
  if (kfree < kneeded)
    {
      if (other_bytes > 0)
	{
	  kother = other_bytes >> 10;
	  if (kother > kfree)
	    {
	      sprintf(prtbuf,STR_no_room_but_we_try,kfree<<10);
	      report_in_minibuffer(sp,prtbuf);
	      return(HUNKER_DOWN);
	    }
	}
      sprintf(prtbuf,STR_no_room_go_on_p,kfree<<10);
      go_on = snd_yes_or_no_p(sp->state,prtbuf);
      if (!go_on) return(GIVE_UP);
      report_in_minibuffer(sp,STR_going_on);
      return(BLIND_LEAP);
    }
  return(NO_PROBLEM);
}

int snd_checked_write(snd_state *ss, int fd, unsigned char *buf, int bytes)
{
  /* io.c checked_write assumes its file descriptors are around */
  int bytes_written,kfree;
  kfree = disk_kspace(fd);
  if (kfree < 0) {snd_error(strerror(errno)); return(-1);}
  if (kfree < (bytes>>10))
    {
      sprintf(prtbuf,STR_no_room,kfree<<10,bytes);
      snd_yes_or_no_p(ss,prtbuf);
      return(-1);
    }
  bytes_written = write(fd,buf,bytes);
  if (bytes_written != bytes)
    {
      snd_error(STR_write_failed,strerror(errno));
      return(-1);
    }
  return(bytes_written);
}

void fill_number(char *fs, char *ps)
{
  int i,j;
  j=snd_strlen(fs);
  if (j>4) j=4;
  if (j<4) {ps[4] = '\0'; ps[3]='0'; ps[2]='0'; ps[1]=STR_decimal;}
  if ((*fs) == STR_decimal) {*ps++ = '0'; if (j==4) j=3;}
  for (i=0;i<j;i++) (*ps++) = (*fs++);
}

static int sect_ctr = 0;
char *shorter_tempnam(char *udir,char *prefix)
{
  /* tempnam turns out names that are inconveniently long (in this case the filename is user-visible) */
  char *str,*tmpdir;
  str = (char *)CALLOC(256,sizeof(char));
  if ((udir) && (*udir))
    sprintf(str,"%s/%s%d.snd",udir,(prefix) ? prefix : "snd_",sect_ctr++);
  else
    {
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
      sprintf(str,"%s/%s%d.snd",tmpdir,(prefix) ? prefix : "snd_",sect_ctr++);
    }
  return(str);
}

#if (!HAVE_TEMPNAM)
static char *tempnam(const char *ignored, const char *tmp)
{
  return(copy_string(tmpnam(NULL)));
}
#endif

char *snd_tempnam(snd_state *ss)
{
  /* problem here is that NULL passed back from Guile becomes "" which is not NULL from tempnam's point of view */
  char *udir;
  udir = temp_dir(ss);
  if ((udir) && (*udir))
    return(tempnam(udir,"snd_"));
  return(tempnam(NULL,"snd_"));
}

Float dB(snd_state *ss, Float py)
{
  return((py <= ss->lin_dB) ? ss->min_dB : (20.0*(log10(py))));
}

Float un_dB(snd_state *ss, Float py)
{
  return((py <= ss->min_dB) ? 0.0 : pow(10.0,py*.05));
}

void snd_exit(int val)
{
#ifndef SND_AS_WIDGET
  exit(val);
#endif
}

/* check_balance stolen from scmwm-0.9/utilities/scwmrepl/scwmrepl.c */

int check_balance(char *expr, int start, int end) 
{
  /* If you think _this_ is hairy, try doing it for C statements. */
  int i;
  int non_whitespace_p=0;
  int paren_count=0;
  int prev_separator=1;
  int quote_wait=0;

  i=start;
  while (i<end) {
    switch(expr[i]) {
    case ';' :
      /* skip till newline. */
      do {
	i++;
      } while (expr[i]!='\n' && i<end);
      break;
    case ' ':
    case '\n':
    case '\t':
    case '\r':
      if (non_whitespace_p && paren_count==0 && !quote_wait) {
	return i;
      } else {
	prev_separator=1;
	i++;
      }
      break;
    case '\"' :
      if (non_whitespace_p && paren_count==0 &&
	  !quote_wait) {
	return i;
      } else {
	/* skip past ", ignoring \" */
	do {
 	  i++;
	  if (i < end && expr[i]=='\\') {
	    i++;
	  }
	} while (i < end && expr[i]!='\"');
	i++;
	if (paren_count==0) {
	  if (i < end) {
	    return i;
	  } else {
	    return 0;
	  }
	} else {
	  prev_separator=1;
	  non_whitespace_p=1;
	  quote_wait=0;
	}
      }
      break;
    case '#' :
      if (non_whitespace_p && paren_count==0 &&
	  !quote_wait) {
	return i;
      } else {
	if (prev_separator && i+1<end && expr[i+1]=='{') {
	  /* skip past }#, ignoring \} */
	  do {
	    i++;
	    if (i < end && expr[i]=='\\') {
	      i++;
	    }
	  } while (i < end && !(expr[i]=='}' && i+1<end
				&& expr[i+1]=='#'));
	  i+=2;
	  if (paren_count==0) {
	    if (i < end) {
	      return i;
	    } else {
	      return 0;
	    }
	  } else {
	    prev_separator=1;
	    non_whitespace_p=1;
	    quote_wait=0;
	  }
	  /* MS:FIXME:: Handle #\) properly! */
	} else {
	  prev_separator=0;
	  quote_wait=0;
	  non_whitespace_p=1;
	  i++;
	}
      }
      break;
    case '(' :
      if (non_whitespace_p && paren_count==0 &&!quote_wait) {
	return i;
      } else {
	i++;
	paren_count++;
	non_whitespace_p=1;
	prev_separator=1;
	quote_wait=0;
      }
      break;
    case ')' :
      paren_count--;
      if (non_whitespace_p && paren_count==0) {
	return i+1;
      } else {
	i++;
	non_whitespace_p=1;
	prev_separator=1;
	quote_wait=0;
      }
      break;
    case '\'' :
      if (prev_separator) {
	non_whitespace_p=1;
	quote_wait=1;
	prev_separator=1;
	i++;
      } else {
	non_whitespace_p=1;
	prev_separator=0;
	i++;
      }
      break;
    default :
      prev_separator=0;
      quote_wait=0;
      non_whitespace_p=1;
      i++;
      break;
    }
  }
  return 0;
}

char *kmg (int num)
{
  /* return number 0..1024, then in terms of K, M, G */
  char *str;
  str = (char *)calloc(16,sizeof(char));
  if (num > 1024)
    {
      if (num > (1024*1024))
	{
	  if (num > (1024*1024*1024))
	    sprintf(str,"%.5fG",(float)num/(float)(1024*1024*1024));
	  else sprintf(str,"%.4fM",(float)num/(float)(1024*1024));
	}
      else sprintf(str,"%.3fK",(float)num/1024.0);
    }
  else sprintf(str,"%d",num);
  return(str);
}

#ifdef DEBUG_MEMORY

/* mtrace-style malloc hooks are not very useful here since I don't care
 * about X allocations (of which there are millions), and I need readable
 * backtrace info for leaks.  Doing it by hand makes it easy to sort
 * output and whatnot. All of Sndlib and Snd use the macros CALLOC,
 * REALLOC, and FREE (never MALLOC though it's defined).
 */

#define MEM_SIZE 65536

static char *encloser = NULL;
void set_encloser(char *name) {encloser = name;} /* for exposing call chains */

static int pointers[MEM_SIZE],sizes[MEM_SIZE],locations[MEM_SIZE];
static char **functions,**files;
static int *lines;
static int forgetting = 0;
static int mem_location = -1;
static int mem_locations = 0;

static int find_mem_location(char *ur_func, char *file, int line)
{
  int i;
  char *func = NULL;
  if (encloser)
    {
      func = (char *)calloc(strlen(encloser)+strlen(ur_func)+4,sizeof(char));
      sprintf(func,"%s->%s",encloser,ur_func);
    }
  else func = ur_func;
  for (i=0;i<=mem_location;i++)
    {
      if ((line == lines[i]) &&
	  (strcmp(func,functions[i]) == 0) &&
	  (strcmp(file,files[i]) == 0))
	return(i);
    }
  mem_location++;
  if (mem_location >= mem_locations)
    {
      if (mem_locations == 0)
	{
	  functions = (char **)calloc(1024,sizeof(char *));
	  files = (char **)calloc(1024,sizeof(char *));
	  lines = (int *)calloc(1024,sizeof(int));
	  mem_locations = 1024;
	}
      else
	{
	  functions = (char **)realloc(functions,(mem_locations+1024)*sizeof(char *));
	  files = (char **)realloc(files,(mem_locations+1024)*sizeof(char *));
	  lines = (int *)realloc(lines,(mem_location+1024)*sizeof(int));
	  for (i=0;i<1024;i++) 
	    {
	      functions[i+mem_locations] = NULL;
	      files[i+mem_locations] = NULL;
	      lines[i+mem_locations] = 0;
	    }
	  mem_locations += 1024;
	}
    }
  /* NOT copy_string HERE!! */
  functions[mem_location] = (char *)calloc(strlen(func)+1,sizeof(char));
  strcpy(functions[mem_location],func);
  files[mem_location] = (char *)calloc(strlen(file)+1,sizeof(char));
  strcpy(files[mem_location],file);
  lines[mem_location] = line;
  return(mem_location);
}

static void forget_pointer(void *ptr, char *func, char *file, int line)
{
  int i;
  if (ptr == NULL) {fprintf(stderr,"attempt to free NULL"); abort();}
  for (i=0;i<MEM_SIZE;i++)
    if (pointers[i] == (int)ptr)
      {
	pointers[i] = 0;
	return;
      }
  if (forgetting == 0) {fprintf(stderr,"forget %p ",ptr); abort();}
}

static void remember_pointer(void *ptr, size_t len, char *func, char *file, int line)
{
  int i,least=10000,least_loc=-1;
  for (i=0;i<MEM_SIZE;i++)
    {
      if (pointers[i] == 0) 
	{
	  least_loc = i;
	  break;
	}
      if (sizes[i]<least)
	{
	  least = sizes[i];
	  least_loc = i;
	}
    }
  if (pointers[least_loc] != 0) {forgetting = 1; fprintf(stderr,"mem overflow!"); abort();}
  pointers[least_loc] = (int)ptr;
  sizes[least_loc] = (int)len;
  locations[least_loc] = find_mem_location(func,file,line);
}

void *mem_calloc(size_t len, size_t size, char *func, char *file, int line)
{
  void *ptr;
  ptr = calloc(len,size);
  remember_pointer(ptr,len*size,func,file,line);
  return(ptr);
}

void *mem_malloc(size_t len, char *func, char *file, int line)
{
  void *ptr;
  ptr = malloc(len);
  remember_pointer(ptr,len,func,file,line);
  return(ptr);
}

void mem_free(void *ptr, char *func, char *file, int line)
{
  forget_pointer(ptr,func,file,line);
  free(ptr);
}

void *mem_realloc(void *ptr, size_t size, char *func, char *file, int line)
{
  void *new_ptr;
  forget_pointer(ptr,func,file,line);
  new_ptr = realloc(ptr,size);
  remember_pointer(new_ptr,size,func,file,line);
  return(new_ptr);
}

char *mem_stats(snd_state *ss, int ub);
char *mem_stats(snd_state *ss, int ub)
{
  int i,ptrs=0,sum=0,snds=0,chns=0;
  snd_info *sp;
  char *result,*ksum=NULL,*kptrs=NULL,*kpers=NULL;
  for (i=0;i<MEM_SIZE;i++)
    if (pointers[i])
      {
	ptrs++;
	sum += sizes[i];
      }
  result = (char *)calloc(128,sizeof(char));
  for (i=0;i<ss->max_sounds;i++)
    {
      if ((sp=((snd_info *)(ss->sounds[i]))))
	{
	  snds++;
	  chns += sp->allocated_chans;
	}
    }
  sprintf(result,"snd mem: %s (%s%s), %d sounds, %d chans (%s)\n",
	  ksum=kmg(sum),kptrs=kmg(ptrs),
	  (forgetting) ? "+" : "",snds,chns,
	  (chns>0) ? (kpers=kmg(ub / chns)) : "");
  if (ksum) free(ksum);
  if (kptrs) free(kptrs);
  if (kpers) free(kpers);
  return(result);
}

void mem_report(void);
void mem_report(void)
{
  int loc,i,sum,ptr=0;
  int *sums,*ptrs;
  FILE *Fp;
  time_t ts;
  char time_buf[TIME_STR_SIZE];

  sums = (int *)calloc(mem_location+1,sizeof(int));
  ptrs = (int *)calloc(mem_location+1,sizeof(int));
  for (loc=0;loc<=mem_location;loc++)
    {
      sum=0;
      ptr=0;
      for (i=0;i<MEM_SIZE;i++)
	{
	  if ((pointers[i]) && (locations[i] == loc))
	    {
	      sum += sizes[i];
	      ptr++;
	    }
	}
      sums[loc]=sum;
      ptrs[loc]=ptr;
    }
  Fp=fopen("memlog","w");

  time(&ts);
  strftime(time_buf,TIME_STR_SIZE,STRFTIME_FORMAT,localtime(&ts));
  fprintf(Fp,"memlog: %s\n\n",time_buf);

  for (i=0;i<=mem_location;i++)
    {
      sum=0;
      for (loc=0;loc<=mem_location;loc++)
	{
	  if (sums[loc]>sum)
	    {
	      ptr = loc;
	      sum = sums[loc];
	    }
	}
      if (sum>0)
	{
	  fprintf(Fp,"%s[%d]:%s:  %d (%d)\n",files[ptr],lines[ptr],functions[ptr],sums[ptr],ptrs[ptr]);
	  sums[ptr] = 0;
	}
    }
  fclose(Fp);
}

#endif

#if DEBUGGING

#if defined(SGI) || defined(LINUX) || defined(BEOS) || defined(SUN)
  #include <sys/time.h>
#endif

#if defined(NEXT) || defined(LINUX) || defined(BEOS) || defined(SGI) || defined(SUN)
  struct timeval t0,t1;
  struct timezone z0,z1;
#else
  static clock_t c1,c2;
#endif

static void start_time (void)
{
#if defined(NEXT) || defined(LINUX) || defined(BEOS) || defined(SUN)
  gettimeofday(&t0,&z0);
#else
  #ifdef SGI
    gettimeofday(&t0);
  #else
    c1=clock();
  #endif
#endif
}

static int run_time (void)
{
  int secs,millisecs;
#if defined(NEXT) || defined(LINUX) || defined(BEOS) || defined(SUN)
  gettimeofday(&t1,&z1);
#else
  #ifdef SGI
    gettimeofday(&t1);
  #else
      c2=clock();
      return((int)(1000.0*((float)(c2-c1))/(float)CLOCKS_PER_SEC));
  #endif
#endif
  secs = (t1.tv_sec - t0.tv_sec);
  millisecs = ((t1.tv_usec - t0.tv_usec) / 1000);
  return(secs*1000+millisecs);
}

static int timing_in_progress = 0;

void start_timing(void)
{
  if (timing_in_progress == 0)
    {
      timing_in_progress = 1;
      start_time();
    }
}

void stop_timing(void)
{
  timing_in_progress = 0;
  fprintf(stderr,"time: %d ",run_time());
}
#endif
