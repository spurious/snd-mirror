
#include <mus-config.h>

#ifdef WITH_RT

#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <stdarg.h>
#include <errno.h>
#include <sched.h>

#include "rt-various.h"

#include "_sndlib.h"
#include "clm.h"


/* Stalin stuff. */

int fix_stalin_c_source(char* infile, char *outfile){
  char temp[10000];
  FILE *file=fopen(infile,"r");
  FILE *out=fopen(outfile,"w");
  while(fgets(temp,9990,file)){
    if(0
       || temp[0]==' '
       || temp[0]=='#'
       || temp[0]=='{'
       || (!isalpha(temp[0]))
       )
      if(!strcmp("#include <gc.h>\n",temp))
        fputs("#include <rt-various.h>\n",out);
      else
        fputs(temp,out);
    else{
      int len=strlen(temp);
      if(0
         || (temp[len-3]!='}' && temp[len-2]==';')
         || (temp[len-3]==';' && temp[len-2]=='}')
         || (temp[len-4]=='}' && temp[len-3]=='}' && temp[len-2]==';')
         || temp[len-2]=='/'
         || temp[len-2]==')'
         ){
        fprintf(out,"static ");
        if(!strcmp("int main(void)\n",temp))
          fputs("int schememain(void)\n",out);
        else
          fputs(temp,out);
      }else
        fputs(temp,out);
    }
  }
  fclose(out);
  fclose(file);
  return 0;
}


/* Rollendurchmesserzeitsammler stuff. */


static void print_error(FILE *where,char *fmt, ...) {
  char temp[10000];
  va_list ap;
  va_start(ap, fmt);{
    vsnprintf (temp, 9998, fmt, ap);
  }va_end(ap);
  //syslog(LOG_INFO,temp);
  fprintf(where,"tar snapshot benchmark: %s\n",temp);
}

static int set_pid_priority(pid_t pid,int policy,int priority,char *message,char *name){
  struct sched_param par={0};
  par.sched_priority=priority;
  if((sched_setscheduler(pid,policy,&par)!=0)){
    print_error(stderr,message,pid,name,strerror(errno));
    return 0;
  }
  return 1;
}

static void set_realtime(void){
  set_pid_priority(0,SCHED_FIFO,60,"Unable to set SCHED_FIFO for %d (\"%s\"). (%s)", "set_realtime()");
}

static void* threadstart_realtime(void *arg){
  tar_threadstart threadstart=(tar_threadstart)arg;
  set_realtime();
  threadstart();
  return NULL;

}

static void create_realtime_thread(tar_threadstart threadstart){
  pthread_t *thread;
  thread=calloc(sizeof(pthread_t*),1);
  pthread_create(thread,NULL,threadstart_realtime,threadstart);
}

static void* threadstart_mark(void *arg){
  tar_threadstart threadstart=(tar_threadstart)arg;
  //set_pid_priority(0,SCHED_FIFO,0,"Unable to set SCHED_FIFO for %d (\"%s\"). (%s)", "threadstart_mark()");
  threadstart();
  return NULL;
}

static void create_mark_thread(tar_threadstart threadstart){
  pthread_t *thread;
  thread=calloc(sizeof(pthread_t*),1);
  pthread_create(thread,NULL,threadstart_mark,threadstart);
}


void init_rollendurchmesserzeitsammler(int a,int b,int c){
  tar_init(a,
	   b,
	   c,
	   create_realtime_thread,
	   create_mark_thread
	   );
}


// Force linking of various functions in rollendurchmesserzeitsammler.
void rt_various_dummy(void){
  fprintf(stderr,"It is an error to call this function.\n");

  tar_get_dynamic_roots_for(NULL,NULL,NULL);
  tar_entering_audio_thread(NULL);
  tar_leave_audio_thread(NULL);
  tar_add_root(NULL,NULL,NULL);
  tar_run_gc(NULL);
  tar_new_heap();
  tar_delete(NULL);
  tar_alloc(NULL,0);
  tar_alloc_atomic(NULL,0);
  tar_malloc_atomic_uncollectable(0);
  tar_calloc_atomic_uncollectable(0,0);
  tar_free_atomic_uncollectable(NULL);

}



/* clm stuff */

static tar_heap_t *clm_tar_heap=NULL;
static error_func_t clm_error_func=NULL;

tar_heap_t *clm_set_tar_heap(tar_heap_t *new_heap){
    tar_heap_t *ret=clm_tar_heap;
    clm_tar_heap=new_heap;
    return ret;
}

error_func_t clm_set_error_func(error_func_t new_error){
  error_func_t ret=clm_error_func;
  clm_error_func=new_error;
  return ret;
}


int rt_mus_error(int type,const char* fmt,...){
  int size;
  va_list argp;
  static char string[1024]; //Should be static, stack is limited in realtime.

  va_start(argp,fmt);
  vsprintf(string,fmt,argp);
  va_end(argp);

  if(clm_error_func==NULL)
    return mus_error(type,string);
  else
    clm_error_func(string);
  return -1;
}


void* clm_calloc_atomic(int num,size_t size,const char* what){
  if(clm_tar_heap==NULL){
    return CALLOC(num,size);
  }else{
    void *ret=tar_alloc_atomic(clm_tar_heap,num*size);
    if(ret==NULL){
      rt_mus_error(0,"clm.c: out of memory. (%s)",what);
    }
    memset(ret,0,num*size);
    return ret;
  }
}


void* clm_calloc(int num,size_t size,const char* what){
  if(clm_tar_heap==NULL){
    return CALLOC(num,size);
  }else{
    void *ret=tar_alloc(clm_tar_heap,num*size);
    if(ret==NULL){
      rt_mus_error(0,"clm.c: out of memory. (%s)",what);
    }
    return ret;
  }
}

void* clm_malloc_atomic(size_t size,const char* what){
  if(clm_tar_heap==NULL){
    return MALLOC(size);
  }else{
    void *ret=tar_alloc_atomic(clm_tar_heap,size);
    if(ret==NULL){
      rt_mus_error(0,"clm.c: out of memory. (%s)",what);
    }
    return ret;
  }
}

void* clm_malloc(size_t size,const char* what){
  if(clm_tar_heap==NULL){
    return MALLOC(size);
  }else{
    void *ret=tar_alloc(clm_tar_heap,size);
    if(ret==NULL){
      rt_mus_error(0,"clm.c: out of memory. (%s)",what);
    }
    return ret;
  }
}

void* clm_realloc(void* old,size_t newsize){
  tar_mem_t *mem=(tar_mem_t*)(((char*)old)-sizeof(tar_mem_t));
  size_t size=mem->size;
  if(newsize<size)
    size=newsize;
  return memcpy(clm_malloc(newsize,"realloc"),old,size);
}


void clm_free(void* p){
  if(clm_tar_heap==NULL)
    FREE(p);
}


#endif // WITH_RT
