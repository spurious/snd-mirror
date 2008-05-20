
#include <mus-config.h>

#ifdef WITH_RT

#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <stdarg.h>
#include <errno.h>

#include "rt-various.h"

#include "rollendurchmesserzeitsammler.h"


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

#endif // WITH_RT
