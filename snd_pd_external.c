/* --------------------------- snd_pd_external  ------------------------------- */
/* Made by Kjetil S. Matheussen, 2005.                                          */
/*                                                                              */
/* Based on:                                                                    */
/*                                                                              */
/*     --------------------------- k_guile  ----------------------------------- */
/*   ;; Kjetil S. Matheussen, 2004.                                             */
/*                                                                              */
/* This program is free software; you can redistribute it and/or                */
/* modify it under the terms of the GNU General Public License                  */
/* as published by the Free Software Foundation; either version 2               */
/* of the License, or (at your option) any later version.                       */
/*                                                                              */
/* This program is distributed in the hope that it will be useful,              */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of               */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                */
/* GNU General Public License for more details.                                 */
/*                                                                              */
/* You should have received a copy of the GNU General Public License            */
/* along with this program; if not, write to the Free Software                  */
/* Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.  */
/*                                                                              */
/* ---------------------------------------------------------------------------- */


/* INFO: SND runs in its own thread. Communication between PD and SND happens
   by sending messages back and forth on two ringbuffer. All functions running
   in the PD thread (ie. main program) have the prefix "pd0_", and all functions
   running in the SND thread have the prefix "snd0_".

   TOPIC: Is gensym() (in m_class.c) thread-safe? It looks so, but I'm not sure.
*/


#include "snd.h"


/* Need some more macros. */

#define POINTER_P(x) (((int) (x) & 3) == 0)
#define INTEGER_P(x) (! POINTER_P (x))

#define GET_INTEGER SCM_INUM 
#define MAKE_INTEGER  SCM_MAKINUM

#define MAKE_STRING(a) scm_mem2string(a,strlen(a))
#define EVAL(a) scm_eval_string(MAKE_STRING(a))

#define MAKE_SYM(a) gensym(SCM_SYMBOL_CHARS(a))

#define MAKE_POINTER(a) scm_ulong2num((unsigned long)a)
#define GET_POINTER(a) (void *)scm_num2ulong(a,0,"GET_POINTER()")

#define GET_X(a) ((t_snd_pd *)GET_POINTER(a))

#define RU_ return SCM_UNSPECIFIED



#include "m_pd.h"
#include <jack/ringbuffer.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>
#include <stdarg.h>


struct snd_pd_workaround;

typedef struct snd_pd
{
  t_object x_ob;

  int num_ins;
  int num_outs;

  struct snd_pd_workaround **inlets;
  t_outlet **outlets;

  SCM inlet_func;
  SCM cleanup_func;

  char *filename;

  bool isworking;
} t_snd_pd;

typedef struct snd_pd_workaround{
  t_object x_ob;
  t_snd_pd *x;
  t_inlet *inlet;
  int index;
  SCM func;
} t_snd_pd_workaround;




#define KG_MAX(a,b) (((a)>(b))?(a):(b))
#define KG_MIN(a,b) (((a)<(b))?(a):(b))



static char *version = "Snd " VERSION " made by Bill Schottstaedt, bil@ccrma.stanford.edu.\nSnd as a PD external made by Kjetil S. Matheussen, kjetil@ccrma.stanford.edu.";

static t_class *snd_pd_class, *snd_pd_workaroundclass;

static SCM pd_backtrace_run;
static SCM pd_backtrace_runx;
static SCM pd_backtrace_run1;
static SCM pd_backtrace_run2;
static SCM pd_backtrace_run3;
static SCM pd_backtrace_run4;
static SCM eval_string_func;

static t_clock *snd_pd_clock;

static pthread_t pthread;
static jack_ringbuffer_t *rb_snd_to_pd;
static jack_ringbuffer_t *rb_pd_to_snd;
static pthread_cond_t thread_cond={{0}};
static pthread_mutex_t thread_mutex={0};

struct dispatch{
  void (*func)(struct dispatch *d);
  t_snd_pd *x;
  union{
    char *filename;
    t_float number;
    int inlet_num;
    int outlet_num;
    t_class **class;
  }data;
  union{
    t_float val;
    int list_length;
    t_symbol *symbol;
    SCM func;
  }data2;
};


/*****************************************************************************************************
 *****************************************************************************************************
 *    Functions to evaluate large amount of scheme code from C.
 *****************************************************************************************************
 *****************************************************************************************************/

static char *evalstring=NULL;
static void snd0_eval2(char *string){
  char *new;
  if(evalstring==NULL){
    new=malloc(strlen(string)+1);
    sprintf(new,"%s",string);
  }else{
    new=malloc(strlen(evalstring)+strlen(string)+1);
    sprintf(new,"%s%s",evalstring,string);
    free(evalstring);
  }
  evalstring=new;
}
static void snd0_eval_file(FILE *file){
  char line[50000];
  for(;;){
    int c=fgetc(file);
    if(c==EOF) break;
    ungetc(c,file);
    fgets(line,49999,file);
    snd0_eval2(line);
  }
}
static SCM snd0_eval_do(void){
  //post(evalstring);
  SCM ret=EVAL(evalstring);
  free(evalstring);
  evalstring=NULL;
  return ret;
}




/*****************************************************************************************************
 *****************************************************************************************************
 *    Pd dispatcher. pd0_tick is called pretty often from pd (~1000 times per second) 
 *    and checks the snd->pd ringbuffer.
 *****************************************************************************************************
 *****************************************************************************************************/

void pd0_das_dispatcher(void *something){
  jack_ringbuffer_t *rb=rb_snd_to_pd;
  struct dispatch d;

  while(jack_ringbuffer_read_space(rb) >= sizeof(struct dispatch)){
    //printf("pd0: I got something to dispatch!\n");
    jack_ringbuffer_read (rb, (char *)&d, sizeof(struct dispatch));
    d.func(&d);
  }

  clock_delay(snd_pd_clock, 1.0);
}


static void snd0_send_message(struct dispatch *d){
  jack_ringbuffer_t *rb=rb_snd_to_pd;
  if(jack_ringbuffer_write (rb, (char*)d, sizeof(struct dispatch)) < sizeof(struct dispatch)){
    scm_gc();
  }else{
    while(jack_ringbuffer_write (rb, (char*)d, sizeof(struct dispatch)) < sizeof(struct dispatch)){
      post("Error. Ringbuffer full. Could not send message to PD. Trying again in one second.");
      sleep(1);
    }
  }
}



/*****************************************************************************************************
 *****************************************************************************************************
 *    Snd dispatcher
 *****************************************************************************************************
 *****************************************************************************************************/

static void snd0_das_dispatcher(void){
  jack_ringbuffer_t *rb=rb_pd_to_snd;
  struct dispatch d;
  while(1){
    //fprintf(stderr,"snd0: waiting for something to dispatch\n");
    while(jack_ringbuffer_read_space(rb) >= sizeof(struct dispatch)){
      //fprintf(stderr,"snd0: I got something to dispatch\n");
      jack_ringbuffer_read (rb, (char *)&d, sizeof(struct dispatch));
      d.func(&d);
    }
    pthread_cond_wait (&thread_cond, &thread_mutex);
  }
}

static void pd0_send_message(struct dispatch *d){
  jack_ringbuffer_t *rb=rb_pd_to_snd;

  if(jack_ringbuffer_write (rb, (char*)d, sizeof(struct dispatch)) < sizeof(struct dispatch))
    post("Error. Ringbuffer full. Could not send message to SND.");
  else{
    pthread_cond_broadcast(&thread_cond);
  }

}



/*****************************************************************************************************
 *****************************************************************************************************
 *    Got data from PD, on its way to Guile. Called either via bind or an inlet.
 *****************************************************************************************************
 *****************************************************************************************************/

static jack_ringbuffer_t *rb_anything;

static void snd0_anything_do(struct dispatch *d){
  int lokke;
  t_int argc;
  SCM applyarg=SCM_EOL;
  t_symbol *s;
  int index=d->data.inlet_num;


  jack_ringbuffer_read(rb_anything,(char *)&s,sizeof(t_symbol*));
  jack_ringbuffer_read(rb_anything,(char *)&argc,sizeof(t_int));

  for(lokke=argc-1;lokke>=0;lokke--){
    SCM to=SCM_BOOL_F;
    t_atom argv;
    jack_ringbuffer_read(rb_anything,(char *)&argv,sizeof(t_atom));
    switch(argv.a_type){
    case A_NULL:
      to=SCM_EOL;
      break;
    case A_FLOAT:
      to=scm_make_real(atom_getfloat(&argv));
      break;
    case A_SYMBOL:
      to=scm_string_to_symbol(MAKE_STRING(atom_getsymbol(&argv)->s_name));
      break;
    default:
      post("Strange");
      break;
    }
    applyarg=scm_cons(to,applyarg);
  }

  if(index>=0){
    // Inlet
    scm_call_4(pd_backtrace_run3,d->x->inlet_func,MAKE_INTEGER(index),scm_string_to_symbol(MAKE_STRING(s->s_name)),applyarg);
  }else{
    // Binding
    if(s!=&s_float && s!=&s_list && s!=&s_symbol){
      applyarg=scm_cons(scm_string_to_symbol(MAKE_STRING(s->s_name)),applyarg);
    }
    if(s!=&s_list && GET_INTEGER(scm_length(applyarg))==1)
      applyarg=SCM_CAR(applyarg);
    scm_call_2(pd_backtrace_run1,d->data2.func,applyarg);
  }
}

static void pd0_anything_do(t_snd_pd *x,int index,SCM func,t_symbol *s, t_int argc, t_atom* argv){
  int lokke;

  if(jack_ringbuffer_write_space(rb_anything) < sizeof(t_symbol*)+sizeof(t_int)+(sizeof(t_atom)*argc)){
    post("Error in function pd0_anything_do. Ringbuffer full. Can't send message to SND.");
    return;
  }

  // Yes, it can be done in three steps. (as long as pd is singlethreaded at least, but I think it is....)
  jack_ringbuffer_write(rb_anything,(char *)&s,sizeof(t_symbol*));
  jack_ringbuffer_write(rb_anything,(char *)&argc,sizeof(t_int));
  jack_ringbuffer_write(rb_anything,(char *)argv,sizeof(t_atom)*argc);

  {
    struct dispatch d;
    d.func=snd0_anything_do;
    d.x=x;
    d.data.inlet_num=index;
    d.data2.func=func;
    pd0_send_message(&d);
  }

}

// Handles inlet>0 and bindings
static void pd0_anything(t_snd_pd_workaround *x2,t_symbol *s, t_int argc, t_atom* argv){
  if(x2->x->isworking==false){
    post("Object not functional. No scheme file was loaded.");
    return;
  }

  if(x2->index>=0){
    // Inlet
    pd0_anything_do(x2->x,x2->index,0,s,argc,argv);
  }else{
    // Binding
    pd0_anything_do(NULL,x2->index,x2->func,s,argc,argv);
  }
}

// Handles first inlet
static void pd0_anything_first(t_snd_pd *x,t_symbol *s, t_int argc, t_atom* argv){
  if(x->isworking==false){
    post("Object not functional. No scheme file was loaded.");
    return;
  }

  pd0_anything_do(x,0,0,s,argc,argv);
}






/*****************************************************************************************************
 *****************************************************************************************************
 *    Initialization, called from the guile side.
 *****************************************************************************************************
 *****************************************************************************************************/

#if 0
static SCM snd0_inited_p(SCM instance){
  t_snd_pd *x=GET_X(instance);
  if(x->isinited==true) return SCM_BOOL_T;
  return SCM_BOOL_F;
}
#endif

static SCM snd0_get_num_inlets(SCM instance){
  t_snd_pd *x=GET_X(instance);
  return MAKE_INTEGER(x->num_ins);
}
static SCM snd0_get_num_outlets(SCM instance){
  t_snd_pd *x=GET_X(instance);
  return MAKE_INTEGER(x->num_outs);
}



/*****************************************************************************************************
 *****************************************************************************************************
 *    Binding and unbinding. Called from the guile side. 
 *****************************************************************************************************
 *****************************************************************************************************/

static SCM snd0_bind(SCM symname,SCM func){
  t_snd_pd_workaround *x2;
  x2=(t_snd_pd_workaround*)pd_new(snd_pd_workaroundclass);
  x2->index=-1;
  x2->func=func;
  scm_protect_object(x2->func);
  pd_bind((t_pd *)x2, MAKE_SYM(symname));
  return MAKE_POINTER(x2);
}
static SCM snd0_unbind(SCM scm_x2,SCM symname){
  t_snd_pd_workaround *x2=GET_POINTER(scm_x2);
  pd_unbind((t_pd *)x2,MAKE_SYM(symname));
  scm_unprotect_object(x2->func);
  pd_free((t_pd*)x2);
  RU_;
}






/*****************************************************************************************************
 *****************************************************************************************************
 *    Got data from the guile side. Distributing to outlets or receivers.
 *    The guile side is responsible for checking that the arguments are correct.
 *****************************************************************************************************
 *****************************************************************************************************/

#define GET_CLASS() (INTEGER_P(symbol)?(t_symbol*)GET_POINTER(symbol):MAKE_SYM(symbol))->s_thing
#define CLASS_INIT t_class **s=GET_CLASS();if(s==NULL) post("no receiver"); else 
#define GET_OUTLET() GET_X(instance)->outlets[GET_INTEGER(outlet)]


/* Number -> float */
static void pd0_outlet_number(struct dispatch *d){
  outlet_float(d->x->outlets[d->data.outlet_num],d->data2.val);
}

static SCM snd0_outlet_number(SCM instance,SCM outlet,SCM val){
  struct dispatch d;
  d.func=pd0_outlet_number;
  d.x=GET_X(instance);
  d.data.outlet_num=GET_INTEGER(outlet);
  d.data2.val=scm_num2dbl(val,"snd0_outlet_number:");
  snd0_send_message(&d);
  RU_;
}


static void pd0_send_number(struct dispatch *d){
  pd_float(d->data.class,d->data2.val);
}
static SCM snd0_send_number(SCM symbol,SCM val){
  CLASS_INIT
    {
      struct dispatch d;
      d.func=pd0_send_number;
      d.data.class=s;
      d.data2.val=scm_num2dbl(val,"snd0_send_number:");
      snd0_send_message(&d);
    }
  RU_;
}


static t_atom snd_pd_atom[5000]; // Should be huge enough.
static volatile int snd_pd_atom_in_use=0; // lock variable for snd_pd_atom.

/* List -> list */
static void snd0_make_list(SCM val){
  t_atom *atom=snd_pd_atom;
  int lokke;
  int length=GET_INTEGER(scm_length(val));

  for(lokke=0;lokke<length;lokke++){
    SCM el=scm_list_ref(val,MAKE_INTEGER(lokke));
    t_atom *to=&atom[lokke];
    if(SCM_INUMP(el)){
      SETFLOAT(to,(float)GET_INTEGER(el));
    }else{
      if(SCM_UNBNDP(el)){
	SETSYMBOL(to,gensym("undefined"));
      }else{
	if(SCM_STRINGP(el)){
	  SETSYMBOL(to,gensym(SCM_STRING_CHARS(el)));
	}else{
	  if(SCM_SYMBOLP(el)){
	    SETSYMBOL(to,MAKE_SYM(el));
	  }else{
	    if(scm_number_p(el)){
	      if(scm_real_p(el)){
		SETFLOAT(to,(float)scm_num2dbl(el,"snd0_outlet_or_send_list"));
	      }else{
		post("Illegal argument to gdp_outlet_or_send_list. Setting atom to 0.");
		SETFLOAT(to,0.0f);
	      }
	    }
	  }
	}
      }
    }
  }
}
static void pd0_outlet_list(struct dispatch *d){
  outlet_list(d->x->outlets[d->data.outlet_num], &s_list, d->data2.list_length, snd_pd_atom);
  snd_pd_atom_in_use=0;
}
static SCM snd0_outlet_list(SCM instance,SCM outlet,SCM val){
  if(snd_pd_atom_in_use==1){
    int num_retries=0;
    scm_gc();
    while(snd_pd_atom_in_use==1){
      num_retries++;
      if(num_retries>(1000000/50)*5){
	post("snd0_outlet_list: Waited 5 seconds for pd to process a list. Something is probably wrong. Could not send list to outlet.");
	RU_;
      }
      usleep(50);
    }
  }
  snd_pd_atom_in_use=1;
  {
    struct dispatch d;
    d.func=pd0_outlet_list;
    d.x=GET_X(instance);
    d.data.outlet_num=GET_INTEGER(outlet);
    d.data2.list_length=GET_INTEGER(scm_length(val));
    snd0_make_list(val);
    snd0_send_message(&d);
  }
  RU_;
}
static void pd0_send_list(struct dispatch *d){
  pd_list(d->data.class, &s_list, d->data2.list_length, snd_pd_atom);
  snd_pd_atom_in_use=0;
}
static SCM snd0_send_list(SCM symbol,SCM val){
  if(snd_pd_atom_in_use==1){
    int num_retries=0;
    scm_gc();
    while(snd_pd_atom_in_use==1){
      num_retries++;
      if(num_retries>(1000000/50)*5){
	post("snd0_send_list: Waited 5 seconds for pd to process a list. Something is probably wrong. Could not send list to receiver.");
	RU_;
      }
      usleep(50);
    }
  }
  snd_pd_atom_in_use=1;
  {
    CLASS_INIT
      {
	struct dispatch d;
	d.func=pd0_send_list;
	d.data.class=s;
	d.data2.list_length=GET_INTEGER(scm_length(val));
	snd0_make_list(val);
	snd0_send_message(&d);
      }
  }
  RU_;
}

/* Symbol -> symbol */
static void pd0_outlet_symbol(struct dispatch *d){
  outlet_symbol(d->x->outlets[d->data.outlet_num],d->data2.symbol);
}
static SCM snd0_outlet_symbol(SCM instance,SCM outlet,SCM val){
  struct dispatch d;
  d.func=pd0_outlet_symbol;
  d.x=GET_X(instance);
  d.data.outlet_num=GET_INTEGER(outlet);
  d.data2.symbol=MAKE_SYM(val);
  snd0_send_message(&d);
  RU_;
}
static void pd0_send_symbol(struct dispatch *d){
  pd_symbol(d->data.class,d->data2.symbol);
}
static SCM snd0_send_symbol(SCM symbol,SCM val){
  CLASS_INIT
    {
      struct dispatch d;
      d.func=pd0_send_symbol;
      d.data.class=s;
      d.data2.symbol=MAKE_SYM(val);
      snd0_send_message(&d);
    }
  RU_;
}

/* String -> symbol */
static SCM snd0_outlet_string(SCM instance,SCM outlet,SCM val){
  struct dispatch d;
  d.func=pd0_outlet_symbol;
  d.x=GET_X(instance);
  d.data.outlet_num=GET_INTEGER(outlet);
  d.data2.symbol=gensym(SCM_STRING_CHARS(val));
  snd0_send_message(&d);
  RU_;
}
static SCM snd0_send_string(SCM symbol,SCM val){
  CLASS_INIT
    {
      struct dispatch d;
      d.func=pd0_send_symbol;
      d.data.class=s;
      d.data2.symbol=gensym(SCM_STRING_CHARS(val));
      snd0_send_message(&d);
    }
  RU_;
}

/* Bang -> bang */
static void pd0_outlet_bang(struct dispatch *d){
  outlet_bang(d->x->outlets[d->data.outlet_num]);
}
static SCM snd0_outlet_bang(SCM instance,SCM outlet){
  struct dispatch d;
  d.func=pd0_outlet_bang;
  d.x=GET_X(instance);
  d.data.outlet_num=GET_INTEGER(outlet);
  snd0_send_message(&d);
  RU_;
}
static void pd0_send_bang(struct dispatch *d){
  pd_bang(d->data.class);
}
static SCM snd0_send_bang(SCM symbol){
  CLASS_INIT
    {
      struct dispatch d;
      d.func=pd0_send_bang;
      d.data.class=s;
      snd0_send_message(&d);
    }
  RU_;
}

/* <- symbol */
static SCM snd0_get_symbol(SCM symname){
  return MAKE_POINTER(MAKE_SYM(symname));
}



/*****************************************************************************************************
 *****************************************************************************************************
 *    Setting up global guile functions.
 *****************************************************************************************************
 *****************************************************************************************************/


static void *snd0_init(void *arg){
  char *command=
    //#include "global_scm.txt"

"(debug-enable 'debug)\n"
"(debug-enable 'trace)\n"
"(debug-enable 'backtrace)\n"
"(use-modules (ice-9 stack-catch))\n"
"(set! (show-backtrace) #t)\n"
"(debug-enable 'debug)\n"
"(if #t\n"
"    (begin\n"
"      (read-enable 'positions)\n"
"      (debug-enable 'debug)\n"
"      (debug-enable 'backtrace)\n"
"      (debug-set! frames 8)\n"
"      (debug-set! depth 50)))\n"
"(define (pd-load-if-exists filename)\n"
"  (if (access? filename F_OK)\n"
"      (load filename)))\n"
"(define (pd-display . args)\n"
"  (if (not (null? args))\n"
"      (begin\n"
"	(display (car args))\n"
"	(apply pd-display (cdr args)))\n"
"      (newline)))\n"
"(define (pd-filter proc list)\n"
"  (if (null? list)\n"
"      '()\n"
"      (if (proc (car list))\n"
"	  (cons (car list) (pd-filter proc (cdr list)))\n"
"	  (pd-filter proc (cdr list)))))\n"
"(define (pd-for init pred least add proc)\n"
"  (if (pred init least)\n"
"      (begin\n"
"	(proc init)\n"
"	(pd-for (+ add init) pred least add proc))))\n"
"(define (pd-check-number number message)\n"
"  (if (number? number)\n"
"      #t\n"
"      (begin\n"
"	(pd-display message \": \" number \" is not a number\")\n"
"	#f)))\n"
"(define pd-global-bindings '())\n"
"(define (pd-bind-do symbol func bindings)\n"
"  (if (or (not (symbol? symbol))\n"
"	  (not (procedure? func)))\n"
"      (begin\n"
"	(pd-display \"Wrong arguments for pd-bind\")\n"
"	bindings)\n"
"      (cons (list symbol \n"
"		  func\n"
"		  (pd-c-bind symbol func))\n"
"	    bindings)))\n"
"(define (pd-unbind-do symbol bindings)\n"
"  (if (not (symbol? symbol))\n"
"      (begin\n"
"	(pd-display \"Wrong arguments for pd-unbind\")\n"
"	bindings)\n"
"      (let ((binding (assq symbol bindings)))\n"
"	(pd-c-unbind (caddr binding) symbol)\n"
"	(pd-filter (lambda (x) (not (eq? symbol (car x))))\n"
"		   bindings))))\n"
"(define (pd-bind symbol func)\n"
"  (set! pd-global-bindings (pd-bind-do symbol func pd-global-bindings)))\n"
"(define (pd-unbind symbol)\n"
"  (set! pd-global-bindings (pd-unbind-do symbol pd-global-bindings)))\n"
"(define (pd-send symbol firstarg . args)\n"
"  (if (or (symbol? symbol)\n"
"	  (number? symbol))\n"
"      (cond ((> (length args) 0) (pd-c-send-list symbol (cons firstarg args)))\n"
"	    ((list? firstarg) (pd-c-send-list symbol firstarg))\n"
"	    ((number? firstarg) (pd-c-send-number symbol firstarg))\n"
"	    ((string? firstarg) (pd-c-send-string symbol firstarg))\n"
"	    ((eq? 'bang firstarg) (pd-c-send-bang symbol))\n"
"	    ((symbol? firstarg) (pd-c-send-symbol symbol firstarg))\n"
"	    (else\n"
"	     (pd-display \"Unknown argument to pd-outlet-or-send:\" firstarg)))))\n"
"(define (pd-get-symbol sym)\n"
"  (if (not (symbol? sym))\n"
"      (pd-display sym \" is not a scheme symbol\")\n"
"      (pd-c-get-symbol sym)))\n"
"(define (pd-backtrace-eval string)\n"
"  (eval-string string))\n"
"(define (pd-display-errorfunc key . args)\n"
"  (let ((dasstack (make-stack #t)))\n"
"    (display-backtrace dasstack (current-output-port) #f #f)\n"
"					;(display (stack-ref (make-stack #t) 1))\n"
"					;(display (stack-length (make-stack #t)))\n"
"    (display key)(newline)\n"
"    (display args)\n"
"    (newline))\n"
"  0)\n"
"(define (pd-backtrace-run thunk)\n"
"  (stack-catch #t\n"
"	       thunk\n"
"	       pd-display-errorfunc))\n"
"(define (pd-backtrace-runx func arg1) \n"
"  (stack-catch #t\n"
"	       (lambda x\n"
"		 (apply func x))\n"
"	       pd-display-errorfunc))\n"
"(define (pd-backtrace-run1 func arg1)\n"
"  (func arg1))\n"
"(define (pd-backtrace-run2 func arg1 arg2)\n"
"  (stack-catch #t\n"
"	       (lambda ()\n"
"		 (func arg1 arg2))\n"
"	       pd-display-errorfunc))\n"
"(define (pd-backtrace-run3 func arg1 arg2 arg3)\n"
"  (stack-catch #t\n"
"	       (lambda ()\n"
"		 (func arg1 arg2 arg3))\n"
"	       pd-display-errorfunc))\n"
"(define (pd-backtrace-run4 func arg1 arg2 arg3 arg4)\n"
"  (stack-catch #t\n"
"	       (lambda ()\n"
"		 (func arg1 arg2 arg3 arg4))\n"
"	       pd-display-errorfunc))\n"
"(pd-backtrace-run1 pd-load-if-exists \"/etc/.k_guile.scm\")\n"
"(pd-backtrace-run1 pd-load-if-exists (string-append (getenv \"HOME\") \"/.k_guile.scm\"))\n"

    ;

  scm_init_guile();
  snd_pd_main();

  //scm_c_define_gsubr("pd-c-inited?",1,0,0,snd0_inited_p);
  scm_c_define_gsubr("pd-c-get-num-inlets",1,0,0,snd0_get_num_inlets);
  scm_c_define_gsubr("pd-c-get-num-outlets",1,0,0,snd0_get_num_outlets);
  scm_c_define_gsubr("pd-c-bind",2,0,0,snd0_bind);
  scm_c_define_gsubr("pd-c-unbind",2,0,0,snd0_unbind);
  scm_c_define_gsubr("pd-c-outlet-number",3,0,0,snd0_outlet_number);
  scm_c_define_gsubr("pd-c-outlet-list",3,0,0,snd0_outlet_list);
  scm_c_define_gsubr("pd-c-outlet-symbol",3,0,0,snd0_outlet_symbol);
  scm_c_define_gsubr("pd-c-outlet-string",3,0,0,snd0_outlet_string);
  scm_c_define_gsubr("pd-c-outlet-bang",2,0,0,snd0_outlet_bang);
  scm_c_define_gsubr("pd-c-send-number",2,0,0,snd0_send_number);
  scm_c_define_gsubr("pd-c-send-list",2,0,0,snd0_send_list);
  scm_c_define_gsubr("pd-c-send-symbol",2,0,0,snd0_send_symbol);
  scm_c_define_gsubr("pd-c-send-string",2,0,0,snd0_send_string);
  scm_c_define_gsubr("pd-c-send-bang",1,0,0,snd0_send_bang);
  scm_c_define_gsubr("pd-c-get-symbol",1,0,0,snd0_get_symbol);

  EVAL(command);

  pd_backtrace_run=EVAL("pd-backtrace-run");
  scm_permanent_object(pd_backtrace_run);

  pd_backtrace_runx=EVAL("pd-backtrace-runx");
  scm_permanent_object(pd_backtrace_runx);

  pd_backtrace_run1=EVAL("pd-backtrace-run1");
  scm_permanent_object(pd_backtrace_run1);

  pd_backtrace_run2=EVAL("pd-backtrace-run2");
  scm_permanent_object(pd_backtrace_run2);

  pd_backtrace_run3=EVAL("pd-backtrace-run3");
  scm_permanent_object(pd_backtrace_run3);

  pd_backtrace_run4=EVAL("pd-backtrace-run4");
  scm_permanent_object(pd_backtrace_run4);

  eval_string_func=EVAL("eval-string");

  snd0_das_dispatcher();

  return NULL;
}

static void pd0_init(void){
  rb_snd_to_pd = jack_ringbuffer_create(1024);
  rb_pd_to_snd = jack_ringbuffer_create(1024);
  rb_anything  = jack_ringbuffer_create(1024);

  //scm_init_guile();

  if(pthread_create(&pthread,NULL,snd0_init,NULL)!=0){
    post("Could not make pthread. (disaster!)\n");
  }  
  return;
}




/*****************************************************************************************************
 *****************************************************************************************************
 *    Starting and stopping new guile script
 *****************************************************************************************************
 *****************************************************************************************************/

static void snd0_load(struct dispatch *d){
  SCM evalret;
  bool ret=false;
  
  FILE *file=fopen(d->data.filename,"r");
  if(file==NULL){
    post("file \"%s\" not found.\n",d->data.filename);
    d->x->isworking=false;  
    goto end;
  }
  d->x->isworking=true;


  // Let the file live in its own name-space (or something like that).
  snd0_eval2("(define (pd-instance-func pd-instance)");
  snd0_eval2(
	     //#include "local_scm.txt"
"(define pd-num-inlets (pd-c-get-num-inlets pd-instance))\n"
"(define pd-num-outlets (pd-c-get-num-outlets pd-instance))\n"
"(define (pd-legaloutlet outlet-num)\n"
"  (if (and (< outlet-num pd-num-outlets)\n"
"	   (>= outlet-num 0))\n"
"      #t\n"
"      (begin\n"
"	(pd-display \"outlet-num out of range\")\n"
"	#f)))\n"
"(define (pd-legalinlet inlet-num)\n"
"  (if (and (< inlet-num pd-num-inlets)\n"
"	   (>= inlet-num 0))\n"
"      #t\n"
"      (begin\n"
"	(pd-display \"inlet-num out of range\")\n"
"	#f)))\n"
"(define pd-inlet-vector (make-vector (pd-c-get-num-inlets pd-instance) '()))\n"
"(define pd-inlet-anyvector (make-vector 1 '()))\n"
"(define (pd-inlet-func inlet-num symbol args)\n"
"  (let ((inlet-func (assq symbol \n"
"			  (vector-ref pd-inlet-vector\n"
"				      inlet-num))))\n"
"    (if (not inlet-func)\n"
"	(begin\n"
"	  (set! inlet-func (assq 'any\n"
"				 (vector-ref pd-inlet-vector inlet-num)))\n"
"	  (set! args (cons symbol args))))\n"
"    (if inlet-func\n"
"	(apply (cadr inlet-func) args)\n"
"	(pd-display \"No function defined for handling <\" symbol \"> to inlet \" inlet-num))))\n"
"(define (pd-inlet inlet-num symbol func)\n"
"  (if (not (procedure? func))\n"
"      (pd-display \"Wrong argument to pd-inlet: \" func \" is not a procedure\")\n"
"      (if (and (pd-check-number inlet-num \"pd-inlet\")\n"
"	       (pd-legalinlet inlet-num))\n"
"	  (let ((inlet-funcs (vector-ref (if (eq? symbol 'any)\n"
"					     pd-inlet-anyvector\n"
"					     pd-inlet-vector)\n"
"					 inlet-num)))\n"
"	    (vector-set! pd-inlet-vector \n"
"			 inlet-num\n"
"			 (cons (list symbol func)\n"
"			       inlet-funcs))))))\n"
"(define (pd-inlets new-num-inlets)\n"
"  (pd-display \"ai\" new-num-inlets)\n"
"  (let ((num-inlets (if (pd-c-inited? pd-instance)\n"
"			(pd-c-get-num-inlets pd-instance)\n"
"			new-num-inlets)))\n"
"    (if (pd-check-number num-inlets \"pd-inlets\")\n"
"	(if (<= num-inlets 0)\n"
"	    (pd-display \"num-inlets must be greater than 0, not \" num-inlets)\n"
"	    (begin\n"
"	      (set! pd-num-inlets num-inlets)\n"
"	      (set! pd-inlet-vector (make-vector num-inlets '()))\n"
"	      (pd-c-inlets pd-instance num-inlets))))))\n"
"(define (pd-outlets new-num-outlets)\n"
"  (let ((num-outlets (if (pd-c-inited? pd-instance)\n"
"			 (pd-c-get-num-outlets pd-instance)\n"
"			 new-num-outlets)))\n"
"    (if (pd-check-number num-outlets \"pd-outlets\")\n"
"	(if (<= num-outlets 0)\n"
"	    (pd-display \"num-outlets must be greater than 0, not \" num-outlets)\n"
"	    (begin\n"
"	      (set! pd-num-outlets num-outlets)\n"
"	      (pd-c-outlets pd-instance num-outlets))))))\n"
"(define (pd-outlet outlet-num firstarg . args)\n"
"  (if (pd-legaloutlet outlet-num)\n"
"      (cond ((> (length args) 0) (pd-c-outlet-list pd-instance outlet-num issymbol (cons firstarg args)))\n"
"	    ((list? firstarg) (pd-c-outlet-list pd-instance outlet-num firstarg))\n"
"	    ((number? firstarg) (pd-c-outlet-number pd-instance outlet-num firstarg))\n"
"	    ((string? firstarg) (pd-c-outlet-string pd-instance outlet-num firstarg))\n"
"	    ((eq? 'bang firstarg) (pd-c-outlet-bang pd-instance outlet-num))\n"
"	    ((symbol? firstarg) (pd-c-outlet-symbol pd-instance outlet-num firstarg))\n"
"	    (else\n"
"	     (pd-display \"Unknown argument to pd-outlet-or-send:\" firstarg)))))\n"
"(define pd-local-bindings '())\n"
"(define (pd-bind symbol func)\n"
"  (set! pd-local-bindings (pd-bind-do symbol func pd-local-bindings)))\n"
"(define (pd-unbind symbol)\n"
"  (set! pd-local-bindings (pd-unbind-do symbol pd-local-bindings)))\n"
"(define (pd-unbind-all)\n"
"  (if (not (null? pd-local-bindings))\n"
"      (begin\n"
"	(pd-unbind (car (car pd-local-bindings)))\n"
"	(pd-unbind-all))))\n"
"(define pd-destroy-func #f)\n"
"(define (pd-set-destroy-func thunk)\n"
"  (if (not (procedure? thunk))\n"
"      (pd-display \"Wrong argument to pd-set-destroy-func: \" thunk \" is not a procedure.\")\n"
"      (set! pd-destroy-func thunk)))\n"
"(define (pd-cleanup-func)\n"
"  (if pd-destroy-func\n"
"      (begin\n"
"	(pd-destroy-func)\n"
"	(set! pd-destroy-func #f)))\n"
"  (pd-unbind-all))\n"

);
  snd0_eval_file(file);
  snd0_eval2("  (cons pd-inlet-func pd-cleanup-func))");
  snd0_eval2("1");

  if(1!=GET_INTEGER(snd0_eval_do())){
    post("Failed.");
    goto exit;
  }

  evalret=scm_call_2(pd_backtrace_run1,EVAL("pd-instance-func"),MAKE_POINTER(d->x));
  if(INTEGER_P(evalret)){
    post("Failed.");
    goto exit;
  }
  d->x->inlet_func=SCM_CAR(evalret);
  d->x->cleanup_func=SCM_CDR(evalret);
  scm_gc_protect_object(d->x->inlet_func);
  scm_gc_protect_object(d->x->cleanup_func);

  ret=true;

 exit:
  fclose(file);
 end:
  //  return ret;
  return;
}

static bool pd0_load(t_snd_pd *x,char *filename){
  struct dispatch d;
  d.func=snd0_load;
  d.x=x;
  d.data.filename=filename;
  pd0_send_message(&d);
  return true;
}

static void *pd0_new(t_symbol *s, t_int argc, t_atom* argv){
  int lokke;
  t_snd_pd *x;

  if(argc==0){
    post("Usage: snd [filename] <num_inlets> <num_outlets>");
    return NULL;
  }

  x= (t_snd_pd *)pd_new(snd_pd_class);
  //x->isinited=false;
  x->filename=atom_getsymbolarg(0,argc,argv)->s_name;


  /************** inlets ****************/
  x->num_ins=1;
  if(argc>1)
    x->num_ins=atom_getfloatarg(1,argc,argv);
  
  x->inlets=calloc(sizeof(t_snd_pd_workaround*),x->num_ins);
  for(lokke=1;lokke<x->num_ins;lokke++){
    t_snd_pd_workaround *x2;
    x2=(t_snd_pd_workaround*)pd_new(snd_pd_workaroundclass);
    x->inlets[lokke]=x2;
    x2->x=x;
    x2->index=lokke;
    x2->inlet=inlet_new(&x->x_ob,(t_pd*)x2,0,0);
  }
  
  /************* outlets ****************/
  x->num_outs=1;
  if(argc>2)
    x->num_outs=atom_getfloatarg(2,argc,argv);
  x->outlets=calloc(sizeof(t_outlet*),x->num_outs);
  
  for(lokke=0;lokke<x->num_outs;lokke++){
    x->outlets[lokke] = outlet_new(&x->x_ob, gensym("anything"));
  }


  if(pd0_load(x,x->filename)==true){
    //x->isinited=true;
    return x;
  }

  return NULL;
}

static void snd0_free(struct dispatch *d){
  if(d->x->isworking==true){
    scm_call_1(pd_backtrace_run,d->x->cleanup_func);
    scm_gc_unprotect_object(d->x->inlet_func);
    scm_gc_unprotect_object(d->x->cleanup_func);
  }

  free(d->x->inlets);
  free(d->x->outlets);
  free(d->x);
}

static void pd0_free(t_snd_pd *x){
  int lokke;
  struct dispatch d;
  t_snd_pd *new_x=malloc(sizeof(t_snd_pd));

  memcpy(new_x,x,sizeof(t_snd_pd));
  d.func=snd0_free;
  d.x=new_x;

  for(lokke=1;lokke<x->num_ins;lokke++){
    inlet_free(x->inlets[lokke]->inlet);
    pd_free((t_pd*)x->inlets[lokke]);
  }
  for(lokke=0;lokke<x->num_outs;lokke++){
    outlet_free(x->outlets[lokke]);
  }

  pd0_send_message(&d);
  
}


static void snd0_reload(struct dispatch *d){
  scm_call_1(pd_backtrace_run,d->x->cleanup_func);
  scm_gc_unprotect_object(d->x->inlet_func);
  scm_gc_unprotect_object(d->x->cleanup_func);
  snd0_load(d);
}

static void pd0_reload(t_snd_pd *x){
  struct dispatch d;

  if(x->isworking==false){
    post("Object not functional. No scheme file was loaded.");
    return;
  }

  d.func=snd0_reload;
  d.x=x;
  d.data.filename=x->filename;
  pd0_send_message(&d);
}

static void snd0_eval(struct dispatch *d){
  scm_call_2(pd_backtrace_run1,eval_string_func,MAKE_STRING(d->data2.symbol->s_name));
}

static void pd0_eval(t_snd_pd *x,t_symbol *s){
  struct dispatch d;
  d.func=snd0_eval;
  d.data2.symbol=s;
  pd0_send_message(&d);
}

//static void snd_pd_evalfile(t_snd_pd *x,t_symbol *s){
//}



static void pd0_at_exit(void){
  //fprintf(stderr,"upp\n");
}

static void finish(int sig){
  pd0_at_exit();
  exit(0);
}

/*****************************************************************************************************
 *****************************************************************************************************
 *    Das setup
 *****************************************************************************************************
 *****************************************************************************************************/
void snd_setup(void){ // (pd0_setup)

  pd0_init();

  snd_pd_class = class_new(gensym("snd"), (t_newmethod)pd0_new,
			   (t_method)pd0_free, sizeof(t_snd_pd), 0, A_GIMME, 0);

  class_addanything(snd_pd_class, (t_method)pd0_anything_first);
  class_addmethod(snd_pd_class, (t_method)pd0_reload, gensym("reload"), 0);
  class_addmethod(snd_pd_class, (t_method)pd0_eval, gensym("eval"), A_DEFSYM,0);
  //class_addmethod(snd_pd_class, (t_method)snd_pd_evalfile, gensym("evalfile"), A_DEFSYM,0);
  class_sethelpsymbol(snd_pd_class, gensym("help-snd_pd.pd"));


  /* This trick(?) is taken from the flext source. (I don't understand what happens...) */
  snd_pd_workaroundclass=class_new(gensym("indexworkaround"),NULL,NULL,sizeof(t_snd_pd_workaround),CLASS_PD|CLASS_NOINLET, A_NULL);
  class_addanything(snd_pd_workaroundclass,pd0_anything);

  snd_pd_clock = clock_new(NULL,(t_method)pd0_das_dispatcher);
  clock_delay(snd_pd_clock, 1.0);

  atexit(pd0_at_exit);
  signal(SIGINT,finish);
  post(version);
}


