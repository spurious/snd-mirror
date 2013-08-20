;;; libpthread.scm
;;;
;;; tie the pthread library into the *libpthread* environment

;;; pthread_t: unsigned long
;;; pthread_once_t: int
;;; pthread_key_t: unsigned int

(if (not (provided? 'cload.scm)) (load "cload.scm"))
(provide 'libpthread.scm)

(if (not (defined? '*libpthread*))
    (define-constant *libpthread*
      (with-environment (initial-environment)
	
	(c-define '(
		    (C-macro (int (PTHREAD_CREATE_JOINABLE PTHREAD_CREATE_DETACHED
				   PTHREAD_MUTEX_DEFAULT PTHREAD_MUTEX_NORMAL
				   PTHREAD_MUTEX_STALLED PTHREAD_MUTEX_ROBUST
				   PTHREAD_INHERIT_SCHED PTHREAD_EXPLICIT_SCHED
				   PTHREAD_SCOPE_SYSTEM PTHREAD_SCOPE_PROCESS
				   PTHREAD_PROCESS_PRIVATE PTHREAD_PROCESS_SHARED
				   PTHREAD_CANCEL_ENABLE PTHREAD_CANCEL_DISABLE PTHREAD_CANCEL_DEFERRED PTHREAD_CANCEL_ASYNCHRONOUS)))

		    (C-macro (void* PTHREAD_CANCELED))

		    (int pthread_attr_destroy (pthread_attr_t*))
		    (int pthread_attr_init (pthread_attr_t*))
		    (int pthread_cond_broadcast (pthread_cond_t*))
		    (int pthread_cond_destroy (pthread_cond_t*))
		    (int pthread_cond_signal (pthread_cond_t*))
		    (int pthread_condattr_destroy (pthread_condattr_t*))
		    (int pthread_condattr_init (pthread_condattr_t*))
		    (int pthread_mutex_destroy (pthread_mutex_t*))
		    (int pthread_mutex_lock (pthread_mutex_t*))
		    (int pthread_mutex_trylock (pthread_mutex_t*))
		    (int pthread_mutex_unlock (pthread_mutex_t*))
		    (int pthread_mutexattr_destroy (pthread_mutexattr_t*))
		    (int pthread_mutexattr_init (pthread_mutexattr_t*))
		    (void pthread_exit (void*))

		    (int pthread_attr_setdetachstate (pthread_attr_t* int))
		    (int pthread_attr_setinheritsched (pthread_attr_t* int))
		    (int pthread_attr_setschedpolicy (pthread_attr_t* int))
		    (int pthread_attr_setscope (pthread_attr_t* int))
		    (int pthread_attr_setstacksize (pthread_attr_t* int)) ; size_t
		    (int pthread_condattr_setpshared (pthread_condattr_t* int))
		    (int pthread_mutexattr_setprioceiling (pthread_mutexattr_t* int))
		    (int pthread_mutexattr_setprotocol (pthread_mutexattr_t* int))
		    (int pthread_mutexattr_setpshared (pthread_mutexattr_t* int))

;; (not in OSX)	    (int pthread_yield (void))
		    (pthread_t pthread_self (void))
		    (void pthread_testcancel (void))
		    (int pthread_cancel ((pthread_t long)))
		    (int pthread_detach ((pthread_t long)))
		    (int pthread_equal ((pthread_t long) (pthread_t long)))

		    (int pthread_key_delete ((pthread_key_t int)))
;; (not in OSX)	    (int pthread_setschedprio ((pthread_t long) int))

		    (in-C "
static s7_pointer s7_pthread_error(s7_scheme *sc, int err, const char *caller)\n\
{\n\
   s7_error(sc, \n\
            s7_make_symbol(sc, \"pthread-error\"), \n\
            s7_list(sc, 3, s7_make_string(sc, \"~A: ~A\"), \n\
                           s7_make_string(sc, caller), \n\
                           s7_make_string(sc, strerror(err))));\n\
  return(s7_f(sc));\n\
}\n\
#define INT_FUNC_WITH_REF_INT(Name, Type) \\\n\
  static s7_pointer Name(s7_scheme *sc, s7_pointer args) \\\n\
  { \\\n\
   int err, ref = 0; \\\n\
   if (!s7_is_c_pointer(s7_car(args))) \\\n\
     return(s7_wrong_type_arg_error(sc, \"pthread_\" #Name, 1, s7_car(args), \"c_pointer\")); \\\n\
   if ((err = pthread_ ## Name ((Type *)s7_c_pointer(s7_car(args)), &ref)) != 0) \\\n\
     s7_pthread_error(sc, err, \"pthread_\" #Name);\\\n\
   return(s7_make_integer(sc, ref)); \\\n\
  } \n\n\
INT_FUNC_WITH_REF_INT(attr_getdetachstate, pthread_attr_t)\n\
INT_FUNC_WITH_REF_INT(attr_getinheritsched, pthread_attr_t)\n\
INT_FUNC_WITH_REF_INT(attr_getschedpolicy, pthread_attr_t)\n\
INT_FUNC_WITH_REF_INT(attr_getscope, pthread_attr_t)\n\
INT_FUNC_WITH_REF_INT(mutexattr_getprioceiling, pthread_mutexattr_t)\n\
INT_FUNC_WITH_REF_INT(mutexattr_getprotocol, pthread_mutexattr_t)\n\
INT_FUNC_WITH_REF_INT(mutexattr_getpshared, pthread_mutexattr_t)\n\
INT_FUNC_WITH_REF_INT(condattr_getpshared, pthread_condattr_t)\n\
")
		    (C-function ("pthread_attr_getdetachstate" attr_getdetachstate "" 1))
		    (C-function ("pthread_attr_getinheritsched" attr_getinheritsched "" 1))
		    (C-function ("pthread_attr_getschedpolicy" attr_getschedpolicy "" 1))
		    (C-function ("pthread_attr_getscope" attr_getscope "" 1))
		    (C-function ("pthread_mutexattr_getprioceiling" mutexattr_getprioceiling "" 1))
		    (C-function ("pthread_mutexattr_getprotocol" mutexattr_getprotocol "" 1))
		    (C-function ("pthread_mutexattr_getprshared" mutexattr_getpshared "" 1))
		    (C-function ("pthread_condattr_getprshared" condattr_getpshared "" 1))
		    (in-C "
static s7_pointer g_pthread_key_create(s7_scheme *sc, s7_pointer args)
{
  int err;
  pthread_key_t key;
  /* TODO: if (pair? args) (car args) is a function 1-arg -> destructor
  */
  if ((err = pthread_key_create(&key, NULL)) != 0) 
    return(s7_pthread_error(sc, err, \"pthread_key_create\"));
  return(s7_make_integer(sc, (s7_Int)key));
}

static s7_pointer g_pthread_setcancelstate(s7_scheme *sc, s7_pointer args)
{
  int err, oldstate = 0;
  if (!s7_is_integer(s7_car(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_setcancelstate\", 1, s7_car(args), \"an integer\"));
  if ((err = pthread_setcancelstate((int)s7_integer(s7_car(args)), &oldstate)) != 0)
    return(s7_pthread_error(sc, err, \"pthread_setcancelstate\"));
  return(s7_make_integer(sc, (s7_Int)oldstate));
}

static s7_pointer g_pthread_setcanceltype(s7_scheme *sc, s7_pointer args)
{
  int err, oldtype = 0;
  if (!s7_is_integer(s7_car(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_setcanceltype\", 1, s7_car(args), \"an integer\"));
  if ((err = pthread_setcanceltype((int)s7_integer(s7_car(args)), &oldtype)) != 0)
    return(s7_pthread_error(sc, err, \"pthread_setcanceltype\"));
  return(s7_make_integer(sc, (s7_Int)oldtype));
}

static s7_pointer g_pthread_setspecific(s7_scheme *sc, s7_pointer args)
{
  pthread_key_t key;
  void *val; 
  if (!s7_is_integer(s7_car(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_setspecific\", 1, s7_car(args), \"an integer\"));
  key = (pthread_key_t)s7_integer(s7_car(args));
  if (!s7_is_string(s7_cadr(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_setspecific\", 2, s7_cadr(args), \"a string\"));
  val = (void *)s7_string(s7_cadr(args));
  return(s7_make_integer(sc, (s7_Int)pthread_setspecific(key, val)));
}

static s7_pointer g_pthread_getspecific(s7_scheme *sc, s7_pointer args)
{
  pthread_key_t key;
  void *val;
  if (!s7_is_integer(s7_car(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_getspecific\", 1, s7_car(args), \"an integer\"));
  key = (pthread_key_t)s7_integer(s7_car(args));
  val = (void *)pthread_getspecific(key);
  if (val)
    return(s7_make_string(sc, (const char *)val));
  return(s7_f(sc));
}

static s7_pointer g_pthread_join(s7_scheme *sc, s7_pointer args)
{
  void *val = NULL;
  int err;
  if (!s7_is_integer(s7_car(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_join\", 1, s7_car(args), \"an integer\"));
  if ((err = pthread_join((pthread_t)s7_integer(s7_car(args)), &val)) != 0)
    return(s7_pthread_error(sc, err, \"pthread_join\"));
  if (val != NULL)
    return(s7_object_to_string(sc, val, true)); /* TODO: actually we want :readable here */
  return(s7_f(sc));
}

static pthread_key_t s7_key;
static s7_scheme **s7_pool = NULL;
static bool *s7_in_use = NULL;
static int s7_pool_size = 0;

static void s7_all_done(void *s)
{
  int i;
  s7_scheme *sc = (s7_scheme *)s;
  for (i = 0; i < s7_pool_size; i++)
    if (s7_pool[i] == sc)
      {
        s7_in_use[i] = false;
	break;
      }
}

static void *start_routine(void *arg)
{
  s7_scheme *sc = NULL;
  s7_pointer result;
  int i, loc;
  if (s7_pool_size == 0)
    {
      s7_pool_size = 4;
      s7_pool = (s7_scheme **)calloc(s7_pool_size, sizeof(s7_scheme *));
      s7_in_use = (bool *)calloc(s7_pool_size, sizeof(bool));
      sc = s7_init();
      s7_pool[0] = sc;
      s7_in_use[0] = true;
      loc = 0;
      pthread_key_create(&s7_key, s7_all_done);
    }
  else
    {
      int i;
      for (i = 0; i < s7_pool_size; i++)
        if (!s7_in_use[i])
          {
            sc = s7_pool[i];
            s7_in_use[i] = true;
            if (sc == NULL)
	      {
		sc = s7_init();
                s7_pool[i] = sc;
                loc = i;
		break;
	      }
          }
       if (!sc)
         {
	   int loc;
           loc = s7_pool_size;
           s7_pool_size *= 2;
           s7_pool = (s7_scheme **)realloc(s7_pool, s7_pool_size * sizeof(s7_scheme *));
           s7_in_use = (bool *)realloc(s7_in_use, s7_pool_size * sizeof(bool));
           for (i = loc; i < s7_pool_size; i++) {s7_pool[i] = NULL; s7_in_use[i] = false;}
           sc = s7_init();
           s7_pool[loc] = sc;
           s7_in_use[loc] = true;
	 }
    }
  pthread_setspecific(s7_key, (void *)sc);
  result = s7_eval_c_string(sc, (const char *)arg);
  s7_in_use[loc] = false;
  return((void *)result);
}

static s7_scheme *current_scheme(void)
{
  return((s7_scheme *)pthread_getspecific(s7_key));
}

static s7_pointer g_pthread_create(s7_scheme *sc, s7_pointer args)
{
  int err;
  pthread_t pt;
  pthread_attr_t *attr = NULL;
  void *arg = NULL;

  if (s7_car(args) != s7_f(sc))
    {
      if (!s7_is_c_pointer(s7_car(args)))
        return(s7_wrong_type_arg_error(sc, \"pthread_create\", 1, s7_car(args), \"a raw pthread_attr_t pointer or #f\"));
      attr = (pthread_attr_t *)s7_c_pointer(s7_car(args));
    }
  if (!s7_is_string(s7_cadr(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_create\", 2, s7_cadr(args), \"a string\"));
  arg = (void *)s7_string(s7_cadr(args));
  
  if ((err = pthread_create(&pt, attr, start_routine, arg)) != 0)
    return(s7_pthread_error(sc, err, \"pthread_create\"));
  return(s7_make_integer(sc, (s7_Int)(pt)));
}

static s7_pointer g_pthread_attr_setschedparam(s7_scheme *sc, s7_pointer args)
{
  /* pass in an int! */
  struct sched_param sp;
  int err;
  pthread_attr_t *attr = NULL;

  if (!s7_is_c_pointer(s7_car(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_attr_setschedparam\", 1, s7_car(args), \"a raw pthread_attr_t pointer\"));
  attr = (pthread_attr_t *)s7_c_pointer(s7_car(args));

  if (!s7_is_integer(s7_cadr(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_attr_setschedparam\", 2, s7_cadr(args), \"an integer\"));
  sp.sched_priority = (int)s7_integer(s7_cadr(args));
  if ((err = pthread_attr_setschedparam(attr, &sp)) != 0)
    return(s7_pthread_error(sc, err, \"pthread_attr_setschedparam\"));
  return(s7_cadr(args));
}

static s7_pointer g_pthread_attr_getschedparam(s7_scheme *sc, s7_pointer args)
{
  struct sched_param sp;
  int err;
  pthread_attr_t *attr;

  if (!s7_is_c_pointer(s7_car(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_attr_getschedparam\", 1, s7_car(args), \"a raw pthread_attr_t pointer\"));
  attr = (pthread_attr_t *)s7_c_pointer(s7_car(args));

  if ((err = pthread_attr_getschedparam(attr, &sp)) != 0)
    return(s7_pthread_error(sc, err, \"pthread_attr_getschedparam\"));
  return(s7_make_integer(sc, sp.sched_priority));
}

static s7_pointer g_pthread_setschedparam(s7_scheme *sc, s7_pointer args)
{
  /* pass in an int! */
  struct sched_param sp;
  int err;

  if (!s7_is_integer(s7_car(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_setschedparam\", 1, s7_car(args), \"an integer\"));
  if (!s7_is_integer(s7_cadr(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_setschedparam\", 2, s7_cadr(args), \"an integer\"));
  if (!s7_is_integer(s7_caddr(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_setschedparam\", 3, s7_caddr(args), \"an integer\"));

  sp.sched_priority = (int)s7_integer(s7_caddr(args));
  if ((err = pthread_setschedparam((pthread_t)s7_integer(s7_car(args)), (int)s7_integer(s7_cadr(args)), &sp)) != 0)
    return(s7_pthread_error(sc, err, \"pthread_setschedparam\"));
  return(s7_cadr(args));
}

static s7_pointer g_pthread_getschedparam(s7_scheme *sc, s7_pointer args)
{
  struct sched_param sp;
  int err, policy = 0;

  if (!s7_is_integer(s7_car(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_getschedparam\", 1, s7_car(args), \"an integer\"));

  if ((err = pthread_getschedparam((pthread_t)s7_integer(s7_car(args)), &policy, &sp)) != 0)
    return(s7_pthread_error(sc, err, \"pthread_getschedparam\"));
  return(s7_list(sc, 2, s7_make_integer(sc, policy), s7_make_integer(sc, sp.sched_priority)));
}

static s7_pointer g_pthread_attr_getstacksize(s7_scheme *sc, s7_pointer args)
{
  int err;
  size_t size;
  pthread_attr_t *attr = NULL;
  
  if (!s7_is_c_pointer(s7_car(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_attr_getstacksize\", 1, s7_car(args), \"a raw pthread_attr_t pointer\"));
  attr = (pthread_attr_t *)s7_c_pointer(s7_car(args));

  if ((err = pthread_attr_getstacksize(attr, &size)) != 0)
    return(s7_pthread_error(sc, err, \"pthread_attr_getstacksize\"));
  return(s7_make_integer(sc, size));
}

static s7_pointer g_pthread_attr_setstacksize(s7_scheme *sc, s7_pointer args)
{
  int err;
  size_t size;
  pthread_attr_t *attr = NULL;
  
  if (!s7_is_c_pointer(s7_car(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_attr_setstacksize\", 1, s7_car(args), \"a raw pthread_attr_t pointer\"));
  attr = (pthread_attr_t *)s7_c_pointer(s7_car(args));

  if (!s7_is_integer(s7_cadr(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_attr_setstacksize\", 2, s7_cadr(args), \"an integer\"));
  size = (size_t)s7_integer(s7_cadr(args));
    
  if ((err = pthread_attr_setstacksize(attr, size)) != 0)
    return(s7_pthread_error(sc, err, \"pthread_attr_setstacksize\"));
  return(s7_cadr(args));
}

static s7_pointer g_make_pthread_mutex(s7_scheme *sc, s7_pointer args)
{
  pthread_mutex_t *m;
  m = (pthread_mutex_t *)calloc(1, sizeof(pthread_mutex_t));
  pthread_mutex_init(m, NULL);
  return(s7_make_c_pointer(sc, (void *)m));
}

static s7_pointer g_make_pthread_cond(s7_scheme *sc, s7_pointer args)
{
  pthread_cond_t *m;
  m = (pthread_cond_t *)calloc(1, sizeof(pthread_cond_t));
  pthread_cond_init(m, NULL);
  return(s7_make_c_pointer(sc, (void *)m));
}

static s7_pointer g_pthread_cond_wait(s7_scheme *sc, s7_pointer args)
{
  int err;
  pthread_cond_t *cond;
  pthread_mutex_t *m;

  if (!s7_is_c_pointer(s7_car(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_cond_wait\", 1, s7_car(args), \"a raw pthread_cond_t pointer\"));
  cond = (pthread_cond_t *)s7_c_pointer(s7_car(args));
  if (!s7_is_c_pointer(s7_cadr(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_cond_wait\", 2, s7_cadr(args), \"a raw pthread_mutex_t pointer\"));
  m = (pthread_mutex_t *)s7_c_pointer(s7_cadr(args));
  if ((err = pthread_cond_wait(cond, m)) != 0)
    return(s7_pthread_error(sc, err, \"pthread_cond_wait\"));  
  return(s7_car(args));
}

static s7_pointer g_pthread_cond_timedwait(s7_scheme *sc, s7_pointer args)
{
  int err;
  pthread_cond_t *cond;
  pthread_mutex_t *m;
  struct timespec *st;

  if (!s7_is_c_pointer(s7_car(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_cond_timedwait\", 1, s7_car(args), \"a raw pthread_cond_t pointer\"));
  cond = (pthread_cond_t *)s7_c_pointer(s7_car(args));
  if (!s7_is_c_pointer(s7_cadr(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_cond_timedwait\", 2, s7_cadr(args), \"a raw pthread_mutex_t pointer\"));
  if (!s7_is_c_pointer(s7_caddr(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_cond_timedwait\", 3, s7_caddr(args), \"a raw timespec pointer\"));
  st = (struct timespec *)s7_c_pointer(s7_caddr(args));

  if ((err = pthread_cond_timedwait(cond, m, st)) != 0)
    return(s7_pthread_error(sc, err, \"pthread_cond_timedwait\"));  
  return(s7_car(args));
}

static s7_pointer g_pthread_yield(s7_scheme *sc, s7_pointer args)
{
#if (!__APPLE__)
  pthread_yield(); /* in linux this returns an error indication, but not elsewhere, so ignore it */
#endif
  return(s7_f(sc));
}

/* TODO: *bsd also doesn't have the 4 pshared guys or setschedprio
*/

static s7_pointer g_pthread_setschedprio(s7_scheme *sc, s7_pointer args)
{
#if (!__APPLE__)
  int err;
  if (!s7_is_integer(s7_car(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_setschedprio\", 1, s7_car(args), \"an integer\"));
  if (!s7_is_integer(s7_cadr(args)))
    return(s7_wrong_type_arg_error(sc, \"pthread_setschedprio\", 2, s7_cadr(args), \"an integer\"));
  if ((err = pthread_setschedprio((pthread_t)s7_integer(s7_car(args)), (int)s7_integer(s7_cadr(args)))) != 0)
    return(s7_pthread_error(sc, err, \"pthread_setschedprio\"));
#endif
  return(s7_f(sc));
}
")
                    (C-function ("pthread_key_create" g_pthread_key_create "" 1 1))
                    (C-function ("pthread_setcancelstate" g_pthread_setcancelstate "" 1 0))
                    (C-function ("pthread_setcanceltype" g_pthread_setcanceltype "" 1 0))
                    (C-function ("pthread_setspecific" g_pthread_setspecific "" 2 0))
                    (C-function ("pthread_getspecific" g_pthread_getspecific "" 1 0))
                    (C-function ("pthread_join" g_pthread_join "" 1 0))
                    (C-function ("pthread_create" g_pthread_create "" 2 0))
                    (C-function ("pthread_attr_getschedparam" g_pthread_attr_getschedparam "" 1 0))
                    (C-function ("pthread_attr_setschedparam" g_pthread_attr_setschedparam "" 2 0))
                    (C-function ("pthread_getschedparam" g_pthread_getschedparam "" 1 0))
                    (C-function ("pthread_setschedparam" g_pthread_setschedparam "" 3 0))
                    (C-function ("pthread_attr_getstacksize" g_pthread_attr_getstacksize "" 1 0))
                    (C-function ("pthread_attr_setstacksize" g_pthread_attr_setstacksize "" 2 0))
                    (C-function ("make_pthread_mutex" g_make_pthread_mutex "(make_pthread_mutex) returns a new, initialized mutex variable" 0))
                    (C-function ("make_pthread_cond" g_make_pthread_cond "(make_pthread_cond) returns a new, initialized condition variable" 0))
                    (C-function ("pthread_cond_wait" g_pthread_cond_wait "" 2))
                    (C-function ("pthread_cond_timedwait" g_pthread_cond_timedwait "" 3))
                    (C-function ("pthread_yield" g_pthread_yield "" 0))
                    (C-function ("pthread_setschedprio" g_pthread_setschedprio "" 2))
		    )
		  "" "pthread.h" "-pthread" "-lpthread" "libpthread_s7")
	
	(current-environment))))

(define pthread_create (*libpthread* 'pthread_create))
(define pthread_join (*libpthread* 'pthread_join))

*libpthread*


#|
(define p ((*libpthread* 'pthread_create) #f "(let () (format *stderr* \"hiho~%\"))"))
((*libpthread* 'pthread_join) p)

|#

;;; TODO: libpthread help strings, doc/test, tie into ws somehow
