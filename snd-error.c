#include "snd.h"

static char *snd_error_buffer = NULL;
#if HAVE_GUILE
  static int ignore_snd_error(char *msg);
  static int ignore_snd_warning(char *msg);
#endif

void snd_warning(char *format, ...)
{
  va_list ap;
  snd_info *sp;
  snd_state *ss;
  if (snd_error_buffer == NULL) 
    {
      snd_error_buffer = (char *)CALLOC(1024, sizeof(char));
      if (snd_error_buffer == NULL) 
	{
	  fprintf(stderr, "serious memory trouble!!"); 
	  return;
	}
    }
#if HAVE_VPRINTF
  va_start(ap, format);
  vsprintf(snd_error_buffer, format, ap);
  va_end(ap);
#if HAVE_GUILE
  if (ignore_snd_warning(snd_error_buffer)) return;
#endif
  ss = get_global_state();
  if (ss)
    {
      sp = any_selected_sound(ss);
      if (sp)
	report_in_minibuffer(sp, snd_error_buffer);
      else fprintf(stderr, snd_error_buffer);
    }
  else fprintf(stderr, snd_error_buffer);
#else
  fprintf(stderr, "warning...");
#endif
}

#ifdef SND_AS_WIDGET
static void (*snd_error_display)(const char *);

void set_snd_error_display (void (*func)(const char *))
{
  snd_error_display = func;
}
#endif

void snd_error(char *format, ...)
{
#if HAVE_VPRINTF
  va_list ap;
  snd_info *sp;
  snd_state *ss;
  if (snd_error_buffer == NULL) 
    {
      snd_error_buffer = (char *)CALLOC(1024, sizeof(char));
      if (snd_error_buffer == NULL) 
	{
	  fprintf(stderr, "serious memory trouble!!"); 
	  return;
	}
    }
  va_start(ap, format);
  vsprintf(snd_error_buffer, format, ap);
  va_end(ap);
#if HAVE_GUILE
  if (ignore_snd_error(snd_error_buffer)) return;
#endif
  ss = get_global_state();
  if ((ss) && (ss->sgx))
    {
#ifdef DEBUGGING
      fprintf(stderr, snd_error_buffer);
#endif

#if USE_NO_GUI
      /* this can happen if there's an error in the init file; after that the guile repl handles errors */
      fprintf(stderr, snd_error_buffer);
#else

#ifdef SND_AS_WIDGET
      if (snd_error_display) 
	snd_error_display(snd_error_buffer);
      else
	{
	  /* don't break (unlikely) existing code? */
#endif
      add_to_error_history(ss, snd_error_buffer, TRUE);
      sp = selected_sound(ss);
      if (sp)
	report_in_minibuffer(sp, snd_error_buffer);
      else post_error_dialog(ss, snd_error_buffer);
#ifdef SND_AS_WIDGET
	}
#endif
#endif
    }
  else 
    {
      fprintf(stderr, snd_error_buffer);
      fputc('\n', stderr);
    }
#else
  fprintf(stderr, "error...");
  fputc('\n', stderr);
#endif
}

/* -------------------------------- SCM connection -------------------------------- */
#if HAVE_GUILE

static SCM g_snd_error(SCM msg)
{
  #define H_snd_error "(" S_snd_error " str) reports error message str"
  SCM_ASSERT(gh_string_p(msg), msg, SCM_ARG1, S_snd_error);
  snd_error(TO_C_STRING(msg));
  return(msg);
}
  
static SCM g_snd_warning(SCM msg)
{
  #define H_snd_warning "(" S_snd_warning " str) reports warning message str"
  SCM_ASSERT(gh_string_p(msg), msg, SCM_ARG1, S_snd_warning);
  snd_warning(TO_C_STRING(msg));
  return(msg);
}
 
static SCM snd_error_hook, snd_warning_hook, mus_error_hook;

#if HAVE_HOOKS
int ignore_mus_error(int type, char *msg)
{
  SCM result = SCM_BOOL_F;
  if (HOOKED(mus_error_hook))
    result = g_c_run_or_hook(mus_error_hook, 
			     SCM_LIST2(TO_SCM_INT(type), 
				       TO_SCM_STRING(msg)));
  return(SCM_NFALSEP(result));
}

static int ignore_snd_error(char *msg)
{
  SCM result = SCM_BOOL_F;
  if (HOOKED(snd_error_hook))
    result = g_c_run_or_hook(snd_error_hook, 
			     SCM_LIST1(TO_SCM_STRING(msg)));
  return(SCM_NFALSEP(result));
}

static int ignore_snd_warning(char *msg)
{
  SCM result = SCM_BOOL_F;
  if (HOOKED(snd_warning_hook))
    result = g_c_run_or_hook(snd_warning_hook, 
			     SCM_LIST1(TO_SCM_STRING(msg)));
  return(SCM_NFALSEP(result));
}

#else
int ignore_mus_error(int type, char *msg) {return(0);}
static int ignore_snd_error(char *msg) {return(0);}
static int ignore_snd_warning(char *msg) {return(0);}
#endif

void g_init_errors(SCM local_doc)
{
  DEFINE_PROC(gh_new_procedure1_0(S_snd_error, g_snd_error), H_snd_error);
  DEFINE_PROC(gh_new_procedure1_0(S_snd_warning, g_snd_warning), H_snd_warning);

  #define H_mus_error_hook S_mus_error_hook " (error-type error-message) is called upon mus_error. \
If it returns #t, Snd ignores the error (it assumes you've handled it via the hook)."

  #define H_snd_error_hook S_snd_error_hook " (error-message) is called upon snd_error. \
If it returns #t, Snd flushes the error (it assumes you've reported it via the hook:\n\
  (add-hook! snd-error-hook\n\
    (lambda (msg) (play \"bong.snd\") #f))"

  #define H_snd_warning_hook S_snd_warning_hook " (warning-message) is called upon snd_warning. \
If it returns #t, Snd flushes the warning (it assumes you've reported it via the hook):\n\
  (define without-warnings\n\
    (lambda (thunk)\n\
      (define no-warning (lambda (msg) #t))\n\
      (add-hook! snd-warning-hook no-warning)\n\
      (thunk)\n\
      (remove-hook! snd-warning-hook no-warning)))"

  mus_error_hook =   MAKE_HOOK(S_mus_error_hook, 2, H_mus_error_hook);     /* arg = error-type error-message */
  snd_error_hook =   MAKE_HOOK(S_snd_error_hook, 1, H_snd_error_hook);     /* arg = error-message */
  snd_warning_hook = MAKE_HOOK(S_snd_warning_hook, 1, H_snd_warning_hook); /* arg = error-message */
}

#endif
