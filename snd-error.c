#include "snd.h"

/* these are needed as C ints below */
#ifndef DEBUGGING
  #define DEBUGGING 0
#endif
#ifndef USE_NO_GUI
  #define USE_NO_GUI 0
#endif

#define SND_ERROR_BUFFER_SIZE 1024
static char *snd_error_buffer = NULL;

static XEN snd_error_hook; 
static XEN snd_warning_hook; 

void snd_warning(char *format, ...)
{
  va_list ap;
  snd_info *sp;
  snd_state *ss;
  if (snd_error_buffer == NULL) 
    snd_error_buffer = (char *)CALLOC(SND_ERROR_BUFFER_SIZE, sizeof(char));
#if HAVE_VPRINTF
  va_start(ap, format);
#if HAVE_VSNPRINTF
  vsnprintf(snd_error_buffer, SND_ERROR_BUFFER_SIZE, format, ap);
#else
  vsprintf(snd_error_buffer, format, ap);
#endif
  va_end(ap);
  if ((XEN_HOOKED(snd_warning_hook)) &&
      (XEN_NOT_FALSE_P(run_or_hook(snd_warning_hook, 
				   XEN_LIST_1(C_TO_XEN_STRING(snd_error_buffer)),
				   S_snd_warning_hook))))
    return;
  ss = get_global_state();
  if ((ss) && (!(ss->batch_mode)))
    {
      sp = any_selected_sound(ss);
      if ((sp) && (sp->active))
	report_in_minibuffer(sp, snd_error_buffer);
      else fprintf(stderr, snd_error_buffer);
    }
  else fprintf(stderr, snd_error_buffer);
#if DEBUGGING
  fprintf(stderr, snd_error_buffer);
#endif
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

static int direct_snd_error_call = FALSE;

void snd_error(char *format, ...)
{
#if HAVE_VPRINTF
  va_list ap;
  snd_info *sp;
  snd_state *ss;
  if (snd_error_buffer == NULL) 
    snd_error_buffer = (char *)CALLOC(SND_ERROR_BUFFER_SIZE, sizeof(char));
  va_start(ap, format);
#if HAVE_VSNPRINTF
  vsnprintf(snd_error_buffer, SND_ERROR_BUFFER_SIZE, format, ap);
#else
  vsprintf(snd_error_buffer, format, ap);
#endif
  va_end(ap);
  if ((XEN_HOOKED(snd_error_hook)) &&
      (XEN_NOT_FALSE_P(run_or_hook(snd_error_hook, 
				   XEN_LIST_1(C_TO_XEN_STRING(snd_error_buffer)),
				   S_snd_error_hook))))
    return;
  ss = get_global_state();
  if ((ss) && (ss->sgx))
    {
      if ((DEBUGGING) || (USE_NO_GUI) || (ss->batch_mode))
	fprintf(stderr, snd_error_buffer);

#ifdef SND_AS_WIDGET
      if (snd_error_display) 
	snd_error_display(snd_error_buffer);
      else
	{
	  /* don't break (unlikely) existing code? */
#endif
      add_to_error_history(ss, snd_error_buffer, TRUE);
      sp = selected_sound(ss);
      ss->catch_message = snd_error_buffer;
      if ((direct_snd_error_call) ||
	  (ss->catch_exists == 0))
	{
	  if ((sp) && (sp->active))
	    report_in_minibuffer(sp, snd_error_buffer);
	  else post_error_dialog(ss, snd_error_buffer);
	}
#ifdef SND_AS_WIDGET
	}
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

static XEN g_snd_error(XEN msg)
{
  #define H_snd_error "(" S_snd_error " str): reports error message str (normally in the error dialog)"
  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, XEN_ONLY_ARG, S_snd_error, "a string");
  direct_snd_error_call = TRUE;
  snd_error(XEN_TO_C_STRING(msg));
  direct_snd_error_call = FALSE;
  return(msg);
}
  
static XEN g_snd_warning(XEN msg)
{
  #define H_snd_warning "(" S_snd_warning " str): reports warning message str (normally in the minibuffer)"
  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, XEN_ONLY_ARG, S_snd_warning, "a string");
  snd_warning(XEN_TO_C_STRING(msg));
  return(msg);
}
 
#ifdef XEN_ARGIFY_1
XEN_NARGIFY_1(g_snd_error_w, g_snd_error)
XEN_NARGIFY_1(g_snd_warning_w, g_snd_warning)
#else
#define g_snd_error_w g_snd_error
#define g_snd_warning_w g_snd_warning
#endif

void g_init_errors(void)
{
  XEN_DEFINE_PROCEDURE(S_snd_error, g_snd_error_w, 1, 0, 0, H_snd_error);
  XEN_DEFINE_PROCEDURE(S_snd_warning, g_snd_warning_w, 1, 0, 0, H_snd_warning);

  #define H_snd_error_hook S_snd_error_hook " (error-message): called upon snd_error. \
If it returns #t, Snd flushes the error (it assumes you've reported it via the hook:\n\
  (add-hook! snd-error-hook\n\
    (lambda (msg) (play \"bong.snd\") #f))"

  #define H_snd_warning_hook S_snd_warning_hook " (warning-message): called upon snd_warning. \
If it returns #t, Snd flushes the warning (it assumes you've reported it via the hook):\n\
  (define without-warnings\n\
    (lambda (thunk)\n\
      (define no-warning (lambda (msg) #t))\n\
      (add-hook! snd-warning-hook no-warning)\n\
      (thunk)\n\
      (remove-hook! snd-warning-hook no-warning)))"

  XEN_DEFINE_HOOK(snd_error_hook, S_snd_error_hook, 1, H_snd_error_hook);       /* arg = error-message */
  XEN_DEFINE_HOOK(snd_warning_hook, S_snd_warning_hook, 1, H_snd_warning_hook); /* arg = error-message */
}

