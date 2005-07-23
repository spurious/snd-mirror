#include "snd.h"

static char *io_error_names[] = {"no error", "save hook cancellation", "bad channel", "bad edit position", "read protected", 
				 "no file", "bad location", "cant reopen file", "too many open files", "unknown sndlib error", 
				 "no memory", "cant open file", "no filename", "bad data format", "bad header type", "sndlib uninitialized", 
				 "not a sound file", "file closed", "write error", "read error", "interrupted", "cant close file", 
				 "bad header", "disk full", "write protected", "write cancellation", "cant move file", "cant remove file", 
				 "cant read selection file", "cant open temp file", "need write confirmation", "no changes"
};

const char *io_error_name(io_error_t err)
{
  if ((err >= 0) &&
      (err <= IO_NO_CHANGES))
    return(io_error_names[(int)err]);
  return("unknown io_error");
}

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

#if DEBUGGING
void redirect_snd_warning_to_1(void (*handler)(const char *warning_msg, void *ufd), void *data, const char *caller)
#else
void redirect_snd_warning_to(void (*handler)(const char *warning_msg, void *ufd), void *data)
#endif
{
#if DEBUGGING
  if ((handler) && (ss->snd_warning_handler))
    fprintf(stderr,"%s: redirect over warn from %s\n", caller, ss->snd_warning_caller);
  if (ss->snd_warning_caller) FREE(ss->snd_warning_caller);
  ss->snd_warning_caller = copy_string(caller);
#endif
  ss->snd_warning_handler = handler;
  ss->snd_warning_data = data;
}

void snd_warning(char *format, ...)
{
  va_list ap;
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

  if (ss->snd_warning_handler)
    {
      (*(ss->snd_warning_handler))(snd_error_buffer, ss->snd_warning_data);
      return;
    }

  if ((XEN_HOOKED(snd_warning_hook)) &&
      (XEN_NOT_FALSE_P(run_or_hook(snd_warning_hook, 
				   XEN_LIST_1(C_TO_XEN_STRING(snd_error_buffer)),
				   S_snd_warning_hook))))
    return;
  if ((ss) && (!(ss->batch_mode)) && (ss->max_sounds > 0))
    {
      snd_info *sp;
      sp = any_selected_sound();
      if ((sp) && (sp->active))
	report_in_minibuffer(sp, snd_error_buffer);
      else fprintf(stderr, snd_error_buffer); /* TODO: need some better error handling here */
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

void set_error_display(void (*func)(const char *))
{
  snd_error_display = func;
}
#endif

static bool direct_snd_error_call = false;

#if DEBUGGING
void redirect_snd_error_to_1(void (*handler)(const char *error_msg, void *ufd), void *data, const char *caller)
#else
void redirect_snd_error_to(void (*handler)(const char *error_msg, void *ufd), void *data)
#endif
{
#if DEBUGGING
  if ((handler) && (ss->snd_error_handler))
    fprintf(stderr,"%s: redirect over snd error from %s\n", caller, ss->snd_error_caller);
  if (ss->snd_error_caller) FREE(ss->snd_error_caller);
  ss->snd_error_caller = copy_string(caller);
#endif
  ss->snd_error_handler = handler;
  ss->snd_error_data = data;
}

void snd_error(char *format, ...)
{
#if HAVE_VPRINTF
  va_list ap;
  if (snd_error_buffer == NULL) 
    snd_error_buffer = (char *)CALLOC(SND_ERROR_BUFFER_SIZE, sizeof(char));
  va_start(ap, format);
#if HAVE_VSNPRINTF
  vsnprintf(snd_error_buffer, SND_ERROR_BUFFER_SIZE, format, ap);
#else
  vsprintf(snd_error_buffer, format, ap);
#endif
  va_end(ap);

  if (ss->snd_error_handler)
    {
      (*(ss->snd_error_handler))(snd_error_buffer, ss->snd_error_data);
      return;
    }

  if ((XEN_HOOKED(snd_error_hook)) &&
      (XEN_NOT_FALSE_P(run_or_hook(snd_error_hook, 
				   XEN_LIST_1(C_TO_XEN_STRING(snd_error_buffer)),
				   S_snd_error_hook))))
    return;
#if USE_NO_GUI
  fprintf(stderr, snd_error_buffer);
#else
  if ((ss) && (ss->sgx))
    {
      if ((DEBUGGING) || (ss->batch_mode))
	fprintf(stderr, snd_error_buffer);
#ifdef SND_AS_WIDGET
      if (snd_error_display) 
	snd_error_display(snd_error_buffer);
      else
	{
	  /* don't break (unlikely) existing code? */
#endif
	  {
	    snd_info *sp;
	    sp = any_selected_sound();
	    if ((direct_snd_error_call) ||
		(ss->catch_exists == 0))
	      {
		if ((sp) && (sp->active))
		  report_in_minibuffer(sp, snd_error_buffer); /* TODO: this truncates the message! needs word wrap in either case */
		else post_it("Error", snd_error_buffer);
	      }
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
#endif
  /* end USE_NO_GUI */
#else
  fprintf(stderr, "error...");
  fputc('\n', stderr);
#endif
}

static XEN g_snd_error(XEN msg)
{
  #define H_snd_error "(" S_snd_error " str): reports error message str (normally in the error dialog)"
  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, XEN_ONLY_ARG, S_snd_error, "a string");
  direct_snd_error_call = true; /* TODO: this looks bogus and should use redirection */
  snd_error(XEN_TO_C_STRING(msg));
  direct_snd_error_call = false;
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
  XEN_DEFINE_PROCEDURE(S_snd_error,   g_snd_error_w,   1, 0, 0, H_snd_error);
  XEN_DEFINE_PROCEDURE(S_snd_warning, g_snd_warning_w, 1, 0, 0, H_snd_warning);

#if HAVE_SCHEME
  #define H_snd_error_hook S_snd_error_hook " (error-message): called upon snd_error. \
If it returns #t, Snd flushes the error (it assumes you've reported it via the hook:\n\
  (add-hook! " S_snd_error_hook "\n\
    (lambda (msg) (" S_play " \"bong.snd\") #f))"

  #define H_snd_warning_hook S_snd_warning_hook " (warning-message): called upon snd_warning. \
If it returns #t, Snd flushes the warning (it assumes you've reported it via the hook):\n\
  (define without-warnings\n\
    (lambda (thunk)\n\
      (define no-warning (lambda (msg) #t))\n\
      (add-hook! " S_snd_warning_hook " no-warning)\n\
      (thunk)\n\
      (remove-hook! " S_snd_warning_hook " no-warning)))"
#endif
#if HAVE_RUBY
  #define H_snd_error_hook S_snd_error_hook " (error-message): called upon snd_error. \
If it returns true, Snd flushes the error (it assumes you've reported it via the hook:\n\
  $snd_error_hook.add-hook!(\"error\") do |msg|\n\
    play(\"bong.snd\")\n\
    false\n\
  end"

  #define H_snd_warning_hook S_snd_warning_hook " (warning-message): called upon snd_warning. \
If it returns true, Snd flushes the warning (it assumes you've reported it via the hook)"
#endif

  snd_error_hook = XEN_DEFINE_HOOK(S_snd_error_hook, 1, H_snd_error_hook);       /* arg = error-message */
  snd_warning_hook = XEN_DEFINE_HOOK(S_snd_warning_hook, 1, H_snd_warning_hook); /* arg = error-message */
}

