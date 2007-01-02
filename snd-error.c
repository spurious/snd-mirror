#include "snd.h"

static char *io_error_names[IO_ERROR_NUM] = {"no error", "save-hook cancellation", "bad channel",
					     "can't reopen file", "too many open files", "unknown sndlib error", 
					     "no memory", "can't open", "no filename", "bad data format", "bad header type", "sndlib uninitialized", 
					     "not a sound file", "file closed", "write error", "interrupted", "can't close", 
					     "bad header", "disk full", "write protected", "can't read selection file",
					     "need write confirmation", "no changes", "io edit-hook cancellation", "can't create file"
};

const char *io_error_name(io_error_t err)
{
  if (err < IO_ERROR_NUM)
    return(io_error_names[(int)err]);
#if MUS_DEBUGGING
  fprintf(stderr, "unknown io_error: %d\n", err);
  abort();
#endif
  return(mus_format("unknown io_error: %d", err));
}

/* these are needed as C ints below */
#ifndef MUS_DEBUGGING
  #define MUS_DEBUGGING 0
#endif
#ifndef USE_NO_GUI
  #define USE_NO_GUI 0
#endif

static XEN snd_error_hook; 
static XEN snd_warning_hook; 

bool run_snd_error_hook(const char *msg)
{
  return((XEN_HOOKED(snd_error_hook)) &&
	 (XEN_NOT_FALSE_P(run_or_hook(snd_error_hook, 
				      XEN_LIST_1(C_TO_XEN_STRING(msg)),
				      S_snd_error_hook))));
}


void redirect_snd_warning_to(void (*handler)(const char *warning_msg, void *ufd), void *data)
{
  ss->snd_warning_handler = handler;
  ss->snd_warning_data = data;
}

#ifdef SND_AS_WIDGET
static void (*snd_error_display)(const char *msg);

void set_error_display(void (*func)(const char *msg))
{
  snd_error_display = func;
}
#endif

void redirect_snd_error_to(void (*handler)(const char *error_msg, void *ufd), void *data)
{
  ss->snd_error_handler = handler;
  ss->snd_error_data = data;
}

static void snd_error_1(const char *msg, bool with_redirection_and_hook)
{
  if (with_redirection_and_hook)
    {
      if (ss->snd_error_handler)
	{
	  /* make sure it doesn't call itself recursively */
	  void (*old_snd_error_handler)(const char *error_msg, void *data);
	  void *old_snd_error_data;
	  old_snd_error_handler = ss->snd_error_handler;
	  old_snd_error_data = ss->snd_error_data;
	  ss->snd_error_handler = NULL;
	  ss->snd_error_data = NULL;
	  (*(old_snd_error_handler))(msg, old_snd_error_data);
	  ss->snd_error_handler = old_snd_error_handler;
	  ss->snd_error_data = old_snd_error_data;
	  return;
	}
      
      if (run_snd_error_hook(msg))
	return;
    }
#if USE_NO_GUI
  fprintf(stderr, msg);
#else
  if ((ss) && (ss->sgx))
    {
      if ((MUS_DEBUGGING) || (ss->batch_mode))
	fprintf(stderr, msg);
#ifdef SND_AS_WIDGET
      if (snd_error_display) 
	snd_error_display(msg);
      else
	{
	  /* don't break (unlikely) existing code? */
#endif
	  {
	    snd_info *sp;
	    sp = any_selected_sound();
	    if (ss->catch_exists == 0) /* not from xen(?) */
	      {
		if ((sp) && (sp->active))
		  display_minibuffer_error(sp, msg);
		else post_it("Error", msg);
	      }
	  }
#ifdef SND_AS_WIDGET
	}
#endif
    }
  else 
    {
      fprintf(stderr, msg);
      fputc('\n', stderr);
    }
#endif
  /* end USE_NO_GUI */
}

static void snd_warning_1(const char *msg)
{
  if (ss->snd_warning_handler)
    {
      /* make sure it doesn't call itself recursively */
      void (*old_snd_warning_handler)(const char *msg, void *data);
      void *old_snd_warning_data;
      old_snd_warning_handler = ss->snd_warning_handler;
      old_snd_warning_data = ss->snd_warning_data;
      ss->snd_warning_handler = NULL;
      ss->snd_warning_data = NULL;
      (*(old_snd_warning_handler))(msg, old_snd_warning_data);
      ss->snd_warning_handler = old_snd_warning_handler;
      ss->snd_warning_data = old_snd_warning_data;
      return;
    }

  if ((XEN_HOOKED(snd_warning_hook)) &&
      (XEN_NOT_FALSE_P(run_or_hook(snd_warning_hook, 
				   XEN_LIST_1(C_TO_XEN_STRING(msg)),
				   S_snd_warning_hook))))
    return;

  if ((ss) && (!(ss->batch_mode)) && (ss->max_sounds > 0))
    {
      snd_info *sp;
      sp = any_selected_sound();
      if ((sp) && (sp->active))
	display_minibuffer_error(sp, msg);
      else 
	{
	  listener_append(msg);
	  fprintf(stderr, msg); 
	}
    }
  else fprintf(stderr, msg);
#if MUS_DEBUGGING
  fprintf(stderr, msg);
#endif
}


#define SND_ERROR_BUFFER_SIZE 1024
static char *snd_error_buffer = NULL;

void snd_warning(char *format, ...)
{
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
  snd_warning_1(snd_error_buffer);
}

void snd_warning_without_format(const char *msg)
{
  snd_warning_1(msg);
}

void snd_error(char *format, ...)
{
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
  snd_error_1(snd_error_buffer, true);
}

void snd_error_without_redirection_or_hook(const char *msg)
{
  snd_error_1(msg, false);
}

void snd_error_without_format(const char *msg)
{
  snd_error_1(msg, true);
}

static XEN g_snd_error(XEN msg)
{
  /* this throws a 'snd-error error; it does not call snd_error_1 or friends above */
  #define H_snd_error "(" S_snd_error " str): throws a 'snd-error error"
  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, XEN_ONLY_ARG, S_snd_error, "a string");

  if (!(run_snd_error_hook(XEN_TO_C_STRING(msg)))) /* have to call this before the throw, else we end up at top level */
    XEN_ERROR(XEN_ERROR_TYPE("snd-error"),
	      XEN_LIST_2(C_TO_XEN_STRING(S_snd_error),
			 msg));
  return(msg);
}
  
static XEN g_snd_warning(XEN msg)
{
  #define H_snd_warning "(" S_snd_warning " str): reports warning message str (normally in the minibuffer)"
  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, XEN_ONLY_ARG, S_snd_warning, "a string");
  snd_warning(XEN_TO_C_STRING(msg));
  return(msg);
}


static XEN clip_hook;

static mus_sample_t run_clip_hook(mus_sample_t val)
{
  if (XEN_HOOKED(clip_hook))
    {
      XEN result;
      result = run_progn_hook(clip_hook,
			      XEN_LIST_1(C_TO_XEN_DOUBLE(MUS_SAMPLE_TO_DOUBLE(val))),
			      S_clip_hook);
      if (XEN_NUMBER_P(result))
	return(MUS_DOUBLE_TO_SAMPLE(XEN_TO_C_DOUBLE(result)));
    }
  /* otherwise mimic the built-in default in io.c */
  if (val >= MUS_SAMPLE_MAX)
    return(MUS_SAMPLE_MAX);
  return(MUS_SAMPLE_MIN);
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
If it returns " PROC_TRUE ", Snd flushes the error (it assumes you've reported it via the hook:\n\
  (add-hook! " S_snd_error_hook "\n\
    (lambda (msg) (" S_play " \"bong.snd\") #f))"

  #define H_snd_warning_hook S_snd_warning_hook " (warning-message): called upon snd_warning. \
If it returns " PROC_TRUE ", Snd flushes the warning (it assumes you've reported it via the hook):\n\
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
#if HAVE_FORTH
  #define H_snd_error_hook S_snd_error_hook " (error-message): called upon snd_error. \
If it returns " PROC_TRUE ", Snd flushes the error (it assumes you've reported it via the hook:\n\
" S_snd_error_hook " lambda: <{ msg }>\n\
  \"bong.snd\" " S_play "\n\
; add-hook!"

  #define H_snd_warning_hook S_snd_warning_hook " (warning-message): called upon snd_warning. \
If it returns " PROC_TRUE ", Snd flushes the warning (it assumes you've reported it via the hook)"
#endif

  snd_error_hook = XEN_DEFINE_HOOK(S_snd_error_hook, 1, H_snd_error_hook);       /* arg = error-message */
  snd_warning_hook = XEN_DEFINE_HOOK(S_snd_warning_hook, 1, H_snd_warning_hook); /* arg = error-message */


  #define H_clip_hook S_clip_hook " (clipping-value) is called each time a sample is about to \
be clipped upon being written to a sound file.  The hook function can return the new vallue to \
be written, or rely on the default (-1.0 or 1.0 depending on the sign of 'clipping-value')."

  clip_hook = XEN_DEFINE_HOOK(S_clip_hook, 1, H_clip_hook); /* arg = clipping val as double */

  mus_clip_set_handler(run_clip_hook);
}

