#include "snd.h"

static const char *io_error_names[IO_ERROR_NUM] = {"no error", "save-hook cancellation", "bad channel",
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
  return(mus_format("unknown io_error: %d", err));
}


/* this is needed as a C int below */
#ifndef USE_NO_GUI
  #define USE_NO_GUI 0
#endif


bool run_snd_error_hook(const char *msg)
{
  return((XEN_HOOKED(ss->snd_error_hook)) &&
	 (XEN_TRUE_P(run_or_hook(ss->snd_error_hook, 
				 XEN_LIST_1(C_TO_XEN_STRING(msg)),
				 S_snd_error_hook))));
}


void redirect_snd_warning_to(void (*handler)(const char *warning_msg, void *ufd), void *data)
{
  ss->snd_warning_handler = handler;
  ss->snd_warning_data = data;
}


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
#if (USE_NO_GUI)
  fprintf(stderr, "%s", msg);
#else
  if (ss)
    {
      if (ss->batch_mode)
	fprintf(stderr, "%s", msg);
#if ((!HAVE_EXTENSION_LANGUAGE) && (!USE_NO_GUI))
      {
	snd_info *sp;
	sp = any_selected_sound();
	if ((sp) && (sp->active))
	  status_report(sp, "%s", msg);
	else post_it("Error", msg);
      }
#endif
    }
  else 
    {
      fprintf(stderr, "%s", msg);
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

  if ((XEN_HOOKED(ss->snd_warning_hook)) &&
      (XEN_TRUE_P(run_or_hook(ss->snd_warning_hook, 
			      XEN_LIST_1(C_TO_XEN_STRING(msg)),
			      S_snd_warning_hook))))
    return;

  if ((ss) && (!(ss->batch_mode)) && (ss->max_sounds > 0))
    {
      snd_info *sp;
      sp = any_selected_sound();
      if ((sp) && (sp->active))
	status_report(sp, "%s", msg); /* make the Mac C compiler happy */
      else 
	{
	  listener_append(msg);
	  fprintf(stderr, "%s", msg); 
	}
    }
  else fprintf(stderr, "%s", msg);
}


static int snd_error_buffer_size = 1024;
static char *snd_error_buffer = NULL;

void snd_warning(const char *format, ...)
{
  int bytes_needed = 0;
  va_list ap;

  if (snd_error_buffer == NULL) 
    snd_error_buffer = (char *)calloc(snd_error_buffer_size, sizeof(char));
  va_start(ap, format);

  /* can't use vasprintf here -- we may jump anywhere leaving unclaimed memory behind 
   */
  bytes_needed = vsnprintf(snd_error_buffer, snd_error_buffer_size, format, ap);
  va_end(ap);

  if (bytes_needed >= snd_error_buffer_size)
    {
      snd_error_buffer_size = bytes_needed * 2;
      free(snd_error_buffer);
      snd_error_buffer = (char *)calloc(snd_error_buffer_size, sizeof(char));

      va_start(ap, format);
      vsnprintf(snd_error_buffer, snd_error_buffer_size, format, ap);
      va_end(ap);
    }
  snd_warning_1(snd_error_buffer);
}


void snd_warning_without_format(const char *msg)
{
  snd_warning_1(msg);
}


void snd_error(const char *format, ...)
{
  int bytes_needed = 0;
  va_list ap;
  if (snd_error_buffer == NULL) 
    snd_error_buffer = (char *)calloc(snd_error_buffer_size, sizeof(char));

  va_start(ap, format);
  bytes_needed = vsnprintf(snd_error_buffer, snd_error_buffer_size, format, ap);
  va_end(ap);

  if (bytes_needed > snd_error_buffer_size)
    {
      snd_error_buffer_size = bytes_needed * 2;
      free(snd_error_buffer);
      snd_error_buffer = (char *)calloc(snd_error_buffer_size, sizeof(char));

      va_start(ap, format);
      vsnprintf(snd_error_buffer, snd_error_buffer_size, format, ap);
      va_end(ap);
    }
  snd_error_1(snd_error_buffer, true);
}


void snd_error_without_format(const char *msg)
{
  snd_error_1(msg, true);
}


static XEN g_snd_error(XEN msg)
{
  /* this throws a 'snd-error error; it does not call snd_error_1 or friends above */
  #define H_snd_error "(" S_snd_error " str): throws a 'snd-error error"
  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, 1, S_snd_error, "a string");

  if (!(run_snd_error_hook(XEN_TO_C_STRING(msg)))) /* have to call this before the throw, else we end up at top level */
    XEN_ERROR(XEN_ERROR_TYPE("snd-error"),
	      XEN_LIST_2(C_TO_XEN_STRING(S_snd_error ": ~A"),
			 msg));
  return(msg);
}

  
static XEN g_snd_warning(XEN msg)
{
  #define H_snd_warning "(" S_snd_warning " str): reports warning message str (normally in the status area)"
  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, 1, S_snd_warning, "a string");
  snd_warning("%s", XEN_TO_C_STRING(msg));
  return(msg);
}


static XEN clip_hook;

static mus_float_t run_clip_hook(mus_float_t val)
{
  if (XEN_HOOKED(clip_hook))
    {
      XEN result;
      result = run_progn_hook(clip_hook,
			      XEN_LIST_1(C_TO_XEN_DOUBLE(val)),
			      S_clip_hook);
      if (XEN_NUMBER_P(result))
	return(XEN_TO_C_DOUBLE(result));
    }
  /* otherwise mimic the built-in default in io.c */
  if (val >= 0.99999)
    return(0.99999);
  return(-1.0);
}

static bool clip_hook_checker(void)
{
  bool result;
  result = XEN_HOOKED(clip_hook);
  if (result)
    mus_clip_set_handler(run_clip_hook);
  else mus_clip_set_handler(NULL);
  return(result);
}


 
XEN_NARGIFY_1(g_snd_error_w, g_snd_error)
XEN_NARGIFY_1(g_snd_warning_w, g_snd_warning)

void g_init_errors(void)
{
  XEN_DEFINE_PROCEDURE(S_snd_error,   g_snd_error_w,   1, 0, 0, H_snd_error);
  XEN_DEFINE_PROCEDURE(S_snd_warning, g_snd_warning_w, 1, 0, 0, H_snd_warning);

#if HAVE_SCHEME
  #define H_snd_error_hook S_snd_error_hook " (message): called upon snd_error. \
If it returns " PROC_TRUE ", Snd flushes the error (it assumes you've reported it via the hook):\n\
  (hook-push " S_snd_error_hook "\n\
    (lambda (hook) (" S_play " \"bong.snd\")))"

  #define H_snd_warning_hook S_snd_warning_hook " (message): called upon snd_warning. \
If it returns " PROC_TRUE ", Snd flushes the warning (it assumes you've reported it via the hook):\n\
  (define without-warnings\n\
    (lambda (thunk)\n\
      (define no-warning (lambda (hook) (set! (hook 'result) #t)))\n\
      (hook-push snd-warning-hook no-warning) \n\
      (thunk)\n\
      (hook-remove snd-warning-hook no-warning)))"
#endif
#if HAVE_RUBY
  #define H_snd_error_hook S_snd_error_hook " (error-message): called upon snd_error. \
If it returns true, Snd flushes the error (it assumes you've reported it via the hook):\n\
  $snd_error_hook.add-hook!(\"error\") do |msg|\n\
    play(\"bong.snd\")\n\
    false\n\
  end"

  #define H_snd_warning_hook S_snd_warning_hook " (warning-message): called upon snd_warning. \
If it returns true, Snd flushes the warning (it assumes you've reported it via the hook)\n\
  def without_warning(&body)\n\
    $snd_warning_hook.add_hook!(\"no_warning\") do |msg| true end\n\
    ret = body.call\n\
    $snd_warning_hook.remove_hook!(\"no_warning\")\n\
    ret\n\
  end\n\
  # without_warning do " S_snd_warning "(\"not shown\") end"
#endif
#if HAVE_FORTH
  #define H_snd_error_hook S_snd_error_hook " (error-message): called upon snd_error. \
If it returns " PROC_TRUE ", Snd flushes the error (it assumes you've reported it via the hook):\n\
" S_snd_error_hook " lambda: <{ msg }>\n\
  \"bong.snd\" " S_play " drop\n\
  #f\n\
; add-hook!"

  #define H_snd_warning_hook S_snd_warning_hook " (warning-message): called upon snd_warning. \
If it returns " PROC_TRUE ", Snd flushes the warning (it assumes you've reported it via the hook)\n\
  : no-warning <{ msg -- f }> #t ;\n\
  : without-warnings <{ xt -- }>\n\
    " S_snd_warning_hook " <'> no-warning add-hook!\n\
    xt execute\n\
    " S_snd_warning_hook " <'> no-warning remove-hook! drop\n\
  ;\n\
  \\ lambda: ( -- ) \"not shown\" " S_snd_warning " ; without-warning\n\
"
#endif

  ss->snd_error_hook =   XEN_DEFINE_HOOK(S_snd_error_hook,   "(make-hook 'message)", 1, H_snd_error_hook);
  ss->snd_warning_hook = XEN_DEFINE_HOOK(S_snd_warning_hook, "(make-hook 'message)", 1, H_snd_warning_hook);


  #define H_clip_hook S_clip_hook " (val) is called each time a sample is about to \
be clipped upon being written to a sound file.  The hook function can return the new value to \
be written, or rely on the default (-1.0 or 1.0 depending on the sign of 'val')."

  clip_hook = XEN_DEFINE_HOOK(S_clip_hook, "(make-hook 'val)", 1, H_clip_hook); 
  mus_clip_set_handler_and_checker(NULL, clip_hook_checker);
}

