#include "snd.h"

static char *snd_error_buffer = NULL;

void snd_warning(char *format, ...)
{
  va_list ap;
  snd_info *sp;
  snd_state *ss;
  if (snd_error_buffer == NULL) snd_error_buffer = (char *)CALLOC(1024,sizeof(char));
  if (snd_error_buffer == NULL) {fprintf(stderr,"serious memory trouble!!"); return;}
#if HAVE_VPRINTF
  va_start(ap,format);
  vsprintf(snd_error_buffer,format,ap);
  va_end(ap);
#if HAVE_GUILE
  if (ignore_snd_warning(snd_error_buffer)) return;
#endif
  ss = get_global_state();
  if (ss)
    {
      sp = any_selected_sound(ss);
      if (sp)
	report_in_minibuffer(sp,snd_error_buffer);
      else fprintf(stderr,snd_error_buffer);
    }
  else fprintf(stderr,snd_error_buffer);
#else
  fprintf(stderr,"warning...");
#endif
}

void snd_error(char *format, ...)
{
#if HAVE_VPRINTF
  va_list ap;
  snd_info *sp;
  snd_state *ss;
  if (snd_error_buffer == NULL) snd_error_buffer = (char *)CALLOC(1024,sizeof(char));
  if (snd_error_buffer == NULL) {fprintf(stderr,"serious memory trouble!!"); return;}
  va_start(ap,format);
  vsprintf(snd_error_buffer,format,ap);
  va_end(ap);
#if HAVE_GUILE
  if (ignore_snd_error(snd_error_buffer)) return;
#endif
  ss = get_global_state();
  if ((ss) && (ss->sgx))
    {
#ifdef DEBUGGING
      fprintf(stderr,snd_error_buffer);
#endif
      add_to_error_history(ss,snd_error_buffer);
      sp = selected_sound(ss);
      if (sp)
	report_in_minibuffer(sp,snd_error_buffer);
      else post_error_dialog(ss,snd_error_buffer);
    }
  else fprintf(stderr,snd_error_buffer);
#else
  fprintf(stderr,"error...");
#endif
}
