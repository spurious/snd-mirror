#include "snd.h"

bool listener_is_visible(void)
{
  return(listener_height() > 5);
}


static XEN read_hook;

#if HAVE_SCHEME && (!USE_NO_GUI)

static int skipped_calls = 0;
#define SKIPPING 10

void listener_begin_hook(s7_scheme *sc, bool *val)
{
  if (skipped_calls < SKIPPING)
    {
      skipped_calls++;
      return;
    }
  skipped_calls = 0;
  ss->C_g_typed = false;

#if USE_MOTIF
  if (XtAppPending(MAIN_APP(ss)) & XtIMXEvent)
    {
      XEvent event;
      XtAppNextEvent(MAIN_APP(ss), &event);
      XtDispatchEvent(&event);
    }
#endif

#if USE_GTK
#if 0
  if (gdk_events_pending()) /* necessary -- otherwise Snd hangs in gtk_main_iteration */
    gtk_main_iteration();
#else
  {
    int i = 50;
    /* we need to let more than 1 event through at a time, else (for example) the listener popup
     *   menu never actually pops up.
     *
     * if no threads (as here) this is just g_main_context_pending(NULL)
     *   then gtk_main_iteration calls g_main_context_iteration(NULL, true), 
     *   so g_main_context_iteration(NULL, false) might combine the two.
     *   But the overhead of gtk_events_pending is insignificant.  This code
     *   is extremely slow -- it more than doubles the compute time of s7test
     *   for example, and spends 10% of its time fiddling with useless locks.
     */

    while ((gtk_events_pending()) && (i != 0))
      {
	gtk_main_iteration();
	i--; 
      }
  }
#endif
#endif

  *val = ss->C_g_typed;
}
#endif


bool have_read_hook(void)
{
  return(XEN_HOOKED(read_hook));
}


XEN run_read_hook(char *str)
{
  return(run_or_hook(read_hook, 
		     XEN_LIST_1(C_TO_XEN_STRING(str)),
		     S_read_hook));
}

#if HAVE_FORTH || HAVE_RUBY
void call_read_hook_or_eval(const char *text)
{
  XEN form;
  if (XEN_HOOKED(read_hook))
    {
      form = run_or_hook(read_hook, 
			 XEN_LIST_1(C_TO_XEN_STRING(text)),
			 S_read_hook);
      if (Xen_is_true(form))
	return;
    }
  else form = XEN_EVAL_C_STRING(text);
  snd_report_listener_result(form);
}
#endif


static XEN g_save_listener(XEN filename)
{
  #define H_save_listener "(" S_save_listener " filename): saves the current listener text in filename"
  FILE *fp = NULL;
  const char *name;
  int err = 0;
  XEN_ASSERT_TYPE(Xen_is_string(filename), filename, 1, S_save_listener, "a string");
  name = XEN_TO_C_STRING(filename);
  fp = FOPEN(name, "w");
  if (fp) 
    {
      err = save_listener_text(fp);
      snd_fclose(fp, name);
    }
  if ((!fp) || (err == -1))
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_save_listener ": can't save ~S, ~A"),
			 filename,
			 C_TO_XEN_STRING(snd_io_strerror())));
  return(filename);
}


static XEN g_clear_listener(void)
{
  #define H_clear_listener "(" S_clear_listener "): removes listener text from the beginning to the cursor"
  clear_listener();
  return(XEN_FALSE);
}


static XEN g_show_listener(void) 
{
  #define H_show_listener "(" S_show_listener ") returns " PROC_TRUE " if the listener is open, otherwise " PROC_FALSE "."
  return(C_TO_XEN_BOOLEAN(listener_is_visible()));
}


static XEN g_set_show_listener(XEN val)
{
  XEN_ASSERT_TYPE(Xen_is_boolean(val), val, 1, S_setB S_show_listener, "a boolean");
  handle_listener(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(listener_is_visible()));
}


void set_listener_prompt(const char *new_prompt)
{
  in_set_listener_prompt((char *)new_prompt);
  ss->listener_prompt_length = mus_strlen(new_prompt);

#if USE_NO_GUI
  {
#if HAVE_FORTH
    char *str;
    XEN_EVAL_C_STRING("before-prompt-hook reset-hook!\n");
    str = mus_format("before-prompt-hook lambda: <{ prompt pos }> \"%s\" ; add-hook!", listener_prompt(ss));
    XEN_EVAL_C_STRING(str);
    free(str);
#endif

#if HAVE_RUBY
    xen_rb_repl_set_prompt(listener_prompt(ss));
#endif

#if HAVE_SCHEME
    xen_s7_set_repl_prompt(listener_prompt(ss));
#endif
  }

#else
  /* not USE_NO_GUI */

  /* here if the prompt changes and the listener exists, we need to make sure
   *   we output a new prompt; otherwise the expression finder gets confused
   *   by the old prompt.
   */
#if (!USE_GTK)
  listener_append_and_prompt(NULL); /* this checks first that the listener exists */
#else
  glistener_set_prompt(ss->listener, listener_prompt(ss));
#endif  
#endif
  
}


static XEN g_listener_prompt(void) {return(C_TO_XEN_STRING(listener_prompt(ss)));}

static XEN g_set_listener_prompt(XEN val) 
{
  #define H_listener_prompt "(" S_listener_prompt "): the current lisp listener prompt character ('>') "

  XEN_ASSERT_TYPE(Xen_is_string(val), val, 1, S_setB S_listener_prompt, "a string"); 

  if (listener_prompt(ss)) free(listener_prompt(ss));
  set_listener_prompt(mus_strdup(XEN_TO_C_STRING(val)));

  return(C_TO_XEN_STRING(listener_prompt(ss)));
}


static XEN g_snd_completion(XEN text)
{
  /* perhaps callable from emacs? */
  char *str, *temp;
  XEN res;

  XEN_ASSERT_TYPE(Xen_is_string(text), text, 1, "snd-completion", "a string"); 

  temp = mus_strdup(XEN_TO_C_STRING(text));
  str = expression_completer(NULL_WIDGET, temp, NULL);
  res = C_TO_XEN_STRING(str);

  free(str);
  free(temp);

  return(res);
}


static XEN g_listener_colorized(void) 
{
  #define H_listener_colorized "(" S_listener_colorized ") returns #t if the listener is highlighting syntax."
#if USE_GTK
  return(C_TO_XEN_BOOLEAN(listener_colorized()));
#else
  return(XEN_FALSE);
#endif
}

static XEN g_listener_set_colorized(XEN val) 
{
#if USE_GTK
  XEN_ASSERT_TYPE(Xen_is_boolean(val), val, 1, S_setB S_listener_colorized, "a boolean");
  listener_set_colorized(XEN_TO_C_BOOLEAN(val));
#endif
  return(val);
}


XEN_NARGIFY_1(g_save_listener_w, g_save_listener)
XEN_NARGIFY_0(g_clear_listener_w, g_clear_listener);
XEN_NARGIFY_0(g_show_listener_w, g_show_listener)
XEN_NARGIFY_1(g_set_show_listener_w, g_set_show_listener)
XEN_NARGIFY_0(g_listener_prompt_w, g_listener_prompt)
XEN_NARGIFY_1(g_set_listener_prompt_w, g_set_listener_prompt)
XEN_NARGIFY_1(g_snd_completion_w, g_snd_completion)
XEN_NARGIFY_0(g_listener_colorized_w, g_listener_colorized)
XEN_NARGIFY_1(g_listener_set_colorized_w, g_listener_set_colorized)

void g_init_listener(void)
{
  XEN_DEFINE_PROCEDURE(S_save_listener,  g_save_listener_w,  1, 0, 0, H_save_listener);
  XEN_DEFINE_PROCEDURE(S_clear_listener, g_clear_listener_w, 0, 0, 0, H_clear_listener);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_show_listener, g_show_listener_w, H_show_listener,
				   S_setB S_show_listener, g_set_show_listener_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_listener_prompt, g_listener_prompt_w, H_listener_prompt,
				   S_setB S_listener_prompt, g_set_listener_prompt_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_listener_colorized, g_listener_colorized_w, H_listener_colorized,
				   S_setB S_listener_colorized, g_listener_set_colorized_w,  0, 0, 1, 0);

  #define H_read_hook S_read_hook " (text): called each time a line is typed into the listener (triggered by the carriage return). \
If it returns true, Snd assumes you've dealt the text yourself, and does not try to evaluate it."
  
  read_hook = XEN_DEFINE_HOOK(S_read_hook, "(make-hook 'text)", 1, H_read_hook);

  XEN_DEFINE_PROCEDURE("snd-completion",        g_snd_completion_w,        1, 0, 0, "return completion of arg");
}
