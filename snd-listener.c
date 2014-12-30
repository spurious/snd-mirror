#include "snd.h"

bool listener_is_visible(void)
{
  return(listener_height() > 5);
}


static Xen read_hook;

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
  return(Xen_hook_has_list(read_hook));
}


Xen run_read_hook(char *str)
{
  return(run_or_hook(read_hook, 
		     Xen_list_1(C_string_to_Xen_string(str)),
		     S_read_hook));
}

#if HAVE_FORTH || HAVE_RUBY
void call_read_hook_or_eval(const char *text)
{
  Xen form;
  if (Xen_hook_has_list(read_hook))
    {
      form = run_or_hook(read_hook, 
			 Xen_list_1(C_string_to_Xen_string(text)),
			 S_read_hook);
      if (Xen_is_true(form))
	return;
    }
  else form = Xen_eval_C_string(text);
  snd_report_listener_result(form);
}
#endif


static Xen g_save_listener(Xen filename)
{
  #define H_save_listener "(" S_save_listener " filename): saves the current listener text in filename"
  FILE *fp = NULL;
  const char *name;
  int err = 0;
  Xen_check_type(Xen_is_string(filename), filename, 1, S_save_listener, "a string");
  name = Xen_string_to_C_string(filename);
  fp = FOPEN(name, "w");
  if (fp) 
    {
      err = save_listener_text(fp);
      snd_fclose(fp, name);
    }
  if ((!fp) || (err == -1))
    Xen_error(CANNOT_SAVE,
	      Xen_list_3(C_string_to_Xen_string(S_save_listener ": can't save ~S, ~A"),
			 filename,
			 C_string_to_Xen_string(snd_io_strerror())));
  return(filename);
}


static Xen g_clear_listener(void)
{
  #define H_clear_listener "(" S_clear_listener "): removes listener text from the beginning to the cursor"
  clear_listener();
  return(Xen_false);
}


static Xen g_show_listener(void) 
{
  #define H_show_listener "(" S_show_listener ") returns " PROC_TRUE " if the listener is open, otherwise " PROC_FALSE "."
  return(C_bool_to_Xen_boolean(listener_is_visible()));
}


static Xen g_set_show_listener(Xen val)
{
  Xen_check_type(Xen_is_boolean(val), val, 1, S_setB S_show_listener, "a boolean");
  handle_listener(Xen_boolean_to_C_bool(val));
  return(C_bool_to_Xen_boolean(listener_is_visible()));
}


void set_listener_prompt(const char *new_prompt)
{
  in_set_listener_prompt((char *)new_prompt);
  ss->listener_prompt_length = mus_strlen(new_prompt);

#if USE_NO_GUI
  {
#if HAVE_FORTH
    char *str;
    Xen_eval_C_string("before-prompt-hook reset-hook!\n");
    str = mus_format("before-prompt-hook lambda: <{ prompt pos }> \"%s\" ; add-hook!", listener_prompt(ss));
    Xen_eval_C_string(str);
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


static Xen g_listener_prompt(void) {return(C_string_to_Xen_string(listener_prompt(ss)));}

static Xen g_set_listener_prompt(Xen val) 
{
  #define H_listener_prompt "(" S_listener_prompt "): the current lisp listener prompt character ('>') "
  Xen_check_type(Xen_is_string(val), val, 1, S_setB S_listener_prompt, "a string"); 

  if (listener_prompt(ss)) free(listener_prompt(ss));
  set_listener_prompt(mus_strdup(Xen_string_to_C_string(val)));

  return(val);
}


static Xen g_snd_completion(Xen text)
{
  /* perhaps callable from emacs? */
  char *str, *temp;
  Xen res;

  Xen_check_type(Xen_is_string(text), text, 1, "snd-completion", "a string"); 

  temp = mus_strdup(Xen_string_to_C_string(text));
  str = expression_completer(NULL_WIDGET, temp, NULL);
  res = C_string_to_Xen_string(str);

  free(str);
  free(temp);

  return(res);
}


static Xen g_listener_colorized(void) 
{
  #define H_listener_colorized "(" S_listener_colorized ") returns #t if the listener is highlighting syntax."
#if USE_GTK
  return(C_bool_to_Xen_boolean(listener_colorized()));
#else
  return(Xen_false);
#endif
}

static Xen g_listener_set_colorized(Xen val) 
{
#if USE_GTK
  Xen_check_type(Xen_is_boolean(val), val, 1, S_setB S_listener_colorized, "a boolean");
  listener_set_colorized(Xen_boolean_to_C_bool(val));
#endif
  return(val);
}


Xen_wrap_1_arg(g_save_listener_w, g_save_listener)
Xen_wrap_no_args(g_clear_listener_w, g_clear_listener);
Xen_wrap_no_args(g_show_listener_w, g_show_listener)
Xen_wrap_1_arg(g_set_show_listener_w, g_set_show_listener)
Xen_wrap_no_args(g_listener_prompt_w, g_listener_prompt)
Xen_wrap_1_arg(g_set_listener_prompt_w, g_set_listener_prompt)
Xen_wrap_1_arg(g_snd_completion_w, g_snd_completion)
Xen_wrap_no_args(g_listener_colorized_w, g_listener_colorized)
Xen_wrap_1_arg(g_listener_set_colorized_w, g_listener_set_colorized)

#if HAVE_SCHEME
#if USE_GTK
static s7_pointer acc_listener_colorized(s7_scheme *sc, s7_pointer args) {return(g_listener_set_colorized(s7_cadr(args)));}
#endif
static s7_pointer acc_listener_prompt(s7_scheme *sc, s7_pointer args) {return(g_set_listener_prompt(s7_cadr(args)));}
#endif

void g_init_listener(void)
{
  Xen_define_procedure(S_save_listener,  g_save_listener_w,  1, 0, 0, H_save_listener);
  Xen_define_procedure(S_clear_listener, g_clear_listener_w, 0, 0, 0, H_clear_listener);

  Xen_define_dilambda(S_show_listener, g_show_listener_w, H_show_listener, S_setB S_show_listener, g_set_show_listener_w,  0, 0, 1, 0);
  Xen_define_dilambda(S_listener_prompt, g_listener_prompt_w, H_listener_prompt, S_setB S_listener_prompt, g_set_listener_prompt_w,  0, 0, 1, 0);
  Xen_define_dilambda(S_listener_colorized, g_listener_colorized_w, H_listener_colorized,
				   S_setB S_listener_colorized, g_listener_set_colorized_w,  0, 0, 1, 0);

  #define H_read_hook S_read_hook " (text): called each time a line is typed into the listener (triggered by the carriage return). \
If it returns true, Snd assumes you've dealt the text yourself, and does not try to evaluate it."
  
  read_hook = Xen_define_hook(S_read_hook, "(make-hook 'text)", 1, H_read_hook);

  Xen_define_procedure("snd-completion",        g_snd_completion_w,        1, 0, 0, "return completion of arg");

#if HAVE_SCHEME
#if USE_GTK
  s7_symbol_set_documentation(s7, ss->listener_colorized_symbol, "*listener-colorized*: number of vector elements to print in the listener (default: 12)");
  s7_symbol_set_access(s7, ss->listener_colorized_symbol, s7_make_function(s7, "[acc-" S_listener_colorized, acc_listener_colorized, 2, 0, false, "accessor"));
#endif
  s7_symbol_set_documentation(s7, ss->listener_prompt_symbol, "*listener-prompt*: the current lisp listener prompt character ('>') ");
  s7_symbol_set_access(s7, ss->listener_prompt_symbol, s7_make_function(s7, "[acc-" S_listener_prompt, acc_listener_prompt, 2, 0, false, "accessor"));
#endif  
}
