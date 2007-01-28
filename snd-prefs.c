/* this file included as text in snd-g|xprefs.c */

static void int_to_textfield(widget_t w, int val)
{
  char *str;
  str = (char *)CALLOC(16, sizeof(char));
  mus_snprintf(str, 16, "%d", val);
  SET_TEXT(w, str);
  FREE(str);
}

static void float_to_textfield(widget_t w, Float val)
{
  char *str;
  str = (char *)CALLOC(12, sizeof(char));
  mus_snprintf(str, 12, "%.3f", val);
  SET_TEXT(w, str);
  FREE(str);
}

static void float_1_to_textfield(widget_t w, Float val)
{
  char *str;
  str = (char *)CALLOC(12, sizeof(char));
  mus_snprintf(str, 12, "%.1f", val);
  SET_TEXT(w, str);
  FREE(str);
}

static TIMEOUT_TYPE unpost_any_error(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  prf->got_error = false;
  black_text(prf);
  set_label(prf->label, prf->saved_label);
  if (prf->reflect_func) (*(prf->reflect_func))(prf);
  TIMEOUT_RESULT
}

static void any_error_to_text(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  prf->got_error = true;
  red_text(prf);
  set_label(prf->label, msg);
  TIMEOUT(unpost_any_error);
}

static void redirect_post_prefs_error(const char *msg, void *data)
{
  post_prefs_error(msg, (prefs_info *)data);
}


static int prefs_size = 0, prefs_top = 0;
static prefs_info **prefs = NULL;

static void remember_pref(prefs_info *prf, 
			  void (*reflect_func)(struct prefs_info *prf),
			  void (*save_func)(struct prefs_info *prf, FILE *fd),
			  void (*help_func)(struct prefs_info *prf),
			  void (*clear_func)(struct prefs_info *prf),
			  void (*revert_func)(struct prefs_info *prf))
{
  if (prefs_size == 0)
    {
      prefs_size = 100;
      prefs = (prefs_info **)CALLOC(prefs_size, sizeof(prefs_info *));
    }
  else
    {
      if (prefs_top >= prefs_size)
	{
	  int i;
	  prefs_size += 100;
	  prefs = (prefs_info **)REALLOC(prefs, prefs_size * sizeof(prefs_info *));
	  for (i = prefs_top; i < prefs_size; i++) prefs[i] = NULL;
	}
    }
  prf->reflect_func = reflect_func;
  prf->save_func = save_func;
  prf->help_func = help_func;
  prf->clear_func = clear_func;
  prf->revert_func = revert_func;
  prefs[prefs_top++] = prf;
}

static void reflect_prefs(void)
{
  int i;
  for (i = 0; i < prefs_top; i++)
    {
      prefs_info *prf;
      prf = prefs[i];
      if ((prf) &&
	  (prf->reflect_func))
	(*(prf->reflect_func))(prf);
    }
}

static void revert_prefs(void)
{
  int i;
  for (i = 0; i < prefs_top; i++)
    {
      prefs_info *prf;
      prf = prefs[i];
      if ((prf) &&
	  (prf->revert_func))
	(*(prf->revert_func))(prf);
    }
}

static void clear_prefs(void)
{
  int i;
  for (i = 0; i < prefs_top; i++)
    {
      prefs_info *prf;
      prf = prefs[i];
      if ((prf) &&
	  (prf->clear_func))
	(*(prf->clear_func))(prf);
    }
}

static void preferences_revert_or_clear(bool revert)
{
  clear_prefs_dialog_error();
  if (revert)
    {
      revert_prefs();
    }
  else
    {
      snd_set_global_defaults(true);
      clear_prefs();
    }
  reflect_prefs();
  prefs_unsaved = false;
  if (prefs_saved_filename) 
    {
      char *fullname;
      fullname = mus_expand_filename(prefs_saved_filename);
      if (mus_file_probe(fullname))
	snd_remove(fullname, IGNORE_CACHE);
      FREE(prefs_saved_filename);
      FREE(fullname);
      prefs_saved_filename = NULL;
    }
  prefs_set_dialog_title(NULL);
}

static bool local_access(char *dir)
{
  int err;
  char *temp;
  temp = shorter_tempnam(dir, "snd_");
  err = mus_file_create(temp);
  if (err != -1)
    {
      snd_close(err, temp);
      snd_remove(temp, IGNORE_CACHE);
    }
  FREE(temp);
  return(err != -1);
}

static bool string_member_p(char *val, char **lst, int len)
{
  int i;
  if ((len == 0) || (!lst) || (!val)) return(false);
  for (i = 0; i < len; i++)
    if ((lst[i]) &&
	(strcmp(val, lst[i]) == 0))
      return(true);
  return(false);
}

static char **load_path_to_string_array(int *len)
{
  char **cdirs = NULL;
  int dir_len = 0, i, j = 0;
  XEN dirs;
  dirs = XEN_LOAD_PATH; /* in Guile, if this is the %load-path symbol value, the list gets mangled?? (so use eval string) */
  dir_len = XEN_LIST_LENGTH(dirs);
  if (dir_len > 0)
    {
      cdirs = (char **)CALLOC(dir_len, sizeof(char *));
      for (i = 0; i < dir_len; i++)
	{
	  char *path;
	  path = XEN_TO_C_STRING(XEN_LIST_REF(dirs, i));
	  if ((path) && (!(string_member_p(path, cdirs, j))))   /* try to remove duplicates */
	    cdirs[j++] = copy_string(path);
	}
    }
  (*len) = j;
  return(cdirs);
}

static void add_local_load_path(FILE *fd, char *path)
{
#if HAVE_GUILE
  fprintf(fd, "(if (not (member \"%s\" %%load-path)) (set! %%load-path (cons \"%s\" %%load-path)))\n", path, path);
#endif
#if HAVE_RUBY
  fprintf(fd, "if (not $LOAD_PATH.include?(\"%s\")) then $LOAD_PATH.push(\"%s\") end\n", path, path);
#endif
#if HAVE_FORTH
  /* this already checks */
  fprintf(fd, "\"%s\" add-load-path\n", path); /* no drop here */
#endif
#if HAVE_GAUCHE
  fprintf(fd, "(add-to-load-path \"%s\")\n", path); /* see snd-xen.c */
#endif
}

static void save_prefs(const char *filename)
{
  char *fullname;
  FILE *fd;
  if (!filename) return; /* error earlier */
  fullname = mus_expand_filename(filename);
  fd = FOPEN(fullname, "a");

  fprintf(fd, "\n");

  if (fd)
    {
      char **current_dirs = NULL;
      int i, current_dirs_len = 0;
      char *unchecked_load_path = NULL;

      /* LOAD_PATH has the current load-path list,
       *   GET_TEXT(load_path_text_widget) has the current text (independent of activation)
       *   include_load_path has whatever the last <cr> set it to.
       *
       * load_path_to_string_array can turn the LOAD_PATH into a char** array.
       *
       * load-path needs to be set even if a later init file adds to it; we need a true
       *   load-path before loading extensions.scm, but this can be called
       *   repeatedly, and across executions, so we don't want to fill up the list
       *   with repetitions,
       *
       * find_sources below is being used to get the current load-path entry that points to extensions.*
       */

      current_dirs = load_path_to_string_array(&current_dirs_len);
      if (current_dirs)
	for (i = current_dirs_len - 1; i >= 0; i--) /* consing on front, so keep original order of paths */
	  add_local_load_path(fd, current_dirs[i]); /* don't try to be smart about startup paths -- just include everybody */

      if ((include_load_path) &&
	  (!(string_member_p(include_load_path, current_dirs, current_dirs_len))))
	add_local_load_path(fd, include_load_path);

      if (load_path_text_widget)
	{
	  unchecked_load_path = GET_TEXT(load_path_text_widget);
	  if ((unchecked_load_path) &&                                                          /* text widget has an entry */
	      (local_access(unchecked_load_path)) &&                                            /* it's a legit path */
	      (!(string_member_p(unchecked_load_path, current_dirs, current_dirs_len))) &&        /* it's not in LOAD_PATH */
	      ((!include_load_path) || (strcmp(unchecked_load_path, include_load_path) != 0)))  /* it's not already included above */
	    add_local_load_path(fd, unchecked_load_path);
	  if (unchecked_load_path) {FREE_TEXT(unchecked_load_path);} /* a no-op in gtk */
	}

      if (current_dirs)
	{
	  for (i = 0; i < current_dirs_len; i++)
	    if (current_dirs[i]) FREE(current_dirs[i]);
	  FREE(current_dirs);
	  current_dirs = NULL;
	}

      /* now finally the load path is set up, so we can call (load-from-path...) if we need to */
      for (i = 0; i < prefs_top; i++)
	{
	  prefs_info *prf;
	  prf = prefs[i];
	  if ((prf) &&
	      (prf->save_func))
	    (*(prf->save_func))(prf, fd);
	}
      snd_fclose(fd, filename);
    }
  else snd_error("can't save preferences: %s %s", filename, snd_io_strerror());
  FREE(fullname);
  prefs_unsaved = false;
  prefs_set_dialog_title(filename);
}

static char *trim_string(const char *str)
{
  int i = 0, len, j = 0, k, m;
  char *trimmed_str;
  len = strlen(str);
  trimmed_str = (char *)CALLOC(len + 1, sizeof(char));
  while ((i < len) && (isspace(str[i]))) i++;
  k = len - 1;
  while ((k > i) && (isspace(str[k]))) k--;
  for (m = i; m <= k; m++)
    trimmed_str[j++] = str[m];
  return(trimmed_str);
}

static char key_buf[16];
static char *possibly_quote(char *key)
{
  int i, j, len;
  len = snd_strlen(key);
  if (len > 12) len = 12;
  for (i = 0, j = 0; i < len; i++)
    if (!(isspace(key[i])))
      {
	if ((j == 0) && (isalpha(key[i])))
	  key_buf[j++] = '\"';
	key_buf[j++] = key[i];
      }
  if ((key_buf[0] == '\"') && (key_buf[j - 1] != '\"'))
    key_buf[j++] = '\"';
  key_buf[j++] = '\0';
#if 0
  fprintf(stderr,"return key %s\n", key_buf);
#endif
  return(key_buf);
}

static void prefs_help(prefs_info *prf)
{
  if (prf->var_name)
    {
      if (prf->help_func)
	{
	  prefs_helping = true;
	  (*(prf->help_func))(prf);
	}
      else
	{
	  XEN sym;
	  sym = C_STRING_TO_XEN_SYMBOL(TO_PROC_NAME((char *)(prf->var_name)));
	  if (XEN_SYMBOL_P(sym))
	    {
	      XEN obj;
	      obj = XEN_OBJECT_HELP(sym);
#if HAVE_GUILE
	      if (XEN_FALSE_P(obj))
		{
		  XEN lookup;
		  lookup = XEN_SYMBOL_TO_VARIABLE(sym);
		  if (!(XEN_FALSE_P(lookup)))
		    {
		      sym = XEN_VARIABLE_REF(lookup);
		      if (XEN_PROCEDURE_P(sym))
			{
			  obj = XEN_PROCEDURE_HELP(sym);
			  if (XEN_FALSE_P(obj))
			    obj = XEN_PROCEDURE_SOURCE_HELP(sym);
			}}}
#endif
	      if (XEN_STRING_P(obj))
		{
		  prefs_helping = true;
		  snd_help(prf->var_name, 
			   XEN_TO_C_STRING(obj),
			   WITH_WORD_WRAP);
		}
	    }
	}
    }
}

#if HAVE_RUBY
static char *no_stars(const char *name)
{
  if (name[0] == '*')
    {
      char *val;
      val = (char *)CALLOC(strlen(name), sizeof(char));
      strncpy(val, (char *)(name + 1), strlen(name) - 2);
      return(val);
    }
  else return(copy_string(name));
}
#endif

static void prefs_variable_set(const char *name, XEN val)
{
#if HAVE_GUILE
  if (XEN_DEFINED_P(name))
    XEN_VARIABLE_SET(XEN_NAME_AS_C_STRING_TO_VARIABLE(name), val);
#endif

#if HAVE_GAUCHE || HAVE_FORTH
  if (XEN_DEFINED_P(name))
    XEN_VARIABLE_SET(name, val);
#endif

#if HAVE_RUBY
  {
    char *str;
    str = no_stars(name);
    if (XEN_DEFINED_P(str))
      XEN_VARIABLE_SET(str, val);
    FREE(str);
  }
#endif
}

static void prefs_variable_save(FILE *fd, const char *name, const char *file, XEN val)
{
#if HAVE_SCHEME
  if (file)
    fprintf(fd, "(if (not (provided? 'snd-%s.scm)) (load-from-path \"%s.scm\"))\n", file, file);
  fprintf(fd, "(set! %s %s)\n", name, XEN_AS_STRING(val));
#endif
#if HAVE_RUBY
  char *str;
  str = no_stars(name);
  if (file)
    fprintf(fd, "require \"%s\"\n", file);
  fprintf(fd, "set_%s(%s)\n", str, XEN_AS_STRING(val));
  FREE(str);
#endif
#if HAVE_FORTH
  if (file)
    fprintf(fd, "require %s\n", file);
  fprintf(fd, "%s set-%s drop\n", XEN_AS_STRING(val), name);
#endif
}

static XEN prefs_variable_get(const char *name)
{
#if HAVE_SCHEME || HAVE_FORTH
  if (XEN_DEFINED_P(name))
    return(XEN_NAME_AS_C_STRING_TO_VALUE(name));
#endif

#if HAVE_RUBY
  {
    char *str;
    XEN val = XEN_FALSE;
    str = no_stars(name);
    if (XEN_DEFINED_P(str))
      val = XEN_NAME_AS_C_STRING_TO_VALUE(name);
    FREE(str);
    return(val);
  }
#endif
  
  return(XEN_FALSE);
}

static void xen_load_file_with_path_and_extension(const char *file)
{
  /* file is bare (no extension, no directory) file name */
  char *str;
  str = mus_format("%s.%s", file, XEN_FILE_EXTENSION);
  XEN_LOAD_FILE_WITH_PATH(str);
  FREE(str);
}

static void prefs_function_call_0(const char *func)
{
  char *str;
#if HAVE_SCHEME
  str = mus_format("(%s)\n", func);
#endif
#if HAVE_RUBY
  str = mus_format("%s()\n", TO_PROC_NAME(func));
#endif
#if HAVE_FORTH
  str = mus_format("%s\n", func);
#endif
  XEN_EVAL_C_STRING(str);
  FREE(str);
}

static void prefs_function_call_1(const char *func, XEN arg)
{
  char *str;
#if HAVE_SCHEME
  str = mus_format("(%s %s)\n", func, XEN_AS_STRING(arg));
#endif
#if HAVE_RUBY
  str = mus_format("%s(%s)\n", TO_PROC_NAME(func), XEN_AS_STRING(arg));
#endif
#if HAVE_FORTH
  str = mus_format("%s %s\n", XEN_AS_STRING(arg), func);
#endif
  XEN_EVAL_C_STRING(str);
  FREE(str);
}

static void prefs_function_save_0(FILE *fd, const char *name, const char *file)
{
#if HAVE_SCHEME
  if (file)
    fprintf(fd, "(if (not (provided? 'snd-%s.scm)) (load-from-path \"%s.scm\"))\n", file, file);
  fprintf(fd, "(%s)\n", name);
#endif
#if HAVE_RUBY
  char *str;
  str = TO_PROC_NAME(name);
  if (file)
    fprintf(fd, "require \"%s\"\n", file);
  fprintf(fd, "%s()\n", str);
  FREE(str);
#endif
#if HAVE_FORTH
  if (file)
    fprintf(fd, "require %s\n", file);
  fprintf(fd, "%s\n", name); /* drop?? */
#endif
}

static void prefs_function_save_1(FILE *fd, const char *name, const char *file, XEN val)
{
#if HAVE_SCHEME
  if (file)
    fprintf(fd, "(if (not (provided? 'snd-%s.scm)) (load-from-path \"%s.scm\"))\n", file, file);
  fprintf(fd, "(%s %s)\n", name, XEN_AS_STRING(val));
#endif
#if HAVE_RUBY
  char *str;
  str = TO_PROC_NAME(name);
  if (file)
    fprintf(fd, "require \"%s\"\n", file);
  fprintf(fd, "%s(%s)\n", str, XEN_AS_STRING(val));
  FREE(str);
#endif
#if HAVE_FORTH
  if (file)
    fprintf(fd, "require %s\n", file);
  fprintf(fd, "%s %s drop\n", XEN_AS_STRING(val), name);
#endif
}



/* ---------------- auto-resize ---------------- */

static bool rts_auto_resize = DEFAULT_AUTO_RESIZE;
static void reflect_auto_resize(prefs_info *prf) {SET_TOGGLE(prf->toggle, auto_resize(ss));}
static void resize_toggle(prefs_info *prf) {set_auto_resize(GET_TOGGLE(prf->toggle));}
static void revert_auto_resize(prefs_info *prf) {set_auto_resize(rts_auto_resize);}
static void save_auto_resize(prefs_info *prf, FILE *ignore) {rts_auto_resize = auto_resize(ss);}


/* ---------------- ask-before-overwrite ---------------- */

static bool rts_ask_before_overwrite = DEFAULT_ASK_BEFORE_OVERWRITE;
static void reflect_ask_before_overwrite(prefs_info *prf) {SET_TOGGLE(prf->toggle, ask_before_overwrite(ss));}
static void overwrite_toggle(prefs_info *prf) {set_ask_before_overwrite(GET_TOGGLE(prf->toggle));}
static void revert_ask_before_overwrite(prefs_info *prf) {set_ask_before_overwrite(rts_ask_before_overwrite);}
static void save_ask_before_overwrite(prefs_info *prf, FILE *ignore) {rts_ask_before_overwrite = ask_before_overwrite(ss);}


/* ---------------- show-controls ---------------- */

static bool rts_show_controls = DEFAULT_SHOW_CONTROLS;
static void reflect_show_controls(prefs_info *prf) {SET_TOGGLE(prf->toggle, in_show_controls(ss));}
static void controls_toggle(prefs_info *prf) {in_set_show_controls(ss, GET_TOGGLE(prf->toggle));}
static void revert_show_controls(prefs_info *prf) {in_set_show_controls(ss, rts_show_controls);}
static void save_show_controls(prefs_info *prf, FILE *ignore) {rts_show_controls = in_show_controls(ss);}


/* ---------------- just-sounds ---------------- */

static bool rts_just_sounds = DEFAULT_JUST_SOUNDS;
static void prefs_reflect_just_sounds(prefs_info *prf) {SET_TOGGLE(prf->toggle, just_sounds(ss));}
static void just_sounds_toggle(prefs_info *prf) {set_just_sounds(GET_TOGGLE(prf->toggle));}
static void revert_just_sounds(prefs_info *prf) {set_just_sounds(rts_just_sounds);}
static void save_just_sounds(prefs_info *prf, FILE *ignore) {rts_just_sounds = just_sounds(ss);}


/* ---------------- verbose-cursor ---------------- */

static bool rts_verbose_cursor = DEFAULT_VERBOSE_CURSOR;
static void reflect_verbose_cursor(prefs_info *prf) {SET_TOGGLE(prf->toggle, verbose_cursor(ss));}
static void verbose_cursor_toggle(prefs_info *prf) {in_set_verbose_cursor(GET_TOGGLE(prf->toggle));}
static void revert_verbose_cursor(prefs_info *prf) {in_set_verbose_cursor(rts_verbose_cursor);}
static void save_verbose_cursor(prefs_info *prf, FILE *ignore) {rts_verbose_cursor = verbose_cursor(ss);}


/* ---------------- graphs-horizontal ---------------- */

static bool rts_graphs_horizontal = DEFAULT_GRAPHS_HORIZONTAL;
static void reflect_graphs_horizontal(prefs_info *prf) {SET_TOGGLE(prf->toggle, graphs_horizontal(ss));}
static void graphs_horizontal_toggle(prefs_info *prf) {in_set_graphs_horizontal(GET_TOGGLE(prf->toggle));}
static void revert_graphs_horizontal(prefs_info *prf) {in_set_graphs_horizontal(rts_graphs_horizontal);}
static void save_graphs_horizontal(prefs_info *prf, FILE *ignore) {rts_graphs_horizontal = graphs_horizontal(ss);}


/* ---------------- show-y-zero ---------------- */

static bool rts_show_y_zero = DEFAULT_SHOW_Y_ZERO;
static void reflect_show_y_zero(prefs_info *prf) {SET_TOGGLE(prf->toggle, show_y_zero(ss));}
static void y_zero_toggle(prefs_info *prf) {in_set_show_y_zero(GET_TOGGLE(prf->toggle));}
static void revert_show_y_zero(prefs_info *prf) {in_set_show_y_zero(rts_show_y_zero);}
static void save_show_y_zero(prefs_info *prf, FILE *ignore) {rts_show_y_zero = show_y_zero(ss);}


/* ---------------- show-grid ---------------- */

static with_grid_t rts_show_grid = DEFAULT_SHOW_GRID;
static void reflect_show_grid(prefs_info *prf) {SET_TOGGLE(prf->toggle, show_grid(ss) == WITH_GRID);}
static void grid_toggle(prefs_info *prf) {in_set_show_grid(((GET_TOGGLE(prf->toggle)) ? WITH_GRID : NO_GRID));}
static void revert_show_grid(prefs_info *prf) {in_set_show_grid(rts_show_grid);}
static void save_show_grid(prefs_info *prf, FILE *ignore) {rts_show_grid = show_grid(ss);}


/* ---------------- fft-log-magnitude ---------------- */

static bool rts_fft_log_magnitude = DEFAULT_FFT_LOG_MAGNITUDE;
static void reflect_fft_log_magnitude(prefs_info *prf) {SET_TOGGLE(prf->toggle, fft_log_magnitude(ss));}
static void log_magnitude_toggle(prefs_info *prf) {in_set_fft_log_magnitude(GET_TOGGLE(prf->toggle));}
static void revert_fft_log_magnitude(prefs_info *prf) {in_set_fft_log_magnitude(rts_fft_log_magnitude);}
static void save_fft_log_magnitude(prefs_info *prf, FILE *ignore) {rts_fft_log_magnitude = fft_log_magnitude(ss);}


/* ---------------- fft-log-frequency ---------------- */

static bool rts_fft_log_frequency = DEFAULT_FFT_LOG_FREQUENCY;
static void reflect_fft_log_frequency(prefs_info *prf) {SET_TOGGLE(prf->toggle, fft_log_frequency(ss));}
static void log_frequency_toggle(prefs_info *prf) {in_set_fft_log_frequency(GET_TOGGLE(prf->toggle));}
static void revert_fft_log_frequency(prefs_info *prf) {in_set_fft_log_frequency(rts_fft_log_frequency);}
static void save_fft_log_frequency(prefs_info *prf, FILE *ignore) {rts_fft_log_frequency = fft_log_frequency(ss);}


/* ---------------- show-backtrace ---------------- */

static bool rts_show_backtrace = DEFAULT_SHOW_BACKTRACE;
static void reflect_show_backtrace(prefs_info *prf) {SET_TOGGLE(prf->toggle, show_backtrace(ss));}
static void show_backtrace_toggle(prefs_info *prf) {set_show_backtrace(GET_TOGGLE(prf->toggle));}
static void revert_show_backtrace(prefs_info *prf) {set_show_backtrace(rts_show_backtrace);}
static void save_show_backtrace(prefs_info *prf, FILE *ignore) {rts_show_backtrace = show_backtrace(ss);}


/* ---------------- dac-combines-channels ---------------- */

static bool rts_dac_combines_channels = DEFAULT_DAC_COMBINES_CHANNELS;
static void reflect_dac_combines_channels(prefs_info *prf) {SET_TOGGLE(prf->toggle, dac_combines_channels(ss));}
static void dac_combines_channels_toggle(prefs_info *prf) {set_dac_combines_channels(GET_TOGGLE(prf->toggle));}
static void revert_dac_combines_channels(prefs_info *prf) {set_dac_combines_channels(rts_dac_combines_channels);}
static void save_dac_combines_channels(prefs_info *prf, FILE *ignore) {rts_dac_combines_channels = dac_combines_channels(ss);}


/* ---------------- recorder-autoload ---------------- */

static bool rts_recorder_autoload = false; /* DEFAULT_RECORDER_AUTOLOAD is defined in snd-rec.c */
static void reflect_recorder_autoload(prefs_info *prf) {SET_TOGGLE(prf->toggle, rec_autoload());}
static void recorder_autoload_toggle(prefs_info *prf) {rec_set_autoload(GET_TOGGLE(prf->toggle));}
static void revert_recorder_autoload(prefs_info *prf) {rec_set_autoload(rts_recorder_autoload);}
static void save_recorder_autoload(prefs_info *prf, FILE *ignore) {rts_recorder_autoload = rec_autoload();}


/* ---------------- show-listener ---------------- */

static bool rts_show_listener = false, prefs_show_listener = false;

static void reflect_show_listener(prefs_info *prf) 
{
  prefs_show_listener = listener_is_visible();
  SET_TOGGLE(prf->toggle, prefs_show_listener);
}

static void show_listener_toggle(prefs_info *prf)
{
  prefs_show_listener = GET_TOGGLE(prf->toggle);
  handle_listener(prefs_show_listener);
}

static void save_show_listener(prefs_info *prf, FILE *fd)
{
  rts_show_listener = prefs_show_listener;
  if (GET_TOGGLE(prf->toggle))
    prefs_function_save_1(fd, "show-listener", NULL, XEN_TRUE);
}

static void revert_show_listener(prefs_info *prf)
{
  prefs_show_listener = rts_show_listener;
  handle_listener(rts_show_listener);
}

static void clear_show_listener(prefs_info *prf)
{
  prefs_show_listener = false;
  handle_listener(false);
}



/* ---------------- basic-color ---------------- */

/* we need the original color (Clear), the last saved color (Revert)
 *   the colors are updated continuously, so the current color variable (and the reflection func) is irrelevant
 *   so: 
 *       set original in snd-gxmain or somewhere: requires ss->sgx fields
 *       set rts in prefs dialog startup
 *       reflect_color: a no-op
 *       save_color: save current color_t value in rts value (actual fd output dealt with elsewhere)
 *       clear_color: set to original (leave rts value alone)
 *       revert_color: set to rts (leave original alone)
 *       help_color: built-in via color variable name
 *       color_func: set color based on rgb values
 */

static color_t saved_basic_color;
static void save_basic_color(prefs_info *prf, FILE *ignore) {saved_basic_color = ss->sgx->basic_color;}
static void basic_color_func(prefs_info *prf, float r, float g, float b) {set_basic_color(rgb_to_color(r, g, b));}

static void revert_basic_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_basic_color); 
  set_basic_color(saved_basic_color);
}

static void clear_basic_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->sgx->basic_color);
  set_basic_color(ss->sgx->orig_basic_color);
}


/* ---------------- highlight-color ---------------- */

static color_t saved_highlight_color;
static void save_highlight_color(prefs_info *prf, FILE *ignore) {saved_highlight_color = ss->sgx->highlight_color;}
static void highlight_color_func(prefs_info *prf, float r, float g, float b) {set_highlight_color(rgb_to_color(r, g, b));}

static void revert_highlight_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_highlight_color); 
  set_highlight_color(saved_highlight_color);
}

static void clear_highlight_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->sgx->orig_highlight_color); 
  set_highlight_color(ss->sgx->orig_highlight_color);
}


/* ---------------- position-color ---------------- */

static color_t saved_position_color;
static void save_position_color(prefs_info *prf, FILE *ignore) {saved_position_color = ss->sgx->position_color;}
static void position_color_func(prefs_info *prf, float r, float g, float b) {set_position_color(rgb_to_color(r, g, b));}

static void revert_position_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_position_color); 
  set_position_color(saved_position_color);
}

static void clear_position_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->sgx->orig_position_color); 
  set_position_color(ss->sgx->orig_position_color);
}


/* ---------------- zoom-color ---------------- */

static color_t saved_zoom_color;
static void save_zoom_color(prefs_info *prf, FILE *ignore) {saved_zoom_color = ss->sgx->zoom_color;}
static void zoom_color_func(prefs_info *prf, float r, float g, float b) {set_zoom_color(rgb_to_color(r, g, b));}

static void revert_zoom_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_zoom_color); 
  set_zoom_color(saved_zoom_color);
}

static void clear_zoom_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->sgx->orig_zoom_color); 
  set_zoom_color(ss->sgx->orig_zoom_color);
}


/* ---------------- cursor-color ---------------- */

static color_t saved_cursor_color;
static void save_cursor_color(prefs_info *prf, FILE *ignore) {saved_cursor_color = ss->sgx->cursor_color;}

static void cursor_color_func(prefs_info *prf, float r, float g, float b)
{
  color_cursor(rgb_to_color(r, g, b));
  for_each_chan(update_graph);
}

static void revert_cursor_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_cursor_color); 
  color_cursor(saved_cursor_color);
}

static void clear_cursor_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->sgx->orig_cursor_color); 
  color_cursor(ss->sgx->orig_cursor_color);
}


/* ---------------- data-color ---------------- */

static color_t saved_data_color;
static void save_data_color(prefs_info *prf, FILE *ignore) {saved_data_color = ss->sgx->data_color;}
static void data_color_func(prefs_info *prf, float r, float g, float b) {set_data_color(rgb_to_color(r, g, b));}

static void revert_data_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_data_color); 
  set_data_color(saved_data_color);
}

static void clear_data_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->sgx->orig_data_color); 
  set_data_color(ss->sgx->orig_data_color);
}


/* ---------------- graph-color ---------------- */

static color_t saved_graph_color;
static void save_graph_color(prefs_info *prf, FILE *ignore) {saved_graph_color = ss->sgx->graph_color;}
static void graph_color_func(prefs_info *prf, float r, float g, float b) {set_graph_color(rgb_to_color(r, g, b));}

static void revert_graph_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_graph_color); 
  set_graph_color(saved_graph_color);
}

static void clear_graph_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->sgx->orig_graph_color); 
  set_graph_color(ss->sgx->orig_graph_color);
}


/* ---------------- selected-data-color ---------------- */

static color_t saved_selected_data_color;
static void save_selected_data_color(prefs_info *prf, FILE *ignore) {saved_selected_data_color = ss->sgx->selected_data_color;}
static void selected_data_color_func(prefs_info *prf, float r, float g, float b) {set_selected_data_color(rgb_to_color(r, g, b));}

static void revert_selected_data_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_selected_data_color); 
  set_selected_data_color(saved_selected_data_color);
}

static void clear_selected_data_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->sgx->orig_selected_data_color); 
  set_selected_data_color(ss->sgx->orig_selected_data_color);
}


/* ---------------- selected-graph-color ---------------- */

static color_t saved_selected_graph_color;
static void save_selected_graph_color(prefs_info *prf, FILE *ignore) {saved_selected_graph_color = ss->sgx->selected_graph_color;}
static void selected_graph_color_func(prefs_info *prf, float r, float g, float b) {set_selected_graph_color(rgb_to_color(r, g, b));}

static void revert_selected_graph_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_selected_graph_color); 
  set_selected_graph_color(saved_selected_graph_color);
}

static void clear_selected_graph_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->sgx->orig_selected_graph_color); 
  set_selected_graph_color(ss->sgx->orig_selected_graph_color);
}


/* ---------------- selection-color ---------------- */

static void set_selection_color(color_t color)
{
  color_selection(color);
  for_each_chan(update_graph);
}

static color_t saved_selection_color;
static void save_selection_color(prefs_info *prf, FILE *ignore) {saved_selection_color = ss->sgx->selection_color;}
static void selection_color_func(prefs_info *prf, float r, float g, float b) {set_selection_color(rgb_to_color(r, g, b));}

static void revert_selection_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_selection_color); 
  set_selection_color(saved_selection_color);
}

static void clear_selection_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->sgx->orig_selection_color); 
  set_selection_color(ss->sgx->orig_selection_color);
}


/* ---------------- mark-color ---------------- */

static color_t saved_mark_color;
static void save_mark_color(prefs_info *prf, FILE *ignore) {saved_mark_color = ss->sgx->mark_color;}
static void mark_color_func(prefs_info *prf, float r, float g, float b) {color_marks(rgb_to_color(r, g, b));}

static void revert_mark_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_mark_color);
  color_marks(saved_mark_color);
}

static void clear_mark_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->sgx->orig_mark_color);
  color_marks(ss->sgx->orig_mark_color);
}


/* ---------------- mix-color (waveform) ---------------- */

static color_t saved_mix_color;
static void save_mix_color(prefs_info *prf, FILE *ignore) {saved_mix_color = ss->sgx->mix_color;}
static void mix_color_func(prefs_info *prf, float r, float g, float b) {color_mixes(rgb_to_color(r, g, b));}

static void revert_mix_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_mix_color);
  color_mixes(saved_mix_color);
}

static void clear_mix_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->sgx->orig_mix_color);
  color_mixes(ss->sgx->orig_mix_color);
}


/* ---------------- listener-color ---------------- */

static color_t saved_listener_color;
static void save_listener_color(prefs_info *prf, FILE *ignore) {saved_listener_color = ss->sgx->listener_color;}
static void listener_color_func(prefs_info *prf, float r, float g, float b) {color_listener(rgb_to_color(r, g, b));}

static void revert_listener_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_listener_color);
  color_listener(saved_listener_color);
}

static void clear_listener_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->sgx->orig_listener_color);
  color_listener(ss->sgx->orig_listener_color);
}


/* ---------------- listener-text-color ---------------- */

static color_t saved_listener_text_color;
static void save_listener_text_color(prefs_info *prf, FILE *ignore) {saved_listener_text_color = ss->sgx->listener_text_color;}
static void listener_text_color_func(prefs_info *prf, float r, float g, float b) {color_listener_text(rgb_to_color(r, g, b));}

static void revert_listener_text_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_listener_text_color);
  color_listener_text(saved_listener_text_color);
}

static void clear_listener_text_color(prefs_info *prf) 
{
  scale_set_color(prf, ss->sgx->orig_listener_text_color);
  color_listener_text(ss->sgx->orig_listener_text_color);
}


/* ---------------- axis-label-font ---------------- */

static char *rts_axis_label_font = NULL;

static TIMEOUT_TYPE axis_label_font_error_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  SET_TEXT(prf->text, axis_label_font(ss));
  TIMEOUT_RESULT
}

static void save_axis_label_font(prefs_info *prf, FILE *ignore)
{
  if (rts_axis_label_font) FREE(rts_axis_label_font);
  rts_axis_label_font = copy_string(axis_label_font(ss));
}

static void reflect_axis_label_font(prefs_info *prf) {SET_TEXT(prf->text, axis_label_font(ss));}
static void revert_axis_label_font(prefs_info *prf) {set_axis_label_font(rts_axis_label_font);}
static void clear_axis_label_font(prefs_info *prf) {set_axis_label_font(ss->orig_axis_label_font);}

static void axis_label_font_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((!str) || (!(*str)))
    {
      SET_TEXT(prf->text, axis_label_font(ss));
      return;
    }
  if (!(set_axis_label_font(str)))
    {
      SET_TEXT(prf->text, "can't find that font");
      TIMEOUT(axis_label_font_error_erase_func);
    }
  if (str) {FREE_TEXT(str);}
}

/* ---------------- axis-numbers-font ---------------- */

static char *rts_axis_numbers_font = NULL;

static TIMEOUT_TYPE axis_numbers_font_error_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  SET_TEXT(prf->text, axis_numbers_font(ss));
  TIMEOUT_RESULT
}

static void save_axis_numbers_font(prefs_info *prf, FILE *ignore)
{
  if (rts_axis_numbers_font) FREE(rts_axis_numbers_font);
  rts_axis_numbers_font = copy_string(axis_numbers_font(ss));
}

static void reflect_axis_numbers_font(prefs_info *prf) {SET_TEXT(prf->text, axis_numbers_font(ss));}
static void revert_axis_numbers_font(prefs_info *prf) {set_axis_numbers_font(rts_axis_numbers_font);}
static void clear_axis_numbers_font(prefs_info *prf) {set_axis_numbers_font(ss->orig_axis_numbers_font);}

static void axis_numbers_font_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((!str) || (!(*str)))
    {
      SET_TEXT(prf->text, axis_numbers_font(ss));
      return;
    }
  if (!(set_axis_numbers_font(str)))
    {
      SET_TEXT(prf->text, "can't find that font");
      TIMEOUT(axis_numbers_font_error_erase_func);
    }
  if (str) {FREE_TEXT(str);}
}

/* ---------------- peaks-font ---------------- */

static char *rts_peaks_font = NULL;

static TIMEOUT_TYPE peaks_font_error_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  SET_TEXT(prf->text, peaks_font(ss));
  TIMEOUT_RESULT
}

static void save_peaks_font(prefs_info *prf, FILE *ignore)
{
  if (rts_peaks_font) FREE(rts_peaks_font);
  rts_peaks_font = copy_string(peaks_font(ss));
}

static void reflect_peaks_font(prefs_info *prf) {SET_TEXT(prf->text, peaks_font(ss));}
static void revert_peaks_font(prefs_info *prf) {set_peaks_font(rts_peaks_font);}
static void clear_peaks_font(prefs_info *prf) {set_peaks_font(ss->orig_peaks_font);}

static void peaks_font_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((!str) || (!(*str)))
    {
      SET_TEXT(prf->text, peaks_font(ss));
      return;
    }
  if (!(set_peaks_font(str)))
    {
      SET_TEXT(prf->text, "can't find that font");
      TIMEOUT(peaks_font_error_erase_func);
    }
  if (str) {FREE_TEXT(str);}
}

/* ---------------- bold-peaks-font ---------------- */

static char *rts_bold_peaks_font = NULL;

static TIMEOUT_TYPE bold_peaks_font_error_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  SET_TEXT(prf->text, bold_peaks_font(ss));
  TIMEOUT_RESULT
}

static void save_bold_peaks_font(prefs_info *prf, FILE *ignore)
{
  if (rts_bold_peaks_font) FREE(rts_bold_peaks_font);
  rts_bold_peaks_font = copy_string(bold_peaks_font(ss));
}

static void reflect_bold_peaks_font(prefs_info *prf) {SET_TEXT(prf->text, bold_peaks_font(ss));}
static void revert_bold_peaks_font(prefs_info *prf) {set_bold_peaks_font(rts_bold_peaks_font);}
static void clear_bold_peaks_font(prefs_info *prf) {set_bold_peaks_font(ss->orig_bold_peaks_font);}

static void bold_peaks_font_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((!str) || (!(*str)))
    {
      SET_TEXT(prf->text, bold_peaks_font(ss));
      return;
    }
  if (!(set_bold_peaks_font(str)))
    {
      SET_TEXT(prf->text, "can't find that font");
      TIMEOUT(bold_peaks_font_error_erase_func);
    }
  if (str) {FREE_TEXT(str);}
}

/* ---------------- tiny-font ---------------- */

static char *rts_tiny_font = NULL;

static TIMEOUT_TYPE tiny_font_error_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  SET_TEXT(prf->text, tiny_font(ss));
  TIMEOUT_RESULT
}

static void save_tiny_font(prefs_info *prf, FILE *ignore)
{
  if (rts_tiny_font) FREE(rts_tiny_font);
  rts_tiny_font = copy_string(tiny_font(ss));
}

static void reflect_tiny_font(prefs_info *prf) {SET_TEXT(prf->text, tiny_font(ss));}
static void revert_tiny_font(prefs_info *prf) {set_tiny_font(rts_tiny_font);}
static void clear_tiny_font(prefs_info *prf) {set_tiny_font(ss->orig_tiny_font);}

static void tiny_font_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((!str) || (!(*str)))
    {
      SET_TEXT(prf->text, tiny_font(ss));
      return;
    }
  if (!(set_tiny_font(str)))
    {
      SET_TEXT(prf->text, "can't find that font");
      TIMEOUT(tiny_font_error_erase_func);
    }
  if (str) {FREE_TEXT(str);}
}


/* ---------------- listener-font ---------------- */

static char *rts_listener_font = NULL;

static TIMEOUT_TYPE listener_font_error_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  SET_TEXT(prf->text, listener_font(ss));
  TIMEOUT_RESULT
}

static void save_listener_font(prefs_info *prf, FILE *ignore)
{
  if (rts_listener_font) FREE(rts_listener_font);
  rts_listener_font = copy_string(listener_font(ss));
}

static void reflect_listener_font(prefs_info *prf) {SET_TEXT(prf->text, listener_font(ss));}
static void revert_listener_font(prefs_info *prf) {set_listener_font(rts_listener_font);}
static void clear_listener_font(prefs_info *prf) {set_listener_font(ss->orig_listener_font);}

static void listener_font_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((!str) || (!(*str)))
    {
      SET_TEXT(prf->text, listener_font(ss));
      return;
    }
  if (!(set_listener_font(str)))
    {
      SET_TEXT(prf->text, "can't find that font");
      TIMEOUT(listener_font_error_erase_func);
    }
  if (str) {FREE_TEXT(str);}
}


/* ---------------- save-state-file ---------------- */

static char *rts_save_state_file = NULL;

static void reflect_save_state_file(prefs_info *prf) 
{
  SET_TEXT(prf->text, save_state_file(ss));
}

static void revert_save_state_file(prefs_info *prf) 
{
  if (save_state_file(ss)) FREE(save_state_file(ss));
  in_set_save_state_file(copy_string(rts_save_state_file));
}

static void save_save_state_file(prefs_info *prf, FILE *ignore) 
{
  if (rts_save_state_file) FREE(rts_save_state_file);
  rts_save_state_file = copy_string(save_state_file(ss));
}

static void save_state_file_text(prefs_info *prf)
{
  char *str, *file = NULL;
  str = GET_TEXT(prf->text);
  if ((!str) || (!(*str))) 
    file = DEFAULT_SAVE_STATE_FILE; /* local, not freed */
  else file = str;
  if (save_state_file(ss)) FREE(save_state_file(ss));
  in_set_save_state_file(copy_string(file));
  if (str) {FREE_TEXT(str);}
}


/* ---------------- temp-dir ---------------- */

static char *rts_temp_dir = NULL;

static void reflect_temp_dir(prefs_info *prf) 
{
  SET_TEXT(prf->text, temp_dir(ss));
}

static void revert_temp_dir(prefs_info *prf) 
{
  if (temp_dir(ss)) FREE(temp_dir(ss));
  set_temp_dir(copy_string(rts_temp_dir));
}

static void save_temp_dir(prefs_info *prf, FILE *ignore) 
{
  if (rts_temp_dir) FREE(rts_temp_dir);
  rts_temp_dir = copy_string(temp_dir(ss));
}

static TIMEOUT_TYPE temp_dir_error_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  SET_TEXT(prf->text, rts_temp_dir);
  TIMEOUT_RESULT
}

static void temp_dir_text(prefs_info *prf)
{
  char *str = NULL, *dir = NULL;
  str = GET_TEXT(prf->text);
  if ((!str) || (!(*str))) 
    dir = MUS_DEFAULT_TEMP_DIR;
  else dir = str;
  if (local_access(dir))
    {
      if (temp_dir(ss)) FREE(temp_dir(ss));
      set_temp_dir(copy_string(dir));
    }
  else
    {
      SET_TEXT(prf->text, "can't access that directory");
      TIMEOUT(temp_dir_error_erase_func);
    }
#if USE_MOTIF
  if (str) XtFree(str);
#endif
}


/* ---------------- save-dir ---------------- */

static char *rts_save_dir = NULL;

static void reflect_save_dir(prefs_info *prf) 
{
  SET_TEXT(prf->text, save_dir(ss));
}

static void revert_save_dir(prefs_info *prf) 
{
  if (save_dir(ss)) FREE(save_dir(ss));
  set_save_dir(copy_string(rts_save_dir));
}

static void save_save_dir(prefs_info *prf, FILE *ignore) 
{
  if (rts_save_dir) FREE(rts_save_dir);
  rts_save_dir = copy_string(save_dir(ss));
}

static TIMEOUT_TYPE save_dir_error_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  SET_TEXT(prf->text, rts_save_dir);
  TIMEOUT_RESULT
}

static void save_dir_text(prefs_info *prf)
{
  char *str = NULL, *dir = NULL;
  str = GET_TEXT(prf->text);
  if ((!str) || (!(*str))) 
    dir = MUS_DEFAULT_SAVE_DIR;
  else dir = str;
  if (local_access(dir))
    {
      if (save_dir(ss)) FREE(save_dir(ss));
      set_save_dir(copy_string(dir));
    }
  else
    {
      SET_TEXT(prf->text, "can't access that directory");
      TIMEOUT(save_dir_error_erase_func);
    }
#if USE_MOTIF
  if (str) XtFree(str);
#endif
}


#if HAVE_LADSPA
/* ---------------- ladspa-dir ---------------- */

static char *rts_ladspa_dir = NULL;

static void reflect_ladspa_dir(prefs_info *prf)
{
  SET_TEXT(prf->text, ladspa_dir(ss));
}

static void revert_ladspa_dir(prefs_info *prf)
{
  if (ladspa_dir(ss)) FREE(ladspa_dir(ss));
  set_ladspa_dir(copy_string(rts_ladspa_dir));
}

static void save_ladspa_dir(prefs_info *prf, FILE *ignore)
{
  if (rts_ladspa_dir) FREE(rts_ladspa_dir);
  rts_ladspa_dir = copy_string(ladspa_dir(ss));
}

static void ladspa_dir_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if (ladspa_dir(ss)) FREE(ladspa_dir(ss));
  if (str)
    {
      set_ladspa_dir(copy_string(str));
      FREE_TEXT(str);
    }
  else set_ladspa_dir(copy_string(DEFAULT_LADSPA_DIR));
}
#endif


/* ---------------- view-files directory ---------------- */

static char *rts_vf_directory = NULL;

static void reflect_view_files_directory(prefs_info *prf)
{
  SET_TEXT(prf->text, view_files_find_any_directory());
}

static void revert_view_files_directory(prefs_info *prf)
{
  if (rts_vf_directory)
    view_files_add_directory(NULL_WIDGET, (const char *)rts_vf_directory);
}

static void save_view_files_directory(prefs_info *prf, FILE *fd)
{
  if (rts_vf_directory) FREE(rts_vf_directory);
  rts_vf_directory = copy_string(view_files_find_any_directory());
  if (rts_vf_directory) 
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s \"%s\")\n", S_add_directory_to_view_files_list, rts_vf_directory);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(\"%s\")\n", TO_PROC_NAME(S_add_directory_to_view_files_list), rts_vf_directory);
#endif
#if HAVE_FORTH
      fprintf(fd, "\"%s\" %s drop\n", rts_vf_directory, S_add_directory_to_view_files_list);
#endif
    }
}

static void view_files_directory_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if (str)
    {
      view_files_add_directory(NULL_WIDGET, (const char *)str);
      FREE_TEXT(str);
    }
}


/* ---------------- html-program ---------------- */

static char *rts_html_program = NULL;

static void reflect_html_program(prefs_info *prf)
{
  SET_TEXT(prf->text, html_program(ss));
}

static void revert_html_program(prefs_info *prf)
{
  if (html_program(ss)) FREE(html_program(ss));
  set_html_program(copy_string(rts_html_program));
}

static void save_html_program(prefs_info *prf, FILE *ignore)
{
  if (rts_html_program) FREE(rts_html_program);
  rts_html_program = copy_string(html_program(ss));
}

static void html_program_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if (html_program(ss)) FREE(html_program(ss));
  if (str)
    {
      set_html_program(copy_string(str));
      FREE_TEXT(str);
    }
  else set_html_program(copy_string(DEFAULT_HTML_PROGRAM));
}


/* ---------------- listener-prompt ---------------- */

static char *rts_listener_prompt = NULL;

static void reflect_listener_prompt(prefs_info *prf)
{
  SET_TEXT(prf->text, listener_prompt(ss));
}

static void revert_listener_prompt(prefs_info *prf)
{
  if (rts_listener_prompt)
    {
      if (listener_prompt(ss)) FREE(listener_prompt(ss));
      set_listener_prompt(copy_string(rts_listener_prompt));
    }
}

static void save_listener_prompt(prefs_info *prf, FILE *ignore)
{
  if (rts_listener_prompt) FREE(rts_listener_prompt);
  rts_listener_prompt = copy_string(listener_prompt(ss));
}

static void listener_prompt_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if (str)
    {
      if (listener_prompt(ss)) FREE(listener_prompt(ss));
      set_listener_prompt(copy_string(str));
      FREE_TEXT(str);
    }
}


/* ---------------- recorder file name ---------------- */

static char *rts_recorder_filename = NULL;

static void reflect_recorder_filename(prefs_info *prf)
{
  SET_TEXT(prf->text, rec_filename());
}

static void revert_recorder_filename(prefs_info *prf)
{
  if (rts_recorder_filename) rec_set_filename(rts_recorder_filename);
}

static void save_recorder_filename(prefs_info *prf, FILE *ignore)
{
  if (rts_recorder_filename) FREE(rts_recorder_filename);
  rts_recorder_filename = copy_string(rec_filename());
}

static void recorder_filename_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      rec_set_filename(str);
      FREE_TEXT(str);
    }
}


/* ---------------- show-transform-peaks ---------------- */

static bool rts_show_transform_peaks = DEFAULT_SHOW_TRANSFORM_PEAKS;
static int rts_max_transform_peaks = DEFAULT_MAX_TRANSFORM_PEAKS;

static void reflect_transform_peaks(prefs_info *prf) 
{
  SET_TOGGLE(prf->toggle, show_transform_peaks(ss));
  int_to_textfield(prf->text, max_transform_peaks(ss));
}

static void revert_transform_peaks(prefs_info *prf) 
{
  in_set_show_transform_peaks(rts_show_transform_peaks);
  in_set_max_transform_peaks(rts_max_transform_peaks);
}

static void save_transform_peaks(prefs_info *prf, FILE *ignore)
{
  rts_show_transform_peaks = show_transform_peaks(ss);
  rts_max_transform_peaks = max_transform_peaks(ss);
}

static void transform_peaks_toggle(prefs_info *prf)
{
  in_set_show_transform_peaks(GET_TOGGLE(prf->toggle));
}

static void max_peaks_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int value = 0;
      redirect_errors_to(any_error_to_text, (void *)prf);
      value = string_to_int(str, 0, "max peaks");
      redirect_errors_to(NULL, NULL);
      if (!(prf->got_error))
	in_set_max_transform_peaks(value);
      FREE_TEXT(str);
    }
}



/* ---------------- show-mix-waveforms ---------------- */

static bool rts_show_mix_waveforms = DEFAULT_SHOW_MIX_WAVEFORMS;
static int rts_mix_waveform_height = DEFAULT_MIX_WAVEFORM_HEIGHT;

static void reflect_mix_waveforms(prefs_info *prf) 
{
  SET_TOGGLE(prf->toggle, show_mix_waveforms(ss));
  int_to_textfield(prf->text, mix_waveform_height(ss));
}

static void revert_mix_waveforms(prefs_info *prf) 
{
  in_set_show_mix_waveforms(rts_show_mix_waveforms);
  in_set_mix_waveform_height(rts_mix_waveform_height);
}

static void save_mix_waveforms(prefs_info *prf, FILE *ignore)
{
  rts_show_mix_waveforms = show_mix_waveforms(ss);
  rts_mix_waveform_height = mix_waveform_height(ss);
}

static void show_mix_waveforms_toggle(prefs_info *prf)
{
  in_set_show_mix_waveforms(GET_TOGGLE(prf->toggle));
}

static void mix_waveform_height_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int value = 0;
      redirect_errors_to(any_error_to_text, (void *)prf);
      value = string_to_int(str, 0, "mix waveform height");
      redirect_errors_to(NULL, NULL);
      if (!(prf->got_error))
	in_set_mix_waveform_height(value);
      FREE_TEXT(str);
    }
}


/* ---------------- selection-creates-region, max-regions ---------------- */

static bool rts_selection_creates_region = DEFAULT_SELECTION_CREATES_REGION;
static int rts_max_regions = DEFAULT_MAX_REGIONS;

static void reflect_selection_creates_region(prefs_info *prf) 
{
  SET_TOGGLE(prf->toggle, selection_creates_region(ss));
  int_to_textfield(prf->text, max_regions(ss));
}

static void revert_selection_creates_region(prefs_info *prf) 
{
  set_selection_creates_region(rts_selection_creates_region);
  in_set_max_regions(rts_max_regions);
}

static void save_selection_creates_region(prefs_info *prf, FILE *ignore)
{
  rts_selection_creates_region = selection_creates_region(ss);
  rts_max_regions = max_regions(ss);
}

static void selection_creates_region_toggle(prefs_info *prf)
{
  set_selection_creates_region(GET_TOGGLE(prf->toggle));
}

static void max_regions_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int value = 0;
      redirect_errors_to(any_error_to_text, (void *)prf);
      value = string_to_int(str, 0, "max regions");
      redirect_errors_to(NULL, NULL);
      if (!(prf->got_error))
	in_set_max_regions(value);
      FREE_TEXT(str);
    }
}


/* ---------------- sinc width ---------------- */

static int rts_sinc_width = DEFAULT_SINC_WIDTH;
static void reflect_sinc_width(prefs_info *prf) {int_to_textfield(prf->text, sinc_width(ss));}
static void revert_sinc_width(prefs_info *prf) {set_sinc_width(rts_sinc_width);}
static void save_sinc_width(prefs_info *prf, FILE *ignore) {rts_sinc_width = sinc_width(ss);}

static void sinc_width_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int value = 0;
      redirect_errors_to(any_error_to_text, (void *)prf);
      value = string_to_int(str, 0, "sinc width");
      redirect_errors_to(NULL, NULL);
      if (!(prf->got_error))
	set_sinc_width(value);
      FREE_TEXT(str);
    }
}


/* ---------------- print-length ---------------- */

static int rts_print_length = DEFAULT_PRINT_LENGTH;
static void reflect_print_length(prefs_info *prf) {int_to_textfield(prf->text, print_length(ss));}
static void revert_print_length(prefs_info *prf) {set_print_length(rts_print_length); mus_vct_set_print_length(rts_print_length);}
static void save_print_length(prefs_info *prf, FILE *ignore) {rts_print_length = print_length(ss);}

static void print_length_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int value = 0;
      redirect_errors_to(any_error_to_text, (void *)prf);
      value = string_to_int(str, 0, "print length");
      redirect_errors_to(NULL, NULL);
      if (!(prf->got_error))
	{
	  set_print_length(value);
	  mus_vct_set_print_length(value);
	  /* the clm array print length variable will be taken care of when ww.scm is loaded in the new context */
	}
      FREE_TEXT(str);
    }
}


/* ---------------- dac-size ---------------- */

static int rts_dac_size = DEFAULT_DAC_SIZE;
static void reflect_dac_size(prefs_info *prf) {int_to_textfield(prf->text, dac_size(ss));}
static void revert_dac_size(prefs_info *prf) {set_dac_size(rts_dac_size);}
static void save_dac_size(prefs_info *prf, FILE *ignore) {rts_dac_size = dac_size(ss);}

static void dac_size_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int value = 0;
      redirect_errors_to(any_error_to_text, (void *)prf);
      value = string_to_int(str, 0, "dac size");
      redirect_errors_to(NULL, NULL);
      if (!(prf->got_error))
	set_dac_size(value);
      FREE_TEXT(str);
    }
}


/* ---------------- recorder-buffer-size ---------------- */

static int rts_recorder_buffer_size = 4096; /* snd-rec.c */
static void reflect_recorder_buffer_size(prefs_info *prf) {int_to_textfield(prf->text, rec_buffer_size());}
static void revert_recorder_buffer_size(prefs_info *prf) {rec_set_buffer_size(rts_recorder_buffer_size);}
static void save_recorder_buffer_size(prefs_info *prf, FILE *ignore) {rts_recorder_buffer_size = rec_buffer_size();}

static void recorder_buffer_size_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int value = 0;
      redirect_errors_to(any_error_to_text, (void *)prf);
      value = string_to_int(str, 0, "recorder buffer size");
      redirect_errors_to(NULL, NULL);
      if (!(prf->got_error))
	rec_set_buffer_size(value);
      FREE_TEXT(str);
    }
}


/* ---------------- min-dB ---------------- */

static Float rts_min_dB = DEFAULT_MIN_DB;
static void reflect_min_dB(prefs_info *prf) {float_1_to_textfield(prf->text, min_dB(ss));}
static void revert_min_dB(prefs_info *prf) {set_min_dB(rts_min_dB);}
static void save_min_dB(prefs_info *prf, FILE *ignore) {rts_min_dB = min_dB(ss);}

static void min_dB_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      float value = 0.0;
      redirect_errors_to(any_error_to_text, (void *)prf);
      value = (float)string_to_Float(str, -100000.0, "min dB");
      redirect_errors_to(NULL, NULL);
      if ((!(prf->got_error)) && (value < 0.0))
	set_min_db(value); /* snd-chn.c -- redisplays */
      FREE_TEXT(str);
    }
}

/* ---------------- fft-window-beta ---------------- */

static Float rts_fft_window_beta = DEFAULT_FFT_WINDOW_BETA;

static void reflect_fft_window_beta(prefs_info *prf)
{
  SET_SCALE(fft_window_beta(ss) / prf->scale_max);
  float_to_textfield(prf->text, fft_window_beta(ss));
}

static void revert_fft_window_beta(prefs_info *prf) {in_set_fft_window_beta(rts_fft_window_beta);}
static void save_fft_window_beta(prefs_info *prf, FILE *ignore) {rts_fft_window_beta = fft_window_beta(ss);}
static void fft_window_beta_scale_callback(prefs_info *prf) {in_set_fft_window_beta(GET_SCALE() * prf->scale_max);}

static void fft_window_beta_text_callback(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      float value = 0.0;
      redirect_errors_to(any_error_to_text, (void *)prf);
      value = (float)string_to_Float(str, 0.0, "fft beta");
      redirect_errors_to(NULL, NULL);
      if ((!(prf->got_error)) && (value <= prf->scale_max))
	{
	  in_set_fft_window_beta(value);
	  SET_SCALE(value / prf->scale_max);
	}
      FREE_TEXT(str);
    }
}


/* ---------------- grid-density ---------------- */

static Float rts_grid_density = DEFAULT_GRID_DENSITY;

static void reflect_grid_density(prefs_info *prf)
{
  SET_SCALE(grid_density(ss) / prf->scale_max);
  float_to_textfield(prf->text, grid_density(ss));
}

static void revert_grid_density(prefs_info *prf) {in_set_grid_density(rts_grid_density);}
static void save_grid_density(prefs_info *prf, FILE *ignore) {rts_grid_density = grid_density(ss);}
static void grid_density_scale_callback(prefs_info *prf) {in_set_grid_density(GET_SCALE() * prf->scale_max);}

static void grid_density_text_callback(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      float value = 0.0;
      redirect_errors_to(any_error_to_text, (void *)prf);
      value = (float)string_to_Float(str, 0.0, "grid density");
      redirect_errors_to(NULL, NULL);
      if ((!(prf->got_error)) && (value <= prf->scale_max))
	{
	  in_set_grid_density(value);
	  SET_SCALE(value / prf->scale_max);
	}
      FREE_TEXT(str);
    }
}


/* ---------------- sync choice ---------------- */

#define SYNC_WITHIN_EACH_SOUND 2
#define SYNC_ACROSS_ALL_SOUNDS 1
#define SYNC_DISABLED 0
#define SYNC_UNSET -1

static int prefs_sync_choice = SYNC_UNSET, rts_sync_choice = 0;

static int sync_choice(void) 
{
  return(XEN_TO_C_INT_OR_ELSE(prefs_variable_get("global-sync-choice"), 
			      SYNC_DISABLED));
}

static void set_sync_choice(int val, const char *load)
{
  if ((load) &&
      (!XEN_DEFINED_P("global-sync-choice")))
    xen_load_file_with_path_and_extension(load);
  if (XEN_DEFINED_P("global-sync-choice")) 
    {
      prefs_variable_set("global-sync-choice", 
			 C_TO_XEN_INT(val));
      if ((load) && 
	  (val != SYNC_DISABLED))
	prefs_function_call_1("set-global-sync", C_TO_XEN_INT(val));
    }
}

static void revert_sync_choice(prefs_info *prf) {set_sync_choice(rts_sync_choice, NULL);}
static void clear_sync_choice(prefs_info *prf) {set_sync_choice(SYNC_DISABLED, NULL);}

static void help_sync_choice(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "Many operations can operate on all channels at once, or only on the currently selected \
channel.  If either of these buttons is selected, such operations operate either on all channels \
within each sound (but not across sounds), or on all current channels at once.  The default is \
to operate only on the selected channel (neither button selected).",
	   WITH_WORD_WRAP);
}

static void reflect_sync_choice(prefs_info *prf)
{
  prefs_sync_choice = sync_choice();
  SET_TOGGLE(prf->toggle, prefs_sync_choice == SYNC_WITHIN_EACH_SOUND);
  SET_TOGGLE(prf->toggle2, prefs_sync_choice == SYNC_ACROSS_ALL_SOUNDS);
}

static void save_sync_choice(prefs_info *prf, FILE *fd)
{
  if (prefs_sync_choice != SYNC_UNSET) 
    {
      rts_sync_choice = prefs_sync_choice;
      if (prefs_sync_choice != SYNC_DISABLED)
	prefs_variable_save(fd, "global-sync-choice", "extensions", C_TO_XEN_INT(prefs_sync_choice));
    }
}

static void sync1_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->toggle))
    prefs_sync_choice = SYNC_WITHIN_EACH_SOUND;
  else prefs_sync_choice = SYNC_DISABLED;
  SET_TOGGLE(prf->toggle2, false);
  /* if user has not loaded extensions, but sets one of the toggle buttons, load extensions and
   *    set the global-sync-choice variable
   */
  set_sync_choice(prefs_sync_choice, "extensions"); 
}

static void sync2_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->toggle2))
    prefs_sync_choice = SYNC_ACROSS_ALL_SOUNDS;
  else prefs_sync_choice = SYNC_DISABLED;
  SET_TOGGLE(prf->toggle, false);
  set_sync_choice(prefs_sync_choice, "extensions");
}



/* ---------------- mark-tag size ---------------- */

static int rts_mark_tag_width = DEFAULT_MARK_TAG_WIDTH, rts_mark_tag_height = DEFAULT_MARK_TAG_HEIGHT;

static void reflect_mark_tag_size(prefs_info *prf)
{
  int_to_textfield(prf->text, mark_tag_width(ss));
  int_to_textfield(prf->rtxt, mark_tag_height(ss));
}

static void revert_mark_tag_size(prefs_info *prf)
{
  set_mark_tag_width(rts_mark_tag_width);
  set_mark_tag_height(rts_mark_tag_height);
}

static void save_mark_tag_size(prefs_info *prf, FILE *ignore)
{
  rts_mark_tag_width = mark_tag_width(ss);
  rts_mark_tag_height = mark_tag_height(ss);
}

static TIMEOUT_TYPE mark_tag_width_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->text, mark_tag_width(ss));
  TIMEOUT_RESULT
}

static TIMEOUT_TYPE mark_tag_height_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->rtxt, mark_tag_height(ss));
  TIMEOUT_RESULT
}

static void mark_tag_width_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  SET_TEXT(prf->text, "must be > 0");
  TIMEOUT(mark_tag_width_erase_func);
}

static void mark_tag_height_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  SET_TEXT(prf->rtxt, "must be > 0");
  TIMEOUT(mark_tag_height_erase_func);
}

static void mark_tag_size_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int width = 0;
      redirect_errors_to(mark_tag_width_error, (void *)prf);
      width = string_to_int(str, 1, "mark tag width");
      redirect_errors_to(NULL, NULL);
      if (width > 0) set_mark_tag_width(width);
      FREE_TEXT(str);
      str = GET_TEXT(prf->rtxt);
      if ((str) && (*str))
	{
	  int height;
	  redirect_errors_to(mark_tag_height_error, (void *)prf);
	  height = string_to_int(str, 1, "mark tag height");
	  redirect_errors_to(NULL, NULL);
	  if (height > 0) set_mark_tag_height(height);
	  FREE_TEXT(str);
	}
    }
}


/* ---------------- mix-tag size ---------------- */

static int rts_mix_tag_width = DEFAULT_MIX_TAG_WIDTH, rts_mix_tag_height = DEFAULT_MIX_TAG_HEIGHT;

static void reflect_mix_tag_size(prefs_info *prf)
{
  int_to_textfield(prf->text, mix_tag_width(ss));
  int_to_textfield(prf->rtxt, mix_tag_height(ss));
}

static void revert_mix_tag_size(prefs_info *prf)
{
  set_mix_tag_width(rts_mix_tag_width);
  set_mix_tag_height(rts_mix_tag_height);
}

static void save_mix_tag_size(prefs_info *prf, FILE *ignore)
{
  rts_mix_tag_width = mix_tag_width(ss);
  rts_mix_tag_height = mix_tag_height(ss);
}

static TIMEOUT_TYPE mix_tag_width_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->text, mix_tag_width(ss));
  TIMEOUT_RESULT
}

static TIMEOUT_TYPE mix_tag_height_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->rtxt, mix_tag_height(ss));
  TIMEOUT_RESULT
}

static void mix_tag_width_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  SET_TEXT(prf->text, "must be > 0");
  TIMEOUT(mix_tag_width_erase_func);
}

static void mix_tag_height_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  SET_TEXT(prf->rtxt, "must be > 0");
  TIMEOUT(mix_tag_height_erase_func);
}

static void mix_tag_size_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int width = 0;
      redirect_errors_to(mix_tag_width_error, (void *)prf);
      width = string_to_int(str, 1, "mix tag width");
      redirect_errors_to(NULL, NULL);
      if (width > 0) set_mix_tag_width(width);
      FREE_TEXT(str);
      str = GET_TEXT(prf->rtxt);
      if ((str) && (*str))
	{
	  int height;
	  redirect_errors_to(mix_tag_height_error, (void *)prf);
	  height = string_to_int(str, 1, "mix tag height");
	  redirect_errors_to(NULL, NULL);
	  if (height > 0) set_mix_tag_height(height);
	  FREE_TEXT(str);
	}
    }
}


/* ---------------- start up size ---------------- */

static int rts_init_window_width = DEFAULT_INIT_WINDOW_WIDTH, rts_init_window_height = DEFAULT_INIT_WINDOW_HEIGHT;

static void revert_init_window_size(prefs_info *prf)
{
  ss->init_window_width = rts_init_window_width;
  ss->init_window_height = rts_init_window_height;
}

static void clear_init_window_size(prefs_info *prf)
{
  ss->init_window_width = DEFAULT_INIT_WINDOW_WIDTH;
  ss->init_window_height = DEFAULT_INIT_WINDOW_HEIGHT;
}

static void save_init_window_size(prefs_info *prf, FILE *ignore)
{
  rts_init_window_width = ss->init_window_width;
  rts_init_window_height = ss->init_window_height;
}

static TIMEOUT_TYPE startup_width_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->text, ss->init_window_width);
  TIMEOUT_RESULT
}

static TIMEOUT_TYPE startup_height_erase_func(TIMEOUT_ARGS)
{
  prefs_info *prf = (prefs_info *)context;
  int_to_textfield(prf->rtxt, ss->init_window_height);
  TIMEOUT_RESULT
}

static void startup_width_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  SET_TEXT(prf->text, "must be > 0");
  TIMEOUT(startup_width_erase_func);
}

static void startup_height_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  SET_TEXT(prf->rtxt, "must be > 0");
  TIMEOUT(startup_height_erase_func);
}

static void startup_size_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int width = 0;
      redirect_errors_to(startup_width_error, (void *)prf);
      width = string_to_int(str, 1, "startup width");
      redirect_errors_to(NULL, NULL);
      if (width > 0) ss->init_window_width = width;
      FREE_TEXT(str);
      str = GET_TEXT(prf->rtxt);
      if ((str) && (*str))
	{
	  int height;
	  redirect_errors_to(startup_height_error, (void *)prf);
	  height = string_to_int(str, 1, "startup height");
	  redirect_errors_to(NULL, NULL);
	  if (height > 0) ss->init_window_height = height;
	  FREE_TEXT(str);
	}
    }
}


/* ---------------- check-for-unsaved-edits ---------------- */

static bool rts_unsaved_edits = false, prefs_unsaved_edits = false;

static bool unsaved_edits(void)
{
  return(XEN_TO_C_BOOLEAN(prefs_variable_get("checking-for-unsaved-edits")));
}

static void set_unsaved_edits(bool val, const char *load)
{
  prefs_unsaved_edits = val;
  if ((load) &&
      (!XEN_DEFINED_P("checking-for-unsaved-edits")))
    xen_load_file_with_path_and_extension(load);
  if (XEN_DEFINED_P("checking-for-unsaved-edits")) 
    prefs_function_call_1("check-for-unsaved-edits", C_TO_XEN_BOOLEAN(val));
}

static void revert_unsaved_edits(prefs_info *prf) {set_unsaved_edits(rts_unsaved_edits, NULL);}
static void clear_unsaved_edits(prefs_info *prf) {set_unsaved_edits(false, NULL);}

static void save_unsaved_edits(prefs_info *prf, FILE *fd)
{
  rts_unsaved_edits = GET_TOGGLE(prf->toggle);
  if (rts_unsaved_edits)
    prefs_function_save_1(fd, "check-for-unsaved-edits", "extensions", C_TO_XEN_BOOLEAN(prefs_unsaved_edits));
}

static void reflect_unsaved_edits(prefs_info *prf) 
{
  prefs_unsaved_edits = unsaved_edits();
  SET_TOGGLE(prf->toggle, prefs_unsaved_edits);
}

static void help_unsaved_edits(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option looks for unsaved edits when you close a file, or exit Snd.  If it \
finds any, it asks you whether you want to save them.",
	   WITH_WORD_WRAP);
}

static void unsaved_edits_toggle(prefs_info *prf)
{
  set_unsaved_edits(GET_TOGGLE(prf->toggle), "extensions");
}



/* ---------------- current-window-display ---------------- */

static bool rts_current_window_display = false, prefs_current_window_display = false;

static bool current_window_display(void)
{
  return(XEN_TO_C_BOOLEAN(prefs_variable_get("current-window-display-is-running")));
}

static void set_current_window_display(bool val, const char *load)
{
  prefs_current_window_display = val;
  if ((load) && (val) &&
      (!XEN_DEFINED_P("current-window-display-is-running")))
    {
      xen_load_file_with_path_and_extension(load);
      prefs_function_call_0("make-current-window-display");
    }
  prefs_variable_set("current-window-display-is-running", C_TO_XEN_BOOLEAN(val));
}

static void revert_current_window_display(prefs_info *prf) {set_current_window_display(rts_current_window_display, NULL);}
static void clear_current_window_display(prefs_info *prf) {set_current_window_display(false, NULL);}

static void save_current_window_display(prefs_info *prf, FILE *fd)
{
  rts_current_window_display = GET_TOGGLE(prf->toggle);
  if (rts_current_window_display)
    prefs_function_save_0(fd, "make-current-window-display", "draw");
}

static void help_current_window(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option displays a small graph of the entire sound in the upper right corner \
of the screen with an indication of where the current window is. If you click somewhere in the \
little graph, the cursor and main window are moved to that spot.",
	   WITH_WORD_WRAP);
}

static void current_window_display_toggle(prefs_info *prf)
{
  set_current_window_display(GET_TOGGLE(prf->toggle), "draw");
}

static void reflect_current_window_display(prefs_info *prf) 
{
  prefs_current_window_display = current_window_display();
  SET_TOGGLE(prf->toggle, prefs_current_window_display);
}



/* ---------------- focus-follows-mouse ---------------- */

static bool rts_focus_follows_mouse = false, prefs_focus_follows_mouse = false;

static bool focus_follows_mouse(void)
{
  return(XEN_TO_C_BOOLEAN(prefs_variable_get("focus-is-following-mouse")));
}

static void set_focus_follows_mouse(bool val, const char *load)
{
  prefs_focus_follows_mouse = val;
  prefs_variable_set("focus-is-following-mouse", C_TO_XEN_BOOLEAN(val));
}

static void revert_focus_follows_mouse(prefs_info *prf) {set_focus_follows_mouse(rts_focus_follows_mouse, NULL);}
static void clear_focus_follows_mouse(prefs_info *prf) {set_focus_follows_mouse(false, NULL);}

static void save_focus_follows_mouse(prefs_info *prf, FILE *fd) 
{
  rts_focus_follows_mouse = GET_TOGGLE(prf->toggle);
  if (rts_focus_follows_mouse)
    prefs_function_save_0(fd, "focus-follows-mouse", "extensions");
}

static void help_focus_follows_mouse(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option implements 'pointer focus' in Snd; that is, the widget under the mouse \
is the active widget. In this mode, you don't need to click a graph or text widget before \
taking some action in it.  (This option only takes effect when you restart Snd).",
	   WITH_WORD_WRAP);
}

static void reflect_focus_follows_mouse(prefs_info *prf) 
{
  prefs_focus_follows_mouse = focus_follows_mouse();
  SET_TOGGLE(prf->toggle, prefs_focus_follows_mouse);
}

static void focus_follows_mouse_toggle(prefs_info *prf)
{
  set_focus_follows_mouse(GET_TOGGLE(prf->toggle), NULL);
}



#if WITH_RUN
/* ---------------- optimization ---------------- */

static int rts_optimization = DEFAULT_OPTIMIZATION;

#define MAX_OPTIMIZATION 6
#define MIN_OPTIMIZATION 0

static void revert_optimization(prefs_info *prf) {set_optimization(rts_optimization);}
static void save_optimization(prefs_info *prf, FILE *ignore) {rts_optimization = optimization(ss);}

static void reflect_optimization(prefs_info *prf)
{
  int_to_textfield(prf->text, optimization(ss));
  SET_SENSITIVE(prf->arrow_up, optimization(ss) < MAX_OPTIMIZATION);
  SET_SENSITIVE(prf->arrow_down, optimization(ss) > MIN_OPTIMIZATION);
}

static void optimization_up(prefs_info *prf)
{
  int val;
  val = optimization(ss) + 1;
  if (val >= MAX_OPTIMIZATION) SET_SENSITIVE(prf->arrow_up, false);
  if (val > MIN_OPTIMIZATION) SET_SENSITIVE(prf->arrow_down, true);
  set_optimization(val);
  int_to_textfield(prf->text, optimization(ss));
}

static void optimization_down(prefs_info *prf)
{
  int val;
  val = optimization(ss) - 1;
  if (val <= MIN_OPTIMIZATION) SET_SENSITIVE(prf->arrow_down, false);
  if (val < MAX_OPTIMIZATION) SET_SENSITIVE(prf->arrow_up, true);
  set_optimization(val);
  int_to_textfield(prf->text, optimization(ss));
}

static void optimization_from_text(prefs_info *prf)
{
  int opt;
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      prf->got_error = false;
      redirect_errors_to(redirect_post_prefs_error, (void *)prf);
      opt = string_to_int(str, MIN_OPTIMIZATION, "optimization"); 
      redirect_errors_to(NULL, NULL);
      FREE_TEXT(str);
      if (!(prf->got_error))
	{
	  if (opt <= MAX_OPTIMIZATION)
	    set_optimization(opt);		 
	  else va_post_prefs_error("%s > %d?", prf, str, MAX_OPTIMIZATION);
	}
      else prf->got_error = false;
    }
}
#endif


/* ---------------- cursor-size ---------------- */

static int rts_cursor_size = DEFAULT_CURSOR_SIZE;

#define MIN_CURSOR_SIZE 1
#define MAX_CURSOR_SIZE 500

static void revert_cursor_size(prefs_info *prf) {in_set_cursor_size(rts_cursor_size);}
static void save_cursor_size(prefs_info *prf, FILE *ignore) {rts_cursor_size = cursor_size(ss);}

static void reflect_cursor_size(prefs_info *prf)
{
  int_to_textfield(prf->text, cursor_size(ss));
  SET_SENSITIVE(prf->arrow_up, cursor_size(ss) < MAX_CURSOR_SIZE);
  SET_SENSITIVE(prf->arrow_down, cursor_size(ss) > MIN_CURSOR_SIZE);
}

static void cursor_size_up(prefs_info *prf)
{
  int size;
  size = cursor_size(ss) + 1;
  if (size >= MAX_CURSOR_SIZE) SET_SENSITIVE(prf->arrow_up, false);
  if (size > MIN_CURSOR_SIZE) SET_SENSITIVE(prf->arrow_down, true);
  in_set_cursor_size(size);
  int_to_textfield(prf->text, cursor_size(ss));
}

static void cursor_size_down(prefs_info *prf)
{
  int size;
  size = cursor_size(ss) - 1;
  if (size <= MIN_CURSOR_SIZE) SET_SENSITIVE(prf->arrow_down, false);
  if (size < MAX_CURSOR_SIZE) SET_SENSITIVE(prf->arrow_up, true);
  in_set_cursor_size(size);
  int_to_textfield(prf->text, cursor_size(ss));
}

static void cursor_size_from_text(prefs_info *prf)
{
  int size;
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      prf->got_error = false;
      redirect_errors_to(redirect_post_prefs_error, (void *)prf);
      size = string_to_int(str, 0, "cursor size"); 
      redirect_errors_to(NULL, NULL);
      FREE_TEXT(str);
      if (!(prf->got_error))
	{
	  if (size >= MIN_CURSOR_SIZE)
	    {
	      if (size <= MAX_CURSOR_SIZE)
		in_set_cursor_size(size);
	      else va_post_prefs_error("%s > %d?", prf, str, MAX_CURSOR_SIZE);
	    }
	  else va_post_prefs_error("%s < %d?", prf, str, MIN_CURSOR_SIZE);
	}
      else prf->got_error = false;
    }
  else post_prefs_error("no size?", prf);
}

/* ---------------- dot-size ---------------- */

static int rts_dot_size = DEFAULT_DOT_SIZE;

#define MIN_DOT_SIZE 0
#define MAX_DOT_SIZE 100

static void revert_dot_size(prefs_info *prf) {in_set_dot_size(rts_dot_size);}
static void save_dot_size(prefs_info *prf, FILE *ignore) {rts_dot_size = dot_size(ss);}

static void reflect_dot_size(prefs_info *prf)
{
  int_to_textfield(prf->text, dot_size(ss));
  SET_SENSITIVE(prf->arrow_up, dot_size(ss) < MAX_DOT_SIZE);
  SET_SENSITIVE(prf->arrow_down, dot_size(ss) > MIN_DOT_SIZE);
}

static void dot_size_up(prefs_info *prf)
{
  int size;
  size = dot_size(ss) + 1;
  if (size >= MAX_DOT_SIZE) SET_SENSITIVE(prf->arrow_up, false);
  if (size > MIN_DOT_SIZE) SET_SENSITIVE(prf->arrow_down, true);
  in_set_dot_size(size);
  int_to_textfield(prf->text, dot_size(ss));
}

static void dot_size_down(prefs_info *prf)
{
  int size;
  size = dot_size(ss) - 1;
  if (size <= MIN_DOT_SIZE) SET_SENSITIVE(prf->arrow_down, false);
  if (size < MAX_DOT_SIZE) SET_SENSITIVE(prf->arrow_up, true);
  in_set_dot_size(size);
  int_to_textfield(prf->text, dot_size(ss));
}

static void dot_size_from_text(prefs_info *prf)
{
  int size;
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      prf->got_error = false;
      redirect_errors_to(redirect_post_prefs_error, (void *)prf);
      size = string_to_int(str, 0, "dot size"); 
      redirect_errors_to(NULL, NULL);
      FREE_TEXT(str);
      if (!(prf->got_error))
	{
	  if (size >= MIN_DOT_SIZE)
	    {
	      if (size <= MAX_DOT_SIZE)
		in_set_dot_size(size);
	      else va_post_prefs_error("%s > %d?", prf, str, MAX_DOT_SIZE);
	    }
	  else va_post_prefs_error("%s < %d?", prf, str, MIN_DOT_SIZE);
	}
      else prf->got_error = false;
    }
  else post_prefs_error("no size?", prf);
}


/* ---------------- fft-size ---------------- */

static int rts_fft_size = DEFAULT_TRANSFORM_SIZE;

#define MAX_TRANSFORM_SIZE 1073741824
#define MIN_TRANSFORM_SIZE 2

static void revert_fft_size(prefs_info *prf) {in_set_transform_size(rts_fft_size);}
static void save_fft_size(prefs_info *prf, FILE *ignore) {rts_fft_size = transform_size(ss);}

static void reflect_fft_size(prefs_info *prf)
{
  int_to_textfield(prf->text, transform_size(ss));
  SET_SENSITIVE(prf->arrow_up, transform_size(ss) < MAX_TRANSFORM_SIZE);
  SET_SENSITIVE(prf->arrow_down, transform_size(ss) > MIN_TRANSFORM_SIZE);
}

static void fft_size_up(prefs_info *prf)
{
  int size;
  size = transform_size(ss) * 2;
  if (size >= MAX_TRANSFORM_SIZE) SET_SENSITIVE(prf->arrow_up, false);
  if (size > MIN_TRANSFORM_SIZE) SET_SENSITIVE(prf->arrow_down, true);
  in_set_transform_size(size);
  int_to_textfield(prf->text, transform_size(ss));
}

static void fft_size_down(prefs_info *prf)
{
  int size;
  size = transform_size(ss) / 2;
  if (size <= MIN_TRANSFORM_SIZE) SET_SENSITIVE(prf->arrow_down, false);
  if (size < MAX_TRANSFORM_SIZE) SET_SENSITIVE(prf->arrow_up, true);
  in_set_transform_size(size);
  int_to_textfield(prf->text, transform_size(ss));
}

static void fft_size_from_text(prefs_info *prf)
{
  int size;
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      prf->got_error = false;
      redirect_errors_to(redirect_post_prefs_error, (void *)prf);
      size = string_to_int(str, MIN_TRANSFORM_SIZE, "size"); 
      redirect_errors_to(NULL, NULL);
      FREE_TEXT(str);
      if (!(prf->got_error))
	{
	  if (POWER_OF_2_P(size))
	    {
	      if (size <= MAX_TRANSFORM_SIZE)
		in_set_transform_size(size);
	      else va_post_prefs_error("%s > %d?", prf, str, MAX_TRANSFORM_SIZE);
	    }
	  else post_prefs_error("size must be a power of 2", prf);
	}
      else prf->got_error = false;
    }
}


/* ---------------- with-tracking-cursor ---------------- */

static tracking_cursor_t rts_with_tracking_cursor = DEFAULT_WITH_TRACKING_CURSOR;
static Float rts_cursor_update_interval = DEFAULT_CURSOR_UPDATE_INTERVAL;
static int rts_cursor_location_offset = DEFAULT_CURSOR_LOCATION_OFFSET;

static void revert_with_tracking_cursor(prefs_info *prf)
{
  in_set_with_tracking_cursor(ss, rts_with_tracking_cursor);
  set_cursor_update_interval(rts_cursor_update_interval);
  set_cursor_location_offset(rts_cursor_location_offset);
}

static void save_with_tracking_cursor(prefs_info *prf, FILE *ignore)
{
  rts_with_tracking_cursor = with_tracking_cursor(ss);
  rts_cursor_update_interval = cursor_update_interval(ss);
  rts_cursor_location_offset = cursor_location_offset(ss);
}

static void reflect_with_tracking_cursor(prefs_info *prf) 
{
  SET_TOGGLE(prf->toggle, with_tracking_cursor(ss));
  int_to_textfield(prf->rtxt, cursor_location_offset(ss));
  float_to_textfield(prf->text, cursor_update_interval(ss));
}

static void with_tracking_cursor_toggle(prefs_info *prf)
{
  in_set_with_tracking_cursor(ss, (GET_TOGGLE(prf->toggle)) ? ALWAYS_TRACK : DONT_TRACK);
}

static void cursor_location_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      float interval = DEFAULT_CURSOR_UPDATE_INTERVAL;
      redirect_errors_to(any_error_to_text, (void *)prf);
      interval = (float)string_to_Float(str, 0.0, "cursor offset");
      redirect_errors_to(NULL, NULL);
      if (!(prf->got_error))
	set_cursor_update_interval(interval);
      FREE_TEXT(str);
      str = GET_TEXT(prf->rtxt);
      if ((str) && (*str))
	{
	  int loc = DEFAULT_CURSOR_LOCATION_OFFSET;
	  redirect_errors_to(any_error_to_text, (void *)prf);
	  loc = string_to_int(str, 0, "cursor offset");
	  redirect_errors_to(NULL, NULL);
	  if (!(prf->got_error))
	    set_cursor_location_offset(loc);
	  FREE_TEXT(str);
	}
    }
}


/* ---------------- channel-style ---------------- */

static channel_style_t rts_channel_style = DEFAULT_CHANNEL_STYLE;

static const char *channel_styles[NUM_CHANNEL_STYLES] = {"separate", "combined", "superimposed"};

static void reflect_channel_style(prefs_info *prf) {set_radio_button(prf, (int)channel_style(ss));}
static void revert_channel_style(prefs_info *prf) {in_set_channel_style(rts_channel_style);}
static void save_channel_style(prefs_info *prf, FILE *ignore) {rts_channel_style = channel_style(ss);}

static void channel_style_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    in_set_channel_style((channel_style_t)which_radio_button(prf));
}


/* ---------------- cursor-style ---------------- */

static cursor_style_t rts_cursor_style = DEFAULT_CURSOR_STYLE;

#define NUM_CURSOR_STYLES 2
static const char *cursor_styles[NUM_CURSOR_STYLES] = {"cross", "line"};

static void reflect_cursor_style(prefs_info *prf) {set_radio_button(prf, (int)cursor_style(ss));}
static void revert_cursor_style(prefs_info *prf) {in_set_cursor_style(rts_cursor_style);}
static void save_cursor_style(prefs_info *prf, FILE *ignore) {rts_cursor_style = cursor_style(ss);}

static void cursor_style_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    in_set_cursor_style((cursor_style_t)which_radio_button(prf));
}


/* ---------------- tracking-cursor-style ---------------- */

static cursor_style_t rts_tracking_cursor_style = DEFAULT_CURSOR_STYLE;

static void reflect_tracking_cursor_style(prefs_info *prf) {set_radio_button(prf, (int)tracking_cursor_style(ss));}
static void revert_tracking_cursor_style(prefs_info *prf) {in_set_tracking_cursor_style(rts_tracking_cursor_style);}
static void save_tracking_cursor_style(prefs_info *prf, FILE *ignore) {rts_tracking_cursor_style = tracking_cursor_style(ss);}

static void tracking_cursor_style_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    in_set_tracking_cursor_style((cursor_style_t)which_radio_button(prf));
}


/* ---------------- transform-graph-type ---------------- */

static graph_type_t rts_transform_graph_type = DEFAULT_TRANSFORM_GRAPH_TYPE;

#define NUM_TRANSFORM_GRAPH_TYPES 3
static const char *transform_graph_types[NUM_TRANSFORM_GRAPH_TYPES] = {"normal", "sonogram", "spectrogram"};

static void reflect_transform_graph_type(prefs_info *prf) {set_radio_button(prf, (int)transform_graph_type(ss));}
static void revert_transform_graph_type(prefs_info *prf) {in_set_transform_graph_type(rts_transform_graph_type);}
static void save_transform_graph_type(prefs_info *prf, FILE *ignore) {rts_transform_graph_type = transform_graph_type(ss);}

static void transform_graph_type_choice(prefs_info *prf) 
{
  if (GET_TOGGLE(prf->radio_button))
    in_set_transform_graph_type((graph_type_t)which_radio_button(prf));
}


/* ---------------- transform-normalization ---------------- */

static fft_normalize_t rts_transform_normalization = DEFAULT_TRANSFORM_NORMALIZATION;

static const char *transform_normalizations[NUM_TRANSFORM_NORMALIZATIONS] = {"none", "by channel", "by sound", "global"};

static void reflect_transform_normalization(prefs_info *prf) {set_radio_button(prf, (int)transform_normalization(ss));}
static void revert_transform_normalization(prefs_info *prf) {in_set_transform_normalization(rts_transform_normalization);}
static void save_transform_normalization(prefs_info *prf, FILE *ignore) {rts_transform_normalization = transform_normalization(ss);}

static void transform_normalization_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    in_set_transform_normalization((fft_normalize_t)which_radio_button(prf));
}


/* ---------------- graph-style ---------------- */

static graph_style_t rts_graph_style = DEFAULT_GRAPH_STYLE;

static const char *graph_styles[NUM_GRAPH_STYLES] = {"line", "dot", "filled", "dot+line", "lollipop"};

static void reflect_graph_style(prefs_info *prf) {set_radio_button(prf, (int)graph_style(ss));}
static void revert_graph_style(prefs_info *prf) {in_set_graph_style(rts_graph_style);}
static void save_graph_style(prefs_info *prf, FILE *ignore) {rts_graph_style = graph_style(ss);}

static void graph_style_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    in_set_graph_style((graph_style_t)which_radio_button(prf));
}


/* ---------------- speed control ---------------- */

static speed_style_t rts_speed_control_style = DEFAULT_SPEED_CONTROL_STYLE;
static int rts_speed_control_tones = DEFAULT_SPEED_CONTROL_TONES;

#define MIN_SPEED_CONTROL_SEMITONES 1
static const char *speed_control_styles[NUM_SPEED_CONTROL_STYLES] = {"float", "ratio", "semitones:"};

static void show_speed_control_semitones(prefs_info *prf)
{
  int_to_textfield(prf->text, speed_control_tones(ss));
  SET_SENSITIVE(prf->arrow_down, (speed_control_tones(ss) > MIN_SPEED_CONTROL_SEMITONES));
}

static void speed_control_up(prefs_info *prf)
{
  in_set_speed_control_tones(ss, speed_control_tones(ss) + 1);
  show_speed_control_semitones(prf);
}

static void speed_control_down(prefs_info *prf)
{
  in_set_speed_control_tones(ss, speed_control_tones(ss) - 1);
  show_speed_control_semitones(prf);
}

static void speed_control_text(prefs_info *prf)
{
  int tones;
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      prf->got_error = false;
      redirect_errors_to(redirect_post_prefs_error, (void *)prf);
      tones = string_to_int(str, MIN_SPEED_CONTROL_SEMITONES, "semitones");
      redirect_errors_to(NULL, NULL);
      FREE_TEXT(str);
      if (!(prf->got_error))
	{
	  in_set_speed_control_tones(ss, tones);
	  SET_SENSITIVE(prf->arrow_down, (speed_control_tones(ss) > MIN_SPEED_CONTROL_SEMITONES));
	}
      else prf->got_error = false;
    }
}

static void reflect_speed_control(prefs_info *prf)
{
  set_radio_button(prf, (int)speed_control_style(ss));
  show_speed_control_semitones(prf);
}

static void speed_control_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    in_set_speed_control_style(ss, (speed_style_t)which_radio_button(prf));
}

static void revert_speed_control(prefs_info *prf) 
{
  in_set_speed_control_style(ss, rts_speed_control_style);
  in_set_speed_control_tones(ss, rts_speed_control_tones);
}

static void save_speed_control(prefs_info *prf, FILE *ignore) 
{
  rts_speed_control_style = speed_control_style(ss);
  rts_speed_control_tones = speed_control_tones(ss);
}


/* ---------------- default-output-chans etc ---------------- */

static int rts_default_output_chans = DEFAULT_OUTPUT_CHANS;
static int rts_default_output_srate = DEFAULT_OUTPUT_SRATE;
static int rts_default_output_data_format = DEFAULT_OUTPUT_DATA_FORMAT;
static int rts_default_output_header_type = DEFAULT_OUTPUT_HEADER_TYPE;

static prefs_info *output_data_format_prf = NULL, *output_header_type_prf = NULL;

#define NUM_OUTPUT_CHAN_CHOICES 4
static const char *output_chan_choices[NUM_OUTPUT_CHAN_CHOICES] = {"1", "2", "4", "8"};
static int output_chans[NUM_OUTPUT_CHAN_CHOICES] = {1, 2, 4, 8};

#define NUM_OUTPUT_SRATE_CHOICES 4
static const char *output_srate_choices[NUM_OUTPUT_SRATE_CHOICES] = {"8000", "22050", "44100", "48000"};
static int output_srates[NUM_OUTPUT_SRATE_CHOICES] = {8000, 22050, 44100, 48000};

#define NUM_OUTPUT_TYPE_CHOICES 6
static const char *output_type_choices[NUM_OUTPUT_TYPE_CHOICES] = {"aifc", "wave", "next/sun", "rf64", "nist", "aiff"};
static int output_types[NUM_OUTPUT_TYPE_CHOICES] = {MUS_AIFC, MUS_RIFF, MUS_NEXT, MUS_RF64, MUS_NIST, MUS_AIFF};

#define NUM_OUTPUT_FORMAT_CHOICES 4
static const char *output_format_choices[NUM_OUTPUT_FORMAT_CHOICES] = {"short", "int", "float", "double"};
static int output_formats[NUM_OUTPUT_FORMAT_CHOICES] = {MUS_LSHORT, MUS_LINT, MUS_LFLOAT, MUS_LDOUBLE};

static int header_to_data(int ht, int frm)
{
  /* nist -> short or int (lb)
     aiff -> short or int (b)
     aifc -> any (b)
     next -> any (b)
     wave -> any (l)
  */
  switch (ht)
    {
    case MUS_NEXT: case MUS_AIFC:
      switch (frm)
	{
	case MUS_LSHORT: return(MUS_BSHORT); break;
	case MUS_LINT: return(MUS_BINT); break;
	case MUS_LFLOAT: return(MUS_BFLOAT); break;
	case MUS_LDOUBLE: return(MUS_BDOUBLE); break;
	}
      break;
    case MUS_AIFF:
      switch (frm)
	{
	case MUS_LSHORT: return(MUS_BSHORT); break;
	case MUS_LINT: return(MUS_BINT); break;
	case MUS_LFLOAT: case MUS_LDOUBLE: case MUS_BFLOAT: case MUS_BDOUBLE: return(MUS_BINT); break;
	}
      break;
    case MUS_NIST:
      switch (frm)
	{
	case MUS_LFLOAT: case MUS_LDOUBLE: return(MUS_LINT); break;
	case MUS_BFLOAT: case MUS_BDOUBLE: return(MUS_BINT); break;
	}
      break;
    case MUS_RF64:
    case MUS_RIFF:
      switch (frm)
	{
	case MUS_BSHORT: return(MUS_LSHORT); break;
	case MUS_BINT: return(MUS_LINT); break;
	case MUS_BFLOAT: return(MUS_LFLOAT); break;
	case MUS_BDOUBLE: return(MUS_LDOUBLE); break;
	}
      break;
    }
  return(frm);
}

static int chans_to_button(int chans)
{
  int i;
  for (i = 0; i < NUM_OUTPUT_CHAN_CHOICES; i++)
    if (chans == output_chans[i])
      return(i);
  return(0);
}

static void reflect_default_output_chans(prefs_info *prf) {set_radio_button(prf, chans_to_button(default_output_chans(ss)));}
static void revert_default_output_chans(prefs_info *prf) {set_default_output_chans(rts_default_output_chans);}
static void save_default_output_chans(prefs_info *prf, FILE *ignore) {rts_default_output_chans = default_output_chans(ss);}

static void default_output_chans_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    set_default_output_chans(output_chans[which_radio_button(prf)]);
}

static int srate_to_button(int srate)
{
  int i;
  for (i = 0; i < NUM_OUTPUT_SRATE_CHOICES; i++)
    if (output_srates[i] == srate)
      return(i);
  return(0);
}

static void reflect_default_output_srate(prefs_info *prf) {set_radio_button(prf, srate_to_button(default_output_srate(ss)));}
static void revert_default_output_srate(prefs_info *prf) {set_default_output_srate(rts_default_output_srate);}
static void save_default_output_srate(prefs_info *prf, FILE *ignore) {rts_default_output_srate = default_output_srate(ss);}

static void default_output_srate_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    set_default_output_srate(output_srates[which_radio_button(prf)]);
}

static void reflect_default_output_header_type(prefs_info *prf)
{
  int which = -1;
  switch (default_output_header_type(ss))
    {
    case MUS_AIFC: which = 0; break;
    case MUS_AIFF: which = 5; break;
    case MUS_RIFF: which = 1; break;
    case MUS_RF64: which = 3; break;
    case MUS_NEXT: which = 2; break;
    case MUS_NIST: which = 4; break;
    }
  set_radio_button(prf, which);
}

static void revert_default_output_header_type(prefs_info *prf) {set_default_output_header_type(rts_default_output_header_type);}
static void save_default_output_header_type(prefs_info *prf, FILE *ignore) {rts_default_output_header_type = default_output_header_type(ss);}

static void reflect_default_output_data_format(prefs_info *prf)
{
  int which = -1;
  switch (default_output_data_format(ss))
    {
    case MUS_LINT: case MUS_BINT: which = 1; break;
    case MUS_LSHORT: case MUS_BSHORT: which = 0; break;
    case MUS_LFLOAT: case MUS_BFLOAT: which = 2; break;
    case MUS_LDOUBLE: case MUS_BDOUBLE: which = 3; break;
    }
  set_radio_button(prf, which);
}

static void default_output_header_type_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    {
      set_default_output_header_type(output_types[which_radio_button(prf)]);
      set_default_output_data_format(header_to_data(default_output_header_type(ss), default_output_data_format(ss)));
      reflect_default_output_data_format(output_data_format_prf);
    }
}

static void revert_default_output_data_format(prefs_info *prf) {set_default_output_data_format(rts_default_output_data_format);}
static void save_default_output_data_format(prefs_info *prf, FILE *ignore) {rts_default_output_data_format = default_output_data_format(ss);}

static void default_output_data_format_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    {
      int which = -1;
      which = which_radio_button(prf);
      set_default_output_data_format(output_formats[which]);

      switch (default_output_data_format(ss))
	{
	case MUS_LSHORT:
	  switch (default_output_header_type(ss))
	    {
	    case MUS_AIFC: case MUS_AIFF: case MUS_NEXT: 
	      set_default_output_data_format(MUS_BSHORT); 
	      break;
	    }
	  break;
	  
	case MUS_LINT:
	  switch (default_output_header_type(ss))
	    {
	    case MUS_AIFC: case MUS_AIFF: case MUS_NEXT: 
	      set_default_output_data_format(MUS_BINT); 
	      break;
	    }
	  break;
	case MUS_LFLOAT:
	  switch (default_output_header_type(ss))
	    {
	    case MUS_AIFC: case MUS_NEXT: 
	      set_default_output_data_format(MUS_BFLOAT); 
	      break;
	    case MUS_AIFF:
	      set_default_output_header_type(MUS_AIFC);
	      set_default_output_data_format(MUS_BFLOAT); 
	      break;
	    case MUS_NIST: 
	      set_default_output_header_type(MUS_RIFF); 
	      break;
	    }
	  break;
	case MUS_LDOUBLE:
	  switch (default_output_header_type(ss))
	    {
	    case MUS_AIFC: case MUS_NEXT: 
	      set_default_output_data_format(MUS_BDOUBLE); 
	      break;
	    case MUS_AIFF:
	      set_default_output_header_type(MUS_AIFC);
	      set_default_output_data_format(MUS_BDOUBLE); 
	      break;
	    case MUS_NIST: 
	      set_default_output_header_type(MUS_RIFF); 
	      break;
	    }
	  break;
	}
      reflect_default_output_header_type(output_header_type_prf);
    }
}


/* ---------------- raw sound defaults ---------------- */

static int rts_raw_chans = DEFAULT_OUTPUT_CHANS;
static int rts_raw_srate = DEFAULT_OUTPUT_SRATE;
static int rts_raw_data_format = DEFAULT_OUTPUT_DATA_FORMAT;

static void revert_raw_chans(prefs_info *prf)
{
  int srate = 0, chans = 0, format = 0;
  mus_header_raw_defaults(&srate, &chans, &format);
  chans = rts_raw_chans;
  mus_header_set_raw_defaults(srate, chans, format);
}

static void save_raw_chans(prefs_info *prf, FILE *ignore)
{
  int srate = 0, chans = 0, format = 0;
  mus_header_raw_defaults(&srate, &chans, &format);
  rts_raw_chans = chans;
}

static void reflect_raw_chans(prefs_info *prf)
{
  int srate = 0, chans = 0, format = 0;
  mus_header_raw_defaults(&srate, &chans, &format);
  int_to_textfield(prf->text, chans);
}

static void raw_chans_choice(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if (str)
    {
      int srate = 0, chans = 0, format = 0;
      mus_header_raw_defaults(&srate, &chans, &format);
      redirect_errors_to(any_error_to_text, (void *)prf);
      chans = string_to_int(str, 1, "raw chans");
      redirect_errors_to(NULL, NULL);
      if (!(prf->got_error))
	mus_header_set_raw_defaults(srate, chans, format);
      FREE_TEXT(str);
    }
}

static void revert_raw_srate(prefs_info *prf)
{
  int srate = 0, chans = 0, format = 0;
  mus_header_raw_defaults(&srate, &chans, &format);
  srate = rts_raw_srate;
  mus_header_set_raw_defaults(srate, chans, format);
}

static void save_raw_srate(prefs_info *prf, FILE *ignore)
{
  int srate = 0, chans = 0, format = 0;
  mus_header_raw_defaults(&srate, &chans, &format);
  rts_raw_srate = srate;
}

static void reflect_raw_srate(prefs_info *prf)
{
  int srate = 0, chans = 0, format = 0;
  mus_header_raw_defaults(&srate, &chans, &format);
  int_to_textfield(prf->text, srate);
}

static void raw_srate_choice(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if (str)
    {
      int srate = 0, chans = 0, format = 0;
      mus_header_raw_defaults(&srate, &chans, &format);
      redirect_errors_to(any_error_to_text, (void *)prf);
      srate = string_to_int(str, 1, "raw srate");
      redirect_errors_to(NULL, NULL);
      if (!(prf->got_error))
	mus_header_set_raw_defaults(srate, chans, format);
      FREE_TEXT(str);
    }
}

static char *raw_data_format_to_string(int format)
{
  /* the "mus-" prefix carries no information in this context, so strip it off */
  char *name;
  name = mus_data_format_to_string(format);
  if (name)
    {
      char *rtn;
      int i, j, len;
      len = strlen(name);
      rtn = (char *)CALLOC(len, sizeof(char));
      for (i = 0, j = 4; j < len; i++, j++)
	{
	  if (name[j] == '-')
	    {
	      rtn[i] = 'u';
	      return(rtn);
	    }
	  else rtn[i] = name[j];
	}
      return(rtn);
    }
  return(copy_string("unknown"));
}

static void revert_raw_data_format(prefs_info *prf)
{
  int srate = 0, chans = 0, format = 0;
  mus_header_raw_defaults(&srate, &chans, &format);
  format = rts_raw_data_format;
  mus_header_set_raw_defaults(srate, chans, format);
}

static void save_raw_data_format(prefs_info *prf, FILE *ignore)
{
  int srate = 0, chans = 0, format = 0;
  mus_header_raw_defaults(&srate, &chans, &format);
  rts_raw_data_format = format;
}

static void reflect_raw_data_format(prefs_info *prf)
{
  int srate = 0, chans = 0, format = 0;
  char *str;
  mus_header_raw_defaults(&srate, &chans, &format);
  str = raw_data_format_to_string(format);
  SET_TEXT(prf->text, str);
  FREE(str);
}

static char **raw_data_format_choices = NULL;

static void raw_data_format_from_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if (str)
    {
      int i, srate = 0, chans = 0, format = 0;
      mus_header_raw_defaults(&srate, &chans, &format);
      for (i = 0; i < MUS_NUM_DATA_FORMATS - 1; i++)
	if (STRCMP(raw_data_format_choices[i], str) == 0)
	  {
	    mus_header_set_raw_defaults(srate, chans, i + 1); /* skipping MUS_UNKNOWN = 0 */
	    reflect_raw_data_format(prf);
	    FREE_TEXT(str);
	    return;
	  }
    }
}

#if USE_MOTIF
static void raw_data_format_from_menu(prefs_info *prf, char *value)
{
  int i, srate = 0, chans = 0, format = 0;
  mus_header_raw_defaults(&srate, &chans, &format);
  for (i = 0; i < MUS_NUM_DATA_FORMATS - 1; i++)
    if (STRCMP(raw_data_format_choices[i], value) == 0)
      {
	mus_header_set_raw_defaults(srate, chans, i + 1);
	SET_TEXT(prf->text, raw_data_format_choices[i]);
	return;
      }
}
#endif


/* ---------------- recorder-out-chans etc ---------------- */

static int rts_recorder_output_chans = 1;
static int rts_recorder_srate = 44100;
static int rts_recorder_output_header_type = MUS_RIFF;
static int rts_recorder_output_data_format = MUS_AUDIO_COMPATIBLE_FORMAT;

static prefs_info *recorder_output_data_format_prf = NULL, *recorder_output_header_type_prf = NULL;

#define NUM_RECORDER_OUT_CHANS_CHOICES 4
static const char *recorder_out_chans_choices[NUM_RECORDER_OUT_CHANS_CHOICES] = {"1", "2", "4", "8"};
static int recorder_chans[NUM_RECORDER_OUT_CHANS_CHOICES] = {1, 2, 4, 8};

#define NUM_RECORDER_SRATE_CHOICES 5
static const char *recorder_srate_choices[NUM_RECORDER_SRATE_CHOICES] = {"8000", "22050", "44100", "48000", "96000"};
static int recorder_srates[NUM_RECORDER_SRATE_CHOICES] = {8000, 22050, 44100, 48000, 96000};

#define NUM_RECORDER_OUT_TYPE_CHOICES 6
static const char *recorder_out_type_choices[NUM_RECORDER_OUT_TYPE_CHOICES] = {"aifc", "wave", "next/sun", "rf64", "nist", "aiff"};
static int recorder_types[NUM_RECORDER_OUT_TYPE_CHOICES] = {MUS_AIFC, MUS_RIFF, MUS_NEXT, MUS_RF64, MUS_NIST, MUS_AIFF};

#define NUM_RECORDER_OUT_FORMAT_CHOICES 4
static const char *recorder_out_format_choices[NUM_RECORDER_OUT_FORMAT_CHOICES] = {"short", "int", "float", "double"};
static int recorder_formats[NUM_RECORDER_OUT_FORMAT_CHOICES] = {MUS_LSHORT, MUS_LINT, MUS_LFLOAT, MUS_LDOUBLE};

static void reflect_recorder_output_chans(prefs_info *prf)
{
  int which = -1;
  switch (rec_output_chans())
    {
    case 1: which = 0; break;
    case 2: which = 1; break;
    case 4: which = 2; break;
    case 8: which = 3; break;
    }
  if (which != -1)
    set_radio_button(prf, which);
}

static void reflect_recorder_srate(prefs_info *prf)
{
  int which = -1, sr;
  sr = rec_srate();
  if (sr == 8000) which = 0; else
  if (sr == 22050) which = 1; else
  if (sr == 44100) which = 2; else
  if (sr == 48000) which = 3; else
  if (sr == 96000) which = 4;
  if (which != -1)
    set_radio_button(prf, which);
}

static void revert_recorder_output_chans(prefs_info *prf) {rec_set_output_chans(rts_recorder_output_chans);}
static void save_recorder_output_chans(prefs_info *prf, FILE *ignore) {rts_recorder_output_chans = rec_output_chans();}

static void revert_recorder_srate(prefs_info *prf) {rec_set_srate(rts_recorder_srate);}
static void save_recorder_srate(prefs_info *prf, FILE *ignore) {rts_recorder_srate = rec_srate();}

static void recorder_output_chans_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    rec_set_output_chans(recorder_chans[which_radio_button(prf)]);
}

static void recorder_srate_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    rec_set_srate(recorder_srates[which_radio_button(prf)]);
}

static void reflect_recorder_output_header_type(prefs_info *prf)
{
  int which = -1;
  switch (rec_output_header_type())
    {
    case MUS_AIFC: which = 0; break;
    case MUS_AIFF: which = 5; break;
    case MUS_RIFF: which = 1; break;
    case MUS_RF64: which = 3; break;
    case MUS_NEXT: which = 2; break;
    case MUS_NIST: which = 4; break;
    }
  if (which != -1)
    set_radio_button(prf, which);
}

static void reflect_recorder_output_data_format(prefs_info *prf)
{
  int which = -1;
  switch (rec_output_data_format())
    {
    case MUS_LINT: case MUS_BINT: which = 1; break;
    case MUS_LSHORT: case MUS_BSHORT: which = 0; break;
    case MUS_LFLOAT: case MUS_BFLOAT: which = 2; break;
    case MUS_LDOUBLE: case MUS_BDOUBLE: which = 3; break;
    }
  if (which != -1)
    set_radio_button(prf, which);
}

static void revert_recorder_output_header_type(prefs_info *prf) {rec_set_output_header_type(rts_recorder_output_header_type);}
static void save_recorder_output_header_type(prefs_info *prf, FILE *ignore) {rts_recorder_output_header_type = rec_output_header_type();}

static void revert_recorder_output_data_format(prefs_info *prf) {rec_set_output_data_format(rts_recorder_output_data_format);}
static void save_recorder_output_data_format(prefs_info *prf, FILE *ignore) {rts_recorder_output_data_format = rec_output_data_format();}

static void recorder_output_header_type_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    {
      rec_set_output_header_type(recorder_types[which_radio_button(prf)]);
      rec_set_output_data_format(header_to_data(rec_output_header_type(), rec_output_data_format()));
      reflect_recorder_output_data_format(recorder_output_data_format_prf);
    }
}

static void recorder_output_data_format_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->radio_button))
    {
      rec_set_output_data_format(recorder_formats[which_radio_button(prf)]);
      switch (rec_output_data_format())
	{
	case MUS_LSHORT:
	  switch (rec_output_header_type())
	    {
	    case MUS_AIFC: case MUS_AIFF: case MUS_NEXT: 
	      rec_set_output_data_format(MUS_BSHORT); 
	      break;
	    }
	  break;
	  
	case MUS_LINT:
	  switch (rec_output_header_type())
	    {
	    case MUS_AIFC: case MUS_AIFF: case MUS_NEXT: 
	      rec_set_output_data_format(MUS_BINT); 
	      break;
	    }
	  break;
	case MUS_LFLOAT:
	  switch (rec_output_header_type())
	    {
	    case MUS_AIFC: case MUS_NEXT: 
	      rec_set_output_data_format(MUS_BFLOAT); 
	      break;
	    case MUS_AIFF:
	      rec_set_output_header_type(MUS_AIFC);
	      rec_set_output_data_format(MUS_BFLOAT); 
	      break;
	    case MUS_NIST: 
	      rec_set_output_header_type(MUS_RIFF); 
	      break;
	    }
	  break;
	case MUS_LDOUBLE:
	  switch (rec_output_header_type())
	    {
	    case MUS_AIFC: case MUS_NEXT: 
	      rec_set_output_data_format(MUS_BDOUBLE); 
	      break;
	    case MUS_AIFF:
	      rec_set_output_header_type(MUS_AIFC);
	      rec_set_output_data_format(MUS_BDOUBLE); 
	      break;
	    case MUS_NIST: 
	      rec_set_output_header_type(MUS_RIFF); 
	      break;
	    }
	  break;
	}
      reflect_recorder_output_header_type(recorder_output_header_type_prf);
    }
}


/* ---------------- with-sound ---------------- */

static bool rts_with_sound = false;
static char *rts_clm_file_name = NULL;
static int rts_clm_file_buffer_size = 65536;
static int rts_clm_table_size = 512;

static bool with_sound_is_loaded(void) {return(XEN_DEFINED_P("with-sound"));}
static void reflect_with_sound(prefs_info *prf) {}
static void revert_with_sound(prefs_info *prf) {SET_TOGGLE(prf->toggle, rts_with_sound);}
static void clear_with_sound(prefs_info *prf) {SET_TOGGLE(prf->toggle, false);}

static void with_sound_toggle(prefs_info *prf)
{
  if ((GET_TOGGLE(prf->toggle)) &&
      (!(XEN_DEFINED_P("with-sound"))))
    xen_load_file_with_path_and_extension("ws");
}

static void save_with_sound(prefs_info *prf, FILE *fd)
{
  rts_with_sound = GET_TOGGLE(prf->toggle);
  if (rts_with_sound)
    {
#if HAVE_SCHEME
      fprintf(fd, "(if (not (provided? 'snd-ws.scm)) (load-from-path \"ws.scm\"))\n");
      if (rts_clm_file_name)
	fprintf(fd, "(set! *clm-file-name* \"%s\")\n", rts_clm_file_name);
      if (rts_clm_file_buffer_size != 65536)
	fprintf(fd, "(set! *clm-file-buffer-size* %d)\n", rts_clm_file_buffer_size);
      if (rts_clm_table_size != 512)
	fprintf(fd, "(set! *clm-table-size* %d)\n", rts_clm_table_size);
#endif
#if HAVE_RUBY
      fprintf(fd, "require \"ws\"\n");
      if (rts_clm_file_name)
	fprintf(fd, "$clm_file_name = \"%s\"\n", rts_clm_file_name);
      if (rts_clm_file_buffer_size != 65536)
	fprintf(fd, "$clm_file_buffer_size = %d\n", rts_clm_file_buffer_size);
      if (rts_clm_table_size != 512)
	fprintf(fd, "$clm_table_size = %d\n", rts_clm_table_size);
#endif
#if HAVE_FORTH
      fprintf(fd, "require clm\n");
      if (rts_clm_file_name)
	fprintf(fd, "\"%s\" to *clm-file-name*\n", rts_clm_file_name);
      if (rts_clm_file_buffer_size != 65536)
	fprintf(fd, "%d to *clm-file-buffer-size*\n", rts_clm_file_buffer_size);
      if (rts_clm_table_size != 512)
	fprintf(fd, "%d to *clm-table-size*\n", rts_clm_table_size);
#endif
    }
}

static void help_with_sound(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "with-sound is the main CLM sound-producing macro.  If you want to use CLM functions to create \
new sounds, then edit them in Snd, include with-sound.",
	   WITH_WORD_WRAP);
}



/* ---------------- clm file name ---------------- */

#define CLM_FILE_NAME "*clm-file-name*"

static void set_clm_file_name(const char *str) {prefs_variable_set(CLM_FILE_NAME, C_TO_XEN_STRING(str));}

static char *find_clm_file_name(void)
{
  XEN val;
  val = prefs_variable_get(CLM_FILE_NAME);
  if (XEN_STRING_P(val))
    return(XEN_TO_C_STRING(val));
  return(NULL);
}

static void clm_file_name_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      rts_with_sound = true;
      if (rts_clm_file_name) FREE(rts_clm_file_name); /* save is done after we're sure with-sound is loaded */
      rts_clm_file_name = copy_string(str);
      set_clm_file_name(str);
      FREE_TEXT(str);
    }
}

static void reflect_clm_file_name(prefs_info *prf) {SET_TEXT(prf->text, find_clm_file_name());}
static void clear_clm_file_name(prefs_info *prf) {prefs_variable_set(CLM_FILE_NAME, C_TO_XEN_STRING("test.snd"));}

static void revert_clm_file_name(prefs_info *prf) 
{
  prefs_variable_set(CLM_FILE_NAME, C_TO_XEN_STRING((char *)((rts_clm_file_name) ? rts_clm_file_name : "test.snd")));
}

static void help_clm_file_name(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option sets the default output file name used by with-sound.",
	   WITH_WORD_WRAP);
}

static void save_clm_file_name(prefs_info *prf, FILE *ignore)
{
  if (rts_clm_file_name) FREE(rts_clm_file_name);
  rts_clm_file_name = copy_string(find_clm_file_name());
}


/* ---------------- clm sizes ---------------- */

#define CLM_TABLE_SIZE "*clm-table-size*"
#define CLM_FILE_BUFFER_SIZE "*clm-file-buffer-size*"

static int find_clm_table_size(void)
{
  return(XEN_TO_C_INT_OR_ELSE(prefs_variable_get(CLM_TABLE_SIZE), 512));
}

static int find_clm_file_buffer_size(void)
{
  return(XEN_TO_C_INT_OR_ELSE(prefs_variable_get(CLM_FILE_BUFFER_SIZE), 65536));
}

static void reflect_clm_sizes(prefs_info *prf)
{
  rts_clm_table_size = find_clm_table_size();
  int_to_textfield(prf->text, rts_clm_table_size);
  rts_clm_file_buffer_size = find_clm_file_buffer_size();
  int_to_textfield(prf->rtxt, rts_clm_file_buffer_size);
}

static void clm_sizes_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int size = 0;
      rts_with_sound = true;
      redirect_errors_to(any_error_to_text, (void *)prf);
      size = string_to_int(str, 1, "table size");
      redirect_errors_to(NULL, NULL);
      if (!(prf->got_error))
	rts_clm_table_size = size;
      FREE_TEXT(str);
    }
  str = GET_TEXT(prf->rtxt);
  if ((str) && (*str))
    {
      int size = 0;
      rts_with_sound = true;
      redirect_errors_to(any_error_to_text, (void *)prf);
      size = string_to_int(str, 1, "file buffer size");
      redirect_errors_to(NULL, NULL);
      if (!(prf->got_error))
	rts_clm_file_buffer_size = size;
      FREE_TEXT(str);
    }
}

static void help_clm_sizes(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option sets the default clm table size and file buffer size.",
	   WITH_WORD_WRAP);
}

static void revert_clm_sizes(prefs_info *prf)
{
  prefs_variable_set(CLM_FILE_BUFFER_SIZE, C_TO_XEN_INT(rts_clm_file_buffer_size));
  prefs_variable_set(CLM_TABLE_SIZE, C_TO_XEN_INT(rts_clm_table_size));
}

static void clear_clm_sizes(prefs_info *prf)
{
  prefs_variable_set(CLM_FILE_BUFFER_SIZE, C_TO_XEN_INT(65536));
  prefs_variable_set(CLM_TABLE_SIZE, C_TO_XEN_INT(512));
}

static void save_clm_sizes(prefs_info *prf, FILE *ignore)
{
  rts_clm_file_buffer_size = find_clm_file_buffer_size();
  rts_clm_table_size = find_clm_table_size();
}


/* ---------------- context sensitive popup ---------------- */

static bool include_context_sensitive_popup = false;

static void help_context_sensitive_popup(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option creates a context-sensitive popup menu (activated by button 3).  The menu \
displayed depends on where the mouse is at the time; there are special menus for the waveform and fft \
portions of the graphs, the listener, the edit history pane, and the current selection.",
	   WITH_WORD_WRAP);
}

static bool find_context_sensitive_popup(void) {return(XEN_DEFINED_P("edhist-help-edits"));}

static void save_context_sensitive_popup(prefs_info *prf, FILE *fd)
{
  include_context_sensitive_popup = GET_TOGGLE(prf->toggle);
  if (include_context_sensitive_popup)
    {
#if HAVE_SCHEME
      fprintf(fd, "(if (provided? 'snd-motif)\n    (if (not (provided? 'snd-popup.scm))\n        (load-from-path \"popup.scm\"))\n    (if (not (provided? 'snd-gtk-popup.scm))\n	(load-from-path \"gtk-popup.scm\")))\n");
#endif
#if HAVE_RUBY
      fprintf(fd, "require \"popup\"\n");
#endif
#if HAVE_FORTH
      fprintf(fd, "require popup\n");
#endif
    }
}

static void context_sensitive_popup_toggle(prefs_info *prf)
{
  if ((GET_TOGGLE(prf->toggle)) &&
      (!(find_context_sensitive_popup())))
#if USE_MOTIF
    xen_load_file_with_path_and_extension("popup");
#else
    xen_load_file_with_path_and_extension("gtk-popup");
#endif
}

static void reflect_context_sensitive_popup(prefs_info *prf) {}
static void revert_context_sensitive_popup(prefs_info *prf) {SET_TOGGLE(prf->toggle, include_context_sensitive_popup);}
static void clear_context_sensitive_popup(prefs_info *prf) {SET_TOGGLE(prf->toggle, false);}



/* ---------------- effects menu ---------------- */

static bool include_effects_menu = false;

static void help_effects_menu(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option creates a top-level menu named 'Effects'.  The effects include such things as \
reverberation, reversal, normalizations, gains and envelopes, inversion, echos, flanging, companding, \
filtering, padding, cross synthesis, and so on.",
	   WITH_WORD_WRAP);
}

static bool find_effects_menu(void) {return(XEN_DEFINED_P("effects-menu"));}

static void save_effects_menu(prefs_info *prf, FILE *fd)
{
  include_effects_menu = GET_TOGGLE(prf->toggle);
  if (include_effects_menu)
    {
#if HAVE_SCHEME
      fprintf(fd, "(if (provided? 'snd-motif)\n    (if (not (provided? 'snd-new-effects.scm))\n        (load-from-path \"new-effects.scm\"))\n    (if (not (provided? 'snd-gtk-effects.scm))\n	(load-from-path \"gtk-effects.scm\")))\n");
#endif
#if HAVE_RUBY
      fprintf(fd, "require \"effects\"\n");
#endif
#if HAVE_FORTH
      fprintf(fd, "require effects\n");
#endif
    }
}

static void effects_menu_toggle(prefs_info *prf)
{
  if ((GET_TOGGLE(prf->toggle)) &&
      (!(find_effects_menu())))
#if USE_MOTIF
    xen_load_file_with_path_and_extension("new-effects");
#else
    xen_load_file_with_path_and_extension("gtk-effects");
#endif
}

static void reflect_effects_menu(prefs_info *prf) {}
static void revert_effects_menu(prefs_info *prf) {SET_TOGGLE(prf->toggle, include_effects_menu);}
static void clear_effects_menu(prefs_info *prf) {SET_TOGGLE(prf->toggle, false);}



#if HAVE_SCHEME
/* ---------------- edit menu ---------------- */

static bool include_edit_menu = false;

static void help_edit_menu(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option adds several options to the top-level Edit menu: selection to file, \
append selection, mono to stereo, trim, crop, etc.",
	   WITH_WORD_WRAP);
}

static bool find_edit_menu(void)
{
  return(XEN_DEFINED_P("make-stereofile")); /* a kludge... currently this is only defined in edit-menu.scm */
}

static void save_edit_menu(prefs_info *prf, FILE *fd)
{
  include_edit_menu = GET_TOGGLE(prf->toggle);
  if (include_edit_menu)
    fprintf(fd, "(if (not (provided? 'snd-edit-menu.scm)) (load-from-path \"edit-menu.scm\"))\n"); /* ok for either case */
}

static void edit_menu_toggle(prefs_info *prf)
{
  if ((GET_TOGGLE(prf->toggle)) &&
      (!(find_edit_menu())))
    xen_load_file_with_path_and_extension("edit-menu");
}

static void reflect_edit_menu(prefs_info *prf) {}
static void revert_edit_menu(prefs_info *prf) {SET_TOGGLE(prf->toggle, include_edit_menu);}
static void clear_edit_menu(prefs_info *prf) {SET_TOGGLE(prf->toggle, false);}


/* ---------------- marks menu ---------------- */

static bool include_marks_menu = false;

static bool find_marks_menu(void) {return(XEN_DEFINED_P("marks-menu"));}

static void help_marks_menu(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option adds a top-level 'Marks' menu that includes such things as play between marks, \
trim, crop, define selection via marks, etc",
	   WITH_WORD_WRAP);
}

static void save_marks_menu(prefs_info *prf, FILE *fd)
{
  include_marks_menu = GET_TOGGLE(prf->toggle);
  if (include_marks_menu)
    fprintf(fd, "(if (not (provided? 'snd-marks-menu.scm)) (load-from-path \"marks-menu.scm\"))\n");
}

static void marks_menu_toggle(prefs_info *prf)
{
  if ((GET_TOGGLE(prf->toggle)) &&
      (!(find_marks_menu())))
    xen_load_file_with_path_and_extension("marks-menu");
}

static void reflect_marks_menu(prefs_info *prf) {}
static void revert_marks_menu(prefs_info *prf) {SET_TOGGLE(prf->toggle, include_marks_menu);}
static void clear_marks_menu(prefs_info *prf) {SET_TOGGLE(prf->toggle, false);}


/* ---------------- mix menu ---------------- */

static bool include_mix_menu = false;

static void help_mix_menu(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option adds a top-level 'Mix/Track' menu.  Included are such choices as delete mix, \
snap mix to beat, delete, play, reverse, save, and transpose track, set track amplitude, speed, and tempo, etc.",
	   WITH_WORD_WRAP);
}

static bool find_mix_menu(void) {return(XEN_DEFINED_P("mix-menu"));}

static void save_mix_menu(prefs_info *prf, FILE *fd)
{
  include_mix_menu = GET_TOGGLE(prf->toggle);
  if (include_mix_menu)
    fprintf(fd, "(if (not (provided? 'snd-mix-menu.scm)) (load-from-path \"mix-menu.scm\"))\n");
}

static void mix_menu_toggle(prefs_info *prf)
{
  if ((GET_TOGGLE(prf->toggle)) &&
      (!(find_mix_menu())))
    xen_load_file_with_path_and_extension("mix-menu");
}

static void reflect_mix_menu(prefs_info *prf) {}
static void revert_mix_menu(prefs_info *prf) {SET_TOGGLE(prf->toggle, include_mix_menu);}
static void clear_mix_menu(prefs_info *prf) {SET_TOGGLE(prf->toggle, false);}


/* ---------------- icon box ---------------- */

static bool include_icon_box = false;

static void help_icon_box(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option adds a top-level box full of various handy icons.  Not implemented in Gtk yet.",
	   WITH_WORD_WRAP);
}

static bool find_icon_box(void) {return(XEN_DEFINED_P("add-useful-icons"));}

static void save_icon_box(prefs_info *prf, FILE *fd)
{
  include_icon_box = GET_TOGGLE(prf->toggle);
  if (include_icon_box)
    fprintf(fd, "(if (not (provided? 'snd-toolbar.scm)) (load-from-path \"toolbar.scm\"))\n");
}

static void icon_box_toggle(prefs_info *prf)
{
  if ((GET_TOGGLE(prf->toggle)) &&
      (!(find_icon_box())))
    xen_load_file_with_path_and_extension("new-buttons");
}

static void reflect_icon_box(prefs_info *prf) {}
static void revert_icon_box(prefs_info *prf) {SET_TOGGLE(prf->toggle, include_icon_box);}
static void clear_icon_box(prefs_info *prf) {SET_TOGGLE(prf->toggle, false);}
#endif


/* ---------------- reopen menu ---------------- */

static bool include_reopen_menu = false;

static void help_reopen_menu(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option adds a top-level 'Reopen' menu. Previously opened sounds that are not currently open are listed \
as menu items.",
	   WITH_WORD_WRAP);
}

static bool find_reopen_menu(void)
{
  return((XEN_DEFINED_P("including-reopen-menu")) &&
	 XEN_TO_C_BOOLEAN(XEN_NAME_AS_C_STRING_TO_VALUE("including-reopen-menu")));
}

static void save_reopen_menu(prefs_info *prf, FILE *fd)
{
  include_reopen_menu = GET_TOGGLE(prf->toggle);
  if (include_reopen_menu)
    prefs_function_save_0(fd, "with-reopen-menu", "extensions");
}

static void reopen_menu_toggle(prefs_info *prf)
{
  if ((GET_TOGGLE(prf->toggle)) &&
      (!(find_reopen_menu())))
    {
      xen_load_file_with_path_and_extension("extensions");
      prefs_function_call_0("with-reopen-menu");
    }
}

static void reflect_reopen_menu(prefs_info *prf) {}
static void revert_reopen_menu(prefs_info *prf) {SET_TOGGLE(prf->toggle, include_reopen_menu);}
static void clear_reopen_menu(prefs_info *prf) {SET_TOGGLE(prf->toggle, false);}



#if USE_MOTIF
/* ---------------- mark-pane ---------------- */

static bool include_mark_pane = false;

static void help_mark_pane(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option adds a pane to each channel window containing information about that channel's marks.",
	   WITH_WORD_WRAP);
}

static bool find_mark_pane(void)
{
  return((XEN_DEFINED_P("including-mark-pane")) &&
	 XEN_TO_C_BOOLEAN(XEN_NAME_AS_C_STRING_TO_VALUE("including-mark-pane")));
}

#if HAVE_SCHEME
  #if USE_MOTIF
    #define MARK_PANE_SOURCE "snd-motif"
  #else
    #define MARK_PANE_SOURCE "snd-gtk"
  #endif
#else
    #define MARK_PANE_SOURCE "snd-xm"
#endif

static void mark_pane_toggle(prefs_info *prf)
{
  if ((GET_TOGGLE(prf->toggle)) &&
      (!(find_mark_pane())))
    {
      xen_load_file_with_path_and_extension(MARK_PANE_SOURCE);
      prefs_function_call_0("add-mark-pane");
    }
}

static void save_mark_pane(prefs_info *prf, FILE *fd)
{
  include_mark_pane = GET_TOGGLE(prf->toggle);
  if (include_mark_pane)
    prefs_function_save_0(fd, "add-mark-pane", MARK_PANE_SOURCE);
}

static void reflect_mark_pane(prefs_info *prf) {}
static void revert_mark_pane(prefs_info *prf) {SET_TOGGLE(prf->toggle, include_mark_pane);}
static void clear_mark_pane(prefs_info *prf) {SET_TOGGLE(prf->toggle, false);}
#endif


#if HAVE_SCHEME
/* ---------------- hidden controls dialog ---------------- */

static bool include_hidden_controls = false;

static void help_hidden_controls(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This adds a 'Hidden Controls' option to the Option menu.  The dialog \
gives access to all the synthesis variables that aren't reflected in the standard \
control panel: 'expand-hop' sets the hop size (per grain), 'expand-length' \
sets the grain length, 'expand-ramp' sets the slope of the grain amplitude envelope, \
'contrast-amp' sets the prescaler for the contrast effect, 'reverb-feedback' sets the feedback \
amount in the reverberator (it sets all the comb filter scalers), and 'reverb-lowpass' sets \
the lowpass filter coefficient in the reverberator.",
	   WITH_WORD_WRAP);
}

static bool find_hidden_controls(void)
{
  return((XEN_DEFINED_P("hidden-controls-dialog")) &&
	 (XEN_NOT_FALSE_P(XEN_NAME_AS_C_STRING_TO_VALUE("hidden-controls-dialog"))));
}

static void save_hidden_controls(prefs_info *prf, FILE *fd)
{
  include_hidden_controls = GET_TOGGLE(prf->toggle);
  if (include_hidden_controls)
    {
      fprintf(fd, "(if (not (provided? 'snd-snd-motif.scm)) (load-from-path \"snd-motif.scm\"))\n");
      fprintf(fd, "(make-hidden-controls-dialog)\n");
    }
}

static void hidden_controls_toggle(prefs_info *prf)
{
  if ((GET_TOGGLE(prf->toggle)) &&
      (!(find_hidden_controls())))
    {
      xen_load_file_with_path_and_extension("snd-motif");
      prefs_function_call_0("make-hidden-controls-dialog");
    }
}

static void reflect_hidden_controls(prefs_info *prf) {}
static void revert_hidden_controls(prefs_info *prf) {SET_TOGGLE(prf->toggle, include_hidden_controls);}
static void clear_hidden_controls(prefs_info *prf) {SET_TOGGLE(prf->toggle, false);}

#endif



#if HAVE_GUILE
/* ---------------- debugging aids ---------------- */

static bool include_debugging_aids = false;

static bool find_debugging_aids(void)
{
  return((XEN_DEFINED_P("snd-break")) && 
	 (XEN_DEFINED_P("untrace-stack")));
}

static void reflect_debugging_aids(prefs_info *prf) {}
static void revert_debugging_aids(prefs_info *prf) {SET_TOGGLE(prf->toggle, include_debugging_aids);}
static void clear_debugging_aids(prefs_info *prf) {SET_TOGGLE(prf->toggle, false);}

static void debugging_aids_toggle(prefs_info *prf)
{
  if ((GET_TOGGLE(prf->toggle)) &&
      (!(find_debugging_aids())))
    xen_load_file_with_path_and_extension("debug");
}

static void save_debugging_aids(prefs_info *prf, FILE *fd)
{
  include_debugging_aids = GET_TOGGLE(prf->toggle);
  if (include_debugging_aids)
    {
      fprintf(fd, "(use-modules (ice-9 debug) (ice-9 session))\n");
#if HAVE_SCM_OBJECT_TO_STRING
      /* 1.6 or later -- not sure 1.4 etc can handle these things */
      fprintf(fd, "(debug-set! stack 0)\n");
      fprintf(fd, "(debug-enable 'debug 'backtrace)\n");
      fprintf(fd, "(read-enable 'positions)\n");
#endif
      fprintf(fd, "(if (not (provided? 'snd-debug.scm)) (load-from-path \"debug.scm\"))\n");
    }
}
#endif

/* ---------------- smpte ---------------- */

static bool include_smpte = false;

static void help_smpte(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option adds a label to the time domain graph showing the current SMPTE frame of the leftmost sample.",
	   WITH_WORD_WRAP);
}

static bool find_smpte(void) 
{
#if HAVE_SCHEME 
  return((XEN_DEFINED_P("smpte-is-on")) && 
	 (!(XEN_FALSE_P(XEN_EVAL_C_STRING("(smpte-is-on)"))))); /* "member" of hook-list -> a list if successful */ 
#endif
#if HAVE_RUBY || HAVE_FORTH
  return((XEN_DEFINED_P("smpte-is-on")) && 
	 XEN_TO_C_BOOLEAN(XEN_EVAL_C_STRING(TO_PROC_NAME("smpte-is-on")))); /* "member" of hook-list -> true */ 
#endif 
  return(false);
}

static void reflect_smpte(prefs_info *prf) {}
static void revert_smpte(prefs_info *prf) {SET_TOGGLE(prf->toggle, include_smpte);}
static void clear_smpte(prefs_info *prf) {SET_TOGGLE(prf->toggle, false);}

#if HAVE_SCHEME
  #if USE_MOTIF
    #define SMPTE_SOURCE "snd-motif"
  #else
    #define SMPTE_SOURCE "snd-gtk"
  #endif
#else
    #define SMPTE_SOURCE "snd-xm"
#endif

static void smpte_toggle(prefs_info *prf)
{
  if ((GET_TOGGLE(prf->toggle)) &&
      (!(find_smpte())))
    xen_load_file_with_path_and_extension(SMPTE_SOURCE);
  if (find_smpte())
    prefs_function_call_1("show-smpte-label", C_TO_XEN_BOOLEAN(GET_TOGGLE(prf->toggle)));
}

static void save_smpte(prefs_info *prf, FILE *fd)
{
  include_smpte = GET_TOGGLE(prf->toggle);
  if (include_smpte)
    {
#if HAVE_SCHEME
  fprintf(fd, "(if (provided? 'snd-motif)\n    (if (not (provided? 'snd-snd-motif.scm))\n        (load-from-path \"snd-motif.scm\"))\n    (if (not (provided? 'snd-snd-gtk.scm))\n        (load-from-path \"snd-gtk.scm\")))\n");
  fprintf(fd, "(show-smpte-label #t)\n");
#endif
#if HAVE_RUBY
  fprintf(fd, "require \"snd-xm\"\n");
  fprintf(fd, "show_smpte_label(true)\n");
#endif
#if HAVE_FORTH
  fprintf(fd, "require snd-xm\n");
  fprintf(fd, "\\ #t show-smpte-label drop\n"); 
  /* SOMEDAY: fs show-smpte-label sometimes hangs */
#endif
    }
}


/* ---------------- remember-sound-state ---------------- */

static int rts_remember_sound_state_choice = 0, remember_sound_state_choice = 0; /* 0=none, 1=local, 2=global+not local, 3=local+global */

static void help_remember_sound_state_choice(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option causes Snd to save most of a sound's display state when it is closed, \
and if that same sound is later re-opened, Snd restores the previous state. This only takes effect upon restarting Snd.",
	   WITH_WORD_WRAP);
}

static int find_remember_sound_state_choice(void) {return(XEN_TO_C_INT_OR_ELSE(prefs_variable_get("remembering-sound-state"), 0));}
static void revert_remember_sound_state(prefs_info *prf) {remember_sound_state_choice = rts_remember_sound_state_choice;}
static void clear_remember_sound_state(prefs_info *prf) {remember_sound_state_choice = 0;}

static void reflect_remember_sound_state_choice(prefs_info *prf)
{
  SET_TOGGLE(prf->toggle, remember_sound_state_choice & 1);
  SET_TOGGLE(prf->toggle2, remember_sound_state_choice & 2);
}

static void save_remember_sound_state_choice(prefs_info *prf, FILE *fd)
{
  rts_remember_sound_state_choice = remember_sound_state_choice;
  if (remember_sound_state_choice != 0)
    {
#if HAVE_SCHEME
      fprintf(fd, "(if (not (provided? 'snd-extensions.scm)) (load-from-path \"extensions.scm\"))\n");
      fprintf(fd, "(remember-sound-state %d)\n", remember_sound_state_choice);
#endif
#if HAVE_RUBY
      fprintf(fd, "require \"extensions\"\n");
      if (remember_sound_state_choice & 2)
	fprintf(fd, "remember_all_sound_properties\n");
      else fprintf(fd, "remember_sound_state\n");
#endif
#if HAVE_FORTH
      fprintf(fd, "require extensions\n");
      fprintf(fd, "%d remember-sound-state\n", remember_sound_state_choice);
#endif
    }
}

static void remember_sound_state_1_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->toggle))
    remember_sound_state_choice |= 1;
  else remember_sound_state_choice &= 2;
}

static void remember_sound_state_2_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->toggle2))
    remember_sound_state_choice |= 2;
  else remember_sound_state_choice &= 1;
}


/* ---------------- show-axes ---------------- */

static show_axes_t rts_show_axes = DEFAULT_SHOW_AXES;

static const char *show_axes_choices[NUM_SHOW_AXES] = {"none", "X and Y", "just X", "X and Y unlabelled", "just X unlabelled", "bare X"};

static void reflect_show_axes(prefs_info *prf) {SET_TEXT(prf->text, (char *)show_axes_choices[(int)show_axes(ss)]);}
static void revert_show_axes(prefs_info *prf) {in_set_show_axes(rts_show_axes);}
static void clear_show_axes(prefs_info *prf) {in_set_show_axes(DEFAULT_SHOW_AXES);}
static void save_show_axes(prefs_info *prf, FILE *ignore) {rts_show_axes = show_axes(ss);}

#if USE_MOTIF
static void show_axes_from_menu(prefs_info *prf, char *value)
{
  int i;
  for (i = 0; i < NUM_SHOW_AXES; i++)
    if (strcmp(value, show_axes_choices[i]) == 0)
      {
	in_set_show_axes((show_axes_t)i);
	SET_TEXT(prf->text, value);
	return;
      }
}
#endif

static void show_axes_from_text(prefs_info *prf)
{
  int i;
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      char *trimmed_str;
      trimmed_str = trim_string(str);
      FREE_TEXT(str);
      if (snd_strlen(trimmed_str) > 0)
	{
	  int curpos = -1;
	  for (i = 0; i < NUM_SHOW_AXES; i++)
	    if (STRCMP(trimmed_str, show_axes_choices[i]) == 0)
	      {
		curpos = i;
		break;
	      }
	  if (curpos >= 0)
	    in_set_show_axes((show_axes_t)curpos);
	  else post_prefs_error("unknown axis choice", prf);
	}
      else post_prefs_error("need an axis choice", prf);
      FREE(trimmed_str);
    }
  else post_prefs_error("need an axis choice", prf);
}

/* ---------------- x-axis-style ---------------- */

static x_axis_style_t rts_x_axis_style = DEFAULT_X_AXIS_STYLE;

static const char *x_axis_styles[NUM_X_AXIS_STYLES] = {"seconds", "samples", "% of total", "beats", "measures", "clock"};

static void reflect_x_axis_style(prefs_info *prf) {SET_TEXT(prf->text, (char *)x_axis_styles[(int)x_axis_style(ss)]);}
static void revert_x_axis_style(prefs_info *prf) {in_set_x_axis_style(rts_x_axis_style);}
static void clear_x_axis_style(prefs_info *prf) {in_set_x_axis_style(DEFAULT_X_AXIS_STYLE);}
static void save_x_axis_style(prefs_info *prf, FILE *ignore) {rts_x_axis_style = x_axis_style(ss);}

#if USE_MOTIF
static void x_axis_style_from_menu(prefs_info *prf, char *value)
{
  int i;
  for (i = 0; i < NUM_X_AXIS_STYLES; i++)
    if (strcmp(value, x_axis_styles[i]) == 0)
      {
	in_set_x_axis_style((x_axis_style_t)i);
	SET_TEXT(prf->text, value);
	return;
      }
}
#endif

static void x_axis_style_from_text(prefs_info *prf)
{
  int i;
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      char *trimmed_str;
      trimmed_str = trim_string(str);
      FREE_TEXT(str);
      if (snd_strlen(trimmed_str) > 0)
	{
	  int curpos = -1;
	  for (i = 0; i < NUM_X_AXIS_STYLES; i++)
	    if (STRCMP(trimmed_str, x_axis_styles[i]) == 0)
	      {
		curpos = i;
		break;
	      }
	  if (curpos >= 0)
	    in_set_x_axis_style((x_axis_style_t)curpos);
	  else post_prefs_error("unknown axis style", prf);
	}
      else post_prefs_error("need an axis style", prf);
      FREE(trimmed_str);
    }
  else post_prefs_error("need an axis style", prf);
}


/* ---------------- transform-type ---------------- */

static int rts_transform_type = DEFAULT_TRANSFORM_TYPE;

static const char *transform_types[NUM_BUILTIN_TRANSFORM_TYPES] = {"Fourier", "Wavelet", "Walsh", "Autocorrelate", "Cepstrum", "Haar"};

static list_completer_info *transform_type_completer_info = NULL;

static void reflect_transform_type(prefs_info *prf)
{
  SET_TEXT(prf->text, (char *)transform_types[mus_iclamp(0, transform_type(ss), NUM_BUILTIN_TRANSFORM_TYPES - 1)]); 
}

static void revert_transform_type(prefs_info *prf) {in_set_transform_type(rts_transform_type);}
static void clear_transform_type(prefs_info *prf) {in_set_transform_type(DEFAULT_TRANSFORM_TYPE);}
static void save_transform_type(prefs_info *prf, FILE *ignore) {rts_transform_type = transform_type(ss);}

static char *transform_type_completer(char *text, void *data)
{
  if (!transform_type_completer_info)
    {
      transform_type_completer_info = (list_completer_info *)CALLOC(1, sizeof(list_completer_info));
      transform_type_completer_info->exact_match = false;
      transform_type_completer_info->values = (char **)transform_types;
      transform_type_completer_info->num_values = NUM_BUILTIN_TRANSFORM_TYPES;
      transform_type_completer_info->values_size = NUM_BUILTIN_TRANSFORM_TYPES;
    }
  return(list_completer(text, (void *)transform_type_completer_info));
}

#if USE_MOTIF
static void transform_type_from_menu(prefs_info *prf, char *value)
{
  int i;
  for (i = 0; i < NUM_BUILTIN_TRANSFORM_TYPES; i++)
    if (strcmp(value, transform_types[i]) == 0)
      {
	in_set_transform_type(i);
	SET_TEXT(prf->text, value);
	return;
      }
}
#endif

static void transform_type_from_text(prefs_info *prf)
{
  int i;
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      char *trimmed_str;
      trimmed_str = trim_string(str);
      FREE_TEXT(str);
      if (snd_strlen(trimmed_str) > 0)
	{
	  int curpos = -1;
	  for (i = 0; i < NUM_BUILTIN_TRANSFORM_TYPES; i++)
	    if (STRCMP(trimmed_str, transform_types[i]) == 0)
	      {
		curpos = i;
		break;
	      }
	  if (curpos >= 0)
	    in_set_transform_type(curpos);
	  else post_prefs_error("unknown tranform", prf);
	}
      else post_prefs_error("no transform?", prf);
      FREE(trimmed_str);
    }
  else post_prefs_error("no transform?", prf);
}


/* -------- fft-window -------- */

static mus_fft_window_t rts_fft_window = DEFAULT_FFT_WINDOW;

static const char *fft_windows[MUS_NUM_WINDOWS] = 
  {"Rectangular", "Hann", "Welch", "Parzen", "Bartlett", "Hamming", "Blackman2", "Blackman3", "Blackman4",
   "Exponential", "Riemann", "Kaiser", "Cauchy", "Poisson", "Gaussian", "Tukey", "Dolph-Chebyshev", "Hann-Poisson", "Connes",
   "Samaraki", "Ultraspherical"};

static void reflect_fft_window(prefs_info *prf) {SET_TEXT(prf->text, (char *)fft_windows[(int)fft_window(ss)]);}
static void revert_fft_window(prefs_info *prf) {in_set_fft_window(rts_fft_window);}
static void clear_fft_window(prefs_info *prf) {in_set_fft_window(DEFAULT_FFT_WINDOW);}
static void save_fft_window(prefs_info *prf, FILE *ignore) {rts_fft_window = fft_window(ss);}

static list_completer_info *fft_window_completer_info = NULL;

static char *fft_window_completer(char *text, void *data)
{
  if (!fft_window_completer_info)
    {
      fft_window_completer_info = (list_completer_info *)CALLOC(1, sizeof(list_completer_info));
      fft_window_completer_info->exact_match = false;
      fft_window_completer_info->values = (char **)fft_windows;
      fft_window_completer_info->num_values = MUS_NUM_WINDOWS;
      fft_window_completer_info->values_size = MUS_NUM_WINDOWS;
    }
  return(list_completer(text, (void *)fft_window_completer_info));
}

#if USE_MOTIF
static void fft_window_from_menu(prefs_info *prf, char *value)
{
  int i;
  for (i = 0; i < MUS_NUM_WINDOWS; i++)
    if (strcmp(value, fft_windows[i]) == 0)
      {
	in_set_fft_window((mus_fft_window_t)i);
	SET_TEXT(prf->text, value);
	return;
      }
}
#endif

static void fft_window_from_text(prefs_info *prf)
{
  int i;
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      char *trimmed_str;
      trimmed_str = trim_string(str);
      FREE_TEXT(str);
      if (snd_strlen(trimmed_str) > 0)
	{
	  int curpos = -1;
	  for (i = 0; i < MUS_NUM_WINDOWS; i++)
	    if (STRCMP(trimmed_str, fft_windows[i]) == 0)
	      {
		curpos = i;
		break;
	      }
	  if (curpos >= 0)
	    in_set_fft_window((mus_fft_window_t)curpos);
	  else post_prefs_error("unknown window", prf);
	}
      else post_prefs_error("no window?", prf);
      FREE(trimmed_str);
    }
  else post_prefs_error("no window?", prf);
}


/* ---------------- colormap ---------------- */

static int rts_colormap = DEFAULT_COLOR_MAP;

static char *colormap_completer(char *text, void *data)
{
  list_completer_info *compinfo;
  char **cmaps;
  int i, len;
  char *result;
  len = num_colormaps();
  cmaps = (char **)CALLOC(len, sizeof(char *));
  for (i = 0; i < len; i++)
    cmaps[i] = colormap_name(i);
  compinfo = (list_completer_info *)CALLOC(1, sizeof(list_completer_info));
  compinfo->exact_match = false;
  compinfo->values = (char **)fft_windows;
  compinfo->num_values = len;
  compinfo->values_size = len;
  result = list_completer(text, (void *)compinfo);
  FREE(cmaps);
  return(result);
}

static void reflect_colormap(prefs_info *prf) {SET_TEXT(prf->text, colormap_name(color_map(ss)));}
static void clear_colormap(prefs_info *prf) {in_set_color_map(DEFAULT_COLOR_MAP);}
static void save_colormap(prefs_info *prf, FILE *ignore) {rts_colormap = color_map(ss);}

static void revert_colormap(prefs_info *prf) 
{
  if (!(is_colormap(rts_colormap))) rts_colormap = DEFAULT_COLOR_MAP;
  in_set_color_map(rts_colormap);
}

static void colormap_from_text(prefs_info *prf)
{
  int i;
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      char *trimmed_str;
      trimmed_str = trim_string(str);
      FREE_TEXT(str);
      if (snd_strlen(trimmed_str) > 0)
	{
	  int len, curpos = -1;
	  len = num_colormaps();
	  for (i = 0; i < len; i++)
	    if ((colormap_name(i)) &&
		(STRCMP(trimmed_str, colormap_name(i)) == 0))
	      {
		curpos = i;
		break;
	      }
	  if (is_colormap(curpos))
	    in_set_color_map(curpos);
	  else post_prefs_error("unknown colormap", prf);
	}
      else post_prefs_error("no colormap?", prf);
      FREE(trimmed_str);
    }
  else post_prefs_error("no colormap?", prf);
}

#if USE_MOTIF
static void colormap_from_menu(prefs_info *prf, char *value)
{
  int i, len;
  len = num_colormaps();
  for (i = 0; i < len; i++)
    if ((colormap_name(i)) &&
	(strcmp(value, colormap_name(i)) == 0))
      {
	in_set_color_map(i);
	SET_TEXT(prf->text, value);
	return;
      }
}
#endif


/* ---------------- peak-envs ---------------- */

static bool include_peak_envs = false, rts_peak_envs = false;
static char *include_peak_env_directory = NULL, *rts_peak_env_directory = NULL;

static void help_peak_envs(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "When a very large file is first opened, Snd scans all the data to build up an overall \
representation of the sound.  If you like to view the entire sound upon opening it, you can speed \
up the process a lot by saving this initial representation.  The data is called a 'peak-env' file \
and it resides in the 'peak-env-directory'.",
	   WITH_WORD_WRAP);
}

static bool find_peak_envs(void) {return(XEN_TO_C_BOOLEAN(prefs_variable_get("save-peak-env-info?")));}

static void clear_peak_envs(prefs_info *prf)
{
  if (include_peak_env_directory) FREE(include_peak_env_directory); 
  include_peak_env_directory = NULL;
  include_peak_envs = false;
}

static void revert_peak_envs(prefs_info *prf)
{
  if (include_peak_env_directory) FREE(include_peak_env_directory); 
  include_peak_env_directory = copy_string(rts_peak_env_directory);
  include_peak_envs = rts_peak_envs;
}

static void save_peak_envs(prefs_info *prf, FILE *fd)
{
  rts_peak_envs = GET_TOGGLE(prf->toggle);
  if (rts_peak_env_directory) FREE(rts_peak_env_directory);
  rts_peak_env_directory = copy_string(include_peak_env_directory);
  if (rts_peak_envs)
    {
#if HAVE_SCHEME
      fprintf(fd, "(if (not (provided? 'snd-peak-env.scm)) (load-from-path \"peak-env.scm\"))\n");
      if (include_peak_env_directory)
	fprintf(fd, "(set! save-peak-env-info-directory \"%s\")\n", include_peak_env_directory);
#endif
#if HAVE_RUBY
      fprintf(fd, "require \"env\"\n");
      if (include_peak_env_directory)
	fprintf(fd, "$save_peak_env_info_directory = \"%s\"\n", include_peak_env_directory);
#endif
#if HAVE_FORTH
      fprintf(fd, "require peak-env\n");
      if (include_peak_env_directory)
	fprintf(fd, "\"%s\" to save-peak-env-info-directory\n", include_peak_env_directory);
#endif
    }
}

static char *peak_env_directory(void)
{
  if (include_peak_env_directory)
    return(include_peak_env_directory);
  if (XEN_DEFINED_P("save-peak-env-info-directory"))
    return(XEN_TO_C_STRING(XEN_NAME_AS_C_STRING_TO_VALUE("save-peak-env-info-directory")));
  return(NULL);
}

static void reflect_peak_envs(prefs_info *prf) 
{
  SET_TOGGLE(prf->toggle, include_peak_envs);
  SET_TEXT(prf->text, include_peak_env_directory);
}

static void peak_envs_toggle(prefs_info *prf)
{
  include_peak_envs = GET_TOGGLE(prf->toggle);
}

static void peak_envs_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      if (include_peak_env_directory) {FREE(include_peak_env_directory); include_peak_env_directory = NULL;}
      include_peak_env_directory = copy_string(str);
      FREE_TEXT(str);
    }
}


/* ---------------- load path ---------------- */

static char *rts_load_path = NULL;

static void help_load_path(prefs_info *prf)
{
  char *hlp;
  hlp = mus_format("Much of Snd's functionality is loaded as needed from the Scheme, Ruby, or Forth \
files found in the Snd tarball.  You can run Snd without \
these files, but there's no reason to!  Just add the directory containing \
them to the load path variable%s.  " XEN_LANGUAGE_NAME " searches these \
directories for any *." XEN_FILE_EXTENSION " files that it can't \
find elsewhere.  The current load path list is: \n\n%s\n",
#if HAVE_GUILE
		   ", %load-path",
#else
#if HAVE_RUBY
		   ", $LOAD_PATH",
#else
#if HAVE_FORTH || HAVE_GAUCHE
		   ", *load-path*",
#else
		   "",
#endif
#endif
#endif
		   XEN_AS_STRING(XEN_LOAD_PATH));
  snd_help("load paths", hlp, 	   WITH_WORD_WRAP);
  FREE(hlp);
}

static char *find_sources(void) /* returns full filename if found else null */
{
  char *file = NULL;
  #define BASE_FILE "extensions." XEN_FILE_EXTENSION

#if HAVE_GUILE
  {
    XEN xfile;
    xfile = scm_sys_search_load_path(C_TO_XEN_STRING(BASE_FILE));
    if (XEN_STRING_P(xfile))
      file = copy_string(XEN_TO_C_STRING(xfile));
  }
#endif

#if HAVE_GAUCHE
  /* mimic Forth code below -- get *load-path* value and run through it */
  {
      int i, len, base_len;
      XEN load_path;
      load_path = XEN_LOAD_PATH;
      len = XEN_LIST_LENGTH(load_path);
      base_len = strlen(BASE_FILE);
      for (i = 0; i < len; i++)
	{
	  char *fname, *path;
	  int flen;
	  path = XEN_TO_C_STRING(XEN_LIST_REF(load_path, i));
	  flen = base_len + 32 + strlen(path);
	  fname = (char *)CALLOC(flen, sizeof(char));
	  snprintf(fname, flen, "%s/%s", path, BASE_FILE);
	  if (mus_file_probe(fname)) 
	    {
	      file = fname;
	      break;
	    }
	  FREE(fname);
	}
    }
#endif

#if HAVE_RUBY
  #if RB_FIND_FILE_TAKES_VALUE
  {
    XEN xfile;
    xfile = rb_find_file(C_TO_XEN_STRING(BASE_FILE));
    if (XEN_STRING_P(xfile))
      file = mus_expand_filename(XEN_TO_C_STRING(xfile));
  }
  #else
    file = mus_expand_filename(rb_find_file(BASE_FILE));
  #endif
#endif

#if HAVE_FORTH
    {
      /* taken from fth/src/misc.c -- fth_find_file looks for an already-loaded file */
      int i, len, base_len;
      XEN load_path;
      load_path = XEN_LOAD_PATH;
      len = fth_array_length(load_path);
      base_len = strlen(BASE_FILE);
      for (i = 0; i < len; i++)
	{
	  char *fname, *path;
	  int flen;
	  path = fth_string_ref(fth_array_ref(load_path, i));
	  flen = base_len + 32 + strlen(path);
	  fname = (char *)CALLOC(flen, sizeof(char));
	  snprintf(fname, flen, "%s/%s", path, BASE_FILE);
	  if (mus_file_probe(fname)) 
	    {
	      file = fname;
	      break;
	    }
	  FREE(fname);
	}
    }
#endif

  if (file)
    {
      int len, exts_len;
      len = snd_strlen(file);
      exts_len = strlen(BASE_FILE);
      if (len > exts_len)
	file[len - exts_len - 1] = '\0';
      return(file);
    }
  return(NULL);
}

static void clear_load_path(prefs_info *prf)
{
  char *str;
  str = find_sources();
  SET_TEXT(prf->text, str);
  if (str) 
    {
      black_text(prf);
      FREE(str);
    }
  else red_text(prf);
}

static void revert_load_path(prefs_info *prf)
{
  SET_TEXT(prf->text, rts_load_path);
  if (rts_load_path) 
    black_text(prf);
  else red_text(prf);
}

static void reflect_load_path(prefs_info *prf) {}

static void load_path_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((!str) || (!(*str)))
    return;
  if (local_access(str))
    {
      black_text(prf);
      if (include_load_path) FREE(include_load_path);
      include_load_path = copy_string(str);
      XEN_ADD_TO_LOAD_PATH(include_load_path);
    }
  if (str) {FREE_TEXT(str);}
}



/* ---------------- initial bounds ---------------- */

static Float rts_initial_beg = 0.0, rts_initial_dur = 0.1;
static bool rts_full_duration = false;
static bool include_duration = false;

static bool full_duration(void)
{
  return(XEN_TO_C_BOOLEAN(prefs_variable_get("prefs-show-full-duration")));
}

static Float initial_beg(void)
{
  return(XEN_TO_C_DOUBLE_OR_ELSE(prefs_variable_get("prefs-initial-beg"), 0.0));
}

static Float initial_dur(void)
{
  return(XEN_TO_C_DOUBLE_OR_ELSE(prefs_variable_get("prefs-initial-dur"), 0.1));
}

static void help_initial_bounds(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "Normally Snd displays just the first 0.1 seconds of a sound in its initial graph. This option \
sets either new bounds for that display, or directs Snd to display the entire sound.",
	   WITH_WORD_WRAP);
}

static char *initial_bounds_to_string(void)
{
  return(mus_format("%.2f : %.2f", initial_beg(), initial_dur()));
}

static void save_initial_bounds(prefs_info *prf, FILE *fd)
{
  char *str;
  rts_full_duration = GET_TOGGLE(prf->toggle);
  str = GET_TEXT(prf->text);
  if (str)
    {
      float a = 0.0, b = 0.0;
      sscanf(str, "%f : %f", &a, &b);  /* these can be doubles -- need conversion to fit all cases */
      rts_initial_beg = (Float)a;
      rts_initial_dur = (Float)b;
      FREE_TEXT(str);
    }
  else
    {
      rts_initial_beg = 0.0;
      rts_initial_dur = 0.1;
    }
  if (include_duration)
    {
#if HAVE_SCHEME
      fprintf(fd, "(if (not (provided? 'snd-extensions.scm)) (load-from-path \"extensions.scm\"))\n");
      fprintf(fd, "(prefs-activate-initial-bounds %.2f %.2f %s)\n", rts_initial_beg, rts_initial_dur, (rts_full_duration) ? "#t" : "#f");
#endif
#if HAVE_RUBY
      fprintf(fd, "require \"extensions\"\n");
      fprintf(fd, "prefs_activate_initial_bounds(%.2f, %.2f, %s)\n", rts_initial_beg, rts_initial_dur, (rts_full_duration) ? "true" : "false");

#endif
#if HAVE_FORTH
      fprintf(fd, "require extensions\n");
      fprintf(fd, "%.2f %.2f %s prefs-activate-initial-bounds\n", rts_initial_beg, rts_initial_dur, (rts_full_duration) ? "true" : "false");
#endif
    }
}

static void reflect_initial_bounds(prefs_info *prf)
{
  /* text has beg : dur, toggle true if full dur */
  char *str;
  str = initial_bounds_to_string();
  SET_TEXT(prf->text, str);
  FREE(str);
  SET_TOGGLE(prf->toggle, full_duration());
}

static void revert_initial_bounds(prefs_info *prf)
{
  prefs_variable_set("prefs-initial-beg", C_TO_XEN_DOUBLE(rts_initial_beg));
  prefs_variable_set("prefs-initial-dur", C_TO_XEN_DOUBLE(rts_initial_dur));
  prefs_variable_set("prefs-show-full-duration", C_TO_XEN_BOOLEAN(rts_full_duration));
}

static void clear_initial_bounds(prefs_info *prf)
{
  prefs_variable_set("prefs-initial-beg", C_TO_XEN_DOUBLE(0.0));
  prefs_variable_set("prefs-initial-dur", C_TO_XEN_DOUBLE(0.1));
  prefs_variable_set("prefs-show-full-duration", XEN_FALSE);
}

static void initial_bounds_toggle(prefs_info *prf)
{
  include_duration = true;
  if (!(XEN_DEFINED_P("prefs-show-full-duration")))
    xen_load_file_with_path_and_extension("extensions");
  prefs_variable_set("prefs-show-full-duration", C_TO_XEN_BOOLEAN(GET_TOGGLE(prf->toggle)));
}

static void initial_bounds_text(prefs_info *prf)
{
  float beg = 0.0, dur = 0.1;
  char *str;
  include_duration = true;
  str = GET_TEXT(prf->text);
  sscanf(str, "%f : %f", &beg, &dur);
  if (!(XEN_DEFINED_P("prefs-initial-beg")))
    xen_load_file_with_path_and_extension("extensions");
  prefs_variable_set("prefs-initial-beg", C_TO_XEN_DOUBLE(beg));
  prefs_variable_set("prefs-initial-dur", C_TO_XEN_DOUBLE(dur));
  FREE_TEXT(str);
}


/* ---------------- keys ---------------- */

static void reflect_key(prefs_info *prf, const char *key_name)
{
  key_info *ki;
  ki = find_prefs_key_binding(key_name);
  SET_TOGGLE(prf->toggle, ki->c);
  SET_TOGGLE(prf->toggle2, ki->m);
  SET_TOGGLE(prf->toggle3, ki->x);
  SET_TEXT(prf->text, ki->key);
  FREE(ki);
}

static void save_key_binding(prefs_info *prf, FILE *fd, char *(*binder)(char *key, bool c, bool m, bool x))
{
  char *key, *expr;
  key = GET_TEXT(prf->text);
  if ((key) && (*key))
    {
      expr = (*binder)(key, 
		       GET_TOGGLE(prf->toggle),
		       GET_TOGGLE(prf->toggle2),
		       GET_TOGGLE(prf->toggle3));
      fprintf(fd, expr);
      FREE(expr);
      FREE_TEXT(key);
    }
}

static void key_bind(prefs_info *prf, char *(*binder)(char *key, bool c, bool m, bool x))
{
  char *key, *expr;
  bool ctrl, meta, cx;
  key = GET_TEXT(prf->text);
  ctrl = GET_TOGGLE(prf->toggle);
  meta = GET_TOGGLE(prf->toggle2);
  cx = GET_TOGGLE(prf->toggle3);
  if ((key) && (*key))
    {
      expr = (*binder)(key, ctrl, meta, cx);
      FREE_TEXT(key);
      XEN_EVAL_C_STRING(expr);
      FREE(expr);
    }
}

static void clear_key(prefs_info *prf, const char *name)
{
  key_info *ki;
  ki = find_prefs_key_binding(name);
  if (ki)
    {
      if (ki->key)
	{
	  int state = 0;
	  if (ki->c) state |= 4;
	  if (ki->m) state |= 8;
#if USE_MOTIF
	  set_keymap_entry((int)XStringToKeysym(ki->key), state, 0, XEN_UNDEFINED, ki->x, name, name);
#else
	  set_keymap_entry((int)gdk_keyval_from_name(ki->key), state, 0, XEN_UNDEFINED, ki->x, name, name);
#endif
	}
      FREE(ki);
    }
}


/* -------- key: play all chans from cursor -------- */

static void help_play_from_cursor(prefs_info *prf)
{
  snd_help("play from cursor",
	   "By default, C-q plays the current channel from the cursor, but one often wants to play the entire \
sound; this option binds a key for that purpose, and also overrides the pause setting.  The new binding does \
not take effect until you type return in the text widget.",
	   WITH_WORD_WRAP);
}

static char *make_pfc_binding(char *key, bool ctrl, bool meta, bool cx)
{
#if HAVE_SCHEME
  return(mus_format("(bind-key %s %d (lambda () (set! (pausing) #f) (play (cursor))) %s \"play sound from cursor\" \"play-from-cursor\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif
#if HAVE_RUBY
  return(mus_format("bind_key(%s, %d, lambda do\n  set_pausing(false)\n  play(cursor())\n  end, %s, \"play sound from cursor\", \"play-from-cursor\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "true" : "false"));
#endif
#if HAVE_FORTH
  return(mus_format("%s %d lambda: <{ }> #f set-pausing drop  #f #f #f cursor play drop ; %s \"play sound from cursor\" \"play-from-cursor\" bind-key drop\n",
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif
  return(NULL);
}

static void reflect_play_from_cursor(prefs_info *prf)
{
  reflect_key(prf, "play-from-cursor");
}

static void save_pfc_binding(prefs_info *prf, FILE *fd)
{
  save_key_binding(prf, fd, make_pfc_binding);
}

static void bind_play_from_cursor(prefs_info *prf)
{
  key_bind(prf, make_pfc_binding);
}

static void clear_play_from_cursor(prefs_info *prf) {clear_key(prf, "play-from-cursor");}


/* -------- key: show all of sound -------- */

static void help_show_all(prefs_info *prf)
{
  snd_help("show entire sound",
	   "This option binds a key to show all of the current sound in the current time domain window, \
equivalent to moving the 'zoom' slider all the way to the right.",
	   WITH_WORD_WRAP);
}

static char *make_show_all_binding(char *key, bool ctrl, bool meta, bool cx)
{
#if HAVE_SCHEME
  return(mus_format("(bind-key %s %d (lambda () \
                                       (let ((old-sync (sync))) \
                                         (set! (sync) (1+ (max-sync))) \
                                         (set! (x-bounds) (list 0.0 (/ (frames) (srate)))) \
                                         (set! (sync) old-sync))) %s \"show entire sound\" \"show-all\")\n",
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif
#if HAVE_RUBY
  return(mus_format("bind_key(%s, %d, lambda do\n\
                                        old_sync = sync()\n\
                                        sync(1 + sync_max())\n\
                                        set_x_bounds([0.0, frames() / srate()])\n\
                                        set_sync(old_sync)\n\
                                        end, %s, \"show entire sound\", \"show-all\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "true" : "false"));
#endif
#if HAVE_FORTH
  return(mus_format("%s %d lambda: <{ }> #f sync sync-max 1+ #f set-sync drop '( 0.0 #f #f #f frames #f srate f/ ) #f #f set-x-bounds drop #f set-sync ; %s \"show entire sound\" \"show-all\" bind-key drop\n",
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif
  return(NULL);
}

static void reflect_show_all(prefs_info *prf)
{
  reflect_key(prf, "show-all");
}

static void save_show_all_binding(prefs_info *prf, FILE *fd)
{
  save_key_binding(prf, fd, make_show_all_binding);
}

static void bind_show_all(prefs_info *prf)
{
  key_bind(prf, make_show_all_binding);
}

static void clear_show_all(prefs_info *prf) {clear_key(prf, "show-all");}


/* -------- key: select all of sound -------- */

static void help_select_all(prefs_info *prf)
{
  snd_help("select entire sound",
	   "This option binds a key to select all of the current sound.  The 'Select all' Edit menu item \
follows the 'sync' buttons when deciding which channels to select.",
	   WITH_WORD_WRAP);
}

static char *make_select_all_binding(char *key, bool ctrl, bool meta, bool cx)
{
#if HAVE_SCHEME
  return(mus_format("(bind-key %s %d (lambda () \
                                       (let ((old-sync (sync))) \
                                         (set! (sync) (1+ (max-sync))) \
                                         (select-all) \
                                         (set! (sync) old-sync))) %s \"select entire sound\" \"select-all\")\n",
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif
#if HAVE_RUBY
  return(mus_format("bind_key(%s, %d, lambda do\n\
                                        old_sync = sync()\n\
                                        sync(1 + sync_max())\n\
                                        select_all()\n\
                                        set_sync(old_sync)\n\
                                        end, %s, \"select entire sound\", \"select-all\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "true" : "false"));
#endif
#if HAVE_FORTH
  return(mus_format("%s %d lambda: <{ }> #f sync sync-max 1+ #f set-sync drop #f #f select-all drop #f set-sync ; %s \"select entire sound\" \"select-all\" bind-key drop\n",
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif
  return(NULL);
}

static void reflect_select_all(prefs_info *prf)
{
  reflect_key(prf, "select-all");
}

static void save_select_all_binding(prefs_info *prf, FILE *fd)
{
  save_key_binding(prf, fd, make_select_all_binding);
}

static void bind_select_all(prefs_info *prf)
{
  key_bind(prf, make_select_all_binding);
}

static void clear_select_all(prefs_info *prf) {clear_key(prf, "select-all");}


/* -------- key: undo all edits -------- */

static void help_revert(prefs_info *prf)
{
  snd_help("undo all edits (revert)",
	   "This option binds a key to undo any edits in the current sound, equivalent to the File:Revert menu item.",
	   WITH_WORD_WRAP);
}

static char *make_revert_binding(char *key, bool ctrl, bool meta, bool cx)
{
#if HAVE_SCHEME
  return(mus_format("(bind-key %s %d revert-sound %s \"undo all edits\" \"revert-sound\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif
#if HAVE_RUBY
  return(mus_format("bind_key(%s, %d, lambda do\n  revert_sound())\n  end, %s, \"undo all edits\", \"revert-sound\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "true" : "false"));
#endif
#if HAVE_FORTH
  return(mus_format("%s %d lambda: <{ }> #f revert-sound ; %s \"undo all edits\" \"revert-sound\" bind-key drop\n",
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif
  return(NULL);
}

static void reflect_revert(prefs_info *prf)
{
  reflect_key(prf, "revert-sound");
}

static void save_revert_binding(prefs_info *prf, FILE *fd)
{
  save_key_binding(prf, fd, make_revert_binding);
}

static void bind_revert(prefs_info *prf)
{
  key_bind(prf, make_revert_binding);
}

static void clear_revert_sound(prefs_info *prf) {clear_key(prf, "revert-sound");}


/* -------- key: exit -------- */

static void help_exit(prefs_info *prf)
{
  snd_help("exit from Snd",
	   "This option binds a key to exit from Snd, equivalent to the File:Exit menu item.",
	   WITH_WORD_WRAP);
}

static char *make_exit_binding(char *key, bool ctrl, bool meta, bool cx)
{
#if HAVE_SCHEME
  return(mus_format("(bind-key %s %d exit %s \"exit\" \"exit\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif
#if HAVE_RUBY
  return(mus_format("bind_key(%s, %d, lambda do\n  exit())\n  end, %s, \"exit\", \"exit\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "true" : "false"));
#endif
#if HAVE_FORTH
  return(mus_format("%s %d lambda: <{ }> 0 snd-exit ; %s \"exit\" \"exit\" bind-key drop\n",
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif
  return(NULL);
}

static void reflect_exit(prefs_info *prf)
{
  reflect_key(prf, "exit");
}

static void save_exit_binding(prefs_info *prf, FILE *fd)
{
  save_key_binding(prf, fd, make_exit_binding);
}

static void bind_exit(prefs_info *prf)
{
  key_bind(prf, make_exit_binding);
}

static void clear_exit(prefs_info *prf) {clear_key(prf, "exit");}


/* -------- key: goto maxamp -------- */

static void help_goto_maxamp(prefs_info *prf)
{
  snd_help("go to maxamp",
	   "This option binds a key to move the view (and cursor) to the position of the current channel's maximum sample.",
	   WITH_WORD_WRAP);
}

static char *make_goto_maxamp_binding(char *key, bool ctrl, bool meta, bool cx)
{
#if HAVE_SCHEME
  return(mus_format("(bind-key %s %d (lambda () (set! (cursor) (maxamp-position))) %s \"goto maxamp\" \"goto-maxamp\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif
#if HAVE_RUBY
  return(mus_format("bind_key(%s, %d, lambda do\n  set_cursor(maxamp_position())\n  end, %s, \"goto maxamp\", \"goto-maxamp\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "true" : "false"));
#endif
#if HAVE_FORTH
  return(mus_format("%s %d lambda: <{ }> #f #f #f maxamp-position #f #f #f set-cursor ; %s \"goto maxamp\" \"goto-maxamp\" bind-key drop\n",
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif
  return(NULL);
}

static void reflect_goto_maxamp(prefs_info *prf)
{
  reflect_key(prf, "goto-maxamp");
}

static void save_goto_maxamp_binding(prefs_info *prf, FILE *fd)
{
  save_key_binding(prf, fd, make_goto_maxamp_binding);
}

static void bind_goto_maxamp(prefs_info *prf)
{
  key_bind(prf, make_goto_maxamp_binding);
}

static void clear_goto_maxamp(prefs_info *prf) {clear_key(prf, "goto-maxamp");}


/* -------- key: show selection -------- */

static void help_show_selection(prefs_info *prf)
{
  snd_help("show selection",
	   "This option binds a key to cause the current selection to fill the time domain graph.",
	   WITH_WORD_WRAP);
}

static char *make_show_selection_binding(char *key, bool ctrl, bool meta, bool cx)
{
#if HAVE_SCHEME
  return(mus_format("(if (not (provided? 'snd-extensions.scm)) (load-from-path \"extensions.scm\"))\n\(bind-key %s %d show-selection %s \"show selection\" \"show-selection\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif
#if HAVE_RUBY
  return(mus_format("require \"extensions\"\nbind_key(%s, %d, lambda do\n  show_selection())\n  end, %s, \"show selection\", \"show-selection\")\n", 
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "true" : "false"));
#endif
#if HAVE_FORTH
  return(mus_format("require extensions\n%s %d ' show-selection %s \"show selection\" \"show-selection\" bind-key drop\n",
		    possibly_quote(key), 
		    ((ctrl) ? 4 : 0) + ((meta) ? 8 : 0),
		    (cx) ? "#t" : "#f"));
#endif
  return(NULL);
}

static void reflect_show_selection(prefs_info *prf)
{
  reflect_key(prf, "show-selection");
}

static void save_show_selection_binding(prefs_info *prf, FILE *fd)
{
  save_key_binding(prf, fd, make_show_selection_binding);
}

static void bind_show_selection(prefs_info *prf)
{
  key_bind(prf, make_show_selection_binding);
}

static void clear_show_selection(prefs_info *prf) {clear_key(prf, "show-selection");}
