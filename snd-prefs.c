/* this file included as text in snd-g|xprefs.c */

/* TODO: ruby var get/set probably isn't working */

#if HAVE_SCHEME
  #define LANG_NAME "scheme"
#endif
#if HAVE_RUBY
  #define LANG_NAME "ruby"
#endif
#if HAVE_FORTH
  #define LANG_NAME "forth"
#endif
#ifndef LANG_NAME
  #define LANG_NAME "source"
#endif


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

static void save_prefs(const char *filename, char *load_path_name)
{
  int i;
  char *fullname;
  FILE *fd;
  if (!filename) return; /* error earlier */
  fullname = mus_expand_filename(filename);
  fd = FOPEN(fullname, "a");
  if (fd)
    {
      if (load_path_name)
	{
#if HAVE_GUILE
	  fprintf(fd, "(set! %%load-path (cons \"%s\" %%load-path))\n", load_path_name);
#endif
#if HAVE_RUBY
	  fprintf(fd, "$:.push(\"%s\")\n", load_path_name);
#endif
#if HAVE_FORTH
	  fprintf(fd, "$\" %s\" add-load-path\n", load_path_name);
#endif
#if HAVE_GAUCHE
	  fprintf(fd, "(add-load-path \"%s\")\n", load_path_name); /* variable is *load-path* */
#endif
	}
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
  fprintf(fd, "(if (not (provided? 'snd-%s.scm)) (load-from-path \"%s.scm\"))\n", file, file);
  fprintf(fd, "(set! %s %s)\n", name, XEN_AS_STRING(val));
#endif
#if HAVE_RUBY
  char *str;
  str = no_stars(name);
  fprintf(fd, "require \"%s\"\n", file);
  fprintf(fd, "set_%s(%s)\n", str, XEN_AS_STRING(val));
  FREE(str);
#endif
#if HAVE_FORTH
  fprintf(fd, "require %s\n", file);
  fprintf(fd, "%s set-%s\n", XEN_AS_STRING(val), name);
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
static void clear_basic_color(prefs_info *prf) {set_basic_color(ss->sgx->orig_basic_color);}
static void save_basic_color(prefs_info *prf, FILE *ignore) {saved_basic_color = ss->sgx->basic_color;}
static void basic_color_func(prefs_info *prf, float r, float g, float b) {set_basic_color(rgb_to_color(r, g, b));}
static void revert_basic_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_basic_color); 
  set_basic_color(saved_basic_color);
}


/* ---------------- highlight-color ---------------- */

static color_t saved_highlight_color;
static void clear_highlight_color(prefs_info *prf) {set_highlight_color(ss->sgx->orig_highlight_color);}
static void save_highlight_color(prefs_info *prf, FILE *ignore) {saved_highlight_color = ss->sgx->highlight_color;}
static void highlight_color_func(prefs_info *prf, float r, float g, float b) {set_highlight_color(rgb_to_color(r, g, b));}
static void revert_highlight_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_highlight_color); 
  set_highlight_color(saved_highlight_color);
}


/* ---------------- position-color ---------------- */

static color_t saved_position_color;
static void clear_position_color(prefs_info *prf) {set_position_color(ss->sgx->orig_position_color);}
static void save_position_color(prefs_info *prf, FILE *ignore) {saved_position_color = ss->sgx->position_color;}
static void position_color_func(prefs_info *prf, float r, float g, float b) {set_position_color(rgb_to_color(r, g, b));}
static void revert_position_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_position_color); 
  set_position_color(saved_position_color);
}


/* ---------------- zoom-color ---------------- */

static color_t saved_zoom_color;
static void clear_zoom_color(prefs_info *prf) {set_zoom_color(ss->sgx->orig_zoom_color);}
static void save_zoom_color(prefs_info *prf, FILE *ignore) {saved_zoom_color = ss->sgx->zoom_color;}
static void zoom_color_func(prefs_info *prf, float r, float g, float b) {set_zoom_color(rgb_to_color(r, g, b));}
static void revert_zoom_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_zoom_color); 
  set_zoom_color(saved_zoom_color);
}


/* ---------------- cursor-color ---------------- */

static color_t saved_cursor_color;
static void clear_cursor_color(prefs_info *prf) {color_cursor(ss->sgx->orig_cursor_color);}
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


/* ---------------- data-color ---------------- */

static color_t saved_data_color;
static void clear_data_color(prefs_info *prf) {set_data_color(ss->sgx->orig_data_color);}
static void save_data_color(prefs_info *prf, FILE *ignore) {saved_data_color = ss->sgx->data_color;}
static void data_color_func(prefs_info *prf, float r, float g, float b) {set_data_color(rgb_to_color(r, g, b));}
static void revert_data_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_data_color); 
  set_data_color(saved_data_color);
}


/* ---------------- graph-color ---------------- */

static color_t saved_graph_color;
static void clear_graph_color(prefs_info *prf) {set_graph_color(ss->sgx->orig_graph_color);}
static void save_graph_color(prefs_info *prf, FILE *ignore) {saved_graph_color = ss->sgx->graph_color;}
static void graph_color_func(prefs_info *prf, float r, float g, float b) {set_graph_color(rgb_to_color(r, g, b));}
static void revert_graph_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_graph_color); 
  set_graph_color(saved_graph_color);
}


/* ---------------- selected-data-color ---------------- */

static color_t saved_selected_data_color;
static void clear_selected_data_color(prefs_info *prf) {set_selected_data_color(ss->sgx->orig_selected_data_color);}
static void save_selected_data_color(prefs_info *prf, FILE *ignore) {saved_selected_data_color = ss->sgx->selected_data_color;}
static void selected_data_color_func(prefs_info *prf, float r, float g, float b) {set_selected_data_color(rgb_to_color(r, g, b));}
static void revert_selected_data_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_selected_data_color); 
  set_selected_data_color(saved_selected_data_color);
}


/* ---------------- selected-graph-color ---------------- */

static color_t saved_selected_graph_color;
static void clear_selected_graph_color(prefs_info *prf) {set_selected_graph_color(ss->sgx->orig_selected_graph_color);}
static void save_selected_graph_color(prefs_info *prf, FILE *ignore) {saved_selected_graph_color = ss->sgx->selected_graph_color;}
static void selected_graph_color_func(prefs_info *prf, float r, float g, float b) {set_selected_graph_color(rgb_to_color(r, g, b));}
static void revert_selected_graph_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_selected_graph_color); 
  set_selected_graph_color(saved_selected_graph_color);
}


/* ---------------- selection-color ---------------- */

static void set_selection_color(color_t color)
{
  color_selection(color);
  for_each_chan(update_graph);
}

static color_t saved_selection_color;
static void clear_selection_color(prefs_info *prf) {set_selection_color(ss->sgx->orig_selection_color);}
static void save_selection_color(prefs_info *prf, FILE *ignore) {saved_selection_color = ss->sgx->selection_color;}
static void selection_color_func(prefs_info *prf, float r, float g, float b) {set_selection_color(rgb_to_color(r, g, b));}
static void revert_selection_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_selection_color); 
  set_selection_color(saved_selection_color);
}


/* ---------------- mark-color ---------------- */

static color_t saved_mark_color;
static void clear_mark_color(prefs_info *prf) {color_marks(ss->sgx->orig_mark_color);}
static void save_mark_color(prefs_info *prf, FILE *ignore) {saved_mark_color = ss->sgx->mark_color;}
static void mark_color_func(prefs_info *prf, float r, float g, float b) {color_marks(rgb_to_color(r, g, b));}
static void revert_mark_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_mark_color);
  color_marks(saved_mark_color);
}


/* ---------------- mix-color (waveform) ---------------- */

static color_t saved_mix_color;
static void clear_mix_color(prefs_info *prf) {color_mixes(ss->sgx->orig_mix_color);}
static void save_mix_color(prefs_info *prf, FILE *ignore) {saved_mix_color = ss->sgx->mix_color;}
static void mix_color_func(prefs_info *prf, float r, float g, float b) {color_mixes(rgb_to_color(r, g, b));}
static void revert_mix_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_mix_color);
  color_mixes(saved_mix_color);
}


/* ---------------- listener-color ---------------- */

static color_t saved_listener_color;
static void clear_listener_color(prefs_info *prf) {color_listener(ss->sgx->orig_listener_color);}
static void save_listener_color(prefs_info *prf, FILE *ignore) {saved_listener_color = ss->sgx->listener_color;}
static void listener_color_func(prefs_info *prf, float r, float g, float b) {color_listener(rgb_to_color(r, g, b));}
static void revert_listener_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_listener_color);
  color_listener(saved_listener_color);
}


/* ---------------- listener-text-color ---------------- */

static color_t saved_listener_text_color;
static void clear_listener_text_color(prefs_info *prf) {color_listener_text(ss->sgx->orig_listener_text_color);}
static void save_listener_text_color(prefs_info *prf, FILE *ignore) {saved_listener_text_color = ss->sgx->listener_text_color;}
static void listener_text_color_func(prefs_info *prf, float r, float g, float b) {color_listener_text(rgb_to_color(r, g, b));}
static void revert_listener_text_color(prefs_info *prf) 
{
  scale_set_color(prf, saved_listener_text_color);
  color_listener_text(saved_listener_text_color);
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
  if (str) FREE_TEXT(str);
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
  if (str) FREE_TEXT(str);
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
  if (str) FREE_TEXT(str);
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
  if (str) FREE_TEXT(str);
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
  if (str) FREE_TEXT(str);
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
  if (str) FREE_TEXT(str);
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
  if (str) FREE_TEXT(str);
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
      fprintf(fd, "$\" %s\" %s drop\n", rts_vf_directory, S_add_directory_to_view_files_list);
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
      sscanf(str, "%d", &value);
      if (value >= 0)
	in_set_max_transform_peaks(value);
      else int_to_textfield(prf->text, max_transform_peaks(ss));
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
      sscanf(str, "%d", &value);
      if (value >= 0)
	in_set_mix_waveform_height(value);
      else int_to_textfield(prf->text, mix_waveform_height(ss));
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
      sscanf(str, "%d", &value);
      if (value >= 0)
	in_set_max_regions(value);
      else int_to_textfield(prf->text, max_regions(ss));
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
      sscanf(str, "%d", &value);
      if (value >= 0)
	set_sinc_width(value);
      else int_to_textfield(prf->text, sinc_width(ss));
      FREE_TEXT(str);
    }
}


/* ---------------- print-length ---------------- */

static int rts_print_length = DEFAULT_PRINT_LENGTH;
static void reflect_print_length(prefs_info *prf) {int_to_textfield(prf->text, print_length(ss));}
static void revert_print_length(prefs_info *prf) {set_print_length(rts_print_length); set_vct_print_length(rts_print_length);}
static void save_print_length(prefs_info *prf, FILE *ignore) {rts_print_length = print_length(ss);}

static void print_length_text(prefs_info *prf)
{
  char *str;
  str = GET_TEXT(prf->text);
  if ((str) && (*str))
    {
      int value = 0;
      sscanf(str, "%d", &value);
      set_print_length(value);
      set_vct_print_length(value);
      /* the clm array print length variable will be taken care of when ww.scm is loaded in the new context */
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
      sscanf(str, "%d", &value);
      if (value > 0)
	set_dac_size(value);
      else int_to_textfield(prf->text, dac_size(ss));
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
      sscanf(str, "%d", &value);
      if (value > 0)
	rec_set_buffer_size(value);
      else int_to_textfield(prf->text, rec_buffer_size());
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
      sscanf(str, "%f", &value);
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
      sscanf(str, "%f", &value);
      if ((value >= 0.0) &&
	  (value <= prf->scale_max))
	{
	  in_set_fft_window_beta(value);
	  SET_SCALE(value / prf->scale_max);
	}
      else SET_TEXT(prf->text, "must be >= 0.0");
      FREE_TEXT(str);
    }
}


/* ---------------- sync choice ---------------- */

#define SYNC_WITHIN_EACH_SOUND 2
#define SYNC_ACROSS_ALL_SOUNDS 1
#define SYNC_DISABLED 0
#define SYNC_UNSET -1

static int global_sync_choice = SYNC_UNSET, rts_sync_choice = 0;

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

static void sync_choice_help(prefs_info *prf)
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
  global_sync_choice = sync_choice();
  SET_TOGGLE(prf->toggle, global_sync_choice == SYNC_WITHIN_EACH_SOUND);
  SET_TOGGLE(prf->toggle2, global_sync_choice == SYNC_ACROSS_ALL_SOUNDS);
}

static void save_sync_choice(prefs_info *prf, FILE *fd)
{
  if (global_sync_choice != SYNC_UNSET) 
    {
      rts_sync_choice = global_sync_choice;
      if (global_sync_choice != SYNC_DISABLED)
	prefs_variable_save(fd, "global-sync-choice", "extensions", C_TO_XEN_INT(global_sync_choice));
    }
}

static void sync1_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->toggle))
    global_sync_choice = SYNC_WITHIN_EACH_SOUND;
  else global_sync_choice = SYNC_DISABLED;
  SET_TOGGLE(prf->toggle2, false);
  /* if user has not loaded extensions, but sets one of the toggle buttons, load extensions and
   *    set the global-sync-choice variable
   */
  set_sync_choice(global_sync_choice, "extensions"); 
}

static void sync2_choice(prefs_info *prf)
{
  if (GET_TOGGLE(prf->toggle2))
    global_sync_choice = SYNC_ACROSS_ALL_SOUNDS;
  else global_sync_choice = SYNC_DISABLED;
  SET_TOGGLE(prf->toggle, false);
  set_sync_choice(global_sync_choice, "extensions");
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





/* ---------------- various extra help strings ---------------- */

static void load_path_help(prefs_info *prf)
{
  snd_help("load paths",
	   "Much of Snd's functionality is loaded as needed from the Scheme or Ruby \
files found in the Snd tarball.  You can run Snd without \
these files, but there's no reason to!  Just add the directory containing \
them to the \"load-path\".  For example, if the Snd build directory was \"/home/bil/snd\", \
add that string to the load paths given here.  Guile and Ruby search these \
directories for any *.scm or *.rb files that they can't \
find elsewhere.",
	   WITH_WORD_WRAP);
}

static void unsaved_edits_help(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option looks for unsaved edits when you close a file, or exit Snd.  If it \
finds any, it asks you whether you want to save them.",
	   WITH_WORD_WRAP);
}

static void current_window_help(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option displays a small graph of the entire sound in the upper right corner \
of the screen with an indication of where the current window is. If you click somewhere in the \
little graph, the cursor and main window are moved to that spot.",
	   WITH_WORD_WRAP);
}

static void mouse_focus_help(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option implements 'pointer focus' in Snd; that is, the widget under the mouse \
is the active widget. In this mode, you don't need to click a graph or text widget before \
taking some action in it.",
	   WITH_WORD_WRAP);
}

static void remember_sound_state_choice_help(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option causes Snd to save most of a sound's display state when it is closed, \
and if that same sound is later re-opened, Snd restores the previous state. Not fully implemented yet.",
	   WITH_WORD_WRAP);
}

static void peak_env_help(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "When a very large file is first opened, Snd scans all the data to build up an overall \
representation of the sound.  If you like to view the entire sound upon opening it, you can speed \
up the process a lot by saving this initial representation.  The data is called a 'peak-env' file \
and it resides in the 'peak-env-directory'.",
	   WITH_WORD_WRAP);
}

static void context_sensitive_popup_help(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option creates a context-sensitive popup menu (activated by button 3).  The menu \
displayed depends on where the mouse is at the time; there are special menus for the waveform and fft \
portions of the graphs, the listener, the edit history pane, and the current selection.",
	   WITH_WORD_WRAP);
}

static void effects_menu_help(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option creates a top-level menu named 'Effects'.  The effects include such things as \
reverberation, reversal, normalizations, gains and envelopes, inversion, echos, flanging, companding, \
filtering, padding, cross synthesis, and so on.",
	   WITH_WORD_WRAP);
}

#if HAVE_SCHEME
static void edit_menu_help(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option adds several options to the top-level Edit menu: selection to file, \
append selection, mono to stereo, trim, crop, etc.",
	   WITH_WORD_WRAP);
}

static void marks_menu_help(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option adds a top-level 'Marks' menu that includes such things as play between marks, \
trim, crop, define selection via marks, etc",
	   WITH_WORD_WRAP);
}

static void mix_menu_help(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option adds a top-level 'Mix/Track' menu.  Included are such choices as delete mix, \
snap mix to beat, delete, play, reverse, save, and transpose track, set track amplitude, speed, and tempo, etc.",
	   WITH_WORD_WRAP);
}

#if USE_MOTIF
static void icon_box_help(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option adds a top-level box full of various handy icons.  Not implemented in Gtk yet.",
	   WITH_WORD_WRAP);
}
#endif
#endif

static void reopen_menu_help(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option adds a top-level 'Reopen' menu. Previously opened sounds that are not currently open are listed \
as menu items.",
	   WITH_WORD_WRAP);
}

static void initial_bounds_help(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "Normally Snd displays just the first 0.1 seconds of a sound in its initial graph. This option \
sets either new bounds for that display, or directs Snd to display the entire sound.",
	   WITH_WORD_WRAP);
}

static void with_sound_help(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "with-sound is the main CLM sound-producing macro.  If you want to use CLM functions to create \
new sounds, then edit them in Snd, include with-sound.",
	   WITH_WORD_WRAP);
}

#if HAVE_SCHEME
static void hidden_controls_help(prefs_info *prf)
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
#endif

static void clm_file_name_help(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option sets the default output file name used by with-sound.",
	   WITH_WORD_WRAP);
}

static void clm_table_size_help(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option sets the default clm table size and file buffer size.",
	   WITH_WORD_WRAP);
}

static void smpte_label_help(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option adds a label to the time domain graph showing the current SMPTE frame of the leftmost sample.",
	   WITH_WORD_WRAP);
}

#if USE_MOTIF
static void mark_pane_help(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "This option adds a pane to each channel window containing information about that channel's marks.",
	   WITH_WORD_WRAP);
}
#endif

static void play_from_cursor_help(prefs_info *prf)
{
  snd_help("play from cursor",
	   "By default, C-q plays the current channel from the cursor, but one often wants to play the entire \
sound; this option binds a key for that purpose, and also overrides the pause setting.  The new binding does \
not take effect until you type return in the text widget.",
	   WITH_WORD_WRAP);
}

static void show_all_help(prefs_info *prf)
{
  snd_help("show entire sound",
	   "This option binds a key to show all of the current sound in the current time domain window, \
equivalent to moving the 'zoom' slider all the way to the right.",
	   WITH_WORD_WRAP);
}

static void select_all_help(prefs_info *prf)
{
  snd_help("select entire sound",
	   "This option binds a key to select all of the current sound.  The 'Select all' Edit menu item \
follows the 'sync' buttons when deciding which channels to select.",
	   WITH_WORD_WRAP);
}

static void revert_help(prefs_info *prf)
{
  snd_help("undo all edits (revert)",
	   "This option binds a key to undo any edits in the current sound, equivalent to the File:Revert menu item.",
	   WITH_WORD_WRAP);
}

static void exit_help(prefs_info *prf)
{
  snd_help("exit from Snd",
	   "This option binds a key to exit from Snd, equivalent to the File:Exit menu item.",
	   WITH_WORD_WRAP);
}

static void goto_maxamp_help(prefs_info *prf)
{
  snd_help("go to maxamp",
	   "This option binds a key to move the view (and cursor) to the position of the current channel's maximum sample.",
	   WITH_WORD_WRAP);
}

static void show_selection_help(prefs_info *prf)
{
  snd_help("show selection",
	   "This option binds a key to cause the current selection to fill the time domain graph.",
	   WITH_WORD_WRAP);
}





/* ---------------- save functions ---------------- */

static void save_unsaved_edits_1(prefs_info *prf, FILE *fd)
{
#if HAVE_SCHEME
  fprintf(fd, "(if (not (provided? 'snd-extensions.scm)) (load-from-path \"extensions.scm\"))\n");
  fprintf(fd, "(check-for-unsaved-edits #t)\n");
#endif
#if HAVE_RUBY
  fprintf(fd, "require \"extensions\"\n");
  fprintf(fd, "check_for_unsaved_edits(true)\n");
#endif
#if HAVE_FORTH
  fprintf(fd, "require extensions\n");
  fprintf(fd, "#t check-for-unsaved-edits\n");
#endif
}

static void save_current_window_display_1(prefs_info *prf, FILE *fd)
{
#if HAVE_SCHEME
  fprintf(fd, "(if (not (provided? 'snd-draw.scm)) (load-from-path \"draw.scm\"))\n");
  fprintf(fd, "(make-current-window-display)\n");
#endif
#if HAVE_RUBY
  fprintf(fd, "require \"draw\"\n");
  fprintf(fd, "make_current_window_display\n");
#endif
#if HAVE_FORTH
  fprintf(fd, "require draw\n");
  fprintf(fd, "make-current-window-display\n");
#endif
}

static void save_focus_follows_mouse_1(prefs_info *prf, FILE *fd) 
{
#if HAVE_SCHEME
  fprintf(fd, "(if (not (provided? 'snd-extensions.scm)) (load-from-path \"extensions.scm\"))\n");
  fprintf(fd, "(focus-follows-mouse)\n");
#endif
#if HAVE_RUBY
  fprintf(fd, "require \"extensions\"\n");
  fprintf(fd, "focus_follows_mouse\n");
#endif
#if HAVE_FORTH
  fprintf(fd, "require extensions\n");
  fprintf(fd, "focus-follows-mouse\n");
#endif
}

static void save_remember_sound_state_choice_1(prefs_info *prf, FILE *fd, int choice)
{
#if HAVE_SCHEME
  fprintf(fd, "(if (not (provided? 'snd-extensions.scm)) (load-from-path \"extensions.scm\"))\n");
  fprintf(fd, "(remember-sound-state %d)\n", choice);
#endif
#if HAVE_RUBY
  fprintf(fd, "require \"extensions\"\n");
  if (choice & 2)
    fprintf(fd, "remember_all_sound_properties\n");
  else fprintf(fd, "remember_sound_state\n");
#endif
#if HAVE_FORTH
  fprintf(fd, "require extensions\n");
  fprintf(fd, "%d remember-sound-state\n", choice);
#endif
}

static void save_peak_envs_1(prefs_info *prf, FILE *fd, char *pdir)
{
#if HAVE_SCHEME
  fprintf(fd, "(if (not (provided? 'snd-peak-env.scm)) (load-from-path \"peak-env.scm\"))\n");
  if (pdir)
    fprintf(fd, "(set! save-peak-env-info-directory \"%s\")\n", pdir);
#endif
#if HAVE_RUBY
  fprintf(fd, "require \"env\"\n");
  if (pdir)
    fprintf(fd, "$save_peak_env_info_directory = \"%s\"\n", pdir);
#endif
#if HAVE_FORTH
  fprintf(fd, "require peak-env\n");
  if (pdir)
    fprintf(fd, "$\" %s\" to save-peak-env-info-directory\n", pdir);
#endif
}

static void save_context_sensitive_popup_1(prefs_info *prf, FILE *fd)
{
#if HAVE_SCHEME
  fprintf(fd, "(if (provided? 'snd-motif)\n    (if (not (provided? 'snd-popup.scm))\n        (load-from-path \"popup.scm\"))\n    (if (not (provided? 'snd-gtk-popup.scm))\n	(load-from-path \"gtk-popup.scm\")))\n");
#endif
#if HAVE_RUBY
  fprintf(fd, "require \"popup\"\n");
#endif
#if HAVE_FORTY
  fprintf(fd, "require popup\n");
#endif
}

static void save_effects_menu_1(prefs_info *prf, FILE *fd)
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

static void save_reopen_menu_1(prefs_info *prf, FILE *fd)
{
#if HAVE_SCHEME
  fprintf(fd, "(if (provided? 'snd-extensions) (load-from-path \"extensions.scm\"))\n");
  fprintf(fd, "(with-reopen-menu)\n");
#endif
#if HAVE_RUBY
  fprintf(fd, "require \"extensions\"\n");
  fprintf(fd, "with_reopen_menu\n");
#endif
#if HAVE_FORTH
  fprintf(fd, "require extensions\n");
  fprintf(fd, "with-reopen-menu\n");
#endif
}

static bool use_full_duration(void)
{
  return((XEN_DEFINED_P("prefs-show-full-duration")) &&
	 (XEN_TRUE_P(XEN_NAME_AS_C_STRING_TO_VALUE("prefs-show-full-duration"))));
}

static char *initial_bounds_to_string(void)
{
  if (XEN_DEFINED_P("prefs-initial-beg"))
    return(mus_format("%.2f : %.2f", 
		      XEN_TO_C_DOUBLE(XEN_NAME_AS_C_STRING_TO_VALUE("prefs-initial-beg")),
		      XEN_TO_C_DOUBLE(XEN_NAME_AS_C_STRING_TO_VALUE("prefs-initial-dur"))));
  return(copy_string("0.0 : 0.1"));
}

static void save_initial_bounds(prefs_info *prf, FILE *fd)
{
  if ((use_full_duration()) ||
      ((XEN_DEFINED_P("prefs-initial-beg")) &&
       ((!(snd_feq(XEN_TO_C_DOUBLE(XEN_NAME_AS_C_STRING_TO_VALUE("prefs-initial-beg")), 0.0))) ||
	(!(snd_feq(XEN_TO_C_DOUBLE(XEN_NAME_AS_C_STRING_TO_VALUE("prefs-initial-dur")), 0.1))))))
    {
#if HAVE_SCHEME
      fprintf(fd, "(if (not (provided? 'snd-extensions.scm)) (load-from-path \"extensions.scm\"))\n");
      fprintf(fd, "(prefs-activate-initial-bounds %.2f %.2f %s)\n",
	      XEN_TO_C_DOUBLE(XEN_NAME_AS_C_STRING_TO_VALUE("prefs-initial-beg")),
	      XEN_TO_C_DOUBLE(XEN_NAME_AS_C_STRING_TO_VALUE("prefs-initial-dur")),
	      (XEN_TRUE_P(XEN_NAME_AS_C_STRING_TO_VALUE("prefs-show-full-duration"))) ? PROC_TRUE : PROC_FALSE);
#endif
#if HAVE_RUBY
      fprintf(fd, "require \"extensions\"\n");
      fprintf(fd, "prefs_activate_initial_bounds(%.2f, %.2f, %s)\n",
  	      XEN_TO_C_DOUBLE(XEN_NAME_AS_C_STRING_TO_VALUE("prefs-initial-beg")),
  	      XEN_TO_C_DOUBLE(XEN_NAME_AS_C_STRING_TO_VALUE("prefs-initial-dur")),
 	      (XEN_TRUE_P(XEN_NAME_AS_C_STRING_TO_VALUE("prefs-show-full-duration"))) ? PROC_TRUE : PROC_FALSE);
#endif
#if HAVE_FORTH
      fprintf(fd, "require extensions\n");
      fprintf(fd, "%.2f %.2f %s prefs-activate-initial-bounds\n",
  	      XEN_TO_C_DOUBLE(XEN_NAME_AS_C_STRING_TO_VALUE("prefs-initial-beg")),
  	      XEN_TO_C_DOUBLE(XEN_NAME_AS_C_STRING_TO_VALUE("prefs-initial-dur")),
 	      (XEN_TRUE_P(XEN_NAME_AS_C_STRING_TO_VALUE("prefs-show-full-duration"))) ? PROC_TRUE : PROC_FALSE);
#endif
      /* code repeated to make emacs' paren matcher happy */
    }
}

static void save_smpte_1(prefs_info *prf, FILE *fd)
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
  fprintf(fd, "#t show-smpte-label\n");
#endif
}

#if USE_MOTIF
static void save_mark_pane_1(prefs_info *prf, FILE *fd)
{
#if HAVE_SCHEME
  fprintf(fd, "(if (provided? 'snd-motif)\n    (if (not (provided? 'snd-snd-motif.scm))\n        (load-from-path \"snd-motif.scm\"))\n    (if (not (provided? 'snd-snd-gtk.scm))\n        (load-from-path \"snd-gtk.scm\")))\n");
  fprintf(fd, "(add-mark-pane)\n");
#endif
#if HAVE_RUBY
  fprintf(fd, "require \"snd-xm\"\n");
  fprintf(fd, "add_mark_pane\n");
#endif
#if HAVE_FORTH
  fprintf(fd, "require snd-xm\n");
  fprintf(fd, "add-mark-pane\n");
#endif
}
#endif

static void save_show_listener_1(prefs_info *prf, FILE *fd)
{
#if HAVE_SCHEME
  /* show-listener is saved in save-state, but not save-options */
  fprintf(fd, "(show-listener)\n");
#endif
#if HAVE_RUBY
  fprintf(fd, "show_listener\n");
#endif
#if HAVE_FORTH
  fprintf(fd, "show-listener drop\n");
#endif
}


/* -------- key: play all chans from cursor -------- */

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

/* -------- key: show all of sound -------- */

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

/* -------- key: select all of sound -------- */

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

/* -------- key: undo all edits -------- */

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

/* -------- key: exit -------- */

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

/* -------- key: goto maxamp -------- */

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

/* -------- key: show selection -------- */

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



/* ---------------- find functions ---------------- */

static char *find_clm_file_name(void)
{
  XEN val;
  val = prefs_variable_get("*clm-file-name*");
  if (XEN_STRING_P(val))
    return(XEN_TO_C_STRING(val));
  return(NULL);
}

static void set_clm_file_name(const char *str)
{
  prefs_variable_set(str, C_TO_XEN_STRING(str));
}

static int find_clm_table_size(void)
{
  XEN size;
  size = prefs_variable_get("*clm-table-size*");
  return(XEN_TO_C_INT_OR_ELSE(size, 512));
}

static int find_clm_file_buffer_size(void)
{
  XEN size;
  size = prefs_variable_get("*clm-file-buffer-size*");
  return(XEN_TO_C_INT_OR_ELSE(size, 65536));
}

static char *find_sources(void) /* returns full filename if found else null */
{
  XEN file;
#if HAVE_GUILE
  #define BASE_FILE "extensions.scm"
  file = scm_sys_search_load_path(C_TO_XEN_STRING(BASE_FILE));
#endif
#if HAVE_GAUCHE 
  #define BASE_FILE "extensions.scm"
  file = C_TO_XEN_STRING("extensions.scm");
#endif
#if HAVE_RUBY
  #define BASE_FILE "extensions.rb"
#if RB_FIND_FILE_TAKES_VALUE
  file = rb_find_file(C_TO_XEN_STRING(BASE_FILE));
#else
  file = C_TO_XEN_STRING(rb_find_file(BASE_FILE));
#endif
#endif
#if HAVE_FORTH
  #define BASE_FILE "extensions.fs"
  file = fth_find_file(C_TO_XEN_STRING(BASE_FILE));
#endif
#ifndef BASE_FILE
  #define BASE_FILE "extensions.scm"
#endif
  if (XEN_STRING_P(file))
    {
      char *str;
      int len, exts_len;
#if HAVE_GUILE || HAVE_FORTH
      str = copy_string(XEN_TO_C_STRING(file));
#endif
#if HAVE_RUBY || HAVE_GAUCHE
      str = mus_expand_filename(XEN_TO_C_STRING(file));
#endif
      len = snd_strlen(str);
      exts_len = strlen(BASE_FILE);
      if (len > exts_len)
	str[len - exts_len - 1] = '\0';
      return(str);
    }
  return(NULL);
}

static bool local_unsaved_edits = false;
static bool unsaved_edits(void)
{
  local_unsaved_edits = XEN_TO_C_BOOLEAN(prefs_variable_get("checking-for-unsaved-edits"));
  return(local_unsaved_edits);
}

static void set_unsaved_edits(bool val)
{
  local_unsaved_edits = val;
  if (XEN_DEFINED_P("checking-for-unsaved-edits"))
    {
      /* TODO: should the unsaved-edits toggle screw around with the hook? reflect case currently assumes it does */
    }
}

static bool find_current_window_display(void)
{
  /* there's no clean way to look for the functions on the hook lists, so I'll kludge up some variable... */
  return((XEN_DEFINED_P("current-window-display-is-running")) &&
	 (XEN_TRUE_P(XEN_NAME_AS_C_STRING_TO_VALUE("current-window-display-is-running"))));
}

static bool focus_is_following_mouse(void)
{
  return((XEN_DEFINED_P("focus-is-following-mouse")) &&
	 (XEN_TRUE_P(XEN_NAME_AS_C_STRING_TO_VALUE("focus-is-following-mouse"))));
}

static int find_remember_sound_state_choice(void)
{
  if (XEN_DEFINED_P("remembering-sound-state"))
    return(XEN_TO_C_INT(XEN_NAME_AS_C_STRING_TO_VALUE("remembering-sound-state")));
  return(0);
}

static bool find_peak_envs(void)
{
  return((XEN_DEFINED_P("save-peak-env-info?")) &&
	 XEN_TO_C_BOOLEAN(XEN_NAME_AS_C_STRING_TO_VALUE("save-peak-env-info?")));
}

static bool find_context_sensitive_popup(void)
{
  return(XEN_DEFINED_P("edhist-help-edits")); /* defined in both cases */
}

static bool find_effects_menu(void)
{
  return(XEN_DEFINED_P("effects-menu"));
}

#if HAVE_SCHEME
static bool find_edit_menu(void)
{
  return(XEN_DEFINED_P("make-stereofile")); /* a kludge... currently this is only defined in edit-menu.scm */
}

static bool find_marks_menu(void)
{
  return(XEN_DEFINED_P("marks-menu"));
}

static bool find_mix_menu(void)
{
  return(XEN_DEFINED_P("mix-menu"));
}

#if USE_MOTIF
static bool find_icon_box(void)
{
  return(XEN_DEFINED_P("add-useful-icons"));
}
#endif
#endif

static bool find_reopen_menu(void)
{
  return((XEN_DEFINED_P("including-reopen-menu")) &&
	 XEN_TO_C_BOOLEAN(XEN_NAME_AS_C_STRING_TO_VALUE("including-reopen-menu")));
}

static bool find_smpte(void)
{
#if HAVE_SCHEME 
  return((XEN_DEFINED_P("smpte-is-on")) && 
	 (!(XEN_FALSE_P(XEN_EVAL_C_STRING("(smpte-is-on)"))))); /* "member" of hook-list -> a list if successful */ 
#else 
  return((XEN_DEFINED_P("smpte-is-on")) && 
	 XEN_TO_C_BOOLEAN(XEN_EVAL_C_STRING(TO_PROC_NAME("smpte-is-on")))); /* "member" of hook-list -> true */ 
#endif 
}

#if HAVE_SCHEME
static bool find_hidden_controls(void)
{
  return((XEN_DEFINED_P("hidden-controls-dialog")) &&
	 (XEN_NOT_FALSE_P(XEN_NAME_AS_C_STRING_TO_VALUE("hidden-controls-dialog"))));
}
#endif

#if USE_MOTIF
static bool find_mark_pane(void)
{
  return((XEN_DEFINED_P("including-mark-pane")) &&
	 XEN_TO_C_BOOLEAN(XEN_NAME_AS_C_STRING_TO_VALUE("including-mark-pane")));
}
#endif

#if HAVE_GUILE
static bool find_debugging_aids(void)
{
  return((XEN_DEFINED_P("snd-break")) && 
	 (XEN_DEFINED_P("untrace-stack")));
}
#endif


/* ---------------- revert/clear functions ---------------- */

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

static bool rts_unsaved_edits = false;
static void revert_unsaved_edits(prefs_info *prf) {set_unsaved_edits(rts_unsaved_edits);}
static void clear_unsaved_edits(prefs_info *prf) {set_unsaved_edits(false);}


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

