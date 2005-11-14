/* this file included as text in snd-g|xprefs.c */

#if HAVE_SCHEME
  #define LANG_NAME "scheme"
#endif
#if HAVE_RUBY
  #define LANG_NAME "ruby"
#endif

static int prefs_size = 0, prefs_top = 0;
static prefs_info **prefs = NULL;

static void remember_pref(prefs_info *prf, 
			  void (*reflect_func)(struct prefs_info *prf),
			  void (*save_func)(struct prefs_info *prf, FILE *fd))
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
  prf->help_func = NULL;
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
#if HAVE_STRFTIME
  {
    time_t ts;
    time(&ts);
    if (!prefs_time) prefs_time = (char *)CALLOC(TIME_STR_SIZE, sizeof(char));
    strftime(prefs_time, TIME_STR_SIZE, "%H:%M", localtime(&ts));
  }
#endif
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
#if HAVE_SCHEME
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

static char *clm_file_name(void)
{
#if HAVE_SCHEME
  if (XEN_DEFINED_P("*clm-file-name*"))
    return(XEN_TO_C_STRING(XEN_NAME_AS_C_STRING_TO_VALUE("*clm-file-name*")));
#endif
#if HAVE_RUBY
  if (XEN_DEFINED_P("clm-file-name"))
    return(XEN_TO_C_STRING(XEN_NAME_AS_C_STRING_TO_VALUE("clm-file-name")));
#endif
  return(NULL);
}

static void set_clm_file_name(const char *str)
{
#if HAVE_SCHEME
  if (XEN_DEFINED_P("*clm-file-name*"))
    XEN_VARIABLE_SET(XEN_NAME_AS_C_STRING_TO_VARIABLE("*clm-file-name*"), C_TO_XEN_STRING(str));
#endif
#if HAVE_RUBY
  if (XEN_DEFINED_P("clm-file-name"))
    XEN_VARIABLE_SET("clm-file-name", C_TO_XEN_STRING(str));
#endif
}

static int clm_table_size(void)
{
#if HAVE_SCHEME
  if (XEN_DEFINED_P("*clm-table-size*"))
    return(XEN_TO_C_INT(XEN_NAME_AS_C_STRING_TO_VALUE("*clm-table-size*")));
#endif
#if HAVE_RUBY
  if (XEN_DEFINED_P("clm-table-size"))
    return(XEN_TO_C_INT(XEN_NAME_AS_C_STRING_TO_VALUE("clm-table-size")));
#endif
  return(512);
}

static int clm_file_buffer_size(void)
{
#if HAVE_SCHEME
  if (XEN_DEFINED_P("*clm-file-buffer-size*"))
    return(XEN_TO_C_INT(XEN_NAME_AS_C_STRING_TO_VALUE("*clm-file-buffer-size*")));
#endif
#if HAVE_RUBY
  if (XEN_DEFINED_P("clm-file-buffer-size"))
    return(XEN_TO_C_INT(XEN_NAME_AS_C_STRING_TO_VALUE("clm-file-buffer-size")));
#endif
  return(65536);
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

static void sync_choice_help(prefs_info *prf)
{
  snd_help(prf->var_name,
	   "Many operations can operate on all channels at once, or only on the currently selected \
channel.  If either of these buttons is selected, such operations operate either on all channels \
within each sound (but not across sounds), or on all current channels at once.  The default is \
to operate only on the selected channel (neither button selected).",
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
  /* needed by Ruby */
  snd_help(prf->var_name,
	   "This option adds a label to the time domain graph showing the current SMPTE frame of the leftmost sample.",
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
}

static void save_sync_choice_1(prefs_info *prf, FILE *fd, int choice)
{
#if HAVE_SCHEME
  fprintf(fd, "(if (not (provided? 'snd-extensions.scm)) (load-from-path \"extensions.scm\"))\n");
  fprintf(fd, "(set-global-sync %d)\n", choice);
#endif
#if HAVE_RUBY
  fprintf(fd, "require \"extensions\"\n");
  fprintf(fd, "set_global_sync(%d)\n", choice);
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
}

static void save_view_files_directory_1(prefs_info *prf, FILE *fd, char *vdir)
{
#if HAVE_SCHEME
  fprintf(fd, "(%s \"%s\")\n", S_add_directory_to_view_files_list, vdir);
#endif
#if HAVE_RUBY
  fprintf(fd, "%s(\"%s\")\n", TO_PROC_NAME(S_add_directory_to_view_files_list), vdir);
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
}

static void save_effects_menu_1(prefs_info *prf, FILE *fd)
{
#if HAVE_SCHEME
  fprintf(fd, "(if (provided? 'snd-motif)\n    (if (not (provided? 'snd-new-effects.scm))\n        (load-from-path \"new-effects.scm\"))\n    (if (not (provided? 'snd-gtk-effects.scm))\n	(load-from-path \"gtk-effects.scm\")))\n");
#endif
#if HAVE_RUBY
  fprintf(fd, "require \"effects\"\n");
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
}

static void save_show_listener_1(prefs_info *prf, FILE *fd)
{
#if HAVE_SCHEME
  /* show-listener is saved in save-state, but not save-options */
  fprintf(fd, "(show-listener)\n");
#endif
#if HAVE_RUBY
  fprintf(fd, "show_listener\n");
#endif
}


/* ---------------- find functions ---------------- */

static char *find_sources(void) /* returns full filename if found else null */
{
  XEN file;
#if HAVE_GUILE
  #define BASE_FILE "extensions.scm"
  file = scm_sys_search_load_path(C_TO_XEN_STRING(BASE_FILE));
#endif
#if HAVE_RUBY
  #define BASE_FILE "extensions.rb"
  file = rb_find_file(C_TO_XEN_STRING(BASE_FILE));
#endif  
  if (XEN_STRING_P(file))
    {
      char *str;
      int len, exts_len;
#if HAVE_GUILE
      str = copy_string(XEN_TO_C_STRING(file));
#endif
#if HAVE_RUBY
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

static bool unsaved_edits(void)
{
  return((XEN_DEFINED_P("checking-for-unsaved-edits")) &&
	 (XEN_TRUE_P(XEN_NAME_AS_C_STRING_TO_VALUE("checking-for-unsaved-edits"))));
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

static int find_sync_choice(void)
{
  if (XEN_DEFINED_P("global-sync-choice"))
    return(XEN_TO_C_INT(XEN_NAME_AS_C_STRING_TO_VALUE("global-sync-choice")));
  return(0);
}

static bool find_peak_envs(void)
{
  return((XEN_DEFINED_P("save-peak-env-info?")) &&
	 XEN_TO_C_BOOLEAN(XEN_NAME_AS_C_STRING_TO_VALUE("save-peak-env-info?")));
}

static bool find_context_sensitive_popup(void)
{
#if HAVE_SCHEME
  return(XEN_DEFINED_P("edhist-help-edits")); /* defined in both cases */
#endif
#if HAVE_RUBY
  return(XEN_DEFINED_P("Snd_popup_menu"));
#endif
}

static bool find_effects_menu(void)
{
#if HAVE_SCHEME
  return(XEN_DEFINED_P("effects-menu"));
#endif
#if HAVE_RUBY
  return(XEN_DEFINED_P("Effects"));
#endif
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
#endif

static bool find_reopen_menu(void)
{
  return((XEN_DEFINED_P("including-reopen-menu")) &&
	 XEN_TO_C_BOOLEAN(XEN_NAME_AS_C_STRING_TO_VALUE("including-reopen-menu")));
}

static bool find_smpte(void)
{
  return((XEN_DEFINED_P("smpte-is-on")) &&
	 (!(XEN_FALSE_P(XEN_EVAL_C_STRING("(smpte-is-on)"))))); /* "member" of hook-list -> a list if successful */
}

#if HAVE_SCHEME
static bool find_hidden_controls(void)
{
  return((XEN_DEFINED_P("hidden-controls-dialog")) &&
	 (XEN_NOT_FALSE_P(XEN_NAME_AS_C_STRING_TO_VALUE("hidden-controls-dialog"))));
}
#endif

#if HAVE_GUILE
static bool find_debugging_aids(void)
{
  return((XEN_DEFINED_P("snd-break")) && 
	 (XEN_DEFINED_P("untrace-stack")));
}
#endif
