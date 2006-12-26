#include "snd.h"
#include "sndlib-strings.h"

static char *current_match = NULL;

/* TAB completion requires knowing what is currently defined, which requires scrounging
 *   around in the symbol tables (an "obarray" in Guile, hash table in Gauche, vector in Ruby)
 */

#if HAVE_GAUCHE
static int completions(char *text)
{
  ScmHashTable *tab;
  int len, matches = 0;
  ScmHashEntry *e;
  ScmHashIter iter;
  ScmModule *m = Scm_UserModule();
  tab = m->table;
  Scm_HashIterInit(tab, &iter);
  len = strlen(text);
  while ((e = Scm_HashIterNext(&iter)) != NULL)
    {
      char *sym;
      /* fprintf(stderr,"e: %p, value: %s %s\n", e, XEN_AS_STRING(e->value), XEN_AS_STRING(SCM_GLOC(e->value)->name)); */
      sym = XEN_SYMBOL_TO_C_STRING(SCM_GLOC(e->value)->name);
      if (strncmp(text, sym, len) == 0)
	{
	  matches++;
	  add_possible_completion(sym);
	  if (current_match == NULL)
	    current_match = copy_string(sym);
	  else 
	    {
	      int j, curlen;
	      curlen = snd_strlen(current_match);
	      for (j = 0; j < curlen; j++)
		if (current_match[j] != sym[j])
		  {
		    current_match[j] = '\0';
		    break;
		  }
	    }
	}
    }
  return(matches);
}
#endif

#if HAVE_GUILE
#if defined(SCM_MODULE_OBARRAY)
static int xen_return_first_int(int a, ...)
{
  return(a);
}

static int scan_tab(XEN tab, char *text, int len, int matches)
{
  int i, n;
  XEN ls = XEN_FALSE, handle = XEN_FALSE;
#ifdef SCM_HASHTABLE_BUCKET
  n = SCM_HASHTABLE_N_BUCKETS(tab);
  for (i = 0; i < n; i++)
    {
      ls = SCM_HASHTABLE_BUCKET(tab, i); /* from libguile/modules.c */
#else
#ifdef SCM_HASHTABLE_BUCKETS
  /* this version searches through Guile's module's hash tables */
  n = SCM_HASHTABLE_N_BUCKETS(tab);
  for (i = 0; i < n; i++)
    {
      ls = SCM_HASHTABLE_BUCKETS(tab)[i];
#else
  n = XEN_VECTOR_LENGTH(tab);
  for (i = 0; i < n; ++i)
    {
      ls = SCM_VELTS(tab)[i];
#endif
#endif
      while (XEN_NOT_NULL_P(ls))
	{
	  handle = XEN_CAR(XEN_CAR(ls));
	  if (XEN_SYMBOL_P(handle)) /* can be a number: (2.0 . #<variable...>) -- wasn't this a bug in weak-tables? */
	    {
	      char *sym;
	      sym = XEN_SYMBOL_TO_C_STRING(handle);
	      if (sym)
		{
		  if (strncmp(text, sym, len) == 0)
		    {
		      matches++;
		      add_possible_completion(sym);
		      if (current_match == NULL)
			current_match = copy_string(sym);
		      else 
			{
			  int j, curlen;
			  curlen = snd_strlen(current_match);
			  for (j = 0; j < curlen; j++)
			    if (current_match[j] != sym[j])
			      {
				current_match[j] = '\0';
				break;
			      }
			}
		    }
		}
	    }
	  ls = XEN_CDR(ls);
	}
    }
  return(xen_return_first_int(matches, handle, ls, tab));
}

static int completions(char *text)
{
  int len, matches = 0;
  XEN curmod = XEN_FALSE, uses = XEN_FALSE;
  len = strlen(text);
  curmod = scm_current_module();
  matches = scan_tab(SCM_MODULE_OBARRAY(curmod), 
		     text, len, 0);
  uses = SCM_MODULE_USES(curmod);
  while (XEN_CONS_P(uses))
    {
      matches = scan_tab(SCM_MODULE_OBARRAY(XEN_CAR(uses)), 
			 text, len, matches);
      uses = XEN_CDR(uses);
    }
  return(xen_return_first_int(matches, curmod, uses));
}
#else
static int completions(char *text) {return(0);}
#endif
#endif
/* end Guile && SCM_MODULE_OBARRAY */

#if HAVE_RUBY
static XEN snd_rb_methods(void)
{
  /* returns all the functions we defined */
  XEN argv[1];
  argv[0] = XEN_TRUE;
  return(rb_class_private_instance_methods(1, argv, rb_mKernel));
  /* rb_ary_new here -- should we free? */
}

static int completions(char *text)
{
  XEN tab;
  int i, n, len, matches = 0;
  tab = snd_rb_methods();
  n = XEN_VECTOR_LENGTH(tab);
  len = strlen(text);
  for (i = 0; i < n; ++i)
    {
      char *sym;
      XEN handle;
      handle = XEN_VECTOR_REF(tab, i);
      sym = XEN_AS_STRING(handle);
      if (strncmp(text, sym, len) == 0)
	{
	  matches++;
	  add_possible_completion(sym);
	  if (current_match == NULL)
	    current_match = copy_string(sym);
	  else 
	    {
	      int j, curlen;
	      curlen = snd_strlen(current_match);
	      for (j = 0; j < curlen; j++)
		if (current_match[j] != sym[j])
		  {
		    current_match[j] = '\0';
		    break;
		  }
	    }
	}
    }
  return(matches);
}
#endif

#if HAVE_FORTH
static int completions(char *text)
{
  XEN tab = fth_find_in_wordlist(text);
  int i, matches = XEN_VECTOR_LENGTH(tab);
  for (i = 0; i < matches; i++)
    {
      char *sym = XEN_TO_C_STRING(XEN_VECTOR_REF(tab, i));
      add_possible_completion(sym);
      if (current_match == NULL)
	current_match = copy_string(sym);
      else 
	{
	  int j, curlen;
	  curlen = snd_strlen(current_match);
	  for (j = 0; j < curlen; j++)
	    if (current_match[j] != sym[j])
	      {
		current_match[j] = '\0';
		break;
	      }
	}
    }
  return(matches);
}
#endif

#if (!HAVE_EXTENSION_LANGUAGE)
static int completions(char *text) {return(0);}
#endif

#if HAVE_FORTH
bool separator_char_p(char c)
{
  /* only space is separator */
  return(!(isgraph((int)c)));
}
#else
bool separator_char_p(char c)
{
  return((!(isalpha((int)c))) &&
	 (!(isdigit((int)c))) &&
#if HAVE_RUBY
	 (c != '?') &&
	 (c != '!') &&
	 (c != '_') &&
#endif
#if HAVE_SCHEME
	 (c != '-') &&
	 (c != '_') &&
	 (c != '>') &&
	 (c != '?') &&
	 (c != '!') &&
	 (c != '=') &&
	 (c != '<') &&
	 (c != '*') &&
	 (c != '+') &&
	 (c != '%') &&
	 (c != ':') &&
#endif
	 (c != '$'));
}
#endif

char *command_completer(char *original_text, void *data)
{
  int i, len, beg, matches = 0;
  char *text;
  /* first back up to some delimiter to get the current command */

  current_match = NULL;
  set_completion_matches(0);
  if ((original_text) && (*original_text))
    {
      len = strlen(original_text);
      for (i = len - 1; i >= 0; i--)
	if (separator_char_p(original_text[i]))
	  break;
      beg = i + 1;
      if (beg == len) 
	{
	  /* returning original = no-op response to <tab> which seems useless;
	   *   so, if it's a function that we recognize and that function has its own completer, call it?
	   *   result null -> return original (current behavior), else append result as selection to current;
	   *   this way, I think it's easy to clear the suggested completion (cursor at end, so backspace deletes all).
	   * or scan back through full text (assuming listener or history available) and find match?
	   */
	  return(copy_string(original_text));
	}
      if (beg > 0) 
	text = (char *)(original_text + beg);
      else text = original_text;
      matches = completions(text);
    }
  else return(copy_string(original_text));
  set_completion_matches(matches);
  if ((current_match) && 
      (*current_match))
    {
      if (beg == 0)
	return(current_match);
      else
	{
	  len = snd_strlen(current_match) + beg + 2;
	  text = (char *)CALLOC(len, sizeof(char));
	  strncpy(text, original_text, beg);
	  strcat(text, current_match);
	  FREE(current_match);
	  return(text);
	}
    }
  return(copy_string(original_text));
}

/* ---------------- COMMAND/FILENAME COMPLETIONS ---------------- */

typedef char *(*completer_func)(char *text, void *data);
static completer_func *completer_funcs = NULL;
static void **completer_data = NULL;
static int completer_funcs_size = 0;
static int completer_funcs_end = 0;

int add_completer_func(char *(*func)(char *text, void *context), void *data)
{
  if (completer_funcs_size == completer_funcs_end)
    {
      completer_funcs_size += 8;
      if (completer_funcs == NULL)
	{
	  completer_funcs = (completer_func *)CALLOC(completer_funcs_size, sizeof(completer_func));
	  completer_data = (void **)CALLOC(completer_funcs_size, sizeof(void *));
	}
      else 
	{
	  int i;
	  completer_funcs = (completer_func *)REALLOC(completer_funcs, completer_funcs_size * sizeof(completer_func));
	  completer_data = (void **)REALLOC(completer_data, completer_funcs_size * sizeof(void *));
	  for (i = completer_funcs_end; i < completer_funcs_size; i++) completer_data[i] = NULL;
	}
    }
  completer_funcs[completer_funcs_end] = func;
  completer_data[completer_funcs_end] = data;
  completer_funcs_end++;
  return(completer_funcs_end - 1);
}

static int completion_matches = 0;
int get_completion_matches(void) {return(completion_matches);}
void set_completion_matches(int matches) {completion_matches = matches;}
static bool save_completions = 0;
static char **possible_completions = NULL;
static int possible_completions_size = 0;
static int possible_completions_ctr = 0;

void set_save_completions(bool save) {save_completions = save;}

void add_possible_completion(const char *text)
{
  if (save_completions)
    {
      if (possible_completions_size == possible_completions_ctr)
	{
	  possible_completions_size += 16;
	  if (possible_completions == NULL)
	    possible_completions = (char **)CALLOC(possible_completions_size, sizeof(char *));
	  else
	    {
	      int i;
	      possible_completions = (char **)REALLOC(possible_completions, possible_completions_size * sizeof(char *));
	      for (i = possible_completions_ctr; i < possible_completions_size; i++) possible_completions[i] = NULL;
	    }
	}
      if (possible_completions[possible_completions_ctr]) FREE(possible_completions[possible_completions_ctr]);
      possible_completions[possible_completions_ctr] = copy_string(text);
      possible_completions_ctr++;
    }
}

void display_completions(void)
{
  if (possible_completions_ctr > 0)
    snd_completion_help(possible_completions_ctr, possible_completions);
}

char *complete_text(char *text, int func)
{
  /* given text, call proc table entry func, return new text (not text!) */
  completion_matches = -1; /* i.e. no completer */
  possible_completions_ctr = 0;
  if ((func >= 0) && 
      (func < completer_funcs_end))
    return((*completer_funcs[func])(text, completer_data[func]));
  else return(copy_string(text));
}

void clear_possible_completions(void) 
{
  int i;
  for (i = 0; i < possible_completions_size; i++)
    if (possible_completions[i]) 
      {
	FREE(possible_completions[i]);
	possible_completions[i] = NULL;
      }
  possible_completions_ctr = 0;
}

static list_completer_info *srate_info = NULL;

static void init_srate_list(void)
{
  if (srate_info == NULL)
    {
      int loc = 0;
      srate_info = (list_completer_info *)CALLOC(1, sizeof(list_completer_info));
      srate_info->exact_match = true;
      srate_info->values_size = 16;
      srate_info->values = (char **)CALLOC(srate_info->values_size, sizeof(char *));
      srate_info->values[loc++] = copy_string("44100");
      srate_info->values[loc++] = copy_string("22050");
      srate_info->values[loc++] = copy_string("8000");
      srate_info->values[loc++] = copy_string("48000");
      srate_info->num_values = loc;
    }
}

char *srate_list_to_string(int row)
{
  if ((srate_info) &&
      (srate_info->num_values > row))
    return(srate_info->values[row]);
  return(NULL);
}

list_completer_info *srate_list(void)
{
  init_srate_list();
  return(srate_info);
}

void add_srate_to_completion_list(int srate)
{
  char *str;
  int i;
  init_srate_list();
  str = (char *)CALLOC(16, sizeof(char));
  mus_snprintf(str, 16, "%d", srate);
  for (i = 0; i < srate_info->num_values; i++)
    if (strcmp(srate_info->values[i], str) == 0)
      {
	FREE(str);
	return;
      }
  if (srate_info->num_values >= srate_info->values_size)
    {
      srate_info->values_size += 16;
      srate_info->values = (char **)REALLOC(srate_info->values, srate_info->values_size * sizeof(char *));
      for (i = srate_info->num_values; i < srate_info->values_size; i++) srate_info->values[i] = NULL;
    }
  srate_info->values[srate_info->num_values++] = str;
}

char *srate_completer(char *text, void * data)
{
  init_srate_list();
  return(list_completer(text, (void *)srate_info));
}

#if HAVE_DIRENT_H
  #include <dirent.h>
#endif

enum {ANY_FILE_TYPE, SOUND_FILE_TYPE};

static char *filename_completer_1(char *text, int file_type)
{
#if HAVE_OPENDIR
  /* assume text is a partial filename */
  /* get directory name, opendir, read files checking for match */
  /* return name of same form as original (i.e. don't change user's directory indication) */
  /* if directory, add "/" -- directory_p(name) static in snd-xfile.c */

  char *full_name = NULL, *dir_name = NULL, *file_name = NULL, *current_match = NULL;
  int i, j, k, len, curlen, matches = 0;
  struct dirent *dirp;
  DIR *dpos;
  if (snd_strlen(text) == 0) return(NULL);
  full_name = mus_expand_filename(text);
  len = snd_strlen(full_name);
  for (i = len - 1; i > 0; i--)
    if (full_name[i] == '/')
      break;
  dir_name = (char *)CALLOC(i + 1, sizeof(char));
  strncpy(dir_name, full_name, i);
  file_name = (char *)CALLOC(len - i + 2, sizeof(char));
  for (j = 0, k = i + 1; k < len; j++, k++) 
    file_name[j] = full_name[k];
  if (full_name) 
    {
      FREE(full_name); 
      full_name = NULL;
    }
  len = snd_strlen(file_name);
  if ((dpos = opendir(dir_name)) != NULL)
    {
      while ((dirp = readdir(dpos)) != NULL)
	if ((dirp->d_name[0] != '.') && 
	    (strncmp(dirp->d_name, file_name, len) == 0)) /* match dirp->d_name against rest of text */
	  {
	    if ((file_type == ANY_FILE_TYPE) ||
		(sound_file_p(dirp->d_name)))
	      {
		matches++;
		add_possible_completion(dirp->d_name);
		if (current_match == NULL)
		  current_match = copy_string(dirp->d_name);
		else 
		  {
		    curlen = strlen(current_match);
		    for (j = 0; j < curlen; j++)
		      if (current_match[j] != dirp->d_name[j])
			{
			  current_match[j] = '\0';
			  break;
			}
		  }
	      }
	  }

      if (closedir(dpos) != 0) 
	snd_error("closedir %s failed (%s)!", dir_name, snd_io_strerror());
    }
  if (dir_name) FREE(dir_name);
  if (file_name) FREE(file_name);
  set_completion_matches(matches);
  if ((current_match) && 
      (*current_match))
    {
      /* attach matched portion to user's indication of dir */
      len = snd_strlen(text);
      for (i = len - 1; i >= 0; i--)
	if (text[i] == '/')
	  break;
      if (i < 0) return(current_match);
      curlen = strlen(current_match) + len + 3;
      file_name = (char *)CALLOC(curlen, sizeof(char));
      strncpy(file_name, text, i + 1);
      strcat(file_name, current_match);
      if (directory_p(file_name)) 
	strcat(file_name, "/");
      FREE(current_match);
      return(file_name);
    }
#endif
  return(copy_string(text));
}

char *filename_completer(char *text, void *data)
{
  return(filename_completer_1(text, ANY_FILE_TYPE));
}

char *sound_filename_completer(char *text, void *data)
{
  return(filename_completer_1(text, SOUND_FILE_TYPE));
}

static bool use_sound_filename_completer(sp_filing_t filing)
{
  return((filing == INPUT_FILING) ||  /* C-x C-f */
	 (filing == CHANGE_FILING) || /* C-x C-q */
	 (filing == INSERT_FILING));   /* C-x C-i */
}

char *info_completer(char *text, void *data)
{
  snd_info *sp = (snd_info *)data;
  if (sp)
    {
      char *new_text = NULL;
      if (sp->search_count != 0) return(copy_string(text));      /* C-s or C-r so as above */
      if ((sp->marking) || (sp->finding_mark)) return(copy_string(text)); /* C-x C-m etc */
      if (sp->printing) return(copy_string(text));               /* C-x C-d so anything is possible */
      if (sp->amp_count != 0) return(env_name_completer(text, NULL));
      if (use_sound_filename_completer(sp->filing)) return(sound_filename_completer(text, NULL));
      if (sp->loading) return(filename_completer(text, NULL));   /* C-x C-l */

      new_text = command_completer(text, NULL);
      if (get_completion_matches() == 0)
	{
	  int i, beg, parens, len;
	  beg = 0;
	  parens = 0;  	                                 /* filename would have to be a string in this context */
	  len = snd_strlen(text);
	  for (i = 0; i < len; i++)
	    if (text[i] == '\"')
	      {
		beg = i + 1;
		parens++;
		break;
	      }
	  if ((beg > 0) && (parens & 1))                 /* i.e. there is a string and we're in it */
	    {
	      char *new_file;
	      if (new_text) FREE(new_text);
	      new_file = filename_completer((char *)(text + beg), NULL);
	      len = beg + 2 + snd_strlen(new_file);
	      new_text = (char *)CALLOC(len, sizeof(char));
	      strncpy(new_text, text, beg);
	      strcat(new_text, new_file);
	      if (new_file) FREE(new_file);
	    }
	}
      return(new_text);
    }
  return(command_completer(text, NULL));
}

static int find_indentation(char *str, int loc)
{
  int line_beg = 0, open_paren = -1, parens, i;
  parens = 0;
  for (i = loc - 1; i >= 0; i--)
    {
      if (str[i] == ')') parens--;
      if (str[i] == '(') parens++;
      if (parens == 1) 
	{
	  open_paren = i; 
	  break;
	}
    }
  if (open_paren == -1) return(1);
  if (open_paren == 0) return(3);
  for (i = open_paren - 1; i > 0; i--)
    if (str[i] == '\n') 
      {
	line_beg = i; 
	break;
      }
  if (line_beg == 0) return(1);
  return(open_paren - line_beg + 2);
}

char *complete_listener_text(char *old_text, int end, bool *try_completion, char **to_file_text)
{
  int len, i, k, spaces, text_pos = 0, cr_pos = 0;
  char *new_text = NULL, *file_text = NULL, *new_file = NULL;
  len = strlen(old_text);
  for (i = len - 1; i > 0; i--)
    {
      if (old_text[i] == '\n')
	{
	  /* tab as indentation */
	  /* look at previous line to decide */
	  spaces = find_indentation(old_text, i);
	  if (spaces > 0)
	    {
	      file_text = (char *)CALLOC(spaces + 1, sizeof(char));
	      for (k = 0; k < spaces; k++) 
		file_text[k] = ' ';
	      file_text[spaces] = 0;
	      append_listener_text(end, file_text);
	      FREE(file_text);
	      file_text = NULL;
	    }
	  (*try_completion) = false;
	  return(NULL);
	}
      if (old_text[i] == ';')
	{
	  /* this isn't quite right, but how much effort should we put in it? */
	  spaces = 20;
	  for (k = i - 1; k > 0; k--) 
	    if (old_text[k] == '\n') 
	      {
		cr_pos = k; 
		break;
	      } 
	    else 
	      if ((!(isspace((int)(old_text[k])))) && 
		  (text_pos == 0)) 
		text_pos = k;
	  if (text_pos > 0)
	    text_pos -= cr_pos;
	  if (cr_pos == 0) spaces--; 
	  if (text_pos < spaces)
	    {
	      file_text = (char *)CALLOC(spaces + 2, sizeof(char));
	      for (k = text_pos + 1; k < spaces; k++) 
		file_text[k - text_pos - 1] = ' ';
	      file_text[spaces] = ';';
	      file_text[spaces + 1] = 0;
	      append_listener_text(end - 1, file_text);
	      FREE(file_text);
	      file_text = NULL;
	    }
	  (*try_completion) = false;
	  return(NULL);
	}
      if (old_text[i] == '\"')
	{
	  file_text = copy_string((char *)(old_text + i + 1));
	  new_file = filename_completer(file_text, NULL);
	  len = snd_strlen(new_file);
	  if (len > 0)
	    {
	      len += i + 2;
	      new_text = (char *)CALLOC(len, sizeof(char));
	      strncpy(new_text, old_text, i + 1);
	      strcat(new_text, new_file);
	      if (new_file) FREE(new_file);
	    }
	  break;
	}
      if (isspace((int)(old_text[i]))) break;
    }
  if (new_text == NULL) new_text = command_completer(old_text, NULL);
  (*try_completion) = true;
  (*to_file_text) = file_text;
  return(new_text);
}

char *list_completer(char *text, void *data)
{
  list_completer_info *info = (list_completer_info *)data;
  int i, j = 0, len, matches = 0, current_match = -1;
  char *trimmed_text;
  set_completion_matches(0);
  /* check for null text */
  len = snd_strlen(text);
  if (len == 0) return(copy_string(text));
  /* strip away leading and trailing white space */
  trimmed_text = (char *)CALLOC(len + 1, sizeof(char));
  for (i = 0; i < len; i++)
    if (!(isspace(text[i])))
      trimmed_text[j++] = text[i];
  if (j == 0)
    {
      FREE(trimmed_text);
      return(copy_string(text));
    }
  /* check for match(es) against values */
  if (info->exact_match)
    {
      for (i = 0; i < info->num_values; i++)
	if ((info->values[i]) &&
	    (strncmp(info->values[i], trimmed_text, len) == 0))
	  {
	    matches++;
	    current_match = i;
	  }
    }
  else
    {
      for (i = 0; i < info->num_values; i++)
	if ((info->values[i]) &&
	    (STRNCMP(info->values[i], trimmed_text, len) == 0))
	  {
	    matches++;
	    current_match = i;
	  }
    }
  FREE(trimmed_text);
  if (matches != 1)
    return(copy_string(text));
  set_completion_matches(1);
  return(copy_string(info->values[current_match]));
}
