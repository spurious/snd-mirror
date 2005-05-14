#include "snd.h"

#if HAVE_DIRENT_H
  #include <dirent.h>
#else
  #define dirent direct
  #if HAVE_SYS_NDIR_H
    #include <sys/ndir.h>
  #endif
  #if HAVE_SYS_DIR_H
    #include <sys/dir.h>
  #endif
  #if HAVE_NDIR_H
    #include <ndir.h>
  #endif
#endif

#if USE_STATVFS
  #include <sys/statvfs.h>
#endif

#if (__bsdi__ || HAVE_SYS_PARAM_H)
  #include <sys/param.h>
#endif

/* to handle mount.h correctly in autoconf I'd need to specialize the header checker for
 *   whatever headers mount.h needs, which I assume depends on the OS -- not worth the
 *   trouble!  Perhaps there's a better way to handle disk-kspace?
 */
#if MAC_OSX || defined(__bsdi__)
  #include <sys/mount.h>
#endif

#if (!USE_STATVFS)
  off_t disk_kspace (const char *filename) {return(1234567);}
#else

off_t disk_kspace (const char *filename)
{
#if SUN
  statvfs_t buf; /* else dumb compiler complaint */
#else
  struct statvfs buf;
#endif
  off_t err = -1;
  err = statvfs(filename, &buf);
  if (err == 0)
    {
      if (buf.f_frsize == 1024) return(buf.f_bfree);
      else return((off_t)(buf.f_frsize * ((double)(buf.f_bfree) / 1024.0)));
    }
  return(err);
}
#endif

bool link_p(const char *filename)
{
#if HAVE_LSTAT
  struct stat statbuf;
  if (lstat(filename, &statbuf) >= 0) 
    return((bool)(S_ISLNK(statbuf.st_mode)));
#endif
  return(false);
}

bool directory_p(const char *filename)
{
#if HAVE_LSTAT
  struct stat statbuf;
  if (lstat(filename, &statbuf) >= 0) 
    return((bool)(S_ISDIR(statbuf.st_mode)));
#endif
  return(false);
}

static bool empty_file_p(const char *filename)
{
#if HAVE_LSTAT
  struct stat statbuf;
  if (lstat(filename, &statbuf) >= 0) 
    return(statbuf.st_size == (off_t)0);
#endif
  return(false);
}

time_t file_write_date(const char *filename)
{
  struct stat statbuf;
  int err;
  err = stat(filename, &statbuf);
  if (err < 0) return((time_t)err);
  return((time_t)(statbuf.st_mtime));
}

static int fallback_srate = 0, fallback_chans = 0, fallback_format = MUS_UNKNOWN;

void set_fallback_srate(int sr) {fallback_srate = sr;}
void set_fallback_chans(int ch) {fallback_chans = ch;}
void set_fallback_format(int fr) {fallback_format = fr;}

static file_info *make_file_info_1(const char *fullname)
{
  file_info *hdr;
  hdr = (file_info *)CALLOC(1, sizeof(file_info));
  hdr->name = copy_string(fullname);
  hdr->type = mus_sound_header_type(fullname);
  if (hdr->type == MUS_RAW)
    mus_header_raw_defaults(&(hdr->srate), &(hdr->chans), &(hdr->format));
  else
    {
      hdr->srate = mus_sound_srate(fullname);
      hdr->chans = mus_sound_chans(fullname);
      hdr->format = mus_sound_data_format(fullname);
    }
  if (!(MUS_DATA_FORMAT_OK(hdr->format))) hdr->format = fallback_format;
  if (hdr->srate <= 0) {if (fallback_srate > 0) hdr->srate = fallback_srate; else hdr->srate = 1;}
  if (hdr->chans <= 0) {if (fallback_chans > 0) hdr->chans = fallback_chans; else hdr->chans = 1;}
  hdr->samples = mus_sound_samples(fullname); /* total samples, not per channel */
  hdr->data_location = mus_sound_data_location(fullname);
  hdr->comment = mus_sound_comment(fullname);
  hdr->loops = mus_sound_loop_info(fullname);
  return(hdr);
}

file_info *copy_header(const char *fullname, file_info *ohdr)
{
  file_info *hdr;
  int i;
  hdr = (file_info *)CALLOC(1, sizeof(file_info));
  hdr->name = copy_string(fullname);
  hdr->comment = NULL;
  hdr->samples = ohdr->samples;
  if (ohdr)
    {
      hdr->data_location = ohdr->data_location;
      hdr->srate = ohdr->srate;
      hdr->chans = ohdr->chans;
      hdr->format = ohdr->format;
      hdr->type = ohdr->type;
      if (ohdr->loops)
	{
	  hdr->loops = (int *)CALLOC(MUS_LOOP_INFO_SIZE, sizeof(int));
	  for (i = 0; i < MUS_LOOP_INFO_SIZE; i++) 
	    hdr->loops[i] = ohdr->loops[i];
	}
    }
  return(hdr);
}

static file_info *translate_file(const char *filename, int type)
{
  file_info *hdr = NULL;
  char *newname;
  int *loops = NULL;
  int err, len, fd;
  len = strlen(filename);
  loops = mus_sound_loop_info(filename);
  newname = (char *)CALLOC(len + 5, sizeof(char));
  mus_snprintf(newname, len + 5, "%s.snd", filename);
  /* too many special cases to do anything smart here -- I'll just tack on '.snd' */
  /* before calling the translator we need to check for write-access to the current directory,
   * and if none, try to find a temp directory we can write to.
   *
   * loop info across translation: 4-Dec-01
   */
  fd = CREAT(newname, 0666);
  if (fd == -1)
    {
      char *tempname;
      tempname = snd_tempnam();
      fd = CREAT(tempname, 0666);
      if (fd == -1)
	{
	  snd_error(_("can't write translation temp file: %s or %s!"),
		    newname, tempname);
	  FREE(newname);
	  FREE(tempname);
	  if (loops) FREE(loops);
	  return(NULL);
	}
      FREE(newname);
      newname = copy_string(tempname);
      FREE(tempname);
    }
  snd_close(fd, newname);
  err = snd_translate(filename, newname, type);
  if (err == MUS_NO_ERROR)
    {
      err = mus_header_read(newname);
      if (err == MUS_NO_ERROR)
	{
	  hdr = make_file_info_1(newname);
	  if (hdr->loops == NULL) hdr->loops = loops;
	  loops = NULL;
	  ss->pending_change = copy_string(newname);
	}
    }
  else snd_remove(newname, REMOVE_FROM_CACHE);
  if (newname) FREE(newname);
  if (loops) FREE(loops);
  return(hdr);
}

static XEN open_raw_sound_hook, bad_header_hook;
#if (!USE_NO_GUI)
static char *raw_data_explanation(const char *filename, file_info *hdr);
#endif

file_info *make_file_info(const char *fullname)
{
  file_info *hdr = NULL;
  if (mus_file_probe(fullname))
    {
      int type = MUS_UNSUPPORTED, format = MUS_UNKNOWN;
      type = mus_sound_header_type(fullname);
      if (type == MUS_ERROR) 
	type = mus_header_type();
#if (!USE_NO_GUI)
      else
	{
	  int sr = 0, ch = 0;
	  sr = mus_sound_srate(fullname);
	  ch = mus_sound_chans(fullname);
	  if ((fallback_srate > 0) && ((sr <= 0) || (sr > 100000000))) sr = fallback_srate;
	  if ((fallback_chans > 0) && ((ch >= 256) || (ch <= 0))) ch = fallback_chans;
	  if ((sr <= 0) || (sr > 100000000) ||
	      (ch >= 256) || (ch <= 0))
	    {
	      if ((XEN_HOOKED(bad_header_hook)) &&
		  (XEN_TRUE_P(run_or_hook(bad_header_hook,
					  XEN_LIST_1(C_TO_XEN_STRING(fullname)),
					  S_bad_header_hook))))
		return(NULL);
	      
	      type = mus_header_type();
	      if ((type != MUS_MIDI_SAMPLE_DUMP) && 
		  (type != MUS_IEEE) &&
		  (type != MUS_MUS10) && 
		  (type != MUS_HCOM))
		{
		  char *tmp;
		  file_info *tmp_hdr;
		  tmp_hdr = make_file_info_1(fullname);
		  tmp = raw_data_explanation(fullname, tmp_hdr);
		  hdr = raw_data_dialog_to_file_info(fullname, tmp);
		  if (tmp) FREE(tmp);
		  if (tmp_hdr) tmp_hdr = free_file_info(tmp_hdr);
		  return(hdr);
		}
	    }
	}
#endif
      if (type == MUS_RAW)
	{
	  XEN res = XEN_FALSE;
	  int res_loc = -1;
	  XEN procs, arg1;
	  int len, srate, chans, data_format;
	  off_t data_location, bytes;

	  if (ss->reloading_updated_file != 0)
	    {
	      /* choices already made, so just send back a header that reflects those choices */
	      if (mus_file_probe(fullname))
		return(make_file_info_1(fullname));
	      else
		{
		  snd_error(_("can't find raw (headerless) file %s: %s"),
			    fullname, strerror(errno));
		  return(NULL);
		}
	    }
	  if (XEN_HOOKED(open_raw_sound_hook))
	    {
	      procs = XEN_HOOK_PROCEDURES(open_raw_sound_hook);
	      arg1 = C_TO_XEN_STRING(fullname);
	      while(XEN_NOT_NULL_P(procs))
		{
		  res = XEN_CALL_2(XEN_CAR(procs), 
				   arg1, 
				   res, 
				   S_open_raw_sound_hook);
		  if (res_loc != -1) snd_unprotect_at(res_loc);
		  res_loc = snd_protect(res);
		  procs = XEN_CDR(procs);
		}
	    }
	  if (XEN_LIST_P(res)) /* empty list ok here -> accept all current defaults */
	    {
	      len = XEN_LIST_LENGTH(res);
	      mus_header_raw_defaults(&srate, &chans, &data_format);
	      if (len > 0) chans = XEN_TO_C_INT(XEN_CAR(res));
	      if (len > 1) srate = XEN_TO_C_INT(XEN_CADR(res));
	      if (len > 2) 
		{
		  XEN df;
		  df = XEN_LIST_REF(res, 2);
		  data_format = XEN_TO_C_INT(df);
		}
	      if (len > 3) data_location = XEN_TO_C_OFF_T(XEN_LIST_REF(res, 3)); else data_location = 0;
	      if (len > 4) bytes = XEN_TO_C_OFF_T(XEN_LIST_REF(res, 4)); else bytes = mus_sound_length(fullname) - data_location;
	      mus_header_set_raw_defaults(srate, chans, data_format);
	      mus_sound_override_header(fullname, srate, chans, data_format, 
					MUS_RAW, data_location,
					mus_bytes_to_samples(data_format, bytes));
	      if (res_loc != -1) snd_unprotect_at(res_loc);	      
	      hdr = (file_info *)CALLOC(1, sizeof(file_info));
	      hdr->name = copy_string(fullname);
	      hdr->type = MUS_RAW;
	      hdr->srate = mus_sound_srate(fullname);
	      hdr->chans = mus_sound_chans(fullname);
	      hdr->format = mus_sound_data_format(fullname);
	      hdr->samples = mus_sound_samples(fullname); /* total samples, not per channel */
	      hdr->data_location = mus_sound_data_location(fullname);
	      hdr->comment = NULL;
	      return(hdr);
	    }
#if (!USE_NO_GUI)
	  else 
	    {
	      char *str;
	      bool just_quit = false;
	      if (XEN_TRUE_P(res)) just_quit = true;
	      if (res_loc != -1) snd_unprotect_at(res_loc);
	      if (just_quit) return(NULL);
	      str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
	      mus_snprintf(str, PRINT_BUFFER_SIZE, _("No header found for %s"), filename_without_home_directory(fullname));
	      hdr = raw_data_dialog_to_file_info(fullname, str);
	      FREE(str);
	      return(hdr);
	    }
#endif
	}
      else
	{
	  if (MUS_HEADER_TYPE_OK(type))
	    {
	      format = mus_sound_data_format(fullname);
	      if (MUS_DATA_FORMAT_OK(format))
		hdr = make_file_info_1(fullname);
	      else hdr = translate_file(fullname, type);
	    }
	  else 
	    snd_error(_("%s does not seem to be a sound file?"), fullname);
	}
    }
  else
    {
      snd_error(_("can't find file %s: %s"), fullname, strerror(errno));
      return(NULL);
    }
  return(hdr);
}

file_info *make_temp_header(const char *fullname, int srate, int chans, off_t samples, const char *caller)
{
  file_info *hdr;
  hdr = (file_info *)CALLOC(1, sizeof(file_info));
  hdr->name = copy_string(fullname);
  hdr->samples = samples;
  hdr->data_location = 28;
  hdr->srate = srate;
  hdr->chans = chans;
  hdr->format = MUS_OUT_FORMAT;
  hdr->type = MUS_NEXT;
  /* want direct read/writes for temp files */
  hdr->comment = copy_string(caller);
  return(hdr);
}

file_info *free_file_info(file_info *hdr)
{
  if (hdr)
    {
      if (hdr->name) FREE(hdr->name);
      if (hdr->comment) FREE(hdr->comment);
      if (hdr->loops) FREE(hdr->loops);
      FREE(hdr);
    }
  return(NULL);
}

/* mus_header_read here (or stripped-down equivalent) was very slow, and is just as easy to
 * fool as an extension check (file might start with the word ".snd" or whatever).
 */

static dir *make_dir(const char *name)
{
  dir *dp;
  dp = (dir *)CALLOC(1, sizeof(dir));
  dp->files = (char **)CALLOC(32, sizeof(char *));
  dp->name = copy_string(name);
  dp->len = 0;
  dp->size = 32;
  return(dp);
}

dir *free_dir (dir *dp)
{
  if (dp->name) FREE(dp->name);
  if (dp->files)
    {
      int i;
      for (i = 0; i < dp->len; i++) 
	if (dp->files[i]) 
	  FREE(dp->files[i]);
      FREE(dp->files);
    }
  FREE(dp);
  return(NULL);
}
  
static void add_snd_file_to_dir_list(dir *dp, const char *name)
{
  dp->files[dp->len] = copy_string(name);
  dp->len++;
  if (dp->len == dp->size) 
    {
      int i;
      dp->size += 32;
      dp->files = (char **)REALLOC(dp->files, dp->size * sizeof(char *));
      for (i = dp->size - 32; i < dp->size; i++) dp->files[i] = NULL;
    }
}

static char **sound_file_extensions = NULL;
static int sound_file_extensions_size = 0;
static int sound_file_extensions_end = 0;

static void add_sound_file_extension(const char *ext)
{
  if (sound_file_extensions_end == sound_file_extensions_size)
    {
      sound_file_extensions_size += 8;
      if (sound_file_extensions == NULL)
	sound_file_extensions = (char **)CALLOC(sound_file_extensions_size, sizeof(char *));
      else sound_file_extensions = (char **)REALLOC(sound_file_extensions, sound_file_extensions_size * sizeof(char *));
    }
  sound_file_extensions[sound_file_extensions_end] = copy_string(ext);
  sound_file_extensions_end++;
}

void init_sound_file_extensions(void)
{
  add_sound_file_extension("snd");
  add_sound_file_extension("aiff");
  add_sound_file_extension("aif");
  add_sound_file_extension("wav");
  add_sound_file_extension("WAV");
  add_sound_file_extension("au");
  add_sound_file_extension("aifc");
  add_sound_file_extension("voc");
  add_sound_file_extension("wve");
  add_sound_file_extension("sf2");
}

static XEN just_sounds_hook;

bool run_just_sounds_hook(const char *name)
{
  XEN res = XEN_TRUE;
  if (XEN_HOOKED(just_sounds_hook))
    res = run_or_hook(just_sounds_hook,
		      XEN_LIST_1(C_TO_XEN_STRING(name)),
		      S_just_sounds_hook);
  return(XEN_TRUE_P(res));
}


bool sound_file_p(char *name)
{
  int i;
  char *dot, *sp;
  dot = NULL;
  for (sp = name; (*sp) != '\0'; sp++) 
    if ((*sp) == '.') 
      dot = (++sp);
  if (dot)
    for (i = 0; i < sound_file_extensions_end; i++)
      if (strcmp(dot, sound_file_extensions[i]) == 0)
	return(true);
  return(false);
}

dir *find_sound_files_in_dir(const char *name)
{
#if (!HAVE_OPENDIR)
  return(NULL);
#else
  DIR *dpos;
  dir *dp = NULL;
  if ((dpos = opendir(name)) != NULL)
    {
      struct dirent *dirp;
      dp = make_dir(name);
      while ((dirp = readdir(dpos)) != NULL)
	if (dirp->d_name[0] != '.')
	  {
	    int i;
	    char *dot, *sp;
	    dot = NULL;
	    for (sp = dirp->d_name; (*sp) != '\0'; sp++) 
	      if ((*sp) == '.') 
		dot = (++sp);
	    if (dot)
	      for (i = 0; i < sound_file_extensions_end; i++)
		if ((strcmp(dot, sound_file_extensions[i]) == 0) && 
		    (!(empty_file_p(dirp->d_name))))
		  {
		    if (run_just_sounds_hook(dirp->d_name))
		      add_snd_file_to_dir_list(dp, dirp->d_name);
		    break;
		  }
	  }
#if CLOSEDIR_VOID
      closedir(dpos);
#else
      if (closedir(dpos) != 0) 
	snd_error(_("closedir %s failed (%s)!"),
		  name, strerror(errno));
#endif
    }
  return(dp);
#endif
}

static bool names_match(char *filename, char *pattern)
{
  /* just "*" for wildcards here */
  char *sn, *sp;
  sn = filename;
  sp = pattern;
  if ((!sn) || (!sp)) 
    {
      if ((sn) || (sp)) 
	return(false); 
      else return(true);
    }
  while ((*sn) && (*sp))
    {
      if ((*sp) == '*') 
	{
	  sp++; 
	  while ((*sp) == '*') sp++; 
	  if (!(*sp)) return(true);
	  while ((*sn) && ((*sn) != (*sp))) sn++;
	  if (!(*sn)) return(false);
	}
      else 
	{
	  if ((*sn) != (*sp)) return(false);
	  sn++; 
	  sp++;
	}
    }
  return(true);
}

dir *filter_sound_files(dir *dp, char *pattern)
{
  int i;
  dir *ndp;
  ndp = make_dir("");
  for (i = 0; i < dp->len; i++)
    if (names_match(dp->files[i], pattern)) 
      add_snd_file_to_dir_list(ndp, dp->files[i]);
  return(ndp);
}

typedef struct {
  int active_sounds;
  char **names;
  int *sounds;
} active_sound_list;

static void add_sound_to_active_list (snd_info *sp, void *sptr1)
{
  active_sound_list *sptr = (active_sound_list *)sptr1;
  sptr->names[sptr->active_sounds] = sp->filename;
  sptr->sounds[sptr->active_sounds] = sp->index;
  (sptr->active_sounds)++;
}

void reflect_file_change_in_title(void)
{
  char *title_buffer = NULL;
  active_sound_list *alist;
  int i, j, len;
  alist = (active_sound_list *)CALLOC(1, sizeof(active_sound_list));
  alist->sounds = (int *)CALLOC(ss->max_sounds, sizeof(int));
  alist->names = (char **)CALLOC(ss->max_sounds, sizeof(char *));
  for_each_sound(add_sound_to_active_list, alist);
  len = snd_strlen(ss->startup_title) + 32;
  if (alist->active_sounds > 0)
    {
      if (alist->active_sounds < 4) 
	j = alist->active_sounds; 
      else j = 4;
      for (i = 0; i < j; i++)
	len += snd_strlen(filename_without_home_directory(alist->names[i]));
    }
  title_buffer = (char *)CALLOC(len, sizeof(char));
  mus_snprintf(title_buffer, len, "%s%s", 
	       ss->startup_title, 
	       ((alist->active_sounds > 0) ? ": " : ""));
  if (alist->active_sounds > 0)
    {
      if (alist->active_sounds < 4) 
	j = alist->active_sounds; 
      else j = 4;
      for (i = 0; i < j; i++)
	{
	  strcat(title_buffer, filename_without_home_directory(alist->names[i]));
	  if (i < j - 1)
	    strcat(title_buffer, ", ");
	}
      if (alist->active_sounds > 4) 
	strcat(title_buffer, "...");
    }
  set_title(title_buffer);
  FREE(title_buffer);
  FREE(alist->sounds);
  FREE(alist->names);
  FREE(alist);
}

static char *snd_opened_sound_file_name(snd_info *sp)
{
  char *newname;
  int len;
  len = strlen(sp->filename);
  newname = (char *)CALLOC(len + 5, sizeof(char));
  mus_snprintf(newname, len + 5, "%s.%s", sp->filename, XEN_FILE_EXTENSION);
  return(newname);
}

static void read_snd_opened_sound_file(snd_info *sp)
{
  char *newname;
  newname = snd_opened_sound_file_name(sp);
  if (file_write_date(newname) >= sp->write_date)
    {
#if HAVE_GUILE
      /* this file shouldn't be left in the load list -- it will confuse the save-state process 
       *   (*snd-opened-sound* is defined here but not at the saved state file reload point)
       * *snd-loaded-files* is the variable name (snd-xen.c), so we save and restore its value if possible 
       */
      XEN var = XEN_FALSE, val = XEN_FALSE;
      var = XEN_NAME_AS_C_STRING_TO_VARIABLE("*snd-loaded-files*");
      if (!(XEN_FALSE_P(var)))
	val = XEN_VARIABLE_REF(var);
      snd_load_file(newname);
      if ((!(XEN_FALSE_P(var))) && (XEN_LIST_P(val)))
	XEN_VARIABLE_SET(var, val);
#else
      snd_load_file(newname);
#endif
    }
  FREE(newname);
}

static XEN snd_opened_sound;
static XEN snd_memo_sound;
static XEN open_hook;
static XEN close_hook;

static void add_to_current_files(const char *shortname);
static void add_to_previous_files(const char *shortname, const char *fullname);

#if HAVE_GUILE_DYNAMIC_WIND
/* cleanup even if error in file lookup process */
typedef struct {
  char *filename;
  bool read_only;
} open_file_context;

static snd_info *open_file_sp = NULL;
static void before_open_file(void *context) {}

static XEN open_file_body(void *context)
{
  open_file_context *sc = (open_file_context *)context;
  open_file_sp = add_sound_window(sc->filename, sc->read_only); /* snd-xsnd.c -> make_file_info (in this file) */
  return(XEN_FALSE);
}

static void after_open_file(void *context)
{
  open_file_context *sc = (open_file_context *)context;
  if (sc->filename) FREE(sc->filename);
  FREE(sc);
}
#endif

static snd_info *snd_open_file_1 (const char *filename, bool select, bool read_only)
{
  snd_info *sp;
  char *mcf = NULL;
  int files;
  XEN res = XEN_FALSE;
  mcf = mus_expand_filename(filename);
  if (XEN_HOOKED(open_hook))
    {
      XEN fstr;
      fstr = C_TO_XEN_STRING(mcf);
      res = run_or_hook(open_hook,
			XEN_LIST_1(fstr),
			S_open_hook);
      if (XEN_TRUE_P(res))
	{
	  if (mcf) FREE(mcf);
	  return(NULL);
	}
      else
	{
	  if (XEN_STRING_P(res))  /* added 14-Aug-01 for user-supplied auto-translations */
	    {
	      if (mcf) FREE(mcf);
	      mcf = mus_expand_filename(XEN_TO_C_STRING(res));
	    }
	}
    }
#if HAVE_GUILE_DYNAMIC_WIND
  {
    open_file_context *ofc;
    ofc = (open_file_context *)CALLOC(1, sizeof(open_file_context));
    ofc->filename = mcf;
    ofc->read_only = read_only;
    scm_internal_dynamic_wind((scm_t_guard)before_open_file, 
			      (scm_t_inner)open_file_body, 
			      (scm_t_guard)after_open_file, 
			      (void *)ofc,
			      (void *)ofc);
    sp = open_file_sp; /* has to be global since we free sc during the unwind */
    mcf = NULL; /* freed above in unwind */
  }
#else
  sp = add_sound_window(mcf, read_only); /* snd-xsnd.c -> make_file_info (in this file) */
  if (mcf) FREE(mcf);
#endif
  if (sp)
    {
#if HAVE_RUBY
      XEN_VARIABLE_SET(S_snd_opened_sound, C_TO_XEN_INT(sp->index));
#else
      XEN_VARIABLE_SET(snd_opened_sound, C_TO_XEN_INT(sp->index));
      XEN_VARIABLE_SET(snd_memo_sound, C_TO_XEN_INT(sp->index)); /* backwards compatibility */
#endif
      sp->write_date = file_write_date(sp->filename);
      sp->need_update = false;
      ss->active_sounds++;
      files = ss->active_sounds;
      if (files == 1) reflect_file_open_in_menu();
      reflect_equalize_panes_in_menu(active_channels(WITHOUT_VIRTUAL_CHANNELS) > 1);
      reflect_file_change_in_title();
#if USE_MOTIF
      unlock_control_panel(sp);
#endif
      add_to_current_files(sp->short_filename);
    }
  map_over_separate_chans(channel_open_pane, NULL);
#if USE_MOTIF
  map_over_separate_chans(channel_unlock_pane, NULL);
#endif
  if (sp) 
    {
      if (select) select_channel(sp, 0);
      read_snd_opened_sound_file(sp);
      if ((sp->channel_style != CHANNELS_SEPARATE) && 
	  (sp->nchans > 1)) 
	{
	  channel_style_t val;
	  val = sp->channel_style;
	  sp->channel_style = CHANNELS_SEPARATE; 
	  if (val == CHANNELS_COMBINED)
	    combine_sound(sp);
	  else superimpose_sound(sp);
	}
    }
  return(sp);
}

snd_info *snd_open_file(const char *filename, bool read_only) {return(snd_open_file_1(filename, true, read_only));}
snd_info *snd_open_file_unselected(const char *filename) {return(snd_open_file_1(filename, false, false));}

void snd_close_file(snd_info *sp)
{
  int files, i;
  XEN res = XEN_FALSE;
  if (XEN_HOOKED(close_hook))
    res = run_or_hook(close_hook,
		      XEN_LIST_1(C_TO_XEN_INT(sp->index)),
		      S_close_hook);
  if (XEN_TRUE_P(res)) return;
  /* exit does not go through this function to clean up temps -- see snd_exit_cleanly in snd-main.c */
  if (selection_creation_in_progress()) finish_selection_creation();
  if (ss->deferred_regions > 0)
    for (i = 0; i < sp->nchans; i++)
      if (sp->chans[i]) 
	sequester_deferred_regions(sp->chans[i], -1);
  sp->inuse = SOUND_IDLE;
  for (i = 0; i < sp->nchans; i++) sp->chans[i]->squelch_update = true;
  add_to_previous_files(sp->short_filename, sp->filename);
  if (sp->playing) stop_playing_sound(sp, PLAY_CLOSE);
  if (sp->sgx) 
    {
      sp->inuse = SOUND_NORMAL;               /* needed to make sure minibuffer is actually cleared in set_minibuffer_string */
      set_minibuffer_string(sp, NULL, false); /* false = don't try to update graphs! */
      sp->inuse = SOUND_IDLE;
    }
  if (sp == selected_sound()) 
    ss->selected_sound = NO_SELECTION;
  /* if sequester_deferred_regions is in free_snd_info (moved up to this level 15-12-03)
   *   if needs an active-looking sound if its active edit op is a ptree read with an in-use closure.
   *   If the sound is set to SOUND_IDLE, the init function returns 'no-such-sound, and the
   *   subsequent read segfaults.
   */
  free_snd_info(sp);
  ss->active_sounds--;
  files = ss->active_sounds;
  if (files == 0) 
    {
      reflect_file_lack_in_menu();
      release_pending_track_states();
    }
  reflect_file_change_in_title();
  reflect_equalize_panes_in_menu(active_channels(WITHOUT_VIRTUAL_CHANNELS) > 1);
  if (!(selection_is_active())) 
    reflect_edit_without_selection_in_menu();
}


int copy_file(const char *oldname, const char *newname)
{
  /* make newname a copy of oldname */
  int ifd, ofd;
  off_t bytes, wb, total;
  char *buf = NULL;
  total = 0;
  ifd = OPEN(oldname, O_RDONLY, 0);
  if (ifd == -1) return(MUS_CANT_OPEN_FILE);
  ofd = CREAT(newname, 0666);
  if (ofd == -1) 
    {
      snd_close(ifd, oldname);
      return(MUS_CANT_OPEN_FILE);
    }
  buf = (char *)CALLOC(8192, sizeof(char));
  while ((bytes = read(ifd, buf, 8192)))
    {
      total += bytes;
      wb = write(ofd, buf, bytes);
      if (wb != bytes) 
	{
	  snd_close(ofd, newname);
	  snd_close(ifd, oldname);
	  FREE(buf); 
	  return(MUS_WRITE_ERROR);
	}
    }
  snd_close(ifd, oldname);
  wb = disk_kspace(newname);
  snd_close(ofd, newname);
  FREE(buf);
  total = total >> 10;
  if (wb < 0) 
    snd_warning("%s: %s\n", newname, strerror(errno));
  else
    if (total > wb) 
      snd_warning(_("disk is nearly full: used " PRId64 " Kbytes leaving " PRId64),
		  total, wb);
  return(MUS_NO_ERROR);
}

int move_file(const char *oldfile, const char *newfile)
{
  int err = 0;
  if ((err = (rename(oldfile, newfile))))
    {
      if (errno == EXDEV)
	{
	  err = copy_file(oldfile, newfile);
	  if (!err) 
	    snd_remove(oldfile, REMOVE_FROM_CACHE);
	}
    }
  if (err != 0)
    snd_error(_("error while overwriting %s: %s"), newfile, strerror(errno));
  return(err);
}

#define TEMP_SOUND_INDEX 123456
/* just a marker for debugging */

snd_info *make_sound_readable(const char *filename, bool post_close)
{
  /* conjure up just enough Snd structure to make this sound readable by the edit-tree readers */
  snd_info *sp;
  chan_info *cp;
  file_info *hdr = NULL;
  snd_data *sd;
  int i, fd;
  off_t len;
  /* we've already checked that filename exists */
  hdr = make_file_info_1(filename);
  sp = make_basic_snd_info(hdr->chans);
  sp->nchans = hdr->chans;
  sp->hdr = hdr;
  sp->inuse = SOUND_READER;
  initialize_control_panel(sp);
  sp->search_proc = XEN_UNDEFINED;
  sp->prompt_callback = XEN_UNDEFINED;
  sp->index = TEMP_SOUND_INDEX;
  sp->sgx = NULL;
  len = (hdr->samples) / (hdr->chans);
  for (i = 0; i < sp->nchans; i++)
    {
      cp = make_chan_info(NULL, i, sp);
      cp->editable = false;
      FREE((cp->cgx)->ax);
      FREE(cp->cgx);
      cp->cgx = NULL;
      sp->chans[i] = cp;
      add_channel_data_1(cp, hdr->srate, len, WITHOUT_GRAPH);
      cp->edits[0] = initial_ed_list(0, len - 1);
      cp->edit_size = 1;
      cp->sound_size = 1;
      fd = snd_open_read(filename); /* sends the error if any */
      if (fd != -1)
	{
	  snd_io *io;
	  mus_file_open_descriptors(fd,
				    filename,
				    hdr->format,
				    mus_sound_datum_size(filename),
				    hdr->data_location,
				    hdr->chans,
				    hdr->type);
	  io = make_file_state(fd, hdr, i, 0,
			       (post_close) ? MAX_BUFFER_SIZE : MIX_FILE_BUFFER_SIZE);
	  cp->sounds[0] = make_snd_data_file(filename, io,
					     copy_header(hdr->name, hdr),
					     DONT_DELETE_ME, cp->edit_ctr, i);
	  if (post_close) 
	    {
	      if (mus_file_close(fd) != 0)
		snd_error(_("can't close file %s: %s"), filename, strerror(errno));
	      sd = cp->sounds[0]; 
	      sd->open = FD_CLOSED; 
	      io->fd = -1;
	    }
	  /* this is not as crazy as it looks -- we've read in the first 64K (or whatever) samples,
	   * and may need this file channel for other opens, so this file can be closed until reposition_file_state_buffers
	   */
	}
    }
  sp->active = true;
  return(sp);
}

typedef struct {
  int chans, fields;
  double *axis_data;
  bool *fftp, *wavep;
} axes_data;

void *free_axes_data(void *usa)
{
  axes_data *sa = (axes_data *)usa;
  if (sa)
    {
      if (sa->axis_data) {FREE(sa->axis_data); sa->axis_data = NULL;}
      if (sa->fftp) {FREE(sa->fftp); sa->fftp = NULL;}
      if (sa->wavep) {FREE(sa->wavep); sa->wavep = NULL;}
      FREE(sa);
    }
  return(NULL);
}

enum {SA_X0, SA_X1, SA_Y0, SA_Y1, SA_XMIN, SA_XMAX, SA_YMIN, SA_YMAX};

void *make_axes_data(snd_info *sp)
{
  axes_data *sa;
  int i;
  sa = (axes_data *)CALLOC(1, sizeof(axes_data));
  sa->chans = sp->nchans;
  sa->fields = 8;
  sa->axis_data = (double *)CALLOC(sa->fields * sa->chans, sizeof(double));
  sa->fftp = (bool *)CALLOC(sa->chans, sizeof(bool));
  sa->wavep = (bool *)CALLOC(sa->chans, sizeof(bool));
  for (i = 0; i < sa->chans; i++)
    {
      chan_info *cp;
      axis_info *ap;
      int loc;
      cp = sp->chans[i];
      ap = cp->axis;
      loc = i * sa->fields;
      sa->axis_data[loc + SA_X0] = ap->x0;
      sa->axis_data[loc + SA_X1] = ap->x1;
      sa->axis_data[loc + SA_Y0] = ap->y0;
      sa->axis_data[loc + SA_Y1] = ap->y1;
      sa->axis_data[loc + SA_XMIN] = ap->xmin;
      sa->axis_data[loc + SA_XMAX] = ap->xmax;
      sa->axis_data[loc + SA_YMIN] = ap->ymin;
      sa->axis_data[loc + SA_YMAX] = ap->ymax;
      sa->wavep[i] = cp->graph_time_p;
      sa->fftp[i] = cp->graph_transform_p;
    }
  return((void *)sa);
}

void restore_axes_data(snd_info *sp, void *usa, Float new_duration, bool need_edit_history_update)
{
  axes_data *sa = (axes_data *)usa;
  int i, j;
  for (i = 0, j = 0; i < sp->nchans; i++)
    {
      Float old_duration;
      chan_info *cp;
      axis_info *ap;
      int loc;
      cp = sp->chans[i];
      loc = j * sa->fields;
      old_duration = sa->axis_data[loc + SA_X1] - sa->axis_data[loc + SA_X0];  /* old duration is x1 - x0 */
      if (new_duration < sa->axis_data[loc + SA_X0])                           /* new duration < old x0 */
	sa->axis_data[loc + SA_X0] = new_duration - old_duration;              /* try to maintain old window size */
      if (sa->axis_data[loc + SA_X0] < 0.0) 
	sa->axis_data[loc + SA_X0] = 0.0;
      if (new_duration < sa->axis_data[loc + SA_X1]) 
	sa->axis_data[loc + SA_X1] = new_duration;                             /* new x1 */
      sa->axis_data[loc + SA_XMAX] = new_duration;                             /* new xmax */
      ap = cp->axis;
      ap->xmin = sa->axis_data[loc + SA_XMIN];
      ap->xmax = sa->axis_data[loc + SA_XMAX];
      ap->ymin = sa->axis_data[loc + SA_YMIN];
      ap->ymax = sa->axis_data[loc + SA_YMAX];
      ap->y_ambit = ap->ymax - ap->ymin;
      ap->x_ambit = ap->xmax - ap->xmin;
      set_axes(cp,
	       sa->axis_data[loc + SA_X0], 
	       sa->axis_data[loc + SA_X1], 
	       sa->axis_data[loc + SA_Y0], 
	       sa->axis_data[loc + SA_Y1]);
      update_graph(cp); /* get normalized state before messing with it */
      if (sa->fftp[j]) 
	fftb(cp, true); 
      if (!(sa->wavep[j])) 
	waveb(cp, false); 
      if (need_edit_history_update) 
	reflect_edit_history_change(cp);
      if (j < (sa->chans - 1)) j++;
    }
}

static void copy_chan_info(chan_info *ncp, chan_info *ocp)
{
  ncp->cursor_on = ocp->cursor_on;
  ncp->cursor_style = ocp->cursor_style;
  ncp->cursor_size = ocp->cursor_size;
  ncp->spectro_x_scale = ocp->spectro_x_scale;
  ncp->spectro_y_scale = ocp->spectro_y_scale;
  ncp->spectro_z_scale = ocp->spectro_z_scale;
  ncp->spectro_z_angle = ocp->spectro_z_angle;
  ncp->spectro_x_angle = ocp->spectro_x_angle;
  ncp->spectro_y_angle = ocp->spectro_y_angle;
  ncp->spectro_cutoff = ocp->spectro_cutoff;
  ncp->spectro_start = ocp->spectro_start;
  ncp->lin_dB = ocp->lin_dB;
  ncp->min_dB = ocp->min_dB;
  ncp->fft_window_beta = ocp->fft_window_beta;
  ncp->beats_per_minute = ocp->beats_per_minute;
  ncp->show_y_zero = ocp->show_y_zero;
  ncp->show_grid = ocp->show_grid;
  ncp->grid_density = ocp->grid_density;
  ncp->show_sonogram_cursor = ocp->show_sonogram_cursor;
  ncp->show_marks = ocp->show_marks;
  ncp->wavo_hop = ocp->wavo_hop;
  ncp->wavo_trace = ocp->wavo_trace;
  ncp->zero_pad = ocp->zero_pad;
  ncp->x_axis_style = ocp->x_axis_style;
  ncp->wavelet_type = ocp->wavelet_type;
  ncp->verbose_cursor = ocp->verbose_cursor;
  ncp->max_transform_peaks = ocp->max_transform_peaks;
  ncp->show_transform_peaks = ocp->show_transform_peaks;
  ncp->show_axes = ocp->show_axes;
  ncp->time_graph_style = ocp->time_graph_style;
  ncp->lisp_graph_style = ocp->lisp_graph_style;
  ncp->transform_graph_style = ocp->transform_graph_style;
  ncp->fft_log_frequency = ocp->fft_log_frequency;
  ncp->fft_log_magnitude = ocp->fft_log_magnitude;
  ncp->transform_size = ocp->transform_size;
  ncp->transform_graph_type = ocp->transform_graph_type;
  ncp->fft_window = ocp->fft_window;
  ncp->time_graph_type = ocp->time_graph_type;
  ncp->dot_size = ocp->dot_size;
  ncp->transform_normalization = ocp->transform_normalization;
  ncp->transform_type = ocp->transform_type;
  ncp->show_mix_waveforms = ocp->show_mix_waveforms;
  ncp->spectro_hop = ocp->spectro_hop;
  ncp->graphs_horizontal = ocp->graphs_horizontal;
  ncp->cursor_proc = ocp->cursor_proc;
  if (XEN_BOUND_P(ncp->cursor_proc)) 
    ncp->cursor_proc_loc = snd_protect(ncp->cursor_proc);
  else ncp->cursor_proc_loc = -1;
}

static void copy_snd_info(snd_info *nsp, snd_info *osp)
{
  nsp->speed_control_style = osp->speed_control_style;
  nsp->speed_control_tones = osp->speed_control_tones;
  nsp->expand_control_length = osp->expand_control_length;
  nsp->expand_control_ramp = osp->expand_control_ramp;
  nsp->expand_control_hop = osp->expand_control_hop;
  nsp->expand_control_jitter = osp->expand_control_jitter;
  nsp->contrast_control_amp = osp->contrast_control_amp;
  nsp->reverb_control_feedback = osp->reverb_control_feedback;
  nsp->reverb_control_lowpass = osp->reverb_control_lowpass;
  nsp->reverb_control_decay = osp->reverb_control_decay;
  nsp->selected_channel = osp->selected_channel;
  nsp->channel_style = osp->channel_style;
  nsp->sync = osp->sync;
  nsp->cursor_follows_play = osp->cursor_follows_play;
  nsp->search_tree = osp->search_tree;
  osp->search_tree = NULL;
  nsp->search_expr = osp->search_expr;
  osp->search_expr = NULL;
  nsp->search_proc = osp->search_proc;
  if (XEN_BOUND_P(nsp->search_proc)) 
    nsp->search_proc_loc = snd_protect(nsp->search_proc);
}

static snd_info *sound_store_chan_info(snd_info *sp)
{
  chan_info **cps;
  snd_info *nsp;
  int i;
  nsp = (snd_info *)CALLOC(1, sizeof(snd_info));
  cps = (chan_info **)CALLOC(sp->nchans, sizeof(chan_info *));
  nsp->chans = cps;
  nsp->nchans = sp->nchans;
  copy_snd_info(nsp, sp);
  for (i = 0; i < sp->nchans; i++)
    {
      cps[i] = (chan_info *)CALLOC(1, sizeof(chan_info));
      copy_chan_info(cps[i], sp->chans[i]);
    }
  return(nsp);
}

static void sound_restore_chan_info(snd_info *nsp, snd_info *osp)
{
  int i;
  chan_info **cps;
  cps = osp->chans;
  copy_snd_info(nsp, osp);
  for (i = 0; i < nsp->nchans; i++)
    {
      copy_chan_info(nsp->chans[i], cps[i]);
      if (XEN_BOUND_P(cps[i]->cursor_proc))
	{
	  snd_unprotect_at(cps[i]->cursor_proc_loc);
	  cps[i]->cursor_proc = XEN_UNDEFINED;
	  cps[i]->cursor_proc_loc = -1;
	}
    }
  if (XEN_BOUND_P(osp->search_proc))
    {
      snd_unprotect_at(osp->search_proc_loc);
      osp->search_proc_loc = -1;
      osp->search_proc = XEN_UNDEFINED;
    }
}

static XEN update_hook;

static snd_info *snd_update_1(snd_info *sp, const char *ur_filename)
{
  /* we can't be real smart here because the channel number may have changed and so on */
  int i, old_srate, old_chans, old_format, sp_chans, old_index, gc_loc = -1;
  channel_style_t old_channel_style;
  bool read_only, old_raw;
  void *sa;
  snd_info *nsp = NULL;
  char *filename;
  void *ms;
  snd_info *saved_sp;
  struct ctrl_state *saved_controls;
  off_t *old_cursors;
  XEN update_hook_result = XEN_FALSE;

  if (XEN_HOOKED(update_hook))
    {
      /* #t => return without updating (not recommended!!), proc of 1 arg will be evaluated after update is complete */
      update_hook_result = run_or_hook(update_hook, 
				       XEN_LIST_1(C_TO_XEN_INT(sp->index)),
				       S_update_hook);
      if (XEN_TRUE_P(update_hook_result)) return(sp);
      if (XEN_PROCEDURE_P(update_hook_result))
	{
	  if (XEN_REQUIRED_ARGS_OK(update_hook_result, 1))
	    gc_loc = snd_protect(update_hook_result);
	  else XEN_BAD_ARITY_ERROR(S_update_hook, 0, update_hook_result, S_update_hook " function result should require 1 arg");
	}
    }

  filename = copy_string(ur_filename);
  read_only = sp->read_only;
  sa = make_axes_data(sp);
  old_raw = (sp->hdr->type == MUS_RAW);
  if (old_raw)
    {
      mus_header_raw_defaults(&old_srate, &old_chans, &old_format);
      mus_header_set_raw_defaults(sp->hdr->srate, sp->hdr->chans, sp->hdr->format);
    }
  sp_chans = sp->nchans;
  old_index = sp->index;
  old_channel_style = sp->channel_style;
  if (sp->channel_style != CHANNELS_SEPARATE)
    set_sound_channel_style(sp, CHANNELS_SEPARATE);
  ms = (void *)sound_store_marks(sp);
  save_controls(sp);
  saved_controls = sp->saved_controls;
  sp->saved_controls = NULL;
  saved_sp = sound_store_chan_info(sp);
  old_cursors = (off_t *)CALLOC(sp_chans, sizeof(off_t));
  /* peak-env code saves the current peak-envs on exit (snd_close), but in this case, that
   *   data is known to be out-of-date.  Since we'll be freeing it eventually anyway, we
   *   do it first here, and the peak-env update-hook clobbers the existing files
   */
  for (i = 0; i < sp_chans; i++) 
    {
      chan_info *ncp;
      ncp = sp->chans[i];
      old_cursors[i] = CURSOR(ncp);
      if (ncp->amp_envs)
	{
	  int k;
	  for (k = 0; k < ncp->edit_size; k++) 
	    ncp->amp_envs[k] = free_amp_env(ncp, k);
	  FREE(ncp->amp_envs);
	  ncp->amp_envs = NULL;
	}
    }
  snd_close_file(sp);
  /* no mus_sound_forget here because we may be simply re-interpreting the existing data (set! (data-format) ...) etc */
  /* this normalizes the fft/lisp/wave state so we need to reset it after reopen */
  alert_new_file();
  ss->reloading_updated_file = (old_index + 1);
  nsp = snd_open_file(filename, read_only);
  ss->reloading_updated_file = 0;
  if (old_raw)
    mus_header_set_raw_defaults(old_srate, old_chans, old_format);
  if (nsp)
    {
      nsp->saved_controls = saved_controls;
      if (saved_controls) restore_controls(nsp);
      if (nsp->nchans == sp_chans) sound_restore_chan_info(nsp, saved_sp);
      restore_axes_data(nsp, sa, mus_sound_duration(filename), false);
      sound_restore_marks(nsp, ms);
      for (i = 0; i < nsp->nchans; i++) 
	update_graph(nsp->chans[i]);
      for (i = 0; (i < nsp->nchans) && (i < sp_chans); i++) CURSOR(nsp->chans[i]) = old_cursors[i];
      if ((nsp->nchans > 1) && (old_channel_style != CHANNELS_SEPARATE))
	set_sound_channel_style(nsp, old_channel_style);
    }
  FREE(old_cursors);

  if (XEN_PROCEDURE_P(update_hook_result))
    {
      XEN_CALL_1(update_hook_result,
		 (nsp) ? C_TO_XEN_INT(nsp->index) : XEN_FALSE,
		 "procedure returned by " S_update_hook);
      if (gc_loc != -1) snd_unprotect_at(gc_loc);
      gc_loc = -1;
    }
  if (saved_sp)
    {
      for (i = 0; i < saved_sp->nchans; i++)
	if (saved_sp->chans[i]) FREE(saved_sp->chans[i]);
      FREE(saved_sp->chans);
      FREE(saved_sp);
    }
  sa = free_axes_data(sa);
  FREE(filename);
  return(nsp);
}

snd_info *snd_update(snd_info *sp)
{
  Latus app_x, app_y;
  if (sp->edited_region) return(sp);
  if (mus_file_probe(sp->filename) == 0)
    {
      /* user deleted file while editing it? */
      report_in_minibuffer_and_save(sp, _("%s no longer exists!"), sp->short_filename);
      return(sp);
    }
  app_x = widget_width(MAIN_SHELL(ss));
  app_y = widget_height(MAIN_SHELL(ss));
  sp = snd_update_1(sp, sp->filename);
  if (sp)
    report_in_minibuffer(sp, _("updated %s"), sp->short_filename);
  else snd_error(_("update failed!"));
  set_widget_size(MAIN_SHELL(ss), app_x, app_y);
  return(sp);
}

static XEN after_save_as_hook;

void run_after_save_as_hook(snd_info *sp, const char *already_saved_as_name, bool from_save_as_dialog)
{
  if (XEN_HOOKED(after_save_as_hook))
    {
      char *fullname;
      fullname = mus_expand_filename(already_saved_as_name);
      run_progn_hook(after_save_as_hook,
		     XEN_LIST_3(C_TO_XEN_INT(sp->index),
				C_TO_XEN_STRING(fullname),
				C_TO_XEN_BOOLEAN(from_save_as_dialog)),
		     S_after_save_as_hook);
      FREE(fullname);
    }
}



/* View:Files lists */

static int prevfile_size = 0;
static int curfile_size = 0;
static char **curnames = NULL;
static char **prevnames = NULL;
static char **prevfullnames = NULL;
static int *prevtimes = NULL;
static int curfile_end = 0;
static int prevfile_end = -1;
static int max_curfile_end = 0;
static int max_prevfile_end = -1;
static int prevtime = 0;

char *get_prevname(int n) {return(prevnames[n]);}
char *get_prevfullname(int n) {return(prevfullnames[n]);}
int get_max_prevfile_end(void) {return(max_prevfile_end);}
void set_max_prevfile_end(int n) {max_prevfile_end = n;}
int get_prevfile_end(void) {return(prevfile_end);}
int get_max_curfile_end(void) {return(max_curfile_end);}
void set_max_curfile_end(int n) {max_curfile_end = n;}
int get_curfile_end(void) {return(curfile_end);}
int get_curfile_size(void) {return(curfile_size);}
int get_prevfile_size(void) {return(prevfile_size);}

char *get_curfullname(int pos)
{
  snd_info *sp;
  sp = find_sound(curnames[pos], 0);
  if (sp) return(sp->filename);
  return(NULL);
}

char *view_curfiles_name(int pos)
{
  return(curnames[pos]);
}

void view_curfiles_play(int pos, bool play)
{
  snd_info *sp;
  sp = find_sound(curnames[pos], 0);
  if (sp)
    {
      if (sp->playing) stop_playing_sound(sp, PLAY_BUTTON_UNSET);
      if (play)
	play_sound(sp, 0, NO_END_SPECIFIED, IN_BACKGROUND, AT_CURRENT_EDIT_POSITION);
      else set_play_button(sp, false);
    }
}

void view_curfiles_select(int pos)
{
  snd_info *sp, *osp;
  curfile_highlight(pos);
  sp = find_sound(curnames[pos], 0);
  osp = any_selected_sound();
  if (sp != osp)
    {
      select_channel(sp, 0);
#if USE_MOTIF
      equalize_sound_panes(sp, sp->chans[0], false);
#endif
      /* goto_graph(sp->chans[0]); */
    }
}

static XEN previous_files_select_hook;

void view_prevfiles_select(int pos)
{
  XEN res = XEN_FALSE;
  if (XEN_HOOKED(previous_files_select_hook))
    res = run_or_hook(previous_files_select_hook,
		      XEN_LIST_1(C_TO_XEN_STRING(prevfullnames[pos])),
		      S_previous_files_select_hook);
  if (XEN_NOT_TRUE_P(res))
    {
      snd_info *sp;
      sp = snd_open_file(prevfullnames[pos], false);
      if (sp) select_channel(sp, 0); 
    }
}

bool view_prevfiles_play(int pos, bool play)
{
  static snd_info *play_sp;
  if (play)
    {
      if (play_sp)
	{
	  if (play_sp->playing) return(true); /* can't play two of these at once */
	  if ((prevnames[pos] == NULL) || (strcmp(play_sp->short_filename, prevnames[pos]) != 0))
	    {
	      completely_free_snd_info(play_sp);
	      play_sp = NULL;
	    }
	}
      if ((!play_sp) && (prevfullnames[pos]))
	{
	  if (mus_file_probe(prevfullnames[pos]))
	    play_sp = make_sound_readable(prevfullnames[pos], false);
	  else snd_error(_("play previous file: can't find %s: %s"), prevfullnames[pos], strerror(errno));
	}
      if (play_sp)
	{
	  play_sp->short_filename = prevnames[pos];
	  play_sp->filename = NULL;
	  play_sound(play_sp, 0, NO_END_SPECIFIED, IN_BACKGROUND, AT_CURRENT_EDIT_POSITION);
	}
      else return(true); /* can't find or setup file */
    }
  else
    { /* play toggled off */
      if ((play_sp) && (play_sp->playing)) 
	stop_playing_sound(play_sp, PLAY_BUTTON_UNSET);
    }
  return(false);
}

static void file_unprevlist(const char *filename)
{
  int i;
  i = find_prevfile_regrow(filename);
  if (i != -1)
    {
      int j;
      FREE(prevnames[i]);
      prevnames[i] = NULL;
      FREE(prevfullnames[i]);
      prevfullnames[i] = NULL;
      for (j = i; j < prevfile_end; j++) 
	{
	  prevnames[j] = prevnames[j + 1];
	  prevfullnames[j] = prevfullnames[j + 1]; 
	  prevtimes[j] = prevtimes[j + 1];
	}
      prevnames[prevfile_end] = NULL; 
      prevfullnames[prevfile_end] = NULL; 
      prevfile_end--;
    }
}

static void file_prevlist(const char *filename, const char *fullname)
{
  int k;
  if (find_prevfile_regrow(filename) != -1) return;
  prevfile_end++;
  if (prevfile_end == prevfile_size)
    {
      int new_size;
      new_size = prevfile_size + 32;
      if (prevfile_size == 0)
	{
	  prevnames = (char **)CALLOC(new_size, sizeof(char *));
	  prevfullnames = (char **)CALLOC(new_size, sizeof(char *));
	  prevtimes = (int *)CALLOC(new_size, sizeof(int));
	}
      else
	{
	  int i;
	  prevnames = (char **)REALLOC(prevnames, new_size * sizeof(char *));
	  prevfullnames = (char **)REALLOC(prevfullnames, new_size * sizeof(char *));
	  prevtimes = (int *)REALLOC(prevtimes, new_size * sizeof(int));
	  for (i = prevfile_size; i < new_size; i++) 
	    {
	      prevnames[i] = NULL; 
	      prevfullnames[i] = NULL; 
	      prevtimes[i] = 0;
	    }
	}
      make_prev_name_row(prevfile_size, new_size);
      prevfile_size = new_size;
    }
  for (k = prevfile_end; k > 0; k--) 
    {
      prevnames[k] = prevnames[k - 1]; 
      prevfullnames[k] = prevfullnames[k - 1];
      prevtimes[k] = prevtimes[k - 1];
    }
  prevnames[0] = copy_string(filename);
  prevfullnames[0] = copy_string(fullname);
  prevtimes[0] = prevtime++;
  if (max_prevfile_end < prevfile_end) 
    max_prevfile_end = prevfile_end;
}

void add_directory_to_prevlist(const char *dirname)
{
  dir *sound_files = NULL;
  sound_files = find_sound_files_in_dir(dirname);
  if ((sound_files) && (sound_files->len > 0))
    {
      char *fullpathname = NULL;
      char **fullnames;
      int i, end;
      fullpathname = (char *)CALLOC(FILENAME_MAX, sizeof(char));
      strcpy(fullpathname, dirname);
      if (dirname[strlen(dirname) - 1] != '/') 
	strcat(fullpathname, "/");
      end = strlen(fullpathname);
      fullnames = (char **)CALLOC(sound_files->len, sizeof(char *));
      for (i = 0; i < sound_files->len; i++) 
	{
	  fullnames[i] = copy_string(strcat(fullpathname, sound_files->files[i]));
	  fullpathname[end] = '\0';
	}
      for (i = 0; i < sound_files->len; i++) 
	{
	  file_prevlist(sound_files->files[i], fullnames[i]);
	  FREE(fullnames[i]); 
	  fullnames[i] = NULL;
	}
      FREE(fullnames);
      free_dir(sound_files);
      FREE(fullpathname);
    }
  if (view_files_dialog_is_active()) make_prevfiles_list();
}

static void add_to_previous_files(const char *shortname, const char *fullname)
{
  int i;
  file_prevlist(shortname, fullname);
  i = find_curfile_regrow(shortname);
  if (i != -1)
    {
      int j;
      if (curnames[i]) FREE(curnames[i]);
      curnames[i] = NULL;
      for (j = i; j < curfile_end - 1; j++)
	curnames[j] = curnames[j + 1];
      curnames[curfile_end - 1] = NULL;
      curfile_end--;
    }
  if (view_files_dialog_is_active())
    {
      make_curfiles_list();
      make_prevfiles_list();
    }
}

void init_curfiles(int size)
{
  if (curfile_size == 0)
    {
      curfile_size = size;
      curnames = (char **)CALLOC(curfile_size, sizeof(char *));
    }
}

static void add_to_current_files(const char *shortname)
{
  if (curfile_end == curfile_size)
    {
      int i, new_size;
      new_size = curfile_size + 32;
      if (curfile_size == 0)
	curnames = (char **)CALLOC(new_size, sizeof(char *));
      else
	{
	  curnames = (char **)REALLOC(curnames, new_size * sizeof(char *));
	  for (i = curfile_size; i < new_size; i++) 
	    curnames[i] = NULL; 
	}
      make_cur_name_row(curfile_size, new_size);
      curfile_size = new_size;
    }
  curnames[curfile_end] = copy_string(shortname);
  curfile_end++;
  if (max_curfile_end < curfile_end)
    max_curfile_end = curfile_end;
  file_unprevlist(shortname);
  if (view_files_dialog_is_active())
    {
      make_curfiles_list();
      make_prevfiles_list();
    }
}

void init_prevfiles(int size)
{
  if (prevfile_size == 0)
    {
      prevfile_size = size;
      prevnames = (char **)CALLOC(prevfile_size, sizeof(char *));
      prevfullnames = (char **)CALLOC(prevfile_size, sizeof(char *));
      prevtimes = (int *)CALLOC(prevfile_size, sizeof(int));
    }
}

int find_curfile_regrow(const char *shortname)
{
  int i;
  for (i = 0; i < curfile_end; i++)
    if (strcmp(curnames[i], shortname) == 0) 
      return(i);
  return(-1);
}

int find_prevfile_regrow(const char *shortname)
{
  int i;
  if (prevnames)
    for (i = 0; i <= prevfile_end; i++)
      if (strcmp(prevnames[i], shortname) == 0) 
	return(i);
  return(-1);
}

void save_prevlist(FILE *fd)
{
  int i;
  if (prevfullnames)
    for (i = 0; i <= prevfile_end; i++)
#if HAVE_RUBY
      fprintf(fd, "%s \"%s\"\n",
	      xen_scheme_procedure_to_ruby(S_preload_file),
#else
      fprintf(fd, "(%s \"%s\")\n",
	      S_preload_file,
#endif
	      prevfullnames[i]);
}

void clear_prevlist(void)
{
  int i;
  if (prevnames)
    {
      for (i = 0; i < prevfile_size; i++)
	if (prevnames[i]) 
	  {
	    FREE(prevnames[i]); 
	    prevnames[i] = NULL;
	    FREE(prevfullnames[i]); 
	    prevfullnames[i] = NULL;
	  }
      prevfile_end = -1;
      prevtime = 0;
      make_prevfiles_list();
    }
}

void update_prevlist(void)
{
  /* here we need the file's full name */
  if (prevnames)
    {
      int i, j;
      for (i = 0; i <= prevfile_end; i++)
	if (prevnames[i]) 
	  {
	    int fd;
	    fd = OPEN(prevfullnames[i], O_RDONLY, 0);
	    if (fd == -1) 
	      {
		FREE(prevnames[i]); 
		prevnames[i] = NULL;
		FREE(prevfullnames[i]); 
		prevfullnames[i] = NULL;
	      }
	    else snd_close(fd, prevfullnames[i]);
	  }
      for (i = 0, j = 0; i <= prevfile_end; i++)
	if (prevnames[i])
	  {
	    if (i != j) 
	      {
		prevnames[j] = prevnames[i]; 
		prevnames[i] = NULL;
		prevfullnames[j] = prevfullnames[i];
		prevfullnames[i] = NULL;
		prevtimes[j] = prevtimes[i];
	      }
	    j++;
	  }
      prevfile_end = j - 1;
    }
}


/* sort prevfiles list by name (aphabetical), or some number (date written, size, entry order, srate? type?) */

typedef struct {
  int vals, times;
  off_t samps;
  char *a1, *a2;
} heapdata;

static int alphabet_compare(const void *a, const void *b)
{
  heapdata *d1 = *(heapdata **)a;
  heapdata *d2 = *(heapdata **)b;
  return(strcmp(d1->a1, d2->a1));
}

static int greater_compare(const void *a, const void *b)
{
  heapdata *d1 = *(heapdata **)a;
  heapdata *d2 = *(heapdata **)b;
  if (d1->samps > d2->samps) 
    return(1); 
  else 
    {
      if (d1->samps == d2->samps) 
	return(0); 
      else return(-1);
    }
}

static int less_compare(const void *a, const void *b)
{
  heapdata *d1 = *(heapdata **)a;
  heapdata *d2 = *(heapdata **)b;
  if (d1->vals < d2->vals) 
    return(1); 
  else 
    {
      if (d1->vals == d2->vals) 
	return(0); 
      else return(-1);
    }
}

void make_prevfiles_list_1(void)
{
  update_prevlist();
  if (prevfile_end >= 0)
    {
      heapdata **data;
      int i, len;
      len = prevfile_end + 1;
      data = (heapdata **)CALLOC(len, sizeof(heapdata *));
      for (i = 0; i < len; i++)
	{
	  data[i] = (heapdata *)CALLOC(1, sizeof(heapdata));
	  data[i]->a1 = prevnames[i];
	  data[i]->a2 = prevfullnames[i];
	  data[i]->times = prevtimes[i];
	}
      switch (previous_files_sort(ss))
	{
	case 0: 
	  break;
	case 1: 
	  qsort((void *)data, prevfile_end + 1, sizeof(heapdata *), alphabet_compare);
	  break;
	case 2:
	  for (i = 0; i <= prevfile_end; i++) 
	    data[i]->vals = file_write_date(prevfullnames[i]);
	  qsort((void *)data, prevfile_end + 1, sizeof(heapdata *), less_compare);
	  break;
	case 3:
	  for (i = 0; i <= prevfile_end; i++) 
	    data[i]->samps = mus_sound_samples(prevfullnames[i]);
	  qsort((void *)data, prevfile_end + 1, sizeof(heapdata *), greater_compare);
	  break;
	case 4:
	  for (i = 0; i <= prevfile_end; i++) 
	    data[i]->vals = prevtimes[i];
	  qsort((void *)data, prevfile_end + 1, sizeof(heapdata *), less_compare);
	  break;
	case 5:
	  if (XEN_PROCEDURE_P(ss->file_sort_proc))
	    {
	      XEN file_list;
	      int j, gc_loc;
	      char *name;
	      file_list = XEN_EMPTY_LIST;
	      for (i = prevfile_end; i >= 0; i--) 
		file_list = XEN_CONS(C_TO_XEN_STRING(prevfullnames[i]), file_list);
	      gc_loc = snd_protect(file_list);
	      file_list = XEN_COPY_ARG(XEN_CALL_1(ss->file_sort_proc, file_list, "previous files sort"));
	      snd_unprotect_at(gc_loc); /* unprotect old version */
	      gc_loc = snd_protect(file_list); /* protect new */
	      if (XEN_LIST_P(file_list))
		{
		  for (i = 0; (i < len) && (XEN_NOT_NULL_P(file_list)); i++, file_list = XEN_CDR(file_list))
		    {
		      name = XEN_TO_C_STRING(XEN_CAR(file_list));
		      for (j = 0; j < len; j++)
			if (strcmp(data[j]->a2, name) == 0)
			  {
			    prevnames[i] = data[j]->a1;
			    prevfullnames[i] = data[j]->a2;
			    prevtimes[i] = data[j]->times;
			  }
		    }
		}
	      snd_unprotect_at(gc_loc);
	    }
	  for (i = 0; i < len; i++) FREE(data[i]);
	  FREE(data);
	  return;
	  break;
	}
      for (i = 0; i < len; i++)
	{
	  prevnames[i] = data[i]->a1;
	  prevfullnames[i] = data[i]->a2;
	  prevtimes[i] = data[i]->times;
	  FREE(data[i]);
	}
      FREE(data);
    }
}

static XEN g_previous_files_sort_procedure(void)
{
  #define H_previous_files_sort_procedure "(" S_previous_files_sort_procedure "): sort procedure for the current files viewer"
  return(ss->file_sort_proc);
}

static XEN g_set_previous_files_sort_procedure(XEN proc)
{
  char *error = NULL;
  if (XEN_PROCEDURE_P(ss->file_sort_proc))
    {
      snd_unprotect_at(ss->file_sort_proc_loc);
      ss->file_sort_proc_loc = -1;
    }
  ss->file_sort_proc = XEN_UNDEFINED;
  error = procedure_ok(proc, 1, "file sort", "sort", 1);
  if (error == NULL)
    {
      ss->file_sort_proc = proc;
      if (XEN_PROCEDURE_P(proc))
	{
	  ss->file_sort_proc_loc = snd_protect(proc);
	  set_file_sort_sensitive(true);
	}
      else set_file_sort_sensitive(false);
    }
  else 
    {
      XEN errstr;
      set_file_sort_sensitive(false);
      errstr = C_TO_XEN_STRING(error);
      FREE(error);
      return(snd_bad_arity_error(S_setB S_previous_files_sort_procedure, errstr, proc));
    }
  return(proc);
}


/* -------- file dialog header/data choices */

#define NUM_NEXT_FORMATS 8
#define NUM_IRCAM_FORMATS 5
#define NUM_WAVE_FORMATS 8
#define NUM_AIFC_FORMATS 13
#define NUM_AIFF_FORMATS 4
#define NUM_NIST_FORMATS 7
#define NUM_RAW_FORMATS 18

#define NEXT_POSITION 0
#define AIFC_POSITION 1
#define RIFF_POSITION 2
#define RAW_POSITION 3
#define AIFF_POSITION 4
#define IRCAM_POSITION 5
#define NIST_POSITION 6

static char **next_data_formats, **ircam_data_formats, **wave_data_formats, **aifc_data_formats, **aiff_data_formats, **nist_data_formats, **raw_data_formats;

static int next_dfs[NUM_NEXT_FORMATS] = {MUS_BSHORT, MUS_MULAW, MUS_BYTE, MUS_BFLOAT, MUS_BINT, MUS_ALAW, MUS_B24INT, MUS_BDOUBLE};
static int ircam_dfs[NUM_IRCAM_FORMATS] = {MUS_BSHORT, MUS_MULAW, MUS_BFLOAT, MUS_BINT, MUS_ALAW};
static int wave_dfs[NUM_WAVE_FORMATS] = {MUS_MULAW, MUS_ALAW, MUS_UBYTE, MUS_LSHORT, MUS_LINT, MUS_LFLOAT, MUS_LDOUBLE, MUS_L24INT};
static int aifc_dfs[NUM_AIFC_FORMATS] = {MUS_BSHORT, MUS_MULAW, MUS_BYTE, MUS_BINT, MUS_ALAW, MUS_B24INT,
					 MUS_BFLOAT, MUS_BDOUBLE, MUS_UBYTE, MUS_LSHORT,
					 MUS_LINT, MUS_L24INT, MUS_UBSHORT};
static int aiff_dfs[NUM_AIFF_FORMATS] = {MUS_BSHORT, MUS_BINT, MUS_BYTE, MUS_B24INT};
static int nist_dfs[NUM_NIST_FORMATS] = {MUS_BSHORT, MUS_LSHORT, MUS_BINT, MUS_LINT, MUS_BYTE, MUS_B24INT, MUS_L24INT};
static int raw_dfs[NUM_RAW_FORMATS] = {MUS_BSHORT, MUS_MULAW, MUS_BYTE, MUS_BFLOAT, MUS_BINT, MUS_ALAW,
				       MUS_UBYTE, MUS_B24INT, MUS_BDOUBLE, MUS_LSHORT, MUS_LINT,
				       MUS_LFLOAT, MUS_LDOUBLE, MUS_UBSHORT, MUS_ULSHORT,
				       MUS_L24INT, MUS_BINTN, MUS_LINTN};

void initialize_format_lists(void)
{
  int i;
  next_data_formats = (char **)CALLOC(NUM_NEXT_FORMATS, sizeof(char *));
  for (i = 0; i < NUM_NEXT_FORMATS; i++) next_data_formats[i] = (char *)mus_data_format_to_string(next_dfs[i]);
  ircam_data_formats = (char **)CALLOC(NUM_IRCAM_FORMATS, sizeof(char *));
  for (i = 0; i < NUM_IRCAM_FORMATS; i++) ircam_data_formats[i] = (char *)mus_data_format_to_string(ircam_dfs[i]);
  wave_data_formats = (char **)CALLOC(NUM_WAVE_FORMATS, sizeof(char *));
  for (i = 0; i < NUM_WAVE_FORMATS; i++) wave_data_formats[i] = (char *)mus_data_format_to_string(wave_dfs[i]);
  aiff_data_formats = (char **)CALLOC(NUM_AIFF_FORMATS, sizeof(char *));
  for (i = 0; i < NUM_AIFF_FORMATS; i++) aiff_data_formats[i] = (char *)mus_data_format_to_string(aiff_dfs[i]);
  aifc_data_formats = (char **)CALLOC(NUM_AIFC_FORMATS, sizeof(char *));
  for (i = 0; i < NUM_AIFC_FORMATS; i++) aifc_data_formats[i] = (char *)mus_data_format_to_string(aifc_dfs[i]);
  nist_data_formats = (char **)CALLOC(NUM_NIST_FORMATS, sizeof(char *));
  for (i = 0; i < NUM_NIST_FORMATS; i++) nist_data_formats[i] = (char *)mus_data_format_to_string(nist_dfs[i]);
  raw_data_formats = (char **)CALLOC(NUM_RAW_FORMATS, sizeof(char *));
  for (i = 0; i < NUM_RAW_FORMATS; i++) raw_data_formats[i] = (char *)mus_data_format_to_string(raw_dfs[i]);
}

int header_type_from_position(int position)
{
  switch (position)
    {
    case NEXT_POSITION:  return(MUS_NEXT);  break;
    case AIFC_POSITION:  return(MUS_AIFC);  break;
    case RIFF_POSITION:  return(MUS_RIFF);  break;
    case IRCAM_POSITION: return(MUS_IRCAM); break;
    case RAW_POSITION:   return(MUS_RAW);   break;
    case AIFF_POSITION:  return(MUS_AIFF);  break;
    case NIST_POSITION:  return(MUS_NIST);  break;
    }
  return(MUS_RAW);
}

int data_format_from_position(int header, int position)
{
  switch (header)
    {
    case MUS_NEXT:  return(next_dfs[position]);  break;
    case MUS_AIFC:  return(aifc_dfs[position]);  break;
    case MUS_RIFF:  return(wave_dfs[position]);  break;
    case MUS_IRCAM: return(ircam_dfs[position]); break;
    case MUS_RAW:   return(raw_dfs[position]);   break;
    case MUS_AIFF:  return(aiff_dfs[position]);  break;
    case MUS_NIST:  return(nist_dfs[position]);  break;
    }
  return(position);
}

char **set_header_and_data_positions(file_data *fdat, int type, int format)
{
  char **fl = NULL;
  int i;
  switch (type)
    {
    case MUS_AIFC: 
      fdat->formats = NUM_AIFC_FORMATS; 
      fl = aifc_data_formats; 
      fdat->header_pos = AIFC_POSITION; 
      fdat->format_pos = 0;
      for (i = 0; i < NUM_AIFC_FORMATS; i++) if (format == aifc_dfs[i]) {fdat->format_pos = i; break;}
      break;
    case MUS_RIFF: 
      fdat->formats = NUM_WAVE_FORMATS; 
      fl = wave_data_formats; 
      fdat->header_pos = RIFF_POSITION;
      fdat->format_pos = 3;
      for (i = 0; i < NUM_WAVE_FORMATS; i++) if (format == wave_dfs[i]) {fdat->format_pos = i; break;}
      break;
    case MUS_IRCAM: 
      fdat->formats =NUM_IRCAM_FORMATS; 
      fl = ircam_data_formats; 
      fdat->header_pos = IRCAM_POSITION; 
      fdat->format_pos = 0;
      for (i = 0; i < NUM_IRCAM_FORMATS; i++) if (format == ircam_dfs[i]) {fdat->format_pos = i; break;}
      break;
    case MUS_NEXT:
      fdat->formats = NUM_NEXT_FORMATS; 
      fl = next_data_formats; 
      fdat->header_pos = NEXT_POSITION; 
      fdat->format_pos = 0;
      for (i = 0; i < NUM_NEXT_FORMATS; i++) if (format == next_dfs[i]) {fdat->format_pos = i; break;}
      break;
    case MUS_NIST:
      fdat->formats = NUM_NIST_FORMATS; 
      fl = nist_data_formats; 
      fdat->header_pos = NIST_POSITION; 
      fdat->format_pos = 0;
      for (i = 0; i < NUM_NIST_FORMATS; i++) if (format == nist_dfs[i]) {fdat->format_pos = i; break;}
      break;
    case MUS_RAW:
      fdat->formats = NUM_RAW_FORMATS; 
      fl = raw_data_formats; 
      fdat->header_pos = RAW_POSITION; 
      fdat->format_pos = 0;
      break;
    case MUS_AIFF: 
      fdat->formats = NUM_AIFF_FORMATS; 
      fl = aiff_data_formats; 
      fdat->header_pos = AIFF_POSITION; 
      fdat->format_pos = 0;
      for (i = 0; i < NUM_AIFF_FORMATS; i++) if (format == aiff_dfs[i]) {fdat->format_pos = i; break;}
      break;
    }
  return(fl);
}

void set_header_type_and_format_from_position(file_data *fdat, int pos)
{
  fdat->header_pos = pos;
  switch (pos)
    {
    case NEXT_POSITION: fdat->current_type = MUS_NEXT; fdat->current_format = MUS_BSHORT; break;
    case NIST_POSITION: fdat->current_type = MUS_NIST; fdat->current_format = MUS_BSHORT; break;
    case AIFC_POSITION: fdat->current_type = MUS_AIFC; fdat->current_format = MUS_BSHORT; break;
    case RIFF_POSITION: fdat->current_type = MUS_RIFF; fdat->current_format = MUS_LSHORT; break;
    case IRCAM_POSITION: fdat->current_type = MUS_IRCAM; fdat->current_format = MUS_BSHORT; break;
    case RAW_POSITION: fdat->current_type = MUS_RAW; fdat->current_format = MUS_BSHORT; break;
    case AIFF_POSITION: fdat->current_type = MUS_AIFF; fdat->current_format = MUS_BSHORT; break;
    }
}

char **set_header_positions_from_type(file_data *fdat, int header_type, int data_format)
{
  char **formats = NULL;
  int *dfs = NULL;
  int i;
  switch (header_type)
    {
    case MUS_NEXT: 
      fdat->formats = NUM_NEXT_FORMATS; 
      formats = next_data_formats; 
      dfs = next_dfs; 
      fdat->header_pos = NEXT_POSITION; 
      break;
    case MUS_NIST: 
      fdat->formats = NUM_NIST_FORMATS; 
      formats = nist_data_formats; 
      dfs = nist_dfs; 
      fdat->header_pos = NIST_POSITION; 
      break;
    case MUS_AIFC: 
      fdat->formats = NUM_AIFC_FORMATS; 
      formats = aifc_data_formats; 
      fdat->header_pos = AIFC_POSITION; 
      dfs = aifc_dfs; 
      break;
    case MUS_RIFF: 
      fdat->formats = NUM_WAVE_FORMATS; 
      formats = wave_data_formats; 
      fdat->header_pos = RIFF_POSITION; 
      dfs = wave_dfs; 
      break;
    case MUS_IRCAM: 
      fdat->formats = NUM_IRCAM_FORMATS; 
      formats = ircam_data_formats; 
      fdat->header_pos = IRCAM_POSITION; 
      dfs = ircam_dfs; 
      break;
    case MUS_RAW: 
      fdat->formats = NUM_RAW_FORMATS; 
      formats = raw_data_formats; 
      fdat->header_pos = RAW_POSITION; 
      dfs = raw_dfs; 
      break;
    case MUS_AIFF: 
      fdat->formats = NUM_AIFF_FORMATS; 
      formats = aiff_data_formats; 
      fdat->header_pos = AIFF_POSITION; 
      dfs = aiff_dfs; 
      break;
    }
  fdat->format_pos = 0;
  for (i = 0; i < fdat->formats; i++) 
    if (data_format == dfs[i]) 
      {
	fdat->format_pos = i; 
	break;
      }
  return(formats);
}

typedef struct {
  snd_info *sp;
  char *filename;
  int edits;
} same_name_info;

static bool check_for_same_name(snd_info *sp1, void *ur_info)
{
  same_name_info *info = (same_name_info *)ur_info;
  if ((sp1) && (strcmp(sp1->filename, info->filename) == 0))
    {
      int i;
      info->sp = sp1;
      for (i = 0; i < sp1->nchans; i++) 
	{
	  chan_info *cp;
	  cp = sp1->chans[i];
	  if (info->edits < cp->edit_ctr) 
	    info->edits = cp->edit_ctr;
	}
      return(true); /* stop immediately and deal with this one */
    }
  return(false);
}

bool saved_file_needs_update(snd_info *sp, char *str, save_dialog_t save_type, int srate, int type, int format, char *comment)
{
  /* only from save-as dialog where type/format are known to be writable */
  /* returns true if new file not yet opened, false if opened (same name as old) or if cancelled by error of some sort */
  same_name_info *collision = NULL;
  char *fullname;
  int result = 0;
  bool needs_update = true;
  if (sp) clear_minibuffer(sp);
  alert_new_file();
  /* now check in-core files -- need to close any of same name -- if edited what to do? */
  /* also it's possible the new file name is the same as the current file name(!) */
  fullname = mus_expand_filename(str);
  if (!(snd_overwrite_ok(fullname))) 
    {
      FREE(fullname); 
      return(false);
    }
  if (strcmp(fullname, sp->filename) == 0)
    {
      char *ofile;
      /* normally save-as saves the current edit tree, merely saving the current state
       * in a separate, presumably inactive file; here we're being asked to overwrite
       * the current file in save-as; we can't have it both ways -- we'll save the edits 
       * in a temp file, then rename/copy the temp, and call update 
       */
      if (sp->read_only)
	{
	  report_in_minibuffer_and_save(sp, _("can't save-as %s (%s is write-protected)"), fullname, sp->short_filename);
	  FREE(fullname);
	  return(false);
	}
      /* it's possible also that the same-named file is open in several windows -- for now we'll ignore that */
      /* also what if a sound is write-protected in one window, and not in another? */

      /* PERHAPS: before-save-as-hook (here and below): (lambda (ofile snd|selection=#t? type format srate comment) ...)
       *            if -> #t, then skip the explicit save
       *            provides a way to call last-minute stuff like src-sound
       *            or should this be at a lower level? (snd-snd.c has after-save-as too as called from Scheme save-sound)
       *            ideally there would be a secure communication path to any after-save-as-hook (for undo)
       *               but that requires another arg to after-save-as (could be whatever is returned by before-save-as)
       *            also, lock out recursive hook call (user might call save-sound-as within hook func)
       */

      ofile = snd_tempnam(); 
      if (save_type == FILE_SAVE_AS)
	result = save_edits_without_display(sp, ofile, type, format, srate, comment, AT_CURRENT_EDIT_POSITION);
      else result = save_selection(ofile, type, format, srate, comment, SAVE_ALL_CHANS);
      if (result != MUS_NO_ERROR)
	report_in_minibuffer(sp, _("save as temp %s hit error: %s"), ofile, strerror(errno));
      else move_file(ofile, sp->filename);
      snd_update(sp);
      needs_update = false;
      FREE(ofile);
      FREE(fullname);
    }
  else
    {
      collision = (same_name_info *)CALLOC(1, sizeof(same_name_info));
      collision->filename = fullname;
      collision->edits = 0;
      collision->sp = NULL;
      map_over_sounds(check_for_same_name, (void *)collision);
      if (collision->sp)
	{
	  /* if no edits, we'll just close, overwrite, reopen */
	  /* if edits, we need to ask luser what to do */
	  /* we don't need to check for overwrites at this point */
	  if (collision->edits > 0)
	    {
	      if (!(snd_yes_or_no_p(_("%s has unsaved edits. Clobber them?"), str)))
		{
		  FREE(fullname); 
		  FREE(collision); 
		  return(false);
		}
	    }
	  snd_close_file(collision->sp);
	}
      mus_sound_forget(fullname);

      /* save-as-hook? */

      if (save_type == FILE_SAVE_AS)
	result = save_edits_without_display(sp, str, type, format, srate, comment, AT_CURRENT_EDIT_POSITION);
      else result = save_selection(str, type, format, srate, comment, SAVE_ALL_CHANS);
      if (result != MUS_NO_ERROR)
	{
	  report_in_minibuffer_and_save(sp, "%s: %s", 
					str, 
					(errno != 0) ? strerror(errno) : mus_error_type_to_string(result));
	  needs_update = false;
	}
      else report_in_minibuffer(sp, _("%s saved as %s"),
				(save_type == FILE_SAVE_AS) ? sp->short_filename : "selection",
				str);
      if (collision->sp) 
	{
	  snd_open_file(fullname, false);
	  if (needs_update) needs_update = false;
	}
      FREE(fullname);
      FREE(collision);
    }
  return(needs_update);
}


void edit_header_callback(snd_info *sp, file_data *edit_header_data)
{
  /* this blindly changes the header info -- it does not actually reformat the data or whatever */
  int err;
  if (sp->read_only)
    {
      snd_error(_("%s is write-protected"), sp->filename);
      return;
    }
#if HAVE_ACCESS
  err = access(sp->filename, W_OK);
#else
  err = 0;
#endif
  if (err == 0)
    {
      off_t loc, samples;
      char *comment, *original_comment = NULL;
      file_info *hdr;
      int chans, srate, type, format;
      hdr = sp->hdr;
      original_comment = mus_sound_comment(sp->filename);
      if ((hdr->type == MUS_AIFF) || (hdr->type == MUS_AIFC)) mus_header_set_aiff_loop_info(mus_sound_loop_info(sp->filename));
      mus_sound_forget(sp->filename);
      /* find out which fields changed -- if possible don't touch the sound data */
      comment = read_file_data_choices(edit_header_data, &srate, &chans, &type, &format, &loc, &samples);
      if (hdr->type != type)
	mus_header_change_type(sp->filename, type, format);
      else
	{
	  if (hdr->format != format)
	    mus_header_change_format(sp->filename, type, format);
	}
      if (hdr->chans != chans)
	mus_header_change_chans(sp->filename, type, chans);
      if (hdr->srate != srate)
	mus_header_change_srate(sp->filename, type, srate);
      if (hdr->samples != samples)
	mus_header_change_data_size(sp->filename, type, mus_samples_to_bytes(format, samples));
      if ((type == MUS_NEXT) &&
	  (hdr->data_location != loc))
	mus_header_change_location(sp->filename, MUS_NEXT, loc);
      if (((comment) && (original_comment) && (strcmp(comment, original_comment) != 0)) ||
	  ((comment) && (original_comment == NULL)) ||
	  ((comment == NULL) && (original_comment)))
	mus_header_change_comment(sp->filename, type, comment);
      if (comment) FREE(comment);
      if (original_comment) FREE(original_comment);
      snd_update(sp);
    }
  else 
    snd_error(_("can't write file %s: %s"), sp->short_filename, strerror(errno));
}

#if (!USE_NO_GUI)
/* raw data dialog funcs */

static int swap_int(int n)
{
  int o;
  unsigned char *inp, *outp; 
  inp = (unsigned char *)&n; 
  outp = (unsigned char *)&o;
  outp[0] = inp[3]; 
  outp[1] = inp[2]; 
  outp[2] = inp[1]; 
  outp[3] = inp[0];
  return(o);
}

static off_t swap_off_t(off_t n)
{
  off_t o;
  unsigned char *inp, *outp; 
  inp = (unsigned char *)&n; 
  outp = (unsigned char *)&o;
  outp[0] = inp[7]; 
  outp[1] = inp[6]; 
  outp[2] = inp[5]; 
  outp[3] = inp[4]; 
  outp[4] = inp[3]; 
  outp[5] = inp[2];
  outp[6] = inp[1];
  outp[7] = inp[0];
  return(o);
}

static short swap_short(short n)
{
  short o;
  unsigned char *inp, *outp; 
  inp = (unsigned char *)&n; 
  outp = (unsigned char *)&o;
  outp[0] = inp[1]; 
  outp[1] = inp[0]; 
  return(o);
}

static char *raw_data_explanation(const char *filename, file_info *hdr)
{
  char *reason_str, *tmp_str, *file_string;
  off_t nsamp;
  int ns, better_srate = 0, better_chans = 0, len;
  reason_str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  tmp_str = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
  /* try to provide some notion of what might be the intended header (currently limited to byte-order mistakes) */
  len = PRINT_BUFFER_SIZE;
  mus_snprintf(reason_str, len, "srate: %d", hdr->srate);
  ns = (int)swap_int(hdr->srate);
  if ((ns < 4000) || (ns > 100000)) 
    ns = (int)swap_short((short)(hdr->srate));
  if ((ns > 4000) && (ns < 100000))
    {
      better_srate = ns;
      mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, " (swapped: %d)", ns);
      reason_str = snd_strcat(reason_str, tmp_str, &len);
    }
  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\nchans: %d", hdr->chans);
  reason_str = snd_strcat(reason_str, tmp_str, &len);
  ns = swap_int(hdr->chans);
  if ((ns < 0) || (ns > 8)) 
    ns = swap_short((short)(hdr->chans));
  if ((ns > 0) && (ns <= 8))
    {
      better_chans = ns;
      mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, " (swapped: %d)", ns);
      reason_str = snd_strcat(reason_str, tmp_str, &len);
    }
  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\nlength: %.3f (" PRId64 " samples, " PRId64 " bytes total)",
	       (float)((double)(hdr->samples) / (float)(hdr->chans * hdr->srate)),
	       hdr->samples,
	       mus_sound_length(filename));
  reason_str = snd_strcat(reason_str, tmp_str, &len);
  nsamp = swap_off_t(hdr->samples);
  if (nsamp < mus_sound_length(filename))
    {
      mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\n  (swapped: " OFF_TD , nsamp);
      reason_str = snd_strcat(reason_str, tmp_str, &len);
      if ((better_chans) && (better_srate))
	{
	  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE,
		       ", swapped length: %.3f / sample-size-in-bytes)",
		       (float)((double)nsamp / (float)(better_chans * better_srate)));
	  reason_str = snd_strcat(reason_str, tmp_str, &len);
	}
      else reason_str = snd_strcat(reason_str, ")", &len);
    }
  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\ndata location: " OFF_TD, hdr->data_location);
  reason_str = snd_strcat(reason_str, tmp_str, &len);
  nsamp = swap_off_t(hdr->data_location);
  if ((nsamp > 0) && (nsamp <= 1024)) 
    {
      mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, " (swapped: " OFF_TD ")", nsamp);
      reason_str = snd_strcat(reason_str, tmp_str, &len);
    }
  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\ntype: %s", mus_header_type_name(hdr->type));
  reason_str = snd_strcat(reason_str, tmp_str, &len);
  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\nformat: %s\n", mus_data_format_name(hdr->format));
  reason_str = snd_strcat(reason_str, tmp_str, &len);
  hdr->type = MUS_RAW;
  post_it("Current header values", reason_str);
  file_string = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  mus_snprintf(file_string, PRINT_BUFFER_SIZE,
	       "Bogus header found for %s", 
	       filename_without_home_directory(filename));
  FREE(tmp_str);
  FREE(reason_str);
  return(file_string);
}
#endif

static XEN g_add_sound_file_extension(XEN ext)
{
  #define H_add_sound_file_extension "(" S_add_sound_file_extension " ext):  add the file extension 'ext' to the list of sound file extensions"
  XEN_ASSERT_TYPE(XEN_STRING_P(ext), ext, XEN_ONLY_ARG, S_add_sound_file_extension, "a string");
  add_sound_file_extension(XEN_TO_C_STRING(ext));
  return(ext);
}

static XEN g_file_write_date(XEN file)
{
  #define S_file_write_date "file-write-date"
#ifndef __GNUC__
  #define H_file_write_date "(" S_file_write_date " file): write date of file"
#else
  #define H_file_write_date "(" S_file_write_date " file) -> write date in the same format as \
current-time:\n(strftime \"%a %d-%b-%Y %H:%M %Z\" (localtime (file-write-date \"oboe.snd\")))\n\
Equivalent to Guile (stat:mtime (stat file))"
#endif

  time_t date;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ONLY_ARG, S_file_write_date, "a string");
  date = file_write_date(XEN_TO_C_STRING(file));
  return(xen_return_first(C_TO_XEN_INT(date), file));
}

static XEN g_sound_loop_info(XEN snd)
{
  #define H_sound_loop_info "(" S_sound_loop_info " (snd #f)): return the sound's loop points as a \
list: (sustain-start sustain-end release-start release-end baseNote detune)"
  int *res;
  snd_info *sp;
  ASSERT_SOUND(S_sound_loop_info, snd, 1);
  sp = get_sp(snd, NO_PLAYERS);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_sound_loop_info, snd));
  res = sp->hdr->loops;
  if (res)
    return(XEN_LIST_8(C_TO_XEN_INT(res[0]), C_TO_XEN_INT(res[1]), C_TO_XEN_INT(res[2]),
		      C_TO_XEN_INT(res[3]), C_TO_XEN_INT(res[4]), C_TO_XEN_INT(res[5]),
		      C_TO_XEN_INT(res[6]), C_TO_XEN_INT(res[7])));
  return(XEN_EMPTY_LIST);
}

static XEN g_set_sound_loop_info(XEN snd, XEN vals)
{
  snd_info *sp;
  char *tmp_file;
  file_info *hdr;
  int type, len = 0, err = MUS_NO_ERROR;
  XEN start0 = XEN_UNDEFINED, end0 = XEN_UNDEFINED; 
  XEN start1 = XEN_UNDEFINED, end1 = XEN_UNDEFINED; 
  XEN mode0 = XEN_UNDEFINED, mode1 = XEN_UNDEFINED;
  XEN note = XEN_UNDEFINED, detune = XEN_UNDEFINED;
  XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(vals) || XEN_LIST_P_WITH_LENGTH(vals, len), vals, XEN_ARG_2, S_setB S_sound_loop_info, "a list");
  if (XEN_NOT_BOUND_P(vals))
    {
      vals = snd;
      len = XEN_LIST_LENGTH(vals); 
      sp = get_sp(XEN_UNDEFINED, NO_PLAYERS);
    }
  else 
    {
      ASSERT_SOUND(S_setB S_sound_loop_info, snd, 1);
      sp = get_sp(snd, NO_PLAYERS);
    }
  if (sp == NULL) 
    return(snd_no_such_sound_error(S_setB S_sound_loop_info, snd));
  if (sp->read_only)
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_setB S_sound_loop_info),
			 C_TO_XEN_STRING(sp->filename),
			 C_TO_XEN_STRING(_("sound is write-protected"))));
  hdr = sp->hdr;
  if (len > 0) 
    {
      start0 = XEN_LIST_REF(vals, 0);
      if (len > 1) 
	{
	  end0 = XEN_LIST_REF(vals, 1);
	  if (len > 2) 
	    {
	      start1 = XEN_LIST_REF(vals, 2);
	      if (len > 3) 
		{
		  end1 = XEN_LIST_REF(vals, 3);
		  if (len > 4)
		    {
		      note = XEN_LIST_REF(vals, 4);
		      if (len > 5) 
			{
			  detune = XEN_LIST_REF(vals, 5);
			  if (len > 6) 
			    {
			      mode0 = XEN_LIST_REF(vals, 6);
			      if (len > 7)
				mode1 = XEN_LIST_REF(vals, 7);
			    }}}}}}}
  if (hdr->loops == NULL)
    hdr->loops = (int *)CALLOC(MUS_LOOP_INFO_SIZE, sizeof(int));
  else memset((void *)(hdr->loops), 0, MUS_LOOP_INFO_SIZE * sizeof(int));
  hdr->loops[0] = XEN_TO_C_INT_OR_ELSE(start0, 0);
  hdr->loops[1] = XEN_TO_C_INT_OR_ELSE(end0, 0);
  hdr->loops[2] = XEN_TO_C_INT_OR_ELSE(start1, 0);
  hdr->loops[3] = XEN_TO_C_INT_OR_ELSE(end1, 0);
  if (len > 4)
    {
      hdr->loops[4] = XEN_TO_C_INT_OR_ELSE(note, 60);
      hdr->loops[5] = XEN_TO_C_INT_OR_ELSE(detune, 0);
    }
  if (len > 6)
    {
      hdr->loops[6] = XEN_TO_C_INT_OR_ELSE(mode0, 0);
      hdr->loops[7] = XEN_TO_C_INT_OR_ELSE(mode1, 0);
    }
  else
    {
      if (!(XEN_FALSE_P(end0))) hdr->loops[6] = 1;
      if (!(XEN_FALSE_P(end1))) hdr->loops[7] = 1;
    }
  mus_sound_set_loop_info(sp->filename, hdr->loops);
  mus_header_set_aiff_loop_info(hdr->loops);
  type = hdr->type;
  if ((type != MUS_AIFF) && 
      (type != MUS_AIFC))
    {
      snd_warning(_("changing %s's header from %s to aifc to accommodate loop info"),
		  sp->short_filename,
		  mus_header_type_name(type));
      type = MUS_AIFC;
    }
  /* ideally set sound_loop_info would just change the header (keeping all other state intact)
   *   but this aspect of AIFC headers is impossibly complicated (if we could assume our own headers,
   *   it would not be so hard).
   */
  tmp_file = snd_tempnam();
  err = save_edits_without_display(sp, tmp_file, type, 
				   hdr->format, 
				   hdr->srate, 
				   hdr->comment,
				   AT_CURRENT_EDIT_POSITION);
  if (err != MUS_NO_ERROR)
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_setB S_sound_loop_info),
			 C_TO_XEN_STRING(tmp_file),
			 C_TO_XEN_STRING(strerror(errno))));
  move_file(tmp_file, sp->filename);
  snd_update(sp);
  FREE(tmp_file);
  return(xen_return_first((err == MUS_NO_ERROR) ? XEN_TRUE : C_TO_XEN_INT(err), snd, vals));
}

static XEN g_soundfont_info(XEN snd)
{
  /* return all soundfont descriptors as list of lists: ((name start loopstart loopend)) */
  #define H_soundfont_info "(" S_soundfont_info " (snd #f)): list of lists describing snd as a soundfont. \
each inner list has the form: (name start loopstart loopend)"

  XEN inlist = XEN_EMPTY_LIST, outlist = XEN_EMPTY_LIST;
  snd_info *sp;
  ASSERT_SOUND(S_soundfont_info, snd, 1);
  sp = get_sp(snd, NO_PLAYERS);
  if (sp == NULL) 
    return(snd_no_such_sound_error(S_soundfont_info, snd));
  mus_header_read(sp->filename);
  if (mus_header_type() == MUS_SOUNDFONT)
    {
      int i, lim;
      lim = mus_header_sf2_entries();
      if (lim > 0)
	for (i = lim - 1; i >= 0; i--)
	  {
	    inlist = XEN_LIST_4(C_TO_XEN_STRING(mus_header_sf2_name(i)),
			        C_TO_XEN_INT(mus_header_sf2_start(i)),
			        C_TO_XEN_INT(mus_header_sf2_loop_start(i)),
			        C_TO_XEN_INT(mus_header_sf2_end(i)));
	    outlist = XEN_CONS(inlist, outlist);
	  }
    }
  return(outlist);
}

static XEN g_preload_directory(XEN directory) 
{
  #define H_preload_directory "(" S_preload_directory " dir): preload into the View:Files dialog any sounds in dir"
  XEN_ASSERT_TYPE(XEN_STRING_P(directory), directory, XEN_ONLY_ARG, S_preload_directory, "a string");
  add_directory_to_prevlist(XEN_TO_C_STRING(directory));
  return(directory);
}

static XEN g_preload_file(XEN file) 
{
  #define H_preload_file "(" S_preload_file " file): preload file into the View:Files dialog"
  char *name = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ONLY_ARG, S_preload_file, "a string");
  name = mus_expand_filename(XEN_TO_C_STRING(file));
  if (mus_file_probe(name))
    add_to_previous_files(filename_without_home_directory(name), name);
  if (name) FREE(name);
  return(file);
}

static XEN g_sound_files_in_directory(XEN dirname)
{
  #define H_sound_files_in_directory "(" S_sound_files_in_directory " (directory \".\")): return a list of the sound files in 'directory'"
  char *name = NULL;
  XEN res = XEN_EMPTY_LIST;
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(dirname), dirname, XEN_ONLY_ARG, S_sound_files_in_directory, "a string");
  if (XEN_STRING_P(dirname))
    name = XEN_TO_C_STRING(dirname);
  else name = ".";
  if (name)
    {
      dir *dp = NULL;
      dp = find_sound_files_in_dir(name);
      if (dp)
	{
	  int i;
	  for (i = dp->len - 1; i >= 0; i--)
	    res = XEN_CONS(C_TO_XEN_STRING(dp->files[i]), res);
	  free_dir(dp);
	}
    }
  return(xen_return_first(res, dirname));
}

#define S_disk_kspace "disk-kspace"
static XEN g_disk_kspace(XEN name)
{
  #define H_disk_kspace "(" S_disk_kspace " filename): kbytes of space available on partition containing 'filename'"
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ONLY_ARG, S_disk_kspace, "a string");
  return(C_TO_XEN_OFF_T(disk_kspace(XEN_TO_C_STRING(name))));
}

static XEN g_open_file_dialog(XEN managed)
{
  widget_t w;
  #define H_open_file_dialog "(" S_open_file_dialog " (managed #t)): create the file dialog if needed and display it if 'managed'"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_open_file_dialog, "a boolean");
  w = make_open_file_dialog(false, (XEN_BOUND_P(managed)) ? XEN_TO_C_BOOLEAN(managed) : true);
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_mix_file_dialog(XEN managed)
{
  widget_t w;
  #define H_mix_file_dialog "(" S_mix_file_dialog " (managed #t)): create the mix file dialog if needed and display it if 'managed'"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_mix_file_dialog, "a boolean");
  w = make_mix_file_dialog((XEN_BOUND_P(managed)) ? XEN_TO_C_BOOLEAN(managed) : true);
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_previous_files_sort(void) {return(C_TO_XEN_INT(previous_files_sort(ss)));}
static XEN g_set_previous_files_sort(XEN val) 
{
  #define H_previous_files_sort "(" S_previous_files_sort "): sort choice in view files (0 = unsorted, 1 = by name, \
2 = by write date, 3 = by size, 4 = by directory order, 5 = by " S_previous_files_sort_procedure "."

  int choice;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_previous_files_sort, "an integer"); 
  update_prevlist();
  choice = XEN_TO_C_INT(val);
  if ((choice < 0) || (choice > 5))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_previous_files_sort, 1, val, "~A, but must be between 0.0 and 0..5");
  set_previous_files_sort(choice);
  if (view_files_dialog_is_active()) 
    make_prevfiles_list();
  return(C_TO_XEN_INT(previous_files_sort(ss)));
}

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_1(g_add_sound_file_extension_w, g_add_sound_file_extension)
XEN_NARGIFY_1(g_file_write_date_w, g_file_write_date)
XEN_ARGIFY_1(g_soundfont_info_w, g_soundfont_info)
XEN_NARGIFY_1(g_preload_directory_w, g_preload_directory)
XEN_NARGIFY_1(g_preload_file_w, g_preload_file)
XEN_ARGIFY_1(g_sound_files_in_directory_w, g_sound_files_in_directory)
XEN_ARGIFY_1(g_sound_loop_info_w, g_sound_loop_info)
XEN_ARGIFY_2(g_set_sound_loop_info_w, g_set_sound_loop_info)
XEN_NARGIFY_0(g_previous_files_sort_procedure_w, g_previous_files_sort_procedure)
XEN_NARGIFY_1(g_set_previous_files_sort_procedure_w, g_set_previous_files_sort_procedure)
XEN_NARGIFY_1(g_disk_kspace_w, g_disk_kspace)
XEN_ARGIFY_1(g_open_file_dialog_w, g_open_file_dialog)
XEN_ARGIFY_1(g_mix_file_dialog_w, g_mix_file_dialog)
XEN_NARGIFY_0(g_previous_files_sort_w, g_previous_files_sort)
XEN_NARGIFY_1(g_set_previous_files_sort_w, g_set_previous_files_sort)
#else
#define g_add_sound_file_extension_w g_add_sound_file_extension
#define g_file_write_date_w g_file_write_date
#define g_soundfont_info_w g_soundfont_info
#define g_preload_directory_w g_preload_directory
#define g_preload_file_w g_preload_file
#define g_sound_files_in_directory_w g_sound_files_in_directory
#define g_sound_loop_info_w g_sound_loop_info
#define g_set_sound_loop_info_w g_set_sound_loop_info
#define g_previous_files_sort_procedure_w g_previous_files_sort_procedure
#define g_set_previous_files_sort_procedure_w g_set_previous_files_sort_procedure
#define g_disk_kspace_w g_disk_kspace
#define g_open_file_dialog_w g_open_file_dialog
#define g_mix_file_dialog_w g_mix_file_dialog
#define g_previous_files_sort_w g_previous_files_sort
#define g_set_previous_files_sort_w g_set_previous_files_sort
#endif

void g_init_file(void)
{
  XEN_DEFINE_PROCEDURE(S_add_sound_file_extension,    g_add_sound_file_extension_w, 1, 0, 0, H_add_sound_file_extension);
  XEN_DEFINE_PROCEDURE(S_file_write_date,             g_file_write_date_w,          1, 0, 0, H_file_write_date);
  XEN_DEFINE_PROCEDURE(S_soundfont_info,              g_soundfont_info_w,           0, 1, 0, H_soundfont_info);
  XEN_DEFINE_PROCEDURE(S_preload_directory,           g_preload_directory_w,        1, 0, 0, H_preload_directory);
  XEN_DEFINE_PROCEDURE(S_preload_file,                g_preload_file_w,             1, 0, 0, H_preload_file);
  XEN_DEFINE_PROCEDURE(S_sound_files_in_directory,    g_sound_files_in_directory_w, 0, 1, 0, H_sound_files_in_directory);
  XEN_DEFINE_PROCEDURE(S_open_file_dialog,            g_open_file_dialog_w,         0, 1, 0, H_open_file_dialog);
  XEN_DEFINE_PROCEDURE(S_mix_file_dialog, g_mix_file_dialog_w,                      0, 1, 0, H_mix_file_dialog);
  XEN_DEFINE_PROCEDURE(S_disk_kspace,                 g_disk_kspace_w,              1, 0, 0, H_disk_kspace);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_sound_loop_info, g_sound_loop_info_w, H_sound_loop_info,
				   S_setB S_sound_loop_info, g_set_sound_loop_info_w,  0, 1, 1, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_previous_files_sort, g_previous_files_sort_w, H_previous_files_sort,
				   S_setB S_previous_files_sort, g_set_previous_files_sort_w,  0, 0, 1, 0);

  XEN_DEFINE_VARIABLE(S_snd_opened_sound, snd_opened_sound, XEN_FALSE);
  XEN_DEFINE_VARIABLE("memo-sound", snd_memo_sound, XEN_FALSE); /* backwards compatibility */

  #define H_open_hook S_open_hook " (filename): called each time a file is opened (before the actual open). \
If it returns #t, the file is not opened."

  #define H_close_hook S_close_hook " (snd): called each time a file is closed (before the close). \
If it returns #t, the file is not closed."

  #define H_just_sounds_hook S_just_sounds_hook " (filename): called on each file (after the sound file extension check) if \
the " S_just_sounds " button is set. Return #f to filter out filename. "

  #define H_bad_header_hook S_bad_header_hook " (filename): called if a file has some bogus-looking header. \
Return #t to give up on that file."

  #define H_after_save_as_hook " (saved-sound-index save-as-full-filename from-save-as-dialog): called \
upon File:Save as completion."

  XEN_DEFINE_HOOK(open_hook, S_open_hook, 1, H_open_hook);                            /* arg = filename */
  XEN_DEFINE_HOOK(close_hook, S_close_hook, 1, H_close_hook);                         /* arg = sound index */
  XEN_DEFINE_HOOK(just_sounds_hook, S_just_sounds_hook, 1, H_just_sounds_hook);       /* arg = filename */
  XEN_DEFINE_HOOK(bad_header_hook, S_bad_header_hook, 1, H_bad_header_hook);          /* arg = filename */
  XEN_DEFINE_HOOK(after_save_as_hook, S_after_save_as_hook, 3, H_after_save_as_hook); /* args: index filename from-dialog */

  #define H_open_raw_sound_hook S_open_raw_sound_hook " (filename current-choices): called when a headerless sound file is opened. \
Its result can be a list describing the raw file's attributes (thereby bypassing the Raw File Dialog and so on). \
The list (passed to subsequent hook functions as 'current-choice') is interpreted as \
(list chans srate data-format data-location data-length) where trailing elements can \
be omitted (location defaults to 0, and length defaults to the file length in bytes)."

  XEN_DEFINE_HOOK(open_raw_sound_hook, S_open_raw_sound_hook, 2, H_open_raw_sound_hook);    /* args = filename current-result */

  #define H_update_hook S_update_hook " (snd): called just before " S_update_sound " is called. \
The update process can  be triggered by a variety of situations, not just by " S_update_sound ". \
The hook is passed the sound's index.  If it returns #t, the update is cancelled (this is not \
recommended!); if it returns a procedure of one argument, that procedure is called upon \
completion of the update operation; its argument is the (possibly different) sound index. \
Snd tries to maintain the index across the update, but if you change the number of channels \
the newly updated sound may have a different index."

  XEN_DEFINE_HOOK(update_hook, S_update_hook, 1, H_update_hook);            /* arg = sound index */

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_previous_files_sort_procedure, g_previous_files_sort_procedure_w, H_previous_files_sort_procedure,
                                   S_setB S_previous_files_sort_procedure, g_set_previous_files_sort_procedure_w,  0, 0, 1, 0);

  #define H_previous_files_select_hook S_previous_files_select_hook "(filename): called when a file is selected in the \
previous files list of the View Files dialog.  If it returns #t, the default action, opening the file, is omitted."

  XEN_DEFINE_HOOK(previous_files_select_hook, S_previous_files_select_hook, 1, H_previous_files_select_hook); /* arg = filename */
}
