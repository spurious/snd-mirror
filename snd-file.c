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
static int original_srate = 0, original_chans = 0, original_format = MUS_UNKNOWN;

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
      original_srate = hdr->srate;
      hdr->chans = mus_sound_chans(fullname);
      original_chans = hdr->chans;
      hdr->format = mus_sound_data_format(fullname);
      original_format = hdr->format;
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
	  ss->translated_filename = copy_string(newname);
	}
    }
  else snd_remove(newname, REMOVE_FROM_CACHE);
  if (newname) FREE(newname);
  if (loops) FREE(loops);
  return(hdr);
}


static XEN open_raw_sound_hook;

static file_info *open_raw_sound(const char *fullname, bool read_only, bool selected)
{
  XEN res = XEN_FALSE;
  int res_loc = NOT_A_GC_LOC;
  XEN procs, arg1;
  int len, srate, chans, data_format;
  off_t data_location, bytes;

  if (ss->reloading_updated_file != 0)
    {
      /* choices already made, so just send back a header that reflects those choices */
      return(make_file_info_1(fullname));
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
	  if (res_loc != NOT_A_GC_LOC) snd_unprotect_at(res_loc);
	  res_loc = snd_protect(res);
	  procs = XEN_CDR(procs);
	}
    }
  if (XEN_LIST_P(res)) /* empty list ok here -> accept all current defaults */
    {
      file_info *hdr;
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
      mus_sound_override_header(fullname, 
				srate, chans, data_format, 
				MUS_RAW, data_location,
				mus_bytes_to_samples(data_format, bytes));
      if (res_loc != NOT_A_GC_LOC) snd_unprotect_at(res_loc);	      
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
      if (res_loc != NOT_A_GC_LOC) snd_unprotect_at(res_loc);
      if (just_quit) return(NULL);

      /* open-sound and view-sound do not fall into the raw data dialog */
      if ((ss->open_requestor == FROM_OPEN_SOUND) || 
	  (ss->open_requestor == FROM_VIEW_SOUND) ||
	  (ss->open_requestor == FROM_NEW_SOUND)  ||
	  (ss->open_requestor == FROM_NEW_FILE_DIALOG))
	return(make_file_info_1(fullname));

      str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
      mus_snprintf(str, PRINT_BUFFER_SIZE, _("No header found for %s"), filename_without_home_directory(fullname));
      raw_data_dialog_to_file_info(fullname, str, NULL, read_only, selected);
    }
#endif
  return(NULL);
}

#if (!USE_NO_GUI)
static char *raw_data_explanation(const char *filename, file_info *hdr, char **info);
#endif

static XEN bad_header_hook;

static file_info *tackle_bad_header(const char *fullname, bool read_only, bool selected)
{
  /* messed up header */
  if ((XEN_HOOKED(bad_header_hook)) &&
      (XEN_TRUE_P(run_or_hook(bad_header_hook,
			      XEN_LIST_1(C_TO_XEN_STRING(fullname)),
			      S_bad_header_hook))))
    return(NULL);

  /* if not from dialog, throw an error ('bad-header) */
  if ((ss->open_requestor == FROM_OPEN_SOUND) || 
      (ss->open_requestor == FROM_VIEW_SOUND) ||
      (ss->open_requestor == FROM_NEW_SOUND)) /* this case should not happen! */
    {
      char *caller;
      if (ss->open_requestor == FROM_OPEN_SOUND)
	caller = S_open_sound;
      else caller = S_view_sound;
      XEN_ERROR(BAD_HEADER,
		XEN_LIST_2(C_TO_XEN_STRING(caller),
			   C_TO_XEN_STRING(fullname)));
      return(NULL);
    }
  
#if (!USE_NO_GUI)
  {
    int type;
    type = mus_header_type();
    if ((type != MUS_MIDI_SAMPLE_DUMP) && 
	(type != MUS_IEEE) &&
	(type != MUS_MUS10) && 
	(type != MUS_HCOM) &&
	(!(encoded_header_p(type))))
      {
	char *info = NULL, *title = NULL;
	title = raw_data_explanation(fullname, make_file_info_1(fullname), &info);
	raw_data_dialog_to_file_info(fullname, title, info, read_only, selected);
      }
  }
#endif
  return(NULL);
}

file_info *make_file_info(const char *fullname, bool read_only, bool selected)
{
  file_info *hdr = NULL;
  if (mus_file_probe(fullname))
    {
      int type = MUS_UNSUPPORTED, format = MUS_UNKNOWN;

      /* open-raw-sound will force it to viewed as a raw sound */
      if (ss->open_requestor == FROM_OPEN_RAW_SOUND)
	return(make_file_info_1(fullname));

      type = mus_sound_header_type(fullname);
      if (type == MUS_ERROR)        /* if something went wrong */
	type = mus_header_type();   /*    try to read it anyway... */

      /* handle some files directly through the translator (headers here are not readable) */
      if ((type == MUS_MIDI_SAMPLE_DUMP) ||
	  (type == MUS_IEEE) ||
	  (type == MUS_MUS10) ||
	  (type == MUS_HCOM) ||
	  (encoded_header_p(type))) /* MUS_MATLAB here? */
	{
	  return(translate_file(fullname, type));
	}
	  
      if (MUS_HEADER_TYPE_OK(type)) 
	{ /* at least the header type seems plausible */
	  /* check header fields */
	  int sr = 0, ch = 0;
	  sr = mus_sound_srate(fullname);
	  ch = mus_sound_chans(fullname);
	  if ((fallback_srate > 0) && ((sr <= 0) || (sr > 100000000))) sr = fallback_srate;
	  if ((fallback_chans > 0) && ((ch >= 256) || (ch <= 0))) ch = fallback_chans;
	  if ((sr <= 0) || (sr > 100000000) ||
	      (ch >= 256) || (ch <= 0))
	    return(tackle_bad_header(fullname, read_only, selected));

	  /* header is ok */
	  if (type == MUS_RAW)
	    return(open_raw_sound(fullname, read_only, selected));
	  else
	    {
	      format = mus_sound_data_format(fullname);
	      if (MUS_DATA_FORMAT_OK(format))
		return(make_file_info_1(fullname));
	      return(translate_file(fullname, type));
	    }
	}
      else snd_error(_("%s does not seem to be a sound file?"), fullname); /* no known header */
    }
  else
    {
      snd_error(_("can't find file %s: %s"), fullname, snd_io_strerror());
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
static int default_sound_file_extensions = 0;

static void add_sound_file_extension(const char *ext)
{
  int i;
  for (i = 0; i < sound_file_extensions_end; i++)
    if (strcmp(ext, sound_file_extensions[i]) == 0)
      return;
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
  default_sound_file_extensions = sound_file_extensions_end;
}

void save_added_sound_file_extensions(FILE *fd)
{
  int i;
  if (sound_file_extensions_end > default_sound_file_extensions)
    for (i = default_sound_file_extensions; i < sound_file_extensions_end; i++)
      {
#if HAVE_SCHEME
	fprintf(fd, "(%s \"%s\")\n", S_add_sound_file_extension, sound_file_extensions[i]);
#endif
#if HAVE_RUBY
	fprintf(fd, "%s(\"%s\")\n", TO_PROC_NAME(S_add_sound_file_extension), sound_file_extensions[i]);
#endif
      }
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


bool sound_file_p(char *name) /* can't be const (compiler confusion) */
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

static int local_error = MUS_NO_ERROR;
static char *local_error_msg = NULL;
static mus_error_handler_t *old_error_handler;
static void local_error2snd(int type, char *msg) 
{
  local_error = type;
  if (local_error_msg) free(local_error_msg);
  if (msg)
    local_error_msg = strdup(msg);
  else local_error_msg = NULL;
}

bool plausible_sound_file_p(const char *name)
{
  int err = MUS_NO_ERROR;
  old_error_handler = mus_error_set_handler(local_error2snd);
  err = mus_header_read(name);
  mus_error_set_handler(old_error_handler);
  return((err == MUS_NO_ERROR) &&
	 (mus_header_type() != MUS_RAW));
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
		  name, snd_io_strerror());
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

static void fam_sp_action(struct fam_info *fp, FAMEvent *fe)
{
#if HAVE_FAM
  snd_info *sp = NULL;
  /* fp has been checked already */
  sp = (snd_info *)(fp->data);
  if (sp->writing) return;
  switch (fe->code)
    {
    case FAMChanged:
      /* this includes cp overwriting old etc */
      if (file_write_date(sp->filename) != sp->write_date) /* otherwise chmod? */
	{
	  sp->need_update = true;
	  if (auto_update(ss))
	    snd_update(sp);
	  else snd_file_bomb_icon(sp, true);
	}
#if HAVE_ACCESS
      else
	{
	  int err;
	  err = access(sp->filename, R_OK);
	  if (err < 0)
	    {
	      char *msg;
	      msg = mus_format(_("%s is read-protected!"), sp->short_filename);
	      display_minibuffer_error(sp, msg); /* TODO: this should clear if the permission bits change */
	      FREE(msg);
	      sp->file_unreadable = true;
	      snd_file_bomb_icon(sp, true);
	    }
	  else
	    {
	      sp->file_unreadable = false;
	      err = access(sp->filename, W_OK);
	      sp->file_read_only = (err < 0);
	      snd_file_lock_icon(sp, sp->user_read_only || sp->file_read_only);
	    }
	}
#endif
      break;

    case FAMDeleted:
      /* snd_update will post a complaint in this case, but I like it explicit */
      if (mus_file_probe(sp->filename) == 0)
	{
	  /* user deleted file while editing it? */
	  report_in_minibuffer(sp, _("%s no longer exists!"), sp->short_filename);
	  sp->file_unreadable = true;
	  snd_file_bomb_icon(sp, true);
	  return;
	}
      /* else I don't know why I got this fam code, but fall through to the update case */
    case FAMCreated:
    case FAMMoved:
      sp->file_unreadable = false;
      sp->need_update = true;
      if (auto_update(ss))
	snd_update(sp);
      else snd_file_bomb_icon(sp, true);
      break;

    default:
      /* ignore the rest */
      break;
    }
#endif
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
#if HAVE_SCHEME
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

#if HAVE_GUILE_DYNAMIC_WIND
/* cleanup even if error in file lookup process */
typedef struct {
  char *filename;
  bool read_only;
  file_info *hdr;
} open_file_context;

static snd_info *open_file_sp = NULL;
static void before_open_file(void *context) {}

static XEN open_file_body(void *context)
{
  open_file_context *sc = (open_file_context *)context;
  open_file_sp = add_sound_window(sc->filename, sc->read_only, sc->hdr); /* snd-xsnd.c -> make_file_info (in this file) */
  return(XEN_FALSE);
}

static void after_open_file(void *context)
{
  open_file_context *sc = (open_file_context *)context;
  if (sc->filename) FREE(sc->filename);
  FREE(sc);
}
#endif

snd_info *finish_opening_sound(snd_info *sp, bool selected)
{
  if (sp)
    {
      int files;
#if HAVE_RUBY
      XEN_VARIABLE_SET(S_snd_opened_sound, C_TO_XEN_INT(sp->index));
#endif
#if HAVE_SCHEME
      XEN_VARIABLE_SET(snd_opened_sound, C_TO_XEN_INT(sp->index));
      XEN_VARIABLE_SET(snd_memo_sound, C_TO_XEN_INT(sp->index)); /* backwards compatibility */
#endif
      sp->write_date = file_write_date(sp->filename);
      sp->need_update = false;
      ss->active_sounds++;
      files = ss->active_sounds;
      reflect_file_change_in_title();
      sp->file_watcher = fam_monitor_file(sp->filename, (void *)sp, fam_sp_action);
    }
  map_over_separate_chans(channel_open_pane, NULL);
#if USE_MOTIF
  map_over_separate_chans(channel_unlock_pane, NULL);
#endif
  if (sp) 
    {
      if (selected) select_channel(sp, 0);
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

static snd_info *snd_open_file_1(const char *filename, bool selected, bool read_only)
{
  file_info *hdr;
  snd_info *sp;
  char *mcf = NULL;
  mcf = mus_expand_filename(filename);
  if (XEN_HOOKED(open_hook))
    {
      XEN res = XEN_FALSE;
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
  hdr = make_file_info(mcf, read_only, selected);
  if (!hdr) 
    {
      if (mcf) FREE(mcf);
      return(NULL);
    }

#if HAVE_GUILE_DYNAMIC_WIND
  {
    open_file_context *ofc;
    ofc = (open_file_context *)CALLOC(1, sizeof(open_file_context));
    ofc->filename = mcf;
    ofc->read_only = read_only;
    ofc->hdr = hdr;
    scm_internal_dynamic_wind((scm_t_guard)before_open_file, 
			      (scm_t_inner)open_file_body, 
			      (scm_t_guard)after_open_file, 
			      (void *)ofc,
			      (void *)ofc);
    sp = open_file_sp; /* has to be global since we free sc during the unwind */
    mcf = NULL; /* freed above in unwind */
  }
#else
  sp = add_sound_window(mcf, read_only, hdr);
  if (mcf) FREE(mcf);
#endif
  return(finish_opening_sound(sp, selected));
}

snd_info *snd_open_file(const char *filename, bool read_only) 
{
  return(snd_open_file_1(filename, FILE_SELECTED, read_only));
}

snd_info *snd_open_file_unselected(const char *filename) 
{
  return(snd_open_file_1(filename, FILE_NOT_SELECTED, FILE_READ_WRITE));
}

void snd_close_file(snd_info *sp)
{
  int files, i;
  XEN res = XEN_FALSE;
  if (XEN_HOOKED(close_hook))
    res = run_or_hook(close_hook,
		      XEN_LIST_1(C_TO_XEN_INT(sp->index)),
		      S_close_hook);
  if (XEN_TRUE_P(res)) return;
  sp->file_watcher = fam_unmonitor_file(sp->filename, sp->file_watcher);

  /* exit does not go through this function to clean up temps -- see snd_exit_cleanly in snd-main.c */
  if (selection_creation_in_progress()) finish_selection_creation();
  if (ss->deferred_regions > 0)
    for (i = 0; i < sp->nchans; i++)
      if (sp->chans[i]) 
	sequester_deferred_regions(sp->chans[i], -1);
  sp->inuse = SOUND_IDLE;
  for (i = 0; i < sp->nchans; i++) sp->chans[i]->squelch_update = true;
  add_file_to_default_view_files_dialog(sp->filename);
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
      release_pending_track_states();
    }
  reflect_file_change_in_title();
  call_selection_watchers(SELECTION_IN_DOUBT);
}

io_error_t copy_file(const char *oldname, const char *newname)
{
  /* make newname a copy of oldname */
  int ifd, ofd;
  off_t bytes, wb, total;
  char *buf = NULL;
  total = 0;
  ifd = OPEN(oldname, O_RDONLY, 0);
  if (ifd == -1) return(IO_CANT_OPEN_FILE);
  ofd = CREAT(newname, 0666);
  if (ofd == -1) 
    {
      snd_close(ifd, oldname);
      return(IO_CANT_OPEN_FILE);
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
	  return(IO_WRITE_ERROR);
	}
    }
  snd_close(ifd, oldname);
  wb = disk_kspace(newname);
  snd_close(ofd, newname);
  FREE(buf);
  if (wb < 0)
    return(IO_DISK_FULL);
  return(IO_NO_ERROR);
}

io_error_t move_file(const char *oldfile, const char *newfile)
{
  io_error_t err = IO_NO_ERROR;
  int rename_err;
  rename_err = RENAME(oldfile, newfile);
  if (rename_err != 0)
    {
      if (errno == EXDEV)
	{
	  err = copy_file(oldfile, newfile);
	  if (err == IO_NO_ERROR)
	    {
	      rename_err = snd_remove(oldfile, REMOVE_FROM_CACHE);
	      if (rename_err == -1)
		return(IO_CANT_MOVE_FILE);
	    }
	}
    }
  /* TODO: this had snd_error which we need to push upwards */
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
      FREE(cp->cgx->ax);
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
		snd_error(_("can't close file %s: %s"), filename, snd_io_strerror());
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
  ncp->beats_per_measure = ocp->beats_per_measure;
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
  else ncp->cursor_proc_loc = NOT_A_GC_LOC;
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
	  cps[i]->cursor_proc_loc = NOT_A_GC_LOC;
	}
    }
  if (XEN_BOUND_P(osp->search_proc))
    {
      snd_unprotect_at(osp->search_proc_loc);
      osp->search_proc_loc = NOT_A_GC_LOC;
      osp->search_proc = XEN_UNDEFINED;
    }
}

static XEN update_hook;

static snd_info *snd_update_1(snd_info *sp, const char *ur_filename)
{
  /* we can't be real smart here because the channel number may have changed and so on */
  int i, old_srate, old_chans, old_format, sp_chans, old_index, gc_loc = NOT_A_GC_LOC;
  channel_style_t old_channel_style;
  bool read_only, old_raw;
  void *sa;
  snd_info *nsp = NULL;
  char *filename;
  void *ms;
  snd_info *saved_sp;
  struct ctrl_state *saved_controls;
  off_t *old_cursors;
  fam_info *old_file_watcher;
  sp_watcher **old_watchers;
  int old_watchers_size;

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
  read_only = sp->user_read_only;
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

  old_file_watcher = sp->file_watcher; /* will be unmonitored in snd_close_file, but we need to know if it is being monitored now */
  old_watchers = sp->watchers;
  old_watchers_size = sp->watchers_size;
  sp->watchers = NULL;       /* don't confuse watchers with a temporary close! */
  sp->watchers_size = 0;

  /* TODO: do I need to save sp->writing? ->unreadable? */

  snd_close_file(sp);

  /* no mus_sound_forget here because we may be simply re-interpreting the existing data (set! (data-format) ...) etc */
  /* this normalizes the fft/lisp/wave state so we need to reset it after reopen */
  alert_new_file();
  ss->reloading_updated_file = (old_index + 1);
  ss->open_requestor = FROM_UPDATE;
  nsp = snd_open_file(filename, read_only);
  ss->reloading_updated_file = 0;
  if (old_raw)
    mus_header_set_raw_defaults(old_srate, old_chans, old_format);
  if (nsp)
    {
      /* if header is bad, nsp can be null awaiting raw data dialog's return */
      if ((old_file_watcher) &&
	  (!(nsp->file_watcher)))
	nsp->file_watcher = fam_monitor_file(nsp->filename, (void *)nsp, fam_sp_action); /* might be a different sp as well as underlying file */
      nsp->watchers = old_watchers;
      nsp->watchers_size = old_watchers_size;

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
      if (gc_loc != NOT_A_GC_LOC) snd_unprotect_at(gc_loc);
      gc_loc = NOT_A_GC_LOC;
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

/* TODO: if channels_combined, I think the window gets smaller each time */
snd_info *snd_update(snd_info *sp)
{
  Latus app_x, app_y;
  if (sp->edited_region) return(sp);
  if ((sp->inuse == SOUND_NORMAL) &&
      (sp->filename))
    {
      if (mus_file_probe(sp->filename) == 0)
	{
	  snd_error(_("%s no longer exists!"), sp->short_filename);
	  return(sp);
	}
      app_x = widget_width(MAIN_SHELL(ss));
      app_y = widget_height(MAIN_SHELL(ss));
      sp = snd_update_1(sp, sp->filename);
      set_widget_size(MAIN_SHELL(ss), app_x, app_y);
    }
  return(sp);
}

static void snd_update_warning_handler(const char *msg, void *data)
{
  report_in_minibuffer((snd_info *)data, msg);
}

static void snd_update_error_handler(const char *msg, void *data)
{
  redirect_snd_error_to(NULL, NULL);
  redirect_snd_warning_to(NULL, NULL);
  XEN_ERROR(CANT_UPDATE_FILE,
	    XEN_LIST_2(C_TO_XEN_STRING((char *)data),
		       C_TO_XEN_STRING(msg)));
}

snd_info *snd_update_within_xen(snd_info *sp, const char *caller)
{
  snd_info *nsp;
  redirect_snd_error_to(snd_update_error_handler, (void *)caller);
  redirect_snd_warning_to(snd_update_warning_handler, (void *)sp);
  nsp = snd_update(sp);
  redirect_snd_error_to(NULL, NULL);
  redirect_snd_warning_to(NULL, NULL);
  return(nsp);
}

static XEN after_save_as_hook;

void run_after_save_as_hook(snd_info *sp, const char *already_saved_as_name, bool from_save_as_dialog)
{
  /* might be save-selection, as well as save-sound-as */
  if (XEN_HOOKED(after_save_as_hook))
    {
      char *fullname;
      fullname = mus_expand_filename(already_saved_as_name);
      run_progn_hook(after_save_as_hook,
		     XEN_LIST_3((sp) ? C_TO_XEN_INT(sp->index) : XEN_FALSE,
				C_TO_XEN_STRING(fullname),
				C_TO_XEN_BOOLEAN(from_save_as_dialog)),
		     S_after_save_as_hook);
      FREE(fullname);
    }
}

static XEN before_save_as_hook;
static bool before_save_as_hook_active = false;

bool run_before_save_as_hook(snd_info *sp, const char *save_as_filename, bool selection, int srate, int type, int format, char *comment)
{
  /* might be save-selection, as well as save-sound-as */
  if (before_save_as_hook_active) return(false);
  if (XEN_HOOKED(before_save_as_hook))
    {
      XEN result = XEN_FALSE;
      before_save_as_hook_active = true;
      result = run_progn_hook(before_save_as_hook,
			      XEN_LIST_7((sp) ? C_TO_XEN_INT(sp->index) : XEN_FALSE,
					 C_TO_XEN_STRING(save_as_filename),
					 C_TO_XEN_BOOLEAN(selection),
					 C_TO_XEN_INT(srate),
					 C_TO_XEN_INT(type),
					 C_TO_XEN_INT(format),
					 (comment) ? C_TO_XEN_STRING(comment) : XEN_FALSE),
			      S_before_save_as_hook);
      before_save_as_hook_active = false;
      return(!(XEN_FALSE_P(result)));
    }
  return(false);
}



/* -------- file dialog header/data choices -------- */

#define NUM_NEXT_FORMATS 8
#define NUM_IRCAM_FORMATS 5
#define NUM_WAVE_FORMATS 8
#define NUM_AIFC_FORMATS 13
#define NUM_AIFF_FORMATS 4
#define NUM_NIST_FORMATS 7
#define NUM_RAW_FORMATS 18
#define NUM_OGG_FORMATS 1
#define NUM_FLAC_FORMATS 1
#define NUM_SPEEX_FORMATS 1
#define NUM_MPEG_FORMATS 1
#define NUM_MIDI_FORMATS 1

#define NEXT_POSITION 0
#define AIFC_POSITION 1
#define RIFF_POSITION 2
#define RAW_POSITION 3
#define AIFF_POSITION 4
#define IRCAM_POSITION 5
#define NIST_POSITION 6
static int OGG_POSITION = -1, FLAC_POSITION = -1, SPEEX_POSITION = -1, MPEG_POSITION = -1, MIDI_POSITION = -1;

static char **next_data_formats, **ircam_data_formats, **wave_data_formats, **aifc_data_formats, **aiff_data_formats, **nist_data_formats, **raw_data_formats;

static int next_dfs[NUM_NEXT_FORMATS] = {MUS_BSHORT, MUS_MULAW, MUS_BYTE, MUS_BFLOAT, MUS_BINT, MUS_ALAW, MUS_B24INT, MUS_BDOUBLE};
static int ircam_dfs[NUM_IRCAM_FORMATS] = {MUS_BSHORT, MUS_MULAW, MUS_BFLOAT, MUS_BINT, MUS_ALAW};
static int wave_dfs[NUM_WAVE_FORMATS] = {MUS_MULAW, MUS_ALAW, MUS_UBYTE, MUS_LSHORT, MUS_LINT, MUS_LFLOAT, MUS_LDOUBLE, MUS_L24INT};
static int aifc_dfs[NUM_AIFC_FORMATS] = {MUS_BSHORT, MUS_MULAW, MUS_BYTE, MUS_BINT, MUS_ALAW, MUS_B24INT,
					 MUS_BFLOAT, MUS_BDOUBLE, MUS_UBYTE, MUS_LSHORT, MUS_LINT, MUS_L24INT, MUS_UBSHORT};
static int aiff_dfs[NUM_AIFF_FORMATS] = {MUS_BSHORT, MUS_BINT, MUS_BYTE, MUS_B24INT};
static int nist_dfs[NUM_NIST_FORMATS] = {MUS_BSHORT, MUS_LSHORT, MUS_BINT, MUS_LINT, MUS_BYTE, MUS_B24INT, MUS_L24INT};
static int raw_dfs[NUM_RAW_FORMATS] = {MUS_BSHORT, MUS_MULAW, MUS_BYTE, MUS_BFLOAT, MUS_BINT, MUS_ALAW,
				       MUS_UBYTE, MUS_B24INT, MUS_BDOUBLE, MUS_LSHORT, MUS_LINT,
				       MUS_LFLOAT, MUS_LDOUBLE, MUS_UBSHORT, MUS_ULSHORT, MUS_L24INT, MUS_BINTN, MUS_LINTN};
static int ogg_dfs[NUM_OGG_FORMATS] = {MUS_LSHORT};
static int flac_dfs[NUM_FLAC_FORMATS] = {MUS_LSHORT};
static int speex_dfs[NUM_SPEEX_FORMATS] = {MUS_LSHORT};
static int mpeg_dfs[NUM_MPEG_FORMATS] = {MUS_LSHORT};
static int midi_dfs[NUM_MIDI_FORMATS] = {MUS_LSHORT};
static char **ogg_data_formats = NULL, **flac_data_formats = NULL, **speex_data_formats = NULL, **mpeg_data_formats = NULL, **midi_data_formats = NULL;

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
#if HAVE_OGG
  ogg_data_formats = (char **)CALLOC(NUM_OGG_FORMATS, sizeof(char *));
  for (i = 0; i < NUM_OGG_FORMATS; i++) ogg_data_formats[i] = (char *)mus_data_format_to_string(ogg_dfs[i]);
#endif
#if HAVE_FLAC
  flac_data_formats = (char **)CALLOC(NUM_FLAC_FORMATS, sizeof(char *));
  for (i = 0; i < NUM_FLAC_FORMATS; i++) flac_data_formats[i] = (char *)mus_data_format_to_string(flac_dfs[i]);
#endif
#if HAVE_SPEEX
  speex_data_formats = (char **)CALLOC(NUM_SPEEX_FORMATS, sizeof(char *));
  for (i = 0; i < NUM_SPEEX_FORMATS; i++) speex_data_formats[i] = (char *)mus_data_format_to_string(speex_dfs[i]);
#endif
#if HAVE_MPEG
  mpeg_data_formats = (char **)CALLOC(NUM_MPEG_FORMATS, sizeof(char *));
  for (i = 0; i < NUM_MPEG_FORMATS; i++) mpeg_data_formats[i] = (char *)mus_data_format_to_string(mpeg_dfs[i]);
#endif
#if HAVE_TIMIDITY
  midi_data_formats = (char **)CALLOC(NUM_MIDI_FORMATS, sizeof(char *));
  for (i = 0; i < NUM_MIDI_FORMATS; i++) midi_data_formats[i] = (char *)mus_data_format_to_string(midi_dfs[i]);
#endif
}

#define NUM_BUILTIN_HEADERS 7
#define NUM_POSSIBLE_HEADERS 12
static char **writable_headers = NULL;
static char **readable_headers = NULL;
static char *builtin_headers[NUM_BUILTIN_HEADERS] = {"sun  ", "aifc ", "wave ", "raw  ", "aiff ", "ircam", "nist "};
static int num_writable_headers = NUM_BUILTIN_HEADERS;
static int num_readable_headers = NUM_BUILTIN_HEADERS;

char **short_builtin_headers(int *len)
{
  (*len) = NUM_BUILTIN_HEADERS;
  return(builtin_headers);
}

char **short_writable_headers(int *len)
{
  /* these are headers that we either write ourself, or have external programs to write (oggenc etc) */
  int i;
  if (!writable_headers)
    {
      writable_headers = (char **)CALLOC(NUM_POSSIBLE_HEADERS, sizeof(char *));
      for (i = 0; i < NUM_BUILTIN_HEADERS; i++)
	writable_headers[i] = builtin_headers[i];
#if HAVE_OGG
      /* write temp as RIFF 16-bit lshort, then
	 oggenc tempfile.wav -o outoggfile.ogg
	 -q 6 sets to higher quality -- add as arg somehow?
	 so format here is just the ogg output format: nothing settable
      */
      OGG_POSITION = i;
      writable_headers[i++] = "ogg";
#endif
#if HAVE_FLAC
      /* flac tempfile.wav -o output.flac
	 no choices
       */
      FLAC_POSITION = i;
      writable_headers[i++] = "flac";
#endif
#if HAVE_SPEEX
      /* speexenc tempfile.wav output.spx
	 no choices
      */
      SPEEX_POSITION = i;
      writable_headers[i++] = "speex";
#endif
      num_writable_headers = i;
    }
  (*len) = num_writable_headers;
  return(writable_headers);
}

char **short_readable_headers(int *len)
{
  int i;
  if (!readable_headers)
    {
      readable_headers = (char **)CALLOC(NUM_POSSIBLE_HEADERS, sizeof(char *));
      for (i = 0; i < NUM_BUILTIN_HEADERS; i++)
	readable_headers[i] = builtin_headers[i];
#if HAVE_OGG
      /* oggdec to tempfile:
	 oggdec infile.ogg -b 16 -o translatedfile.wav
	 defaults for rest are signed little endian wav
	 
	 so formats (for output temp) here are not relevant -- no one will want 1 byte
      */
      OGG_POSITION = i;
      readable_headers[i++] = "ogg";
#endif
#if HAVE_FLAC
      /* flac -d infile.flac -o output.wav
       *  the -d -> decode 
       *  no choices
       */
      FLAC_POSITION = i;
      readable_headers[i++] = "flac";
#endif
#if HAVE_SPEEX
      /* speexdec infile.spx tempfile.wav
	 no other choices
       */
      SPEEX_POSITION = i;
      readable_headers[i++] = "speex";
#endif
#if HAVE_MPEG
      /* mpg321 -q -w output.wav input.mpg
	   this is mpeg->wav only
	   no choices
	   mpg123 is probably the same
       */
      MPEG_POSITION = i;
      readable_headers[i++] = "mpeg";
#endif
#if HAVE_TIMIDITY
      /* timidity input.mid -Ou -o output.snd
	 midi->next/sun
	 there are other choices...
      */
      MIDI_POSITION = i;
      readable_headers[i++] = "midi";
#endif
      num_readable_headers = i;
    }
  (*len) = num_readable_headers;
  return(readable_headers);
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
  if (position == OGG_POSITION) return(MUS_OGG);
  if (position == FLAC_POSITION) return(MUS_FLAC);
  if (position == SPEEX_POSITION) return(MUS_SPEEX);
  if (position == MPEG_POSITION) return(MUS_MPEG);
  if (position == MIDI_POSITION) return(MUS_MIDI);
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
  if (header == MUS_OGG) return(OGG_POSITION);
  if (header == MUS_FLAC) return(FLAC_POSITION);
  if (header == MUS_SPEEX) return(SPEEX_POSITION);
  if (header == MUS_MPEG) return(MPEG_POSITION);
  if (header == MUS_MIDI) return(MIDI_POSITION);
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
    case MUS_OGG:
      fdat->formats = NUM_OGG_FORMATS;
      fl = ogg_data_formats; 
      fdat->header_pos = OGG_POSITION; 
      fdat->format_pos = 0;
      for (i = 0; i < NUM_OGG_FORMATS; i++) if (format == ogg_dfs[i]) {fdat->format_pos = i; break;}
      break;
    case MUS_FLAC:
      fdat->formats = NUM_FLAC_FORMATS;
      fl = flac_data_formats; 
      fdat->header_pos = FLAC_POSITION; 
      fdat->format_pos = 0;
      for (i = 0; i < NUM_FLAC_FORMATS; i++) if (format == flac_dfs[i]) {fdat->format_pos = i; break;}
      break;
    case MUS_SPEEX:
      fdat->formats = NUM_SPEEX_FORMATS;
      fl = speex_data_formats; 
      fdat->header_pos = SPEEX_POSITION; 
      fdat->format_pos = 0;
      for (i = 0; i < NUM_SPEEX_FORMATS; i++) if (format == speex_dfs[i]) {fdat->format_pos = i; break;}
      break;
    case MUS_MPEG:
      fdat->formats = NUM_MPEG_FORMATS;
      fl = mpeg_data_formats; 
      fdat->header_pos = MPEG_POSITION; 
      fdat->format_pos = 0;
      for (i = 0; i < NUM_MPEG_FORMATS; i++) if (format == mpeg_dfs[i]) {fdat->format_pos = i; break;}
      break;
    case MUS_MIDI:
      fdat->formats = NUM_MIDI_FORMATS;
      fl = midi_data_formats; 
      fdat->header_pos = MIDI_POSITION; 
      fdat->format_pos = 0;
      for (i = 0; i < NUM_MIDI_FORMATS; i++) if (format == midi_dfs[i]) {fdat->format_pos = i; break;}
      break;
    }
  return(fl);
}

void set_header_type_and_format_from_position(file_data *fdat, int pos)
{
  fdat->header_pos = pos;
  switch (pos)
    {
    case NEXT_POSITION:  fdat->current_type = MUS_NEXT;  fdat->current_format = MUS_BSHORT; break;
    case NIST_POSITION:  fdat->current_type = MUS_NIST;  fdat->current_format = MUS_BSHORT; break;
    case AIFC_POSITION:  fdat->current_type = MUS_AIFC;  fdat->current_format = MUS_BSHORT; break;
    case RIFF_POSITION:  fdat->current_type = MUS_RIFF;  fdat->current_format = MUS_LSHORT; break;
    case IRCAM_POSITION: fdat->current_type = MUS_IRCAM; fdat->current_format = MUS_BSHORT; break;
    case RAW_POSITION:   fdat->current_type = MUS_RAW;   fdat->current_format = MUS_BSHORT; break;
    case AIFF_POSITION:  fdat->current_type = MUS_AIFF;  fdat->current_format = MUS_BSHORT; break;
    }
  if (pos == OGG_POSITION) 
    {
      fdat->current_type = MUS_OGG;  fdat->current_format = MUS_LSHORT;
    }
  if (pos == FLAC_POSITION)
    {
      fdat->current_type = MUS_FLAC;  fdat->current_format = MUS_LSHORT;
    }
  if (pos == SPEEX_POSITION)
    {
      fdat->current_type = MUS_SPEEX;  fdat->current_format = MUS_LSHORT;
    }
  if (pos == MPEG_POSITION)
    {
      fdat->current_type = MUS_MPEG;  fdat->current_format = MUS_LSHORT;
    }
  if (pos == MIDI_POSITION)
    {
      fdat->current_type = MUS_MIDI;  fdat->current_format = MUS_LSHORT;
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
    case MUS_OGG:
      fdat->formats = NUM_OGG_FORMATS; 
      formats = ogg_data_formats; 
      fdat->header_pos = OGG_POSITION; 
      dfs = ogg_dfs; 
      break;
    case MUS_FLAC:
      fdat->formats = NUM_FLAC_FORMATS; 
      formats = flac_data_formats; 
      fdat->header_pos = FLAC_POSITION; 
      dfs = flac_dfs; 
      break;
    case MUS_SPEEX:
      fdat->formats = NUM_SPEEX_FORMATS; 
      formats = speex_data_formats; 
      fdat->header_pos = SPEEX_POSITION; 
      dfs = speex_dfs; 
      break;
    case MUS_MPEG:
      fdat->formats = NUM_MPEG_FORMATS; 
      formats = mpeg_data_formats; 
      fdat->header_pos = MPEG_POSITION; 
      dfs = mpeg_dfs; 
      break;
    case MUS_MIDI:
      fdat->formats = NUM_MIDI_FORMATS; 
      formats = midi_data_formats; 
      fdat->header_pos = MIDI_POSITION; 
      dfs = midi_dfs; 
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

bool encoded_header_p(int header_type)
{
  /* going either way here */
  return((header_type == MUS_OGG) ||
	 (header_type == MUS_FLAC) ||
	 (header_type == MUS_SPEEX) ||
	 (header_type == MUS_MPEG) ||
	 (header_type == MUS_MIDI));
}

int snd_encode(int type, const char *input_filename, const char *output_filename)
{
  /* write lshort wav tmpfile, encode, remove tmpfile */
  char *command = NULL;
  if (mus_file_probe(output_filename))
    snd_remove(output_filename, IGNORE_CACHE);
  /* these guys balk at overwriting */

  switch (type)
    {
#if HAVE_OGG
    case MUS_OGG:
      command = mus_format("%s %s -o %s", PATH_OGGENC, input_filename, output_filename);
      break;
#endif
#if HAVE_FLAC
    case MUS_FLAC: 
      command = mus_format("%s %s -o %s", PATH_FLAC, input_filename, output_filename);
      break;
#endif
#if HAVE_SPEEX
    case MUS_SPEEX:
      command = mus_format("%s %s %s", PATH_SPEEXENC, input_filename, output_filename);
      break;
#endif
    default: 
      return(-1);
      break;
    }
  if (command)
    {
      system(command);
      FREE(command);
    }
  return(0);
}

int snd_decode(int type, const char *input_filename, const char *output_filename)
{
  char *command = NULL;
  if (mus_file_probe(output_filename))
    snd_remove(output_filename, IGNORE_CACHE);
  /* these guys balk at overwriting */

  switch (type)
    {
#if HAVE_OGG
    case MUS_OGG:
      command = mus_format("%s %s -b 16 -o %s", PATH_OGGDEC, input_filename, output_filename);
      break;
#endif
#if HAVE_FLAC
    case MUS_FLAC: 
      command = mus_format("%s -d %s -o %s", PATH_FLAC, input_filename, output_filename);
      break;
#endif
#if HAVE_SPEEX
    case MUS_SPEEX:
      command = mus_format("%s %s %s", PATH_SPEEXDEC, input_filename, output_filename);
      break;
#endif
#if HAVE_MPEG
    case MUS_MPEG:
#if HAVE_MPG321
      command = mus_format("%s -q -w %s %s", PATH_MPG321, output_filename, input_filename);
#else
      command = mus_format("%s -q -w %s %s", PATH_MPG123, output_filename, input_filename);
#endif
      break;
#endif
#if HAVE_TIMIDITY
    case MUS_MIDI:
      command = mus_format("%s %s -Ou -o %s", PATH_TIMIDITY, input_filename, output_filename);
      break;
#endif
    default: 
      return(-1);
      break;
    }
  if (command)
    {
      system(command);
      FREE(command);
    }
  return(0);
}
/* TODO: is there a similar ext prog for matlab translation? ace? */


typedef struct {
  snd_info *parlous_sp, *current_sp;
  char *filename;
} same_name_info;

static bool check_for_same_name(snd_info *sp1, void *ur_info)
{
  same_name_info *info = (same_name_info *)ur_info;
  if ((sp1) && 
      (sp1 != info->current_sp) &&
      (strcmp(sp1->filename, info->filename) == 0))
    {
      int i;
      for (i = 0; i < sp1->nchans; i++) 
	{
	  chan_info *cp;
	  cp = sp1->chans[i];
	  if (cp->edit_ctr > 0)
	    {
	      info->parlous_sp = sp1;
	      return(true);
	    }
	}
    }
  return(false);
}

snd_info *file_is_open_elsewhere_and_has_unsaved_edits(snd_info *sp, const char *fullname)
{
  same_name_info *collision;
  snd_info *result;
  collision = (same_name_info *)CALLOC(1, sizeof(same_name_info));
  collision->filename = (char *)fullname;
  collision->parlous_sp = NULL;
  collision->current_sp = sp;
  map_over_sounds(check_for_same_name, (void *)collision);
  result = collision->parlous_sp;
  FREE(collision);
  return(result);
}

bool edit_header_callback(snd_info *sp, file_data *edit_header_data, 
			  void (*outer_handler)(const char *error_msg, void *ufd),
			  void (*inner_handler)(const char *error_msg, void *ufd))
{
  /* this just changes the header -- it does not actually reformat the data or whatever */

  off_t loc, samples;
  char *comment, *original_comment = NULL;
  file_info *hdr;
  int chans, srate, type, format;
  if (sp->user_read_only || sp->file_read_only)
    {
      snd_error(_("%s is write-protected"), sp->filename);
      return(false);
    }
#if HAVE_ACCESS && (!HAVE_FAM)
  if (access(sp->filename, W_OK) < 0)
    {
      snd_error(_("%s is write-protected"), sp->filename);
      return(false);
    }
#endif
  hdr = sp->hdr;

  /* find out which fields changed -- if possible don't touch the sound data */
  redirect_snd_error_to(inner_handler, (void *)edit_header_data);
  comment = get_file_dialog_sound_attributes(edit_header_data, &srate, &chans, &type, &format, &loc, &samples, 1);
  redirect_snd_error_to(outer_handler, (void *)edit_header_data);
  if (edit_header_data->error_widget != NOT_A_SCANF_WIDGET) /* bad field value, perhaps */
    return(false);

  if (sp->hdr->type != MUS_RAW)
    {
      original_comment = mus_sound_comment(sp->filename);
      if ((hdr->type == MUS_AIFF) || 
	  (hdr->type == MUS_AIFC)) 
	mus_header_set_aiff_loop_info(mus_sound_loop_info(sp->filename));
    }
  mus_sound_forget(sp->filename);
  if (hdr->type != type)
    {
      sp->writing = true;
      mus_header_change_type(sp->filename, type, format);
      sp->writing = false;
    }
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
  return(true);
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

static char *raw_data_explanation(const char *filename, file_info *hdr, char **info)
{
  char *reason_str, *tmp_str, *file_string;
  off_t nsamp;
  bool ok;
  int ns, better_srate = 0, better_chans = 0, len;
  reason_str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  tmp_str = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
  /* try to provide some notion of what might be the intended header (currently limited to byte-order mistakes) */
  len = PRINT_BUFFER_SIZE;

  /* srate */
  ok = ((original_srate >= 8000) && 
	(original_srate <= 100000));
  mus_snprintf(reason_str, len, "srate%s: %d", (ok) ? "" : " looks wrong", original_srate);
  if (!ok)
    {
      ns = (int)swap_int(original_srate);
      if ((ns < 4000) || (ns > 100000)) 
	ns = (int)swap_short((short)(original_srate));
      if ((ns > 4000) && (ns < 100000))
	{
	  better_srate = ns;
	  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, " (swapped: %d)", ns);
	  reason_str = snd_strcat(reason_str, tmp_str, &len);
	}
    }

  /* chans */
  ok = ((original_chans > 0) && 
	(original_chans < 1000));
  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\nchans%s: %d", (ok) ? "" : " looks wrong", original_chans);
  reason_str = snd_strcat(reason_str, tmp_str, &len);
  if (!ok)
    {
      ns = swap_int(original_chans);
      if ((ns < 0) || (ns > 8)) 
	ns = swap_short((short)(original_chans));
      if ((ns > 0) && (ns <= 8))
	{
	  better_chans = ns;
	  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, " (swapped: %d)", ns);
	  reason_str = snd_strcat(reason_str, tmp_str, &len);
	}
    }

  /* header type */
  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\ntype: %s", mus_header_type_name(hdr->type));
  reason_str = snd_strcat(reason_str, tmp_str, &len);

  /* data format */
  if (!(MUS_DATA_FORMAT_OK(original_format)))
    {
      char *format_info;
      if (original_format != MUS_UNKNOWN)
	format_info = (char *)mus_data_format_name(original_format);
      else format_info = (char *)mus_header_original_format_name(mus_sound_original_format(filename), 
								 hdr->type);
      mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\nformat looks bogus: %s", format_info);
    }
  else mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\nformat: %s", (char *)mus_data_format_name(original_format));
  reason_str = snd_strcat(reason_str, tmp_str, &len);

  /* samples */
  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\nlength: %.3f (" PRId64 " samples, " PRId64 " bytes total)",
	       (float)((double)(hdr->samples) / (float)(hdr->chans * hdr->srate)),
	       hdr->samples,
	       mus_sound_length(filename));
  reason_str = snd_strcat(reason_str, tmp_str, &len);
  nsamp = swap_off_t(hdr->samples);
  if (nsamp < mus_sound_length(filename))
    {
      mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, " (swapped: " OFF_TD , nsamp);
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

  /* data location */
  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\ndata location: " OFF_TD, hdr->data_location);
  reason_str = snd_strcat(reason_str, tmp_str, &len);
  nsamp = swap_off_t(hdr->data_location);
  if ((nsamp > 0) && 
      (nsamp <= 1024)) 
    {
      mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, " (swapped: " OFF_TD ")", nsamp);
      reason_str = snd_strcat(reason_str, tmp_str, &len);
    }
  (*info) = reason_str;
  file_string = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  mus_snprintf(file_string, PRINT_BUFFER_SIZE,
	       "Bad header found on %s", 
	       filename_without_home_directory(filename));
  FREE(tmp_str);
  free_file_info(hdr);
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

/* TODO: test/doc sound-file-extensions */
static XEN g_sound_file_extensions(void)
{
  #define H_sound_file_extensions "(" S_sound_file_extensions ") -> list of current sound file extensions (used \
by the just-sounds file filters)"

  XEN res = XEN_EMPTY_LIST;
  int i;
  for (i = 0; i < sound_file_extensions_end; i++)
    res = XEN_CONS(C_TO_XEN_STRING(sound_file_extensions[i]),
		   res);
  return(res);
}

static XEN g_set_sound_file_extensions(XEN lst)
{
  XEN lst1;
  sound_file_extensions_end = 0;
  default_sound_file_extensions = 0;
  for (lst1 = XEN_COPY_ARG(lst); XEN_NOT_NULL_P(lst1); lst1 = XEN_CDR(lst1))
    add_sound_file_extension(XEN_TO_C_STRING(XEN_CAR(lst1)));
  return(lst);
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
  int type, len = 0;
  
  XEN start0 = XEN_UNDEFINED, end0 = XEN_UNDEFINED; 
  XEN start1 = XEN_UNDEFINED, end1 = XEN_UNDEFINED; 
  XEN mode0 = XEN_UNDEFINED, mode1 = XEN_UNDEFINED;
  XEN note = XEN_UNDEFINED, detune = XEN_UNDEFINED;
  XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(vals) || XEN_LIST_P_WITH_LENGTH(vals, len), vals, XEN_ARG_2, S_setB S_sound_loop_info, "a list");
  if (XEN_NOT_BOUND_P(vals))
    {
      /* what is going on here? -- (set! (sound-loop-info) (list...))? */
      XEN_ASSERT_TYPE(XEN_LIST_P(snd), snd, XEN_ARG_1, S_setB S_sound_loop_info, "a list");
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
  if (sp->user_read_only || sp->file_read_only)
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
  {
    io_error_t err;
    err = save_edits_without_display(sp, tmp_file, type, 
				     hdr->format, 
				     hdr->srate, 
				     hdr->comment,
				     AT_CURRENT_EDIT_POSITION);
    if ((err != IO_NO_ERROR) &&
	(err != IO_SAVE_HOOK_CANCELLATION))
      {
	XEN_ERROR(CANNOT_SAVE,
		  XEN_LIST_3(C_TO_XEN_STRING(S_setB S_sound_loop_info),
			     C_TO_XEN_STRING(tmp_file),
			     C_TO_XEN_STRING(snd_io_strerror())));
	return(XEN_FALSE); /* not executed -- just for emphasis */
      }
    sp->writing = true;
    if (err == IO_SAVE_HOOK_CANCELLATION)
      snd_remove(tmp_file, IGNORE_CACHE);
    else move_file(tmp_file, sp->filename); /* should we cancel and restart a monitor? */ /* TODO: err check from move_file */
    sp->writing = false;
    if (err != IO_SAVE_HOOK_CANCELLATION) 
      snd_update(sp);
    FREE(tmp_file); /* TODO: better error */
    return(xen_return_first((err == IO_NO_ERROR) ? XEN_TRUE : C_TO_XEN_INT((int)err), snd, vals));
  }
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

static XEN g_add_directory_to_view_files_list(XEN directory) 
{
  #define H_add_directory_to_view_files_list "(" S_add_directory_to_view_files_list " dir): adds any sound files in 'dir' to the View:Files dialog"
  XEN_ASSERT_TYPE(XEN_STRING_P(directory), directory, XEN_ONLY_ARG, S_add_directory_to_view_files_list, "a string");
  add_directory_to_default_view_files_dialog(XEN_TO_C_STRING(directory));
  return(directory);
}

static XEN g_add_file_to_view_files_list(XEN file) 
{
  #define H_add_file_to_view_files_list "(" S_add_file_to_view_files_list " file): adds file to the View:Files dialog's list"
  char *name = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ONLY_ARG, S_add_file_to_view_files_list, "a string");
  name = mus_expand_filename(XEN_TO_C_STRING(file));
  if (mus_file_probe(name))
    add_file_to_default_view_files_dialog(name);
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

static XEN g_insert_file_dialog(XEN managed)
{
  widget_t w;
  #define H_insert_file_dialog "(" S_insert_file_dialog " (managed #t)): create the insert file dialog if needed and display it if 'managed'"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_insert_file_dialog, "a boolean");
  w = make_insert_file_dialog((XEN_BOUND_P(managed)) ? XEN_TO_C_BOOLEAN(managed) : true);
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_view_files_dialog(XEN managed)
{
  widget_t w;
  #define H_view_files_dialog "(" S_view_files_dialog "): start the View Files dialog"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_view_files_dialog, "a boolean");
  w = start_view_files_dialog(XEN_TO_C_BOOLEAN(managed));
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_edit_header_dialog(XEN snd_n) 
{
  widget_t w;
  #define H_edit_header_dialog "(" S_edit_header_dialog " snd): start the Edit Header dialog on sound snd"
  snd_info *sp; 
  sp = get_sp(snd_n, NO_PLAYERS);
  if ((sp == NULL) || (sp->inuse != SOUND_NORMAL))
    return(snd_no_such_sound_error(S_edit_header_dialog, snd_n));
  w = edit_header(sp);
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_save_selection_dialog(XEN managed)
{
  widget_t w;
  #define H_save_selection_dialog "(" S_save_selection_dialog "): start the Selection Save-as dialog"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_save_selection_dialog, "a boolean");
  w = make_selection_save_as_dialog(XEN_TO_C_BOOLEAN(managed));
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_save_sound_dialog(XEN managed)
{
  widget_t w;
  #define H_save_sound_dialog "(" S_save_sound_dialog "): start the File Save-as dialog"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_save_sound_dialog, "a boolean");
  w = make_sound_save_as_dialog(XEN_TO_C_BOOLEAN(managed));
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_info_dialog(XEN subject, XEN msg)
{
  widget_t w;
  #define H_info_dialog "(" S_info_dialog " subject message): start the Info window with subject and message"
  XEN_ASSERT_TYPE(XEN_STRING_P(subject), subject, XEN_ARG_1, S_info_dialog, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, XEN_ARG_2, S_info_dialog, "a string");
  w = post_it(XEN_TO_C_STRING(subject), XEN_TO_C_STRING(msg));
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_view_files_sort_procedure(void)
{
  #define H_view_files_sort_procedure "(" S_view_files_sort_procedure "): sort procedure for the current files viewer"
  return(ss->view_files_sort_proc);
}

static XEN g_set_view_files_sort_procedure(XEN proc)
{
  char *error = NULL;
  if (XEN_PROCEDURE_P(ss->view_files_sort_proc))
    {
      snd_unprotect_at(ss->view_files_sort_proc_loc);
      ss->view_files_sort_proc_loc = NOT_A_GC_LOC;
    }
  ss->view_files_sort_proc = XEN_UNDEFINED;
  if (XEN_PROCEDURE_P(proc))
    {
      error = procedure_ok(proc, 1, "file sort", "sort", 1);
      view_files_set_sort_by_proc_sensitive(error == NULL);
      if (error == NULL)
	{
	  ss->view_files_sort_proc = proc;
	  ss->view_files_sort_proc_loc = snd_protect(proc);
	}
      else 
	{
	  XEN errstr;
	  errstr = C_TO_XEN_STRING(error);
	  FREE(error);
	  return(snd_bad_arity_error(S_setB S_view_files_sort_procedure, errstr, proc));
	}
    }
  else view_files_set_sort_by_proc_sensitive(false);
  return(proc);
}

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_1(g_add_sound_file_extension_w, g_add_sound_file_extension)
XEN_NARGIFY_0(g_sound_file_extensions_w, g_sound_file_extensions)
XEN_NARGIFY_1(g_set_sound_file_extensions_w, g_set_sound_file_extensions)
XEN_NARGIFY_1(g_file_write_date_w, g_file_write_date)
XEN_ARGIFY_1(g_soundfont_info_w, g_soundfont_info)
XEN_NARGIFY_1(g_add_directory_to_view_files_list_w, g_add_directory_to_view_files_list)
XEN_NARGIFY_1(g_add_file_to_view_files_list_w, g_add_file_to_view_files_list)
XEN_ARGIFY_1(g_sound_files_in_directory_w, g_sound_files_in_directory)
XEN_ARGIFY_1(g_sound_loop_info_w, g_sound_loop_info)
XEN_ARGIFY_2(g_set_sound_loop_info_w, g_set_sound_loop_info)
XEN_NARGIFY_0(g_view_files_sort_procedure_w, g_view_files_sort_procedure)
XEN_NARGIFY_1(g_set_view_files_sort_procedure_w, g_set_view_files_sort_procedure)
XEN_NARGIFY_1(g_disk_kspace_w, g_disk_kspace)
XEN_ARGIFY_1(g_open_file_dialog_w, g_open_file_dialog)
XEN_ARGIFY_1(g_mix_file_dialog_w, g_mix_file_dialog)
XEN_ARGIFY_1(g_insert_file_dialog_w, g_insert_file_dialog)
XEN_ARGIFY_1(g_view_files_dialog_w, g_view_files_dialog)
XEN_ARGIFY_1(g_edit_header_dialog_w, g_edit_header_dialog)
XEN_ARGIFY_1(g_save_selection_dialog_w, g_save_selection_dialog)
XEN_ARGIFY_1(g_save_sound_dialog_w, g_save_sound_dialog)
XEN_NARGIFY_2(g_info_dialog_w, g_info_dialog)
#else
#define g_add_sound_file_extension_w g_add_sound_file_extension
#define g_sound_file_extensions_w g_sound_file_extensions
#define g_set_sound_file_extensions_w g_set_sound_file_extensions
#define g_file_write_date_w g_file_write_date
#define g_soundfont_info_w g_soundfont_info
#define g_add_directory_to_view_files_list_w g_add_directory_to_view_files_list
#define g_add_file_to_view_files_list_w g_add_file_to_view_files_list
#define g_sound_files_in_directory_w g_sound_files_in_directory
#define g_sound_loop_info_w g_sound_loop_info
#define g_set_sound_loop_info_w g_set_sound_loop_info
#define g_view_files_sort_procedure_w g_view_files_sort_procedure
#define g_set_view_files_sort_procedure_w g_set_view_files_sort_procedure
#define g_disk_kspace_w g_disk_kspace
#define g_open_file_dialog_w g_open_file_dialog
#define g_mix_file_dialog_w g_mix_file_dialog
#define g_insert_file_dialog_w g_insert_file_dialog
#define g_view_files_dialog_w g_view_files_dialog
#define g_edit_header_dialog_w g_edit_header_dialog
#define g_save_selection_dialog_w g_save_selection_dialog
#define g_save_sound_dialog_w g_save_sound_dialog
#define g_info_dialog_w g_info_dialog
#endif

void g_init_file(void)
{
  XEN_DEFINE_PROCEDURE(S_add_sound_file_extension,         g_add_sound_file_extension_w,         1, 0, 0, H_add_sound_file_extension);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_sound_file_extensions, g_sound_file_extensions_w, H_sound_file_extensions,
				   S_setB S_sound_file_extensions, g_set_sound_file_extensions_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE(S_file_write_date,                  g_file_write_date_w,                  1, 0, 0, H_file_write_date);
  XEN_DEFINE_PROCEDURE(S_soundfont_info,                   g_soundfont_info_w,                   0, 1, 0, H_soundfont_info);
  XEN_DEFINE_PROCEDURE(S_add_directory_to_view_files_list, g_add_directory_to_view_files_list_w, 1, 0, 0, H_add_directory_to_view_files_list);
  XEN_DEFINE_PROCEDURE(S_add_file_to_view_files_list,      g_add_file_to_view_files_list_w,      1, 0, 0, H_add_file_to_view_files_list);
  XEN_DEFINE_PROCEDURE(S_sound_files_in_directory,         g_sound_files_in_directory_w,         0, 1, 0, H_sound_files_in_directory);
  XEN_DEFINE_PROCEDURE(S_open_file_dialog,                 g_open_file_dialog_w,                 0, 1, 0, H_open_file_dialog);
  XEN_DEFINE_PROCEDURE(S_mix_file_dialog,                  g_mix_file_dialog_w,                  0, 1, 0, H_mix_file_dialog);
  XEN_DEFINE_PROCEDURE(S_insert_file_dialog,               g_insert_file_dialog_w,               0, 1, 0, H_insert_file_dialog);
  XEN_DEFINE_PROCEDURE(S_view_files_dialog,                g_view_files_dialog_w,                0, 1, 0, H_view_files_dialog);
  XEN_DEFINE_PROCEDURE(S_edit_header_dialog,               g_edit_header_dialog_w,               0, 1, 0, H_edit_header_dialog);
  XEN_DEFINE_PROCEDURE(S_save_selection_dialog,            g_save_selection_dialog_w,            0, 1, 0, H_save_selection_dialog);
  XEN_DEFINE_PROCEDURE(S_save_sound_dialog,                g_save_sound_dialog_w,                0, 1, 0, H_save_sound_dialog);
  XEN_DEFINE_PROCEDURE(S_info_dialog,                      g_info_dialog_w,                      2, 0, 0, H_info_dialog);
  XEN_DEFINE_PROCEDURE(S_disk_kspace,                      g_disk_kspace_w,                      1, 0, 0, H_disk_kspace);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_sound_loop_info,      g_sound_loop_info_w, H_sound_loop_info,
				   S_setB S_sound_loop_info, g_set_sound_loop_info_w,  0, 1, 1, 1);

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
upon File:Save as or " S_save_sound_as " completion."

  #define H_before_save_as_hook " (index filename selection srate type format comment): called \
before File:Save as or " S_save_sound_as ". Provides a way to fixup a sound just before it is saved."

  open_hook =           XEN_DEFINE_HOOK(S_open_hook, 1,           H_open_hook);           /* arg = filename */
  close_hook =          XEN_DEFINE_HOOK(S_close_hook, 1,          H_close_hook);          /* arg = sound index */
  just_sounds_hook =    XEN_DEFINE_HOOK(S_just_sounds_hook, 1,    H_just_sounds_hook);    /* arg = filename */
  bad_header_hook =     XEN_DEFINE_HOOK(S_bad_header_hook, 1,     H_bad_header_hook);     /* arg = filename */
  after_save_as_hook =  XEN_DEFINE_HOOK(S_after_save_as_hook, 3,  H_after_save_as_hook);  /* args: index filename from-dialog */
  before_save_as_hook = XEN_DEFINE_HOOK(S_before_save_as_hook, 7, H_before_save_as_hook); /* args: index filename selection srate type format comment */

  #define H_open_raw_sound_hook S_open_raw_sound_hook " (filename current-choices): called when a headerless sound file is opened. \
Its result can be a list describing the raw file's attributes (thereby bypassing the Raw File Dialog and so on). \
The list (passed to subsequent hook functions as 'current-choice') is interpreted as \
(list chans srate data-format data-location data-length) where trailing elements can \
be omitted (location defaults to 0, and length defaults to the file length in bytes)."

  open_raw_sound_hook = XEN_DEFINE_HOOK(S_open_raw_sound_hook, 2, H_open_raw_sound_hook);    /* args = filename current-result */

  #define H_update_hook S_update_hook " (snd): called just before " S_update_sound " is called. \
The update process can  be triggered by a variety of situations, not just by " S_update_sound ". \
The hook is passed the sound's index.  If it returns #t, the update is cancelled (this is not \
recommended!); if it returns a procedure of one argument, that procedure is called upon \
completion of the update operation; its argument is the (possibly different) sound index. \
Snd tries to maintain the index across the update, but if you change the number of channels \
the newly updated sound may have a different index."

  update_hook = XEN_DEFINE_HOOK(S_update_hook, 1, H_update_hook);            /* arg = sound index */

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_view_files_sort_procedure, g_view_files_sort_procedure_w, H_view_files_sort_procedure,
                                   S_setB S_view_files_sort_procedure, g_set_view_files_sort_procedure_w,  0, 0, 1, 0);
}
