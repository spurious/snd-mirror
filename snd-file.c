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

#if HAVE_SYS_STATFS_H
  #include <sys/statfs.h>
#endif
#if HAVE_SYS_VFS_H
  #include <sys/vfs.h>
#endif
#if (__bsdi__ || HAVE_SYS_PARAM_H)
  #include <sys/param.h>
#endif
#if (defined(HAVE_SYS_MOUNT_H) || defined(__APPLE__) || defined(__bsdi__))
  #include <sys/mount.h>
#endif

#if (!HAVE_FSTATFS) && (!HAVE_STATFS)
  int disk_kspace (int fd, char *filename) {return(1234567);}
  int is_link(char *filename) {return(0);}
  int is_directory(char *filename) {return(0);}
#else

int disk_kspace (int fd, char *filename)
{
  struct statfs buf;
  int err;
#if HAVE_STATFS
  if (filename)
#if (FSTATFS_ARGS == 4)
    /* SGI case -- applies to both fstatfs and statfs */
    err = statfs(filename, &buf, sizeof(buf), 0);
#else
    err = statfs(filename, &buf);
#endif
  else
#endif
    {
      /* this block not currently used, I think */
#if (FSTATFS_ARGS == 4)
    err = fstatfs(fd, &buf, sizeof(buf), 0);
#else
    err = fstatfs(fd, &buf);
#endif
    }
  /* in 32 bit land, the number of bytes can easily go over 2^32, so we'll look at kbytes here */
  if (err == 0) 
    {
      if (buf.f_bsize == 1024) return(buf.f_bfree);
      else if (buf.f_bsize == 512) return(buf.f_bfree >> 1);
      else return((int)(buf.f_bsize * ((Float)(buf.f_bfree) / 1024.0)));
    }
  return(err);
}

int is_link(char *filename)
{
  struct stat statbuf;
  if (lstat(filename, &statbuf) >= 0) 
    return(S_ISLNK(statbuf.st_mode));
  return(0);
}

int is_directory(char *filename)
{
  struct stat statbuf;
  if (lstat(filename, &statbuf) >= 0) 
    return(S_ISDIR(statbuf.st_mode));
  return(0);
}
#endif

time_t file_write_date(char *filename)
{
  struct stat statbuf;
  int err;
  err = stat(filename, &statbuf);
  if (err < 0) return(err);
  return((time_t)(statbuf.st_mtime));
}

static file_info *make_file_info_1(char *fullname)
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
      if ((hdr->srate) == 0) hdr->srate = 1;
      hdr->chans = mus_sound_chans(fullname);
      if ((hdr->chans) == 0) hdr->chans = 1;
      hdr->format = mus_sound_data_format(fullname);
    }
  hdr->samples = mus_sound_samples(fullname); /* total samples, not per channel */
  hdr->data_location = mus_sound_data_location(fullname);
  hdr->comment = NULL;
  hdr->loops = mus_sound_loop_info(fullname);
  return(hdr);
}

file_info *copy_header(char *fullname, file_info *ohdr)
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
	  hdr->loops = (int *)CALLOC(6, sizeof(int));
	  for (i = 0; i < 6; i++) 
	    hdr->loops[i] = ohdr->loops[i];
	}
    }
  return(hdr);
}

static file_info *translate_file(char *filename, snd_state *ss, int type)
{
  char *newname, *tempname;
  file_info *hdr = NULL;
  int err, len, fd;
  len = strlen(filename);
  newname = (char *)CALLOC(len + 5, sizeof(char));
  mus_snprintf(newname, len + 5, "%s.snd", filename);
  /* too many special cases to do anything smart here -- I'll just tack on '.snd' */
  /* before calling the translator we need to check for write-access to the current directory,
   * and if none, try to find a temp directory we can write to
   */
  fd = creat(newname, 0666);
  if (fd == -1)
    {
      tempname = snd_tempnam(ss);
      fd = creat(tempname, 0666);
      if (fd == -1)
	{
	  snd_error("can't write translator temp file: %s or %s!",
		    newname, tempname);
	  return(NULL);
	}
      FREE(newname);
      newname = copy_string(tempname);
      FREE(tempname);
    }
  if (close(fd) != 0)
    snd_error("can't close %d (%s): %s [%s[%d] %s]",
	      fd, newname, strerror(errno),
	      __FILE__, __LINE__, __FUNCTION__);
  err = snd_translate(filename, newname, type);
  if (err == MUS_NO_ERROR)
    {
      err = mus_header_read(newname);
      if (err == MUS_NO_ERROR)
	{
	  hdr = make_file_info_1(newname);
	  if (hdr) ss->pending_change = newname;
	}
    }
  else snd_remove(newname);
  return(hdr);
}

static SCM open_raw_sound_hook;

file_info *make_file_info(char *fullname, snd_state *ss)
{
  file_info *hdr = NULL;
  int type = MUS_UNSUPPORTED, format = MUS_UNKNOWN;
  if (mus_file_probe(fullname))
    {
      type = mus_sound_header_type(fullname);
      if (type == MUS_ERROR) 
	type = mus_header_type();
#if (!USE_NO_GUI)
      else
	{
	  if ((mus_sound_srate(fullname) <= 0) || (mus_sound_srate(fullname) > 100000000) ||
	      (mus_sound_chans(fullname) >= 256) || (mus_sound_chans(fullname) <= 0))
	    {
	      type = mus_header_type();
	      if ((type != MUS_MIDI_SAMPLE_DUMP) && 
		  (type != MUS_IEEE) &&
		  (type != MUS_MUS10) && 
		  (type != MUS_HCOM))
		{
		  char *tmp;
		  file_info *tmp_hdr;
		  tmp_hdr = make_file_info_1(fullname);
		  tmp = raw_data_explanation(fullname, ss, tmp_hdr);
		  hdr = raw_data_dialog_to_file_info(fullname, ss, tmp);
		  if (tmp) FREE(tmp);
		  if (tmp_hdr) tmp_hdr = free_file_info(tmp_hdr);
		  return(hdr);
		}
	    }
	}
#endif
      if (type == MUS_RAW)
	{
	  SCM res = FALSE_VALUE;
	  SCM procs, arg1;
	  int len, srate, chans, data_format, data_location, bytes;

	  if (ss->reloading_updated_file)
	    {
	      /* choices already made, so just send back a header that reflects those choices */
	      if (mus_file_probe(fullname))
		return(make_file_info_1(fullname));
	      else
		{
		  snd_error("can't find raw (headerless) file %s: %s",
			    fullname, strerror(errno));
		  return(NULL);
		}
	    }
	  if (HOOKED(open_raw_sound_hook))
	    {
	      procs = HOOK_PROCEDURES (open_raw_sound_hook);
	      arg1 = TO_SCM_STRING(fullname);
	      while (NOT_NULL_P(procs))
		{
		  res = CALL_2(CAR(procs), arg1, res, S_open_raw_sound_hook);
		  procs = CDR (procs);
		}
	    }
	  if (LIST_P(res)) /* empty list ok here -> accept all current defaults */
	    {
	      len = LIST_LENGTH(res);
	      mus_header_raw_defaults(&srate, &chans, &data_format);
	      if (len > 0) chans = TO_C_INT(CAR(res));
	      if (len > 1) srate = TO_C_INT(CADR(res));
	      if (len > 2) data_format = TO_C_INT(LIST_REF(res, 2)); 
	      if (len > 3) data_location = TO_C_INT(LIST_REF(res, 3)); else data_location = 0;
	      if (len > 4) bytes = TO_C_INT(LIST_REF(res, 4)); else bytes = mus_sound_length(fullname) - data_location;
	      mus_header_set_raw_defaults(srate, chans, data_format);
	      mus_sound_override_header(fullname, srate, chans, data_format, 
					MUS_RAW, data_location,
					mus_bytes_to_samples(data_format, bytes));
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
	      str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
	      mus_snprintf(str, PRINT_BUFFER_SIZE, "No header found for %s", filename_without_home_directory(fullname));
	      hdr = raw_data_dialog_to_file_info(fullname, ss, str);
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
	      else hdr = translate_file(fullname, ss, type);
	    }
	  else 
	    snd_error("%s does not seem to be a sound file?", fullname);
	}
    }
  else
    {
      snd_error("can't find %s: %s", fullname, strerror(errno));
      return(NULL);
    }
  return(hdr);
}

file_info *make_temp_header(char *fullname, int srate, int chans, int samples, char *caller)
{
  /* make a header for Sun/NeXT, no comment, copy other old_hdr fields */
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

static dir *make_dir (char *name)
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
  int i;
  if (dp->name) FREE(dp->name);
  if (dp->files)
    {
      for (i = 0; i < dp->len; i++) 
	if (dp->files[i]) 
	  FREE(dp->files[i]);
      FREE(dp->files);
    }
  FREE(dp);
  return(NULL);
}
  
static void add_snd_file_to_dir_list(dir *dp, char *name)
{
  int i;
  dp->files[dp->len] = copy_string(name);
  dp->len++;
  if (dp->len == dp->size) 
    {
      dp->size += 32;
      dp->files = (char **)REALLOC(dp->files, dp->size*sizeof(char *));
      for (i = dp->size-32; i < dp->size; i++) dp->files[i] = NULL;
    }
}

static char **sound_file_extensions = NULL;
static int sound_file_extensions_size = 0;
static int sound_file_extensions_end = 0;

static void add_sound_file_extension(char *ext)
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
  add_sound_file_extension("au");
  add_sound_file_extension("aifc");
  add_sound_file_extension("voc");
  add_sound_file_extension("wve");
}

static int just_sounds_happy(char *filename);

dir *find_sound_files_in_dir (char *name)
{
#if (!HAVE_OPENDIR)
  return(NULL);
#else
  /* TODO: here we could insert the file-dialog (just-sounds) filter procedure, but it's also called in prevlist and sound-files-in-directory */
  struct dirent *dirp;
  DIR *dpos;
  char *dot, *sp;
  dir *dp = NULL;
  int i;
  if ((dpos = opendir(name)) != NULL)
    {
      dp = make_dir(name);
      while ((dirp = readdir(dpos)) != NULL)
	if (dirp->d_name[0] != '.')
	  {
	    dot = NULL;
	    for (sp = dirp->d_name; (*sp) != '\0'; sp++) 
	      if ((*sp) == '.') 
		dot = (++sp);
	    if (dot)
	      for (i = 0; i < sound_file_extensions_end; i++)
		if (strcmp(dot, sound_file_extensions[i]) == 0)
		  {
		    if (just_sounds_happy(dirp->d_name))
		      add_snd_file_to_dir_list(dp, dirp->d_name);
		    break;
		  }
	  }
#if defined(CLOSEDIR_VOID)
      closedir(dpos);
#else
      if (closedir(dpos) != 0) 
	snd_error("%s[%d] %s: closedir %s failed!",
		  __FILE__, __LINE__, __FUNCTION__,
		  name);
#endif
    }
  return(dp);
#endif
}

static int names_match(char *filename, char *pattern)
{
  /* just "*" for wildcards here */
  char *sn, *sp;
  sn = filename;
  sp = pattern;
  if ((!sn) || (!sp)) 
    {
      if ((sn) || (sp)) 
	return(0); 
      else return(1);
    }
  while ((*sn) && (*sp))
    {
      if ((*sp) == '*') 
	{
	  sp++; 
	  while ((*sp) == '*') sp++; 
	  if (!(*sp)) return(1);
	  while ((*sn) && ((*sn) != (*sp))) sn++;
	  if (!(*sn)) return(0);
	}
      else 
	{
	  if ((*sn) != (*sp)) return(0);
	  sn++; 
	  sp++;
	}
    }
  return(1);
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

static int add_sound_to_active_list (snd_info *sp, void *sptr1)
{
  active_sound_list *sptr = (active_sound_list *)sptr1;
  sptr->names[sptr->active_sounds] = sp->filename;
  sptr->sounds[sptr->active_sounds] = sp->index;
  (sptr->active_sounds)++;
  return(0);                        /*assume no problem -- nothing can go wrong! */
}

static char title_buffer[4 * (MUS_MAX_FILE_NAME)];

static void reflect_file_change_in_title(snd_state *ss)
{
  active_sound_list *alist;
  int i, j;
  alist = (active_sound_list *)CALLOC(1, sizeof(active_sound_list));
  alist->sounds = (int *)CALLOC(ss->max_sounds, sizeof(int));
  alist->names = (char **)CALLOC(ss->max_sounds, sizeof(char *));
  map_over_sounds(ss, add_sound_to_active_list, alist);
  mus_snprintf(title_buffer, 4 * (MUS_MAX_FILE_NAME),
	  "%s%s", 
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
  set_title(ss, title_buffer);
  FREE(alist->sounds);
  FREE(alist->names);
  FREE(alist);
}

static char *memo_file_name(snd_info *sp)
{
  char *newname;
  int len;
  len = strlen(sp->filename);
  newname = (char *)CALLOC(len + 5, sizeof(char));
  mus_snprintf(newname, len + 5, "%s.scm", sp->filename);
  return(newname);
}

static void read_memo_file(snd_info *sp)
{
  /* sp->filename + ".scm" = possible memo file (memo-sound set in this file, snd_open_file_1) */
  char *newname;
  newname = memo_file_name(sp);
  if (file_write_date(newname) >= sp->write_date)
    snd_load_file(newname);
  FREE(newname);
}

static SCM memo_sound, open_hook, close_hook, just_sounds_hook;

static int dont_open(char *file)
{
  char *mcf = NULL;
  SCM res = FALSE_VALUE, fstr;
  if (HOOKED(open_hook))
    {
      mcf = mus_expand_filename(file);
      fstr = TO_SCM_STRING(mcf);
      if (mcf) FREE(mcf);
      res = g_c_run_or_hook(open_hook,
			    LIST_1(fstr),
			    S_open_hook);
    }
  return(TRUE_P(res));
}

static int dont_close(snd_info *sp)
{
  SCM res = FALSE_VALUE;
  if (HOOKED(close_hook))
    res = g_c_run_or_hook(close_hook,
			  LIST_1(TO_SMALL_SCM_INT(sp->index)),
			  S_close_hook);
  return(TRUE_P(res));
}

static int just_sounds_happy(char *filename)
{
  SCM res = TRUE_VALUE;
  if (HOOKED(just_sounds_hook))
    res = g_c_run_or_hook(just_sounds_hook,
			  LIST_1(TO_SCM_STRING(filename)),
			  S_just_sounds_hook);
  return(TRUE_P(res));
}


static void greet_me(snd_state *ss, char *shortname);
static void remember_me(snd_state *ss, char *shortname, char *fullname);


static snd_info *snd_open_file_1 (char *filename, snd_state *ss, int select, int read_only)
{
  snd_info *sp;
  char *mcf = NULL;
  int files, val;
  if (dont_open(filename)) return(NULL);
  sp = add_sound_window(mcf = mus_expand_filename(filename), ss, read_only); /* snd-xsnd.c -> make_file_info */
  if (mcf) FREE(mcf);
  if (sp)
    {
      SET_OBJECT_REF(memo_sound, TO_SMALL_SCM_INT(sp->index));
      sp->write_date = file_write_date(sp->filename);
      sp->need_update = 0;
      ss->active_sounds++;
      files = ss->active_sounds;
      if (files == 1) reflect_file_open_in_menu();
      reflect_equalize_panes_in_menu(active_channels(ss, WITHOUT_VIRTUAL_CHANNELS) > 1);
      reflect_file_change_in_title(ss);
      unlock_ctrls(sp);
      greet_me(ss, sp->short_filename);
    }
  map_over_separate_chans(ss, channel_open_pane, NULL);
  map_over_separate_chans(ss, channel_unlock_pane, NULL);
  if (sp) 
    {
      if (select) select_channel(sp, 0);
      read_memo_file(sp);
      if ((sp->channel_style != CHANNELS_SEPARATE) && 
	  (sp->nchans > 1)) 
	{
	  val = sp->channel_style;
	  sp->channel_style = CHANNELS_SEPARATE; 
	  if (val == CHANNELS_COMBINED)
	    combine_sound(sp);
	  else superimpose_sound(sp);
	}
    }

  return(sp);
}

snd_info *snd_open_file (char *filename, snd_state *ss, int read_only) {return(snd_open_file_1(filename, ss, TRUE, read_only));}
snd_info *snd_open_file_unselected (char *filename, snd_state *ss, int read_only) {return(snd_open_file_1(filename, ss, FALSE, read_only));}

void snd_close_file(snd_info *sp, snd_state *ss)
{
  int files;
  if (dont_close(sp)) return;
  sp->inuse = 0;
  remember_me(ss, sp->short_filename, sp->filename);
  if (sp->playing) stop_playing_sound(sp);
  clear_minibuffer(sp);
  if (sp == selected_sound(ss)) 
    ss->selected_sound = NO_SELECTION;
  free_snd_info(sp);
  ss->active_sounds--;
  files = ss->active_sounds;
  if (files == 0) reflect_file_lack_in_menu();
  reflect_file_change_in_title(ss);
  reflect_equalize_panes_in_menu(active_channels(ss, WITHOUT_VIRTUAL_CHANNELS) > 1);
  if (!(selection_is_active())) 
    reflect_edit_without_selection_in_menu();
}


int copy_file(char *oldname, char *newname)
{
  /* make newname a copy of oldname */
  int ifd, ofd;
  long bytes, wb, total;
  char *buf = NULL;
  total = 0;
  ifd = open(oldname, O_RDONLY, 0);
  if (ifd == -1) return(MUS_CANT_OPEN_FILE);
  ofd = creat(newname, 0666);
  if (ofd == -1) 
    {
      if (close(ifd) != 0)
	snd_error("can't close %d (%s): %s [%s[%d] %s]",
		  ifd, oldname, strerror(errno),
		  __FILE__, __LINE__, __FUNCTION__);
      return(MUS_CANT_OPEN_FILE);
    }
  buf = (char *)CALLOC(8192, sizeof(char));
  while ((bytes = read(ifd, buf, 8192)))
    {
      total += bytes;
      wb = write(ofd, buf, bytes);
      if (wb != bytes) 
	{
	  if (close(ofd) != 0)
	    snd_error("can't close %d (%s): %s [%s[%d] %s]",
		      ofd, newname, strerror(errno),
		      __FILE__, __LINE__, __FUNCTION__);
	  if (close(ifd) != 0)
	    snd_error("can't close %d (%s): %s [%s[%d] %s]",
		      ifd, oldname, strerror(errno),
		      __FILE__, __LINE__, __FUNCTION__);
	  FREE(buf); 
	  return(MUS_WRITE_ERROR);
	}
    }
  if (close(ifd) != 0)
    snd_error("can't close %d (%s): %s [%s[%d] %s]",
	      ifd, oldname, strerror(errno),
	      __FILE__, __LINE__, __FUNCTION__);
  total = total >> 10;
  wb = disk_kspace(ofd, newname);
  if (wb < 0) 
    snd_error(strerror(errno));
  else
    if (total > wb) 
      snd_error("disk nearly full: used %d Kbytes leaving %d",
		(int)total, (int)wb);
  FREE(buf);
  if (close(ofd) != 0)
    snd_error("can't close %d (%s): %s [%s[%d] %s]",
	      ofd, newname, strerror(errno),
	      __FILE__, __LINE__, __FUNCTION__);
  return(MUS_NO_ERROR);
}

int move_file(char *oldfile, char *newfile)
{
  int err;
  err = 0;
  if ((err = (rename(oldfile, newfile))))
    {
      if (errno == EXDEV)
	{
	  err = copy_file(oldfile, newfile);
	  if (!err) 
	    snd_remove(oldfile);
	}
    }
  if (err != 0)
    snd_error("trouble overwriting %s: %s", newfile, strerror(errno));
  return(err);
}

#define TEMP_SOUND_INDEX 123456
/* just a marker for debugging */

snd_info *make_sound_readable(snd_state *ss, char *filename, int post_close)
{
  /* conjure up just enough Snd structure to make this sound readable by the edit-tree readers */
  snd_info *sp;
  chan_info *cp;
  file_info *hdr = NULL;
  snd_data *sd;
  int *datai;
  int i, fd, len;
  /* we've already checked that filename exists */
  hdr = make_file_info_1(filename);
  if (hdr == NULL)
    {
      if (ss->catch_exists)
	ERROR(NO_SUCH_FILE,
	      LIST_2(TO_SCM_STRING(__FUNCTION__),
			TO_SCM_STRING(ss->catch_message)));
      return(NULL);
    }
  sp = (snd_info *)CALLOC(1, sizeof(snd_info));
  sp->nchans = mus_sound_chans(filename);
  sp->allocated_chans = sp->nchans;
  sp->chans = (chan_info **)CALLOC(sp->nchans, sizeof(chan_info *));
  sp->hdr = hdr;
  sp->inuse = 1;
  sp->state = ss;
  sp->expand_control = 1.0;
  sp->expand_control_p = 0;
  sp->amp_control = 1.0;
  sp->speed_control = 1.0;
  sp->speed_control_direction = 1;
  sp->contrast_control_p = 0;
  sp->contrast_control = 0.0;
  sp->reverb_control_p = 0;
  sp->reverb_control_scale = 0.0;
  sp->filter_control_p = 0;
  sp->search_proc = UNDEFINED_VALUE;
  sp->prompt_callback = UNDEFINED_VALUE;
  sp->index = TEMP_SOUND_INDEX;
  sp->sgx = NULL;
  len = (hdr->samples) / (hdr->chans);
  for (i = 0; i < sp->nchans; i++)
    {
      cp = make_chan_info(NULL, i, sp, ss);
      FREE((cp->cgx)->ax);
      FREE(cp->cgx);
      cp->cgx = NULL;
      sp->chans[i] = cp;
      add_channel_data_1(cp, sp, WITHOUT_GRAPH);
      set_initial_ed_list(cp, len - 1);
      cp->edit_size = 1;
      cp->sound_size = 1;
      fd = snd_open_read(ss, filename); /* sends the error if any */
      if (fd != -1)
	{
	  mus_file_set_descriptors(fd,
				   filename,
				   hdr->format,
				   mus_sound_datum_size(filename),
				   hdr->data_location,
				   hdr->chans,
				   hdr->type);
	  datai = make_file_state(fd, hdr, i, 
				  (post_close) ? MAX_BUFFER_SIZE : MIX_FILE_BUFFER_SIZE);
	  cp->sounds[0] = make_snd_data_file(filename, datai,
					     MUS_SAMPLE_ARRAY(datai[file_state_channel_offset(i)]),
					     copy_header(hdr->name, hdr),
					     DONT_DELETE_ME, cp->edit_ctr, i);
	  if (post_close) 
	    {
	      if (mus_file_close(fd) != 0)
		snd_error("can't close %d (%s): %s [%s[%d] %s]",
			  fd, filename, strerror(errno),
			  __FILE__, __LINE__, __FUNCTION__);
	      sd = cp->sounds[0]; 
	      sd->open = FD_CLOSED; 
	      set_file_state_fd(datai, -1);
	    }
	  /* this is not as crazy as it looks -- we've read in the first 64K (or whatever) samples,
	   * and may need this file channel for other opens, so this file can be closed until reposition_file_state_buffers
	   */
	}
    }
  sp->active = 1;
  return(sp);
}

static snd_info *snd_update_1(snd_state *ss, snd_info *sp, char *ur_filename)
{
  /* we can't be real smart here because the channel number may have changed and so on */
  Float *axis_data;
  int *ffts, *waves;
  int i, j, old_chans, old_sync, old_combine, need_update, read_only;
  Float duration;
  chan_info *cp;
  axis_info *ap;
  snd_info *nsp;
  char *filename;
  need_update = 0;
  filename = copy_string(ur_filename);
  old_chans = sp->nchans;
  old_sync = sp->sync;
  old_combine = sp->channel_style;
  read_only = sp->read_only;
  axis_data = (Float *)CALLOC(4 * old_chans, sizeof(Float));
  ffts = (int *)CALLOC(old_chans, sizeof(int));
  waves = (int *)CALLOC(old_chans, sizeof(int));
  for (i = 0; i < old_chans; i++)
    {
      cp = sp->chans[i];
      ap = cp->axis;
      axis_data[(i * 4) + 0] = ap->x0;
      axis_data[(i * 4) + 1] = ap->x1;
      axis_data[(i * 4) + 2] = ap->y0;
      axis_data[(i * 4) + 3] = ap->y1;
      waves[i] = cp->graph_time_p;
      ffts[i] = cp->graph_transform_p;
    }
  snd_close_file(sp, ss);
  /* this normalizes the fft/lisp/wave state so we need to reset it after reopen */
  alert_new_file();
  /* if it's a raw sound file being updated, we don't want to re-confirm the sound format and whatnot
   * so we set the use-raw-defaults flag in a sort of ugly wrapper around the snd_open_file
   */
  ss->reloading_updated_file = TRUE;
  nsp = snd_open_file(filename, ss, read_only);
  ss->reloading_updated_file = FALSE;
  /* end wrapper */
  duration = (Float)mus_sound_samples(filename) / (Float)(mus_sound_chans(filename) * mus_sound_srate(filename));
  for (i = 0, j = 0; i < nsp->nchans; i++)
    {
      cp = nsp->chans[i];
      if (duration < axis_data[(j * 4) + 0]) 
	axis_data[(j * 4) + 0] = duration - .1;
      if (duration < axis_data[(j * 4) + 1]) 
	axis_data[(j * 4) + 1] = duration;
      set_axes(cp,
	       axis_data[(j * 4) + 0], 
	       axis_data[(j * 4) + 1], 
	       axis_data[(j * 4) + 2], 
	       axis_data[(j * 4) + 3]);
      update_graph(cp, NULL); /* get normalized state before messing with it */
      if (ffts[j]) 
	{
	  fftb(cp, TRUE); 
	  need_update = 1;
	}
      if (!(waves[j])) 
	{
	  waveb(cp, FALSE); 
	  need_update = 1;
	}
      if (j < (old_chans - 1)) j++;
    }
  if (nsp->channel_style != old_combine) set_sound_channel_style(nsp, old_combine);
  if (nsp->sync != old_sync) syncb(nsp, old_sync);
  if (need_update) 
    for (i = 0; i < nsp->nchans; i++) 
      update_graph(nsp->chans[i], NULL);
  FREE(axis_data);
  FREE(waves);
  FREE(ffts);
  FREE(filename);
  return(nsp);
}

void snd_update(snd_state *ss, snd_info *sp)
{
  int app_x, app_y;
  if (sp->edited_region) return;
  if (mus_file_probe(sp->filename) == 0)
    {
      /* user deleted file while editing it? */
      report_in_minibuffer_and_save(sp, "%s no longer exists!", sp->short_filename);
      return;
    }
  app_x = widget_width(MAIN_SHELL(ss));
  app_y = widget_height(MAIN_SHELL(ss));
  sp = snd_update_1(ss, sp, sp->filename);
  report_in_minibuffer(sp, "updated %s", sp->short_filename);
  set_widget_size(MAIN_SHELL(ss), app_x, app_y);
}

char *update_chan_stats(chan_info *cp)
{
  char *desc;
  char *vals[8];
  int i;
  desc = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  vals[0] = kmg(cp->stats[ARRAY_USAGE]);
  vals[1] = kmg(cp->stats[ARRAYS_ACTIVE]);
  vals[2] = kmg(cp->stats[FILE_USAGE]);
  vals[3] = kmg(cp->stats[TEMP_USAGE]);
  vals[4] = kmg(cp->stats[TEMPS_ACTIVE]);
  vals[5] = kmg(cp->stats[TEMPS_OPEN]);
  vals[6] = kmg(cp->stats[AMP_ENVS_ACTIVE]);
  vals[7] = kmg(cp->stats[AMP_ENV_USAGE]);
  if (cp->stats[TEMPS_ACTIVE] != cp->stats[TEMPS_OPEN])
    mus_snprintf(desc, PRINT_BUFFER_SIZE, "%s %d: %s (%s), %s, %s (%s, %s), %s %s\n",
	    (cp->sound)->short_filename,
	    cp->chan + 1,
	    vals[0], vals[1], vals[2], vals[3], vals[4], vals[5], vals[6], vals[7]);
  else
    mus_snprintf(desc, PRINT_BUFFER_SIZE, "%s %d: %s (%s), %s, %s (%s), %s %s\n",
	    (cp->sound)->short_filename,
	    cp->chan + 1,
	    vals[0], vals[1], vals[2], vals[3], vals[4], vals[6], vals[7]);
  for (i = 0; i < 8; i++) free(vals[i]);
  return(desc);
}

/* View:Files lists */

static int prevfile_size = 0;
static int curfile_size = 0;
static char **curnames = NULL;
static char **prevnames = NULL;
static char **prevfullnames = NULL;
static int *a_big_star = NULL;
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
  snd_state *ss;
  ss = get_global_state();
  sp = find_sound(ss, curnames[pos]);
  if (sp) return(sp->filename);
  return(NULL);
}

char *view_curfiles_name(int pos)
{
  char *str;
  str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  mus_snprintf(str, PRINT_BUFFER_SIZE, "%s%s", curnames[pos], (a_big_star[pos]) ? "*" : "");
  return(str);
}

void view_curfiles_play(snd_state *ss, int pos, int play)
{
  snd_info *sp;
  sp = find_sound(ss, curnames[pos]);
  if (sp)
    {
      if (sp->playing) stop_playing_sound(sp);
      if (play)
	{
	  play_sound(sp, 0, NO_END_SPECIFIED, IN_BACKGROUND, TO_SCM_INT(AT_CURRENT_EDIT_POSITION), "current files play", 0);
	  set_play_button(sp, 1);
	}
      else set_play_button(sp, 0);
    }
}

void view_curfiles_select(snd_state *ss, int pos)
{
  snd_info *sp, *osp;
  curfile_highlight(ss, pos);
  sp = find_sound(ss, curnames[pos]);
  osp = any_selected_sound(ss);
  if (sp != osp)
    {
      select_channel(sp, 0);
      equalize_sound_panes(ss, sp, sp->chans[0], FALSE);
      /* goto_graph(sp->chans[0]); */
    }
}

void view_curfiles_save(snd_state *ss, int pos)
{
  snd_info *sp;
  sp = find_sound(ss, curnames[pos]);
  if (sp) save_edits(sp, NULL);
}

void view_prevfiles_select(snd_state *ss, int pos)
{
  snd_info *sp;
  sp = snd_open_file(prevfullnames[pos], ss, FALSE);
  if (sp) select_channel(sp, 0); 
}

int view_prevfiles_play(snd_state *ss, int pos, int play)
{
  static snd_info *play_sp;
  if (play)
    {
      if (play_sp)
	{
	  if (play_sp->playing) return(1); /* can't play two of these at once */
	  if (strcmp(play_sp->short_filename, prevnames[pos]) != 0)
	    {
	      completely_free_snd_info(play_sp);
	      play_sp = NULL;
	    }
	}
      if (!play_sp) 
	{
	  if (mus_file_probe(prevfullnames[pos]))
	    play_sp = make_sound_readable(ss, prevfullnames[pos], FALSE);
	  else snd_error("can't find %s: %s ", prevfullnames[pos], strerror(errno));
	}
      if (play_sp)
	{
	  play_sp->short_filename = prevnames[pos];
	  play_sp->filename = NULL;
	  play_sound(play_sp, 0, NO_END_SPECIFIED, IN_BACKGROUND, TO_SCM_INT(AT_CURRENT_EDIT_POSITION), "previous files play", 0);
	}
      else return(1); /* can't find or setup file */
    }
  else
    { /* play toggled off */
      if ((play_sp) && (play_sp->playing)) 
	stop_playing_sound(play_sp);
    }
  return(0);
}

void file_unprevlist(char *filename)
{
  int i, j;
  i = find_prevfile_regrow(filename);
  if (i != -1)
    {
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

static void file_prevlist(char *filename, char *fullname)
{
  int k, i, new_size;
  if (find_prevfile_regrow(filename) != -1) return;
  prevfile_end++;
  if (prevfile_end == prevfile_size)
    {
      new_size = prevfile_size + 32;
      if (prevfile_size == 0)
	{
	  prevnames = (char **)CALLOC(new_size, sizeof(char *));
	  prevfullnames = (char **)CALLOC(new_size, sizeof(char *));
	  prevtimes = (int *)CALLOC(new_size, sizeof(char *));
	}
      else
	{
	  prevnames = (char **)REALLOC(prevnames, new_size * sizeof(char *));
	  prevfullnames = (char **)REALLOC(prevfullnames, new_size * sizeof(char *));
	  prevtimes = (int *)REALLOC(prevtimes, new_size * sizeof(int *));
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

void update_prevfiles(snd_state *ss)
{
  if (file_dialog_is_active()) 
    make_prevfiles_list(ss);
}

void add_directory_to_prevlist(snd_state *ss, char *dirname)
{
  dir *sound_files = NULL;
  char *fullpathname = NULL;
  char **fullnames;
  int i, end;
  sound_files = find_sound_files_in_dir(dirname);
  if ((sound_files) && (sound_files->len > 0))
    {
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
      if (file_dialog_is_active()) 
	make_prevfiles_list(ss);
      free_dir(sound_files);
      FREE(fullpathname);
    }
  if (file_dialog_is_active()) make_prevfiles_list(ss);
}

static void remember_me(snd_state *ss, char *shortname, char *fullname)
{
  int i, j;
  file_prevlist(shortname, fullname);
  i = find_curfile_regrow(shortname);
  if (i != -1)
    {
      if (curnames[i]) FREE(curnames[i]);
      curnames[i] = NULL;
      for (j = i; j < curfile_end-1; j++)
	{
	  curnames[j] = curnames[j + 1];
	  a_big_star[j] = a_big_star[j + 1];
	}
      curnames[curfile_end - 1] = NULL;
      curfile_end--;
    }
  if (file_dialog_is_active())
    {
      make_curfiles_list(ss);
      make_prevfiles_list(ss);
    }
}

static void greet_me(snd_state *ss, char *shortname)
{
  int i, new_size;
  if (curfile_end == curfile_size)
    {
      new_size = curfile_size + 32;
      if (curfile_size == 0)
	{
	  curnames = (char **)CALLOC(new_size, sizeof(char *));
	  a_big_star = (int *)CALLOC(new_size, sizeof(int *));
	}
      else
	{
	  curnames = (char **)REALLOC(curnames, new_size * sizeof(char *));
	  a_big_star = (int *)REALLOC(a_big_star, new_size * sizeof(int *));
	  for (i = curfile_size; i < new_size; i++) 
	    {
	      curnames[i] = NULL; 
	      a_big_star[i] = 0;
	    }
	}
      make_cur_name_row(curfile_size, new_size);
      curfile_size = new_size;
    }
  curnames[curfile_end] = copy_string(shortname);
  a_big_star[curfile_end] = 0;
  curfile_end++;
  if (max_curfile_end < curfile_end)
    max_curfile_end = curfile_end;
  file_unprevlist(shortname);
  if (file_dialog_is_active())
    {
      make_curfiles_list(ss);
      make_prevfiles_list(ss);
    }
}

void init_curfiles(int size)
{
  if (curfile_size == 0)
    {
      curfile_size = size;
      curnames = (char **)CALLOC(curfile_size, sizeof(char *));
      a_big_star = (int *)CALLOC(curfile_size, sizeof(int *));
    }
}

void init_prevfiles(int size)
{
  if (prevfile_size == 0)
    {
      prevfile_size = size;
      prevnames = (char **)CALLOC(prevfile_size, sizeof(char *));
      prevfullnames = (char **)CALLOC(prevfile_size, sizeof(char *));
      prevtimes = (int *)CALLOC(prevfile_size, sizeof(char *));
    }
}

void make_a_big_star_outa_me(char *shortname, int big_star)
{
  int i;
  i = find_curfile_regrow(shortname);
  if ((i != -1) && 
      (a_big_star[i] != big_star))
    {
      if (file_dialog_is_active())
	view_curfiles_set_row_name(i);
      a_big_star[i] = big_star;
    }
}

int find_curfile_regrow(char *shortname)
{
  int i;
  for (i = 0; i < curfile_end; i++)
    if (strcmp(curnames[i], shortname) == 0) 
      return(i);
  return(-1);
}

int find_prevfile_regrow(char *shortname)
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
      fprintf(fd, "(%s \"%s\")\n",
	      S_preload_file,
	      prevfullnames[i]);
}

void clear_prevlist(snd_state *ss)
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
      make_prevfiles_list(ss);
    }
}

void update_prevlist(snd_state *ss)
{
  /* here we need the file's full name */
  int i, j, fd;
  if (prevnames)
    {
      for (i = 0; i <= prevfile_end; i++)
	if (prevnames[i]) 
	  {
	    fd = open(prevfullnames[i], O_RDONLY, 0);
	    if (fd == -1) 
	      {
		FREE(prevnames[i]); 
		prevnames[i] = NULL;
		FREE(prevfullnames[i]); 
		prevfullnames[i] = NULL;
	      }
	    else 
	      if (close(fd) != 0)
		snd_error("can't close %d (%s): %s [%s[%d] %s]",
			  fd, prevfullnames[i], strerror(errno),
			  __FILE__, __LINE__, __FUNCTION__);
	  }
      for (i = 0, j = 0; i <= prevfile_end; i++)
	if (prevnames[i])
	  {
	    if (i != j) 
	      {
		prevnames[j] = prevnames[i]; 
		prevfullnames[j] = prevfullnames[i];
		prevtimes[j] = prevtimes[i];
	      }
	    j++;
	  }
      prevfile_end = j - 1;
      if (file_dialog_is_active()) make_prevfiles_list(ss);
    }
}


/* sort prevfiles list by name (aphabetical), or some number (date written, size, entry order, srate? type?) */

typedef struct {
  int vals, times;
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
  if (d1->vals > d2->vals) 
    return(1); 
  else 
    {
      if (d1->vals == d2->vals) 
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

void make_prevfiles_list_1(snd_state *ss)
{
  heapdata **data;
  int i, len;
  if (prevfile_end >= 0)
    {
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
	    data[i]->vals = mus_sound_samples(prevfullnames[i]);
	  qsort((void *)data, prevfile_end + 1, sizeof(heapdata *), greater_compare);
	  break;
	case 4:
	  for (i = 0; i <= prevfile_end; i++) 
	    data[i]->vals = prevtimes[i];
	  qsort((void *)data, prevfile_end + 1, sizeof(heapdata *), less_compare);
	  break;
	case 5:
	  if (PROCEDURE_P(ss->file_sort_proc))
	    {
	      SCM file_list;
	      int j;
	      char *name;
	      file_list = EMPTY_LIST;
	      for (i = prevfile_end; i >= 0; i--) 
		file_list = CONS(TO_SCM_STRING(prevfullnames[i]), file_list);
	      file_list = CALL_1(ss->file_sort_proc, file_list, "previous files sort");
	      if (LIST_P(file_list))
		{
		  for (i = 0; (i < len) && (NOT_NULL_P(file_list)); i++, file_list = CDR(file_list))
		    {
		      name = TO_C_STRING(CAR(file_list));
		      for (j = 0; j < len; j++)
			if (strcmp(data[j]->a2, name) == 0)
			  {
			    prevnames[i] = data[j]->a1;
			    prevfullnames[i] = data[j]->a2;
			    prevtimes[i] = data[j]->times;
			  }
		    }
		}
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

static SCM g_previous_files_sort_procedure(void)
{
  #define H_previous_files_sort_procedure "(" S_previous_files_sort_procedure ") -> file sort procedure for the current files viewer"
  snd_state *ss;
  ss = get_global_state();
  return(ss->file_sort_proc);
}

static SCM g_set_previous_files_sort_procedure(SCM proc)
{
  snd_state *ss;
  char *error = NULL;
  SCM errstr;
  ss = get_global_state();
  if (PROCEDURE_P(ss->file_sort_proc))
    snd_unprotect(ss->file_sort_proc);
  ss->file_sort_proc = UNDEFINED_VALUE;
  error = procedure_ok(proc, 1, "file sort", "sort", 1);
  if (error == NULL)
    {
      ss->file_sort_proc = proc;
      snd_protect(proc);
      set_file_sort_sensitive(TRUE);
    }
  else 
    {
      set_file_sort_sensitive(FALSE);
      errstr = TO_SCM_STRING(error);
      FREE(error);
      return(snd_bad_arity_error("set-" S_previous_files_sort_procedure, errstr, proc));
    }
  return(proc);
}


/* -------- file dialog header/data choices */

#define NUM_HEADER_TYPES 7
#define NUM_NEXT_FORMATS 8
#define NUM_IRCAM_FORMATS 5
#define NUM_WAVE_FORMATS 7
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

int num_header_types(void) {return(NUM_HEADER_TYPES);}


static char *header_short_names[NUM_HEADER_TYPES] = {"sun  ", "aifc ", "wave ", "raw  ", "aiff ", "ircam", "nist "};
char *header_short_name(int i) 
{
  if ((i >= 0) && 
      (i < NUM_HEADER_TYPES)) 
    return(header_short_names[i]); 
  else return("unknown type");
}

static char *next_data_formats[NUM_NEXT_FORMATS] = {"short", "mulaw", "signed byte  ", "float", "long", "alaw", "24-bit", "double"};
static char *ircam_data_formats[NUM_IRCAM_FORMATS] = {"short", "mulaw", "float        ", "long", "alaw"};
static char *wave_data_formats[NUM_WAVE_FORMATS] = {"mulaw", "alaw", "unsigned byte", "short", "long", "float", "24-bit"};
static char *aifc_data_formats[NUM_AIFC_FORMATS] = {"short", "mulaw", "signed byte  ", "long", "alaw", "24 bit",
						    "float", "double", "unsigned byte", "short swapped",
						    "long swapped", "24-bit swapped", "unsigned short"};
static char *aiff_data_formats[NUM_AIFF_FORMATS] = {"short", "long", "signed byte", "24 bit"};
static char *nist_data_formats[NUM_NIST_FORMATS] = {"BE short", "LE short", "BE int", "LE int", "8-bit", "BE 24-bit", "LE 24-bit"};
static char *raw_data_formats[NUM_RAW_FORMATS] =   {"BE short", "mulaw", "byte", "BE float", "BE int", "alaw", "char", "BE 24-bit",
						    "BE double", "LE short", "LE int", "LE float", "LE double", "BE unsigned short",
						    "LE unsigned short", "LE 24-bit", "BE int normalized", "LE int normalized"};

static int next_dfs[NUM_NEXT_FORMATS] = {MUS_BSHORT, MUS_MULAW, MUS_BYTE, MUS_BFLOAT, MUS_BINT, MUS_ALAW,
					 MUS_B24INT, MUS_BDOUBLE};
static int ircam_dfs[NUM_IRCAM_FORMATS] = {MUS_BSHORT, MUS_MULAW, MUS_BFLOAT, MUS_BINT, MUS_ALAW};
static int wave_dfs[NUM_WAVE_FORMATS] = {MUS_MULAW, MUS_ALAW, MUS_UBYTE, MUS_LSHORT,
					 MUS_LINT, MUS_LFLOAT, MUS_L24INT};
static int aifc_dfs[NUM_AIFC_FORMATS] = {MUS_BSHORT, MUS_MULAW, MUS_BYTE, MUS_BINT, MUS_ALAW, MUS_B24INT,
					 MUS_BFLOAT, MUS_BDOUBLE, MUS_UBYTE, MUS_LSHORT,
					 MUS_LINT, MUS_L24INT, MUS_UBSHORT};
static int aiff_dfs[NUM_AIFF_FORMATS] = {MUS_BSHORT, MUS_BINT, MUS_BYTE, MUS_B24INT};
static int nist_dfs[NUM_NIST_FORMATS] = {MUS_BSHORT, MUS_LSHORT, MUS_BINT, MUS_LINT,
					 MUS_BYTE, MUS_B24INT, MUS_L24INT};
static int raw_dfs[NUM_RAW_FORMATS] = {MUS_BSHORT, MUS_MULAW, MUS_BYTE, MUS_BFLOAT, MUS_BINT, MUS_ALAW,
				       MUS_UBYTE, MUS_B24INT, MUS_BDOUBLE, MUS_LSHORT, MUS_LINT,
				       MUS_LFLOAT, MUS_LDOUBLE, MUS_UBSHORT, MUS_ULSHORT,
				       MUS_L24INT, MUS_BINTN, MUS_LINTN};

#define NUM_DATA_FORMATS 19
static char *data_formats[NUM_DATA_FORMATS] = {
  "no sound",
  "16 bit big-endian int",
  "8 bit mulaw",
  "8 bit signed int",
  "32 bit big-endian float",
  "32 bit big-endian int",
  "8 bit alaw",
  "8 bit unsigned int",
  "24 bit big-endian int",
  "64 bit big-endian double",
  "16 bit little-endian int",
  "32 bit little-endian int",
  "32 bit little-endian float",
  "64 bit little-endian double",
  "16 bit big-endian unsigned int",
  "16 bit little-endian unsigned int",
  "24 bit little-endian int",
  "32 bit big-endian normalized int",
  "32 bit little-endian normalized int"};

int num_data_formats(void) {return(NUM_DATA_FORMATS);}
char *data_format_name(int i)
{
  if ((i >= 0) && 
      (i < NUM_DATA_FORMATS)) 
    return(data_formats[i]); 
  else return("unknown format");
}

/* must parallel sndlib.h definitions */

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

static int check_for_same_name(snd_info *sp1, void *ur_info)
{
  int i;
  chan_info *cp;
  same_name_info *info = (same_name_info *)ur_info;
  if ((sp1) && (strcmp(sp1->filename, info->filename) == 0))
    {
      info->sp = sp1;
      for (i = 0; i < sp1->nchans; i++) 
	{
	  cp = sp1->chans[i];
	  if (info->edits < cp->edit_ctr) 
	    info->edits = cp->edit_ctr;
	}
      return(1); /* stop immediately and deal with this one */
    }
  return(0);
}

int check_for_filename_collisions_and_save(snd_state *ss, snd_info *sp, char *str, int save_type, int srate, int type, int format, char *comment)
{
  same_name_info *collision = NULL;
  char *fullname, *ofile;
  int err, result = 0;
  if (sp) clear_minibuffer(sp);
  alert_new_file();
  /* now check in-core files -- need to close any of same name -- if edited what to do? */
  /* also it's possible the new file name is the same as the current file name(!) */
  fullname = mus_expand_filename(str);
  if (!(snd_overwrite_ok(ss, fullname))) {FREE(fullname); return(-1);}
  if (strcmp(fullname, sp->filename) == 0)
    {
      /* normally save-as saves the current edit tree, merely saving the current state
       * in a separate, presumably inactive file; here we're being asked to overwrite
       * the current file in save-as; we can't have it both ways -- we'll save the edits 
       * in a temp file, then rename/copy the temp, and call update 
       */
      if (sp->read_only)
	{
	  report_in_minibuffer_and_save(sp, "can't save-as %s (%s is write-protected)", fullname, sp->short_filename);
	  FREE(fullname);
	  return(-1);
	}
      /* it's possible also that the same-named file is open in several windows -- for now we'll ignore that */
      /* also what if a sound is write-protected in one window, and not in another? */
      ofile = snd_tempnam(ss); 
      if (save_type == FILE_SAVE_AS)
	result = save_edits_without_display(sp, ofile, type, format, srate, comment, TO_SCM_INT(AT_CURRENT_EDIT_POSITION), "file save as", 0);
      else result = save_selection(ss, ofile, type, format, srate, comment, SAVE_ALL_CHANS);
      if (result != MUS_NO_ERROR)
	report_in_minibuffer(sp, "save as temp: %s: %s", ofile, strerror(errno));
      else err = move_file(ofile, sp->filename);
      snd_update(ss, sp);
      FREE(ofile);
      FREE(fullname);
    }
  else
    {
      collision = (same_name_info *)CALLOC(1, sizeof(same_name_info));
      collision->filename = fullname;
      collision->edits = 0;
      collision->sp = NULL;
      map_over_sounds(ss, check_for_same_name, (void *)collision);
      if (collision->sp)
	{
	  /* if no edits, we'll just close, overwrite, reopen */
	  /* if edits, we need to ask luser what to do */
	  /* we don't need to check for overwrites at this point */
	  if (collision->edits > 0)
	    {
	      if (!(snd_yes_or_no_p(ss, "%s has unsaved edits.\nClobber them and overwrite %s?", str, str)))
		{
		  FREE(fullname); 
		  FREE(collision); 
		  return(-1);
		}
	    }
	  snd_close_file(collision->sp, ss);
	}
      if (save_type == FILE_SAVE_AS)
	result = save_edits_without_display(sp, str, type, format, srate, comment, TO_SCM_INT(AT_CURRENT_EDIT_POSITION), "file save as", 0);
      else result = save_selection(ss, str, type, format, srate, comment, SAVE_ALL_CHANS);
      if (result != MUS_NO_ERROR)
	report_in_minibuffer_and_save(sp, "%s: %s", str, strerror(errno));
      else report_in_minibuffer(sp, "%s saved as %s",
				(save_type == FILE_SAVE_AS) ? sp->short_filename : "selection",
				str);
      if (collision->sp) snd_open_file(fullname, ss, FALSE);
      FREE(fullname);
      FREE(collision);
    }
  return(result);
}


#define RIPPLE_SIZE 65536
/* needs to be big enough to accommodate any newly added header or header comments */

void edit_header_callback(snd_state *ss, snd_info *sp, file_data *edit_header_data)
{
  unsigned char *ripple0, *ripple1, *zerobuf;
  int fd, err, chans, srate, loc, comlen, type, format, bytes0, bytes1, curloc, readloc, writeloc, curbytes, totalbytes;
  char *comment;
  file_info *hdr;
#if HAVE_ACCESS
  err = access(sp->filename, W_OK);
#else
  err = 0;
#endif
  if (err == 0)
    {
      fd = open(sp->filename, O_RDWR, 0);
      if (fd != -1)
	{
	  hdr = sp->hdr;
	  ripple0 = (unsigned char *)CALLOC(RIPPLE_SIZE, sizeof(unsigned char));
	  ripple1 = (unsigned char *)CALLOC(RIPPLE_SIZE, sizeof(unsigned char));
	  lseek(fd, hdr->data_location, SEEK_SET);
	  bytes0 = read(fd, ripple0, RIPPLE_SIZE);
	  if (bytes0 == RIPPLE_SIZE) 
	    bytes1 = read(fd, ripple1, RIPPLE_SIZE); 
	  else bytes1 = -1;
	  srate = hdr->srate;
	  chans = hdr->chans;
	  type = hdr->type;
	  if ((type == MUS_AIFF) || 
	      (type == MUS_AIFC))
	    mus_header_set_aiff_loop_info(mus_sound_loop_info(sp->filename));
	  mus_sound_forget(sp->filename);
	  format = hdr->format;
	  loc = hdr->data_location;
	  comment = read_file_data_choices(edit_header_data, &srate, &chans, &type, &format, &loc);
	  comlen = snd_strlen(comment);
	  curloc = 0;
	  curbytes = 0;
	  totalbytes = hdr->samples * mus_data_format_to_bytes_per_sample(hdr->format);
	  lseek(fd, 0, SEEK_SET);
	  if (type != MUS_RAW)
	    {
	      mus_header_write_with_fd(fd, type, srate, chans, loc, hdr->samples, format, comment, comlen);
	      curloc = mus_header_data_location();
	      if ((loc != curloc) && 
		  (loc != hdr->data_location)) /* user changed it */
		{
		  /* pad if possible ? */
		  if (loc > curloc)
		    {
		      zerobuf = (unsigned char *)CALLOC(loc-curloc, sizeof(unsigned char));
		      write(fd, zerobuf, loc-curloc);
		      FREE(zerobuf);
		      curloc = loc;
		    }
		}
	    }
	  readloc = RIPPLE_SIZE * 2;
	  writeloc = curloc;
	  if (writeloc > readloc) 
	    snd_error("%s[%d] %s: writeloc > readloc!",
		      __FILE__, __LINE__, __FUNCTION__);
	  while (bytes0 > 0)
	    {
	      write(fd, ripple0, bytes0);
	      curbytes += bytes0;
	      writeloc += RIPPLE_SIZE;
	      if (bytes1 > 0)
		{
		  lseek(fd, readloc, SEEK_SET);
		  readloc += RIPPLE_SIZE;
		  bytes0 = read(fd, ripple0, RIPPLE_SIZE);
		  lseek(fd, writeloc, SEEK_SET);
		  writeloc += RIPPLE_SIZE;
		  write(fd, ripple1, bytes1);
		  curbytes += bytes1;
		  if (bytes0 > 0)
		    {
		      lseek(fd, readloc, SEEK_SET);
		      readloc += RIPPLE_SIZE;
		      bytes1 = read(fd, ripple1, RIPPLE_SIZE);
		    }
		}
	      if (curbytes > totalbytes) break; /* ?? this should not happen */
	    }
	  if (close(fd) != 0)
	    snd_error("can't close %d (%s): %s [%s[%d] %s]",
		      fd, sp->filename, strerror(errno),
		      __FILE__, __LINE__, __FUNCTION__);
	  clear_minibuffer(sp);
	  snd_file_bomb_icon(sp, TRUE);
	  FREE(ripple0);
	  FREE(ripple1);
	  FREE(comment);
	  if (auto_update(ss)) 
	    map_over_sounds(ss, snd_not_current, NULL);
	}
      else 
	snd_error("can't open %s: %s", sp->short_filename, strerror(errno));
      mus_header_set_aiff_loop_info(NULL);
    }
  else 
    snd_error("can't write %s: %s", sp->short_filename, strerror(errno));
}


/* raw data dialog funcs */

static int swap_int (int n)
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

static short swap_short (short n)
{
  short o;
  unsigned char *inp, *outp; 
  inp = (unsigned char *)&n; 
  outp = (unsigned char *)&o;
  outp[0] = inp[1]; 
  outp[1] = inp[0]; 
  return(o);
}

char *raw_data_explanation(char *filename, snd_state *ss, file_info *hdr)
{
  char *reason_str, *tmp_str, *file_string;
  int ns, better_srate = 0, better_chans = 0;
  reason_str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  tmp_str = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
  /* try to provide some notion of what might be the intended header (currently limited to byte-order mistakes) */
  mus_snprintf(reason_str, PRINT_BUFFER_SIZE, "srate: %d", hdr->srate);
  ns = (int)swap_int(hdr->srate);
  if ((ns < 4000) || (ns > 100000)) 
    ns = (int)swap_short((short)(hdr->srate));
  if ((ns > 4000) && (ns < 100000))
    {
      better_srate = ns;
      mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, " (swapped: %d)", ns);
      strcat(reason_str, tmp_str);
    }
  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\nchans: %d", hdr->chans);
  strcat(reason_str, tmp_str);
  ns = swap_int(hdr->chans);
  if ((ns < 0) || (ns > 8)) 
    ns = swap_short((short)(hdr->chans));
  if ((ns > 0) && (ns <= 8))
    {
      better_chans = ns;
      mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, " (swapped: %d)", ns);
      strcat(reason_str, tmp_str);
    }
  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\nlength: %.3f (%d samples, %d bytes total)",
	  (float)(hdr->samples) / (float)(hdr->chans * hdr->srate),
	  hdr->samples,
	  mus_sound_length(filename));
  strcat(reason_str, tmp_str);
  ns = swap_int(hdr->samples);
  if (ns < mus_sound_length(filename))
    {
      mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\n  (swapped: %d", ns);
      strcat(reason_str, tmp_str);
      if ((better_chans) && (better_srate))
	{
	  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE,
		  ", swapped length: %.3f / sample-size-in-bytes)",
		  (float)ns / (float)(better_chans * better_srate));
	  strcat(reason_str, tmp_str);
	}
      else strcat(reason_str, ")");
    }
  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\ndata location: %d", hdr->data_location);
  strcat(reason_str, tmp_str);
  ns = swap_int(hdr->data_location);
  if ((ns > 0) && (ns <= 1024)) 
    {
      mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, " (swapped: %d)", ns);
      strcat(reason_str, tmp_str);
    }
  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\ntype: %s", mus_header_type_name(hdr->type));
  strcat(reason_str, tmp_str);
  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\nformat: %s\n", mus_data_format_name(hdr->format));
  strcat(reason_str, tmp_str);
  hdr->type = MUS_RAW;
  snd_help(ss, "Current header values", reason_str);
  file_string = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  mus_snprintf(file_string, PRINT_BUFFER_SIZE,
	  "Bogus header found for %s", 
	  filename_without_home_directory(filename));
  FREE(tmp_str);
  FREE(reason_str);
  return(file_string);
}


static SCM g_add_sound_file_extension(SCM ext)
{
  #define H_add_sound_file_extension "(" S_add_sound_file_extension " ext)  adds the file extension ext to the list of sound file extensions"
  ASSERT_TYPE(STRING_P(ext), ext, ARGn, S_add_sound_file_extension, "a string");
  add_sound_file_extension(TO_C_STRING(ext));
  return(ext);
}

static SCM g_file_write_date(SCM file)
{
  #define S_file_write_date "file-write-date"
  #define H_file_write_date "(" S_file_write_date " file) -> write date in the same format as current-time:\n\
(strftime \"%a %d-%b-%Y %H:%M %Z\" (localtime (file-write-date \"oboe.snd\")))\n\
Equivalent to Guile (stat:mtime (stat file))"

  time_t date;
  ASSERT_TYPE(STRING_P(file), file, ARGn, S_file_write_date, "a string");
  date = file_write_date(TO_C_STRING(file));
  return(scm_return_first(TO_SCM_INT(date), file));
}

static SCM g_sound_loop_info(SCM snd)
{
  int *res;
  SCM sres = EMPTY_LIST;
  snd_info *sp;
  ASSERT_SOUND(S_sound_loop_info, snd, 1);
  sp = get_sp(snd);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_sound_loop_info, snd));
  res = mus_sound_loop_info(sp->filename);
  if (res)
    {
      sres = LIST_6(TO_SCM_INT(res[0]), TO_SCM_INT(res[1]), TO_SCM_INT(res[2]),
		       TO_SCM_INT(res[3]), TO_SCM_INT(res[4]), TO_SCM_INT(res[5]));
      FREE(res);
    }
  return(sres);
}

static SCM g_set_sound_loop_info(SCM snd, SCM vals)
{
  #define H_sound_loop_info "(" "set-" S_sound_loop_info " snd vals) sets loop points"
  snd_info *sp;
  char *tmp_file;
  file_info *hdr;
  int type, len = 0;
  SCM start0 = UNDEFINED_VALUE, end0 = UNDEFINED_VALUE, start1 = UNDEFINED_VALUE, end1 = UNDEFINED_VALUE;
  ASSERT_SOUND("set-" S_sound_loop_info, snd, 1);
  ASSERT_TYPE(NOT_BOUND_P(vals) || LIST_P_WITH_LENGTH(vals, len), vals, ARG2, "set-" S_sound_loop_info, "a list");
  if (NOT_BOUND_P(vals))
    {
      vals = snd;
      sp = get_sp(UNDEFINED_VALUE);
    }
  else sp = get_sp(snd);
  if (sp == NULL) 
    return(snd_no_such_sound_error("set-" S_sound_loop_info, snd));
  hdr = sp->hdr;
  if (len > 0) start0 = CAR(vals);
  if (len > 1) end0 = CADR(vals);
  if (len > 2) start1 = CADDR(vals);
  if (len > 3) end1 = CADDDR(vals);
  if (hdr->loops == NULL)
    hdr->loops = (int *)CALLOC(6, sizeof(int));
  hdr->loops[0] = TO_C_INT_OR_ELSE(start0, 0);
  hdr->loops[1] = TO_C_INT_OR_ELSE(end0, 0);
  hdr->loops[2] = TO_C_INT_OR_ELSE(start1, 0);
  hdr->loops[3] = TO_C_INT_OR_ELSE(end1, 0);
  mus_sound_set_loop_info(sp->filename, hdr->loops);
  type = hdr->type;
  if ((type != MUS_AIFF) && 
      (type != MUS_AIFC))
    {
      snd_warning("changing %s header from %s to aifc to accommodate loop info",
		  sp->short_filename,
		  mus_header_type_name(type));
      type = MUS_AIFC;
    }
  tmp_file = snd_tempnam(sp->state);
  save_edits_without_display(sp, tmp_file, type, 
			     hdr->format, 
			     hdr->srate, 
			     hdr->comment,
			     TO_SCM_INT(AT_CURRENT_EDIT_POSITION),
			     S_sound_loop_info, 0);
  move_file(tmp_file, sp->filename);
  FREE(tmp_file);
  snd_update(sp->state, sp);
  return(TRUE_VALUE);
}

static SCM g_soundfont_info(SCM snd)
{
  /* return all soundfont descriptors as list of lists: ((name start loopstart loopend)) */
  #define H_soundfont_info "(" S_soundfont_info " &optional snd) -> list of lists describing snd as a soundfont. \
each inner list has the form: (name start loopstart loopend)"

  SCM inlist = EMPTY_LIST, outlist = EMPTY_LIST;
  int i, lim;
  snd_info *sp;
  ASSERT_SOUND(S_soundfont_info, snd, 1);
  sp = get_sp(snd);
  if (sp == NULL) 
    return(snd_no_such_sound_error(S_soundfont_info, snd));
  mus_header_read(sp->filename);
  if (mus_header_type() == MUS_SOUNDFONT)
    {
      lim = mus_header_sf2_entries();
      if (lim > 0)
	for (i = lim-1; i >= 0; i--)
	  {
	    inlist = LIST_4(TO_SCM_STRING(mus_header_sf2_name(i)),
			       TO_SCM_INT(mus_header_sf2_start(i)),
			       TO_SCM_INT(mus_header_sf2_loop_start(i)),
			       TO_SCM_INT(mus_header_sf2_end(i)));
	    outlist = CONS(inlist, outlist);
	  }
    }
  return(outlist);
}

static SCM g_preload_directory(SCM directory) 
{
  #define H_preload_directory "(" S_preload_directory " dir) preloads (into the View:Files dialog) any sounds in dir"
  ASSERT_TYPE(STRING_P(directory), directory, ARGn, S_preload_directory, "a string");
  add_directory_to_prevlist(get_global_state(), 
			    TO_C_STRING(directory));
  return(directory);
}

static SCM g_preload_file(SCM file) 
{
  #define H_preload_file "(" S_preload_file " file) preloads file (into the View:Files dialog)"
  char *name = NULL;
  ASSERT_TYPE(STRING_P(file), file, ARGn, S_preload_file, "a string");
  name = mus_expand_filename(TO_C_STRING(file));
  remember_me(get_global_state(), 
	      filename_without_home_directory(name), 
	      name);
  if (name) FREE(name);
  return(file);
}

static SCM g_sound_files_in_directory(SCM dirname)
{
  #define H_sound_files_in_directory "(" S_sound_files_in_directory " directory) returns a list of sound files in directory"
  dir *dp = NULL;
  char *name = NULL;
  int i;
  SCM res = EMPTY_LIST;
  ASSERT_TYPE(STRING_P(dirname), dirname, ARGn, S_sound_files_in_directory, "a string");
  name = TO_C_STRING(dirname);
  if (name)
    {
      dp = find_sound_files_in_dir(name);
      if (dp)
	{
	  for (i = dp->len - 1; i >= 0; i--)
	    res = CONS(TO_SCM_STRING(dp->files[i]), res);
	  free_dir(dp);
	}
    }
  return(res);
}

void g_init_file(SCM local_doc)
{
  DEFINE_PROC(S_add_sound_file_extension,    g_add_sound_file_extension, 1, 0, 0,     H_add_sound_file_extension);
  DEFINE_PROC(S_file_write_date,             g_file_write_date, 1, 0, 0,              H_file_write_date);
  DEFINE_PROC(S_soundfont_info,              g_soundfont_info, 0, 1, 0,               H_soundfont_info);
  DEFINE_PROC(S_preload_directory,           g_preload_directory, 1, 0, 0,            H_preload_directory);
  DEFINE_PROC(S_preload_file,                g_preload_file, 1, 0, 0,                 H_preload_file);
  DEFINE_PROC(S_sound_files_in_directory,    g_sound_files_in_directory, 1, 0, 0,     H_sound_files_in_directory);

  define_procedure_with_setter(S_sound_loop_info, PROCEDURE g_sound_loop_info, H_sound_loop_info,
			       "set-" S_sound_loop_info, PROCEDURE g_set_sound_loop_info, local_doc, 0, 1, 1, 1);
#if HAVE_GUILE
#if HAVE_SCM_C_DEFINE
  memo_sound = scm_permanent_object(scm_c_define(S_memo_sound, FALSE_VALUE));
#else
  memo_sound = gh_define(S_memo_sound, FALSE_VALUE);
#endif
#endif

  #define H_open_hook S_open_hook " (filename) is called each time a file is opened (before the actual open). \
If it returns #t, the file is not opened."

  #define H_close_hook S_close_hook " (snd) is called each time a file is closed (before the close). \
If it returns #t, the file is not closed."

  #define H_just_sounds_hook S_just_sounds_hook " (filename) is called on each file (after the sound file extension check) if the \
just-sounds button is set. Return #f to filter out filename. "

  open_hook =        MAKE_HOOK(S_open_hook, 1, H_open_hook);                 /* arg = filename */
  close_hook =       MAKE_HOOK(S_close_hook, 1, H_close_hook);               /* arg = sound index */
  just_sounds_hook = MAKE_HOOK(S_just_sounds_hook, 1, H_just_sounds_hook);   /* arg = filename */

#define H_open_raw_sound_hook S_open_raw_sound_hook " (filename current-choices) is called when a headerless sound file is opened. \
Its result can be a list describing the raw file's attributes (thereby bypassing the Raw File Dialog and so on). \
The list (passed to subsequent hook functions as 'current-choice') is interpreted as \
(list chans srate data-format data-location data-length) where trailing elements can \
be omitted (location defaults to 0, and length defaults to the file length in bytes)."

  open_raw_sound_hook = MAKE_HOOK(S_open_raw_sound_hook, 2, H_open_raw_sound_hook);    /* args = filename current-result */

  define_procedure_with_setter(S_previous_files_sort_procedure, PROCEDURE g_previous_files_sort_procedure, H_previous_files_sort_procedure,
			       "set-" S_previous_files_sort_procedure, PROCEDURE g_set_previous_files_sort_procedure, local_doc, 0, 0, 1, 0);
}
