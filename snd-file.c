#include "snd.h"
#include "snd-file.h"
#include "sndlib-strings.h"

#if HAVE_DIRENT_H
  #include <dirent.h>
#endif



/* -------------------------------- basic file attributes -------------------------------- */

#if USE_STATVFS
  #include <sys/statvfs.h>
#endif

#if __bsdi__ || HAVE_SYS_PARAM_H
  #include <sys/param.h>
#endif

#if (HAVE_SYS_MOUNT_H && HAVE_OSX) || __bsdi__ || HAVE_NETBSD
  #include <sys/mount.h>
#endif


#if (!USE_STATVFS)

#if USE_STATFS
mus_long_t disk_kspace(const char *filename)
{
  struct statfs buf;
  int err;
  err = statfs(filename, &buf);
  if (err == 0)
    {
      if (buf.f_bsize == 1024) 
	return(buf.f_bavail);
      return((mus_long_t)(buf.f_bsize * ((double)(buf.f_bavail) / 1024.0)));
    }
  return(err);
}
#else
mus_long_t disk_kspace(const char *filename) {return(1234567);}
#endif

#else

mus_long_t disk_kspace(const char *filename)
{
#if HAVE_SUN
  statvfs_t buf; /* else dumb compiler complaint */
#else
  struct statvfs buf;
#endif
  mus_long_t err = -1;
  err = statvfs(filename, &buf);
  if (err == 0)
    {
      if (buf.f_frsize == 1024) 
	return(buf.f_bfree);
      return((mus_long_t)(buf.f_frsize * ((double)(buf.f_bfree) / 1024.0)));
    }
  return(err);
}
#endif


bool link_p(const char *filename)
{
#if __MINGW32__ 
  return(false);
#else 
  struct stat statbuf;
#if HAVE_LSTAT
  return((lstat(filename, &statbuf) >= 0) &&
	 (S_ISLNK(statbuf.st_mode)));
#else
  return((stat(filename, &statbuf) == 0) && 
	 (S_ISLNK(statbuf.st_mode)));
#endif
#endif
}


bool directory_p(const char *filename)
{
  struct stat statbuf;
#if HAVE_LSTAT
  return((lstat(filename, &statbuf) >= 0) &&
	 (S_ISDIR(statbuf.st_mode)));
#else
  return((stat(filename, &statbuf) == 0) && 
	 (S_ISDIR(statbuf.st_mode)));
#endif
}


static bool empty_file_p(const char *filename)
{
#if HAVE_LSTAT
  struct stat statbuf;
  if (lstat(filename, &statbuf) >= 0) 
    return(statbuf.st_size == (mus_long_t)0);
#endif
  return(false);
}


static mus_long_t file_bytes(const char *filename)
{
#if HAVE_LSTAT
  struct stat statbuf;
  if (lstat(filename, &statbuf) >= 0) 
    return(statbuf.st_size);
  return(0);
#else
  int chan;
  mus_long_t bytes;
  chan = mus_file_open_read(filename);
  if (chan == -1) return(0);
  bytes = lseek(chan, 0L, SEEK_END);
  snd_close(chan, filename);
  return(bytes);
#endif
}


time_t file_write_date(const char *filename)
{
  struct stat statbuf;
  int err;
  err = stat(filename, &statbuf);
  if (err < 0) return((time_t)err);
  return((time_t)(statbuf.st_mtime));
}


bool directory_exists(char *name)
{
  char temp;
  bool result;
  int i, len, last_slash = -1;
  len = strlen(name);
  for (i = 0; i < len; i++) 
    if (name[i] == '/') 
      last_slash = i;
  if (last_slash <= 0)
    return(true);
  if (last_slash >= len - 1) /* can't be > */
    return(directory_p(name));
  temp = name[last_slash + 1];
  name[last_slash + 1] = '\0';
  result = directory_p(name);
  name[last_slash + 1] = temp;
  return(result);
}


const char *short_data_format_name(int sndlib_format, const char *filename)
{
  if (mus_data_format_p(sndlib_format))
    return(mus_data_format_short_name(sndlib_format));
  else return(mus_header_original_format_name(mus_sound_original_format(filename),
					      mus_sound_header_type(filename)));
}


static void forget_filename(const char *filename, char **names)
{
  int i, j = 0;
  for (i = 0; i < FILENAME_LIST_SIZE; i++)
    if ((names[i]) &&
	(strcmp(names[i], filename) == 0))
      {
	free(names[i]);
	names[i] = NULL;
      }
  for (i = 0; i < FILENAME_LIST_SIZE; i++)
    if (names[i])
      {
	if (i != j) 
	  {
	    names[j] = names[i];
	    names[i] = NULL;
	  }
	j++;
      }
}


void remember_filename(const char *filename, char **names)
{
  int i;
  forget_filename(filename, names); /* clear out old copy, if any, then compact the list */
  if (names[FILENAME_LIST_SIZE - 1]) free(names[FILENAME_LIST_SIZE - 1]);
  for (i = FILENAME_LIST_SIZE - 1; i > 0; i--) 
    names[i] = names[i - 1];
  names[0] = mus_strdup(filename);
}


char **make_filename_list(void)
{
  return((char **)calloc(FILENAME_LIST_SIZE, sizeof(char *)));
}


static char *preloaded_files[FILENAME_LIST_SIZE];

void preload_filenames(char **files)
{
  int i;
  for (i = 0; (i < FILENAME_LIST_SIZE) && (preloaded_files[i]); i++)
    files[i] = mus_strdup(preloaded_files[i]);
}


int recent_files_size(void)
{
  /* return how many files in the preloaded list are not currently open (for Open Recent menu item) */
  int i, size = 0;
  for (i = 0; (i < FILENAME_LIST_SIZE) && (preloaded_files[i]); i++)
    if ((!(find_sound(preloaded_files[i], 0))) &&
	(mus_file_probe(preloaded_files[i])))
      size++;
  return(size);
}


char **recent_files(void)
{
  int i, j = 0;
  char **new_list;
  new_list = make_filename_list();
  for (i = 0; (i < FILENAME_LIST_SIZE) && (preloaded_files[i]); i++)
    if ((!(find_sound(preloaded_files[i], 0))) &&
	(mus_file_probe(preloaded_files[i])))
      new_list[j++] = mus_strdup(preloaded_files[i]);
  return(new_list);
}



/* -------------------------------- file list positioning -------------------------------- */

static dirpos_info *make_dirpos_info(const char *dir, position_t pos)
{
  dirpos_info *dp;
  dp = (dirpos_info *)calloc(1, sizeof(dirpos_info));
  dp->directory_name = mus_strdup(dir);
  dp->list_top = pos;
  return(dp);
}


dirpos_list *make_dirpos_list(void)
{
  dirpos_list *dl;
  dl = (dirpos_list *)calloc(1, sizeof(dirpos_list));
  dl->size = 8;
  dl->top = 0;
  dl->dirs = (dirpos_info **)calloc(dl->size, sizeof(dirpos_info *));
  return(dl);
}


void dirpos_update(dirpos_list *dl, const char *dir, position_t pos)
{
  int i;
  if (!dl) return;
  for (i = 0; i < dl->top; i++)
    {
      if ((dl->dirs[i]) && 
	  (strcmp(dir, dl->dirs[i]->directory_name) == 0))
	{
	  dirpos_info *dp;
	  dp = dl->dirs[i];
	  dp->list_top = pos;
	  return;
	}
    }
  if (dl->top >= dl->size)
    {
      int old_size;
      old_size = dl->size;
      dl->size += 8;
      dl->dirs = (dirpos_info **)realloc(dl->dirs, dl->size * sizeof(dirpos_info *));
      for (i = old_size; i < dl->size; i++) dl->dirs[i] = NULL;
    }
  dl->dirs[dl->top++] = make_dirpos_info(dir, pos);
}


position_t dirpos_list_top(dirpos_list *dl, const char *dirname)
{
  int i;
  if (dl)
    for (i = 0; i < dl->top; i++)
      if ((dl->dirs[i]) && 
	  (strcmp(dirname, dl->dirs[i]->directory_name) == 0))
	return(dl->dirs[i]->list_top);
  return(POSITION_UNKNOWN);
}


/* -------------------------------- sorters -------------------------------- */

static sort_info *free_sort_info(sort_info *ptr)
{
  if (ptr)
    {
      if (ptr->filename) free(ptr->filename);
      if (ptr->full_filename) free(ptr->full_filename);
      free(ptr);
    }
  return(NULL);
}


static sort_info *make_sort_info(const char *filename, const char *full_filename)
{
  sort_info *ptr;
  ptr = (sort_info *)calloc(1, sizeof(sort_info));
  ptr->filename = mus_strdup(filename); /* not mus_strdup -> these are glomming up memlog */
  ptr->full_filename = mus_strdup(full_filename);
  return(ptr);
}


/* sort files list by name (aphabetical), or some number (date written, size), or by xen proc */

static int sort_a_to_z(const void *a, const void *b)
{
  sort_info *d1 = *(sort_info **)a;
  sort_info *d2 = *(sort_info **)b;
  return(strcmp(d1->filename, d2->filename));
}


static int sort_z_to_a(const void *a, const void *b)
{
  return(-sort_a_to_z(a, b));
}


static int sort_small_to_big(const void *a, const void *b)
{
  sort_info *d1 = *(sort_info **)a;
  sort_info *d2 = *(sort_info **)b;
  if (d1->samps > d2->samps) 
    return(1); 
  else 
    {
      if (d1->samps == d2->samps) 
	return(0); 
      else return(-1);
    }
}


static int sort_big_to_small(const void *a, const void *b)
{
  return(-sort_small_to_big(a, b));
}


static int sort_new_to_old(const void *a, const void *b)
{
  sort_info *d1 = *(sort_info **)a;
  sort_info *d2 = *(sort_info **)b;
  if (d1->time < d2->time) 
    return(1); 
  else 
    {
      if (d1->time == d2->time) 
	return(0); 
      else return(-1);
    }
}


static int sort_old_to_new(const void *a, const void *b)
{
  return(-sort_new_to_old(a, b));
}


static XEN sorter_func;

static int sort_xen(const void *a, const void *b)
{
  /* sorter function gets two names, returns -1, 0, or 1 just like the other comparators */
  sort_info *d1 = *(sort_info **)a;
  sort_info *d2 = *(sort_info **)b;
  return(XEN_TO_C_INT(XEN_CALL_2(sorter_func, C_TO_XEN_STRING(d1->full_filename), C_TO_XEN_STRING(d2->full_filename), "sort func")));
}


void snd_sort(int sorter, sort_info **data, int len)
{
  int i, sorter_pos;
  switch (sorter)
    {
    case SORT_A_TO_Z: 
      qsort((void *)data, len, sizeof(sort_info *), sort_a_to_z);
      break;

    case SORT_Z_TO_A: 
      qsort((void *)data, len, sizeof(sort_info *), sort_z_to_a);
      break;

    case SORT_NEW_TO_OLD:
      for (i = 0; i < len; i++) 
	data[i]->time = file_write_date(data[i]->full_filename);
      qsort((void *)data, len, sizeof(sort_info *), sort_new_to_old);
      break;

    case SORT_OLD_TO_NEW:
      for (i = 0; i < len; i++) 
	data[i]->time = file_write_date(data[i]->full_filename);
      qsort((void *)data, len, sizeof(sort_info *), sort_old_to_new);
      break;

    case SORT_SMALL_TO_BIG:
      for (i = 0; i < len; i++)
	data[i]->samps = file_bytes(data[i]->full_filename);
      qsort((void *)data, len, sizeof(sort_info *), sort_small_to_big);
      break;

    case SORT_BIG_TO_SMALL:
      for (i = 0; i < len; i++)
	data[i]->samps = file_bytes(data[i]->full_filename);
      qsort((void *)data, len, sizeof(sort_info *), sort_big_to_small);
      break;

    default:
    case SORT_XEN:
      /* sorter is SORT_XEN + index into file_sorters list */
      /*   that list is a vector of pairs (name proc) */
      sorter_pos = sorter - SORT_XEN;
      if ((sorter_pos >= 0) &&
	  (sorter_pos < ss->file_sorters_size))
	{
	  if (XEN_LIST_P(XEN_VECTOR_REF(ss->file_sorters, sorter_pos)))
	    {
	      sorter_func = XEN_CADR(XEN_VECTOR_REF(ss->file_sorters, sorter_pos));
	      qsort((void *)data, len, sizeof(sort_info *), sort_xen);
	      return;
	    }
	}
      snd_warning("no such file-sorter (%d)", sorter_pos);
      break;
    }
}


/* -------------------------------- directory readers -------------------------------- */

static dir_info *make_dir_info(const char *name)
{
  dir_info *dp;
  dp = (dir_info *)calloc(1, sizeof(dir_info));
  dp->files = (sort_info **)calloc(32, sizeof(sort_info *));
  dp->dir_name = mus_strdup(name);
  dp->len = 0;
  dp->size = 32;
  return(dp);
}


dir_info *free_dir_info(dir_info *dp)
{
  if (dp->dir_name) free(dp->dir_name);
  if (dp->files)
    {
      int i;
      for (i = 0; i < dp->len; i++) 
	if (dp->files[i]) 
	  dp->files[i] = free_sort_info(dp->files[i]);
      free(dp->files);
    }
  free(dp);
  return(NULL);
}
  

static void add_filename_to_dir_info(dir_info *dp, const char *name, const char *fullname)
{
  dp->files[dp->len] = make_sort_info(name, fullname);
  dp->len++;
  if (dp->len == dp->size) 
    {
      int i;
      dp->size += 32;
      dp->files = (sort_info **)realloc(dp->files, dp->size * sizeof(sort_info *));
      for (i = dp->size - 32; i < dp->size; i++) dp->files[i] = NULL;
    }
}


static void load_dir(DIR *dpos, dir_info *dp, bool (*filter)(const char *filename))
{
  struct dirent *dirp;
  char *fullname;
  int fullname_start = 0, path_max = 0;

#if HAVE_PATHCONF
  path_max = pathconf("/", _PC_PATH_MAX);
#endif
  if (path_max < 1024)
    {
#if defined(PATH_MAX)
      path_max = PATH_MAX;
#endif
      if (path_max < 1024) 
	path_max = 1024;
    }

  fullname = (char *)calloc(path_max, sizeof(char));
  strcpy(fullname, dp->dir_name);
  fullname_start = strlen(dp->dir_name);
  while ((dirp = readdir(dpos)) != NULL)
    if (dirp->d_name[0] != '.')
      {
	strcat(fullname, dirp->d_name);
	if (filter(fullname))
	  add_filename_to_dir_info(dp, dirp->d_name, fullname);
	fullname[fullname_start] = '\0';
      }
  free(fullname);
}


static bool not_directory_p(const char *name)
{
  return(!(directory_p(name)));
}


dir_info *find_files_in_dir(const char *name)
{
#if (!HAVE_OPENDIR)
  return(NULL);
#else
  DIR *dpos;
  dir_info *dp = NULL;
  dpos = opendir(name);
  if (dpos)
    {
      dp = make_dir_info(name);
      load_dir(dpos, dp, not_directory_p);
      if (closedir(dpos) != 0) 
	snd_error("closedir %s failed (%s)!",
		  name, snd_io_strerror());
    }
  return(dp);
#endif
}


#if USE_GTK
dir_info *find_directories_in_dir(const char *name)
{
#if (!HAVE_OPENDIR)
  return(NULL);
#else
  DIR *dpos;
  dir_info *dp = NULL;
  dpos = opendir(name);
  if (dpos)
    {
      dp = make_dir_info(name);
      if (strcmp(name, "/") != 0)
	{
	  char *fullname;
	  int len;
	  len = mus_strlen(name) + mus_strlen(PARENT_DIRECTORY) + 2;  /* PARENT_DIRECTORY = ".." (snd-file.h) */
	  fullname = (char *)calloc(len, sizeof(char));
	  strcpy(fullname, name);
	  strcat(fullname, PARENT_DIRECTORY);
	  add_filename_to_dir_info(dp, PARENT_DIRECTORY, fullname); /* always back pointer */
	  free(fullname);
	}
      load_dir(dpos, dp, directory_p);
      if (closedir(dpos) != 0) 
	snd_error("closedir %s failed (%s)!",
		  name, snd_io_strerror());
    }
  return(dp);
#endif
}
#endif


static XEN filter_func;

static bool filter_xen(const char *name)
{
  return(XEN_TO_C_BOOLEAN(XEN_CALL_1(filter_func, C_TO_XEN_STRING(name), "filter func")));
}


dir_info *find_filtered_files_in_dir(const char *name, int filter_choice)
{
  bool (*filter)(const char *filename);
#if (!HAVE_OPENDIR)
  return(NULL);
#else
  DIR *dpos;
  dir_info *dp = NULL;
  if ((dpos = opendir(name)) != NULL)
    {
      if (filter_choice == JUST_SOUNDS_FILTER)
	filter = sound_file_p;
      else
	{
	  int filter_pos;
	  filter = filter_xen;
	  filter_pos = filter_choice - 2;
	  if ((filter_pos >= 0) &&
	      (filter_pos < ss->file_filters_size))
	    filter_func = XEN_CADR(XEN_VECTOR_REF(ss->file_filters, filter_pos));
	  else
	    {
	      snd_warning("no such file-filter (%d)", filter_pos);
	      return(find_files_in_dir(name));
	    }
	}

      dp = make_dir_info(name);
      load_dir(dpos, dp, filter);

      if (closedir(dpos) != 0) 
	snd_error("closedir %s failed (%s)!",
		  name, snd_io_strerror());
    }
  return(dp);
#endif
}


static dir_info *find_files_from_pattern(dir_info *dp, const char *pattern);

dir_info *find_filtered_files_in_dir_with_pattern(const char *name, int filter_choice, const char *pattern)
{
  dir_info *full_dir, *pattern_dir;
  if (filter_choice != NO_FILE_FILTER)
    full_dir = find_filtered_files_in_dir(name, filter_choice);
  else full_dir = find_files_in_dir(name);
  pattern_dir = find_files_from_pattern(full_dir, pattern);
  full_dir = free_dir_info(full_dir);
  return(pattern_dir);
}


/* -------- sound file extensions list -------- */

static char **sound_file_extensions = NULL;
static int sound_file_extensions_size = 0;
static int sound_file_extensions_end = 0;
static int default_sound_file_extensions = 0;

static void add_sound_file_extension(const char *ext)
{
  int i;
  if ((!ext) || (!(*ext))) return;

  for (i = 0; i < sound_file_extensions_end; i++)
    if (strcmp(ext, sound_file_extensions[i]) == 0)
      return;
  if (sound_file_extensions_end == sound_file_extensions_size)
    {
      sound_file_extensions_size += 8;
      if (sound_file_extensions == NULL)
	sound_file_extensions = (char **)calloc(sound_file_extensions_size, sizeof(char *));
      else sound_file_extensions = (char **)realloc(sound_file_extensions, sound_file_extensions_size * sizeof(char *));
    }
  sound_file_extensions[sound_file_extensions_end] = mus_strdup(ext);
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
  add_sound_file_extension("rf64");
  add_sound_file_extension("caf");

#if HAVE_OGG
  add_sound_file_extension("ogg");
#endif

#if HAVE_SPEEX
  add_sound_file_extension("speex"); /* ?? */
#endif

#if HAVE_FLAC
  add_sound_file_extension("flac");
#endif

#if HAVE_MIDI
  add_sound_file_extension("mid");
#endif

#if HAVE_MPEG
  add_sound_file_extension("mpeg");
#endif

#if HAVE_SHORTEN
  add_sound_file_extension("shn");
#endif

#if HAVE_TTA
  add_sound_file_extension("tta");
#endif

#if HAVE_WAVPACK
  add_sound_file_extension("wv");
#endif

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

#if HAVE_FORTH
	fprintf(fd, "\"%s\" %s drop\n", sound_file_extensions[i], S_add_sound_file_extension);
#endif
      }
}


/* mus_header_read here (or stripped-down equivalent) was very slow, and is just as easy to
 * fool as an extension check (file might start with the word ".snd" or whatever).
 */

bool sound_file_p(const char *name)
{
  int i, dot_loc = -1, len;
  if (!name) return(false);
  len = mus_strlen(name);
  for (i = 0; i < len; i++)
    if (name[i] == '.')
      dot_loc = i;
  /* dot_loc is last dot in the name */
  if ((dot_loc > 0) &&
      (dot_loc < len - 1))
    {
      const char *ext;
      ext = (const char *)(name + dot_loc + 1);
      for (i = 0; i < sound_file_extensions_end; i++)
	if (mus_strcmp(ext, sound_file_extensions[i]))
	  return(true);
    }
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
    local_error_msg = mus_strdup(msg);
  else local_error_msg = NULL;
}


bool plausible_sound_file_p(const char *name)
{
  int err = MUS_NO_ERROR;
  if (empty_file_p(name)) return(false);
  old_error_handler = mus_error_set_handler(local_error2snd);
  err = mus_header_read(name);
  mus_error_set_handler(old_error_handler);
  return((err == MUS_NO_ERROR) &&
	 (mus_header_type() != MUS_RAW));
}


static dir_info *find_sound_files_in_dir(const char *name)
{
  return(find_filtered_files_in_dir(name, JUST_SOUNDS_FILTER));
}


static bool names_match(const char *filename, const char *pattern)
{
  /* just "*" for wildcards here */
  const char *sn, *sp;
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


static dir_info *find_files_from_pattern(dir_info *dp, const char *pattern)
{
  int i;
  dir_info *ndp;
  ndp = make_dir_info(dp->dir_name);
  for (i = 0; i < dp->len; i++)
    if (names_match(dp->files[i]->filename, pattern))
      add_filename_to_dir_info(ndp, dp->files[i]->filename, dp->files[i]->full_filename);
  return(ndp);
}


/* -------------------------------- Snd title (lists open sounds) -------------------------------- */

typedef struct {
  int active_sounds;
  char **names;
  int *sounds;
} active_sound_list;

static void add_sound_to_active_list(snd_info *sp, void *sptr1)
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

  alist = (active_sound_list *)calloc(1, sizeof(active_sound_list));
  alist->sounds = (int *)calloc(ss->max_sounds, sizeof(int));
  alist->names = (char **)calloc(ss->max_sounds, sizeof(char *));
  for_each_sound_with_void(add_sound_to_active_list, (void *)alist);

  len = mus_strlen(ss->startup_title) + 32;
  if (alist->active_sounds > 0)
    {
      if (alist->active_sounds < 4) 
	j = alist->active_sounds; 
      else j = 4;
      for (i = 0; i < j; i++)
	len += mus_strlen(filename_without_directory(alist->names[i]));
    }

  title_buffer = (char *)calloc(len, sizeof(char));
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
	  strcat(title_buffer, filename_without_directory(alist->names[i]));
	  if (i < j - 1)
	    strcat(title_buffer, ", ");
	}
      if (alist->active_sounds > 4) 
	strcat(title_buffer, "...");
    }

  set_title(title_buffer);
  free(title_buffer);
  free(alist->sounds);
  free(alist->names);
  free(alist);
}



/* -------------------------------- open sound file -------------------------------- */

static int fallback_srate = 0, fallback_chans = 0, fallback_format = MUS_UNKNOWN;
static int original_srate = 0, original_chans = 0, original_format = MUS_UNKNOWN;

void set_fallback_srate(int sr) {fallback_srate = sr;}
void set_fallback_chans(int ch) {fallback_chans = ch;}
void set_fallback_format(int fr) {fallback_format = fr;}


static file_info *make_file_info_1(const char *fullname)
{
  file_info *hdr;
  hdr = (file_info *)calloc(1, sizeof(file_info));
  hdr->name = mus_strdup(fullname);
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
  if (!(mus_data_format_p(hdr->format))) hdr->format = fallback_format;
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
  hdr = (file_info *)calloc(1, sizeof(file_info));
  hdr->name = mus_strdup(fullname);
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
	  hdr->loops = (int *)calloc(MUS_LOOP_INFO_SIZE, sizeof(int));
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
  loops = mus_sound_loop_info(filename); /* allocated anew */
  newname = (char *)calloc(len + 5, sizeof(char));
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
	  if (loops) free(loops);
	  free(newname);
	  free(tempname);
	  snd_error("can't write translation temp file! (%s)", snd_open_strerror());
	  return(NULL);
	}
      free(newname);
      newname = mus_strdup(tempname);
      free(tempname);
    }
  snd_close(fd, newname);
  err = snd_translate(filename, newname, type);
  if (err == MUS_NO_ERROR)
    {
      err = mus_header_read(newname);
      if (err == MUS_NO_ERROR)
	{
	  hdr = make_file_info_1(newname);
	  if (hdr->loops == NULL) 
	    hdr->loops = loops;
	  else 
	    if (loops) free(loops);
	  loops = NULL;
	  ss->translated_filename = mus_strdup(newname);
	}
    }
  else snd_remove(newname, REMOVE_FROM_CACHE);
  if (newname) free(newname);
  if (loops) free(loops);
  return(hdr);
}


static XEN open_raw_sound_hook;

static file_info *open_raw_sound(const char *fullname, read_only_t read_only, bool selected)
{
  XEN res = XEN_FALSE;
  int res_loc = NOT_A_GC_LOC;
  int len, srate, chans, data_format;
  mus_long_t data_location, bytes;

  if (ss->reloading_updated_file != 0)
    {
      /* choices already made, so just send back a header that reflects those choices */
      return(make_file_info_1(fullname));
    }

  if (XEN_HOOKED(open_raw_sound_hook))
    {
#if HAVE_SCHEME
      res = s7_call(s7, open_raw_sound_hook, s7_list(s7, 2, C_TO_XEN_STRING(fullname), XEN_FALSE));
#else
      XEN arg1, procs;
      procs = XEN_HOOK_PROCEDURES(open_raw_sound_hook);
      arg1 = C_TO_XEN_STRING(fullname);
      while (XEN_NOT_NULL_P(procs))
	{
	  res = XEN_CALL_2(XEN_CAR(procs), 
			   arg1, 
			   res, 
			   S_open_raw_sound_hook);
	  if (res_loc != NOT_A_GC_LOC) snd_unprotect_at(res_loc);
	  res_loc = snd_protect(res);
	  procs = XEN_CDR(procs);
	}
#endif
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
      if (len > 3) data_location = XEN_TO_C_LONG_LONG(XEN_LIST_REF(res, 3)); else data_location = 0;
      if (len > 4) bytes = XEN_TO_C_LONG_LONG(XEN_LIST_REF(res, 4)); else bytes = mus_sound_length(fullname) - data_location;

      mus_header_set_raw_defaults(srate, chans, data_format);
      mus_sound_override_header(fullname, 
				srate, chans, data_format, 
				MUS_RAW, data_location,
				mus_bytes_to_samples(data_format, bytes));

      if (res_loc != NOT_A_GC_LOC) snd_unprotect_at(res_loc);	      
      hdr = (file_info *)calloc(1, sizeof(file_info));
      hdr->name = mus_strdup(fullname);
      hdr->type = MUS_RAW;
      hdr->srate = mus_sound_srate(fullname);
      hdr->chans = mus_sound_chans(fullname);
      hdr->format = mus_sound_data_format(fullname);
      hdr->samples = mus_sound_samples(fullname); /* total samples, not per channel */
      hdr->data_location = mus_sound_data_location(fullname);
      hdr->comment = NULL;
      return(hdr);
    }
  else 
    {
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

#if (!USE_NO_GUI)
      {
	char *str;
	str = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
	mus_snprintf(str, PRINT_BUFFER_SIZE, "No header found for %s", filename_without_directory(fullname));
	raw_data_dialog_to_file_info(fullname, str, NULL, read_only, selected); /* dialog frees str */
      }
#else
      fprintf(stderr, "No header found for %s", filename_without_directory(fullname));
#endif
    }
  return(NULL);
}

#if (!USE_NO_GUI)
static char *raw_data_explanation(const char *filename, file_info *hdr, char **info);
#endif


static XEN bad_header_hook;

static file_info *tackle_bad_header(const char *fullname, read_only_t read_only, bool selected)
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
      const char *caller;
      if (ss->open_requestor == FROM_OPEN_SOUND)
	caller = S_open_sound;
      else caller = S_view_sound;
      XEN_ERROR(BAD_HEADER,
		XEN_LIST_3(C_TO_XEN_STRING("~A: ~S has a bad header?"),
			   C_TO_XEN_STRING(caller),
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


file_info *make_file_info(const char *fullname, read_only_t read_only, bool selected)
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
	  (encoded_header_p(type)))
	{
	  return(translate_file(fullname, type));
	}
	  
      if (mus_header_type_p(type)) 
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
	      if (mus_data_format_p(format))
		return(make_file_info_1(fullname));
	      return(translate_file(fullname, type));
	    }
	}
      else snd_error("%s does not seem to be a sound file?", fullname); /* no known header */
    }
  else snd_error("can't find file %s: %s", fullname, snd_io_strerror());
  return(hdr);
}


file_info *make_temp_header(const char *fullname, int srate, int chans, mus_long_t samples, const char *caller)
{
  file_info *hdr;
  hdr = (file_info *)calloc(1, sizeof(file_info));
  hdr->name = mus_strdup(fullname);
  hdr->samples = samples;
  hdr->data_location = 28;
  hdr->srate = srate;
  hdr->chans = chans;
  hdr->format = MUS_OUT_FORMAT;
  hdr->type = MUS_NEXT;
  /* want direct read/writes for temp files */
  hdr->comment = mus_strdup(caller);
  return(hdr);
}


file_info *free_file_info(file_info *hdr)
{
  if (hdr)
    {
      if (hdr->name) free(hdr->name);
      if (hdr->comment) free(hdr->comment);
      if (hdr->loops) free(hdr->loops);
      free(hdr);
    }
  return(NULL);
}


static char *opened_sound_file_name(snd_info *sp)
{
  char *newname;
  int len;
  len = strlen(sp->filename);
  newname = (char *)calloc(len + 32, sizeof(char));
  mus_snprintf(newname, len + 32, "%s.%s", sp->filename, XEN_FILE_EXTENSION);
  return(newname);
}


static char *remembered_sound_file_name(snd_info *sp)
{
  char *newname;
  int len;
  len = strlen(sp->filename);
  newname = (char *)calloc(len + 32, sizeof(char));
  mus_snprintf(newname, len + 32, "remembered-%s.%s", sp->short_filename, XEN_FILE_EXTENSION);
  return(newname);
}


static void load_sound_file_extras(snd_info *sp)
{
  char *newname;
  /* possible sound.scm file */
  newname = opened_sound_file_name(sp);
  if (file_write_date(newname) >= sp->write_date)
      snd_load_file(newname);
  free(newname);

  /* remembered-sound.scm -- this is written if remember-sound-state which will also overwrite it */
  if (remember_sound_state(ss))
    {
      newname = remembered_sound_file_name(sp);
      if (file_write_date(newname) >= sp->write_date)
	snd_load_file(newname);
      free(newname);
    }
}


static void remember_sound_file(snd_info *sp)
{
  char *locale = NULL, *newname;
  FILE *fd;

  newname = remembered_sound_file_name(sp);
  fd = FOPEN(newname, "w");

  if (fd == NULL)
    {
      snd_error("remember sound state can't write %s: %s", newname, snd_io_strerror());
      return;
    }
#if HAVE_SETLOCALE
  locale = mus_strdup(setlocale(LC_NUMERIC, "C")); /* must use decimal point in floats since Scheme assumes that format */
#endif

  sp->remembering = true;
  save_sound_state(sp, fd);
  sp->remembering = false;

  if (locale)
    {
#if HAVE_SETLOCALE
      setlocale(LC_NUMERIC, locale);
#endif
      free(locale);
    }
  snd_fclose(fd, newname);
  free(newname);
}


static XEN snd_opened_sound;
static XEN open_hook;
static XEN close_hook;
static XEN before_close_hook;
static XEN during_open_hook;
static XEN after_open_hook;
static XEN output_name_hook;

void during_open(int fd, const char *file, open_reason_t reason)
{
  if (XEN_HOOKED(during_open_hook))
    run_hook(during_open_hook,
	     XEN_LIST_3(C_TO_XEN_INT(fd),
			C_TO_XEN_STRING(file),
			C_TO_XEN_INT((int)reason)),
	     S_during_open_hook);
}


void after_open(snd_info *sp)
{
  /* the sync-style choice used to be handled via after-open-hook in extensions.*, but 15-Feb-11 has
   *   been moved into the main Snd.  So here is the code...
   */
  if (sp->nchans > 1)
    {
      switch (sync_style(ss))
	{
	case SYNC_NONE:
	  sp->sync = 0;
	  break;
	  
	case SYNC_ALL:
	  sp->sync = 1;
	  break;
	  
	case SYNC_BY_SOUND:
	  ss->sound_sync_max++;
	  sp->sync = ss->sound_sync_max; /* if we had used (set! (sync) ...) this would be set (via syncb) to the new max */
	  break;
	}
    }
  else sp->sync = 0;
  syncb(sp, sp->sync);

  if (XEN_HOOKED(after_open_hook))
    run_hook(after_open_hook,
	     XEN_LIST_1(C_INT_TO_XEN_SOUND(sp->index)),
	     S_after_open_hook);

  if (XEN_HOOKED(ss->snd_open_file_hook))
    run_hook(ss->snd_open_file_hook,
	     XEN_LIST_1(C_TO_XEN_INT(FILE_OPENED)),
	     "open-file-hook");

  if (XEN_HOOKED(ss->effects_hook))
    run_hook(ss->effects_hook, XEN_EMPTY_LIST, S_effects_hook);
}


char *output_name(const char *current_name)
{
  if (XEN_HOOKED(output_name_hook))
    {
#if HAVE_SCHEME
      XEN result;
      result = s7_call(s7, output_name_hook, s7_cons(s7, C_TO_XEN_STRING(current_name), XEN_EMPTY_LIST));
      if (XEN_STRING_P(result)) 
	return(mus_strdup(XEN_TO_C_STRING(result)));
#else      
      XEN result, fname, procs;
      procs = XEN_HOOK_PROCEDURES (output_name_hook);
      fname = C_TO_XEN_STRING(current_name);
      while (XEN_NOT_NULL_P(procs))
	{
	  result = XEN_CALL_1(XEN_CAR(procs),
			      fname,
			      S_output_name_hook);
	  if (XEN_STRING_P(result)) 
	    return(mus_strdup(XEN_TO_C_STRING(result)));
	  procs = XEN_CDR (procs);
	}
#endif
    }
  return(mus_strdup(current_name));
}


snd_info *finish_opening_sound(snd_info *sp, bool selected)
{
  if (sp)
    {
#if HAVE_RUBY || HAVE_FORTH
      XEN_VARIABLE_SET(S_snd_opened_sound, C_INT_TO_XEN_SOUND(sp->index));
#endif

#if HAVE_SCHEME
      XEN_VARIABLE_SET(snd_opened_sound, C_INT_TO_XEN_SOUND(sp->index));
#endif

      sp->write_date = file_write_date(sp->filename); /* redundant (see snd-xsnd.c) */
      sp->need_update = false;
      ss->active_sounds++;
      reflect_file_change_in_title();
      monitor_sound(sp);
    }
  for_each_separate_chan(channel_open_pane);

#if USE_MOTIF
  for_each_separate_chan(channel_unlock_pane);
#endif

  if (sp) 
    {
      add_srate_to_completion_list(SND_SRATE(sp));
      if ((selected) &&
	  (sp->active) &&
	  (sp->inuse == SOUND_NORMAL))
	select_channel(sp, 0);
      load_sound_file_extras(sp);

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


/* (hook-push open-hook (lambda (f) (display f) #f)) */

snd_info *snd_open_file(const char *filename, read_only_t read_only)
{
  file_info *hdr;
  snd_info *sp;
  static char *mcf = NULL;

  if (mcf) free(mcf);
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
	  if (mcf) {free(mcf); mcf = NULL;}
	  return(NULL);
	}
      else
	{
	  if (XEN_STRING_P(res))  /* added 14-Aug-01 for user-supplied auto-translations */
	    {
	      if (mcf) free(mcf);
	      mcf = mus_expand_filename(XEN_TO_C_STRING(res));
	    }
	}
    }

  hdr = make_file_info(mcf, read_only, FILE_SELECTED);
  if (!hdr) 
    {
      if (mcf) {free(mcf); mcf = NULL;}
      return(NULL);
    }

  sp = add_sound_window(mcf, read_only, hdr);

  if (mcf) {free(mcf); mcf = NULL;}
  return(finish_opening_sound(sp, FILE_SELECTED));
}


static void view_files_add_file(widget_t dialog, const char *filename);

void snd_close_file(snd_info *sp)
{
  int i;
  XEN res = XEN_FALSE;
  snd_info *chosen_sp = NULL;

  /* before-close-hook can cancel the close, whereas close-hook can't */
  if (XEN_HOOKED(before_close_hook))
    res = run_or_hook(before_close_hook,
		      XEN_LIST_1(C_INT_TO_XEN_SOUND(sp->index)),
		      S_before_close_hook);
  if (XEN_TRUE_P(res)) return;

#if (!USE_NO_GUI)
  if ((ask_about_unsaved_edits(ss)) &&
      (has_unsaved_edits(sp)))
    {
      save_edits_now(sp);
      return;
    }
  unpost_unsaved_edits_if_any(sp);
  unpost_file_has_changed_if_any(sp);
#endif

  if (peak_env_dir(ss))
    map_over_sound_chans(sp, write_peak_env_info_file);

  if (XEN_HOOKED(close_hook))
    run_hook(close_hook,
	     XEN_LIST_1(C_INT_TO_XEN_SOUND(sp->index)),
	     S_close_hook);

  if (remember_sound_state(ss))
    remember_sound_file(sp);

  remember_filename(sp->filename, preloaded_files); /* for open dialog(s) previous files list and File:open recent files menu */

  /* an experiment -- event queue seems to be glomming up when lots of fast open/close,
   * but squelch updates just in case a redisplay event is in the queue.  But check_for_event
   * here is dangerous because a channel might be closed and deallocated already, then
   * this check lets a mouse event through, cp->axis is NULL, cp->active has been stomped on,
   * segfault!  
   */

  for (i = 0; i < sp->nchans; i++) 
    sp->chans[i]->squelch_update = true;
  /* check_for_event(); */

  sp->file_watcher = unmonitor_file(sp->file_watcher);

  /* exit does not go through this function to clean up temps -- see snd_exit_cleanly in snd-main.c */
  if (selection_creation_in_progress()) finish_selection_creation();

  if (ss->deferred_regions > 0)
    for (i = 0; i < sp->nchans; i++)
      if (sp->chans[i]) 
	sequester_deferred_regions(sp->chans[i], -1);

  sp->inuse = SOUND_IDLE;
  if (sp->playing) 
    stop_playing_sound(sp, PLAY_CLOSE);

#if (!USE_NO_GUI)
  sp->inuse = SOUND_NORMAL;               /* needed to make sure the status area is actually cleared in set_status */
  set_status(sp, NULL, false); /* false = don't try to update graphs! */
  sp->inuse = SOUND_IDLE;
#endif

  if ((sp == selected_sound()) &&
      (!(ss->exiting)))
    {
      int i, curmax = -1;
      /* look for the last selected sound, if any */
      for (i = 0; i < ss->max_sounds; i++)
	{
	  snd_info *nsp;
	  nsp = ss->sounds[i];
	  if ((nsp) && 
	      (nsp->inuse == SOUND_NORMAL) &&
	      (nsp != sp) &&
	      (nsp->active) &&
	      (nsp->selectpos > curmax))
	    {
	      curmax = nsp->selectpos;
	      chosen_sp = nsp;
	    }
	}
    }

  /* if sequester_deferred_regions is in free_snd_info (moved up to this level 15-12-03)
   *   If the sound is set to SOUND_IDLE, the init function returns 'no-such-sound, and the
   *   subsequent read segfaults.
   */
  free_snd_info(sp);

  ss->active_sounds--;
  reflect_file_change_in_title();
  enved_reflect_selection(selection_is_active());
  reflect_selection_in_save_as_dialog(selection_is_active());

  if (XEN_HOOKED(ss->snd_open_file_hook))
    run_hook(ss->snd_open_file_hook,
	     XEN_LIST_1(C_TO_XEN_INT(FILE_CLOSED)),
	     "open-file-hook");

  if (XEN_HOOKED(ss->effects_hook))
    run_hook(ss->effects_hook, XEN_EMPTY_LIST, S_effects_hook);

  if (chosen_sp)
    select_channel(chosen_sp, 0);
  else 
    {
      if (sp == selected_sound())
	ss->selected_sound = NO_SELECTION;

      if ((!(ss->exiting)) && 
	  (any_selected_sound() == NULL)) /* I hope this can't be fooled... */
	reset_mix_ctr();
    }
  
  reflect_save_as_sound_selection(NULL);
}


#define TEMP_SOUND_INDEX 123456
/* just a marker for debugging -- but it actually appears in things like start-playing-hook!
 */


snd_info *make_sound_readable(const char *filename, bool post_close)
{
  /* conjure up just enough Snd structure to make this sound readable by the edit-tree readers */
  snd_info *sp;
  chan_info *cp;
  file_info *hdr = NULL;
  int i, fd;
  mus_long_t len;
  /* we've already checked that filename exists */

  hdr = make_file_info_1(filename);
  sp = make_basic_snd_info(hdr->chans);
  sp->nchans = hdr->chans;
  sp->hdr = hdr;
  sp->inuse = SOUND_READER;
  initialize_control_panel(sp);
  sp->index = TEMP_SOUND_INDEX;
  len = (hdr->samples) / (hdr->chans);

  for (i = 0; i < sp->nchans; i++)
    {
      cp = make_chan_info(NULL, i, sp);
      cp->editable = false;
      free(cp->ax);
      cp->ax = NULL;
      sp->chans[i] = cp;
      add_channel_data_1(cp, hdr->srate, len, WITHOUT_GRAPH);

      fd = snd_open_read(filename); /* sends the error if any */
      if (fd != -1)
	set_up_snd_io(cp, i, fd, filename, hdr, post_close);
    }
  sp->active = true;
  return(sp);
}


axes_data *free_axes_data(axes_data *sa)
{
  if (sa)
    {
      if (sa->axis_data) {free(sa->axis_data); sa->axis_data = NULL;}
      if (sa->fftp) {free(sa->fftp); sa->fftp = NULL;}
      if (sa->wavep) {free(sa->wavep); sa->wavep = NULL;}
      free(sa);
    }
  return(NULL);
}


enum {SA_X0, SA_X1, SA_Y0, SA_Y1, SA_XMIN, SA_XMAX, SA_YMIN, SA_YMAX, SA_ZX, SA_ZY, SA_SX, SA_SY, SA_GSY, SA_GZY};
#define SA_FIELDS 14

axes_data *make_axes_data(snd_info *sp)
{
  axes_data *sa;
  int i;
  sa = (axes_data *)calloc(1, sizeof(axes_data));
  sa->chans = sp->nchans;
  sa->fields = SA_FIELDS;
  sa->axis_data = (double *)calloc(sa->fields * sa->chans, sizeof(double));
  sa->fftp = (bool *)calloc(sa->chans, sizeof(bool));
  sa->wavep = (bool *)calloc(sa->chans, sizeof(bool));
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
      sa->axis_data[loc + SA_ZX] = ap->zx;
      sa->axis_data[loc + SA_SX] = ap->sx;
      sa->axis_data[loc + SA_ZY] = ap->zy;
      sa->axis_data[loc + SA_SY] = ap->sy;
      sa->axis_data[loc + SA_GSY] = cp->gsy;
      sa->axis_data[loc + SA_GZY] = cp->gzy;
      sa->wavep[i] = cp->graph_time_p;
      sa->fftp[i] = cp->graph_transform_p;
      
      /* unite and sync buttons are being cleared in snd_info_cleanup and explicitly in snd_update
       *   then probably reset in change_channel_style
       *   meanwhile channel_style is saved in copy_snd_info below
       *   but also restored explicitly in snd_update.
       *   but... if unite was off  before the update, it seems that it is set on multi-channel files 
       *    (via channel_style(ss) which defaults to channels_combined)
       */
    }
  return(sa);
}


void restore_axes_data(snd_info *sp, axes_data *sa, mus_float_t new_duration, bool need_edit_history_update)
{
  int i, j;
  for (i = 0, j = 0; i < sp->nchans; i++)
    {
      chan_info *cp;
      axis_info *ap;
      int loc;

      cp = sp->chans[i];
      loc = j * sa->fields;

      if ((show_full_duration(ss)) &&
	  (sa->axis_data[loc + SA_X0] == 0.0) &&
	  (sa->axis_data[loc + SA_X1] == sa->axis_data[loc + SA_XMAX]))
	{
	  /* we were viewing the full duration when the update occurred, and show-full-duration is #t,
	   *   so show the full new duration.
	   */
	  sa->axis_data[loc + SA_X1] = new_duration;                             /* new x1 */
	}
      else
	{
	  mus_float_t old_duration;                                            /* old duration is x1 - x0 */
	  old_duration = sa->axis_data[loc + SA_X1] - sa->axis_data[loc + SA_X0];  
	  if (new_duration < sa->axis_data[loc + SA_X0])                       /* new duration < old x0 */
	    sa->axis_data[loc + SA_X0] = new_duration - old_duration;          /* try to maintain old window size */
	  if (sa->axis_data[loc + SA_X0] < 0.0) 
	    sa->axis_data[loc + SA_X0] = 0.0;
	  if (new_duration < sa->axis_data[loc + SA_X1]) 
	    sa->axis_data[loc + SA_X1] = new_duration;                         /* new x1 */
	}
      sa->axis_data[loc + SA_XMAX] = new_duration;                             /* new xmax */

      ap = cp->axis;
      ap->xmin = sa->axis_data[loc + SA_XMIN];
      ap->xmax = sa->axis_data[loc + SA_XMAX];
      /* zx and sx are reset by set_axes below? */
      ap->zx = sa->axis_data[loc + SA_ZX];
      ap->sx = sa->axis_data[loc + SA_SX];

      ap->x_ambit = ap->xmax - ap->xmin;
      
      if (!show_full_range(ss))
	{
	  ap->ymin = sa->axis_data[loc + SA_YMIN];
	  ap->ymax = sa->axis_data[loc + SA_YMAX];
	  ap->zy = sa->axis_data[loc + SA_ZY];
	  ap->sy = sa->axis_data[loc + SA_SY];
	  ap->y_ambit = ap->ymax - ap->ymin;
	}
      else
	{
	  sa->axis_data[loc + SA_Y0] = ap->y0;
	  sa->axis_data[loc + SA_Y1] = ap->y1;
	}

      cp->gzy = sa->axis_data[loc + SA_GZY];
      cp->gsy = sa->axis_data[loc + SA_GSY];

      set_axes(cp,
	       sa->axis_data[loc + SA_X0], 
	       sa->axis_data[loc + SA_X1], 
	       sa->axis_data[loc + SA_Y0], 
	       sa->axis_data[loc + SA_Y1]);

      set_z_scrollbars(cp, ap);

      if ((cp->chan == 0) &&
	  (cp->gzy != 1.0))
	change_gzy(cp->gzy, cp); /* also fixes gsy slider */

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
  ncp->tracking_cursor_style = ocp->tracking_cursor_style;
  ncp->cursor_size = ocp->cursor_size;
  ncp->spectro_x_scale = ocp->spectro_x_scale;
  ncp->spectro_y_scale = ocp->spectro_y_scale;
  ncp->spectro_z_scale = ocp->spectro_z_scale;
  ncp->spectro_z_angle = ocp->spectro_z_angle;
  ncp->spectro_x_angle = ocp->spectro_x_angle;
  ncp->spectro_y_angle = ocp->spectro_y_angle;
  ncp->spectrum_end = ocp->spectrum_end;
  ncp->spectrum_start = ocp->spectrum_start;
  ncp->lin_dB = ocp->lin_dB;
  ncp->min_dB = ocp->min_dB;
  ncp->fft_window_alpha = ocp->fft_window_alpha;
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
  nsp->channel_style = osp->channel_style;
  nsp->sync = osp->sync;
}


static snd_info *sound_store_chan_info(snd_info *sp)
{
  chan_info **cps;
  snd_info *nsp;
  int i;

  nsp = (snd_info *)calloc(1, sizeof(snd_info));
  cps = (chan_info **)calloc(sp->nchans, sizeof(chan_info *));
  nsp->chans = cps;
  nsp->nchans = sp->nchans;
  copy_snd_info(nsp, sp);
  for (i = 0; i < sp->nchans; i++)
    {
      cps[i] = (chan_info *)calloc(1, sizeof(chan_info));
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
}


static XEN update_hook;

snd_info *snd_update(snd_info *sp)
{
  /* we can't be real smart here because the channel number may have changed and so on */
  int i, old_srate, old_chans, old_format, sp_chans, old_index, gc_loc = NOT_A_GC_LOC, old_selected_channel = NO_SELECTION;
  channel_style_t old_channel_style;
  read_only_t read_only;
  bool old_raw;
  axes_data *sa;
  snd_info *nsp = NULL;
  char *filename;
  void *ms;
  snd_info *saved_sp;
  struct ctrl_state *saved_controls;
  mus_long_t *old_cursors;
  void *old_file_watcher;
  XEN update_hook_result = XEN_FALSE;
  const char *ur_filename;
  int app_x, app_y;

#if USE_MOTIF
  int snd_height;
  snd_height = snd_pane_height(sp);
#endif

  if (sp->edited_region) return(sp);
  if ((sp->inuse != SOUND_NORMAL) ||
      (!(sp->filename)))
    return(sp);

  if (mus_file_probe(sp->filename) == 0)
    {
      snd_error("%s no longer exists!", sp->short_filename);
      return(sp);
    }

  app_x = widget_width(MAIN_SHELL(ss));
  app_y = widget_height(MAIN_SHELL(ss));
  ur_filename = sp->filename;

  if (peak_env_dir(ss))
    for_each_sound_chan(sp, delete_peak_env_info_file);

  if (with_inset_graph(ss))
    for_each_sound_chan(sp, clear_inset_graph);

  if (XEN_HOOKED(update_hook))
    {
      /* #t => return without updating (not recommended!!), proc of 1 arg will be evaluated after update is complete */
      update_hook_result = run_or_hook(update_hook, 
				       XEN_LIST_1(C_INT_TO_XEN_SOUND(sp->index)),
				       S_update_hook);
      if (XEN_TRUE_P(update_hook_result)) return(sp);
      if (XEN_PROCEDURE_P(update_hook_result))
	{
	  if (XEN_REQUIRED_ARGS_OK(update_hook_result, 1))
	    gc_loc = snd_protect(update_hook_result);
	  else XEN_BAD_ARITY_ERROR(S_update_hook, 0, update_hook_result, S_update_hook " function result should require 1 arg");
	}
    }

  filename = mus_strdup(ur_filename);
  read_only = sp->user_read_only;
  if (sp->nchans > 1)
    ss->update_sound_channel_style = sp->channel_style;
  sa = make_axes_data(sp);
  old_raw = (sp->hdr->type == MUS_RAW);
  if (old_raw)
    {
      mus_header_raw_defaults(&old_srate, &old_chans, &old_format);
      mus_header_set_raw_defaults(sp->hdr->srate, sp->hdr->chans, sp->hdr->format);
    }

  if (sp == selected_sound())
    old_selected_channel = sp->selected_channel;

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
  old_cursors = (mus_long_t *)calloc(sp_chans, sizeof(mus_long_t));

  /* peak-env code saves the current peak-envs on exit (snd_close), but in this case, that
   *   data is known to be out-of-date.  Since we'll be freeing it eventually anyway, we
   *   do it first here, and the peak-env update-hook clobbers the existing files
   */
  for (i = 0; i < sp_chans; i++) 
    {
      chan_info *ncp;
      int k;
      ncp = sp->chans[i];
      old_cursors[i] = CURSOR(ncp);
      for (k = 0; k < ncp->edit_size; k++)
	{
	  ed_list *ed;
	  ed = ncp->edits[k];
	  if (ed)
	    ed->peak_env = free_peak_env(ncp, k);
	}
    }
  old_file_watcher = sp->file_watcher; /* will be unmonitored in snd_close_file, but we need to know if it is being monitored now */

  snd_close_file(sp);

  /* no mus_sound_forget here because we may be simply re-interpreting the existing data (set! (data-format) ...) etc */
  /* this normalizes the fft/lisp/wave state so we need to reset it after reopen */

  if (!(ss->file_monitor_ok))
    alert_new_file();

  ss->reloading_updated_file = (old_index + 1);
  ss->open_requestor = FROM_UPDATE;

  nsp = snd_open_file(filename, read_only);

#if USE_MOTIF
  XtVaSetValues(w_snd_pane(sp),
		    XmNpaneMinimum, snd_height,
		    XmNpaneMaximum, snd_height,
		    NULL);
#endif
  set_widget_size(MAIN_SHELL(ss), app_x, app_y); /* was at end */

  ss->reloading_updated_file = 0;
  if (old_raw)
    mus_header_set_raw_defaults(old_srate, old_chans, old_format);
  if (nsp)
    {
      /* if header is bad, nsp can be null awaiting raw data dialog's return */

      if ((old_file_watcher) &&
	  (!(nsp->file_watcher)))
	monitor_sound(nsp);
      /* might be a different sp as well as underlying file */

      nsp->saved_controls = saved_controls;
      if (saved_controls) restore_controls(nsp);
      if (nsp->nchans == sp_chans) sound_restore_chan_info(nsp, saved_sp);

      if ((old_selected_channel != NO_SELECTION) &&
	  (old_selected_channel < nsp->nchans) &&
	  (nsp == selected_sound()))
	select_channel(nsp, old_selected_channel);

      restore_axes_data(nsp, sa, mus_sound_duration(filename), false);
      sound_restore_marks(nsp, ms);

      for (i = 0; (i < nsp->nchans) && (i < sp_chans); i++) 
	CURSOR(nsp->chans[i]) = old_cursors[i];

      if ((nsp->nchans > 1) && 
	  (old_channel_style != CHANNELS_SEPARATE)) /* we set it to separate before the update */
	set_sound_channel_style(nsp, old_channel_style);
    }

  free(old_cursors);

  if (XEN_PROCEDURE_P(update_hook_result))
    {
      XEN_CALL_1(update_hook_result,
		 (nsp) ? C_INT_TO_XEN_SOUND(nsp->index) : XEN_FALSE,
		 "procedure returned by " S_update_hook);
      if (gc_loc != NOT_A_GC_LOC) snd_unprotect_at(gc_loc);
    }

  if (saved_sp)
    {
      for (i = 0; i < saved_sp->nchans; i++)
	if (saved_sp->chans[i]) free(saved_sp->chans[i]);
      free(saved_sp->chans);
      free(saved_sp);
    }
  sa = free_axes_data((axes_data *)sa);
  free(filename);

#if USE_MOTIF
  XtVaSetValues(w_snd_pane(sp),
		    XmNpaneMinimum, 1,
		    XmNpaneMaximum, LOTSA_PIXELS,
		    NULL);
#endif

  return(nsp);
}


static void snd_update_warning_handler(const char *msg, void *data)
{
  set_status((snd_info *)data, msg, false);
}


static void snd_update_error_handler(const char *msg, void *data)
{
  redirect_snd_error_to(NULL, NULL);
  redirect_snd_warning_to(NULL, NULL);
  XEN_ERROR(CANT_UPDATE_FILE,
	    XEN_LIST_3(C_TO_XEN_STRING("~A: ~A"),
		       C_TO_XEN_STRING((char *)data),
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
		     XEN_LIST_3((sp) ? C_INT_TO_XEN_SOUND(sp->index) : XEN_FALSE,
				C_TO_XEN_STRING(fullname),
				C_TO_XEN_BOOLEAN(from_save_as_dialog)),
		     S_after_save_as_hook);
      free(fullname);
    }
}


static XEN before_save_as_hook;
static bool before_save_as_hook_active = false;

bool run_before_save_as_hook(snd_info *sp, const char *save_as_filename, bool selection, int srate, int type, int format, const char *comment)
{
  /* might be save-selection, as well as save-sound-as */
  if (before_save_as_hook_active) return(false);
  if (XEN_HOOKED(before_save_as_hook))
    {
      XEN result = XEN_FALSE;
      before_save_as_hook_active = true;
      result = run_progn_hook(before_save_as_hook,
			      XEN_LIST_7((sp) ? C_INT_TO_XEN_SOUND(sp->index) : XEN_FALSE,
					 C_TO_XEN_STRING(save_as_filename),
					 C_TO_XEN_BOOLEAN(selection),
					 C_TO_XEN_INT(srate),
					 C_TO_XEN_INT(type),
					 C_TO_XEN_INT(format),
					 (comment) ? C_TO_XEN_STRING(comment) : XEN_FALSE),
			      S_before_save_as_hook);
      before_save_as_hook_active = false;
      return(XEN_TRUE_P(result));
    }
  return(false);
}


/* -------- file dialog header/data choices -------- */

enum {H_NEXT, H_AIFC, H_RIFF, H_RF64, H_RAW, H_AIFF, H_IRCAM, H_NIST, H_CAFF, /* the "built-in" choices for output */
      H_OGG, H_FLAC, H_SPEEX, H_TTA, H_WAVPACK,                               /* readable/writable via external programs */
      H_MPEG, H_MIDI, H_SHORTEN,                                              /* readable via external programs */
      H_SIZE};

static int h_num_formats[H_SIZE] = {12 /* next */, 13 /* aifc */,  8 /* riff */, 8 /* rf64 */, 18 /* raw */, 4 /* aiff */, 5  /* ircam */, 7 /* nist */, 13 /* caff */,
				    1 /* ogg */,  1  /* flac */,  1 /* speex */, 1 /* tta */, 1 /*wavpack */,
				    1 /* mpeg */, 1  /* midi */,  1 /* shorten */};
#define H_DFS_MAX 18

static int h_dfs[H_SIZE][H_DFS_MAX] = { /* next */  {MUS_BFLOAT, MUS_BSHORT, MUS_LSHORT, MUS_LFLOAT, 
						     MUS_MULAW, MUS_BYTE, MUS_BINT, MUS_ALAW, MUS_B24INT, MUS_BDOUBLE, MUS_LINT, MUS_LDOUBLE},
					/* aifc */  {MUS_BFLOAT, MUS_BSHORT, MUS_MULAW, MUS_BYTE, MUS_BINT, MUS_ALAW, MUS_B24INT,
						     MUS_BDOUBLE, MUS_UBYTE, MUS_LSHORT, MUS_LINT, MUS_L24INT, MUS_UBSHORT},
					/* riff */  {MUS_LFLOAT, MUS_LSHORT, MUS_MULAW, MUS_ALAW, MUS_UBYTE, MUS_LINT, MUS_LDOUBLE, MUS_L24INT},
					/* rf64 */  {MUS_LFLOAT, MUS_LSHORT, MUS_MULAW, MUS_ALAW, MUS_UBYTE, MUS_LINT, MUS_LDOUBLE, MUS_L24INT},
					/* raw  */  {MUS_BFLOAT, MUS_LFLOAT, MUS_BSHORT, MUS_MULAW, MUS_BYTE, MUS_BINT, MUS_ALAW,
						     MUS_UBYTE, MUS_B24INT, MUS_BDOUBLE, MUS_LSHORT, MUS_LINT,
						     MUS_LDOUBLE, MUS_UBSHORT, MUS_ULSHORT, MUS_L24INT, MUS_BINTN, MUS_LINTN},
					/* aiff */  {MUS_BSHORT, MUS_BINT, MUS_BYTE, MUS_B24INT},
					/* ircam */ {MUS_BFLOAT, MUS_BSHORT, MUS_MULAW, MUS_BINT, MUS_ALAW},
					/* nist */  {MUS_BSHORT, MUS_LSHORT, MUS_BINT, MUS_LINT, MUS_BYTE, MUS_B24INT, MUS_L24INT},
					/* caff  */ {MUS_BFLOAT, MUS_BSHORT, MUS_LFLOAT, MUS_LSHORT, MUS_MULAW, MUS_BYTE, MUS_BINTN, MUS_ALAW,
						     MUS_B24INT, MUS_BDOUBLE, MUS_LINTN, MUS_L24INT, MUS_LDOUBLE},
					/* ogg */   {MUS_LSHORT},
					/* flac */  {MUS_LSHORT},
					/* speex */ {MUS_LSHORT},
					/* tta */   {MUS_LSHORT},
					/* wavpack */ {MUS_LSHORT},
					/* readonly */  {-1}, {-1}, {-1}
};
static const char *h_df_names[H_SIZE][H_DFS_MAX];

static const char *h_names[H_SIZE] = {"au/next  ", "aifc   ", "wave   ", "rf64  ", "raw    ", "aiff   ", "ircam ", "nist  ", "caff  ",
				      "ogg   ", "flac  ", "speex ", "tta   ", "wavpack",
				      "mpeg  ", "midi  ", "shorten"};
static int h_pos_to_type[H_SIZE] = {MUS_NEXT, MUS_AIFC, MUS_RIFF, MUS_RF64, MUS_RAW, MUS_AIFF, MUS_IRCAM, MUS_NIST, MUS_CAFF, -1, -1, -1, -1, -1, -1, -1, -1};
static int h_type_to_pos[MUS_NUM_HEADER_TYPES];
static int h_type_to_h[MUS_NUM_HEADER_TYPES];


static const char *data_format_name(int format)
{
  switch (format)
    {
    case MUS_BSHORT:           return("16-bit int (be)");      break;
    case MUS_MULAW:            return("mulaw");                break;
    case MUS_BYTE:             return("8-bit int");            break;
    case MUS_BFLOAT:           return("float (be)");           break;
    case MUS_BFLOAT_UNSCALED:  return("float unscaled (be))"); break;
    case MUS_BINT:             return("32-bit int (be)");      break;
    case MUS_ALAW:             return("alaw");                 break;
    case MUS_UBYTE:            return("unsigned byte");        break;
    case MUS_B24INT:           return("24-bit int (be)");      break;
    case MUS_BDOUBLE:          return("double (be)");          break;
    case MUS_BDOUBLE_UNSCALED: return("double unscaled (be)"); break;
    case MUS_LSHORT:           return("16-bit int (le)");      break;
    case MUS_LINT:             return("32-bit int (le)");      break;
    case MUS_LFLOAT:           return("float (le)");           break;
    case MUS_LDOUBLE:          return("double (le)");          break;
    case MUS_LFLOAT_UNSCALED:  return("float unscaled (le)");  break;
    case MUS_LDOUBLE_UNSCALED: return("double unscaled (le)"); break;
    case MUS_UBSHORT:          return("unsigned short (be)");  break;
    case MUS_ULSHORT:          return("unsigned short (le)");  break;
    case MUS_L24INT:           return("24-bit int (le)");      break;
    case MUS_BINTN:            return("normalized int (be)");  break;
    case MUS_LINTN:            return("normalized int (le)");  break;
    default:                   return("unknown");              break;
    }
}

void initialize_format_lists(void)
{
  int i, h;

  for (h = 0; h < H_SIZE; h++)
    for (i = 0; i < h_num_formats[h]; i++)
      h_df_names[h][i] = data_format_name(h_dfs[h][i]);
  
  for (i = 0; i < MUS_NUM_HEADER_TYPES; i++)
    {
      h_type_to_pos[i] = -1;
      h_type_to_h[i] = -1;
    }
  h_type_to_pos[MUS_NEXT] = 0;
  h_type_to_pos[MUS_AIFC] = 1;
  h_type_to_pos[MUS_RIFF] = 2;
  h_type_to_pos[MUS_RF64] = 3;
  h_type_to_pos[MUS_RAW] = 4;
  h_type_to_pos[MUS_AIFF] = 5;
  h_type_to_pos[MUS_IRCAM] = 6;
  h_type_to_pos[MUS_NIST] = 7;
  h_type_to_pos[MUS_CAFF] = 8;

  h_type_to_h[MUS_NEXT] = H_NEXT;
  h_type_to_h[MUS_AIFC] = H_AIFC;
  h_type_to_h[MUS_RIFF] = H_RIFF;
  h_type_to_h[MUS_RF64] = H_RF64;
  h_type_to_h[MUS_RAW] = H_RAW;
  h_type_to_h[MUS_AIFF] = H_AIFF;
  h_type_to_h[MUS_IRCAM] = H_IRCAM;
  h_type_to_h[MUS_NIST] = H_NIST;
  h_type_to_h[MUS_CAFF] = H_CAFF;
  h_type_to_h[MUS_OGG] = H_OGG;
  h_type_to_h[MUS_FLAC] = H_FLAC;
  h_type_to_h[MUS_SPEEX] = H_SPEEX;
  h_type_to_h[MUS_MIDI] = H_MIDI;
  h_type_to_h[MUS_MPEG] = H_MPEG;
  h_type_to_h[MUS_SHORTEN] = H_SHORTEN;
  h_type_to_h[MUS_TTA] = H_TTA;
  h_type_to_h[MUS_WAVPACK] = H_WAVPACK;
  
  i = 9;
  /* readable/writable */
#if HAVE_OGG
  h_type_to_pos[MUS_OGG] = i;
  h_pos_to_type[i++] = MUS_OGG;
#endif

#if HAVE_FLAC
  h_type_to_pos[MUS_FLAC] = i;
  h_pos_to_type[i++] = MUS_FLAC;
#endif

#if HAVE_SPEEX
  h_type_to_pos[MUS_SPEEX] = i;
  h_pos_to_type[i++] = MUS_SPEEX;
#endif

#if HAVE_TTA
  h_type_to_pos[MUS_TTA] = i;
  h_pos_to_type[i++] = MUS_TTA;
#endif

#if HAVE_WAVPACK
  h_type_to_pos[MUS_WAVPACK] = i;
  h_pos_to_type[i++] = MUS_WAVPACK;
#endif

  /* readable */
#if HAVE_MPEG
  h_type_to_pos[MUS_MPEG] = i;
  h_pos_to_type[i++] = MUS_MPEG;
#endif

#if HAVE_MIDI
  h_type_to_pos[MUS_MIDI] = i;
  h_pos_to_type[i++] = MUS_MIDI;
#endif

#if HAVE_SHORTEN
  h_type_to_pos[MUS_SHORTEN] = i;
  h_pos_to_type[i++] = MUS_SHORTEN;
#endif
}


#define NUM_BUILTIN_HEADERS 9
#define NUM_POSSIBLE_HEADERS 16
static const char **writable_headers = NULL;
static const char **readable_headers = NULL;
static int num_writable_headers = NUM_BUILTIN_HEADERS;
static int num_readable_headers = NUM_BUILTIN_HEADERS;


const char **short_builtin_headers(int *len)
{
  (*len) = NUM_BUILTIN_HEADERS;
  return(h_names);
}


const char **short_writable_headers(int *len)
{
  /* these are headers that we either write ourself, or have external programs to write (oggenc etc) */
  int i;
  if (!writable_headers)
    {
      writable_headers = (const char **)calloc(NUM_POSSIBLE_HEADERS, sizeof(char *));
      for (i = 0; i < NUM_BUILTIN_HEADERS; i++)
	writable_headers[i] = h_names[i];
#if HAVE_OGG
      /* write temp as RIFF 16-bit lshort, then
	 oggenc tempfile.wav -o outoggfile.ogg
	 -q 6 sets to higher quality -- add as arg somehow?
	 so format here is just the ogg output format: nothing settable
      */
      writable_headers[i++] = h_names[H_OGG];
#endif

#if HAVE_FLAC
      /* flac tempfile.wav -o output.flac */
      writable_headers[i++] = h_names[H_FLAC];
#endif

#if HAVE_SPEEX
      /* speexenc tempfile.wav output.spx */
      writable_headers[i++] = h_names[H_SPEEX];
#endif

#if HAVE_TTA
      /* ttaenc -e in out */
      writable_headers[i++] = h_names[H_TTA];
#endif

#if HAVE_WAVPACK
      /* wavpack in -o out */
      writable_headers[i++] = h_names[H_WAVPACK];
#endif

      num_writable_headers = i;
    }
  (*len) = num_writable_headers;
  return(writable_headers);
}


const char **short_readable_headers(int *len)
{
  int i;
  if (!readable_headers)
    {
      readable_headers = (const char **)calloc(NUM_POSSIBLE_HEADERS, sizeof(char *));
      for (i = 0; i < NUM_BUILTIN_HEADERS; i++)
	readable_headers[i] = h_names[i];
#if HAVE_OGG
      /* oggdec to tempfile:
	 oggdec infile.ogg -b 16 -o translatedfile.wav
	 defaults for rest are signed little endian wav
	 
	 so formats (for output temp) here are not relevant -- no one will want 1 byte
      */
      readable_headers[i++] = h_names[H_OGG];
#endif

#if HAVE_FLAC
      /* flac -d infile.flac -o output.wav
       *  the -d -> decode 
       *  no choices
       */
      readable_headers[i++] = h_names[H_FLAC];
#endif

#if HAVE_SPEEX
      /* speexdec infile.spx tempfile.wav
	 no other choices
       */
      readable_headers[i++] = h_names[H_SPEEX];
#endif

#if HAVE_MPEG
      /* mpg321 -q -w output.wav input.mpg
	   this is mpeg->wav only
	   no choices
	   mpg123 is probably the same
       */
      readable_headers[i++] = h_names[H_MPEG];
#endif

#if HAVE_TIMIDITY
      /* timidity input.mid -Ou -o output.snd
	 midi->next/sun
	 there are other choices...
      */
      readable_headers[i++] = h_names[H_MIDI];
#endif

#if HAVE_SHORTEN
      readable_headers[i++] = h_names[H_SHORTEN];
#endif

#if HAVE_TTA
      readable_headers[i++] = h_names[H_TTA];
#endif

#if HAVE_WAVPACK
      readable_headers[i++] = h_names[H_WAVPACK];
#endif
      num_readable_headers = i;
    }
  (*len) = num_readable_headers;
  return(readable_headers);
}


int position_to_type(int position)
{
  return(h_pos_to_type[position]);
}


int position_to_format(int header, int position)
{
  return(h_dfs[h_type_to_pos[header]][position]);
}


static int h_to_format_pos(int h, int frm)
{
  int i;
  for (i = 0; i < h_num_formats[h]; i++)
    if (h_dfs[h][i] == frm)
      return(i);
  return(0);
}


const char **type_and_format_to_position(file_data *fdat, int type, int format)
{
  int h;
  h = h_type_to_h[type];
  fdat->formats = h_num_formats[h];
  fdat->header_pos = h_type_to_pos[type];
  fdat->format_pos = h_to_format_pos(h, format);
  return(h_df_names[h]);
}


void position_to_type_and_format(file_data *fdat, int pos)
{
  int h;
  h = h_type_to_h[h_pos_to_type[pos]];
  fdat->header_pos = pos;
  fdat->current_type = h_pos_to_type[pos];
  fdat->format_pos = h_to_format_pos(h, fdat->current_format);
  fdat->current_format = h_dfs[h][fdat->format_pos];
}


bool encoded_header_p(int header_type)
{
  /* going either way here */
  return((header_type == MUS_OGG) ||
	 (header_type == MUS_FLAC) ||
	 (header_type == MUS_SPEEX) ||
	 (header_type == MUS_MPEG) ||
	 (header_type == MUS_MIDI) ||
	 (header_type == MUS_SHORTEN) ||
	 (header_type == MUS_TTA) ||
	 (header_type == MUS_WAVPACK)
	 );
}


void snd_encode(int type, const char *input_filename, const char *output_filename)
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

#if HAVE_TTA
    case MUS_TTA:
      command = mus_format("%s -e %s -o %s", PATH_TTA, input_filename, output_filename);
      break;
#endif

#if HAVE_WAVPACK
    case MUS_WAVPACK:
      command = mus_format("%s %s -o %s", PATH_WAVPACK, input_filename, output_filename);
      break;
#endif

    default: 
      break;
    }
  if (command)
    {
      int err;
      err = system(command);
      free(command);
      if (err == -1)
	fprintf(stderr, "%s failed?", command);
    }
}


int snd_decode(int type, const char *input_filename, const char *output_filename)
{
  int err = 0;
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

#if HAVE_SHORTEN
    case MUS_SHORTEN:
      command = mus_format("%s -x %s %s", PATH_SHORTEN, input_filename, output_filename);
      break;
#endif

#if HAVE_TTA
    case MUS_TTA:
      command = mus_format("%s -d %s -o %s", PATH_TTA, input_filename, output_filename);
      break;
#endif

#if HAVE_WAVPACK
    case MUS_WAVPACK:
      command = mus_format("%s %s -o %s", PATH_WVUNPACK, input_filename, output_filename);
      break;
#endif
    default: 
      err = -1;
      break;
    }
  if (command)
    {
      int err;
      err = system(command);
      free(command);
      if (err == -1)
	fprintf(stderr, "%s failed?", command);
    }
  return(err);
}


typedef struct {
  snd_info *parlous_sp, *current_sp;
  char *filename;
} same_name_info;

static bool check_for_same_name(snd_info *sp1, same_name_info *info)
{
  if ((sp1) && 
      (sp1 != info->current_sp) &&
      (mus_strcmp(sp1->filename, info->filename)))
    {
      if (has_unsaved_edits(sp1))
	{
	  info->parlous_sp = sp1;
	  return(true);
	}
    }
  return(false);
}


static void map_over_sounds_with_collision(bool (*func)(snd_info *, same_name_info *col), same_name_info *collision)
{
  /* true = abort map, skips inactive sounds */
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	{
	  bool val;
	  val = (*func)(sp, collision);
	  if (val) return;
	}
    }
}


snd_info *file_is_open_elsewhere_and_has_unsaved_edits(snd_info *sp, const char *fullname)
{
  same_name_info *collision;
  snd_info *result;
  collision = (same_name_info *)calloc(1, sizeof(same_name_info));
  collision->filename = (char *)fullname;
  collision->parlous_sp = NULL;
  collision->current_sp = sp;
  map_over_sounds_with_collision(check_for_same_name, collision);
  result = collision->parlous_sp;
  free(collision);
  return(result);
}


bool edit_header_callback(snd_info *sp, file_data *edit_header_data, 
			  void (*outer_handler)(const char *error_msg, void *ufd),
			  void (*inner_handler)(const char *error_msg, void *ufd))
{
  /* this just changes the header -- it does not actually reformat the data or whatever */

  mus_long_t loc, samples;
  char *comment, *original_comment = NULL;
  file_info *hdr;
  int chans, srate, type, format;
  if ((sp->user_read_only == FILE_READ_ONLY) || 
      (sp->file_read_only == FILE_READ_ONLY))
    {
      snd_error("%s is write-protected", sp->filename);
      return(false);
    }
#if HAVE_ACCESS
  if (!(ss->file_monitor_ok))
    if (access(sp->filename, W_OK) < 0)
      {
	snd_error("%s is write-protected", sp->filename);
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
  if (!(mus_strcmp(comment, original_comment)))
    mus_header_change_comment(sp->filename, type, comment);
  if (comment) free(comment);
  if (original_comment) free(original_comment);
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


static mus_long_t swap_mus_long_t(mus_long_t n)
{
  mus_long_t o;
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
  mus_long_t nsamp;
  bool ok;
  int ns, better_srate = 0, better_chans = 0, len;
  reason_str = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
  tmp_str = (char *)calloc(LABEL_BUFFER_SIZE, sizeof(char));
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
	  reason_str = mus_strcat(reason_str, tmp_str, &len);
	}
    }

  /* chans */
  ok = ((original_chans > 0) && 
	(original_chans < 1000));
  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\nchans%s: %d", (ok) ? "" : " looks wrong", original_chans);
  reason_str = mus_strcat(reason_str, tmp_str, &len);
  if (!ok)
    {
      ns = swap_int(original_chans);
      if ((ns < 0) || (ns > 8)) 
	ns = swap_short((short)(original_chans));
      if ((ns > 0) && (ns <= 8))
	{
	  better_chans = ns;
	  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, " (swapped: %d)", ns);
	  reason_str = mus_strcat(reason_str, tmp_str, &len);
	}
    }

  /* header type */
  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\ntype: %s", mus_header_type_name(hdr->type));
  reason_str = mus_strcat(reason_str, tmp_str, &len);

  /* data format */
  if (!(mus_data_format_p(original_format)))
    {
      char *format_info;
      if (original_format != MUS_UNKNOWN)
	format_info = (char *)mus_data_format_name(original_format);
      else format_info = (char *)mus_header_original_format_name(mus_sound_original_format(filename), 
								 hdr->type);
      mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\nformat looks bogus: %s", format_info);
    }
  else mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\nformat: %s", (char *)mus_data_format_name(original_format));
  reason_str = mus_strcat(reason_str, tmp_str, &len);

  /* samples */
  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\nlength: %.3f (%lld samples, %lld bytes total)",
	       (float)((double)(hdr->samples) / (float)(hdr->chans * hdr->srate)),
	       hdr->samples,
	       mus_sound_length(filename));
  reason_str = mus_strcat(reason_str, tmp_str, &len);
  nsamp = swap_mus_long_t(hdr->samples);
  if (nsamp < mus_sound_length(filename))
    {
      mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, " (swapped: %lld" , nsamp);
      reason_str = mus_strcat(reason_str, tmp_str, &len);
      if ((better_chans) && (better_srate))
	{
	  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE,
		       ", swapped length: %.3f / sample-size-in-bytes)",
		       (float)((double)nsamp / (float)(better_chans * better_srate)));
	  reason_str = mus_strcat(reason_str, tmp_str, &len);
	}
      else reason_str = mus_strcat(reason_str, ")", &len);
    }

  /* data location */
  mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, "\ndata location: %lld", hdr->data_location);
  reason_str = mus_strcat(reason_str, tmp_str, &len);
  nsamp = swap_mus_long_t(hdr->data_location);
  if ((nsamp > 0) && 
      (nsamp <= 1024)) 
    {
      mus_snprintf(tmp_str, LABEL_BUFFER_SIZE, " (swapped: %lld)", nsamp);
      reason_str = mus_strcat(reason_str, tmp_str, &len);
    }
  (*info) = reason_str;
  file_string = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
  mus_snprintf(file_string, PRINT_BUFFER_SIZE,
	       "Bad header found on %s", 
	       filename_without_directory(filename));
  free(tmp_str);
  free_file_info(hdr);
  return(file_string);
}
#endif


/* -------- view files shared code -------- */

static void view_files_clear_selected_files(view_files_info *vdat);
static int view_files_add_selected_file(view_files_info *vdat, vf_row *r);
static int view_files_find_row(view_files_info *vdat, const char *name);
static int view_files_info_size = 0;
static view_files_info **view_files_infos = NULL;

static XEN vf_open_file_watcher(XEN hook_or_reason)
{
  int k;
  /* reasons are FILE_OPENED|CLOSED, but it's not worth the trouble of splitting them out here */
  
  /* loop through all vf dialogs ... */
  for (k = 0; k < view_files_info_size; k++)
    if ((view_files_infos[k]) &&
	(view_files_infos[k]->dialog) &&
	(widget_is_active(view_files_infos[k]->dialog)))
      {
	view_files_info *vdat;
	vdat = view_files_infos[k];
	vf_mix_insert_buttons_set_sensitive(vdat, 
					    ((vdat->currently_selected_files > 0) &&
					     (any_selected_sound())));
      }
  return(XEN_FALSE);
}

#ifdef XEN_ARGIFY_1
  XEN_NARGIFY_1(vf_open_file_watcher_w, vf_open_file_watcher)
#else
  #define vf_open_file_watcher_w vf_open_file_watcher
#endif


int view_files_dialog_list_length(void)
{
  int i, n = 0;
  for (i = 0; i < view_files_info_size; i++)
    if ((view_files_infos[i]) &&
	(view_files_infos[i]->dialog))
      n++;
  return(n);
}


char **view_files_dialog_titles(void)
{
  int n;
  n = view_files_dialog_list_length();
  if (n > 0)
    {
      char **titles;
      int i, j = 0;
      titles = (char **)calloc(n + 1, sizeof(char *));
      for (i = 0; i < view_files_info_size; i++)
	if ((view_files_infos[i]) &&
	    (view_files_infos[i]->dialog))
	  titles[j++] = dialog_get_title(view_files_infos[i]->dialog);
      return(titles);
    }
  return(NULL);
}


void view_files_start_dialog_with_title(const char *title)
{
  int i;
  for (i = 0; i < view_files_info_size; i++)
    if ((view_files_infos[i]) &&
	(view_files_infos[i]->dialog))
      {
	char *dialog_title = NULL;
	dialog_title = dialog_get_title(view_files_infos[i]->dialog);
	if (mus_strcmp(title, dialog_title)) /* this includes NULL == NULL */
	  {
	    if (dialog_title) free(dialog_title);
	    make_view_files_dialog_1(view_files_infos[i], true);
	    return;
	  }
	if (dialog_title) free(dialog_title);
      }
}


view_files_info *new_view_files_dialog(void)
{
  /* always returns a new (empty) file viewer -- changed 8-Jan-08 */
  int loc = -1;
  view_files_info *vdat;

  if (view_files_info_size == 0)
    {
      loc = 0;
      view_files_info_size = 4;
      view_files_infos = (view_files_info **)calloc(view_files_info_size, sizeof(view_files_info *));
    }
  else
    {
      int i;
      for (i = 0; i < view_files_info_size; i++)
	if (!view_files_infos[i])
	  {
	    loc = i;
	    break;
	  }
      if (loc == -1)
	{
	  loc = view_files_info_size;
	  view_files_info_size += 4;
	  view_files_infos = (view_files_info **)realloc(view_files_infos, view_files_info_size * sizeof(view_files_info *));
	  for (i = loc; i < view_files_info_size; i++) view_files_infos[i] = NULL;
	}
    }

  view_files_infos[loc] = (view_files_info *)calloc(1, sizeof(view_files_info));
  vdat = view_files_infos[loc];
  vdat->index = loc;
  vdat->dialog = NULL_WIDGET;
  vdat->file_list = NULL_WIDGET;
  vdat->file_list_holder = NULL_WIDGET;
  vdat->file_list_entries = NULL;
  vdat->size = 0;
  vdat->end = -1;
  vdat->names = NULL;
  vdat->full_names = NULL;
  vdat->selected_files = NULL;
  vdat->selected_files_size = 0;
  vdat->location_choice = VF_AT_CURSOR;
  vdat->error_p = false;
  vdat->need_update = false;
  vdat->dirs_size = 0;
  vdat->dirs = NULL;
  vdat->dir_names = NULL;
  vdat->amp = 1.0;
  vdat->speed = 1.0;
  vdat->amp_env = default_env(1.0, 1.0);
  vdat->sort_items_size = 0;
  vdat->sort_items = NULL;
  vdat->speed_style = speed_control_style(ss);

#if USE_GTK
  vdat->at_sample_text_handler_id = 0;
  vdat->at_mark_text_handler_id = 0;
  vdat->at_sample_button_handler_id = 0; 
  vdat->at_mark_button_handler_id = 0;
  vdat->add_text_handler_id = 0;
#endif

  /* don't clear at this point! */
  view_files_infos[loc]->currently_selected_files = 0;
  view_files_infos[loc]->sorter = view_files_sort(ss);
  return(view_files_infos[loc]);
}


static int vf_dialog_to_index(widget_t dialog)
{
  int i;
  if ((!dialog) &&
      (view_files_infos[0]) &&
      (view_files_infos[0]->dialog))
    return(0);
  for (i = 0; i < view_files_info_size; i++)
    if ((view_files_infos[i]) &&
	(view_files_infos[i]->dialog == dialog))
      return(i);
  return(-1);
}


static view_files_info *vf_dialog_to_info(widget_t dialog)
{
  int index;
  index = vf_dialog_to_index(dialog);
  if (index >= 0)
    return(view_files_infos[index]);
  return(NULL);
}


static char **vf_selected_files(view_files_info *vdat)
{
  int len;
  char **files = NULL;
  len = vdat->currently_selected_files;
  if (len > 0)
    {
      int i;
      files = (char **)calloc(len, sizeof(char *));
      for (i = 0; i < len; i++) 
	files[i] = mus_strdup(vdat->full_names[vdat->selected_files[i]]);
    }
  return(files);
}


static char **view_files_selected_files(widget_t dialog, int *len)
{
  /* free result */
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    {
      (*len) = vdat->currently_selected_files;
      return(vf_selected_files(vdat));
    }
  (*len) = 0;
  return(NULL);
}


static void view_files_run_select_hook(widget_t dialog, const char *selected_file);

static char **view_files_set_selected_files(widget_t dialog, char **files, int len)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    {
      int i;
      view_files_clear_selected_files(vdat);
      for (i = 0; i < len; i++)
	if (files[i])
	  {
	    int loc;
	    loc = view_files_find_row(vdat, (const char *)(files[i]));
	    if (loc >= 0)
	      {
		view_files_add_selected_file(vdat, vdat->file_list_entries[loc]);
		view_files_run_select_hook(vdat->dialog, (const char *)(files[i]));
	      }
	  }
      vf_mix_insert_buttons_set_sensitive(vdat, 
					  ((vdat->currently_selected_files > 0) &&
					   (any_selected_sound())));
    }
  return(files);
}


static char **view_files_files(widget_t dialog, int *len)
{
  /* don't free result! */
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    {
      (*len) = vdat->end + 1;
      return(vdat->full_names);
    }
  (*len) = 0;
  return(NULL);
}


static void view_files_clear_list(view_files_info *vdat);

static char **view_files_set_files(widget_t dialog, char **files, int len)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    {
      int i;
      view_files_clear_selected_files(vdat);
      view_files_clear_list(vdat);
      if (len > 0)
	{
	  for (i = 0; i < len; i++)
	    if (files[i])
	      view_files_add_file_or_directory(vdat, (const char *)(files[i]));
	}
      view_files_display_list(vdat);
    }
  return(files);
}


void vf_mix_insert_buttons_set_sensitive(view_files_info *vdat, bool sensitive)
{
  if (vdat->mixB)
    {
      set_sensitive(vdat->mixB, sensitive);
      set_sensitive(vdat->insertB, sensitive);
    }
}


static void view_files_clear_selected_files(view_files_info *vdat)
{
  int len;
  len = vdat->currently_selected_files;
  if (len > 0)
    {
      int i;
      for (i = 0; i < len; i++)
	{
	  vf_row *r;
	  r = vdat->file_list_entries[vdat->selected_files[i]];
	  if (r)
	    vf_unhighlight_row(r->nm, r->rw);
	}
    }
  vdat->currently_selected_files = 0;
  vf_mix_insert_buttons_set_sensitive(vdat, false);
}


static void view_files_unselect_file(view_files_info *vdat, vf_row *r)
{
  vf_unhighlight_row(r->nm, r->rw);
  if (vdat->currently_selected_files > 1)
    {
      /* need to fixup selected_files list */
      int i, new_loc = 0;
      for (i = 0; i < vdat->currently_selected_files; i++)
	if (vdat->selected_files[i] != r->pos)
	  vdat->selected_files[new_loc++] = vdat->selected_files[i];
    }
  vdat->currently_selected_files--;
  if (vdat->currently_selected_files < 0) 
    vdat->currently_selected_files = 0;
  if (vdat->currently_selected_files == 0)
    {
      vf_mix_insert_buttons_set_sensitive(vdat, false);
      vf_unpost_info(vdat);
    }
}


static int view_files_add_selected_file(view_files_info *vdat, vf_row *r)
{
  /* returns how many are now selected (counting new) */
  if (vdat->selected_files_size == 0)
    {
      vdat->selected_files_size = 4;
      vdat->selected_files = (int *)calloc(vdat->selected_files_size, sizeof(int));
      vdat->selected_files[0] = r->pos;
      vdat->currently_selected_files = 1;
    }
  else
    {
      if (vdat->currently_selected_files >= vdat->selected_files_size)
	{
	  vdat->selected_files_size += 4;
	  vdat->selected_files = (int *)realloc(vdat->selected_files, vdat->selected_files_size * sizeof(int));
	  vdat->selected_files[vdat->currently_selected_files++] = r->pos;
	}
      else 
	{
	  vdat->selected_files[vdat->currently_selected_files++] = r->pos;
	}
    }
  vf_highlight_row(r->nm, r->rw);
  return(vdat->currently_selected_files);
}


static void vf_fixup_selected_files(view_files_info *vdat, char **saved_selected_files, int len)
{
  /* various things change the order or contents of the files list, so the selected locs list needs to reflect that */
  int i, newly_selected = 0;
  for (i = 0; i < len; i++)
    {
      int j;
      for (j = 0; j <= vdat->end; j++)
	if ((vdat->full_names[j]) &&
	    (strcmp(vdat->full_names[j], saved_selected_files[i]) == 0))
	  {
	    vf_row *old_r, *new_r;
	    /* fprintf(stderr,"old %d at %d -> %d at %d\n", vdat->selected_files[i], i, j, newly_selected); */
	    old_r = vdat->file_list_entries[vdat->selected_files[i]];
	    vdat->selected_files[newly_selected++] = j;
	    new_r = vdat->file_list_entries[j];
	    if (new_r != old_r)
	      {
		vf_highlight_row(new_r->nm, new_r->rw);
		vf_unhighlight_row(old_r->nm, old_r->rw);
	      }
	    break;
	  }
    }
  vdat->currently_selected_files = newly_selected;
}


static int view_files_find_row(view_files_info *vdat, const char *name)
{
  int i;
  if (vdat->names)
    for (i = 0; i <= vdat->end; i++)
      if ((vdat->names[i]) && 
	  (strcmp(vdat->names[i], name) == 0))
  	return(i);
  if (vdat->full_names)
    for (i = 0; i <= vdat->end; i++)
      if ((vdat->full_names[i]) && 
	  (strcmp(vdat->full_names[i], name) == 0))
	return(i);
  return(-1);
}


void view_files_select(vf_row *r, bool add_to_selected)
{
  view_files_info *vdat = (view_files_info *)(r->vdat);
  int i, curloc = -1;

  for (i = 0; i < vdat->currently_selected_files; i++)
    if (vdat->selected_files[i] == r->pos)
      {
	curloc = r->pos;
	break;
      }
  if (curloc == -1)
    {
      /* file not currently selected */
      if (!add_to_selected)         /* not shift click, so remove all currently selected files first */
	view_files_clear_selected_files(vdat);
      view_files_add_selected_file(vdat, r);
      view_files_run_select_hook(vdat->dialog, vdat->full_names[r->pos]);
    }
  else
    {
      /* file already selected, so remove from selected files list */
      view_files_unselect_file(vdat, r);
    }

  if ((vdat->currently_selected_files == 0) ||
      ((vdat->currently_selected_files == 1) &&
       (!(plausible_sound_file_p(vdat->full_names[vdat->selected_files[0]])))))
    vf_unpost_info(vdat);
  else
    {
      if (vdat->currently_selected_files == 1)
	vf_post_info(vdat, vdat->selected_files[0]);
      else vf_post_selected_files_list(vdat);
    }
  vf_mix_insert_buttons_set_sensitive(vdat, 
				      ((vdat->currently_selected_files > 0) &&
				       (any_selected_sound())));
}


bool view_files_play(view_files_info *vdat, int pos, bool play)
{
  static snd_info *play_sp;
  if (play)
    {
      if (play_sp)
	{
	  if (play_sp->playing) return(true); /* can't play two of these at once */
	  if ((vdat->names[pos] == NULL) || 
	      (strcmp(play_sp->short_filename, vdat->names[pos]) != 0))
	    {
	      completely_free_snd_info(play_sp);
	      play_sp = NULL;
	    }
	}
      if ((!play_sp) && 
	  (vdat->full_names[pos]))
	play_sp = make_sound_readable(vdat->full_names[pos], false);
      if (play_sp)
	{
	  play_sp->short_filename = vdat->names[pos];
	  play_sp->filename = NULL;
	  /* pass view files dialog settings to play */
	  play_sp->speed_control = vdat->speed;
	  play_sp->amp_control = vdat->amp;
	  play_sound(play_sp, 0, NO_END_SPECIFIED);
	}
      else return(true); /* can't find or setup file */
    }
  else
    { /* play toggled off */
      if ((play_sp) && (play_sp->playing)) 
	{
	  stop_playing_sound(play_sp, PLAY_BUTTON_UNSET);
	  vdat->current_play_button = NULL_WIDGET;
	}
    }
  return(false);
}


void view_files_unplay(void)
{
  int k;
  for (k = 0; k < view_files_info_size; k++)
    if ((view_files_infos[k]) &&
	(view_files_infos[k]->dialog) &&
	(widget_is_active(view_files_infos[k]->dialog)))
      {
	view_files_info *vdat;
	vdat = view_files_infos[k];
	if ((vdat->current_play_button) &&
#if USE_MOTIF
	    (XmToggleButtonGetState(vdat->current_play_button) != XmUNSET)
#else
  #if USE_GTK
	    ((bool)gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(vdat->current_play_button)))
  #else
            (0)
  #endif
#endif
	    )
	  {
	    set_toggle_button(vdat->current_play_button, false, true, (void *)vdat);
	    vdat->current_play_button = NULL_WIDGET;
	  }
      }
}


void view_files_reflect_sort_items(void)
{
  int i;
#if USE_MOTIF || USE_GTK
  view_files_info *vdat;
  int j = 0, k;
#endif
  if (view_files_info_size == 0) return;
  for (i = 0; i < ss->file_sorters_size; i++)
    {
      XEN ref;
      ref = XEN_VECTOR_REF(ss->file_sorters, i);
      if (XEN_PAIR_P(ref))
	{
#if USE_MOTIF
	  XmString s1;
	  s1 = XmStringCreateLocalized((char *)XEN_TO_C_STRING(XEN_CAR(ref)));
	  for (k = 0; k < view_files_info_size; k++)
	    if ((view_files_infos[k]) &&
		(view_files_infos[k]->dialog))
	      {
		vdat = view_files_infos[k];
		if (j >= vdat->sort_items_size)
		  {
		    int n = 0, k, old_size;
		    Arg args[20];
		    old_size = vdat->sort_items_size;
		    XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
		    vdat->sort_items_size += 4;
		    vdat->sort_items = (Widget *)realloc(vdat->sort_items, vdat->sort_items_size * sizeof(Widget));
		    for (k = old_size; k < vdat->sort_items_size; k++)
		      vdat->sort_items[k] = XtCreateWidget("unused", xmPushButtonWidgetClass, vdat->smenu, args, n);
		  }
		XtVaSetValues(vdat->sort_items[j], 
			      XmNlabelString, s1,
			      XmNuserData, i + SORT_XEN, /* this is an index into the file_sorters list, not the widget list */
			      NULL);
		XtManageChild(vdat->sort_items[j]);
	      }
	  j++;
	  XmStringFree(s1);
#else
  #if USE_GTK
	  for (k = 0; k < view_files_info_size; k++)
	    if ((view_files_infos[k]) &&
		(view_files_infos[k]->dialog))
	      {
		vdat = view_files_infos[k];
		if (j >= vdat->sort_items_size)
		  {
		    int k, old_size;
		    old_size = vdat->sort_items_size;
		    vdat->sort_items_size += 4;
		    vdat->sort_items = (GtkWidget **)realloc(vdat->sort_items, vdat->sort_items_size * sizeof(GtkWidget *));
		    for (k = old_size; k < vdat->sort_items_size; k++)
		      {
			vdat->sort_items[k] = gtk_menu_item_new_with_label("unused");
			gtk_menu_shell_append(GTK_MENU_SHELL(vdat->smenu), vdat->sort_items[i]);
		      }
		  }
		set_menu_label(vdat->sort_items[j], 
			       XEN_TO_C_STRING(XEN_CAR(ref)));
		{
		  pointer_or_int_t data;
		  data = i + SORT_XEN;
		  set_user_data(G_OBJECT(vdat->sort_items[j]), 
				(gpointer)data); /* this is an index into the file_sorters list, not the widget list */
		}
		gtk_widget_show(vdat->sort_items[j]);
	      }
	  j++;
  #endif
#endif
	}
    }
}


/* (add-file-sorter "duration" 
		(lambda (lst)
		  (sort lst 
			(lambda (a b)
			  (> (mus-sound-duration a) (mus-sound-duration b))))))

 */


static void vf_add_file(view_files_info *vdat, const char *filename, const char *fullname)
{
  vdat->end++;
  if (vdat->end >= vdat->size)
    {
      int new_size;
      new_size = vdat->size + 32;
      if (vdat->size == 0)
	{
	  vdat->names = (char **)calloc(new_size, sizeof(char *));
	  vdat->full_names = (char **)calloc(new_size, sizeof(char *));
	}
      else
	{
	  int i;
	  vdat->names = (char **)realloc(vdat->names, new_size * sizeof(char *));
	  vdat->full_names = (char **)realloc(vdat->full_names, new_size * sizeof(char *));
	  for (i = vdat->size; i < new_size; i++) 
	    {
	      vdat->names[i] = NULL; 
	      vdat->full_names[i] = NULL; 
	    }
	}
      if (vdat->file_list_entries == NULL)
	vdat->file_list_entries = (vf_row **)calloc(new_size, sizeof(vf_row *));
      else 
	{
	  int i;
	  vdat->file_list_entries = (vf_row **)realloc(vdat->file_list_entries, new_size * sizeof(vf_row *));
	  for (i = vdat->size; i < new_size; i++) vdat->file_list_entries[i] = NULL;
	}
      vdat->size = new_size;
    }
  vdat->names[vdat->end] = mus_strdup(filename);
  vdat->full_names[vdat->end] = mus_strdup(fullname);
}


void add_file_to_view_files_list(view_files_info *vdat, const char *filename, const char *fullname)
{
  int row;

  row = view_files_find_row(vdat, filename);
  if (row != -1)
    {
      if ((vdat->dialog) &&
	  (widget_is_active(vdat->dialog)) &&
	  (vdat->file_list_entries[row]))
	{
	  ensure_scrolled_window_row_visible(vdat->file_list, row, vdat->end + 1);
	  vf_flash_row(vdat->file_list_entries[row]);
	}
      return;
    }

  errno = 0;
  if (!(mus_file_probe(fullname)))
    {
      char *msg;
      if ((vdat->dialog) &&
	  (widget_is_active(vdat->dialog)))
	{
	  if (errno != 0)
	    msg = mus_format("%s: %s", filename, strerror(errno));
	  else msg = mus_format("%s does not exist", filename);
	  vf_post_add_error(msg, vdat);
	  free(msg);
	}
      return;
    }

  vf_add_file(vdat, filename, fullname);
}


void vf_add_file_if_absent(view_files_info *vdat, const char *filename)
{
  int row;
  row = view_files_find_row(vdat, filename);
  if (row == -1)
    {
      vf_add_file(vdat, filename_without_directory(filename), filename);
      vdat->need_update = true;
    }
}


void vf_remove_file_if_present(view_files_info *vdat, const char *filename)
{
  int i, row;
  row = view_files_find_row(vdat, filename);
  if (row != -1)
    {
      vdat->need_update = true;

      /* if deleted file is selected, unselect */
      for (i = 0; i < vdat->currently_selected_files; i++)
	if (vdat->selected_files[i] == row)
	  {
	    view_files_unselect_file(vdat, vdat->file_list_entries[row]);
	    break;
	  }
    }
}


static void dialog_set_title(widget_t dialog, const char *titlestr)
{
#if USE_MOTIF
  XmString title;
  title = XmStringCreateLocalized((char *)titlestr);
  XtVaSetValues(dialog, XmNdialogTitle, title, NULL);
  XmStringFree(title);
#endif
#if USE_GTK
  gtk_window_set_title(GTK_WINDOW(dialog), titlestr);
#endif
}


char *dialog_get_title(widget_t dialog)
{
#if USE_MOTIF
  char *temp, *titlestr = NULL;
  XmString title;
  XtVaGetValues(dialog, XmNdialogTitle, &title, NULL);
  temp = (char *)XmStringUnparse(title, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
  if (temp)
    {
      titlestr = mus_strdup(temp);
      XtFree(temp);
    }
  return(titlestr);
#endif
#if USE_GTK
  return(mus_strdup(gtk_window_get_title(GTK_WINDOW(dialog))));
#endif
#if USE_NO_GUI
  return(NULL); /* make the compiler happy */
#endif
}


/* what about temps coming and going -- should we just add a need-update switch for later remanage? */
/*   remanagement only through make_view_files_dialog -- this file */
/*   perhaps ss->making|deleting_temp_file -> ignore this fam event? */

void add_directory_to_view_files_list(view_files_info *vdat, const char *dirname)
{
  /* I think all directory additions come through here */
  dir_info *sound_files = NULL;

  if ((dirname) && (dirname[strlen(dirname) - 1] != '/'))
    {
      char *add_slash;
      add_slash = mus_format("%s/", dirname);
      add_directory_to_view_files_list(vdat, add_slash);
      free(add_slash);
    }
  else
    {
#if (!USE_NO_GUI)
      view_files_monitor_directory(vdat, dirname);
#endif
      sound_files = find_sound_files_in_dir(dirname);
      if ((sound_files) && 
	  (sound_files->len > 0))
	{
	  int i, dirs = 0, len = 16;
	  for (i = 0; i < sound_files->len; i++) 
	    add_file_to_view_files_list(vdat, sound_files->files[i]->filename, sound_files->files[i]->full_filename);
	  sound_files = free_dir_info(sound_files);

	  /* fixup title */
	  for (i = 0; i < vdat->dirs_size; i++)
	    if (vdat->dir_names[i])
	      {
		dirs++;
		len += mus_strlen(just_filename(vdat->dir_names[i]));
	      }
	  if ((dirs < 4) &&
	      (len < 512))
	    {
	      int cur_dir = 0;
	      char *titlestr = NULL, *dirstr;
	      titlestr = (char *)calloc(len + dirs * 8, sizeof(char));
	      strcat(titlestr, "Files: ");
	      for (i = 0; i < vdat->dirs_size; i++)
		if (vdat->dir_names[i])
		  {
		    dirstr = just_filename(vdat->dir_names[i]);
		    strcat(titlestr, dirstr);
		    free(dirstr);
		    cur_dir++;
		    if (cur_dir < dirs)
		      strcat(titlestr, ", ");
		  }
	      dialog_set_title(vdat->dialog, titlestr);
	      free(titlestr);
	    }
	}
    }
}


static void view_files_sort_list(view_files_info *vdat)
{
  if (vdat->end >= 0)
    {
      sort_info **data;
      int i, len;

      len = vdat->end + 1;
      data = (sort_info **)calloc(len, sizeof(sort_info *));

      for (i = 0; i < len; i++)
	{
	  data[i] = (sort_info *)calloc(1, sizeof(sort_info));
	  data[i]->filename = vdat->names[i];
	  data[i]->full_filename = vdat->full_names[i];
	}

      snd_sort(vdat->sorter, data, len);

      for (i = 0; i < len; i++)
	{
	  vdat->names[i] = data[i]->filename;
	  vdat->full_names[i] = data[i]->full_filename;
	  free(data[i]);
	}
      free(data);
    }
}


void view_files_display_list(view_files_info *vdat)
{
  int i;
  widget_t last_row = NULL_WIDGET; /* ignored in gtk version */
  vf_row *r;

  if (!vdat) return;
  if (!(vdat->dialog)) return;

  if (vdat->end >= 0)
    {
      int i, old_len;
      char **old_names = NULL;

      old_len = vdat->currently_selected_files;
      if (old_len > 0)
	old_names = vf_selected_files(vdat);

      view_files_sort_list(vdat);
      for (i = 0; i <= vdat->end; i++)
	{
	  r = vdat->file_list_entries[i];
	  if (!r)
	    {
	      r = view_files_make_row(vdat, last_row);
	      vdat->file_list_entries[i] = r;
	      r->pos = i;
	    }
	  set_button_label(r->nm, vdat->names[r->pos]);
#if WITH_AUDIO
	  set_toggle_button(r->pl, false, false, (void *)vdat);
#endif
	  if (!(widget_is_active(r->rw))) activate_widget(r->rw);
	  last_row = r->rw;
	}

      if (old_names)
	{
	  vf_fixup_selected_files(vdat, old_names, old_len);
	  for (i = 0; i < old_len; i++) free(old_names[i]);
	  free(old_names);
	}
    }

  for (i = vdat->end + 1; i < vdat->size; i++)
    {
      r = vdat->file_list_entries[i];
      if (r)
	{
	  if (widget_is_active(r->rw)) 
	    deactivate_widget(r->rw);
	}
    }

  if (!(widget_is_active(vdat->file_list))) 
    activate_widget(vdat->file_list);
}


static void view_files_clear_list(view_files_info *vdat)
{
  int i;
#if (!USE_NO_GUI)
  view_files_unmonitor_directories(vdat);
#endif
  if (vdat->names)
    {
      for (i = 0; i < vdat->size; i++)
	if (vdat->names[i]) 
	  {
	    free(vdat->names[i]); 
	    vdat->names[i] = NULL;
	    free(vdat->full_names[i]); 
	    vdat->full_names[i] = NULL;
	  }
      vdat->end = -1;
      vdat->currently_selected_files = 0;
    }
}


void view_files_update_list(view_files_info *vdat)
{
  /* here we need the file's full name */
  int i, old_len;
  char **old_names = NULL;

  old_len = vdat->currently_selected_files;
  if (old_len > 0) 
    old_names = vf_selected_files(vdat);

  if (vdat->names)
    {
      int i, j;
      for (i = 0; i <= vdat->end; i++)
	if (vdat->names[i]) 
	  {
	    if (!(mus_file_probe(vdat->full_names[i])))
	      {
		free(vdat->names[i]); 
		vdat->names[i] = NULL;
		free(vdat->full_names[i]); 
		vdat->full_names[i] = NULL;
	      }
	  }

      for (i = 0, j = 0; i <= vdat->end; i++)
	if (vdat->names[i])
	  {
	    if (i != j) 
	      {
		vdat->names[j] = vdat->names[i]; 
		vdat->names[i] = NULL;
		vdat->full_names[j] = vdat->full_names[i];
		vdat->full_names[i] = NULL;
	      }
	    j++;
	  }
      vdat->end = j - 1;
    }

  if (old_names)
    {
      vf_fixup_selected_files(vdat, old_names, old_len);
      for (i = 0; i < old_len; i++) free(old_names[i]);
      free(old_names);
    }
}


void vf_clear_error(view_files_info *vdat)
{
  if (vdat->currently_selected_files == 1)
    vf_post_info(vdat, vdat->selected_files[0]);
  else
    {
      if (vdat->currently_selected_files == 0)
	vf_unpost_info(vdat);
      else vf_post_selected_files_list(vdat);
    }
  vdat->error_p = false;
}


int vf_mix(view_files_info *vdat)
{
  int len, id_or_error = 0;
  snd_info *sp;

  sp = any_selected_sound();
  len = vdat->currently_selected_files;

  if ((len == 1) &&
      (snd_feq(vdat->amp, 1.0)) &&
      (snd_feq(vdat->speed, 1.0)) &&
      (default_env_p(vdat->amp_env)))
    id_or_error = mix_complete_file(sp, vdat->beg, 
				    vdat->full_names[vdat->selected_files[0]], 
				    with_mix_tags(ss), DONT_DELETE_ME, MIX_SETS_SYNC_LOCALLY, NULL);
  else
    {
      int i;
      bool err = false;
      char *tempfile;
      char **selected_files;

      selected_files = vf_selected_files(vdat);
      tempfile = scale_and_src(selected_files, len, sp->nchans, vdat->amp, vdat->speed, vdat->amp_env, &err);

      if (err)
	{
	  vf_post_error(tempfile, vdat);
	  id_or_error = MIX_FILE_NO_TEMP_FILE;
	}
      else
	{ 
	  if (sp->nchans > 1)
	    remember_temp(tempfile, sp->nchans);
	  id_or_error = mix_complete_file(sp, vdat->beg, tempfile,
					  with_mix_tags(ss), 
					  (sp->nchans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
					  MIX_SETS_SYNC_LOCALLY, NULL);
	}
      free(tempfile);
      for (i = 0; i < len; i++)
	free(selected_files[i]);
      free(selected_files);
    }
  return(id_or_error);
}


void view_files_mix_selected_files(widget_t w, view_files_info *vdat)
{
  vdat->error_p = false;
  redirect_snd_error_to(redirect_vf_post_location_error, (void *)vdat);
  vdat->beg = vf_location(vdat);
  redirect_snd_error_to(NULL, NULL);

  if (!(vdat->error_p))
    {
      int id_or_error = 0;

      redirect_snd_error_to(redirect_vf_post_error, (void *)vdat);
      ss->requestor_dialog = w;
      ss->open_requestor_data = (void *)vdat;
      ss->open_requestor = FROM_VIEW_FILES_MIX_DIALOG;
      id_or_error = vf_mix(vdat);

      /* "id_or_error" here is either one of the mix id's or an error indication such as MIX_FILE_NO_MIX */
      /*    the possible error conditions have been checked already, or go through snd_error */

      redirect_snd_error_to(NULL, NULL);
      if (id_or_error >= 0)
	{
	  char *msg;
	  if (vdat->currently_selected_files == 1)
	    msg = mus_format("%s mixed in at %lld", vdat->names[vdat->selected_files[0]], vdat->beg);
	  else msg = mus_format("selected files mixed in at %lld", vdat->beg);
	  vf_post_error(msg, vdat);
	  vdat->error_p = false;
	  free(msg);
	}
    }
}


bool vf_insert(view_files_info *vdat)
{
  int len;
  bool ok = false;
  snd_info *sp;
  sp = any_selected_sound();

  len = vdat->currently_selected_files;
  if ((len == 1) &&
      (snd_feq(vdat->amp, 1.0)) &&
      (snd_feq(vdat->speed, 1.0)) &&
      (default_env_p(vdat->amp_env)))
    ok = insert_complete_file(sp, 
			      vdat->full_names[vdat->selected_files[0]], 
			      vdat->beg,
			      DONT_DELETE_ME);
  else
    {
      int i;
      bool err = false;
      char *tempfile;
      char **selected_files;

      selected_files = vf_selected_files(vdat);
      tempfile = scale_and_src(selected_files, len, sp->nchans, vdat->amp, vdat->speed, vdat->amp_env, &err);

      if (err)
	{
	  vf_post_error(tempfile, vdat);
	  ok = false;
	}
      else
	{
	  vf_clear_error(vdat);
	  if (sp->nchans > 1)
	    remember_temp(tempfile, sp->nchans);
	  ok = insert_complete_file(sp, 
				    tempfile,
				    vdat->beg,
				    (sp->nchans > 1) ? MULTICHANNEL_DELETION : DELETE_ME);
	}
      free(tempfile);
      for (i = 0; i < len; i++)
	free(selected_files[i]);
      free(selected_files);
    }
  return(ok);
}


void view_files_insert_selected_files(widget_t w, view_files_info *vdat)
{
  vdat->error_p = false;
  redirect_snd_error_to(redirect_vf_post_location_error, (void *)vdat);
  vdat->beg = vf_location(vdat);
  redirect_snd_error_to(NULL, NULL);

  if (!(vdat->error_p))
    {
      bool ok = false;

      redirect_snd_error_to(redirect_vf_post_error, (void *)vdat);
      redirect_snd_warning_to(redirect_vf_post_error, (void *)vdat);
      ss->requestor_dialog = w;
      ss->open_requestor = FROM_VIEW_FILES_INSERT_DIALOG;
      ss->open_requestor_data = (void *)vdat;
      ok = vf_insert(vdat);
      redirect_snd_error_to(NULL, NULL);
      redirect_snd_warning_to(NULL, NULL);

      if (ok)
	{
	  char *msg;
	  if (vdat->currently_selected_files == 1)
	    msg = mus_format("%s inserted at %lld", vdat->names[vdat->selected_files[0]], vdat->beg);
	  else msg = mus_format("selected files inserted at %lld", vdat->beg);
	  vf_post_error(msg, vdat);
	  vdat->error_p = false;
	  free(msg);
	}
      /* else we've already posted whatever went wrong (make_file_info etc) */
    }
}


static mus_float_t view_files_amp(widget_t dialog)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    return(vdat->amp);
  return(0.0);
}


static mus_float_t view_files_set_amp(widget_t dialog, mus_float_t new_amp)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    vf_set_amp(vdat, new_amp);
  return(new_amp);
}


static mus_float_t view_files_speed(widget_t dialog)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    return(vdat->speed);
  return(1.0);
}


static mus_float_t view_files_set_speed(widget_t dialog, mus_float_t new_speed)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    vf_set_speed(vdat, new_speed);
  return(new_speed);
}


static speed_style_t view_files_speed_style(widget_t dialog)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    return(vdat->speed_style);
  return(SPEED_CONTROL_AS_FLOAT);
}


static speed_style_t view_files_set_speed_style(widget_t dialog, speed_style_t speed_style)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    {
      vdat->speed_style = speed_style;
      vf_set_speed(vdat, vdat->speed); /* update label etc */
    }
  return(speed_style);
}


static env *view_files_amp_env(widget_t dialog)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    return(vdat->amp_env);
  return(NULL);
}


static void view_files_set_amp_env(widget_t dialog, env *new_e)
{
  vf_set_amp_env(vf_dialog_to_info(dialog), new_e);
}


static int view_files_local_sort(widget_t dialog)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    return(vdat->sorter);
  return(-1);
}


static int view_files_set_local_sort(widget_t dialog, int sort_choice)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    {
      vdat->sorter = sort_choice;
      view_files_display_list(vdat);
      vf_reflect_sort_choice_in_menu(vdat);
    }
  return(sort_choice);
}


static view_files_info *view_files_find_dialog(widget_t dialog)
{
  int i;
  for (i = 0; i < view_files_info_size; i++)
    if ((view_files_infos[i]) &&
	(view_files_infos[i]->dialog == dialog))
      return(view_files_infos[i]);
  return(NULL);
}


widget_t make_view_files_dialog(bool managed, bool make_new)
{
  int i;
  view_files_info *vdat = NULL;

  if (make_new)
    return(make_view_files_dialog_1(new_view_files_dialog(), managed));

  for (i = 0; i < view_files_info_size; i++)
    if ((view_files_infos[i]) &&
	(view_files_infos[i]->dialog))
      {
	vdat = view_files_infos[i];
	if (widget_is_active(vdat->dialog))
	  break;
      }

  if (vdat)
    return(make_view_files_dialog_1(vdat, managed));
  return(make_view_files_dialog_1(new_view_files_dialog(), managed));
}


void save_view_files_dialogs(FILE *fd) 
{
#if HAVE_EXTENSION_LANGUAGE
  int i;
  view_files_info *vdat;
  for (i = 0; i < view_files_info_size; i++)
    if ((view_files_infos[i]) &&
	(view_files_infos[i]->dialog) &&
	(widget_is_active(view_files_infos[i]->dialog)))
      {
	int k;
	vdat = view_files_infos[i];

#if HAVE_SCHEME
	fprintf(fd, "(let ((vf (" S_view_files_dialog " #t #t)))\n");

	if (vdat->full_names)
	  {
	    fprintf(fd, "  (set! (" S_view_files_files " vf) (list");
	    for (k = 0; k <= vdat->end; k++)
	      fprintf(fd, " \"%s\"", vdat->full_names[k]);
	    fprintf(fd, "))\n");
	    if (vdat->currently_selected_files > 0)
	      {
		fprintf(fd, "  (set! (" S_view_files_selected_files " vf) (list");
		for (k = 0; k < vdat->currently_selected_files; k++)
		  fprintf(fd, " \"%s\"", vdat->full_names[vdat->selected_files[k]]);
		fprintf(fd, "))\n");
	      }
	  }
	if (!(snd_feq(vdat->amp, 1.0)))
	  fprintf(fd, "  (set! (" S_view_files_amp " vf) %.3f)\n", vdat->amp);

	if (!(snd_feq(vdat->speed, 1.0)))
	  fprintf(fd, "  (set! (" S_view_files_speed " vf) %.3f)\n", vdat->speed);

	if (!(default_env_p(vdat->amp_env)))
	  fprintf(fd, "  (set! (" S_view_files_amp_env " vf) %s)\n", env_to_string(vdat->amp_env));

	/* assume file-sorters are set up already */
	fprintf(fd, "  (set! (" S_view_files_sort " vf) %d)\n", vdat->sorter);	    
	fprintf(fd, ")\n");
#endif

#if HAVE_RUBY
	fprintf(fd, "vf = view_files_dialog(true, true)\n");

	if (vdat->full_names)
	  {
	    fprintf(fd, "  set_view_files_files(vf, [");
	    for (k = 0; k < vdat->end; k++)
	      fprintf(fd, "\"%s\", ", vdat->full_names[k]);
	    fprintf(fd, "\"%s\"])\n", vdat->full_names[vdat->end]);
	    if (vdat->currently_selected_files > 0)
	      {
		fprintf(fd, "  set_view_files_selected_files(vf, [");
		for (k = 0; k < vdat->currently_selected_files - 1; k++)
		  fprintf(fd, "\"%s\", ", vdat->full_names[vdat->selected_files[k]]);
		fprintf(fd, "\"%s\"])\n", vdat->full_names[vdat->selected_files[vdat->currently_selected_files]]);
	      }
	  }
	if (!(snd_feq(vdat->amp, 1.0)))
	  fprintf(fd, "  set_view_files_amp(vf, %.3f)\n", vdat->amp);

	if (!(snd_feq(vdat->speed, 1.0)))
	  fprintf(fd, "  set_view_files_speed(vf, %.3f)\n", vdat->speed);

	if (!(default_env_p(vdat->amp_env)))
	  fprintf(fd, "  set_view_files_amp_env(vf, %s)\n", env_to_string(vdat->amp_env));

	/* assume file-sorters are set up already */
	fprintf(fd, "  set_view_files_sort(vf, %d)\n", vdat->sorter);	    
	fprintf(fd, "\n");
#endif

#if HAVE_FORTH
	fprintf(fd, "#t #t view-files-dialog value vf\n");

	if (vdat->full_names)
	  {
	    fprintf(fd, "  vf '(");
	    for (k = 0; k <= vdat->end; k++)
	      fprintf(fd, " \"%s\"", vdat->full_names[k]);
	    fprintf(fd, " ) set-view-files-files drop\n");
	    if (vdat->currently_selected_files > 0)
	      {
		fprintf(fd, "  vf '(");
		for (k = 0; k <= vdat->currently_selected_files; k++)
		  fprintf(fd, " \"%s\"", vdat->full_names[vdat->selected_files[k]]);
		fprintf(fd, " ) set-view-files-selected-files drop\n");
	      }
	  }
	if (!(snd_feq(vdat->amp, 1.0)))
	  fprintf(fd, "  vf %.3f set-view-files-amp drop\n", vdat->amp);

	if (!(snd_feq(vdat->speed, 1.0)))
	  fprintf(fd, "  vf %.3f set-view-files-speed drop\n", vdat->speed);

	if (!(default_env_p(vdat->amp_env)))
	  fprintf(fd, "  vf %s set-view-files-amp-env drop\n", env_to_string(vdat->amp_env));

	/* assume file-sorters are set up already */
	fprintf(fd, "  vf %d set-view-files-sort drop\n\n", vdat->sorter);
#endif
      }
#endif
}


void view_files_add_directory(widget_t dialog, const char *dirname) 
{
  view_files_info *vdat = NULL;
  char *full_filename;

  if (dialog)
    vdat = view_files_find_dialog(dialog);
  else 
    {
      if (view_files_info_size > 0)
	vdat = view_files_infos[0];
      else 
	{
	  vdat = new_view_files_dialog();
	  make_view_files_dialog_1(vdat, false);
	}
    }

  if (vdat)
    {
      full_filename = mus_expand_filename((const char *)dirname);
      if (!(mus_file_probe(full_filename)))
	{
	  char *msg;
	  if ((vdat->dialog) &&
	      (widget_is_active(vdat->dialog)))
	    {
	      if (errno != 0)
		msg = mus_format("%s: %s", full_filename, strerror(errno));
	      else msg = mus_format("%s does not exist", full_filename);
	      vf_post_add_error(msg, vdat);
	      free(msg);
	    }
	}
      else
	{
	  add_directory_to_view_files_list(vdat, full_filename);
	}
      free(full_filename);
    }
}


static void view_files_add_file(widget_t dialog, const char *filename)
{
  view_files_info *vdat = NULL;
  char *full_filename;

  if (dialog)
    vdat = view_files_find_dialog(dialog);
  else 
    {
      if (view_files_info_size > 0)
	vdat = view_files_infos[0];
      else 
	{
	  vdat = new_view_files_dialog();
	  make_view_files_dialog_1(vdat, false);
	}
    }

  if (vdat)
    {
      full_filename = mus_expand_filename((const char *)filename);
      add_file_to_view_files_list(vdat, filename, full_filename);
      free(full_filename);
    }
}


void view_files_open_selected_files(view_files_info *vdat)
{
  snd_info *sp = NULL;
  ss->open_requestor = FROM_VIEW_FILES;

  if (vdat->currently_selected_files > 0)
    {
      int i;
      for (i = 0; i < vdat->currently_selected_files; i++)
	sp = snd_open_file(vdat->full_names[vdat->selected_files[i]], FILE_READ_WRITE);
      if (sp) select_channel(sp, 0); 
    }
}


char *view_files_find_any_directory(void)
{
  /* find any active directory in any vf dialog */
  if (view_files_info_size > 0)
    {
      int j;
      for (j = 0; j < view_files_info_size; j++)
	{
	  view_files_info *vdat;
	  vdat = view_files_infos[j];
	  if ((vdat) && 
	      (vdat->dir_names))
	    {
	      int i;
	      for (i = 0; i < vdat->dirs_size; i++)
		if (vdat->dir_names[i])
		  return(vdat->dir_names[i]);
	    }
	}
    }
  return(NULL);
}


/* -------- info popup -------- */

static XEN info_popup_hook;

#if (!USE_NO_GUI)

static char *display_file_maxamps(const char *filename, int chans)
{
  char *ampstr = NULL;
  int i, len;
  mus_float_t *vals;
  mus_long_t *times;
  len = chans * 32;
  ampstr = (char *)calloc(len, sizeof(char));
  vals = (mus_float_t *)calloc(chans, sizeof(mus_float_t));
  times = (mus_long_t *)calloc(chans, sizeof(mus_long_t));
  mus_snprintf(ampstr, len, "maxamp%s: ", (chans > 1) ? "s" : "");
  mus_sound_maxamps(filename, chans, vals, times);
  for (i = 0; i < chans; i++)
    {
      ampstr = mus_strcat(ampstr, prettyf(vals[i], 3), &len);
      ampstr = mus_strcat(ampstr, " ", &len);
    }
  free(vals);
  free(times);
  return(ampstr);
}


static char *display_sound_maxamps(snd_info *sp)
{
  char *ampstr = NULL;
  int i, len;
  len = sp->nchans * 32;
  ampstr = (char *)calloc(len, sizeof(char));
  mus_snprintf(ampstr, len, "maxamp%s: ", (sp->nchans > 1) ? "s" : "");
  for (i = 0; i < sp->nchans; i++)
    {
      ampstr = mus_strcat(ampstr, prettyf(channel_maxamp(sp->chans[i], AT_CURRENT_EDIT_POSITION), 3), &len);
      ampstr = mus_strcat(ampstr, " ", &len);
    }
  return(ampstr);
}


void display_info(snd_info *sp)
{
  #define INFO_BUFFER_SIZE 1024
  if (sp)
    {
      file_info *hdr;
      int i;
      char *comment = NULL, *ampstr = NULL, *buffer = NULL;

      hdr = sp->hdr;
      buffer = (char *)calloc(INFO_BUFFER_SIZE, sizeof(char));

      mus_snprintf(buffer, INFO_BUFFER_SIZE, 
		   "srate: %d\nchans: %d\nlength: %.3f (%lld %s)\n%s\n",
		   SND_SRATE(sp),
		   sp->nchans,
		   (double)(CURRENT_SAMPLES(sp->chans[0])) / (double)SND_SRATE(sp),
		   CURRENT_SAMPLES(sp->chans[0]),
		   (sp->nchans == 1) ? "samples" : "frames",
		   ampstr = display_sound_maxamps(sp));
      post_it(sp->short_filename, buffer);
      if (ampstr) free(ampstr);

      /* run info-popup-hook, appending each string */
      if (XEN_HOOKED(info_popup_hook))
	{
	  XEN result;
#if HAVE_SCHEME
	  result = s7_call(s7, info_popup_hook, s7_cons(s7, C_INT_TO_XEN_SOUND(sp->index), XEN_EMPTY_LIST));
	  if (XEN_STRING_P(result))
	    post_it_append(XEN_TO_C_STRING(result));
#else
	  XEN procs;
	  procs = XEN_HOOK_PROCEDURES(info_popup_hook);
	  while (XEN_NOT_NULL_P(procs))
	    {
	      result = XEN_CALL_1(XEN_CAR(procs), C_INT_TO_XEN_SOUND(sp->index), S_info_popup_hook);
	      if (XEN_STRING_P(result))
		post_it_append(XEN_TO_C_STRING(result));
	      procs = XEN_CDR(procs);
	    }
#endif
	}

      mus_snprintf(buffer, INFO_BUFFER_SIZE, "\n----------------------------------------\n%s:", sp->filename);
      post_it_append(buffer);

      mus_snprintf(buffer, INFO_BUFFER_SIZE, "\n    type: %s\n    format: %s\n    written: %s\n",
		   mus_header_type_name(hdr->type),
		   mus_data_format_name(hdr->format),
		   snd_strftime(STRFTIME_FORMAT, sp->write_date));
      post_it_append(buffer);

      if (hdr->srate != SND_SRATE(sp))
	{
	  mus_snprintf(buffer, INFO_BUFFER_SIZE, "    original srate: %d\n", hdr->srate);
	  post_it_append(buffer);
	}

      if (hdr->chans != sp->nchans)
	{
	  mus_snprintf(buffer, INFO_BUFFER_SIZE, "    original chans: %d\n", hdr->chans);
	  post_it_append(buffer);
	}

      comment = hdr->comment;
      while ((comment) && (*comment) && 
	     (((*comment) == '\n') || 
	      ((*comment) == '\t') || 
	      ((*comment) == ' ') || 
	      ((*comment) == '\xd')))
	comment++;
      if ((comment) && (*comment))
	{
	  mus_snprintf(buffer, INFO_BUFFER_SIZE, "    comment: \"%s\"\n", comment);
	  post_it_append(buffer);
	}

      if (mus_sound_maxamp_exists(sp->filename))
	{
	  bool edits = false;
	  for (i = 0; i < sp->nchans; i++)
	    if (sp->chans[i]->edit_ctr > 0)
	      {
		edits = true;
		break;
	      }

	  if (edits)
	    {
	      ampstr = display_file_maxamps(sp->filename, sp->nchans);
	      if (ampstr)
		{
		  mus_snprintf(buffer, INFO_BUFFER_SIZE, "    original %s\n", ampstr);
		  post_it_append(buffer);
		  free(ampstr);
		}
	    }
	}
      free(buffer);
    }
}
#endif



/* -------- extlang connections -------- */

/* -------- view-files variables -------- */

static XEN g_view_files_dialog(XEN managed, XEN make_new)
{
  #define H_view_files_dialog "(" S_view_files_dialog " :optional managed create-new-dialog): start the View Files dialog"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ARG_1, S_view_files_dialog, "a boolean");
  return(XEN_WRAP_WIDGET(make_view_files_dialog(XEN_TO_C_BOOLEAN(managed), XEN_TRUE_P(make_new))));
}


static XEN g_add_directory_to_view_files_list(XEN directory, XEN dialog) 
{
  #define H_add_directory_to_view_files_list "(" S_add_directory_to_view_files_list " dir :optional w): adds any sound files in 'dir' to the View:Files dialog"
  
  XEN_ASSERT_TYPE(XEN_STRING_P(directory), directory, XEN_ARG_1, S_add_directory_to_view_files_list, "a string");
  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog) || XEN_NOT_BOUND_P(dialog), dialog, XEN_ARG_2, S_add_directory_to_view_files_list, "a view-files dialog widget"); 

  if (XEN_NOT_BOUND_P(dialog))
    view_files_add_directory(NULL_WIDGET, XEN_TO_C_STRING(directory));
  else view_files_add_directory((widget_t)(XEN_UNWRAP_WIDGET(dialog)), XEN_TO_C_STRING(directory));
  return(directory);
}


static XEN g_add_file_to_view_files_list(XEN file, XEN dialog) 
{
  #define H_add_file_to_view_files_list "(" S_add_file_to_view_files_list " file :optional w): adds file to the View:Files dialog's list"
  char *name = NULL;

  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, S_add_file_to_view_files_list, "a string");
  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog) || XEN_NOT_BOUND_P(dialog), dialog, XEN_ARG_2, S_add_file_to_view_files_list, "a view-files dialog widget"); 

  name = mus_expand_filename(XEN_TO_C_STRING(file));
  if (mus_file_probe(name))
    {
      if (XEN_NOT_BOUND_P(dialog))
	view_files_add_file(NULL_WIDGET, name);
      else view_files_add_file((widget_t)(XEN_UNWRAP_WIDGET(dialog)), name);
    }
  if (name) free(name);
  return(file);
}


static XEN g_view_files_sort(XEN dialog) 
{
  #define H_view_files_sort "(" S_view_files_sort " :optional dialog): sort choice in View:files dialog."
  if (XEN_BOUND_P(dialog))
    {
      XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ONLY_ARG, S_view_files_sort, "a view-files dialog widget"); 
      return(C_TO_XEN_INT(view_files_local_sort((widget_t)(XEN_UNWRAP_WIDGET(dialog)))));
    }
  return(C_TO_XEN_INT(view_files_sort(ss)));
}


static XEN g_set_view_files_sort(XEN dialog, XEN val) 
{
  int choice;
  XEN sort_choice;

  if (XEN_BOUND_P(val)) sort_choice = val; else sort_choice = dialog;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(sort_choice), sort_choice, XEN_ARG_1, S_setB S_view_files_sort, "an integer"); 

  choice = XEN_TO_C_INT(sort_choice);
  if ((choice < 0) ||
      (choice >= (ss->file_sorters_size + SORT_XEN)))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_view_files_sort, 2, sort_choice, "must be a valid file-sorter index");

  if (XEN_BOUND_P(val))
    {
      widget_t w;
      XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ARG_1, S_setB S_view_files_sort, "a view-files dialog widget"); 
      w = (widget_t)(XEN_UNWRAP_WIDGET(dialog));
      view_files_set_local_sort(w, choice);
      return(C_TO_XEN_INT((int)view_files_sort(ss)));
    }
  /* else set global (default) sort choice */
  set_view_files_sort(choice);
  return(C_TO_XEN_INT((int)view_files_sort(ss)));
}


static XEN g_view_files_amp(XEN dialog)
{
  #define H_view_files_amp "(" S_view_files_amp " dialog): amp setting in the given View:Files dialog"
  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ONLY_ARG, S_view_files_amp, "a view-files dialog widget"); 
  return(C_TO_XEN_DOUBLE(view_files_amp((widget_t)(XEN_UNWRAP_WIDGET(dialog)))));
}


static XEN g_view_files_set_amp(XEN dialog, XEN amp)
{
  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ARG_1, S_setB S_view_files_amp, "a view-files dialog widget"); 
  XEN_ASSERT_TYPE(XEN_NUMBER_P(amp), amp, XEN_ARG_2, S_setB S_view_files_amp, "a number");
  view_files_set_amp((widget_t)(XEN_UNWRAP_WIDGET(dialog)), XEN_TO_C_DOUBLE(amp));
  return(amp);
}


static XEN g_view_files_speed(XEN dialog)
{
  #define H_view_files_speed "(" S_view_files_speed " dialog): speed setting in the given View:Files dialog"
  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ONLY_ARG, S_view_files_speed, "a view-files dialog widget"); 
  return(C_TO_XEN_DOUBLE(view_files_speed((widget_t)(XEN_UNWRAP_WIDGET(dialog)))));
}


static XEN g_view_files_set_speed(XEN dialog, XEN speed)
{
  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ONLY_ARG, S_setB S_view_files_speed, "a view-files dialog widget"); 
  XEN_ASSERT_TYPE(XEN_NUMBER_P(speed), speed, XEN_ARG_2, S_setB S_view_files_speed, "a number");
  view_files_set_speed((widget_t)(XEN_UNWRAP_WIDGET(dialog)), XEN_TO_C_DOUBLE(speed));
  return(speed);
}


static XEN g_view_files_amp_env(XEN dialog)
{
  #define H_view_files_amp_env "(" S_view_files_amp_env " dialog): amp env breakpoints in the given View:Files dialog"
  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ONLY_ARG, S_view_files_amp_env, "a view-files dialog widget"); 
  return(env_to_xen(view_files_amp_env((widget_t)(XEN_UNWRAP_WIDGET(dialog)))));
}


static XEN g_view_files_set_amp_env(XEN dialog, XEN amp_env)
{
  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ONLY_ARG, S_setB S_view_files_amp_env, "a view-files dialog widget"); 
  XEN_ASSERT_TYPE(XEN_LIST_P(amp_env), amp_env, XEN_ARG_2, S_setB S_view_files_amp_env, "an envelope");
  view_files_set_amp_env((widget_t)(XEN_UNWRAP_WIDGET(dialog)), xen_to_env(amp_env));
  return(amp_env);
}


static XEN g_view_files_speed_style(XEN dialog)
{
  #define H_view_files_speed_style "(" S_view_files_speed_style " dialog): speed_style in use in the given View:Files dialog"
  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ONLY_ARG, S_view_files_speed_style, "a view-files dialog widget"); 
  return(C_TO_XEN_INT((int)(view_files_speed_style((widget_t)(XEN_UNWRAP_WIDGET(dialog))))));
}


static XEN g_view_files_set_speed_style(XEN dialog, XEN speed_style)
{
  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ONLY_ARG, S_setB S_view_files_speed_style, "a view-files dialog widget"); 
  XEN_ASSERT_TYPE(XEN_INTEGER_P(speed_style), speed_style, XEN_ARG_2, S_setB S_view_files_speed_style, "an int");
  view_files_set_speed_style((widget_t)(XEN_UNWRAP_WIDGET(dialog)), (speed_style_t)(XEN_TO_C_INT(speed_style)));
  return(speed_style);
}


static XEN g_view_files_selected_files(XEN dialog)
{
  #define H_view_files_selected_files "(" S_view_files_selected_files " dialog): list of files currently selected in the given View:Files dialog"
  XEN result = XEN_EMPTY_LIST;
  char **selected_files;
  int i, len = 0;

  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ONLY_ARG, S_view_files_selected_files, "a view-files dialog widget"); 

  selected_files = view_files_selected_files((widget_t)(XEN_UNWRAP_WIDGET(dialog)), &len);
  if ((selected_files) && (len > 0))
    {
      for (i = 0; i < len; i++)
	{
	  result = XEN_CONS(C_TO_XEN_STRING(selected_files[i]), result);
	  free(selected_files[i]);
	}
      free(selected_files);
    }
  return(result);
}


static XEN g_view_files_set_selected_files(XEN dialog, XEN files)
{
  int i, len;
  char **cfiles = NULL;

  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ARG_1, S_setB S_view_files_selected_files, "a view-files dialog widget");   
  XEN_ASSERT_TYPE(XEN_LIST_P(files), files, XEN_ARG_2, S_setB S_view_files_selected_files, "a list of files or directories");

  len = XEN_LIST_LENGTH(files);
  if (len > 0)
    {
      for (i = 0; i < len; i++)
	if (!(XEN_STRING_P(XEN_LIST_REF(files, i))))
	  {
	    XEN_ASSERT_TYPE(0, XEN_LIST_REF(files, i), i, S_setB S_view_files_selected_files, "a filename (string)");
	    return(XEN_FALSE);
	  }
      cfiles = (char **)calloc(len, sizeof(char *));
      for (i = 0; i < len; i++)
	cfiles[i] = (char *)XEN_TO_C_STRING(XEN_LIST_REF(files, i));
      view_files_set_selected_files((widget_t)(XEN_UNWRAP_WIDGET(dialog)), cfiles, len);
      free(cfiles);
    }
  return(files);
}


static XEN g_view_files_files(XEN dialog)
{
  #define H_view_files_files "(" S_view_files_files " dialog): list of files currently available in the given View:Files dialog"
  XEN result = XEN_EMPTY_LIST;
  char **files;
  int i, len = 0;

  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ONLY_ARG, S_view_files_files, "a view-files dialog widget"); 

  files = view_files_files((widget_t)(XEN_UNWRAP_WIDGET(dialog)), &len);
  if ((files) && (len > 0))
    for (i = 0; i < len; i++)
      result = XEN_CONS(C_TO_XEN_STRING(files[i]), result);
  return(result);
}


static XEN g_view_files_set_files(XEN dialog, XEN files)
{
  int i, len = 0;
  char **cfiles = NULL;

  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ARG_1, S_setB S_view_files_files, "a view-files dialog widget");   
  XEN_ASSERT_TYPE(XEN_LIST_P(files), files, XEN_ARG_2, S_setB S_view_files_files, "a list of files or directories");

  len = XEN_LIST_LENGTH(files);
  if (len > 0)
    {
      for (i = 0; i < len; i++)
	if (!(XEN_STRING_P(XEN_LIST_REF(files, i))))
	  {
	    XEN_ASSERT_TYPE(0, XEN_LIST_REF(files, i), i, S_setB S_view_files_files, "a filename (string)");
	    return(XEN_FALSE);
	  }
      cfiles = (char **)calloc(len, sizeof(char *));
      for (i = 0; i < len; i++)
	cfiles[i] = (char *)XEN_TO_C_STRING(XEN_LIST_REF(files, i));
    }
  view_files_set_files((widget_t)(XEN_UNWRAP_WIDGET(dialog)), cfiles, len);
  if (cfiles) free(cfiles);
  return(files);
}


static XEN view_files_select_hook;

static void view_files_run_select_hook(widget_t dialog, const char *selected_file)
{
  if (XEN_HOOKED(view_files_select_hook))
    run_hook(view_files_select_hook,
	     XEN_LIST_2(XEN_WRAP_WIDGET(dialog),
			C_TO_XEN_STRING(selected_file)),
	     S_view_files_select_hook);
}


static XEN g_add_sound_file_extension(XEN ext)
{
  #define H_add_sound_file_extension "(" S_add_sound_file_extension " ext):  add the file extension 'ext' to the list of sound file extensions"
  XEN_ASSERT_TYPE(XEN_STRING_P(ext), ext, XEN_ONLY_ARG, S_add_sound_file_extension, "a string");
  add_sound_file_extension(XEN_TO_C_STRING(ext));
  return(ext);
}


static XEN g_sound_file_extensions(void)
{
  #define H_sound_file_extensions "(" S_sound_file_extensions "): list of current sound file extensions (used \
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
  int i, len;
  for (i = 0; i < sound_file_extensions_end; i++)
    if (sound_file_extensions[i])
      {
	free(sound_file_extensions[i]);
	sound_file_extensions[i] = NULL;
      }
  sound_file_extensions_end = 0;
  default_sound_file_extensions = 0;
  len = XEN_LIST_LENGTH(lst);
  for (i = 0; i < len; i++)
    if (!(XEN_STRING_P(XEN_LIST_REF(lst, i))))
      {
	XEN_ASSERT_TYPE(0, XEN_LIST_REF(lst, i), i, S_setB S_sound_file_extensions, "a filename extension (a string like \"snd\")");
	return(XEN_FALSE);
      }
  for (i = 0; i < len; i++)
    add_sound_file_extension(XEN_TO_C_STRING(XEN_LIST_REF(lst, i)));
  return(lst);
}


static XEN g_file_write_date(XEN file)
{
  #if HAVE_RUBY
    #define write_date_equivalent "Equivalent to Ruby's File.mtime(file)"
  #endif

  #if HAVE_FORTH
    #define write_date_equivalent "Equivalent to Forth's file-mtime"
  #endif

  #if HAVE_SCHEME
    #define write_date_equivalent ""
  #endif

  #define S_file_write_date "file-write-date"
#ifndef __GNUC__
  #define H_file_write_date "(" S_file_write_date " file): write date of file"
#else
  #define H_file_write_date "(" S_file_write_date " file): write date in the same format as \
current-time:\n(strftime \"%a %d-%b-%Y %H:%M %Z\" (localtime (" S_file_write_date " \"oboe.snd\")))\n" write_date_equivalent
#endif

  time_t date;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ONLY_ARG, S_file_write_date, "a string");
  date = file_write_date(XEN_TO_C_STRING(file));
  return(C_TO_XEN_INT(date));
}


static XEN g_sound_loop_info(XEN snd)
{
  #define H_sound_loop_info "(" S_sound_loop_info " :optional snd): return the sound's loop points as a \
list: (sustain-start sustain-end release-start release-end baseNote detune)"
  int *res;
  snd_info *sp;
  ASSERT_SOUND(S_sound_loop_info, snd, 1);
  sp = get_sp(snd);
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
      sp = get_sp(XEN_UNDEFINED);
    }
  else 
    {
      ASSERT_SOUND(S_setB S_sound_loop_info, snd, 1);
      sp = get_sp(snd);
    }

  if (sp == NULL) 
    return(snd_no_such_sound_error(S_setB S_sound_loop_info, snd));

  if ((sp->user_read_only == FILE_READ_ONLY) || 
      (sp->file_read_only == FILE_READ_ONLY))
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_2(C_TO_XEN_STRING(S_setB S_sound_loop_info ": ~S is write-protected"),
			 C_TO_XEN_STRING(sp->filename)));

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
    hdr->loops = (int *)calloc(MUS_LOOP_INFO_SIZE, sizeof(int));
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
      snd_warning("changing %s's header from %s to aifc to accommodate loop info",
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
		  XEN_LIST_3(C_TO_XEN_STRING(S_setB S_sound_loop_info ": can't save ~S, ~A"),
			     C_TO_XEN_STRING(tmp_file),
			     C_TO_XEN_STRING(snd_io_strerror())));
	return(XEN_FALSE); /* not executed -- just for emphasis */
      }
    sp->writing = true;
    if (err == IO_SAVE_HOOK_CANCELLATION)
      snd_remove(tmp_file, IGNORE_CACHE);
    else 
      {
	err = move_file(tmp_file, sp->filename);
	if (SERIOUS_IO_ERROR(err))
	  {
	    free(tmp_file);
	    sp->writing = false;
	    XEN_ERROR(CANT_UPDATE_FILE,
		      XEN_LIST_4(C_TO_XEN_STRING(S_setB S_sound_loop_info ": can't update ~S: ~A ~A"),
				 C_TO_XEN_STRING(sp->filename),
				 C_TO_XEN_STRING(io_error_name(err)),
				 C_TO_XEN_STRING(snd_io_strerror())));
	    return(XEN_FALSE);
	  }
      }
    sp->writing = false;
    if (err != IO_SAVE_HOOK_CANCELLATION) 
      snd_update(sp);
    free(tmp_file);
    return((err == IO_NO_ERROR) ? XEN_TRUE : C_TO_XEN_INT((int)err));
  }
}


static XEN g_soundfont_info(XEN snd)
{
  /* return all soundfont descriptors as list of lists: ((name start loopstart loopend)) */
  #define H_soundfont_info "(" S_soundfont_info " :optional snd): list of lists describing snd as a soundfont. \
each inner list has the form: (name start loopstart loopend)"

  XEN outlist = XEN_EMPTY_LIST;
  snd_info *sp;
  ASSERT_SOUND(S_soundfont_info, snd, 1);
  sp = get_sp(snd);
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
	    XEN inlist;
	    inlist = XEN_LIST_4(C_TO_XEN_STRING(mus_header_sf2_name(i)),
			        C_TO_XEN_INT(mus_header_sf2_start(i)),
			        C_TO_XEN_INT(mus_header_sf2_loop_start(i)),
			        C_TO_XEN_INT(mus_header_sf2_end(i)));
	    outlist = XEN_CONS(inlist, outlist);
	  }
    }
  return(outlist);
}


static XEN g_sound_files_in_directory(XEN dirname)
{
  #define H_sound_files_in_directory "(" S_sound_files_in_directory " :optional (directory \".\")): return a list of the sound files in 'directory'"
  char *name = NULL;
  XEN res = XEN_EMPTY_LIST;
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(dirname), dirname, XEN_ONLY_ARG, S_sound_files_in_directory, "a string");
  if (XEN_STRING_P(dirname))
    name = mus_expand_filename(XEN_TO_C_STRING(dirname));
  else name = mus_expand_filename(".");
  if (name)
    {
      dir_info *dp = NULL;
      dp = find_sound_files_in_dir(name);
      if (dp)
	{
	  int i;
	  for (i = dp->len - 1; i >= 0; i--)
	    res = XEN_CONS(C_TO_XEN_STRING(dp->files[i]->filename), res);
	  dp = free_dir_info(dp);
	}
      free(name);
    }
  return(res);
}


#define S_disk_kspace "disk-kspace"

static XEN g_disk_kspace(XEN name)
{
  #define H_disk_kspace "(" S_disk_kspace " filename): kbytes of space available on partition containing 'filename'"
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ONLY_ARG, S_disk_kspace, "a string");
  return(C_TO_XEN_LONG_LONG(disk_kspace(XEN_TO_C_STRING(name))));
}


static XEN g_open_file_dialog(XEN managed)
{
  #define H_open_file_dialog "(" S_open_file_dialog " :optional (managed " PROC_TRUE ")): create the file dialog if needed and display it if 'managed'"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_open_file_dialog, "a boolean");
  return(XEN_WRAP_WIDGET(make_open_file_dialog(FILE_READ_WRITE, (XEN_BOUND_P(managed)) ? XEN_TO_C_BOOLEAN(managed) : true)));
}


static XEN g_mix_file_dialog(XEN managed)
{
  #define H_mix_file_dialog "(" S_mix_file_dialog " :optional (managed " PROC_TRUE ")): create the mix file dialog if needed and display it if 'managed'"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_mix_file_dialog, "a boolean");
  return(XEN_WRAP_WIDGET(make_mix_file_dialog((XEN_BOUND_P(managed)) ? XEN_TO_C_BOOLEAN(managed) : true)));
}


static XEN g_insert_file_dialog(XEN managed)
{
  #define H_insert_file_dialog "(" S_insert_file_dialog " :optional (managed " PROC_TRUE ")): create the insert file dialog if needed and display it if 'managed'"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_insert_file_dialog, "a boolean");
  return(XEN_WRAP_WIDGET(make_insert_file_dialog((XEN_BOUND_P(managed)) ? XEN_TO_C_BOOLEAN(managed) : true)));
}


static XEN g_edit_header_dialog(XEN snd_n) 
{
  #define H_edit_header_dialog "(" S_edit_header_dialog " :optional snd): start the Edit Header dialog on sound snd"
  snd_info *sp; 
  sp = get_sp(snd_n);
  if ((sp == NULL) || (sp->inuse != SOUND_NORMAL))
    return(snd_no_such_sound_error(S_edit_header_dialog, snd_n));
  return(XEN_WRAP_WIDGET(edit_header(sp)));
}


static XEN g_save_selection_dialog(XEN managed)
{
#define H_save_selection_dialog "(" S_save_selection_dialog " :optional managed): start the Selection Save-as dialog"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_save_selection_dialog, "a boolean");
  return(XEN_WRAP_WIDGET(make_selection_save_as_dialog(XEN_TO_C_BOOLEAN(managed))));
}


static XEN g_save_region_dialog(XEN managed)
{
  #define H_save_region_dialog "(" S_save_region_dialog " :optional managed): start the Region Save-as dialog"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_save_region_dialog, "a boolean");
  return(XEN_WRAP_WIDGET(make_region_save_as_dialog(XEN_TO_C_BOOLEAN(managed))));
}


static XEN g_save_sound_dialog(XEN managed)
{
  #define H_save_sound_dialog "(" S_save_sound_dialog " :optional managed): start the File Save-as dialog"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_save_sound_dialog, "a boolean");
  return(XEN_WRAP_WIDGET(make_sound_save_as_dialog(XEN_TO_C_BOOLEAN(managed))));
}


static XEN g_info_dialog(XEN subject, XEN msg)
{
  #define H_info_dialog "(" S_info_dialog " subject message): start the Info window with subject and message"
  XEN_ASSERT_TYPE(XEN_STRING_P(subject), subject, XEN_ARG_1, S_info_dialog, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, XEN_ARG_2, S_info_dialog, "a string");
  return(XEN_WRAP_WIDGET(post_it(XEN_TO_C_STRING(subject), XEN_TO_C_STRING(msg))));
}


static XEN g_new_sound_dialog(XEN managed)
{
  #define H_new_sound_dialog "(" S_new_sound_dialog " :optional managed): start the File New sound dialog"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_new_sound_dialog, "a boolean");
  return(XEN_WRAP_WIDGET(make_new_file_dialog(XEN_TO_C_BOOLEAN(managed))));
}


/* -------- file-filters and file-sorters -------- */

#define INITIAL_FILE_FILTERS_SIZE 4
#define INITIAL_FILE_SORTERS_SIZE 4

static XEN g_expand_vector(XEN vector, int new_size)
{
  int i, len;
  XEN new_vect;
  len = XEN_VECTOR_LENGTH(vector);
  new_vect = XEN_MAKE_VECTOR(new_size, XEN_FALSE);
  XEN_PROTECT_FROM_GC(new_vect);
  for (i = 0; i < len; i++)
    {
      XEN_VECTOR_SET(new_vect, i, XEN_VECTOR_REF(vector, i));
      XEN_VECTOR_SET(vector, i, XEN_FALSE);
    }
#if HAVE_RUBY || HAVE_FORTH
  XEN_UNPROTECT_FROM_GC(vector);
#endif
  return(new_vect);
}


static bool file_filter_ok(XEN name, XEN proc, const char *caller)
{
  char *errmsg;
  XEN errstr;
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_1, caller, "a string");   
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(proc), proc, XEN_ARG_2, caller, "a procedure of 1 arg (filename)");
  errmsg = procedure_ok(proc, 1, caller, "function", 2);
  if (errmsg)
    {
      errstr = C_TO_XEN_STRING(errmsg);
      free(errmsg);
      snd_bad_arity_error(caller, errstr, proc);
      return(false);
    }
  return(true);
}


static XEN g_add_file_filter(XEN name, XEN proc)
{
  #define H_add_file_filter "(" S_add_file_filter " name proc) -- add proc with identifier name to file filter list"
  int i, len;
  if (file_filter_ok(name, proc, S_add_file_filter))
    {
      len = ss->file_filters_size;
      for (i = 0; i < len; i++)
	{
	  if (XEN_FALSE_P(XEN_VECTOR_REF(ss->file_filters, i)))
	    {
	      XEN_VECTOR_SET(ss->file_filters, i, XEN_LIST_2(name, proc));
	      return(C_TO_XEN_INT(i));
	    }
	}
      ss->file_filters_size = len * 2;
      ss->file_filters = g_expand_vector(ss->file_filters, ss->file_filters_size);
      XEN_VECTOR_SET(ss->file_filters, len, XEN_LIST_2(name, proc));
      return(C_TO_XEN_INT(len));
    }
  return(XEN_FALSE);
}


static XEN g_delete_file_filter(XEN index)
{
  #define H_delete_file_filter "(" S_delete_file_filter " index) -- delete proc with identifier index from file filter list"
  int pos;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(index), index, XEN_ONLY_ARG, S_delete_file_filter, "a file-filter function index");   
  pos = XEN_TO_C_INT(index);
  if ((pos >= 0) &&
      (pos < ss->file_filters_size))
    XEN_VECTOR_SET(ss->file_filters, pos, XEN_FALSE);
  return(index);
}


static bool file_sorter_ok(XEN name, XEN proc, const char *caller)
{
  char *errmsg;
  XEN errstr;
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_1, caller, "a string");   
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(proc), proc, XEN_ARG_2, caller, "a procedure of 2 args (file1 and file2)");
  errmsg = procedure_ok(proc, 2, caller, "function", 2);
  if (errmsg)
    {
      errstr = C_TO_XEN_STRING(errmsg);
      free(errmsg);
      snd_bad_arity_error(caller, errstr, proc);
      return(false);
    }
  return(true);
}


static XEN g_add_file_sorter(XEN name, XEN proc)
{
  #define H_add_file_sorter "(" S_add_file_sorter " name proc) -- add proc with identifier name to file sorter list, returns its index"
  int i, len, choice = -1;
  /* type checks are redundant here */

  if (file_sorter_ok(name, proc, S_add_file_sorter))
    {
      len = ss->file_sorters_size;
      for (i = 0; i < len; i++)
	{
	  if (XEN_FALSE_P(XEN_VECTOR_REF(ss->file_sorters, i)))
	    {
	      XEN_VECTOR_SET(ss->file_sorters, i, XEN_LIST_2(name, proc));
	      choice = i;
	      break;
	    }
	}
      if (choice == -1)
	{
	  ss->file_sorters_size = len * 2;
	  ss->file_sorters = g_expand_vector(ss->file_sorters, ss->file_sorters_size);
	  XEN_VECTOR_SET(ss->file_sorters, len, XEN_LIST_2(name, proc));
	  choice = len;
	}
      view_files_reflect_sort_items();
    }
  return(C_TO_XEN_INT(choice + SORT_XEN));
}


static XEN g_delete_file_sorter(XEN index)
{
  #define H_delete_file_sorter "(" S_delete_file_sorter " index) -- delete proc with identifier name from file sorter list"
  int pos;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(index), index, XEN_ONLY_ARG, S_delete_file_sorter, "a file-sorter index");   
  pos = XEN_TO_C_INT(index);
  if ((pos >= SORT_XEN) &&
      ((pos - SORT_XEN) < ss->file_sorters_size))
    XEN_VECTOR_SET(ss->file_sorters, pos - SORT_XEN, XEN_FALSE);
  view_files_reflect_sort_items();
  return(index);
}


static XEN g_sound_file_p(XEN name)
{
  #define H_sound_file_p "(" S_sound_file_p " name): " PROC_TRUE " if name has a known sound file extension"
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ONLY_ARG, S_sound_file_p, "a filename");   
  return(C_TO_XEN_BOOLEAN(sound_file_p(XEN_TO_C_STRING(name))));
}


static XEN g_snd_tempnam(void) 
{
  #define H_snd_tempnam "(" S_snd_tempnam "): return a new temp file name using " S_temp_dir "."
  char *tmp;
  XEN res;
  tmp = snd_tempnam();
  res = C_TO_XEN_STRING(tmp);
  free(tmp);
  return(res);
}


static XEN g_auto_update(void) {return(C_TO_XEN_BOOLEAN(auto_update(ss)));}

static XEN g_set_auto_update(XEN val) 
{
  #define H_auto_update "(" S_auto_update "): " PROC_TRUE " if Snd should automatically update a file if it changes unexpectedly (default: " PROC_FALSE "). \
The number of seconds between update checks is set by " S_auto_update_interval "."
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_auto_update, "a boolean");
  set_auto_update(XEN_TO_C_BOOLEAN(val)); 
  return(C_TO_XEN_BOOLEAN(auto_update(ss)));
}


static XEN g_auto_update_interval(void) {return(C_TO_XEN_DOUBLE(auto_update_interval(ss)));}

static XEN g_set_auto_update_interval(XEN val) 
{
  mus_float_t ctime, old_time;
  #define H_auto_update_interval "(" S_auto_update_interval "): time (seconds) between background checks for changed file on disk (default: 60). \
This value only matters if " S_auto_update " is " PROC_TRUE

  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_auto_update_interval, "a number"); 

  ctime = XEN_TO_C_DOUBLE(val);
  if ((ctime < 0.0) || (ctime > (24 * 3600)))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_auto_update_interval, 1, val, "invalid time");

  old_time = auto_update_interval(ss);
  set_auto_update_interval(ctime);
  /* if new value is 0.0, auto_update_check will notice that, and not run or re-start the update check */
  /* if new value is not 0.0, and old value was 0.0, we need to restart the timeout proc, unless it's still on the queue */

  if ((ctime > 0.0) && (old_time == 0.0))
    auto_update_restart();
  return(C_TO_XEN_DOUBLE(auto_update_interval(ss)));
}


static XEN g_default_output_chans(void) {return(C_TO_XEN_INT(default_output_chans(ss)));}

static XEN g_set_default_output_chans(XEN val) 
{
  #define MAX_OUTPUT_CHANS 1024
  #define H_default_output_chans "(" S_default_output_chans "): default number of channels when a new or temporary file is created (1)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_default_output_chans, "an integer"); 
  set_default_output_chans(mus_iclamp(1, XEN_TO_C_INT(val), MAX_OUTPUT_CHANS));
  return(C_TO_XEN_INT(default_output_chans(ss)));
}


static XEN g_default_output_srate(void) {return(C_TO_XEN_INT(default_output_srate(ss)));}

static XEN g_set_default_output_srate(XEN val) 
{
  #define MAX_OUTPUT_SRATE 1000000000
  #define H_default_output_srate "(" S_default_output_srate "): default srate when a new or temporary file is created (22050)" 
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_default_output_srate, "a number"); 
  set_default_output_srate(mus_iclamp(1, XEN_TO_C_INT_OR_ELSE(val, 0), MAX_OUTPUT_SRATE));
  return(C_TO_XEN_INT(default_output_srate(ss)));
}


static XEN g_default_output_header_type(void) {return(C_TO_XEN_INT(default_output_header_type(ss)));}

static XEN g_set_default_output_header_type(XEN val) 
{
  int typ;
  #define H_default_output_header_type "(" S_default_output_header_type "): default header type when a new or temporary file is created. \
Normally this is " S_mus_next "; -1 here indicates you want Snd to use the current sound's header type, if possible. \
Other writable headers include " S_mus_aiff ", " S_mus_riff ", " S_mus_ircam ", " S_mus_nist ", " S_mus_aifc ", and " S_mus_raw "."

  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_default_output_header_type, "an integer"); 

  typ = XEN_TO_C_INT(val);
  if (mus_header_writable(typ, -2))
    set_default_output_header_type(typ); 
  else XEN_OUT_OF_RANGE_ERROR(S_setB S_default_output_header_type, 1, val, "unwritable header type");
  return(C_TO_XEN_INT(default_output_header_type(ss)));
}


static XEN g_default_output_data_format(void) {return(C_TO_XEN_INT(default_output_data_format(ss)));}

static XEN g_set_default_output_data_format(XEN val) 
{
  int format;
  #define H_default_output_data_format "(" S_default_output_data_format "): default data format when a new or temporary file is created, \
normally " S_mus_bshort "; -1 here means try to use the current sound's data format; many other formats \
are available, but not all are compatible with all header types"

  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_default_output_data_format, "an integer"); 

  format = XEN_TO_C_INT(val);
  if (mus_data_format_p(format))
    set_default_output_data_format(format); 
  else XEN_OUT_OF_RANGE_ERROR(S_setB S_default_output_data_format, 1, val, "unknown data format");
  return(C_TO_XEN_INT(default_output_data_format(ss)));
}


static XEN g_clipping(void) {return(C_TO_XEN_BOOLEAN(clipping(ss)));}

static XEN g_set_clipping(XEN val) 
{
  #define H_clipping "(" S_clipping "): " PROC_TRUE " if Snd should clip output values to the current \
output data format's maximum. The default (" PROC_FALSE ") allows them to wrap-around which makes a very loud click"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_clipping, "a boolean");
  set_clipping(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(clipping(ss)));
}


static XEN g_ask_before_overwrite(void) {return(C_TO_XEN_BOOLEAN(ask_before_overwrite(ss)));}

static XEN g_set_ask_before_overwrite(XEN val) 
{
  #define H_ask_before_overwrite "(" S_ask_before_overwrite "): " PROC_TRUE " if you want Snd to ask before overwriting a file. \
If " PROC_FALSE ", any existing file of the same name will be overwritten without warning when you save a sound."
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_ask_before_overwrite, "a boolean");
  set_ask_before_overwrite(XEN_TO_C_BOOLEAN(val)); 
  return(C_TO_XEN_BOOLEAN(ask_before_overwrite(ss)));
}


static XEN g_with_toolbar(void) {return(C_TO_XEN_BOOLEAN(with_toolbar(ss)));}

void set_with_toolbar_and_display(bool val)
{
  set_with_toolbar(val);
#if (!USE_NO_GUI)
  if (with_toolbar(ss))
    show_toolbar();
  else hide_toolbar();
#endif
}

static XEN g_set_with_toolbar(XEN val) 
{
  #define H_with_toolbar "(" S_with_toolbar "): " PROC_TRUE " if you want a toolbar"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_with_toolbar, "a boolean");
  set_with_toolbar_and_display(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(with_toolbar(ss)));
}


static XEN g_with_tooltips(void) {return(C_TO_XEN_BOOLEAN(with_tooltips(ss)));}

void set_with_tooltips(bool val)
{
  in_set_with_tooltips(val);
#if USE_GTK
  g_object_set(gtk_settings_get_default(), "gtk-enable-tooltips", val, NULL);
#endif
}

static XEN g_set_with_tooltips(XEN val) 
{
  #define H_with_tooltips "(" S_with_tooltips "): " PROC_TRUE " if you want tooltips displayed at all"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_with_tooltips, "a boolean");
  set_with_tooltips(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(with_tooltips(ss)));
}


void set_with_menu_icons(bool val)
{
  in_set_with_menu_icons(val);
#if USE_GTK
  g_object_set(gtk_settings_get_default(), "gtk-menu-images", with_menu_icons(ss), NULL);
  g_object_set(gtk_settings_get_default(), "gtk-button-images", with_menu_icons(ss), NULL);
#endif
  /* in Scheme: (g_object_set (GPOINTER (gtk_settings_get_default)) "gtk-menu-images" #t) 
   *   the list of these properties is in GtkSettings.html
   *   others of possible interest: 
   *     gtk-cursor-blink-time, gtk-cursor-blink (set in glistener)
   *     gtk-double-click-time, gtk-enable-tooltips (see set_with_tooltips above)
   *     gtk-menu-bar-accel
   */
}


static XEN g_with_menu_icons(void) {return(C_TO_XEN_BOOLEAN(with_menu_icons(ss)));}

static XEN g_set_with_menu_icons(XEN val) 
{
  #define H_with_menu_icons "(" S_with_menu_icons "): " PROC_TRUE " if you want icons in the menus (gtk only)"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_with_menu_icons, "a boolean");
  set_with_menu_icons(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(with_menu_icons(ss)));
}


static void set_save_as_dialog_src(bool val)
{
  in_set_save_as_dialog_src(val);
  reflect_save_as_src(val);
}

static XEN g_save_as_dialog_src(void) {return(C_TO_XEN_BOOLEAN(save_as_dialog_src(ss)));}

static XEN g_set_save_as_dialog_src(XEN val) 
{
  #define H_save_as_dialog_src "(" S_save_as_dialog_src "): " PROC_TRUE " if you want the 'src' button set by default in the various Save-as dialogs"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_save_as_dialog_src, "a boolean");
  set_save_as_dialog_src(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(save_as_dialog_src(ss)));
}


static void set_save_as_dialog_auto_comment(bool val)
{
  in_set_save_as_dialog_auto_comment(val);
  reflect_save_as_auto_comment(val);
}

static XEN g_save_as_dialog_auto_comment(void) {return(C_TO_XEN_BOOLEAN(save_as_dialog_auto_comment(ss)));}

static XEN g_set_save_as_dialog_auto_comment(XEN val) 
{
  #define H_save_as_dialog_auto_comment "(" S_save_as_dialog_auto_comment "): " PROC_TRUE " if you want the 'auto' button set by default in the various Save-as dialogs"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_save_as_dialog_auto_comment, "a boolean");
  set_save_as_dialog_auto_comment(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(save_as_dialog_auto_comment(ss)));
}


static XEN g_remember_sound_state(void) {return(C_TO_XEN_BOOLEAN(remember_sound_state(ss)));}

static XEN g_set_remember_sound_state(XEN val) 
{
  #define H_remember_sound_state "(" S_remember_sound_state "): " PROC_TRUE " if you want a Snd to remember the current \
state of each sound when it is closed, restoring that state when it is opened again later."
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_remember_sound_state, "a boolean");
  set_remember_sound_state(XEN_TO_C_BOOLEAN(val)); 
  return(C_TO_XEN_BOOLEAN(remember_sound_state(ss)));
}


static XEN g_ask_about_unsaved_edits(void) {return(C_TO_XEN_BOOLEAN(ask_about_unsaved_edits(ss)));}

static XEN g_set_ask_about_unsaved_edits(XEN val) 
{
  #define H_ask_about_unsaved_edits "(" S_ask_about_unsaved_edits "): " PROC_TRUE " if you want Snd to ask whether \
to save unsaved edits when a sound is closed."

  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_ask_about_unsaved_edits, "a boolean");
  set_ask_about_unsaved_edits(XEN_TO_C_BOOLEAN(val)); 
  return(C_TO_XEN_BOOLEAN(ask_about_unsaved_edits(ss)));
}


static XEN g_show_full_duration(void) {return(C_TO_XEN_BOOLEAN(show_full_duration(ss)));}

static XEN g_set_show_full_duration(XEN val) 
{
  int i;
  #define H_show_full_duration "(" S_show_full_duration "): " PROC_TRUE " if you want the entire sound \
displayed whn it is opened."

  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_show_full_duration, "a boolean");
  set_show_full_duration(XEN_TO_C_BOOLEAN(val)); 
  
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	{
	  int j;
	  for (j = 0; j < sp->nchans; j++)
	    set_x_axis_x0x1(sp->chans[j], 0.0, sp->chans[j]->axis->xmax);
	}
    }

  return(C_TO_XEN_BOOLEAN(show_full_duration(ss)));
}


static XEN g_initial_beg(void) {return(C_TO_XEN_DOUBLE(initial_beg(ss)));}

static XEN g_set_initial_beg(XEN val) 
{
  #define H_initial_beg "(" S_initial_beg "): the begin point (in seconds) for the initial graph of a sound."

  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_initial_beg, "a number");
  set_initial_beg(XEN_TO_C_DOUBLE(val)); 
  return(C_TO_XEN_DOUBLE(initial_beg(ss)));
}


static XEN g_initial_dur(void) {return(C_TO_XEN_DOUBLE(initial_dur(ss)));}

static XEN g_set_initial_dur(XEN val) 
{
  #define H_initial_dur "(" S_initial_dur "): the duration (in seconds) for the initial graph of a sound."

  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_initial_dur, "a number");
  set_initial_dur(XEN_TO_C_DOUBLE(val)); 
  return(C_TO_XEN_DOUBLE(initial_dur(ss)));
}


static XEN g_show_full_range(void) {return(C_TO_XEN_BOOLEAN(show_full_range(ss)));}

static XEN g_set_show_full_range(XEN val) 
{
  int i;
  #define H_show_full_range "(" S_show_full_range "): " PROC_TRUE " if you want the graph y-bounds to accommodate the sound's \
max and min when it is opened."

  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_show_full_range, "a boolean");
  set_show_full_range(XEN_TO_C_BOOLEAN(val)); 

  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	{
	  int j;
	  for (j = 0; j < sp->nchans; j++)
	    {
	      chan_info *cp;
	      axis_info *ap;
	      mus_float_t hi;
	      cp = sp->chans[j];
	      ap = cp->axis;
	      hi = channel_maxamp(cp, AT_CURRENT_EDIT_POSITION);
	      if (hi > ap->ymax)
		{
		  ap->ymin = -hi;
		  ap->ymax = hi;
		  ap->y_ambit = 2 * hi;
		  ap->y0 = -hi;
		  ap->y1 = hi;
		  ap->zy = 1.0;
		  ap->sy = 0.0;
		  resize_sy_and_zy(cp);
		  apply_y_axis_change(cp);
		}
	    }
	}
    }

  return(C_TO_XEN_BOOLEAN(show_full_range(ss)));
}




#ifdef XEN_ARGIFY_1
XEN_ARGIFY_1(g_view_files_sort_w, g_view_files_sort)
XEN_ARGIFY_2(g_set_view_files_sort_w, g_set_view_files_sort)
XEN_NARGIFY_1(g_add_sound_file_extension_w, g_add_sound_file_extension)
XEN_NARGIFY_0(g_sound_file_extensions_w, g_sound_file_extensions)
XEN_NARGIFY_1(g_set_sound_file_extensions_w, g_set_sound_file_extensions)
XEN_NARGIFY_1(g_file_write_date_w, g_file_write_date)
XEN_ARGIFY_1(g_soundfont_info_w, g_soundfont_info)
XEN_ARGIFY_2(g_add_directory_to_view_files_list_w, g_add_directory_to_view_files_list)
XEN_ARGIFY_2(g_add_file_to_view_files_list_w, g_add_file_to_view_files_list)
XEN_ARGIFY_1(g_sound_files_in_directory_w, g_sound_files_in_directory)
XEN_ARGIFY_1(g_sound_loop_info_w, g_sound_loop_info)
XEN_ARGIFY_2(g_set_sound_loop_info_w, g_set_sound_loop_info)
XEN_NARGIFY_1(g_disk_kspace_w, g_disk_kspace)
XEN_ARGIFY_1(g_open_file_dialog_w, g_open_file_dialog)
XEN_ARGIFY_1(g_mix_file_dialog_w, g_mix_file_dialog)
XEN_ARGIFY_1(g_insert_file_dialog_w, g_insert_file_dialog)
XEN_ARGIFY_2(g_view_files_dialog_w, g_view_files_dialog)
XEN_ARGIFY_1(g_edit_header_dialog_w, g_edit_header_dialog)
XEN_ARGIFY_1(g_save_selection_dialog_w, g_save_selection_dialog)
XEN_ARGIFY_1(g_save_region_dialog_w, g_save_region_dialog)
XEN_ARGIFY_1(g_save_sound_dialog_w, g_save_sound_dialog)
XEN_ARGIFY_1(g_new_sound_dialog_w, g_new_sound_dialog)
XEN_NARGIFY_2(g_info_dialog_w, g_info_dialog)
XEN_NARGIFY_1(g_view_files_amp_w, g_view_files_amp)
XEN_NARGIFY_2(g_view_files_set_amp_w, g_view_files_set_amp)
XEN_NARGIFY_1(g_view_files_speed_w, g_view_files_speed)
XEN_NARGIFY_2(g_view_files_set_speed_w, g_view_files_set_speed)
XEN_NARGIFY_1(g_view_files_amp_env_w, g_view_files_amp_env)
XEN_NARGIFY_2(g_view_files_set_amp_env_w, g_view_files_set_amp_env)
XEN_NARGIFY_1(g_view_files_speed_style_w, g_view_files_speed_style)
XEN_NARGIFY_2(g_view_files_set_speed_style_w, g_view_files_set_speed_style)
XEN_NARGIFY_1(g_view_files_selected_files_w, g_view_files_selected_files)
XEN_NARGIFY_1(g_view_files_files_w, g_view_files_files)
XEN_NARGIFY_2(g_view_files_set_selected_files_w, g_view_files_set_selected_files)
XEN_NARGIFY_2(g_view_files_set_files_w, g_view_files_set_files)
XEN_NARGIFY_1(g_delete_file_filter_w, g_delete_file_filter)
XEN_NARGIFY_2(g_add_file_filter_w, g_add_file_filter)
XEN_NARGIFY_1(g_delete_file_sorter_w, g_delete_file_sorter)
XEN_NARGIFY_2(g_add_file_sorter_w, g_add_file_sorter)
XEN_NARGIFY_1(g_sound_file_p_w, g_sound_file_p)
XEN_NARGIFY_0(g_snd_tempnam_w, g_snd_tempnam)
XEN_NARGIFY_0(g_auto_update_w, g_auto_update)
XEN_NARGIFY_1(g_set_auto_update_w, g_set_auto_update)
XEN_NARGIFY_0(g_auto_update_interval_w, g_auto_update_interval)
XEN_NARGIFY_1(g_set_auto_update_interval_w, g_set_auto_update_interval)
XEN_NARGIFY_0(g_default_output_chans_w, g_default_output_chans)
XEN_NARGIFY_1(g_set_default_output_chans_w, g_set_default_output_chans)
XEN_NARGIFY_0(g_default_output_srate_w, g_default_output_srate)
XEN_NARGIFY_1(g_set_default_output_srate_w, g_set_default_output_srate)
XEN_NARGIFY_0(g_default_output_header_type_w, g_default_output_header_type)
XEN_NARGIFY_1(g_set_default_output_header_type_w, g_set_default_output_header_type)
XEN_NARGIFY_0(g_default_output_data_format_w, g_default_output_data_format)
XEN_NARGIFY_1(g_set_default_output_data_format_w, g_set_default_output_data_format)
XEN_NARGIFY_0(g_ask_before_overwrite_w, g_ask_before_overwrite)
XEN_NARGIFY_1(g_set_ask_before_overwrite_w, g_set_ask_before_overwrite)
XEN_NARGIFY_0(g_with_toolbar_w, g_with_toolbar)
XEN_NARGIFY_1(g_set_with_toolbar_w, g_set_with_toolbar)
XEN_NARGIFY_0(g_with_tooltips_w, g_with_tooltips)
XEN_NARGIFY_1(g_set_with_tooltips_w, g_set_with_tooltips)
XEN_NARGIFY_0(g_with_menu_icons_w, g_with_menu_icons)
XEN_NARGIFY_1(g_set_with_menu_icons_w, g_set_with_menu_icons)
XEN_NARGIFY_0(g_save_as_dialog_src_w, g_save_as_dialog_src)
XEN_NARGIFY_1(g_set_save_as_dialog_src_w, g_set_save_as_dialog_src)
XEN_NARGIFY_0(g_save_as_dialog_auto_comment_w, g_save_as_dialog_auto_comment)
XEN_NARGIFY_1(g_set_save_as_dialog_auto_comment_w, g_set_save_as_dialog_auto_comment)
XEN_NARGIFY_0(g_remember_sound_state_w, g_remember_sound_state)
XEN_NARGIFY_1(g_set_remember_sound_state_w, g_set_remember_sound_state)
XEN_NARGIFY_0(g_ask_about_unsaved_edits_w, g_ask_about_unsaved_edits)
XEN_NARGIFY_1(g_set_ask_about_unsaved_edits_w, g_set_ask_about_unsaved_edits)
XEN_NARGIFY_0(g_show_full_duration_w, g_show_full_duration)
XEN_NARGIFY_1(g_set_show_full_duration_w, g_set_show_full_duration)
XEN_NARGIFY_0(g_show_full_range_w, g_show_full_range)
XEN_NARGIFY_1(g_set_show_full_range_w, g_set_show_full_range)
XEN_NARGIFY_0(g_initial_beg_w, g_initial_beg)
XEN_NARGIFY_1(g_set_initial_beg_w, g_set_initial_beg)
XEN_NARGIFY_0(g_initial_dur_w, g_initial_dur)
XEN_NARGIFY_1(g_set_initial_dur_w, g_set_initial_dur)
XEN_NARGIFY_0(g_clipping_w, g_clipping)
XEN_NARGIFY_1(g_set_clipping_w, g_set_clipping)
#else
#define g_view_files_sort_w g_view_files_sort
#define g_set_view_files_sort_w g_set_view_files_sort
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
#define g_disk_kspace_w g_disk_kspace
#define g_open_file_dialog_w g_open_file_dialog
#define g_mix_file_dialog_w g_mix_file_dialog
#define g_insert_file_dialog_w g_insert_file_dialog
#define g_view_files_dialog_w g_view_files_dialog
#define g_edit_header_dialog_w g_edit_header_dialog
#define g_save_selection_dialog_w g_save_selection_dialog
#define g_save_region_dialog_w g_save_region_dialog
#define g_save_sound_dialog_w g_save_sound_dialog
#define g_new_sound_dialog_w g_new_sound_dialog
#define g_info_dialog_w g_info_dialog
#define g_view_files_amp_w g_view_files_amp
#define g_view_files_set_amp_w g_view_files_set_amp
#define g_view_files_amp_env_w g_view_files_amp_env
#define g_view_files_set_amp_env_w g_view_files_set_amp_env
#define g_view_files_speed_style_w g_view_files_speed_style
#define g_view_files_set_speed_style_w g_view_files_set_speed_style
#define g_view_files_speed_w g_view_files_speed
#define g_view_files_set_speed_w g_view_files_set_speed
#define g_view_files_selected_files_w g_view_files_selected_files
#define g_view_files_files_w g_view_files_files
#define g_view_files_set_selected_files_w g_view_files_set_selected_files
#define g_view_files_set_files_w g_view_files_set_files
#define g_delete_file_filter_w g_delete_file_filter
#define g_add_file_filter_w g_add_file_filter
#define g_delete_file_sorter_w g_delete_file_sorter
#define g_add_file_sorter_w g_add_file_sorter
#define g_sound_file_p_w g_sound_file_p
#define g_snd_tempnam_w g_snd_tempnam
#define g_auto_update_w g_auto_update
#define g_set_auto_update_w g_set_auto_update
#define g_auto_update_interval_w g_auto_update_interval
#define g_set_auto_update_interval_w g_set_auto_update_interval
#define g_default_output_chans_w g_default_output_chans
#define g_set_default_output_chans_w g_set_default_output_chans
#define g_default_output_srate_w g_default_output_srate
#define g_set_default_output_srate_w g_set_default_output_srate
#define g_default_output_header_type_w g_default_output_header_type
#define g_set_default_output_header_type_w g_set_default_output_header_type
#define g_default_output_data_format_w g_default_output_data_format
#define g_set_default_output_data_format_w g_set_default_output_data_format
#define g_ask_before_overwrite_w g_ask_before_overwrite
#define g_set_ask_before_overwrite_w g_set_ask_before_overwrite
#define g_with_toolbar_w g_with_toolbar
#define g_set_with_toolbar_w g_set_with_toolbar
#define g_with_tooltips_w g_with_tooltips
#define g_set_with_tooltips_w g_set_with_tooltips
#define g_with_menu_icons_w g_with_menu_icons
#define g_set_with_menu_icons_w g_set_with_menu_icons
#define g_save_as_dialog_src_w g_save_as_dialog_src
#define g_set_save_as_dialog_src_w g_set_save_as_dialog_src
#define g_save_as_dialog_auto_comment_w g_save_as_dialog_auto_comment
#define g_set_save_as_dialog_auto_comment_w g_set_save_as_dialog_auto_comment
#define g_remember_sound_state_w g_remember_sound_state
#define g_set_remember_sound_state_w g_set_remember_sound_state
#define g_ask_about_unsaved_edits_w g_ask_about_unsaved_edits
#define g_set_ask_about_unsaved_edits_w g_set_ask_about_unsaved_edits
#define g_show_full_duration_w g_show_full_duration
#define g_set_show_full_duration_w g_set_show_full_duration
#define g_show_full_range_w g_show_full_range
#define g_set_show_full_range_w g_set_show_full_range
#define g_initial_beg_w g_initial_beg
#define g_set_initial_beg_w g_set_initial_beg
#define g_initial_dur_w g_initial_dur
#define g_set_initial_dur_w g_set_initial_dur
#define g_clipping_w g_clipping
#define g_set_clipping_w g_set_clipping
#endif


void g_init_file(void)
{
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_view_files_amp, g_view_files_amp_w, H_view_files_amp,
				   S_setB S_view_files_amp, g_view_files_set_amp_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_view_files_amp_env, g_view_files_amp_env_w, H_view_files_amp_env,
				   S_setB S_view_files_amp_env, g_view_files_set_amp_env_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_view_files_speed_style, g_view_files_speed_style_w, H_view_files_speed_style,
				   S_setB S_view_files_speed_style, g_view_files_set_speed_style_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_view_files_speed, g_view_files_speed_w, H_view_files_speed,
				   S_setB S_view_files_speed, g_view_files_set_speed_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_view_files_files, g_view_files_files_w, H_view_files_files,
				   S_setB S_view_files_files, g_view_files_set_files_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_view_files_selected_files, g_view_files_selected_files_w, H_view_files_selected_files,
				   S_setB S_view_files_selected_files, g_view_files_set_selected_files_w,  1, 0, 2, 0);

  XEN_DEFINE_SAFE_PROCEDURE(S_add_sound_file_extension, g_add_sound_file_extension_w, 1, 0, 0, H_add_sound_file_extension);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_sound_file_extensions, g_sound_file_extensions_w, H_sound_file_extensions,
				   S_setB S_sound_file_extensions, g_set_sound_file_extensions_w,  0, 0, 1, 0);

  XEN_DEFINE_SAFE_PROCEDURE(S_sound_file_p,                     g_sound_file_p_w,                     1, 0, 0, H_sound_file_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_file_write_date,                  g_file_write_date_w,                  1, 0, 0, H_file_write_date);
  XEN_DEFINE_SAFE_PROCEDURE(S_soundfont_info,                   g_soundfont_info_w,                   0, 1, 0, H_soundfont_info);
  XEN_DEFINE_SAFE_PROCEDURE(S_add_directory_to_view_files_list, g_add_directory_to_view_files_list_w, 1, 1, 0, H_add_directory_to_view_files_list);
  XEN_DEFINE_SAFE_PROCEDURE(S_add_file_to_view_files_list,      g_add_file_to_view_files_list_w,      1, 1, 0, H_add_file_to_view_files_list);
  XEN_DEFINE_SAFE_PROCEDURE(S_sound_files_in_directory,         g_sound_files_in_directory_w,         0, 1, 0, H_sound_files_in_directory);
  XEN_DEFINE_SAFE_PROCEDURE(S_disk_kspace,                      g_disk_kspace_w,                      1, 0, 0, H_disk_kspace);

  XEN_DEFINE_PROCEDURE(S_open_file_dialog,                 g_open_file_dialog_w,                 0, 1, 0, H_open_file_dialog);
  XEN_DEFINE_PROCEDURE(S_mix_file_dialog,                  g_mix_file_dialog_w,                  0, 1, 0, H_mix_file_dialog);
  XEN_DEFINE_PROCEDURE(S_insert_file_dialog,               g_insert_file_dialog_w,               0, 1, 0, H_insert_file_dialog);
  XEN_DEFINE_PROCEDURE(S_view_files_dialog,                g_view_files_dialog_w,                0, 2, 0, H_view_files_dialog);
  XEN_DEFINE_PROCEDURE(S_edit_header_dialog,               g_edit_header_dialog_w,               0, 1, 0, H_edit_header_dialog);
  XEN_DEFINE_PROCEDURE(S_save_selection_dialog,            g_save_selection_dialog_w,            0, 1, 0, H_save_selection_dialog);
  XEN_DEFINE_PROCEDURE(S_save_region_dialog,               g_save_region_dialog_w,               0, 1, 0, H_save_region_dialog);
  XEN_DEFINE_PROCEDURE(S_save_sound_dialog,                g_save_sound_dialog_w,                0, 1, 0, H_save_sound_dialog);
  XEN_DEFINE_PROCEDURE(S_new_sound_dialog,                 g_new_sound_dialog_w,                 0, 1, 0, H_new_sound_dialog);
  XEN_DEFINE_PROCEDURE(S_info_dialog,                      g_info_dialog_w,                      2, 0, 0, H_info_dialog);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_sound_loop_info,      g_sound_loop_info_w, H_sound_loop_info,
				   S_setB S_sound_loop_info, g_set_sound_loop_info_w,  0, 1, 1, 1);

  XEN_DEFINE_VARIABLE(S_snd_opened_sound, snd_opened_sound, XEN_FALSE);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_view_files_sort, g_view_files_sort_w, H_view_files_sort,
				   S_setB S_view_files_sort, g_set_view_files_sort_w,  0, 1, 1, 1);

  XEN_ADD_HOOK(ss->snd_open_file_hook, vf_open_file_watcher_w, "view-files-dialog-open-file-handler", "view-files dialog open-file handler");

  #define H_open_hook S_open_hook " (name): called each time a file is opened (before the actual open). \
If it returns " PROC_TRUE ", the file is not opened."

  #define H_before_close_hook S_before_close_hook " (snd): called each time a file is closed (before the close). \
If it returns " PROC_TRUE ", the file is not closed."

  #define H_close_hook S_close_hook " (snd): called each time a file is closed (before the close)."

  #define H_bad_header_hook S_bad_header_hook " (name): called if a file has some bogus-looking header. \
Return " PROC_TRUE " to give up on that file."

  #define H_after_save_as_hook S_after_save_as_hook " (snd name dialog): called \
upon File:Save as or " S_save_sound_as " completion."

  #define H_before_save_as_hook S_before_save_as_hook " (snd name selection sampling-rate header-type data-format comment): called \
before File:Save as or " S_save_sound_as ". Provides a way to fixup a sound just before it is saved."

#if HAVE_SCHEME
  #define H_during_open_hook S_during_open_hook " (fd name reason): called after file is opened, \
but before data has been read. \n\
  (hook-push " S_during_open_hook "\n\
    (lambda (fd name reason) \n\
      (if (= (" S_mus_sound_header_type " name) " S_mus_raw ")\n\
          (set! (" S_mus_file_prescaler " fd) 500.0))))"

  #define H_after_open_hook S_after_open_hook " (snd): called just before the new file's window is displayed. \
This provides a way to set various sound-specific defaults. \n\
  (hook-push " S_after_open_hook "\n\
    (lambda (snd) \n\
      (if (> (" S_channels " snd) 1) \n\
          (set! (" S_channel_style " snd) " S_channels_combined "))))"
#endif

#if HAVE_RUBY
  #define H_during_open_hook "$" S_during_open_hook " lambda do |fd, name, reason| ...; called after file is opened, \
but before data has been read. \n\
  $during_open_hook.add_hook!(\"during-open-hook\") do |fd, name, reason|\n\
    if (mus_sound_header_type(name) == Mus_raw)\n\
      set_mus_file_prescaler(fd, 500.0)\n\
    end\n\
  end"
  #define H_after_open_hook S_after_open_hook " (snd): called just before the new file's window is displayed. \
This provides a way to set various sound-specific defaults. \n\
  $after_open_hook.add-hook!(\"set-channels-combined\") do |snd| \n\
    if (channels(snd) > 1) \n\
      set_channel_style(Channels_combined, snd)\n\
    end\n\
  end"
#endif

#if HAVE_FORTH
  #define H_during_open_hook S_during_open_hook " (fd name reason): called after file is opened, \
but before data has been read. \n\
" S_during_open_hook " lambda: <{ fd name reason }>\n\
  name " S_mus_sound_header_type " " S_mus_raw " = if\n\
    fd 500.0 set-" S_mus_file_prescaler "\n\
  else\n\
    #f\n\
  then\n\
; add-hook!"
  #define H_after_open_hook S_after_open_hook " (snd): called just before the new file's window is displayed. \
This provides a way to set various sound-specific defaults. \n\
" S_after_open_hook " lambda: <{ snd }>\n\
  snd " S_channels " 1 > if\n\
    " S_channels_combined " snd set-" S_channel_style "\n\
  else\n\
    #f\n\
  then\n\
; add-hook!"
#endif

  #define H_output_name_hook S_output_name_hook " (name): called from the File:New dialog.  If it returns a filename, \
that name is presented in the New File dialog."

  open_hook =           XEN_DEFINE_HOOK(S_open_hook,           "(make-hook 'name)",                1, H_open_hook);
  before_close_hook =   XEN_DEFINE_HOOK(S_before_close_hook,   "(make-hook 'snd)",                 1, H_before_close_hook);
  close_hook =          XEN_DEFINE_HOOK(S_close_hook,          "(make-hook 'snd)",                 1, H_close_hook);
  bad_header_hook =     XEN_DEFINE_HOOK(S_bad_header_hook,     "(make-hook 'name)",                1, H_bad_header_hook);
  after_save_as_hook =  XEN_DEFINE_HOOK(S_after_save_as_hook,  "(make-hook 'snd 'name 'dialog)",   3, H_after_save_as_hook);
  before_save_as_hook = XEN_DEFINE_HOOK(S_before_save_as_hook, "(make-hook 'snd 'name 'selection 'sampling-rate 'header-type 'data-format 'comment)", 7, H_before_save_as_hook); 
  during_open_hook =    XEN_DEFINE_HOOK(S_during_open_hook,    "(make-hook 'fd 'name 'reason)",    3, H_during_open_hook);
  after_open_hook =     XEN_DEFINE_HOOK(S_after_open_hook,     "(make-hook 'snd)",                 1, H_after_open_hook);
  output_name_hook =    XEN_DEFINE_HOOK(S_output_name_hook,    "(make-hook 'name)",                1, H_output_name_hook);

  #define H_open_raw_sound_hook S_open_raw_sound_hook " (name state): called when a headerless sound file is opened. \
Its result can be a list describing the raw file's attributes (thereby bypassing the Raw File Dialog and so on). \
The list is interpreted as (list chans srate data-format data-location data-length) where trailing elements can \
be omitted (location defaults to 0, and length defaults to the file length in bytes)."

  open_raw_sound_hook = XEN_DEFINE_HOOK(S_open_raw_sound_hook, "(make-hook 'name 'state)", 2, H_open_raw_sound_hook);

  #define H_update_hook S_update_hook " (snd): called just before " S_update_sound " is called. \
The update process can  be triggered by a variety of situations, not just by " S_update_sound ". \
The hook is passed the sound's index.  If it returns " PROC_TRUE ", the update is cancelled (this is not \
recommended!); if it returns a procedure of one argument, that procedure is called upon \
completion of the update operation; its argument is the (possibly different) sound index. \
Snd tries to maintain the index across the update, but if you change the number of channels \
the newly updated sound may have a different index."

  update_hook = XEN_DEFINE_HOOK(S_update_hook, "(make-hook 'snd)", 1, H_update_hook);

  #define H_view_files_select_hook S_view_files_select_hook "(dialog name): called when a file is selected in the \
files list of the View Files dialog.  If it returns " PROC_TRUE ", the default action, opening the file, is omitted."

  view_files_select_hook = XEN_DEFINE_HOOK(S_view_files_select_hook, "(make-hook 'dialog 'name)", 2, H_view_files_select_hook);

  #define H_info_popup_hook S_info_popup_hook " (snd): called by the info popup dialog."

  info_popup_hook = XEN_DEFINE_HOOK(S_info_popup_hook, "(make-hook 'snd)", 1, H_info_popup_hook); 


  /* file-filters and file-sorters are lists from user's point of view, but I want to
   *   make sure they're gc-protected through add/delete/set, and want such code compatible
   *   with current Ruby xen macros, so I'll use an array internally.
   */
  ss->file_filters_size = INITIAL_FILE_FILTERS_SIZE;
  ss->file_sorters_size = INITIAL_FILE_SORTERS_SIZE;
  ss->file_filters = XEN_MAKE_VECTOR(ss->file_filters_size, XEN_FALSE);
  ss->file_sorters = XEN_MAKE_VECTOR(ss->file_sorters_size, XEN_FALSE);
  XEN_PROTECT_FROM_GC(ss->file_filters);
  XEN_PROTECT_FROM_GC(ss->file_sorters);

  XEN_DEFINE_SAFE_PROCEDURE(S_add_file_filter,    g_add_file_filter_w,    2, 0, 0, H_add_file_filter);
  XEN_DEFINE_SAFE_PROCEDURE(S_delete_file_filter, g_delete_file_filter_w, 1, 0, 0, H_delete_file_filter);

  XEN_DEFINE_SAFE_PROCEDURE(S_add_file_sorter,    g_add_file_sorter_w,    2, 0, 0, H_add_file_sorter);
  XEN_DEFINE_SAFE_PROCEDURE(S_delete_file_sorter, g_delete_file_sorter_w, 1, 0, 0, H_delete_file_sorter);
  XEN_DEFINE_SAFE_PROCEDURE(S_snd_tempnam,        g_snd_tempnam_w,        0, 0, 0, H_snd_tempnam);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_auto_update, g_auto_update_w, H_auto_update,
				   S_setB S_auto_update, g_set_auto_update_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_auto_update_interval, g_auto_update_interval_w, H_auto_update_interval,
				   S_setB S_auto_update_interval, g_set_auto_update_interval_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_ask_before_overwrite, g_ask_before_overwrite_w, H_ask_before_overwrite,
				   S_setB S_ask_before_overwrite, g_set_ask_before_overwrite_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_with_toolbar, g_with_toolbar_w, H_with_toolbar,
				   S_setB S_with_toolbar, g_set_with_toolbar_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_with_tooltips, g_with_tooltips_w, H_with_tooltips,
				   S_setB S_with_tooltips, g_set_with_tooltips_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_with_menu_icons, g_with_menu_icons_w, H_with_menu_icons,
				   S_setB S_with_menu_icons, g_set_with_menu_icons_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_save_as_dialog_src, g_save_as_dialog_src_w, H_save_as_dialog_src,
				   S_setB S_save_as_dialog_src, g_set_save_as_dialog_src_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_save_as_dialog_auto_comment, g_save_as_dialog_auto_comment_w, H_save_as_dialog_auto_comment,
				   S_setB S_save_as_dialog_auto_comment, g_set_save_as_dialog_auto_comment_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_remember_sound_state, g_remember_sound_state_w, H_remember_sound_state,
				   S_setB S_remember_sound_state, g_set_remember_sound_state_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_ask_about_unsaved_edits, g_ask_about_unsaved_edits_w, H_ask_about_unsaved_edits,
				   S_setB S_ask_about_unsaved_edits, g_set_ask_about_unsaved_edits_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_show_full_duration, g_show_full_duration_w, H_show_full_duration,
				   S_setB S_show_full_duration, g_set_show_full_duration_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_show_full_range, g_show_full_range_w, H_show_full_range,
				   S_setB S_show_full_range, g_set_show_full_range_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_initial_beg, g_initial_beg_w, H_initial_beg,
				   S_setB S_initial_beg, g_set_initial_beg_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_initial_dur, g_initial_dur_w, H_initial_dur,
				   S_setB S_initial_dur, g_set_initial_dur_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_default_output_chans, g_default_output_chans_w, H_default_output_chans,
				   S_setB S_default_output_chans, g_set_default_output_chans_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_default_output_srate, g_default_output_srate_w, H_default_output_srate,
				   S_setB S_default_output_srate, g_set_default_output_srate_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_default_output_header_type, g_default_output_header_type_w, H_default_output_header_type,
				   S_setB S_default_output_header_type, g_set_default_output_header_type_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_default_output_data_format, g_default_output_data_format_w, H_default_output_data_format,
				   S_setB S_default_output_data_format, g_set_default_output_data_format_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_clipping, g_clipping_w, H_clipping,
				   S_setB S_clipping, g_set_clipping_w,  0, 0, 1, 0);
}
