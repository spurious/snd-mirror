#include "snd.h"

#if defined(NEXT) || defined(HAVE_SYS_DIR_H)
  #include <sys/dir.h>
  #include <sys/dirent.h>
  #define dirent direct
#else
  #if defined(WINDOZE) && (!(defined(__CYGWIN__)))
    #include <direct.h>
  #else
    #include <dirent.h>
  #endif
#endif
      
#if defined(HAVE_SYS_STATFS_H)
  #include <sys/statfs.h>
#endif
#if defined(HAVE_SYS_VFS_H)
  #include <sys/vfs.h>
#endif
#if (defined(HAVE_SYS_MOUNT_H) || defined(__APPLE__))
  #include <sys/mount.h>
#endif

#if defined(WINDOZE) || (!(defined(FSTATFS_ARGS))) || (FSTATFS_ARGS == 0) || defined(BEOS)
  int disk_kspace (int fd) {return(1234567);}
  int is_link(char *filename) {return(0);}
  int is_directory(char *filename) {return(0);}
#else

int disk_kspace (int fd)
{
  struct statfs buf;
  int err;
#if (FSTATFS_ARGS == 4)
  err = fstatfs(fd,&buf,sizeof(buf),0);
#else
  err = fstatfs(fd,&buf);
#endif
  /* in 32 bit land, the number of bytes can easily go over 2^32, so we'll look at kbytes here */
  if (err == 0) 
    {
#ifndef NEXT
      if (buf.f_bsize == 1024) return(buf.f_bfree);
      else if (buf.f_bsize == 512) return(buf.f_bfree >> 1);
      else return((int)(buf.f_bsize * ((Float)(buf.f_bfree)/1024.0)));
#else
      return(buf.f_bavail);
#endif
    }
  return(err);
}
int is_link(char *filename)
{
  struct stat statbuf;
  if (lstat(filename,&statbuf) >= 0) return(S_ISLNK(statbuf.st_mode));
  return(0);
}

int is_directory(char *filename)
{
  struct stat statbuf;
  if (lstat(filename,&statbuf) >= 0) return(S_ISDIR(statbuf.st_mode));
  return(0);
}
#endif

time_t file_write_date(char *filename)
{
  struct stat statbuf;
  int err;
  err = stat(filename,&statbuf);
  if (err < 0) return(err);
  return((time_t)(statbuf.st_mtime));
}

file_info *make_file_info_1(char *fullname, snd_state *ss)
{
  int fd;
  file_info *hdr;
  fd = open(fullname,O_RDONLY,0);
  if (fd == -1)
    {
      snd_error("%s: %s",fullname,strerror(errno));
      return(NULL);
    }
  else
    close(fd);  
  hdr = (file_info *)CALLOC(1,sizeof(file_info));
  hdr->name = copy_string(fullname);
  hdr->type = mus_sound_header_type(fullname);
  if ((hdr->type == MUS_RAW) && (use_raw_defaults(ss)))
    {
      hdr->srate = raw_srate(ss);
      hdr->format = raw_format(ss);
      hdr->chans = raw_chans(ss);
      mus_header_set_raw_defaults(raw_srate(ss),raw_chans(ss),raw_format(ss));
    }
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
  hdr = (file_info *)CALLOC(1,sizeof(file_info));
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
	  hdr->loops = (int *)CALLOC(6,sizeof(int));
	  for (i=0;i<6;i++) hdr->loops[i] = ohdr->loops[i];
	}
    }
  return(hdr);
}

static file_info *translate_file(char *filename, snd_state *ss)
{
  char *newname,*tempname;
  file_info *hdr = NULL;
  int err,len,fd;
  len = strlen(filename);
  newname = (char *)CALLOC(len+5,sizeof(char));
  strcpy(newname,filename);
  /* too many special cases to do anything smart here -- I'll just tack on '.snd' */
  newname[len]='.';
  newname[len+1]='s';
  newname[len+2]='n';
  newname[len+3]='d';
  newname[len+4]='\0';
  /* before calling the translator we need to check for write-access to the current directory,
   * and if none, try to find a temp directory we can write to
   */
  fd = creat(newname,0666);
  if (fd == -1)
    {
      tempname = snd_tempnam(ss);
      fd = creat(tempname,0666);
      if (fd == -1)
	{
	  snd_error("can't write translator temp file: %s or %s!",newname,tempname);
	  return(NULL);
	}
      FREE(newname);
      newname = copy_string(tempname);
      free(tempname);
    }
  close(fd);
  err = snd_translate(ss,filename,newname);
  if (err == 0)
    {
      err = mus_header_read(newname);
      if (err == 0)
	{
	  hdr = make_file_info_1(newname,ss);
	  if (hdr) ss->pending_change = newname;
	}
    }
  return(hdr);
}

file_info *make_file_info(char *fullname, snd_state *ss)
{
  file_info *hdr = NULL;
  int type,format,fd;
  fd = open(fullname,O_RDONLY,0);
  if (fd != -1)
    {
      close(fd);  
      type = mus_sound_header_type(fullname);
      format = mus_sound_data_format(fullname);
      if ((mus_sound_srate(fullname) <= 0) || (mus_sound_srate(fullname) > 100000000) ||
	  (mus_sound_chans(fullname) >= 256) || (mus_sound_chans(fullname) <= 0))
	{
	  if ((type != MUS_MIDI_SAMPLE_DUMP) && (type != MUS_IEEE) &&
	      (type != MUS_MUS10) && (type != MUS_HCOM))
	    return(get_reasonable_file_info(fullname,ss,make_file_info_1(fullname,ss)));
	}
      if (format == MUS_UNSUPPORTED) hdr = translate_file(fullname,ss);
      if (type == MUS_RAW)
	{
	  /* need type format chans srate (latter 2 need to over-ride current for hdr build below) */
	  return(get_raw_file_info(fullname,ss));
	}
      hdr = make_file_info_1(fullname,ss);
    }
  if (hdr == NULL)
    {
      set_snd_IO_error(SND_CANNOT_FIND_FILE);
      if (errno != 0)
	snd_error("can't find %s: %s",fullname,strerror(errno));
      else snd_error("can't find %s",fullname);
    }
  return(hdr);
}

file_info *make_temp_header(snd_state *ss, char *fullname, file_info *old_hdr, int samples)
{
  /* make a header for Sun/NeXT, no comment, copy other old_hdr fields */
  file_info *hdr;
  hdr = (file_info *)CALLOC(1,sizeof(file_info));
  hdr->name = copy_string(fullname);
  hdr->samples = samples;
  hdr->data_location = 28;
  hdr->srate = old_hdr->srate;
  hdr->chans = old_hdr->chans;
  hdr->format = default_output_format(ss);
  hdr->type = default_output_type(ss);
  hdr->comment = NULL;
  return(hdr);
}

file_info *free_file_info(file_info *hdr)
{
  if (hdr)
    {
      if (hdr->name) FREE(hdr->name);
      if (hdr->comment) free(hdr->comment);
      if (hdr->loops) FREE(hdr->loops);
      FREE(hdr);
    }
  return(NULL);
}

int *make_file_state(int fd, file_info *hdr, int direction, int chan, int suggested_bufsize)
{
  int *datai;
  int i,bufsize,chansize;
  bufsize = suggested_bufsize;
  if (direction == SND_IO_IN_FILE)
    {
      chansize = (hdr->samples/hdr->chans);
      if (MAX_BUFFER_SIZE > chansize) bufsize = chansize+1;
    }
  else chansize = 0;
  datai = (int *)CALLOC(SND_IO_DATS + SND_AREF_HEADER_SIZE + hdr->chans + 2,sizeof(int)); /* why the +2?? */
  datai[SND_IO_DIR] = direction;
  datai[SND_IO_FD] = fd;
  datai[SND_IO_CHANS] = hdr->chans;
  datai[SND_IO_SIZE] = chansize;
  datai[SND_IO_BEG] = 0;
  datai[SND_IO_END] = bufsize-1;
  datai[SND_IO_BUFSIZ] = bufsize;
  datai[SND_IO_DATA_START] = 0; 
  datai[SND_IO_DATA_END] = 0;
  datai[SND_IO_HDR_END] = hdr->data_location; 
  datai[SND_IO_DATS+SND_AREF_BLOCK]=SND_IO_DATS + SND_AREF_HEADER_SIZE;
  datai[SND_IO_DATS+SND_AREF_SIZE]=hdr->chans;
  if (direction == SND_IO_IN_FILE)
    {
      datai[SND_IO_DATS + SND_AREF_HEADER_SIZE + chan] = (int)(MUS_MAKE_SAMPLE_ARRAY(bufsize));
    }
  else
    {
      for (i=0;i<hdr->chans;i++) 
	datai[SND_IO_DATS + SND_AREF_HEADER_SIZE+i] = (int)(MUS_MAKE_SAMPLE_ARRAY(bufsize));
    }
  if (direction == SND_IO_IN_FILE) mus_file_reset(0,datai,datai); /* get ready to read -- we're assuming mus_file_read_chans here */
  return(datai);
}

int *free_file_state(int *datai)
{
  /* gotta free the IO buffers as well as the descriptor buffer */
  int i,chans;
  if (datai)
    {
      chans = datai[SND_IO_CHANS];
      for (i=0;i<chans;i++)
	{
	  if (datai[SND_IO_DATS + SND_AREF_HEADER_SIZE+i]) MUS_FREE_SAMPLE_ARRAY(datai[SND_IO_DATS + SND_AREF_HEADER_SIZE+i]);
	}
      FREE(datai);
    }
  return(NULL);
}

/* mus_header_read here (or stripped-down equivalent) was very slow, and is just as easy to
 * fool as an extension check (file might start with the word ".snd" or whatever).
 */

static dir *make_dir (char *name)
{
  dir *dp;
  dp = (dir *)CALLOC(1,sizeof(dir));
  dp->files = (char **)CALLOC(32,sizeof(char *));
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
      for (i=0;i<dp->len;i++) {if (dp->files[i]) FREE(dp->files[i]);}
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
      dp->files = (char **)REALLOC(dp->files,dp->size*sizeof(char *));
      for (i=dp->size-32;i<dp->size;i++) dp->files[i] = NULL;
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
	sound_file_extensions = (char **)CALLOC(sound_file_extensions_size,sizeof(char *));
      else sound_file_extensions = (char **)REALLOC(sound_file_extensions,sound_file_extensions_size * sizeof(char *));
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

dir *find_sound_files_in_dir (char *name)
{
#if (!HAVE_OPENDIR)
  return(NULL);
#else
  struct dirent *dirp;
  DIR *dpos;
  char *dot,*sp;
  dir *dp = NULL;
  int i;
  if ((dpos=opendir(name)) != NULL)
    {
      dp = make_dir(name);
      while ((dirp=readdir(dpos)) != NULL)
	{
	  if (dirp->d_name[0] != '.')
	    {
	      dot = NULL;
	      for (sp=dirp->d_name;(*sp) != '\0';sp++) if ((*sp) == '.') dot=(++sp);
	      if (dot)
		{
		  for (i=0;i<sound_file_extensions_end;i++)
		    {
		      if (strcmp(dot,sound_file_extensions[i]) == 0)
			{
			  add_snd_file_to_dir_list(dp,dirp->d_name);
			  break;
			}
		    }
		}
	    }
	}
#if defined(CLOSEDIR_VOID)
      closedir(dpos);
#else
      if (closedir(dpos) != 0) snd_error("%s[%d] %s: closedir failed!",__FILE__,__LINE__,__FUNCTION__);
#endif
    }
  return(dp);
#endif
}

#if FILE_PER_CHAN
dir *all_files_in_dir (char *name)
{
#if (!HAVE_OPENDIR)
  return(NULL);
#else
  struct dirent *dirp;
  DIR *dpos;
  dir *dp = NULL;
  if ((dpos=opendir(name)) != NULL)
    {
      dp = make_dir(name);
      while ((dirp=readdir(dpos)) != NULL)
	{
	  if (dirp->d_name[0] != '.')
	    add_snd_file_to_dir_list(dp,dirp->d_name);
	}
#if defined(CLOSEDIR_VOID)
      closedir(dpos);
#else
      if (closedir(dpos) != 0) snd_error("%s[%d] %s: closedir failed!",__FILE__,__LINE__,__FUNCTION__);
#endif
    }
  return(dp);
#endif
}
#endif


#if DEBUGGING
int temp_files_in_tmpdir(snd_state *ss)
{
  /* at exit, after deleting temp files, look to see if any are left */
#if (!HAVE_OPENDIR)
  return(0);
#else
  struct dirent *dirp;
  DIR *dpos;
  char *tmpdir = NULL;
  tmpdir = temp_dir(ss);
  if (tmpdir == NULL)
    {
      tmpdir = getenv("TMPDIR");
  #ifdef CCRMA
      if (tmpdir == NULL) tmpdir = "/zap";
  #else
    #ifdef P_tmpdir
      if (tmpdir == NULL) tmpdir = P_tmpdir; /* /usr/include/stdio.h */
    #else
      if (tmpdir == NULL) tmpdir = "/tmp";
    #endif
  #endif
    }
  if (tmpdir != NULL)
    {
      if ((dpos = opendir(tmpdir)) != NULL)
	{
	  while ((dirp=readdir(dpos)) != NULL)
	    {
	      if ((snd_strlen(dirp->d_name) > 4) && (strncmp(dirp->d_name,"snd_",4) == 0))
		{
		  closedir(dpos);
		  return(1);
		}
	    }
	}
      closedir(dpos);
    }
  return(0);
#endif
}
#endif


static int names_match(char *filename, char *pattern)
{
  /* just "*" for wildcards here */
  char *sn,*sp;
  sn = filename;
  sp = pattern;
  if ((!sn) || (!sp)) {if ((sn) || (sp)) return(0); else return(1);}
  while ((*sn) && (*sp))
    {
      if ((*sp) == '*') 
	{
	  sp++; 
	  while ((*sp) == '*') {sp++;} 
	  if (!(*sp)) return(1);
	  while ((*sn) && ((*sn) != (*sp))) {sn++;}
	  if (!(*sn)) return(0);
	}
      else 
	{
	  if ((*sn) != (*sp)) return(0);
	  sn++; sp++;
	}
    }
  return(1);
}

dir *filter_sound_files(dir *dp, char *pattern)
{
  int i;
  dir *ndp;
  ndp = make_dir("");
  for (i=0;i<dp->len;i++)
    {
      if (names_match(dp->files[i],pattern)) {add_snd_file_to_dir_list(ndp,dp->files[i]);}
    }
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
  sptr->names[sptr->active_sounds] = sp->fullname;
  sptr->sounds[sptr->active_sounds] = sp->index;
  (sptr->active_sounds)++;
  return(0); /*assume no problem -- nothing can go wrong! */
}

static char title_buffer[4*(MUS_MAX_FILE_NAME)];

static void reflect_file_change_in_title(snd_state *ss)
{
  active_sound_list *alist;
  int i,j;
  alist = (active_sound_list *)CALLOC(1,sizeof(active_sound_list));
  alist->sounds = (int *)CALLOC(ss->max_sounds,sizeof(int));
  alist->names = (char **)CALLOC(ss->max_sounds,sizeof(char *));
  map_over_sounds(ss,add_sound_to_active_list,alist);
  sprintf(title_buffer,"%s%s",ss->startup_title,((alist->active_sounds > 0) ? ": " : ""));
  if (alist->active_sounds > 0)
    {
      if (alist->active_sounds < 4) j=alist->active_sounds; else j=4;
      for (i=0;i<j;i++)
	{
	  strcat(title_buffer,filename_without_home_directory(alist->names[i]));
	  if (i<j-1) strcat(title_buffer,", ");
	}
      if (alist->active_sounds>4) strcat(title_buffer,"...");
    }
  set_title(ss,title_buffer);
  FREE(alist->sounds);
  FREE(alist->names);
  FREE(alist);
}

static char *memo_file_name(snd_info *sp)
{
  char *newname;
  int len;
  len = strlen(sp->fullname);
  newname = (char *)CALLOC(len+5,sizeof(char));
  strcpy(newname,sp->fullname);
  newname[len]='.'; newname[len+1]='s'; newname[len+2]='c'; newname[len+3]='m'; newname[len+4]='\0'; 
  return(newname);
}

static void read_memo_file(snd_info *sp)
{
  /* sp->fullname + ".scm" = possible memo file */
  char *newname;
  newname = memo_file_name(sp);
  if (file_write_date(newname) >= sp->write_date)
    snd_load_file(newname);
  FREE(newname);
}

#if HAVE_GUILE
#include "sg.h"

static SCM memo_sound,open_hook,close_hook;

#if  (!HAVE_GUILE_1_3_0)
static int dont_open(snd_state *ss, char *file)
{
  char *mcf = NULL;
  SCM res = SCM_BOOL_F;
  if (!(ss->open_hook_active))
    {
      if (HOOKED(open_hook))
	{
	  ss->open_hook_active = 1;
	  res = g_c_run_or_hook(open_hook,SCM_LIST1(gh_str02scm(mcf = mus_file_full_name(file))));
	  if (mcf) FREE(mcf);
	  ss->open_hook_active = 0;
	}
    }
  return(SCM_TRUE_P(res));
}

static int dont_close(snd_state *ss, snd_info *sp)
{
  SCM res = SCM_BOOL_F;
  if (!(ss->close_hook_active))
    {
      if (HOOKED(close_hook))
	{
	  ss->close_hook_active = 1;
	  res = g_c_run_or_hook(close_hook,SCM_LIST1(gh_int2scm(sp->index)));
	  ss->close_hook_active = 0;
	}
    }
  return(SCM_TRUE_P(res));
}

#else
  static int dont_open(snd_state *ss, char *file) {return(0);}
  static int dont_close(snd_state *ss, snd_info *sp) {return(0);}
#endif
#else
  static int dont_open(snd_state *ss, char *file) {return(0);}
  static int dont_close(snd_state *ss, snd_info *sp) {return(0);}
#endif


static snd_info *snd_open_file_1 (char *filename, snd_state *ss, int select)
{
  snd_info *sp;
  char *mcf = NULL;
  int files,val;
  if (dont_open(ss,filename)) return(NULL);
  sp = add_sound_window(mcf = mus_file_full_name(filename),ss); /* snd-xsnd.c -> make_file_info */
  if (mcf) FREE(mcf);
  if (sp)
    {
#if HAVE_GUILE
      SCM_SETCDR(memo_sound,gh_int2scm(sp->index));
#endif
      sp->write_date = file_write_date(sp->fullname);
      sp->need_update = 0;
      if (ss->viewing) sp->read_only = 1;
      ss->active_sounds++;
      files = ss->active_sounds;
      if (files == 1) reflect_file_open_in_menu();
      reflect_normalize_in_menu(active_channels(ss,0) > 1);
      reflect_file_change_in_title(ss);
      unlock_ctrls(sp);
      greet_me(ss,sp->shortname);
    }
  map_over_separate_chans(ss,channel_open_pane,NULL);
  map_over_separate_chans(ss,channel_unlock_pane,NULL);
  ss->viewing = 0;
  if (sp) 
    {
      if (select) select_channel(sp,0);
      read_memo_file(sp);
      if ((sp->combining != CHANNELS_SEPARATE) && (sp->nchans > 1)) 
	{
	  val = sp->combining;
	  sp->combining = CHANNELS_SEPARATE; 
	  if (val == CHANNELS_COMBINED)
	    combine_sound(sp);
	  else superimpose_sound(sp);
	}
    }

  return(sp);
}

snd_info *snd_open_file (char *filename, snd_state *ss) {return(snd_open_file_1(filename,ss,TRUE));}
snd_info *snd_open_file_unselected (char *filename, snd_state *ss) {return(snd_open_file_1(filename,ss,FALSE));}


void snd_close_file(snd_info *sp, snd_state *ss)
{
  int files;
  if (dont_close(ss,sp)) return;
  sp->inuse = 0;
  remember_me(ss,sp->shortname,sp->fullname);
  if (sp->playing) stop_playing_sound(sp);
  clear_minibuffer(sp);
  if ((region_ok(0)) && (selection_member(sp))) 
    {
      cancel_keyboard_selection();
      deactivate_selection();
    }
  if (sp == selected_sound(ss)) ss->selected_sound = NO_SELECTION;
  free_snd_info(sp);
  ss->active_sounds--;
  files = ss->active_sounds;
  if (files == 0) reflect_file_lack_in_menu();
  reflect_file_change_in_title(ss);
  reflect_normalize_in_menu(active_channels(ss,0) > 1);
}


int copy_file(char *oldname, char *newname)
{
  /* make newname a copy of oldname */
  int ifd,ofd;
  long bytes,wb,total;
  char *buf = NULL;
  total = 0;
  ifd = open(oldname,O_RDONLY,0);
  if (ifd == -1) return(SND_CANNOT_FIND_FILE);
  ofd = creat(newname,0666);
  if (ofd == -1) {close(ifd); return(SND_CANNOT_WRITE_DATA);}
  buf = (char *)CALLOC(8192,sizeof(char));
  while ((bytes = read(ifd,buf,8192)))
    {
      total += bytes;
      wb = write(ofd,buf,bytes);
      if (wb != bytes) {close(ofd); close(ifd); FREE(buf); return(SND_CANNOT_WRITE_DATA);}
    }
  close(ifd);
  total = total >> 10;
  wb = disk_kspace(ofd);
  if (wb < 0) 
    snd_error(strerror(errno));
  else
    if (total > wb) snd_error("disk nearly full: used %d Kbytes leaving %d",(int)total,(int)wb);
  FREE(buf);
  close(ofd);
  return(SND_NO_ERROR);
}

int snd_copy_file(char *oldfile, char *newfile)
{
  int err;
  err = 0;
  if ((err = (rename(oldfile,newfile))))
    {
      if (errno == EXDEV)
	{
	  err = copy_file(oldfile,newfile);
	  if (!err) remove(oldfile);
	}
    }
  if (err != 0)
    snd_error("trouble overwriting %s: %s",newfile,strerror(errno));
  return(err);
}


snd_info *make_sound_readable(snd_state *ss, char *filename, int post_close)
{
  /* conjure up just enough Snd structure to make this sound readable by the edit-tree readers */
  snd_info *sp;
  chan_info *cp;
  file_info *hdr;
  snd_data *sd;
  int *datai;
  int i,fd,len;
  hdr = make_file_info_1(filename,ss);
  if (!hdr) return(NULL);
  sp = (snd_info *)CALLOC(1,sizeof(snd_info));
  sp->s_type = SND_INFO;
  sp->nchans = mus_sound_chans(filename);
  sp->allocated_chans = sp->nchans;
  sp->chans = (chan_info **)CALLOC(sp->nchans,sizeof(chan_info *));
  sp->hdr = hdr;
  sp->inuse = 1;
  sp->state = ss;
  sp->expand = 1.0;
  sp->expanding = 0;
  sp->amp = 1.0;
  sp->srate = 1.0;
  sp->play_direction = 1;
  sp->contrasting = 0;
  sp->contrast = 0.0;
  sp->reverbing = 0;
  sp->revscl = 0.0;
  sp->filtering = 0;
  sp->index = -2;
  sp->sgx = NULL;
  len = (hdr->samples)/(hdr->chans);
  for (i=0;i<sp->nchans;i++)
    {
      cp = make_chan_info(NULL,i,sp,ss);
      FREE((cp->cgx)->ax);
      FREE(cp->cgx);
      cp->cgx = NULL;
      sp->chans[i] = cp;
      add_channel_data_1(cp,sp,ss,0);
      set_initial_ed_list(cp,len-1);
      cp->edit_size = 1;
      cp->sound_size = 1;
      fd = snd_open_read(ss,filename);
      mus_file_set_descriptors(fd,filename,hdr->format,mus_sound_datum_size(filename),hdr->data_location,hdr->chans,hdr->type);
      datai = make_file_state(fd,hdr,SND_IO_IN_FILE,i,(post_close) ? MAX_BUFFER_SIZE : MIX_FILE_BUFFER_SIZE);
      cp->sounds[0] = make_snd_data_file(filename,datai,
					 MUS_SAMPLE_ARRAY(datai[SND_IO_DATS + SND_AREF_HEADER_SIZE+i]),
					 copy_header(hdr->name,hdr),
					 DONT_DELETE_ME,cp->edit_ctr,i);
      if (post_close) {snd_close(fd); sd = cp->sounds[0]; sd->open = FD_CLOSED; datai[SND_IO_FD] = -1;}
      /* this is not as crazy as it looks -- we've read in the first 64K (or whatever) samples,
       * and may need this file channel for other opens, so this file can be closed until mus_file_reset
       */
    }
  return(sp);
}

static snd_info *snd_update_1(snd_state *ss, snd_info *sp, char *ur_filename)
{
  /* we can't be real smart here because the channel number may have changed and so on */
  Float *axis_data;
  int *ffts,*waves;
  int i,j,old_chans,old_sync,old_combine,need_update;
  int raw_def;
  Float duration;
  chan_info *cp;
  axis_info *ap;
  snd_info *nsp;
  char *filename;
  need_update = 0;
  filename = copy_string(ur_filename);
  old_chans = sp->nchans;
  old_sync = sp->syncing;
  old_combine = sp->combining;
  axis_data = (Float *)CALLOC(4*old_chans,sizeof(Float));
  ffts = (int *)CALLOC(old_chans,sizeof(int));
  waves = (int *)CALLOC(old_chans,sizeof(int));
  for (i=0;i<old_chans;i++)
    {
      cp = sp->chans[i];
      ap = cp->axis;
      axis_data[(i*4)+0]=ap->x0;
      axis_data[(i*4)+1]=ap->x1;
      axis_data[(i*4)+2]=ap->y0;
      axis_data[(i*4)+3]=ap->y1;
      waves[i] = cp->waving;
      ffts[i] = cp->ffting;
    }
  snd_close_file(sp,ss);
  /* this normalizes the fft/lisp/wave state so we need to reset it after reopen */
  alert_new_file();
  /* if it's a raw sound file being updated, we don't want to re-confirm the sound format and whatnot
   * so we set the use-raw-defaults flag in a sort of ugly wrapper around the snd_open_file
   */
  raw_def = use_raw_defaults(ss);
  set_use_raw_defaults(ss,1);
  nsp = snd_open_file(filename,ss);
  set_use_raw_defaults(ss,raw_def);
  /* end wrapper */
  duration = (Float)mus_sound_samples(filename) / (Float)(mus_sound_chans(filename) * mus_sound_srate(filename));
  for (i=0,j=0;i<nsp->nchans;i++)
    {
      cp = nsp->chans[i];
      if (duration < axis_data[(j*4)+0]) axis_data[(j*4)+0]=duration-.1;
      if (duration < axis_data[(j*4)+1]) axis_data[(j*4)+1]=duration;
      set_axes(cp,axis_data[(j*4)+0],axis_data[(j*4)+1],axis_data[(j*4)+2],axis_data[(j*4)+3]);
      update_graph(cp,NULL); /* get normalized state before messing with it */
      if (ffts[j]) {fftb(cp,TRUE); need_update = 1;}
      if (!(waves[j])) {waveb(cp,FALSE); need_update = 1;}
      if (j<(old_chans-1)) j++;
    }
  if (nsp->combining != old_combine) combineb(nsp,old_combine);
  if (nsp->syncing != old_sync) syncb(nsp,old_sync);
  if (need_update) {for (i=0;i<nsp->nchans;i++) update_graph(nsp->chans[i],NULL);}
  FREE(axis_data);
  FREE(waves);
  FREE(ffts);
  FREE(filename);
  return(nsp);
}

void snd_update(snd_state *ss, snd_info *sp)
{
  char *buf;
  if (sp->edited_region) return;
  if ((snd_probe_file(sp->fullname)) == FILE_DOES_NOT_EXIST)
    {
      /* user deleted file while editing it? */
      buf = (char *)CALLOC(256,sizeof(char));
      sprintf(buf,"%s no longer exists!",sp->shortname);
      report_in_minibuffer(sp,buf);
      FREE(buf);
      return;
    }
  save_window_size(ss);
  sp = snd_update_1(ss,sp,sp->fullname);
  buf = (char *)CALLOC(64,sizeof(char));
  sprintf(buf,"updated %s",sp->shortname);
  report_in_minibuffer(sp,buf);
  FREE(buf);
  restore_window_size(ss);
}

char *update_chan_stats(chan_info *cp)
{
  char *desc;
  char *vals[8];
  int i;
  desc = (char *)CALLOC(256,sizeof(char));
  vals[0] = kmg(cp->stats[ARRAY_USAGE]);
  vals[1] = kmg(cp->stats[ARRAYS_ACTIVE]);
  vals[2] = kmg(cp->stats[FILE_USAGE]);
  vals[3] = kmg(cp->stats[TEMP_USAGE]);
  vals[4] = kmg(cp->stats[TEMPS_ACTIVE]);
  vals[5] = kmg(cp->stats[TEMPS_OPEN]);
  vals[6] = kmg(cp->stats[AMP_ENVS_ACTIVE]);
  vals[7] = kmg(cp->stats[AMP_ENV_USAGE]);
  if (cp->stats[TEMPS_ACTIVE] != cp->stats[TEMPS_OPEN])
    sprintf(desc,"%s %d: %s (%s), %s, %s (%s, %s), %s %s\n",(cp->sound)->shortname,cp->chan + 1,
	    vals[0],vals[1],vals[2],vals[3],vals[4],vals[5],vals[6],vals[7]);
  else
    sprintf(desc,"%s %d: %s (%s), %s, %s (%s), %s %s\n",(cp->sound)->shortname,cp->chan + 1,
	    vals[0],vals[1],vals[2],vals[3],vals[4],vals[6],vals[7]);
  for (i=0;i<8;i++) free(vals[i]);
  return(desc);
}

/* files lists -- it was probably a mistake to try to split this out of the graphics files */

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

char *get_curnames(int n) {return(curnames[n]);}
char *get_prevnames(int n) {return(prevnames[n]);}
char *get_prevfullnames(int n) {return(prevfullnames[n]);}
int get_a_big_star(int n) {return(a_big_star[n]);}
void set_a_big_star(int n,int i) {a_big_star[n]=i;}
int get_max_prevfile_end(void) {return(max_prevfile_end);}
void set_max_prevfile_end(int n) {max_prevfile_end=n;}
int get_prevfile_end(void) {return(prevfile_end);}
int get_max_curfile_end(void) {return(max_curfile_end);}
void set_max_curfile_end(int n) {max_curfile_end=n;}
int get_curfile_end(void) {return(curfile_end);}
int get_curfile_size(void) {return(curfile_size);}
int get_prevfile_size(void) {return(prevfile_size);}

void init_curfiles(int size)
{
  curfile_size = size;
  curnames = (char **)CALLOC(curfile_size,sizeof(char *));
  a_big_star = (int *)CALLOC(curfile_size,sizeof(int *));
}

void init_prevfiles(int size)
{
  prevfile_size = size;
  prevnames = (char **)CALLOC(prevfile_size,sizeof(char *));
  prevfullnames = (char **)CALLOC(prevfile_size,sizeof(char *));
  prevtimes = (int *)CALLOC(prevfile_size,sizeof(char *));
}

int find_curfile_regrow(char *shortname)
{
  int i;
  for (i=0;i<curfile_end;i++)
    {
      if (strcmp(curnames[i],shortname) == 0) return(i);
    }
  return(-1);
}

int find_prevfile_regrow(char *shortname)
{
  int i;
  for (i=0;i<=prevfile_end;i++)
    {
      if (strcmp(prevnames[i],shortname) == 0) return(i);
    }
  return(-1);
}

void save_prevlist(FILE *fd)
{
  int i;
  for (i=0;i<=prevfile_end;i++)
    fprintf(fd,"(%s \"%s\")\n",S_preload_file,prevfullnames[i]);
}

void file_uncurlist(char *filename)
{
  int i,j;
  i = find_curfile_regrow(filename);
  if (i != -1)
    {
      if (curnames[i]) FREE(curnames[i]);
      curnames[i] = NULL;
      for (j=i;j<curfile_end-1;j++)
	{
	  curnames[j] = curnames[j+1];
	  a_big_star[j] = a_big_star[j+1];
	}
      curnames[curfile_end-1] = NULL;
      curfile_end--;
    }
}

void file_unprevlist(char *filename)
{
  int i,j;
  i = find_prevfile_regrow(filename);
  if (i != -1)
    {
      FREE(prevnames[i]);
      prevnames[i] = NULL;
      FREE(prevfullnames[i]);
      prevfullnames[i] = NULL;
      for (j=i;j<prevfile_end;j++) 
	{
	  prevnames[j] = prevnames[j+1];
	  prevfullnames[j] = prevfullnames[j+1]; 
	  prevtimes[j] = prevtimes[j+1];
	}
      prevnames[prevfile_end] = NULL; 
      prevfile_end--;
    }
}

void clear_prevlist(void)
{
  int i;
  for (i=0;i<=prevfile_end;i++)
    {
      if (prevnames[i]) 
	{
	  FREE(prevnames[i]); prevnames[i]=NULL;
	  FREE(prevfullnames[i]); prevfullnames[i]=NULL;
	}
    }
  prevfile_end = -1;
  prevtime = 0;
}

void update_prevlist(void)
{
  /* here we need the file's full name */
  int i,j,fd;
  for (i=0;i<=prevfile_end;i++)
    {
      if (prevnames[i]) 
	{
	  fd = open(prevfullnames[i],O_RDONLY,0);
	  if (fd == -1) 
	    {
	      FREE(prevnames[i]); prevnames[i]=NULL;
	      FREE(prevfullnames[i]); prevfullnames[i]=NULL;
	    }
	  else close(fd);
	}
    }
  for (i=0,j=0;i<=prevfile_end;i++)
    {
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
    }
  prevfile_end = j-1;
}

void file_curlist(char *filename)
{
  int i,new_size;
  if (curfile_end == curfile_size)
    {
      new_size = curfile_size+32;
      if (curfile_size == 0)
	{
	  curnames = (char **)CALLOC(new_size,sizeof(char *));
	  a_big_star = (int *)CALLOC(new_size,sizeof(int *));
	}
      else
	{
	  curnames = (char **)REALLOC(curnames,new_size * sizeof(char *));
	  a_big_star = (int *)REALLOC(a_big_star,new_size * sizeof(int *));
	  for (i=curfile_size;i<new_size;i++) {curnames[i] = NULL; a_big_star[i] = 0;}
	}
      make_cur_name_row(curfile_size,new_size);
      curfile_size = new_size;
    }
  curnames[curfile_end] = copy_string(filename);
  a_big_star[curfile_end] = 0;
  curfile_end++;
  if (max_curfile_end < curfile_end) max_curfile_end = curfile_end;
}

void file_prevlist(char *filename, char *fullname)
{
  int k,i,new_size;
  prevfile_end++;
  if (prevfile_end == prevfile_size)
    {
      new_size = prevfile_size+32;
      if (prevfile_size == 0)
	{
	  prevnames = (char **)CALLOC(new_size,sizeof(char *));
	  prevfullnames = (char **)CALLOC(new_size,sizeof(char *));
	  prevtimes = (int *)CALLOC(new_size,sizeof(char *));
	}
      else
	{
	  prevnames = (char **)REALLOC(prevnames,new_size * sizeof(char *));
	  prevfullnames = (char **)REALLOC(prevfullnames,new_size * sizeof(char *));
	  prevtimes = (int *)REALLOC(prevtimes,new_size * sizeof(int *));
	  for (i=prevfile_size;i<new_size;i++) {prevnames[i] = NULL; prevfullnames[i] = NULL; prevtimes[i] = 0;}
	}
      make_prev_name_row(prevfile_size,new_size);
      prevfile_size = new_size;
    }
  for (k=prevfile_end;k>0;k--) 
    {
      prevnames[k]=prevnames[k-1]; 
      prevfullnames[k]=prevfullnames[k-1];
      prevtimes[k] = prevtimes[k-1];
    }
  prevnames[0] = copy_string(filename);
  prevfullnames[0] = copy_string(fullname);
  prevtimes[0] = prevtime++;
  if (max_prevfile_end < prevfile_end) max_prevfile_end = prevfile_end;
}

void add_directory_to_prevlist_1(snd_state *ss, char *dirname)
{
  dir *sound_files = NULL;
  char *fullpathname = NULL;
  char **fullnames;
  int i,end;
  sound_files = find_sound_files_in_dir(dirname);
  if ((sound_files) && (sound_files->len > 0))
    {
      fullpathname = (char *)CALLOC(FILENAME_MAX,sizeof(char));
      strcpy(fullpathname,dirname);
      if (dirname[strlen(dirname)-1] != '/') strcat(fullpathname,"/");
      end = strlen(fullpathname);
      fullnames = (char **)CALLOC(sound_files->len,sizeof(char *));
      for (i=0;i<sound_files->len;i++) 
	{
	  fullnames[i] = copy_string(strcat(fullpathname,sound_files->files[i]));
	  fullpathname[end] = '\0';
	}
      add_files_to_prevlist(ss,sound_files->files,fullnames,sound_files->len);
      for (i=0;i<sound_files->len;i++) {FREE(fullnames[i]); fullnames[i]=NULL;}
      FREE(fullnames);
      free_dir(sound_files);
      FREE(fullpathname);
    }
}

/* sort prevfiles list by name (aphabetical), or some number (date written, size, entry order, srate? type?) */

typedef struct {
  int vals,times;
  char *a1,*a2;
} heapdata;

#if (!HAVE_GSL)
static void snd_heapsort(unsigned int n, int choice, heapdata **data)
{
  /* sort a1 or vals, parallel sort in a2 */
  unsigned int i,j,ir,k;
  int val;
  heapdata *curval;
  if (n<2) return;
  k=(n>>1)+1;
  ir=n;
  for (;;)
    {
      if (k>1)
	{
	  k--;
	  curval = data[k-1];
	}
      else
	{
	  curval = data[ir-1];
	  data[ir-1] = data[0];
	  ir--;
	  if (ir == 1)
	    {
	      data[0] = curval;
	      break;
	    }
	}
      i=k;
      j=k+1;
      while (j<=ir)
	{
	  if (j<ir) 
	    {
	      switch (choice)
		{
		case ALPHABET: if (strcmp(data[j-1]->a1,data[j]->a1) < 0) j++; break;
		case VALS_GREATER: if (data[j-1]->vals < data[j]->vals) j++; break;
		case VALS_LESS: if (data[j-1]->vals > data[j]->vals) j++; break;
		}
	    }
	  val = 0;
	  switch (choice)
	    {
	    case ALPHABET: val = (strcmp(curval->a1,data[j-1]->a1) < 0); break;
	    case VALS_GREATER: val = (curval->vals < data[j-1]->vals); break;
	    case VALS_LESS: val = (curval->vals > data[j-1]->vals); break;
	    }
	  if (val)
	    {
	      data[i-1] = data[j-1];
	      i=j;
	      j<<=1;
	    }
	  else j=ir+1;
	}
      data[i-1] = curval;
    }
}
#else

#include <gsl/gsl_heapsort.h>

static int alphabet_compare(const void *a, const void *b)
{
  heapdata *d1 = *(heapdata **)a;
  heapdata *d2 = *(heapdata **)b;
  return(strcmp(d1->a1,d2->a1));
}

static int greater_compare(const void *a, const void *b)
{
  heapdata *d1 = *(heapdata **)a;
  heapdata *d2 = *(heapdata **)b;
  if (d1->vals > d2->vals) return(1); else if (d1->vals == d2->vals) return(0); else return(-1);
}

static int less_compare(const void *a, const void *b)
{
  heapdata *d1 = *(heapdata **)a;
  heapdata *d2 = *(heapdata **)b;
  if (d1->vals < d2->vals) return(1); else if (d1->vals == d2->vals) return(0); else return(-1);
}

static void snd_heapsort(unsigned int n, int choice, heapdata **data)
{
  switch (choice)
    {
    case ALPHABET:     gsl_heapsort((void *)data,n,sizeof(heapdata *),(gsl_comparison_fn_t) & alphabet_compare); break;
    case VALS_GREATER: gsl_heapsort((void *)data,n,sizeof(heapdata *),(gsl_comparison_fn_t) & greater_compare); break;
    case VALS_LESS:    gsl_heapsort((void *)data,n,sizeof(heapdata *),(gsl_comparison_fn_t) & less_compare); break;
    }
}
#endif

void make_prevfiles_list_1(snd_state *ss)
{
  heapdata **data;
  int i,len;
  if (prevfile_end >= 0)
    {
      len = prevfile_end+1;
      data = (heapdata **)CALLOC(len,sizeof(heapdata *));
      for (i=0;i<len;i++)
	{
	  data[i] = (heapdata *)CALLOC(1,sizeof(heapdata));
	  data[i]->a1 = prevnames[i];
	  data[i]->a2 = prevfullnames[i];
	  data[i]->times = prevtimes[i];
	}
      switch (previous_files_sort(ss))
	{
	case 0: 
	  break;
	case 1: 
	  snd_heapsort(prevfile_end+1,ALPHABET,data);
	  break;
	case 2:
	  for (i=0;i<=prevfile_end;i++) data[i]->vals = file_write_date(get_prevfullnames(i));
	  snd_heapsort(prevfile_end+1,VALS_LESS,data);
	  break;
	case 3:
	  for (i=0;i<=prevfile_end;i++) data[i]->vals = mus_sound_samples(get_prevfullnames(i));
	  snd_heapsort(prevfile_end+1,VALS_GREATER,data);
	  break;
	case 4:
	  for (i=0;i<=prevfile_end;i++) data[i]->vals = prevtimes[i];
	  snd_heapsort(prevfile_end+1,VALS_LESS,data);
	  break;
	}
      for (i=0;i<len;i++)
	{
	  prevnames[i] = data[i]->a1;
	  prevfullnames[i] = data[i]->a2;
	  prevtimes[i] = data[i]->times;
	  FREE(data[i]);
	}
      FREE(data);
    }
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


static char *header_short_names[NUM_HEADER_TYPES] = {"sun  ","aifc ","wave ","raw  ","aiff ","ircam","nist "};
char *header_short_name(int i) {if ((i>=0) && (i<NUM_HEADER_TYPES)) return(header_short_names[i]); else return("unknown type");}

static char *next_data_formats[NUM_NEXT_FORMATS] = {"short","mulaw","signed byte  ","float","long","alaw","24-bit","double"};
static char *ircam_data_formats[NUM_IRCAM_FORMATS] = {"short","mulaw","float        ","long","alaw"};
static char *wave_data_formats[NUM_WAVE_FORMATS] = {"mulaw","alaw","unsigned byte","short","long","float","24-bit"};
static char *aifc_data_formats[NUM_AIFC_FORMATS] = {"short","mulaw","signed byte  ","long","alaw","24 bit",
						    "float","double","unsigned byte","short swapped",
						    "long swapped","24-bit swapped","unsigned short"};
static char *aiff_data_formats[NUM_AIFF_FORMATS] = {"short","long","signed byte","24 bit"};
static char *nist_data_formats[NUM_NIST_FORMATS] = {"BE short","LE short","BE int","LE int","8-bit","BE 24-bit","LE 24-bit"};
static char *raw_data_formats[NUM_RAW_FORMATS] =   {"BE short", "mulaw","byte","BE float","BE int","alaw","char","BE 24-bit",
						    "BE double","LE short","LE int","LE float","LE double","BE unsigned short",
						    "LE unsigned short","LE 24-bit","BE int normalized","LE int normalized"};

static int next_dfs[NUM_NEXT_FORMATS] = {MUS_BSHORT,MUS_MULAW,MUS_BYTE,MUS_BFLOAT,MUS_BINT,MUS_ALAW,
					 MUS_B24INT,MUS_BDOUBLE};
static int ircam_dfs[NUM_IRCAM_FORMATS] = {MUS_BSHORT,MUS_MULAW,MUS_BFLOAT,MUS_BINT,MUS_ALAW};
static int wave_dfs[NUM_WAVE_FORMATS] = {MUS_MULAW,MUS_ALAW,MUS_UBYTE,MUS_LSHORT,
					 MUS_LINT,MUS_LFLOAT,MUS_L24INT};
static int aifc_dfs[NUM_AIFC_FORMATS] = {MUS_BSHORT,MUS_MULAW,MUS_BYTE,MUS_BINT,MUS_ALAW,MUS_B24INT,
					 MUS_BFLOAT,MUS_BDOUBLE,MUS_UBYTE,MUS_LSHORT,
					 MUS_LINT,MUS_L24INT,MUS_UBSHORT};
static int aiff_dfs[NUM_AIFF_FORMATS] = {MUS_BSHORT,MUS_BINT,MUS_BYTE,MUS_B24INT};
static int nist_dfs[NUM_NIST_FORMATS] = {MUS_BSHORT,MUS_LSHORT,MUS_BINT,MUS_LINT,
					 MUS_BYTE,MUS_B24INT,MUS_L24INT};
static int raw_dfs[NUM_RAW_FORMATS] = {MUS_BSHORT,MUS_MULAW,MUS_BYTE,MUS_BFLOAT,MUS_BINT,MUS_ALAW,
				       MUS_UBYTE,MUS_B24INT,MUS_BDOUBLE,MUS_LSHORT,MUS_LINT,
				       MUS_LFLOAT,MUS_LDOUBLE,MUS_UBSHORT,MUS_ULSHORT,
				       MUS_L24INT,MUS_BINTN,MUS_LINTN};

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
char *data_format_name(int i) {if ((i>=0) && (i<NUM_DATA_FORMATS)) return(data_formats[i]); else return("unknown format");}

/* must parallel sndlib.h definitions */

int header_type_from_position(int position)
{
  switch (position)
    {
    case NEXT_POSITION: return(MUS_NEXT); break;
    case AIFC_POSITION: return(MUS_AIFC); break;
    case RIFF_POSITION: return(MUS_RIFF); break;
    case IRCAM_POSITION: return(MUS_IRCAM); break;
    case RAW_POSITION: return(MUS_RAW); break;
    case AIFF_POSITION: return(MUS_AIFF); break;
    case NIST_POSITION: return(MUS_NIST); break;
    }
  return(MUS_RAW);
}

int data_format_from_position(int header, int position)
{
  switch (header)
    {
    case MUS_NEXT: return(next_dfs[position]); break;
    case MUS_AIFC: return(aifc_dfs[position]); break;
    case MUS_RIFF: return(wave_dfs[position]); break;
    case MUS_IRCAM: return(ircam_dfs[position]); break;
    case MUS_RAW: return(raw_dfs[position]); break;
    case MUS_AIFF: return(aiff_dfs[position]); break;
    case MUS_NIST: return(nist_dfs[position]); break;
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
      for (i=0;i<NUM_AIFC_FORMATS;i++) if (format == aifc_dfs[i]) {fdat->format_pos = i; break;}
      break;
    case MUS_RIFF: 
      fdat->formats = NUM_WAVE_FORMATS; 
      fl = wave_data_formats; 
      fdat->header_pos = RIFF_POSITION;
      fdat->format_pos = 3;
      for (i=0;i<NUM_WAVE_FORMATS;i++) if (format == wave_dfs[i]) {fdat->format_pos = i; break;}
      break;
    case MUS_IRCAM: 
      fdat->formats =NUM_IRCAM_FORMATS; 
      fl = ircam_data_formats; 
      fdat->header_pos = IRCAM_POSITION; 
      fdat->format_pos = 0;
      for (i=0;i<NUM_IRCAM_FORMATS;i++) if (format == ircam_dfs[i]) {fdat->format_pos = i; break;}
      break;
    case MUS_NEXT:
      fdat->formats = NUM_NEXT_FORMATS; 
      fl = next_data_formats; 
      fdat->header_pos = NEXT_POSITION; 
      fdat->format_pos = 0;
      for (i=0;i<NUM_NEXT_FORMATS;i++) if (format == next_dfs[i]) {fdat->format_pos = i; break;}
      break;
    case MUS_NIST:
      fdat->formats = NUM_NIST_FORMATS; 
      fl = nist_data_formats; 
      fdat->header_pos = NIST_POSITION; 
      fdat->format_pos = 0;
      for (i=0;i<NUM_NIST_FORMATS;i++) if (format == nist_dfs[i]) {fdat->format_pos = i; break;}
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
      for (i=0;i<NUM_AIFF_FORMATS;i++) if (format == aiff_dfs[i]) {fdat->format_pos = i; break;}
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
    case MUS_NEXT: fdat->formats = NUM_NEXT_FORMATS; formats = next_data_formats; dfs = next_dfs; fdat->header_pos = NEXT_POSITION; break;
    case MUS_NIST: fdat->formats = NUM_NIST_FORMATS; formats = nist_data_formats; dfs = nist_dfs; fdat->header_pos = NIST_POSITION; break;
    case MUS_AIFC: fdat->formats = NUM_AIFC_FORMATS; formats = aifc_data_formats; fdat->header_pos = AIFC_POSITION; dfs = aifc_dfs; break;
    case MUS_RIFF: fdat->formats = NUM_WAVE_FORMATS; formats = wave_data_formats; fdat->header_pos = RIFF_POSITION; dfs = wave_dfs; break;
    case MUS_IRCAM: fdat->formats = NUM_IRCAM_FORMATS; formats = ircam_data_formats; fdat->header_pos = IRCAM_POSITION; dfs = ircam_dfs; break;
    case MUS_RAW: fdat->formats = NUM_RAW_FORMATS; formats = raw_data_formats; fdat->header_pos = RAW_POSITION; dfs = raw_dfs; break;
    case MUS_AIFF: fdat->formats = NUM_AIFF_FORMATS; formats = aiff_data_formats; fdat->header_pos = AIFF_POSITION; dfs = aiff_dfs; break;
    }
  fdat->format_pos = 0;
  for (i=0;i<fdat->formats;i++) if (data_format == dfs[i]) {fdat->format_pos = i; break;}
  return(formats);
}

typedef struct {
  snd_info *sp;
  char *fullname;
  int edits;
} same_name_info;

static int check_for_same_name(snd_info *sp1, void *ur_info)
{
  int i;
  chan_info *cp;
  same_name_info *info = (same_name_info *)ur_info;
  if ((sp1) && (strcmp(sp1->fullname,info->fullname) == 0))
    {
      
      info->sp = sp1;
      for (i=0;i<sp1->nchans;i++) 
	{
	  cp = sp1->chans[i];
	  if (info->edits < cp->edit_ctr) info->edits = cp->edit_ctr;
	}
      return(1); /* stop immediately and deal with this one */
    }
  return(0);
}

int check_for_filename_collisions_and_save(snd_state *ss, snd_info *sp, char *str, int save_type, int srate, int type, int format, char *comment)
{
  same_name_info *collision = NULL;
  char *file_string,*fullname,*ofile;
  int err,result = 0;
  if (sp) clear_minibuffer(sp);
  alert_new_file();
  /* now check in-core files -- need to close any of same name -- if edited what to do? */
  /* also it's possible the new file name is the same as the current file name(!) */
  fullname = mus_file_full_name(str);
  if (!(snd_overwrite_ok(ss,fullname))) {FREE(fullname); return(-1);}
  if (strcmp(fullname,sp->fullname) == 0)
    {
      /* normally save-as saves the current edit tree, merely saving the current state
       * in a separate, presumably inactive file; here we're being asked to overwrite
       * the current file in save-as; we can't have it both ways -- we'll save the edits 
       * in a temp file, then rename/copy the temp, and call update 
       */
      if (sp->read_only)
	{
	  file_string = (char *)CALLOC(256,sizeof(char));
	  sprintf(file_string,"can't save-as %s (%s is write-protected)",fullname,sp->shortname);
	  report_in_minibuffer(sp,file_string);
	  FREE(fullname);
	  FREE(file_string);
	  return(-1);
	}
      /* it's possible also that the same-named file is open in several windows -- for now we'll ignore that */
      /* also what if a sound is write-protected in one window, and not in another? */
      ofile = snd_tempnam(ss); 
      if (save_type == FILE_SAVE_AS)
	result = save_edits_2(sp,ofile,type,format,srate,comment);
      else result = save_selection(ss,ofile,type,format,srate,comment);
      if (result != SND_NO_ERROR)
	{
	  file_string = (char *)CALLOC(256,sizeof(char));
	  sprintf(file_string,"save as temp: %s: %s (%s)",ofile,strerror(errno),snd_error_name(result));
	  report_in_minibuffer(sp,file_string);
	  FREE(file_string);
	}
      else err = snd_copy_file(ofile,sp->fullname);
      snd_update(ss,sp);
      free(ofile);
      FREE(fullname);
    }
  else
    {
      collision = (same_name_info *)CALLOC(1,sizeof(same_name_info));
      collision->fullname = fullname;
      collision->edits = 0;
      collision->sp = NULL;
      map_over_sounds(ss,check_for_same_name,(void *)collision);
      if (collision->sp)
	{
	  /* if no edits, we'll just close, overwrite, reopen */
	  /* if edits, we need to ask luser what to do */
	  /* we don't need to check for overwrites at this point */
	  if (collision->edits > 0)
	    {
	      file_string = (char *)CALLOC(256,sizeof(char));
	      sprintf(file_string,"%s has unsaved edits.\nClobber them and overwrite %s?",str,str);
	      if (!(snd_yes_or_no_p(ss,file_string))) {FREE(fullname); FREE(collision); FREE(file_string); return(-1);}
	      FREE(file_string);
	    }
	  snd_close_file(collision->sp,ss);
	}
      if (save_type == FILE_SAVE_AS)
	result = save_edits_2(sp,str,type,format,srate,comment);
      else result = save_selection(ss,str,type,format,srate,comment);
      file_string = (char *)CALLOC(256,sizeof(char));
      if (result != SND_NO_ERROR)
	sprintf(file_string,"%s: %s (%s)",str,strerror(errno),snd_error_name(result));
      else sprintf(file_string,"%s saved as %s",(save_type == FILE_SAVE_AS) ? sp->shortname : "selection",str);
      report_in_minibuffer(sp,file_string);
      FREE(file_string);
      if (collision->sp) snd_open_file(fullname,ss);
      FREE(fullname);
      FREE(collision);
    }
  return(result);
}


#define RIPPLE_SIZE 65536
/* needs to be big enough to accomodate any newly added header or header comments */

int edit_header_callback(snd_state *ss, snd_info *sp, file_data *edit_header_data)
{
  unsigned char *ripple0,*ripple1,*zerobuf;
  int fd,err,chans,srate,loc,comlen,type,format,bytes0,bytes1,curloc,readloc,writeloc,curbytes,totalbytes;
  char *comment;
  file_info *hdr;
#if HAVE_ACCESS
  err = access(sp->fullname,W_OK);
#else
  err = 0;
#endif
  if (err == 0)
    {
      fd = open(sp->fullname,O_RDWR,0);
      if (fd != -1)
	{
	  hdr = sp->hdr;
	  ripple0 = (unsigned char *)CALLOC(RIPPLE_SIZE,sizeof(unsigned char));
	  ripple1 = (unsigned char *)CALLOC(RIPPLE_SIZE,sizeof(unsigned char));
	  lseek(fd,hdr->data_location,SEEK_SET);
	  bytes0 = read(fd,ripple0,RIPPLE_SIZE);
	  if (bytes0 == RIPPLE_SIZE) bytes1 = read(fd,ripple1,RIPPLE_SIZE); else bytes1 = -1;
	  srate = hdr->srate;
	  chans = hdr->chans;
	  type = hdr->type;
	  if ((type == MUS_AIFF) || (type == MUS_AIFC))
	    mus_header_set_aiff_loop_info(mus_sound_loop_info(sp->fullname));
	  mus_sound_forget(sp->fullname);
	  format = hdr->format;
	  loc = hdr->data_location;
	  comment = read_file_data_choices(edit_header_data,&srate, &chans, &type, &format, &loc);
	  comlen = snd_strlen(comment);
	  curloc = 0;
	  curbytes = 0;
	  totalbytes = hdr->samples * mus_data_format_to_bytes_per_sample(hdr->format);
	  lseek(fd,0,SEEK_SET);
	  if (type != MUS_RAW)
	    {
	      mus_header_write_with_fd(fd,type,srate,chans,loc,hdr->samples,format,comment,comlen);
	      curloc = mus_header_data_location();
	      if ((loc != curloc) && (loc != hdr->data_location)) /* user changed it */
		{
		  /* pad if possible ? */
		  if (loc > curloc)
		    {
		      zerobuf = (unsigned char *)CALLOC(loc-curloc,sizeof(unsigned char));
		      write(fd,zerobuf,loc-curloc);
		      FREE(zerobuf);
		      curloc = loc;
		    }
		}
	    }
	  readloc = RIPPLE_SIZE * 2;
	  writeloc = curloc;
	  if (writeloc > readloc) snd_error("%s[%d] %s: writeloc > readloc!",__FILE__,__LINE__,__FUNCTION__);
	  while (bytes0 > 0)
	    {
	      write(fd,ripple0,bytes0);
	      curbytes += bytes0;
	      writeloc += RIPPLE_SIZE;
	      if (bytes1 > 0)
		{
		  lseek(fd,readloc,SEEK_SET);
		  readloc += RIPPLE_SIZE;
		  bytes0 = read(fd,ripple0,RIPPLE_SIZE);
		  lseek(fd,writeloc,SEEK_SET);
		  writeloc += RIPPLE_SIZE;
		  write(fd,ripple1,bytes1);
		  curbytes += bytes1;
		  if (bytes0 > 0)
		    {
		      lseek(fd,readloc,SEEK_SET);
		      readloc += RIPPLE_SIZE;
		      bytes1 = read(fd,ripple1,RIPPLE_SIZE);
		    }
		}
	      if (curbytes > totalbytes) break; /* ?? this should not happen */
	    }
	  close(fd);
	  clear_minibuffer(sp);
	  snd_file_bomb_icon(sp,TRUE);
	  FREE(ripple0);
	  FREE(ripple1);
	  free(comment);
	  if (auto_update(ss)) map_over_sounds(ss,snd_not_current,NULL);
	}
      else 
	snd_error("can't open %s",sp->shortname);
      mus_header_set_aiff_loop_info(NULL);
    }
  else 
    snd_error("can't write %s",sp->shortname);
  return(0);
}


/* raw data dialog funcs */

static int swap_int (int n)
{
  int o;
  unsigned char *inp,*outp; 
  inp=(unsigned char *)&n; 
  outp=(unsigned char *)&o;
  outp[0]=inp[3]; outp[1]=inp[2]; outp[2]=inp[1]; outp[3]=inp[0];
  return(o);
}

static short swap_short (short n)
{
  short o;
  unsigned char *inp,*outp; 
  inp=(unsigned char *)&n; 
  outp=(unsigned char *)&o;
  outp[0]=inp[1]; outp[1]=inp[0]; 
  return(o);
}

char *raw_data_explanation(char *filename, snd_state *ss, file_info *hdr)
{
  char *reason_str,*tmp_str,*file_string;
  int ns,better_srate = 0,better_chans = 0;
  reason_str = (char *)CALLOC(1024,sizeof(char));
  tmp_str = (char *)CALLOC(64,sizeof(char));
  /* try to provide some notion of what might be the intended header (currently limited to byte-order mistakes) */
  sprintf(reason_str,"srate: %d",hdr->srate);
  ns = (int)swap_int(hdr->srate);
  if ((ns<4000) || (ns>100000)) ns = (int)swap_short((short)(hdr->srate));
  if ((ns>4000) && (ns<100000))
    {
      better_srate = ns;
      sprintf(tmp_str," (swapped: %d)",ns);
      strcat(reason_str,tmp_str);
    }
  sprintf(tmp_str,"\nchans: %d",hdr->chans);
  strcat(reason_str,tmp_str);
  ns = swap_int(hdr->chans);
  if ((ns<0) || (ns>8)) ns=swap_short((short)(hdr->chans));
  if ((ns>0) && (ns <= 8))
    {
      better_chans = ns;
      sprintf(tmp_str," (swapped: %d)",ns);
      strcat(reason_str,tmp_str);
    }
  sprintf(tmp_str,"\nlength: %.3f (%d samples, %d bytes total)",
	  (float)(hdr->samples)/(float)(hdr->chans * hdr->srate),
	  hdr->samples,
	  mus_sound_length(filename));
  strcat(reason_str,tmp_str);
  ns = swap_int(hdr->samples);
  if (ns < mus_sound_length(filename))
    {
      sprintf(tmp_str,"\n  (swapped: %d",ns);
      strcat(reason_str,tmp_str);
      if ((better_chans) && (better_srate))
	{
	  sprintf(tmp_str,", swapped length: %.3f / sample-size-in-bytes)",(float)ns/(float)(better_chans * better_srate));
	  strcat(reason_str,tmp_str);
	}
      else strcat(reason_str,")");
    }
  sprintf(tmp_str,"\ndata location: %d",hdr->data_location);
  strcat(reason_str,tmp_str);
  ns = swap_int(hdr->data_location);
  if ((ns>0) && (ns<=1024)) 
    {
      sprintf(tmp_str," (swapped: %d)",ns);
      strcat(reason_str,tmp_str);
    }
  sprintf(tmp_str,"\ntype: %s",mus_header_type_name(hdr->type));
  strcat(reason_str,tmp_str);
  sprintf(tmp_str,"\nformat: %s\n",mus_data_format_name(hdr->format));
  strcat(reason_str,tmp_str);
  hdr->type = MUS_RAW;
  snd_help(ss,"Current header values",reason_str);
  file_string = (char *)CALLOC(256,sizeof(char));
  sprintf(file_string,"Bogus header found for %s",filename_without_home_directory(filename));
  FREE(tmp_str);
  FREE(reason_str);
  return(file_string);
}


/* new file funcs */

snd_info *finish_new_file(snd_state *ss,char *newname,int header_type, int data_format, int srate, int chans, char *new_comment)
{
  snd_info *sp;
  int chan,size;
  unsigned char* buf;
  if (snd_overwrite_ok(ss,newname))
    {
      if (mus_header_writable(header_type,data_format))
	{
	  snd_write_header(ss,newname,header_type,srate,chans,0,chans*2,data_format,new_comment,snd_strlen(new_comment),NULL);
	  chan = snd_reopen_write(ss,newname);
	  lseek(chan,mus_header_data_location(),SEEK_SET);
	  size = chans * mus_samples_to_bytes(data_format,2); /* why 2 samples? */
	  buf = (unsigned char *)CALLOC(size,sizeof(unsigned char));
	  write(chan,buf,size);
	  close(chan);
	  FREE(buf);
	  sp = snd_open_file(newname,ss);
	  return(sp);
	}
      else 
	{
	  snd_error("can't write %s %s file with %s data format",
		    ((header_type != MUS_RIFF) && (header_type != MUS_NEXT)) ? "an" : "a",
		    mus_header_type_name(header_type),
		    data_format_name(data_format));
	}
    }
  return(NULL);
}

snd_info *snd_new_file(snd_state *ss, char *newname, int header_type, int data_format, int srate, int chans, char *comment)
{
  /* first post dialog if needed to fill in defaults, then create the thing and make room in Snd */
  mus_sound_forget(newname);
  if (!(mus_header_writable(header_type,data_format)))
    return(make_new_file_dialog(ss,newname,header_type,data_format,srate,chans,comment));
  else return(finish_new_file(ss,newname,header_type,data_format,srate,chans,comment));
}


#if HAVE_GUILE
#include "sg.h"

static SCM g_add_sound_file_extension(SCM ext)
{
  #define H_add_sound_file_extension "(" S_add_sound_file_extension " ext)  adds the file extension ext to the list of sound file extensions"
  char *name;
  ERRS1(ext,S_add_sound_file_extension);
  name = gh_scm2newstr(ext,NULL);
  add_sound_file_extension(name);
  free(name);
  return(ext);
}

static SCM g_file_write_date(SCM file)
{
  #define S_file_write_date "file-write-date"
  #define H_file_write_date "(" S_file_write_date " file) -> write date"
  char *name;
  time_t date;
  ERRS1(file,S_file_write_date);
  name = gh_scm2newstr(file,NULL);
  date = file_write_date(name);
  free(name);
  return(gh_int2scm(date));
}

static SCM g_override_data_location(SCM loc, SCM snd) 
{
  #define H_override_data_location "(" S_override_data_location " loc &optional snd) overrides snd's notion of its data location"
  snd_info *sp;
  ERRN1(loc,S_override_data_location);
  ERRSP(S_override_data_location,snd,2);
  sp = get_sp(snd);
  if (sp == NULL) return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(S_override_data_location),snd)));
  mus_sound_override_header(sp->fullname,-1,-1,-1,-1,g_scm2int(loc),-1);
  snd_update(sp->state,sp);
  return(loc);
}

static SCM g_override_data_format(SCM frm, SCM snd) 
{
  #define H_override_data_format "(" S_override_data_format " format &optional snd) overrides snd's notion of its data format"
  snd_info *sp;
  ERRN1(frm,S_override_data_format);
  ERRSP(S_override_data_format,snd,2);
  sp = get_sp(snd);
  if (sp == NULL) return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(S_override_data_format),snd)));
  mus_sound_override_header(sp->fullname,-1,-1,g_scm2int(frm),-1,-1,-1);
  snd_update(sp->state,sp);
  return(frm);
}

static SCM g_override_data_size(SCM over, SCM snd) 
{
  #define H_override_data_size "(" S_override_data_size " samples &optional snd) overrides snd's notion of its data size"
  snd_info *sp;
  ERRN1(over,S_override_data_size);
  ERRSP(S_override_data_size,snd,2);
  sp = get_sp(snd);
  if (sp == NULL) return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(S_override_data_size),snd)));
  mus_sound_override_header(sp->fullname,-1,-1,-1,-1,-1,g_scm2int(over));
  snd_update(sp->state,sp);
  return(over);
}

static SCM g_set_sound_loop_info(SCM start0, SCM end0, SCM start1, SCM end1, SCM snd)
{
  #define H_set_sound_loop_info "(" S_set_sound_loop_info " start0 end0 &optional start1 end1 snd) sets loop points"
  snd_info *sp;
  char *tmp_file;
  int type;
  ERRN1(start0,S_set_sound_loop_info);
  ERRN2(end0,S_set_sound_loop_info);
  ERRB3(start1,S_set_sound_loop_info);
  ERRB4(end1,S_set_sound_loop_info);
  ERRSP(S_set_sound_loop_info,snd,5);
  sp = get_sp(snd);
  if (sp == NULL) return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(S_set_sound_loop_info),snd)));
  if ((sp->hdr)->loops == NULL)
    (sp->hdr)->loops = (int *)CALLOC(6,sizeof(int));
  (sp->hdr)->loops[0] = g_scm2int(start0);
  (sp->hdr)->loops[1] = g_scm2int(end0);
  (sp->hdr)->loops[2] = g_scm2intdef(start1,0);
  (sp->hdr)->loops[3] = g_scm2intdef(end1,0);
  mus_sound_set_loop_info(sp->fullname,(sp->hdr)->loops);
  type = (sp->hdr)->type;
  if ((type != MUS_AIFF) && (type != MUS_AIFC))
    {
      snd_warning("changing %s header from %s to aifc to accomodate loop info",sp->shortname,mus_header_type_name(type));
      type = MUS_AIFC;
    }
  tmp_file = snd_tempnam(sp->state);
  save_edits_2(sp,tmp_file,type,(sp->hdr)->format,(sp->hdr)->srate,(sp->hdr)->comment);
  snd_copy_file(tmp_file,sp->fullname);
  remove(tmp_file);
  free(tmp_file);
  snd_update(sp->state,sp);
  return(SCM_BOOL_T);
}

static SCM g_soundfont_info(SCM snd)
{
  /* return all soundfont descriptors as list of lists: ((name start loopstart loopend)) */
  #define H_soundfont_info "(" S_soundfont_info " &optional snd) -> list of lists describing snd as a soundfont.\n\
   each inner list has the form: (name start loopstart loopend)"

  SCM inlist = SCM_EOL,outlist = SCM_EOL;
  int i,lim;
  snd_info *sp;
  ERRSP(S_soundfont_info,snd,1);
  sp = get_sp(snd);
  if (sp == NULL) return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(S_soundfont_info),snd)));
  mus_header_read(sp->fullname);
  if (mus_header_type() == MUS_SOUNDFONT)
    {
      lim = mus_header_sf2_entries();
      if (lim > 0)
	{
	  for (i=lim-1;i>=0;i--)
	    {
	      inlist = SCM_LIST4(gh_str02scm(mus_header_sf2_name(i)),
				 gh_int2scm(mus_header_sf2_start(i)),
				 gh_int2scm(mus_header_sf2_loop_start(i)),
				 gh_int2scm(mus_header_sf2_end(i)));
	      outlist = gh_cons(inlist,outlist);
	    }
	}
    }
  return(outlist);
}

static SCM g_preload_directory(SCM directory) 
{
  #define H_preload_directory "(" S_preload_directory " dir) preloads (into the View:Files dialog) any sounds in dir"
  char *str;
  ERRS1(directory,S_preload_directory);
  str = gh_scm2newstr(directory,NULL);
  if (str) add_directory_to_prevlist(get_global_state(),str);
  free(str);
  return(directory);
}

static SCM g_preload_file(SCM file) 
{
  #define H_preload_file "(" S_preload_file " file) preloads file (into the View:Files dialog)"
  char *name = NULL,*urn;
  ERRS1(file,S_preload_file);
  urn = gh_scm2newstr(file,NULL);
  name = mus_file_full_name(urn);
  free(urn);
  remember_me(get_global_state(),filename_without_home_directory(name),name);
  if (name) FREE(name);
  return(file);
}

static SCM g_sound_files_in_directory(SCM dirname)
{
  #define H_sound_files_in_directory "(" S_sound_files_in_directory " directory) returns a vector of sound files in directory"
  dir *dp = NULL;
  char *name = NULL;
  int i,numfiles;
  SCM vect = SCM_BOOL_F;
  ERRS1(dirname,S_sound_files_in_directory);
  name = gh_scm2newstr(dirname,NULL);
  if (name)
    {
      dp = find_sound_files_in_dir(name);
      free(name);
      if (dp)
	{
	  numfiles = dp->len;
	  vect = gh_make_vector(gh_int2scm(numfiles),SCM_BOOL_F);
	  for (i=0;i<numfiles;i++)
	    gh_vector_set_x(vect,gh_int2scm(i),gh_str02scm(dp->files[i]));
	  free_dir(dp);
	}
    }
  return(vect);
}


void g_init_file(SCM local_doc)
{
  DEFINE_PROC(gh_new_procedure1_0(S_add_sound_file_extension,g_add_sound_file_extension),H_add_sound_file_extension);
  DEFINE_PROC(gh_new_procedure1_0(S_file_write_date,g_file_write_date),H_file_write_date);
  DEFINE_PROC(gh_new_procedure(S_set_sound_loop_info,SCM_FNC g_set_sound_loop_info,2,3,0),H_set_sound_loop_info);
  DEFINE_PROC(gh_new_procedure0_1(S_soundfont_info,g_soundfont_info),H_soundfont_info);
  DEFINE_PROC(gh_new_procedure1_1(S_override_data_location,g_override_data_location),H_override_data_location);
  DEFINE_PROC(gh_new_procedure1_1(S_override_data_format,g_override_data_format),H_override_data_format);
  DEFINE_PROC(gh_new_procedure1_1(S_override_data_size,g_override_data_size),H_override_data_size);
  DEFINE_PROC(gh_new_procedure1_0(S_preload_directory,g_preload_directory),H_preload_directory);
  DEFINE_PROC(gh_new_procedure1_0(S_preload_file,g_preload_file),H_preload_file);
  DEFINE_PROC(gh_new_procedure1_0(S_sound_files_in_directory,g_sound_files_in_directory),H_sound_files_in_directory);

  memo_sound = gh_define(S_memo_sound,SCM_BOOL_F);

#if (!HAVE_GUILE_1_3_0)
  open_hook = scm_create_hook(S_open_hook,1);                     /* arg = filename */
  close_hook = scm_create_hook(S_close_hook,1);                   /* arg = sound index */
#else
  open_hook = gh_define(S_open_hook,SCM_BOOL_F);
  close_hook = gh_define(S_close_hook,SCM_BOOL_F);
#endif
}
#endif
