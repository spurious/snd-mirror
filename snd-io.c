#include "snd.h"

/* file buffers (i.e. a sliding window on a given file's data) */

static void c_io_bufclr (snd_io *io, int beg)
{
  int k, end;
  end = io->bufsize;
  for (k = 0; k < io->chans; k++)
    {
      mus_sample_t *j;
      j = io->arrays[k];
      if (j)
	memset((void *)(j + beg), 0, (end - beg) * sizeof(mus_sample_t));
    }
}

static void reposition_file_buffers_1(off_t loc, snd_io *io)
{
  /* called when loc is outside the current in-core frame for the file pointed to by io */
  off_t frames;
  frames = io->frames - loc;
  if (frames > io->bufsize) frames = io->bufsize;
  if (frames <= 0)                   /* tried to access beyond current end of file */
    {
      io->beg = loc; 
      c_io_bufclr(io, 0);
    }
  else
    {
      mus_file_seek_frame(io->fd, loc);
      io->beg = loc;
      mus_file_read_chans(io->fd,
			  0, frames - 1,
			  io->chans,
			  io->arrays,
			  (mus_sample_t *)(io->arrays));
      if (frames < io->bufsize) c_io_bufclr(io, frames);
    }
  io->end = io->beg + io->bufsize - 1;
}

static void reposition_file_buffers(snd_data *sd, off_t index)
{
  int fd = 0;
  bool reclose = false;
  if (index < 0) index = 0; /* if reading in reverse, don't fall off the start of the buffer */
  if (sd->open == FD_CLOSED)
    {
      file_info *hdr;
      /* try to open it with sndlib descriptors */
      fd = mus_file_open_read(sd->filename); 
      if (fd == -1) 
	{
	  /* our file has disappeared?!? */
	  snd_error(_("%s is unreadable: %s?"), sd->filename, strerror(errno));
	  return;
	}
      hdr = sd->hdr;
      /* these need to flush active data before hidden close and fixup the io indices */
      mus_file_open_descriptors(fd,
				sd->filename,
				hdr->format,
				mus_bytes_per_sample(hdr->format),
				hdr->data_location,
				hdr->chans,
				hdr->type);
      during_open(fd, sd->filename, SND_REOPEN_CLOSED_FILE);
      /* fix up io->fd and whatever else is clobbered by mus_file_close */
      sd->io->fd = fd;
      sd->open = FD_OPEN;
      reclose = true;
    }
  reposition_file_buffers_1(index, sd->io);
  if (reclose)
    {
      mus_file_close(fd); 
      sd->open = FD_CLOSED; 
      sd->io->fd = -1;
    }
}

snd_io *make_file_state(int fd, file_info *hdr, int chan, off_t beg, int suggested_bufsize)
{
  snd_io *io;
  int bufsize;
  off_t chansize;
  bufsize = suggested_bufsize;
  chansize = (hdr->samples / hdr->chans); /* this can be bogus if the header is messed up */
  if ((chansize > 0) && 
      (bufsize > chansize)) 
    bufsize = chansize + 1;
  io = (snd_io *)CALLOC(1, sizeof(snd_io));
  io->arrays = (mus_sample_t **)CALLOC(hdr->chans, sizeof(mus_sample_t *));
  io->fd = fd;
  io->chans = hdr->chans;
  io->frames = chansize;
  io->beg = 0;
  io->end = bufsize - 1;
  io->bufsize = bufsize;
#if DEBUGGING
  if (chan >= hdr->chans) {fprintf(stderr,"make_file_state, chan: %d %d\n", chan, hdr->chans); abort();}
  if (bufsize <= 0) {fprintf(stderr,"make_file_state, bufsize: %d\n", bufsize); abort();}
#endif
  io->arrays[chan] = (mus_sample_t *)CALLOC(bufsize, sizeof(mus_sample_t));
  reposition_file_buffers_1(beg, io); /* get ready to read -- we're assuming mus_file_read_chans here */
  return(io);
}

void file_buffers_forward(off_t ind0, off_t ind1, off_t indx, snd_fd *sf, snd_data *cur_snd)
{
  /* need to track in-core buffer and file-relative index */
  if ((indx < cur_snd->io->beg) ||
      (indx > cur_snd->io->end)) 
    reposition_file_buffers(cur_snd, indx);
  sf->loc = indx - cur_snd->io->beg;
  if (ind0 >= cur_snd->io->beg)
    sf->first = ind0 - cur_snd->io->beg;
  else sf->first = 0;
  if (ind1 <= cur_snd->io->end) 
    sf->last = ind1 - cur_snd->io->beg;
  else sf->last = cur_snd->io->bufsize - 1;
}

void file_buffers_back(off_t ind0, off_t ind1, off_t indx, snd_fd *sf, snd_data *cur_snd)
{
  if ((indx > cur_snd->io->end) || 
      (indx < cur_snd->io->beg)) 
    reposition_file_buffers(cur_snd, indx - cur_snd->io->bufsize + 1);
  sf->loc = indx - cur_snd->io->beg;
  if (ind1 <= cur_snd->io->end)
    sf->last = ind1 - cur_snd->io->beg;
  else sf->last = cur_snd->io->bufsize - 1;
  if (ind0 >= cur_snd->io->beg) 
    sf->first = ind0 - cur_snd->io->beg;
  else sf->first = 0;
}


/* wrappers for low level sndlib open/close/access functions -- we can't use
 * the sndlib versions directly because in some cases, Snd has more than FOPEN_MAX
 * files nominally open and accessible (mix temps in with-sound explode for example).
 * these wrappers provide checks for EMFILE as errno from open and try to close
 * temps to make room. 
 *
 * there is a hidden limit that might come into play if FOPEN_MAX > MUS_FILE_DESCRIPTORS (see io.c)
 * on the SGI, FOPEN_MAX is 100, but we can open many more files than that without hitting the EMFILE error.
 */

static void close_temp_files(chan_info *cp, void *closed)
{
  if ((cp) && (cp->sounds))
    {
      int i, rtn;
      rtn = (*((int *)closed));
      for (i = 0; i < cp->sound_size; i++)
	{
	  snd_data *sd;
	  sd = cp->sounds[i];
	  if ((sd) && 
	      (sd->type == SND_DATA_FILE) && 
	      (sd->io) && 
	      (sd->open == FD_OPEN))
	    {
	      mus_file_close(sd->io->fd);
	      sd->open = FD_CLOSED;
	      sd->io->fd = -1;
	      rtn++;
	    }
	}
      (*((int *)closed)) = rtn;
    }
}

static int too_many_files_cleanup(void)
{
  int *closed;
  int rtn;
  rtn = -1;
  closed = (int *)MALLOC(sizeof(int));
  (*closed) = 0;
  for_each_normal_chan_1(close_temp_files, (void *)closed);
  if ((*closed) == 0) 
    for_each_region_chan(close_temp_files, (void *)closed);
  if ((*closed) == 0)
    rtn = -1;
  else rtn = (*closed);
  FREE(closed);
  return(rtn);
}

int snd_open_read(const char *arg) 
{
  int fd;
  fd = OPEN(arg, O_RDONLY, 0);
  if ((fd == -1) && (errno == EMFILE)) /* there's also ENFILE = file table overflow (/usr/include/asm/errno.h) */
    {
      fd = too_many_files_cleanup();
      if (fd != -1) 
	fd = OPEN(arg, O_RDONLY, 0);
      if (fd == -1) 
	snd_error("%s: %s", arg, strerror(errno));
    }
  return(fd);
}

bool snd_overwrite_ok(const char *ofile)
{
  bool rtn = true;
  if (ask_before_overwrite(ss))
    {
      int fil;
      fil = OPEN(ofile, O_RDONLY, O_NONBLOCK);
      if (fil != -1) 
	{
	  CLOSE(fil);
	  rtn = snd_yes_or_no_p(_("file %s exists. Overwrite?"), ofile);
	}
    }
  return(rtn);
}

int snd_reopen_write(const char *arg)
{
  int fd;
  fd = OPEN(arg, O_RDWR, 0);
  if ((fd == -1) && 
      (errno == EMFILE))
    {
      fd = too_many_files_cleanup();
      if (fd != -1) 
	fd = OPEN(arg, O_RDWR, 0);
      if (fd == -1) 
	snd_error("%s: %s", arg, strerror(errno));
    }
  return(fd);
}

static int local_mus_error = MUS_NO_ERROR;
static mus_error_handler_t *old_error_handler;
static void local_mus_error2snd(int type, char *msg) {local_mus_error = type;}

int snd_write_header(const char *name, int type, int srate, int chans, off_t loc, 
		     off_t samples, int format, const char *comment, int len, int *loops)
{
  int err;
  /* trap mus_error locally here so that callers of open_temp_file can cleanup sample readers and whatnot */
  old_error_handler = mus_error_set_handler(local_mus_error2snd);
  mus_sound_forget(name);
  mus_header_set_aiff_loop_info(loops);
  err = mus_header_write(name, type, srate, chans, loc, samples, format, comment, len);
  if (err == -1)
    {
      if (errno == EMFILE) /* 0 => no error (err not actually returned unless it's -1) */
	{
	  err = too_many_files_cleanup();
	  if (err != -1) 
	    err = mus_header_write(name, type, srate, chans, loc, samples, format, comment, len);
	}
    }
  if (err != -1)
    mus_header_set_aiff_loop_info(NULL);
  mus_error_set_handler(old_error_handler);
  if ((err == -1) || (local_mus_error != MUS_NO_ERROR))
    {
      local_mus_error = MUS_NO_ERROR;
      return(-1);
    }
  return(err);
}

int snd_remove(const char *name, cache_remove_t forget)
{
  int err = 0;
  if (forget == REMOVE_FROM_CACHE) mus_sound_forget(name); /* no error here if not in sound tables */
  err = remove(name);
  if (err == -1)
    snd_warning(_("can't remove file %s: %s"), name, strerror(errno));
  return(err);
}

int snd_close(int fd, const char *name)
{
  int val;
  val = CLOSE(fd);
  if (val != 0)
    snd_warning(_("can't close file %d (%s): %s"), fd, name, strerror(errno));
  return(val);
}

void snd_fclose(FILE *fd, const char *name)
{
  int val;
  val = FCLOSE(fd);
  if (val != 0)
    snd_warning(_("can't close file %s: %s"), name, strerror(errno));
}


/* there are a few special-case multi-channel temp files that need a kind of reference count to handle deletion */
/* this machinery affects only these special cases, not temp files in general */

typedef struct {
  char *name;
  int chans;
  int *ticks;
} tempfile_ctr;

static tempfile_ctr **tempfiles = NULL;
static int tempfiles_size = 0;

void remember_temp(const char *filename, int chans)
{
  int i, old_size;
  tempfile_ctr *tmp = NULL;
  if (tempfiles_size == 0)
    {
      tempfiles_size = 8;
      tempfiles = (tempfile_ctr **)CALLOC(tempfiles_size, sizeof(tempfile_ctr *));
      i = 0;
    }
  else
    {
      for (i = 0; i < tempfiles_size; i++)
	if (tempfiles[i] == NULL)
	  break;
      if (i >= tempfiles_size)
	{
	  old_size = tempfiles_size;
	  tempfiles_size += 8;
	  tempfiles = (tempfile_ctr **)REALLOC(tempfiles, tempfiles_size * sizeof(tempfile_ctr *));
	  for (i = old_size; i < tempfiles_size; i++) tempfiles[i] = NULL;
	  i = old_size;
	}
    }
  tmp = (tempfile_ctr *)CALLOC(1, sizeof(tempfile_ctr));
  tempfiles[i] = tmp;
  tmp->name = copy_string(filename);
  tmp->chans = chans;
  tmp->ticks = (int *)CALLOC(chans, sizeof(int));
}

static void forget_temp(const char *filename, int chan)
{
  int i, j;
  for (i = 0; i < tempfiles_size; i++)
    {
      bool happy = false;
      tempfile_ctr *tmp;
      tmp = tempfiles[i];
      if ((tmp) && (strcmp(filename, tmp->name) == 0))
	{
	  tmp->ticks[chan]--;
	  for (j = 0; j < tmp->chans; j++)
	    if (tmp->ticks[j] > 0) 
	      {
		happy = true;
		return;
	      }
	  if (!happy)
	    {
	      snd_remove(tmp->name, REMOVE_FROM_CACHE);
	      FREE(tmp->name);
	      FREE(tmp->ticks);
	      FREE(tmp);
	      tempfiles[i] = NULL;
	    }
	  return;
	}
    }
}

static void tick_temp(const char *filename, int chan)
{
  int i;
  for (i = 0; i < tempfiles_size; i++)
    {
      tempfile_ctr *tmp;
      tmp = tempfiles[i];
      if ((tmp) && (strcmp(filename, tmp->name) == 0))
	{
	  tmp->ticks[chan]++;
	  return;
	}
    }
}

void forget_temps(void)
{
  /* this should always be a no-op -- probably can be deleted */
  int i;
  for (i = 0; i < tempfiles_size; i++)
    if (tempfiles[i])
      snd_remove(tempfiles[i]->name, REMOVE_FROM_CACHE);
}

snd_data *make_snd_data_file(const char *name, snd_io *io, file_info *hdr, file_delete_t temp, int ctr, int temp_chan)
{
  snd_data *sd;
  sd = (snd_data *)CALLOC(1, sizeof(snd_data));
  sd->type = SND_DATA_FILE;
#if DEBUGGING
  if (temp_chan >= hdr->chans) {fprintf(stderr,"make_snd_data_file, chans: %d %d\n", temp_chan, hdr->chans); abort();}
#endif
  sd->buffered_data = io->arrays[temp_chan];
  sd->io = io;
  sd->filename = copy_string(name);
  sd->hdr = hdr;
  sd->temporary = temp;
  if (temp == MULTICHANNEL_DELETION) tick_temp(name, temp_chan);
  sd->edit_ctr = ctr;
  sd->open = FD_OPEN;
  sd->inuse = false;
  sd->copy = false;
  sd->chan = temp_chan;
  sd->len = (hdr->samples) * (mus_bytes_per_sample(hdr->format)) + hdr->data_location;
  return(sd);
}

snd_data *copy_snd_data(snd_data *sd, off_t beg, int bufsize)
{
  snd_data *sf;
  snd_io *io;
  int fd;
  file_info *hdr;
  hdr = sd->hdr;
  fd = snd_open_read(sd->filename);
  if (fd == -1) 
    return(NULL);
  mus_file_open_descriptors(fd,
			    sd->filename,
			    hdr->format,
			    mus_bytes_per_sample(hdr->format),
			    hdr->data_location,
			    hdr->chans,
			    hdr->type);
  during_open(fd, sd->filename, SND_COPY_READER);
  io = make_file_state(fd, hdr, sd->chan, beg, bufsize);
  sf = (snd_data *)CALLOC(1, sizeof(snd_data));
  sf->type = sd->type;
  sf->buffered_data = io->arrays[sd->chan];
  sf->io = io;
  sf->filename = copy_string(sd->filename);
  sf->hdr = hdr;
  sf->temporary = DONT_DELETE_ME;
  sf->edit_ctr = sd->edit_ctr;
  sf->open = FD_OPEN;
  sf->inuse = false;
  sf->copy = true;
  return(sf);
}

snd_data *make_snd_data_buffer(mus_sample_t *data, int len, int ctr)
{
  snd_data *sf;
  sf = (snd_data *)CALLOC(1, sizeof(snd_data));
  sf->type = SND_DATA_BUFFER;
  sf->buffered_data = (mus_sample_t *)MALLOC((len + 1) * sizeof(mus_sample_t));
  /* sigh... using len + 1 rather than len to protect against access to inserted buffer at end mixups (final fragment uses end + 1) */
  /*   the real problem here is that I never decided whether insert starts at the cursor or just past it */
  /*   when the cursor is on the final sample, this causes cross-fragment ambiguity as to the length of a trailing insertion */
  /*   C > (make-region 1000 2000) (insert-region (cursor)) C-v hits this empty slot and gets confused about the previously final sample value */
  memcpy((void *)(sf->buffered_data), (void *)data, len * sizeof(mus_sample_t));
  sf->edit_ctr = ctr;
  sf->copy = false;
  sf->inuse = false;
  sf->len = len * 4;
  return(sf);
}

snd_data *make_snd_data_buffer_for_simple_channel(int len)
{
  snd_data *sf;
  sf = (snd_data *)CALLOC(1, sizeof(snd_data));
  sf->type = SND_DATA_BUFFER;
  sf->buffered_data = (mus_sample_t *)CALLOC(len, sizeof(mus_sample_t));
  sf->edit_ctr = 0;
  sf->copy = false;
  sf->inuse = false;
  sf->len = len * 4;
  return(sf);
}

snd_data *free_snd_data(snd_data *sd)
{
  if (sd)
    {
      if (!(sd->inuse))
	{
	  /* assume the inuse cases will eventually be freed via Guile GC.
	   *   this can happen if a sample-reader is created, and forgotten,
	   *   and the associated sound is closed.  The closing runs through
	   *   the snd_data (sounds) list freeing the descriptors, but the
	   *   forgotten sample-reader is still idle somewhere thinking it
	   *   might someday find a use for itself...
	   */
	  if (sd->temporary == ALREADY_DELETED)
	    return(NULL);
	  if (sd->temporary == MULTICHANNEL_DELETION)
	    forget_temp(sd->filename, sd->chan);
	  if ((sd->type == SND_DATA_BUFFER) && 
	      (sd->buffered_data)) 
	    FREE(sd->buffered_data);
	  sd->buffered_data = NULL;
	  if ((!(sd->copy)) && 
	      (sd->hdr)) 
	    free_file_info(sd->hdr);
	  sd->hdr = NULL;
	  if (sd->io)
	    {
	      if (sd->open == FD_OPEN) mus_file_close(sd->io->fd);
	      if (sd->io)
		{
		  /* free the IO buffers as well as the descriptor buffer */
		  int i, chans;
		  chans = sd->io->chans;
		  for (i = 0; i < chans; i++)
		    if (sd->io->arrays[i]) 
		      FREE(sd->io->arrays[i]);
		  FREE(sd->io->arrays);
		  FREE(sd->io);
		  sd->io = NULL;
		}
	      if (sd->temporary == DELETE_ME) 
		snd_remove(sd->filename, REMOVE_FROM_CACHE);
	    }
	  if (sd->filename) FREE(sd->filename);
	  sd->filename = NULL;
	  sd->temporary = ALREADY_DELETED;
	  sd->copy = false;
	  sd->type = SND_DATA_NO_DATA;
	  FREE(sd);
	}
      else 
	{
	  sd->free_me = true;
	}
    }
  return(NULL);
}

int open_temp_file(const char *ofile, int chans, file_info *hdr)
{
  int ofd, len, err;
  len = snd_strlen(hdr->comment);
  if (!(mus_header_writable(hdr->type, hdr->format)))
    {
      hdr->type = default_output_type(ss);
      if (mus_header_writable(hdr->type, default_output_format(ss)))
	hdr->format = default_output_format(ss);
      else
	{
	  /* was default_output_* here, but that's for the user's output, not ours */
	  hdr->type = MUS_NEXT;
	  hdr->format = MUS_OUT_FORMAT;
	}
    }
  err = snd_write_header(ofile, hdr->type, hdr->srate, chans, 0, 0, hdr->format, hdr->comment, len, hdr->loops);
  if (err == -1) return(-1);
  if ((ofd = snd_reopen_write(ofile)) == -1) return(-1);
  hdr->data_location = mus_header_data_location(); /* header might have changed size (aiff extras) */
  mus_file_open_descriptors(ofd,
			    ofile,
			    hdr->format,
			    mus_bytes_per_sample(hdr->format),
			    hdr->data_location,
			    chans,
			    hdr->type);
  lseek(ofd, hdr->data_location, SEEK_SET);
  return(ofd);
}

int close_temp_file(const char *filename, int ofd, int type, off_t bytes, snd_info *sp)
{
  off_t kleft, kused;
  int err;
  err = mus_file_close(ofd);
  mus_header_change_data_size(filename, type, bytes);
  kleft = disk_kspace(filename);
  if (kleft < 0)
    snd_error(_("disk full?: %s"), strerror(errno));
  else
    {
      kused = bytes >> 10;
      if ((kused > kleft) && (sp))
	report_in_minibuffer_and_save(sp, _("disk nearly full: used " PRId64 " Kbytes in the last operation, leaving " PRId64), kused, kleft);
    }
  return(err);
}

