#include "snd.h"

/* file buffers (i.e. a sliding window on a given file's data */

#define SND_AREF_BLOCK 0
#define SND_AREF_SIZE 1
#define SND_AREF_HEADER_SIZE 2

#define SND_IO_FD 1
#define SND_IO_CHANS 2
#define SND_IO_SIZE 3
#define SND_IO_BEG 4
#define SND_IO_END 5
#define SND_IO_BUFSIZ 6
#define SND_IO_HDR_END 7
#define SND_IO_DATS 8

static void c_io_bufclr (int *io, int *datai, int beg)
{
  int i, k, end;
  MUS_SAMPLE_TYPE *j;
  if (io[SND_IO_DATS+SND_AREF_BLOCK] == 0)
    snd_error("attempt to clear deallocated IO buffer");
  else
    {
      end = io[SND_IO_BUFSIZ];
      for (k = 0; k < io[SND_IO_CHANS]; k++)
	{
	  j = MUS_SAMPLE_ARRAY(datai[io[SND_IO_DATS + SND_AREF_BLOCK] + k]);
	  if (j)
	    {
#if HAVE_MEMSET
	      if (beg == 0)
		memset((void *)j, 0, end * sizeof(MUS_SAMPLE_TYPE));
	      else
#endif
	      for (i = beg; i < end; i++) 
		j[i] = MUS_SAMPLE_0;
	    }
	}
    }
}

static void reposition_file_buffers_1(int loc, int *io, int *datai)
{
  /* called when loc is outside the current in-core frame for the file pointed to by io */
  int file_end, bytes;
  int bufend, filbytes;
#if LONG_INT_P
  int i;
  MUS_SAMPLE_TYPE **bufs;
#endif
  if ((loc < io[SND_IO_BEG]) && 
      ((loc + (int)(.9 * io[SND_IO_BUFSIZ])) > io[SND_IO_BEG]))
    {
      if ((loc + 10) > io[SND_IO_BEG]) loc -= (int)(.75 * io[SND_IO_BUFSIZ]);
      if (loc < 0) loc = 0;
      if (io[SND_IO_CHANS] == 1) loc = (2 * (int)(loc / 2));
    }
  file_end = io[SND_IO_SIZE];
  bytes = file_end - loc;
  if (bytes > io[SND_IO_BUFSIZ]) 
    bytes = io[SND_IO_BUFSIZ];
  if (bytes < 0)                   /* tried to access beyond current end of file */
    {
      io[SND_IO_BEG] = loc; 
      c_io_bufclr(io, datai, 0);
    }
  else /* bytes is positive or 0 */
    {
      mus_file_seek(io[SND_IO_FD],
		    io[SND_IO_HDR_END] + (2 * io[SND_IO_CHANS] * loc),
		    SEEK_SET);
      io[SND_IO_BEG] = loc;
      if (bytes > 0) 
	{
#if LONG_INT_P
	  bufs = (MUS_SAMPLE_TYPE **)MALLOC(io[SND_IO_CHANS] * sizeof(MUS_SAMPLE_TYPE *));
	  for (i = 0; i < io[SND_IO_CHANS]; i++) 
	    bufs[i] = MUS_SAMPLE_ARRAY(datai[io[SND_IO_DATS + SND_AREF_BLOCK] + i]);
	  mus_file_read_chans(io[SND_IO_FD],
			      0, bytes - 1,
			      io[SND_IO_CHANS],
			      bufs,
			      (MUS_SAMPLE_TYPE *)bufs);
	  FREE(bufs);
#else
	  mus_file_read_chans(io[SND_IO_FD],
			      0, bytes - 1,
			      io[SND_IO_CHANS],
			      (MUS_SAMPLE_TYPE **)(datai + io[SND_IO_DATS + SND_AREF_BLOCK]),
			      (MUS_SAMPLE_TYPE *)(datai + io[SND_IO_DATS + SND_AREF_BLOCK]));
#endif
	}
      if (bytes < io[SND_IO_BUFSIZ]) c_io_bufclr(io, datai, bytes);
      bufend = io[SND_IO_BUFSIZ] - 1;
      filbytes = file_end-io[SND_IO_BEG] - 1;
      if (filbytes < bufend) bufend = filbytes;
    }
  io[SND_IO_END] = io[SND_IO_BEG]+io[SND_IO_BUFSIZ] - 1;
}

static void reposition_file_buffers(snd_data *sd, int index)
{
  int fd = 0;
  int reclose = 0;
  file_info *hdr;
  if (index < 0) index = 0; /* if reading in reverse, don't fall off the start of the buffer */
  if (sd->just_zeros)
    {
      sd->io[SND_IO_BEG] = index;
      sd->io[SND_IO_END] = sd->io[SND_IO_BEG] + sd->io[SND_IO_BUFSIZ] - 1;
      return;
    }
  if (sd->open == FD_CLOSED)
    {
      /* try to open it with sndlib descriptors */
      fd = mus_file_open_read(sd->filename); 
      if (fd == -1) 
	{
	  /* our file has disappeared?!? */
	  snd_error("%s is unreadable? open: %s", sd->filename, strerror(errno));
	  return;
	}
      hdr = sd->hdr;
      /* these need to flush active data before hidden close and fixup the datai indices */
      mus_file_open_descriptors(fd,
			       sd->filename,
			       hdr->format,
			       mus_data_format_to_bytes_per_sample(hdr->format),
			       hdr->data_location,
			       hdr->chans,
			       hdr->type);
      during_open(fd, sd->filename, SND_REOPEN_CLOSED_FILE);
      /* fix up io[SND_IO_FD] and whatever else is clobbered by mus_file_close */
      sd->io[SND_IO_FD] = fd;
      sd->open = FD_OPEN;
      reclose = 1;
    }
  reposition_file_buffers_1(index, sd->io, sd->io);
  if (reclose)
    {
      mus_file_close(fd); 
      sd->open = FD_CLOSED; 
      sd->io[SND_IO_FD] = -1;
    }
}

int *make_file_state(int fd, file_info *hdr, int chan, int suggested_bufsize)
{
  int *datai;
  int bufsize, chansize;
  bufsize = suggested_bufsize;
  chansize = (hdr->samples / hdr->chans); /* this can be bogus if the header is messed up */
  if ((chansize > 0) && 
      (bufsize > chansize)) 
    bufsize = chansize + 1;
  datai = (int *)CALLOC(SND_IO_DATS + SND_AREF_HEADER_SIZE + hdr->chans + 2, sizeof(int)); /* why the +2?? */
  datai[SND_IO_FD] = fd;
  datai[SND_IO_CHANS] = hdr->chans;
  datai[SND_IO_SIZE] = chansize;
  datai[SND_IO_BEG] = 0;
  datai[SND_IO_END] = bufsize - 1;
  datai[SND_IO_BUFSIZ] = bufsize;
  datai[SND_IO_HDR_END] = hdr->data_location; 
  datai[SND_IO_DATS + SND_AREF_BLOCK] = SND_IO_DATS + SND_AREF_HEADER_SIZE;
  datai[SND_IO_DATS + SND_AREF_SIZE] = hdr->chans;
  datai[SND_IO_DATS + SND_AREF_HEADER_SIZE + chan] = (int)(MUS_MAKE_SAMPLE_ARRAY(bufsize));
  reposition_file_buffers_1(0, datai, datai); /* get ready to read -- we're assuming mus_file_read_chans here */
  return(datai);
}

#define ZERO_BUFFER_SIZE 1024
int *make_zero_file_state(int size)
{
  int *datai;
  datai = (int *)CALLOC(SND_IO_DATS + SND_AREF_HEADER_SIZE + 1 + 2, sizeof(int));
  datai[SND_IO_FD] = -1;
  datai[SND_IO_CHANS] = 1;
  datai[SND_IO_SIZE] = size;
  datai[SND_IO_BEG] = 0;
  datai[SND_IO_END] = ZERO_BUFFER_SIZE - 1;
  datai[SND_IO_BUFSIZ] = ZERO_BUFFER_SIZE;
  datai[SND_IO_HDR_END] = 0;
  datai[SND_IO_DATS + SND_AREF_BLOCK] = SND_IO_DATS + SND_AREF_HEADER_SIZE;
  datai[SND_IO_DATS + SND_AREF_SIZE] = 1;
  datai[SND_IO_DATS + SND_AREF_HEADER_SIZE] = (int)(MUS_MAKE_SAMPLE_ARRAY(ZERO_BUFFER_SIZE));
  return(datai);
}

int *free_file_state(int *datai)
{
  /* gotta free the IO buffers as well as the descriptor buffer */
  int i, chans;
  if (datai)
    {
      chans = datai[SND_IO_CHANS];
      for (i = 0; i < chans; i++)
	if (datai[SND_IO_DATS + SND_AREF_HEADER_SIZE + i]) 
	  MUS_FREE_SAMPLE_ARRAY(datai[SND_IO_DATS + SND_AREF_HEADER_SIZE + i]);
      FREE(datai);
    }
  return(NULL);
}

int file_state_channel_offset(int chan) {return(SND_IO_DATS + SND_AREF_HEADER_SIZE + chan);}
void set_file_state_fd(int *datai, int fd) {datai[SND_IO_FD] = fd;}
void close_file_state_fd(int *datai) {mus_file_close(datai[SND_IO_FD]);}
int file_state_buffer_size(int *datai) {return(datai[SND_IO_BUFSIZ]);}

void file_buffers_forward(int ind0, int ind1, int indx, snd_fd *sf, snd_data *cur_snd)
{
  /* need to track in-core buffer and file-relative index */
  if ((indx < cur_snd->io[SND_IO_BEG]) ||
      (indx > cur_snd->io[SND_IO_END])) 
    reposition_file_buffers(cur_snd, indx);
  sf->view_buffered_data = (MUS_SAMPLE_TYPE *)(cur_snd->buffered_data + indx - cur_snd->io[SND_IO_BEG]);
  /* only indx is guaranteed to be within the current in-core buffer */
  if (ind0 >= cur_snd->io[SND_IO_BEG])
    sf->first = (MUS_SAMPLE_TYPE *)(cur_snd->buffered_data + ind0 - cur_snd->io[SND_IO_BEG]);
  else sf->first = cur_snd->buffered_data;
  if (ind1 <= cur_snd->io[SND_IO_END]) 
    {
      sf->last = (MUS_SAMPLE_TYPE *)(cur_snd->buffered_data + ind1 - cur_snd->io[SND_IO_BEG]);
      sf->eof = 1;
    }
  else 
    {
      sf->last = (MUS_SAMPLE_TYPE *)(cur_snd->buffered_data + cur_snd->io[SND_IO_BUFSIZ] - 1);
      sf->eof = 0;
    }
  sf->beg = cur_snd->io[SND_IO_BEG];
  sf->end = cur_snd->io[SND_IO_END];
}

void file_buffers_back(int ind0, int ind1, int indx, snd_fd *sf, snd_data *cur_snd)
{
  if ((indx > cur_snd->io[SND_IO_END]) || 
      (indx < cur_snd->io[SND_IO_BEG])) 
    reposition_file_buffers(cur_snd, indx - cur_snd->io[SND_IO_BUFSIZ] + 1);
  sf->view_buffered_data = (MUS_SAMPLE_TYPE *)(cur_snd->buffered_data + indx - cur_snd->io[SND_IO_BEG]);
  if (ind1 <= cur_snd->io[SND_IO_END])
    sf->last = (MUS_SAMPLE_TYPE *)(cur_snd->buffered_data + ind1 - cur_snd->io[SND_IO_BEG]);
  else sf->last = (MUS_SAMPLE_TYPE *)(cur_snd->buffered_data + cur_snd->io[SND_IO_BUFSIZ] - 1);
  if (ind0 >= cur_snd->io[SND_IO_BEG]) 
    {
      sf->first = (MUS_SAMPLE_TYPE *)(cur_snd->buffered_data + ind0 - cur_snd->io[SND_IO_BEG]);
      sf->eof = 1;
    }
  else 
    {
      sf->first = cur_snd->buffered_data;
      sf->eof = 0;
    }
  sf->beg = cur_snd->io[SND_IO_BEG];
  sf->end = cur_snd->io[SND_IO_END];
}

MUS_SAMPLE_TYPE snd_file_read_sample(snd_data *ur_sd, int index, chan_info *cp)
{
  int copied;
  MUS_SAMPLE_TYPE val;
  snd_data *sd = NULL;
  copied = 0;
  /* first try to grab the sample without moving any buffers */
  if ((index >= ur_sd->io[SND_IO_BEG]) && 
      (index <= ur_sd->io[SND_IO_END])) 
    return(ur_sd->buffered_data[index - ur_sd->io[SND_IO_BEG]]);
  /* not in current buffer, so create a new reader and go looking for it */
  if (ur_sd->inuse) 
    {
      sd = copy_snd_data(ur_sd, cp, 4); 
      copied = 1;
    } 
  else sd = ur_sd;
  sd->inuse = TRUE;
  if ((index < sd->io[SND_IO_BEG]) || 
      (index > sd->io[SND_IO_END])) 
    reposition_file_buffers(sd, index);
  val = sd->buffered_data[index - sd->io[SND_IO_BEG]];
  if (copied) 
    {
      sd->inuse = FALSE; 
      free_snd_data(sd);
    }
  return(val); 
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

static int close_temp_files(chan_info *cp, void *closed)
{
  int i, rtn;
  snd_data *sd;
  if (cp)
    {
      if (cp->sounds)
	{
	  rtn = (*((int *)closed));
	  for (i = 0; i < cp->sound_size; i++)
	    {
	      sd = cp->sounds[i];
	      if ((sd) && 
		  (sd->type == SND_DATA_FILE) && 
		  (sd->io) && 
		  (sd->open == FD_OPEN))
		{
		  mus_file_close(sd->io[SND_IO_FD]);
		  sd->open = FD_CLOSED;
		  sd->io[SND_IO_FD] = -1;
		  rtn++;
		}
	    }
	  (*((int *)closed)) = rtn;
	}
    }
  return(0);
}

static int too_many_files_cleanup(snd_state *ss)
{
  int *closed;
  int rtn;
  rtn = -1;
  closed = (int *)MALLOC(sizeof(int));
  (*closed) = 0;
  map_over_chans(ss, close_temp_files, (void *)closed);
  if ((*closed) == 0) 
    rtn = -1; 
  else rtn = (*closed);
  FREE(closed);
  return(rtn);
}

int snd_open_read(snd_state *ss, const char *arg) 
{
  int fd;
  fd = open(arg, O_RDONLY, 0);
  if ((fd == -1) && (errno == EMFILE))
    {
      fd = too_many_files_cleanup(ss);
      if (fd != -1) 
	fd = open(arg, O_RDONLY, 0);
      if (fd == -1) 
	snd_error("%s: %s", arg, strerror(errno));
    }
  return(fd);
}

int snd_overwrite_ok(snd_state *ss, const char *ofile)
{
  int fil, rtn = 1;
  if (ask_before_overwrite(ss))
    {
#ifndef _MSC_VER
      fil = open(ofile, O_RDONLY, O_NONBLOCK);
#else
      fil = open(ofile, O_RDONLY);
#endif
      if (fil != -1) 
	{
	  close(fil);
	  rtn = snd_yes_or_no_p(ss, "%s exists. Overwrite?", ofile);
	}
    }
  return(rtn);
}

int snd_reopen_write(snd_state *ss, const char *arg)
{
  int fd;
  fd = open(arg, O_RDWR, 0);
  if ((fd == -1) && 
      (errno == EMFILE))
    {
      fd = too_many_files_cleanup(ss);
      if (fd != -1) 
	fd = open(arg, O_RDWR, 0);
      if (fd == -1) 
	snd_error("%s: %s", arg, strerror(errno));
    }
  return(fd);
}

int snd_write_header(snd_state *ss, const char *name, int type, int srate, int chans, int loc, int size, int format, const char *comment, int len, int *loops)
{
  int fd;
  mus_sound_forget(name);
  mus_header_set_aiff_loop_info(loops);
  fd = mus_header_write(name, type, srate, chans, loc, size, format, comment, len);
  if (fd == -1)
    {
      if (errno == EMFILE) /* 0 => no error (fd not actually returned unless it's -1) */
	{
	  fd = too_many_files_cleanup(ss);
	  if (fd != -1) 
	    fd = mus_header_write(name, type, srate, chans, loc, size, format, comment, len);
	}
      if (fd == -1) 
	mus_error(MUS_CANT_OPEN_FILE, "%s: %s", name, strerror(errno));
    }
  mus_header_set_aiff_loop_info(NULL);
  return(fd);
}

int snd_remove(const char *name)
{
  int err = 0;
  mus_sound_forget(name); /* no error here if not in sound tables */
  err = remove(name);
  if (err == -1)
    snd_warning("can't remove %s: %s", name, strerror(errno));
  return(err);
}
