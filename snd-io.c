#include "snd.h"

/* extracted from CLM's cmus.c and changed slightly for Snd */

static void c_io_bufclr (int *io, int *datai, int beg)
{
  int i,k,end;
  MUS_SAMPLE_TYPE *j;
  if (io[SND_IO_DATS+SND_AREF_BLOCK] == 0)
    snd_error("attempt to clear deallocated IO buffer");
  else
    {
      end=io[SND_IO_BUFSIZ];
      for (k=0;k<io[SND_IO_CHANS];k++)
	{
	  j=MUS_SAMPLE_ARRAY(datai[io[SND_IO_DATS+SND_AREF_BLOCK]+k]);
	  if (j) {for (i=beg; i<end;i++) j[i]=MUS_SAMPLE_0;}
	}
    }
}

void mus_file_reset(int loc0, int *io, int *datai)
{
  /* called when loc is outside the current in-core frame for the file pointed to by io */
  /* equivalent to the io.lisp function file-check + read-in */
  int file_end,bytes,loc;
  int bufend,filbytes;
#if LONG_INT_P
  int i;
  MUS_SAMPLE_TYPE **bufs;
#endif
  loc = loc0;
  if (io[SND_IO_DIR] != SND_IO_IN_FILE) 
    {
      if ((io[SND_IO_FD] != FALSE) &&
	  (io[SND_IO_DATA_END] != 0) &&
	  (io[SND_IO_DATA_START] != io[SND_IO_DATA_END]))
	{
	  if (io[SND_IO_DATA_END] > io[SND_IO_BUFSIZ])
	    snd_error("data end indication in IO buffer is too big: %d > %d",io[SND_IO_DATA_END],io[SND_IO_BUFSIZ]);
	  else
	    {
	      mus_file_seek(io[SND_IO_FD],io[SND_IO_HDR_END]+(2*io[SND_IO_CHANS]*(io[SND_IO_BEG]+io[SND_IO_DATA_START])),SEEK_SET);
	      if ((io[SND_IO_DATA_START] < 0) || (io[SND_IO_DATA_END] < 0)) 
		{
		  snd_error("file buffer index is negative! -- will try to fix it...");
		  if (io[SND_IO_DATA_START] < 0) io[SND_IO_DATA_START] = 0; else io[SND_IO_DATA_END] = 0;
		}
#if LONG_INT_P
	      bufs = (MUS_SAMPLE_TYPE **)CALLOC(io[SND_IO_CHANS],sizeof(MUS_SAMPLE_TYPE *));
	      for (i=0;i<io[SND_IO_CHANS];i++) bufs[i] = MUS_SAMPLE_ARRAY(datai[io[SND_IO_DATS + SND_AREF_BLOCK]+i]);
	      mus_file_write(io[SND_IO_FD],io[SND_IO_DATA_START],io[SND_IO_DATA_END],io[SND_IO_CHANS],bufs);
	      FREE(bufs);
#else
	      mus_file_write(io[SND_IO_FD],io[SND_IO_DATA_START],io[SND_IO_DATA_END],io[SND_IO_CHANS],(MUS_SAMPLE_TYPE **)(datai+io[SND_IO_DATS + SND_AREF_BLOCK]));
#endif
	      io[SND_IO_DATA_START] = io[SND_IO_DATA_END];
	      if (io[SND_IO_SIZE] <= (io[SND_IO_BEG] + io[SND_IO_DATA_END])) io[SND_IO_SIZE] = (io[SND_IO_BEG] + 1 + io[SND_IO_DATA_END]);
	    }
	}
    }
  if ((loc < io[SND_IO_BEG]) && ((loc + (int)(.9*io[SND_IO_BUFSIZ])) > io[SND_IO_BEG]))
    {
      if ((loc + 10) > io[SND_IO_BEG]) loc -= (int)(.75*io[SND_IO_BUFSIZ]);
      if (loc < 0) loc = 0;
      if (io[SND_IO_CHANS] == 1) loc = (2 * (int)(loc / 2));
    }
  file_end = io[SND_IO_SIZE];
  bytes = file_end - loc;
  if (bytes > io[SND_IO_BUFSIZ]) bytes=io[SND_IO_BUFSIZ];
  if (bytes < 0)                   /* tried to access beyond current end of file */
    {
      if (io[SND_IO_DIR] == SND_IO_IN_FILE) {io[SND_IO_BEG]=loc; c_io_bufclr(io,datai,0);} /* different from CLM */
      else
	{
	  c_io_bufclr(io,datai,0);
	  bytes = io[SND_IO_BUFSIZ];
	  io[SND_IO_DATA_START] = 0;
	  io[SND_IO_DATA_END] = 0;
	  mus_file_seek(io[SND_IO_FD],0,SEEK_END);
	  if (io[SND_IO_CHANS] != 1)
	    {
	      mus_file_write_zeros(io[SND_IO_FD],io[SND_IO_CHANS]*(loc-file_end)); 
	      io[SND_IO_BEG]=loc;
	    }
	  else
	    {
	      mus_file_write_zeros(io[SND_IO_FD],loc-file_end); 
	      if ((loc%2)==0) io[SND_IO_BEG]=loc; 
	      else io[SND_IO_BEG]=loc-1;
	    }
	}
    }
  else /* bytes is positive or 0 */
    {
      mus_file_seek(io[SND_IO_FD],io[SND_IO_HDR_END]+(2*io[SND_IO_CHANS]*loc),SEEK_SET);
      io[SND_IO_BEG] = loc;
      if (bytes > 0) 
	{
	  if (bytes > io[SND_IO_BUFSIZ]) 
	    snd_error("input request is too big: %d", bytes);
	  else
	    {
#if LONG_INT_P
	      bufs = (MUS_SAMPLE_TYPE **)CALLOC(io[SND_IO_CHANS],sizeof(MUS_SAMPLE_TYPE *));
	      for (i=0;i<io[SND_IO_CHANS];i++) bufs[i] = MUS_SAMPLE_ARRAY(datai[io[SND_IO_DATS + SND_AREF_BLOCK]+i]);
	      mus_file_read_chans(io[SND_IO_FD],0,bytes-1,io[SND_IO_CHANS],bufs,(MUS_SAMPLE_TYPE *)bufs);
	      FREE(bufs);
#else
	      mus_file_read_chans(io[SND_IO_FD],0,bytes-1,io[SND_IO_CHANS],
			     (MUS_SAMPLE_TYPE **)(datai+io[SND_IO_DATS + SND_AREF_BLOCK]),
			     (MUS_SAMPLE_TYPE *)(datai+io[SND_IO_DATS + SND_AREF_BLOCK]));
	      /* too clever -- I'm using the array of pointers to data buffers as the channel chooser as well */
#endif
	    }
	}
      if (bytes < io[SND_IO_BUFSIZ]) c_io_bufclr(io,datai,bytes);
      io[SND_IO_DATA_START] = 0;
      io[SND_IO_DATA_END] = 0;
      if (io[SND_IO_DIR] == SND_IO_IN_FILE)
	{
	  bufend = io[SND_IO_BUFSIZ]-1;
	  filbytes = file_end-io[SND_IO_BEG]-1;
	  if (filbytes < bufend) bufend = filbytes;
	  if (bufend > 0) io[SND_IO_DATA_END] = bufend;
	}
    }
  io[SND_IO_END] = io[SND_IO_BEG]+io[SND_IO_BUFSIZ]-1;
  /* io[SND_IO_LOC] = (loc0-io[SND_IO_BEG]); */
}

/* now wrappers for low level sndlib open/close/access functions -- we can't use
 * the sndlib versions directly because in some cases, Snd has more than FOPEN_MAX
 * files nominally open and accessible (mix temps in with-sound explode for example).
 * these wrappers provide checks for EMFILE as errno from open and try to close
 * temps to make room. 
 *
 * there is a hidden limit that might come into play if FOPEN_MAX > MUS_FILE_DESCRIPTORS (see io.c)
 * on the SGI, FOPEN_MAX is 100, but we can open many more files than that without hitting the EMFILE error.
 */

void snd_file_reset(snd_data *sd, int index)
{
  int fd = 0;
  int reclose = 0;
  file_info *hdr;
  if (sd->open == FD_CLOSED)
    {
      /* try to open it with sndlib descriptors */
      if (sd->io[SND_IO_DIR] == SND_IO_IN_FILE) 
	fd = mus_file_open_read(sd->filename); 
      else fd = mus_file_reopen_write(sd->filename);
      if (fd == -1) 
	{
	  /* our file has disappeared?!? */
	  snd_error("%s is unreadable? open: %s",sd->filename,strerror(errno));
	  return;
	}
      hdr = sd->hdr;
      /* these need to flush active data before hidden close and fixup the datai indices */
      mus_file_set_descriptors(fd,sd->filename,
			       hdr->format,mus_data_format_to_bytes_per_sample(hdr->format),hdr->data_location,
			       hdr->chans,hdr->type);
      during_open(fd,sd->filename,SND_REOPEN_CLOSED_FILE);
      /* fix up io[SND_IO_FD] and whatever else is clobbered by mus_file_close */
      sd->io[SND_IO_FD] = fd;
      sd->open = FD_OPEN;
      reclose = 1;
    }

  mus_file_reset(index,sd->io,sd->io);

  if (reclose)
    {
      snd_close(fd); 
      sd->open = FD_CLOSED; 
      sd->io[SND_IO_FD] = -1;
    }
}

static int close_temp_files(chan_info *cp, void *closed)
{
  int i,rtn;
  snd_data *sd;
  if (cp)
    {
      if (cp->sounds)
	{
	  rtn = (*((int *)closed));
	  for (i=0;i<cp->sound_size;i++)
	    {
	      sd = cp->sounds[i];
	      if ((sd) && (sd->type == SND_DATA_FILE) && (sd->io) && (sd->open == FD_OPEN))
		{
		  snd_close(sd->io[SND_IO_FD]);
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
  closed = (int *)CALLOC(1,sizeof(int));
  (*closed) = 0;
  map_over_chans(ss,close_temp_files,(void *)closed);
  if ((*closed) == 0) rtn = -1; else rtn = (*closed);
  FREE(closed);
  return(rtn);
}

int snd_open_read(snd_state *ss, char *arg) 
{
  int fd;
  fd = open(arg,O_RDONLY,0);
  if ((fd == -1) && (errno == EMFILE))
    {
      fd = too_many_files_cleanup(ss);
      if (fd != -1) fd = open(arg,O_RDONLY,0);
      if (fd == -1) snd_error(strerror(errno));
    }
  return(fd);
}

int snd_probe_file(char *name)
{
  if (mus_file_probe(name)) return(FILE_EXISTS);
  return(FILE_DOES_NOT_EXIST);
}

int snd_overwrite_ok(snd_state *ss, char *ofile)
{
  char *buf;
  int fil,rtn = 1;
  if (ask_before_overwrite(ss))
    {
#ifndef _MSC_VER
      fil = open(ofile,O_RDONLY,O_NONBLOCK);
#else
      fil = open(ofile,O_RDONLY);
#endif
      if (fil != -1) 
	{
	  close(fil);
	  buf = (char *)CALLOC(256,sizeof(char));
	  sprintf(buf,STR_exists_overwrite,ofile);
	  rtn = snd_yes_or_no_p(ss,buf);
	  FREE(buf);
	}
    }
  return(rtn);
}

#if 0
int snd_open_write(snd_state *ss, char *arg)
{ /* not currently used anywhere */
  int fd;
  if ((fd = open(arg,O_RDWR,0)) == -1)
    {
      fd = creat(arg,0666);
      if ((fd == -1) && (errno == EMFILE))
	{
	  fd = too_many_files_cleanup(ss);
	  if (fd != -1) fd = creat(arg,0666);
	  if (fd == -1) snd_error("%s: %s",arg,strerror(errno));
	}
    }
  else lseek(fd,0L,SEEK_END);
  return(fd);
}

int snd_create(snd_state *ss, char *arg)
{
  int fd;
  fd = creat(arg,0666);
  if ((fd == -1) && (errno == EMFILE))
    {
      fd = too_many_files_cleanup(ss);
      if (fd != -1) fd = creat(arg,0666);
      if (fd == -1) snd_error("%s: %s",arg,strerror(errno));
    }
  return(fd);
}
#endif

int snd_reopen_write(snd_state *ss, char *arg)
{
  int fd;
  fd = open(arg,O_RDWR,0);
  if ((fd == -1) && (errno == EMFILE))
    {
      fd = too_many_files_cleanup(ss);
      if (fd != -1) fd = open(arg,O_RDWR,0);
      if (fd == -1) snd_error("%s: %s",arg,strerror(errno));
    }
  return(fd);
}

void snd_close(int fd)
{
  mus_file_close_descriptors(fd);
  close(fd);
}

int snd_write_header(snd_state *ss, char *name, int type, int srate, int chans, int loc, int size, int format, char *comment, int len, int *loops)
{
  int fd;
  mus_sound_forget(name);
  mus_header_set_aiff_loop_info(loops);
  fd = mus_header_write(name,type,srate,chans,loc,size,format,comment,len);
  if ((fd == -1) && (errno == EMFILE)) /* 0 => no error (fd not actually returned unless it's -1) */
    {
      fd = too_many_files_cleanup(ss);
      if (fd != -1) fd = mus_header_write(name,type,srate,chans,loc,size,format,comment,len);
      if (fd == -1) snd_error("%s: %s",name,strerror(errno));
    }
  mus_header_set_aiff_loop_info(NULL);
  return(fd);
}
