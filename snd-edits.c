#include "snd.h"

/* -------------------------------- EDIT LISTS -------------------------------- *
 *
 * each channel has a list of lists containing the current edit history and the associated sound temp files or buffers
 * undo: back up current list position
 * redo: push position foward
 * No actual changes are flushed out to the file system until the file is saved.
 *
 * the three editing possibilities are insert, change, delete.  All input goes through these lists.
 */

/* ed_list fields accessed only in this file */

#if (!HAVE_GUILE) || (HAVE_GUILE_1_3_0)
static int dont_edit(chan_info *cp) {return(0);}
static void call_undo_hook(chan_info *cp, int undo) {return;}
static int dont_save(snd_state *ss, snd_info *sp, char *newname) {return(0);}
#else
static int dont_edit(chan_info *cp);
static void call_undo_hook(chan_info *cp, int undo);
static int dont_save(snd_state *ss, snd_info *sp, char *newname);
#endif

static char edit_buf[256];

/* each block in an edit-list list describes one fragment of the current sound */
#define ED_OUT 0
#define ED_SND 1
#define ED_BEG 2
#define ED_END 3
#define ED_SIZE 4
#define EDIT_LIST_END_MARK -2
#define EDIT_ALLOC_SIZE 128

/* ED_SND is either an index into the cp->sounds array (snd_data structs) or EDIT_LIST_END_MARK */
/* ED_BEG and ED_END are indices into the associated data => start/end point of data used in current segment */
/* ED_OUT is running segment location within current overall edited data */
/* EDIT_ALLOC_SIZE is the allocation amount (pointers) each time cp->sounds is (re)allocated */

#define INSERTION_EDIT 0
#define DELETION_EDIT 1
#define CHANGE_EDIT 2
#define INITIALIZE_EDIT 3
#define PACK_EDIT(a,b) ((a)<<16 | (b))
#define EDIT_TYPE(a) (((a) >> 16) & 0xffff)
#define EDIT_LOCATION(a) ((a) & 0xffff)
/* edit history decoding info */

/* color could be added at this level ED_COLOR 4, ED_SIZE 5, then sf->cb[ED_COLOR] during edit tree read */

static char *edit_names[4] = {"insert","delete","set",""};

static void display_ed_list(chan_info *cp, int i, ed_list *ed)
{
  int len,j,type,index;
  snd_data *sd;
  len=ed->size; /* number of fragments in this list */
  type = EDIT_TYPE(ed->sfnum);
  switch (type)
    {
    case INSERTION_EDIT: fprintf(stderr,"\n (insert %d %d) ",ed->beg,ed->len); break;
    case DELETION_EDIT: fprintf(stderr,"\n (delete %d %d) ",ed->beg,ed->len); break;
    case CHANGE_EDIT: fprintf(stderr,"\n (set %d %d) ",ed->beg,ed->len); break;
    case INITIALIZE_EDIT: fprintf(stderr,"\n (begin) "); break;
    }
  if (ed->origin) fprintf(stderr,"; %s ",ed->origin);
  fprintf(stderr,"[%d:%d]:",i,len);
  for (j=0;j<len;j++)
    {
      fprintf(stderr,"\n   (at %d, cp->sounds[%d][%d:%d])",
	      ed->fragments[j*ED_SIZE+ED_OUT],
	      index = ed->fragments[j*ED_SIZE+ED_SND],
	      ed->fragments[j*ED_SIZE+ED_BEG],
	      ed->fragments[j*ED_SIZE+ED_END]);
      if (index != EDIT_LIST_END_MARK)
	{
	  sd = cp->sounds[index];
	  if (sd == NULL) 
	    fprintf(stderr," [nil!]");
	  else 
	    if (sd->type == SND_DATA_FILE)
	      fprintf(stderr," [file: %s[%d] %d %d %p %d]",sd->filename,sd->chan,sd->inuse,sd->copy,sd->owner,sd->edit_ctr);
	    else 
	      if (sd->type == SND_DATA_BUFFER)
		fprintf(stderr," [buf (%p): %d %d] ",sd->data,sd->len/4,sd->edit_ctr);
	      else fprintf(stderr," [bogus!]");
	}
    }
}

int edit_changes_begin_at(chan_info *cp)
{
  ed_list *ed,*old_ed;
  int len,old_len,i,min_len;
  old_ed = cp->edits[cp->edit_ctr-1];
  ed = cp->edits[cp->edit_ctr];
  old_len = old_ed->size;
  len = ed->size;
  if (len < old_len) min_len = (len*ED_SIZE); else min_len = (old_len*ED_SIZE);
  for (i=0;i<min_len;i+=ED_SIZE)
    if ((ed->fragments[i+ED_OUT] != old_ed->fragments[i+ED_OUT]) || 
	(ed->fragments[i+ED_SND] != old_ed->fragments[i+ED_SND]) || 
	(ed->fragments[i+ED_BEG] != old_ed->fragments[i+ED_BEG]))
      return(ed->fragments[i+ED_OUT]);
  return(0);
}

int edit_changes_end_at(chan_info *cp)
{
  ed_list *ed,*old_ed;
  int len,old_len;
  old_ed = cp->edits[cp->edit_ctr-1];
  ed = cp->edits[cp->edit_ctr];
  old_len = old_ed->size - 1;
  len = ed->size - 1;
  if (ed->fragments[len*ED_SIZE + ED_SND] == EDIT_LIST_END_MARK) len--;
  if (old_ed->fragments[old_len*ED_SIZE + ED_SND] == EDIT_LIST_END_MARK) old_len--;
  while ((len>=0) && (old_len>=0))
    {
      if ((ed->fragments[len*ED_SIZE+ED_SND] == old_ed->fragments[old_len*ED_SIZE+ED_SND]) &&
	  (ed->fragments[len*ED_SIZE+ED_END] == old_ed->fragments[old_len*ED_SIZE+ED_END]))
	{
	  if (ed->fragments[len*ED_SIZE+ED_BEG] != old_ed->fragments[old_len*ED_SIZE+ED_BEG])
	    return(cp->samples[cp->edit_ctr - 1] - (ed->fragments[len*ED_SIZE + ED_END] - ed->fragments[len*ED_SIZE + ED_BEG]));
	  len--;
	  old_len--;
	}
      else break;
    }
  return(0);
}

static char edbuf[128];

char *edit_to_string(chan_info *cp, int edit)
{
  ed_list *ed;
  ed = cp->edits[edit];
  /* only for edit list in snd-xchn.c */
  sprintf(edbuf,"%s : (%s %d %d)",ed->origin,edit_names[EDIT_TYPE(ed->sfnum)],ed->beg,ed->len);
  return(edbuf);
}

static void display_edits(chan_info *cp)
{
  int eds,i;
  ed_list *ed;
  eds = cp->edit_ctr;
  fprintf(stderr,"\nEDITS: %d\n",eds);
  for (i=0;i<=eds;i++)
    {
      ed = cp->edits[i];
      if (!ed) 
	fprintf(stderr,"\nedit_ctr is %d, but [%d] is nil!",eds,i);
      else display_ed_list(cp,i,ed);
    }
}

static void edit_data_to_file(FILE *fd, ed_list *ed, chan_info *cp)
{
  snd_data *sf;
  snd_state *ss;
  char *newname;
  int i,snd,err;
  snd = EDIT_LOCATION(ed->sfnum);
  if (snd < cp->sound_size)
    {
      sf = cp->sounds[snd];
      if (sf->type == SND_DATA_BUFFER)
	{
	  fprintf(fd,"#(");
	  for (i=0;i<ed->len;i++) 
#if SNDLIB_USE_FLOATS
	    fprintf(fd,"%f ",sf->data[i]);
#else
	    fprintf(fd,"%d ",sf->data[i]);
#endif
	  fprintf(fd,")");
	}
      else
	{
	  ss = cp->state;
	  if (save_dir(ss))
	    {
	      newname = shorter_tempnam(save_dir(ss),"snd_");
	      err = snd_copy_file(sf->filename,newname);
	      fprintf(fd,"\"%s\"",newname);
	      FREE(newname);
	    }
	  else
	    {
	      /* read at very low level */
	      int ifd,idataloc,bufnum,n,cursamples,samples,sample;
	      MUS_SAMPLE_TYPE *buffer;
	      MUS_SAMPLE_TYPE **ibufs;
	      fprintf(fd,"#(");
	      ifd = mus_file_open_read(sf->filename);
	      idataloc = mus_sound_data_location(sf->filename);
	      mus_file_set_descriptors(ifd,sf->filename,
				       mus_sound_data_format(sf->filename),mus_sound_datum_size(sf->filename),idataloc,
				       mus_sound_chans(sf->filename),mus_sound_header_type(sf->filename));
	      samples = mus_sound_samples(sf->filename);
	      mus_file_seek(ifd,idataloc,SEEK_SET);
	      ibufs = (MUS_SAMPLE_TYPE **)CALLOC(1,sizeof(MUS_SAMPLE_TYPE *));
	      ibufs[0] = (MUS_SAMPLE_TYPE *)CALLOC(FILE_BUFFER_SIZE,sizeof(MUS_SAMPLE_TYPE));
	      bufnum = (FILE_BUFFER_SIZE);
	      sample = 0;
	      for (n=0;n<samples;n+=bufnum)
		{
		  if ((n+bufnum)<samples) cursamples = bufnum; else cursamples = (samples-n);
		  mus_file_read(ifd,0,cursamples-1,1,ibufs);
		  buffer = (MUS_SAMPLE_TYPE *)(ibufs[0]);
		  for (i=0;i<cursamples;i++) 
		    {
#if SNDLIB_USE_FLOAT
		      fprintf(fd,"%f ",MUS_SAMPLE_TO_FLOAT(buffer[i]));
#else
		      fprintf(fd,"%d ",MUS_SAMPLE_TO_INT(buffer[i]));
#endif
		      sample++;
		      if (sample == ed->len)
			{
			  mus_file_close(ifd);
			  fprintf(fd,")");
			  return;
			}
		    }
		}
	      fprintf(fd,")");
	      mus_file_close(ifd);
	    }
	}
    }
}

void edit_history_to_file(FILE *fd, chan_info *cp)
{
  /* write edit list as a snd-guile program to fd (open for writing) for subsequent load */
  /* this needs only 3 operations: delete-samples, insert-samples, and change-samples */
  /*   the actual user-operations that produced these are included as comments */
  /*   the data is included as a vector, so the file can be very large! */
  /*   the entire current list is written, then the edit_ctr is fixed up to reflect its current state */
  int i,edits;
  ed_list *ed;
  edits = cp->edit_ctr;
  while ((edits<(cp->edit_size-1)) && (cp->edits[edits+1])) edits++;
  /* 0 case = open-sound */
  for (i=1;i<=edits;i++)
    {
      ed = cp->edits[i];
      if (ed)
	{
	  switch (EDIT_TYPE(ed->sfnum))
	    {
	    case INSERTION_EDIT: 
	      /* samp data snd chn */
	      fprintf(fd,"      (%s %d %d \"%s\" ",S_insert_samples_with_origin,ed->beg,ed->len,(ed->origin) ? ed->origin : "");
	      edit_data_to_file(fd,ed,cp);
	      fprintf(fd," sfile %d)\n",cp->chan);
	      break;
	    case DELETION_EDIT:
	      /* samp samps snd chn */
	      fprintf(fd,"      (%s %d %d \"%s\" sfile %d)\n",S_delete_samples_with_origin,ed->beg,ed->len,(ed->origin) ? ed->origin : "",cp->chan);
	      break;
	    case CHANGE_EDIT:
	      fprintf(fd,"      (%s %d %d \"%s\" ",S_change_samples_with_origin,ed->beg,ed->len,(ed->origin) ? ed->origin : "");
	      edit_data_to_file(fd,ed,cp);
	      fprintf(fd," sfile %d)\n",cp->chan);
	      break;
	    }
	}
    }
  if (cp->edit_ctr < edits) fprintf(fd,"      (undo %d sfile %d)\n",edits-cp->edit_ctr,cp->chan);
  save_mark_list(fd,cp);
}

static void copy_ed_blocks(int *new_list, int *old_list, int new_beg, int old_beg, int num_lists)
{
  /* beg and num_lists here are in terms of blocks, not ints */
  int i,end,k;
  if (num_lists > 0)
    {
      end = (old_beg+num_lists)*ED_SIZE;
      for (k=new_beg*ED_SIZE,i=old_beg*ED_SIZE;i<end;i++,k++) {new_list[k] = old_list[i];}
    }
}

static ed_list *make_ed_list(int size)
{
  ed_list *ed;
  ed=(ed_list *)CALLOC(1,sizeof(ed_list));
  ed->size = size;
  ed->fragments = (int *)CALLOC(size*ED_SIZE,sizeof(int));
  ed->origin = NULL;
  return(ed);
}

static ed_list *free_ed_list(ed_list *ed)
{
  if (ed)
    {
      if (ed->fragments) FREE(ed->fragments);
      if (ed->origin) FREE(ed->origin);
      FREE(ed);
    }
  return(NULL);
}

void backup_edit_list(chan_info *cp)
{
  int cur,i;
  snd_data *sf;
  cur = cp->edit_ctr;
  if (cur <= 0) return;
  free_ed_list(cp->edits[cur - 1]);
  free_amp_env(cp,cur-1);
  cp->edits[cur - 1] = cp->edits[cur];
  cp->amp_envs[cur-1] = cp->amp_envs[cur];
  cp->edits[cur] = NULL;
  cp->amp_envs[cur] = NULL;
  cp->samples[cur - 1] = cp->samples[cur];
  if (cp->sounds)
    { /* protect from release_pending_sounds upon edit after undo after as-one-edit or whatever */
      for (i=0;i<cp->sound_size;i++)
	{
	  sf = cp->sounds[i];
	  if ((sf) && (sf->edit_ctr == cur)) sf->edit_ctr--;
	}
    }
  /* marks backup added 23-Jun-00 */
  if (cp->marks)
    {
      release_pending_marks(cp,cur - 1);
      cp->marks[cur - 1] = cp->marks[cur];
      cp->marks[cur] = NULL;
      cp->mark_ctr[cur - 1] = cp->mark_ctr[cur];
      cp->mark_ctr[cur] = -1;
    }
  cp->edit_ctr--;
  reflect_edit_history_change(cp);
}

void free_edit_list(chan_info *cp)
{
  int i;
  if (cp)
    {
      if (cp->edits)
	{
	  for (i=0;i<cp->edit_size;i++)
	    {
	      /* cp->edit_ctr follows current edit state (redo/undo) */
	      if (cp->edits[i]) free_ed_list(cp->edits[i]);
	      if (cp->amp_envs[i]) free_amp_env(cp,i);
	    }
	  FREE(cp->edits);
	  FREE(cp->amp_envs);
	}
      cp->edits = NULL;
      cp->amp_envs = NULL;
      cp->edit_ctr = -1;
      cp->edit_size = 0;
    }
}

static ed_list *initial_ed_list(int beg, int end)
{
  ed_list *ed;
  ed = make_ed_list(2);
  ed->beg = beg;
  ed->len = end+1;
  ed->sfnum = PACK_EDIT(INITIALIZE_EDIT,0);
  /* origin (channel %s %d) desc channel should be obvious from context */
  ed->fragments[0*ED_SIZE + ED_BEG] = beg;
  ed->fragments[0*ED_SIZE + ED_END] = end;
  ed->fragments[0*ED_SIZE + ED_SND] = 0;
  ed->fragments[0*ED_SIZE + ED_OUT] = 0;
  /* second block is our end-of-tree marker */
  ed->fragments[1*ED_SIZE + ED_BEG] = 0;
  ed->fragments[1*ED_SIZE + ED_END] = 0;
  ed->fragments[1*ED_SIZE + ED_SND] = EDIT_LIST_END_MARK;
  ed->fragments[1*ED_SIZE + ED_OUT] = end+1;
  return(ed);
}

void allocate_ed_list(chan_info *cp) {cp->edits = (ed_list **)CALLOC(cp->edit_size,sizeof(ed_list *));}
void set_initial_ed_list(chan_info *cp,int len) {cp->edits[0] = initial_ed_list(0,len);}

static int find_split_loc (chan_info *cp, int samp, ed_list *current_state)
{
  int i,k;
  if (!current_state) 
#if DEBUGGING
    {fprintf(stderr,"%s[%d] %s: current state missing!",__FILE__,__LINE__,__FUNCTION__); abort();}
#else
    return(0);
#endif
  for (i=0,k=0;i<current_state->size;i++,k+=ED_SIZE)
    {
      if (current_state->fragments[k+ED_OUT] >= samp) return(i);
    }
  fprintf(stderr,"%s[%d] %s: failed at %d!\n",__FILE__,__LINE__,__FUNCTION__,samp); 
  fprintf(stderr,"looking at: ");
  display_ed_list(cp,-1,current_state);
  fprintf(stderr,"\n in: \n");
  display_edits(cp);
  abort();
  return(0); /* make sgi compiler happy */
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

void remember_temp(char *filename, int chans)
{
  int i;
  tempfile_ctr *tmp;
  if (tempfiles_size == 0)
    {
      tempfiles_size = 8;
      tempfiles = (tempfile_ctr **)CALLOC(tempfiles_size,sizeof(tempfile_ctr *));
      i = 0;
    }
  else
    {
      for (i=0;i<tempfiles_size;i++)
	if (tempfiles[i] == NULL)
	  break;
      if (i >= tempfiles_size)
	{
	  tempfiles = (tempfile_ctr **)REALLOC(tempfiles,tempfiles_size * sizeof(tempfile_ctr *));
	  for (i=tempfiles_size;i<(tempfiles_size+8);i++) tempfiles[i] = NULL;
	  i = tempfiles_size;
	  tempfiles_size += 8;
	}
    }
  tmp = (tempfile_ctr *)CALLOC(1,sizeof(tempfile_ctr));
  tempfiles[i] = tmp;
  tmp->name = copy_string(filename);
  tmp->chans = chans;
  tmp->ticks = (int *)CALLOC(chans,sizeof(int));
}

static void forget_temp(char *filename, int chan)
{
  int i,j,happy=0;
  tempfile_ctr *tmp;
  for (i=0;i<tempfiles_size;i++)
    {
      tmp = tempfiles[i];
      if ((tmp) && (strcmp(filename,tmp->name) == 0))
	{
	  tmp->ticks[chan]--;
	  for (j=0;j<tmp->chans;j++)
	    if (tmp->ticks[j] > 0) 
	      {
		happy = 1;
		return;
	      }
	  if (happy == 0)
	    {
	      remove(tmp->name);
	      mus_sound_forget(tmp->name);
	      FREE(tmp->name);
	      FREE(tmp->ticks);
	      tempfiles[i] = NULL;
	    }
	  return;
	}
    }
#if DEBUGGING
  fprintf(stderr,"lost temp: %s?",filename);
  abort();
#endif
}

static void tick_temp(char *filename, int chan)
{
  int i;
  tempfile_ctr *tmp;
  for (i=0;i<tempfiles_size;i++)
    {
      tmp = tempfiles[i];
      if ((tmp) && (strcmp(filename,tmp->name) == 0))
	{
	  tmp->ticks[chan]++;
	  return;
	}
    }
#if DEBUGGING
  fprintf(stderr,"lost temp: %s?", filename);
  abort();
#endif
}

void forget_temps(void)
{
  int i;
  tempfile_ctr *tmp;
  for (i=0;i<tempfiles_size;i++)
    {
      tmp = tempfiles[i];
      if (tmp) 
	{
#if DEBUGGING
	  fprintf(stderr,"somehow %s got overlooked? ",tmp->name);	  
#endif
	  remove(tmp->name);
	}
    }
}

#if DEBUGGING
static snd_data **active_sds=NULL;
static int active_sds_size=0;
static void remember_sd(snd_data *sd)
{
  int i;
  if (active_sds == NULL)
    {
      active_sds_size = 256;
      active_sds = (snd_data **)CALLOC(active_sds_size,sizeof(snd_data *));
      active_sds[0] = sd;
      return;
    }
  for (i=0;i<active_sds_size;i++)
    if (active_sds[i] == NULL)
      {
	active_sds[i] = sd;
	return;
      }
  active_sds = (snd_data **)REALLOC(active_sds,(active_sds_size + 256) * sizeof(snd_data *));
  for (i=active_sds_size;i<active_sds_size+256;i++) active_sds[i]=NULL;
  active_sds[active_sds_size] = sd;
  active_sds_size += 256;
}
static void forget_sd(snd_data *sd)
{
  int i;
  for (i=0;i<active_sds_size;i++)
    if (sd == active_sds[i])
      {
	active_sds[i] = NULL;
	return;
      }
  fprintf(stderr,"attempt to forget unknown pointer?");
  abort();
}
#endif

snd_data *make_snd_data_file(char *name, int *io, MUS_SAMPLE_TYPE *data, file_info *hdr, int temp, int ctr, int temp_chan)
{
  snd_data *sf;
  sf = (snd_data *)CALLOC(1,sizeof(snd_data));
  sf->type = SND_DATA_FILE;
  sf->data = data;
  sf->io = io;
  sf->filename = copy_string(name);
  sf->hdr = hdr;
  sf->temporary = temp;
  if (temp == MULTICHANNEL_DELETION) tick_temp(name,temp_chan);
  sf->edit_ctr = ctr;
  sf->open = FD_OPEN;
  sf->inuse = FALSE;
  sf->copy = FALSE;
  sf->chan = temp_chan;
  sf->len = (hdr->samples)*(mus_data_format_to_bytes_per_sample(hdr->format)) + hdr->data_location;
  sf->owner = NULL; 
#if DEBUGGING
  remember_sd(sf);
#endif
  return(sf);
}

static snd_data *copy_snd_data(snd_data *sd, chan_info *cp, int bufsize)
{
  snd_data *sf;
  int *datai;
  int fd;
  file_info *hdr;
  hdr = sd->hdr;
  fd = snd_open_read(cp->state,sd->filename);
  if (fd == -1) return(NULL);
  mus_file_set_descriptors(fd,sd->filename,
			   hdr->format,mus_data_format_to_bytes_per_sample(hdr->format),hdr->data_location,
			   hdr->chans,hdr->type);
  during_open(fd,sd->filename,SND_COPY_READER);
  datai = make_file_state(fd,hdr,SND_IO_IN_FILE,sd->chan,bufsize);
  sf = (snd_data *)CALLOC(1,sizeof(snd_data));
  sf->type = sd->type;
  sf->data = MUS_SAMPLE_ARRAY(datai[SND_IO_DATS + SND_AREF_HEADER_SIZE+sd->chan]);
  sf->io = datai;
  sf->filename = copy_string(sd->filename);
  sf->hdr = hdr;
  sf->temporary = DONT_DELETE_ME;
  sf->edit_ctr = sd->edit_ctr;
  sf->open = FD_OPEN;
  sf->inuse = FALSE;
  sf->copy = TRUE;
  sf->owner = NULL;
#if DEBUGGING
  remember_sd(sf);
#endif
  return(sf);
}

snd_data *make_snd_data_buffer(MUS_SAMPLE_TYPE *data, int len, int ctr)
{
  snd_data *sf;
  int i;
  sf = (snd_data *)CALLOC(1,sizeof(snd_data));
  sf->type = SND_DATA_BUFFER;
  sf->data = (MUS_SAMPLE_TYPE *)CALLOC(len+1,sizeof(MUS_SAMPLE_TYPE));
  /* sigh... using len+1 rather than len to protect against access to inserted buffer at end mixups (final fragment uses end+1) */
  /*   the real problem here is that I never decided whether insert starts at the cursor or just past it */
  /*   when the cursor is on the final sample, this causes cross-fragment ambiguity as to the length of a trailing insertion */
  /*   C > (make-region 1000 2000) (insert-region (cursor)) C-v hits this empty slot and gets confused about the previously final sample value */
  for (i=0;i<len;i++) sf->data[i] = data[i];
  sf->edit_ctr = ctr;
  sf->copy = FALSE;
  sf->inuse = FALSE;
  sf->len = len*4;
  sf->owner = NULL;
#if DEBUGGING
  remember_sd(sf);
#endif
  return(sf);
}

static snd_data *free_snd_data(snd_data *sf)
{
  /* in the snd file case, these pointers are dealt with elsewhere (where??) */
  if (sf)
    {
#if DEBUGGING
      forget_sd(sf);
#endif
      if (sf->temporary == ALREADY_DELETED)
	return(NULL);
      if (sf->temporary == MULTICHANNEL_DELETION)
	forget_temp(sf->filename,sf->chan);
      if ((sf->type != SND_DATA_BUFFER) && (sf->type != SND_DATA_FILE)) 
	{
#if DEBUGGING
	  snd_error("%s[%d] %s: data type of %p = %d?",__FILE__,__LINE__,__FUNCTION__,sf,sf->type);
	  abort();
#endif
	  return(NULL); /* if not debugging, try to go on... */
	}
      if ((sf->type == SND_DATA_BUFFER) && (sf->data)) FREE(sf->data);
      sf->data = NULL;
      if ((!(sf->copy)) && (sf->hdr)) free_file_info(sf->hdr);
      sf->hdr = NULL;
      if (sf->io)
	{
	  if (sf->open == FD_OPEN) snd_close(sf->io[SND_IO_FD]);
	  sf->io = free_file_state(sf->io);
	  if (sf->temporary == DELETE_ME) 
	    {
#if DEBUGGING
	      {
		int i;
		snd_data *tmp;
		for (i=0;i<active_sds_size;i++)
		  {
		    tmp = active_sds[i];
		    if ((tmp) &&
			(tmp->type == SND_DATA_FILE) &&
			(strcmp(sf->filename,tmp->filename) == 0))
		      {
			fprintf(stderr,"about to delete in-use temp!!");
			abort();
		      }
		  }
	      }
#endif
	      remove(sf->filename);
	      mus_sound_forget(sf->filename);
	    }
	  FREE(sf->filename);
	  sf->filename = NULL;
	}
      sf->temporary = ALREADY_DELETED;
      sf->owner = NULL;
      FREE(sf);
    }
  return(NULL);
}

void free_sound_list (chan_info *cp)
{
  int i;
  if (cp)
    {
      if (cp->sounds)
	{
	  for (i=0;i<cp->sound_size;i++)
	    {
	      if (cp->sounds[i]) cp->sounds[i] = free_snd_data(cp->sounds[i]);
	    }
	  FREE(cp->sounds);
	  cp->sounds = NULL;
	}
      cp->sound_ctr = -1;
      cp->sound_size = 0;
    }
}

static int gather_usage_stats_1(chan_info *cp, void *ptr)
{
  int i;
  snd_data *sf;
  env_info *ep;
  if (cp)
    {
      if (cp->stats) 
	{for (i=0;i<8;i++) cp->stats[i] = 0;}
      else cp->stats = (int *)CALLOC(8,sizeof(int));
      if (cp->amp_envs)
	{
	  for (i=0;i<cp->edit_size;i++)
	    {
	      ep = cp->amp_envs[i];
	      if (ep)
		{
		  cp->stats[AMP_ENVS_ACTIVE]++;
		  cp->stats[AMP_ENV_USAGE] += (2 * ((cp->amp_envs[i])->amp_env_size) * sizeof(int));
		}
	    }
	}
      if (cp->sounds)
	{
	  for (i=0;i<cp->sound_size;i++)
	    {
	      if ((sf = (cp->sounds[i])))
		{
		  if (sf->type == SND_DATA_BUFFER)
		    {
		      if (sf->data) 
			{
			  cp->stats[ARRAY_USAGE] += sf->len;
			  cp->stats[ARRAYS_ACTIVE]++;
			}
		    }
		  else 
		    {
		      if (sf->io)
			{
			  cp->stats[ARRAY_USAGE] += (sf->io[SND_IO_BUFSIZ]*4);
			  cp->stats[ARRAYS_ACTIVE]++;
			}
		      if (sf->temporary == DELETE_ME)
			{
			  cp->stats[TEMP_USAGE] += sf->len;
			  cp->stats[TEMPS_ACTIVE]++;
			  if (sf->open == FD_OPEN) cp->stats[TEMPS_OPEN]++;
			}
		      else
			cp->stats[FILE_USAGE] += (sf->len)/((cp->sound)->nchans);
		    }}}}}
  return(0);
}

void gather_usage_stats(chan_info *cp)
{
  gather_usage_stats_1(cp,NULL);
  update_stats_display(cp->state,FALSE);
}

void update_all_usage_stats(snd_state *ss)
{
  map_over_chans(ss,gather_usage_stats_1,NULL);
}
  
static void release_pending_sounds(chan_info *cp, int edit_ctr)
{
  /* look for buffers or open temp files that are no longer reachable after pruning the edit tree */
  int i,j,del;
  snd_data *sf;
  if (cp)
    {
      if (cp->sounds)
	{
	  del= -1;
	  for (i=0;i<cp->sound_size;i++)
	    {
	      if ((sf = (cp->sounds[i])))
		{
		  if (sf->edit_ctr >= edit_ctr)
		    {
		      cp->sounds[i] = free_snd_data(sf);
		      if (del == -1) del = i;
		    }
		}
	    }
	  if (del != -1)
	    {
	      for (j=del,i=del;i<cp->sound_size;i++)
		{
		  if (cp->sounds[i]) 
		    {
		      if (j != i) 
			{
			  cp->sounds[j] = cp->sounds[i];
			  cp->sounds[i] = NULL;
			  j++;
			}
		    }
		}
	      cp->sound_ctr = j-1;
	    }
	}
      if (show_usage_stats(cp->state)) gather_usage_stats(cp);
    }
}

static void prepare_sound_list (chan_info *cp)
{
  int i;
  cp->sound_ctr++;
  /* this is the only place the sound set is incremented */
  if (cp->sound_ctr >= cp->sound_size)
    {
      cp->sound_size += EDIT_ALLOC_SIZE;
      cp->sounds = (snd_data **)REALLOC(cp->sounds,cp->sound_size * sizeof(snd_data *));
      for (i=cp->sound_ctr;i<cp->sound_size;i++) cp->sounds[i] = NULL;
    }
  if (cp->sounds[cp->sound_ctr]) cp->sounds[cp->sound_ctr] = free_snd_data(cp->sounds[cp->sound_ctr]);
}

static int add_sound_buffer_to_edit_list(chan_info *cp, MUS_SAMPLE_TYPE *data, int len)
{
  prepare_sound_list(cp);
  cp->sounds[cp->sound_ctr] = make_snd_data_buffer(data,len,cp->edit_ctr);
  if (show_usage_stats(cp->state)) gather_usage_stats(cp);
  return(cp->sound_ctr);
}

static int add_sound_file_to_edit_list(chan_info *cp, char *name, int *io, MUS_SAMPLE_TYPE *data, file_info *hdr, int temp, int chan)
{
  prepare_sound_list(cp);
  cp->sounds[cp->sound_ctr] = make_snd_data_file(name,io,data,hdr,temp,cp->edit_ctr,chan);
  if (show_usage_stats(cp->state)) gather_usage_stats(cp);
  return(cp->sound_ctr);
}

static void ripple_out(int *list,int beg,int num, int len)
{
  int i,k;
  for (i=beg,k=beg*ED_SIZE;i<len;i++,k+=ED_SIZE) list[k+ED_OUT] += num;
}

static void prepare_edit_list(chan_info *cp, int len)
{
  int i;
  snd_info *sp;
  sp = cp->sound;
  stop_amp_env(cp);
  if ((sp) && (sp->playing)) stop_playing_sound(sp);
  cp->edit_ctr++;
  if (cp->edit_ctr >= cp->edit_size)
    {
      cp->edit_size += EDIT_ALLOC_SIZE;
      if (!cp->edits) cp->edits = (ed_list **)CALLOC(cp->edit_size,sizeof(ed_list *));
      else cp->edits = (ed_list **)REALLOC(cp->edits,cp->edit_size*sizeof(ed_list *));
      if (!cp->samples) cp->samples = (int *)CALLOC(cp->edit_size,sizeof(int));
      else cp->samples = (int *)REALLOC(cp->samples,cp->edit_size*sizeof(int));
      if (!(cp->amp_envs)) cp->amp_envs = (env_info **)CALLOC(cp->edit_size,sizeof(env_info *));
      else cp->amp_envs = (env_info **)REALLOC(cp->amp_envs,cp->edit_size*sizeof(env_info *));
      for (i=cp->edit_ctr;i<cp->edit_size;i++) {cp->edits[i] = NULL; cp->amp_envs[i] = NULL; cp->samples[i] = 0;}
    }
  if (cp->edits[cp->edit_ctr]) 
    {
      for (i=cp->edit_ctr;i<cp->edit_size;i++) 
	{
	  cp->edits[i] = free_ed_list(cp->edits[i]);
	  cp->amp_envs[i] = free_amp_env(cp,i);
	}
      release_pending_marks(cp,cp->edit_ctr);
      release_pending_mixes(cp,cp->edit_ctr);
      release_pending_sounds(cp,cp->edit_ctr);
      reflect_no_more_redo_in_menu();
    }
  reflect_undo_ok_in_menu();
  cp->samples[cp->edit_ctr] = len;
}

int current_ed_samples(chan_info *cp)
{
  if (cp) 
    return(cp->samples[cp->edit_ctr]);
  else return(0);
}

static void reflect_sample_change_in_axis(chan_info *cp)
{
  axis_info *ap;
  int samps;
  ap = cp->axis;
  if (ap)
    {
      samps = current_ed_samples(cp);
      ap->xmax = (double)samps/(double)SND_SRATE(cp->sound);
      if (ap->x1 > ap->xmax) ap->x1 = ap->xmax;
      if ((samps == 0) || (ap->no_data))
	{
	  ap->no_data = (samps == 0);
	  if (ap->xlabel) FREE(ap->xlabel);
	  if (samps == 0) ap->xlabel = copy_string(STR_no_data); else ap->xlabel = copy_string(STR_time);
	}
      set_x_bounds(ap);
    }
}

static void reflect_file_change_in_label (chan_info *cp)
{
  snd_info *sp;
  char *starred_name;
  int len;
  if (cp->edit_ctr == 0) return;
  sp = cp->sound;
  if (sp->sgx == NULL) return;
  len = strlen(shortname(sp)) + 16;
  starred_name = (char *)CALLOC(len,sizeof(char));
  strcpy(starred_name,shortname_indexed(sp));
  if (sp->read_only) 
    strcat(starred_name,"(*)");
  else strcat(starred_name,"*");
  set_sound_pane_file_label(sp,starred_name);
  make_a_big_star_outa_me(sp->shortname,1);
  FREE(starred_name);
}

static void check_for_first_edit(chan_info *cp)
{
  if ((cp->cgx) && (cp->edit_ctr == 1)) /* first edit on this file (?) */
    {
      reflect_file_change_in_menu();
      reflect_file_change_in_label(cp);
    }
}

static void fixup_edlist_endmark(ed_list *new_state, ed_list *current_state, int len)
{
  int k;
  if (new_state->fragments[(new_state->size - 1)*ED_SIZE + ED_SND] != EDIT_LIST_END_MARK)
    {
      for (k = new_state->size-1;k>0;k--)
	if (new_state->fragments[k*ED_SIZE + ED_SND] == EDIT_LIST_END_MARK) break;
      if (k>0) 
	new_state->size = k+1;
      else
	{
	  if (new_state->size == current_state->size)
	    {
	      if (new_state->size < (len+1))
		{
		  k = new_state->size;
		  new_state->size += 1;
		  new_state->fragments[ED_SIZE*new_state->size + ED_SND] = EDIT_LIST_END_MARK;
		  new_state->fragments[ED_SIZE*new_state->size + ED_BEG] = new_state->fragments[ED_SIZE*k + ED_BEG];
		  new_state->fragments[ED_SIZE*new_state->size + ED_OUT] = new_state->fragments[ED_SIZE*k + ED_OUT];
		  new_state->fragments[ED_SIZE*new_state->size + ED_END] = new_state->fragments[ED_SIZE*k + ED_END];
		}
	    }
	}
    }
}

static ed_list *insert_samples_1 (int samp, int num, MUS_SAMPLE_TYPE* vals, ed_list *current_state, chan_info *cp, int **cb_back, char *origin)
{
  int len,k,old_beg,old_end,old_snd,old_out;
  ed_list *new_state;
  int *cb,*cbback,*ed;
  if (num <= 0) return(NULL);
  len = current_state->size;
  ed = current_state->fragments;
  k=find_split_loc(cp,samp,current_state);
  cb = (int *)(ed + k*ED_SIZE);
  if ((k == 0) && (cb[ED_END] == -1))
    {
      /* no data: just set insertion */
      new_state = make_ed_list(len);
      copy_ed_blocks(new_state->fragments,ed,0,0,len);
      cb = (int *)(new_state->fragments);
      cb[ED_END] = num-1;
    }
  else
    {
      if ((samp == cb[ED_OUT]) || (samp == (cb[ED_OUT] - 1)))
	{
	  new_state = make_ed_list(len+1);
	  copy_ed_blocks(new_state->fragments,ed,0,0,k);
	  copy_ed_blocks(new_state->fragments,ed,k+1,k,len-k);
	  len++;
	}
      else
	{
	  cbback = (int *)(ed + (k-1)*ED_SIZE);
	  old_beg = cbback[ED_BEG];
	  old_end = cbback[ED_END];
	  old_snd = cbback[ED_SND];
	  old_out = cbback[ED_OUT];
	  new_state = make_ed_list(len+2);
	  copy_ed_blocks(new_state->fragments,ed,0,0,k);
	  copy_ed_blocks(new_state->fragments,ed,k+2,k,len-k);
	  cb = (int *)(new_state->fragments + (k+1)*ED_SIZE);  /* old after split */
	  cbback = (int *)(new_state->fragments + (k-1)*ED_SIZE); /* old before split */
	  cb[ED_SND] = old_snd;
	  cb[ED_OUT] = samp;
	  cb[ED_BEG] = old_beg+samp-old_out;
	  cb[ED_END] = old_end;
	  cbback[ED_END] = old_beg+samp-old_out-1;
	  len += 2;
	}
      cb = (int *)(new_state->fragments + k*ED_SIZE); /* new */
      cb[ED_BEG] = 0;
      cb[ED_END] = num-1;
      cb[ED_OUT] = samp;
    }
  if (vals) 
    {
      cb[ED_SND] = add_sound_buffer_to_edit_list(cp,vals,num); 
      new_state->sfnum = PACK_EDIT(INSERTION_EDIT,cb[ED_SND]);
    }
  else (*cb_back) = cb;
  /* vals is null if data is in a file -- handled later */
  new_state->beg = samp;
  new_state->len = num;
  if (origin) new_state->origin = copy_string(origin);
  ripple_out(new_state->fragments,k+1,num,len);
  ripple_marks(cp,samp,num);
  ripple_mixes(cp,samp,num);
  ripple_selection(cp,samp,num);
  reflect_sample_change_in_axis(cp);
  check_for_first_edit(cp);
  fixup_edlist_endmark(new_state,current_state,len);
  return(new_state);
}

static void extend_insertion(int beg, int num, MUS_SAMPLE_TYPE *vals, chan_info *cp, char *origin)
{
  int k,len;
  int *cb;
  if (num <= 0) return;
  len = current_ed_samples(cp);
  k=cp->edit_ctr;
  prepare_edit_list(cp,len+num);
  cp->edits[cp->edit_ctr] = insert_samples_1(beg,num,vals,cp->edits[k],cp,&cb,origin);
  reflect_edit_history_change(cp);
}

void file_insert_samples(int beg, int num, char *inserted_file, chan_info *cp, int chan, int auto_delete, char *origin)
{
  int k,len;
  int *cb;
  MUS_SAMPLE_TYPE *zeros;
  int fd;
  int *datai;
  ed_list *ed;
  file_info *hdr;
  if (num <= 0) 
    {
      if ((inserted_file) && (auto_delete == DELETE_ME)) remove(inserted_file);
      if ((inserted_file) && (auto_delete == MULTICHANNEL_DELETION)) forget_temp(inserted_file,chan);
      return;
    }
  if (dont_edit(cp)) return;
  len = current_ed_samples(cp);
  if (beg >= len)
    {
      zeros = (MUS_SAMPLE_TYPE *)CALLOC(beg-len+1,sizeof(MUS_SAMPLE_TYPE));
      extend_insertion(len,beg-len+1,zeros,cp,"(insert-extend)");
      check_for_first_edit(cp); /* needed to activate revert menu option */
      FREE(zeros);
      len = current_ed_samples(cp);
    }
  k=cp->edit_ctr;
  prepare_edit_list(cp,len+num);
  cp->edits[cp->edit_ctr] = insert_samples_1(beg,num,NULL,cp->edits[k],cp,&cb,origin);
  reflect_edit_history_change(cp);
  hdr = make_file_info(inserted_file,cp->state);
  if (hdr)
    {
      fd = snd_open_read(cp->state,inserted_file);
      mus_file_set_descriptors(fd,inserted_file,
			       hdr->format,mus_data_format_to_bytes_per_sample(hdr->format),hdr->data_location,
			       hdr->chans,hdr->type);
      during_open(fd,inserted_file,SND_INSERT_FILE);
      datai = make_file_state(fd,hdr,SND_IO_IN_FILE,chan,FILE_BUFFER_SIZE);
      cb[ED_SND] = add_sound_file_to_edit_list(cp,inserted_file,datai,
					       MUS_SAMPLE_ARRAY(datai[SND_IO_DATS + SND_AREF_HEADER_SIZE+chan]),
					       hdr,auto_delete,chan);
      ed = cp->edits[cp->edit_ctr];
      ed->sfnum = PACK_EDIT(INSERTION_EDIT,cb[ED_SND]);
      lock_affected_mixes(cp,beg,beg+num);
      if (cp->mix_md) reflect_mix_edit(cp,origin);
    }
}

void insert_samples(int beg, int num, MUS_SAMPLE_TYPE *vals, chan_info *cp, char *origin)
{
  int k,len;
  int *cb;
  MUS_SAMPLE_TYPE *zeros;
  if (num <= 0) return;
  if (dont_edit(cp)) return;
  len = current_ed_samples(cp);
  if (beg >= len)
    {
      zeros = (MUS_SAMPLE_TYPE *)CALLOC(beg-len+1,sizeof(MUS_SAMPLE_TYPE));
      extend_insertion(len,beg-len+1,zeros,cp,"(insert-extend)");
      check_for_first_edit(cp); /* needed to activate revert menu option */
      FREE(zeros);
      len = current_ed_samples(cp);
    }
  k=cp->edit_ctr;
  prepare_edit_list(cp,len+num);
  cp->edits[cp->edit_ctr] = insert_samples_1(beg,num,vals,cp->edits[k],cp,&cb,origin);
  reflect_edit_history_change(cp);
  lock_affected_mixes(cp,beg,beg+num);
  if (cp->mix_md) reflect_mix_edit(cp,origin);
}

static ed_list *delete_samples_1(int beg, int num, ed_list *current_state, chan_info *cp, char *origin)
{
  int len,k,need_to_delete,curbeg,old_out,cbi,start_del,len_fixup;
  int *cb,*temp_cb;
  ed_list *new_state;
  if (num <= 0) return(NULL);
  len=current_state->size;
  len_fixup = -1;
  k=find_split_loc(cp,beg,current_state);
  need_to_delete = num;
  start_del = k;
  curbeg = beg;
  cb = (int *)(current_state->fragments + k*ED_SIZE);
  if (cb[ED_OUT]>beg) start_del--;
  new_state = make_ed_list(len+1);
  copy_ed_blocks(new_state->fragments,current_state->fragments,0,0,start_del);
  cbi=start_del;
  temp_cb = (int *)(current_state->fragments + start_del*ED_SIZE);
  old_out = temp_cb[ED_OUT];
  if (beg>old_out)
    {
      cb = (int *)(new_state->fragments + start_del*ED_SIZE);
      cb[ED_OUT] = old_out;
      cb[ED_SND] = temp_cb[ED_SND];
      cb[ED_BEG] = temp_cb[ED_BEG];
      cb[ED_END] = temp_cb[ED_BEG]+beg-old_out-1;
      start_del++;
      len_fixup++;
    }
  while (need_to_delete > 0)
    {
      old_out = (current_state->fragments[(cbi+1)*ED_SIZE+ED_OUT]);
      need_to_delete -= (old_out-curbeg);
      if (need_to_delete > 0)
	{
	  cbi++;
	  curbeg = old_out;
	}
    }
  if (need_to_delete < 0)
    {
      temp_cb = (int *)(current_state->fragments+cbi*ED_SIZE);
      cb = (int *)(new_state->fragments+start_del*ED_SIZE);
      cb[ED_OUT] = beg;
      cb[ED_SND] = temp_cb[ED_SND];
      cb[ED_BEG] = temp_cb[ED_END]+1+need_to_delete;
      cb[ED_END] = temp_cb[ED_END];
      start_del++;
      len_fixup++;
    }
  cbi++;
  copy_ed_blocks(new_state->fragments,current_state->fragments,start_del,cbi,len-cbi); /* ??? */
  new_state->beg = beg;
  new_state->len = num;
  if (origin) new_state->origin = copy_string(origin);
  new_state->sfnum = PACK_EDIT(DELETION_EDIT,0);
  ripple_out(new_state->fragments,start_del,-num,len+len_fixup);
  ripple_marks(cp,beg,-num);
  ripple_mixes(cp,beg,-num);
  ripple_selection(cp,beg,-num);
  reflect_sample_change_in_axis(cp);
  check_for_first_edit(cp);
  new_state->size = len+len_fixup; /* don't propogate useless trailing blocks */
  
  if (new_state->fragments[(new_state->size - 1)*ED_SIZE + ED_SND] != EDIT_LIST_END_MARK)
    {
      for (k = new_state->size-1;k>0;k--)
	if (new_state->fragments[k*ED_SIZE + ED_SND] == EDIT_LIST_END_MARK) break;
      if (k>0) 
	new_state->size = k+1; 
    }
  if (new_state->size == 1)
    {
      /* if cut all, no data remains, so (I think) it is legit to have a one-length fragment = end_mark */
      /* but just to be safe, I'll copy that mark */
      new_state->size = 2;
      if (new_state->fragments[ED_BEG] < 0) new_state->fragments[ED_BEG] = 0;
      if (new_state->fragments[ED_OUT] < 0) new_state->fragments[ED_OUT] = 0;
      new_state->fragments[ED_SIZE + ED_SND] = EDIT_LIST_END_MARK;
      new_state->fragments[ED_SIZE + ED_BEG] = new_state->fragments[ED_BEG];
      new_state->fragments[ED_SIZE + ED_OUT] = new_state->fragments[ED_OUT];
      new_state->fragments[ED_SIZE + ED_END] = new_state->fragments[ED_END];
    }

  return(new_state);
}    

void delete_samples(int beg, int num, chan_info *cp, char *origin)
{
  int k,len;
  char *errstr;
  if (num <= 0) return;
  if (dont_edit(cp)) return;
  len = current_ed_samples(cp);
  if ((beg < len) && (beg >= 0))
    {
      if ((beg+num) > len) num = len-beg;
      k=cp->edit_ctr;
      prepare_edit_list(cp,len-num);
      cp->edits[cp->edit_ctr] = delete_samples_1(beg,num,cp->edits[k],cp,origin);
      reflect_edit_history_change(cp);
      lock_affected_mixes(cp,beg,beg+num);
      if (cp->mix_md) reflect_mix_edit(cp,origin);
    }
  else
    {
      errstr = (char *)CALLOC(64,sizeof(char));
      if (num == 1)
	sprintf(errstr,"can't delete sample %d (current len=%d)",beg,len);
      else sprintf(errstr,"can't delete samples %d to %d (current len=%d)",beg,beg+num-1,len);
      report_in_minibuffer(cp->sound,errstr);
      FREE(errstr);
    }
}

static ed_list *change_samples_1(int beg, int num, MUS_SAMPLE_TYPE *vals, ed_list *current_state, chan_info *cp, int **cb_back, int lengthen, char *origin)
{
  int len,k,start_del,cbi,curbeg,len_fixup,need_to_delete,old_out;
  ed_list *new_state;
  int *cb,*temp_cb;
  if (num <= 0) return(NULL);
  len = current_state->size;
  len_fixup = -1;
  k=find_split_loc(cp,beg,current_state);
  need_to_delete = num - lengthen;
  start_del = k;
  curbeg = beg;
  cbi=0;
  cb = (int *)(current_state->fragments + k*ED_SIZE);
  if (cb[ED_OUT]>beg) start_del--;
  new_state = make_ed_list(len+2);
  copy_ed_blocks(new_state->fragments,current_state->fragments,0,0,start_del);
  cbi=start_del;
  temp_cb = (int *)(current_state->fragments + start_del*ED_SIZE);  
  old_out = temp_cb[ED_OUT];
  if (beg>old_out)
    {
      cb = (int *)(new_state->fragments + start_del*ED_SIZE);
      cb[ED_OUT] = old_out;
      cb[ED_SND] = temp_cb[ED_SND];
      cb[ED_BEG] = temp_cb[ED_BEG];
      cb[ED_END] = temp_cb[ED_BEG]+beg-old_out-1;
      start_del++;
      len_fixup++;
    }
  while (need_to_delete > 0)
    {
      old_out = (current_state->fragments[(cbi+1)*ED_SIZE+ED_OUT]);
      need_to_delete -= (old_out-curbeg);
      if (need_to_delete > 0)
	{
	  cbi++;
	  curbeg = old_out;
	}
    }
  cb = (int *)(new_state->fragments+start_del*ED_SIZE);
  if (vals) 
    {
      cb[ED_SND] = add_sound_buffer_to_edit_list(cp,vals,num); 
      new_state->sfnum = PACK_EDIT(CHANGE_EDIT,cb[ED_SND]);
    }
  else (*cb_back) = cb;
  cb[ED_OUT] = beg;
  cb[ED_BEG] = 0;
  cb[ED_END] = num-1;
  start_del++;
  len_fixup++;
  if (need_to_delete < 0)
    {
      temp_cb = (int *)(current_state->fragments+cbi*ED_SIZE);
      cb = (int *)(new_state->fragments+start_del*ED_SIZE);
      cb[ED_OUT] = beg+num;
      cb[ED_SND] = temp_cb[ED_SND];
      cb[ED_BEG] = temp_cb[ED_END]+1+need_to_delete;
      cb[ED_END] = temp_cb[ED_END];
      start_del++;
      len_fixup++;
    }
  cbi++;
  copy_ed_blocks(new_state->fragments,current_state->fragments,start_del,cbi,len-cbi);
  new_state->beg = beg;
  new_state->len = num;
  if (origin) new_state->origin = copy_string(origin);
  if (lengthen)
    {
      ripple_out(new_state->fragments,k+1,lengthen,len+len_fixup);
      reflect_sample_change_in_axis(cp);
    }
  new_state->size = len+len_fixup; /* don't propogate useless trailing blocks */
  ripple_marks(cp,0,0);
  check_for_first_edit(cp);
  fixup_edlist_endmark(new_state,current_state,len);
  return(new_state);
}    

void file_change_samples(int beg, int num, char *tempfile, chan_info *cp, int chan, int auto_delete, int lock, char *origin)
{
  int k,prev_len,new_len;
  int *cb;
  MUS_SAMPLE_TYPE *zeros;
  int fd;
  int *datai;
  ed_list *ed;
  file_info *hdr;
  if (num <= 0) 
    {
      if ((tempfile) && (auto_delete == DELETE_ME)) remove(tempfile);
      if ((tempfile) && (auto_delete == MULTICHANNEL_DELETION)) forget_temp(tempfile,chan);
      return;
    }
  if (dont_edit(cp)) return;
  prev_len = current_ed_samples(cp);

  if (beg >= prev_len)
    {
      zeros = (MUS_SAMPLE_TYPE *)CALLOC(beg-prev_len+1,sizeof(MUS_SAMPLE_TYPE));
      extend_insertion(prev_len,beg-prev_len+1,zeros,cp,"(change-extend)");
      check_for_first_edit(cp); /* needed to activate revert menu option */
      FREE(zeros);
      prev_len = current_ed_samples(cp);
    }

  new_len = beg+num;
  if (new_len < prev_len) new_len = prev_len;
  k=cp->edit_ctr;
  prepare_edit_list(cp,new_len);
  cp->edits[cp->edit_ctr] = change_samples_1(beg,num,NULL,cp->edits[k],cp,&cb,new_len - prev_len,origin);
  reflect_edit_history_change(cp);
  if (lock == LOCK_MIXES) lock_affected_mixes(cp,beg,beg+num);
  hdr = make_file_info(tempfile,cp->state);
  if (hdr)
    {
      fd = snd_open_read(cp->state,tempfile);
      mus_file_set_descriptors(fd,tempfile,
			       hdr->format,mus_data_format_to_bytes_per_sample(hdr->format),hdr->data_location,
			       hdr->chans,hdr->type);
      during_open(fd,tempfile,SND_CHANGE_FILE);
      datai = make_file_state(fd,hdr,SND_IO_IN_FILE,chan,FILE_BUFFER_SIZE);
      cb[ED_SND] = add_sound_file_to_edit_list(cp,tempfile,datai,
					       MUS_SAMPLE_ARRAY(datai[SND_IO_DATS + SND_AREF_HEADER_SIZE+chan]),
					       hdr,auto_delete,chan);
      ed = cp->edits[cp->edit_ctr];
      ed->sfnum = PACK_EDIT(CHANGE_EDIT,cb[ED_SND]);
      if (cp->mix_md) reflect_mix_edit(cp,origin);
    }
}

void file_override_samples(int num, char *tempfile, chan_info *cp, int chan, int auto_delete, int lock, char *origin)
{
  int fd;
  ed_list *e;
  int *datai;
  file_info *hdr;
  snd_state *ss;
  if (num == 0) 
    {
      if ((tempfile) && (auto_delete == DELETE_ME)) remove(tempfile);
      if ((tempfile) && (auto_delete == MULTICHANNEL_DELETION)) forget_temp(tempfile,chan);
      return;
    }
  if (dont_edit(cp)) return;
  ss = cp->state;
  hdr = make_file_info(tempfile,ss);
  if (hdr) 
    {
      if (num == -1) num = (hdr->samples/hdr->chans);
      prepare_edit_list(cp,num);
      fd = snd_open_read(ss,tempfile);
      mus_file_set_descriptors(fd,tempfile,
			       hdr->format,mus_data_format_to_bytes_per_sample(hdr->format),hdr->data_location,
			       hdr->chans,hdr->type);
      during_open(fd,tempfile,SND_OVERRIDE_FILE);
      datai = make_file_state(fd,hdr,SND_IO_IN_FILE,chan,FILE_BUFFER_SIZE);
      e = initial_ed_list(0,num-1);
      if (origin) e->origin = copy_string(origin);
      cp->edits[cp->edit_ctr] = e;
      if (lock == LOCK_MIXES) lock_affected_mixes(cp,0,num);
      e->fragments[0 + ED_SND] = add_sound_file_to_edit_list(cp,tempfile,datai,
							     MUS_SAMPLE_ARRAY(datai[SND_IO_DATS + SND_AREF_HEADER_SIZE+chan]),
							     hdr,auto_delete,chan);
      e->sfnum = PACK_EDIT(CHANGE_EDIT,e->fragments[0 + ED_SND]);
      reflect_edit_history_change(cp);
      reflect_sample_change_in_axis(cp);
      ripple_marks(cp,0,0);
      check_for_first_edit(cp);
      update_graph(cp,NULL);
      if (cp->mix_md) reflect_mix_edit(cp,origin);
    }
}

void change_samples(int beg, int num, MUS_SAMPLE_TYPE *vals, chan_info *cp, int lock, char *origin)
{
  int k,prev_len,new_len;
  MUS_SAMPLE_TYPE *zeros;
  if (num <= 0) return;
  if (dont_edit(cp)) return;
  prev_len = current_ed_samples(cp);
  if (beg >= prev_len)
    {
      zeros = (MUS_SAMPLE_TYPE *)CALLOC(beg-prev_len+1,sizeof(MUS_SAMPLE_TYPE));
      extend_insertion(prev_len,beg-prev_len+1,zeros,cp,"(change-extend)");
      check_for_first_edit(cp); /* needed to activate revert menu option */
      FREE(zeros);
      prev_len = current_ed_samples(cp);
    }
  new_len = beg+num;
  if (new_len < prev_len) new_len = prev_len;
  k=cp->edit_ctr;
  prepare_edit_list(cp,new_len);
  cp->edits[cp->edit_ctr] = change_samples_1(beg,num,vals,cp->edits[k],cp,NULL,new_len - prev_len,origin);
  reflect_edit_history_change(cp);
  if (lock == LOCK_MIXES) lock_affected_mixes(cp,beg,beg+num);
  if (cp->mix_md) reflect_mix_edit(cp,origin);
}

static MUS_SAMPLE_TYPE snd_file_read(snd_data *ur_sd, int index, chan_info *cp)
{
  int copied;
  MUS_SAMPLE_TYPE val;
  snd_data *sd = NULL;
  copied = 0;
  /* first try to grab the sample without moving any buffers */
  if ((index >= ur_sd->io[SND_IO_BEG]) && (index <= ur_sd->io[SND_IO_END])) 
    return(ur_sd->data[index - ur_sd->io[SND_IO_BEG]]);
  /* not in current buffer, so create a new reader and go looking for it */
  if (ur_sd->inuse) 
    {
      sd = copy_snd_data(ur_sd,cp,4); 
      copied = 1;
    } 
  else sd = ur_sd;
  sd->inuse = TRUE;
  if ((index < sd->io[SND_IO_BEG]) || (index > sd->io[SND_IO_END])) 
    snd_file_reset(sd,index);
  val = sd->data[index - sd->io[SND_IO_BEG]];
  if (copied) 
    {
      sd->inuse = FALSE; 
      free_snd_data(sd);
    }
  return(val); 
}

static MUS_SAMPLE_TYPE sample_1 (int samp, chan_info *cp)
{ /* slow access */
  ed_list *current_state;
  snd_data *sd;
  int len,i,cb,true_cb,index;
  int *data;
  if (samp < 0) return(MUS_SAMPLE_0);
  if (samp > cp->samples[cp->edit_ctr]) return(MUS_SAMPLE_0);
  current_state = (ed_list *)(cp->edits[cp->edit_ctr]);
  data = current_state->fragments;
  len = current_state->size;
  for (i=0,cb=0;i<len;i++,cb+=ED_SIZE)
    {
      if (samp < data[cb+ED_OUT])
	{
	  true_cb = cb-ED_SIZE;
	  if (data[true_cb+ED_SND] == EDIT_LIST_END_MARK) return(0);
	  index = data[true_cb+ED_BEG]+samp-data[true_cb+ED_OUT];
	  sd = cp->sounds[data[true_cb+ED_SND]];
	  if (sd->type == SND_DATA_BUFFER)
	    return(sd->data[index]);
	  else return(snd_file_read(sd,index,cp));
	}
    }
  return(0);
}

Float sample(int samp, chan_info *cp) {return(MUS_SAMPLE_TO_FLOAT(sample_1(samp,cp)));}

/* now for optimized sample access -- since everything goes through these lists, we want the access to be fast */

snd_fd *free_snd_fd(snd_fd *sf)
{
  snd_data *sd;
  if (sf) 
    {
      sd = sf->current_sound;
      if (sd)
	{
	  sd->inuse = FALSE;
	  if ((sd->copy) && (sd->owner == (void *)sf)) free_snd_data(sd);
	}
      FREE(sf);
    }
  return(NULL);
}

static void massage_snd_file(int ind0, int ind1, int indx, snd_fd *sf, snd_data *cur_snd)
{
  /* need to track in-core buffer and file-relative index */
  if ((indx < cur_snd->io[SND_IO_BEG]) || (indx > cur_snd->io[SND_IO_END])) 
    snd_file_reset(cur_snd,indx);
  sf->data = (MUS_SAMPLE_TYPE *)(cur_snd->data + indx - cur_snd->io[SND_IO_BEG]);
  /* only indx is guaranteed to be within the current in-core buffer */

  if (ind0 >= cur_snd->io[SND_IO_BEG])
    sf->first = (MUS_SAMPLE_TYPE *)(cur_snd->data + ind0 - cur_snd->io[SND_IO_BEG]);
  else sf->first = cur_snd->data;

  if (ind1 <= cur_snd->io[SND_IO_END]) 
    {
      sf->last = (MUS_SAMPLE_TYPE *)(cur_snd->data + ind1 - cur_snd->io[SND_IO_BEG]);
      sf->eof = 1;
    }
  else 
    {
      sf->last = (MUS_SAMPLE_TYPE *)(cur_snd->data + cur_snd->io[SND_IO_BUFSIZ] -1);
      sf->eof = 0;
    }
  sf->beg = cur_snd->io[SND_IO_BEG];
  sf->end = cur_snd->io[SND_IO_END];
}

static void massage_snd_file_back(int ind0, int ind1, int indx, snd_fd *sf, snd_data *cur_snd)
{
  if ((indx > cur_snd->io[SND_IO_END]) || (indx < cur_snd->io[SND_IO_BEG])) 
    snd_file_reset(cur_snd,indx - cur_snd->io[SND_IO_BUFSIZ] + 1);
  sf->data = (MUS_SAMPLE_TYPE *)(cur_snd->data + indx - cur_snd->io[SND_IO_BEG]);

  if (ind1 <= cur_snd->io[SND_IO_END])
    sf->last = (MUS_SAMPLE_TYPE *)(cur_snd->data + ind1 - cur_snd->io[SND_IO_BEG]);
  else sf->last = (MUS_SAMPLE_TYPE *)(cur_snd->data + cur_snd->io[SND_IO_BUFSIZ] -1);

  if (ind0 >= cur_snd->io[SND_IO_BEG]) 
    {
      sf->first = (MUS_SAMPLE_TYPE *)(cur_snd->data + ind0 - cur_snd->io[SND_IO_BEG]);
      sf->eof = 1;
    }
  else 
    {
      sf->first = cur_snd->data;
      sf->eof = 0;
    }
  sf->beg = cur_snd->io[SND_IO_BEG];
  sf->end = cur_snd->io[SND_IO_END];
}

#if LONG_INT_P
  int current_location(snd_fd *sf) {return(sf->cb[ED_OUT] - sf->cb[ED_BEG] + sf->beg + (int)(((long)(sf->data) - (long)(sf->first))>>2));}
#else
  int current_location(snd_fd *sf) {return(sf->cb[ED_OUT] - sf->cb[ED_BEG] + sf->beg + (((int)(sf->data) - (int)(sf->first))>>2));}
#endif


snd_fd *init_sample_read_any (int samp, chan_info *cp, int direction, int edit_position)
{
  snd_fd *sf;
  snd_info *sp;
  ed_list *ed;
  int len,i,k,ind0,ind1,indx,curlen;
  int *cb;
  snd_data *first_snd = NULL;
  sf = (snd_fd *)CALLOC(1,sizeof(snd_fd));
  sf->initial_samp = samp;
  sf->direction = 0;
  sf->cp = cp;
  sp = cp->sound;
  if (sp->need_update) 
    {
      if ((snd_probe_file(sp->fullname)) == FILE_DOES_NOT_EXIST)
	snd_error("%s no longer exists!",sp->shortname);
      else snd_warning("%s has changed since we last read it!",sp->shortname);
    }
  if ((edit_position < 0) || (edit_position > cp->edit_size)) return(NULL);
  ed = (ed_list *)(cp->edits[edit_position]);
  if (!ed) return(NULL);
  curlen = cp->samples[edit_position];
  sf->current_state = ed;
  if ((curlen <= 0) ||    /* no samples, not ed->len (delete->len = #deleted samps) */
      (samp < 0) ||       /* this should never happen */
      ((samp >= curlen) && (direction == READ_FORWARD)))
    {
      sf->eof = 1;
      sf->data = (MUS_SAMPLE_TYPE *)1;
      sf->last = (MUS_SAMPLE_TYPE *)0; /* can I get away with this?? -- we're trying to do something with an empty file here */
      sf->first = (MUS_SAMPLE_TYPE *)2;
      /* data>last and data<first are triggers to ignore data and try to move in the fragment list */
      sf->current_sound = NULL;
      sf->cbi = 0;
      return(sf);
    }
  if (samp >= curlen) samp = curlen-1;
  len = ed->size;
  for (i=0,k=0;i<len;i++,k+=ED_SIZE)
    {
      cb = (int *)(ed->fragments+k);
      if ((cb[ED_OUT] > samp) || (cb[ED_SND] == EDIT_LIST_END_MARK))
	{
	  sf->cb = (int *)(ed->fragments+k-ED_SIZE);
	  sf->cbi = i-1;
	  ind0 = sf->cb[ED_BEG];
	  indx = sf->cb[ED_BEG]+samp-sf->cb[ED_OUT];
	  ind1 = sf->cb[ED_END];
	  sf->sounds = (snd_data **)(cp->sounds);
	  first_snd = sf->sounds[sf->cb[ED_SND]];
	  if (first_snd->type == SND_DATA_FILE)
	    {
	      /* since arbitrarily many work procs can be running in parallel, reading the same 
	       * data (edit tree sound file entries), we can't share the clm-style IO buffers since these contain
	       * a local notion of current position which is not accessed on every sample by the
	       * sample readers (they trust their snd_fd indices); we wouldn't want to be
	       * constantly jumping around and re-reading data buffers (in the worst case
	       * many times per sample) anyway, so we copy the IO buffer, allocate a relatively
	       * small data buffer, and then free all the copied snd_data stuff as soon as
	       * the current reader is done.
	       */
	      if (first_snd->inuse) 
		{
		  first_snd = copy_snd_data(first_snd,cp,MIX_FILE_BUFFER_SIZE);
		  first_snd->owner = (void *)sf;
		}
	      first_snd->inuse = TRUE;
	      sf->current_sound = first_snd;
	      if (direction == READ_FORWARD)
		massage_snd_file(ind0,ind1,indx,sf,first_snd);
	      else massage_snd_file_back(ind0,ind1,indx,sf,first_snd);
	    }
	  else 
	    {
	      sf->current_sound = NULL;
	      sf->data = (MUS_SAMPLE_TYPE *)(first_snd->data+indx);
	      sf->first = (MUS_SAMPLE_TYPE *)(first_snd->data+ind0);
	      sf->last = (MUS_SAMPLE_TYPE *)(first_snd->data+ind1);
	      sf->eof = 1;
	    }
	  sf->current_value = (*sf->data);
	  return(sf);
	}
    }
#if DEBUGGING
  display_edits(cp);
  fprintf(stderr,"%s[%d] %s: big trouble",__FILE__,__LINE__,__FUNCTION__); abort();
#endif
  return(NULL);
}

snd_fd *init_sample_read (int samp, chan_info *cp, int direction)
{
  return(init_sample_read_any(samp,cp,direction,cp->edit_ctr));
}

MUS_SAMPLE_TYPE previous_sound (snd_fd *sf) 
{
  int ind0,ind1,indx;
  snd_data *prev_snd;
  if (sf->eof)
    {
      if (sf->current_sound) 
	{
	  prev_snd = sf->current_sound; 
	  prev_snd->inuse = FALSE; 
	  sf->current_sound = NULL;
	  if ((prev_snd->owner == (void *)sf) && (prev_snd->copy)) free_snd_data(prev_snd);
	}
      if (sf->cbi == 0) return(MUS_SAMPLE_0); /* can't back up any further */
      sf->cbi--;
      /* now start in the final portion of this block (if a file) */
      sf->cb = (int *)((sf->current_state)->fragments+sf->cbi*ED_SIZE);
      ind0 = sf->cb[ED_BEG];
      ind1 = sf->cb[ED_END];
      prev_snd = sf->sounds[sf->cb[ED_SND]];

      if (prev_snd->type == SND_DATA_FILE)
	{
	  if (prev_snd->inuse) 
	    {
	      prev_snd = copy_snd_data(prev_snd,sf->cp,MIX_FILE_BUFFER_SIZE);
	      prev_snd->owner = (void *)sf;
	    }
	  prev_snd->inuse = TRUE;
	  sf->current_sound = prev_snd;
	  massage_snd_file_back(ind0,ind1,ind1,sf,prev_snd);
	}
      else 
	{
	  sf->data = (MUS_SAMPLE_TYPE *)(prev_snd->data+ind1);
	  sf->first = (MUS_SAMPLE_TYPE *)(prev_snd->data+ind0);
	  sf->last = sf->data;
	  sf->eof = 1;
	}
    }
  else
    {
      /* back up in current file */
      ind0 = sf->cb[ED_BEG];
      ind1 = sf->cb[ED_END];
      indx = sf->beg-1;
      massage_snd_file_back(ind0,ind1,indx,sf,sf->current_sound);
    }
  return(sf->current_value = *sf->data--);
}

MUS_SAMPLE_TYPE next_sound (snd_fd *sf)
{
  int ind0,ind1,indx;
  snd_data *nxt_snd;
  if (sf->eof) /* a convenience -- we could figure this out from various pointers */
    {
      if (sf->current_sound) 
	{
	  nxt_snd = sf->current_sound; 
	  nxt_snd->inuse = FALSE; 
	  sf->current_sound = NULL;
	  if ((nxt_snd->owner == (void *)sf) && (nxt_snd->copy)) free_snd_data(nxt_snd);
	}
      if (sf->last == (MUS_SAMPLE_TYPE *)0) return(MUS_SAMPLE_0);
      sf->cbi++;
      if (sf->cbi >= (sf->current_state)->size)
	{
          sf->data = (MUS_SAMPLE_TYPE *)1;
	  sf->last = (MUS_SAMPLE_TYPE *)0;
	  return(MUS_SAMPLE_0);
	}
      sf->cb = (int *)((sf->current_state)->fragments+sf->cbi*ED_SIZE);
      if ((!(sf->cb)) || (sf->cb[ED_SND] == EDIT_LIST_END_MARK))
	{
          sf->data = (MUS_SAMPLE_TYPE *)1;
	  sf->last = (MUS_SAMPLE_TYPE *)0;
	  return(MUS_SAMPLE_0);
	}
      ind0 = sf->cb[ED_BEG];
      ind1 = sf->cb[ED_END];
      nxt_snd = sf->sounds[sf->cb[ED_SND]];
      if (nxt_snd->type == SND_DATA_FILE)
	{
	  if (nxt_snd->inuse) 
	    {
	      nxt_snd = copy_snd_data(nxt_snd,sf->cp,MIX_FILE_BUFFER_SIZE);
	      nxt_snd->owner = (void *)sf;
	    }
	  nxt_snd->inuse = TRUE;
	  sf->current_sound = nxt_snd;
	  massage_snd_file(ind0,ind1,ind0,sf,nxt_snd);
	}
      else 
	{
	  sf->data = (MUS_SAMPLE_TYPE *)(nxt_snd->data+ind0);
	  sf->first = sf->data;
	  sf->last = (MUS_SAMPLE_TYPE *)(nxt_snd->data+ind1);
	  sf->eof = 1;
	}
    }
  else
    { 
      ind0 = sf->cb[ED_BEG];
      ind1 = sf->cb[ED_END];
      indx = sf->end+1;
      massage_snd_file(ind0,ind1,indx,sf,sf->current_sound);
    }
  return(sf->current_value = *sf->data++);
}

MUS_SAMPLE_TYPE next_sample_1 (snd_fd *sf)
{
  if (sf->data > sf->last) return(next_sound(sf));
  return(sf->current_value = *sf->data++);
}

MUS_SAMPLE_TYPE previous_sample_1(snd_fd *sf)
{
  if (sf->data < sf->first) return(previous_sound(sf));
  return(sf->current_value = *sf->data--);
}

Float next_sample (snd_fd *sf) {return(MUS_SAMPLE_TO_FLOAT(next_sample_1(sf)));}
static Float previous_sample (snd_fd *sf) {return(MUS_SAMPLE_TO_FLOAT(previous_sample_1(sf)));}

int read_sample_eof (snd_fd *sf)
{
  if (sf->cb)
    return((sf->cb[ED_SND] == EDIT_LIST_END_MARK) || ((sf->cbi == 0) && (sf->data < sf->first)));
  else return(sf->data >= sf->last);
}


/* -------------------------------- EDITS -------------------------------- */


static int snd_io_error;
void set_snd_IO_error(int err) {snd_io_error = err;}

static char *snd_error_names[] = {
  "no error","can't write header","can't open file","can't write data",
  "unsupported header type","can't find file","unsupported data format","output interrupted"};

char *snd_error_name(int i) {return(snd_error_names[i]);}

int open_temp_file(char *ofile, int chans, file_info *hdr, snd_state *ss)
{
  int ofd,len,err;
  len = snd_strlen(hdr->comment);
  if (!(mus_header_writable(hdr->type,hdr->format)))
    {
      hdr->type = default_output_type(ss);
      if (mus_header_writable(hdr->type,default_output_format(ss)))
	hdr->format = default_output_format(ss);
      else
	{
	  hdr->type = DEFAULT_OUTPUT_TYPE;
	  hdr->format = DEFAULT_OUTPUT_FORMAT;
	}
    }
  err = snd_write_header(ss,ofile,hdr->type,hdr->srate,chans,0,0,hdr->format,hdr->comment,len,hdr->loops);
  if (err == -1) return(-1);
  if ((ofd = snd_reopen_write(ss,ofile)) == -1) return(-1);
  hdr->data_location = mus_header_data_location(); /* header might have changed size (aiff extras) */
  mus_file_set_descriptors(ofd,ofile,
			   hdr->format,mus_data_format_to_bytes_per_sample(hdr->format),hdr->data_location,
			   chans,hdr->type);
  mus_file_set_data_clipped(ofd,data_clipped(ss));
  mus_file_seek(ofd,hdr->data_location,SEEK_SET);
  return(ofd);
}

int close_temp_file(int ofd, file_info *hdr, long bytes, snd_info *sp)
{
  int kleft,kused;
  char *prtbuf;
  mus_header_update_with_fd(ofd,hdr->type,bytes);
  kleft = disk_kspace(ofd);
  if (kleft < 0)
    snd_error("close temp file: %s",strerror(errno));
  else
    {
      kused = bytes>>10;
      if ((kused > kleft) && (sp))
	{
	  prtbuf = (char *)CALLOC(64,sizeof(char));
	  sprintf(prtbuf,"disk nearly full: used %d Kbytes leaving %d",kused,kleft);
	  report_in_minibuffer(sp,prtbuf);
	  FREE(prtbuf);
	}
    }
  snd_close(ofd);
  return(0);
}

int snd_make_file(char *ofile, int chans, file_info *hdr, snd_fd **sfs, int length, snd_state *ss)
{
  /* create ofile, fill it by following sfs, use hdr for srate/type/format decisions */
  /* used only in this file and snd-chn (for external temps, snd->temp) */
  int ofd;
  int i,j,len,datumb,reporting=0,total=0,err=0;
  chan_info *cp=NULL;
  MUS_SAMPLE_TYPE **obufs;
  snd_io_error = SND_NO_ERROR;
  ofd = open_temp_file(ofile,chans,hdr,ss);
  if (ofd == -1) return(SND_CANNOT_OPEN_TEMP_FILE);
  datumb = mus_data_format_to_bytes_per_sample(hdr->format);
  obufs = (MUS_SAMPLE_TYPE **)CALLOC(chans,sizeof(MUS_SAMPLE_TYPE *));
  ss->stopped_explicitly = 0;
  for (i=0;i<chans;i++)
    {
      obufs[i] = (MUS_SAMPLE_TYPE *)CALLOC(FILE_BUFFER_SIZE,sizeof(MUS_SAMPLE_TYPE));
    }
  j=0;
  reporting = (length > (10 * MAX_BUFFER_SIZE));
  if (reporting) 
    {
      cp = sfs[0]->cp;
      start_progress_report(ss,cp->sound,NOT_FROM_ENVED);
    }
  if (chans == 1)
    {
      for (len=0;len<length;len++)
	{
	  NEXT_SAMPLE(obufs[0][j],sfs[0]);
	  j++;
	  if (j == FILE_BUFFER_SIZE)
	    {
	      err = mus_file_write(ofd,0,j-1,1,obufs);
	      j=0;
	      if (err == -1) break;
	      if (reporting)
		{
		  total += FILE_BUFFER_SIZE;
		  progress_report(ss,cp->sound,NULL,1,1,(Float)total / (Float)length,NOT_FROM_ENVED);
		}
	      check_for_event(ss);
	      if (ss->stopped_explicitly)
		{
		  ss->stopped_explicitly = 0;
		  snd_warning("file save cancelled by C-g");
		  snd_io_error = SND_OUTPUT_INTERRUPTED;
		  break;
		}
	    }
	}
    }
  else
    {
      for (len=0;len<length;len++)
	{
	  for (i=0;i<chans;i++)
	    {
	      NEXT_SAMPLE(obufs[i][j],sfs[i]);
	    }
	  j++;
	  if (j == FILE_BUFFER_SIZE)
	    {
	      err = mus_file_write(ofd,0,j-1,chans,obufs);
	      j=0;
	      if (err == -1) break;
	      if (reporting)
		{
		  total += FILE_BUFFER_SIZE;
		  progress_report(ss,cp->sound,NULL,1,1,(Float)total / (Float)length,NOT_FROM_ENVED);
		}
	      check_for_event(ss);
	      if (ss->stopped_explicitly)
		{
		  ss->stopped_explicitly = 0;
		  snd_warning("file save cancelled by C-g");
		  snd_io_error = SND_OUTPUT_INTERRUPTED;
		  break;
		}
	    }
	}
    }
  if ((snd_io_error == SND_NO_ERROR) && (j > 0))
    mus_file_write(ofd,0,j-1,chans,obufs);
  if (snd_io_error == SND_NO_ERROR)
    {
      close_temp_file(ofd,hdr,len*chans*datumb,any_selected_sound(ss));
      alert_new_file();
    }
  else snd_close(ofd);
  if (reporting) finish_progress_report(ss,cp->sound,NOT_FROM_ENVED);
  for (i=0;i<chans;i++) FREE(obufs[i]);
  FREE(obufs);
  return(snd_io_error);
}

#if FILE_PER_CHAN
static int multifile_save_edits(char *ofile, snd_info *sp, snd_fd **sfs, snd_state *ss, int save_as)
{
  /* write each channel as a separate file */
  int i,err=0,needs_free=1;
  chan_info *cp;
  snd_fd *cpfs[1];
  char *nfile,*file;
  for (i=0;i<sp->nchans;i++)
    {
      cp = sp->chans[i];
      cpfs[0] = sfs[i];
      if (save_as)
	{
	  nfile = snd_tempnam(ss);
	  snd_make_file(nfile,1,cp->hdr,cpfs,current_ed_samples(cp),ss);
#if HAVE_GUILE
	  file = multifile_save(sp->index,cp->chan);
	  if (file == NULL)
	    {
#endif
	  file = (char *)CALLOC(MUS_MAX_FILE_NAME,sizeof(char));
	  sprintf(file,"%s/%d.1",ofile,cp->chan);
#if HAVE_GUILE
	    }
	  else needs_free=0;
#endif
	  err = snd_copy_file(nfile,file);
	  if (needs_free) FREE(file);
	}
      else
	{
	  snd_make_file(ofile,1,cp->hdr,cpfs,current_ed_samples(cp),ss);
	  err = snd_copy_file(ofile,cp->filename);
	}
    }
  return(err);
}
#endif

static int only_save_edits(snd_info *sp, file_info *nhdr, char *ofile)
{
  snd_state *ss;
  int i,err;
  snd_fd **sf;
  ss = sp->state;
  sf = (snd_fd **)CALLOC(sp->nchans,sizeof(snd_fd *));
  for (i=0;i<sp->nchans;i++) sf[i] = init_sample_read(0,sp->chans[i],READ_FORWARD);
#if FILE_PER_CHAN
  if (sp->chan_type == FILE_PER_CHANNEL)
    err = multifile_save_edits(ofile,sp,sf,ss,TRUE);
  else
#endif
  err = snd_make_file(ofile,sp->nchans,nhdr,sf,current_ed_samples(sp->chans[0]),ss);
  for (i=0;i<sp->nchans;i++) free_snd_fd(sf[i]);
  FREE(sf);
  if (err == SND_NO_ERROR)
    for (i=0;i<sp->nchans;i++) reflect_save_as_in_edit_history(sp->chans[i],ofile);
  return(err);
}

static int save_edits_1(snd_info *sp)
{
  /* open temp, write current state, rename to old, reopen and clear all state */
  /* can't overwrite current because we may have cut/paste backpointers scattered around the current edit list */
  /* have to decide here what header/data type to write as well -- original? */
  /* if latter, must be able to write all headers! -- perhaps warn user and use snd/aiff/riff/ircam */
  char *ofile=NULL;
  int err,saved_errno = 0;
  snd_state *ss;
  int i,samples;
  chan_info *cp;
  axis_info *ap;
  snd_fd **sf;
  Float *axis_data;
  int *ffts,*waves;
  file_info *sphdr;
  ss = sp->state;
#if HAVE_GUILE
  if (dont_save(ss,sp,NULL)) return(SND_NO_ERROR);
#endif
  samples = current_ed_samples(sp->chans[0]);
  snd_io_error = SND_NO_ERROR;
  ofile = snd_tempnam(ss); 
  /* this will use user's TMPDIR if temp_dir(ss) is not set, else stdio.h's P_tmpdir else /tmp */
  axis_data = (Float *)CALLOC(4*sp->nchans,sizeof(Float));
  ffts = (int *)CALLOC(sp->nchans,sizeof(int));
  waves = (int *)CALLOC(sp->nchans,sizeof(int));
  for (i=0;i<sp->nchans;i++)
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
  sf = (snd_fd **)CALLOC(sp->nchans,sizeof(snd_fd *));
  for (i=0;i<sp->nchans;i++) sf[i] = init_sample_read(0,sp->chans[i],READ_FORWARD);
  sprintf(edit_buf,"saving %s",sp->shortname);
  report_in_minibuffer(sp,edit_buf);
  sphdr = sp->hdr;
#if FILE_PER_CHAN
  if (sp->chan_type == FILE_PER_CHANNEL)
    snd_io_error = multifile_save_edits(ofile,sp,sf,ss,FALSE);
  else
#endif
  snd_io_error = snd_make_file(ofile,sp->nchans,sp->hdr,sf,samples,ss);
  if (snd_io_error != SND_NO_ERROR) 
    {
      for (i=0;i<sp->nchans;i++) free_snd_fd(sf[i]);
      FREE(sf);
      FREE(ffts);
      FREE(waves);
      FREE(axis_data);
      return(snd_io_error);
    }
  sphdr->samples = samples*sp->nchans;
  collapse_marks(sp);
  for (i=0;i<sp->nchans;i++)
    {
      cp = sp->chans[i];
      if (cp->mixes) reset_mix_list(cp);
      if (cp->edits) free_edit_list(cp);
      free_snd_fd(sf[i]);  /* must precede free_sound_list since it access the snd_data structs that free_sound_list frees */
      if (cp->sounds) free_sound_list(cp);
    }
  FREE(sf);
#if FILE_PER_CHAN
  if (sp->chan_type == FILE_PER_SOUND)
    {
      err = 0;
      saved_errno = 0;
#endif

#if (!HAVE_ACCESS)
  err = 0;
#else
  err = access(sp->fullname,W_OK);
#endif
  /* very weird -- in Linux we can write a write-protected file?? */
  if (err == 0)
    {
      err = snd_copy_file(ofile,sp->fullname);
      if (err) saved_errno = errno;
    }
  else saved_errno = errno;
#if FILE_PER_CHAN
    }
#endif
  sp->write_date = file_write_date(sp->fullname);

  add_sound_data(sp->fullname,sp,ss);

  for (i=0;i<sp->nchans;i++)
    {
      cp = sp->chans[i];
      set_axes(cp,axis_data[(i*4)+0],axis_data[(i*4)+1],axis_data[(i*4)+2],axis_data[(i*4)+3]);
      update_graph(cp,NULL); /* get normalized state before messing with it */
      if (ffts[i]) fftb(cp,TRUE);
      if (!(waves[i])) waveb(cp,FALSE);
      reflect_edit_history_change(cp);
    }
  FREE(axis_data);
  FREE(waves);
  FREE(ffts);

  reflect_file_revert_in_label(sp);
  reflect_file_save_in_menu(ss);
  if (err)
    sprintf(edit_buf,"write failed: %s, edits saved in: %s",strerror(saved_errno),ofile);
  else sprintf(edit_buf,"wrote %s",sp->fullname); 
  report_in_minibuffer(sp,edit_buf);
  if (ofile) {free(ofile); ofile=NULL;}
  if (auto_update(ss)) map_over_sounds(ss,snd_not_current,NULL);
  return(SND_NO_ERROR); /* don't erase our error message for the special write-permission problem */
}

int save_edits_2(snd_info *sp, char *new_name, int type, int format, int srate, char *comment)
{ /* file save as menu option -- changed 19-June-97 to retain current state after writing */
  file_info *hdr,*ohdr;
  int res;
#if HAVE_GUILE
  if (dont_save(sp->state,sp,new_name)) return(SND_NO_ERROR);
#endif
  if (MUS_DATA_FORMAT_OK(format))
    {
      if (MUS_HEADER_TYPE_OK(type))
	{
	  ohdr = sp->hdr;
	  hdr = copy_header(new_name,ohdr);
	  hdr->format = format;
	  hdr->srate = srate;
	  hdr->type = type;
	  hdr->comment = comment;
	  hdr->data_location = 0; /* in case comment changes it */
	  res = only_save_edits(sp,hdr,new_name);
	  free_file_info(hdr);
	  return(res);
	}
      else 
	{
	  snd_error("unknown header type?!? %d ",type);
	  return(SND_UNSUPPORTED_HEADER_TYPE);
	}
    }
  else snd_error("impossible data format?!? %d ",format);
  return(SND_UNSUPPORTED_DATA_FORMAT);
}

int chan_save_edits(chan_info *cp, char *ofile)
{
  /* channel extraction -- does not (normally) cause reversion of edits, or change of in-window file, etc */
  snd_info *sp;
  snd_fd **sf;
  int err;
  char *nfile;
  snd_state *ss;
  ss = cp->state;
  sp = cp->sound;
  snd_io_error = SND_NO_ERROR;
  if (!(snd_overwrite_ok(ss,ofile))) return(SND_NO_ERROR); /* no error because decision was explicit */
  if (strcmp(ofile,sp->fullname) == 0)
    {
      if (sp->read_only)
	{
	  sprintf(edit_buf,"can't save channel as %s (%s is write-protected)",ofile,sp->shortname);
	  report_in_minibuffer(sp,edit_buf);
	  return(SND_CANNOT_WRITE_DATA);
	}
      /* here we're overwriting the current (possibly multi-channel) file with one of its channels */
      nfile = snd_tempnam(ss); 
      sf = (snd_fd **)CALLOC(1,sizeof(snd_fd *));
      sf[0] = init_sample_read(0,cp,READ_FORWARD);
      snd_io_error = snd_make_file(nfile,1,sp->hdr,sf,current_ed_samples(cp),cp->state);
      free_snd_fd(sf[0]);
      FREE(sf);
      if (snd_io_error != SND_NO_ERROR)
	{
	  sprintf(edit_buf,"save channel as temp: %s: %s (%s)",nfile,strerror(errno),snd_error_name(snd_io_error));
	  report_in_minibuffer(sp,edit_buf);
	}
      else err = snd_copy_file(nfile,ofile);
      reflect_save_as_in_edit_history(cp,ofile);
      snd_update(ss,sp);
      free(nfile);
    }
  else
    {
      sf = (snd_fd **)CALLOC(1,sizeof(snd_fd *));
      sf[0] = init_sample_read(0,cp,READ_FORWARD);
      snd_io_error = snd_make_file(ofile,1,sp->hdr,sf,current_ed_samples(cp),cp->state);
      free_snd_fd(sf[0]);
      FREE(sf);
    }
  return(snd_io_error);
}

int save_edits(snd_info *sp, void *ptr)
{
  int i,need_save,err;
  time_t current_write_date;
  chan_info *cp;
  if (!sp->read_only)
    {
      need_save = 0;
      for (i=0;i<sp->nchans;i++)
	{
	  cp = sp->chans[i];
	  if (cp->edit_ctr > 0) 
	    {
	      need_save = 1;
	      break;
	    }
	}
      if (need_save)
	{
	  errno = 0;
	  /* check for change to file while we were editing it */
	  current_write_date = file_write_date(sp->fullname);
	  if (current_write_date != sp->write_date)
	    {
	      sprintf(edit_buf,"%s changed on disk! Save anyway?",sp->shortname);
	      err = snd_yes_or_no_p(sp->state,edit_buf);
	      if (err == 0) return(0);
	    }
	  err = save_edits_1(sp);
	  if (err)
	    {
	      sprintf(edit_buf,"%s: %s (%s)",sp->fullname,strerror(errno),snd_error_name(err));
	      report_in_minibuffer(sp,edit_buf);
	    }
	  else
	    {
	      if (sp->edited_region) save_region_backpointer(sp);
	    }
	}
      else
	{
	  report_in_minibuffer(sp,"(no changes need to be saved)");
	}
    }
  else
    {
      sprintf(edit_buf,"can't write %s (it is read-only)",sp->shortname);
      report_in_minibuffer(sp,edit_buf);
    }
  return(0);
}

int revert_edits(chan_info *cp, void *ptr)
{
  snd_info *sp;
  int old_ctr;
  old_ctr = cp->edit_ctr;
  sp = cp->sound;
  if (sp->playing) stop_playing_sound(sp);
  cp->edit_ctr = 0;
  reflect_edit_counter_change(cp);
  reflect_sample_change_in_axis(cp);
  update_graph(cp,NULL);
  if ((cp->mix_md) && (old_ctr != 0)) reflect_mix_edit(cp,"revert-sound");
  call_undo_hook(cp,TRUE);
  return(0);
}

void undo_edit(chan_info *cp, int count)
{
  snd_info *sp;
  if ((cp) && (cp->edit_ctr > 0) && (count != 0))
    {
      sp = cp->sound;
      if (sp->playing) stop_playing_sound(sp);
      cp->edit_ctr -= count; 
      if (cp->edit_ctr < 0) cp->edit_ctr = 0;
      reflect_edit_counter_change(cp);
      reflect_sample_change_in_axis(cp);
      reflect_undo_in_menu();
      if (cp->edit_ctr == 0)
	{
	  reflect_file_revert_in_label(sp);
	  reflect_file_revert_in_menu(cp->state);
	}
      update_graph(cp,NULL);
      if (cp->mix_md) reflect_mix_edit(cp,"undo");
      call_undo_hook(cp,TRUE);
    }
}

void undo_EDIT(void *ptr, int count)
{
  chan_info *cp;
  snd_info *sp;
  int i;
  sync_info *si;
  if (count < 0)
    redo_EDIT(ptr,-count);
  else
    {
      si = NULL;
      cp = current_channel(ptr);
      if (cp)
	{
	  sp = cp->sound;
	  if (sp->syncing != 0) si = snd_sync(cp->state,sp->syncing);
	  if (si)
	    {
	      for (i=0;i<si->chans;i++) undo_edit(si->cps[i],count);
	      free_sync_info(si);
	    }
	  else undo_edit(cp,count);
	}
    }
}

void redo_edit(chan_info *cp, int count)
{
  snd_info *sp;
  if (cp)
    {
      sp = cp->sound;
      if (sp->playing) stop_playing_sound(sp);
      cp->edit_ctr += count; 
      while ((cp->edit_ctr >= cp->edit_size) || (!(cp->edits[cp->edit_ctr]))) {cp->edit_ctr--;}
      if (((cp->edit_ctr+1) == cp->edit_size) || (!(cp->edits[cp->edit_ctr+1]))) 
	reflect_no_more_redo_in_menu();
      if (cp->edit_ctr != 0) /* possibly a sync'd redo to chan that has no edits */
	{
	  reflect_file_change_in_label(cp);
	  reflect_redo_in_menu();
	  reflect_edit_counter_change(cp);
	  reflect_sample_change_in_axis(cp);
	  update_graph(cp,NULL);
	  if (cp->mix_md) reflect_mix_edit(cp,"redo");
	}
      call_undo_hook(cp,FALSE);
    }
}

void redo_EDIT(void *ptr, int count)
{
  chan_info *cp;
  snd_info *sp;
  int i;
  sync_info *si;
  if (count < 0)
    undo_EDIT(ptr,-count);
  else
    {
      si = NULL;
      cp = current_channel(ptr);
      if (cp)
	{
	  sp = cp->sound;
	  if (sp->syncing != 0) si = snd_sync(cp->state,sp->syncing);
	  if (si)
	    {
	      for (i=0;i<si->chans;i++) redo_edit(si->cps[i],count);
	      free_sync_info(si);
	    }
	  else redo_edit(cp,count);
	}
    }
}


#if HAVE_GUILE
#include "sg.h"

#if DEBUGGING
static SCM g_display_edits(SCM snd, SCM chn)
{
  #define H_display_edits " prints current edit tree state"
  display_edits(get_cp(snd,chn,"display-edits"));
  return(SCM_BOOL_F);
}
#endif

static SCM g_edit_fragment(SCM uctr, SCM snd, SCM chn)
{
  #define H_edit_fragment "(" S_edit_fragment " ctr &optional snd chn) returns the edit history entry at 'ctr'\n\
   associated with snd's channel chn; this is a list (origin type start-sample samps)"

  chan_info *cp;
  ed_list *ed;
  int ctr;
  ERRCP(S_edit_fragment,snd,chn,2);
  ERRB1(uctr,S_edit_fragment);
  cp = get_cp(snd,chn,S_edit_fragment);
  ctr = g_scm2intdef(uctr,cp->edit_ctr);
  if ((ctr < cp->edit_size) && (ctr >= 0))
    {
      ed = cp->edits[ctr];
      if (ed) 
	return(SCM_LIST4(gh_str02scm(ed->origin),
			 gh_str02scm(edit_names[EDIT_TYPE(ed->sfnum)]),
			 gh_int2scm(ed->beg),
			 gh_int2scm(ed->len)));
    }
  return(scm_throw(NO_SUCH_EDIT,SCM_LIST4(gh_str02scm(S_edit_fragment),snd,chn,uctr)));
}

/* ---------------- sample readers ---------------- */

static int sf_tag = 0;
static SCM mark_sf(SCM obj) {SCM_SETGC8MARK(obj); return(SCM_BOOL_F);}

int sf_p(SCM obj); /* currently for snd-ladspa.c */
int sf_p(SCM obj) {return((SCM_NIMP(obj)) && (GH_TYPE_OF(obj) == (SCM)sf_tag));}

static SCM g_sf_p(SCM obj) 
{
  #define H_sf_p "(" S_sample_readerQ " obj) -> #t if obj is a sample-reader"
  RTNBOOL(sf_p(obj));
}

snd_fd *get_sf(SCM obj); /* currently for snd-ladspa.c */
snd_fd *get_sf(SCM obj) {if (sf_p(obj)) return((snd_fd *)GH_VALUE_OF(obj)); else return(NULL);}

static int print_sf(SCM obj, SCM port, scm_print_state *pstate) 
{
  char *desc,*name;
  chan_info *cp;
  snd_fd *fd;
  fd = get_sf(obj);
  if (fd == NULL)
    scm_puts("<null>",port);
  else
    {
      cp = fd->cp;
      if (fd->local_sp) 
	name = ((fd->local_sp)->hdr)->name;
      else
	{
	  if (cp) 
	    name = (cp->sound)->shortname;
	  else name = "unknown source";
	}
      desc = (char *)CALLOC(128,sizeof(char));
      sprintf(desc,"<sample-reader %p: %s from %d, at %d (%.4f)",
	      fd,
	      name,
	      fd->initial_samp,
	      current_location(fd),
	      MUS_SAMPLE_TO_FLOAT(fd->current_value));
      scm_puts(desc,port); 
      FREE(desc);
    }
  return(1);
}

static SCM equalp_sf(SCM obj1, SCM obj2) 
{
  RTNBOOL(get_sf(obj1) == get_sf(obj2));
}

static scm_sizet free_sf(SCM obj) 
{
  snd_fd *fd = (snd_fd *)GH_VALUE_OF(obj); 
  if (fd) 
    {
      if (fd->local_sp) {completely_free_snd_info(fd->local_sp); fd->local_sp = NULL;}
      free_snd_fd(fd); 
    }
  return(0);
}

#if HAVE_GUILE_1_3_0
static scm_smobfuns sf_smobfuns = {
  &mark_sf,
  &free_sf,
  &print_sf,
  &equalp_sf};
#endif

static SCM g_sample_reader_at_end(SCM obj) 
{
  #define H_sample_reader_at_end "(" S_sample_reader_at_endQ " obj) -> #t if sample-reader has reached the end of its data"
  SCM_ASSERT(sf_p(obj),obj,SCM_ARG1,S_sample_reader_at_endQ);
  RTNBOOL(read_sample_eof(get_sf(obj)));
}

SCM g_c_make_sample_reader(snd_fd *fd)
{
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(sf_tag,(SCM)fd);
#else
  SCM new_sf;
  SCM_NEWCELL(new_sf);
  SCM_SETCDR(new_sf,(SCM)fd);
  SCM_SETCAR(new_sf,sf_tag);
  return(new_sf);
#endif
}

static SCM g_make_sample_reader(SCM samp_n, SCM snd, SCM chn, SCM dir1, SCM pos) /* "dir" confuses Mac OS-X Objective-C! */
{
  #define H_make_sample_reader "(" S_make_sample_reader " &optional (start-samp 0) snd chn (dir 1) edit-position)\n\
   returns a reader ready to access snd's channel chn's data starting at 'start-samp', going in direction 'dir'\n\
   (-1 = backward), reading the version of the data indicated by 'edit-position' which defaults to the current version.\n\
   snd can be a filename, a sound index number, or a list with a mix id number."

  snd_fd *fd = NULL;
  int edpos,chan;
  chan_info *cp;
  char *loc_name;
  snd_state *ss;
  snd_info *loc_sp = NULL;
#if HAVE_GUILE_1_3_0
  SCM new_sf;
#endif
  ERRB1(samp_n,S_make_sample_reader);
  ERRB4(dir1,S_make_sample_reader);
  ss = get_global_state();
  if (gh_string_p(snd))
    {
      if (!((gh_number_p(chn)) || (SCM_FALSEP(chn)) || (SCM_UNBNDP(chn)))) scm_wrong_type_arg(S_make_sample_reader,3,chn);
      loc_name = gh_scm2newstr(snd,NULL);
      loc_sp = make_sound_readable(ss,loc_name,FALSE);
      free(loc_name);
      if (loc_sp == NULL) return(scm_throw(NO_SUCH_SOUND,SCM_LIST1(gh_str02scm(S_make_sample_reader))));
      chan = g_scm2intdef(chn,0);
      if ((chan < 0) || (chan > loc_sp->nchans))
	{
	  completely_free_snd_info(loc_sp);
	  return(scm_throw(NO_SUCH_CHANNEL,SCM_LIST3(gh_str02scm(S_make_sample_reader),snd,chn)));
	}
      cp = loc_sp->chans[chan];
    }
  else 
    {
      ERRCP(S_make_sample_reader,snd,chn,2);
      cp = get_cp(snd,chn,S_make_sample_reader);
    }
  edpos = g_scm2intdef(pos,cp->edit_ctr);
  fd = init_sample_read_any(g_scm2intdef(samp_n,0),cp,g_scm2intdef(dir1,1),edpos);
  fd->local_sp = loc_sp;
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(sf_tag,(SCM)fd);
#else
  SCM_NEWCELL(new_sf);
  SCM_SETCDR(new_sf,(SCM)fd);
  SCM_SETCAR(new_sf,sf_tag);
  return(new_sf);
#endif
  return(SCM_BOOL_F);
}

static SCM g_make_region_sample_reader(SCM samp_n, SCM reg, SCM chn, SCM dir1)
{
  #define H_make_region_sample_reader "(" S_make_region_sample_reader " &optional (start-samp 0) (region 0) chn (dir 1))\n\
   returns a reader ready to access region's channel chn data starting at 'start-samp' going in direction 'dir'"

  snd_fd *fd = NULL;
#if HAVE_GUILE_1_3_0
  SCM new_sf;
#endif
  ERRB1(samp_n,S_make_sample_reader);
  ERRB2(reg,S_make_sample_reader);
  ERRB3(chn,S_make_sample_reader);
  ERRB4(dir1,S_make_sample_reader);
  fd = init_region_read(get_global_state(),g_scm2intdef(samp_n,0),g_scm2intdef(reg,0),g_scm2intdef(chn,0),g_scm2intdef(dir1,1));
  if (fd)
    {
#if (!HAVE_GUILE_1_3_0)
      SCM_RETURN_NEWSMOB(sf_tag,(SCM)fd);
#else
      SCM_NEWCELL(new_sf);
      SCM_SETCDR(new_sf,(SCM)fd);
      SCM_SETCAR(new_sf,sf_tag);
      return(new_sf);
#endif
    }
  return(SCM_BOOL_F);
}

static SCM g_next_sample(SCM obj)
{
  #define H_next_sample "(" S_next_sample " reader) -> next sample from reader"
  SCM_ASSERT(sf_p(obj),obj,SCM_ARG1,S_next_sample);
  return(gh_double2scm(next_sample(get_sf(obj))));
}

static SCM g_previous_sample(SCM obj)
{
  #define H_previous_sample "(" S_previous_sample " reader) -> previous sample from reader"
  SCM_ASSERT(sf_p(obj),obj,SCM_ARG1,S_previous_sample);
  return(gh_double2scm(previous_sample(get_sf(obj))));
}

static SCM g_free_sample_reader(SCM obj)
{
  #define H_free_sample_reader "(" S_free_sample_reader " reader) frees sample reader 'reader'"
  snd_fd *fd;
  SCM_ASSERT(sf_p(obj),obj,SCM_ARG1,S_free_sample_reader);
  fd = get_sf(obj);
  if (fd->local_sp)
    {
      completely_free_snd_info(fd->local_sp);
      fd->local_sp = NULL;
    }
  free_snd_fd(fd);
  GH_SET_VALUE_OF(obj,(SCM)NULL);
  return(SCM_BOOL_F);
}

typedef Float (*g_plug)(Float val);

static SCM g_loop_samples(SCM reader, SCM proc, SCM calls, SCM origin)
{
  #define H_loop_samples "(" S_loop_samples " reader func calls origin) calls (func (reader)) 'calls' times,\n\
   replacing current data with the function results; origin is the edit-history name for this operation"

  /* proc here is a pointer to a float procedure that takes a float arg */
  g_plug func;
  chan_info *cp;
  snd_info *sp;
  char *ofile;
  snd_state *ss;
  int num,i,j=0,ofd,datumb,err=0;
  MUS_SAMPLE_TYPE val;
  snd_fd *sf;
  file_info *hdr;
  MUS_SAMPLE_TYPE **data;
  MUS_SAMPLE_TYPE *idata;
  SCM_ASSERT(sf_p(reader),reader,SCM_ARG1,S_loop_samples);
  SCM_ASSERT(gh_number_p(calls),calls,SCM_ARG3,S_loop_samples);
  SCM_ASSERT(gh_string_p(origin),origin,SCM_ARG4,S_loop_samples);
  num = g_scm2int(calls);
  sf = get_sf(reader);
  cp = sf->cp;
  ss = cp->state;
  func = (g_plug)gh_scm2ulong(proc);
  ofile = snd_tempnam(ss);
  sp = (cp->sound);
  hdr = make_temp_header(ss,ofile,sp->hdr,num);
  hdr->chans = 1;
  ofd = open_temp_file(ofile,1,hdr,ss);
  datumb = mus_data_format_to_bytes_per_sample(hdr->format);
  data = (MUS_SAMPLE_TYPE **)CALLOC(1,sizeof(MUS_SAMPLE_TYPE *));
  data[0] = (MUS_SAMPLE_TYPE *)CALLOC(MAX_BUFFER_SIZE,sizeof(MUS_SAMPLE_TYPE)); 
  idata = data[0];
  for (i=0;i<num;i++)
    {
      NEXT_SAMPLE(val,sf);
      idata[j] = MUS_FLOAT_TO_SAMPLE((*func)(MUS_SAMPLE_TO_FLOAT(val)));
      j++;
      if (j == MAX_BUFFER_SIZE)
	{
	  err = mus_file_write(ofd,0,j-1,1,data);
	  j=0;
	  if (err == -1) break;
	  if (ss->stopped_explicitly) break;
	}
    }
  if (j > 0) mus_file_write(ofd,0,j-1,1,data);
  close_temp_file(ofd,hdr,num*datumb,sp);
  hdr = free_file_info(hdr);
  file_change_samples(sf->initial_samp,num,ofile,cp,0,DELETE_ME,LOCK_MIXES,gh_scm2newstr(origin,NULL));
  update_graph(cp,NULL); /* is this needed? */
  if (ofile) free(ofile);
  FREE(data[0]);
  FREE(data);
  return(SCM_BOOL_F);
}


static SCM g_save_edit_history(SCM filename, SCM snd, SCM chn)
{
  #define H_save_edit_history "(" S_save_edit_history " filename &optional snd chn) saves snd channel's chn edit history in filename"
  FILE *fd;
  int i,j;
  snd_info *sp;
  chan_info *cp;
  char *mcf = NULL,*urn;
  snd_state *ss;
  ERRS1(filename,S_save_edit_history);
  ERRCP(S_save_edit_history,snd,chn,2);
  urn = gh_scm2newstr(filename,NULL);
  mcf = mus_file_full_name(urn);
  free(urn);
  fd = fopen(mcf,"w");
  if (mcf) FREE(mcf);
  if (fd)
    {
      if ((gh_number_p(chn)) && (gh_number_p(snd)))
	{
	  cp = get_cp(snd,chn,S_save_edit_history);
	  edit_history_to_file(fd,cp);
	}
      else
	{
	  if (gh_number_p(snd))
	    {
	      sp = get_sp(snd);
	      if (sp)
		for (i=0;i<sp->nchans;i++)
		  edit_history_to_file(fd,sp->chans[i]);
	    }
	  else
	    {
	      ss = get_global_state();
	      for (i=0;i<ss->max_sounds;i++)
		{
		  if ((sp=((snd_info *)(ss->sounds[i]))))
		    {
		      if (sp->inuse)
			{
			  for (j=0;j<sp->nchans;j++)
			    edit_history_to_file(fd,sp->chans[j]);
			}
		    }
		}
	    }
	}
      fclose(fd);
      return(SCM_BOOL_T);
    }
  return(scm_throw(CANNOT_SAVE,SCM_LIST1(gh_str02scm(S_save_edit_history))));
}

static SCM g_undo(SCM ed_n, SCM snd_n, SCM chn_n) /* opt ed_n */
{
  #define H_undo "("  S_undo " &optional (count 1) snd chn) undoes count edits in snd's channel chn"
  chan_info *cp;
  ERRCP(S_undo,snd_n,chn_n,2);
  cp = get_cp(snd_n,chn_n,S_undo);
  if (gh_number_p(ed_n))
    undo_EDIT(cp,g_scm2int(ed_n));
  else undo_EDIT(cp,1);
  update_graph(cp,NULL);
  return(SCM_BOOL_T);
}

static SCM g_redo(SCM ed_n, SCM snd_n, SCM chn_n) /* opt ed_n */
{
  #define H_redo "("  S_redo " &optional (count 1) snd chn) redoes count edits in snd's channel chn"
  chan_info *cp;
  ERRCP(S_redo,snd_n,chn_n,2);
  cp = get_cp(snd_n,chn_n,S_redo);
  if (gh_number_p(ed_n))
    redo_EDIT(cp,g_scm2int(ed_n));
  else redo_EDIT(cp,1);
  update_graph(cp,NULL);
  return(SCM_BOOL_T);
}

static int chan_ctr=0;

static int init_as_one_edit(chan_info *cp, void *ptr) 
{
  ((int *)ptr)[chan_ctr] = cp->edit_ctr; 
  chan_ctr++; 
  return(0);
}

static int finish_as_one_edit(chan_info *cp, void *ptr) 
{
  int one_edit;
  one_edit = (((int *)ptr)[chan_ctr]+1);
  if (cp->edit_ctr > one_edit)
    {
      while (cp->edit_ctr > one_edit) backup_edit_list(cp);
      if (cp->mixes) backup_mix_list(cp,one_edit);
    }
  update_graph(cp,NULL); 
  chan_ctr++; 
  return(0);
}

static SCM g_as_one_edit(SCM proc)
{
  #define H_as_one_edit "(" S_as_one_edit " func) runs func, collecting all edits into one from the edit historys' point of view"
  int chans;
  int *cur_edits;
  snd_state *ss;
  SCM result = SCM_BOOL_F;
  SCM_ASSERT(gh_procedure_p(proc),proc,SCM_ARG1,S_as_one_edit);
  ss = get_global_state();
  chans = active_channels(ss,TRUE);
  if (chans > 0)
    {
      cur_edits = (int *)CALLOC(chans,sizeof(int));
      chan_ctr = 0;
      map_over_chans(ss,init_as_one_edit,(void *)cur_edits);
      result = g_call0(proc);
      chan_ctr = 0;
      map_over_chans(ss,finish_as_one_edit,(void *)cur_edits);
      FREE(cur_edits);
    }
  return(result);
}

#if (!HAVE_GUILE_1_3_0)
static int dont_edit(chan_info *cp) 
{
  SCM res = SCM_BOOL_F;
  if (HOOKED(cp->edit_hook))
    res = g_c_run_or_hook(cp->edit_hook,SCM_EOL);
  return(SCM_TRUE_P(res));
}

static void call_undo_hook(chan_info *cp, int undo)
{
  if (HOOKED(cp->undo_hook))
    g_c_run_progn_hook(cp->undo_hook,SCM_EOL);
}

static SCM save_hook;
static int dont_save(snd_state *ss, snd_info *sp, char *newname)
{
  SCM res = SCM_BOOL_F;
  if (!(ss->save_hook_active))
    {
      if (HOOKED(save_hook))
	{
	  ss->save_hook_active = 1;
	  if (newname)
	    res = g_c_run_or_hook(save_hook,SCM_LIST2(gh_int2scm(sp->index),gh_str02scm(newname)));
	  else res = g_c_run_or_hook(save_hook,SCM_LIST2(gh_int2scm(sp->index),SCM_BOOL_F));
	  ss->save_hook_active = 0;
	}
    }
  return(SCM_TRUE_P(res));
}
#endif

void g_init_edits(SCM local_doc)
{
#if (!HAVE_GUILE_1_3_0)
  /* sf_tag = scm_make_smob_type_mfpe("sf",sizeof(SCM),mark_sf,free_sf,print_sf,equalp_sf); */
  sf_tag = scm_make_smob_type("sf",sizeof(SCM));
  scm_set_smob_mark(sf_tag,mark_sf);
  scm_set_smob_print(sf_tag,print_sf);
  scm_set_smob_free(sf_tag,free_sf);
  scm_set_smob_equalp(sf_tag,equalp_sf);
#else
  sf_tag = scm_newsmob(&sf_smobfuns);
#endif
  DEFINE_PROC(gh_new_procedure(S_make_sample_reader,SCM_FNC g_make_sample_reader,0,5,0),H_make_sample_reader);
  DEFINE_PROC(gh_new_procedure(S_make_region_sample_reader,SCM_FNC g_make_region_sample_reader,0,4,0),H_make_region_sample_reader);
  DEFINE_PROC(gh_new_procedure(S_next_sample,SCM_FNC g_next_sample,1,0,0),H_next_sample);
  DEFINE_PROC(gh_new_procedure(S_previous_sample,SCM_FNC g_previous_sample,1,0,0),H_previous_sample);
  DEFINE_PROC(gh_new_procedure(S_free_sample_reader,SCM_FNC g_free_sample_reader,1,0,0),H_free_sample_reader);
  DEFINE_PROC(gh_new_procedure(S_sample_readerQ,SCM_FNC g_sf_p,1,0,0),H_sf_p);
  DEFINE_PROC(gh_new_procedure(S_sample_reader_at_endQ,SCM_FNC g_sample_reader_at_end,1,0,0),H_sample_reader_at_end);
  DEFINE_PROC(gh_new_procedure(S_loop_samples,SCM_FNC g_loop_samples,4,0,0),H_loop_samples);

  DEFINE_PROC(gh_new_procedure(S_save_edit_history,SCM_FNC g_save_edit_history,1,2,0),H_save_edit_history);
  DEFINE_PROC(gh_new_procedure(S_edit_fragment,SCM_FNC g_edit_fragment,0,3,0),H_edit_fragment);
  DEFINE_PROC(gh_new_procedure(S_undo,SCM_FNC g_undo,0,3,0),H_undo);
  DEFINE_PROC(gh_new_procedure(S_redo,SCM_FNC g_redo,0,3,0),H_redo);
  DEFINE_PROC(gh_new_procedure1_0(S_as_one_edit,g_as_one_edit),H_as_one_edit);

#if DEBUGGING
  DEFINE_PROC(gh_new_procedure("display-edits",SCM_FNC g_display_edits,0,2,0),H_display_edits);
#endif

#if (!HAVE_GUILE_1_3_0)
  save_hook = scm_create_hook(S_save_hook,2);                   /* arg = sound index, possible new name */
#endif
}
#endif
