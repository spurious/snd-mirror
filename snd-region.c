#include "snd.h" 

/* TODO region creation should happen in the background (and be cancelable)
 */

#define REGION_ARRAY 0
#define REGION_FILE 1
/* region data can be stored either in-core (if less than MAX_BUFFER_SIZE ints), else in a temp file that */
/*    is deleted when the region is deleted (hence must be copied upon insert or mix) */

#define CLEAR_REGION_DATA 0
#define COMPLETE_DELETION 1


static int region_id_ctr = 0;

typedef struct {
  MUS_SAMPLE_TYPE **data;
  int chans;
  int frames;
  int srate;               /* for file save (i.e. region->file) */
  int header_type;         /* for file save */
  int save;
  snd_info *rsp;
  char *name,*start,*end;  /* for region browser */
  char *filename;          /* if region data is stored in a temp file */
  int use_temp_file;       /* REGION_ARRAY = data is in 'data' arrays, else in temp file 'filename' */
  Float maxamp;
  snd_info *editor_copy;
  char *editor_name;
  int id;
} region;

static void free_region(region *r, int complete)
{
  int i;
  snd_info *sp;
  /* if not complete, just clear out old data (edited region being saved) */
  if (r)
    {
      if ((complete == COMPLETE_DELETION) && (r->editor_copy))
	{
	  sp = r->editor_copy; 
	  sp->edited_region = NULL;
	  r->editor_copy = NULL;
	}
      if (r->data)  /* null if temp file */
	{
	  for (i=0;i<r->chans;i++) 
	    if (r->data[i]) 
	      FREE(r->data[i]);
	  FREE(r->data);
	  r->data = NULL;
	}
      if (complete == COMPLETE_DELETION)
	{
	  if (r->name) FREE(r->name);
	  if (r->start) FREE(r->start);
	  if (r->end) FREE(r->end);
	}
      if (r->use_temp_file == REGION_FILE) /* we can delete this temp file because all references copy first */
	{
	  if (r->filename)
	    {
	      mus_sound_forget(r->filename);
	      remove(r->filename);
	      FREE(r->filename);   /* ok because tempnam used */
	    }
	  r->filename = NULL;
	}
      if (r->rsp) 
	r->rsp = completely_free_snd_info(r->rsp);
      if (complete == COMPLETE_DELETION) FREE(r);
    }
}

static region **regions = NULL; /* regions[0] => current global selection from X viewpoint */
static int regions_size = 0;

void allocate_regions(snd_state *ss,int numreg)
{
  int i;
  if (numreg > regions_size)
    {
      if (regions)
	{
	  regions = (region **)REALLOC(regions,numreg * sizeof(region *));
	  for (i=regions_size;i<numreg;i++) regions[i] = NULL;
	}
      else regions = (region **)CALLOC(numreg,sizeof(region *));
    }
  else
    {
      if (regions_size > numreg)
	{
	  for (i=numreg;i<regions_size;i++)
	    if (regions[i])
	      {
		free_region(regions[i],COMPLETE_DELETION);
		regions[i] = NULL;
	      }
	  if (region_browser_is_active()) update_region_browser(ss,1);
	}
    }
  regions_size = numreg;
}

static void set_max_regions(snd_state *ss, int n)
{
  if (n >= 0)
    {
      allocate_regions(ss,n);
      allocate_region_rows(ss,n);
      in_set_max_regions(ss,n);
    }
}

int region_ok(int n) {return((n<regions_size) && (regions[n]));}
int region_len(int n) {if (region_ok(n)) return(regions[n]->frames); else return(0);}
int region_chans(int n) {if (region_ok(n)) return(regions[n]->chans); else return(0);}
int region_srate(int n) {if (region_ok(n)) return(regions[n]->srate); else return(0);}
Float region_maxamp(int n) {if (region_ok(n)) return(regions[n]->maxamp); else return(0.0);}
int region_id(int n) {if (region_ok(n)) return(regions[n]->id); else return(-1);}

static int id_region(int id)
{
  int i;
  for (i=0;i<regions_size;i++)
    if ((regions[i]) && (regions[i]->id == id))
      return(i);
  return(-1);
}

static Float region_sample(int reg, int chn, int samp)
{
  region *r;
  snd_fd *sf;
  Float val;
  if (region_ok(reg))
    {
      r = regions[reg];
      if ((samp < r->frames) && (chn < r->chans)) 
	{
	  if (r->use_temp_file == REGION_ARRAY)
	    return(MUS_SAMPLE_TO_FLOAT(r->data[chn][samp]));
	  else 
	    {
	      sf = init_region_read(get_global_state(),samp,reg,chn,READ_FORWARD);
	      val = next_sample_to_float(sf);
	      free_snd_fd(sf);
	      return(val);
	    }
	}
    }
  return(0.0);
}

static void region_samples(int reg, int chn, int beg, int num, Float *data)
{
  region *r;
  snd_fd *sf;
  int i,j;
  if (region_ok(reg))
    {
      r = regions[reg];
      if ((beg < r->frames) && (chn < r->chans))
	{
	  if (r->use_temp_file == REGION_ARRAY)
	    {
	      for (i=beg,j=0;(i<r->frames) && (j<num);i++,j++) 
		data[j] = MUS_SAMPLE_TO_FLOAT(r->data[chn][i]);
	    }
	  else
	    {
	      sf = init_region_read(get_global_state(),beg,reg,chn,READ_FORWARD);
	      for (i=beg,j=0;(i<r->frames) && (j<num);i++,j++) 
		data[j] = next_sample_to_float(sf);
	      free_snd_fd(sf);
	    }
	  if (j < num) for (;j<num;j++) data[j] = 0.0;
	}
    }
}

static int first_region_active(void)
{
  int i;
  for (i=0;i<regions_size;i++)
    if (regions[i]) 
      return(i);
  return(NO_REGIONS);
}
  
static int check_regions(void)
{
  int act;
  act = first_region_active();
  if (act == NO_REGIONS) reflect_no_regions_in_menu();
  return(act);
}

static void make_region_readable(region *r, snd_state *ss)
{
  snd_info *regsp;
  chan_info *cp;
  file_info *hdr;
  int *datai;
  int i,fd;
  if (r->rsp) return;
  regsp = (snd_info *)CALLOC(1,sizeof(snd_info));
  regsp->nchans = r->chans;
  regsp->allocated_chans = r->chans; /* needed for complete GC */
  regsp->chans = (chan_info **)CALLOC(r->chans,sizeof(chan_info *));
  regsp->hdr = (file_info *)CALLOC(1,sizeof(file_info));
  hdr = regsp->hdr;
  hdr->samples = r->frames * r->chans;
  hdr->srate = r->srate;
  hdr->chans = r->chans;
  hdr->comment = NULL;
  for (i=0;i<r->chans;i++)
    {
      cp = make_chan_info(NULL,i,regsp,ss);
      regsp->chans[i] = cp;
      add_channel_data_1(cp,regsp,ss,WITHOUT_GRAPH);
      set_initial_ed_list(cp,r->frames-1);
      cp->edit_size = 1;
      cp->sound_size = 1;
      cp->hookable = 0;
      if (r->use_temp_file == REGION_ARRAY)
	cp->sounds[0] = make_snd_data_buffer(r->data[i],r->frames,cp->edit_ctr);
      else
	{
	  hdr = make_file_info(r->filename,ss);
	  if (hdr)
	    {
	      fd = snd_open_read(ss,r->filename);
	      mus_file_set_descriptors(fd,
				       r->filename,
				       hdr->format,
				       mus_data_format_to_bytes_per_sample(hdr->format),
				       hdr->data_location,
				       hdr->chans,
				       hdr->type);
	      datai = make_file_state(fd,hdr,i,FILE_BUFFER_SIZE);
	      cp->sounds[0] = make_snd_data_file(r->filename,datai,
						 MUS_SAMPLE_ARRAY(datai[file_state_channel_offset(i)]),
						 hdr,DONT_DELETE_ME,cp->edit_ctr,i); /* don't auto-delete! */
	    }
	}
    }
  r->rsp = regsp;
}

file_info *fixup_region_data(chan_info *cp, int chan, int n)
{
  region *r;
  snd_info *nsp;
  chan_info *ncp;
  if (region_ok(n))
    {
      r = regions[n];
      if (chan < r->chans)
	{
	  make_region_readable(r,cp->state);
	  nsp = r->rsp;
	  ncp = nsp->chans[chan];
	  cp->sounds = ncp->sounds;
	  cp->sound_size = ncp->sound_size;
	  cp->edits = ncp->edits;
	  cp->edit_size = ncp->edit_size;
	  cp->edit_ctr = ncp->edit_ctr;
	  cp->samples[0] = ncp->samples[0];
	  cp->axis = ncp->axis;
	  initialize_scrollbars(cp);
	  return(nsp->hdr);
	}
    }
  return(NULL);
}

region_state *region_report(void)
{
  region_state *rs;
  int i,len;
  char *reg_buf;
  region *r;
  rs = (region_state *)CALLOC(1,sizeof(region_state));
  len = regions_size;
  for (i=0;i<regions_size;i++) 
    if (!(regions[i])) 
      {
	len = i; 
	break;
      }
  rs->len = len;
  if (len == 0) return(rs);
  rs->save = (int *)CALLOC(len,sizeof(int));
  rs->name = (char **)CALLOC(len,sizeof(char *));
  for (i=0;i<len;i++)
    {
      r = regions[i];
      rs->save[i] = r->save;
      reg_buf = (char *)CALLOC(64,sizeof(char));
      sprintf(reg_buf,"%d: %s (%s:%s)",i,r->name,r->start,r->end);
      rs->name[i] = reg_buf;
    }
  return(rs);
}

void free_region_state (region_state *r)
{
  int i;
  if (r)
    {
      for (i=0;i<r->len;i++)
	if (r->name[i]) FREE(r->name[i]);
      if (r->name) FREE(r->name);
      if (r->save) FREE(r->save);
      FREE(r);
    }
}

void select_region(int n) /* region browser */
{
  int i;
  region *r;
  if (region_ok(n))
    {
      r = regions[n];
      for (i=n;i>0;i--) regions[i]=regions[i-1]; 
      regions[0] = r;
    }
}

int delete_region(int n) /* region browser */
{
  int i;
  /* delete-region-hook? (passes region-id to hook, if #t, don't delete?) -- restack uses free_region instead */
  if (n >= regions_size) return(INVALID_REGION);
  if (region_ok(n)) 
    {
      stop_playing_region(n);
      free_region(regions[n],COMPLETE_DELETION);
    }
  for (i=n;i<regions_size-1;i++) regions[i]=regions[i+1]; 
  regions[regions_size-1] = NULL;
  return(check_regions());
}

void protect_region(int n,int protect) /* region browser */
{
  region *r;
  if (region_ok(n))
    {
      r = regions[n];
      if (r) r->save = protect;
    }
}

static void stack_region(snd_state *ss, region *r) 
{
  int i,okr = -1;
  /* leave protected regions alone -- search for highest unprotected region */
  for (i=max_regions(ss)-1;i>=0;i--) 
    {
      if ((!(regions[i])) || (!((regions[i])->save))) {okr = i; break;}
    }
  if (okr == -1)
    {
      /* all possible slots are taken by protected regions! */
      okr = regions_size;
      set_max_regions(ss,regions_size*2);
    }
  if (regions[okr]) 
    {
      stop_playing_region(okr);
      free_region(regions[okr],COMPLETE_DELETION);
    }
  for (i=okr;i>0;i--) regions[i]=regions[i-1]; 
  regions[0]=r;
  if (!r) check_regions();
}

static int save_region_1(snd_state *ss, char *ofile,int type, int format, int srate, int reg, char *comment)
{
  int ofd,oloc,ifd,chans,i,frames,cursamples,iloc,comlen,err=0;
  MUS_SAMPLE_TYPE **bufs;
  region *r;
  comlen = snd_strlen(comment);
  if (region_ok(reg)) r = regions[reg]; else r=NULL;
  if (r)
    {
      if ((snd_write_header(ss,ofile,type,srate,r->chans,28,r->chans*r->frames,format,comment,comlen,NULL)) == -1)
	return(MUS_HEADER_WRITE_FAILED);
      oloc = mus_header_data_location();
      if ((ofd = snd_reopen_write(ss,ofile)) == -1) 
	return(MUS_CANT_OPEN_TEMP_FILE);
      mus_file_set_descriptors(ofd,ofile,format,mus_data_format_to_bytes_per_sample(format),oloc,r->chans,type);
      mus_file_set_data_clipped(ofd,data_clipped(ss));
      mus_file_seek(ofd,oloc,SEEK_SET);
      if (r->use_temp_file == REGION_ARRAY)
	mus_file_write(ofd,0,r->frames-1,r->chans,r->data); /* was * r->chans --> mus_file_write wants per channel size */
      else
	{
	  /* copy r->filename with possible header/data format changes */
	  if ((ifd = snd_open_read(ss,r->filename)) == -1) 
	    {
	      snd_error("can't find region %d data file %s: %s",
			reg,r->filename,strerror(errno));
	      return(MUS_CANT_OPEN_TEMP_FILE);
	    }
	  chans = mus_sound_chans(r->filename);
	  frames = mus_sound_samples(r->filename) / chans;
	  iloc = mus_sound_data_location(r->filename);
	  mus_file_set_descriptors(ifd,
				   r->filename,
				   mus_sound_data_format(r->filename),
				   mus_sound_datum_size(r->filename),
				   iloc,
				   chans,
				   mus_sound_header_type(r->filename));
	  mus_file_seek(ifd,iloc,SEEK_SET);
	  bufs = (MUS_SAMPLE_TYPE **)CALLOC(chans,sizeof(MUS_SAMPLE_TYPE *));
	  for (i=0;i<chans;i++) bufs[i] = (MUS_SAMPLE_TYPE *)CALLOC(FILE_BUFFER_SIZE,sizeof(MUS_SAMPLE_TYPE));

	  /* TODO: check for disk space */

	  for (i=0;i<frames;i+=FILE_BUFFER_SIZE)
	    {
	      if ((i+FILE_BUFFER_SIZE)<frames) cursamples = FILE_BUFFER_SIZE; else cursamples = (frames-i);
	      mus_file_read(ifd,0,cursamples-1,chans,bufs);
	      err = mus_file_write(ofd,0,cursamples-1,chans,bufs);
	      if (err == -1) break;
	      check_for_event(ss); /* added 3-Jul-00 -- is this safe? */
	      if (ss->stopped_explicitly)
		{
		  ss->stopped_explicitly = 0;
		  snd_warning("save region %d stopped",reg);
		  break;
		}
	    }
	  mus_file_close(ifd);
	  for (i=0;i<chans;i++) FREE(bufs[i]);
	  FREE(bufs);
	}
      mus_file_close(ofd);
      alert_new_file();
    }
  return(MUS_NO_ERROR);
}

int save_region(snd_state *ss, int n, char *ofile, int data_format)
{
  region *r;
  r = regions[n];
  if (data_format == MUS_UNKNOWN) data_format = MUS_OUT_FORMAT;
  if (!(mus_header_writable(r->header_type,data_format))) 
    {
      if (mus_header_writable(MUS_NEXT,data_format))
	r->header_type = MUS_NEXT;
      else
	{
	  if (mus_header_writable(MUS_RIFF,data_format))
	    r->header_type = MUS_RIFF;
	  else r->header_type = MUS_RAW;
	}
    }
  if (r) return(save_region_1(ss,ofile,r->header_type,data_format,r->srate,n,"created by save-region in Snd"));
  return(0);
}

static int paste_region_1(int n, chan_info *cp, int add, int beg, char *origin)
{
  region *r;
  int i,j,err=MUS_NO_ERROR,id=-1,idtmp;
  snd_info *sp;
  sync_info *si;
  chan_info *ncp;
  MUS_SAMPLE_TYPE *data;
  snd_state *ss;
  char *tempfile = NULL;
  ss = cp->state;
  sp = cp->sound;
  si = NULL;
  if (region_ok(n)) r = regions[n]; else return(-1);
  si = sync_to_chan(cp);
  if (add)
    {
      if (r->use_temp_file == REGION_ARRAY)
	idtmp = mix_array(beg,r->frames,r->data,si->cps,r->chans,si->chans,SND_SRATE(sp),origin,with_mix_tags(ss));
      else idtmp = copy_file_and_mix(beg,r->frames,r->filename,si->cps,si->chans,origin,with_mix_tags(ss));
      if (id == -1) id = idtmp;
    }
  else
    {
      if (r->use_temp_file == REGION_FILE)
	{
	  tempfile = snd_tempnam(ss);
	  err = copy_file(r->filename,tempfile);
	  if (err != MUS_NO_ERROR)
	    snd_error("can't make region %d temp file (%s: %s)",n,tempfile,strerror(errno));
	  else
	    if (r->chans > 1) 
	      remember_temp(tempfile,r->chans);
	}
      for (i=0;((i<r->chans) && (i<si->chans));i++)
	{
	  ncp = si->cps[i]; /* currently syncd chan that we might paste to */
	  if (r->use_temp_file == REGION_ARRAY)
	    {
	      data = (MUS_SAMPLE_TYPE *)CALLOC(r->frames,sizeof(MUS_SAMPLE_TYPE));
	      for (j=0;j<r->frames;j++) 
		data[j] = r->data[i][j];
	      insert_samples(beg,r->frames,data,ncp,origin);
	      FREE(data);
	    }
	  else
	    {
	      if (err == MUS_NO_ERROR)
		file_insert_samples(beg,r->frames,tempfile,ncp,i,
				    (r->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
				    origin);
	    }
	  update_graph(si->cps[i],NULL);
	}
    }
  if (si) si = free_sync_info(si);
  return(id);
}

void paste_region(int n, chan_info *cp,char *origin) {paste_region_1(n,cp,FALSE,cp->cursor,origin);}
void add_region(int n, chan_info *cp, char *origin) {paste_region_1(n,cp,TRUE,cp->cursor,origin);}
static int mix_region(int n, chan_info *cp, int beg) {return(paste_region_1(n,cp,TRUE,beg,S_mix_region));}

void region_stats(int *vals)
{
  int i,fil=0,arr=0;
  region* r;
  for (i=0;i<regions_size;i++) 
    {
      r = regions[i];
      if (r)
	{
	  if (r->use_temp_file == REGION_FILE)
	    fil += (r->frames*r->chans*2);
	  else arr += (r->frames*r->chans*4);
	}
    }
  vals[0] = arr;
  vals[1] = fil;
}

void define_region(sync_info *si, int *ends)
{
  /* now look at all sync'd channels, collect them into the current region */
  /* we created the necessary pointers in create_selection above */
  int i,j,len,k,ofd = 0,datumb = 0,err=0;
  MUS_SAMPLE_TYPE val,mval,curval;
  chan_info *cp0;
  snd_info *sp0;
  region *r;
  snd_fd **sfs;
  snd_state *ss;
  file_info *hdr = NULL;
  len = 0;
  for (i=0;i<si->chans;i++)
    if (len < (ends[i] - si->begs[i]))
      len = ends[i] - si->begs[i];
  len += 1;
  if (len <= 0) return;
  r = (region *)CALLOC(1,sizeof(region));
  r->id = region_id_ctr++;
  cp0 = si->cps[0];
  sp0 = cp0->sound;
  ss = cp0->state;
  if (regions[0]) stack_region(ss,r); else regions[0] = r;
  r->header_type = (sp0->hdr)->type;
  r->srate = SND_SRATE(sp0);
  r->maxamp = 0.0;
  r->editor_copy = NULL;
  r->name = copy_string(sp0->shortname);
  r->chans = si->chans;
  r->data = (MUS_SAMPLE_TYPE **)CALLOC(r->chans,sizeof(MUS_SAMPLE_TYPE *));
  r->frames = len;
  val = MUS_SAMPLE_0; 
  mval = MUS_SAMPLE_0;
  r->start = prettyf((Float)si->begs[0]/(Float)(r->srate),2);
  r->end = prettyf((Float)ends[0]/(Float)(r->srate),2);
  sfs = (snd_fd **)CALLOC(r->chans,sizeof(snd_fd *));
  if (r->frames >= MAX_BUFFER_SIZE)
    {
      r->use_temp_file = REGION_FILE;
      r->filename = copy_string(snd_tempnam(ss));
      hdr = make_temp_header(r->filename,r->srate,r->chans,0);
      ofd = open_temp_file(r->filename,r->chans,hdr,ss);
      datumb = mus_data_format_to_bytes_per_sample(hdr->format);
    }
  else 
    {
      r->use_temp_file = REGION_ARRAY;
      r->filename = NULL;
    }
  for (i=0;i<r->chans;i++)
    {
      sfs[i]=init_sample_read(si->begs[i],si->cps[i],READ_FORWARD);
      if (r->use_temp_file == REGION_ARRAY)
	r->data[i] = (MUS_SAMPLE_TYPE *)CALLOC(len,sizeof(MUS_SAMPLE_TYPE));
      else r->data[i] = (MUS_SAMPLE_TYPE *)CALLOC(MAX_BUFFER_SIZE,sizeof(MUS_SAMPLE_TYPE));
    }
  for (j=0,k=0;j<len;j++,k++) 
    {
      if (k == MAX_BUFFER_SIZE)
	{
	  err = mus_file_write(ofd,0,k-1,r->chans,r->data);
	  k = 0;
	  if (err == -1) break;
	}
      for (i=0;i<r->chans;i++)
	{
	  if (j < ends[i]) 
	    {
	      curval = next_sample(sfs[i]);
	      r->data[i][k] = curval;
	      if (curval > val) val = curval;
	      if (curval < mval) mval = curval;
	    }
	  else r->data[i][k] = MUS_SAMPLE_0;
	}
    }
  if (r->use_temp_file == REGION_FILE)
    {
      if (k > 0) mus_file_write(ofd,0,k-1,r->chans,r->data);
      close_temp_file(ofd,hdr,len*r->chans*datumb,sp0);
      hdr = free_file_info(hdr);
      for (i=0;i<r->chans;i++) FREE(r->data[i]);
      FREE(r->data);
      r->data = NULL; /* filename only access in this case */
    }
  if (val < (-mval)) val=-mval;
  r->maxamp = MUS_SAMPLE_TO_FLOAT(val);
  for (i=0;i<r->chans;i++) free_snd_fd(sfs[i]);
  FREE(sfs);
  reflect_regions_in_menu();
  if (region_browser_is_active()) update_region_browser(ss,1);
}


snd_fd *init_region_read (snd_state *ss, int beg, int n, int chan, int direction)
{
  /* conjure up a reasonable looking ed list and sound list */
  region *r;
  snd_info *rsp;
  if (region_ok(n))
    {
      r = regions[n];
      make_region_readable(r,ss);
      if ((r) && (chan < r->chans))
	{
	  rsp = r->rsp;
	  if ((beg == 0) && (direction == READ_BACKWARD)) beg=r->frames-1;
	  return(init_sample_read(beg,rsp->chans[chan],direction));
	}
    }
  return(NULL);
}

void cleanup_region_temp_files(void)
{ /* called upon exit to get rid of lingering region-related temp files */
  int i;
  region *r;
  for (i=0;i<regions_size;i++)
    {
      r = regions[i];
      if ((r) && (r->use_temp_file == REGION_FILE) && (r->filename))
	{
	  mus_sound_forget(r->filename);
	  remove(r->filename);
	  r->filename = NULL;
	}
    }
}

int snd_regions(void)
{
  int i,num;
  num = 0;
  for (i=0;i<regions_size;i++) if (regions[i]) num++;
  return(num);
}

/* (restore-region n chans len srate maxamp name start end #(chan-1 int-data ...)) */

void save_regions(snd_state *ss, FILE *fd)
{
  int i,j,k;
  region *r;
  char *newname;
  for (i=0;i<regions_size;i++)
    {
      r = regions[i];
      if (r)
	{
	  fprintf(fd,"(%s %d %d %d %d %.4f \"%s\" \"%s\" \"%s\"",
	          S_restore_region,i,r->chans,r->frames,r->srate,r->maxamp,r->name,r->start,r->end);
	  if (r->use_temp_file == REGION_ARRAY)
	    {
	      fprintf(fd,"\n  #(");
	      for (j=0;j<r->chans;j++)
		{
		  for (k=0;k<r->frames;k++)
#if SNDLIB_USE_FLOATS
		    fprintf(fd,"%f ",r->data[j][k]);
#else
		    fprintf(fd,"%d ",r->data[j][k]);
#endif
		}
	      fprintf(fd,")");
	    }
	  else /* file data */
	    {
	      if (save_dir(ss))
		{
		  newname = shorter_tempnam(save_dir(ss),"snd_save_");
		  copy_file(r->filename,newname);
		  fprintf(fd," \"%s\"",newname);
		  FREE(newname);
		}
	      else
		{
		  /* read at very low level */
		  int ifd,iloc;
		  MUS_SAMPLE_TYPE **ibufs;
		  ifd = mus_file_open_read(r->filename);
		  iloc = mus_sound_data_location(r->filename);
		  mus_file_set_descriptors(ifd,r->filename,
					   mus_sound_data_format(r->filename),mus_sound_datum_size(r->filename),iloc,
					   r->chans,mus_sound_header_type(r->filename));
		  mus_file_seek(ifd,iloc,SEEK_SET);
		  ibufs = (MUS_SAMPLE_TYPE **)CALLOC(r->chans,sizeof(MUS_SAMPLE_TYPE *));
		  for (j=0;j<r->chans;j++)
		    ibufs[j] = (MUS_SAMPLE_TYPE *)CALLOC(r->frames,sizeof(MUS_SAMPLE_TYPE));
		  mus_file_read(ifd,0,r->frames-1,r->chans,ibufs);
		  fprintf(fd,"\n  #(");
		  for (j=0;j<r->chans;j++)
		    {
		      for (k=0;k<r->frames;k++) 
#if SNDLIB_USE_FLOATS
			fprintf(fd,"%f ",ibufs[j][k]);
#else
			fprintf(fd,"%d ",ibufs[j][k]);
#endif
		    }
		  fprintf(fd,")");
		  mus_file_close(ifd);
		  for (j=0;j<r->chans;j++) FREE(ibufs[j]);
		  FREE(ibufs);
		}
	    }
	  fprintf(fd,")\n");
	}
    }
}

void region_edit(snd_state *ss, int reg)
{
  /* from region browser:
   *   load region into temp file, load that into snd editor,
   *   if 'save', save temp file and update region (browser also) (cancelling active display if any)
   *   while editing, if delete in browser, cut backpointer in editor and signal it
   */
  char *temp_region_name;
  snd_info *sp;
  int err;
  region *r;
  if (region_ok(reg)) 
    {
      r = regions[reg];
      if (r->editor_copy)
	snd_error("region %d already being edited",reg);
      else
	{
	  temp_region_name = shorter_tempnam(temp_dir(ss),"region-");
	  if (r->use_temp_file == REGION_FILE)
	    err = copy_file(r->filename,temp_region_name);
	  else err = save_region(ss, reg, temp_region_name, MUS_OUT_FORMAT);
	  if (err == MUS_NO_ERROR)
	    {
	      sp = snd_open_file(temp_region_name,ss);
	      if (sp)
		{
		  r->editor_copy = sp;
		  r->editor_name = copy_string(temp_region_name);
		  sp->edited_region = r;
		  /* save backpointer so subsequent save affects region if still legit */
		  /* also, since it's a temp file, if closed, delete temp */
		}
	      else snd_error("edit region: can't open region %d temp sound %s: %s!",
			     reg,temp_region_name,strerror(errno));
	    }
	  else 
	    snd_error("edit region: can't save region %d in temp file (%s: %s)",
		      reg,temp_region_name,strerror(errno));
	  FREE(temp_region_name);
	}
    }
  else snd_error("edit region: no region %d!",reg);
}

void clear_region_backpointer(snd_info *sp)
{
  region *r;
  if (sp->edited_region)
    {
      r = (region *)(sp->edited_region);
      if (r)
	{
	  mus_sound_forget(r->editor_name);
	  remove(r->editor_name);
	  FREE(r->editor_name);
	  r->editor_name = NULL;
	  r->editor_copy = NULL;
	}
      sp->edited_region = NULL;
    }
}

void save_region_backpointer(snd_info *sp)
{
  /* region being edited, user chose 'save' */
  region *r;
  int i,err;
  Float val;
  snd_state *ss;
  if (sp->edited_region)
    {
      r = (region *)(sp->edited_region);
      ss = sp->state;
      if (r)
	{
	  /* update r's data either in array or file, deleting old, redisplay if browser active etc */
	  if (r == regions[0]) deactivate_selection();
	  free_region(r,CLEAR_REGION_DATA);
	  r->use_temp_file = REGION_FILE;
	  r->maxamp = 0.0;
	  r->frames = current_ed_samples(sp->chans[0]);
	  for (i=0;i<sp->nchans;i++)
	    {
	      val = get_maxamp(sp,sp->chans[i]);
	      if (val > r->maxamp) r->maxamp = val;
	    }
	  /* make new region temp file */
	  r->filename = snd_tempnam(ss);
	  err = copy_file(r->editor_name,r->filename);
	  if (err != MUS_NO_ERROR)
	    snd_error("can't make region temp file (%s: %s)",r->filename,strerror(errno));
	  make_region_readable(r,ss);
	  if (region_browser_is_active()) update_region_browser(ss,1);
	}
    }
}

#if HAVE_GUILE

static char *g_scm2newstr(SCM str)
{
  char *temp,*res;
  temp = gh_scm2newstr(str,NULL);
  res = copy_string(temp);
  free(temp);
  return(res);
}

static SCM g_restore_region(SCM n, SCM chans, SCM len, SCM srate, SCM maxamp, SCM name, SCM start, SCM end, SCM data)
{
  region *r;
  int i,j,k,regn;
  r = (region *)CALLOC(1,sizeof(region));
  regn = gh_scm2int(n);
  regions[regn] = r;
  r->id = region_id_ctr++;
  r->maxamp = gh_scm2double(maxamp);
  r->chans = gh_scm2int(chans);
  r->rsp = NULL;
  r->editor_copy = NULL;
  r->editor_name = NULL;
  r->frames = gh_scm2int(len);
  r->srate = gh_scm2int(srate);
  r->name = g_scm2newstr(name);
  r->start = g_scm2newstr(start);
  r->end = g_scm2newstr(end);
  if (gh_string_p(data))
    {
      r->use_temp_file = REGION_FILE;
      r->filename = g_scm2newstr(data);
    }
  else 
    {
      r->use_temp_file = REGION_ARRAY;
      r->filename = NULL;
      r->data = (MUS_SAMPLE_TYPE **)CALLOC(r->chans,sizeof(MUS_SAMPLE_TYPE *));
      k=0; 
      for (i=0;i<r->chans;i++)
	{
	  r->data[i] = (MUS_SAMPLE_TYPE *)CALLOC(r->frames,sizeof(MUS_SAMPLE_TYPE));
	  for (j=0;j<r->frames;j++,k++)
#if SNDLIB_USE_FLOATS
	    r->data[i][j] = gh_scm2double(scm_vector_ref(data,gh_int2scm(k)));
#else
	    r->data[i][j] = gh_scm2int(scm_vector_ref(data,gh_int2scm(k)));
#endif
	}
    }
  reflect_regions_in_menu();
  RTNINT(region_id(regn));
}

static SCM g_insert_region(SCM samp_n, SCM reg_n, SCM snd_n, SCM chn_n) /* opt reg_n */
{
  #define H_insert_region "("  S_insert_region " &optional (start-samp 0) (region 0) snd chn) inserts region data\n\
   into snd's channel chn starting at 'start-samp'"

  chan_info *cp;
  int rg,samp;
  ERRB1(samp_n,S_insert_region);
  ERRB2(reg_n,S_insert_region);
  ERRCP(S_insert_region,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n,S_insert_region);
  rg = g_scm2intdef(reg_n,0);
  if (!(region_ok(rg))) return(scm_throw(NO_SUCH_REGION,SCM_LIST2(gh_str02scm(S_insert_region),reg_n)));
  samp = g_scm2intdef(samp_n,0);
  paste_region_1(rg,cp,FALSE,samp,S_insert_region);
  update_graph(cp,NULL);
  RTNINT(region_id(rg));
}

static SCM g_max_regions(void) 
{
  #define H_max_regions "(" S_max_regions ") -> max number of regions saved on the region list"
  snd_state *ss;
  ss = get_global_state();
  RTNINT(max_regions(ss));
}

static SCM g_set_max_regions(SCM n) 
{
  snd_state *ss;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(n)),n,SCM_ARG1,"set-" S_max_regions); 
  ss = get_global_state();
  set_max_regions(ss,g_scm2int(n));
  RTNINT(max_regions(ss));
}

enum {REGION_LENGTH,REGION_SRATE,REGION_CHANS,REGION_MAXAMP,REGION_SELECT,REGION_DELETE,REGION_PLAY,REGION_ID};

static SCM region_read(int field, SCM n, char *caller)
{
  int rg;
  int i;
  SCM res = SCM_EOL;
  if (SCM_EQ_P(n,SCM_BOOL_T))
    {
      for (i=0;i<regions_size;i++)
	if (regions[i])
	  res = gh_cons(region_read(field,gh_int2scm(i),caller),res);
      return(scm_reverse(res));
    }
  else
    {
      rg = g_scm2intdef(n,0);
      if (region_ok(rg))
	{
	  switch (field)
	    {
	    case REGION_LENGTH: RTNINT(region_len(rg)); break;
	    case REGION_SRATE:  RTNINT(region_srate(rg)); break;
	    case REGION_CHANS:  RTNINT(region_chans(rg)); break;
	    case REGION_MAXAMP: RTNFLT(region_maxamp(rg)); break;
	    case REGION_SELECT: select_region_and_update_browser(get_global_state(),rg); return(n); break;
	    case REGION_DELETE: delete_region_and_update_browser(get_global_state(),rg); return(n); break;
	    case REGION_ID:     RTNINT(region_id(rg)); break;
	    }
	}
      else return(scm_throw(NO_SUCH_REGION,SCM_LIST2(gh_str02scm(caller),n)));
    }
  RTNINT(0);
}

static SCM g_regionQ(SCM n)
{
  #define H_regionQ "(" S_regionQ " reg) -> #t if region is active"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(n)),n,SCM_ARG1,S_regionQ);
  RTNBOOL(region_ok(g_scm2int(n)));
}

static SCM g_region_length (SCM n) 
{
  #define H_region_length "(" S_region_length " &optional (n 0)) -> length in frames of region"
  ERRB1(n,S_region_length); 
  return(region_read(REGION_LENGTH,n,S_region_length));
}

static SCM g_region_srate (SCM n) 
{
  #define H_region_srate "(" S_region_srate " &optional (n 0)) -> srate of region n"
  ERRB1(n,S_region_srate); 
  return(region_read(REGION_SRATE,n,S_region_srate));
}

static SCM g_region_chans (SCM n) 
{
  #define H_region_chans "(" S_region_chans " &optional (n 0) -> channels of data in region n"
  ERRB1(n,S_region_chans); 
  return(region_read(REGION_CHANS,n,S_region_chans));
}

static SCM g_region_id (SCM n) 
{
  #define H_region_id "(" S_region_id " &optional (n 0) -> unique id of region n"
  ERRB1(n,S_region_id); 
  return(region_read(REGION_ID,n,S_region_id));
}

static SCM g_id_region (SCM n) 
{
  #define H_id_region "(" S_id_region " &optional (id 0) -> stack location of region with id"
  int sn;
  ERRB1(n,S_id_region); 
  sn = id_region(g_scm2intdef(n,0));
  if (sn == -1) return(scm_throw(NO_SUCH_REGION,SCM_LIST2(gh_str02scm(S_id_region),n)));
  RTNINT(sn);
}

static SCM g_region_maxamp (SCM n) 
{
  #define H_region_maxamp "(" S_region_maxamp " &optional (n 0)) -> max amp of region n"
  ERRB1(n,S_region_maxamp); 
  return(region_read(REGION_MAXAMP,n,S_region_maxamp));
}

static SCM g_select_region (SCM n) 
{
  #define H_select_region "(" S_select_region " &optional (n 0)) selects region n (moves it to the top of the region list)"
  ERRB1(n,S_select_region); 
  return(region_read(REGION_SELECT,n,S_select_region));
}

static SCM g_delete_region (SCM n) 
{
  #define H_delete_region "(" S_delete_region " &optional (n 0)) remove region n from the region list"
  ERRB1(n,S_delete_region); 
  return(region_read(REGION_DELETE,n,S_delete_region));
}

static SCM g_play_region (SCM n, SCM wait) 
{
  #define H_play_region "(" S_play_region " &optional (n 0) (wait #f)) play region n, if wait is #t, play to end before returning"
  int rg,wt=0;
  ERRB1(n,S_play_region); 
  ERRB2(wait,S_play_region);
  rg = g_scm2intdef(n,0);
  if (SCM_TRUE_P(wait)) wt = 1; else wt = g_scm2intdef(n,0);
  if (region_ok(rg))
    play_region(get_global_state(),rg,!wt);
  else return(scm_throw(NO_SUCH_REGION,SCM_LIST2(gh_str02scm(S_play_region),n)));
  return(n);
}

static SCM g_protect_region (SCM n, SCM protect) 
{
  #define H_protect_region "(" S_protect_region " &optional (n 0) (val #t)) if val is #t protects region n from being\n\
   pushed off the end of the region list"

  int rg;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(n)),n,SCM_ARG1,S_protect_region);
  ERRB2(protect,S_protect_region);
  rg = g_scm2intdef(n,0);
  if (region_ok(rg))
    set_region_protect(rg,bool_int_or_one(protect)); 
  else return(scm_throw(NO_SUCH_REGION,SCM_LIST2(gh_str02scm(S_protect_region),n)));
  return(protect);
}

static SCM g_regions(void) 
{
  #define H_regions "(" S_regions ") -> list of ids of regions currently in the region list"
  int i;
  SCM result;
  result = SCM_EOL;
  for (i=(regions_size-1);i>=0;i--)
    if (regions[i])
      result = gh_cons(gh_int2scm(regions[i]->id),result);
  return(result);
}

static SCM g_make_region (SCM beg, SCM end, SCM snd_n, SCM chn_n)
{
  #define H_make_region "(" S_make_region " beg end &optional snd chn) makes a new region between beg and end in snd"
  chan_info *cp;
  sync_info *si;
  int ends[1];
  int ibeg;
  if (SCM_UNBNDP(beg))
    make_region_from_selection();
  else
    {
      SCM_ASSERT(SCM_NFALSEP(scm_real_p(beg)),beg,SCM_ARG1,S_make_region);
      SCM_ASSERT(SCM_NFALSEP(scm_real_p(end)),end,SCM_ARG2,S_make_region);
      cp = get_cp(snd_n,chn_n,S_make_region);
      ibeg = g_scm2intdef(beg,0);
      ends[0] = g_scm2intdef(end,0);
      if (current_ed_samples(cp)-1 < ends[0]) ends[0] = current_ed_samples(cp)-1;
      if (ends[0] < ibeg) return(scm_throw(IMPOSSIBLE_BOUNDS,SCM_LIST5(gh_str02scm(S_make_region),beg,end,snd_n,chn_n)));
      si = make_simple_sync(cp,ibeg);
      define_region(si,ends);
      reactivate_selection(si->cps[0],si->begs[0],ends[0]); /* ??? */
      si = free_sync_info(si);
    }
  RTNINT(region_id(0));
}

static SCM g_save_region (SCM n, SCM filename, SCM format) 
{
  #define H_save_region "(" S_save_region " region filename &optional format) saves region in filename using data format (mus-bshort)"
  char *name = NULL,*urn;
  int res=MUS_NO_ERROR,rg;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(n)),n,SCM_ARG1,S_save_region);
  SCM_ASSERT(gh_string_p(filename),filename,SCM_ARG2,S_save_region);
  ERRB3(format,S_save_region);
  rg = g_scm2int(n);
  if (region_ok(rg))
    {
      urn = gh_scm2newstr(filename,NULL);
      name = mus_file_full_name(urn);
      free(urn);
      res = save_region(get_global_state(),rg,name,g_scm2intdef(format,0));
      if (name) FREE(name);
    }
  else return(scm_throw(NO_SUCH_REGION,SCM_LIST2(gh_str02scm(S_save_region),n)));
  if (res != MUS_NO_ERROR)
    return(SCM_BOOL_F);
  return(scm_throw(CANNOT_SAVE,SCM_LIST1(gh_str02scm(S_save_region))));
}

static SCM g_mix_region(SCM chn_samp_n, SCM reg_n, SCM snd_n, SCM chn_n)
{
  #define H_mix_region "(" S_mix_region " &optional (chn-samp 0) (region 0) snd chn) mixer region\n\
   into snd's channel chn starting at chn-samp; returns new mix id."

  chan_info *cp;
  int rg,id=-1;
  ERRB1(chn_samp_n,S_mix_region);
  ERRB2(reg_n,S_mix_region);
  ERRCP(S_mix_region,snd_n,chn_n,3);
  rg = g_scm2intdef(reg_n,0);
  if (region_ok(rg))
    {
      cp = get_cp(snd_n,chn_n,S_mix_region);
      id = mix_region(rg,cp,
		      g_scm2intdef(chn_samp_n,cp->cursor));
    }
  else return(scm_throw(NO_SUCH_REGION,SCM_LIST2(gh_str02scm(S_mix_region),reg_n)));
  return(gh_int2scm(id));
}

static SCM g_region_sample(SCM samp_n, SCM reg_n, SCM chn_n)
{
  #define H_region_sample "(" S_region_sample " &optional (samp 0) (region 0) (chan 0)) -> region's sample at samp in chan"

  int rg,chan;
  ERRB1(samp_n,S_region_sample);
  ERRB2(reg_n,S_region_sample);
  ERRB3(chn_n,S_region_sample);
  rg = g_scm2intdef(reg_n,0);
  chan = g_scm2intdef(chn_n,0);
  if (region_ok(rg))
    {
      if (chan < region_chans(rg))
	RTNFLT(region_sample(rg,chan,g_scm2intdef(samp_n,0)));
      else return(scm_throw(NO_SUCH_CHANNEL,SCM_LIST3(gh_str02scm(S_region_sample),SCM_LIST1(reg_n),chn_n)));
    }
  else return(scm_throw(NO_SUCH_REGION,SCM_LIST2(gh_str02scm(S_region_sample),reg_n)));
}

static SCM g_region_samples(SCM beg_n, SCM num, SCM reg_n, SCM chn_n)
{
  #define H_region_samples "(" S_region_samples " &optional (beg 0) samps (region 0) (chan 0)) returns a vector with\n\
   region's samples starting at samp for samps from channel chan"

  SCM new_vect;
  Float *data;
  int len,reg,i,chn;
  ERRB1(beg_n,S_region_samples);
  ERRB2(num,S_region_samples);
  ERRB3(reg_n,S_region_samples);
  ERRB4(chn_n,S_region_samples);
  reg = g_scm2intdef(reg_n,0);
  if (!(region_ok(reg))) return(scm_throw(NO_SUCH_REGION,SCM_LIST2(gh_str02scm(S_region_samples),reg_n)));
  chn = g_scm2intdef(chn_n,0);
  if (chn < region_chans(reg))
    {
      len = g_scm2intdef(num,0);
      if (len == 0) len = region_len(reg);
      if (len > 0)
	{
	  new_vect = gh_make_vector(gh_int2scm(len),gh_double2scm(0.0));
	  data = (Float *)CALLOC(len,sizeof(Float));
	  region_samples(reg,chn,g_scm2intdef(beg_n,0),len,data);
	  for (i=0;i<len;i++) gh_vector_set_x(new_vect,gh_int2scm(i),gh_double2scm(data[i]));
	  FREE(data);
	  return(new_vect);
	}
    }
  else return(scm_throw(NO_SUCH_CHANNEL,SCM_LIST3(gh_str02scm(S_region_samples),SCM_LIST1(reg_n),chn_n)));
  return(SCM_BOOL_F);
}

#include "vct.h"

static SCM g_region_samples2vct(SCM beg_n, SCM num, SCM reg_n, SCM chn_n, SCM v)
{
  #define H_region_samples2vct "(" S_region_samples_vct " &optional (beg 0) samps (region 0) (chan 0) obj) writes\n\
   region's samples starting at beg for samps in channel chan to vct obj, returning obj (or creating a new one)"

  Float *data;
  int len,reg,chn;
  vct *v1 = get_vct(v);
  ERRB1(beg_n,S_region_samples_vct);
  ERRB2(num,S_region_samples_vct);
  ERRB3(reg_n,S_region_samples_vct);
  ERRB4(chn_n,S_region_samples_vct);
  reg = g_scm2intdef(reg_n,0);
  if (!(region_ok(reg))) return(scm_throw(NO_SUCH_REGION,SCM_LIST2(gh_str02scm(S_region_samples_vct),reg_n)));
  chn = g_scm2intdef(chn_n,0);
  if (chn >= region_chans(reg)) return(scm_throw(NO_SUCH_CHANNEL,SCM_LIST3(gh_str02scm(S_region_samples_vct),SCM_LIST1(reg_n),chn_n)));
  len = g_scm2intdef(num,0);
  if (len == 0) len = region_len(reg);
  if (len > 0)
    {
      if (v1)
	data = v1->data;
      else data = (Float *)CALLOC(len,sizeof(Float));
      region_samples(reg,chn,g_scm2intdef(beg_n,0),len,data);
      if (v1)
	return(v);
      else return(make_vct(len,data));
    }
  return(SCM_BOOL_F);
}


void g_init_regions(SCM local_doc)
{
  DEFINE_PROC(gh_new_procedure(S_restore_region,SCM_FNC g_restore_region,9,0,0),"restores a region");
  DEFINE_PROC(gh_new_procedure(S_insert_region,SCM_FNC g_insert_region,0,4,0),H_insert_region);
  DEFINE_PROC(gh_new_procedure(S_regions,SCM_FNC g_regions,0,0,0),H_regions);
  DEFINE_PROC(gh_new_procedure(S_region_length,SCM_FNC g_region_length,0,1,0),H_region_length);
  DEFINE_PROC(gh_new_procedure(S_region_srate,SCM_FNC g_region_srate,0,1,0),H_region_srate);
  DEFINE_PROC(gh_new_procedure(S_region_chans,SCM_FNC g_region_chans,0,1,0),H_region_chans);
  DEFINE_PROC(gh_new_procedure(S_region_id,SCM_FNC g_region_id,0,1,0),H_region_id);
  DEFINE_PROC(gh_new_procedure(S_id_region,SCM_FNC g_id_region,0,1,0),H_id_region);
  DEFINE_PROC(gh_new_procedure(S_region_maxamp,SCM_FNC g_region_maxamp,0,1,0),H_region_maxamp);
  DEFINE_PROC(gh_new_procedure(S_save_region,SCM_FNC g_save_region,2,1,0),H_save_region);
  DEFINE_PROC(gh_new_procedure(S_select_region,SCM_FNC g_select_region,0,1,0),H_select_region);
  DEFINE_PROC(gh_new_procedure(S_delete_region,SCM_FNC g_delete_region,0,1,0),H_delete_region);
  DEFINE_PROC(gh_new_procedure(S_protect_region,SCM_FNC g_protect_region,2,0,0),H_protect_region);
  DEFINE_PROC(gh_new_procedure(S_play_region,SCM_FNC g_play_region,0,2,0),H_play_region);
  DEFINE_PROC(gh_new_procedure(S_make_region,SCM_FNC g_make_region,0,4,0),H_make_region);
  DEFINE_PROC(gh_new_procedure(S_mix_region,SCM_FNC g_mix_region,0,4,0),H_mix_region);
  DEFINE_PROC(gh_new_procedure(S_region_sample,SCM_FNC g_region_sample,0,3,0),H_region_sample);
  DEFINE_PROC(gh_new_procedure(S_region_samples,SCM_FNC g_region_samples,0,4,0),H_region_samples);
  DEFINE_PROC(gh_new_procedure(S_region_samples_vct,SCM_FNC g_region_samples2vct,0,5,0),H_region_samples2vct);
  DEFINE_PROC(gh_new_procedure(S_regionQ,SCM_FNC g_regionQ,1,0,0),H_regionQ);

  define_procedure_with_setter(S_max_regions,SCM_FNC g_max_regions,H_max_regions,
			       "set-" S_max_regions,SCM_FNC g_set_max_regions,
			       local_doc,0,0,1,0);
}

#endif
