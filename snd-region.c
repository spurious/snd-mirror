#include "snd.h" 

/* snd selections/regions */

typedef struct { /* one for each 'channel' */
  int visible;
  int type;
  snd_info *sp;
  chan_info *cp;
  int first,last;
  int x0,y0,width,height;
  int *fixups;
  int fixup_size;
} region_context;  

static int samp0(region_context *rg)
{
  chan_info *cp;
  if (!rg->fixups) return(rg->first);
  cp = rg->cp;
  if (cp->edit_ctr < 0) return(rg->first);
  return(rg->first+rg->fixups[cp->edit_ctr]);
}

static int samp1(region_context *rg)
{
  chan_info *cp;
  if (!rg->fixups) return(rg->last);
  cp = rg->cp;
  if (cp->edit_ctr < 0) return(rg->last);
  return(rg->last+rg->fixups[cp->edit_ctr]);
}

#define REGION_ARRAY 0
#define REGION_FILE 1
/* region data can be stored either in-core (if less than MAX_BUFFER_SIZE ints), else in a temp file that */
/*    is deleted when the region is deleted (hence must be copied upon insert or mix) */

static int region_id_ctr = 0;

typedef struct {
  MUS_SAMPLE_TYPE **data;
  int chans;
  int len;
  int beg;
  int srate;        /* for file save (i.e. region->file) */
  int header_type;         /* for file save */
  int save;
  snd_info *rsp;
  region_context **rgx;
  char *name,*start,*end;
  char *filename;  /* if region data is stored in a temp file */
  int use_temp_file;       /* REGION_ARRAY = data is in 'data' arrays, else in temp file 'filename' */
  Float maxamp;
  snd_info *editor_copy;
  char *editor_name;
  int id;
} region;

static void free_region_contexts(region *r)
{
  int i;
  region_context *rg;
  for (i=0;i<r->chans;i++)
    {
      if ((rg = r->rgx[i]))
	{
	  if (rg->fixups) FREE(rg->fixups);
	  FREE(rg);
	}
    }
  FREE(r->rgx);
  r->rgx = NULL;
}

#define CLEAR_REGION_DATA 0
#define COMPLETE_DELETION 1

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
      if (r->rgx) free_region_contexts(r);
      if (r->data)  /* null if temp file */
	{
	  for (i=0;i<r->chans;i++) 
	    {
	      if (r->data[i]) FREE(r->data[i]);
	    }
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
int region_len(int n) {if (region_ok(n)) return(regions[n]->len); else return(0);}
int region_chans(int n) {if (region_ok(n)) return(regions[n]->chans); else return(0);}
int region_srate(int n) {if (region_ok(n)) return(regions[n]->srate); else return(0);}
Float region_maxamp(int n) {if (region_ok(n)) return(regions[n]->maxamp); else return(0.0);}

static int region_id(int n) {if (region_ok(n)) return(regions[n]->id); else return(-1);}
static int id_region(int id)
{
  int i;
  for (i=0;i<regions_size;i++)
    if ((regions[i]) && (regions[i]->id == id))
      return(i);
  return(-1);
}

int selection_is_current(void) 
{
  region *r;
  r = regions[0];
  return((r) && (r->rgx));
}

static Float region_sample(int reg, int chn, int samp)
{
  region *r;
  snd_fd *sf;
  MUS_SAMPLE_TYPE val;
  if (region_ok(reg))
    {
      r = regions[reg];
      if ((samp < r->len) && (chn < r->chans)) 
	{
	  if (r->use_temp_file == REGION_ARRAY)
	    return(MUS_SAMPLE_TO_FLOAT(r->data[chn][samp]));
	  else 
	    {
	      sf = init_region_read(get_global_state(),samp,reg,chn,READ_FORWARD);
	      NEXT_SAMPLE(val,sf);
	      free_snd_fd(sf);
	      return(MUS_SAMPLE_TO_FLOAT(val));
	    }
	}
    }
  return(0.0);
}

static void region_samples(int reg, int chn, int beg, int num, Float *data)
{
  region *r;
  snd_fd *sf;
  MUS_SAMPLE_TYPE val;
  int i,j;
  if (region_ok(reg))
    {
      r = regions[reg];
      if ((beg < r->len) && (chn < r->chans))
	{
	  if (r->use_temp_file == REGION_ARRAY)
	    {
	      for (i=beg,j=0;(i<r->len) && (j<num);i++,j++) data[j] = MUS_SAMPLE_TO_FLOAT(r->data[chn][i]);
	    }
	  else
	    {
	      sf = init_region_read(get_global_state(),beg,reg,chn,READ_FORWARD);
	      for (i=beg,j=0;(i<r->len) && (j<num);i++,j++) 
		{
		  NEXT_SAMPLE(val,sf);		  
		  data[j] = MUS_SAMPLE_TO_FLOAT(val);
		}
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
    {
      if (regions[i]) return(i);
    }
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
  regsp->s_type = SND_INFO;
  regsp->nchans = r->chans;
  regsp->allocated_chans = r->chans; /* needed for complete GC */
  regsp->chans = (chan_info **)CALLOC(r->chans,sizeof(chan_info *));
  regsp->hdr = (file_info *)CALLOC(1,sizeof(file_info));
  hdr = regsp->hdr;
  hdr->samples = r->len * r->chans;
  hdr->srate = r->srate;
  hdr->chans = r->chans;
  hdr->comment = NULL;
  for (i=0;i<r->chans;i++)
    {
      cp = make_chan_info(NULL,i,regsp,ss);
      regsp->chans[i] = cp;
      add_channel_data_1(cp,regsp,ss,0);
      set_initial_ed_list(cp,r->len-1);
      cp->edit_size = 1;
      cp->sound_size = 1;
      cp->hookable = 0;
      if (r->use_temp_file == REGION_ARRAY)
	cp->sounds[0] = make_snd_data_buffer(r->data[i],r->len,cp->edit_ctr);
      else
	{
	  hdr = make_file_info(r->filename,ss);
	  if (hdr)
	    {
	      fd = snd_open_read(ss,r->filename);
	      mus_file_open_descriptors(fd,hdr->format,mus_data_format_to_bytes_per_sample(hdr->format),hdr->data_location);
	      datai = make_file_state(fd,hdr,SND_IO_IN_FILE,i,FILE_BUFFER_SIZE);
	      cp->sounds[0] = make_snd_data_file(r->filename,datai,
						 MUS_SAMPLE_ARRAY(datai[SND_IO_DATS+SND_AREF_HEADER_SIZE+i]),
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
  finish_keyboard_selection();
  rs = (region_state *)CALLOC(1,sizeof(region_state));
  len = regions_size;
  for (i=0;i<regions_size;i++) {if (!(regions[i])) {len = i; break;}}
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
	{
	  if (r->name[i]) FREE(r->name[i]);
	}
      if (r->name) FREE(r->name);
      if (r->save) FREE(r->save);
      FREE(r);
    }
}

void select_region(int n)
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

int delete_region(int n)
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

void protect_region(int n,int protect)
{
  region *r;
  if (region_ok(n))
    {
      r = regions[n];
      if (r) r->save = protect;
    }
}

snd_info *region_sound(int n) 
{
  region *r; 
  region_context **rgx;
  if (region_ok(n))
    {
      r = regions[n]; 
      rgx = r->rgx;
      if ((rgx) && (rgx[0])) return(rgx[0]->sp); else return(NULL);
    }
  return(NULL);
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

static region_context *region_member(chan_info *cp, region* r)
{
  int i;
  region_context **rg;
  if ((r) && (r->rgx))
    {
      rg = r->rgx;
      for (i=0;i<r->chans;i++)
	{
	  if ((rg[i]) && (rg[i]->cp == cp)) return(rg[i]);
	}
    }
  return(NULL);
}

int selection_is_current_in_channel(chan_info *cp)
{
  return(region_member(cp,regions[0]) != NULL);
}

int selection_member(snd_info *sp)
{
  region *r;
  region_context **rg;
  int i;
  r = regions[0];
  if ((r) && (r->rgx))
    {
      rg = r->rgx;
      for (i=0;i<r->chans;i++)
	{
	  if ((rg[i]) && (rg[i]->sp == sp)) return(1);
	}
    }
  return(0);
}

int active_selection (chan_info *cp)
{
  region_context *rg;
  return((regions[0]) &&
	 (rg=(region_member(cp,regions[0]))) && 
	 (rg->visible));
}

int selection_beg(chan_info *cp) 
{
  region *r;
  region_context *rg;
  region_context **rgx;
  int i,fallback_beg = 0;
  if ((r = regions[0]))
    {
      fallback_beg = r->beg;
      rgx = r->rgx;
      if (rgx)
	{
	  if (cp == NULL) return(samp0(rgx[0]));
	  for (i=0;i<r->chans;i++)
	    {
	      rg = rgx[i];
	      if (rg)
		{
		  if (cp == rg->cp)
		    return(samp0(rg));
		  else fallback_beg = rg->first;
		}
	    }
	}
    }
  return(fallback_beg); /* was 0 which seems useless */
}
 
void selection_off(chan_info *cp)
{
  region *r;
  region_context *rg;
  if ((r = regions[0]) && (rg = region_member(cp,r))) rg->visible = 0;
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
      if ((snd_write_header(ss,ofile,type,srate,r->chans,28,r->chans*r->len,format,comment,comlen,NULL)) == -1) return(SND_CANNOT_WRITE_HEADER);
      oloc = mus_header_data_location();
      if ((ofd = snd_reopen_write(ss,ofile)) == -1) return(SND_CANNOT_OPEN_TEMP_FILE);
      mus_file_set_descriptors(ofd,ofile,format,mus_data_format_to_bytes_per_sample(format),oloc,r->chans,type);
      mus_file_set_data_clipped(ofd,data_clipped(ss));
      mus_file_seek(ofd,oloc,SEEK_SET);
      if (r->use_temp_file == REGION_ARRAY)
	{
	  mus_file_write(ofd,0,r->len-1,r->chans,r->data); /* was * r->chans --> mus_file_write wants per channel size */
	}
      else
	{
	  /* copy r->filename with possible header/data format changes */
	  if ((ifd = snd_open_read(ss,r->filename)) == -1) return(SND_CANNOT_OPEN_TEMP_FILE);
	  chans = mus_sound_chans(r->filename);
	  frames = mus_sound_samples(r->filename) / chans;
	  iloc = mus_sound_data_location(r->filename);
	  mus_file_set_descriptors(ifd,r->filename,
				   mus_sound_data_format(r->filename),mus_sound_datum_size(r->filename),iloc,
				   chans,mus_sound_header_type(r->filename));
	  mus_file_seek(ifd,iloc,SEEK_SET);
	  bufs = (MUS_SAMPLE_TYPE **)CALLOC(chans,sizeof(MUS_SAMPLE_TYPE *));
	  for (i=0;i<chans;i++) bufs[i] = (MUS_SAMPLE_TYPE *)CALLOC(FILE_BUFFER_SIZE,sizeof(MUS_SAMPLE_TYPE));
	  for (i=0;i<frames;i+=FILE_BUFFER_SIZE)
	    {
	      if ((i+FILE_BUFFER_SIZE)<frames) cursamples = FILE_BUFFER_SIZE; else cursamples = (frames-i);
	      mus_file_read(ifd,0,cursamples-1,chans,bufs);
	      err = mus_file_write(ofd,0,cursamples-1,chans,bufs);
	      if (err == -1) break;
	      check_for_event(ss); /* added 3-Jul-00 -- is this safe? */
	    }
	  snd_close(ifd);
	  for (i=0;i<chans;i++) FREE(bufs[i]);
	  FREE(bufs);
	}
      snd_close(ofd);
      alert_new_file();
    }
  return(SND_NO_ERROR);
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

int delete_selection(char *origin, int regraph)
{
  /* if we own the selection, delete it from the current sync'd channels, reset stippling, update displays */
  region *r;
  int i;
  chan_info **ncp;
  region_context *rg;
  finish_keyboard_selection();
  r = regions[0];
  if ((r) && (r->rgx))
    {
      ncp = (chan_info **)CALLOC(r->chans,sizeof(chan_info *));
      for (i=0;i<r->chans;i++)
	{
	  rg = r->rgx[i];
	  ncp[i] = rg->cp;
	  if (samp1(rg) > samp0(rg))
	    delete_samples(samp0(rg),samp1(rg) - samp0(rg) + 1,ncp[i],origin);
	}
      free_region_contexts(r);
      if (regraph == UPDATE_DISPLAY)
	{
	  for (i=0;i<r->chans;i++)
	    {
	      update_graph(ncp[i],NULL);
	    }
	}
      FREE(ncp);
      reflect_edit_without_selection_in_menu();
      return(1);
    }
  return(0);
}

static int paste_region_1(int n, chan_info *cp, int add, int beg, Float scaler, char *origin)
{
  region *r;
  region_context *rg;
  int chn,i,j,k,err=SND_NO_ERROR,id=-1,idtmp;
  snd_info *sp;
  sync_info *si;
  chan_info *ncp;
  MUS_SAMPLE_TYPE *data;
  snd_state *ss;
  char *tempfile = NULL;
  ss = cp->state;
  sp = cp->sound;
  si = NULL;
  finish_keyboard_selection();
  /* 
   * two cases: if current selection, rg has the cp pointers, else traverse in parallel
   * so -- get syncd chans, if rg, loop looking for matches, else loop until either exhausted
   */
  if (region_ok(n)) r = regions[n]; else r=NULL;
  if (r)
    {
      /* get syncd chans relative to current (cp) */
      if (sp->syncing != 0) si = snd_sync(ss,sp->syncing);  
      else si = make_simple_sync(cp,beg);
      /* chans in si->cps[i], for si->chans */
      if (add)
	{
	  if (r->use_temp_file == REGION_ARRAY)
	    idtmp = mix_array(beg,r->len,r->data,si->cps,r->chans,si->chans,SND_SRATE(sp),origin,with_mix_consoles(ss));
	  else idtmp = mix_file(beg,r->len,r->filename,si->cps,si->chans,origin,with_mix_consoles(ss));
	  if (id == -1) id = idtmp;
	}
      else
	{
	  if (r->use_temp_file == REGION_FILE)
	    {
	      tempfile = snd_tempnam(ss);
	      err = copy_file(r->filename,tempfile);
	      if (err != SND_NO_ERROR)
		snd_error("can't make region temp file (%s: %s)",tempfile,strerror(errno));
	      else if (r->chans > 1) remember_temp(tempfile,r->chans);
	    }
	  /* if current_selection and rg->cp == si->cps[i] for some rg or i, edit there, else go through each in order until either is done */
	  for (i=0;((i<r->chans) && (i<si->chans));i++)
	    {
	      ncp = si->cps[i]; /* currently syncd chan that we might paste to */
	      chn  = i;         /* default is parallel paste */
	      if ((n == 0) && (r->rgx))
		{
		  for (k=0;k<r->chans;k++)
		    {
		      rg = r->rgx[k];
		      if ((rg->cp) == ncp)
			{
			  chn = k;
			  break;
			}
		    }
		}
	      /* now chn = region chan, ncp = Snd chan */
	      if (r->use_temp_file == REGION_ARRAY)
		{
		  data = (MUS_SAMPLE_TYPE *)CALLOC(r->len,sizeof(MUS_SAMPLE_TYPE));
		  if (scaler == 1.0)
		    {
		      for (j=0;j<r->len;j++) data[j] = r->data[chn][j];
		    }
		  else
		    {
		      for (j=0;j<r->len;j++) data[j] = (MUS_SAMPLE_TYPE)(scaler * r->data[chn][j]);
		    }
		  insert_samples(beg,r->len,data,ncp,origin);
		  FREE(data);
		}
	      else
		{
		  if (err == SND_NO_ERROR)
		    file_insert_samples(beg,r->len,tempfile,ncp,chn,(r->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,origin);
		}
	      if ((n == 0) && (r->rgx) && ((beg+r->len) >= samp0(r->rgx[0])) && (beg <= samp1(r->rgx[0])))
		free_region_contexts(r);
	      /* we mixed or pasted into the active selection, so it should not remain active */
	      update_graph(ncp,NULL);
	    }
	}
    }
  if (si) free_sync_info(si);
  return(id);
}

static int paste_fix_region(int n) {if (n > regions_size) return(0); return(n);}
static Float paste_fix_scaler(int n, chan_info *cp) {if (n > regions_size) return((Float)n/(Float)SND_SRATE(cp->sound)); return(1.0);}

void paste_region(int n, chan_info *cp,char *origin) {paste_region_1(paste_fix_region(n),cp,FALSE,cp->cursor,paste_fix_scaler(n,cp),origin);}
int add_region(int n, chan_info *cp, char *origin) {return(paste_region_1(paste_fix_region(n),cp,TRUE,cp->cursor,paste_fix_scaler(n,cp),origin));}
static int mix_region(int n, chan_info *cp, int beg, Float scaler) {return(paste_region_1(n,cp,TRUE,beg,scaler,S_mix_region));}

/* we're drawing the selection in one channel, but others may be sync'd to it */

static double selbeg,selend;    /* true bounds (as the axes move etc) */
static int keyboard_selecting = 0;

void finish_keyboard_selection(void)
{
  region_context **rgx;
  if (keyboard_selecting)
    {
      rgx = regions[0]->rgx;
      define_selection(rgx[0]->cp);
      keyboard_selecting = 0;
    }
}

int cancel_keyboard_selection (void)
{
  region_context **rgx;
  snd_info *sp = NULL;
  int beg = -1;
  if (keyboard_selecting)
    {
      keyboard_selecting = 0;
      rgx = regions[0]->rgx;
      if ((rgx) && (sp = (rgx[0]->sp)))
	{
	 clear_minibuffer(sp);
	 beg = (int)(selbeg * SND_SRATE(sp)); /* during kbd selection we have to use selbeg (samp0 not set yet) */
	}
      deactivate_selection();
      delete_region(0);
    }
  return(beg);
}

void start_selection (chan_info *cp, int x)
{ /* only from mouse so we use ungrf here (very inaccurate if lots of data displayed) */
  if (keyboard_selecting) /* i.e. mouse button while c-space active = save and start again */
    {
      finish_keyboard_selection();
    }
  /* reflect_edit_with_selection_in_menu(); */ /* can be called when cursor first defined -- no selection intended */
  selbeg = ungrf_x(cp->axis,x);
  if (selbeg < 0.0) selbeg = 0.0;
  selend = selbeg;
}

void start_keyboard_selection(chan_info *cp, int x)
{
  if (active_selection(cp)) deactivate_selection(); /* i.e. c-space while c-space active = flush and start again */
  keyboard_selecting = 0;
  create_selection(cp);
  reflect_edit_with_selection_in_menu();
  if (cp->cursor_on)                           /* use exact sample if it's available */
    selbeg = (double)(cp->cursor)/(double)SND_SRATE(cp->sound);
  else ungrf_x(cp->axis,x);
  if (selbeg < 0.0) selbeg = 0.0;
  selend = selbeg;
  keyboard_selecting = 1;
  cp->cursor_on = 1;
}

static void redraw_selection(chan_info *cp, int x);

void check_keyboard_selection(chan_info *cp, int x)
{
  if (keyboard_selecting) redraw_selection(cp,x);
}

static void draw_selection_portion(chan_info *cp, region_context *rg)
{
  if (cp->waving)
    fill_rectangle(selection_context(cp),rg->x0,rg->y0,rg->width,rg->height);
}

static void erase_selection_portion(chan_info *cp, region_context *rg)
{
  if (cp->waving)
    fill_rectangle(selection_context(cp),rg->x0,rg->y0,rg->width,rg->height);
}

void deactivate_selection(void)
{
  region *r;
  region_context *rg;
  int i;
  chan_info *ncp;
  r = regions[0];
  if ((r) && (r->rgx))
    {
      for (i=0;i<r->chans;i++)
	{
	  rg = r->rgx[i];
	  ncp = rg->cp;
	  if (rg->visible) erase_selection_portion(ncp,rg);
	}
      free_region_contexts(r);
    }
  reflect_edit_without_selection_in_menu();
}

/* if the current view is small (i.e. widely spaced individual samples),
 * we need to get only those samples that fall within the selection bounds.
 * But this needs to be a little sloppy for convenience.
 */

static int round_up(double x)
{
  int xint;
  Float xfrac;
  xint = (int)x;
  xfrac = x - xint;
  if (xfrac > .2) return(xint+1);
  return(xint);
}

static int round_down(double x)
{
  int xint;
  Float xfrac;
  xint = (int)x;
  xfrac = x - xint;
  if (xfrac < .8) return(xint);
  return(xint+1);
}

static void redraw_selection(chan_info *cp, int x)
{
  /* called as mouse is dragged, for example */
  region *r;
  region_context *rg;
  int i;
  Float sx0,sx1;
  chan_info *ncp;
  axis_info *ap;
  snd_state *ss;
  ss = cp->state;
  if (keyboard_selecting)
    selend = (double)(cp->cursor)/(double)SND_SRATE(cp->sound);
  else selend = ungrf_x(cp->axis,x);
  if (selend < 0.0) selend = 0.0;
  r=regions[0];
  if (r)
    {
      for (i=0;i<r->chans;i++)
	{
	  rg = r->rgx[i];
	  if (rg)
	    {
	      ncp = rg->cp;
	      ap = ncp->axis;
	      if (rg->visible) erase_selection_portion(ncp,rg);
	      if (selbeg < selend)
		{
		  if (selbeg >= ap->x0) sx0 = selbeg; else sx0 = ap->x0;
		  if (selend <= ap->x1) sx1 = selend; else sx1 = ap->x1;
		}
	      else
		{
		  if (selend >= ap->x0) sx0 = selend; else sx0 = ap->x0;
		  if (selbeg <= ap->x1) sx1 = selbeg; else sx1 = ap->x1;
		}
	      rg->x0 = grf_x(sx0,ap);
	      rg->y0 = ap->y_axis_y1;
	      rg->width = grf_x(sx1,ap) - rg->x0;
	      rg->height = (unsigned int)(ap->y_axis_y0-ap->y_axis_y1);
	      rg->visible = 1;
	      
	      if (show_selection_transform(ss)) 
		{
		  if (selbeg < selend)
		    {
		      rg->first = round_up(selbeg * SND_SRATE(cp->sound));
		      rg->last = round_down(selend * SND_SRATE(cp->sound));
		    }
		  else
		    {
		      rg->first = round_up(selend * SND_SRATE(cp->sound));
		      rg->last = round_down(selbeg * SND_SRATE(cp->sound));
		    }
		  r->len = rg->last - rg->first + 1;
		}

	      draw_selection_portion(ncp,rg);
	      if ((show_selection_transform(ss)) && (ncp->ffting) && (!(chan_fft_in_progress(ncp)))) calculate_fft(ncp,NULL);
	    }
	}
    }
}

int selection_active(chan_info *cp)
{
  return((regions[0]) && (region_member(cp,regions[0])));
}

void display_selection(chan_info *cp)
{ /* cp's graph was just cleared and redrawn -- now add selection, if relevant */
  int x0,x1,sx0,sx1;
  region_context *rg;
  axis_info *ap;
  snd_info *sp;
  if ((regions[0]) && (rg=(region_member(cp,regions[0]))))
    {
      /* we have the current selection -- should it be visible? */
      ap = cp->axis;
      sp = cp->sound;
      if (keyboard_selecting) /* selection definition is in progress, so rg->first and last are not set yet */
	{
	  x0 = (int)(selbeg * SND_SRATE(sp));
	  x1 = (int)(selend * SND_SRATE(sp));
	  if (x1 < x0) {sx0 = x0; x0 = x1; x1 = sx0;}
	}
      else
	{
	  x0 = samp0(rg);
	  x1 = samp1(rg);
	}
      if ((x0 <= ap->hisamp) && (x1 >= ap->losamp))
	{
	  if (ap->losamp > x0) x0 = ap->losamp;
	  if (ap->hisamp < x1) x1 = ap->hisamp;
	  sx0 = grf_x((double)x0/(double)SND_SRATE(sp),ap);
	  sx1 = grf_x((double)x1/(double)SND_SRATE(sp),ap);
	  rg->x0 = sx0;
	  rg->y0 = ap->y_axis_y1;
	  rg->width = sx1-sx0;
	  rg->height = (unsigned int)(ap->y_axis_y0-ap->y_axis_y1);
	  rg->visible = 1;
	  draw_selection_portion(cp,rg);
	}
    }
}

void ripple_selection(chan_info *cp, int beg, int num)
{
  /* if selection channel = cp, and selection samp > beg, fixup samp nums, etc */
  /* this has to be undo-able, so these fixups need to follow edit_ctr -- */
  /* region_context needs an integer array of num which we use before accessing samp0/samp1 */
  region_context *rg;
  int previous_fixup,i,old_size;
  if ((regions[0]) && (rg=region_member(cp,regions[0])))
    {
      if (samp0(rg) > beg)
	{
	  draw_selection_portion(cp,rg);
	  if (!rg->fixups)
	    {
	      rg->fixups = (int *)CALLOC(cp->edit_size,sizeof(int));
	      rg->fixup_size = cp->edit_size;
	      previous_fixup = 0;
	    }
	  else 
	    {
	      if (cp->edit_size > rg->fixup_size)
		{
		  /* must have REALLOCated on this edit */
		  old_size = rg->fixup_size;
		  rg->fixup_size = cp->edit_size;
		  rg->fixups = (int *)REALLOC(rg->fixups,rg->fixup_size * sizeof(int));
		  for (i=old_size;i<rg->fixup_size;i++) rg->fixups[i] = 0;
		}
	      if (cp->edit_ctr <= 0) /* can this happen? */
		previous_fixup = 0;
	      else previous_fixup = rg->fixups[cp->edit_ctr-1];
	    }

	  rg->fixups[cp->edit_ctr] = previous_fixup+num;
	  display_selection(cp);
	}
    }
}

void create_selection(chan_info *cp)
{
  /* called upon initial mouse drag notification and elsewhere */
  /* if we're sync'd collect all */
  snd_info *sp,*nsp;
  snd_state *ss;
  region *r;
  region_context *rg;
  int chans,i,j,k;
  r = (region *)CALLOC(1,sizeof(region));
  r->id = region_id_ctr++;
  sp = cp->sound;
  ss = cp->state;
  if (regions[0]) stack_region(ss,r); else regions[0] = r;
  report_in_minibuffer(sp,STR_defining_region);
  r->header_type = (sp->hdr)->type;
  r->srate = SND_SRATE(sp);
  r->maxamp = 0.0;
  r->editor_copy = NULL;
  r->name = copy_string(sp->shortname);
  chans = 1;
  if (sp->syncing != 0) chans = syncd_chans(ss,sp->syncing);
  r->chans = chans;
  r->rgx = (region_context **)CALLOC(chans,sizeof(region_context *));
  r->data = (MUS_SAMPLE_TYPE **)CALLOC(chans,sizeof(MUS_SAMPLE_TYPE *));
  /* don't alloc these until definition time when we know the selection length */
  if (sp->syncing != 0)
    {
      k = 0;
      for (i=0;i<ss->max_sounds;i++)
	{
	  nsp = ss->sounds[i];
	  if ((nsp) && (nsp->inuse) && (nsp->syncing == sp->syncing))
	    {
	      for (j=0;j<nsp->nchans;j++)
		{
		  rg = (region_context *)CALLOC(1,sizeof(region_context));
		  r->rgx[k] = rg;
		  /* set cp sp fields while we know what we're doing */
		  rg->sp = nsp;
		  rg->cp = nsp->chans[j];
		  rg->visible = 0;
		  k++;
		}
	    }
	}
    }
  else 
    {
      rg = (region_context *)CALLOC(1,sizeof(region_context));
      r->rgx[0] = rg;
      rg->sp = sp;
      rg->cp = cp;
      rg->visible = 0;
    }
}

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
	    fil += (r->len*r->chans*2);
	  else arr += (r->len*r->chans*4);
	}
    }
  vals[0] = arr;
  vals[1] = fil;
}

static void load_region(chan_info *cp, region *r, int beg, int end)
{
  /* now look at all sync'd channels, collect them into the current region */
  /* we created the necessary pointers in create_selection above */
  int i,j,len,k,ofd = 0,datumb = 0,err=0;
  MUS_SAMPLE_TYPE val,mval,curval;
  snd_fd **sfs;
  snd_state *ss;
  snd_info *sp;
  file_info *hdr = NULL;
  region_context *rg;
  len = end-beg+1;
  if (len <= 0) return;
  r->len = len;
  r->beg = beg;
  val = MUS_SAMPLE_0; 
  mval = MUS_SAMPLE_0;
  r->start = prettyf((Float)beg/(Float)(r->srate),2);
  r->end = prettyf((Float)end/(Float)(r->srate),2);
  sfs = (snd_fd **)CALLOC(r->chans,sizeof(snd_fd *));
  ss = cp->state;
  if (r->len >= MAX_BUFFER_SIZE)
    {
      sp = cp->sound;
      r->use_temp_file = REGION_FILE;
      r->filename = copy_string(snd_tempnam(ss));
      hdr = make_temp_header(ss,r->filename,sp->hdr,0);
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
      rg = r->rgx[i];
      rg->first = beg;
      rg->last = end;
      sfs[i]=init_sample_read(beg,rg->cp,READ_FORWARD);
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
	  NEXT_SAMPLE(curval,sfs[i]);
	  r->data[i][k] = curval;
	  if (curval>val) val=curval;
	  if (curval<mval) mval=curval;
	}
    }
  if (r->use_temp_file == REGION_FILE)
    {
      if (k > 0) mus_file_write(ofd,0,k-1,r->chans,r->data);
      close_temp_file(ofd,hdr,len*r->chans*datumb,cp->sound);
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

static int watching_selection = 0;
static int last_selection_x = 0;

static void start_selection_watching(chan_info *cp)
{
  StartSelectionWatch(cp);
  watching_selection = 1;
}

static void cancel_selection_watch(void)
{
  CancelSelectionWatch();
  watching_selection = 0;
}

static void move_selection_1(chan_info *cp, int x)
{
  axis_info *ap;
  int nx;
  ap = cp->axis;
  if ((x > ap->x_axis_x1) || (x < ap->x_axis_x0)) 
    {
      if (((x > ap->x_axis_x1) && (ap->x1 == ap->xmax)) ||
	  ((x < ap->x_axis_x0) && (ap->x0 == ap->xmin)))
	return;
      nx = move_axis(cp,ap,x);
      if (!watching_selection) start_selection_watching(cp);
    }
  else 
    {
      nx = x;
      if (watching_selection) cancel_selection_watch();
    }
  redraw_selection(cp,nx);
}

void move_selection_2(chan_info *cp)
{
  move_selection_1(cp,last_selection_x); /* called via watch work proc */
}

void move_selection(chan_info *cp, int x)
{
  last_selection_x = x; /* called in snd-xchn -- sets last_selection_x */
  move_selection_1(cp,x);
}


void define_selection(chan_info *cp)
{
  /* called in snd-xchn.c upon mouse button release */
  region *r;
  region_context *rg;
  Float tmp;
  if (watching_selection) cancel_selection_watch();
  keyboard_selecting = 0; /* mouse click might have interrupted kbd definition */
  r = regions[0];
  rg = region_member(cp,r);
  if (rg->sp) clear_minibuffer(rg->sp);
  if (selbeg > selend) {tmp = selbeg; selbeg = selend; selend = tmp;}
  load_region(cp,r,round_up(selbeg * SND_SRATE(cp->sound)),round_down(selend * SND_SRATE(cp->sound)));
  reflect_edit_with_selection_in_menu();
}

void define_region(chan_info *cp, int beg, int end, int cleared)
{
  deactivate_selection();
  if (beg > end) return; /* was >=? */
  create_selection(cp);
  load_region(cp,regions[0],beg,end);
  if (cleared) clear_minibuffer(cp->sound);
  reflect_edit_with_selection_in_menu();
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
	  if ((beg == 0) && (direction == READ_BACKWARD)) beg=r->len-1;
	  return(init_sample_read(beg,rsp->chans[chan],direction));
	}
    }
  return(NULL);
}

void play_region(snd_state *ss, int n, void *rg, int to_end)
{
  region_info *ri;
  finish_keyboard_selection();
  if (!(region_ok(n))) return;
  ri = (region_info *)CALLOC(1,sizeof(region_info));
  ri->s_type = REGION_INFO;
  ri->r = (void *)regions[n];
  ri->n = n;
  ri->rg = rg;
  ri->ss = ss;
  if (to_end)
    play_to_end(ri,0,NO_END_SPECIFIED);
  else start_playing(ri,0,NO_END_SPECIFIED);
}

sync_info *region_sync(int n)
{
  region *r;
  region_context *rg;
  sync_info *si;
  int i;
  finish_keyboard_selection();
  if (region_ok(n)) r = regions[n]; else r=NULL;
  if (r)
    {
      si = (sync_info *)CALLOC(1,sizeof(sync_info));
      si->chans = r->chans;
      si->cps = (chan_info **)CALLOC(si->chans,sizeof(chan_info *));
      si->begs = (int *)CALLOC(si->chans,sizeof(int));
      for (i=0;i<r->chans;i++)
	{
	  rg = r->rgx[i];
	  if (rg)
	    {
	      si->begs[i] = samp0(rg);
	      si->cps[i] = rg->cp;
	    }
	}
      return(si);
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
	          S_restore_region,i,r->chans,r->len,r->srate,r->maxamp,r->name,r->start,r->end);
	  if (r->use_temp_file == REGION_ARRAY)
	    {
	      fprintf(fd,"\n  #(");
	      for (j=0;j<r->chans;j++)
		{
		  for (k=0;k<r->len;k++)
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
		  newname = shorter_tempnam(save_dir(ss),"snd_");
		  snd_copy_file(r->filename,newname);
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
		    ibufs[j] = (MUS_SAMPLE_TYPE *)CALLOC(r->len,sizeof(MUS_SAMPLE_TYPE));
		  mus_file_read(ifd,0,r->len-1,r->chans,ibufs);
		  fprintf(fd,"\n  #(");
		  for (j=0;j<r->chans;j++)
		    {
		      for (k=0;k<r->len;k++) 
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
	snd_error("region already being edited");
      else
	{
	  temp_region_name = shorter_tempnam(temp_dir(ss),"region-");
	  if (r->use_temp_file == REGION_FILE)
	    err = copy_file(r->filename,temp_region_name);
	  else err = save_region(ss, reg, temp_region_name, MUS_BINT);
	  if (err == SND_NO_ERROR)
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
	      else snd_error("can't open sound pane!");
	    }
	  else 
	    snd_error("can't save region in temp file (%s: %s)",temp_region_name,strerror(errno));
	  FREE(temp_region_name);
	}
    }
  else snd_error("no such region!");
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
	  r->len = current_ed_samples(sp->chans[0]);
	  for (i=0;i<sp->nchans;i++)
	    {
	      val = get_maxamp(sp,sp->chans[i]);
	      if (val > r->maxamp) r->maxamp = val;
	    }
	  /* make new region temp file */
	  r->filename = snd_tempnam(ss);
	  err = copy_file(r->editor_name,r->filename);
	  if (err != SND_NO_ERROR)
	    snd_error("can't make region temp file (%s: %s)",r->filename,strerror(errno));
	  make_region_readable(r,ss);
	  if (region_browser_is_active()) update_region_browser(ss,1);
	}
    }
}

#if HAVE_GUILE
#include "sg.h"

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
  r->rgx = NULL;
  r->editor_copy = NULL;
  r->editor_name = NULL;
  r->len = gh_scm2int(len);
  r->srate = gh_scm2int(srate);
  r->name = gh_scm2newstr(name,NULL);
  r->start = gh_scm2newstr(start,NULL);
  r->end = gh_scm2newstr(end,NULL);
  if (gh_string_p(data))
    {
      r->use_temp_file = REGION_FILE;
      r->filename = gh_scm2newstr(data,NULL);
    }
  else 
    {
      r->use_temp_file = REGION_ARRAY;
      r->filename = NULL;
      r->data = (MUS_SAMPLE_TYPE **)CALLOC(r->chans,sizeof(MUS_SAMPLE_TYPE *));
      k=0; 
      for (i=0;i<r->chans;i++)
	{
	  r->data[i] = (MUS_SAMPLE_TYPE *)CALLOC(r->len,sizeof(MUS_SAMPLE_TYPE));
	  for (j=0;j<r->len;j++,k++)
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
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  finish_keyboard_selection();
  rg = g_scm2intdef(reg_n,0);
  if (!(region_ok(rg))) return(NO_SUCH_REGION);
  samp = g_scm2intdef(samp_n,0);
  paste_region_1(rg,cp,FALSE,samp,1.0,S_insert_region);
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
  #define H_set_max_regions "(" S_set_max_regions " val) sets the max length of the region list"
  snd_state *ss;
  ERRN1(n,S_set_max_regions); 
  ss = get_global_state();
  set_max_regions(ss,g_scm2int(n));
  RTNINT(max_regions(ss));
}

enum {REGION_LENGTH,REGION_SRATE,REGION_CHANS,REGION_MAXAMP,REGION_SELECT,REGION_DELETE,REGION_PLAY,REGION_ID};

static SCM region_read(int field, SCM n)
{
  int rg;
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
  else return(NO_SUCH_REGION);
  RTNINT(0);
}

static SCM g_regionQ(SCM n)
{
  #define H_regionQ "(" S_regionQ " reg) -> #t if region is active"
  ERRN1(n,S_regionQ);
  RTNBOOL(region_ok(g_scm2int(n)));
}

static SCM g_region_length (SCM n) 
{
  #define H_region_length "(" S_region_length " &optional (n 0)) -> length in frames of region"
  ERRB1(n,S_region_length); 
  return(region_read(REGION_LENGTH,n));
}

static SCM g_region_srate (SCM n) 
{
  #define H_region_srate "(" S_region_srate " &optional (n 0)) -> srate of region n"
  ERRB1(n,S_region_srate); 
  return(region_read(REGION_SRATE,n));
}

static SCM g_region_chans (SCM n) 
{
  #define H_region_chans "(" S_region_chans " &optional (n 0) -> channels of data in region n"
  ERRB1(n,S_region_chans); 
  return(region_read(REGION_CHANS,n));
}

static SCM g_region_id (SCM n) 
{
  #define H_region_id "(" S_region_id " &optional (n 0) -> unique id of region n"
  ERRB1(n,S_region_id); 
  return(region_read(REGION_ID,n));
}

static SCM g_id_region (SCM n) 
{
  #define H_id_region "(" S_id_region " &optional (id 0) -> stack location of region with id"
  int sn;
  ERRB1(n,S_id_region); 
  sn = id_region(g_scm2intdef(n,0));
  if (sn == -1) return(NO_SUCH_REGION);
  RTNINT(sn);
}

static SCM g_region_maxamp (SCM n) 
{
  #define H_region_maxamp "(" S_region_maxamp " &optional (n 0)) -> max amp of region n"
  ERRB1(n,S_region_maxamp); 
  return(region_read(REGION_MAXAMP,n));
}

static SCM g_select_region (SCM n) 
{
  #define H_select_region "(" S_select_region " &optional (n 0)) selects region n (moves it to the top of the region list)"
  ERRB1(n,S_select_region); 
  return(region_read(REGION_SELECT,n));
}

static SCM g_delete_region (SCM n) 
{
  #define H_delete_region "(" S_delete_region " &optional (n 0)) remove region n from the region list"
  ERRB1(n,S_delete_region); 
  return(region_read(REGION_DELETE,n));
}

static SCM g_play_region (SCM n, SCM wait) 
{
  #define H_play_region "(" S_play_region " &optional (n 0) (wait #f)) play region n, if wait is #t, play to end before returning"
  int rg;
  ERRB1(n,S_play_region); 
  ERRB2(wait,S_play_region);
  rg = g_scm2intdef(n,0);
  if (region_ok(rg))
    play_region(get_global_state(),rg,NULL,g_scm2intdef(wait,0));
  else return(NO_SUCH_REGION);
  return(n);
}

static SCM g_protect_region (SCM n, SCM protect) 
{
  #define H_protect_region "(" S_protect_region " &optional (n 0) (val #t)) if val is #t protects region n from being\n\
   pushed off the end of the region list"

  ERRN1(n,S_protect_region);
  ERRB2(protect,S_protect_region);
  set_region_protect(g_scm2int(n),bool_int_or_one(protect)); 
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
  ERRN1(beg,S_make_region);
  ERRN2(end,S_make_region);
  ERRCP(S_make_region,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  define_region(cp,g_scm2int(beg),g_scm2int(end),FALSE);
  RTNINT(region_id(0));
}

static SCM g_selectionQ(void)
{
  #define H_selectionQ "(" S_selectionQ ") -> #t if selection is currently active, visible, etc"
  RTNBOOL(selection_is_current());
}

static SCM g_selection_beg(void)
{
  #define H_selection_beg "(" S_selection_beg ") -> selection start samp in selected sound"
  if (selection_is_current())
    return(gh_int2scm(selection_beg(NULL)));
  return(NO_ACTIVE_SELECTION);
}

static SCM g_selection_length(void)
{
  #define H_selection_length "(" S_selection_length ") -> length (frames) of selected portion"
  if (selection_is_current())
    return(gh_int2scm(region_len(0)));
  return(NO_ACTIVE_SELECTION);
}

static SCM g_selection_member(SCM snd, SCM chn)
{
  #define H_selection_member "(" S_selection_member " &optional snd chn) -> #t if snd's channel chn is a member of the current selection"
  chan_info *cp;
  ERRCP(S_selection_member,snd,chn,1);
  cp = get_cp(snd,chn);
  if ((cp) && (selection_is_current_in_channel(cp)))
    return(SCM_BOOL_T);
  return(SCM_BOOL_F);
}

static SCM g_select_all (SCM snd_n, SCM chn_n)
{
  #define H_select_all "(" S_select_all " &optional snd chn) makes a new selection containing all of snd's channel chn"
  chan_info *cp;
  ERRCP(S_select_all,snd_n,chn_n,1);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  define_region(cp,0,current_ed_samples(cp),FALSE);
  update_graph(cp,NULL);
  RTNINT(region_id(0));
}

static SCM g_save_region (SCM n, SCM filename, SCM format) 
{
  #define H_save_region "(" S_save_region " region filename &optional format) saves region in filename using data format (mus-bshort)"
  char *name = NULL,*urn;
  int res=SND_NO_ERROR,rg;
  ERRN1(n,S_save_region);
  ERRS2(filename,S_save_region);
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
  else return(NO_SUCH_REGION);
  if (res != SND_NO_ERROR)
    return(SCM_BOOL_F);
  return(CANNOT_SAVE);
}

static SCM g_mix_region(SCM chn_samp_n, SCM scaler, SCM reg_n, SCM snd_n, SCM chn_n)
{
  #define H_mix_region "(" S_mix_region " &optional (chn-samp 0) (scaler 1.0) (region 0) snd chn) mixer region\n\
   into snd's channel chn starting at chn-samp scaled by scaler; returns new mix id."

  chan_info *cp;
  int rg,id=-1;
  ERRB1(chn_samp_n,S_mix_region);
  ERRB2(scaler,S_mix_region);
  ERRB3(reg_n,S_mix_region);
  ERRCP(S_mix_region,snd_n,chn_n,4);
  rg = g_scm2intdef(reg_n,0);
  if (region_ok(rg))
    {
      cp = get_cp(snd_n,chn_n);
      if (cp)
	id = mix_region(rg,cp,
			g_scm2intdef(chn_samp_n,cp->cursor),
			(gh_number_p(scaler) ? (gh_scm2double(scaler)) : 1.0));
      else return(NO_SUCH_CHANNEL);
    }
  else return(NO_SUCH_REGION);
  return(gh_int2scm(id));
}

static SCM g_region_sample(SCM samp_n, SCM reg_n, SCM chn_n)
{
  #define H_region_sample "(" S_region_sample " &optional (samp 0) (region 0) (chan 0)) -> region's sample at samp in chan"
  ERRB1(samp_n,S_region_sample);
  ERRB2(reg_n,S_region_sample);
  ERRB3(chn_n,S_region_sample);
  finish_keyboard_selection();
  RTNFLT(region_sample(g_scm2intdef(reg_n,0),g_scm2intdef(chn_n,0),g_scm2intdef(samp_n,0)));
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
  finish_keyboard_selection();
  reg = g_scm2intdef(reg_n,0);
  if (!(region_ok(reg))) return(NO_SUCH_REGION);
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
  else return(NO_SUCH_CHANNEL);
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
  finish_keyboard_selection();
  ERRB1(beg_n,S_region_samples_vct);
  ERRB2(num,S_region_samples_vct);
  ERRB3(reg_n,S_region_samples_vct);
  ERRB4(chn_n,S_region_samples_vct);
  reg = g_scm2intdef(reg_n,0);
  if (!(region_ok(reg))) return(NO_SUCH_REGION);
  chn = g_scm2intdef(chn_n,0);
  if (chn >= region_chans(reg)) return(NO_SUCH_CHANNEL);
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
  DEFINE_PROC(gh_new_procedure(S_max_regions,SCM_FNC g_max_regions,0,0,0),H_max_regions);
  DEFINE_PROC(gh_new_procedure(S_set_max_regions,SCM_FNC g_set_max_regions,1,0,0),H_set_max_regions);
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
  DEFINE_PROC(gh_new_procedure(S_make_region,SCM_FNC g_make_region,2,2,0),H_make_region);
  DEFINE_PROC(gh_new_procedure(S_selection_beg,SCM_FNC g_selection_beg,0,0,0),H_selection_beg);
  DEFINE_PROC(gh_new_procedure(S_selection_length,SCM_FNC g_selection_length,0,0,0),H_selection_length);
  DEFINE_PROC(gh_new_procedure(S_selection_member,SCM_FNC g_selection_member,0,2,0),H_selection_member);
  DEFINE_PROC(gh_new_procedure(S_select_all,SCM_FNC g_select_all,0,2,0),H_select_all);
  DEFINE_PROC(gh_new_procedure(S_mix_region,SCM_FNC g_mix_region,0,5,0),H_mix_region);
  DEFINE_PROC(gh_new_procedure(S_region_sample,SCM_FNC g_region_sample,0,3,0),H_region_sample);
  DEFINE_PROC(gh_new_procedure(S_region_samples,SCM_FNC g_region_samples,0,4,0),H_region_samples);
  DEFINE_PROC(gh_new_procedure(S_region_samples_vct,SCM_FNC g_region_samples2vct,0,5,0),H_region_samples2vct);
  DEFINE_PROC(gh_new_procedure(S_regionQ,SCM_FNC g_regionQ,1,0,0),H_regionQ);
  DEFINE_PROC(gh_new_procedure(S_selectionQ,SCM_FNC g_selectionQ,0,0,0),H_selectionQ);
}

#endif
