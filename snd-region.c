#include "snd.h" 

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
  char *name, *start, *end;  /* for region browser */
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
	  for (i = 0; i < r->chans; i++) 
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
	      snd_remove(r->filename);
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

void allocate_regions(snd_state *ss, int numreg)
{
  int i;
  if (numreg > regions_size)
    {
      if (regions)
	{
	  regions = (region **)REALLOC(regions, numreg * sizeof(region *));
	  for (i = regions_size; i < numreg; i++) regions[i] = NULL;
	}
      else regions = (region **)CALLOC(numreg, sizeof(region *));
    }
  else
    {
      if (regions_size > numreg)
	{
	  for (i = numreg; i < regions_size; i++)
	    if (regions[i])
	      {
		free_region(regions[i], COMPLETE_DELETION);
		regions[i] = NULL;
	      }
	  if (region_browser_is_active()) update_region_browser(ss, 1);
	}
    }
  regions_size = numreg;
}

static void set_max_regions(snd_state *ss, int n)
{
  if (n >= 0)
    {
      allocate_regions(ss, n);
      allocate_region_rows(ss, n);
      in_set_max_regions(ss, n);
    }
}

int region_ok(int n) {return((n >= 0) && (n < regions_size) && (regions[n]));}
int region_len(int n) {if (region_ok(n)) return(regions[n]->frames); else return(0);}
int region_chans(int n) {if (region_ok(n)) return(regions[n]->chans); else return(0);}
int region_srate(int n) {if (region_ok(n)) return(regions[n]->srate); else return(0);}
Float region_maxamp(int n) {if (region_ok(n)) return(regions[n]->maxamp); else return(0.0);}
int region_id(int n) {if (region_ok(n)) return(regions[n]->id); else return(-1);}

static int id_region(int id)
{
  int i;
  for (i = 0; i < regions_size; i++)
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
	      sf = init_region_read(get_global_state(), samp, reg, chn, READ_FORWARD);
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
  int i, j;
  if (region_ok(reg))
    {
      r = regions[reg];
      if ((beg < r->frames) && (chn < r->chans))
	{
	  if (r->use_temp_file == REGION_ARRAY)
	    {
	      for (i = beg, j = 0; (i < r->frames) && (j < num); i++, j++) 
		data[j] = MUS_SAMPLE_TO_FLOAT(r->data[chn][i]);
	    }
	  else
	    {
	      sf = init_region_read(get_global_state(), beg, reg, chn, READ_FORWARD);
	      for (i = beg, j = 0; (i < r->frames) && (j < num); i++, j++) 
		data[j] = next_sample_to_float(sf);
	      free_snd_fd(sf);
	    }
	  if (j < num) 
	    for (; j < num; j++) 
	      data[j] = 0.0;
	}
    }
}

static int first_region_active(void)
{
  int i;
  for (i = 0; i < regions_size; i++)
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
  int i, fd;
  if (r->rsp) return;
  regsp = (snd_info *)CALLOC(1, sizeof(snd_info));
  regsp->nchans = r->chans;
  regsp->allocated_chans = r->chans; /* needed for complete GC */
  regsp->chans = (chan_info **)CALLOC(r->chans, sizeof(chan_info *));
  regsp->hdr = (file_info *)CALLOC(1, sizeof(file_info));
  regsp->search_proc = SCM_UNDEFINED;
  regsp->eval_proc = SCM_UNDEFINED;
  regsp->prompt_callback = SCM_UNDEFINED;
  hdr = regsp->hdr;
  hdr->samples = r->frames * r->chans;
  hdr->srate = r->srate;
  hdr->chans = r->chans;
  hdr->comment = NULL;
  for (i = 0; i < r->chans; i++)
    {
      cp = make_chan_info(NULL, i, regsp, ss);
      regsp->chans[i] = cp;
      add_channel_data_1(cp, regsp, ss, WITHOUT_GRAPH);
      set_initial_ed_list(cp, r->frames-1);
      cp->edit_size = 1;
      cp->sound_size = 1;
      cp->hookable = 0;
      if (r->use_temp_file == REGION_ARRAY)
	cp->sounds[0] = make_snd_data_buffer(r->data[i], r->frames, cp->edit_ctr);
      else
	{
	  hdr = make_file_info(r->filename, ss);
	  if (hdr)
	    {
	      fd = snd_open_read(ss, r->filename);
	      mus_file_open_descriptors(fd,
				       r->filename,
				       hdr->format,
				       mus_data_format_to_bytes_per_sample(hdr->format),
				       hdr->data_location,
				       hdr->chans,
				       hdr->type);
	      datai = make_file_state(fd, hdr, i, FILE_BUFFER_SIZE);
	      cp->sounds[0] = make_snd_data_file(r->filename, datai,
						 MUS_SAMPLE_ARRAY(datai[file_state_channel_offset(i)]),
						 hdr, DONT_DELETE_ME, cp->edit_ctr, i); /* don't auto-delete! */
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
	  make_region_readable(r, cp->state);
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
  int i, len;
  char *reg_buf;
  region *r;
  rs = (region_state *)CALLOC(1, sizeof(region_state));
  len = regions_size;
  for (i = 0; i < regions_size; i++) 
    if (!(regions[i])) 
      {
	len = i; 
	break;
      }
  rs->len = len;
  if (len == 0) return(rs);
  rs->save = (int *)CALLOC(len, sizeof(int));
  rs->name = (char **)CALLOC(len, sizeof(char *));
  for (i = 0; i < len; i++)
    {
      r = regions[i];
      rs->save[i] = r->save;
      reg_buf = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
      mus_snprintf(reg_buf, LABEL_BUFFER_SIZE, "%d: %s (%s:%s)", i, r->name, r->start, r->end);
      rs->name[i] = reg_buf;
    }
  return(rs);
}

void free_region_state (region_state *r)
{
  int i;
  if (r)
    {
      for (i = 0; i < r->len; i++)
	if (r->name[i]) 
	  FREE(r->name[i]);
      if (r->name) FREE(r->name);
      if (r->save) FREE(r->save);
      FREE(r);
    }
}

static SCM select_region_hook;

void select_region(int n) /* region browser and others indirectly */
{
  int i;
  region *r;
  if (region_ok(n))
    {
      r = regions[n];
      for (i = n; i > 0; i--) 
	regions[i] = regions[i - 1]; 
      regions[0] = r;
      if (HOOKED(select_region_hook))
	g_c_run_progn_hook(select_region_hook,
			   SCM_LIST1(TO_SCM_INT(r->id)),
			   S_select_region_hook);
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
      free_region(regions[n], COMPLETE_DELETION);
    }
  for (i = n; i < regions_size - 1; i++) 
    regions[i] = regions[i + 1]; 
  regions[regions_size - 1] = NULL;
  return(check_regions());
}

void protect_region(int n, int protect) /* region browser */
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
  int i, okr = -1;
  /* leave protected regions alone -- search for highest unprotected region */
  for (i = max_regions(ss) - 1; i >= 0; i--) 
    {
      if ((!(regions[i])) || 
	  (!((regions[i])->save))) 
	{
	  okr = i; 
	  break;
	}
    }
  if (okr == -1)
    {
      /* all possible slots are taken by protected regions! */
      okr = regions_size;
      set_max_regions(ss, regions_size * 2);
    }
  if (regions[okr]) 
    {
      stop_playing_region(okr);
      free_region(regions[okr], COMPLETE_DELETION);
    }
  for (i = okr; i > 0; i--) 
    regions[i] = regions[i - 1]; 
  regions[0] = r;
  if (!r) check_regions();
}

static int save_region_1(snd_state *ss, char *ofile, int type, int format, int srate, int reg, char *comment)
{
  int ofd, oloc, ifd, chans, i, frames, cursamples, iloc, comlen, err = 0;
  MUS_SAMPLE_TYPE **bufs;
  region *r;
  comlen = snd_strlen(comment);
  if (region_ok(reg)) r = regions[reg]; else r = NULL;
  if (r)
    {
      if ((snd_write_header(ss, ofile, type, srate, r->chans, 28, r->chans * r->frames, format, comment, comlen, NULL)) == -1)
	return(MUS_HEADER_WRITE_FAILED);
      oloc = mus_header_data_location();
      if ((ofd = snd_reopen_write(ss, ofile)) == -1) 
	return(MUS_CANT_OPEN_TEMP_FILE);
      mus_file_open_descriptors(ofd, ofile, format, 
			       mus_data_format_to_bytes_per_sample(format), 
			       oloc, r->chans, type);
      mus_file_set_data_clipped(ofd, data_clipped(ss));
      mus_file_seek(ofd, oloc, SEEK_SET);
      if (r->use_temp_file == REGION_ARRAY)
	mus_file_write(ofd, 0, r->frames - 1, r->chans, r->data); /* was * r->chans --> mus_file_write wants per channel size */
      else
	{
	  /* copy r->filename with possible header/data format changes */
	  if ((ifd = snd_open_read(ss, r->filename)) == -1) 
	    {
	      snd_error("can't find region %d data file %s: %s",
			reg, r->filename, 
			strerror(errno));
	      return(MUS_CANT_OPEN_TEMP_FILE);
	    }
	  chans = mus_sound_chans(r->filename);
	  frames = mus_sound_samples(r->filename) / chans;
	  iloc = mus_sound_data_location(r->filename);
	  mus_file_open_descriptors(ifd,
				   r->filename,
				   mus_sound_data_format(r->filename),
				   mus_sound_datum_size(r->filename),
				   iloc,
				   chans,
				   mus_sound_header_type(r->filename));
	  mus_file_seek(ifd, iloc, SEEK_SET);
	  bufs = (MUS_SAMPLE_TYPE **)CALLOC(chans, sizeof(MUS_SAMPLE_TYPE *));
	  for (i = 0; i < chans; i++) bufs[i] = (MUS_SAMPLE_TYPE *)CALLOC(FILE_BUFFER_SIZE, sizeof(MUS_SAMPLE_TYPE));

	  if (((frames * chans * mus_sound_datum_size(r->filename)) >> 10) > disk_kspace(ofd))
	    snd_warning("not enough space? -- need %d bytes to save region %d",
			frames * chans * mus_sound_datum_size(r->filename),
			reg);

	  for (i = 0; i < frames; i += FILE_BUFFER_SIZE)
	    {
	      if ((i + FILE_BUFFER_SIZE) < frames) 
		cursamples = FILE_BUFFER_SIZE; 
	      else cursamples = (frames-i);
	      mus_file_read(ifd, 0, cursamples - 1, chans, bufs);
	      err = mus_file_write(ofd, 0, cursamples - 1, chans, bufs);
	      if (err == -1) break; /* mus_file_write presumably posted an error message */
	      check_for_event(ss);  /* added 3-Jul-00 -- is this safe? */
	      if (ss->stopped_explicitly)
		{
		  ss->stopped_explicitly = 0;
		  snd_warning("save region %d stopped", reg);
		  break;
		}
	    }
	  if (mus_file_close(ifd) != 0)
	    snd_error("can't close %d (%s): %s! [%s[%d] %s]",
		      ifd, r->filename,
		      strerror(errno),
		      __FILE__, __LINE__, __FUNCTION__);
	  for (i = 0; i < chans; i++) FREE(bufs[i]);
	  FREE(bufs);
	}
      if (mus_file_close(ofd) != 0)
	snd_error("can't close %d (%s): %s! [%s[%d] %s]",
		  ofd, ofile,
		  strerror(errno),
		  __FILE__, __LINE__, __FUNCTION__);
      alert_new_file();
    }
  return(MUS_NO_ERROR);
}

int save_region(snd_state *ss, int n, char *ofile, int data_format)
{
  region *r;
  r = regions[n];
  if (data_format == MUS_UNKNOWN) data_format = MUS_OUT_FORMAT;
  if (!(mus_header_writable(r->header_type, data_format))) 
    {
      if (mus_header_writable(MUS_NEXT, data_format))
	r->header_type = MUS_NEXT;
      else
	{
	  if (mus_header_writable(MUS_RIFF, data_format))
	    r->header_type = MUS_RIFF;
	  else r->header_type = MUS_RAW;
	}
    }
  if (r) 
    return(save_region_1(ss, ofile, r->header_type, data_format, r->srate, n, "created by save-region in Snd"));
  return(0);
}

static int paste_region_1(int n, chan_info *cp, int add, int beg, const char *origin)
{
  region *r;
  int i, err = MUS_NO_ERROR, id = -1, idtmp;
  snd_info *sp;
  sync_info *si;
  chan_info *ncp;
  MUS_SAMPLE_TYPE *data = NULL;
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
	idtmp = mix_array(beg, r->frames, r->data, si->cps, r->chans, si->chans, SND_SRATE(sp), origin, with_mix_tags(ss));
      else idtmp = copy_file_and_mix(beg, r->frames, r->filename, si->cps, si->chans, origin, with_mix_tags(ss));
      if (id == -1) id = idtmp;
    }
  else
    {
      if (r->use_temp_file == REGION_FILE)
	{
	  tempfile = snd_tempnam(ss);
	  err = copy_file(r->filename, tempfile);
	  if (err != MUS_NO_ERROR)
	    snd_error("can't make region %d temp file (%s: %s)", n, tempfile, strerror(errno));
	  else
	    if (r->chans > 1) 
	      remember_temp(tempfile, r->chans);
	}
      for (i = 0; ((i < r->chans) && (i < si->chans)); i++)
	{
	  ncp = si->cps[i];                       /* currently syncd chan that we might paste to */
	  if (r->use_temp_file == REGION_ARRAY)
	    {
	      data = (MUS_SAMPLE_TYPE *)MALLOC(r->frames * sizeof(MUS_SAMPLE_TYPE));
	      memcpy((void *)data, (void *)(r->data[i]), r->frames * sizeof(MUS_SAMPLE_TYPE));
	      insert_samples(beg, r->frames, data, ncp, origin);
	      FREE(data);
	    }
	  else
	    {
	      if (err == MUS_NO_ERROR)
		file_insert_samples(beg, r->frames, tempfile, ncp, i,
				    (r->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
				    origin);
	    }
	  update_graph(si->cps[i], NULL);
	}
      if ((r->use_temp_file == REGION_FILE) && (tempfile)) FREE(tempfile);
    }
  if (si) si = free_sync_info(si);
  return(id);
}

void paste_region(int n, chan_info *cp, const char *origin) {paste_region_1(n, cp, FALSE, cp->cursor, origin);}
void add_region(int n, chan_info *cp, const char *origin) {paste_region_1(n, cp, TRUE, cp->cursor, origin);}
static int mix_region(int n, chan_info *cp, int beg) {return(paste_region_1(n, cp, TRUE, beg, S_mix_region));}

void region_stats(int *vals)
{
  int i, fil = 0, arr = 0;
  region *r;
  for (i = 0; i < regions_size; i++) 
    {
      r = regions[i];
      if (r)
	{
	  if (r->use_temp_file == REGION_FILE)
	    fil += (r->frames * r->chans * 2);
	  else arr += (r->frames * r->chans * 4);
	}
    }
  vals[0] = arr;
  vals[1] = fil;
}

void define_region(sync_info *si, int *ends)
{
  /* now look at all sync'd channels, collect them into the current region */
  /* we created the necessary pointers in create_selection above */
  int i, j, len, k, ofd = 0, datumb = 0, err = 0;
  MUS_SAMPLE_TYPE val, mval, curval;
  chan_info *cp0;
  snd_info *sp0;
  region *r;
  snd_fd **sfs;
  snd_state *ss;
  file_info *hdr = NULL;
  len = 0;
  for (i = 0; i < si->chans; i++)
    if (len < (ends[i] - si->begs[i]))
      len = ends[i] - si->begs[i];
  len += 1;
  if (len <= 0) return;
  r = (region *)CALLOC(1, sizeof(region));
  r->id = region_id_ctr++;
  cp0 = si->cps[0];
  sp0 = cp0->sound;
  ss = cp0->state;
  if (regions[0]) 
    stack_region(ss, r); 
  else regions[0] = r;
  r->header_type = (sp0->hdr)->type;
  r->srate = SND_SRATE(sp0);
  r->maxamp = 0.0;
  r->editor_copy = NULL;
  r->name = copy_string(sp0->shortname);
  r->chans = si->chans;
  r->data = (MUS_SAMPLE_TYPE **)CALLOC(r->chans, sizeof(MUS_SAMPLE_TYPE *));
  r->frames = len;
  val = MUS_SAMPLE_0; 
  mval = MUS_SAMPLE_0;
  r->start = prettyf((Float)si->begs[0] / (Float)(r->srate), 2);
  r->end = prettyf((Float)ends[0] / (Float)(r->srate), 2);
  sfs = (snd_fd **)CALLOC(r->chans, sizeof(snd_fd *));
  if (r->frames >= MAX_BUFFER_SIZE)
    {
      r->use_temp_file = REGION_FILE;
      r->filename = snd_tempnam(ss);
      hdr = make_temp_header(r->filename, r->srate, r->chans, 0);
      ofd = open_temp_file(r->filename, r->chans, hdr, ss);
      datumb = mus_data_format_to_bytes_per_sample(hdr->format);
    }
  else 
    {
      r->use_temp_file = REGION_ARRAY;
      r->filename = NULL;
    }
  for (i = 0; i < r->chans; i++)
    {
      sfs[i] = init_sample_read(si->begs[i], si->cps[i], READ_FORWARD);
      if (r->use_temp_file == REGION_ARRAY)
	r->data[i] = (MUS_SAMPLE_TYPE *)CALLOC(len, sizeof(MUS_SAMPLE_TYPE));
      else r->data[i] = (MUS_SAMPLE_TYPE *)CALLOC(MAX_BUFFER_SIZE, sizeof(MUS_SAMPLE_TYPE));
    }
  for (j = 0, k = 0; j < len; j++, k++) 
    {
      if (k == MAX_BUFFER_SIZE)
	{
	  err = mus_file_write(ofd, 0, k - 1, r->chans, r->data);
	  k = 0;
	  if (err == -1) break;
	}
      for (i = 0; i < r->chans; i++)
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
      if (k > 0) 
	mus_file_write(ofd, 0, k - 1, r->chans, r->data);
      close_temp_file(ofd, hdr, len * r->chans * datumb, sp0);
      hdr = free_file_info(hdr);
      for (i = 0; i < r->chans; i++) FREE(r->data[i]);
      FREE(r->data);
      r->data = NULL; /* filename only access in this case */
    }
  if (val < (-mval)) val = -mval;
  r->maxamp = MUS_SAMPLE_TO_FLOAT(val);
  for (i = 0; i < r->chans; i++) free_snd_fd(sfs[i]);
  FREE(sfs);
  reflect_regions_in_menu();
  if (region_browser_is_active()) update_region_browser(ss, 1);
}


snd_fd *init_region_read (snd_state *ss, int beg, int n, int chan, int direction)
{
  /* conjure up a reasonable looking ed list and sound list */
  region *r;
  snd_info *rsp;
  if (region_ok(n))
    {
      r = regions[n];
      make_region_readable(r, ss);
      if ((r) && (chan < r->chans))
	{
	  rsp = r->rsp;
	  if ((beg == 0) && 
	      (direction == READ_BACKWARD)) 
	    beg = r->frames - 1;
	  return(init_sample_read(beg, rsp->chans[chan], direction));
	}
    }
  return(NULL);
}

void cleanup_region_temp_files(void)
{ /* called upon exit to get rid of lingering region-related temp files */
  int i;
  region *r;
  for (i = 0; i < regions_size; i++)
    {
      r = regions[i];
      if ((r) && 
	  (r->use_temp_file == REGION_FILE) && 
	  (r->filename))
	{
	  snd_remove(r->filename);
	  r->filename = NULL;
	}
    }
}

int snd_regions(void)
{
  int i, num;
  num = 0;
  for (i = 0; i < regions_size; i++) 
    if (regions[i]) 
      num++;
  return(num);
}

/* (restore-region n chans len srate maxamp name start end #(chan-1 int-data ...)) */

void save_regions(snd_state *ss, FILE *fd)
{
  int i, j, k;
  region *r;
  char *newname;
  for (i = 0; i < regions_size; i++)
    {
      r = regions[i];
      if (r)
	{
	  fprintf(fd, "(%s %d %d %d %d %.4f \"%s\" \"%s\" \"%s\"",
	          S_restore_region, i, r->chans, r->frames, r->srate, r->maxamp, r->name, r->start, r->end);
	  if (r->use_temp_file == REGION_ARRAY)
	    {
	      fprintf(fd, "\n  #(");
	      for (j = 0; j < r->chans; j++)
		{
		  for (k = 0; k < r->frames; k++)
#if SNDLIB_USE_FLOATS
		    fprintf(fd, "%f ", r->data[j][k]);
#else
		    fprintf(fd, "%d ", r->data[j][k]);
#endif
		}
	      fprintf(fd, ")");
	    }
	  else /* file data */
	    {
	      if (save_dir(ss))
		{
		  newname = shorter_tempnam(save_dir(ss), "snd_save_");
		  copy_file(r->filename, newname);
		  fprintf(fd, " \"%s\"", newname);
		  FREE(newname);
		}
	      else
		{
		  /* read at very low level */
		  int ifd, iloc;
		  MUS_SAMPLE_TYPE **ibufs;
		  ifd = mus_file_open_read(r->filename);
		  iloc = mus_sound_data_location(r->filename);
		  mus_file_open_descriptors(ifd,
					   r->filename,
					   mus_sound_data_format(r->filename),
					   mus_sound_datum_size(r->filename),
					   iloc,
					   r->chans,
					   mus_sound_header_type(r->filename));
		  mus_file_seek(ifd, iloc, SEEK_SET);
		  ibufs = (MUS_SAMPLE_TYPE **)CALLOC(r->chans, sizeof(MUS_SAMPLE_TYPE *));
		  for (j = 0; j < r->chans; j++)
		    ibufs[j] = (MUS_SAMPLE_TYPE *)CALLOC(r->frames, sizeof(MUS_SAMPLE_TYPE));
		  mus_file_read(ifd, 0, r->frames - 1, r->chans, ibufs);
		  fprintf(fd, "\n  #(");
		  for (j = 0; j < r->chans; j++)
		    {
		      for (k = 0; k < r->frames; k++) 
#if SNDLIB_USE_FLOATS
			fprintf(fd, "%f ", ibufs[j][k]);
#else
			fprintf(fd, "%d ", ibufs[j][k]);
#endif
		    }
		  fprintf(fd, ")");
		  mus_file_close(ifd);
		  for (j = 0; j < r->chans; j++) FREE(ibufs[j]);
		  FREE(ibufs);
		}
	    }
	  fprintf(fd, ")\n");
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
	snd_error("region %d already being edited", reg);
      else
	{
	  temp_region_name = shorter_tempnam(temp_dir(ss), "region-");
	  if (r->use_temp_file == REGION_FILE)
	    err = copy_file(r->filename, temp_region_name);
	  else err = save_region(ss, reg, temp_region_name, MUS_OUT_FORMAT);
	  if (err == MUS_NO_ERROR)
	    {
	      sp = snd_open_file(temp_region_name, ss, FALSE);
	      if (sp)
		{
		  r->editor_copy = sp;
		  r->editor_name = copy_string(temp_region_name);
		  sp->edited_region = r;
		  /* save backpointer so subsequent save affects region if still legit */
		  /* also, since it's a temp file, if closed, delete temp */
		}
	      else snd_error("edit region: can't open region %d temp sound %s: %s!",
			     reg, temp_region_name, strerror(errno));
	    }
	  else 
	    snd_error("edit region: can't save region %d in temp file (%s: %s)",
		      reg, temp_region_name, strerror(errno));
	  FREE(temp_region_name);
	}
    }
  else snd_error("edit region: no region %d!", reg);
}

void clear_region_backpointer(snd_info *sp)
{
  region *r;
  if (sp->edited_region)
    {
      r = (region *)(sp->edited_region);
      if (r)
	{
	  snd_remove(r->editor_name);
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
  int i, err;
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
	  free_region(r, CLEAR_REGION_DATA);
	  r->use_temp_file = REGION_FILE;
	  r->maxamp = 0.0;
	  r->frames = current_ed_samples(sp->chans[0]);
	  for (i = 0; i < sp->nchans; i++)
	    {
	      val = get_maxamp(sp, sp->chans[i], AT_CURRENT_EDIT_POSITION);
	      if (val > r->maxamp) r->maxamp = val;
	    }
	  /* make new region temp file */
	  r->filename = snd_tempnam(ss);
	  err = copy_file(r->editor_name, r->filename);
	  if (err != MUS_NO_ERROR)
	    snd_error("can't make region temp file (%s: %s)", 
		      r->filename, 
		      strerror(errno));
	  make_region_readable(r, ss);
	  if (region_browser_is_active()) 
	    update_region_browser(ss, 1);
	}
    }
}

static SCM snd_no_such_region_error(const char *caller, SCM n)
{
  ERROR(NO_SUCH_REGION,
	SCM_LIST2(TO_SCM_STRING(caller),
		  n));
  return(SCM_BOOL_F);
}

static SCM g_restore_region(SCM n, SCM chans, SCM len, SCM srate, SCM maxamp, SCM name, SCM start, SCM end, SCM data)
{
  region *r;
  int i, j, k, regn;
  SCM *vdata;
  r = (region *)CALLOC(1, sizeof(region));
  regn = TO_SMALL_C_INT(n);
  regions[regn] = r;
  r->id = region_id_ctr++;
  r->maxamp = TO_C_DOUBLE(maxamp);
  r->chans = TO_SMALL_C_INT(chans);
  r->rsp = NULL;
  r->editor_copy = NULL;
  r->editor_name = NULL;
  r->frames = TO_C_INT(len);
  r->srate = TO_C_INT(srate);
  r->name = copy_string(TO_C_STRING(name));
  r->start = copy_string(TO_C_STRING(start));
  r->end = copy_string(TO_C_STRING(end));
  if (STRING_P(data))
    {
      r->use_temp_file = REGION_FILE;
      r->filename = copy_string(TO_C_STRING(data));
    }
  else 
    {
      r->use_temp_file = REGION_ARRAY;
      r->filename = NULL;
      r->data = (MUS_SAMPLE_TYPE **)CALLOC(r->chans, sizeof(MUS_SAMPLE_TYPE *));
      k = 0; 
      vdata = SCM_VELTS(data);
      for (i = 0; i < r->chans; i++)
	{
	  r->data[i] = (MUS_SAMPLE_TYPE *)CALLOC(r->frames, sizeof(MUS_SAMPLE_TYPE));
	  for (j = 0; j < r->frames; j++, k++)
	    {
#if SNDLIB_USE_FLOATS
	      r->data[i][j] = TO_C_DOUBLE(vdata[k]);
#else
	      r->data[i][j] = TO_C_INT(vdata[k]);
#endif
	    }
	}
    }
  reflect_regions_in_menu();
  return(TO_SCM_INT(region_id(regn)));
}

static SCM g_insert_region(SCM samp_n, SCM reg_n, SCM snd_n, SCM chn_n) /* opt reg_n */
{
  #define H_insert_region "("  S_insert_region " &optional (start-samp 0) (region 0) snd chn)\n\
inserts region data into snd's channel chn starting at 'start-samp'"

  chan_info *cp;
  int rg, samp;
  ASSERT_TYPE(NUMBER_IF_BOUND_P(samp_n), samp_n, SCM_ARG1, S_insert_region, "a number");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(reg_n), reg_n, SCM_ARG2, S_insert_region, "an integer");
  SND_ASSERT_CHAN(S_insert_region, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_insert_region);
  rg = TO_C_INT_OR_ELSE(reg_n, 0);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_insert_region, reg_n));
  samp = TO_C_INT_OR_ELSE(samp_n, 0);
  paste_region_1(rg, cp, FALSE, samp, S_insert_region);
  update_graph(cp, NULL);
  return(TO_SCM_INT(region_id(rg)));
}

static SCM g_max_regions(void) 
{
  #define H_max_regions "(" S_max_regions ") -> max number of regions saved on the region list"
  snd_state *ss;
  ss = get_global_state();
  return(TO_SCM_INT(max_regions(ss)));
}

static SCM g_set_max_regions(SCM n) 
{
  snd_state *ss;
  int regs;
  ASSERT_TYPE(INTEGER_P(n), n, SCM_ARGn, "set-" S_max_regions, "an integer"); 
  regs = TO_C_INT(n);
  if (regs < 0)
    mus_misc_error("set-" S_max_regions, "max_regions can't be < 0", n);
  ss = get_global_state();
  set_max_regions(ss, regs);
  return(TO_SCM_INT(max_regions(ss)));
}

enum {REGION_LENGTH, REGION_SRATE, REGION_CHANS, REGION_MAXAMP, REGION_SELECT, REGION_DELETE, REGION_PLAY, REGION_ID};

static SCM region_read(int field, SCM n, char *caller)
{
  int rg;
  int i;
  SCM res = SCM_EOL;
  if (SCM_EQ_P(n, SCM_BOOL_T)) /* can this happen? all callers appear to check for int_if_bound */
    {
      for (i = 0; i < regions_size; i++)
	if (regions[i])
	  res = CONS(region_read(field, TO_SCM_INT(i), caller), res);
      return(REVERSE_LIST(res));
    }
  else
    {
      rg = TO_C_INT_OR_ELSE_WITH_ORIGIN(n, 0, caller);
      if (!(region_ok(rg)))
	return(snd_no_such_region_error(caller, n));
      switch (field)
	{
	case REGION_LENGTH: return(TO_SCM_INT(region_len(rg))); break;
	case REGION_SRATE:  return(TO_SCM_INT(region_srate(rg))); break;
	case REGION_CHANS:  return(TO_SCM_INT(region_chans(rg))); break;
	case REGION_MAXAMP: return(TO_SCM_DOUBLE(region_maxamp(rg))); break;
	case REGION_SELECT: select_region_and_update_browser(get_global_state(), rg); return(n); break;
	case REGION_DELETE: delete_region_and_update_browser(get_global_state(), rg); return(n); break;
	case REGION_ID:     return(TO_SCM_INT(region_id(rg))); break;
	}
    }
  return(TO_SCM_INT(0));
}

static SCM g_regionQ(SCM n)
{
  #define H_regionQ "(" S_regionQ " reg) -> #t if region is active"
  ASSERT_TYPE(INTEGER_P(n), n, SCM_ARGn, S_regionQ, "an integer");
  return(TO_SCM_BOOLEAN(region_ok(TO_C_INT(n))));
}

static SCM g_region_length (SCM n) 
{
  #define H_region_length "(" S_region_length " &optional (n 0)) -> length in frames of region"
  ASSERT_TYPE(INTEGER_IF_BOUND_P(n), n, SCM_ARGn, S_region_length, "an integer");
  return(region_read(REGION_LENGTH, n, S_region_length));
}

static SCM g_region_srate (SCM n) 
{
  #define H_region_srate "(" S_region_srate " &optional (n 0)) -> srate of region n"
  ASSERT_TYPE(INTEGER_IF_BOUND_P(n), n, SCM_ARGn, S_region_srate, "an integer");
  return(region_read(REGION_SRATE, n, S_region_srate));
}

static SCM g_region_chans (SCM n) 
{
  #define H_region_chans "(" S_region_chans " &optional (n 0) -> channels of data in region n"
  ASSERT_TYPE(INTEGER_IF_BOUND_P(n), n, SCM_ARGn, S_region_chans, "an integer");
  return(region_read(REGION_CHANS, n, S_region_chans));
}

static SCM g_region_id (SCM n) 
{
  #define H_region_id "(" S_region_id " &optional (n 0) -> unique id of region n"
  ASSERT_TYPE(INTEGER_IF_BOUND_P(n), n, SCM_ARGn, S_region_id, "an integer");
  return(region_read(REGION_ID, n, S_region_id));
}

/* TODO: should id-region be id->region?? */
static SCM g_id_region (SCM n) 
{
  #define H_id_region "(" S_id_region " &optional (id 0) -> stack location of region with id"
  int sn;
  ASSERT_TYPE(INTEGER_IF_BOUND_P(n), n, SCM_ARGn, S_id_region, "an integer");
  sn = id_region(TO_C_INT_OR_ELSE(n, 0));
  if (sn == -1) 
    snd_no_such_region_error(S_id_region, n);
  return(TO_SCM_INT(sn));
}

static SCM g_region_maxamp (SCM n) 
{
  #define H_region_maxamp "(" S_region_maxamp " &optional (n 0)) -> max amp of region n"
  ASSERT_TYPE(INTEGER_IF_BOUND_P(n), n, SCM_ARGn, S_region_maxamp, "an integer");
  return(region_read(REGION_MAXAMP, n, S_region_maxamp));
}

static SCM g_select_region (SCM n) 
{
  #define H_select_region "(" S_select_region " &optional (n 0)) selects region n (moves it to the top of the region list)"
  ASSERT_TYPE(INTEGER_IF_BOUND_P(n), n, SCM_ARGn, S_select_region, "an integer");
  return(region_read(REGION_SELECT, n, S_select_region));
}

static SCM g_delete_region (SCM n) 
{
  #define H_delete_region "(" S_delete_region " &optional (n 0)) remove region n from the region list"
  ASSERT_TYPE(INTEGER_IF_BOUND_P(n), n, SCM_ARGn, S_delete_region, "an integer");
  return(region_read(REGION_DELETE, n, S_delete_region));
}

static SCM g_play_region (SCM n, SCM wait) 
{
  #define H_play_region "(" S_play_region " &optional (n 0) (wait #f)) play region n, if wait is #t, play to end before returning"
  int rg, wt = 0;
  ASSERT_TYPE(INTEGER_IF_BOUND_P(n), n, SCM_ARG1, S_play_region, "an integer");
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(wait), wait, SCM_ARG2, S_play_region, "a boolean");
  if (TRUE_P(wait)) wt = 1;
  rg = TO_C_INT_OR_ELSE(n, 0);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_play_region, n));
  play_region(get_global_state(), rg, !wt);
  return(n);
}

static SCM g_protect_region (SCM n, SCM protect) 
{
  #define H_protect_region "(" S_protect_region " &optional (n 0) (val #t))\n\
if val is #t protects region n from being pushed off the end of the region list"

  int rg;
  ASSERT_TYPE(INTEGER_P(n), n, SCM_ARG1, S_protect_region, "an integer");
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(protect), protect, SCM_ARG2, S_protect_region, "a boolean");
  rg = TO_C_INT(n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_protect_region, n));
  set_region_protect(rg, TO_C_BOOLEAN_OR_T(protect)); 
  return(protect);
}

static SCM g_regions(void) 
{
  #define H_regions "(" S_regions ") -> list of ids of regions currently in the region list"
  int i;
  SCM result;
  result = SCM_EOL;
  for (i=(regions_size-1); i >= 0; i--)
    if (regions[i])
      result = CONS(TO_SCM_INT(regions[i]->id), result);
  return(result);
}

static SCM g_make_region (SCM beg, SCM end, SCM snd_n, SCM chn_n)
{
  #define H_make_region "(" S_make_region " beg end &optional snd chn) makes a new region between beg and end in snd"
  chan_info *cp;
  sync_info *si;
  int ends[1];
  int ibeg;
  if (NOT_BOUND_P(beg))
    make_region_from_selection();
  else
    {
      ASSERT_TYPE(NUMBER_P(beg), beg, SCM_ARG1, S_make_region, "a number");
      ASSERT_TYPE(NUMBER_P(end), end, SCM_ARG2, S_make_region, "a number");
      cp = get_cp(snd_n, chn_n, S_make_region);
      ibeg = TO_C_INT_OR_ELSE(beg, 0);
      ends[0] = TO_C_INT_OR_ELSE(end, 0);
      if (current_ed_samples(cp) - 1 < ends[0]) 
	ends[0] = current_ed_samples(cp) - 1;
      if (ends[0] < ibeg) 
	ERROR(IMPOSSIBLE_BOUNDS,
	      SCM_LIST5(TO_SCM_STRING(S_make_region),
			beg, end,
			snd_n, chn_n));
      si = make_simple_sync(cp, ibeg);
      define_region(si, ends);
      reactivate_selection(si->cps[0], si->begs[0], ends[0]); /* ??? */
      si = free_sync_info(si);
    }
  return(TO_SCM_INT(region_id(0)));
}

static mus_error_handler_t *old_mus_error;

static void mus_local_error(int type, char *msg)
{
  mus_error_set_handler(old_mus_error);           /* make sure subsequent errors are handled by the default handler */
  ERROR(CANNOT_SAVE,
	SCM_LIST2(TO_SCM_STRING(S_save_sound_as),
		  TO_SCM_STRING(msg)));
}

static SCM g_save_region (SCM n, SCM filename, SCM format) 
{
  #define H_save_region "(" S_save_region " region filename &optional format) saves region in filename using data format (mus-bshort)"
  char *name = NULL;
  int res = MUS_NO_ERROR, rg;
  ASSERT_TYPE(INTEGER_P(n), n, SCM_ARG1, S_save_region, "an integer");
  ASSERT_TYPE(STRING_P(filename), filename, SCM_ARG2, S_save_region, "a string");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(format), format, SCM_ARG3, S_save_region, "an integer");
  rg = TO_C_INT(n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_save_region, n));
  name = mus_expand_filename(TO_C_STRING(filename));
  old_mus_error = mus_error_set_handler(mus_local_error);
  res = save_region(get_global_state(), rg, name, TO_C_INT_OR_ELSE(format, 0));
  mus_error_set_handler(old_mus_error);
  if (name) FREE(name);
  if (res != MUS_NO_ERROR)
    ERROR(CANNOT_SAVE,
	  SCM_LIST1(TO_SCM_STRING(S_save_region)));
  return(n);
}

static SCM g_mix_region(SCM chn_samp_n, SCM reg_n, SCM snd_n, SCM chn_n)
{
  #define H_mix_region "(" S_mix_region " &optional (chn-samp 0) (region 0) snd chn)\n\
mixes region into snd's channel chn starting at chn-samp; returns new mix id."

  chan_info *cp;
  int rg, id = -1;
  ASSERT_TYPE(NUMBER_IF_BOUND_P(chn_samp_n), chn_samp_n, SCM_ARG1, S_mix_region, "a number");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(reg_n), reg_n, SCM_ARG2, S_mix_region, "an integer");
  SND_ASSERT_CHAN(S_mix_region, snd_n, chn_n, 3);
  rg = TO_C_INT_OR_ELSE(reg_n, 0);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_mix_region, reg_n));
  cp = get_cp(snd_n, chn_n, S_mix_region);
  id = mix_region(rg, cp,
		  TO_C_INT_OR_ELSE(chn_samp_n, cp->cursor));
  return(TO_SCM_INT(id));
}

static SCM g_region_sample(SCM samp_n, SCM reg_n, SCM chn_n)
{
  #define H_region_sample "(" S_region_sample " &optional (samp 0) (region 0) (chan 0)) -> region's sample at samp in chan"

  int rg, chan;
  ASSERT_TYPE(NUMBER_IF_BOUND_P(samp_n), samp_n, SCM_ARG1, S_region_sample, "a number");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(reg_n), reg_n, SCM_ARG2, S_region_sample, "an integer");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(chn_n), chn_n, SCM_ARG3, S_region_sample, "an integer");
  chan = TO_C_INT_OR_ELSE(chn_n, 0);
  rg = TO_C_INT_OR_ELSE(reg_n, 0);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_region_sample, reg_n));
  if (chan < region_chans(rg))
    return(TO_SCM_DOUBLE(region_sample(rg, chan, TO_C_INT_OR_ELSE(samp_n, 0))));
  else snd_no_such_channel_error(S_region_sample, SCM_LIST1(reg_n), chn_n);
  return(samp_n);
}

static SCM g_region_samples(SCM beg_n, SCM num, SCM reg_n, SCM chn_n)
{
  #define H_region_samples "(" S_region_samples " &optional (beg 0) samps (region 0) (chan 0))\n\
returns a vector with region's samples starting at samp for samps from channel chan"

  SCM new_vect;
  SCM *vdata;
  Float *data;
  int len, reg, i, chn, beg;
  ASSERT_TYPE(NUMBER_IF_BOUND_P(beg_n), beg_n, SCM_ARG1, S_region_samples, "a number");
  ASSERT_TYPE(NUMBER_IF_BOUND_P(num), num, SCM_ARG2, S_region_samples, "a number");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(reg_n), reg_n, SCM_ARG3, S_region_samples, "an integer");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(chn_n), chn_n, SCM_ARG4, S_region_samples, "an integer");
  reg = TO_C_INT_OR_ELSE(reg_n, 0);
  if (!(region_ok(reg))) 
    return(snd_no_such_region_error(S_region_samples, reg_n));
  chn = TO_C_INT_OR_ELSE(chn_n, 0);
  if (chn < region_chans(reg))
    {
      len = TO_C_INT_OR_ELSE(num, 0);
      if (len == 0) len = region_len(reg);
      if (len > 0)
	{
	  beg = TO_C_INT_OR_ELSE(beg_n, 0);
	  if ((beg < 0) || (beg >= region_len(reg))) 
	    return(SCM_BOOL_F);
	  new_vect = MAKE_VECTOR(len, TO_SCM_DOUBLE(0.0));
	  vdata = SCM_VELTS(new_vect);
	  data = (Float *)CALLOC(len, sizeof(Float));
	  region_samples(reg, chn, beg, len, data);
	  for (i = 0; i < len; i++) 
	    vdata[i] = TO_SCM_DOUBLE(data[i]);
	  FREE(data);
	  return(new_vect);
	}
    }
  else snd_no_such_channel_error(S_region_samples, SCM_LIST1(reg_n), chn_n);
  return(SCM_BOOL_F);
}

#include "vct.h"

static SCM g_region_samples2vct(SCM beg_n, SCM num, SCM reg_n, SCM chn_n, SCM v)
{
  #define H_region_samples2vct "(" S_region_samples2vct " &optional (beg 0) samps (region 0) (chan 0) obj)\n\
writes region's samples starting at beg for samps in channel chan to vct obj, returning obj (or creating a new one)"

  Float *data;
  int len, reg, chn, beg;
  vct *v1 = get_vct(v);
  ASSERT_TYPE(NUMBER_IF_BOUND_P(beg_n), beg_n, SCM_ARG1, S_region_samples2vct, "a number");
  ASSERT_TYPE(NUMBER_IF_BOUND_P(num), num, SCM_ARG2, S_region_samples2vct, "a number");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(reg_n), reg_n, SCM_ARG3, S_region_samples2vct, "an integer");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(chn_n), chn_n, SCM_ARG4, S_region_samples2vct, "an integer");
  reg = TO_C_INT_OR_ELSE(reg_n, 0);
  if (!(region_ok(reg)))
    return(snd_no_such_region_error(S_region_samples2vct, reg_n));
  chn = TO_C_INT_OR_ELSE(chn_n, 0);
  if (chn >= region_chans(reg)) 
    return(snd_no_such_channel_error(S_region_samples2vct, SCM_LIST1(reg_n), chn_n));
  len = TO_C_INT_OR_ELSE(num, 0);
  if (len == 0) len = region_len(reg);
  if (len > 0)
    {
      beg = TO_C_INT_OR_ELSE(beg_n, 0);
      if ((beg < 0) || (beg >= region_len(reg))) 
	return(SCM_BOOL_F);
      if (v1)
	data = v1->data;
      else data = (Float *)CALLOC(len, sizeof(Float));
      region_samples(reg, chn, beg, len, data);
      if (v1)
	return(v);
      else return(make_vct(len, data));
    }
  return(SCM_BOOL_F);
}


void g_init_regions(SCM local_doc)
{
  DEFINE_PROC(S_restore_region,     g_restore_region, 9, 0, 0,     "restores a region");
  DEFINE_PROC(S_insert_region,      g_insert_region, 0, 4, 0,      H_insert_region);
  DEFINE_PROC(S_regions,            g_regions, 0, 0, 0,            H_regions);
  DEFINE_PROC(S_region_length,      g_region_length, 0, 1, 0,      H_region_length);
  DEFINE_PROC(S_region_srate,       g_region_srate, 0, 1, 0,       H_region_srate);
  DEFINE_PROC(S_region_chans,       g_region_chans, 0, 1, 0,       H_region_chans);
  DEFINE_PROC(S_region_id,          g_region_id, 0, 1, 0,          H_region_id);
  DEFINE_PROC(S_id_region,          g_id_region, 0, 1, 0,          H_id_region);
  DEFINE_PROC(S_region_maxamp,      g_region_maxamp, 0, 1, 0,      H_region_maxamp);
  DEFINE_PROC(S_save_region,        g_save_region, 2, 1, 0,        H_save_region);
  DEFINE_PROC(S_select_region,      g_select_region, 0, 1, 0,      H_select_region);
  DEFINE_PROC(S_delete_region,      g_delete_region, 0, 1, 0,      H_delete_region);
  DEFINE_PROC(S_protect_region,     g_protect_region, 2, 0, 0,     H_protect_region);
  DEFINE_PROC(S_play_region,        g_play_region, 0, 2, 0,        H_play_region);
  DEFINE_PROC(S_make_region,        g_make_region, 0, 4, 0,        H_make_region);
  DEFINE_PROC(S_mix_region,         g_mix_region, 0, 4, 0,         H_mix_region);
  DEFINE_PROC(S_region_sample,      g_region_sample, 0, 3, 0,      H_region_sample);
  DEFINE_PROC(S_region_samples,     g_region_samples, 0, 4, 0,     H_region_samples);
  DEFINE_PROC(S_region_samples2vct, g_region_samples2vct, 0, 5, 0, H_region_samples2vct);
  DEFINE_PROC(S_regionQ,            g_regionQ, 1, 0, 0,            H_regionQ);

  define_procedure_with_setter(S_max_regions, SCM_FNC g_max_regions, H_max_regions,
			       "set-" S_max_regions, SCM_FNC g_set_max_regions,
			       local_doc, 0, 0, 1, 0);

  #define H_select_region_hook S_select_region_hook " (id) is called when a region is selected. \
The hook function argument 'id' is the newly selected region's id."

  select_region_hook = MAKE_HOOK(S_select_region_hook, 1, H_select_region_hook); /* arg = newly selected region id */
}

