#include "snd.h"
#include "sndlib-strings.h"

#define REGION_FILE 1
#define REGION_DEFERRED 0
/* region data can be stored either in a temp file that is deleted when the region is deleted (hence must be copied upon insert or mix)
 *    or as a descriptor of current chan/beg/num/edpos locs.  The descriptor form is used until some use is made of the data
 *    that requires a file anyway (e.g. mixing), or if the data the descriptor depends on is about to be flushed (e.g. the
 *    underlying edit list is about to be cleared, or the file is being closed, etc).
 */

#define CLEAR_REGION_DATA 0
#define COMPLETE_DELETION 1

static int region_id_ctr = 0;

typedef struct {
  int chans;
  off_t len;
  chan_info **cps;
  off_t *begs, *lens;
  int *edpos;
} deferred_region;

static deferred_region *free_deferred_region(deferred_region *dr)
{
  if (dr)
    {
      if (dr->cps) FREE(dr->cps);
      if (dr->begs) FREE(dr->begs);
      if (dr->lens) FREE(dr->lens);
      if (dr->edpos) FREE(dr->edpos);
      FREE(dr);
    }
  return(NULL);
}

typedef struct {
  int chans;
  off_t frames;
  int srate;                /* for file save (i.e. region->file) */
  int header_type;          /* for file save */
  int save;
  snd_info *rsp;
  char *name, *start, *end; /* for region browser */
  char *filename;           /* if region data is stored in a temp file */
  int use_temp_file;        /* REGION_FILE = in temp file 'filename', REGION_DEFERRED = in 'dr' */
  Float maxamp;
  snd_info *editor_copy;
  char *editor_name;
  int id;
  deferred_region *dr;      /* REGION_DEFERRED descriptor */
  env_info **amp_envs;
} region;

static void deferred_region_to_temp_file(region *r);

static void free_region(region *r, int complete)
{
  snd_info *sp;
  int i;
  /* if not complete, just clear out old data (edited region being saved) */
  if (r)
    {
      if ((complete == COMPLETE_DELETION) && (r->editor_copy))
	{
	  sp = r->editor_copy; 
	  sp->edited_region = NULL;
	  r->editor_copy = NULL;
	}
      if (complete == COMPLETE_DELETION)
	{
	  if (r->name) FREE(r->name);
	  if (r->start) FREE(r->start);
	  if (r->end) FREE(r->end);
	  if (r->amp_envs)
	    {
	      for (i = 0; i < r->chans; i++)
		if (r->amp_envs[i]) 
		  r->amp_envs[i] = free_env_info(r->amp_envs[i]);
	      FREE(r->amp_envs);
	      r->amp_envs = NULL;
	    }
	}
      if (r->use_temp_file == REGION_FILE) /* we can delete this temp file because all references copy first */
	{
	  if (r->filename)
	    {
	      snd_remove(r->filename, TRUE);
	      FREE(r->filename);
	    }
	  r->filename = NULL;
	}
      if (r->use_temp_file == REGION_DEFERRED)
	r->dr = free_deferred_region(r->dr);
      if (r->rsp) 
	r->rsp = completely_free_snd_info(r->rsp);
      if (complete == COMPLETE_DELETION) FREE(r);
    }
}

static region **regions = NULL;
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
      allocate_region_rows(n);
      in_set_max_regions(ss, n);
    }
}

int id_to_stack_position(int id)
{
  int i;
  if ((id >= 0) && (id < region_id_ctr))
    for (i = 0; i < regions_size; i++)
      if ((regions[i]) && 
	  (regions[i]->id == id))
	return(i);
  return(INVALID_REGION);
}

int stack_position_to_id(int n) 
{
  if ((n >= 0) && 
      (n < regions_size) &&
      (regions[n]))
    return(regions[n]->id);
  return(INVALID_REGION);
}

static region *id_to_region(int id)
{
  int i;
  if ((id >= 0) && (id < region_id_ctr))
    for (i = 0; i < regions_size; i++)
      if ((regions[i]) && 
	  (regions[i]->id == id))
	return(regions[i]);
  return(NULL);
}

int region_ok(int id) 
{
  return(id_to_region(id) != NULL);
}

off_t region_len(int n) 
{
  region *r;
  r = id_to_region(n);
  if (r) 
    return(r->frames); 
  return(0);
}

int region_chans(int n) 
{  
  region *r;
  r = id_to_region(n);
  if (r) 
    return(r->chans); 
  return(0);
}

int region_srate(int n) 
{  
  region *r;
  r = id_to_region(n);
  if (r) 
    return(r->srate); 
  return(0);
}

Float region_maxamp(int n) 
{
  region *r;
  r = id_to_region(n);
  if (r)
    {
      if ((r->maxamp < 0.0) && (r->use_temp_file == REGION_DEFERRED))
	deferred_region_to_temp_file(r);
      return(r->maxamp); 
    }
  return(0.0);
}

static Float region_sample(int reg, int chn, off_t samp)
{
  region *r;
  snd_fd *sf;
  Float val;
  deferred_region *drp;
  r = id_to_region(reg);
  if (r)
    {
      if ((samp < r->frames) && (chn < r->chans)) 
	{
	  switch (r->use_temp_file)
	    {
	    case REGION_FILE:
	      sf = init_region_read(samp, reg, chn, READ_FORWARD);
	      val = read_sample_to_float(sf);
	      free_snd_fd(sf);
	      return(val);
	    case REGION_DEFERRED:
	      drp = r->dr;
	      return(chn_sample(samp + drp->begs[chn], drp->cps[chn], drp->edpos[chn]));
	      break;
	    }
	}
    }
  return(0.0);
}

static void region_samples(int reg, int chn, off_t beg, off_t num, Float *data)
{
  region *r;
  snd_fd *sf;
  off_t i, j = 0;
  deferred_region *drp;
  r = id_to_region(reg);
  if (r)
    {
      if ((beg < r->frames) && (chn < r->chans))
	{
	  switch (r->use_temp_file)
	    {
	    case REGION_FILE:
	      sf = init_region_read(beg, reg, chn, READ_FORWARD);
	      for (i = beg, j = 0; (i < r->frames) && (j < num); i++, j++) 
		data[j] = read_sample_to_float(sf);
	      free_snd_fd(sf);
	      break;
	    case REGION_DEFERRED:
	      drp = r->dr;
	      sf = init_sample_read_any(beg + drp->begs[chn], drp->cps[chn], READ_FORWARD, drp->edpos[chn]);
	      for (i = beg, j = 0; (i < r->frames) && (j < num); i++, j++) 
		data[j] = read_sample_to_float(sf);
	      free_snd_fd(sf);
	      break;
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
  if (act == NO_REGIONS) 
    {
      reflect_no_regions_in_menu();
      reflect_no_regions_in_region_browser();
    }
  return(act);
}

snd_info *make_initial_region_sp(snd_state *ss, widget_t region_grf)
{
  int id;
  snd_info *reg_sp;
  chan_info *cp;
  file_info *hdr;
  id = stack_position_to_id(0);
  reg_sp = make_basic_snd_info(1);
  reg_sp->nchans = 1;
  reg_sp->inuse = TRUE;
  reg_sp->active = TRUE;
  reg_sp->hdr = (file_info *)CALLOC(1, sizeof(file_info));
  reg_sp->search_proc = XEN_UNDEFINED;
  reg_sp->prompt_callback = XEN_UNDEFINED;
  hdr = reg_sp->hdr;
  hdr->samples = region_len(id);
  hdr->srate = region_srate(id);
  hdr->comment = NULL;
  hdr->chans = 1;
  add_channel_window(reg_sp, 0, ss, 0, 0, region_grf, WITH_ARROWS);
  cp = reg_sp->chans[0];
  cp->sound = reg_sp;
  cp->edit_size = 1;
  cp->edit_ctr = 0;
  allocate_ed_list(cp);
  cp->samples = (off_t *)CALLOC(cp->edit_size, sizeof(off_t));
  cp->cursors = (off_t *)CALLOC(cp->edit_size, sizeof(off_t));
  cp->sound_size = 1;
  cp->sound_ctr = 0;
  cp->sounds = (snd_data **)CALLOC(cp->sound_size, sizeof(snd_data *));
  cp->samples[0] = region_len(id);
  cp->time_graph_style = region_graph_style(ss); /* added 8-Aug-01 */
  cp->dot_size = dot_size(ss);
  return(reg_sp);
}

static void make_region_readable(region *r)
{
  snd_info *regsp;
  chan_info *cp;
  file_info *hdr;
  snd_io *io;
  int i, fd;
  snd_state *ss;

  if (r->use_temp_file == REGION_DEFERRED) 
    deferred_region_to_temp_file(r);

  if (r->rsp) return;

  ss = get_global_state();
  regsp = make_basic_snd_info(r->chans);
  regsp->nchans = r->chans;
  regsp->hdr = (file_info *)CALLOC(1, sizeof(file_info));
  regsp->search_proc = XEN_UNDEFINED;
  regsp->prompt_callback = XEN_UNDEFINED;
  hdr = regsp->hdr;
  hdr->samples = r->frames * r->chans;
  hdr->srate = r->srate;
  hdr->chans = r->chans;
  hdr->comment = NULL;
  for (i = 0; i < r->chans; i++)
    {
      cp = make_chan_info(NULL, i, regsp, ss);
      regsp->chans[i] = cp;
      add_channel_data_1(cp, regsp, WITHOUT_GRAPH);
      cp->edits[0] = initial_ed_list(0, r->frames - 1);
      cp->edit_size = 1;
      cp->sound_size = 1;
      cp->hookable = FALSE;

      ss->catch_message = NULL;
      hdr = make_file_info(r->filename, ss);
      if (hdr)
	{
	  fd = snd_open_read(ss, r->filename);
	  mus_file_open_descriptors(fd,
				    r->filename,
				    hdr->format,
				    mus_bytes_per_sample(hdr->format),
				    hdr->data_location,
				    hdr->chans,
				    hdr->type);
	  io = make_file_state(fd, hdr, i, FILE_BUFFER_SIZE);
	  cp->sounds[0] = make_snd_data_file(r->filename, io, hdr, DONT_DELETE_ME, cp->edit_ctr, i); /* don't auto-delete! */
	}
      else
	{
	  XEN_ERROR(MUS_MISC_ERROR,
		    XEN_LIST_2(C_TO_XEN_STRING(_("can't read region file!!")),
			       C_TO_XEN_STRING(ss->catch_message)));
	}
    }
  r->rsp = regsp;
}

file_info *fixup_region_data(chan_info *cp, int chan, int pos)
{
  /* for region browser labels */
  region *r;
  snd_info *nsp;
  chan_info *ncp;
  if ((pos >= 0) && 
      (pos < regions_size) &&
      (regions[pos]))
    {
      r = regions[pos];
      if (chan < r->chans)
	{
	  make_region_readable(r);
	  nsp = r->rsp;
	  ncp = nsp->chans[chan];
	  cp->sounds = ncp->sounds;
	  cp->sound_size = ncp->sound_size;
	  cp->edits = ncp->edits; /* ?? is this safe ?? */
	  cp->edit_size = ncp->edit_size;
	  cp->edit_ctr = ncp->edit_ctr;
	  cp->samples[0] = ncp->samples[0];
	  cp->axis = ncp->axis;
	  if ((r->amp_envs) && (r->amp_envs[chan]))
	    {
	      if (cp->amp_envs == NULL)
		cp->amp_envs = (env_info **)CALLOC(cp->edit_size, sizeof(env_info *));
	      cp->amp_envs[0] = r->amp_envs[chan];
	    }
	  else
	    {
	      if ((cp->amp_envs) && (cp->amp_envs[0]))
		cp->amp_envs[0] = NULL;
	    }
	  initialize_scrollbars(cp);
	  return(nsp->hdr);
	}
    }
  return(NULL);
}

void for_each_region_chan(void (*func)(chan_info *, void *), void *userptr)
{
  /* used only in snd-io.c to remove dangling temp files (probably can't actually happen) */
  int i, chn;
  region *r;
  chan_info *cp;
  for (i = 0; i < regions_size; i++)
    {
      r = regions[i];
      if ((r) && (r->rsp) && (r->use_temp_file == REGION_FILE))
	for (chn = 0; chn < r->chans; chn++)
	  {
	    cp = r->rsp->chans[chn];
	    (*func)(cp, userptr);
	  }
    }
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
      mus_snprintf(reg_buf, LABEL_BUFFER_SIZE, "%d: %s (%s:%s)", r->id, r->name, r->start, r->end);
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

int remove_region_from_stack(int pos) /* region browser */
{
  int i, id;
  id = stack_position_to_id(pos);
  if (id == INVALID_REGION) return(INVALID_REGION);
  stop_playing_region(id);
  free_region(id_to_region(id), COMPLETE_DELETION);
  for (i = pos; i < regions_size - 1; i++) 
    regions[i] = regions[i + 1]; 
  regions[regions_size - 1] = NULL;
  return(check_regions());
}

void protect_region(int pos, int protect) /* region browser */
{
  if ((pos >= 0) && 
      (pos < regions_size) &&
      (regions[pos]))
    regions[pos]->save = protect;
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
      stop_playing_region(regions[okr]->id);
      free_region(regions[okr], COMPLETE_DELETION);
    }
  for (i = okr; i > 0; i--) 
    regions[i] = regions[i - 1]; 
  regions[0] = r;
  if (!r) check_regions();
}

static int save_region_1(snd_state *ss, char *ofile, int type, int format, int srate, int reg, char *comment)
{
  int ofd, ifd, chans, i, comlen, err = 0;
  off_t oloc, iloc, ioff, frames, cursamples;
  mus_sample_t **bufs;
  region *r;
  comlen = snd_strlen(comment);
  r = id_to_region(reg);
  if (r)
    {
      if (r->use_temp_file == REGION_DEFERRED) 
	deferred_region_to_temp_file(r);

      if ((snd_write_header(ss, ofile, type, srate, r->chans, 28, r->chans * r->frames, format, comment, comlen, NULL)) == -1)
	return(MUS_HEADER_WRITE_FAILED);
      oloc = mus_header_data_location();
      if ((ofd = snd_reopen_write(ss, ofile)) == -1) 
	return(MUS_CANT_OPEN_TEMP_FILE);
      mus_file_open_descriptors(ofd, ofile, format, 
				mus_bytes_per_sample(format), 
				oloc, r->chans, type);
      mus_file_set_data_clipped(ofd, data_clipped(ss));
      lseek(ofd, oloc, SEEK_SET);

      /* copy r->filename with possible header/data format changes */
      if ((ifd = snd_open_read(ss, r->filename)) == -1) 
	{
	  snd_error(_("can't find region %d data file %s: %s"),
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
      lseek(ifd, iloc, SEEK_SET);
      bufs = (mus_sample_t **)CALLOC(chans, sizeof(mus_sample_t *));
      for (i = 0; i < chans; i++) bufs[i] = (mus_sample_t *)CALLOC(FILE_BUFFER_SIZE, sizeof(mus_sample_t));
      
      if (((frames * chans * mus_sound_datum_size(r->filename)) >> 10) > disk_kspace(ofile))
	snd_warning(_("not enough space to save region? -- need " PRId64 " bytes"),
		    frames * chans * mus_sound_datum_size(r->filename));
      for (ioff = 0; ioff < frames; ioff += FILE_BUFFER_SIZE)
	{
	  if ((ioff + FILE_BUFFER_SIZE) < frames) 
	    cursamples = FILE_BUFFER_SIZE; 
	  else cursamples = (frames - ioff);
	  mus_file_read(ifd, 0, cursamples - 1, chans, bufs);
	  err = mus_file_write(ofd, 0, cursamples - 1, chans, bufs);
	  if (err == -1) break; /* mus_file_write presumably posted an error message */
	}
      if (mus_file_close(ifd) != 0)
	snd_error(_("save-region: can't close %s: %s!"), r->filename, strerror(errno));
      for (i = 0; i < chans; i++) FREE(bufs[i]);
      FREE(bufs);
      if (mus_file_close(ofd) != 0)
	snd_error(_("save-region: can't close %s: %s!"), ofile, strerror(errno));
      alert_new_file();
    }
  return(MUS_NO_ERROR);
}

int save_region(snd_state *ss, int n, char *ofile, int data_format)
{
  /* called only in snd-kbd.c */
  region *r;
  r = id_to_region(n);
  if (r)
    {
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
      return(save_region_1(ss, ofile, r->header_type, data_format, r->srate, n, _("created by save-region in Snd")));
    }
  return(INVALID_REGION);
}

static int paste_region_1(int n, chan_info *cp, int add, off_t beg, const char *origin)
{
  region *r;
  int i, err = MUS_NO_ERROR, id = -1;
  sync_info *si;
  chan_info *ncp;
  snd_state *ss;
  char *tempfile = NULL;
  ss = cp->state;
  si = NULL;
  r = id_to_region(n);
  if ((r == NULL) || (r->frames == 0)) return(INVALID_REGION);

  if (r->use_temp_file == REGION_DEFERRED)
    deferred_region_to_temp_file(r);

  si = sync_to_chan(cp);
  if (add)
    {
      char *newname;
      newname = shorter_tempnam(temp_dir(ss), "snd_");
      err = copy_file(r->filename, newname);
      if (err != MUS_NO_ERROR)
	snd_error(_("can't save mix temp file (%s: %s)"), newname, strerror(errno));
      else id = mix(beg, r->frames, si->chans, si->cps, newname, DELETE_ME, origin, with_mix_tags(ss));
      if (newname) FREE(newname);
    }
  else
    {
      if (r->use_temp_file == REGION_FILE)
	{
	  tempfile = snd_tempnam(ss);
	  err = copy_file(r->filename, tempfile);
	  if (err != MUS_NO_ERROR)
	    {
	      snd_error(_("can't make region %d temp file (%s: %s)"), n, tempfile, strerror(errno));
	      if (si) si = free_sync_info(si);
	      return(INVALID_REGION);
	    }
	  else
	    if (r->chans > 1) 
	      remember_temp(tempfile, r->chans);
	}
      for (i = 0; ((i < r->chans) && (i < si->chans)); i++)
	{
	  ncp = si->cps[i];                       /* currently syncd chan that we might paste to */
	  file_insert_samples(beg, r->frames, tempfile, ncp, i,
			      (r->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
			      origin, ncp->edit_ctr);
	  update_graph(si->cps[i]);
	}
      if ((r->use_temp_file == REGION_FILE) && (tempfile)) FREE(tempfile);
    }
  if (si) si = free_sync_info(si);
  return(id);
}

void paste_region(int n, chan_info *cp, const char *origin) {paste_region_1(n, cp, FALSE, CURSOR(cp), origin);}
void add_region(int n, chan_info *cp, const char *origin) {paste_region_1(n, cp, TRUE, CURSOR(cp), origin);}

int define_region(sync_info *si, off_t *ends)
{
  /* now look at all sync'd channels, collect them into the current region */
  /* we created the necessary pointers in create_selection above */
  int i;
  off_t len;
  chan_info *cp0;
  snd_info *sp0;
  region *r;
  snd_state *ss;
  env_info *ep;
  deferred_region *drp;
  len = 0;
  for (i = 0; i < si->chans; i++)
    if (len < (ends[i] - si->begs[i]))
      len = ends[i] - si->begs[i];
  len += 1;
  if (len <= 0) return(INVALID_REGION);
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
  r->maxamp = -1.0;
  r->editor_copy = NULL;
  r->name = copy_string(sp0->short_filename);
  r->chans = si->chans;
  r->frames = len;
  r->start = prettyf((Float)((double)(si->begs[0]) / (Float)(r->srate)), 2);
  r->end = prettyf((Float)((double)(ends[0]) / (Float)(r->srate)), 2);
  r->use_temp_file = REGION_DEFERRED;
  ss->deferred_regions++;
  r->dr = (deferred_region *)CALLOC(1, sizeof(deferred_region));
  drp = r->dr;
  drp->chans = si->chans;
  drp->cps = (chan_info **)CALLOC(drp->chans, sizeof(chan_info *));
  drp->begs = (off_t *)CALLOC(drp->chans, sizeof(off_t));
  drp->lens = (off_t *)CALLOC(drp->chans, sizeof(off_t));
  drp->edpos = (int *)CALLOC(drp->chans, sizeof(int));
  drp->len = len;
  for (i = 0; i < drp->chans; i++)
    {
      drp->cps[i] = si->cps[i];
      drp->begs[i] = si->begs[i];
      drp->lens[i] = ends[i] - si->begs[i];
      drp->edpos[i] = drp->cps[i]->edit_ctr;
      if ((drp->lens[i] > AMP_ENV_CUTOFF) &&
	  (drp->cps[i]->amp_envs))
	{
	  ep = drp->cps[i]->amp_envs[drp->edpos[i]];
	  if ((ep) && (ep->completed))
	    {
	      if (r->amp_envs == NULL)
		r->amp_envs = (env_info **)CALLOC(r->chans, sizeof(env_info *));
	      r->amp_envs[i] = amp_env_section(drp->cps[i], drp->begs[i], drp->lens[i], drp->edpos[i]);
	    }
	}
    }
  reflect_regions_in_menu();
  reflect_regions_in_region_browser();
  if (region_browser_is_active()) update_region_browser(ss, 1);
  return(r->id);
}

static void deferred_region_to_temp_file(region *r)
{
  int i, k, ofd = 0, datumb = 0, err = 0, copy_ok;
  off_t j, len = 0;
  mus_sample_t val, curval;
  snd_fd **sfs = NULL;
  snd_state *ss = NULL;
  snd_info *sp0;
  file_info *hdr = NULL;
  deferred_region *drp = NULL;
  mus_sample_t **data = NULL;
  
  ss = get_global_state();
  ss->deferred_regions--;
  drp = r->dr;
  len = drp->len;
  val = MUS_SAMPLE_0; 

  r->use_temp_file = REGION_FILE;
  r->filename = snd_tempnam(ss);

  sp0 = drp->cps[0]->sound;
  copy_ok = ((mus_header_writable(MUS_NEXT, sp0->hdr->format)) && 
	     (r->chans == sp0->nchans) &&
	     (r->amp_envs != NULL) &&
	     ((drp->len - 1) == drp->lens[0]));
  if (copy_ok)
    for (i = 0; i < r->chans; i++)
      if ((drp->edpos[i] != 0) || 
	  (drp->cps[i]->sound != sp0) ||
	  (drp->begs[i] != drp->begs[0]) ||
	  (drp->lens[i] != (drp->len - 1)) ||
	  (r->amp_envs[i] == NULL))
	{
	  copy_ok = 0;
	  break;
	}

  if (copy_ok)
    {
      /* write next header with correct len
       * seek loc in sp0->filename (drp->begs[0])
       * copy len*data-size bytes
       * get max from amp envs
       */
      off_t data_size, bytes;
      int fdi, fdo;
      char *buffer;
      mus_sample_t ymax = MUS_SAMPLE_0;
      env_info *ep;
      datumb = mus_bytes_per_sample(sp0->hdr->format);
      data_size = drp->len * r->chans * datumb;
      fdo = mus_file_create(r->filename);
      if (fdo == -1)
	snd_error(_("can't write region temp file %s: %s"), r->filename, strerror(errno));
      else
	{
	  mus_header_write_next_header(fdo, r->srate, r->chans, 28, data_size, sp0->hdr->format, "region deferred temp", 20);
	  fdi = mus_file_open_read(sp0->filename);
	  if (fdi == -1)
	    snd_error(_("can't read region's original sound? %s: %s"), sp0->filename, strerror(errno));
	  else
	    {
	      lseek(fdi, 28 + r->chans * datumb * drp->begs[0], SEEK_SET);
	      buffer = (char *)CALLOC(8192, sizeof(char));
	      for (j = 0; j < data_size; j += 8192)
		{
		  bytes = data_size - j;
		  if (bytes > 8192) bytes = 8192;
		  read(fdi, buffer, bytes);
		  write(fdo, buffer, bytes);
		}
	      FREE(buffer);
	      snd_close(fdi, sp0->filename);
	      for (i = 0; i < r->chans; i++)
		{
		  ep = r->amp_envs[i];
		  if (ymax < ep->fmax) 
		    ymax = ep->fmax;
		  if (ymax < -ep->fmin)
		    ymax = -ep->fmin;
		}
	      r->maxamp = MUS_SAMPLE_TO_FLOAT(ymax);
	    }
	  snd_close(fdo, r->filename);
	}
    }
  else
    {
      hdr = make_temp_header(r->filename, r->srate, r->chans, 0, (char *)__FUNCTION__);
      ofd = open_temp_file(r->filename, r->chans, hdr, ss);
      if (ofd == -1)
	snd_error(_("can't write region temp file %s: %s"), r->filename, strerror(errno));
      else
	{
	  sfs = (snd_fd **)CALLOC(r->chans, sizeof(snd_fd *));
	  data = (mus_sample_t **)CALLOC(r->chans, sizeof(mus_sample_t *));
	  datumb = mus_bytes_per_sample(hdr->format);

	  /* here if amp_envs, maxamp exists */
	  for (i = 0; i < r->chans; i++)
	    {
	      sfs[i] = init_sample_read_any(drp->begs[i], drp->cps[i], READ_FORWARD, drp->edpos[i]);
	      data[i] = (mus_sample_t *)CALLOC(MAX_BUFFER_SIZE, sizeof(mus_sample_t));
	    }
	  for (j = 0, k = 0; j < len; j++, k++) 
	    {
	      if (k == MAX_BUFFER_SIZE)
		{
		  err = mus_file_write(ofd, 0, k - 1, r->chans, data);
		  k = 0;
		  if (err == -1) break;
		}
	      for (i = 0; i < r->chans; i++)
		{
		  if (j <= drp->lens[i])  /* ??? was < ends[i] */
		    {
		      data[i][k] = read_sample(sfs[i]);
		      curval = mus_sample_abs(data[i][k]);
		      if (curval > val) val = curval;
		    }
		  else data[i][k] = MUS_SAMPLE_0;
		}
	    }
	  if (k > 0) 
	    mus_file_write(ofd, 0, k - 1, r->chans, data);
	  close_temp_file(ofd, hdr, len * r->chans * datumb, drp->cps[0]->sound);
	  r->maxamp = MUS_SAMPLE_TO_FLOAT(val);
	  for (i = 0; i < r->chans; i++) FREE(data[i]);
	  for (i = 0; i < r->chans; i++) free_snd_fd(sfs[i]);
	  FREE(sfs);
	  FREE(data);
	  data = NULL;
	}
      hdr = free_file_info(hdr);
    }
  r->dr = free_deferred_region(r->dr);
}

void sequester_deferred_regions(chan_info *cp, int edit_top)
{
  region *r;
  deferred_region *drp;
  int i, j;
  for (i = 0; i < regions_size; i++)
    {
      r = regions[i];
      if ((r) && (r->use_temp_file == REGION_DEFERRED))
	{
	  drp = r->dr;
	  for (j = 0; j < drp->chans; j++)
	    if ((drp->cps[j] == cp) &&
		(drp->edpos[j] > edit_top))
	      {
		report_in_minibuffer(cp->sound, _("sequestering region %d..."), r->id);
		deferred_region_to_temp_file(r);
		clear_minibuffer(cp->sound);
		break;
	      }
	}
    }
}

snd_fd *init_region_read (off_t beg, int n, int chan, int direction)
{
  /* conjure up a reasonable looking ed list and sound list */
  region *r;
  deferred_region *drp;
  r = id_to_region(n);
  if ((r) && (chan < r->chans))
    {
      if ((beg == 0) && 
	  (direction == READ_BACKWARD)) 
	beg = r->frames - 1;
      if (r->use_temp_file == REGION_DEFERRED)
	{
	  drp = r->dr;
	  return(init_sample_read_any(drp->begs[chan] + beg, drp->cps[chan], direction, drp->edpos[chan]));
	}
      else
	{
	  make_region_readable(r);
	  return(init_sample_read(beg, r->rsp->chans[chan], direction));
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
	  snd_remove(r->filename, TRUE);
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

/* (restore-region n chans len srate maxamp name start end filename) */

void save_regions(snd_state *ss, FILE *fd)
{
  int i;
  region *r;
  char *newname;
  for (i = 0; i < regions_size; i++)
    {
      r = regions[i];
      if (r)
	{
#if HAVE_RUBY
	  fprintf(fd, "%s(%d, %d, " OFF_TD ", %d, %.4f, \"%s\", \"%s\", \"%s\", ",
	          "restore_region", i, r->chans, r->frames, r->srate, r->maxamp, r->name, r->start, r->end);
#else
	  fprintf(fd, "(%s %d %d " OFF_TD " %d %.4f \"%s\" \"%s\" \"%s\"",
	          S_restore_region, i, r->chans, r->frames, r->srate, r->maxamp, r->name, r->start, r->end);
#endif

	  if (r->use_temp_file == REGION_DEFERRED) 
	    deferred_region_to_temp_file(r);

	  newname = run_save_state_hook(shorter_tempnam(save_dir(ss), "snd_save_"));
	  copy_file(r->filename, newname);
	  fprintf(fd, " \"%s\")\n", newname);
	  FREE(newname);
	}
    }
}

void region_edit(snd_state *ss, int pos)
{
  /* from region browser:
   *   load region into temp file, load that into snd editor,
   *   if 'save', save temp file and update region (browser also) (cancelling active display if any)
   *   while editing, if delete in browser, cut backpointer in editor and signal it
   */
  char *temp_region_name;
  snd_info *sp;
  int err;
  region *r = NULL;
  if ((pos >= 0) && 
      (pos < regions_size) &&
      (regions[pos]))
    r = regions[pos];
  if (r) 
    {
      if (r->editor_copy)
	snd_error(_("region %d already being edited"), r->id);
      else
	{

	  if (r->use_temp_file == REGION_DEFERRED) 
	    deferred_region_to_temp_file(r);

	  temp_region_name = shorter_tempnam(temp_dir(ss), "region-");
	  err = copy_file(r->filename, temp_region_name);
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
	      else snd_error(_("edit region: can't open region %d temp sound %s: %s!"),
			     r->id, temp_region_name, strerror(errno));
	    }
	  else 
	    snd_error(_("edit region: can't save region %d in temp file (%s: %s)"),
		      r->id, temp_region_name, strerror(errno));
	  FREE(temp_region_name);
	}
    }
  else snd_error(_("edit region: no region at position %d!"), pos);
}

void clear_region_backpointer(snd_info *sp)
{
  region *r;
  if (sp->edited_region)
    {
      r = (region *)(sp->edited_region);
      if (r)
	{
	  snd_remove(r->editor_name, TRUE);
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
  r = (region *)(sp->edited_region);
  ss = sp->state;
  /* update r's data in file, deleting old, redisplay if browser active etc */
  if (r == regions[0]) deactivate_selection();
  free_region(r, CLEAR_REGION_DATA);
  r->use_temp_file = REGION_FILE;
  r->maxamp = 0.0;
  r->frames = CURRENT_SAMPLES(sp->chans[0]);
  for (i = 0; i < sp->nchans; i++)
    {
      val = get_maxamp(sp, sp->chans[i], AT_CURRENT_EDIT_POSITION);
      if (val > r->maxamp) r->maxamp = val;
    }
  /* make new region temp file */
  r->filename = snd_tempnam(ss);
  err = copy_file(r->editor_name, r->filename);
  if (err != MUS_NO_ERROR)
    snd_error(_("can't make region temp file (%s: %s)"), 
	      r->filename, 
	      strerror(errno));
  else
    {
      make_region_readable(r);
      if (region_browser_is_active()) 
	update_region_browser(ss, 1);
    }
}


#define XEN_REGION_P(Val) XEN_INTEGER_P(Val)
#define XEN_REGION_IF_BOUND_P(Val) ((XEN_NOT_BOUND_P(Val)) || (XEN_INTEGER_P(Val)))
#define XEN_REGION_TO_C_INT(Val) ((XEN_INTEGER_P(Val)) ? XEN_TO_C_INT(Val) : stack_position_to_id(0))
#define C_INT_TO_XEN_REGION(Val) C_TO_XEN_INT(Val)


static XEN snd_no_such_region_error(const char *caller, XEN n)
{
  XEN_ERROR(NO_SUCH_REGION,
	    XEN_LIST_2(C_TO_XEN_STRING(caller),
		       n));
  return(XEN_FALSE);
}

static XEN g_restore_region(XEN pos, XEN chans, XEN len, XEN srate, XEN maxamp, XEN name, XEN start, XEN end, XEN filename)
{
  /* internal function used by save-state mechanism -- not intended for external use */
  region *r;
  int regn;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(pos), pos, XEN_ARG_1, S_restore_region, "a region id");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chans), chans, XEN_ARG_2, S_restore_region, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(len), len, XEN_ARG_3, S_restore_region, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(srate), srate, XEN_ARG_4, S_restore_region, "an integer");
  XEN_ASSERT_TYPE(XEN_DOUBLE_P(maxamp), maxamp, XEN_ARG_5, S_restore_region, "a double");
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_6, S_restore_region, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_P(start), start, XEN_ARG_7, S_restore_region, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_P(end), end, XEN_ARG_8, S_restore_region, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ARG_9, S_restore_region, "a string");
  r = (region *)CALLOC(1, sizeof(region));
  regn = XEN_TO_SMALL_C_INT(pos);
  if (regions[regn]) free_region(regions[regn], COMPLETE_DELETION);
  regions[regn] = r;
  r->id = region_id_ctr++;
  r->maxamp = XEN_TO_C_DOUBLE(maxamp);
  r->chans = XEN_TO_SMALL_C_INT(chans);
  r->rsp = NULL;
  r->editor_copy = NULL;
  r->editor_name = NULL;
  r->frames = XEN_TO_C_OFF_T(len);
  r->srate = XEN_TO_C_INT(srate);
  r->name = copy_string(XEN_TO_C_STRING(name));
  r->start = copy_string(XEN_TO_C_STRING(start));
  r->end = copy_string(XEN_TO_C_STRING(end));
  r->use_temp_file = REGION_FILE;
  r->filename = copy_string(XEN_TO_C_STRING(filename));
  reflect_regions_in_menu();
  reflect_regions_in_region_browser();
  return(C_TO_XEN_INT(r->id));
}

static XEN g_insert_region(XEN samp_n, XEN reg_n, XEN snd_n, XEN chn_n) /* opt reg_n */
{
  #define H_insert_region "("  S_insert_region " (start-samp 0) (region-id 0) (snd #f) (chn #f)): \
insert region data into snd's channel chn starting at start-samp"

  chan_info *cp;
  int rg;
  off_t samp;
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samp_n), samp_n, XEN_ARG_1, S_insert_region, "a number");
  XEN_ASSERT_TYPE(XEN_REGION_IF_BOUND_P(reg_n), reg_n, XEN_ARG_2, S_insert_region, "a region id");
  ASSERT_CHANNEL(S_insert_region, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_insert_region);
  rg = XEN_REGION_TO_C_INT(reg_n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_insert_region, reg_n));
  samp = beg_to_sample(samp_n, S_insert_region);
  paste_region_1(rg, cp, FALSE, samp, S_insert_region);
  update_graph(cp);
  return(reg_n);
}

static XEN g_max_regions(void) 
{
  #define H_max_regions "(" S_max_regions "): max number of regions saved on the region list"
  snd_state *ss;
  ss = get_global_state();
  return(C_TO_XEN_INT(max_regions(ss)));
}

static XEN g_set_max_regions(XEN n) 
{
  snd_state *ss;
  int regs;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_setB S_max_regions, "an integer"); 
  regs = XEN_TO_C_INT(n);
  if (regs < 0)
    XEN_OUT_OF_RANGE_ERROR(S_setB S_max_regions, 1, n, S_max_regions " ~A < 0?");
  ss = get_global_state();
  set_max_regions(ss, regs);
  return(C_TO_XEN_INT(max_regions(ss)));
}

enum {REGION_FRAMES, REGION_SRATE, REGION_CHANS, REGION_MAXAMP, REGION_FORGET, REGION_PLAY};

static XEN region_get(int field, XEN n, char *caller)
{
  int rg;
  rg = XEN_REGION_TO_C_INT(n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(caller, n));
  switch (field)
    {
    case REGION_FRAMES: return(C_TO_XEN_OFF_T(region_len(rg))); break;
    case REGION_SRATE:  return(C_TO_XEN_INT(region_srate(rg))); break;
    case REGION_CHANS:  return(C_TO_XEN_INT(region_chans(rg))); break;
    case REGION_MAXAMP: return(C_TO_XEN_DOUBLE(region_maxamp(rg))); break;
    case REGION_FORGET: delete_region_and_update_browser(get_global_state(), id_to_stack_position(rg)); return(n); break;
    }
  return(C_TO_XEN_INT(0));
}

static XEN g_region_p(XEN n)
{
  #define H_region_p "(" S_region_p " reg): #t if region is active"
  if (XEN_REGION_P(n))
    return(C_TO_XEN_BOOLEAN(region_ok(XEN_REGION_TO_C_INT(n))));
  return(XEN_FALSE);
}

static XEN g_region_frames (XEN n) 
{
  #define H_region_frames "(" S_region_frames " (reg 0)): region length in frames"
  XEN_ASSERT_TYPE(XEN_REGION_IF_BOUND_P(n), n, XEN_ONLY_ARG, S_region_frames, "a region id");
  return(region_get(REGION_FRAMES, n, S_region_frames));
}

static XEN g_region_srate (XEN n) 
{
  #define H_region_srate "(" S_region_srate " (reg 0)): region (nominal) srate"
  XEN_ASSERT_TYPE(XEN_REGION_IF_BOUND_P(n), n, XEN_ONLY_ARG, S_region_srate, "a region id");
  return(region_get(REGION_SRATE, n, S_region_srate));
}

static XEN g_region_chans (XEN n) 
{
  #define H_region_chans "(" S_region_chans " (reg 0): region channels"
  XEN_ASSERT_TYPE(XEN_REGION_IF_BOUND_P(n), n, XEN_ONLY_ARG, S_region_chans, "a region id");
  return(region_get(REGION_CHANS, n, S_region_chans));
}

static XEN g_region_maxamp (XEN n) 
{
  #define H_region_maxamp "(" S_region_maxamp " (reg 0)): region maxamp"
  XEN_ASSERT_TYPE(XEN_REGION_IF_BOUND_P(n), n, XEN_ONLY_ARG, S_region_maxamp, "a region id");
  return(region_get(REGION_MAXAMP, n, S_region_maxamp));
}

static XEN g_forget_region (XEN n) 
{
  #define H_forget_region "(" S_forget_region " (reg 0)): remove region reg from the region list"
  XEN_ASSERT_TYPE(XEN_REGION_IF_BOUND_P(n), n, XEN_ONLY_ARG, S_forget_region, "a region id");
  return(region_get(REGION_FORGET, n, S_forget_region));
}

static XEN g_play_region (XEN n, XEN wait) 
{
  #define H_play_region "(" S_play_region " (reg 0) (wait #f)): play region reg; if wait is #t, play to end before returning"
  int rg, wt = FALSE;
  XEN_ASSERT_TYPE(XEN_REGION_IF_BOUND_P(n), n, XEN_ARG_1, S_play_region, "a region id");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(wait), wait, XEN_ARG_2, S_play_region, "a boolean");
  if (XEN_TRUE_P(wait)) wt = TRUE;
  rg = XEN_REGION_TO_C_INT(n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_play_region, n));
  play_region(get_global_state(), rg, !wt);
  return(n);
}

static XEN g_protect_region (XEN n, XEN protect) 
{
  #define H_protect_region "(" S_protect_region " (reg 0) (val #t)): \
if val is #t protect region n from being pushed off the end of the region list, else unprotect it"

  int rg;
  XEN_ASSERT_TYPE(XEN_REGION_P(n), n, XEN_ARG_1, S_protect_region, "a region id");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(protect), protect, XEN_ARG_2, S_protect_region, "a boolean");
  rg = XEN_REGION_TO_C_INT(n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_protect_region, n));
  set_region_protect(rg, XEN_TO_C_BOOLEAN_OR_TRUE(protect)); 
  return(protect);
}

static XEN g_regions(void) 
{
  #define H_regions "(" S_regions "): current active regions (a list of region ids)"
  int i;
  XEN result;
  result = XEN_EMPTY_LIST;
  for (i = (regions_size - 1); i >= 0; i--)
    if (regions[i])
      result = XEN_CONS(C_INT_TO_XEN_REGION(regions[i]->id), result);
  return(result);
}

static XEN g_make_region(XEN beg, XEN end, XEN snd_n, XEN chn_n)
{
  #define H_make_region "(" S_make_region " (beg) (end) (snd #f) (chn #f)): make a new region between beg and end in snd, returning its id. \
If chn is #t, all chans are included, taking the snd sync field into account if it's not 0.  If no args are passed, the current \
selection is used."
  chan_info *cp;
  sync_info *si = NULL;
  snd_info *sp;
  snd_state *ss;
  off_t *ends = NULL;
  off_t ibeg, iend;
  int id = INVALID_REGION, new_sync, old_sync, i;
  if (XEN_NOT_BOUND_P(beg))
    id = make_region_from_selection();
  else
    {
      XEN_ASSERT_TYPE(XEN_NUMBER_P(beg), beg, XEN_ARG_1, S_make_region, "a number");
      XEN_ASSERT_TYPE(XEN_NUMBER_P(end), end, XEN_ARG_2, S_make_region, "a number");
      ibeg = XEN_TO_C_OFF_T_OR_ELSE(beg, 0);
      iend = XEN_TO_C_OFF_T_OR_ELSE(end, 0);
      ss = get_global_state();
      if (XEN_TRUE_P(chn_n))
	{
	  /* all chans and all sync'd chans if sync not 0 */
	  sp = get_sp(snd_n, NO_PLAYERS);
	  if (sp)
	    {
	      old_sync = sp->sync;
	      if (sp->sync == 0)
		{
		  /* set up temp sync for snd_sync */
		  new_sync = 1;
		  for (i = 0; i < ss->max_sounds; i++)
		    if ((ss->sounds[i]) && (new_sync <= ss->sounds[i]->sync))
		      new_sync = ss->sounds[i]->sync + 1;
		  sp->sync = new_sync;
		}
	      si = snd_sync(ss, sp->sync);
	      sp->sync = old_sync;
	    }
	  else return(snd_no_such_sound_error(S_make_region, snd_n));
	}
      else
	{
	  cp = get_cp(snd_n, chn_n, S_make_region);
	  si = make_simple_sync(cp, ibeg);
	}
      ends = (off_t *)CALLOC(si->chans, sizeof(off_t));
      for (i = 0; i < si->chans; i++)
	{
	  if (CURRENT_SAMPLES(si->cps[i]) - 1 < iend)
	    ends[i] = CURRENT_SAMPLES(si->cps[i]) - 1;
	  else ends[i] = iend;
	  if (ends[i] < ibeg) 
	    {
	      FREE(ends);
	      ends = NULL;
	      si = free_sync_info(si);
	      XEN_OUT_OF_RANGE_ERROR(S_make_region, 1, end, "end ~A < beg?");
	    }
	}
      if (ends)
	{
	  id = define_region(si, ends);
	  if (selection_creates_region(ss))
	    reactivate_selection(si->cps[0], si->begs[0], ends[0]);
	  si = free_sync_info(si);
	  FREE(ends);
	}
    }
  return(C_INT_TO_XEN_REGION(id));
}

static XEN g_save_region (XEN n, XEN filename, XEN type, XEN format, XEN comment) 
{
  #define H_save_region "(" S_save_region " region filename (type #f) (format #f) (comment #f)): save region in filename \
using data format (default depends on machine), header type (" S_mus_next " by default), and comment"

  char *name = NULL, *com = NULL;
  snd_state *ss;
  int res = MUS_NO_ERROR, rg, data_format, header_type;
  XEN_ASSERT_TYPE(XEN_REGION_P(n), n, XEN_ARG_1, S_save_region, "a region id");
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ARG_2, S_save_region, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(type), type, XEN_ARG_3, S_save_region, "an integer (mus-next etc)");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(format), format, XEN_ARG_4, S_save_region, "an integer (mus-bshort etc)");
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(comment), comment, XEN_ARG_5, S_save_region, "a string");
  rg = XEN_REGION_TO_C_INT(n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_save_region, n));
  data_format = XEN_TO_C_INT_OR_ELSE_WITH_CALLER(format, MUS_OUT_FORMAT, S_save_region);
  header_type = XEN_TO_C_INT_OR_ELSE_WITH_CALLER(type, MUS_NEXT, S_save_region);
  name = mus_expand_filename(XEN_TO_C_STRING(filename));
  ss = get_global_state();
  ss->catch_message = NULL;
  if (!(mus_header_writable(header_type, data_format))) 
    {
      if (mus_header_writable(MUS_NEXT, data_format))
	header_type = MUS_NEXT;
      else
	{
	  if (mus_header_writable(MUS_RIFF, data_format))
	    header_type = MUS_RIFF;
	  else header_type = MUS_RAW;
	}
    }
  if (XEN_STRING_P(comment)) com = XEN_TO_C_STRING(comment);
  res = save_region_1(ss, name, header_type, data_format, region_srate(rg), rg, com);
  if (name) FREE(name);
  if (res != MUS_NO_ERROR)
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_2(C_TO_XEN_STRING(S_save_region),
			 C_TO_XEN_STRING(ss->catch_message)));
  return(n);
}

static XEN g_mix_region(XEN chn_samp_n, XEN reg_n, XEN snd_n, XEN chn_n)
{
  #define H_mix_region "(" S_mix_region " (chn-samp 0) (region 0) (snd #f) (chn #f)): \
mix region into snd's channel chn starting at chn-samp; return new mix id."

  chan_info *cp;
  off_t samp;
  int rg, id = -1;
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(chn_samp_n), chn_samp_n, XEN_ARG_1, S_mix_region, "a number");
  XEN_ASSERT_TYPE(XEN_REGION_IF_BOUND_P(reg_n), reg_n, XEN_ARG_2, S_mix_region, "a region id");
  ASSERT_CHANNEL(S_mix_region, snd_n, chn_n, 3);
  rg = XEN_REGION_TO_C_INT(reg_n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_mix_region, reg_n));
  cp = get_cp(snd_n, chn_n, S_mix_region);
  if (XEN_BOUND_P(chn_samp_n))
    samp = beg_to_sample(chn_samp_n, S_mix_region);
  else samp = CURSOR(cp);
  cp->state->catch_message = NULL;
  id = paste_region_1(rg, cp, TRUE, samp, S_mix_region);
  if (id == INVALID_MIX_ID)
    XEN_ERROR(MUS_MISC_ERROR,
	      XEN_LIST_2(C_TO_XEN_STRING(S_mix_region),
			 C_TO_XEN_STRING(cp->state->catch_message)));
  return(C_TO_XEN_INT(id));
}

static XEN g_region_sample(XEN samp_n, XEN reg_n, XEN chn_n)
{
  #define H_region_sample "(" S_region_sample " (samp 0) (region 0) (chan 0)): region's sample at samp in chan"

  int rg, chan;
  off_t samp;
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samp_n), samp_n, XEN_ARG_1, S_region_sample, "a number");
  XEN_ASSERT_TYPE(XEN_REGION_IF_BOUND_P(reg_n), reg_n, XEN_ARG_2, S_region_sample, "a region id");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chn_n), chn_n, XEN_ARG_3, S_region_sample, "an integer");
  chan = XEN_TO_C_INT_OR_ELSE(chn_n, 0);
  rg = XEN_REGION_TO_C_INT(reg_n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_region_sample, reg_n));
  samp = beg_to_sample(samp_n, S_region_sample);
  if ((chan >= 0) && (chan < region_chans(rg)))
    return(C_TO_XEN_DOUBLE(region_sample(rg, chan, samp)));
  else return(snd_no_such_channel_error(S_region_sample, XEN_LIST_1(reg_n), chn_n));
}

static XEN g_region_samples2vct(XEN beg_n, XEN num, XEN reg_n, XEN chn_n, XEN v)
{
  #define H_region_samples2vct "(" S_region_samples2vct " (beg 0) (samps reglen) (region 0) (chan 0) (v #f)): \
write region's samples starting at beg for samps in channel chan to vct v; return v (or create a new one)"

  Float *data;
  int reg, chn;
  off_t len, beg;
  vct *v1 = get_vct(v);
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(beg_n), beg_n, XEN_ARG_1, S_region_samples2vct, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(num), num, XEN_ARG_2, S_region_samples2vct, "a number");
  XEN_ASSERT_TYPE(XEN_REGION_IF_BOUND_P(reg_n), reg_n, XEN_ARG_3, S_region_samples2vct, "a region id");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chn_n), chn_n, XEN_ARG_4, S_region_samples2vct, "an integer");
  reg = XEN_REGION_TO_C_INT(reg_n);
  if (!(region_ok(reg)))
    return(snd_no_such_region_error(S_region_samples2vct, reg_n));
  chn = XEN_TO_C_INT_OR_ELSE(chn_n, 0);
  if ((chn < 0) || (chn >= region_chans(reg)))
    return(snd_no_such_channel_error(S_region_samples2vct, XEN_LIST_1(reg_n), chn_n));
  len = XEN_TO_C_OFF_T_OR_ELSE(num, 0);
  if (len == 0) len = region_len(reg);
  if (len > 0)
    {
      beg = beg_to_sample(beg_n, S_region_samples2vct);
      if (beg >= region_len(reg)) 
	return(XEN_FALSE);
      if (v1)
	data = v1->data;
      else data = (Float *)CALLOC(len, sizeof(Float));
      region_samples(reg, chn, beg, len, data);
      if (v1)
	return(v);
      else return(make_vct(len, data));
    }
  return(XEN_FALSE);
}

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_9(g_restore_region_w, g_restore_region)
XEN_ARGIFY_4(g_insert_region_w, g_insert_region)
XEN_NARGIFY_0(g_regions_w, g_regions)
XEN_ARGIFY_1(g_region_frames_w, g_region_frames)
XEN_ARGIFY_1(g_region_srate_w, g_region_srate)
XEN_ARGIFY_1(g_region_chans_w, g_region_chans)
XEN_ARGIFY_1(g_region_maxamp_w, g_region_maxamp)
XEN_ARGIFY_5(g_save_region_w, g_save_region)
XEN_ARGIFY_1(g_forget_region_w, g_forget_region)
XEN_NARGIFY_2(g_protect_region_w, g_protect_region)
XEN_ARGIFY_2(g_play_region_w, g_play_region)
XEN_ARGIFY_4(g_make_region_w, g_make_region)
XEN_ARGIFY_4(g_mix_region_w, g_mix_region)
XEN_ARGIFY_3(g_region_sample_w, g_region_sample)
XEN_ARGIFY_5(g_region_samples2vct_w, g_region_samples2vct)
XEN_NARGIFY_1(g_region_p_w, g_region_p)
XEN_NARGIFY_0(g_max_regions_w, g_max_regions)
XEN_NARGIFY_1(g_set_max_regions_w, g_set_max_regions)
#else
#define g_restore_region_w g_restore_region
#define g_insert_region_w g_insert_region
#define g_regions_w g_regions
#define g_region_frames_w g_region_frames
#define g_region_srate_w g_region_srate
#define g_region_chans_w g_region_chans
#define g_region_maxamp_w g_region_maxamp
#define g_save_region_w g_save_region
#define g_forget_region_w g_forget_region
#define g_protect_region_w g_protect_region
#define g_play_region_w g_play_region
#define g_make_region_w g_make_region
#define g_mix_region_w g_mix_region
#define g_region_sample_w g_region_sample
#define g_region_samples2vct_w g_region_samples2vct
#define g_region_p_w g_region_p
#define g_max_regions_w g_max_regions
#define g_set_max_regions_w g_set_max_regions
#endif

void g_init_regions(void)
{
  XEN_DEFINE_PROCEDURE(S_restore_region,     g_restore_region_w, 9, 0, 0,     "internal func used in save-state, restores a region");
  XEN_DEFINE_PROCEDURE(S_insert_region,      g_insert_region_w, 0, 4, 0,      H_insert_region);
  XEN_DEFINE_PROCEDURE(S_regions,            g_regions_w, 0, 0, 0,            H_regions);
  XEN_DEFINE_PROCEDURE(S_region_frames,      g_region_frames_w, 0, 1, 0,      H_region_frames);
  XEN_DEFINE_PROCEDURE(S_region_srate,       g_region_srate_w, 0, 1, 0,       H_region_srate);
  XEN_DEFINE_PROCEDURE(S_region_chans,       g_region_chans_w, 0, 1, 0,       H_region_chans);
  XEN_DEFINE_PROCEDURE(S_region_maxamp,      g_region_maxamp_w, 0, 1, 0,      H_region_maxamp);
  XEN_DEFINE_PROCEDURE(S_save_region,        g_save_region_w, 2, 3, 0,        H_save_region);
  XEN_DEFINE_PROCEDURE(S_forget_region,      g_forget_region_w, 0, 1, 0,      H_forget_region);
  XEN_DEFINE_PROCEDURE(S_protect_region,     g_protect_region_w, 2, 0, 0,     H_protect_region);
  XEN_DEFINE_PROCEDURE(S_play_region,        g_play_region_w, 0, 2, 0,        H_play_region);
  XEN_DEFINE_PROCEDURE(S_make_region,        g_make_region_w, 0, 4, 0,        H_make_region);
  XEN_DEFINE_PROCEDURE(S_mix_region,         g_mix_region_w, 0, 4, 0,         H_mix_region);
  XEN_DEFINE_PROCEDURE(S_region_sample,      g_region_sample_w, 0, 3, 0,      H_region_sample);
  XEN_DEFINE_PROCEDURE(S_region_samples2vct, g_region_samples2vct_w, 0, 5, 0, H_region_samples2vct);
  XEN_DEFINE_PROCEDURE(S_region_p,           g_region_p_w, 1, 0, 0,           H_region_p);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_max_regions, g_max_regions_w, H_max_regions, S_setB S_max_regions, g_set_max_regions_w, 0, 0, 1, 0);
}

