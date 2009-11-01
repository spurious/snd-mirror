#include "snd.h"
#include "sndlib-strings.h"
#include "clm2xen.h"


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
  mus_long_t len;
  chan_info **cps;
  int *edpos;
} deferred_region;


static deferred_region *free_deferred_region(deferred_region *dr)
{
  if (dr)
    {
      if (dr->cps) free(dr->cps);
      if (dr->edpos) free(dr->edpos);
      free(dr);
    }
  return(NULL);
}


typedef struct region {
  int chans;
  mus_long_t frames;
  int srate;                /* for file save (i.e. region->file) */
  int header_type;          /* for file save */
  snd_info *rsp;
  char *name, *start, *end; /* for region browser */
  char *filename;           /* if region data is stored in a temp file */
  int use_temp_file;        /* REGION_FILE = in temp file 'filename', REGION_DEFERRED = in 'dr' */
  mus_float_t maxamp;
  mus_long_t maxamp_position;
  snd_info *editor_copy;
  char *editor_name;
  int id;
  deferred_region *dr;      /* REGION_DEFERRED descriptor */
  peak_env_info **peak_envs;
  mus_long_t *begs, *lens;
} region;


static void deferred_region_to_temp_file(region *r);

static void free_region(region *r, int complete)
{
  /* if not complete, just clear out old data (edited region being saved) */
  if (r)
    {
      if (complete == COMPLETE_DELETION)
	{
	  if (r->editor_copy)
	    {
	      snd_info *sp;
	      sp = r->editor_copy; 
	      sp->edited_region = NULL;
	      r->editor_copy = NULL;
	    }
	  if (r->name) free(r->name);
	  if (r->start) free(r->start);
	  if (r->end) free(r->end);
	  if (r->begs) free(r->begs);
	  if (r->lens) free(r->lens);
	  if (r->peak_envs)
	    {
	      int i;
	      for (i = 0; i < r->chans; i++)
		if (r->peak_envs[i]) 
		  r->peak_envs[i] = free_peak_env_info(r->peak_envs[i]);
	      free(r->peak_envs);
	      r->peak_envs = NULL;
	    }
	}
      if (r->use_temp_file == REGION_FILE) /* we can delete this temp file because all references copy first */
	{
	  if (r->filename)
	    {
	      snd_remove(r->filename, REMOVE_FROM_CACHE);
	      free(r->filename);
	    }
	  r->filename = NULL;
	}
      if (r->use_temp_file == REGION_DEFERRED)
	r->dr = free_deferred_region(r->dr);
      if (r->rsp) 
	r->rsp = completely_free_snd_info(r->rsp);
      if (complete == COMPLETE_DELETION) free(r);
    }
}


static region **regions = NULL;
static int regions_size = 0, regions_allocated_size = 0;

void allocate_regions(int numreg)
{
  int i;
  if (numreg > regions_allocated_size)
    {
      if (regions)
	{
	  regions = (region **)realloc(regions, numreg * sizeof(region *));
	  for (i = regions_allocated_size; i < numreg; i++) regions[i] = NULL;
	}
      else regions = (region **)calloc(numreg, sizeof(region *)); 
      regions_allocated_size = numreg;
    }
  if (regions_size > numreg)
    {
      for (i = numreg; i < regions_size; i++)
	if (regions[i])
	  {
	    free_region(regions[i], COMPLETE_DELETION);
	    regions[i] = NULL;
	  }
      if (region_browser_is_active()) update_region_browser(true);
    }
  regions_size = numreg;
}


static void set_max_regions(int n)
{
  if (n >= 0)
    {
      allocate_regions(n);
      allocate_region_rows(n);
      in_set_max_regions(n);
    }
}


int region_id_to_list_position(int id)
{
  int i;
  if ((id >= 0) && (id < region_id_ctr))
    for (i = 0; i < regions_size; i++)
      if ((regions[i]) && 
	  (regions[i]->id == id))
	return(i);
  return(INVALID_REGION);
}


int region_list_position_to_id(int n) 
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


bool region_ok(int id) 
{
  return(id_to_region(id) != NULL);
}


mus_long_t region_len(int n) 
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


const char *region_file_name(int n) 
{  
  region *r;
  r = id_to_region(n);
  if (r) 
    return(r->name); 
  return(NULL);
}


mus_float_t region_maxamp(int n) 
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


static mus_long_t region_maxamp_position(int n) 
{
  region *r;
  r = id_to_region(n);
  if (r)
    {
      if ((r->maxamp < 0.0) && (r->use_temp_file == REGION_DEFERRED))
	deferred_region_to_temp_file(r);
      if ((r->maxamp_position == -1) && (r->filename)) /* not picked up elsewhere */
	{
	  /* it exists as r->filename, so just use sndlib... */
	  mus_sample_t *vals;
	  mus_long_t *times;
	  int i;
	  mus_long_t maxpos;
	  mus_sample_t maxsamp;
	  vals = (mus_sample_t *)calloc(r->chans, sizeof(mus_sample_t));
	  times = (mus_long_t *)calloc(r->chans, sizeof(mus_long_t));
	  mus_sound_maxamps(r->filename, r->chans, vals, times);
	  maxpos = times[0];
	  maxsamp = vals[0];
	  for (i = 1; i < r->chans; i++)
	    if (vals[i] > maxsamp)
	      {
		maxsamp = vals[i];
		maxpos = times[i];
	      }
	  free(vals);
	  free(times);
	  r->maxamp_position = maxpos;
	}
      return(r->maxamp_position);
    }
  return(-1);
}


static mus_float_t region_sample(int reg, int chn, mus_long_t samp)
{
  region *r;
  r = id_to_region(reg);
  if (r)
    {
      if ((samp < r->frames) && (chn < r->chans)) 
	{
	  snd_fd *sf;
	  mus_float_t val;
	  deferred_region *drp;
	  switch (r->use_temp_file)
	    {
	    case REGION_FILE:
	      sf = init_region_read(samp, reg, chn, READ_FORWARD);
	      val = read_sample(sf);
	      free_snd_fd(sf);
	      return(val);
	      break;
	    case REGION_DEFERRED:
	      drp = r->dr;
	      return(chn_sample(samp + r->begs[chn], drp->cps[chn], drp->edpos[chn]));
	      break;
	    }
	}
    }
  return(0.0);
}


mus_long_t region_current_location(snd_fd *fd)
{
  region *r;
  r = id_to_region(fd->region);
  switch (r->use_temp_file)
    {
    case REGION_FILE:
      return(current_location(fd));
      break;
    case REGION_DEFERRED:
      return(current_location(fd) - r->begs[0]);
      break;
    }
  return(-1);
}


static void region_samples(int reg, int chn, mus_long_t beg, mus_long_t num, mus_float_t *data)
{
  region *r;
  r = id_to_region(reg);
  if (r)
    {
      if ((beg < r->frames) && (chn < r->chans))
	{
	  snd_fd *sf;
	  mus_long_t i, j = 0;
	  deferred_region *drp;
	  switch (r->use_temp_file)
	    {
	    case REGION_FILE:
	      sf = init_region_read(beg, reg, chn, READ_FORWARD);
	      for (i = beg, j = 0; (i < r->frames) && (j < num); i++, j++) 
		data[j] = read_sample(sf);
	      free_snd_fd(sf);
	      break;
	    case REGION_DEFERRED:
	      drp = r->dr;
	      sf = init_sample_read_any(beg + r->begs[chn], drp->cps[chn], READ_FORWARD, drp->edpos[chn]);
	      for (i = beg, j = 0; (i < r->frames) && (j < num); i++, j++) 
		data[j] = read_sample(sf);
	      free_snd_fd(sf);
	      break;
	    }
	  if (j < num)
	    {
	      memset((void *)(data + j), 0, (num - j) * sizeof(mus_float_t));
	      /* for (; j < num; j++)  data[j] = 0.0; */
	    }
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
    reflect_no_regions_in_region_browser();
  return(act);
}


static void make_region_readable(region *r)
{
  snd_info *regsp;
  file_info *hdr;
  int i;

  if (r->use_temp_file == REGION_DEFERRED) 
    deferred_region_to_temp_file(r);
  if (r->rsp) return;

  regsp = make_basic_snd_info(r->chans);
  regsp->nchans = r->chans;
  regsp->hdr = (file_info *)calloc(1, sizeof(file_info));
  regsp->search_proc = XEN_UNDEFINED;
  regsp->prompt_callback = XEN_UNDEFINED;
  regsp->inuse = SOUND_READER;

  hdr = regsp->hdr;
  hdr->samples = r->frames * r->chans;
  hdr->srate = r->srate;
  hdr->chans = r->chans;
  hdr->comment = NULL;

  for (i = 0; i < r->chans; i++)
    {
      chan_info *cp;
      cp = make_chan_info(NULL, i, regsp);
      cp->editable = false;
      regsp->chans[i] = cp;
      add_channel_data_1(cp, r->srate, r->frames, WITHOUT_GRAPH);
      cp->hookable = WITHOUT_HOOK;

      hdr = make_file_info(r->filename, FILE_READ_ONLY, FILE_NOT_SELECTED);
      if (hdr)
	{
	  snd_io *io;
	  int fd;
	  fd = snd_open_read(r->filename);
	  snd_file_open_descriptors(fd,
				    r->filename,
				    hdr->format,
				    hdr->data_location,
				    hdr->chans,
				    hdr->type);
	  io = make_file_state(fd, hdr, i, 0, FILE_BUFFER_SIZE);
	  cp->sounds[0] = make_snd_data_file(r->filename, io, hdr, DONT_DELETE_ME, cp->edit_ctr, i); /* don't auto-delete! */
	}
      else
	{
	  XEN_ERROR(XEN_ERROR_TYPE("IO-error"),
		    XEN_LIST_3(C_TO_XEN_STRING(_("can't read region file!!")),
			       C_TO_XEN_STRING(r->filename),
			       C_TO_XEN_STRING(snd_open_strerror())));
	}
    }
  r->rsp = regsp;
}


file_info *fixup_region_data(chan_info *cp, int chan, int pos)
{
  /* for region browser labels */
  if ((pos >= 0) && 
      (pos < regions_size) &&
      (regions[pos]))
    {
      region *r;
      r = regions[pos];
      if (chan < r->chans)
	{
	  snd_info *nsp;
	  chan_info *ncp;

	  make_region_readable(r);
	  nsp = r->rsp;
	  ncp = nsp->chans[chan];

	  cp->sounds = ncp->sounds;
	  cp->sound_size = ncp->sound_size;
	  cp->edits = ncp->edits; /* ?? is this safe ?? */
	  cp->edit_size = ncp->edit_size;
	  cp->edit_ctr = ncp->edit_ctr;
	  cp->edits[0]->samples = ncp->edits[0]->samples;
	  cp->axis = ncp->axis;

	  if ((r->peak_envs) && (r->peak_envs[chan]))
	    cp->edits[0]->peak_env = r->peak_envs[chan];
	  else
	    {
	      if (cp->edits[0]->peak_env)
		cp->edits[0]->peak_env = NULL;
	    }

	  initialize_scrollbars(cp);
	  return(nsp->hdr);
	}
    }
  return(NULL);
}


void for_each_region_chan_with_refint(void (*func)(chan_info *ncp, int *val), int *value)
{
  /* used only in snd-io.c to remove dangling temp files (probably can't actually happen) */
  int i;
  for (i = 0; i < regions_size; i++)
    {
      int chn;
      region *r;
      r = regions[i];
      if ((r) && (r->rsp) && (r->use_temp_file == REGION_FILE))
	for (chn = 0; chn < r->chans; chn++)
	  {
	    chan_info *cp;
	    cp = r->rsp->chans[chn];
	    (*func)(cp, value);
	  }
    }
}


region_state *region_report(void)
{
  region_state *rs;
  int i, len;
  rs = (region_state *)calloc(1, sizeof(region_state));
  len = regions_size;
  for (i = 0; i < regions_size; i++) 
    if (!(regions[i])) 
      {
	len = i; 
	break;
      }
  rs->len = len;
  if (len == 0) return(rs);
  rs->name = (char **)calloc(len, sizeof(char *));
  for (i = 0; i < len; i++)
    {
      region *r;
      char *reg_buf;
      r = regions[i];
      reg_buf = (char *)calloc(LABEL_BUFFER_SIZE, sizeof(char));
      mus_snprintf(reg_buf, LABEL_BUFFER_SIZE, "%d: %s (%s:%s)", r->id, r->name, r->start, r->end);
      rs->name[i] = reg_buf;
    }
  return(rs);
}


char *region_description(int rg)
{
  region *r;
  r = id_to_region(rg);
  if (r)
    return(mus_format("region data from %s (%s : %s)", r->name, r->start, r->end));
  return(NULL);
}


void free_region_state(region_state *r)
{
  if (r)
    {
      int i;
      for (i = 0; i < r->len; i++)
	if (r->name[i]) 
	  free(r->name[i]);
      if (r->name) free(r->name);
      free(r);
    }
}


int remove_region_from_list(int pos) /* region browser */
{
  int i, id;
  id = region_list_position_to_id(pos);
  if (id == INVALID_REGION) return(INVALID_REGION);
  stop_playing_region(id, PLAY_CLOSE);
  free_region(id_to_region(id), COMPLETE_DELETION);
  for (i = pos; i < regions_size - 1; i++) 
    regions[i] = regions[i + 1]; 
  regions[regions_size - 1] = NULL;
  return(check_regions());
}


static void add_to_region_list(region *r) 
{
  int i, okr = -1;
  for (i = max_regions(ss) - 1; i >= 0; i--) 
    {
      if (!(regions[i]))
	{
	  okr = i; 
	  break;
	}
    }
  if (okr == -1)
    okr = max_regions(ss) - 1;
  if (regions[okr]) 
    {
      stop_playing_region(regions[okr]->id, PLAY_CLOSE);
      free_region(regions[okr], COMPLETE_DELETION);
    }
  for (i = okr; i > 0; i--) 
    regions[i] = regions[i - 1]; 
  regions[0] = r;
  if (!r) check_regions();
}


#define NOT_EDITABLE -2

static int paste_region_1(int n, chan_info *cp, bool add, mus_long_t beg, io_error_t *err, int start_chan, int *out_chans)
{
  region *r;
  char *origin = NULL;
  int i, id = -1;
  io_error_t io_err;
  sync_info *si = NULL;

  r = id_to_region(n);
  if ((r == NULL) || 
      (r->frames == 0)) 
    return(INVALID_REGION);

  if (!(editable_p(cp))) 
    {
      (*err) = IO_EDIT_HOOK_CANCELLATION;
      return(NOT_EDITABLE);
    }

  if (r->use_temp_file == REGION_DEFERRED)
    deferred_region_to_temp_file(r);

  si = sync_to_chan(cp);
  (*out_chans) = si->chans;

  if (add)
    {
      /* unfortunately we need to copy here since the region may fall off the region stack while we're still using the mix */
      char *newname;
      newname = shorter_tempnam(temp_dir(ss), "snd_");
      io_err = copy_file(r->filename, newname);
      if (io_err != IO_NO_ERROR)
	{
	  (*err) = io_err;
	  return(INVALID_REGION);
	}
      else 
	{
#if HAVE_FORTH
	  origin = mus_format("%d %s " MUS_LD " %s drop", n, S_integer_to_region, beg, S_mix_region);
#endif
#if HAVE_RUBY
	  origin = mus_format("%s(%s(%d), " MUS_LD, TO_PROC_NAME(S_mix_region), TO_PROC_NAME(S_integer_to_region), n, beg);
#endif
#if HAVE_SCHEME || (!HAVE_EXTENSION_LANGUAGE)
	  origin = mus_format("%s (%s %d) " MUS_LD, S_mix_region, S_integer_to_region, n, beg);
#endif
	  if (si->chans > 1)
	    remember_temp(newname, si->chans);

	  id = mix_file(beg, r->frames, si->chans, si->cps, newname, 
			(si->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
			origin, with_mix_tags(ss), start_chan);
	  free(origin);
	}
      if (newname) free(newname);
    }
  else
    {
      char *tempfile = NULL;
      if (r->use_temp_file == REGION_FILE)
	{
	  tempfile = snd_tempnam();
	  io_err = copy_file(r->filename, tempfile);
	  if (io_err != IO_NO_ERROR)
	    {
	      if (si) si = free_sync_info(si);
	      (*err) = io_err;
	      return(INVALID_REGION);
	    }
	  else
	    if (r->chans > 1) 
	      remember_temp(tempfile, r->chans);
	}

#if HAVE_FORTH
	  origin = mus_format("%d %s " MUS_LD " %s drop", n, S_integer_to_region, beg, S_insert_region);
#endif
#if HAVE_RUBY
	  origin = mus_format("%s(%s(%d), " MUS_LD, TO_PROC_NAME(S_insert_region), TO_PROC_NAME(S_integer_to_region), n, beg);
#endif
#if HAVE_SCHEME || (!HAVE_EXTENSION_LANGUAGE)
	  origin = mus_format("%s (%s %d) " MUS_LD, S_insert_region, S_integer_to_region, n, beg);
#endif

      for (i = 0; ((i < r->chans) && (i < si->chans)); i++)
	{
	  chan_info *ncp;
	  ncp = si->cps[i];                       /* currently syncd chan that we might paste to */
	  if (file_insert_samples(beg, r->frames, tempfile, ncp, i,
				  (r->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
				  origin, ncp->edit_ctr))
	    update_graph(si->cps[i]);
	}
      free(origin);
      if ((r->use_temp_file == REGION_FILE) && (tempfile)) free(tempfile);
    }
  if (si) si = free_sync_info(si);
  return(id);
}


static io_error_t paste_region_2(int n, chan_info *cp, bool add, mus_long_t beg)
{
  io_error_t err = IO_NO_ERROR;
  int id, chans = 0;
  id = paste_region_1(n, cp, add, beg, &err, 0, &chans);
  return(err);
}


io_error_t paste_region(int n, chan_info *cp) 
{
  return(paste_region_2(n, cp, false, CURSOR(cp)));
}


io_error_t add_region(int n, chan_info *cp) 
{
  return(paste_region_2(n, cp, true, CURSOR(cp)));
}


int define_region(sync_info *si, mus_long_t *ends)
{
  /* now look at all sync'd channels, collect them into the current region */
  /* we created the necessary pointers in create_selection above */
  int i;
  mus_long_t len;
  chan_info *cp0;
  snd_info *sp0;
  region *r;
  deferred_region *drp;

  len = 0;
  for (i = 0; i < si->chans; i++)
    if (len < (ends[i] - si->begs[i]))
      len = ends[i] - si->begs[i];
  len += 1;
  if (len <= 0) return(INVALID_REGION);

  cp0 = si->cps[0];
  sp0 = cp0->sound;

  r = (region *)calloc(1, sizeof(region));
  r->id = region_id_ctr++;

  if (regions[0]) 
    add_to_region_list(r); 
  else regions[0] = r;

  r->header_type = (sp0->hdr)->type;
  r->srate = SND_SRATE(sp0);
  r->maxamp = -1.0;
  r->maxamp_position = -1;
  r->editor_copy = NULL;
  r->name = mus_strdup(sp0->short_filename);
  r->chans = si->chans;
  r->frames = len;
  r->start = prettyf((double)(si->begs[0]) / (double)(r->srate), 2);
  r->end = prettyf((double)(ends[0]) / (double)(r->srate), 2);
  r->use_temp_file = REGION_DEFERRED;
  r->begs = (mus_long_t *)calloc(r->chans, sizeof(mus_long_t));
  r->lens = (mus_long_t *)calloc(r->chans, sizeof(mus_long_t));

  ss->deferred_regions++;
  r->dr = (deferred_region *)calloc(1, sizeof(deferred_region));
  drp = r->dr;
  drp->chans = si->chans;
  drp->cps = (chan_info **)calloc(drp->chans, sizeof(chan_info *));
  drp->edpos = (int *)calloc(drp->chans, sizeof(int));
  drp->len = len;

  for (i = 0; i < drp->chans; i++)
    {
      drp->cps[i] = si->cps[i];
      r->begs[i] = si->begs[i];
      r->lens[i] = ends[i] - si->begs[i];
      drp->edpos[i] = drp->cps[i]->edit_ctr;
      if (r->lens[i] > PEAK_ENV_CUTOFF)
	{
	  peak_env_info *ep;
	  ep = drp->cps[i]->edits[drp->edpos[i]]->peak_env;
	  if ((ep) && (ep->completed))
	    {
	      if (r->peak_envs == NULL)
		r->peak_envs = (peak_env_info **)calloc(r->chans, sizeof(peak_env_info *));
	      r->peak_envs[i] = peak_env_section(drp->cps[i], r->begs[i], r->lens[i], drp->edpos[i]);
	    }
	}
    }

  reflect_regions_in_region_browser();
  if (region_browser_is_active()) update_region_browser(true);
  return(r->id);
}


static void deferred_region_to_temp_file(region *r)
{
  int i, k, ofd = 0, datumb = 0, err = 0;
  bool copy_ok;
  mus_long_t j, len = 0;
  mus_sample_t val;
  snd_fd **sfs = NULL;
  snd_info *sp0;
  file_info *hdr = NULL;
  deferred_region *drp = NULL;
  mus_sample_t **data = NULL;

  ss->deferred_regions--;
  drp = r->dr;
  len = drp->len;
  val = MUS_SAMPLE_0; 
  r->use_temp_file = REGION_FILE;
  r->filename = snd_tempnam();
  sp0 = drp->cps[0]->sound;

  copy_ok = ((mus_header_writable(MUS_NEXT, sp0->hdr->format)) && 
	     (r->chans == sp0->nchans) &&
	     (r->peak_envs != NULL) &&
	     ((drp->len - 1) == r->lens[0]));
  if (copy_ok)
    for (i = 0; i < r->chans; i++)
      if ((drp->edpos[i] != 0) || 
	  (drp->cps[i]->sound != sp0) ||
	  (r->begs[i] != r->begs[0]) ||
	  (r->lens[i] != (drp->len - 1)) ||
	  (r->peak_envs[i] == NULL))
	{
	  copy_ok = false;
	  break;
	}

  if (copy_ok)
    {
      /* write next header with correct len
       * seek loc in sp0->filename (r->begs[0])
       * copy len*data-size bytes
       * get max from amp envs
       */
      mus_long_t bytes, err;
      int fdi, fdo;
      char *buffer;
      mus_float_t ymax = 0.0;
      peak_env_info *ep;

      datumb = mus_bytes_per_sample(sp0->hdr->format);
      err = mus_write_header(r->filename, MUS_NEXT, r->srate, r->chans, drp->len * r->chans, sp0->hdr->format, "region deferred temp");

      if (err != MUS_NO_ERROR)
	snd_error(_("can't write region temp file %s: %s"), r->filename, snd_io_strerror());
      else
	{
	  mus_long_t oloc;
	  oloc = mus_header_data_location();
	  fdo = snd_reopen_write(r->filename);
	  lseek(fdo, oloc, SEEK_SET);
	  fdi = mus_file_open_read(sp0->filename);
	  if (fdi == -1)
	    snd_error(_("can't read region's original sound? %s: %s"), sp0->filename, snd_io_strerror());
	  else
	    {
	      mus_long_t data_size;
	      lseek(fdi, sp0->hdr->data_location + r->chans * datumb * r->begs[0], SEEK_SET);
	      buffer = (char *)calloc(MAX_BUFFER_SIZE, sizeof(char));
	      data_size = drp->len * r->chans * datumb;
	      for (j = 0; j < data_size; j += MAX_BUFFER_SIZE)
		{
		  ssize_t n;
		  bytes = data_size - j;
		  if (bytes > MAX_BUFFER_SIZE) 
		    bytes = MAX_BUFFER_SIZE;
		  if (bytes > 0)
		    {
		      /* read and write return 0 to indicate end of file, apparently */
		      n = read(fdi, buffer, bytes);
		      if (n < 0)
			fprintf(stderr, "IO error while reading region temp file: %d %s\n", (int)n, strerror(errno));
		      if (n > 0)
			{
			  n = write(fdo, buffer, bytes);
			  if (n < 0)
			    fprintf(stderr, "IO error while writing region temp file: %d %s\n", (int)n, strerror(errno));
			}
		    }
		}
	      free(buffer);
	      snd_close(fdi, sp0->filename);
	      for (i = 0; i < r->chans; i++)
		{
		  ep = r->peak_envs[i];
		  if (ymax < ep->fmax) 
		    ymax = ep->fmax;
		  if (ymax < -ep->fmin)
		    ymax = -ep->fmin;
		}
	      r->maxamp = ymax;
	      r->maxamp_position = -1; /* not tracked in amp-env stuff */
	    }
	  snd_close(fdo, r->filename);
	}
    }
  else
    {
      mus_long_t max_position = 0;
      io_error_t io_err = IO_NO_ERROR;
#if MUS_DEBUGGING
      char *regstr;
      regstr = mus_format("region %d (%s: %s %s), from %s[%d]",
			  r->id, r->name, r->start, r->end,
			  (r->rsp) ? (r->rsp->filename) : "unknown sound",
			  (r->rsp) ? (r->rsp->index) : 0);
      hdr = make_temp_header(r->filename, r->srate, r->chans, 0, regstr);
      free(regstr);
#else
      hdr = make_temp_header(r->filename, r->srate, r->chans, 0, (char *)c__FUNCTION__);
#endif
      ofd = open_temp_file(r->filename, r->chans, hdr, &io_err);
      if (ofd == -1)
	snd_error(_("%s region temp file %s: %s"), 
		  (io_err != IO_NO_ERROR) ? io_error_name(io_err) : "can't open",
		  r->filename, 
		  snd_open_strerror());
      else
	{
	  sfs = (snd_fd **)calloc(r->chans, sizeof(snd_fd *));
	  data = (mus_sample_t **)calloc(r->chans, sizeof(mus_sample_t *));
	  datumb = mus_bytes_per_sample(hdr->format);
	  /* here if peak_envs, maxamp exists */
	  for (i = 0; i < r->chans; i++)
	    {
	      sfs[i] = init_sample_read_any(r->begs[i], drp->cps[i], READ_FORWARD, drp->edpos[i]);
	      data[i] = (mus_sample_t *)calloc(MAX_BUFFER_SIZE, sizeof(mus_sample_t));
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
		  if (j <= r->lens[i])
		    {
		      mus_sample_t curval;
		      data[i][k] = read_sample_to_mus_sample(sfs[i]);
		      curval = mus_sample_abs(data[i][k]);
		      if (curval > val) 
			{
			  val = curval;
			  max_position = j;
			}
		    }
		  else data[i][k] = MUS_SAMPLE_0;
		}
	    }
	  if (k > 0) 
	    mus_file_write(ofd, 0, k - 1, r->chans, data);
	  close_temp_file(r->filename, ofd, hdr->type, len * r->chans * datumb);
	  r->maxamp = MUS_SAMPLE_TO_FLOAT(val);
	  r->maxamp_position = max_position;
	  for (i = 0; i < r->chans; i++) free(data[i]);
	  for (i = 0; i < r->chans; i++) free_snd_fd(sfs[i]);
	  free(sfs);
	  free(data);
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
  int i;
  for (i = 0; i < regions_size; i++)
    {
      r = regions[i];
      if ((r) && (r->use_temp_file == REGION_DEFERRED))
	{
	  int j;
	  drp = r->dr;
	  for (j = 0; j < drp->chans; j++)
	    if ((drp->cps[j] == cp) &&
		(drp->edpos[j] > edit_top))
	      {
		if (r->lens[j] > 1000000)
		  report_in_minibuffer(cp->sound, _("sequestering region %d..."), r->id);
		deferred_region_to_temp_file(r);
		if (r->lens[j] > 1000000)
		  clear_minibuffer(cp->sound);
		break;
	      }
	}
    }
}


snd_fd *init_region_read(mus_long_t beg, int n, int chan, read_direction_t direction)
{
  /* conjure up a reasonable looking ed list and sound list */
  region *r;
  r = id_to_region(n);
  if ((r) && (chan < r->chans))
    {
      if ((beg == 0) && 
	  (direction == READ_BACKWARD)) 
	beg = r->frames - 1;
      if (r->use_temp_file == REGION_DEFERRED)
	{
	  deferred_region *drp;
	  drp = r->dr;
	  return(init_sample_read_any(r->begs[chan] + beg, drp->cps[chan], direction, drp->edpos[chan]));
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
  for (i = 0; i < regions_size; i++)
    {
      region *r;
      r = regions[i];
      if ((r) && 
	  (r->use_temp_file == REGION_FILE) && 
	  (r->filename))
	{
	  snd_remove(r->filename, REMOVE_FROM_CACHE);
	  free(r->filename);
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


/* (restore-region n chans len srate maxamp name start end filename [date-and-length]) */

void save_regions(FILE *fd)
{
  int i;
  for (i = 0; i < regions_size; i++)
    {
      region *r;
      r = regions[i];
      if (r)
	{
	  io_error_t io_err;
	  char *newname;
	  char *ofile = NULL;

	  if (r->use_temp_file == REGION_DEFERRED) 
	    deferred_region_to_temp_file(r);
	  ofile = shorter_tempnam(save_dir(ss), "snd_save_");
	  newname = run_save_state_hook(ofile);
	  free(ofile);

	  io_err = copy_file(r->filename, newname);
	  if (io_err != IO_NO_ERROR)
	    {
	      snd_warning("trying to save region %d (%s) in %s: %s", r->id, r->filename, newname, io_error_name(io_err));
	    }
	  else
	    {
#if HAVE_RUBY
	      fprintf(fd, "%s(%d, %d, " MUS_LD ", %d, %.4f, \"%s\", \"%s\", \"%s\", ",
		      "restore_region", i, r->chans, r->frames, r->srate, r->maxamp, r->name, r->start, r->end);
	      fprintf(fd, " \"%s\", [%d, " MUS_LD "])\n",
		      newname,
		      (int)mus_sound_write_date(newname),
		      mus_sound_length(newname));
#endif
#if HAVE_SCHEME
	      fprintf(fd, "(%s %d %d " MUS_LD " %d %.4f \"%s\" \"%s\" \"%s\"",
		      S_restore_region, i, r->chans, r->frames, r->srate, r->maxamp, r->name, r->start, r->end);
	      fprintf(fd, " \"%s\" (list %d " MUS_LD "))\n",
		      newname,
		      (int)mus_sound_write_date(newname),
		      mus_sound_length(newname));
#endif
#if HAVE_FORTH
	  fprintf(fd, "%d %d " MUS_LD " %d %.4f \"%s\" \"%s\" \"%s\"",
	          i, r->chans, r->frames, r->srate, r->maxamp, r->name, r->start, r->end);
 	  fprintf(fd, " \"%s\" '( %d " MUS_LD " ) %s drop\n",
 		  newname,
 		  (int)mus_sound_write_date(newname),
 		  mus_sound_length(newname),
		  S_restore_region);
#endif
	    }
	  free(newname);
	}
    }
}


void region_edit(int pos)
{
  /* from region browser:
   *   load region into temp file, load that into snd editor,
   *   if 'save', save temp file and update region (browser also) (cancelling active display if any)
   *   while editing, if delete in browser, cut backpointer in editor and signal it
   */
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
	  io_error_t io_err;
	  char *temp_region_name;
	  if (r->use_temp_file == REGION_DEFERRED) 
	    deferred_region_to_temp_file(r);
	  temp_region_name = shorter_tempnam(temp_dir(ss), "region-");
	  io_err = copy_file(r->filename, temp_region_name);
	  if (io_err == IO_NO_ERROR)
	    {
	      snd_info *sp;
	      ss->open_requestor = FROM_REGION_EDIT;
	      sp = snd_open_file(temp_region_name, FILE_READ_WRITE);
	      if (sp)
		{
		  r->editor_copy = sp;
		  r->editor_name = mus_strdup(temp_region_name);
		  sp->edited_region = r;
		  /* save backpointer so subsequent save affects region if still legit */
		  /* also, since it's a temp file, if closed, delete temp */
		}
	      else snd_error(_("edit region: can't open region %d temp sound %s: %s!"),
			     r->id, temp_region_name, snd_io_strerror());
	    }
	  else 
	    snd_error(_("edit region: can't save region %d in temp file (%s: %s)"),
		      r->id, temp_region_name, snd_io_strerror());
	  free(temp_region_name);
	}
    }
  else snd_error(_("edit region: no region at position %d!"), pos);
}


void clear_region_backpointer(snd_info *sp)
{
  if (sp->edited_region)
    {
      region *r;
      r = sp->edited_region;
      if (r)
	{
	  snd_remove(r->editor_name, REMOVE_FROM_CACHE);
	  free(r->editor_name);
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
  int i;
  io_error_t io_err;

  r = sp->edited_region;
  /* update r's data in file, deleting old, redisplay if browser active etc */

  if (r == regions[0]) deactivate_selection();
  free_region(r, CLEAR_REGION_DATA);

  r->use_temp_file = REGION_FILE;
  r->maxamp = 0.0;
  r->maxamp_position = -1;
  r->frames = CURRENT_SAMPLES(sp->chans[0]);

  for (i = 0; i < sp->nchans; i++)
    {
      mus_float_t val;
      val = channel_maxamp(sp->chans[i], AT_CURRENT_EDIT_POSITION);
      if (val > r->maxamp) r->maxamp = val;
    }

  /* make new region temp file */
  r->filename = snd_tempnam();
  io_err = copy_file(r->editor_name, r->filename);
  if (io_err != IO_NO_ERROR)
    {
      if (io_err == IO_CANT_OPEN_FILE)
	snd_error(_("can't find edited region temp file (%s: %s)"), r->editor_name, snd_io_strerror());
      else snd_error(_("can't make region temp file (%s: %s)"), r->filename, snd_io_strerror());
    }
  else
    {
      make_region_readable(r);
      if (region_browser_is_active()) 
	update_region_browser(true);
    }
}


io_error_t save_region(int rg, const char *name, int type, int format, const char *comment)
{
  region *r;
  io_error_t io_err = IO_NO_ERROR;

  r = id_to_region(rg);
  if (r->use_temp_file == REGION_DEFERRED) 
    deferred_region_to_temp_file(r);

  io_err = snd_write_header(name, type, region_srate(rg), r->chans, r->chans * r->frames, format, comment, NULL);
  if (io_err == IO_NO_ERROR)
    {
      mus_long_t oloc;
      int ofd;

      oloc = mus_header_data_location();
      ofd = snd_reopen_write(name);
      if (ofd != -1)
	{
	  int ifd, ioff;
	  snd_file_open_descriptors(ofd, name, format, oloc, r->chans, type);
	  mus_file_set_clipping(ofd, clipping(ss));
	  lseek(ofd, oloc, SEEK_SET);
	  /* copy r->filename with possible header/data format changes */

	  ifd = snd_open_read(r->filename);
	  if (ifd != -1)
	    {
	      mus_long_t iloc, frames, cursamples;
	      int chans, i, err = 0;
	      mus_sample_t **bufs;

	      chans = mus_sound_chans(r->filename);
	      frames = mus_sound_samples(r->filename) / chans;
	      iloc = mus_sound_data_location(r->filename);

	      snd_file_open_descriptors(ifd,
					r->filename,
					mus_sound_data_format(r->filename),
					iloc,
					chans,
					mus_sound_header_type(r->filename));
	      lseek(ifd, iloc, SEEK_SET);

	      bufs = (mus_sample_t **)calloc(chans, sizeof(mus_sample_t *));
	      for (i = 0; i < chans; i++) bufs[i] = (mus_sample_t *)calloc(FILE_BUFFER_SIZE, sizeof(mus_sample_t));

	      if (((frames * chans * mus_sound_datum_size(r->filename)) >> 10) > disk_kspace(name))
		snd_warning(_("not enough space to save region? -- need " MUS_LD " bytes"),
			    frames * chans * mus_sound_datum_size(r->filename));
	      err = 0;

	      for (ioff = 0; ioff < frames; ioff += FILE_BUFFER_SIZE)
		{
		  if ((ioff + FILE_BUFFER_SIZE) < frames) 
		    cursamples = FILE_BUFFER_SIZE; 
		  else cursamples = (frames - ioff);
		  mus_file_read(ifd, 0, cursamples - 1, chans, bufs);
		  err = mus_file_write(ofd, 0, cursamples - 1, chans, bufs);
		  if (err == -1) 
		    {
		      snd_warning("write error during %s", S_save_region);
		      break;
		    }
		}
	      err = mus_file_close(ifd);
	      for (i = 0; i < chans; i++) free(bufs[i]);
	      free(bufs);

	      if (err != 0)
		snd_warning("can't close %s input!", S_save_region);
	      err = mus_file_close(ofd);
	      if (ss->fam_ok)
		{
		  if (err != 0)
		    snd_error("%s %d: %s %s", S_save_region, rg, r->filename, snd_io_strerror());
		}
	      else
		{
		  if (err == 0)
		    alert_new_file();
		  else snd_error("%s %d: %s %s", S_save_region, rg, r->filename, snd_io_strerror());
		}
	    }
	  else snd_error("%s %d: %s %s", S_save_region, rg, r->filename, snd_io_strerror());
	}
      else snd_error("%s %d: %s %s", S_save_region, rg, name, snd_io_strerror());
    }
  else snd_error("%s %d: %s %s", S_save_region, rg, name, snd_io_strerror());
  return(io_err);
}


/* ---------------------------------------- region objects ---------------------------------------- */

typedef struct {
  int n;
} xen_region;


#define XEN_TO_XEN_REGION(arg) ((xen_region *)XEN_OBJECT_REF(arg))

int xen_region_to_int(XEN n)
{
  xen_region *mx;
  mx = XEN_TO_XEN_REGION(n);
  return(mx->n);
}


static XEN_OBJECT_TYPE xen_region_tag;

bool xen_region_p(XEN obj) 
{
  return(XEN_OBJECT_TYPE_P(obj, xen_region_tag));
}


static void xen_region_free(xen_region *v) {if (v) free(v);}

XEN_MAKE_OBJECT_FREE_PROCEDURE(xen_region, free_xen_region, xen_region_free)


static char *xen_region_to_string(xen_region *v)
{
  #define XEN_REGION_PRINT_BUFFER_SIZE 64
  char *buf;
  if (v == NULL) return(NULL);
  buf = (char *)calloc(XEN_REGION_PRINT_BUFFER_SIZE, sizeof(char));
  sprintf(buf, "#<region %d>", v->n);
  return(buf);
}

XEN_MAKE_OBJECT_PRINT_PROCEDURE(xen_region, print_xen_region, xen_region_to_string)


#if HAVE_FORTH || HAVE_RUBY
static XEN g_xen_region_to_string(XEN obj)
{
  char *vstr;
  XEN result;
  #define S_xen_region_to_string "region->string"

  XEN_ASSERT_TYPE(XEN_REGION_P(obj), obj, XEN_ONLY_ARG, S_xen_region_to_string, "a region");

  vstr = xen_region_to_string(XEN_TO_XEN_REGION(obj));
  result = C_TO_XEN_STRING(vstr);
  free(vstr);
  return(result);
}
#endif


#if (!HAVE_S7)
static bool xen_region_equalp(xen_region *v1, xen_region *v2) 
{
  return((v1 == v2) ||
	 (v1->n == v2->n));
}

static XEN equalp_xen_region(XEN obj1, XEN obj2)
{
  if ((!(XEN_REGION_P(obj1))) || (!(XEN_REGION_P(obj2)))) return(XEN_FALSE);
  return(xen_return_first(C_TO_XEN_BOOLEAN(xen_region_equalp(XEN_TO_XEN_REGION(obj1), XEN_TO_XEN_REGION(obj2))), obj1, obj2));
}
#endif


static xen_region *xen_region_make(int n)
{
  xen_region *new_v;
  new_v = (xen_region *)malloc(sizeof(xen_region));
  new_v->n = n;
  return(new_v);
}


XEN new_xen_region(int n)
{
  xen_region *mx;
  if (n < 0)
    return(XEN_FALSE);

  mx = xen_region_make(n);
  XEN_MAKE_AND_RETURN_OBJECT(xen_region_tag, mx, 0, free_xen_region);
}


#if HAVE_S7
static bool s7_xen_region_equalp(void *obj1, void *obj2)
{
  return((obj1 == obj2) ||
	 (((xen_region *)obj1)->n == ((xen_region *)obj2)->n));
}

static XEN s7_xen_region_length(s7_scheme *sc, XEN obj)
{
  return(g_region_frames(obj, XEN_ZERO));
}
#endif


static void init_xen_region(void)
{
#if HAVE_S7
  xen_region_tag = XEN_MAKE_OBJECT_TYPE("<region>", print_xen_region, free_xen_region, s7_xen_region_equalp, NULL, NULL, NULL, s7_xen_region_length, NULL, NULL);
#else
#if HAVE_RUBY
  xen_region_tag = XEN_MAKE_OBJECT_TYPE("XenRegion", sizeof(xen_region));
#else
  xen_region_tag = XEN_MAKE_OBJECT_TYPE("Region", sizeof(xen_region));
#endif
#endif

#if HAVE_GUILE
  scm_set_smob_print(xen_region_tag,  print_xen_region);
  scm_set_smob_free(xen_region_tag,   free_xen_region);
  scm_set_smob_equalp(xen_region_tag, equalp_xen_region);
#endif

#if HAVE_FORTH
  fth_set_object_inspect(xen_region_tag,   print_xen_region);
  fth_set_object_dump(xen_region_tag,      g_xen_region_to_string);
  fth_set_object_equal(xen_region_tag,     equalp_xen_region);
  fth_set_object_free(xen_region_tag,      free_xen_region);
#endif

#if HAVE_RUBY
  rb_define_method(xen_region_tag, "to_s",     XEN_PROCEDURE_CAST print_xen_region, 0);
  rb_define_method(xen_region_tag, "eql?",     XEN_PROCEDURE_CAST equalp_xen_region, 1);
  rb_define_method(xen_region_tag, "==",       XEN_PROCEDURE_CAST equalp_xen_region, 1);
  rb_define_method(xen_region_tag, "to_str",   XEN_PROCEDURE_CAST g_xen_region_to_string, 0);
#endif
}

/* -------------------------------------------------------------------------------- */


static XEN g_integer_to_region(XEN n)
{
  #define H_integer_to_region "(" S_integer_to_region " n) returns a region object corresponding to the given integer"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_integer_to_region, "an integer");
  return(new_xen_region(XEN_TO_C_INT(n)));
}


static XEN g_region_to_integer(XEN n)
{
  #define H_region_to_integer "(" S_region_to_integer " id) returns the integer corresponding to the given region"
  XEN_ASSERT_TYPE(XEN_REGION_P(n), n, XEN_ONLY_ARG, S_region_to_integer, "a region");
  return(C_TO_XEN_INT(xen_region_to_int(n)));
}


static XEN snd_no_such_region_error(const char *caller, XEN n)
{
  XEN_ERROR(XEN_ERROR_TYPE("no-such-region"),
	    XEN_LIST_2(C_TO_XEN_STRING(caller),
		       n));
  return(XEN_FALSE);
}


static XEN g_restore_region(XEN pos, XEN chans, XEN len, XEN srate, XEN maxamp, XEN name, XEN start, XEN end, XEN filename, XEN date)
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
  XEN_ASSERT_TYPE(XEN_LIST_P(date) && (XEN_LIST_LENGTH(date) == 2), date, XEN_ARG_10, S_restore_region, "a list: '(time bytes)");

  check_saved_temp_file("region", filename, date);

  r = (region *)calloc(1, sizeof(region));
  regn = XEN_TO_C_INT(pos);
  if (regions[regn]) free_region(regions[regn], COMPLETE_DELETION);
  regions[regn] = r;
  r->id = region_id_ctr++;
  r->maxamp = XEN_TO_C_DOUBLE(maxamp);
  r->maxamp_position = -1; /* not saved/restored */
  r->chans = XEN_TO_C_INT(chans);
  r->rsp = NULL;
  r->editor_copy = NULL;
  r->editor_name = NULL;
  r->frames = XEN_TO_C_INT64_T(len);
  r->srate = XEN_TO_C_INT(srate);
  r->name = mus_strdup(XEN_TO_C_STRING(name));
  r->start = mus_strdup(XEN_TO_C_STRING(start));
  r->end = mus_strdup(XEN_TO_C_STRING(end));
  r->use_temp_file = REGION_FILE;
  r->filename = mus_strdup(XEN_TO_C_STRING(filename));

  reflect_regions_in_region_browser();
  return(C_TO_XEN_INT(r->id));
}


static XEN g_insert_region(XEN reg_n, XEN samp_n, XEN snd_n, XEN chn_n) /* opt reg_n */
{
  #define H_insert_region "("  S_insert_region " region :optional (start-samp 0) snd chn): \
insert region data into snd's channel chn starting at start-samp"

  chan_info *cp;
  int rg;
  mus_long_t samp;
  io_error_t err = IO_NO_ERROR;

  XEN_ASSERT_TYPE(XEN_REGION_P(reg_n), reg_n, XEN_ARG_1, S_insert_region, "a region id");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samp_n), samp_n, XEN_ARG_2, S_insert_region, "a number");

  ASSERT_CHANNEL(S_insert_region, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_insert_region);
  if (!cp) return(XEN_FALSE);

  rg = XEN_REGION_TO_C_INT(reg_n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_insert_region, reg_n));

  samp = beg_to_sample(samp_n, S_insert_region);

  err = paste_region_2(rg, cp, false, samp);

  if (SERIOUS_IO_ERROR(err))
    XEN_ERROR(CANT_UPDATE_FILE,
	      XEN_LIST_2(C_TO_XEN_STRING(S_insert_region),
			 C_TO_XEN_STRING(io_error_name(err))));
  update_graph(cp);
  return(reg_n);
}


static XEN g_max_regions(void) 
{
  #define H_max_regions "(" S_max_regions "): max number of regions saved on the region list"
  return(C_TO_XEN_INT(max_regions(ss)));
}


static XEN g_set_max_regions(XEN n) 
{
  int regs;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_setB S_max_regions, "an integer"); 

  regs = XEN_TO_C_INT(n);
  if (regs < 0)
    XEN_OUT_OF_RANGE_ERROR(S_setB S_max_regions, 1, n, S_max_regions " ~A < 0?");

  set_max_regions(regs);
  return(C_TO_XEN_INT(max_regions(ss)));
}


static XEN g_region_p(XEN n)
{
  #define H_region_p "(" S_region_p " reg): " PROC_TRUE " if region is active"
  return(C_TO_XEN_BOOLEAN((XEN_REGION_P(n)) && (region_ok(XEN_REGION_TO_C_INT(n)))));
}


XEN g_region_frames(XEN n, XEN chan) 
{
  region *r;
  int rg, chn;
  #define H_region_frames "(" S_region_frames " reg :optional (chan 0)): region length in frames"

  XEN_ASSERT_TYPE(XEN_REGION_P(n), n, XEN_ARG_1, S_region_frames, "a region");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chan), chan, XEN_ARG_2, S_region_frames, "an integer");

  rg = XEN_REGION_TO_C_INT(n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_region_frames, n));

  if (XEN_NOT_BOUND_P(chan))
    return(C_TO_XEN_INT64_T(region_len(rg)));
  chn = XEN_TO_C_INT(chan);
  if ((chn < 0) || (chn >= region_chans(rg)))
    return(snd_no_such_channel_error(S_region_frames, XEN_LIST_1(n), chan));

  r = id_to_region(rg);
  return(C_TO_XEN_INT64_T(r->lens[chn] + 1));
}


static XEN g_region_position(XEN n, XEN chan) 
{
  region *r;
  int rg, chn;
  #define H_region_position "(" S_region_position " reg :optional (chan 0)): region's position in the original sound"

  XEN_ASSERT_TYPE(XEN_REGION_P(n), n, XEN_ARG_1, S_region_position, "a region");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chan), chan, XEN_ARG_2, S_region_position, "an integer");

  rg = XEN_REGION_TO_C_INT(n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_region_position, n));

  chn = XEN_TO_C_INT_OR_ELSE(chan, 0);
  if ((chn < 0) || (chn >= region_chans(rg)))
    return(snd_no_such_channel_error(S_region_position, XEN_LIST_1(n), chan));

  r = id_to_region(rg);
  return(C_TO_XEN_INT64_T(r->begs[chn]));
}


typedef enum {REGION_SRATE, REGION_CHANS, REGION_MAXAMP, REGION_FORGET, REGION_PLAY, REGION_MAXAMP_POSITION, REGION_HOME} region_field_t;

static XEN region_get(region_field_t field, XEN n, const char *caller)
{
  int rg;
  rg = XEN_REGION_TO_C_INT(n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(caller, n));

  switch (field)
    {
    case REGION_SRATE:  return(C_TO_XEN_INT(region_srate(rg)));                                      break;
    case REGION_CHANS:  return(C_TO_XEN_INT(region_chans(rg)));                                      break;
    case REGION_MAXAMP: return(C_TO_XEN_DOUBLE(region_maxamp(rg)));                                  break;
    case REGION_MAXAMP_POSITION: return(C_TO_XEN_INT64_T(region_maxamp_position(rg)));               break;
    case REGION_FORGET: delete_region_and_update_browser(region_id_to_list_position(rg)); return(n); break;
    case REGION_HOME:
      {
	region *r;
	r = id_to_region(rg);
	if (r)
	  return(XEN_LIST_3(C_TO_XEN_STRING(r->name), 
			    C_TO_XEN_INT64_T(r->begs[0]), 
			    C_TO_XEN_INT64_T(r->lens[0]))); 
      }
      break;
    default: break;
    }
  return(XEN_FALSE);
}


XEN g_region_srate(XEN n) 
{
  #define H_region_srate "(" S_region_srate " reg): region (nominal) srate"
  XEN_ASSERT_TYPE(XEN_REGION_P(n), n, XEN_ONLY_ARG, S_region_srate, "a region");
  return(region_get(REGION_SRATE, n, S_region_srate));
}


XEN g_region_chans(XEN n) 
{
  #define H_region_chans "(" S_region_chans " reg): region channels"
  XEN_ASSERT_TYPE(XEN_REGION_P(n), n, XEN_ONLY_ARG, S_region_chans, "a region");
  return(region_get(REGION_CHANS, n, S_region_chans));
}


static XEN g_region_home(XEN n) 
{
  #define H_region_home "(" S_region_home " reg): a list with the region source sound name and position info"
  XEN_ASSERT_TYPE(XEN_REGION_P(n), n, XEN_ONLY_ARG, S_region_home, "a region");
  return(region_get(REGION_HOME, n, S_region_home));
}


XEN g_region_maxamp(XEN n) 
{
  #define H_region_maxamp "(" S_region_maxamp " reg): region maxamp"
  XEN_ASSERT_TYPE(XEN_REGION_P(n), n, XEN_ONLY_ARG, S_region_maxamp, "a region");
  return(region_get(REGION_MAXAMP, n, S_region_maxamp));
}


static XEN g_region_maxamp_position(XEN n) 
{
  #define H_region_maxamp_position "(" S_region_maxamp_position " reg): first sample where region maxamp occurs"
  XEN_ASSERT_TYPE(XEN_REGION_P(n), n, XEN_ONLY_ARG, S_region_maxamp_position, "a region");
  return(region_get(REGION_MAXAMP_POSITION, n, S_region_maxamp_position));
}


static XEN g_forget_region(XEN n) 
{
  #define H_forget_region "(" S_forget_region " reg): remove region from the region list"
  XEN_ASSERT_TYPE(XEN_REGION_P(n), n, XEN_ONLY_ARG, S_forget_region, "a region");
  return(region_get(REGION_FORGET, n, S_forget_region));
}


XEN g_play_region(XEN n, XEN wait, XEN stop_proc) 
{
  #define H_play_region "(" S_play_region " reg :optional wait stop-proc): play region; if wait is " PROC_TRUE ", play to end before returning"
  int rg;
  bool wt = false;

  XEN_ASSERT_TYPE(XEN_REGION_P(n), n, XEN_ARG_1, S_play_region, "a region");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(wait), wait, XEN_ARG_2, S_play_region, "a boolean");
  XEN_ASSERT_TYPE(((XEN_PROCEDURE_P(stop_proc)) && (procedure_arity_ok(stop_proc, 1))) ||
		  (XEN_NOT_BOUND_P(stop_proc)) || 
		  (XEN_FALSE_P(stop_proc)), 
		  stop_proc, XEN_ARG_3, S_play_region, "a procedure of 1 arg");
  if (XEN_TRUE_P(wait)) wt = true;

  rg = XEN_REGION_TO_C_INT(n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_play_region, n));

  make_region_readable(id_to_region(rg));
  play_region_1(rg, (wt) ? NOT_IN_BACKGROUND : IN_BACKGROUND, stop_proc);

  return(n);
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
  #define H_make_region "(" S_make_region " :optional beg end snd chn): make a new region between beg and end in snd. \
If chn is " PROC_TRUE ", all chans are included, taking the snd sync field into account if it's not 0.  If no args are passed, the current \
selection is used."
  int id = INVALID_REGION, old_sync, i;

  if (max_regions(ss) <= 0) return(XEN_FALSE);
  if (XEN_NOT_BOUND_P(beg))
    id = make_region_from_selection();
  else
    {
      chan_info *cp;
      sync_info *si = NULL;
      snd_info *sp;
      mus_long_t ibeg, iend;
      mus_long_t *ends = NULL;

      XEN_ASSERT_TYPE(XEN_NUMBER_P(beg), beg, XEN_ARG_1, S_make_region, "a number");
      XEN_ASSERT_TYPE(XEN_NUMBER_P(end), end, XEN_ARG_2, S_make_region, "a number");

      ibeg = beg_to_sample(beg, S_make_region);
      iend = beg_to_sample(end, S_make_region);

      if (XEN_TRUE_P(chn_n))
	{
	  /* all chans and all sync'd chans if sync not 0 */
	  sp = get_sp(snd_n);
	  if (sp)
	    {
	      old_sync = sp->sync;
	      if (sp->sync == 0)
		{
		  /* set up temp sync for snd_sync */
		  sp->sync = ss->sound_sync_max + 1;
		  ss->sound_sync_max++;
		}
	      si = snd_sync(sp->sync);
	      sp->sync = old_sync;
	    }
	  else return(snd_no_such_sound_error(S_make_region, snd_n));
	}
      else
	{
	  cp = get_cp(snd_n, chn_n, S_make_region);
	  si = make_simple_sync(cp, ibeg);
	}

      ends = (mus_long_t *)calloc(si->chans, sizeof(mus_long_t));
      for (i = 0; i < si->chans; i++)
	{
	  if (CURRENT_SAMPLES(si->cps[i]) - 1 < iend)
	    ends[i] = CURRENT_SAMPLES(si->cps[i]) - 1;
	  else ends[i] = iend;
	  si->begs[i] = ibeg;
	  if (ends[i] < ibeg) 
	    {
	      free(ends);
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
	  free(ends);
	}
    }
  return(C_INT_TO_XEN_REGION(id));
}


static XEN kw_header_type, kw_data_format, kw_comment, kw_file;

static void init_region_keywords(void)
{
  kw_header_type = XEN_MAKE_KEYWORD("header-type");
  kw_data_format = XEN_MAKE_KEYWORD("data-format");
  kw_comment = XEN_MAKE_KEYWORD("comment");
  kw_file = XEN_MAKE_KEYWORD("file");
}


static void save_region_to_xen_error(const char *msg, void *data)
{
  redirect_snd_error_to(NULL, NULL);
  XEN_ERROR(CANNOT_SAVE,
	    XEN_LIST_2(C_TO_XEN_STRING(S_save_region),
		       C_TO_XEN_STRING(msg)));
}


static XEN g_save_region(XEN n, XEN arg1, XEN arg2, XEN arg3, XEN arg4, XEN arg5, XEN arg6, XEN arg7, XEN arg8)
{
  #define H_save_region "(" S_save_region " region :file :header-type :data-format :comment): save region in file \
using data format (default depends on machine byte order), header type (" S_mus_next "), and comment"

  char *name = NULL;
    const char *file = NULL, *com = NULL;
  int rg, data_format = MUS_OUT_FORMAT, header_type = MUS_NEXT;
  XEN args[8]; 
  XEN keys[4];
  int orig_arg[4] = {0, 0, 0, 0};
  int vals;

  keys[0] = kw_file;
  keys[1] = kw_header_type;
  keys[2] = kw_data_format;
  keys[3] = kw_comment;
  args[0] = arg1; args[1] = arg2; args[2] = arg3; args[3] = arg4; args[4] = arg5; args[5] = arg6; args[6] = arg7; args[7] = arg8; 

  XEN_ASSERT_TYPE(XEN_REGION_P(n), n, XEN_ARG_1, S_save_region, "a region id");

  rg = XEN_REGION_TO_C_INT(n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_save_region, n));

  vals = mus_optkey_unscramble(S_save_region, 4, keys, args, orig_arg);
  if (vals > 0)
    {
      file = mus_optkey_to_string(keys[0], S_save_region, orig_arg[0], NULL);
      header_type = mus_optkey_to_int(keys[1], S_save_region, orig_arg[1], header_type);
      data_format = mus_optkey_to_int(keys[2], S_save_region, orig_arg[2], data_format);
      com = mus_optkey_to_string(keys[3], S_save_region, orig_arg[3], NULL);
    }

  if (file == NULL) 
    XEN_ERROR(XEN_ERROR_TYPE("IO-error"),
	      XEN_LIST_2(C_TO_XEN_STRING(S_save_region),
			 C_TO_XEN_STRING("no output file?")));

  name = mus_expand_filename(file);

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

  redirect_snd_error_to(save_region_to_xen_error, NULL); /* could perhaps pass name here for free in case of error */
  save_region(rg, name, header_type, data_format, com);
  redirect_snd_error_to(NULL, NULL);

  if (name) free(name);
  return(args[orig_arg[0] - 1]); /* -> filename, parallel save-selection */
}


static XEN g_mix_region(XEN reg_n, XEN chn_samp_n, XEN snd_n, XEN chn_n, XEN reg_chn)
{
  #define H_mix_region "(" S_mix_region " region :optional (chn-samp 0) snd chn (region-chan #t)): \
mix region's channel region-chan (or all chans if region-chan is " PROC_TRUE ") into snd's channel chn starting at chn-samp; \
it returns a list of the new mixes"

  chan_info *cp;
  mus_long_t samp;
  io_error_t err = IO_NO_ERROR;
  int i, rg, id = -1, reg_chan = 0, reg_chans = 0;
  XEN result = XEN_EMPTY_LIST;

  XEN_ASSERT_TYPE(XEN_REGION_P(reg_n), reg_n, XEN_ARG_1, S_mix_region, "a region");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(chn_samp_n), chn_samp_n, XEN_ARG_2, S_mix_region, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(reg_chn), reg_chn, XEN_ARG_5, S_mix_region, "an integer or " PROC_TRUE);
  ASSERT_CHANNEL(S_mix_region, snd_n, chn_n, 3);

  rg = XEN_REGION_TO_C_INT(reg_n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_mix_region, reg_n));

  cp = get_cp(snd_n, chn_n, S_mix_region);
  if (!cp) return(XEN_FALSE);

  if (XEN_BOUND_P(chn_samp_n))
    samp = beg_to_sample(chn_samp_n, S_mix_region);
  else samp = CURSOR(cp);

  if (XEN_INTEGER_P(reg_chn))
    reg_chan = XEN_TO_C_INT(reg_chn);

  id = paste_region_1(rg, cp, true, samp, &err, reg_chan, &reg_chans);

  /* id might legitmately be invalid mix id if with_mix_tags is #f or virtual mix not ok */
  if (SERIOUS_IO_ERROR(err))
    XEN_ERROR(CANT_UPDATE_FILE,
	      XEN_LIST_2(C_TO_XEN_STRING(S_mix_region),
			 C_TO_XEN_STRING(io_error_name(err))));
  
  if (id == -1) return(XEN_FALSE);
  for (i = 0; i < reg_chans; i++)
    result = XEN_CONS(new_xen_mix(id + i), result);

  return(XEN_LIST_REVERSE(result));
}


static XEN g_region_sample(XEN reg_n, XEN samp_n, XEN chn_n)
{
  #define H_region_sample "(" S_region_sample " region samp :optional (chan 0)): region's sample at samp in chan"

  int rg, chan;
  mus_long_t samp;

  XEN_ASSERT_TYPE(XEN_REGION_P(reg_n), reg_n, XEN_ARG_1, S_region_sample, "a region");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samp_n), samp_n, XEN_ARG_2, S_region_sample, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chn_n), chn_n, XEN_ARG_3, S_region_sample, "an integer");

  chan = XEN_TO_C_INT_OR_ELSE(chn_n, 0);
  rg = XEN_REGION_TO_C_INT(reg_n);
  if (!(region_ok(rg)))
    return(snd_no_such_region_error(S_region_sample, reg_n));

  samp = beg_to_sample(samp_n, S_region_sample);
  if ((chan >= 0) && (chan < region_chans(rg)))
    return(C_TO_XEN_DOUBLE(region_sample(rg, chan, samp)));
  return(snd_no_such_channel_error(S_region_sample, XEN_LIST_1(reg_n), chn_n));
}


static XEN g_region_to_vct(XEN reg_n, XEN beg_n, XEN num, XEN chn_n, XEN v)
{
  #define H_region_to_vct "(" S_region_to_vct " region :optional (beg 0) samps (chan 0) v): \
write region's samples starting at beg for samps in channel chan to vct v; return v (or create a new one)"

  mus_float_t *data;
  int reg, chn;
  mus_long_t len;
  vct *v1 = xen_to_vct(v);

  XEN_ASSERT_TYPE(XEN_REGION_P(reg_n), reg_n, XEN_ARG_1, S_region_to_vct, "a region");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(beg_n), beg_n, XEN_ARG_2, S_region_to_vct, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(num), num, XEN_ARG_3, S_region_to_vct, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chn_n), chn_n, XEN_ARG_4, S_region_to_vct, "an integer");

  reg = XEN_REGION_TO_C_INT(reg_n);
  if (!(region_ok(reg)))
    return(snd_no_such_region_error(S_region_to_vct, reg_n));

  chn = XEN_TO_C_INT_OR_ELSE(chn_n, 0);
  if ((chn < 0) || (chn >= region_chans(reg)))
    return(snd_no_such_channel_error(S_region_to_vct, XEN_LIST_1(reg_n), chn_n));

  len = XEN_TO_C_INT64_T_OR_ELSE(num, 0);
  if (len < 0)
    XEN_OUT_OF_RANGE_ERROR(S_region_to_vct, 2, num, "length ~A < 0?");
  if ((len == 0) || (len > region_len(reg)))
    len = region_len(reg);

  if (len > 0)
    {
      mus_long_t beg;
      beg = beg_to_sample(beg_n, S_region_to_vct);
      if (beg >= region_len(reg)) 
	return(XEN_FALSE);
      if (v1)
	{
	  data = v1->data;
	  if (len > v1->length) len = v1->length;
	}
      else 
	{
	  if ((beg + len) > region_len(reg))
	    len = region_len(reg) - beg;
	  data = (mus_float_t *)calloc(len, sizeof(mus_float_t));
	}
      region_samples(reg, chn, beg, len, data);
      if (v1)
	return(v);
      else return(xen_make_vct(len, data));
    }
  return(XEN_FALSE);
}


static XEN g_region_graph_style(void) {return(C_TO_XEN_INT(region_graph_style(ss)));}

static XEN g_set_region_graph_style(XEN val) 
{
  int style;
  #define H_region_graph_style "(" S_region_graph_style "): graph style of the region dialog graph. \
The " S_region_graph_style " choices are " S_graph_lines ", " S_graph_dots ", " S_graph_filled ", " S_graph_lollipops ", \
and " S_graph_dots_and_lines "."

  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_region_graph_style, "an integer");

  style = XEN_TO_C_INT(val);
  if (!(graph_style_p(style)))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_region_graph_style, 1, val, "~A: unknown " S_lisp_graph_style);

  set_region_graph_style((graph_style_t)style);
  reflect_region_graph_style();
  return(val);
}


#ifdef XEN_ARGIFY_1
XEN_ARGIFY_10(g_restore_region_w, g_restore_region)
XEN_ARGIFY_4(g_insert_region_w, g_insert_region)
XEN_NARGIFY_0(g_regions_w, g_regions)
XEN_ARGIFY_2(g_region_frames_w, g_region_frames)
XEN_ARGIFY_2(g_region_position_w, g_region_position)
XEN_NARGIFY_1(g_region_srate_w, g_region_srate)
XEN_NARGIFY_1(g_region_chans_w, g_region_chans)
XEN_NARGIFY_1(g_region_home_w, g_region_home)
XEN_NARGIFY_1(g_region_maxamp_w, g_region_maxamp)
XEN_NARGIFY_1(g_region_maxamp_position_w, g_region_maxamp_position)
XEN_ARGIFY_9(g_save_region_w, g_save_region)
XEN_NARGIFY_1(g_forget_region_w, g_forget_region)
XEN_ARGIFY_3(g_play_region_w, g_play_region)
XEN_ARGIFY_4(g_make_region_w, g_make_region)
XEN_ARGIFY_5(g_mix_region_w, g_mix_region)
XEN_ARGIFY_3(g_region_sample_w, g_region_sample)
XEN_ARGIFY_5(g_region_to_vct_w, g_region_to_vct)
XEN_NARGIFY_1(g_region_p_w, g_region_p)
XEN_NARGIFY_0(g_max_regions_w, g_max_regions)
XEN_NARGIFY_1(g_set_max_regions_w, g_set_max_regions)
XEN_NARGIFY_0(g_region_graph_style_w, g_region_graph_style)
XEN_NARGIFY_1(g_set_region_graph_style_w, g_set_region_graph_style)
XEN_NARGIFY_1(g_integer_to_region_w, g_integer_to_region)
XEN_NARGIFY_1(g_region_to_integer_w, g_region_to_integer)
#else
#define g_restore_region_w g_restore_region
#define g_insert_region_w g_insert_region
#define g_regions_w g_regions
#define g_region_frames_w g_region_frames
#define g_region_position_w g_region_position
#define g_region_srate_w g_region_srate
#define g_region_chans_w g_region_chans
#define g_region_home_w g_region_home
#define g_region_maxamp_w g_region_maxamp
#define g_region_maxamp_position_w g_region_maxamp_position
#define g_save_region_w g_save_region
#define g_forget_region_w g_forget_region
#define g_play_region_w g_play_region
#define g_make_region_w g_make_region
#define g_mix_region_w g_mix_region
#define g_region_sample_w g_region_sample
#define g_region_to_vct_w g_region_to_vct
#define g_region_p_w g_region_p
#define g_max_regions_w g_max_regions
#define g_set_max_regions_w g_set_max_regions
#define g_region_graph_style_w g_region_graph_style
#define g_set_region_graph_style_w g_set_region_graph_style
#define g_integer_to_region_w g_integer_to_region
#define g_region_to_integer_w g_region_to_integer
#endif

void g_init_regions(void)
{
  init_xen_region();

  init_region_keywords();

  XEN_DEFINE_PROCEDURE(S_restore_region,         g_restore_region_w,         9, 1, 0, "internal func used in save-state, restores a region");
  XEN_DEFINE_PROCEDURE(S_insert_region,          g_insert_region_w,          2, 2, 0, H_insert_region);
  XEN_DEFINE_PROCEDURE(S_regions,                g_regions_w,                0, 0, 0, H_regions);
  XEN_DEFINE_PROCEDURE(S_region_frames,          g_region_frames_w,          1, 1, 0, H_region_frames);
  XEN_DEFINE_PROCEDURE(S_region_position,        g_region_position_w,        1, 1, 0, H_region_position);
  XEN_DEFINE_PROCEDURE(S_region_srate,           g_region_srate_w,           1, 0, 0, H_region_srate);
  XEN_DEFINE_PROCEDURE(S_region_chans,           g_region_chans_w,           1, 0, 0, H_region_chans);
  XEN_DEFINE_PROCEDURE(S_region_home,            g_region_home_w,            1, 0, 0, H_region_home);
  XEN_DEFINE_PROCEDURE(S_region_maxamp,          g_region_maxamp_w,          1, 0, 0, H_region_maxamp);
  XEN_DEFINE_PROCEDURE(S_region_maxamp_position, g_region_maxamp_position_w, 1, 0, 0, H_region_maxamp_position);
  XEN_DEFINE_PROCEDURE(S_save_region,            g_save_region_w,            2, 7, 0, H_save_region);
  XEN_DEFINE_PROCEDURE(S_forget_region,          g_forget_region_w,          1, 0, 0, H_forget_region);
  XEN_DEFINE_PROCEDURE(S_play_region,            g_play_region_w,            1, 2, 0, H_play_region);
  XEN_DEFINE_PROCEDURE(S_make_region,            g_make_region_w,            0, 4, 0, H_make_region);
  XEN_DEFINE_PROCEDURE(S_mix_region,             g_mix_region_w,             1, 4, 0, H_mix_region);
  XEN_DEFINE_PROCEDURE(S_region_sample,          g_region_sample_w,          2, 1, 0, H_region_sample);
  XEN_DEFINE_PROCEDURE(S_region_to_vct,          g_region_to_vct_w,          1, 4, 0, H_region_to_vct);
  XEN_DEFINE_PROCEDURE(S_region_p,               g_region_p_w,               1, 0, 0, H_region_p);

  XEN_DEFINE_PROCEDURE(S_integer_to_region,      g_integer_to_region_w,      1, 0, 0, H_integer_to_region);
  XEN_DEFINE_PROCEDURE(S_region_to_integer,      g_region_to_integer_w,      1, 0, 0, H_region_to_integer);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_max_regions, g_max_regions_w, H_max_regions, S_setB S_max_regions, g_set_max_regions_w, 0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_region_graph_style, g_region_graph_style_w, H_region_graph_style,
				   S_setB S_region_graph_style, g_set_region_graph_style_w,  0, 0, 1, 0);
}

