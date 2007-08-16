#include "snd.h"

static bool mix_vct_untagged(vct *v, chan_info *cp, off_t beg, const char *origin)
{
  mus_sample_t *data;
  int i, len;
  snd_fd *sf;
  bool result = false;

  len = v->length;
  data = (mus_sample_t *)CALLOC(len, sizeof(mus_sample_t)); /* don't add into v->data! */

  sf = init_sample_read(beg, cp, READ_FORWARD);
  for (i = 0; i < len; i++)
    data[i] = read_sample_to_mus_sample(sf) + MUS_FLOAT_TO_SAMPLE(v->data[i]);
  sf = free_snd_fd(sf);

  result = change_samples(beg, len, data, cp, origin, cp->edit_ctr); /* cp->edit_ctr since mix-vct has no edpos arg, similarly mix */
  if (result) update_graph(cp);

  FREE(data);
  return(result);
}


static bool mix_file_untagged(const char *filename, int in_chan, chan_info *cp, off_t beg, off_t num, file_delete_t auto_delete, const char *origin)
{
  if ((num > 0) && (editable_p(cp)))
    {
      file_info *ihdr;
      ihdr = make_file_info(filename, FILE_READ_ONLY, FILE_NOT_SELECTED);
      if ((ihdr) && (in_chan < ihdr->chans))
	{
	  char *ofile;
	  file_info *ohdr;
	  int ofd;
	  io_error_t io_err = IO_NO_ERROR;

	  ofile = snd_tempnam();
	  ohdr = make_temp_header(ofile, SND_SRATE(cp->sound), 1, 0, (char *)origin);
	  ofd = open_temp_file(ofile, 1, ohdr, &io_err);
	  if (ofd == -1) 
	    {
	      free_file_info(ihdr);
	      free_file_info(ohdr);
	      snd_error(_("%s mix temp file %s: %s"), 
			(io_err != IO_NO_ERROR) ? io_error_name(io_err) : "can't open",
			ofile, 
			snd_open_strerror()); 
	    }
	  else
	    {
	      if ((disk_space_p(num * mus_bytes_per_sample(ohdr->format), ofile)) == DISK_SPACE_OK)
		{
		  snd_fd *sf = NULL;
		  int ifd = 0;
		  sf = init_sample_read(beg, cp, READ_FORWARD);
		  if (sf)
		    ifd = snd_open_read(filename);
		  if ((!sf) ||        /* i.e. no space for temp, I guess */
		      (ifd < 0))       /* maybe too many files open? */
		    {
		      free_file_info(ihdr);
		      free_file_info(ohdr);
		      mus_file_close(ofd);
		      snd_remove(ofile, REMOVE_FROM_CACHE);
		      FREE(ofile);
		    }
		  else
		    {
		      off_t i, j, size, in_chans;
		      int err = 0;
		      mus_sample_t **data;
		      mus_sample_t *chandata;
		      if (beg < 0) beg = 0;
		      in_chans = ihdr->chans;

		      snd_file_open_descriptors(ifd, filename,
						ihdr->format,
						ihdr->data_location,
						ihdr->chans,
						ihdr->type);
		      during_open(ifd, filename, SND_MIX_FILE);
		      if (num < MAX_BUFFER_SIZE) size = num; else size = MAX_BUFFER_SIZE;

		      data = (mus_sample_t **)CALLOC(in_chans, sizeof(mus_sample_t *));
		      data[in_chan] = (mus_sample_t *)CALLOC(size, sizeof(mus_sample_t));
		      chandata = data[in_chan];

		      lseek(ofd, ohdr->data_location, SEEK_SET);
		      lseek(ifd, ihdr->data_location, SEEK_SET);
		      mus_file_read_chans(ifd, 0, size - 1, in_chans, data, data);
		      for (i = 0, j = 0; i < num; i++)
			{
			  if (j == size)
			    {
			      err = mus_file_write(ofd, 0, size - 1, 1, &chandata);
			      mus_file_read_chans(ifd, 0, size - 1, in_chans, data, data);
			      j = 0;
			      if (err == -1) break;
			    }
			  chandata[j] += read_sample_to_mus_sample(sf);
			  j++;
			}
		      if (j > 0) mus_file_write(ofd, 0, j - 1, 1, &chandata);

		      close_temp_file(ofile, ofd, ohdr->type, num * mus_bytes_per_sample(ohdr->format));
		      mus_file_close(ifd);
		      sf = free_snd_fd(sf);
		      FREE(data[in_chan]);
		      FREE(data);
		      free_file_info(ihdr);
		      free_file_info(ohdr);
		      file_change_samples(beg, num, ofile, cp, 0, DELETE_ME, origin, cp->edit_ctr);
		      if (ofile) FREE(ofile);

		      if (auto_delete == DELETE_ME)
			snd_remove(filename, REMOVE_FROM_CACHE);

		      update_graph(cp);
		      return(true);
		    }
		}
	    }
	}
    }
  return(false);
}


int mix_complete_file_at_cursor(snd_info *sp, const char *filename)
{
  if ((sp) && (filename) && (*filename))
    {
      chan_info *cp;
      int err = 0;
      char *fullname;
      fullname = mus_expand_filename(filename);
      cp = any_selected_channel(sp);
      err = mix_complete_file(sp, CURSOR(cp), fullname, with_mix_tags(ss), DONT_DELETE_ME, MIX_FOLLOWS_SYNC);
      if (err == MIX_FILE_NO_FILE) 
	snd_error("can't mix file: %s, %s", filename, snd_io_strerror());
      else
	{
	  if (err == MIX_FILE_NO_MIX) 
	    snd_error("no data to mix in %s", filename);
	}
      if (fullname) FREE(fullname);
      return(err);
    }
  return(MIX_FILE_NO_SP);
}


void drag_and_drop_mix_at_x_y(int data, const char *filename, int x, int y)
{
  int chn, snd;
  chn = UNPACK_CHANNEL(data);
  snd = UNPACK_SOUND(data);
  if ((snd >= 0) &&
      (snd < ss->max_sounds) && 
      (snd_ok(ss->sounds[snd])) &&
      (chn >= 0) &&
      (chn < ss->sounds[snd]->nchans) &&
      (mus_file_probe(filename)))
    {
      snd_info *sp = NULL;
      chan_info *cp;
      off_t sample;
      char *fullname = NULL;
      sp = ss->sounds[snd];
      cp = sp->chans[chn];
      if ((sp->nchans > 1) && 
	  (sp->channel_style == CHANNELS_COMBINED))
	{
	  cp = which_channel(sp, y);
	  chn = cp->chan;
	}
      select_channel(sp, chn);
      sample = snd_round_off_t(ungrf_x(cp->axis, x) * (double)(SND_SRATE(sp)));
      if (sample < 0) sample = 0;
      fullname = mus_expand_filename(filename);
      mix_complete_file(sp, sample, fullname, with_mix_tags(ss), DONT_DELETE_ME, MIX_FOLLOWS_SYNC);
      if (fullname) FREE(fullname);
    }
}


#if HAVE_GUILE_DYNAMIC_WIND
typedef struct {
  off_t beg, len;
  int chans;
  chan_info **cps;
  const char *fullname;
  bool with_tag;
  file_delete_t auto_delete;
  sync_info *si;
  int old_sync;
  snd_info *sp;
} mix_file_context;

static void before_mix_file(void *ignore) {}


static XEN mix_file_body(void *context)
{
  mix_file_context *mx = (mix_file_context *)context;
  int id;
  id = mix_file(mx->beg, mx->len, mx->chans, mx->cps, mx->fullname, mx->auto_delete, NULL, mx->with_tag, 0);
  return(C_TO_XEN_INT(id));
}


static void after_mix_file(void *context)
{
  mix_file_context *mx = (mix_file_context *)context;
  if (mx->si) 
    mx->si = free_sync_info(mx->si); 
  else 
    {
      if (mx->cps) 
	FREE(mx->cps);
    }
  mx->sp->sync = mx->old_sync;
  FREE(mx);
}
#endif


int mix_complete_file(snd_info *sp, off_t beg, const char *fullname, bool with_tag, file_delete_t auto_delete, mix_sync_t all_chans)
{
  chan_info *cp;
  chan_info **cps = NULL;
  int chans, id = MIX_FILE_NO_MIX, old_sync;
  off_t len;
  sync_info *si = NULL;

  len = mus_sound_frames(fullname);
  if (len < 0) return(MIX_FILE_NO_FILE);
  if (len == 0) return(MIX_FILE_NO_MIX);

  cp = any_selected_channel(sp);
  old_sync = sp->sync;
  if ((old_sync == 0) && 
      (all_chans == MIX_SETS_SYNC_LOCALLY))
    {
      sp->sync = ss->sound_sync_max + 1;
      ss->sound_sync_max++;
    }
  if (sp->sync != 0)
    {
      si = snd_sync(sp->sync); 
      cps = si->cps;
      chans = si->chans;
    }
  else
    {
      cps = (chan_info **)CALLOC(1, sizeof(chan_info *));
      cps[0] = cp;
      chans = 1;
    }

#if HAVE_GUILE_DYNAMIC_WIND
  {
    mix_file_context *mx;
    XEN result;
    mx = (mix_file_context *)CALLOC(1, sizeof(mix_file_context));
    mx->beg = beg;
    mx->len = len;
    mx->chans = chans;
    mx->cps = cps;
    mx->fullname = fullname;
    mx->si = si;
    mx->sp = sp;
    mx->with_tag = with_tag;
    mx->auto_delete = auto_delete;
    mx->old_sync = old_sync;
    result = scm_internal_dynamic_wind((scm_t_guard)before_mix_file, 
				       (scm_t_inner)mix_file_body, 
				       (scm_t_guard)after_mix_file, 
				       (void *)mx,
				       (void *)mx);
    id = XEN_TO_C_INT(result);
  }
#else
  id = mix_file(beg, len, chans, cps, fullname, auto_delete, NULL, with_tag, 0);
  if (si) 
    si = free_sync_info(si); 
  else 
    {
      if (cps) 
	FREE(cps);
    }
  sp->sync = old_sync;
#endif
  return(id);
}


static char *b2s(bool val) 
{
  return((val) ? (char *)PROC_TRUE : (char *)PROC_FALSE);  /* cast needed by g++ > 3.4 */
} 


static int mix_infos_ctr = 0;

static char *tagged_mix_to_string(const char *mixinfile, off_t beg, int file_channel, bool delete_file)
{
#if HAVE_FORTH
  return(mus_format("\"%s\" " OFF_TD " %d snd chn %s %s %s to -mix-%d", mixinfile, beg, file_channel, b2s(true), b2s(delete_file), S_mix, mix_infos_ctr));
#endif
#if HAVE_SCHEME
  return(mus_format("(set! -mix-%d (%s \"%s\" " OFF_TD " %d snd chn %s %s))", mix_infos_ctr, S_mix, mixinfile, beg, file_channel, b2s(true), b2s(delete_file)));
#endif
#if HAVE_RUBY
  return(mus_format("_mix_%d = %s(\"%s\", " OFF_TD ", %d, snd, chn, %s, %s)", mix_infos_ctr, TO_PROC_NAME(S_mix), mixinfile, beg, file_channel, b2s(true), b2s(delete_file)));
#endif
}


static char *untagged_mix_to_string(const char *mixinfile, off_t beg, int file_channel, bool delete_file)
{
#if HAVE_FORTH
  return(mus_format("\"%s\" " OFF_TD " %d snd chn %s %s %s", mixinfile, beg, file_channel, b2s(false), b2s(delete_file), S_mix));
#endif
#if HAVE_SCHEME
  return(mus_format("(%s \"%s\" " OFF_TD " %d snd chn %s %s)", S_mix, mixinfile, beg, file_channel, b2s(false), b2s(delete_file)));
#endif
#if HAVE_RUBY
  return(mus_format("%s(\"%s\", " OFF_TD ", %d, snd, chn, %s, %s)", TO_PROC_NAME(S_mix), mixinfile, beg, file_channel, b2s(false), b2s(delete_file)));
#endif
}


int mix_file(off_t beg, off_t num, int chans, chan_info **cps, const char *mixinfile, file_delete_t temp, const char *origin, bool with_tag, int start_chan)
{
  /* used in mix_selection and paste(mix)_region, and in mix_complete_file */

  int i, id = MIX_FILE_NO_MIX, in_chans;
  char *new_origin = NULL;

  in_chans =  mus_sound_chans(mixinfile);
  if (chans > in_chans) chans = in_chans;

  if (temp == MULTICHANNEL_DELETION)
    remember_temp(mixinfile, in_chans);

  for (i = 0; i < chans; i++) 
    {
      chan_info *cp;
      cp = cps[i];

      if ((!with_tag) ||
	  (!virtual_mix_ok(cp, cp->edit_ctr)))
	{
	  /* not a virtual mix */
	  if (!origin)
	    new_origin = untagged_mix_to_string(mixinfile, beg, start_chan + i, temp != DONT_DELETE_ME);
	  else new_origin = copy_string(origin);
	  mix_file_untagged(mixinfile, i + start_chan, cp, beg, num, temp, new_origin);
	}
      else 
	{
	  /* virtual mix */
	  int cur_id;
	  if (!origin)
	    new_origin = tagged_mix_to_string(mixinfile, beg, start_chan + i, temp != DONT_DELETE_ME);
	  else new_origin = copy_string(origin);
	  cur_id = mix_file_with_tag(cp, mixinfile, i + start_chan, beg, temp, new_origin);
	  if (id == MIX_FILE_NO_MIX) id = cur_id;
	}

      if (new_origin) 
	{
	  FREE(new_origin); 
	  new_origin = NULL;
	}
    }

  for (i = 0; i < chans; i++) 
    update_graph(cps[i]);
  return(id);
}



static mix_state *free_mix_state(mix_state *ms)
{
  if (ms)
    {
      if (ms->amp_env) 
	ms->amp_env = free_env(ms->amp_env);
      FREE(ms);
    }
  return(NULL);
}


static mix_state *make_mix_state(int id, int index, off_t beg, off_t len)
{
  mix_state *ms;
  ms = (mix_state *)CALLOC(1, sizeof(mix_state));
  ms->mix_id = id;
  ms->scaler = 1.0;
  ms->speed = 1.0;
  ms->beg = beg;
  ms->len = len;
  ms->amp_env = NULL;
  ms->index = index;
  return(ms);
}


mix_state *copy_mix_state(mix_state *old_ms)
{
  mix_state *ms;
  ms = (mix_state *)CALLOC(1, sizeof(mix_state));
  ms->mix_id = old_ms->mix_id;
  ms->scaler = old_ms->scaler;
  ms->speed = old_ms->speed;
  ms->beg = old_ms->beg;
  ms->len = old_ms->len;
  ms->index = old_ms->index;
  ms->amp_env = copy_env(old_ms->amp_env); /* this is the amp env (not the peak env) */
  return(ms);
}


/* this is the edit list header for the list of mix states: ed_list->mixes (void* in snd-1.h) */
typedef struct {
  int size;
  mix_state **list;
} mix_list;

void free_ed_mixes(void *ptr)
{
  if (ptr)
    {
      int i;
      mix_list *mxl = (mix_list *)ptr;
      for (i = 0; i < mxl->size; i++)
	if (mxl->list[i])
	  mxl->list[i] = free_mix_state(mxl->list[i]);
      FREE(mxl->list);
      FREE(mxl);
    }
}


void add_ed_mix(ed_list *ed, mix_state *ms)
{
  mix_list *mxl;
  int loc = -1;
  if (!(ed->mixes))
    {
      ed->mixes = (mix_list *)CALLOC(1, sizeof(mix_list));
      mxl = (mix_list *)(ed->mixes);
      mxl->size = 2;
      mxl->list = (mix_state **)CALLOC(mxl->size, sizeof(mix_state *));
      loc = 0;
    }
  else
    {
      int i;
      mxl = (mix_list *)(ed->mixes);
      for (i = 0; i < mxl->size; i++)
	if (mxl->list[i] == NULL)
	  {
	    loc = i;
	    break;
	  }
      if (loc == -1)
	{
	  loc = mxl->size;
	  mxl->size *= 2;
	  mxl->list = (mix_state **)REALLOC(mxl->list, mxl->size * sizeof(mix_state *));
	  for (i = loc; i < mxl->size; i++) mxl->list[i] = NULL;
	}
    }
  mxl->list[loc] = ms;
}


void preload_mixes(mix_state **mixes, int low_id, ed_list *ed)
{
  mix_list *mxl;
  mxl = (mix_list *)(ed->mixes);
  if (mxl)
    {
      int i;
      for (i = 0; i < mxl->size; i++)
	if (mxl->list[i])
	  mixes[mxl->list[i]->mix_id - low_id] = mxl->list[i];
    }
}


static mix_state *ed_mix_state(ed_list *ed, int mix_id)
{
  mix_list *mxl;
  mxl = (mix_list *)(ed->mixes);
  if (mxl)
    {
      int i;
      for (i = 0; i < mxl->size; i++)
	if ((mxl->list[i]) &&
	    (mxl->list[i]->mix_id == mix_id))
	  return(mxl->list[i]);
    }
  return(NULL);
}


/* these are the nominally unchanging fields in a mix (they don't follow the edit lists) */

#define MIX_TAG_ERASED -1

typedef struct {
  int id;
  char *name;
  chan_info *cp;
  int original_index;
  char *in_filename;
  off_t in_samps;
  int in_chan;
  int tag_x, tag_y;
  int sync;
  file_delete_t temporary;     /* in-filename was written by us and needs to be deleted when mix state is deleted */
  peak_env_info *peak_env;
  XEN properties;
  int properties_gc_loc;
  color_t color;
  int x, y;  /* these are needed to know where to erase while dragging the tag */
} mix_info;


static mix_state *current_mix_state(mix_info *md)
{
  if (md)
    return(ed_mix_state(md->cp->edits[md->cp->edit_ctr], md->id));
  return(NULL);
}


#if 0
/* if edpos args to various mix field (getters anyway), mix? mixes make-mix-sample-reader... */

static mix_state *mix_state_at_edpos(mix_info *md, int edpos)
{
  if (md)
    {
      if (edpos == AT_CURRENT_EDIT_POSITION)
	return(current_mix_state(md));
      else
	{
	  chan_info *cp;
	  cp = md->cp;
	  if ((edpos >= 0) && 
	      (edpos < cp->edit_size) &&
	      (cp->edits[edpos]))
	    return(ed_mix_state(cp->edits[edpos], md->id));
	}
    }
  return(NULL);
}
#endif


/* for ease of access, all tagged mixes that are still accessible (perhaps via undo from fixed state, etc)
 *   are saved in an array indexed by the mix id.  A mix is "ok" if it's still in this array, and "active"
 *   if it's represented in the current edit's mix list.
 */

#define MIX_INFO_INCREMENT 16
static mix_info **mix_infos = NULL;
static int mix_infos_size = 0;

bool mix_exists(int n) 
{
  return((n >= 0) && 
	 (n < mix_infos_size) &&
	 (mix_infos[n]));
}


bool mix_is_active(int n)
{
  return((mix_exists(n)) &&
	 (current_mix_state(mix_infos[n])));
}


static mix_info *md_from_id(int n) 
{
  if (mix_exists(n))
    return(mix_infos[n]);
  return(NULL);
}


int any_mix_id(void)
{
  int i;
  for (i = 0; i < mix_infos_ctr; i++) 
    if (mix_is_active(i)) 
      return(i);
  return(INVALID_MIX_ID);
}


int next_mix_id(int id)
{
  int i;
  for (i = id + 1; i < mix_infos_ctr; i++) 
    if (mix_is_active(i)) 
      return(i);
  return(INVALID_MIX_ID);
}


int previous_mix_id(int id)
{
  int i, top;
  top = id - 1;
  if (top >= mix_infos_ctr) top = mix_infos_ctr - 1;
  for (i = top; i >= 0; i--) 
    if (mix_is_active(i)) 
      return(i);
  return(INVALID_MIX_ID);
}


static int last_lowest_id = 0;

int lowest_mix_id(void)
{
  int i;
  for (i = last_lowest_id; i < mix_infos_ctr; i++) 
    if (mix_infos[i])
      {
	last_lowest_id = i;
	return(i);
      }
  return(INVALID_MIX_ID);
}


int highest_mix_id(void)
{
  int i;
  for (i = mix_infos_ctr - 1; i >= 0; i--)
    if (mix_infos[i])
      return(i);
  return(INVALID_MIX_ID);
}


static mix_info *free_mix_info(mix_info *md)
{
  if (md)
    {
      if (md->name) {FREE(md->name); md->name = NULL;}
      mix_infos[md->id] = NULL;
      if (md->temporary == DELETE_ME)
	{
	  if (mus_file_probe(md->in_filename))
	    snd_remove(md->in_filename, REMOVE_FROM_CACHE);
	}
      if (md->in_filename) {FREE(md->in_filename); md->in_filename = NULL;}
      if (md->properties_gc_loc != NOT_A_GC_LOC)
	{
	  snd_unprotect_at(md->properties_gc_loc);
	  md->properties_gc_loc = NOT_A_GC_LOC;
	  md->properties = XEN_FALSE;
	}
      if (md->peak_env)
	md->peak_env = free_peak_env_info(md->peak_env);
      FREE(md);
    }
  return(NULL);
}


void free_channel_mixes(chan_info *cp)
{
  /* called in snd-data.c during chan_info cleanup */
  int i;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      mix_info *md;
      md = mix_infos[i];
      if ((md) && (md->cp == cp))
	mix_infos[i] = free_mix_info(md);
    }
}


void reset_mix_ctr(void)
{
  mix_infos_ctr = 0;
}


char *mix_name(int id)
{
  if (mix_exists(id))
    return(mix_infos[id]->name);
  return(NULL);
}


int mix_name_to_id(const char *name)
{
  int i, loc_so_far = -1;
  chan_info *selected_cp = NULL;
  selected_cp = selected_channel();
  for (i = 0; i < mix_infos_size; i++)
    if ((mix_infos[i]) &&
	(mix_infos[i]->name) &&
	(strcmp(mix_infos[i]->name, name) == 0))
      {
	if ((!selected_cp) ||
	    (mix_infos[i]->cp == selected_cp))  /* try to find mix in the currently selected channel (possible name collisions) */
	  return(i);
	if (loc_so_far == -1)
	  loc_so_far = i;
      }
  return(loc_so_far);
}


static mix_info *make_mix_info(chan_info *cp)
{
  mix_info *md;
  if (mix_infos == NULL)
    {
      mix_infos_size = MIX_INFO_INCREMENT;
      mix_infos = (mix_info **)CALLOC(mix_infos_size, sizeof(mix_info *));
    }
  else
    {
      if (mix_infos_ctr >= mix_infos_size)
	{
	  int i;
	  mix_infos_size += MIX_INFO_INCREMENT;
	  mix_infos = (mix_info **)REALLOC(mix_infos, mix_infos_size * sizeof(mix_info *));
	  for (i = mix_infos_size - MIX_INFO_INCREMENT; i < mix_infos_size; i++) 
	    mix_infos[i] = NULL;
	}
    }
  md = (mix_info *)CALLOC(1, sizeof(mix_info));
#if MUS_DEBUGGING
  if (mix_infos[mix_infos_ctr])
    fprintf(stderr, "mix[%d] still exists! %p\n", mix_infos_ctr, mix_infos[mix_infos_ctr]);
#endif
  mix_infos[mix_infos_ctr] = md;
  md->id = mix_infos_ctr++;
  md->cp = cp;
  md->temporary = DONT_DELETE_ME;
  md->color = ss->sgx->mix_color;
  md->tag_y = 0;
  md->tag_x = 0;
  md->name = NULL;
  md->y = MIX_TAG_ERASED;
  md->peak_env = NULL;
  md->properties_gc_loc = NOT_A_GC_LOC;
  md->properties = XEN_FALSE;
  return(md);
}


mix_state *prepare_mix_state_for_channel(chan_info *cp, int mix_loc, off_t beg, off_t len)
{
  mix_info *md;
  md = make_mix_info(cp);  /* make the mix_info data for this virtual mix */
  md->original_index = mix_loc;
  md->in_samps = len;
  return(make_mix_state(md->id, mix_loc, beg, len)); 
}


static void map_over_channel_mixes(chan_info *cp, void (*func)(mix_info *umx))
{
  int i;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      mix_info *md;
      md = mix_infos[i];
      if ((md) && (md->cp == cp))
	(*func)(md);
    }
}


bool channel_has_mixes(chan_info *cp)
{
  int i;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      mix_info *md;
      md = mix_infos[i];
      if ((md) && 
	  (md->cp == cp))
	return(true);
    }
  return(false);
}


bool channel_has_active_mixes(chan_info *cp)
{
  return(cp->edits[cp->edit_ctr]->mixes != NULL);
}


static void remove_temporary_mix_file(mix_info *md)
{
  if ((md->temporary == DELETE_ME) &&
      (mus_file_probe(md->in_filename)))
    snd_remove(md->in_filename, REMOVE_FROM_CACHE);
}


void delete_any_remaining_mix_temp_files_at_exit(chan_info *cp)
{
  map_over_channel_mixes(cp, remove_temporary_mix_file);
}



static int compare_mix_positions(const void *umx1, const void *umx2)
{
  off_t mx1, mx2;
  mx1 = (*((off_t *)umx1));
  mx2 = (*((off_t *)umx2));
  if (mx1 > mx2) return(1);
  if (mx1 == mx2) return(0);
  return(-1);
}


void goto_mix(chan_info *cp, int count)
{
  /* C-x C-j */
  mix_list *mxl;
  if (!cp) return;
  mxl = (mix_list *)(cp->edits[cp->edit_ctr]->mixes); /* active mixes in the current edit of this channel */
  if (mxl)
    {
      int i, k = 0;
      for (i = 0; i < mxl->size; i++)
	if (mxl->list[i])
	  k++;
      if (k > 0)
	{
	  int j = 0;
	  off_t *begs;
	  begs = (off_t *)CALLOC(k, sizeof(off_t));
	  for (i = 0; i < mxl->size; i++)
	    if (mxl->list[i])
	      begs[j++] = mxl->list[i]->beg;
	  if (k == 1)
	    cursor_moveto(cp, begs[0]);
	  else
	    {
	      qsort((void *)begs, j, sizeof(off_t), compare_mix_positions);
	      /* now find where we are via CURSOR(cp) and go forward or back as per count */
	      if (count > 0)
		{
		  for (i = 0; i < j; i++)
		    if (begs[i] > CURSOR(cp))
		      {
			count--;
			if (count == 0)
			  {
			    cursor_moveto(cp, begs[i]);
			    break;
			  }
		      }
		  if ((count > 0) && (CURSOR(cp) < begs[j - 1]))
		    cursor_moveto(cp, begs[j - 1]);
		}
	      else
		{
		  for (i = j - 1; i >= 0; i--)
		    if (begs[i] < CURSOR(cp))
		      {
			count++;
			if (count == 0)
			  {
			    cursor_moveto(cp, begs[i]);
			    break;
			  }
		      }
		  if ((count < 0) && (CURSOR(cp) > begs[0]))
		    cursor_moveto(cp, begs[0]);
		}
	    }
	  FREE(begs);
	}
    }
}


off_t zoom_focus_mix_in_channel_to_position(chan_info *cp)
{
  mix_list *mxl;
  mxl = (mix_list *)(cp->edits[cp->edit_ctr]->mixes);
  if (mxl)
    {
      off_t lo, hi;
      mix_state *ms;
      int i;
      lo = cp->axis->losamp;
      hi = cp->axis->hisamp;
      for (i = 0; i < mxl->size; i++)
	{
	  ms = mxl->list[i];
	  if ((ms) &&
	      (ms->beg >= lo) && 
	      (ms->beg <= hi))
	    return(ms->beg);
	}
    }
  return(-1);
}


/* follow edit list */
off_t mix_position_from_id(int id)
{
  mix_state *ms;
  ms = current_mix_state(md_from_id(id));
  if (ms)
    return(ms->beg);
  return(0);
}


off_t mix_length_from_id(int id)
{
  mix_state *ms;
  ms = current_mix_state(md_from_id(id));
  if (ms)
    return(ms->len);
  return(0);
}


Float mix_amp_from_id(int id)
{
  mix_state *ms;
  ms = current_mix_state(md_from_id(id));
  if (ms)
    return(ms->scaler);
  return(0.0);
}


Float mix_speed_from_id(int id)
{
  mix_state *ms;
  ms = current_mix_state(md_from_id(id));
  if (ms)
    return(ms->speed);
  return(0.0);
}


env *mix_amp_env_from_id(int id)
{
  mix_state *ms;
  ms = current_mix_state(md_from_id(id));
  if (ms)
    return(ms->amp_env);
  return(NULL);
}


/* stable (not in edit list) */

static int mix_sync_from_id(int id)
{
  mix_info *md;
  md = md_from_id(id);
  if (md)
    return(md->sync);
  return(0);
}


static int current_mix_sync_max = 0;

static int mix_set_sync_from_id(int id, int new_sync)
{
  mix_info *md;
  md = md_from_id(id);
  if (md)
    {
      md->sync = new_sync;
      if (new_sync > current_mix_sync_max)
	current_mix_sync_max = new_sync;
      return(md->sync);
    }
  return(0);
}


static char *mix_name_from_id(int id)
{
  mix_info *md;
  md = md_from_id(id);
  if (md)
    return(md->name);
  return(0);
}


static char *mix_set_name_from_id(int id, const char *new_name)
{
  mix_info *md;
  md = md_from_id(id);
  if (md)
    {
      if (md->name) FREE(md->name);
      md->name = copy_string(new_name);
    }
  return(0);
}


chan_info *mix_chan_info_from_id(int id)
{
  mix_info *md;
  md = md_from_id(id);
  if (md)
    return(md->cp);
  return(NULL);
}


static int mix_tag_y_from_id(int id)
{
  mix_info *md;
  md = md_from_id(id);
  if (md)
    return(md->tag_y);
  return(0);
}


static color_t mix_color_from_id(int mix_id)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if (md)
    return(md->color);
  return(ss->sgx->mix_color);
}


static color_t mix_set_color_from_id(int id, color_t new_color)
{
  mix_info *md;
  md = md_from_id(id);
  if (md)
    md->color = new_color;
  return(new_color);
}



bool mix_set_amp_edit(int id, Float amp)
{
  mix_info *md;
  bool edited = false;
  mix_state *old_ms = NULL;
  md = md_from_id(id);
  if (md) old_ms = current_mix_state(md); /* needed for edit bounds and existence check */
  if (old_ms)
    {
      if (old_ms->scaler != amp)
	{
	  mix_state *ms;
	  char *origin;
#if HAVE_FORTH
	  origin = mus_format("-mix-%d %.4f set-mix-amp", id, amp);
#endif
#if HAVE_SCHEME
	  origin = mus_format("(set! (mix-amp -mix-%d) %.4f)", id, amp);
#endif
#if HAVE_RUBY
	  origin = mus_format("set_mix_amp(_mix_%d, %.4f)", id, amp);
#endif
	  edited = begin_mix_op(md->cp, old_ms->beg, old_ms->len, old_ms->beg, old_ms->len, md->cp->edit_ctr, origin);
	  FREE(origin);
	  if (edited)
	    {
	      ms = current_mix_state(md);         /* this is the new copy reflecting this edit */
	      ms->scaler = amp;
	      end_mix_op(md->cp, 0, 0);
	    }
	}
    }
  return(edited);
}


static Float src_input(void *arg, int direction)
{
  return(read_sample((snd_fd *)arg));
}


static int remake_mix_data(mix_state *ms, mix_info *md)
{
  chan_info *cp;
  off_t len;
  snd_fd *mix_reader;
  Float old_amp;
  mus_any *egen = NULL, *src_gen = NULL;
  env *e;

  cp = md->cp;
  old_amp = ms->scaler;
  ms->scaler = 1.0;
  len = snd_round_off_t((double)(md->in_samps) / (double)(ms->speed));
  e = ms->amp_env;

  mix_reader = make_virtual_mix_reader(cp, 0, md->in_samps, md->original_index, 1.0, READ_FORWARD);

  if (e)
    egen = mus_make_env(e->data, e->pts, 1.0, 0.0, 1.0, 0.0, len - 1, NULL);
  if (ms->speed != 1.0)
    src_gen = mus_make_src(&src_input, ms->speed, sinc_width(ss), (void *)mix_reader);

  prepare_sound_list(cp);
  if (cp->sounds[md->original_index]->type == SND_DATA_BUFFER)
    {
      int i;
      mus_sample_t *new_buffer;
      new_buffer = (mus_sample_t *)MALLOC(len * sizeof(mus_sample_t));
      if (!src_gen)
	{
	  for (i = 0; i < len; i++)
	    new_buffer[i] = MUS_FLOAT_TO_SAMPLE(mus_env(egen) * read_sample(mix_reader));
	}
      else
	{
	  if (!egen)
	    {
	      for (i = 0; i < len; i++)
		new_buffer[i] = MUS_FLOAT_TO_SAMPLE(mus_src(src_gen, 0.0, &src_input));
	    }
	  else
	    {
 	      for (i = 0; i < len; i++)
		new_buffer[i] = MUS_FLOAT_TO_SAMPLE(mus_src(src_gen, 0.0, &src_input) * mus_env(egen));
	    }
	}
      cp->sounds[cp->sound_ctr] = make_snd_data_buffer(new_buffer, (int)len, cp->edit_ctr);
      FREE(new_buffer);
    }
  else
    {
      off_t i;
      file_info *hdr;
      int fd, err = 0;
      char *temp_file;
      io_error_t io_err = IO_NO_ERROR;
      mus_sample_t **data;
      mus_sample_t *new_buffer;
      int j = 0;
      
      temp_file = snd_tempnam();
      hdr = make_temp_header(temp_file, SND_SRATE(cp->sound), 1, len, S_setB S_mix_amp_env);
      fd = open_temp_file(temp_file, 1, hdr, &io_err);
      data = (mus_sample_t **)MALLOC(sizeof(mus_sample_t *));
      new_buffer = (mus_sample_t *)MALLOC(MAX_BUFFER_SIZE * sizeof(mus_sample_t));
      data[0] = new_buffer;

      if (!src_gen)
	{
	  for (i = 0; i < len; i++)
	    {
	      new_buffer[j++] = MUS_FLOAT_TO_SAMPLE(read_sample(mix_reader) * mus_env(egen));
	      if (j == MAX_BUFFER_SIZE)
		{
		  err = mus_file_write(fd, 0, j - 1, 1, data);
		  j = 0;
		  if (err == -1) break;
		}
	    }
	}
      else
	{
	  if (!egen)
	    {
	      for (i = 0; i < len; i++)
		{
		  new_buffer[j++] = MUS_FLOAT_TO_SAMPLE(mus_src(src_gen, 0.0, &src_input));
		  if (j == MAX_BUFFER_SIZE)
		    {
		      err = mus_file_write(fd, 0, j - 1, 1, data);
		      j = 0;
		      if (err == -1) break;
		    }
		}
	    }
	  else
	    {
	      for (i = 0; i < len; i++)
		{
		  new_buffer[j++] = MUS_FLOAT_TO_SAMPLE(mus_src(src_gen, 0.0, &src_input) * mus_env(egen));
		  if (j == MAX_BUFFER_SIZE)
		    {
		      err = mus_file_write(fd, 0, j - 1, 1, data);
		      j = 0;
		      if (err == -1) break;
		    }
		}
	    }
	}
      if (j > 0) mus_file_write(fd, 0, j - 1, 1, data);
      close_temp_file(temp_file, fd, hdr->type, len * mus_bytes_per_sample(hdr->format));
      free_file_info(hdr);
      
      hdr = make_file_info(temp_file, FILE_READ_ONLY, FILE_NOT_SELECTED);
      fd = snd_open_read(temp_file);
      snd_file_open_descriptors(fd,
				temp_file,
				hdr->format,
				hdr->data_location,
				hdr->chans,
				hdr->type);
      cp->sounds[cp->sound_ctr] = make_snd_data_file(temp_file, 
						     make_file_state(fd, hdr, 0, 0, FILE_BUFFER_SIZE),
						     hdr, DELETE_ME, cp->edit_ctr, 0);
      FREE(new_buffer);
      FREE(data);
    }
  
  free_snd_fd(mix_reader);
  if (egen) mus_free(egen);
  if (src_gen) mus_free(src_gen);
  
  ms->scaler = old_amp;
  return(cp->sound_ctr);
}


bool mix_set_amp_env_edit(int id, env *e)
{
  mix_info *md;
  bool edited = false;
  mix_state *old_ms = NULL;
  md = md_from_id(id);
  if (md) old_ms = current_mix_state(md); /* needed for edit bounds and existence check */
  if (old_ms)
    {
      if (!(envs_equal(old_ms->amp_env, e)))
	{
	  chan_info *cp;
	  mix_state *ms;
	  char *origin, *envstr;
	  
	  envstr = env_to_string(e);
#if HAVE_FORTH
	  origin = mus_format("-mix-%d %s set-mix-amp-env", id, envstr);
#endif
#if HAVE_SCHEME
	  origin = mus_format("(set! (mix-amp-env -mix-%d) %s)", id, envstr);
#endif
#if HAVE_RUBY
	  origin = mus_format("set_mix_amp_env(_mix_%d, %s)", id, envstr);
#endif
	  FREE(envstr);

	  cp = md->cp;
	  edited = begin_mix_op(cp, old_ms->beg, old_ms->len, old_ms->beg, old_ms->len, cp->edit_ctr, origin); /* this does not change beg or len */
	  FREE(origin);
	  if (edited)
	    {
	      ms = current_mix_state(md);         /* this is the new copy reflecting this edit */
	      if (ms->amp_env) free_env(ms->amp_env);
	      ms->amp_env = copy_env(e);

	      /* can't use mus_env (as the reader op) here because we need to run backwards */
	      if ((e) || (ms->speed != 1.0))
		ms->index = remake_mix_data(ms, md);
	      else ms->index = md->original_index;
	  
	      end_mix_op(cp, 0, 0);
	    }
	}
    }
  return(edited);
}


bool mix_set_position_edit(int id, off_t pos)
{
  mix_info *md;
  bool edited = false;
  mix_state *old_ms = NULL;
  if (pos < 0) pos = 0;
  md = md_from_id(id);
  if (md) old_ms = current_mix_state(md);
  if (old_ms)
    {
      if (old_ms->beg != pos)
	{
	  mix_state *ms;
	  char *origin;
#if HAVE_FORTH
	  origin = mus_format("-mix-%d " OFF_TD " set-mix-position", id, pos);
#endif
#if HAVE_SCHEME
	  origin = mus_format("(set! (mix-position -mix-%d) " OFF_TD ")", id, pos);
#endif
#if HAVE_RUBY
	  origin = mus_format("set_mix_position(_mix_%d, " OFF_TD ")", id, pos);
#endif
	  edited = begin_mix_op(md->cp, old_ms->beg, old_ms->len, pos, old_ms->len, md->cp->edit_ctr, origin); /* this does not change beg or len */

	  FREE(origin);
	  if (edited)
	    {
	      ms = current_mix_state(md);         /* this is the new copy reflecting this edit */
	      unmix(md->cp, ms);
	      ms->beg = pos;
	      remix(md->cp, ms);
	      end_mix_op(md->cp, (old_ms->beg != pos) ? old_ms->beg : 0, old_ms->len);
	    }
	}
    }
  return(edited);
}


bool mix_set_speed_edit(int id, Float spd)
{
  mix_info *md;
  bool edited = false;
  mix_state *old_ms = NULL;
  md = md_from_id(id);
  if (md) old_ms = current_mix_state(md); /* needed for edit bounds and existence check */
  if (old_ms)
    {
      if (old_ms->speed != spd)
	{
	  chan_info *cp;
	  mix_state *ms;
	  off_t len;
	  char *origin;
#if HAVE_FORTH
	  origin = mus_format("-mix-%d %.4f set-mix-speed", id, spd);
#endif
#if HAVE_SCHEME
	  origin = mus_format("(set! (mix-speed -mix-%d) %.4f)", id, spd);
#endif
#if HAVE_RUBY
	  origin = mus_format("set_mix_speed(_mix_%d, %.4f)", id, spd);
#endif
	  cp = md->cp;
	  len = snd_round_off_t((double)(md->in_samps) / (double)spd);
	  edited = begin_mix_op(cp, old_ms->beg, old_ms->len, old_ms->beg, len, cp->edit_ctr, origin);
	  
	  FREE(origin);
	  if (edited)
	    {
	      ms = current_mix_state(md);         /* this is the new copy reflecting this edit */
	      unmix(cp, ms);                      /*   but unmix before changing mix length! */

	      ms->speed = spd;
	      ms->len = len;
	      if ((ms->speed != 1.0) || (ms->amp_env))
		ms->index = remake_mix_data(ms, md);
	      else ms->index = md->original_index;
	      
	      remix(cp, ms);
	      end_mix_op(cp, 0, 0); /* old_ms->beg, old_ms->len); */
	    }
	}
    }
  return(edited);
}

/* mix-samples/set-mix-samples? 
 *   combine mix_set_amp_env_edit with remake_mix_data accepting either vct or filename
 *   one problem is how to handle md->peak_env: currently I think it is based on the original
 *     and src stretches/mix-env uses env-on-env, so it's never remade.
 */


/* edit-list->function support for mixes:
 *      mix list search for current channel, make outer let holding all names as (-mix-### ###)
 *      origins for make-mix procs: (set! -mix-### (...))
 *      origins otherwise, reference to mix: -mix-###
 */

char *edit_list_mix_init(chan_info *cp)
{
  char *new_list = NULL, *old_list = NULL;
  mix_list *mxl;
  mxl = (mix_list *)(cp->edits[cp->edit_ctr]->mixes);
  if (mxl)
    {
      int i;
      for (i = 0; i < mxl->size; i++)
	if (mxl->list[i])
	  {
	    int id;
	    id = mxl->list[i]->mix_id;

	    old_list = new_list;
#if HAVE_SCHEME
	    new_list = mus_format("%s%s(-mix-%d %d)", 
				  (old_list) ? old_list : "", 
				  (old_list) ? " " : "",  /* strcat of previous + possible space */
				  id, id);                  
#endif
#if HAVE_RUBY
	    new_list = mus_format("%s%s_mix_%d = %d", 
				  (old_list) ? old_list : "", 
				  (old_list) ? "; " : "",  /* strcat of previous + possible space */
				  id, id);                  
#endif
#if HAVE_FORTH
	    new_list = mus_format("%s%s%d { -mix-%d }", 
				  (old_list) ? old_list : "", 
				  (old_list) ? " " : "",   /* strcat of previous + possible space */
				  id, id);                  
#endif
	    if (old_list) FREE(old_list);
	  }
    }
  return(new_list);
}


#define MIX_TAG_Y_OFFSET mix_tag_height(ss)

int hit_mix(chan_info *cp, int x, int y) /* mix tag press in snd-chn.c */
{
  #define SLOPPY_MOUSE 3
  mix_list *mxl;
  mxl = (mix_list *)(cp->edits[cp->edit_ctr]->mixes); /* active mixes in the current edit of this channel */
  if (mxl)
    {
      int i, width, height;
      width = mix_tag_width(ss);
      height = mix_tag_height(ss);
      for (i = 0; i < mxl->size; i++)
	{
	  mix_state *ms;
	  ms = mxl->list[i];
	  if (ms)
	    {
	      int mx, my;
	      mx = mix_infos[ms->mix_id]->tag_x;
	      if (mx <= 0)
		mx = grf_x((double)(ms->beg) / (double)(SND_SRATE(cp->sound)), cp->axis);
	      my = mix_infos[ms->mix_id]->tag_y + MIX_TAG_Y_OFFSET + cp->axis->y_offset;
	      if ((x + SLOPPY_MOUSE >= (mx - width / 2)) && 
		  (x - SLOPPY_MOUSE <= (mx + width / 2)) &&
		  (y + SLOPPY_MOUSE >= (my - height)) && 
		  (y - SLOPPY_MOUSE <= (my + 0)))
		return(ms->mix_id);
	    }
	}
    }
  return(NO_MIX_TAG);
}



/* mix display */

void channel_set_mix_tags_erased(chan_info *cp)
{
  mix_list *mxl;
  mxl = (mix_list *)(cp->edits[cp->edit_ctr]->mixes);
  if (mxl)
    {
      int i;
      for (i = 0; i < mxl->size; i++)
	if (mxl->list[i])
	  {
	    mix_info *md;
	    md = mix_infos[mxl->list[i]->mix_id];
	    md->y = MIX_TAG_ERASED;
	  }
    }
}


static XEN draw_mix_hook;

static void draw_mix_tag(mix_info *md, int x, int y)
{
  chan_info *cp;
  int width, height;
  axis_context *ax;
  char *lab = NULL;

#if USE_MOTIF
  #define STRING_Y_OFFSET 3
  #define STRING_HEIGHT 12
#else
  #define STRING_Y_OFFSET -8
  #define STRING_HEIGHT 12
#endif

  if (XEN_HOOKED(draw_mix_hook))
    {
      XEN res;
      res = run_progn_hook(draw_mix_hook,
			   XEN_LIST_5(C_TO_XEN_INT(md->id), 
				      C_TO_XEN_INT(md->x),
				      C_TO_XEN_INT(md->y),
				      C_TO_XEN_INT(x),
				      C_TO_XEN_INT(y)),
			   S_draw_mix_hook);
      if (!(XEN_FALSE_P(res)))
	{
	  md->x = x;
	  md->y = y;
	  if (XEN_LIST_P(res))
	    {
	      md->tag_x = XEN_TO_C_INT(XEN_CAR(res));
	      md->tag_y = XEN_TO_C_INT(XEN_CADR(res));
	    }
	  return;
	}
    }

  cp = md->cp;

  /* draw the mix tag */
  width = mix_tag_width(ss);
  height = mix_tag_height(ss);

  if (md->y != MIX_TAG_ERASED)
    {
      /* erase old tag and name */
      ax = erase_context(cp);
      fill_rectangle(ax, md->x - width / 2 - 1, md->y - height - 1, width + 2, height + STRING_HEIGHT);
      md->y = MIX_TAG_ERASED;
    }

  md->x = x;
  md->y = y;

  ax = mix_waveform_context(cp);
  set_foreground_color(ax, md->color); 
  fill_rectangle(ax, x - width / 2, y - height, width, height);

  /* redraw the mix id underneath the tag */
  set_tiny_numbers_font(cp);
  if (cp->printing) ps_set_tiny_numbers_font();

  if (md->name)
    lab = copy_string(md->name);
  else
    {
      lab = (char *)CALLOC(16, sizeof(char));
      mus_snprintf(lab, 16, "%d", md->id);
    }
  ax = copy_context(cp);
  draw_string(ax, x - width / 2, y + height / 2 + STRING_Y_OFFSET, lab, strlen(lab));
  if (cp->printing) ps_draw_string(cp->axis, x - width / 2, y + height / 2 + STRING_Y_OFFSET, lab);

  if (lab) {FREE(lab); lab = NULL;}
}


static int local_grf_x(double val, axis_info *ap)
{
  if (val >= ap->x1) return(ap->x_axis_x1);
  if (val <= ap->x0) return(ap->x_axis_x0);
  return((int)(ap->x_base + val * ap->x_scale));
}


#define MIX_PEAK_ENV_CUTOFF 20000

static peak_env_info *make_mix_input_peak_env(mix_info *md)
{
  mix_state *ms;
  ms = current_mix_state(md);
  if (ms->len >= MIX_PEAK_ENV_CUTOFF)
    {
      peak_env_info *ep;
      snd_fd *sf;
      int val, sb = 0;
      off_t n;

      ep = (peak_env_info *)CALLOC(1, sizeof(peak_env_info));
      val = (int)(log((double)(ms->len)));
      if (val > 20) val = 20;
      ep->peak_env_size = snd_int_pow2(val);
      ep->samps_per_bin = (int)(ceil((double)(ms->len) / (double)(ep->peak_env_size)));
      ep->data_max = (Float *)CALLOC(ep->peak_env_size, sizeof(Float));
      ep->data_min = (Float *)CALLOC(ep->peak_env_size, sizeof(Float));
      ep->fmin = MIN_INIT;
      ep->fmax = MAX_INIT;

      sf = make_virtual_mix_reader(md->cp, 0, ms->len, ms->index, 1.0, READ_FORWARD);

      for (n = 0; n < ms->len; n += ep->samps_per_bin)
	{
	  Float ymin, ymax, val;
	  int i;
	  val = read_sample(sf);
	  ymin = val;
	  ymax = val;
	  for (i = 1; i < ep->samps_per_bin; i++)
	    {
	      val = read_sample(sf);
	      if (ymin > val) 
		ymin = val; 
	      else 
		if (ymax < val) 
		  ymax = val;
	    }
	  ep->data_max[sb] = ymax;
	  ep->data_min[sb++] = ymin;
	  if (ymin < ep->fmin) ep->fmin = ymin;
	  if (ymax > ep->fmax) ep->fmax = ymax;
	}

      ep->completed = true;
      free_snd_fd(sf);
      return(ep);
    }
  return(NULL);
}


static bool mix_input_peak_env_usable(mix_info *md, Float samples_per_pixel) 
{
  if (!(md->peak_env))
    md->peak_env = make_mix_input_peak_env(md);
  return((md->peak_env) && 
	 (samples_per_pixel >= (Float)(md->peak_env->samps_per_bin)));
}


static peak_env_info *env_on_env(env *e, peak_env_info *peaks)
{
  peak_env_info *ep;
  ep = copy_peak_env_info(peaks, false);
  if (ep)
    {
      int i;
      Float val;
      mus_any *me;
      me = mus_make_env(e->data, e->pts, 1.0, 0.0, e->base, 0.0, ep->peak_env_size - 1, NULL);
      for (i = 0; i < ep->peak_env_size; i++) 
	{
	  val = mus_env(me);
	  if (val >= 0.0)
	    {
	      ep->data_min[i] = ep->data_min[i] * val;
	      ep->data_max[i] = ep->data_max[i] * val;
	    }
	  else
	    {
	      ep->data_min[i] = ep->data_max[i] * val;
	      ep->data_max[i] = ep->data_min[i] * val;
	    }
	}
      mus_free(me);
    }
  return(ep);
}


static int prepare_mix_peak_env(mix_info *md, Float scl, int yoff, off_t newbeg, off_t newend, double srate, axis_info *ap)
{
  int i, j, mix_start;
  int lastx, newx;
  double xend, xstart, xstep, mix_samps_per_bin;
  peak_env_info *ep;
  env *amp_env;
  Float ymin = 0.0, ymax = 0.0;

  amp_env = mix_amp_env_from_id(md->id);
  if (!amp_env)
    ep = md->peak_env;
  else ep = env_on_env(amp_env, md->peak_env);

  mix_samps_per_bin = (double)(ep->samps_per_bin) / mix_speed_from_id(md->id);

  /* mix starts at newbeg, current display starts at lo,
     mix goes to newend, current display goes to hi,
  */

  if (ap->losamp > newbeg)
    {
      mix_start = snd_round((double)(ap->losamp - newbeg) / mix_samps_per_bin);
      xstart = ap->x0;
    }
  else 
    {
      mix_start = 0;
      xstart = (double)(newbeg) / srate;
    }

  if (ap->hisamp < newend)
    xend = ap->x1;
  else xend = (double)(newend) / srate;

  xstep = mix_samps_per_bin / srate;
  lastx = local_grf_x(xstart, ap);

  for (i = mix_start, j = 0; (xstart < xend) && (i < ep->peak_env_size); xstart += xstep, i++)
    {
      Float low, high;
      low = ep->data_min[i];
      high = ep->data_max[i];
      newx = local_grf_x(xstart, ap);
      if (newx > lastx)                  /* set lastx's bin (j) from min/max for that output bin */
	{
	  set_grf_points(lastx, j++,
			 (int)(yoff - scl * ymin),
			 (int)(yoff - scl * ymax));
	  if (j >= POINT_BUFFER_SIZE) break;
	  lastx = newx;
	  ymin = low;
	  ymax = high;
	}
      else
	{
	  if (high > ymax) ymax = high;
	  if (low < ymin) ymin = low;
	}
    }

  if (amp_env)
    free_peak_env_info(ep);

  return(j);
}


static int prepare_mix_waveform(mix_info *md, mix_state *ms, axis_info *ap, Float scl, int yoff, double cur_srate, bool *two_sided)
{
  off_t i, newbeg, newend;
  int pts = 0;
  off_t samps;
  bool widely_spaced;
  Float samples_per_pixel;
  double x, incr, initial_x;
  off_t lo, hi;
  snd_fd *sf = NULL;
  int x_start, x_end;
  double start_time;

  newbeg = ms->beg;
  newend = newbeg + ms->len;
  lo = ap->losamp;
  hi = ap->hisamp;
  if ((newend <= lo) || (newbeg >= hi)) return(0);
  if ((ap->y_axis_y0 - ap->y_axis_y1) < scl) return(0);
  start_time = (double)(ap->losamp) / cur_srate;
  x_start = ap->x_axis_x0;
  x_end = ap->x_axis_x1;
  if (newend > hi) newend = hi;
  samps = ap->hisamp - ap->losamp + 1;
  samples_per_pixel = (Float)((double)(samps - 1) / (Float)(x_end - x_start));

  if ((samples_per_pixel < 1.0) ||
      ((samples_per_pixel < 5.0) && 
       (samps < POINT_BUFFER_SIZE)))
    {
      int j;
      if (newbeg < lo) /* mix starts before current left x0 point */
	{
	  sf = make_virtual_mix_reader(md->cp, lo - newbeg, ms->len, ms->index, ms->scaler * scl, READ_FORWARD);
	  newbeg = lo;
	}
      else sf = make_virtual_mix_reader(md->cp, 0, ms->len, ms->index, ms->scaler * scl, READ_FORWARD);

      if (!sf) return(0);
      if (samples_per_pixel < 1.0)
	{
	  incr = 1.0 / samples_per_pixel;
	  initial_x = x_start;
	  widely_spaced = true;
	}
      else
	{
	  incr = (double)1.0 /cur_srate;
	  initial_x = start_time;
	  widely_spaced = false;
	}
      x = initial_x + (incr * (newbeg - lo));
      for (j = 0, i = newbeg; i <= newend; i++, j++, x += incr)
	{
	  int ina_i;
	  ina_i = (int)(yoff - read_sample(sf));
	  if (widely_spaced)
	    set_grf_point((int)x, j, ina_i);
	  else set_grf_point(local_grf_x(x, ap), j, ina_i);
	}
      free_snd_fd(sf);
      pts = j;
      (*two_sided) = false;
    }
  else
    {
      (*two_sided) = true;
      if (mix_input_peak_env_usable(md, samples_per_pixel))
	pts = prepare_mix_peak_env(md, scl, yoff, newbeg, newend, (double)cur_srate, ap);
      else
	{
	  int xi, j;
	  off_t endi;
	  Float ymin, ymax, xf;
	  if (newbeg < lo)
	    {
	      sf = make_virtual_mix_reader(md->cp, lo - newbeg, ms->len, ms->index, ms->scaler * scl, READ_FORWARD);
	      newbeg = lo;
	    }
	  else sf = make_virtual_mix_reader(md->cp, 0, ms->len, ms->index, ms->scaler * scl, READ_FORWARD);
	  if (!sf) return(0);

	  j = 0;      /* graph point counter */
	  x = ap->x0;
	  xi = local_grf_x(x, ap);
	  xf = 0.0;     /* samples per pixel counter */
	  ymin = 100.0;
	  ymax = -100.0;
	  if (newend < hi) endi = newend; else endi = hi;
	  for (i = lo; i < newbeg; i++)
	    {
	      xf += 1.0;
	      if (xf > samples_per_pixel)
		{
		  xi++;
		  xf -= samples_per_pixel;
		}
	    }
	  for (i = newbeg; i <= endi; i++)
	    {
	      Float ina;
	      ina = read_sample(sf);
	      if (ina > ymax) ymax = ina;
	      if (ina < ymin) ymin = ina;
	      xf += 1.0;
	      if (xf > samples_per_pixel)
		{
		  set_grf_points(xi, j,
				 (int)(yoff - ymin),
				 (int)(yoff - ymax));
		  j++;
		  ymin = 100.0;
		  ymax = -100.0;
		  xi++;
		  xf -= samples_per_pixel;
		}
	    }
	  pts = j;
	  free_snd_fd(sf);
	}
    }
  return(pts);
}


int prepare_mix_dialog_waveform(int mix_id, axis_info *ap, bool *two_sided)
{
  mix_info *md;
  mix_state *ms;
  Float scl, x0, x1, y0, y1;
  off_t old_lo, old_hi;
  double cur_srate;
  int pts;
  md = md_from_id(mix_id);
  if (!md) return(0);
  scl = ap->y_axis_y0 - ap->y_axis_y1;
  old_lo = ap->losamp;
  old_hi = ap->hisamp;
  x0 = ap->x0;
  x1 = ap->x1;
  y0 = ap->y0;
  y1 = ap->y1;
  cur_srate = (double)SND_SRATE(md->cp->sound);
  ms = current_mix_state(md);
  ap->losamp = ms->beg;
  ap->hisamp = ms->beg + ms->len;
  ap->x0 = (double)(ap->losamp) / cur_srate;
  ap->x1 = (double)(ap->hisamp) / cur_srate;
  ap->y0 = -1.0;
  ap->y1 = 1.0;
  init_axis_scales(ap);
  pts = prepare_mix_waveform(md, ms, ap, scl * .5, (int)(scl * .5), cur_srate, two_sided);
  ap->x0 = x0;
  ap->x1 = x1;
  ap->y0 = y0;
  ap->y1 = y1;
  ap->losamp = old_lo;
  ap->hisamp = old_hi;
  init_axis_scales(ap);
  return(pts);
}


static void erase_mix_tag_and_waveform(mix_state *ms, chan_info *cp, axis_info *ap, int x, int y)
{
  int wave_width, wave_height, old_x;
  wave_width = (int)(ms->len * ((double)(ap->x_axis_x1 - ap->x_axis_x0) / (double)(ap->hisamp - ap->losamp)));
  wave_height = mix_waveform_height(ss);
  old_x = x + mix_tag_width(ss) - 2;
  if ((old_x + wave_width) > ap->x_axis_x1)
    wave_width = ap->x_axis_x1 - old_x;
  fill_rectangle(erase_context(cp), old_x, (y > wave_height) ? (y - wave_height) : 0, wave_width + 2, wave_height + 2);
}


static void draw_mix_tag_and_waveform(mix_info *md, mix_state *ms, int x)
{
  bool show_wave;
  int y;
  chan_info *cp;
  axis_info *ap;

  cp = md->cp;
  ap = cp->axis;
  show_wave = ((show_mix_waveforms(ss)) && (cp->show_mix_waveforms));
  y = ap->y_offset + md->tag_y + MIX_TAG_Y_OFFSET;
    
  if ((show_wave) &&
      (md->y != MIX_TAG_ERASED))
    erase_mix_tag_and_waveform(ms, cp, ap, md->x, y);

  if (ms->beg >= ap->losamp)
    draw_mix_tag(md, x, y);

  if (show_wave)
    {
      bool two_sided = false;
      int pts;
      pts = prepare_mix_waveform(md, ms, ap, mix_waveform_height(ss), y, (double)SND_SRATE(cp->sound), &two_sided);
      if (pts > 0)
	{
	  axis_context *ax;
	  ax = mix_waveform_context(cp);
	  if (two_sided)
	    draw_both_grf_points(cp->dot_size, ax, pts, cp->time_graph_style);
	  else draw_grf_points(cp->dot_size, ax, pts, ap, ungrf_y(ap, y), cp->time_graph_style);
	  copy_context(cp);
	}
    }
}


static void display_one_mix_with_bounds(mix_state *ms, chan_info *cp, axis_info *ap, off_t beg, off_t end)
{
  if ((ms) && 
      (ms->beg + ms->len > beg) && 
      (ms->beg < end))
    {
      mix_info *md;
      int x;
      md = mix_infos[ms->mix_id];
      x = grf_x((double)(ms->beg) / (double)SND_SRATE(cp->sound), ap);
      if ((x + mix_tag_width(ss)) <= ap->x_axis_x1)  /* not cut off on right */
	draw_mix_tag_and_waveform(md, ms, x);
    }
}


static void display_one_mix(mix_state *ms, chan_info *cp)
{
  display_one_mix_with_bounds(ms, cp, cp->axis, cp->axis->losamp, cp->axis->hisamp);
}


void display_channel_mixes_with_bounds(chan_info *cp, off_t beg, off_t end)
{
  /* called in display_channel_data if cp has active mixes
   *   it used to draw the tag and waveform if show_mix_waveforms(ss), but I think the tag should be drawn in any case
   */
  mix_list *mxl;
  if ((cp->sound->channel_style == CHANNELS_SUPERIMPOSED) ||
      (cp->squelch_update))
    return;
  mxl = (mix_list *)(cp->edits[cp->edit_ctr]->mixes); /* active mixes in the current edit of this channel */
  if (mxl)
    {
      int i;
      for (i = 0; i < mxl->size; i++)
	display_one_mix_with_bounds(mxl->list[i], cp, cp->axis, beg, end);
    }
}


void display_channel_mixes(chan_info *cp)
{
  display_channel_mixes_with_bounds(cp, cp->axis->losamp, cp->axis->hisamp);
}


#define MIX_WAIT_TIME    50
static timeout_result_t watch_mix_proc = 0;

static void stop_watch_mix_proc(void)
{
  if (watch_mix_proc != 0)
    {
      TIMEOUT_REMOVE(watch_mix_proc);
      watch_mix_proc = 0;
    }
}


static float watch_mix_x_incr = 1.0;

static TIMEOUT_TYPE watch_mix(TIMEOUT_ARGS)
{
  mix_info *md = (mix_info *)context;
  if (watch_mix_proc != 0)
    {
      watch_mix_x_incr *= 1.1;
      move_mix_tag(md->id, (int)(md->x + watch_mix_x_incr), md->y);
      watch_mix_proc = CALL_TIMEOUT(watch_mix, MIX_WAIT_TIME, md);
    }
  TIMEOUT_RESULT
}


static int edpos_before_drag = 0;
static with_hook_t hookable_before_drag;
static bool mix_dragged = false;
static XEN mix_release_hook;
static XEN mix_drag_hook;
/* also mix_click_hook in snd-chn.c */

static off_t drag_beg = 0, drag_end = 0;

void move_mix_tag(int mix_id, int x, int y) 
{
  /* dragging mix, hit_mix returns id, called only from snd-chn.c and above (watch_mix) */
  mix_info *md;
  mix_state *ms;
  axis_info *ap;
  chan_info *cp;
  bool axis_changed = false;

  md = md_from_id(mix_id);
  cp = md->cp;

  if (!mix_dragged) /* starting to drag -- unmix now while we know the original position */
    {
      edpos_before_drag = cp->edit_ctr;
      hookable_before_drag = cp->hookable;
      cp->hookable = WITHOUT_HOOK;
      drag_beg = mix_position_from_id(mix_id);
      drag_end = drag_beg + mix_length_from_id(mix_id);
    }
  else cp->edit_ctr = edpos_before_drag;
  
  mix_set_position_edit(mix_id, snd_round_off_t(ungrf_x(cp->axis, x) * (double)(SND_SRATE(cp->sound))));

  mix_dragged = true;
  ap = cp->axis;
  ms = current_mix_state(md); 

  if ((x > ap->x_axis_x1) || (x < ap->x_axis_x0)) 
    {
      /* we're outside the graph */
      if (watch_mix_proc != 0)
	{
	  if ((x < ap->x_axis_x0) && (ap->x0 == ap->xmin)) return;
	  if ((x > ap->x_axis_x1) && (ap->x1 == ap->xmax)) return;
	}
      else
	{
	  if (mix_dragged)
	    {
	      if (x < ap->x_axis_x0)
		watch_mix_x_incr = -1.0;
	      else watch_mix_x_incr = 1.0;
	      watch_mix_proc = CALL_TIMEOUT(watch_mix, MIX_WAIT_TIME, md);
	    }
	}
      x = move_axis(cp, ap, x); /* calls update_graph eventually (in snd-chn.c reset_x_display) */
      axis_changed = true;
    }
  else 
    {
      if (watch_mix_proc != 0) 
	{
	  stop_watch_mix_proc();
	  watch_mix_proc = 0;
	}
    }

  reflect_mix_change(mix_id);

  if ((axis_changed) ||
      (cp->sound->channel_style == CHANNELS_SUPERIMPOSED))
    display_channel_time_data(cp);
  else
    {
      off_t cur_end;
      cur_end = ms->beg + ms->len;
      if (cur_end > drag_end)
	drag_end = cur_end;
      if (ms->beg < drag_beg)
	drag_beg = ms->beg;
      make_partial_graph(cp, drag_beg, drag_end);
      display_channel_mixes_with_bounds(cp, drag_beg, drag_end);
    }

  if (XEN_HOOKED(mix_drag_hook))
    run_hook(mix_drag_hook,
	     XEN_LIST_3(C_TO_XEN_INT(mix_id),
			C_TO_XEN_INT(x),
			C_TO_XEN_INT(y)),
	     S_mix_drag_hook);
}


void finish_moving_mix_tag(int mix_id, int x)
{
  /* from mouse release after tag drag in snd-chn.c only */
  mix_info *md;
  off_t pos;
  XEN res = XEN_FALSE;
  chan_info *cp;

  mix_dragged = false;
  if (watch_mix_proc != 0) 
    {
      stop_watch_mix_proc();
      watch_mix_proc = 0;
    }

  md = md_from_id(mix_id);
  if (!md) return;
  cp = md->cp;
  
  pos = snd_round_off_t(ungrf_x(cp->axis, x) * (double)(SND_SRATE(cp->sound)));
  if (pos < 0) pos = 0;
  cp->hookable = hookable_before_drag;
  
  if (cp->edit_ctr > edpos_before_drag) /* possibly dragged it back to start point, so no edit took place */
    cp->edit_ctr--;

  if (XEN_HOOKED(mix_release_hook))
    res = run_progn_hook(mix_release_hook,
			 XEN_LIST_2(C_TO_XEN_INT(mix_id),
				    C_TO_XEN_OFF_T(pos - mix_position_from_id(mix_id))),
			 S_mix_release_hook);

  if (!(XEN_TRUE_P(res)))
    {
      if (mix_set_position_edit(mix_id, pos))
	{
	  CURSOR(cp) = pos;
	  after_edit(cp);
	  update_graph(cp); /* this causes flashing, but it's next to impossible to fix
			     *   display_channel_id assumes previous id was erased, as does any after_graph_hook function
			     *   and we have to run lisp/fft graphs in any case (and the hook),
			     *   but display_channel_data_1 erases the old graph, so it's hard to specialize for this case
			     */
	}
    }
}


/* View:Mixes dialog display */

void mix_display_during_drag(int mix_id, off_t drag_beg, off_t drag_end)
{
  chan_info *cp;
  cp = mix_chan_info_from_id(mix_id);

  if (cp->sound->channel_style == CHANNELS_SUPERIMPOSED)
    display_channel_time_data(cp);
  else
    {
      off_t cur_end, ms_beg;
      ms_beg = mix_position_from_id(mix_id);
      cur_end = ms_beg + mix_length_from_id(mix_id);
      if (cur_end > drag_end)
	drag_end = cur_end;
      if (ms_beg < drag_beg)
	drag_beg = ms_beg;
      make_partial_graph(cp, drag_beg, drag_end);
      display_channel_mixes_with_bounds(cp, drag_beg, drag_end);
    }
}



/* xen connection */

static XEN snd_no_such_mix_error(const char *caller, XEN n)
{
  XEN_ERROR(XEN_ERROR_TYPE("no-such-mix"),
	XEN_LIST_2(C_TO_XEN_STRING(caller),
		   n));
  return(XEN_FALSE);
}


void after_mix_edit(int id)
{
  mix_info *md;
  md = md_from_id(id);
  if ((md) && (md->cp))
    {
      after_edit(md->cp);
      update_graph(md->cp);
    }
}


/* mix-state */

static XEN g_mix_length(XEN n) 
{
  #define H_mix_length "(" S_mix_length " id): mix's length in samples"
  int id;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_mix_length, "an integer");
  id = XEN_TO_C_INT(n);
  if (mix_is_active(id))
    return(C_TO_XEN_OFF_T(mix_length_from_id(id)));
  return(snd_no_such_mix_error(S_mix_length, n));
}


static XEN g_mix_position(XEN n) 
{
  int id;
  #define H_mix_position "(" S_mix_position " id): mix's begin time in the output in samples"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_mix_position, "an integer");
  id = XEN_TO_C_INT(n);
  if (mix_is_active(id))
    return(C_TO_XEN_OFF_T(mix_position_from_id(id)));
  return(snd_no_such_mix_error(S_mix_position, n));
}


static XEN g_set_mix_position(XEN n, XEN pos) 
{
  int id;
  off_t beg;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_setB S_mix_position, "an integer");
  XEN_ASSERT_TYPE(XEN_OFF_T_P(pos), pos, XEN_ARG_2, S_setB S_mix_position, "an integer");

  id = XEN_TO_C_INT(n);
  if (!(mix_is_active(id)))
    return(snd_no_such_mix_error(S_setB S_mix_position, n));

  beg = beg_to_sample(pos, S_setB S_mix_position);
  if (mix_set_position_edit(id, beg))
    after_mix_edit(id);

  return(pos);
}


static XEN g_mix_amp(XEN n) 
{
  #define H_mix_amp "(" S_mix_amp " id): mix's scaler"
  int id;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_mix_amp, "an integer");
  id = XEN_TO_C_INT(n);
  if (mix_is_active(id))
    return(C_TO_XEN_DOUBLE(mix_amp_from_id(id)));
  return(snd_no_such_mix_error(S_mix_amp, n));
}


static XEN g_set_mix_amp(XEN n, XEN uval) 
{
  int id;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_setB S_mix_amp, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(uval), uval, XEN_ARG_2, S_setB S_mix_amp, "a number");

  id = XEN_TO_C_INT(n);
  if (!(mix_is_active(id)))
    return(snd_no_such_mix_error(S_setB S_mix_amp, n));

  if (mix_set_amp_edit(id, XEN_TO_C_DOUBLE(uval)))
    after_mix_edit(id);

  return(uval);
}


static XEN g_mix_amp_env(XEN n) 
{
  #define H_mix_amp_env "(" S_mix_amp_env " id): amplitude envelope applied to mix"
  int id;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_mix_amp_env, "an integer");
  id = XEN_TO_C_INT(n);
  if (mix_is_active(id))
    return(env_to_xen(mix_amp_env_from_id(id)));
  return(snd_no_such_mix_error(S_mix_amp_env, n));
}


static XEN g_set_mix_amp_env(XEN n, XEN val) 
{
  env *e = NULL;
  int id;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_setB S_mix_amp_env, "an integer");
  XEN_ASSERT_TYPE(XEN_LIST_P(val) || XEN_FALSE_P(val), val, XEN_ARG_2, S_setB S_mix_amp_env, "a list or " PROC_FALSE);

  id = XEN_TO_C_INT(n);
  if (!(mix_is_active(id)))
    return(snd_no_such_mix_error(S_setB S_mix_amp_env, n));  

  if (XEN_LIST_P(val))
    e = get_env(val, S_setB S_mix_amp_env);

  if (mix_set_amp_env_edit(id, e))
    after_mix_edit(id);

  /* e is copied by mix_set_amp_env_edit, and created by get_env (xen_to_env), so it should be freed here */
  if (e) free_env(e);
  return(val);
}


static XEN g_mix_speed(XEN n) 
{
  #define H_mix_speed "(" S_mix_speed " id): mix's resampling ratio"
  int id;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_mix_speed, "an integer");
  id = XEN_TO_C_INT(n);
  if (mix_is_active(id))
    return(C_TO_XEN_DOUBLE(mix_speed_from_id(id)));
  return(snd_no_such_mix_error(S_mix_speed, n));
}


static XEN g_set_mix_speed(XEN n, XEN uval) 
{
  int id;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_setB S_mix_speed, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(uval), uval, XEN_ARG_2, S_setB S_mix_speed, "a number");

  id = XEN_TO_C_INT(n);
  if (!(mix_is_active(id)))
    return(snd_no_such_mix_error(S_setB S_mix_speed, n));  

  if (mix_set_speed_edit(id, XEN_TO_C_DOUBLE(uval)))
    after_mix_edit(id);

  return(uval);
}



/* mix-info */

static XEN g_mix_name(XEN n) 
{
  #define H_mix_name "(" S_mix_name " id): name of mix"
  int id;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_mix_name, "an integer");
  id = XEN_TO_C_INT(n);
  if (mix_exists(id))
    return(C_TO_XEN_STRING(mix_name_from_id(id)));
  return(snd_no_such_mix_error(S_mix_name, n));
}


static XEN g_set_mix_name(XEN n, XEN val) 
{
  int id;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_setB S_mix_name, "an integer");
  XEN_ASSERT_TYPE(XEN_STRING_P(val) || XEN_FALSE_P(val), val, XEN_ARG_2, S_setB S_mix_name, "a string");
  id = XEN_TO_C_INT(n);
  if (mix_exists(id))
    mix_set_name_from_id(id, (XEN_STRING_P(val) ? XEN_TO_C_STRING(val) : NULL));
  else return(snd_no_such_mix_error(S_setB S_mix_name, n));
  return(val);
}


static XEN g_mix_sync(XEN n) 
{
  #define H_mix_sync "(" S_mix_sync " id): mix sync field (an integer)"
  int id;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_mix_sync, "an integer");
  id = XEN_TO_C_INT(n);
  if (mix_exists(id))
    return(C_TO_XEN_INT(mix_sync_from_id(id)));
  return(snd_no_such_mix_error(S_mix_sync, n));
}


static XEN g_set_mix_sync(XEN n, XEN val) 
{
  int id;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_setB S_mix_sync, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_2, S_setB S_mix_sync, "an integer");
  id = XEN_TO_C_INT(n);
  if (mix_exists(id))
    mix_set_sync_from_id(id, XEN_TO_C_INT(val));
  else return(snd_no_such_mix_error(S_setB S_mix_sync, n));
  return(val);
}


static XEN g_mix_sync_max(void) 
{
  #define H_mix_sync_max "(" S_mix_sync_max "): max mix sync value seen so far"
  return(C_TO_XEN_INT(current_mix_sync_max));
}


static XEN g_mix_tag_y(XEN n) 
{
  #define H_mix_tag_y "(" S_mix_tag_y " id): height of mix's tag"
  int id;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_mix_tag_y, "an integer");
  id = XEN_TO_C_INT(n);
  if (mix_exists(id))
    return(C_TO_XEN_INT(mix_tag_y_from_id(id)));
  return(snd_no_such_mix_error(S_mix_tag_y, n));
}


static XEN g_set_mix_tag_y(XEN n, XEN val) 
{
  int id;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_setB S_mix_tag_y, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_2, S_setB S_mix_tag_y, "an integer");
  id = XEN_TO_C_INT(n);
  if (mix_exists(id))
    {
      chan_info *cp;
      mix_state *ms;
      mix_info *md;
      md = md_from_id(id);
      ms = current_mix_state(md);
      cp = md->cp;
      if ((cp) &&
	  (!(cp->squelch_update)))
	erase_mix_tag_and_waveform(ms, cp, cp->axis, md->x, cp->axis->y_offset + md->tag_y + MIX_TAG_Y_OFFSET);
      md->tag_y = XEN_TO_C_INT(val);
      if ((cp) &&
	  (!(cp->squelch_update)))
	display_one_mix(ms, cp);
    }
  else return(snd_no_such_mix_error(S_setB S_mix_tag_y, n));
  return(val);
}


static XEN g_mix_properties(XEN n)
{
  #define H_mix_properties "(S_mix_properties id):  A property list associated with the given mix. \
The accessor mix-property is provided in mix." XEN_FILE_EXTENSION "."

  mix_info *md;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_mix_properties, "an integer");

  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error(S_mix_properties, n));

  if (!(XEN_VECTOR_P(md->properties)))
    {
      md->properties = XEN_MAKE_VECTOR(1, XEN_EMPTY_LIST);
      md->properties_gc_loc = snd_protect(md->properties);
    }
  return(XEN_VECTOR_REF(md->properties, 0));
}


static XEN g_set_mix_properties(XEN n, XEN val)
{
  mix_info *md;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_mix_properties, "an integer");

  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error(S_setB S_mix_properties, n));

  if (!(XEN_VECTOR_P(md->properties)))
    {
      md->properties = XEN_MAKE_VECTOR(1, XEN_EMPTY_LIST);
      md->properties_gc_loc = snd_protect(md->properties);
    }
  XEN_VECTOR_SET(md->properties, 0, val);
  return(XEN_VECTOR_REF(md->properties, 0));
}


static XEN g_mix_home(XEN n) 
{
  #define H_mix_home "(" S_mix_home " id): list of sound index and channel for the output of the mix, and the \
filename or " PROC_FALSE " and the input channel for its data."
  mix_info *md; 
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_mix_home, "an integer");
  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error(S_mix_home, n));
  return(XEN_LIST_4(C_TO_XEN_INT((md->cp->sound)->index),
		    C_TO_XEN_INT((md->cp->chan)),
		    (md->in_filename) ? C_TO_XEN_STRING(md->in_filename) : XEN_FALSE,
		    C_TO_XEN_INT(md->in_chan)));
}



void color_mixes(color_t color)
{
  int i;
  set_mix_color(color);
  for (i = 0; i < mix_infos_ctr; i++)
    if (mix_infos[i])
      mix_infos[i]->color = color;
  for_each_normal_chan(update_graph);
}


static XEN g_mix_color(XEN mix_id) 
{
  #define H_mix_color "(" S_mix_color " :optional mix-id): color of all mix tags (if mix-id is omitted), or of mix-id's tag"
  if (XEN_INTEGER_P(mix_id))
    return(XEN_WRAP_PIXEL(mix_color_from_id(XEN_TO_C_INT(mix_id))));
  return(XEN_WRAP_PIXEL(ss->sgx->mix_color));
}


static XEN g_set_mix_color(XEN color, XEN mix_id)
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ARG_1, S_setB S_mix_color, "a color"); 
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(mix_id), mix_id, XEN_ARG_2, S_setB S_mix_color, "an integer");
  if (XEN_INTEGER_P(mix_id))
    mix_set_color_from_id(XEN_TO_C_INT(mix_id), XEN_UNWRAP_PIXEL(color));
  else color_mixes(XEN_UNWRAP_PIXEL(color));
  return(color);
}

WITH_TWO_SETTER_ARGS(g_set_mix_color_reversed, g_set_mix_color)




/* mix-related globals */

static void update_mix_waveforms(chan_info *cp)
{
  if ((cp) && (channel_has_active_mixes(cp)))
    update_graph(cp);
}


static XEN g_mix_waveform_height(void) 
{
  #define H_mix_waveform_height "(" S_mix_waveform_height "): max height (pixels) of mix waveforms (20)"
  return(C_TO_XEN_INT(mix_waveform_height(ss)));
}


static XEN g_set_mix_waveform_height(XEN val) 
{
  int new_val;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_mix_waveform_height, "a number"); 
  new_val = mus_iclamp(0, XEN_TO_C_INT_OR_ELSE(val, 0), LOTSA_PIXELS);
  in_set_mix_waveform_height(new_val);
  for_each_normal_chan(update_mix_waveforms);
  return(C_TO_XEN_INT(mix_waveform_height(ss)));
}


static XEN g_with_mix_tags(void) 
{
  #define H_with_mix_tags "(" S_with_mix_tags "): " PROC_TRUE " if Snd should try to use virtual (tagged) mixing"
  return(C_TO_XEN_BOOLEAN(with_mix_tags(ss)));
}


static XEN g_set_with_mix_tags(XEN val) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_with_mix_tags, "a boolean");
  set_with_mix_tags(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(with_mix_tags(ss)));
}


static XEN g_mix_tag_width(void) 
{
  #define H_mix_tag_width "(" S_mix_tag_width "): width (pixels) of mix tags (6)"
  return(C_TO_XEN_INT(mix_tag_width(ss)));
}


static XEN g_set_mix_tag_width(XEN val) 
{
  int width;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_mix_tag_width, "an integer"); 
  width = mus_iclamp(0, XEN_TO_C_INT(val), LOTSA_PIXELS);
  set_mix_tag_width(width);
  for_each_normal_chan(update_graph);
  return(C_TO_XEN_INT(mix_tag_width(ss)));
}


static XEN g_mix_tag_height(void) 
{
  #define H_mix_tag_height "(" S_mix_tag_height "): height (pixels) of mix tags (14)"
  return(C_TO_XEN_INT(mix_tag_height(ss)));
}


static XEN g_set_mix_tag_height(XEN val) 
{
  int height;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_mix_tag_height, "an integer"); 
  height = mus_iclamp(0, XEN_TO_C_INT(val), LOTSA_PIXELS);
  set_mix_tag_height(height);
  for_each_normal_chan(update_graph);
  return(C_TO_XEN_INT(mix_tag_height(ss)));
}


static XEN g_mix_p(XEN n) 
{
  #define H_mix_p "(" S_mix_p " id): returns " PROC_TRUE " if the mix 'n' exists somewhere in the edit list, \
the id (n) if the mix is currently active, of " PROC_FALSE " if 'n' does not refer to a mix."
  if (XEN_INTEGER_P(n))
    {
      int id;
      id = XEN_TO_C_INT(n);
      if (mix_is_active(id))
	return(n);
      if (mix_exists(id))
	return(XEN_TRUE);
    }
  return(XEN_FALSE);
}


static XEN g_mixes(XEN snd, XEN chn)
{
  #define H_mixes "(" S_mixes " :optional snd chn): list of mixes (ids) associated with snd and chn"
  snd_info *sp;
  chan_info *cp;
  int i, j;
  XEN res1 = XEN_EMPTY_LIST;
  if (XEN_INTEGER_P(snd))
    {
      if (XEN_INTEGER_P(chn))
	{
	  /* scan all mixes for any associated with this channel */
	  cp = get_cp(snd, chn, S_mixes);
	  if (!cp) return(XEN_FALSE);
	  for (i = mix_infos_ctr - 1; i >= 0; i--)
	    if ((mix_exists(i)) && (mix_infos[i]->cp == cp))
	      res1 = XEN_CONS(C_TO_XEN_INT(i), res1);
	}
      else
	{
	  sp = get_sp(snd, NO_PLAYERS);
	  if (sp == NULL) 
	    return(snd_no_such_sound_error(S_mixes, snd));
	  for (i = sp->nchans - 1; i >= 0; i--)
	    res1 = XEN_CONS(g_mixes(snd, C_TO_XEN_INT(i)), res1);
	}
    }
  else
    {
      for (j = ss->max_sounds - 1; j >= 0; j--)
	{
	  sp = ss->sounds[j];
	  if ((sp) && (sp->inuse == SOUND_NORMAL))
	    res1 = XEN_CONS(g_mixes(C_TO_XEN_INT(j), 
				    XEN_UNDEFINED), 
			    res1);
	}
    }
  return(res1);
}



static XEN g_mix_vct(XEN obj, XEN beg, XEN snd, XEN chn, XEN with_tag, XEN origin)
{
  #define H_mix_vct "(" S_mix_vct " data :optional (beg 0) snd chn (with-tag " S_with_mix_tags ") origin): \
mix data (a vct) into snd's channel chn starting at beg; return the new mix id, if any"

  vct *v;
  off_t bg;
  chan_info *cp;
  char *edname = NULL;
  mus_sample_t *data;
  int len, mix_id = NO_MIX_TAG;
  bool with_mixer;

  XEN_ASSERT_TYPE(MUS_VCT_P(obj), obj, XEN_ARG_1, S_mix_vct, "a vct");
  ASSERT_CHANNEL(S_mix_vct, snd, chn, 3);
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(beg), beg, XEN_ARG_2, S_mix_vct, "an integer");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(with_tag), with_tag, XEN_ARG_5, S_mix_vct, "a boolean");
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(origin), origin, XEN_ARG_6, S_mix_vct, "a string");

  cp = get_cp(snd, chn, S_mix_vct);
  if (!cp) return(XEN_FALSE);
  if (!(editable_p(cp))) return(XEN_FALSE);

  if (XEN_STRING_P(origin))
    edname = XEN_TO_C_STRING(origin);
  else edname = S_mix_vct;

  bg = beg_to_sample(beg, S_mix_vct);
  v = XEN_TO_VCT(obj);
  len = v->length;

  with_mixer = virtual_mix_ok(cp, cp->edit_ctr);
  if (with_mixer)
    {
      if (XEN_NOT_BOUND_P(with_tag))
	with_mixer = with_mix_tags(ss);
      else with_mixer = XEN_TO_C_BOOLEAN(with_tag);
    }

  if (!with_mixer)
    return(C_TO_XEN_BOOLEAN(mix_vct_untagged(v, cp, bg, edname)));

#if SNDLIB_USE_FLOATS
  data = v->data;
#else
  {
    int i;
    data = (mus_sample_t *)CALLOC(len, sizeof(mus_sample_t));
    for (i = 0; i < len; i++)
      data[i] = MUS_FLOAT_TO_SAMPLE(v->data[i]);
  }
#endif
  
  /* make_snd_data_buffer copies the data array, so we don't need GC protection */
  /*    we can't use v->data directly because the user might change it */
  {
    char *new_origin;
    /* (mix-vct (vct .1 .2 .3) 100 0 0 #t "mix-vct (vct .1 .2 .3)") */

#if HAVE_FORTH
    /* vct( 0.1 0.2 0.3 ) 100 snd chn #t "vct( 0.1 0.2, 0.3 )" mix-vct */ 
    { 
      if (edname && *edname) 
	{ 
	  char *name; 
	  if ((name = strrchr(edname, ' '))) 
	    name++; 
	  else 
	    name = S_mix_vct; 
	  new_origin = mus_format("%.*s " OFF_TD " snd chn %s to -mix-%d", 
				  strlen(edname) - strlen(name) - 1, edname, 
				  bg, name, mix_infos_ctr); 
	} 
      else new_origin = mus_format("vct( 0 ) " OFF_TD " snd chn %s to -mix-%d", bg, S_mix_vct, mix_infos_ctr); 
    } 
#endif
#if HAVE_SCHEME
    new_origin = mus_format("(set! -mix-%d (%s " OFF_TD " snd chn))", mix_infos_ctr, edname, bg);
#endif
#if HAVE_RUBY
    /* mix_vct(vct(0.1, 0.2, 0.3), 100, snd, chn, true, "mix_vct(vct(0.1, 0.2, 0.3)") */ 
    new_origin = mus_format("_mix_%d = %s, " OFF_TD ", snd, chn)", mix_infos_ctr, edname, bg); 
#endif

    mix_id = mix_buffer_with_tag(cp, data, bg, len, new_origin); 
    FREE(new_origin);
  }

#if (!SNDLIB_USE_FLOATS)
  FREE(data);
#endif
  update_graph(cp);

  return(xen_return_first(C_TO_XEN_INT(mix_id), obj, origin));
}


static XEN g_mix(XEN file, XEN chn_samp_n, XEN file_chn, XEN snd_n, XEN chn_n, XEN tag, XEN auto_delete)
{
  #define H_mix "(" S_mix " file :optional (beg 0) (file-chan 0) snd chn (with-tag " S_with_mix_tags ") auto-delete): \
mix channel file-chan of file into snd's channel chn starting at beg (in the output), returning the new mix's id.  \
if with-tag is " PROC_FALSE ", no draggable tag is created.  If \
auto-delete is " PROC_TRUE ", the input file is deleted when it is no longer needed."

  chan_info *cp = NULL;
  static char *name = NULL;
  int chans, id = NO_MIX_TAG, file_channel = 0;
  bool with_mixer;
  off_t beg = 0, len = 0;

  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, S_mix, "a string");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(chn_samp_n), chn_samp_n, XEN_ARG_2, S_mix, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(file_chn), file_chn, XEN_ARG_3, S_mix, "an integer");
  ASSERT_CHANNEL(S_mix, snd_n, chn_n, 4);
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(tag), tag, XEN_ARG_6, S_mix, "a boolean");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(auto_delete), auto_delete, XEN_ARG_7, S_mix, "a boolean or an integer");
  if (name) FREE(name);

  name = mus_expand_filename(XEN_TO_C_STRING(file));

  if (!(mus_file_probe(name)))
    return(snd_no_such_file_error(S_mix, file));

  cp = get_cp(snd_n, chn_n, S_mix);
  if (!cp) return(XEN_FALSE);

  if (XEN_OFF_T_P(chn_samp_n))
    beg = XEN_TO_C_OFF_T(chn_samp_n);

  chans = mus_sound_chans(name);
  if (chans <= 0)
    {
      XEN_ERROR(BAD_HEADER,
		XEN_LIST_4(C_TO_XEN_STRING(S_mix),
			   file,
			   C_TO_XEN_STRING("chans <= 0"),
			   C_TO_XEN_INT(chans)));
    }
  if (XEN_INTEGER_P(file_chn))
    {
      file_channel = XEN_TO_C_INT(file_chn);
      if (file_channel >= chans)
	{
	  XEN_ERROR(NO_SUCH_CHANNEL,
		    XEN_LIST_3(C_TO_XEN_STRING(S_mix),
			       C_TO_XEN_STRING("chan: ~A, ~A chans: ~A"),
			       XEN_LIST_3(file_chn,
					  file,
					  C_TO_XEN_INT(chans))));
	}
    }
  else
    {
      if (XEN_TRUE_P(file_chn)) /* this used to be the default as well -- XEN_NOT_BOUND_P case */
	{
	  file_delete_t delete_choice = DONT_DELETE_ME;
	  if (XEN_INTEGER_P(auto_delete))
	    delete_choice = (file_delete_t)XEN_TO_C_INT(auto_delete);
	  else
	    {
	      if (XEN_BOOLEAN_P(auto_delete))
		{
		  if (XEN_TO_C_BOOLEAN(auto_delete))
		    {
		      if ((chans > 1) &&
			  (cp->sound->nchans > 1)) /* mix_complete_file sets sync locally so all we care about here is nchans */
			{
			  remember_temp(name, chans);
			  delete_choice = MULTICHANNEL_DELETION;
			}
		      else delete_choice = DELETE_ME;
		    }
		  else delete_choice = DONT_DELETE_ME;
		}
	    }
	  return(C_TO_XEN_INT(mix_complete_file(cp->sound, beg, name, 
						(XEN_NOT_BOUND_P(tag)) ? with_mix_tags(ss) : XEN_TO_C_BOOLEAN(tag),
						delete_choice,
						MIX_SETS_SYNC_LOCALLY)));
	}
    }

  len = mus_sound_frames(name);
  if (len <= 0) return(XEN_FALSE);

  with_mixer = virtual_mix_ok(cp, cp->edit_ctr);
  if (with_mixer)
    {
      if (XEN_NOT_BOUND_P(tag))
	with_mixer = with_mix_tags(ss);
      else with_mixer = XEN_TO_C_BOOLEAN(tag);
    }

  {
    file_delete_t delete_file = DONT_DELETE_ME;

    delete_file = xen_to_file_delete_t(auto_delete, S_mix);
    if ((delete_file == MULTICHANNEL_DELETION) || (delete_file == MULTICHANNEL_DELETION_IF_FILE))
      remember_temp(name, chans);

    if (!with_mixer)
      {
	char *origin;
	origin = untagged_mix_to_string(name, beg, file_channel, delete_file == DELETE_ME);
	mix_file_untagged(name, file_channel, cp, beg, len, delete_file, origin);
	FREE(origin);
      }
    else 
      {
	char *origin;
	origin = tagged_mix_to_string(name, beg, file_channel, delete_file == DELETE_ME);

	if (len < FILE_BUFFER_SIZE)
	  {
	    mus_sample_t *data;
	    data = (mus_sample_t *)MALLOC(len * sizeof(mus_sample_t));
	    len = mus_file_to_array(name, file_channel, 0, len, data); 
	    id = mix_buffer_with_tag(cp, data, beg, len, origin);
	    FREE(data);
	    if (delete_file == DELETE_ME)
	      snd_remove(name, REMOVE_FROM_CACHE);
	    else
	      {
		if (delete_file == MULTICHANNEL_DELETION_IF_FILE)
		  forget_temp(name, file_channel);
	      }
	  }
	else 
	  {
	    mix_info *md;
	    id = mix_file_with_tag(cp, name, file_channel, beg, delete_file, origin);
	    if (mix_exists(id))   /* if edit is blocked, mix_file can return NO_MIX_TAG (-1) */
	      {
		md = md_from_id(id);
		if (!md->in_filename)
		  md->in_filename = copy_string(name);
	      }
	  }
	FREE(origin);
      }
  }
  update_graph(cp);
  return(C_TO_XEN_INT(id));
}



/* ---------------- mix sample readers ---------------- */

typedef struct mix_fd {
  mix_info *md;
  snd_fd *sf;
} mix_fd;

static XEN_OBJECT_TYPE mf_tag;

bool mix_sample_reader_p(XEN obj) 
{
  return(XEN_OBJECT_TYPE_P(obj, mf_tag));
}


#define TO_MIX_SAMPLE_READER(obj) ((mix_fd *)XEN_OBJECT_REF(obj))
#define MIX_SAMPLE_READER_P(Obj) XEN_OBJECT_TYPE_P(Obj, mf_tag)

void *xen_to_mix_sample_reader(XEN obj) 
{
  if (MIX_SAMPLE_READER_P(obj)) 
    return(TO_MIX_SAMPLE_READER(obj)); 
  return(NULL);
}


static XEN g_mix_sample_reader_p(XEN obj) 
{
  #define H_mix_sample_reader_p "(" S_mix_sample_reader_p " obj): " PROC_TRUE " if obj is a mix-sample-reader"
  return(C_TO_XEN_BOOLEAN(mix_sample_reader_p(obj)));
}


static char *mix_sample_reader_to_string(mix_fd *fd) 
{
  char *desc;
  desc = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  if ((fd == NULL) || (fd->sf == NULL))
    sprintf(desc, "#<mix-sample-reader: null>");
  else
    {
      if ((mix_is_active(fd->sf->region)) &&
	  (fd->md) &&
	  (fd->sf->region == (fd->md->id)))
	{
	  mix_info *md;
	  md = fd->md;
	  mus_snprintf(desc, PRINT_BUFFER_SIZE, "#<mix-sample-reader mix %d, (from " OFF_TD ", at " OFF_TD "%s): %s>",
		       md->id,
		       fd->sf->initial_samp,
		       fd->sf->loc,
		       (fd->sf->at_eof) ? ", at eof" : "",
		       (md->in_filename) ? md->in_filename : "<vct>");
	}
      else sprintf(desc, "#<mix-sample-reader: inactive>");
    }
  return(desc);
}

XEN_MAKE_OBJECT_PRINT_PROCEDURE(mix_fd, print_mf, mix_sample_reader_to_string)


static void mf_free(mix_fd *fd)
{
  if (fd) 
    {
      if (fd->sf)
	free_snd_fd(fd->sf);
      fd->sf = NULL;
      fd->md = NULL;
      FREE(fd);
    }
}

XEN_MAKE_OBJECT_FREE_PROCEDURE(mix_fd, free_mf, mf_free)


static XEN g_make_mix_sample_reader(XEN mix_id, XEN ubeg)
{
  #define H_make_mix_sample_reader "(" S_make_mix_sample_reader " id :optional (beg 0)): return a reader ready to access mix id"
  mix_info *md = NULL;
  mix_state *ms;
  off_t beg;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(mix_id), mix_id, XEN_ARG_1, S_make_mix_sample_reader, "an integer");
  ASSERT_SAMPLE_TYPE(S_make_mix_sample_reader, ubeg, XEN_ARG_2);
  md = md_from_id(XEN_TO_C_INT(mix_id));
  if (md == NULL)
    return(snd_no_such_mix_error(S_make_mix_sample_reader, mix_id));
  beg = beg_to_sample(ubeg, S_make_mix_sample_reader);

  ms = current_mix_state(md);
  if (ms)
    {
      mix_fd *mf = NULL;
      mf = (mix_fd *)CALLOC(1, sizeof(mix_fd));
      mf->md = md;
      mf->sf = make_virtual_mix_reader(md->cp, beg, ms->len, ms->index, ms->scaler, READ_FORWARD);
      if (mf->sf)
	{
	  mf->sf->region = md->id;
	  XEN_MAKE_AND_RETURN_OBJECT(mf_tag, mf, 0, free_mf);
	}
    }
  return(XEN_FALSE);
}


static XEN g_read_mix_sample(XEN obj)
{
  mix_fd *mf;
  #define H_read_mix_sample "(" S_read_mix_sample " reader): read sample from mix reader"
  XEN_ASSERT_TYPE(MIX_SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_read_mix_sample, "a mix-sample-reader");
  mf = TO_MIX_SAMPLE_READER(obj);
  return(C_TO_XEN_DOUBLE(read_sample(mf->sf)));
}


/* tie into generic sample-reader procedures (snd-edits.c) */

XEN g_copy_mix_sample_reader(XEN obj)
{
  mix_fd *mf;
  mf = TO_MIX_SAMPLE_READER(obj);
  return(g_make_mix_sample_reader(C_TO_XEN_INT(mf->md->id),
				  C_TO_XEN_OFF_T(current_location(mf->sf))));
}


XEN g_mix_sample_reader_home(XEN obj)
{
  mix_fd *mf;
  mf = TO_MIX_SAMPLE_READER(obj);
  return(C_TO_XEN_INT(mf->md->id));
}


bool mix_sample_reader_at_end_p(void *ptr)
{
  mix_fd *mf = (mix_fd *)ptr;
  return(mf->sf->at_eof);
}


XEN g_mix_sample_reader_at_end_p(XEN obj)
{
  mix_fd *mf;
  mf = TO_MIX_SAMPLE_READER(obj);
  return(C_TO_XEN_BOOLEAN(mix_sample_reader_at_end_p(mf)));
}


XEN g_mix_sample_reader_position(XEN obj)
{
  mix_fd *mf;
  mf = TO_MIX_SAMPLE_READER(obj);
  if (mix_sample_reader_at_end_p(mf)) return(XEN_ZERO);
  return(C_TO_XEN_OFF_T(current_location(mf->sf)));
}


XEN g_free_mix_sample_reader(XEN obj)
{
  mix_fd *mf;
  mf = TO_MIX_SAMPLE_READER(obj);

  if (mf)
    {
      if (mf->sf)
	mf->sf = free_snd_fd(mf->sf);
      mf->md = NULL;
    }
  return(xen_return_first(XEN_FALSE, obj));
}



/* these are for snd-run */

int mix_sync_max(void)
{
  return(current_mix_sync_max);
}


Float run_read_mix_sample(void *ptr) 
{
  mix_fd *mf = (mix_fd *)ptr;
  return(read_sample(mf->sf));
}


void *run_make_mix_sample_reader(int id, off_t beg) 
{
  mix_info *md;
  md = md_from_id(id);
  if (md)
    {
      mix_state *ms;
      ms = current_mix_state(md);
      if (ms)
	{
	  mix_fd *mf;
	  mf = (mix_fd *)CALLOC(1, sizeof(mix_fd));
	  mf->md = md;
	  mf->sf = make_virtual_mix_reader(md->cp, beg, ms->len, ms->index, ms->scaler, READ_FORWARD);
	  return((void *)mf);
	}
    }
  return(NULL);
}


char *run_mix_sample_reader_to_string(void *ptr) 
{
  return(mix_sample_reader_to_string((mix_fd *)ptr));
}


void run_free_mix_sample_reader(void *ptr) 
{
  mf_free((mix_fd *)ptr);
}



static XEN g_view_mixes_dialog(void)
{
  widget_t w;
  #define H_view_mixes_dialog "(" S_view_mixes_dialog "): start the Mix browser"
  w = make_mix_dialog();
  return(XEN_WRAP_WIDGET(w));
}


static XEN g_mix_dialog_mix(void)
{
  #define H_mix_dialog_mix "(" S_mix_dialog_mix "): current mix id displayed in mix dialog."
  return(C_TO_XEN_INT(mix_dialog_mix()));
}


static XEN g_set_mix_dialog_mix(XEN val)
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_mix_dialog_mix, "mix id (an int)");
  mix_dialog_set_mix(XEN_TO_C_INT(val));
  return(val);
}


static bool play_mix(mix_info *md, off_t beg)
{
  mix_state *ms;
  ms = current_mix_state(md);
  if (!ms)
    {
      int i;
      for (i = md->cp->edit_ctr - 1; (ms == NULL) && (i < 0); i--)
	ms = ed_mix_state(md->cp->edits[i], md->id);
    }
  if (ms)
    return(add_mix_to_play_list(ms, md->cp, beg));
  return(false);
}


static XEN g_play_mix(XEN num, XEN beg)
{
  #define H_play_mix "(" S_play_mix " id :optional (beg 0)): play mix.  'beg' is where to start playing."
  mix_info *md;
  off_t samp;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(num), num, XEN_ONLY_ARG, S_play_mix, "an integer");
  md = md_from_id(XEN_TO_C_INT(num));
  if (md == NULL)
    return(snd_no_such_mix_error(S_play_mix, num));
  ASSERT_SAMPLE_TYPE(S_play_mix, beg, XEN_ARG_2);
  samp = beg_to_sample(beg, S_play_mix);
  play_mix(md, samp); 
  return(num);
}


bool play_mix_from_id(int mix_id) 
{
  mix_info *md;
  md = md_from_id(mix_id);
  if (md)
    return(play_mix(md, 0));
  return(false);
}


#ifdef XEN_ARGIFY_1

XEN_NARGIFY_1(g_mix_position_w, g_mix_position)
XEN_NARGIFY_2(g_set_mix_position_w, g_set_mix_position)
XEN_NARGIFY_1(g_mix_speed_w, g_mix_speed)
XEN_NARGIFY_2(g_set_mix_speed_w, g_set_mix_speed)
XEN_NARGIFY_1(g_mix_amp_w, g_mix_amp)
XEN_NARGIFY_2(g_set_mix_amp_w, g_set_mix_amp)
XEN_NARGIFY_1(g_mix_amp_env_w, g_mix_amp_env)
XEN_NARGIFY_2(g_set_mix_amp_env_w, g_set_mix_amp_env)

XEN_NARGIFY_1(g_mix_name_w, g_mix_name)
XEN_NARGIFY_2(g_set_mix_name_w, g_set_mix_name)
XEN_NARGIFY_1(g_mix_tag_y_w, g_mix_tag_y)
XEN_NARGIFY_2(g_set_mix_tag_y_w, g_set_mix_tag_y)
XEN_NARGIFY_1(g_mix_sync_w, g_mix_sync)
XEN_NARGIFY_2(g_set_mix_sync_w, g_set_mix_sync)
XEN_NARGIFY_0(g_mix_sync_max_w, g_mix_sync_max)
XEN_NARGIFY_1(g_mix_length_w, g_mix_length)
XEN_NARGIFY_1(g_mix_home_w, g_mix_home)
XEN_NARGIFY_1(g_mix_properties_w, g_mix_properties)
XEN_NARGIFY_2(g_set_mix_properties_w, g_set_mix_properties)
XEN_ARGIFY_1(g_mix_color_w, g_mix_color)
XEN_ARGIFY_2(g_set_mix_color_w, g_set_mix_color)

XEN_NARGIFY_0(g_mix_waveform_height_w, g_mix_waveform_height)
XEN_NARGIFY_1(g_set_mix_waveform_height_w, g_set_mix_waveform_height)
XEN_NARGIFY_0(g_with_mix_tags_w, g_with_mix_tags)
XEN_NARGIFY_1(g_set_with_mix_tags_w, g_set_with_mix_tags)
XEN_NARGIFY_0(g_mix_tag_width_w, g_mix_tag_width)
XEN_NARGIFY_1(g_set_mix_tag_width_w, g_set_mix_tag_width)
XEN_NARGIFY_0(g_mix_tag_height_w, g_mix_tag_height)
XEN_NARGIFY_1(g_set_mix_tag_height_w, g_set_mix_tag_height)

XEN_NARGIFY_1(g_mix_p_w, g_mix_p)
XEN_ARGIFY_2(g_mixes_w, g_mixes)
XEN_ARGIFY_7(g_mix_w, g_mix)
XEN_ARGIFY_6(g_mix_vct_w, g_mix_vct)

XEN_ARGIFY_2(g_make_mix_sample_reader_w, g_make_mix_sample_reader)
XEN_NARGIFY_1(g_read_mix_sample_w, g_read_mix_sample)
XEN_NARGIFY_1(g_mix_sample_reader_p_w, g_mix_sample_reader_p)
XEN_ARGIFY_2(g_play_mix_w, g_play_mix)

XEN_NARGIFY_0(g_view_mixes_dialog_w, g_view_mixes_dialog)
XEN_NARGIFY_0(g_mix_dialog_mix_w, g_mix_dialog_mix)
XEN_NARGIFY_1(g_set_mix_dialog_mix_w, g_set_mix_dialog_mix)

#else

#define g_mix_position_w g_mix_position
#define g_set_mix_position_w g_set_mix_position
#define g_mix_speed_w g_mix_speed
#define g_set_mix_speed_w g_set_mix_speed
#define g_mix_amp_w g_mix_amp
#define g_set_mix_amp_w g_set_mix_amp
#define g_mix_amp_env_w g_mix_amp_env
#define g_set_mix_amp_env_w g_set_mix_amp_env

#define g_mix_name_w g_mix_name
#define g_set_mix_name_w g_set_mix_name
#define g_mix_tag_y_w g_mix_tag_y
#define g_set_mix_tag_y_w g_set_mix_tag_y
#define g_mix_sync_w g_mix_sync
#define g_set_mix_sync_w g_set_mix_sync
#define g_mix_sync_max_w g_mix_sync_max
#define g_mix_length_w g_mix_length
#define g_mix_home_w g_mix_home
#define g_mix_properties_w g_mix_properties
#define g_set_mix_properties_w g_set_mix_properties
#define g_mix_color_w g_mix_color
#define g_set_mix_color_w g_set_mix_color

#define g_mix_waveform_height_w g_mix_waveform_height
#define g_set_mix_waveform_height_w g_set_mix_waveform_height
#define g_with_mix_tags_w g_with_mix_tags
#define g_set_with_mix_tags_w g_set_with_mix_tags
#define g_mix_tag_width_w g_mix_tag_width
#define g_set_mix_tag_width_w g_set_mix_tag_width
#define g_mix_tag_height_w g_mix_tag_height
#define g_set_mix_tag_height_w g_set_mix_tag_height

#define g_mix_p_w g_mix_p
#define g_mixes_w g_mixes
#define g_mix_w g_mix
#define g_mix_vct_w g_mix_vct

#define g_make_mix_sample_reader_w g_make_mix_sample_reader
#define g_read_mix_sample_w g_read_mix_sample
#define g_mix_sample_reader_p_w g_mix_sample_reader_p
#define g_play_mix_w g_play_mix

#define g_view_mixes_dialog_w g_view_mixes_dialog
#define g_mix_dialog_mix_w g_mix_dialog_mix
#define g_set_mix_dialog_mix_w g_set_mix_dialog_mix

#endif


void g_init_mix(void)
{
#if (!HAVE_GAUCHE)
  mf_tag = XEN_MAKE_OBJECT_TYPE("MixSampleReader", sizeof(mix_fd));
#else
  mf_tag = XEN_MAKE_OBJECT_TYPE("<mix-sample-reader>", sizeof(mix_fd), print_mf, free_mf);
  XEN_EVAL_C_STRING("(define-method object-apply ((rd <mix-sample-reader>)) (read-mix-sample rd))");
#endif

#if HAVE_GUILE
  scm_set_smob_print(mf_tag, print_mf);
  scm_set_smob_free(mf_tag, free_mf);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(mf_tag, XEN_PROCEDURE_CAST g_read_mix_sample, 0, 0, 0);
#endif
#endif

#if HAVE_RUBY
  rb_define_method(mf_tag, "to_s", XEN_PROCEDURE_CAST print_mf, 0);
  rb_define_method(mf_tag, "call", XEN_PROCEDURE_CAST g_read_mix_sample, 0);
#endif

#if HAVE_FORTH
  fth_set_object_inspect(mf_tag, print_mf);
  fth_set_object_free(mf_tag, free_mf);
  fth_set_object_apply(mf_tag, XEN_PROCEDURE_CAST g_read_mix_sample, 0, 0, 0);
#endif

  XEN_DEFINE_PROCEDURE(S_make_mix_sample_reader, g_make_mix_sample_reader_w, 1, 1, 0, H_make_mix_sample_reader);
  XEN_DEFINE_PROCEDURE(S_read_mix_sample,        g_read_mix_sample_w,        1, 0, 0, H_read_mix_sample);
  XEN_DEFINE_PROCEDURE(S_mix_sample_reader_p,    g_mix_sample_reader_p_w,    1, 0, 0, H_mix_sample_reader_p);
  XEN_DEFINE_PROCEDURE(S_play_mix,               g_play_mix_w,               0, 2, 0, H_play_mix);

  XEN_DEFINE_PROCEDURE(S_mix,                    g_mix_w,                    1, 6, 0, H_mix);
  XEN_DEFINE_PROCEDURE(S_mix_vct,                g_mix_vct_w,                1, 5, 0, H_mix_vct);
  XEN_DEFINE_PROCEDURE(S_mixes,                  g_mixes_w,                  0, 2, 0, H_mixes);
  XEN_DEFINE_PROCEDURE(S_mix_home,               g_mix_home_w,               1, 0, 0, H_mix_home);
  XEN_DEFINE_PROCEDURE(S_mix_p,                  g_mix_p_w,                  1, 0, 0, H_mix_p);
  XEN_DEFINE_PROCEDURE(S_mix_length,             g_mix_length_w,             1, 0, 0, H_mix_length);
  XEN_DEFINE_PROCEDURE(S_view_mixes_dialog,      g_view_mixes_dialog_w,      0, 0, 0, H_view_mixes_dialog);
  XEN_DEFINE_PROCEDURE(S_mix_sync_max,           g_mix_sync_max_w,           0, 0, 0, H_mix_sync_max);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_position,   g_mix_position_w,   H_mix_position,   S_setB S_mix_position,   g_set_mix_position_w,   1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_speed,      g_mix_speed_w,      H_mix_speed,      S_setB S_mix_speed,      g_set_mix_speed_w,      1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_amp,        g_mix_amp_w,        H_mix_amp,        S_setB S_mix_amp,        g_set_mix_amp_w,        1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_amp_env,    g_mix_amp_env_w,    H_mix_amp_env,    S_setB S_mix_amp_env,    g_set_mix_amp_env_w,    1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_name,       g_mix_name_w,       H_mix_name,       S_setB S_mix_name,       g_set_mix_name_w,       1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_sync,       g_mix_sync_w,       H_mix_sync,       S_setB S_mix_sync,       g_set_mix_sync_w,       1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_properties, g_mix_properties_w, H_mix_properties, S_setB S_mix_properties, g_set_mix_properties_w, 1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_tag_y,      g_mix_tag_y_w,      H_mix_tag_y,      S_setB S_mix_tag_y,      g_set_mix_tag_y_w,      1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_tag_width,  g_mix_tag_width_w,  H_mix_tag_width,  S_setB S_mix_tag_width,  g_set_mix_tag_width_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_tag_height, g_mix_tag_height_w, H_mix_tag_height, S_setB S_mix_tag_height, g_set_mix_tag_height_w, 0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_mix_color, g_mix_color_w, H_mix_color,
					    S_setB S_mix_color, g_set_mix_color_w, g_set_mix_color_reversed, 0, 1, 1, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_waveform_height, g_mix_waveform_height_w, H_mix_waveform_height,
				   S_setB S_mix_waveform_height, g_set_mix_waveform_height_w, 0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_with_mix_tags, g_with_mix_tags_w, H_with_mix_tags,
				   S_setB S_with_mix_tags, g_set_with_mix_tags_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_dialog_mix, g_mix_dialog_mix_w, H_mix_dialog_mix, 
				   S_setB S_mix_dialog_mix, g_set_mix_dialog_mix_w, 0, 0, 1, 0);


  #define H_mix_release_hook S_mix_release_hook " (mix-id samps): called after the mouse has dragged a mix to some new position. \
'samps' = samples moved in the course of the drag. If it returns " PROC_TRUE ", the actual remix is the hook's responsibility."

  mix_release_hook = XEN_DEFINE_HOOK(S_mix_release_hook, 2, H_mix_release_hook);

  #define H_mix_drag_hook S_mix_drag_hook " (id x y): called when a mix is dragged"

  mix_drag_hook = XEN_DEFINE_HOOK(S_mix_drag_hook, 3, H_mix_drag_hook); /* args = id, mouse x, mouse or tag y */

  /* the name draw-mix-hook is inconsistent with the other mix hooks (mix-draw-hook?), but is intended to parallel draw-mark-hook */
  #define H_draw_mix_hook S_draw_mix_hook " (id): called when a mix tag is about to be displayed"

  draw_mix_hook = XEN_DEFINE_HOOK(S_draw_mix_hook, 5, H_draw_mix_hook); /* arg = id, old-x, old-y, x, y */
}
