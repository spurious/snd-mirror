#include "snd.h"

/* selection support changed 11-Sep-00 to handle edit list movements */

static bool cp_has_selection(chan_info *cp, void *ignore)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  return((ed) && (ed->selection_beg != NO_SELECTION));
}

bool selection_is_active(void)
{
  /* is selection active in any channel */
  return(map_over_chans(cp_has_selection, NULL));
}

bool selection_is_active_in_channel(chan_info *cp)
{
  return((cp) && (cp_has_selection(cp, NULL)));
}

static bool selection_is_visible(chan_info *cp)
{
  ed_list *ed;
  axis_info *ap;
  ed = cp->edits[cp->edit_ctr];
  if (ed->selection_beg == NO_SELECTION) return(false);
  ap = cp->axis;
  return((ed) && 
	 (ap->losamp < ed->selection_end) && 
	 (ap->hisamp > ed->selection_beg));
}

bool selection_is_visible_in_channel (chan_info *cp)
{
  return((cp_has_selection(cp, NULL)) && 
	 (selection_is_visible(cp)));
}

static off_t off_t_map_over_chans(off_t (*func)(chan_info *, off_t *), off_t *userptr)
{
  int i, j;
  off_t val;
  snd_info *sp;
  chan_info *cp;
  val = 0;
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	for (j = 0; j < sp->nchans; j++)
	  if ((cp = ((chan_info *)(sp->chans[j]))))
	    {
	      val = (*func)(cp, userptr);
	      if (val) return(val);
	    }
    }
  return(val);
}

static off_t cp_selection_beg(chan_info *cp, off_t *beg) 
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  if (ed->selection_beg != NO_SELECTION)
    {
      beg[0] = ed->selection_beg;
      return(1); /* i.e. stop map_over_chans */
    }
  return(0);
}

off_t selection_beg(chan_info *cp)
{
  off_t beg[1];
  beg[0] = 0;
  if (cp)
    cp_selection_beg(cp, beg);
  else off_t_map_over_chans(cp_selection_beg, beg);
  return(beg[0]);
}

static void cp_set_selection_beg(chan_info *cp, off_t beg)
{
  ed_list *ed;
  off_t len;
  ed = cp->edits[cp->edit_ctr];
  len = CURRENT_SAMPLES(cp);
  if (beg < len)
    ed->selection_beg = beg;
  else ed->selection_beg = len - 1;
  ed->selection_maxamp = -1.0;
}

off_t selection_end(chan_info *cp) /* never called without selection_member check in advance */
{
  return(cp->edits[cp->edit_ctr]->selection_end);
}

static off_t cp_selection_len(chan_info *cp, off_t *ptr)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  if ((ed) && (ed->selection_beg != NO_SELECTION))
    return(ed->selection_end - ed->selection_beg + 1); 
  return(0);
}

off_t selection_len(void)
{
  return(off_t_map_over_chans(cp_selection_len, NULL));
}

static void cp_set_selection_len(chan_info *cp, off_t len)
{
  ed_list *ed;
  off_t cplen;
  ed = cp->edits[cp->edit_ctr];
  cplen = CURRENT_SAMPLES(cp);
  ed->selection_end = ed->selection_beg + len - 1;
  if (ed->selection_end >= cplen) ed->selection_end = cplen - 1;
  ed->selection_maxamp = -1.0;
}

static void selection_chans_1(chan_info *cp, void *count)
{
  int *counter = (int *)count;
  if (cp_has_selection(cp, NULL)) counter[0]++;
}

int selection_chans(void)
{
  int count[1];
  count[0] = 0;
  for_each_chan_1(selection_chans_1, (void *)count);
  return(count[0]);
}

static off_t selection_srate_1(chan_info *cp, off_t *ignored)
{
  if (cp_has_selection(cp, NULL)) 
    return((off_t)SND_SRATE(cp->sound));
  return(0);
}

int selection_srate(void)
{
  if (selection_is_active())
    return((int)off_t_map_over_chans(selection_srate_1, NULL));
  return(0);
}

Float selection_maxamp(chan_info *cp)
{
  Float val = 0.0;
  if (selection_is_active_in_channel(cp))
    {
      val = ed_selection_maxamp(cp);
      if (val >= 0.0) return(val);
      val = local_maxamp(cp, 
			 selection_beg(cp), 
			 selection_end(cp) - selection_beg(cp) + 1,
			 cp->edit_ctr);
      set_ed_selection_maxamp(cp, val);
    }
  return(val);
}

static void cp_delete_selection(chan_info *cp, void *origin)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  if ((ed) && (ed->selection_beg != NO_SELECTION))
    {
      delete_samples(ed->selection_beg, cp_selection_len(cp, NULL), cp, (const char *)origin, cp->edit_ctr);
      ed = cp->edits[cp->edit_ctr];
      ed->selection_beg = NO_SELECTION;
    }
}

bool delete_selection(const char *origin, cut_selection_regraph_t regraph)
{
  if (selection_is_active())
    {
      for_each_chan_1(cp_delete_selection, (void *)origin);
      if (regraph == UPDATE_DISPLAY) for_each_chan(update_graph);
      reflect_edit_without_selection_in_menu();
      return(true);
    }
  return(false);
}

static void cp_deactivate_selection(chan_info *cp)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  if (ed) ed->selection_beg = NO_SELECTION;
}

static sync_info *selection_creation_chans = NULL;

void deactivate_selection(void)
{
  for_each_chan(cp_deactivate_selection);
  for_each_chan(update_graph);
  reflect_edit_without_selection_in_menu();
  if (selection_creation_chans) 
    selection_creation_chans = free_sync_info(selection_creation_chans);
}

void reactivate_selection(chan_info *cp, off_t beg, off_t end)
{
  ed_list *ed;
  off_t len;
  ed = cp->edits[cp->edit_ctr];
  len = CURRENT_SAMPLES(cp) - 1;
  if (beg < 0) beg = 0;
  if (end < 0) end = 0;
  if (beg > len) beg = len;
  if (end > len) end = len;
  if (beg > end) end = beg;
  ed->selection_beg = beg;
  ed->selection_end = end;
  cp->selection_visible = false;
  ed->selection_maxamp = -1.0;
  reflect_edit_with_selection_in_menu();
}

static void update_selection(chan_info *cp, off_t newend)
{
  ed_list *ed;
  off_t samps;
  ed = cp->edits[cp->edit_ctr];
  ed->selection_maxamp = -1.0;
  if ((newend != ed->selection_beg) && (newend != ed->selection_end)) /* redundant call from somewhere */
    {
      if (newend < ed->selection_beg) 
	{
	  if (newend >= 0)
	    ed->selection_beg = newend;
	  else ed->selection_beg = 0;
	}
      else 
	{
	  samps = CURRENT_SAMPLES(cp);
	  if (newend < samps)
	    ed->selection_end = newend;
	  else ed->selection_end = samps - 1;
	}
    }
}

void ripple_selection(ed_list *ed, off_t beg, off_t num)
{
  /* beg = insert or delete begin point (snd-edits.c), num = samps inserted (num positive) or deleted (num negative) at beg */
  if (ed->selection_beg != NO_SELECTION)
    {
      if (beg < ed->selection_beg) 
	{
	  ed->selection_beg += num;
	  if (beg >= ed->selection_beg) 
	    ed->selection_beg = NO_SELECTION; /* deletion included some of current selection from outside */
	  else ed->selection_end += num;
	}
      else
	{
	  if (beg < ed->selection_end)
	    {
	      ed->selection_end += num;
	      if (ed->selection_end < beg)
		ed->selection_beg = NO_SELECTION; /* same as above but from end */
	    }
	}
    }
}

static void next_selection_chan(chan_info *cp, void *sidata)
{
  sync_info *si = (sync_info *)sidata;
  int chan;
  if (cp_has_selection(cp, NULL))
    {
      chan = si->chans;
      si->chans++;
      si->begs[chan] = selection_beg(cp);
      si->cps[chan] = cp;
    }
}

sync_info *selection_sync(void)
{
  sync_info *si;
  if (!(selection_is_active())) return(NULL);
  si = (sync_info *)CALLOC(1, sizeof(sync_info));
  si->chans = selection_chans();
  si->cps = (chan_info **)CALLOC(si->chans, sizeof(chan_info *));
  si->begs = (off_t *)CALLOC(si->chans, sizeof(off_t));
  si->chans = 0;
  for_each_chan_1(next_selection_chan, (void *)si);
  return(si);
}

static int mix_selection(chan_info *cp, off_t beg, const char *origin, int trk)
{
  char *tempfile = NULL;
  sync_info *si_out;
  int err, id = INVALID_MIX_ID;
  if (!(editable_p(cp))) return(id);
  tempfile = snd_tempnam();
  err = save_selection(tempfile, MUS_NEXT, MUS_OUT_FORMAT, SND_SRATE(cp->sound), NULL, SAVE_ALL_CHANS);
  if (err == MUS_NO_ERROR)
    {
      si_out = sync_to_chan(cp);
      id = mix_file(beg, selection_len(), si_out->chans, si_out->cps, tempfile, 
		    (si_out->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME, 
		    origin, with_mix_tags(ss), trk);
      free_sync_info(si_out);	      
    }
  if (tempfile) FREE(tempfile);
  cp->edit_hook_checked = false;
  return(id);
}

void add_selection_or_region(int reg, chan_info *cp, const char *origin)
{
  if (cp) 
    {
      if ((reg == 0) && (selection_is_active()))
	mix_selection(cp, CURSOR(cp), origin, 0);
      else add_region(reg, cp, origin);
    }
}

static int insert_selection(chan_info *cp, off_t beg, const char *origin)
{
  char *tempfile = NULL;
  sync_info *si_out, *si_in;
  chan_info *cp_in, *cp_out;
  int i, err = MUS_NO_ERROR, out_format = MUS_OUT_FORMAT;
  off_t len;
  if (!(editable_p(cp))) return(MUS_NO_ERROR);
  if (mus_header_writable(MUS_NEXT, cp->sound->hdr->format))
    out_format = cp->sound->hdr->format;
  tempfile = snd_tempnam();
  err = save_selection(tempfile, MUS_NEXT, out_format, SND_SRATE(cp->sound), NULL, SAVE_ALL_CHANS);
  if (err == MUS_NO_ERROR)
    {
      si_out = sync_to_chan(cp);
      si_in = selection_sync();
      if (si_in->chans > 1) 
	remember_temp(tempfile, si_in->chans);
      for (i = 0; ((i < si_in->chans) && (i < si_out->chans)); i++)
	{
	  cp_out = si_out->cps[i]; /* currently syncd chan that we might paste to */
	  cp_in = si_in->cps[i];   /* selection chan to paste in (no wrap-around here) */
	  len = cp_selection_len(cp_in, NULL);
	  if (file_insert_samples(beg, len,
				  tempfile, cp_out, i,
				  (si_in->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
				  origin, cp_out->edit_ctr))
	    update_graph(cp_out);
	}
      free_sync_info(si_in);
      free_sync_info(si_out);
    }
  if (tempfile) FREE(tempfile);
  cp->edit_hook_checked = false;
  return(err);
}

void insert_selection_or_region(int reg, chan_info *cp, const char *origin)
{
  if (cp) 
    {
      if ((reg == 0) && (selection_is_active()))
	insert_selection(cp, CURSOR(cp), origin);
      else paste_region(reg, cp, origin);
    }
}

void insert_selection_from_menu(void)
{
  insert_selection_or_region(0, selected_channel(), "Edit: Insert selection");
}


/* we're drawing the selection in one channel, but others may be sync'd to it */

void start_selection_creation(chan_info *cp, off_t samp)
{  
  int i;
  if ((selection_creation_chans) && (selection_creates_region(ss)))
    /* hmmm -- if keyboard selection in progress, then mouse press? */
    make_region_from_selection();
  deactivate_selection();
  selection_creation_chans = sync_to_chan(cp);
  for (i = 0; i < selection_creation_chans->chans; i++)
    reactivate_selection(selection_creation_chans->cps[i], samp, samp);
}

static void redraw_selection(void);

void update_possible_selection_in_progress(off_t samp)
{
  int i;
  if (selection_creation_chans)
    {
      for (i = 0; i < selection_creation_chans->chans; i++)
	update_selection(selection_creation_chans->cps[i], samp);
      redraw_selection();
    }
}

bool selection_creation_in_progress(void) {return(selection_creation_chans != NULL);}

void finish_selection_creation(void)
{
  if (selection_creation_chans)
    {
      if (selection_creates_region(ss)) 
	make_region_from_selection();
      reflect_edit_with_selection_in_menu();
      selection_creation_chans = free_sync_info(selection_creation_chans);      
    }
}

static void cp_redraw_selection(chan_info *cp, void *with_fft)
{
  Locus x0, x1;
  off_t beg, end;
  axis_info *ap;
  double sp_srate;
  if (selection_is_visible(cp))
    {
      ap = cp->axis;
      beg = selection_beg(cp);
      end = selection_end(cp);
      sp_srate = (double)SND_SRATE(cp->sound);
      if (ap->losamp < beg)
	x0 = grf_x((double)beg / sp_srate, ap);
      else x0 = ap->x_axis_x0;
      if (ap->hisamp > end)
	x1 = grf_x((double)end / sp_srate, ap);
      else x1 = ap->x_axis_x1;
      if (cp->selection_visible)
	fill_rectangle(selection_context(cp),
		       cp->old_x0,
		       ap->y_axis_y1,
		       cp->old_x1 - cp->old_x0,
		       (int)(ap->y_axis_y0 - ap->y_axis_y1));
      fill_rectangle(selection_context(cp),
		     x0,
		     ap->y_axis_y1,
		     x1 - x0,
		     (int)(ap->y_axis_y0 - ap->y_axis_y1));
      cp->old_x0 = x0;
      cp->old_x1 = x1;
      cp->selection_visible = true;
    }
  if ((with_fft) && (cp->graph_transform_p) && (cp_has_selection(cp, NULL)) && (!(chan_fft_in_progress(cp))))
    {
      if (show_selection_transform(ss)) 
	calculate_fft(cp);
    }
}

static void redraw_selection(void)
{
  /* selection transform while synced redraw */
  for_each_chan_1(cp_redraw_selection, (void *)1);
}

void display_selection(chan_info *cp)
{ 
  cp_redraw_selection(cp, NULL);
}

int make_region_from_selection(void)
{
  off_t *ends = NULL;
  int i, id = -1;
  bool happy = false;
  sync_info *si;
  if (!(selection_is_active())) return(-1);
  si = selection_sync();
  ends = (off_t *)CALLOC(si->chans, sizeof(off_t));
  for (i = 0; i < si->chans; i++) 
    {
      ends[i] = selection_end(si->cps[i]);
      if (ends[i] > si->begs[i]) happy = true;
      /* C-space followed by mouse click causes a bogus selection-creation event */
    }
  if (happy) id = define_region(si, ends);
  si = free_sync_info(si);
  if (ends) FREE(ends);
  return(id);
}

int select_all(chan_info *cp)
{
  sync_info *si;
  int i;
  if (cp) 
    {
      deactivate_selection();
      si = sync_to_chan(cp);
      for (i = 0; i < si->chans; i++)
	{
	  reactivate_selection(si->cps[i], 0, CURRENT_SAMPLES(si->cps[i]));
	  update_graph(si->cps[i]);
	}
      si = free_sync_info(si);
      if (selection_creates_region(ss)) 
	return(make_region_from_selection());
    }
  return(-1);
}


/* ---------------- selection mouse motion ---------------- */

static int last_selection_x = 0;
static Cessator watch_selection_button = 0;

void cancel_selection_watch(void)
{
  if (watch_selection_button)
    BACKGROUND_REMOVE(watch_selection_button);
  watch_selection_button = 0;
}

static void start_selection_watching(chan_info *cp);

static void move_selection_1(chan_info *cp, int x)
{
  axis_info *ap;
  ap = cp->axis;
  if ((x > ap->x_axis_x1) || (x < ap->x_axis_x0)) 
    {
      if (((x > ap->x_axis_x1) && (ap->x1 == ap->xmax)) ||
	  ((x < ap->x_axis_x0) && (ap->x0 == ap->xmin)))
	return;
      move_axis(cp, ap, x);
      if (!watch_selection_button) start_selection_watching(cp);
    }
  else 
    if (watch_selection_button) 
      cancel_selection_watch();
  redraw_selection();
}

static Cessate WatchSelection(Indicium cp)
{
  if (watch_selection_button)
    {
      move_selection_1((chan_info *)cp, last_selection_x);
      return(BACKGROUND_CONTINUE);
    }
  else return(BACKGROUND_QUIT);
}

static void start_selection_watching(chan_info *cp)
{
  watch_selection_button = BACKGROUND_ADD(WatchSelection, cp);
}

void move_selection(chan_info *cp, int x)
{
  last_selection_x = x; /* called in snd-xchn -- sets last_selection_x */
  move_selection_1(cp, x);
}

int save_selection(char *ofile, int type, int format, int srate, const char *comment, int chan)
{
  /* type and format have already been checked */
  int ofd, comlen, err = MUS_NO_ERROR, bps;
  bool reporting = false;
  disk_space_t no_space;
  off_t oloc;
  sync_info *si = NULL;
  off_t *ends;
  int i, j, k, chans;
  off_t dur, num, ioff;
  snd_fd **sfs;
  snd_info *sp = NULL;
  mus_sample_t **data;
  si = selection_sync();
  if ((si) && (si->cps) && (si->cps[0])) sp = si->cps[0]->sound;
  comlen = snd_strlen(comment);
  dur = selection_len();
  if (chan == SAVE_ALL_CHANS)
    chans = si->chans;
  else chans = 1;
  if ((snd_write_header(ofile, type, srate, chans, 28, chans * dur, format, comment, comlen, NULL)) == -1) 
    {
      si = free_sync_info(si);
      return(MUS_HEADER_WRITE_FAILED);
    }
  oloc = mus_header_data_location();
  ofd = snd_reopen_write(ofile);
  if (sp)
    {
      bool copy_ok = false;
      bps = mus_bytes_per_sample(format);
      num = dur * bps * chans;
      no_space = disk_space_p(sp, num, 0, ofile);
      if (no_space == GIVE_UP)
	{
	  /* has already interacted with user about disk problem */
	  snd_close(ofd, ofile);
	  si = free_sync_info(si);
	  return(MUS_WRITE_ERROR);
	}
      copy_ok = ((format == sp->hdr->format) && 
		 (chans == sp->nchans) &&
		 (chan == SAVE_ALL_CHANS));
      if (copy_ok)
	for (i = 0; i < chans; i++)
	  if ((sp->chans[i]->edit_ctr != 0) || 
	      (si->cps[i]->sound != sp) ||
	      (si->begs[i] != si->begs[0]))
	    {
	      copy_ok = false;
	      break;
	    }
      if (copy_ok)
	{
	  /* write next header with correct len
	   * seek loc in sp->filename
	   * copy len*data-size bytes
	   * get max from amp envs
	   */
	  off_t bytes, iloc;
	  int fdi;
	  char *buffer;
	  lseek(ofd, oloc, SEEK_SET);
	  fdi = mus_file_open_read(sp->filename); /* this does not read the header */
	  if (fdi == -1)
	    snd_error(_("can't read selection's original sound? %s: %s"), sp->filename, strerror(errno));
	  else
	    {
	      iloc = mus_sound_data_location(sp->filename);
	      lseek(fdi, iloc + chans * bps * si->begs[0], SEEK_SET);
	      buffer = (char *)CALLOC(MAX_BUFFER_SIZE, sizeof(char));
	      for (j = 0; j < num; j += MAX_BUFFER_SIZE)
		{
		  bytes = num - j;
		  if (bytes > MAX_BUFFER_SIZE) bytes = MAX_BUFFER_SIZE;
		  read(fdi, buffer, bytes);
		  write(ofd, buffer, bytes);
		}
	      FREE(buffer);
	      snd_close(fdi, sp->filename);
	    }
	  snd_close(ofd, ofile);
	  si = free_sync_info(si);
	  alert_new_file();
	  return(MUS_NO_ERROR);
	}
    }
  reporting = ((sp) && (dur > REPORTING_SIZE));
  if (reporting) start_progress_report(sp, NOT_FROM_ENVED);
  ends = (off_t *)CALLOC(chans, sizeof(off_t));
  sfs = (snd_fd **)CALLOC(chans, sizeof(snd_fd *));
  if (chan == SAVE_ALL_CHANS)
    for (i = 0; i < chans; i++) 
      {
	ends[i] = selection_end(si->cps[i]);
	sfs[i] = init_sample_read(selection_beg(si->cps[i]), si->cps[i], READ_FORWARD);
      }
  else
    {
      ends[0] = selection_end(si->cps[chan]);
      sfs[0] = init_sample_read(selection_beg(si->cps[chan]), si->cps[chan], READ_FORWARD);
    }
  mus_file_open_descriptors(ofd, ofile, format, 
			    mus_bytes_per_sample(format), 
			    oloc, chans, type);
  mus_file_set_data_clipped(ofd, data_clipped(ss));
  lseek(ofd, oloc, SEEK_SET);
  data = (mus_sample_t **)CALLOC(chans, sizeof(mus_sample_t *));
  for (i = 0; i < chans; i++) 
    data[i] = (mus_sample_t *)CALLOC(FILE_BUFFER_SIZE, sizeof(mus_sample_t)); 
  j = 0;
  ss->stopped_explicitly = false;
  for (ioff = 0; ioff < dur; ioff++)
    {
      for (k = 0; k < chans; k++)
	{
	  if (ioff <= ends[k]) 
	    data[k][j] = read_sample(sfs[k]);
	  else data[k][j] = MUS_SAMPLE_0;
	}
      j++;
      if (j == FILE_BUFFER_SIZE)
	{
	  err = mus_file_write(ofd, 0, j - 1, chans, data);
	  j = 0;
	  if (err == MUS_ERROR) break; /* error message already posted */
	  if (reporting) 
	    progress_report(sp, S_save_selection, chans - 1, chans, (Float)((double)ioff / (double)dur), NOT_FROM_ENVED);
	  if (ss->stopped_explicitly)
	    {
	      ss->stopped_explicitly = false;
	      snd_warning(_("save selection stopped"));
	      break;
	    }
	}
    }
  if ((err == MUS_NO_ERROR) && (j > 0)) 
    mus_file_write(ofd, 0, j - 1, chans, data);
  if (reporting) finish_progress_report(sp, NOT_FROM_ENVED);
  for (i = 0; i < chans; i++)
    {
      free_snd_fd(sfs[i]);
      FREE(data[i]);
    }
  FREE(sfs);
  FREE(data);
  si = free_sync_info(si);
  FREE(ends);
  if (mus_file_close(ofd) != 0)
    {
      snd_error(_("save-selection: can't close %s: %s!"), ofile, strerror(errno));
      return(MUS_CANT_CLOSE_FILE);
    }
  alert_new_file();
  return(err);
}


static XEN g_delete_selection(void)
{
  #define H_delete_selection "(" S_delete_selection "): delete the currently selected portion"
  if (selection_is_active())
    {
      delete_selection(S_delete_selection, UPDATE_DISPLAY);
      return(XEN_TRUE);
    }
  return(snd_no_active_selection_error(S_delete_selection));
}

static XEN g_insert_selection(XEN beg, XEN snd, XEN chn)
{
  #define H_insert_selection "(" S_insert_selection " (beg 0) (snd #f) (chn #f)): insert the currently selected portion starting at beg"
  chan_info *cp;
  off_t samp;
  int err = MUS_NO_ERROR;
  if (selection_is_active())
    {
      ASSERT_CHANNEL(S_insert_selection, snd, chn, 2);
      XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(beg), beg, XEN_ARG_1, S_insert_selection, "a number");
      cp = get_cp(snd, chn, S_insert_selection);
      samp = beg_to_sample(beg, S_insert_selection);
      err = insert_selection(cp, samp, S_insert_selection);
      return(C_TO_XEN_BOOLEAN((err == MUS_NO_ERROR)));
    }
  return(snd_no_active_selection_error(S_insert_selection));
}

static XEN g_mix_selection(XEN beg, XEN snd, XEN chn, XEN id)
{
  #define H_mix_selection "(" S_mix_selection " (beg 0) (snd #f) (chn #f) (track 0)): mix the currently selected portion starting at beg"
  chan_info *cp;
  off_t obeg;
  int track_id;
  if (selection_is_active())
    {
      ASSERT_CHANNEL(S_mix_selection, snd, chn, 2);
      XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(beg), beg, XEN_ARG_1, S_mix_selection, "a number");
      XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(id), id, XEN_ARG_4, S_mix_selection, "an integer");
      cp = get_cp(snd, chn, S_mix_selection);
      obeg = beg_to_sample(beg, S_mix_selection);
      track_id = XEN_TO_C_INT_OR_ELSE(id, 0);
      if ((track_id > 0) && (!(track_p(track_id))))
	XEN_ERROR(NO_SUCH_TRACK,
		  XEN_LIST_2(C_TO_XEN_STRING(S_mix_selection),
			     C_TO_XEN_INT(id)));
      return(C_TO_XEN_INT(mix_selection(cp, obeg, S_mix_selection, track_id)));
    }
  return(snd_no_active_selection_error(S_mix_selection));
}

static XEN g_selection_p(void)
{
  #define H_selection_p "(" S_selection_p "): #t if selection is currently active, visible, etc"
  return(C_TO_XEN_BOOLEAN(selection_is_active()));
}

static XEN g_selection_position(XEN snd, XEN chn)
{
  #define H_selection_position "(" S_selection_position " (snd #f) (chn #f)): selection start samp"
  chan_info *cp;
  if (selection_is_active())
    {
      if (XEN_NOT_BOUND_P(snd))
	return(C_TO_XEN_OFF_T(selection_beg(NULL)));
      else
	{
	  ASSERT_CHANNEL(S_selection_position, snd, chn, 1);
	  cp = get_cp(snd, chn, S_selection_position);
	  return(C_TO_XEN_OFF_T(selection_beg(cp)));
	}
    }
  return(snd_no_active_selection_error(S_selection_position));
}

static XEN g_set_selection_position(XEN pos, XEN snd, XEN chn)
{
  chan_info *cp;
  sync_info *si = NULL;
  int i;
  off_t beg;
  ASSERT_CHANNEL(S_setB S_selection_position, snd, chn, 2);
  XEN_ASSERT_TYPE(XEN_NUMBER_P(pos), pos, XEN_ARG_1, S_selection_position, "a number");
  beg = beg_to_sample(pos, S_setB S_selection_position);
  if (XEN_NOT_BOUND_P(snd))
    {
      if (selection_is_active())
	si = selection_sync();
      else 
	{
	  cp = current_channel();
	  if (cp) si = sync_to_chan(cp);
	}
      if (si)
	{
	  for (i = 0; i < si->chans; i++) 
	    cp_set_selection_beg(si->cps[i], beg);
	  si = free_sync_info(si);
	}
    }
  else 
    {
      cp = get_cp(snd, chn, S_setB S_selection_position);
      cp_set_selection_beg(cp, beg);
    }
  redraw_selection();
  return(pos);
}

WITH_REVERSED_CHANNEL_ARGS(g_set_selection_position_reversed, g_set_selection_position)

static XEN g_selection_frames(XEN snd, XEN chn)
{
  #define H_selection_frames "(" S_selection_frames " (snd #f) (chn #f)): selection length"
  chan_info *cp;
  if (selection_is_active())
    {
      if (XEN_NOT_BOUND_P(snd))
	return(C_TO_XEN_OFF_T(selection_len()));
      else
	{
	  ASSERT_CHANNEL(S_selection_frames, snd, chn, 1);
	  cp = get_cp(snd, chn, S_selection_frames);
	  return(C_TO_XEN_OFF_T(cp_selection_len(cp, NULL)));
	}
    }
  return(snd_no_active_selection_error(S_selection_frames));
}

static XEN g_set_selection_frames(XEN samps, XEN snd, XEN chn)
{
  chan_info *cp;
  sync_info *si = NULL;
  int i;
  off_t len;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samps), samps, XEN_ARG_1, S_setB S_selection_frames, "a number");
  len = XEN_TO_C_OFF_T_OR_ELSE(samps, 0);
  if (len <= 0)
    XEN_WRONG_TYPE_ARG_ERROR(S_setB S_selection_frames, XEN_ARG_1, samps, "a positive integer");
  if (XEN_NOT_BOUND_P(snd))
    {
      if (selection_is_active())
	si = selection_sync();
      else 
	{
	  cp = current_channel();
	  if (cp) si = sync_to_chan(cp);
	}
      if (si)
	{
	  for (i = 0; i < si->chans; i++)
	    cp_set_selection_len(si->cps[i], len);
	  si = free_sync_info(si);
	}
    }
  else 
    {
      ASSERT_CHANNEL(S_setB S_selection_frames, snd, chn, 2);
      cp = get_cp(snd, chn, S_setB S_selection_frames);
      cp_set_selection_len(cp, len);
    }
  redraw_selection();
  return(samps);
}

WITH_REVERSED_CHANNEL_ARGS(g_set_selection_frames_reversed, g_set_selection_frames)

static XEN g_selection_member(XEN snd, XEN chn)
{
  #define H_selection_member "(" S_selection_member " (snd #f) (chn #f)): #t if snd's channel chn is a member of the current selection"
  chan_info *cp;
  ASSERT_CHANNEL(S_selection_member, snd, chn, 1);
  cp = get_cp(snd, chn, S_selection_member);
  return(C_TO_XEN_BOOLEAN(selection_is_active_in_channel(cp)));
}

static XEN g_set_selection_member(XEN on, XEN snd, XEN chn)
{
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_selection_member, "a boolean");
  if ((XEN_TRUE_P(snd)) && (XEN_FALSE_P(on)))
    deactivate_selection();
  else
    {
      ASSERT_CHANNEL(S_setB S_selection_member, snd, chn, 2);
      cp = get_cp(snd, chn, S_setB S_selection_member);
      if (XEN_TRUE_P(on))
	{
	  if (selection_is_active())
	    cp_set_selection_beg(cp, selection_beg(NULL));
	  else cp_set_selection_beg(cp, 0);
	}
      else cp_deactivate_selection(cp);
      if (selection_is_active())
	{
	  reflect_edit_with_selection_in_menu();
	  redraw_selection();
	}
      else reflect_edit_without_selection_in_menu();
    }
  return(on);
}

WITH_REVERSED_CHANNEL_ARGS(g_set_selection_member_reversed, g_set_selection_member)

static XEN g_select_all(XEN snd_n, XEN chn_n)
{
  #define H_select_all "(" S_select_all " (snd #f) (chn #f)): make a new selection containing all of snd's channel chn. \
If sync is set, all chans are included.  The new region id is returned (if " S_selection_creates_region " is #t)."
  chan_info *cp;
  int id;
  ASSERT_CHANNEL(S_select_all, snd_n, chn_n, 1);
  cp = get_cp(snd_n, chn_n, S_select_all);
  id = select_all(cp);
  if (selection_creates_region(ss)) 
    return(C_TO_XEN_INT(id)); /* C_INT_TO_XEN_REGION to be consistent with-snd-region.c */
  else return(XEN_TRUE);
}

static XEN g_save_selection(XEN filename, XEN header_type, XEN data_format, XEN srate, XEN comment, XEN chan)
{
  #define H_save_selection "(" S_save_selection " filename (header-type #f) (data-format #f) (srate #f) (comment #f) (chan #f)): \
save the current selection in filename using the indicated file attributes.  If chan is given, save only that channel."
  int type, format, sr, err, chn;
  char *com = NULL, *fname = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ARG_1, S_save_selection, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(header_type), header_type, XEN_ARG_2, S_save_selection, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(data_format), data_format, XEN_ARG_3, S_save_selection, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_OR_BOOLEAN_IF_BOUND_P(srate), srate, XEN_ARG_4, S_save_selection, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(chan), chan, XEN_ARG_6, S_save_selection, "an integer");
  if (!(selection_is_active()))
    return(snd_no_active_selection_error(S_save_selection));
  if (XEN_INTEGER_P(header_type)) 
    type = XEN_TO_C_INT(header_type); 
  else type = MUS_NEXT;
  format = XEN_TO_C_INT_OR_ELSE(data_format, MUS_OUT_FORMAT);
  if (!(mus_header_writable(type, -2)))
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_save_selection),
			 C_TO_XEN_STRING(_("can't write this header type:")),
			 C_TO_XEN_STRING(mus_header_type_name(type))));
  if (!(mus_header_writable(type, format)))
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_4(C_TO_XEN_STRING(S_save_selection),
			 C_TO_XEN_STRING(_("can't write this combination of header type and data format:")),
			 C_TO_XEN_STRING(mus_header_type_name(type)),
			 C_TO_XEN_STRING(mus_data_format_name(format))));
  sr = XEN_TO_C_INT_OR_ELSE(srate, selection_srate());
  if (XEN_STRING_P(comment)) 
    com = XEN_TO_C_STRING(comment); 
  else com = NULL;
  chn = XEN_TO_C_INT_OR_ELSE(chan, SAVE_ALL_CHANS);
  fname = mus_expand_filename(XEN_TO_C_STRING(filename));
  ss->catch_message = NULL;
  err = save_selection(fname, type, format, sr, com, chn);
  if (fname) FREE(fname);
  if (err != MUS_NO_ERROR) 
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_2(C_TO_XEN_STRING(S_save_selection),
			 C_TO_XEN_STRING(ss->catch_message)));
  return(filename);
}

static XEN g_selection_chans(void)
{
  #define H_selection_chans "(" S_selection_chans "): chans in active selection"
  return(C_TO_XEN_INT(selection_chans()));
}

static XEN g_selection_srate(void)
{
  #define H_selection_srate "(" S_selection_srate "): selection srate"
  return(C_TO_XEN_INT(selection_srate()));
}

static XEN g_selection_maxamp(XEN snd, XEN chn)
{
  #define H_selection_maxamp "(" S_selection_maxamp " (snd #f) (chn #f)): selection maxamp in given channel"
  chan_info *cp;
  ASSERT_CHANNEL(S_selection_maxamp, snd, chn, 1);
  cp = get_cp(snd, chn, S_selection_maxamp);
  return(C_TO_XEN_DOUBLE(selection_maxamp(cp)));
}

#ifdef XEN_ARGIFY_1
XEN_ARGIFY_2(g_selection_position_w, g_selection_position)
XEN_ARGIFY_3(g_set_selection_position_w, g_set_selection_position)
XEN_ARGIFY_2(g_selection_frames_w, g_selection_frames)
XEN_ARGIFY_3(g_set_selection_frames_w, g_set_selection_frames)
XEN_ARGIFY_2(g_selection_member_w, g_selection_member)
XEN_ARGIFY_3(g_set_selection_member_w, g_set_selection_member)
XEN_NARGIFY_0(g_selection_p_w, g_selection_p)
XEN_NARGIFY_0(g_selection_chans_w, g_selection_chans)
XEN_NARGIFY_0(g_selection_srate_w, g_selection_srate)
XEN_ARGIFY_2(g_selection_maxamp_w, g_selection_maxamp)
XEN_NARGIFY_0(g_delete_selection_w, g_delete_selection)
XEN_ARGIFY_3(g_insert_selection_w, g_insert_selection)
XEN_ARGIFY_4(g_mix_selection_w, g_mix_selection)
XEN_ARGIFY_2(g_select_all_w, g_select_all)
XEN_ARGIFY_6(g_save_selection_w, g_save_selection)
#else
#define g_selection_position_w g_selection_position
#define g_set_selection_position_w g_set_selection_position
#define g_selection_frames_w g_selection_frames
#define g_set_selection_frames_w g_set_selection_frames
#define g_selection_member_w g_selection_member
#define g_set_selection_member_w g_set_selection_member
#define g_selection_p_w g_selection_p
#define g_selection_chans_w g_selection_chans
#define g_selection_srate_w g_selection_srate
#define g_selection_maxamp_w g_selection_maxamp
#define g_delete_selection_w g_delete_selection
#define g_insert_selection_w g_insert_selection
#define g_mix_selection_w g_mix_selection
#define g_select_all_w g_select_all
#define g_save_selection_w g_save_selection
#endif

void g_init_selection(void)
{
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_selection_position, g_selection_position_w, H_selection_position,
					    S_setB S_selection_position, g_set_selection_position_w, g_set_selection_position_reversed,
					    0, 2, 1, 2);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_selection_frames, g_selection_frames_w, H_selection_frames,
					    S_setB S_selection_frames, g_set_selection_frames_w, g_set_selection_frames_reversed,
					    0, 2, 1, 2);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_selection_member, g_selection_member_w, H_selection_member,
					    S_setB S_selection_member, g_set_selection_member_w, g_set_selection_member_reversed,
					    0, 2, 1, 2);

  XEN_DEFINE_PROCEDURE(S_selection_p,      g_selection_p_w, 0, 0, 0,      H_selection_p);
  XEN_DEFINE_PROCEDURE(S_selection_chans,  g_selection_chans_w, 0, 0, 0,  H_selection_chans);
  XEN_DEFINE_PROCEDURE(S_selection_srate,  g_selection_srate_w, 0, 0, 0,  H_selection_srate);
  XEN_DEFINE_PROCEDURE(S_selection_maxamp, g_selection_maxamp_w, 0, 2, 0, H_selection_maxamp);
  XEN_DEFINE_PROCEDURE(S_delete_selection, g_delete_selection_w, 0, 0, 0, H_delete_selection);
  XEN_DEFINE_PROCEDURE(S_insert_selection, g_insert_selection_w, 0, 3, 0, H_insert_selection);
  XEN_DEFINE_PROCEDURE(S_mix_selection,    g_mix_selection_w, 0, 4, 0,    H_mix_selection);
  XEN_DEFINE_PROCEDURE(S_select_all,       g_select_all_w, 0, 2, 0,       H_select_all);
  XEN_DEFINE_PROCEDURE(S_save_selection,   g_save_selection_w, 1, 5, 0,   H_save_selection);
}
