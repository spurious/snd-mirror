#include "snd.h"

/* selection support changed 11-Sep-00 to handle edit list movements */

static int cp_has_selection(chan_info *cp, void *ignore)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  return((ed) && (ed->selection_beg != NO_SELECTION));
}

int selection_is_active(void)
{
  /* is selection active in any channel */
  return(map_over_chans(get_global_state(), cp_has_selection, NULL));
}

int selection_is_active_in_channel(chan_info *cp)
{
  return((cp) && (cp_has_selection(cp, NULL)));
}

static int selection_is_visible(chan_info *cp)
{
  ed_list *ed;
  axis_info *ap;
  ed = cp->edits[cp->edit_ctr];
  if (ed->selection_beg == NO_SELECTION) return(0);
  ap = cp->axis;
  return((ed) && 
	 (ap->losamp < ed->selection_end) && 
	 (ap->hisamp > ed->selection_beg));
}

int selection_is_visible_in_channel (chan_info *cp)
{
  return((cp_has_selection(cp, NULL)) && (selection_is_visible(cp)));
}

static int cp_selection_beg(chan_info *cp, void *begptr) 
{
  ed_list *ed;
  int *beg = (int *)begptr;
  ed = cp->edits[cp->edit_ctr];
  if (ed->selection_beg != NO_SELECTION)
    {
      beg[0] = ed->selection_beg;
      return(1); /* i.e. stop map_over_chans */
    }
  return(0);
}

int selection_beg(chan_info *cp)
{
  int beg[1];
  beg[0] = 0;
  if (cp)
    cp_selection_beg(cp, (void *)beg);
  else map_over_chans(get_global_state(), cp_selection_beg, (void *)beg);
  return(beg[0]);
}

static void cp_set_selection_beg(chan_info *cp, int beg)
{
  ed_list *ed;
  int len;
  ed = cp->edits[cp->edit_ctr];
  len = current_ed_samples(cp);
  if (beg < len)
    ed->selection_beg = beg;
  else ed->selection_beg = len - 1;
}

static int cp_selection_end(chan_info *cp, void *begptr) 
{
  ed_list *ed;
  int *beg = (int *)begptr;
  ed = cp->edits[cp->edit_ctr];
  if (ed->selection_beg != NO_SELECTION)
    {
      beg[0] = ed->selection_end;
      return(1); /* i.e. stop map_over_chans */
    }
  return(0);
}

int selection_end(chan_info *cp)
{
  int beg[1];
  beg[0] = 0;
  if (cp)
    cp_selection_end(cp, (void *)beg);
  else map_over_chans(get_global_state(), cp_selection_end, (void *)beg);
  return(beg[0]);
}

static int cp_selection_len(chan_info *cp, void *ptr)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  if ((ed) && (ed->selection_beg != NO_SELECTION))
    return(ed->selection_end - ed->selection_beg + 1); 
  return(0);
}

int selection_len(void)
{
  return(map_over_chans(get_global_state(), cp_selection_len, NULL));
}

static void cp_set_selection_len(chan_info *cp, int len)
{
  ed_list *ed;
  int cplen;
  ed = cp->edits[cp->edit_ctr];
  cplen = current_ed_samples(cp);
  ed->selection_end = ed->selection_beg + len - 1;
  if (ed->selection_end >= cplen) ed->selection_end = cplen - 1;
}

static int selection_chans_1(chan_info *cp, void *count)
{
  int *counter = (int *)count;
  if (cp_has_selection(cp, NULL)) counter[0]++;
  return(0);
}

int selection_chans(void)
{
  int count[1];
  count[0] = 0;
  map_over_chans(get_global_state(), selection_chans_1, (void *)count);
  return(count[0]);
}

static int cp_delete_selection(chan_info *cp, void *origin)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  if ((ed) && (ed->selection_beg != NO_SELECTION))
    {
      delete_samples(ed->selection_beg, cp_selection_len(cp, NULL), cp, (const char *)origin);
      ed = cp->edits[cp->edit_ctr];
      ed->selection_beg = NO_SELECTION;
    }
  return(0);
}

int delete_selection(const char *origin, int regraph)
{
  snd_state *ss;
  if (selection_is_active())
    {
      ss = get_global_state();
      map_over_chans(ss, cp_delete_selection, (void *)origin);
      if (regraph == UPDATE_DISPLAY) map_over_chans(ss, update_graph, NULL);
      reflect_edit_without_selection_in_menu();
      return(1);
    }
  return(0);
}

static int cp_deactivate_selection(chan_info *cp, void *ignore)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  if (ed) ed->selection_beg = NO_SELECTION;
  return(0);
}

static sync_info *selection_creation_chans = NULL;

void deactivate_selection(void)
{
  snd_state *ss;
  ss = get_global_state();
  map_over_chans(ss, cp_deactivate_selection, NULL);
  map_over_chans(ss, update_graph, NULL);
  reflect_edit_without_selection_in_menu();
  if (selection_creation_chans) 
    selection_creation_chans = free_sync_info(selection_creation_chans);
}

void reactivate_selection(chan_info *cp, int beg, int end)
{
  ed_list *ed;
  int len;
  ed = cp->edits[cp->edit_ctr];
  len = current_ed_samples(cp) - 1;
  if (beg < 0) beg = 0;
  if (end < 0) end = 0;
  if (beg > len) beg = len;
  if (end > len) end = len;
  if (beg > end) end = beg;
  ed->selection_beg = beg;
  ed->selection_end = end;
  cp->selection_visible = 0;
  reflect_edit_with_selection_in_menu();
}

static void update_selection(chan_info *cp, int newend)
{
  ed_list *ed;
  int samps;
  ed = cp->edits[cp->edit_ctr];
  if (newend < ed->selection_beg) 
    {
      if (newend >= 0)
	ed->selection_beg = newend;
      else ed->selection_beg = 0;
    }
  else 
    {
      samps = current_ed_samples(cp);
      if (newend < samps)
	ed->selection_end = newend;
      else ed->selection_end = samps-1;
    }
}

void ripple_selection(ed_list *ed, int beg, int num)
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

static int next_selection_chan(chan_info *cp, void *sidata)
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
  return(0);
}

sync_info *selection_sync(void)
{
  sync_info *si;
  if (!(selection_is_active())) return(NULL);
  si = (sync_info *)CALLOC(1, sizeof(sync_info));
  si->chans = selection_chans();
  si->cps = (chan_info **)CALLOC(si->chans, sizeof(chan_info *));
  si->begs = (int *)CALLOC(si->chans, sizeof(int));
  si->chans = 0;
  map_over_chans(get_global_state(), next_selection_chan, (void *)si);
  return(si);
}

static int mix_selection(snd_state *ss, chan_info *cp, int beg, const char *origin)
{
  char *tempfile = NULL;
  sync_info *si_out;
  int err, id = INVALID_MIX_ID;
  tempfile = snd_tempnam(ss);
  err = save_selection(ss, tempfile, MUS_NEXT, MUS_OUT_FORMAT, SND_SRATE(cp->sound), NULL);
  if (err == MUS_NO_ERROR)
    {
      si_out = sync_to_chan(cp);
      id = mix_file_and_delete(beg, selection_len(), tempfile, si_out->cps, si_out->chans, origin, with_mix_tags(ss));
      free_sync_info(si_out);	      
    }
  return(id);
}

void add_selection_or_region(snd_state *ss, int reg, chan_info *cp, const char *origin)
{
  if (cp) 
    {
      if ((reg == 0) && (selection_is_active()))
	mix_selection(ss, cp, cp->cursor, origin);
      else add_region(reg, cp, origin);
    }
}

void mix_selection_from_menu(snd_state *ss)
{
  add_selection_or_region(ss, 0, selected_channel(ss), "Edit: mix");
}

static int insert_selection(snd_state *ss, chan_info *cp, int beg, const char *origin)
{
  char *tempfile = NULL;
  sync_info *si_out, *si_in;
  chan_info *cp_in, *cp_out;
  int i, err = MUS_NO_ERROR;
  tempfile = snd_tempnam(ss);
  err = save_selection(ss, tempfile, MUS_NEXT, MUS_OUT_FORMAT, SND_SRATE(cp->sound), NULL);
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
	  file_insert_samples(beg,
			      cp_selection_len(cp_in, NULL),
			      tempfile, cp_out, i,
			      (si_in->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
			      origin);
	  update_graph(cp_out, NULL);
	}
      free_sync_info(si_in);
      free_sync_info(si_out);
    }
  return(err);
}

void paste_selection_or_region(snd_state *ss, int reg, chan_info *cp, const char *origin)
{
  if (cp) 
    {
      if ((reg == 0) && (selection_is_active()))
	insert_selection(ss, cp, cp->cursor, origin);
      else paste_region(reg, cp, origin);
    }
}

void paste_selection_from_menu(snd_state *ss)
{
  paste_selection_or_region(ss, 0, selected_channel(ss), "Edit: Paste");
}


/* we're drawing the selection in one channel, but others may be sync'd to it */

void start_selection_creation(chan_info *cp, int samp)
{  
  int i;
  if ((selection_creation_chans) && (selection_creates_region(cp->state)))
    /* hmmm -- if keyboard selection in progress, then mouse press? */
    make_region_from_selection();
  deactivate_selection();
  selection_creation_chans = sync_to_chan(cp);
  for (i = 0; i < selection_creation_chans->chans; i++)
    reactivate_selection(selection_creation_chans->cps[i], samp, samp);
}

static void redraw_selection(void);

void update_possible_selection_in_progress(int samp)
{
  int i;
  if (selection_creation_chans)
    {
      for (i = 0; i < selection_creation_chans->chans; i++)
	update_selection(selection_creation_chans->cps[i], samp);
      redraw_selection();
    }
}

int selection_creation_in_progress(void) {return(selection_creation_chans != NULL);}

void finish_selection_creation(void)
{
  snd_state *ss;
  if (selection_creation_chans)
    {
      ss = get_global_state();
      if (selection_creates_region(ss)) 
	make_region_from_selection();
      reflect_edit_with_selection_in_menu();
      selection_creation_chans = free_sync_info(selection_creation_chans);      
    }
}

static int cp_redraw_selection(chan_info *cp, void *with_fft)
{
  int x0, x1;
  int beg, end;
  axis_info *ap;
  double sp_srate;
  snd_state *ss;
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
		       (unsigned int)(ap->y_axis_y0 - ap->y_axis_y1));
      fill_rectangle(selection_context(cp),
		     x0,
		     ap->y_axis_y1,
		     x1 - x0,
		     (unsigned int)(ap->y_axis_y0 - ap->y_axis_y1));
      cp->old_x0 = x0;
      cp->old_x1 = x1;
      cp->selection_visible = 1;
    }
  if ((with_fft) && (cp->ffting) && (cp_has_selection(cp, NULL)) && (!(chan_fft_in_progress(cp))))
    {
      ss = get_global_state();
      if (show_selection_transform(ss)) 
	calculate_fft(cp, NULL);
    }
  return(0);
}

static void redraw_selection(void)
{
  /* selection transform while synced redraw */
  map_over_chans(get_global_state(), cp_redraw_selection, (void *)1);
}

void display_selection(chan_info *cp)
{ 
  cp_redraw_selection(cp, NULL);
}

void make_region_from_selection(void)
{
  int *ends = NULL;
  int i, happy = 0;
  sync_info *si;
  if (!(selection_is_active())) return;
  si = selection_sync();
  ends = (int *)CALLOC(si->chans, sizeof(int));
  for (i = 0; i < si->chans; i++) 
    {
      ends[i] = selection_end(si->cps[i]);
      if (ends[i] > si->begs[i]) happy = 1;
      /* C-space followed by mouse click causes a bogus selection-creation event */
    }
  if (happy)
    {
      if (selection_len() > 10000000)
	report_in_minibuffer(si->cps[0]->sound, "making region...");
      define_region(si, ends);
      if (selection_len() > 10000000) 
	report_in_minibuffer(si->cps[0]->sound, " ");
    }
  si = free_sync_info(si);
  if (ends) FREE(ends);
}

void select_all(chan_info *cp)
{
  sync_info *si;
  int i;
  if (cp) 
    {
      deactivate_selection();
      si = sync_to_chan(cp);
      for (i = 0; i < si->chans; i++)
	{
	  reactivate_selection(si->cps[i], 0, current_ed_samples(si->cps[i]));
	  update_graph(si->cps[i], NULL);
	}
      si = free_sync_info(si);
      if (selection_creates_region(cp->state)) 
	make_region_from_selection();
    }
}


/* ---------------- selection mouse motion ---------------- */

static int last_selection_x = 0;
static BACKGROUND_FUNCTION_TYPE watch_selection_button = 0;

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

static BACKGROUND_TYPE WatchSelection(GUI_POINTER cp)
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
  watch_selection_button = BACKGROUND_ADD(cp->state, WatchSelection, cp);
}

void move_selection(chan_info *cp, int x)
{
  last_selection_x = x; /* called in snd-xchn -- sets last_selection_x */
  move_selection_1(cp, x);
}

int save_selection(snd_state *ss, char *ofile, int type, int format, int srate, char *comment)
{
  int ofd, oloc, comlen, err = MUS_NO_ERROR, reporting = 0, no_space, num, bps;
  sync_info *si;
  int *ends;
  int i, dur, j, k;
  snd_fd **sfs;
  snd_info *sp = NULL;
  MUS_SAMPLE_TYPE **data;

  if (MUS_DATA_FORMAT_OK(format))
    {
      if (MUS_HEADER_TYPE_OK(type))
	{
	  si = selection_sync();
	  if ((si) && (si->cps) && (si->cps[0])) sp = si->cps[0]->sound;
	  comlen = snd_strlen(comment);
	  dur = selection_len();

	  if ((snd_write_header(ss, ofile, type, srate, si->chans, 28, si->chans * dur, format, comment, comlen, NULL)) == -1) 
	    {
	      si = free_sync_info(si);
	      return(MUS_HEADER_WRITE_FAILED);
	    }
	  oloc = mus_header_data_location();
	  if ((ofd = snd_reopen_write(ss, ofile)) == -1) 
	    {
	      si = free_sync_info(si);
	      return(MUS_CANT_OPEN_TEMP_FILE);
	    }
	  if (sp)
	    {
	      bps = mus_data_format_to_bytes_per_sample(format);
	      num = dur * bps * si->chans;
	      no_space = disk_space_p(sp, ofd, num, 0);
	      if (no_space == GIVE_UP)
		{
		  /* has already interacted with user about disk problem */
		  close(ofd);
		  si = free_sync_info(si);
		  return(MUS_WRITE_ERROR);
		}
	    }
	  reporting = ((sp) && (dur > 1000000));
	  if (reporting) start_progress_report(sp, NOT_FROM_ENVED);
	  ends = (int *)CALLOC(si->chans, sizeof(int));
	  sfs = (snd_fd **)CALLOC(si->chans, sizeof(snd_fd *));
	  for (i = 0; i < si->chans; i++) 
	    {
	      ends[i] = selection_end(si->cps[i]);
	      sfs[i] = init_sample_read(selection_beg(si->cps[i]), si->cps[i], READ_FORWARD);
	    }
	  mus_file_set_descriptors(ofd, ofile, format, 
				   mus_data_format_to_bytes_per_sample(format), 
				   oloc, si->chans, type);
	  mus_file_set_data_clipped(ofd, data_clipped(ss));
	  mus_file_seek(ofd, oloc, SEEK_SET);
	  data = (MUS_SAMPLE_TYPE **)CALLOC(si->chans, sizeof(MUS_SAMPLE_TYPE *));
	  for (i = 0; i < si->chans; i++) 
	    data[i] = (MUS_SAMPLE_TYPE *)CALLOC(FILE_BUFFER_SIZE, sizeof(MUS_SAMPLE_TYPE)); 
	  j = 0;
	  for (i = 0; i < dur; i++)
	    {
	      for (k = 0; k < si->chans; k++)
		{
		  if (i <= ends[k]) 
		    data[k][j] = next_sample(sfs[k]);
		  else data[k][j] = MUS_SAMPLE_0;
		}
	      j++;
	      if (j == FILE_BUFFER_SIZE)
		{
		  err = mus_file_write(ofd, 0, j - 1, si->chans, data);
		  j = 0;
		  if (err == MUS_ERROR) break; /* error message already posted */
		  if (reporting) 
		    progress_report(sp, "save-selection", si->chans - 1, si->chans, (Float)i / (Float)dur, NOT_FROM_ENVED);
		  if (ss->stopped_explicitly)
		    {
		      ss->stopped_explicitly = 0;
		      snd_warning("save selection stopped");
		      break;
		    }
		}
	    }
	  if ((err == MUS_NO_ERROR) && (j > 0)) 
	    mus_file_write(ofd, 0, j - 1, si->chans, data);
	  if (reporting) finish_progress_report(sp, NOT_FROM_ENVED);
	  for (i = 0; i < si->chans; i++)
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
	      snd_error("can't close %d (%s): %s [%s[%d] %s]",
			ofd, ofile, strerror(errno), 
			__FILE__, __LINE__, __FUNCTION__);
	      return(MUS_CANT_CLOSE_FILE);
	    }
	  alert_new_file();
	  return(err);
	}
      else 
	{
	  snd_error("save-selection: unsupported header type? %d (%s) [%s[%d] %s]",
		    type, mus_header_type_name(type), 
		    __FILE__, __LINE__, __FUNCTION__);
	  return(MUS_UNSUPPORTED_HEADER_TYPE);
	}
    }
  else snd_error("save-selection: unsupported data format? %d (%s) [%s[%d] %s]",
		 format, mus_data_format_name(format),
		 __FILE__, __LINE__, __FUNCTION__);
  return(MUS_UNSUPPORTED_DATA_FORMAT);
}



#if HAVE_GUILE

static SCM g_cut(void)
{
  #define H_cut "(" S_cut ") cuts (deletes) the currently selected portion (same as 'delete-selection')"
  #define H_delete_selection "(" S_delete_selection ") deletes the currently selected portion (same as 'cut')"
  if (selection_is_active())
    {
      delete_selection(S_cut, UPDATE_DISPLAY);
      return(SCM_BOOL_T);
    }
  return(scm_throw(NO_ACTIVE_SELECTION,
		   SCM_LIST1(TO_SCM_STRING(S_cut))));
}

static SCM g_insert_selection(SCM beg, SCM snd, SCM chn)
{
  #define H_insert_selection "(" S_insert_selection ") inserts the currently selected portion start at beg"
  chan_info *cp;
  snd_state *ss;
  int err = MUS_NO_ERROR;
  if (selection_is_active())
    {
      ss = get_global_state();
      cp = get_cp(snd, chn, S_insert_selection);
      if (cp == NULL) 
	cp = selected_channel(ss);
      if (cp == NULL) 
	return(scm_throw(NO_SUCH_CHANNEL,
			 SCM_LIST3(TO_SCM_STRING(S_insert_selection),
				   snd, chn)));
      err = insert_selection(ss, cp, 
			     TO_C_INT_OR_ELSE(beg, 0), 
			     S_insert_selection);
      return(TO_SCM_BOOLEAN((err == MUS_NO_ERROR)));
    }
  return(scm_throw(NO_ACTIVE_SELECTION,
		   SCM_LIST1(TO_SCM_STRING(S_insert_selection))));
}

static SCM g_mix_selection(SCM beg, SCM snd, SCM chn)
{
  #define H_mix_selection "(" S_mix_selection ") mixes the currently selected portion start at beg"
  chan_info *cp;
  snd_state *ss;
  if (selection_is_active())
    {
      ss = get_global_state();
      cp = get_cp(snd, chn, S_mix_selection);
      if (cp == NULL) 
	cp = selected_channel(ss);
      if (cp == NULL) 
	return(scm_throw(NO_SUCH_CHANNEL,
			 SCM_LIST3(TO_SCM_STRING(S_mix_selection),
				   snd, chn)));
      return(TO_SCM_INT(mix_selection(ss, cp, 
				      TO_C_INT_OR_ELSE(beg, 0), 
				      S_mix_selection)));
    }
  return(scm_throw(NO_ACTIVE_SELECTION,
		   SCM_LIST1(TO_SCM_STRING(S_mix_selection))));
}

static SCM g_selectionQ(void)
{
  #define H_selectionQ "(" S_selectionQ ") -> #t if selection is currently active, visible, etc"
  return(TO_SCM_BOOLEAN(selection_is_active()));
}

static SCM g_selection_position(SCM snd, SCM chn)
{
  #define H_selection_position "(" S_selection_position " &optional snd chn) -> selection start samp"
  chan_info *cp;
  if (selection_is_active())
    {
      if (SCM_UNBNDP(snd))
	return(TO_SCM_INT(selection_beg(NULL)));
      else
	{
	  cp = get_cp(snd, chn, S_selection_position);
	  return(TO_SCM_INT(selection_beg(cp)));
	}
    }
  return(scm_throw(NO_ACTIVE_SELECTION,
		   SCM_LIST1(TO_SCM_STRING(S_selection_position))));
}

static SCM g_set_selection_position(SCM pos, SCM snd, SCM chn)
{
  chan_info *cp;
  sync_info *si = NULL;
  int i, beg;
  beg = TO_C_INT_OR_ELSE(pos, 0);
  if (SCM_UNBNDP(snd))
    {
      if (selection_is_active())
	si = selection_sync();
      else 
	{
	  cp = current_channel(get_global_state());
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
      cp = get_cp(snd, chn, "set-" S_selection_position);
      cp_set_selection_beg(cp, beg);
    }
  redraw_selection();
  return(pos);
}

WITH_REVERSED_CHANNEL_ARGS(g_set_selection_position_reversed, g_set_selection_position)

static SCM g_selection_length(SCM snd, SCM chn)
{
  #define H_selection_length "(" S_selection_length " &optional snd chn) -> selection length"
  chan_info *cp;
  if (selection_is_active())
    {
      if (SCM_UNBNDP(snd))
	return(TO_SCM_INT(selection_len()));
      else
	{
	  cp = get_cp(snd, chn, S_selection_length);
	  return(TO_SCM_INT(cp_selection_len(cp, NULL)));
	}
    }
  return(scm_throw(NO_ACTIVE_SELECTION,
		   SCM_LIST1(TO_SCM_STRING(S_selection_length))));
}

static SCM g_set_selection_length(SCM samps, SCM snd, SCM chn)
{
  chan_info *cp;
  sync_info *si = NULL;
  int i, len;
  len = TO_C_INT_OR_ELSE(samps, 0);
  if (SCM_UNBNDP(snd))
    {
      if (selection_is_active())
	si = selection_sync();
      else 
	{
	  cp = current_channel(get_global_state());
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
      cp = get_cp(snd, chn, "set-" S_selection_length);
      cp_set_selection_len(cp, len);
    }
  redraw_selection();
  return(samps);
}

WITH_REVERSED_CHANNEL_ARGS(g_set_selection_length_reversed, g_set_selection_length)

static SCM g_selection_member(SCM snd, SCM chn)
{
  #define H_selection_member "(" S_selection_member " &optional snd chn) -> #t if snd's channel chn is a member of the current selection"
  chan_info *cp;
  SND_ASSERT_CHAN(S_selection_member, snd, chn, 1);
  cp = get_cp(snd, chn, S_selection_member);
  return(TO_SCM_BOOLEAN(selection_is_active_in_channel(cp)));
}

static SCM g_set_selection_member(SCM on, SCM snd, SCM chn)
{
  chan_info *cp;
  SND_ASSERT_CHAN("set-" S_selection_member, snd, chn, 2);
  cp = get_cp(snd, chn, "set-" S_selection_member);
  if (TO_C_INT_OR_ELSE(on, 1))
    {
      if (selection_is_active())
	cp_set_selection_beg(cp, selection_beg(NULL));
      else cp_set_selection_beg(cp, 0);
    }
  else cp_deactivate_selection(cp, NULL);
  if (selection_is_active())
    {
      reflect_edit_with_selection_in_menu();
      redraw_selection();
    }
  else reflect_edit_without_selection_in_menu();
  return(on);
}

WITH_REVERSED_CHANNEL_ARGS(g_set_selection_member_reversed, g_set_selection_member)

static SCM g_select_all (SCM snd_n, SCM chn_n)
{
  #define H_select_all "(" S_select_all " &optional snd chn) makes a new selection containing all of snd's channel chn"
  chan_info *cp;
  SND_ASSERT_CHAN(S_select_all, snd_n, chn_n, 1);
  cp = get_cp(snd_n, chn_n, S_select_all);
  select_all(cp);
  return(TO_SCM_INT(region_id(0)));
}

static mus_error_handler_t *old_mus_error;

static void mus_local_error(int type, char *msg)
{
  mus_error_set_handler(old_mus_error);           /* make sure subsequent errors are handled by the default handler */
  scm_throw(CANNOT_SAVE,
	    SCM_LIST2(TO_SCM_STRING(S_save_sound_as),
		      TO_SCM_STRING(msg)));
}

static SCM g_save_selection(SCM filename, SCM header_type, SCM data_format, SCM srate, SCM comment)
{
  #define H_save_selection "(" S_save_selection " filename\n    &optional header-type data-format srate comment)\n\
saves the current selection in filename using the indicated file attributes"

  snd_state *ss;
  int type, format, sr, err;
  char *com = NULL, *fname = NULL;
  SCM_ASSERT(gh_string_p(filename), filename, SCM_ARG1, S_save_selection);
  if (selection_is_active() == 0)
    return(scm_throw(NO_ACTIVE_SELECTION,
		     SCM_LIST1(TO_SCM_STRING(S_save_selection))));
  ss = get_global_state();
  if (gh_number_p(header_type)) 
    type = TO_C_INT_OR_ELSE(header_type, 0); 
#if MUS_LITTLE_ENDIAN
  else type = MUS_RIFF;
#else
  else type = MUS_NEXT;
#endif
  format = TO_C_INT_OR_ELSE(data_format, MUS_OUT_FORMAT);
  sr = TO_C_INT_OR_ELSE(srate, region_srate(0));
  if (gh_string_p(comment)) 
    com = TO_NEW_C_STRING(comment); 
  else com = NULL;
  fname = mus_file_full_name(TO_C_STRING(filename));
  old_mus_error = mus_error_set_handler(mus_local_error);
  err = save_selection(ss, fname, type, format, sr, com);
  mus_error_set_handler(old_mus_error);
  if (fname) FREE(fname);
  if (com) free(com);
  if (err == MUS_NO_ERROR) return(filename);
  return(TO_SCM_INT(err));
}

void g_init_selection(SCM local_doc)
{
  define_procedure_with_reversed_setter(S_selection_position, SCM_FNC g_selection_position, H_selection_position,
					"set-" S_selection_position, SCM_FNC g_set_selection_position, SCM_FNC g_set_selection_position_reversed,
					local_doc, 0, 2, 1, 2);

  define_procedure_with_reversed_setter(S_selection_length, SCM_FNC g_selection_length, H_selection_length,
					"set-" S_selection_length, SCM_FNC g_set_selection_length, SCM_FNC g_set_selection_length_reversed,
					local_doc, 0, 2, 1, 2);

  define_procedure_with_reversed_setter(S_selection_member, SCM_FNC g_selection_member, H_selection_member,
					"set-" S_selection_member, SCM_FNC g_set_selection_member, SCM_FNC g_set_selection_member_reversed,
					local_doc, 0, 2, 1, 2);

  DEFINE_PROC(gh_new_procedure(S_selectionQ,       SCM_FNC g_selectionQ, 0, 0, 0),       H_selectionQ);
  DEFINE_PROC(gh_new_procedure(S_cut,              SCM_FNC g_cut, 0, 0, 0),              H_cut);
  DEFINE_PROC(gh_new_procedure(S_delete_selection, SCM_FNC g_cut, 0, 0, 0),              H_delete_selection);
  DEFINE_PROC(gh_new_procedure(S_insert_selection, SCM_FNC g_insert_selection, 0, 3, 0), H_insert_selection);
  DEFINE_PROC(gh_new_procedure(S_mix_selection,    SCM_FNC g_mix_selection, 0, 3, 0),    H_mix_selection);
  DEFINE_PROC(gh_new_procedure(S_select_all,       SCM_FNC g_select_all, 0, 2, 0),       H_select_all);
  DEFINE_PROC(gh_new_procedure(S_save_selection,   SCM_FNC g_save_selection, 1, 4, 0),   H_save_selection);
}

#endif

