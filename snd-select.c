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

static int selection_srate_1(chan_info *cp, void *ignored)
{
  if (cp_has_selection(cp, NULL)) 
    return(SND_SRATE(cp->sound));
  return(0);
}

int selection_srate(void)
{
  if (selection_is_active())
    return(map_over_chans(get_global_state(), selection_srate_1, NULL));
  return(0);
}

static int cp_delete_selection(chan_info *cp, void *origin)
{
  ed_list *ed;
  ed = cp->edits[cp->edit_ctr];
  if ((ed) && (ed->selection_beg != NO_SELECTION))
    {
      delete_samples(ed->selection_beg, cp_selection_len(cp, NULL), cp, (const char *)origin, cp->edit_ctr);
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
	  samps = current_ed_samples(cp);
	  if (newend < samps)
	    ed->selection_end = newend;
	  else ed->selection_end = samps - 1;
	}
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
  err = save_selection(ss, tempfile, MUS_NEXT, MUS_OUT_FORMAT, SND_SRATE(cp->sound), NULL, SAVE_ALL_CHANS);
  if (err == MUS_NO_ERROR)
    {
      si_out = sync_to_chan(cp);
      id = mix_file_and_delete(beg, selection_len(), tempfile, si_out->cps, si_out->chans, origin, with_mix_tags(ss));
      free_sync_info(si_out);	      
    }
  if (tempfile) FREE(tempfile);
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
  err = save_selection(ss, tempfile, MUS_NEXT, MUS_OUT_FORMAT, SND_SRATE(cp->sound), NULL, SAVE_ALL_CHANS);
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
			      origin, cp_out->edit_ctr);
	  update_graph(cp_out, NULL);
	}
      free_sync_info(si_in);
      free_sync_info(si_out);
    }
  if (tempfile) FREE(tempfile);
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
  Locus x0, x1;
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
		       (int)(ap->y_axis_y0 - ap->y_axis_y1));
      fill_rectangle(selection_context(cp),
		     x0,
		     ap->y_axis_y1,
		     x1 - x0,
		     (int)(ap->y_axis_y0 - ap->y_axis_y1));
      cp->old_x0 = x0;
      cp->old_x1 = x1;
      cp->selection_visible = 1;
    }
  if ((with_fft) && (cp->graph_transform_p) && (cp_has_selection(cp, NULL)) && (!(chan_fft_in_progress(cp))))
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

int make_region_from_selection(void)
{
  int *ends = NULL;
  int i, happy = 0, id = -1;
  sync_info *si;
  if (!(selection_is_active())) return(-1);
  si = selection_sync();
  ends = (int *)CALLOC(si->chans, sizeof(int));
  for (i = 0; i < si->chans; i++) 
    {
      ends[i] = selection_end(si->cps[i]);
      if (ends[i] > si->begs[i]) happy = 1;
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
	  reactivate_selection(si->cps[i], 0, current_ed_samples(si->cps[i]));
	  update_graph(si->cps[i], NULL);
	}
      si = free_sync_info(si);
      if (selection_creates_region(cp->state)) 
	return(make_region_from_selection());
    }
  return(-1);
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

int save_selection(snd_state *ss, char *ofile, int type, int format, int srate, char *comment, int chan)
{
  int ofd, oloc, comlen, err = MUS_NO_ERROR, reporting = 0, no_space, num, bps;
  sync_info *si;
  int *ends;
  int i, dur, j, k, chans;
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
	  if (chan == SAVE_ALL_CHANS)
	    chans = si->chans;
	  else chans = 1;

	  if ((snd_write_header(ss, ofile, type, srate, chans, 28, chans * dur, format, comment, comlen, NULL)) == -1) 
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
	      num = dur * bps * chans;
	      no_space = disk_space_p(sp, num, 0, ofile);
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
	  ends = (int *)CALLOC(chans, sizeof(int));
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
				    mus_data_format_to_bytes_per_sample(format), 
				    oloc, chans, type);
	  mus_file_set_data_clipped(ofd, data_clipped(ss));
	  lseek(ofd, oloc, SEEK_SET);
	  data = (MUS_SAMPLE_TYPE **)CALLOC(chans, sizeof(MUS_SAMPLE_TYPE *));
	  for (i = 0; i < chans; i++) 
	    data[i] = (MUS_SAMPLE_TYPE *)CALLOC(FILE_BUFFER_SIZE, sizeof(MUS_SAMPLE_TYPE)); 
	  j = 0;
	  for (i = 0; i < dur; i++)
	    {
	      for (k = 0; k < chans; k++)
		{
		  if (i <= ends[k]) 
		    data[k][j] = next_sample(sfs[k]);
		  else data[k][j] = MUS_SAMPLE_0;
		}
	      j++;
	      if (j == FILE_BUFFER_SIZE)
		{
		  err = mus_file_write(ofd, 0, j - 1, chans, data);
		  j = 0;
		  if (err == MUS_ERROR) break; /* error message already posted */
		  if (reporting) 
		    progress_report(sp, "save-selection", chans - 1, chans, (Float)i / (Float)dur, NOT_FROM_ENVED);
		  if (ss->stopped_explicitly)
		    {
		      ss->stopped_explicitly = 0;
		      snd_warning("save selection stopped");
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



static XEN g_delete_selection(void)
{
  #define H_delete_selection "(" S_delete_selection ") deletes the currently selected portion"
  if (selection_is_active())
    {
      delete_selection(S_delete_selection, UPDATE_DISPLAY);
      return(XEN_TRUE);
    }
  snd_no_active_selection_error(S_delete_selection);
  return(XEN_FALSE);
}

static XEN g_insert_selection(XEN beg, XEN snd, XEN chn)
{
  #define H_insert_selection "(" S_insert_selection ") inserts the currently selected portion start at beg"
  chan_info *cp;
  snd_state *ss;
  int err = MUS_NO_ERROR;
  if (selection_is_active())
    {
      ASSERT_CHANNEL(S_insert_selection, snd, chn, 2);
      XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(beg), beg, XEN_ARG_1, S_insert_selection, "a number");
      ss = get_global_state();
      cp = get_cp(snd, chn, S_insert_selection);
      if (cp == NULL) 
	cp = selected_channel(ss);
      if (cp == NULL) 
	return(snd_no_such_channel_error(S_insert_selection, snd, chn));
      err = insert_selection(ss, cp, 
			     XEN_TO_C_INT_OR_ELSE(beg, 0), 
			     S_insert_selection);
      return(C_TO_XEN_BOOLEAN((err == MUS_NO_ERROR)));
    }
  snd_no_active_selection_error(S_insert_selection);
  return(beg);
}

static XEN g_mix_selection(XEN beg, XEN snd, XEN chn)
{
  #define H_mix_selection "(" S_mix_selection ") mixes the currently selected portion start at beg"
  chan_info *cp;
  snd_state *ss;
  if (selection_is_active())
    {
      ASSERT_CHANNEL(S_mix_selection, snd, chn, 2);
      XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(beg), beg, XEN_ARG_1, S_mix_selection, "a number");
      ss = get_global_state();
      cp = get_cp(snd, chn, S_mix_selection);
      if (cp == NULL) 
	cp = selected_channel(ss);
      if (cp == NULL) 
	return(snd_no_such_channel_error(S_mix_selection, snd, chn));
      return(C_TO_XEN_INT(mix_selection(ss, cp, 
					XEN_TO_C_INT_OR_ELSE(beg, 0), 
					S_mix_selection)));
    }
  snd_no_active_selection_error(S_mix_selection);
  return(beg);
}

static XEN g_selection_p(void)
{
  #define H_selection_p "(" S_selection_p ") -> #t if selection is currently active, visible, etc"
  return(C_TO_XEN_BOOLEAN(selection_is_active()));
}

static XEN g_selection_position(XEN snd, XEN chn)
{
  #define H_selection_position "(" S_selection_position " &optional snd chn) -> selection start samp"
  chan_info *cp;
  if (selection_is_active())
    {
      if (XEN_NOT_BOUND_P(snd))
	return(C_TO_XEN_INT(selection_beg(NULL)));
      else
	{
	  ASSERT_CHANNEL(S_selection_position, snd, chn, 1);
	  cp = get_cp(snd, chn, S_selection_position);
	  return(C_TO_XEN_INT(selection_beg(cp)));
	}
    }
  snd_no_active_selection_error(S_selection_position);
  return(snd);
}

static XEN g_set_selection_position(XEN pos, XEN snd, XEN chn)
{
  chan_info *cp;
  sync_info *si = NULL;
  int i, beg;
  ASSERT_CHANNEL("set-" S_selection_position, snd, chn, 2);
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(pos), pos, XEN_ARG_1, S_selection_position, "a number");
  beg = XEN_TO_C_INT_OR_ELSE(pos, 0);
  if (XEN_NOT_BOUND_P(snd))
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

static XEN g_selection_length(XEN snd, XEN chn)
{
  #define H_selection_length "(" S_selection_length " &optional snd chn) -> selection length"
  chan_info *cp;
  if (selection_is_active())
    {
      if (XEN_NOT_BOUND_P(snd))
	return(C_TO_XEN_INT(selection_len()));
      else
	{
	  ASSERT_CHANNEL(S_selection_length, snd, chn, 1);
	  cp = get_cp(snd, chn, S_selection_length);
	  return(C_TO_XEN_INT(cp_selection_len(cp, NULL)));
	}
    }
  snd_no_active_selection_error(S_selection_length);
  return(snd);
}

static XEN g_set_selection_length(XEN samps, XEN snd, XEN chn)
{
  chan_info *cp;
  sync_info *si = NULL;
  int i, len;
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samps), samps, XEN_ARG_1, "set-" S_selection_length, "a number");
  len = XEN_TO_C_INT_OR_ELSE(samps, 0);
  if (XEN_NOT_BOUND_P(snd))
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
      ASSERT_CHANNEL("set-" S_selection_length, snd, chn, 2);
      cp = get_cp(snd, chn, "set-" S_selection_length);
      cp_set_selection_len(cp, len);
    }
  redraw_selection();
  return(samps);
}

WITH_REVERSED_CHANNEL_ARGS(g_set_selection_length_reversed, g_set_selection_length)

static XEN g_selection_member(XEN snd, XEN chn)
{
  #define H_selection_member "(" S_selection_member " &optional snd chn) -> #t if snd's channel chn is a member of the current selection"
  chan_info *cp;
  ASSERT_CHANNEL(S_selection_member, snd, chn, 1);
  cp = get_cp(snd, chn, S_selection_member);
  return(C_TO_XEN_BOOLEAN(selection_is_active_in_channel(cp)));
}

static XEN g_set_selection_member(XEN on, XEN snd, XEN chn)
{
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(on), on, XEN_ARG_1, "set-" S_selection_member, "a boolean");
  if ((XEN_TRUE_P(snd)) && (XEN_FALSE_P(on)))
    deactivate_selection();
  else
    {
      ASSERT_CHANNEL("set-" S_selection_member, snd, chn, 2);
      cp = get_cp(snd, chn, "set-" S_selection_member);
      if ((XEN_NOT_BOUND_P(on)) || (XEN_TRUE_P(on)))
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
    }
  return(on);
}

WITH_REVERSED_CHANNEL_ARGS(g_set_selection_member_reversed, g_set_selection_member)

static XEN g_select_all (XEN snd_n, XEN chn_n)
{
  #define H_select_all "(" S_select_all " &optional snd chn) makes a new selection containing all of snd's channel chn"
  chan_info *cp;
  int id;
  ASSERT_CHANNEL(S_select_all, snd_n, chn_n, 1);
  cp = get_cp(snd_n, chn_n, S_select_all);
  id = select_all(cp);
  if (selection_creates_region(cp->state)) 
    return(C_TO_XEN_INT(id));
  else return(XEN_TRUE);
}

static XEN g_save_selection(XEN filename, XEN header_type, XEN data_format, XEN srate, XEN comment, XEN chan)
{
  #define H_save_selection "(" S_save_selection " filename\n    &optional header-type data-format srate comment chan)\n\
saves the current selection in filename using the indicated file attributes.  If chan is given, save only that channel."

  snd_state *ss;
  int type, format, sr, err, chn;
  char *com = NULL, *fname = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ARG_1, S_save_selection, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(header_type), header_type, XEN_ARG_2, S_save_selection, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(data_format), data_format, XEN_ARG_3, S_save_selection, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_OR_BOOLEAN_IF_BOUND_P(srate), srate, XEN_ARG_4, S_save_selection, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(chan), chan, XEN_ARG_6, S_save_selection, "an integer");
  if (selection_is_active() == 0)
    return(snd_no_active_selection_error(S_save_selection));
  ss = get_global_state();
  if (XEN_INTEGER_P(header_type)) 
    type = XEN_TO_C_INT(header_type); 
#if MUS_LITTLE_ENDIAN
  else type = MUS_RIFF;
#else
  else type = MUS_NEXT;
#endif
  format = XEN_TO_C_INT_OR_ELSE(data_format, MUS_OUT_FORMAT);
  sr = XEN_TO_C_INT_OR_ELSE(srate, selection_srate());
  if (XEN_STRING_P(comment)) 
    com = XEN_TO_C_STRING(comment); 
  else com = NULL;
  chn = XEN_TO_C_INT_OR_ELSE(chan, SAVE_ALL_CHANS);
  fname = mus_expand_filename(XEN_TO_C_STRING(filename));
  ss->catch_message = NULL;
  err = save_selection(ss, fname, type, format, sr, com, chn);
  if (fname) FREE(fname);
  if (err == MUS_NO_ERROR) 
    return(filename);
  else XEN_ERROR(CANNOT_SAVE,
		 XEN_LIST_2(C_TO_XEN_STRING(S_save_selection),
			    C_TO_XEN_STRING(ss->catch_message)));
  return(C_TO_XEN_INT(err));
}

static XEN g_selection_chans(void)
{
  #define H_selection_chans "(" S_selection_chans ") -> chans in active selection"
  return(C_TO_XEN_INT(selection_chans()));
}

static XEN g_selection_srate(void)
{
  #define H_selection_srate "(" S_selection_srate ") -> selection srate"
  return(C_TO_XEN_INT(selection_srate()));
}

#ifdef XEN_ARGIFY_1
XEN_ARGIFY_2(g_selection_position_w, g_selection_position)
XEN_ARGIFY_3(g_set_selection_position_w, g_set_selection_position)
XEN_ARGIFY_2(g_selection_length_w, g_selection_length)
XEN_ARGIFY_3(g_set_selection_length_w, g_set_selection_length)
XEN_ARGIFY_2(g_selection_member_w, g_selection_member)
XEN_ARGIFY_3(g_set_selection_member_w, g_set_selection_member)
XEN_NARGIFY_0(g_selection_p_w, g_selection_p)
XEN_NARGIFY_0(g_selection_chans_w, g_selection_chans)
XEN_NARGIFY_0(g_selection_srate_w, g_selection_srate)
XEN_NARGIFY_0(g_delete_selection_w, g_delete_selection)
XEN_ARGIFY_3(g_insert_selection_w, g_insert_selection)
XEN_ARGIFY_3(g_mix_selection_w, g_mix_selection)
XEN_ARGIFY_2(g_select_all_w, g_select_all)
XEN_ARGIFY_6(g_save_selection_w, g_save_selection)
#else
#define g_selection_position_w g_selection_position
#define g_set_selection_position_w g_set_selection_position
#define g_selection_length_w g_selection_length
#define g_set_selection_length_w g_set_selection_length
#define g_selection_member_w g_selection_member
#define g_set_selection_member_w g_set_selection_member
#define g_selection_p_w g_selection_p
#define g_selection_chans_w g_selection_chans
#define g_selection_srate_w g_selection_srate
#define g_delete_selection_w g_delete_selection
#define g_insert_selection_w g_insert_selection
#define g_mix_selection_w g_mix_selection
#define g_select_all_w g_select_all
#define g_save_selection_w g_save_selection
#endif

void g_init_selection(void)
{
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_selection_position, g_selection_position_w, H_selection_position,
					    "set-" S_selection_position, g_set_selection_position_w, g_set_selection_position_reversed,
					    0, 2, 1, 2);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_selection_length, g_selection_length_w, H_selection_length,
					    "set-" S_selection_length, g_set_selection_length_w, g_set_selection_length_reversed,
					    0, 2, 1, 2);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_selection_member, g_selection_member_w, H_selection_member,
					    "set-" S_selection_member, g_set_selection_member_w, g_set_selection_member_reversed,
					    0, 2, 1, 2);

  XEN_DEFINE_PROCEDURE(S_selection_p,      g_selection_p_w, 0, 0, 0,      H_selection_p);
  XEN_DEFINE_PROCEDURE(S_selection_chans,  g_selection_chans_w, 0, 0, 0,  H_selection_chans);
  XEN_DEFINE_PROCEDURE(S_selection_srate,  g_selection_srate_w, 0, 0, 0,  H_selection_srate);
  XEN_DEFINE_PROCEDURE(S_delete_selection, g_delete_selection_w, 0, 0, 0, H_delete_selection);
  XEN_DEFINE_PROCEDURE(S_insert_selection, g_insert_selection_w, 0, 3, 0, H_insert_selection);
  XEN_DEFINE_PROCEDURE(S_mix_selection,    g_mix_selection_w, 0, 3, 0,    H_mix_selection);
  XEN_DEFINE_PROCEDURE(S_select_all,       g_select_all_w, 0, 2, 0,       H_select_all);
  XEN_DEFINE_PROCEDURE(S_save_selection,   g_save_selection_w, 1, 5, 0,   H_save_selection);
}
