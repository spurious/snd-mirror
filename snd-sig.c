#include "snd.h"

/* collect syncd chans */
typedef struct {
  sync_info *si;
  snd_fd **sfs;
  int dur;
} sync_state;

static void free_sync_state(sync_state *sc)
{
  if (sc)
    {
      if (sc->si) 
	sc->si = free_sync_info(sc->si);
      if (sc->sfs) 
	FREE(sc->sfs);
      FREE(sc);
    }
}

int to_c_edit_position(chan_info *cp, XEN edpos, const char *caller, int arg_pos)
{
  int pos;
  XEN errstr;
  char *errmsg = NULL;
  /* need to allow #f here for optargs */
  XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(edpos) || XEN_INTEGER_P(edpos) || XEN_PROCEDURE_P(edpos) || XEN_BOOLEAN_P(edpos), edpos, arg_pos, caller, "an integer or a procedure");
  if (XEN_PROCEDURE_P(edpos))
    {
      errmsg = procedure_ok(edpos, 2, caller, "edit position", arg_pos);
      if (errmsg)
	{
	  errstr = C_TO_XEN_STRING(errmsg);
	  FREE(errmsg);
	  snd_bad_arity_error(caller, errstr, edpos);
	  return(0); /* never called, presumably */
	}
      pos = XEN_TO_C_INT_OR_ELSE_WITH_CALLER(XEN_CALL_2(edpos, 
							C_TO_SMALL_XEN_INT(cp->sound->index), 
							C_TO_SMALL_XEN_INT(cp->chan),
							caller),
					     AT_CURRENT_EDIT_POSITION, caller);
    }
  else pos = XEN_TO_C_INT_OR_ELSE_WITH_CALLER(edpos, AT_CURRENT_EDIT_POSITION, caller);
  if ((pos == AT_CURRENT_EDIT_POSITION) || (pos < 0) || (pos > cp->edit_size))
    return(cp->edit_ctr);
  return(pos);
}

int to_c_edit_samples(chan_info *cp, XEN edpos, const char *caller, int arg_pos)
{
  return(cp->samples[to_c_edit_position(cp, edpos, caller, arg_pos)]);
}

static sync_state *get_sync_state_1(snd_state *ss, snd_info *sp, chan_info *cp, int beg, int regexpr, 
				    int forwards, int prebeg, XEN edpos, const char *caller, int arg_pos)
{
  /* can return NULL if regexpr and no current selection */
  sync_info *si = NULL;
  snd_fd **sfs = NULL;
  chan_info *ncp;
  int dur, i, pbeg, pos;
  sync_state *sc;
  dur = 0;
  if ((!regexpr) && (sp == NULL)) return(NULL);
  if ((!regexpr) && (sp->sync != 0))
    {
      si = snd_sync(ss, sp->sync);
      sfs = (snd_fd **)MALLOC(si->chans * sizeof(snd_fd *));
      for (i = 0; i < si->chans; i++) 
	{
	  ncp = si->cps[i];
	  si->begs[i] = beg;
	  pos = to_c_edit_position(ncp, edpos, caller, arg_pos);
	  if (forwards == READ_FORWARD)
	    sfs[i] = init_sample_read_any(beg, ncp, READ_FORWARD, pos);
	  else sfs[i] = init_sample_read_any(cp->samples[pos] - 1, ncp, READ_BACKWARD, pos);
	}
    }
  else
    {
      if (regexpr)
	{
	  if (selection_is_active())
	    {
	      si = selection_sync();
	      dur = selection_len();
	      sfs = (snd_fd **)MALLOC(si->chans * sizeof(snd_fd *));
	      for (i = 0; i < si->chans; i++) 
		{
		  ncp = si->cps[i];
		  pos = to_c_edit_position(ncp, edpos, caller, arg_pos);
		  if (forwards == READ_FORWARD)
		    {
		      pbeg = si->begs[i] - prebeg;
		      if (pbeg < 0) pbeg = 0;
		      sfs[i] = init_sample_read_any(pbeg, ncp, READ_FORWARD, pos);
		    }
		  else sfs[i] = init_sample_read_any(si->begs[i] + dur - 1, ncp, READ_BACKWARD, pos);
		}
	    }
	  else 
	    {
	      snd_warning("no current selection");
	      return(NULL);
	    }
	}
    }
  if (si == NULL) 
    {
      si = make_simple_sync(cp, beg);
      sfs = (snd_fd **)MALLOC(sizeof(snd_fd *));
      pos = to_c_edit_position(cp, edpos, caller, arg_pos);
      if (forwards == READ_FORWARD)
	sfs[0] = init_sample_read_any(beg, cp, READ_FORWARD, pos);
      else sfs[0] = init_sample_read_any(cp->samples[pos] - 1, cp, READ_BACKWARD, pos);
    }
  sc = (sync_state *)CALLOC(1, sizeof(sync_state));
  sc->dur = dur;
  sc->sfs = sfs;
  sc->si = si;
  return(sc);
}

static sync_state *get_sync_state(snd_state *ss, snd_info *sp, chan_info *cp, int beg, int regexpr, int forwards, XEN edpos, const char *caller, int arg_pos)
{
  return(get_sync_state_1(ss, sp, cp, beg, regexpr, forwards, 0, edpos, caller, arg_pos));
}

static sync_state *get_sync_state_without_snd_fds(snd_state *ss, snd_info *sp, chan_info *cp, int beg, int regexpr)
{
  sync_info *si = NULL;
  int dur, i;
  sync_state *sc;
  dur = 0;
  if ((sp->sync != 0) && (!regexpr))
    {
      si = snd_sync(ss, sp->sync);
      for (i = 0; i < si->chans; i++) 
	si->begs[i] = beg;
    }
  else
    {
      if (regexpr)
	{
	  if (selection_is_active())
	    {
	      si = selection_sync();
	      dur = selection_len();
	    }
	  else
	    {
	      snd_warning("no current selection");
	      return(NULL);
	    }
	}
    }
  if (si == NULL) 
    si = make_simple_sync(cp, beg);
  sc = (sync_state *)CALLOC(1, sizeof(sync_state));
  sc->dur = dur;
  sc->si = si;
  sc->sfs = NULL;
  return(sc);
}

static char *convolve_with_or_error(char *filename, Float amp, chan_info *cp, XEN edpos, int arg_pos)
{
  /* if string returned, needs to be freed */
  /* amp == 0.0 means unnormalized, cp == NULL means current selection */
  sync_state *sc;
  sync_info *si;
  snd_state *ss;
  snd_info *sp = NULL, *gsp = NULL;
  int ip, stop_point = 0, err, impulse_chan = 0, filter_chans, ok = 0;
  file_info *hdr;
  int scfd, fltfd, fftsize, ipow, filtersize = 0, filesize = 0, dataloc, dataformat;
  char *ofile = NULL, *saved_chan_file;
  chan_info *ncp, *ucp;
  char *origin;

  if (cp) 
    {
      ss = cp->state; 
      sp = cp->sound; 
      ncp = cp;
    }
  else
    {
      ss = get_global_state();
      sp = any_selected_sound(ss);
      ncp = any_selected_channel(sp);
    }
  sc = get_sync_state_without_snd_fds(ss, sp, ncp, 0, (cp == NULL));
  if (sc == NULL) return(NULL);
  si = sc->si;

  origin = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  mus_snprintf(origin, PRINT_BUFFER_SIZE,
	  "%s %s %.3f", 
	  (cp == NULL) ? S_convolve_selection_with : S_convolve_with, 
	  filename, amp);
  filter_chans = mus_sound_chans(filename);
  filtersize = mus_sound_samples(filename) / filter_chans;
  /* if impulse response is stereo, we need to use both its channels */
  dataloc = mus_sound_data_location(filename);
  dataformat = mus_sound_data_format(filename);
  if (!(ss->stopped_explicitly))
    {
      for (ip = 0; ip < si->chans; ip++)
	{
	  ok = 0;
      	  ucp = si->cps[ip];
	  sp = ucp->sound;
	  if ((ip > 0) && (sp != gsp)) 
	    finish_progress_report(gsp, NOT_FROM_ENVED);
	  if ((ip == 0) || (sp != gsp)) 
	    {
	      gsp = ucp->sound; 
	      start_progress_report(gsp, NOT_FROM_ENVED);
	    }

	  /* ofile here = new convolved data */
	  ofile = snd_tempnam(ss);

	  saved_chan_file = snd_tempnam(ss);
	  err = save_channel_edits(ucp, saved_chan_file, edpos, S_convolve_with, arg_pos);
	  if (err != MUS_NO_ERROR)
	    {
	      FREE(ofile);
	      return(mus_format("convolve: save chan (%s[%d]) in %s: %s\n",
				sp->short_filename, ucp->chan, 
				saved_chan_file, strerror(errno)));
	    }
	  else
	    {
	      scfd = mus_file_open_read(saved_chan_file);
	      if (scfd == -1) 
		return(mus_format("convolve: open saved chan (%s[%d]) file %s: %s\n",
				  sp->short_filename, ucp->chan, 
				  saved_chan_file, strerror(errno)));
	      else
		{
		  hdr = sp->hdr;
		  mus_file_open_descriptors(scfd,
					    saved_chan_file,
					    hdr->format,
					    mus_data_format_to_bytes_per_sample(hdr->format),
					    hdr->data_location,
					    1, hdr->type); /* ??? */
		  fltfd = mus_file_open_read(filename);
		  if (fltfd == -1) 
		    return(mus_format("convolve: open filter file %s: %s\n", 
				      filename, strerror(errno)));
		  else
		    {
		      mus_file_open_descriptors(fltfd,
						filename,
						dataformat,
						mus_data_format_to_bytes_per_sample(dataformat),
						dataloc,
						filter_chans,
						mus_sound_header_type(filename));
		      if (cp == NULL)
			filesize = selection_len();
		      else filesize = to_c_edit_samples(ucp, edpos, S_convolve_with, arg_pos);
		      if (filesize > 0)
			{
			  ipow = (int)(ceil(log(filtersize + filesize) / log(2.0))) + 1;
			  fftsize = (int)(pow(2.0, ipow));
			  ok = 1;
			  c_convolve(ofile, amp, scfd,
				     mus_sound_data_location(saved_chan_file),
				     fltfd, dataloc, filtersize, fftsize, filter_chans, impulse_chan,
				     filtersize + filesize + 1,
				     gsp, NOT_FROM_ENVED, ip, si->chans);
			  impulse_chan++;
			  if (impulse_chan >= filter_chans) 
			    impulse_chan = 0;
			}
		      if (mus_file_close(fltfd) != 0)
			return(mus_format("convolve: close filter file %s: %s\n", 
					  filename, strerror(errno)));
		    }
		}
	      if (mus_file_close(scfd) != 0)
		return(mus_format("convolve: close saved chan (%s[%d]) file %s: %s\n",
				  sp->short_filename, ucp->chan, 
				  saved_chan_file, strerror(errno)));
	    }
	  snd_remove(saved_chan_file);
	  FREE(saved_chan_file);

	  if (ok)
	    {
	      if (cp == NULL)
		{
		  ok = delete_selection(origin, DONT_UPDATE_DISPLAY);
		  file_insert_samples(si->begs[ip], filtersize + filesize, ofile, ucp, 0, DELETE_ME, origin);
		  reactivate_selection(si->cps[ip], 
				       si->begs[ip], 
				       si->begs[ip] + filtersize + filesize);
		  if (ok) 
		    backup_edit_list(ucp); 
		  if (ucp->marks) 
		    ripple_trailing_marks(ucp, si->begs[ip], sc->dur, filtersize + filesize);
		  update_graph(ucp, NULL); 
		}
	      else file_override_samples(filtersize + filesize, ofile, ucp, 0, DELETE_ME, LOCK_MIXES, origin);
	    }
	  if (ofile) FREE(ofile);
	  check_for_event(ss);
	  if (ss->stopped_explicitly) 
	    {
	      stop_point = ip;
	      break;
	    }
	}
    }
  if (origin) 
    {
      FREE(origin); 
      origin = NULL;
    }
  if (gsp) finish_progress_report(gsp, NOT_FROM_ENVED);
  if (ss->stopped_explicitly)
    {
      /* clean up and undo all edits up to stop_point */
      ss->stopped_explicitly = 0;
      for (ip = 0; ip <= stop_point; ip++)
	{
	  ucp = si->cps[ip];
	  undo_edit(ucp, 1);
	}
    }
  free_sync_state(sc);
  return(NULL);
}

/* amplitude scaling */

void scale_by(chan_info *cp, Float *ur_scalers, int len, int selection)
{
  /* if selection, sync to current selection, else sync to current sound */
  /* 3-Oct-00: the scale factors are now embedded in the edit fragments,
   *   and folded into the MUS_SAMPLE_TO_FLOAT calculation if possible,
   *   so scaling comes at no extra cost, happens instantly, and requires no disk space,
   *   no matter how big the underlying sound.  This also affects scale_to.
   */
  sync_info *si;
  chan_info *ncp;
  int i, j, beg, frames;
  if (selection)
    si = selection_sync();
  else si = sync_to_chan(cp);
  for (i = 0, j = 0; i < si->chans; i++)
    {
      ncp = si->cps[i];
      if (selection)
	{
	  beg = selection_beg(ncp);
	  frames = selection_end(ncp) - beg + 1;
	}
      else
	{
	  beg = 0;
	  frames = current_ed_samples(ncp);
	}
      scale_channel(ncp, ur_scalers[j], beg, frames);
      j++;
      if (j >= len) j = 0;
    }
  free_sync_info(si);
}

Float get_maxamp(snd_info *sp, chan_info *cp, int edpos)
{
  snd_fd *sf;
  Float ymax, val;
  int i, len, pos;
  if (!sp) return(0.0);
  if (!cp) cp = sp->chans[0];
  if (edpos == AT_CURRENT_EDIT_POSITION) pos = cp->edit_ctr; else pos = edpos;
  if (amp_env_maxamp_ok(cp, pos)) 
    return(amp_env_maxamp(cp, pos));
  val = ed_maxamp(cp, pos);
  if (val >= 0.0) return(val);
  sf = init_sample_read_any(0, cp, READ_FORWARD, pos);
  if (sf == NULL) return(0.0);
  ymax = 0.0;
  len = cp->samples[pos];
  for (i = 0; i < len; i++)
    {
      val = next_sample_to_float(sf);
      if (val < 0.0) val = -val;
      if (val > ymax) ymax = val;
    }
  free_snd_fd(sf);
  set_ed_maxamp(cp, pos, ymax);
  return(ymax);
}

Float get_selection_maxamp(chan_info *cp)
{
  snd_fd *sf;
  Float ymax, val;
  int i, len;
  val = ed_selection_maxamp(cp);
  if (val >= 0.0) return(val);
  sf = init_sample_read(selection_beg(cp), cp, READ_FORWARD);
  if (sf == NULL) return(0.0);
  len = selection_end(cp) - selection_beg(cp) + 1;
  ymax = 0.0;
  for (i = 0; i < len; i++)
    {
      val = next_sample_to_float(sf);
      if (val < 0.0) val = -val;
      if (val > ymax) ymax = val;
    }
  free_snd_fd(sf);
  set_ed_selection_maxamp(cp, ymax);
  return(ymax);
}

void scale_to(snd_state *ss, snd_info *sp, chan_info *cp, Float *ur_scalers, int len, int selection)
{
  /* essentially the same as scale-by, but first take into account current maxamps */
  /* here it matters if more than one arg is given -- if one, get overall maxamp */
  /*   if more than one, get successive maxamps */
  int i, chans, nlen, beg, frames, datum_size;
  sync_info *si = NULL;
  chan_info *ncp;
  Float maxamp, val;
  Float *scalers;
  if ((!selection) && (cp == NULL)) return;
  if (selection) 
    {
      if (!(selection_is_active())) return;
      si = selection_sync();
      sp = si->cps[0]->sound;
    }
  else si = sync_to_chan(cp);
  datum_size = mus_data_format_to_bytes_per_sample((sp->hdr)->format);
  chans = si->chans;
  scalers = (Float *)CALLOC(chans, sizeof(Float));
  if (chans < len)
    nlen = chans; else nlen = len;
  for (i = 0; i < nlen; i++) 
    scalers[i] = ur_scalers[i];
  if (chans > len)
    for (i = len; i < chans; i++) 
      scalers[i] = ur_scalers[len - 1];
  /* now find maxamps (special if len==1) and fixup the scalers */
  if (len == 1)
    {
      if (scalers[0] != 0.0)
	{
	  maxamp = 0.0;
	  for (i = 0; i < chans; i++)
	    {
	      ncp = si->cps[i];
	      if (selection)
		val = get_selection_maxamp(ncp);
	      else val = get_maxamp(ncp->sound, ncp, AT_CURRENT_EDIT_POSITION);
	      if (val > maxamp) maxamp = val;
	    }
	  if ((data_clipped(ss) == 0) && 
	      (scalers[0] == 1.0) && 
	      (datum_size < 4)) 
	    scalers[0] = 32767.0 / 32768.0;
	  /* 1.0 = -1.0 in these cases, so we'll get a click  -- added 13-Dec-99 */
	  if (maxamp != 0.0)
	    val = scalers[0] / maxamp;
	  else val = 0.0;
	}
      else val = 0.0;
      for (i = 0; i < chans; i++) 
	scalers[i] = val;
    }
  else
    {
      for (i = 0; i < chans; i++)
	{
	  ncp = si->cps[i];
	  if (scalers[i] != 0.0)
	    {
	      if (selection)
		val = get_selection_maxamp(ncp);
	      else val = get_maxamp(ncp->sound, ncp, AT_CURRENT_EDIT_POSITION);
	      if (val != 0.0)
		{
		  if ((data_clipped(ss) == 0) && 
		      (scalers[i] == 1.0) && 
		      (datum_size < 4)) 
		    scalers[i] = 32767.0 / 32768.0;
		  scalers[i] /= val;
		}
	      else scalers[i] = 0.0;
	    }
	}
    }
  for (i = 0; i < si->chans; i++)
    {
      ncp = si->cps[i];
      if (selection)
	{
	  beg = selection_beg(ncp);
	  frames = selection_end(ncp) - beg + 1;
	}
      else
	{
	  beg = 0;
	  frames = current_ed_samples(ncp);
	}
      scale_channel(ncp, scalers[i], beg, frames);
    }
  FREE(scalers);
  free_sync_info(si);
}

static void swap_channels(snd_state *ss, int beg, int dur, snd_fd *c0, snd_fd *c1)
{
  chan_info *cp0, *cp1;
  snd_info *sp0;
  file_info *hdr0 = NULL, *hdr1 = NULL;
  int j, k, ofd0 = 0, ofd1 = 0, datumb = 0, temp_file, err = 0;
  MUS_SAMPLE_TYPE **data0, **data1;
  MUS_SAMPLE_TYPE *idata0, *idata1;
  int reporting = 0;
  char *ofile0 = NULL, *ofile1 = NULL;
  if (dur <= 0) return;
  cp0 = c0->cp;
  sp0 = cp0->sound;
  cp1 = c1->cp;
  reporting = ((sp0) && (dur > (MAX_BUFFER_SIZE * 10)));
  if (reporting) start_progress_report(sp0, NOT_FROM_ENVED);
  if (dur > MAX_BUFFER_SIZE)
    {
      temp_file = 1; 
      ofile0 = snd_tempnam(ss);
      hdr0 = make_temp_header(ofile0, SND_SRATE(sp0), 1, dur, (char *)S_swap_channels);
      ofd0 = open_temp_file(ofile0, 1, hdr0, ss);
      if (ofd0 == -1)
	{
	  snd_error("can't open swap-channels temp file %s: %s\n", ofile0, strerror(errno));
	  return;
	}
      datumb = mus_data_format_to_bytes_per_sample(hdr0->format);
      ofile1 = snd_tempnam(ss);
      hdr1 = make_temp_header(ofile1, SND_SRATE(sp0), 1, dur, (char *)S_swap_channels);
      ofd1 = open_temp_file(ofile1, 1, hdr1, ss);
      if (ofd1 == -1)
	{
	  close_temp_file(ofd0, hdr0, 0, sp0);
	  snd_error("can't open swap-channels temp file %s: %s\n", ofile1, strerror(errno));
	  return;
	}
    }
  else temp_file = 0;
  data0 = (MUS_SAMPLE_TYPE **)MALLOC(sizeof(MUS_SAMPLE_TYPE *));
  data0[0] = (MUS_SAMPLE_TYPE *)CALLOC(MAX_BUFFER_SIZE, sizeof(MUS_SAMPLE_TYPE)); 
  data1 = (MUS_SAMPLE_TYPE **)MALLOC(sizeof(MUS_SAMPLE_TYPE *));
  data1[0] = (MUS_SAMPLE_TYPE *)CALLOC(MAX_BUFFER_SIZE, sizeof(MUS_SAMPLE_TYPE)); 
  idata0 = data0[0];
  idata1 = data1[0];
  j = 0;
  for (k = 0; k < dur; k++)
    {
      idata0[j] = next_sample(c1);
      idata1[j] = next_sample(c0);
      j++;
      if ((temp_file) && (j == MAX_BUFFER_SIZE))
	{
	  err = mus_file_write(ofd0, 0, j - 1, 1, data0);
	  err = mus_file_write(ofd1, 0, j - 1, 1, data1);
	  j = 0;
	  if (err == -1) break;
	  if (reporting) progress_report(sp0, "scl", 1, 1, (Float)k / (Float)dur, NOT_FROM_ENVED);
	}
    }
  if (temp_file)
    {
      if (j > 0) 
	{
	  mus_file_write(ofd0, 0, j - 1, 1, data0);
	  mus_file_write(ofd1, 0, j - 1, 1, data1);
	}
      close_temp_file(ofd0, hdr0, dur * datumb, sp0);
      close_temp_file(ofd1, hdr1, dur * datumb, sp0); /* sp0 used here in case of error report */
      free_file_info(hdr0);
      free_file_info(hdr1);
      file_change_samples(beg, dur, ofile0, cp0, 0, DELETE_ME, LOCK_MIXES, S_swap_channels);
      file_change_samples(beg, dur, ofile1, cp1, 0, DELETE_ME, LOCK_MIXES, S_swap_channels);
      if (ofile0) {FREE(ofile0); ofile0 = NULL;}
      if (ofile1) {FREE(ofile1); ofile1 = NULL;}
      if (reporting) finish_progress_report(sp0, NOT_FROM_ENVED);
    }
  else 
    {
      change_samples(beg, dur, data0[0], cp0, LOCK_MIXES, S_swap_channels);
      change_samples(beg, dur, data1[0], cp1, LOCK_MIXES, S_swap_channels);
    }
  update_graph(cp0, NULL);
  update_graph(cp1, NULL);
  if (ofile0) FREE(ofile0);
  if (ofile1) FREE(ofile1);
  FREE(data0[0]);
  FREE(data0);
  FREE(data1[0]);
  FREE(data1);
}

/* -------- src -------- */

static Float input_as_needed(void *arg, int dir) 
{
  src_state *sr = (src_state *)arg;
  snd_fd *sf;
  sf = sr->sf;
  sr->sample++;
  if (dir > 0) 
    return(next_sample_to_float(sf));
  else return(previous_sample_to_float(sf));
}

src_state *make_src(snd_state *ss, Float srate, snd_fd *sf)
{
  src_state *sr;
  sr = (src_state *)CALLOC(1, sizeof(src_state));
  sr->sf = sf;
  sr->gen = mus_make_src(&input_as_needed, srate, sinc_width(ss), (void *)sr);
  sr->sample = 0; /* this is how the old form worked */
  return(sr);
}

Float run_src(src_state *sr, Float sr_change)
{
  return(mus_src(sr->gen, sr_change, &input_as_needed));
}

src_state *free_src(src_state *sr)
{
  mus_free(sr->gen);
  FREE(sr);
  return(NULL);
}

void src_env_or_num(snd_state *ss, chan_info *cp, env *e, Float ratio, int just_num, 
		    int from_enved, const char *origin, int over_selection, mus_any *gen, XEN edpos, int arg_pos, Float e_base)
{
  snd_info *sp = NULL;
  int reporting = 0;
  sync_state *sc;
  sync_info *si;
  snd_fd **sfs;
  snd_fd *sf;
  int i, idiff, jj;
  MUS_SAMPLE_TYPE **data;
  file_info *hdr = NULL;
  int j, k, ofd = 0, datumb = 0, ok, err = 0;
  char *ofile = NULL;
  MUS_SAMPLE_TYPE *idata;
  src_state *sr;
  int scdur, dur;
  int *old_marks = NULL, *new_marks = NULL;
  mark **mps;
  int cur_mark = 0, cur_mark_sample = -1, cur_marks = 0, m;
  Float env_val;
  int next_pass, stop_point = 0;
  mus_any *egen;

  if ((!just_num) && (e == NULL) && (gen == NULL)) return;
  if ((just_num) && (ratio == 0.0)) return;

  /* get envelope or src ratio */
  if (cp == NULL)
    {
      sp = any_selected_sound(ss); 
      if (sp) 
	cp = any_selected_channel(sp); 
      else 
	if (!over_selection) 
	  return;
    }
  else sp = cp->sound;
  /* get current syncd chans */
  sc = get_sync_state(ss, sp, cp, 0, over_selection, 
		      (ratio < 0.0) ? READ_BACKWARD : READ_FORWARD, /* 0->beg, 0->regexpr (ratio = 0.0 if from_enved) */
		      edpos,
		      origin, arg_pos);      
  if (sc == NULL) return;
  si = sc->si;
  sfs = sc->sfs;
  scdur = sc->dur;
  data = (MUS_SAMPLE_TYPE **)MALLOC(sizeof(MUS_SAMPLE_TYPE *));
  data[0] = (MUS_SAMPLE_TYPE *)CALLOC(MAX_BUFFER_SIZE, sizeof(MUS_SAMPLE_TYPE)); 

  if (!(ss->stopped_explicitly))
    {
      for (i = 0; i < si->chans; i++)
	{
	  cp = si->cps[i];
	  sp = cp->sound;
	  cur_marks = 0;
	  new_marks = NULL;
	  if (scdur == 0) 
	    dur = to_c_edit_samples(cp, edpos, origin, arg_pos); 
	  else dur = scdur;
	  if (dur == 0) 
	    {
	      if (sfs[i]) 
		{
		  free_snd_fd(sfs[i]); 
		  sfs[i] = NULL;
		}
	      continue;
	    }
	  reporting = ((sp) && (dur > (MAX_BUFFER_SIZE * 4)));
	  if (reporting) start_progress_report(sp, from_enved);
	  ofile = snd_tempnam(ss);
	  hdr = make_temp_header(ofile, SND_SRATE(sp), 1, dur, (char *)origin);
	  ofd = open_temp_file(ofile, 1, hdr, ss);
	  if (ofd == -1)
	    {
	      snd_error("can't open src-sound temp file %s: %s\n", ofile, strerror(errno));
	      break;
	    }
	  datumb = mus_data_format_to_bytes_per_sample(hdr->format);
	  sf = sfs[i];
	  idata = data[0];
	  if (!just_num) ratio = 0.0;            /* added 14-Mar-01 otherwise the envelope is an offset? */
	  sr = make_src(ss, ratio, sf);
	  j = 0;
	  if (just_num)
	    {
	      for (k = 0; sr->sample < dur; k++) /* sr->sample tracks input location -- produce output until input exhausted */
		{
		  idata[j] = (MUS_FLOAT_TO_SAMPLE(run_src(sr, 0.0)));
		  j++;
		  if (j == MAX_BUFFER_SIZE)
		    {
		      err = mus_file_write(ofd, 0, j - 1, 1, data);
		      j = 0;
		      if (err == -1) break;
		      if (reporting) 
			{
			  progress_report(sp, S_src_sound, i + 1, si->chans, (Float)(sr->sample) / (Float)dur, from_enved);
			  if (ss->stopped_explicitly) break;
			}
		    }
		}
	    }
	  else
	    {
	      /* envelope case -- have to go by sr->sample, not output sample counter, also check marks */
	      cur_mark_sample = -1;
	      if ((cp->marks) && 
		  (cp->mark_ctr[cp->edit_ctr] >= 0))
		{
		  mps = cp->marks[cp->edit_ctr];
		  cur_marks = cp->mark_ctr[cp->edit_ctr] + 1;
		  new_marks = (int *)MALLOC(cur_marks * sizeof(int));
		  old_marks = (int *)MALLOC(cur_marks * sizeof(int));
		  for (m = 0; m < cur_marks; m++)
		    {
		      new_marks[m] = -1;
		      old_marks[m] = mps[m]->samp;
		    }
		  for (m = 0; m < cur_marks; m++)
		    if (old_marks[m] > si->begs[i])
		      {
			cur_mark_sample = old_marks[m];
			cur_mark = m;
			break;
		      }
		}
	      if (e)
		egen = mus_make_env(e->data, e->pts, 1.0, 0.0, e_base, 0.0, 0, dur - 1, NULL);
	      else egen = gen;
	      next_pass = sr->sample;
	      env_val = mus_env(egen);
	      for (k = 0; sr->sample < dur; k++)
		{
		  idata[j] = (MUS_FLOAT_TO_SAMPLE(run_src(sr, env_val)));
		  j++;
		  if (j == MAX_BUFFER_SIZE)
		    {
		      err = mus_file_write(ofd, 0, j - 1, 1, data);
		      j = 0;
		      if (err == -1) break;
		      if (reporting) 
			{
			  progress_report(sp, S_src_sound, i + 1, si->chans, (Float)(sr->sample) / (Float)dur, from_enved);
			  if (ss->stopped_explicitly) break;
			}
		    }
		  if (next_pass != sr->sample)             /* tick env forward dependent on sr->sample */
		    {
		      idiff = sr->sample - next_pass;
		      next_pass = sr->sample;
		      if ((new_marks) && 
			  (cur_mark_sample != -1) && 
			  (next_pass >= (cur_mark_sample - si->begs[i]))) 
			{
			                                    /* not '==' because sr->sample can be incremented by more than 1 */
			  new_marks[cur_mark] = k + si->begs[i];
			  cur_mark++;
			  if (cur_mark < cur_marks)
			    cur_mark_sample = old_marks[cur_mark];
			  else cur_mark_sample = -1;
			}
		      for (jj = 0; jj < idiff; jj++) 
			env_val = mus_env(egen);
		    }
		}
	      if (e) 
		mus_free(egen);
	      else mus_restart_env(gen);
	    }
	  if (reporting) finish_progress_report(sp, from_enved);
	  sr = free_src(sr);
	  if (j > 0) mus_file_write(ofd, 0, j - 1, 1, data);
	  close_temp_file(ofd, hdr, k * datumb, sp);
	  hdr = free_file_info(hdr);
	  if (over_selection)
	    {
	      /* here we need delete followed by insert since dur is probably different */
	      if (k == dur)
		file_change_samples(si->begs[i], dur, ofile, cp, 0, DELETE_ME, LOCK_MIXES, origin);
	      else
		{
		  ok = delete_selection(S_src_selection, DONT_UPDATE_DISPLAY);
		  file_insert_samples(si->begs[i], k, ofile, cp, 0, DELETE_ME, S_src_selection);
		  reactivate_selection(si->cps[i], si->begs[i], si->begs[i]+k);
		  if (ok) 
		    backup_edit_list(cp);
		  if (cp->marks) 
		    ripple_marks(cp, 0, 0);
		  update_graph(cp, NULL);
		}
	    }
	  else file_override_samples(k, ofile, cp, 0, DELETE_ME, LOCK_MIXES, origin);
	  /* not file_change_samples because that would not necessarily change the current file length */
	  if (cp->marks)
	    {
	      if (just_num)
		src_marks(cp, ratio, dur, k, si->begs[i], over_selection);
	      else
		{
		  if (new_marks) 
		    reset_marks(cp, cur_marks, new_marks, si->begs[i] + dur, (k - dur), over_selection);
		}
	      update_graph(cp, NULL);
	    }
	  if (old_marks) FREE(old_marks);
	  old_marks = NULL;
	  if (new_marks) FREE(new_marks);
	  new_marks = NULL;
	  FREE(ofile); 
	  ofile = NULL;
	  sfs[i] = free_snd_fd(sfs[i]);
	  if (ss->stopped_explicitly) 
	    {
	      stop_point = i;
	      break;
	    }
	}
    }
  if (ss->stopped_explicitly)
    {
      /* clean up and undo all edits up to stop_point */
      ss->stopped_explicitly = 0;
      for (i = 0; i <= stop_point; i++)
	{
	  cp = si->cps[i];
	  undo_edit(cp, 1);
	}
    }
  FREE(data[0]);
  FREE(data);
  for (i = 0; i < si->chans; i++) 
    if (sfs[i]) 
      free_snd_fd(sfs[i]);
  free_sync_state(sc);
}


/* FIR filtering */

static Float *env2array(int order, env *e)
{
  Float *fdata = NULL;
  Float x;
  int i, j;
  Float last_x, step;
  if (!e) return(NULL);
  /* get the frequency envelope and design the FIR filter */
  fdata = (Float *)CALLOC(order, sizeof(Float));
  last_x = e->data[(e->pts - 1) * 2];
  step = 2 * last_x / ((Float)order - 1);
  for (i = 0, x = 0.0; i < order / 2; i++, x += step) 
    fdata[i] = list_interp(x, e->data, e->pts); /* not mus_env here since it's likely the points fall between the order-related samples */
  for (j = order / 2 - 1, i = order / 2; 
       (i < order) && (j >= 0); 
       i++, j--) 
    fdata[i] = fdata[j];
  return(fdata);
}

static Float *get_filter_coeffs(int order, env *e)
{
  /* interpret e as frequency response */
  Float *a = NULL, *fdata;
  if (!e) return(NULL);
  /* get the frequency envelope and design the FIR filter */
  fdata = env2array(order, e);
  if (!fdata) return(NULL);
  a = (Float *)CALLOC(order, sizeof(Float));
  mus_make_fir_coeffs(order, fdata, a);
  FREE(fdata);
  return(a);
}

static Float frequency_response(Float *coeffs, int order, Float frq)
{
  Float at = 0.0, am;
  int n2, i;
  n2 = order >> 1;
  am = (order + 1) * 0.5;
  for (i = 0; i < n2; i++) 
    at += coeffs[i] * cos(M_PI * (am - i - 1) * frq);
  if (at < 0.0) return(-2 * at);
  return(2 * at);
}

static Float dB(snd_state *ss, Float py)
{
  return((py <= ss->lin_dB) ? ss->min_dB : (20.0 * (log10(py))));
}

void display_frequency_response(snd_state *ss, env *e, axis_info *ap, axis_context *gax, int order, int dBing)
{
  /* not cp->min_dB here -- this is sound panel related which refers to ss->min_dB */
  Float *coeffs = NULL;
  int height, width, i, pts, x0, y0, x1, y1;
  Float samps_per_pixel, invpts, resp, frq, pix;
  if (order & 1) order++;
  height = (ap->y_axis_y1 - ap->y_axis_y0);
  width = (ap->x_axis_x1 - ap->x_axis_x0);
  pts = order * 4;
  if (pts > (width * 10)) return;        /* no point in the graph since it just duplicates the current envelope and slows everything to a halt */
  if (pts > width) pts = width;
  if (pts <= 0) pts = 1;
  invpts = 1.0 / (Float)pts;
  samps_per_pixel = (Float)(ap->x_axis_x1 - ap->x_axis_x0) * invpts;
  x1 = ap->x_axis_x0;
  coeffs = get_filter_coeffs(order, e);
  if (!coeffs) return;
  resp = frequency_response(coeffs, order, 0.0);
  if (dBing)
    y1 = (int)(ap->y_axis_y0 + (ss->min_dB - dB(ss, resp)) * height / ss->min_dB);
  else y1 = (int)(ap->y_axis_y0 + resp * height);
  for (i = 1, pix = x1, frq = invpts; 
       i < pts; 
       i++, pix += samps_per_pixel, frq += invpts)
    {
      x0 = x1;
      y0 = y1;
      x1 = (int)(pix);
      resp = frequency_response(coeffs, order, frq);
      if (dBing)
	y1 = (int)(ap->y_axis_y0 + (ss->min_dB - dB(ss, resp)) * height / ss->min_dB);
      else y1 = (int)(ap->y_axis_y0 + resp * height);
      draw_line(gax, x0, y0, x1, y1);
    }
  FREE(coeffs);
}


/* Hartley Transfrom */

static Float *fht_sines = NULL, *fht_cosines = NULL;
static int fht_last_length = 0, fht_length = 0;

static void make_sines(int length)
{
  int i;
  Float freq, temp;
  if (length != fht_last_length)
    {
      if (fht_length < length)
	{
	  if (fht_sines) FREE(fht_sines);
	  if (fht_cosines) FREE(fht_cosines);
	  fht_sines = (Float *)MALLOC(length * sizeof(Float));
	  fht_cosines = (Float *)MALLOC(length * sizeof(Float));
	  fht_length = length;
	}
      fht_last_length = length;
      freq = TWO_PI / (Float)length;
      for (i = 0; i < length; i++) 
	{
	  temp = freq * i;
	  fht_sines[i] = sin(temp);
	  fht_cosines[i] = cos(temp);
        }
    }
}

void fht(int powerOfFour, Float *array)
{
  /*  In place Fast Hartley Transform of floating point data in array.
      Size of data array must be power of four. Lots of sets of four 
      inline code statements, so it is verbose and repetitive, but fast. 
      The Fast Hartley Transform algorithm is patented, and is documented
      in the book "The Hartley Transform", by Ronald N. Bracewell.
      This routine was converted to C from a BASIC routine in the above book,
      that routine Copyright 1985, The Board of Trustees of Stanford University 	
   */
  /* fht is its own inverse, so to speak -- run it again to return to original data */

  register int j = 0, i = 0, k = 0, L = 0;
  int n = 0, n4 = 0, d1 = 0, d2 = 0, d3 = 0, d4 = 0, d5 = 1, d6 = 0, d7 = 0, d8 = 0, d9 = 0;
  int L1 = 0, L2 = 0, L3 = 0, L4 = 0, L5 = 0, L6 = 0, L7 = 0, L8 = 0;
  int n_over_d3;
  Float r = 0.0;
  int a1 = 0, a2 = 0, a3 = 0;
  Float t = 0.0, t1 = 0.0, t2 =0.0, t3 = 0.0, t4 = 0.0, t5 = 0.0, t6 = 0.0, t7 = 0.0, t8 = 0.0;
  Float t9 = 0.0, t0 = 0.0;
  n = (int)(pow(4.0 , (double)powerOfFour));
  make_sines(n);
  n4 = n / 4;
  r = 1.414213562;
  j = 1;
  i = 0;
  while (i < n - 1)	
    {
      i++;
      if (i < j)	
	{
	  t = array[j - 1];
	  array[j - 1] = array[i - 1];
	  array[i - 1] = t;
    	}
      k = n4;
      while ((3 * k) < j)	
	{
	  j -= 3 * k;
	  k /= 4;
    	}
      j += k;
    }
  for (i = 0; i < n; i += 4) 
    {
      t5 = array[i];
      t6 = array[i + 1];
      t7 = array[i + 2];
      t8 = array[i + 3];
      t1 = t5 + t6;
      t2 = t5 - t6;
      t3 = t7 + t8;
      t4 = t7 - t8;
      array[i] = t1 + t3;
      array[i + 1] = t1 - t3;
      array[i + 2] = t2 + t4;
      array[i + 3] = t2 - t4;
    }
  for (L = 2; L <= powerOfFour; L++)  
    {
      d1 = (int)(pow(2.0 , L + L - 3.0));
      d2 = d1 + d1;
      d3 = d2 + d2;
      n_over_d3 = n / 2 / d3;
      d4 = d2 + d3;
      d5 = d3 + d3;
      for (j = 0; j < n; j += d5)	  
	{
	  t5 = array[j];
	  t6 = array[j + d2];
	  t7 = array[j + d3];
	  t8 = array[j + d4];
	  t1 = t5 + t6;
	  t2 = t5 - t6;
	  t3 = t7 + t8;
	  t4 = t7 - t8;
	  array[j] = t1 + t3;
	  array[j + d2] = t1 - t3;
	  array[j + d3] = t2 + t4;
	  array[j + d4] = t2 - t4;
	  d6 = j + d1;
	  d7 = j + d1 + d2;
	  d8 = j + d1 + d3;
	  d9 = j + d1 + d4;
	  t1 = array[d6];
	  t2 = array[d7] * r;
	  t3 = array[d8];
	  t4 = array[d9] * r;
	  array[d6] = t1 + t2 + t3;
	  array[d7] = t1 - t3 + t4;
	  array[d8] = t1 - t2 + t3;
	  array[d9] = t1 - t3 - t4;
	  for (k = 1; k < d1; k++)	
	    {
	      L1 = j + k;
	      L2 = L1 + d2;
	      L3 = L1 + d3;
	      L4 = L1 + d4;
	      L5 = j + d2 - k;
	      L6 = L5 + d2;
	      L7 = L5 + d3;
	      L8 = L5 + d4;
	      a1 = (int) (k * n_over_d3) % n;
	      a2 = (a1 + a1) % n;
	      a3 = (a1 + a2) % n;
	      t5 = array[L2] * fht_cosines[a1] + array[L6] * fht_sines[a1];
	      t6 = array[L3] * fht_cosines[a2] + array[L7] * fht_sines[a2];
	      t7 = array[L4] * fht_cosines[a3] + array[L8] * fht_sines[a3];
	      t8 = array[L6] * fht_cosines[a1] - array[L2] * fht_sines[a1];
	      t9 = array[L7] * fht_cosines[a2] - array[L3] * fht_sines[a2];
	      t0 = array[L8] * fht_cosines[a3] - array[L4] * fht_sines[a3];
	      t1 = array[L5] - t9;
	      t2 = array[L5] + t9;
	      t3 = - t8 - t0;
	      t4 = t5 - t7;
	      array[L5] = t1 + t4;
	      array[L6] = t2 + t3;
	      array[L7] = t1 - t4;
	      array[L8] = t2 - t3;
	      t1 = array[L1] + t6;
	      t2 = array[L1] - t6;
	      t3 = t8 - t0;
	      t4 = t5 + t7;
	      array[L1] = t1 + t4;
	      array[L2] = t2 + t3;
	      array[L3] = t1 - t4;
	      array[L4] = t2 - t3;
	    }
	}
    }		  
  if (n > 65536)
    {
      FREE(fht_sines);
      fht_sines = NULL;
      FREE(fht_cosines);
      fht_cosines = NULL;
      fht_last_length = 0;
      fht_length = 0;
    }
}

/* TODO: non-fht(fft) case is centered but straight case is not! */
/* TODO: apply_filter should take beg/dur */

static char *apply_filter_or_error(chan_info *ncp, int order, env *e, int from_enved, 
				   const char *origin, int over_selection, Float *ur_a, mus_any *gen, XEN edpos, int arg_pos)
{
  /* if string returned, needs to be freed */
  /* interpret e as frequency response and apply as filter to all sync'd chans */
  Float *a = NULL, *d = NULL;
  Float x;
  sync_state *sc;
  sync_info *si;
  snd_state *ss;
  snd_info *sp;
  int reporting = 0;
  int i, m, scdur, dur, k, stop_point = 0, prebeg = 0;
  snd_fd **sfs;
  snd_fd *sf;
  file_info *hdr = NULL;
  int j, ofd = 0, datumb = 0, temp_file, err = 0;
  MUS_SAMPLE_TYPE **data;
  MUS_SAMPLE_TYPE *idata;
  char *ofile = NULL;
  chan_info *cp;
  int fsize;
  Float scale;
  Float *sndrdat = NULL, *fltdat = NULL;
  int pow4;

  if ((!e) && (!ur_a) && (!gen)) 
    return(NULL);
  if ((gen) && (!(MUS_RUN_P(gen))))
    return(mus_format("%s can't handle %s generators [%s[%d]: %s]",
		      origin,
		      mus_name(gen),
		      __FILE__, __LINE__, __FUNCTION__));
  ss = ncp->state;
  sp = ncp->sound;
  sc = get_sync_state_1(ss, sp, ncp, 0, over_selection, 
			READ_FORWARD, (over_selection) ? (order - 1) : 0, 
			edpos,
			origin, arg_pos);
  if (sc == NULL) return(NULL);
  si = sc->si;
  sfs = sc->sfs;
  scdur = sc->dur;

  if ((!ur_a) && 
      (!gen) && 
      (!over_selection) && 
      ((order == 0) || (order >= 256)) && 
      ((int)((to_c_edit_samples(ncp, edpos, origin, arg_pos) + order) / 128) < ss->memory_available))
    {
      /* use convolution if order is large and there's memory available (and not over_selection) */
      /*   probably faster here would be overlap-add */
      /*   but user is almost certainly making a mistake elsewhere... */
      for (i = 0; i < si->chans; i++)
	{
	  cp = si->cps[i];
	  sp = cp->sound;
	  if (scdur == 0) 
	    dur = to_c_edit_samples(cp, edpos, origin, arg_pos);
	  else dur = scdur;
	  if (dur == 0) 
	    {
	      if (sfs[i]) {free_snd_fd(sfs[i]); sfs[i] = NULL;}
	      continue;
	    }
	  dur += order;
	  fsize = (int)(pow(4.0, pow4 = (int)ceil(log(dur) / log(4.0))));
	  sndrdat = (Float *)CALLOC(fsize, sizeof(Float));
	  fltdat = env2array(fsize, e);

	  sf = sfs[i];                                 /* init_sample_read(0, cp, READ_FORWARD); */
	  for (k = 0; k < dur; k++) 
	    sndrdat[k] = (Float)(next_sample_to_float(sf));
	  sfs[i] = free_snd_fd(sf);

	  fht(pow4, sndrdat);
	  check_for_event(ss);
	  if (ss->stopped_explicitly)
	    {
	      ss->stopped_explicitly = 0;
	      report_in_minibuffer(sp, "stopped");
	      FREE(sndrdat);
	      FREE(fltdat);
	      break;
	    }
	  scale = 1.0 / (Float)fsize;
	  for (k = 0; k < fsize; k++)
	    sndrdat[k] *= (scale * fltdat[k]);         /* fltdat (via env2array) is already reflected around midpoint */
	  fht(pow4, sndrdat);

	  ofile = snd_tempnam(ss);
	  hdr = make_temp_header(ofile, SND_SRATE(sp), 1, dur, (char *)origin);
#if MUS_LITTLE_ENDIAN
	  if (sizeof(Float) == 4)
	    hdr->format = MUS_LFLOAT;
	  else hdr->format = MUS_LDOUBLE;
#else
	  if (sizeof(Float) == 4)
	    hdr->format = MUS_BFLOAT;
	  else hdr->format = MUS_BDOUBLE;
#endif
	  ofd = open_temp_file(ofile, 1, hdr, ss);
	  if (ofd == -1)
	    {
	      snd_error("can't open filter-sound temp file %s: %s\n", ofile, strerror(errno));
	      break;
	    }
	  write(ofd, sndrdat, fsize * sizeof(Float));
	  close_temp_file(ofd, hdr, fsize * sizeof(Float), sp);
	  hdr = free_file_info(hdr);
	  file_change_samples(0, dur + order, ofile, cp, 0, DELETE_ME, LOCK_MIXES, origin);
	  if (ofile) {FREE(ofile); ofile = NULL;}
	  update_graph(cp, NULL);
	  FREE(sndrdat);
	  FREE(fltdat);
	  check_for_event(ss);
	  if (ss->stopped_explicitly)
	    {
	      ss->stopped_explicitly = 0;
	      report_in_minibuffer(sp, "stopped");
	      break;
	    }
	}
    }
  else
    {
      if (!gen)
	{
	  if (ur_a)
	    a = ur_a;
	  else 
	    {
	      if (order & 1) order++;
	      a = get_filter_coeffs(order, e);
	    }
	  if (!a) return(NULL);
	  d = (Float *)CALLOC(order, sizeof(Float));
	}
      /* now filter all currently sync'd chans (one by one) */
      /* for each decide whether a file or internal array is needed, scale, update edit tree */
      data = (MUS_SAMPLE_TYPE **)MALLOC(sizeof(MUS_SAMPLE_TYPE *));
      data[0] = (MUS_SAMPLE_TYPE *)CALLOC(MAX_BUFFER_SIZE, sizeof(MUS_SAMPLE_TYPE)); 
      
      if (!(ss->stopped_explicitly))
	{
	  for (i = 0; i < si->chans; i++)
	    {
	      /* done channel at a time here, rather than in parallel as in apply_env because */
	      /* in this case, the various sync'd channels may be different lengths */
	      cp = si->cps[i];
	      sp = cp->sound;
	      if (scdur == 0) 
		dur = to_c_edit_samples(cp, edpos, origin, arg_pos);
	      else dur = scdur;
	      if (dur == 0) 
		{
		  if (sfs[i]) {free_snd_fd(sfs[i]); sfs[i] = NULL;}
		  continue;
		}
	      dur += order;
	      reporting = ((sp) && (dur > (MAX_BUFFER_SIZE * 4)));
	      if (reporting) start_progress_report(sp, from_enved);
	      if (dur > MAX_BUFFER_SIZE)
		{
		  temp_file = 1; 
		  ofile = snd_tempnam(ss);
		  hdr = make_temp_header(ofile, SND_SRATE(sp), 1, dur, (char *)origin);
		  ofd = open_temp_file(ofile, 1, hdr, ss);
		  if (ofd == -1)
		    {
		      snd_error("can't open filter-sound temp file %s: %s\n", ofile, strerror(errno));
		      break;
		    }
		  datumb = mus_data_format_to_bytes_per_sample(hdr->format);
		}
	      else temp_file = 0;
	      sf = sfs[i];
	      idata = data[0];
	      if (gen)
		mus_clear_filter_state(gen);
	      else
		{
		  for (m = 0; m < order; m++) d[m] = 0.0;
		  if (over_selection)
		    {
		      /* see if there's data to pre-load the filter */
		      if (si->begs[i] >= order)
			prebeg = order - 1;
		      else prebeg = si->begs[i];
		      if (prebeg > 0)
			for (m = prebeg; m > 0; m--)
			  d[m] = next_sample_to_float(sf);
		    }
		}
	      j = 0;
	      for (k = 0; k < dur; k++)
		{
		  if (gen)
		    x = MUS_RUN(gen, next_sample_to_float(sf), 0.0);
		  else
		    {
		      x = 0.0; 
		      d[0] = next_sample_to_float(sf);
		      for (m = order - 1; m > 0; m--) 
			{
			  x += d[m] * a[m]; 
			  d[m] = d[m - 1];
			} 
		      x += d[0] * a[0]; 
		    }
		  idata[j] = MUS_FLOAT_TO_SAMPLE(x);
		  j++;
		  if ((temp_file) && (j == MAX_BUFFER_SIZE))
		    {
		      err = mus_file_write(ofd, 0, j - 1, 1, data);
		      j = 0;
		      if (err == -1) break;
		      if (reporting) 
			{
			  progress_report(sp, S_filter_sound, i + 1, si->chans, (Float)k / (Float)dur, from_enved);
			  if (ss->stopped_explicitly) break;
			}
		    }
		}
	      if (reporting) finish_progress_report(sp, from_enved);
	      if (temp_file)
		{
		  if (j > 0) mus_file_write(ofd, 0, j - 1, 1, data);
		  close_temp_file(ofd, hdr, dur * datumb, sp);
		  hdr = free_file_info(hdr);
		  if (over_selection)
		    file_change_samples(si->begs[i], dur, ofile, cp, 0, DELETE_ME, LOCK_MIXES, origin);
		  else file_change_samples(0, dur, ofile, cp, 0, DELETE_ME, LOCK_MIXES, origin);
		  if (ofile) {FREE(ofile); ofile = NULL;}
		}
	      else change_samples(si->begs[i], dur, data[0], cp, LOCK_MIXES, origin);
	      update_graph(cp, NULL); 
	      sfs[i] = free_snd_fd(sfs[i]);
	      if (ss->stopped_explicitly) 
		{
		  stop_point = i;
		  break;
		}
	    }
	}
      if (ss->stopped_explicitly)
	{
	  /* clean up and undo all edits up to stop_point */
	  ss->stopped_explicitly = 0;
	  for (i = 0; i <= stop_point; i++)
	    {
	      cp = si->cps[i];
	      undo_edit(cp, 1);
	    }
	}
      FREE(data[0]);
      FREE(data);
      if ((a) && (!ur_a)) FREE(a);
      if (d) FREE(d);
    }
  free_sync_state(sc);
  return(NULL);
}

void apply_filter(chan_info *ncp, int order, env *e, int from_enved, 
		  const char *origin, int over_selection, Float *ur_a, mus_any *gen, XEN edpos, int arg_pos)
{
  char *error;
  error = apply_filter_or_error(ncp, order, e, from_enved, origin, over_selection, ur_a, gen, edpos, arg_pos);
  if (error)
    {
      snd_error(error);
      FREE(error);
    }
}

static MUS_SAMPLE_TYPE previous_sample_unscaled(snd_fd *sf)
{
  if (sf->view_buffered_data < sf->first)
    return(previous_sound(sf));
  else return(*sf->view_buffered_data--);
}

static void reverse_sound(chan_info *ncp, int over_selection, XEN edpos, int arg_pos)
{
  sync_state *sc;
  sync_info *si;
  snd_state *ss;
  snd_info *sp;
  env_info *ep;
  int i, dur, k, stop_point = 0;
  snd_fd **sfs;
  snd_fd *sf;
  file_info *hdr = NULL;
  int j, ofd = 0, datumb = 0, temp_file, err = 0;
  MUS_SAMPLE_TYPE **data;
  MUS_SAMPLE_TYPE *idata;
  char *ofile = NULL;
  chan_info *cp;
  char *caller;
  ss = ncp->state;
  sp = ncp->sound;
  caller = (char *)((over_selection) ? S_reverse_selection : S_reverse_sound);
  sc = get_sync_state(ss, sp, ncp, 0, over_selection, READ_BACKWARD, 
		      edpos,
		      (const char *)caller, arg_pos);
  if (sc == NULL) return;
  si = sc->si;
  sfs = sc->sfs;
  data = (MUS_SAMPLE_TYPE **)MALLOC(sizeof(MUS_SAMPLE_TYPE *));
  data[0] = (MUS_SAMPLE_TYPE *)CALLOC(MAX_BUFFER_SIZE, sizeof(MUS_SAMPLE_TYPE)); 
  if (!(ss->stopped_explicitly))
    {
      for (i = 0; i < si->chans; i++)
	{
	  cp = si->cps[i];
	  sp = cp->sound;
	  ep = NULL;
	  if (over_selection)
	    dur = sc->dur;
	  else 
	    {
	      dur = to_c_edit_samples(cp, edpos, caller, arg_pos);
	      ep = amp_env_copy(cp, TRUE, to_c_edit_position(cp, edpos, caller, arg_pos));
	    }
	  if (dur == 0) 
	    {
	      if (sfs[i]) {free_snd_fd(sfs[i]); sfs[i] = NULL;}
	      continue;
	    }
	  if (dur > MAX_BUFFER_SIZE)
	    {
	      temp_file = 1; 
	      ofile = snd_tempnam(ss);
	      hdr = make_temp_header(ofile, SND_SRATE(sp), 1, dur, (char *)S_reverse_sound);
	      ofd = open_temp_file(ofile, 1, hdr, ss);
	      if (ofd == -1)
		{
		  snd_error("can't open reverse-sound temp file %s: %s\n", ofile, strerror(errno));
		  break;
		}
	      datumb = mus_data_format_to_bytes_per_sample(hdr->format);
	    }
	  else temp_file = 0;
	  sf = sfs[i];
	  idata = data[0];
	  j = 0;
	  if (no_ed_scalers(cp))
	    {
	      for (k = 0; k < dur; k++)
		{
		  idata[j] = previous_sample_unscaled(sf);
		  j++;
		  if ((temp_file) && (j == MAX_BUFFER_SIZE))
		    {
		      err = mus_file_write(ofd, 0, j - 1, 1, data);
		      j = 0;
		      if (err == -1) break;
		    }
		}
	    }
	  else
	    {
	      for (k = 0; k < dur; k++)
		{
		  idata[j] = previous_sample(sf);
		  j++;
		  if ((temp_file) && (j == MAX_BUFFER_SIZE))
		    {
		      err = mus_file_write(ofd, 0, j - 1, 1, data);
		      j = 0;
		      if (err == -1) break;
		    }
		}
	    }
	  if (temp_file)
	    {
	      if (j > 0) mus_file_write(ofd, 0, j - 1, 1, data);
	      close_temp_file(ofd, hdr, dur * datumb, sp);
	      hdr = free_file_info(hdr);
	      if (over_selection)
		file_change_samples(si->begs[i], dur, ofile, cp, 0, DELETE_ME, LOCK_MIXES, S_reverse_selection);
	      else file_change_samples(0, dur, ofile, cp, 0, DELETE_ME, LOCK_MIXES, S_reverse_sound);
	      if (ofile) {FREE(ofile); ofile = NULL;}
	    }
	  else change_samples(si->begs[i], dur, data[0], cp, LOCK_MIXES, caller);
	  if (ep) cp->amp_envs[cp->edit_ctr] = ep;
	  ep = NULL;
	  if (cp->marks)
	    {
	      /* marks refer to particular samples, not positions, so they too must be reversed */
	      /* it just occurs to me that mark indices cannot be used across undo/redo */
	      reverse_marks(cp, over_selection);
	    }
	  update_graph(cp, NULL); 
	  sfs[i] = free_snd_fd(sfs[i]);
	  if (ss->stopped_explicitly) 
	    {
	      stop_point = i;
	      break;
	    }
	}
    }
  if (ss->stopped_explicitly)
    {
      ss->stopped_explicitly = 0;
      for (i = 0; i <= stop_point; i++)
	{
	  cp = si->cps[i];
	  undo_edit(cp, 1);
	}
    }
  FREE(data[0]);
  FREE(data);
  free_sync_state(sc);
}


/* amplitude envelopes */
/*   changed to use mus_env 20-Dec-00 */

void apply_env(chan_info *cp, env *e, int beg, int dur, Float scaler, int regexpr, 
	       int from_enved, const char *origin, mus_any *gen, XEN edpos, int arg_pos, Float e_base)
{
  snd_fd *sf = NULL;
  snd_info *sp;
  sync_info *si;
  sync_state *sc;
  snd_fd **sfs;
  file_info *hdr = NULL;
  int i, j, k, ofd = 0, datumb = 0, temp_file, err = 0, scalable = 1;
  MUS_SAMPLE_TYPE **data;
  MUS_SAMPLE_TYPE *idata;
  int reporting = 0;
  Float val[1];
  char *ofile = NULL;
  snd_state *ss;
  mus_any *egen;
  Float egen_val;
  
  if ((!e) && (!gen)) return;
  if (e)
    {
      if (e->pts == 0) return;
      val[0] = e->data[1];
      for (i = 1, j = 2; i < e->pts; i++, j += 2)
	if (e->data[j + 1] != val[0]) 
	  {
	    scalable = 0; 
	    break;
	  }
      if (scalable)
	{
	  val[0] *= scaler;
	  if ((beg == 0) && 
	      ((dur == 0) || (dur >= current_ed_samples(cp))))
	    {
	      scale_by(cp, val, 1, regexpr);
	      return;
	    }
	}
    }
  else scalable = 0;
  si = NULL;
  sp = cp->sound;
  ss = cp->state;
  sc = get_sync_state(ss, sp, cp, beg, regexpr, READ_FORWARD, edpos, origin, arg_pos);
  if (sc == NULL) return;
  si = sc->si;
  sfs = sc->sfs;
  if (regexpr) dur = sc->dur;
  if (dur == 0) 
    {
      for (i = 0; i < si->chans; i++) 
	if (sfs[i]) 
	  free_snd_fd(sfs[i]);
      free_sync_state(sc); 
      return;
    }
  if (scalable)
    {
      for (i = 0; i < si->chans; i++) 
	scale_channel(si->cps[i], val[0], beg, dur);
      free_sync_state(sc);
      return;
    }
  if (e)
    egen = mus_make_env(e->data, e->pts, scaler, 0.0, e_base, 0.0, 0, dur - 1, NULL);
  else egen = gen;

  if (dur > MAX_BUFFER_SIZE) /* if smaller than this, we don't gain anything by using a temp file (its buffers are this large) */
    {
      temp_file = 1; 
      ofile = snd_tempnam(ss); 
      hdr = make_temp_header(ofile, SND_SRATE(sp), si->chans, dur, (char *)origin);
      ofd = open_temp_file(ofile, si->chans, hdr, ss);
      if (ofd == -1)
	{
	  if (e) mus_free(egen);
	  for (i = 0; i < si->chans; i++) 
	    if (sfs[i]) 
	      free_snd_fd(sfs[i]);
	  free_sync_state(sc);
	  snd_error("can't open env-sound temp file %s: %s\n", ofile, strerror(errno));
	  FREE(ofile);
	  return;
	}
      datumb = mus_data_format_to_bytes_per_sample(hdr->format);
    }
  else temp_file = 0;

  data = (MUS_SAMPLE_TYPE **)MALLOC(si->chans * sizeof(MUS_SAMPLE_TYPE *));
  for (i = 0; i < si->chans; i++) 
    {
      if (temp_file)
	data[i] = (MUS_SAMPLE_TYPE *)CALLOC(FILE_BUFFER_SIZE, sizeof(MUS_SAMPLE_TYPE)); 
      else data[i] = (MUS_SAMPLE_TYPE *)CALLOC(dur, sizeof(MUS_SAMPLE_TYPE)); 
    }

  j = 0;
  reporting = (dur > (MAX_BUFFER_SIZE * 4));
  if (reporting) start_progress_report(sp, from_enved);
  if (si->chans > 1)
    {
      for (i = 0; i < dur; i++)
	{
	  egen_val = mus_env(egen);
	  for (k = 0; k < si->chans; k++)
	    data[k][j] = MUS_FLOAT_TO_SAMPLE(next_sample_to_float(sfs[k]) * egen_val);
	  j++;
	  if ((temp_file) && (j == FILE_BUFFER_SIZE))
	    {
	      if (reporting) 
		progress_report(sp, S_env_sound, 0, 0, (Float)i / ((Float)dur), from_enved);
	      err = mus_file_write(ofd, 0, j - 1, si->chans, data);
	      j = 0;
	      if (err == -1) break;
	      if (ss->stopped_explicitly) break;
	    }
	}
    }
  else
    {
      sf = sfs[0];
      idata = data[0];
      for (i = 0; i < dur; i++)
	{
	  idata[j] = MUS_FLOAT_TO_SAMPLE(next_sample_to_float(sf) * mus_env(egen));
	  j++;
	  if ((temp_file) && (j == FILE_BUFFER_SIZE))
	    {
	      if (reporting)
		progress_report(sp, S_env_sound, 0, 0, (Float)i / ((Float)dur), from_enved);
	      err = mus_file_write(ofd, 0, j - 1, 1, data);
	      j = 0;
	      if (err == -1) break;
	      if (ss->stopped_explicitly) break;
	    }
	}
    }

  if (temp_file)
    {
      if (j > 0) mus_file_write(ofd, 0, j - 1, si->chans, data);
      close_temp_file(ofd, hdr, dur * si->chans * datumb, sp);
      free_file_info(hdr);
    }
  if (reporting) finish_progress_report(sp, from_enved);
  if (ss->stopped_explicitly)
    {
      ss->stopped_explicitly = 0;
      if (temp_file) 
	snd_remove(ofile);
    }
  else
    {
      if ((temp_file) && 
	  (si->chans > 1)) 
	remember_temp(ofile, si->chans);
      for (i = 0; i < si->chans; i++)
	{
	  if (temp_file)
	    file_change_samples(si->begs[i], dur, ofile, si->cps[i], i, 
				(si->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME, 
				LOCK_MIXES, origin);
	  else change_samples(si->begs[i], dur, data[i], si->cps[i], LOCK_MIXES, origin);
	  update_graph(si->cps[i], NULL);
	}
    }
  for (i = 0; i < si->chans; i++)
    {
      sfs[i] = free_snd_fd(sfs[i]);
      FREE(data[i]);
    }
  if ((temp_file) && (ofile)) {FREE(ofile); ofile = NULL;}
  if (data) FREE(data);
  if (e) mus_free(egen);
  free_sync_state(sc);
}

int cursor_delete(chan_info *cp, int count, const char *origin)
{
  int i, beg;
  snd_info *sp;
  sync_info *si;
  chan_info **cps;
  si = NULL;
  beg = cp->cursor;
  sp = cp->sound;
  if (sp->sync!= 0)
    {
      si = snd_sync(cp->state, sp->sync);
      cps = si->cps;
      for (i = 0; i < si->chans; i++)
	{
	  if (count > 0)
	    delete_samples(beg, count, cps[i], origin); 
	  else delete_samples(beg + count, -count, cps[i], origin);
	  update_graph(si->cps[i], NULL);
	}
      si = free_sync_info(si);
    }
  else
    {
      if (count > 0)
	delete_samples(beg, count, cp, origin);
      else delete_samples(beg + count, -count, cp, origin);
    }
  return(CURSOR_UPDATE_DISPLAY);
}

int cursor_delete_previous(chan_info *cp, int count, const char *origin)
{
  if (cp->cursor <= 0) return(CURSOR_UPDATE_DISPLAY);
  cp->cursor -= count;
  if (cp->cursor < 0)
    {
      count += cp->cursor;
      cp->cursor = 0;
    }
  return(cursor_delete(cp, count, origin));
}

int cursor_insert(chan_info *cp, int beg, int count, const char *origin)
{
  int i;
  snd_info *sp;
  sync_info *si;
  chan_info **cps;
  si = NULL;
  sp = cp->sound;
  if (count < 0) 
    {
      count = -count;
      if (count > beg) count = beg;
      beg -= count;
    }
  if (sp->sync != 0)
    {
      si = snd_sync(cp->state, sp->sync);
      cps = si->cps;
      for (i = 0; i < si->chans; i++)
	{
	  extend_with_zeros(cps[i], 
			    mus_iclamp(0, beg, current_ed_samples(si->cps[i]) - 1), 
			    count, origin);
	  update_graph(cps[i], NULL);
	}
      si = free_sync_info(si);
    }
  else 
    {
      extend_with_zeros(cp, 
			mus_iclamp(0, beg, current_ed_samples(cp)), 
			count, origin);
      update_graph(cp, NULL);
    }
  return(CURSOR_UPDATE_DISPLAY);
}

int cursor_zeros(chan_info *cp, int count, int regexpr)
{
  int i, num, old_sync, beg;
  snd_info *sp, *nsp;
  sync_info *si = NULL;
  chan_info *ncp;
  Float scaler[1];
  if (count == 0) return(CURSOR_NO_ACTION);
  if (count < 0) num = -count; else num = count;
  sp = cp->sound;
  if ((sp->sync != 0) && (!regexpr))
    {
      si = snd_sync(cp->state, sp->sync);
      for (i = 0; i < si->chans; i++) 
	si->begs[i] = cp->cursor;
    }
  else
    {
      if ((regexpr) && (selection_is_active()))
	{
	  si = selection_sync();
	  num = selection_len();
	}
    }
  if (!si) si = make_simple_sync(cp, cp->cursor);
  for (i = 0; i < si->chans; i++)
    {
      /* if zeroing entire sound, set scalers and remake amp_env */
      ncp = si->cps[i];
      if ((si->begs[i] == 0) && 
	  (num >= current_ed_samples(ncp)))
	{
	  nsp = ncp->sound;
	  old_sync = nsp->sync;
	  nsp->sync = 0;
	  scaler[0] = 0.0;
	  scale_by(ncp, scaler, 1, FALSE);
	  nsp->sync = old_sync;
	}
      else
	{
	  if (count > 0) 
	    beg = si->begs[i];
	  else beg = si->begs[i] + count;
	  /* special case 1 sample -- if already 0, treat as no-op */
	  if ((count != 1) || 
	      (beg >= current_ed_samples(ncp)) || 
	      (chn_sample(beg, ncp, ncp->edit_ctr) != 0.0))
	    scale_channel(ncp, 0.0, beg, num);
	}
    }
  si = free_sync_info(si);
  return(CURSOR_IN_VIEW);
}

void cos_smooth(chan_info *cp, int beg, int num, int regexpr, const char *origin)
{
  /* verbatim, so to speak from Dpysnd */
  /* start at beg, apply a cosine for num samples, matching endpoints */
  MUS_SAMPLE_TYPE *data = NULL;
  chan_info *ncp;
  sync_state *sc;
  int i, k;
  Float y0, y1, angle, incr, off, scale;
  snd_info *sp;
  sync_info *si;
  sp = cp->sound;
  sc = get_sync_state_without_snd_fds(cp->state, sp, cp, beg, regexpr);
  if (sc == NULL) return;
  si = sc->si;
  if (regexpr) num = sc->dur;
  for (i = 0; i < si->chans; i++)
    {
      ncp = si->cps[i];
      y0 = chn_sample(si->begs[i], ncp, ncp->edit_ctr);
      y1 = chn_sample(si->begs[i] + num, ncp, ncp->edit_ctr);
      if (y1 > y0) angle = M_PI; else angle = 0.0;
      incr = M_PI/(Float)num;
      off = 0.5 * (y1 + y0);
      scale = 0.5 * fabs(y0 - y1);
      data = (MUS_SAMPLE_TYPE *)CALLOC(num, sizeof(MUS_SAMPLE_TYPE));
      for (k = 0; k < num; k++, angle += incr) 
	data[k] = MUS_FLOAT_TO_SAMPLE(off + scale * cos(angle));
      change_samples(si->begs[i], num, data, ncp, LOCK_MIXES, origin);
      update_graph(ncp, NULL);
      FREE(data);
    }
  free_sync_state(sc);
}



#include "vct.h"
#include "clm2xen.h"

static XEN g_map_chan(XEN proc, XEN s_beg, XEN s_end, XEN org, XEN snd, XEN chn, XEN edpos) 
{ 
  #define H_map_chan "(" S_map_chan " func &optional (start 0) end edname snd chn edpos)\n\
apply func to samples in current channel, edname is the edit history name for this editing operation.\n\
  (map-chan abs)"

  snd_state *ss;
  chan_info *cp;
  char *caller;
  int beg, end;
  snd_info *sp;
  snd_fd *sf = NULL;
  XEN errstr;
  int kp, len, num, reporting = 0, rpt = 0, rpt4, i, j, cured;
  XEN res = XEN_FALSE;
  char *errmsg;
  char *filename;
  mus_any *outgen = NULL;
  XEN *data;
  vct *v;

  if (XEN_STRING_P(org)) 
    caller = XEN_TO_C_STRING(org);
  else caller = S_map_chan;
  XEN_ASSERT_TYPE((XEN_PROCEDURE_P(proc)), proc, XEN_ARG_1, caller, "a procedure");
  XEN_ASSERT_TYPE(XEN_NUMBER_OR_BOOLEAN_IF_BOUND_P(s_beg), s_beg, XEN_ARG_2, caller, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_OR_BOOLEAN_IF_BOUND_P(s_end), s_end, XEN_ARG_3, caller, "a number");
  ASSERT_CHANNEL(S_map_chan, snd, chn, 5); 
  ss = get_global_state();
  cp = get_cp(snd, chn, caller);
  beg = XEN_TO_C_INT_OR_ELSE_WITH_CALLER(s_beg, 0, caller);
  end = XEN_TO_C_INT_OR_ELSE_WITH_CALLER(s_end, 0, caller);
  if (end == 0) end = to_c_edit_samples(cp, edpos, caller, 7) - 1;
  num = end - beg + 1;
  if (num > 0)
    {
      errmsg = procedure_ok(proc, 1, caller, "", 1);
      if (errmsg)
	{
	  errstr = C_TO_XEN_STRING(errmsg);
	  FREE(errmsg);
	  return(snd_bad_arity_error(caller, errstr, proc));
	}
      sp = cp->sound;
      reporting = (num > MAX_BUFFER_SIZE);
      if (reporting) start_progress_report(sp, NOT_FROM_ENVED);
      sf = init_sample_read_any(beg, cp, READ_FORWARD, to_c_edit_position(cp, edpos, caller, 7));
      if (sf == NULL) return(XEN_TRUE);
      rpt4 = MAX_BUFFER_SIZE / 4;
      filename = snd_tempnam(ss);
      outgen = mus_make_sample2file(filename, 1, MUS_OUT_FORMAT, MUS_NEXT);
      j = 0;
      for (kp = 0; kp < num; kp++)
	{
	  res = XEN_CALL_1(proc, 
		      C_TO_XEN_DOUBLE((double)next_sample_to_float(sf)),
		      caller);
	  if (XEN_NUMBER_P(res))                         /* one number -> replace current sample */
	    mus_outa(j++, XEN_TO_C_DOUBLE(res), (mus_output *)outgen);
	  else
	    {
	      if (XEN_NOT_FALSE_P(res))                  /* if #f, no output on this pass */
		{
		  if (XEN_TRUE_P(res))                   /* if #t we halt the entire map */
		    break;
		  else
		    {
		      if (XEN_VECTOR_P(res))
			{
			  len = XEN_VECTOR_LENGTH(res);
			  data = XEN_VECTOR_ELEMENTS(res);
			  for (i = 0; i < len; i++) 
			    mus_outa(j++, XEN_TO_C_DOUBLE(data[i]), (mus_output *)outgen);
			}
		      else
			{
			  if (VCT_P(res))
			    {
			      v = TO_VCT(res);
			      for (i = 0; i < v->length; i++) 
				mus_outa(j++, v->data[i], (mus_output *)outgen);
			    }
			  else
			    {
			      if (XEN_LIST_P(res))
				{
				  len = XEN_LIST_LENGTH(res);
				  for (i = 0; i < len; i++, res = XEN_CDR(res)) 
				    mus_outa(j++, XEN_TO_C_DOUBLE(XEN_CAR(res)), (mus_output *)outgen);
				}
			    }
			}
		    }
		}
	    }
	  if (reporting) 
	    {
	      rpt++;
	      if (rpt > rpt4)
		{
		  progress_report(sp, caller, 1, 1, (Float)kp / (Float)num, NOT_FROM_ENVED);
		  rpt = 0;		    
		}
	    }
	  if (ss->stopped_explicitly) 
	    break;
	}
      /* mus_close_file(outgen); */
      if (outgen) mus_free(outgen);
      if (sf) sf = free_snd_fd(sf);
      if (reporting) finish_progress_report(sp, NOT_FROM_ENVED);
      if (ss->stopped_explicitly) 
	ss->stopped_explicitly = 0;
      else
	{
	  if (j == num)
	    file_change_samples(beg, j, filename, cp, 0, DELETE_ME, LOCK_MIXES, caller);
	  else
	    {
	      cured = cp->edit_ctr;
	      delete_samples(beg, num, cp, caller);
	      file_insert_samples(beg, j, filename, cp, 0, DELETE_ME, caller);
	      backup_edit_list(cp);
	      if (cp->edit_ctr > cured)
		backup_edit_list(cp);
	      if (cp->marks) 
		ripple_trailing_marks(cp, beg, num, j);
	    }
	  update_graph(cp, NULL);
	}
      FREE(filename);
    }
  return(res);
}

static XEN g_sp_scan(XEN proc, XEN s_beg, XEN s_end, XEN snd, XEN chn, 
		     const char *caller, int counting, XEN edpos, int arg_pos)
{
  snd_state *ss;
  chan_info *cp;
  int beg, end;
  snd_info *sp;
  snd_fd *sf;
  XEN errstr;
  int kp, len, num, reporting = 0, rpt = 0, rpt4, counts = 0;
  XEN res;
  char *errmsg;

  XEN_ASSERT_TYPE((XEN_PROCEDURE_P(proc)), proc, XEN_ARG_1, caller, "a procedure");
  XEN_ASSERT_TYPE(XEN_NUMBER_OR_BOOLEAN_IF_BOUND_P(s_beg), s_beg, XEN_ARG_2, caller, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_OR_BOOLEAN_IF_BOUND_P(s_end), s_end, XEN_ARG_3, caller, "a number");
  ASSERT_CHANNEL(caller, snd, chn, 4);
  cp = get_cp(snd, chn, caller);
  beg = XEN_TO_C_INT_OR_ELSE_WITH_CALLER(s_beg, 0, caller);
  end = XEN_TO_C_INT_OR_ELSE_WITH_CALLER(s_end, 0, caller);

  errmsg = procedure_ok(proc, 1, caller, "", 1);
  if (errmsg)
    {
      errstr = C_TO_XEN_STRING(errmsg);
      FREE(errmsg);
      return(snd_bad_arity_error(caller, errstr, proc));
    }
  ss = get_global_state();
  sp = cp->sound;
  sf = init_sample_read_any(beg, cp, READ_FORWARD, to_c_edit_position(cp, edpos, caller, arg_pos));
  if (sf == NULL) return(XEN_TRUE);
  rpt4 = MAX_BUFFER_SIZE / 4;
  len = to_c_edit_samples(cp, edpos, caller, arg_pos);
  if (end >= len) end = len - 1;
  if (end == 0) end = len - 1;
  num = end - beg + 1;
  if (num > 0)
    {
      reporting = (num > MAX_BUFFER_SIZE);
      if (reporting) start_progress_report(sp, NOT_FROM_ENVED);
      for (kp = 0; kp < num; kp++)
	{
	  res = XEN_CALL_1(proc,
			   C_TO_XEN_DOUBLE((double)next_sample_to_float(sf)),
			   caller);
	  if (XEN_NOT_FALSE_P(res))
	    {
	      if ((counting) &&
		  (XEN_TRUE_P(res)))
		counts++;
	      else
		{
		  sf = free_snd_fd(sf);
		  if (reporting) 
		    finish_progress_report(sp, NOT_FROM_ENVED);
		  return(XEN_LIST_2(res,
				    C_TO_XEN_INT(kp + beg)));
		}
	    }
	  if (reporting) 
	    {
	      rpt++;
	      if (rpt > rpt4)
		{
		  progress_report(sp, caller, 1, 1, (Float)kp / (Float)num, NOT_FROM_ENVED);
		  rpt = 0;
		}
	    }
	  if (ss->stopped_explicitly)
	    {
	      ss->stopped_explicitly = 0;
	      if (reporting) finish_progress_report(sp, NOT_FROM_ENVED);
	      report_in_minibuffer(sp, "C-G stopped %s at sample %d", 
				   caller, kp + beg);
	      sf = free_snd_fd(sf);
	      if (counting)
		return(C_TO_XEN_INT(counts));
	      return(XEN_FALSE);
	    }
	}
      if (reporting) finish_progress_report(sp, NOT_FROM_ENVED);
    }
  if (sf) sf = free_snd_fd(sf);
  if (counting)
    return(C_TO_XEN_INT(counts));
  return(XEN_FALSE);
}

static XEN g_scan_chan(XEN proc, XEN beg, XEN end, XEN snd, XEN chn, XEN edpos) 
{ 
  #define H_scan_chan "(" S_scan_chan " func &optional (start 0) end snd chn edpos)\n\
apply func to samples in current channel (or the specified channel) \
func is a function of one argument, the current sample. \
if func returns non-#f, the scan stops, and the value is returned to the caller with the sample number. \n\
  (scan-chan (lambda (x) (> x .1)))"

  ASSERT_CHANNEL(S_scan_chan, snd, chn, 4); 
  return(g_sp_scan(proc, beg, end, snd, chn, S_scan_chan, FALSE, edpos, 6));
}

static XEN g_find(XEN expr, XEN sample, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_find "(" S_find " func &optional (start-samp 0) snd chn edpos) applies func, a function of one argument, \
the current sample, to each sample in snd's channel chn, starting at 'start-samp' until func returns #t"

  /* no free here -- it's handled as ss->search_expr in snd-find.c */
  ASSERT_CHANNEL(S_find, snd_n, chn_n, 3);
  return(g_sp_scan(expr, sample, XEN_FALSE, snd_n, chn_n, S_find, FALSE, edpos, 5));
}

static XEN g_count_matches(XEN expr, XEN sample, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_count_matches "(" S_count_matches " func &optional (start-samp 0) snd chn edpos) returns how many \
samples satisfy func (a function of one argument, the current sample, returning #t upon match)"

  ASSERT_CHANNEL(S_count_matches, snd_n, chn_n, 3);
  return(g_sp_scan(expr, sample, XEN_FALSE, snd_n, chn_n, S_count_matches, TRUE, edpos, 5));
}

static XEN g_smooth_sound(XEN beg, XEN num, XEN snd_n, XEN chn_n)
{
  #define H_smooth_sound "(" S_smooth_sound " start-samp samps &optional snd chn) smooths data from start-samp for samps in snd's channel chn"
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(beg), beg, XEN_ARG_1, S_smooth_sound, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(num), num, XEN_ARG_2, S_smooth_sound, "a number");
  ASSERT_CHANNEL(S_smooth_sound, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_smooth_sound);
  cos_smooth(cp,
	     XEN_TO_C_INT_OR_ELSE(beg, 0),
	     XEN_TO_C_INT_OR_ELSE(num, 0),
	     FALSE,
	     S_smooth_sound); 
  return(XEN_TRUE);
}

static XEN g_smooth_selection(void)
{
  #define H_smooth_selection "(" S_smooth_selection ") smooths the data in the currently selected portion"
  chan_info *cp;
  if (selection_is_active() == 0) 
    return(snd_no_active_selection_error(S_smooth_selection));
  cp = get_cp(XEN_FALSE, XEN_FALSE, S_smooth_selection);
  cos_smooth(cp, 0, 0, TRUE, S_smooth_selection);
  return(XEN_TRUE);
}

static XEN g_reverse_sound(XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_reverse_sound "(" S_reverse_sound " &optional snd chn edpos) reverses snd's channel chn"
  chan_info *cp;
  ASSERT_CHANNEL(S_reverse_sound, snd_n, chn_n, 1);
  cp = get_cp(snd_n, chn_n, S_reverse_sound);
  reverse_sound(cp, FALSE, edpos, 3);
  return(XEN_FALSE);
}

static XEN g_reverse_selection(void)
{
  #define H_reverse_selection "(" S_reverse_selection ") reverses the data in the currently selected portion"
  chan_info *cp;
  if (selection_is_active() == 0) 
    return(snd_no_active_selection_error(S_reverse_selection));
  cp = get_cp(XEN_FALSE, XEN_FALSE, S_reverse_selection);
  reverse_sound(cp, TRUE, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0);
  return(XEN_FALSE);
}

static XEN g_insert_silence(XEN beg, XEN num, XEN snd, XEN chn)
{
  #define H_insert_silence "(" S_insert_silence " beg num snd chn) inserts num zeros at beg in snd's chn"
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(beg), beg, XEN_ARG_1, S_insert_silence, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(num), num, XEN_ARG_2, S_insert_silence, "a number");
  ASSERT_CHANNEL(S_insert_silence, snd, chn, 3);
  cp = get_cp(snd, chn, S_insert_silence);
  cursor_insert(cp,
		XEN_TO_C_INT_OR_ELSE(beg, 0),
		XEN_TO_C_INT_OR_ELSE(num, 0),
		S_insert_silence);
  return(beg);
}

static XEN g_swap_channels(XEN snd0, XEN chn0, XEN snd1, XEN chn1, XEN beg, XEN dur)
{
  #define H_swap_channels "(" S_swap_channels " snd0 chn0 snd1 chn1 beg dur) swaps the indicated channels"
  chan_info *cp0 = NULL, *cp1 = NULL;
  snd_fd *c0, *c1;
  int dur0 = 0, dur1 = 0, beg0 = 0, num, old_squelch0, old_squelch1;
  snd_info *sp = NULL;
  env_info *e0, *e1;
  ASSERT_CHANNEL(S_swap_channels, snd0, chn0, 1);
  cp0 = get_cp(snd0, chn0, S_swap_channels);
  if (XEN_INTEGER_P(snd1) && XEN_INTEGER_P(chn1)) 
    {
      ASSERT_CHANNEL(S_swap_channels, snd1, chn1, 3);
      cp1 = get_cp(snd1, chn1, S_swap_channels);
    }
  else
    {
      if (XEN_INTEGER_P(snd1))
	sp = get_sp(snd1);
      else sp = cp0->sound;
      if (sp == NULL) 
	return(snd_no_such_sound_error(S_swap_channels, snd1));
      if (cp0->sound == sp)
	{
	  if ((cp0->chan + 1) < sp->nchans)
	    cp1 = sp->chans[cp0->chan + 1];
	  else cp1 = sp->chans[0];
	}
      else cp1 = sp->chans[0];
    }
  if ((cp0) && (cp1))
    {
      if (XEN_INTEGER_P(beg)) 
	beg0 = XEN_TO_C_INT(beg);
      if (XEN_INTEGER_P(dur)) 
	num = XEN_TO_C_INT(dur);
      else
	{
	  dur0 = current_ed_samples(cp0);
	  dur1 = current_ed_samples(cp1);
	  if (dur0 > dur1) num = dur1; else num = dur0;
	}
      if ((beg0 == 0) && ((num == dur0) || (num == dur1)) &&
	  (no_ed_scalers(cp0)) && (no_ed_scalers(cp1)) &&
	  (cp0->edit_ctr == 0) && (cp1->edit_ctr == 0))
	{
	  /* common special case -- just setup a new ed-list entry with the channels/sounds swapped */
	  old_squelch0 = cp0->squelch_update;
	  old_squelch1 = cp1->squelch_update;
	  e0 = amp_env_copy(cp0, FALSE, cp0->edit_ctr);
	  e1 = amp_env_copy(cp1, FALSE, cp1->edit_ctr);
	  cp0->squelch_update = 1;
	  cp1->squelch_update = 1;
	  file_override_samples(dur1, cp1->sound->filename, cp0, cp1->chan, DONT_DELETE_ME, LOCK_MIXES, S_swap_channels);
	  file_override_samples(dur0, cp0->sound->filename, cp1, cp0->chan, DONT_DELETE_ME, LOCK_MIXES, S_swap_channels);
	  if ((e0) && (e1))
	    {
	      cp0->amp_envs[cp0->edit_ctr] = e1;
	      cp1->amp_envs[cp1->edit_ctr] = e0;
	    }
	  cp0->squelch_update = old_squelch0;
	  cp1->squelch_update = old_squelch1;
	  update_graph(cp0, NULL);
	  update_graph(cp1, NULL);
	}
      else
	{
	  c0 = init_sample_read(beg0, cp0, READ_FORWARD);
	  c1 = init_sample_read(beg0, cp1, READ_FORWARD);
	  swap_channels(cp0->state, beg0, num, c0, c1);
	  free_snd_fd(c0);
	  free_snd_fd(c1);
	}
    }
  return(XEN_FALSE);
}

static XEN g_fht(XEN data)
{
  #define H_fht "(fht vct-obj) returns the Hartley transform of the data in the vct object whose size must be a power of 4"
  vct *v;
  int pow4;
  XEN_ASSERT_TYPE(VCT_P(data), data, XEN_ONLY_ARG, S_fht, "a vct");
  v = TO_VCT(data);
  pow4 = (int)(snd_round(log(v->length) / (log(4))));
  if (((int)(pow(4.0, pow4))) != v->length) 
    mus_misc_error(S_fht,
		   "fht data length must be a power of 4",
		   XEN_LIST_3(C_TO_XEN_INT(v->length),
			      C_TO_XEN_DOUBLE((log(v->length) / (log(4)))),
			      C_TO_XEN_INT((int)(pow(4.0, pow4)))));
  fht(pow4, v->data);
  return(data);
}

static Float *load_Floats(XEN scalers, int *result_len)
{
  int len, i;
  Float *scls;
  XEN lst;
  XEN *vdata;
  if (XEN_VECTOR_P(scalers))
    len = XEN_VECTOR_LENGTH(scalers);
  else
    if (XEN_LIST_P(scalers))
      len = XEN_LIST_LENGTH(scalers);
    else len = 1;
  if (len <= 0) len = 1;
  scls = (Float *)CALLOC(len, sizeof(Float));
  if (XEN_VECTOR_P(scalers))
    {
      vdata = XEN_VECTOR_ELEMENTS(scalers);
      for (i = 0; i < len; i++) 
	scls[i] = (Float)XEN_TO_C_DOUBLE(vdata[i]);
    }
  else
    if (XEN_LIST_P(scalers))
      {
	for (i = 0, lst = scalers; i < len; i++, lst = XEN_CDR(lst)) 
	  scls[i] = (Float)XEN_TO_C_DOUBLE(XEN_CAR(lst));
      }
    else
      if (XEN_NUMBER_P(scalers))
	scls[0] = (Float)XEN_TO_C_DOUBLE(scalers);
      else scls[0] = 1.0;
  result_len[0] = len;
  return(scls);
}

static XEN g_scale_to(XEN scalers, XEN snd_n, XEN chn_n)
{
  #define H_scale_to "(" S_scale_to " norms &optional snd chn)\n\
normalizes snd to norms (following sync) norms can be a float or a vector of floats"

  /* chn_n irrelevant if sync */
  chan_info *cp;
  int len[1];
  Float *scls;
  ASSERT_CHANNEL(S_scale_to, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_scale_to);
  scls = load_Floats(scalers, len);
  scale_to(cp->state, cp->sound, cp, scls, len[0], FALSE); /* last arg for selection */
  FREE(scls);
  return(scalers);
}

static XEN g_scale_by(XEN scalers, XEN snd_n, XEN chn_n)
{
  #define H_scale_by "(" S_scale_by " scalers &optional snd chn)\n\
scales snd by scalers (following sync) scalers can be a float or a vector of floats"

  /* chn_n irrelevant if sync */
  chan_info *cp;
  int len[1];
  Float *scls;
  ASSERT_CHANNEL(S_scale_by, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_scale_by);
  scls = load_Floats(scalers, len);
  scale_by(cp, scls, len[0], FALSE);
  FREE(scls);
  return(scalers);
}

static XEN g_scale_selection_to(XEN scalers)
{
  #define H_scale_selection_to "(" S_scale_selection_to " norms &optional chn) normalizes current selected portion to norms"
  int len[1];
  Float *scls;
  if (selection_is_active())
    {
      scls = load_Floats(scalers, len);
      scale_to(get_global_state(), NULL, NULL, scls, len[0], TRUE);
      FREE(scls);
      return(scalers);
    }
  snd_no_active_selection_error(S_scale_selection_to);
  return(scalers);
}

static XEN g_scale_selection_by(XEN scalers)
{
  #define H_scale_selection_by "(" S_scale_selection_by " scalers &optional chn) scales current selected portion by scalers"
  int len[1];
  Float *scls;
  if (selection_is_active())
    {
      scls = load_Floats(scalers, len);
      scale_by(NULL, scls, len[0], TRUE);
      FREE(scls);
      return(scalers);
    }
  snd_no_active_selection_error(S_scale_selection_by);
  return(scalers);
}

static XEN g_env_selection(XEN edata, XEN base, XEN snd_n, XEN chn_n)
{
  #define H_env_selection "(" S_env_selection " env &optional (env-base 1.0) snd chn)\n\
applies envelope 'env' to the currently selected portion of snd's channel chn using 'env-base' to determine how breakpoints are connected"

  chan_info *cp;
  env *e;
  mus_any *egen;
  ASSERT_CHANNEL(S_env_selection, snd_n, chn_n, 3);
  if (selection_is_active() == 0) 
    return(snd_no_active_selection_error(S_env_selection));
  cp = get_cp(snd_n, chn_n, S_env_selection);
  if (XEN_LIST_P(edata))
    {
      e = get_env(edata, S_env_selection);
      if (e)
	{
	  apply_env(cp, e, 0, 0, 1.0, TRUE, NOT_FROM_ENVED, S_env_selection, 
		    NULL, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0, 
		    XEN_TO_C_DOUBLE_OR_ELSE(base, 1.0));
	  free_env(e);
	  return(edata);
	}
    }
  else
    {
      XEN_ASSERT_TYPE((mus_xen_p(edata)) && (mus_env_p(egen = mus_xen_to_clm(edata))), edata, XEN_ARG_1, S_env_selection, "an env generator or a list");
      apply_env(cp, NULL, 0, 0, 1.0, TRUE, NOT_FROM_ENVED, S_env_selection, egen, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0, 1.0);
      return(edata);
    }
  return(XEN_FALSE);
}

static XEN g_env_sound(XEN edata, XEN samp_n, XEN samps, XEN base, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_env_sound "(" S_env_sound " env &optional (start-samp 0) samps (env-base 1.0) snd chn edpos)\n\
applies amplitude envelope 'env' (a list of breakpoints or a CLM env) to snd's channel chn starting at start-samp, going \
either to the end of the sound or for 'samps' samples, with segments interpolating according to 'env-base'"

  chan_info *cp;
  env *e;
  int beg = 0, dur;
  mus_any *egen;
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samp_n), samp_n, XEN_ARG_2, S_env_sound, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samps), samps, XEN_ARG_3, S_env_sound, "a number");
  ASSERT_CHANNEL(S_env_sound, snd_n, chn_n, 5);
  cp = get_cp(snd_n, chn_n, S_env_sound);
  beg = XEN_TO_C_INT_OR_ELSE(samp_n, 0);
  dur = XEN_TO_C_INT_OR_ELSE(samps, 0);
  if (dur == 0) dur = to_c_edit_samples(cp, edpos, S_env_sound, 7);
  if (XEN_LIST_P(edata))
    {
      e = get_env(edata, S_env_sound);
      if (e)
	{
	  apply_env(cp, e, beg, dur, 1.0, FALSE, NOT_FROM_ENVED, S_env_sound, NULL, edpos, 7, XEN_TO_C_DOUBLE_OR_ELSE(base, 1.0));
	  free_env(e);
	  return(edata);
	}
    }
  else
    {
      XEN_ASSERT_TYPE((mus_xen_p(edata)) && (mus_env_p(egen = mus_xen_to_clm(edata))), edata, XEN_ARG_1, S_env_sound, "an env generator or a list");
      apply_env(cp, NULL, beg, dur, 1.0, FALSE, NOT_FROM_ENVED, S_env_sound, egen, edpos, 7, 1.0);
      return(edata);
    }
  return(XEN_FALSE);
}

static XEN g_fft_1(XEN reals, XEN imag, XEN sign, int use_fft)
{
  vct *v1 = NULL, *v2 = NULL;
  int ipow, n, n2, i, isign = 1, need_free = 0;
  Float *rl, *im;
  XEN *rvdata; XEN *ivdata;
  XEN_ASSERT_TYPE(((VCT_P(reals)) || (XEN_VECTOR_P(reals))), reals, XEN_ARG_1, ((use_fft) ? S_fft : S_convolve_arrays), "a vector or a vct");
  XEN_ASSERT_TYPE(((VCT_P(imag)) || (XEN_VECTOR_P(imag))), imag, XEN_ARG_2, ((use_fft) ? S_fft : S_convolve_arrays), "a vector or a vct");
  if ((VCT_P(reals)) && (VCT_P(imag)))
    {
      v1 = (vct *)XEN_OBJECT_REF(reals);
      v2 = (vct *)XEN_OBJECT_REF(imag);
      n = v1->length;
    }
  else
    n = XEN_VECTOR_LENGTH(reals);
  if (POWER_OF_2_P(n))
    n2 = n;
  else
    {
      ipow = (int)ceil(log((Float)n) / log(2.0));
      n2 = (int)pow(2.0, (Float)ipow);
    }
#if DEBUGGING
  if (n2 < n) abort();
#endif
  if ((!v1) || (n != n2))
    {
      rl = (Float *)CALLOC(n2, sizeof(Float));
      im = (Float *)CALLOC(n2, sizeof(Float));
      need_free = 1;
    }
  else
    {
      rl = v1->data;
      im = v2->data;
    }
  if (XEN_NUMBER_P(sign)) isign = XEN_TO_C_INT_OR_ELSE(sign, 0);
  if (isign == 0) isign = 1;
  if (v1 == NULL)
    {
      rvdata = XEN_VECTOR_ELEMENTS(reals);
      ivdata = XEN_VECTOR_ELEMENTS(imag);
      for (i = 0; i < n; i++)
	{
	  rl[i] = XEN_TO_C_DOUBLE(rvdata[i]);
	  im[i] = XEN_TO_C_DOUBLE(ivdata[i]);
	}
    }
  else
    {
      if (n != n2)
	{
	  for (i = 0; i < n; i++)
	    {
	      rl[i] = v1->data[i];
	      im[i] = v2->data[i];
	    }
	}
    }
  if (use_fft) 
    {
      mus_fft(rl, im, n2, isign);
      if (v1 == NULL)
	{
	  rvdata = XEN_VECTOR_ELEMENTS(reals);
	  ivdata = XEN_VECTOR_ELEMENTS(imag);
	  for (i = 0; i < n; i++)
	    {
	      rvdata[i] = C_TO_XEN_DOUBLE(rl[i]);
	      ivdata[i] = C_TO_XEN_DOUBLE(im[i]);
	    }
	}
      else
	{
	  if (n != n2)
	    {
	      for (i = 0; i < n; i++)
		{
		  v1->data[i] = rl[i];
		  v2->data[i] = im[i];
		}
	    }
	}
    }
  else 
    {
      mus_convolution(rl, im, n2);
      if (v1 == NULL)
	{
	  rvdata = XEN_VECTOR_ELEMENTS(reals);
	  for (i = 0; i < n; i++)
	    rvdata[i] = C_TO_XEN_DOUBLE(rl[i]);
	}
      else
	{
	  if (n != n2)
	    for (i = 0; i < n; i++) 
	      v1->data[i] = rl[i];
	}
    }
  if (need_free)
    {
      FREE(rl);
      FREE(im);
    }
  return(reals);
}

static XEN g_fft(XEN reals, XEN imag, XEN sign)
{
  #define H_fft "(" S_fft " reals imags &optional (sign 1)) ffts the data returning the result in reals. \
If sign is -1, performs inverse fft"

  return(g_fft_1(reals, imag, sign, TRUE));
}

static XEN g_convolve_with(XEN file, XEN new_amp, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_convolve_with "(" S_convolve_with " file &optional (amp 1.0) snd chn edpos)\n\
convolves file with snd's channel chn (or the currently sync'd channels), amp is the resultant peak amp"

  chan_info *cp;
  Float amp;
  XEN errstr;
  char *fname = NULL, *error = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, S_convolve_with, "a string");
  ASSERT_CHANNEL(S_convolve_with, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_convolve_with);
  if (XEN_NUMBER_P(new_amp)) 
    amp = XEN_TO_C_DOUBLE(new_amp);
  else
    {
      if (XEN_FALSE_P(new_amp))
	amp = 0.0;
      else amp = 1.0;
    }
  fname = mus_expand_filename(XEN_TO_C_STRING(file));
  if (mus_file_probe(fname))
    {
      error = convolve_with_or_error(fname, amp, cp, edpos, 5);
      if (error)
	{
	  if (fname) FREE(fname);
	  errstr = C_TO_XEN_STRING(error);
	  FREE(error);
	  mus_misc_error(S_convolve_with, NULL, errstr);
	}
    }
  else 
    {
      if (fname) FREE(fname);
      return(snd_no_such_file_error(S_convolve_with, file));
    }
  if (fname) FREE(fname);
  return(xen_return_first(file, new_amp));
}

static XEN g_snd_spectrum(XEN data, XEN win, XEN len, XEN linear_or_dB)
{
  #define H_snd_spectrum "(" S_snd_spectrum " data window len linear-or-dB)\n\
return magnitude spectrum of data (vct) in data using fft-window win and fft length len"

  int i, j, n, p = 0, n2, linear, wtype;
  Float maxa, todb, lowest, val;
  Float *idat, *rdat, *window;
  vct *v;
  XEN_ASSERT_TYPE((VCT_P(data)), data, XEN_ARG_1, S_snd_spectrum, "a vct");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(win), win, XEN_ARG_2, S_snd_spectrum, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(len), len, XEN_ARG_3, S_snd_spectrum, "an integer");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(linear_or_dB), linear_or_dB, XEN_ARG_4, S_snd_spectrum, "a boolean");
  v = TO_VCT(data);
  rdat = v->data;
  n = XEN_TO_C_INT_OR_ELSE(len, v->length);
  if (n <= 0)
    mus_misc_error(S_snd_spectrum, "length <= 0?", len);
  if (n > v->length) n = v->length;
  if (XEN_NOT_FALSE_P(linear_or_dB)) linear = 1; else linear = 0;
  wtype = XEN_TO_C_INT_OR_ELSE(win, MUS_RECTANGULAR_WINDOW);
  if (!(MUS_FFT_WINDOW_OK(wtype)))
    mus_misc_error(S_snd_spectrum, "unknown fft window", win);
  idat = (Float *)CALLOC(n, sizeof(Float));
  window = (Float *)CALLOC(n, sizeof(Float));
  make_fft_window_1(window, n, wtype, 0.0);
  for (i = 0; i < n; i++) rdat[i] *= window[i];
  FREE(window);

  p = (int)(log(n) / log(4.0));
  if (n == (int)pow(4, p))
    {
      fht(p, rdat);
      rdat[0] *= rdat[0];
      n2 = n / 2;
      rdat[n2] *= rdat[n2];
      for (i = 1, j = n - 1; i < n2; i++, j--)
	{
	  rdat[i] = 0.5 * ((rdat[i] * rdat[i]) + (rdat[j] * rdat[j]));
	  rdat[j] = rdat[i];
	}
    }
  else
    {
      mus_fft(rdat, idat, n, 1);
      rdat[0] *= rdat[0];
      n2 = n / 2;
      rdat[n2] *= rdat[n2];
      for (i = 1, j = n - 1; i < n2; i++, j--)
	{
	  rdat[i] = rdat[i] * rdat[i] + idat[i] * idat[i];
	  rdat[j] = rdat[i];
	}
    }

  lowest = 0.00000001;
  maxa = 0.0;
  n = n / 2;
  for (i = 0; i < n; i++)
    {
      val = rdat[i];
      if (val < lowest)
	idat[i] = 0.0;
      else 
	{
	  idat[i] = sqrt(val);
	  if (idat[i] > maxa) maxa = idat[i];
	}
    }
  if (maxa > 0.0)
    {
      maxa = 1.0 / maxa;
      if (linear == 0) /* dB */
	{
	  todb = 20.0 / log(10.0);
	  for (i = 0; i < n; i++) 
	    if (idat[i] > 0.0)
	      idat[i] = todb * log(idat[i] * maxa);
	    else idat[i] = -90.0;
	}
      else 
	for (i = 0; i < n; i++) 
	  idat[i] *= maxa;
    }
  return(xen_return_first(make_vct(n, idat), data));
}

static XEN g_convolve_selection_with(XEN file, XEN new_amp)
{
  #define H_convolve_selection_with "(" S_convolve_selection_with " file &optional (amp 1.0))\n\
convolves the current selection with file; amp is the resultant peak amp"

  Float amp;
  XEN errstr;
  char *fname = NULL, *error;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, S_convolve_selection_with, "a string");
  if (selection_is_active() == 0) 
    return(snd_no_active_selection_error(S_convolve_selection_with));
  if (XEN_NUMBER_P(new_amp)) 
    amp = XEN_TO_C_DOUBLE(new_amp);
  else
    {
      if (XEN_FALSE_P(new_amp))
	amp = 0.0;
      else amp = 1.0;
    }
  fname = mus_expand_filename(XEN_TO_C_STRING(file));
  if (mus_file_probe(fname))
    {
      error = convolve_with_or_error(fname, amp, NULL, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0);
      if (error)
	{
	  if (fname) FREE(fname);
	  errstr = C_TO_XEN_STRING(error);
	  FREE(error);
	  mus_misc_error(S_convolve_selection_with, NULL, errstr);
	}
    }
  else 
    {
      if (fname) FREE(fname);
      return(snd_no_such_file_error(S_convolve_selection_with, file));
    }
  if (fname) FREE(fname);
  return(xen_return_first(file, new_amp));
}

static XEN g_convolve(XEN reals, XEN imag)
{
  #define H_convolve "(" S_convolve_arrays " rl1 rl2) convolves vectors or vcts rl1 and rl2, result in rl1 (which needs to be big enough)"
  /* if reals is a string = filename and imag is a Float (or nada), assume user missppelledd convolve-with */
  if (XEN_STRING_P(reals))
    return(g_convolve_with(reals, imag, XEN_FALSE, XEN_FALSE, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION)));
  /* result in reals (which needs to be big enough and zero padded) */
  else return(g_fft_1(reals, imag, C_TO_SMALL_XEN_INT(1), FALSE));
}

static Float check_src_envelope(env *e, char *caller)
{
  /* can't go through zero here, and if negative need to return 1.0 */
  int i;
  Float res = 0.0;
  for (i = 0; i < (2 * e->pts); i += 2)
    {

      if (e->data[i + 1] == 0.0)
	mus_misc_error(caller, "src envelope hits 0.0", env_to_xen(e));
      else
	{
	  if (e->data[i + 1] < 0.0)
	    {
	      if (res <= 0.0)
		res = -1.0;
	      else mus_misc_error(caller, "src envelope passes through 0.0", env_to_xen(e));
	    }
	  else
	    {
	      if (res >= 0)
		res = 1.0;
	      else mus_misc_error(caller, "src envelope passes through 0.0", env_to_xen(e));
	    }
	}
    }
  return(res);
}

static XEN g_src_sound(XEN ratio_or_env, XEN base, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_src_sound "(" S_src_sound " ratio-or-env &optional (base 1.0) snd chn edpos)\n\
sampling-rate converts snd's channel chn by ratio, or following an envelope. Negative ratio reverses the sound"

  chan_info *cp;
  env *e = NULL;
  mus_any *egen;
  Float e_ratio = 1.0;
  ASSERT_CHANNEL(S_src_sound, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_src_sound);
  if (XEN_NUMBER_P(ratio_or_env))
    src_env_or_num(cp->state, cp, NULL, XEN_TO_C_DOUBLE(ratio_or_env), 
		   TRUE, NOT_FROM_ENVED, S_src_sound,
		   FALSE, NULL, edpos, 5, 1.0);
  else 
    {
      if (XEN_LIST_P(ratio_or_env))
	{
	  e = get_env(ratio_or_env, S_src_sound);
	  e_ratio = check_src_envelope(e, S_src_sound);
	  src_env_or_num(cp->state, cp,
			 e, e_ratio,
			 FALSE, NOT_FROM_ENVED, S_src_sound, 
			 FALSE, NULL, edpos, 5, 
			 XEN_TO_C_DOUBLE_OR_ELSE(base, 1.0));
	  if (e) free_env(e);
	}
      else
	{
	  XEN_ASSERT_TYPE((mus_xen_p(ratio_or_env)) && (mus_env_p(egen = mus_xen_to_clm(ratio_or_env))), 
		      ratio_or_env, XEN_ARG_1, S_src_sound, "a number, list, or env generator");
	  src_env_or_num(cp->state, cp, NULL, 
			 (mus_phase(egen) >= 0.0) ? 1.0 : -1.0,
			 FALSE, NOT_FROM_ENVED, S_src_sound, 
			 FALSE, egen, edpos, 5, 1.0);
	}
    }
  return(xen_return_first(ratio_or_env, base));
}

static XEN g_src_selection(XEN ratio_or_env, XEN base)
{
  #define H_src_selection "(" S_src_selection " ratio-or-env &optional (base 1.0) edpos)\n\
sampling-rate converts the currently selected data by ratio (which can be an envelope)"
  env *e = NULL;
  mus_any *egen;
  Float e_ratio = 1.0;
  chan_info *cp;
  if (selection_is_active() == 0) 
    return(snd_no_active_selection_error(S_src_selection));
  cp = get_cp(XEN_FALSE, XEN_FALSE, S_src_selection);
  if (XEN_NUMBER_P(ratio_or_env))
    src_env_or_num(cp->state, cp, 
		   NULL, 
		   XEN_TO_C_DOUBLE(ratio_or_env), 
		   TRUE, NOT_FROM_ENVED, S_src_selection, TRUE, NULL, 
		   C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0, 1.0);
  else 
    {
      if (XEN_LIST_P(ratio_or_env))
	{
	  e = get_env(ratio_or_env, S_src_selection);
	  e_ratio = check_src_envelope(e, S_src_selection);
	  src_env_or_num(cp->state, cp,
			 e, e_ratio, FALSE, NOT_FROM_ENVED, S_src_selection, 
			 TRUE, NULL, 
			 C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0, 
			 XEN_TO_C_DOUBLE_OR_ELSE(base, 1.0));
	  if (e) free_env(e);
	}
      else
	{
	  XEN_ASSERT_TYPE((mus_xen_p(ratio_or_env)) && 
			  (mus_env_p(egen = mus_xen_to_clm(ratio_or_env))), 
			  ratio_or_env, XEN_ARG_1, S_src_selection, "a number, list, or env generator");
	  src_env_or_num(cp->state, cp, NULL,
			 (mus_phase(egen) >= 0.0) ? 1.0 : -1.0,
			 FALSE, NOT_FROM_ENVED, S_src_selection, 
			 TRUE, egen, 
			 C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0, 1.0);
	}
    }
  return(xen_return_first(ratio_or_env, base));
}

static XEN g_filter_sound(XEN e, XEN order, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_filter_sound "(" S_filter_sound " filter order &optional snd chn edpos)\n\
applies FIR filter to snd's channel chn. 'filter' is either the frequency response envelope, a CLM filter, or a vct object with the actual coefficients"

  chan_info *cp;
  vct *v;
  int len;
  XEN errstr;
  env *ne = NULL;
  char *error;
  ASSERT_CHANNEL(S_filter_sound, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_filter_sound);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(order), order, XEN_ARG_2, S_filter_sound, "an integer");
  if (mus_xen_p(e))
    {
      error = apply_filter_or_error(cp, 0, NULL, NOT_FROM_ENVED, S_filter_sound, FALSE, NULL, mus_xen_to_clm(e), edpos, 5);
      if (error)
	{
	  errstr = C_TO_XEN_STRING(error);
	  FREE(error);
	  mus_misc_error(S_filter_sound, NULL, errstr);
	}
    }
  else
    {
      len = XEN_TO_C_INT_OR_ELSE(order, 0);
      if (len <= 0) 
	mus_misc_error(S_filter_sound, "order <= 0?", order);
      if (VCT_P(e)) /* the filter coefficients direct */
	{
	  v = TO_VCT(e);
	  if (len > v->length) 
	    mus_misc_error(S_filter_sound, "order > length coeffs?", XEN_LIST_2(order, e));
	  apply_filter(cp, len, NULL, NOT_FROM_ENVED, S_filter_sound, FALSE, v->data, NULL, edpos, 5);
	}
      else 
	{
	  XEN_ASSERT_TYPE((XEN_VECTOR_P(e) || (XEN_LIST_P(e))), e, XEN_ARG_1, S_filter_sound, "a list, vector, vct, or env generator");
	  apply_filter(cp, len,
		       ne = get_env(e, S_filter_sound),
		       NOT_FROM_ENVED, S_filter_sound, FALSE, NULL, NULL, edpos, 5);
	  if (ne) free_env(ne); 
	}
    }
  return(xen_return_first(XEN_TRUE, e));
}

static XEN g_filter_selection(XEN e, XEN order)
{
  #define H_filter_selection "(" S_filter_selection " filter order) applies filter to current selection"
  chan_info *cp;
  char *error;
  vct *v;
  int len;
  XEN errstr;
  env *ne = NULL;
  if (selection_is_active() == 0) 
    return(snd_no_active_selection_error(S_filter_selection));
  cp = get_cp(XEN_FALSE, XEN_FALSE, S_filter_selection);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(order), order, XEN_ARG_2, S_filter_selection, "an integer");
  if (mus_xen_p(e))
    {
      error = apply_filter_or_error(cp, 0, NULL, NOT_FROM_ENVED, S_filter_selection, 
				    TRUE, NULL, mus_xen_to_clm(e), 
				    C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION) ,0);
      if (error)
	{
	  errstr = C_TO_XEN_STRING(error);
	  FREE(error);
	  mus_misc_error(S_filter_selection, NULL, errstr);
	}
    }
  else
    {
      len = XEN_TO_C_INT_OR_ELSE(order, 0);
      if (len <= 0) 
	mus_misc_error(S_filter_selection, "order <= 0?", order);
      if (VCT_P(e)) /* the filter coefficients direct */
	{
	  v = TO_VCT(e);
	  if (len > v->length) 
	    mus_misc_error(S_filter_selection, "order > length coeffs?", XEN_LIST_2(order, e));
	  apply_filter(cp, len, NULL, NOT_FROM_ENVED, S_filter_selection, TRUE, v->data, NULL, 
		       C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0);
	}
      else 
	{
	  XEN_ASSERT_TYPE((XEN_VECTOR_P(e) || (XEN_LIST_P(e))), e, XEN_ARG_1, S_filter_selection, "a list, vector, vct, or env generator");
	  apply_filter(cp, len,
		       ne = get_env(e, S_filter_selection),
		       NOT_FROM_ENVED, S_filter_selection, TRUE, NULL, NULL, 
		       C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0); 
	  if (ne) free_env(ne);
	}
    }
  return(xen_return_first(XEN_TRUE, e));
}

#ifdef XEN_ARGIFY_1
XEN_ARGIFY_6(g_scan_chan_w, g_scan_chan)
XEN_ARGIFY_7(g_map_chan_w, g_map_chan)
XEN_ARGIFY_5(g_find_w, g_find)
XEN_ARGIFY_5(g_count_matches_w, g_count_matches)
XEN_ARGIFY_4(g_smooth_sound_w, g_smooth_sound)
XEN_NARGIFY_0(g_smooth_selection_w, g_smooth_selection)
XEN_ARGIFY_3(g_reverse_sound_w, g_reverse_sound)
XEN_NARGIFY_0(g_reverse_selection_w, g_reverse_selection)
XEN_ARGIFY_6(g_swap_channels_w, g_swap_channels)
XEN_ARGIFY_4(g_insert_silence_w, g_insert_silence)
XEN_NARGIFY_1(g_fht_w, g_fht)
XEN_ARGIFY_1(g_scale_selection_to_w, g_scale_selection_to)
XEN_ARGIFY_1(g_scale_selection_by_w, g_scale_selection_by)
XEN_ARGIFY_3(g_scale_to_w, g_scale_to)
XEN_ARGIFY_3(g_scale_by_w, g_scale_by)
XEN_ARGIFY_4(g_env_selection_w, g_env_selection)
XEN_ARGIFY_7(g_env_sound_w, g_env_sound)
XEN_ARGIFY_3(g_fft_w, g_fft)
XEN_ARGIFY_4(g_snd_spectrum_w, g_snd_spectrum)
XEN_ARGIFY_2(g_convolve_w, g_convolve)
XEN_ARGIFY_5(g_convolve_with_w, g_convolve_with)
XEN_ARGIFY_2(g_convolve_selection_with_w, g_convolve_selection_with)
XEN_ARGIFY_5(g_src_sound_w, g_src_sound)
XEN_ARGIFY_2(g_src_selection_w, g_src_selection)
XEN_ARGIFY_5(g_filter_sound_w, g_filter_sound)
XEN_ARGIFY_2(g_filter_selection_w, g_filter_selection)
#else
#define g_scan_chan_w g_scan_chan
#define g_map_chan_w g_map_chan
#define g_find_w g_find
#define g_count_matches_w g_count_matches
#define g_smooth_sound_w g_smooth_sound
#define g_smooth_selection_w g_smooth_selection
#define g_reverse_sound_w g_reverse_sound
#define g_reverse_selection_w g_reverse_selection
#define g_swap_channels_w g_swap_channels
#define g_insert_silence_w g_insert_silence
#define g_fht_w g_fht
#define g_scale_selection_to_w g_scale_selection_to
#define g_scale_selection_by_w g_scale_selection_by
#define g_scale_to_w g_scale_to
#define g_scale_by_w g_scale_by
#define g_env_selection_w g_env_selection
#define g_env_sound_w g_env_sound
#define g_fft_w g_fft
#define g_snd_spectrum_w g_snd_spectrum
#define g_convolve_w g_convolve
#define g_convolve_with_w g_convolve_with
#define g_convolve_selection_with_w g_convolve_selection_with
#define g_src_sound_w g_src_sound
#define g_src_selection_w g_src_selection
#define g_filter_sound_w g_filter_sound
#define g_filter_selection_w g_filter_selection
#endif

void g_init_sig(void)
{
  XEN_DEFINE_PROCEDURE(S_scan_chan,               g_scan_chan_w, 1, 5, 0,               H_scan_chan);
  XEN_DEFINE_PROCEDURE(S_map_chan,                g_map_chan_w, 1, 6, 0,                H_map_chan);
  XEN_DEFINE_PROCEDURE(S_find,                    g_find_w, 1, 4, 0,                    H_find);
  XEN_DEFINE_PROCEDURE(S_count_matches,           g_count_matches_w, 1, 4, 0,           H_count_matches);

  XEN_DEFINE_PROCEDURE(S_smooth_sound,            g_smooth_sound_w, 2, 2, 0,            H_smooth_sound);
  XEN_DEFINE_PROCEDURE(S_smooth_selection,        g_smooth_selection_w, 0, 0, 0,        H_smooth_selection);
  XEN_DEFINE_PROCEDURE(S_reverse_sound,           g_reverse_sound_w, 0, 3, 0,           H_reverse_sound);
  XEN_DEFINE_PROCEDURE(S_reverse_selection,       g_reverse_selection_w, 0, 0, 0,       H_reverse_selection);
  XEN_DEFINE_PROCEDURE(S_swap_channels,           g_swap_channels_w, 0, 6, 0,           H_swap_channels);
  XEN_DEFINE_PROCEDURE(S_insert_silence,          g_insert_silence_w, 2, 2, 0,          H_insert_silence);
  XEN_DEFINE_PROCEDURE(S_fht,                     g_fht_w, 1, 0, 0,                     H_fht);

  XEN_DEFINE_PROCEDURE(S_scale_selection_to,      g_scale_selection_to_w, 0, 1, 0,      H_scale_selection_to);
  XEN_DEFINE_PROCEDURE(S_scale_selection_by,      g_scale_selection_by_w, 0, 1, 0,      H_scale_selection_by);
  XEN_DEFINE_PROCEDURE(S_scale_to,                g_scale_to_w, 0, 3, 0,                H_scale_to);
  XEN_DEFINE_PROCEDURE(S_scale_by,                g_scale_by_w, 0, 3, 0,                H_scale_by);
  XEN_DEFINE_PROCEDURE(S_env_selection,           g_env_selection_w, 1, 3, 0,           H_env_selection);
  XEN_DEFINE_PROCEDURE(S_env_sound,               g_env_sound_w, 1, 6, 0,               H_env_sound);
  XEN_DEFINE_PROCEDURE(S_fft,                     g_fft_w, 2, 1, 0,                     H_fft);
  XEN_DEFINE_PROCEDURE(S_snd_spectrum,            g_snd_spectrum_w, 1, 3, 0,            H_snd_spectrum);
  XEN_DEFINE_PROCEDURE(S_convolve_arrays,         g_convolve_w, 1, 1, 0,                H_convolve);
  XEN_DEFINE_PROCEDURE(S_convolve_with,           g_convolve_with_w, 1, 4, 0,           H_convolve_with);
  XEN_DEFINE_PROCEDURE(S_convolve_selection_with, g_convolve_selection_with_w, 1, 1, 0, H_convolve_selection_with);
  XEN_DEFINE_PROCEDURE(S_src_sound,               g_src_sound_w, 1, 4, 0,               H_src_sound);
  XEN_DEFINE_PROCEDURE(S_src_selection,           g_src_selection_w, 1, 1, 0,           H_src_selection);
  XEN_DEFINE_PROCEDURE(S_filter_sound,            g_filter_sound_w, 1, 4, 0,            H_filter_sound);
  XEN_DEFINE_PROCEDURE(S_filter_selection,        g_filter_selection_w, 1, 1, 0,        H_filter_selection);

}


/* TODO: there's a lot of inconsistency in the *-sound|selection arguments:
 *          convolve-with file amp s c e
 *          env-sound env beg dur s c e
 *          filter-sound env order s c e
 *          map|scan-chan func beg end origin s c e
 *          play beg s c sync end e
 *          reverse-sound s c e
 *          scale-sound-to|by scl beg dur s c
 *          set-samples beg dur data s c trunc fchan
 *          smooth-sound beg dur s c
 *          src-sound num base s c e
 *          swap-channels s1 c1 s2 c2 beg dur
 */
