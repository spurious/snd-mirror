#include "snd.h"
#include "clm2xen.h"
#include "clm-strings.h"

/* collect syncd chans */
typedef struct {
  sync_info *si;
  snd_fd **sfs;
  off_t dur;
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
  /* also remember that there might be no extension language */
#if (!HAVE_EXTENSION_LANGUAGE)
  return(cp->edit_ctr);
#endif
  XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(edpos) || XEN_INTEGER_P(edpos) || XEN_PROCEDURE_P(edpos) || XEN_BOOLEAN_P(edpos), 
		  edpos, arg_pos, caller, "an integer or a procedure");
  if (XEN_PROCEDURE_P(edpos))
    {
      errmsg = procedure_ok(edpos, 2, caller, "edit position", arg_pos);
      if (errmsg)
	{
	  errstr = C_TO_XEN_STRING(errmsg);
	  FREE(errmsg);
	  snd_bad_arity_error(caller, errstr, edpos);
	  return(AT_CURRENT_EDIT_POSITION);
	}
      pos = XEN_TO_C_INT_OR_ELSE_WITH_CALLER(XEN_CALL_2(edpos, 
							C_TO_SMALL_XEN_INT(cp->sound->index), 
							C_TO_SMALL_XEN_INT(cp->chan),
							caller),
					     AT_CURRENT_EDIT_POSITION, caller);
    }
  else pos = XEN_TO_C_INT_OR_ELSE_WITH_CALLER(edpos, AT_CURRENT_EDIT_POSITION, caller);
  if (pos == AT_CURRENT_EDIT_POSITION)
    return(cp->edit_ctr);
  if ((pos < 0) || 
      (pos >= cp->edit_size) ||
      (cp->edits[pos] == NULL))
    XEN_ERROR(NO_SUCH_EDIT,
	      XEN_LIST_3(C_TO_XEN_STRING(caller),
			 C_TO_XEN_STRING("edpos: ~A (from ~A), sound index: ~A (~A), chan: ~A, current edit: ~A"),
			 XEN_LIST_6(C_TO_XEN_INT(pos),
				    edpos,
				    C_TO_XEN_INT(cp->sound->index),
				    C_TO_XEN_STRING(cp->sound->short_filename),
				    C_TO_XEN_INT(cp->chan),
				    C_TO_XEN_INT(cp->edit_ctr))));
  return(pos);
}

off_t to_c_edit_samples(chan_info *cp, XEN edpos, const char *caller, int arg_pos)
{
  return(cp->samples[to_c_edit_position(cp, edpos, caller, arg_pos)]);
}

off_t beg_to_sample(XEN beg, const char *caller)
{
  off_t start;
  start = XEN_TO_C_OFF_T_OR_ELSE(beg, 0);
  if (start < 0) XEN_ERROR(NO_SUCH_SAMPLE,
			   XEN_LIST_2(C_TO_XEN_STRING(caller),
				      beg));
  return(start);
}

off_t dur_to_samples(XEN dur, off_t beg, chan_info *cp, int edpos, int argn, const char *caller)
{
  off_t samps;
  samps = XEN_TO_C_OFF_T_OR_ELSE(dur, cp->samples[edpos] - beg);
  if (samps < 0)
    XEN_WRONG_TYPE_ARG_ERROR(caller, argn, dur, "a positive integer");
  return(samps);
}

static off_t end_to_sample(XEN end, chan_info *cp, int edpos, const char *caller)
{
  off_t last;
  last = XEN_TO_C_OFF_T_OR_ELSE(end, cp->samples[edpos] - 1);
  if (last < 0) 
    XEN_ERROR(NO_SUCH_SAMPLE,
	      XEN_LIST_2(C_TO_XEN_STRING(caller),
			 end));
  return(last);
}


static sync_state *get_sync_state_1(snd_info *sp, chan_info *cp, off_t beg, bool regexpr, 
				    read_direction_t forwards, off_t prebeg, XEN edpos, const char *caller, int arg_pos)
{
  /* can return NULL if regexpr and no current selection */
  sync_info *si = NULL;
  snd_fd **sfs = NULL;
  chan_info *ncp;
  int i, pos;
  off_t dur, pbeg;
  sync_state *sc;
  dur = 0;
  if ((!regexpr) && (sp == NULL)) return(NULL);
  if ((!regexpr) && (sp->sync != 0))
    {
      si = snd_sync(sp->sync);
      sfs = (snd_fd **)MALLOC(si->chans * sizeof(snd_fd *));
      for (i = 0; i < si->chans; i++) 
	{
	  ncp = si->cps[i];
	  si->begs[i] = beg;
	  pos = to_c_edit_position(ncp, edpos, caller, arg_pos);
	  if (forwards == READ_FORWARD)
	    sfs[i] = init_sample_read_any(beg, ncp, READ_FORWARD, pos);
	  else sfs[i] = init_sample_read_any(ncp->samples[pos] - 1, ncp, READ_BACKWARD, pos);
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
	      snd_warning(_("no selection"));
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

static sync_state *get_sync_state(snd_info *sp, chan_info *cp, off_t beg, bool regexpr, 
				  read_direction_t forwards, XEN edpos, const char *caller, int arg_pos)
{
  return(get_sync_state_1(sp, cp, beg, regexpr, forwards, 0, edpos, caller, arg_pos));
}

static sync_state *get_sync_state_without_snd_fds(snd_info *sp, chan_info *cp, off_t beg, bool regexpr)
{
  sync_info *si = NULL;
  off_t dur;
  int i;
  sync_state *sc;
  dur = 0;
  if ((sp->sync != 0) && (!regexpr))
    {
      si = snd_sync(sp->sync);
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
	      snd_warning(_("no selection"));
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
  snd_info *sp = NULL, *gsp = NULL;
  int ip, stop_point = 0, err, impulse_chan = 0, filter_chans;
  bool ok = false;
  file_info *hdr;
  int scfd, fltfd, fftsize, ipow, dataformat;
  off_t filtersize = 0, filesize = 0, dataloc;
  char *ofile = NULL, *saved_chan_file;
  chan_info *ncp, *ucp;
  char *origin;
  if (cp) 
    {
      sp = cp->sound; 
      ncp = cp;
    }
  else
    {
      sp = any_selected_sound();
      if ((sp == NULL) || (ss == NULL)) return(NULL);
      ncp = any_selected_channel(sp);
    }
  filter_chans = mus_sound_chans(filename);
  if (filter_chans <= 0)
    return(mus_format(_("convolve: impulse response file %s chans: %d"), filename, filter_chans));
  filtersize = mus_sound_samples(filename) / filter_chans;
  if (filtersize <= 0) 
    return(mus_format(_("convolve: impulse response file %s is empty"), filename));
  /* if impulse response is stereo, we need to use both its channels */
  dataloc = mus_sound_data_location(filename);
  dataformat = mus_sound_data_format(filename);

  sc = get_sync_state_without_snd_fds(sp, ncp, 0, (cp == NULL));
  if (sc == NULL) return(NULL);
  si = sc->si;

  origin = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  mus_snprintf(origin, PRINT_BUFFER_SIZE,
	       "%s %s %.3f", 
	       (cp == NULL) ? S_convolve_selection_with : S_convolve_with, 
	       filename, amp);
  if (!(ss->stopped_explicitly))
    {
      for (ip = 0; ip < si->chans; ip++)
	{
	  ok = false;
      	  ucp = si->cps[ip];
	  if (!(editable_p(ucp))) continue;
	  sp = ucp->sound;
	  if ((ip > 0) && (sp != gsp)) 
	    finish_progress_report(gsp, NOT_FROM_ENVED);
	  if ((ip == 0) || (sp != gsp)) 
	    {
	      gsp = ucp->sound; 
	      start_progress_report(gsp, NOT_FROM_ENVED);
	    }

	  /* ofile here = new convolved data */
	  ofile = snd_tempnam();

	  saved_chan_file = snd_tempnam();
	  err = save_channel_edits(ucp, saved_chan_file, edpos, S_convolve_with, arg_pos);
	  if (err != MUS_NO_ERROR)
	    {
	      if (ofile) FREE(ofile);
	      free_sync_state(sc);
	      ucp->edit_hook_checked = false;
	      return(mus_format(_("convolve: save chan (%s[%d]) in %s hit error: %s\n"),
				sp->short_filename, ucp->chan, 
				saved_chan_file, strerror(errno)));
	    }
	  else
	    {
	      scfd = mus_file_open_read(saved_chan_file);
	      if (scfd == -1) 
		{
		  if (ofile) FREE(ofile);
		  free_sync_state(sc);
		  ucp->edit_hook_checked = false;
		  return(mus_format(_("convolve: open saved chan (%s[%d]) file %s hit error: %s\n"),
				    sp->short_filename, ucp->chan, 
				    saved_chan_file, strerror(errno)));
		}
	      else
		{
		  hdr = sp->hdr;
		  mus_file_open_descriptors(scfd,
					    saved_chan_file,
					    hdr->format,
					    mus_bytes_per_sample(hdr->format),
					    hdr->data_location,
					    1, hdr->type); /* ??? */
		  fltfd = mus_file_open_read(filename);
		  if (fltfd == -1) 
		    {
		      if (ofile) FREE(ofile);
		      ucp->edit_hook_checked = false;
		      free_sync_state(sc);
		      return(mus_format(_("convolve: open filter file %s hit error: %s\n"), 
					filename, strerror(errno)));
		    }
		  else
		    {
		      mus_file_open_descriptors(fltfd,
						filename,
						dataformat,
						mus_bytes_per_sample(dataformat),
						dataloc,
						filter_chans,
						mus_sound_header_type(filename));
		      if (cp == NULL)
			filesize = selection_len();
		      else filesize = to_c_edit_samples(ucp, edpos, S_convolve_with, arg_pos);
		      if (filesize > 0)
			{
			  ipow = (int)(ceil(log(filtersize + filesize) / log(2.0))) + 1;
			  fftsize = snd_ipow2(ipow);
			  ok = true;
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
			{
			  if (ofile) FREE(ofile);
			  free_sync_state(sc);
			  ucp->edit_hook_checked = false;
			  return(mus_format(_("convolve: close filter file %s hit error: %s\n"), 
					    filename, strerror(errno)));
			}
		    }
		}
	      if (mus_file_close(scfd) != 0)
		{
		  if (ofile) FREE(ofile);
		  free_sync_state(sc);
		  ucp->edit_hook_checked = false;
		  return(mus_format(_("convolve: close saved chan (%s[%d]) file %s hit error: %s\n"),
				    sp->short_filename, ucp->chan, 
				    saved_chan_file, strerror(errno)));
		}
	    }
	  snd_remove(saved_chan_file, REMOVE_FROM_CACHE);
	  FREE(saved_chan_file);

	  if (ok)
	    {
	      if (cp == NULL)
		{
		  delete_samples(si->begs[ip], sc->dur, ucp, origin, ucp->edit_ctr);
		  if ((filtersize + filesize) > 0)
		    {
		      file_insert_samples(si->begs[ip], filtersize + filesize, ofile, ucp, 0, DELETE_ME, origin, ucp->edit_ctr);
		      reactivate_selection(ucp, si->begs[ip], si->begs[ip] + filtersize + filesize);
		      backup_edit_list(ucp); 
		      ripple_trailing_marks(ucp, si->begs[ip], sc->dur, filtersize + filesize);
		    }
		  else snd_remove(ofile, REMOVE_FROM_CACHE);
		  update_graph(ucp); 
		}
	      else file_override_samples(filtersize + filesize, ofile, ucp, 0, DELETE_ME, LOCK_MIXES, origin);
	    }
	  ucp->edit_hook_checked = false;
	  if (ofile) FREE(ofile);
	  check_for_event();
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
      ss->stopped_explicitly = false;
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

void scale_by(chan_info *cp, Float *ur_scalers, int len, bool selection)
{
  /* if selection, sync to current selection, else sync to current sound */
  /* 3-Oct-00: the scale factors are now embedded in the edit fragments  */
  sync_info *si;
  chan_info *ncp;
  int i, j;
  off_t beg, frames;
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
	  frames = CURRENT_SAMPLES(ncp);
	}
      scale_channel(ncp, ur_scalers[j], beg, frames, ncp->edit_ctr, false);
      j++;
      if (j >= len) j = 0;
    }
  free_sync_info(si);
}

Float get_maxamp(snd_info *sp, chan_info *cp, int edpos)
{
  Float val;
  int pos;
  if (!sp) return(0.0);
  if (!cp) cp = sp->chans[0];
  if (edpos == AT_CURRENT_EDIT_POSITION) pos = cp->edit_ctr; else pos = edpos;
  if (amp_env_maxamp_ok(cp, pos)) 
    return(amp_env_maxamp(cp, pos));
  val = ed_maxamp(cp, pos);
  if (val >= 0.0) return(val);
  val = local_maxamp(cp, 0, cp->samples[pos], pos);
  set_ed_maxamp(cp, pos, val);
  return(val);
}

void scale_to(snd_info *sp, chan_info *cp, Float *ur_scalers, int len, bool selection)
{
  /* essentially the same as scale-by, but first take into account current maxamps */
  /* here it matters if more than one arg is given -- if one, get overall maxamp */
  /*   if more than one, get successive maxamps */
  int i, chans, nlen, datum_size;
  off_t beg, frames;
  sync_info *si = NULL;
  chan_info *ncp;
  Float maxamp, val;
  Float *scalers;
  if ((!selection) && (cp == NULL)) return;
  if (selection) 
    {
      if (!(selection_is_active())) 
	return;
      si = selection_sync();
      sp = si->cps[0]->sound;
    }
  else si = sync_to_chan(cp);
  datum_size = mus_bytes_per_sample((sp->hdr)->format);
  chans = si->chans;
  scalers = (Float *)CALLOC(chans, sizeof(Float));
  if (chans < len)
    nlen = chans; 
  else nlen = len;
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
		val = selection_maxamp(ncp);
	      else val = get_maxamp(ncp->sound, ncp, AT_CURRENT_EDIT_POSITION);
	      if (val > maxamp) maxamp = val;
	    }
	  if ((!(data_clipped(ss))) && 
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
		val = selection_maxamp(ncp);
	      else val = get_maxamp(ncp->sound, ncp, AT_CURRENT_EDIT_POSITION);
	      if (val != 0.0)
		{
		  if ((!(data_clipped(ss))) && 
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
	  frames = CURRENT_SAMPLES(ncp);
	}
      scale_channel(ncp, scalers[i], beg, frames, ncp->edit_ctr, false);
    }
  FREE(scalers);
  free_sync_info(si);
}

static void swap_channels(chan_info *cp0, chan_info *cp1, off_t beg, off_t dur, int pos0, int pos1)
{
  snd_fd *c0, *c1;
  snd_info *sp0;
  file_info *hdr0 = NULL, *hdr1 = NULL;
  int j, ofd0 = 0, ofd1 = 0, datumb = 0, err = 0;
  bool temp_file;
  off_t k;
  mus_sample_t **data0, **data1;
  mus_sample_t *idata0, *idata1;
  bool reporting = false;
  char *ofile0 = NULL, *ofile1 = NULL;
  if (dur <= 0) return;
  if ((!(editable_p(cp0))) || (!(editable_p(cp1)))) return;
  sp0 = cp0->sound;
  reporting = ((sp0) && (dur > REPORTING_SIZE));
  if (reporting) start_progress_report(sp0, NOT_FROM_ENVED);
  if (dur > MAX_BUFFER_SIZE)
    {
      temp_file = true; 
      ofile0 = snd_tempnam();
      hdr0 = make_temp_header(ofile0, SND_SRATE(sp0), 1, dur, (char *)S_swap_channels);
      ofd0 = open_temp_file(ofile0, 1, hdr0);
      if (ofd0 == -1)
	{
	  cp0->edit_hook_checked = false;
	  cp1->edit_hook_checked = false;
	  free_file_info(hdr0);
	  snd_error(_("can't open swap-channels temp file %s: %s\n"), ofile0, strerror(errno));
	  return;
	}
      datumb = mus_bytes_per_sample(hdr0->format);
      ofile1 = snd_tempnam();
      hdr1 = make_temp_header(ofile1, SND_SRATE(sp0), 1, dur, (char *)S_swap_channels);
      ofd1 = open_temp_file(ofile1, 1, hdr1);
      if (ofd1 == -1)
	{
	  cp0->edit_hook_checked = false;
	  cp1->edit_hook_checked = false;
	  close_temp_file(ofd0, hdr0, 0, sp0);
	  free_file_info(hdr0);
	  free_file_info(hdr1);
	  if (ofile0) FREE(ofile0);
	  snd_error(_("can't open swap-channels temp file %s: %s\n"), ofile1, strerror(errno));
	  return;
	}
    }
  else temp_file = false;
  data0 = (mus_sample_t **)MALLOC(sizeof(mus_sample_t *));
  data0[0] = (mus_sample_t *)CALLOC(MAX_BUFFER_SIZE, sizeof(mus_sample_t)); 
  data1 = (mus_sample_t **)MALLOC(sizeof(mus_sample_t *));
  data1[0] = (mus_sample_t *)CALLOC(MAX_BUFFER_SIZE, sizeof(mus_sample_t)); 
  idata0 = data0[0];
  idata1 = data1[0];
  c0 = init_sample_read_any(beg, cp0, READ_FORWARD, pos0);
  c1 = init_sample_read_any(beg, cp1, READ_FORWARD, pos1);
  if (temp_file)
    {
      j = 0;
      for (k = 0; k < dur; k++)
	{
	  idata0[j] = read_sample(c1);
	  idata1[j] = read_sample(c0);
	  j++;
	  if (j == MAX_BUFFER_SIZE)
	    {
	      err = mus_file_write(ofd0, 0, j - 1, 1, data0);
	      err = mus_file_write(ofd1, 0, j - 1, 1, data1);
	      j = 0;
	      if (err == -1) break;
	      if (reporting) progress_report(sp0, "scl", 1, 1, (Float)((double)k / (double)dur), NOT_FROM_ENVED);
	    }
	}
      if (j > 0) 
	{
	  mus_file_write(ofd0, 0, j - 1, 1, data0);
	  mus_file_write(ofd1, 0, j - 1, 1, data1);
	}
      close_temp_file(ofd0, hdr0, dur * datumb, sp0);
      close_temp_file(ofd1, hdr1, dur * datumb, sp0); /* sp0 used here in case of error report */
      free_file_info(hdr0);
      free_file_info(hdr1);
      file_change_samples(beg, dur, ofile0, cp0, 0, DELETE_ME, LOCK_MIXES, S_swap_channels, cp0->edit_ctr);
      file_change_samples(beg, dur, ofile1, cp1, 0, DELETE_ME, LOCK_MIXES, S_swap_channels, cp1->edit_ctr);
      if (ofile0) {FREE(ofile0); ofile0 = NULL;}
      if (ofile1) {FREE(ofile1); ofile1 = NULL;}
      if (reporting) finish_progress_report(sp0, NOT_FROM_ENVED);
    }
  else 
    {
      for (k = 0; k < dur; k++)
	{
	  idata0[k] = read_sample(c1);
	  idata1[k] = read_sample(c0);
	}
      change_samples(beg, dur, data0[0], cp0, LOCK_MIXES, S_swap_channels, cp0->edit_ctr);
      change_samples(beg, dur, data1[0], cp1, LOCK_MIXES, S_swap_channels, cp1->edit_ctr);
    }
  swap_marks(cp0, cp1);
  update_graph(cp0);
  update_graph(cp1);
  if (ofile0) FREE(ofile0);
  if (ofile1) FREE(ofile1);
  FREE(data0[0]);
  FREE(data0);
  FREE(data1[0]);
  FREE(data1);
  free_snd_fd(c0);
  free_snd_fd(c1);
  cp0->edit_hook_checked = false;
  cp1->edit_hook_checked = false;
}

/* -------- src -------- */

Float src_input_as_needed(void *arg, int direction) 
{
  src_state *sr = (src_state *)arg;
  snd_fd *sf;
  sf = sr->sf;
  sr->sample++;
  if (direction != sr->dir)
    {
      read_sample_change_direction(sf, (direction == 1) ? READ_FORWARD : READ_BACKWARD);
      sr->dir = direction;
    }
  return(read_sample_to_float(sf));
}

src_state *make_src(Float srate, snd_fd *sf, Float initial_srate)
{
  src_state *sr;
  sr = (src_state *)CALLOC(1, sizeof(src_state));
  sr->sf = sf;
  if (initial_srate >= 0.0) sr->dir = 1; else sr->dir = -1;
  sr->gen = mus_make_src(&src_input_as_needed, initial_srate, sinc_width(ss), (void *)sr);
  mus_set_increment(sr->gen, srate);
  sr->sample = 0; /* this is how the old form worked */
  return(sr);
}

src_state *free_src(src_state *sr)
{
  mus_free(sr->gen);
  FREE(sr);
  return(NULL);
}

static int off_t_compare(const void *a, const void *b)
{
  off_t *m1, *m2;
  m1 = (off_t *)a;
  m2 = (off_t *)b;
  if (*m1 < *m2) return(-1);
  if (*m1 == *m2) return(0);
  return(1);
}

static char *src_channel_with_error(chan_info *cp, snd_fd *sf, off_t beg, off_t dur, Float ratio, mus_any *egen, 
				    enved_progress_t from_enved, const char *origin, bool over_selection, int curchan, int chans)
{
  snd_info *sp = NULL;
  bool reporting = false;
  int jj;
  bool full_chan;
  mus_sample_t **data;
  file_info *hdr = NULL;
  int j, ofd = 0, datumb = 0, err = 0;
  off_t k;
  char *ofile = NULL;
  mus_sample_t *idata;
  src_state *sr;
  off_t *old_marks = NULL, *new_marks = NULL;
  mark **mps;
  int cur_mark = 0, cur_new_mark = 0;
  off_t cur_mark_sample = -1;
  int cur_marks = 0, m;
  Float env_val;
  off_t next_pass;
  if ((ratio == 1.0) && (egen == NULL)) return(NULL);
  if (!(editable_p(cp))) return(NULL);
  sp = cp->sound;
  full_chan = ((beg == 0) && (dur == CURRENT_SAMPLES(cp)));
  cur_marks = 0;
  new_marks = NULL;
  reporting = ((sp) && (dur > REPORTING_SIZE));
  if (reporting) start_progress_report(sp, from_enved);
  ofile = snd_tempnam();
  hdr = make_temp_header(ofile, SND_SRATE(sp), 1, dur, (char *)origin);
  ofd = open_temp_file(ofile, 1, hdr);
  if (ofd == -1)
    {
      cp->edit_hook_checked = false;
      return(mus_format(_("can't open %s temp file %s: %s\n"), origin, ofile, strerror(errno)));
    }
  data = (mus_sample_t **)MALLOC(sizeof(mus_sample_t *));
  data[0] = (mus_sample_t *)CALLOC(MAX_BUFFER_SIZE, sizeof(mus_sample_t)); 
  datumb = mus_bytes_per_sample(hdr->format);
  idata = data[0];
  sr = make_src(ratio, sf, ratio);
  j = 0;
  ss->stopped_explicitly = false;
  if (egen == NULL)
    {
      if ((ratio == 0.5) || (ratio == 2.0))
	{
	  for (k = 0; sr->sample < dur; k++)
	    {
	      if (ratio == 2.0)
		idata[j] = (MUS_FLOAT_TO_SAMPLE(mus_src_20(sr->gen, &src_input_as_needed)));
	      else idata[j] = (MUS_FLOAT_TO_SAMPLE(mus_src_05(sr->gen, &src_input_as_needed)));
	      j++;
	      if (j == MAX_BUFFER_SIZE)
		{
		  err = mus_file_write(ofd, 0, j - 1, 1, data);
		  j = 0;
		  if (err == -1) break;
		  if (reporting) 
		    {
		      progress_report(sp, origin, curchan + 1, chans, (Float)((double)(sr->sample) / (double)dur), from_enved);
		      if (ss->stopped_explicitly) break;
		    }
		}
	    }
	}
      else
	{
	  for (k = 0; sr->sample < dur; k++) /* sr->sample tracks input location -- produce output until input exhausted */
	    {
	      idata[j] = (MUS_FLOAT_TO_SAMPLE(mus_src(sr->gen, 0.0, &src_input_as_needed)));
	      j++;
	      if (j == MAX_BUFFER_SIZE)
		{
		  err = mus_file_write(ofd, 0, j - 1, 1, data);
		  j = 0;
		  if (err == -1) break;
		  if (reporting) 
		    {
		      progress_report(sp, origin, curchan + 1, chans, (Float)((double)(sr->sample) / (double)dur), from_enved);
		      if (ss->stopped_explicitly) break;
		    }
		}
	    }
	}
    }
  else
    {
      /* envelope case -- have to go by sr->sample, not output sample counter, also check marks */
      cur_mark_sample = -1;
      env_val = mus_env(egen);
      if ((cp->marks) && 
	  (cp->mark_ctr[cp->edit_ctr] >= 0))
	{
	  mps = cp->marks[cp->edit_ctr];
	  cur_marks = cp->mark_ctr[cp->edit_ctr] + 1;
	  new_marks = (off_t *)MALLOC(cur_marks * sizeof(off_t));
	  old_marks = (off_t *)MALLOC(cur_marks * sizeof(off_t));
	  for (m = 0; m < cur_marks; m++)
	    {
	      new_marks[m] = -1;
	      if ((env_val >= 0.0) ||
		  (mps[m]->samp < beg) ||
		  (mps[m]->samp > (beg + dur)))
		old_marks[m] = mps[m]->samp;
	      else 
		{
		  old_marks[m] = (dur - mps[m]->samp - 1) + beg; /* moving backwards, so flip marks */
		  cur_new_mark = m;
		}
	    }
	  if ((env_val < 0.0) && (cur_marks > 1))
	    qsort((void *)old_marks, cur_marks, sizeof(off_t), off_t_compare);
	  for (m = 0; m < cur_marks; m++)
	    if (old_marks[m] > beg)
	      {
		cur_mark_sample = old_marks[m];
		cur_mark = m;
		if ((env_val >= 0.0) || (cur_marks <= 1))
		  cur_new_mark = m;
		break;
	      }
	}
      next_pass = sr->sample;
      for (k = 0; sr->sample < dur; k++)
	{
	  idata[j] = (MUS_FLOAT_TO_SAMPLE(mus_src(sr->gen, env_val, &src_input_as_needed)));
	  j++;
	  if (j == MAX_BUFFER_SIZE)
	    {
	      err = mus_file_write(ofd, 0, j - 1, 1, data);
	      j = 0;
	      if (err == -1) break;
	      if (reporting) 
		{
		  progress_report(sp, origin, curchan + 1, chans, (Float)((double)(sr->sample) / (double)dur), from_enved);
		  if (ss->stopped_explicitly) break;
		}
	    }
	  if (next_pass != sr->sample)             /* tick env forward dependent on sr->sample */
	    {
	      off_t idiff;
	      idiff = sr->sample - next_pass;
	      next_pass = sr->sample;
	      if ((new_marks) && 
		  (cur_mark_sample != -1) && 
		  (next_pass >= (cur_mark_sample - beg))) 
		{
		  /* not '==' because sr->sample can be incremented by more than 1 */
		  new_marks[cur_new_mark] = k + beg;
		  cur_mark++;
		  if (env_val >= 0.0) cur_new_mark++; else cur_new_mark--;
		  if (cur_mark < cur_marks)
		    cur_mark_sample = old_marks[cur_mark];
		  else cur_mark_sample = -1;
		}
	      for (jj = 0; jj < idiff; jj++) 
		env_val = mus_env(egen);
	    }
	}
    }
  if (reporting) finish_progress_report(sp, from_enved);
  sr = free_src(sr);
  if ((!(ss->stopped_explicitly)) && (j > 0)) mus_file_write(ofd, 0, j - 1, 1, data);
  close_temp_file(ofd, hdr, k * datumb, sp);
  hdr = free_file_info(hdr);
  if (!(ss->stopped_explicitly))
    {
      if (!full_chan)
	{
	  /* here we need delete followed by insert since dur is probably different */
	  if (k == dur)
	    file_change_samples(beg, dur, ofile, cp, 0, DELETE_ME, LOCK_MIXES, origin, cp->edit_ctr);
	  else
	    {
	      delete_samples(beg, dur, cp, origin, cp->edit_ctr);
	      file_insert_samples(beg, k, ofile, cp, 0, DELETE_ME, origin, cp->edit_ctr);
	      if (over_selection)
		reactivate_selection(cp, beg, beg + k); /* backwards compatibility */
	      backup_edit_list(cp);
	      ripple_marks(cp, 0, 0);
	    }
	  update_graph(cp);
	}
      else file_override_samples(k, ofile, cp, 0, DELETE_ME, LOCK_MIXES, origin);
      /* not file_change_samples because that would not necessarily change the current file length */
      if (cp->marks)
	{
	  if (egen == NULL)
	    src_marks(cp, ratio, dur, k, beg, full_chan);
	  else
	    {
	      if (new_marks) 
		reset_marks(cp, cur_marks, new_marks, beg + dur, (k - dur), full_chan);
	    }
	}
      update_graph(cp);
    }
  else
    {
      report_in_minibuffer(sp, _("src interrupted"));
      ss->stopped_explicitly = false;
    }
  if (old_marks) FREE(old_marks);
  old_marks = NULL;
  if (new_marks) FREE(new_marks);
  new_marks = NULL;
  FREE(ofile); 
  ofile = NULL;
  FREE(data[0]);
  FREE(data);
  cp->edit_hook_checked = false;
  return(NULL);
}

void src_env_or_num(chan_info *cp, env *e, Float ratio, bool just_num, 
		    enved_progress_t from_enved, const char *origin, bool over_selection, mus_any *gen, XEN edpos, int arg_pos, Float e_base)
{
  snd_info *sp = NULL;
  sync_state *sc;
  sync_info *si;
  snd_fd **sfs;
  int i;
  off_t scdur, dur;
  int stop_point = 0;
  mus_any *egen = NULL;
  char *errmsg = NULL;

  if ((!just_num) && (e == NULL) && (gen == NULL)) return;
  if ((just_num) && (ratio == 0.0)) return;

  /* get envelope or src ratio */
  sp = cp->sound;
  /* get current syncd chans */
  sc = get_sync_state(sp, cp, 0, over_selection, 
		      (ratio < 0.0) ? READ_BACKWARD : READ_FORWARD, /* 0->beg, 0->regexpr (ratio = 0.0 if from_enved) */
		      edpos,
		      origin, arg_pos);      
  if (sc == NULL) return;
  si = sc->si;
  sfs = sc->sfs;
  scdur = sc->dur;

  if (!(ss->stopped_explicitly))
    {
      for (i = 0; i < si->chans; i++)
	{
	  cp = si->cps[i];
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
	  if (!just_num)
	    {
	      if (e)
		egen = mus_make_env(e->data, e->pts, 1.0, 0.0, e_base, 0.0, 0, dur - 1, NULL);
	      else egen = gen;
	      if (egen) ratio = 0.0;            /* added 14-Mar-01 otherwise the envelope is an offset? */
	    }
	  errmsg = src_channel_with_error(cp, sfs[i], si->begs[i], dur, ratio, egen, from_enved, origin, over_selection, i, si->chans);
	  if (egen)
	    {
	      if (e) 
		mus_free(egen);
	      else mus_restart_env(gen);
	    }
	  if (errmsg)
	    {
	      snd_error(errmsg);
	      break;
	    }
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
      ss->stopped_explicitly = false;
      for (i = 0; i <= stop_point; i++)
	{
	  cp = si->cps[i];
	  undo_edit(cp, 1);
	}
    }
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
  int i, j, lim, size;
  Float last_x, step;
  if (!e) return(NULL);
  /* get the frequency envelope and design the FIR filter */
  fdata = (Float *)CALLOC(order, sizeof(Float));
  last_x = e->data[(e->pts - 1) * 2];
  step = 2.0 * last_x / (Float)(order - 1);
  lim = order / 2;
  size = e->pts * 2;
  for (i = 0, x = 0.0; i < lim; i++, x += step) 
    fdata[i] = list_interp(x, e->data, size); /* not mus_env here since it's likely the points fall between the order-related samples */
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

void display_frequency_response(env *e, axis_info *ap, axis_context *gax, int order, bool dBing)
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
    y1 = (int)(ap->y_axis_y0 + (ss->min_dB - in_dB(ss->min_dB, ss->lin_dB, resp)) * height / ss->min_dB);
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
	y1 = (int)(ap->y_axis_y0 + (ss->min_dB - in_dB(ss->min_dB, ss->lin_dB, resp)) * height / ss->min_dB);
      else y1 = (int)(ap->y_axis_y0 + resp * height);
      draw_line(gax, x0, y0, x1, y1);
    }
  FREE(coeffs);
}

static char *clm_channel(chan_info *cp, mus_any *gen, off_t beg, off_t dur, int edpos, char *caller, off_t overlap)
{
  /* calls gen over cp[beg for dur] data, replacing. */
  snd_info *sp;
  file_info *hdr = NULL;
  int j, ofd = 0, datumb = 0, err = 0;
  bool temp_file;
  off_t k;
  mus_sample_t **data;
  mus_sample_t *idata;
  char *ofile = NULL;
  snd_fd *sf;
  if ((beg < 0) || ((dur + overlap) <= 0)) return(NULL);
  sp = cp->sound;
  if (!(MUS_RUN_P(gen)))
    return(mus_format(_("clm-channel: %s can't handle %s generators"),
		      caller,
		      mus_name(gen)));
  if (!(editable_p(cp))) return(NULL);
  sf = init_sample_read_any(beg, cp, READ_FORWARD, edpos);
  if (sf == NULL)
    {
      cp->edit_hook_checked = false;
      return(mus_format(_("%s: can't read %s[%d] channel data!"), caller, sp->short_filename, cp->chan));
    }
  if ((dur + overlap) > MAX_BUFFER_SIZE)
    {
      temp_file = true; 
      ofile = snd_tempnam();
      hdr = make_temp_header(ofile, SND_SRATE(sp), 1, dur + overlap, caller);
      ofd = open_temp_file(ofile, 1, hdr);
      if (ofd == -1)
	{
	  cp->edit_hook_checked = false;
	  free_snd_fd(sf); 
	  return(mus_format(_("can't open %s temp file %s: %s\n"), caller, ofile, strerror(errno)));
	}
      datumb = mus_bytes_per_sample(hdr->format);
    }
  else temp_file = false;
  data = (mus_sample_t **)MALLOC(sizeof(mus_sample_t *));
  data[0] = (mus_sample_t *)CALLOC(MAX_BUFFER_SIZE, sizeof(mus_sample_t)); 
  idata = data[0];
  if (temp_file)
    {
      j = 0;
      for (k = 0; k < dur; k++)
	{
	  idata[j++] = MUS_FLOAT_TO_SAMPLE(MUS_RUN(gen, read_sample_to_float(sf), 0.0));
	  if (j == MAX_BUFFER_SIZE)
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
	idata[k] = MUS_FLOAT_TO_SAMPLE(MUS_RUN(gen, read_sample_to_float(sf), 0.0));
      j = (int)dur;
    }
  for (k = 0; k < overlap; k++)
    {
      idata[j++] = MUS_FLOAT_TO_SAMPLE(MUS_RUN(gen, 0.0, 0.0) + read_sample_to_float(sf));
      if ((temp_file) && (j == MAX_BUFFER_SIZE))
	{
	  err = mus_file_write(ofd, 0, j - 1, 1, data);
	  j = 0;
	  if (err == -1) break;
	}
    }
  dur += overlap;
  if (temp_file)
    {
      if (j > 0) mus_file_write(ofd, 0, j - 1, 1, data);
      close_temp_file(ofd, hdr, dur * datumb, sp);
      hdr = free_file_info(hdr);
      file_change_samples(beg, dur, ofile, cp, 0, DELETE_ME, LOCK_MIXES, caller, cp->edit_ctr);
      if (ofile) 
	{
	  FREE(ofile); 
	  ofile = NULL;
	}
    }
  else 
    {
      if (dur > 0) 
	change_samples(beg, dur, idata, cp, LOCK_MIXES, caller, cp->edit_ctr);
    }
  update_graph(cp); 
  free_snd_fd(sf);
  FREE(data[0]);
  FREE(data);
  cp->edit_hook_checked = false;
  return(NULL);
}

static char *apply_filter_or_error(chan_info *ncp, int order, env *e, enved_progress_t from_enved, 
				   const char *origin, bool over_selection, Float *ur_a, mus_any *gen, XEN edpos, int arg_pos)
{
  /* if string returned, needs to be freed */
  /* interpret e as frequency response and apply as filter to all sync'd chans */
  Float *a = NULL, *d = NULL;
  Float x;
  sync_state *sc;
  sync_info *si;
  snd_info *sp;
  bool reporting = false;
  int i, m, stop_point = 0;
  off_t prebeg = 0;
  off_t scdur, dur, offk;
  snd_fd **sfs;
  snd_fd *sf;
  file_info *hdr = NULL;
  int j, ofd = 0, datumb = 0, err = 0;
  bool temp_file;
  mus_sample_t **data;
  mus_sample_t *idata;
  char *ofile = NULL;
  chan_info *cp;
#if HAVE_FFTW || HAVE_FFTW3
  int fsize, k;
  Float scale;
  Float *sndrdat = NULL, *fltdat = NULL;
#endif
  if ((!e) && (!ur_a) && (!gen)) 
    return(NULL);
  if ((gen) && (!(MUS_RUN_P(gen))))
    return(mus_format(_("%s: can't handle %s generators"),
		      origin,
		      mus_name(gen)));
  sp = ncp->sound;
  sc = get_sync_state_1(sp, ncp, 0, over_selection, 
			READ_FORWARD, (over_selection) ? (order - 1) : 0, 
			edpos,
			origin, arg_pos);
  if (sc == NULL) return(NULL);
  si = sc->si;
  sfs = sc->sfs;
  scdur = sc->dur;
  ss->stopped_explicitly = false;
#if HAVE_FFTW || HAVE_FFTW3
  if ((!ur_a) && 
      (!gen) && 
      (!over_selection) &&
      ((order == 0) || (order >= 128)))
    {
      /* use convolution if order is large and not over_selection */
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
	  if (!(editable_p(cp))) continue;
	  dur += order;
	  fsize = snd_2pow2(dur);
	  sndrdat = (Float *)CALLOC(fsize, sizeof(Float));
	  fltdat = env2array(fsize, e);

	  sf = sfs[i];                                 /* init_sample_read(0, cp, READ_FORWARD); */
	  for (k = 0; k < dur; k++) 
	    sndrdat[k] = (Float)(read_sample_to_float(sf));
	  sfs[i] = free_snd_fd(sf);

	  mus_fftw(sndrdat, fsize, 1);
	  check_for_event();
	  if (ss->stopped_explicitly)
	    {
	      ss->stopped_explicitly = false;
	      report_in_minibuffer(sp, _("stopped"));
	      FREE(sndrdat);
	      FREE(fltdat);
	      cp->edit_hook_checked = false;
	      break;
	    }
	  scale = 1.0 / (Float)fsize;
	  for (k = 0; k < fsize; k++)
	    sndrdat[k] *= (scale * fltdat[k]);         /* fltdat (via env2array) is already reflected around midpoint */
	  mus_fftw(sndrdat, fsize, -1);

	  ofile = snd_tempnam();
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
	  ofd = open_temp_file(ofile, 1, hdr);
	  if (ofd == -1)
	    {
	      cp->edit_hook_checked = false;
	      snd_error(_("can't open %s temp file %s: %s\n"), origin, ofile, strerror(errno));
	      break;
	    }
	  write(ofd, sndrdat, fsize * sizeof(Float));
	  close_temp_file(ofd, hdr, fsize * sizeof(Float), sp);
	  hdr = free_file_info(hdr);
	  file_change_samples(0, dur + order, ofile, cp, 0, DELETE_ME, LOCK_MIXES, origin, cp->edit_ctr);
	  if (ofile) {FREE(ofile); ofile = NULL;}
	  update_graph(cp);
	  FREE(sndrdat);
	  FREE(fltdat);
	  cp->edit_hook_checked = false;
	  check_for_event();
	  if (ss->stopped_explicitly)
	    {
	      ss->stopped_explicitly = false;
	      report_in_minibuffer(sp, _("stopped"));
	      break;
	    }
	}
    }
  else
#endif
    {
      if (order == 0) order = enved_filter_order(ss);
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
      data = (mus_sample_t **)MALLOC(sizeof(mus_sample_t *));
      data[0] = (mus_sample_t *)CALLOC(MAX_BUFFER_SIZE, sizeof(mus_sample_t)); 
      
      if (!(ss->stopped_explicitly))
	{
	  for (i = 0; i < si->chans; i++)
	    {
	      /* done channel at a time here, rather than in parallel as in apply-env because */
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
	      if (!(editable_p(cp))) continue;
	      dur += order;
	      reporting = ((sp) && (dur > REPORTING_SIZE));
	      if (reporting) start_progress_report(sp, from_enved);
	      if (dur > MAX_BUFFER_SIZE)
		{
		  temp_file = true; 
		  ofile = snd_tempnam();
		  hdr = make_temp_header(ofile, SND_SRATE(sp), 1, dur, (char *)origin);
		  ofd = open_temp_file(ofile, 1, hdr);
		  if (ofd == -1)
		    {
		      snd_error(_("can't open %s temp file %s: %s\n"), origin, ofile, strerror(errno));
		      cp->edit_hook_checked = false;
		      break;
		    }
		  datumb = mus_bytes_per_sample(hdr->format);
		}
	      else temp_file = false;
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
			for (m = (int)prebeg; m > 0; m--)
			  d[m] = read_sample_to_float(sf);
		    }
		}
	      j = 0;
	      for (offk = 0; offk < dur; offk++)
		{
		  if (gen)
		    x = MUS_RUN(gen, read_sample_to_float(sf), 0.0);
		  else
		    {
		      x = 0.0; 
		      d[0] = read_sample_to_float(sf);
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
			  progress_report(sp, S_filter_sound, i + 1, si->chans, (Float)((double)offk / (double)dur), from_enved);
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
		    file_change_samples(si->begs[i], dur, ofile, cp, 0, DELETE_ME, LOCK_MIXES, origin, cp->edit_ctr);
		  else file_change_samples(0, dur, ofile, cp, 0, DELETE_ME, LOCK_MIXES, origin, cp->edit_ctr);
		  if (ofile) {FREE(ofile); ofile = NULL;}
		}
	      else change_samples(si->begs[i], dur, data[0], cp, LOCK_MIXES, origin, cp->edit_ctr);
	      update_graph(cp); 
	      sfs[i] = free_snd_fd(sfs[i]);
	      cp->edit_hook_checked = false;
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
	  ss->stopped_explicitly = false;
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

void apply_filter(chan_info *ncp, int order, env *e, enved_progress_t from_enved, 
		  const char *origin, bool over_selection, Float *ur_a, mus_any *gen, XEN edpos, int arg_pos)
{
  char *error;
  error = apply_filter_or_error(ncp, order, e, from_enved, origin, over_selection, ur_a, gen, edpos, arg_pos);
  if (error)
    {
      snd_error(error);
      FREE(error);
    }
}

static char *reverse_channel(chan_info *cp, snd_fd *sf, off_t beg, off_t dur, XEN edp, char *caller, int arg_pos)
{
  snd_info *sp;
  env_info *ep = NULL;
  file_info *hdr = NULL;
  int i, j, ofd = 0, datumb = 0, err = 0, edpos = 0;
  bool section = false, temp_file;
  off_t k;
  mus_sample_t **data;
  mus_sample_t *idata;
  char *ofile = NULL;
  if ((beg < 0) || (dur <= 0)) return(NULL);
  if (!(editable_p(cp))) return(NULL);
  sp = cp->sound;
  edpos = to_c_edit_position(cp, edp, caller, arg_pos);
  if (dur > cp->samples[edpos]) dur = cp->samples[edpos];
  if (dur > MAX_BUFFER_SIZE)
    {
      temp_file = true; 
      ofile = snd_tempnam();
      hdr = make_temp_header(ofile, SND_SRATE(sp), 1, dur, caller);
      ofd = open_temp_file(ofile, 1, hdr);
      if (ofd == -1)
	{
	  if (ofile) FREE(ofile);
	  cp->edit_hook_checked = false;	  
	  return(mus_format(_("can't open %s temp file %s: %s\n"), caller, ofile, strerror(errno)));
	}
      datumb = mus_bytes_per_sample(hdr->format);
    }
  else temp_file = false;
  if ((beg == 0) && (dur == cp->samples[edpos]))
    ep = amp_env_copy(cp, true, edpos); /* true -> reversed */
  else 
    {
      int sbin, ebin;
      mus_sample_t min1, max1;
      ep = amp_env_copy(cp, false, edpos);
      if (ep) 
	{
	  /* now reverse the selection */
	  sbin = (int)ceil(beg / ep->samps_per_bin);
	  ebin = (int)floor((beg + dur) / ep->samps_per_bin);
	  if (ebin > ep->amp_env_size) ebin = ep->amp_env_size;
	  for (i = sbin, j = ebin - 1; i < j; i++, j--)
	    {
	      min1 = ep->data_min[i];
	      max1 = ep->data_max[i];
	      ep->data_min[i] = ep->data_min[j];
	      ep->data_max[i] = ep->data_max[j];
	      ep->data_min[j] = min1;
	      ep->data_max[j] = max1;
	    }
	  if (sbin > 0) pick_one_bin(ep, sbin - 1, ep->samps_per_bin * (sbin - 1), cp, edpos);
	  if (ebin < ep->amp_env_size) pick_one_bin(ep, ebin, ep->samps_per_bin * ebin, cp, edpos);
	}
      section = true; /* only for reverse_marks below */
    }
  data = (mus_sample_t **)MALLOC(sizeof(mus_sample_t *));
  data[0] = (mus_sample_t *)CALLOC(MAX_BUFFER_SIZE, sizeof(mus_sample_t)); 
  idata = data[0];
  if (temp_file)
    {
      j = 0;
      for (k = 0; k < dur; k++)
	{
	  idata[j++] = read_sample(sf);
	  if (j == MAX_BUFFER_SIZE)
	    {
	      err = mus_file_write(ofd, 0, j - 1, 1, data);
	      j = 0;
	      if (err == -1) break;
	    }
	}
      if (j > 0) mus_file_write(ofd, 0, j - 1, 1, data);
      close_temp_file(ofd, hdr, dur * datumb, sp);
      hdr = free_file_info(hdr);
      file_change_samples(beg, dur, ofile, cp, 0, DELETE_ME, LOCK_MIXES, caller, cp->edit_ctr);
      if (ofile) 
	{
	  FREE(ofile); 
	  ofile = NULL;
	}
    }
  else 
    {
      for (k = 0; k < dur; k++)
	idata[k] = read_sample(sf);
      change_samples(beg, dur, idata, cp, LOCK_MIXES, caller, cp->edit_ctr);
    }
  if (ep) cp->amp_envs[cp->edit_ctr] = ep;
  reverse_marks(cp, (section) ? beg : -1, dur);
  update_graph(cp); 
  FREE(data[0]);
  FREE(data);
  cp->edit_hook_checked = false;	  
  return(NULL);
}

static void reverse_sound(chan_info *ncp, bool over_selection, XEN edpos, int arg_pos)
{
  sync_state *sc;
  sync_info *si;
  snd_info *sp;
  int i, stop_point = 0;
  off_t dur;
  snd_fd **sfs;
  char *errmsg = NULL;
  chan_info *cp;
  char *caller;
  sp = ncp->sound;
  caller = (char *)((over_selection) ? S_reverse_selection : S_reverse_sound);
  sc = get_sync_state(sp, ncp, 0, over_selection, READ_BACKWARD, 
		      edpos,
		      (const char *)caller, arg_pos);
  if (sc == NULL) return;
  si = sc->si;
  sfs = sc->sfs;
  if (!(ss->stopped_explicitly))
    {
      for (i = 0; i < si->chans; i++)
	{
	  cp = si->cps[i];
	  sp = cp->sound;
	  if (over_selection)
	    dur = sc->dur;
	  else dur = to_c_edit_samples(cp, edpos, caller, arg_pos);
	  if (dur == 0) 
	    {
	      if (sfs[i]) 
		{
		  free_snd_fd(sfs[i]); 
		  sfs[i] = NULL;
		}
	      continue;
	    }
	  errmsg = reverse_channel(cp, sfs[i], si->begs[i], dur, edpos, caller, arg_pos);
	  sfs[i] = free_snd_fd(sfs[i]);
	  if (errmsg)
	    {
	      snd_error(errmsg);
	      FREE(errmsg);
	      break;
	    }
	  if (ss->stopped_explicitly) 
	    {
	      stop_point = i;
	      break;
	    }
	}
    }
  if (ss->stopped_explicitly)
    {
      ss->stopped_explicitly = false;
      for (i = 0; i <= stop_point; i++)
	{
	  cp = si->cps[i];
	  undo_edit(cp, 1);
	}
    }
  free_sync_state(sc);
}

void apply_env(chan_info *cp, env *e, off_t beg, off_t dur, bool regexpr, 
	       enved_progress_t from_enved, const char *origin, mus_any *gen, XEN edpos, int arg_pos, Float e_base)
{
  /* four cases here: 
   *    if only one Y value in env, use scale_channel
   *    if step env (base=0), use sequence of scale_channels
   *    if linear segments and no underlying ramps, use sequence of (x)ramp_channels and scale_channels to mimic env
   *    else use mus_env and multiply every sample
   */
  snd_info *sp;
  sync_info *si;
  sync_state *sc = NULL;
  int i, j, err = 0, k, len;
  bool scalable = true, rampable = true;
  Float val[1];
  char *tmpstr = NULL;
  mus_any *egen;
  off_t *passes;
  double *rates;
  Float egen_val, base;
  double scaler, offset;
  if ((!e) && (!gen)) return;
  if (regexpr) 
    {
      /* both beg and dur are meaningless here -- use selection bounds (per chan) */
      if (selection_is_active())
	dur = selection_len();
      else 
	{
	  snd_no_active_selection_error(S_env_selection); /* can't actually happen currently (checked at all higher levels) */
	  return;
	}
    }
  if (dur == 0) return;
  if (e)
    {
      if (e->pts == 0) return;
      val[0] = e->data[1];                            /* ok because no possibility of scaler/offset here */
      for (i = 1, j = 2; i < e->pts; i++, j += 2)
	if (e->data[j + 1] != val[0]) 
	  {
	    scalable = false; 
	    break;
	  }
      if ((scalable) && (beg == 0) && (dur >= CURRENT_SAMPLES(cp)))
	{
	  scale_by(cp, val, 1, regexpr);
	  return;
	}
    }
  else scalable = false;
  si = NULL;
  sp = cp->sound;
  if (scalable) /* only true if e (not gen) and all vals are equal and not full chan (latter case handled above) */
    {
      sc = get_sync_state_without_snd_fds(sp, cp, beg, regexpr);
      if (sc == NULL) return;
      si = sc->si;
      for (i = 0; i < si->chans; i++) 
	{
	  if (regexpr)
	    scale_channel(si->cps[i], 
			  val[0], 
			  si->begs[i], 
			  selection_end(si->cps[i]) - si->begs[i] + 1, 
			  to_c_edit_position(si->cps[i], edpos, origin, arg_pos),
			  false);
	  else scale_channel(si->cps[i], val[0], si->begs[i], dur, 
			     to_c_edit_position(si->cps[i], edpos, origin, arg_pos),
			     false);
	}
      free_sync_state(sc);
      return;
    }

  if (e)
    egen = mus_make_env(e->data, e->pts, 1.0, 0.0, e_base, 0.0, 0, dur - 1, NULL); /* dur - 1 = end sample number */
  else egen = gen;
  /* must take into account possible egen scaler/offset/base and funny lengths from here on */

  len = mus_env_breakpoints(egen);
  passes = mus_env_passes(egen);
  rates = mus_env_rates(egen);
  scaler = mus_env_scaler(egen);
  offset = mus_env_offset(egen);
  base = mus_increment(egen);

  if (base == 0.0) 
    {
      /* step env -- handled as sequence of scalings */
      int local_edpos, k, pos;
      off_t segbeg, segnum, segend;
      env *newe;
      char *new_origin; /* need a complete origin since this appears as a scaled-edit in 
			 *   the edit history lists, save_edit_history needs something
			 *   that can actually recreate the original.
			 */
      /* base == 0 originally, so it's a step env */
      sc = get_sync_state_without_snd_fds(sp, cp, beg, regexpr);
      if (sc == NULL) 
	{
	  if (e) mus_free(egen);
	  return;
	}
      si = sc->si;
      for (i = 0; i < si->chans; i++) 
	{
	  if (!(editable_p(si->cps[i]))) continue;
	  segbeg = si->begs[i];
	  segend = si->begs[i] + dur;
	  segnum = passes[0] + 1;
	  local_edpos = si->cps[i]->edit_ctr; /* for as_one_edit backup */
	  pos = to_c_edit_position(si->cps[i], edpos, origin, arg_pos);
	  for (k = 0; k < len; k++)
	    {
	      if ((segbeg + segnum) > segend) 
		segnum = segend - segbeg;
	      else
		if ((k == (len - 1)) && 
		    ((segbeg + segnum) < segend))
		  segnum = segend - segbeg; /* last value is sticky in envs */
	      if (segnum > 0)
		{
		  scale_channel(si->cps[i], (Float)(offset + scaler * rates[k]), segbeg, segnum, pos, true);
		  pos = si->cps[i]->edit_ctr;
		}
	      segbeg += segnum;
	      if (segbeg >= segend) break;
	      segnum = passes[k + 1] - passes[k];
	    }
	  newe = make_envelope(mus_data(egen), mus_env_breakpoints(egen) * 2);
	  new_origin = mus_format("env-channel (make-env %s :base 0 :end " OFF_TD ") " OFF_TD " " OFF_TD,
				  tmpstr = env_to_string(newe), 
				  (len > 1) ? (passes[len - 2]) : dur,
				  si->begs[i], dur);
	  if (tmpstr) FREE(tmpstr);
	  free_env(newe);
	  as_one_edit(si->cps[i], local_edpos + 1, new_origin);
	  FREE(new_origin);
	  si->cps[i]->edit_hook_checked = false;	  
	}
      free_sync_state(sc);
      if (e) mus_free(egen);
      return;
    }

  /* step env and degenerate cases are out of the way */
  if (sp->sync)
    si = snd_sync(sp->sync);
  else si = make_simple_sync(cp, beg);
  for (i = 0; i < si->chans; i++)
    if (ramp_or_ptree_fragments_in_use(si->cps[i], 
				       si->begs[i],
				       dur,
				       to_c_edit_position(si->cps[i], edpos, origin, arg_pos),
				       base))
      {
	rampable = false;
	break;
      }
  si = free_sync_info(si);

  if (!rampable) /* must have envs already in section to be ramped */
    {
      off_t ioff;
      mus_sample_t **data;
      mus_sample_t *idata;
      bool reporting = false, temp_file = false;
      int ofd = 0, datumb = 0;
      file_info *hdr = NULL;
      char *ofile = NULL;
      snd_fd **sfs;
      snd_fd *sf = NULL;
      /* run env over samples */
      sc = get_sync_state(sp, cp, beg, regexpr, READ_FORWARD, edpos, origin, arg_pos);
      if (sc == NULL) 
	{
	  if (e) mus_free(egen);
	  return;
	}
      si = sc->si;
      sfs = sc->sfs;
      if (dur > MAX_BUFFER_SIZE) /* if smaller than this, we don't gain anything by using a temp file (its buffers are this large) */
	{
	  temp_file = true; 
	  ofile = snd_tempnam(); 
	  hdr = make_temp_header(ofile, SND_SRATE(sp), si->chans, dur, (char *)origin);
	  ofd = open_temp_file(ofile, si->chans, hdr);
	  if (ofd == -1)
	    {
	      if (e) mus_free(egen);
	      for (i = 0; i < si->chans; i++) 
		if (sfs[i]) 
		  free_snd_fd(sfs[i]);
	      free_sync_state(sc);
	      snd_error(_("can't open %s temp file %s: %s\n"), origin, ofile, strerror(errno));
	      FREE(ofile);
	      if (e) mus_free(egen);
	      return;
	    }
	  datumb = mus_bytes_per_sample(hdr->format);
	}
      else temp_file = false;
      data = (mus_sample_t **)MALLOC(si->chans * sizeof(mus_sample_t *));
      for (i = 0; i < si->chans; i++) 
	{
	  if (temp_file)
	    data[i] = (mus_sample_t *)CALLOC(FILE_BUFFER_SIZE, sizeof(mus_sample_t)); 
	  else data[i] = (mus_sample_t *)CALLOC(dur, sizeof(mus_sample_t)); 
	}
      j = 0;
      reporting = (dur > REPORTING_SIZE);
      if (reporting) start_progress_report(sp, from_enved);
      if (si->chans > 1)
	{
	  ss->stopped_explicitly = false;
	  for (ioff = 0; ioff < dur; ioff++)
	    {
	      egen_val = mus_env(egen);
	      for (k = 0; k < si->chans; k++)
		data[k][j] = (mus_sample_t)(read_sample(sfs[k]) * egen_val);
	      j++;
	      if ((temp_file) && (j == FILE_BUFFER_SIZE))
		{
		  if (reporting) 
		    progress_report(sp, origin, 0, 0, (Float)((double)ioff / ((double)dur)), from_enved);
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
	  if (temp_file)
	    {
	      ss->stopped_explicitly = false;
	      for (ioff = 0; ioff < dur; ioff++)
		{
		  idata[j] = (mus_sample_t)(read_sample(sf) * mus_env(egen));
		  j++;
		  if ((temp_file) && (j == FILE_BUFFER_SIZE))
		    {
		      if (reporting)
			progress_report(sp, origin, 0, 0, (Float)((double)ioff / ((double)dur)), from_enved);
		      err = mus_file_write(ofd, 0, j - 1, 1, data);
		      j = 0;
		      if (err == -1) break;
		      if (ss->stopped_explicitly) break;
		    }
		}
	    }
	  else
	    {
	      for (ioff = 0; ioff < dur; ioff++)
		idata[j++] = (mus_sample_t)(read_sample(sf) * mus_env(egen));
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
	  ss->stopped_explicitly = false;
	  if (temp_file) 
	    snd_remove(ofile, REMOVE_FROM_CACHE);
	}
      else
	{
	  if ((temp_file) && 
	      (si->chans > 1)) 
	    remember_temp(ofile, si->chans);
	  for (i = 0; i < si->chans; i++)
	    {
	      if (temp_file)
		{
		  int pos;
		  pos = to_c_edit_position(si->cps[i], edpos, origin, arg_pos);
		  file_change_samples(si->begs[i], dur, ofile, si->cps[i], i, 
				      (si->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME, 
				      LOCK_MIXES, origin, si->cps[i]->edit_ctr);
		  if ((si->begs[i] == 0) && (dur == si->cps[i]->samples[pos]))
		    amp_env_env(si->cps[i], mus_data(egen), len, pos, base, scaler, offset);
		  else 
		    {
		      if ((len < 2) || (snd_abs_off_t(dur - passes[len - 2]) < 2))
			amp_env_env_selection_by(si->cps[i], egen, si->begs[i], dur, pos);
		    }

		}
	      else change_samples(si->begs[i], dur, data[i], si->cps[i], LOCK_MIXES, origin, si->cps[i]->edit_ctr);
	      update_graph(si->cps[i]);
	    }
	}
      for (i = 0; i < si->chans; i++)
	{
	  sfs[i] = free_snd_fd(sfs[i]);
	  FREE(data[i]);
	}
      if ((temp_file) && (ofile)) {FREE(ofile); ofile = NULL;}
      if (data) FREE(data);
    }
  else /* rampable -- treat env as a sequence of virtual (x)ramps and scalings (if slope=0) */
    {
      int local_edpos, k, m, pos, env_pos;
      bool need_xramp = false;
      off_t segbeg, segnum, segend;
      env *newe;
      double power = 0.0;
      Float *data;
      char *new_origin; /* need a complete origin since this appears as a scaled-edit in 
			 *   the edit history lists, save_edit_history needs something
			 *   that can actually recreate the original.
			 */
      data = mus_data(egen);
      if (base != 1.0) need_xramp = true;
      sc = get_sync_state_without_snd_fds(sp, cp, beg, regexpr);
      if (sc == NULL) 
	{
	  if (e) mus_free(egen);
	  return;
	}
      si = sc->si;
      for (i = 0; i < si->chans; i++) 
	{
	  if (!(editable_p(si->cps[i]))) continue;
	  segbeg = si->begs[i];
	  segend = si->begs[i] + dur;
	  segnum = passes[0] + 1;
	  local_edpos = si->cps[i]->edit_ctr; /* for as_one_edit backup */
	  pos = to_c_edit_position(si->cps[i], edpos, origin, arg_pos);
	  env_pos = pos;
	  for (k = 0, m = 1; k < len; k++, m += 2)
	    {
	      if ((segbeg + segnum) > segend) 
		segnum = segend - segbeg;
	      else
		if ((k == (len - 1)) && 
		    ((segbeg + segnum) < segend))
		  segnum = segend - segbeg; /* last value is sticky in envs */
	      if (segnum > 0)
		{
		  if (k == 0) 
		    {
		      if (need_xramp)
			{
			  power = mus_env_initial_power(egen);
			  xramp_channel(si->cps[i], 
					(Float)power,
					power + rates[0] * (segnum - 1),
					scaler, offset, segbeg, segnum, pos, true, egen, 0);
			  power += rates[0] * (segnum - 1);
			}
		      else ramp_channel(si->cps[i], 
					(Float)(offset + scaler * data[m]),
					(Float)(offset + scaler * data[m + 2]), 
					segbeg, segnum, pos, true);
		    }
		  else 
		    {
		      if (need_xramp)
			/* divide by segnum since we end at the break point and don't want to repeat it, so go to next position in env */
			{
			  xramp_channel(si->cps[i], 
					power + rates[k],
					power + rates[k] * segnum,
					scaler, offset, segbeg, segnum, pos, true, egen, k);
			  power += rates[k] * segnum;
			}
		      else ramp_channel(si->cps[i], 
					(Float)(offset + (scaler * (data[m] + ((data[m + 2] - data[m]) / (double)segnum)))),
					(Float)(offset + scaler * data[m + 2]), 
					segbeg, segnum, pos, true);
		    }
		  pos = si->cps[i]->edit_ctr;
		}
	      segbeg += segnum;
	      if (segbeg >= segend) break;
	      segnum = passes[k + 1] - passes[k];
	    }
	  if ((si->begs[i] == 0) && (dur == si->cps[i]->samples[env_pos]))
	    amp_env_env(si->cps[i], mus_data(egen), len, env_pos, base, scaler, offset);
	  else 
	    {
	      if ((len < 2) || (snd_abs_off_t(dur - passes[len - 2]) < 2))
		amp_env_env_selection_by(si->cps[i], egen, si->begs[i], dur, env_pos);
	    }
	  newe = make_envelope(mus_data(egen), mus_env_breakpoints(egen) * 2);
	  new_origin = mus_format("env-channel (make-env %s :base %.4f :end " OFF_TD ") " OFF_TD " " OFF_TD,
				  tmpstr = env_to_string(newe), 
				  base,
				  (len > 1) ? (passes[len - 2]) : dur,
				  si->begs[i], dur);
	  if (tmpstr) FREE(tmpstr);
	  free_env(newe);
	  as_one_edit(si->cps[i], local_edpos + 1, new_origin);
	  update_graph(si->cps[i]);
	  si->cps[i]->edit_hook_checked = false;	  
	  FREE(new_origin);
	}
    }
  if (e) mus_free(egen);
  free_sync_state(sc);
}

void cursor_delete(chan_info *cp, off_t count, const char *origin)
{
  int i;
  off_t beg;
  snd_info *sp;
  sync_info *si;
  chan_info **cps;
  si = NULL;
  if (count == 0) return;
  if (count > 0)
    beg = CURSOR(cp);
  else
    {
      count = -count;
      beg = CURSOR(cp) - count;
      if (beg < 0)
	{
	  count += beg;
	  beg = 0;
	  if (count <= 0) return;
	}
    }
  sp = cp->sound;
  if (sp->sync != 0)
    {
      si = snd_sync(sp->sync);
      cps = si->cps;
      for (i = 0; i < si->chans; i++)
	{
	  if (delete_samples(beg, count, cps[i], origin, cps[i]->edit_ctr))
	    {
	      CURSOR(cps[i]) = beg;
	      update_graph(si->cps[i]);
	    }
	}
      si = free_sync_info(si);
    }
  else
    {
      if (delete_samples(beg, count, cp, origin, cp->edit_ctr))
	{
	  CURSOR(cp) = beg;
	  update_graph(cp);
	}
    }
}

void cursor_insert(chan_info *cp, off_t beg, off_t count, const char *origin)
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
      si = snd_sync(sp->sync);
      cps = si->cps;
      for (i = 0; i < si->chans; i++)
	{
	  if (extend_with_zeros(cps[i], 
				mus_oclamp(0, beg, CURRENT_SAMPLES(si->cps[i])), 
				count, origin,
				cps[i]->edit_ctr))
	    update_graph(cps[i]);
	}
      si = free_sync_info(si);
    }
  else 
    {
      if (extend_with_zeros(cp, 
			    mus_oclamp(0, beg, CURRENT_SAMPLES(cp)), 
			    count, origin,
			    cp->edit_ctr))
	update_graph(cp);
    }
}

void cursor_zeros(chan_info *cp, off_t count, bool regexpr)
{
  int i, old_sync;
  off_t beg, num;
  snd_info *sp, *nsp;
  sync_info *si = NULL;
  chan_info *ncp;
  Float scaler[1];
  if (count == 0) return;
  if (count < 0) num = -count; else num = count;
  sp = cp->sound;
  if ((sp->sync != 0) && (!regexpr))
    {
      si = snd_sync(sp->sync);
      for (i = 0; i < si->chans; i++) 
	si->begs[i] = CURSOR(cp);
    }
  else
    {
      if ((regexpr) && (selection_is_active()))
	{
	  si = selection_sync();
	  num = selection_len();
	}
    }
  if (!si) si = make_simple_sync(cp, CURSOR(cp));
  for (i = 0; i < si->chans; i++)
    {
      /* if zeroing entire sound, set scalers and remake amp_env */
      ncp = si->cps[i];
      if ((si->begs[i] == 0) && 
	  (num >= CURRENT_SAMPLES(ncp)))
	{
	  nsp = ncp->sound;
	  old_sync = nsp->sync;
	  nsp->sync = 0;
	  scaler[0] = 0.0;
	  scale_by(ncp, scaler, 1, false);
	  nsp->sync = old_sync;
	}
      else
	{
	  if (count > 0) 
	    beg = si->begs[i];
	  else beg = si->begs[i] + count;
	  /* special case 1 sample -- if already 0, treat as no-op */
	  if ((count != 1) || 
	      (beg >= CURRENT_SAMPLES(ncp)) || 
	      (chn_sample(beg, ncp, ncp->edit_ctr) != 0.0))
	    scale_channel(ncp, 0.0, beg, num, ncp->edit_ctr, false);
	}
    }
  si = free_sync_info(si);
}

static void smooth_channel(chan_info *cp, off_t beg, off_t dur, int edpos, const char *origin)
{
  mus_sample_t *data = NULL;
  off_t k;
  Float y0, y1, angle, incr, off, scale;
  if ((beg < 0) || (dur <= 0)) return;
  if (!(editable_p(cp))) return;
  if ((beg + dur) > cp->samples[edpos]) 
    {
      dur = cp->samples[edpos] - beg;
      if (dur <= 0) return;
    }
  y0 = chn_sample(beg, cp, edpos);
  y1 = chn_sample(beg + dur, cp, edpos); /* one past end -- this is a debatable choice */
  if (y1 > y0) angle = M_PI; else angle = 0.0;
  incr = M_PI / (double)dur;
  off = 0.5 * (y1 + y0);
  scale = 0.5 * fabs(y0 - y1);
  data = (mus_sample_t *)CALLOC(dur, sizeof(mus_sample_t));
  for (k = 0; k < dur; k++, angle += incr) 
    data[k] = MUS_FLOAT_TO_SAMPLE(off + scale * cos(angle));
  change_samples(beg, dur, data, cp, LOCK_MIXES, origin, cp->edit_ctr);
  update_graph(cp);
  cp->edit_hook_checked = false;
  FREE(data);
}

void cos_smooth(chan_info *cp, off_t beg, off_t num, bool regexpr, const char *origin)
{
  /* verbatim, so to speak from Dpysnd */
  /* start at beg, apply a cosine for num samples, matching endpoints */
  sync_state *sc;
  int i;
  snd_info *sp;
  sync_info *si;
  sp = cp->sound;
  sc = get_sync_state_without_snd_fds(sp, cp, beg, regexpr);
  if (sc == NULL) return;
  si = sc->si;
  if (regexpr) num = sc->dur;
  for (i = 0; i < si->chans; i++)
    smooth_channel(si->cps[i], si->begs[i], num, si->cps[i]->edit_ctr, origin);
  free_sync_state(sc);
}

static char *run_channel(chan_info *cp, void *upt, off_t beg, off_t dur, int edpos)
{
  snd_info *sp;
  file_info *hdr = NULL;
  int j, ofd = 0, datumb = 0, err = 0;
  bool temp_file;
  off_t k;
  mus_sample_t **data;
  mus_sample_t *idata;
  char *ofile = NULL;
  snd_fd *sf;
  if ((beg < 0) || (dur <= 0)) return(NULL);
  if (!(editable_p(cp))) return(NULL);
  sp = cp->sound;
  sf = init_sample_read_any(beg, cp, READ_FORWARD, edpos);
  if (sf == NULL) 
    {
      cp->edit_hook_checked = false;
      return(mus_format(_("run-channel: can't read %s[%d] channel data!"), sp->short_filename, cp->chan));
    }
  if (dur > MAX_BUFFER_SIZE)
    {
      temp_file = true; 
      ofile = snd_tempnam();
      hdr = make_temp_header(ofile, SND_SRATE(sp), 1, dur, S_map_channel);
      ofd = open_temp_file(ofile, 1, hdr);
      if (ofd == -1)
	{
	  free_snd_fd(sf); 
	  cp->edit_hook_checked = false;
	  return(mus_format(_("can't open run-channel temp file %s: %s\n"), ofile, strerror(errno)));
	}
      datumb = mus_bytes_per_sample(hdr->format);
    }
  else temp_file = false;
  data = (mus_sample_t **)MALLOC(sizeof(mus_sample_t *));
  data[0] = (mus_sample_t *)CALLOC(MAX_BUFFER_SIZE, sizeof(mus_sample_t)); 
  idata = data[0];
  if (temp_file)
    {
      j = 0;
      for (k = 0; k < dur; k++)
	{
	  idata[j++] = MUS_FLOAT_TO_SAMPLE(evaluate_ptree_1f2f(upt, read_sample_to_float(sf)));
	  if (j == MAX_BUFFER_SIZE)
	    {
	      err = mus_file_write(ofd, 0, j - 1, 1, data);
	      j = 0;
	      if (err == -1) break;
	    }
	}
      if (j > 0) mus_file_write(ofd, 0, j - 1, 1, data);
      close_temp_file(ofd, hdr, dur * datumb, sp);
      hdr = free_file_info(hdr);
      if (err != -1)
	file_change_samples(beg, dur, ofile, cp, 0, DELETE_ME, LOCK_MIXES, S_map_channel, cp->edit_ctr);
      if (ofile) 
	{
	  FREE(ofile); 
	  ofile = NULL;
	}
    }
  else 
    {
      if (dur > 0) 
	{
	  for (k = 0; k < dur; k++)
	    idata[k] = MUS_FLOAT_TO_SAMPLE(evaluate_ptree_1f2f(upt, read_sample_to_float(sf)));
	  change_samples(beg, dur, idata, cp, LOCK_MIXES, S_map_channel, cp->edit_ctr);
	}
    }
  update_graph(cp); 
  free_snd_fd(sf);
  FREE(data[0]);
  FREE(data);
  cp->edit_hook_checked = false;
  return(NULL);
}

#define MUS_OUTA_1(Frame, Val, Fd) ((*(Fd->core)->write_sample))(Fd, Frame, 0, Val)
/* avoids all the CLM error checking */

static XEN g_map_chan_1(XEN proc_and_list, XEN s_beg, XEN s_end, XEN org, XEN snd, XEN chn, XEN edpos, XEN s_dur, char *fallback_caller) 
{ 
  chan_info *cp;
  char *caller;
  off_t beg = 0, end = 0, dur = 0;
  snd_info *sp;
  snd_fd *sf = NULL;
  XEN errstr;
  off_t kp, len, j = 0, num;
  int rpt = 0, rpt4, i, cured, pos;
  bool reporting = false;
  XEN res = XEN_FALSE;
  char *errmsg;
  char *filename;
  mus_any *outgen = NULL;
  XEN *data;
  vct *v;
  XEN proc = XEN_FALSE;
  void *pt = NULL;
  if (XEN_LIST_P(proc_and_list))
    proc = XEN_CADR(proc_and_list);
  else proc = proc_and_list;
  if (XEN_STRING_P(org)) 
    caller = XEN_TO_C_STRING(org);
  else caller = fallback_caller;
  XEN_ASSERT_TYPE((XEN_PROCEDURE_P(proc)), proc, XEN_ARG_1, caller, "a procedure");
  ASSERT_SAMPLE_TYPE(caller, s_beg, XEN_ARG_2);
  ASSERT_SAMPLE_TYPE(caller, s_end, XEN_ARG_3);
  ASSERT_SAMPLE_TYPE(caller, s_dur, XEN_ARG_3);
  ASSERT_CHANNEL(caller, snd, chn, 5); 
  cp = get_cp(snd, chn, caller);
  pos = to_c_edit_position(cp, edpos, caller, 7);
  beg = beg_to_sample(s_beg, caller);
  if (beg > cp->samples[pos])
    XEN_ERROR(NO_SUCH_SAMPLE,
	      XEN_LIST_2(C_TO_XEN_STRING(caller),
			 s_beg));
  if (!(editable_p(cp))) return(XEN_FALSE);
  if (XEN_FALSE_P(s_dur))
    end = end_to_sample(s_end, cp, pos, caller);
  else dur = dur_to_samples(s_dur, beg, cp, pos, 8, caller);
  if (end == 0) 
    {
      if (dur != 0) 
	end = beg + dur - 1;
      else end = cp->samples[pos] - 1;
    }
  num = end - beg + 1;
  if (num > 0)
    {
      errmsg = procedure_ok(proc, 1, caller, "", 1);
      if (errmsg)
	{
	  errstr = C_TO_XEN_STRING(errmsg);
	  FREE(errmsg);
	  cp->edit_hook_checked = false;
	  return(snd_bad_arity_error(caller, errstr, proc));
	}

      if (optimization(ss) > 0)
	{
	  pt = form_to_ptree_1_f(proc_and_list);
	  if (pt)
	    {
	      char *err_str;
	      err_str = run_channel(cp, pt, beg, num, pos);
	      free_ptree(pt);
	      if (err_str == NULL)
		{
		  cp->edit_hook_checked = false;
		  return(XEN_ZERO);
		}
	      else FREE(err_str); /* and fallback on normal evaluator */
	    }
	}
      sp = cp->sound;
      reporting = (num > REPORTING_SIZE);
      if (reporting) start_progress_report(sp, NOT_FROM_ENVED);
      sf = init_sample_read_any(beg, cp, READ_FORWARD, pos);
      if (sf == NULL) 
	{
	  cp->edit_hook_checked = false;
	  return(XEN_TRUE);
	}
      rpt4 = MAX_BUFFER_SIZE / 4;
      filename = snd_tempnam();
      outgen = mus_make_sample2file(filename, 1, MUS_OUT_FORMAT, MUS_NEXT);
      j = 0;
      ss->stopped_explicitly = false;
      for (kp = 0; kp < num; kp++)
	{
	  /* changed here to remove catch 24-Mar-02 */
	  res = XEN_CALL_1_NO_CATCH(proc, 
				    C_TO_XEN_DOUBLE((double)read_sample_to_float(sf)),
				    caller);
	  if (XEN_NUMBER_P(res))                         /* one number -> replace current sample */
	    MUS_OUTA_1(j++, XEN_TO_C_DOUBLE(res), outgen);
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
			    MUS_OUTA_1(j++, XEN_TO_C_DOUBLE(data[i]), outgen);
			}
		      else
			{
			  if (VCT_P(res))
			    {
			      v = TO_VCT(res);
			      for (i = 0; i < v->length; i++) 
				MUS_OUTA_1(j++, v->data[i], outgen);
			    }
			  else
			    {
			      if (XEN_LIST_P(res))
				{
				  len = XEN_LIST_LENGTH(res);
				  for (i = 0; i < len; i++, res = XEN_CDR(res)) 
				    MUS_OUTA_1(j++, XEN_TO_C_DOUBLE(XEN_CAR(res)), outgen);
				}
			      else 
				{
				  if (outgen) mus_free(outgen);
				  if (sf) sf = free_snd_fd(sf);
				  if (reporting) finish_progress_report(sp, NOT_FROM_ENVED);
				  snd_remove(filename, REMOVE_FROM_CACHE);
				  FREE(filename);
				  cp->edit_hook_checked = false;
				  XEN_ERROR(BAD_TYPE,
					    XEN_LIST_3(C_TO_XEN_STRING(caller),
						       C_TO_XEN_STRING("result of procedure must be a number, boolean, vct, list, or vector:"),
						       res));
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
		  progress_report(sp, caller, 1, 1, (Float)((double)kp / (double)num), NOT_FROM_ENVED);
		  rpt = 0;		    
		}
	    }
	  if (ss->stopped_explicitly) 
	    break;
	}
      if (outgen) mus_free(outgen);
      if (sf) sf = free_snd_fd(sf);
      if (reporting) finish_progress_report(sp, NOT_FROM_ENVED);
      if (ss->stopped_explicitly) 
	ss->stopped_explicitly = false;
      else
	{
	  if (j == num)
	    file_change_samples(beg, j, filename, cp, 0, DELETE_ME, LOCK_MIXES, caller, cp->edit_ctr);
	  else
	    {
	      delete_samples(beg, num, cp, caller, cp->edit_ctr);
	      cured = cp->edit_ctr;
	      if (j > 0)
		{
		  file_insert_samples(beg, j, filename, cp, 0, DELETE_ME, caller, cp->edit_ctr);
		  backup_edit_list(cp);
		  if (cp->edit_ctr > cured)
		    backup_edit_list(cp);
		  ripple_trailing_marks(cp, beg, num, j);
		}
	      else snd_remove(filename, REMOVE_FROM_CACHE);
	    }
	  update_graph(cp);
	}
      FREE(filename);
    }
  cp->edit_hook_checked = false;
  return(res);
}

static XEN g_map_chan_ptree_fallback(XEN proc, XEN init_func, chan_info *cp, off_t beg, off_t num, int pos)
{ 
  snd_fd *sf = NULL;
  off_t kp;
  bool temp_file;
  XEN res = XEN_FALSE;
  char *filename = NULL;
  mus_any *outgen = NULL;
  XEN v;
  mus_sample_t *data = NULL;
  if (!(editable_p(cp))) return(XEN_FALSE);
  sf = init_sample_read_any(beg, cp, READ_FORWARD, pos);
  if (sf == NULL) 
    {
      cp->edit_hook_checked = false;
      return(XEN_TRUE);
    }
  temp_file = (num > MAX_BUFFER_SIZE);
  if (temp_file)
    {
      filename = snd_tempnam();
      outgen = mus_make_sample2file(filename, 1, MUS_OUT_FORMAT, MUS_NEXT);
      if (XEN_PROCEDURE_P(init_func))
	{
	  int loc;
	  v = XEN_CALL_2(init_func,
		     C_TO_XEN_OFF_T(0),
		     C_TO_XEN_OFF_T(num),
		     "ptree-channel fallback init func");
	  loc = snd_protect(v);
	  for (kp = 0; kp < num; kp++)
	    {
	      res = XEN_CALL_3(proc, 
			       C_TO_XEN_DOUBLE((double)read_sample_to_float(sf)),
			       v,
			       XEN_TRUE,
			       "ptree-channel fallback proc");
	      MUS_OUTA_1(kp, XEN_TO_C_DOUBLE(res), outgen);
	    }
	  snd_unprotect_at(loc);
	}
      else
	{
	  for (kp = 0; kp < num; kp++)
	    {
	      res = XEN_CALL_1(proc, 
			       C_TO_XEN_DOUBLE((double)read_sample_to_float(sf)),
			       "ptree-channel fallback proc");
	      MUS_OUTA_1(kp, XEN_TO_C_DOUBLE(res), outgen);
	    }
	}
      if (outgen) mus_free(outgen);
    }
  else
    {
      data = (mus_sample_t *)CALLOC(num, sizeof(mus_sample_t)); 
      if (XEN_PROCEDURE_P(init_func))
	{
	  int loc;
	  v = XEN_CALL_2(init_func,
		     C_TO_XEN_OFF_T(0),
		     C_TO_XEN_OFF_T(num),
		     "ptree-channel fallback init func");
	  loc = snd_protect(v);
	  for (kp = 0; kp < num; kp++)
	    {
	      res = XEN_CALL_3(proc, 
			       C_TO_XEN_DOUBLE((double)read_sample_to_float(sf)),
			       v,
			       XEN_TRUE,
			       "ptree-channel fallback proc");
	      data[kp] = MUS_DOUBLE_TO_SAMPLE(XEN_TO_C_DOUBLE(res));
	    }
	  snd_unprotect_at(loc);
	}
      else
	{
	  for (kp = 0; kp < num; kp++)
	    {
	      res = XEN_CALL_1(proc, 
			       C_TO_XEN_DOUBLE((double)read_sample_to_float(sf)),
			       "ptree-channel fallback proc");
	      data[kp] = MUS_DOUBLE_TO_SAMPLE(XEN_TO_C_DOUBLE(res));
	    }
	}
    }
  free_snd_fd(sf);
  if (temp_file)
    {
      file_change_samples(beg, num, filename, cp, 0, DELETE_ME, LOCK_MIXES, "ptree-channel fallback", cp->edit_ctr);
      FREE(filename);
    }
  else 
    {
      change_samples(beg, num, data, cp, LOCK_MIXES, "ptree-channel fallback", cp->edit_ctr);
      FREE(data);
    }
  update_graph(cp); 
  cp->edit_hook_checked = false;
  return(proc);
}

static XEN g_ptree_channel(XEN proc_and_list, XEN s_beg, XEN s_dur, XEN snd, XEN chn, 
			   XEN edpos, XEN env_too, XEN init_func, XEN use_map_channel_fallback)
{
  #define H_ptree_channel "(" S_ptree_channel " proc (beg 0) (dur len) (snd #f) (chn #f) (edpos #f) (peak-env-also #f) (init-func #f) (use-map-channel-fallback #t)): \
apply 'proc' as a 'virtual edit'; that is, the effect of 'proc' (a function of one argument, the \
current sample, if init-func is not specified), comes about as an implicit change in the way the data is read.  \
This is similar to scaling and some envelope operations in that no data actually changes.  If 'peak-env-also' is #t, \
the same function is applied to the peak env values to get the new version. \
If 'proc' needs some state, it can be supplied in a vct returned by 'init-func'. \
'init-func' is a function of 2 args, the current fragment-relative begin position, \
and the overall fragment duration. In this case, 'proc' is a function of 3 args: \
the current sample, the vct returned by 'init-func', and the current read direction. \
'map-fallback', can be set to #f if you want to try out a more general virtual operator: 'proc' and 'init-func' can \
be anything legal in Snd, and the actual forms are evaluated as virtual ops \
at run-time.  See extsnd.html for the gory details."

  chan_info *cp;
  off_t beg = 0, dur = 0;
  int pos;
  bool ptrees_present = false;
  XEN proc = XEN_FALSE;
  void *pt = NULL;
  /* (ptree-channel (lambda (y) (* y 2))) -> ((lambda (y) (* y 2)) #<procedure #f ((y) (* y 2))>) as "proc_and_list" */
  /*   the cadr proc gives access to the environment, run walks the car */
  if (XEN_LIST_P(proc_and_list))
    proc = XEN_CADR(proc_and_list);
  XEN_ASSERT_TYPE((XEN_PROCEDURE_P(proc)) && ((XEN_REQUIRED_ARGS(proc) == 1) || (XEN_REQUIRED_ARGS(proc) == 3)),
		  proc, XEN_ARG_1, S_ptree_channel, "a procedure of one or three args");
  ASSERT_SAMPLE_TYPE(S_ptree_channel, s_beg, XEN_ARG_2);
  ASSERT_SAMPLE_TYPE(S_ptree_channel, s_dur, XEN_ARG_3);
  ASSERT_CHANNEL(S_ptree_channel, snd, chn, 4); 
  cp = get_cp(snd, chn, S_ptree_channel);
  pos = to_c_edit_position(cp, edpos, S_ptree_channel, 6);
  if (pos > cp->edit_ctr)
    {
      cp->edit_hook_checked = false;
      XEN_ERROR(NO_SUCH_EDIT,
		XEN_LIST_3(C_TO_XEN_STRING(S_ptree_channel),
			   C_TO_XEN_STRING("edpos: ~A, ~A chan ~A has ~A edits"),
			   XEN_LIST_4(edpos,
				      C_TO_XEN_STRING(cp->sound->short_filename),
				      chn,
				      C_TO_XEN_INT(cp->edit_ctr))));
    }
  beg = beg_to_sample(s_beg, S_ptree_channel);
  if (beg > cp->samples[pos])
    XEN_ERROR(NO_SUCH_SAMPLE,
	      XEN_LIST_2(C_TO_XEN_STRING(S_ptree_channel),
			 s_beg));
  dur = dur_to_samples(s_dur, beg, cp, pos, 3, S_ptree_channel);
  if (dur <= 0) return(XEN_FALSE);
  ptrees_present = ptree_fragments_in_use(cp, beg, dur, pos, XEN_FALSE_P(use_map_channel_fallback));
  if (XEN_PROCEDURE_P(init_func))
    {
      if (XEN_REQUIRED_ARGS(init_func) != 2)
	XEN_BAD_ARITY_ERROR(S_ptree_channel, 8, init_func, "init-func must take 2 args");
      if (XEN_REQUIRED_ARGS(proc) != 3)
	XEN_BAD_ARITY_ERROR(S_ptree_channel, 1, proc, "main func must take 3 args if the init-func is present");
      if (!ptrees_present)
	{
	  pt = form_to_ptree_3_f(proc_and_list);
	  if (pt)
	    {
	      ptree_channel(cp, pt, beg, dur, pos, XEN_TRUE_P(env_too), init_func, false);
	      return(proc_and_list);
	    }
	  /* ptree not ok -- use map chan? */
	  if (XEN_FALSE_P(use_map_channel_fallback))
	    {
	      ptree_channel(cp, (void *)proc, beg, dur, pos, XEN_TRUE_P(env_too), init_func, true);
	      return(proc_and_list);
	    }
	}
      /* fallback on map chan */
      g_map_chan_ptree_fallback(proc, init_func, cp, beg, dur, pos);
      return(proc_and_list);
    }
  if (XEN_REQUIRED_ARGS(proc) == 1)
    pt = form_to_ptree_1_f(proc_and_list);
  if (pt)
    {
      if (ptrees_present)
	{
	  run_channel(cp, pt, beg, dur, pos);
	  pt = free_ptree(pt);
	}
      else ptree_channel(cp, pt, beg, dur, pos, XEN_TRUE_P(env_too), init_func, false);
    }
  else
    {
      if ((XEN_FALSE_P(use_map_channel_fallback)) && (!ptrees_present))
	{
	  if (!(XEN_PROCEDURE_P(init_func)))
	    {
	      if (XEN_REQUIRED_ARGS(proc) != 3)
		XEN_BAD_ARITY_ERROR(S_ptree_channel, 1, proc, "main func must take 3 args if it can't be optimized");
	    }
	  ptree_channel(cp, (void *)proc, beg, dur, pos, XEN_TRUE_P(env_too), init_func, true);
	}
      else g_map_chan_ptree_fallback(proc, init_func, cp, beg, dur, pos);
    }
  return(proc_and_list);
}


#if HAVE_DYNAMIC_WIND
typedef struct {
  XEN proc;
  off_t beg, num;
  snd_fd *sf;
  snd_info *sp;
  const char *caller;
  bool counting;
  bool reporting;
} scan_context;

static scan_context *make_scan_context(XEN p, off_t b, off_t n, snd_fd *f, snd_info *s, const char *orig, bool count)
{
  scan_context *sc;
  sc = (scan_context *)CALLOC(1, sizeof(scan_context));
  sc->proc = p;
  sc->beg = b;
  sc->num = n;
  sc->sf = f;
  sc->sp = s;
  sc->caller = orig;
  sc->counting = count;
  sc->reporting = false;
  return(sc);
}

static void before_scan(void *ignore)
{
  /* we could possibly catch call/cc into previous scan here, but that requires an smob for the context */
  /*   put init_sample_read here, update sc->beg in scan body, don't free context explicitly */
}

static XEN scan_body(void *context)
{
  off_t kp;
  int counts = 0, rpt = 0, rpt4 = 0;
  XEN res = XEN_FALSE;
  scan_context *sc = (scan_context *)context;
  sc->reporting = (sc->num > REPORTING_SIZE);
  rpt4 = MAX_BUFFER_SIZE / 4;
  if (sc->reporting) start_progress_report(sc->sp, NOT_FROM_ENVED);
  ss->stopped_explicitly = false;
  for (kp = 0; kp < sc->num; kp++)
    {
      res = XEN_CALL_1_NO_CATCH(sc->proc,
				C_TO_XEN_DOUBLE((double)read_sample_to_float(sc->sf)),
				sc->caller);
      if (XEN_NOT_FALSE_P(res))
	{
	  if ((sc->counting) &&
	      (XEN_TRUE_P(res)))
	    counts++;
	  else
	    return(XEN_LIST_2(res,
			      C_TO_XEN_OFF_T(kp + sc->beg)));
	}
      if (sc->reporting) 
	{
	  rpt++;
	  if (rpt > rpt4)
	    {
	      progress_report(sc->sp, sc->caller, 1, 1, (Float)((double)kp / (double)(sc->num)), NOT_FROM_ENVED);
	      rpt = 0;
	    }
	}
      if (ss->stopped_explicitly)
	{
	  ss->stopped_explicitly = false;
	  report_in_minibuffer(sc->sp, _("C-G stopped %s at sample " PRId64), 
			       sc->caller, kp + sc->beg);
	  break;
	}
    }
  if (sc->counting)
    return(C_TO_XEN_INT(counts));
  return(XEN_FALSE);
}

static void after_scan(void *context)
{
  scan_context *sc = (scan_context *)context;
  if (sc->sf) sc->sf = free_snd_fd(sc->sf);
  if (sc->reporting) finish_progress_report(sc->sp, NOT_FROM_ENVED);
  FREE(sc);
}
#endif

static XEN g_sp_scan(XEN proc_and_list, XEN s_beg, XEN s_end, XEN snd, XEN chn, 
		     const char *caller, bool counting, XEN edpos, int arg_pos, XEN s_dur)
{
  chan_info *cp;
  off_t beg = 0, end = 0, dur = 0;
  snd_info *sp;
  snd_fd *sf;
  XEN errstr;
  off_t kp, num;
#if (!HAVE_DYNAMIC_WIND)
  int rpt = 0, rpt4;
  bool reporting = false;
  XEN res;
#endif
  int counts = 0, pos;
  char *errmsg;
  XEN proc = XEN_FALSE;
  void *pt = NULL;
  if (XEN_LIST_P(proc_and_list))
    proc = XEN_CADR(proc_and_list);
  else proc = proc_and_list;
  XEN_ASSERT_TYPE((XEN_PROCEDURE_P(proc)), proc, XEN_ARG_1, caller, "a procedure");
  ASSERT_SAMPLE_TYPE(caller, s_beg, XEN_ARG_2);
  ASSERT_SAMPLE_TYPE(caller, s_end, XEN_ARG_3);
  ASSERT_SAMPLE_TYPE(caller, s_dur, XEN_ARG_3);
  ASSERT_CHANNEL(caller, snd, chn, 4);
  cp = get_cp(snd, chn, caller);
  pos = to_c_edit_position(cp, edpos, caller, arg_pos);
  beg = beg_to_sample(s_beg, caller);
  if (beg > cp->samples[pos])
    XEN_ERROR(NO_SUCH_SAMPLE,
	      XEN_LIST_2(C_TO_XEN_STRING(caller),
			 s_beg));
  if (XEN_FALSE_P(s_dur))
    end = end_to_sample(s_end, cp, pos, caller);
  else dur = dur_to_samples(s_dur, beg, cp, pos, 3, caller);
  errmsg = procedure_ok(proc, 1, caller, "", 1);
  if (errmsg)
    {
      errstr = C_TO_XEN_STRING(errmsg);
      FREE(errmsg);
      return(snd_bad_arity_error(caller, errstr, proc));
    }
  sp = cp->sound;
  if (end == 0) 
    {
      if (dur != 0)
	end = beg + dur - 1;
      else end = cp->samples[pos] - 1;
    }
  num = end - beg + 1;
  if (num <= 0) return(XEN_FALSE);
  sf = init_sample_read_any(beg, cp, READ_FORWARD, pos);
  if (sf == NULL) return(XEN_TRUE);

  if (optimization(ss) > 0)
    {
      pt = form_to_ptree_1_b(proc_and_list);
      if (pt)
	{
	  for (kp = 0; kp < num; kp++)
	    if (evaluate_ptree_1f2b(pt, read_sample_to_float(sf)))
	      {
		if (counting)
		  counts++;
		else
		  {
		    sf = free_snd_fd(sf);
		    free_ptree(pt);
		    return(XEN_LIST_2(XEN_TRUE,
				      C_TO_XEN_OFF_T(kp + beg)));
		  }
	      }
	  sf = free_snd_fd(sf);
	  free_ptree(pt);
	  if (counting)
	    return(C_TO_XEN_INT(counts));
	  return(XEN_FALSE);
	}
    }
#if HAVE_DYNAMIC_WIND
  /* package up context and try to protect against errors/throws in the user's code */
  {
    scan_context *sc;
    sc = make_scan_context(proc, beg, num, sf, sp, caller, counting);
    return(scm_internal_dynamic_wind((scm_t_guard)before_scan, 
				     (scm_t_inner)scan_body, 
				     (scm_t_guard)after_scan, 
				     (void *)sc,
				     (void *)sc));
  }
#else
  reporting = (num > REPORTING_SIZE);
  if (reporting) start_progress_report(sp, NOT_FROM_ENVED);
  rpt4 = MAX_BUFFER_SIZE / 4;
  ss->stopped_explicitly = false;
  for (kp = 0; kp < num; kp++)
    {
      res = XEN_CALL_1_NO_CATCH(proc,
				C_TO_XEN_DOUBLE((double)read_sample_to_float(sf)),
				caller);
      /* leak here -- if reader active and error occurs, we jump out without cleanup */
      /* see dynamic_wind above */
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
				C_TO_XEN_OFF_T(kp + beg)));
	    }
	}
      if (reporting) 
	{
	  rpt++;
	  if (rpt > rpt4)
	    {
	      progress_report(sp, caller, 1, 1, (Float)((double)kp / (double)num), NOT_FROM_ENVED);
	      rpt = 0;
	    }
	}
      if (ss->stopped_explicitly)
	{
	  ss->stopped_explicitly = false;
	  report_in_minibuffer(sp, _("C-G stopped %s at sample " PRId64), 
			       caller, kp + beg);
	  break;
	}
    }
  if (reporting) finish_progress_report(sp, NOT_FROM_ENVED);
  if (sf) sf = free_snd_fd(sf);
  if (counting)
    return(C_TO_XEN_INT(counts));
#endif
  return(XEN_FALSE);
}

static XEN g_scan_chan(XEN proc, XEN beg, XEN end, XEN snd, XEN chn, XEN edpos) 
{ 
  #define H_scan_chan "(" S_scan_chan " func (start 0) (end len) (snd #f) (chn #f) (edpos #f)): \
apply 'func' to samples in current channel (or the specified channel). \
'func' is a function of one argument, the current sample. \
if 'func' returns non-#f, the scan stops, and the value is returned to the caller with the sample number.\n\
  (scan-chan (lambda (x) (> x .1)))"

  ASSERT_CHANNEL(S_scan_chan, snd, chn, 4); 
  return(g_sp_scan(proc, beg, end, snd, chn, S_scan_chan, false, edpos, 6, XEN_FALSE));
}

static XEN g_scan_channel(XEN proc, XEN beg, XEN dur, XEN snd, XEN chn, XEN edpos) 
{ 
  #define H_scan_channel "(" S_scan_channel " func (start 0) (dur len) (snd #f) (chn #f) (edpos #f)): \
apply func to samples in current channel (or the specified channel) \
func is a function of one argument, the current sample. \
if func returns non-#f, the scan stops, and the value is returned to the caller with the sample number. \n\
  (scan-channel (lambda (x) (> x .1)))"

  ASSERT_CHANNEL(S_scan_channel, snd, chn, 4); 
  return(g_sp_scan(proc, beg, XEN_FALSE, snd, chn, S_scan_channel, false, edpos, 6, (XEN_BOUND_P(dur)) ? dur : XEN_FALSE));
}

static XEN g_map_chan(XEN proc, XEN s_beg, XEN s_end, XEN org, XEN snd, XEN chn, XEN edpos) 
{
#define H_map_chan "(" S_map_chan " func (start 0) (end len) (edname #f) (snd #f) (chn #f) (edpos #f)): \
apply func to samples in current channel; edname is the edit history name for this editing operation.\n\
  (map-chan abs)"
  return(g_map_chan_1(proc, s_beg, s_end, org, snd, chn, edpos, XEN_FALSE, S_map_chan));
}

static XEN g_map_channel(XEN proc, XEN s_beg, XEN s_dur, XEN snd, XEN chn, XEN edpos, XEN org) 
{
  #define H_map_channel "(" S_map_channel " func (start 0) (dur len) (snd #f) (chn #f) (edpos #f) (edname #f)): \
apply func to samples in current channel; edname is the edit history name for this editing operation.\n\
  (map-channel abs)"
  return(g_map_chan_1(proc, s_beg, XEN_FALSE, org, snd, chn, edpos, (XEN_BOUND_P(s_dur)) ? s_dur : XEN_FALSE, S_map_channel));
}

static XEN g_find(XEN expr, XEN sample, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_find "(" S_find " func (start-samp 0) (snd #f) (chn #f) (edpos #f)): apply func, a function of one argument, \
the current sample, to each sample in snd's channel chn, starting at 'start-samp' until func returns #t"

  /* no free here -- it's handled as ss->search_expr in snd-find.c */
  ASSERT_CHANNEL(S_find, snd_n, chn_n, 3);
  return(g_sp_scan(expr, sample, XEN_FALSE, snd_n, chn_n, S_find, false, edpos, 5, XEN_FALSE));
}

static XEN g_count_matches(XEN expr, XEN sample, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_count_matches "(" S_count_matches " func (start-samp 0) (snd #f) (chn #f) (edpos #f)): return how many \
samples satisfy func (a function of one argument, the current sample, returning #t upon match)"

  ASSERT_CHANNEL(S_count_matches, snd_n, chn_n, 3);
  return(g_sp_scan(expr, sample, XEN_FALSE, snd_n, chn_n, S_count_matches, true, edpos, 5, XEN_FALSE));
}

static XEN g_smooth_sound(XEN beg, XEN num, XEN snd_n, XEN chn_n)
{
  #define H_smooth_sound "(" S_smooth_sound " (start-samp 0) (samps len) (snd #f) (chn #f)): smooth \
data from start-samp for samps in snd's channel chn"
  chan_info *cp;
  off_t start, samps;
  char *buf;
  ASSERT_SAMPLE_TYPE(S_smooth_sound, beg, XEN_ARG_1);
  ASSERT_SAMPLE_TYPE(S_smooth_sound, num, XEN_ARG_2);
  ASSERT_CHANNEL(S_smooth_sound, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_smooth_sound);
  start = beg_to_sample(beg, S_smooth_sound);
  samps = dur_to_samples(num, start, cp, cp->edit_ctr, 2, S_smooth_sound);
  buf = mus_format("%s from " OFF_TD " for " OFF_TD, S_smooth_sound, start, samps);
  cos_smooth(cp, start, samps, false, buf); 
  FREE(buf);
  return(beg);
}

static XEN g_smooth_channel(XEN beg, XEN dur, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_smooth_channel "(" S_smooth_channel " (beg 0) (dur len) (snd #f) (chn #f) (edpos #f)): \
smooth data from beg for dur in snd's channel chn"
  chan_info *cp;
  off_t start, num;
  int pos;
  ASSERT_SAMPLE_TYPE(S_smooth_channel, beg, XEN_ARG_1);
  ASSERT_SAMPLE_TYPE(S_smooth_channel, dur, XEN_ARG_2);
  ASSERT_CHANNEL(S_smooth_channel, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_smooth_channel);
  pos = to_c_edit_position(cp, edpos, S_smooth_channel, 5);
  start = beg_to_sample(beg, S_smooth_channel);
  num = dur_to_samples(dur, start, cp, pos, 2, S_smooth_channel);
  if ((start < cp->samples[pos]) &&
      (num > 0))
    {
      char *buf;
      buf = mus_format("%s from " OFF_TD " for " OFF_TD, S_smooth_channel, start, num);
      smooth_channel(cp, start, num, pos, buf);
      FREE(buf);
    }
  return(beg);
}

static XEN g_smooth_selection(void)
{
  #define H_smooth_selection "(" S_smooth_selection "): smooth the data in the currently selected portion"
  chan_info *cp;
  if (!(selection_is_active())) 
    return(snd_no_active_selection_error(S_smooth_selection));
  cp = get_cp(XEN_FALSE, XEN_FALSE, S_smooth_selection);
  cos_smooth(cp, 0, 0, true, S_smooth_selection);
  return(XEN_TRUE);
}

static XEN g_reverse_sound(XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_reverse_sound "(" S_reverse_sound " (snd #f) (chn #f) (edpos #f)): reverse snd's channel chn"
  chan_info *cp;
  ASSERT_CHANNEL(S_reverse_sound, snd_n, chn_n, 1);
  cp = get_cp(snd_n, chn_n, S_reverse_sound);
  reverse_sound(cp, false, edpos, 3);
  return(XEN_FALSE);
}

static XEN g_reverse_selection(void)
{
  #define H_reverse_selection "(" S_reverse_selection "): reverse the data in the currently selected portion"
  chan_info *cp;
  if (!(selection_is_active())) 
    return(snd_no_active_selection_error(S_reverse_selection));
  cp = get_cp(XEN_FALSE, XEN_FALSE, S_reverse_selection);
  reverse_sound(cp, true, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0);
  return(XEN_FALSE);
}

static XEN g_reverse_channel(XEN s_beg, XEN s_dur, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_reverse_channel "(" S_reverse_channel " (beg 0) (dur len) (snd #f) (chn #f) (edpos #f)): reverse a portion of snd's channel chn"
  chan_info *cp;
  char *errmsg;
  off_t beg = 0, dur = 0, end;
  int pos;
  snd_fd *sf;
  XEN str;
  ASSERT_SAMPLE_TYPE(S_reverse_channel, s_beg, XEN_ARG_1);
  ASSERT_SAMPLE_TYPE(S_reverse_channel, s_dur, XEN_ARG_2);
  ASSERT_CHANNEL(S_reverse_channel, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_reverse_channel);
  beg = beg_to_sample(s_beg, S_reverse_channel);
  pos = to_c_edit_position(cp, edpos, S_reverse_channel, 5);
  dur = dur_to_samples(s_dur, beg, cp, pos, 2, S_reverse_channel);
  if ((beg > cp->samples[pos]) || (dur == 0)) return(XEN_FALSE);
  end = beg + dur;
  if (end > cp->samples[pos])
    end = cp->samples[pos];
  sf = init_sample_read_any(end - 1, cp, READ_BACKWARD, pos);
  errmsg = reverse_channel(cp, sf, beg, end - beg, edpos, S_reverse_channel, 5);
  free_snd_fd(sf);
  if (errmsg)
    {
      str = C_TO_XEN_STRING(errmsg);
      FREE(errmsg);
      mus_misc_error(S_reverse_channel, NULL, str);
    }
  return(s_beg);
}

static XEN g_insert_silence(XEN beg, XEN num, XEN snd, XEN chn)
{
  #define H_insert_silence "(" S_insert_silence " beg num (snd #f) (chn #f)): insert num zeros at beg in snd's chn"
  chan_info *cp; /* follows sync */
  off_t start = 0, len = 0;
  char *buf;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(beg), beg, XEN_ARG_1, S_insert_silence, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(num), num, XEN_ARG_2, S_insert_silence, "a number");
  ASSERT_CHANNEL(S_insert_silence, snd, chn, 3);
  cp = get_cp(snd, chn, S_insert_silence);
  start = XEN_TO_C_OFF_T(beg);
  if (start < 0) XEN_ERROR(NO_SUCH_SAMPLE,
			   XEN_LIST_2(C_TO_XEN_STRING(S_insert_silence),
				      beg));
  len = XEN_TO_C_OFF_T(num);
  if (len <= 0) return(XEN_FALSE);
  buf = mus_format("%s from " OFF_TD " for " OFF_TD, S_insert_silence, start, len);
  cursor_insert(cp, start, len, buf);
  FREE(buf);
  return(beg);
}

static XEN g_pad_channel(XEN beg, XEN num, XEN snd, XEN chn, XEN edpos)
{
  #define H_pad_channel "(" S_pad_channel " beg dur (snd #f) (chn #f) (edpos #f)): insert dur zeros at beg in snd's chn"
  chan_info *cp;
  off_t bg, len;
  int pos;
  char *buf;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(beg), beg, XEN_ARG_1, S_pad_channel, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(num), num, XEN_ARG_2, S_pad_channel, "a number");
  ASSERT_CHANNEL(S_pad_channel, snd, chn, 3);
  cp = get_cp(snd, chn, S_pad_channel);
  bg = beg_to_sample(beg, S_pad_channel);
  pos = to_c_edit_position(cp, edpos, S_pad_channel, 5);
  len = XEN_TO_C_OFF_T_OR_ELSE(num, cp->samples[pos] - bg);
  buf = mus_format("%s from " OFF_TD " for " OFF_TD, S_pad_channel, bg, len);
  if (extend_with_zeros(cp, bg,	len, buf, pos))
    update_graph(cp);
  FREE(buf);
  return(beg);
}

static XEN g_swap_channels(XEN snd0, XEN chn0, XEN snd1, XEN chn1, XEN beg, XEN dur, XEN edpos0, XEN edpos1)
{
  #define H_swap_channels "(" S_swap_channels " (snd0 #f) (chn0 #f) (snd1 #f) (chn1 #f) (beg 0) (dur len) (edpos0 #f) (edpos1 #f)): \
swap the indicated channels"
  chan_info *cp0 = NULL, *cp1 = NULL;
  off_t dur0 = 0, dur1 = 0, beg0 = 0, num;
  int pos0, pos1;
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
	sp = get_sp(snd1, NO_PLAYERS);
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
  if (cp0 == cp1) return(XEN_FALSE);
  if ((cp0) && (cp1))
    {
      if (XEN_NUMBER_P(beg)) 
	beg0 = XEN_TO_C_OFF_T(beg);
      dur0 = CURRENT_SAMPLES(cp0);
      dur1 = CURRENT_SAMPLES(cp1);
      if (XEN_NUMBER_P(dur)) 
	num = XEN_TO_C_OFF_T(dur);
      else
	{
	  if (dur0 > dur1) 
	    num = dur0; 
	  else num = dur1; /* was min here 13-Dec-02 */
	}
      pos0 = to_c_edit_position(cp0, edpos0, S_swap_channels, 7);
      pos1 = to_c_edit_position(cp1, edpos1, S_swap_channels, 8);
      if ((beg0 != 0) ||
	  ((num != dur0) && (num != dur1))) /* if just a section being swapped, use readers */
	swap_channels(cp0, cp1, beg0, num, pos0, pos1);
      else
	{
	  if ((pos0 == 0) &&
	      (pos1 == 0))
	    {
	      /* common special case -- just setup a new ed-list entry with the channels/sounds swapped */
	      if ((dur0 == 0) && (dur1 == 0)) return(XEN_FALSE);
	      if ((editable_p(cp0)) && (editable_p(cp1)))
		{
		  e0 = amp_env_copy(cp0, false, cp0->edit_ctr);
		  e1 = amp_env_copy(cp1, false, cp1->edit_ctr);
		  file_override_samples(dur1, cp1->sound->filename, cp0, cp1->chan, DONT_DELETE_ME, LOCK_MIXES, S_swap_channels);
		  file_override_samples(dur0, cp0->sound->filename, cp1, cp0->chan, DONT_DELETE_ME, LOCK_MIXES, S_swap_channels);
		  cp0->amp_envs[cp0->edit_ctr] = e1;
		  cp1->amp_envs[cp1->edit_ctr] = e0;
		  swap_marks(cp0, cp1);
		  update_graph(cp0);
		  update_graph(cp1);
		  cp0->edit_hook_checked = false;
		  cp1->edit_hook_checked = false;
		}
	    }
	  else
	    {
	      /* look for simple cases where copying the current edit tree entry is not too hard */
	      if ((num < FILE_BUFFER_SIZE) ||
		  (ptree_or_sound_fragments_in_use(cp0, pos0)) ||
		  (ptree_or_sound_fragments_in_use(cp1, pos1)))
		swap_channels(cp0, cp1, beg0, num, pos0, pos1);
	      else copy_then_swap_channels(cp0, cp1, pos0, pos1); /* snd-edits.c */
	    }
	}
    }
  return(XEN_FALSE);
}

static Float *load_Floats(XEN scalers, int *result_len, const char *caller)
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
  if (len == 0) 
    XEN_ERROR(NO_DATA,
	      XEN_LIST_3(C_TO_XEN_STRING(caller), 
			 C_TO_XEN_STRING("scalers empty?"), 
			 scalers));
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
	for (i = 0, lst = XEN_COPY_ARG(scalers); i < len; i++, lst = XEN_CDR(lst)) 
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
  #define H_scale_to "(" S_scale_to " (norms 1.0) (snd #f) (chn #f)): \
normalize snd to norms (following sync); norms can be a float or a vct/vector/list of floats"

  /* chn_n irrelevant if sync */
  chan_info *cp;
  int len[1];
  Float *scls;
  ASSERT_CHANNEL(S_scale_to, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_scale_to);
  scls = load_Floats(scalers, len, S_scale_to);
  scale_to(cp->sound, cp, scls, len[0], false); /* last arg for selection */
  FREE(scls);
  return(scalers);
}

static XEN g_scale_by(XEN scalers, XEN snd_n, XEN chn_n)
{
  #define H_scale_by "(" S_scale_by " scalers (snd #f) (chn #f)): \
scale snd by scalers (following sync); scalers can be a float or a vct/vector/list of floats"

  /* chn_n irrelevant if sync */
  chan_info *cp;
  int len[1];
  Float *scls;
  ASSERT_CHANNEL(S_scale_by, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_scale_by);
  scls = load_Floats(scalers, len, S_scale_by);
  scale_by(cp, scls, len[0], false);
  FREE(scls);
  return(scalers);
}

static XEN g_scale_selection_to(XEN scalers)
{
  #define H_scale_selection_to "(" S_scale_selection_to " norms): normalize selected portion to norms"
  int len[1];
  Float *scls;
  if (selection_is_active())
    {
      scls = load_Floats(scalers, len, S_scale_selection_to);
      scale_to(NULL, NULL, scls, len[0], true);
      FREE(scls);
      return(scalers);
    }
  return(snd_no_active_selection_error(S_scale_selection_to));
}

static XEN g_scale_selection_by(XEN scalers)
{
  #define H_scale_selection_by "(" S_scale_selection_by " scalers): scale selected portion by scalers"
  int len[1];
  Float *scls;
  if (selection_is_active())
    {
      scls = load_Floats(scalers, len, S_scale_selection_by);
      scale_by(NULL, scls, len[0], true);
      FREE(scls);
      return(scalers);
    }
  return(snd_no_active_selection_error(S_scale_selection_by));
}

static XEN g_clm_channel(XEN gen, XEN samp_n, XEN samps, XEN snd_n, XEN chn_n, XEN edpos, XEN overlap)
{
  #define H_clm_channel "(" S_clm_channel " gen (beg 0) (dur len) (snd #f) (chn #f) (edpos #f) (overlap 0)): \
apply gen to snd's channel chn starting at beg for dur samples. overlap is the 'ring' time, if any."

  chan_info *cp;
  off_t beg = 0, dur = 0;
  int pos;
  mus_any *egen;
  char *errmsg = NULL;
  XEN str;
  ASSERT_SAMPLE_TYPE(S_clm_channel, samp_n, XEN_ARG_2);
  ASSERT_SAMPLE_TYPE(S_clm_channel, samps, XEN_ARG_3);
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(overlap), overlap, XEN_ARG_7, S_clm_channel, "a number");
  ASSERT_CHANNEL(S_clm_channel, snd_n, chn_n, 4);
  cp = get_cp(snd_n, chn_n, S_clm_channel);
  pos = to_c_edit_position(cp, edpos, S_clm_channel, XEN_ARG_6);
  beg = beg_to_sample(samp_n, S_clm_channel);
  dur = dur_to_samples(samps, beg, cp, pos, XEN_ARG_3, S_clm_channel);
  if (dur == 0) return(XEN_FALSE);
  XEN_ASSERT_TYPE(mus_xen_p(gen), gen, XEN_ARG_1, S_clm_channel, "a clm generator");
  egen = XEN_TO_MUS_ANY(gen);
  errmsg = clm_channel(cp, egen, beg, dur, pos, S_clm_channel, XEN_TO_C_OFF_T_OR_ELSE(overlap, 0));
  if (errmsg)
    {
      str = C_TO_XEN_STRING(errmsg);
      FREE(errmsg);
      mus_misc_error(S_clm_channel, NULL, str);
    }
  return(gen);
}

static XEN g_env_1(XEN edata, off_t beg, off_t dur, XEN base, chan_info *cp, XEN edpos, const char *caller, bool selection)
{
  env *e;
  Float fbase = 1.0;
  mus_any *egen = NULL;
  if (XEN_LIST_P(edata))
    {
      e = get_env(edata, caller);
      if (e)
	{
	  fbase = XEN_TO_C_DOUBLE_OR_ELSE(base, 1.0);
	  if (fbase < 0.0)
	    XEN_OUT_OF_RANGE_ERROR(caller, 4, base, "base ~A < 0.0?");
	  apply_env(cp, e, beg, dur, selection, NOT_FROM_ENVED, caller, NULL, edpos, 7, fbase);
	  free_env(e);
	  return(edata);
	}
    }
  else
    {
      XEN_ASSERT_TYPE((mus_xen_p(edata)) && (mus_env_p(egen = XEN_TO_MUS_ANY(edata))), edata, XEN_ARG_1, caller, "an env generator or a list");
      apply_env(cp, NULL, beg, dur, selection, NOT_FROM_ENVED, caller, egen, edpos, 7, 1.0);
      return(edata);
    }
  return(XEN_FALSE);
}

static XEN g_env_selection(XEN edata, XEN base)
{
  #define H_env_selection "(" S_env_selection " env (env-base 1.0)): \
apply envelope to the selection using env-base to determine how breakpoints are connected"

  if (!(selection_is_active())) 
    return(snd_no_active_selection_error(S_env_selection));
  return(g_env_1(edata, 0, 0, base, 
		 get_cp(XEN_FALSE, XEN_FALSE, S_env_selection), 
		 C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 
		 S_env_selection, true));
}

static XEN g_env_sound(XEN edata, XEN samp_n, XEN samps, XEN base, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_env_sound "(" S_env_sound " env (start-samp 0) (samps len) (env-base 1.0) (snd #f) (chn #f) (edpos #f)): \
apply amplitude envelope (a list of breakpoints or a CLM env) to snd's channel chn starting at start-samp, going \
either to the end of the sound or for samps samples, with segments interpolating according to env-base"

  off_t beg = 0, dur = 0;
  int pos;
  chan_info *cp;
  ASSERT_SAMPLE_TYPE(S_env_sound, samp_n, XEN_ARG_2);
  ASSERT_SAMPLE_TYPE(S_env_sound, samps, XEN_ARG_3);
  ASSERT_CHANNEL(S_env_sound, snd_n, chn_n, 5);
  cp = get_cp(snd_n, chn_n, S_env_sound);
  pos = to_c_edit_position(cp, edpos, S_env_sound, 7);
  beg = beg_to_sample(samp_n, S_env_sound);
  dur = dur_to_samples(samps, beg, cp, pos, XEN_ARG_3, S_env_sound);
  return(g_env_1(edata, beg, dur, base, cp, edpos, S_env_sound, false));
}

static XEN g_env_channel(XEN gen, XEN samp_n, XEN samps, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_env_channel "(" S_env_channel " clm-env-gen-or-envelope (beg 0) (dur len) (snd #f) (chn #f) (edpos #f)): \
apply amplitude envelope to snd's channel chn starting at beg for dur samples."

  chan_info *cp;
  snd_info *sp;
  off_t beg = 0, dur;
  int old_sync = 0, pos;
  XEN val;
  ASSERT_SAMPLE_TYPE(S_env_channel, samp_n, XEN_ARG_2);
  ASSERT_SAMPLE_TYPE(S_env_channel, samps, XEN_ARG_3);
  ASSERT_CHANNEL(S_env_channel, snd_n, chn_n, 4);
  cp = get_cp(snd_n, chn_n, S_env_channel);
  beg = beg_to_sample(samp_n, S_env_channel);
  pos = to_c_edit_position(cp, edpos, S_env_channel, 6);
  dur = dur_to_samples(samps, beg, cp, pos, 3, S_env_channel);
  if (dur == 0) return(XEN_FALSE);
  if (beg > cp->samples[pos]) return(XEN_FALSE); /* not redundant */
  sp = cp->sound;
  old_sync = sp->sync;
  sp->sync = 0;
  val = g_env_1(gen, beg, dur, C_TO_XEN_DOUBLE(1.0), cp, edpos, S_env_channel, false);
  sp->sync = old_sync;
  return(val);
}

static XEN g_ramp_channel(XEN rmp0, XEN rmp1, XEN beg, XEN num, XEN snd, XEN chn, XEN edpos)
{
  #define H_ramp_channel "(" S_ramp_channel " rmp0 rmp1 (beg 0) (dur len) (snd #f) (chn #f) (edpos #f)): \
scale samples in the given sound/channel between beg and beg + num by a ramp going from rmp0 to rmp1."

  chan_info *cp;
  off_t samp, samps;
  int pos;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(rmp0), rmp0, XEN_ARG_1, S_ramp_channel, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(rmp1), rmp1, XEN_ARG_2, S_ramp_channel, "a number");
  ASSERT_SAMPLE_TYPE(S_ramp_channel, beg, XEN_ARG_3);
  ASSERT_SAMPLE_TYPE(S_ramp_channel, num, XEN_ARG_4);
  ASSERT_SOUND(S_ramp_channel, snd, 5);
  samp = beg_to_sample(beg, S_ramp_channel);
  cp = get_cp(snd, chn, S_ramp_channel);
  pos = to_c_edit_position(cp, edpos, S_ramp_channel, 7);
  samps = dur_to_samples(num, samp, cp, pos, 4, S_ramp_channel);
  if (ramp_or_ptree_fragments_in_use(cp, samp, samps, pos, 1.0))
    {
      snd_info *sp;
      int old_sync;
      XEN val;
      sp = cp->sound;
      old_sync = sp->sync;
      sp->sync = 0;
      val = g_env_1(XEN_LIST_4(C_TO_XEN_DOUBLE(0.0), 
			       rmp0,
			       C_TO_XEN_DOUBLE(1.0), 
			       rmp1),
		    samp, samps, C_TO_XEN_DOUBLE(1.0), cp, edpos, S_ramp_channel, false);
      sp->sync = old_sync;
      return(val);
    }
  if (ramp_channel(cp, XEN_TO_C_DOUBLE(rmp0), XEN_TO_C_DOUBLE(rmp1), samp, samps, pos, false))
    {
      if (cp->amp_envs[pos])
	{
	  Float data[4];
	  data[0] = 0.0;
	  data[1] = XEN_TO_C_DOUBLE(rmp0);
	  data[2] = 1.0;
	  data[3] = XEN_TO_C_DOUBLE(rmp1);
	  if ((samp == 0) && 
	      (samps >= cp->samples[pos]))
	    amp_env_env(cp, data, 2, pos, 1.0, 1.0, 0.0);
	  else 
	    {
	      mus_any *egen;
	      egen = mus_make_env(data, 2, 1.0, 0.0, 1.0, 0.0, 0, samps - 1, NULL);
	      amp_env_env_selection_by(cp, egen, samp, samps, pos);
	      mus_free(egen);
	    }
	}
      update_graph(cp);
    }
  return(rmp0);
}			  

static XEN g_xramp_channel(XEN rmp0, XEN rmp1, XEN base, XEN beg, XEN num, XEN snd, XEN chn, XEN edpos)
{
  #define H_xramp_channel "(" S_xramp_channel " rmp0 rmp1 base (beg 0) (dur len) (snd #f) (chn #f) (edpos #f)): \
scale samples in the given sound/channel between beg and beg + num by an exponential ramp going from rmp0 to rmp1 with curvature set by base."

  chan_info *cp;
  off_t samp, samps;
  int pos;
  Float ebase = 1.0;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(rmp0), rmp0, XEN_ARG_1, S_xramp_channel, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(rmp1), rmp1, XEN_ARG_2, S_xramp_channel, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(base), base, XEN_ARG_3, S_xramp_channel, "a number");
  ASSERT_SAMPLE_TYPE(S_xramp_channel, beg, XEN_ARG_4);
  ASSERT_SAMPLE_TYPE(S_xramp_channel, num, XEN_ARG_5);
  ASSERT_SOUND(S_xramp_channel, snd, 6);
  samp = beg_to_sample(beg, S_xramp_channel);
  cp = get_cp(snd, chn, S_xramp_channel);
  pos = to_c_edit_position(cp, edpos, S_xramp_channel, 8);
  samps = dur_to_samples(num, samp, cp, pos, 4, S_xramp_channel);
  ebase = XEN_TO_C_DOUBLE(base);
  if (ebase < 0.0) 
    XEN_OUT_OF_RANGE_ERROR(S_xramp_channel, 3, base, "base ~A < 0.0?");
  if (ramp_or_ptree_fragments_in_use(cp, samp, samps, pos, ebase))
    {
      snd_info *sp;
      int old_sync;
      XEN val;
      sp = cp->sound;
      old_sync = sp->sync;
      sp->sync = 0;
      val = g_env_1(XEN_LIST_4(C_TO_XEN_DOUBLE(0.0), 
			       rmp0,
			       C_TO_XEN_DOUBLE(1.0), 
			       rmp1),
		    samp, samps, base, cp, edpos, S_xramp_channel, false);
      sp->sync = old_sync;
      return(val);
    }
  else
    {
      Float *data;
      double *rates;
      off_t *passes;
      mus_any *e;
      Float r0, r1;
      if (ebase == 0.0)
	scale_channel(cp, XEN_TO_C_DOUBLE(rmp0), samp, samps, pos, false);
      else
	{
	  if (ebase == 1.0)
	    ramp_channel(cp, XEN_TO_C_DOUBLE(rmp0), XEN_TO_C_DOUBLE(rmp1), samp, samps, pos, false);
	  else
	    {
	      data = (Float *)CALLOC(4, sizeof(Float));
	      data[0] = 0.0;
	      data[1] = XEN_TO_C_DOUBLE(rmp0);
	      data[2] = 1.0;
	      data[3] = XEN_TO_C_DOUBLE(rmp1);
	      e = mus_make_env(data, 2, 1.0, 0.0, ebase, 0.0, samp, samp + samps - 1, NULL);
	      /* fprintf(stderr,"e: %s\n", mus_inspect(e)); */
	      r0 = mus_env_initial_power(e);
	      rates = mus_env_rates(e);
	      passes = mus_env_passes(e);
	      r1 = r0 + passes[0] * rates[0];
	      if (xramp_channel(cp, r0, r1, mus_env_scaler(e), mus_env_offset(e), samp, samps, pos, false, e, 0))
		{
		  if (cp->amp_envs[pos])
		    {
		      if ((samp == 0) && 
			  (samps >= cp->samples[pos]))
			amp_env_env(cp, data, 2, pos, ebase, mus_env_scaler(e), mus_env_offset(e));
		      else 
			{
			  mus_any *egen;
			  egen = mus_make_env(data, 2, 1.0, 0.0, ebase, 0.0, 0, samps - 1, NULL);
			  amp_env_env_selection_by(cp, egen, samp, samps, pos);
			  mus_free(egen);
			}
		    }
		}
	      FREE(data);
	      mus_free(e);
	      update_graph(cp);
	    }
	}
    }
  return(rmp0);
}			  

static XEN g_fft_1(XEN reals, XEN imag, XEN sign, bool use_fft)
{
  vct *v1 = NULL, *v2 = NULL;
  int ipow = 0, n = 0, n2 = 0, i, isign = 1;
  bool need_free = false, use_vectors = false;
  Float *rl = NULL, *im = NULL;
  XEN *rvdata = NULL, *ivdata = NULL;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(sign), sign, XEN_ARG_3, (use_fft) ? S_fft : S_vct_convolve, "an integer");
  if (!(((VCT_P(reals)) && (VCT_P(imag))) || 
	((XEN_VECTOR_P(reals)) && (XEN_VECTOR_P(imag)))))
    XEN_ERROR(BAD_TYPE,
	      XEN_LIST_4(C_TO_XEN_STRING(((use_fft) ? S_fft : S_vct_convolve)),
			 reals, 
			 imag,
			 C_TO_XEN_STRING("both vct or vector")));
  isign = XEN_TO_C_INT_OR_ELSE(sign, 1);
  if ((VCT_P(reals)) && (VCT_P(imag)))
    {
      v1 = (vct *)XEN_OBJECT_REF(reals);
      v2 = (vct *)XEN_OBJECT_REF(imag);
      n = v1->length;
      if (v2->length < n) n = v2->length;
    }
  else
    {
      n = XEN_VECTOR_LENGTH(reals);
      if (XEN_VECTOR_LENGTH(imag) < n)
	n = XEN_VECTOR_LENGTH(imag);
      use_vectors = true;
    }
  if (n == 0) return(XEN_ZERO);
  if (POWER_OF_2_P(n))
    n2 = n;
  else
    {
      ipow = (int)ceil(log(n + 1) / log(2.0)); /* ceil because we're assuming below that n2 >= n */
      n2 = snd_ipow2(ipow);
#if DEBUGGING
      if (n2 < n) {fprintf(stderr,"n2: %d, n: %d\n", n2, n); abort();}
#endif
    }
  if ((use_vectors) || (n != n2))
    {
      rl = (Float *)CALLOC(n2, sizeof(Float));
      im = (Float *)CALLOC(n2, sizeof(Float));
      need_free = true;
    }
  else
    {
      rl = v1->data;
      im = v2->data;
    }
  if (use_vectors)
    {
      rvdata = XEN_VECTOR_ELEMENTS(reals);
      ivdata = XEN_VECTOR_ELEMENTS(imag);
      for (i = 0; i < n; i++)
	{
	  rl[i] = XEN_TO_C_DOUBLE_OR_ELSE(rvdata[i], 0.0);
	  im[i] = XEN_TO_C_DOUBLE_OR_ELSE(ivdata[i], 0.0);
	}
    }
  else
    {
      if (need_free)
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
      if (use_vectors)
	{
	  rvdata = XEN_VECTOR_ELEMENTS(reals);
	  ivdata = XEN_VECTOR_ELEMENTS(imag);
	  for (i = 0; i < n; i++)
	    {
	      /* VECTOR_SET here and below */
	      rvdata[i] = C_TO_XEN_DOUBLE(rl[i]);
	      ivdata[i] = C_TO_XEN_DOUBLE(im[i]);
	    }
	}
      else
	{
	  if (need_free)
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
      if (use_vectors)
	{
	  rvdata = XEN_VECTOR_ELEMENTS(reals);
	  for (i = 0; i < n; i++)
	    rvdata[i] = C_TO_XEN_DOUBLE(rl[i]);
	}
      else
	{
	  if (need_free)
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
  #define H_fft "(" S_fft " reals imags (sign 1)): fft the data returning the result in reals. \
If sign is -1, perform inverse fft"

  return(g_fft_1(reals, imag, sign, true));
}

static XEN g_snd_spectrum(XEN data, XEN win, XEN len, XEN linear_or_dB, XEN beta, XEN in_place, XEN normalized)
{
  #define H_snd_spectrum "(" S_snd_spectrum " data (window " S_rectangular_window ") (len data-len) (linear-or-dB linear) (beta 0.0) (in-place #f) (normalized #t)): \
magnitude spectrum of data (a vct), in data if in-place, using fft-window win and fft length len"

  bool linear = false, in_data = false, normed = true;
  int i, j, n, n2;
  mus_fft_window_t wtype;
  Float maxa, todb, lowest, val, b = 0.0;
  Float *idat, *rdat, *window;
  vct *v;
  XEN_ASSERT_TYPE((VCT_P(data)), data, XEN_ARG_1, S_snd_spectrum, "a vct");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(win), win, XEN_ARG_2, S_snd_spectrum, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(len), len, XEN_ARG_3, S_snd_spectrum, "an integer");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(linear_or_dB), linear_or_dB, XEN_ARG_4, S_snd_spectrum, "a boolean");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(beta), beta, XEN_ARG_5, S_snd_spectrum, "a number");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(in_place), in_place, XEN_ARG_6, S_snd_spectrum, "a boolean");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(normalized), normalized, XEN_ARG_7, S_snd_spectrum, "a boolean");
  v = TO_VCT(data);
  rdat = v->data;
  n = XEN_TO_C_INT_OR_ELSE(len, v->length);
  if (n <= 0)
    XEN_OUT_OF_RANGE_ERROR(S_snd_spectrum, 3, len, "length ~A <= 0?");
  if (n > v->length) n = v->length;
  if (XEN_NOT_FALSE_P(linear_or_dB)) linear = true;
  if (XEN_TRUE_P(in_place)) in_data = true;
  if (XEN_FALSE_P(normalized)) normed = false;
  wtype = (mus_fft_window_t)XEN_TO_C_INT_OR_ELSE(win, MUS_RECTANGULAR_WINDOW);
  if (!(MUS_FFT_WINDOW_OK(wtype)))
    XEN_OUT_OF_RANGE_ERROR(S_snd_spectrum, 2, win, "~A: unknown fft window");
  if (XEN_NUMBER_P(beta)) b = XEN_TO_C_DOUBLE(beta);
  if (b < 0.0) b = 0.0; else if (b > 1.0) b = 1.0;
  idat = (Float *)CALLOC(n, sizeof(Float));
  window = (Float *)CALLOC(n, sizeof(Float));
  mus_make_fft_window_with_window(wtype, n, b * fft_beta_max(wtype), window);
  for (i = 0; i < n; i++) rdat[i] *= window[i];
  FREE(window);
  n2 = n / 2;
#if HAVE_FFTW || HAVE_FFTW3
  mus_fftw(rdat, n, 1);
  rdat[0] *= rdat[0];
  rdat[n2] *= rdat[n2];
  for (i = 1, j = n - 1; i < n2; i++, j--)
    {
      rdat[i] = rdat[i] * rdat[i] + rdat[j] * rdat[j];
      rdat[j] = rdat[i];
    }
#else
  mus_fft(rdat, idat, n, 1);
  rdat[0] *= rdat[0];
  rdat[n2] *= rdat[n2];
  for (i = 1, j = n - 1; i < n2; i++, j--)
    {
      rdat[i] = rdat[i] * rdat[i] + idat[i] * idat[i];
      rdat[j] = rdat[i];
    }
#endif
  if (in_data) 
    {
      FREE(idat);
      idat = rdat;
    }
  lowest = 0.000001;
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
      if (normed)
	maxa = 1.0 / maxa;
      else maxa = 1.0;
      if (!linear) /* dB */
	{
	  todb = 20.0 / log(10.0);
	  for (i = 0; i < n; i++) 
	    if (idat[i] > 0.0)
	      idat[i] = todb * log(idat[i] * maxa);
	    else idat[i] = -90.0;
	}
      else 
	{
	  if (normed)
	    for (i = 0; i < n; i++) 
	      idat[i] *= maxa;
	}
    }
  if (in_data)
    return(data);
  return(xen_return_first(make_vct(n, idat), data));
}

static XEN g_convolve_with_1(XEN file, XEN new_amp, chan_info *cp, XEN edpos, const char *caller)
{
  /* cp NULL -> selection (see above) */
  Float amp;
  XEN errstr;
  char *fname = NULL, *error = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, caller, "a string");
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
	  mus_misc_error(caller, NULL, errstr);
	}
    }
  else 
    {
      if (fname) FREE(fname);
      return(snd_no_such_file_error(caller, file));
    }
  if (fname) FREE(fname);
  return(xen_return_first(file, new_amp));
}

static XEN g_convolve_with(XEN file, XEN new_amp, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_convolve_with "(" S_convolve_with " file (amp 1.0) (snd #f) (chn #f) (edpos #f)): \
convolve file with snd's channel chn (or the currently sync'd channels); amp is the resultant peak amp"

  chan_info *cp;
  ASSERT_CHANNEL(S_convolve_with, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_convolve_with);
  return(g_convolve_with_1(file, new_amp, cp, edpos, S_convolve_with));
}

static XEN g_convolve_selection_with(XEN file, XEN new_amp)
{
  #define H_convolve_selection_with "(" S_convolve_selection_with " file (amp 1.0)): \
convolve the selection with file; amp is the resultant peak amp"

  if (!(selection_is_active())) 
    return(snd_no_active_selection_error(S_convolve_selection_with));
  return(g_convolve_with_1(file, new_amp, NULL, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), S_convolve_selection_with));
}

static XEN g_vct_convolve(XEN reals, XEN imag)
{
  #define H_vct_convolve "(" S_vct_convolve " rl1 rl2): convolve vcts rl1 and rl2, result in rl1 (which needs to be big enough)"
  return(g_fft_1(reals, imag, C_TO_SMALL_XEN_INT(1), false));
}

static Float check_src_envelope(int pts, Float *data, const char *caller)
{
  /* can't go through zero here, and if negative need to return 1.0 */
  int i;
  Float res = 0.0;
  for (i = 0; i < (2 * pts); i += 2)
    if (data[i + 1] == 0.0)
      XEN_OUT_OF_RANGE_ERROR(caller, 1, mus_array_to_list(data, 0, pts * 2), "~A: envelope hits 0.0");
    else
      {
	if (data[i + 1] < 0.0)
	  {
	    if (res <= 0.0)
	      res = -1.0;
	    else XEN_OUT_OF_RANGE_ERROR(caller, 1, mus_array_to_list(data, 0, pts * 2), "~A: envelope passes through 0.0");
	  }
	else
	  {
	    if (res >= 0)
	      res = 1.0;
	    else XEN_OUT_OF_RANGE_ERROR(caller, 1, mus_array_to_list(data, 0, pts * 2), "~A: envelope passes through 0.0");
	  }
      }
  return(res);
}


static XEN g_src_channel(XEN ratio_or_env_gen, XEN beg_n, XEN dur_n, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_src_channel "(" S_src_channel " ratio-or-env-gen (beg 0) (dur len) (snd #f) (chn #f) (edpos #f)): \
sampling-rate convert snd's channel chn by ratio, or following an envelope generator."

  chan_info *cp;
  char *errmsg;
  snd_fd *sf;
  off_t beg, dur;
  int pos;
  Float ratio = 0.0;
  mus_any *egen = NULL;
  XEN err;
  XEN_ASSERT_TYPE((XEN_NUMBER_P(ratio_or_env_gen)) || 
		  ((mus_xen_p(ratio_or_env_gen)) && 
		   (mus_env_p(egen = XEN_TO_MUS_ANY(ratio_or_env_gen)))),
		  ratio_or_env_gen, XEN_ARG_1, S_src_channel, "a number or a CLM env generator");
  ASSERT_SAMPLE_TYPE(S_src_channel, beg_n, XEN_ARG_2);
  ASSERT_SAMPLE_TYPE(S_src_channel, dur_n, XEN_ARG_3);
  ASSERT_CHANNEL(S_src_channel, snd_n, chn_n, 4);
  cp = get_cp(snd_n, chn_n, S_src_channel);
  beg = beg_to_sample(beg_n, S_src_channel);
  pos = to_c_edit_position(cp, edpos, S_src_channel, 6);
  dur = dur_to_samples(dur_n, beg, cp, pos, 3, S_src_channel);
  if (dur == 0) return(XEN_FALSE);
  if (beg > cp->samples[pos]) return(XEN_FALSE);
  if (XEN_NUMBER_P(ratio_or_env_gen))
    {
      ratio = XEN_TO_C_DOUBLE(ratio_or_env_gen);
      if ((ratio == 0.0) || (ratio == 1.0)) return(XEN_FALSE);
    }
  else check_src_envelope(mus_env_breakpoints(egen), mus_data(egen), S_src_channel);
  sf = init_sample_read_any(beg, cp, READ_FORWARD, pos);
  errmsg = src_channel_with_error(cp, sf, beg, dur, ratio, egen, NOT_FROM_ENVED, S_src_channel, false, 1, 1);
  sf = free_snd_fd(sf);
  if (errmsg)
    {
      err = C_TO_XEN_STRING(errmsg);
      FREE(errmsg);
      XEN_ERROR(MUS_MISC_ERROR,
		XEN_LIST_2(C_TO_XEN_STRING(S_src_channel),
			   err));
    }
  return(ratio_or_env_gen);
}

static XEN g_src_1(XEN ratio_or_env, XEN base, XEN snd_n, XEN chn_n, XEN edpos, const char *caller, bool selection)
{
  chan_info *cp;
  env *e = NULL;
  mus_any *egen;
  Float e_ratio = 1.0;
  ASSERT_CHANNEL(caller, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, caller);
  if (XEN_NUMBER_P(ratio_or_env))
    src_env_or_num(cp, NULL, XEN_TO_C_DOUBLE(ratio_or_env), 
		   true, NOT_FROM_ENVED, caller,
		   selection, NULL, edpos, 5, 1.0);
  else 
    {
      if (XEN_LIST_P(ratio_or_env))
	{
	  e = get_env(ratio_or_env, caller);
	  e_ratio = check_src_envelope(e->pts, e->data, caller);
	  src_env_or_num(cp,
			 e, e_ratio,
			 false, NOT_FROM_ENVED, caller, 
			 selection, NULL, edpos, 5,
			 XEN_TO_C_DOUBLE_OR_ELSE(base, 1.0));
	  if (e) free_env(e);
	}
      else
	{
	  XEN_ASSERT_TYPE((mus_xen_p(ratio_or_env)) && (mus_env_p(egen = XEN_TO_MUS_ANY(ratio_or_env))), 
			  ratio_or_env, XEN_ARG_1, caller, "a number, list, or env generator");
	  check_src_envelope(mus_env_breakpoints(egen), mus_data(egen), caller);
	  src_env_or_num(cp, NULL, 
			 (mus_phase(egen) >= 0.0) ? 1.0 : -1.0,
			 false, NOT_FROM_ENVED, caller, 
			 selection, egen, edpos, 5, 1.0);
	}
    }
  return(xen_return_first(ratio_or_env, base));
}

static XEN g_src_sound(XEN ratio_or_env, XEN base, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_src_sound "(" S_src_sound " ratio-or-env (base 1.0) (snd #f) (chn #f) (edpos #f)): \
sampling-rate convert snd's channel chn by ratio, or following an envelope. A negative ratio reverses the sound"

  return(g_src_1(ratio_or_env, base, snd_n, chn_n, edpos, S_src_sound, false));
}

static XEN g_src_selection(XEN ratio_or_env, XEN base)
{
  #define H_src_selection "(" S_src_selection " ratio-or-env (base 1.0)): \
sampling-rate convert the currently selected data by ratio (which can be an envelope)"

  if (!(selection_is_active())) 
    return(snd_no_active_selection_error(S_src_selection));
  return(g_src_1(ratio_or_env, base, XEN_FALSE, XEN_FALSE, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), S_src_selection, true));
}

static XEN g_filter_1(XEN e, XEN order, XEN snd_n, XEN chn_n, XEN edpos, const char *caller, bool selection)
{
  chan_info *cp;
  vct *v;
  int len;
  XEN errstr;
  env *ne = NULL;
  char *error;
  ASSERT_CHANNEL(caller, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, caller);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(order), order, XEN_ARG_2, caller, "an integer");
  if (mus_xen_p(e))
    {
      error = apply_filter_or_error(cp, 0, NULL, NOT_FROM_ENVED, caller, selection, NULL, XEN_TO_MUS_ANY(e), edpos, 5);
      if (error)
	{
	  errstr = C_TO_XEN_STRING(error);
	  FREE(error);
	  mus_misc_error(caller, NULL, errstr);
	}
    }
  else
    {
      len = XEN_TO_C_INT_OR_ELSE(order, 0);
      if (len <= 0) 
	XEN_OUT_OF_RANGE_ERROR(caller, 2, order, "order ~A <= 0?");
      if (VCT_P(e)) /* the filter coefficients direct */
	{
	  v = TO_VCT(e);
	  if (len > v->length) 
	    XEN_OUT_OF_RANGE_ERROR(caller, 2, order, "order ~A > length coeffs?");
	  apply_filter(cp, len, NULL, NOT_FROM_ENVED, caller, selection, v->data, NULL, edpos, 5);
	}
      else 
	{
	  XEN_ASSERT_TYPE((XEN_VECTOR_P(e) || (XEN_LIST_P(e))), e, XEN_ARG_1, caller, "a list, vector, vct, or env generator");
	  apply_filter(cp, len,
		       ne = get_env(e, caller),
		       NOT_FROM_ENVED, caller, selection, NULL, NULL, edpos, 5);
	  if (ne) free_env(ne); 
	}
    }
  return(xen_return_first(XEN_TRUE, e));
}

static XEN g_filter_sound(XEN e, XEN order, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_filter_sound "(" S_filter_sound " filter order (snd #f) (chn #f) (edpos #f)): \
applies FIR filter to snd's channel chn. 'filter' is either the frequency response envelope, a CLM filter, or a vct with the actual coefficients"

  return(g_filter_1(e, order, snd_n, chn_n, edpos, S_filter_sound, false));
}

static XEN g_filter_selection(XEN e, XEN order)
{
  #define H_filter_selection "(" S_filter_selection " filter order): apply filter to selection"

  if (!(selection_is_active())) 
    return(snd_no_active_selection_error(S_filter_selection));
  return(g_filter_1(e, order, XEN_FALSE, XEN_FALSE, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), S_filter_selection, true));
}

static XEN g_sinc_width(void) {return(C_TO_XEN_INT(sinc_width(ss)));}
static XEN g_set_sinc_width(XEN val) 
{
  #define H_sinc_width "(" S_sinc_width "): sampling rate conversion sinc width (10). \
The higher this number, the better the src low-pass filter, but the slower \
src runs.  If you use too low a setting, you can sometimes hear high \
frequency whistles leaking through."

  int len;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_sinc_width, "an integer"); 
  len = XEN_TO_C_INT(val);
  if (len >= 0)
    set_sinc_width(len);
  return(C_TO_XEN_INT(sinc_width(ss)));
}


#ifdef XEN_ARGIFY_1
XEN_ARGIFY_6(g_scan_chan_w, g_scan_chan)
XEN_ARGIFY_7(g_map_chan_w, g_map_chan)
XEN_ARGIFY_6(g_scan_channel_w, g_scan_channel)
XEN_ARGIFY_7(g_map_channel_w, g_map_channel)
XEN_ARGIFY_5(g_find_w, g_find)
XEN_ARGIFY_5(g_count_matches_w, g_count_matches)
XEN_ARGIFY_4(g_smooth_sound_w, g_smooth_sound)
XEN_ARGIFY_5(g_smooth_channel_w, g_smooth_channel)
XEN_NARGIFY_0(g_smooth_selection_w, g_smooth_selection)
XEN_ARGIFY_3(g_reverse_sound_w, g_reverse_sound)
XEN_ARGIFY_5(g_reverse_channel_w, g_reverse_channel)
XEN_NARGIFY_0(g_reverse_selection_w, g_reverse_selection)
XEN_ARGIFY_8(g_swap_channels_w, g_swap_channels)
XEN_ARGIFY_4(g_insert_silence_w, g_insert_silence)
XEN_ARGIFY_1(g_scale_selection_to_w, g_scale_selection_to)
XEN_NARGIFY_1(g_scale_selection_by_w, g_scale_selection_by)
XEN_ARGIFY_3(g_scale_to_w, g_scale_to)
XEN_ARGIFY_3(g_scale_by_w, g_scale_by)
XEN_ARGIFY_2(g_env_selection_w, g_env_selection)
XEN_ARGIFY_7(g_env_sound_w, g_env_sound)
XEN_ARGIFY_6(g_env_channel_w, g_env_channel)
XEN_ARGIFY_7(g_ramp_channel_w, g_ramp_channel)
XEN_ARGIFY_8(g_xramp_channel_w, g_xramp_channel)
XEN_ARGIFY_3(g_fft_w, g_fft)
XEN_ARGIFY_7(g_snd_spectrum_w, g_snd_spectrum)
XEN_ARGIFY_2(g_vct_convolve_w, g_vct_convolve)
XEN_ARGIFY_5(g_convolve_with_w, g_convolve_with)
XEN_ARGIFY_2(g_convolve_selection_with_w, g_convolve_selection_with)
XEN_ARGIFY_5(g_src_sound_w, g_src_sound)
XEN_ARGIFY_2(g_src_selection_w, g_src_selection)
XEN_ARGIFY_6(g_src_channel_w, g_src_channel)
XEN_ARGIFY_5(g_pad_channel_w, g_pad_channel)
XEN_ARGIFY_5(g_filter_sound_w, g_filter_sound)
XEN_ARGIFY_2(g_filter_selection_w, g_filter_selection)
XEN_ARGIFY_7(g_clm_channel_w, g_clm_channel)
XEN_NARGIFY_0(g_sinc_width_w, g_sinc_width)
XEN_NARGIFY_1(g_set_sinc_width_w, g_set_sinc_width)
XEN_ARGIFY_9(g_ptree_channel_w, g_ptree_channel)
#else
#define g_scan_chan_w g_scan_chan
#define g_map_chan_w g_map_chan
#define g_scan_channel_w g_scan_channel
#define g_map_channel_w g_map_channel
#define g_find_w g_find
#define g_count_matches_w g_count_matches
#define g_smooth_sound_w g_smooth_sound
#define g_smooth_channel_w g_smooth_channel
#define g_smooth_selection_w g_smooth_selection
#define g_reverse_sound_w g_reverse_sound
#define g_reverse_channel_w g_reverse_channel
#define g_reverse_selection_w g_reverse_selection
#define g_swap_channels_w g_swap_channels
#define g_insert_silence_w g_insert_silence
#define g_scale_selection_to_w g_scale_selection_to
#define g_scale_selection_by_w g_scale_selection_by
#define g_scale_to_w g_scale_to
#define g_scale_by_w g_scale_by
#define g_env_selection_w g_env_selection
#define g_env_sound_w g_env_sound
#define g_env_channel_w g_env_channel
#define g_ramp_channel_w g_ramp_channel
#define g_xramp_channel_w g_xramp_channel
#define g_fft_w g_fft
#define g_snd_spectrum_w g_snd_spectrum
#define g_vct_convolve_w g_vct_convolve
#define g_convolve_with_w g_convolve_with
#define g_convolve_selection_with_w g_convolve_selection_with
#define g_src_sound_w g_src_sound
#define g_src_selection_w g_src_selection
#define g_src_channel_w g_src_channel
#define g_pad_channel_w g_pad_channel
#define g_filter_sound_w g_filter_sound
#define g_filter_selection_w g_filter_selection
#define g_clm_channel_w g_clm_channel
#define g_sinc_width_w g_sinc_width
#define g_set_sinc_width_w g_set_sinc_width
#define g_ptree_channel_w g_ptree_channel
#endif

void g_init_sig(void)
{
#if WITH_RUN
  XEN_EVAL_C_STRING("(use-modules (ice-9 optargs))");
  XEN_DEFINE_PROCEDURE(S_scan_channel "-1",       g_scan_channel_w,  1, 5, 0, H_scan_channel);
  XEN_DEFINE_PROCEDURE(S_scan_chan "-1",          g_scan_chan_w,     1, 5, 0, H_scan_chan);
  XEN_DEFINE_PROCEDURE(S_find "-1",               g_find_w,          1, 4, 0, H_find);
  XEN_DEFINE_PROCEDURE(S_count_matches "-1",      g_count_matches_w, 1, 4, 0, H_count_matches);
  XEN_DEFINE_PROCEDURE(S_map_chan "-1",           g_map_chan_w,      1, 6, 0, H_map_chan);
  XEN_DEFINE_PROCEDURE(S_map_channel "-1",        g_map_channel_w,   1, 6, 0, H_map_channel);
  XEN_DEFINE_PROCEDURE(S_ptree_channel "-1",      g_ptree_channel_w, 1, 8, 0, H_ptree_channel);

  XEN_EVAL_C_STRING("(defmacro* scan-channel (form #:rest args) `(apply scan-channel-1 (list (list ',form ,form) ,@args)))");
  XEN_SET_DOCUMENTATION(S_scan_channel, H_scan_channel);
  XEN_EVAL_C_STRING("(defmacro* scan-chan (form #:rest args) `(apply scan-chan-1 (list (list ',form ,form) ,@args)))");
  XEN_SET_DOCUMENTATION(S_scan_chan, H_scan_chan);
  XEN_EVAL_C_STRING("(defmacro* find (form #:rest args) `(apply find-1 (list (list ',form ,form) ,@args)))");
  XEN_SET_DOCUMENTATION(S_find, H_find);
  XEN_EVAL_C_STRING("(defmacro* count-matches (form #:rest args) `(apply count-matches-1 (list (list ',form ,form) ,@args)))");
  XEN_SET_DOCUMENTATION(S_count_matches, H_count_matches);
  XEN_EVAL_C_STRING("(defmacro* map-channel (form #:rest args) `(apply map-channel-1 (list (list ',form ,form) ,@args)))");
  XEN_SET_DOCUMENTATION(S_map_channel, H_map_channel);
  XEN_EVAL_C_STRING("(defmacro* map-chan (form #:rest args) `(apply map-chan-1 (list (list ',form ,form) ,@args)))");
  XEN_SET_DOCUMENTATION(S_map_chan, H_map_chan);
  XEN_EVAL_C_STRING("(defmacro* ptree-channel (form #:rest args) `(apply ptree-channel-1 (list (list ',form ,form) ,@args)))");
  XEN_SET_DOCUMENTATION(S_ptree_channel, H_ptree_channel);
#else
  XEN_DEFINE_PROCEDURE(S_scan_channel,            g_scan_channel_w,            1, 5, 0, H_scan_channel);
  XEN_DEFINE_PROCEDURE(S_scan_chan,               g_scan_chan_w,               1, 5, 0, H_scan_chan);
  XEN_DEFINE_PROCEDURE(S_find,                    g_find_w,                    1, 4, 0, H_find);
  XEN_DEFINE_PROCEDURE(S_count_matches,           g_count_matches_w,           1, 4, 0, H_count_matches);
  XEN_DEFINE_PROCEDURE(S_map_chan,                g_map_chan_w,                1, 6, 0, H_map_chan);
  XEN_DEFINE_PROCEDURE(S_map_channel,             g_map_channel_w,             1, 6, 0, H_map_channel);
  XEN_DEFINE_PROCEDURE(S_ptree_channel,           g_ptree_channel_w,           1, 8, 0, H_ptree_channel);
#endif

  XEN_DEFINE_PROCEDURE(S_smooth_sound,            g_smooth_sound_w,            0, 4, 0, H_smooth_sound);
  XEN_DEFINE_PROCEDURE(S_smooth_selection,        g_smooth_selection_w,        0, 0, 0, H_smooth_selection);
  XEN_DEFINE_PROCEDURE(S_reverse_sound,           g_reverse_sound_w,           0, 3, 0, H_reverse_sound);
  XEN_DEFINE_PROCEDURE(S_reverse_selection,       g_reverse_selection_w,       0, 0, 0, H_reverse_selection);
  XEN_DEFINE_PROCEDURE(S_swap_channels,           g_swap_channels_w,           0, 8, 0, H_swap_channels);
  XEN_DEFINE_PROCEDURE(S_insert_silence,          g_insert_silence_w,          2, 2, 0, H_insert_silence);

  XEN_DEFINE_PROCEDURE(S_scale_selection_to,      g_scale_selection_to_w,      0, 1, 0, H_scale_selection_to);
  XEN_DEFINE_PROCEDURE(S_scale_selection_by,      g_scale_selection_by_w,      1, 0, 0, H_scale_selection_by);
  XEN_DEFINE_PROCEDURE(S_scale_to,                g_scale_to_w,                0, 3, 0, H_scale_to);
  XEN_DEFINE_PROCEDURE(S_scale_by,                g_scale_by_w,                1, 2, 0, H_scale_by);
  XEN_DEFINE_PROCEDURE(S_env_selection,           g_env_selection_w,           1, 1, 0, H_env_selection);
  XEN_DEFINE_PROCEDURE(S_env_sound,               g_env_sound_w,               1, 6, 0, H_env_sound);
  XEN_DEFINE_PROCEDURE(S_fft,                     g_fft_w,                     2, 1, 0, H_fft);
  XEN_DEFINE_PROCEDURE(S_snd_spectrum,            g_snd_spectrum_w,            1, 6, 0, H_snd_spectrum);
  XEN_DEFINE_PROCEDURE(S_vct_convolve,            g_vct_convolve_w,            1, 1, 0, H_vct_convolve);
  XEN_DEFINE_PROCEDURE(S_convolve_with,           g_convolve_with_w,           1, 4, 0, H_convolve_with);
  XEN_DEFINE_PROCEDURE(S_convolve_selection_with, g_convolve_selection_with_w, 1, 1, 0, H_convolve_selection_with);
  XEN_DEFINE_PROCEDURE(S_src_sound,               g_src_sound_w,               1, 4, 0, H_src_sound);
  XEN_DEFINE_PROCEDURE(S_src_selection,           g_src_selection_w,           1, 1, 0, H_src_selection);
  XEN_DEFINE_PROCEDURE(S_filter_sound,            g_filter_sound_w,            1, 4, 0, H_filter_sound);
  XEN_DEFINE_PROCEDURE(S_filter_selection,        g_filter_selection_w,        1, 1, 0, H_filter_selection);

  XEN_DEFINE_PROCEDURE(S_reverse_channel,         g_reverse_channel_w,         0, 5, 0, H_reverse_channel);
  XEN_DEFINE_PROCEDURE(S_clm_channel,             g_clm_channel_w,             1, 6, 0, H_clm_channel);
  XEN_DEFINE_PROCEDURE(S_env_channel,             g_env_channel_w,             1, 5, 0, H_env_channel);
  XEN_DEFINE_PROCEDURE(S_ramp_channel,            g_ramp_channel_w,            2, 5, 0, H_ramp_channel);
  XEN_DEFINE_PROCEDURE(S_xramp_channel,           g_xramp_channel_w,           2, 6, 0, H_xramp_channel);
  XEN_DEFINE_PROCEDURE(S_smooth_channel,          g_smooth_channel_w,          0, 5, 0, H_smooth_channel);
  XEN_DEFINE_PROCEDURE(S_src_channel,             g_src_channel_w,             1, 5, 0, H_src_channel);
  XEN_DEFINE_PROCEDURE(S_pad_channel,             g_pad_channel_w,             2, 3, 0, H_pad_channel);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_sinc_width, g_sinc_width_w, H_sinc_width,
				   S_setB S_sinc_width, g_set_sinc_width_w,  0, 0, 1, 0);
}


