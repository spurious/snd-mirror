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
	free(sc->sfs);
      free(sc);
    }
}


int to_c_edit_position(chan_info *cp, XEN edpos, const char *caller, int arg_pos)
{
  int pos;
  /* need to allow #f here for optargs */
  /* also remember that there might be no extension language */

#if (!HAVE_EXTENSION_LANGUAGE)
  return(cp->edit_ctr);
#endif

  XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(edpos) || XEN_INTEGER_P(edpos) || XEN_PROCEDURE_P(edpos) || XEN_FALSE_P(edpos), 
		  edpos, arg_pos, caller, "an integer, " PROC_FALSE ", or a procedure");

  if (XEN_PROCEDURE_P(edpos))
    {
      char *errmsg = NULL;
      errmsg = procedure_ok(edpos, 2, caller, "edit position", arg_pos);
      if (errmsg)
	{
	  XEN errstr;
	  errstr = C_TO_XEN_STRING(errmsg);
	  free(errmsg);
	  snd_bad_arity_error(caller, errstr, edpos);
	  return(cp->edit_ctr);
	}

      pos = XEN_TO_C_INT_OR_ELSE(XEN_CALL_2(edpos, 
					    C_TO_XEN_INT(cp->sound->index), 
					    C_TO_XEN_INT(cp->chan),
					    caller),
				 AT_CURRENT_EDIT_POSITION);
      if (cp->active < CHANNEL_HAS_EDIT_LIST) /* edpos proc clobbered channel somehow... */
	XEN_ERROR(NO_SUCH_CHANNEL,
		  XEN_LIST_3(C_TO_XEN_STRING(caller),
			     C_TO_XEN_STRING("edpos arg (a function) clobbered the current sound!"),
			     edpos));
    }
  else pos = XEN_TO_C_INT_OR_ELSE(edpos, AT_CURRENT_EDIT_POSITION);
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
  return(cp->edits[to_c_edit_position(cp, edpos, caller, arg_pos)]->samples);
}


off_t beg_to_sample(XEN beg, const char *caller)
{
  off_t start;
  start = XEN_TO_C_OFF_T_OR_ELSE(beg, 0);
  if (start < 0) 
    XEN_ERROR(NO_SUCH_SAMPLE,
	      XEN_LIST_2(C_TO_XEN_STRING(caller),
			 beg));
  return(start);
}


off_t dur_to_samples(XEN dur, off_t beg, chan_info *cp, int edpos, int argn, const char *caller)
{
  off_t samps;
  samps = XEN_TO_C_OFF_T_OR_ELSE(dur, cp->edits[edpos]->samples - beg);
  if (samps < 0)
    XEN_WRONG_TYPE_ARG_ERROR(caller, argn, dur, "a positive integer");
  return(samps);
}


static off_t end_to_sample(XEN end, chan_info *cp, int edpos, const char *caller)
{
  off_t last;
  last = XEN_TO_C_OFF_T_OR_ELSE(end, cp->edits[edpos]->samples - 1);
  if (last < 0) 
    XEN_ERROR(NO_SUCH_SAMPLE,
	      XEN_LIST_2(C_TO_XEN_STRING(caller),
			 end));
  return(last);
}


static sync_state *get_sync_state_1(snd_info *sp, chan_info *cp, off_t beg, bool over_selection, 
				    read_direction_t forwards, off_t prebeg, XEN edpos, const char *caller, int arg_pos)
{
  /* can return NULL if over_selection and no current selection */
  sync_info *si = NULL;
  snd_fd **sfs = NULL;
  chan_info *ncp;
  int i, pos;
  off_t dur = 0, pbeg;
  sync_state *sc;

  if ((!over_selection) && (sp == NULL)) return(NULL);

  if ((!over_selection) && (sp->sync != 0))
    {
      si = snd_sync(sp->sync);
      sfs = (snd_fd **)malloc(si->chans * sizeof(snd_fd *));
      for (i = 0; i < si->chans; i++) 
	{
	  ncp = si->cps[i];
	  si->begs[i] = beg;
	  pos = to_c_edit_position(ncp, edpos, caller, arg_pos);
	  if (forwards == READ_FORWARD)
	    sfs[i] = init_sample_read_any(beg, ncp, READ_FORWARD, pos);
	  else sfs[i] = init_sample_read_any(ncp->edits[pos]->samples - 1, ncp, READ_BACKWARD, pos);
	}
    }
  else
    {
      if (over_selection)
	{
	  if (selection_is_active())
	    {
	      si = selection_sync();
	      dur = selection_len();
	      sfs = (snd_fd **)malloc(si->chans * sizeof(snd_fd *));
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
	      snd_warning_without_format(_("no selection"));
	      return(NULL);
	    }
	}
    }
  if (si == NULL) 
    {
      snd_fd *sf = NULL;
      pos = to_c_edit_position(cp, edpos, caller, arg_pos);
      if (forwards == READ_FORWARD)
	sf = init_sample_read_any(beg, cp, READ_FORWARD, pos);
      else sf = init_sample_read_any(cp->edits[pos]->samples - 1, cp, READ_BACKWARD, pos);
      if (sf)
	{
	  si = make_simple_sync(cp, beg);
	  sfs = (snd_fd **)malloc(sizeof(snd_fd *));
	  sfs[0] = sf;
	}
      else return(NULL);
    }
  sc = (sync_state *)calloc(1, sizeof(sync_state));
  sc->dur = dur;
  sc->sfs = sfs;
  sc->si = si;
  return(sc);
}


static sync_state *get_sync_state(snd_info *sp, chan_info *cp, off_t beg, bool over_selection, 
				  read_direction_t forwards, XEN edpos, const char *caller, int arg_pos)
{
  return(get_sync_state_1(sp, cp, beg, over_selection, forwards, 0, edpos, caller, arg_pos));
}


static sync_state *get_sync_state_without_snd_fds(snd_info *sp, chan_info *cp, off_t beg, bool over_selection)
{
  sync_info *si = NULL;
  off_t dur = 0;
  sync_state *sc;
  if ((sp->sync != 0) && (!over_selection))
    {
      int i;
      si = snd_sync(sp->sync);
      for (i = 0; i < si->chans; i++) 
	si->begs[i] = beg;
    }
  else
    {
      if (over_selection)
	{
	  if (selection_is_active())
	    {
	      si = selection_sync();
	      dur = selection_len();
	    }
	  else
	    {
	      snd_warning_without_format(_("no selection"));
	      return(NULL);
	    }
	}
    }
  if (si == NULL) 
    si = make_simple_sync(cp, beg);
  sc = (sync_state *)calloc(1, sizeof(sync_state));
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
  int ip, stop_point = 0, impulse_chan = 0, filter_chans, fftsize, dataformat;
  bool ok = false;
  off_t filtersize = 0, filesize = 0, dataloc;
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
      if (!sp) return(NULL);
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

#if HAVE_FORTH
  origin = mus_format("\"%s\" %.3f %s", 
		      filename, amp,  (cp == NULL) ? S_convolve_selection_with : S_convolve_with);
#else
  origin = mus_format("%s" PROC_OPEN "\"%s\"" PROC_SEP "%.3f", 
	       TO_PROC_NAME((cp == NULL) ? S_convolve_selection_with : S_convolve_with), 
	       filename, amp);
#endif
  if (!(ss->stopped_explicitly))
    {
      for (ip = 0; ip < si->chans; ip++)
	{
	  char *ofile, *saved_chan_file;
	  io_error_t io_err;

	  ok = false;
      	  ucp = si->cps[ip];
	  if (!(editable_p(ucp))) continue;
	  sp = ucp->sound;
	  if (!(sp->active)) continue;
	  if ((ip == 0) || (sp != gsp)) 
	    gsp = ucp->sound; 

	  /* ofile here = new convolved data */
	  ofile = snd_tempnam();

	  saved_chan_file = snd_tempnam();
	  io_err = save_channel_edits(ucp, saved_chan_file, to_c_edit_position(ucp, edpos, S_convolve_with, arg_pos));
	  if (io_err != IO_NO_ERROR)
	    {
	      if (ofile) free(ofile);
	      free_sync_state(sc);
	      return(mus_format(_("convolve: save chan (%s[%d]) in %s hit error: %s\n"),
				sp->short_filename, ucp->chan, 
				saved_chan_file, snd_open_strerror()));
	    }
	  else
	    {
	      int scfd;
	      scfd = mus_file_open_read(saved_chan_file);
	      if (scfd == -1) 
		{
		  if (ofile) free(ofile);
		  free_sync_state(sc);
		  return(mus_format(_("convolve: open saved chan (%s[%d]) file %s hit error: %s\n"),
				    sp->short_filename, ucp->chan, 
				    saved_chan_file, snd_open_strerror()));
		}
	      else
		{
		  file_info *hdr;
		  int fltfd;
		  hdr = sp->hdr;
		  snd_file_open_descriptors(scfd,
					    saved_chan_file,
					    hdr->format,
					    hdr->data_location,
					    1, hdr->type); /* ??? */
		  fltfd = mus_file_open_read(filename);
		  if (fltfd == -1) 
		    {
		      if (ofile) free(ofile);
		      free_sync_state(sc);
		      return(mus_format(_("convolve: open filter file %s hit error: %s\n"), 
					filename, snd_open_strerror()));
		    }
		  else
		    {
		      snd_file_open_descriptors(fltfd,
						filename,
						dataformat,
						dataloc,
						filter_chans,
						mus_sound_header_type(filename));
		      if (cp == NULL)
			filesize = selection_len();
		      else filesize = to_c_edit_samples(ucp, edpos, S_convolve_with, arg_pos);
		      if (filesize > 0)
			{
			  int ipow;
			  ipow = (int)(ceil(log(filtersize + filesize) / log(2.0))) + 1;
			  fftsize = snd_int_pow2(ipow);
			  ok = true;
			  c_convolve(ofile, amp, scfd,
				     mus_sound_data_location(saved_chan_file),
				     fltfd, dataloc, filtersize, fftsize, filter_chans, impulse_chan,
				     filtersize + filesize + 1,
				     gsp);
			  impulse_chan++;
			  if (impulse_chan >= filter_chans) 
			    impulse_chan = 0;
			}
		      if (mus_file_close(fltfd) != 0)
			{
			  if (ofile) free(ofile);
			  free_sync_state(sc);
			  return(mus_format(_("convolve: close filter file %s hit error: %s\n"), 
					    filename, snd_io_strerror()));
			}
		    }
		}
	      if (mus_file_close(scfd) != 0)
		{
		  if (ofile) free(ofile);
		  free_sync_state(sc);
		  return(mus_format(_("convolve: close saved chan (%s[%d]) file %s hit error: %s\n"),
				    sp->short_filename, ucp->chan, 
				    saved_chan_file, snd_io_strerror()));
		}
	    }
	  snd_remove(saved_chan_file, REMOVE_FROM_CACHE);
	  free(saved_chan_file);

	  if (ok)
	    {
	      if (cp == NULL)
		{
		  delete_samples(si->begs[ip], sc->dur, ucp, ucp->edit_ctr);
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
	      else file_override_samples(filtersize + filesize, ofile, ucp, 0, DELETE_ME, origin);
	    }
	  if (ofile) free(ofile);
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
      free(origin); 
      origin = NULL;
    }
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

void scale_by(chan_info *cp, Float *ur_scalers, int len, bool over_selection)
{
  /* if over_selection, sync to current selection, else sync to current sound */
  /* 3-Oct-00: the scale factors are now embedded in the edit fragments  */
  sync_info *si;
  int i, j;
  if (over_selection)
    si = selection_sync();
  else si = sync_to_chan(cp);
  for (i = 0, j = 0; i < si->chans; i++)
    {
      off_t beg, frames;
      chan_info *ncp;
      ncp = si->cps[i];
      if (over_selection)
	{
	  beg = selection_beg(ncp);
	  frames = selection_end(ncp) - beg + 1;
	}
      else
	{
	  beg = 0;
	  frames = CURRENT_SAMPLES(ncp);
	}
      scale_channel(ncp, ur_scalers[j], beg, frames, ncp->edit_ctr, NOT_IN_AS_ONE_EDIT);
      j++;
      if (j >= len) j = 0;
    }
  free_sync_info(si);
}


static Float channel_maxamp_and_position(chan_info *cp, int edpos, off_t *maxpos)
{
  /* maxamp position is not tracked in peak env because it gloms up the code, and cannot easily be saved/restored in the peak env files */
  Float val;
  int pos;
  if (edpos == AT_CURRENT_EDIT_POSITION) pos = cp->edit_ctr; else pos = edpos;
  if ((peak_env_maxamp_ok(cp, pos)) && (!maxpos))
    return(peak_env_maxamp(cp, pos));
  val = ed_maxamp(cp, pos);
  if (maxpos) (*maxpos) = ed_maxamp_position(cp, pos);
  if ((val >= 0.0) && ((!maxpos) || ((*maxpos) >= 0)))
    return(val);
  val = channel_local_maxamp(cp, 0, cp->edits[pos]->samples, pos, maxpos);
  set_ed_maxamp(cp, pos, val);
  if (maxpos) set_ed_maxamp_position(cp, pos, (*maxpos));
  return(val);
}


Float channel_maxamp(chan_info *cp, int edpos)
{
  return(channel_maxamp_and_position(cp, edpos, NULL));
}


off_t channel_maxamp_position(chan_info *cp, int edpos)
{
  off_t maxpos = 0;
  channel_maxamp_and_position(cp, edpos, &maxpos);
  return(maxpos);
}


bool scale_to(snd_info *sp, chan_info *cp, Float *ur_scalers, int len, bool over_selection)
{
  /* essentially the same as scale-by, but first take into account current maxamps */
  /* here it matters if more than one arg is given -- if one, get overall maxamp */
  /*   if more than one, get successive maxamps */
  bool scaled = false;
  int i, chans, nlen, datum_size;
  off_t beg, frames;
  sync_info *si = NULL;
  chan_info *ncp;
  Float maxamp = -1.0, val, norm = 1.0;
  Float *scalers;
  char *origin = NULL;

  if ((!over_selection) && (cp == NULL)) return(false);
  if (over_selection) 
    {
      if (!(selection_is_active())) 
	return(false);
      si = selection_sync();
      sp = si->cps[0]->sound;
    }
  else si = sync_to_chan(cp);

  datum_size = mus_bytes_per_sample((sp->hdr)->format);
  chans = si->chans;
  scalers = (Float *)calloc(chans, sizeof(Float));
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
	  for (i = 0; i < chans; i++)
	    {
	      ncp = si->cps[i];
	      if (over_selection)
		val = selection_maxamp(ncp);
	      else val = channel_maxamp(ncp, AT_CURRENT_EDIT_POSITION);
	      if (val > maxamp) maxamp = val;
	    }
	  if ((!(clipping(ss))) && 
	      (scalers[0] == 1.0) &&
	      (datum_size <= 2))
	    {
	      if (datum_size == 2)
		scalers[0] = 32767.0 / 32768.0;
	      else scalers[0] = 127.0 / 128.0;
	    }
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
	      if (over_selection)
		val = selection_maxamp(ncp);
	      else val = channel_maxamp(ncp, AT_CURRENT_EDIT_POSITION);
	      if (val > maxamp) maxamp = val;
	      if (val != 0.0)
		{
		  if ((!(clipping(ss))) && 
		      (scalers[i] == 1.0) && 
		      (datum_size <= 2))
		    {
		      if (datum_size == 2)
			scalers[0] = 32767.0 / 32768.0;
		      else scalers[0] = 127.0 / 128.0;
		    }
		  scalers[i] /= val;
		}
	      else scalers[i] = 0.0;
	    }
	  else maxamp = 1.0; /* turn off the maxamp check */
	}
    }

  if (maxamp != 0.0)
    {
      for (i = 0; i < si->chans; i++)
	{
	  ncp = si->cps[i];
	  if (nlen > i) norm = ur_scalers[i]; else norm = ur_scalers[0];
	  if (over_selection)
	    {
	      beg = selection_beg(ncp);
	      frames = selection_end(ncp) - beg + 1;
#if HAVE_FORTH
	      origin = mus_format("%.3f" PROC_SEP OFF_TD PROC_SEP OFF_TD " %s", norm, beg, frames, S_normalize_channel);
#else
	      origin = mus_format("%s" PROC_OPEN "%.3f" PROC_SEP OFF_TD PROC_SEP OFF_TD, TO_PROC_NAME(S_normalize_channel), norm, beg, frames);
#endif
	    }
	  else
	    {
	      beg = 0;
	      frames = CURRENT_SAMPLES(ncp);
#if HAVE_FORTH
	      origin = mus_format("%.3f 0 " PROC_FALSE " %s", norm, S_normalize_channel);
#else
	      origin = mus_format("%s" PROC_OPEN "%.3f" PROC_SEP "0" PROC_SEP PROC_FALSE, TO_PROC_NAME(S_normalize_channel), norm);
#endif
	    }
	  scale_channel_with_origin(ncp, scalers[i], beg, frames, ncp->edit_ctr, NOT_IN_AS_ONE_EDIT, origin);
	  if (origin) free(origin);
	  origin = NULL;
	}
      scaled = true;
    }

  free(scalers);
  free_sync_info(si);
  return(scaled);
}


static void swap_channels(chan_info *cp0, chan_info *cp1, off_t beg, off_t dur, int pos0, int pos1)
{
  snd_fd *c0, *c1;
  snd_info *sp0;
  file_info *hdr0 = NULL, *hdr1 = NULL;
  int j, ofd0 = 0, ofd1 = 0, datumb = 0;
  bool temp_file;
  off_t k;
  mus_sample_t **data0, **data1;
  mus_sample_t *idata0, *idata1;
  bool reporting = false;
  char *ofile0 = NULL, *ofile1 = NULL;
  io_error_t io_err = IO_NO_ERROR;

  if (dur <= 0) return;
  if ((!(editable_p(cp0))) || (!(editable_p(cp1)))) return;
  sp0 = cp0->sound;
  reporting = ((sp0) && (dur > REPORTING_SIZE) && (!(cp0->squelch_update)));
  if (reporting) start_progress_report(cp0);

  if (dur > MAX_BUFFER_SIZE)
    {
      temp_file = true; 
      ofile0 = snd_tempnam();
      hdr0 = make_temp_header(ofile0, SND_SRATE(sp0), 1, dur, (char *)S_swap_channels);
      ofd0 = open_temp_file(ofile0, 1, hdr0, &io_err);
      if (ofd0 == -1)
	{
	  free_file_info(hdr0);
	  snd_error(_("%s " S_swap_channels " temp file %s: %s\n"), 
		    (io_err != IO_NO_ERROR) ? io_error_name(io_err) : "can't open",
		    ofile0, 
		    snd_open_strerror());
	  return;
	}
      datumb = mus_bytes_per_sample(hdr0->format);
      ofile1 = snd_tempnam();
      hdr1 = make_temp_header(ofile1, SND_SRATE(sp0), 1, dur, (char *)S_swap_channels);
      ofd1 = open_temp_file(ofile1, 1, hdr1, &io_err);
      if (ofd1 == -1)
	{
	  close_temp_file(ofile0, ofd0, hdr0->type, 0);
	  free_file_info(hdr0);
	  free_file_info(hdr1);
	  if (ofile0) free(ofile0);
	  snd_error(_("%s " S_swap_channels " temp file %s: %s\n"), 
		    (io_err != IO_NO_ERROR) ? io_error_name(io_err) : "can't open",
		    ofile1, 
		    snd_open_strerror());
	  return;
	}
    }
  else temp_file = false;

  data0 = (mus_sample_t **)malloc(sizeof(mus_sample_t *));
  data0[0] = (mus_sample_t *)calloc(MAX_BUFFER_SIZE, sizeof(mus_sample_t)); 
  data1 = (mus_sample_t **)malloc(sizeof(mus_sample_t *));
  data1[0] = (mus_sample_t *)calloc(MAX_BUFFER_SIZE, sizeof(mus_sample_t)); 
  idata0 = data0[0];
  idata1 = data1[0];
  c0 = init_sample_read_any(beg, cp0, READ_FORWARD, pos0);
  c1 = init_sample_read_any(beg, cp1, READ_FORWARD, pos1);

  if (temp_file)
    {
      ss->stopped_explicitly = false;
      j = 0;
      for (k = 0; k < dur; k++)
	{
	  idata0[j] = read_sample_to_mus_sample(c1);
	  idata1[j] = read_sample_to_mus_sample(c0);
	  j++;
	  if (j == MAX_BUFFER_SIZE)
	    {
	      int err;
	      err = mus_file_write(ofd0, 0, j - 1, 1, data0);
	      err = mus_file_write(ofd1, 0, j - 1, 1, data1);
	      j = 0;
	      if (err == -1) break;
	      if (reporting) 
		{
		  progress_report(cp0, (Float)((double)k / (double)dur));
		  if (ss->stopped_explicitly) break;
		  if ((cp0->active < CHANNEL_HAS_EDIT_LIST) || 
		      (cp1->active < CHANNEL_HAS_EDIT_LIST))
		    {
		      ss->stopped_explicitly = true;
		      break;
		    }
		}
	    }
	}
      if (!(ss->stopped_explicitly))
	{
	  if (j > 0) 
	    {
	      mus_file_write(ofd0, 0, j - 1, 1, data0);
	      mus_file_write(ofd1, 0, j - 1, 1, data1);
	    }
	}
      close_temp_file(ofile0, ofd0, hdr0->type, dur * datumb);
      close_temp_file(ofile1, ofd1, hdr1->type, dur * datumb);
      free_file_info(hdr0);
      free_file_info(hdr1);
      if (!(ss->stopped_explicitly))
	{
	  file_change_samples(beg, dur, ofile0, cp0, 0, DELETE_ME, S_swap_channels, cp0->edit_ctr);
	  file_change_samples(beg, dur, ofile1, cp1, 0, DELETE_ME, S_swap_channels, cp1->edit_ctr);
	}
      else
	{
	  string_to_minibuffer(sp0, _("swap interrupted"));
	  ss->stopped_explicitly = false;
	}
      if (ofile0) {free(ofile0); ofile0 = NULL;}
      if (ofile1) {free(ofile1); ofile1 = NULL;}
      if (reporting) finish_progress_report(cp0);
    }
  else 
    {
      for (k = 0; k < dur; k++)
	{
	  idata0[k] = read_sample_to_mus_sample(c1);
	  idata1[k] = read_sample_to_mus_sample(c0);
	}
      change_samples(beg, dur, data0[0], cp0, S_swap_channels, cp0->edit_ctr);
      change_samples(beg, dur, data1[0], cp1, S_swap_channels, cp1->edit_ctr);
    }
  swap_marks(cp0, cp1);
  update_graph(cp0);
  update_graph(cp1);
  if (ofile0) free(ofile0);
  if (ofile1) free(ofile1);
  free(data0[0]);
  free(data0);
  free(data1[0]);
  free(data1);
  free_snd_fd(c0);
  free_snd_fd(c1);
}


/* -------- src -------- */

typedef struct {
  mus_any *gen;
  snd_fd *sf;
  off_t sample;
  int dir;
} src_state;

static Float src_input_as_needed(void *arg, int direction) 
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
  return(read_sample(sf));
}


static src_state *make_src(Float srate, snd_fd *sf)
{
  src_state *sr;
  if ((sinc_width(ss) > MUS_MAX_CLM_SINC_WIDTH) ||
      (sinc_width(ss) < 0) ||
      (fabs(srate) > MUS_MAX_CLM_SRC))
    return(NULL);
  sr = (src_state *)calloc(1, sizeof(src_state));
  sr->sf = sf;
  if (srate >= 0.0) sr->dir = 1; else sr->dir = -1;          /* if env on src, this will be 0.0 even if env vals are < 0.0 */
  sr->gen = mus_make_src(&src_input_as_needed, srate, sinc_width(ss), (void *)sr);
  mus_set_increment(sr->gen, srate);
  sr->sample = 0;
  return(sr);
}


static src_state *free_src(src_state *sr)
{
  mus_free(sr->gen);
  free(sr);
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

static char *reverse_channel(chan_info *cp, snd_fd *sf, off_t beg, off_t dur, XEN edp, const char *caller, int arg_pos);


static char *src_channel_with_error(chan_info *cp, snd_fd *sf, off_t beg, off_t dur, Float ratio, mus_any *egen, 
				    const char *origin, bool over_selection,
				    bool *clm_err)
{
  snd_info *sp = NULL;
  bool reporting = false;
  bool full_chan;
  mus_sample_t **data;
  file_info *hdr = NULL;
  int j, ofd = 0, datumb = 0, err = 0;
  off_t *old_marks = NULL, *new_marks = NULL;
  int cur_marks = 0, m;
  off_t k;
  char *ofile = NULL;
  mus_sample_t *idata;
  io_error_t io_err = IO_NO_ERROR;
  src_state *sr;

  if ((egen == NULL) && (sf->edit_ctr == cp->edit_ctr))
    {
      if (ratio == 1.0) 
	return(NULL);
      if (ratio == -1.0)
	return(reverse_channel(cp, sf, beg, dur, C_TO_XEN_INT(sf->edit_ctr), origin, 0));
    }

  sp = cp->sound;
  if (!(editable_p(cp))) return(NULL); /* edit hook result perhaps */

  sr = make_src(ratio, sf);  /* ratio is 0.0 if egen because the envelope is the srate, but it's passed as the "sr-change" arg */
  if ((egen) &&
      (mus_phase(egen) < 0.0))
    sr->dir = -1;

  if (sr == NULL) 
    {
      (*clm_err) = true;
      return(mus_format("invalid src ratio: %f\n", ratio));
    }

  full_chan = ((beg == 0) && (dur == cp->edits[sf->edit_ctr]->samples)); /* not CURRENT_SAMPLES here! */

  reporting = ((sp) && (dur > REPORTING_SIZE) && (!(cp->squelch_update)));
  if (reporting) start_progress_report(cp);

  ofile = snd_tempnam();
  hdr = make_temp_header(ofile, SND_SRATE(sp), 1, dur, (char *)origin);
  ofd = open_temp_file(ofile, 1, hdr, &io_err);
  if (ofd == -1)
    {
      return(mus_format(_("%s %s temp file %s: %s\n"), 
			(io_err != IO_NO_ERROR) ? io_error_name(io_err) : "can't open",
			origin, ofile, 
			snd_open_strerror()));
    }

  data = (mus_sample_t **)malloc(sizeof(mus_sample_t *));
  data[0] = (mus_sample_t *)calloc(MAX_BUFFER_SIZE, sizeof(mus_sample_t)); 
  datumb = mus_bytes_per_sample(hdr->format);
  idata = data[0];
  j = 0;
  ss->stopped_explicitly = false;

  if (egen == NULL)
    {
      if ((ratio == 0.5) || (ratio == 2.0)) /* read forward is built-in in the mus_src_20 (7176) and mus_src_05 cases */
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
		      progress_report(cp, (Float)((double)(sr->sample) / (double)dur));
		      if (ss->stopped_explicitly) break;
		      if (!(sp->active))
			{
			  ss->stopped_explicitly = true;
			  break;
			}
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
		      progress_report(cp, (Float)((double)(sr->sample) / (double)dur));
		      if (ss->stopped_explicitly) break;
		      if (!(sp->active))
			{
			  ss->stopped_explicitly = true;
			  break;
			}
		    }
		}
	    }
	}
    }
  else
    {
      off_t next_pass;
      off_t cur_mark_sample = -1;
      int cur_mark = 0, cur_new_mark = 0;
      Float env_val;

      cur_mark_sample = -1;
      env_val = mus_env(egen); 
      /* envelope case -- have to go by sr->sample, not output sample counter, also check marks */

      if ((cp->edits[cp->edit_ctr]->marks) && 
	  (cp->edits[cp->edit_ctr]->mark_ctr >= 0))
	{
	  mark **mps;
	  mps = cp->edits[cp->edit_ctr]->marks;
	  cur_marks = cp->edits[cp->edit_ctr]->mark_ctr + 1;
	  new_marks = (off_t *)malloc(cur_marks * sizeof(off_t));
	  old_marks = (off_t *)malloc(cur_marks * sizeof(off_t));
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
		  progress_report(cp, (Float)((double)(sr->sample) / (double)dur));
		  if (ss->stopped_explicitly) break;
		  if (!(sp->active))
		    {
		      ss->stopped_explicitly = true;
		      break;
		    }
		}
	    }
	  if (next_pass != sr->sample)             /* tick env forward dependent on sr->sample */
	    {
	      off_t jj, idiff;
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

  if (reporting) finish_progress_report(cp);
  sr = free_src(sr);
  if ((!(ss->stopped_explicitly)) && (j > 0)) 
    mus_file_write(ofd, 0, j - 1, 1, data);
  close_temp_file(ofile, ofd, hdr->type, k * datumb);
  hdr = free_file_info(hdr);

  if (!(ss->stopped_explicitly))
    {
      char *new_origin = NULL;
      /* egen null -> use ratio, else env, if dur=samples #f */
      if (egen == NULL)
	{

#if HAVE_FORTH
	  if (dur == cp->edits[sf->edit_ctr]->samples)
	    new_origin = mus_format("%.4f" PROC_SEP OFF_TD PROC_SEP PROC_FALSE " %s", ratio, beg, S_src_channel);
	  else new_origin = mus_format("%.4f" PROC_SEP OFF_TD PROC_SEP OFF_TD " %s", ratio, beg, dur, S_src_channel);
#else
	  if (dur == cp->edits[sf->edit_ctr]->samples)
	    new_origin = mus_format("%s" PROC_OPEN "%.4f" PROC_SEP OFF_TD PROC_SEP PROC_FALSE, TO_PROC_NAME(S_src_channel), ratio, beg);
	  else new_origin = mus_format("%s" PROC_OPEN "%.4f" PROC_SEP OFF_TD PROC_SEP OFF_TD, TO_PROC_NAME(S_src_channel), ratio, beg, dur);
#endif
	}
      else
	{
	  Float base;
	  char *envstr;
	  env *newe;
	  base = mus_increment(egen);
	  newe = make_envelope_with_offset_and_scaler(mus_data(egen), mus_env_breakpoints(egen) * 2, mus_offset(egen), mus_scaler(egen));
	  envstr = env_to_string(newe);

#if HAVE_FORTH
	  if (base == 1.0)
	    {
	      if (dur == cp->edits[sf->edit_ctr]->samples)
		new_origin = mus_format("%s" PROC_SEP OFF_TD PROC_SEP PROC_FALSE " %s", envstr, beg, S_src_channel);
	      else new_origin = mus_format("%s" PROC_SEP OFF_TD PROC_SEP OFF_TD " %s", envstr, beg, dur, S_src_channel);
	    }
	  else new_origin = mus_format("%s :base %.4f :end " OFF_TD " %s " OFF_TD PROC_SEP OFF_TD " %s", envstr, base, dur, S_make_env, beg, dur, S_src_channel);
#else
	  if (base == 1.0)
	    {
	      if (dur == cp->edits[sf->edit_ctr]->samples)
		new_origin = mus_format("%s" PROC_OPEN "%s" PROC_SEP OFF_TD PROC_SEP PROC_FALSE, TO_PROC_NAME(S_src_channel), envstr, beg);
	      else new_origin = mus_format("%s" PROC_OPEN "%s" PROC_SEP OFF_TD PROC_SEP OFF_TD, TO_PROC_NAME(S_src_channel), envstr, beg, dur);
	    }
	  else new_origin = mus_format("%s" PROC_OPEN BPAREN "%s" PROC_OPEN "%s" PROC_SEP ":base" PROC_SEP "%.4f" PROC_SEP ":end" PROC_SEP OFF_TD ")" PROC_SEP OFF_TD PROC_SEP OFF_TD, 
				       TO_PROC_NAME(S_make_env), TO_PROC_NAME(S_src_channel), envstr, base, dur, beg, dur);
#endif
	  if (envstr) free(envstr);
	  free_env(newe);
	}

      if (!full_chan)
	{
	  /* here we need delete followed by insert since dur is probably different */
	  if (k == dur)
	    file_change_samples(beg, dur, ofile, cp, 0, DELETE_ME, new_origin, sf->edit_ctr);
	  else
	    {
	      delete_samples(beg, dur, cp, sf->edit_ctr);
	      file_insert_samples(beg, k, ofile, cp, 0, DELETE_ME, new_origin, cp->edit_ctr);
	      if (over_selection)
		reactivate_selection(cp, beg, beg + k); /* backwards compatibility */
	      backup_edit_list(cp);
	      ripple_marks(cp, 0, 0);
	    }
	  update_graph(cp);
	}
      else file_override_samples(k, ofile, cp, 0, DELETE_ME, new_origin);

      if (new_origin) free(new_origin);

      /* not file_change_samples because that would not necessarily change the current file length */
      if (cp->edits[cp->edit_ctr]->marks)
	{
	  if (egen == NULL)
	    src_marks(cp, ratio, dur, k, beg, full_chan);
	  else
	    {
	      if (new_marks) 
		reset_marks(cp, cur_marks, new_marks, beg + dur, (k - dur), full_chan);
	    }
	}

      /* if possible, copy the previous amp env and change the samps_per_bin to reflect ratio */
      if ((full_chan) && (egen == NULL) &&          /* just ratio -- egen is freed by caller */
	  (!(cp->edits[cp->edit_ctr]->peak_env)))   /* can this happen? */
	{
	  peak_env_info *ep;
	  ep = cp->edits[sf->edit_ctr]->peak_env; /* previous peak env (sf is freed by caller) */
	  if (ep)
	    {
	      Float bratio;
	      int iratio;
	      bratio = ep->samps_per_bin / fabs(ratio);
	      iratio = (int)bratio;
	      if ((bratio - iratio) < .001)
		{
		  peak_env_info *new_ep;
		  new_ep = copy_peak_env_info(ep, (ratio < 0.0)); /* might return NULL if ep but not ep->completed */
		  if (new_ep)
		    {
		      new_ep->samps_per_bin = iratio;
		      cp->edits[cp->edit_ctr]->peak_env = new_ep;
		    }
		}
	    }
	}

      update_graph(cp);
    }
  else
    {
      string_to_minibuffer(sp, _("src interrupted"));
      /* should we remove the temp file here? */
      ss->stopped_explicitly = false;
    }

  if (old_marks) free(old_marks);
  old_marks = NULL;
  if (new_marks) free(new_marks);
  new_marks = NULL;
  free(ofile); 
  ofile = NULL;
  free(data[0]);
  free(data);
  return(NULL);
}


void src_env_or_num(chan_info *cp, env *e, Float ratio, bool just_num, 
		    const char *origin, bool over_selection, mus_any *gen, XEN edpos, int arg_pos)
{
  snd_info *sp = NULL;
  sync_state *sc;
  sync_info *si;
  snd_fd **sfs;
  int i;
  off_t scdur;
  int stop_point = 0;
  char *errmsg = NULL;

  if ((!just_num) && (e == NULL) && (gen == NULL)) return;
  if ((just_num) && (ratio == 0.0)) return;

  /* get envelope or src ratio */
  sp = cp->sound;
  /* get current syncd chans */
  sc = get_sync_state(sp, cp, 0, over_selection, 
		      (ratio < 0.0) ? READ_BACKWARD : READ_FORWARD, /* 0->beg, 0->over_selection (ratio = 0.0 if from enved) */ 
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
	  off_t dur;
	  mus_any *egen = NULL;
	  bool clm_err = false;
	  cp = si->cps[i];
	  if (scdur == 0) 
	    dur = to_c_edit_samples(cp, edpos, origin, arg_pos); 
	  else dur = scdur;
	  if (dur == 0) 
	    {
	      sfs[i] = free_snd_fd(sfs[i]); 
	      continue;
	    }
	  if (!just_num)
	    {
	      if (e)
		egen = mus_make_env_with_length(e->data, e->pts, 1.0, 0.0, e->base, dur);
	      else egen = gen;
	      if (egen) ratio = 0.0;            /* added 14-Mar-01 otherwise the envelope is an offset? */
	    }
	  errmsg = src_channel_with_error(cp, sfs[i], si->begs[i], dur, ratio, egen, origin, over_selection, &clm_err);
	  if (egen)
	    {
	      if (e) 
		mus_free(egen);
	      else mus_reset(gen);
	    }
	  if (errmsg) break;
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
    free_snd_fd(sfs[i]);
  free_sync_state(sc);
  if (errmsg)
    snd_error_without_format(errmsg);
}


/* FIR filtering */

static Float *get_filter_coeffs(int order, env *e)
{
  /* interpret e as frequency response */
  Float *a = NULL, *fdata;
  if (!e) return(NULL);

  /* get the frequency envelope and design the FIR filter */
  fdata = sample_linear_env(e, order);
  if (!fdata) return(NULL);
  a = (Float *)calloc(order, sizeof(Float));

  mus_make_fir_coeffs(order, fdata, a);

  free(fdata);
  return(a);
}


void display_frequency_response(env *e, axis_info *ap, axis_context *gax, int order, bool dBing)
{
  /* not cp->min_dB here -- this is sound panel related which refers to ss->min_dB */
  Float *coeffs = NULL;
  int height, width, i, pts, x1, y1;
  Float samps_per_pixel, invpts, resp, pix;
  int fsize, j;
  Float step, fx;
  Float *rl, *im;

  if (order & 1) order++;

  height = (ap->y_axis_y1 - ap->y_axis_y0);
  width = (ap->x_axis_x1 - ap->x_axis_x0);
  pts = order * 4;
  if (pts > width) pts = width;
  if (pts <= 0) pts = 1;
  invpts = 1.0 / (Float)pts;
  samps_per_pixel = (Float)(ap->x_axis_x1 - ap->x_axis_x0) * invpts;

  x1 = ap->x_axis_x0;
  coeffs = get_filter_coeffs(order, e);

  if (!coeffs) return;
  fsize = 2 * snd_to_int_pow2((pts > order) ? pts : order); /* *2 for 1/2 frqs */
  rl = (Float *)calloc(fsize, sizeof(Float));
  im = (Float *)calloc(fsize, sizeof(Float));
  for (i = 0, j = order - 1; i < order / 2; i++, j -= 2) rl[j] = coeffs[i]; /* by 2 from 1 for 1/2 bins */

  mus_fft(rl, im, fsize, -1);

  resp = 2 * rl[0];
  if (dBing)
    y1 = (int)(ap->y_axis_y0 + (min_dB(ss) - in_dB(min_dB(ss), ss->lin_dB, resp)) * height / min_dB(ss));
  else y1 = (int)(ap->y_axis_y0 + resp * height);
  x1 = ap->x_axis_x0;
  step = (Float)(fsize - 1) / (4 * (Float)pts); /* fsize-1 since we got 1 already, *4 due to double size fft */
  for (i = 1, pix = x1, fx = step; 
       i < pts; 
       i++, pix += samps_per_pixel, fx += step)
    {
      int fxi, x0, y0;
      x0 = x1;
      y0 = y1;
      x1 = (int)(pix);
      fxi = (int)fx;
      resp = 2 * (rl[fxi] + (fx - fxi) * (rl[fxi + 1] - rl[fxi]));
      if (resp < 0.0) resp = -resp;
      if (dBing)
	y1 = (int)(ap->y_axis_y0 + (min_dB(ss) - in_dB(min_dB(ss), ss->lin_dB, resp)) * height / min_dB(ss));
      else y1 = (int)(ap->y_axis_y0 + resp * height);
      draw_line(gax, x0, y0, x1, y1);
    }
  free(rl);
  free(im);
  free(coeffs);
}


static char *clm_channel(chan_info *cp, mus_any *gen, off_t beg, off_t dur, int edpos, off_t overlap, const char *origin)
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
  if (!(editable_p(cp))) return(NULL);

  sf = init_sample_read_any(beg, cp, READ_FORWARD, edpos);
  if (sf == NULL)
    return(mus_format(_("%s can't read %s[%d] channel data!"), S_clm_channel, sp->short_filename, cp->chan));

  if ((dur + overlap) > MAX_BUFFER_SIZE)
    {
      io_error_t io_err = IO_NO_ERROR;
      temp_file = true; 
      ofile = snd_tempnam();
      hdr = make_temp_header(ofile, SND_SRATE(sp), 1, dur + overlap, S_clm_channel);
      ofd = open_temp_file(ofile, 1, hdr, &io_err);
      if (ofd == -1)
	{
	  free_snd_fd(sf); 
	  return(mus_format(_("%s %s temp file %s: %s\n"), 
			    (io_err != IO_NO_ERROR) ? io_error_name(io_err) : "can't open",
			    S_clm_channel, ofile, 
			    snd_open_strerror()));
	}
      datumb = mus_bytes_per_sample(hdr->format);
    }
  else temp_file = false;
  data = (mus_sample_t **)malloc(sizeof(mus_sample_t *));
  data[0] = (mus_sample_t *)calloc(MAX_BUFFER_SIZE, sizeof(mus_sample_t)); 
  idata = data[0];
  if (temp_file)
    {
      j = 0;
      for (k = 0; k < dur; k++)
	{
	  idata[j++] = MUS_FLOAT_TO_SAMPLE(MUS_RUN(gen, read_sample(sf), 0.0));
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
	idata[k] = MUS_FLOAT_TO_SAMPLE(MUS_RUN(gen, read_sample(sf), 0.0));
      j = (int)dur;
    }
  for (k = 0; k < overlap; k++)
    {
      idata[j++] = MUS_FLOAT_TO_SAMPLE(MUS_RUN(gen, 0.0, 0.0) + read_sample(sf));
      if ((temp_file) && (j == MAX_BUFFER_SIZE))
	{
	  err = mus_file_write(ofd, 0, j - 1, 1, data);
	  j = 0;
	  if (err == -1) break;
	}
    }
  dur += overlap;
  sf = free_snd_fd(sf);
  if (temp_file)
    {
      if (j > 0) mus_file_write(ofd, 0, j - 1, 1, data);
      close_temp_file(ofile, ofd, hdr->type, dur * datumb);
      hdr = free_file_info(hdr);
      file_change_samples(beg, dur, ofile, cp, 0, DELETE_ME, origin, edpos);
      if (ofile) 
	{
	  free(ofile); 
	  ofile = NULL;
	}
    }
  else 
    {
      if (dur > 0) 
	change_samples(beg, dur, idata, cp, origin, edpos);
    }
  update_graph(cp); 
  free(data[0]);
  free(data);
  return(NULL);
}

#if HAVE_FFTW || HAVE_FFTW3
#define TWO_30 1073741824
#define MAX_SINGLE_FFT_SIZE 1048576


static Float convolve_next_sample(void *ptr, int dir)
{
  return(read_sample(((snd_fd *)ptr)));
}


static char *convolution_filter(chan_info *cp, int order, env *e, snd_fd *sf, off_t beg, off_t dur, 
				const char *origin, Float *precalculated_coeffs)
{
  snd_info *sp;
  file_info *hdr = NULL;
  int j, ofd = 0, datumb = 0, err = 0;
  char *ofile = NULL;
  int fsize;
  Float *fltdat = NULL;
  io_error_t io_err = IO_NO_ERROR;

  if (!(editable_p(cp))) return(NULL);
  sp = cp->sound;
  dur += order;
  if (dur < TWO_30)
    fsize = snd_to_int_pow2(dur);
  else fsize = TWO_30;
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

  ofd = open_temp_file(ofile, 1, hdr, &io_err);
  if (ofd == -1)
    {
      return(mus_format(_("%s %s temp file %s: %s\n"), 
			(io_err != IO_NO_ERROR) ? io_error_name(io_err) : "can't open",
			origin, ofile, 
			snd_open_strerror()));
    }

  if (fsize > MAX_SINGLE_FFT_SIZE)
    {
      /* set up convolution generator and run overlap-add in order-sized blocks */
      bool reporting;
      mus_any *gen;
      mus_sample_t **data;
      mus_sample_t *idata;
      off_t offk;

      reporting = ((sp) && (dur > REPORTING_SIZE) && (!(cp->squelch_update)));
      if (order == 0) order = 65536; /* presumably fsize is enormous here, so no MIN needed */
      if (!(POWER_OF_2_P(order)))
	order = snd_to_int_pow2(order);
      fsize = 2 * order; /* need room for convolution */
      if (precalculated_coeffs)
	fltdat = precalculated_coeffs;
      else fltdat = get_filter_coeffs(order, e);

      gen = mus_make_convolve(NULL, fltdat, fsize, order, (void *)sf);

      j = 0;
      data = (mus_sample_t **)malloc(sizeof(mus_sample_t *));
      data[0] = (mus_sample_t *)calloc(MAX_BUFFER_SIZE, sizeof(mus_sample_t)); 
      idata = data[0];
      if (reporting) start_progress_report(cp);
      ss->stopped_explicitly = false;
      for (offk = 0; offk < dur; offk++)
	{
	  Float x;

	  x = mus_convolve(gen, convolve_next_sample);

	  idata[j] = MUS_FLOAT_TO_SAMPLE(x);
	  j++;
	  if (j == MAX_BUFFER_SIZE)
	    {
	      err = mus_file_write(ofd, 0, j - 1, 1, data);
	      j = 0;
	      if (err == -1) break;
	      if (reporting)
		{
		  progress_report(cp, (Float)((double)offk / (double)dur));
		  if (ss->stopped_explicitly) break;
		  if (!(sp->active))
		    {
		      ss->stopped_explicitly = true;
		      break;
		    }
		}
	    }
	}
      if (reporting) finish_progress_report(cp);
      if ((j > 0) && (!(ss->stopped_explicitly)))
	mus_file_write(ofd, 0, j - 1, 1, data);
      close_temp_file(ofile, ofd, hdr->type, dur * datumb);
      if (!(ss->stopped_explicitly))
	file_change_samples(beg, dur, ofile, cp, 0, DELETE_ME, origin, sf->edit_ctr);
      else 
	{
	  string_to_minibuffer(sp, _("filter interrupted"));
	  ss->stopped_explicitly = false;
	}
      mus_free(gen);
      free(data[0]);
      free(data);
    }
  else
    {
      /* we think there's enough memory to do the entire thing in one pass */
      if (precalculated_coeffs)
	fltdat = precalculated_coeffs;
      else fltdat = sample_linear_env(e, fsize);
      if (fltdat)
	{
	  Float *sndrdat;
	  Float scale;
	  int k;
	  ssize_t bytes;
	  sndrdat = (Float *)calloc(fsize, sizeof(Float));
	  for (k = 0; k < dur; k++) 
	    sndrdat[k] = (Float)(read_sample(sf));

	  mus_fftw(sndrdat, fsize, 1);

	  scale = 1.0 / (Float)fsize;
	  for (k = 0; k < fsize; k++)
	    sndrdat[k] *= (scale * fltdat[k]);         /* fltdat is already reflected around midpoint */

	  mus_fftw(sndrdat, fsize, -1);

	  bytes = write(ofd, sndrdat, fsize * sizeof(Float));
	  close_temp_file(ofile, ofd, hdr->type, fsize * sizeof(Float));
	  if (bytes != 0)
	    file_change_samples(beg, dur + order, ofile, cp, 0, DELETE_ME, origin, sf->edit_ctr);
	  else string_to_minibuffer(sp, _("can't write data?"));
	  free(sndrdat);
	}
      else 
	{
	  close_temp_file(ofile, ofd, hdr->type, 0);
	  snd_remove(ofile, REMOVE_FROM_CACHE);
	}
    }
  if (ofile) {free(ofile); ofile = NULL;}
  hdr = free_file_info(hdr);
  if ((fltdat) && (!precalculated_coeffs))  free(fltdat);
  update_graph(cp);
  return(NULL);
}
#endif



static char *direct_filter(chan_info *cp, int order, env *e, snd_fd *sf, off_t beg, off_t dur, 
			   const char *origin, bool truncate,
			   bool over_selection, mus_any *gen, Float *precalculated_coeffs)
{
  Float *a = NULL, *d = NULL;
  Float x = 0.0;
  snd_info *sp;
  bool reporting = false;
  int m;
  off_t offk;
  file_info *hdr = NULL;
  int j = 0, ofd = 0, datumb = 0, err = 0;
  bool temp_file;
  char *new_origin = NULL;
  mus_sample_t **data;
  mus_sample_t *idata;
  char *ofile = NULL;
  io_error_t io_err = IO_NO_ERROR;

  if (!(editable_p(cp))) return(NULL);
  sp = cp->sound;
  if ((!over_selection) || (!truncate))
    dur += order;
  /* if over-selection this causes it to clobber samples beyond the selection end -- maybe mix? */
  reporting = ((sp) && (dur > REPORTING_SIZE) && (!(cp->squelch_update)));
  if (reporting) start_progress_report(cp);

  if (dur > MAX_BUFFER_SIZE)
    {
      temp_file = true; 
      ofile = snd_tempnam();
      hdr = make_temp_header(ofile, SND_SRATE(sp), 1, dur, (char *)origin);
      ofd = open_temp_file(ofile, 1, hdr, &io_err);
      if (ofd == -1)
	{
	  return(mus_format(_("%s %s temp file %s: %s\n"), 
			    (io_err != IO_NO_ERROR) ? io_error_name(io_err) : "can't open",
			    origin, ofile, 
			    snd_open_strerror()));
	}
      datumb = mus_bytes_per_sample(hdr->format);
    }
  else temp_file = false;

  data = (mus_sample_t **)malloc(sizeof(mus_sample_t *));
  data[0] = (mus_sample_t *)calloc(MAX_BUFFER_SIZE, sizeof(mus_sample_t)); 
  idata = data[0];
  if (precalculated_coeffs)
    a = precalculated_coeffs;
  else 
    {
      if (order & 1) order++;
      a = get_filter_coeffs(order, e);
    }
  d = (Float *)calloc(order, sizeof(Float));
  if (gen)
    mus_reset(gen);
  else
    {
      for (m = 0; m < order; m++) d[m] = 0.0;
      if (over_selection)
	{
	  off_t prebeg = 0;
	  /* see if there's data to pre-load the filter */
	  if (beg >= order)
	    prebeg = order - 1;
	  else prebeg = beg;
	  if (prebeg > 0)
	    for (m = (int)prebeg; m > 0; m--)
	      d[m] = read_sample(sf);
	}
    }
  if ((over_selection) && (!truncate))
    dur -= order;
  if (!temp_file)
    {
      if (gen)
	{
	  for (j = 0; j < dur; j++)
	    idata[j] = MUS_FLOAT_TO_SAMPLE(MUS_RUN(gen, read_sample(sf), 0.0));
	}
      else
	{
	  /* splitting out symmetric case did not speed up this loop appreciably */
	  /* and using memmove for the "state" changes slowed it down by a factor of 2! */
	  for (j = 0; j < dur; j++)
	    {
	      x = 0.0; 
	      d[0] = read_sample(sf);
	      for (m = order - 1; m > 0; m--) 
		{
		  x += d[m] * a[m]; 
		  d[m] = d[m - 1];
		} 
	      x += d[0] * a[0]; 
	      idata[j] = MUS_FLOAT_TO_SAMPLE(x);
	    }
	}
    }
  else
    {
      for (offk = 0; offk < dur; offk++)
	{
	  if (gen)
	    x = MUS_RUN(gen, read_sample(sf), 0.0);
	  else
	    {
	      x = 0.0; 
	      d[0] = read_sample(sf);
	      for (m = order - 1; m > 0; m--) 
		{
		  x += d[m] * a[m]; 
		  d[m] = d[m - 1];
		} 
	      x += d[0] * a[0]; 
	    }
	  idata[j] = MUS_FLOAT_TO_SAMPLE(x);
	  j++;
	  if (j == MAX_BUFFER_SIZE)
	    {
	      err = mus_file_write(ofd, 0, j - 1, 1, data);
	      j = 0;
	      if (err == -1) break;
	      if (reporting) 
		{
		  progress_report(cp, (Float)((double)offk / (double)dur));
		  if (ss->stopped_explicitly) return(NULL);
		  if (!(sp->active))
		    {
		      ss->stopped_explicitly = true;
		      break;
		    }
		}
	    }
	}
    }
  if ((over_selection) && (!truncate))
    {
      snd_fd *sfold;
      sfold = init_sample_read_any(beg + dur, cp, READ_FORWARD, sf->edit_ctr);
      for (offk = 0; offk < order; offk++)
	{
	  if (gen)
	    x = MUS_RUN(gen, read_sample(sf), 0.0);
	  else
	    {
	      x = 0.0; 
	      d[0] = read_sample(sf);
	      for (m = order - 1; m > 0; m--) 
		{
		  x += d[m] * a[m]; 
		  d[m] = d[m - 1];
		} 
	      x += d[0] * a[0]; 
	    }
	  idata[j] = MUS_FLOAT_TO_SAMPLE(x + read_sample(sfold));
	  j++;
	  if ((temp_file) && (j == MAX_BUFFER_SIZE))
	    {
	      err = mus_file_write(ofd, 0, j - 1, 1, data);
	      j = 0;
	      if (err == -1) break;
	    }
	}
      dur += order;
      free_snd_fd(sfold);
    }
  if (reporting) finish_progress_report(cp);
  if (origin)
    new_origin = mus_strdup(origin);
  else
    {
      if (precalculated_coeffs)
	{
	  vct *v;
	  char *vstr = NULL;
	  v = (vct *)calloc(1, sizeof(vct));
	  v->length = order;
	  v->data = precalculated_coeffs;
	  vstr = mus_vct_to_readable_string(v);
#if HAVE_FORTH
	  if (dur == (order + cp->edits[sf->edit_ctr]->samples))
	    new_origin = mus_format("%s %d " OFF_TD PROC_SEP PROC_FALSE " %s", vstr, order, beg, S_filter_channel);
	  else new_origin = mus_format("%s %d " OFF_TD PROC_SEP OFF_TD " %s", vstr, order, beg, dur, S_filter_channel);
#else
	  if (dur == (order + cp->edits[sf->edit_ctr]->samples))
	    new_origin = mus_format("%s" PROC_OPEN "%s" PROC_SEP "%d" PROC_SEP OFF_TD PROC_SEP PROC_FALSE, 
				    TO_PROC_NAME(S_filter_channel), vstr, order, beg);
	  else new_origin = mus_format("%s" PROC_OPEN "%s" PROC_SEP "%d" PROC_SEP OFF_TD PROC_SEP OFF_TD, 
				       TO_PROC_NAME(S_filter_channel), vstr, order, beg, dur);
#endif
	  if (vstr) free(vstr);
	  free(v); /* not mus_vct_free because we don't own the data array */
	}
      else
	{
	  /* new_origin = filter-channel + envelope */
	  char *envstr;
	  envstr = env_to_string(e);
#if HAVE_FORTH
	  if (dur == (order + cp->edits[sf->edit_ctr]->samples))
	    new_origin = mus_format("%s %d " OFF_TD PROC_SEP PROC_FALSE " %s", envstr, order, beg, S_filter_channel);
	  else new_origin = mus_format("%s %d " OFF_TD PROC_SEP OFF_TD " %s", envstr, order, beg, dur, S_filter_channel);
#else
	  if (dur == (order + cp->edits[sf->edit_ctr]->samples))
	    new_origin = mus_format("%s" PROC_OPEN "%s" PROC_SEP "%d" PROC_SEP OFF_TD PROC_SEP PROC_FALSE, 
				    TO_PROC_NAME(S_filter_channel), envstr, order, beg);
	  else new_origin = mus_format("%s" PROC_OPEN "%s" PROC_SEP "%d" PROC_SEP OFF_TD PROC_SEP OFF_TD, 
				       TO_PROC_NAME(S_filter_channel), envstr, order, beg, dur);
#endif
	  if (envstr) free(envstr);
	}
    }
  if (temp_file)
    {
      if (j > 0) mus_file_write(ofd, 0, j - 1, 1, data);
      close_temp_file(ofile, ofd, hdr->type, dur * datumb);
      hdr = free_file_info(hdr);
      file_change_samples(beg, dur, ofile, cp, 0, DELETE_ME, new_origin, sf->edit_ctr);
      if (ofile) {free(ofile); ofile = NULL;}
    }
  else change_samples(beg, dur, data[0], cp, new_origin, sf->edit_ctr);
  if (new_origin) free(new_origin);
  update_graph(cp); 
  free(data[0]);
  free(data);
  if (d) free(d);
  if ((a) && (!precalculated_coeffs)) free(a);
  return(NULL);
}


static char *filter_channel(chan_info *cp, int order, env *e, off_t beg, off_t dur, int edpos, const char *origin, bool truncate, Float *coeffs)
{
  bool over_selection;
  snd_fd *sf;
  char *errstr = NULL;
  if ((order == 1) && (coeffs != NULL) && (e == NULL))
    {
      /* a silly optimization... */
      if ((coeffs[0] != 1.0) || (edpos != cp->edit_ctr))
	scale_channel(cp, coeffs[0], beg, dur, edpos, NOT_IN_AS_ONE_EDIT);
      return(NULL);
    }
  over_selection = ((beg != 0) || (dur < cp->edits[edpos]->samples));
  sf = init_sample_read_any(beg, cp, READ_FORWARD, edpos);
#if HAVE_FFTW || HAVE_FFTW3
  if ((!over_selection) &&
      ((order == 0) || (order >= 128)))
    errstr = convolution_filter(cp, order, e, sf, beg, dur, origin, coeffs);
  else
#endif
    errstr = direct_filter(cp, order, e, sf, beg, dur, origin, truncate, over_selection, NULL, coeffs);
  free_snd_fd(sf);
  return(errstr);
}


static char *apply_filter_or_error(chan_info *ncp, int order, env *e, 
				   const char *caller, const char *origin, bool over_selection, Float *ur_a, 
				   mus_any *gen, XEN edpos, int arg_pos, bool truncate, bool *clm_error)
{
  /* if string returned, needs to be freed */
  /* interpret e as frequency response and apply as filter to all sync'd chans */
  Float *a = NULL;
  sync_state *sc;
  sync_info *si;
  snd_info *sp;
  int i, stop_point = 0;
  off_t scdur, dur;
  snd_fd **sfs;
  chan_info *cp;
  char *errstr = NULL;

  if ((!e) && (!ur_a) && (!gen)) 
    return(NULL);

  if ((gen) && (!(MUS_RUN_P(gen))))
    {
      (*clm_error) = true;
      return(mus_format(_("%s: can't handle %s generators"),
			caller,
			mus_name(gen)));
    }

  sp = ncp->sound;
  sc = get_sync_state_1(sp, ncp, 0, over_selection, 
			READ_FORWARD, (over_selection) ? (order - 1) : 0, 
			edpos,
			caller, arg_pos);
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
	  if (!(sp->active)) continue;
	  if (scdur == 0) 
	    dur = to_c_edit_samples(cp, edpos, caller, arg_pos);
	  else dur = scdur;
	  if (dur == 0) 
	    {
	      sfs[i] = free_snd_fd(sfs[i]);
	      continue;
	    }

	  errstr = convolution_filter(cp, order, e, sfs[i], si->begs[i], dur, (origin) ? origin : caller, NULL);

	  sfs[i] = free_snd_fd(sfs[i]);
	  check_for_event();
	  if ((errstr) || (ss->stopped_explicitly))
	    {
	      stop_point = i;
	      break;
	    }
	}
    }
  else
#endif
    {
      /* use FIR filter */
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
	}
      /* now filter all currently sync'd chans (one by one) */
      /* for each decide whether a file or internal array is needed, scale, update edit tree */
      if (!(ss->stopped_explicitly))
	{
	  for (i = 0; i < si->chans; i++)
	    {
	      /* done channel at a time here, rather than in parallel as in apply-env because */
	      /* in this case, the various sync'd channels may be different lengths */
	      cp = si->cps[i];
	      if (scdur == 0) 
		dur = to_c_edit_samples(cp, edpos, caller, arg_pos);
	      else dur = scdur;
	      if (dur == 0) 
		{
		  sfs[i] = free_snd_fd(sfs[i]);
		  continue;
		}

	      errstr = direct_filter(cp, order, e, sfs[i], si->begs[i], dur,
				     (origin) ? origin : caller, truncate, over_selection,
				     gen, a);

	      sfs[i] = free_snd_fd(sfs[i]);
	      if ((errstr) || (ss->stopped_explicitly))
		{
		  stop_point = i;
		  break;
		}
	    }
	}
      if ((a) && (!ur_a)) free(a);
    }

  if (ss->stopped_explicitly)
    {
      /* clean up and undo all edits up to stop_point */
      string_to_minibuffer(sp, _("filter stopped"));
      ss->stopped_explicitly = false;
      for (i = 0; i <= stop_point; i++)
	{
	  cp = si->cps[i];
	  undo_edit(cp, 1);
	}
    }
  free_sync_state(sc);
  return(errstr);
}


void apply_filter(chan_info *ncp, int order, env *e,
		  const char *caller, const char *origin, bool over_selection, Float *ur_a, mus_any *gen, 
		  XEN edpos, int arg_pos, bool truncate)
{
  char *error;
  bool err_type; /* ignored in this context */
  error = apply_filter_or_error(ncp, order, e, caller, origin, over_selection, ur_a, gen, edpos, arg_pos, truncate, &err_type);
  if (error)
    {
      snd_error_without_format(error);
      free(error);
    }
}


static char *reverse_channel(chan_info *cp, snd_fd *sf, off_t beg, off_t dur, XEN edp, const char *caller, int arg_pos)
{
  snd_info *sp;
  peak_env_info *ep = NULL;
  file_info *hdr = NULL;
  int i, j, ofd = 0, datumb = 0, err = 0, edpos = 0;
  bool section = false, temp_file;
  off_t k;
  char *origin = NULL;
  mus_sample_t **data;
  mus_sample_t *idata;
  char *ofile = NULL;
  io_error_t io_err = IO_NO_ERROR;

  if ((beg < 0) || (dur <= 0)) return(NULL);
  if (!(editable_p(cp))) return(NULL);
  /* if last was reverse and start/end match, we could just copy preceding edlist entry, or undo/redo etc --
   *   how to tell that this is happening?
   */
  sp = cp->sound;
  edpos = to_c_edit_position(cp, edp, caller, arg_pos);

  if (dur > cp->edits[edpos]->samples) dur = cp->edits[edpos]->samples;
  if (dur > MAX_BUFFER_SIZE)
    {
      temp_file = true; 
      ofile = snd_tempnam();
      hdr = make_temp_header(ofile, SND_SRATE(sp), 1, dur, caller);
      ofd = open_temp_file(ofile, 1, hdr, &io_err);
      if (ofd == -1)
	{
	  if (ofile) free(ofile);
	  return(mus_format(_("%s %s temp file %s: %s\n"), 
			    (io_err != IO_NO_ERROR) ? io_error_name(io_err) : "can't open",
			    caller, ofile, 
			    snd_open_strerror()));
	}
      datumb = mus_bytes_per_sample(hdr->format);
    }
  else temp_file = false;

  if ((beg == 0) && (dur == cp->edits[edpos]->samples))
    ep = peak_env_copy(cp, true, edpos); /* true -> reversed */
  else 
    {
      int sbin, ebin;
      Float min1, max1;
      ep = peak_env_copy(cp, false, edpos);
      if (ep) 
	{
	  /* now reverse the selection */
	  sbin = (int)ceil(beg / ep->samps_per_bin);
	  ebin = (int)floor((beg + dur) / ep->samps_per_bin);
	  if (ebin > ep->peak_env_size) ebin = ep->peak_env_size;
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
	  if (ebin < ep->peak_env_size) pick_one_bin(ep, ebin, ep->samps_per_bin * ebin, cp, edpos);
	}
      section = true; /* only for reverse_marks below */
    }

  data = (mus_sample_t **)malloc(sizeof(mus_sample_t *));
  data[0] = (mus_sample_t *)calloc(MAX_BUFFER_SIZE, sizeof(mus_sample_t)); 
  idata = data[0];
#if HAVE_FORTH
  if (dur == cp->edits[edpos]->samples)
    origin = mus_format(OFF_TD PROC_SEP PROC_FALSE " %s", beg, S_reverse_channel);
  else origin = mus_format(OFF_TD PROC_SEP OFF_TD " %s", beg, dur, S_reverse_channel);
#else
  if (dur == cp->edits[edpos]->samples)
    origin = mus_format("%s" PROC_OPEN OFF_TD PROC_SEP PROC_FALSE, TO_PROC_NAME(S_reverse_channel), beg);
  else origin = mus_format("%s" PROC_OPEN OFF_TD PROC_SEP OFF_TD, TO_PROC_NAME(S_reverse_channel), beg, dur);
#endif

  if (temp_file)
    {
      j = 0;
      for (k = 0; k < dur; k++)
	{
	  idata[j++] = read_sample_to_mus_sample(sf);
	  if (j == MAX_BUFFER_SIZE)
	    {
	      err = mus_file_write(ofd, 0, j - 1, 1, data);
	      j = 0;
	      if (err == -1) break;
	    }
	}
      if (j > 0) mus_file_write(ofd, 0, j - 1, 1, data);
      close_temp_file(ofile, ofd, hdr->type, dur * datumb);
      hdr = free_file_info(hdr);
      file_change_samples(beg, dur, ofile, cp, 0, DELETE_ME, origin, edpos);
      if (ofile) 
	{
	  free(ofile); 
	  ofile = NULL;
	}
    }
  else 
    {
      for (k = 0; k < dur; k++)
	idata[k] = read_sample_to_mus_sample(sf);
      change_samples(beg, dur, idata, cp, origin, edpos);
    }
  if (ep) cp->edits[cp->edit_ctr]->peak_env = ep;
  reverse_marks(cp, (section) ? beg : -1, dur);
  update_graph(cp); 
  free(data[0]);
  free(data);
  if (origin) free(origin);
  return(NULL);
}


static void reverse_sound(chan_info *ncp, bool over_selection, XEN edpos, int arg_pos)
{
  sync_state *sc;
  sync_info *si;
  int i, stop_point = 0;
  snd_fd **sfs;
  char *caller;
  snd_info *sp;
  chan_info *cp;

  sp = ncp->sound;
  caller = (char *)((over_selection) ? S_reverse_selection : S_reverse_sound);
  sc = get_sync_state(sp, ncp, 0, over_selection, READ_BACKWARD, edpos, (const char *)caller, arg_pos);
  if (sc == NULL) return;
  si = sc->si;
  sfs = sc->sfs;

  if (!(ss->stopped_explicitly))
    {
      for (i = 0; i < si->chans; i++)
	{
	  char *errmsg = NULL;
	  off_t dur;
	  cp = si->cps[i];
	  sp = cp->sound;
	  if (over_selection)
	    dur = sc->dur;
	  else dur = to_c_edit_samples(cp, edpos, caller, arg_pos);
	  if (dur == 0) 
	    {
	      sfs[i] = free_snd_fd(sfs[i]); 
	      continue;
	    }

	  errmsg = reverse_channel(cp, sfs[i], si->begs[i], dur, edpos, caller, arg_pos);

	  sfs[i] = free_snd_fd(sfs[i]);
	  if (errmsg)
	    {
	      snd_error_without_format(errmsg);
	      free(errmsg);
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
      string_to_minibuffer(sp, _("reverse stopped"));
      ss->stopped_explicitly = false;
      for (i = 0; i <= stop_point; i++)
	{
	  cp = si->cps[i];
	  undo_edit(cp, 1);
	}
    }
  free_sync_state(sc);
}


static char *edit_list_envelope(mus_any *egen, off_t beg, off_t env_dur, off_t called_dur, off_t chan_dur, Float base)
{
  char *new_origin, *envstr;
  env *newe;

  newe = make_envelope_with_offset_and_scaler(mus_data(egen), mus_env_breakpoints(egen) * 2, mus_offset(egen), mus_scaler(egen));
  /* mus_env_offset|scaler are the fixed up versions, the originals are mus_offset|scaler.  mus_data is the original data */

  envstr = env_to_string(newe);
  if (((env_dur == chan_dur) || (env_dur == (chan_dur - 1))) &&
      (called_dur == chan_dur))
    {
#if HAVE_FORTH
      if (base == 1.0)
	new_origin = mus_format("%s " OFF_TD PROC_SEP PROC_FALSE " %s", envstr, beg, S_env_channel);
      else new_origin = mus_format("%s %.4f " OFF_TD PROC_SEP PROC_FALSE " %s", 
				   envstr, base, beg, S_env_channel_with_base);
#else
      if (base == 1.0)
	new_origin = mus_format("%s" PROC_OPEN "%s" PROC_SEP OFF_TD PROC_SEP PROC_FALSE, 
				TO_PROC_NAME(S_env_channel), envstr, beg);
      else new_origin = mus_format("%s" PROC_OPEN "%s" PROC_SEP "%.4f" PROC_SEP OFF_TD PROC_SEP PROC_FALSE, 
				   TO_PROC_NAME(S_env_channel_with_base), envstr, base, beg);
#endif
    }
  else 
    {
      /* env dur was apparently not chan dur, or called dur was not full sound? */
#if HAVE_FORTH
      new_origin = mus_format("%s :base %.4f :end " OFF_TD " %s " OFF_TD PROC_SEP OFF_TD " %s",
			      envstr, base, env_dur, S_make_env, beg, called_dur, S_env_channel);
#else
      new_origin = mus_format("%s" PROC_OPEN BPAREN "%s" PROC_OPEN "%s" PROC_SEP ":base" PROC_SEP "%.4f" PROC_SEP ":end" PROC_SEP OFF_TD ")" PROC_SEP OFF_TD PROC_SEP OFF_TD,
			      TO_PROC_NAME(S_env_channel), TO_PROC_NAME(S_make_env), envstr, base, env_dur, beg, called_dur);
#endif
    }
  if (envstr) free(envstr);
  free_env(newe);
  return(new_origin);
}


void apply_env(chan_info *cp, env *e, off_t beg, off_t dur, bool over_selection, 
	       const char *origin, mus_any *gen, XEN edpos, int arg_pos)
{
  /* basic cases: if env has 1 y value, use scale-channel,
   *              if step env (base == 0.0), use sequence of scale-channels,
   *              if not optimizable (via virtual edits), call mus_env on each sample
   *              if optimizable, use sequence of (x)ramp-channels
   */
  /* e can be NULL => use gen */
  snd_info *sp;
  sync_info *si;
  sync_state *sc = NULL;
  int i, j, err = 0, k, len;
  bool scalable = true, rampable = true, is_xramp = false;
  Float val[1];
  mus_any *egen;
  off_t *passes;
  double *rates;
  Float egen_val, base;
  double scaler, offset;
  char *new_origin;

  if ((!e) && (!gen)) return;
  if (over_selection) dur = selection_len();
  if (dur <= 0) return;
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
      if ((scalable) && 
	  (beg == 0))
	{
	  int pos;
	  pos = to_c_edit_position(cp, edpos, origin, arg_pos);
	  if ((cp->edit_ctr == pos) &&
	      (dur >= cp->edits[pos]->samples))
	    {
	      scale_by(cp, val, 1, over_selection);
	      return;
	    }
	}
    }
  else scalable = false;

  si = NULL;
  sp = cp->sound;
  if (scalable) /* only true if e (not gen) and all vals are equal and not full chan (latter case handled above) */
    {
      /* ---------------- use scale-channel ---------------- */
      sc = get_sync_state_without_snd_fds(sp, cp, beg, over_selection);
      if (sc == NULL) return;
      si = sc->si;
      for (i = 0; i < si->chans; i++) 
	{
	  if (over_selection)
	    scale_channel(si->cps[i], 
			  val[0], 
			  si->begs[i], 
			  selection_end(si->cps[i]) - si->begs[i] + 1, 
			  to_c_edit_position(si->cps[i], edpos, origin, arg_pos),
			  NOT_IN_AS_ONE_EDIT);
	  else scale_channel(si->cps[i], val[0], si->begs[i], dur, 
			     to_c_edit_position(si->cps[i], edpos, origin, arg_pos),
			     NOT_IN_AS_ONE_EDIT);
	}
      free_sync_state(sc);
      return;
    }

  if (e)
    egen = mus_make_env_with_length(e->data, e->pts, 1.0, 0.0, e->base, dur);
  else egen = gen;
  len = mus_env_breakpoints(egen);
  passes = mus_env_passes(egen);
  rates = mus_env_rates(egen);
  scaler = mus_env_scaler(egen); /* fixed-up versions if base != 1.0 */
  offset = mus_env_offset(egen);
  base = mus_increment(egen);

  if (base == 0.0) 
    {
      /* ---------------- step env -- handled as sequence of scalings ---------------- */
      int local_edpos, pos;
      off_t segbeg, segnum, segend;
      /* base == 0 originally, so it's a step env */
      sc = get_sync_state_without_snd_fds(sp, cp, beg, over_selection);
      if (sc == NULL) 
	{
	  if (e) mus_free(egen);
	  return;
	}
      si = sc->si;
      for (i = 0; i < si->chans; i++) 
	{
	  bool edited = false;
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
		  if (scale_channel(si->cps[i], (Float)(offset + scaler * rates[k]), segbeg, segnum, pos, IN_AS_ONE_EDIT))
		    edited = true;
		  pos = si->cps[i]->edit_ctr;
		}
	      segbeg += segnum;
	      if (segbeg >= segend) break;
	      segnum = passes[k + 1] - passes[k];
	    }
	  if (edited)
	    {
	      as_one_edit(si->cps[i], local_edpos + 1);
	      if (cp->edits[cp->edit_ctr]->origin) free(cp->edits[cp->edit_ctr]->origin);
	      cp->edits[cp->edit_ctr]->origin = edit_list_envelope(egen, si->begs[i], (len > 1) ? (passes[len - 2]) : dur, dur, CURRENT_SAMPLES(si->cps[i]), base);
	      after_edit(cp);
	      update_graph(si->cps[i]);
	      reflect_edit_history_change(si->cps[i]);
	    }
	}
      free_sync_state(sc);
      if (e) mus_free(egen);
      return;
    }

  /* step env, special env, and degenerate cases are out of the way */
  /* need to use the same sync/selection choice as will be used below! */
  sc = get_sync_state_without_snd_fds(sp, cp, beg, over_selection);
  si = sc->si;
  if (base != 1.0) is_xramp = true;
  for (i = 0; i < si->chans; i++)
    if (unrampable(si->cps[i], si->begs[i], dur, to_c_edit_position(si->cps[i], edpos, origin, arg_pos), is_xramp))
      {
	rampable = false;
	break;
      }
  free_sync_state(sc);

  if (!rampable)
    {
      /* ---------------- not optimizable, so call mus_env on each sample ---------------- */
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
      sc = get_sync_state(sp, cp, beg, over_selection, READ_FORWARD, edpos, origin, arg_pos);
      if (sc == NULL) 
	{
	  if (e) mus_free(egen);
	  return;
	}
      si = sc->si;
      sfs = sc->sfs;
      if (dur > MAX_BUFFER_SIZE) /* if smaller than this, we don't gain anything by using a temp file (its buffers are this large) */
	{
	  io_error_t io_err = IO_NO_ERROR;
	  temp_file = true; 
	  ofile = snd_tempnam(); 
	  hdr = make_temp_header(ofile, SND_SRATE(sp), si->chans, dur, (char *)origin);
	  ofd = open_temp_file(ofile, si->chans, hdr, &io_err);
	  if (ofd == -1)
	    {
	      if (e) mus_free(egen);
	      for (i = 0; i < si->chans; i++) 
		sfs[i] = free_snd_fd(sfs[i]);
	      free_sync_state(sc);
	      if (e) mus_free(egen);
	      snd_error(_("%s %s temp file %s: %s\n"), 
			(io_err != IO_NO_ERROR) ? io_error_name(io_err) : "can't open",
			origin, ofile, 
			snd_open_strerror());
	      free(ofile);
	      return;
	    }
	  datumb = mus_bytes_per_sample(hdr->format);
	}
      else temp_file = false;
      data = (mus_sample_t **)malloc(si->chans * sizeof(mus_sample_t *));
      for (i = 0; i < si->chans; i++) 
	{
	  if (temp_file)
	    data[i] = (mus_sample_t *)calloc(MAX_BUFFER_SIZE, sizeof(mus_sample_t)); 
	  else data[i] = (mus_sample_t *)calloc(dur, sizeof(mus_sample_t)); 
	}
      j = 0;
      reporting = ((dur > REPORTING_SIZE) && (!(cp->squelch_update)));
      if (reporting) start_progress_report(cp);
      if (si->chans > 1)
	{
	  ss->stopped_explicitly = false;
	  if (temp_file)
	    {
	      for (ioff = 0; ioff < dur; ioff++)
		{
		  egen_val = mus_env(egen);
		  for (k = 0; k < si->chans; k++)
		    data[k][j] = MUS_FLOAT_TO_SAMPLE(read_sample(sfs[k]) * egen_val);
		  j++;
		  if ((temp_file) && (j == MAX_BUFFER_SIZE))
		    {
		      if (reporting)
			{
			  progress_report(cp, (Float)((double)ioff / ((double)dur)));
			  if (ss->stopped_explicitly) break;
			  if (!(sp->active))
			    {
			      ss->stopped_explicitly = true;
			      break;
			    }
			}
		      err = mus_file_write(ofd, 0, j - 1, si->chans, data);
		      j = 0;
		      if (err == -1) break;
		    }
		}
	    }
	  else
	    {
	      for (j = 0; j < dur; j++)
		{
		  egen_val = mus_env(egen);
		  for (k = 0; k < si->chans; k++)
		    data[k][j] = MUS_FLOAT_TO_SAMPLE(read_sample(sfs[k]) * egen_val);
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
		  idata[j] = MUS_FLOAT_TO_SAMPLE(read_sample(sf) * mus_env(egen));
		  j++;
		  if ((temp_file) && (j == MAX_BUFFER_SIZE))
		    {
		      if (reporting)
			{
			  progress_report(cp, (Float)((double)ioff / ((double)dur)));
			  if (ss->stopped_explicitly) break;
			  if (!(sp->active))
			    {
			      ss->stopped_explicitly = true;
			      break;
			    }
			}
		      err = mus_file_write(ofd, 0, j - 1, 1, data);
		      j = 0;
		      if (err == -1) break;
		    }
		}
	    }
	  else
	    {
	      for (j = 0; j < dur; j++)
		idata[j] = MUS_FLOAT_TO_SAMPLE(read_sample(sf) * mus_env(egen));
	    }
	}
      if (temp_file)
	{
	  if (j > 0) mus_file_write(ofd, 0, j - 1, si->chans, data);
	  close_temp_file(ofile, ofd, hdr->type, dur * si->chans * datumb);
	  free_file_info(hdr);
	}
      if (reporting) finish_progress_report(cp);
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
	      int pos;
	      pos = to_c_edit_position(si->cps[i], edpos, origin, arg_pos);
	      new_origin = edit_list_envelope(egen, si->begs[i], (len > 1) ? (passes[len - 2]) : dur, dur, CURRENT_SAMPLES(si->cps[i]), base);
	      if (temp_file)
		{
		  file_change_samples(si->begs[i], dur, ofile, si->cps[i], i, 
				      (si->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME, 
				      new_origin, pos);
		  if ((si->begs[i] == 0) && (dur == si->cps[i]->edits[pos]->samples))
		    amp_env_env(si->cps[i], mus_data(egen), len, pos, base, scaler, offset);
		  else 
		    {
		      if ((len < 2) || (snd_abs_off_t(dur - passes[len - 2]) < 2))
			amp_env_env_selection_by(si->cps[i], egen, si->begs[i], dur, pos);
		    }

		}
	      else change_samples(si->begs[i], dur, data[i], si->cps[i], new_origin, pos);
	      free(new_origin);
	      update_graph(si->cps[i]);
	    }
	}
      for (i = 0; i < si->chans; i++)
	{
	  sfs[i] = free_snd_fd(sfs[i]);
	  free(data[i]);
	}
      if ((temp_file) && (ofile)) {free(ofile); ofile = NULL;}
      if (data) free(data);
    }
  else 
    {
      /* ---------------- optimizable -- treat env as a sequence of virtual (x)ramps and scalings (if slope=0) ---------------- */
      int local_edpos, m, pos, env_pos;
      bool need_xramp = false;
      off_t segbeg, segnum, segend;
      double power = 0.0;
      Float *data;

      data = mus_data(egen);
      if (base != 1.0) need_xramp = true;
      sc = get_sync_state_without_snd_fds(sp, cp, beg, over_selection);
      if (sc == NULL) 
	{
	  if (e) mus_free(egen);
	  return;
	}
      si = sc->si;
      /* in snd-test.scm, one sync_state pointer is lost here because env-channel requests edpos 2 (or is it 123?), but only 1 exists */

      for (i = 0; i < si->chans; i++) 
	{
	  bool edited = false;
	  if (!(editable_p(si->cps[i]))) continue;
	  segbeg = si->begs[i];
	  segend = si->begs[i] + dur;
	  segnum = passes[0];
	  local_edpos = si->cps[i]->edit_ctr; /* for as_one_edit backup */
	  pos = to_c_edit_position(si->cps[i], edpos, origin, arg_pos);
	  env_pos = pos;
	  for (k = 0, m = 1; k < len; k++, m += 2)
	    {
	      bool applied_ramp = false;

	      if ((segbeg + segnum) > segend) 
		segnum = segend - segbeg;
	      else
		if ((k >= (len - 2)) && 
		    ((segbeg + segnum) < segend))
		  segnum = segend - segbeg; /* last value is sticky in envs */

	      if (segnum > 0)
		{
		  if (k == 0) 
		    {
		      if (need_xramp)
			{
			  power = mus_env_initial_power(egen);
			  applied_ramp = xramp_channel(si->cps[i], 
						       power,
						       rates[0],
						       scaler, offset, segbeg, segnum, pos, IN_AS_ONE_EDIT, egen, 0);
			  power *= exp(log(rates[0]) * segnum);
			}
		      else applied_ramp = ramp_channel(si->cps[i], 
						       offset + scaler * data[m],
						       rates[0],
						       segbeg, segnum, pos, IN_AS_ONE_EDIT);
		    }
		  else 
		    {
		      if (need_xramp)
			/* divide by segnum since we end at the break point and don't want to repeat it, so go to next position in env */
			{
			  applied_ramp = xramp_channel(si->cps[i], 
						       power, 
						       rates[k],
						       scaler, offset, segbeg, segnum, pos, IN_AS_ONE_EDIT, egen, k);
			  power *= exp(log(rates[k]) * segnum);
			}
		      else 
			{
			  if (k == (len - 1)) /* oops -- must have sticky end in play here? this doesn't work if a clm env passed */
			    applied_ramp = scale_channel(si->cps[i], 
							 (Float)(offset + scaler * data[m]), 
							 segbeg, segnum, pos, IN_AS_ONE_EDIT);
			  else applied_ramp = ramp_channel(si->cps[i], 
							   offset + scaler * data[m],
							   rates[k],
							   segbeg, segnum, pos, IN_AS_ONE_EDIT);
			}
		    }
		  pos = si->cps[i]->edit_ctr;
		}

	      if (!edited) edited = applied_ramp;

	      segbeg += segnum;
	      if (segbeg >= segend) break;
	      segnum = passes[k + 1] - passes[k];
	    }

	  if (edited)
	    {
	      if ((si->begs[i] == 0) && (dur == si->cps[i]->edits[env_pos]->samples))
		amp_env_env(si->cps[i], mus_data(egen), len, env_pos, base, scaler, offset);
	      else 
		{
		  if ((len < 2) || (snd_abs_off_t(dur - passes[len - 2]) < 2))
		    amp_env_env_selection_by(si->cps[i], egen, si->begs[i], dur, env_pos);
		}

	      as_one_edit(si->cps[i], local_edpos + 1);
	      if (si->cps[i]->edits[si->cps[i]->edit_ctr]->origin) 
		free(si->cps[i]->edits[si->cps[i]->edit_ctr]->origin);
	      si->cps[i]->edits[si->cps[i]->edit_ctr]->origin = edit_list_envelope(egen, 
										   si->begs[i], (len > 1) ? (passes[len - 2]) : dur, 
										   dur, 
										   CURRENT_SAMPLES(si->cps[i]), 
										   base);
	      after_edit(cp);
	      update_graph(si->cps[i]);
	      reflect_edit_history_change(si->cps[i]);
	    }
	}
    }
  if (e) mus_free(egen);
  free_sync_state(sc);
}


void cursor_delete(chan_info *cp, off_t count)
{
  off_t beg;
  snd_info *sp;
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
      int i;
      sync_info *si;
      chan_info **cps;
      si = snd_sync(sp->sync);
      cps = si->cps;
      for (i = 0; i < si->chans; i++)
	{
	  if (delete_samples(beg, count, cps[i], cps[i]->edit_ctr))
	    {
	      CURSOR(cps[i]) = beg;
	      update_graph(si->cps[i]);
	    }
	}
      si = free_sync_info(si);
    }
  else
    {
      if (delete_samples(beg, count, cp, cp->edit_ctr))
	{
	  CURSOR(cp) = beg;
	  update_graph(cp);
	}
    }
}


void cursor_insert(chan_info *cp, off_t beg, off_t count)
{
  snd_info *sp;
  sp = cp->sound;
  if (count < 0) 
    {
      count = -count;
      if (count > beg) count = beg;
      beg -= count;
    }
  if (sp->sync != 0)
    {
      int i;
      sync_info *si;
      chan_info **cps;
      si = snd_sync(sp->sync);
      cps = si->cps;
      for (i = 0; i < si->chans; i++)
	{
	  if ((count > 0) &&
	      (extend_with_zeros(cps[i], 
				 mus_oclamp(0, beg, CURRENT_SAMPLES(si->cps[i])), 
				 count, 
				 cps[i]->edit_ctr,
				 "cursor insert")))
	    update_graph(cps[i]);
	}
      si = free_sync_info(si);
    }
  else 
    {
      if ((count > 0) &&
	  (extend_with_zeros(cp, 
			     mus_oclamp(0, beg, CURRENT_SAMPLES(cp)), 
			     count, 
			     cp->edit_ctr,
			     "cursor insert")))
	update_graph(cp);
    }
}


void cursor_zeros(chan_info *cp, off_t count, bool over_selection)
{
  int i;
  off_t beg, num;
  snd_info *sp;
  sync_info *si = NULL;
  chan_info *ncp;

  if (count == 0) return;
  if (count < 0) num = -count; else num = count;
  sp = cp->sound;

  if ((sp->sync != 0) && (!over_selection))
    {
      si = snd_sync(sp->sync);
      for (i = 0; i < si->chans; i++) 
	si->begs[i] = CURSOR(cp);
    }
  else
    {
      if ((over_selection) && (selection_is_active()))
	{
	  si = selection_sync();
	  num = selection_len();
	}
    }

  if (!si) si = make_simple_sync(cp, CURSOR(cp));

  for (i = 0; i < si->chans; i++)
    {
      /* if zeroing entire sound, set scalers and remake peak_env */
      ncp = si->cps[i];
      if ((si->begs[i] == 0) && 
	  (num >= CURRENT_SAMPLES(ncp)))
	{
	  Float scaler[1];
	  snd_info *nsp;
	  int old_sync;
	  nsp = ncp->sound;
	  old_sync = nsp->sync;
	  nsp->sync = 0;
	  scaler[0] = 0.0;

	  scale_by(ncp, scaler, 1, OVER_SOUND);

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
	    scale_channel(ncp, 0.0, beg, num, ncp->edit_ctr, NOT_IN_AS_ONE_EDIT);
	}
    }
  si = free_sync_info(si);
}


/* smooth-channel could be a built-in virtual op, but the smoothed section is never long, so it doesn't save anything */

static void smooth_channel(chan_info *cp, off_t beg, off_t dur, int edpos)
{
  mus_sample_t *data = NULL;
  off_t k;
  char *origin = NULL;
  Float y0, y1, angle, incr, off, scale;
  if ((beg < 0) || (dur <= 0)) return;
  if (!(editable_p(cp))) return;
  if ((beg + dur) > cp->edits[edpos]->samples) 
    {
      dur = cp->edits[edpos]->samples - beg;
      if (dur <= 0) return;
    }
  y0 = chn_sample(beg, cp, edpos);
  y1 = chn_sample(beg + dur, cp, edpos); /* one past end -- this is a debatable choice */
  if (y1 > y0) angle = M_PI; else angle = 0.0;
  incr = M_PI / (double)dur;
  off = 0.5 * (y1 + y0);
  scale = 0.5 * fabs(y0 - y1);
  data = (mus_sample_t *)calloc(dur, sizeof(mus_sample_t));
  for (k = 0; k < dur; k++, angle += incr) 
    data[k] = MUS_FLOAT_TO_SAMPLE(off + scale * cos(angle));
#if HAVE_FORTH
  origin = mus_format(OFF_TD PROC_SEP OFF_TD " %s", beg, dur, S_smooth_channel);
#else
  origin = mus_format("%s" PROC_OPEN OFF_TD PROC_SEP OFF_TD, TO_PROC_NAME(S_smooth_channel), beg, dur);
#endif
  change_samples(beg, dur, data, cp, origin, edpos);
  if (origin) free(origin);
  update_graph(cp);
  free(data);
}


void cos_smooth(chan_info *cp, off_t beg, off_t num, bool over_selection)
{
  /* verbatim, so to speak from Dpysnd */
  /* start at beg, apply a cosine for num samples, matching endpoints */
  sync_state *sc;
  int i;
  snd_info *sp;
  sync_info *si;

  sp = cp->sound;
  sc = get_sync_state_without_snd_fds(sp, cp, beg, over_selection);
  if (sc == NULL) return;
  si = sc->si;
  if (over_selection) num = sc->dur;

  for (i = 0; i < si->chans; i++)
    smooth_channel(si->cps[i], si->begs[i], num, si->cps[i]->edit_ctr);

  free_sync_state(sc);
}


static char *run_channel(chan_info *cp, struct ptree *pt, off_t beg, off_t dur, int edpos, const char *origin, const char *caller)
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
      return(mus_format(_("%s: can't read %s[%d] channel data!"), caller, sp->short_filename, cp->chan));
    }

  if (dur > MAX_BUFFER_SIZE)
    {
      io_error_t io_err = IO_NO_ERROR;
      temp_file = true; 
      ofile = snd_tempnam();
      hdr = make_temp_header(ofile, SND_SRATE(sp), 1, dur, "run_channel temp");
      ofd = open_temp_file(ofile, 1, hdr, &io_err);
      if (ofd == -1)
	{
	  free_snd_fd(sf); 
	  return(mus_format(_("%s %s temp file %s: %s\n"), 
			    (io_err != IO_NO_ERROR) ? io_error_name(io_err) : "can't open",
			    caller, ofile, 
			    snd_open_strerror()));
	}
      datumb = mus_bytes_per_sample(hdr->format);
    }
  else temp_file = false;

  data = (mus_sample_t **)malloc(sizeof(mus_sample_t *));
  data[0] = (mus_sample_t *)calloc(MAX_BUFFER_SIZE, sizeof(mus_sample_t)); 
  idata = data[0];

  if (temp_file)
    {
      j = 0;
      ss->stopped_explicitly = false;
      for (k = 0; k < dur; k++)
	{
	  idata[j++] = MUS_FLOAT_TO_SAMPLE(mus_run_evaluate_ptree_1f2f(pt, read_sample(sf)));
	  if (j == MAX_BUFFER_SIZE)
	    {
	      err = mus_file_write(ofd, 0, j - 1, 1, data);
	      j = 0;
	      if (err == -1) break;
	      check_for_event();
	      if (ss->stopped_explicitly) break;
	    }
	}
      if (j > 0) mus_file_write(ofd, 0, j - 1, 1, data);
      close_temp_file(ofile, ofd, hdr->type, dur * datumb);
      hdr = free_file_info(hdr);
      if (err != -1)
	file_change_samples(beg, dur, ofile, cp, 0, DELETE_ME, origin, edpos);
      if (ofile) 
	{
	  free(ofile); 
	  ofile = NULL;
	}
    }
  else 
    {
      if (dur > 0) 
	{
	  for (k = 0; k < dur; k++)
	    idata[k] = MUS_FLOAT_TO_SAMPLE(mus_run_evaluate_ptree_1f2f(pt, read_sample(sf)));
	  change_samples(beg, dur, idata, cp, origin, edpos);
	}
    }

  update_graph(cp); 
  free_snd_fd(sf);
  free(data[0]);
  free(data);
  return(NULL);
}


typedef struct {
  snd_fd **fds;
  int len;
} scale_and_src_data;


static Float scale_and_src_input(void *data, int direction)
{
  scale_and_src_data *sd = (scale_and_src_data *)data;
  int i;
  Float sum;
  sum = 0.0;
  for (i = 0; i < sd->len; i++)
    if (sd->fds[i])
      sum += read_sample(sd->fds[i]);
  return(sum);
}


char *scale_and_src(char **files, int len, int max_chans, Float amp, Float speed, env *amp_env, bool *temp_file_err)
{
  /* view files mix and insert possible src change */
  char *tempfile;
  snd_fd ***fds = NULL;
  snd_info **sps = NULL;
  int i, chan, chans = 0;
  off_t k, new_dur = 0, dur = 0;
  mus_sample_t **data;
  file_info *hdr = NULL;
  int j, ofd = 0, datumb = 0, err = 0, srate = 0;
  io_error_t io_err = IO_NO_ERROR;
  Float sum;
  mus_any *e = NULL;
  mus_any **sgens = NULL;
  scale_and_src_data **sdata = NULL;

  (*temp_file_err) = false;
  tempfile = snd_tempnam();

  for (i = 0; i < len; i++)
    {
      int fchans, fsrate;
      off_t flen;
      fchans = mus_sound_chans(files[i]);
      flen = mus_sound_frames(files[i]);
      fsrate = mus_sound_srate(files[i]);
      if (chans < fchans) chans = fchans;
      if (srate < fsrate) srate = fsrate;
      if (dur < flen) dur = flen;
    }

  /* open output sound file */
  hdr = make_temp_header(tempfile, srate, chans, dur, "scale-and-src temp");
  ofd = open_temp_file(tempfile, chans, hdr, &io_err);
  if (ofd == -1)
    {
      (*temp_file_err) = true;
      free_file_info(hdr);
      free(tempfile);
      return(mus_format(_("%s temp file %s: %s\n"), 
			(io_err != IO_NO_ERROR) ? io_error_name(io_err) : "can't open",
			tempfile, 
			snd_open_strerror()));
    }

  fds = (snd_fd ***)calloc(len, sizeof(snd_fd **));
  sps = (snd_info **)calloc(len, sizeof(snd_info *));
  for (i = 0; i < len; i++)
    {
      fds[i] = (snd_fd **)calloc(max_chans, sizeof(snd_fd *));
      sps[i] = make_sound_readable(files[i], false);
      sps[i]->short_filename = filename_without_directory(files[i]);
      sps[i]->filename = NULL; /* why? squelch graphics perhaps? */
      for (chan = 0; chan < sps[i]->nchans; chan++)
	fds[i][chan] = init_sample_read(0, sps[i]->chans[chan], READ_FORWARD);
    }

  /* now we have readers set up for all chans of all sounds about to be mixed/scaled/enveloped/resampled... */

  datumb = mus_bytes_per_sample(hdr->format);
  data = (mus_sample_t **)calloc(chans, sizeof(mus_sample_t *));
  for (i = 0; i < chans; i++)
    data[i] = (mus_sample_t *)calloc(MAX_BUFFER_SIZE, sizeof(mus_sample_t)); 

  if (!(snd_feq(speed, 1.0)))
    {
      new_dur = (off_t)((double)dur / (double)speed);
      sgens = (mus_any **)calloc(chans, sizeof(mus_any *));
      sdata = (scale_and_src_data **)calloc(chans, sizeof(scale_and_src_data *));
      for (chan = 0; chan < chans; chan++)
	{
	  int m;
	  sdata[chan] = (scale_and_src_data *)calloc(1, sizeof(scale_and_src_data));
	  sdata[chan]->len = len;
	  sdata[chan]->fds = (snd_fd **)calloc(len, sizeof(snd_fd *));
	  for (m = 0; m < len; m++)
	    sdata[chan]->fds[m] = fds[m][chan];

	  sgens[chan] = mus_make_src(scale_and_src_input, speed, 0, (void *)(sdata[chan])); /* width=0 -> use current default */
	}
    }
  else  new_dur = dur;

  if (!(default_env_p(amp_env)))
    e = mus_make_env_with_length(amp_env->data, amp_env->pts, amp, 0.0, 1.0, new_dur);

  j = 0;
  if (!sgens)
    {
      for (k = 0; k < dur; k++)
	{
	  if (e) amp = mus_env(e);
	  for (chan = 0; chan < chans; chan++)
	    {
	      sum = 0.0;
	      for (i = 0; i < len; i++)
		if (fds[i][chan])
		  sum += read_sample(fds[i][chan]);
	      sum *= amp;
	      data[chan][j] = MUS_FLOAT_TO_SAMPLE(sum);
	    }
	  j++;
	  if (j == MAX_BUFFER_SIZE)
	    {
	      err = mus_file_write(ofd, 0, j - 1, chans, data);
	      j = 0;
	      if (err == -1) break;
	    }
	}
    }
  else
    {
      for (k = 0; k < new_dur; k++)
	{
	  if (e) amp = mus_env(e);
	  for (chan = 0; chan < chans; chan++)
	    data[chan][j] = MUS_FLOAT_TO_SAMPLE(amp * mus_src(sgens[chan], 0.0, &scale_and_src_input));
	  j++;
	  if (j == MAX_BUFFER_SIZE)
	    {
	      err = mus_file_write(ofd, 0, j - 1, chans, data);
	      j = 0;
	      if (err == -1) break;
	    }
	}
    }

  if (j > 0) 
    mus_file_write(ofd, 0, j - 1, chans, data);

  /* close and free everything */
  close_temp_file(tempfile, ofd, hdr->type, new_dur * datumb);
  hdr = free_file_info(hdr);
  if (e) mus_free(e);

  for (i = 0; i < len; i++)
    {
      for (chan = 0; chan < sps[i]->nchans; chan++)
	free_snd_fd(fds[i][chan]);
      free(fds[i]);
      sps[i] = completely_free_snd_info(sps[i]);
    }
  free(fds);
  free(sps);

  for (i = 0; i < chans; i++)
    free(data[i]);
  free(data);

  if (sgens)
    {
      for (chan = 0; chan < chans; chan++)
	{
	  free(sdata[chan]->fds);
	  free(sdata[chan]);
	  mus_free(sgens[chan]);
	}
      free(sdata);
      free(sgens);
    }

  return(tempfile);
}

#define MUS_OUTA_1(Frame, Val, Fd) ((*(Fd->core)->write_sample))(Fd, Frame, 0, Val)
/* avoids all the CLM error checking */


static XEN g_map_chan_1(XEN proc_and_list, XEN s_beg, XEN s_end, XEN org, XEN snd, XEN chn, XEN edpos, XEN s_dur, const char *fallback_caller) 
{ 
  chan_info *cp;
  const char *caller;
  off_t beg = 0, end = 0, dur = 0;
  off_t num;
  int rpt = 0, i, pos;
  bool temp_file = false, backup = false;
  XEN res = XEN_FALSE;
  XEN proc = XEN_FALSE;

#if WITH_RUN
#if (!HAVE_S7)
  if (XEN_LIST_P(proc_and_list))
    proc = XEN_CADR(proc_and_list);
  else proc = proc_and_list;
#else
  proc = proc_and_list;
#endif
#else
  proc = proc_and_list;
#endif

  if (XEN_STRING_P(org)) 
    caller = XEN_TO_C_STRING(org);
  else caller = fallback_caller;

  XEN_ASSERT_TYPE((XEN_PROCEDURE_P(proc)) || (mus_xen_p(proc)), proc, XEN_ARG_1, caller, "a procedure");
  ASSERT_SAMPLE_TYPE(caller, s_beg, XEN_ARG_2);
  ASSERT_SAMPLE_TYPE(caller, s_end, XEN_ARG_3);
  ASSERT_SAMPLE_TYPE(caller, s_dur, XEN_ARG_3);
  ASSERT_CHANNEL(caller, snd, chn, 5); 

  cp = get_cp(snd, chn, caller);
  if (!cp) return(XEN_FALSE);
  if (!(editable_p(cp))) return(XEN_FALSE);

  pos = to_c_edit_position(cp, edpos, caller, 7);
  beg = beg_to_sample(s_beg, caller);
  if (XEN_FALSE_P(s_dur))
    end = end_to_sample(s_end, cp, pos, caller);
  else dur = dur_to_samples(s_dur, beg, cp, pos, 8, caller);
  if (end == 0) 
    {
      if (dur != 0) 
	end = beg + dur - 1;
      else end = cp->edits[pos]->samples - 1;
    }
  num = end - beg + 1;
  if (num > 0)
    {
      snd_fd *sf = NULL;
      char *errmsg = NULL;
      snd_info *sp;

      errmsg = procedure_ok(proc, 1, caller, "", 1);
      if (errmsg)
	{
	  XEN errstr;
	  errstr = C_TO_XEN_STRING(errmsg);
	  free(errmsg);
	  return(snd_bad_arity_error(caller, errstr, proc));
	}

      /* added 27-Oct-06 -- can't see why map-channel should be that different from insert-samples et al */
      if (beg > cp->edits[pos]->samples)
	{
	  if (!(extend_with_zeros(cp, cp->edits[pos]->samples, beg - cp->edits[pos]->samples, pos, "extend for " S_map_channel))) 
	    return(XEN_FALSE);
	  backup = true;
	  pos = cp->edit_ctr;
	}

      if (optimization(ss) > 0)
	{
	  struct ptree *pt = NULL;
#if HAVE_S7
	  pt = mus_run_form_to_ptree_1_f(XEN_PROCEDURE_SOURCE(proc_and_list));
#else
	  pt = mus_run_form_to_ptree_1_f(proc_and_list);
#endif
	  if (pt)
	    {
	      char *err_str;
	      err_str = run_channel(cp, pt, beg, num, pos, caller, S_map_channel);
	      mus_run_free_ptree(pt);
	      if (err_str == NULL)
		return(XEN_ZERO);
	      else free(err_str); /* and fallback on normal evaluator */
	    }
	}

      sp = cp->sound;
      sf = init_sample_read_any(beg, cp, READ_FORWARD, pos);
      if (sf == NULL) 
	return(XEN_TRUE);

      temp_file = (num > MAX_BUFFER_SIZE);
      if (temp_file)
	{
	  mus_any *outgen = NULL;
	  int rpt4;
	  char *filename;
	  off_t j, kp;
	  bool reporting = false;

	  reporting = ((num > REPORTING_SIZE) && (!(cp->squelch_update)));
	  if (reporting) start_progress_report(cp);
	  rpt4 = MAX_BUFFER_SIZE / 4;

	  filename = snd_tempnam();
	  outgen = mus_make_sample_to_file_with_comment(filename, 1, MUS_OUT_FORMAT, MUS_NEXT, "map-channel temp");
	  j = 0;
	  ss->stopped_explicitly = false;
	  for (kp = 0; kp < num; kp++)
	    {
	      /* changed here to remove catch 24-Mar-02 */
	      res = XEN_CALL_1_NO_CATCH(proc, C_TO_XEN_DOUBLE((double)read_sample(sf)));
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
			  if (MUS_VCT_P(res))
			    {
			      vct *v;
			      v = XEN_TO_VCT(res);
			      for (i = 0; i < v->length; i++) 
				MUS_OUTA_1(j++, v->data[i], outgen);
			    }
			  else
			    {
			      if (outgen) mus_free(outgen);
			      sf = free_snd_fd(sf);
			      if (reporting) finish_progress_report(cp);
			      snd_remove(filename, REMOVE_FROM_CACHE);
			      free(filename);
			      XEN_ERROR(BAD_TYPE,
					XEN_LIST_3(C_TO_XEN_STRING(caller),
						   C_TO_XEN_STRING("result of procedure must be a (non-complex) number, boolean, or vct:"),
						   res));
			    }
			}
		    }
		}
	      if (reporting) 
		{
		  rpt++;
		  if (rpt > rpt4)
		    {
		      progress_report(cp, (Float)((double)kp / (double)num));
		      if (!(sp->active))
			{
			  ss->stopped_explicitly = true;
			  break;
			}
		      rpt = 0;		    
		    }
		}
	      if (ss->stopped_explicitly) break;
	    }
	  if (outgen) mus_free(outgen);
	  sf = free_snd_fd(sf);
	  if (reporting) finish_progress_report(cp);
	  if (ss->stopped_explicitly) 
	    ss->stopped_explicitly = false;
	  else
	    {
	      if (cp->active < CHANNEL_HAS_EDIT_LIST)
		{
		  snd_remove(filename, REMOVE_FROM_CACHE);
		  free(filename);
		  XEN_ERROR(NO_SUCH_CHANNEL,
			    XEN_LIST_2(C_TO_XEN_STRING(caller),
				       C_TO_XEN_STRING("can't edit closed channel!")));
		  return(XEN_FALSE);
		}
	      if (j == num)
		file_change_samples(beg, j, filename, cp, 0, DELETE_ME, caller, pos);
	      else
		{
		  delete_samples(beg, num, cp, pos);
		  if (j > 0)
		    {
		      int cured;
		      cured = cp->edit_ctr;
		      file_insert_samples(beg, j, filename, cp, 0, DELETE_ME, caller, cp->edit_ctr);
		      backup_edit_list(cp);
		      if (cp->edit_ctr > cured)
			backup_edit_list(cp);
		      ripple_trailing_marks(cp, beg, num, j);
		    }
		  else snd_remove(filename, REMOVE_FROM_CACHE);
		}
	    }
	  free(filename);
	}
      else
	{
	  /* not temp_file -- use resizable buffer */
	  int data_pos = 0, kp;
	  off_t cur_size;
	  mus_sample_t *data = NULL;
	  data = (mus_sample_t *)calloc(num, sizeof(mus_sample_t));
	  cur_size = num;
	  for (kp = 0; kp < num; kp++)
	    {
	      res = XEN_CALL_1_NO_CATCH(proc, C_TO_XEN_DOUBLE((double)read_sample(sf)));
	      if (XEN_NUMBER_P(res))                         /* one number -> replace current sample */
		{
		  if (data_pos >= cur_size)
		    {
		      cur_size *= 2;
		      data = (mus_sample_t *)realloc(data, cur_size * sizeof(mus_sample_t));
		    }
		  data[data_pos++] = MUS_DOUBLE_TO_SAMPLE(XEN_TO_C_DOUBLE(res));
		}
	      else
		{
		  if (XEN_NOT_FALSE_P(res))                  /* if #f, no output on this pass */
		    {
		      if (XEN_TRUE_P(res))                   /* if #t we halt the entire map */
			break;
		      else
			{
			  if (MUS_VCT_P(res))
			    {
			      vct *v;
			      v = XEN_TO_VCT(res);
			      for (i = 0; i < v->length; i++)
				{
				  if (data_pos >= cur_size)
				    {
				      cur_size *= 2;
				      data = (mus_sample_t *)realloc(data, cur_size * sizeof(mus_sample_t));
				    }
				  data[data_pos++] = MUS_DOUBLE_TO_SAMPLE(v->data[i]);
				}
			    }
			  else
			    {
			      if (data) {free(data); data = NULL;}
			      sf = free_snd_fd(sf);
			      XEN_ERROR(BAD_TYPE,
					XEN_LIST_3(C_TO_XEN_STRING(caller),
						   C_TO_XEN_STRING("result of procedure must be a number, boolean, or vct:"),
						   res));
			    }
			}
		    }
		}
	    }
	  sf = free_snd_fd(sf);
	  if (cp->active < CHANNEL_HAS_EDIT_LIST)
	    {
	     if (data) {free(data); data = NULL;} 
	      XEN_ERROR(NO_SUCH_CHANNEL,
			XEN_LIST_2(C_TO_XEN_STRING(caller),
				   C_TO_XEN_STRING("can't edit closed channel!")));
	      return(XEN_FALSE);
	    }
	  if (data_pos == num)
	    change_samples(beg, data_pos, data, cp, caller, pos);
	  else
	    {
	      /* the version above truncates to the new length... */
	      delete_samples(beg, num, cp, pos);
	      if (data_pos > 0)
		{
		  int cured;
		  cured = cp->edit_ctr;
		  insert_samples(beg, data_pos, data, cp, caller, cp->edit_ctr);
		  backup_edit_list(cp);
		  if (cp->edit_ctr > cured)
		    backup_edit_list(cp);
		  ripple_trailing_marks(cp, beg, num, data_pos);
		}
	    }
	  if (data) {free(data); data = NULL;}
	}

      if (backup)
	backup_edit_list(cp);

      update_graph(cp);
    }
  return(xen_return_first(res, org));
}


static XEN g_map_chan_ptree_fallback(XEN proc, XEN init_func, chan_info *cp, off_t beg, off_t num, int pos, const char *origin)
{ 
  snd_fd *sf = NULL;
  bool temp_file;
  char *filename = NULL;
  off_t kp;
  int loc = NOT_A_GC_LOC;
  mus_sample_t *data = NULL;
  XEN res = XEN_FALSE, v = XEN_FALSE;

  if (!(editable_p(cp))) return(XEN_FALSE);

  sf = init_sample_read_any(beg, cp, READ_FORWARD, pos);
  if (sf == NULL) 
    return(XEN_TRUE);

  if (XEN_PROCEDURE_P(init_func))
    {
      if (XEN_REQUIRED_ARGS_OK(init_func, 3))
	v = XEN_CALL_3(init_func,
		       C_TO_XEN_OFF_T(0),
		       C_TO_XEN_OFF_T(num),
		       XEN_TRUE,             /* reading forward */
		       origin);
      else v = XEN_CALL_2(init_func,
			  C_TO_XEN_OFF_T(0),
			  C_TO_XEN_OFF_T(num),
			  origin);
      loc = snd_protect(v);
    }

  temp_file = (num > MAX_BUFFER_SIZE);
  if (temp_file)
    {
      mus_any *outgen = NULL;
      filename = snd_tempnam();
      outgen = mus_make_sample_to_file_with_comment(filename, 1, MUS_OUT_FORMAT, MUS_NEXT, origin);
      if (XEN_REQUIRED_ARGS_OK(proc, 3))
	{
	  for (kp = 0; kp < num; kp++)
	    {
	      res = XEN_CALL_3(proc, 
			       C_TO_XEN_DOUBLE((double)read_sample(sf)),
			       v,
			       XEN_TRUE,
			       origin);
	      MUS_OUTA_1(kp, XEN_TO_C_DOUBLE(res), outgen);
	    }
	}
      else
	{
	  for (kp = 0; kp < num; kp++)
	    {
	      res = XEN_CALL_1(proc, 
			       C_TO_XEN_DOUBLE((double)read_sample(sf)),
			       origin);
	      MUS_OUTA_1(kp, XEN_TO_C_DOUBLE(res), outgen);
	    }
	}
      if (outgen) mus_free(outgen);
    }
  else
    {
      data = (mus_sample_t *)calloc(num, sizeof(mus_sample_t)); 
      if (XEN_REQUIRED_ARGS_OK(proc, 3))
	{
	  for (kp = 0; kp < num; kp++)
	    {
	      res = XEN_CALL_3(proc, 
			       C_TO_XEN_DOUBLE((double)read_sample(sf)),
			       v,
			       XEN_TRUE,
			       origin);
	      data[kp] = MUS_DOUBLE_TO_SAMPLE(XEN_TO_C_DOUBLE(res));
	    }
	}
      else
	{
	  for (kp = 0; kp < num; kp++)
	    {
	      res = XEN_CALL_1(proc, 
			       C_TO_XEN_DOUBLE((double)read_sample(sf)),
			       origin);
	      data[kp] = MUS_DOUBLE_TO_SAMPLE(XEN_TO_C_DOUBLE(res));
	    }
	}
    }
  free_snd_fd(sf);
  if (temp_file)
    {
      file_change_samples(beg, num, filename, cp, 0, DELETE_ME, origin, pos);
      free(filename);
    }
  else 
    {
      change_samples(beg, num, data, cp, origin, pos);
      free(data);
    }
  if (loc != NOT_A_GC_LOC) snd_unprotect_at(loc);
  update_graph(cp); 
  return(proc);
}


static XEN g_ptree_channel(XEN proc_and_list, XEN s_beg, XEN s_dur, XEN snd, XEN chn, 
			   XEN edpos, XEN env_too, XEN init_func, XEN origin)
{
  #define H_ptree_channel "(" S_ptree_channel " proc :optional (beg 0) (dur len) snd chn edpos peak-env-also init-func origin): \
apply 'proc' as a 'virtual edit'; that is, the effect of 'proc' (a function of one argument, the \
current sample, if init-func is not specified), comes about as an implicit change in the way the data is read.  \
This is similar to scaling and some envelope operations in that no data actually changes.  If 'peak-env-also' is " PROC_TRUE ", \
the same function is applied to the peak env values to get the new version. \
If 'proc' needs some state, it can be supplied in a vct returned by 'init-func'. \
'init-func' is a function of 2 or 3 args, the current fragment-relative begin position, \
the overall fragment duration, and optionally the read direction. In this case, 'proc' is a function of 3 args: \
the current sample, the vct returned by 'init-func', and the current read direction."

  chan_info *cp;
  char *caller = NULL;
  off_t beg = 0, dur = 0;
  bool backup = false;
  int pos;
#if WITH_RUN
  bool too_many_ptrees = false;
  struct ptree *pt = NULL;
#endif
  XEN proc = XEN_FALSE;
  /* (ptree-channel (lambda (y) (* y 2))) -> ((lambda (y) (* y 2)) #<procedure #f ((y) (* y 2))>) as "proc_and_list" */
  /*   the cadr proc gives access to the environment, run walks the car */

#if WITH_RUN
#if (!HAVE_S7)
  if (XEN_LIST_P(proc_and_list))
    proc = XEN_CADR(proc_and_list);
  else proc = proc_and_list;
#else
  proc = proc_and_list;
#endif
#else
  proc = proc_and_list;
#endif

  XEN_ASSERT_TYPE((XEN_PROCEDURE_P(proc)) && ((XEN_REQUIRED_ARGS_OK(proc, 1)) || (XEN_REQUIRED_ARGS_OK(proc, 3))),
		  proc, XEN_ARG_1, S_ptree_channel, "a procedure of one or three args");
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(origin), origin, 10, S_ptree_channel, "a string");
  ASSERT_SAMPLE_TYPE(S_ptree_channel, s_beg, XEN_ARG_2);
  ASSERT_SAMPLE_TYPE(S_ptree_channel, s_dur, XEN_ARG_3);
  ASSERT_CHANNEL(S_ptree_channel, snd, chn, 4); 
  cp = get_cp(snd, chn, S_ptree_channel);
  if (!cp) return(XEN_FALSE);
  pos = to_c_edit_position(cp, edpos, S_ptree_channel, 6);
  if (pos > cp->edit_ctr)
    {
      XEN_ERROR(NO_SUCH_EDIT,
		XEN_LIST_3(C_TO_XEN_STRING(S_ptree_channel),
			   C_TO_XEN_STRING("edpos: ~A, ~A chan ~A has ~A edits"),
			   XEN_LIST_4(edpos,
				      C_TO_XEN_STRING(cp->sound->short_filename),
				      chn,
				      C_TO_XEN_INT(cp->edit_ctr))));
    }
  beg = beg_to_sample(s_beg, S_ptree_channel);
  dur = dur_to_samples(s_dur, beg, cp, pos, 3, S_ptree_channel);
  if (dur <= 0) return(XEN_FALSE);
  if ((beg + dur) > cp->edits[pos]->samples)
    {
      if (!(extend_with_zeros(cp, cp->edits[pos]->samples, beg + dur - cp->edits[pos]->samples, pos, "extend for " S_ptree_channel))) 
	return(XEN_FALSE);
      backup = true;
      pos = cp->edit_ctr;
    }

#if (!WITH_RUN)
  if (XEN_STRING_P(origin)) caller = mus_strdup(XEN_TO_C_STRING(origin)); else caller = mus_strdup(S_ptree_channel);
  g_map_chan_ptree_fallback(proc, init_func, cp, beg, dur, pos, caller);
  if (caller) {free(caller); caller = NULL;}
#else

  too_many_ptrees = unptreeable(cp, beg, dur, pos);

  if (XEN_PROCEDURE_P(init_func))
    {

      /* fprintf(stderr, "init: %s\n", XEN_AS_STRING(XEN_CAR(XEN_PROCEDURE_SOURCE(init_func)))); */

      if ((!(XEN_REQUIRED_ARGS_OK(init_func, 2))) &&
	  (!(XEN_REQUIRED_ARGS_OK(init_func, 3))))
	XEN_BAD_ARITY_ERROR(S_ptree_channel, 8, init_func, "init-func must take 2 or 3 args");
      if (!(XEN_REQUIRED_ARGS_OK(proc, 3)))
	XEN_BAD_ARITY_ERROR(S_ptree_channel, 1, proc, "main func must take 3 args if the init-func is present");
      if (XEN_STRING_P(origin)) caller = mus_strdup(XEN_TO_C_STRING(origin)); else caller = mus_strdup(S_ptree_channel);

      if (!too_many_ptrees)
	{
#if HAVE_S7
	  pt = mus_run_form_to_ptree_3_f(XEN_PROCEDURE_SOURCE(proc_and_list));
#else
	  pt = mus_run_form_to_ptree_3_f(proc_and_list);
#endif
	  if (pt)
	    {
	      ptree_channel(cp, pt, beg, dur, pos, XEN_TRUE_P(env_too), init_func, caller);
	      if (backup)
		backup_edit_list(cp);
	      if (caller) {free(caller); caller = NULL;}
	      return(proc_and_list);
	    }
	}

      /* fallback on map chan */
      g_map_chan_ptree_fallback(proc, init_func, cp, beg, dur, pos, caller);
      if (backup)
	backup_edit_list(cp);
      if (caller) {free(caller); caller = NULL;}
      return(proc_and_list);
    }

  /* no init-func from here on */

  if (XEN_STRING_P(origin)) caller = mus_strdup(XEN_TO_C_STRING(origin)); else caller = mus_strdup(S_ptree_channel);
  if (XEN_REQUIRED_ARGS_OK(proc, 1))
#if HAVE_S7
    pt = mus_run_form_to_ptree_1_f(XEN_PROCEDURE_SOURCE(proc_and_list));
#else
    pt = mus_run_form_to_ptree_1_f(proc_and_list);
#endif
  else
    {
      if ((!too_many_ptrees) && (XEN_REQUIRED_ARGS_OK(proc, 3)))
#if HAVE_S7
	pt = mus_run_form_to_ptree_3_f(XEN_PROCEDURE_SOURCE(proc_and_list));
#else
	pt = mus_run_form_to_ptree_3_f(proc_and_list); /* caller forgot init_func, but maybe it's ok anyway */
                                               /* (ptree-channel (lambda (y data dir) (* y 2))) */
#endif
    }
  if (pt)
    {
      if (too_many_ptrees)
	{
	  run_channel(cp, pt, beg, dur, pos, caller, S_ptree_channel);
	  mus_run_free_ptree(pt);
	  pt = NULL;
	}
      else ptree_channel(cp, pt, beg, dur, pos, XEN_TRUE_P(env_too), init_func, caller);
    }
  else g_map_chan_ptree_fallback(proc, init_func, cp, beg, dur, pos, caller);
  if (backup)
    backup_edit_list(cp);
  if (caller) {free(caller); caller = NULL;}
#endif
  return(proc_and_list);
}


#if HAVE_GUILE_DYNAMIC_WIND
typedef struct {
  XEN proc;
  off_t beg, num;
  snd_fd *sf;
  snd_info *sp;
  chan_info *cp;
  const char *caller;
  bool counting;
  bool reporting;
} scan_context;


static scan_context *make_scan_context(XEN p, off_t b, off_t n, snd_fd *f, snd_info *s, chan_info *cp, const char *orig, bool count)
{
  scan_context *sc;
  sc = (scan_context *)calloc(1, sizeof(scan_context));
  sc->proc = p;
  sc->beg = b;
  sc->num = n;
  sc->sf = f;
  sc->sp = s;
  sc->cp = cp;
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
  scan_context *sc = (scan_context *)context;
  sc->reporting = (sc->num > REPORTING_SIZE);
  rpt4 = MAX_BUFFER_SIZE / 4;
  if (sc->reporting) start_progress_report(sc->cp);
  ss->stopped_explicitly = false;
  for (kp = 0; kp < sc->num; kp++)
    {
      XEN res;
      res = XEN_CALL_1_NO_CATCH(sc->proc, C_TO_XEN_DOUBLE((double)read_sample(sc->sf)));
      /* in Forth, a return value of 0 is assumed to be false -- should we check for that? */
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
	      progress_report(sc->cp, (Float)((double)kp / (double)(sc->num)));
	      if (!(sc->sp->active))
		{
		  ss->stopped_explicitly = true;
		  break;
		}
	      rpt = 0;
	    }
	}
      if (ss->stopped_explicitly)
	{
	  ss->stopped_explicitly = false;
	  report_in_minibuffer(sc->sp, _("%s stopped at sample " OFF_TD), sc->caller, kp + sc->beg);
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
  sc->sf = free_snd_fd(sc->sf);
  if (sc->reporting) finish_progress_report(sc->cp);
  free(sc);
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
#if (!HAVE_GUILE_DYNAMIC_WIND)
  int rpt = 0, rpt4;
  bool reporting = false;
#endif
  int counts = 0, pos;
  char *errmsg;
  XEN proc = XEN_FALSE;
  struct ptree *pt = NULL;

#if WITH_RUN
#if (!HAVE_S7)
  if (XEN_LIST_P(proc_and_list))
    proc = XEN_CADR(proc_and_list);
  else proc = proc_and_list;
#else
  proc = proc_and_list;
#endif
#else
  proc = proc_and_list;
#endif

  XEN_ASSERT_TYPE((XEN_PROCEDURE_P(proc)), proc, XEN_ARG_1, caller, "a procedure");
  ASSERT_SAMPLE_TYPE(caller, s_beg, XEN_ARG_2);
  ASSERT_SAMPLE_TYPE(caller, s_end, XEN_ARG_3);
  ASSERT_SAMPLE_TYPE(caller, s_dur, XEN_ARG_3);
  ASSERT_CHANNEL(caller, snd, chn, 4);

  cp = get_cp(snd, chn, caller);
  if (!cp) return(XEN_FALSE);
  pos = to_c_edit_position(cp, edpos, caller, arg_pos);
  beg = beg_to_sample(s_beg, caller);
  if (beg > cp->edits[pos]->samples) return(XEN_FALSE);
  if (XEN_FALSE_P(s_dur))
    end = end_to_sample(s_end, cp, pos, caller);
  else dur = dur_to_samples(s_dur, beg, cp, pos, 3, caller);

  errmsg = procedure_ok(proc, 1, caller, "", 1);
  if (errmsg)
    {
      errstr = C_TO_XEN_STRING(errmsg);
      free(errmsg);
      return(snd_bad_arity_error(caller, errstr, proc));
    }

  sp = cp->sound;
  if (end == 0) 
    {
      if (dur != 0)
	end = beg + dur - 1;
      else end = cp->edits[pos]->samples - 1;
    }
  num = end - beg + 1;
  if (num <= 0) return(XEN_FALSE);
  sf = init_sample_read_any(beg, cp, READ_FORWARD, pos);
  if (sf == NULL) return(XEN_TRUE);

  if (optimization(ss) > 0)
    {
#if HAVE_S7
      pt = mus_run_form_to_ptree_1_b(XEN_PROCEDURE_SOURCE(proc_and_list));
#else
      pt = mus_run_form_to_ptree_1_b(proc_and_list);
#endif
      if (pt)
	{
	  for (kp = 0; kp < num; kp++)
	    if (mus_run_evaluate_ptree_1f2b(pt, read_sample(sf)))
	      {
		if (counting)
		  counts++;
		else
		  {
		    sf = free_snd_fd(sf);
		    mus_run_free_ptree(pt);
		    return(XEN_LIST_2(XEN_TRUE,
				      C_TO_XEN_OFF_T(kp + beg)));
		  }
	      }
	  sf = free_snd_fd(sf);
	  mus_run_free_ptree(pt);
	  if (counting)
	    return(C_TO_XEN_INT(counts));
	  return(XEN_FALSE);
	}
    }

#if HAVE_GUILE_DYNAMIC_WIND
  /* package up context and try to protect against errors/throws in the user's code */
  {
    scan_context *sc;
    sc = make_scan_context(proc, beg, num, sf, sp, cp, caller, counting);
    return(scm_internal_dynamic_wind((scm_t_guard)before_scan, 
				     (scm_t_inner)scan_body, 
				     (scm_t_guard)after_scan, 
				     (void *)sc,
				     (void *)sc));
  }
#else
  reporting = ((num > REPORTING_SIZE) && (!(cp->squelch_update)));
  if (reporting) start_progress_report(cp);
  rpt4 = MAX_BUFFER_SIZE / 4;
  ss->stopped_explicitly = false;
  for (kp = 0; kp < num; kp++)
    {
      XEN res;
      res = XEN_CALL_1_NO_CATCH(proc, C_TO_XEN_DOUBLE((double)read_sample(sf)));
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
		finish_progress_report(cp);
	      return(XEN_LIST_2(res,
				C_TO_XEN_OFF_T(kp + beg)));
	    }
	}
      if (reporting) 
	{
	  rpt++;
	  if (rpt > rpt4)
	    {
	      progress_report(cp, (Float)((double)kp / (double)num));
	      if (!(sp->active))
		{
		  ss->stopped_explicitly = true;
		  break;
		}
	      rpt = 0;
	    }
	}
      if (ss->stopped_explicitly)
	{
	  ss->stopped_explicitly = false;
	  report_in_minibuffer(sp, _("%s stopped at sample " OFF_TD), caller, kp + beg);
	  break;
	}
    }
  if (reporting) finish_progress_report(cp);
  sf = free_snd_fd(sf);
  if (counting)
    return(C_TO_XEN_INT(counts));
#endif
  return(XEN_FALSE);
}


static XEN g_scan_chan(XEN proc, XEN beg, XEN end, XEN snd, XEN chn, XEN edpos) 
{ 
  #if HAVE_SCHEME
    #define scan_chan_example "(scan-chan (lambda (y) (> y .1)))"
  #endif
  #if HAVE_RUBY
    #define scan_chan_example "scan_chan(lambda do |y| y > 0.1 end)"
  #endif
  #if HAVE_FORTH
    #define scan_chan_example "lambda: <{ y }> y 0.1 f> ; scan-chan"
  #endif

  #define H_scan_chan "(" S_scan_chan " func :optional (start 0) (end len) snd chn edpos): \
apply 'func' to samples in current channel (or the specified channel). \
'func' is a function of one argument, the current sample. \
if 'func' returns non-" PROC_FALSE ", the scan stops, and the value is returned to the caller with the sample number.\n  " scan_chan_example

  ASSERT_CHANNEL(S_scan_chan, snd, chn, 4); 
  return(g_sp_scan(proc, beg, end, snd, chn, S_scan_chan, false, edpos, 6, XEN_FALSE));
}


static XEN g_scan_channel(XEN proc, XEN beg, XEN dur, XEN snd, XEN chn, XEN edpos) 
{ 
  #if HAVE_SCHEME
    #define scan_channel_example "(scan-channel (lambda (y) (> y .1)))"
  #endif
  #if HAVE_RUBY
    #define scan_channel_example "scan_channel(lambda do |y| y > 0.1 end)"
  #endif
  #if HAVE_FORTH
    #define scan_channel_example "lambda: <{ y }> y 0.1 f> ; scan-channel"
  #endif

  #define H_scan_channel "(" S_scan_channel " func :optional (start 0) (dur len) snd chn edpos): \
apply func to samples in current channel (or the specified channel). \
func is a function of one argument, the current sample. \
if func returns non-" PROC_FALSE ", the scan stops, and the value is returned to the caller with the sample number. \n  " scan_channel_example

  ASSERT_CHANNEL(S_scan_channel, snd, chn, 4); 
  return(g_sp_scan(proc, beg, XEN_FALSE, snd, chn, S_scan_channel, false, edpos, 6, (XEN_BOUND_P(dur)) ? dur : XEN_FALSE));
}


static XEN g_map_chan(XEN proc, XEN s_beg, XEN s_end, XEN org, XEN snd, XEN chn, XEN edpos) 
{
  #if HAVE_SCHEME
    #define map_chan_example "(map-chan (lambda (y) (* y 2.0)))"
  #endif
  #if HAVE_RUBY
    #define map_chan_example "map_chan(lambda do |y| y * 2.0 end)"
  #endif
  #if HAVE_FORTH
    #define map_chan_example "lambda: <{ y }> y 2.0 f* ; map-chan"
  #endif

  #define H_map_chan "(" S_map_chan " func :optional (start 0) (end len) edname snd chn edpos): \
apply func to samples in current channel; edname is the edit history name for this editing operation.\n  " map_chan_example

  return(g_map_chan_1(proc, s_beg, s_end, org, snd, chn, edpos, XEN_FALSE, S_map_chan));
}


static XEN g_map_channel(XEN proc, XEN s_beg, XEN s_dur, XEN snd, XEN chn, XEN edpos, XEN org) 
{
  #if HAVE_SCHEME
    #define map_channel_example "(map-channel (lambda (y) (* y 2.0)))"
  #endif
  #if HAVE_RUBY
    #define map_channel_example "map_channel(lambda do |y| y * 2.0 end)"
  #endif
  #if HAVE_FORTH
    #define map_channel_example "lambda: <{ y }> y 2.0 f* ; map-channel"
  #endif

  #define H_map_channel "(" S_map_channel " func :optional (start 0) (dur len) snd chn edpos edname): \
apply func to samples in current channel; edname is the edit history name for this editing operation.\n  " map_channel_example

  return(g_map_chan_1(proc, s_beg, XEN_FALSE, org, snd, chn, edpos, (XEN_BOUND_P(s_dur)) ? s_dur : XEN_FALSE, S_map_channel));
}


static XEN g_find_channel(XEN expr, XEN sample, XEN snd_n, XEN chn_n, XEN edpos)
{
  #if HAVE_SCHEME
    #define find_channel_example "(find-channel (lambda (y) (> y .1)))"
  #endif
  #if HAVE_RUBY
    #define find_channel_example "find_channel(lambda do |y| y > 0.1 end)"
  #endif
  #if HAVE_FORTH
    #define find_channel_example "lambda: <{ y }> y 0.1 f> ; find-channel"
  #endif

  #define H_find_channel "(" S_find_channel " func :optional (start-samp 0) snd chn edpos): apply func, a function of one argument, \
the current sample, to each sample in snd's channel chn, starting at 'start-samp' until func returns something other than " PROC_FALSE ": \n  " find_channel_example

  /* no free here -- it's handled as ss->search_expr in snd-find.c */
  ASSERT_CHANNEL(S_find_channel, snd_n, chn_n, 3);
  return(g_sp_scan(expr, sample, XEN_FALSE, snd_n, chn_n, S_find_channel, false, edpos, 5, XEN_FALSE));
}


static XEN g_count_matches(XEN expr, XEN sample, XEN snd_n, XEN chn_n, XEN edpos)
{
  #if HAVE_SCHEME
    #define count_matches_example "(count-matches (lambda (y) (> y .1)))"
  #endif
  #if HAVE_RUBY
    #define count_matches_example "count_matches(lambda do |y| y > 0.1 end)"
  #endif
  #if HAVE_FORTH
    #define count_matches_example "lambda: <{ y }> y 0.1 f> ; count-matches"
  #endif

  #define H_count_matches "(" S_count_matches " func :optional (start-samp 0) snd chn edpos): return how many \
samples satisfy func (a function of one argument, the current sample, returning " PROC_TRUE " upon match):\n  " count_matches_example

  ASSERT_CHANNEL(S_count_matches, snd_n, chn_n, 3);
  return(g_sp_scan(expr, sample, XEN_FALSE, snd_n, chn_n, S_count_matches, true, edpos, 5, XEN_FALSE));
}


static XEN g_smooth_sound(XEN beg, XEN num, XEN snd_n, XEN chn_n)
{
  #define H_smooth_sound "(" S_smooth_sound " :optional (start-samp 0) (samps len) snd chn): smooth \
data from start-samp for samps in snd's channel chn"
  chan_info *cp;
  off_t start, samps;

  ASSERT_SAMPLE_TYPE(S_smooth_sound, beg, XEN_ARG_1);
  ASSERT_SAMPLE_TYPE(S_smooth_sound, num, XEN_ARG_2);
  ASSERT_CHANNEL(S_smooth_sound, snd_n, chn_n, 3);

  cp = get_cp(snd_n, chn_n, S_smooth_sound);
  if (!cp) return(XEN_FALSE);
  start = beg_to_sample(beg, S_smooth_sound);
  samps = dur_to_samples(num, start, cp, cp->edit_ctr, 2, S_smooth_sound);

  cos_smooth(cp, start, samps, OVER_SOUND); 

  return(beg);
}


static XEN g_smooth_channel(XEN beg, XEN dur, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_smooth_channel "(" S_smooth_channel " :optional (beg 0) (dur len) snd chn edpos): \
smooth data from beg for dur in snd's channel chn"
  chan_info *cp;
  off_t start, num;
  int pos;

  ASSERT_SAMPLE_TYPE(S_smooth_channel, beg, XEN_ARG_1);
  ASSERT_SAMPLE_TYPE(S_smooth_channel, dur, XEN_ARG_2);
  ASSERT_CHANNEL(S_smooth_channel, snd_n, chn_n, 3);

  cp = get_cp(snd_n, chn_n, S_smooth_channel);
  if (!cp) return(XEN_FALSE);
  pos = to_c_edit_position(cp, edpos, S_smooth_channel, 5);
  start = beg_to_sample(beg, S_smooth_channel);
  num = dur_to_samples(dur, start, cp, pos, 2, S_smooth_channel);

  if ((start < cp->edits[pos]->samples) &&
      (num > 0))
    smooth_channel(cp, start, num, pos);

  return(beg);
}


static XEN g_smooth_selection(void)
{
  #define H_smooth_selection "(" S_smooth_selection "): smooth the data in the currently selected portion"
  chan_info *cp;

  if (!(selection_is_active())) 
    return(snd_no_active_selection_error(S_smooth_selection));
  cp = get_cp(XEN_FALSE, XEN_FALSE, S_smooth_selection);
  if (!cp) return(XEN_FALSE);

  cos_smooth(cp, 0, 0, OVER_SELECTION);

  return(XEN_TRUE);
}


static XEN g_reverse_sound(XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_reverse_sound "(" S_reverse_sound " :optional snd chn edpos): reverse snd's channel chn"
  chan_info *cp;

  ASSERT_CHANNEL(S_reverse_sound, snd_n, chn_n, 1);
  cp = get_cp(snd_n, chn_n, S_reverse_sound);
  if (!cp) return(XEN_FALSE);

  reverse_sound(cp, OVER_SOUND, edpos, 3);

  return(XEN_FALSE);
}


static XEN g_reverse_selection(void)
{
  #define H_reverse_selection "(" S_reverse_selection "): reverse the data in the currently selected portion"
  chan_info *cp;

  if (!(selection_is_active())) 
    return(snd_no_active_selection_error(S_reverse_selection));

  cp = get_cp(XEN_FALSE, XEN_FALSE, S_reverse_selection);
  if (!cp) return(XEN_FALSE);

  reverse_sound(cp, OVER_SELECTION, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0);

  return(XEN_FALSE);
}


static XEN g_reverse_channel(XEN s_beg, XEN s_dur, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_reverse_channel "(" S_reverse_channel " :optional (beg 0) (dur len) snd chn edpos): reverse a portion of snd's channel chn"
  chan_info *cp;
  char *errmsg;
  off_t beg = 0, dur = 0, end;
  int pos;
  snd_fd *sf;

  ASSERT_SAMPLE_TYPE(S_reverse_channel, s_beg, XEN_ARG_1);
  ASSERT_SAMPLE_TYPE(S_reverse_channel, s_dur, XEN_ARG_2);
  ASSERT_CHANNEL(S_reverse_channel, snd_n, chn_n, 3);

  cp = get_cp(snd_n, chn_n, S_reverse_channel);
  if (!cp) return(XEN_FALSE);
  beg = beg_to_sample(s_beg, S_reverse_channel);
  pos = to_c_edit_position(cp, edpos, S_reverse_channel, 5);
  dur = dur_to_samples(s_dur, beg, cp, pos, 2, S_reverse_channel);
  if ((beg > cp->edits[pos]->samples) || (dur == 0)) return(XEN_FALSE);
  end = beg + dur;
  if (end > cp->edits[pos]->samples)
    end = cp->edits[pos]->samples;

  sf = init_sample_read_any(end - 1, cp, READ_BACKWARD, pos);
  errmsg = reverse_channel(cp, sf, beg, end - beg, edpos, S_reverse_channel, 5);

  free_snd_fd(sf);
  if (errmsg)
    {
      XEN str;
      str = C_TO_XEN_STRING(errmsg);
      free(errmsg);
      XEN_ERROR(XEN_ERROR_TYPE("IO-error"),
		XEN_LIST_2(C_TO_XEN_STRING(S_reverse_channel),
			   str));
    }
  return(s_beg);
}


static XEN g_insert_silence(XEN beg, XEN num, XEN snd, XEN chn)
{
  #define H_insert_silence "(" S_insert_silence " beg num :optional snd chn): insert num zeros at beg in snd's chn"
  chan_info *cp; /* follows sync */
  off_t start = 0, len = 0;

  XEN_ASSERT_TYPE(XEN_NUMBER_P(beg), beg, XEN_ARG_1, S_insert_silence, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(num), num, XEN_ARG_2, S_insert_silence, "a number");
  ASSERT_CHANNEL(S_insert_silence, snd, chn, 3);

  cp = get_cp(snd, chn, S_insert_silence);
  if (!cp) return(XEN_FALSE);
  start = XEN_TO_C_OFF_T(beg);
  if (start < 0) XEN_ERROR(NO_SUCH_SAMPLE,
			   XEN_LIST_2(C_TO_XEN_STRING(S_insert_silence),
				      beg));
  len = XEN_TO_C_OFF_T(num);
  if (len <= 0) return(XEN_FALSE);

  cursor_insert(cp, start, len);

  return(beg);
}


static XEN g_pad_channel(XEN beg, XEN num, XEN snd, XEN chn, XEN edpos)
{
  #define H_pad_channel "(" S_pad_channel " beg dur :optional snd chn edpos): insert dur zeros at beg in snd's chn"
  chan_info *cp;
  off_t bg, len;
  int pos;

  XEN_ASSERT_TYPE(XEN_NUMBER_P(beg), beg, XEN_ARG_1, S_pad_channel, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(num), num, XEN_ARG_2, S_pad_channel, "a number");
  ASSERT_CHANNEL(S_pad_channel, snd, chn, 3);

  cp = get_cp(snd, chn, S_pad_channel);
  if (!cp) return(XEN_FALSE);
  bg = beg_to_sample(beg, S_pad_channel);
  pos = to_c_edit_position(cp, edpos, S_pad_channel, 5);
  len = XEN_TO_C_OFF_T_OR_ELSE(num, cp->edits[pos]->samples - bg);

  if ((len > 0) &&
      (extend_with_zeros(cp, bg, len, pos, S_pad_channel)))
    update_graph(cp);
  return(beg);
}


static XEN g_swap_channels(XEN snd0, XEN chn0, XEN snd1, XEN chn1, XEN beg, XEN dur, XEN edpos0, XEN edpos1)
{
  #define H_swap_channels "(" S_swap_channels " :optional snd0 chn0 snd1 chn1 (beg 0) (dur len) edpos0 edpos1): \
swap the indicated channels"
  chan_info *cp0 = NULL, *cp1 = NULL;
  snd_info *sp = NULL;

  ASSERT_CHANNEL(S_swap_channels, snd0, chn0, 1);

  cp0 = get_cp(snd0, chn0, S_swap_channels);
  if (!cp0) return(XEN_FALSE);
  if (!(cp0->editable)) return(XEN_FALSE);

  if (XEN_INTEGER_P(chn1))
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
  if (!(cp1->editable)) return(XEN_FALSE);
  if ((cp0) && (cp1))
    {
      int pos0, pos1;
      off_t dur0, dur1, beg0 = 0, num;

      if (XEN_NUMBER_P(beg)) 
	beg0 = XEN_TO_C_OFF_T(beg);

      pos0 = to_c_edit_position(cp0, edpos0, S_swap_channels, 7);
      pos1 = to_c_edit_position(cp1, edpos1, S_swap_channels, 8);

      dur0 = cp0->edits[pos0]->samples;
      dur1 = cp1->edits[pos1]->samples;

      if (XEN_NUMBER_P(dur)) 
	num = XEN_TO_C_OFF_T(dur);
      else
	{
	  if (dur0 > dur1) 
	    num = dur0; 
	  else num = dur1; /* was min here 13-Dec-02 */
	}

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
		  peak_env_info *e0, *e1;
		  e0 = peak_env_copy(cp0, false, cp0->edit_ctr);
		  e1 = peak_env_copy(cp1, false, cp1->edit_ctr);
		  file_override_samples(dur1, cp1->sound->filename, cp0, cp1->chan, DONT_DELETE_ME, S_swap_channels);
		  file_override_samples(dur0, cp0->sound->filename, cp1, cp0->chan, DONT_DELETE_ME, S_swap_channels);
		  cp0->edits[cp0->edit_ctr]->peak_env = e1; /* can be NULL */
		  cp1->edits[cp1->edit_ctr]->peak_env = e0;
		  swap_marks(cp0, cp1);
		  update_graph(cp0);
		  update_graph(cp1);
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
  int len = 0, i;
  Float *scls;
  vct *v = NULL;
  if (XEN_NUMBER_P(scalers))
    len = 1;
  else
    {
      if (MUS_VCT_P(scalers))
	{
	  v = XEN_TO_VCT(scalers);
	  len = v->length;
	}
      else
	{
	  if (XEN_LIST_P(scalers))
	    {
	      len = XEN_LIST_LENGTH(scalers);
	      if (len == 0) 
		XEN_ERROR(NO_DATA,
			  XEN_LIST_3(C_TO_XEN_STRING(caller), 
				     C_TO_XEN_STRING("scalers list empty?"), 
				     scalers));
	    }
	  else XEN_WRONG_TYPE_ARG_ERROR(caller, 1, scalers, "a number, list, or vct");
	}
    }
  scls = (Float *)calloc(len, sizeof(Float));
  if (v)
    memcpy((void *)scls, (void *)(v->data), len * sizeof(Float));
  else
    {
      if (XEN_LIST_P(scalers))
	{
	  XEN lst;
	  for (i = 0, lst = XEN_COPY_ARG(scalers); i < len; i++, lst = XEN_CDR(lst)) 
	    scls[i] = (Float)XEN_TO_C_DOUBLE(XEN_CAR(lst));
	}
      else scls[0] = (Float)XEN_TO_C_DOUBLE(scalers);
    }
  result_len[0] = len;
  return(scls);
}


static XEN g_scale_to(XEN scalers, XEN snd_n, XEN chn_n)
{
  #define H_scale_to "(" S_scale_to " :optional (norms 1.0) snd chn): \
normalize snd to norms (following sync); norms can be a float or a vct/list of floats"

  /* chn_n irrelevant if sync */
  chan_info *cp;
  bool happy;
  int len[1];
  Float *scls;

  ASSERT_CHANNEL(S_scale_to, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_scale_to);
  if (!cp) return(XEN_FALSE);

  scls = load_Floats(scalers, len, S_scale_to);
  happy = scale_to(cp->sound, cp, scls, len[0], OVER_SOUND);

  free(scls);
  if (happy)
    return(scalers);
  return(XEN_FALSE);
}


static XEN g_scale_by(XEN scalers, XEN snd_n, XEN chn_n)
{
  #define H_scale_by "(" S_scale_by " scalers :optional snd chn): \
scale snd by scalers (following sync); scalers can be a float or a vct/list of floats"

  /* chn_n irrelevant if sync */
  chan_info *cp;
  int len[1];
  Float *scls;
  ASSERT_CHANNEL(S_scale_by, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_scale_by);
  if (!cp) return(XEN_FALSE);

  scls = load_Floats(scalers, len, S_scale_by);
  scale_by(cp, scls, len[0], OVER_SOUND);

  free(scls);
  return(scalers);
}


static XEN g_scale_selection_to(XEN scalers)
{
  #define H_scale_selection_to "(" S_scale_selection_to " norms): normalize selected portion to norms"
  if (selection_is_active())
    {
      int len[1];
      bool happy;
      Float *scls;

      scls = load_Floats(scalers, len, S_scale_selection_to);
      happy = scale_to(NULL, NULL, scls, len[0], OVER_SELECTION);

      free(scls);
      if (happy)
	return(scalers);
      return(XEN_FALSE);
    }
  return(snd_no_active_selection_error(S_scale_selection_to));
}


static XEN g_scale_selection_by(XEN scalers)
{
  #define H_scale_selection_by "(" S_scale_selection_by " scalers): scale selected portion by scalers"
  if (selection_is_active())
    {
      int len[1];
      Float *scls;

      scls = load_Floats(scalers, len, S_scale_selection_by);
      scale_by(NULL, scls, len[0], OVER_SELECTION);

      free(scls);
      return(scalers);
    }
  return(snd_no_active_selection_error(S_scale_selection_by));
}


static XEN g_clm_channel(XEN gen, XEN samp_n, XEN samps, XEN snd_n, XEN chn_n, XEN edpos, XEN overlap, XEN origin)
{
  #define H_clm_channel "(" S_clm_channel " gen :optional (beg 0) (dur len) snd chn edpos (overlap 0) origin): \
apply gen to snd's channel chn starting at beg for dur samples. overlap is the 'ring' time, if any."

  chan_info *cp;
  off_t beg = 0, dur = 0;
  int pos;
  mus_any *egen;
  char *errmsg = NULL, *caller = NULL;

  ASSERT_SAMPLE_TYPE(S_clm_channel, samp_n, XEN_ARG_2);
  ASSERT_SAMPLE_TYPE(S_clm_channel, samps, XEN_ARG_3);
  XEN_ASSERT_TYPE(XEN_NUMBER_P(overlap) || XEN_FALSE_P(overlap) || XEN_NOT_BOUND_P(overlap), overlap, XEN_ARG_7, S_clm_channel, "a number or " PROC_FALSE);
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(origin), origin, XEN_ARG_8, S_clm_channel, "a string");
  ASSERT_CHANNEL(S_clm_channel, snd_n, chn_n, 4);

  cp = get_cp(snd_n, chn_n, S_clm_channel);
  if (!cp) return(XEN_FALSE);
  pos = to_c_edit_position(cp, edpos, S_clm_channel, XEN_ARG_6);
  beg = beg_to_sample(samp_n, S_clm_channel);
  dur = dur_to_samples(samps, beg, cp, pos, XEN_ARG_3, S_clm_channel);
  if (dur == 0) return(XEN_FALSE);
  XEN_ASSERT_TYPE(mus_xen_p(gen), gen, XEN_ARG_1, S_clm_channel, "a clm generator");
  egen = XEN_TO_MUS_ANY(gen);
  if (XEN_STRING_P(origin)) caller = mus_strdup(XEN_TO_C_STRING(origin)); else caller = mus_strdup(S_clm_channel);

  errmsg = clm_channel(cp, egen, beg, dur, pos, XEN_TO_C_OFF_T_OR_ELSE(overlap, 0), caller);

  free(caller);
  if (errmsg)
    {
      XEN str;
      str = C_TO_XEN_STRING(errmsg);
      free(errmsg);
      XEN_ERROR(XEN_ERROR_TYPE("IO-error"),
		XEN_LIST_2(C_TO_XEN_STRING(S_clm_channel),
			   str));
    }
  return(gen);
}


static XEN g_env_1(XEN edata, off_t beg, off_t dur, XEN ebase, chan_info *cp, XEN edpos, const char *caller, bool over_selection)
{
  if (XEN_LIST_P(edata))
    {
      env *e;
      e = get_env(edata, caller);
      if (e)
	{
	  if (XEN_NUMBER_P(ebase))
	    {
	      /* env 'e' is a temp here, so we can clobber its base, etc */
	      e->base = XEN_TO_C_DOUBLE_OR_ELSE(ebase, 1.0);
	      if (e->base < 0.0)
		{
		  free_env(e);
		  XEN_OUT_OF_RANGE_ERROR(caller, 4, ebase, "base ~A < 0.0?");
		}
	    }
	  apply_env(cp, e, beg, dur, over_selection, caller, NULL, edpos, 7);
	  free_env(e);
	  return(edata);
	}
    }
  else
    {
      mus_any *egen = NULL;
      XEN_ASSERT_TYPE((mus_xen_p(edata)) && (mus_env_p(egen = XEN_TO_MUS_ANY(edata))), edata, XEN_ARG_1, caller, "an env generator or a list");
      apply_env(cp, NULL, beg, dur, over_selection, caller, egen, edpos, 7);
      return(edata);
    }
  return(XEN_FALSE);
}


static XEN g_env_selection(XEN edata, XEN base)
{
  #define H_env_selection "(" S_env_selection " env :optional (env-base 1.0)): \
apply envelope to the selection using env-base to determine how breakpoints are connected"

  if (!(selection_is_active())) 
    return(snd_no_active_selection_error(S_env_selection));
  return(g_env_1(edata, 0, 0, base, 
		 get_cp(XEN_FALSE, XEN_FALSE, S_env_selection), 
		 C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 
		 S_env_selection, OVER_SELECTION));
}


static XEN g_env_sound(XEN edata, XEN samp_n, XEN samps, XEN base, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_env_sound "(" S_env_sound " env :optional (start-samp 0) (samps len) (env-base 1.0) snd chn edpos): \
apply amplitude envelope (a list of breakpoints or a CLM env) to snd's channel chn starting at start-samp, going \
either to the end of the sound or for samps samples, with segments interpolating according to env-base"

  off_t beg = 0, dur = 0;
  int pos;
  chan_info *cp;

  ASSERT_SAMPLE_TYPE(S_env_sound, samp_n, XEN_ARG_2);
  ASSERT_SAMPLE_TYPE(S_env_sound, samps, XEN_ARG_3);
  ASSERT_CHANNEL(S_env_sound, snd_n, chn_n, 5);

  cp = get_cp(snd_n, chn_n, S_env_sound);
  if (!cp) return(XEN_FALSE);
  pos = to_c_edit_position(cp, edpos, S_env_sound, 7);
  beg = beg_to_sample(samp_n, S_env_sound);
  dur = dur_to_samples(samps, beg, cp, pos, XEN_ARG_3, S_env_sound);

  return(g_env_1(edata, beg, dur, base, cp, edpos, S_env_sound, OVER_SOUND));
}


static XEN g_env_channel(XEN gen, XEN samp_n, XEN samps, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_env_channel "(" S_env_channel " clm-env-gen-or-envelope :optional (beg 0) (dur len) snd chn edpos): \
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
  if (!cp) return(XEN_FALSE);
  beg = beg_to_sample(samp_n, S_env_channel);
  pos = to_c_edit_position(cp, edpos, S_env_channel, 6);
  dur = dur_to_samples(samps, beg, cp, pos, 3, S_env_channel);
  if (dur == 0) return(XEN_FALSE);
  if (beg > cp->edits[pos]->samples) return(XEN_FALSE); /* not redundant */
  sp = cp->sound;
  old_sync = sp->sync;
  sp->sync = 0;

  val = g_env_1(gen, beg, dur, XEN_FALSE, cp, edpos, S_env_channel, OVER_SOUND);

  sp->sync = old_sync;
  return(val);
}


static XEN g_env_channel_with_base(XEN gen, XEN base, XEN samp_n, XEN samps, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_env_channel_with_base "(" S_env_channel_with_base " clm-env-gen-or-envelope :optional (base 1.0) (beg 0) (dur len) snd chn edpos): \
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
  if (!cp) return(XEN_FALSE);
  beg = beg_to_sample(samp_n, S_env_channel);
  pos = to_c_edit_position(cp, edpos, S_env_channel, 6);
  dur = dur_to_samples(samps, beg, cp, pos, 3, S_env_channel);
  if (dur == 0) return(XEN_FALSE);
  if (beg > cp->edits[pos]->samples) return(XEN_FALSE); /* not redundant */
  sp = cp->sound;
  old_sync = sp->sync;
  sp->sync = 0;

  val = g_env_1(gen, beg, dur, base, cp, edpos, S_env_channel, OVER_SOUND);

  sp->sync = old_sync;
  return(val);
}


static XEN g_ramp_channel(XEN rmp0, XEN rmp1, XEN beg, XEN num, XEN snd, XEN chn, XEN edpos)
{
  #define H_ramp_channel "(" S_ramp_channel " rmp0 rmp1 :optional (beg 0) (dur len) snd chn edpos): \
scale samples in the given sound/channel between beg and beg + num by a ramp going from rmp0 to rmp1."

  chan_info *cp;
  off_t samp, samps;
  int pos;
  double seg0, seg1;

  XEN_ASSERT_TYPE(XEN_NUMBER_P(rmp0), rmp0, XEN_ARG_1, S_ramp_channel, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(rmp1), rmp1, XEN_ARG_2, S_ramp_channel, "a number");
  ASSERT_SAMPLE_TYPE(S_ramp_channel, beg, XEN_ARG_3);
  ASSERT_SAMPLE_TYPE(S_ramp_channel, num, XEN_ARG_4);
  ASSERT_SOUND(S_ramp_channel, snd, 5);

  samp = beg_to_sample(beg, S_ramp_channel);
  cp = get_cp(snd, chn, S_ramp_channel);
  if (!cp) return(XEN_FALSE);
  pos = to_c_edit_position(cp, edpos, S_ramp_channel, 7);
  samps = dur_to_samples(num, samp, cp, pos, 4, S_ramp_channel);

  if (unrampable(cp, samp, samps, pos, false)) /* false - not xramp */
    {
      snd_info *sp;
      int old_sync;
      XEN val;

      sp = cp->sound;
      old_sync = sp->sync;
      sp->sync = 0;
      val = g_env_1(XEN_LIST_4(C_TO_XEN_DOUBLE(0.0), rmp0, C_TO_XEN_DOUBLE(1.0), rmp1),
		    samp, samps, C_TO_XEN_DOUBLE(1.0), cp, edpos, S_ramp_channel, false);
      sp->sync = old_sync;
      return(val);
    }

  seg0 = XEN_TO_C_DOUBLE(rmp0);
  seg1 = XEN_TO_C_DOUBLE(rmp1);

  if (ramp_channel(cp, seg0, (seg1 - seg0) / (double)(samps - 1),
		   samp, samps, pos, NOT_IN_AS_ONE_EDIT))
    {
      if (cp->edits[pos]->peak_env)
	{
	  Float data[4];
	  data[0] = 0.0;
	  data[1] = seg0;
	  data[2] = 1.0;
	  data[3] = seg1;
	  if ((samp == 0) && 
	      (samps >= cp->edits[pos]->samples))
	    amp_env_env(cp, data, 2, pos, 1.0, 1.0, 0.0);
	  else 
	    {
	      mus_any *egen;
	      egen = mus_make_env_with_length(data, 2, 1.0, 0.0, 1.0, samps);
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
  #define H_xramp_channel "(" S_xramp_channel " rmp0 rmp1 base :optional (beg 0) (dur len) snd chn edpos): \
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
  if (!cp) return(XEN_FALSE);
  pos = to_c_edit_position(cp, edpos, S_xramp_channel, 8);
  samps = dur_to_samples(num, samp, cp, pos, 4, S_xramp_channel);

  ebase = XEN_TO_C_DOUBLE(base);
  if (ebase < 0.0) 
    XEN_OUT_OF_RANGE_ERROR(S_xramp_channel, 3, base, "base ~A < 0.0?");

  if (unrampable(cp, samp, samps, pos, (ebase != 1.0)))
    {
      snd_info *sp;
      int old_sync;
      XEN val;
      sp = cp->sound;
      old_sync = sp->sync;
      sp->sync = 0;
      val = g_env_1(XEN_LIST_4(C_TO_XEN_DOUBLE(0.0), rmp0, C_TO_XEN_DOUBLE(1.0), rmp1),
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
      double seg0, seg1;

      seg0 = XEN_TO_C_DOUBLE(rmp0);

      if (ebase == 0.0)
	scale_channel(cp, seg0, samp, samps, pos, NOT_IN_AS_ONE_EDIT);
      else
	{
	  seg1 = XEN_TO_C_DOUBLE(rmp1);

	  if (ebase == 1.0)
	    ramp_channel(cp, seg0, (seg1 - seg0) / (double)(samps - 1), samp, samps, pos, NOT_IN_AS_ONE_EDIT);
	  else
	    {
	      data = (Float *)calloc(4, sizeof(Float));
	      data[0] = 0.0;
	      data[1] = seg0;
	      data[2] = 1.0;
	      data[3] = seg1;
	      e = mus_make_env_with_length(data, 2, 1.0, 0.0, ebase, samps);

	      rates = mus_env_rates(e);
	      passes = mus_env_passes(e);

	      if (xramp_channel(cp, mus_env_initial_power(e), rates[0], mus_env_scaler(e), mus_env_offset(e), samp, samps, pos, NOT_IN_AS_ONE_EDIT, e, 0))
		{
		  if (cp->edits[pos]->peak_env)
		    {
		      if ((samp == 0) && 
			  (samps >= cp->edits[pos]->samples))
			amp_env_env(cp, data, 2, pos, ebase, mus_env_scaler(e), mus_env_offset(e));
		      else 
			{
			  mus_any *egen;
			  egen = mus_make_env_with_length(data, 2, 1.0, 0.0, ebase, samps);
			  amp_env_env_selection_by(cp, egen, samp, samps, pos);
			  mus_free(egen);
			}
		    }
		}
	      free(data);
	      mus_free(e);
	      update_graph(cp);
	    }
	}
    }
  return(rmp0);
}			  


static XEN g_fft(XEN reals, XEN imag, XEN sign)
{
  #define H_fft "(" S_fft " reals imags :optional (sign 1)): fft the data returning the result in reals. \
If sign is -1, perform inverse fft.  Incoming data is in vcts."

  vct *v1 = NULL, *v2 = NULL;
  int n = 0, n2 = 0, isign = 1;
  bool need_free = false;
  Float *rl = NULL, *im = NULL;

  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(sign), sign, XEN_ARG_3, S_fft, "an integer");
  XEN_ASSERT_TYPE(MUS_VCT_P(reals), reals, XEN_ARG_1, S_fft, "vct");
  XEN_ASSERT_TYPE(MUS_VCT_P(imag), imag, XEN_ARG_2, S_fft, "vct");

  isign = XEN_TO_C_INT_OR_ELSE(sign, 1);
  v1 = XEN_TO_VCT(reals);
  v2 = XEN_TO_VCT(imag);

  n = v1->length;
  if (v2->length < n) n = v2->length;
  if (n == 0) return(XEN_ZERO);
  if (POWER_OF_2_P(n))
    {
      n2 = n;
      rl = v1->data;
      im = v2->data;
    }
  else
    {
      int ipow;
      ipow = (int)ceil(log(n + 1) / log(2.0)); /* ceil because we're assuming below that n2 >= n */
      n2 = snd_int_pow2(ipow);
      rl = (Float *)calloc(n2, sizeof(Float));
      im = (Float *)calloc(n2, sizeof(Float));
      need_free = true;
      memcpy((void *)rl, (void *)(v1->data), n * sizeof(Float));
      memcpy((void *)im, (void *)(v2->data), n * sizeof(Float));
    }

  mus_fft(rl, im, n2, isign);

  if (need_free)
    {
      memcpy((void *)(v1->data), (void *)rl, n * sizeof(Float));
      memcpy((void *)(v2->data), (void *)im, n * sizeof(Float));
      free(rl);
      free(im);
    }
  return(xen_return_first(reals, imag));
}


static XEN g_snd_spectrum(XEN data, XEN win, XEN len, XEN linear_or_dB, XEN beta, XEN in_place, XEN normalized)
{
  #define H_snd_spectrum "(" S_snd_spectrum " data :optional (window " S_rectangular_window ") (len data-len) (linear " PROC_TRUE ") (beta 0.0) in-place (normalized " PROC_TRUE ")): \
magnitude spectrum of data (a vct), in data if in-place, using fft-window win and fft length len."

  bool linear = true, in_data = false, normed = true;
  int i, j, n, n2, wtype;
  Float maxa, lowest, b = 0.0;
  Float *rdat;
  vct *v;

  XEN_ASSERT_TYPE((MUS_VCT_P(data)), data, XEN_ARG_1, S_snd_spectrum, "a vct");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(win), win, XEN_ARG_2, S_snd_spectrum, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(len), len, XEN_ARG_3, S_snd_spectrum, "an integer");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(linear_or_dB), linear_or_dB, XEN_ARG_4, S_snd_spectrum, "a boolean");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(beta), beta, XEN_ARG_5, S_snd_spectrum, "a number");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(in_place), in_place, XEN_ARG_6, S_snd_spectrum, "a boolean");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(normalized), normalized, XEN_ARG_7, S_snd_spectrum, "a boolean");

  v = XEN_TO_VCT(data);
  n = XEN_TO_C_INT_OR_ELSE(len, v->length);
  if (n <= 0)
    XEN_OUT_OF_RANGE_ERROR(S_snd_spectrum, 3, len, "length ~A <= 0?");
  if (n > v->length) n = v->length;

  if (XEN_BOOLEAN_P(linear_or_dB)) linear = XEN_TO_C_BOOLEAN(linear_or_dB);
  if (XEN_BOOLEAN_P(in_place)) in_data = XEN_TO_C_BOOLEAN(in_place);
  if (XEN_BOOLEAN_P(normalized)) normed = XEN_TO_C_BOOLEAN(normalized);

  wtype = XEN_TO_C_INT_OR_ELSE(win, (int)MUS_RECTANGULAR_WINDOW);
  if (!(mus_fft_window_p(wtype)))
    XEN_OUT_OF_RANGE_ERROR(S_snd_spectrum, 2, win, "~A: unknown fft window");

  if (XEN_NUMBER_P(beta)) b = XEN_TO_C_DOUBLE(beta);
  if (b < 0.0) b = 0.0; else if (b > 1.0) b = 1.0;

  if (!in_data)
    {
      rdat = (Float *)malloc(n * sizeof(Float));
      if (n < v->length)
	for (i = 0; i < n; i++) rdat[i] = v->data[i];
      else memcpy((void *)rdat, (void *)(v->data), v->length * sizeof(Float));
    }
  else rdat = v->data;

  if (wtype != (int)MUS_RECTANGULAR_WINDOW)
    {
      Float *window;
      window = (Float *)calloc(n, sizeof(Float));
      mus_make_fft_window_with_window((mus_fft_window_t)wtype, n, b * fft_beta_max((mus_fft_window_t)wtype), 0.0, window);
      for (i = 0; i < n; i++) rdat[i] *= window[i];
      free(window);
    }
    
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
  {
    Float *idat;
    idat = (Float *)calloc(n, sizeof(Float));
    mus_fft(rdat, idat, n, 1);
    rdat[0] *= rdat[0];
    rdat[n2] *= rdat[n2];
    for (i = 1, j = n - 1; i < n2; i++, j--)
      {
	rdat[i] = rdat[i] * rdat[i] + idat[i] * idat[i];
	rdat[j] = rdat[i];
      }
    free(idat);
  }
#endif

  lowest = 0.000001;
  maxa = 0.0;
  n = n / 2;
  for (i = 0; i < n; i++)
    {
      Float val;
      val = rdat[i];
      if (val < lowest)
	rdat[i] = 0.0;
      else 
	{
	  rdat[i] = sqrt(val);
	  if (rdat[i] > maxa) maxa = rdat[i];
	}
    }
  if (maxa > 0.0)
    {
      if (normed)
	maxa = 1.0 / maxa;
      else maxa = 1.0;
      if (!linear) /* dB */
	{
	  Float todb;
	  todb = 20.0 / log(10.0);
	  for (i = 0; i < n; i++) 
	    if (rdat[i] > 0.0)
	      rdat[i] = todb * log(rdat[i] * maxa);
	    else rdat[i] = -90.0; /* min_dB(ss)? or could channel case be less? */
	}
      else 
	{
	  if (normed)
	    for (i = 0; i < n; i++) 
	      rdat[i] *= maxa;
	}
    }
  if (in_data)
    return(data);
  return(xen_return_first(xen_make_vct(n, rdat), data, win)); /* xen_make_vct uses the data array directly (frees upon gc) */
}


static XEN g_convolve_with_1(XEN file, XEN new_amp, chan_info *cp, XEN edpos, const char *caller)
{
  /* cp NULL -> selection (see above) */
  Float amp;
  static char *fname = NULL;

  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, caller, "a string");

  if (XEN_NUMBER_P(new_amp)) 
    amp = XEN_TO_C_DOUBLE(new_amp);
  else
    {
      if (XEN_FALSE_P(new_amp))
	amp = 0.0;
      else amp = 1.0;
    }
  if (fname) free(fname);
  fname = mus_expand_filename(XEN_TO_C_STRING(file));

  if (mus_file_probe(fname))
    {
      char *error = NULL;

      error = convolve_with_or_error(fname, amp, cp, edpos, 5);

      if (error)
	{
	  XEN errstr;
	  errstr = C_TO_XEN_STRING(error);
	  free(error);
	  XEN_ERROR(XEN_ERROR_TYPE("IO-error"),
		    XEN_LIST_2(C_TO_XEN_STRING(caller),
			       errstr));
	}
    }
  else return(snd_no_such_file_error(caller, file));
  return(xen_return_first(file, new_amp));
}


static XEN g_convolve_with(XEN file, XEN new_amp, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_convolve_with "(" S_convolve_with " file :optional (amp 1.0) snd chn edpos): \
convolve file with snd's channel chn (or the currently sync'd channels); amp is the resultant peak amp"

  chan_info *cp;
  ASSERT_CHANNEL(S_convolve_with, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_convolve_with);
  if (!cp) return(XEN_FALSE);
  return(g_convolve_with_1(file, new_amp, cp, edpos, S_convolve_with));
}


static XEN g_convolve_selection_with(XEN file, XEN new_amp)
{
  #define H_convolve_selection_with "(" S_convolve_selection_with " file :optional (amp 1.0)): \
convolve the selection with file; amp is the resultant peak amp"

  if (!(selection_is_active())) 
    return(snd_no_active_selection_error(S_convolve_selection_with));
  return(g_convolve_with_1(file, new_amp, NULL, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), S_convolve_selection_with));
}


enum {SRC_ENV_NO_ERROR, SRC_ENV_HIT_ZERO, SRC_ENV_THROUGH_ZERO};

static Float check_src_envelope(int pts, Float *data, int *error)
{
  /* can't go through zero here, and if negative need to return 1.0 */
  int i;
  Float res = 0.0;
  (*error) = SRC_ENV_NO_ERROR;
  for (i = 0; i < (2 * pts); i += 2)
    if (data[i + 1] == 0.0)
      {
	(*error) = SRC_ENV_HIT_ZERO;
	return(res);
      }
    else
      {
	if (data[i + 1] < 0.0)
	  {
	    if (res <= 0.0)
	      res = -1.0;
	    else
	      {
		(*error) = SRC_ENV_THROUGH_ZERO;
		return(res);
	      }
	  }
	else
	  {
	    if (res >= 0)
	      res = 1.0;
	    else
	      {
		(*error) = SRC_ENV_THROUGH_ZERO;
		return(res);
	      }
	  }
      }
  return(res);
}


static XEN g_src_channel(XEN ratio_or_env, XEN beg_n, XEN dur_n, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_src_channel "(" S_src_channel " ratio-or-env :optional (beg 0) (dur len) snd chn edpos): \
sampling-rate convert snd's channel chn by ratio, or following an envelope (a list or a CLM env generator)."

  chan_info *cp;
  char *errmsg;
  snd_fd *sf;
  off_t beg, dur;
  int pos;
  bool clm_err = false;
  mus_any *egen = NULL;
  bool need_free = false;
  Float ratio = 0.0; /* not 1.0 here! -- the zero is significant */

  XEN_ASSERT_TYPE((XEN_NUMBER_P(ratio_or_env)) || 
		  (XEN_LIST_P(ratio_or_env)) ||
		  ((mus_xen_p(ratio_or_env)) && 
		   (mus_env_p(egen = XEN_TO_MUS_ANY(ratio_or_env)))),
		  ratio_or_env, XEN_ARG_1, S_src_channel, "a number, an envelope, or a CLM env generator");
  ASSERT_SAMPLE_TYPE(S_src_channel, beg_n, XEN_ARG_2);
  ASSERT_SAMPLE_TYPE(S_src_channel, dur_n, XEN_ARG_3);
  ASSERT_CHANNEL(S_src_channel, snd_n, chn_n, 4);

  cp = get_cp(snd_n, chn_n, S_src_channel);
  if (!cp) return(XEN_FALSE);
  beg = beg_to_sample(beg_n, S_src_channel);
  pos = to_c_edit_position(cp, edpos, S_src_channel, 6);
  dur = dur_to_samples(dur_n, beg, cp, pos, 3, S_src_channel);
  if (dur == 0) return(XEN_FALSE);
  if (beg > cp->edits[pos]->samples) return(XEN_FALSE);

  if (XEN_NUMBER_P(ratio_or_env))
    {
      ratio = XEN_TO_C_DOUBLE(ratio_or_env);
      if ((pos == cp->edit_ctr) &&
	  ((ratio == 0.0) || (ratio == 1.0)))
	return(XEN_FALSE);
    }
  else 
    {
      int error = SRC_ENV_NO_ERROR;
      if (egen == NULL)
	{
	  env *e;
	  e = get_env(ratio_or_env, S_src_channel);
	  egen = mus_make_env_with_length(e->data, e->pts, 1.0, 0.0, e->base, dur);
	  need_free = true;
	  free_env(e);
	}
      check_src_envelope(mus_env_breakpoints(egen), mus_data(egen), &error);
      if (error != SRC_ENV_NO_ERROR)
	{
	  XEN data;
	  data = mus_array_to_list(mus_data(egen), 0, mus_env_breakpoints(egen) * 2);
	  if (need_free) 
	    {
	      mus_free(egen); 
	      need_free = false;
	    }
	  if (error == SRC_ENV_HIT_ZERO)
	    XEN_OUT_OF_RANGE_ERROR(S_src_channel, 1, data, "~A: envelope hits 0.0");
	  else XEN_OUT_OF_RANGE_ERROR(S_src_channel, 1, data, "~A: envelope passes through 0.0");
	  return(XEN_FALSE); /* just for clarity... */
	}
    }

  if (((egen) && (mus_phase(egen) >= 0.0)) ||
      ((!egen) && (ratio >= 0.0))) /* ratio == 0.0 if env in use because env is the srate (as change arg) */
    sf = init_sample_read_any(beg, cp, READ_FORWARD, pos);
  else sf = init_sample_read_any(beg + dur - 1, cp, READ_BACKWARD, pos);

  errmsg = src_channel_with_error(cp, sf, beg, dur, ratio, egen, S_src_channel, OVER_SOUND, &clm_err);
  sf = free_snd_fd(sf);
  if (need_free) mus_free(egen);
  if (errmsg)
    {
      XEN err;
      err = C_TO_XEN_STRING(errmsg);
      free(errmsg);
      XEN_ERROR(XEN_ERROR_TYPE((clm_err) ? "mus-error" : "IO-error"),
		XEN_LIST_2(C_TO_XEN_STRING(S_src_channel),
			   err));
    }
  return(ratio_or_env);
}


static XEN g_src_1(XEN ratio_or_env, XEN ebase, XEN snd_n, XEN chn_n, XEN edpos, const char *caller, bool over_selection)
{
  chan_info *cp;
  ASSERT_CHANNEL(caller, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, caller);
  if (!cp) return(XEN_FALSE);
  if (XEN_NUMBER_P(ratio_or_env))
    {
      Float ratio;
      ratio = XEN_TO_C_DOUBLE(ratio_or_env);
      if (ratio != 1.0)
	src_env_or_num(cp, NULL, ratio,
		       true, caller,
		       over_selection, NULL, edpos, 5);
    }
  else 
    {
      int error = SRC_ENV_NO_ERROR;
      if (XEN_LIST_P(ratio_or_env))
	{
	  env *e = NULL;
	  Float e_ratio = 1.0;

	  /* env 'e' is a temp here, so we can clobber its base, etc */
	  e = get_env(ratio_or_env, caller);
	  if (XEN_NUMBER_P(ebase))
	    e->base = XEN_TO_C_DOUBLE_OR_ELSE(ebase, 1.0);

	  e_ratio = check_src_envelope(e->pts, e->data, &error);
	  if (error != SRC_ENV_NO_ERROR)
	    {
	      XEN data;
	      data = mus_array_to_list(e->data, 0, e->pts * 2);
	      if (e) {free_env(e); e = NULL;}
	      if (error == SRC_ENV_HIT_ZERO)
		XEN_OUT_OF_RANGE_ERROR(caller, 1, data, "~A: envelope hits 0.0");
	      else XEN_OUT_OF_RANGE_ERROR(caller, 1, data, "~A: envelope passes through 0.0");
	    }
	  else
	    {
	      src_env_or_num(cp, e, e_ratio, false, caller, over_selection, NULL, edpos, 5);
	      if (e) free_env(e);
	    }
	}
      else
	{
	  mus_any *egen;
	  XEN_ASSERT_TYPE(mus_xen_p(ratio_or_env), ratio_or_env, XEN_ARG_1, caller, "a number, list, or env generator");
	  egen = XEN_TO_MUS_ANY(ratio_or_env);
	  XEN_ASSERT_TYPE(mus_env_p(egen), ratio_or_env, XEN_ARG_1, caller, "a number, list, or env generator");

	  check_src_envelope(mus_env_breakpoints(egen), mus_data(egen), &error);
	  if (error != SRC_ENV_NO_ERROR)
	    {
	      XEN data;
	      data = mus_array_to_list(mus_data(egen), 0, mus_env_breakpoints(egen) * 2);
	      if (error == SRC_ENV_HIT_ZERO)
		XEN_OUT_OF_RANGE_ERROR(S_src_channel, 1, data, "~A: envelope hits 0.0");
	      else XEN_OUT_OF_RANGE_ERROR(S_src_channel, 1, data, "~A: envelope passes through 0.0");
	    }
	  else
	    src_env_or_num(cp, NULL, 
			   (mus_phase(egen) >= 0.0) ? 1.0 : -1.0, /* mus_phase of env apparently = current_value(!) */
			   false, caller, 
			   over_selection, egen, edpos, 5);
	}
    }
  return(xen_return_first(ratio_or_env, ebase));
}


static XEN g_src_sound(XEN ratio_or_env, XEN base, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_src_sound "(" S_src_sound " ratio-or-env :optional (base 1.0) snd chn edpos): \
sampling-rate convert snd's channel chn by ratio, or following an envelope. A negative ratio reverses the sound"

  return(g_src_1(ratio_or_env, base, snd_n, chn_n, edpos, S_src_sound, OVER_SOUND));
}


static XEN g_src_selection(XEN ratio_or_env, XEN base)
{
  #define H_src_selection "(" S_src_selection " ratio-or-env :optional (base 1.0)): \
sampling-rate convert the currently selected data by ratio (which can be an envelope)"

  if (!(selection_is_active())) 
    return(snd_no_active_selection_error(S_src_selection));
  return(g_src_1(ratio_or_env, base, XEN_FALSE, XEN_FALSE, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), S_src_selection, OVER_SELECTION));
}


static XEN g_filter_channel(XEN e, XEN order, XEN beg, XEN dur, XEN snd_n, XEN chn_n, XEN edpos, XEN truncate, XEN origin)
{
  #define H_filter_channel "(" S_filter_channel " env :optional order beg dur snd chn edpos (truncate " PROC_TRUE ") origin): \
applies an FIR filter to snd's channel chn. 'env' is the frequency response envelope, or a vct with the coefficients."

  chan_info *cp;
  char *errstr = NULL;
  const char *caller = NULL;
  bool truncate_1 = true;
  int order_1 = 0, edpos_1 = AT_CURRENT_EDIT_POSITION;
  off_t beg_1 = 0, dur_1 = 0;
  env *e_1 = NULL;
  vct *v = NULL;
  Float *coeffs = NULL;

  XEN_ASSERT_TYPE(XEN_LIST_P(e) || MUS_VCT_P(e), e, XEN_ARG_1, S_filter_channel, "an envelope or a vct");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(order), order, XEN_ARG_2, S_filter_channel, "an integer");
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(origin), origin, XEN_ARG_9, S_filter_channel, "a string");
  if (XEN_INTEGER_P(order)) order_1 = XEN_TO_C_INT(order);
  ASSERT_CHANNEL(S_filter_channel, snd_n, chn_n, 5);
  cp = get_cp(snd_n, chn_n, S_filter_channel);
  if (!cp) return(XEN_FALSE);
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(truncate), truncate, XEN_ARG_8, S_filter_channel, "boolean");
  if (XEN_BOOLEAN_P(truncate)) truncate_1 = XEN_TO_C_BOOLEAN(truncate);
  ASSERT_SAMPLE_TYPE(S_filter_channel, beg, XEN_ARG_3);
  ASSERT_SAMPLE_TYPE(S_filter_channel, dur, XEN_ARG_4);
  beg_1 = beg_to_sample(beg, S_filter_channel);
  edpos_1 = to_c_edit_position(cp, edpos, S_filter_channel, 7);
  dur_1 = dur_to_samples(dur, beg_1, cp, edpos_1, 4, S_filter_channel);
  if (XEN_LIST_P(e))
    {
      e_1 = get_env(e, S_filter_channel);
      if (e_1 == NULL) return(XEN_FALSE);
      if (order_1 == 0) order_1 = e_1->pts * 4;
    }
  else 
    {
      v = XEN_TO_VCT(e);
      coeffs = v->data;
      if (order_1 == 0) order_1 = v->length;
    }
  if (XEN_STRING_P(origin))
    caller = XEN_TO_C_STRING(origin);
  errstr = filter_channel(cp, order_1, e_1, beg_1, dur_1, edpos_1, caller, truncate_1, coeffs);
  if (e_1) free_env(e_1);
  if (errstr)
    {
      XEN str;
      str = C_TO_XEN_STRING(errstr);
      free(errstr);
      XEN_ERROR(XEN_ERROR_TYPE("IO-error"),
		XEN_LIST_2(C_TO_XEN_STRING(S_filter_channel),
			   str));
    }
  return(e);
}


static XEN g_filter_1(XEN e, XEN order, XEN snd_n, XEN chn_n, XEN edpos, const char *caller, const char *origin, bool over_selection, bool truncate)
{
  chan_info *cp;
  ASSERT_CHANNEL(caller, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, caller);
  if (!cp) return(XEN_FALSE);
  if (mus_xen_p(e))
    {
      char *error;
      bool clm_err = false;
      error = apply_filter_or_error(cp, 0, NULL, caller, origin, over_selection, NULL, XEN_TO_MUS_ANY(e), edpos, 5, truncate, &clm_err);
      if (error)
	{
	  XEN errstr;
	  errstr = C_TO_XEN_STRING(error);
	  free(error);
	  XEN_ERROR(XEN_ERROR_TYPE((clm_err) ? "mus-error" : "IO-error"),
		    XEN_LIST_2(C_TO_XEN_STRING(caller),
			       errstr));
	}
    }
  else
    {
      int len = 0;
      XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(order), order, XEN_ARG_2, caller, "an integer");
      if (XEN_INTEGER_P(order)) 
	{
	  len = XEN_TO_C_INT(order);
	  if (len <= 0) 
	    XEN_OUT_OF_RANGE_ERROR(caller, 2, order, "order ~A <= 0?");
	}
      if (MUS_VCT_P(e)) /* the filter coefficients direct */
	{
	  vct *v;
	  char *new_origin = NULL, *estr = NULL;
	  v = XEN_TO_VCT(e);
	  if (len > v->length) 
	    XEN_OUT_OF_RANGE_ERROR(caller, 2, order, "order ~A > length coeffs?");
	  else
	    {
	      if (len == 0) len = v->length;
	    }
	  if ((!origin) && (v->length < 16))
	    {
	      estr = mus_vct_to_readable_string(v);
#if HAVE_FORTH
	      new_origin = mus_format("%s %d%s %s", 
				      estr, len, 
				      (over_selection) ? "" : PROC_SEP "0" PROC_SEP PROC_FALSE,
				      caller);
#else
	      new_origin = mus_format("%s" PROC_OPEN "%s" PROC_SEP "%d%s", 
				      TO_PROC_NAME(caller), estr, len, 
				      (over_selection) ? "" : PROC_SEP "0" PROC_SEP PROC_FALSE);
#endif
	    }
	  else new_origin = mus_strdup(origin);
	  apply_filter(cp, len, NULL, caller, new_origin, over_selection, v->data, NULL, edpos, 5, truncate);
	  if (estr) free(estr);
	  if (new_origin) free(new_origin);
	}
      else 
	{
	  env *ne = NULL;
	  char *new_origin = NULL, *estr = NULL;
	  XEN_ASSERT_TYPE(XEN_LIST_P(e), e, XEN_ARG_1, caller, "a list, vct, or env generator");
	  ne = get_env(e, caller); /* arg here must be a list */
	  if (!origin)
	    {
	      estr = env_to_string(ne);
#if HAVE_FORTH
	      new_origin = mus_format("%s %d%s %s", 
				      estr, len,
				      (over_selection) ? "" : " 0 " PROC_FALSE,
				      caller);
#else
	      new_origin = mus_format("%s" PROC_OPEN "%s" PROC_SEP "%d%s", 
				      TO_PROC_NAME(caller), estr, len, (over_selection) ? "" : PROC_SEP "0" PROC_SEP PROC_FALSE);
#endif
	    }
	  else new_origin = mus_strdup(origin);
	  if (len == 0) len = ne->pts * 4;
	  apply_filter(cp, len, ne, caller, new_origin, over_selection, NULL, NULL, edpos, 5, truncate);
	  if (ne) free_env(ne); 
	  if (estr) free(estr);
	  if (new_origin) free(new_origin);
	}
    }
  return(xen_return_first(XEN_TRUE, e));
}


static XEN g_filter_sound(XEN e, XEN order, XEN snd_n, XEN chn_n, XEN edpos, XEN origin)
{
  #define H_filter_sound "(" S_filter_sound " filter :optional order snd chn edpos origin): \
applies FIR filter to snd's channel chn. 'filter' is either the frequency response envelope, a CLM filter, or a vct with the actual coefficients"

  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(origin), origin, XEN_ARG_6, S_filter_sound, "a string");
  return(g_filter_1(e, order, snd_n, chn_n, edpos, 
		    S_filter_sound, (XEN_STRING_P(origin)) ? XEN_TO_C_STRING(origin) : NULL, 
		    OVER_SOUND, false));
}


static XEN g_filter_selection(XEN e, XEN order, XEN truncate)
{
  #define H_filter_selection "(" S_filter_selection " filter :optional order (truncate " PROC_TRUE ")): apply filter to selection. If truncate, \
cut off filter output at end of selection, else mix"

  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(truncate), truncate, XEN_ARG_3, S_filter_selection, "boolean");
  if (!(selection_is_active())) 
    return(snd_no_active_selection_error(S_filter_selection));
  return(g_filter_1(e, order, XEN_FALSE, XEN_FALSE, 
		    C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 
		    S_filter_selection, NULL, 
		    OVER_SELECTION, XEN_TO_C_BOOLEAN(truncate)));
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
  if ((len >= 0) && (len <= MUS_MAX_CLM_SINC_WIDTH))
    set_sinc_width(len);
  return(C_TO_XEN_INT(sinc_width(ss)));
}


#if HAVE_NESTED_FUNCTIONS

#define S_find_min_peak_phases "find-min-peak-phases"

static XEN g_find_min_peak_phases(XEN arglist)
{
  #define H_find_min_peak_phases "(" S_find_min_peak_phases " dist vcts...) returns a vector of (sample-wise) offsets \
that give a minimum peak amplitude when the signals are added together."

  Float best = 0.0;
  int n, size = 0, dist = 0;
  int *best_phases;
  int *current_phases;
  Float *current_max;
  Float **current_sum;
  Float **sines;
  int *sizes;
  auto void try_case(int i);

  void try_case(int i)
  {
    if ((current_max[i - 1] - n + i) < best)
      {
	int j, k, kk, n1, isize;

	n1 = n - 1;
	isize = sizes[i];

	for (j = 0; j < dist; j++)
	  {
	    /* j is current phase of i-th component */

	    current_max[i] = 0.0;
	    current_phases[i] = j;

	    for (k = 0, kk = j; k < size; k++, kk++)
	      {
		Float fval;
		if (kk >= isize) kk = 0;
		fval = current_sum[i - 1][k] + sines[i][kk];
		current_sum[i][k] = fval;
		fval = fabs(fval);
		if (fval > current_max[i])
		  {
		    current_max[i] = fval;
		    if ((i == n1) &&
			(fval > best))
		      break;
		  }
	      }

	    if (i == n1)
	      {
		if (current_max[i] < best)
		  {
		    int m;
		    best = current_max[i];
		    for (m = 0; m < n; m++)
		      best_phases[m] = current_phases[m];
		  }
	      }
	    else try_case(i + 1);
	  }
      }
  }

  int i;
  Float cmax;
  XEN result = XEN_EMPTY_LIST;

  n = (XEN_LIST_LENGTH(arglist) - 1);
  if (n < 2) return(XEN_LIST_1(C_TO_XEN_INT(0)));
  dist = XEN_TO_C_INT(XEN_CAR(arglist));
  arglist = XEN_CDR(arglist);

  sines = (Float **)calloc(n, sizeof(Float *));
  sizes = (int *)calloc(n, sizeof(int));
  best_phases = (int *)calloc(n, sizeof(int));
  current_phases = (int *)calloc(n, sizeof(int));
  current_sum = (Float **)calloc(n, sizeof(Float *));
  current_max = (Float *)calloc(n, sizeof(Float));

  for (i = 0; i < n; i++)
    {
      vct *v;
      current_max[i] = 0.0;
      v = XEN_TO_VCT(XEN_LIST_REF(arglist, i));
      sines[i] = v->data;
      sizes[i] = v->length;
      if (sizes[i] > size)
	size = sizes[i];
    }

  for (i = 0; i < n; i++)
    current_sum[i] = (Float *)calloc(size, sizeof(Float));

  best = 10000.0;
  cmax = fabs(sines[0][0]);
  for (i = 1; i < sizes[0]; i++) 
    {
      Float absv;
      absv = fabs(sines[0][i]); 
      if (absv > cmax) cmax = absv;
    }
  current_max[0] = cmax;
  current_phases[0] = 0;

  for (i = 0; i < size; i++)
    current_sum[0][i] = sines[0][i];

  try_case(1);

  for (i = 0; i < n; i++)
    result = XEN_CONS(C_TO_XEN_INT(best_phases[i]), result);

  free(best_phases);
  free(current_phases);
  for (i = 0; i < n; i++)
    free(current_sum[i]);
  free(current_sum);
  free(sines);
  free(current_max);

  return(XEN_LIST_2(C_TO_XEN_DOUBLE(best), XEN_LIST_REVERSE(result)));
}

#if 0
(let ((v1 (make-vct 2048))
      (v2 (make-vct 2048))
      (v3 (make-vct 2048))
      (incr (/ (* 2 pi) 2048)))
  (do ((i 0 (+ i 1))
       (x 0.0 (+ x incr)))
      ((= i 2048))
    (vct-set! v1 i (sin x))
    (vct-set! v2 i (sin (* 2 x)))
    (vct-set! v3 i (sin (* 3 x))))
 (find-min-peak-phases 1024 v1 v2 v3))

(1.98045 (0 210 1940))

:(modulo (/ (* 210 2) 1024.0) 2.0)
0.41015625
:(modulo (/ (* 1940 3) 1024.0) 2.0)
1.68359375
#endif


#if HAVE_SYS_TIME_H
  #include <sys/time.h>
#endif

#define S_find_min_peak_phases_via_sa "find-min-peak-phases-via-sa"

typedef struct {
  Float pk;
  Float *phases;
} pk_data;


static XEN g_find_min_peak_phases_via_sa(XEN x_choice, XEN x_n, XEN x_size, XEN x_increment, XEN x_counts, XEN x_file, XEN x_just_best)
{
  #define H_find_min_peak_phases_via_sa "(" S_find_min_peak_phases_via_sa " choice n (size 1000) (increment 1.0) (counts 50) output-file report-best) searches \
for a peak-amp minimum using a simulated annealing form of the genetic algorithm.  choice: 0=all, 1=odd, 2=even, 3=prime."

  #define FFT_MULT 128
  #define INCR_DOWN 0.9
  #define INCR_UP 1.11
  #define INCR_MAX 1.0
  #define INCR_MIN 0.001
  #define RETRIES 10
  #define RETRY_MULT 2
  #define INIT_TRIES 1000
  #define ALL 0
  #define ODD 1
  #define EVEN 2
  #define PRIME 3

  /* (find-min-peak-phases-via-sa 0 8 1024 1.0 1000) */

  int choice, n, size, counts = 0, day_counter = 0, days = 0, years = 0, free_top = 0, fft_size = 0;
  off_t ffts = 0;
  Float increment = INCR_MAX, orig_incr, local_best = 1000.0, incr_mult = INCR_DOWN, overall_min;
  Float *min_phases = NULL, *temp_phases = NULL, *diff_phases = NULL;
  char *choice_name[4] = {"all", "odd", "even", "prime"};
  pk_data **choices = NULL, **free_choices = NULL;
  Float *rl, *im;
  const char *file = NULL;
  bool just_best = true;

  static int primes[129] = {1, 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 
			    89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 
			    191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 
			    283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 
			    401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 
			    509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 
			    631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719};

  static Float all_mins[128] = {1.0000, 1.7600, 1.9797, 2.0390, 2.3435, 2.5493, 2.6394, 2.7947, 2.9618, 3.1027, 3.2185, 3.3894, 3.5252, 3.6148, 3.7707, 3.8769, 4.0179, 4.1595, 4.2714, 4.2972, 4.5025, 4.5965, 4.6269, 4.8284, 4.9269, 5.0273, 5.0836, 5.2679, 5.3326, 5.4523, 5.5953, 5.6193, 5.6965, 5.8219, 5.9503, 6.1257, 6.1978, 6.3435, 6.5085, 6.4878, 6.6441, 6.7020, 6.7939, 6.9326, 6.9962, 6.9547, 7.2168, 7.2732, 7.4638, 7.5149, 7.6679, 7.6894, 7.9162, 7.9121, 8.0678, 8.0261, 8.0906, 8.2107, 8.4502, 8.4668, 8.4479, 8.5318, 8.6346, 8.8393, 8.6660, 8.8047, 9.2612, 9.3290, 9.2101, 9.2568, 9.2908, 9.6381, 9.5244, 9.7072, 9.6856, 9.7685, 9.8787, 9.8857, 10.1199, 9.8661, 10.2143, 10.3042, 10.3984, 10.5100, 10.5646, 10.6828, 10.6059, 10.6512, 10.9031, 10.7360, 10.8762, 11.1956, 11.2454, 11.0737, 11.1708, 11.2879, 11.2232, 11.2492, 11.3409, 11.7061, 11.5530, 11.7391, 12.0150, 11.9801, 12.1842, 11.9097, 11.9889, 11.9986, 12.1649, 12.4430, 12.2566, 12.2453, 12.5819, 12.5320, 12.6797, 12.6437, 12.6492, 12.8358, 12.9435, 13.0008, 13.0728, 13.3460, 13.2101, 13.4446, 13.1920, 13.5434, 13.0299, 13.4039};

  static Float odd_mins[128] = {1.0000, 1.5390, 1.7387, 2.0452, 2.3073, 2.5227, 2.6184, 2.7908, 2.8865, 3.0538, 3.1771, 3.3628, 3.4758, 3.5998, 3.7418, 3.8607, 3.9558, 4.0793, 4.1940, 4.3752, 4.4620, 4.6110, 4.7242, 4.8070, 4.9403, 5.0582, 5.1390, 5.2759, 5.3165, 5.5557, 5.6145, 5.7272, 5.9201, 5.9364, 6.1337, 6.2024, 6.3452, 6.4988, 6.4621, 6.6360, 6.9244, 6.8612, 6.8205, 7.0276, 7.0156, 7.1994, 7.3767, 7.3927, 7.6095, 7.5838, 7.6035, 7.8122, 7.8346, 8.0192, 8.0570, 8.0144, 8.1612, 8.3401, 8.2379, 8.5646, 8.4698, 8.6778, 8.4935, 8.6112, 9.1140, 9.2052, 9.2263, 9.2944, 9.1870, 9.4407, 9.2568, 9.4386, 9.8958, 9.7606, 9.7974, 9.9192, 10.2311, 10.1554, 9.9580, 10.2859, 10.5240, 10.2987, 10.5277, 10.3449, 10.6591, 10.7596, 10.7491, 10.8645, 11.0295, 10.6243, 11.0832, 11.1025, 11.2975, 11.3129, 11.0756, 11.4449, 11.6123, 11.4624, 11.9005, 11.8437, 11.8643, 12.0445, 11.9704, 12.1713, 12.1138, 12.3211, 12.2717, 12.6212, 12.6901, 12.5508, 12.3335, 12.6528, 12.6896, 12.7357, 12.5388, 12.8764, 12.8545, 13.0860, 13.2148, 13.2046, 13.0295, 13.3308, 13.3802, 13.1349, 13.5020, 13.5474, 13.6024, 13.7021};

  static Float prime_mins[128] = {1.0000, 1.7600, 1.9798, 2.1921, 2.4768, 2.8055, 3.0619, 3.2630, 3.3826, 3.6026, 3.7793, 3.9389, 4.1805, 4.3288, 4.4821, 4.6627, 4.7328, 5.0131, 5.2245, 5.3328, 5.4869, 5.5946, 5.8299, 5.9499, 6.0160, 6.2062, 6.3061, 6.3476, 6.5570, 6.6997, 6.8025, 7.0594, 7.0959, 7.2019, 7.4036, 7.5876, 7.6839, 7.8411, 7.8274, 7.9107, 8.0697, 8.1674, 8.4260, 8.5680, 8.6060, 8.7683, 8.8215, 9.0524, 9.1627, 9.0363, 9.5282, 9.4762, 9.5539, 9.6162, 9.9782, 10.2323, 10.1988, 10.2538, 10.2892, 10.4623, 10.7092, 10.8440, 10.7537, 10.9221, 11.1046, 10.9904, 11.4203, 11.5639, 11.3183, 11.4297, 11.5959, 12.0523, 11.8836, 12.1569, 11.7339, 12.1164, 12.1038, 12.1877, 12.5344, 12.6652, 12.8292, 12.7533, 12.9178, 12.6803, 12.9856, 13.0590, 13.3143, 13.2348, 13.7775, 13.4455, 13.4881, 13.5451, 13.3385, 14.0556, 13.7647, 14.2813, 14.3273, 14.3091, 14.4479, 14.5451, 14.8538, 14.7827, 14.7885, 15.3207, 15.1590, 15.1682, 15.4821, 15.4423, 15.8350, 15.5673, 15.3480, 15.9092, 15.6546, 15.5505, 15.9319, 15.9615, 15.9801, 16.1738, 16.2489, 16.4854, 16.0579, 16.4014, 16.7553, 16.4575, 16.8264, 16.6542, 16.9029, 17.0447};

  static Float even_mins[128] = {1.0000, 1.7602, 2.0215, 2.4306, 2.6048, 2.8370, 3.0470, 3.1977, 3.4543, 3.5591, 3.6569, 3.7880, 3.9735, 4.0996, 4.1938, 4.3286, 4.4943, 4.5897, 4.7604, 4.9058, 4.9874, 5.1579, 5.2158, 5.3455, 5.5091, 5.6440, 5.7732, 5.8934, 5.9996, 6.0390, 6.1689, 6.2921, 6.5087, 6.4396, 6.6475, 6.7895, 6.8175, 7.0568, 7.1310, 7.2227, 7.3089, 7.4514, 7.3740, 7.7176, 7.6417, 7.7408, 7.9068, 7.9023, 8.1505, 8.1101, 8.2782, 8.1577, 8.3996, 8.4774, 8.6452, 8.7317, 8.7723, 9.0306, 8.9431, 9.2227, 9.1101, 9.3700, 9.4620, 9.4925, 9.4432, 9.6274, 9.7628, 9.8611, 10.1597, 10.1279, 10.2390, 10.3369, 10.4563, 10.5145, 10.3332, 10.3364, 10.4900, 10.6633, 10.8996, 11.1033, 10.8561, 11.0184, 11.1319, 11.1943, 11.4184, 11.4124, 11.3366, 11.5377, 11.6459, 11.7827, 11.7495, 11.7728, 11.8253, 12.0501, 12.0304, 12.1450, 12.2489, 12.1674, 12.3814, 12.3327, 12.3703, 12.4228, 12.5085, 12.7726, 12.6584, 12.7670, 13.0367, 12.9740, 12.9893, 13.0990, 13.0804, 13.2401, 13.2340, 13.1151, 13.2880, 13.4806, 13.6894, 13.5589, 13.6387, 13.6890, 13.9565, 14.1125, 14.2115, 14.1692, 14.3261, 14.4166, 14.5306, 14.0186};


  Float saved_min(int ch, int nn)
  {
    switch (ch)
      {
      case ALL:   return(all_mins[nn - 1]);
      case ODD:   return(odd_mins[nn - 1]);
      case EVEN:  return(even_mins[nn - 1]);
      case PRIME: return(prime_mins[nn - 1]);
      }
    return((Float)nn);
  }

  Float get_peak(Float *phases)
  {
    int i, m;
    Float pi2, mx;

    pi2 = M_PI / 2.0;
    memset((void *)rl, 0, fft_size * sizeof(Float));
    memset((void *)im, 0, fft_size * sizeof(Float));

    for (m = 0; m < n; m++)
      {
	int bin;
	Float phi;
	phi = (M_PI * phases[m]) + pi2;
	if (choice == ALL)
	  bin = m + 1;
	else
	  {
	    if (choice == ODD)
	      bin = (m * 2) + 1;
	    else 
	      {
		if (choice == EVEN)
		  {
		    bin = m * 2;
		    if (bin == 0) bin = 1;
		  }
		else bin = primes[m];
	      }
	  }
	rl[bin] = cos(phi);
	im[bin] = sin(phi);
      }

    mus_fft(rl, im, fft_size, -1);
    ffts++;

    mx = fabs(rl[0]);
    for (i = 1; i < fft_size; i++)
      {
	Float mxtemp;
	mxtemp = fabs(rl[i]);
	if (mxtemp > mx)
	  mx = mxtemp;
      }
    return(mx);
  }
  
  pk_data *next_choice(pk_data *data)
  {
    Float *phases;
    Float cur_min, temp_min = 100000.0, pk = 100000.0;
    int len, local_try, i, k, local_tries;
    pk_data *new_pk;

    new_pk = free_choices[--free_top];
    cur_min = data->pk;
    phases = data->phases;
    len = n;
    local_tries = RETRIES + day_counter * RETRY_MULT;

    /* try to find a point nearby that is better */
    for (local_try = 0; (local_try < local_tries) && (pk >= cur_min); local_try++)
      {
	for (i = 1; i < len; i++)
	  temp_phases[i] = fmod(phases[i] + mus_random(increment), 2.0); /* not mus_frandom! */
	pk = get_peak(temp_phases);
	
	if (pk < temp_min)
	  {
	    temp_min = pk;
	    new_pk->pk = pk;
	    for (k = 1; k < len; k++) new_pk->phases[k] = temp_phases[k];	    
	  }
      }
    
    /* if a better point is found, try to follow the slopes */
    if (new_pk->pk < data->pk)
      {
	bool happy = true;
	for (k = 1; k < len; k++)
	  diff_phases[k] = new_pk->phases[k] - data->phases[k];

	while (happy)
	  {
	    for (k = 1; k < len; k++)
	      temp_phases[k] = fmod(new_pk->phases[k] + diff_phases[k], 2.0); /* frandom here? */
	    pk = get_peak(temp_phases);

	    if (pk < new_pk->pk)
	      {
		new_pk->pk = pk;
		for (k = 1; k < len; k++) new_pk->phases[k] = temp_phases[k];
	      }
	    else happy = false;
	  }
      }

    pk = new_pk->pk;

    if (pk < local_best)
      {
	local_best = pk;
	if ((!just_best) ||
	    (pk < overall_min))
	  {
	    FILE *ofile;
	    if (file)
	      ofile = fopen(file, "a");
	    else ofile = stderr;
	    for (k = 1; k < len; k++) min_phases[k] = new_pk->phases[k];
	    fprintf(ofile, "[%d, %d, %f, %lld]: %s, %d %f #(", years, days, increment, ffts, choice_name[choice], n, pk);
	    for (k = 0; k < len - 1; k++) fprintf(ofile, "%f ", min_phases[k]);
	    fprintf(ofile, "%f)\n\n", min_phases[len - 1]);
	    if (file) fclose(ofile);
	    if (pk < overall_min) overall_min = pk;
	  }

	day_counter = 0;
      }
    return(new_pk);
  }

  bool day(void)
  {
    int i, j = 0, k, len;
    Float sum = 0.0, avg;
    len = size;
    day_counter++;
    days++;
    for (i = 0; i < len; i++) sum += choices[i]->pk;
    avg = sum / len;

    for (i = 0; i < len; i++)
      {
	pk_data *datum;
	datum = choices[i];
	choices[i] = NULL;
	if (datum->pk < avg)
	  choices[j++] = datum;
	else free_choices[free_top++] = datum;
      }

    for (i = 0, k = j; k < len; i++, k++)
      {
	if (i == j)
	  i = 0;
	choices[k] = next_choice(choices[i]);
      }

    if (day_counter < counts)
      {
	increment *= incr_mult;
	if (increment < INCR_MIN) 
	  {
	    increment = INCR_MIN;
	    /* incr_mult = INCR_UP; */ /* this doesn't seem to gain us anything */
	  }
	if (increment > INCR_MAX)
	  {
	    increment = INCR_MAX;
	    incr_mult = INCR_DOWN;
	  }
	return(true);
      }
    return(false);
  }

#if HAVE_SYS_TIME_H
  {
    struct timeval tm;
    struct timezone tz;
    gettimeofday(&tm, &tz);
    mus_set_rand_seed((unsigned long)(tm.tv_sec * 1000 + tm.tv_usec / 1000));
  }
#endif

  choice = XEN_TO_C_INT(x_choice);
  n = XEN_TO_C_INT(x_n);

  if (XEN_INTEGER_P(x_size))
    size = XEN_TO_C_INT(x_size);
  else size = 1000;

  if (XEN_INTEGER_P(x_counts))
    counts = XEN_TO_C_INT(x_counts);
  else counts = 50;

  if (XEN_DOUBLE_P(x_increment))
    increment = XEN_TO_C_DOUBLE(x_increment);
  else increment = INCR_MAX;
  orig_incr = increment;
  incr_mult = INCR_DOWN;

  if (XEN_STRING_P(x_file))
    file = XEN_TO_C_STRING(x_file);

  if (XEN_BOOLEAN_P(x_just_best))
    just_best = XEN_TO_C_BOOLEAN(x_just_best);

  min_phases = (Float *)calloc(n, sizeof(Float));
  overall_min = saved_min(choice, n);

  temp_phases = (Float *)calloc(n, sizeof(Float));
  diff_phases = (Float *)calloc(n, sizeof(Float));

  {
    int start, n1;

    if (choice == ALL)
      n1 = n;
    else
      {
	if (choice != PRIME)
	  n1 = n * 2;
	else n1 = primes[n];
      }
    fft_size = (int)pow(2.0, (int)ceil(log(FFT_MULT * n1) / log(2.0)));
    rl = (Float *)calloc(fft_size, sizeof(Float));
    im = (Float *)calloc(fft_size, sizeof(Float));

    choices = (pk_data **)calloc(size, sizeof(pk_data *));
    free_choices = (pk_data **)calloc(size, sizeof(pk_data *));

    for (start = 0; start < size; start++)
      {
	choices[start] = (pk_data *)calloc(1, sizeof(pk_data));
	choices[start]->phases = (Float *)calloc(n, sizeof(Float));
      }

    while (true)
      {
	free_top = 0;
	day_counter = 0;
	days = 0;
	local_best = (Float)n;
	increment = orig_incr;

	for (start = 0; start < size; start++)
	  {
	    Float pk, local_pk = 100000.0;
	    int k, init_try;

	    for (init_try = 0;  init_try < INIT_TRIES; init_try++)
	      {
		for (k = 1; k < n; k++) temp_phases[k] = mus_frandom(2.0);
		pk = get_peak(temp_phases);

		if (pk < local_best)
		  {
		    local_best = pk;
		    if ((!just_best) ||
			(pk < overall_min))
		      {
			FILE *ofile;
			if (file)
			  ofile = fopen(file, "a");
			else ofile = stderr;
			for (k = 1; k < n; k++) min_phases[k] = temp_phases[k];
			fprintf(ofile, "[%d, %d, %f, %lld]: %s, %d %f #(", years, days, increment, ffts, choice_name[choice], n, pk);
			for (k = 0; k < n - 1; k++) fprintf(ofile, "%f ", min_phases[k]);
			fprintf(ofile, "%f)\n\n", min_phases[n - 1]);
			if (file) fclose(ofile);
			if (pk < overall_min) overall_min = pk;
		      }
		  }

		if (pk < local_pk)
		  {
		    for (k = 1; k < n; k++) choices[start]->phases[k] = temp_phases[k];
		    choices[start]->pk = pk;
		    local_pk = pk;
		  }
	      }
	  }

	while (day()) {}

	/* try again from the top... */
	if (!file) fprintf(stderr, "[%d: %d, %f]\n", years, days, local_best);
	years++;
      }
  }
  return(XEN_FALSE);
}


#endif



#ifdef XEN_ARGIFY_1
XEN_ARGIFY_6(g_scan_chan_w, g_scan_chan)
XEN_ARGIFY_7(g_map_chan_w, g_map_chan)
XEN_ARGIFY_6(g_scan_channel_w, g_scan_channel)
XEN_ARGIFY_7(g_map_channel_w, g_map_channel)
XEN_ARGIFY_5(g_find_channel_w, g_find_channel)
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
XEN_ARGIFY_7(g_env_channel_with_base_w, g_env_channel_with_base)
XEN_ARGIFY_7(g_ramp_channel_w, g_ramp_channel)
XEN_ARGIFY_8(g_xramp_channel_w, g_xramp_channel)
XEN_ARGIFY_3(g_fft_w, g_fft)
XEN_ARGIFY_7(g_snd_spectrum_w, g_snd_spectrum)
XEN_ARGIFY_5(g_convolve_with_w, g_convolve_with)
XEN_ARGIFY_2(g_convolve_selection_with_w, g_convolve_selection_with)
XEN_ARGIFY_5(g_src_sound_w, g_src_sound)
XEN_ARGIFY_2(g_src_selection_w, g_src_selection)
XEN_ARGIFY_6(g_src_channel_w, g_src_channel)
XEN_ARGIFY_5(g_pad_channel_w, g_pad_channel)
XEN_ARGIFY_9(g_filter_channel_w, g_filter_channel)
XEN_ARGIFY_6(g_filter_sound_w, g_filter_sound)
XEN_ARGIFY_3(g_filter_selection_w, g_filter_selection)
XEN_ARGIFY_8(g_clm_channel_w, g_clm_channel)
XEN_NARGIFY_0(g_sinc_width_w, g_sinc_width)
XEN_NARGIFY_1(g_set_sinc_width_w, g_set_sinc_width)
XEN_ARGIFY_9(g_ptree_channel_w, g_ptree_channel)
#if HAVE_NESTED_FUNCTIONS
XEN_VARGIFY(g_find_min_peak_phases_w, g_find_min_peak_phases)
XEN_ARGIFY_7(g_find_min_peak_phases_via_sa_w, g_find_min_peak_phases_via_sa)
#endif
#else
#define g_scan_chan_w g_scan_chan
#define g_map_chan_w g_map_chan
#define g_scan_channel_w g_scan_channel
#define g_map_channel_w g_map_channel
#define g_find_channel_w g_find_channel
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
#define g_env_channel_with_base_w g_env_channel_with_base
#define g_ramp_channel_w g_ramp_channel
#define g_xramp_channel_w g_xramp_channel
#define g_fft_w g_fft
#define g_snd_spectrum_w g_snd_spectrum
#define g_convolve_with_w g_convolve_with
#define g_convolve_selection_with_w g_convolve_selection_with
#define g_src_sound_w g_src_sound
#define g_src_selection_w g_src_selection
#define g_src_channel_w g_src_channel
#define g_pad_channel_w g_pad_channel
#define g_filter_channel_w g_filter_channel
#define g_filter_sound_w g_filter_sound
#define g_filter_selection_w g_filter_selection
#define g_clm_channel_w g_clm_channel
#define g_sinc_width_w g_sinc_width
#define g_set_sinc_width_w g_set_sinc_width
#define g_ptree_channel_w g_ptree_channel
#if HAVE_NESTED_FUNCTIONS
#define g_find_min_peak_phases_w g_find_min_peak_phases
#define g_find_min_peak_phases_via_sa_w g_find_min_peak_phases_via_sa
#endif
#endif

void g_init_sig(void)
{
#if WITH_RUN
#if HAVE_GUILE
  XEN_EVAL_C_STRING("(use-modules (ice-9 optargs))");
#endif

#if (!HAVE_S7)
  XEN_DEFINE_PROCEDURE(S_scan_channel "-1",       g_scan_channel_w,  1, 5, 0, H_scan_channel);
  XEN_DEFINE_PROCEDURE(S_scan_chan "-1",          g_scan_chan_w,     1, 5, 0, H_scan_chan);
  XEN_DEFINE_PROCEDURE(S_find_channel "-1",       g_find_channel_w,  1, 4, 0, H_find_channel);
  XEN_DEFINE_PROCEDURE(S_count_matches "-1",      g_count_matches_w, 1, 4, 0, H_count_matches);
  XEN_DEFINE_PROCEDURE(S_map_chan "-1",           g_map_chan_w,      1, 6, 0, H_map_chan);
  XEN_DEFINE_PROCEDURE(S_map_channel "-1",        g_map_channel_w,   1, 6, 0, H_map_channel);
  XEN_DEFINE_PROCEDURE(S_ptree_channel "-1",      g_ptree_channel_w, 1, 8, 0, H_ptree_channel);

  XEN_EVAL_C_STRING("(defmacro scan-channel (form . args) `(apply scan-channel-1 (list (list ',form ,form) ,@args)))");
  XEN_SET_DOCUMENTATION(S_scan_channel, H_scan_channel);
  XEN_EVAL_C_STRING("(defmacro scan-chan (form . args) `(apply scan-chan-1 (list (list ',form ,form) ,@args)))");
  XEN_SET_DOCUMENTATION(S_scan_chan, H_scan_chan);
  XEN_EVAL_C_STRING("(defmacro find-channel (form . args) `(apply find-channel-1 (list (list ',form ,form) ,@args)))");
  XEN_SET_DOCUMENTATION(S_find_channel, H_find_channel);
  XEN_EVAL_C_STRING("(defmacro count-matches (form . args) `(apply count-matches-1 (list (list ',form ,form) ,@args)))");
  XEN_SET_DOCUMENTATION(S_count_matches, H_count_matches);
  XEN_EVAL_C_STRING("(defmacro map-channel (form . args) `(apply map-channel-1 (list (list ',form ,form) ,@args)))");
  XEN_SET_DOCUMENTATION(S_map_channel, H_map_channel);
  XEN_EVAL_C_STRING("(defmacro map-chan (form . args) `(apply map-chan-1 (list (list ',form ,form) ,@args)))");
  XEN_SET_DOCUMENTATION(S_map_chan, H_map_chan);
  XEN_EVAL_C_STRING("(defmacro ptree-channel (form . args) `(apply ptree-channel-1 (list (list ',form ,form) ,@args)))");
  XEN_SET_DOCUMENTATION(S_ptree_channel, H_ptree_channel);
#else
  XEN_DEFINE_PROCEDURE(S_scan_channel,            g_scan_channel_w,            1, 5, 0, H_scan_channel);
  XEN_DEFINE_PROCEDURE(S_scan_chan,               g_scan_chan_w,               1, 5, 0, H_scan_chan);
  XEN_DEFINE_PROCEDURE(S_find_channel,            g_find_channel_w,            1, 4, 0, H_find_channel);
  XEN_DEFINE_PROCEDURE(S_count_matches,           g_count_matches_w,           1, 4, 0, H_count_matches);
  XEN_DEFINE_PROCEDURE(S_map_chan,                g_map_chan_w,                1, 6, 0, H_map_chan);
  XEN_DEFINE_PROCEDURE(S_map_channel,             g_map_channel_w,             1, 6, 0, H_map_channel);
  XEN_DEFINE_PROCEDURE(S_ptree_channel,           g_ptree_channel_w,           1, 8, 0, H_ptree_channel);
#endif

#else
  /* not with run */
  XEN_DEFINE_PROCEDURE(S_scan_channel,            g_scan_channel_w,            1, 5, 0, H_scan_channel);
  XEN_DEFINE_PROCEDURE(S_scan_chan,               g_scan_chan_w,               1, 5, 0, H_scan_chan);
  XEN_DEFINE_PROCEDURE(S_find_channel,            g_find_channel_w,            1, 4, 0, H_find_channel);
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
  XEN_DEFINE_PROCEDURE(S_convolve_with,           g_convolve_with_w,           1, 4, 0, H_convolve_with);
  XEN_DEFINE_PROCEDURE(S_convolve_selection_with, g_convolve_selection_with_w, 1, 1, 0, H_convolve_selection_with);
  XEN_DEFINE_PROCEDURE(S_src_sound,               g_src_sound_w,               1, 4, 0, H_src_sound);
  XEN_DEFINE_PROCEDURE(S_src_selection,           g_src_selection_w,           1, 1, 0, H_src_selection);
  XEN_DEFINE_PROCEDURE(S_filter_channel,          g_filter_channel_w,          1, 8, 0, H_filter_channel);
  XEN_DEFINE_PROCEDURE(S_filter_sound,            g_filter_sound_w,            1, 5, 0, H_filter_sound);
  XEN_DEFINE_PROCEDURE(S_filter_selection,        g_filter_selection_w,        1, 2, 0, H_filter_selection);

  XEN_DEFINE_PROCEDURE(S_reverse_channel,         g_reverse_channel_w,         0, 5, 0, H_reverse_channel);
  XEN_DEFINE_PROCEDURE(S_clm_channel,             g_clm_channel_w,             1, 7, 0, H_clm_channel);
  XEN_DEFINE_PROCEDURE(S_env_channel,             g_env_channel_w,             1, 5, 0, H_env_channel);
  XEN_DEFINE_PROCEDURE(S_env_channel_with_base,   g_env_channel_with_base_w,   1, 6, 0, H_env_channel_with_base);
  XEN_DEFINE_PROCEDURE(S_ramp_channel,            g_ramp_channel_w,            2, 5, 0, H_ramp_channel);
  XEN_DEFINE_PROCEDURE(S_xramp_channel,           g_xramp_channel_w,           2, 6, 0, H_xramp_channel);
  XEN_DEFINE_PROCEDURE(S_smooth_channel,          g_smooth_channel_w,          0, 5, 0, H_smooth_channel);
  XEN_DEFINE_PROCEDURE(S_src_channel,             g_src_channel_w,             1, 5, 0, H_src_channel);
  XEN_DEFINE_PROCEDURE(S_pad_channel,             g_pad_channel_w,             2, 3, 0, H_pad_channel);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_sinc_width, g_sinc_width_w, H_sinc_width,
				   S_setB S_sinc_width, g_set_sinc_width_w,  0, 0, 1, 0);
#if HAVE_NESTED_FUNCTIONS
  XEN_DEFINE_PROCEDURE(S_find_min_peak_phases, g_find_min_peak_phases_w, 0, 0, 1, H_find_min_peak_phases);
  XEN_DEFINE_PROCEDURE(S_find_min_peak_phases_via_sa, g_find_min_peak_phases_via_sa_w, 2, 5, 0, H_find_min_peak_phases_via_sa);
#endif
}

