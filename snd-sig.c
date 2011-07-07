#include "snd.h"
#include "clm2xen.h"
#include "clm-strings.h"


/* collect syncd chans */
typedef struct {
  sync_info *si;
  snd_fd **sfs;
  mus_long_t dur;
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
					    C_INT_TO_XEN_SOUND(cp->sound->index), 
					    C_TO_XEN_INT(cp->chan),
					    caller),
				 AT_CURRENT_EDIT_POSITION);
      if (cp->active < CHANNEL_HAS_EDIT_LIST) /* edpos proc clobbered channel somehow... */
	XEN_ERROR(NO_SUCH_CHANNEL,
		  XEN_LIST_3(C_TO_XEN_STRING("~A: edpos arg (~A) clobbered the current sound!"),
			     C_TO_XEN_STRING(caller),
			     edpos));
    }
  else pos = XEN_TO_C_INT_OR_ELSE(edpos, AT_CURRENT_EDIT_POSITION);

  if (pos == AT_CURRENT_EDIT_POSITION)
    return(cp->edit_ctr);

  if ((pos < 0) || 
      (pos >= cp->edit_size) ||
      (cp->edits[pos] == NULL))
    XEN_ERROR(NO_SUCH_EDIT,
	      XEN_LIST_8(C_TO_XEN_STRING("~A: no such edpos: ~A (from ~A), sound index: ~A (~S), chan: ~A, current edit: ~A"),
			 C_TO_XEN_STRING(caller),
			 C_TO_XEN_INT(pos),
			 edpos,
			 C_INT_TO_XEN_SOUND(cp->sound->index),
			 C_TO_XEN_STRING(cp->sound->short_filename),
			 C_TO_XEN_INT(cp->chan),
			 C_TO_XEN_INT(cp->edit_ctr)));
  return(pos);
}


mus_long_t to_c_edit_samples(chan_info *cp, XEN edpos, const char *caller, int arg_pos)
{
  return(cp->edits[to_c_edit_position(cp, edpos, caller, arg_pos)]->samples);
}


mus_long_t beg_to_sample(XEN beg, const char *caller)
{
  mus_long_t start;
  start = XEN_TO_C_INT64_T_OR_ELSE(beg, 0);
  if (start < 0) 
    XEN_ERROR(NO_SUCH_SAMPLE,
	      XEN_LIST_3(C_TO_XEN_STRING("~A: no such sample: ~A"),
			 C_TO_XEN_STRING(caller),
			 beg));
  return(start);
}


mus_long_t dur_to_samples(XEN dur, mus_long_t beg, chan_info *cp, int edpos, int argn, const char *caller)
{
  mus_long_t samps;
  samps = XEN_TO_C_INT64_T_OR_ELSE(dur, cp->edits[edpos]->samples - beg);
  if (samps < 0)
    XEN_WRONG_TYPE_ARG_ERROR(caller, argn, dur, "a positive integer");
  return(samps);
}


static mus_long_t end_to_sample(XEN end, chan_info *cp, int edpos, const char *caller)
{
  mus_long_t last;
  last = XEN_TO_C_INT64_T_OR_ELSE(end, cp->edits[edpos]->samples - 1);
  if (last < 0) 
    XEN_ERROR(NO_SUCH_SAMPLE,
	      XEN_LIST_3(C_TO_XEN_STRING("~A: no such sample: ~A"),
			 C_TO_XEN_STRING(caller),
			 end));
  return(last);
}


static sync_state *get_sync_state_1(snd_info *sp, chan_info *cp, mus_long_t beg, bool over_selection, 
				    read_direction_t forwards, mus_long_t prebeg, XEN edpos, const char *caller, int arg_pos)
{
  /* can return NULL if over_selection and no current selection */
  sync_info *si = NULL;
  snd_fd **sfs = NULL;
  chan_info *ncp;
  int i, pos;
  mus_long_t dur = 0, pbeg;
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
	      snd_warning_without_format("no selection");
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


static sync_state *get_sync_state(snd_info *sp, chan_info *cp, mus_long_t beg, bool over_selection, 
				  read_direction_t forwards, XEN edpos, const char *caller, int arg_pos)
{
  return(get_sync_state_1(sp, cp, beg, over_selection, forwards, 0, edpos, caller, arg_pos));
}


static sync_state *get_sync_state_without_snd_fds(snd_info *sp, chan_info *cp, mus_long_t beg, bool over_selection)
{
  sync_info *si = NULL;
  mus_long_t dur = 0;
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
	      snd_warning_without_format("no selection");
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


static char *convolve_with_or_error(char *filename, mus_float_t amp, chan_info *cp, XEN edpos, int arg_pos)
{
  /* if string returned, needs to be freed */
  /* amp == 0.0 means unnormalized, cp == NULL means current selection */
  sync_state *sc;
  sync_info *si;
  snd_info *sp = NULL, *gsp = NULL;
  int ip, stop_point = 0, impulse_chan = 0, filter_chans, dataformat;
  bool ok = false;
  mus_long_t filtersize = 0, filesize = 0, dataloc, fftsize;
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
    return(mus_format("convolve: impulse response file %s chans: %d", filename, filter_chans));

  filtersize = mus_sound_samples(filename) / filter_chans;
  if (filtersize <= 0) 
    return(mus_format("convolve: impulse response file %s is empty", filename));
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
	      return(mus_format("convolve: save chan (%s[%d]) in %s hit error: %s\n",
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
		  return(mus_format("convolve: open saved chan (%s[%d]) file %s hit error: %s\n",
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
		      return(mus_format("convolve: open filter file %s hit error: %s\n", 
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
			  fftsize = snd_mus_long_t_pow2(ipow);
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
			  return(mus_format("convolve: close filter file %s hit error: %s\n", 
					    filename, snd_io_strerror()));
			}
		    }
		}
	      if (mus_file_close(scfd) != 0)
		{
		  if (ofile) free(ofile);
		  free_sync_state(sc);
		  return(mus_format("convolve: close saved chan (%s[%d]) file %s hit error: %s\n",
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

void scale_by(chan_info *cp, mus_float_t *ur_scalers, int len, bool over_selection)
{
  /* if over_selection, sync to current selection, else sync to current sound */
  /* 3-Oct-00: the scale factors are now embedded in the edit fragments  */
  sync_info *si;
  int i, j;

  if (over_selection)
    si = selection_sync();
  else si = sync_to_chan(cp);
  if (!si) return;

  for (i = 0, j = 0; i < si->chans; i++)
    {
      mus_long_t beg, frames;
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


bool scale_to(snd_info *sp, chan_info *cp, mus_float_t *ur_scalers, int len, bool over_selection)
{
  /* essentially the same as scale-by, but first take into account current maxamps */
  /* here it matters if more than one arg is given -- if one, get overall maxamp */
  /*   if more than one, get successive maxamps */
  bool scaled = false;
  int i, chans, nlen, datum_size;
  mus_long_t beg, frames;
  sync_info *si = NULL;
  chan_info *ncp;
  mus_float_t maxamp = -1.0, val, norm = 1.0;
  mus_float_t *scalers;
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
  scalers = (mus_float_t *)calloc(chans, sizeof(mus_float_t));
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
	      origin = mus_format("%.3f" PROC_SEP MUS_LD PROC_SEP MUS_LD " %s", norm, beg, frames, S_normalize_channel);
#else
	      origin = mus_format("%s" PROC_OPEN "%.3f" PROC_SEP MUS_LD PROC_SEP MUS_LD, TO_PROC_NAME(S_normalize_channel), norm, beg, frames);
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


static void swap_channels(chan_info *cp0, chan_info *cp1, mus_long_t beg, mus_long_t dur, int pos0, int pos1)
{
  snd_fd *c0, *c1;
  snd_info *sp0;
  file_info *hdr0 = NULL, *hdr1 = NULL;
  int j, ofd0 = 0, ofd1 = 0, datumb = 0;
  bool temp_file;
  mus_long_t k;
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
	  snd_error("%s " S_swap_channels " temp file %s: %s\n", 
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
	  snd_error("%s " S_swap_channels " temp file %s: %s\n", 
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
	      if (err != MUS_NO_ERROR) break;
	      if (reporting) 
		{
		  progress_report(cp0, (mus_float_t)((double)k / (double)dur));
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
	  string_to_minibuffer(sp0, "swap interrupted");
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
  mus_long_t sample;
  int dir;
} src_state;

static mus_float_t src_input_as_needed(void *arg, int direction) 
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


static src_state *make_src(mus_float_t srate, snd_fd *sf)
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


static int mus_long_t_compare(const void *a, const void *b)
{
  mus_long_t *m1, *m2;
  m1 = (mus_long_t *)a;
  m2 = (mus_long_t *)b;
  if (*m1 < *m2) return(-1);
  if (*m1 == *m2) return(0);
  return(1);
}

static char *reverse_channel(chan_info *cp, snd_fd *sf, mus_long_t beg, mus_long_t dur, XEN edp, const char *caller, int arg_pos);


static char *src_channel_with_error(chan_info *cp, snd_fd *sf, mus_long_t beg, mus_long_t dur, mus_float_t ratio, mus_any *egen, 
				    const char *origin, bool over_selection,
				    bool *clm_err)
{
  snd_info *sp = NULL;
  bool reporting = false;
  bool full_chan;
  mus_sample_t **data;
  file_info *hdr = NULL;
  int j, ofd = 0, datumb = 0, err = 0;
  mus_long_t *old_marks = NULL, *new_marks = NULL;
  int cur_marks = 0, m;
  mus_long_t k;
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
      return(mus_format("%s %s temp file %s: %s\n", 
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
		  if (err != MUS_NO_ERROR) break;
		  if (reporting) 
		    {
		      progress_report(cp, (mus_float_t)((double)(sr->sample) / (double)dur));
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
		  if (err != MUS_NO_ERROR) break;
		  if (reporting) 
		    {
		      progress_report(cp, (mus_float_t)((double)(sr->sample) / (double)dur));
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
      mus_long_t next_pass;
      mus_long_t cur_mark_sample = -1;
      int cur_mark = 0, cur_new_mark = 0;
      mus_float_t env_val;

      cur_mark_sample = -1;
      env_val = mus_env(egen); 
      /* envelope case -- have to go by sr->sample, not output sample counter, also check marks */

      if ((cp->edits[cp->edit_ctr]->marks) && 
	  (cp->edits[cp->edit_ctr]->mark_ctr >= 0))
	{
	  mark **mps;
	  mps = cp->edits[cp->edit_ctr]->marks;
	  cur_marks = cp->edits[cp->edit_ctr]->mark_ctr + 1;
	  new_marks = (mus_long_t *)malloc(cur_marks * sizeof(mus_long_t));
	  old_marks = (mus_long_t *)malloc(cur_marks * sizeof(mus_long_t));
	  for (m = 0; m < cur_marks; m++)
	    {
	      mus_long_t pos;
	      pos = mark_sample(mps[m]);
	      new_marks[m] = -1;
	      if ((env_val >= 0.0) ||
		  (pos < beg) ||
		  (pos > (beg + dur)))
		old_marks[m] = pos;
	      else 
		{
		  old_marks[m] = (dur - pos - 1) + beg; /* moving backwards, so flip marks */
		  cur_new_mark = m;
		}
	    }
	  if ((env_val < 0.0) && (cur_marks > 1))
	    qsort((void *)old_marks, cur_marks, sizeof(mus_long_t), mus_long_t_compare);
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
	      if (err != MUS_NO_ERROR) break;
	      if (reporting) 
		{
		  progress_report(cp, (mus_float_t)((double)(sr->sample) / (double)dur));
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
	      mus_long_t jj, idiff;
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
	    new_origin = mus_format("%.4f" PROC_SEP MUS_LD PROC_SEP PROC_FALSE " %s", ratio, beg, S_src_channel);
	  else new_origin = mus_format("%.4f" PROC_SEP MUS_LD PROC_SEP MUS_LD " %s", ratio, beg, dur, S_src_channel);
#else
	  if (dur == cp->edits[sf->edit_ctr]->samples)
	    new_origin = mus_format("%s" PROC_OPEN "%.4f" PROC_SEP MUS_LD PROC_SEP PROC_FALSE, TO_PROC_NAME(S_src_channel), ratio, beg);
	  else new_origin = mus_format("%s" PROC_OPEN "%.4f" PROC_SEP MUS_LD PROC_SEP MUS_LD, TO_PROC_NAME(S_src_channel), ratio, beg, dur);
#endif
	}
      else
	{
	  mus_float_t base;
	  char *envstr;
	  env *newe;
	  base = mus_increment(egen);
	  newe = make_envelope_with_offset_and_scaler(mus_data(egen), mus_env_breakpoints(egen) * 2, mus_offset(egen), mus_scaler(egen));
	  envstr = env_to_string(newe);

#if HAVE_FORTH
	  if (base == 1.0)
	    {
	      if (dur == cp->edits[sf->edit_ctr]->samples)
		new_origin = mus_format("%s" PROC_SEP MUS_LD PROC_SEP PROC_FALSE " %s", envstr, beg, S_src_channel);
	      else new_origin = mus_format("%s" PROC_SEP MUS_LD PROC_SEP MUS_LD " %s", envstr, beg, dur, S_src_channel);
	    }
	  else new_origin = mus_format("%s :base %.4f :end " MUS_LD " %s " MUS_LD PROC_SEP MUS_LD " %s", envstr, base, dur, S_make_env, beg, dur, S_src_channel);
#else
	  if (base == 1.0)
	    {
	      if (dur == cp->edits[sf->edit_ctr]->samples)
		new_origin = mus_format("%s" PROC_OPEN "%s" PROC_SEP MUS_LD PROC_SEP PROC_FALSE, TO_PROC_NAME(S_src_channel), envstr, beg);
	      else new_origin = mus_format("%s" PROC_OPEN "%s" PROC_SEP MUS_LD PROC_SEP MUS_LD, TO_PROC_NAME(S_src_channel), envstr, beg, dur);
	    }
	  else new_origin = mus_format("%s" PROC_OPEN BPAREN "%s" PROC_OPEN "%s" PROC_SEP ":base" PROC_SEP "%.4f" PROC_SEP ":end" PROC_SEP MUS_LD ")" PROC_SEP MUS_LD PROC_SEP MUS_LD, 
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
	      mus_float_t bratio;
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
      string_to_minibuffer(sp, "src interrupted");
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


void src_env_or_num(chan_info *cp, env *e, mus_float_t ratio, bool just_num, 
		    const char *origin, bool over_selection, mus_any *gen, XEN edpos, int arg_pos)
{
  snd_info *sp = NULL;
  sync_state *sc;
  sync_info *si;
  snd_fd **sfs;
  int i;
  mus_long_t scdur;
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
	  mus_long_t dur;
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


static mus_float_t input_as_needed(void *arg, int dir) 
{
  return(mus_readin((mus_any *)arg));
}


void src_file(const char *file, double ratio)
{
  mus_any **rds, **srcs;
  char *temp_out;
  const char *comment;
  int k, chan, chans, width, out_fd, data_format, header_type, buffer_size;
  mus_long_t samp, old_samps, new_samps;
  mus_float_t old_srate, new_srate;
  mus_sample_t **obufs;

  old_srate = mus_srate();
  new_srate = mus_sound_srate(file); /* need have no connection with previous CLM srate setting */
  mus_set_srate(new_srate);

  chans = mus_sound_chans(file);
  data_format = mus_sound_data_format(file);
  header_type = mus_sound_header_type(file);
  comment = mus_sound_comment(file);
  buffer_size = mus_file_buffer_size();
  old_samps = mus_sound_frames(file);
  new_samps = old_samps / ratio;  /* old-srate/new-srate in-coming */

  width = sinc_width(ss);
  if (width < 32) width = 32;

  temp_out = snd_tempnam();
  out_fd = mus_sound_open_output(temp_out, new_srate, chans, data_format, header_type, comment);

  srcs = (mus_any **)malloc(chans * sizeof(mus_any *));
  rds = (mus_any **)malloc(chans * sizeof(mus_any *));
  obufs = (mus_sample_t **)malloc(chans * sizeof(mus_sample_t *));

  for (chan = 0; chan < chans; chan++)
    {
      rds[chan] = mus_make_readin(file, chan, 0, 1);
      srcs[chan] = mus_make_src(NULL, ratio, width, (void *)rds[chan]);
      obufs[chan] = (mus_sample_t *)malloc(buffer_size * sizeof(mus_sample_t));
    }

  for (k = 0, samp = 0; samp < new_samps; samp++)
    {
      for (chan = 0; chan < chans; chan++)
	obufs[chan][k] = MUS_FLOAT_TO_SAMPLE(mus_src(srcs[chan], 0.0, &input_as_needed));
      k++;
      if (k == buffer_size)
	{
	  mus_sound_write(out_fd, 0, buffer_size - 1, chans, obufs);
	  k = 0;
	}
    }
  if (k > 0) 
    mus_sound_write(out_fd, 0, k - 1, chans, obufs);

  mus_sound_close_output(out_fd, new_samps * chans * mus_bytes_per_sample(data_format));
  mus_sound_forget(file);

  for (chan = 0; chan < chans; chan++)
    {
      free(obufs[chan]);
      mus_free(srcs[chan]);
      mus_free(rds[chan]);
    }
  free(obufs);
  free(srcs);
  free(rds);

  move_file(temp_out, file);
  free(temp_out);
  mus_set_srate(old_srate);
}


/* FIR filtering */

static mus_float_t *get_filter_coeffs(int order, env *e)
{
  /* interpret e as frequency response */
  mus_float_t *a = NULL, *fdata;
  if (!e) return(NULL);

  /* get the frequency envelope and design the FIR filter */
  fdata = sample_linear_env(e, order);
  if (!fdata) return(NULL);
  a = (mus_float_t *)calloc(order, sizeof(mus_float_t));

  mus_make_fir_coeffs(order, fdata, a);

  free(fdata);
  return(a);
}


void display_frequency_response(env *e, axis_info *ap, graphics_context *gax, int order, bool dBing)
{
  /* not cp->min_dB here -- this is sound panel related which refers to ss->min_dB */
  mus_float_t *coeffs = NULL;
  int height, width, i, pts, x1, y1;
  mus_float_t samps_per_pixel, invpts, resp, pix;
  int fsize, j;
  mus_float_t step, fx;
  mus_float_t *rl, *im;

  if (order & 1) order++;

  height = (ap->y_axis_y1 - ap->y_axis_y0);
  width = (ap->x_axis_x1 - ap->x_axis_x0);
  pts = order * 4;
  if (pts > width) pts = width;
  if (pts <= 0) pts = 1;
  invpts = 1.0 / (mus_float_t)pts;
  samps_per_pixel = (mus_float_t)(ap->x_axis_x1 - ap->x_axis_x0) * invpts;

  coeffs = get_filter_coeffs(order, e);
  if (!coeffs) return;

  fsize = 2 * snd_to_int_pow2((pts > order) ? pts : order); /* *2 for 1/2 frqs */
  rl = (mus_float_t *)calloc(fsize, sizeof(mus_float_t));
  im = (mus_float_t *)calloc(fsize, sizeof(mus_float_t));
  for (i = 0, j = order - 1; i < order / 2; i++, j -= 2) rl[j] = coeffs[i]; /* by 2 from 1 for 1/2 bins */

  mus_fft(rl, im, fsize, -1);

  resp = 2 * rl[0];
  if (dBing)
    y1 = (int)(ap->y_axis_y0 + (min_dB(ss) - in_dB(min_dB(ss), ss->lin_dB, resp)) * height / min_dB(ss));
  else y1 = (int)(ap->y_axis_y0 + resp * height);
  x1 = ap->x_axis_x0;
  step = (mus_float_t)(fsize - 1) / (4 * (mus_float_t)pts); /* fsize-1 since we got 1 already, *4 due to double size fft */
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


static char *clm_channel(chan_info *cp, mus_any *gen, mus_long_t beg, mus_long_t dur, int edpos, mus_long_t overlap, const char *origin)
{
  /* calls gen over cp[beg for dur] data, replacing. */
  snd_info *sp;
  file_info *hdr = NULL;
  int j, ofd = 0, datumb = 0, err = 0;
  bool temp_file;
  mus_long_t k;
  mus_sample_t **data;
  mus_sample_t *idata;
  char *ofile = NULL;
  snd_fd *sf;

  if ((beg < 0) || ((dur + overlap) <= 0)) return(NULL);
  sp = cp->sound;
  if (!(editable_p(cp))) return(NULL);

  sf = init_sample_read_any(beg, cp, READ_FORWARD, edpos);
  if (sf == NULL)
    return(mus_format("%s can't read %s[%d] channel data!", S_clm_channel, sp->short_filename, cp->chan));

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
	  return(mus_format("%s %s temp file %s: %s\n", 
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
	      if (err != MUS_NO_ERROR) break;
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
	  if (err != MUS_NO_ERROR) break;
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

#define TWO_30 1073741824
#define MAX_SINGLE_FFT_SIZE 1048576

static mus_float_t convolve_next_sample(void *ptr, int dir)
{
  return(read_sample(((snd_fd *)ptr)));
}


static char *convolution_filter(chan_info *cp, int order, env *e, snd_fd *sf, mus_long_t beg, mus_long_t dur, 
				const char *origin, mus_float_t *precalculated_coeffs)
{
  snd_info *sp;
  file_info *hdr = NULL;
  int j, ofd = 0, datumb = 0, err = 0;
  char *ofile = NULL;
  int fsize;
  mus_float_t *fltdat = NULL;
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
  if (sizeof(mus_float_t) == 4)
    hdr->format = MUS_LFLOAT;
  else hdr->format = MUS_LDOUBLE;
#else
  if (sizeof(mus_float_t) == 4)
    hdr->format = MUS_BFLOAT;
  else hdr->format = MUS_BDOUBLE;
#endif

  ofd = open_temp_file(ofile, 1, hdr, &io_err);
  if (ofd == -1)
    {
      return(mus_format("%s %s temp file %s: %s\n", 
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
      mus_long_t offk;

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
	  mus_float_t x;

	  x = mus_convolve(gen, convolve_next_sample);

	  idata[j] = MUS_FLOAT_TO_SAMPLE(x);
	  j++;
	  if (j == MAX_BUFFER_SIZE)
	    {
	      err = mus_file_write(ofd, 0, j - 1, 1, data);
	      j = 0;
	      if (err != MUS_NO_ERROR) break;
	      if (reporting)
		{
		  progress_report(cp, (mus_float_t)((double)offk / (double)dur));
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
	  string_to_minibuffer(sp, "filter interrupted");
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
	  mus_float_t *sndrdat, *sndidat;
	  mus_float_t scale;
	  mus_long_t k;
	  ssize_t bytes;

	  sndrdat = (mus_float_t *)calloc(fsize, sizeof(mus_float_t));
	  sndidat = (mus_float_t *)calloc(fsize, sizeof(mus_float_t));

	  for (k = 0; k < dur; k++) 
	    sndrdat[k] = (mus_float_t)(read_sample(sf));

	  mus_fft(sndrdat, sndidat, fsize, 1);
	  scale = 1.0 / (mus_float_t)fsize;
	  for (k = 0; k < fsize; k++)
	    {
	      double scl;
	      scl = scale * fltdat[k];
	      sndrdat[k] *= scl;         /* fltdat is already reflected around midpoint */
	      sndidat[k] *= scl;
	    }
	  mus_fft(sndrdat, sndidat, fsize, -1);

	  bytes = write(ofd, sndrdat, fsize * sizeof(mus_float_t));
	  close_temp_file(ofile, ofd, hdr->type, fsize * sizeof(mus_float_t));
	  if (bytes != 0)
	    file_change_samples(beg, dur + order, ofile, cp, 0, DELETE_ME, origin, sf->edit_ctr);
	  else string_to_minibuffer(sp, "can't write data?");

	  free(sndrdat);
	  free(sndidat);
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


static char *direct_filter(chan_info *cp, int order, env *e, snd_fd *sf, mus_long_t beg, mus_long_t dur, 
			   const char *origin, bool truncate,
			   bool over_selection, mus_any *gen, mus_float_t *precalculated_coeffs)
{
  mus_float_t *a = NULL, *d = NULL;
  mus_float_t x = 0.0;
  snd_info *sp;
  bool reporting = false;
  int m;
  mus_long_t offk;
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
	  return(mus_format("%s %s temp file %s: %s\n", 
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
  d = (mus_float_t *)calloc(order, sizeof(mus_float_t));
  if (gen)
    mus_reset(gen);
  else
    {
      for (m = 0; m < order; m++) d[m] = 0.0;
      if (over_selection)
	{
	  mus_long_t prebeg = 0;
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
	      mus_float_t *ap, *dp, *dprev;

	      ap = (mus_float_t *)(a + order - 1);
	      dp = (mus_float_t *)(d + order - 1);

	      d[0] = read_sample(sf);
	      x = d[0] * a[0]; 

	      while (dp > d)
		{
		  x += (*dp) * (*ap--);
		  dprev = dp--;
		  (*dprev) = (*dp);
		}
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
	      mus_float_t *ap, *dp, *dprev;

	      ap = (mus_float_t *)(a + order - 1);
	      dp = (mus_float_t *)(d + order - 1);

	      d[0] = read_sample(sf);
	      x = d[0] * a[0]; 

	      while (dp > d)
		{
		  x += (*dp) * (*ap--);
		  dprev = dp--;
		  (*dprev) = (*dp);
		}
	    }
	  idata[j] = MUS_FLOAT_TO_SAMPLE(x);
	  j++;
	  if (j == MAX_BUFFER_SIZE)
	    {
	      err = mus_file_write(ofd, 0, j - 1, 1, data);
	      j = 0;
	      if (err != MUS_NO_ERROR) break;
	      if (reporting) 
		{
		  progress_report(cp, (mus_float_t)((double)offk / (double)dur));
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
	      mus_float_t *ap, *dp, *dprev;

	      ap = (mus_float_t *)(a + order - 1);
	      dp = (mus_float_t *)(d + order - 1);

	      d[0] = read_sample(sf);
	      x = d[0] * a[0];

	      while (dp > d)
		{
		  x += (*dp) * (*ap--);
		  dprev = dp--;
		  (*dprev) = (*dp);
		}
	    }
	  idata[j] = MUS_FLOAT_TO_SAMPLE(x + read_sample(sfold));
	  j++;
	  if ((temp_file) && (j == MAX_BUFFER_SIZE))
	    {
	      err = mus_file_write(ofd, 0, j - 1, 1, data);
	      j = 0;
	      if (err != MUS_NO_ERROR) break;
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
	    new_origin = mus_format("%s %d " MUS_LD PROC_SEP PROC_FALSE " %s", vstr, order, beg, S_filter_channel);
	  else new_origin = mus_format("%s %d " MUS_LD PROC_SEP MUS_LD " %s", vstr, order, beg, dur, S_filter_channel);
#else
	  if (dur == (order + cp->edits[sf->edit_ctr]->samples))
	    new_origin = mus_format("%s" PROC_OPEN "%s" PROC_SEP "%d" PROC_SEP MUS_LD PROC_SEP PROC_FALSE, 
				    TO_PROC_NAME(S_filter_channel), vstr, order, beg);
	  else new_origin = mus_format("%s" PROC_OPEN "%s" PROC_SEP "%d" PROC_SEP MUS_LD PROC_SEP MUS_LD, 
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
	    new_origin = mus_format("%s %d " MUS_LD PROC_SEP PROC_FALSE " %s", envstr, order, beg, S_filter_channel);
	  else new_origin = mus_format("%s %d " MUS_LD PROC_SEP MUS_LD " %s", envstr, order, beg, dur, S_filter_channel);
#else
	  if (dur == (order + cp->edits[sf->edit_ctr]->samples))
	    new_origin = mus_format("%s" PROC_OPEN "%s" PROC_SEP "%d" PROC_SEP MUS_LD PROC_SEP PROC_FALSE, 
				    TO_PROC_NAME(S_filter_channel), envstr, order, beg);
	  else new_origin = mus_format("%s" PROC_OPEN "%s" PROC_SEP "%d" PROC_SEP MUS_LD PROC_SEP MUS_LD, 
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


static char *filter_channel(chan_info *cp, int order, env *e, mus_long_t beg, mus_long_t dur, int edpos, const char *origin, bool truncate, mus_float_t *coeffs)
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

  if ((!over_selection) &&
      ((order == 0) || (order >= 128)))
    errstr = convolution_filter(cp, order, e, sf, beg, dur, origin, coeffs);
  else  errstr = direct_filter(cp, order, e, sf, beg, dur, origin, truncate, over_selection, NULL, coeffs);

  free_snd_fd(sf);
  return(errstr);
}


static char *apply_filter_or_error(chan_info *ncp, int order, env *e, 
				   const char *caller, const char *origin, bool over_selection, mus_float_t *ur_a, 
				   mus_any *gen, XEN edpos, int arg_pos, bool truncate, bool *clm_error)
{
  /* if string returned, needs to be freed */
  /* interpret e as frequency response and apply as filter to all sync'd chans */
  mus_float_t *a = NULL;
  sync_state *sc;
  sync_info *si;
  snd_info *sp;
  int i, stop_point = 0;
  mus_long_t scdur, dur;
  snd_fd **sfs;
  chan_info *cp;
  char *errstr = NULL;

  if ((!e) && (!ur_a) && (!gen)) 
    return(NULL);

  if ((gen) && (!(MUS_RUN_P(gen))))
    {
      (*clm_error) = true;
      return(mus_format("%s: can't handle %s generators",
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
      string_to_minibuffer(sp, "filter stopped");
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
		  const char *caller, const char *origin, bool over_selection, mus_float_t *ur_a, mus_any *gen, 
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


static char *reverse_channel(chan_info *cp, snd_fd *sf, mus_long_t beg, mus_long_t dur, XEN edp, const char *caller, int arg_pos)
{
  snd_info *sp;
  peak_env_info *ep = NULL;
  file_info *hdr = NULL;
  int i, j, ofd = 0, datumb = 0, err = 0, edpos = 0;
  bool section = false, temp_file;
  mus_long_t k;
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
	  return(mus_format("%s %s temp file %s: %s\n", 
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
      mus_float_t min1, max1;
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
    origin = mus_format(MUS_LD PROC_SEP PROC_FALSE " %s", beg, S_reverse_channel);
  else origin = mus_format(MUS_LD PROC_SEP MUS_LD " %s", beg, dur, S_reverse_channel);
#else
  if (dur == cp->edits[edpos]->samples)
    origin = mus_format("%s" PROC_OPEN MUS_LD PROC_SEP PROC_FALSE, TO_PROC_NAME(S_reverse_channel), beg);
  else origin = mus_format("%s" PROC_OPEN MUS_LD PROC_SEP MUS_LD, TO_PROC_NAME(S_reverse_channel), beg, dur);
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
	      if (err != MUS_NO_ERROR) break;
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


void reverse_sound(chan_info *ncp, bool over_selection, XEN edpos, int arg_pos)
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
	  mus_long_t dur;
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
      string_to_minibuffer(sp, "reverse stopped");
      ss->stopped_explicitly = false;
      for (i = 0; i <= stop_point; i++)
	{
	  cp = si->cps[i];
	  undo_edit(cp, 1);
	}
    }
  free_sync_state(sc);
}


static char *edit_list_envelope(mus_any *egen, mus_long_t beg, mus_long_t env_dur, mus_long_t called_dur, mus_long_t chan_dur, mus_float_t base)
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
	new_origin = mus_format("%s " MUS_LD PROC_SEP PROC_FALSE " %s", envstr, beg, S_env_channel);
      else new_origin = mus_format("%s %.4f " MUS_LD PROC_SEP PROC_FALSE " %s", 
				   envstr, base, beg, S_env_channel_with_base);
#else
      if (base == 1.0)
	new_origin = mus_format("%s" PROC_OPEN "%s" PROC_SEP MUS_LD PROC_SEP PROC_FALSE, 
				TO_PROC_NAME(S_env_channel), envstr, beg);
      else new_origin = mus_format("%s" PROC_OPEN "%s" PROC_SEP "%.4f" PROC_SEP MUS_LD PROC_SEP PROC_FALSE, 
				   TO_PROC_NAME(S_env_channel_with_base), envstr, base, beg);
#endif
    }
  else 
    {
      /* env dur was apparently not chan dur, or called dur was not full sound? */
#if HAVE_FORTH
      new_origin = mus_format("%s :base %.4f :end " MUS_LD " %s " MUS_LD PROC_SEP MUS_LD " %s",
			      envstr, base, env_dur, S_make_env, beg, called_dur, S_env_channel);
#else
      new_origin = mus_format("%s" PROC_OPEN BPAREN "%s" PROC_OPEN "%s" PROC_SEP ":base" PROC_SEP "%.4f" PROC_SEP ":end" PROC_SEP MUS_LD ")" PROC_SEP MUS_LD PROC_SEP MUS_LD,
			      TO_PROC_NAME(S_env_channel), TO_PROC_NAME(S_make_env), envstr, base, env_dur, beg, called_dur);
#endif
    }
  if (envstr) free(envstr);
  free_env(newe);
  return(new_origin);
}


void apply_env(chan_info *cp, env *e, mus_long_t beg, mus_long_t dur, bool over_selection, 
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
  mus_float_t val[1];
  mus_any *egen;
  mus_long_t *passes;
  double *rates;
  mus_float_t egen_val, base;
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
      mus_long_t segbeg, segnum, segend;
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
		  if (scale_channel(si->cps[i], (mus_float_t)(offset + scaler * rates[k]), segbeg, segnum, pos, IN_AS_ONE_EDIT))
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
      mus_long_t ioff;
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
	      snd_error("%s %s temp file %s: %s\n", 
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
			  progress_report(cp, (mus_float_t)((double)ioff / ((double)dur)));
			  if (ss->stopped_explicitly) break;
			  if (!(sp->active))
			    {
			      ss->stopped_explicitly = true;
			      break;
			    }
			}
		      err = mus_file_write(ofd, 0, j - 1, si->chans, data);
		      j = 0;
		      if (err != MUS_NO_ERROR) break;
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
			  progress_report(cp, (mus_float_t)((double)ioff / ((double)dur)));
			  if (ss->stopped_explicitly) break;
			  if (!(sp->active))
			    {
			      ss->stopped_explicitly = true;
			      break;
			    }
			}
		      err = mus_file_write(ofd, 0, j - 1, 1, data);
		      j = 0;
		      if (err != MUS_NO_ERROR) break;
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
		      if ((len < 2) || (snd_abs_mus_long_t(dur - passes[len - 2]) < 2))
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
      mus_long_t segbeg, segnum, segend;
      double power = 0.0;
      mus_float_t *data;

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
							 (mus_float_t)(offset + scaler * data[m]), 
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
		  if ((len < 2) || (snd_abs_mus_long_t(dur - passes[len - 2]) < 2))
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


void cursor_delete(chan_info *cp, mus_long_t count)
{
  mus_long_t beg;
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


void cursor_insert(chan_info *cp, mus_long_t beg, mus_long_t count)
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


void cursor_zeros(chan_info *cp, mus_long_t count, bool over_selection)
{
  int i;
  mus_long_t beg, num;
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
	  mus_float_t scaler[1];
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

static void smooth_channel(chan_info *cp, mus_long_t beg, mus_long_t dur, int edpos)
{
  mus_sample_t *data = NULL;
  mus_long_t k;
  char *origin = NULL;
  mus_float_t y0, y1, angle, incr, off, scale;
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
  origin = mus_format(MUS_LD PROC_SEP MUS_LD " %s", beg, dur, S_smooth_channel);
#else
  origin = mus_format("%s" PROC_OPEN MUS_LD PROC_SEP MUS_LD, TO_PROC_NAME(S_smooth_channel), beg, dur);
#endif
  change_samples(beg, dur, data, cp, origin, edpos);
  if (origin) free(origin);
  update_graph(cp);
  free(data);
}


void cos_smooth(chan_info *cp, mus_long_t beg, mus_long_t num, bool over_selection)
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


#if HAVE_SCHEME
static char *run_channel(chan_info *cp, struct ptree *pt, mus_long_t beg, mus_long_t dur, int edpos, const char *origin, const char *caller)
{
  snd_info *sp;
  file_info *hdr = NULL;
  int j, ofd = 0, datumb = 0, err = 0;
  bool temp_file;
  mus_long_t k;
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
      return(mus_format("%s: can't read %s[%d] channel data!", caller, sp->short_filename, cp->chan));
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
	  return(mus_format("%s %s temp file %s: %s\n", 
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
	      if (err != MUS_NO_ERROR) break;
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
#endif


typedef struct {
  snd_fd **fds;
  int len;
} scale_and_src_data;


static mus_float_t scale_and_src_input(void *data, int direction)
{
  scale_and_src_data *sd = (scale_and_src_data *)data;
  int i;
  mus_float_t sum;
  sum = 0.0;
  for (i = 0; i < sd->len; i++)
    if (sd->fds[i])
      sum += read_sample(sd->fds[i]);
  return(sum);
}


char *scale_and_src(char **files, int len, int max_chans, mus_float_t amp, mus_float_t speed, env *amp_env, bool *temp_file_err)
{
  /* view files mix and insert possible src change */
  char *tempfile;
  snd_fd ***fds = NULL;
  snd_info **sps = NULL;
  int i, chan, chans = 0;
  mus_long_t k, new_dur = 0, dur = 0;
  mus_sample_t **data;
  file_info *hdr = NULL;
  int j, ofd = 0, datumb = 0, err = 0, srate = 0;
  io_error_t io_err = IO_NO_ERROR;
  mus_float_t sum;
  mus_any *e = NULL;
  mus_any **sgens = NULL;
  scale_and_src_data **sdata = NULL;

  (*temp_file_err) = false;
  tempfile = snd_tempnam();

  for (i = 0; i < len; i++)
    {
      int fchans, fsrate;
      mus_long_t flen;
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
      return(mus_format("%s temp file %s: %s\n", 
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
      new_dur = (mus_long_t)((double)dur / (double)speed);
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
	      if (err != MUS_NO_ERROR) break;
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
	      if (err != MUS_NO_ERROR) break;
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


static XEN g_map_chan_1(XEN proc_and_list, XEN s_beg, XEN s_end, XEN org, XEN snd, XEN chn, XEN edpos, XEN s_dur, const char *fallback_caller) 
{ 
  chan_info *cp;
  const char *caller;
  mus_long_t beg = 0, end = 0, dur = 0;
  mus_long_t num;
  int rpt = 0, i, pos;
  bool temp_file = false, backup = false;
  XEN res = XEN_FALSE;
  XEN proc = XEN_FALSE;

  proc = proc_and_list;

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
  else dur = dur_to_samples(s_dur, beg, cp, pos, 3, caller); /* 3 is arg num from caller's point of view */
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

#if HAVE_SCHEME
      if (optimization(ss) > 0)
	{
	  struct ptree *pt = NULL;
	  pt = mus_run_form_to_ptree_1_f(XEN_PROCEDURE_SOURCE(proc_and_list));
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
#endif

      sp = cp->sound;
      sf = init_sample_read_any(beg, cp, READ_FORWARD, pos);
      if (sf == NULL) 
	return(XEN_TRUE);

      temp_file = (num > MAX_BUFFER_SIZE);
      if (temp_file)
	{
	  int rpt4, ofd, datumb;
	  char *filename;
	  file_info *hdr;
	  mus_long_t kp, samps = 0;
	  int j = 0;
	  bool reporting = false;
	  io_error_t io_err = IO_NO_ERROR;

	  reporting = ((num > REPORTING_SIZE) && (!(cp->squelch_update)));
	  if (reporting) start_progress_report(cp);
	  rpt4 = MAX_BUFFER_SIZE / 4;

	  filename = snd_tempnam();
	  hdr = make_temp_header(filename, SND_SRATE(cp->sound), 1, 0, S_map_channel);
	  ofd = open_temp_file(filename, 1, hdr, &io_err);
	  if (ofd == -1)
	    snd_error("%s: %s (temp file) %s: %s", 
		      S_map_channel,
		      (io_err != IO_NO_ERROR) ? io_error_name(io_err) : "can't open",
		      filename, 
		      snd_open_strerror());
	  else
	    {
	      int err = MUS_NO_ERROR;
	      mus_sample_t **data;

#if HAVE_SCHEME
	      s7_pointer arg_list;
	      int gc_loc;
	      arg_list = XEN_LIST_1(XEN_FALSE);
	      gc_loc = s7_gc_protect(s7, arg_list);
#endif

	      data = (mus_sample_t **)calloc(1, sizeof(mus_sample_t *));
	      data[0] = (mus_sample_t *)calloc(MAX_BUFFER_SIZE, sizeof(mus_sample_t));
	      datumb = mus_bytes_per_sample(hdr->format);
	      ss->stopped_explicitly = false;

	      for (kp = 0; kp < num; kp++)
		{
		  /* changed here to remove catch 24-Mar-02 */
#if HAVE_SCHEME
		  s7_set_car(arg_list, C_TO_XEN_DOUBLE((double)read_sample(sf)));
		  res = s7_call_with_location(s7, proc, arg_list, c__FUNCTION__, __FILE__, __LINE__);
#else
		  res = XEN_CALL_1_NO_CATCH(proc, C_TO_XEN_DOUBLE((double)read_sample(sf)));
#endif
		  if (XEN_NUMBER_P(res))                         /* one number -> replace current sample */
		    {
		      samps++;
		      data[0][j++] = XEN_TO_C_DOUBLE(res);
		      if (j == MAX_BUFFER_SIZE)
			{
			  err = mus_file_write(ofd, 0, j - 1, 1, data);
			  j = 0;
			  if (err != MUS_NO_ERROR) break;
			}
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
				      data[0][j++] = v->data[i];
				      if (j == MAX_BUFFER_SIZE)
					{
					  err = mus_file_write(ofd, 0, j - 1, 1, data);
					  j = 0;
					  if (err != MUS_NO_ERROR) break;
					}
				    }
				  samps += v->length - 1;
				}
			      else
				{
				  close_temp_file(filename, ofd, hdr->type, samps * datumb);
				  sf = free_snd_fd(sf);
				  if (reporting) finish_progress_report(cp);
				  snd_remove(filename, REMOVE_FROM_CACHE);
				  free(filename);
				  free(data[0]);
				  free(data);
#if HAVE_SCHEME
				  s7_gc_unprotect_at(s7, gc_loc);
#endif
				  
				  XEN_ERROR(BAD_TYPE,
					    XEN_LIST_3(C_TO_XEN_STRING("~A: result of procedure must be a (non-complex) number, boolean, or vct: ~A"),
						       C_TO_XEN_STRING(caller),
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
			  progress_report(cp, (mus_float_t)((double)kp / (double)num));
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

#if HAVE_SCHEME
	      s7_gc_unprotect_at(s7, gc_loc);
#endif
	      if (j > 0) 
		mus_file_write(ofd, 0, j - 1, 1, data);
	      close_temp_file(filename, ofd, hdr->type, samps * datumb);

	      free_file_info(hdr);
	      free(data[0]);
	      free(data);

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
				XEN_LIST_2(C_TO_XEN_STRING("~A: can't edit closed channel!"),
					   C_TO_XEN_STRING(caller)));
		      return(XEN_FALSE);
		    }

		  if (samps == num)
		    file_change_samples(beg, samps, filename, cp, 0, DELETE_ME, caller, pos);
		  else
		    {
		      delete_samples(beg, num, cp, pos);
		      if (samps > 0)
			{
			  int cured;
			  cured = cp->edit_ctr;
			  file_insert_samples(beg, samps, filename, cp, 0, DELETE_ME, caller, cp->edit_ctr);
			  backup_edit_list(cp);
			  if (cp->edit_ctr > cured)
			    backup_edit_list(cp);
			  ripple_trailing_marks(cp, beg, num, samps);
			}
		      else snd_remove(filename, REMOVE_FROM_CACHE);
		    }
		}
	    }
	  free(filename);
	}
      else
	{
	  /* not temp_file -- use resizable buffer */
	  int data_pos = 0, kp;
	  mus_long_t cur_size;
	  mus_sample_t *data = NULL;
#if HAVE_SCHEME
	  s7_pointer arg_list;
	  int gc_loc;
	  arg_list = XEN_LIST_1(XEN_FALSE);
	  gc_loc = s7_gc_protect(s7, arg_list);
#endif

	  data = (mus_sample_t *)calloc(num, sizeof(mus_sample_t));
	  cur_size = num;

	  for (kp = 0; kp < num; kp++)
	    {
#if HAVE_SCHEME
	      s7_set_car(arg_list, C_TO_XEN_DOUBLE((double)read_sample(sf)));
	      res = s7_call_with_location(s7, proc, arg_list, c__FUNCTION__, __FILE__, __LINE__);
#else
	      res = XEN_CALL_1_NO_CATCH(proc, C_TO_XEN_DOUBLE((double)read_sample(sf)));
#endif
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
#if HAVE_SCHEME
			      s7_gc_unprotect_at(s7, gc_loc);
#endif
			      XEN_ERROR(BAD_TYPE,
					XEN_LIST_3(C_TO_XEN_STRING("~A: result of procedure must be a number, boolean, or vct: ~A"),
						   C_TO_XEN_STRING(caller),
						   res));
			    }
			}
		    }
		}
	    }
	  sf = free_snd_fd(sf);
#if HAVE_SCHEME
	  s7_gc_unprotect_at(s7, gc_loc);
#endif
	  if (cp->active < CHANNEL_HAS_EDIT_LIST)
	    {
	     if (data) {free(data); data = NULL;} 
	      XEN_ERROR(NO_SUCH_CHANNEL,
			XEN_LIST_2(C_TO_XEN_STRING("~A: can't edit closed channel!"),
				   C_TO_XEN_STRING(caller)));
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
  return(res);
}


#define MUS_OUTA_1(Frame, Val, Fd) ((*(Fd->core)->write_sample))(Fd, Frame, 0, Val)
/* avoids all the CLM error checking */

static XEN g_map_chan_ptree_fallback(XEN proc, XEN init_func, chan_info *cp, mus_long_t beg, mus_long_t num, int pos, const char *origin)
{ 
  snd_fd *sf = NULL;
  bool temp_file;
  char *filename = NULL;
  mus_long_t kp;
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
		       C_TO_XEN_INT64_T(0),
		       C_TO_XEN_INT64_T(num),
		       XEN_TRUE,             /* reading forward */
		       origin);
      else v = XEN_CALL_2(init_func,
			  C_TO_XEN_INT64_T(0),
			  C_TO_XEN_INT64_T(num),
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
  mus_long_t beg = 0, dur = 0;
  int pos;
#if HAVE_SCHEME
  bool backup = false;
  bool too_many_ptrees = false;
  struct ptree *pt = NULL;
#endif
  XEN proc = XEN_FALSE;
  /* (ptree-channel (lambda (y) (* y 2))) -> ((lambda (y) (* y 2)) #<procedure #f ((y) (* y 2))>) as "proc_and_list" */
  /*   the cadr proc gives access to the environment, run walks the car */

  proc = proc_and_list;

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
		XEN_LIST_6(C_TO_XEN_STRING("~A: no such edpos: ~A, ~S chan ~A has ~A edits"),
			   C_TO_XEN_STRING(S_ptree_channel),
			   edpos,
			   C_TO_XEN_STRING(cp->sound->short_filename),
			   chn,
			   C_TO_XEN_INT(cp->edit_ctr)));
    }
  beg = beg_to_sample(s_beg, S_ptree_channel);
  dur = dur_to_samples(s_dur, beg, cp, pos, 3, S_ptree_channel);
  if (dur <= 0) return(XEN_FALSE);
  if ((beg + dur) > cp->edits[pos]->samples)
    {
      if (!(extend_with_zeros(cp, cp->edits[pos]->samples, beg + dur - cp->edits[pos]->samples, pos, "extend for " S_ptree_channel))) 
	return(XEN_FALSE);
#if HAVE_SCHEME
      backup = true;
#endif
      pos = cp->edit_ctr;
    }

#if (!HAVE_SCHEME)
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
	  pt = mus_run_form_to_ptree_3_f(XEN_PROCEDURE_SOURCE(proc_and_list));
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
    pt = mus_run_form_to_ptree_1_f(XEN_PROCEDURE_SOURCE(proc_and_list));
  else
    {
      if ((!too_many_ptrees) && (XEN_REQUIRED_ARGS_OK(proc, 3)))
	pt = mus_run_form_to_ptree_3_f(XEN_PROCEDURE_SOURCE(proc_and_list));
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


static XEN g_sp_scan(XEN proc_and_list, XEN s_beg, XEN s_end, XEN snd, XEN chn, 
		     const char *caller, bool counting, XEN edpos, int arg_pos, XEN s_dur)
{
  chan_info *cp;
  mus_long_t beg = 0, end = 0, dur = 0;
  snd_info *sp;
  snd_fd *sf;
  XEN errstr;
  mus_long_t kp, num;
  int rpt = 0, rpt4;
  bool reporting = false;
  int counts = 0, pos;
  char *errmsg;
  XEN proc = XEN_FALSE;
#if HAVE_SCHEME
  struct ptree *pt = NULL;
#endif

  proc = proc_and_list;

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

#if HAVE_SCHEME
  if (optimization(ss) > 0)
    {
      pt = mus_run_form_to_ptree_1_b(XEN_PROCEDURE_SOURCE(proc_and_list));
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
		    return(C_TO_XEN_INT64_T(kp + beg));
		  }
	      }
	  sf = free_snd_fd(sf);
	  mus_run_free_ptree(pt);
	  if (counting)
	    return(C_TO_XEN_INT(counts));
	  return(XEN_FALSE);
	}
    }
#endif

  reporting = ((num > REPORTING_SIZE) && (!(cp->squelch_update)));
  if (reporting) start_progress_report(cp);
  rpt4 = MAX_BUFFER_SIZE / 4;
  ss->stopped_explicitly = false;

#if HAVE_SCHEME
  {
    s7_pointer arg_list;
    int gc_loc;
    arg_list = XEN_LIST_1(XEN_FALSE);
    gc_loc = s7_gc_protect(s7, arg_list);
#endif

  for (kp = 0; kp < num; kp++)
    {
      XEN res;
#if HAVE_SCHEME
      s7_set_car(arg_list, C_TO_XEN_DOUBLE((double)read_sample(sf)));
      res = s7_call_with_location(s7, proc, arg_list, c__FUNCTION__, __FILE__, __LINE__);
#else
      res = XEN_CALL_1_NO_CATCH(proc, C_TO_XEN_DOUBLE((double)read_sample(sf)));
#endif
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
#if HAVE_SCHEME
	      s7_gc_unprotect_at(s7, gc_loc);
#endif
	      return(C_TO_XEN_INT64_T(kp + beg));
	    }
	}
      if (reporting) 
	{
	  rpt++;
	  if (rpt > rpt4)
	    {
	      progress_report(cp, (mus_float_t)((double)kp / (double)num));
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
	  report_in_minibuffer(sp, "%s stopped at sample " MUS_LD, caller, kp + beg);
	  break;
	}
    }
#if HAVE_SCHEME
  s7_gc_unprotect_at(s7, gc_loc);
  }
#endif
  if (reporting) finish_progress_report(cp);
  sf = free_snd_fd(sf);
  if (counting)
    return(C_TO_XEN_INT(counts));

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
if 'func' returns non-" PROC_FALSE ", the scan stops, and the current sample number is returned.\n  " scan_chan_example

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
if func returns non-" PROC_FALSE ", the scan stops, and the current sample number is returned. \n  " scan_channel_example

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


static XEN g_find_channel(XEN expr, XEN sample, XEN snd, XEN chn_n, XEN edpos)
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
  ASSERT_CHANNEL(S_find_channel, snd, chn_n, 3);
  return(g_sp_scan(expr, sample, XEN_FALSE, snd, chn_n, S_find_channel, false, edpos, 5, XEN_FALSE));
}


static XEN g_count_matches(XEN expr, XEN sample, XEN snd, XEN chn_n, XEN edpos)
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

  ASSERT_CHANNEL(S_count_matches, snd, chn_n, 3);
  return(g_sp_scan(expr, sample, XEN_FALSE, snd, chn_n, S_count_matches, true, edpos, 5, XEN_FALSE));
}


static XEN g_smooth_sound(XEN beg, XEN num, XEN snd, XEN chn_n)
{
  #define H_smooth_sound "(" S_smooth_sound " :optional (start-samp 0) (samps len) snd chn): smooth \
data from start-samp for samps in snd's channel chn"
  chan_info *cp;
  mus_long_t start, samps;

  ASSERT_SAMPLE_TYPE(S_smooth_sound, beg, XEN_ARG_1);
  ASSERT_SAMPLE_TYPE(S_smooth_sound, num, XEN_ARG_2);
  ASSERT_CHANNEL(S_smooth_sound, snd, chn_n, 3);

  cp = get_cp(snd, chn_n, S_smooth_sound);
  if (!cp) return(XEN_FALSE);
  start = beg_to_sample(beg, S_smooth_sound);
  samps = dur_to_samples(num, start, cp, cp->edit_ctr, 2, S_smooth_sound);

  cos_smooth(cp, start, samps, OVER_SOUND); 

  return(beg);
}


static XEN g_smooth_channel(XEN beg, XEN dur, XEN snd, XEN chn_n, XEN edpos)
{
  #define H_smooth_channel "(" S_smooth_channel " :optional (beg 0) (dur len) snd chn edpos): \
smooth data from beg for dur in snd's channel chn"
  chan_info *cp;
  mus_long_t start, num;
  int pos;

  ASSERT_SAMPLE_TYPE(S_smooth_channel, beg, XEN_ARG_1);
  ASSERT_SAMPLE_TYPE(S_smooth_channel, dur, XEN_ARG_2);
  ASSERT_CHANNEL(S_smooth_channel, snd, chn_n, 3);

  cp = get_cp(snd, chn_n, S_smooth_channel);
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


static void cut_and_smooth_1(chan_info *cp, mus_long_t beg, mus_long_t end, bool over_selection, int pos)
{
  #define SPLICE_LEN 32
  /* making this 128 is not a big improvement */
  mus_long_t start;
  mus_sample_t splice[2 * SPLICE_LEN];
  double ramp, incr;
  int i;
  snd_fd *sf, *sf_end;
  
  incr = 0.5 / SPLICE_LEN;
  if (end < SPLICE_LEN)
    start = 0;
  else start = end - SPLICE_LEN;
  
  sf_end = init_sample_read_any_with_bufsize(start, cp, READ_FORWARD, pos, 2 * SPLICE_LEN);
  
  if (beg < SPLICE_LEN)
    start = 0;
  else start = beg - SPLICE_LEN;
  
  sf = init_sample_read_any_with_bufsize(start, cp, READ_FORWARD, pos, 2 * SPLICE_LEN);
  for (i = 0, ramp = 1.0; i < 2 * SPLICE_LEN; i++, ramp -= incr)
    {
      mus_float_t x, y;
      x = read_sample(sf);
      y = read_sample(sf_end);
      splice[i] = MUS_DOUBLE_TO_SAMPLE((x * ramp) + (y * (1.0 - ramp)));
    }
  free_snd_fd(sf);
  free_snd_fd(sf_end);
  
  if (over_selection)
    cp_delete_selection(cp);
  else delete_samples(beg, end - beg + 1, cp, pos);
  
  change_samples(start, 2 * SPLICE_LEN, splice, cp, 
		 (over_selection) ? S_delete_selection_and_smooth : S_delete_samples_and_smooth, 
		 cp->edit_ctr);
}


void cut_and_smooth(chan_info *cp)
{
  if (selection_is_active_in_channel(cp))
    cut_and_smooth_1(cp, selection_beg(cp), selection_end(cp), true, cp->edit_ctr);
}


static XEN g_delete_selection_and_smooth(void)
{
  #define H_delete_selection_and_smooth "(" S_delete_selection_and_smooth ") deletes the current selection, and tries to \
make the splice-point smooth."

  if (!(selection_is_active())) 
    return(snd_no_active_selection_error(S_delete_selection_and_smooth));
  for_each_chan(cut_and_smooth);
  return(XEN_FALSE);
}



static XEN g_delete_samples_and_smooth(XEN samp_n, XEN samps, XEN snd, XEN chn_n, XEN edpos)
{
  #define H_delete_samples_and_smooth "(" S_delete_samples_and_smooth " start-samp samps :optional snd chn edpos): \
delete 'samps' samples from snd's channel chn starting at 'start-samp', then try to smooth-over the splice"

  chan_info *cp;
  int pos;
  mus_long_t samp, len;

  XEN_ASSERT_TYPE(XEN_NUMBER_P(samp_n), samp_n, XEN_ARG_1, S_delete_samples_and_smooth, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samps), samps, XEN_ARG_2, S_delete_samples_and_smooth, "a number");

  ASSERT_CHANNEL(S_delete_samples_and_smooth, snd, chn_n, 3);
  cp = get_cp(snd, chn_n, S_delete_samples_and_smooth);
  if (!cp) return(XEN_FALSE);

  pos = to_c_edit_position(cp, edpos, S_delete_samples_and_smooth, 5);
  samp = beg_to_sample(samp_n, S_delete_samples_and_smooth);
  len = XEN_TO_C_INT64_T_OR_ELSE(samps, 0);
  if (len <= 0) return(XEN_FALSE);

  cut_and_smooth_1(cp, samp, samp + len - 1, false, pos);
  update_graph(cp);
  return(samp_n);
}



static XEN g_reverse_sound(XEN snd, XEN chn_n, XEN edpos)
{
  #define H_reverse_sound "(" S_reverse_sound " :optional snd chn edpos): reverse snd's channel chn"
  chan_info *cp;

  ASSERT_CHANNEL(S_reverse_sound, snd, chn_n, 1);
  cp = get_cp(snd, chn_n, S_reverse_sound);
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


static XEN g_reverse_channel(XEN s_beg, XEN s_dur, XEN snd, XEN chn_n, XEN edpos)
{
  #define H_reverse_channel "(" S_reverse_channel " :optional (beg 0) (dur len) snd chn edpos): reverse a portion of snd's channel chn"
  chan_info *cp;
  char *errmsg;
  mus_long_t beg = 0, dur = 0, end;
  int pos;
  snd_fd *sf;

  ASSERT_SAMPLE_TYPE(S_reverse_channel, s_beg, XEN_ARG_1);
  ASSERT_SAMPLE_TYPE(S_reverse_channel, s_dur, XEN_ARG_2);
  ASSERT_CHANNEL(S_reverse_channel, snd, chn_n, 3);

  cp = get_cp(snd, chn_n, S_reverse_channel);
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
		XEN_LIST_2(C_TO_XEN_STRING(S_reverse_channel ": IO error ~A"),
			   str));
    }
  return(s_beg);
}


static XEN g_insert_silence(XEN beg, XEN num, XEN snd, XEN chn)
{
  #define H_insert_silence "(" S_insert_silence " beg num :optional snd chn): insert num zeros at beg in snd's chn"
  chan_info *cp; /* follows sync */
  mus_long_t start = 0, len = 0;

  XEN_ASSERT_TYPE(XEN_NUMBER_P(beg), beg, XEN_ARG_1, S_insert_silence, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(num), num, XEN_ARG_2, S_insert_silence, "a number");
  ASSERT_CHANNEL(S_insert_silence, snd, chn, 3);

  cp = get_cp(snd, chn, S_insert_silence);
  if (!cp) return(XEN_FALSE);
  start = XEN_TO_C_INT64_T(beg);
  if (start < 0) XEN_ERROR(NO_SUCH_SAMPLE,
			   XEN_LIST_2(C_TO_XEN_STRING(S_insert_silence ": no such sample: ~A"),
				      beg));
  len = XEN_TO_C_INT64_T(num);
  if (len <= 0) return(XEN_FALSE);

  cursor_insert(cp, start, len);

  return(beg);
}


static XEN g_pad_channel(XEN beg, XEN num, XEN snd, XEN chn, XEN edpos)
{
  #define H_pad_channel "(" S_pad_channel " beg dur :optional snd chn edpos): insert dur zeros at beg in snd's chn"
  chan_info *cp;
  mus_long_t bg, len;
  int pos;

  XEN_ASSERT_TYPE(XEN_NUMBER_P(beg), beg, XEN_ARG_1, S_pad_channel, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(num), num, XEN_ARG_2, S_pad_channel, "a number");
  ASSERT_CHANNEL(S_pad_channel, snd, chn, 3);

  cp = get_cp(snd, chn, S_pad_channel);
  if (!cp) return(XEN_FALSE);
  bg = beg_to_sample(beg, S_pad_channel);
  pos = to_c_edit_position(cp, edpos, S_pad_channel, 5);
  len = XEN_TO_C_INT64_T_OR_ELSE(num, cp->edits[pos]->samples - bg);

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
      if (XEN_INTEGER_P(snd1) || XEN_SOUND_P(snd1))
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

  if (cp0 == cp1) return(XEN_FALSE);
  if (!(cp1->editable)) return(XEN_FALSE);
  if ((cp0) && (cp1))
    {
      int pos0, pos1;
      mus_long_t dur0, dur1, beg0 = 0, num;

      if (XEN_NUMBER_P(beg)) 
	beg0 = XEN_TO_C_INT64_T(beg);

      pos0 = to_c_edit_position(cp0, edpos0, S_swap_channels, 7);
      pos1 = to_c_edit_position(cp1, edpos1, S_swap_channels, 8);

      dur0 = cp0->edits[pos0]->samples;
      dur1 = cp1->edits[pos1]->samples;

      if (XEN_NUMBER_P(dur)) 
	num = XEN_TO_C_INT64_T(dur);
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


static mus_float_t *load_mus_float_ts(XEN scalers, int *result_len, const char *caller)
{
  int len = 0, i;
  mus_float_t *scls;
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
	      if (len < 0)
		XEN_WRONG_TYPE_ARG_ERROR(caller, 1, scalers, "a proper list");
	    }
	  else XEN_WRONG_TYPE_ARG_ERROR(caller, 1, scalers, "a number, list, or vct");
	}

      if (len == 0) 
	XEN_ERROR(NO_DATA,
		  XEN_LIST_2(C_TO_XEN_STRING("~A: scalers data is empty?"), 
			     C_TO_XEN_STRING(caller)));
    }
  
  scls = (mus_float_t *)calloc(len, sizeof(mus_float_t));
  if (v)
    memcpy((void *)scls, (void *)(v->data), len * sizeof(mus_float_t));
  else
    {
      if (XEN_LIST_P(scalers))
	{
	  XEN lst;
	  for (i = 0, lst = XEN_COPY_ARG(scalers); i < len; i++, lst = XEN_CDR(lst)) 
	    scls[i] = (mus_float_t)XEN_TO_C_DOUBLE(XEN_CAR(lst));
	}
      else scls[0] = (mus_float_t)XEN_TO_C_DOUBLE(scalers);
    }
  result_len[0] = len;
  return(scls);
}


static XEN g_scale_to(XEN scalers, XEN snd, XEN chn_n)
{
  #define H_scale_to "(" S_scale_to " :optional (norms 1.0) snd chn): \
normalize snd to norms (following sync); norms can be a float or a vct/list of floats"

  /* chn_n irrelevant if sync */
  chan_info *cp;
  bool happy;
  int len[1];
  mus_float_t *scls;

  ASSERT_CHANNEL(S_scale_to, snd, chn_n, 2);
  cp = get_cp(snd, chn_n, S_scale_to);
  if (!cp) return(XEN_FALSE);

  scls = load_mus_float_ts(scalers, len, S_scale_to);
  happy = scale_to(cp->sound, cp, scls, len[0], OVER_SOUND);

  free(scls);
  if (happy)
    return(scalers);
  return(XEN_FALSE);
}


static XEN g_scale_by(XEN scalers, XEN snd, XEN chn_n)
{
  #define H_scale_by "(" S_scale_by " scalers :optional snd chn): \
scale snd by scalers (following sync); scalers can be a float or a vct/list of floats"

  /* chn_n irrelevant if sync */
  chan_info *cp;
  int len[1];
  mus_float_t *scls;

  ASSERT_CHANNEL(S_scale_by, snd, chn_n, 2);
  cp = get_cp(snd, chn_n, S_scale_by);
  if (!cp) return(XEN_FALSE);
  len[0] = 0;

  /* fprintf(stderr, "(scale-by %s %s %s)\n", XEN_AS_STRING(scalers), XEN_AS_STRING(snd), XEN_AS_STRING(chn_n)); */

  scls = load_mus_float_ts(scalers, len, S_scale_by);
  if (len[0] == 0)
    {
      /* fprintf(stderr, "len is 0\n"); */
      return(XEN_FALSE);
    }
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
      mus_float_t *scls;

      scls = load_mus_float_ts(scalers, len, S_scale_selection_to);
      happy = scale_to(NULL, NULL, scls, len[0], OVER_SELECTION);

      free(scls);
      if (happy)
	return(scalers);
      return(XEN_FALSE);
    }
  return(snd_no_active_selection_error(S_scale_selection_to));
}


XEN g_scale_selection_by(XEN scalers)
{
  #define H_scale_selection_by "(" S_scale_selection_by " scalers): scale selected portion by scalers"
  if (selection_is_active())
    {
      int len[1];
      mus_float_t *scls;

      scls = load_mus_float_ts(scalers, len, S_scale_selection_by);
      scale_by(NULL, scls, len[0], OVER_SELECTION);

      free(scls);
      return(scalers);
    }
  return(snd_no_active_selection_error(S_scale_selection_by));
}


static XEN g_clm_channel(XEN gen, XEN samp_n, XEN samps, XEN snd, XEN chn_n, XEN edpos, XEN overlap, XEN origin)
{
  #define H_clm_channel "(" S_clm_channel " gen :optional (beg 0) (dur len) snd chn edpos (overlap 0) origin): \
apply gen to snd's channel chn starting at beg for dur samples. overlap is the 'ring' time, if any."

  chan_info *cp;
  mus_long_t beg = 0, dur = 0;
  int pos;
  mus_any *egen;
  char *errmsg = NULL, *caller = NULL;

  ASSERT_SAMPLE_TYPE(S_clm_channel, samp_n, XEN_ARG_2);
  ASSERT_SAMPLE_TYPE(S_clm_channel, samps, XEN_ARG_3);
  XEN_ASSERT_TYPE(XEN_NUMBER_P(overlap) || XEN_FALSE_P(overlap) || XEN_NOT_BOUND_P(overlap), overlap, XEN_ARG_7, S_clm_channel, "a number or " PROC_FALSE);
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(origin), origin, XEN_ARG_8, S_clm_channel, "a string");
  ASSERT_CHANNEL(S_clm_channel, snd, chn_n, 4);

  cp = get_cp(snd, chn_n, S_clm_channel);
  if (!cp) return(XEN_FALSE);
  pos = to_c_edit_position(cp, edpos, S_clm_channel, XEN_ARG_6);
  beg = beg_to_sample(samp_n, S_clm_channel);
  dur = dur_to_samples(samps, beg, cp, pos, XEN_ARG_3, S_clm_channel);
  if (dur == 0) return(XEN_FALSE);
  XEN_ASSERT_TYPE(mus_xen_p(gen), gen, XEN_ARG_1, S_clm_channel, "a clm generator");
  egen = XEN_TO_MUS_ANY(gen);
  if (XEN_STRING_P(origin)) caller = mus_strdup(XEN_TO_C_STRING(origin)); else caller = mus_strdup(S_clm_channel);

  errmsg = clm_channel(cp, egen, beg, dur, pos, XEN_TO_C_INT64_T_OR_ELSE(overlap, 0), caller);

  free(caller);
  if (errmsg)
    {
      XEN str;
      str = C_TO_XEN_STRING(errmsg);
      free(errmsg);
      XEN_ERROR(XEN_ERROR_TYPE("IO-error"),
		XEN_LIST_2(C_TO_XEN_STRING(S_clm_channel ": IO error ~A"),
			   str));
    }
  return(gen);
}


static XEN g_env_1(XEN edata, mus_long_t beg, mus_long_t dur, XEN ebase, chan_info *cp, XEN edpos, const char *caller, bool over_selection)
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


static XEN g_env_sound(XEN edata, XEN samp_n, XEN samps, XEN base, XEN snd, XEN chn_n, XEN edpos)
{
  #define H_env_sound "(" S_env_sound " env :optional (start-samp 0) (samps len) (env-base 1.0) snd chn edpos): \
apply amplitude envelope (a list of breakpoints or a CLM env) to snd's channel chn starting at start-samp, going \
either to the end of the sound or for samps samples, with segments interpolating according to env-base"

  mus_long_t beg = 0, dur = 0;
  int pos;
  chan_info *cp;

  ASSERT_SAMPLE_TYPE(S_env_sound, samp_n, XEN_ARG_2);
  ASSERT_SAMPLE_TYPE(S_env_sound, samps, XEN_ARG_3);
  ASSERT_CHANNEL(S_env_sound, snd, chn_n, 5);

  cp = get_cp(snd, chn_n, S_env_sound);
  if (!cp) return(XEN_FALSE);
  pos = to_c_edit_position(cp, edpos, S_env_sound, 7);
  beg = beg_to_sample(samp_n, S_env_sound);
  dur = dur_to_samples(samps, beg, cp, pos, XEN_ARG_3, S_env_sound);

  return(g_env_1(edata, beg, dur, base, cp, edpos, S_env_sound, OVER_SOUND));
}


static XEN g_env_channel(XEN gen, XEN samp_n, XEN samps, XEN snd, XEN chn_n, XEN edpos)
{
  #define H_env_channel "(" S_env_channel " clm-env-gen-or-envelope :optional (beg 0) (dur len) snd chn edpos): \
apply amplitude envelope to snd's channel chn starting at beg for dur samples."

  chan_info *cp;
  snd_info *sp;
  mus_long_t beg = 0, dur;
  int old_sync = 0, pos;
  XEN val;

  ASSERT_SAMPLE_TYPE(S_env_channel, samp_n, XEN_ARG_2);
  ASSERT_SAMPLE_TYPE(S_env_channel, samps, XEN_ARG_3);
  ASSERT_CHANNEL(S_env_channel, snd, chn_n, 4);

  cp = get_cp(snd, chn_n, S_env_channel);
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


static XEN g_env_channel_with_base(XEN gen, XEN base, XEN samp_n, XEN samps, XEN snd, XEN chn_n, XEN edpos)
{
  #define H_env_channel_with_base "(" S_env_channel_with_base " clm-env-gen-or-envelope :optional (base 1.0) (beg 0) (dur len) snd chn edpos): \
apply amplitude envelope to snd's channel chn starting at beg for dur samples."

  chan_info *cp;
  snd_info *sp;
  mus_long_t beg = 0, dur;
  int old_sync = 0, pos;
  XEN val;

  ASSERT_SAMPLE_TYPE(S_env_channel, samp_n, XEN_ARG_2);
  ASSERT_SAMPLE_TYPE(S_env_channel, samps, XEN_ARG_3);
  ASSERT_CHANNEL(S_env_channel, snd, chn_n, 4);

  cp = get_cp(snd, chn_n, S_env_channel);
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
  mus_long_t samp, samps;
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
	  mus_float_t data[4];
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
  mus_long_t samp, samps;
  int pos;
  mus_float_t ebase = 1.0;

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
      mus_float_t *data;
      double *rates;
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
	      data = (mus_float_t *)calloc(4, sizeof(mus_float_t));
	      data[0] = 0.0;
	      data[1] = seg0;
	      data[2] = 1.0;
	      data[3] = seg1;
	      e = mus_make_env_with_length(data, 2, 1.0, 0.0, ebase, samps);

	      rates = mus_env_rates(e);
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
  mus_float_t *rl = NULL, *im = NULL;

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
      rl = (mus_float_t *)calloc(n2, sizeof(mus_float_t));
      im = (mus_float_t *)calloc(n2, sizeof(mus_float_t));
      need_free = true;
      memcpy((void *)rl, (void *)(v1->data), n * sizeof(mus_float_t));
      memcpy((void *)im, (void *)(v2->data), n * sizeof(mus_float_t));
    }

  mus_fft(rl, im, n2, isign);

  if (need_free)
    {
      memcpy((void *)(v1->data), (void *)rl, n * sizeof(mus_float_t));
      memcpy((void *)(v2->data), (void *)im, n * sizeof(mus_float_t));
      free(rl);
      free(im);
    }
  return(reals);
}


static XEN g_snd_spectrum(XEN data, XEN win, XEN len, XEN linear_or_dB, XEN beta, XEN in_place, XEN normalized)
{
  #define H_snd_spectrum "(" S_snd_spectrum " data :optional (window " S_rectangular_window ") (len data-len) (linear " PROC_TRUE ") (beta 0.0) in-place (normalized " PROC_TRUE ")): \
magnitude spectrum of data (a vct), in data if in-place, using fft-window win and fft length len."

  bool linear = true, in_data = false, normed = true;
  int i, j, n, n2, wtype;
  mus_float_t maxa, lowest, b = 0.0;
  mus_float_t *rdat;
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
      rdat = (mus_float_t *)malloc(n * sizeof(mus_float_t));
      if (n < v->length)
	for (i = 0; i < n; i++) rdat[i] = v->data[i];
      else memcpy((void *)rdat, (void *)(v->data), v->length * sizeof(mus_float_t));
    }
  else rdat = v->data;

  if (wtype != (int)MUS_RECTANGULAR_WINDOW)
    {
      mus_float_t *window;
      window = (mus_float_t *)calloc(n, sizeof(mus_float_t));
      mus_make_fft_window_with_window((mus_fft_window_t)wtype, n, b * fft_beta_max((mus_fft_window_t)wtype), 0.0, window);
      for (i = 0; i < n; i++) rdat[i] *= window[i];
      free(window);
    }
    
    n2 = n / 2;
  {
    mus_float_t *idat;
    idat = (mus_float_t *)calloc(n, sizeof(mus_float_t));
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

  lowest = 0.000001;
  maxa = 0.0;
  n = n / 2;
  for (i = 0; i < n; i++)
    {
      mus_float_t val;
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
	  mus_float_t todb;
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
  return(xen_make_vct(n, rdat)); /* xen_make_vct uses the data array directly (frees upon gc) */
}


static XEN g_convolve_with_1(XEN file, XEN new_amp, chan_info *cp, XEN edpos, const char *caller)
{
  /* cp NULL -> selection (see above) */
  mus_float_t amp;
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
		    XEN_LIST_3(C_TO_XEN_STRING("~A: IO error ~A"),
			       C_TO_XEN_STRING(caller),
			       errstr));
	}
    }
  else return(snd_no_such_file_error(caller, file));
  return(file);
}


static XEN g_convolve_with(XEN file, XEN new_amp, XEN snd, XEN chn_n, XEN edpos)
{
  #define H_convolve_with "(" S_convolve_with " file :optional (amp 1.0) snd chn edpos): \
convolve file with snd's channel chn (or the currently sync'd channels); amp is the resultant peak amp"

  chan_info *cp;
  ASSERT_CHANNEL(S_convolve_with, snd, chn_n, 3);
  cp = get_cp(snd, chn_n, S_convolve_with);
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

static mus_float_t check_src_envelope(int pts, mus_float_t *data, int *error)
{
  /* can't go through zero here, and if negative need to return 1.0 */
  int i;
  mus_float_t res = 0.0;
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


static XEN g_src_channel(XEN ratio_or_env, XEN beg_n, XEN dur_n, XEN snd, XEN chn_n, XEN edpos)
{
  #define H_src_channel "(" S_src_channel " ratio-or-env :optional (beg 0) (dur len) snd chn edpos): \
sampling-rate convert snd's channel chn by ratio, or following an envelope (a list or a CLM env generator)."

  chan_info *cp;
  char *errmsg;
  snd_fd *sf;
  mus_long_t beg, dur;
  int pos;
  bool clm_err = false;
  mus_any *egen = NULL;
  bool need_free = false;
  mus_float_t ratio = 0.0; /* not 1.0 here! -- the zero is significant */

  XEN_ASSERT_TYPE((XEN_NUMBER_P(ratio_or_env)) || 
		  (XEN_LIST_P(ratio_or_env)) ||
		  ((mus_xen_p(ratio_or_env)) && 
		   (mus_env_p(egen = XEN_TO_MUS_ANY(ratio_or_env)))),
		  ratio_or_env, XEN_ARG_1, S_src_channel, "a number, an envelope, or a CLM env generator");
  ASSERT_SAMPLE_TYPE(S_src_channel, beg_n, XEN_ARG_2);
  ASSERT_SAMPLE_TYPE(S_src_channel, dur_n, XEN_ARG_3);
  ASSERT_CHANNEL(S_src_channel, snd, chn_n, 4);

  cp = get_cp(snd, chn_n, S_src_channel);
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
	    mus_free(egen); 

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
		XEN_LIST_2(C_TO_XEN_STRING(S_src_channel ": ~A"),
			   err));
    }
  return(ratio_or_env);
}


static XEN g_src_1(XEN ratio_or_env, XEN ebase, XEN snd, XEN chn_n, XEN edpos, const char *caller, bool over_selection)
{
  chan_info *cp;
  ASSERT_CHANNEL(caller, snd, chn_n, 3);
  cp = get_cp(snd, chn_n, caller);
  if (!cp) return(XEN_FALSE);
  if (XEN_NUMBER_P(ratio_or_env))
    {
      mus_float_t ratio;
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
	  mus_float_t e_ratio = 1.0;

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
  return(ratio_or_env);
}


static XEN g_src_sound(XEN ratio_or_env, XEN base, XEN snd, XEN chn_n, XEN edpos)
{
  #define H_src_sound "(" S_src_sound " ratio-or-env :optional (base 1.0) snd chn edpos): \
sampling-rate convert snd's channel chn by ratio, or following an envelope. A negative ratio reverses the sound"

  return(g_src_1(ratio_or_env, base, snd, chn_n, edpos, S_src_sound, OVER_SOUND));
}


static XEN g_src_selection(XEN ratio_or_env, XEN base)
{
  #define H_src_selection "(" S_src_selection " ratio-or-env :optional (base 1.0)): \
sampling-rate convert the currently selected data by ratio (which can be an envelope)"

  if (!(selection_is_active())) 
    return(snd_no_active_selection_error(S_src_selection));
  return(g_src_1(ratio_or_env, base, XEN_FALSE, XEN_FALSE, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), S_src_selection, OVER_SELECTION));
}


static XEN g_filter_channel(XEN e, XEN order, XEN beg, XEN dur, XEN snd, XEN chn_n, XEN edpos, XEN truncate, XEN origin)
{
  #define H_filter_channel "(" S_filter_channel " env :optional order beg dur snd chn edpos (truncate " PROC_TRUE ") origin): \
applies an FIR filter to snd's channel chn. 'env' is the frequency response envelope, or a vct with the coefficients."

  chan_info *cp;
  char *errstr = NULL;
  const char *caller = NULL;
  bool truncate_1 = true;
  int order_1 = 0, edpos_1 = AT_CURRENT_EDIT_POSITION;
  mus_long_t beg_1 = 0, dur_1 = 0;
  env *e_1 = NULL;
  vct *v = NULL;
  mus_float_t *coeffs = NULL;

  XEN_ASSERT_TYPE(XEN_LIST_P(e) || MUS_VCT_P(e), e, XEN_ARG_1, S_filter_channel, "an envelope or a vct");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(order), order, XEN_ARG_2, S_filter_channel, "an integer");
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(origin), origin, XEN_ARG_9, S_filter_channel, "a string");

  if (XEN_INTEGER_P(order)) order_1 = XEN_TO_C_INT(order);
  ASSERT_CHANNEL(S_filter_channel, snd, chn_n, 5);
  cp = get_cp(snd, chn_n, S_filter_channel);
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
		XEN_LIST_2(C_TO_XEN_STRING(S_filter_channel ": IO error ~A"),
			   str));
    }
  return(e);
}


static XEN g_filter_1(XEN e, XEN order, XEN snd, XEN chn_n, XEN edpos, const char *caller, const char *origin, bool over_selection, bool truncate)
{
  chan_info *cp;
  ASSERT_CHANNEL(caller, snd, chn_n, 3);
  cp = get_cp(snd, chn_n, caller);
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
		    XEN_LIST_3(C_TO_XEN_STRING("~A: ~A"),
			       C_TO_XEN_STRING(caller),
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
	    XEN_OUT_OF_RANGE_ERROR(caller, 2, order, "> 0 (order = ~A)");
	}
      if (MUS_VCT_P(e)) /* the filter coefficients direct */
	{
	  vct *v;
	  char *new_origin = NULL, *estr = NULL;
	  v = XEN_TO_VCT(e);
	  if (len > v->length) 
	    XEN_OUT_OF_RANGE_ERROR(caller, 2, order, "<= length coeffs (order = ~A)");
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
  return(XEN_TRUE);
}


static XEN g_filter_sound(XEN e, XEN order, XEN snd, XEN chn_n, XEN edpos, XEN origin)
{
  #define H_filter_sound "(" S_filter_sound " filter :optional order snd chn edpos origin): \
applies FIR filter to snd's channel chn. 'filter' is either the frequency response envelope, a CLM filter, or a vct with the actual coefficients"

  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(origin), origin, XEN_ARG_6, S_filter_sound, "a string");
  return(g_filter_1(e, order, snd, chn_n, edpos, 
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

  mus_float_t best = 0.0;
  int n, size = 0, dist = 0;
  int *best_phases;
  int *current_phases;
  mus_float_t *current_max;
  mus_float_t **current_sum;
  mus_float_t **sines;
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
		mus_float_t fval;
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
  mus_float_t cmax;
  XEN result = XEN_EMPTY_LIST;

  n = (XEN_LIST_LENGTH(arglist) - 1);
  if (n < 2) return(XEN_LIST_1(XEN_ZERO));
  dist = XEN_TO_C_INT(XEN_CAR(arglist));
  arglist = XEN_CDR(arglist);

  sines = (mus_float_t **)calloc(n, sizeof(mus_float_t *));
  sizes = (int *)calloc(n, sizeof(int));
  best_phases = (int *)calloc(n, sizeof(int));
  current_phases = (int *)calloc(n, sizeof(int));
  current_sum = (mus_float_t **)calloc(n, sizeof(mus_float_t *));
  current_max = (mus_float_t *)calloc(n, sizeof(mus_float_t));

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
    current_sum[i] = (mus_float_t *)calloc(size, sizeof(mus_float_t));

  best = 10000.0;
  cmax = fabs(sines[0][0]);
  for (i = 1; i < sizes[0]; i++) 
    {
      mus_float_t absv;
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

#if 0
  static int primes[129] = {1, 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 
			    89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 
			    191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 
			    283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 
			    401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 
			    509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 
			    631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719};
#else
  static int primes[2049] = 
    {1, 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 
     89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 
     191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 
     283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 
     401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 
     509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 
     631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 
     769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 
     887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997, 1009, 1013, 1019, 
     1021, 1031, 1033, 1039, 1049, 1051, 1061, 1063, 1069, 1087, 1091, 1093, 1097, 1103, 1109, 1117, 1123, 1129, 
     1151, 1153, 1163, 1171, 1181, 1187, 1193, 1201, 1213, 1217, 1223, 1229, 1231, 1237, 1249, 1259, 1277, 1279, 
     1283, 1289, 1291, 1297, 1301, 1303, 1307, 1319, 1321, 1327, 1361, 1367, 1373, 1381, 1399, 1409, 1423, 1427, 
     1429, 1433, 1439, 1447, 1451, 1453, 1459, 1471, 1481, 1483, 1487, 1489, 1493, 1499, 1511, 1523, 1531, 1543, 
     1549, 1553, 1559, 1567, 1571, 1579, 1583, 1597, 1601, 1607, 1609, 1613, 1619, 1621, 1627, 1637, 1657, 1663, 
     1667, 1669, 1693, 1697, 1699, 1709, 1721, 1723, 1733, 1741, 1747, 1753, 1759, 1777, 1783, 1787, 1789, 1801, 
     1811, 1823, 1831, 1847, 1861, 1867, 1871, 1873, 1877, 1879, 1889, 1901, 1907, 1913, 1931, 1933, 1949, 1951, 
     1973, 1979, 1987, 1993, 1997, 1999, 2003, 2011, 2017, 2027, 2029, 2039, 2053, 2063, 2069, 2081, 2083, 2087, 
     2089, 2099, 2111, 2113, 2129, 2131, 2137, 2141, 2143, 2153, 2161, 2179, 2203, 2207, 2213, 2221, 2237, 2239, 
     2243, 2251, 2267, 2269, 2273, 2281, 2287, 2293, 2297, 2309, 2311, 2333, 2339, 2341, 2347, 2351, 2357, 2371, 
     2377, 2381, 2383, 2389, 2393, 2399, 2411, 2417, 2423, 2437, 2441, 2447, 2459, 2467, 2473, 2477, 2503, 2521, 
     2531, 2539, 2543, 2549, 2551, 2557, 2579, 2591, 2593, 2609, 2617, 2621, 2633, 2647, 2657, 2659, 2663, 2671, 
     2677, 2683, 2687, 2689, 2693, 2699, 2707, 2711, 2713, 2719, 2729, 2731, 2741, 2749, 2753, 2767, 2777, 2789, 
     2791, 2797, 2801, 2803, 2819, 2833, 2837, 2843, 2851, 2857, 2861, 2879, 2887, 2897, 2903, 2909, 2917, 2927, 
     2939, 2953, 2957, 2963, 2969, 2971, 2999, 3001, 3011, 3019, 3023, 3037, 3041, 3049, 3061, 3067, 3079, 3083, 
     3089, 3109, 3119, 3121, 3137, 3163, 3167, 3169, 3181, 3187, 3191, 3203, 3209, 3217, 3221, 3229, 3251, 3253, 
     3257, 3259, 3271, 3299, 3301, 3307, 3313, 3319, 3323, 3329, 3331, 3343, 3347, 3359, 3361, 3371, 3373, 3389, 
     3391, 3407, 3413, 3433, 3449, 3457, 3461, 3463, 3467, 3469, 3491, 3499, 3511, 3517, 3527, 3529, 3533, 3539, 
     3541, 3547, 3557, 3559, 3571, 3581, 3583, 3593, 3607, 3613, 3617, 3623, 3631, 3637, 3643, 3659, 3671, 3673, 
     3677, 3691, 3697, 3701, 3709, 3719, 3727, 3733, 3739, 3761, 3767, 3769, 3779, 3793, 3797, 3803, 3821, 3823, 
     3833, 3847, 3851, 3853, 3863, 3877, 3881, 3889, 3907, 3911, 3917, 3919, 3923, 3929, 3931, 3943, 3947, 3967, 
     3989, 4001, 4003, 4007, 4013, 4019, 4021, 4027, 4049, 4051, 4057, 4073, 4079, 4091, 4093, 4099, 4111, 4127, 
     4129, 4133, 4139, 4153, 4157, 4159, 4177, 4201, 4211, 4217, 4219, 4229, 4231, 4241, 4243, 4253, 4259, 4261, 
     4271, 4273, 4283, 4289, 4297, 4327, 4337, 4339, 4349, 4357, 4363, 4373, 4391, 4397, 4409, 4421, 4423, 4441, 
     4447, 4451, 4457, 4463, 4481, 4483, 4493, 4507, 4513, 4517, 4519, 4523, 4547, 4549, 4561, 4567, 4583, 4591, 
     4597, 4603, 4621, 4637, 4639, 4643, 4649, 4651, 4657, 4663, 4673, 4679, 4691, 4703, 4721, 4723, 4729, 4733, 
     4751, 4759, 4783, 4787, 4789, 4793, 4799, 4801, 4813, 4817, 4831, 4861, 4871, 4877, 4889, 4903, 4909, 4919, 
     4931, 4933, 4937, 4943, 4951, 4957, 4967, 4969, 4973, 4987, 4993, 4999, 5003, 5009, 5011, 5021, 5023, 5039, 
     5051, 5059, 5077, 5081, 5087, 5099, 5101, 5107, 5113, 5119, 5147, 5153, 5167, 5171, 5179, 5189, 5197, 5209, 
     5227, 5231, 5233, 5237, 5261, 5273, 5279, 5281, 5297, 5303, 5309, 5323, 5333, 5347, 5351, 5381, 5387, 5393, 
     5399, 5407, 5413, 5417, 5419, 5431, 5437, 5441, 5443, 5449, 5471, 5477, 5479, 5483, 5501, 5503, 5507, 5519, 
     5521, 5527, 5531, 5557, 5563, 5569, 5573, 5581, 5591, 5623, 5639, 5641, 5647, 5651, 5653, 5657, 5659, 5669, 
     5683, 5689, 5693, 5701, 5711, 5717, 5737, 5741, 5743, 5749, 5779, 5783, 5791, 5801, 5807, 5813, 5821, 5827, 
     5839, 5843, 5849, 5851, 5857, 5861, 5867, 5869, 5879, 5881, 5897, 5903, 5923, 5927, 5939, 5953, 5981, 5987, 
     6007, 6011, 6029, 6037, 6043, 6047, 6053, 6067, 6073, 6079, 6089, 6091, 6101, 6113, 6121, 6131, 6133, 6143, 
     6151, 6163, 6173, 6197, 6199, 6203, 6211, 6217, 6221, 6229, 6247, 6257, 6263, 6269, 6271, 6277, 6287, 6299, 
     6301, 6311, 6317, 6323, 6329, 6337, 6343, 6353, 6359, 6361, 6367, 6373, 6379, 6389, 6397, 6421, 6427, 6449, 
     6451, 6469, 6473, 6481, 6491, 6521, 6529, 6547, 6551, 6553, 6563, 6569, 6571, 6577, 6581, 6599, 6607, 6619, 
     6637, 6653, 6659, 6661, 6673, 6679, 6689, 6691, 6701, 6703, 6709, 6719, 6733, 6737, 6761, 6763, 6779, 6781, 
     6791, 6793, 6803, 6823, 6827, 6829, 6833, 6841, 6857, 6863, 6869, 6871, 6883, 6899, 6907, 6911, 6917, 6947, 
     6949, 6959, 6961, 6967, 6971, 6977, 6983, 6991, 6997, 7001, 7013, 7019, 7027, 7039, 7043, 7057, 7069, 7079, 
     7103, 7109, 7121, 7127, 7129, 7151, 7159, 7177, 7187, 7193, 7207, 7211, 7213, 7219, 7229, 7237, 7243, 7247, 
     7253, 7283, 7297, 7307, 7309, 7321, 7331, 7333, 7349, 7351, 7369, 7393, 7411, 7417, 7433, 7451, 7457, 7459, 
     7477, 7481, 7487, 7489, 7499, 7507, 7517, 7523, 7529, 7537, 7541, 7547, 7549, 7559, 7561, 7573, 7577, 7583, 
     7589, 7591, 7603, 7607, 7621, 7639, 7643, 7649, 7669, 7673, 7681, 7687, 7691, 7699, 7703, 7717, 7723, 7727, 
     7741, 7753, 7757, 7759, 7789, 7793, 7817, 7823, 7829, 7841, 7853, 7867, 7873, 7877, 7879, 7883, 7901, 7907, 
     7919, 7927, 7933, 7937, 7949, 7951, 7963, 7993, 8009, 8011, 8017, 8039, 8053, 8059, 8069, 8081, 8087, 8089, 
     8093, 8101, 8111, 8117, 8123, 8147, 8161, 8167, 8171, 8179, 8191, 8209, 8219, 8221, 8231, 8233, 8237, 8243, 
     8263, 8269, 8273, 8287, 8291, 8293, 8297, 8311, 8317, 8329, 8353, 8363, 8369, 8377, 8387, 8389, 8419, 8423, 
     8429, 8431, 8443, 8447, 8461, 8467, 8501, 8513, 8521, 8527, 8537, 8539, 8543, 8563, 8573, 8581, 8597, 8599, 
     8609, 8623, 8627, 8629, 8641, 8647, 8663, 8669, 8677, 8681, 8689, 8693, 8699, 8707, 8713, 8719, 8731, 8737, 
     8741, 8747, 8753, 8761, 8779, 8783, 8803, 8807, 8819, 8821, 8831, 8837, 8839, 8849, 8861, 8863, 8867, 8887, 
     8893, 8923, 8929, 8933, 8941, 8951, 8963, 8969, 8971, 8999, 9001, 9007, 9011, 9013, 9029, 9041, 9043, 9049, 
     9059, 9067, 9091, 9103, 9109, 9127, 9133, 9137, 9151, 9157, 9161, 9173, 9181, 9187, 9199, 9203, 9209, 9221, 
     9227, 9239, 9241, 9257, 9277, 9281, 9283, 9293, 9311, 9319, 9323, 9337, 9341, 9343, 9349, 9371, 9377, 9391, 
     9397, 9403, 9413, 9419, 9421, 9431, 9433, 9437, 9439, 9461, 9463, 9467, 9473, 9479, 9491, 9497, 9511, 9521, 
     9533, 9539, 9547, 9551, 9587, 9601, 9613, 9619, 9623, 9629, 9631, 9643, 9649, 9661, 9677, 9679, 9689, 9697, 
     9719, 9721, 9733, 9739, 9743, 9749, 9767, 9769, 9781, 9787, 9791, 9803, 9811, 9817, 9829, 9833, 9839, 9851, 
     9857, 9859, 9871, 9883, 9887, 9901, 9907, 9923, 9929, 9931, 9941, 9949, 9967, 9973, 10007, 10009, 10037, 10039, 
     10061, 10067, 10069, 10079, 10091, 10093, 10099, 10103, 10111, 10133, 10139, 10141, 10151, 10159, 10163, 10169, 10177, 10181, 
     10193, 10211, 10223, 10243, 10247, 10253, 10259, 10267, 10271, 10273, 10289, 10301, 10303, 10313, 10321, 10331, 10333, 10337, 
     10343, 10357, 10369, 10391, 10399, 10427, 10429, 10433, 10453, 10457, 10459, 10463, 10477, 10487, 10499, 10501, 10513, 10529, 
     10531, 10559, 10567, 10589, 10597, 10601, 10607, 10613, 10627, 10631, 10639, 10651, 10657, 10663, 10667, 10687, 10691, 10709, 
     10711, 10723, 10729, 10733, 10739, 10753, 10771, 10781, 10789, 10799, 10831, 10837, 10847, 10853, 10859, 10861, 10867, 10883, 
     10889, 10891, 10903, 10909, 10937, 10939, 10949, 10957, 10973, 10979, 10987, 10993, 11003, 11027, 11047, 11057, 11059, 11069, 
     11071, 11083, 11087, 11093, 11113, 11117, 11119, 11131, 11149, 11159, 11161, 11171, 11173, 11177, 11197, 11213, 11239, 11243, 
     11251, 11257, 11261, 11273, 11279, 11287, 11299, 11311, 11317, 11321, 11329, 11351, 11353, 11369, 11383, 11393, 11399, 11411, 
     11423, 11437, 11443, 11447, 11467, 11471, 11483, 11489, 11491, 11497, 11503, 11519, 11527, 11549, 11551, 11579, 11587, 11593, 
     11597, 11617, 11621, 11633, 11657, 11677, 11681, 11689, 11699, 11701, 11717, 11719, 11731, 11743, 11777, 11779, 11783, 11789, 
     11801, 11807, 11813, 11821, 11827, 11831, 11833, 11839, 11863, 11867, 11887, 11897, 11903, 11909, 11923, 11927, 11933, 11939, 
     11941, 11953, 11959, 11969, 11971, 11981, 11987, 12007, 12011, 12037, 12041, 12043, 12049, 12071, 12073, 12097, 12101, 12107, 
     12109, 12113, 12119, 12143, 12149, 12157, 12161, 12163, 12197, 12203, 12211, 12227, 12239, 12241, 12251, 12253, 12263, 12269, 
     12277, 12281, 12289, 12301, 12323, 12329, 12343, 12347, 12373, 12377, 12379, 12391, 12401, 12409, 12413, 12421, 12433, 12437, 
     12451, 12457, 12473, 12479, 12487, 12491, 12497, 12503, 12511, 12517, 12527, 12539, 12541, 12547, 12553, 12569, 12577, 12583, 
     12589, 12601, 12611, 12613, 12619, 12637, 12641, 12647, 12653, 12659, 12671, 12689, 12697, 12703, 12713, 12721, 12739, 12743, 
     12757, 12763, 12781, 12791, 12799, 12809, 12821, 12823, 12829, 12841, 12853, 12889, 12893, 12899, 12907, 12911, 12917, 12919, 
     12923, 12941, 12953, 12959, 12967, 12973, 12979, 12983, 13001, 13003, 13007, 13009, 13033, 13037, 13043, 13049, 13063, 13093, 
     13099, 13103, 13109, 13121, 13127, 13147, 13151, 13159, 13163, 13171, 13177, 13183, 13187, 13217, 13219, 13229, 13241, 13249, 
     13259, 13267, 13291, 13297, 13309, 13313, 13327, 13331, 13337, 13339, 13367, 13381, 13397, 13399, 13411, 13417, 13421, 13441, 
     13451, 13457, 13463, 13469, 13477, 13487, 13499, 13513, 13523, 13537, 13553, 13567, 13577, 13591, 13597, 13613, 13619, 13627, 
     13633, 13649, 13669, 13679, 13681, 13687, 13691, 13693, 13697, 13709, 13711, 13721, 13723, 13729, 13751, 13757, 13759, 13763, 
     13781, 13789, 13799, 13807, 13829, 13831, 13841, 13859, 13873, 13877, 13879, 13883, 13901, 13903, 13907, 13913, 13921, 13931, 
     13933, 13963, 13967, 13997, 13999, 14009, 14011, 14029, 14033, 14051, 14057, 14071, 14081, 14083, 14087, 14107, 14143, 14149, 
     14153, 14159, 14173, 14177, 14197, 14207, 14221, 14243, 14249, 14251, 14281, 14293, 14303, 14321, 14323, 14327, 14341, 14347, 
     14369, 14387, 14389, 14401, 14407, 14411, 14419, 14423, 14431, 14437, 14447, 14449, 14461, 14479, 14489, 14503, 14519, 14533, 
     14537, 14543, 14549, 14551, 14557, 14561, 14563, 14591, 14593, 14621, 14627, 14629, 14633, 14639, 14653, 14657, 14669, 14683, 
     14699, 14713, 14717, 14723, 14731, 14737, 14741, 14747, 14753, 14759, 14767, 14771, 14779, 14783, 14797, 14813, 14821, 14827, 
     14831, 14843, 14851, 14867, 14869, 14879, 14887, 14891, 14897, 14923, 14929, 14939, 14947, 14951, 14957, 14969, 14983, 15013, 
     15017, 15031, 15053, 15061, 15073, 15077, 15083, 15091, 15101, 15107, 15121, 15131, 15137, 15139, 15149, 15161, 15173, 15187, 
     15193, 15199, 15217, 15227, 15233, 15241, 15259, 15263, 15269, 15271, 15277, 15287, 15289, 15299, 15307, 15313, 15319, 15329, 
     15331, 15349, 15359, 15361, 15373, 15377, 15383, 15391, 15401, 15413, 15427, 15439, 15443, 15451, 15461, 15467, 15473, 15493, 
     15497, 15511, 15527, 15541, 15551, 15559, 15569, 15581, 15583, 15601, 15607, 15619, 15629, 15641, 15643, 15647, 15649, 15661, 
     15667, 15671, 15679, 15683, 15727, 15731, 15733, 15737, 15739, 15749, 15761, 15767, 15773, 15787, 15791, 15797, 15803, 15809, 
     15817, 15823, 15859, 15877, 15881, 15887, 15889, 15901, 15907, 15913, 15919, 15923, 15937, 15959, 15971, 15973, 15991, 16001, 
     16007, 16033, 16057, 16061, 16063, 16067, 16069, 16073, 16087, 16091, 16097, 16103, 16111, 16127, 16139, 16141, 16183, 16187, 
     16189, 16193, 16217, 16223, 16229, 16231, 16249, 16253, 16267, 16273, 16301, 16319, 16333, 16339, 16349, 16361, 16363, 16369, 
     16381, 16411, 16417, 16421, 16427, 16433, 16447, 16451, 16453, 16477, 16481, 16487, 16493, 16519, 16529, 16547, 16553, 16561, 
     16567, 16573, 16603, 16607, 16619, 16631, 16633, 16649, 16651, 16657, 16661, 16673, 16691, 16693, 16699, 16703, 16729, 16741, 
     16747, 16759, 16763, 16787, 16811, 16823, 16829, 16831, 16843, 16871, 16879, 16883, 16889, 16901, 16903, 16921, 16927, 16931, 
     16937, 16943, 16963, 16979, 16981, 16987, 16993, 17011, 17021, 17027, 17029, 17033, 17041, 17047, 17053, 17077, 17093, 17099, 
     17107, 17117, 17123, 17137, 17159, 17167, 17183, 17189, 17191, 17203, 17207, 17209, 17231, 17239, 17257, 17291, 17293, 17299, 
     17317, 17321, 17327, 17333, 17341, 17351, 17359, 17377, 17383, 17387, 17389, 17393, 17401, 17417, 17419, 17431, 17443, 17449, 
     17467, 17471, 17477, 17483, 17489, 17491, 17497, 17509, 17519, 17539, 17551, 17569, 17573, 17579, 17581, 17597, 17599, 17609, 
     17623, 17627, 17657, 17659, 17669, 17681, 17683, 17707, 17713, 17729, 17737, 17747, 17749, 17761, 17783, 17789, 17791, 17807, 
     17827, 17837, 17839, 17851, 17863};
#endif

  static mus_float_t all_mins[128] = {1.0000, 1.7600, 1.9797, 2.0390, 2.3435, 2.5493, 2.6394, 2.7946, 2.9617, 3.1023, 3.2180, 3.3887, 3.5241, 3.6122, 3.7680, 3.8738, 3.9802, 4.1438, 4.2210, 4.2890, 4.4824, 4.5866, 4.6052, 4.7280, 4.8531, 5.0050, 5.0640, 5.1573, 5.2413, 5.3623, 5.4800, 5.5259, 5.6320, 5.7167, 5.7648, 5.9260, 5.9291, 6.1063, 6.1247, 6.2928, 6.3296, 6.4582, 6.4755, 6.5454, 6.6312, 6.6921, 6.8293, 6.8630, 6.9144, 7.0054, 7.0637, 7.1368, 7.1997, 7.2564, 7.3299, 7.3515, 7.4959, 7.5951, 7.6448, 7.6004, 7.7645, 7.7988, 7.9057, 7.9675, 8.0372, 8.0641, 8.1492, 8.1759, 8.2203, 8.1858, 8.3294, 8.4729, 8.4355, 8.5093, 8.6166, 8.6292, 8.7018, 8.7310, 8.8532, 8.8399, 8.9659, 9.0766, 8.9469, 9.0279, 9.1400, 9.2143, 9.3401, 9.3266, 9.3608, 9.4148, 9.4762, 9.5310, 9.6718, 9.6238, 9.6045, 9.7150, 9.8212, 9.7808, 9.8684, 9.9443, 10.0642, 10.0883, 10.0825, 10.1311, 10.1809, 10.2442, 10.2892, 10.3212, 10.4396, 10.5102, 10.5072, 10.4710, 10.6064, 10.6388, 10.6937, 10.6756, 10.7588, 10.8087, 10.8940, 10.8897, 10.9437, 10.9655, 11.0969, 11.0834, 11.1658, 11.1605, 11.1939, 11.3151};

  static mus_float_t odd_mins[128] = {1.0000, 1.5390, 1.7387, 2.0452, 2.3073, 2.5227, 2.6183, 2.7907, 2.8862, 3.0534, 3.1766, 3.3619, 3.4745, 3.5985, 3.7384, 3.8570, 3.9264, 4.0695, 4.1719, 4.3580, 4.4485, 4.5810, 4.6616, 4.7864, 4.8868, 5.0064, 5.0888, 5.0889, 5.2634, 5.3531, 5.4189, 5.5633, 5.6030, 5.7405, 5.8333, 5.9776, 6.0191, 6.1445, 6.1815, 6.2725, 6.3216, 6.4032, 6.4742, 6.5992, 6.6249, 6.7092, 6.7852, 6.8280, 6.9901, 6.9471, 7.0877, 7.0801, 7.2526, 7.3293, 7.3642, 7.4191, 7.4889, 7.5881, 7.6178, 7.6996, 7.7755, 7.8170, 7.9041, 7.9574, 8.0448, 8.1381, 8.1280, 8.2279, 8.2749, 8.3285, 8.4772, 8.3664, 8.5585, 8.4879, 8.6513, 8.6513, 8.7070, 8.7153, 8.8678, 8.9481, 8.9263, 8.8955, 9.0607, 9.1893, 9.1729, 9.2133, 9.3555, 9.3240, 9.3316, 9.4217, 9.4566, 9.5527, 9.6459, 9.6612, 9.7356, 9.7654, 9.8383, 9.9209, 9.9651, 9.9804, 9.9646, 10.0458, 10.1114, 10.1772, 10.1158, 10.1983, 10.3094, 10.3255, 10.4207, 10.4081, 10.6167, 10.5872, 10.6006, 10.6208, 10.6856, 10.7399, 10.7935, 10.8122, 10.9204, 10.9206, 11.0192, 11.0363, 11.1313, 11.2049, 11.1221, 11.2516, 11.2823, 11.2612};

  static mus_float_t prime_mins[128] = {1.0000, 1.7600, 1.9798, 2.1921, 2.4768, 2.8054, 3.0618, 3.2628, 3.3822, 3.6019, 3.7784, 3.9359, 4.1545, 4.3244, 4.4669, 4.6015, 4.7191, 4.8554, 5.0150, 5.1886, 5.3250, 5.4444, 5.5636, 5.6457, 5.8110, 6.0603, 6.1342, 6.1909, 6.3650, 6.4518, 6.7015, 6.8403, 6.8471, 6.9918, 7.1647, 7.2743, 7.2923, 7.3972, 7.4571, 7.7036, 7.8670, 7.9689, 8.0462, 8.1786, 8.1587, 8.2656, 8.4225, 8.4701, 8.6383, 8.6779, 8.6547, 8.8203, 8.9537, 9.1135, 9.1486, 9.4076, 9.5698, 9.4963, 9.4489, 9.6577, 9.8482, 9.7939, 9.7212, 9.9180, 10.2736, 10.2168, 10.3295, 10.4019, 10.4139, 10.4406, 10.5788, 10.5922, 10.7617, 10.7115, 11.0223, 11.0723, 10.8825, 11.1288, 11.2266, 11.4514, 11.5009, 11.4800, 11.5157, 11.5609, 11.6403, 11.5250, 11.9270, 11.9889, 12.2189, 12.0405, 12.1250, 12.1239, 12.3234, 12.3723, 12.5103, 12.6686, 12.6382, 12.8988, 13.0893, 13.1158, 13.0683, 13.3991, 13.4243, 13.1969, 13.1439, 13.2161, 13.5618, 13.6628, 13.6943, 13.7797, 13.7655, 13.8646, 14.0364, 14.2144, 14.1698, 14.4672, 14.6007, 14.5453, 14.3907, 14.5061, 14.5506, 14.8748, 14.6063, 14.8769, 14.7724, 14.9200, 14.9658, 14.6466};

  static mus_float_t even_mins[128] = {1.0000, 1.7602, 2.0215, 2.4306, 2.6048, 2.8370, 3.0470, 3.1975, 3.4540, 3.5587, 3.6561, 3.7869, 3.9726, 4.0967, 4.1921, 4.3250, 4.4630, 4.5694, 4.7415, 4.8395, 4.9197, 5.0552, 5.1479, 5.2532, 5.4032, 5.4523, 5.6204, 5.7317, 5.7663, 5.9070, 5.9878, 6.0611, 6.1626, 6.2228, 6.3623, 6.4321, 6.5805, 6.5366, 6.6832, 6.7481, 6.8810, 6.9415, 7.0552, 7.0483, 7.1652, 7.2760, 7.2926, 7.4670, 7.5877, 7.6224, 7.6548, 7.7863, 7.7505, 7.8451, 8.0075, 8.0420, 8.1156, 8.1027, 8.1945, 8.3124, 8.3566, 8.3910, 8.4139, 8.5009, 8.6650, 8.7856, 8.8244, 8.7974, 8.8704, 9.0010, 9.0999, 8.9855, 9.1604, 9.2507, 9.2084, 9.3920, 9.3628, 9.3359, 9.5324, 9.5713, 9.5437, 9.6632, 9.7525, 9.7487, 9.6937, 9.8045, 9.8747, 9.9683, 10.1103, 10.2395, 10.1678, 10.2333, 10.1208, 10.4361, 10.4453, 10.5480, 10.4547, 10.5295, 10.4140, 10.4721, 10.7995, 10.8253, 10.8315, 10.7829, 10.9236, 10.9172, 10.9844, 11.0254, 11.0804, 11.2647, 11.3526, 11.2728, 11.1978, 11.3362, 11.3367, 11.5548, 11.3673, 11.5805, 11.6248, 11.7167, 11.5799, 11.7805, 11.7526, 11.8398, 11.8194, 11.9815, 11.8908, 11.9243};

  static mus_float_t min_8[4] = {19.4199, 19.7800, 21.1471, 25.4193};
  static mus_float_t min_9[4] = {31.3912, 31.6276, 31.6281, 40.2509};
  static mus_float_t min_10[4] = {49.8672, 48.8125, 51.6272, 70.1400};
  static mus_float_t min_11[4] = {77.3502, 78.9374, 78.0793, 102.6190};

#define USE_CLM_RANDOM (!HAVE_SCHEME)

static mus_float_t local_random(mus_float_t val)
{
#if USE_CLM_RANDOM
  return(mus_random(val));
#else
  return(val * (1.0  - (s7_random(s7, NULL) * 2.0)));
#endif
}


static mus_float_t local_frandom(mus_float_t val)
{
#if USE_CLM_RANDOM
  return(mus_frandom(val));
#else
  return(val * s7_random(s7, NULL));
#endif
}


typedef struct {
  mus_float_t pk;
  mus_float_t *phases;
} pk_data;


/* -------------------------------------------------------------------------------- */

#define ALL 0
#define ODD 1
#define EVEN 2
#define PRIME 3

#define FFT_MULT 200
  /* if 64, errors or .005 are common 
   * if 128, which works in 99% of the cases, errors can be as much as .002
   */

#define S_fpsap "fpsap"

static XEN g_fpsap(XEN x_choice, XEN x_n, XEN start_phases, XEN x_size, XEN x_increment)
{
  #define H_fpsap "(" S_fpsap " choice n phases (size 6000) (increment 0.06)) searches \
for a peak-amp minimum using a simulated annealing form of the genetic algorithm.  choice: 0=all, 1=odd, 2=even, 3=prime."

  #define INCR_DOWN 0.9
  #define INCR_MAX 1.0
  #define INCR_MIN 0.0005
  #define RETRIES 10
  #define RETRY_MULT 2
  #define INIT_TRIES 1000

  int choice, n, size, counts = 0, day_counter = 0, free_top = 0, fft_size = 0;
  mus_float_t increment = INCR_MAX, orig_incr, local_best = 1000.0, incr_mult = INCR_DOWN, overall_min;
  mus_float_t *min_phases = NULL, *temp_phases = NULL, *diff_phases = NULL, *initial_phases = NULL;
  char *choice_name[4] = {"all", "odd", "even", "prime"};
  pk_data **choices = NULL, **free_choices = NULL;
  mus_float_t *rl, *im;
  const char *file = NULL;
  bool just_best = false;

  auto mus_float_t saved_min(int ch, int nn);
  auto mus_float_t get_peak(mus_float_t *phases);
  auto pk_data *next_choice(pk_data *data);
  auto bool day(void);

  mus_float_t saved_min(int ch, int nn)
  {
    if (nn <= 128)
      {
	switch (ch)
	  {
	  case ALL:   return(all_mins[nn - 1]);
	  case ODD:   return(odd_mins[nn - 1]);
	  case EVEN:  return(even_mins[nn - 1]);
	  case PRIME: return(prime_mins[nn - 1]);
	  }
      }
    if (nn == 256) return(min_8[ch]);
    if (nn == 512) return(min_9[ch]);
    if (nn == 1024) return(min_10[ch]);
    if (nn == 2048) return(min_11[ch]);
    return((mus_float_t)nn);
  }

  mus_float_t get_peak(mus_float_t *phases)
  {
    int i, m;
    mus_float_t pi2, mx_sin, mx_cos;

    pi2 = M_PI / 2.0;
    memset((void *)rl, 0, fft_size * sizeof(mus_float_t));
    memset((void *)im, 0, fft_size * sizeof(mus_float_t));

    for (m = 0; m < n; m++)
      {
	int bin;
	mus_float_t phi;
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
    /* real part is sine reconstruction, imaginary part is cosine, we're interested in both! */
    /*   we could also add and subtract the 2 to get 2 more cases "for free", amp sqrt(2), phase asin(cos(0)/sqrt(2)) */
    /*   and repeat this with a shift (rotation from i) for 2n other cases */
    /*   resultant amp is between 0 and 2 (cosine) */

    mx_sin = fabs(rl[0]);
    mx_cos = fabs(im[0]);
    for (i = 1; i < fft_size; i++)
      {
	mus_float_t mxtemp;
	mxtemp = fabs(rl[i]);
	if (mxtemp > mx_sin)
	  mx_sin = mxtemp;
	mxtemp = fabs(im[i]);
	if (mxtemp > mx_cos)
	  mx_cos = mxtemp;
      }

    if (mx_sin <= mx_cos)
      return(mx_sin);

    /* use the cosine case, but make it sine-based with 0.0 initial phase for the fundamental */
    for (m = 1; m < n; m++)
      {
	int bin;
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
	phases[m] += (0.5 * (bin - 1));
      }

    return(mx_cos);
  }
  
  pk_data *next_choice(pk_data *data)
  {
    mus_float_t *phases;
    mus_float_t cur_min, temp_min = 100000.0, pk = 100000.0;
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
	  temp_phases[i] = fmod(phases[i] + local_random(increment) + local_random(increment), 2.0); /* not mus_frandom! */
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
	int happy = 3;
	for (k = 1; k < len; k++)
	  diff_phases[k] = new_pk->phases[k] - data->phases[k];

	while (happy > 0)
	  {
	    for (k = 1; k < len; k++)
	      temp_phases[k] = fmod(new_pk->phases[k] + local_frandom(diff_phases[k]), 2.0); /* use frandom 30-mar-11 */
	    pk = get_peak(temp_phases);

	    if (pk < new_pk->pk)
	      {
		new_pk->pk = pk;
		for (k = 1; k < len; k++) new_pk->phases[k] = temp_phases[k];
		happy = 3;
	      }
	    else happy--;
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
	    for (k = 1; k < len; k++) min_phases[k] = new_pk->phases[k];
	    if (pk < overall_min)
	      {
		if (file)
		  ofile = fopen(file, "a");
		else ofile = stderr;
		fprintf(ofile, "%s, %d %f #(", choice_name[choice], n, pk);
		for (k = 0; k < len - 1; k++) fprintf(ofile, "%f ", min_phases[k]);
		fprintf(ofile, "%f)\n", min_phases[len - 1]);
		if (file) fclose(ofile);
		overall_min = pk;
	      }
	  }

	day_counter = 0;
      }
    return(new_pk);
  }

  bool day(void)
  {
    int i, j = 0, k, len;
    mus_float_t sum = 0.0, avg;
    len = size;
    day_counter++;
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
	/* .9^50 = .005, so starting at .1 bottoms out at .0005
	 *   perhaps the counts variable should be (ceiling (log INCR_MIN incr_mult)) = 90 or so in the current case
	 *   incr_mult is currently always INCR_DOWN = .9
	 */
	increment *= incr_mult;
	if (increment < INCR_MIN) 
	  {
	    increment = INCR_MIN;
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
  if ((choice < ALL) || (choice > PRIME))
    choice = ALL;

  n = XEN_TO_C_INT(x_n);

  if (XEN_INTEGER_P(x_size))
    size = XEN_TO_C_INT(x_size);
  else size = 6000; /* was 3000 */

  if (XEN_DOUBLE_P(x_increment))
    increment = XEN_TO_C_DOUBLE(x_increment);
  else increment = 0.06; /* was .03 */

  counts = 50; /* was 100 */
  orig_incr = increment;
  incr_mult = INCR_DOWN;
  file = "test.data";
  just_best = false;

  if (XEN_VECTOR_P(start_phases))
    {
      int i;
      initial_phases = (mus_float_t *)malloc(n * sizeof(mus_float_t));
      for (i = 0; i < n; i++)
	initial_phases[i] = (mus_float_t)XEN_TO_C_DOUBLE(XEN_VECTOR_REF(start_phases, i));
    }

  min_phases = (mus_float_t *)calloc(n, sizeof(mus_float_t));
  overall_min = saved_min(choice, n);
  temp_phases = (mus_float_t *)calloc(n, sizeof(mus_float_t));
  diff_phases = (mus_float_t *)calloc(n, sizeof(mus_float_t));

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
    rl = (mus_float_t *)calloc(fft_size, sizeof(mus_float_t));
    im = (mus_float_t *)calloc(fft_size, sizeof(mus_float_t));

    choices = (pk_data **)calloc(size, sizeof(pk_data *));
    free_choices = (pk_data **)calloc(size, sizeof(pk_data *));

    for (start = 0; start < size; start++)
      {
	choices[start] = (pk_data *)calloc(1, sizeof(pk_data));
	choices[start]->phases = (mus_float_t *)calloc(n, sizeof(mus_float_t));
      }

    free_top = 0;
    day_counter = 0;
    local_best = (mus_float_t)n;
    increment = orig_incr;
    
    for (start = 0; start < size; start++)
      {
	mus_float_t pk, local_pk = 100000.0;
	int k, init_try;
	
	for (init_try = 0;  init_try < INIT_TRIES; init_try++)
	  {
	    if (initial_phases)
	      {
		for (k = 1; k < n; k++) 
		  temp_phases[k] = initial_phases[k] + local_random(increment) + local_random(increment);
	      }
	    else
	      {
		for (k = 1; k < n; k++) 
		  temp_phases[k] = local_frandom(2.0);
	      }
	    pk = get_peak(temp_phases);
	    
	    if (pk < local_best)
	      {
		local_best = pk;
		if ((!just_best) ||
		    (pk < overall_min))
		  {
		    FILE *ofile;
		    for (k = 1; k < n; k++) min_phases[k] = temp_phases[k];
		    if (pk < overall_min)
		      {
			if (file)
			  ofile = fopen(file, "a");
			else ofile = stderr;
			fprintf(ofile, "%s, %d %f #(", choice_name[choice], n, pk);
			for (k = 0; k < n - 1; k++) fprintf(ofile, "%f ", min_phases[k]);
			fprintf(ofile, "%f)\n", min_phases[n - 1]);
			if (file) fclose(ofile);
			overall_min = pk;
		      }
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
  }
  
  free(temp_phases);
  free(diff_phases);
  free(rl);
  free(im);
  free(free_choices);
  if (initial_phases) free(initial_phases);

  {
    int i;
    for (i = 0; i < size; i++)
      {
	free(choices[i]->phases);
	free(choices[i]);
      }
    free(choices);
  }

  return(XEN_LIST_2(C_TO_XEN_DOUBLE(local_best), 
		    xen_make_vct(n, min_phases)));
}



/* -------------------------------------------------------------------------------- */

#define S_fpgrid "fpgrid"

static char *choice_name[4] = {"all", "odd", "even", "prime"};

static void save_peaks(const char *file, int choice, int n, mus_float_t peak, mus_float_t *phases)
{
  FILE *ofile;
  int k;
  ofile = fopen(file, "a");
  fprintf(ofile, "%s, %d %f #(", choice_name[choice], n, peak);
  for (k = 0; k < n - 1; k++) 
    fprintf(ofile, "%f ", phases[k]);
  fprintf(ofile, "%f)\n", phases[n - 1]);
  fclose(ofile);
}


static XEN g_fpgrid(XEN x_choice, XEN x_n, XEN start_phases)
{
  int choice, current_choice, n, size, fft_size, hits, big_counter;
  mus_float_t increment = .01, overall_min, current_min;
  mus_float_t *initial_phases = NULL;
  pk_data **choices = NULL;
  mus_float_t *rl, *im;

  auto mus_float_t saved_min(int ch, int nn);
  auto mus_float_t get_peak(mus_float_t *phases);

  mus_float_t saved_min(int ch, int nn)
  {
    if (nn <= 128)
      {
	switch (ch)
	  {
	  case ALL:   return(all_mins[nn - 1]);
	  case ODD:   return(odd_mins[nn - 1]);
	  case EVEN:  return(even_mins[nn - 1]);
	  case PRIME: return(prime_mins[nn - 1]);
	  }
      }
    if (nn == 256) return(min_8[ch]);
    if (nn == 512) return(min_9[ch]);
    if (nn == 1024) return(min_10[ch]);
    if (nn == 2048) return(min_11[ch]);
    return((mus_float_t)nn);
  }

  mus_float_t get_peak(mus_float_t *phases)
  {
    int i, m;
    mus_float_t pi2, mx_sin, mx_cos;

    pi2 = M_PI / 2.0;
    memset((void *)rl, 0, fft_size * sizeof(mus_float_t));
    memset((void *)im, 0, fft_size * sizeof(mus_float_t));

    for (m = 0; m < n; m++)
      {
	int bin;
	mus_float_t phi;
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
    /* real part is sine reconstruction, imaginary part is cosine, we're interested in both! */
    /*   we could also add and subtract the 2 to get 2 more cases "for free", amp sqrt(2), phase asin(cos(0)/sqrt(2)) */
    /*   and repeat this with a shift (rotation from i) for 2n other cases */
    /*   resultant amp is between 0 and 2 (cosine) */

    mx_sin = fabs(rl[0]);
    mx_cos = fabs(im[0]);
    for (i = 1; i < fft_size; i++)
      {
	mus_float_t mxtemp;
	mxtemp = fabs(rl[i]);
	if (mxtemp > mx_sin)
	  mx_sin = mxtemp;
	mxtemp = fabs(im[i]);
	if (mxtemp > mx_cos)
	  mx_cos = mxtemp;
      }

    if (mx_sin <= mx_cos)
      return(mx_sin);

    /* use the cosine case, but make it sine-based with 0.0 initial phase for the fundamental */
    for (m = 1; m < n; m++)
      {
	int bin;
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
	phases[m] += (0.5 * (bin - 1));
      }

    return(mx_cos);
  }
  

  choice = XEN_TO_C_INT(x_choice);
  if ((choice < ALL) || (choice > PRIME))
    choice = ALL;

  n = XEN_TO_C_INT(x_n);
  
  size = n - 1;
  increment = 0.001;

  if (XEN_VECTOR_P(start_phases))
    {
      int i;
      initial_phases = (mus_float_t *)malloc(n * sizeof(mus_float_t));
      for (i = 0; i < n; i++)
	initial_phases[i] = (mus_float_t)XEN_TO_C_DOUBLE(XEN_VECTOR_REF(start_phases, i));
    }
  else return(XEN_FALSE);

  overall_min = saved_min(choice, n);

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
    rl = (mus_float_t *)calloc(fft_size, sizeof(mus_float_t));
    im = (mus_float_t *)calloc(fft_size, sizeof(mus_float_t));

    choices = (pk_data **)calloc(size, sizeof(pk_data *));

    for (start = 0; start < size; start++)
      {
	int k;
	choices[start] = (pk_data *)calloc(1, sizeof(pk_data));
	choices[start]->phases = (mus_float_t *)calloc(n, sizeof(mus_float_t));
	for (k = 1; k < n; k++)
	  choices[start]->phases[k] = initial_phases[k];
      }
  }

    /* now the initial population has the initial phases */

  current_min = overall_min;
  current_choice = 0;

  while (true)
    {
      int k;
      mus_float_t peak;

      for (k = 1; k < n; k++)
	{
	  mus_float_t phs, orig_incr;
	  orig_incr = increment;

	  phs = choices[current_choice]->phases[k];
	  choices[current_choice]->phases[k] += increment;
	  peak = get_peak(choices[current_choice]->phases);
	  if (peak >= overall_min)
	    choices[current_choice]->phases[k] = phs;
	  else
	    {
	      choices[current_choice]->pk = peak;
	      if (peak < current_min)
		{
		  save_peaks("test.data", choice, n, peak, choices[current_choice]->phases);
		  current_min = peak;
		}
	      while (true)
		{
		  mus_float_t local_peak;
		  phs = choices[current_choice]->phases[k];
		  choices[current_choice]->phases[k] += increment;
		  local_peak = get_peak(choices[current_choice]->phases);
		  if (local_peak >= peak)
		    {
		      choices[current_choice]->phases[k] = phs;
		      break;
		    }
		  else
		    {
		      peak = local_peak;
		      choices[current_choice]->pk = peak;
		      if (peak < current_min)
			{
			  save_peaks("test.data", choice, n, peak, choices[current_choice]->phases);
			  current_min = local_peak;
			}
		    }
		}
	      current_choice++;
	      if (current_choice >= size) break;
	    }

	  increment = orig_incr;
	  phs = choices[current_choice]->phases[k];
	  choices[current_choice]->phases[k] -= increment;
	  peak = get_peak(choices[current_choice]->phases);
	  if (peak >= overall_min)
	    choices[current_choice]->phases[k] = phs;
	  else
	    {
	      choices[current_choice]->pk = peak;
	      if (peak < current_min)
		{
		  save_peaks("test.data", choice, n, peak, choices[current_choice]->phases);
		  current_min = peak;
		}
	      while (true)
		{
		  mus_float_t local_peak;
		  phs = choices[current_choice]->phases[k];
		  choices[current_choice]->phases[k] -= increment;
		  local_peak = get_peak(choices[current_choice]->phases);
		  if (local_peak >= peak)
		    {
		      choices[current_choice]->phases[k] = phs;
		      break;
		    }
		  else
		    {
		      peak = local_peak;
		      choices[current_choice]->pk = peak;
		      if (peak < current_min)
			{
			  save_peaks("test.data", choice, n, peak, choices[current_choice]->phases);
			  current_min = local_peak;
			}
		    }
		}
	      current_choice++;
	      if (current_choice >= size) break;
	    }

	}

      if (current_choice >= size) 
	{
	  overall_min = current_min;
	  current_choice = 0;
	  continue;
	}

      increment *= 0.75;
      if (increment < .00001) break;
    }

  /* now try the counter combinations */
  increment = .0001;
  current_choice = 0;
  big_counter = 0;
  hits = 0;

  while (true)
    {
      int counter;
      mus_float_t peak;

      for (current_choice = 0; current_choice < size; current_choice++)
	{
	  for (counter = 1; counter < 10000; counter++)
	    {
	      int k, bits, cur;
	      big_counter++;
	      bits = big_counter << 1;
	      
	      for (k = 1; k < n; k++)
		{
		  cur = 1 << k;
		  if (cur > bits) break;
		  if ((cur & bits) != 0)
		    choices[current_choice]->phases[k] += increment;
		}
	      peak = get_peak(choices[current_choice]->phases);
	      if (peak < choices[current_choice]->pk)
		{
		  hits++;
		  choices[current_choice]->pk = peak;
		  if (peak < current_min)
		    {
		      save_peaks("test.data", choice, n, peak, choices[current_choice]->phases);
		      current_min = peak;
		    }
		}
	      else
		{
		  for (k = 1; k < n; k++)
		    {
		      cur = 1 << k;
		      if (cur > bits) break;
		      if ((cur & bits) != 0)
			choices[current_choice]->phases[k] -= 2 * increment;
		    }
		  peak = get_peak(choices[current_choice]->phases);
		  if (peak >= choices[current_choice]->pk)
		    {
		      for (k = 1; k < n; k++)
			{
			  cur = 1 << k;
			  if (cur > bits) break;
			  if ((cur & bits) != 0)
			    choices[current_choice]->phases[k] += increment;
			}
		    }
		  else
		    {
		      hits++;
		      choices[current_choice]->pk = peak;
		      if (peak < current_min)
			{
			  save_peaks("test.data", choice, n, peak, choices[current_choice]->phases);
			  current_min = peak;
			}
		    }
		}
	    }
	}

      overall_min = current_min;
      if (hits < 3) 
	{
	  increment *= 0.75;
	  big_counter = 0;
	  if (increment < .00001) break;
	}
      hits = 0;
    }

  free(rl);
  free(im);
  if (initial_phases) free(initial_phases);

  {
    int i;
    for (i = 0; i < size; i++)
      {
	free(choices[i]->phases);
	free(choices[i]);
      }
    free(choices);
  }

  fprintf(stderr, "%d %d is done\n", choice, n);
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
XEN_NARGIFY_0(g_delete_selection_and_smooth_w, g_delete_selection_and_smooth)
XEN_ARGIFY_5(g_delete_samples_and_smooth_w, g_delete_samples_and_smooth)
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
XEN_ARGIFY_5(g_fpsap_w, g_fpsap)
XEN_ARGIFY_3(g_fpgrid_w, g_fpgrid)
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
#define g_delete_selection_and_smooth_w g_delete_selection_and_smooth
#define g_delete_samples_and_smooth_w g_delete_samples_and_smooth
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
#define g_fpsap_w g_fpsap
#define g_fpgrid_w g_fpgrid
#endif
#endif

void g_init_sig(void)
{
  XEN_DEFINE_PROCEDURE(S_scan_channel,                g_scan_channel_w,                1, 5, 0, H_scan_channel);
  XEN_DEFINE_PROCEDURE(S_scan_chan,                   g_scan_chan_w,                   1, 5, 0, H_scan_chan);
  XEN_DEFINE_PROCEDURE(S_find_channel,                g_find_channel_w,                1, 4, 0, H_find_channel);
  XEN_DEFINE_PROCEDURE(S_count_matches,               g_count_matches_w,               1, 4, 0, H_count_matches);
  XEN_DEFINE_PROCEDURE(S_map_chan,                    g_map_chan_w,                    1, 6, 0, H_map_chan);
  XEN_DEFINE_PROCEDURE(S_map_channel,                 g_map_channel_w,                 1, 6, 0, H_map_channel);
  XEN_DEFINE_PROCEDURE(S_ptree_channel,               g_ptree_channel_w,               1, 8, 0, H_ptree_channel);

  XEN_DEFINE_PROCEDURE(S_smooth_sound,                g_smooth_sound_w,                0, 4, 0, H_smooth_sound);
  XEN_DEFINE_PROCEDURE(S_smooth_selection,            g_smooth_selection_w,            0, 0, 0, H_smooth_selection);
  XEN_DEFINE_PROCEDURE(S_delete_selection_and_smooth, g_delete_selection_and_smooth_w, 0, 0, 0, H_delete_selection_and_smooth);
  XEN_DEFINE_PROCEDURE(S_delete_samples_and_smooth,   g_delete_samples_and_smooth_w,   2, 3, 0, H_delete_samples_and_smooth);
  XEN_DEFINE_PROCEDURE(S_reverse_sound,               g_reverse_sound_w,               0, 3, 0, H_reverse_sound);
  XEN_DEFINE_PROCEDURE(S_reverse_selection,           g_reverse_selection_w,           0, 0, 0, H_reverse_selection);
  XEN_DEFINE_PROCEDURE(S_swap_channels,               g_swap_channels_w,               0, 8, 0, H_swap_channels);
  XEN_DEFINE_PROCEDURE(S_insert_silence,              g_insert_silence_w,              2, 2, 0, H_insert_silence);

  XEN_DEFINE_PROCEDURE(S_scale_selection_to,          g_scale_selection_to_w,          0, 1, 0, H_scale_selection_to);
  XEN_DEFINE_PROCEDURE(S_scale_selection_by,          g_scale_selection_by_w,          1, 0, 0, H_scale_selection_by);
  XEN_DEFINE_PROCEDURE(S_scale_to,                    g_scale_to_w,                    0, 3, 0, H_scale_to);
  XEN_DEFINE_PROCEDURE(S_scale_by,                    g_scale_by_w,                    1, 2, 0, H_scale_by);
  XEN_DEFINE_PROCEDURE(S_env_selection,               g_env_selection_w,               1, 1, 0, H_env_selection);
  XEN_DEFINE_PROCEDURE(S_env_sound,                   g_env_sound_w,                   1, 6, 0, H_env_sound);
  XEN_DEFINE_SAFE_PROCEDURE(S_fft,                    g_fft_w,                         2, 1, 0, H_fft);
  XEN_DEFINE_SAFE_PROCEDURE(S_snd_spectrum,           g_snd_spectrum_w,                1, 6, 0, H_snd_spectrum);
  XEN_DEFINE_SAFE_PROCEDURE(S_convolve_with,          g_convolve_with_w,               1, 4, 0, H_convolve_with);
  XEN_DEFINE_SAFE_PROCEDURE(S_convolve_selection_with, g_convolve_selection_with_w,     1, 1, 0, H_convolve_selection_with);
  XEN_DEFINE_PROCEDURE(S_src_sound,                   g_src_sound_w,                   1, 4, 0, H_src_sound);
  XEN_DEFINE_PROCEDURE(S_src_selection,               g_src_selection_w,               1, 1, 0, H_src_selection);
  XEN_DEFINE_PROCEDURE(S_filter_channel,              g_filter_channel_w,              1, 8, 0, H_filter_channel);
  XEN_DEFINE_PROCEDURE(S_filter_sound,                g_filter_sound_w,                1, 5, 0, H_filter_sound);
  XEN_DEFINE_PROCEDURE(S_filter_selection,            g_filter_selection_w,            1, 2, 0, H_filter_selection);

  XEN_DEFINE_PROCEDURE(S_reverse_channel,             g_reverse_channel_w,             0, 5, 0, H_reverse_channel);
  XEN_DEFINE_PROCEDURE(S_clm_channel,                 g_clm_channel_w,                 1, 7, 0, H_clm_channel);
  XEN_DEFINE_PROCEDURE(S_env_channel,                 g_env_channel_w,                 1, 5, 0, H_env_channel);
  XEN_DEFINE_PROCEDURE(S_env_channel_with_base,       g_env_channel_with_base_w,       1, 6, 0, H_env_channel_with_base);
  XEN_DEFINE_PROCEDURE(S_ramp_channel,                g_ramp_channel_w,                2, 5, 0, H_ramp_channel);
  XEN_DEFINE_PROCEDURE(S_xramp_channel,               g_xramp_channel_w,               2, 6, 0, H_xramp_channel);
  XEN_DEFINE_PROCEDURE(S_smooth_channel,              g_smooth_channel_w,              0, 5, 0, H_smooth_channel);
  XEN_DEFINE_PROCEDURE(S_src_channel,                 g_src_channel_w,                 1, 5, 0, H_src_channel);
  XEN_DEFINE_PROCEDURE(S_pad_channel,                 g_pad_channel_w,                 2, 3, 0, H_pad_channel);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_sinc_width, g_sinc_width_w, H_sinc_width,
				   S_setB S_sinc_width, g_set_sinc_width_w,  0, 0, 1, 0);
#if HAVE_NESTED_FUNCTIONS
  XEN_DEFINE_PROCEDURE(S_find_min_peak_phases, g_find_min_peak_phases_w, 0, 0, 1, H_find_min_peak_phases);
  XEN_DEFINE_PROCEDURE(S_fpsap, g_fpsap_w, 3, 2, 0, H_fpsap);
  XEN_DEFINE_PROCEDURE(S_fpgrid, g_fpgrid_w, 3, 0, 0, "");
#endif
}

