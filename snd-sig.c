#include "snd.h"

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
	  return(0); /* never called, presumably */
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
	      XEN_LIST_5(C_TO_XEN_STRING(caller),
			 C_TO_XEN_INT(cp->sound->index),
			 C_TO_XEN_INT(cp->chan),
			 C_TO_XEN_INT(pos),
			 edpos));
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

off_t end_to_sample(XEN end, chan_info *cp, int edpos, const char *caller)
{
  off_t last;
  last = XEN_TO_C_OFF_T_OR_ELSE(end, cp->samples[edpos] - 1);
  if (last < 0) 
    XEN_ERROR(NO_SUCH_SAMPLE,
	      XEN_LIST_2(C_TO_XEN_STRING(caller),
			 end));
  return(last);
}


static sync_state *get_sync_state_1(snd_state *ss, snd_info *sp, chan_info *cp, off_t beg, int regexpr, 
				    int forwards, off_t prebeg, XEN edpos, const char *caller, int arg_pos)
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
      si = snd_sync(ss, sp->sync);
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

static sync_state *get_sync_state(snd_state *ss, snd_info *sp, chan_info *cp, off_t beg, int regexpr, int forwards, XEN edpos, const char *caller, int arg_pos)
{
  return(get_sync_state_1(ss, sp, cp, beg, regexpr, forwards, 0, edpos, caller, arg_pos));
}

static sync_state *get_sync_state_without_snd_fds(snd_state *ss, snd_info *sp, chan_info *cp, off_t beg, int regexpr)
{
  sync_info *si = NULL;
  off_t dur;
  int i;
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

#if 0
/* the fht sometimes out-performs fftw's fft -- especially if len is 65536
 */
static float *fht_sines = NULL, *fht_cosines = NULL;
static int fht_last_length = 0;
#define TWO_PI (2.0 * M_PI)

static void make_sines(int length)
{
  int i, j;
  double freq, incr;
  if (length != fht_last_length)
    {
      if (fht_sines) free(fht_sines);
      if (fht_cosines) free(fht_cosines);
      fht_sines = (float *)malloc(length * sizeof(float));
      fht_cosines = (float *)malloc(length * sizeof(float));
      fht_last_length = length;
      incr = TWO_PI / (double)length;
      for (i = 0, freq = 0.0; i < length; i++, freq += incr) 
	fht_cosines[i] = cos(freq);
      for (i = 0, j = length / 4; i < length; i++)
	{
	  fht_sines[j++] = fht_cosines[i];
	  if (j == length) j = 0;
        }
    }
}

void fht(int powerOfFour, float *array)
{
  /*  In place Fast Hartley Transform of floating point data in array.
      Size of data array must be power of four. Lots of sets of four 
      inline code statements, so it is verbose and repetitive, but fast. 
      The Fast Hartley Transform algorithm is patented, and is documented
      in the book "The Hartley Transform", by Ronald N. Bracewell.
      This routine was converted to C from a BASIC routine in the above book,
      that routine Copyright 1985, The Board of Trustees of Stanford University 	
   */
  /* fht is its own inverse -- run it again to return to original data */

  register int j = 0, i = 0, k = 0, L = 0;
  int n = 0, n4 = 0, d1 = 0, d2 = 0, d3 = 0, d4 = 0, d5 = 1, d6 = 0, d7 = 0, d8 = 0, d9 = 0;
  int L1 = 0, L2 = 0, L3 = 0, L4 = 0, L5 = 0, L6 = 0, L7 = 0, L8 = 0;
  int n_over_d3;
  float r = 0.0;
  int a1 = 0, a2 = 0, a3 = 0;
  float t = 0.0, t1 = 0.0, t2 =0.0, t3 = 0.0, t4 = 0.0, t5 = 0.0, t6 = 0.0, t7 = 0.0, t8 = 0.0;
  float t9 = 0.0, t0 = 0.0;
  if (powerOfFour < 1) return;
  n = (int)(pow(4.0, (double)powerOfFour));
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
      d1 = snd_ipow2(L + L - 3);
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
	      a1 = k * n_over_d3;
	      a2 = 2 * a1;
	      a3 = 3 * a1;
	      /* (these never even get close to n so the mod is not needed)
	      a1 = (int) (k * n_over_d3) % n;
	      a2 = (a1 + a1) % n;
	      a3 = (a1 + a2) % n;
	      */
	      /* use of table-lookup here speeds up the overall function by a factor of about 8! */
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
}
#endif

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
  int scfd, fltfd, fftsize, ipow, dataformat;
  off_t filtersize = 0, filesize = 0, dataloc;
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
					    mus_bytes_per_sample(hdr->format),
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
	  snd_remove(saved_chan_file, TRUE);
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
		  else snd_remove(ofile, TRUE);
		  update_graph(ucp); 
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
      scale_channel(ncp, ur_scalers[j], beg, frames, ncp->edit_ctr);
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

void scale_to(snd_state *ss, snd_info *sp, chan_info *cp, Float *ur_scalers, int len, int selection)
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
      if (!(selection_is_active())) return;
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
		val = selection_maxamp(ncp);
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
	  frames = CURRENT_SAMPLES(ncp);
	}
      scale_channel(ncp, scalers[i], beg, frames, ncp->edit_ctr);
    }
  FREE(scalers);
  free_sync_info(si);
}

static void swap_channels(snd_state *ss, off_t beg, off_t dur, snd_fd *c0, snd_fd *c1)
{
  chan_info *cp0, *cp1;
  snd_info *sp0;
  file_info *hdr0 = NULL, *hdr1 = NULL;
  int j, ofd0 = 0, ofd1 = 0, datumb = 0, temp_file, err = 0;
  off_t k;
  mus_sample_t **data0, **data1;
  mus_sample_t *idata0, *idata1;
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
	  free_file_info(hdr0);
	  snd_error("can't open swap-channels temp file %s: %s\n", ofile0, strerror(errno));
	  return;
	}
      datumb = mus_bytes_per_sample(hdr0->format);
      ofile1 = snd_tempnam(ss);
      hdr1 = make_temp_header(ofile1, SND_SRATE(sp0), 1, dur, (char *)S_swap_channels);
      ofd1 = open_temp_file(ofile1, 1, hdr1, ss);
      if (ofd1 == -1)
	{
	  close_temp_file(ofd0, hdr0, 0, sp0);
	  free_file_info(hdr0);
	  free_file_info(hdr1);
	  if (ofile0) FREE(ofile0);
	  snd_error("can't open swap-channels temp file %s: %s\n", ofile1, strerror(errno));
	  return;
	}
    }
  else temp_file = 0;
  data0 = (mus_sample_t **)MALLOC(sizeof(mus_sample_t *));
  data0[0] = (mus_sample_t *)CALLOC(MAX_BUFFER_SIZE, sizeof(mus_sample_t)); 
  data1 = (mus_sample_t **)MALLOC(sizeof(mus_sample_t *));
  data1[0] = (mus_sample_t *)CALLOC(MAX_BUFFER_SIZE, sizeof(mus_sample_t)); 
  idata0 = data0[0];
  idata1 = data1[0];
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
}

/* -------- src -------- */

Float src_input_as_needed(void *arg, int dir) 
{
  src_state *sr = (src_state *)arg;
  snd_fd *sf;
  sf = sr->sf;
  sr->sample++;
  if (dir != sr->dir)
    {
      read_sample_change_direction(sf, dir);
      sr->dir = dir;
    }
  return(read_sample_to_float(sf));
}

src_state *make_src(snd_state *ss, Float srate, snd_fd *sf, Float initial_srate)
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
				    int from_enved, const char *origin, int over_selection, int curchan, int chans)
{
  snd_info *sp = NULL;
  snd_state *ss;
  int reporting = 0;
  int idiff, jj, full_chan;
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
  sp = cp->sound;
  ss = cp->state;
  full_chan = ((beg == 0) && (dur == CURRENT_SAMPLES(cp)));
  cur_marks = 0;
  new_marks = NULL;
  reporting = ((sp) && (dur > (MAX_BUFFER_SIZE * 4)));
  if (reporting) start_progress_report(sp, from_enved);
  ofile = snd_tempnam(ss);
  hdr = make_temp_header(ofile, SND_SRATE(sp), 1, dur, (char *)origin);
  ofd = open_temp_file(ofile, 1, hdr, ss);
  if (ofd == -1)
    return(mus_format("can't open %s temp file %s: %s\n", origin, ofile, strerror(errno)));
  data = (mus_sample_t **)MALLOC(sizeof(mus_sample_t *));
  data[0] = (mus_sample_t *)CALLOC(MAX_BUFFER_SIZE, sizeof(mus_sample_t)); 
  datumb = mus_bytes_per_sample(hdr->format);
  idata = data[0];

  sr = make_src(ss, ratio, sf, ratio);
  j = 0;
  if (egen == NULL)
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
  if (j > 0) mus_file_write(ofd, 0, j - 1, 1, data);
  close_temp_file(ofd, hdr, k * datumb, sp);
  hdr = free_file_info(hdr);
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
      update_graph(cp);
    }
  if (old_marks) FREE(old_marks);
  old_marks = NULL;
  if (new_marks) FREE(new_marks);
  new_marks = NULL;
  FREE(ofile); 
  ofile = NULL;
  FREE(data[0]);
  FREE(data);
  return(NULL);
}

void src_env_or_num(snd_state *ss, chan_info *cp, env *e, Float ratio, int just_num, 
		    int from_enved, const char *origin, int over_selection, mus_any *gen, XEN edpos, int arg_pos, Float e_base)
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
      ss->stopped_explicitly = 0;
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

static char *clm_channel(chan_info *cp, mus_any *gen, off_t beg, off_t dur, int edpos, char *caller, off_t overlap)
{
  /* calls gen over cp[beg for dur] data, replacing. */
  snd_state *ss;
  snd_info *sp;
  file_info *hdr = NULL;
  int j, ofd = 0, datumb = 0, temp_file, err = 0;
  off_t k;
  mus_sample_t **data;
  mus_sample_t *idata;
  char *ofile = NULL;
  snd_fd *sf;
  if ((beg < 0) || ((dur + overlap) <= 0)) return(NULL);
  ss = cp->state;
  sp = cp->sound;
  if (!(MUS_RUN_P(gen)))
    return(mus_format("clm-channel: %s can't handle %s generators",
		      caller,
		      mus_name(gen)));
  sf = init_sample_read_any(beg, cp, READ_FORWARD, edpos);
  if (sf == NULL) return(mus_format("%s: can't read %s[%d] channel data!", caller, sp->short_filename, cp->chan));
  if ((dur + overlap) > MAX_BUFFER_SIZE)
    {
      temp_file = 1; 
      ofile = snd_tempnam(ss);
      hdr = make_temp_header(ofile, SND_SRATE(sp), 1, dur + overlap, caller);
      ofd = open_temp_file(ofile, 1, hdr, ss);
      if (ofd == -1)
	{
	  free_snd_fd(sf); 
	  return(mus_format("can't open %s temp file %s: %s\n", caller, ofile, strerror(errno)));
	}
      datumb = mus_bytes_per_sample(hdr->format);
    }
  else temp_file = 0;
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
  return(NULL);
}

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
  int i, m, stop_point = 0;
  off_t prebeg = 0;
  off_t scdur, dur, offk;
  snd_fd **sfs;
  snd_fd *sf;
  file_info *hdr = NULL;
  int j, ofd = 0, datumb = 0, temp_file, err = 0;
  mus_sample_t **data;
  mus_sample_t *idata;
  char *ofile = NULL;
  chan_info *cp;
#if HAVE_FFTW
  int fsize, k;
  Float scale;
  Float *sndrdat = NULL, *fltdat = NULL;
#endif
  if ((!e) && (!ur_a) && (!gen)) 
    return(NULL);

  if ((gen) && (!(MUS_RUN_P(gen))))
    return(mus_format("filter-channel: %s can't handle %s generators",
		      origin,
		      mus_name(gen)));
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
#if HAVE_FFTW
  if ((!ur_a) && 
      (!gen) && 
      (!over_selection) &&   /* TODO: make fft-filter work with selection */
      ((order == 0) || (order >= 128)) && 
      ((ss->memory_available == 0) ||
       ((int)((to_c_edit_samples(ncp, edpos, origin, arg_pos) + order) / 128) < ss->memory_available))) /* this is in Kbytes */
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
	  fsize = snd_2pow2(dur);
	  sndrdat = (Float *)CALLOC(fsize, sizeof(Float));
	  fltdat = env2array(fsize, e);

	  sf = sfs[i];                                 /* init_sample_read(0, cp, READ_FORWARD); */
	  for (k = 0; k < dur; k++) 
	    sndrdat[k] = (Float)(read_sample_to_float(sf));
	  sfs[i] = free_snd_fd(sf);

	  mus_fftw(sndrdat, fsize, 1);
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
	  mus_fftw(sndrdat, fsize, -1);

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
	      snd_error("can't open %s temp file %s: %s\n", origin, ofile, strerror(errno));
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
		      snd_error("can't open %s temp file %s: %s\n", origin, ofile, strerror(errno));
		      break;
		    }
		  datumb = mus_bytes_per_sample(hdr->format);
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

static char *reverse_channel(chan_info *cp, snd_fd *sf, off_t beg, off_t dur, XEN edp, char *caller, int arg_pos)
{
  snd_state *ss;
  snd_info *sp;
  env_info *ep = NULL;
  file_info *hdr = NULL;
  int i, j, ofd = 0, datumb = 0, temp_file, err = 0, section = 0, edpos = 0;
  off_t k;
  mus_sample_t **data;
  mus_sample_t *idata;
  char *ofile = NULL;
  if ((beg < 0) || (dur <= 0)) return(NULL);
  ss = cp->state;
  sp = cp->sound;
  edpos = to_c_edit_position(cp, edp, caller, arg_pos);
  if (dur > cp->samples[edpos]) dur = cp->samples[edpos];
  if (dur > MAX_BUFFER_SIZE)
    {
      temp_file = 1; 
      ofile = snd_tempnam(ss);
      hdr = make_temp_header(ofile, SND_SRATE(sp), 1, dur, caller);
      ofd = open_temp_file(ofile, 1, hdr, ss);
      if (ofd == -1)
	return(mus_format("can't open %s temp file %s: %s\n", caller, ofile, strerror(errno)));
      datumb = mus_bytes_per_sample(hdr->format);
    }
  else temp_file = 0;
  if ((beg == 0) && (dur == cp->samples[edpos]))
    ep = amp_env_copy(cp, TRUE, edpos); /* TRUE -> reversed */
  else 
    {
      int sbin, ebin;
      mus_sample_t min1, max1;
      ep = amp_env_copy(cp, FALSE, edpos);
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
      section = 1; /* only for reverse_marks below */
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
  return(NULL);
}

static void reverse_sound(chan_info *ncp, int over_selection, XEN edpos, int arg_pos)
{
  sync_state *sc;
  sync_info *si;
  snd_state *ss;
  snd_info *sp;
  int i, stop_point = 0;
  off_t dur;
  snd_fd **sfs;
  char *errmsg = NULL;
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
      ss->stopped_explicitly = 0;
      for (i = 0; i <= stop_point; i++)
	{
	  cp = si->cps[i];
	  undo_edit(cp, 1);
	}
    }
  free_sync_state(sc);
}

void amp_env_env_selection_by(chan_info *cp, mus_any *e, off_t beg, off_t num, int pos);

/* amplitude envelopes */
/*   changed to use mus_env 20-Dec-00 */
/*   changed to use virtual envs (fragment eds) 8-Apr-02 */

void apply_env(chan_info *cp, env *e, off_t beg, off_t dur, Float scaler, int regexpr, 
	       int from_enved, const char *origin, mus_any *gen, XEN edpos, int arg_pos, Float e_base)
{
  /* four cases here: 
   *    if only one Y value in env, use scale_channel
   *    if step env (base=0), use sequence of scale_channels
   *    if linear segments and no underlying ramps, use sequence of ramp_channels and scale_channels to mimic env
   *    else use mus_env and multiply every sample
   */
  snd_fd *sf = NULL;
  snd_info *sp;
  sync_info *si;
  sync_state *sc = NULL;
  snd_fd **sfs;
  file_info *hdr = NULL;
  int i, j, ofd = 0, datumb = 0, temp_file = FALSE, err = 0, scalable = TRUE, rampable = TRUE, k, len;
  off_t ioff;
  mus_sample_t **data;
  mus_sample_t *idata;
  int reporting = 0;
  Float val[1];
  char *ofile = NULL, *tmpstr = NULL;
  snd_state *ss;
  mus_any *egen;
  off_t *passes;
  double *rates;
  Float egen_val;
  
  if ((!e) && (!gen)) return;
  if (regexpr) 
    {
      /* both beg and dur are meaningless here -- use selection bounds (per chan) */
      if (selection_is_active())
	dur = selection_len();
      else snd_no_active_selection_error(S_env_selection);
    }
  if (dur == 0) return;

  if (e)
    {
      if (e->pts == 0) return;
      val[0] = e->data[1];
      for (i = 1, j = 2; i < e->pts; i++, j += 2)
	if (e->data[j + 1] != val[0]) 
	  {
	    scalable = FALSE; 
	    break;
	  }
      if (scalable)
	{
	  val[0] *= scaler;
	  if ((beg == 0) && (dur >= CURRENT_SAMPLES(cp)))
	    {
	      scale_by(cp, val, 1, regexpr);
	      return;
	    }
	}
    }
  else scalable = FALSE;

  si = NULL;
  sp = cp->sound;
  ss = cp->state;

  if (scalable)
    {
      sc = get_sync_state_without_snd_fds(ss, sp, cp, beg, regexpr);
      if (sc == NULL) return;
      si = sc->si;
      for (i = 0; i < si->chans; i++) 
	{
	  if (regexpr)
	    scale_channel(si->cps[i], 
			  val[0], 
			  si->begs[i], 
			  selection_end(si->cps[i]) - si->begs[i] + 1, 
			  to_c_edit_position(si->cps[i], edpos, origin, arg_pos));
	  else scale_channel(si->cps[i], val[0], si->begs[i], dur, 
			     to_c_edit_position(si->cps[i], edpos, origin, arg_pos));
	}
      free_sync_state(sc);
      return;
    }

  if (e)
    egen = mus_make_env(e->data, e->pts, scaler, 0.0, e_base, 0.0, 0, dur - 1, NULL); /* dur - 1 = end sample number */
  else egen = gen;

  len = mus_length(egen);
  passes = mus_env_passes(egen);
  rates = mus_env_rates(egen);

  if (mus_increment(egen) == 0.0) 
    {
      /* step env -- handled as sequence of scalings */
      int local_edpos, k, pos, old_squelch;
      off_t segbeg, segnum, segend;
      env *newe;
      char *new_origin; /* need a complete origin since this appears as a scaled-edit in 
			 *   the edit history lists, save_edit_history needs something
			 *   that can actually recreate the original.
			 */
      /* base == 0 originally, so it's a step env */
      sc = get_sync_state_without_snd_fds(ss, sp, cp, beg, regexpr);
      /* TODO: if selection, begs/durs can be different so we need a new env on each (begs are correct) */
      if (sc == NULL) return;
      si = sc->si;
      for (i = 0; i < si->chans; i++) 
	{
	  segbeg = si->begs[i];
	  segend = si->begs[i] + dur;
	  segnum = passes[0] + 1;
	  local_edpos = si->cps[i]->edit_ctr; /* for as_one_edit backup */
	  old_squelch = si->cps[i]->squelch_update;
	  si->cps[i]->squelch_update = 1;
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
		  scale_channel(si->cps[i], (Float)(rates[k]), segbeg, segnum, pos);
		  pos = si->cps[i]->edit_ctr;
		}
	      segbeg += segnum;
	      if (segbeg >= segend) break;
	      segnum = passes[k + 1] - passes[k];
	    }
	  si->cps[i]->squelch_update = old_squelch;
	  newe = make_envelope(mus_data(egen), mus_length(egen) * 2);
	  new_origin = mus_format("env-channel (make-env %s :base 0 :end " OFF_TD ") " OFF_TD " " OFF_TD,
				  tmpstr = env_to_string(newe), 
				  (len > 1) ? (passes[len - 2] - 1) : dur,
				  si->begs[i], dur);
	  if (tmpstr) FREE(tmpstr);
	  free_env(newe);
	  as_one_edit(si->cps[i], local_edpos + 1, new_origin);
	  FREE(new_origin);
	}
      free_sync_state(sc);
      if (e) mus_free(egen);
      return;
    }

  if ((dur > FILE_BUFFER_SIZE) && (mus_increment(egen) == 1.0))
    {
      if (sp->sync)
	si = snd_sync(ss, sp->sync);
      else si = make_simple_sync(cp, beg);
      for (i = 0; i < si->chans; i++)
	if (ramp_or_ptree_fragments_in_use(si->cps[i], 
					   si->begs[i],
					   dur,
					   to_c_edit_position(si->cps[i], edpos, origin, arg_pos)))
	  {
	    rampable = FALSE;
	    break;
	  }
      si = free_sync_info(si);
    }
  else rampable = FALSE;

  if (!rampable)
    {
      /* run env over samples */
      sc = get_sync_state(ss, sp, cp, beg, regexpr, READ_FORWARD, edpos, origin, arg_pos);
      if (sc == NULL) return;
      si = sc->si;
      sfs = sc->sfs;
      if (dur > MAX_BUFFER_SIZE) /* if smaller than this, we don't gain anything by using a temp file (its buffers are this large) */
	{
	  temp_file = TRUE; 
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
	      snd_error("can't open %s temp file %s: %s\n", origin, ofile, strerror(errno));
	      FREE(ofile);
	      return;
	    }
	  datumb = mus_bytes_per_sample(hdr->format);
	}
      else temp_file = FALSE;
      data = (mus_sample_t **)MALLOC(si->chans * sizeof(mus_sample_t *));
      for (i = 0; i < si->chans; i++) 
	{
	  if (temp_file)
	    data[i] = (mus_sample_t *)CALLOC(FILE_BUFFER_SIZE, sizeof(mus_sample_t)); 
	  else data[i] = (mus_sample_t *)CALLOC(dur, sizeof(mus_sample_t)); 
	}
      j = 0;
      reporting = (dur > (MAX_BUFFER_SIZE * 4));
      if (reporting) start_progress_report(sp, from_enved);
      if (si->chans > 1)
	{
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
	  ss->stopped_explicitly = 0;
	  if (temp_file) 
	    snd_remove(ofile, TRUE);
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
		    amp_env_env(si->cps[i], mus_data(egen), len, pos);
		  else 
		    {
		      if ((len < 2) || (abs(dur - passes[len - 2]) < 2))
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
  else
    {
      /* line segment env that is long enough to reward optimization -- handled as sequence of ramps and scalings */
      int local_edpos, k, m, pos, old_squelch, env_pos;
      off_t segbeg, segnum, segend;
      Float *data;
      env *newe;
      char *new_origin; /* need a complete origin since this appears as a scaled-edit in 
			 *   the edit history lists, save_edit_history needs something
			 *   that can actually recreate the original.
			 */
      /* base == 0 originally, so it's a step env */
      data = mus_data(egen);
      sc = get_sync_state_without_snd_fds(ss, sp, cp, beg, regexpr);
      if (sc == NULL) return;
      si = sc->si;
      for (i = 0; i < si->chans; i++) 
	{
	  segbeg = si->begs[i];
	  segend = si->begs[i] + dur;
	  segnum = passes[0] + 1;
	  local_edpos = si->cps[i]->edit_ctr; /* for as_one_edit backup */
	  old_squelch = si->cps[i]->squelch_update;
	  si->cps[i]->squelch_update = 1;
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
		    ramp_channel(si->cps[i], (Float)(data[m]),
				 (Float)(data[m + 2]), segbeg, segnum, pos);
		  else ramp_channel(si->cps[i], (Float)(data[m]) + (data[m + 2] - data[m]) / (double)segnum,
				    (Float)(data[m + 2]), segbeg, segnum, pos);
		  pos = si->cps[i]->edit_ctr;
		}
	      segbeg += segnum;
	      if (segbeg >= segend) break;
	      segnum = passes[k + 1] - passes[k];
	    }
	  if ((si->begs[i] == 0) && (dur == si->cps[i]->samples[env_pos]))
	    amp_env_env(si->cps[i], mus_data(egen), len, env_pos);
	  else 
	    {
	      if ((len < 2) || (abs(dur - passes[len - 2]) < 2))
		amp_env_env_selection_by(si->cps[i], egen, si->begs[i], dur, env_pos);
	    }
	  si->cps[i]->squelch_update = old_squelch;
	  newe = make_envelope(mus_data(egen), mus_length(egen) * 2);
	  new_origin = mus_format("env-channel (make-env %s :base 1 :end " OFF_TD ") " OFF_TD " " OFF_TD,
				  tmpstr = env_to_string(newe), 
				  (len > 1) ? (passes[len - 2] - 1) : dur,
				  si->begs[i], dur);
	  if (tmpstr) FREE(tmpstr);
	  free_env(newe);
	  as_one_edit(si->cps[i], local_edpos + 1, new_origin);
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
    beg = cp->cursor;
  else
    {
      count = -count;
      beg = cp->cursor - count;
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
      si = snd_sync(cp->state, sp->sync);
      cps = si->cps;
      for (i = 0; i < si->chans; i++)
	{
	  delete_samples(beg, count, cps[i], origin, cps[i]->edit_ctr); 
	  cps[i]->cursor = beg;
	  update_graph(si->cps[i]);
	}
      si = free_sync_info(si);
    }
  else
    {
      delete_samples(beg, count, cp, origin, cp->edit_ctr);
      cp->cursor = beg;
      update_graph(cp);
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
      si = snd_sync(cp->state, sp->sync);
      cps = si->cps;
      for (i = 0; i < si->chans; i++)
	{
	  extend_with_zeros(cps[i], 
			    mus_oclamp(0, beg, CURRENT_SAMPLES(si->cps[i]) - 1), 
			    count, origin,
			    cps[i]->edit_ctr);
	  update_graph(cps[i]);
	}
      si = free_sync_info(si);
    }
  else 
    {
      extend_with_zeros(cp, 
			mus_oclamp(0, beg, CURRENT_SAMPLES(cp)), 
			count, origin,
			cp->edit_ctr);
      update_graph(cp);
    }
}

void cursor_zeros(chan_info *cp, off_t count, int regexpr)
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
	  (num >= CURRENT_SAMPLES(ncp)))
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
	      (beg >= CURRENT_SAMPLES(ncp)) || 
	      (chn_sample(beg, ncp, ncp->edit_ctr) != 0.0))
	    scale_channel(ncp, 0.0, beg, num, ncp->edit_ctr);
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
  FREE(data);
}

void cos_smooth(chan_info *cp, off_t beg, off_t num, int regexpr, const char *origin)
{
  /* verbatim, so to speak from Dpysnd */
  /* start at beg, apply a cosine for num samples, matching endpoints */
  sync_state *sc;
  int i;
  snd_info *sp;
  sync_info *si;
  sp = cp->sound;
  sc = get_sync_state_without_snd_fds(cp->state, sp, cp, beg, regexpr);
  if (sc == NULL) return;
  si = sc->si;
  if (regexpr) num = sc->dur;
  for (i = 0; i < si->chans; i++)
    smooth_channel(si->cps[i], si->begs[i], num, si->cps[i]->edit_ctr, origin);
  free_sync_state(sc);
}



#include "vct.h"
#include "clm2xen.h"

static char *run_channel(chan_info *cp, void *upt, off_t beg, off_t dur, int edpos)
{
  snd_state *ss;
  snd_info *sp;
  file_info *hdr = NULL;
  int j, ofd = 0, datumb = 0, temp_file, err = 0;
  off_t k;
  mus_sample_t **data;
  mus_sample_t *idata;
  char *ofile = NULL;
  snd_fd *sf;
  if ((beg < 0) || (dur <= 0)) return(NULL);
  ss = cp->state;
  sp = cp->sound;
  sf = init_sample_read_any(beg, cp, READ_FORWARD, edpos);
  if (sf == NULL) return(mus_format("run-channel: can't read %s[%d] channel data!", sp->short_filename, cp->chan));
  if (dur > MAX_BUFFER_SIZE)
    {
      temp_file = 1; 
      ofile = snd_tempnam(ss);
      hdr = make_temp_header(ofile, SND_SRATE(sp), 1, dur, S_map_channel);
      ofd = open_temp_file(ofile, 1, hdr, ss);
      if (ofd == -1)
	{
	  free_snd_fd(sf); 
	  return(mus_format("can't open run-channel temp file %s: %s\n", ofile, strerror(errno)));
	}
      datumb = mus_bytes_per_sample(hdr->format);
    }
  else temp_file = 0;
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
  return(NULL);
}

#define MUS_OUTA_1(Frame, Val, Fd) ((*((Fd)->base)->sample))((void *)(Fd), Frame, 0, Val)
/* avoids all the CLM error checking */

static XEN g_map_chan_1(XEN proc_and_list, XEN s_beg, XEN s_end, XEN org, XEN snd, XEN chn, XEN edpos, XEN s_dur, char *fallback_caller) 
{ 
  snd_state *ss;
  chan_info *cp;
  char *caller;
  off_t beg = 0, end = 0, dur = 0;
  snd_info *sp;
  snd_fd *sf = NULL;
  XEN errstr;
  off_t kp, len, j = 0, num;
  int reporting = 0, rpt = 0, rpt4, i, cured, pos;
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
  ss = get_global_state();
  cp = get_cp(snd, chn, caller);
  pos = to_c_edit_position(cp, edpos, caller, 7);
  beg = beg_to_sample(s_beg, caller);
  if (beg > cp->samples[pos])
    XEN_ERROR(NO_SUCH_SAMPLE,
	      XEN_LIST_2(C_TO_XEN_STRING(caller),
			 s_beg));
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
	  return(snd_bad_arity_error(caller, errstr, proc));
	}

      if (optimization(ss) > 0)
	{
	  pt = form_to_ptree_1f2f(proc_and_list);
	  if (pt)
	    {
	      char *err_str;
	      err_str = run_channel(cp, pt, beg, num, pos);
	      free_ptree(pt);
	      if (err_str == NULL)
		return(XEN_ZERO);
	      else FREE(err_str); /* and fallback on normal evaluator */
	    }
	}
      sp = cp->sound;
      reporting = (num > MAX_BUFFER_SIZE);
      if (reporting) start_progress_report(sp, NOT_FROM_ENVED);
      sf = init_sample_read_any(beg, cp, READ_FORWARD, pos);
      if (sf == NULL) return(XEN_TRUE);
      rpt4 = MAX_BUFFER_SIZE / 4;
      filename = snd_tempnam(ss);
      outgen = mus_make_sample2file(filename, 1, MUS_OUT_FORMAT, MUS_NEXT);
      j = 0;
      for (kp = 0; kp < num; kp++)
	{
	  /* changed here to remove catch 24-Mar-02 */
	  res = XEN_CALL_1_NO_CATCH(proc, 
				    C_TO_XEN_DOUBLE((double)read_sample_to_float(sf)),
				    caller);
	  if (XEN_NUMBER_P(res))                         /* one number -> replace current sample */
	    MUS_OUTA_1(j++, XEN_TO_C_DOUBLE(res), (mus_output *)outgen);
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
			    MUS_OUTA_1(j++, XEN_TO_C_DOUBLE(data[i]), (mus_output *)outgen);
			}
		      else
			{
			  if (VCT_P(res))
			    {
			      v = TO_VCT(res);
			      for (i = 0; i < v->length; i++) 
				MUS_OUTA_1(j++, v->data[i], (mus_output *)outgen);
			    }
			  else
			    {
			      if (XEN_LIST_P(res))
				{
				  len = XEN_LIST_LENGTH(res);
				  for (i = 0; i < len; i++, res = XEN_CDR(res)) 
				    MUS_OUTA_1(j++, XEN_TO_C_DOUBLE(XEN_CAR(res)), (mus_output *)outgen);
				}
			      else 
				{
				  if (outgen) mus_free(outgen);
				  if (sf) sf = free_snd_fd(sf);
				  if (reporting) finish_progress_report(sp, NOT_FROM_ENVED);
				  snd_remove(filename, TRUE);
				  FREE(filename);
				  XEN_ERROR(BAD_TYPE,
					    XEN_LIST_2(C_TO_XEN_STRING(caller),
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
	ss->stopped_explicitly = 0;
      else
	{
	  if (j == num)
	    file_change_samples(beg, j, filename, cp, 0, DELETE_ME, LOCK_MIXES, caller, cp->edit_ctr);
	  else
	    {
	      cured = cp->edit_ctr;
	      delete_samples(beg, num, cp, caller, cp->edit_ctr);
	      if (j > 0)
		{
		  file_insert_samples(beg, j, filename, cp, 0, DELETE_ME, caller, cp->edit_ctr);
		  backup_edit_list(cp);
		  if (cp->edit_ctr > cured)
		    backup_edit_list(cp);
		  ripple_trailing_marks(cp, beg, num, j);
		}
	      else snd_remove(filename, TRUE);
	    }
	  update_graph(cp);
	}
      FREE(filename);
    }
  return(res);
}

static XEN g_ptree_channel(XEN proc_and_list, XEN s_beg, XEN s_dur, XEN snd, XEN chn, XEN edpos, XEN env_too)
{
  #define H_ptree_channel "(" S_ptree_channel " proc &optional beg dur snd chn edpos peak-env-also) \
applies 'proc' as a 'virtual edit'; that is, the effect of 'proc' (a function of one argument, the \
current sample), comes about as an implicit change in the way the data is read.  This is similar to \
scaling and some envelope operations in that no data actually changes.  If 'peak-env-also' is #t, \
the same function is applied to the peak env values to get the new version.  If the underlying data \
has any envelope ramps of previous ptree operations, map-channel is called instead and the new \
data is saved in the normal manner.  Also, there's currently no way to inform the 'proc' that \
a new read has begun, so (for now) 'proc' should not have any state or make any assumptions \
about where it is in the sound."

  chan_info *cp;
  off_t beg = 0, dur = 0;
  int pos;
  XEN proc = XEN_FALSE;
  void *pt = NULL;
  if (XEN_LIST_P(proc_and_list))
    proc = XEN_CADR(proc_and_list);
  XEN_ASSERT_TYPE((XEN_PROCEDURE_P(proc)) && (XEN_REQUIRED_ARGS(proc) == 1), proc, XEN_ARG_1, S_ptree_channel, "a procedure of one arg");
  ASSERT_SAMPLE_TYPE(S_ptree_channel, s_beg, XEN_ARG_2);
  ASSERT_SAMPLE_TYPE(S_ptree_channel, s_dur, XEN_ARG_3);
  ASSERT_CHANNEL(S_ptree_channel, snd, chn, 4); 
  cp = get_cp(snd, chn, S_ptree_channel);
  pos = to_c_edit_position(cp, edpos, S_ptree_channel, 6);
  beg = beg_to_sample(s_beg, S_ptree_channel);
  if (beg > cp->samples[pos])
    XEN_ERROR(NO_SUCH_SAMPLE,
	      XEN_LIST_2(C_TO_XEN_STRING(S_ptree_channel),
			 s_beg));
  dur = dur_to_samples(s_dur, beg, cp, pos, 3, S_ptree_channel);
  clear_minibuffer(cp->sound);
  pt = form_to_ptree_1f2f(proc_and_list);
  if (pt)
    {
      if (ramp_or_ptree_fragments_in_use(cp, beg, dur, pos))
	{
	  snd_warning("ptree not virtual in this case");
	  run_channel(cp, pt, beg, dur, pos);
	  free_ptree(pt);
	}
      else ptree_channel(cp, pt, beg, dur, pos, (XEN_TRUE_P(env_too)) ? pt : NULL);
    }
  else
    {
      snd_warning("can't optimize ptree; calling map-channel");
      g_map_chan_1(proc_and_list, s_beg, XEN_FALSE, C_TO_XEN_STRING(S_ptree_channel), snd, chn, edpos, s_dur, "map-chan");
    }
  return(XEN_FALSE);
}

#if HAVE_DYNAMIC_WIND
typedef struct {
  XEN proc;
  off_t beg, num;
  snd_fd *sf;
  snd_info *sp;
  const char *caller;
  int counting, reporting;
} scan_context;

static scan_context *make_scan_context(XEN p, off_t b, off_t n, snd_fd *f, snd_info *s, const char *orig, int count)
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
  sc->reporting = FALSE;
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
  snd_state *ss;
  int counts = 0, rpt = 0, rpt4 = 0;
  XEN res = XEN_FALSE;
  scan_context *sc = (scan_context *)context;
  sc->reporting = (sc->num > MAX_BUFFER_SIZE);
  rpt4 = MAX_BUFFER_SIZE / 4;
  ss = get_global_state();
  if (sc->reporting) start_progress_report(sc->sp, NOT_FROM_ENVED);
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
	  ss->stopped_explicitly = 0;
	  report_in_minibuffer(sc->sp, "C-G stopped %s at sample " OFF_TD, 
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
		     const char *caller, int counting, XEN edpos, int arg_pos, XEN s_dur)
{
  snd_state *ss;
  chan_info *cp;
  off_t beg = 0, end = 0, dur = 0;
  snd_info *sp;
  snd_fd *sf;
  XEN errstr;
  off_t kp, num;
#if (!HAVE_DYNAMIC_WIND)
  int reporting = 0, rpt = 0, rpt4;
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
  ss = get_global_state();
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
  ss = get_global_state();
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
      pt = form_to_ptree_1f2b(proc_and_list);
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
  reporting = (num > MAX_BUFFER_SIZE);
  if (reporting) start_progress_report(sp, NOT_FROM_ENVED);
  rpt4 = MAX_BUFFER_SIZE / 4;
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
	  ss->stopped_explicitly = 0;
	  report_in_minibuffer(sp, "C-G stopped %s at sample " OFF_TD, 
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
  #define H_scan_chan "(" S_scan_chan " func &optional (start 0) end snd chn edpos)\n\
apply 'func' to samples in current channel (or the specified channel). \
'func' is a function of one argument, the current sample. \
if 'func' returns non-#f, the scan stops, and the value is returned to the caller with the sample number. \n\
  (scan-chan (lambda (x) (> x .1)))"

  ASSERT_CHANNEL(S_scan_chan, snd, chn, 4); 
  return(g_sp_scan(proc, beg, end, snd, chn, S_scan_chan, FALSE, edpos, 6, XEN_FALSE));
}

static XEN g_scan_channel(XEN proc, XEN beg, XEN dur, XEN snd, XEN chn, XEN edpos) 
{ 
  #define H_scan_channel "(" S_scan_channel " func &optional (start 0) dur snd chn edpos)\n\
apply func to samples in current channel (or the specified channel) \
func is a function of one argument, the current sample. \
if func returns non-#f, the scan stops, and the value is returned to the caller with the sample number. \n\
  (scan-channel (lambda (x) (> x .1)))"

  ASSERT_CHANNEL(S_scan_channel, snd, chn, 4); 
  return(g_sp_scan(proc, beg, XEN_FALSE, snd, chn, S_scan_channel, FALSE, edpos, 6, (XEN_BOUND_P(dur)) ? dur : XEN_FALSE));
}

static XEN g_map_chan(XEN proc, XEN s_beg, XEN s_end, XEN org, XEN snd, XEN chn, XEN edpos) 
{
  #define H_map_chan "(" S_map_chan " func &optional (start 0) end edname snd chn edpos)\n\
apply func to samples in current channel, edname is the edit history name for this editing operation.\n\
  (map-chan abs)"
  return(g_map_chan_1(proc, s_beg, s_end, org, snd, chn, edpos, XEN_FALSE, S_map_chan));
}

static XEN g_map_channel(XEN proc, XEN s_beg, XEN s_dur, XEN snd, XEN chn, XEN edpos, XEN org) 
{
  #define H_map_channel "(" S_map_channel " func &optional (start 0) dur snd chn edpos edname)\n\
apply func to samples in current channel, edname is the edit history name for this editing operation.\n\
  (map-channel abs)"
  return(g_map_chan_1(proc, s_beg, XEN_FALSE, org, snd, chn, edpos, (XEN_BOUND_P(s_dur)) ? s_dur : XEN_FALSE, S_map_channel));
}

static XEN g_find(XEN expr, XEN sample, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_find "(" S_find " func &optional (start-samp 0) snd chn edpos) applies func, a function of one argument, \
the current sample, to each sample in snd's channel chn, starting at 'start-samp' until func returns #t"

  /* no free here -- it's handled as ss->search_expr in snd-find.c */
  ASSERT_CHANNEL(S_find, snd_n, chn_n, 3);
  return(g_sp_scan(expr, sample, XEN_FALSE, snd_n, chn_n, S_find, FALSE, edpos, 5, XEN_FALSE));
}

static XEN g_count_matches(XEN expr, XEN sample, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_count_matches "(" S_count_matches " func &optional (start-samp 0) snd chn edpos) returns how many \
samples satisfy func (a function of one argument, the current sample, returning #t upon match)"

  ASSERT_CHANNEL(S_count_matches, snd_n, chn_n, 3);
  return(g_sp_scan(expr, sample, XEN_FALSE, snd_n, chn_n, S_count_matches, TRUE, edpos, 5, XEN_FALSE));
}

static XEN g_smooth_sound(XEN beg, XEN num, XEN snd_n, XEN chn_n)
{
  #define H_smooth_sound "(" S_smooth_sound " &optional start-samp samps snd chn) smooths data from start-samp for samps in snd's channel chn"
  chan_info *cp;
  off_t start;
  ASSERT_SAMPLE_TYPE(S_smooth_sound, beg, XEN_ARG_1);
  ASSERT_SAMPLE_TYPE(S_smooth_sound, num, XEN_ARG_2);
  ASSERT_CHANNEL(S_smooth_sound, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_smooth_sound);
  start = beg_to_sample(beg, S_smooth_sound);
  cos_smooth(cp,
	     start,
	     dur_to_samples(num, start, cp, cp->edit_ctr, 2, S_smooth_sound),
	     FALSE,
	     S_smooth_sound); 
  return(XEN_TRUE);
}

static XEN g_smooth_channel(XEN beg, XEN dur, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_smooth_channel "(" S_smooth_channel " &optional beg dur snd chn edpos) smooths data from beg for dur in snd's channel chn"
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
    smooth_channel(cp, start, num, pos, S_smooth_channel);
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

static XEN g_reverse_channel(XEN s_beg, XEN s_dur, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_reverse_channel "(" S_reverse_channel " &optional (beg 0) dur snd chn edpos) reverses a portion of snd's channel chn"
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
  return(XEN_FALSE);
}

static XEN g_insert_silence(XEN beg, XEN num, XEN snd, XEN chn)
{
  #define H_insert_silence "(" S_insert_silence " beg num &optional snd chn) inserts num zeros at beg in snd's chn"
  chan_info *cp;
  off_t start = 0, len = 0;
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
  cursor_insert(cp, /* follows sync */
		start,
		len,
		S_insert_silence);
  return(beg);
}

static XEN g_pad_channel(XEN beg, XEN num, XEN snd, XEN chn, XEN edpos)
{
  #define H_pad_channel "(" S_pad_channel " beg dur snd chn edpos) inserts dur zeros at beg in snd's chn"
  chan_info *cp;
  off_t bg;
  int pos;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(beg), beg, XEN_ARG_1, S_pad_channel, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(num), num, XEN_ARG_2, S_pad_channel, "a number");
  ASSERT_CHANNEL(S_pad_channel, snd, chn, 3);
  cp = get_cp(snd, chn, S_pad_channel);
  bg = XEN_TO_C_OFF_T_OR_ELSE(beg, 0);
  if (bg < 0) XEN_ERROR(NO_SUCH_SAMPLE,
			XEN_LIST_2(C_TO_XEN_STRING(S_pad_channel),
				   beg));
  pos = to_c_edit_position(cp, edpos, S_pad_channel, 5);
  extend_with_zeros(cp, 
		    bg,
		    XEN_TO_C_OFF_T_OR_ELSE(num, cp->samples[pos] - bg),
		    S_pad_channel,
		    pos);
  return(beg);
}

static XEN g_swap_channels(XEN snd0, XEN chn0, XEN snd1, XEN chn1, XEN beg, XEN dur)
{
  #define H_swap_channels "(" S_swap_channels " snd0 chn0 snd1 chn1 beg dur) swaps the indicated channels"
  chan_info *cp0 = NULL, *cp1 = NULL;
  snd_fd *c0, *c1;
  off_t dur0 = 0, dur1 = 0, beg0 = 0, num;
  int old_squelch0, old_squelch1;
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
      if (XEN_NUMBER_P(beg)) 
	beg0 = XEN_TO_C_OFF_T(beg);
      if (XEN_NUMBER_P(dur)) 
	num = XEN_TO_C_OFF_T(dur);
      else
	{
	  dur0 = CURRENT_SAMPLES(cp0);
	  dur1 = CURRENT_SAMPLES(cp1);
	  if (dur0 > dur1) num = dur1; else num = dur0;
	}
      if ((beg0 == 0) && ((num == dur0) || (num == dur1)) &&
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
	  swap_marks(cp0, cp1);
	  update_graph(cp0);
	  update_graph(cp1);
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
    mus_misc_error(caller, "scalers empty?", scalers);
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
  #define H_scale_to "(" S_scale_to " norms &optional snd chn)\n\
normalizes snd to norms (following sync) norms can be a float or a vector of floats"

  /* chn_n irrelevant if sync */
  chan_info *cp;
  int len[1];
  Float *scls;
  ASSERT_CHANNEL(S_scale_to, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_scale_to);
  scls = load_Floats(scalers, len, S_scale_to);
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
  scls = load_Floats(scalers, len, S_scale_by);
  scale_by(cp, scls, len[0], FALSE);
  FREE(scls);
  return(scalers);
}

static XEN g_scale_selection_to(XEN scalers)
{
  #define H_scale_selection_to "(" S_scale_selection_to " norms) normalizes current selected portion to norms"
  int len[1];
  Float *scls;
  if (selection_is_active())
    {
      scls = load_Floats(scalers, len, S_scale_selection_to);
      scale_to(get_global_state(), NULL, NULL, scls, len[0], TRUE);
      FREE(scls);
      return(scalers);
    }
  snd_no_active_selection_error(S_scale_selection_to);
  return(scalers);
}

static XEN g_scale_selection_by(XEN scalers)
{
  #define H_scale_selection_by "(" S_scale_selection_by " scalers) scales current selected portion by scalers"
  int len[1];
  Float *scls;
  if (selection_is_active())
    {
      scls = load_Floats(scalers, len, S_scale_selection_by);
      scale_by(NULL, scls, len[0], TRUE);
      FREE(scls);
      return(scalers);
    }
  snd_no_active_selection_error(S_scale_selection_by);
  return(scalers);
}

static XEN g_clm_channel(XEN gen, XEN samp_n, XEN samps, XEN snd_n, XEN chn_n, XEN edpos, XEN overlap)
{
  #define H_clm_channel "(" S_clm_channel " gen &optional beg dur snd chn edpos overlap)\n\
applies gen to snd's channel chn starting at beg for dur samples. overlap is the 'ring' time, if any."

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
  egen = MUS_XEN_TO_CLM(gen);
  errmsg = clm_channel(cp, egen, beg, dur, pos, S_clm_channel, XEN_TO_C_OFF_T_OR_ELSE(overlap, 0));
  if (errmsg)
    {
      str = C_TO_XEN_STRING(errmsg);
      FREE(errmsg);
      mus_misc_error(S_clm_channel, NULL, str);
    }
  return(gen);
}

static XEN g_env_1(XEN edata, off_t beg, off_t dur, XEN base, chan_info *cp, XEN edpos, const char *caller, int selection)
{
  env *e;
  mus_any *egen = NULL;
  if (XEN_LIST_P(edata))
    {
      e = get_env(edata, (char *)caller);
      if (e)
	{
	  apply_env(cp, e, beg, dur, 1.0, selection, NOT_FROM_ENVED, caller, NULL, edpos, 7, XEN_TO_C_DOUBLE_OR_ELSE(base, 1.0));
	  free_env(e);
	  return(edata);
	}
    }
  else
    {
      XEN_ASSERT_TYPE((mus_xen_p(edata)) && (mus_env_p(egen = MUS_XEN_TO_CLM(edata))), edata, XEN_ARG_1, caller, "an env generator or a list");
      apply_env(cp, NULL, beg, dur, 1.0, selection, NOT_FROM_ENVED, caller, egen, edpos, 7, 1.0);
      return(edata);
    }
  return(XEN_FALSE);
}

static XEN g_env_selection(XEN edata, XEN base)
{
  #define H_env_selection "(" S_env_selection " env &optional (env-base 1.0))\n\
applies envelope 'env' to the selection using 'env-base' to determine how breakpoints are connected"

  if (selection_is_active() == 0) 
    return(snd_no_active_selection_error(S_env_selection));
  return(g_env_1(edata, 0, 0, base, 
		 get_cp(XEN_FALSE, XEN_FALSE, S_env_selection), 
		 C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 
		 S_env_selection, TRUE));
}

static XEN g_env_sound(XEN edata, XEN samp_n, XEN samps, XEN base, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_env_sound "(" S_env_sound " env &optional (start-samp 0) samps (env-base 1.0) snd chn edpos)\n\
applies amplitude envelope 'env' (a list of breakpoints or a CLM env) to snd's channel chn starting at start-samp, going \
either to the end of the sound or for 'samps' samples, with segments interpolating according to 'env-base'"

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
  return(g_env_1(edata, beg, dur, base, cp, edpos, S_env_sound, FALSE));
}

static XEN g_env_channel(XEN gen, XEN samp_n, XEN samps, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_env_channel "(" S_env_channel " clm-env-gen-or-envelope &optional beg dur snd chn edpos)\n\
applies amplitude envelope 'clm-env-gen-or-envelope' to snd's channel chn starting at beg for dur samples."

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
  val = g_env_1(gen, beg, dur, C_TO_XEN_DOUBLE(1.0), cp, edpos, S_env_channel, FALSE);
  sp->sync = old_sync;
  return(val);
}

static XEN g_fft_1(XEN reals, XEN imag, XEN sign, int use_fft)
{
  vct *v1 = NULL, *v2 = NULL;
  int ipow, n, n2, i, isign = 1, need_free = 0;
  Float *rl, *im;
  XEN *rvdata; XEN *ivdata;
  XEN_ASSERT_TYPE(((VCT_P(reals)) || (XEN_VECTOR_P(reals))), reals, XEN_ARG_1, ((use_fft) ? S_fft : S_convolve_arrays), "a vector or a vct");
  XEN_ASSERT_TYPE(((VCT_P(imag)) || (XEN_VECTOR_P(imag))), imag, XEN_ARG_2, ((use_fft) ? S_fft : S_convolve_arrays), "a vector or a vct");
  if (XEN_NUMBER_P(sign)) isign = XEN_TO_C_INT_OR_ELSE(sign, 0);
  if (isign == 0) isign = 1;
  if ((VCT_P(reals)) && (VCT_P(imag)))
    {
      v1 = (vct *)XEN_OBJECT_REF(reals);
      v2 = (vct *)XEN_OBJECT_REF(imag);
      n = v1->length;
    }
  else
    {
      n = XEN_VECTOR_LENGTH(reals);
      if (n == 0) return(XEN_ZERO);
      n = XEN_VECTOR_LENGTH(imag);
      if (n == 0) return(XEN_ZERO);
    }
  if (POWER_OF_2_P(n))
    n2 = n;
  else
    {
      ipow = (int)(log(n + 1) / log(2.0));
      n2 = snd_ipow2(ipow);
    }
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
	      /* VECTOR_SET here and below */
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

static XEN g_snd_spectrum(XEN data, XEN win, XEN len, XEN linear_or_dB, XEN beta)
{
  #define H_snd_spectrum "(" S_snd_spectrum " data window len linear-or-dB beta)\n\
return magnitude spectrum of data (vct) in data using fft-window win and fft length len"

  int i, j, n, n2, linear, wtype;
  Float maxa, todb, lowest, val, b = 0.0;
  Float *idat, *rdat, *window;
  vct *v;
  XEN_ASSERT_TYPE((VCT_P(data)), data, XEN_ARG_1, S_snd_spectrum, "a vct");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(win), win, XEN_ARG_2, S_snd_spectrum, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(len), len, XEN_ARG_3, S_snd_spectrum, "an integer");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(linear_or_dB), linear_or_dB, XEN_ARG_4, S_snd_spectrum, "a boolean");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(beta), beta, XEN_ARG_5, S_snd_spectrum, "a number");
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
  if (XEN_NUMBER_P(beta)) b = XEN_TO_C_DOUBLE(beta);
  if (b < 0.0) b = 0.0; else if (b > 1.0) b = 1.0;
  idat = (Float *)CALLOC(n, sizeof(Float));
  window = (Float *)CALLOC(n, sizeof(Float));
  mus_make_fft_window_with_window(wtype, n, b * fft_beta_max(wtype), window);
  for (i = 0; i < n; i++) rdat[i] *= window[i];
  FREE(window);
  n2 = n / 2;
#if HAVE_FFTW
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
  #define H_convolve_with "(" S_convolve_with " file &optional (amp 1.0) snd chn edpos)\n\
convolves file with snd's channel chn (or the currently sync'd channels), amp is the resultant peak amp"

  chan_info *cp;
  ASSERT_CHANNEL(S_convolve_with, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_convolve_with);
  return(g_convolve_with_1(file, new_amp, cp, edpos, S_convolve_with));
}

static XEN g_convolve_selection_with(XEN file, XEN new_amp)
{
  #define H_convolve_selection_with "(" S_convolve_selection_with " file &optional (amp 1.0))\n\
convolves the current selection with file; amp is the resultant peak amp"

  if (selection_is_active() == 0) 
    return(snd_no_active_selection_error(S_convolve_selection_with));
  return(g_convolve_with_1(file, new_amp, NULL, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), S_convolve_selection_with));
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

static Float check_src_envelope(int pts, Float *data, const char *caller)
{
  /* can't go through zero here, and if negative need to return 1.0 */
  int i;
  Float res = 0.0;
  for (i = 0; i < (2 * pts); i += 2)
    if (data[i + 1] == 0.0)
      mus_misc_error(caller, "envelope hits 0.0", mus_array_to_list(data, 0, pts * 2));
    else
      {
	if (data[i + 1] < 0.0)
	  {
	    if (res <= 0.0)
	      res = -1.0;
	    else mus_misc_error(caller, "envelope passes through 0.0", mus_array_to_list(data, 0, pts * 2));
	  }
	else
	  {
	    if (res >= 0)
	      res = 1.0;
	    else mus_misc_error(caller, "envelope passes through 0.0", mus_array_to_list(data, 0, pts * 2));
	  }
      }
  return(res);
}


static XEN g_src_channel(XEN ratio_or_env_gen, XEN beg_n, XEN dur_n, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_src_channel "(" S_src_channel " ratio-or-env-gen &optional beg dur snd chn edpos)\n\
sampling-rate converts snd's channel chn by ratio, or following an envelope generator."

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
		   (mus_env_p(egen = MUS_XEN_TO_CLM(ratio_or_env_gen)))),
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
  else check_src_envelope(mus_length(egen), mus_data(egen), S_src_channel);
  sf = init_sample_read_any(beg, cp, READ_FORWARD, pos);
  errmsg = src_channel_with_error(cp, sf, beg, dur, ratio, egen, FALSE, S_src_channel, FALSE, 1, 1);
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

static XEN g_src_1(XEN ratio_or_env, XEN base, XEN snd_n, XEN chn_n, XEN edpos, const char *caller, int selection)
{
  chan_info *cp;
  snd_state *ss;
  env *e = NULL;
  mus_any *egen;
  Float e_ratio = 1.0;
  ASSERT_CHANNEL(caller, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, caller);
  ss = cp->state;
  if (XEN_NUMBER_P(ratio_or_env))
    src_env_or_num(ss, cp, NULL, XEN_TO_C_DOUBLE(ratio_or_env), 
		   TRUE, NOT_FROM_ENVED, caller,
		   selection, NULL, edpos, 5, 1.0);
  else 
    {
      if (XEN_LIST_P(ratio_or_env))
	{
	  e = get_env(ratio_or_env, (char *)caller);
	  e_ratio = check_src_envelope(e->pts, e->data, caller);
	  src_env_or_num(ss, cp,
			 e, e_ratio,
			 FALSE, NOT_FROM_ENVED, caller, 
			 FALSE, NULL, edpos, 5, 
			 XEN_TO_C_DOUBLE_OR_ELSE(base, 1.0));
	  if (e) free_env(e);
	}
      else
	{
	  XEN_ASSERT_TYPE((mus_xen_p(ratio_or_env)) && (mus_env_p(egen = MUS_XEN_TO_CLM(ratio_or_env))), 
			  ratio_or_env, XEN_ARG_1, caller, "a number, list, or env generator");
	  check_src_envelope(mus_length(egen), mus_data(egen), caller);
	  src_env_or_num(ss, cp, NULL, 
			 (mus_phase(egen) >= 0.0) ? 1.0 : -1.0,
			 FALSE, NOT_FROM_ENVED, caller, 
			 FALSE, egen, edpos, 5, 1.0);
	}
    }
  return(xen_return_first(ratio_or_env, base));
}

static XEN g_src_sound(XEN ratio_or_env, XEN base, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_src_sound "(" S_src_sound " ratio-or-env &optional (base 1.0) snd chn edpos)\n\
sampling-rate converts snd's channel chn by ratio, or following an envelope. Negative ratio reverses the sound"

  return(g_src_1(ratio_or_env, base, snd_n, chn_n, edpos, S_src_sound, FALSE));
}

static XEN g_src_selection(XEN ratio_or_env, XEN base)
{
  #define H_src_selection "(" S_src_selection " ratio-or-env &optional (base 1.0))\n\
sampling-rate converts the currently selected data by ratio (which can be an envelope)"

  if (selection_is_active() == 0) 
    return(snd_no_active_selection_error(S_src_selection));
  return(g_src_1(ratio_or_env, base, XEN_FALSE, XEN_FALSE, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), S_src_selection, TRUE));
}

static XEN g_filter_1(XEN e, XEN order, XEN snd_n, XEN chn_n, XEN edpos, const char *caller, int selection)
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
      error = apply_filter_or_error(cp, 0, NULL, NOT_FROM_ENVED, caller, selection, NULL, MUS_XEN_TO_CLM(e), edpos, 5);
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
	mus_misc_error(caller, "order <= 0?", order);
      if (VCT_P(e)) /* the filter coefficients direct */
	{
	  v = TO_VCT(e);
	  if (len > v->length) 
	    mus_misc_error(caller, "order > length coeffs?", XEN_LIST_2(order, e));
	  apply_filter(cp, len, NULL, NOT_FROM_ENVED, caller, selection, v->data, NULL, edpos, 5);
	}
      else 
	{
	  XEN_ASSERT_TYPE((XEN_VECTOR_P(e) || (XEN_LIST_P(e))), e, XEN_ARG_1, caller, "a list, vector, vct, or env generator");
	  apply_filter(cp, len,
		       ne = get_env(e, (char *)caller),
		       NOT_FROM_ENVED, caller, selection, NULL, NULL, edpos, 5);
	  if (ne) free_env(ne); 
	}
    }
  return(xen_return_first(XEN_TRUE, e));
}

static XEN g_filter_sound(XEN e, XEN order, XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_filter_sound "(" S_filter_sound " filter order &optional snd chn edpos)\n\
applies FIR filter to snd's channel chn. 'filter' is either the frequency response envelope, a CLM filter, or a vct object with the actual coefficients"

  return(g_filter_1(e, order, snd_n, chn_n, edpos, S_filter_sound, FALSE));
}

static XEN g_filter_selection(XEN e, XEN order)
{
  #define H_filter_selection "(" S_filter_selection " filter order) applies filter to current selection"

  if (selection_is_active() == 0) 
    return(snd_no_active_selection_error(S_filter_selection));
  return(g_filter_1(e, order, XEN_FALSE, XEN_FALSE, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), S_filter_selection, TRUE));
}

static XEN g_sinc_width(void) {return(C_TO_XEN_INT(sinc_width(get_global_state())));}
static XEN g_set_sinc_width(XEN val) 
{
  #define H_sinc_width "(" S_sinc_width ") -> sampling rate conversion sinc width (10). \
The higher this number, the better the src low-pass filter, but the slower \
src runs.  If you use too low a setting, you can sometimes hear high \
frequency whistles leaking through."

  int len;
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, "set! " S_sinc_width, "an integer"); 
  len = XEN_TO_C_INT(val);
  if (len >= 0)
    set_sinc_width(ss, len);
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
XEN_ARGIFY_6(g_swap_channels_w, g_swap_channels)
XEN_ARGIFY_4(g_insert_silence_w, g_insert_silence)
XEN_ARGIFY_1(g_scale_selection_to_w, g_scale_selection_to)
XEN_ARGIFY_1(g_scale_selection_by_w, g_scale_selection_by)
XEN_ARGIFY_3(g_scale_to_w, g_scale_to)
XEN_ARGIFY_3(g_scale_by_w, g_scale_by)
XEN_ARGIFY_2(g_env_selection_w, g_env_selection)
XEN_ARGIFY_7(g_env_sound_w, g_env_sound)
XEN_ARGIFY_6(g_env_channel_w, g_env_channel)
XEN_ARGIFY_3(g_fft_w, g_fft)
XEN_ARGIFY_5(g_snd_spectrum_w, g_snd_spectrum)
XEN_ARGIFY_2(g_convolve_w, g_convolve)
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
XEN_ARGIFY_1(g_set_sinc_width_w, g_set_sinc_width)
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
#define g_fft_w g_fft
#define g_snd_spectrum_w g_snd_spectrum
#define g_convolve_w g_convolve
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
#endif

void g_init_sig(void)
{
#if WITH_RUN
  XEN_EVAL_C_STRING("(use-modules (ice-9 optargs))");
  XEN_DEFINE_PROCEDURE(S_scan_channel "-1",       g_scan_channel_w, 1, 5, 0,            H_scan_channel);
  XEN_DEFINE_PROCEDURE(S_scan_chan "-1",          g_scan_chan_w, 1, 5, 0,               H_scan_chan);
  XEN_DEFINE_PROCEDURE(S_find "-1",               g_find_w, 1, 4, 0,                    H_find);
  XEN_DEFINE_PROCEDURE(S_count_matches "-1",      g_count_matches_w, 1, 4, 0,           H_count_matches);
  XEN_DEFINE_PROCEDURE(S_map_chan "-1",           g_map_chan_w, 1, 6, 0,                H_map_chan);
  XEN_DEFINE_PROCEDURE(S_map_channel "-1",        g_map_channel_w, 1, 6, 0,             H_map_channel);
  XEN_DEFINE_PROCEDURE(S_ptree_channel "-1",      g_ptree_channel, 1, 6, 0,             H_ptree_channel);

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
  XEN_DEFINE_PROCEDURE(S_scan_channel,            g_scan_channel_w, 1, 5, 0,            H_scan_channel);
  XEN_DEFINE_PROCEDURE(S_scan_chan,               g_scan_chan_w, 1, 5, 0,               H_scan_chan);
  XEN_DEFINE_PROCEDURE(S_find,                    g_find_w, 1, 4, 0,                    H_find);
  XEN_DEFINE_PROCEDURE(S_count_matches,           g_count_matches_w, 1, 4, 0,           H_count_matches);
  XEN_DEFINE_PROCEDURE(S_map_chan,                g_map_chan_w, 1, 6, 0,                H_map_chan);
  XEN_DEFINE_PROCEDURE(S_map_channel,             g_map_channel_w, 1, 6, 0,             H_map_channel);
  XEN_DEFINE_PROCEDURE(S_ptree_channel,           g_ptree_channel, 1, 6, 0,             H_ptree_channel);
#endif

  XEN_DEFINE_PROCEDURE(S_smooth_sound,            g_smooth_sound_w, 0, 4, 0,            H_smooth_sound);
  XEN_DEFINE_PROCEDURE(S_smooth_selection,        g_smooth_selection_w, 0, 0, 0,        H_smooth_selection);
  XEN_DEFINE_PROCEDURE(S_reverse_sound,           g_reverse_sound_w, 0, 3, 0,           H_reverse_sound);
  XEN_DEFINE_PROCEDURE(S_reverse_selection,       g_reverse_selection_w, 0, 0, 0,       H_reverse_selection);
  XEN_DEFINE_PROCEDURE(S_swap_channels,           g_swap_channels_w, 0, 6, 0,           H_swap_channels);
  XEN_DEFINE_PROCEDURE(S_insert_silence,          g_insert_silence_w, 2, 2, 0,          H_insert_silence);

  XEN_DEFINE_PROCEDURE(S_scale_selection_to,      g_scale_selection_to_w, 0, 1, 0,      H_scale_selection_to);
  XEN_DEFINE_PROCEDURE(S_scale_selection_by,      g_scale_selection_by_w, 0, 1, 0,      H_scale_selection_by);
  XEN_DEFINE_PROCEDURE(S_scale_to,                g_scale_to_w, 0, 3, 0,                H_scale_to);
  XEN_DEFINE_PROCEDURE(S_scale_by,                g_scale_by_w, 0, 3, 0,                H_scale_by);
  XEN_DEFINE_PROCEDURE(S_env_selection,           g_env_selection_w, 1, 1, 0,           H_env_selection);
  XEN_DEFINE_PROCEDURE(S_env_sound,               g_env_sound_w, 1, 6, 0,               H_env_sound);
  XEN_DEFINE_PROCEDURE(S_fft,                     g_fft_w, 2, 1, 0,                     H_fft);
  XEN_DEFINE_PROCEDURE(S_snd_spectrum,            g_snd_spectrum_w, 1, 4, 0,            H_snd_spectrum);
  XEN_DEFINE_PROCEDURE(S_convolve_arrays,         g_convolve_w, 1, 1, 0,                H_convolve);
  XEN_DEFINE_PROCEDURE(S_convolve_with,           g_convolve_with_w, 1, 4, 0,           H_convolve_with);
  XEN_DEFINE_PROCEDURE(S_convolve_selection_with, g_convolve_selection_with_w, 1, 1, 0, H_convolve_selection_with);
  XEN_DEFINE_PROCEDURE(S_src_sound,               g_src_sound_w, 1, 4, 0,               H_src_sound);
  XEN_DEFINE_PROCEDURE(S_src_selection,           g_src_selection_w, 1, 1, 0,           H_src_selection);
  XEN_DEFINE_PROCEDURE(S_filter_sound,            g_filter_sound_w, 1, 4, 0,            H_filter_sound);
  XEN_DEFINE_PROCEDURE(S_filter_selection,        g_filter_selection_w, 1, 1, 0,        H_filter_selection);

  XEN_DEFINE_PROCEDURE(S_reverse_channel,         g_reverse_channel_w, 0, 5, 0,         H_reverse_channel);
  XEN_DEFINE_PROCEDURE(S_clm_channel,             g_clm_channel_w, 1, 6, 0,             H_clm_channel);
  XEN_DEFINE_PROCEDURE(S_env_channel,             g_env_channel_w, 1, 5, 0,             H_env_channel);
  XEN_DEFINE_PROCEDURE(S_smooth_channel,          g_smooth_channel_w, 0, 5, 0,          H_smooth_channel);
  XEN_DEFINE_PROCEDURE(S_src_channel,             g_src_channel_w, 1, 5, 0,             H_src_channel);
  XEN_DEFINE_PROCEDURE(S_pad_channel,             g_pad_channel_w, 2, 3, 0,             H_pad_channel);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_sinc_width, g_sinc_width_w, H_sinc_width,
				   "set-" S_sinc_width, g_set_sinc_width_w,  0, 0, 0, 1);
}


