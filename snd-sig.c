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

static sync_state *get_sync_state_1(snd_state *ss, snd_info *sp, chan_info *cp, int beg, int regexpr, int forwards, int prebeg)
{
  /* can return NULL if regexpr and no current selection */
  sync_info *si = NULL;
  snd_fd **sfs = NULL;
  int dur, i, pbeg;
  sync_state *sc;
  dur = 0;
  if ((!regexpr) && (sp == NULL)) return(NULL);
  if ((!regexpr) && (sp->syncing != 0))
    {
      si = snd_sync(ss, sp->syncing);
      sfs = (snd_fd **)CALLOC(si->chans, sizeof(snd_fd *));
      for (i = 0; i < si->chans; i++) 
	{
	  si->begs[i] = beg;
	  if (forwards == READ_FORWARD)
	    sfs[i] = init_sample_read(beg, si->cps[i], READ_FORWARD);
	  else sfs[i] = init_sample_read(current_ed_samples(si->cps[i]) - 1, 
					 si->cps[i], 
					 READ_BACKWARD);
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
	      sfs = (snd_fd **)CALLOC(si->chans, sizeof(snd_fd *));
	      for (i = 0; i < si->chans; i++) 
		{
		  if (forwards == READ_FORWARD)
		    {
		      pbeg = si->begs[i] - prebeg;
		      if (pbeg < 0) pbeg = 0;
		      sfs[i] = init_sample_read(pbeg, 
						si->cps[i], 
						READ_FORWARD);
		    }
		  else sfs[i] = init_sample_read(si->begs[i] + dur - 1, 
						 si->cps[i], 
						 READ_BACKWARD);
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
      sfs = (snd_fd **)CALLOC(1, sizeof(snd_fd *));
      if (forwards == READ_FORWARD)
	sfs[0] = init_sample_read(beg, cp, READ_FORWARD);
      else sfs[0] = init_sample_read(current_ed_samples(cp) - 1, cp, READ_BACKWARD);
    }
  sc = (sync_state *)CALLOC(1, sizeof(sync_state));
  sc->dur = dur;
  sc->sfs = sfs;
  sc->si = si;
  return(sc);
}

static sync_state *get_sync_state(snd_state *ss, snd_info *sp, chan_info *cp, int beg, int regexpr, int forwards)
{
  return(get_sync_state_1(ss, sp, cp, beg, regexpr, forwards, 0));
}

#if HAVE_GUILE
/* support for scan and map functions */

static sync_state *get_chan_sync_state(chan_info *cp, int beg)
{
  sync_info *si;
  snd_fd **sfs;
  sync_state *sc;
  si = make_simple_sync(cp, beg);
  sfs = (snd_fd **)CALLOC(1, sizeof(snd_fd *));
  sfs[0] = init_sample_read(beg, cp, READ_FORWARD);
  sc = (sync_state *)CALLOC(1, sizeof(sync_state));
  sc->sfs = sfs;
  sc->si = si;
  return(sc);
}

static sync_state *get_sound_chans_sync_state(chan_info *cp, int beg)
{
  sync_info *si;
  snd_fd **sfs;
  sync_state *sc;
  snd_info *sp;
  int i;
  sp = cp->sound;
  si = (sync_info *)CALLOC(1, sizeof(sync_info));
  si->chans = sp->nchans;
  si->cps = (chan_info **)CALLOC(sp->nchans, sizeof(chan_info *));
  si->begs = (int *)CALLOC(sp->nchans, sizeof(int));
  sfs = (snd_fd **)CALLOC(sp->nchans, sizeof(snd_fd *));
  for (i = 0; i < sp->nchans; i++) 
    {
      si->cps[i] = sp->chans[i];
      si->begs[i] = beg;
      sfs[i] = init_sample_read(beg, si->cps[i], READ_FORWARD);
    }
  sc = (sync_state *)CALLOC(1, sizeof(sync_state));
  sc->sfs = sfs;
  sc->si = si;
  return(sc);
}

static sync_state *get_active_chans_sync_state(snd_state *ss, int beg)
{
  sync_info *si;
  snd_fd **sfs;
  sync_state *sc;
  snd_info *sp;
  int snd, chn, i;
  si = (sync_info *)CALLOC(1, sizeof(sync_info));
  si->chans = active_channels(ss, WITH_VIRTUAL_CHANNELS);
  si->cps = (chan_info **)CALLOC(si->chans, sizeof(chan_info *));
  si->begs = (int *)CALLOC(si->chans, sizeof(int));
  sfs = (snd_fd **)CALLOC(si->chans, sizeof(snd_fd *));
  chn = 0;
  for (snd = 0; snd < ss->max_sounds; snd++)
    {
      sp = (ss->sounds[snd]);
      if (snd_ok(sp))
	for (i = 0; i < sp->nchans; i++)
	  {
	    si->cps[chn] = sp->chans[i];
	    si->begs[chn] = beg;
	    sfs[chn] = init_sample_read(beg, si->cps[chn], READ_FORWARD);
	    chn++;
	  }
    }
  sc = (sync_state *)CALLOC(1, sizeof(sync_state));
  sc->sfs = sfs;
  sc->si = si;
  return(sc);
}
#endif

void eval_expression(chan_info *cp, snd_info *sp, int count, int regexpr)
{
#if HAVE_GUILE
  sync_state *sc; 
  sync_info *si;
  snd_fd *sf;
  int dur, chan_dur = 0, chan, j, k;
  int beg = 0;
  SCM res;
  snd_state *ss;
  snd_fd **sfs = NULL;
  Float val = 0.0;
  char *s1;
  if (gh_procedure_p(sp->eval_proc))
    {
      ss = cp->state;
      if (!regexpr)
	{
	  beg = cp->cursor;
	  if (count < 0)
	    {
	      count = -count;
	      if ((beg - count) >= 0) 
		beg -= count;
	      else
		{
		  count = beg;
		  beg = 0;
		}
	    }
	}
      sc = get_sync_state(ss, sp, cp, beg, regexpr, READ_FORWARD); /* beg ignored if regexpr (starts at region 0 = si->begs[]) */
      if (sc == NULL) return;
      si = sc->si;
      sfs = sc->sfs;
      if (regexpr)
	dur = sc->dur;
      else dur = count;
      if (dur > 0)
	{
	  for (chan = 0; chan < si->chans; chan++)
	    {
	      sf = sfs[chan];	      
	      if (regexpr) 
		chan_dur = dur;
	      else
		{
		  chan_dur = current_ed_samples(si->cps[chan]) - si->begs[chan];
		  if (dur < chan_dur) chan_dur = dur;
		  if (chan_dur == 0) chan_dur = 1;
		  if (dur > MAX_BUFFER_SIZE) start_progress_report(sp, FALSE);
		}
	      j = 0;
	      for (k = 0; k < chan_dur; k++)
		{
		  res = g_call1(sp->eval_proc,
				TO_SCM_DOUBLE((double)next_sample_to_float(sf)), __FUNCTION__);
		  if (SCM_SYMBOLP(res))
		    {
		      for (j = chan; j < si->chans; j++) 
			free_snd_fd(sfs[j]);
		      free_sync_state(sc);
		      scm_throw(res,
				SCM_LIST1(TO_SCM_STRING("eval expression")));
		      return;
		    }
		  if (gh_number_p(res)) val = TO_C_DOUBLE(res);
		  j++;
		  if (j == MAX_BUFFER_SIZE)
		    {
		      progress_report(sp, "C-x C-x", chan, si->chans, (Float)k / ((Float)chan_dur), FALSE);
		      check_for_event(ss);
		      if ((ss->stopped_explicitly) || (!(sp->inuse)))
			{
			  ss->stopped_explicitly = 0;
			  report_in_minibuffer(sp, "stopped");
			  break;
			}
		      j = 0;
		    }
		}
	      free_snd_fd(sf);
	      if (dur > MAX_BUFFER_SIZE) finish_progress_report(sp, NOT_FROM_ENVED);
	    }
	  if ((!regexpr) && (chan_dur == 1))
	    {
	      report_in_minibuffer(sp, "%s = %s", sp->eval_expr, s1 = prettyf(val, 2));
	      FREE(s1);
	    }
	}
      else
	{
	  for (chan = 0; chan < si->chans; chan++)
	    if (sfs[chan]) 
	      sfs[chan] = free_snd_fd(sfs[chan]);
	}
      free_sync_state(sc);
    }
#endif
}

#if HAVE_GUILE
enum {SCAN_CURRENT_CHAN, SCAN_SOUND_CHANS, SCAN_SYNCD_CHANS, SCAN_ALL_CHANS};

static SCM series_scan(snd_state *ss, chan_info *cp, SCM proc, int chan_choice, int beg, int end, 
		       const char *origin, const char *procname, int procn)
{
  sync_state *sc = NULL;
  sync_info *si;
  snd_info *sp;
  snd_fd **sfs;
  snd_fd *sf;
  SCM errstr;
  int kp, j, ip, len, num, reporting = 0, rpt = 0, rpt4;
  SCM res;
  char *errmsg;
  errmsg = procedure_ok(proc, 1, 0, origin, procname, procn);
  if (errmsg)
    {
      errstr = TO_SCM_STRING(errmsg);
      FREE(errmsg);
      snd_bad_arity_error(origin, errstr, proc);
    }
  sp = cp->sound;
  switch (chan_choice)
    {
    case SCAN_SYNCD_CHANS: 
      sc = get_sync_state(ss, sp, cp, beg, FALSE, READ_FORWARD); 
      break;
    case SCAN_ALL_CHANS: 
      sc = get_active_chans_sync_state(ss, beg); 
      break;
    case SCAN_SOUND_CHANS: 
      sc = get_sound_chans_sync_state(cp, beg); 
      break;
    case SCAN_CURRENT_CHAN: 
      sc = get_chan_sync_state(cp, beg); 
      break;
    }
  if (sc == NULL) return(SCM_BOOL_T);
  si = sc->si;
  sfs = sc->sfs;
  rpt4 = MAX_BUFFER_SIZE / 4;
  for (ip = 0; ip < si->chans; ip++)
    {
      cp = si->cps[ip];
      sp = cp->sound;
      sf = sfs[ip];
      /* since each channel can be of arbitrary length, it is unreasonable to
       * expect the caller to deal with that unless a specific subsequence is
       * known in advance; so here if end is 0, that means it was not set
       * by the caller, meaning go from beg to whatever the current end is.
       * if beg > len, omit this chaneel,
       * if beg > end after fixup, omit.
       */
      len = current_ed_samples(cp);
      if (end >= len) end = len - 1;
      if (end == 0) end = len - 1;
      num = end - beg + 1;
      if (num > 0)
	{
	  reporting = (num > MAX_BUFFER_SIZE);
	  if (reporting) start_progress_report(sp, NOT_FROM_ENVED);
	  for (kp = 0; kp < num; kp++)
	    {
	      res = g_call1(proc,
			    TO_SCM_DOUBLE((double)next_sample_to_float(sf)),
			    origin);
	      if ((SCM_NFALSEP(res)) || 
		  (SCM_SYMBOLP(res)))
		{
		  for (j = ip; j < si->chans; j++) 
		    free_snd_fd(sfs[j]);
		  free_sync_state(sc); 
		  if (reporting) 
		    finish_progress_report(sp, NOT_FROM_ENVED);
		  if (SCM_SYMBOLP(res))
		    scm_throw(res,
			      SCM_LIST1(TO_SCM_STRING(origin)));
		  return(gh_list(res,
				 TO_SCM_INT(kp + beg),
				 TO_SMALL_SCM_INT(cp->chan),
				 TO_SMALL_SCM_INT(sp->index),
				 SCM_UNDEFINED));
		}
	      if (reporting) 
		{
		  rpt++;
		  if (rpt > rpt4)
		    {
		      progress_report(sp, origin, ip + 1, si->chans, (Float)kp / (Float)num, NOT_FROM_ENVED);
		      rpt = 0;
		      if (ss->stopped_explicitly)
			{
			  ss->stopped_explicitly = 0;
			  if (reporting) finish_progress_report(sp, NOT_FROM_ENVED);
			  if (si->chans == 1)
			    report_in_minibuffer(sp, "C-G stopped %s at sample %d", 
						 origin, kp + beg);
			  else report_in_minibuffer(sp, "C-G stopped %s in %s chan %d at sample %d",
						    origin, sp->shortname, cp->chan + 1, kp + beg);
			  for (j = ip; j < si->chans; j++) 
			    free_snd_fd(sfs[j]);
			  free_sync_state(sc); 
			  return(SCM_BOOL_F);
			}
		    }
		}
	    }
	  if (reporting) finish_progress_report(sp, NOT_FROM_ENVED);
	}
      sfs[ip] = free_snd_fd(sfs[ip]);
    }
  free_sync_state(sc);
  return(SCM_BOOL_F);
}

static SCM parallel_scan(snd_state *ss, chan_info *cp, SCM proc, int chan_choice, int beg, int end, 
			 const char *origin, const char *procname, int procn)
{
  /* here we need to load the arglist in order */
  sync_state *sc = NULL;
  sync_info *si;
  snd_info *sp;
  snd_fd **sfs;
  SCM *vdata;
  SCM errstr;
  int kp, ip, pos = 0, len, num, reporting = 0, rpt = 0, rpt4;
  SCM res = SCM_UNDEFINED, args, gh_chans;
  char *errmsg;
  errmsg = procedure_ok(proc, 2, 0, origin, procname, procn);
  if (errmsg)
    {
      errstr = TO_SCM_STRING(errmsg);
      FREE(errmsg);
      snd_bad_arity_error(origin, errstr, proc);
    }
  sp = cp->sound;
  switch (chan_choice)
    {
    case SCAN_SYNCD_CHANS: 
      sc = get_sync_state(ss, sp, cp, beg, FALSE, READ_FORWARD); 
      break;
    case SCAN_ALL_CHANS: 
      sc = get_active_chans_sync_state(ss, beg); 
      break;
    case SCAN_SOUND_CHANS: 
      sc = get_sound_chans_sync_state(cp, beg); 
      break;
    case SCAN_CURRENT_CHAN: 
      sc = get_chan_sync_state(cp, beg); 
      break;
    }
  if (sc == NULL) return(SCM_BOOL_T);
  si = sc->si;
  sfs = sc->sfs;
  rpt4 = MAX_BUFFER_SIZE / 4;
  gh_chans = TO_SMALL_SCM_INT(si->chans);
  args = gh_make_vector(gh_chans, SCM_BOOL_F);
  vdata = SCM_VELTS(args);
  /* fixup for different length chans as above, but in this case
   * we need to capture the longest of the channels, padding the
   * others with zero.
   */
  len = current_ed_samples(si->cps[0]);
  for (ip = 1; ip < si->chans; ip++)
    {
      num = current_ed_samples(si->cps[ip]);
      if (num > len) len = num;
    }
  if (end >= len) end = len - 1;
  if (end == 0) end = len - 1;
  num = end - beg + 1;
  reporting = (num > MAX_BUFFER_SIZE);
  if (reporting) start_progress_report(sp, NOT_FROM_ENVED);
  if (si->chans == 1)
    { /* optimize common special case */
      for (kp = 0; kp < num; kp++)
	{
	  vdata[0] = TO_SCM_DOUBLE((double)next_sample_to_float(sfs[0]));
	  res = g_call2(proc, args, gh_chans, origin);
	  if ((SCM_NFALSEP(res)) || 
	      (SCM_SYMBOLP(res))) 
	    {
	      pos = kp + beg; 
	      break;
	    }
	  if (reporting) 
	    {
	      rpt++;
	      if (rpt > rpt4)
		{
		  progress_report(sp, origin, 1, 1, (Float)kp / (Float)num, NOT_FROM_ENVED);
		  rpt = 0;
		  check_for_event(ss);
 		  if ((ss->stopped_explicitly) || (!(cp->sound))) break;
		}
	    }
	}
    }
  else
    {
      for (kp = 0; kp < num; kp++)
	{
	  for (ip = 0; ip < si->chans; ip++)
	    vdata[ip] = TO_SCM_DOUBLE((double)next_sample_to_float(sfs[ip]));
	  res = g_call2(proc, args, gh_chans, origin);
	  if ((SCM_NFALSEP(res)) || 
	      (SCM_SYMBOLP(res))) 
	    {
	      pos = kp + beg; 
	      break;
	    }
	  if (reporting) 
	    {
	      rpt++;
	      if (rpt > rpt4)
		{
		  progress_report(sp, origin, 1, 1, (Float)kp / (Float)num, NOT_FROM_ENVED);
		  rpt = 0;
 		  if (ss->stopped_explicitly) break;
		}
	    }
	}
    }
  if (reporting) finish_progress_report(sp, NOT_FROM_ENVED);
  for (ip = 0; ip < si->chans; ip++) 
    free_snd_fd(sfs[ip]);
  free_sync_state(sc); 
  if (ss->stopped_explicitly)
    {
      report_in_minibuffer(sp, "C-G stopped %s at sample %d", origin, kp + beg);
      ss->stopped_explicitly = 0;
    }
  else
    {
      if (SCM_SYMBOLP(res))
	scm_throw(res,
		  SCM_LIST1(TO_SCM_STRING(origin)));
      else
	{
	  if (SCM_NFALSEP(res))
	    return(TO_SCM_INT(pos));
	}
    }
  return(SCM_BOOL_F);
}

/* someday it might be good to tie this into the rest of this file */
typedef struct {
  int buffer_size, data_size, datumb, loc, fd, filing, orig_size;
  char *filename;
  MUS_SAMPLE_TYPE *buffer;
  MUS_SAMPLE_TYPE **mus_data;
  file_info *hdr, *sp_hdr;
} output_state;

static output_state *start_output(int bufsize, file_info *default_hdr, int orig_size)
{
  output_state *os;
  os = (output_state *)CALLOC(1, sizeof(output_state));
  os->buffer_size = bufsize;
  os->buffer = (MUS_SAMPLE_TYPE *)CALLOC(bufsize, sizeof(MUS_SAMPLE_TYPE));
  os->filing = 0;
  os->sp_hdr = default_hdr;
  os->loc = 0;
  os->data_size = 0;
  os->orig_size = orig_size;
  os->hdr = NULL;
  return(os);
}

static void output_sample(snd_state *ss, output_state *os, int srate, MUS_SAMPLE_TYPE sample)
{
  os->buffer[os->loc] = sample;
  os->loc++;
  os->data_size++;
  if (os->loc == os->buffer_size)
    {
      if (os->filing == 0)
	{
	  os->filename = snd_tempnam(ss);
	  os->filing = 1;
	  os->hdr = make_temp_header(os->filename, srate, 1, 0);
	  os->fd = open_temp_file(os->filename, 1, os->hdr, ss);
	  os->datumb = mus_data_format_to_bytes_per_sample((os->hdr)->format);
	  os->mus_data = (MUS_SAMPLE_TYPE **)CALLOC(1, sizeof(MUS_SAMPLE_TYPE *));
	  os->mus_data[0] = os->buffer;
	}
      mus_file_write(os->fd, 0, os->loc - 1, 1, os->mus_data);
      os->loc = 0;
    }
}

static output_state *end_output(output_state *os, int beg, chan_info *cp, const char *origin)
{
  int cured;
  if (os->data_size > 0)
    {
      if (os->filing)
	{
	  if (os->loc > 0) mus_file_write(os->fd, 0, os->loc - 1, 1, os->mus_data);
	  close_temp_file(os->fd, os->hdr, os->data_size * os->datumb, cp->sound);
	  if (os->data_size == os->orig_size)
	    file_change_samples(beg, os->data_size, os->filename, cp, 0, DELETE_ME, LOCK_MIXES, origin);
	  else
	    {
	      cured = cp->edit_ctr;
	      delete_samples(beg, os->orig_size, cp, origin);
	      file_insert_samples(beg, os->data_size, os->filename, cp, 0, DELETE_ME, origin);
	      backup_edit_list(cp);
	      if (cp->edit_ctr > cured)
		backup_edit_list(cp);
	      if (cp->marks) 
		ripple_trailing_marks(cp, beg, os->orig_size, os->data_size);
	    }
	  free(os->filename);
	  FREE(os->mus_data);
	  if (os->hdr) os->hdr = free_file_info(os->hdr);
	}
      else 
	{
	  if (os->orig_size == os->data_size)
	    change_samples(beg, os->data_size, os->buffer, cp, LOCK_MIXES, origin);
	  else
	    {
	      cured = cp->edit_ctr;
	      delete_samples(beg, os->orig_size, cp, origin);
	      insert_samples(beg, os->data_size, os->buffer, cp, origin);
	      backup_edit_list(cp);
	      if (cp->edit_ctr > cured) 
		backup_edit_list(cp);
	      if (cp->marks) 
		ripple_trailing_marks(cp, beg, os->orig_size, os->data_size);
	    }
	}
      update_graph(cp, NULL);
    }
  FREE(os->buffer);
  FREE(os);
  return(NULL);
}

static SCM series_map(snd_state *ss, chan_info *cp, SCM proc, int chan_choice, int beg, int end, 
		      const char *origin, const char *procname, int procn)
{
  sync_state *sc = NULL;
  sync_info *si;
  snd_info *sp;
  snd_fd **sfs;
  snd_fd *sf;
  output_state *os = NULL;
  int kp, j, k, ip, num, val_size, reporting = 0, rpt = 0, rpt4;
  MUS_SAMPLE_TYPE *vals;
  SCM res;
  SCM errstr;
  char *errmsg;
  errmsg = procedure_ok(proc, 1, 0, origin, procname, procn);
  if (errmsg)
    {
      errstr = TO_SCM_STRING(errmsg);
      FREE(errmsg);
      snd_bad_arity_error(origin, errstr, proc);
    }
  sp = cp->sound;
  switch (chan_choice)
    {
    case SCAN_SYNCD_CHANS: 
      sc = get_sync_state(ss, sp, cp, beg, FALSE, READ_FORWARD); 
      break;
    case SCAN_ALL_CHANS: 
      sc = get_active_chans_sync_state(ss, beg); 
      break;
    case SCAN_SOUND_CHANS: 
      sc = get_sound_chans_sync_state(cp, beg); 
      break;
    case SCAN_CURRENT_CHAN: 
      sc = get_chan_sync_state(cp, beg); 
      break;
    }
  if (sc == NULL) return(SCM_BOOL_T);
  si = sc->si;
  sfs = sc->sfs;
  rpt4 = MAX_BUFFER_SIZE / 4;
  for (ip = 0; ip < si->chans; ip++)
    {
      cp = si->cps[ip];
      sp = cp->sound;
      sf = sfs[ip];
      if (end == 0) end = current_ed_samples(cp) - 1;
      num = end - beg + 1;
      if (num > 0)
	{
	  reporting = (num > MAX_BUFFER_SIZE);
	  if (reporting) start_progress_report(sp, NOT_FROM_ENVED);
	  os = start_output(MAX_BUFFER_SIZE, sp->hdr, num);
	  for (kp = 0; kp < num; kp++)
	    {
	      res = g_call1(proc, 
			    TO_SCM_DOUBLE((double)next_sample_to_float(sf)),
			    origin);
	      if (SCM_NFALSEP(res))                      /* if #f, no output on this pass */
		{
		  if (res != SCM_BOOL_T)                 /* if #t we halt the entire map */
		    {
		      if (SCM_SYMBOLP(res))
			{
			  end_output(os, beg, cp, origin);
			  for (j = ip; j < si->chans; j++) free_snd_fd(sfs[j]);    
			  free_sync_state(sc); 
			  if (reporting) finish_progress_report(sp, NOT_FROM_ENVED);
			  scm_throw(res,
				    SCM_LIST1(TO_SCM_STRING(origin)));
			}
		      if (gh_number_p(res))              /* one number -> replace current sample */
			output_sample(ss, os, SND_SRATE(sp), MUS_FLOAT_TO_SAMPLE(TO_C_DOUBLE(res)));
		      else                               /* list or vector or vct, splice in data */
			{
			  val_size = 0;
			  vals = g_floats_to_samples(res, &val_size, origin, 1);
			  if (vals)
			    {
			      for (k = 0; k < val_size; k++) 
				output_sample(ss, os, SND_SRATE(sp), vals[k]);
			      FREE(vals);
			    }
			}
		    }
		  else                                   /* #t -> halt */
		    {
		      os = end_output(os, beg, cp, origin);
		      for (j = ip; j < si->chans; j++) 
			free_snd_fd(sfs[j]);    
		      free_sync_state(sc); 
		      if (reporting) finish_progress_report(sp, NOT_FROM_ENVED);
		      return(res);
		    }
		}
	      if (reporting) 
		{
		  rpt++;
		  if (rpt > rpt4)
		    {
		      progress_report(sp, origin, ip + 1, si->chans, (Float)kp / (Float)num, NOT_FROM_ENVED);
		      rpt = 0;
		      if (ss->stopped_explicitly) 
			{
			  os = end_output(os, beg, cp, origin);
			  for (j = ip; j < si->chans; j++) 
			    free_snd_fd(sfs[j]);    
			  free_sync_state(sc); 
			  if (reporting) finish_progress_report(sp, NOT_FROM_ENVED);
			  ss->stopped_explicitly = 0;
			  return(SCM_BOOL_F);
			}
		    }
		}
	    }
	  if (reporting) finish_progress_report(sp, NOT_FROM_ENVED);
	  os = end_output(os, beg, cp, origin);
	}
      sfs[ip] = free_snd_fd(sfs[ip]);
    }
  free_sync_state(sc);
  return(SCM_BOOL_F);
}


static SCM parallel_map(snd_state *ss, chan_info *cp, SCM proc, int chan_choice, int beg, int end, 
			const char *origin, const char *procname, int procn)
{
  sync_state *sc = NULL;
  sync_info *si;
  snd_info *sp;
  snd_fd **sfs;
  SCM *vdata, *vargs;
  output_state **os_arr;
  int kp, k, n, ip, len, num, val_size, res_size, reporting = 0, rpt = 0, rpt4;
  MUS_SAMPLE_TYPE *vals;
  SCM res = SCM_UNDEFINED, args, gh_chans, resval, errstr;
  char *errmsg;
  errmsg = procedure_ok(proc, 2, 0, origin, procname, procn);
  if (errmsg)
    {
      errstr = TO_SCM_STRING(errmsg);
      FREE(errmsg);
      snd_bad_arity_error(origin, errstr, proc);
    }
  sp = cp->sound;
  switch (chan_choice)
    {
    case SCAN_SYNCD_CHANS: 
      sc = get_sync_state(ss, sp, cp, beg, FALSE, READ_FORWARD); 
      break;
    case SCAN_ALL_CHANS: 
      sc = get_active_chans_sync_state(ss, beg);
      break;
    case SCAN_SOUND_CHANS: 
      sc = get_sound_chans_sync_state(cp, beg); 
      break;
    case SCAN_CURRENT_CHAN: 
      sc = get_chan_sync_state(cp, beg); 
      break;
    }
  if (sc == NULL) return(SCM_BOOL_T);
  si = sc->si;
  sfs = sc->sfs;
  rpt4 = MAX_BUFFER_SIZE / 4;
  len = current_ed_samples(si->cps[0]);
  for (ip = 1; ip < si->chans; ip++)
    {
      num = current_ed_samples(si->cps[ip]);
      if (num > len) len = num;
    }
  if (end == 0) end = len - 1;
  num = end - beg + 1;
  reporting = (num > MAX_BUFFER_SIZE);
  if (reporting) start_progress_report(sp, NOT_FROM_ENVED);
  os_arr = (output_state **)CALLOC(si->chans, sizeof(output_state *)); 
  for (ip = 0; ip < si->chans; ip++)
    os_arr[ip] = start_output(MAX_BUFFER_SIZE, sp->hdr, num);
  gh_chans = TO_SMALL_SCM_INT(si->chans);
  args = gh_make_vector(gh_chans, SCM_BOOL_F);
  vargs = SCM_VELTS(args);
  for (kp = 0; kp < num; kp++)
    {
      for (ip = 0; ip < si->chans; ip++)
	vargs[ip] = TO_SCM_DOUBLE((double)next_sample_to_float(sfs[ip]));
      res = g_call2(proc, args, gh_chans, origin);
                                                      /* #f -> no output in any channel, #t -> halt */
      if (SCM_NFALSEP(res))                           /* if #f, no output on this pass */
	{
	  if (res != SCM_BOOL_T)                      /* if #t we halt the entire map */
	    {
	      if (SCM_SYMBOLP(res)) break;
	                                              /* assume res here is a vector */
	      if (gh_vector_p(res))
		{
		  res_size = gh_vector_length(res);
		  vdata = SCM_VELTS(res);
		  for (n = 0; n < res_size; n++)
		    {
		      resval = vdata[n];
		      if (SCM_NFALSEP(resval))
			{
			  if (gh_number_p(resval))    /* one number -> replace current sample */
			    output_sample(ss, os_arr[n], 
					  SND_SRATE(sp), 
					  MUS_FLOAT_TO_SAMPLE(TO_C_DOUBLE(resval)));
			  else                        /* list or vector or vct, splice in data */
			    {
			      val_size = 0;
			      vals = g_floats_to_samples(resval, &val_size, origin, 1);
			      if (vals)
				{
				  for (k = 0; k < val_size; k++) 
				    output_sample(ss, os_arr[n], SND_SRATE(sp), vals[k]);
				  FREE(vals);
				}
			    }
			}
		    }
		}
	      else break;                             /* #t -> halt */
	    }
	}
      if (reporting) 
	{
	  rpt++;
	  if (rpt > rpt4)
	    {
	      progress_report(sp, origin, ip + 1, si->chans, (Float)kp / (Float)num, NOT_FROM_ENVED);
	      rpt = 0;
	      if (ss->stopped_explicitly) break;
	    }
	}
    }
  if (reporting) finish_progress_report(sp, NOT_FROM_ENVED);
  for (ip = 0; ip < si->chans; ip++) 
    {
      os_arr[ip] = end_output(os_arr[ip], beg, si->cps[ip], origin);
      sfs[ip] = free_snd_fd(sfs[ip]);
    }
  FREE(os_arr);
  free_sync_state(sc);
  if (SCM_SYMBOLP(res))
    scm_throw(res,
	      SCM_LIST1(TO_SCM_STRING(origin)));
  if (ss->stopped_explicitly)
    {
      ss->stopped_explicitly = 0;
      report_in_minibuffer(sp, "C-G stopped map at sample %d", kp + beg);
    }
  else
    {
      if (SCM_NFALSEP(res))
	return(TO_SCM_INT(kp + beg));
    }
  return(SCM_BOOL_F);
}

#endif /* HAVE_GUILE */

static char *convolve_with_or_error(char *filename, Float amp, chan_info *cp)
{
  /* if string returned, needs to be freed */
  /* amp == 0.0 means unnormalized, cp == NULL means current selection */
  sync_state *sc;
  sync_info *si;
  snd_state *ss;
  snd_info *sp = NULL, *gsp = NULL;
  int ip, stop_point = 0, err, impulse_chan = 0, filter_chans, ok = 0;
  snd_fd **sfs;
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
  sc = get_sync_state(ss, sp, ncp, 0, (cp == NULL), READ_FORWARD);
  if (sc == NULL) return(NULL);
  si = sc->si;
  sfs = sc->sfs;

  origin = (char *)CALLOC(128, sizeof(char));
  sprintf(origin, 
	  "%s \"%s\" %.3f", 
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
	  err = chan_save_edits(ucp, saved_chan_file);
	  if (err != MUS_NO_ERROR)
	    return(mus_format("convolve: save chan (%s[%d]) in %s: %s\n",
			      sp->shortname, ucp->chan, 
			      saved_chan_file, strerror(errno)));
	  else
	    {
	      scfd = mus_file_open_read(saved_chan_file);
	      if (scfd == -1) 
		return(mus_format("convolve: open saved chan (%s[%d]) file %s: %s\n",
				  sp->shortname, ucp->chan, 
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
		      else filesize = current_ed_samples(ucp);
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
				  sp->shortname, ucp->chan, 
				  saved_chan_file, strerror(errno)));
	    }
	  snd_remove(saved_chan_file);
	  free(saved_chan_file);

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
	  if (ofile) free(ofile);
	  sfs[ip] = free_snd_fd(sfs[ip]);
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
   *   and folded into the MUS_SAMPLE_TO_FLOAT calculcation if possible,
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
	  if ((beg == 0) && 
	      (frames >= current_ed_samples(ncp)))
	    {
	      parse_tree_scale_by(ncp, ur_scalers[j]);
	      amp_env_scale_by(ncp, ur_scalers[j]);
	    }
	  else 
	    {
	      parse_tree_selection_scale_by(ncp, ur_scalers[j], beg, frames);
	      amp_env_scale_selection_by(ncp, ur_scalers[j], beg, frames);
	    }
	}
      else
	{
	  parse_tree_scale_by(ncp, ur_scalers[j]);
	  amp_env_scale_by(ncp, ur_scalers[j]);
	}
      j++;
      if (j >= len) j = 0;
      update_graph(ncp, NULL);
    }
  free_sync_info(si);
}

Float get_maxamp(snd_info *sp, chan_info *cp)
{
  snd_fd *sf;
  Float ymax, val;
  int i, len;
  if (!sp) return(0.0);
  if (!cp) cp = sp->chans[0];
  if (amp_env_maxamp_ok(cp)) return(amp_env_maxamp(cp));
  val = ed_maxamp(cp);
  if (val >= 0.0) return(val);
  sf = init_sample_read(0, cp, READ_FORWARD);
  ymax = 0.0;
  len = current_ed_samples(cp);
  for (i = 0; i < len; i++)
    {
      val = next_sample_to_float(sf);
      if (val < 0.0) val = -val;
      if (val > ymax) ymax = val;
    }
  free_snd_fd(sf);
  set_ed_maxamp(cp, ymax);
  return(ymax);
}

static Float get_selection_maxamp(chan_info *cp)
{
  snd_fd *sf;
  Float ymax, val;
  int i, len;
  val = ed_selection_maxamp(cp);
  if (val >= 0.0) return(val);
  sf = init_sample_read(selection_beg(cp), cp, READ_FORWARD);
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
	      else val = get_maxamp(ncp->sound, ncp);
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
	      else val = get_maxamp(ncp->sound, ncp);
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
	  if ((beg == 0) && 
	      (frames >= current_ed_samples(ncp)))
	    {
	      parse_tree_scale_by(ncp, scalers[i]);
	      amp_env_scale_by(ncp, scalers[i]);
	    }
	  else 
	    {
	      parse_tree_selection_scale_by(ncp, scalers[i], beg, frames);
	      amp_env_scale_selection_by(ncp, scalers[i], beg, frames);
	    }
	}
      else
	{
	  parse_tree_scale_by(si->cps[i], scalers[i]);
	  amp_env_scale_by(si->cps[i], scalers[i]);
	}
      update_graph(ncp, NULL);
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
  data0 = (MUS_SAMPLE_TYPE **)CALLOC(1, sizeof(MUS_SAMPLE_TYPE *));
  data0[0] = (MUS_SAMPLE_TYPE *)CALLOC(MAX_BUFFER_SIZE, sizeof(MUS_SAMPLE_TYPE)); 
  data1 = (MUS_SAMPLE_TYPE **)CALLOC(1, sizeof(MUS_SAMPLE_TYPE *));
  data1[0] = (MUS_SAMPLE_TYPE *)CALLOC(MAX_BUFFER_SIZE, sizeof(MUS_SAMPLE_TYPE)); 
  cp0 = c0->cp;
  sp0 = cp0->sound;
  cp1 = c1->cp;
  reporting = ((sp0) && (dur > (MAX_BUFFER_SIZE * 10)));
  if (reporting) start_progress_report(sp0, NOT_FROM_ENVED);
  if (dur > MAX_BUFFER_SIZE)
    {
      temp_file = 1; 
      ofile0 = snd_tempnam(ss);
      hdr0 = make_temp_header(ofile0, SND_SRATE(sp0), 1, dur);
      ofd0 = open_temp_file(ofile0, 1, hdr0, ss);
      datumb = mus_data_format_to_bytes_per_sample(hdr0->format);
      ofile1 = snd_tempnam(ss);
      hdr1 = make_temp_header(ofile1, SND_SRATE(sp0), 1, dur);
      ofd1 = open_temp_file(ofile1, 1, hdr1, ss);
    }
  else temp_file = 0;
  idata0 = data0[0];
  idata1 = data1[0];
  j = 0;
  for (k = 0; k < dur; k++)
    {
      idata0[j] = next_sample(c1);
      idata1[j] = next_sample(c0);
      j++;
      if (temp_file)
	{
	  if (j == MAX_BUFFER_SIZE)
	    {
	      err = mus_file_write(ofd0, 0, j - 1, 1, data0);
	      err = mus_file_write(ofd1, 0, j - 1, 1, data1);
	      j = 0;
	      if (err == -1) break;
	      if (reporting) progress_report(sp0, "scl", 1, 1, (Float)k / (Float)dur, NOT_FROM_ENVED);
	    }
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
      if (ofile0) {free(ofile0); ofile0 = NULL;}
      if (ofile1) {free(ofile1); ofile1 = NULL;}
      if (reporting) finish_progress_report(sp0, NOT_FROM_ENVED);
    }
  else 
    {
      change_samples(beg, dur, data0[0], cp0, LOCK_MIXES, S_swap_channels);
      change_samples(beg, dur, data1[0], cp1, LOCK_MIXES, S_swap_channels);
    }
  update_graph(cp0, NULL);
  update_graph(cp1, NULL);
  if (ofile0) free(ofile0);
  if (ofile1) free(ofile1);
  FREE(data0[0]);
  FREE(data0);
  FREE(data1[0]);
  FREE(data1);
}

typedef struct {int selection; int files; char **old_filenames; char **new_filenames; void *sc;} snd_exf;

static snd_exf *snd_to_temp(chan_info *cp, int selection, int one_file, int header_type, int data_format)
{
  /* save current state starting at cp and following sync buttons
   *   just current selection if selection, else entire channel
   *   in one file is one_file, else separate files.
   *   return array of temp file names
   * this is intended as the first third of the editor interface to an arbitrary external program.
   *   the next stage calls the external program passing the temp file names and getting new temps.
   *   the third step is temp_to_snd -- snd assumes it is deciding when these temps can be deleted.
   *   if a name is null in the returned array (third step input) no edit is performed on the corresponding channel
   */
  snd_state *ss;
  snd_info *sp;
  sync_state *sc;
  sync_info *si;
  int i, chans, len;
  file_info *nhdr;
  snd_fd **temp_sfs;
  snd_exf *data = NULL;
  if (!cp) return(NULL);
  ss = cp->state;
  sp = cp->sound;
  if (!sp->inuse) return(NULL);
  sc = get_sync_state(ss, sp, cp, 0, selection, READ_FORWARD);
  if (sc == NULL) return(NULL);
  si = sc->si;
  chans = si->chans;
  data = (snd_exf *)CALLOC(1, sizeof(snd_exf));
  data->selection = selection;
  if (one_file) 
    data->files = 1; 
  else data->files = chans;
  data->old_filenames = (char **)CALLOC(data->files, sizeof(char *));
  data->new_filenames = (char **)CALLOC(data->files, sizeof(char *));
  for (i = 0; i < data->files; i++)
    {
      data->old_filenames[i] = snd_tempnam(ss);
      data->new_filenames[i] = NULL;
    }
  data->sc = (void *)sc;
  if (one_file)
    {
      if (selection) 
	len = sc->dur; 
      else len = current_ed_samples(cp);
      nhdr = copy_header(data->old_filenames[0], sp->hdr);
      if (header_type != MUS_UNSUPPORTED) nhdr->type = header_type;
      if (data_format != MUS_UNSUPPORTED) nhdr->format = data_format;
      snd_make_file(data->old_filenames[0], chans, nhdr, sc->sfs, len, ss);
      free_file_info(nhdr);
    }
  else
    {
      temp_sfs = (snd_fd **)CALLOC(1, sizeof(snd_fd *));
      for (i = 0; i < chans; i++)
	{
	  if (selection) 
	    len = sc->dur; 
	  else len = current_ed_samples(si->cps[i]);
	  nhdr = copy_header(data->old_filenames[i], sp->hdr);
	  if (header_type != MUS_UNSUPPORTED) nhdr->type = header_type;
	  if (data_format != MUS_UNSUPPORTED) nhdr->format = data_format;
	  temp_sfs[0] = sc->sfs[i];
	  snd_make_file(data->old_filenames[i], 1, nhdr, temp_sfs, len, ss);
	  nhdr = free_file_info(nhdr);
	}
      FREE(temp_sfs);
    }
  for (i = 0; i < chans; i++) 
    sc->sfs[i] = free_snd_fd(sc->sfs[i]);
  return(data);
}

static int temp_to_snd(snd_exf *data, const char *origin)
{
  sync_state *sc;
  sync_info *si;
  int i, k, chans, err, new_len, old_len, new_chans, ok, orig_chans;
  if (!data) return(-1);
  sc = (sync_state *)(data->sc);
  si = sc->si;
  chans = si->chans;
  orig_chans = chans;
  for (i = 0; i < data->files; i++)
    if (data->old_filenames[i])
      {
	/* if user didn't re-use the temp file for his output, delete it */
	if ((data->new_filenames[i] == NULL) || 
	    (strcmp(data->new_filenames[i], data->old_filenames[i]) != 0))
	  err = snd_remove(data->old_filenames[i]);
	free(data->old_filenames[i]);
      }
  if (data->selection)
    {
      /* should this balk if there's no active selection? */
      old_len = selection_len();
      if (data->files == 1)
	{
	  if (snd_strlen(data->new_filenames[0]) > 0)
	    {
	      new_len = mus_sound_samples(data->new_filenames[0])/chans;
	      if (new_len != -1)
		{
		  new_chans = mus_sound_chans(data->new_filenames[0]);
		  if (chans != new_chans)
		    {
		      snd_warning("temp-to-selection: original chans: %d, new chans: %d", chans, new_chans);
		      if (chans > new_chans) 
			chans = new_chans;
		    }
		  if (old_len == new_len)
		    {
		      for (k = 0; k < chans; k++)
			file_change_samples(si->begs[k], old_len, data->new_filenames[0], si->cps[k], k, DELETE_ME, LOCK_MIXES, origin);
		    }
		  else
		    {
		      ok = delete_selection(origin, DONT_UPDATE_DISPLAY);
		      if (!ok) 
			snd_warning("temp-to-selection: no active selection? (inserting at sample %d...)", si->begs[0]);
		      for (k = 0; k < chans; k++)
			{
			  file_insert_samples(si->begs[k], new_len, data->new_filenames[0], si->cps[k], k, DELETE_ME, origin);
			  reactivate_selection(si->cps[k], si->begs[k], si->begs[k] + new_len);
			  if (ok) 
			    backup_edit_list(si->cps[k]);
			  if ((si->cps[k])->marks) 
			    ripple_trailing_marks(si->cps[k], si->begs[k], old_len, new_len);
			}
		    }
		}
	      else snd_warning("temp-to-selection: %s not readable?", data->new_filenames[0]);
	    }
	}
      else
	{
	  ok = delete_selection(origin, DONT_UPDATE_DISPLAY);        /* "ok" means there was a selection that was deleted */
	  if (!ok) 
	    snd_warning("temps-to-selection: no active selection? (inserting at sample %d...)", si->begs[0]);
	  for (k = 0; k < data->files; k++)
	    if (snd_strlen(data->new_filenames[k]) > 0)
	      {
		new_len = mus_sound_samples(data->new_filenames[k]);
		if (new_len != -1)
		  {
		    file_insert_samples(si->begs[k], new_len, data->new_filenames[k], si->cps[k], 0, DELETE_ME, origin);
		    reactivate_selection(si->cps[k], si->begs[k], si->begs[k] + new_len);
		    if (ok) 
		      backup_edit_list(si->cps[k]);
		    if ((si->cps[k])->marks) 
		      ripple_trailing_marks(si->cps[k], si->begs[k], old_len, new_len);
		  }
		else snd_warning("temps-to-selection: %s not readable?", data->new_filenames[k]);
	      }
	}
    }
  else
    {
      if (data->files == 1)
	{
	  if (snd_strlen(data->new_filenames[0]) > 0)
	    {
	      new_len = mus_sound_samples(data->new_filenames[0]) / chans;
	      if (new_len != -1)
		{
		  new_chans = mus_sound_chans(data->new_filenames[0]);
		  if (chans != new_chans)
		    {
		      snd_warning("temp-to-sound: original chans: %d, new chans: %d", chans, new_chans);
		      if (chans > new_chans) 
			chans = new_chans;
		    }
		  for (k = 0; k < chans; k++)
		    file_override_samples(new_len, data->new_filenames[0], si->cps[k], k, DELETE_ME, LOCK_MIXES, origin);
		}
	      else snd_warning("temp-to-sound: %s not readable?", data->new_filenames[0]);
	    }
	}
      else
	{
	  for (k = 0; k < data->files; k++)
	    if (snd_strlen(data->new_filenames[k]) > 0)
	      {
		new_len = mus_sound_samples(data->new_filenames[k]);
		if (new_len != -1)
		  file_override_samples(new_len, data->new_filenames[k], si->cps[k], 0, DELETE_ME, LOCK_MIXES, origin);
		else snd_warning("temps-to-sound: %s not readable?", data->new_filenames[k]);
	      }
	}
    }
  for (i = 0; i < orig_chans; i++)
    {
      update_graph(si->cps[i], NULL); 
      sc->sfs[i] = free_snd_fd(sc->sfs[i]);
    }
  for (i = 0; i < data->files; i++)
    if (data->new_filenames[i]) 
      free(data->new_filenames[i]); /* from tempnam */
  FREE(data->new_filenames);
  FREE(data->old_filenames);
  free_sync_state(sc);
  FREE(data);
  return(0);
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
		    int from_enved, const char *origin, int over_selection, mus_any *gen)
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
		      (ratio < 0.0) ? READ_BACKWARD : READ_FORWARD);      /* 0->beg, 0->regexpr (ratio = 0.0 if from_enved) */
  if (sc == NULL) return;
  si = sc->si;
  sfs = sc->sfs;
  scdur = sc->dur;
  data = (MUS_SAMPLE_TYPE **)CALLOC(1, sizeof(MUS_SAMPLE_TYPE *));
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
	    dur = current_ed_samples(cp); 
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
	  hdr = make_temp_header(ofile, SND_SRATE(sp), 1, dur);
	  ofd = open_temp_file(ofile, 1, hdr, ss);
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
		  idata[j] =(MUS_FLOAT_TO_SAMPLE(run_src(sr, 0.0)));
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
		  new_marks = (int *)CALLOC(cur_marks, sizeof(int));
		  old_marks = (int *)CALLOC(cur_marks, sizeof(int));
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
		egen = mus_make_env(e->data, e->pts, 1.0, 0.0, e->base, 0.0, 0, dur - 1, NULL);
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
	  free(ofile); 
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
	  fht_sines = (Float *)CALLOC(length, sizeof(Float));
	  fht_cosines = (Float *)CALLOC(length, sizeof(Float));
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

static void fht(int powerOfFour, Float *array)
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
  while (i < n-1)	
    {
      i++;
      if (i < j)	
	{
	  t = array[j-1];
	  array[j-1] = array[i-1];
	  array[i-1] = t;
    	}
      k = n4;
      while ((3*k)<j)	
	{
	  j -= 3 * k;
	  k /= 4;
    	}
      j += k;
    }
  for (i = 0; i < n; i += 4) 
    {
      t5 = array[i];
      t6 = array[i+1];
      t7 = array[i+2];
      t8 = array[i+3];
      t1 = t5 + t6;
      t2 = t5 - t6;
      t3 = t7 + t8;
      t4 = t7 - t8;
      array[i] = t1 + t3;
      array[i+1] = t1 - t3;
      array[i+2] = t2 + t4;
      array[i+3] = t2 - t4;
    }
  for (L = 2; L <= powerOfFour; L++)  
    {
      d1 = (int)(pow(2.0 , L+L-3.0));
      d2 = d1 + d1;
      d3 = d2 + d2;
      n_over_d3 = n / 2 / d3;
      d4 = d2 + d3;
      d5 = d3 + d3;
      for (j = 0; j < n; j += d5)	  
	{
	  t5 = array[j];
	  t6 = array[j+d2];
	  t7 = array[j+d3];
	  t8 = array[j+d4];
	  t1 = t5 + t6;
	  t2 = t5 - t6;
	  t3 = t7 + t8;
	  t4 = t7 - t8;
	  array[j] = t1 + t3;
	  array[j+d2] = t1 - t3;
	  array[j+d3] = t2 + t4;
	  array[j+d4] = t2 - t4;
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
#if DEBUGGING
  FREE(fht_sines);
  fht_sines = NULL;
  FREE(fht_cosines);
  fht_cosines = NULL;
  fht_last_length = 0;
  fht_length = 0;
#endif
}


#define USE_MUS_FFT 0

static char *apply_filter_or_error(chan_info *ncp, int order, env *e, int from_enved, 
				   const char *origin, int over_selection, Float *ur_a, mus_any *gen)
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
  Float *sndrdat, *fltdat;
#if USE_MUS_FFT
  Float *sndidat;
  Float spectr;
#else
  int pow4;
#endif

  if ((!e) && (!ur_a) && (!gen)) 
    return(NULL);
  if ((gen) && (!(MUS_RUN_P(gen))))
    return(mus_format("%s can't handle %s generators [%s[%d]: %s]",
		      origin,
		      mus_name(gen),
		      __FILE__, __LINE__, __FUNCTION__));
  ss = ncp->state;
  sp = ncp->sound;
  sc = get_sync_state_1(ss, sp, ncp, 0, over_selection, READ_FORWARD, (over_selection) ? (order - 1) : 0);
  if (sc == NULL) return(NULL);
  si = sc->si;
  sfs = sc->sfs;
  scdur = sc->dur;

  if ((!ur_a) && (!gen) && (!over_selection) && (order >= 256) && ((int)((current_ed_samples(ncp)+order)/128) < ss->memory_available))
    {
      /* use convolution if order is large and there's memory available (and not over_selection) */
      /*   probably faster here would be overlap-add */
      /*   but user is almost certainly making a mistake elsewhere... */
      for (i = 0; i < si->chans; i++)
	{
	  cp = si->cps[i];
	  sp = cp->sound;
	  if (scdur == 0) 
	    dur = current_ed_samples(cp); 
	  else dur = scdur;
	  if (dur == 0) 
	    {
	      if (sfs[i]) {free_snd_fd(sfs[i]); sfs[i] = NULL;}
	      continue;
	    }
#if USE_MUS_FFT
	  /* this is about 3 times slower then the fht version below */
	  fsize = (int)(pow(2.0, (int)ceil(log(order + dur) / log(2.0))));
	  sndrdat = (Float *)CALLOC(fsize, sizeof(Float));
	  sndidat = (Float *)CALLOC(fsize, sizeof(Float));
	  fltdat = env2array(fsize, e);

	  sf = sfs[i]; /* init_sample_read(0, cp, READ_FORWARD); */
	  for (k = 0; k < dur; k++) 
	    sndrdat[k] = (Float)(next_sample_to_float(sf));
	  sfs[i] = free_snd_fd(sf);

	  mus_fft(sndrdat, sndidat, fsize, 1);
	  check_for_event(ss);
	  if (ss->stopped_explicitly)
	    {
	      ss->stopped_explicitly = 0;
	      report_in_minibuffer(sp, "stopped");
	      break;
	    }
	  scale = 2.0 / (Float)fsize;
	  for (k = 0; k < fsize; k++)
	    {
	      spectr = scale * fltdat[k];
	      sndrdat[k] *= spectr;
	      sndidat[k] *= spectr;
	    }
	  mus_fft(sndrdat, sndidat, fsize, -1);
#else
	  fsize = (int)(pow(4.0, pow4 = (int)ceil(log(order + dur) / log(4.0))));
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
	      break;
	    }
	  scale = 1.0 / (Float)fsize;
	  for (k = 0; k < fsize; k++)
	    sndrdat[k] *= (scale * fltdat[k]);         /* fltdat (via env2array) is already reflected around midpoint */
	  fht(pow4, sndrdat);
#endif

	  ofile = snd_tempnam(ss);
	  hdr = make_temp_header(ofile, SND_SRATE(sp), 1, dur);
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
	  write(ofd, sndrdat, fsize * sizeof(Float));
	  close_temp_file(ofd, hdr, fsize * sizeof(Float), sp);
	  hdr = free_file_info(hdr);
	  file_change_samples(0, dur + order, ofile, cp, 0, DELETE_ME, LOCK_MIXES, origin);
	  if (ofile) {free(ofile); ofile = NULL;}
	  update_graph(cp, NULL);
	  FREE(sndrdat);
#if USE_MUS_FFT
	  FREE(sndidat);
#endif
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
      data = (MUS_SAMPLE_TYPE **)CALLOC(1, sizeof(MUS_SAMPLE_TYPE *));
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
		dur = current_ed_samples(cp); 
	      else dur = scdur;
	      if (dur == 0) 
		{
		  if (sfs[i]) {free_snd_fd(sfs[i]); sfs[i] = NULL;}
		  continue;
		}
	      reporting = ((sp) && (dur > (MAX_BUFFER_SIZE * 4)));
	      if (reporting) start_progress_report(sp, from_enved);
	      if (dur > MAX_BUFFER_SIZE)
		{
		  temp_file = 1; 
		  ofile = snd_tempnam(ss);
		  hdr = make_temp_header(ofile, SND_SRATE(sp), 1, dur);
		  ofd = open_temp_file(ofile, 1, hdr, ss);
		  datumb = mus_data_format_to_bytes_per_sample(hdr->format);
		}
	      else temp_file = 0;
	      sf = sfs[i];
	      idata = data[0];
	      if (gen)
		{
		  mus_clear_filter_state(gen);
		}
	      else
		{
		  for (m = 0; m < order; m++) d[m] = 0.0;
		  if (over_selection)
		    {
		      /* see if there's data to pre-load the filter */
		      if (si->begs[i] >= order)
			prebeg = order-1;
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
		      for (m = order-1; m > 0; m--) 
			{
			  x += d[m] * a[m]; 
			  d[m] = d[m-1];
			} 
		      x += d[0] * a[0]; 
		    }
		  idata[j] = MUS_FLOAT_TO_SAMPLE(x);
		  j++;
		  if (temp_file)
		    {
		      if (j == MAX_BUFFER_SIZE)
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
		  if (ofile) {free(ofile); ofile = NULL;}
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
		  const char *origin, int over_selection, Float *ur_a, mus_any *gen)
{
  char *error;
  error = apply_filter_or_error(ncp, order, e, from_enved, origin, over_selection, ur_a, gen);
  if (error)
    {
      snd_error(error);
      FREE(error);
    }
}

static inline MUS_SAMPLE_TYPE previous_sample_unscaled(snd_fd *sf)
{
  if (sf->view_buffered_data < sf->first)
    return(previous_sound(sf));
  else return(*sf->view_buffered_data--);
}

static void reverse_sound(chan_info *ncp, int over_selection)
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
  ss = ncp->state;
  sp = ncp->sound;
  sc = get_sync_state(ss, sp, ncp, 0, over_selection, READ_BACKWARD);
  if (sc == NULL) return;
  si = sc->si;
  sfs = sc->sfs;
  data = (MUS_SAMPLE_TYPE **)CALLOC(1, sizeof(MUS_SAMPLE_TYPE *));
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
	      dur = current_ed_samples(cp);
	      ep = amp_env_copy(cp, TRUE);
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
	      hdr = make_temp_header(ofile, SND_SRATE(sp), 1, dur);
	      ofd = open_temp_file(ofile, 1, hdr, ss);
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
	      if (ofile) {free(ofile); ofile = NULL;}
	    }
	  else change_samples(si->begs[i], dur, data[0], cp, LOCK_MIXES, 
			      (char *)((over_selection) ? S_reverse_selection : S_reverse_sound));
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
	       int from_enved, const char *origin, mus_any *gen)
{
  snd_fd *sf = NULL;
  snd_info *sp;
  sync_info *si;
  sync_state *sc;
  snd_fd **sfs;
  file_info *hdr;
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
	  scale_by(cp, val, 1, regexpr);
	  return;
	}
    }
  si = NULL;
  sp = cp->sound;
  ss = cp->state;
  hdr = sp->hdr;
  sc = get_sync_state(ss, sp, cp, beg, regexpr, READ_FORWARD);
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
  if (e)
    egen = mus_make_env(e->data, e->pts, scaler, 0.0, e->base, 0.0, 0, dur - 1, NULL);
  else egen = gen;

  if (dur > MAX_BUFFER_SIZE) /* if smaller than this, we don't gain anything by using a temp file (its buffers are this large) */
    {
      temp_file = 1; 
      ofile = snd_tempnam(ss); /* see warning below -- don't use tmpnam without deleting free */
      ofd = open_temp_file(ofile, si->chans, hdr, ss);
      if (ofd == -1)
	{
	  free(ofile);
	  if (e) mus_free(egen);
	  for (i = 0; i < si->chans; i++) 
	    if (sfs[i]) 
	      free_snd_fd(sfs[i]);
	  free_sync_state(sc);
	  return; /* hopefully someone raised an error flag?? */
	}
      datumb = mus_data_format_to_bytes_per_sample(hdr->format);
    }
  else temp_file = 0;

  data = (MUS_SAMPLE_TYPE **)CALLOC(si->chans, sizeof(MUS_SAMPLE_TYPE *));
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
	  /* TODO: ?? envelope the amp-env? */
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
  if ((temp_file) && (ofile)) {free(ofile); ofile = NULL;} /* safe only if snd_tempnam, not tmpnam used */
  if (data) FREE(data);
  if (e) mus_free(egen);
  free_sync_state(sc);
}


/* various simple editing ops */

int cursor_delete(chan_info *cp, int count, const char *origin)
{
  int i, beg;
  snd_info *sp;
  sync_info *si;
  chan_info **cps;
  si = NULL;
  beg = cp->cursor;
  sp = cp->sound;
  if (sp->syncing!= 0)
    {
      si = snd_sync(cp->state, sp->syncing);
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
  if (sp->syncing != 0)
    {
      si = snd_sync(cp->state, sp->syncing);
      cps = si->cps;
      for (i = 0; i < si->chans; i++)
	{
	  extend_with_zeros(cps[i], 
			    iclamp(0, beg, current_ed_samples(si->cps[i]) - 1), 
			    count, origin);
	  update_graph(cps[i], NULL);
	}
      si = free_sync_info(si);
    }
  else extend_with_zeros(cp, 
			 iclamp(0, beg, current_ed_samples(cp)), 
			 count, origin);
  return(CURSOR_UPDATE_DISPLAY);
}

int cursor_zeros(chan_info *cp, int count, int regexpr)
{
  int i, num, old_sync, beg;
  snd_info *sp, *nsp;
  MUS_SAMPLE_TYPE *zeros;
  sync_info *si = NULL;
  chan_info *ncp;
  Float scaler[1];
  if (count == 0) return(CURSOR_NO_ACTION);
  if (count < 0) num = -count; else num = count;
  sp = cp->sound;
  if ((sp->syncing != 0) && (!regexpr))
    {
      si = snd_sync(cp->state, sp->syncing);
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
	  old_sync = nsp->syncing;
	  nsp->syncing = 0;
	  scaler[0] = 0.0;
	  scale_by(ncp, scaler, 1, FALSE);
	  nsp->syncing = old_sync;
	}
      else
	{
	  if (count > 0) 
	    beg = si->begs[i];
	  else beg = si->begs[i] + count;
	  /* special case 1 sample -- if already 0, treat as no-op */
	  if ((count != 1) || 
	      (beg >= current_ed_samples(ncp)) || 
	      (sample(beg, ncp) != 0.0))
	    {
	      if (num < 1024)
		{
		  zeros = (MUS_SAMPLE_TYPE *)CALLOC(num, sizeof(MUS_SAMPLE_TYPE));
		  change_samples(beg, num, zeros, ncp, LOCK_MIXES, "C-z"); 
		  FREE(zeros);
		}
	      else parse_tree_selection_scale_by(ncp, 0.0, beg, num);
	      amp_env_scale_selection_by(ncp, 0.0, beg, num);
	      update_graph(ncp, NULL);
	    }
	}
    }
  si = free_sync_info(si);
  return(CURSOR_IN_VIEW);
}

static sync_state *get_sync_state_without_snd_fds(snd_state *ss, snd_info *sp, chan_info *cp, int beg, int regexpr)
{
  sync_info *si = NULL;
  int dur, i;
  sync_state *sc;
  dur = 0;
  if ((sp->syncing != 0) && (!regexpr))
    {
      si = snd_sync(ss, sp->syncing);
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
      y0 = sample(si->begs[i], ncp);
      y1 = sample(si->begs[i] + num, ncp);
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



#if HAVE_GUILE
#include "vct.h"
#include "clm2scm.h"

#define USE_FULL_FILE 0
#define USE_SELECTION 1
#define USE_ONE_FILE 1
#define USE_MANY_FILES 0

static SCM g_temp_filenames(SCM data)
{
  #define H_temp_filenames "(" S_temp_filenames " data) -> vector of temp filenames (used by sound-to-temp et al)"
  snd_exf *program_data;
  int i;
  SCM *vlst;
  SCM lst;
  SCM_ASSERT(SND_WRAPPED(data), data, SCM_ARG1, S_temp_filenames);
  program_data = (snd_exf *)(SND_UNWRAP(data));
  lst = gh_make_vector(TO_SCM_INT(program_data->files), SCM_BOOL_F);
  vlst = SCM_VELTS(lst);
  for (i = 0; i < program_data->files; i++)
    vlst[i] = TO_SCM_STRING(program_data->old_filenames[i]);
  return(lst);
}

static SCM g_sound_to_temp_1(SCM ht, SCM df, int selection, int one_file)
{
  snd_exf *program_data;
  chan_info *cp;
  int type, format;
  snd_state *ss;
  if ((selection) && (selection_is_active() == 0)) 
    snd_no_active_selection_error((one_file) ? S_selection_to_temp : S_selection_to_temps);
  ss = get_global_state();
  cp = current_channel(ss);
  if (cp)
    {
      type = TO_C_INT_OR_ELSE(ht, MUS_UNSUPPORTED);
      format = TO_C_INT_OR_ELSE(df, MUS_UNSUPPORTED);
      program_data = snd_to_temp(cp, selection, one_file, type, format);
      if (program_data)
	return(SND_WRAP(program_data));
    }
  return(SCM_BOOL_F);
}

static SCM g_sound_to_temp(SCM ht, SCM df) 
{
  #define H_sound_to_temp "(" S_sound_to_temp " &optional header-type data-format) writes the syncd data to a temp file \
with the indicated header type and data format; returns temp file name"

  return(g_sound_to_temp_1(ht, df, USE_FULL_FILE, USE_ONE_FILE));
}

static SCM g_sound_to_temps(SCM ht, SCM df) 
{
  #define H_sound_to_temps "(" S_sound_to_temps " &optional header-type data-format) writes the syncd data to mono temp files \
with the indicated header type and data format; returns temp file names"

  return(g_sound_to_temp_1(ht, df, USE_FULL_FILE, USE_MANY_FILES));
}

static SCM g_selection_to_temp(SCM ht, SCM df) 
{
  #define H_selection_to_temp "(" S_selection_to_temp " &optional header-type data-format) writes the selected data to a temp file \
with the indicated header type and data format; returns temp file name"

  return(g_sound_to_temp_1(ht, df, USE_SELECTION, USE_ONE_FILE));
}

static SCM g_selection_to_temps(SCM ht, SCM df) 
{
  #define H_selection_to_temps "(" S_selection_to_temps " &optional header-type data-format) writes the selected data to mono temp files \
with the indicated header type and data format; returns temp file names"

  return(g_sound_to_temp_1(ht, df, USE_SELECTION, USE_MANY_FILES));
}

static SCM g_temp_to_sound(SCM data, SCM new_name, SCM origin)
{
  #define H_temp_to_sound "(" S_temp_to_sound " data new-name origin) reads new-name to complete the edit begun by " S_sound_to_temp " \
using data returned by the latter and origin as the edit history entry for the edit"

  snd_exf *program_data;
  SCM_ASSERT(gh_string_p(new_name), new_name, SCM_ARG2, S_temp_to_sound);
  SCM_ASSERT(gh_string_p(origin), origin, SCM_ARG3, S_temp_to_sound);
  SCM_ASSERT(SND_WRAPPED(data), data, SCM_ARG1, S_temp_to_sound);
  program_data = (snd_exf *)(SND_UNWRAP(data));
  program_data->new_filenames[0] = TO_NEW_C_STRING(new_name);
  temp_to_snd(program_data, TO_C_STRING(origin));
  return(SCM_BOOL_T);
}

static SCM g_temps_to_sound(SCM data, SCM new_names, SCM origin)
{
  #define H_temps_to_sound "(" S_temps_to_sound " data new-names origin) reads new-names to complete the edit begun by " S_sound_to_temps " \
using data returned by the latter and origin as the edit history entry for the edit"

  snd_exf *program_data;
  int i, len;
  SCM *vdata;
  SCM_ASSERT(SND_WRAPPED(data), data, SCM_ARG1, S_temps_to_sound);
  SCM_ASSERT((gh_vector_p(new_names)), new_names, SCM_ARG2, S_temps_to_sound);
  SCM_ASSERT(gh_string_p(origin), origin, SCM_ARG3, S_temps_to_sound);
  program_data = (snd_exf *)(SND_UNWRAP(data));
  len = (int)gh_vector_length(new_names);
  vdata = SCM_VELTS(new_names);
  for (i = 0; i < len; i++)
    program_data->new_filenames[i] = TO_NEW_C_STRING(vdata[i]);
  temp_to_snd(program_data, TO_C_STRING(origin));
  return(SCM_BOOL_T);
}

static SCM g_sp_scan(SCM proc, int chan_choice, SCM s_beg, SCM s_end, int series, int scan, SCM snd, SCM chn, 
		     const char *caller, const char *procname, int procn)
{
  snd_state *ss;
  chan_info *cp;
  int beg, end;
  SCM result;
  SCM_ASSERT((gh_procedure_p(proc)), proc, SCM_ARG1, caller);
  SCM_ASSERT(NUMBER_OR_BOOLEAN_IF_BOUND_P(s_beg), s_beg, SCM_ARG2, caller);
  SCM_ASSERT(NUMBER_OR_BOOLEAN_IF_BOUND_P(s_end), s_end, SCM_ARG3, caller);
  ss = get_global_state();
  cp = get_cp(snd, chn, caller);
  beg = TO_C_INT_OR_ELSE(s_beg, 0);
  end = TO_C_INT_OR_ELSE(s_end, 0);
  if (scan)
    {
      if (series)
	return(series_scan(ss, cp, proc, chan_choice, beg, end, caller, procname, procn));
      else return(parallel_scan(ss, cp, proc, chan_choice, beg, end, caller, procname, procn));
    }
  else
    {
      if (series)
	result = series_map(ss, cp, proc, chan_choice, beg, end, caller, procname, proc);
      else result = parallel_map(ss, cp, proc, chan_choice, beg, end, caller, procname, procn);
      return(result);
    }
}

static SCM g_scan_chan(SCM proc, SCM beg, SCM end, SCM snd, SCM chn) 
{ 
  #define H_scan_chan "(" S_scan_chan " func &optional (start 0) end snd chn)\n\
apply func to samples in current channel (or the specified channel) \
func is a function of one argument, either the current sample, or #f (to indicate end-of-data) \
if func returns non-#f, the scan stops, and the value is returned to the caller with the sample number"

  SND_ASSERT_CHAN(S_scan_chan, snd, chn, 4); 
  return(g_sp_scan(proc, SCAN_CURRENT_CHAN, beg, end, TRUE, TRUE, snd, chn, S_scan_chan, "func", 1));
}

static SCM g_scan_chans(SCM proc, SCM beg, SCM end) 
{ 
  #define H_scan_chans "(" S_scan_chans " func &optional (start 0) end)\n\
   apply func to samples in all sync'd channels, one channel after another"
   
  return(g_sp_scan(proc, SCAN_SYNCD_CHANS, beg, end, TRUE, TRUE, SCM_BOOL_F, SCM_BOOL_F, S_scan_chans, "func", 1));
}

static SCM g_scan_all_chans(SCM proc, SCM beg, SCM end) 
{ 
  #define H_scan_all_chans "(" S_scan_all_chans " func &optional (start 0) end)\n\
   apply func to samples in all channels, one after the other"

  return(g_sp_scan(proc, SCAN_ALL_CHANS, beg, end, TRUE, TRUE, SCM_BOOL_F, SCM_BOOL_F, S_scan_all_chans, "func", 1));
}

static SCM g_scan_sound_chans(SCM proc, SCM beg, SCM end, SCM snd) 
{ 
  #define H_scan_sound_chans "(" S_scan_sound_chans " func &optional (start 0) end snd)\n\
apply func to samples in all of sound snd's channels"

  SND_ASSERT_SND(S_scan_sound_chans, snd, 4); 
  return(g_sp_scan(proc, SCAN_SOUND_CHANS, beg, end, TRUE, TRUE, snd, SCM_BOOL_F, S_scan_sound_chans, "func", 1));
}

static SCM g_scan_across_chans(SCM proc, SCM beg, SCM end) 
{ 
  #define H_scan_across_chans "(" S_scan_across_chans " func &optional (start 0) end)\n\
apply func to samples in all sync'd channels in parallel"

  return(g_sp_scan(proc, SCAN_SYNCD_CHANS, beg, end, FALSE, TRUE, SCM_BOOL_F, SCM_BOOL_F, S_scan_across_chans, "func", 1));
}

static SCM g_scan_across_all_chans(SCM proc, SCM beg, SCM end) 
{ 
  #define H_scan_across_all_chans "(" S_scan_across_all_chans " func &optional (start 0) end)\n\
apply func to samples in all channels in parallel"

  return(g_sp_scan(proc, SCAN_ALL_CHANS, beg, end, FALSE, TRUE, SCM_BOOL_F, SCM_BOOL_F, S_scan_across_all_chans, "func", 1));
}

static SCM g_scan_across_sound_chans(SCM proc, SCM beg, SCM end, SCM snd) 
{ 
  #define H_scan_across_sound_chans "(" S_scan_across_sound_chans " func &optional (start 0) end snd)\n\
apply func to samples in sound snd's channels in parallel"

  SND_ASSERT_SND(S_scan_across_sound_chans, snd, 4); 
  return(g_sp_scan(proc, SCAN_SOUND_CHANS, beg, end, FALSE, TRUE, snd, SCM_BOOL_F, S_scan_across_sound_chans, "func", 1));
}

static SCM g_map_chan(SCM proc, SCM beg, SCM end, SCM org, SCM snd, SCM chn) 
{ 
  #define H_map_chan "(" S_map_chan "func &optional (start 0) end edname snd chn)\n\
apply func to samples in current channel, edname is the edit history name for this editing operation"

  char *caller;
  if (gh_string_p(org)) 
    caller = TO_C_STRING(org);
  else caller = S_map_chan;
  SND_ASSERT_CHAN(S_map_chan, snd, chn, 5); 
  return(g_sp_scan(proc, SCAN_CURRENT_CHAN, beg, end, TRUE, FALSE, snd, chn, caller, "func", 1));
}

static SCM g_map_chans(SCM proc, SCM beg, SCM end, SCM org) 
{ 
  #define H_map_chans "(" S_map_chans "func &optional (start 0) end edname)\n\
apply func to currently sync'd channels, edname is the edit history name for this editing operation"

  char *caller;
  if (gh_string_p(org)) 
    caller = TO_C_STRING(org);
  else caller = S_map_chans;
  return(g_sp_scan(proc, SCAN_SYNCD_CHANS, beg, end, TRUE, FALSE, SCM_BOOL_F, SCM_BOOL_F, caller, "func", 1));
}

static SCM g_map_all_chans(SCM proc, SCM beg, SCM end, SCM org) 
{ 
  #define H_map_all_chans "(" S_map_all_chans "func &optional (start 0) end edname)\n\
apply func to all channels, edname is the edit history name for this editing operation"

  char *caller;
  if (gh_string_p(org)) 
    caller = TO_C_STRING(org);
  else caller = S_map_all_chans;
  return(g_sp_scan(proc, SCAN_ALL_CHANS, beg, end, TRUE, FALSE, SCM_BOOL_F, SCM_BOOL_F, caller, "func", 1));
}

static SCM g_map_sound_chans(SCM proc, SCM beg, SCM end, SCM org, SCM snd) 
{
  #define H_map_sound_chans "(" S_map_sound_chans "func &optional (start 0) end edname snd)\n\
apply func to sound snd's channels, edname is the edit history name for this editing operation"

  char *caller;
  if (gh_string_p(org)) 
    caller = TO_C_STRING(org);
  else caller = S_map_sound_chans;
  SND_ASSERT_SND(S_map_sound_chans, snd, 5); 
  return(g_sp_scan(proc, SCAN_SOUND_CHANS, beg, end, TRUE, FALSE, snd, SCM_BOOL_F, caller, "func", 1));
}

static SCM g_map_across_chans(SCM proc, SCM beg, SCM end, SCM org) 
{
  #define H_map_across_chans "(" S_map_across_chans "func &optional (start 0) end edname)\n\
apply func to currently sync'd channels in parallel, edname is the edit history name for this editing operation"

  char *caller;
  if (gh_string_p(org)) 
    caller = TO_C_STRING(org);
  else caller = S_map_across_chans;
  return(g_sp_scan(proc, SCAN_SYNCD_CHANS, beg, end, FALSE, FALSE, SCM_BOOL_F, SCM_BOOL_F, caller, "func", 1));
}

static SCM g_map_across_all_chans(SCM proc, SCM beg, SCM end, SCM org) 
{
  #define H_map_across_all_chans "(" S_map_across_all_chans "func &optional (start 0) end edname)\n\
apply func to all channels in parallel, edname is the edit history name for this editing operation"

  char *caller;
  if (gh_string_p(org)) 
    caller = TO_C_STRING(org);
  else caller = S_map_across_all_chans;
  return(g_sp_scan(proc, SCAN_ALL_CHANS, beg, end, FALSE, FALSE, SCM_BOOL_F, SCM_BOOL_F, caller, "func", 1));
}

static SCM g_map_across_sound_chans(SCM proc, SCM beg, SCM end, SCM org, SCM snd) 
{
  #define H_map_across_sound_chans "(" S_map_across_sound_chans "func &optional (start 0) end edname snd)\n\
apply func to sound snd's channels in parallel, edname is the edit history name for this editing operation"

  char *caller;
  if (gh_string_p(org)) 
    caller = TO_C_STRING(org);
  else caller = S_map_across_sound_chans;
  SND_ASSERT_SND(S_map_across_sound_chans, snd, 5); 
  return(g_sp_scan(proc, SCAN_SOUND_CHANS, beg, end, FALSE, FALSE, snd, SCM_BOOL_F, caller, "func", 1));
}

static SCM g_find(SCM expr, SCM sample, SCM snd_n, SCM chn_n)
{
  #define H_find "(" S_find " func &optional (start-samp 0) snd chn) applies func, a function of one argument, \
the current sample, to each sample in snd's channel chn, starting at 'start-samp' until func returns #t"

  /* no free here -- it's handled as ss->search_expr in snd-find.c */
  SCM_ASSERT(gh_procedure_p(expr), expr, SCM_ARG1, S_find);
  SCM_ASSERT(NUMBER_IF_BOUND_P(sample), sample, SCM_ARG2, S_find);
  SND_ASSERT_CHAN(S_find, snd_n, chn_n, 3);
  return(g_sp_scan(expr, SCAN_CURRENT_CHAN, sample, SCM_BOOL_F, TRUE, TRUE, snd_n, chn_n, S_find, "func", 1));
}

static SCM g_count_matches(SCM expr, SCM sample, SCM snd_n, SCM chn_n)
{
  #define H_count_matches "(" S_count_matches " func &optional (start-samp 0) snd chn) returns how many \
samples satisfy func (a function of one argument, the current sample, returning #t upon match)"

  chan_info *cp = NULL;
  int samp = 0, matches, lim;
  SCM match, cursamp;
  SCM_ASSERT(gh_procedure_p(expr), expr, SCM_ARG1, S_count_matches);
  SCM_ASSERT(NUMBER_IF_BOUND_P(sample), sample, SCM_ARG2, S_count_matches);
  SND_ASSERT_CHAN(S_count_matches, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_count_matches);
  samp = TO_C_INT_OR_ELSE(sample, 0);
  matches = 0;
  lim = current_ed_samples(cp);
  while (samp < lim)
    {
      cursamp = TO_SCM_INT(samp);
      match = g_sp_scan(expr, SCAN_CURRENT_CHAN, cursamp, SCM_BOOL_F, TRUE, TRUE, snd_n, chn_n, S_count_matches, "func", 1);
      if ((gh_list_p(match)) && 
	  (SCM_TRUE_P(SCM_CAR(match))))
	{
	  matches++;
	  samp = TO_C_INT_OR_ELSE(SCM_CADR(match), 0) + 1;
	}
      else break;
    }
  return(TO_SCM_INT(matches));
}

static SCM g_smooth(SCM beg, SCM num, SCM snd_n, SCM chn_n)
{
  #define H_smooth "(" S_smooth " start-samp samps &optional snd chn) smooths data from start-samp for samps in snd's channel chn"
  chan_info *cp;
  SCM_ASSERT(NUMBER_P(beg), beg, SCM_ARG1, S_smooth);
  SCM_ASSERT(NUMBER_P(num), num, SCM_ARG2, S_smooth);
  SND_ASSERT_CHAN(S_smooth, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_smooth);
  cos_smooth(cp,
	     TO_C_INT_OR_ELSE(beg, 0),
	     TO_C_INT_OR_ELSE(num, 0),
	     FALSE,
	     S_smooth); 
  return(SCM_BOOL_T);
}

static SCM g_smooth_selection(void)
{
  #define H_smooth_selection "(" S_smooth_selection ") smooths the data in the currently selected portion"
  chan_info *cp;
  if (selection_is_active() == 0) 
    snd_no_active_selection_error(S_smooth_selection);
  cp = get_cp(SCM_BOOL_F, SCM_BOOL_F, S_smooth_selection);
  cos_smooth(cp, 0, 0, TRUE, S_smooth_selection);
  return(SCM_BOOL_T);
}

static SCM g_reverse_sound(SCM snd_n, SCM chn_n)
{
  #define H_reverse_sound "(" S_reverse_sound " &optional snd chn) reverses snd's channel chn"
  chan_info *cp;
  SND_ASSERT_CHAN(S_reverse_sound, snd_n, chn_n, 1);
  cp = get_cp(snd_n, chn_n, S_reverse_sound);
  reverse_sound(cp, FALSE);
  return(SCM_BOOL_F);
}

static SCM g_reverse_selection(void)
{
  #define H_reverse_selection "(" S_reverse_selection ") reverses the data in the currently selected portion"
  chan_info *cp;
  if (selection_is_active() == 0) 
    snd_no_active_selection_error(S_reverse_selection);
  cp = get_cp(SCM_BOOL_F, SCM_BOOL_F, S_reverse_selection);
  reverse_sound(cp, TRUE);
  return(SCM_BOOL_F);
}

static SCM g_insert_silence(SCM beg, SCM num, SCM snd, SCM chn)
{
  #define H_insert_silence "(" S_insert_silence " beg num snd chn) inserts num zeros at beg in snd's chn"
  chan_info *cp;
  SCM_ASSERT(NUMBER_P(beg), beg, SCM_ARG1, S_insert_silence);
  SCM_ASSERT(NUMBER_P(num), num, SCM_ARG2, S_insert_silence);
  SND_ASSERT_CHAN(S_insert_silence, snd, chn, 3);
  cp = get_cp(snd, chn, S_insert_silence);
  cursor_insert(cp,
		TO_C_INT_OR_ELSE(beg, 0),
		TO_C_INT_OR_ELSE(num, 0),
		S_insert_silence);
  return(beg);
}

static SCM g_swap_channels(SCM snd0, SCM chn0, SCM snd1, SCM chn1, SCM beg, SCM dur)
{
  #define H_swap_channels "(" S_swap_channels " snd0 chn0 snd1 chn1) swaps the indicated channels"
  chan_info *cp0 = NULL, *cp1 = NULL;
  snd_fd *c0, *c1;
  int dur0 = 0, dur1 = 0, beg0 = 0, num, old_squelch0, old_squelch1;
  snd_info *sp = NULL;
  env_info *e0, *e1;
  SND_ASSERT_CHAN(S_swap_channels, snd0, chn0, 1);
  cp0 = get_cp(snd0, chn0, S_swap_channels);
  if (SCM_INUMP(snd1) && SCM_INUMP(chn1)) 
    {
      SND_ASSERT_CHAN(S_swap_channels, snd1, chn1, 3);
      cp1 = get_cp(snd1, chn1, S_swap_channels);
    }
  else
    {
      if (SCM_INUMP(snd1))
	sp = get_sp(snd1);
      else sp = cp0->sound;
      if (SCM_INUM(snd0) == SCM_INUM(snd1))
	{
	  if ((cp0->chan + 1) < sp->nchans)
	    cp1 = sp->chans[cp0->chan + 1];
	  else cp1 = sp->chans[0];
	}
      else cp1 = sp->chans[0];
    }
  if ((cp0) && (cp1))
    {
      if (SCM_INUMP(beg)) 
	beg0 = SCM_INUM(beg);
      if (SCM_INUMP(dur)) 
	num = SCM_INUM(dur);
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
	  e0 = amp_env_copy(cp0, FALSE);
	  e1 = amp_env_copy(cp1, FALSE);
	  cp0->squelch_update = 1;
	  cp1->squelch_update = 1;
	  file_override_samples(dur1, cp1->sound->fullname, cp0, cp1->chan, DONT_DELETE_ME, LOCK_MIXES, S_swap_channels);
	  file_override_samples(dur0, cp0->sound->fullname, cp1, cp0->chan, DONT_DELETE_ME, LOCK_MIXES, S_swap_channels);
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
  return(SCM_BOOL_F);
}

static SCM g_fht(SCM data)
{
  #define H_fht "(fht vct-obj) returns the Hartley transform of the data in the vct object whose size must be a power of 4"
  vct *v;
  int pow4;
  SCM_ASSERT(vct_p(data), data, SCM_ARG1, S_fht);
  v = get_vct(data);
  pow4 = (int)(round(log(v->length) / (log(4))));
  if (((int)(pow(4.0, pow4))) != v->length) 
    scm_misc_error(S_fht,
		   "fht data length must be a power of 4: ~S: ~S (~S)",
		   SCM_LIST3(TO_SCM_INT(v->length),
			     TO_SCM_DOUBLE((log(v->length) / (log(4)))),
			     TO_SCM_INT((int)(pow(4.0, pow4)))));
  fht(pow4, v->data);
  return(data);
}

static Float *load_Floats(SCM scalers, int *result_len)
{
  int len, i;
  Float *scls;
  SCM lst;
  SCM *vdata;
  if (gh_vector_p(scalers))
    len = gh_vector_length(scalers);
  else
    if (gh_list_p(scalers))
      len = gh_length(scalers);
    else len = 1;
  if (len <= 0) len = 1;
  scls = (Float *)CALLOC(len, sizeof(Float));
  if (gh_vector_p(scalers))
    {
      vdata = SCM_VELTS(scalers);
      for (i = 0; i < len; i++) 
	scls[i] = (Float)TO_C_DOUBLE(vdata[i]);
    }
  else
    if (gh_list_p(scalers))
      {
	for (i = 0, lst = scalers; i < len; i++, lst = SCM_CDR(lst)) 
	  scls[i] = (Float)TO_C_DOUBLE(SCM_CAR(lst));
      }
    else
      if (gh_number_p(scalers))
	scls[0] = (Float)TO_C_DOUBLE(scalers);
      else scls[0] = 1.0;
  result_len[0] = len;
  return(scls);
}

static SCM g_scale_to(SCM scalers, SCM snd_n, SCM chn_n)
{
  #define H_scale_to "(" S_scale_to " norms &optional snd chn)\n\
normalizes snd to norms (following sync) norms can be a float or a vector of floats"

  /* chn_n irrelevant if syncing */
  chan_info *cp;
  int len[1];
  Float *scls;
  SND_ASSERT_CHAN(S_scale_to, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_scale_to);
  scls = load_Floats(scalers, len);
  scale_to(cp->state, cp->sound, cp, scls, len[0], FALSE); /* last arg for selection */
  FREE(scls);
  return(scalers);
}

static SCM g_scale_by(SCM scalers, SCM snd_n, SCM chn_n)
{
  #define H_scale_by "(" S_scale_by " scalers &optional snd chn)\n\
scales snd by scalers (following sync) scalers can be a float or a vector of floats"

  /* chn_n irrelevant if syncing */
  chan_info *cp;
  int len[1];
  Float *scls;
  SND_ASSERT_CHAN(S_scale_by, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_scale_by);
  scls = load_Floats(scalers, len);
  scale_by(cp, scls, len[0], FALSE);
  FREE(scls);
  return(scalers);
}

static SCM g_scale_selection_to(SCM scalers)
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

static SCM g_scale_selection_by(SCM scalers)
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

static SCM g_env_selection(SCM edata, SCM base, SCM snd_n, SCM chn_n)
{
  #define H_env_selection "(" S_env_selection " env &optional (env-base 1.0) snd chn)\n\
applies envelope 'env' to the currently selected portion of snd's channel chn using 'env-base' to determine how breakpoints are connected"

  chan_info *cp;
  env *e;
  mus_any *egen;
  SND_ASSERT_CHAN(S_env_selection, snd_n, chn_n, 3);
  if (selection_is_active() == 0) 
    snd_no_active_selection_error(S_env_selection);
  cp = get_cp(snd_n, chn_n, S_env_selection);
  if (gh_list_p(edata))
    {
      e = get_env(edata, base, S_env_selection);
      if (e)
	{
	  apply_env(cp, e, 0, 0, 1.0, TRUE, NOT_FROM_ENVED, S_env_selection, NULL);
	  free_env(e);
	  return(edata);
	}
    }
  else
    {
      if (mus_scm_p(edata))
	{
	  egen = mus_scm_to_clm(edata);
	  if (mus_env_p(egen))
	    {
	      apply_env(cp, NULL, 0, 0, 1.0, TRUE, NOT_FROM_ENVED, S_env_selection, egen);
	      return(edata);
	    }
	  else scm_wrong_type_arg(S_env_selection, 1, edata);
	}
      else scm_wrong_type_arg(S_env_selection, 1, edata);
    }
  return(SCM_BOOL_F);
}

static SCM g_env_sound(SCM edata, SCM samp_n, SCM samps, SCM base, SCM snd_n, SCM chn_n)
{
  #define H_env_sound "(" S_env_sound " env &optional (start-samp 0) samps (env-base 1.0) snd chn)\n\
applies amplitude envelope 'env' (a list of breakpoints or a CLM env) to snd's channel chn starting at start-samp, going \
either to the end of the sound or for 'samps' samples, with segments interpolating according to 'env-base'"

  chan_info *cp;
  env *e;
  int beg = 0, dur;
  mus_any *egen;
  SCM_ASSERT(NUMBER_IF_BOUND_P(samp_n), samp_n, SCM_ARG2, S_env_sound);
  SCM_ASSERT(NUMBER_IF_BOUND_P(samps), samps, SCM_ARG3, S_env_sound);
  SND_ASSERT_CHAN(S_env_sound, snd_n, chn_n, 5);
  cp = get_cp(snd_n, chn_n, S_env_sound);
  beg = TO_C_INT_OR_ELSE(samp_n, 0);
  dur = TO_C_INT_OR_ELSE(samps, 0);
  if (dur == 0) dur = current_ed_samples(cp);
  if (gh_list_p(edata))
    {
      e = get_env(edata, base, S_env_sound);
      if (e)
	{
	  apply_env(cp, e, beg, dur, 1.0, FALSE, NOT_FROM_ENVED, S_env_sound, NULL);
	  free_env(e);
	  return(edata);
	}
    }
  else
    {
      if (mus_scm_p(edata))
	{
	  egen = mus_scm_to_clm(edata);
	  if (mus_env_p(egen))
	    {
	      apply_env(cp, NULL, beg, dur, 1.0, FALSE, NOT_FROM_ENVED, S_env_sound, egen);
	      return(edata);
	    }
	  else scm_wrong_type_arg(S_env_sound, 1, edata);
	}
      else scm_wrong_type_arg(S_env_sound, 1, edata);
    }
  return(SCM_BOOL_F);
}

static SCM g_fft_1(SCM reals, SCM imag, SCM sign, int use_fft)
{
  vct *v1 = NULL, *v2 = NULL;
  int ipow, n, n2, i, isign = 1;
  Float *rl, *im;
  SCM *rvdata, *ivdata;
  SCM_ASSERT(((vct_p(reals)) || (gh_vector_p(reals))), reals, SCM_ARG1, ((use_fft) ? S_fft : S_convolve_arrays));
  SCM_ASSERT(((vct_p(imag)) || (gh_vector_p(imag))), imag, SCM_ARG2, ((use_fft) ? S_fft : S_convolve_arrays));
  if ((vct_p(reals)) && (vct_p(imag)))
    {
      v1 = (vct *)SND_VALUE_OF(reals);
      v2 = (vct *)SND_VALUE_OF(imag);
      n = v1->length;
    }
  else
    n = gh_vector_length(reals);
  ipow = (int)ceil(log((Float)n) / log(2.0));
  n2 = (int)pow(2.0, (Float)ipow);
  if ((!v1) || (n != n2))
    {
      rl = (Float *)CALLOC(n2, sizeof(Float));
      im = (Float *)CALLOC(n2, sizeof(Float));
    }
  else
    {
      rl = v1->data;
      im = v2->data;
    }
  if (gh_number_p(sign)) isign = TO_C_INT_OR_ELSE(sign, 0);
  if (isign == 0) isign = 1;
  if (v1 == NULL)
    {
      rvdata = SCM_VELTS(reals);
      ivdata = SCM_VELTS(imag);
      for (i = 0; i < n; i++)
	{
	  rl[i] = TO_C_DOUBLE(rvdata[i]);
	  im[i] = TO_C_DOUBLE(ivdata[i]);
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
	  rvdata = SCM_VELTS(reals);
	  ivdata = SCM_VELTS(imag);
	  for (i = 0; i < n; i++)
	    {
	      rvdata[i] = TO_SCM_DOUBLE(rl[i]);
	      ivdata[i] = TO_SCM_DOUBLE(im[i]);
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
	  rvdata = SCM_VELTS(reals);
	  for (i = 0; i < n; i++)
	    rvdata[i] = TO_SCM_DOUBLE(rl[i]);
	}
      else
	{
	  if (n != n2)
	    for (i = 0; i < n; i++) 
	      v1->data[i] = rl[i];
	}
    }
  if ((!v1) || (n != n2))
    {
      FREE(rl);
      FREE(im);
    }
  return(reals);
}

static SCM g_fft(SCM reals, SCM imag, SCM sign)
{
  #define H_fft "(" S_fft " reals imags &optional (sign 1)) ffts the data returning the result in reals. \
If sign is -1, performs inverse fft"

  return(g_fft_1(reals, imag, sign, TRUE));
}

static SCM g_convolve_with(SCM file, SCM new_amp, SCM snd_n, SCM chn_n)
{
  #define H_convolve_with "(" S_convolve_with " file &optional (amp 1.0) snd chn)\n\
convolves file with snd's channel chn (or the currently sync'd channels), amp is the resultant peak amp"

  chan_info *cp;
  Float amp;
  SCM errstr;
  char *fname = NULL, *error = NULL;
  SCM_ASSERT(gh_string_p(file), file, SCM_ARG1, S_convolve_with);
  SND_ASSERT_CHAN(S_convolve_with, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_convolve_with);
  if (gh_number_p(new_amp)) 
    amp = TO_C_DOUBLE(new_amp);
  else
    {
      if (SCM_FALSEP(new_amp))
	amp = 0.0;
      else amp = 1.0;
    }
  fname = mus_expand_filename(TO_C_STRING(file));
  if (mus_file_probe(fname))
    {
      error = convolve_with_or_error(fname, amp, cp);
      if (error)
	{
	  if (fname) FREE(fname);
	  errstr = TO_SCM_STRING(error);
	  FREE(error);
	  mus_misc_error(S_convolve_with, NULL, errstr);
	}
    }
  else 
    {
      if (fname) FREE(fname);
      snd_no_such_file_error(S_convolve_with, file);
    }
  if (fname) FREE(fname);
  return(scm_return_first(file, new_amp));
}

static SCM g_snd_spectrum(SCM data, SCM win, SCM len, SCM linear_or_dB)
{
  #define H_snd_spectrum "(" S_snd_spectrum " data window len linear-or-dB)\n\
return magnitude spectrum of data (vct) in data using fft-window win and fft length len"

  int i, n, linear, wtype;
  Float maxa, todb, lowest, val;
  Float *idat, *rdat, *window;
  vct *v;
  SCM_ASSERT((vct_p(data)), data, SCM_ARG1, S_snd_spectrum);
  SCM_ASSERT(INTEGER_P(win), win, SCM_ARG2, S_snd_spectrum);
  SCM_ASSERT(INTEGER_P(len), len, SCM_ARG3, S_snd_spectrum);
  SCM_ASSERT(BOOLEAN_IF_BOUND_P(linear_or_dB), linear_or_dB, SCM_ARG1, S_snd_spectrum);
  v = get_vct(data);
  rdat = v->data;
  n = TO_C_INT(len);
  if (n <= 0)
    mus_misc_error(S_snd_spectrum, "length <= 0?", len);
  if (n > v->length) n = v->length;
  if (SCM_TRUE_P(linear_or_dB)) linear = 1; else linear = 0;
  wtype = TO_C_INT(win);
  if (!(MUS_FFT_WINDOW_OK(wtype)))
    mus_misc_error(S_snd_spectrum, "unknown fft window", win);
  idat = (Float *)CALLOC(n, sizeof(Float));
  window = (Float *)CALLOC(n, sizeof(Float));
  make_fft_window_1(window, n, wtype, 0.0);
  for (i = 0; i < n; i++) rdat[i] *= window[i];
  FREE(window);
  mus_fft(rdat, idat, n, 1);
  lowest = 0.00000001;
  maxa = 0.0;
  n = n / 2;
  for (i = 0; i < n; i++)
    {
      val = rdat[i] * rdat[i] + idat[i] * idat[i];
      if (val < lowest)
	idat[i] = .0001;
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
	    idat[i] = todb * log(idat[i] * maxa);
	}
      else 
	for (i = 0; i < n; i++) 
	  idat[i] *= maxa;
    }
  return(scm_return_first(make_vct(n, idat), data));
}

static SCM g_convolve_selection_with(SCM file, SCM new_amp)
{
  #define H_convolve_selection_with "(" S_convolve_selection_with " file &optional (amp 1.0))\n\
convolves the current selection with file; amp is the resultant peak amp"

  Float amp;
  SCM errstr;
  char *fname = NULL, *error;
  SCM_ASSERT(gh_string_p(file), file, SCM_ARG1, S_convolve_selection_with);
  if (selection_is_active() == 0) 
    snd_no_active_selection_error(S_convolve_selection_with);
  if (gh_number_p(new_amp)) 
    amp = TO_C_DOUBLE(new_amp);
  else
    {
      if (SCM_FALSEP(new_amp))
	amp = 0.0;
      else amp = 1.0;
    }
  fname = mus_expand_filename(TO_C_STRING(file));
  if (mus_file_probe(fname))
    {
      error = convolve_with_or_error(fname, amp, NULL);
      if (error)
	{
	  if (fname) FREE(fname);
	  errstr = TO_SCM_STRING(error);
	  FREE(error);
	  mus_misc_error(S_convolve_selection_with, NULL, errstr);
	}
    }
  else 
    {
      if (fname) FREE(fname);
      snd_no_such_file_error(S_convolve_selection_with, file);
    }
  if (fname) FREE(fname);
  return(scm_return_first(file, new_amp));
}

static SCM g_convolve(SCM reals, SCM imag)
{
  #define H_convolve "(" S_convolve_arrays " rl1 rl2) convolves vectors or vcts rl1 and rl2, result in rl1 (which needs to be big enough)"
  /* if reals is a string = filename and imag is a Float (or nada), assume user missppelledd convolve-with */
  if (gh_string_p(reals))
    return(g_convolve_with(reals, imag, SCM_BOOL_F, SCM_BOOL_F));
  /* result in reals (which needs to be big enough and zero padded) */
  else return(g_fft_1(reals, imag, TO_SMALL_SCM_INT(1), FALSE));
}

static Float check_src_envelope(env *e, char *caller)
{
  /* can't go through zero here, and if negative need to return 1.0 */
  int i;
  Float res = 0.0;
  for (i = 0; i < (2 * e->pts); i += 2)
    {

      if (e->data[i + 1] == 0.0)
	mus_misc_error(caller, "src envelope hits 0.0", env2scm(e));
      else
	{
	  if (e->data[i + 1] < 0.0)
	    {
	      if (res <= 0.0)
		res = -1.0;
	      else mus_misc_error(caller, "src envelope passes through 0.0", env2scm(e));
	    }
	  else
	    {
	      if (res >= 0)
		res = 1.0;
	      else mus_misc_error(caller, "src envelope passes through 0.0", env2scm(e));
	    }
	}
    }
  return(res);
}

static SCM g_src_sound(SCM ratio_or_env, SCM base, SCM snd_n, SCM chn_n)
{
  #define H_src_sound "(" S_src_sound " ratio-or-env &optional (base 1.0) snd chn)\n\
sampling-rate converts snd's channel chn by ratio, or following an envelope. Negative ratio reverses the sound"

  chan_info *cp;
  env *e = NULL;
  mus_any *egen;
  Float e_ratio = 1.0;
  SND_ASSERT_CHAN(S_src_sound, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_src_sound);
  if (gh_number_p(ratio_or_env))
    src_env_or_num(cp->state, cp, NULL, TO_C_DOUBLE(ratio_or_env), TRUE, NOT_FROM_ENVED, S_src_sound, FALSE, NULL);
  else 
    {
      if (gh_list_p(ratio_or_env))
	{
	  e = get_env(ratio_or_env, base, S_src_sound);
	  e_ratio = check_src_envelope(e, S_src_sound);
	  src_env_or_num(cp->state, cp,
			 e, e_ratio, FALSE, NOT_FROM_ENVED, S_src_sound, FALSE, NULL);
	  if (e) free_env(e);
	}
      else
	{
	  if (mus_scm_p(ratio_or_env))
	    {
	      egen = mus_scm_to_clm(ratio_or_env);
	      if (mus_env_p(egen))
		src_env_or_num(cp->state, cp, NULL, 
			       (mus_phase(egen) >= 0.0) ? 1.0 : -1.0,
			       FALSE, NOT_FROM_ENVED, S_src_sound, FALSE, egen);
	      else mus_misc_error(S_src_sound, "clm gen not an envelope handler", ratio_or_env);
	    }
	  else scm_wrong_type_arg(S_src_sound, 1, ratio_or_env);
	}
    }
  return(scm_return_first(ratio_or_env, base));
}

static SCM g_src_selection(SCM ratio_or_env, SCM base)
{
  #define H_src_selection "(" S_src_selection " ratio-or-env &optional (base 1.0))\n\
sampling-rate converts the currently selected data by ratio (which can be an envelope)"
  env *e = NULL;
  mus_any *egen;
  Float e_ratio = 1.0;
  chan_info *cp;
  if (selection_is_active() == 0) 
    snd_no_active_selection_error(S_src_selection);
  cp = get_cp(SCM_BOOL_F, SCM_BOOL_F, S_src_selection);
  if (gh_number_p(ratio_or_env))
    src_env_or_num(cp->state, cp, 
		   NULL, 
		   TO_C_DOUBLE(ratio_or_env), TRUE, NOT_FROM_ENVED, S_src_selection, TRUE, NULL);
  else 
    {
      if (gh_list_p(ratio_or_env))
	{
	  e = get_env(ratio_or_env, base, S_src_selection);
	  e_ratio = check_src_envelope(e, S_src_selection);
	  src_env_or_num(cp->state, cp,
			 e, e_ratio, FALSE, NOT_FROM_ENVED, S_src_selection, TRUE, NULL);
	  if (e) free_env(e);
	}
      else
	{
	  if (mus_scm_p(ratio_or_env))
	    {
	      egen = mus_scm_to_clm(ratio_or_env);
	      if (mus_env_p(egen))
		src_env_or_num(cp->state, cp, NULL,
			       (mus_phase(egen) >= 0.0) ? 1.0 : -1.0,
			       FALSE, NOT_FROM_ENVED, S_src_selection, TRUE, egen);
	      else mus_misc_error(S_src_selection, "clm generator not an envelope handler", ratio_or_env);
	    }
	  else scm_wrong_type_arg(S_src_selection, 1, ratio_or_env);
	}
    }
  return(scm_return_first(ratio_or_env, base));
}

static SCM g_filter_sound(SCM e, SCM order, SCM snd_n, SCM chn_n)
{
  #define H_filter_sound "(" S_filter_sound " filter order &optional snd chn)\n\
applies FIR filter to snd's channel chn. 'filter' is either the frequency response envelope, a CLM filter, or a vct object with the actual coefficients"

  chan_info *cp;
  vct *v;
  int len;
  SCM errstr;
  env *ne = NULL;
  char *error;
  SND_ASSERT_CHAN(S_filter_sound, snd_n, chn_n, 3);
  cp = get_cp(snd_n, chn_n, S_filter_sound);
  if (mus_scm_p(e))
    {
      error = apply_filter_or_error(cp, 0, NULL, NOT_FROM_ENVED, S_filter_sound, FALSE, NULL, mus_scm_to_clm(e));
      if (error)
	{
	  errstr = TO_SCM_STRING(error);
	  FREE(error);
	  mus_misc_error(S_filter_sound, NULL, errstr);
	}
    }
  else
    {
      len = TO_C_INT_OR_ELSE(order, 0);
#if HAVE_SCM_OUT_OF_RANGE_POS
      if (len <= 0) 
	scm_out_of_range_pos(S_filter_sound, order, TO_SMALL_SCM_INT(2));
#endif
      if (vct_p(e)) /* the filter coefficients direct */
	{
	  v = get_vct(e);
#if HAVE_SCM_OUT_OF_RANGE_POS
	  if (len > v->length) 
	    scm_out_of_range_pos(S_filter_sound, order, TO_SMALL_SCM_INT(2));
#endif
	  apply_filter(cp, len, NULL, NOT_FROM_ENVED, S_filter_sound, FALSE, v->data, NULL);
	}
      else 
	{
	  if (gh_vector_p(e) || (gh_list_p(e)))
	    {
	      apply_filter(cp, len,
			   ne = get_env(e, TO_SCM_DOUBLE(1.0), S_filter_sound),
			   NOT_FROM_ENVED, S_filter_sound, FALSE, NULL, NULL);
	      if (ne) free_env(ne); 
	    }
	  else scm_wrong_type_arg(S_filter_sound, 1, e);
	}
    }
  return(scm_return_first(SCM_BOOL_T, e));
}

static SCM g_filter_selection(SCM e, SCM order)
{
  #define H_filter_selection "(" S_filter_selection " filter order) applies filter to current selection"
  chan_info *cp;
  char *error;
  vct *v;
  int len;
  SCM errstr;
  env *ne = NULL;
  if (selection_is_active() == 0) 
    snd_no_active_selection_error(S_filter_selection);
  cp = get_cp(SCM_BOOL_F, SCM_BOOL_F, S_filter_selection);
  if (mus_scm_p(e))
    {
      error = apply_filter_or_error(cp, 0, NULL, NOT_FROM_ENVED, S_filter_selection, TRUE, NULL, mus_scm_to_clm(e));
      if (error)
	{
	  errstr = TO_SCM_STRING(error);
	  FREE(error);
	  mus_misc_error(S_filter_selection, NULL, errstr);
	}
    }
  else
    {
      len = TO_C_INT_OR_ELSE(order, 0);
#if HAVE_SCM_OUT_OF_RANGE_POS
      if (len <= 0) 
	scm_out_of_range_pos(S_filter_selection, order, TO_SMALL_SCM_INT(2));
#endif
      if (vct_p(e)) /* the filter coefficients direct */
	{
	  v = get_vct(e);
#if HAVE_SCM_OUT_OF_RANGE_POS
	  if (len > v->length) 
	    scm_out_of_range_pos(S_filter_selection, order, TO_SMALL_SCM_INT(2));
#endif
	  apply_filter(cp, len, NULL, NOT_FROM_ENVED, S_filter_selection, TRUE, v->data, NULL);
	}
      else 
	{
	  if (gh_vector_p(e) || (gh_list_p(e)))
	    {
	      apply_filter(cp, len,
			   ne = get_env(e, TO_SCM_DOUBLE(1.0), S_filter_selection),
			   NOT_FROM_ENVED, S_filter_selection, TRUE, NULL, NULL); 
	      if (ne) free_env(ne);
	    }
	  else scm_wrong_type_arg(S_filter_selection, 1, e);
	}
    }
  return(scm_return_first(SCM_BOOL_T, e));
}


void g_init_sig(SCM local_doc)
{
  DEFINE_PROC(gh_new_procedure(S_temp_filenames,          SCM_FNC g_temp_filenames, 1, 0, 0),          H_temp_filenames);
  DEFINE_PROC(gh_new_procedure(S_sound_to_temp,           SCM_FNC g_sound_to_temp, 0, 2, 0),           H_sound_to_temp);
  DEFINE_PROC(gh_new_procedure(S_sound_to_temps,          SCM_FNC g_sound_to_temps, 0, 2, 0),          H_sound_to_temps);
  DEFINE_PROC(gh_new_procedure(S_selection_to_temp,       SCM_FNC g_selection_to_temp, 0, 2, 0),       H_selection_to_temp);
  DEFINE_PROC(gh_new_procedure(S_selection_to_temps,      SCM_FNC g_selection_to_temps, 0, 2, 0),      H_selection_to_temps);
  DEFINE_PROC(gh_new_procedure(S_temp_to_sound,           SCM_FNC g_temp_to_sound, 3, 0, 0),           H_temp_to_sound);
  DEFINE_PROC(gh_new_procedure(S_temps_to_sound,          SCM_FNC g_temps_to_sound, 3, 0, 0),          H_temps_to_sound);
  DEFINE_PROC(gh_new_procedure(S_temp_to_selection,       SCM_FNC g_temp_to_sound, 3, 0, 0),           H_temp_to_sound);
  DEFINE_PROC(gh_new_procedure(S_temps_to_selection,      SCM_FNC g_temps_to_sound, 3, 0, 0),          H_temps_to_sound);
  DEFINE_PROC(gh_new_procedure(S_scan_chan,               SCM_FNC g_scan_chan, 1, 4, 0),               H_scan_chan);
  DEFINE_PROC(gh_new_procedure(S_scan_chans,              SCM_FNC g_scan_chans, 1, 2, 0),              H_scan_chans);
  DEFINE_PROC(gh_new_procedure(S_scan_all_chans,          SCM_FNC g_scan_all_chans, 1, 2, 0),          H_scan_all_chans);
  DEFINE_PROC(gh_new_procedure(S_scan_sound_chans,        SCM_FNC g_scan_sound_chans, 1, 3, 0),        H_scan_sound_chans);
  DEFINE_PROC(gh_new_procedure(S_scan_across_chans,       SCM_FNC g_scan_across_chans, 1, 2, 0),       H_scan_across_chans);
  DEFINE_PROC(gh_new_procedure(S_scan_across_all_chans,   SCM_FNC g_scan_across_all_chans, 1, 2, 0),   H_scan_across_all_chans);
  DEFINE_PROC(gh_new_procedure(S_scan_across_sound_chans, SCM_FNC g_scan_across_sound_chans, 1, 3, 0), H_scan_across_sound_chans);
  DEFINE_PROC(gh_new_procedure(S_map_chan,                SCM_FNC g_map_chan, 1, 5, 0),                H_map_chan);
  DEFINE_PROC(gh_new_procedure(S_map_chans,               SCM_FNC g_map_chans, 1, 3, 0),               H_map_chans);
  DEFINE_PROC(gh_new_procedure(S_map_all_chans,           SCM_FNC g_map_all_chans, 1, 3, 0),           H_map_all_chans);
  DEFINE_PROC(gh_new_procedure(S_map_sound_chans,         SCM_FNC g_map_sound_chans, 1, 4, 0),         H_map_sound_chans);
  DEFINE_PROC(gh_new_procedure(S_map_across_chans,        SCM_FNC g_map_across_chans, 1, 3, 0),        H_map_across_chans);
  DEFINE_PROC(gh_new_procedure(S_map_across_all_chans,    SCM_FNC g_map_across_all_chans, 1, 3, 0),    H_map_across_all_chans);
  DEFINE_PROC(gh_new_procedure(S_map_across_sound_chans,  SCM_FNC g_map_across_sound_chans, 1, 4, 0),  H_map_across_sound_chans);
  DEFINE_PROC(gh_new_procedure(S_find,                    SCM_FNC g_find, 1, 3, 0),                    H_find);
  DEFINE_PROC(gh_new_procedure(S_count_matches,           SCM_FNC g_count_matches, 1, 3, 0),           H_count_matches);

  DEFINE_PROC(gh_new_procedure(S_smooth,                  SCM_FNC g_smooth, 2, 2, 0),                  H_smooth);
  DEFINE_PROC(gh_new_procedure(S_smooth_selection,        SCM_FNC g_smooth_selection, 0, 0, 0),        H_smooth_selection);
  DEFINE_PROC(gh_new_procedure(S_reverse_sound,           SCM_FNC g_reverse_sound, 0, 2, 0),           H_reverse_sound);
  DEFINE_PROC(gh_new_procedure(S_reverse_selection,       SCM_FNC g_reverse_selection, 0, 0, 0),       H_reverse_selection);
  DEFINE_PROC(gh_new_procedure(S_swap_channels,           SCM_FNC g_swap_channels, 0, 6, 0),           H_swap_channels);
  DEFINE_PROC(gh_new_procedure(S_insert_silence,          SCM_FNC g_insert_silence, 2, 2, 0),          H_insert_silence);
  DEFINE_PROC(gh_new_procedure(S_fht,                     SCM_FNC g_fht, 1, 0, 0),                     H_fht);

  DEFINE_PROC(gh_new_procedure(S_scale_selection_to,      SCM_FNC g_scale_selection_to, 0, 1, 0),      H_scale_selection_to);
  DEFINE_PROC(gh_new_procedure(S_scale_selection_by,      SCM_FNC g_scale_selection_by, 0, 1, 0),      H_scale_selection_by);
  DEFINE_PROC(gh_new_procedure(S_scale_to,                SCM_FNC g_scale_to, 0, 3, 0),                H_scale_to);
  DEFINE_PROC(gh_new_procedure(S_scale_by,                SCM_FNC g_scale_by, 0, 3, 0),                H_scale_by);
  DEFINE_PROC(gh_new_procedure(S_env_selection,           SCM_FNC g_env_selection, 1, 3, 0),           H_env_selection);
  DEFINE_PROC(gh_new_procedure(S_env_sound,               SCM_FNC g_env_sound, 1, 5, 0),               H_env_sound);
  DEFINE_PROC(gh_new_procedure(S_fft,                     SCM_FNC g_fft, 2, 1, 0),                     H_fft);
  DEFINE_PROC(gh_new_procedure(S_snd_spectrum,            SCM_FNC g_snd_spectrum, 3, 1, 0),            H_snd_spectrum);
  DEFINE_PROC(gh_new_procedure(S_convolve_arrays,         SCM_FNC g_convolve, 1, 1, 0),                H_convolve);
  DEFINE_PROC(gh_new_procedure(S_convolve_with,           SCM_FNC g_convolve_with, 1, 3, 0),           H_convolve_with);
  DEFINE_PROC(gh_new_procedure(S_convolve_selection_with, SCM_FNC g_convolve_selection_with, 1, 1, 0), H_convolve_selection_with);
  DEFINE_PROC(gh_new_procedure(S_src_sound,               SCM_FNC g_src_sound, 1, 3, 0),               H_src_sound);
  DEFINE_PROC(gh_new_procedure(S_src_selection,           SCM_FNC g_src_selection, 1, 1, 0),           H_src_selection);
  DEFINE_PROC(gh_new_procedure(S_filter_sound,            SCM_FNC g_filter_sound, 1, 3, 0),            H_filter_sound);
  DEFINE_PROC(gh_new_procedure(S_filter_selection,        SCM_FNC g_filter_selection, 1, 1, 0),        H_filter_selection);

}

#endif
