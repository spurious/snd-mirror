#include "snd.h"


/* -------------------------------- DATA STRUCTURES -------------------------------- 
 *
 * axis_info: axis data
 * file_info: header data describing sound data in file
 * fft_info: data relating to one fft display
 * chan_info: state info for one channel
 * snd_info: state info for one sound
 * snd_state: overall state of program
 */

lisp_grf *free_lisp_info(chan_info *cp)
{
  lisp_grf *lg;
  int i;
  if (cp)
    {
      lg = cp->lisp_info;
      if (lg)
	{
	  if (lg->axis) 
	    free_axis_info(lg->axis);
	  if (lg->data) 
	    {
	      for (i=0;i<lg->graphs;i++) 
		if (lg->data[i]) 
		  FREE(lg->data[i]);
	      FREE(lg->data);
	    }
	  if (lg->len) 
	    FREE(lg->len);
	  FREE(lg);
	}
    }
  return(NULL);
}

chan_info *make_chan_info(chan_info *cip, int chan, snd_info *sound, snd_state *ss)
{
  chan_info *cp; /* may be re-use */
  if (!cip)
    {
      cp = (chan_info *)CALLOC(1,sizeof(chan_info)); 
      cp->cgx = (chan_context *)CALLOC(1,sizeof(chan_context));
      (cp->cgx)->ax = (axis_context *)CALLOC(1,sizeof(axis_context));
      cp->mixes = 0;
      cp->last_sonogram = NULL;
      cp->temp_sonogram = NULL;
#if HAVE_HOOKS
      cp->edit_hook = scm_make_hook(SCM_MAKINUM(0));
      snd_protect(cp->edit_hook);
      cp->undo_hook = scm_make_hook(SCM_MAKINUM(0));
      snd_protect(cp->undo_hook);
#endif
    }
  else cp = cip;
  cp->tcgx = NULL;
  cp->chan = chan;
  cp->sound = sound;
  cp->sound_ctr = -1;
  cp->edit_ctr = -1;
  cp->sound_size = 0;
  cp->edit_size = 0;
  cp->cursor_on = 0;
  cp->cursor_visible = 0;
  cp->selection_visible = 0;
  cp->cursor = 0;
  cp->cursor_style = CURSOR_CROSS;
  cp->squelch_update = 0;
  cp->show_y_zero = show_y_zero(ss);
  cp->show_marks = show_marks(ss);
  cp->wavo = wavo(ss);
  cp->wavo_hop = wavo_hop(ss);
  cp->wavo_trace = wavo_trace(ss);
  cp->line_size = line_size(ss);
  cp->max_fft_peaks = max_fft_peaks(ss);
  cp->show_fft_peaks = show_fft_peaks(ss);
  cp->zero_pad = zero_pad(ss);
  cp->verbose_cursor = verbose_cursor(ss);
  cp->fft_log_frequency = fft_log_frequency(ss);
  cp->fft_log_magnitude = fft_log_magnitude(ss);
  cp->min_dB = ss->min_dB;
  cp->lin_dB = ss->lin_dB;
  cp->wavelet_type = wavelet_type(ss);
  cp->spectro_x_angle = spectro_x_angle(ss);
  cp->spectro_y_angle = spectro_y_angle(ss);
  cp->spectro_z_angle = spectro_z_angle(ss);
  cp->spectro_x_scale = spectro_x_scale(ss);
  cp->spectro_y_scale = spectro_y_scale(ss);
  cp->spectro_z_scale = spectro_z_scale(ss);
  cp->spectro_cutoff = spectro_cutoff(ss);
  cp->spectro_start = spectro_start(ss);
  cp->spectro_hop = spectro_hop(ss);
  cp->fft_beta = fft_beta(ss);
  cp->fft_size = fft_size(ss);
  cp->fft_style = fft_style(ss);
  cp->fft_window = fft_window(ss);
  cp->transform_type = transform_type(ss);
  cp->normalize_fft = normalize_fft(ss);
  cp->show_mix_waveforms = show_mix_waveforms(ss);
  cp->graph_style = graph_style(ss);
  cp->graphs_horizontal = graphs_horizontal(ss);
  cp->dot_size = dot_size(ss);
  cp->show_axes = show_axes(ss);
  cp->sync = 0;
  cp->waving = 1; /* the default state (button is set when we start) */
  cp->ffting = 0;
  cp->printing = 0;
  cp->waiting_to_make_graph = 0;
  cp->state = ss;
  cp->amp_envs = NULL;
  cp->sonogram_data = NULL;
  cp->lisp_info = NULL;
  cp->mix_md = NULL;
  cp->stats = NULL;
  cp->hookable = 1;
  cp->gzy = 1.0;
  cp->gsy = 1.0;
  cp->selection_transform_size = 0;
  if (cp->last_sonogram) {FREE(cp->last_sonogram); cp->last_sonogram = NULL;}
  return(cp);
}

static chan_info *free_chan_info(chan_info *cp)
{
  /* this does not free the associated widgets -- they are merely unmanaged */
  snd_state *ss;
  chan_info_cleanup(cp);
  ss = cp->state;
  cp->squelch_update = 1;
  cp->tcgx = NULL;
  cp->axis = free_axis_info(cp->axis);
  if (cp->fft) cp->fft = free_fft_info(cp->fft);
  if (cp->fft_data) {FREE(cp->fft_data); cp->fft_data = NULL;}
  /* this may leave ->wp window unfreed? -- see snd-fft.c free_fft_state */
  cp->ffting = 0;
  cp->printing = 0;
  cp->waving = 1;
  if (cp->samples) {FREE(cp->samples); cp->samples = NULL;}
  if (cp->edits) free_edit_list(cp);
  if (cp->sounds) free_sound_list(cp);
  if (cp->marks) free_mark_list(cp,-1);
  if (cp->sounds) {FREE(cp->sounds); cp->sounds = NULL;}
  free_mixes(cp);
  cp->sound = NULL;  /* a backpointer */
  cp->state = NULL;
  cp->mix_md = NULL;
  cp->cursor_on = 0;
  cp->cursor_visible = 0;
  cp->selection_visible = 0;
  cp->cursor = 0;
  cp->cursor_style = CURSOR_CROSS;
  cp->waiting_to_make_graph = 0;
  if (cp->sonogram_data) free_sono_info(cp);
  if (cp->temp_sonogram) {FREE(cp->temp_sonogram); cp->temp_sonogram = NULL;} /* special case -- background fft process never got a chance to run */
  if (cp->last_sonogram) {FREE(cp->last_sonogram); cp->last_sonogram = NULL;}
  if (cp->lisp_info) cp->lisp_info = free_lisp_info(cp);
  if (cp->stats) 
    { 
      FREE(cp->stats); 
      cp->stats = NULL;
      if (show_usage_stats(ss)) update_stats_display(ss,FALSE);
    }
  cp->lisp_graphing = 0;
  cp->selection_transform_size = 0;
#if HAVE_HOOKS
  scm_reset_hook_x(cp->edit_hook);
  scm_reset_hook_x(cp->undo_hook);
#endif
  return(cp);  /* pointer is left for possible future re-use */
}

static void file_maxamps(char *ifile, Float *vals, int ichans, int format);

snd_info *make_snd_info(snd_info *sip, snd_state *state, char *filename, file_info *hdr, int snd_slot)
{
  snd_info *sp = NULL;
  int chans,i;
  Float secs;
  snd_state *ss = (snd_state *)state;
  /* assume file has been found and header read before reaching us */
  /* if a reused pointer, may need to extend current chans array */
  chans = hdr->chans;
  if (!sip)
    {
      sp = (snd_info *)CALLOC(1,sizeof(snd_info));
      sp->chans = (chan_info **)CALLOC(chans,sizeof(chan_info *));
      sp->allocated_chans = chans;
      sp->sgx = (snd_context *)CALLOC(1,sizeof(snd_context));
    }
  else 
    {
      sp = sip;
      if (sp->allocated_chans < chans) 
	{
	  sp->chans = (chan_info **)REALLOC(sp->chans,chans*sizeof(chan_info *));
	  for (i=sp->allocated_chans;i<chans;i++) sp->chans[i] = NULL;
	  sp->allocated_chans = chans;
	}
    }
  sp->index = snd_slot;
  sp->nchans = chans;
  sp->hdr = hdr;
  sp->inuse = 1;
  sp->fullname = copy_string(filename);
  sp->shortname = filename_without_home_directory(sp->fullname); /* a pointer into fullname, not a new string */
  sp->state = ss;
  sp->syncing = DEFAULT_SYNCING;
  sp->expand = DEFAULT_EXPAND;
  sp->last_expand = 0.0;
  sp->saved_expand = 0.0;
  sp->expanding = DEFAULT_EXPANDING;
  secs = (Float)hdr->samples/(Float)(hdr->chans*hdr->srate);
  if (secs < 1.0) 
    sp->sx_scroll_max = 100;
  else sp->sx_scroll_max = (int)pow(10,(ceil(log10(secs)) + 2));
  sp->amp = DEFAULT_AMP;
  sp->last_amp = 1.0;
  sp->saved_amp = 1.0;
  sp->srate = fabs(DEFAULT_SPEED);
  sp->last_srate = 1.0;
  sp->saved_srate = 1.0;
  if (DEFAULT_SPEED > 0.0) sp->play_direction = 1; else sp->play_direction = -1;
  sp->contrasting = DEFAULT_CONTRASTING;
  sp->contrast = DEFAULT_CONTRAST;
  sp->last_contrast = 0.0;
  sp->saved_contrast = 0.0;
  sp->reverbing = DEFAULT_REVERBING;
  sp->filtering = DEFAULT_FILTERING;
  sp->searching = 0;
  if (chans > 1)
    sp->combining = channel_style(ss);
  else sp->combining = CHANNELS_SEPARATE;
  sp->loading = 0;
  sp->evaling = 0;
  sp->marking = 0;
  sp->filing = 0;
  sp->read_only = 0;
  sp->minibuffer_on = 0;
  sp->minibuffer_temp = 0;
  sp->expand_length = DEFAULT_EXPAND_LENGTH;
  sp->expand_ramp = DEFAULT_EXPAND_RAMP;
  sp->expand_hop = DEFAULT_EXPAND_HOP;
  sp->revfb = DEFAULT_REVERB_FEEDBACK;
  sp->revlp = DEFAULT_REVERB_LOWPASS;
  sp->revscl = DEFAULT_REVERB_SCALE;
  sp->reverb_decay = reverb_decay(ss);
  sp->speed_tones = speed_tones(ss);
  sp->speed_style = speed_style(ss);
  sp->last_revscl = 0.0;
  sp->saved_revscl = 0.0;
  sp->revlen = DEFAULT_REVERB_LENGTH;
  sp->last_revlen = 0.0;
  sp->saved_revlen = 0.0;
  sp->filter_order = DEFAULT_FILTER_ORDER;
  sp->filter_dBing = DEFAULT_FILTER_DBING;
  sp->filter_changed = 0;
  if (filter_env_in_hz(ss))
    sp->filter_env_xmax = hdr->srate / 2;
  else sp->filter_env_xmax = 1.0;
  sp->selected_channel = NO_SELECTION;
  sp->playing = 0;
  sp->applying = 0;
  sp->lisp_graphing = 0;
  sp->saved_controls = NULL;
  sp->env_anew = 0;
  sp->contrast_amp = DEFAULT_CONTRAST_AMP;
  sp->fit_data_amps = NULL;
  sp->search_expr = NULL;
  sp->lacp = NULL;
#if HAVE_GUILE
  sp->search_proc = SCM_UNDEFINED;
  sp->eval_proc = SCM_UNDEFINED;
  sp->prompt_callback = SCM_UNDEFINED;
#endif
  sp->filter_env = default_env(sp->filter_env_xmax,1.0);
  sp->delete_me = 0;

  if (fit_data_on_open(ss)) 
    {
      sp->fit_data_amps = (Float *)CALLOC(sp->nchans,sizeof(Float));
      file_maxamps(sp->fullname,sp->fit_data_amps,hdr->chans,hdr->format);
      /* can't use snd-chn.c get_maxamp here because the file edit tree is not yet set up */
      /* can't use mus_sound_chans etc here because this might be a raw header file */
    }
  return(sp);
}

void free_snd_info(snd_info *sp)
{
  int i;
  /* leave most for reuse as in free_chan_info */
  if (sp->sgx)
    {
      if ((sp->sgx)->apply_in_progress) remove_apply(sp);
    }
  snd_info_cleanup(sp);
  snd_filter_cleanup(sp);
  sp->syncing = 0;
  for (i=0;i<sp->nchans;i++)
    {
      if (sp->chans[i]) sp->chans[i]=free_chan_info(sp->chans[i]);
    }
  sp->state = NULL;
  sp->inuse = 0;
  sp->amp = 1.0;
  sp->srate = 1.0;
  sp->expand = 0.0;
  sp->expanding = 0;
  sp->contrasting = 0;
  sp->reverbing = 0;
  sp->filtering = 0;
  sp->filter_changed = 0;
  sp->play_direction = 1;
  sp->playing_mark = NULL;
  sp->playing = 0;
  sp->searching = 0;
  sp->loading = 0;
  sp->evaling = 0;
  sp->marking = 0;
  sp->filing = 0;
  sp->applying = 0;
  sp->combining = CHANNELS_SEPARATE;
  sp->read_only = 0;
  sp->lisp_graphing = 0;
  sp->minibuffer_on = 0;   /* if it's on, should we clear it first ?? */
  sp->minibuffer_temp = 0;
  if (sp->search_expr) {free(sp->search_expr); sp->search_expr = NULL;}
  if (sp->eval_expr) {free(sp->eval_expr); sp->eval_expr = NULL;}
#if HAVE_GUILE
  if ((sp->search_proc) && (gh_procedure_p(sp->search_proc))) snd_unprotect(sp->search_proc);
  sp->search_proc = SCM_UNDEFINED;
  if ((sp->eval_proc) && (gh_procedure_p(sp->eval_proc))) snd_unprotect(sp->eval_proc);
  sp->eval_proc = SCM_UNDEFINED;
  if ((sp->prompt_callback) && (gh_procedure_p(sp->prompt_callback))) snd_unprotect(sp->prompt_callback);
  sp->prompt_callback = SCM_UNDEFINED;
#endif
  sp->selected_channel = NO_SELECTION;
  sp->shortname = NULL; /* was a pointer into fullname */
  if (sp->fullname) FREE(sp->fullname);
  sp->fullname = NULL;
  if (sp->filter_env) sp->filter_env = free_env(sp->filter_env);
  if (sp->saved_controls) free_controls(sp);
  sp->env_anew = 0;
  sp->contrast_amp = 1.0;
  sp->delete_me = 0;
  sp->lacp = NULL;
  if (sp->fit_data_amps)
    {
      FREE(sp->fit_data_amps);
      sp->fit_data_amps = NULL;
    }
  if (sp->hdr) sp->hdr = free_file_info(sp->hdr);
  if (sp->edited_region) clear_region_backpointer(sp);
  clear_mini_strings(sp);
  clear_filter_strings(sp);
}

snd_info *completely_free_snd_info(snd_info *sp)
{
  int i;
  chan_info *cp;
  free_snd_info(sp);
  if (sp->sgx) FREE(sp->sgx);
  for (i=0;i<sp->allocated_chans;i++) 
    {
      cp = sp->chans[i];
      if (cp)
	{
	  if (cp->cgx) 
	    {
	      if ((cp->cgx)->ax) 
		FREE((cp->cgx)->ax);
	      FREE(cp->cgx);
	    }
	  FREE(cp);
	}
    }
  FREE(sp->chans);
  FREE(sp);
  return(NULL);
}

int map_over_chans (snd_state *ss, int (*func)(chan_info *,void *), void *userptr)
{
  /* argument to func is chan_info pointer+void pointer of user spec, return non-zero = abort map, skips inactive sounds */
  int i,j,val;
  snd_info *sp;
  chan_info *cp;
  val = 0;
  if (ss)
    {
      for (i=0;i<ss->max_sounds;i++)
	{
	  if ((sp=((snd_info *)(ss->sounds[i]))))
	    {
	      if (sp->inuse)
		{
		  for (j=0;j<(sp->nchans);j++)
		    {
		      if ((cp=((chan_info *)(sp->chans[j]))))
			{
			  val = (*func)(cp,userptr);
			  if (val) return(val);}}}}}}
  return(val);
}

int map_over_sound_chans (snd_info *sp, int (*func)(chan_info *,void *), void *userptr)
{
  /* argument to func is chan_info pointer+void pointer of user spec, return non-zero = abort map, skips inactive sounds */
  int j,val;
  chan_info *cp;
  val = 0;
  for (j=0;j<(sp->nchans);j++)
    {
      if ((cp=sp->chans[j]))
	{
	  val = (*func)(cp,userptr);
	  if (val) return(val);
	}
    }
  return(val);
}

int map_over_sounds (snd_state *ss, int (*func)(snd_info *,void *), void *userptr)
{
  /* argument to func is snd_info pointer, return non-zero = abort map, skips inactive sounds */
  int i,val;
  snd_info *sp;
  val = 0;
  if (ss)
    {
      for (i=0;i<ss->max_sounds;i++)
	{
	  if ((sp=((snd_info *)(ss->sounds[i]))))
	    {
	      if (sp->inuse)
		{
		  val = (*func)(sp,userptr);
		  if (val) return(val);}}}}
  return(val);
}

int map_over_separate_chans(snd_state *ss, int (*func)(chan_info *,void *), void *userptr)
{
  int i,val;
  snd_info *sp;
  val = 0;
  if (ss)
    {
      for (i=0;i<ss->max_sounds;i++)
	{
	  if ((sp=((snd_info *)(ss->sounds[i]))))
	    {
	      if (sp->inuse)
		{
		  if (sp->combining != CHANNELS_SEPARATE)
		    val = (*func)(sp->chans[0],userptr);
		  else val = map_over_sound_chans(sp,func,userptr);
		  if (val) return(val);}}}}
  return(val);
}

int snd_ok (snd_info *sp) {return((sp) && (sp->inuse));}

int active_channels (snd_state *ss,int count_virtual_channels)
{
  int chans,i;
  snd_info *sp;
  chans = 0;
  for (i=0;i<ss->max_sounds;i++)
    {
      if (snd_ok(sp = (ss->sounds[i])))
	{
	  if ((count_virtual_channels == WITH_VIRTUAL_CHANNELS) || (sp->combining == CHANNELS_SEPARATE))
	    chans += sp->nchans;
	  else chans++;
	}
    }
  return(chans);
}

int find_free_sound_slot (snd_state *state, int desired_chans)
{
  int i,j;
  snd_info *sp;
  /* first try to find an unused slot that can accomodate desired_chans (to reduce Widget creation) */
  for (i=0;i<state->max_sounds;i++)
    {
      sp = state->sounds[i];
      if ((sp) && (sp->inuse == 0) && (sp->allocated_chans == desired_chans)) return(i);
    }
  for (i=0;i<state->max_sounds;i++)
    {
      sp = state->sounds[i];
      if ((sp) && (sp->inuse == 0) && (sp->allocated_chans > desired_chans)) return(i);
    }
  for (i=0;i<state->max_sounds;i++)
    {
      sp = state->sounds[i];
      if (sp == NULL) return(i);
      if (sp->inuse == 0) return(i);
    }
  /* need to REALLOC sounds to make space */
  j = state->max_sounds;
  state->max_sounds += 4;
  state->sounds = (snd_info **)REALLOC(state->sounds,state->max_sounds*sizeof(snd_info *));
  for (i=j;i<state->max_sounds;i++) state->sounds[i] = NULL;
  return(j);
}

static snd_info *any_active_sound(snd_state *ss)
{
  snd_info *sp;
  int i;
  for (i=0;i<ss->max_sounds;i++)
    {
      if ((sp=(ss->sounds[i])) && (sp->inuse)) return(sp);
    }
  return(NULL);
}

snd_info *selected_sound(snd_state *ss)
{
  if (ss->selected_sound != NO_SELECTION) return(ss->sounds[ss->selected_sound]);
  return(NULL);
}

snd_info *any_selected_sound (snd_state *ss)
{
  snd_info *sp;
  sp = selected_sound(ss);
  if (!sp) sp = any_active_sound(ss);
  return(sp);
}

chan_info *any_selected_channel(snd_info *sp)
{
  if (sp->selected_channel != NO_SELECTION) return(sp->chans[sp->selected_channel]);
  return(sp->chans[0]);
}

chan_info *selected_channel(snd_state *ss)
{
  snd_info *sp;
  if (ss->selected_sound != NO_SELECTION)
    {
      sp = ss->sounds[ss->selected_sound];
      if ((sp->inuse) && (sp->selected_channel != NO_SELECTION))
	return(sp->chans[sp->selected_channel]);
    }
  return(NULL);
}

static void select_sound (snd_state *ss, snd_info *sp)
{
  snd_info *osp = NULL;
  if (ss->selected_sound != sp->index)
    {
      if (!ss->using_schemes)
	{
	  if (ss->selected_sound != NO_SELECTION) osp = ss->sounds[ss->selected_sound];
	  if ((osp) && (sp != osp) && (osp->inuse)) 
	    {
	      highlight_color(ss,w_snd_name(osp));
#if ((USE_MOTIF) && (XmVERSION > 1))
	      if (sound_style(ss) == SOUNDS_IN_NOTEBOOK) 
		XmChangeColor((osp->sgx)->tab,(ss->sgx)->graph_color);
#endif
	    }
	  if (sp->selected_channel != NO_SELECTION) 
	    {
	      white_color(ss,w_snd_name(sp));
#if ((USE_MOTIF) && (XmVERSION > 1))
	      if (sound_style(ss) == SOUNDS_IN_NOTEBOOK) 
		XmChangeColor((sp->sgx)->tab,(ss->sgx)->selected_graph_color);
#endif
	    }
	}
      ss->selected_sound = sp->index;
      highlight_selected_sound(ss);
      reflect_undo_or_redo_in_menu(any_selected_channel(sp));
      new_active_channel_alert(ss);
    }
}

void select_channel(snd_info *sp, int chan)
{
  snd_state *ss = sp->state;
  chan_info *cp,*ncp;
  cp = selected_channel(ss);
  if (cp != sp->chans[chan])
    {
      sp->selected_channel = chan;
      select_sound(ss,sp);
      if (cp) 
	{
	  recolor_graph(cp,FALSE);
	  (cp->cgx)->selected = 0;
	  if (sp != cp->sound) (cp->sound)->selected_channel = NO_SELECTION;
	  update_graph(cp,NULL);
	}
      ncp = sp->chans[chan];
      reflect_undo_or_redo_in_menu(ncp);
      recolor_graph(ncp,TRUE);
      (ncp->cgx)->selected = 1;
      if ((ss->sgx)->data_color != (ss->sgx)->selected_data_color) 
	update_graph(ncp,NULL);
      /* else draw_graph_border(ncp); */
      goto_graph(ncp);
    }
}

chan_info *current_channel(snd_state *ss)
{
  snd_info *sp = NULL;
  if (!ss) return(NULL); /* can be null when Snd has only the menu bar ?? really?? */
  if (ss->selected_sound != NO_SELECTION)
    sp = ss->sounds[ss->selected_sound];
  else sp = any_active_sound(ss);
  if (sp) return(any_selected_channel(sp));
  return(NULL);
}

sync_info *free_sync_info (sync_info *si)
{
  if (si)
    {
      if (si->begs) FREE(si->begs);
      si->begs = NULL;
      if (si->cps) FREE(si->cps);
      si->cps = NULL;
      FREE(si);
    }
  return(NULL);
}

sync_info *snd_sync(snd_state *ss, int sync)
{
  int i,j,k,chans=0;
  snd_info *sp;
  sync_info *si;
  for (i=0;i<ss->max_sounds;i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse) && (sp->syncing == sync)) 
	chans += sp->nchans;
    }
  if (chans > 0)
    {
      si = (sync_info *)CALLOC(1,sizeof(sync_info));
      si->begs = (int *)CALLOC(chans,sizeof(int));
      si->cps = (chan_info **)CALLOC(chans,sizeof(chan_info *));
      si->chans = chans;
      j=0;
      for (i=0;i<ss->max_sounds;i++)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse) && (sp->syncing == sync))
	    for (k=0;k<sp->nchans;k++,j++)
	      si->cps[j] = sp->chans[k];
	}
      return(si);
    }
  return(NULL);
}

sync_info *make_simple_sync (chan_info *cp, int beg)
{
  sync_info *si;
  si = (sync_info *)CALLOC(1,sizeof(sync_info));
  si->chans = 1;
  si->cps = (chan_info **)CALLOC(1,sizeof(chan_info *));
  si->cps[0] = cp;
  si->begs = (int *)CALLOC(1,sizeof(int));
  si->begs[0] = beg;
  return(si);
}

sync_info *sync_to_chan(chan_info *cp)
{
  snd_info *sp;
  sp = cp->sound;
  if (sp->syncing)
    return(snd_sync(cp->state,sp->syncing));
  return(make_simple_sync(cp,0));
}

snd_info *find_sound(snd_state *ss, char *name)
{
  snd_info *sp;
  char *sname;
  int i;
  if (name == NULL) return(NULL);
  for (i=0;i<ss->max_sounds;i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse))
	if ((strcmp(name,sp->shortname) == 0) || (strcmp(name,sp->fullname) == 0)) 
	  return(sp);
    }
  sname = filename_without_home_directory(name);
  for (i=0;i<ss->max_sounds;i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse) && (strcmp(sname,sp->shortname) == 0))
	return(sp);
    }
  return(NULL);
}

static char timestr[TIME_STR_SIZE];
#define INFO_BUFFER_SIZE 1024

void display_info(snd_info *sp)
{
  char *buffer = NULL;
  file_info *hdr;
  char *comment,*cstr = NULL;
  if (sp)
    {
      hdr = sp->hdr;
      if (hdr)
	{
	  buffer = (char *)CALLOC(INFO_BUFFER_SIZE,sizeof(char));
	  cstr = mus_sound_comment(sp->fullname);
	  comment = cstr;
	  while ((comment) && (*comment) && 
		 (((*comment) == '\n') || 
		  ((*comment) == '\t') || 
		  ((*comment) == ' ') || 
		  ((*comment) == '\xd')))
	    comment++;
#if (!defined(HAVE_CONFIG_H)) || defined(HAVE_STRFTIME)
	  strftime(timestr,TIME_STR_SIZE,STRFTIME_FORMAT,localtime(&(sp->write_date)));
#endif
	  sprintf(buffer,"srate: %d\nchans: %d\nlength: %.3f (%d %s)\ntype: %s\nformat: %s\nwritten: %s\ncomment: %s\n",
		  hdr->srate,
		  hdr->chans,
		  (Float)(hdr->samples)/(Float)(hdr->chans * hdr->srate),
		  (hdr->samples)/(hdr->chans),
		  (hdr->chans == 1) ? "samples" : "frames",
		  mus_header_type_name(hdr->type),
		  mus_data_format_name(hdr->format),
		  timestr,
		  (comment) ? comment : "");
	  ssnd_help(sp->state,
		    sp->shortname,
		    buffer,
		    NULL);
	  if (cstr) FREE(cstr);
	  FREE(buffer);
	}
    }
}

static void file_maxamps(char *ifile, Float *vals, int ichans, int format)
{
  int ifd,idataloc,bufnum,n,cursamples,idatasize,loc,i,samples,chn;
  MUS_SAMPLE_TYPE fc;
  MUS_SAMPLE_TYPE *buffer,*amps;
  MUS_SAMPLE_TYPE **ibufs;
  if ((ifd=mus_file_open_read(ifile)) == -1) return;
  idataloc = mus_sound_data_location(ifile);
  mus_file_set_descriptors(ifd,
			   ifile,
			   format,
			   mus_data_format_to_bytes_per_sample(format),
			   idataloc,
			   ichans,
			   mus_sound_header_type(ifile));
  idatasize = mus_sound_samples(ifile);
  samples = (idatasize / ichans);
  if (samples <= 0) 
    {
      mus_file_close(ifd); 
      return;
    }
  loc=mus_file_seek(ifd,idataloc,SEEK_SET);
  if (loc<idataloc) 
    {
      mus_file_close(ifd); 
      return;
    }
  ibufs = (MUS_SAMPLE_TYPE **)CALLOC(ichans,sizeof(MUS_SAMPLE_TYPE *));
  for (i=0;i<ichans;i++) ibufs[i] = (MUS_SAMPLE_TYPE *)CALLOC(FILE_BUFFER_SIZE,sizeof(MUS_SAMPLE_TYPE));
  amps = (MUS_SAMPLE_TYPE *)CALLOC(ichans,sizeof(MUS_SAMPLE_TYPE));
  bufnum = (FILE_BUFFER_SIZE);
  for (n=0;n<samples;n+=bufnum)
    {
      if ((n+bufnum)<samples) cursamples = bufnum; else cursamples = (samples-n);
      mus_file_read(ifd,0,cursamples-1,ichans,ibufs);
      for (chn=0;chn<ichans;chn++)
	{
	  buffer = (MUS_SAMPLE_TYPE *)(ibufs[chn]);
	  fc=amps[chn];
	  for (i=0;i<cursamples;i++) 
	    {
	      if ((buffer[i] > fc) || (fc < -buffer[i])) 
		{
		  fc=buffer[i]; 
		  if (fc < MUS_SAMPLE_0) fc = -fc;
		}
	    }
	  amps[chn]=fc;
	}
    }
  mus_file_close(ifd);
  for (chn=0;chn<ichans;chn++) vals[chn] = MUS_SAMPLE_TO_FLOAT(amps[chn]);
  for (i=0;i<ichans;i++) FREE(ibufs[i]);
  FREE(ibufs);
  FREE(amps);
}

