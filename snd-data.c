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

chan_info *make_chan_info(chan_info *cip, int chan, snd_info *sound)
{
  chan_info *cp; /* may be re-used */
  if (!cip)
    {
      cp = (chan_info *)CALLOC(1, sizeof(chan_info)); 
      cp->cgx = (chan_context *)CALLOC(1, sizeof(chan_context));
      (cp->cgx)->ax = (axis_context *)CALLOC(1, sizeof(axis_context));
      cp->mixes = 0;
      cp->last_sonogram = NULL;
      cp->temp_sonogram = NULL;
#if HAVE_GL
      cp->gl_fft_list = NO_LIST;
#endif
      /* these hooks are never unprotected, so they might as well use the permanent protection mechanism */
      XEN_DEFINE_SIMPLE_HOOK(cp->edit_hook, 0);
      XEN_PROTECT_FROM_GC(cp->edit_hook);
      /* snd_protect(cp->edit_hook); */
      XEN_DEFINE_SIMPLE_HOOK(cp->after_edit_hook, 0);
      XEN_PROTECT_FROM_GC(cp->after_edit_hook);
      /* snd_protect(cp->after_edit_hook); */
      XEN_DEFINE_SIMPLE_HOOK(cp->undo_hook, 0);
      XEN_PROTECT_FROM_GC(cp->undo_hook);
      /* snd_protect(cp->undo_hook); */
      cp->properties = XEN_FALSE; /* will be a vector of 1 element (permanently protected) if it's ever used */
    }
  else cp = cip;
  cp->tcgx = NULL;
  cp->chan = chan;
  cp->sound = sound;
  cp->sound_ctr = -1;
  cp->edit_ctr = -1;
  cp->sound_size = 0;
  cp->ptrees = NULL;
  cp->ptree_size = 0;
  cp->ptree_ctr = -1;
  cp->edit_size = 0;
  cp->tracks = NULL;
  cp->cursor_on = false;
  cp->cursor_visible = false;
  cp->selection_visible = false;
  cp->cursor_style = cursor_style(ss);
  cp->cursor_size = cursor_size(ss);
  cp->cursor_proc = XEN_UNDEFINED;
  cp->squelch_update = false;
  cp->show_y_zero = show_y_zero(ss);
  cp->show_marks = show_marks(ss);
  cp->time_graph_type = time_graph_type(ss);
  cp->wavo_hop = wavo_hop(ss);
  cp->wavo_trace = wavo_trace(ss);
  cp->max_transform_peaks = max_transform_peaks(ss);
  cp->show_transform_peaks = show_transform_peaks(ss);
  cp->zero_pad = zero_pad(ss);
  cp->verbose_cursor = verbose_cursor(ss);
  cp->fft_log_frequency = fft_log_frequency(ss);
  cp->fft_log_magnitude = fft_log_magnitude(ss);
  cp->min_dB = ss->min_dB;
  cp->lin_dB = ss->lin_dB;
  cp->in_as_one_edit = false;
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
  cp->fft_window_beta = fft_window_beta(ss);
  cp->transform_size = transform_size(ss);
  cp->transform_graph_type = transform_graph_type(ss);
  cp->fft_window = fft_window(ss);
  cp->transform_type = transform_type(ss);
  cp->transform_normalization = transform_normalization(ss);
  cp->show_mix_waveforms = show_mix_waveforms(ss);
  cp->time_graph_style = graph_style(ss);
  cp->lisp_graph_style = graph_style(ss);
  cp->transform_graph_style = graph_style(ss);
  cp->graphs_horizontal = graphs_horizontal(ss);
  cp->dot_size = dot_size(ss);
  cp->x_axis_style = x_axis_style(ss);
  cp->beats_per_minute = beats_per_minute(ss);
  cp->show_axes = show_axes(ss);
  cp->graph_time_p = true; /* the default state (button is set when we start) */
  cp->graph_transform_p = false;
  cp->printing = false;
  cp->waiting_to_make_graph = false;
  cp->new_peaks = false;
  cp->amp_envs = NULL;
  cp->sonogram_data = NULL;
  cp->lisp_info = NULL;
  cp->amp_control = NULL;
  cp->hookable = WITH_HOOK;
  cp->gzy = 1.0;
  cp->gsy = 1.0;
  cp->cx = 0;
  cp->cy = 0;
  cp->selection_transform_size = 0;
  cp->last_search_result = SEARCH_OK;
  if (cp->last_sonogram) 
    {
      FREE(cp->last_sonogram); 
      cp->last_sonogram = NULL;
    }
  cp->active = true;
  return(cp);
}

static chan_info *free_chan_info(chan_info *cp)
{
  /* this does not free the associated widgets -- they are merely unmanaged */
  cp->active = false;
  /* need an indication right away that this channel is being deleted -- during free_snd_info (close-sound),
   *   an error may occur (an edit list temp file might have vanished for example), and normally Snd
   *   attempts to post an error message in the sound's minibuffer.  To force this out, we have to
   *   call XmUpdate or equivalent, which can cause all the sound's channels to attempt to redisplay;
   *   since the one causing the error is half-deallocated, trouble can ensue.  So both the channel
   *   and the sound have "active" flags that are true only when everything is ship-shape.
   */
  chan_info_cleanup(cp);
  cp->squelch_update = true;
  cp->tcgx = NULL;
  cp->axis = free_axis_info(cp->axis);
  if (cp->fft) cp->fft = free_fft_info(cp->fft);
  cp_free_fft_state(cp);
  cp->graph_transform_p = false;
  cp->printing = false;
  cp->graph_time_p = true;
  release_dangling_readers(cp, -1);
  if (cp->samples) {FREE(cp->samples); cp->samples = NULL;}
  if (cp->cursors) {FREE(cp->cursors); cp->cursors = NULL;}
  if (cp->tracks) free_track_info_list(cp); /* needs to precede free_edit_list which clobbers cp->edit_size */
  if (cp->edits) free_edit_list(cp);
  if (cp->sounds) free_sound_list(cp);
  if (cp->ptrees) free_ptree_list(cp);
  if (cp->enved_spectra) free_enved_spectra(cp);
  free_mark_list(cp, -1);
  free_mixes(cp);
  cp->sound = NULL;  /* a backpointer */
  cp->cursor_on = false;
  cp->cursor_visible = false;
  cp->selection_visible = false;
  if (cp->amp_control)
    {
      /* not sure this is the right thing */
      FREE(cp->amp_control);
      cp->amp_control = NULL;
    }
  if (XEN_PROCEDURE_P(cp->cursor_proc))
    {
      snd_unprotect(cp->cursor_proc);
      cp->cursor_proc = XEN_UNDEFINED;
    }
  if (XEN_VECTOR_P(cp->properties)) /* using vector as node for GC */
    XEN_VECTOR_SET(cp->properties, 0, XEN_EMPTY_LIST);
  cp->waiting_to_make_graph = false;
  if (cp->sonogram_data) free_sono_info(cp);
  if (cp->temp_sonogram) 
    {
      /* special case -- background fft process never got a chance to run */
      if (cp->temp_sonogram == cp->last_sonogram) cp->last_sonogram = NULL;
      free_sonogram_fft_state(cp->temp_sonogram);
      FREE(cp->temp_sonogram); 
      cp->temp_sonogram = NULL;
    } 
  if (cp->last_sonogram) 
    {
      free_sonogram_fft_state(cp->last_sonogram);
      FREE(cp->last_sonogram); 
      cp->last_sonogram = NULL;
    }
  if (cp->lisp_info) cp->lisp_info = free_lisp_info(cp);
  cp->graph_lisp_p = false;
  cp->selection_transform_size = 0;
  cp->edit_hook_checked = false;
  XEN_CLEAR_HOOK(cp->edit_hook);
  XEN_CLEAR_HOOK(cp->after_edit_hook);
  XEN_CLEAR_HOOK(cp->undo_hook);
  return(cp);  /* pointer is left for possible future re-use */
}

snd_info *make_basic_snd_info(int chans)
{
  snd_info *sp = NULL;
  sp = (snd_info *)CALLOC(1, sizeof(snd_info));
  sp->chans = (chan_info **)CALLOC(chans, sizeof(chan_info *));
  sp->allocated_chans = chans;
  sp->properties = XEN_FALSE; /* will be a vector of 1 element (permanently protected) if it's ever used */
  return(sp);
}

void initialize_control_panel(snd_info *sp)
{
  sp->expand_control = DEFAULT_EXPAND_CONTROL;
  sp->last_expand_control = 0.0;
  sp->saved_expand_control = 0.0;
  sp->expand_control_p = DEFAULT_EXPAND_CONTROL_P;
  sp->amp_control = DEFAULT_AMP_CONTROL;
  sp->last_amp_control = 1.0;
  sp->saved_amp_control = 1.0;
  sp->speed_control = fabs(DEFAULT_SPEED_CONTROL);
  sp->last_speed_control = 1.0;
  sp->saved_speed_control = 1.0;
  if (DEFAULT_SPEED_CONTROL > 0.0) sp->speed_control_direction = 1; else sp->speed_control_direction = -1;
  sp->contrast_control_p = DEFAULT_CONTRAST_CONTROL_P;
  sp->contrast_control = DEFAULT_CONTRAST_CONTROL;
  sp->last_contrast_control = 0.0;
  sp->saved_contrast_control = 0.0;
  sp->reverb_control_p = DEFAULT_REVERB_CONTROL_P;
  sp->filter_control_p = DEFAULT_FILTER_CONTROL_P;
  sp->expand_control_length = DEFAULT_EXPAND_CONTROL_LENGTH;
  sp->expand_control_ramp = DEFAULT_EXPAND_CONTROL_RAMP;
  sp->expand_control_hop = DEFAULT_EXPAND_CONTROL_HOP;
  sp->reverb_control_feedback = DEFAULT_REVERB_CONTROL_FEEDBACK;
  sp->reverb_control_lowpass = DEFAULT_REVERB_CONTROL_LOWPASS;
  sp->reverb_control_scale = DEFAULT_REVERB_CONTROL_SCALE;
  sp->reverb_control_decay = reverb_control_decay(ss);
  sp->speed_control_tones = speed_control_tones(ss);
  sp->speed_control_style = speed_control_style(ss);
  sp->speed_control_numerator = 1;
  sp->speed_control_denominator = 1;
  sp->last_reverb_control_scale = 0.0;
  sp->saved_reverb_control_scale = 0.0;
  sp->reverb_control_length = DEFAULT_REVERB_CONTROL_LENGTH;
  sp->last_reverb_control_length = 0.0;
  sp->saved_reverb_control_length = 0.0;
  sp->filter_control_order = DEFAULT_FILTER_CONTROL_ORDER;
  sp->filter_control_in_dB = DEFAULT_FILTER_CONTROL_IN_DB;
  sp->filter_control_in_hz = DEFAULT_FILTER_CONTROL_IN_HZ;
  if (sp->filter_control_in_hz)
    sp->filter_control_xmax = (Float)(SND_SRATE(sp) / 2);
  else sp->filter_control_xmax = 1.0;
  sp->filter_control_changed = false;
  sp->contrast_control_amp = DEFAULT_CONTRAST_CONTROL_AMP;
  sp->saved_controls = NULL;
}

snd_info *make_snd_info(snd_info *sip, const char *filename, file_info *hdr, int snd_slot, bool read_only)
{
  snd_info *sp = NULL;
  int chans, i;
  /* assume file has been found and header read before reaching us */
  /* if a reused pointer, may need to extend current chans array */
  chans = hdr->chans;
  if (!sip)
    {
      sp = make_basic_snd_info(chans);
      sp->sgx = (snd_context *)CALLOC(1, sizeof(snd_context));
    }
  else 
    {
      sp = sip;
      if (sp->allocated_chans < chans) 
	{
	  sp->chans = (chan_info **)REALLOC(sp->chans, chans * sizeof(chan_info *));
	  for (i = sp->allocated_chans; i < chans; i++) sp->chans[i] = NULL;
	  sp->allocated_chans = chans;
	}
    }
  sp->read_only = read_only;  /* need to be sure this is set before any hooks run */
  sp->index = snd_slot;
  sp->nchans = chans;
  sp->hdr = hdr;
  sp->inuse = SOUND_NORMAL;
  sp->filename = copy_string(filename);
  sp->short_filename = filename_without_home_directory(sp->filename); /* a pointer into filename, not a new string */
  sp->sync = DEFAULT_SYNC;
  sp->previous_sync = sp->sync;
  initialize_control_panel(sp);
  sp->searching = 0;
  if (chans > 1)
    sp->channel_style = channel_style(ss);
  else sp->channel_style = CHANNELS_SEPARATE;
  sp->loading = false;
  sp->marking = 0;
  sp->filing = NOT_FILING;
  sp->minibuffer_on = MINI_OFF;
  sp->selected_channel = NO_SELECTION;
  sp->playing = 0;
  sp->applying = false;
  sp->search_expr = NULL;
  sp->lacp = NULL;
  sp->search_tree = NULL;
  sp->search_proc = XEN_UNDEFINED;
  sp->prompt_callback = XEN_UNDEFINED;
  sp->delete_me = NULL;
  sp->name_string = NULL;
  sp->active = true;
  sp->cursor_follows_play = DONT_FOLLOW;
  return(sp);
}

void free_snd_info(snd_info *sp)
{
  int i;
  if (sp->sgx)
    {
      /* make sure trough colors are ok upon reuse */
      if (sp->reverb_control_p != DEFAULT_REVERB_CONTROL_P)
	toggle_reverb_button(sp, DEFAULT_REVERB_CONTROL_P);
      if (sp->expand_control_p != DEFAULT_EXPAND_CONTROL_P)
	toggle_expand_button(sp, DEFAULT_EXPAND_CONTROL_P);
      if (sp->contrast_control_p != DEFAULT_CONTRAST_CONTROL_P)
	toggle_contrast_button(sp, DEFAULT_CONTRAST_CONTROL_P);
    }
  /* leave most for reuse as in free_chan_info */
  sp->active = false;
  if (sp->sgx)
    {
      env_editor *edp;
      if ((sp->sgx)->apply_in_progress) remove_apply(sp);
      edp = (env_editor *)(sp->sgx->flt);
      if (edp)
	{
	  edp->edited = false;
	  edp->env_dragged = false;
	  edp->env_pos = 0;
	  edp->click_to_delete = false;
	}
      set_filter_text(sp, "");
    }
  sp->sync = 0;
  snd_info_cleanup(sp);
  sp->previous_sync = sp->sync;
  for (i = 0; i < sp->nchans; i++)
    if (sp->chans[i]) 
      sp->chans[i] = free_chan_info(sp->chans[i]);
  sp->inuse = SOUND_IDLE;
  sp->playing_mark = NULL;
  sp->playing = 0;
  sp->searching = 0;
  sp->loading = false;
  sp->marking = 0;
  sp->filing = NOT_FILING;
  sp->applying = false;
  sp->channel_style = CHANNELS_SEPARATE;
  sp->read_only = false;
  sp->minibuffer_on = MINI_OFF;                     /* if it's on, should we clear it first ?? */
  if (sp->search_expr) 
    {
      FREE(sp->search_expr); 
      sp->search_expr = NULL;
    }
  if (sp->search_tree)
    free_ptree(sp->search_tree);
  sp->search_tree = NULL;
  if (XEN_PROCEDURE_P(sp->search_proc))
    snd_unprotect(sp->search_proc);
  sp->search_proc = XEN_UNDEFINED;
  if (XEN_PROCEDURE_P(sp->prompt_callback))
    snd_unprotect(sp->prompt_callback);
  sp->prompt_callback = XEN_UNDEFINED;
  if (XEN_VECTOR_P(sp->properties)) /* using vector as node for GC */
    XEN_VECTOR_SET(sp->properties, 0, XEN_EMPTY_LIST);
  sp->selected_channel = NO_SELECTION;
  sp->short_filename = NULL;                      /* was a pointer into filename */
  if (sp->filename) FREE(sp->filename);
  sp->filename = NULL;
  if (sp->filter_control_envelope) sp->filter_control_envelope = free_env(sp->filter_control_envelope);
  if (sp->saved_controls) free_controls(sp);
  sp->delete_me = NULL;
  if (sp->name_string) FREE(sp->name_string);
  sp->name_string = NULL;
  sp->lacp = NULL;
  if (sp->hdr) sp->hdr = free_file_info(sp->hdr);
  if (sp->edited_region) clear_region_backpointer(sp);
  clear_mini_strings(sp);
  clear_filter_strings(sp);
  clear_players();
  reflect_mix_or_track_change(ANY_MIX_ID, ANY_TRACK_ID, false);
}

snd_info *completely_free_snd_info(snd_info *sp)
{
  int i;
  chan_info *cp;
  free_snd_info(sp);
  if (sp->sgx) FREE(sp->sgx);
  for (i = 0; i < sp->allocated_chans; i++) 
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
	  snd_unprotect(cp->edit_hook);
	  snd_unprotect(cp->after_edit_hook);
	  snd_unprotect(cp->undo_hook);
	  if (XEN_VECTOR_P(cp->properties))
	    snd_unprotect(cp->properties);
	  FREE(cp);
	}
    }
  FREE(sp->chans);
  if (XEN_VECTOR_P(sp->properties))
    snd_unprotect(sp->properties);
  FREE(sp);
  return(NULL);
}

bool map_over_chans(bool (*func)(chan_info *, void *), void *userptr)
{
  /* argument to func is chan_info pointer+void pointer of user spec, return non-zero = abort map, skips inactive sounds */
  int i, j;
  bool val;
  snd_info *sp;
  chan_info *cp;
  val = false;
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

void for_each_chan_1(void (*func)(chan_info *, void *), void *userptr)
{
  int i, j;
  snd_info *sp;
  chan_info *cp;
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	for (j = 0; j < sp->nchans; j++)
	  if ((cp = ((chan_info *)(sp->chans[j]))))
	    (*func)(cp, userptr);
    }
}

void for_each_chan(void (*func)(chan_info *))
{
  int i, j;
  snd_info *sp;
  chan_info *cp;
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	for (j = 0; j < sp->nchans; j++)
	  if ((cp = ((chan_info *)(sp->chans[j]))))
	    (*func)(cp);
    }
}

bool map_over_sound_chans(snd_info *sp, bool (*func)(chan_info *, void *), void *userptr)
{
  /* argument to func is chan_info pointer+void pointer of user spec, return non-zero = abort map, skips inactive sounds */
  int j;
  bool val;
  chan_info *cp;
  val = false;
  for (j = 0; j < sp->nchans; j++)
    if ((cp = sp->chans[j]))
      {
	val = (*func)(cp, userptr);
	if (val) return(val);
      }
  return(val);
}

void for_each_sound_chan(snd_info *sp, void (*func)(chan_info *))
{
  int j;
  chan_info *cp;
  for (j = 0; j < sp->nchans; j++)
    if ((cp = sp->chans[j]))
      (*func)(cp);
}

bool map_over_sounds(bool (*func)(snd_info *, void *), void *userptr)
{
  /* argument to func is snd_info pointer, return true = abort map, skips inactive sounds */
  int i;
  bool val;
  snd_info *sp;
  val = false;
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	{
	  val = (*func)(sp, userptr);
	  if (val) return(val);
	}
    }
  return(val);
}

void for_each_sound(void (*func)(snd_info *, void *), void *userptr)
{
  int i;
  snd_info *sp;
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	(*func)(sp, userptr);
    }
}

bool map_over_separate_chans(bool (*func)(chan_info *, void *), void *userptr)
{
  int i;
  bool val;
  snd_info *sp;
  val = false;
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	{
	  if (sp->channel_style != CHANNELS_SEPARATE)
	    val = (*func)(sp->chans[0], userptr);
	  else val = map_over_sound_chans(sp, func, userptr);
	  if (val) return(val);
	}
    }
  return(val);
}

bool snd_ok(snd_info *sp) 
{
  return((sp) && 
	 (sp->inuse != SOUND_IDLE) && 
	 (sp->active));
}

int active_channels(virtual_channels_t count_virtual_channels)
{
  int chans, i;
  snd_info *sp;
  chans = 0;
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if (snd_ok(sp) && 
	  (sp->inuse != SOUND_WRAPPER))
	{
	  if ((count_virtual_channels == WITH_VIRTUAL_CHANNELS) ||
	      (sp->channel_style == CHANNELS_SEPARATE))
	    chans += sp->nchans;
	  else chans++;
	}
    }
  return(chans);
}

#define SOUNDS_ALLOC_SIZE 4

int find_free_sound_slot(int desired_chans)
{
  int i, j;
  snd_info *sp;
  /* first try to find an unused slot that can accommodate desired_chans (to reduce Widget creation) */
  if (ss->reloading_updated_file > 0)
    {
      /* snd_update should change the underlying slot only when it has to (user increased chans) */
      sp = ss->sounds[ss->reloading_updated_file - 1];
      if ((sp == NULL) ||
	  ((sp->inuse == SOUND_IDLE) && (sp->allocated_chans >= desired_chans)))
	return(ss->reloading_updated_file - 1);
    }
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_IDLE) && (sp->allocated_chans == desired_chans)) return(i);
    }
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_IDLE) && (sp->allocated_chans > desired_chans)) return(i);
    }
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if (sp == NULL) return(i);
      if (sp->inuse == SOUND_IDLE) return(i);
    }
  j = ss->max_sounds;
  ss->max_sounds += SOUNDS_ALLOC_SIZE;
  ss->sounds = (snd_info **)REALLOC(ss->sounds, ss->max_sounds * sizeof(snd_info *));
  for (i = j; i < ss->max_sounds; i++) ss->sounds[i] = NULL;
  return(j);
}

int find_free_sound_slot_for_channel_display(void)
{
  int i, j;
  for (i = 0; i < ss->max_sounds; i++)
    if (ss->sounds[i] == NULL) return(i);
  j = ss->max_sounds;
  ss->max_sounds += SOUNDS_ALLOC_SIZE;
  ss->sounds = (snd_info **)REALLOC(ss->sounds, ss->max_sounds * sizeof(snd_info *));
  for (i = j; i < ss->max_sounds; i++) ss->sounds[i] = NULL;
  return(j);
}

static snd_info *any_active_sound(void)
{
  snd_info *sp;
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	return(sp);
    }
  return(NULL);
}

snd_info *selected_sound(void)
{
  if (ss->selected_sound != NO_SELECTION)
    return(ss->sounds[ss->selected_sound]);
  return(NULL);
}

snd_info *any_selected_sound (void)
{
  snd_info *sp;
  sp = selected_sound();
  if (!sp) sp = any_active_sound();
  return(sp);
}

chan_info *any_selected_channel(snd_info *sp)
{
  if (sp->selected_channel != NO_SELECTION) 
    return(sp->chans[sp->selected_channel]);
  return(sp->chans[0]);
}

chan_info *selected_channel(void)
{
  snd_info *sp;
  if (ss->selected_sound != NO_SELECTION)
    {
      sp = ss->sounds[ss->selected_sound];
      if ((sp->inuse == SOUND_NORMAL) && (sp->selected_channel != NO_SELECTION))
	return(sp->chans[sp->selected_channel]);
    }
  return(NULL);
}

static XEN select_sound_hook;
static XEN select_channel_hook;

static void select_sound(snd_info *sp)
{
  if ((sp == NULL) || (sp->inuse != SOUND_NORMAL)) return;
  if (XEN_HOOKED(select_sound_hook))
    run_hook(select_sound_hook,
	     XEN_LIST_1(C_TO_XEN_INT(sp->index)),
	     S_select_sound_hook);
  if (ss->selected_sound != sp->index)
    {
#if USE_MOTIF
      if (!ss->using_schemes)
	{
	  snd_info *osp = NULL;
	  if (ss->selected_sound != NO_SELECTION) osp = ss->sounds[ss->selected_sound];
	  if ((osp) && (sp != osp) && (osp->inuse == SOUND_NORMAL)) 
	    {
	      XmChangeColor(w_snd_name(osp), (ss->sgx)->highlight_color);
#if (XmVERSION > 1)
	      if (sound_style(ss) == SOUNDS_IN_NOTEBOOK) 
		XmChangeColor((osp->sgx)->tab, (ss->sgx)->graph_color);
#endif
	    }
	  if (sp->selected_channel != NO_SELECTION) 
	    {
	      XmChangeColor(w_snd_name(sp), (ss->sgx)->white);
#if (XmVERSION > 1)
	      if (sound_style(ss) == SOUNDS_IN_NOTEBOOK) 
		{
		  int page, current_page;
		  XmNotebookPageStatus status;
		  XmNotebookPageInfo info;
		  XmChangeColor((sp->sgx)->tab, (ss->sgx)->selected_graph_color);
		  XtVaGetValues(SOUND_PANE(ss), XmNcurrentPageNumber, &current_page, NULL);
		  XtVaGetValues(sp->sgx->tab, XmNpageNumber, &page, NULL);
		  if (page != current_page)
		    {
		      status = XmNotebookGetPageInfo(SOUND_PANE(ss), page, &info);
		      if (status == XmPAGE_FOUND)
			{
			  XtVaSetValues(SOUND_PANE(ss), XmNcurrentPageNumber, page, NULL);
			  if (sp->nchans > 1)
			    equalize_sound_panes(sp, sp->chans[0], false);
			}
		    }
		}
#endif
	    }
	}
#endif
#if USE_GTK
      {
	snd_info *osp = NULL;
	if (ss->selected_sound != NO_SELECTION) osp = ss->sounds[ss->selected_sound];
	if ((osp) && (sp != osp) && (osp->inuse == SOUND_NORMAL)) 
	  gtk_widget_modify_fg(w_snd_name(osp), GTK_STATE_NORMAL, ss->sgx->black);
	if (sp->selected_channel != NO_SELECTION) 
	  gtk_widget_modify_fg(w_snd_name(sp), GTK_STATE_NORMAL, ss->sgx->red);
	if (sound_style(ss) == SOUNDS_IN_NOTEBOOK) 
	  {
	    int page, current_page;
	    current_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(SOUND_PANE_BOX(ss)));
	    page = sp->sgx->page;
	    if ((page != current_page) && (current_page >= 0))
	      {
		ss->selected_sound = sp->index; /* break infinite recursion here */
		gtk_notebook_set_current_page(GTK_NOTEBOOK(SOUND_PANE_BOX(ss)), page);
	      }
	  }
      }
#endif
      ss->selected_sound = sp->index;
      highlight_selected_sound();
      reflect_undo_or_redo_in_menu(any_selected_channel(sp));
      new_active_channel_alert();
    }
}

chan_info *color_selected_channel(snd_info *sp)
{
  chan_info *ncp;
  ncp = sp->chans[sp->selected_channel];
  recolor_graph(ncp, true);
  (ncp->cgx)->selected = true;
  if ((ss->sgx)->data_color != (ss->sgx)->selected_data_color) 
    update_graph(ncp);
  return(ncp);
}

void select_channel(snd_info *sp, int chan)
{
  chan_info *cp, *ncp;
  if ((sp == NULL) || (sp->inuse != SOUND_NORMAL)) return;
  cp = selected_channel();
  if (cp != sp->chans[chan])
    {
      sp->selected_channel = chan;
      select_sound(sp);
      if (cp) 
	{
	  recolor_graph(cp, false);
	  (cp->cgx)->selected = false;
	  if (sp != cp->sound) (cp->sound)->selected_channel = NO_SELECTION;
	  update_graph(cp);
	}
      if (XEN_HOOKED(select_channel_hook))
	run_hook(select_channel_hook,
		 XEN_LIST_2(C_TO_XEN_INT(sp->index),
			    C_TO_XEN_INT(chan)),
		 S_select_channel_hook);
      ncp = color_selected_channel(sp);
      reflect_undo_or_redo_in_menu(ncp);
      goto_graph(ncp);
    }
}

chan_info *current_channel(void)
{
  snd_info *sp = NULL;
  if (ss->selected_sound != NO_SELECTION)
    sp = ss->sounds[ss->selected_sound];
  else sp = any_active_sound();
  if (sp) return(any_selected_channel(sp));
  return(NULL);
}

sync_info *free_sync_info(sync_info *si)
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

sync_info *snd_sync(int sync)
{
  int i, j, k, chans = 0;
  snd_info *sp;
  sync_info *si;
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL) && (sp->sync == sync)) 
	chans += sp->nchans;
    }
  if (chans > 0)
    {
      si = (sync_info *)CALLOC(1, sizeof(sync_info));
      si->begs = (off_t *)CALLOC(chans, sizeof(off_t));
      si->cps = (chan_info **)CALLOC(chans, sizeof(chan_info *));
      si->chans = chans;
      j = 0;
      for (i = 0; i < ss->max_sounds; i++)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse == SOUND_NORMAL) && (sp->sync == sync))
	    for (k = 0; k < sp->nchans; k++, j++)
	      si->cps[j] = sp->chans[k];
	}
      return(si);
    }
  return(NULL);
}

sync_info *make_simple_sync(chan_info *cp, off_t beg)
{
  sync_info *si;
  si = (sync_info *)CALLOC(1, sizeof(sync_info));
  si->chans = 1;
  si->cps = (chan_info **)CALLOC(1, sizeof(chan_info *));
  si->cps[0] = cp;
  si->begs = (off_t *)CALLOC(1, sizeof(off_t));
  si->begs[0] = beg;
  return(si);
}

sync_info *sync_to_chan(chan_info *cp)
{
  snd_info *sp;
  sp = cp->sound;
  if (sp->sync)
    return(snd_sync(sp->sync));
  return(make_simple_sync(cp, 0));
}

snd_info *find_sound(const char *name, int nth)
{
  snd_info *sp;
  char *sname;
  int i, which = 0;
  if (name == NULL) return(NULL);
  sname = filename_without_home_directory(name);
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	if ((strcmp(name, sp->short_filename) == 0) || 
	    (strcmp(name, sp->filename) == 0) ||
	    ((sname != NULL) && (strcmp(sname, sp->short_filename) == 0)))
	  {
	    if (which == nth) return(sp);
	    which++;
	  }
    }
  return(NULL);
}

static char *display_maxamps(const char *filename, int chans)
{
  char *ampstr;
  int i, len;
  mus_sample_t *vals;
  off_t *times;
  len = chans * 32;
  ampstr = (char *)CALLOC(len, sizeof(char));
  vals = (mus_sample_t *)CALLOC(chans, sizeof(mus_sample_t));
  times = (off_t *)CALLOC(chans, sizeof(off_t));
  sprintf(ampstr, _("\nmax amp: "));
  mus_sound_maxamps(filename, chans, vals, times);
  for (i = 0; i < chans; i++)
    {
      ampstr = snd_strcat(ampstr, prettyf(MUS_SAMPLE_TO_FLOAT(vals[i]), 3), &len);
      ampstr = snd_strcat(ampstr, " ", &len);
    }
  FREE(vals);
  FREE(times);
  return(ampstr);
}

static char timestr[TIME_STR_SIZE];
#define INFO_BUFFER_SIZE 1024

void display_info(snd_info *sp)
{
  char *buffer = NULL;
  file_info *hdr;
  char *comment = NULL, *ampstr = NULL;
  if (sp)
    {
      hdr = sp->hdr;
      if (hdr)
	{
	  comment = hdr->comment;
	  while ((comment) && (*comment) && 
		 (((*comment) == '\n') || 
		  ((*comment) == '\t') || 
		  ((*comment) == ' ') || 
		  ((*comment) == '\xd')))
	    comment++;
	  if (mus_sound_maxamp_exists(sp->filename))
	    ampstr = display_maxamps(sp->filename, sp->nchans);
	  buffer = (char *)CALLOC(INFO_BUFFER_SIZE, sizeof(char));
#if HAVE_STRFTIME
	  strftime(timestr, TIME_STR_SIZE, STRFTIME_FORMAT, localtime(&(sp->write_date)));
#endif
	  mus_snprintf(buffer, INFO_BUFFER_SIZE, 
		       _("srate: %d\nchans: %d\nlength: %.3f (" PRId64 " %s)\ntype: %s\nformat: %s\nwritten: %s%s%s%s\n"),
		       hdr->srate,
		       hdr->chans,
		       (Float)((double)(hdr->samples) / (Float)(hdr->chans * hdr->srate)),
		       (off_t)((hdr->samples) / (hdr->chans)),
		       (hdr->chans == 1) ? _("samples") : _("frames"),
		       mus_header_type_name(hdr->type),
		       mus_data_format_name(hdr->format),
		       timestr,
		       (ampstr) ? ampstr : "",
		       (comment) ? _("\ncomment: ") : "",
		       (comment) ? comment : "");
	  post_it(sp->short_filename, buffer);
	  if (ampstr) FREE(ampstr);
	  FREE(buffer);
	}
    }
}

#if DEBUGGING && HAVE_GUILE
static XEN g_display_info(void)
{
  display_info(selected_sound());
  return(XEN_FALSE);
}
#endif

void g_init_data(void)
{
  #define H_select_sound_hook S_select_sound_hook " (snd): called whenever a sound is selected."

  #define H_select_channel_hook S_select_channel_hook "(snd chn): called whenever a channel is selected. \
Its arguments are the sound index and the channel number."

  XEN_DEFINE_HOOK(select_sound_hook, S_select_sound_hook, 1, H_select_sound_hook);       /* arg = sound index */
  XEN_DEFINE_HOOK(select_channel_hook, S_select_channel_hook, 2, H_select_channel_hook); /* args = sound index, channel */

#if DEBUGGING && HAVE_GUILE
  XEN_DEFINE_PROCEDURE("popup-display-info", g_display_info, 0, 0, 0, "internal test function");
#endif
}

