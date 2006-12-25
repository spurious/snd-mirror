#include "snd.h"

/*
 * each channel currently being played has an associated dac_info struct
 *   all active dac_info structs are held in a play_list
 *   channels can come and go as a play is in progress
 */

/* -------------------------------- per-channel control-panel state -------------------------------- */

typedef struct {
  mus_any *gen;
  struct dac_info *dp;
  bool speeding;
  Float sr;
} spd_info;

typedef struct dac_info {
  Float cur_index;
  Float cur_amp;
  Float cur_srate;
  Float cur_exp;
  Float cur_rev;       /* rev scaler -- len is set at initialization */
  Float contrast_amp;
  bool expanding, reverbing, filtering; /* these need lots of preparation, so they're noticed only at the start */
  int audio_chan;      /* where channel's output is going (wrap-around if not enough audio output channels) */
  int slot;
  Float *a;            /* filter coeffs */
  int a_size;          /* user can change filter order while playing (sigh...) */
  snd_fd *chn_fd;      /* sample reader */
  spd_info *spd;
  mus_any *flt;
  int region;          /* to reset region-browser play button upon completion */
  bool selection;
  src_state *src;
  snd_info *sp;        /* needed to see button callback changes etc */
  chan_info *cp;
  bool never_sped;
  int expand_ring_frames;
  off_t end;
  XEN stop_procedure;
  int stop_procedure_gc_loc;
} dac_info;

#define AMP_CONTROL(sp, dp) ((dp->cp->amp_control) ? (dp->cp->amp_control[0]) : sp->amp_control)
/* an experiment */


/* -------- filter -------- */

static mus_any *make_flt(dac_info *dp, int order, Float *env)
{
  if (order <= 0) return(NULL);
  dp->a_size = order;
  dp->a = (Float *)CALLOC(order, sizeof(Float));
  if (env) mus_make_fir_coeffs(order, env, dp->a);
  return(mus_make_fir_filter(order, dp->a, NULL));
}


/* -------- sample-rate conversion -------- */
static Float speed(dac_info *dp, Float sr)
{
  if (dp->never_sped)
    return(read_sample_to_float(dp->chn_fd));
  if (dp->src == NULL)
    dp->src = make_src(0.0, dp->chn_fd, sr);
  return(mus_src(dp->src->gen, sr, &src_input_as_needed));
}

/* -------- granular synthesis -------- */
static Float expand_input_as_needed(void *arg, int dir) 
{
  spd_info *spd = (spd_info *)arg;
  dac_info *dp;
  dp = spd->dp;
  if (spd->speeding)
    return(speed(dp, spd->sr));
  else return(read_sample_to_float(dp->chn_fd));
}

static int max_expand_control_len(snd_info *sp)
{
  if (sp->expand_control_length > .5)
    return(0);
  return((int)(SND_SRATE(sp) * .5));
}

static void *make_expand(snd_info *sp, Float initial_ex, dac_info *dp)
{
  spd_info *spd;
  spd = (spd_info *)CALLOC(1, sizeof(spd_info));
  spd->gen = mus_make_granulate(&expand_input_as_needed,
				initial_ex, sp->expand_control_length,
				.6, /* expand scaler, not currently settable -- dac_set_expand_scaler below */
				sp->expand_control_hop, sp->expand_control_ramp, 
				sp->expand_control_jitter,
				max_expand_control_len(sp), NULL, (void *)spd);
  spd->dp = dp;
  spd->speeding = false;
  spd->sr = 0.0;
  return(spd);
}

static void free_expand(void *ur_spd)
{
  spd_info *spd = (spd_info *)ur_spd;
  if (ur_spd)
    {
      mus_free(spd->gen);
      FREE(spd);
    }
}

static Float expand(dac_info *dp, Float sr, Float ex)
{
  /* from mixer.sai, used in "Leviathan", 1986 */
  bool speeding;
  snd_info *sp;
  spd_info *spd;
  sp = dp->sp;
  speeding = ((sp->speed_control_direction != 1) || (!(snd_feq(sp->speed_control, 1.0))) || (!(snd_feq(dp->cur_srate, 1.0))));
  spd = dp->spd;
  spd->speeding = speeding;
  spd->sr = sr;
  return(mus_granulate(spd->gen, &expand_input_as_needed));
}


/* -------- reverb -------- */

/* to implement run-time reverb-length, we would need to save the individual delay lens, both nominal
   and actual (given srate); set up the combs/allpasses to make room for 5.0 as reverb_control_length,
   then at each call, check current reverb len (how?), and if not original, recalculate how many
   samples each delay needs to be expanded by and so on -- more complexity than it's worth!
*/

static int prime(int num)
{
  if (num == 2) return(1);
  if ((num % 2) == 1)
    {
      int lim, i;
      lim = (int)(sqrt(num));
      for (i = 3; i < lim; i += 2)
	if ((num % i) == 0) 
	  return(0);
      return(1);
    }
  return(0);
}

static int get_prime(int num)
{
  int i;
  if ((num % 2) == 1)
    i = num;
  else i = num + 1;
  while (!(prime(i))) i += 2;
  return(i);
}

typedef struct {
  int num_combs;
  mus_any **combs;
  int num_allpasses;
  mus_any **allpasses;
  mus_any *onep;
} rev_info;

static rev_info *global_rev = NULL;

static void nrev(rev_info *r, Float *rins, Float *routs, int chans)
{
  Float rout, rin = 0.0;
  int i;
  for (i = 0; i < chans; i++) rin += rins[i];
  rout = mus_all_pass(r->allpasses[3],
	   mus_one_pole(r->onep,
	     mus_all_pass(r->allpasses[2],
	       mus_all_pass(r->allpasses[1],
		 mus_all_pass(r->allpasses[0],
	           mus_comb(r->combs[0], rin, 0.0) + 
	           mus_comb(r->combs[1], rin, 0.0) + 
	           mus_comb(r->combs[2], rin, 0.0) + 
	           mus_comb(r->combs[3], rin, 0.0) + 
	           mus_comb(r->combs[4], rin, 0.0) + 
	           mus_comb(r->combs[5], rin, 0.0), 0.0), 0.0), 0.0)), 0.0);
  for (i = 0; i < chans; i++)
    routs[i] = mus_all_pass(r->allpasses[i + 4], rout, 0.0);
}

static Float *r_ins, *r_outs;
static int reverb_chans = 0;

static void reverb(rev_info *r, Float **rins, mus_sample_t **outs, int ind)
{
  int i, chans;
  chans = reverb_chans;
  for (i = 0; i < chans; i++)
    {
      r_ins[i] = rins[i][ind];
      r_outs[i] = 0.0;
    }
  nrev(r, r_ins, r_outs, chans); 
  for (i = 0; i < chans; i++)
    outs[i][ind] += MUS_FLOAT_TO_SAMPLE(r_outs[i]);
}

static void free_reverb(void) 
{
  rev_info *r;
  r = global_rev;
  if (r)
    {
      int i;
      if (r->combs)
	{
	  for (i = 0; i < r->num_combs; i++) 
	    if (r->combs[i]) 
	      mus_free(r->combs[i]);
	  FREE(r->combs);
	}
      if (r->onep) mus_free(r->onep);
      if (r->allpasses)
	{
	  for (i = 0; i < r->num_allpasses; i++) 
	    if (r->allpasses[i]) 
	      mus_free(r->allpasses[i]);
	  FREE(r->allpasses);
	}
      FREE(r);
    }
  global_rev = NULL;
}

static Float *comb_factors = NULL;

static rev_info *make_nrev(snd_info *sp, int chans) 
{
  /* Mike McNabb's nrev from Mus10 days (ca. 1978) */
  #define BASE_DLY_LEN 14
  static int base_dly_len[BASE_DLY_LEN] = {1433, 1601, 1867, 2053, 2251, 2399, 347, 113, 37, 59, 43, 37, 29, 19};
  static int dly_len[BASE_DLY_LEN];
  #define NREV_COMBS 6
  static Float nrev_comb_factors[NREV_COMBS] = {0.822, 0.802, 0.773, 0.753, 0.753, 0.733};
  Float srscale;
  int i, j, len;
  rev_info *r;
  if (sp == NULL) return(NULL);
  srscale = sp->reverb_control_length * SND_SRATE(sp) / 25641.0;
  for (i = 0; i < BASE_DLY_LEN; i++) 
    dly_len[i] = get_prime((int)(srscale * base_dly_len[i]));
  r = (rev_info *)CALLOC(1, sizeof(rev_info));
  r->num_combs = NREV_COMBS;
  r->combs = (mus_any **)CALLOC(r->num_combs, sizeof(mus_any *));
  r->num_allpasses = 4 + chans;
  r->allpasses = (mus_any **)CALLOC(r->num_allpasses, sizeof(mus_any *));
  if (comb_factors) FREE(comb_factors);
  comb_factors = (Float *)CALLOC(r->num_combs, sizeof(Float));
  for (i = 0; i < r->num_combs; i++) 
    {
      comb_factors[i] = nrev_comb_factors[i];
      r->combs[i] = mus_make_comb(comb_factors[i] * sp->reverb_control_feedback, dly_len[i], NULL, dly_len[i], MUS_INTERP_LINEAR);
    }
  r->onep = mus_make_one_pole(sp->reverb_control_lowpass, sp->reverb_control_lowpass - 1.0);
  for (i = 0, j = r->num_combs; i < 4; i++, j++) 
    r->allpasses[i] = mus_make_all_pass(-0.700, 0.700, dly_len[j], NULL, dly_len[j], MUS_INTERP_LINEAR);
  for (i = 0, j = 10; i < chans; i++)
    {
      if (j < BASE_DLY_LEN) 
	len = dly_len[j]; 
      else len = get_prime((int)(40 + mus_random(20.0)));
      r->allpasses[i + 4] = mus_make_all_pass(-0.700, 0.700, len, NULL, len, MUS_INTERP_LINEAR);
    }
  return(r);
}

static void make_reverb(snd_info *sp, int chans)
{ 
  reverb_chans = chans;
  global_rev = make_nrev(sp, chans);
}

static void set_reverb_filter_coeff(Float newval)
{
  if (global_rev)
    {
      mus_set_xcoeff(global_rev->onep, 0, newval);
      mus_set_ycoeff(global_rev->onep, 1, 1.0 - newval);
    }
}

static void set_reverb_comb_factors(Float val)
{
  int j;
  if (global_rev)
    for (j = 0; j < global_rev->num_combs; j++)
      mus_set_scaler(global_rev->combs[j], comb_factors[j] * val);
}


/* -------- contrast-enhancement -------- */

static Float contrast (dac_info *dp, Float amp, Float index, Float inval)
{
  return(amp * mus_contrast_enhancement(dp->contrast_amp * inval, index));
}

static mus_error_handler_t *old_error_handler;
static bool got_local_error = false;
static void local_mus_error(int type, char *msg)
{
  got_local_error = true;
  snd_error_without_format(msg);
}

Float *sample_linear_env(env *e, int order)
{
  /* used only for filter coeffs (env here is the frequency response curve) */
  mus_any *ge;
  int ordp;
  Float *data = NULL;
  Float last_x, step, x;
  last_x = e->data[(e->pts - 1) * 2];
  step = 2 * last_x / ((Float)order - 1);
  ordp = e->pts; 
  if (ordp < order) ordp = order;
  got_local_error = false;
  old_error_handler = mus_error_set_handler(local_mus_error);
  ge = mus_make_env(e->data, e->pts, 1.0, 0.0, e->base, 0.0, 0, 1000 * ordp, NULL);
  mus_error_set_handler(old_error_handler);
  if (!got_local_error)
    {
      int i, j;
      data = (Float *)CALLOC(order, sizeof(Float));
      for (i = 0, x = 0.0; i < order / 2; i++, x += step) 
	data[i] = mus_env_interp(x, ge);
      for (j = order / 2 - 1, i = order / 2; (i < order) && (j >= 0); i++, j--) 
	data[i] = data[j];
      mus_free(ge);
    }
  return(data);
}

static dac_info *make_dac_info(chan_info *cp, snd_info *sp, snd_fd *fd, int out_chan)
{
  dac_info *dp;
  dp = (dac_info *)CALLOC(1, sizeof(dac_info)); /* only place dac_info is created */
  dp->stop_procedure = XEN_FALSE;
  dp->region = -1;
  dp->a = NULL;
  dp->a_size = 0;
  dp->audio_chan = out_chan;
  dp->never_sped = true;
  if (sp)
    {
      dp->expanding = sp->expand_control_p;
      dp->filtering = ((sp->filter_control_p) && (sp->filter_control_order > 0));
      dp->reverbing = sp->reverb_control_p;
      dp->contrast_amp = sp->contrast_control_amp;
      if (!(snd_feq(sp->speed_control * sp->speed_control_direction, 1.0)))
	{
	  dp->src = make_src(0.0, fd, sp->speed_control * sp->speed_control_direction);
	  if (dp->src == NULL)
	    {
	      FREE(dp);
	      return(NULL);
	    }
	}
      /* that is, if user wants fancy src, he needs to say so before we start */
      if (dp->expanding) 
	{
	  dp->spd = (spd_info *)make_expand(sp, sp->expand_control, dp);
	  dp->expand_ring_frames = (int)(SND_SRATE(sp) * sp->expand_control * sp->expand_control_length * 2);
	}
      if (dp->filtering)
	{
	  sp->filter_control_changed = false;
	  if (!(sp->filter_control_envelope)) 
	    dp->filtering = false;
	  else
	    {
	      Float *data = NULL;
	      data = sample_linear_env(sp->filter_control_envelope, sp->filter_control_order);
	      if (data)
		{
		  dp->flt = make_flt(dp, sp->filter_control_order, data);
		  FREE(data);
		}
	      else dp->filtering = false;
	    }
	}
    }
  dp->chn_fd = fd;
  dp->sp = sp;
  dp->cp = cp;
  return(dp);
}

static void free_dac_info(dac_info *dp, play_stop_t reason)
{
  if (XEN_PROCEDURE_P(dp->stop_procedure))
    {
      XEN_CALL_1(dp->stop_procedure, 
		 C_TO_XEN_INT((int)reason),
		 "play stop procedure");
      snd_unprotect_at(dp->stop_procedure_gc_loc);
      dp->stop_procedure = XEN_FALSE;
    }
  if (dp->a) {FREE(dp->a); dp->a = NULL; dp->a_size = 0;}
  dp->chn_fd = free_snd_fd(dp->chn_fd);
  if (dp->spd) free_expand(dp->spd);
  if (dp->src) free_src(dp->src);
  if (dp->flt) mus_free(dp->flt);
  dp->sp = NULL;
  dp->cp = NULL;
  FREE(dp);
}


static int dac_max_sounds = 0;
static dac_info **play_list = NULL;
#define INITIAL_MAX_SOUNDS 16
static int play_list_members = 0;
static int max_active_slot = -1;


/* -------------------------------- special "hidden" control panel variables -------------------------------- */

typedef enum {DAC_EXPAND, DAC_EXPAND_RAMP, DAC_EXPAND_LENGTH, DAC_EXPAND_HOP, DAC_EXPAND_SCALER, 
	      DAC_CONTRAST_AMP, DAC_REVERB_FEEDBACK, DAC_REVERB_LOWPASS} dac_field_t;

static void dac_set_field(snd_info *sp, Float newval, dac_field_t field)
{
  /* if sp == NULL, sets globally */
  if (play_list)
    {
      if (field == DAC_REVERB_LOWPASS)
	{
	  if (global_rev)
	    set_reverb_filter_coeff(newval);
	}
      else
	{
	  if (field == DAC_REVERB_FEEDBACK)
	    {
	      if (global_rev)
		set_reverb_comb_factors(newval);
	    }
	  else
	    {
	      int i;
	      for (i = 0; i <= max_active_slot; i++)
		{
		  dac_info *dp;
		  dp = play_list[i];
		  if ((dp) && ((sp == NULL) || (sp == dp->sp)))
		    {
		      int val;
		      switch (field)
			{
			case DAC_EXPAND: 
			  if (dp->spd)
			    mus_set_increment(dp->spd->gen, newval); 
			  break;
			case DAC_EXPAND_LENGTH: /* segment length */
			  if (dp->spd)
			    {
			      val = (int)(SND_SRATE(sp) * newval);
			      mus_set_length(dp->spd->gen, val);
			      mus_set_ramp(dp->spd->gen, (int)(val * sp->expand_control_ramp));
			    }
			  break;
			case DAC_EXPAND_RAMP: 
			  if (dp->spd)
			    {
			      val = (int)(newval * sp->expand_control_length * SND_SRATE(sp));
			      mus_set_ramp(dp->spd->gen, val); 
			    }
			  break;
			case DAC_EXPAND_HOP: /* output hop */
			  if (dp->spd)
			    {
			      val = (int)(SND_SRATE(sp) * newval);
			      mus_set_hop(dp->spd->gen, val); 
			      mus_set_increment(dp->spd->gen, sp->expand_control);
			    }
			  break;
			case DAC_EXPAND_SCALER:
			  if (dp->spd)
			    mus_set_scaler(dp->spd->gen, newval); 
			  break;
			case DAC_CONTRAST_AMP:
			  dp->contrast_amp = newval;
			  break;
			default:
			  break;
			}
		    }
		}
	    }
	}
    }
}

void dac_set_expand(snd_info *sp, Float newval) {dac_set_field(sp, newval, DAC_EXPAND);}
void dac_set_expand_length(snd_info *sp, Float newval) {dac_set_field(sp, newval, DAC_EXPAND_LENGTH);}
void dac_set_expand_ramp(snd_info *sp, Float newval) {dac_set_field(sp, newval, DAC_EXPAND_RAMP);}
void dac_set_expand_hop(snd_info *sp, Float newval) {dac_set_field(sp, newval, DAC_EXPAND_HOP);}
/* static void dac_set_expand_scaler(snd_info *sp, Float newval) {dac_set_field(sp, newval, DAC_EXPAND_SCALER);} */ /* not currently accessible */
void dac_set_contrast_amp(snd_info *sp, Float newval) {dac_set_field(sp, newval, DAC_CONTRAST_AMP);}
void dac_set_reverb_feedback(snd_info *sp, Float newval) {dac_set_field(sp, newval, DAC_REVERB_FEEDBACK);}
void dac_set_reverb_lowpass(snd_info *sp, Float newval) {dac_set_field(sp, newval, DAC_REVERB_LOWPASS);}



/* -------------------------------- stop playing (remove from play-list) -------------------------------- */

typedef struct {
  int srate;                /* output srate */
  int channels;             /* total output channels currently active */
  int frames;               /* samples per channel per output block */
  int devices;              /* output devices active */
  int *chans_per_device;    /* channels sent to each active device */
  int out_format;           /* output data format */
  int slice;                /* background process state (i.e. starting, running, quitting) */
  off_t reverb_ring_frames; /* how long the reverb rings after the end (if reverb, of course) */

} dac_state;

static dac_state *snd_dacp = NULL;

static void free_dac_state(void)
{
  if (snd_dacp)
    {
      if (snd_dacp->chans_per_device) FREE(snd_dacp->chans_per_device);
      FREE(snd_dacp);
      snd_dacp = NULL;
    }
}

static bool dac_running = false;
static XEN play_hook;
static XEN start_playing_hook;
static XEN stop_playing_hook;
static XEN stop_playing_selection_hook;
static XEN start_playing_selection_hook;

#define MAX_DEVICES 8
static int dev_fd[MAX_DEVICES] = {-1, -1, -1, -1, -1, -1, -1, -1};
static void cleanup_dac_hook(void);

void cleanup_dac(void)
{
  /* called only from snd_exit_cleanly in snd-main.c */
  int i;
  if (dac_running) 
    for (i = 0; i < MAX_DEVICES; i++) 
      if (dev_fd[i] != -1) 
	{
	  mus_audio_close(dev_fd[i]);
	  dev_fd[i] = -1;
	}
  for (i = 0; i < MAX_DEVICES; i++) dev_fd[i] = -1;
  cleanup_dac_hook();
  dac_running = false;
}

static void reflect_play_stop(snd_info *sp) 
{
#if (!USE_NO_GUI)
  set_control_panel_play_button(sp);
#endif
  set_open_file_play_button(false);
  reflect_play_stop_in_popup_menu();
  view_files_unplay();
}

static void free_player(snd_info *sp);

typedef enum {WITHOUT_TOGGLE, WITH_TOGGLE} dac_toggle_t;

static void stop_playing_with_toggle(dac_info *dp, dac_toggle_t toggle, with_hook_t with_hook, play_stop_t reason)
{
  snd_info *sp = NULL;
  bool sp_stopping = false;
  if ((dp == NULL) || (play_list == NULL)) return;
  sp = dp->sp;
  if ((sp) && (sp->inuse != SOUND_IDLE))
    {
      sp->playing_mark = NULL;
      if (sp->playing > 0) sp->playing--;
      if (sp->playing == 0) sp_stopping = true;
      if (sp_stopping)
	{
	  if ((sp->inuse == SOUND_NORMAL) && (sp->with_tracking_cursor != DONT_TRACK) && (sp->index >= 0))
	    {
	      int i;
	      for (i = 0; i < sp->nchans; i++)
		cursor_moveto_with_window(sp->chans[i], 
					  sp->chans[i]->original_cursor, 
					  sp->chans[i]->original_left_sample, 
					  sp->chans[i]->original_window_size);
	    }
	  /* this is needed to get the original window/cursor location displayed after playing */
	  if (sp->with_tracking_cursor == TRACK_ONCE)
	    sp->with_tracking_cursor = DONT_TRACK;
	}
      /* if ctrl-click play, c-t, c-q -> this flag is still set from aborted previous play, so clear at c-t (or c-g) */
    }
  else sp = NULL; /* don't free it as a player below */
  play_list[dp->slot] = NULL;
  play_list_members--;
  if (toggle == WITH_TOGGLE) 
    {
      if ((sp) && (sp->playing <= 0)) 
	reflect_play_stop(sp);
      else 
	if (dp->region >= 0) 
	  reflect_play_region_stop(dp->region);
    }
  if (dp->slot == max_active_slot) max_active_slot--;
  if (with_hook == WITH_HOOK)
    {
      if (dp->region < 0) /* not region play */
	{
	  if (dp->selection)
	    {
	      reflect_play_selection_stop();
	      if (XEN_HOOKED(stop_playing_selection_hook))
		run_hook(stop_playing_selection_hook, 
			 XEN_EMPTY_LIST, 
			 S_stop_playing_selection_hook);
	    }
	  else
	    {
	      if ((sp_stopping) && (XEN_HOOKED(stop_playing_hook)))
		run_hook(stop_playing_hook,
			 XEN_LIST_1(C_TO_XEN_INT(sp->index)),
			 S_stop_playing_hook);
	    }
	}
    }
  if ((sp) && (IS_PLAYER(sp))) 
    {
      int k;
      bool free_ok = true;
      dp->sp = NULL; /* free_player frees it */
      for (k = dp->slot + 1; k < dac_max_sounds; k++)
	{
	  dac_info *ndp;
	  ndp = play_list[k];
	  if ((ndp) &&
	      (ndp->sp) &&
	      (ndp->sp == sp))
	    {
	      free_ok = false;
	      break;
	    }
	}
      if (free_ok) 
	free_player(sp); 
      sp = NULL;
    }
  free_dac_info(dp, reason); /* this will call the stop-function, if any */
  if ((sp) && (sp_stopping) && (sp->delete_me)) 
    {
      if (sp->delete_me != (struct dialog_play_info *)1) clear_deleted_snd_info(sp->delete_me); /* for various file dialog play buttons */
      completely_free_snd_info(sp); /* dummy snd_info struct for (play "filename") in snd-xen.c */
    }
}

static void stop_playing(dac_info *dp, with_hook_t with_hook, play_stop_t reason) {stop_playing_with_toggle(dp, WITH_TOGGLE, with_hook, reason);}
static Cessate dac_in_background(Indicium ptr);
static Cessate dac_work_proc = 0; /* needed in gtk to force process to stop */

static void stop_playing_sound_with_toggle(snd_info *sp, dac_toggle_t toggle, with_hook_t with_hook, play_stop_t reason)
{
  /* this needs to scan all current play_list members and remove any that are referring
   * to sp, even indirectly (as through the current selection)
   */

  /* if reason = PLAY_STOP_CALLED, PLAY_C_T, or PLAY_C_G, we need to slam the output volumes to 0.0 (so the
   *    draining output is not played), then once we're sure output has stopped, put them back where they were.
   *    But, this action is sound-driver (OS) specific -- perhaps just hit MUS_AUDIO_AMP and hope?
   *    How to tell that on-card buffers are finally empty?  Wait 1 or 2 seconds?
   *
   * an added annoyance -- to get current amps, we have to make separate read call for each chan!
   */

  if ((sp) && (play_list))
    {
      int i;
      dac_info *dp;
      for (i = 0; i < dac_max_sounds; i++)
	if ((play_list[i]) && 
	    (sp == (play_list[i]->sp)))
	  {
	    dp = play_list[i];
	    play_list[i] = NULL;	  
	    stop_playing_with_toggle(dp, toggle, with_hook, reason);
	  }
      if ((play_list_members > 0) && (with_hook == WITH_HOOK))
	{
	  /* see comment below */
	  for (i = 0; i < dac_max_sounds; i++)
	    if ((play_list[i]) && 
		(sp == (play_list[i]->sp)))
	      {
		dp = play_list[i];
		play_list[i] = NULL;	  
		stop_playing_with_toggle(dp, toggle, WITHOUT_HOOK, reason); /* do it again but with hook disabled */
	      }
	}
    }
}

void stop_playing_sound(snd_info *sp, play_stop_t reason) {stop_playing_sound_with_toggle(sp, WITH_TOGGLE, WITH_HOOK, reason);}
void stop_playing_sound_without_hook(snd_info *sp, play_stop_t reason) {stop_playing_sound_with_toggle(sp, WITH_TOGGLE, WITHOUT_HOOK, reason);}
void stop_playing_sound_no_toggle(snd_info *sp, play_stop_t reason) {stop_playing_sound_with_toggle(sp, WITHOUT_TOGGLE, WITH_HOOK, reason);}

static void stop_playing_all_sounds_1(with_hook_t with_hook, play_stop_t reason)
{
  if (play_list)
    {
      int i;
      dac_info *dp;
      for (i = 0; i < dac_max_sounds; i++)
	{
	  dp = play_list[i];
	  play_list[i] = NULL;	  
	  stop_playing(dp, with_hook, reason);
	  /* this can call stop_playing_hook which can make a new play_list member where the old one used to be! */
	}
      if ((play_list_members > 0) && (with_hook == WITH_HOOK))
	{
	  /* stop-playing-hook must have started a new player, but in this case,
	   *   the rest of Snd assumes the dac has stopped no matter what.  So,
	   *   we make a second pass with the hooks disabled.
	   */
	  for (i = 0; i < dac_max_sounds; i++)
	    {
	      dp = play_list[i];
	      play_list[i] = NULL;	  
	      stop_playing(dp, WITHOUT_HOOK, reason);
	    }
	}
    }
  if (snd_dacp)
    {
      snd_dacp->slice = 2;
      dac_in_background(NULL);   /* try to force completion (work proc might not be called above) */
    }
}

void stop_playing_all_sounds(play_stop_t reason) {stop_playing_all_sounds_1(WITH_HOOK, reason);}
static void stop_playing_all_sounds_without_hook(play_stop_t reason) {stop_playing_all_sounds_1(WITHOUT_HOOK, reason);}

void stop_playing_region(int n, play_stop_t reason)
{
  if (play_list)
    {
      int i;
      dac_info *dp;
      for (i = 0; i < dac_max_sounds; i++)
	if ((play_list[i]) &&
	    (play_list[i]->region == n))
	  {
	    dp = play_list[i];
	    play_list[i] = NULL;	  
	    stop_playing(dp, WITHOUT_HOOK, reason);
	  }
    }
}

static bool playing(void)
{
  int i;
  if (play_list)
    for (i = 0; i < dac_max_sounds; i++)
      if (play_list[i]) return(true);
  return(false);
}


/* -------------------------------- play (add to play-list) -------------------------------- */

static int find_slot_to_play(void)
{
  int i, old_size;
  if (play_list == NULL)
    {
      dac_max_sounds = INITIAL_MAX_SOUNDS;
      play_list = (dac_info **)CALLOC(dac_max_sounds, sizeof(dac_info *));
    }
  for (i = 0; i < dac_max_sounds; i++) 
    if (!play_list[i]) 
      return(i);
  old_size = dac_max_sounds;
  dac_max_sounds += INITIAL_MAX_SOUNDS;
  play_list = (dac_info **)REALLOC(play_list, dac_max_sounds * sizeof(dac_info *));
  for (i = old_size; i < dac_max_sounds; i++) play_list[i] = NULL;
  return(old_size);
}

static dac_info *init_dp(int slot, chan_info *cp, snd_info *sp, snd_fd *fd, off_t end, int out_chan)
{
  dac_info *dp;
  dp = make_dac_info(cp, sp, fd, out_chan); /* sp == NULL if region */
  if (dp == NULL) 
    {
      free_snd_fd(fd);
      return(NULL);
    }
  play_list_members++;
  dp->end = end;
  play_list[slot] = dp;
  dp->slot = slot;
  if (max_active_slot < slot) max_active_slot = slot;
  if (sp)
    {
      dp->cur_srate = sp->speed_control * sp->speed_control_direction;
      if (!(snd_feq(dp->cur_srate, 1.0))) dp->never_sped = false;
      dp->cur_amp = AMP_CONTROL(sp, dp);
      dp->cur_index = sp->contrast_control;
      dp->cur_exp = sp->expand_control;
      dp->cur_rev = sp->reverb_control_scale;
    }
  return(dp);
}

static void start_dac(int srate, int channels, play_process_t background, Float decay)
{
  int i;
  /* look for channel folding cases etc */
  /* channels = how many output audio chans we have; dac_combines_channels sets whether to wrap or muffle chans outside this limit */
  for (i = 0; i <= max_active_slot; i++)
    {
      dac_info *dp;
      dp = play_list[i];
      if ((dp) && (dac_running))                          /* dac_running also if apply */
	{
	  /* in the "normal" (non-apply) case the reverb allocation is deferred until we're sure about the number of output channels */
	  if ((dp->reverbing) && 
	      (dp->sp) && 
	      (global_rev == NULL))
	    make_reverb(dp->sp, channels);
          if (dp->audio_chan >= channels)                 /* if dac_running, the number of channels has already been set and won't change */
	    {
	      if (dac_combines_channels(ss))
		dp->audio_chan %= channels;
	      else stop_playing(dp, WITHOUT_HOOK, PLAY_NO_CHANNEL);
	    }
	}
    }
  /* any number of sounds can be piling into the dac buffers with new ones added at any time
   *   (this is the play_list -- an array of dac_info pointers, each one a separate channel rendition)
   */
  if (!dac_running)
    {
      if (snd_dacp) free_dac_state();
      snd_dacp = (dac_state *)CALLOC(1, sizeof(dac_state));
      snd_dacp->slice = 0;
      snd_dacp->srate = srate;
      snd_dacp->out_format = MUS_AUDIO_COMPATIBLE_FORMAT;
      if (snd_dacp->srate <= 0) snd_dacp->srate = 44100;
      snd_dacp->channels = channels;
      if (dac_size(ss) > 0)
	snd_dacp->frames = dac_size(ss);
      else snd_dacp->frames = 256;
      snd_dacp->devices = 1;  /* just a first guess */
      snd_dacp->reverb_ring_frames = (off_t)(srate * decay);
      if (background == IN_BACKGROUND) 
	dac_work_proc = BACKGROUND_ADD(dac_in_background, NULL);
      else
	{
	  /* here we want to play as an atomic (not background) action */
	  while (dac_in_background(NULL) == BACKGROUND_CONTINUE)
	    check_for_event(); /* need to be able to C-g out of this */
	}
    }
}

/*  pos = to_c_edit_position(cp, edpos, caller, arg_pos); */

static dac_info *add_channel_to_play_list(chan_info *cp, snd_info *sp, off_t start, off_t end, int pos, int out_chan)
{
  /* if not sp, control panel is ignored */
  int slot;
  read_direction_t direction = READ_FORWARD;
  off_t beg = 0;
  snd_fd *sf;
  if (pos == AT_CURRENT_EDIT_POSITION) pos = cp->edit_ctr;
  if (sp)
    {
      if (sp->speed_control_direction == 1) 
	{
	  if (start >= cp->samples[pos]) return(NULL);
	  direction = READ_FORWARD; 
	  beg = start;
	}
      else 
	{
	  direction = READ_BACKWARD;
	  if ((start == 0) || (start >= cp->samples[pos])) /* don't be pedantic about the start point */
	    beg = cp->samples[pos] - 1;
	  else beg = start;
	  if ((end == 0) || (end >= (cp->samples[pos])))
	    end = NO_END_SPECIFIED;
	}
    }
  slot = find_slot_to_play();
  if (slot == -1) return(NULL);
  sf = init_sample_read_any(beg, cp, direction, pos);
  if (sf)
    {
      if (sp) 
	{
	  sp->playing++;
	  if (sp->with_tracking_cursor != DONT_TRACK)
	    {
	      cp->original_cursor = CURSOR(cp);
	      if (cp->axis)
		{
		  cp->original_left_sample = cp->axis->losamp;
		  cp->original_window_size = cp->axis->hisamp - cp->axis->losamp;
		}
	      cp->cursor_on = true;
	      cursor_moveto_without_verbosity(cp, start);
	    }
	}
      return(init_dp(slot, cp, sp, sf, end, out_chan));
    }
  return(NULL);
}

static dac_info *add_region_channel_to_play_list(int region, int chan, off_t beg, off_t end, int out_chan)
{
  int slot;
  snd_fd *fd;
  slot = find_slot_to_play();
  if (slot == -1) return(NULL);
  fd = init_region_read(beg, region, chan, READ_FORWARD);
  if (fd)
    return(init_dp(slot, fd->cp, NULL, fd, end, out_chan));
  return(NULL);
}

void play_region_1(int region, play_process_t background, XEN stop_proc)
{
  /* just plays region (not current selection) -- no control panel etc */
  int chans, i;
  dac_info *dp = NULL, *rtn_dp = NULL;
  if ((background == NOT_IN_BACKGROUND) && (play_list_members > 0)) return;
  if (!(region_ok(region))) return;
  chans = region_chans(region);
  if (chans == 0) return;
  for (i = 0; i < chans; i++) 
    {
      dp = add_region_channel_to_play_list(region, i, 0, NO_END_SPECIFIED, i); /* i = out chan */
      if (dp) 
	{
	  if (rtn_dp == NULL) rtn_dp = dp;
	  dp->region = region;
	}
    }
  if (rtn_dp)
    {
      rtn_dp->stop_procedure = stop_proc;
      if (XEN_PROCEDURE_P(stop_proc))
	rtn_dp->stop_procedure_gc_loc = snd_protect(stop_proc);
      start_dac(region_srate(region), chans, background, DEFAULT_REVERB_CONTROL_DECAY);
    }
}

void play_region(int region, play_process_t background)
{
  play_region_1(region, background, XEN_FALSE);
}

static bool call_start_playing_hook(snd_info *sp)
{
  if ((XEN_HOOKED(start_playing_hook)) &&
      (XEN_TRUE_P(run_or_hook(start_playing_hook,
			      XEN_LIST_1(C_TO_XEN_INT(sp->index)),
			      S_start_playing_hook))))
    {
      reflect_play_stop(sp);           /* turns off buttons */
      if (sp->delete_me) 
	completely_free_snd_info(sp);  /* dummy snd_info struct for (play "filename") in snd-xen.c */
      return(true);
    }
  return(false);
}

static bool call_start_playing_selection_hook(void)
{
  return((XEN_HOOKED(start_playing_selection_hook)) &&
	 (XEN_TRUE_P(run_or_hook(start_playing_selection_hook,
				 XEN_EMPTY_LIST,
				 S_start_playing_selection_hook))));
}

static dac_info *play_channel_1(chan_info *cp, off_t start, off_t end, play_process_t background, 
				int pos, XEN stop_proc, int out_chan)
{
  /* just plays one channel (ignores possible sync), includes start-hook */
  snd_info *sp = NULL;
  dac_info *dp = NULL;
  sp = cp->sound;
  if (sp->inuse == SOUND_IDLE) return(NULL);
  if (call_start_playing_hook(sp)) return(NULL);
  dp = add_channel_to_play_list(cp, sp, start, end, pos, out_chan);
  if (dp) 
    {
      dp->stop_procedure = stop_proc;
      if (XEN_PROCEDURE_P(stop_proc))
	dp->stop_procedure_gc_loc = snd_protect(stop_proc);
      set_play_button(sp, true);
      start_dac(SND_SRATE(sp), 1, background, sp->reverb_control_decay);
    }
  return(dp);
}

void play_channel(chan_info *cp, off_t start, off_t end)
{
  play_channel_1(cp, start, end, IN_BACKGROUND, AT_CURRENT_EDIT_POSITION, XEN_FALSE, cp->chan);
}

static dac_info *play_sound_1(snd_info *sp, off_t start, off_t end, play_process_t background, 
			      XEN edpos, XEN stop_proc, const char *caller, int arg_pos)
{
  /* just plays one sound (ignores possible sync) */
  int i, pos;
  dac_info *dp = NULL, *rtn_dp = NULL;

  if ((background == NOT_IN_BACKGROUND) && 
      (play_list_members > 0)) 
    return(NULL);
  if (sp->inuse == SOUND_IDLE) return(NULL);
  if (call_start_playing_hook(sp)) return(NULL);
  for (i = 0; i < sp->nchans; i++)
    {
      pos  = to_c_edit_position(sp->chans[i], edpos, caller, arg_pos);
      dp = add_channel_to_play_list(sp->chans[i], sp, start, end, pos, i); /* i = out chan */
      if ((dp) && (rtn_dp == NULL)) rtn_dp = dp;
    }
  if (rtn_dp)
    {
      rtn_dp->stop_procedure = stop_proc;
      if (XEN_PROCEDURE_P(stop_proc))
	rtn_dp->stop_procedure_gc_loc = snd_protect(stop_proc);
      set_play_button(sp, true);
      start_dac(SND_SRATE(sp), sp->nchans, background, sp->reverb_control_decay);
    }
  return(rtn_dp);
}

void play_sound(snd_info *sp, off_t start, off_t end)
{
  play_sound_1(sp, start, end, IN_BACKGROUND, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), XEN_FALSE, NULL, 0);
}

static dac_info *play_channels_1(chan_info **cps, int chans, off_t *starts, off_t *ur_ends, play_process_t background, 
				 XEN edpos, bool selection, XEN stop_proc, const char *caller, int arg_pos)
{
  /* ends can be NULL */
  int i, pos;
  snd_info *sp = NULL;
  dac_info *dp = NULL, *rtn_dp = NULL;
  off_t *ends;
  if ((background == NOT_IN_BACKGROUND) && 
      (play_list_members > 0)) 
    return(NULL);
  if (chans <= 0) return(NULL);
  if (!selection)
    {
      if (call_start_playing_hook(cps[0]->sound)) return(NULL);
    }
  else 
    {
      if (call_start_playing_selection_hook()) return(NULL);
    }
  if (ur_ends)
    ends = ur_ends;
  else
    {
      ends = (off_t *)CALLOC(chans, sizeof(off_t));
      for (i = 0; i < chans; i++) 
	ends[i] = NO_END_SPECIFIED;
    }
  sp = cps[0]->sound;
  for (i = 0; i < chans; i++) 
    {
      pos  = to_c_edit_position(cps[i], edpos, caller, arg_pos);
      dp = add_channel_to_play_list(cps[i], sp, starts[i], ends[i], pos, i);
      if (dp) 
	{
	  if (rtn_dp == NULL) rtn_dp = dp;
	  if (selection) dp->selection = true;
	}
    }
  if (ur_ends == NULL) FREE(ends);
  if ((sp) && (rtn_dp)) 
    {
      rtn_dp->stop_procedure = stop_proc;
      if (XEN_PROCEDURE_P(stop_proc))
	rtn_dp->stop_procedure_gc_loc = snd_protect(stop_proc);
      set_play_button(sp, true);
      start_dac(SND_SRATE(sp), chans, background, sp->reverb_control_decay);
    }
  return(rtn_dp);
}

void play_channels(chan_info **cps, int chans, off_t *starts, off_t *ur_ends, 
		   play_process_t background, XEN edpos, bool selection, const char *caller, int arg_pos)
{
  play_channels_1(cps, chans, starts, ur_ends, background, edpos, selection, XEN_FALSE, caller, arg_pos);
}

static dac_info *play_selection_1(play_process_t background, XEN stop_proc)
{
  /* just plays the current selection */
  dac_info *dp = NULL;
  if (selection_is_active())
    {
      sync_info *si = NULL;
      si = selection_sync();
      if (si)
	{
	  int i;
	  off_t *ends;
	  ends = (off_t *)CALLOC(si->chans, sizeof(off_t));
	  for (i = 0; i < si->chans; i++) 
	    {
	      snd_info *sp;
	      sp = si->cps[i]->sound;
	      if ((sp) && 
		  (!(snd_feq(sp->speed_control, 1.0))) && 
		  (sp->speed_control > 0.0))
		ends[i] = si->begs[i] + (off_t)(((double)selection_len() / (Float)(sp->speed_control)));
	      /* user might move speed control while playing selection, so ideally we'd watch dp->chn_fd here */
	      else ends[i] = si->begs[i] + selection_len();
	    }
	  dp = play_channels_1(si->cps, si->chans, si->begs, ends, background, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), true, stop_proc, NULL, 0);
	  si = free_sync_info(si); /* does not free sample readers */
	  FREE(ends);
	}
    }
  return(dp);
}

void play_selection(play_process_t background)
{
  play_selection_1(background, XEN_FALSE);
}


/* -------------------------------- process samples and write to DAC -------------------------------- */

#define NO_CHANGE 0
#define JUST_AMP 1
#define JUST_SPEED 2
#define ALL_CHANGES 3

static int choose_dac_op(dac_info *dp, snd_info *sp)
{
  if (!sp) return(NO_CHANGE);
  if ((dp->expanding) || (dp->filtering) || (dp->reverbing) || (sp->contrast_control_p)) 
    return(ALL_CHANGES);
  else
    {
      if ((sp->speed_control_direction != 1) || (!(snd_feq(sp->speed_control, 1.0))) || (!(snd_feq(dp->cur_srate, 1.0))))
	return(JUST_SPEED);
      else
	{
	  if ((AMP_CONTROL(sp, dp) == dp->cur_amp) && (snd_feq(AMP_CONTROL(sp, dp), 1.0)))
	    return(NO_CHANGE);
	  else return(JUST_AMP);
	}
    }
}

static int cursor_time;
/* can't move cursor on each dac buffer -- causes clicks */

static bool dac_pausing = false;
void toggle_dac_pausing(void) {dac_pausing = (!dac_pausing); play_button_pause(dac_pausing);} /* only below and snd-kbd.c */
bool play_in_progress(void) {return(play_list_members > 0);}

static unsigned char **audio_bytes = NULL;
static int audio_bytes_size = 0;
static int audio_bytes_devices = 0;

static mus_sample_t **dac_buffers = NULL;
static int dac_buffer_size = 0;
static int dac_buffer_chans = 0; /* chans allocated */
static Float **rev_ins;

#define WRITE_TO_DAC 1
#define WRITE_TO_FILE 0

static XEN dac_hook;
static XEN stop_dac_hook;
static XEN sdobj;
static int sdobj_loc = NOT_A_GC_LOC;
static void cleanup_dac_hook(void)
{
  if (XEN_HOOKED(stop_dac_hook))
    run_hook(stop_dac_hook, 
	     XEN_EMPTY_LIST,
	     S_stop_dac_hook);
  if (!(XEN_FALSE_P(sdobj)))
    {
      snd_unprotect_at(sdobj_loc);
      sdobj = XEN_FALSE;
      sdobj_loc = NOT_A_GC_LOC;
    }
}

static int fill_dac_buffers(int write_ok)
{
  /* return value used only by Apply */
  int i, j;
  bool cursor_change = false;
  int bytes, frames;
  Float *revin;
  Float amp, incr, sr, sincr, ind, indincr, ex, exincr, rev, revincr, fval;
  dac_info *dp;
  snd_info *sp;
  mus_sample_t *buf;
#if (HAVE_OSS || HAVE_ALSA)
  mus_sample_t **dev_bufs;
#endif
  frames = snd_dacp->frames;
  /* clear buffers */
  for (i = 0; i < snd_dacp->channels; i++) 
    memset(dac_buffers[i], 0, frames * sizeof(mus_sample_t));
  if (global_rev)
    for (i = 0; i < snd_dacp->channels; i++) 
      memset(rev_ins[i], 0, frames * sizeof(Float));

  if (dac_pausing) 
    cursor_change = false;
  else
    {
      if (XEN_HOOKED(play_hook))
	run_hook(play_hook, 
		 XEN_LIST_1(C_TO_XEN_INT(frames)),
		 S_play_hook);
      cursor_time += frames;
      cursor_change = (cursor_time >= (int)(snd_dacp->srate * cursor_update_interval(ss)));
      /* the check for hooks and so on has to be separate from the fill loop to keep everything synchronized across stop-function replay requests etc */

      /* first check for disasters (user closed sound while playing it or running Apply!) */
      for (i = 0; i <= max_active_slot; i++)
	{
	  dp = play_list[i];
	  if (dp)
	    {
	      sp = dp->sp; /* can be nil if region playing */
	      if ((sp) && ((sp->inuse == SOUND_IDLE) || (sp->playing == 0)))
		stop_playing(dp, WITHOUT_HOOK, PLAY_CLOSE); 
	    }
	}
      /* now read currently active chans and update locations -- don't call any hooks or stop-functions! */
      for (i = 0; i <= max_active_slot; i++)
	{
	  dp = play_list[i];
	  if (dp)
	    {
	      /* check for moving cursor */
	      sp = dp->sp; /* can be nil if region playing */
	      if ((sp) && 
		  (cursor_change) && 
		  (sp->with_tracking_cursor != DONT_TRACK) &&
		  (!(dp->chn_fd->at_eof)) &&
		  (dp->chn_fd->cb))
		{
		  off_t loc;
		  bool old_just_zero;
		  old_just_zero = dp->cp->just_zero;
		  dp->cp->just_zero = true;
		  loc = current_location(dp->chn_fd);
		  loc -= (int)(cursor_location_offset(ss) * dp->cur_srate); /* should work in either direction */
		  dp->cp->tracking = true;
		  cursor_moveto_without_verbosity(dp->cp, loc);
		  dp->cp->tracking = false;
		  dp->cp->just_zero = old_just_zero;
		}
	      /* add a buffer's worth from the current source into dp->audio_chan */
	      buf = dac_buffers[dp->audio_chan];
	      if (buf == NULL) continue;
	      revin = rev_ins[dp->audio_chan];
	      switch (choose_dac_op(dp, sp))
		{
		case NO_CHANGE:
		  /* simplest case -- no changes at all */
		  for (j = 0; j < frames; j++)
		    buf[j] += read_sample(dp->chn_fd);
		  break;

		case JUST_AMP:
		  /* AMP_CONTROL(sp, dp) is current UI value, dp->cur_amp is current local value */
		  amp = dp->cur_amp;
		  incr = (AMP_CONTROL(sp, dp) - amp) / (Float)(frames);
		  for (j = 0; j < frames; j++, amp += incr) 
		    buf[j] += (mus_sample_t)(read_sample(dp->chn_fd) * amp);
		  dp->cur_amp = amp;
		  break;

		case JUST_SPEED:
		  /* includes amp changes */
		  /* sp->speed_control is current UI value, dp->cur_srate is current local value */
		  dp->never_sped = false;
		  amp = dp->cur_amp;
		  incr = (AMP_CONTROL(sp, dp) - amp) / (Float)(frames);
		  sr = dp->cur_srate;
		  sincr = (sp->speed_control * sp->speed_control_direction - sr) / (Float)(frames);
		  if ((sr != 0.0) || (sincr != 0.0))
		    {
		      for (j = 0; j < frames; j++, amp += incr, sr += sincr) 
			buf[j] += MUS_FLOAT_TO_SAMPLE(amp * speed(dp, sr));
		    }
		  dp->cur_srate = sr;
		  dp->cur_amp = amp;
		  break;

		case ALL_CHANGES:
		  amp = dp->cur_amp;
		  incr = (AMP_CONTROL(sp, dp) - amp) / (Float)(frames);
		  sr = dp->cur_srate;
		  sincr = (sp->speed_control * sp->speed_control_direction - sr) / (Float)(frames);
		  if ((sincr != 0.0) || (!(snd_feq(sr, 1.0)))) dp->never_sped = false;
		  ind = dp->cur_index;
		  indincr = (sp->contrast_control - ind) / (Float)(frames);
		  rev = dp->cur_rev;
		  revincr = (sp->reverb_control_scale - rev) / (Float)(frames);
		  if ((dp->filtering) && (sp->filter_control_changed))
		    {
		      Float *data = NULL;
		      data = sample_linear_env(sp->filter_control_envelope, sp->filter_control_order);
		      if (data)
			{
			  /* check all chans in sp before clearing filter_order_changed flag */
			  int j;
			  for (j = i; j <= max_active_slot; j++)
			    {
			      dac_info *ndp;
			      ndp = play_list[j];
			      if ((ndp) && 
				  (ndp->sp == sp) && 
				  (ndp->filtering))
				{
				  if (sp->filter_control_order > ndp->a_size) /* need more room in dp->a == flt->xcoeffs and flt->state */
				    {
				      FREE(ndp->a);
				      ndp->a_size = sp->filter_control_order;
				      ndp->a = (Float *)CALLOC(ndp->a_size, sizeof(Float));
				    }
				  mus_make_fir_coeffs(sp->filter_control_order, data, ndp->a);      /* fill dp->a with new coeffs */
				  mus_filter_set_xcoeffs(ndp->flt, ndp->a);                         /* tell gen about them */
				  if (mus_filter_set_order(ndp->flt, sp->filter_control_order) < 0) /* fixup gen's order (and internal state array) */
				    snd_warning("trouble in filter (order not changed?)");
				}
			    }
			  FREE(data);
			}
		      sp->filter_control_changed = false;
		    }
		  if (dp->expanding)
		    {
		      ex = dp->cur_exp;
		      exincr = (sp->expand_control - ex) / (Float)(frames);
		      for (j = 0; j < frames; j++, amp += incr, sr += sincr, ind += indincr, ex += exincr, rev += revincr) 
			{
			  fval = expand(dp, sr, ex);
			  if (sp->contrast_control_p) fval = contrast(dp, amp, ind, fval); else fval *= amp;
			  if (dp->filtering) fval = mus_fir_filter(dp->flt, fval);
			  if (dp->reverbing) revin[j] += fval * rev;
			  buf[j] += MUS_FLOAT_TO_SAMPLE(fval);
			}
		      dp->cur_exp = ex;
		    }
		  else
		    {
		      if (dp->filtering)
			{
			  for (j = 0; j < frames; j++, amp += incr, sr += sincr, ind += indincr, rev += revincr) 
			    {
			      fval = speed(dp, sr);
			      if (sp->contrast_control_p) fval = contrast(dp, amp, ind, fval); else fval *= amp;
			      fval = mus_fir_filter(dp->flt, fval);
			      if (dp->reverbing) revin[j] += fval * rev;
			      buf[j] += MUS_FLOAT_TO_SAMPLE(fval);
			    }
			}
		      else
			{
			  if (sp->contrast_control_p)
			    {
			      for (j = 0; j < frames; j++, amp += incr, sr += sincr, ind += indincr, rev += revincr) 
				{
				  fval = contrast(dp, amp, ind, speed(dp, sr));
				  if (dp->reverbing) revin[j] += fval * rev;
				  buf[j] += MUS_FLOAT_TO_SAMPLE(fval);
				}
			    }
			  else
			    {
			      if (dp->never_sped)
				{
				  for (j = 0; j < frames; j++, amp += incr, rev += revincr) 
				    {
				      fval = amp * read_sample_to_float(dp->chn_fd);
				      revin[j] += fval * rev;
				      buf[j] += MUS_FLOAT_TO_SAMPLE(fval);
				    }
				}
			      else
				{
				  for (j = 0; j < frames; j++, amp += incr, sr += sincr, rev += revincr) 
				    {
				      fval = amp * speed(dp, sr);
				      revin[j] += fval * rev;
				      buf[j] += MUS_FLOAT_TO_SAMPLE(fval);
				    }
				}
			    }
			}
		    }
		  dp->cur_srate = sr;
		  dp->cur_amp = amp;
		  dp->cur_rev = rev;
		  dp->cur_index = ind;
		  break;
		}
	      if (dp->end != NO_END_SPECIFIED)
		{
		  if ((dp->chn_fd->cb == NULL) ||
		      ((dp->sp->speed_control_direction > 0) && 
		       ((dp->chn_fd->at_eof) || 
			(dp->end <= current_location(dp->chn_fd)))) ||
		      ((dp->sp->speed_control_direction < 0) && 
			(dp->end > current_location(dp->chn_fd))))
		    dp->end = 0;
		}
	    }
	} /* fill-case loop through max_active_slot */

      if (global_rev) 
	for (i = 0; i < frames; i++)
	  reverb(global_rev, rev_ins, dac_buffers, i);

      /* now hooks, stop-function etc */
      for (i = 0; i <= max_active_slot; i++)
	{
	  dp = play_list[i];
	  if (dp)
	    {
	      sp = dp->sp; /* can be nil if region playing */
	      /* check for EOF or specified end point */
	      if (write_ok == WRITE_TO_DAC) /* not Apply */
		{
		  if (dp->end == 0)
		    stop_playing(dp, WITH_HOOK, PLAY_COMPLETE);
		  else
		    {
		      if (dp->chn_fd->at_eof)
			{
			  if (!(dp->expanding))
			    stop_playing(dp, WITH_HOOK, PLAY_COMPLETE);
			  else
			    {
			      dp->expand_ring_frames -= frames;
			      if (dp->expand_ring_frames <= 0)
				stop_playing(dp, WITH_HOOK, PLAY_COMPLETE);
			    }
			}
		    }
		}
	      else /* Apply, always sets the end point explicitly */
		{
		  if (dp->end == 0)
		    {
		      stop_playing_all_sounds_without_hook(PLAY_COMPLETE);
		      play_list_members = 0; 
		      max_active_slot = -1;
		    }
		}
	    }
	} /* stop-case loop through max_active_slot */

      if ((global_rev) &&
	  (snd_dacp) &&
	  (play_list_members == 0))
	{
	  snd_dacp->reverb_ring_frames -= frames;
	  if (snd_dacp->reverb_ring_frames <= 0) 
	    free_reverb();
	}
    }
  /* now parcel these buffers out to the available devices */

  if ((snd_dacp) && (XEN_HOOKED(dac_hook)))
    {
      if (XEN_FALSE_P(sdobj))
	{
#if SNDLIB_USE_FLOATS
	  sdobj = wrap_sound_data(snd_dacp->channels, snd_dacp->frames, dac_buffers);
	  sdobj_loc = snd_protect(sdobj);
#else
	  {
	    sound_data *sd;
	    int i, j;
	    sdobj = make_sound_data(snd_dacp->channels, snd_dacp->frames);
	    sdobj_loc = snd_protect(sdobj);
	    sd = (sound_data *)XEN_OBJECT_REF(sdobj);
	    for (j = 0; j < sd->chans; j++)
	      for (i = 0; i < sd->length; i++)
		sd->data[j][i] = MUS_SAMPLE_TO_DOUBLE(dac_buffers[j][i]);
	  }
#endif
	}
    run_hook(dac_hook, 
	     XEN_LIST_1(sdobj),
	     S_dac_hook);
    }
  /* dac-hook might have forced stop-dac, so snd_dacp might be invalid here */
  if (snd_dacp)
    {
#if (HAVE_OSS || HAVE_ALSA)
      if (write_ok == WRITE_TO_DAC) 
	{
	  dev_bufs = dac_buffers;
	  for (i = 0; i < snd_dacp->devices; i++)
	    if (dev_fd[i] != -1)
	      {
		mus_file_write_buffer(snd_dacp->out_format,
				      0, frames - 1,
				      snd_dacp->chans_per_device[i],
				      dev_bufs,
				      (char *)(audio_bytes[i]),
				      clipping(ss));
		dev_bufs += snd_dacp->chans_per_device[i];
	      }
	  for (i = 0; i < snd_dacp->devices; i++)
	    if (dev_fd[i] != -1)
	      {
		bytes = snd_dacp->chans_per_device[i] * frames * mus_bytes_per_sample(snd_dacp->out_format);
		mus_audio_write(dev_fd[i], (char *)(audio_bytes[i]), bytes);
	      }
	}
#else
      if (write_ok == WRITE_TO_DAC) 
	{
	  mus_file_write_buffer(snd_dacp->out_format, 0, frames - 1, snd_dacp->channels, dac_buffers, (char *)(audio_bytes[0]), clipping(ss));
	  bytes = snd_dacp->channels * frames * mus_bytes_per_sample(snd_dacp->out_format);
	  mus_audio_write(dev_fd[0], (char *)(audio_bytes[0]), bytes);
	}
#endif
    }
  if (cursor_change) cursor_time = 0;
  return(frames);
}


/* -------------------------------- specialize mus_print -------------------------------- */

static mus_print_handler_t *old_dac_printer = NULL;
static char *last_print = NULL;

static void dac_mus_print(char *msg)
{
  if (last_print) FREE(last_print);
  last_print = copy_string(msg);
  (*old_dac_printer)(msg);
}

static void set_dac_print(void)
{
  if (last_print) FREE(last_print);
  last_print = NULL;
  if (old_dac_printer != dac_mus_print)
    old_dac_printer = mus_print_set_handler(dac_mus_print);
}

static void unset_dac_print(void)
{
  mus_print_set_handler(old_dac_printer);
}

static char *describe_dac(void)
{
  /* this is only called in dac_error and only then upon a failed mus_audio_open_output */
  int players = 0, i;
  dac_info *ptr = NULL;
  for (i = 0; i < dac_max_sounds; i++) 
    if (play_list[i]) 
      {
	ptr = play_list[i]; 
	players++;
      }
  if ((players == 1) && 
      (ptr->sp))
    return(ptr->sp->short_filename);
  return("");
}

static void dac_error(void)
{
  stop_playing_all_sounds_without_hook(PLAY_ERROR);
  if ((!last_print) &&
      (mus_audio_systems() == 0))
    snd_error_without_format("can't play: no audio support");
  else snd_error(_("can't play %s: %s"),
		 describe_dac(),
		 (last_print) ? last_print : "reason not known");
}


/* -------------------------------- initialize DAC -------------------------------- */

static void make_dac_buffers(void)
{
  /* make the per-channel buffers and audio output buffers */
  int bytes, i;
  if ((dac_buffers == NULL) || 
      (dac_buffer_chans < snd_dacp->channels) || 
      (dac_buffer_size < snd_dacp->frames))
    {
      if (dac_buffers)
	{
	  for (i = 0; i < dac_buffer_chans; i++) FREE(dac_buffers[i]);
	  FREE(dac_buffers);
	}
      if (rev_ins)
	{
	  for (i = 0; i < dac_buffer_chans; i++) FREE(rev_ins[i]);
	  FREE(rev_ins);
	}
      dac_buffers = (mus_sample_t **)CALLOC(snd_dacp->channels, sizeof(mus_sample_t *));
      rev_ins = (Float **)CALLOC(snd_dacp->channels, sizeof(Float *));
      for (i = 0; i < snd_dacp->channels; i++) 
	{
	  dac_buffers[i] = (mus_sample_t *)CALLOC(snd_dacp->frames, sizeof(mus_sample_t));
	  rev_ins[i] = (Float *)CALLOC(snd_dacp->frames, sizeof(Float));
	}
      dac_buffer_chans = snd_dacp->channels;
      dac_buffer_size = snd_dacp->frames;
      if (r_outs) FREE(r_outs);
      if (r_ins) FREE(r_ins);
      r_outs = (Float *)CALLOC(snd_dacp->channels, sizeof(Float));
      r_ins = (Float *)CALLOC(snd_dacp->channels, sizeof(Float));
    }
  bytes = snd_dacp->channels * dac_buffer_size * mus_bytes_per_sample(snd_dacp->out_format);
  if ((audio_bytes_size < bytes) || 
      (audio_bytes_devices < snd_dacp->devices))
    {
      if (audio_bytes)
	{
	  for (i = 0; i < audio_bytes_devices; i++) FREE(audio_bytes[i]);
	  FREE(audio_bytes);
	}
      audio_bytes = (unsigned char **)CALLOC(snd_dacp->devices, sizeof(unsigned char *));
      for (i = 0; i < snd_dacp->devices; i++) 
	audio_bytes[i] = (unsigned char *)CALLOC(bytes, sizeof(unsigned char));
      audio_bytes_size = bytes;
      audio_bytes_devices = snd_dacp->devices;
    }
}

static void stop_audio_output(void);

#if (HAVE_ALSA || HAVE_OSS)


/* Controls behavior of device selection logic below. No amount of logic
 * can make everybody happy all the time. The [i]logic below cannot always
 * produce the desired result but deleting it altogether will break the
 * systems that currently rely on it. Not wise without an external api
 * in place designed to select whatever the user _really_ wants. Till 
 * then set this to "1" to always send to the first device. */

static int feed_first_device = 0;

#define ALSA_MAX_DEVICES 64
static int alsa_devices[ALSA_MAX_DEVICES];
static int alsa_available_chans[ALSA_MAX_DEVICES];
static int alsa_max_chans[ALSA_MAX_DEVICES];
static int alsa_min_chans[ALSA_MAX_DEVICES];

static int alsa_max_chans_value = 0;
static int alsa_max_chans_dev = 0;
static bool audio_devices_scanned = false;
static int alsa_devices_available = 0;

static void scan_audio_devices(void)
{
  int err, cards, card, devs, dev, d;
  int index = 0;
  float direction;
  float val[ALSA_MAX_DEVICES];
  if (!audio_devices_scanned)
    {
      audio_devices_scanned = true;
       /* At this time
       * we always select the widest device if the requested channels fit into it. 
       * Otherwise we try to combine devices, if all fails we modify snd settings
       * so that channel folding takes place. This is inefficient but works for now. 
       */
      cards = mus_audio_systems();
      index = 0;
      /* scan all cards and build a list of available output devices */
      for (card = 0; card < cards; card++) 
	{
	  if ((err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(card), 
					  MUS_AUDIO_PORT, 
					  ALSA_MAX_DEVICES, val)) != MUS_NO_ERROR) 
	    {
	      snd_warning("can't get audio device info of %d", card);
	    }
	  devs = (int)(val[0]);
	  /* scan all devices in the card */
	  for (d = 0; d < devs; d++) 
	    {
	      dev = (int)(val[d + 1]);
	      if ((err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(card) | dev, 
					      MUS_AUDIO_DIRECTION, 
					      0, 
					      &direction)) != MUS_NO_ERROR) 
		{
		  snd_warning("can't read direction, ignoring device %d", dev);
		  direction = 0;
		} 
	      else 
		{
		  if ((int)direction == 0) 
		    {
		      /* remember output device */
		      alsa_devices[index++] = MUS_AUDIO_PACK_SYSTEM(card) | dev;
		      if (index >= ALSA_MAX_DEVICES) goto NO_MORE_DEVICES;
		    }
		}
	    }
	}
    NO_MORE_DEVICES:
      /* get channel availability for all devices */
      for (d = 0; d < index; d++) 
	{
	  alsa_available_chans[d] = 0;
	  alsa_min_chans[d] = 0;
	  alsa_max_chans[d] = 0;
	  if ((err = mus_audio_mixer_read(alsa_devices[d], MUS_AUDIO_CHANNEL, 2, val)) == MUS_NO_ERROR) 
	    {
	      alsa_available_chans[d] = (int)(val[0]);
	      alsa_min_chans[d] = (int)(val[1]);
	      alsa_max_chans[d] = (int)(val[2]);
	      if (alsa_max_chans[d] > alsa_max_chans_value) 
		{
		  /* remember widest device */
		  alsa_max_chans_value = alsa_max_chans[d];
		  alsa_max_chans_dev = d;
		}
	    }
	}
    }
#if 0
  if (index == 0)
    snd_warning("can't find any playable device");
  /* this seems to happen all the time? */
#endif
  alsa_devices_available = index;
}

static bool start_audio_output_1(void)
{
  int err;
  int i, d;
  int samples_per_channel = 256;
  float val[ALSA_MAX_DEVICES];
  static int out_dev[ALSA_MAX_DEVICES];
  int alloc_devs = 0;
  int alloc_chans = 0;
  int oss_available_chans = 2;

  /* -------------------- ALSA not OSS -------------------- */
  if (mus_audio_api() == ALSA_API) 
    {
      scan_audio_devices();
      /* allocate devices for playback */
      alloc_chans = 0;
      alloc_devs = 0;
      for (d = 0; d < ALSA_MAX_DEVICES; d++) out_dev[d] = -1; 
      for (d = 0; d < MAX_DEVICES; d++) dev_fd[d] = -1;
      if (feed_first_device == 0) 
	{
	  /* see if widest device can accommodate all channels */
	  if (alsa_max_chans_value >= snd_dacp->channels) 
	    {
	      out_dev[alloc_devs++] = alsa_max_chans_dev;
	      alloc_chans += alsa_max_chans_value;
	    }
	  if (alloc_devs == 0) 
	    {
	      /* try to use several devices */
	      int this_format = -1;
	      int prev_format = -1;
	      for (d = 0; d < alsa_devices_available; d++) 
		{
		  this_format = mus_audio_compatible_format(alsa_devices[d]);
		  if (prev_format == -1) 
		    {
		      prev_format = this_format;
		    }
		  /* format for all selected devices should match */
		  if (this_format == prev_format) 
		    {
		      out_dev[alloc_devs++] = d;
		      alloc_chans += alsa_available_chans[d];
		      if (alloc_devs >= ALSA_MAX_DEVICES)
			break;
		    }
		}
	      if ((alloc_devs != 0) && (alloc_chans < snd_dacp->channels))
		{
		  /* not enough available channels, give up */
		  for (d = 0; d < ALSA_MAX_DEVICES; d++) out_dev[d] = -1;
		  alloc_devs = 0;
		  alloc_chans = 0;
		}
	      if (alloc_devs == 0) 
		{
		  /* fold all channels into the first device */
		  out_dev[alloc_devs++] = 0;
		  alloc_chans += alsa_available_chans[0];
		}
	    }
	}
      else 
	{
	  /* first device on first card is the one */
	  out_dev[alloc_devs++] = 0;
	  alloc_chans += alsa_available_chans[0];
	}
      snd_dacp->out_format = mus_audio_compatible_format(alsa_devices[out_dev[0]]);
      if (alloc_devs < 2) 
	{
	  /* see if we have a minimum sized frame to fill 
	   * FIXME: could this happen in more than one device? */
	  int c;
	  c = alsa_min_chans[out_dev[0]];
	  if (c > snd_dacp->channels) 
	    {
	      snd_dacp->channels = c;
	    }
	}
      /* see if we have to fold channels */
      if (alloc_chans < snd_dacp->channels) 
	{
	  if (dac_combines_channels(ss)) 
	    snd_warning(_("folding %d chans into %d "), 
			snd_dacp->channels, alloc_chans);
	  snd_dacp->channels = alloc_chans;
	}
      /* read the number of samples per channel the device wants buffered */
      if ((err = mus_audio_mixer_read(alsa_devices[out_dev[0]], 
				      MUS_AUDIO_SAMPLES_PER_CHANNEL, 
				      2, val)) != MUS_ERROR) 
	{
	  samples_per_channel = (int)(val[0]);
	}
      snd_dacp->frames = samples_per_channel;
      set_dac_size(snd_dacp->frames * mus_bytes_per_sample(snd_dacp->out_format));
      /* open all allocated devices */
      for (d = 0; d < alloc_devs; d++) 
	{
	  int channels;
	  channels = alsa_available_chans[out_dev[d]];
	  if (alloc_chans <= alsa_available_chans[out_dev[d]]) 
	    {
	      if (snd_dacp->channels < alsa_min_chans[out_dev[d]]) 
		{
		  channels = alsa_min_chans[out_dev[d]];
		} 
	      else 
		{
		  channels = snd_dacp->channels;
		}
	    }
	  /* FIXME: assumes devices are same size... */
	  set_dac_print();
	  dev_fd[d] = mus_audio_open_output(alsa_devices[out_dev[d]], 
					    snd_dacp->srate,
					    channels, 
					    snd_dacp->out_format, 
					    snd_dacp->frames * channels * mus_bytes_per_sample(snd_dacp->out_format));
	  unset_dac_print();
      
	  if (dev_fd[d] == -1) 
	    {
	      /* could not open a device, close all others and quit playback */
	      int i;
	      for (i = 0; i < d; i++) 
		{
		  mus_audio_close(alsa_devices[out_dev[i]]);
		}
	      dac_error();
	      dac_running = false;
	      cleanup_dac_hook();
	      unlock_recording_audio();
	      if (global_rev) free_reverb();
	      max_active_slot = -1;
	      return(false);
	    }
	}
      snd_dacp->devices = alloc_devs;
      /* for now assume all are same number of chans */
      snd_dacp->chans_per_device = (int *)CALLOC(snd_dacp->devices, sizeof(int));
      for (i = 0; i < snd_dacp->devices; i++) 
	snd_dacp->chans_per_device[i] = snd_dacp->channels / snd_dacp->devices;
      make_dac_buffers();
    } 
  else 
    {

      /* -------------------- OSS not ALSA -------------------- */
      /* api == OSS_API */
      if (snd_dacp->channels > 2)
	{
	  err = mus_audio_mixer_read(audio_output_device(ss), MUS_AUDIO_CHANNEL, 0, val);
	  if (err != MUS_ERROR) 
	    {
	      int chans;
	      chans = (int)(val[0]);
	      if (chans > 0)
		oss_available_chans = chans;
	      else snd_warning("open audio output got %d chans?\n", chans);
	    }
	}
      for (i = 0; i < MAX_DEVICES; i++) dev_fd[i] = -1;
      /* see if we can play 16 bit output */
      snd_dacp->out_format = mus_audio_compatible_format(audio_output_device(ss));
#ifndef PPC
      /* check for chans > def chans, open 2 cards if available */
      if ((oss_available_chans < snd_dacp->channels) && (snd_dacp->channels == 4))
	{
	  if (mus_audio_systems() > 1)
	    {
	      set_dac_print();
	      dev_fd[0] = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss), 
						snd_dacp->srate, 2, 
						snd_dacp->out_format, 
						dac_size(ss));
	      unset_dac_print();
	      if (dev_fd[0] != MUS_ERROR) 
		dev_fd[1] = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(1) | audio_output_device(ss), 
						  snd_dacp->srate, 2, 
						  snd_dacp->out_format, 
						  dac_size(ss));
	    }
	  else
	    {
	      /* there is one special case here: Ensoniq's allow you to play quad
	       * by sending two channels (non-clock-synchronous with the other two)
	       * out the line in port, but this possibility confuses LinuxPPC (OSS-Free)
	       */
	      set_dac_print();
	      dev_fd[0] = mus_audio_open_output(MUS_AUDIO_AUX_OUTPUT, 
						snd_dacp->srate, 2, 
						snd_dacp->out_format, 
						dac_size(ss));
	      unset_dac_print();
	      if (dev_fd[0] != MUS_ERROR) 
		dev_fd[1] = mus_audio_open_output(audio_output_device(ss), 
						  snd_dacp->srate, 2, 
						  snd_dacp->out_format,
						  dac_size(ss));
	    }
	  if (dev_fd[1] == MUS_ERROR)
	    {
	      mus_audio_close(dev_fd[0]);
	      dev_fd[0] = MUS_ERROR;
	    }
	  else oss_available_chans = 4;
	}
#endif
      if ((oss_available_chans < snd_dacp->channels) &&
          (oss_available_chans > 0))
 	{
	  if (dac_combines_channels(ss)) 
	    snd_warning(_("folding %d chans into %d "), 
			snd_dacp->channels, oss_available_chans);
	  snd_dacp->channels = oss_available_chans;
	}
      set_dac_print();
      if (dev_fd[0] == MUS_ERROR)
	dev_fd[0] = mus_audio_open_output(audio_output_device(ss), 
					  snd_dacp->srate, snd_dacp->channels, 
					  snd_dacp->out_format, 
					  dac_size(ss));
      unset_dac_print();
      if (dev_fd[0] == MUS_ERROR)
	{
	  dac_error();
	  stop_audio_output();
	  return(false);
	}
      snd_dacp->devices = (dev_fd[1] != -1) ? 2 : 1;
      snd_dacp->chans_per_device = (int *)CALLOC(snd_dacp->devices, sizeof(int));
      for (i = 0; i < snd_dacp->devices; i++) 
	snd_dacp->chans_per_device[i] = snd_dacp->channels / snd_dacp->devices;
      make_dac_buffers();
    }
  return(true);
}
#else /* not ALSA or OSS */

static bool start_audio_output_1(void)
{
  int err;
  int i;
  int available_chans = 2;
  float val[32];
  if (snd_dacp->channels > 2)
    {
      err = mus_audio_mixer_read(audio_output_device(ss), MUS_AUDIO_CHANNEL, 0, val);
      if (err != MUS_ERROR) 
	available_chans = (int)(val[0]);
      else 
	{
	  stop_playing_all_sounds(PLAY_ERROR);
	  snd_error(_("can't get audio output chans? (%d) "), audio_output_device(ss));
	  return(false);
	}
    }
  for (i = 0; i < MAX_DEVICES; i++) dev_fd[i] = -1;
  snd_dacp->out_format = MUS_AUDIO_COMPATIBLE_FORMAT;

#ifdef SUN
  /* can audio hardware handle this data format? */
  err = mus_audio_mixer_read(audio_output_device(ss), MUS_AUDIO_FORMAT, 32, val);
  {
    bool happy = false;
    for (i = 1; i <= (int)(val[0]); i++)
      if ((int)(val[i]) == MUS_AUDIO_COMPATIBLE_FORMAT)
	{
	  happy = true;
	  break;
	}
    if (!happy)
      {
	int max_bits = -1, best_fit = 0;
	for (i = 1; i <= (int)(val[0]); i++)
	  if (mus_bytes_per_sample((int)(val[i])) > max_bits)
	    {
	      best_fit = (int)(val[i]);
	      max_bits = mus_bytes_per_sample(best_fit); /* probably mus-mulaw is all we get */
	    }
	if (max_bits == -1)
	  {
	    snd_warning_without_format("can't find any playable data format!");
	    return(false);
	  }
	snd_dacp->out_format = best_fit;
      }
  }
  /* but there's still trouble -- on opteron solaris 10, the only playable srate is 48000, but the mixer lies about it! */
  /*   I can't see what to do in this case */
#endif
  if (available_chans <= 0)
    {
      snd_warning("no available channels??");
      return(false);
    }
  if (available_chans < snd_dacp->channels) 
    {
      if (dac_combines_channels(ss)) 
	snd_warning(_("folding %d chans into %d "), 
		    snd_dacp->channels, 
		    available_chans);
      snd_dacp->channels = available_chans;
    }
  set_dac_print();
  if (dev_fd[0] == MUS_ERROR)
    dev_fd[0] = mus_audio_open_output(audio_output_device(ss), 
				      snd_dacp->srate, 
				      snd_dacp->channels, 
				      snd_dacp->out_format, 
				      dac_size(ss));
  unset_dac_print();
  if (dev_fd[0] == MUS_ERROR)
    {
      dac_error();
      stop_audio_output();
      return(false);
    }
  snd_dacp->devices = 1;
  snd_dacp->chans_per_device = (int *)CALLOC(snd_dacp->devices, sizeof(int));
  for (i = 0; i < snd_dacp->devices; i++) 
    snd_dacp->chans_per_device[i] = available_chans / snd_dacp->devices;
  make_dac_buffers();
  return(true);
}
#endif

static bool start_audio_output(void)
{
  /* at this point the desired output srate and chans are set in dacp (via start_dac) */
  dac_info *dp;
  int i;
  cursor_time = 0;
  lock_recording_audio();
  if (start_audio_output_1()) /* number of channels may be less than requested initially */
    {
      for (i = 0; i <= max_active_slot; i++)
	{
	  dp = play_list[i];
	  if (dp)
	    {
	      /* deferred reverb allocation since start_audio_output_1 may force more chans open */
	      if ((dp->reverbing) && 
		  (dp->sp) && 
		  (global_rev == NULL))
		make_reverb(dp->sp, snd_dacp->channels);

	      if (dp->audio_chan >= snd_dacp->channels)
		{
		  if (dac_combines_channels(ss))
		    dp->audio_chan %= snd_dacp->channels;
		  else stop_playing(dp, WITHOUT_HOOK, PLAY_NO_CHANNEL);
		}
	    }
	}
      dac_running = true;
      fill_dac_buffers(WRITE_TO_DAC);
      if (!snd_dacp) return(false);
      return(true);
    }
  return(false);
}
 
static void stop_audio_output(void)
{
   int i;
   for (i = 0; i < MAX_DEVICES; i++)
     if (dev_fd[i] != -1) 
       {
	 mus_audio_close(dev_fd[i]);
	 dev_fd[i] = -1;
       }
   dac_running = false;
   cleanup_dac_hook();
   unlock_recording_audio();
   dac_pausing = false;
   if (global_rev) free_reverb();
   max_active_slot = -1;
}

static Cessate dac_in_background(Indicium ptr)
{
  /* slice 0: try to open audio output devices and get ready to send samples
   *       1: loop sending data until the play_list is empty or some error (C-g) happens
   *       2: try to close all active outputs and remove background procedure
   */
  if (snd_dacp == NULL) return(BACKGROUND_QUIT);
  switch (snd_dacp->slice)
    {
    case 0:
      if (start_audio_output())
	{
	  if (snd_dacp == NULL) return(BACKGROUND_QUIT);
	  snd_dacp->slice = 1;
	  return(BACKGROUND_CONTINUE);
	}
      else 
	{
	  free_dac_state();
	  return(BACKGROUND_QUIT);
	}
      break;
    case 1:
      fill_dac_buffers(WRITE_TO_DAC);
      if (snd_dacp == NULL) return(BACKGROUND_QUIT); /* dac-hook called stop-playing or something equally perverse */
      if ((global_rev == NULL) && (play_list_members == 0)) snd_dacp->slice = 2;
      return(BACKGROUND_CONTINUE);
      break;
     case 2:
       stop_audio_output();
       free_dac_state();
#if USE_GTK
       /* in gtk:
	*   (play)
	*   (apply-controls) ; while playing
	*   -> segfault and many other weird problems
	* This bug appears to be in gtk -- if a background function returns false, it is not supposed to be called 
	* again.  But for some reason gtk continues to call dac_in_background after I return false.  I haven't 
	* tracked this down in the glib sources (glib/gmain.c) -- I suspect that they're "destroying" the "source"
	* (in their jargon) before checking whether to remove the function from the poll list.  Or possibly,
	* I'm destroying it somewhere -- in any case, I'll stop the thing by hand.
	*/
       if (dac_work_proc)
 	 {
 	   BACKGROUND_REMOVE(dac_work_proc);
 	   dac_work_proc = 0;
 	 }
#endif
       return(BACKGROUND_QUIT);
       break;
     }
  return(BACKGROUND_QUIT);
}
 

/* ---------------- support for Apply button (snd-apply.c) ---------------- */

void initialize_apply(snd_info *sp, int chans, off_t beg, off_t dur)
{
  int curchan = 0;
  stop_playing_all_sounds_without_hook(PLAY_APPLY);
  /* see note above about gtk and work procs */
  if (snd_dacp)                             /* may not have been able to complete despite slice=2 in stop all */
    {
      snd_dacp->slice = 2;
      dac_in_background(NULL);              /* so try to force completion */
    }
  if (chans <= 0) return;
  max_active_slot = -1;
  play_list_members = 0;
  dac_running = true; /* this keeps start_dac from actually starting the dac */
  if (snd_dacp) free_dac_state();
  snd_dacp = (dac_state *)CALLOC(1, sizeof(dac_state));
  snd_dacp->slice = 0;
  snd_dacp->srate = SND_SRATE(sp);
  snd_dacp->out_format = MUS_AUDIO_COMPATIBLE_FORMAT;
  if (snd_dacp->srate <= 0) snd_dacp->srate = 44100;
  snd_dacp->channels = chans;
  snd_dacp->frames = 8192;
  snd_dacp->devices = 1;
  snd_dacp->chans_per_device = (int *)CALLOC(1, sizeof(int));
  snd_dacp->chans_per_device[0] = chans;
  snd_dacp->reverb_ring_frames = (off_t)(snd_dacp->srate * sp->reverb_control_decay);
  make_dac_buffers();
  switch (ss->apply_choice)
    {
    case APPLY_TO_SOUND: 
      play_sound(sp, beg, beg + dur); 
      break;
    case APPLY_TO_SELECTION: 
      play_selection(IN_BACKGROUND); 
      break;
    case APPLY_TO_CHANNEL: 
      if (sp->selected_channel != NO_SELECTION)
	curchan = sp->selected_channel;
      play_channel(sp->chans[curchan], beg, beg + dur); 
      break;
    }
}

void finalize_apply(snd_info *sp)
{
  /* if no reverb, these need to be cleaned up */
  stop_playing_all_sounds_without_hook(PLAY_APPLY);
  max_active_slot = -1;
  play_list_members = 0;
  sp->playing = 0;
  dac_running = false;
  cleanup_dac_hook();
  if (snd_dacp) free_dac_state();
  if (global_rev) free_reverb();
}

int run_apply(int ofd)
{
  int len, chans;
  if (!(snd_dacp)) return(-1);
  chans = snd_dacp->channels; /* snd_dacp might be freed by fill_dac_buffers */
  len = fill_dac_buffers(WRITE_TO_FILE);
  mus_file_write(ofd, 0, len - 1, chans, dac_buffers);
  return(len);
}


/* -------------------------------- scheme connection -------------------------------- */

static XEN g_play_1(XEN samp_n, XEN snd_n, XEN chn_n, bool back, bool syncd, XEN end_n, XEN edpos, 
		    const char *caller, int arg_pos, XEN stop_proc, XEN out_chan)
{
  /* all chans if chn_n omitted, arbitrary file if snd_n is name */
  snd_info *sp;
  chan_info *cp = NULL;
  sync_info *si = NULL;
  static char *play_name = NULL;
  int i;
  off_t samp = 0;
  off_t end = NO_END_SPECIFIED;
  off_t *ends = NULL;
  play_process_t background;
  if (XEN_OFF_T_P(end_n)) end = XEN_TO_C_OFF_T(end_n);
#if USE_NO_GUI
  background = NOT_IN_BACKGROUND;
#else
  if (back) background = IN_BACKGROUND; else background = NOT_IN_BACKGROUND;
#endif
  XEN_ASSERT_TYPE(((XEN_PROCEDURE_P(stop_proc)) && (procedure_arity_ok(stop_proc, 1))) ||
		  (XEN_NOT_BOUND_P(stop_proc)) || 
		  (XEN_FALSE_P(stop_proc)), 
		  stop_proc, arg_pos + 1, caller, "a procedure of 1 arg");
  if (play_name) {FREE(play_name); play_name = NULL;}

  /* if even samp_n is XEN_UNDEFINED, start_dac? */

  if (XEN_STRING_P(samp_n))
    {
      /* filename beg end background syncd ignored */
      samp = beg_to_sample(snd_n, caller);
      if (samp < 0) XEN_ERROR(NO_SUCH_SAMPLE,
			      XEN_LIST_2(C_TO_XEN_STRING(caller),
					 snd_n));

      play_name = mus_expand_filename(XEN_TO_C_STRING(samp_n));

      if (!(mus_file_probe(play_name)))
	return(snd_no_such_file_error(caller, samp_n));

      if (!(MUS_HEADER_TYPE_OK(mus_sound_header_type(play_name))))
	XEN_ERROR(BAD_HEADER,
		  XEN_LIST_3(C_TO_XEN_STRING(caller),
			     samp_n, 
			     C_TO_XEN_STRING(mus_header_type_name(mus_header_type()))));

      if (!(MUS_DATA_FORMAT_OK(mus_sound_data_format(play_name))))
	XEN_ERROR(XEN_ERROR_TYPE("bad-format"),
		  XEN_LIST_3(C_TO_XEN_STRING(caller),
			     samp_n, 
			     C_TO_XEN_STRING(mus_header_original_format_name(mus_sound_original_format(play_name),
									     mus_sound_header_type(play_name)))));
      sp = make_sound_readable(play_name, false);
      sp->short_filename = filename_without_directory(play_name);
      sp->filename = NULL;
      sp->delete_me = (struct dialog_play_info *)1;
      if (XEN_OFF_T_P(chn_n)) end = XEN_TO_C_OFF_T(chn_n);
      play_sound_1(sp, samp, end, background, edpos, stop_proc, caller, arg_pos);
    }
  else
    {
      XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samp_n), samp_n, XEN_ARG_1, caller, "a number");
      ASSERT_CHANNEL(caller, snd_n, chn_n, 2);
      samp = beg_to_sample(samp_n, caller);
      if (samp < 0) XEN_ERROR(NO_SUCH_SAMPLE,
			      XEN_LIST_2(C_TO_XEN_STRING(caller),
					 samp_n));
      sp = get_sp(snd_n, PLAYERS_OK);
      if (sp == NULL) 
	return(snd_no_such_sound_error(caller, snd_n));
      if ((syncd) && (sp->sync != 0) && (!(IS_PLAYER(sp))))
	{
	  si = snd_sync(sp->sync);
	  if (end != NO_END_SPECIFIED)
	    {
	      ends = (off_t *)CALLOC(si->chans, sizeof(off_t));
	      for (i = 0; i < si->chans; i++) ends[i] = end;
	    }
	  for (i = 0; i < si->chans; i++) si->begs[i] = samp;
	  play_channels_1(si->cps, si->chans, si->begs, ends, background, edpos, false, stop_proc, caller, arg_pos);
	  si = free_sync_info(si);
	  if (ends) FREE(ends);
	}
      else
	{
	  if (!(XEN_INTEGER_P(chn_n)))
	    play_sound_1(sp, samp, end, background, edpos, stop_proc, caller, arg_pos);
	  else 
	    {
	      int ochan = -1, pos;
	      cp = get_cp(snd_n, chn_n, caller);
	      XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(out_chan), out_chan, arg_pos + 2, caller, "an integer");
	      if (XEN_INTEGER_P(out_chan)) ochan = XEN_TO_C_INT(out_chan);
	      if (ochan < 0) ochan = cp->chan;
	      pos = to_c_edit_position(cp, edpos, caller, arg_pos);
	      if (cp) play_channel_1(cp, samp, end, background, pos, stop_proc, ochan);
	    }
	}
    }
  return(XEN_FALSE);
}

#define TO_C_BOOLEAN_OR_FALSE(a) (XEN_TRUE_P(a) || ((XEN_INTEGER_P(a)) && (XEN_TO_C_INT(a) == 1)))

static XEN g_play(XEN samp_n, XEN snd_n, XEN chn_n, XEN syncd, XEN end_n, XEN edpos, XEN stop_proc, XEN out_chan) 
{
  #if HAVE_SCHEME
    #define play_example "(play \"oboe.snd\")"
  #endif
  #if HAVE_RUBY
    #define play_example "play(\"oboe.snd\")"
  #endif
  #if HAVE_FORTH
    #define play_example "\"oboe.snd\" play"
  #endif

  #define H_play "(" S_play " :optional (start 0) snd chn sync end (pos -1) stop-proc out-chan): play snd or snd's channel chn starting at start. \
'start' can also be a filename: " play_example ".  If 'sync' is true, all sounds syncd to snd are played. \
If 'end' is not given, " S_play " plays to the end of the sound.  If 'pos' is -1 or not given, the current edit position is \
played."

  return(g_play_1(samp_n, snd_n, chn_n, true, TO_C_BOOLEAN_OR_FALSE(syncd), end_n, edpos, S_play, 6, stop_proc, out_chan));
}

static XEN g_play_channel(XEN beg, XEN dur, XEN snd_n, XEN chn_n, XEN edpos, XEN stop_proc, XEN out_chan) 
{
  #define H_play_channel "(" S_play_channel " :optional (beg 0) (dur len) snd chn (pos -1) stop-proc out-chan): \
play snd or snd's channel chn starting at beg for dur samps."
  XEN end = XEN_FALSE;
  if (XEN_INTEGER_P(dur))
    {
      off_t len;
      len = XEN_TO_C_OFF_T(dur);
      if (len <= 0) return(XEN_FALSE);
      end = C_TO_XEN_OFF_T(beg_to_sample(beg, S_play_channel) + len);
    }
  return(g_play_1(beg, snd_n, chn_n, true, false, end, edpos, S_play_channel, 5, stop_proc, out_chan));
}

static XEN g_play_selection(XEN wait, XEN stop_proc) 
{
  #define H_play_selection "(" S_play_selection " :optional wait stop-proc): play the selection. \
'wait', if " PROC_TRUE ", causes " S_play_selection " to wait until the playing is finished \
before returning."
  bool back;
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(wait), wait, XEN_ARG_1, S_play_selection, "a boolean");
  XEN_ASSERT_TYPE(((XEN_PROCEDURE_P(stop_proc)) && (procedure_arity_ok(stop_proc, 1))) ||
		  (XEN_NOT_BOUND_P(stop_proc)) || 
		  (XEN_FALSE_P(stop_proc)), 
		  stop_proc, XEN_ARG_3, S_play_selection, "a procedure of 1 arg");
  back = (!(TO_C_BOOLEAN_OR_FALSE(wait)));
  if (selection_is_active())
    {
      play_selection_1((back) ? IN_BACKGROUND : NOT_IN_BACKGROUND, stop_proc);
      return(XEN_FALSE);
    }
  return(snd_no_active_selection_error(S_play_selection));
}

static XEN g_play_and_wait(XEN samp_n, XEN snd_n, XEN chn_n, XEN syncd, XEN end_n, XEN edpos, XEN stop_proc, XEN out_chan) 
{
  #if HAVE_SCHEME
    #define play_and_wait_example "(play-and-wait \"oboe.snd\")"
  #endif
  #if HAVE_RUBY
    #define play_and_wait_example "play_and_wait(\"oboe.snd\")"
  #endif
  #if HAVE_FORTH
    #define play_and_wait_example "\"oboe.snd\" play-and-wait"
  #endif

  #define H_play_and_wait "(" S_play_and_wait " (start 0) snd chn syncd end (pos -1) stop-proc out-chan): \
play snd or snd's channel chn starting at start \
and waiting for the play to complete before returning.  'start' can also be a filename:\n  " play_and_wait_example

  return(g_play_1(samp_n, snd_n, chn_n, false, TO_C_BOOLEAN_OR_FALSE(syncd), end_n, edpos, S_play_and_wait, 6, stop_proc, out_chan));
}

static XEN g_stop_playing(XEN snd_n)
{
  #define H_stop_playing "(" S_stop_playing " :optional snd): stop play (DAC output) in progress"
  snd_info *sp = NULL;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(snd_n), snd_n, XEN_ONLY_ARG, S_stop_playing, "an integer");
  if (XEN_INTEGER_P(snd_n)) sp = get_sp(snd_n, PLAYERS_OK);
  if (sp) 
    stop_playing_sound(sp, PLAY_STOP_CALLED); 
  else stop_playing_all_sounds(PLAY_STOP_CALLED);
  return(XEN_FALSE);
}


/* -------- players -------- */

static snd_info **players = NULL;
static int *player_chans = NULL;
static int players_size = 0;

static int new_player_index(void)
{
  int i, old_size;
  if (players_size == 0)
    {
      players_size = 8;
      players = (snd_info **)CALLOC(players_size, sizeof(snd_info *));
      player_chans = (int *)CALLOC(players_size, sizeof(int));
      return(-1);
    }
  for (i = 1; i < players_size; i++)
    if (players[i] == NULL)
      return(-i);
  old_size = players_size;
  players_size += 8;
  players = (snd_info **)REALLOC(players, players_size * sizeof(snd_info *));
  player_chans = (int *)REALLOC(player_chans, players_size * sizeof(int));
  for (i = old_size; i < players_size; i++)
    {
      players[i] = NULL;
      player_chans[i] = 0;
    }
  return(-old_size);
}

static int make_player(snd_info *sp, chan_info *cp)
{
  /* store sp so we can access it via find_sound (get_sp) later */
  players[PLAYER(sp)] = sp;
  player_chans[PLAYER(sp)] = cp->chan;
  return(sp->index);
}

snd_info *player(int index)
{
  if ((players) && (index < 0) && ((-index) < players_size))
    return(players[-index]);
  return(NULL);
}

static void free_player(snd_info *sp)
{
  if (players)
    {
      players[PLAYER(sp)] = NULL;
      player_chans[PLAYER(sp)] = 0;
    }
  FREE(sp->filename);
  sp->filename = NULL;
  FREE(sp->chans);
  sp->chans = NULL;
  if (sp->filter_control_envelope) sp->filter_control_envelope = free_env(sp->filter_control_envelope);
  sp->inuse = SOUND_IDLE;
  FREE(sp);
}

void clear_players(void)
{
  /* called only in free_snd_info, snd-data.c -- make sure currently closing sound is not playing (via a user-created player) */
  int i;
  for (i = 0; i < players_size; i++)
    {
      int j;
      snd_info *sp;
      sp = players[i];
      if (sp)
	for (j = 0; j < sp->nchans; j++)
	  if ((sp->chans[j] == NULL) ||
	      (!(sp->chans[j]->active)) ||
	      (sp->chans[j]->sound == NULL))
	    {
	      int k;
	      for (k = 0; k <= max_active_slot; k++)
		{
		  dac_info *dp;
		  dp = play_list[k];
		  if ((dp) && (sp == dp->sp))
		    {
		      play_list[k] = NULL;
		      play_list_members--;
		      free_dac_info(dp, PLAY_CLOSE); /* calls stop-function, if any. used in snd-data.c in free_snd_info */
		    }
		}
	      if (IS_PLAYER(sp)) free_player(sp);
	      break;
	    }
    }
}

/* add-player make-player stop-player start-playing */

static XEN snd_no_such_player_error(const char *caller, XEN index)
{
  XEN_ERROR(XEN_ERROR_TYPE("no-such-player"),
	    XEN_LIST_2(C_TO_XEN_STRING(caller),
		       index));
  return(XEN_FALSE);
}

static XEN g_make_player(XEN snd, XEN chn)
{
  #define H_make_player "(" S_make_player " :optional snd chn): \
make a new player associated with snd's channel chn. \
A player is a sort of wrapper for a channel of a sound that supports \
all the control panel functions.  Once created, you can set these \
fields, then call " S_add_player " to add this channel to the list of \
channels either being played (if a play is in progress) or about \
to be played (via " S_start_playing ")."

  snd_info *true_sp, *new_sp;
  chan_info *cp;
  ASSERT_CHANNEL(S_make_player, snd, chn, 1);
  true_sp = get_sp(snd, NO_PLAYERS);
  if (true_sp == NULL) 
    return(snd_no_such_sound_error(S_make_player, snd));
  cp = get_cp(snd, chn, S_make_player);
  if (cp == NULL) return(XEN_FALSE); /* won't happen */
  new_sp = make_snd_info(NULL, "make_player:wrapper", true_sp->hdr, new_player_index(), true);
  FREE(new_sp->sgx); /* no built-in GUI */
  new_sp->sgx = NULL;
  new_sp->chans[cp->chan] = cp;
  return(C_TO_XEN_INT(make_player(new_sp, cp)));
}

static XEN g_player_home(XEN snd_chn)
{
  #define H_player_home "(" S_player_home " player): a list of the sound index and channel number associated with player"
  int index;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(snd_chn), snd_chn, XEN_ONLY_ARG, S_player_home, "an integer");
  index = -XEN_TO_C_INT(snd_chn);
  if ((index > 0) && 
      (index < players_size) && 
      (players[index]) &&
      (players[index]->chans) &&
      (player_chans[index] < players[index]->nchans))
    {
      chan_info *cp;
      cp = players[index]->chans[player_chans[index]]; /* trying to get back to the original sound index (not the player index) */
      if ((cp->sound) && (cp->sound->active))
	return(XEN_LIST_2(C_TO_XEN_INT(cp->sound->index),
			  C_TO_XEN_INT(cp->chan)));
      else return(XEN_FALSE); /* can this happen? */
    }
  return(snd_no_such_player_error(S_player_home, snd_chn));
}

static XEN g_add_player(XEN snd_chn, XEN start, XEN end, XEN edpos, XEN stop_proc, XEN out_chan)
{
  #define H_add_player "(" S_add_player " player :optional (start 0) (end len) (pos -1) stop-proc (out-chan player-chan)): \
add a player to the play list; the play begins when " S_start_playing " is called. \
The start, end, and edit-position of the portion played can be specified.  'out-chan' is \
the audio hardware output channel to use to play this channel.  It defaults to the \
channel number in the sound that contains the channel being played."

  snd_info *sp = NULL;
  int index, pos;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(snd_chn), snd_chn, XEN_ARG_1, S_add_player, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(start), start, XEN_ARG_2, S_add_player, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(end), end, XEN_ARG_3, S_add_player, "a number");
  XEN_ASSERT_TYPE(((XEN_PROCEDURE_P(stop_proc)) && (procedure_arity_ok(stop_proc, 1))) ||
		  (XEN_NOT_BOUND_P(stop_proc)) || 
		  (XEN_FALSE_P(stop_proc)), 
		  stop_proc, XEN_ARG_5, S_add_player, "a procedure of 1 arg");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(out_chan), out_chan, XEN_ARG_6, S_add_player, "an integer");
  index = -XEN_TO_C_INT(snd_chn);
  if ((index > 0) && (index < players_size)) sp = players[index];
  if (sp)
    {
      chan_info *cp;
      dac_info *dp = NULL;
      int i, ochan = -1;
      if (play_list)
	for (i = 0; i < dac_max_sounds; i++)
	  if ((play_list[i]) && 
	      (sp == (play_list[i]->sp)))
	    XEN_ERROR(XEN_ERROR_TYPE("arg-error"),
		      XEN_LIST_3(C_TO_XEN_STRING(S_add_player),
				 C_TO_XEN_STRING("player is already in the play list"),
				 snd_chn));
      cp = sp->chans[player_chans[index]];
      pos = to_c_edit_position(cp, edpos, S_add_player, XEN_ARG_4);
      if (XEN_INTEGER_P(out_chan)) ochan = XEN_TO_C_INT(out_chan);
      if (ochan < 0) ochan = cp->chan;
      dp = add_channel_to_play_list(cp,
				    sp, /* this is not cp->sound! */
				    beg_to_sample(start, S_add_player),
				    XEN_TO_C_OFF_T_OR_ELSE(end, NO_END_SPECIFIED),
				    pos,
				    ochan);
      if (dp == NULL) return(XEN_FALSE);
      dp->stop_procedure = stop_proc;
      if (XEN_PROCEDURE_P(stop_proc))
	dp->stop_procedure_gc_loc = snd_protect(stop_proc);
    }
  else return(snd_no_such_player_error(S_add_player, snd_chn));
  return(snd_chn);
}

static XEN g_start_playing(XEN Chans, XEN Srate, XEN In_Background)
{
  #define H_start_playing "(" S_start_playing " :optional (chans 1) (srate 44100) (in-background " PROC_TRUE ")): \
If a play-list is waiting, start it."

  int chans, srate;
  bool back;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(Chans), Chans, XEN_ARG_1, S_start_playing, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(Srate), Srate, XEN_ARG_2, S_start_playing, "a number");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(In_Background), In_Background, XEN_ARG_3, S_start_playing, "a boolean");
  chans = XEN_TO_C_INT_OR_ELSE(Chans, 1);
  if ((chans <= 0) || (chans > 256))
    XEN_OUT_OF_RANGE_ERROR(S_start_playing, 1, Chans, "chans ~A <= 0 or > 256?");
  srate = XEN_TO_C_INT_OR_ELSE(Srate, 44100);
  if (srate <= 0)
    XEN_OUT_OF_RANGE_ERROR(S_start_playing, 2, Srate, "srate ~A <= 0?");
  back = XEN_TO_C_BOOLEAN(In_Background);
  start_dac(srate, chans, (back) ? IN_BACKGROUND : NOT_IN_BACKGROUND, DEFAULT_REVERB_CONTROL_DECAY);
  return(XEN_FALSE);
}

static XEN g_stop_player(XEN snd_chn)
{
  #define H_stop_player "(" S_stop_player " player): stop player (remove its associated sound from the current DAC playlist)"
  int index;
  snd_info *sp = NULL;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(snd_chn), snd_chn, XEN_ONLY_ARG, S_stop_player, "an integer");
  index = -XEN_TO_C_INT(snd_chn);
  if ((index > 0) && (index < players_size)) sp = players[index];
  if (sp) 
    stop_playing_sound(sp, PLAY_STOP_CALLED);
  else return(snd_no_such_player_error(S_stop_player, snd_chn));
  return(snd_chn);
}

static XEN g_free_player(XEN snd_chn)
{
  #define H_free_player "(" S_free_player " player): free all resources associated with 'player' and remove it from the current DAC playlist)"
  int index;
  snd_info *sp = NULL;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(snd_chn), snd_chn, XEN_ONLY_ARG, S_free_player, "an integer");
  index = -XEN_TO_C_INT(snd_chn);
  if ((index > 0) && (index < players_size)) sp = players[index];
  if (sp) 
    free_player(sp);
  else return(snd_no_such_player_error(S_free_player, snd_chn));
  return(XEN_FALSE);
}

/* also the dac filler needs to run on empty buffers in this case? */

static XEN g_player_p(XEN snd_chn)
{
  #define H_player_p "(" S_player_p " obj): is 'obj' an active player"
  int index;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(snd_chn), snd_chn, XEN_ONLY_ARG, S_player_p, "an integer");
  index = -XEN_TO_C_INT(snd_chn);
  return(C_TO_XEN_BOOLEAN((index > 0) && 
			  (index < players_size) && 
			  (players[index])));
}

/* player-position? -- need quick way from index to dp to its sample-reader, then C_TO_XEN_OFF_T(current_location(fd)) */

static XEN g_players(void)
{
  #define H_players "(" S_players ") -> list of currently active players."
  XEN lst = XEN_EMPTY_LIST;
  int i;
  for (i = 0; i < players_size; i++)
    if (players[i])
      lst = XEN_CONS(C_TO_XEN_INT(-i), lst);
  return(lst);
}

static XEN g_dac_size(void) {return(C_TO_XEN_INT(dac_size(ss)));}
static XEN g_set_dac_size(XEN val) 
{
  #define H_dac_size "(" S_dac_size "): the current DAC buffer size in frames (256)"
  int len;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_dac_size, "a number");
  len = XEN_TO_C_INT_OR_ELSE(val, 0);
#if 0
#if MUS_MAC_OSX
  if (len > 256) len = 256;
  /* actual built-in buffer seems to be 4096, and with auto-conversions, that means 512 or greater can overflow. 
     mulaw 22050 mono -> float 44100 stereo => 16:1 expansion
   */
#endif
#endif
  if (len > 0)
    set_dac_size(len); /* macro in snd-0.h */
  return(C_TO_XEN_INT(dac_size(ss)));
}

static XEN g_dac_combines_channels(void) {return(C_TO_XEN_BOOLEAN(dac_combines_channels(ss)));}
static XEN g_set_dac_combines_channels(XEN val) 
{
  #define H_dac_combines_channels "(" S_dac_combines_channels "): " PROC_TRUE " if extra channels are to be mixed into available ones during playing. \
That is, if the sound to be played has 4 channels, but the DAC can only handle 2, if this \
variable is " PROC_TRUE ", the extra channels are mixed into the available ones; otherwise they are ignored."

  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_dac_combines_channels, "a boolean");
  set_dac_combines_channels(XEN_TO_C_BOOLEAN(val)); 
  return(C_TO_XEN_BOOLEAN(dac_combines_channels(ss)));
}

static XEN g_playing(void) 
{
  #define H_playing "(" S_playing "): " PROC_TRUE " if sound output is in progress.  If players are waiting to start, \
setting this to " PROC_TRUE " starts them; setting it to " PROC_FALSE " stops output."
  return(C_TO_XEN_BOOLEAN(playing()));
}

static XEN g_set_playing(XEN on)
{
  bool starting = false;
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ONLY_ARG, S_setB S_playing, "a boolean");
  starting = XEN_TO_C_BOOLEAN(on);
  if (starting)
    start_dac((int)mus_srate(), 1, IN_BACKGROUND, DEFAULT_REVERB_CONTROL_DECAY); /* how to get plausible srate chans here? */
  else stop_playing_all_sounds(PLAY_STOP_CALLED);
  return(on);
}

static XEN g_pausing(void)
{
  #define H_pausing "(" S_pausing "): " PROC_TRUE " if sound output is currently pausing (settable."
  return(C_TO_XEN_BOOLEAN(dac_pausing));
}

static XEN g_set_pausing(XEN pause)
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(pause), pause, XEN_ONLY_ARG, S_setB S_pausing, "a boolean");
  dac_pausing = XEN_TO_C_BOOLEAN(pause);
  play_button_pause(dac_pausing);
  return(pause);
}

static XEN g_cursor_update_interval(void) {return(C_TO_XEN_DOUBLE(cursor_update_interval(ss)));}
static XEN g_set_cursor_update_interval(XEN val) 
{
  Float ctime;
  #define H_cursor_update_interval "(" S_cursor_update_interval "): time (seconds) between cursor updates if " S_with_tracking_cursor "."
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_cursor_update_interval, "a number"); 
  ctime = XEN_TO_C_DOUBLE(val);
  if ((ctime < 0.0) || (ctime > (24 * 3600)))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_cursor_update_interval, 1, val, "~A: invalid time");
  set_cursor_update_interval(ctime);
  return(C_TO_XEN_DOUBLE(cursor_update_interval(ss)));
}

static XEN g_cursor_location_offset(void) {return(C_TO_XEN_INT(cursor_location_offset(ss)));}
static XEN g_set_cursor_location_offset(XEN val) 
{
  int ctime;
  #define H_cursor_location_offset "(" S_cursor_location_offset "): samples added to cursor location if cursor displayed during play."
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_cursor_location_offset, "a number"); 
  ctime = XEN_TO_C_INT(val);
  set_cursor_location_offset(ctime);
  return(C_TO_XEN_INT(cursor_location_offset(ss)));
}


#ifdef XEN_ARGIFY_1
XEN_ARGIFY_8(g_play_w, g_play)
XEN_ARGIFY_7(g_play_channel_w, g_play_channel)
XEN_ARGIFY_2(g_play_selection_w, g_play_selection)
XEN_ARGIFY_8(g_play_and_wait_w, g_play_and_wait)
XEN_ARGIFY_1(g_stop_playing_w, g_stop_playing)
XEN_ARGIFY_2(g_make_player_w, g_make_player)
XEN_ARGIFY_6(g_add_player_w, g_add_player)
XEN_NARGIFY_1(g_player_home_w, g_player_home)
XEN_ARGIFY_3(g_start_playing_w, g_start_playing)
XEN_NARGIFY_1(g_stop_player_w, g_stop_player)
XEN_NARGIFY_1(g_free_player_w, g_free_player)
XEN_NARGIFY_0(g_players_w, g_players)
XEN_NARGIFY_1(g_player_p_w, g_player_p)
XEN_NARGIFY_0(g_dac_size_w, g_dac_size)
XEN_NARGIFY_1(g_set_dac_size_w, g_set_dac_size)
XEN_NARGIFY_0(g_dac_combines_channels_w, g_dac_combines_channels)
XEN_NARGIFY_1(g_set_dac_combines_channels_w, g_set_dac_combines_channels)
XEN_NARGIFY_0(g_playing_w, g_playing)
XEN_NARGIFY_1(g_set_playing_w, g_set_playing)
XEN_NARGIFY_0(g_pausing_w, g_pausing)
XEN_NARGIFY_1(g_set_pausing_w, g_set_pausing)
XEN_NARGIFY_0(g_cursor_update_interval_w, g_cursor_update_interval)
XEN_NARGIFY_1(g_set_cursor_update_interval_w, g_set_cursor_update_interval)
XEN_NARGIFY_0(g_cursor_location_offset_w, g_cursor_location_offset)
XEN_NARGIFY_1(g_set_cursor_location_offset_w, g_set_cursor_location_offset)
#else
#define g_play_w g_play
#define g_play_channel_w g_play_channel
#define g_play_selection_w g_play_selection
#define g_play_and_wait_w g_play_and_wait
#define g_stop_playing_w g_stop_playing
#define g_make_player_w g_make_player
#define g_add_player_w g_add_player
#define g_player_home_w g_player_home
#define g_start_playing_w g_start_playing
#define g_stop_player_w g_stop_player
#define g_free_player_w g_free_player
#define g_players_w g_players
#define g_player_p_w g_player_p
#define g_dac_size_w g_dac_size
#define g_set_dac_size_w g_set_dac_size
#define g_dac_combines_channels_w g_dac_combines_channels
#define g_set_dac_combines_channels_w g_set_dac_combines_channels
#define g_playing_w g_playing
#define g_set_playing_w g_set_playing
#define g_pausing_w g_pausing
#define g_set_pausing_w g_set_pausing
#define g_cursor_update_interval_w g_cursor_update_interval
#define g_set_cursor_update_interval_w g_set_cursor_update_interval
#define g_cursor_location_offset_w g_cursor_location_offset
#define g_set_cursor_location_offset_w g_set_cursor_location_offset
#endif

void g_init_dac(void)
{
  XEN_DEFINE_PROCEDURE(S_play,           g_play_w,           0, 8, 0, H_play);
  XEN_DEFINE_PROCEDURE(S_play_channel,   g_play_channel_w,   0, 7, 0, H_play_channel);
  XEN_DEFINE_PROCEDURE(S_play_selection, g_play_selection_w, 0, 2, 0, H_play_selection);
  XEN_DEFINE_PROCEDURE(S_play_and_wait,  g_play_and_wait_w,  0, 8, 0, H_play_and_wait);
  XEN_DEFINE_PROCEDURE(S_stop_playing,   g_stop_playing_w,   0, 1, 0, H_stop_playing);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_pausing, g_pausing_w, H_pausing, S_setB S_pausing, g_set_pausing_w, 0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_playing, g_playing_w, H_playing, S_setB S_playing, g_set_playing_w, 0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE(S_make_player,    g_make_player_w,    0, 2, 0, H_make_player);
  XEN_DEFINE_PROCEDURE(S_add_player,     g_add_player_w,     1, 5, 0, H_add_player);
  XEN_DEFINE_PROCEDURE(S_player_home,    g_player_home_w,    1, 0, 0, H_player_home);
  XEN_DEFINE_PROCEDURE(S_start_playing,  g_start_playing_w,  0, 3, 0, H_start_playing);
  XEN_DEFINE_PROCEDURE(S_stop_player,    g_stop_player_w,    1, 0, 0, H_stop_player);
  XEN_DEFINE_PROCEDURE(S_free_player,    g_free_player_w,    1, 0, 0, H_free_player);
  XEN_DEFINE_PROCEDURE(S_players,        g_players_w,        0, 0, 0, H_players);
  XEN_DEFINE_PROCEDURE(S_player_p,       g_player_p_w,       1, 0, 0, H_player_p);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_dac_size, g_dac_size_w, H_dac_size,
				   S_setB S_dac_size, g_set_dac_size_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_dac_combines_channels, g_dac_combines_channels_w, H_dac_combines_channels,
				   S_setB S_dac_combines_channels, g_set_dac_combines_channels_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_cursor_update_interval, g_cursor_update_interval_w, H_cursor_update_interval,
				   S_setB S_cursor_update_interval, g_set_cursor_update_interval_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_cursor_location_offset, g_cursor_location_offset_w, H_cursor_location_offset,
				   S_setB S_cursor_location_offset, g_set_cursor_location_offset_w,  0, 0, 1, 0);

  #define H_stop_playing_hook S_stop_playing_hook " (snd): called when a sound finishes playing."
  #define H_play_hook S_play_hook " (samps): called each time a buffer is sent to the DAC."
  #define H_start_playing_hook S_start_playing_hook " (snd): called when a play request is triggered. \
If it returns " PROC_TRUE ", the sound is not played."
  #define H_dac_hook S_dac_hook " (sdobj): called just before data is sent to DAC passing data as sound-data object"
  #define H_stop_dac_hook S_stop_dac_hook " (): called upon mus_audio_close (when DAC is turned off)"
  #define H_stop_playing_selection_hook S_stop_playing_selection_hook " (): called when the selection stops playing"
  #define H_start_playing_selection_hook S_start_playing_selection_hook " (): called when the selection starts playing"

  stop_playing_hook = XEN_DEFINE_HOOK(S_stop_playing_hook, 1, H_stop_playing_hook);                         /* arg = sound */
  start_playing_hook = XEN_DEFINE_HOOK(S_start_playing_hook, 1, H_start_playing_hook);                      /* arg = sound */
  play_hook = XEN_DEFINE_HOOK(S_play_hook, 1, H_play_hook);                                                 /* args = size */
  dac_hook = XEN_DEFINE_HOOK(S_dac_hook, 1, H_dac_hook);                                                    /* args = data as sound_data obj */
  stop_dac_hook = XEN_DEFINE_HOOK(S_stop_dac_hook, 0, H_stop_dac_hook);                                     /* no args */
  stop_playing_selection_hook = XEN_DEFINE_HOOK(S_stop_playing_selection_hook, 0, H_stop_playing_selection_hook); /* no args */
  start_playing_selection_hook = XEN_DEFINE_HOOK(S_start_playing_selection_hook, 0, H_start_playing_selection_hook); /* no args */

  sdobj = XEN_FALSE;
  sdobj_loc = NOT_A_GC_LOC;
}
